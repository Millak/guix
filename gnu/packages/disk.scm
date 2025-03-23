;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2015 Mathieu Lirzin <mthl@gnu.org>
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016, 2018–2022 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2016, 2019-2021, 2023, 2025 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016, 2023 Janneke Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2016 Roel Janssen <roel@gnu.org>
;;; Copyright © 2016, 2017 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2017 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2017 Stefan Reichör <stefan@xsteve.at>
;;; Copyright © 2018 Vasile Dumitrascu <va511e@yahoo.com>
;;; Copyright © 2018 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2018 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2018, 2019, 2020 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2019, 2021 Leo Famulari <leo@famulari.name>
;;; Copyright © 2019 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2020 Pkill -9 <pkill9@runbox.com>
;;; Copyright © 2020 Vincent Legoll <vincent.legoll@gmail.com>
;;; Copyright © 2020 Raghav Gururajan <raghavgururajan@disroot.org>
;;; Copyright © 2021 Michael Rohleder <mike@rohleder.de>
;;; Copyright © 2021 Mathieu Othacehe <othacehe@gnu.org>
;;; Copyright © 2021 Brice Waegeneire <brice@waegenei.re>
;;; Copyright © 2021 Justin Veilleux <terramorpha@cock.li>
;;; Copyright © 2021 Sarah Morgensen <iskarian@mgsn.dev>
;;; Copyright © 2014, 2022 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2022 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2022 Disseminate Dissent <disseminatedissent@protonmail.com>
;;; Copyright © 2023 Timotej Lazar <timotej.lazar@araneo.si>
;;; Copyright © 2023 Morgan Smith <Morgan.J.Smith@outlook.com>
;;; Copyright © 2023 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2024 Sharlatan Hellseher <sharlatanus@gmail.com>
;;; Copyright © 2025 Ashish SHUKLA <ashish.is@lostca.se>

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

(define-module (gnu packages disk)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages c)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages cryptsetup)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages file-systems)
  #:use-module (gnu packages file)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages graphics)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages hurd)
  #:use-module (gnu packages image)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-web)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages popt)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages samba)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages w3m)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system go)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system python)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system qt)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system scons)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public bcache-tools
  ;; The 1.1 release is a year old and missing new features & documentation.
  (let ((commit "096d205a9f1be8540cbc5a468c0da8203023de70")
        (revision "0"))
    (package
      (name "bcache-tools")
      (version (git-version "1.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url (string-append "https://git.kernel.org/pub/scm/"
                                   "linux/kernel/git/colyli/bcache-tools.git"))
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0r0vwg4vacz5zgsafk360xn7gi2scy01c79mkmjrdyxjfij5z3iy"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f                    ; no test suite
         #:make-flags
         (list (string-append "PREFIX=" (assoc-ref %outputs "out"))
               (string-append "UDEVLIBDIR=" (assoc-ref %outputs "out")
                              "/lib/udev")
               (string-append "DRACUTLIBDIR=" (assoc-ref %outputs "out")
                              "/lib/dracut")
               (string-append "CC=" ,(cc-for-target)))
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)          ; no configure script
           (add-before 'install 'fix-hard-coded-file-names
             (lambda _
               ;; Some rules still hard-code /usr.
               (substitute* "Makefile"
                 (("/usr") "${PREFIX}"))
               #t))
           (add-before 'install 'create-target-directories
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out")))
                 (for-each (lambda (dir)
                             (mkdir-p (string-append out dir)))
                           (list "/lib/udev/rules.d"
                                 "/sbin"
                                 "/share/man/man8"))
                 #t))))))
      (native-inputs
       (list pkg-config))
      (inputs
       `(("util-linux:lib" ,util-linux "lib"))) ; libblkid
      (home-page "https://bcache.evilpiepirate.org")
      (synopsis "Tools for the Linux kernel block layer cache")
      (description
       "This package contains user-space utilities to create and inspect bcache
partitions.  It's rather minimal as bcache is designed to work well without
configuration on any system.

Linux's @acronym{bcache, block layer cache} lets one or more fast block devices,
such as flash-based @acronym{SSDs, solid state drives}, to act as a cache for
one or more slower (and inexpensive) devices, such as hard disk drives or
redundant storage arrays.  In fact, bcache intends to be a superior alternative
to battery-backed RAID controllers.

Bcache is designed around the performance characteristics of SSDs and tries to
minimize write inflation.  It's file-system agnostic and does both write-through
and write-back caching.")
      (license license:gpl2))))

(define-public udevil
  (package
    (name "udevil")
    (version "0.4.4")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/IgnorantGuru/udevil")
         (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0x9mjr9abvbxzfa9mrip5264iz1qxvsl01k3ybz95q4a7xl4jcb3"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list
        "--disable-systemd"
        (string-append "--sysconfdir="
                       (assoc-ref %outputs "out")
                       "/etc")
        ;; udevil expects these programs to be run with an UID of root.
        ;; mount and umount are %default-privileged-programs on Guix System;
        ;; the others must be explicitly added if desired.
        "--with-mount-prog=/run/privileged/bin/mount"
        "--with-umount-prog=/run/privileged/bin/umount"
        "--with-losetup-prog=/run/privileged/bin/losetup"
        "--with-setfacl-prog=/run/privileged/bin/setfacl")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-root-reference
           (lambda _
             (substitute* "src/Makefile.in"
               (("-o root -g root") ""))
             #t))
         (add-after 'unpack 'patch-udevil-reference
           ;; udevil expects itself to be run with uid set as root.
           ;; devmon also expects udevil to be run with uid set as root.
           ;; user has to manually add udevil to privileged-programs.
           (lambda _
             (substitute* "src/udevil.c"
               (("/usr/bin/udevil") "/run/privileged/bin/udevil"))
             (substitute* "src/devmon"
               (("`which udevil 2>/dev/null`") "/run/privileged/bin/udevil"))
             #t)))))
    (native-inputs
     (list intltool pkg-config))
    (inputs
     (list cifs-utils
           curlftpfs
           eudev
           fakeroot
           glib
           sshfs))
    (synopsis "Device and file system manager")
    (description "udevil is a command line program that mounts and unmounts
removable devices without a password, shows device info, and monitors device
changes.  It can also mount ISO files, NFS, SMB, FTP, SSH and WebDAV URLs, and
tmpfs/ramfs filesystems.")
    (home-page "https://ignorantguru.github.io/udevil/")
    (license license:gpl3+)))

(define-public parted
  (package
    (name "parted")
    (version "3.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/parted/parted-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "04p6b4rygrfd1jrskwrx3bn2icajg1mvbfhyc0c9l3ya7kixnhrv"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:configure-flags (if (target-hurd?)
                            #~'("--disable-device-mapper")
                            #~'())
      #:tests? (not (or (target-hurd?)
                        (%current-target-system)))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-locales-and-python
            (lambda _
              (substitute* "tests/msdos-overlap"
                (("/usr/bin/python") (which "python"))))))))
    (inputs
     `(,@(if (target-hurd?)
             (list hurd-minimal)
             (list lvm2))
       ,readline
       (,util-linux "lib")))
    (native-inputs
     (list gettext-minimal

           ;; For the tests.
           e2fsprogs
           perl
           python-wrapper
           util-linux))
    (outputs '("out" "debug"))
    (home-page "https://www.gnu.org/software/parted/")
    (synopsis "Disk partition editor")
    (description
     "GNU Parted is a package for creating and manipulating disk partition
tables.  It includes a library and command-line utility.")
    (license license:gpl3+)))

(define-public parted-3.4
  (package
    (inherit parted)
    (version "3.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/parted/parted-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "0hjkv84x1bs2qqyx1fnzjqyyqrhv7kpdbq9bgydmi99d8wi80ag1"))))))

(define-public fdisk
  (package
    (name "fdisk")
    (version "2.0.0a1")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/fdisk/gnufdisk-"
                          version ".tar.gz"))
      (sha256
       (base32
        "1d8za79kw8ihnp2br084rgyjv9whkwp7957rzw815i0izx6xhqy9"))))
    (build-system gnu-build-system)
    (inputs
     `(("gettext" ,gettext-minimal)
       ("guile" ,guile-1.8)
       ("libxcrypt" ,libxcrypt)
       ("util-linux" ,util-linux "lib")
       ("parted" ,parted)))
    ;; The build neglects to look for its own headers in its own tree.  A next
    ;; release should fix this, but may never come: GNU fdisk looks abandoned.
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-broken-header-probes
           (lambda _
             (substitute* "backend/configure"
               (("gnufdisk-common.h .*") "\n"))
             #t)))
       #:make-flags (list (string-append "CPPFLAGS="
                                         " -I../common/include "
                                         " -I../debug/include "
                                         " -I../exception/include"))))
    (home-page "https://www.gnu.org/software/fdisk/")
    (synopsis "Low-level disk partitioning and formatting")
    (description
     "GNU fdisk provides a GNU version of the common disk partitioning tool
fdisk.  fdisk is used for the creation and manipulation of disk partition
tables, and it understands a variety of different formats.")
    (license license:gpl3+)))

(define-public findimagedupes
  (package
    (name "findimagedupes")
    (version "2.20.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jhnc/findimagedupes")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1zfxmc6c1z4hzsq3k85xxida1v291frq4wbmxv9cg4jmw0ddk5ic"))))
    (build-system perl-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases #~(modify-phases %standard-phases
                   (delete 'configure)
                   (delete 'build)
                   (replace 'install
                     ;; There's no ‘make install’ target.
                     (lambda* (#:key outputs #:allow-other-keys)
                       (install-file "findimagedupes"
                                     (string-append #$output "/bin"))))
                   (add-after 'unpack 'use-image-magick
                     ;; TODO: package perl-graphics-magick and switch this out
                     (lambda _
                       (substitute* "findimagedupes"
                         (("Graphics::Magick")
                          "Image::Magick"))))
                   (add-after 'unpack 'set-inline-dir
                     (lambda _
                       (substitute* "findimagedupes"
                         (("/usr/local")
                          #$output))))
                   (add-after 'install 'inline-generation
                     (lambda _
                       (mkdir-p (string-append #$output "/lib/findimagedupes"))
                       (invoke (string-append #$output "/bin/findimagedupes"))))
                   (add-after 'install 'wrap-findimagedupes
                     (lambda* (#:key outputs #:allow-other-keys)
                       (wrap-program (string-append #$output
                                                    "/bin/findimagedupes")
                         `("PERL5LIB" ":" prefix
                           (,(getenv "PERL5LIB") ,(string-append #$output
                                                   "/lib/perl5/site_perl")))))))))
    (inputs (list bash-minimal perl-db-file perl-file-mimeinfo
                  perl-image-magick perl-inline-c))
    (home-page "https://github.com/jhnc/findimagedupes")
    (synopsis "Find visually similar or duplicate images")
    (description "findimagedupes compares a list of files for visual
similarity.")
    (license license:gpl3+)))

(define-public gpart
  ;; The latest (0.3) release is from 2015 and is missing a crash fix.
  (let ((commit "ec03350a01ad69708b5a3e2d47b8e002b0eba6c9")
        (revision "0"))
    (package
      (name "gpart")
      (version (git-version "0.3" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/baruch/gpart")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1dassswliaiwhhmx7yz540yyxgk53fvg672dbvgc5q0v6cqrh5jx"))))
      (build-system gnu-build-system)
      (arguments
       `(#:configure-flags
         (list (string-append "--docdir=" (assoc-ref %outputs "out") "/share/doc/"
                              ,name "-" ,version))
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'skip-premature-configuration
             (lambda _
               (substitute* "autogen.sh"
                 (("\\./configure") "")))))))
      (native-inputs
       (list autoconf automake))
      (home-page "https://github.com/baruch/gpart")
      (synopsis "Guess and recover PC-style partition tables")
      (description
       "Gpart tries to guess the partitions on a PC-style, MBR-partitioned disk
after they have been inadvertently deleted or the primary partition table at
sector 0 damaged.  In both cases, the contents of these partitions still exist
on the disk but the operating system cannot access them.

Gpart ignores the partition table and scans each sector of the device or image
file for several known file system and partition types.  Only partitions which
have been formatted in some way can be recognized.  Several file system guessing
modules are built in; more can be written and loaded at run time.

The guessed table can be restored manually, for example with @command{fdisk},
written to a file, or---if you firmly believe it's entirely correct---directly
to disk.

It should be stressed that gpart does a very heuristic job.  It can easily be
right in its guesswork but it can also be terribly wrong.  Never believe its
output without any plausibility checks.")
      (license license:gpl2+))))

(define-public gptfdisk
  (package
    (name "gptfdisk")
    (version "1.0.9")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://sourceforge/gptfdisk/gptfdisk/"
                          version "/gptfdisk-" version ".tar.gz"))
      (sha256
       (base32 "1hjh5m77fmfq5m44yy61kchv7mbfgx026aw3jy5qxszsjckavzns"))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-include-directory
           (lambda _
             (substitute* "gptcurses.cc"
               (("ncursesw/ncurses.h") "ncurses.h"))))
         (delete 'configure)            ; no configure script
         (replace 'install
           ;; There's no ‘make install’ target.
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (man (string-append out "/share/man/man8")))
               (install-file "gdisk" bin)
               (install-file "sgdisk" bin)
               (install-file "cgdisk" bin)
               (install-file "fixparts" bin)
               (install-file "cgdisk.8" man)
               (install-file "fixparts.8" man)
               (install-file "gdisk.8" man)
               (install-file "sgdisk.8" man)))))))
    (native-inputs
     (list gettext-minimal))
    (inputs
     (list ncurses
           popt
           `(,util-linux "lib"))) ;libuuid
    (home-page "https://www.rodsbooks.com/gdisk/")
    (synopsis "Low-level GPT disk partitioning and formatting")
    (description "GPT fdisk (aka gdisk) is a text-mode partitioning tool that
works on Globally Unique Identifier (@dfn{GUID}) Partition Table (@dfn{GPT})
disks, rather than on the older Master Boot Record (@dfn{MBR}) partition
scheme.")
    (license license:gpl2)))

(define-public ddrescue
  (package
    (name "ddrescue")
    (version "1.29.1")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/ddrescue/ddrescue-"
                          version ".tar.lz"))
      (sha256
       (base32 "0xfx0hg3kkc6d3z0z9g1ingg2bnzcp1vjspcl8spi016y1fx9myx"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags (list (string-append "CXX=" ,(cxx-for-target)))))
    (home-page "https://www.gnu.org/software/ddrescue/ddrescue.html")
    (synopsis "Data recovery utility")
    (native-inputs (list lzip))
    (description
     "GNU ddrescue is a fully automated data recovery tool.  It copies data
from one file to another, working to rescue data in case of read errors.  The
program also includes a tool for manipulating its log files, which are used
to recover data more efficiently by only reading the necessary blocks.")
    (license license:gpl3+)))

(define-public dosfstools
  (package
    (name "dosfstools")
    (version "4.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dosfstools/dosfstools")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1xygsixmmc9l7drxylggnzkqqiks8zmlsbhg3z723ii2ak94236s"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags (list "--enable-compat-symlinks")
       #:make-flags (list (string-append "PREFIX=" (assoc-ref %outputs "out"))
                          (string-append "CC=" ,(cc-for-target)))))
    (native-inputs
     (list autoconf automake
           ;; For tests.
           xxd))
    (home-page "https://github.com/dosfstools/dosfstools")
    (synopsis "Utilities for making and checking MS-DOS FAT file systems")
    (description
     "The dosfstools package includes the mkfs.fat and fsck.fat utilities,
which respectively make and check MS-DOS FAT file systems.")
    (license license:gpl3+)))

(define dosfstools/static
  (static-package
   (package (inherit dosfstools))))

(define-public fatfsck/static
  (package
    (name "fatfsck-static")
    (version (package-version dosfstools))
    (build-system trivial-build-system)
    (source #f)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((src (string-append (assoc-ref %build-inputs "dosfstools")
                                   "/sbin"))
               (exe "fsck.fat")
               (bin (string-append (assoc-ref %outputs "out") "/sbin")))
           (mkdir-p bin)
           (with-directory-excursion bin
             (copy-file (string-append src "/" exe) exe)
             (remove-store-references exe)
             (chmod exe #o555)
             ;; Add fsck.vfat symlink to match the Linux driver name.
             (symlink exe "fsck.vfat")
             #t)))))
    (inputs (list dosfstools/static))
    (home-page (package-home-page dosfstools))
    (synopsis "Statically linked fsck.fat from dosfstools")
    (description "This package provides a statically-linked @command{fsck.fat}
and a @command{fsck.vfat} compatibility symlink for use in an initrd.")
    (license (package-license dosfstools))))

(define-public fatresize
  (package
    (name "fatresize")
    (version "1.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ya-mouse/fatresize")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1vhz84kxfyl0q7mkqn68nvzzly0a4xgzv76m6db0bk7xyczv1qr2"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list parted))
    (home-page "https://github.com/ya-mouse/fatresize")
    (synopsis "Resize FAT partitions")
    (description
     "This package provides a tool to resize FAT partitions using libparted.")
    (license license:gpl3+)))

(define-public hdparm
  (package
    (name "hdparm")
    (version "9.65")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/hdparm/hdparm/"
                                  "hdparm-" version ".tar.gz"))
              (sha256
               (base32
                "0jssagggg52ssl9kg99m88afghj7bm1854vyf4p96q6h23wjjjfi"))))
    (build-system gnu-build-system)
    (arguments
     (list #:make-flags
           #~(list (string-append "binprefix=" #$output)
                   (string-append "manprefix=" #$output)
                   (string-append "CC=" #$(cc-for-target))
                   ;; Let Guix strip binaries and not break cross-compilation.
                   "STRIP=true")
           #:phases
           #~(modify-phases %standard-phases
               (delete 'configure))     ; no configure script
           #:tests? #f))                ; no test suite
    (home-page "https://sourceforge.net/projects/hdparm/")
    (synopsis "View and tune ATA disk drive parameters")
    (description
     "@command{hdparm} is a command-line utility to control ATA controllers and
disk drives.  It can increase performance and/or reliability by careful tuning
of hardware settings like power and acoustic management, DMA modes, and caching.
It can also display detailed device information, or be used as a simple
performance benchmarking tool.

@command{hdparm} provides a command line interface to various Linux kernel
interfaces provided by the SATA/ATA/SAS @code{libata} subsystem, and the older
IDE driver subsystem.  Many external USB drive enclosures with SCSI-ATA Command
Translation (@dfn{SAT}) are also supported.")
    (license (license:non-copyleft "file://LICENSE.TXT"))))

(define-public sdparm
  (package
    (name "sdparm")
    (version "1.12")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://sg.danny.cz/sg/p/"
                           "sdparm-" version ".tar.xz"))
       (sha256
        (base32 "1gmdxr36allrgap2j4dv238d8awkj327ww0jjwpjwrpbvfpyzjf4"))))
    (build-system gnu-build-system)
    (home-page "https://sg.danny.cz/sg/sdparm.html")
    (synopsis "Provide access to SCSI device parameters")
    (description
     "Sdparm reads and modifies SCSI device parameters.  These devices can be
SCSI disks, in which case the role of @command{sdparm} is similar to its
namesake: the @command{hdparm} utility originally designed for ATA disks.
However, @command{sdparm} can be used to access parameters on any device that
uses a SCSI command set.  Such devices include CD/DVD drives (irrespective of
transport), SCSI and ATAPI tape drives, and SCSI enclosures.  This utility can
also send commands associated with starting and stopping the media, loading
and unloading removable media and some other housekeeping functions.")
    (license license:bsd-3)))

(define-public idle3-tools
  (package
    (name "idle3-tools")
    (version "0.9.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/idle3-tools/idle3-tools-"
                           version ".tgz"))
       (sha256
        (base32
         "00ia7xq9yldxyl9gz0mr4xa568nav14p0fnv82f2rbbkg060cy4p"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ;no test suite
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))
       #:make-flags (list "CC=gcc"
                          (string-append "manprefix=")
                          (string-append "DESTDIR="
                                         (assoc-ref %outputs "out")))))
    (home-page "https://idle3-tools.sourceforge.net")
    (synopsis "Change or disable Western Digital hard drives' Idle3 timer")
    (description
     "Idle3-tools provides a utility to get, set, or disable the Idle3 timer
present in many Western Digital hard drives.  This timer is part of the
\"IntelliPark\" feature that stops the disk when not in use.  Unfortunately,
the default timer setting is not well suited to Linux or other *nix systems,
and can dramatically shorten the lifespan of the drive if left unchecked.")
    (license license:gpl3+)))

(define-public greaseweazle-host-tools
  (package
    (name "greaseweazle-host-tools")
    (version "1.12")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/keirf/greaseweazle")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1lpvjlf2xg4ccwik8npiihi0lgw9dx5h12pp4ry343gkz4pwgk9x"))))
    (build-system python-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'setuptools-version
            (lambda _
              (setenv "SETUPTOOLS_SCM_PRETEND_VERSION" "1.8")))
          (add-after 'install 'install-udev-rules
            (lambda _
              (install-file "scripts/49-greaseweazle.rules"
                            (string-append #$output "/lib/udev/rules.d/")))))))
    (native-inputs (list python-setuptools-scm))
    (propagated-inputs
     (list python-bitarray python-crcmod python-pyserial python-requests))
    (synopsis "Tools for accessing a floppy drive at the raw flux level")
    (description
     "This package provides the host tools for controlling a Greaseweazle: an
Open Source USB device capable of reading and writing raw data on nearly any
type of floppy disk")
    (home-page "https://github.com/keirf/greaseweazle")
    (license license:public-domain)))

(define-public gparted
  (package
    (name "gparted")
    (version "1.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/gparted/gparted/gparted-"
                           version "/gparted-" version ".tar.gz"))
       (sha256
        (base32 "0nzaqvbdwq3daddby79k9rh3d560g5kaxqamkasxqka9rsrm37wv"))))
    (build-system glib-or-gtk-build-system)
    (arguments
      ;; Tests require access to files outside the build container, such
      ;; as ‘/dev/disk/by-id/’.
     `(#:tests? #f))
    (inputs
     (list `(,util-linux "lib") parted glib gtkmm-3 lvm2 libxml2))
    (native-inputs
     (list intltool
           itstool
           yelp-tools
           pkg-config))
    (home-page "https://gparted.org/")
    (synopsis "Partition editor to graphically manage disk partitions")
    (description "GParted is a GNOME partition editor for creating,
reorganizing, and deleting disk partitions.  It uses libparted from the parted
project to detect and manipulate partition tables.  Optional file system tools
permit managing file systems not included in libparted.")
    ;; The home page says GPLv2, but the source code says GPLv2+.
    (license license:gpl2+)))

(define-public testdisk
  (package
    (name "testdisk")
    (version "7.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.cgsecurity.org/testdisk-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "1zlh44w67py416hkvw6nrfmjickc2d43v51vcli5p374d5sw84ql"))))
    (build-system gnu-build-system)
    (inputs
     (list ntfs-3g
           `(,util-linux "lib")
           openssl
           ;; FIXME: add reiserfs.
           zlib
           e2fsprogs
           libjpeg-turbo
           ncurses))
    (home-page "https://www.cgsecurity.org/wiki/TestDisk")
    (synopsis "Data recovery tool")
    (description "TestDisk is primarily designed to help recover lost
partitions and/or make non-booting disks bootable again when these symptoms
were caused by faulty software or human error (such as accidentally deleting a
partition table).  TestDisk can:
@enumerate
@item Fix partition table, recover deleted partition
@item Recover FAT32 boot sector from its backup
@item Rebuild FAT12/FAT16/FAT32 boot sector
@item Fix FAT tables
@item Rebuild NTFS boot sector
@item Recover NTFS boot sector from its backup
@item Fix MFT using MFT mirror
@item Locate ext2/ext3/ext4 Backup SuperBlock
@item Un-delete files from FAT, exFAT, NTFS and ext2 file systems
@item Copy files from deleted FAT, exFAT, NTFS and ext2/ext3/ext4 partitions.
@end enumerate
This package also includes the @command{photorec} command, described below.

PhotoRec is file data recovery software designed to recover lost files
including video, documents and archives from hard disks, CD-ROMs, and lost
pictures (thus the Photo Recovery name) from digital camera memory.  PhotoRec
ignores the file system and goes after the underlying data, so it will still
work even if your media's file system has been severely damaged or
reformatted.  It can recover lost files from at least:
@enumerate
@item FAT
@item NTFS
@item exFAT
@item ext2/ext3/ext4 file system
@item HFS+
@end enumerate")
    (license license:gpl2+)))

(define-public pydf
  (package
    (name "pydf")
    (version "12")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pydf" version))
       (sha256
        (base32
         "0f8ly8xyp93i2hm9c0qjqd4y86nz73axw2f09z01mszwmg1sfivz"))))
  (build-system python-build-system)
  (home-page "http://kassiopeia.juls.savba.sk/~garabik/software/pydf/")
  (synopsis "Colourised @command{df} clone")
  (description "All-singing, all-dancing, fully colourised @command{df} clone
written in Python.  It displays the amount of disk space available on the
mounted file systems, using different colours for different types of file
systems.  Output format is completely customizable.")
  (license license:public-domain)))

(define-public f3
  (package
    (name "f3")
    (version "8.0")
    (source
     (origin
      (method git-fetch)
      (uri (git-reference
            (url "https://github.com/AltraMayor/f3")
            (commit (string-append "v" version))))
      (file-name (git-file-name name version))
      (sha256
       (base32 "17l5vspfcgfbkqg7bakp3gql29yb05gzawm8n3im30ilzdr53678"))))
    (build-system gnu-build-system)
    (arguments
     (list #:tests? #f                      ; no check target
           #:make-flags #~(list (string-append "CC=" #$(cc-for-target))
                                (string-append "PREFIX=" #$output))
           #:phases
           #~(modify-phases %standard-phases
               (delete 'configure)            ; no configure script
               (add-after 'build 'build-extra
                 (lambda* (#:key make-flags #:allow-other-keys)
                   (apply invoke "make" "extra" make-flags)))
               (add-after 'build 'install-extra
                 (lambda* (#:key make-flags #:allow-other-keys)
                   (apply invoke "make" "install-extra" make-flags))))))
    (inputs
     (list eudev parted))
    (home-page "http://oss.digirati.com.br/f3/")
    (synopsis "Test real capacity of flash memory cards and such")
    (description "F3 (Fight Flash Fraud or Fight Fake Flash) tests the full
capacity of a flash card (flash drive, flash disk, pendrive).  F3 writes to
the card and then checks if can read it.  It will assure you haven't been sold
a card with a smaller capacity than stated.")
    (license license:gpl3+)))

(define-public python-pyparted
  (package
    (name "python-pyparted")
    (version "3.13.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dcantrell/pyparted")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "09gjp60sr8l0v41anmdfnh9lizfz1mbb7mxvqq73s30vn4504982"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list e2fsprogs
           pkg-config
           python-pytest
           python-setuptools
           python-wheel))
    (inputs
     (list parted))
    (home-page "https://github.com/dcantrell/pyparted")
    (synopsis "Parted bindings for Python")
    (description "This package provides @code{parted} bindings for Python.")
    (license license:gpl2+)))

(define-public python-parted
  (deprecated-package "python-parted" python-pyparted))

(define-public duperemove
  (package
    (name "duperemove")
    (version "0.14.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/markfasheh/duperemove")
             (commit (string-append "v" version))))
       (sha256
        (base32 "0kl6bisbgf6x8a6gws6r097zrawhp9jxwh7m6nhq7dd48b8zrjw8"))
       (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list glib sqlite))
    (arguments
     `(#:tests? #f                      ; no test suite
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))           ; no configure script
       #:make-flags (list (string-append "PREFIX=" %output)
                          (string-append "CC=" ,(cc-for-target))
                          ;; Set to <next release>dev by default.
                          (string-append "VER=" ,version))))
    (home-page "https://github.com/markfasheh/duperemove")
    (synopsis "Tools for de-duplicating file system data")
    (description "Duperemove is a simple tool for finding duplicated extents
and submitting them for deduplication.  When given a list of files it will
hash their contents on a block by block basis and compare those hashes to each
other, finding and categorizing blocks that match each other.  When given the
@option{-d} option, duperemove will submit those extents for deduplication
using the Linux kernel extent-same @code{ioctl}.

Duperemove can store the hashes it computes in a @dfn{hash file}.  If given an
existing hash file, duperemove will only compute hashes for those files which
have changed since the last run.  Thus you can run duperemove repeatedly on
your data as it changes, without having to re-checksum unchanged data.

Duperemove can also take input from the @command{fdupes} program.")
    (license license:gpl2)))

(define-public ranger
  (package
    (name "ranger")
    (version "1.9.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://ranger.github.io/"
                                  "ranger-" version ".tar.gz"))
              (sha256
               (base32
                "0lfjrpv3z4h0knd3v94fijrw2zjba51mrp3mjqx2c98wr428l26f"))))
    (build-system python-build-system)
    (inputs
     (list bash-minimal w3m))
    (native-inputs
     (list which
           ;; For tests.
           python-pytest))
    (arguments
     '( ;; The 'test' target runs developer tools like pylint, which fail.
       #:test-target "test_pytest"
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'wrap-program
           ;; Tell 'ranger' where 'w3mimgdisplay' is.
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out  (assoc-ref outputs "out"))
                    (ranger (string-append out "/bin/ranger"))
                    (w3m (assoc-ref inputs "w3m"))
                    (w3mimgdisplay (string-append w3m
                                   "/libexec/w3m/w3mimgdisplay")))
               (wrap-program ranger
                 `("W3MIMGDISPLAY_PATH" ":" prefix (,w3mimgdisplay))))))
         (replace 'check
           ;; The default check phase simply prints 'Ran 0 tests in 0.000s'.
           (lambda* (#:key test-target #:allow-other-keys)
             (invoke "make" test-target))))))
    (home-page "https://ranger.github.io/")
    (synopsis "Console file manager")
    (description "ranger is a console file manager with Vi key bindings.  It
provides a minimalistic and nice curses interface with a view on the directory
hierarchy.  It ships with @code{rifle}, a file launcher that is good at
automatically finding out which program to use for what file type.")
    (license license:gpl3)))

(define-public fff
  (package
   (name "fff")
   (version "2.2")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/dylanaraps/fff")
                  (commit version)))
            (file-name (git-file-name name version))
            (sha256
             (base32
              "14ymdw6l6phnil0xf1frd5kgznaiwppcic0v4hb61s1zpf4wrshg"))))
   (build-system gnu-build-system)
   (inputs
    (list bash
          file))
   (arguments
    (list
     #:tests? #f                       ; no tests
     #:make-flags
     #~(list
        (string-append "PREFIX=" #$output))
     #:phases
     #~(modify-phases %standard-phases
         (add-after 'unpack 'refer-to-inputs
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((file (assoc-ref inputs "file")))
               (substitute* "fff"
                 (("\\bfile [-\"]" match)
                  (string-append file "/bin/" match))))))
         (delete 'configure))))         ; no configure script
   (home-page "https://github.com/dylanaraps/fff")
   (synopsis "Simple file manager written in bash")
   (description
    "@command{fff} (fast file-manager) is a simple, blazing fast and minimal
file manager for Linux, written in bash.  It only requires bash and coreutils,
and its highly optimized now for efficient performance.")
   (license license:expat)))

(define-public volume-key
  (package
    (name "volume-key")
    (version "0.3.12")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://releases.pagure.org/volume_key/volume_key-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "16rhfz6sjwxlmss1plb2wv2i3jq6wza02rmz1d2jrlnsq67p98vc"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config swig python-3))           ; used to generate the Python bindings
    (inputs
     (append
      (cons cryptsetup (libcryptsetup-propagated-inputs))
      (cons lvm2 (libdevmapper-propagated-inputs))
      (list nss
            (list util-linux "lib")
            glib
            gpgme)))
    (arguments
     `(#:tests? #f ; not sure how tests are supposed to pass, even when run manually
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'patch-python.h-path
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((python (assoc-ref inputs "python")))
               (substitute* "Makefile.in"
                 (("/usr/include/python") (string-append python "/include/python")))
               #t))))))
    (home-page "https://pagure.io/volume_key")
    (synopsis "Manipulate storage volume encryption keys")
    (description
     "This package provides a library for manipulating storage volume
encryption keys and storing them separately from volumes to handle forgotten
passphrases.")
    (license license:gpl2)))

(define-public ndctl
  (package
    (name "ndctl")
    (version "78")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/pmem/ndctl")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0rhmxjajxxslsikixlf9cdg5vcn42h7zzqkqj5p5pshxch368kn0"))))
    (build-system meson-build-system)
    (arguments
     ;; The test suite runs but SKIPs all tests: do not consider this tested!
     (list #:configure-flags
           #~(list (string-append "-Drootprefix=" #$output)
                   (string-append "-Dlibdir=" #$output "/lib")
                   (string-append "-Dbashcompletiondir=" #$output
                                  "/share/bash-completion/completions")
                   (string-append "-Dsysconfdir=" #$output "/etc")
                   (string-append "-Diniparserdir="
                                  #$(this-package-input "iniparser")
                                  "/include")
                   "-Dasciidoctor=disabled" ; use docbook-xsl instead
                   "-Dsystemd=disabled")
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'fix-version
                 ;; Our VERSION's always better than the build's poor guess.
                 (lambda _
                   (with-output-to-file "version"
                     (lambda _ (display #$version)))))
               (add-after 'unpack 'patch-FHS-file-names
                 (lambda _
                   (substitute* "git-version-gen"
                     (("/bin/sh") (which "sh")))
                   (substitute* "git-version"
                     (("/bin/bash") (which "bash"))))))))
    (native-inputs
     (list asciidoc
           bash-completion
           docbook-xsl
           libxml2
           pkg-config
           xmlto
           ;; Required for offline docbook generation.
           which))
    (inputs
     (list eudev
           iniparser
           json-c
           keyutils
           kmod
           libtraceevent
           libtracefs
           `(,util-linux "lib")))
    (home-page "https://github.com/pmem/ndctl")
    (synopsis "Manage the non-volatile memory device sub-system in the Linux kernel")
    (description
     "This package provides a utility library for managing the
libnvdimm (non-volatile memory device) sub-system in the Linux kernel.")
    ;; COPYING says LGPL2.1, but many source files are GPL2 so that's
    ;; the effective license.  Note that some files under ccan/ are
    ;; covered by BSD-3 or public domain, see the individual directories.
    (license license:gpl2)))

(define-public dmraid
  (package
    (name "dmraid")
    (version "1.0.0.rc16-3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://people.redhat.com/~heinzm/sw/dmraid/src/dmraid-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "1n7vsqvh7y6yvil682q129d21yhb0cmvd5fvsbkza7ypd78inhlk"))))
    (build-system gnu-build-system)
    (inputs (list lvm2))
    (native-inputs (list which))
    (arguments
     `(#:tests? #f                      ; No tests.
       ;; Prevent a race condition where some target would attempt to link
       ;; libdmraid.so before it had been built as reported in
       ;; <https://bugs.gnu.org/31999#187>.
       #:parallel-build? #f
       #:phases (modify-phases %standard-phases
                  (add-before 'configure 'change-directory
                    (lambda _
                      (chdir (string-append ,version "/dmraid"))
                      (substitute* "make.tmpl.in"
                        (("/bin/sh") (which "sh")))
                      #t)))
       #:configure-flags (list ;; Make sure programs such as 'dmevent_tool' can
                               ;; find libdmraid.so.
                               (string-append "LDFLAGS=-Wl,-rpath="
                                              (assoc-ref %outputs "out")
                                              "/lib"))))
    (home-page "https://people.redhat.com/~heinzm/sw/dmraid/")
    (synopsis "Device mapper RAID interface")
    (description
     "This software supports RAID device discovery, RAID set activation, creation,
removal, rebuild and display of properties for ATARAID/DDF1 metadata.

@command{dmraid} uses @file{libdevmapper} and the device-mapper kernel runtime
to create devices with respective mappings for the ATARAID sets discovered.")
    (license license:gpl2+)))

(define-public libblockdev
  (package
    (name "libblockdev")
    (version "3.3.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/storaged-project/"
                                  "libblockdev/releases/download/"
                                  version "/libblockdev-" version ".tar.gz"))
              (sha256
               (base32
                "19yzidh8i32327h8h7b9yx86yv1hm7kz1y91cn4gxzyypqd8wwy4"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-configuration-directory
            (lambda _
              (substitute* "src/lib/blockdev.c"
                (("/etc/libblockdev/conf.d/" path)
                 (string-append #$output path)))))
          (add-after 'unpack 'patch-plugin-paths
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* (find-files "src/plugins" "\\.c$")
                (("(gchar \\*arg.+\\{\")([^\"]+)" all start program)
                 (string-append
                  start (or (false-if-exception
                             (search-input-file inputs
                                                (string-append "bin/" program)))
                            (false-if-exception
                             (search-input-file inputs
                                                (string-append "sbin/" program)))
                            (begin
                              (format (current-warning-port)
                                      "warning: program ~s left unpatched~%"
                                      program)
                              program))))))))))
    (native-inputs
     (list gobject-introspection
           pkg-config
           python-wrapper))
    (inputs
     (append
      (cons cryptsetup (libcryptsetup-propagated-inputs))
      (if (supported-package? multipath-tools)
          (list multipath-tools)
          '())
      (list bcache-tools
            btrfs-progs
            dosfstools
            dmraid
            e2fsprogs
            eudev
            glib
            gptfdisk
            json-glib-minimal
            keyutils
            kmod
            libatasmart
            libbytesize
            libnvme
            libyaml
            lvm2
            mdadm
            ndctl
            nss
            ntfs-3g
            parted
            util-linux
            volume-key
            xfsprogs)))
    (home-page "https://github.com/storaged-project/libblockdev")
    (synopsis "Library for manipulating block devices")
    (description
     "libblockdev is a C library supporting GObject introspection for
manipulation of block devices.  It has a plugin-based architecture where each
technology (like LVM, Btrfs, MD RAID, Swap...) is implemented in a separate
plugin, possibly with multiple implementations (e.g. using LVM CLI or the new
LVM D-Bus API).")
    (license license:lgpl2.1+)))

(define-public rmlint
  (package
    (name "rmlint")
    (version "2.10.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/sahib/rmlint")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0sk4v1chnk2hvzi92svyf8qgamfs4fvial90qwx4a7dayxhkbsm4"))))
    (build-system scons-build-system)
    (arguments
     (list
      #:scons-flags
      #~(list (string-append "--prefix=" #$output)
              (string-append "--actual-prefix=" #$output))
      #:tests? #f                       ; no tests
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'scons-propagate-environment
            (lambda* (#:key inputs #:allow-other-keys)
              ;; TODO: `rmlint --gui` fails with
              ;; "Failed to load shredder: No module named 'shredder'".
              ;; The GUI might also need extra dependencies, such as
              ;; python-gobject, python-cairo, dconf, librsvg, gtksourceview3.
              (substitute* "lib/cmdline.c"
                (("const char \\*commands\\[\\] = \\{\"python3\", \"python\", NULL\\};")
                 (string-append "const char *commands[] = {\""
                                (search-input-file inputs "/bin/python")
                                "\", \"python\", NULL};")))
              ;; By design, SCons does not, by default, propagate
              ;; environment variables to subprocesses.  See:
              ;; <http://comments.gmane.org/gmane.linux.distributions.nixos/4969>
              ;; Here, we modify the SConstruct file to arrange for
              ;; environment variables to be propagated.
              (substitute* "SConstruct"
                (("^env = Environment\\(.*\\)" all)
                 (string-append all "\nenv['ENV']=os.environ"))))))))
    (native-inputs
     (list `(,glib "bin") pkg-config python-sphinx))
    (inputs
     (list elfutils
           glib
           json-glib
           python-wrapper
           `(,util-linux "lib")))
    (home-page "https://rmlint.rtfd.org")
    (synopsis "Remove duplicates and other lint from the file system")
    (description "@command{rmlint} finds space waste and other broken things
on your file system and offers to remove it.  @command{rmlint} can find:

@itemize
@item duplicate files and duplicate directories,
@item non-stripped binaries (i.e. binaries with debug symbols),
@item broken symbolic links,
@item empty files and directories,
@item files with broken user and/or group ID.
@end itemize\n")
    (license license:gpl3+)))

(define-public lf
  (package
    (name "lf")
    (version "33")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gokcehan/lf")
             (commit (string-append "r" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1jmqf27ysi35n3hqahlzs5hym7i4w1mplklrvv0lc0baddzx7av8"))))
    (build-system go-build-system)
    (arguments
     (list
      #:install-source? #f
      #:import-path "github.com/gokcehan/lf"))
    (native-inputs
     (list go-github-com-djherbis-times
           go-github-com-fsnotify-fsnotify
           go-github-com-gdamore-tcell-v2
           go-github-com-mattn-go-runewidth
           go-github-com-xuanwo-go-locale
           go-golang-org-x-sys
           go-golang-org-x-term
           go-golang-org-x-text))
    (home-page "https://github.com/gokcehan/lf")
    (synopsis "Console file browser similar to Ranger")
    (description
     "@code{lf} (as in \"list files\") is a terminal file manager written in
Go.  It is heavily inspired by @code{ranger} with some missing and extra
features.  Some of the missing features are deliberately omitted since they
are better handled by external tools.")
    (license license:expat)))

(define-public xfe
  (package
    (name "xfe")
    (version "1.44")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://sourceforge/xfe/xfe/" version "/"
                       "xfe-" version ".tar.xz"))
       (sha256
        (base32 "1dihq03jqjllb69r78d9ihjjadi39v7sgzdf68qpxz5xhp8i8k2r"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-potfiles
           (lambda _
             ;; To add missing entry 'intl/plural.c' to potfiles list.
             ;; Refer to https://sourceforge.net/p/xfe/bugs/257/
             (substitute* "po/POTFILES.in"
               (("src/help.h")
                (string-append "src/help.h\n"
                               "intl/plural.c")))))
         (add-after 'unpack 'patch-bin-dirs
           (lambda* (#:key inputs #:allow-other-keys)
             (let* ((bash (assoc-ref inputs "bash"))
                    (coreutils (assoc-ref inputs "coreutils"))
                    (findutils (assoc-ref inputs "findutils"))
                    (file-prog (assoc-ref inputs "file")))
               (with-directory-excursion "src"
                 (substitute* '("FilePanel.cpp" "help.h" "SearchPanel.cpp"
                                "startupnotification.cpp" "xfeutils.cpp"
                                "../st/config.h")
                   (("/bin/sh" file) (string-append bash file))
                   (("/bin/ls" file) (string-append coreutils file))
                   (("/usr(/bin/du)" _ file) (string-append coreutils file))
                   (("/usr(/bin/sort)" _ file) (string-append coreutils file))
                   (("/usr(/bin/cut)" _ file) (string-append coreutils file))
                   (("/usr(/bin/xargs)" _ file) (string-append findutils file))
                   (("/usr(/bin/file)" _ file) (string-append file-prog file))))
               #t)))
         (add-after 'unpack 'patch-share-dirs
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (share (string-append out "/share"))
                    (xfe (string-append share "/xfe")))
               (with-directory-excursion "src"
                 (substitute* '("foxhacks.cpp" "help.h" "xfedefs.h"
                                "XFileExplorer.cpp")
                   (("/(usr|opt)(/local)?/share") share)
                   (("~/.config/xfe") xfe)))
               #t))))))
    (native-inputs
     (list intltool pkg-config))
    (inputs
     `(("bash" ,bash)
       ("coreutils" ,coreutils)
       ("file" ,file)
       ("findutils" ,findutils)
       ("fox" ,fox)
       ("freetype" ,freetype)
       ("x11" ,libx11)
       ("xcb" ,libxcb)
       ("xcb-util" ,xcb-util)
       ("xft" ,libxft)
       ("xrandr" ,libxrandr)))
    (synopsis "File Manager for X-Based Graphical Systems")
    (description "XFE (X File Explorer) is a file manager for X.  It is based on
the popular but discontinued, X Win Commander.  It aims to be the file manager
of choice for all light thinking Unix addicts!")
    (home-page "http://roland65.free.fr/xfe/")
    (license license:gpl2+)))

(define-public hddtemp
  (package
    (name "hddtemp")
    ;; <https://savannah.nongnu.org/projects/hddtemp/> advertises the project as
    ;; ‘orphaned/unmaintained’.  Use a maintained fork/continuation.
    (version "0.4.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/vitlav/hddtemp")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "04kylb2ka0jimi238zpfq1yii2caidpmj3ck51rvxz03y5lpq8fw"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags (list (string-append
                                "--with-db-path="
                                (assoc-ref %outputs "out")
                                "/share/hddtemp/hddtemp.db"))
       #:phases
       (modify-phases %standard-phases
         (add-before 'bootstrap 'delete-autogen.sh
           (lambda _
             ;; The default 'bootstrap phase works better.
             (delete-file "autogen.sh")))
         (add-after 'install 'install-db
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (install-file "data/hddtemp.db"
                             (string-append out "/share/hddtemp"))))))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("gettext" ,gettext-minimal)))
    (home-page "https://github.com/vitlav/hddtemp")
    (synopsis "Report the temperature of hard drives from S.M.A.R.T. information")
    (description "@command{hddtemp} is a small utility that gives you the
temperature of your hard drive by reading S.M.A.R.T. information (for drives
that support this feature).")
    (license license:gpl2+)))

(define-public memkind
  (package
    (name "memkind")
    (version "1.13.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/memkind/memkind")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32 "0zbil6xqmsrnh773ihxyfna6pvvxv3kczdb3g863ssflwwvv7h4w"))))
    (build-system gnu-build-system)
    (inputs
     (list ;; memkind patched jemalloc to add je_arenalookupx,
           ;; je_check_reallocatex--i.e. they forked jemalloc.
           ;("jemalloc" ,jemalloc)
           ndctl numactl))
    (native-inputs
     (list autoconf automake libtool))
    (arguments
     `(#:configure-flags
       (list (string-append "--docdir=" (assoc-ref %outputs "out")
                            "/share/doc/" ,name "-" ,version))
       #:parallel-build? #f             ;fails otherwise
       #:tests? #f ; Tests require a NUMA-enabled system.
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'autogen-jemalloc
           (lambda _
             (with-directory-excursion "jemalloc"
               (substitute* "Makefile.in"
                (("/bin/sh") (which "sh")))
               (invoke "autoconf")
               (substitute* "configure"
                (("/bin/sh") (which "sh")))))))))
    (home-page "https://github.com/memkind/memkind")
    (synopsis "Heap manager with memory kinds (for NUMA)")
    (description "This package provides a user-extensible heap manager
built on top of jemalloc which enables control of memory characteristics
and a partitioning of the heap between kinds of memory (for NUMA).")
    (license license:bsd-3)))

(define-public mmc-utils
  (let ((commit "3969aa4804edb8aed7bcb3c958e49d0c7388b067")
        (revision "1"))
    (package
      (name "mmc-utils")
      (version (git-version "0.1" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://git.kernel.org/pub/scm/utils/mmc/mmc-utils.git")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32 "0pvcm685x63afvp8795jd4vn4zs8psh8bs6j2yvk1kgrawpyk10g"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f ; No test suite
         #:make-flags (list (string-append "prefix=" (assoc-ref %outputs "out"))
                            (string-append "CC=" ,(cc-for-target)))
         #:phases
         (modify-phases %standard-phases
           ;; No ./configure script
           (delete 'configure)
           ;; The Makefile's "install-man" target is a no-op.
           (add-after 'install 'install-doc
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (man1 (string-append out "/share/man/man1")))
                 (install-file "man/mmc.1" man1)))))))
      (home-page
       "https://www.kernel.org/doc/html/latest/driver-api/mmc/mmc-tools.html")
      (synopsis "Configure MMC storage devices from userspace")
      (description "mmc-utils is a command-line tool for configuring and
inspecting MMC storage devices from userspace.")
      (license license:gpl2))))

(define-public bmaptools
  (package
    (name "bmaptools")
    (version "3.6")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/intel/bmap-tools")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "01xzrv5nvd2nvj91lz4x9s91y9825j9pj96z0ap6yvy3w2dgvkkl"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             ;; XXX: Remove failing test.
             (invoke "nosetests" "-v"
                     "--exclude" "test_bmap_helpers"))))))
    (native-inputs
     (list python-mock python-nose))
    (propagated-inputs
     (list python-six))
    (home-page "https://github.com/intel/bmap-tools")
    (synopsis "Create block map for a file or copy a file using block map")
    (description "Bmaptool is a tool for creating the block map (bmap) for a
file and copying files using the block map.  The idea is that large files,
like raw system image files, can be copied or flashed a lot faster and more
reliably with @code{bmaptool} than with traditional tools, like @code{dd} or
@code{cp}.")
    (license license:gpl2)))

(define-public duc
  (package
    (name "duc")
    (version "1.4.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/zevv/duc")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0sglcn38rgn6y3m5ahngizyn3x2rzhqjphs7g0ppnlinkz56rcv4"))
              (patches
               (search-patches "duc-fix-test-sh.patch"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out")))
                (substitute* "src/duc/cmd-ui.c"
                  (("ncursesw/ncurses.h") "ncurses.h"))
                (substitute* "examples/index.cgi"
                  (("/usr/local/bin/duc")
                   (string-append out "/bin/duc"))))))
          (add-after 'install 'install-examples
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (doc (string-append out "/share/doc/" #$name "-" #$version)))
                (copy-recursively "examples" (string-append doc "/examples")))))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (substitute* "test.sh"
                ;; Keep the test logs where --keep-failed can see them.
                (("^(DUC_TEST_DIR=).*" _ assign)
                 (format #f "~a~a/test-directory~%" assign (getcwd)))
                ;; XXX ‘actual size’ differed on my system (a consistent 348160
                ;; bytes where the tests expect 540672).  However, the ‘apparent
                ;; size’ matches, as does the actual test output.  Good enough…?
                ((" [0-9]*B actual") " [0-9]*B actual"))
              (when tests?
                (invoke "./test.sh"))))))) ; no ‘check’ target
    (native-inputs
     (list autoconf automake libtool pkg-config))
    (inputs
     (list cairo pango tokyocabinet ncurses))
    (home-page "https://duc.zevv.nl")
    (synopsis "Library and suite of tools for inspecting disk usage")
    (description "Duc maintains a database of accumulated sizes of
directories of the file system, and allows you to query this database with
some tools, or create fancy graphs showing you where your bytes are.

Duc comes with console utilities, ncurses and X11 user interfaces and a CGI
wrapper for disk usage querying and visualisation.")
    (license license:lgpl3+)))

(define-public qdirstat
  (package
    (name "qdirstat")
    (version "1.8.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/shundhammer/qdirstat")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "04vpdlwk01kgmc4r5rnrmrgd4sf2kfh1rjzb2rjkfxdd4pbghsy9"))))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (system* "qmake"
                      (string-append "INSTALL_PREFIX="
                                     (assoc-ref outputs "out")))))
         (add-after 'install 'wrap
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (wrap-program (string-append
                            (assoc-ref outputs "out")
                            "/bin/qdirstat-cache-writer")
               `("PERL5LIB" ":" prefix
                 (,(string-append
                    (assoc-ref inputs "perl-uri-escape")
                    "/lib/perl5/site_perl")))))))))
    (build-system qt-build-system)
    (inputs
     (list bash-minimal
           perl
           perl-uri-escape
           qtbase-5
           qtwayland-5
           zlib))
    (synopsis "Storage utilisation visualization tool")
    (description
     "QDirStat is a graphical application to show where your disk space has
gone and to help you to clean it up.")
    (home-page "https://github.com/shundhammer/qdirstat")
    (license license:gpl2)))

(define-public nwipe
  (package
    (name "nwipe")
    (version "0.36")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/martijnvanbrummelen/nwipe")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "075zwakv3lva46v6wab00l5x7bs2k73mzzhwlbvm839r8a6gm0dh"))))
    (build-system gnu-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-after 'install 'wrap
                 (lambda* (#:key inputs outputs #:allow-other-keys)
                   (wrap-program (search-input-file outputs "bin/nwipe")
                     (list "PATH" ":" 'prefix
                           (map (lambda (p) (dirname (search-input-file inputs p)))
                                '("sbin/dmidecode"
                                  "sbin/hdparm"
                                  "sbin/smartctl")))))))))
    (inputs
     (list bash-minimal
           dmidecode
           hdparm
           libconfig
           ncurses
           parted
           smartmontools))
    (native-inputs
     (list autoconf automake libtool pkg-config))
    (home-page "https://github.com/martijnvanbrummelen/nwipe")
    (synopsis "Secure disk wiping utility")
    (description
     "@command{nwipe} securely erases disks using a variety of methods to
ensure the data cannot be recovered.  It can wipe multiple drives in parallel
and can be used noninteractively or with a text-based user interface.")
    (license
     (list license:gpl2
           license:bsd-3 ; mt19937ar-cok
           license:public-domain)))) ; {isaac_rand,PDFGen}

(define-public wipe
  (package
    (name "wipe")
    (version "2.3.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/wipe/wipe/" version
                                  "/wipe-" version ".tar.bz2"))
              (sha256
               (base32
                "180snqvh6k6il6prb19fncflf2jcvkihlb4w84sbndcv1wvicfa6"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f                       ;no tests
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-makefile
            (lambda _
              (substitute* "Makefile.in"
                ;; The Makefile.in uses install -o root, but during the
                ;; build there is no root user, so if we leave that in,
                ;; the build fails with the following error:
                ;; /gnu/[...]/install: invalid user ‘root’
                (("-o root") "")
                ;; It's up to the distribution to strip the binaries or
                ;; not.
                (("\\$\\(INSTALL_BIN\\) -s ")
                 "$(INSTALL_BIN) "))))
          (add-after 'unpack 'force-autotools-bootstrap
            (lambda _
              ;; Rebuild the build system scripts, as the ones in bundles are
              ;; very old and do not support all the options used by Guix.
              (delete-file "configure"))))))
    (native-inputs (list autoconf automake libtool))
    (home-page "https://wipe.sourceforge.net")
    (synopsis "Secure file/block device wiping utility")
    (description
     "Wipe can erase files and block devices securely.  To work properly it
relies on several assumptions like having the block device write the correct
sectors, etc.  For files it also doesn't work on log-structured file systems
such as F2FS, JFFS, LogFS, etc.  You should @emph{not} trust @command{wipe} to
work as advertised until you have manually verified that all its assumption
hold true on your system.  To overwrite data it uses the Mersenne Twister
pseudo-random number generator (PRNG) that is seeded with @file{/dev/urandom}
or, if unavailable, @file{/dev/random}.")
    (license license:gpl2+)))
