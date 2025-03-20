;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2018, 2020–2022, 2024 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Gábor Boskovits <boskovits@gmail.com>
;;; Copyright © 2017, 2018, 2021, 2023 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2018 Leo Famulari <leo@famulari.name>
;;; Copyright © 2019-2023, 2025 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2020 Raghav Gururajan <raghavgururajan@disroot.org>
;;; Copyright © 2020 Morgan Smith <Morgan.J.Smith@outlook.com>
;;; Copyright © 2021 raid5atemyhomework <raid5atemyhomework@protonmail.com>
;;; Copyright © 2021 Stefan Reichör <stefan@xsteve.at>
;;; Copyright © 2021 Noisytoot <noisytoot@disroot.org>
;;; Copyright © 2021, 2023 Kaelyn Takata <kaelyn.alexi@protonmail.com>
;;; Copyright © 2022 Brian Cully <bjc@spork.org>
;;; Copyright © 2023 Aaron Covrig <aaron.covrig.us@ieee.org>
;;; Copyright © 2024 Ahmad Draidi <a.r.draidi@redscript.org>
;;; Copyright © 2024 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2025 Julian Flake <flake@uni-koblenz.de>
;;; Copyright © 2025 Ashish SHUKLA <ashish.is@lostca.se>
;;; Copyright © 2020-2025 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;
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

(define-module (gnu packages file-systems)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix platform)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (guix build-system linux-module)
  #:use-module (guix build-system python)
  #:use-module (guix build-system trivial)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages acl)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages attr)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages cyrus-sasl)
  #:use-module (gnu packages datastructures)
  #:use-module (gnu packages digest)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-check)
  #:use-module (gnu packages golang-crypto)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages jemalloc)
  #:use-module (gnu packages kerberos)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages libunwind)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages man)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages nfs)
  #:use-module (gnu packages onc-rpc)
  #:use-module (gnu packages openldap)
  #:use-module (gnu packages password-utils)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages photo)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages popt)
  #:use-module (gnu packages pretty-print)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages rsync)
  #:use-module (gnu packages sssd)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages valgrind)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml))

(define-public autofs
  (package
    (name "autofs")
    (version "5.1.8")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kernel.org/linux/daemons/autofs/"
                           "v" (version-major version) "/"
                           "autofs-" version ".tar.xz"))
       (sha256
        (base32 "1zf0fgf6kr9amxq5amlgsp1v13sizwl3wvx2xl7b4r2nhmci0gdk"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list "--enable-ignore-busy"     ; during shutdown
             "--enable-sloppy-mount"    ; support mount(8) -s
             "--with-libtirpc"
             (string-append "--with-openldap="
                            (assoc-ref %build-inputs "openldap"))
             (string-append "--with-sasl="
                            (assoc-ref %build-inputs "cyrus-sasl"))
             "HAVE_SSS_AUTOFS=1"        ; required to make sssldir click
             (string-append "sssldir="
                            (assoc-ref %build-inputs "sssd")
                            "/lib/sssd/modules"))
       #:tests? #f                      ; no test suite
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'fix-hard-coded-search-path
           (lambda _
             (substitute* "configure"
               (("^searchpath=\".*\"")
                "searchpath=\"$PATH\""))))
         (add-before 'configure 'fix-rpath
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute* "Makefile.rules"
                 (("^AUTOFS_LIB_LINK.*=" match)
                  (string-append match " -Wl,-rpath=" out "/lib"))))))
         (add-before 'install 'omit-obsolete-lookup_nis.so-link
           ;; Building lookup_yp.so depends on $(YPCLNT) but this doesn't,
           ;; leading to a make error.  Since it's broken, comment it out.
           (lambda _
             (substitute* "modules/Makefile"
               (("ln -fs lookup_yp.so" match)
                (string-append "# " match))))))))
    (native-inputs
     (list bison flex pkg-config rpcsvc-proto))
    (inputs
     (list cyrus-sasl
           e2fsprogs ; for e[234]fsck
           libtirpc
           libxml2 ; needed for LDAP, SASL
           mit-krb5 ; needed for LDAP, SASL
           nfs-utils ; for mount.nfs
           openldap
           openssl ; needed for SASL
           sssd
           util-linux))     ; for mount, umount
    ;; XXX A directory index is the closest thing this has to a home page.
    (home-page "https://www.kernel.org/pub/linux/daemons/autofs/")
    (synopsis "Kernel-based automounter for Linux")
    (description
     "Autofs is a kernel-based automounter for use with the Linux autofs4
module.  It automatically mounts selected file systems when they are used and
unmounts them after a set period of inactivity.  This provides
centrally-managed, consistent file names for users and applications, even in a
large and/or frequently changing (network) environment.")
    ;; fedfs/ is GPL-2-only but not built.
    (license (list license:bsd-3        ; modules/cyrus-sasl.c
                   license:gpl2+))))    ; the rest

(define-public bindfs
  (package
    (name "bindfs")
    (version "1.17.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://bindfs.org/downloads/bindfs-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1k1xkyjk8ms11djbhlmykkzfbcids6ls5vpq7rhdnazcladszm3g"))))
    (build-system gnu-build-system)
    (arguments
     ;; XXX: The tests have no hope of passing until there is a "nogroup"
     ;; entry (or at least some group to which the guix builder does
     ;; not belong) in the /etc/group file of the build environment.
     ;; Currently we do not have such a group.  Disable tests for now.
     '(#:tests? #f))
    (native-inputs
       ;; Native inputs to run the tests
       ;; ("ruby" ,ruby)
       ;; ("valgrind" ,valgrind)
       ;; ("which" ,which)
     (list pkg-config))
    (inputs
     (list fuse-2))
    (home-page "https://bindfs.org")
    (synopsis "Bind mount a directory and alter permission bits")
    (description
     "@command{bindfs} is a FUSE file system for mounting a directory to
another location, similar to @command{mount --bind}.  It can be used for:
@itemize
@item Making a directory read-only.
@item Making all executables non-executable.
@item Sharing a directory with a list of users (or groups).
@item Modifying permission bits using rules with chmod-like syntax.
@item Changing the permissions with which files are created.
@end itemize")
    (license license:gpl2+)))

(define-public cachefilesd-inotify
  (package
    (name "cachefilesd-inotify")
    (version "0.11.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/tomalok/cachefilesd-inotify")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0qkrpz69ql6fb3fwh0l35hhf9znnqyxhgv5fzd1gl2a2kz13rq5a"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list (string-append "CC=" ,(cc-for-target))
             ;; The Makefile doesn't support prefix= or similar.
             (string-append "DESTDIR=" (assoc-ref %outputs "out"))
             "MANDIR=/share/man")
       #:tests? #f                      ; no test suite
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))         ; no configure script
    (home-page "https://gitlab.com/tomalok/cachefilesd-inotify")
    (synopsis
     "CacheFiles file system cache management daemon (using @code{inotify})")
    (description
     "This package provides the user space component of CacheFiles, a caching
back end that uses a directory on a locally mounted file system (such as ext4)
as a cache to speed up (by reducing) access to a slower file system and make it
appear more reliable.

The cached file system is often a network file system such as NFS or CIFS, but
can also be a local file system like ISO 9660 on a slow optical drive.

CacheFiles itself is part of the kernel but relies on this user space
@command{cachefilesd} daemon to perform maintenance tasks like culling and
reaping stale nodes.  Only one such daemon can be running at a time, and
communicates with the kernel through the @file{/dev/cachefiles} character
device.

This version modifies David Howells original cachefilesd---which appears
unmaintained---to use the @code{inotify} API instead of the deprecated
@code{dnotify} to monitor file changes.")
    (license license:gpl2+)))

(define-public avfs
  (package
    (name "avfs")
    (version "1.1.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/avf/avfs/" version
                                  "/avfs-" version ".tar.bz2"))
              (sha256
               (base32
                "1kvjaaj2dlps98alpc8rhnzhk4vriw46f3y7b2h0jq2d21j3p7xd"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--enable-library" "--enable-fuse")))
    (native-inputs (list pkg-config))
    (inputs (list xz fuse-2))
    (synopsis "Virtual file system that allows browsing of compressed files")
    (description
     "AVFS is a FUSE-based filesystem that allows browsing of compressed
files.  It provides the @command{mountavfs} command that starts a small
@command{avfsd} daemon.  When a specially formatted path under @file{~/.avfs}
is accessed, the daemon provides listings and content access on the fly.  The
canonical form of virtual file name is:

@example
[basepath]#handler[options][:parameters][/internalpath]
@end example

Example file names:
@itemize
@item @file{~/.avfs/home/user/archive.tar.gz#ugz#utar/path/file}
@item @file{~/.avfs/#http:localhost|some|path}
@end itemize

@code{emacs-dired-hacks} has @code{dired-avfs} module which enables seamless
integration with @code{avfs}.")
    (home-page "https://avf.sourceforge.net")
    (license license:gpl2+)))

(define-public davfs2
  (package
    (name "davfs2")
    (version "1.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://download.savannah.nongnu.org/releases/"
                           "davfs2/davfs2-" version ".tar.gz"))
       (sha256
        (base32 "1b5izj2qivys6nkqjy08nznjwszar8d46ajmw5cf5jvkcw6dv3i9"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list "--sysconfdir=/etc"        ; so man pages & binaries contain /etc
             (string-append "--docdir=" (assoc-ref %outputs "out")
                            "/share/doc/" ,name "-" ,version)
             (string-append "ssbindir=" (assoc-ref %outputs "out") "/sbin")
             ;; The default ‘davfs2’ user and group don't exist on most systems.
             "dav_user=nobody"
             "dav_group=nogroup")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'omit-redundancy
           ;; Don't install redundant copies of /etc examples into /share.
           (lambda _
             (substitute* "etc/Makefile.in"
               (("(dist_pkgdata_DATA =.*) davfs2.conf secrets(.*)"
                 _ prefix suffix)
                (string-append prefix suffix)))))
         (add-after 'unpack 'patch-file-names
           (lambda _
             ;; Don't auto-load the FUSE kernel module.  That's up to root.
             ;; XXX If/when we restore the previous behaviour, make sure not
             ;; to introduce a security hole when mount.davfs is setuid.
             (substitute* "src/kernel_interface.c"
               (("/sbin/modprobe") "/modprobe/disabled"))))
         (replace 'install
           (lambda* (#:key make-flags outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (apply invoke "make" "install"
                      (string-append "pkgsysconfdir=" out "/etc")
                      make-flags)))))))
    (inputs
     (list neon
           ;; Neon requires but doesn't propagate zlib, nor would we want that.
           ;; XZ as well, but that's already present in the build environment.
           zlib))
    (home-page "https://savannah.nongnu.org/projects/davfs2")
    (synopsis "Mount remote WebDAV resources in the local file system")
    (description
     "The @acronym{WebDAV, Web Distributed Authoring and Versioning} extension
to the HTTP protocol defines a standard way to author resources on a remote Web
server.  Davfs2 exposes such resources as a typical file system which can be
used by standard applications with no built-in support for WebDAV, such as the
GNU coreutils (@command{cp}, @command{mv}, etc.) or a graphical word processor.

Davfs2 works with most WebDAV servers with no or little configuration.  It
supports TLS (HTTPS), HTTP proxies, HTTP basic and digest authentication, and
client certificates.  It performs extensive caching to avoid unnecessary network
traffic, stay responsive even over slow or unreliable connections, and prevent
data loss.  It aims to make use by unprivileged users as easy and secure as
possible.

However, davfs2 is not a full-featured WebDAV client.  The file system interface
and the WebDAV protocol are quite different.  Translating between the two is not
always possible.")
    (license (list license:bsd-2        ; src/fuse_kernel.h
                   license:gpl3+))))    ; everything else

(define-public exfat-utils
  (package
    (name "exfat-utils")
    (version "1.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/relan/exfat/releases/download/v"
             version "/exfat-utils-" version ".tar.gz"))
       (sha256
        (base32 "0sdzflmwcxjjliq1yqhidy46kbkvj16kxrbrgsj0ci0hjgx7a594"))))
    (build-system gnu-build-system)
    (home-page "https://github.com/relan/exfat")
    (synopsis "Utilities to manipulate exFAT file systems")
    (description
     "This package provides an implementation of the exFAT file system,
including command-line tools to validate exFAT file systems and to create new
ones.")
    (license license:gpl2+)))

(define-public fsarchiver
  (package
    (name "fsarchiver")
    (version "0.8.7")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/fdupoux/fsarchiver")
         (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1vy8ay0fn32i298bx9scqghi7xm9z2101zxk5xshbrkl00b2m4nm"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake pkg-config))
    (inputs
     (list bzip2
           e2fsprogs
           libgcrypt
           lz4
           lzo
           `(,util-linux "lib")
           xz
           zlib
           `(,zstd "lib")))
    (synopsis "File system back-up, deployment, and migration tool")
    (description
     "FSArchiver saves the contents of a file system to a compressed archive
file, and restores it to a different file system and/or partition.  This
partition can be of a different size than the original and FSArchiver will
create a new file system if none exists.

All standard file attributes supported by the kernel are preserved, including
file permissions, timestamps, symbolic and hard links, and extended attributes.

Each file in the archive is protected by a checksum.  If part of the archive
is corrupted you'll lose the affected file(s) but not the whole back-up.")
    (home-page "https://www.fsarchiver.org/")
    (license license:gpl2)))

(define-public fstransform
  (package
    (name "fstransform")
    (version "0.9.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cosmos72/fstransform")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0vhypb6pbghja95av62ds4mhldbg0h8g4yg94k9r7lsxm7bgpfr3"))))
    (build-system gnu-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-after 'install 'install-README
                 ;; There are no man pages.  Install a poor substitute.
                 (lambda* (#:key outputs #:allow-other-keys)
                   (let* ((out (assoc-ref outputs "out"))
                          (doc (string-append out "/share/doc/" #$name)))
                     (install-file "README" doc)))))))
    (inputs (list e2fsprogs))
    (home-page "https://github.com/cosmos72/fstransform")
    (synopsis "Convert file system types in place without copying all data")
    (description
     "This package provides several command-line tools to transform a supported
file system, such as XFS, into one of a different supported type, such as ext4.
All existing file contents, names, and directories are preserved.

The conversion happens @dfn{in place}, without the need to create a complete
copy of the original data.  This lets you transform almost full file systems on
systems where adding (sufficient) additional storage space is not an option.

Do @emph{not} use this package when you could simply create an empty file system
from scratch and restore from a back-up.  Transformation is limited, slow, and
significantly increases the risk of irreversible data loss!")
    ;; Please follow <https://github.com/cosmos72/fstransform/issues/46>.
    (license (list license:gpl2         ; fsattr/src/e4attr.* → sbin/fsattr
                   license:gpl3+))))    ; the rest

(define-public gocryptfs
  (package
    (name "gocryptfs")
    (version "2.5.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/rfjakob/gocryptfs")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0ai30h56qvp31a3rl72biwx8w9blmi7va7d1bflmxbp41zhl6dn9"))))
    (build-system go-build-system)
    (arguments
     (list
      #:install-source? #f
      #:import-path "github.com/rfjakob/gocryptfs"
      #:build-flags
      #~(list
         "-ldflags" (string-append
                     "-X main.GitVersion=" #$version
                     " -X main.GitVersionFuse=" #$(package-version
                                                   go-github-com-hanwen-go-fuse-v2)
                     " -X main.BuildDate=" "[reproducible]"))
      #:test-flags
      #~(list "-skip" (string-join
                       (list "TestPrepareAtSyscall"
                             "TestPrepareAtSyscallPlaintextnames"
                             "TestGetdents")
                       "|"))
      ;; XXX: Test suit requires a root access to mount, limit to some unit
      ;; tests, figure out how to enable most of the them.
      #:test-subdirs #~(list "internal/...")
      #:phases
      #~(modify-phases %standard-phases
          ;; after 'check phase, should maybe unmount leftover mounts as in
          ;; https://github.com/rfjakob/gocryptfs/blob/a55b3cc15a6d9bce116a90f33df4bc99d9dd6a10/test.bash#L28
          (add-after 'unpack 'fix-paths
            (lambda* (#:key import-path #:allow-other-keys)
              (let* ((fusermount3 "/run/setuid-programs/fusermount3"))
                (substitute* (format #f "src/~a/mount.go" import-path)
                  (("/bin/fusermount") fusermount3)))))
          (replace 'build
            (lambda arguments
              (for-each
               (lambda (directory)
                 (apply (assoc-ref %standard-phases 'build)
                        (append arguments (list #:import-path directory))))
               (list
                "github.com/rfjakob/gocryptfs"
                "github.com/rfjakob/gocryptfs/gocryptfs-xray"
                "github.com/rfjakob/gocryptfs/contrib/statfs"
                "github.com/rfjakob/gocryptfs/contrib/findholes"
                "github.com/rfjakob/gocryptfs/contrib/atomicrename")))))))
    (native-inputs (list
                    go-github-com-aperturerobotics-jacobsa-crypto
                    go-github-com-hanwen-go-fuse-v2
                    go-github-com-moby-sys-mountinfo
                    go-github-com-pkg-xattr
                    go-github-com-rfjakob-eme
                    go-github-com-sabhiram-go-gitignore
                    go-github-com-spf13-pflag
                    go-golang-org-x-crypto
                    go-golang-org-x-sys
                    go-golang-org-x-term
                    openssl
                    pkg-config))
    (inputs (list
             fuse))
    (home-page "https://github.com/rfjakob/gocryptfs")
    (synopsis "Encrypted overlay filesystem")
    (description
     "Gocryptfs is an encrypted overlay filesystem written in Go.  It
features a file-based encryption that is implemented as a mountable
FUSE filesystem.

Gocryptfs was inspired by EncFS and strives to fix its security issues
while providing good performance.  Gocryptfs is as fast as EncFS in the
default mode and significantly faster than paranoia mode in EncFS,
which provides a security level comparable to Gocryptfs.

On CPUs without AES-NI, gocryptfs uses OpenSSL through a thin wrapper
called stupidgcm.  This provides a 4x speedup compared to Go's builtin
AES-GCM implementation.")
    (license license:expat)))

(define-public gphotofs
  (package
    (name "gphotofs")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://sourceforge/gphoto/gphotofs/" version
                       "/gphotofs-0.5.tar.gz"))
       (sha256
        (base32
         "04slwhr6ap9xcc27wphk22ad8yn79ngyy5z10lxams3k5liahvc2"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list fuse-2 glib libgphoto2))
    (synopsis "Virtual file system for libgphoto2 using FUSE")
    (description "GPhotoFS is a FUSE file system module to mount your camera as
a file system on Linux.  This allow using your camera with any tool able to read
from a mounted file system.")
    (home-page "http://www.gphoto.org/proj/gphotofs/")
    (license license:gpl2+)))

(define bcachefs-tools-rust-target
  (platform-rust-target (lookup-platform-by-target-or-system
                         (or (%current-target-system)
                             (%current-system)))))

(define bcachefs-tools-target/release
  (string-append "target/" bcachefs-tools-rust-target "/release"))

(define bcachefs-tools-cargo-args
  ;; Distinct from -MAKE-FLAGS for use with ‘cargo test’ in 'check.
  #~(list "--release"
          (string-append "--target=" #$bcachefs-tools-rust-target)))

;; XXX We want to share common make flags across different packages & phases,
;; but the cargo-build-system doesn't allow #:make-flags.
(define bcachefs-tools-make-flags
  ;; These result of these flags should be as minimal as possible.
  ;; Enable any optional features in the bcachefs-tools package instead.
  #~(list (string-append "CARGO_BUILD_ARGS="
                         (string-join #$bcachefs-tools-cargo-args " "))
          (string-append "CC=" #$(cc-for-target))
          (string-append "PKG_CONFIG=" #$(pkg-config-for-target))))

(define bcachefs-tools-make-install-flags
  #~(cons* (string-append "PREFIX=" #$output)
           "INITRAMFS_DIR=$(PREFIX)/share/initramfs-tools"
           "PKGCONFIG_UDEVRULESDIR=$(PREFIX)/lib/udev/rules.d"
           #$bcachefs-tools-make-flags))

(define bcachefs-tools-minimal
  ;; This ‘minimal’ package is not *that* minimal, and not different enough to
  ;; export.  It's used as-is only when cross-compiling, as an input to the
  ;; public bcachefs-tools package that inherits from it to generate the shell
  ;; completions by running a native bcachefs binary at build time.
  (package
    (name "bcachefs-tools-minimal")
    (version "1.11.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://evilpiepirate.org/git/bcachefs-tools.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0m6z8z1cv78ay9yspypgr0kv70ck4wpln5n44f6n57i7sihqhrrg"))))
    (build-system cargo-build-system)
    (arguments
     (list
      #:install-source? #f
      ;; The Makefile CCs *every* C file anywhere beneath the build directory,
      ;; even in Rust crates, creating ludicrous and totally bogus dependencies
      ;; such as the Android SDK.  Put our crates elsewhere.
      #:vendor-dir "../guix-vendor"
      #:cargo-inputs
      `(("rust-aho-corasick" ,rust-aho-corasick-1)
        ("rust-anstream" ,rust-anstream-0.6)
        ("rust-anstyle" ,rust-anstyle-1)
        ("rust-anstyle-parse" ,rust-anstyle-parse-0.2)
        ("rust-anyhow" ,rust-anyhow-1)
        ("rust-autocfg" ,rust-autocfg-1)
        ("rust-bitfield" ,rust-bitfield-0.14)
        ("rust-clap" ,rust-clap-4)
        ("rust-clap-complete" ,rust-clap-complete-4)
        ("rust-either" ,rust-either-1)
        ("rust-errno" ,rust-errno-0.2)
        ("rust-env-logger" ,rust-env-logger-0.10)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-log" ,rust-log-0.4)
        ("rust-memoffset" ,rust-memoffset-0.8)
        ("rust-owo-colors" ,rust-owo-colors-4)
        ("rust-rustix" ,rust-rustix-0.38)
        ("rust-strum" ,rust-strum-0.26)
        ("rust-strum-macros" ,rust-strum-macros-0.26)
        ("rust-udev" ,rust-udev-0.7)
        ("rust-uuid" ,rust-uuid-1)
        ("rust-zeroize" ,rust-zeroize-1))
      #:phases
      #~(modify-phases %standard-phases
          (replace 'build
            (lambda* (#:key parallel-build? #:allow-other-keys)
              (apply invoke "make"
                     "-j" (if parallel-build?
                              (number->string (parallel-job-count))
                              "1")
                     (string-append "VERSION=" #$version)
                     #$bcachefs-tools-make-flags)))
          (add-before 'install 'patch-install
            ;; ‘make install’ hard-codes target/release/bcachefs, which is
            ;; incorrect when passing --target, as required to cross-compile or
            ;; even just link statically.  We always pass it, so always patch.
            (lambda _
              (substitute* "Makefile"
                (("target/release")
                 #$bcachefs-tools-target/release))))
          (replace 'install
            (lambda _
              (apply invoke "make" "install"
                     #$bcachefs-tools-make-install-flags))))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list clang
           eudev
           keyutils
           libaio
           libscrypt
           libsodium
           liburcu
           `(,util-linux "lib")         ;libblkid
           lz4
           zlib
           `(,zstd "lib")))
    (home-page "https://bcachefs.org/")
    (synopsis "Tools to create and manage bcachefs file systems")
    (description
     "This package provides the @command{bcachefs} command-line tool with many
subcommands for creating, checking, and otherwise managing bcachefs file
systems.  Traditional aliases like @command{mkfs.bcachefs} are also included.

@dfn{Bcachefs} is a @acronym{CoW, copy-on-write} file system supporting native
encryption, compression, snapshots, and (meta)data checksums.  It can use
multiple block devices for replication and/or performance, similar to RAID.

In addition, bcachefs provides all the functionality of bcache, a block-layer
caching system, and lets you assign different roles to each device based on its
performance and other characteristics.")
    (license license:gpl2+)))

(define-public bcachefs-tools
  ;; The final public package with shell completion even when cross-compiling,
  ;; as well as optional features such as FUSE (‘bcachefs fusemount’).
  (package
    (inherit bcachefs-tools-minimal)
    (name "bcachefs-tools")
    (arguments
     (substitute-keyword-arguments
	 (package-arguments bcachefs-tools-minimal)
       ((#:modules modules '())
	`(,@modules
	  (guix build cargo-build-system)
          (guix build utils)
          (srfi srfi-26)))
       ((#:phases phases #~%standard-phases)
	#~(modify-phases #$phases
            (add-before 'build 'enable-fuse
              (lambda _
                ;; This must be an environment variable, not a make flag!
                (setenv "BCACHEFS_FUSE" "1")))
	    (add-after 'install 'install-completions
              (lambda* (#:key native-inputs #:allow-other-keys)
                (define bcachefs
                  (or (false-if-exception (search-input-file native-inputs
                                                             "sbin/bcachefs"))
                      (string-append #$bcachefs-tools-target/release
                                     "/bcachefs")))

                (define (output-completions shell file)
                  (let ((output (string-append #$output "/" file)))
                    (mkdir-p (dirname output))
                    (with-output-to-file output
                      (lambda _
                        (invoke bcachefs "completions" shell)))))

                (for-each (cut apply output-completions <>)
                          '(("bash"
                             "share/bash-completion/completions/bcachefs")
                            ("fish"
                             "share/fish/vendor_completions.d/bcachefs.fish")
                            ("zsh"
                             "share/zsh/site-functions/_bcachefs")))))))))
    (native-inputs
     (append (package-native-inputs bcachefs-tools-minimal)
	     (if (%current-target-system)
		 (list bcachefs-tools-minimal)
		 (list))))
    (inputs
     (modify-inputs (package-inputs bcachefs-tools-minimal)
       (append fuse)))))

(define-public bcachefs-tools-minimal/static
  ;; The static variant is public for consistency with the other file system
  ;; tools packages, but ours is based on the private minimal package.  We
  ;; don't need/want a bcachefs with FUSE support in the initrd, and nobody
  ;; is likely to complain about the lack of a non-minimal bcachefs-static…
  (package
    (inherit bcachefs-tools-minimal)
    (name "bcachefs-tools-minimal-static")
    (arguments
     (substitute-keyword-arguments (package-arguments bcachefs-tools-minimal)
       ((#:phases phases #~%standard-phases)
        #~(modify-phases #$phases
            (add-after 'configure 'set-rust-flags
              (lambda _
                (setenv "RUSTFLAGS" (string-join
                                     '("-C" "link-arg=-z"
                                       "-C" "link-arg=muldefs"
                                       "-C" "target-feature=+crt-static"
                                       "-C" "relocation-model=static")
                                     " "))))
            (replace 'build
              (lambda* (#:key parallel-build? #:allow-other-keys)
                (apply invoke "make"
                       "-j" (if parallel-build?
                                (number->string (parallel-job-count))
                                "1")
                       (string-append "VERSION="
                                      #$(package-version this-package))
                       #$bcachefs-tools-make-flags)))
            (replace 'check
              (lambda* (#:key tests? #:allow-other-keys)
                (when tests?
                  (apply invoke "cargo" "test" #$bcachefs-tools-cargo-args))))
            (replace 'install
              (lambda _
                (apply invoke "make" "install"
                       (string-append "PREFIX=" #$output)
                       #$bcachefs-tools-make-install-flags)))))))
    (inputs (modify-inputs (package-inputs bcachefs-tools-minimal)
              (prepend `(,eudev "static")
                       `(,keyutils "static")
                       `(,libscrypt "static")
                       `(,lz4 "static")
                       `(,util-linux "static")
                       `(,zlib "static")
                       `(,zstd "static"))))
    (synopsis "Statically-linked, minimal variant of bcachefs-tools")))

(define-public bcachefs/static
  (package
    (name "bcachefs-static")
    (version (package-version bcachefs-tools-minimal/static))
    (source #f)
    (build-system trivial-build-system)
    (arguments
     (list #:modules '((guix build utils))
           #:builder
           #~(begin
               (use-modules (guix build utils))
               (let ((target (string-append #$output "/sbin/bcachefs")))
                 (install-file (search-input-file %build-inputs "sbin/bcachefs")
                               (dirname target))
                 (remove-store-references target)))))
    (inputs
     (list bcachefs-tools-minimal/static))
    (home-page (package-home-page bcachefs-tools-minimal/static))
    (synopsis "Statically-linked bcachefs command from bcachefs-tools")
    (description
     "This package provides the statically-linked @command{bcachefs} from a
minimal bcachefs-tools package.  It is meant to be used in initrds.")
    (license (package-license bcachefs-tools-minimal/static))))

(define-public exfatprogs
  (package
    (name "exfatprogs")
    (version "1.2.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/exfatprogs/exfatprogs")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0plj52kjvhy94hdk0bq8bc7ql6yh44x76kryxhn46vwbxayv790j"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list "--disable-static")
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (setenv "FSCK1" "../fsck/fsck.exfat")
                ;; Upstream CI uses a second FSCK provided by its host operating
                ;; system to verify the results of the newly-built one.  That
                ;; makes no sense in Guix, but we can detect crashes, unexpected
                ;; inconsistencies, and other badness by testing with only one.
                (setenv "FSCK2" (getenv "FSCK1"))
                (with-directory-excursion "tests"
                  (invoke "./test_fsck.sh"))))))))
    (native-inputs
     (list autoconf automake libtool pkg-config))
    (home-page "https://github.com/exfatprogs/exfatprogs")
    (synopsis "Tools to create, check, and repair exFAT file systems")
    (description
     "These are command-line user space tools for the @acronym{exFAT,
Extensible File Allocation Table} file systems.  Included are
@command{mkfs.exfat} to create (format) new exFAT file systems, and
@command{fsck.exfat} to check their consistency and repair them.")
    (license license:gpl2+)))

(define-public httpfs2
  (package
    (name "httpfs2")
    (version "0.1.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/httpfs/httpfs2/"
                           "httpfs2-" version ".tar.gz"))
       (sha256
        (base32
         "1h8ggvhw30n2r6w11n1s458ypggdqx6ldwd61ma4yd7binrlpjq1"))))
    (build-system gnu-build-system)
    (native-inputs
     (list asciidoc docbook-xml libxml2 libxslt pkg-config))
    (inputs
     (list fuse-2 gnutls))
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (delete 'configure)      ; no configure script
               (replace 'install
                 ;; There's no ‘install’ target. Install all variants manually.
                 (lambda _
                   (let* ((bin (string-append #$output "/bin"))
                          (man1 (string-append #$output "/share/man/man1")))
                     (mkdir-p bin)
                     (mkdir-p man1)
                     (for-each
                      (lambda (variant)
                        (let ((man1-page (string-append variant ".1")))
                          (install-file variant bin)
                          (install-file man1-page man1)))
                      (list "httpfs2"
                            "httpfs2-mt"
                            "httpfs2-ssl"
                            "httpfs2-ssl-mt"))))))
           #:make-flags
           #~(list (string-append "CC=" #$(cc-for-target)))
           #:parallel-build? #f         ; can result in missing man pages
           #:tests? #f))                ; no tests
    (home-page "https://sourceforge.net/projects/httpfs/")
    (synopsis "Mount remote files over HTTP")
    (description "httpfs2 is a @code{fuse} file system for mounting any
@dfn{HyperText} (HTTP or HTTPS) URL.  It uses HTTP/1.1 byte ranges to request
arbitrary bytes from the web server, without needing to download the entire
file.  This is particularly useful with large archives such as ZIP files and
ISO images when you only need to inspect their contents or extract specific
files.  Since the HTTP protocol itself has no notion of directories, only a
single file can be mounted.")
    (license license:gpl2+)))

(define-public jfsutils
  (package
    (name "jfsutils")
    (version "1.1.15")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://jfs.sourceforge.net/project/pub/jfsutils-"
                           version ".tar.gz"))
       (sha256
        (base32 "0kbsy2sk1jv4m82rxyl25gwrlkzvl3hzdga9gshkxkhm83v1aji4"))
       (patches (search-patches "jfsutils-add-sysmacros.patch"
                                "jfsutils-gcc-compat.patch"
                                "jfsutils-include-systypes.patch"))))
    (build-system gnu-build-system)
    (inputs
     (list `(,util-linux "lib")))
    (home-page "https://jfs.sourceforge.net/home.html")
    (synopsis "Utilities for managing JFS file systems")
    (description
     "The JFSutils are a collection of utilities for managing the @acronym{JFS,
Journaled File System}, a 64-bit journaling file system created by IBM and later
ported to the kernel Linux.  The following commands are available:
@enumerate
@item @command{fsck.jfs}: check and repair a JFS file system or replay its
transaction log.
@item @command{logdump}: dump the JFS journal log.
@item @command{logredo}: replay the JFS journal log.
@item @command{mkfs.jfs}: create a new JFS file system.
@item @command{xchklog}: save a JFS fsck log to a file.
@item @command{xchkdmp}: dump the contents of such a log file.
@item @command{xpeek}: a JFS file system editor with a shell-like interface.
@end enumerate\n")
    (license license:gpl3+)))          ; no explicit version given

(define-public jfsutils/static
  (static-package
   (package
     (inherit jfsutils)
     (name "jfsutils-static")
     (inputs
      `(("util-linux:static" ,util-linux "static")
        ,@(package-inputs jfsutils))))))

(define-public jfs_fsck/static
  (package
    (name "jfs_fsck-static")
    (version (package-version jfsutils))
    (source #f)
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils)
                      (ice-9 ftw)
                      (srfi srfi-26))
         (let* ((jfsutils (assoc-ref %build-inputs "jfsutils"))
                (fsck     "jfs_fsck")
                (out      (assoc-ref %outputs "out"))
                (sbin     (string-append out "/sbin")))
           (mkdir-p sbin)
           (with-directory-excursion sbin
             (install-file (string-append jfsutils "/sbin/" fsck)
                           ".")
             (remove-store-references fsck)
             (chmod fsck #o555))
           #t))))
    (inputs
     `(("jfsutils" ,jfsutils/static)))
    (home-page (package-home-page jfsutils))
    (synopsis "Statically-linked jfs_fsck command from jfsutils")
    (description "This package provides statically-linked jfs_fsck command taken
from the jfsutils package.  It is meant to be used in initrds.")
    (license (package-license jfsutils))))

(define-public nilfs-utils
  (package
    (name "nilfs-utils")
    (version "2.2.11")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://nilfs.sourceforge.io/download"
                            "/nilfs-utils-" version ".tar.bz2"))
        (sha256
         (base32 "1k9l5kzhdph3jh04kxz4dn5yb210205iycbnpklrpi6jy1zqj0l6"))))
    (build-system gnu-build-system)
    (arguments
     (list
       #:configure-flags
       #~(list "--enable-static=no")
       #:phases
       #~(modify-phases %standard-phases
           (add-before 'bootstrap 'force-bootstrap
             (lambda _
               (delete-file "configure")
               (substitute* "configure.ac"
                 (("\\[/etc\\]") "[${prefix}/etc]")
                 (("\\[/sbin\\]") "[${prefix}/sbin]")))))))
    (inputs
     (list (list util-linux "lib")))
    (native-inputs (list autoconf automake libtool))
    (home-page "https://nilfs.sourceforge.io/")
    (synopsis "Continuous Snapshotting Filesystem")
    (description
     "NILFS is a log-structured file system supporting versioning of the entire
file system and continuous snapshotting, which allows users to even restore
files mistakenly overwritten or destroyed just a few seconds ago.")
    (license license:gpl3+)))

(define-public disorderfs
  (package
    (name "disorderfs")
    (version "0.5.11")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://salsa.debian.org/reproducible-builds/disorderfs.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1pnrj0h8sgqwgsc18vz3fkqsp6vhigdbi75vdj0si1r6wgslnr7z"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list fuse-2 attr))
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (delete 'configure))  ; no configure script
       #:make-flags (let ((out (assoc-ref %outputs "out")))
                      (list (string-append "PREFIX=" out)))
       #:test-target "test"
       ;; FIXME: Tests require 'run-parts' which is not in Guix yet.
       #:tests? #f))
    (home-page "https://salsa.debian.org/reproducible-builds/disorderfs")
    (synopsis "FUSE file system that introduces non-determinism")
    (description
     "An overlay FUSE file system that introduces non-determinism
into file system metadata.  For example, it can randomize the order
in which directory entries are read.  This is useful for detecting
non-determinism in the build process.")
    (license license:gpl3+)))

(define-public glusterfs
  (package
    (name "glusterfs")
    (version "7.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://download.gluster.org/pub/gluster/glusterfs/"
                           (version-major version) "/"
                           (version-major+minor version) "/"
                           "glusterfs-" version ".tar.gz"))
       (sha256
        (base32
         "0yzhx710ypj0j3m5dcgmmgvkp7p0rmmp2p7ld0axrm4vpwc2b1wa"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (let ((out (assoc-ref %outputs "out"))
             (p2 (assoc-ref %build-inputs "python-2")))
         (list (string-append "PYTHON=" p2 "/bin/python")
               (string-append "--with-initdir=" out "/etc/init.d")
               (string-append "--with-mountutildir=" out "/sbin")
               "--enable-cmocka"  ; unit tests
               ;; "--enable-debug"  ; debug build options
               ;; "--enable-asan"  ; Address Sanitizer
               ;; "--enable-tsan"  ; ThreadSanitizer
               ))
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'autogen
           (lambda _ (invoke "./autogen.sh"))))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("libtirpc" ,libtirpc)
       ("rpcsvc-proto" ,rpcsvc-proto)
       ("python-2" ,python-2) ; must be version 2
       ("flex" ,flex)
       ("bison" ,bison)
       ("libtool" ,libtool)
       ("autoconf" ,autoconf)
       ("automake" ,automake)
       ("cmocka" ,cmocka)))
    (inputs
     `(("acl" ,acl)
       ("fuse" ,fuse-2)
       ("openssl" ,openssl)
       ("liburcu" ,liburcu)
       ("libuuid" ,util-linux "lib")
       ("libxml2" ,libxml2)
       ("readline" ,readline)
       ("zlib" ,zlib)
       ("libaio" ,libaio)
       ("rdma-core" ,rdma-core)))
    (home-page "https://www.gluster.org")
    (synopsis "Distributed file system")
    (description "GlusterFS is a distributed scalable network file system
suitable for data-intensive tasks such as cloud storage and media streaming.
It allows rapid provisioning of additional storage based on your storage
consumption needs.  It incorporates automatic failover as a primary feature.
All of this is accomplished without a centralized metadata server.")
    ;; The user may choose either LGPLv3+ or GPLv2 only.
    (license (list license:lgpl3+ license:gpl2+))))

(define-public curlftpfs
  (package
    (name "curlftpfs")
    (version "0.9.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/curlftpfs/curlftpfs/" version
                           "/curlftpfs-" version ".tar.gz"))
       (sha256
        (base32 "0n397hmv21jsr1j7zx3m21i7ryscdhkdsyqpvvns12q7qwwlgd2f"))
       (patches
        (search-patches "curlftpfs-fix-error-closing-file.patch"
                        "curlftpfs-fix-file-names.patch"
                        "curlftpfs-fix-memory-leak.patch"
                        "curlftpfs-fix-no_verify_hostname.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-test
           (lambda _
             ;; One of the 512-Byte block counts is definitely wrong.
             ;; See <https://sourceforge.net/p/curlftpfs/bugs/73/>.
             (substitute* "tests/ftpfs-ls_unittest.c"
              (("4426192") "12814800"))
             #t)))))
    (inputs
     (list curl glib fuse-2))
    (native-inputs
     (list pkg-config))
    (home-page "https://curlftpfs.sourceforge.net/")
    (synopsis "Mount remote file systems over FTP")
    (description
     "This is a file system client based on the FTP File Transfer Protocol.")
    (license license:gpl2+)))

(define-public libeatmydata
  (package
    (name "libeatmydata")
    (version "131")        ; also update the "debian-files" input if available
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.flamingspork.com/projects/libeatmydata/"
                           "libeatmydata-" version ".tar.gz"))
       (sha256
        (base32 "1i5bp9a2vmljci3ihzlxf8482106di2ayy1lpr0qb8rq472sh66g"))))
    (build-system gnu-build-system)
    (arguments
     ;; All tests pass---but only if the host kernel allows PTRACE_TRACEME.
     `(#:tests? #f
       #:configure-flags
       (list "--disable-static")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-file-names
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* (list "eatmydata.in" "eatmydata.sh.in")
               (("basename|readlink|uname" command)
                (search-input-file inputs
                                   (string-append "bin/" command))))))
         (add-before 'patch-file-names 'tighten-symlink-mode
           ;; When the ‘eatmydata’ helper detects that it's a symlink, it will
           ;; transparently invoke the command of the same name.  However, it's
           ;; *always* a link in Guix profiles and doesn't handle that well.
           ;; Patch it to treat its own $name specially.
           (lambda _
             (substitute* "eatmydata.in"
               (("-L \"\\$0\"" match)
                (string-append match " ] && [ "
                               "\"x$(basename \"$0\")\" != \"x$name\"")))))
         (add-after 'install 'install-debian-files
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((debian (assoc-ref inputs "debian-files"))
                    (out    (assoc-ref outputs "out"))
                    (share  (string-append out "/share")))
               (invoke "tar" "xvf" debian)
               (with-directory-excursion "debian"
                 (install-file "eatmydata.1" (string-append share "/man/man1"))
                 (install-file "eatmydata.bash-completion"
                               (string-append share "/bash-completion"
                                              "/completions")))))))))
    (native-inputs
     `(("debian-files"                  ; for the man page
        ,(origin
           ;; Debian being what it is, its version can lag behind a bit.  This
           ;; is tolerable as the man page is general and the command stable.
           (method url-fetch)
           (uri (string-append "https://deb.debian.org/debian/pool/main/"
                               "libe/libeatmydata/libeatmydata_130-2"
                               ".debian.tar.xz"))
           (sha256
            (base32 "1sg9g1nv3wl9ymzz33ig4ns563npkbxj67a64m7p34cc813jl95w"))))
       ;; For the test suite.
       ("strace" ,strace)
       ("which" ,which)))
    (inputs
     (list coreutils))
    (home-page "https://www.flamingspork.com/projects/libeatmydata/")
    (synopsis "Transparently ignore calls to synchronize data safely to disk")
    (description
     "Libeatmydata transparently disables most ways a program might force data
to be written to the file system, such as @code{fsync()} or @code{open(O_SYNC)}.

Such synchronisation calls provide important data integrity guarantees but are
expensive to perform and can significantly slow down software that (over)uses
them.

This price is worth paying if you care about the files being modified---which is
typically the case---or when manipulating important components of your system.
Please, @emph{do not} use something called ``eat my data'' in such cases!

However, it does not make sense to accept this performance hit if the data is
unimportant and you can afford to lose all of it in the event of a crash, for
example when running a software test suite.  Adding @file{libeatmydata.so} to
the @env{LD_PRELOAD} environment of such tasks will override all C library data
synchronisation functions with custom @i{no-op} ones that do nothing and
immediately return success.

A simple @command{eatmydata} script is included that does this for you.")
    (license license:gpl3+)))

(define-public libnfs
  (package
    (name "libnfs")
    (version "4.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/sahlberg/libnfs")
                    (commit (string-append "libnfs-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0i27wd4zvhjz7620q043p4d4mkx8zv2yz9adm1byin47dynahyda"))))
    (build-system gnu-build-system)
    (home-page "https://github.com/sahlberg/libnfs")
    (native-inputs
     (list autoconf automake libtool pkg-config))
    (synopsis "Client library for accessing NFS shares")
    (description "LIBNFS is a client library for accessing NFS shares over a
network.  LIBNFS offers three different APIs, for different use :

@enumerate
@item RAW, a fully asynchronous low level RPC library for NFS protocols.  This
  API provides very flexible and precise control of the RPC issued.
@item NFS ASYNC, a fully asynchronous library for high level vfs functions
@item NFS SYNC, a synchronous library for high level vfs functions.
@end enumerate\n")
    (license (list license:lgpl2.1+ ; library
                   license:gpl3+    ; tests
                   license:bsd-3    ; copied nsf4 files
                   ))))

(define-public apfs-fuse
  (let ((commit "66b86bd525e8cb90f9012543be89b1f092b75cf3")
        (revision "2"))
    (package
      (name "apfs-fuse")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/sgan81/apfs-fuse")
                       (recursive? #t) ; for lzfse
                       (commit commit)))
         (sha256
          (base32 "0f63slyzv8fbgshpzrx2g01x9h73g5yvh5kis4yazl19fjm2b05r"))
         (file-name (git-file-name name version))))
      (build-system cmake-build-system)
      (arguments
       `(#:tests? #f ; No test suite
         #:phases
         (modify-phases %standard-phases
           ;; No 'install' target in CMakeLists.txt
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (bin (string-append out "/bin"))
                      (lib (string-append out "/lib"))
                      (doc (string-append out "/share/doc/"
                                          (string-append ,name "-" ,version))))
                 (install-file "apfs-dump" bin)
                 (install-file "apfs-dump-quick" bin)
                 (install-file "apfs-fuse" bin)
                 (install-file "../source/README.md" doc)))))))
      (inputs
       (list bzip2 fuse zlib))
      (synopsis "Read-only FUSE driver for the APFS file system")
      (description "APFS-FUSE is a read-only FUSE driver for the @dfn{Apple File
System} (APFS).  It is currently in an experimental state — it may not be able
to read all files, and it does not support all the compression methods in
APFS.")
      (home-page "https://github.com/sgan81/apfs-fuse")
      (license license:gpl2+))))

(define-public snapper
  (package
    (name "snapper")
    (version "0.12.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/openSUSE/snapper")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1i5623cnhzivf64zr0g1nlyn9sjgabhyawhpsffykdxvcrnyqn69"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           (delete-file-recursively "dists")
           (delete-file-recursively "zypp-plugin")
           (substitute* '("configure.ac" "doc/Makefile.am")
             ((".*dists.*") "")
             ((".*zypp-plugin.*") ""))
           (substitute* "Makefile.am"
             (("zypp-plugin") ""))))))
    (build-system gnu-build-system)
    (arguments
     (list #:phases #~(modify-phases %standard-phases
                        (add-after 'unpack 'relative-file-locations
                          (lambda* (#:key outputs #:allow-other-keys)
                            (let* ((out (assoc-ref outputs "out")))
                              (substitute* '("scripts/Makefile.am"
                                             "client/systemd-helper/Makefile.am"
                                             "client/installation-helper/Makefile.am"
                                             "data/Makefile.am")
                                (("/usr/share")
                                 (string-append out "/share"))
                                (("/usr/lib")
                                 (string-append out "/lib"))
                                (("/etc/")
                                 (string-append out "/etc/")))
                              (substitute* (cons "data/org.opensuse.Snapper.service"
                                                 (find-files "scripts/" "\\.sh"))
                                (("/usr/bin/snapper")
                                 (string-append out "/bin/snapper"))
                                (("/usr/sbin/snapperd")
                                 (string-append out "/sbin/snapperd"))
                                (("/sbin/btrfs")
                                 (which "btrfs")))
                              (substitute* "scripts/snapper-hourly"
                                (("PATH=.*$")
                                 (format #f "PATH=~a/sbin:~a/bin\n"
                                         out out)))
                              (substitute* (find-files "." "Makefile.am")
                                (("/usr/lib")
                                 "@libdir@"))))))))
    (home-page "https://snapper.io")
    (native-inputs
     (list glibc-locales autoconf automake libtool pkg-config))
    (inputs
     (list btrfs-progs
           e2fsprogs
           `(,util-linux "lib")
           linux-pam
           dbus
           libxml2
           json-c
           acl
           boost
           ncurses/tinfo
           libxslt
           docbook-xsl
           gettext-minimal))
    (synopsis "Manage Btrfs file system snapshots and allow roll-backs")
    (description "This package provides Snapper, a tool that helps with
managing snapshots of Btrfs subvolumes and thin-provisioned LVM volumes.  It
can create and compare snapshots, revert differences between them, and
supports automatic snapshots timelines.")
    (license license:gpl2)))

(define-public xfstests
  ;; The last release (1.1.0) is from 2011.
  (let ((revision "3")
        (commit "8de535c53887bb49adae74a1b2e83e77d7e8457d"))
    (package
      (name "xfstests")
      (version (git-version "1.1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "git://git.kernel.org/pub/scm/fs/xfs/xfstests-dev.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1sbkryl04xflrk6jb4fsl3h2whilj5m3vpdkpwwb26idp7ckjjv6"))))
      (build-system gnu-build-system)
      (arguments
       (list
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'patch-tool-locations
              (lambda* (#:key inputs #:allow-other-keys)
                (substitute* "common/config"
                  ;; Make absolute file names relative.
                  (("(MKFS_PROG=\").*(\")" _ pre post)
                   (string-append pre "mkfs" post)))
                (for-each (lambda (file)
                            (substitute* file
                              (("( -s|#.|[= ])(/bin/sh|/bin/bash)" _ pre match)
                               (string-append pre
                                              (search-input-file inputs match)))
                              (("/bin/(rm|true)" match)
                               (search-input-file inputs match))
                              (("/usr(/bin/time)" _ match)
                               (search-input-file inputs match))))
                          (append (find-files "common" ".*")
                                  (find-files "tests" ".*")
                                  (find-files "tools" ".*")
                                  (find-files "src" "\\.(c|sh)$")))))
            (replace 'bootstrap
              (lambda* (#:key make-flags #:allow-other-keys)
                (substitute* "Makefile"
                  ;; Avoid a mysterious (to me) ‘permission denied’ error.
                  (("cp ") "cp -f "))
                (substitute* "m4/package_utilies.m4"
                  ;; Fix the bogus hard-coded paths for every single binary.
                  (("(AC_PATH_PROG\\(.*, ).*(\\))" _ pre post)
                   (string-append pre (getenv "PATH") post)))
                (apply invoke "make" "configure" make-flags)))
            (add-after 'install 'wrap-xfstests/check
              ;; Keep wrapping distinct from 'create-helper-script below: users
              ;; must be able to invoke xfstests/check directly if they prefer.
              (lambda* (#:key inputs #:allow-other-keys)
                (wrap-program (string-append #$output "/xfstests/check")
                  ;; Prefix the user's PATH with the minimum required tools.
                  ;; The suite has many other optional dependencies and will
                  ;; automatically select tests based on the original PATH.
                  `("PATH" ":" prefix
                    ,(map (lambda (file)
                            (dirname (search-input-file inputs file)))
                          (list "bin/setfacl"         ; acl
                                "bin/attr"            ; attr
                                "bin/bc"              ; bc
                                "bin/df"              ; coreutils
                                "bin/hostname"        ; inetutils
                                "bin/perl"            ; perl
                                "sbin/mkfs.xfs")))))) ; xfsprogs
            (add-after 'install 'create-helper
              ;; Upstream installs only a ‘check’ script that's not in $PATH and
              ;; would try to write to the store without explaining how to change
              ;; that.  Install a simple helper script to make it discoverable.
              (lambda* (#:key inputs #:allow-other-keys)
                (let* ((check  (string-append #$output "/xfstests/check"))
                       (bin    (string-append #$output "/bin"))
                       (helper (string-append bin "/xfstests-check")))
                  (mkdir-p bin)
                  (with-output-to-file helper
                    (lambda _
                      (format #t "#!~a --no-auto-compile\n!#\n"
                              (search-input-file inputs "/bin/guile"))
                      (write
                       `(begin
                          (define (try proc dir)
                            "Try to PROC DIR.  Return DIR on success, else #f."
                            (with-exception-handler (const #f)
                              (lambda _ (proc dir) dir)
                              #:unwind? #t))

                          (define args
                            (cdr (command-line)))

                          (when (or (member "--help" args)
                                    (member "-h" args))
                            (format #t "Usage: ~a [OPTION]...
This Guix helper sets up a new writable RESULT_BASE if it's unset, then executes
xfstest's \"~a\" command (with any OPTIONs) as documented below.\n\n"
                                    ,(basename helper)
                                    ,(basename check)))

                          (let* ((gotenv-base (getenv "RESULT_BASE"))
                                 (base (or gotenv-base
                                           (let loop ((count 0))
                                             (or (try mkdir
                                                      (format #f "xfstests.~a"
                                                              count))
                                                 (loop (+ 1 count))))))
                                 (result-base (if (string-prefix? "/" base)
                                                  base
                                                  (string-append (getcwd) "/"
                                                                 base))))
                            (setenv "RESULT_BASE" result-base)
                            ;; CHECK must run in its own directory or will fail.
                            (chdir ,(dirname check))
                            (let ((status
                                   (status:exit-val (apply system* ,check args))))
                              (unless gotenv-base
                                (try rmdir result-base))
                              status))))))
                  (chmod helper #o755)))))))
      (native-inputs
       (list autoconf automake libtool))
      (inputs
       (list acl
             attr
             bash-minimal
             bc
             guile-3.0                  ; for our xfstests-check helper script
             inetutils
             `(,util-linux "lib")
             perl
             time
             xfsprogs))
      (home-page "https://git.kernel.org/pub/scm/fs/xfs/xfstests-dev.git")
      (synopsis "File system @acronym{QA, Quality Assurance} test suite")
      (description
       "The @acronym{FSQA, File System Quality Assurance} regression test suite,
more commonly known as xfstests, comprises over 1,500 tests that exercise
(@dfn{torture}) both the user- and kernel-space parts of many different file
systems.

As the package's name subtly implies, it was originally developed to test the
XFS file system.  Today, xfstests is the primary test suite for all major file
systems supported by the kernel Linux including XFS, ext4, and Btrfs, but also
virtual and network file systems such as NFS, 9P, and the overlay file system.

The packaged @command{check} script is not in @env{PATH} but can be invoked
with the included @command{xfstests-check} helper.")
      (license license:gpl2))))

(define-public zfs
  (package
    (name "zfs")
    (version "2.3.1")
    (outputs '("out" "module" "src"))
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/openzfs/zfs/releases"
                           "/download/zfs-" version
                           "/zfs-" version ".tar.gz"))
       (sha256
        (base32 "01lh3yxzh271hzzlgn4b5rj3wb4ckbhx08kfcgf0p4l6jdwk6ch5"))))
    (build-system linux-module-build-system)
    (arguments
     (list
      ;; The ZFS kernel module should not be downloaded since the license
      ;; terms don't allow for distributing it, only building it locally.
      #:substitutable? #f
      ;; Tests cannot run in an unprivileged build environment.
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'configure 'really-configure
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "configure"
                (("-/bin/sh") (string-append "-" (which "sh"))))
              (invoke "./configure"
                      "--with-config=all"
                      (string-append "--prefix=" #$output)
                      (string-append "--with-dracutdir=" #$output
                                     "/lib/dracut")
                      (string-append "--with-udevdir=" #$output
                                     "/lib/udev")
                      (string-append "--with-mounthelperdir=" #$output
                                     "/sbin")
                      (string-append "--with-linux="
                                     (search-input-directory
                                      inputs
                                      "lib/modules/build")))))
          (add-after 'unpack 'patch-source
            (lambda* (#:key inputs #:allow-other-keys)
              ;; New feature "compatibility=" in 2.1.0.
              ;; This feature looks up in two locations:
              ;;   /etc/zfs/compatibility.d/
              ;;   /usr/share/zfs/compatibility.d/
              ;; The first is intended for system-specific compatibility
              ;; sets, while the second is what is installed with the
              ;; OpenZFS package, so use the absolute path for the first
              ;; (which requires patching in the file) and the store path
              ;; for the second (which it gets by default).
              (substitute* "include/sys/fs/zfs.h"
                (("#define\tZPOOL_SYSCONF_COMPAT_D.*$")
                 ;; Use absolute path.
                 "#define\tZPOOL_SYSCONF_COMPAT_D\t\"/etc/zfs/compatibility.d\"\n"))
              ;; Also update the manual, which uses absolute paths, so that
              ;; /usr/share/zfs/compatibility.d/ is referred via the store.
              (substitute* '("man/man7/zpoolprops.7"
                             "man/man7/zpool-features.7")
                (("/usr/share/zfs/compatibility.d")
                 (string-append #$output "/share/zfs/compatibility.d")))
              (substitute* "lib/libzfs/os/linux/libzfs_util_os.c"
                ;; Use path to /gnu/store/*-kmod in actual path that is
                ;; exec'ed.
                (("\"/sbin/modprobe\"")
                 (string-append "\""
                                (search-input-file inputs "/bin/modprobe")
                                "\""))
                ;; Just use 'modprobe' in message to user, since Guix
                ;; does not have a traditional /sbin/
                (("'/sbin/modprobe ") "'modprobe "))
              (substitute* "configure"
                (("/etc/default")
                 (string-append #$output "/etc/default"))
                (("/etc/bash_completion.d")
                 (string-append #$output "/etc/bash_completion.d")))
              (substitute* "Makefile.in"
                (("/usr/share/initramfs-tools")
                 (string-append #$output "/usr/share/initramfs-tools")))
              (substitute* "contrib/initramfs/Makefile.am"
                (("/usr/share/initramfs-tools")
                 (string-append #$output "/usr/share/initramfs-tools")))
              (substitute* "module/os/linux/zfs/zfs_ctldir.c"
                (("/usr/bin/env\", \"umount")
                 (string-append (search-input-file inputs "/bin/umount")
                                "\", \"-n"))
                (("/usr/bin/env\", \"mount")
                 (string-append (search-input-file inputs "/bin/mount")
                                "\", \"-n")))
              (substitute* "lib/libzfs/os/linux/libzfs_mount_os.c"
                (("/bin/mount") (search-input-file inputs "/bin/mount"))
                (("/bin/umount") (search-input-file inputs "/bin/umount")))
              (substitute* "lib/libshare/os/linux/nfs.c"
                (("/usr/sbin/exportfs")
                 (search-input-file inputs "/sbin/exportfs")))
              (substitute* "config/zfs-build.m4"
                (("\\$sysconfdir/init.d")
                 (string-append #$output "/etc/init.d")))
              (substitute* '("cmd/zed/Makefile.am")
                (("\\$\\(sysconfdir)") (string-append #$output "/etc")))
              (substitute* "udev/vdev_id"
                (("PATH=/bin:/sbin:/usr/bin:/usr/sbin")
                 (string-append "PATH="
                                (dirname (which "chmod")) ":"
                                (dirname (which "grep")) ":"
                                (dirname (which "sed")) ":"
                                (dirname (which "gawk")))))
              (substitute* '("Makefile.am" "Makefile.in")
                (("\\$\\(prefix)/src") (string-append #$output:src "/src")))
              (substitute* (find-files "udev/rules.d/" ".rules.in$")
                (("/sbin/modprobe")
                 (search-input-file inputs "/bin/modprobe")))))
          (replace 'build
            (lambda* (#:key make-flags parallel-build? #:allow-other-keys)
              (apply invoke "make"
                     `(,@(if parallel-build?
                             `("-j" ,(number->string (parallel-job-count)))
                             '())
                       ,@make-flags))))
          (replace 'install
            (lambda* (#:key inputs native-inputs
                      make-flags parallel-build? #:allow-other-keys)
              (apply invoke "make" "install"
                     (string-append "DEFAULT_INITCONF_DIR="
                                    #$output "/etc/default")
                     (string-append "DEPMOD="
                                    (search-input-file
                                     (or native-inputs inputs)
                                     "/bin/depmod"))
                     (string-append "INSTALL_PATH=" #$output)
                     (string-append "INSTALL_MOD_PATH=" #$output:module)
                     "INSTALL_MOD_STRIP=1"
                     `(,@(if parallel-build?
                             `("-j" ,(number->string (parallel-job-count)))
                             '())
                       ,@make-flags))
              (install-file
               "contrib/bash_completion.d/zfs"
               (string-append #$output
                              "/share/bash-completion/completions")))))))
    (native-inputs
     (list attr kmod pkg-config))
    (inputs (list eudev
                  kmod
                  libaio
                  libtirpc
                  nfs-utils
                  openssl
                  python
                  python-cffi
                  util-linux
                  `(,util-linux "lib")
                  zlib))
    (home-page "https://zfsonlinux.org/")
    (synopsis "OpenZFS on Linux")
    (description
     "OpenZFS is an advanced file system and volume manager which was
originally developed for Solaris and is now maintained by the OpenZFS
community.")
    (license license:cddl1.0)))

(define-public zfs-auto-snapshot
  (package
    (name "zfs-auto-snapshot")
    (version "1.2.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url
              (string-append "https://github.com/zfsonlinux/" name))
             (commit
              (string-append "upstream/" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0m4xw7h5qlbn5zdf9wb137pcr5l7hyrr7w2dgr16dfm5ay64vvfq"))))
    (build-system gnu-build-system)
    (inputs
     ;; Note: if you are inheriting from the above zfs package in order
     ;; to provide a specific stable kernel version, you should also
     ;; inherit this package and replace the sole input below.
     (list zfs))
    (arguments
     `(#:tests? #f ; No tests
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         ;; Guix System may not have a traditional cron system, but
         ;; the cron scripts installed by this package are convenient
         ;; to use as targets for an mcron job specification, so make
         ;; sure they can be run in-store.
         (add-before 'install 'fix-scripts
           (lambda* (#:key outputs inputs #:allow-other-keys)
             (let* ((out                (assoc-ref outputs "out"))
                    (zfs-auto-snapshot  (string-append
                                         out
                                         "/sbin/zfs-auto-snapshot"))
                    (zfs-package        (assoc-ref inputs "zfs"))
                    (zpool              (string-append
                                         zfs-package
                                         "/sbin/zpool"))
                    (zfs                (string-append
                                         zfs-package
                                         "/sbin/zfs")))
               (substitute* '("etc/zfs-auto-snapshot.cron.daily"
                              "etc/zfs-auto-snapshot.cron.frequent"
                              "etc/zfs-auto-snapshot.cron.hourly"
                              "etc/zfs-auto-snapshot.cron.monthly"
                              "etc/zfs-auto-snapshot.cron.weekly")
                 (("zfs-auto-snapshot")
                  zfs-auto-snapshot))
               (substitute* "src/zfs-auto-snapshot.sh"
                 (("LC_ALL=C zfs list")
                  (string-append "LC_ALL=C " zfs " list"))
                 (("LC_ALL=C zpool status")
                  (string-append "LC_ALL=C " zpool " status"))
                 (("zfs snapshot")
                  (string-append zfs " snapshot"))
                 (("zfs destroy")
                  (string-append zfs " destroy"))))))
         ;; Provide DESTDIR and PREFIX on make command.
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (invoke "make" "install"
                       "PREFIX="
                       (string-append "DESTDIR=" out)))
             #t)))))
    (home-page "https://github.com/zfsonlinux/zfs-auto-snapshot")
    (synopsis "Automatically create, rotate and destroy ZFS snapshots")
    (description "An alternative implementation of the zfs-auto-snapshot
service for Linux that is compatible with zfs-linux (now OpenZFS) and
zfs-fuse.

On Guix System, you will need to invoke the included shell scripts as
@code{job} definitions in your @code{operating-system} declaration.")
    (license license:gpl2+)))

(define-public mergerfs
  (package
    (name "mergerfs")
    (version "2.33.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/trapexit/mergerfs/"
                           "releases/download/" version "/"
                           "mergerfs-" version ".tar.gz"))
       (sha256
        (base32 "0m0fg191ax855yb20vqpvn6v2gc3i5xdizx09pgpymj1ybxc7yyw"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list (string-append "CC=" ,(cc-for-target))
             (string-append "CXX=" ,(cxx-for-target))
             (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:tests? #f                     ; all require a kernel with FUSE loaded
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)            ; no configure script
         (add-after 'unpack 'set-file-names
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (substitute* "libfuse/Makefile"
               (("/sbin") "$(EXEC_PREFIX)/sbin")
               (("chown") "true")  ; disallowed in the build environment
               (("strip") "true")) ; breaks cross-compilation
             ;; These were copied from the fuse-2 package.
             (substitute* '("libfuse/lib/mount_util.c"
                            "libfuse/util/mount_util.c")
               (("/bin/(u?)mount" _ maybe-u)
                (search-input-file inputs
                                   (string-append "bin/" maybe-u
                                                  "mount"))))
             (substitute* '("libfuse/util/mount.mergerfs.c")
               (("/bin/sh" command)
                (string-append (assoc-ref inputs "bash-minimal") command))
               ;; mount.mergerfs tries to execute `mergerfs`, which cannot be found
               ;; without an absolute path. Hard-coding the path is fine, since we don’t
               ;; link mount.mergerfs to mount.fuse anyway.
               (("add_arg\\(&command, type\\);")
                (string-append "add_arg(&command, \"" (assoc-ref outputs "out")
                               "/bin/mergerfs\");"))))))))
    ;; Mergerfs bundles a heavily modified copy of fuse.
    (inputs
     (list bash-minimal util-linux))
    (home-page "https://github.com/trapexit/mergerfs")
    (synopsis "Featureful union file system")
    (description "mergerfs is a union file system geared towards simplifying
storage and management of files across numerous commodity storage devices.  It
is similar to mhddfs, unionfs, and aufs.")
    (license (list
              license:isc                   ; mergerfs
              license:gpl2 license:lgpl2.0  ; Imported libfuse code.
              ))))

(define-public mergerfs-tools
  (let ((commit "3b6fe008517aeda715c306eaf4914f6f537da88d")
        (revision "3"))
    (package
      (name "mergerfs-tools")
      ;; No released version exists.
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/trapexit/mergerfs-tools")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "15pgym6c4viy57ccgp28dnqwh12f3gr02axg86y578aqa2yaa0ad"))))
      (build-system copy-build-system)
      (inputs
       (list python python-xattr rsync))
      (arguments
       '(#:install-plan
         '(("src/" "bin/"))
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'patch-paths
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* (find-files "src" "^mergerfs\\.")
                 (("'rsync'")
                  (string-append "'" (assoc-ref inputs "rsync") "/bin/rsync'"))
                 (("'rm'")
                  (string-append "'" (assoc-ref inputs "coreutils") "/bin/rm'")))
               (substitute* "src/mergerfs.mktrash"
                 (("xattr")
                  (search-input-file inputs "/bin/xattr"))
                 (("mkdir")
                  (search-input-file inputs "/bin/mkdir"))))))))
      (synopsis "Tools to help manage data in a mergerfs pool")
      (description "mergerfs-tools is a suite of programs that can audit
permissions and ownership of files and directories on a mergerfs volume,
duplicates files and directories across branches in its pool, find and remove
duplicate files, balance pool drives, consolidate files in a single mergerfs
directory onto a single drive and create FreeDesktop.org Trash specification
compatible directories.")
      (home-page "https://github.com/trapexit/mergerfs-tools")
      (license license:isc))))

(define-public python-dropbox
  (package
    (name "python-dropbox")
    (version "12.0.2")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "dropbox" version))
        (sha256
         (base32 "0qlrc2ykl7zmv808apqv5ycfzrwnm13ngz1daizh9kszmpapy1ah"))
        (snippet
         '(begin
            (use-modules (guix build utils))
            (substitute* "setup.py"
              (("pytest-runner==5\\.2\\.0") "pytest-runner"))))))
    (build-system python-build-system)
    (arguments '(#:tests? #f))  ; Tests not included in the release tarball.
    (native-inputs
     (list python-pytest python-pytest-runner))
    (propagated-inputs
     (list python-requests python-six python-stone))
    (home-page "https://www.dropbox.com/developers")
    (synopsis "Official Dropbox API Client")
    (description "This package provides a Python SDK for integrating with the
Dropbox API v2.")
    (license license:expat)))

(define-public dbxfs
  (package
    (name "dbxfs")
    (version "1.0.63")
    (source
      (origin
        ;; Release tarball contains files not in git repository.
        (method git-fetch)
        (uri (git-reference
               (url "https://thelig.ht/code/dbxfs")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1vzfhw3z2r0rb6s0qdzirh3pl7rv1z8xmxa0z5h7h1wqhpl05ai7"))
        (patches (search-patches "dbxfs-remove-sentry-sdk.patch"))
        (snippet
         #~(begin (use-modules (guix build utils))
                  ;; Don't check for package updates.
                  (substitute* "dbxfs/main.py"
                    (("if version") "if False"))))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f)) ; tests requires safefs
    (propagated-inputs
     (list python-appdirs
           python-block-tracing
           python-dropbox
           python-keyring
           python-keyrings.alt
           python-privy
           python-userspacefs))
  (home-page "https://thelig.ht/code/dbxfs/")
  (synopsis "User-space file system for Dropbox")
  (description
   "@code{dbxfs} allows you to mount your Dropbox folder as if it were a
local file system using FUSE.")
  (license license:gpl3+)))

(define-public rewritefs
  (let ((revision "1")
        (commit "3a56de8b5a2d44968b8bc3885c7d661d46367306"))
    (package
      (name "rewritefs")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/sloonz/rewritefs")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1w2rik0lhqm3wr68x51zs45gqfx79l7fi4p0sqznlfq7sz5s8xxn"))))
      (build-system gnu-build-system)
      (arguments
       (list
        #:modules
        '((guix build gnu-build-system)
          (guix build utils)
          (srfi srfi-26))
        #:make-flags
        #~(list (string-append "PREFIX=" #$output))
        #:test-target "test"
        #:tests? #f                    ; all require a kernel with FUSE loaded
        #:phases
        #~(modify-phases %standard-phases
            (delete 'configure)         ; no configure script
            (add-after 'install 'install-examples
              (lambda _
                (let ((doc (string-append #$output "/share/doc/"
                                          #$name "-" #$version)))
                  (for-each (cut install-file <> (string-append doc "/examples"))
                            (find-files "." "^config\\."))))))))
      (native-inputs
       (list pkg-config))
      (inputs
       (list fuse pcre))
      (home-page "https://github.com/sloonz/rewritefs")
      (synopsis "FUSE file system that changes particular file names")
      (description
       "RewriteFS is a @acronym{FUSE, File system in USEr space} to change the
name of accessed files on the fly based on any number of regular expressions.
It's like the @code{rewrite} action of many Web servers, but for your file
system.  For example, it can help keep your home directory tidy by transparently
rewriting the location of configuration files of software that doesn't follow
the XDG directory specification from @file{~/.@var{name}} to
@file{~/.config/@var{name}}.")
      (license license:gpl2+))))

(define-public squashfuse
  (package
    (name "squashfuse")
    (version "0.1.105")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/vasi/squashfuse")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "03aw8pw8694jyrzpnbry05rk9718sqw66kiyq878bbb679gl7224"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-cross-compile
            (lambda _
              (substitute* "autogen.sh"
                (("pkg-config")
                 #$(pkg-config-for-target))))))))
    (native-inputs (list autoconf automake libtool pkg-config))
    (inputs (list attr fuse-2 xz zlib `(,zstd "lib")))
    (home-page "https://github.com/vasi/squashfuse")
    (synopsis "Fuse filesystem to mount squashfs archives")
    (description
     "Squashfuse lets you mount SquashFS archives in user-space.  It supports
almost all features of the SquashFS format, yet is still fast and
memory-efficient.")
    (license license:bsd-2)))

(define-public squashfuse-for-appimage
  (package
    (inherit squashfuse)
    (name "squashfuse-for-appimage")
    (arguments
     (cons*
      #:configure-flags
      #~'("CFLAGS=-ffunction-sections -fdata-sections -Os -no-pie"
          "LDFLAGS=-static")
      (substitute-keyword-arguments (package-arguments squashfuse)
        ((#:phases phases)
         #~(modify-phases #$phases
             (add-after 'install 'install-private-headers
               (lambda _
                 (install-file "fuseprivate.h"
                               (string-append #$output
                                              "/include/squashfuse/")))))))))
    (inputs (list fuse-for-appimage
                  `(,zstd "lib")
                  `(,zstd "static")
                  `(,zlib "out")
                  `(,zlib "static")))))

(define-public tmsu
  (package
    (name "tmsu")
    (version "0.7.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/oniony/TMSU")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0834hah7p6ad81w60ifnxyh9zn09ddfgrll04kwjxwp7ypbv38wq"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/oniony/TMSU"
      #:unpack-path "github.com/oniony/TMSU"
      #:install-source? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'adjust-makefile
            (lambda* (#:key import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (substitute* "Makefile"
                  (("SHELL=.*") (string-append "SHELL=" (which "sh") "\n"))
                  ;; Make sure we do not compile 2 more times during the check
                  ;; phase.
                  (("unit-test: compile") "unit-test:")
                  (("integration-test: compile") "integration-test:")
                  ;; Simplify install path.
                  (("usr/") "")))))
          (replace 'build
            (lambda* (#:key import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (invoke "make" "compile"))))
          (replace 'check
            (lambda* (#:key import-path tests? #:allow-other-keys)
              (when tests?
                (with-directory-excursion (string-append "src/" import-path)
                  ;; Remove shaky test file, see
                  ;; <https://github.com/oniony/TMSU/issues/281>.
                  (for-each
                   (lambda (test-file)
                     (delete-file test-file))
                   (find-files "." "^fingerprinter_test.go$"))
                  (invoke "make" "test")))))
          (replace 'install
            (lambda* (#:key import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (setenv "DESTDIR" #$output)
                (invoke "make" "install")))))))
    (inputs
     (list go-github-com-mattn-go-sqlite3 go-github-com-hanwen-go-fuse))
    (home-page "https://github.com/oniony/TMSU")
    (synopsis "Tag files and access them through a virtual file system")
    (description
     "TMSU is a tool for tagging your files.  It provides a simple
command-line utility for applying tags and a virtual file system to give you a
tag-based view of your files from any other program.  TMSU does not alter your
files in any way: they remain unchanged on disk, or on the network, wherever
your put them.  TMSU maintains its own database and you simply gain an
additional view, which you can mount where you like, based upon the tags you
set up.")
    (license license:gpl3+)))

(define-public udftools
  (package
    (name "udftools")
    (version "2.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/pali/udftools")
                    (commit version)))
              (sha256
               (base32
                "1nl2s61znyzaap23zhbdg3znj6l6akr313fchn5wwvjzj8k70is9"))
              (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list (string-append "--docdir=" (assoc-ref %outputs "out")
                            "/share/doc/" ,name "-" ,version))))
    (native-inputs
     (list automake autoconf libtool pkg-config))
    (home-page "https://github.com/pali/udftools")
    (synopsis "Tools to manage UDF file systems and DVD/CD-R(W) drives")
    (description "@code{udftools} is a set of programs for reading
and modifying @acronym{UDF, Universal Disk Format} file systems.
@acronym{UDF, Universal Disk Format} is a file system mostly used for DVDs
and other optical media.  It supports read-only media (DVD/CD-R)
and rewritable media that wears out (DVD/CD-RW).")
    (license license:gpl2+)))

(define-public fuse-overlayfs
  (package
    (name "fuse-overlayfs")
    (version "1.13")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/containers/fuse-overlayfs")
                    (commit (string-append "v" version))))
              (sha256
               (base32
                "03gqb4czswqhx6zrv9jj88mf3mczk4m7azcjgr785c2lmga442ly"))
              (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (native-inputs
     (list automake autoconf libtool pkg-config))
    (inputs
     (list fuse))
    (home-page "https://github.com/containers/fuse-overlayfs")
    (synopsis "FUSE implementation of overlayfs")
    (description "This package provides an implementation of overlay+shiftfs
in FUSE for rootless containers.")
    (license license:gpl3)))

(define-public bees
  (package
    (name "bees")
    (version "0.10")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Zygo/bees")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (modules '((guix build utils)))
              (snippet
               ;; Unbundle cityhash.
               #~(begin
                   (for-each delete-file
                             '("lib/city.cc" "include/crucible/city.h"))
                   (substitute* "lib/Makefile"
                     (("city.o.*") ""))
                   (substitute* "src/bees-hash.cc"
                     (("#include .crucible/city.h.") "#include <city.h>"))))
              (sha256
               (base32
                "1j1v9bxijs8gvrb7rg0q1158xjvmfc8dlzwx768fxf3w8w2gfwvz"))))
    (build-system gnu-build-system)
    (arguments
     (list #:test-target "test"
           #:make-flags
           #~(list (string-append "CC=" #$(cc-for-target))
                   (string-append "DESTDIR=" #$output)
                   (string-append "BEES_VERSION=" #$version)
                   "PREFIX=''")
           #:phases
           #~(modify-phases %standard-phases
               (delete 'configure)
               (add-after 'unpack 'fixpath
                 (lambda* (#:key inputs #:allow-other-keys)
                   (substitute* "scripts/beesd.in"
                     (((string-append "\\<(" (string-join (list "realpath"
                                                                "uuidparse"
                                                                "grep"
                                                                "false"
                                                                "sed"
                                                                "true"
                                                                "head"
                                                                "mkdir"
                                                                "mount"
                                                                "touch"
                                                                "du"
                                                                "cut"
                                                                "rm"
                                                                "truncate"
                                                                "chmod")
                                                          "|") ")\\>") command)
                      (search-input-file inputs (string-append "/bin/" command)))

                     (("btrfs sub")
                      (string-append (search-input-file inputs "/bin/btrfs")
                                     " sub"))))))))
    (inputs (list btrfs-progs cityhash util-linux))
    (home-page "https://github.com/Zygo/bees")
    (synopsis "Deduplication agent for btrfs file systems")
    (description
     "@acronym{BEES, Best-Effort Extent-Same} is a block-oriented, user-space
deduplication agent designed for large btrfs file systems.  It combines off-line
data deduplication with incremental scanning to minimize the time your data
spend on disk between being written and being deduplicated.")
    (license (list license:gpl3+     ; the combined work
                   license:zlib      ; lib/crc64.cc
                   license:gpl2))))  ; include/crucible/btrfs.h

(define-public dwarfs
  (package
    (name "dwarfs")
    (version "0.6.2")
    (source (origin
              (method url-fetch)
              ;; The release archive is needed so that version.h is included.
              (uri (string-append "https://github.com/mhx/dwarfs/releases/download/v"
                                  version "/dwarfs-" version ".tar.xz"))
              (sha256
               (base32
                "1kncxf85gsj3anck8ccjmxn2azp5ifqbgkiky2kharmvphkbmfcv"))
              (snippet
               #~(begin
                   (use-modules (guix build utils))
                   ;; Prefer system libraries instead of submodules.
                   ;; TODO: Package fbthrift.
                   ;; TODO: Can we use Guix own folly?  There is no CMake option for it.
                   ;; TODO: Package parallel-hashmap.
                   (for-each delete-file-recursively
                             '(;; "fbthrift"
                               ;; "folly"
                               ;; "parallel-hashmap"
                               "xxHash"
                               "zstd"))))))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #f ; TODO: 1 test fails because 'modprobe fuse' needs privileged access.
       #:configure-flags
       (list "-DPREFER_SYSTEM_ZSTD=ON"
             "-DPREFER_SYSTEM_XXHASH=ON"
             "-DPREFER_SYSTEM_GTEST=ON"
             "-DWITH_TESTS=ON"
             ;; Disable man pages since ronn fails to run without hpricot.
             "-DWITH_MAN_PAGES=OFF")))
    (native-inputs
     (list
      ;; FIXME: Building with ronn fails because hpricot is missing from Guix.
      folly googletest libdwarf libevent pkg-config))
    (inputs
     (list
      boost
      double-conversion
      fmt
      fuse
      gflags
      jemalloc
      libarchive
      libunwind
      lz4
      openssl
      xxhash
      xz
      `(,zstd "lib")))
    (home-page "https://github.com/mhx/dwarfs")
    (synopsis "Fast high compression read-only file system")
    (description "DwarFS is a read-only file system with a focus on achieving
very high compression ratios in particular for very redundant data.

DwarFS also doesn't compromise on speed and for some cases it is on par with
or performs better than SquashFS.  For the primary use case, DwarFS
compression is an order of magnitude better than SquashFS compression, it's 6
times faster to build the file system, it's typically faster to access files
on DwarFS and it uses less CPU resources.

Distinct features of DwarFS are:

@itemize

@item Clustering of files by similarity using a similarity hash function.  This
makes it easier to exploit the redundancy across file boundaries.

@item Segmentation analysis across file system blocks in order to reduce the
size of the uncompressed file system.  This saves memory when using the
compressed file system and thus potentially allows for higher cache hit rates
as more data can be kept in the cache.

@item Highly multi-threaded implementation.  Both the file system creation tool
as well as the FUSE driver are able to make good use of the many cores of your
system.

@item Optional experimental Python scripting support to provide custom
filtering and ordering functionality.

@end itemize\n")
    (license license:gpl3)))

(define-public sirikali
  (package
    (name "sirikali")
    (version "1.5.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/mhogomchungu/sirikali")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1l52s8rxkfcxcx3s2fnsh08wy6hhjjvp7gcggdi84aqc4dq3rdnm"))))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #f ;No tests
       #:configure-flags '("-DQT5=true")))
    (inputs (list xdg-utils libpwquality libgcrypt libsecret qtbase-5))
    (native-inputs (list pkg-config))
    (home-page "https://mhogomchungu.github.io/sirikali/")
    (synopsis "Graphical program for managing encrypted file-systems")
    (description "@dfn{SiriKali} is a Qt / C++ @acronym{GUI, graphical user
interface} application that manages ecryptfs, cryfs, encfs, gocryptfs, fscrypt
and securefs based encrypted folders.")
    (license license:gpl2+)))
