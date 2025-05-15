;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015, 2016, 2017 Manolis Fragkiskos Ragkousis <manolis837@gmail.com>
;;; Copyright © 2018, 2020-2024 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2020 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2020, 2022, 2023, 2024 Janneke Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2020 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2020 Rene Saavedra <pacoon@protonmail.com>
;;; Copyright © 2023 Josselin Poiret <dev@jpoiret.xyz>
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

(define-module (gnu packages hurd)
  #:use-module ((guix licenses) #:hide (zlib))
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cross-base)
  #:use-module (gnu packages disk)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages libdaemon)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages onc-rpc)
  #:use-module (gnu packages xorg) ;libpciaccess
  #:use-module (guix git-download)
  #:use-module (ice-9 match))

(define (hurd-source-url version)
  (string-append "mirror://gnu/hurd/hurd-"
                 version ".tar.gz"))

(define-public gnumach-headers
  (let ((commit "v1.8+git20240714"))
    (package
      (name "gnumach-headers")
      (version (string-drop commit 1))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://git.savannah.gnu.org/git/hurd/gnumach.git")
               (commit commit)))
         (patches (search-patches "gnumach-version.patch"))
         (file-name (git-file-name "gnumach" version))
         (sha256
          (base32 "0ykav1kx0bgxcxw04bpcsh5s4531fzdkahjgrlsfs2h3w3vfkga0"))))
      (build-system gnu-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (replace 'install
             (lambda _
               (invoke "make" "install-data" "install-data-hook")))
           (delete 'build))
         #:tests? #f))
      (native-inputs
       (list autoconf automake texinfo-4))
      (supported-systems %hurd-systems)
      (home-page "https://www.gnu.org/software/hurd/microkernel/mach/gnumach.html")
      (synopsis "GNU Mach kernel headers")
      (description
       "Headers of the GNU Mach kernel.")
      (license gpl2+))))

(define-public mig
  (package
    (name "mig")
    (version "1.8+git20231217")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://git.savannah.gnu.org/git/hurd/mig.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1mx7w5vzw5ws0zplm1y6s679jb1g2hjkiwl3dlk5lxys0dxs5g4g"))))
    (build-system gnu-build-system)
    ;; Flex is needed both at build and run time.
    (inputs (list gnumach-headers flex))
    (native-inputs (list autoconf automake flex bison))
    (arguments
     (list #:tests? #f
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'install 'avoid-perl-dependency
                 (lambda* (#:key build inputs outputs #:allow-other-keys)
                   (let* ((out (assoc-ref outputs "out"))
                          (bin (string-append out "/bin")))
                     ;; By default 'mig' (or 'TARGET-mig') uses Perl to
                     ;; compute 'libexecdir_rel'.  Avoid it.
                     (substitute* (find-files bin "mig$")
                       (("^libexecdir_rel=.*")
                        "libexecdir_rel=../libexec\n"))))))))
    (home-page "https://www.gnu.org/software/hurd/microkernel/mach/mig/gnu_mig.html")
    (synopsis "Mach 3.0 interface generator for the Hurd")
    (description
     "GNU MIG is the GNU distribution of the Mach 3.0 interface generator
MIG, as maintained by the GNU Hurd developers for the GNU project.
You need this tool to compile the GNU Mach and GNU Hurd distributions,
and to compile the GNU C library for the Hurd.  Also, you will need it
for other software in the GNU system that uses Mach-based inter-process
communication.")
    (license gpl2+)))

(define-public hurd-headers
  (let ((revision "3")
        (commit "v0.9.git20240714"))
    (package
      (name "hurd-headers")
      (version (string-drop commit 1))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://git.savannah.gnu.org/git/hurd/hurd.git")
                      (commit commit)))
                (sha256
                 (base32
                  "0wvzil3llmrjg7ymwqs86d11bm5fl771jwncv7kk679lsvqca0ll"))
                (file-name (git-file-name name version))))
      (build-system gnu-build-system)
      (native-inputs
       (list autoconf
             automake
             (if (%current-target-system)
                 (cross-mig (%current-target-system))
                 mig)))
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (replace 'install
             (lambda _
               (invoke "make" "install-headers" "no_deps=t")))
           (delete 'build))

         #:configure-flags '( ;; Reduce set of dependencies.
                             "--without-parted"
                             "--disable-ncursesw"
                             "--disable-test"
                             "--without-libbz2"
                             "--without-libcrypt"
                             "--without-libz"
                             "--without-rump"
                             ;; Skip the clnt_create check because it expects
                             ;; a working glibc causing a circular dependency.
                             "ac_cv_search_clnt_create=no"

                             ;; Annihilate the checks for the 'file_exec_paths'
                             ;; & co. libc functions to avoid "link tests are
                             ;; not allowed after AC_NO_EXECUTABLES" error.
                             "ac_cv_func_file_exec_paths=no"
                             "ac_cv_func_exec_exec_paths=no"
                             "ac_cv_func__hurd_exec_paths=no"
                             "ac_cv_func__hurd_libc_proc_init=no"
                             "ac_cv_func_file_futimens=no"
                             "ac_cv_func_file_utimens=no"
                             "ac_cv_lib_acpica_acpi_init=no")

         #:tests? #f))
      (supported-systems %hurd-systems)
      (home-page "https://www.gnu.org/software/hurd/hurd.html")
      (synopsis "GNU Hurd headers")
      (description
       "This package provides C headers of the GNU Hurd, used to build the GNU C
Library and other user programs.")
      (license gpl2+))))

(define-public hurd-minimal
  (package (inherit hurd-headers)
    (name "hurd-minimal")
    (inputs (list glibc/hurd-headers gnumach-headers))
    (arguments
     (substitute-keyword-arguments (package-arguments hurd-headers)
       ((#:make-flags flags '())
        #~'(#$(string-append "lib-subdirs=libshouldbeinlibc libihash libstore")
            "prog-subdirs="
            "other-subdirs="
            #$@flags))
       ((#:phases _)
        #~%standard-phases)
       ((#:validate-runpath? validate-runpath? #f)
        #f)))
    (supported-systems %hurd-systems)
    (home-page "https://www.gnu.org/software/hurd/hurd.html")
    (synopsis "GNU Hurd libraries")
    (description
     "This package provides libshouldbeinlibc, libihash, libstore, libports,
libiohelp, libfshelp, libtrivfs, and libmachdev, needed to build the GNU C
Library, Parted and netdde for GNU/Hurd.")
    (license gpl2+)))

(define-public hurd-core-headers
  (package
    (name "hurd-core-headers")
    (version (package-version hurd-headers))
    (source #f)
    (build-system trivial-build-system)
    (arguments
     '(#:modules ((guix build union))
       #:builder (begin
                   (use-modules (srfi srfi-1)
                                (srfi srfi-26)
                                (ice-9 match)
                                (guix build union))
                   (let ((inputs (filter
                                  (compose (cute member <> '("gnumach-headers"
                                                             "hurd-headers"
                                                             "hurd-minimal"))
                                           car)
                                  %build-inputs)))
                     (match inputs
                       (((names . directories) ...)
                        (union-build (assoc-ref %outputs "out")
                                     directories)
                        #t))))))
    (inputs (list gnumach-headers hurd-headers hurd-minimal))
    (supported-systems %hurd-systems)
    (synopsis "Union of the Hurd headers and libraries")
    (description
     "This package contains the union of the Mach and Hurd headers and the
Hurd-minimal package which are needed for both glibc and GCC.")
    (home-page (package-home-page hurd-headers))
    (license (package-license hurd-headers))))

(define-public gnumach
  (package
    (inherit gnumach-headers)
    (name "gnumach")
    (arguments
     (substitute-keyword-arguments (package-arguments gnumach-headers)
       ((#:configure-flags flags ''())
        `(cons* "--enable-kdb"          ;enable kernel debugger
                "--disable-net-group"
                "--disable-pcmcia-group"
                "--disable-wireless-group"
                ,flags))
       ((#:phases phases '%standard-phases)
        #~(modify-phases %standard-phases
            (add-after 'install 'produce-image
              (lambda _
                (let ((boot (string-append #$output "/boot")))
                  (invoke "make" "gnumach.gz")
                  (install-file "gnumach.gz" boot))))))))
    (native-inputs
     (list autoconf
           automake
           (if (%current-target-system)
               (cross-mig (%current-target-system))
               mig)
           perl
           texinfo-4))
    (supported-systems `("i686-linux" ,@%hurd-systems))
    (synopsis "Microkernel of the GNU system")
    (description
     "GNU Mach is the microkernel upon which a GNU Hurd system is based.")))

(define unifont
  ;; GNU Unifont, <http://gnu.org/s/unifont>.
  ;; Used the the VGA driver of the Hurd's console client.
  (origin
    (method url-fetch)
    (uri
     "http://unifoundry.com/pub/unifont-7.0.06/font-builds/unifont-7.0.06.bdf.gz")
    (sha256
     (base32
      "0p2vhnc18cnbmb39vq4m7hzv4mhnm2l0a2s7gx3ar277fwng3hys"))))

(define dde-sources
  ;; This is the current tip of the dde branch
  (let ((commit "b6c8526c703f3ba76294d9002f195c63897ec661"))
    (origin
      (method git-fetch)
      (uri (git-reference
            (url "https://git.savannah.gnu.org/git/hurd/incubator.git")
            (commit commit)))
      (sha256
       (base32
        "0k1ilj8ghli8x43xaksbc4y419pqh0w16k374914c07svq419bbr"))
      (file-name (git-file-name "dde" (string-take commit 7))))))

(define %import-from-dde
  (list "libmachdevdde" "libddekit" "libdde_linux26"))

(define %add-to-hurd-subdirs
  (list "libmachdevdde" "libddekit"))

(define-public hurd
  (package
    (name "hurd")
    (source (origin
              (inherit (package-source hurd-headers))
              (patches (search-patches "hurd-refcounts-assert.patch"
                                       "hurd-rumpdisk-no-hd.patch"
                                       "hurd-startup.patch"
                                       "hurd-socket-activation.patch"
                                       "hurd-64bit.patch"))))
    (version (package-version hurd-headers))
    (arguments
     `(#:tests? #f                      ;no "check" target
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'prepare-addons
           (lambda* (#:key native-inputs inputs #:allow-other-keys)
             ;; First we import the things we want from dde.
             (for-each make-file-writable (find-files "."))
             (let ((dde (assoc-ref (or native-inputs inputs) "dde-sources")))
               (for-each (lambda (dir)
                           (copy-recursively
                            (string-append dde "/" dir ) dir))
                         '("libmachdevdde" "libddekit" "libdde_linux26")))
             ;; And we add some as subdirs so that they're built by the main
             ;; Makefile. libdde_linux26 is built later in its own phase.
             (substitute* "Makefile"
               (("libbpf ")
                "libbpf libmachdevdde libddekit "))))
         (add-after 'unpack 'find-tirpc
           (lambda* (#:key inputs #:allow-other-keys)
             (for-each (lambda (var)
                         (setenv var
                                 (string-append
                                  (search-input-directory inputs
                                                          "include/tirpc")
                                  ":" (or (getenv var) ""))))
                       '("CROSS_C_INCLUDE_PATH" "C_INCLUDE_PATH"
                         "CROSS_CPATH" "CPATH"))
             #t))
         (add-after 'unpack 'fix-rpc-headers
           (lambda _
             (substitute* "nfs/mount.c"
               (("#undef (TRUE|FALSE)") "")
               (("#include <rpc/pmap_prot.h>" m)
                (string-append  "#include <rpc/xdr.h>\n" m)))
             (substitute* '("nfsd/cache.c")
               (("#undef (TRUE|FALSE)") ""))
             (substitute* '("nfsd/loop.c"
                            "nfsd/main.c"
                            "nfsd/ops.c")
               (("#include <rpc/pmap_prot.h>" m)
                (string-append "#include <rpc/types.h>\n#include <rpc/xdr.h>\n" m)))
             #t))
         ,@(if (%current-target-system)
               '((add-after 'configure 'fixup-cross-configure
                   (lambda _
                     (substitute* "config.make"
                       (("HAVE_LIBRUMP = no") "HAVE_LIBRUMP = yes")))))
               '())
         (add-before 'build 'pre-build
           (lambda _
             ;; Don't change the ownership of any file at this time.
             (substitute* '("daemons/Makefile" "utils/Makefile")
               (("-o root -m 4755") ""))
             #t))
         (add-after 'unpack 'create-runsystem
           (lambda _
             ;; XXX Work towards having startup.c invoke the Guile rc
             (delete-file "daemons/runsystem.sh")
             (with-output-to-file "daemons/runsystem.sh"
               (lambda _
                 (display "#! /bin/bash

# XXX Guile needs pipe support for its finalizer thread, to start.
# Remove this script when Linux and the Hurd have xattr patches.
PATH=@PATH@

fsck --yes --force /
fsysopts / --writable

mkdir -p /servers/socket
rm -f /servers/socket/1
# Note: this /hurd/ gets substituted
settrans --create /servers/socket/1 /hurd/pflocal

# Upon second boot, (file-exists? /dev/null) in hurd-boot-system hangs unless:
rm -f /dev/urandom

# parse multiboot arguments
for i in \"$@\"; do
    case $i in
        (gnu.system=*)
            system=${i#gnu.system=}
            ;;
    esac
done

echo Starting ${system}/rc...
exec ${system}/rc \"$@\"
")))))
         (add-before 'build 'set-file-names
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out  (assoc-ref outputs "out"))
                    (bash (assoc-ref inputs "bash-minimal"))
                    (coreutils (assoc-ref inputs "coreutils"))
                    (sed  (assoc-ref inputs "sed"))
                    (grep (assoc-ref inputs "grep"))
                    (util-linux (assoc-ref inputs "util-linux")))
               (substitute* '("daemons/runttys.c" "daemons/getty.c" "utils/login.c")
                 (("/bin/login")
                  (string-append out "/bin/login"))
                 (("/bin/bash") (string-append bash "/bin/bash")))
               (substitute* '("startup/startup.c" "config/ttys")
                 (("/libexec/")
                  (string-append out "/libexec/")))
               (substitute* '("utils/uptime.sh")
                 (("/bin/w")
                  (string-append out "/bin/w")))
               ;; Upon first boot the /hurd symlink does not exist; it is
               ;; created during activation: Hard-code the .../hurd store file
               ;; name.
               (substitute* '("boot/boot.c"
                              "daemons/console-run.c"
                              "startup/startup.c")
                 (("/hurd/")
                  (string-append out "/hurd/")))
               (substitute* '("libdiskfs/boot-start.c"
                              "libdiskfs/opts-std-startup.c")
                 (("_HURD_STARTUP")
                  (string-append "\"" out "/hurd/startup\"")))
               (substitute* '("daemons/runsystem.sh"
                              "utils/fakeroot.sh"
                              "utils/remap.sh"
                              "sutils/MAKEDEV.sh"
                              "sutils/losetup.sh")
                 (("^PATH=.*")
                  (string-append "PATH=" out "/bin"
                                 ":" out "/sbin"
                                 ":" coreutils "/bin"
                                 ":" grep "/bin"
                                 ":" sed "/bin"
                                 ":" util-linux "/sbin\n"))
                 (("/sbin/") (string-append out "/sbin/"))
                 (("/libexec/") (string-append out "/libexec/"))
                 (("/hurd/") (string-append out "/hurd/")))
               #t)))
         (add-after 'patch-shebangs 'patch-libexec-shebangs
           (lambda* (#:key inputs outputs #:allow-other-keys)
             ;; XXX: Since the 'patch-shebangs' phase doesn't traverse
             ;; /libexec, do it here.
             (let* ((out  (assoc-ref outputs "out"))
                    (bash (assoc-ref inputs "bash-minimal"))
                    (path (list (string-append bash "/bin"))))
               (for-each (lambda (file)
                           (patch-shebang file path))
                         (find-files (string-append out "/libexec")))
               #t)))
         (add-after 'build 'build-libdde-linux
          (lambda* (#:key inputs native-inputs #:allow-other-keys)
            (let ((arch ,(match (or (%current-target-system)
                                    (%current-system))
                           ((? target-x86-32?)
                            "x86")
                           ((? target-x86-64?)
                            "amd64")
                           (_
                            ;; XXX: Cross-compiling this package to an
                            ;; unsupported system.
                            "UNSUPPORTED_SYSTEM"))))
              (when ,(target-hurd64?)
                (let ((dir "libdde_linux26/build/include"))
                  (mkdir-p (string-append dir "/x86"))
                  (format #t "symlink ~a -> ~a\n"
                          (string-append dir "/x86/amd64") "x86")
                  (symlink "x86" (string-append dir "/amd64"))
                  (format #t "symlink ~a -> ~a\n"
                          (string-append dir "/amd64/asm-x86_64") "asm-x86")
                  (symlink "asm-x86" (string-append dir "/amd64/asm-x86_64"))))
              (invoke (string-append (assoc-ref (or native-inputs inputs) "make")
                                     "/bin/make")
                      ;; XXX There can be a race condition because subdirs
                      ;; aren't interdependent targets in the Makefile.
                      "-j1" "-C" "libdde_linux26"
                      (string-append "SHELL="
                                     (assoc-ref (or native-inputs inputs) "bash")
                                     "/bin/bash")
                      (string-append "CC="
                                     ,(cc-for-target))
                      (string-append "WARNINGS="
                                     " -Wno-declaration-missing-parameter-type"
                                     " -Wno-implicit-function-declaration"
                                     " -Wno-implicit-int"
                                     " -Wno-int-conversion"
                                     " -Wno-strict-prototypes")
                      (string-append "ARCH=" arch)))))
         (add-after 'install 'install-goodies
          (lambda* (#:key inputs native-inputs outputs #:allow-other-keys)
            ;; Install additional goodies.
            ;; TODO: Build & install *.msgids for rpctrace.
            (let* ((out (assoc-ref outputs "out"))
                   (datadir (string-append out "/share/hurd"))
                   (arch ,(match (or (%current-target-system)
                                     (%current-system))
                            ((? target-x86-32?)
                             "x86")
                            ((? target-x86-64?)
                             "amd64")
                            (_
                             ;; XXX: Cross-compiling this package to an
                             ;; unsupported system.
                             "UNSUPPORTED_SYSTEM")))
                   (dir (string-append out "/share/libdde_linux26/build/include")))
              (mkdir-p dir)
              (when ,(target-hurd64?)
                (mkdir-p (string-append dir "/amd64"))
                (format #t "symlink ~a -> ~a\n"
                        (string-append dir "/amd64/asm-x86_64")
                        "x86")
                (symlink "x86" (string-append dir "/amd46")))
              (invoke (string-append (assoc-ref (or native-inputs inputs) "make")
                                     "/bin/make")
                      "-C" "libdde_linux26" "install"
                      (string-append "SHELL="
                                     (assoc-ref (or native-inputs inputs) "bash")
                                     "/bin/bash")
                      (string-append "INSTALLDIR=" dir)
                      (string-append "ARCH=" arch))
              (when ,(target-hurd64?)
                (format #t "symlink ~a -> ~a\n"
                        (string-append dir "/amd64/asm-x86_64")
                        "asm-x86")
                (symlink "asm-x86" (string-append dir "/amd64/asm-x86_64")))
              ;; Install the fancy UTF-8 motd.
              (mkdir-p (string-append out "/etc"))
              (copy-file "console/motd.UTF8"
                         (string-append out "/etc/motd"))

              ;; Install the BDF font for use by the console client.
              (copy-file (assoc-ref inputs "unifont")
                         "unifont.gz")
              (invoke "gunzip" "unifont.gz")
              (mkdir-p datadir)
              (copy-file "unifont"
                         (string-append datadir "/vga-system.bdf"))))))
       #:configure-flags
       ,#~(list (string-append "LDFLAGS=-Wl,-rpath="
                               #$output "/lib")
                "--enable-static-progs=ext2fs,iso9660fs,rumpdisk,pci-arbiter,acpi"
                "--disable-ncursesw"
                "--without-libbz2"
                "--without-libz"
                ;; This is needed to pass the configure check for
                ;; clnt_create
                "ac_func_search_save_LIBS=-ltirpc"
                "ac_cv_search_clnt_create=false"
                "CFLAGS=-fcommon")))
    (build-system gnu-build-system)
    (inputs
     `(("libgcrypt" ,libgcrypt)                  ;for /hurd/random
       ("libdaemon" ,libdaemon)                  ;for /bin/console --daemonize
       ("unifont" ,unifont)
       ("libpciaccess" ,libpciaccess-static)

       ;; For NFS support
       ("libtirpc" ,libtirpc/hurd)

       ;; Tools for the /libexec/* scripts.
       ("bash-minimal" ,bash-minimal)
       ("coreutils" ,coreutils)
       ("sed" ,sed)
       ("grep" ,grep)
       ("util-linux" ,util-linux "static")       ;libuuid.a, for parted
       ("parted" ,parted)                        ;for rumpdisk
       ("rumpkernel" ,rumpkernel)))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libgcrypt" ,libgcrypt)                   ;for 'libgcrypt-config'
       ("mig" , (if (%current-target-system)
                    (cross-mig (%current-target-system))
                    mig))
       ("pkg-config" ,pkg-config)
       ("perl" ,perl)
       ("texinfo" ,texinfo-4)
       ("dde-sources" ,dde-sources)))
    (supported-systems %hurd-systems)
    (home-page "https://www.gnu.org/software/hurd/hurd.html")
    (synopsis "The kernel servers for the GNU operating system")
    (description
     "The Hurd is the kernel for the GNU system, a replacement and
augmentation of standard Unix kernels.  It is a collection of protocols for
system interaction (file systems, networks, authentication), and servers
implementing them.")
    (license gpl2+)))

(define-public netdde
  (let ((commit "c0ef248dc7c5ccc1273e2a796f3ece30c5b645df")
        (revision "3"))
    (package
      (name "netdde")
      ;; The version prefix corresponds to the version of Linux from which the
      ;; drivers were taken.
      (version (git-version "2.6.32.65" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://git.savannah.gnu.org/git/hurd/incubator.git")
                      (commit commit)))
                (patches (search-patches "netdde-build-fix.patch"
                                         "netdde-csum.patch"))
                (sha256
                 (base32
                  "070fpmd4nvn3mp8dj9w4if63iwz7j2m0h6ywq888znw70wlrc6sh"))
                (file-name (git-file-name name commit))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f                    ;no "check" target
         #:make-flags
         (list (string-append "SHELL="
                              (search-input-file %build-inputs "/bin/bash"))
               "PKGDIR=libdde_linux26"
               (string-append "CC=" ,(cc-for-target))
               (string-append "WARNINGS="
                              " -Wno-declaration-missing-parameter-type"
                              " -Wno-implicit-function-declaration"
                              " -Wno-implicit-int"
                              " -Wno-int-conversion"
                              " -Wno-strict-prototypes")
               (let ((arch ,(match (or (%current-target-system)
                                       (%current-system))
                              ((? target-x86-32?)
                               "x86")
                              ((? target-x86-64?)
                               "amd64")
                              (_
                               ;; XXX: Cross-compiling this package to an
                               ;; unsupported system.
                               "UNSUPPORTED_SYSTEM"))))
                 (string-append "ARCH=" arch)))
         #:configure-flags
         ,#~(list (string-append "LDFLAGS=-Wl,-rpath=" #$output "/lib"))
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (add-after 'unpack 'prepare-dde
             (lambda* (#:key native-inputs inputs #:allow-other-keys)
               (for-each make-file-writable (find-files "."))
               (let ((dde (assoc-ref (or native-inputs inputs) "dde-sources")))
                 (for-each (lambda (dir)
                             (copy-recursively
                              (string-append dde "/" dir ) dir))
                           '("libdde_linux26" "libddekit")))
               (substitute* "libdde_linux26/mk/rel2abs.sh"
                 (("/bin/bash") (which "bash")))))
           (add-after 'patch-generated-file-shebangs 'build-libdde-linux26
             (lambda* (#:key make-flags #:allow-other-keys)
               (when ,(target-hurd64?)
                 (let ((dir "libdde_linux26/build/include"))
                   (mkdir-p (string-append dir "/x86"))
                   (format #t "symlink ~a -> ~a\n"
                           (string-append dir "/x86/amd64") "x86")
                   (symlink "x86" (string-append dir "/amd64"))
                   (format #t "symlink ~a -> ~a\n"
                           (string-append dir "/amd64/asm-x86_64") "asm-x86")
                   (symlink "asm-x86" (string-append dir "/amd64/asm-x86_64"))))
               (with-directory-excursion "libdde_linux26"
                 (apply invoke "make"
                        (delete "PKGDIR=libdde_linux26" make-flags)))))
           (add-after 'build-libdde-linux26 'convert
             (lambda* (#:key make-flags #:allow-other-keys)
               (apply invoke "make" "convert" make-flags)))
           (replace 'build
             (lambda* (#:key make-flags #:allow-other-keys)
               (apply invoke "make"
                      ,(string-append "LINK_PROGRAM=" (cc-for-target))
                      make-flags)
               ;; This hack to build netdde.static was found in
               ;; https://salsa.debian.org/hurd-team/netdde/-/blob/b539b2ad7a171371f140c3da58cce33f1a91ac12/debian/rules
               (delete-file "Makefile.inc")
               (apply invoke "make"
                      ,(string-append "LINK_PROGRAM=" (cc-for-target) " -static")
                      "TARGET=netdde.static"
                      make-flags)))
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((hurd (string-append (assoc-ref outputs "out") "/hurd")))
                 (install-file "netdde" hurd)
                 (install-file "netdde.static" hurd)))))))
      (inputs
       (list hurd libpciaccess-static zlib `(,zlib "static")))
      (native-inputs
       `(("coreutils" ,coreutils)
         ("gawk" ,gawk)
         ("grep" ,grep)
         ("perl" ,perl)
         ("sed" ,sed)
         ("dde-sources" ,dde-sources)))
      (supported-systems %hurd-systems)
      (home-page "https://www.gnu.org/software/hurd/hurd.html")
      (synopsis "Linux network drivers glued by the DDE layer")
      (description
       "This package provides Linux 2.6 network drivers that can be embedded
in userland processes thanks to the DDE layer.")
      ;; Some drivers are dually licensed with the options being GPLv2 or one
      ;; of MPL/Expat/BSD-3 (dependent on the driver).
      (license gpl2))))

(define-public rumpkernel
  (let ((commit "f1ffd6405f225336e595a0f99f01095ed7438337")
        (revision "0"))
    (package
      (name "rumpkernel")
      (version (git-version "0-20211031" revision commit))
      ;; This uses the Debian Salsa rumpkernel package git as upstream as that
      ;; is where development happens.  Once things have stabilized, upstream
      ;; may change to the NetBSD git from where Debian takes their snapshots.
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://salsa.debian.org/hurd-team/rumpkernel.git")
                      (commit commit)))
                (sha256
                 (base32
                  "1ygn3ysji06ik3k44sf906fjpdmabznkspw70llldbk2zkrcdw7i"))
                (file-name (git-file-name name version))))
      (build-system gnu-build-system)
      (arguments
       (list
        #:tests? #f
        #:modules '((srfi srfi-26)
                    (ice-9 rdelim)
                    (guix build utils)
                    (guix build gnu-build-system))
        ;; As we are using the Debian package as upstream, we follow their
        ;; build:
        ;;   * apply patches in debian/patches taken from the
        ;;     debian/patches/series file
        ;;   * for the configure, make, and install stages, follow
        ;;     the code in debian/rules
        ;; The Debian patchset includes a cross build feature that we
        ;; use with two differences
        ;;   * Debian uses a multiarch toolchain
        ;;   * we use cross-mig
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'apply-patches
              (lambda* (#:key target #:allow-other-keys)
                (let* ((patch-directory "debian/patches/")
                       (series (string-append patch-directory "series"))
                       (text (with-input-from-file series read-string))
                       (lines (string-split (string-trim-right text) #\newline))
                       (patches (filter (negate (cute string-prefix? "#" <>))
                                        lines))
                       (patch-files (map
                                     (cute string-append patch-directory <>)
                                     patches)))
                  (for-each
                   (cute invoke "patch" "--force" "-p1" "-i" <>)
                   patch-files)
                  ;; Somewhere in the build.sh/make process MIG is not being
                  ;; exported, apparently.
                  (let* ((prefix (if (not target) "" (string-append target "-")))
                         (mig (string-append prefix "mig")))
                    (substitute* "pci-userspace/src-gnu/Makefile.inc"
                      (("MIG=mig")
                       (string-append "MIG=" mig)))))))
            (add-before 'configure 'setenv
              (lambda* (#:key build target #:allow-other-keys)
                (define (noisy-setenv name value)
                  (setenv name value)
                  (format (current-error-port) "set ~a=~s\n" name value))
                (noisy-setenv "HOST_CC" "gcc")
                (let* ((prefix (if (not target) "" (string-append target "-"))))
                  (noisy-setenv "TARGET_AR" (string-append prefix "ar"))
                  (noisy-setenv "TARGET_CC" (string-append prefix "gcc"))
                  (noisy-setenv "TARGET_CXX" (string-append prefix "g++"))
                  (noisy-setenv "TARGET_LD" (string-append prefix "ld"))
                  (noisy-setenv "TARGET_MIG" (string-append prefix "mig"))
                  (noisy-setenv "TARGET_NM" (string-append prefix "nm"))
                  (noisy-setenv "MIG" (string-append prefix "mig")))
                (setenv "PAWD" "pwd")
                (for-each
                 (cute noisy-setenv <> "")
                 '("_GCC_CRTENDS"
                   "_GCC_CRTEND"
                   "_GCC_CRTBEGINS"
                   "_GCC_CRTBEGIN"
                   "_GCC_CRTI"
                   "_GCC_CRTN"))))
            (replace 'configure
              (lambda args
                (let ((configure (assoc-ref %standard-phases 'configure)))
                  (with-directory-excursion "buildrump.sh/src/lib/librumpuser"
                    (apply configure args)))))
            ;; The build has three toplevel entry points
            ;;   * buildrump.sh/src/build.sh: create a NetBSD-compatible
            ;;     toolchain and supports cross-compiling
            ;;   * buildrump.sh/src/lib/librumpuser: the librump* libraries
            ;;   * pci-userspace/src-gnu: the librumpdev_pci* libraries
            (replace 'build
              (lambda* (#:key parallel-build? #:allow-other-keys)
                (let* ((jobs (if parallel-build? (parallel-job-count) 1))
                       (host-cpu #$(match (or (%current-target-system)
                                              (%current-system))
                                     ((? target-x86-32?)
                                      "i386")
                                     ((? target-x86-64?)
                                      "amd64")
                                     (_ "unknown")))
                       (toprump (string-append
                                 (getcwd)
                                 "/buildrump.sh/src/sys/rump"))
                       (rump-make (string-append
                                   (getcwd)
                                   "/buildrump.sh/src/obj/tooldir/bin/nbmake-"
                                   host-cpu)))
                  (mkdir "obj")
                  (with-directory-excursion "buildrump.sh/src"
                    (invoke
                     "sh" "build.sh"
                     "-V" "TOOLS_BUILDRUMP=yes"
                     "-V" "MKBINUTILS=no"
                     "-V" "MKGDB=no"
                     "-V" "MKGROFF=no"
                     "-V" "MKDTRACE=no"
                     "-V" "MKZFS=no"

                     "-V" (string-append "TOPRUMP=" toprump)
                     "-V" "BUILDRUMP_CPPFLAGS=-Wno-error=stringop-overread"
                     "-V" "RUMPUSER_EXTERNAL_DPLIBS=pthread"
		     "-V" (string-append
                           "CPPFLAGS="
                           " -I../../obj/destdir." host-cpu "/usr/include"
                           " -D_FILE_OFFSET_BITS=64"
                           " -DRUMP_REGISTER_T=int"
                           " -DRUMPUSER_CONFIG=yes"
                           " -DNO_PCI_MSI_MSIX=yes"
                           " -DNUSB_DMA=1"
                           " -DPAE")
                     "-V" (string-append
                           "CWARNFLAGS="
                           " -Wno-error=maybe-uninitialized"
                           " -Wno-error=address-of-packed-member"
                           " -Wno-error=unused-variable"
                           " -Wno-error=stack-protector"
                           " -Wno-error=array-parameter"
                           " -Wno-error=array-bounds"
                           " -Wno-error=stringop-overflow"
                           " -Wno-error=sign-compare")
                     "-V" "LIBCRTBEGIN="
                     "-V" "LIBCRTEND="
                     "-V" "LIBCRT0="
                     "-V" "LIBCRTI="
                     "-V" "_GCC_CRTENDS="
                     "-V" "_GCC_CRTEND="
                     "-V" "_GCC_CRTBEGINS="
                     "-V" "_GCC_CRTBEGIN="
                     "-V" "_GCC_CRTI="
                     "-V" "_GCC_CRTN="
                     "-U"
                     "-u"
                     "-T" "./obj/tooldir"
                     "-m" host-cpu
                     "-j" (number->string jobs)
                     "tools"
                     "rump"))
                  (with-directory-excursion "buildrump.sh/src/lib/librumpuser"
                    (setenv "RUMPRUN" "true")
                    (invoke rump-make "dependall"))
                  (with-directory-excursion "pci-userspace/src-gnu"
                    (invoke rump-make "dependall")))))
            (replace 'install
              (lambda _
                (define (install-file file target)
                  (let ((dest (string-append target (basename file))))
                    (format (current-output-port) "`~a' -> `~a'~%" file dest)
                    (mkdir-p (dirname dest))
                    ;; Some libraries are duplicated/copied around in the
                    ;; build system, do not fail trying to install one
                    ;; a second time.
                    (if (file-exists? dest)
                        (format (current-error-port)
                                "warning: skipping: ~a\n" file)
                        (let ((stat (lstat file)))
                          (case (stat:type stat)
                            ((symlink)
                             (let ((target (readlink file)))
                               (symlink target dest)))
                            (else
                             (copy-file file dest)))))))
                (let ((header (string-append #$output "/include/rump"))
                      (lib (string-append #$output "/lib/")))
                  (mkdir-p header)
                  (copy-recursively "buildrump.sh/src/sys/rump/include/rump"
                                    header)
                  (mkdir-p lib)
                  (for-each
                   (cute install-file <> lib)
                   (append (find-files "buildrump.sh/src" "librump.*[.](a|so.*)")
                           (find-files "obj" "librump.*[.](a|so.*)")))))))))
      (inputs
       (list gnumach-headers libpciaccess))
      (native-inputs
       (list autoconf
             automake
             libgcrypt
             (if (%current-target-system)
                 (cross-mig (%current-target-system))
                 mig)
             zlib))
      (supported-systems %hurd-systems)
      (home-page "https://wiki.netbsd.org/rumpkernel")
      (synopsis "NetBSD as rumpkernel for the GNU/Hurd")
      (description
       "This package provides NetBSD as rumpkernel for the GNU/Hurd, so that
the Hurd may be installed on iron.  Using this rumpkernel package, the hurd
package's rumpdisk can be built which provides the pci.arbiter and rumpdisk
servers.")
      (license
       ;; The NetBSD rumpkernel code is a big hodgepodge of softwares many of
       ;; which have their own different licensing terms, see also
       ;; https://salsa.debian.org/hurd-team/rumpkernel/-/blob/master/debian/copyright
       (list asl2.0
             boost1.0
             bsd-2
             bsd-3
             bsd-4
             cddl1.0
             expat
             gpl1
             gpl2+
             gpl3+
             isc
             lgpl2.0+
             public-domain
             (@ (guix licenses) zlib)
             (non-copyleft "file://src/lib/libc/hash/hashhl.c"
                           "See debian/copyright in the distribution."))))))
