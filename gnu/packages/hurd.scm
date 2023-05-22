;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015, 2016, 2017 Manolis Fragkiskos Ragkousis <manolis837@gmail.com>
;;; Copyright © 2018, 2020-2023 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2020 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2020, 2022 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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
  #:use-module (gnu packages xorg) ; libpciaccess
  #:use-module (guix git-download))

(define (hurd-source-url version)
  (string-append "mirror://gnu/hurd/hurd-"
                 version ".tar.gz"))

(define-public gnumach-headers
  (package
    (name "gnumach-headers")
    (version "1.8+git20221224") ;; This is an upstream tag
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.savannah.gnu.org/git/hurd/gnumach.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name "gnumach" version))
       (sha256
        (base32
         "0f49zqxf64ds75rmskizpybl2mw7sxs05k59gjp3pgspvr87w7gs"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'install
           (lambda _
             (invoke "make" "install-data")))
         (delete 'build))
       #:tests? #f))
    (native-inputs
     (list autoconf automake texinfo-4))
    (supported-systems %hurd-systems)
    (home-page "https://www.gnu.org/software/hurd/microkernel/mach/gnumach.html")
    (synopsis "GNU Mach kernel headers")
    (description
     "Headers of the GNU Mach kernel.")
    (license gpl2+)))

(define-public mig
  (package
    (name "mig")
    (version "1.8+git20230520")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://git.savannah.gnu.org/git/hurd/mig.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "10r0fdjqjzqsy6ajb21rifvhw0wpjvrw6a1zdyliqlzqny5k0qlz"))))
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
  ;; This commit is now slightly behind 0.9.git20220818 as this one needs a
  ;; newer glibc
  (let ((revision "2")
        (commit "3ff70531ee672f431dbb0c11f286bfe85dce98fc"))
    (package
      (name "hurd-headers")
      (version (git-version "0.9" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://git.savannah.gnu.org/git/hurd/hurd.git")
                      (commit commit)))
                (sha256
                 (base32
                  "1jb9f2h2v4lf6acsji1c12aqg3pixkvjdyb4q6axkd8jp22fdgc0"))
                (file-name (git-file-name name version))
                (patches (search-patches "hurd-add-without-rump-configure-option.patch"
                                         "hurd-fix-types-of-read-write-and-readables-methods.patch"
                                         "hurd-fix-types-of-read-write-and-readables-methods-2.patch"))))
      (build-system gnu-build-system)
      (native-inputs
       (list autoconf
             automake
             (if (%current-target-system)
                 (let* ((cross-base (resolve-interface '(gnu packages cross-base)))
                        (cross-mig (module-ref cross-base 'cross-mig)))
                   (cross-mig (%current-target-system)))
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
                             "ac_cv_func_file_futimens=no")

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
    (inputs (list glibc/hurd-headers))
    (arguments
     (substitute-keyword-arguments (package-arguments hurd-headers)
       ((#:phases _)
        '(modify-phases %standard-phases
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 ;; We need to copy libihash.a to the output directory manually,
                 ;; since there is no target for that in the makefile.
                 (mkdir-p (string-append out "/include"))
                 (copy-file "libihash/ihash.h"
                            (string-append out "/include/ihash.h"))
                 (mkdir-p (string-append out "/lib"))
                 (copy-file "libihash/libihash.a"
                            (string-append out "/lib/libihash.a"))
                 #t)))
           (replace 'build
             (lambda _
               ;; Install <assert-backtrace.h> & co.
               (invoke "make" "-Clibshouldbeinlibc"
                       "../include/assert-backtrace.h")

               ;; Build libihash.
               (invoke "make" "-Clibihash" "libihash.a")))))))
    (supported-systems %hurd-systems)
    (home-page "https://www.gnu.org/software/hurd/hurd.html")
    (synopsis "GNU Hurd libraries")
    (description
     "This package provides libihash, needed to build the GNU C
Library for GNU/Hurd.")
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
       ((#:make-flags flags ''())
        `(cons "CFLAGS=-fcommon" ,flags))
       ((#:configure-flags flags ''())
        `(cons "--enable-kdb" ,flags))            ;enable kernel debugger
       ((#:phases phases '%standard-phases)
        `(modify-phases %standard-phases
           (add-after 'install 'produce-image
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out  (assoc-ref outputs "out"))
                      (boot (string-append out "/boot")))
                 (invoke "make" "gnumach.gz")
                 (install-file "gnumach.gz" boot))))))))
    (native-inputs
     (list autoconf
           automake
           (if (%current-target-system)
                   (let* ((cross-base (resolve-interface '(gnu packages cross-base)))
                          (cross-mig (module-ref cross-base 'cross-mig)))
                     (cross-mig (%current-target-system)))
                   mig)
           perl
           texinfo-4))
    (supported-systems %hurd-systems)
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
  (let ((commit "ce8810277fa3584eb36ecb23da58394153fabe6f"))
    (origin
      (method git-fetch)
      (uri (git-reference
            (url "https://git.savannah.gnu.org/git/hurd/incubator.git")
            (commit commit)))
      (sha256
       (base32
        "0ygk7jm4jmhpvh0zzi5bk638242z7sbcab2i57fkb4y2mmdkjjbw"))
      (file-name (git-file-name "dde" commit)))))

(define %import-from-dde
  (list "libmachdevdde" "libddekit" "libdde_linux26"))

(define %add-to-hurd-subdirs
  (list "libmachdevdde" "libddekit"))

(define-public hurd
  (package
    (name "hurd")
    (source (package-source hurd-headers))
    (version (package-version hurd-headers))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'prepare-dde
           (lambda* (#:key native-inputs inputs #:allow-other-keys)
             ;; First we import the things we want from dde.
             (for-each make-file-writable (find-files "."))
             (let ((dde (or (assoc-ref inputs "dde-sources")
                            (assoc-ref native-inputs "dde-sources"))))
               (for-each (lambda (dir)
                           (copy-recursively
                            (string-append dde "/" dir ) dir))
                         '("libmachdevdde" "libddekit" "libdde_linux26")))
             ;; And we add some as subdirs so that they're built by the main
             ;; Makefile. libdde_linux26 is built later in its own phase.
             (substitute* "Makefile"
               (("libbpf ")
                "libbpf libmachdevdde libddekit"))))
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

# Note: this /hurd/ gets substituted
settrans --create /servers/socket/1 /hurd/pflocal

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
             (invoke (string-append (assoc-ref native-inputs "make")
                                    "/bin/make")
                     ;; XXX There can be a race condition because subdirs
                     ;; aren't interdependent targets in the Makefile.
                     "-j1" "-C" "libdde_linux26"
                     (string-append "SHELL="
                                    (assoc-ref native-inputs "bash")
                                    "/bin/bash")
                     (string-append "CC="
                                    ,(cc-for-target)))))
         (add-after 'install 'install-goodies
           (lambda* (#:key inputs native-inputs outputs #:allow-other-keys)
             ;; Install additional goodies.
             ;; TODO: Build & install *.msgids for rpctrace.
             (let* ((out (assoc-ref outputs "out"))
                    (datadir (string-append out "/share/hurd")))
               ;; Install libdde_linux26.
               (invoke (string-append (assoc-ref native-inputs "make")
                                      "/bin/make")
                       "-C" "libdde_linux26" "install"
                       (string-append "SHELL="
                                    (assoc-ref native-inputs "bash")
                                    "/bin/bash")
                       (string-append "INSTALLDIR="
                                      out
                                      "/share/libdde_linux26/build/include"))
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
                          (string-append datadir "/vga-system.bdf"))
               #t))))
       #:configure-flags
       ,#~(list (string-append "LDFLAGS=-Wl,-rpath="
                               #$output "/lib")
                "--disable-ncursesw"
                "--without-libbz2"
                "--without-libz"
                "--without-parted"
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
       ("libpciaccess" ,libpciaccess)

       ;; For NFS support
       ("libtirpc" ,libtirpc/hurd)

       ;; Tools for the /libexec/* scripts.
       ("bash-minimal" ,bash-minimal)
       ("coreutils" ,coreutils)
       ("sed" ,sed)
       ("grep" ,grep)
       ("util-linux" ,util-linux)))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libgcrypt" ,libgcrypt)                   ;for 'libgcrypt-config'
       ("mig" ,(if (%current-target-system)
                   (let* ((cross-base (resolve-interface '(gnu packages cross-base)))
                          (cross-mig (module-ref cross-base 'cross-mig)))
                     (cross-mig (%current-target-system)))
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
  (let ((commit "4a1016f130b6f2065d3f088325e5fb0b2997ae12")
        (revision "1"))
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
                (sha256
                 (base32
                  "1njv9dszq4lj05yq4v9j5v247hfghpzvvz4hzy0khjjr35mw7hr8"))
                (file-name (git-file-name name commit))))
      (build-system gnu-build-system)
      (arguments
       `(#:make-flags
         (list (string-append "SHELL="
                              (search-input-file %build-inputs "/bin/bash"))
               "PKGDIR=libdde_linux26"
               ,@(if (%current-target-system)
                     (list "CC=i586-pc-gnu-gcc"
                           "LINK_PROGRAM=i586-pc-gnu-gcc")
                     (list "CC=gcc")))
         #:configure-flags
         ,#~(list (string-append "LDFLAGS=-Wl,-rpath=" #$output "/lib"))
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (add-after 'unpack 'prepare-dde
             (lambda* (#:key native-inputs inputs #:allow-other-keys)
               (for-each make-file-writable (find-files "."))
               (let ((dde (or (assoc-ref inputs "dde-sources")
                              (assoc-ref native-inputs "dde-sources"))))
                 (for-each (lambda (dir)
                             (copy-recursively
                              (string-append dde "/" dir ) dir))
                           '("libdde_linux26" "libddekit")))
               (substitute* "libdde_linux26/mk/rel2abs.sh"
                 (("/bin/bash") (which "bash")))
               #t))
           (add-after 'patch-generated-file-shebangs 'build-libdde-linux26
             (lambda* (#:key make-flags #:allow-other-keys)
               (with-directory-excursion "libdde_linux26"
                 (apply invoke "make"
                        (delete "PKGDIR=libdde_linux26" make-flags)))))
           (add-after 'build-libdde-linux26 'convert
             (lambda* (#:key make-flags #:allow-other-keys)
               (apply invoke "make" "convert" make-flags)))
           (replace 'build
             (lambda* (#:key make-flags #:allow-other-keys)
               ;; no-common can be dropped with GCC 10+ where this is the
               ;; default.
               (apply invoke "make" "CFLAGS=-fno-common" make-flags)))
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (install-file "netdde"
                             (string-append (assoc-ref outputs "out")
                                            "/bin"))
               #t)))))
      (inputs
       (list hurd libpciaccess zlib))
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
