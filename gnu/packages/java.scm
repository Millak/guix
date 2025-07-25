;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015-2024 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016, 2017 Roel Janssen <roel@gnu.org>
;;; Copyright © 2017, 2019, 2021 Carlo Zancanaro <carlo@zancanaro.id.au>
;;; Copyright © 2017-2022 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2017 Thomas Danckaert <post@thomasdanckaert.be>
;;; Copyright © 2016, 2017, 2018 Alex Vong <alexvong1995@gmail.com>
;;; Copyright © 2017, 2019, 2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018, 2019 Gábor Boskovits <boskovits@gmail.com>
;;; Copyright © 2018 Chris Marusich <cmmarusich@gmail.com>
;;; Copyright © 2018-2024 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2019, 2020, 2021 Björn Höfling <bjoern.hoefling@bjoernhoefling.de>
;;; Copyright © 2020, 2025 Janneke Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2020 Raghav Gururajan <raghavgururajan@disroot.org>
;;; Copyright © 2020, 2022, 2025 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2021 Vincent Legoll <vincent.legoll@gmail.com>
;;; Copyright © 2021 Mike Gerwitz <mtg@gnu.org>
;;; Copyright © 2021 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2021 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2022, 2024 Artyom V. Poptsov <poptsov.artyom@gmail.com>
;;; Copyright © 2024 Paul A. Patience <paul@apatience.com>
;;; Copyright © 2024 Raven Hallsby <karl@hallsby.com>
;;; Copyright © 2025 Zheng Junjie <873216071@qq.com>
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

(define-module (gnu packages java)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix hg-download)
  #:use-module (guix git-download)
  #:use-module (guix svn-download)
  #:use-module ((guix build utils) #:select (alist-replace))
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix build-system ant)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system maven)
  #:use-module (guix build-system python)
  #:use-module (gnu packages)
  #:use-module (gnu packages attr)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages cpio)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop) ; wayland
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages ghostscript) ;lcms
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages groovy)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image)
  #:use-module (gnu packages java-bootstrap)
  #:use-module (gnu packages java-compression)
  #:use-module (gnu packages java-xml)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages linux) ;alsa
  #:use-module (gnu packages maths)
  #:use-module (gnu packages maven)
  #:use-module (gnu packages maven-parent-pom)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages onc-rpc)
  #:use-module (gnu packages web)
  #:use-module (gnu packages wget)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages popt)
  #:use-module (gnu packages kerberos)
  #:use-module (gnu packages security-token)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages texinfo)
  #:use-module ((srfi srfi-1) #:select (fold alist-delete))
  #:use-module (srfi srfi-11)
  #:use-module (ice-9 match))

(define-public icedtea-7
  (let* ((version "2.6.13")
         (drop (lambda* (name hash #:optional (patches '()))
                 (origin
                   (method url-fetch)
                   (uri (string-append
                         "http://icedtea.classpath.org/download/drops"
                         "/icedtea7/" version "/" name ".tar.bz2"))
                   (sha256 (base32 hash))
                   (patches patches)))))
    (package
      (name "icedtea")
      (version version)
      (source (origin
                (method url-fetch)
                (uri (string-append
                      "http://icedtea.wildebeest.org/download/source/icedtea-"
                      version ".tar.xz"))
                (sha256
                 (base32
                  "1w331rdqx1dcx2xb0fmjmrkdc71xqn20fxsgw8by4xhiblh88khh"))
                (modules '((guix build utils)))
                (snippet
                 '(begin
                    (substitute* "Makefile.in"
                      ;; do not leak information about the build host
                      (("DISTRIBUTION_ID=\"\\$\\(DIST_ID\\)\"")
                       "DISTRIBUTION_ID=\"\\\"guix\\\"\""))
                    #t))))
      (build-system gnu-build-system)
      (outputs '("out"   ; Java Runtime Environment
                 "jdk"   ; Java Development Kit
                 "doc")) ; all documentation
      (arguments
       `(;; There are many test failures.  Some are known to
         ;; fail upstream, others relate to not having an X
         ;; server running at test time, yet others are a
         ;; complete mystery to me.

         ;; hotspot:   passed: 241; failed: 45; error: 2
         ;; langtools: passed: 1,934; failed: 26
         ;; jdk:       unknown
         #:tests? #f

         ;; The DSOs use $ORIGIN to refer to each other, but (guix build
         ;; gremlin) doesn't support it yet, so skip this phase.
         #:validate-runpath? #f

         #:modules ((guix build utils)
                    (guix build gnu-build-system)
                    (ice-9 match)
                    (ice-9 popen)
                    (srfi srfi-19)
                    (srfi srfi-26))

         #:configure-flags
         ;; TODO: package sctp and add to inputs
         `("--disable-system-sctp"
           "--enable-system-pcsc"
           "--enable-system-lcms"
           "--enable-bootstrap"
           "--enable-nss"
           "--without-rhino"
           ,(string-append "--with-parallel-jobs="
                           (number->string (parallel-job-count)))
           "--disable-downloading"
           "--disable-tests"        ;they are run in the check phase instead
           "--with-openjdk-src-dir=./openjdk.src"
           ,(string-append "--with-ecj="
                           (assoc-ref %build-inputs "ecj4-javac-wrapper")
                           "/bin/javac")
           ,(string-append "--with-jdk-home="
                           (assoc-ref %build-inputs "classpath"))
           ,(string-append "--with-java="
                           (assoc-ref %build-inputs "jamvm")
                           "/bin/jamvm")
           ,(string-append "--with-jar="
                           (assoc-ref %build-inputs "classpath")
                           "/bin/gjar"))
         #:phases
         (modify-phases %standard-phases
           (replace 'unpack
             (lambda* (#:key source inputs #:allow-other-keys)
               (let ((target (string-append "icedtea-" ,version))
                     (unpack (lambda* (name #:optional dir)
                               (let ((dir (or dir
                                              (string-drop-right name 5))))
                                 (mkdir dir)
                                 (invoke "tar" "xvf"
                                         (assoc-ref inputs name)
                                         "-C" dir
                                         "--strip-components=1")))))
                 (mkdir target)
                 (invoke "tar" "xvf" source
                         "-C" target "--strip-components=1")
                 (chdir target)
                 (unpack "openjdk-src" "openjdk.src")
                 (with-directory-excursion "openjdk.src"
                   (for-each unpack
                             (filter (cut string-suffix? "-drop" <>)
                                     (map (match-lambda
                                            ((name . _) name))
                                          inputs)))))))
           (add-after 'unpack 'use-classpath
             (lambda* (#:key inputs #:allow-other-keys)
               (let ((tools  (search-input-file inputs "/share/classpath/tools.zip"))
                     (rt.jar (search-input-file inputs "/lib/rt.jar")))
                 ;; GNU Classpath does not provide rt.jar, but jamvm provides
                 ;; Classpath's glibj.zip as rt.jar, so we just use that.
                 (substitute* "Makefile.in"
                   (("\\$\\(SYSTEM_JDK_DIR\\)/jre/lib/rt.jar") rt.jar))
                 ;; Make sure we can find all classes.
                 (setenv "CLASSPATH"
                         (string-append rt.jar ":" tools))
                 (setenv "JAVACFLAGS"
                         (string-append "-cp " rt.jar ":" tools)))))
           (add-after 'unpack 'patch-bitrot
             (lambda _
               (substitute* '("patches/boot/revert-6973616.patch"
                              "openjdk.src/jdk/make/common/shared/Defs-versions.gmk")
                 (("REQUIRED_FREETYPE_VERSION = 2.2.1")
                  "REQUIRED_FREETYPE_VERSION = 2.10.1"))
               ;; As of attr 2.4.48 this header is no longer
               ;; included.  It is provided by the libc instead.
               (substitute* '("configure"
                              "openjdk.src/jdk/src/solaris/native/sun/nio/fs/LinuxNativeDispatcher.c")
                 (("attr/xattr.h") "sys/xattr.h"))))
           (add-after 'unpack 'fix-openjdk
             (lambda _
               (substitute* "openjdk.src/jdk/make/common/Defs-linux.gmk"
                 (("CFLAGS_COMMON   = -fno-strict-aliasing" all)
                  (string-append all
                                 " -fcommon"
                                 " -Wno-error=implicit-function-declaration"
                                 " -Wno-error=implicit-int"
                                 " -Wno-error=incompatible-pointer-types"
                                 " -Wno-error=int-conversion")))
               (substitute*
                   '("openjdk.src/jdk/src/solaris/native/java/net/PlainSocketImpl.c"
                     "openjdk.src/jdk/src/solaris/native/java/net/PlainDatagramSocketImpl.c")
                 (("#include <sys/sysctl.h>")
                  "#include <linux/sysctl.h>"))

               ;; XXX 'ldd' in glibc 2.35 segfaults upon reading
               ;;   openjdk.build-boot/lib/amd64/libnio.so (!).
               ;; It is only used as a verification step, so ignore it;
               ;; try removing this substitution for newer versions of glibc.
               (substitute* "openjdk.src/jdk/make/common/shared/Defs-linux.gmk"
                 (("\\$\\(LDD\\) \\$1 &&")
                  ""))

               ;; It looks like the "h = 31 * h + c" line of the jsum()
               ;; function gets miscompiled. After a few iterations of the loop
               ;; the result of "31 * h" is always 0x8000000000000000.
               ;; Disable optimizations of dump.cpp as a workaround.
               (substitute* "openjdk.src/hotspot/make/linux/makefiles/gcc.make"
                 (("OPT_CFLAGS/NOOPT.*" all)
                  (string-append all "\n"
                                 "OPT_CFLAGS/dump.o += -O0")))))
           (add-after 'unpack 'fix-x11-extension-include-path
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "openjdk.src/jdk/make/sun/awt/mawt.gmk"
                 (((string-append "\\$\\(firstword \\$\\(wildcard "
                                  "\\$\\(OPENWIN_HOME\\)"
                                  "/include/X11/extensions\\).*$"))
                  (string-append (assoc-ref inputs "libxrender")
                                 "/include/X11/extensions"
                                 " -I" (assoc-ref inputs "libxtst")
                                 "/include/X11/extensions"
                                 " -I" (assoc-ref inputs "libxinerama")
                                 "/include/X11/extensions"))
                 (("\\$\\(wildcard /usr/include/X11/extensions\\)\\)") ""))))
           (add-after 'unpack 'patch-paths
             (lambda* (#:key inputs #:allow-other-keys)
               ;; buildtree.make generates shell scripts, so we need to replace
               ;; the generated shebang
               (substitute* '("openjdk.src/hotspot/make/linux/makefiles/buildtree.make")
                 (("/bin/sh") (which "bash")))

               (let ((corebin (string-append
                               (assoc-ref inputs "coreutils") "/bin/"))
                     (binbin  (string-append
                               (assoc-ref inputs "binutils") "/bin/"))
                     (grepbin (string-append
                               (assoc-ref inputs "grep") "/bin/")))
                 (substitute* '("openjdk.src/jdk/make/common/shared/Defs-linux.gmk"
                                "openjdk.src/corba/make/common/shared/Defs-linux.gmk")
                   (("UNIXCOMMAND_PATH  = /bin/")
                    (string-append "UNIXCOMMAND_PATH = " corebin))
                   (("USRBIN_PATH  = /usr/bin/")
                    (string-append "USRBIN_PATH = " corebin))
                   (("DEVTOOLS_PATH *= */usr/bin/")
                    (string-append "DEVTOOLS_PATH = " corebin))
                   (("COMPILER_PATH *= */usr/bin/")
                    (string-append "COMPILER_PATH = "
                                   (assoc-ref inputs "gcc") "/bin/"))
                   (("DEF_OBJCOPY *=.*objcopy")
                    (string-append "DEF_OBJCOPY = " (which "objcopy"))))

                 ;; fix path to alsa header
                 (substitute* "openjdk.src/jdk/make/common/shared/Sanity.gmk"
                   (("ALSA_INCLUDE=/usr/include/alsa/version.h")
                    (string-append "ALSA_INCLUDE="
                                   (assoc-ref inputs "alsa-lib")
                                   "/include/alsa/version.h")))

                 ;; fix hard-coded utility paths
                 (substitute* '("openjdk.src/jdk/make/common/shared/Defs-utils.gmk"
                                "openjdk.src/corba/make/common/shared/Defs-utils.gmk")
                   (("ECHO *=.*echo")
                    (string-append "ECHO = " (which "echo")))
                   (("^GREP *=.*grep")
                    (string-append "GREP = " (which "grep")))
                   (("EGREP *=.*egrep")
                    (string-append "EGREP = " (which "egrep")))
                   (("CPIO *=.*cpio")
                    (string-append "CPIO = " (which "cpio")))
                   (("READELF *=.*readelf")
                    (string-append "READELF = " (which "readelf")))
                   (("^ *AR *=.*ar")
                    (string-append "AR = " (which "ar")))
                   (("^ *TAR *=.*tar")
                    (string-append "TAR = " (which "tar")))
                   (("AS *=.*as")
                    (string-append "AS = " (which "as")))
                   (("LD *=.*ld")
                    (string-append "LD = " (which "ld")))
                   (("STRIP *=.*strip")
                    (string-append "STRIP = " (which "strip")))
                   (("NM *=.*nm")
                    (string-append "NM = " (which "nm")))
                   (("^SH *=.*sh")
                    (string-append "SH = " (which "bash")))
                   (("^FIND *=.*find")
                    (string-append "FIND = " (which "find")))
                   (("LDD *=.*ldd")
                    (string-append "LDD = " (which "ldd")))
                   (("NAWK *=.*(n|g)awk")
                    (string-append "NAWK = " (which "gawk")))
                   (("XARGS *=.*xargs")
                    (string-append "XARGS = " (which "xargs")))
                   (("UNZIP *=.*unzip")
                    (string-append "UNZIP = " (which "unzip")))
                   (("ZIPEXE *=.*zip")
                    (string-append "ZIPEXE = " (which "zip")))
                   (("SED *=.*sed")
                    (string-append "SED = " (which "sed"))))

                 ;; Some of these timestamps cause problems as they are more than
                 ;; 10 years ago, failing the build process.
                 (substitute*
                     "openjdk.src/jdk/src/share/classes/java/util/CurrencyData.properties"
                   (("AZ=AZM;2005-12-31-20-00-00;AZN") "AZ=AZN")
                   (("MZ=MZM;2006-06-30-22-00-00;MZN") "MZ=MZN")
                   (("RO=ROL;2005-06-30-21-00-00;RON") "RO=RON")
                   (("TR=TRL;2004-12-31-22-00-00;TRY") "TR=TRY")))))
           (add-before 'configure 'set-additional-paths
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "openjdk.src/jdk/make/common/shared/Sanity.gmk"
                 (("ALSA_INCLUDE=/usr/include/alsa/version.h")
                  (string-append "ALSA_INCLUDE="
                                 (assoc-ref inputs "alsa-lib")
                                 "/include/alsa/version.h")))
               (setenv "CC" "gcc")
               (setenv "CPATH"
                       (string-append (assoc-ref inputs "libxcomposite")
                                      "/include/X11/extensions" ":"
                                      (assoc-ref inputs "libxrender")
                                      "/include/X11/extensions" ":"
                                      (assoc-ref inputs "libxtst")
                                      "/include/X11/extensions" ":"
                                      (assoc-ref inputs "libxinerama")
                                      "/include/X11/extensions" ":"
                                      (or (getenv "CPATH") "")))
               (setenv "ALT_OBJCOPY" (which "objcopy"))
               (setenv "ALT_CUPS_HEADERS_PATH"
                       (string-append (assoc-ref inputs "cups")
                                      "/include"))
               (setenv "ALT_FREETYPE_HEADERS_PATH"
                       (string-append (assoc-ref inputs "freetype")
                                      "/include"))
               (setenv "ALT_FREETYPE_LIB_PATH"
                       (string-append (assoc-ref inputs "freetype")
                                      "/lib"))))
           (add-before 'build 'disable-os-version-check
             ;; allow build on linux major version change
             (lambda _
               (setenv "DISABLE_HOTSPOT_OS_VERSION_CHECK" "ok")))
           (add-before 'check 'fix-test-framework
             (lambda _
               ;; Fix PATH in test environment
               (substitute* "test/jtreg/com/sun/javatest/regtest/Main.java"
                 (("PATH=/bin:/usr/bin")
                  (string-append "PATH=" (getenv "PATH"))))
               (substitute* "test/jtreg/com/sun/javatest/util/SysEnv.java"
                 (("/usr/bin/env") (which "env")))
               (substitute* "openjdk.src/hotspot/test/test_env.sh"
                 (("/bin/rm") (which "rm"))
                 (("/bin/cp") (which "cp"))
                 (("/bin/mv") (which "mv")))))
           (add-before 'check 'fix-hotspot-tests
             (lambda _
               (with-directory-excursion "openjdk.src/hotspot/test/"
                 (substitute* "jprt.config"
                   (("PATH=\"\\$\\{path4sdk\\}\"")
                    (string-append "PATH=" (getenv "PATH")))
                   (("make=/usr/bin/make")
                    (string-append "make=" (which "make"))))
                 (substitute* '("runtime/6626217/Test6626217.sh"
                                "runtime/7110720/Test7110720.sh")
                   (("/bin/rm") (which "rm"))
                   (("/bin/cp") (which "cp"))
                   (("/bin/mv") (which "mv"))))))
           (add-before 'check 'fix-jdk-tests
             (lambda _
               (with-directory-excursion "openjdk.src/jdk/test/"
                 (substitute* "com/sun/jdi/JdbReadTwiceTest.sh"
                   (("/bin/pwd") (which "pwd")))
                 (substitute* "com/sun/jdi/ShellScaffold.sh"
                   (("/bin/kill") (which "kill")))
                 (substitute* "start-Xvfb.sh"
                   ;;(("/usr/bin/X11/Xvfb") (which "Xvfb"))
                   (("/usr/bin/nohup")    (which "nohup")))
                 (substitute* "javax/security/auth/Subject/doAs/Test.sh"
                   (("/bin/rm") (which "rm")))
                 (substitute* "tools/launcher/MultipleJRE.sh"
                   (("echo \"#!/bin/sh\"")
                    (string-append "echo \"#!" (which "rm") "\""))
                   (("/usr/bin/zip") (which "zip")))
                 (substitute* "com/sun/jdi/OnThrowTest.java"
                   (("#!/bin/sh") (string-append "#!" (which "sh"))))
                 (substitute* "java/lang/management/OperatingSystemMXBean/GetSystemLoadAverage.java"
                   (("/usr/bin/uptime") (which "uptime")))
                 (substitute* "java/lang/ProcessBuilder/Basic.java"
                   (("/usr/bin/env") (which "env"))
                   (("/bin/false") (which "false"))
                   (("/bin/true") (which "true"))
                   (("/bin/cp") (which "cp"))
                   (("/bin/sh") (which "sh")))
                 (substitute* "java/lang/ProcessBuilder/FeelingLucky.java"
                   (("/bin/sh") (which "sh")))
                 (substitute* "java/lang/ProcessBuilder/Zombies.java"
                   (("/usr/bin/perl") (which "perl"))
                   (("/bin/ps") (which "ps"))
                   (("/bin/true") (which "true")))
                 (substitute* "java/lang/Runtime/exec/ConcurrentRead.java"
                   (("/usr/bin/tee") (which "tee")))
                 (substitute* "java/lang/Runtime/exec/ExecWithDir.java"
                   (("/bin/true") (which "true")))
                 (substitute* "java/lang/Runtime/exec/ExecWithInput.java"
                   (("/bin/cat") (which "cat")))
                 (substitute* "java/lang/Runtime/exec/ExitValue.java"
                   (("/bin/sh") (which "sh"))
                   (("/bin/true") (which "true"))
                   (("/bin/kill") (which "kill")))
                 (substitute* "java/lang/Runtime/exec/LotsOfDestroys.java"
                   (("/usr/bin/echo") (which "echo")))
                 (substitute* "java/lang/Runtime/exec/LotsOfOutput.java"
                   (("/usr/bin/cat") (which "cat")))
                 (substitute* "java/lang/Runtime/exec/SleepyCat.java"
                   (("/bin/cat") (which "cat"))
                   (("/bin/sleep") (which "sleep"))
                   (("/bin/sh") (which "sh")))
                 (substitute* "java/lang/Runtime/exec/StreamsSurviveDestroy.java"
                   (("/bin/cat") (which "cat")))
                 (substitute* "java/rmi/activation/CommandEnvironment/SetChildEnv.java"
                   (("/bin/chmod") (which "chmod")))
                 (substitute* "java/util/zip/ZipFile/Assortment.java"
                   (("/bin/sh") (which "sh"))))))
           (replace 'check
             (lambda* (#:key tests? #:allow-other-keys)
               ;; The "make check-*" targets always return zero, so we need to
               ;; check for errors in the associated log files to determine
               ;; whether any tests have failed.
               (when tests?
                 (use-modules (ice-9 rdelim))
                 (let* ((error-pattern (make-regexp "^(Error|FAILED):.*"))
                        (checker (lambda (port)
                                   (let loop ()
                                     (let ((line (read-line port)))
                                       (cond
                                        ((eof-object? line) #t)
                                        ((regexp-exec error-pattern line)
                                         (error "test failed"))
                                        (else (loop)))))))
                        (run-test (lambda (test)
                                    (invoke "make" test)
                                    (call-with-input-file
                                        (string-append "test/" test ".log")
                                      checker))))
                   (run-test "check-hotspot")
                   (run-test "check-langtools")
                   (run-test "check-jdk")))))
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((doc (string-append (assoc-ref outputs "doc")
                                         "/share/doc/icedtea"))
                     (jre (assoc-ref outputs "out"))
                     (jdk (assoc-ref outputs "jdk")))
                 (copy-recursively "openjdk.build/docs" doc)
                 (copy-recursively "openjdk.build/j2re-image" jre)
                 (copy-recursively "openjdk.build/j2sdk-image" jdk))))
           ;; Some of the libraries in the lib/amd64 folder link to libjvm.so.
           ;; But that shared object is located in the server/ folder, so it
           ;; cannot be found.  This phase creates a symbolic link in the
           ;; lib/amd64 folder so that the other libraries can find it.
           ;;
           ;; See:
           ;; https://lists.gnu.org/archive/html/guix-devel/2017-10/msg00169.html
           ;;
           ;; FIXME: Find the bug in the build system, so that this symlink is
           ;; not needed.
           (add-after 'install 'install-libjvm
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let* ((lib-path (string-append (assoc-ref outputs "out")
                                               ;; See 'INSTALL_ARCH_DIR' in
                                               ;; 'configure'.
                                               ,(match (%current-system)
                                                  ("i686-linux"
                                                   "/lib/i386")
                                                  ("x86_64-linux"
                                                   "/lib/amd64")
                                                  ("armhf-linux"
                                                   "/lib/arm")
                                                  ("aarch64-linux"
                                                   "/lib/aarch64")
                                                  ("powerpc-linux"
                                                   "/lib/ppc")
                                                  ;; We need a catch-all, dropping
                                                  ;; '-linux' works in most cases.
                                                  (_
                                                    (string-append
                                                      "/lib/"
                                                      (string-drop-right
                                                        (%current-system) 6)))))))
                 (symlink (string-append lib-path "/server/libjvm.so")
                          (string-append lib-path "/libjvm.so")))))
           ;; By default IcedTea only generates an empty keystore.  In order to
           ;; be able to use certificates in Java programs we need to generate a
           ;; keystore from a set of certificates.  For convenience we use the
           ;; certificates from the nss-certs package.
           (add-after 'install 'install-keystore
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (use-modules (ice-9 rdelim))
               (let* ((keystore  "cacerts")
                      (certs-dir (search-input-directory inputs
                                                         "etc/ssl/certs"))
                      (keytool   (string-append (assoc-ref outputs "jdk")
                                                "/bin/keytool")))
                 (define (extract-cert file target)
                   (call-with-input-file file
                     (lambda (in)
                       (call-with-output-file target
                         (lambda (out)
                           (let loop ((line (read-line in 'concat))
                                      (copying? #f))
                             (cond
                              ((eof-object? line) #t)
                              ((string-prefix? "-----BEGIN" line)
                               (display line out)
                               (loop (read-line in 'concat) #t))
                              ((string-prefix? "-----END" line)
                               (display line out)
                               #t)
                              (else
                               (when copying? (display line out))
                               (loop (read-line in 'concat) copying?)))))))))
                 (define (import-cert cert)
                   (format #t "Importing certificate ~a\n" (basename cert))
                   (let ((temp "tmpcert"))
                     (extract-cert cert temp)
                     (let ((port (open-pipe* OPEN_WRITE keytool
                                             "-import"
                                             "-alias" (basename cert)
                                             "-keystore" keystore
                                             "-storepass" "changeit"
                                             "-file" temp)))
                       (display "yes\n" port)
                       (when (not (zero? (status:exit-val (close-pipe port))))
                         (format #t "failed to import ~a\n" cert)))
                     (delete-file temp)))

                 ;; This is necessary because the certificate directory contains
                 ;; files with non-ASCII characters in their names.
                 (setlocale LC_ALL "C.UTF-8")
                 (setenv "LC_ALL" "C.UTF-8")

                 (for-each import-cert (find-files certs-dir "\\.pem$"))
                 (mkdir-p (string-append (assoc-ref outputs "out")
                                         "/lib/security"))
                 (mkdir-p (string-append (assoc-ref outputs "jdk")
                                         "/jre/lib/security"))

                 ;; The cacerts files we are going to overwrite are chmod'ed as
                 ;; read-only (444) in icedtea-8 (which derives from this
                 ;; package).  We have to change this so we can overwrite them.
                 (chmod (string-append (assoc-ref outputs "out")
                                       "/lib/security/" keystore) #o644)
                 (chmod (string-append (assoc-ref outputs "jdk")
                                       "/jre/lib/security/" keystore) #o644)

                 (install-file keystore
                               (string-append (assoc-ref outputs "out")
                                              "/lib/security"))
                 (install-file keystore
                               (string-append (assoc-ref outputs "jdk")
                                              "/jre/lib/security"))))))))
      (native-inputs
       ;; XXX: Compiling with gcc-14 will encounter the following error:
       ;; BUILD FAILED:
       ;; /tmp/guix-build-icedtea-2.6.13.drv-0/icedtea-2.6.13/openjdk/langtools/
       ;; make/build.xml:452: The following error occurred while executing this
       ;; line:
       ;; java.lang.AbstractMethodError: java.lang.Throwable.toString()Ljava/lang/String;
       `(("gcc" ,gcc-13)
         ("openjdk-src"
          ,(drop "openjdk"
                 "0l34ikyf62hbzlf9032alzkkqvf7bpmckz4gvirvph755w7gka8l"))
         ("corba-drop"
          ,(drop "corba"
                 "050gv2jbg1pi6qkn8w18bwpbklfa5b0kymjvan9pncddbj8m84fz"))
         ("jaxp-drop"
          ,(drop "jaxp"
                 "1k6yldwnxfzdg5926r1nlfv8d1r1j7rlp2nkz6gqh05vgyamnfhl"))
         ("jaxws-drop"
          ,(drop "jaxws"
                 "110j7jlz47x2gg6f7653x12mssan5kvj9l9h1m1c8c92drfxbqyk"))
         ("jdk-drop"
          ,(drop "jdk"
                 "0d1mca38ksxvdskp9im3pp7fdijhj1n3lwq9w13r9s4v3qyskgdd"
                 (search-patches "jdk-currency-time-bomb.patch")))
         ("langtools-drop"
          ,(drop "langtools"
                 "0nq5236fzxn3p6x8cgncl56mzcmsj07q9gymysnws4c8byc6n0qj"))
         ("hotspot-drop"
          ,(origin
             (method url-fetch)
             (uri (string-append
                   "http://icedtea.classpath.org/downloads/drops"
                   "/icedtea7/" version "/hotspot.tar.bz2"))
             (sha256
              (base32
               "17bdv39n4lh8l5737c96f3xgamx4y305m067p01cywgp7zaddqws"))
             (patches (search-patches
                       "icedtea-7-hotspot-aarch64-use-c++98.patch"
                       "icedtea-7-hotspot-pointer-comparison.patch"))))
         ("ant" ,ant-bootstrap)
         ("attr" ,attr)
         ("classpath" ,classpath-devel)
         ("coreutils" ,coreutils)
         ("diffutils" ,diffutils)       ;for tests
         ("ecj4-javac-wrapper" ,ecj4-javac-wrapper)
         ("fastjar" ,fastjar) ;only for the configure phase; we actually use gjar
         ("gawk" ,gawk)
         ("grep" ,grep)
         ("jamvm" ,jamvm-with-ecj4)
         ("libtool" ,libtool)
         ("pkg-config" ,pkg-config)
         ("wget" ,wget)
         ("which" ,which)
         ("cpio" ,cpio)
         ("zip" ,zip)
         ("unzip" ,unzip)
         ("libxslt" ,libxslt)           ;for xsltproc
         ("nss-certs" ,nss-certs)
         ("perl" ,perl)
         ("procps" ,procps)))  ;for "free", even though I'm not sure we should use it
      (inputs
       (list alsa-lib
             cups
             fontconfig
             freetype
             giflib
             gtk+-2
             lcms
             libjpeg-turbo
             libpng
             libx11
             libxcomposite
             libxi
             libxinerama
             libxrender
             libxt
             libxtst
             mit-krb5
             nss
             pcsc-lite
             zlib))
      (home-page "https://icedtea.classpath.org")
      (synopsis "Java development kit")
      (description
       "This package provides the Java development kit OpenJDK built with the
IcedTea build harness.")

      ;; 'configure' lists "mips" and "mipsel", but not "mips64el'.
      (supported-systems (delete "mips64el-linux" %supported-systems))

      ;; IcedTea is released under the GPL2 + Classpath exception, which is the
      ;; same license as both GNU Classpath and OpenJDK.
      (license license:gpl2+))))

(define-public icedtea-8
  (let* ((version "3.19.0")
         (drop (lambda* (name hash #:optional (patches '()))
                 (origin
                   (method url-fetch)
                   (uri (string-append
                         "http://icedtea.classpath.org/download/drops"
                         "/icedtea8/" version "/" name ".tar.xz"))
                   (sha256 (base32 hash))
                   (patches patches)))))
    (package (inherit icedtea-7)
      (version "3.19.0")
      (source (origin
                (method url-fetch)
                (uri (string-append
                      "http://icedtea.wildebeest.org/download/source/icedtea-"
                      version ".tar.xz"))
                (sha256
                 (base32
                  "1cmms7cb2sav3ywc36ynqmybzx73sl279rm6j8i5nqrmp98ixmpf"))
                (modules '((guix build utils)))
                (snippet
                 '(substitute* '("configure"
                                 "acinclude.m4")
                    ;; Do not embed build time
                    (("(DIST_ID=\"Custom build).*$" _ prefix)
                     (string-append prefix "\"\n"))
                    ;; Do not leak information about the build host
                    (("DIST_NAME=\"\\$build_os\"")
                     "DIST_NAME=\"guix\"")))))
      (arguments
       `(#:imported-modules
         ((guix build ant-build-system)
          ,@%default-gnu-imported-modules)

         #:disallowed-references ,(list (gexp-input icedtea-7 "jdk"))

         ,@(substitute-keyword-arguments (package-arguments icedtea-7)
             ((#:modules modules)
              `((guix build utils)
                (guix build gnu-build-system)
                ((guix build ant-build-system) #:prefix ant:)
                (ice-9 match)
                (ice-9 popen)
                (srfi srfi-19)
                (srfi srfi-26)))
             ((#:configure-flags flags)
              `(let ((jdk (assoc-ref %build-inputs "jdk")))
                 `(,(string-append "CFLAGS=-fcommon"
                                   " -Wno-error=implicit-function-declaration"
                                   " -Wno-error=implicit-int"
                                   " -Wno-error=incompatible-pointer-types"
                                   " -Wno-error=int-conversion")
                   "CXXFLAGS=-fcommon"
                   "--enable-bootstrap"
                   "--enable-nss"
                   ,(string-append "--with-parallel-jobs="
                                   (number->string (parallel-job-count)))
                   ;; Java Flight Recorder isn't supported on some architectures.
                   ,@(if ,(target-ppc32?)
                       `("--enable-jfr=no")
                       '())
                   "--disable-docs"     ; This phase can take hours on slow machines.
                   "--disable-downloading"
                   "--disable-system-pcsc"
                   "--disable-system-sctp"
                   "--disable-tests"  ;they are run in the check phase instead
                   "--with-openjdk-src-dir=./openjdk.src"
                   ,(string-append "--with-jdk-home=" jdk))))
             ((#:phases phases)
              `(modify-phases ,phases
                 (delete 'fix-x11-extension-include-path)
                 (delete 'patch-paths)
                 (delete 'set-additional-paths)
                 (delete 'patch-patches)
                 (delete 'patch-bitrot)
                 (delete 'use-classpath)
                 ;; Prevent passing -j (parallel-job-count) to make
                 (replace 'build
                   (lambda* (#:key (make-flags '()) #:allow-other-keys)
                     (apply invoke "make" make-flags)))
                 ;; Prevent the keytool from recording the current time when
                 ;; adding certificates at build time.
                 (add-after 'unpack 'patch-keystore
                   (lambda _
                     (substitute* "openjdk.src/jdk/src/share/classes/sun/security/provider/JavaKeyStore.java"
                       (("date = new Date\\(\\);")
                        "\
date = (System.getenv(\"SOURCE_DATE_EPOCH\") != null) ?\
new Date(Long.parseLong(System.getenv(\"SOURCE_DATE_EPOCH\"))) :\
new Date();"))
                     #t))
                 (add-after 'unpack 'patch-jni-libs
                   ;; Hardcode dynamically loaded libraries.
                   (lambda _
                     (define remove
                       (@ (srfi srfi-1) remove))

                     (define (icedtea-or-openjdk? path)
                       (or (string-contains path "openjdk")
                           (string-contains path "icedtea")))

                     (let* ((library-path (remove icedtea-or-openjdk?
                                                  (search-path-as-string->list
                                                   (getenv "LIBRARY_PATH"))))
                            (find-library (lambda (name)
                                            (or (search-path
                                                 library-path
                                                 (string-append "lib" name ".so"))
                                                (string-append "lib" name ".so")))))
                       (for-each
                        (lambda (file)
                          (catch 'decoding-error
                            (lambda ()
                              (substitute* file
                                (("VERSIONED_JNI_LIB_NAME\\(\"(.*)\", \"(.*)\"\\)"
                                  _ name version)
                                 (string-append "\"" (find-library name) "\""))
                                (("JNI_LIB_NAME\\(\"(.*)\"\\)" _ name)
                                 (string-append "\"" (find-library name) "\""))))
                            (lambda _
                              ;; Those are safe to skip.
                              (format (current-error-port)
                                      "warning: failed to substitute: ~a~%"
                                      file))))
                        (find-files "openjdk.src/jdk/src/solaris/native"
                                    "\\.c|\\.h")))))
                 (replace 'fix-openjdk
                   (lambda _
                     (substitute*
                         '("openjdk.src/jdk/src/solaris/native/java/net/PlainSocketImpl.c"
                           "openjdk.src/jdk/src/solaris/native/java/net/PlainDatagramSocketImpl.c")
                       (("#include <sys/sysctl.h>")
                        "#include <linux/sysctl.h>"))))
                 (replace 'install
                   (lambda* (#:key outputs #:allow-other-keys)
                     (let ((doc (string-append (assoc-ref outputs "doc")
                                               "/share/doc/icedtea"))
                           (jre (assoc-ref outputs "out"))
                           (jdk (assoc-ref outputs "jdk")))
                       (copy-recursively "openjdk.build/docs" doc)
                       (copy-recursively "openjdk.build/images/j2re-image" jre)
                       (copy-recursively "openjdk.build/images/j2sdk-image" jdk)
                       ;; Install the nss.cfg file to JRE to enable SSL/TLS
                       ;; support via NSS.
                       (copy-file (string-append jdk "/jre/lib/security/nss.cfg")
                                  (string-append jre "/lib/security/nss.cfg")))))
                 (add-after 'install 'strip-jar-timestamps
                   (assoc-ref ant:%standard-phases 'strip-jar-timestamps)))))))
      (native-inputs
       `(("jdk" ,icedtea-7 "jdk")
         ("openjdk-src"
          ,(drop "openjdk"
                 "1l3bzmd3s38scxpwamfhnwbv7vndgjq6hz3bl58437fgl9kgbl69"))
         ("aarch32-drop"
          ,(drop "aarch32"
                 "0k4dwpi3x3lj41rj32xyxbn76r7cb2g2whh44r1z4iwhw1xd2lpq"))
         ("corba-drop"
          ,(drop "corba"
                 "0xhh6gf5gh5c6vf1607xcy49wnp5prch2rim13x14wvsn817xf0r"))
         ("jaxp-drop"
          ,(drop "jaxp"
                 "043g335rgi5ipl8dp3q2cc3gcfhxk77ipxs43sv344z71bn8xmxr"))
         ("jaxws-drop"
          ,(drop "jaxws"
                 "1pc0pv4v2mn2mjc0vp19d94v2150xigyhxsmckqasy647zcm6w0r"))
         ("jdk-drop"
          ,(drop "jdk"
                 "1742lcm55l8zhi522x83v65ccr0rd6511q9rj7crw44x3ymdrhrv"
                 (search-patches "jdk-currency-time-bomb2.patch")))
         ("langtools-drop"
          ,(drop "langtools"
                 "08iz7p2xcddlphipf6gahyabr5cawlnydap12p1n4f0md069b50b"))
         ("hotspot-drop"
          ,(drop "hotspot"
                 "1ffaxfnb3yn1i7crivqigc1r1q0z6cp044i6nfring4z6c8pfhd2"))
         ("nashorn-drop"
          ,(drop "nashorn"
                 "15fn7cpm2i1npa88h57njxg0f8qkrqhrc30pb54d3hxlx5zyjl94"))
         ("shenandoah-drop"
          ,(drop "shenandoah"
                 "1jjzjjx1ykyhbc4llh8249dlr8j5g1ki6r7g9baj2mxyb9whc5nq"))
         ,@(fold alist-delete (package-native-inputs icedtea-7)
                 '("openjdk-src" "corba-drop" "jaxp-drop" "jaxws-drop"
                   "jdk-drop" "langtools-drop" "hotspot-drop"
                   "classpath" "ecj4-javac-wrapper" "jamvm" "fastjar")))))))

(define-public icedtea icedtea-8)

(define-public openjdk9
  (package
    (name "openjdk")
    (version "9.181")
    (source (origin
              (method hg-fetch)
              (uri (hg-reference (url "https://hg.openjdk.org/jdk/jdk")
                                 (changeset "jdk-9+181")))
              (file-name (hg-file-name name version))
              (modules '((guix build utils)))
              (snippet '(for-each delete-file
                                  (find-files "." ".*.(bin|exe|jar)$")))
              (sha256
               (base32
                "1v92nzdqx07c35x945awzir4yk0fk22vky6fpp8mq9js930sxsz0"))
              (patches
               (search-patches "openjdk-9-pointer-comparison.patch"
                               "openjdk-9-classlist-reproducibility.patch"
                               "openjdk-currency-time-bomb.patch"
                               "openjdk-9-jar-reproducibility.patch"
                               "openjdk-9-module-reproducibility.patch"
                               "openjdk-9-module2-reproducibility.patch"
                               "openjdk-9-module3-reproducibility.patch"
                               "openjdk-9-idlj-reproducibility.patch"
                               "openjdk-9-setsignalhandler.patch"))))
    (build-system gnu-build-system)
    (outputs '("out" "jdk" "doc"))
    (arguments
     `(#:imported-modules
       ((guix build ant-build-system)
        ,@%default-gnu-imported-modules)
       #:modules
       ((guix build utils)
        (guix build gnu-build-system)
        (ice-9 popen))
       #:tests? #f                      ; require jtreg
       #:make-flags '("all")
       #:disallowed-references ,(list (gexp-input icedtea-8)
                                      (gexp-input icedtea-8 "jdk"))

       #:phases
       (modify-phases %standard-phases
         ,@(if (target-aarch64?)
               `((add-after 'unpack 'patch-for-aarch64
                   (lambda _
                     (substitute* "hotspot/src/cpu/aarch64/vm/interp_masm_aarch64.hpp"
                       ;; This line is duplicated, so remove both occurrences,
                       ;; then add back one occurrence by substituting a
                       ;; comment that occurs once.
                       (("using MacroAssembler::call_VM_leaf_base;") "")
                       (("Interpreter specific version of call_VM_base")
                        "Interpreter specific version of call_VM_base
  using MacroAssembler::call_VM_leaf_base;")))))
               '())
         (add-after 'patch-source-shebangs 'fix-java-shebangs
           (lambda _
             ;; This file was "fixed" by patch-source-shebangs, but it requires
             ;; this exact first line.
             (substitute* "jdk/make/data/blacklistedcertsconverter/blacklisted.certs.pem"
               (("^#!.*") "#! java BlacklistedCertsConverter SHA-256\n"))))
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             ;; TODO: unbundle libpng and lcms
             (invoke "bash" "./configure"
                     ;; Add flags for compilation with gcc >= 10
                     ,(string-append "--with-extra-cflags=-fcommon"
                                     " -fno-delete-null-pointer-checks"
                                     " -fno-lifetime-dse"
                                     ;; flags for compilation with gcc >= 14.
                                     " -Wno-error=int-conversion")
                     (string-append "--with-freetype="
                                    (assoc-ref inputs "freetype"))
                     "--disable-freetype-bundling"
                     "--disable-warnings-as-errors"
                     "--disable-hotspot-gtest"
                     "--with-giflib=system"
                     "--with-libjpeg=system"
                     (string-append "--prefix=" (assoc-ref outputs "out")))))
         (add-before 'build 'write-source-revision-file
           (lambda _
             (with-output-to-file ".src-rev"
               (lambda _
                 (display ,version)))))
         (replace 'build
           (lambda* (#:key make-flags parallel-build? #:allow-other-keys)
             (apply invoke "make"
                    `(,@(if parallel-build?
                            (list (string-append "JOBS="
                                                 (number->string (parallel-job-count))))
                            '())
                      ,@make-flags))))
         (add-after 'unpack 'patch-jni-libs
           ;; Hardcode dynamically loaded libraries.
           (lambda _
             (define remove
               (@ (srfi srfi-1) remove))

             (define (icedtea-or-openjdk? path)
               (or (string-contains path "openjdk")
                   (string-contains path "icedtea")))

             (let* ((library-path (remove icedtea-or-openjdk?
                                          (search-path-as-string->list
                                           (getenv "LIBRARY_PATH"))))
                    (find-library (lambda (name)
                                    (or (search-path
                                         library-path
                                         (string-append "lib" name ".so"))
                                        (string-append "lib" name ".so")))))
               (for-each
                (lambda (file)
                  (catch 'decoding-error
                    (lambda ()
                      (substitute* file
                        (("VERSIONED_JNI_LIB_NAME\\(\"(.*)\", \"(.*)\"\\)"
                          _ name version)
                         (string-append "\"" (find-library name) "\""))
                        (("JNI_LIB_NAME\\(\"(.*)\"\\)" _ name)
                         (string-append "\"" (find-library name) "\""))))
                    (lambda _
                      ;; Those are safe to skip.
                      (format (current-error-port)
                              "warning: failed to substitute: ~a~%"
                              file))))
                (find-files "."
                            "\\.c$|\\.h$")))))
         ;; By default OpenJDK only generates an empty keystore.  In order to
         ;; be able to use certificates in Java programs we need to generate a
         ;; keystore from a set of certificates.  For convenience we use the
         ;; certificates from the nss-certs package.
         (add-after 'install 'install-keystore
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (use-modules (ice-9 rdelim))
             (let* ((keystore  "cacerts")
                    (certs-dir (search-input-directory inputs
                                                       "etc/ssl/certs"))
                    (keytool   (string-append (assoc-ref outputs "jdk")
                                              "/bin/keytool")))
               (define (extract-cert file target)
                 (call-with-input-file file
                   (lambda (in)
                     (call-with-output-file target
                       (lambda (out)
                         (let loop ((line (read-line in 'concat))
                                    (copying? #f))
                           (cond
                            ((eof-object? line) #t)
                            ((string-prefix? "-----BEGIN" line)
                             (display line out)
                             (loop (read-line in 'concat) #t))
                            ((string-prefix? "-----END" line)
                             (display line out)
                             #t)
                            (else
                             (when copying? (display line out))
                             (loop (read-line in 'concat) copying?)))))))))
               (define (import-cert cert)
                 (format #t "Importing certificate ~a\n" (basename cert))
                 (let ((temp "tmpcert"))
                   (extract-cert cert temp)
                   (let ((port (open-pipe* OPEN_WRITE keytool
                                           "-import"
                                           "-alias" (basename cert)
                                           "-keystore" keystore
                                           "-storepass" "changeit"
                                           "-file" temp)))
                     (display "yes\n" port)
                     (when (not (zero? (status:exit-val (close-pipe port))))
                       (format #t "failed to import ~a\n" cert)))
                   (delete-file temp)))

               ;; This is necessary because the certificate directory contains
               ;; files with non-ASCII characters in their names.
               (setlocale LC_ALL "C.UTF-8")
               (setenv "LC_ALL" "C.UTF-8")

               (copy-file (string-append (assoc-ref outputs "out")
                                         "/lib/security/cacerts")
                          keystore)
               (chmod keystore #o644)
               (for-each import-cert (find-files certs-dir "\\.pem$"))
               (mkdir-p (string-append (assoc-ref outputs "out")
                                       "/lib/security"))
               (mkdir-p (string-append (assoc-ref outputs "jdk")
                                       "/lib/security"))

               ;; The cacerts files we are going to overwrite are chmod'ed as
               ;; read-only (444) in icedtea-8 (which derives from this
               ;; package).  We have to change this so we can overwrite them.
               (chmod (string-append (assoc-ref outputs "out")
                                     "/lib/security/" keystore) #o644)
               (chmod (string-append (assoc-ref outputs "jdk")
                                     "/lib/security/" keystore) #o644)

               (install-file keystore
                             (string-append (assoc-ref outputs "out")
                                            "/lib/security"))
               (install-file keystore
                             (string-append (assoc-ref outputs "jdk")
                                            "/lib/security")))))
         ;; Some of the libraries in the lib/ folder link to libjvm.so.
         ;; But that shared object is located in the server/ folder, so it
         ;; cannot be found.  This phase creates a symbolic link in the
         ;; lib/ folder so that the other libraries can find it.
         ;;
         ;; See:
         ;; https://lists.gnu.org/archive/html/guix-devel/2017-10/msg00169.html
         ;;
         ;; FIXME: Find the bug in the build system, so that this symlink is
         ;; not needed.
         (add-after 'install 'install-libjvm
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((lib-out (string-append (assoc-ref outputs "out")
                                            "/lib"))
                    (lib-jdk (string-append (assoc-ref outputs "jdk")
                                            "/lib")))
               (symlink (string-append lib-jdk "/server/libjvm.so")
                        (string-append lib-jdk "/libjvm.so"))
               (symlink (string-append lib-out "/server/libjvm.so")
                        (string-append lib-out "/libjvm.so")))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (jdk (assoc-ref outputs "jdk"))
                   (doc (assoc-ref outputs "doc"))
                   (images (car (find-files "build" ".*-server-release"
                                            #:directories? #t))))
               (copy-recursively (string-append images "/images/jdk") jdk)
               (copy-recursively (string-append images "/images/jre") out)
               (copy-recursively (string-append images "/images/docs") doc))))
         (add-after 'install 'strip-zip-timestamps
           (lambda* (#:key outputs #:allow-other-keys)
             (for-each
              (lambda (zip)
                (let ((dir (mkdtemp "zip-contents.XXXXXX")))
                  (with-directory-excursion dir
                    ;; This is an exact copy of the implementation of invoke,
                    ;; but this accepts exit code 1 as OK.
                    (let ((code (system* "unzip" "--" zip)))
                      ;; jmod files are zip files with an extra header in
                      ;; front.  unzip will warn about that -- but otherwise
                      ;; work.
                      (when (> (status:exit-val code) 1) ; 1 is just a warning
                        (raise
                         (condition
                          (&invoke-error
                           (program "unzip")
                           (arguments (list "--" zip))
                           (exit-status (status:exit-val code))
                           (term-signal (status:term-sig code))
                           (stop-signal (status:stop-sig code))))))))
                  (delete-file zip)
                  (for-each (lambda (file)
                              (let ((s (lstat file)))
                                (format #t "reset ~a~%" file)
                                (utime file 1 1 0 0
                                       AT_SYMLINK_NOFOLLOW)))
                            (find-files dir #:directories? #t))
                  (with-directory-excursion dir
                    (let ((files (cons "./META-INF/MANIFEST.MF"
                                       (append
                                        (find-files "./META-INF" ".*")
                                        ;; for jmod:
                                        (list "./classes/module-info.class")
                                        (find-files "." ".*")))))
                      (apply invoke "zip" "--symlinks" "-0" "-X" zip files)
                      (when (string-suffix? ".jmod" zip)
                        (let ((new-zip (string-append zip "n"))
                              (contents (call-with-input-file zip
                                          (@ (ice-9 binary-ports)
                                             get-bytevector-all))))
                          (call-with-output-file new-zip
                            (lambda (output-port)
                              ((@ (ice-9 binary-ports) put-bytevector)
                               output-port
                               #vu8(#x4a #x4d #x01 #x00)) ; JM
                              ((@ (ice-9 binary-ports) put-bytevector)
                               output-port
                               contents)))
                          (rename-file new-zip zip)))))))
              (append (find-files (string-append
                                   (assoc-ref outputs "doc")
                                   "/api")
                                  "\\.zip$")
                      (find-files (assoc-ref outputs "doc") "src\\.zip$")
                      (find-files (assoc-ref outputs "jdk") "src\\.zip$")
                      (find-files (assoc-ref outputs "jdk") "\\.jmod$")
                      (find-files (assoc-ref outputs "jdk") "\\.diz$")
                      (find-files (assoc-ref outputs "out") "\\.diz$")

                      (list (string-append (assoc-ref outputs "jdk")
                                           "/lib/jrt-fs.jar"))
                      (find-files (string-append (assoc-ref outputs "jdk")
                                                 "/demo")
                                  "\\.jar$"))))))))
    (inputs
     (list alsa-lib
           cups
           fontconfig
           freetype
           giflib
           lcms
           elfutils
           libjpeg-turbo
           libice
           libpng
           libx11
           libxcomposite
           libxi
           libxinerama
           libxrender
           libxt
           libxtst))
    (native-inputs
     (list icedtea-8
           `(,icedtea-8 "jdk")
           ;; XXX: The build system fails with newer versions of GNU Make.
           gnu-make-4.2
           nss-certs
           unzip
           which
           zip))
    (home-page "https://openjdk.org/projects/jdk9/")
    (synopsis "Java development kit")
    (description
     "This package provides the Java development kit OpenJDK.")
    (license license:gpl2+)))

(define-public openjdk10
  (package
    (inherit openjdk9)
    (name "openjdk")
    (version "10.46")
    (source (origin
              (method hg-fetch)
              (uri (hg-reference (url "https://hg.openjdk.org/jdk/jdk")
                                 (changeset "jdk-10+46")))
              (file-name (hg-file-name name version))
              (modules '((guix build utils)))
              (snippet `(begin
                          (for-each delete-file
                                    (find-files "." ".*.(bin|exe|jar)$"))))
              (sha256
               (base32
                "0i47ar8lxzjrkkiwbzybfxs473390h4jq9ahm3xqdvy5zpchxy3y"))
              (patches (search-patches
                        "openjdk-10-char-reproducibility.patch"
                        "openjdk-10-classlist-reproducibility.patch"
                        "openjdk-10-corba-reproducibility.patch"
                        "openjdk-10-idlj-reproducibility.patch"
                        "openjdk-10-module-reproducibility.patch"
                        "openjdk-10-module3-reproducibility.patch"
                        "openjdk-10-module4-reproducibility.patch"
                        "openjdk-10-jar-reproducibility.patch"
                        "openjdk-10-jtask-reproducibility.patch"
                        "openjdk-10-pointer-comparison.patch"
                        "openjdk-10-setsignalhandler.patch"
                        "openjdk-currency-time-bomb2.patch"))))
    (arguments
     (substitute-keyword-arguments (package-arguments openjdk9)
       ((#:phases phases)
        `(modify-phases ,phases
         ,@(if (target-aarch64?)
               `((replace 'patch-for-aarch64
                   (lambda _
                     (substitute* "src/hotspot/cpu/aarch64/interp_masm_aarch64.hpp"
                       ;; This line is duplicated, so remove both occurrences,
                       ;; then add back one occurrence by substituting a
                       ;; comment that occurs once.
                       (("using MacroAssembler::call_VM_leaf_base;") "")
                       (("Interpreter specific version of call_VM_base")
                        (string-append "Interpreter specific version of call_VM_base\n"
                                       "  using MacroAssembler::call_VM_leaf_base;"))))))
               '())
           (replace 'fix-java-shebangs
             (lambda _
               ;; This file was "fixed" by patch-source-shebangs, but it requires
               ;; this exact first line.
               (substitute* "make/data/blacklistedcertsconverter/blacklisted.certs.pem"
                 (("^#!.*") "#! java BlacklistedCertsConverter SHA-256\n"))))
           (add-after 'unpack 'remove-timestamping
             (lambda _
               (substitute* "./src/hotspot/share/runtime/vm_version.cpp"
                 (("__DATE__") "")
                 (("__TIME__") ""))))
           (replace 'configure
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (invoke "bash" "./configure"
                       ;; Add flags for compilation with gcc >= 10
                       ,(string-append "--with-extra-cflags=-fcommon"
                                       " -fno-delete-null-pointer-checks"
                                       " -fno-lifetime-dse"
                                       ;; flags for compilation with gcc >= 14.
                                       " -Wno-error=int-conversion")
                       (string-append "--with-freetype="
                                      (assoc-ref inputs "freetype"))
                       "--disable-freetype-bundling"
                       "--disable-warnings-as-errors"
                       "--disable-hotspot-gtest"
                       "--with-giflib=system"
                       "--with-libjpeg=system"
                       "--with-native-debug-symbols=zipped"
                       (string-append "--prefix=" (assoc-ref outputs "out")))))
           (add-after 'unpack 'disable-warnings-as-errors
             (lambda _
               ;; It looks like the "--disable-warnings-as-errors" option of
               ;; the 'configure' phase is not working.
               (substitute* "make/autoconf/generated-configure.sh"
                 (("-Werror") ""))))))
       ((#:disallowed-references _ '())
        `(,(gexp-input openjdk9)
          ,(gexp-input openjdk9 "jdk")))))
    (native-inputs
     `(("openjdk9" ,openjdk9)
       ("openjdk9:jdk" ,openjdk9 "jdk")
       ("make@4.2" ,gnu-make-4.2)
       ("nss-certs" ,nss-certs)
       ("unzip" ,unzip)
       ("which" ,which)
       ("zip" ,zip)))))

(define-public openjdk11
  (package
    (name "openjdk")
    (version "11.0.22")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://openjdk-sources.osci.io/openjdk11/openjdk-"
                                  version "-ga.tar.xz"))
              (file-name (string-append name "-" version ".tar.xz"))
              (sha256
               (base32
                "18ca4syp9xlrqjgyjkb1sp9835riy6aym5xs81r8byrz6jlb2473"))
              (modules '((guix build utils)))
              (snippet
               '(for-each delete-file (find-files "." "\\.(bin|exe|jar)$")))
              (patches
               (search-patches "openjdk-10-module3-reproducibility.patch"
                               "openjdk-10-module4-reproducibility.patch"
                               "openjdk-10-char-reproducibility.patch"
                               "openjdk-11-classlist-reproducibility.patch"
                               "openjdk-10-jar-reproducibility.patch"
                               "openjdk-10-jtask-reproducibility.patch"
                               "openjdk-currency-time-bomb2.patch"))))
    (build-system gnu-build-system)
    (outputs '("out" "jdk" "doc"))
    (arguments
     (list
      #:modules `((guix build gnu-build-system)
                  (guix build utils)
                  (ice-9 match)
                  (ice-9 popen)
                  (srfi srfi-1)
                  (srfi srfi-26))
      #:disallowed-references (list (gexp-input openjdk10)
                                    (gexp-input openjdk10 "jdk"))

      #:tests? #f                       ; requires jtreg
      ;; TODO package jtreg
      #:configure-flags
      #~(list
         ;; Add flags for compilation with gcc >= 10.
         #$(string-append "--with-extra-cflags=-fcommon"
                          " -fno-delete-null-pointer-checks"
                          " -fno-lifetime-dse"
                          ;; flags for compilation with gcc >= 14.
                          " -Wno-error=int-conversion")
         ;; Otherwise, the '--enable-fast-install' causes an error.
         "--disable-option-checking"
         "--disable-warnings-as-errors"
         ;; Make validate-runpath pass (see:
         ;; http://issues.guix.info/issue/32894).
         "--with-native-debug-symbols=zipped"
         ;; Do not use the bundled libraries.
         "--with-giflib=system"
         "--with-lcms=system"
         "--with-libjpeg=system"
         "--with-libpng=system"
         "--with-version-pre="
         ;; Should be set by SOURCE_DATE_EPOCH handler, but isn't being
         ;; set; do it manually.
         "--with-hotspot-build-time=1970-01-01T00:00:01"
         "--enable-reproducible-build"  ; to be sure
         ;; Allow the build system to locate the system freetype.
         (string-append "--with-freetype-include="
                        #$(this-package-input "freetype") "/include")
         (string-append "--with-freetype-lib="
                        #$(this-package-input "freetype") "/lib"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'patch-source-shebangs 'fix-java-shebangs
            (lambda _
              ;; This file was "fixed" by patch-source-shebangs, but it requires
              ;; this exact first line.
              (substitute* "make/data/blockedcertsconverter/blocked.certs.pem"
                (("^#!.*") "#! java BlockedCertsConverter SHA-256\n"))))
          (add-after 'unpack 'remove-timestamping
            (lambda _
              (substitute* "src/hotspot/share/runtime/abstract_vm_version.cpp"
                (("__DATE__") "")
                (("__TIME__") ""))))
          (add-after 'unpack 'patch-jni-libs
            ;; Hardcode dynamically loaded libraries.
            (lambda _
              (define remove
                (@ (srfi srfi-1) remove))

              (define (icedtea-or-openjdk? path)
                (or (string-contains path "openjdk")
                    (string-contains path "icedtea")))

              (let* ((library-path (remove icedtea-or-openjdk?
                                           (search-path-as-string->list
                                            (getenv "LIBRARY_PATH"))))
                     (find-library (lambda (name)
                                     (or (search-path
                                          library-path
                                          (string-append "lib" name ".so"))
                                         (string-append "lib" name ".so")))))
                (for-each
                 (lambda (file)
                   (catch 'decoding-error
                     (lambda ()
                       (substitute* file
                         (("VERSIONED_JNI_LIB_NAME\\(\"([^\"]*)\", \"([^\"]*)\"\\)"
                           _ name version)
                          (string-append "\"" (find-library name) "\""))
                         (("JNI_LIB_NAME\\(\"([^\"]*)\"\\)" _ name)
                          (string-append "\"" (find-library name) "\""))))
                     (lambda _
                       ;; Those are safe to skip.
                       (format (current-error-port)
                               "warning: failed to substitute: ~a~%"
                               file))))
                 (find-files "." "\\.c$|\\.h$")))))
          (add-before 'build 'write-source-revision-file
            (lambda _
              (with-output-to-file ".src-rev"
                (lambda _
                  (display #$version)))))
          (replace 'build
            (lambda* (#:key parallel-build? make-flags #:allow-other-keys)
              (apply invoke "make" "all"
                     `(,@(if parallel-build?
                             (list (string-append "JOBS="
                                                  (number->string (parallel-job-count))))
                             '())
                       ,@make-flags))))
          ;; jdk 11 does not build jre by default any more; so explicitly build
          ;; it (see:
          ;; https://github.com/AdoptOpenJDK/openjdk-build/issues/356).
          (add-after 'build 'build-jre
            (lambda* (#:key parallel-build? make-flags #:allow-other-keys)
              (apply invoke "make" "legacy-jre-image"
                     `(,@(if parallel-build?
                             (list (string-append "JOBS="
                                                  (number->string (parallel-job-count))))
                             '())
                       ,@make-flags))))
          (replace 'install
            (lambda _
              (let ((images (car (find-files "build" "-server-release"
                                             #:directories? #t))))
                (copy-recursively (string-append images "/images/jdk")
                                  #$output:jdk)
                (copy-recursively (string-append images "/images/jre")
                                  #$output)
                (copy-recursively (string-append images "/images/docs")
                                  #$output:doc))))
          ;; Some of the libraries in the lib/ folder link to libjvm.so.
          ;; But that shared object is located in the server/ folder, so it
          ;; cannot be found.  This phase creates a symbolic link in the
          ;; lib/ folder so that the other libraries can find it.
          ;;
          ;; See:
          ;; https://lists.gnu.org/archive/html/guix-devel/2017-10/msg00169.html
          ;;
          ;; FIXME: Find the bug in the build system, so that this symlink is
          ;; not needed.
          (add-after 'install 'install-libjvm
            (lambda _
              (let ((lib-out (string-append #$output "/lib"))
                    (lib-jdk (string-append #$output:jdk "/lib")))
                (symlink (string-append lib-jdk "/server/libjvm.so")
                         (string-append lib-jdk "/libjvm.so"))
                (symlink (string-append lib-out "/server/libjvm.so")
                         (string-append lib-out "/libjvm.so")))))
          (add-after 'install 'strip-character-data-timestamps
            (lambda _
              (let ((archive (string-append #$output:jdk "/lib/src.zip"))
                    (dir (mkdtemp "zip-contents.XXXXXX")))
                (with-directory-excursion dir
                  (invoke "unzip" archive))
                (delete-file archive)
                (with-directory-excursion dir
                  (let ((char-data-files (find-files "." "CharacterData")))
                    (for-each (lambda (file)
                                (substitute* file
                                  (((string-append "This file was generated "
                                                   "AUTOMATICALLY from a template "
                                                   "file.*"))
                                   (string-append "This file was generated "
                                                  "AUTOMATICALLY from a template "
                                                  "file"))))
                              char-data-files)))
                (with-directory-excursion dir
                  (let ((files (find-files "." #:directories? #t)))
                    (apply invoke "zip" "-0" "-X" archive files))))))
          (add-after 'strip-character-data-timestamps 'remove-extraneous-files
            (lambda* (#:key outputs #:allow-other-keys)
              ;; Remove the *.diz and src.zip files for space considerations.
              ;; The former are compressed debuginfo files not typically
              ;; shipped with Java distributions, while the later corresponds
              ;; to Java core API source files.
              (for-each delete-file
                        (append-map (cut find-files <> "(^src\\.zip|\\.diz)$")
                                    (map (match-lambda
                                           ((name . dir)
                                            dir))
                                         outputs)))))
          (add-after 'remove-extraneous-files 'strip-archive-timestamps
            (lambda _
              (use-modules (ice-9 binary-ports)
                           (rnrs bytevectors))
              (letrec ((repack-archive
                        (lambda (archive)
                          (let ((dir (mkdtemp "zip-contents.XXXXXX")))
                            (with-directory-excursion dir
                              (invoke "unzip" archive))
                            (delete-file archive)
                            (for-each (compose repack-archive canonicalize-path)
                                      (find-files dir "(ct\\.sym|\\.jar)$"))
                            (let ((reset-file-timestamp
                                   (lambda (file)
                                     (let ((s (lstat file)))
                                       (unless (eq? (stat:type s) 'symlink)
                                         (format #t "reset ~a~%" file)
                                         (utime file 0 0 0 0))))))
                              (for-each reset-file-timestamp
                                        (find-files dir #:directories? #t)))
                            (with-directory-excursion dir
                              (let ((files (find-files "." #:directories? #t)))
                                (apply invoke "zip" "-0" "-X" archive files)))))))
                (for-each repack-archive
                          (find-files #$output:doc "\\.zip$"))
                (for-each repack-archive
                          (find-files #$output:jdk "\\.(zip|jar)$"))
                (repack-archive (string-append #$output:jdk "/lib/ct.sym"))
                (let ((repack-jmod
                       (lambda (file-name)
                         (call-with-input-file file-name
                           (lambda (file)
                             (let ((header #vu8(#x4a #x4d #x01 #x00)))
                               (if (equal? (get-bytevector-n
                                            file (bytevector-length header))
                                           header)
                                   (let* ((header-length (bytevector-length header))
                                          (temp-file (mkstemp!
                                                      (string-copy
                                                       "temp-file.XXXXXX")))
                                          (temp-filename (port-filename temp-file))
                                          (content-length
                                           (- (stat:size (stat file))
                                              header-length)))
                                     (sendfile temp-file file content-length header-length)
                                     (delete-file file-name)
                                     (close-port temp-file)
                                     (repack-archive (canonicalize-path temp-filename))
                                     (call-with-output-file file-name
                                       (lambda (file)
                                         (put-bytevector file header)
                                         (call-with-input-file temp-filename
                                           (lambda (temp-file)
                                             (sendfile
                                              file temp-file
                                              (stat:size (stat temp-file)) 0)))))))))))))
                  (for-each repack-jmod
                            (find-files #$output:jdk "\\.jmod$"))))))
          (add-after 'install 'remove-timestamp-from-api-summary
            (lambda _
              (substitute* (string-append #$output:doc
                                          "/api/overview-summary.html")
                (("Generated by javadoc \\(11-internal\\).*$")
                 "Generated by javadoc (11-internal) -->\n")))))))
    (inputs
     (list alsa-lib
           cups
           fontconfig
           freetype
           giflib
           lcms
           libjpeg-turbo
           libpng
           libx11
           libxext
           libxrandr
           libxrender
           libxt
           libxtst))
    (native-inputs
     (list autoconf
           bash                         ; not bash-minimal, needs ulimit
           openjdk10
           `(,openjdk10 "jdk")
           gnu-make-4.2
           nss-certs
           pkg-config
           unzip
           which
           zip))
    (home-page "https://openjdk.org/projects/jdk/11/")
    (synopsis "Java development kit")
    (description
     "This package provides the Java development kit OpenJDK.")
    (license license:gpl2+)))

(define-syntax make-openjdk
  ;; Return an OpenJDK package at VERSION with checksum HASH, using BOOTSTRAP,
  ;; the bootstrap package.  One or more FIELD can be provided to further
  ;; refine the package definition; for convenience, the BASE, NAME and
  ;; VERSION are defined in their scope.
  (lambda (x)
    (syntax-case x ()
      ((_ bootstrap version* hash field ...)
       (with-syntax ((base (datum->syntax x 'base))
                     (name (datum->syntax x 'name))
                     (version (datum->syntax x 'version)))
         #'(let ((base (package
                         (inherit bootstrap)
                         (name "openjdk")
                         (version version*)
                         (source
                          (origin
                            (inherit (package-source bootstrap))
                            (method git-fetch)
                            (uri (git-reference
                                  (url (format
                                        #f "https://github.com/openjdk/jdk~au"
                                        (version-major version*)))
                                  (commit (string-append "jdk-" version*
                                                         "-ga"))))
                            (file-name (git-file-name name version))
                            (sha256 (base32 hash))))
                         (native-inputs
                          (modify-inputs (package-native-inputs bootstrap)
                            (replace "openjdk" bootstrap)))
                         (home-page (string-append
                                     "https://openjdk.java.net/projects/jdk/"
                                     (version-major version)))))
                 (name "openjdk")
                 (version version*))
             (package
               (inherit base)
               field
               ...)))))))

(define-public openjdk12
  (make-openjdk
   openjdk11 "12.33" "0pi2gwib3j2imi4l623iaywrmvfh9rqzh82lj2gxqbrmg55swvjf"
   (source
    (origin
      (method url-fetch)
      (uri "http://hg.openjdk.java.net/jdk/jdk/archive/0276cba45aac.tar.bz2")
      (file-name (string-append name "-" version ".tar.bz2"))
      (sha256
       (base32
        "0pi2gwib3j2imi4l623iaywrmvfh9rqzh82lj2gxqbrmg55swvjf"))
      (modules '((guix build utils)))
      (snippet
       '(for-each delete-file (find-files "." "\\.(bin|exe|jar)$")))
      (patches (search-patches "openjdk-10-setsignalhandler.patch"))))
   (arguments
    (substitute-keyword-arguments (package-arguments openjdk11)
      ((#:phases phases)
       #~(modify-phases #$phases
           #$@(if (target-aarch64?)
                #~((add-after 'unpack 'patch-for-aarch64
                    (lambda _
                      (substitute* "src/hotspot/cpu/aarch64/interp_masm_aarch64.hpp"
                        ;; This line is duplicated, so remove both occurrences,
                        ;; then add back one occurrence by substituting a
                        ;; comment that occurs once.
                        (("using MacroAssembler::call_VM_leaf_base;") "")
                        (("Interpreter specific version of call_VM_base")
                         (string-append "Interpreter specific version of call_VM_base\n"
                                        "  using MacroAssembler::call_VM_leaf_base;"))))))
                #~())
           (replace 'remove-timestamping
             (lambda _
               (substitute* "src/hotspot/share/runtime/vm_version.cpp"
                (("__DATE__") "")
                (("__TIME__") ""))))
           (replace 'fix-java-shebangs
             (lambda _
               ;; 'blocked' was renamed to 'blacklisted' in this version for
               ;; some reason.
               (substitute* "make/data/blacklistedcertsconverter/\
blacklisted.certs.pem"
                 (("^#!.*")
                  "#! java BlacklistedCertsConverter SHA-256\n"))))))))))

(define-public openjdk13
  (make-openjdk openjdk12 "13.0.14"
                "1v92i5rhahqkjw8mz09c9qasnxqc67ygy0y266kdmm534z0da755"
   (source (origin
             (inherit (package-source base))
             (patches (search-patches "openjdk-13-classlist-reproducibility.patch"
                                      "openjdk-10-jtask-reproducibility.patch"))))
   (arguments
    (substitute-keyword-arguments (package-arguments openjdk12)
      ((#:phases phases)
       #~(modify-phases #$phases
           (replace 'remove-timestamping
             (lambda _
               (substitute*
                "src/hotspot/share/runtime/abstract_vm_version.cpp"
                (("__DATE__") "")
                (("__TIME__") ""))))))))))

(define-public openjdk14
  (make-openjdk
   openjdk13 "14.0.2"
   "07k9bsbxwyf2z2n50z96nvhsdai916mxdxcr5lm44jz7f6xrwfq6"
   (source (origin
             (inherit (package-source base))
             (snippet                   ;override snippet
              '(begin
                 ;; The m4 macro uses 'help' to search for builtins, which is
                 ;; not available in bash-minimal
                 (substitute* "make/autoconf/basics.m4"
                   (("if help") "if command -v"))
                 (for-each delete-file (find-files "." "\\.(bin|exe|jar)$"))))
             (patches (search-patches "openjdk-10-setsignalhandler.patch"
                                      "openjdk-10-jtask-reproducibility.patch"
                                      "openjdk-13-classlist-reproducibility.patch"))))))

(define-public openjdk15
  (make-openjdk
   openjdk14 "15.0.10"
   "0hdllv348bws6m992bh73jik18x0sv0k2m9l817b3zb7q802sp7x"
   (source (origin
             (inherit (package-source base))
             (modules '())
             (snippet #f)
             (patches
              (search-patches "openjdk-15-jtask-reproducibility.patch"
                              "openjdk-15-xcursor-no-dynamic.patch"))))
   (inputs
    (modify-inputs (package-inputs base)
      (append libxcursor)))             ;for our patch to work
   (native-inputs
    (modify-inputs (package-native-inputs base)
      (delete "make"                    ;remove old gnu-make-4.2
              "openjdk")                ;to remove non-jdk output
      (append `(,openjdk14 "jdk"))))))

(define-public openjdk16
  (make-openjdk openjdk15 "16.0.2"
                "0587px2qbz07g3xi4a3ya6m630p72dvkxcn0bj1813pxnwvcgigz"
   (source (origin
             (inherit (package-source base))
             (patches (search-patches "openjdk-15-xcursor-no-dynamic.patch"
                                      "openjdk-10-setsignalhandler.patch"))))))

(define-public openjdk17
  (make-openjdk
   openjdk16 "17.0.10"
   "1bq1rqnipz6wdr05s20gm8nlpb3328ljxckzvc5ag0gf7fzlhn5f"
   (source (origin
             (inherit (package-source base))
             (patches (search-patches "openjdk-15-xcursor-no-dynamic.patch"))))
   (arguments
    (substitute-keyword-arguments (package-arguments openjdk16)
      ((#:phases phases)
       #~(modify-phases #$phases
           (replace 'fix-java-shebangs
             (lambda _
               ;; 'blacklisted' was renamed back to 'blocked'.
               (substitute* "make/data/blockedcertsconverter/blocked.certs.pem"
                 (("^#!.*") "#! java BlockedCertsConverter SHA-256\n"))))))))))

(define-public openjdk18
  (make-openjdk openjdk17 "18.0.2.1"
                "0zxanjzz4p3psqahlidh55vx1ldanq70c2ygk3gcfn9a94vnr9rg"))

(define-public openjdk19
  (make-openjdk openjdk18 "19.0.2"
                "08kvx7n8qhhfl25pig966881j5h4x7y0pf4brq16x0283fc0f4d4"
   (arguments
    (substitute-keyword-arguments (package-arguments openjdk18)
      ((#:phases phases)
       #~(modify-phases #$phases
           (replace 'fix-java-shebangs
             (lambda _
               ;; Update file path.
               (substitute* "src/java.base/share/data/blockedcertsconverter/blocked.certs.pem"
                 (("^#!.*") "#! java BlockedCertsConverter SHA-256\n"))))
           (add-before 'configure 'define-java-environment-variables
             (lambda* (#:key inputs #:allow-other-keys)
               ;; Fix for "valid range 1980-01-01T00:00:02Z to 2099-12-31T23:59:59Z".
               (setenv "SOURCE_DATE_EPOCH" "1234567890")))))))))

(define-public openjdk20
  (make-openjdk openjdk19 "20.0.2"
                "1af1v2c3d8x4c6shzl6cv9qwq7a4hn5map5pjh9vjcky0hkzd489"))

(define-public openjdk21
  (make-openjdk openjdk20 "21.0.2"
                "0d1g3wnzr5whjpq8gvxq0h7kd7lxd3xgc6bh3kg8vzz096asn0kj"
   (source (origin
             (inherit (package-source base))
             (patches (search-patches "openjdk-21-fix-rpath.patch"
                                      "openjdk-15-xcursor-no-dynamic.patch"))))
   (arguments
    (substitute-keyword-arguments (package-arguments base)
      ((#:phases phases)
       #~(modify-phases #$phases
           (replace 'fix-java-shebangs
             (lambda _
               ;; 'blacklisted' was renamed back to 'blocked'.
               (substitute* "src/java.base/share/data/blockedcertsconverter/blocked.certs.pem"
                 (("^#!.*") "#! java BlockedCertsConverter SHA-256\n"))))))))))

(define-public openjdk22
  (make-openjdk
   openjdk21 "22.0.2"
   "1nj414yj6v9qrlm48yv5llr4jmgj9g20v6zsd39xrdx4x4x4p3b6"
   (arguments
    (substitute-keyword-arguments (package-arguments base)
      ((#:phases phases)
       #~(modify-phases #$phases
           (add-after 'unpack 'do-not-disable-new-dtags
             (lambda _
               ;; Our validate-runpath phases checks for RUNPATH, not RPATH.
               (substitute* "make/autoconf/flags-cflags.m4"
                 ((" -Wl,--disable-new-dtags") ""))))))))))

(define-public openjdk23
  (make-openjdk
   openjdk22 "23.0.2"
   "0kxllznzhgqfn8b97krg2yp1ag41g4phmgqahrvzafd2bq6zclnf"
   (source (origin
             (inherit (package-source base))
             ;; The 'openjdk-21-fix-rpath.patch' no longer applies, and it
             ;; appears not needed anymore.  The
             ;; 'openjdk-15-xcursor-no-dynamic.patch' doesn't apply anymore;
             ;; the fix should be pursued in libx11 (see:
             ;; https://issues.guix.gnu.org/54654)
             (patches '())))))

(define-public openjdk24
  (make-openjdk
   openjdk23 "24.0.1"
   "0h6sbzbjyqg85iml41pswdh2z3d7h2hhb0sd5yll37r1mj5lsxmx"))

;;; Convenience alias to point to the latest version of OpenJDK.
(define-public openjdk openjdk24)


;; This version of JBR is here in order to be able to build custom
;; IntelliJ plugins.  Those usually need both jbr11 and jbr17 for
;; tests.
(define-public jbr11
  (package
    (inherit openjdk11)
    (name "jbr")
    (version "11_0_16-b2248")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/JetBrains/JetBrainsRuntime.git")
                     (commit (string-append "jb" version))))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "1fnrdx0wb21ghm6jczjzk7b9fz9hbdzd62512xhwpzvca57v2z09"))))
    (arguments
     (substitute-keyword-arguments (package-arguments openjdk11)
       ((#:configure-flags configure-flags)
        #~(append #$configure-flags
                  (list "--with-jvm-features=shenandoahgc"
                        "--enable-cds=yes"
                        "--with-vendor-name=JetBrains s.r.o"
                        "--with-vendor-url=https://www.jetbrains.com/"
                        "--with-vendor-bug-url=https://youtrack.jetbrains.com/issues/JBR")))))
    (synopsis "JetBrains Java Runtime")
    (description "This package provides a Java runtime environment for
and Java development kit.  It supports enhanced class redefinition (DCEVM),
includes a number of improvements in font rendering, keyboards support,
windowing/focus subsystems, HiDPI, accessibility, and performance,
provides better desktop integration and bugfixes not yet present in
OpenJDK.")
    (home-page "https://www.jetbrains.com/")
    (license license:gpl2+)))

(define-public jbr17
  (package
    (inherit openjdk17)
    (name "jbr")
    (version "17.0.11b1207.30")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/JetBrains/JetBrainsRuntime.git")
                     (commit (string-append "jbr-release-" version))))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "5bbf5z8faf9sdkig2fwkj06hb7hs9s3bpzzvbqn2i75lbwqk3bln"))
              (patches (search-patches "jbr-17-xcursor-no-dynamic.patch"))))
    (arguments
     (substitute-keyword-arguments (package-arguments openjdk17)
       ((#:configure-flags configure-flags)
        #~(append #$configure-flags
                  (list "--with-jvm-features=shenandoahgc"
                        "--enable-cds=yes"
                        "--with-vendor-name=JetBrains s.r.o"
                        "--with-vendor-url=https://www.jetbrains.com/"
                        "--with-vendor-bug-url=https://youtrack.jetbrains.com/issues/JBR")))))
    (synopsis "JetBrains Java Runtime")
    (description "This package provides a Java runtime environment for
and Java development kit.  It supports enhanced class redefinition (DCEVM),
includes a number of improvements in font rendering, keyboards support,
windowing/focus subsystems, HiDPI, accessibility, and performance,
provides better desktop integration and bugfixes not yet present in
OpenJDK.")
    (home-page "https://www.jetbrains.com/")
    (license license:gpl2+)))

(define-public jbr21
  (package
    (inherit openjdk21)
    (name "jbr")
    (version "21.0.3b509.11")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/JetBrains/JetBrainsRuntime.git")
                     (commit (string-append "jbr-release-" version))))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "03xkghbp0b1czc2ags9n8ilprf9sy4bhsma0wf8xfapqn6d2sfyd"))
              (patches (search-patches "openjdk-21-fix-rpath.patch"
                                       "jbr-17-xcursor-no-dynamic.patch"))))
    (inputs
     `(("wayland" ,wayland)
       ("libxkbcommon" ,libxkbcommon) ; for wayland
       ,@(package-inputs openjdk21)))
    (arguments
     (substitute-keyword-arguments (package-arguments openjdk21)
       ((#:configure-flags configure-flags)
        #~(append #$configure-flags
                  (list "--with-jvm-features=shenandoahgc"
                        "--enable-cds=yes"
                        "--with-vendor-name=JetBrains s.r.o"
                        "--with-vendor-url=https://www.jetbrains.com/"
                        "--with-vendor-bug-url=https://youtrack.jetbrains.com/issues/JBR")))))
    (synopsis "JetBrains Java Runtime")
    (description "This package provides a Java runtime environment for
and Java development kit.  It supports enhanced class redefinition (DCEVM),
includes a number of improvements in font rendering, keyboards support,
windowing/focus subsystems, HiDPI, accessibility, and performance,
provides better desktop integration and bugfixes not yet present in
OpenJDK.")
    (home-page "https://www.jetbrains.com/")
    (license license:gpl2+)))


(define-public ant/java8
  (package
    (name "ant")
    (version "1.10.13")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/ant/source/apache-ant-"
                                  version "-src.tar.gz"))
              (sha256
               (base32
                "01l4g9b1xnnq450ljvhrlvcf8wzzmr45wmhkybrx0hcdi166y06s"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (for-each delete-file
                            (find-files "lib/optional" "\\.jar$"))
                  #t))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:modules '((srfi srfi-1)
                  (guix build gnu-build-system)
                  (guix build utils))
      #:tests? #f                       ;no "check" target
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-scripts
            ;; Remove bat / cmd scripts for DOS as well as the antRun and runant
            ;; wrappers.
            (lambda _
              (for-each delete-file
                        (find-files "src/script"
                                    "(.*\\.(bat|cmd)|runant.*|antRun.*)"))))
          (delete 'bootstrap)
          (delete 'configure)
          (replace 'build
            (lambda* (#:key inputs #:allow-other-keys)
              (setenv "JAVA_HOME" (assoc-ref inputs "jdk"))

              ;; Disable tests to avoid dependency on hamcrest-core, which needs
              ;; Ant to build.  This is necessary in addition to disabling the
              ;; "check" phase, because the dependency on "test-jar" would always
              ;; result in the tests to be run.
              (substitute* "build.xml"
                (("depends=\"jars,test-jar") "depends=\"jars"))
              (invoke "bash" "bootstrap.sh"
                      (string-append "-Ddist.dir=" #$output))))
          (add-after 'build 'strip-jar-timestamps ;based on ant-build-system
            (lambda _
              (define (repack-archive jar)
                (let* ((dir (mkdtemp "jar-contents.XXXXXX"))
                       (manifest (string-append dir "/META-INF/MANIFESTS.MF")))
                  (with-directory-excursion dir
                    (invoke "unzip" jar))
                  (delete-file jar)
                  ;; XXX: copied from (gnu build install)
                  (for-each (lambda (file)
                              (let ((s (lstat file)))
                                (unless (eq? (stat:type s) 'symlink)
                                  (utime file  0 0 0 0))))
                            (find-files dir #:directories? #t))
                  ;; It is important that the manifest appears first.
                  (with-directory-excursion dir
                    (let* ((files (find-files "." ".*" #:directories? #t))
                           ;; To ensure that the reference scanner can
                           ;; detect all store references in the jars
                           ;; we disable compression with the "-0" option.
                           (command (if (file-exists? manifest)
                                        `("zip" "-0" "-X" ,jar ,manifest
                                          ,@files)
                                        `("zip" "-0" "-X" ,jar ,@files))))
                      (apply invoke command)))))
              (for-each repack-archive
                        (find-files
                         (string-append #$output "/lib")
                         "\\.jar$"))))
          (delete 'install))))
    (native-inputs
     `(("jdk" ,icedtea-8 "jdk")
       ("zip" ,zip)
       ("unzip" ,unzip)))
    (home-page "https://ant.apache.org")
    (synopsis "Build tool for Java")
    (description
     "Ant is a platform-independent build tool for Java.  It is similar to
make but is implemented using the Java language, requires the Java platform,
and is best suited to building Java projects.  Ant uses XML to describe the
build process and its dependencies, whereas Make uses Makefile format.")
    (license license:asl2.0)))

;; The 1.9.x series is the last that can be built with GCJ.  The 1.10.x series
;; requires Java 8.
(define-public ant
  (package (inherit ant/java8)
    (version "1.9.15")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/ant/source/apache-ant-"
                                  version "-src.tar.gz"))
              (sha256
               (base32
                "1xy30f1w5gaqk6g3f0vw7ygix4rb6032qkcw42y4z8wd9jihgygd"))))
    ;; XXX: we do this to avoid a rebuild.  This mess will be cleaned up
    ;; later.
    (arguments
     (substitute-keyword-arguments
         `(#:modules ((srfi srfi-1)
                      (guix build gnu-build-system)
                      (guix build utils))
           #:tests? #f                  ; no "check" target
           #:phases
           (modify-phases %standard-phases
             (delete 'bootstrap)
             (delete 'configure)
             (add-before 'build 'define-java-environment-variables
               (lambda* (#:key inputs #:allow-other-keys)
                 ;; First, set environment variables (eases debugging on -K).
                 (setenv "JAVA_HOME" (assoc-ref inputs "jamvm"))
                 (setenv "JAVACMD" (search-input-file inputs "/bin/jamvm"))
                 (setenv "JAVAC" (search-input-file inputs "/bin/jikes"))
                 (setenv "CLASSPATH" (search-input-file inputs "/lib/rt.jar"))))
             (replace 'build
               (lambda* (#:key inputs outputs #:allow-other-keys)
                 ;; Ant complains if this file doesn't exist.
                 (setenv "HOME" "/tmp")
                 (with-output-to-file "/tmp/.ant.properties"
                   (lambda _ (display "")))

                 ;; Use jikes instead of javac for <javac ...> tags in build.xml
                 (setenv "ANT_OPTS" "-Dbuild.compiler=jikes")

                 ;; jikes produces lots of warnings, but they are not very
                 ;; interesting, so we silence them.
                 (setenv "$BOOTJAVAC_OPTS" "-nowarn")

                 ;; Without these JamVM options the build may freeze.
                 (substitute* "bootstrap.sh"
                   (("^\"\\$\\{JAVACMD\\}\" " m)
                    ,@(if (string-prefix? "armhf" (or (%current-system)
                                                      (%current-target-system)))
                          `((string-append m "-Xnocompact "))
                          `((string-append m "-Xnocompact -Xnoinlining ")))))

                 ;; Disable tests because we are bootstrapping and thus don't have
                 ;; any of the dependencies required to build and run the tests.
                 (substitute* "build.xml"
                   (("depends=\"jars,test-jar\"") "depends=\"jars\""))
                 (invoke "bash" "bootstrap.sh"
                         (string-append "-Ddist.dir="
                                        (assoc-ref outputs "out")))))
             (add-after 'build 'strip-jar-timestamps ;based on ant-build-system
               (lambda* (#:key outputs #:allow-other-keys)
                 (define (repack-archive jar)
                   (let* ((dir (mkdtemp "jar-contents.XXXXXX"))
                          (manifest (string-append dir "/META-INF/MANIFESTS.MF")))
                     (with-directory-excursion dir
                       (invoke "unzip" jar))
                     (delete-file jar)
                     ;; XXX: copied from (gnu build install)
                     (for-each (lambda (file)
                                 (let ((s (lstat file)))
                                   (unless (eq? (stat:type s) 'symlink)
                                     (utime file  0 0 0 0))))
                               (find-files dir #:directories? #t))
                     ;; It is important that the manifest appears first.
                     (with-directory-excursion dir
                       (let* ((files (find-files "." ".*" #:directories? #t))
                              ;; To ensure that the reference scanner can
                              ;; detect all store references in the jars
                              ;; we disable compression with the "-0" option.
                              (command (if (file-exists? manifest)
                                           `("zip" "-0" "-X" ,jar ,manifest
                                             ,@files)
                                           `("zip" "-0" "-X" ,jar ,@files))))
                         (apply invoke command)))))
                 (for-each repack-archive
                           (find-files
                            (string-append (assoc-ref %outputs "out") "/lib")
                            "\\.jar$"))))
             (delete 'install)))
       ((#:phases phases)
        `(modify-phases ,phases
           (delete 'define-java-environment-variables)
           (add-after 'unpack 'remove-scripts
             ;; Remove bat / cmd scripts for DOS as well as the antRun and runant
             ;; wrappers.
             (lambda _
               (for-each delete-file
                         (find-files "src/script"
                                     "(.*\\.(bat|cmd)|runant.*|antRun.*)"))
               #t))
           (replace 'build
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (setenv "JAVA_HOME" (assoc-ref inputs "jdk"))

               ;; Disable tests to avoid dependency on hamcrest-core, which needs
               ;; Ant to build.  This is necessary in addition to disabling the
               ;; "check" phase, because the dependency on "test-jar" would always
               ;; result in the tests to be run.
               (substitute* "build.xml"
                 (("depends=\"jars,test-jar") "depends=\"jars"))
               (invoke "bash" "bootstrap.sh"
                       (string-append "-Ddist.dir="
                                      (assoc-ref outputs "out")))))))))
    (native-inputs
     `(("jdk" ,icedtea-7 "jdk")
       ("zip" ,zip)
       ("unzip" ,unzip)))))

(define-public ant-apache-bcel
  (package
    (inherit ant/java8)
    (name "ant-apache-bcel")
    (arguments
     (substitute-keyword-arguments (package-arguments ant/java8)
       ((#:phases phases)
        #~(modify-phases #$phases
            (add-after 'unpack 'link-bcel
              (lambda* (#:key inputs #:allow-other-keys)
                (for-each (lambda (file)
                            (symlink file
                                     (string-append "lib/optional/"
                                                    (basename file))))
                          (find-files (assoc-ref inputs "java-commons-bcel")
                                      "\\.jar$"))))
            (add-after 'build 'install
              (lambda _
                (let ((share (string-append #$output "/share/java"))
                      (bin   (string-append #$output "/bin"))
                      (lib   (string-append #$output "/lib")))
                  (mkdir-p share)
                  (install-file (string-append lib "/ant-apache-bcel.jar") share)
                  (delete-file-recursively bin)
                  (delete-file-recursively lib))))))))
    (inputs
     (modify-inputs (package-inputs ant/java8)
       (prepend java-commons-bcel)))))

(define-public ant-junit
  (package
    (inherit ant/java8)
    (name "ant-junit")
    (arguments
     (substitute-keyword-arguments (package-arguments ant/java8)
       ((#:phases phases)
        #~(modify-phases #$phases
            (add-after 'unpack 'link-junit
              (lambda* (#:key inputs #:allow-other-keys)
                (for-each (lambda (file)
                            (symlink file
                                     (string-append "lib/optional/"
                                                    (basename file))))
                          (find-files (assoc-ref inputs "java-junit")
                                      "\\.jar$"))))
            (add-after 'build 'install
              (lambda _
                (let ((share (string-append #$output "/share/java"))
                      (bin   (string-append #$output "/bin"))
                      (lib   (string-append #$output "/lib")))
                  (mkdir-p share)
                  (install-file (string-append lib "/ant-junit.jar") share)
                  (delete-file-recursively bin)
                  (delete-file-recursively lib))))))))
    (inputs
     (modify-inputs (package-inputs ant/java8)
       (prepend java-junit)))))

(define-public libantlr3c
  (package
    (name "libantlr3c")
    (version "3.4")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://www.antlr3.org/download/C/"
                       name "-" version ".tar.gz"))
       (sha256
        (base32 "0lpbnb4dq4azmsvlhp6khq1gy42kyqyjv8gww74g5lm2y6blm4fa"))))
    (build-system gnu-build-system)
    (arguments
     (list #:configure-flags
           #~(list "--enable-debuginfo"
                   "--disable-static"
                   #$@(if (target-64bit?)
                          #~("--enable-64bit")
                          #~())
                   #$@(if (target-riscv64?)
                          #~("--disable-abiflags")
                          #~()))))
    (synopsis "ANTLR C Library")
    (description "LIBANTLR3C provides run-time C libraries for ANTLR3 (ANother
Tool for Language Recognition v3).")
    (home-page "https://www.antlr3.org/")
    (license license:bsd-3)))

(define-public drip
  ;; Last release is from 2014, with a few important commits afterwards.
  (let ((commit "a4bd00df0199e78243847f06cc04ecaea31f8f08"))
    (package
      (name "drip")
      (version (git-version "0.2.4" "1" commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/ninjudd/drip")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0wzmjwfyldr3jn49517xd8yn7dgdk8h88qkga3kjyg1zc375ylg2"))))
      (build-system gnu-build-system)
      (native-inputs
       `(("jdk" ,icedtea "jdk")))
      (arguments
       `(#:tests? #f                    ; No tests.
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (add-before 'install 'fix-wrapper
             (lambda* (#:key inputs #:allow-other-keys)
               (let ((jps (search-input-file inputs "/bin/jps")))
                 (substitute* "bin/drip"
                   (("jps") jps)
                   (("brew update && brew upgrade drip") "guix pull && guix install drip")
                   ;; No need to make:
                   (("\\(cd -- \"\\$drip_dir\" && make -s\\) \\|\\| exit 1") "")
                   ;; No need to include source:
                   (("\\[\\[ -r \\$drip_dir/src/org/flatland/drip/Main\\.java \\]\\]")
                    "true"))
                 #t)))
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (bin (string-append out "/bin"))
                      (share (string-append out "/share/drip")))
                 (mkdir-p bin)
                 (for-each
                  (lambda (file)
                    (install-file (string-append "bin/" file) bin))
                  '("drip" "drip_daemon" "drip_proxy"))
                 (install-file "drip.jar" share)
                 (substitute* (string-append bin "/drip")
                   (("drip_dir=\\$bin_dir/..")
                    (string-append "drip_dir=" share)))
                 #t))))))
      (home-page "https://github.com/ninjudd/drip")
      (synopsis "Faster Java Virtual Machine launching")
      (description "Drip is a launcher for the Java Virtual Machine that
provides much faster startup times than the @command{java} command.  The @command{drip}
script is intended to be a drop-in replacement for the @command{java} command,
only faster.")
      (license license:epl1.0))))

(define-public java-openjfx-build
  (package
    (name "java-openjfx-build")
    ;; This is a java-8 version
    (version "8.202")
    (source (origin
              (method hg-fetch)
              (uri (hg-reference
                    (url "http://hg.openjdk.java.net/openjfx/8u-dev/rt")
                    (changeset (string-append
                                (string-join (string-split version #\.) "u")
                                "-ga"))))
              (file-name (string-append name "-" version "-checkout"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Delete included gradle jar
                  (delete-file-recursively "gradle/wrapper")
                  #t))
              (sha256
               (base32
                "0yg38mwpivswccv9n96k06x3iv82i4px1a9xg9l8dswzwmfj259f"))
              (patches (search-patches "java-openjfx-build-jdk_version.patch"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "java-openjfx.jar"
       #:source-dir "buildSrc/src/main/java"
       #:test-dir "buildSrc/src/test"
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'generate-jsl-parser
           (lambda _
             (invoke "antlr3" "-o"
                     "buildSrc/src/main/java/com/sun/scenario/effect/compiler"
                     "buildSrc/src/main/antlr/JSL.g"))))))
    (inputs
     (list antlr3 java-stringtemplate))
    (native-inputs
     (list java-junit java-hamcrest-core))
    (home-page "https://openjfx.io")
    (synopsis "Graphical application toolkit in Java")
    (description "OpenJFX is a client application platform for desktop,
mobile and embedded systems built on Java.  Its goal is to produce a
modern, efficient, and fully featured toolkit for developing rich client
applications.  This package contains base classes for the OpenJFX
distribution and helper classes for building other parts of the
distribution.")
    (license license:gpl2))) ;gpl2 only, with classpath exception

(define-public java-openjfx-base
  (package (inherit java-openjfx-build)
    (name "java-openjfx-base")
    (arguments
     `(#:jar-name "java-openjfx-base.jar"
       #:source-dir "modules/base/src/main/java:modules/base/src/main/java8:modules/base/src/main/version-info"
       #:test-dir "modules/base/src/test"
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'remove-empty-file
           (lambda _
             (with-directory-excursion "modules/base/src/test/java"
               ;; These files are completely commented, but junit expects them to
               ;; contain a class, so tests fail.
               (delete-file
                 "com/sun/javafx/property/adapter/PropertyDescriptorTest.java")
               (delete-file
                 "com/sun/javafx/property/adapter/ReadOnlyPropertyDescriptorTest.java")
               (delete-file "javafx/beans/property/PropertiesTest.java")
               (delete-file
                 "javafx/beans/property/adapter/ReadOnlyJavaBeanPropertyBuilder_General_Test.java")
               ;; This one fails
               (delete-file "com/sun/javafx/runtime/VersionInfoTest.java"))
             #t)))))
    (propagated-inputs
     (list java-openjfx-build))
    (description "OpenJFX is a client application platform for desktop,
mobile and embedded systems built on Java.  Its goal is to produce a
modern, efficient, and fully featured toolkit for developing rich client
applications.  This package contains base classes for the OpenJFX
distribution.")))

(define-public java-openjfx-graphics
  (package (inherit java-openjfx-build)
    (name "java-openjfx-graphics")
    (arguments
     `(#:jar-name "java-openjfx-graphics.jar"
       #:source-dir "modules/graphics/src/main/java"
       #:tests? #f; require X
       #:test-dir "modules/graphics/src/test"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'copy-missing-file
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((target "modules/graphics/src/main/native-prism-sw/JNativeSurface.c"))
               (copy-file (assoc-ref inputs "JNativeSurface.c") target)
               ;; XXX: looks like the missing file we found isn't *quite*
               ;; compatible...
               (substitute* target
                 (("case TYPE_INT_ARGB:") "")))))
         (add-after 'build 'build-native
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((jdk (assoc-ref inputs "jdk"))
                   (class-file->class-name
                    (lambda (class-file)
                      (string-map (lambda (c)
                                    (if (char=? c #\/) #\. c))
                                  (string-drop-right class-file
                                                     (string-length ".class"))))))
               (setenv "CPPFLAGS"
                       (string-append "-DINLINE=inline "
                                      "-DLINUX "
                                      "-I" jdk "/include "
                                      "-I" jdk "/include/linux "
                                      "-I " (getcwd) "/build/classes/include "
                                      "-I " (getcwd) "/modules/graphics/src/main/native-prism-sw"))

               ;; Instructions have been adapted from buildSrc/linux.gradle
               (with-directory-excursion "build/classes"
                 ;; Build prism
                 (mkdir-p "include")

                 ;; Generate headers for prism
                 (apply invoke "javah" "-d" "include" "-cp" "."
                        (map class-file->class-name
                             (append (find-files "com/sun/prism/impl" "\\.class$")
                                     (find-files "com/sun/prism" "PresentableState.*\\.class$"))))

                 ;; ...then for prism_sw
                 (apply invoke "javah" "-d" "include" "-cp" "."
                        (map class-file->class-name
                             (find-files "com/sun/pisces" "\\.class$")))

                 ;; ...and for prism_es2
                 (apply invoke "javah" "-d" "include" "-cp" "."
                        (map class-file->class-name
                             (find-files "com/sun/prism/es2" "\\.class$")))))

             (with-directory-excursion "netbeans/native-prism"
               (invoke "make" "CONF=Release"))
             (with-directory-excursion "netbeans/native-prism-sw"
               (invoke "make" "CONF=Release"))
             ;; TODO: This fails due to unknown EGL procedure names
             #;
             (with-directory-excursion "netbeans/native-prism-es2"
               (invoke "make" "CONF=Release"))

             (let* ((out (assoc-ref outputs "out"))
                    (dir ,(match (%current-system)
                            ("i686-linux"
                             "i386")
                            ((or "armhf-linux" "aarch64-linux")
                             "arm")
                            ((or "x86_64-linux")
                             "amd64")
                            (_ "unknown")))
                    (target (string-append out "/share/" dir "/")))
               (mkdir-p target)
               (for-each (lambda (file)
                           (let ((new-name
                                  (string-append "lib"
                                                 (string-map
                                                  (lambda (c)
                                                    (if (char=? c #\-) #\_ c))
                                                  (string-drop (basename file)
                                                               (string-length "libnative-"))))))
                             (copy-file file
                                        (string-append target new-name))))
                         (find-files "netbeans" "\\.so$"))))))))
    (propagated-inputs
     (list java-openjfx-base))
    (inputs
     (list java-swt))
    ;; XXX: for unknown reasons
    ;; modules/graphics/src/main/native-prism-sw/JNativeSurface.c is missing
    ;; in this revision.
    (native-inputs
     `(("JNativeSurface.c"
        ,(origin
           (method url-fetch)
           (uri "https://raw.githubusercontent.com/openjdk/jfx/8u20-b02\
/modules/graphics/src/main/native-prism-sw/JNativeSurface.c")
           (sha256
            (base32
             "1kp15wbnd6rn0nciczp5ibq0ikby2yysvx1gnz5fa05vl2mm8mbm"))))))
    (description "OpenJFX is a client application platform for desktop,
mobile and embedded systems built on Java.  Its goal is to produce a
modern, efficient, and fully featured toolkit for developing rich client
applications.  This package contains graphics-related classes for the
OpenJFX distribution.")))

(define-public java-openjfx-media
  (package (inherit java-openjfx-build)
    (name "java-openjfx-media")
    (propagated-inputs
     `(("java-openjxf-graphics" ,java-openjfx-graphics)))
    (arguments
     `(#:jar-name "java-openjfx-media.jar"
       #:source-dir "modules/media/src/main/java"
       #:tests? #f)); no tests
    (description "OpenJFX is a client application platform for desktop,
mobile and embedded systems built on Java.  Its goal is to produce a
modern, efficient, and fully featured toolkit for developing rich client
applications.  This package contains media-related classes for the
OpenJFX distribution.")))

(define-public java-openjfx-controls
  (package (inherit java-openjfx-build)
    (name "java-openjfx-controls")
    (propagated-inputs
     `(("java-openjxf-graphics" ,java-openjfx-graphics)))
    (arguments
     `(#:jar-name "java-openjfx-controls.jar"
       #:source-dir "modules/controls/src/main/java"
       #:test-dir "modules/controls/src/test"
       ;; TODO: tests require com.sun.javafx.pgstub,
       ;; javafx.collections.MockSetObserver, and
       ;; com.sun.javafx.binding.ExpressionHelperUtility
       #:tests? #false
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'copy-resources
           (lambda _
             (copy-recursively "modules/controls/src/test/resources"
                               "build/test-classes")
             (copy-recursively "modules/controls/src/main/resources"
                               "build/classes"))))))
    (description "OpenJFX is a client application platform for desktop,
mobile and embedded systems built on Java.  Its goal is to produce a
modern, efficient, and fully featured toolkit for developing rich client
applications.  This package contains UI control classes for the
OpenJFX distribution.")))

(define-public javacc-4
  (package
    (name "javacc")
    (version "4.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/javacc/javacc")
                    (commit "release_41")))
              (file-name (string-append "javacc-" version "-checkout"))
              (sha256
               (base32
                "07ysav7j8r1c6h8qxrgqk6lwdp74ly0ad1935lragxml0qqc3ka0"))
              (modules '((guix build utils)))
              ;; delete bundled jars
              (snippet '(begin (delete-file-recursively "lib") #t))))
    (build-system ant-build-system)
    ;; Tests fail with
    ;; /tmp/guix-build-javacc-4.1.drv-0/source/test/javacodeLA/build.xml:60:
    ;; JAVACODE failed
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         ;; Delete tests to avoid build failure (we don't run them anyway).
         (add-after 'unpack 'delete-tests
           (lambda _
             (for-each delete-file
                       '("src/org/javacc/JavaCCTestCase.java"
                         "src/org/javacc/parser/ExpansionTest.java"
                         "src/org/javacc/parser/OptionsTest.java"
                         "src/org/javacc/jjtree/JJTreeOptionsTest.java"))
             (for-each delete-file-recursively
                       '("src/org/javacc/parser/test"
                         "src/org/javacc/jjdoc/test"))
             #t))
         (replace 'install (install-jars "bin/lib")))))
    (home-page "https://javacc.org/")
    (synopsis "Java parser generator")
    (description "Java Compiler Compiler (JavaCC) is the most popular parser
generator for use with Java applications.  A parser generator is a tool that
reads a grammar specification and converts it to a Java program that can
recognize matches to the grammar.  In addition to the parser generator itself,
JavaCC provides other standard capabilities related to parser generation such
as tree building (via a tool called JJTree included with JavaCC), actions,
debugging, etc.")
    (license license:bsd-3)))

;; javacc-3, as javacc-4 is not properly bootstrapped: is contains a javacc.jar
;; in the bootstrap/ directory.
(define-public javacc-3
  (package
    (inherit javacc-4)
    (version "3.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/javacc/javacc")
                    (commit "release_32")))
              (file-name (string-append "javacc-" version "-checkout"))
              (sha256
               (base32
                "1pyf1xyh8gk83nxqn2v2mdws32l68ydznha41cxa4l2kkbq1v1g3"))))
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'set-java-version
           (lambda _
             (for-each
               (lambda (file)
                 (substitute* file
                   (("debug=") "source=\"1.4\" debug=")))
               (find-files "." "build.xml"))
             #t))
         (replace 'install (install-jars "bin/lib")))))))

(define-public javacc
  (package
    (inherit javacc-4)
    (version "7.0.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/javacc/javacc")
             (commit version)))
       (file-name (git-file-name "javacc" version))
       (sha256
        (base32 "18kkak3gda93gr25jrgy6q00g0jr8i24ri2wk4kybz1v234fxx9i"))
       (modules '((guix build utils)))
       ;; Delete bundled jars.
       (snippet '(begin (for-each delete-file-recursively
                                  '("bootstrap" "lib"))))))
    (arguments
     `(#:make-flags                     ; bootstrap from javacc-4
       ,#~(list (string-append "-Dbootstrap-jar="
                               #$(this-package-native-input "javacc")
                               "/share/java/javacc.jar"))
       #:test-target "test"
       #:phases
       (modify-phases %standard-phases
         (replace 'install (install-jars "target"))
         (add-after 'install 'install-bin
           (lambda* (#:key outputs inputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (dir (string-append out "/share/java"))
                    (bin (string-append out "/bin"))
                    (javacc (string-append bin "/javacc")))
               (mkdir-p bin)
               (with-output-to-file javacc
                 (lambda _
                   (display
                     (string-append "#!/bin/sh\n"
                                    (assoc-ref inputs "icedtea") "/bin/java"
                                    " -cp " dir "/javacc.jar" " `basename $0`" " $*"))))
               (chmod javacc #o755)
               ;; symlink to different names to affect the first argument and
               ;; change the behavior of the jar file.
               (symlink javacc (string-append bin "/jjdoc"))
               (symlink javacc (string-append bin "/jjtree"))))))))
    (native-inputs
     (list javacc-4))
    (inputs (list icedtea-8))))

(define-public java-ecj
  (package
    (name "java-ecj")
    (version "4.6.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://archive.eclipse.org/eclipse/"
                           "downloads/drops4/R-" version "-201703010400/"
                           "ecjsrc-" version ".jar"))
       (sha256
        (base32
         "11cfgsdgznja1pvlxkjbqykxd7pcd5655vkm7s44xmahmap15gpl"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f                      ; none included
       #:build-target "build"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-build.xml
           (lambda _
             (substitute* "src/build.xml"
               (("^.*MANIFEST.*$")
                "")
               (("^.*properties.*$")
                "<include name=\"**/*.properties\"/>
 <include name=\"**/*.props\"/>"))))
         (add-before 'build 'chdir
           (lambda _ (chdir "src")))
         (replace 'install (install-jars ".")))))
    (home-page "https://eclipse.org")
    (synopsis "Eclipse Java development tools core batch compiler")
    (description "This package provides the Eclipse Java core batch compiler.")
    (license license:epl1.0)))

(define-public java-cisd-base
  (let ((revision 38938)
        (base-version "14.12.0"))
    (package
      (name "java-cisd-base")
      (version (string-append base-version "-" (number->string revision)))
      (source (origin
                (method svn-fetch)
                (uri (svn-reference
                      (url (string-append "http://svnsis.ethz.ch/repos/cisd/"
                                          "base/tags/release/"
                                          (version-major+minor base-version)
                                          ".x/" base-version "/base/"))
                      (revision revision)))
                (file-name (string-append "java-cisd-base-" version "-checkout"))
                (sha256
                 (base32
                  "1i5adyf7nzclb0wydgwa1az04qliid8035vpahaandmkmigbnxiy"))
                (modules '((guix build utils)))
                (snippet
                 '(begin
                    ;; Delete included gradle jar
                    (delete-file-recursively "gradle/wrapper")
                    ;; Delete pre-built native libraries
                    (delete-file-recursively "libs")
                    #t))))
      (build-system ant-build-system)
      (arguments
       `(#:make-flags '("-file" "build/build.xml")
         #:test-target "jar-test"
         #:jdk ,icedtea-8
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'unpack-build-resources
             (lambda* (#:key inputs #:allow-other-keys)
               (copy-recursively (assoc-ref inputs "build-resources")
                                 "../build_resources")
               #t))
           (add-after 'unpack-build-resources 'fix-dependencies
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "build/build.xml"
                 (("\\$\\{lib\\}/testng/testng-jdk15.jar")
                  (search-input-file inputs
                                     "/share/java/java-testng.jar"))
                 (("\\$\\{lib\\}/commons-lang/commons-lang.jar")
                  (search-input-file inputs
                                     (string-append
                                      "/share/java/commons-lang-"
                                      ,(package-version java-commons-lang)
                                      ".jar")))
                 (("\\$\\{lib\\}/commons-io/commons-io.jar")
                  (search-input-file inputs
                                     (string-append
                                      "/lib/m2/commons-io/commons-io/"
                                      ,(package-version java-commons-io)
                                      "/commons-io-"
                                      ,(package-version java-commons-io)
                                      ".jar")))
                 ;; Remove dependency on svn
                 (("<build-info.*") "")
                 (("\\$\\{revision.number\\}")
                  ,(number->string revision))
                 (("\\$\\{version.number\\}") ,base-version))
               ;; Remove dependency on classycle
               (substitute* "../build_resources/ant/build-common.xml"
                 (("<taskdef name=\"dependency-checker.*") "")
                 (("classname=\"classycle.*") "")
                 (("classpath=\"\\$\\{lib\\}/classycle.*") ""))
               #t))
           ;; A few tests fail because of the lack of a proper /etc/groups and
           ;; /etc/passwd file in the build container.
           (add-after 'unpack 'disable-broken-tests
             (lambda _
               (substitute* "sourceTest/java/ch/systemsx/cisd/base/AllTests.java"
                 (("Unix.isOperational\\(\\)") "false"))
               #t))
           ;; These decorators are almost useless and pull in an unpackaged
           ;; dependency.
           (add-after 'unpack 'remove-useless-decorators
             (lambda _
               (substitute* "source/java/ch/systemsx/cisd/base/unix/Unix.java"
                 (("@Private") "")
                 (("import ch.rinn.restrictions.Private;") ""))
               (substitute* "sourceTest/java/ch/systemsx/cisd/base/unix/UnixTests.java"
                 (("@Friend.*") "")
                 (("import ch.rinn.restrictions.Friend;") ""))
               #t))
           (add-before 'configure 'build-native-code
             (lambda* (#:key inputs #:allow-other-keys)
               (let ((jdk (assoc-ref inputs "jdk"))
                     (dir ,(match (%current-system)
                             ("i686-linux"
                              "i386-Linux")
                             ((or "armhf-linux" "aarch64-linux")
                              "arm-Linux")
                             ((or "x86_64-linux")
                              "amd64-Linux")
                             (_ "unknown-Linux"))))
                 (with-directory-excursion "source/c"
                   (invoke "gcc" "-shared" "-O3" "-fPIC" "unix.c"
                           (string-append "-I" jdk "/include")
                           (string-append "-I" jdk "/include/linux")
                           "-o" "libunix.so")
                   (invoke "gcc" "-shared" "-O3" "-fPIC"
                           "-DMACHINE_BYTE_ORDER=1"
                           "copyCommon.c"
                           "copyByteChar.c"
                           "copyByteDouble.c"
                           "copyByteFloat.c"
                           "copyByteInt.c"
                           "copyByteLong.c"
                           "copyByteShort.c"
                           (string-append "-I" jdk "/include")
                           (string-append "-I" jdk "/include/linux")
                           "-o" "libnativedata.so"))
                 (install-file "source/c/libunix.so"
                               (string-append "libs/native/unix/" dir))
                 (install-file "source/c/libnativedata.so"
                               (string-append "libs/native/nativedata/" dir))
                 #t)))
           ;; In the "check" phase we only build the test executable.
           (add-after 'check 'run-tests
             (lambda _
               (invoke "java" "-jar" "targets/dist/sis-base-test.jar")
               (delete-file "targets/dist/sis-base-test.jar")
               #t))
           (replace 'install (install-jars "targets/dist")))))
      (native-inputs
       `(("jdk" ,icedtea-8)
         ("java-commons-lang" ,java-commons-lang)
         ("java-commons-io" ,java-commons-io)
         ("java-testng" ,java-testng)
         ("build-resources"
          ,(origin
             (method svn-fetch)
             (uri (svn-reference
                   (url (string-append "http://svnsis.ethz.ch/repos/cisd/"
                                       "base/tags/release/"
                                       (version-major+minor base-version)
                                       ".x/" base-version
                                       "/build_resources/"))
                   (revision revision)))
             (sha256
              (base32
               "0b6335gkm4x895rac6kfg9d3rpq0sy19ph4zpg2gyw6asfsisjhk"))))))
      (home-page "https://svnsis.ethz.ch")
      (synopsis "Utility classes for libraries from ETH Zurich")
      (description "This library supplies some utility classes needed for
libraries from the SIS division at ETH Zurich like jHDF5.")
      ;; The C sources are under a non-copyleft license, which looks like a
      ;; variant of the BSD licenses.  The whole package is under the ASL2.0.
      (license (list license:asl2.0
                     (license:non-copyleft "file://source/c/COPYING"))))))

(define-public java-cisd-args4j
  (let ((revision 39162)
        (base-version "9.11.2"))
    (package
      (name "java-cisd-args4j")
      (version (string-append base-version "-" (number->string revision)))
      (source (origin
                (method svn-fetch)
                (uri (svn-reference
                      (url (string-append "http://svnsis.ethz.ch/repos/cisd/"
                                          "args4j/tags/release/"
                                          (version-major+minor base-version)
                                          ".x/" base-version "/args4j/"))
                      (revision revision)))
                (file-name (string-append "java-cisd-args4j-" version "-checkout"))
                (sha256
                 (base32
                  "0hhqznjaivq7ips7mkwas78z42s6djsm20rrs7g1zd59rcsakxn2"))))
      (build-system ant-build-system)
      (arguments
       `(#:make-flags '("-file" "build/build.xml")
         #:tests? #f ; there are no tests
         #:modules ((guix build ant-build-system)
                    (guix build utils)
                    (guix build java-utils)
                    (sxml simple)
                    (sxml transform)
                    (sxml xpath))
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'unpack-build-resources
             (lambda* (#:key inputs #:allow-other-keys)
               (mkdir-p "../build_resources")
               (copy-recursively (assoc-ref inputs "build-resources")
                                 "../build_resources")
               (mkdir-p "../build_resources/lib")))
           (add-after 'unpack-build-resources 'fix-dependencies
             (lambda* (#:key inputs #:allow-other-keys)
               ;; FIXME: There should be a more convenient abstraction for
               ;; editing XML files.
               (with-directory-excursion "../build_resources/ant/"
                 (chmod "build-common.xml" #o664)
                 (call-with-output-file "build-common.xml.new"
                   (lambda (port)
                     (sxml->xml
                      (pre-post-order
                       (with-input-from-file "build-common.xml"
                         (lambda _ (xml->sxml #:trim-whitespace? #t)))
                       `(;; Remove dependency on classycle and custom ant tasks
                         (taskdef   . ,(lambda (tag . kids)
                                         (let ((name ((sxpath '(name *text*)) kids)))
                                           (if (or (member "build-info" name)
                                                   (member "dependency-checker" name)
                                                   (member "build-java-subprojects" name)
                                                   (member "project-classpath" name))
                                               '() ; skip
                                               `(,tag ,@kids)))))
                         (typedef   . ,(lambda (tag . kids)
                                         (let ((name ((sxpath '(name *text*)) kids)))
                                           (if (member "recursive-jar" name)
                                               '() ; skip
                                               `(,tag ,@kids)))))
                         (build-java-subprojects . ,(lambda _ '()))
                         ;; Ignore everything else
                         (*default* . ,(lambda (tag . kids) `(,tag ,@kids)))
                         (*text*    . ,(lambda (_ txt) txt))))
                      port)))
                 (rename-file "build-common.xml.new" "build-common.xml"))
               (substitute* "build/build.xml"
                 (("\\$\\{lib\\}/cisd-base/cisd-base.jar")
                  (search-input-file inputs "/share/java/sis-base.jar"))
                 ;; Remove dependency on svn
                 (("<build-info.*") "")
                 (("\\$\\{revision.number\\}")
                  ,(number->string revision))
                 (("\\$\\{version.number\\}") ,base-version)
                 ;; Don't use custom ant tasks.
                 (("recursive-jar") "jar")
                 (("<project-classpath.*") ""))))
           (replace 'install (install-jars "targets/dist")))))
      (inputs
       (list java-cisd-base))
      (native-inputs
       `(("ecj" ,java-ecj)
         ("build-resources"
          ,(origin
             (method svn-fetch)
             (uri (svn-reference
                   (url (string-append "http://svnsis.ethz.ch/repos/cisd/"
                                       "args4j/tags/release/"
                                       (version-major+minor base-version)
                                       ".x/" base-version
                                       "/build_resources/"))
                   (revision revision)))
             (sha256
              (base32
               "056cam4k8pll7ass31sy6gwn8g8719njc41yf4l02b0342nilkyf"))
             (modules '((guix build utils)))
             ;; Delete bundled pre-built jars.
             (snippet
              '(begin (delete-file-recursively "lib/") #t))))))
      (home-page "https://svnsis.ethz.ch")
      (synopsis "Command line parser library")
      (description "This package provides a parser for command line arguments.")
      (license license:asl2.0))))

(define-public java-cisd-jhdf5
  (let ((revision 39162)
        (base-version "14.12.6"))
    (package
      (name "java-cisd-jhdf5")
      (version (string-append base-version "-" (number->string revision)))
      (source (origin
                (method svn-fetch)
                (uri (svn-reference
                      (url (string-append "http://svnsis.ethz.ch/repos/cisd/"
                                          "jhdf5/tags/release/"
                                          (version-major+minor base-version)
                                          ".x/" base-version "/jhdf5/"))
                      (revision revision)))
                (file-name (string-append "java-cisd-jhdf5-" version "-checkout"))
                (sha256
                 (base32
                  "13i17s2hn0q9drdqvp8csy7770p3hdbh9rp30ihln2ldkfawdmz0"))
                (modules '((guix build utils)))
                (snippet
                 '(begin
                    ;; Delete included gradle jar
                    (delete-file-recursively "gradle/wrapper")
                    ;; Delete pre-built native libraries
                    (delete-file-recursively "libs")))))
      (build-system ant-build-system)
      (arguments
       `(#:make-flags '("-file" "build/build.xml")
         #:build-target "jar-all"
         #:test-target "jar-test"
         #:phases
         (modify-phases %standard-phases
           ;; FIXME: this build phase fails.
           (delete 'generate-jar-indices)
           ;; Don't erase results from the build phase when building tests.
           (add-after 'unpack 'separate-test-target-from-clean
             (lambda _
               (substitute* "build/build.xml"
                 (("\"jar-test\" depends=\"clean, ")
                  "\"jar-test\" depends=\""))))
           (add-after 'unpack 'unpack-build-resources
             (lambda* (#:key inputs #:allow-other-keys)
               (copy-recursively (assoc-ref inputs "build-resources")
                                 "../build_resources")
               (delete-file-recursively "../build_resources/lib/")
               (mkdir-p "../build_resources/lib")
               ;; Remove dependency on classycle
               (substitute* "../build_resources/ant/build-common.xml"
                 (("<taskdef name=\"dependency-checker.*") "")
                 (("classname=\"classycle.*") "")
                 (("classpath=\"\\$\\{lib\\}/classycle.*") ""))
               ;; Remove dependency on svn
               (substitute* "build/build.xml"
                 (("<build-info.*") "")
                 (("\\$\\{revision.number\\}")
                  ,(number->string revision))
                 (("\\$\\{version.number\\}") ,base-version))))
           (add-after 'unpack-build-resources 'fix-dependencies
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "../build_resources/ant/build-common.xml"
                 (("../libraries/testng/testng-jdk15.jar")
                  (search-input-file inputs
                                     "/share/java/java-testng.jar")))
               (substitute* "build/build.xml"
                 (("\\$\\{lib\\}/sis-base/sis-base.jar")
                  (search-input-file inputs
                                     "/share/java/sis-base.jar"))
                 (("\\$\\{lib\\}/cisd-args4j/cisd-args4j.jar")
                  (search-input-file inputs
                                     "/share/java/cisd-args4j.jar"))
                 (("\\$\\{lib\\}/commons-lang/commons-lang.jar")
                  (search-input-file inputs
                                     (string-append
                                      "/share/java/commons-lang-"
                                      ,(package-version java-commons-lang)
                                      ".jar")))
                 (("\\$\\{lib\\}/commons-io/commons-io.jar")
                  (search-input-file inputs
                                     (string-append
                                      "/lib/m2/commons-io/commons-io/"
                                      ,(package-version java-commons-io)
                                      "/commons-io-"
                                      ,(package-version java-commons-io)
                                      ".jar")))
                 (("\\$\\{lib\\}/testng/testng-jdk15.jar")
                  (search-input-file inputs
                                     "/share/java/java-testng.jar"))
                 (("\\$\\{lib\\}/junit4/junit.jar")
                  (car (find-files (assoc-ref inputs "java-junit") "jar$")))
                 (("\\$\\{lib\\}/jmock/hamcrest/hamcrest-core.jar")
                  (car (find-files (assoc-ref inputs "java-hamcrest-core")
                                   "jar$"))))
               ;; Remove dependency on ch.rinn.restrictions
               (with-directory-excursion "source/java/ch/systemsx/cisd/hdf5/"
                 (substitute* '("BitSetConversionUtils.java"
                                "HDF5Utils.java")
                   (("import ch.rinn.restrictions.Private;") "")
                   (("@Private") "")))
               (with-directory-excursion "sourceTest/java/ch/systemsx/cisd/hdf5/"
                 (substitute* '("BitSetConversionTest.java"
                                "h5ar/HDF5ArchiverTest.java")
                   (("import ch.rinn.restrictions.Friend;") "")
                   (("@Friend.*") ""))
                 ;; Remove leftovers from removing @Friend
                 (substitute* "h5ar/HDF5ArchiverTest.java"
                   (("\\{ HDF5Archiver.class, IdCache.class, LinkRecord.class \\}\\)")
                    "")))))
           (add-before 'configure 'build-native-library
             (lambda* (#:key inputs #:allow-other-keys)
               (let ((jdk  (assoc-ref inputs "jdk"))
                     (hdf5 (assoc-ref inputs "hdf5"))
                     (dir  ,(match (%current-system)
                              ("i686-linux"
                               "i386-Linux")
                              ((or "armhf-linux" "aarch64-linux")
                               "arm-Linux")
                              ((or "x86_64-linux")
                               "amd64-Linux")
                              (_ "unknown-Linux"))))
                 (with-directory-excursion "source/c"
                   (apply invoke `("gcc" "-shared" "-O3"
                                   "-fPIC"
                                   "-Wl,--exclude-libs,ALL"
                                   ,@(find-files "jhdf5" "\\.c$")
                                   ,@(find-files "hdf-java" "\\.c$")
                                   ,(string-append "-I" hdf5 "/include")
                                   ,(string-append "-I" jdk "/include")
                                   ,(string-append "-I" jdk "/include/linux")
                                   ,(string-append hdf5 "/lib/libhdf5.a")
                                   "-o" "libjhdf5.so" "-lz")))
                 (install-file "source/c/libjhdf5.so"
                               (string-append "libs/native/jhdf5/" dir)))))
           ;; In the "check" phase we only build the test executable.
           (add-after 'check 'run-tests
             (lambda _
               (invoke "java" "-jar" "targets/dist/sis-jhdf5-test.jar")
               (delete-file "targets/dist/sis-jhdf5-test.jar")))
           (replace 'install
             (install-jars "targets/dist")))))
      (inputs
       (list java-cisd-base
             java-cisd-args4j
             java-commons-lang
             java-commons-io
             hdf5-1.8
             zlib))
      (native-inputs
       `(("jdk" ,icedtea-8)
         ("java-testng" ,java-testng)
         ("java-junit" ,java-junit)
         ("java-jmock" ,java-jmock)
         ("java-hamcrest-core" ,java-hamcrest-core)
         ("build-resources"
          ,(origin
             (method svn-fetch)
             (uri (svn-reference
                   (url (string-append "http://svnsis.ethz.ch/repos/cisd/"
                                       "jhdf5/tags/release/"
                                       (version-major+minor base-version)
                                       ".x/" base-version
                                       "/build_resources/"))
                   (revision revision)))
             (sha256
              (base32
               "0b6335gkm4x895rac6kfg9d3rpq0sy19ph4zpg2gyw6asfsisjhk"))))))
      (home-page "https://wiki-bsse.ethz.ch/display/JHDF5/")
      (synopsis "Java binding for HDF5")
      (description "JHDF5 is a high-level API in Java for reading and writing
HDF5 files, building on the libraries provided by the HDF Group.")
      ;; The C sources are under a non-copyleft license, which looks like a
      ;; variant of the BSD licenses.  The whole package is under the ASL2.0.
      (license (list license:asl2.0
                     (license:non-copyleft "file://source/c/COPYING"))))))

(define-public java-classpathx-servletapi
  (package
    (name "java-classpathx-servletapi")
    (version "3.0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/classpathx/servletapi/"
                                  "servletapi-" version ".tar.gz"))
              (sha256
               (base32
                "07d8h051siga2f33fra72hk12sbq1bxa4jifjg0qj0vfazjjff0x"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f ; there is no test target
       #:build-target "compile"
       #:make-flags
       ,#~(list "-Dbuild.compiler=javac1.8"
                (string-append "-Ddist=" #$output))
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (lambda* (#:key make-flags #:allow-other-keys)
             (apply invoke `("ant" "dist" ,@make-flags)))))))
    (home-page "https://www.gnu.org/software/classpathx/")
    (synopsis "Java servlet API implementation")
    (description "This is the GNU servlet API distribution, part of the
ClasspathX project.  It provides implementations of version 3.0 of the servlet
API and version 2.1 of the Java ServerPages API.")
    (license license:gpl3+)))

(define-public java-javaee-servletapi
  (package
    (name "java-javaee-servletapi")
    (version "3.1.0")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/javaee/servlet-spec")
                      (commit version)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0s03lj8w5an70lkqbjycgfrfk0kc07vbfav91jzk87gh3awf9ksl"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "javax-servletapi.jar"
       ;; no tests
       #:tests? #f
       #:source-dir "src/main/java"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-resources
           (lambda _
             (mkdir-p "build/classes/javax/servlet/http")
             (let ((from-prefix "src/main/java/javax/servlet/")
                   (to-prefix "build/classes/javax/servlet/"))
               (for-each (lambda (f)
                           (copy-file (string-append from-prefix f)
                                      (string-append to-prefix f)))
                         (list "LocalStrings_ja.properties"
                               "LocalStrings.properties"
                               "LocalStrings_fr.properties"
                               "http/LocalStrings_es.properties"
                               "http/LocalStrings_ja.properties"
                               "http/LocalStrings.properties"
                               "http/LocalStrings_fr.properties")))
               #t)))))
    (native-inputs
     (list unzip))
    (home-page "https://javaee.github.io/servlet-spec/")
    (synopsis "Java servlet API")
    (description "Java Servlet is the foundation web specification in the
Java Enterprise Platform.  Developers can build web applications using the
Servlet API to interact with the request/response workflow.  This project
provides information on the continued development of the Java Servlet
specification.")
    ;; Main code is dual-licensed by Oracle under either GLP2 or CDDL 1.1.
    ;; Some files are licensed under ASL 2.0.
    (license (list license:asl2.0 license:gpl2 license:cddl1.1))))

(define-public java-swt
  (package
    (name "java-swt")
    (version "4.7.1a")
    (source
     ;; The types of many variables and procedures differ in the sources
     ;; dependent on whether the target architecture is a 32-bit system or a
     ;; 64-bit system.  Instead of patching the sources on demand in a build
     ;; phase we download either the 32-bit archive (which mostly uses "int"
     ;; types) or the 64-bit archive (which mostly uses "long" types).
     (let ((hash32 "09q0cbh90d90q3a7dx9430kc4m6bijrkr4lajrmzzvi0jjdpq4v9")
           (hash64 "17k5hs75a87ssmc5xhwdfdm2gn4zba0r837l2455za01krnkaa2q")
           (file32 "x86")
           (file64 "x86_64"))
       (let-values (((hash file)
                     (match (or (%current-target-system) (%current-system))
                       ("x86_64-linux" (values hash64 file64))
                       (_              (values hash32 file32)))))
         (origin
           (method url-fetch)
           (uri (string-append
                 "https://archive.eclipse.org/eclipse/downloads/drops4/"
                 "R-" version "-201710090410/swt-" version
                 "-gtk-linux-" file ".zip"))
           (sha256 (base32 hash))))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "swt.jar"
       #:jdk ,icedtea-8
       #:tests? #f ; no "check" target
       #:phases
       (modify-phases %standard-phases
         (replace 'unpack
           (lambda* (#:key source #:allow-other-keys)
             (mkdir "swt")
             (invoke "unzip" source "-d" "swt")
             (chdir "swt")
             (mkdir "src")
             (invoke "unzip" "src.zip" "-d" "src")))
         ;; The classpath contains invalid icecat jars.  Since we don't need
         ;; anything other than the JDK on the classpath, we can simply unset
         ;; it.
         (add-after 'configure 'unset-classpath
           (lambda _ (unsetenv "CLASSPATH") #t))
         (add-before 'build 'build-native
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((lib (string-append (assoc-ref outputs "out") "/lib")))
               ;; Build shared libraries.  Users of SWT have to set the system
               ;; property swt.library.path to the "lib" directory of this
               ;; package output.
               (mkdir-p lib)
               (setenv "OUTPUT_DIR" lib)
               (setenv "CC" ,(cc-for-target))
               (with-directory-excursion "src"
                 (invoke "bash" "build.sh")))))
         (add-after 'install 'install-native
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((lib (string-append (assoc-ref outputs "out") "/lib")))
               (for-each (lambda (file)
                           (install-file file lib))
                         (find-files "." "\\.so$"))
               #t))))))
    (inputs
     `(("gtk" ,gtk+-2)
       ("libxtst" ,libxtst)
       ("libxt" ,libxt)
       ("mesa" ,mesa)
       ("glu" ,glu)))
    (native-inputs
     (list pkg-config unzip))
    (home-page "https://www.eclipse.org/swt/")
    (synopsis "Widget toolkit for Java")
    (description
     "SWT is a widget toolkit for Java designed to provide efficient, portable
access to the user-interface facilities of the operating systems on which it
is implemented.")
    ;; SWT code is licensed under EPL1.0
    ;; Gnome and Gtk+ bindings contain code licensed under LGPLv2.1
    ;; Cairo bindings contain code under MPL1.1
    ;; XULRunner 1.9 bindings contain code under MPL2.0
    (license (list
              license:epl1.0
              license:mpl1.1
              license:mpl2.0
              license:lgpl2.1+))))

;; java-hamcrest-core uses qdox version 1.12.  We package this version instead
;; of the latest release.
(define-public java-qdox-1.12
  (package
    (name "java-qdox")
    (version "1.12.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://repo1.maven.org/maven2/"
                                  "com/thoughtworks/qdox/qdox/" version
                                  "/qdox-" version "-sources.jar"))
              (sha256
               (base32
                "0hlfbqq2avf5s26wxkksqmkdyk6zp9ggqn37c468m96mjv0n9xfl"))))
    (build-system ant-build-system)
    (arguments
     `(;; Tests require junit
       #:tests? #f
       #:jar-name "qdox.jar"
       #:phases
       (modify-phases %standard-phases
         (replace 'unpack
           (lambda* (#:key source #:allow-other-keys)
             (mkdir "src")
             (with-directory-excursion "src"
               (invoke "jar" "-xf" source))))
         ;; At this point we don't have junit, so we must remove the API
         ;; tests.
         (add-after 'unpack 'delete-tests
           (lambda _
             (delete-file-recursively "src/com/thoughtworks/qdox/junit")
             #t)))))
    (home-page "https://github.com/codehaus/qdox")
    (synopsis "Parse definitions from Java source files")
    (description
     "QDox is a high speed, small footprint parser for extracting
class/interface/method definitions from source files complete with JavaDoc
@code{@@tags}.  It is designed to be used by active code generators or
documentation tools.")
    (license license:asl2.0)))

(define-public java-qdox
  (package
    (name "java-qdox")
    ; Newer version exists, but this version is required by java-plexus-component-metadata
    (version "2.0-M2")
    (source (origin
              (method url-fetch)
              ;; 2.0-M4, -M5 at https://github.com/paul-hammant/qdox
              ;; Older releases at https://github.com/codehaus/qdox/
              ;; Note: The release at maven is pre-generated. The release at
              ;; github requires jflex.
              (uri (string-append "https://repo1.maven.org/maven2/"
                                  "com/thoughtworks/qdox/qdox/" version
                                  "/qdox-" version "-sources.jar"))
              (sha256
               (base32
                "10xxrcaicq6axszcr2jpygisa4ch4sinyx5q7kqqxv4lknrmxp5x"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "qdox.jar"
       #:tests? #f; no tests
       #:phases
       (modify-phases %standard-phases
         (add-before 'install 'create-pom
           (generate-pom.xml "pom.xml" "com.thoughtworks.qdox" "qdox" ,version
                             #:name "QDox"))
         (replace 'install
           (install-from-pom "pom.xml")))))
    (home-page "https://github.com/codehaus/qdox")
    (synopsis "Parse definitions from Java source files")
    (description "QDox is a high speed, small footprint parser for extracting
class/interface/method definitions from source files complete with JavaDoc
@code{@@tags}.  It is designed to be used by active code generators or
documentation tools.")
    (license license:asl2.0)))

(define-public java-qdox-2-M9
  (package
    (inherit java-qdox)
    (version "2.0-M9"); required by plexus-java
    (source (origin
              (method url-fetch)
              ;; 2.0-M4, -M5 at https://github.com/paul-hammant/qdox
              ;; Older releases at https://github.com/codehaus/qdox/
              ;; Note: The release at maven is pre-generated. The release at
              ;; github requires jflex.
              (uri (string-append "https://repo1.maven.org/maven2/"
                                  "com/thoughtworks/qdox/qdox/" version
                                  "/qdox-" version "-sources.jar"))
              (sha256
               (base32
                "1s2jnmx2dkwnaha12lcj26aynywgwa8sslc47z82wx8xai13y4fg"))))
    (arguments
     (substitute-keyword-arguments (package-arguments java-qdox)
       ((#:phases phases)
        `(modify-phases ,phases
           (replace 'create-pom
             (generate-pom.xml "pom.xml" "com.thoughtworks.qdox" "qdox" ,version
                               #:name "QDox"))))))))

(define-public java-jarjar
  (package
    (name "java-jarjar")
    (version "1.4")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://storage.googleapis.com/google-code-archive-downloads/v2/"
                    "code.google.com/jarjar/jarjar-src-" version ".zip"))
              (sha256
               (base32
                "1v8irhni9cndcw1l1wxqgry013s2kpj0qqn57lj2ji28xjq8ndjl"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Delete bundled thirds-party jar archives.
                  (delete-file-recursively "lib")
                  (delete-file "src/test/enumtest.jar")
                  ;; the CLASSPATH needs this directory, create an empty one
                  (mkdir-p "lib")
                  #t))))
    (build-system ant-build-system)
    (arguments
     `(;; Tests require junit, which ultimately depends on this package.
       #:tests? #f
       #:build-target "jar"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'do-not-use-bundled-asm
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "build.xml"
               (("<path id=\"path.build\">")
                (string-append "<path id=\"path.build\"><fileset dir=\""
                               (assoc-ref inputs "java-asm-bootstrap")
                               "/lib/m2\" includes=\"**/*.jar\"/>"))
               (("<zipfileset src=\"lib/asm-4.0.jar\"/>") "")
               (("lib/asm-commons-4.0.jar")
                (car (find-files (assoc-ref inputs "java-asm-bootstrap")
                                 "asm-6.0.jar")))
               (("<include name=\"org/objectweb/asm/commons/Remap\\*\\.class\"/>")
                (string-append "<include name=\"org/objectweb/asm/"
                               "commons/Remap*.class\"/>"
                               "<include name=\"org/objectweb/asm/*.class\"/>"
                               "<include name=\"org/objectweb/asm/"
                               "signature/*.class\"/>"
                               "<include name=\"org/objectweb/asm/"
                               "commons/SignatureRemapper.class\"/>")))
             #t))
         (add-before 'build 'remove-maven-dependency
           (lambda _
             ;; We do not need to build jarjar as a maven plugin just yet, so
             ;; remove this file.  Maven requires jarjar (but not that plugin),
             ;; so removing it improves the bootstrap.
             (delete-file "src/main/com/tonicsystems/jarjar/JarJarMojo.java")
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((target (string-append (assoc-ref outputs "out")
                                          "/share/java")))
               (install-file (string-append "dist/jarjar-" ,version ".jar")
                             target))
             #t)))))
    (inputs
     (list java-asm-bootstrap))
    (native-inputs
     (list unzip))
    (home-page "https://code.google.com/archive/p/jarjar/")
    (synopsis "Repackage Java libraries")
    (description
     "Jar Jar Links is a utility that makes it easy to repackage Java
libraries and embed them into your own distribution.  Jar Jar Links includes
an Ant task that extends the built-in @code{jar} task.")
    (license license:asl2.0)))

(define-public java-hamcrest-core
  (package
    (name "java-hamcrest-core")
    (version "1.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/hamcrest/JavaHamcrest/")
                     (commit (string-append "hamcrest-java-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "16fxxkrd31ahqvcaby30jgh3z1i0zxh51m24hxgz0z2agxj6bc63"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Delete bundled thirds-party jar archives.
                  (delete-file-recursively "lib")
                  #t))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f ; Tests require junit
       #:modules ((guix build ant-build-system)
                  (guix build java-utils)
                  (guix build utils)
                  (srfi srfi-1))
       #:make-flags (list (string-append "-Dversion=" ,version))
       #:test-target "unit-test"
       #:build-target "core"
       #:phases
       (modify-phases %standard-phases
         ;; Disable unit tests, because they require junit, which requires
         ;; hamcrest-core.  We also give a fixed value to the "Built-Date"
         ;; attribute from the manifest for reproducibility.
         (add-before 'configure 'patch-build.xml
           (lambda _
             (substitute* "build.xml"
               (("unit-test, ") "")
               (("\\$\\{build.timestamp\\}") "guix"))
             #t))
         ;; Java's "getMethods()" returns methods in an unpredictable order.
         ;; To make the output of the generated code deterministic we must
         ;; sort the array of methods.
         (add-after 'unpack 'make-method-order-deterministic
           (lambda _
             (substitute* "hamcrest-generator/src/main/java/org/hamcrest/generator/ReflectiveFactoryReader.java"
               (("import java\\.util\\.Iterator;" line)
                (string-append line "\n"
                               "import java.util.Arrays; import java.util.Comparator;"))
               (("allMethods = cls\\.getMethods\\(\\);" line)
                (string-append "_" line
                               "
private Method[] getSortedMethods() {
  Arrays.sort(_allMethods, new Comparator<Method>() {
    @Override
    public int compare(Method a, Method b) {
      return a.toString().compareTo(b.toString());
    }
  });
  return _allMethods;
}

private Method[] allMethods = getSortedMethods();")))
             #t))
         (add-before 'build 'do-not-use-bundled-qdox
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "build.xml"
               (("lib/generator/qdox-1.12.jar")
                (string-append (assoc-ref inputs "java-qdox-1.12")
                               "/share/java/qdox.jar")))
             #t))
         ;; build.xml searches for .jar files in this directoy, which
         ;; we remove  from the source archive.
         (add-before 'build 'create-dummy-directories
           (lambda _
             (mkdir-p "lib/integration")
             #t))
         (add-before 'build 'create-pom
           (lambda _
             (substitute* "pom/hamcrest-core.pom"
               (("@VERSION@") ,version))
             #t))
         (replace 'install
           (install-from-pom "pom/hamcrest-core.pom")))))
    (native-inputs
     `(("java-qdox-1.12" ,java-qdox-1.12)
       ("java-jarjar" ,java-jarjar)))
    (propagated-inputs
     (list java-hamcrest-parent-pom))
    (home-page "https://hamcrest.org/")
    (synopsis "Library of matchers for building test expressions")
    (description
     "This package provides a library of matcher objects (also known as
constraints or predicates) allowing @code{match} rules to be defined
declaratively, to be used in other frameworks.  Typical scenarios include
testing frameworks, mocking libraries and UI validation rules.")
    (license license:bsd-2)))

(define-public java-hamcrest-parent-pom
  (package
    (inherit java-hamcrest-core)
    (properties '((hidden? . #t)))
    (name "java-hamcrest-parent-pom")
    (propagated-inputs '())
    (native-inputs '())
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'build
           (lambda _
             (substitute* "pom/hamcrest-parent.pom"
               (("@VERSION@") ,(package-version java-hamcrest-core)))
             #t))
         (replace 'install
           (install-pom-file "pom/hamcrest-parent.pom")))))))

(define-public java-hamcrest-library
  (package
    (inherit java-hamcrest-core)
    (name "java-hamcrest-library")
    (arguments
     (substitute-keyword-arguments (package-arguments java-hamcrest-core)
      ((#:build-target _) "library")
      ((#:phases phases)
       `(modify-phases ,phases
          (add-after 'unpack 'patch-classpath-for-integration
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "build.xml"
                (("build/hamcrest-core-\\$\\{version\\}\\.jar")
                 (car (find-files (assoc-ref inputs "java-hamcrest-core")
                                  "jar$"))))
              #t))
          (replace 'create-pom
            (lambda _
             (substitute* "pom/hamcrest-library.pom"
               (("@VERSION@") ,(package-version java-hamcrest-core)))
              #t))
          (replace 'install
            (install-from-pom "pom/hamcrest-library.pom"))))))
    (propagated-inputs
     (list java-hamcrest-core java-hamcrest-parent-pom))))

(define-public java-junit
  (package
    (name "java-junit")
    (version "4.12")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/junit-team/junit/")
                     (commit (string-append "r" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1j8avi91px1z8rjc89cfikwrvfifdmmsarwiyrcnr59ynvpz0v8h"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Delete bundled jar archives.
                  (delete-file-recursively "lib")
                  #t))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "junit.jar"
       #:source-dir "src/main/java"
       #:test-dir "src/test"
       #:test-exclude (list "**/SimpleTest.java" "**/StackTracesTest.java"
                            "**/RuleChainTest.java" "**/TestWatchmanTest.java")
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'copy-test-resources
           (lambda _
             (copy-recursively "src/test/resources" "build/test-classes")
             #t))
         (replace 'install
           (install-from-pom "pom.xml")))))
    (propagated-inputs
     (list java-hamcrest-core))
    (native-inputs
     (list java-hamcrest-library))
    (home-page "https://junit.org/junit4/")
    (synopsis "Test framework for Java")
    (description
     "JUnit is a simple framework to write repeatable tests for Java projects.
JUnit provides assertions for testing expected results, test fixtures for
sharing common test data, and test runners for running tests.")
    (license license:epl1.0)))

(define-public java-junitparams
  (package
    (name "java-junitparams")
    (version "1.1.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/Pragmatists/JUnitParams")
                     (commit (string-append "JUnitParams-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0rb52xdfp99invyjrras3w0bf0a81cz30yd47rkkiyqcqj0y1q9b"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "junitparams.jar"
       #:source-dir "src/main/java"
       #:test-dir "src/test"
       #:test-exclude (list "**/SuperclassTest.java")))
    (inputs
     (list java-junit))
    (native-inputs
     (list java-junit java-hamcrest-core java-assertj))
    (home-page "https://github.com/Pragmatists/JUnitParams")
    (synopsis "Parameterised test support for JUnit")
    (description "The JUnitParams project adds a new runner to JUnit and
provides much easier and readable parametrised tests for JUnit.")
    (license license:asl2.0)))

(define-public java-plexus-utils
  (package
    (name "java-plexus-utils")
    (version "3.3.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/codehaus-plexus/plexus-utils")
                     (commit (string-append "plexus-utils-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0d0fq21rzjy0j55kcp8w9k1rbq9rwr0r7cc8239p9jbz54vihp0g"))))
    (build-system ant-build-system)
    ;; FIXME: The default build.xml does not include a target to install
    ;; javadoc files.
    (arguments
     `(#:jar-name "plexus-utils.jar"
       #:source-dir "src/main"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-reference-to-/bin-and-/usr
           (lambda _
             (substitute* "src/main/java/org/codehaus/plexus/util/\
cli/shell/BourneShell.java"
               (("/bin/sh") (which "sh"))
               (("/usr/")   (getcwd)))
             #t))
         (add-after 'unpack 'fix-or-disable-broken-tests
           (lambda _
             (with-directory-excursion "src/test/java/org/codehaus/plexus/util"
               (substitute* '("cli/CommandlineTest.java"
                              "cli/shell/BourneShellTest.java")
                 (("/bin/sh")   (which "sh"))
                 (("/bin/echo") (which "echo")))

               ;; This test depends on MavenProjectStub, but we don't have
               ;; a package for Maven.
               (delete-file "introspection/ReflectionValueExtractorTest.java")

               ;; FIXME: The command line tests fail, maybe because they use
               ;; absolute paths.
               (delete-file "cli/CommandlineTest.java")

               ;; These tests require openjdk jmh, which is not packaged yet
               (for-each delete-file (find-files "." "PerfTest.java$")))
             #t))
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "src/main/resources" "build/classes")
             #t))
         (replace 'install (install-from-pom "pom.xml")))))
    (native-inputs
     (list java-hamcrest-core java-junit))
    (propagated-inputs
     (list plexus-parent-pom-5.1))
    (home-page "https://codehaus-plexus.github.io/plexus-utils/")
    (synopsis "Common utilities for the Plexus framework")
    (description "This package provides various Java utility classes for the
Plexus framework to ease working with strings, files, command lines, XML and
more.")
    (license license:asl2.0)))

(define-public java-plexus-utils-3.2.1
  (package
    (inherit java-plexus-utils)
    ;; plexus-build-api needs this version, later versions don't work
    (version "3.2.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/codehaus-plexus/plexus-utils")
                     (commit (string-append "plexus-utils-" version))))
              (file-name (git-file-name "java-plexus-utils" version))
              (sha256
               (base32
                "1w169glixyk94jbczj8jzg897lsab46jihiaa3dhw0p06g35va8b"))))))

(define-public java-plexus-interpolation
  (package
    (name "java-plexus-interpolation")
    (version "1.26")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/codehaus-plexus/plexus-interpolation")
                     (commit (string-append "plexus-interpolation-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1rahjmhywf6d5m32qzlc9izawyvcd71abfm9k03f13rs2xmfxzlh"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "plexus-interpolation.jar"
       #:source-dir "src/main"
       #:phases
       (modify-phases %standard-phases
         (replace 'install (install-from-pom "pom.xml")))))
    (propagated-inputs
     `(("plexus-parent-pom-5.1" ,plexus-parent-pom-5.1)))
    (native-inputs
     (list java-junit java-hamcrest-core))
    (home-page "https://codehaus-plexus.github.io/plexus-interpolation/")
    (synopsis "Java components for interpolating ${} strings and the like")
    (description "Plexus interpolator is a modular, flexible interpolation
framework for the expression language style commonly seen in Maven, Plexus,
and other related projects.

It has its foundation in the @code{org.codehaus.plexus.utils.interpolation}
package within @code{plexus-utils}, but has been separated in order to allow
these two libraries to vary independently of one another.")
    (license license:asl2.0)))

(define-public java-plexus-classworlds
  (package
    (name "java-plexus-classworlds")
    (version "2.6.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/codehaus-plexus/plexus-classworlds")
                     (commit (string-append "plexus-classworlds-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "034k2hgvj1saalwbnzrbg4n0zgzwcpz1lhlb8q4kgglsp9pmm03s"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "plexus-classworlds.jar"
       #:source-dir "src/main"
       #:tests? #f;; FIXME: we need to generate some resources as in pom.xml
       #:phases
       (modify-phases %standard-phases
         (replace 'install (install-from-pom "pom.xml")))))
    (propagated-inputs
     `(("plexus-parent-pom-5.1" ,plexus-parent-pom-5.1)))
    (native-inputs
     (list java-junit))
    (home-page "https://codehaus-plexus.github.io/plexus-classworlds/")
    (synopsis "Java class loader framework")
    (description "Plexus classworlds replaces the native @code{ClassLoader}
mechanism of Java.  It is especially useful for dynamic loading of application
components.")
    (license license:asl2.0)))

(define java-plexus-container-default-bootstrap
  (package
    (name "java-plexus-container-default-bootstrap")
    (version "2.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/codehaus-plexus/plexus-containers")
                     (commit (string-append "plexus-containers-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0r9yq67c1hvi1pz5wmx6x6hk5fmavp8a7yal3j5hkaad757firn1"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "container-default.jar"
       #:source-dir "plexus-container-default/src/main/java"
       #:test-dir "plexus-container-default/src/test"
       #:tests? #f; requires plexus-archiver, which depends on this package
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'fix-google-collections
           (lambda _
             ;; Google collections are now replaced with guava
             (substitute* "plexus-container-default/pom.xml"
               (("google-collections") "guava")
               (("com.google.collections") "com.google.guava"))
             #t))
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively
               "plexus-container-default/src/main/resources/"
               "build/classes")
             #t))
         (replace 'install
           (install-from-pom "plexus-container-default/pom.xml")))))
    (propagated-inputs
     `(("java-plexus-worldclass" ,java-plexus-classworlds)
       ("java-geronimo-xbean-reflect" ,java-geronimo-xbean-reflect)
       ("java-plexus-utils" ,java-plexus-utils)
       ("java-junit" ,java-junit)
       ("java-guava" ,java-guava)
       ("java-plexus-containers-parent-pom" ,java-plexus-containers-parent-pom)))
    (home-page "https://github.com/codehaus-plexus/plexus-containers")
    (synopsis "Inversion-of-control container")
    (description "Plexus-default-container is Plexus' inversion-of-control
(@dfn{IoC}) container.  It is composed of its public API and its default
implementation.")
    (license license:asl2.0)))

(define java-plexus-containers-parent-pom
  (package
    (inherit java-plexus-container-default-bootstrap)
    (name "java-plexus-containers-parent-pom")
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (replace 'install
           (install-pom-file "pom.xml")))))
    (propagated-inputs
     `(("plexus-parent-pom" ,plexus-parent-pom-4.0)))))

(define-public java-plexus-io
  (package
    (name "java-plexus-io")
    (version "3.2.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/codehaus-plexus/plexus-io")
                     (commit (string-append "plexus-io-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1r3wqfpbxq8vp4p914i8p88r0994rmcjw02hz14n11cfb6gsyvlr"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "plexus-io.jar"
       #:source-dir "src/main/java"
       #:test-dir "src/test"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-resources
           (lambda _
             (mkdir-p "build/classes")
             (copy-recursively "src/main/resources" "build/classes")
             (mkdir-p "build/test-classes")
             (copy-recursively "src/test/resources" "build/test-classes")
             #t))
         (replace 'install (install-from-pom "pom.xml")))))
    (propagated-inputs
     (list java-plexus-utils java-commons-io plexus-parent-pom-5.1))
    (inputs
     (list java-jsr305))
    (native-inputs
     `(("junit" ,java-junit)
       ("hamcrest" ,java-hamcrest-core)
       ("guava" ,java-guava)
       ("classworlds" ,java-plexus-classworlds)
       ("xbean" ,java-geronimo-xbean-reflect)
       ("container-default" ,java-plexus-container-default-bootstrap)))
    (home-page "https://github.com/codehaus-plexus/plexus-io")
    (synopsis "I/O plexus components")
    (description "Plexus IO is a set of plexus components, which are designed
for use in I/O operations.  This implementation using plexus components allows
reusing it in maven.")
    (license license:asl2.0)))

(define-public java-plexus-archiver
  (package
    (name "java-plexus-archiver")
    (version "4.2.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/codehaus-plexus/plexus-archiver"
                                  "/archive/plexus-archiver-" version ".tar.gz"))
              (sha256
               (base32
                "144n971r3lfrx3l12nf2scm80x4xdvgbkk4bjpa4vcvvdrll6qys"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "plexus-archiver.jar"
       #:source-dir "src/main/java"
       #:test-dir "src/test"
       #:test-exclude (list "**/Abstract*.java" "**/Base*.java")
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'remove-failing
           (lambda _
             ;; Requires an older version of plexus container
             (delete-file
              "src/test/java/org/codehaus/plexus/archiver/DuplicateFilesTest.java")
             #t))
         (add-before 'check 'fix-test-building
           (lambda _
             (substitute* "build.xml"
               (("srcdir=\"src/test\"") "srcdir=\"src/test/java\""))
             #t))
         (add-before 'build 'copy-resources
           (lambda _
             (mkdir-p "build/classes")
             (copy-recursively "src/main/resources" "build/classes")
             (mkdir-p "build/test-classes")
             (copy-recursively "src/test/resources" "build/test-classes")
             #t))
         (replace 'install (install-from-pom "pom.xml")))))
    (propagated-inputs
     (list java-plexus-utils java-plexus-io java-iq80-snappy
           java-commons-compress plexus-parent-pom-6.1))
    (inputs
     `(("java-jsr305" ,java-jsr305)
       ("java-plexus-container-default"
        ,java-plexus-container-default-bootstrap)))
    (native-inputs
     `(("java-hamcrest-core" ,java-hamcrest-core)
       ("junit" ,java-junit)
       ("classworld" ,java-plexus-classworlds)
       ("xbean" ,java-geronimo-xbean-reflect)
       ("xz" ,java-xz)
       ("guava" ,java-guava)))
    (home-page "https://github.com/codehaus-plexus/plexus-archiver")
    (synopsis "Archiver component of the Plexus project")
    (description "Plexus-archiver contains a component to deal with project
archives (jar).")
    (license license:asl2.0)))

(define-public java-plexus-container-default
  (package
    (inherit java-plexus-container-default-bootstrap)
    (name "java-plexus-container-default")
    (arguments
     `(#:jar-name "container-default.jar"
       #:source-dir "plexus-container-default/src/main/java"
       #:test-dir "plexus-container-default/src/test"
       #:test-exclude (list ;"**/*Test.java"
                            "**/Abstract*.java"
                            ;; Requires plexus-hierarchy
                            "**/PlexusHierarchyTest.java"
                            ;; Failures
                            "**/ComponentRealmCompositionTest.java"
                            "**/PlexusContainerTest.java")
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'fix-google-collections
           (lambda _
             ;; Google collections are now replaced with guava
             (substitute* "plexus-container-default/pom.xml"
               (("google-collections") "guava")
               (("com.google.collections") "com.google.guava"))
             #t))
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively
               "plexus-container-default/src/main/resources/"
               "build/classes")
             #t))
         (add-before 'check 'fix-paths
           (lambda _
             (let ((dir "plexus-container-default/src/test/java/org/codehaus"))
               (substitute*
                 (string-append
                   dir "/plexus/component/composition/"
                   "ComponentRealmCompositionTest.java")
                 (("src/test") "plexus-container-default/src/test"))
               #t)))
         (replace 'install
           (install-from-pom "plexus-container-default/pom.xml")))))
    (inputs
     `(("worldclass" ,java-plexus-classworlds)
       ("xbean" ,java-geronimo-xbean-reflect)
       ("utils" ,java-plexus-utils)
       ("junit" ,java-junit)
       ("guava" ,java-guava)))
    (native-inputs
     `(("archiver" ,java-plexus-archiver)
       ("hamcrest" ,java-hamcrest-core)))))

(define-public java-plexus-component-annotations
  (package
    (inherit java-plexus-container-default)
    (name "java-plexus-component-annotations")
    (arguments
     `(#:jar-name "plexus-component-annotations.jar"
       #:source-dir "plexus-component-annotations/src/main/java"
       #:tests? #f; no tests
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (install-from-pom "plexus-component-annotations/pom.xml")))))
    (propagated-inputs
     (list java-plexus-containers-parent-pom))
    (inputs '())
    (native-inputs '())
    (synopsis "Plexus descriptors generator")
    (description "This package is a Maven plugin to generate Plexus descriptors
from source tags and class annotations.")))

(define-public java-plexus-component-metadata
  (package
    (inherit java-plexus-container-default)
    (name "java-plexus-component-metadata")
    (arguments
     `(#:jar-name "plexus-component-metadata.jar"
       #:source-dir "src/main/java"
       #:test-dir "src/test"
       #:jdk ,icedtea-8
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'chdir
           (lambda _
             (chdir "plexus-component-metadata")
             #t))
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "src/main/resources"
                               "build/classes/")
             #t))
         (add-before 'build 'fix-jdom
           (lambda _
             ;; The newer version of jdom now sets multiple features by default
             ;; that are not supported.
             ;; Skip these features
             (substitute* "src/main/java/org/codehaus/plexus/metadata/merge/MXParser.java"
               (("throw new XmlPullParserException\\(\"unsupporte feature \"\\+name\\);")
                "// skip"))))
         (add-before 'build 'reinstate-cli
           ;; The CLI was removed in 2.1.0, but we still need it to build some
           ;; maven dependencies, and some parts of maven itself. We can't use
           ;; the maven plugin for that yet.
           (lambda _
             (with-output-to-file "src/main/java/org/codehaus/plexus/metadata/PlexusMetadataGeneratorCli.java"
               (lambda _
                 ;; Copied from a previous version of this package.
                 (display "package org.codehaus.plexus.metadata;

import java.io.File;
import java.util.Arrays;
import java.util.Collections;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.OptionBuilder;
import org.apache.commons.cli.Options;
import org.codehaus.plexus.PlexusContainer;
import org.codehaus.plexus.tools.cli.AbstractCli;

public class PlexusMetadataGeneratorCli
    extends AbstractCli
{
    public static final char SOURCE_DIRECTORY = 's';
    public static final char SOURCE_ENCODING = 'e';
    public static final char CLASSES_DIRECTORY = 'c';
    public static final char OUTPUT_FILE = 'o';
    public static final char DESCRIPTORS_DIRECTORY = 'm';

    public static void main( String[] args )
        throws Exception
    {
        new PlexusMetadataGeneratorCli().execute( args );
    }

    @Override
    public String getPomPropertiesPath()
    {
        return \"META-INF/maven/org.codehaus.plexus/plexus-metadata-generator/pom.properties\";
    }

    @Override
    @SuppressWarnings(\"static-access\")
    public Options buildCliOptions( Options options )
    {
        options.addOption( OptionBuilder.withLongOpt( \"source\" ).hasArg().withDescription( \"Source directory.\" ).create( SOURCE_DIRECTORY ) );
        options.addOption( OptionBuilder.withLongOpt( \"encoding\" ).hasArg().withDescription( \"Source file encoding.\" ).create( SOURCE_ENCODING ) );
        options.addOption( OptionBuilder.withLongOpt( \"classes\" ).hasArg().withDescription( \"Classes directory.\" ).create( CLASSES_DIRECTORY ) );
        options.addOption( OptionBuilder.withLongOpt( \"output\" ).hasArg().withDescription( \"Output directory.\" ).create( OUTPUT_FILE ) );
        options.addOption( OptionBuilder.withLongOpt( \"descriptors\" ).hasArg().withDescription( \"Descriptors directory.\" ).create( DESCRIPTORS_DIRECTORY ) );
        return options;
    }

    public void invokePlexusComponent( CommandLine cli, PlexusContainer plexus )
        throws Exception
    {
        MetadataGenerator metadataGenerator = plexus.lookup( MetadataGenerator.class );

        MetadataGenerationRequest request = new MetadataGenerationRequest();
        request.classesDirectory = new File( cli.getOptionValue( CLASSES_DIRECTORY ) );
        request.classpath = Collections.emptyList();
        request.sourceDirectories = Arrays.asList( new String[]{ new File( cli.getOptionValue( SOURCE_DIRECTORY ) ).getAbsolutePath() } );
        request.sourceEncoding = cli.getOptionValue( SOURCE_ENCODING );
        request.useContextClassLoader = true;
        request.outputFile = new File( cli.getOptionValue( OUTPUT_FILE ) );
        request.componentDescriptorDirectory = new File( cli.getOptionValue( DESCRIPTORS_DIRECTORY ) );

        metadataGenerator.generateDescriptor( request );
    }
}")))))
         (add-before 'check 'fix-test-location
           (lambda _
             (substitute* '("src/test/java/org/codehaus/plexus/metadata/DefaultComponentDescriptorWriterTest.java"
                            "src/test/java/org/codehaus/plexus/metadata/merge/ComponentsXmlMergerTest.java")
               (("target") "build")))))))
    (propagated-inputs
     `(("java-plexus-container-default" ,java-plexus-container-default)
       ("java-plexu-component-annotations" ,java-plexus-component-annotations)
       ("java-plexus-utils" ,java-plexus-utils)
       ("java-plexus-cli" ,java-plexus-cli)
       ("java-plexus-classworlds" ,java-plexus-classworlds)
       ("maven-plugin-api" ,maven-plugin-api)
       ("maven-plugin-annotations" ,maven-plugin-annotations)
       ("maven-core-bootstrap" ,maven-core-bootstrap)
       ("maven-model" ,maven-model)
       ("java-commons-cli" ,java-commons-cli)
       ("java-qdox" ,java-qdox)
       ("java-jdom2" ,java-jdom2)
       ("java-asm-8" ,java-asm-8)))
    (native-inputs
     (list java-junit java-guava java-geronimo-xbean-reflect))
    (synopsis "Inversion-of-control container for Maven")
    (description "The Plexus project provides a full software stack for creating
and executing software projects.  Based on the Plexus container, the
applications can utilise component-oriented programming to build modular,
reusable components that can easily be assembled and reused.  This package
provides the Maven plugin generating the component metadata.")))

(define-public java-plexus-container-default-1.7
  (package
    (inherit java-plexus-container-default)
    (version "1.7.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/codehaus-plexus/plexus-containers")
                     (commit (string-append "plexus-containers-" version))))
              (file-name (git-file-name "java-plexus-container-default" version))
              (sha256
               (base32
                "1316hrp5vqfv0aw7miq2fp0wwy833h66h502h29vnh5sxj27x228"))))))

(define java-plexus-containers-parent-pom-1.7
  (package
    (inherit java-plexus-container-default-1.7)
    (name "java-plexus-containers-parent-pom")
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (replace 'install
           (install-pom-file "pom.xml")))))
    (propagated-inputs
     `(("plexus-parent-pom" ,plexus-parent-pom-4.0)))))

(define-public java-plexus-component-annotations-1.7
  (package
    (inherit java-plexus-container-default-1.7)
    (name "java-plexus-component-annotations")
    (arguments
     `(#:jar-name "plexus-component-annotations.jar"
       #:source-dir "plexus-component-annotations/src/main/java"
       #:tests? #f; no tests
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (install-from-pom "plexus-component-annotations/pom.xml")))))
    (propagated-inputs
     `(("java-plexus-containers-parent-pom-1.7" ,java-plexus-containers-parent-pom-1.7)))
    (inputs '())
    (native-inputs '())
    (synopsis "Plexus descriptors generator")
    (description "This package is a Maven plugin to generate Plexus descriptors
from source tags and class annotations.")))

(define-public java-plexus-component-metadata-1.7
  (package
    (inherit java-plexus-container-default-1.7)
    (name "java-plexus-component-metadata")
    (arguments
     `(#:jar-name "plexus-component-metadata.jar"
       #:source-dir "src/main/java"
       #:test-dir "src/test"
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'chdir
           (lambda _
             (chdir "plexus-component-metadata")
             #t))
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "src/main/resources"
                               "build/classes/")
             #t))
         (add-before 'build 'fix-jdom
           (lambda _
             ;; The newer version of jdom now sets multiple features by default
             ;; that are not supported.
             ;; Skip these features
             (substitute* "src/main/java/org/codehaus/plexus/metadata/merge/MXParser.java"
               (("throw new XmlPullParserException\\(\"unsupporte feature \"\\+name\\);")
                "// skip"))))
         (add-before 'check 'fix-test-location
           (lambda _
             (substitute* '("src/test/java/org/codehaus/plexus/metadata/DefaultComponentDescriptorWriterTest.java"
                            "src/test/java/org/codehaus/plexus/metadata/merge/ComponentsXmlMergerTest.java")
               (("target") "build")))))))
    (propagated-inputs
     (list java-plexus-container-default-1.7
           java-plexus-component-annotations-1.7
           java-plexus-utils
           java-plexus-cli
           java-plexus-cli
           java-plexus-classworlds
           maven-plugin-api
           maven-plugin-annotations
           maven-core-bootstrap
           maven-model
           java-commons-cli
           java-qdox
           java-jdom2
           java-asm-8))
    (native-inputs
     (list java-junit java-guava java-geronimo-xbean-reflect))
    (synopsis "Inversion-of-control container for Maven")
    (description "The Plexus project provides a full software stack for creating
and executing software projects.  Based on the Plexus container, the
applications can utilise component-oriented programming to build modular,
reusable components that can easily be assembled and reused.  This package
provides the Maven plugin generating the component metadata.")))

(define-public java-plexus-cipher
  (package
    (name "java-plexus-cipher")
    (version "2.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/codehaus-plexus/plexus-cipher")
                     (commit (string-append "plexus-cipher-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "01fipdsm090n8j4207fl8kbxznkgkmkkgyazf53hm1nwn6na5aai"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "plexus-cipher.jar"
       #:source-dir "src/main/java"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'generate-javax.inject.Named
           (lambda _
             (mkdir-p "build/classes/META-INF/sisu")
             (with-output-to-file "build/classes/META-INF/sisu/javax.inject.Named"
               (lambda _
                 (display
                   "org.sonatype.plexus.components.cipher.DefaultPlexusCipher\n")))
             #t))
         (replace 'install (install-from-pom "pom.xml")))))
    (inputs
     `(("java-cdi-api" ,java-cdi-api)
       ("java-javax-inject" ,java-javax-inject)))
    (propagated-inputs
     `(("java-sonatype-spice-parent-pom" ,java-sonatype-spice-parent-pom-15)
       ("java-eclipse-sisu-inject" ,java-eclipse-sisu-inject)))
    (native-inputs
     `(("java-junit" ,java-junit)))
    (home-page "https://github.com/sonatype/plexus-cipher")
    (synopsis "Encryption/decryption Component")
    (description "Plexus-cipher contains a component to deal with encryption
and decryption.")
    (license license:asl2.0)))

(define-public java-plexus-cipher-1.7
  (package
    (inherit java-plexus-cipher)
    (version "1.7")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/codehaus-plexus/plexus-cipher")
                     (commit (string-append "plexus-cipher-" version))))
              (file-name (git-file-name "java-plexus-cipher" version))
              (sha256
               (base32
                "0m638nzlxbmnbcj5cwdpgs326ab584yv0k803zlx37r6iqwvf6b0"))))
    (arguments
     `(#:jar-name "plexus-cipher.jar"
       #:source-dir "src/main/java"
       #:tests? #f; FIXME: requires sisu-inject-bean
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "src/main/resources" "build/classes")
             (mkdir-p "build/classes/META-INF/sisu")
             (with-output-to-file "build/classes/META-INF/sisu/javax.inject.Named"
               (lambda _
                 (display "org.sonatype.plexus.components.cipher.DefaultPlexusCipher\n")))
             #t))
         (add-before 'install 'fix-test-dependency
           (lambda _
             ;; sisu-inject-bean is only used for tests, but its scope is "provided".
             (substitute* "pom.xml"
               (("provided") "test"))
             #t))
         (replace 'install (install-from-pom "pom.xml")))))
    (propagated-inputs
     (list java-sonatype-spice-parent-pom-15))))

(define-public java-plexus-java
  (package
    (name "java-plexus-java")
    (version "0.9.10")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/codehaus-plexus/plexus-languages")
                     (commit (string-append "plexus-languages-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0vmvgq5hfxs90yyxgssfpwq78l7vwx1ljwpkk594mrdr8sm668b5"))
              (modules '((guix build utils)))
              (snippet
               `(begin
                  (for-each delete-file (find-files "." ".*.jar$"))
                  #t))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "plexus-java.java"
       #:source-dir "plexus-java/src/main/java"
       #:test-dir "plexus-java/src/test"
       #:tests? #f; require mockito 2
       #:phases
       (modify-phases %standard-phases
         (add-after 'build 'generate-metadata
           (lambda _
             (invoke "java" "-cp" (string-append (getenv "CLASSPATH") ":build/classes")
                     "org.codehaus.plexus.metadata.PlexusMetadataGeneratorCli"
                     "--source" "plexus-java/src/main/java"
                     "--output" "build/classes/META-INF/plexus/components.xml"
                     "--classes" "build/classes"
                     "--descriptors" "build/classes/META-INF")
             (invoke "ant" "jar")
             #t))
         (add-before 'install 'install-parent
           (install-pom-file "pom.xml"))
         (replace 'install
           (install-from-pom "plexus-java/pom.xml")))))
    (propagated-inputs
     (list java-asm java-qdox-2-M9 java-javax-inject
           plexus-parent-pom-4.0))
    (inputs
     (list java-plexus-component-annotations-1.7))
    (native-inputs
     (list java-plexus-component-metadata-1.7 java-junit))
    (home-page "https://codehaus-plexus.github.io/plexus-languages/plexus-java")
    (synopsis "Shared language features for Java")
    (description "This package contains shared language features of the Java
language, for the plexus project.")
    (license license:asl2.0)))

(define-public java-plexus-compiler-api
  (package
    (name "java-plexus-compiler-api")
    (version "2.8.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/codehaus-plexus/plexus-compiler")
                     (commit (string-append "plexus-compiler-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1nq1gnn3s6z1j29gmi1hqbklsmm8b1lmnafb0191914f95mn18gk"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "plexus-compiler-api.jar"
       #:source-dir "plexus-compiler-api/src/main/java"
       #:test-dir "plexus-compiler-api/src/test"
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (install-from-pom "plexus-compiler-api/pom.xml")))))
    (propagated-inputs
     `(("java-plexus-container-default" ,java-plexus-container-default)
       ("java-plexus-compiler-pom" ,java-plexus-compiler-pom)
       ("java-plexus-util" ,java-plexus-utils)))
    (native-inputs
     (list java-junit))
    (home-page "https://github.com/codehaus-plexus/plexus-compiler")
    (synopsis "Plexus Compilers component's API to manipulate compilers")
    (description "This package contains the API used by components to manipulate
compilers.")
    (license (list license:asl2.0
                   license:expat))))

(define java-plexus-compiler-pom
  (package
    (inherit java-plexus-compiler-api)
    (name "java-plexus-compiler-pom")
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (replace 'install
           (install-pom-file "pom.xml"))
         (add-after 'install 'install-compilers
           (install-pom-file "plexus-compilers/pom.xml")))))
    (propagated-inputs
     `(("plexus-components-parent-pom-4.0" ,plexus-components-parent-pom-4.0)))))

(define plexus-components-parent-pom-4.0
  (package
    (name "plexus-components-parent-pom")
    (version "4.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/codehaus-plexus/plexus-components")
                     (commit (string-append "plexus-components-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "041bm8yv0m2i17mqg8zljib4ykpha7ijls2qfdwvkma4d39lhysi"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (replace 'install
           (install-pom-file "pom.xml")))))
    (propagated-inputs
      `(("plexus-parent-pom-4.0" ,plexus-parent-pom-4.0)))
    (home-page "https://codehaus-plexus.github.io/plexus-components")
    (synopsis "Plexus parent pom")
    (description "This package contains the Plexus components parent POM.")
    (license license:asl2.0)))

(define-public java-plexus-compiler-manager
  (package
    (inherit java-plexus-compiler-api)
    (name "java-plexus-compiler-manager")
    (arguments
     `(#:jar-name "compiler-compiler-manager.java"
       #:source-dir "plexus-compiler-manager/src/main/java"
       #:test-dir "plexus-compiler-manager/src/test"
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'build 'generate-metadata
           (lambda _
             (invoke "java" "-cp" (string-append (getenv "CLASSPATH") ":build/classes")
                     "org.codehaus.plexus.metadata.PlexusMetadataGeneratorCli"
                     "--source" "plexus-compiler-manager/src/main/java"
                     "--output" "build/classes/META-INF/plexus/components.xml"
                     "--classes" "build/classes"
                     "--descriptors" "build/classes/META-INF")
             (invoke "ant" "jar")
             #t))
         (add-after 'generate-metadata 'rebuild
           (lambda _
             (invoke "ant" "jar")
             #t))
         (replace 'install
           (install-from-pom "plexus-compiler-manager/pom.xml")))))
    (propagated-inputs
     (list java-plexus-compiler-api java-plexus-compiler-pom
           java-plexus-container-default-1.7))
    (native-inputs
     (list unzip java-plexus-component-metadata-1.7))
    (synopsis "Compiler management for Plexus Compiler component")
    (description "Plexus Compiler is a Plexus component to use different
compilers through a uniform API.  This component chooses the compiler
implementation to use in a project.")))

(define-public java-plexus-compiler-javac
  (package
    (inherit java-plexus-compiler-api)
    (name "java-plexus-compiler-javac")
    (arguments
     `(#:jar-name "plexus-compiler-javac.jar"
       #:source-dir "plexus-compilers/plexus-compiler-javac/src/main/java"
       #:jdk ,icedtea-8
       #:tests? #f; depends on compiler-test -> maven-core -> ... -> this package.
       #:test-dir "plexus-compilers/plexus-compiler-javac/src/test"
       #:modules ((guix build ant-build-system)
                  (guix build utils)
                  (guix build java-utils)
                  (sxml simple))
       #:phases
       (modify-phases %standard-phases
         ;; We cannot use java-plexus-component-metadata to generate the metadata
         ;; because it ultimately depends on this package.
         ;; Create it manually instead
         (add-before 'build 'create-metadata
           (lambda _
             (let* ((dir "build/classes/META-INF/plexus")
                    (file (string-append dir "/components.xml")))
               (mkdir-p dir)
               (with-output-to-file file
                 (lambda _
                   (sxml->xml
                     `(component-set
                        (components
                          (component
                            (role "org.codehaus.plexus.compiler.Compiler")
                            (role-hint "javac")
                            (implementation "org.codehaus.plexus.compiler.javac.JavacCompiler")
                            (isolated-realm "false"))))))))
             #t))
         (replace 'install
           (install-from-pom "plexus-compilers/plexus-compiler-javac/pom.xml")))))
    (propagated-inputs
     (list java-plexus-compiler-api java-plexus-utils
           java-plexus-container-default))
    (synopsis "Javac Compiler support for Plexus Compiler component")
    (description "This package contains the Javac Compiler support for Plexus
Compiler component.")))

(define plexus-components-pom-1.1.20
  (package
    (name "plexus-components-pom-1.1.20")
    (version "1.1.20")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/codehaus-plexus/plexus-components")
                     (commit (string-append "plexus-components-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1q254k95m9icyfsvaw8c226midg8v6v436wvivhv7im825mnp5yb"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (replace 'install
           (install-pom-file "pom.xml")))))
    (propagated-inputs
     `(("plexus-parent-pom" ,plexus-parent-pom-3.1)))
    (home-page "https://github.com/codehaus-plexus/plexus-components")
    (synopsis "Maven parent pom for plexus packages")
    (description "This package contains the parent pom for plexus component
packages.")
    (license license:asl2.0)))

(define-public java-plexus-digest
  (package
    (name "java-plexus-digest")
    (version "1.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/codehaus-plexus/plexus-digest")
                     (commit "2a52ad1bda8297fa0e287163d2fa37245ec6a430")))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "19w5wxsliz8r42niry68qa665kvjsb8081dazg9vgd3pca72w07x"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "plexus-digest.jar"
       #:source-dir "src/main/java"
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (install-from-pom "pom.xml")))))
    (propagated-inputs
     (list java-plexus-utils plexus-components-pom-1.1.20))
    (native-inputs
     (list java-junit))
    (home-page "https://github.com/codehaus-plexus/plexus-digest")
    (synopsis "Hash function utilities for Java")
    (description "This package is a plexus component that contains hash
function utilities.")
    (license license:asl2.0)))

(define-public java-plexus-sec-dispatcher
  (package
    (name "java-plexus-sec-dispatcher")
    (version "2.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/codehaus-plexus/plexus-sec-dispatcher")
                     (commit (string-append "plexus-sec-dispatcher-" version))))
              (sha256
               (base32
                "0665zcyxkv2knydxgv2dn64zvy1dx9j9af12ds9s64qmzd1rk6pk"))
              (file-name (git-file-name name version))))
    (arguments
     `(#:jar-name "plexus-sec-dispatcher.jar"
       #:source-dir "src/main/java"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'generate-models
           (lambda* (#:key inputs #:allow-other-keys)
             (define (modello-single-mode file version mode)
               (invoke "java"
                       "org.codehaus.modello.ModelloCli"
                       file mode "src/main/java" version
                       "false" "true"))
             (let ((file "src/main/mdo/settings-security.mdo"))
               (modello-single-mode file "1.0.0" "java")
               (modello-single-mode file "1.0.0" "xpp3-reader")
               (modello-single-mode file "1.0.0" "xpp3-writer"))
             #t))
         (add-before 'build 'generate-javax.inject.Named
           (lambda _
             (mkdir-p "build/classes/META-INF/sisu")
             (with-output-to-file "build/classes/META-INF/sisu/javax.inject.Named"
               (lambda _
                 (display
                   "org.sonatype.plexus.components.sec.dispatcher.DefaultSecDispatcher\n")))
             #t))
         (add-before 'check 'fix-paths
           (lambda _
             (copy-recursively "src/test/resources" "target")
             #t))
         (replace 'install (install-from-pom "pom.xml")))))
    (propagated-inputs
     (list java-plexus-utils java-plexus-cipher plexus-parent-pom-8))
    (native-inputs
     (list java-javax-inject
           java-modello-core
           ;; for modello
           java-plexus-container-default
           java-plexus-classworlds
           java-plexus-utils
           java-guava
           java-geronimo-xbean-reflect
           ;; modello plugins
           java-modello-plugins-java
           java-modello-plugins-xml
           java-modello-plugins-xpp3
           ;; for tests
           java-junit))
    (build-system ant-build-system)
    (home-page "https://github.com/sonatype/plexus-sec-dispatcher")
    (synopsis "Plexus Security Dispatcher Component")
    (description "This package is the Plexus Security Dispatcher Component.
This component decrypts a string passed to it.")
    (license license:asl2.0)))

(define-public java-plexus-sec-dispatcher-1.4
  (package
    (inherit java-plexus-sec-dispatcher)
    (version "1.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/codehaus-plexus/plexus-sec-dispatcher")
                     (commit (string-append "sec-dispatcher-" version))))
              (sha256
               (base32
                "1ng4yliy4cqpjr4fxxjbpwyk1wkch5f8vblm1kvwf328s4gibszs"))
              (file-name (git-file-name "java-plexus-sec-dispatcher" version))))
    (arguments
     (substitute-keyword-arguments (package-arguments java-plexus-sec-dispatcher)
      ((#:phases phases)
       `(modify-phases ,phases
         (delete 'generate-javax.inject.Named)
         (add-before 'build 'generate-components.xml
           (lambda _
             (mkdir-p "build/classes/META-INF/plexus")
             (with-output-to-file "build/classes/META-INF/plexus/components.xml"
               (lambda _
                 (display
                   "<component-set>\n
  <components>\n
    <component>\n
      <role>org.sonatype.plexus.components.sec.dispatcher.SecDispatcher</role>\n
      <role-hint>default</role-hint>\n
      <implementation>org.sonatype.plexus.components.sec.dispatcher.DefaultSecDispatcher</implementation>\n
      <description></description>\n
      <requirements>\n
        <requirement>\n
          <role>org.sonatype.plexus.components.cipher.PlexusCipher</role>\n
          <field-name>_cipher</field-name>\n
        </requirement>\n
        <requirement>\n
          <role>org.sonatype.plexus.components.sec.dispatcher.PasswordDecryptor</role>\n
          <field-name>_decryptors</field-name>\n
        </requirement>\n
      </requirements>\n
      <configuration>\n
        <_configuration-file>~/.settings-security.xml</_configuration-file>\n
      </configuration>\n
    </component>\n
  </components>\n
</component-set>\n")))))))))
    (propagated-inputs
     (list java-plexus-utils java-plexus-cipher-1.7
           java-sonatype-spice-parent-pom-12))))

(define-public java-plexus-cli
  (package
    (name "java-plexus-cli")
    (version "1.7")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/sonatype/plexus-cli")
                     (commit "a776afa6bca84e5107bedb69440329cdb24ed645")))
              (file-name (string-append name "-" version))
              (sha256
               (base32
                "0xjrlay605rypv3zd7y24vlwf0039bil3n2cqw54r1ddpysq46vx"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "plexus-cli.jar"
       #:source-dir "src/main/java"
       #:jdk ,icedtea-8
       #:test-dir "src/test"))
    (inputs
     (list java-commons-cli java-plexus-container-default
           java-plexus-classworlds))
    (native-inputs
     (list java-plexus-utils java-junit java-guava))
    (home-page "https://codehaus-plexus.github.io/plexus-cli")
    (synopsis "CLI building library for plexus")
    (description "This package is a library to help creating CLI around
Plexus components.")
    (license license:asl2.0)))

(define-public java-plexus-build-api
  (package
    (name "java-plexus-build-api")
    (version "0.0.7")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/codehaus-plexus/plexus-build-api")
                     (commit (string-append "plexus-build-api-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1d5w6c58gkx30d51v7qwv1xrhc0ly76848gihmgshj19yf6yhca0"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "plexus-build-api.jar"
       #:source-dir "src/main/java"
       #:jdk ,icedtea-8
       #:tests? #f; FIXME: how to run the tests?
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "src/main/resources" "build/classes")
             (substitute* (find-files "build/classes")
               (("\\$\\{project.version\\}") ,version))
             #t))
         (add-before 'build 'generate-plexus-compontent
           (lambda _
             (mkdir-p "build/classes/META-INF/plexus")
             ;; This file is required for plexus to inject this package.
             ;; FIXME: how is it generated?
             (with-output-to-file "build/classes/META-INF/plexus/components.xml"
               (lambda _
                 (display
                   "<component-set>\n
  <components>\n
    <component>\n
      <role>org.sonatype.plexus.build.incremental.BuildContext</role>\n
      <role-hint>default</role-hint>\n
      <implementation>org.sonatype.plexus.build.incremental.DefaultBuildContext</implementation>\n
      <description>Filesystem based non-incremental build context implementation\n
which behaves as if all files were just created.</description>\n
    </component>\n
  </components>\n
</component-set>\n")))
             #t))
         (replace 'install
           (install-from-pom "pom.xml")))))
    (inputs
     (list java-plexus-utils-3.2.1 java-plexus-container-default))
    (home-page "https://github.com/codehaus-plexus/plexus-build-api/")
    (synopsis "Base build API for maven")
    (description "This package contains the base build API for maven and
a default implementation of it.  This API is about scanning files in a
project and determining what files need to be rebuilt.")
    (license license:asl2.0)))

(define-public java-modello-core
  (package
    (name "java-modello-core")
    (version "1.11")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/codehaus-plexus/modello")
                     (commit (string-append "modello-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "18885sim7z9j3wy19i9083y9kc8l9xxl2px823a96q4rnqj5z8s2"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "modello-core.jar"
       #:source-dir "modello-core/src/main/java"
       #:test-dir "modello-core/src/test"
       #:main-class "org.codehaus.modello.ModelloCli"
       #:jdk ,icedtea-8
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-resources
           (lambda _
             (mkdir-p "build/classes/META-INF/plexus")
             (copy-file "modello-core/src/main/resources/META-INF/plexus/components.xml"
                        "build/classes/META-INF/plexus/components.xml")
             #t))
         (add-before 'check 'fix-tests
           (lambda _
             (with-directory-excursion "modello-core/src/test/java/org/codehaus"
               (substitute* '("modello/core/DefaultModelloCoreTest.java"
                              "modello/core/io/ModelReaderTest.java")
                 (("src/test") "modello-core/src/test")))
             #t)))))
    (propagated-inputs
     (list java-plexus-utils java-plexus-container-default
           java-plexus-build-api))
    (native-inputs
     (list java-junit java-plexus-classworlds java-geronimo-xbean-reflect
           java-guava))
    (home-page "https://codehaus-plexus.github.io/modello/")
    (synopsis "Framework for code generation from a simple model")
    (description "Modello is a framework for code generation from a simple model.

Modello generates code from a simple model format: based on a plugin
architecture, various types of code and descriptors can be generated from the
single model, including Java POJOs, XML/JSON/YAML marshallers/unmarshallers,
XSD and documentation.")
    (license (list
               license:expat
               ;; Although this package uses only files licensed under expat,
               ;; other parts of the source are licensed under different
               ;; licenses.  We include them to be inherited by other packages.
               license:asl2.0
               ;; Some files in modello-plugin-java are licensed under a
               ;; 5-clause BSD license.
               (license:non-copyleft
                 (string-append "file:///modello-plugins/modello-plugin-java/"
                                "src/main/java/org/codehaus/modello/plugin/"
                                "java/javasource/JNaming.java"))))))

(define-public java-modello-plugins-java
  (package
    (inherit java-modello-core)
    (name "java-modello-plugins-java")
    (arguments
     `(#:jar-name "modello-plugins-java.jar"
       #:source-dir "modello-plugins/modello-plugin-java/src/main/java"
       #:test-dir "modello-plugins/modello-plugin-java/src/test"
       #:jdk ,icedtea-8
       #:tests? #f; requires maven-model, which depends on this package
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-resources
           (lambda _
             (mkdir-p "build/classes")
             (copy-recursively "modello-plugins/modello-plugin-java/src/main/resources"
                               "build/classes")
             #t)))))
    (inputs
     (list java-modello-core))
    (synopsis "Modello Java Plugin")
    (description "Modello Java Plugin generates Java objects for the model.")))

(define-public java-modello-plugins-xml
  (package
    (inherit java-modello-core)
    (name "java-modello-plugins-xml")
    (arguments
     `(#:jar-name "modello-plugins-xml.jar"
       #:source-dir "modello-plugins/modello-plugin-xml/src/main/java"
       #:test-dir "modello-plugins/modello-plugin-xml/src/test"
       #:jdk ,icedtea-8
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-resources
           (lambda _
             (mkdir-p "build/classes")
             (copy-recursively
               "modello-plugins/modello-plugin-xml/src/main/resources"
               "build/classes")
             #t))
         (add-before 'check 'fix-paths
           (lambda _
             (with-directory-excursion "modello-plugins/modello-plugin-xml/src/test"
               (substitute*
                 "java/org/codehaus/modello/plugins/xml/XmlModelloPluginTest.java"
                 (("src/test") "modello-plugins/modello-plugin-xml/src/test")))
             #t)))))
    (propagated-inputs
     (list java-modello-core java-modello-plugins-java))
    (synopsis "Modello XML Plugin")
    (description "Modello XML Plugin contains shared code for every plugins
working on XML representation of the model.")))

(define-public java-modello-test
  (package
    (inherit java-modello-core)
    (name "java-modello-test")
    (arguments
     `(#:jar-name "modello-test.jar"
       #:source-dir "modello-test/src/main/java"
       #:tests? #f; no tests
       #:jdk ,icedtea-8))
    (inputs
     (list java-plexus-utils java-plexus-compiler-api
           java-plexus-compiler-javac java-plexus-container-default))
    (synopsis "Modello test package")
    (description "The modello test package contains the basis to create
Modello generator unit-tests, including sample models and xml files to test
every feature for every plugin.")))

(define-public java-modello-plugins-xpp3
  (package
    (inherit java-modello-core)
    (name "java-modello-plugins-xpp3")
    (arguments
     `(#:jar-name "modello-plugins-xpp3.jar"
       #:source-dir "modello-plugins/modello-plugin-xpp3/src/main/java"
       #:test-dir "modello-plugins/modello-plugin-xpp3/src/test"
       ;; One of the test dependencies is maven-model which depends on this package.
       #:tests? #f
       #:jdk ,icedtea-8
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-resources
           (lambda _
             (mkdir-p "build/classes")
             (copy-recursively "modello-plugins/modello-plugin-xpp3/src/main/resources"
                               "build/classes")
             #t)))))
    (propagated-inputs
     (list java-modello-core java-modello-plugins-java
           java-modello-plugins-xml))
    (native-inputs
     (modify-inputs (package-native-inputs java-modello-core)
       (prepend java-xmlunit java-modello-test)))
    (synopsis "Modello XPP3 Plugin")
    (description "The modello XPP3 plugin generates XML readers and writers based
on the XPP3 API (XML Pull Parser).")))

(define-public java-ow-util-ant-tasks
  (package
    (name "java-ow-util-ant-tasks")
    (version "1.3.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                     "mirror://debian/pool/main/o/ow-util-ant-tasks/"
                     "ow-util-ant-tasks_" version ".orig.tar.gz"))
              (sha256
               (base32
                "1y5ln1g36aligwcadqksdj18i5ghqnxn523wjbzy2zyd7w58fgy5"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "ow-util-ant-tasks.jar"
       #:tests? #f; no tests
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'delete-cyclic-dependency
           (lambda _
             ;; This file depends on asm-3, which depends on this package
             (delete-file "src/org/objectweb/util/ant/DependencyAnalyzer.java")
             ;; This file depends on xalan
             (delete-file "src/org/objectweb/util/ant/Xalan2Liaison.java")))
         (add-before 'build 'fix-new-ant
           (lambda _
             (substitute* "src/org/objectweb/util/ant/MultipleCopy.java"
               ((", destFile.getAbsolutePath\\(\\)")
                ", new String[] { destFile.getAbsolutePath() }")))))))
    (home-page "https://packages.debian.org/source/stretch/ow-util-ant-tasks")
    (synopsis "Replacement for base ant tasks")
    (description "This library is used in the legacy build process of several
key frameworks developed by ObjectWeb, among them legacy versions of the
ObjectWeb ASM bytecode manipulation framework.")
    (license license:lgpl2.0+)))

(define-public java-asm
  (package
    (name "java-asm")
    (version "6.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://download.forge.ow2.org/asm/"
                                  "asm-" version ".tar.gz"))
              (sha256
               (base32
                "115l5pqblirdkmzi32dxx7gbcm4jy0s14y5wircr6h8jdr9aix00"))))
    (build-system ant-build-system)
    (arguments
     `(#:build-target "compile"
       ;; The tests require an old version of Janino, which no longer compiles
       ;; with the JDK7.
       #:tests? #f
       #:make-flags
       (list
        ;; We don't need these extra ant tasks, but the build system asks us to
        ;; provide a path anyway.
        "-Dobjectweb.ant.tasks.path=dummy-path"
        ;; The java-aqute.bndlib JAR file will be put onto the classpath and
        ;; used during the build automatically by ant-build-system, but
        ;; java-asm's build.xml fails unless we provide something here.
        "-Dbiz.aQute.bnd.path=dummy-path")
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'remove-bnd-dependency
           (lambda _
             ;; This file is the only one to require bnd, and is not needed
             ;; because we don't build a bundle.
             (delete-file "src/org/objectweb/asm/tools/ModuleInfoBndPlugin.java")
             #t))
         (add-before 'install 'build-jars
           (lambda* (#:key make-flags #:allow-other-keys)
             ;; We cannot use the "jar" target because it depends on a couple
             ;; of unpackaged, complicated tools.
             (mkdir "dist")
             (invoke "jar"
                     "-cf" (string-append "dist/asm-" ,version ".jar")
                     "-C" "output/build/tmp" ".")))
         (add-before 'install 'fix-pom
           (lambda _
             (substitute* (find-files "archive" "\\.pom$")
               (("@product.artifact@") ,version))
             #t))
         (add-before 'install 'install-parent
           (install-pom-file "archive/asm-parent.pom"))
         (replace 'install
           (install-from-pom "archive/asm.pom")))))
    (native-inputs
     (list java-junit))
    (propagated-inputs
     (list java-org-ow2-parent-pom-1.3))
    (home-page "https://asm.ow2.io/")
    (synopsis "Very small and fast Java bytecode manipulation framework")
    (description "ASM is an all purpose Java bytecode manipulation and
analysis framework.  It can be used to modify existing classes or dynamically
generate classes, directly in binary form.  The provided common
transformations and analysis algorithms allow easily assembling custom
complex transformations and code analysis tools.")
    (license license:bsd-3)))

(define-public java-org-ow2-parent-pom-1.3
  (package
    (name "java-org-ow2-parent-pom")
    (version "1.3")
    (source (origin
              (method url-fetch)
              (uri "https://repo1.maven.org/maven2/org/ow2/ow2/1.3/ow2-1.3.pom")
              (sha256
               (base32
                "1yr8hfx8gffpppa4ii6cvrsq029a6x8hzy7nsavxhs60s9kmq8ai"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'unpack)
         (delete 'build)
         (delete 'configure)
         (replace 'install
           ,#~(install-pom-file #$source)))))
    (home-page "https://ow2.org")
    (synopsis "Ow2.org parent pom")
    (description "This package contains the parent pom for projects from ow2.org,
including java-asm.")
    (properties '((hidden? . #t)))
    (license license:lgpl2.1+)))

(define-public java-asm-bootstrap
  (package
    (inherit java-asm)
    (name "java-asm-bootstrap")
    (properties '((hidden? . #t)))
    (arguments
     (substitute-keyword-arguments (package-arguments java-asm)
       ((#:tests? _) #f)))
    (native-inputs `())))

(define-public java-asm-3
  (package
    (inherit java-asm)
    (version "3.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://gitlab.ow2.org/asm/asm")
                     (commit "ASM_3_1")))
              (file-name (git-file-name "java-asm" version))
              (sha256
               (base32
                "0xbyf2sl8j6mrvfpg2da0vjdp906rac62l66gkk82x5cn3vc30h4"))
              (modules '((guix build utils)))
              (snippet `(for-each delete-file (find-files "." "\\.jar$")))))
    (arguments
     `(#:build-target "jar"
       #:test-target "test"
       #:tests? #f; require legacy test software
       #:phases
       (modify-phases %standard-phases
         (replace 'install (install-jars "output/dist"))
         (delete 'generate-jar-indices))))
    (native-inputs (list java-ow-util-ant-tasks))))

(define-public java-asm-8
  (package
    (inherit java-asm)
    (version "8.0.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://gitlab.ow2.org/asm/asm")
                     (commit (string-append
                               "ASM_" (string-join (string-split version #\.)
                                                   "_")))))
              (file-name (git-file-name "java-asm" version))
              (sha256
               (base32
                "1s6j27zc1i76gh891w2g48b1c3abp9w8zp5j54yb1vm5h8djkd69"))))
    (arguments
     `(#:jar-name "asm8.jar"
       #:source-dir "asm/src/main/java"
       #:test-dir "asm/src/test"
       ;; tests depend on junit5
       #:tests? #f))
    (propagated-inputs '())
    (native-inputs '())))

(define-public java-asm-tree-8
  (package
    (inherit java-asm-8)
    (name "java-asm-tree")
    (arguments
     `(#:jar-name "asm-tree.jar"
       #:source-dir "asm-tree/src/main/java"
       #:test-dir "asm-tree/src/test"
       ;; tests depend on junit5
       #:tests? #f))
    (inputs
     (list java-asm-8))))

(define-public java-asm-analysis-8
  (package
    (inherit java-asm-8)
    (name "java-asm-analysis")
    (arguments
     `(#:jar-name "asm-analysis.jar"
       #:source-dir "asm-analysis/src/main/java"
       #:test-dir "asm-analysis/src/test"
       ;; tests depend on junit5
       #:tests? #f))
    (inputs
     (list java-asm-8 java-asm-tree-8))))

(define-public java-asm-util-8
  (package
    (inherit java-asm-8)
    (name "java-asm-util")
    (arguments
     `(#:jar-name "asm-util8.jar"
       #:source-dir "asm-util/src/main/java"
       #:test-dir "asm-util/src/test"
       ;; tests depend on junit5
       #:tests? #f))
    (inputs
     (list java-asm-8 java-asm-analysis-8 java-asm-tree-8))))

(define-public java-asm-commons-8
  (package
    (inherit java-asm-8)
    (name "java-asm-commons")
    (arguments
     (list #:jar-name "asm-commons8.jar"
           #:source-dir "asm-commons/src/main/java"
           #:test-dir "asm-commons/src/test"
           ;; tests depend on junit5
           #:tests? #f))
    (inputs (list java-asm-8 java-asm-analysis-8 java-asm-tree-8))))

(define-public java-asm-9
  (package
    (inherit java-asm)
    (version "9.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://gitlab.ow2.org/asm/asm")
                     (commit (string-append
                               "ASM_" (string-join (string-split version #\.)
                                                   "_")))))
              (file-name (git-file-name "java-asm" version))
              (sha256
               (base32
                "0c00m638skr5md1p6y1c2xn11kj5w6sjapyvwp9mh70rw095bwzk"))))
    (arguments
     `(#:jar-name "asm9.jar"
       #:source-dir "asm/src/main/java"
       #:test-dir "asm/src/test"
       ;; tests depend on junit5
       #:tests? #f))
    (propagated-inputs '())
    (native-inputs '())))

(define-public java-asm-tree-9
  (package
    (inherit java-asm-9)
    (name "java-asm-tree")
    (arguments
     `(#:jar-name "asm-tree.jar"
       #:source-dir "asm-tree/src/main/java"
       #:test-dir "asm-tree/src/test"
       ;; tests depend on junit5
       #:tests? #f))
    (inputs
     (list java-asm-9))))

(define-public java-asm-analysis-9
  (package
    (inherit java-asm-9)
    (name "java-asm-analysis")
    (arguments
     `(#:jar-name "asm-analysis.jar"
       #:source-dir "asm-analysis/src/main/java"
       #:test-dir "asm-analysis/src/test"
       ;; tests depend on junit5
       #:tests? #f))
    (inputs
     (list java-asm-9 java-asm-tree-9))))

(define-public java-asm-util-9
  (package
    (inherit java-asm-9)
    (name "java-asm-util")
    (arguments
     `(#:jar-name "asm-util8.jar"
       #:source-dir "asm-util/src/main/java"
       #:test-dir "asm-util/src/test"
       ;; tests depend on junit5
       #:tests? #f))
    (inputs
     (list java-asm-9 java-asm-analysis-9 java-asm-tree-9))))

(define-public java-asm-commons-9
  (package
    (inherit java-asm-9)
    (name "java-asm-commons")
    (arguments
     (list #:jar-name "asm-commons8.jar"
           #:source-dir "asm-commons/src/main/java"
           #:test-dir "asm-commons/src/test"
           ;; tests depend on junit5
           #:tests? #f))
    (inputs (list java-asm-9 java-asm-analysis-9 java-asm-tree-9))))

(define-public java-cglib
  (package
    (name "java-cglib")
    (version "3.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cglib/cglib")
             (commit (string-append
                      "RELEASE_"
                      (string-map (lambda (c) (if (char=? c #\.) #\_ c))
                                  version)))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1lnscamc6bnhh7jgij5garxagp3zn2jp4cbq0rsn4xr3l0cnd014"))))
    (build-system ant-build-system)
    (arguments
     `(;; FIXME: tests fail because junit runs
       ;; "net.sf.cglib.transform.AbstractTransformTest", which does not seem
       ;; to describe a test at all.
       #:tests? #f
       #:jar-name "cglib.jar"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _ (chdir "cglib") #t)))))
    (native-inputs (list java-junit))
    (propagated-inputs (list java-asm-8))
    (home-page "https://github.com/cglib/cglib/")
    (synopsis "Java byte code generation library")
    (description "The byte code generation library CGLIB is a high level API
to generate and transform Java byte code.")
    (license license:asl2.0)))

(define-public java-objenesis
  (package
    (name "java-objenesis")
    (version "3.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/easymock/objenesis")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1brlcn536p4s1v1f3vzxhr38lvyhc33wjrbj2x06kdrd8agy90cr"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f; require junit-5
       #:jar-name "objenesis.jar"
       #:source-dir "main/src/main/java"
       #:test-dir "main/src/test/"))
    (native-inputs
     (list java-junit java-hamcrest-core))
    (home-page "http://objenesis.org/")
    (synopsis "Bypass the constructor when creating an object")
    (description "Objenesis is a small Java library that serves one purpose:
to instantiate a new object of a particular class.  It is common to see
restrictions in libraries stating that classes must require a default
constructor.  Objenesis aims to overcome these restrictions by bypassing the
constructor on object instantiation.")
    (license license:asl2.0)))

(define-public java-easymock
  (package
    (name "java-easymock")
    (version "3.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/easymock/easymock/")
                     (commit (string-append "easymock-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "02vybm8hc0i0n9sp2f2iiqn54zwqhq835f76wc6b2m7819z5a8dq"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "easymock.jar"
       #:source-dir "core/src/main"
       #:test-dir "core/src/test"
       #:phases
       (modify-phases %standard-phases
         ;; FIXME: Android support requires the following packages to be
         ;; available: com.google.dexmaker.stock.ProxyBuilder
         (add-after 'unpack 'delete-android-support
           (lambda _
             (with-directory-excursion "core/src/main/java/org/easymock/internal"
               (substitute* "MocksControl.java"
                 (("AndroidSupport.isAndroid\\(\\)") "false")
                 (("return classProxyFactory = new AndroidClassProxyFactory\\(\\);") ""))
               (delete-file "AndroidClassProxyFactory.java"))
             #t))
         (add-after 'unpack 'delete-broken-tests
           (lambda _
             (with-directory-excursion "core/src/test/java/org/easymock"
               ;; This test depends on dexmaker.
               (delete-file "tests2/ClassExtensionHelperTest.java")

               ;; This is not a test.
               (delete-file "tests/BaseEasyMockRunnerTest.java")

               ;; This test should be executed with a different runner...
               (delete-file "tests2/EasyMockAnnotationsTest.java")
               ;; ...but deleting it means that we also have to delete these
               ;; dependent files.
               (delete-file "tests2/EasyMockRunnerTest.java")
               (delete-file "tests2/EasyMockRuleTest.java")

               ;; This test fails because the file "easymock.properties" does
               ;; not exist.
               (delete-file "tests2/EasyMockPropertiesTest.java"))
             #t)))))
    (inputs
     (list java-cglib java-objenesis))
    (native-inputs
     (list java-junit java-hamcrest-core))
    (home-page "https://easymock.org/")
    (synopsis "Java library providing mock objects for unit tests")
    (description "EasyMock is a Java library that provides an easy way to use
mock objects in unit testing.")
    (license license:asl2.0)))

(define-public java-easymock-3.2
  (package
    (inherit java-easymock)
    (name "java-easymock")
    (version "3.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/easymock/easymock/")
                     (commit (string-append "easymock-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0cn6qqa261mhk9mwxrsz39lkkknfv2h7iprr5zw7wpz9p96dwgv4"))))
    (arguments
     (list #:jar-name "easymock.jar"
           #:source-dir "easymock/src/main"
           #:test-dir "easymock/src/test"
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'delete-android-support
                 (lambda _
                   (with-directory-excursion "easymock/src/main/java/org/easymock/internal"
                     (substitute* "MocksControl.java"
                       (("AndroidSupport.isAndroid\\(\\)") "false")
                       (("return classProxyFactory = new AndroidClassProxyFactory\\(\\);") ""))
                     (delete-file "AndroidClassProxyFactory.java"))))
               (add-after 'unpack 'delete-broken-tests
                 (lambda _
                   (with-directory-excursion "easymock/src/test/java/org/easymock"
                     ;; This test depends on dexmaker.
                     (delete-file "tests2/ClassExtensionHelperTest.java")
                     ;; This is not a test.
                     (delete-file "tests/BaseEasyMockRunnerTest.java")
                     ;; ...but deleting it means that we also have to delete these
                     ;; dependent files.
                     (delete-file "tests2/EasyMockRunnerTest.java")
                     ;; This test fails because the file "easymock.properties" does
                     ;; not exist.
                     (delete-file "tests2/EasyMockPropertiesTest.java")))))))))

(define-public java-easymock-class-extension
  (package
    (inherit java-easymock-3.2)
    (name "java-easymock-class-extension")
    (build-system ant-build-system)
    (arguments
     (list #:jar-name "easymock-class-extensions.jar"
           #:source-dir "easymock-classextension/src/main/java"
           #:test-dir "easymock-classextension/src/test"))
    (inputs (list java-asm-8
                  java-easymock-3.2
                  java-cglib
                  java-objenesis))
    (native-inputs
     (list java-junit java-hamcrest-core))
    (home-page "https://easymock.org/")
    (synopsis "Easymock extension to mock classes")
    (description "This package provides an extension to earlier versions of
easymock that allows mocking classes.")
    (license license:asl2.0)))

(define-public java-jmock-1
  (package
    (name "java-jmock")
    (version "1.2.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/jmock-developers/jmock-library")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0lkga995xd9b9mmzxmcd301hlw83p1h78nibh7djlx7wydscr85z"))))
    (build-system ant-build-system)
    (arguments
     `(#:build-target "jars"
       #:test-target "run.tests"
       #:phases
       (modify-phases %standard-phases
         (replace 'install (install-jars "build")))))
    (home-page "http://jmock.org/")
    (synopsis "Mock object library for test-driven development")
    (description "JMock is a library that supports test-driven development of
Java code with mock objects.  Mock objects help you design and test the
interactions between the objects in your programs.

The jMock library

@itemize
@item makes it quick and easy to define mock objects
@item lets you precisely specify the interactions between
  your objects, reducing the brittleness of your tests
@item plugs into your favourite test framework
@item is easy to extend.
@end itemize\n")
    (license license:bsd-3)))

(define-public java-jmock
  (package
    (inherit java-jmock-1)
    (name "java-jmock")
    (version "2.8.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/jmock-developers/jmock-library")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "12b7l22g3nrjvf2dzcw3z03fpd2chrgp0d8xkvn8w55rwb57pax6"))))
    (inputs
     (list java-hamcrest-all java-bsh java-junit))
    (native-inputs
     `(("cglib" ,java-cglib)))
    (arguments
     `(#:jar-name "java-jmock.jar"
       #:source-dir "jmock/src/main/java"
       #:test-dir "jmock/src/test"))))

(define-public java-jmock-junit4
  (package
    (inherit java-jmock)
    (name "java-jmock-junit4")
    (arguments
     `(#:jar-name "java-jmock-junit4.jar"
       #:source-dir "jmock-junit4/src/main/java"
       #:test-dir "jmock-junit4/src/test"))
    (inputs
     `(("java-hamcrest-all" ,java-hamcrest-all)
       ("java-bsh" ,java-bsh)
       ("java-jmock" ,java-jmock)
       ("java-jumit" ,java-junit)))))

(define-public java-jmock-legacy
  (package
    (inherit java-jmock)
    (name "java-jmock-legacy")
    (arguments
     `(#:jar-name "java-jmock-legacy.jar"
       #:source-dir "jmock-legacy/src/main/java"
       #:test-dir "jmock-legacy/src/test"
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'copy-tests
           (lambda _
             ;; This file is a dependancy of some tests
             (let ((file "org/jmock/test/acceptance/PackageProtectedType.java"))
               (copy-file (string-append "jmock/src/test/java/" file)
                          (string-append "jmock-legacy/src/test/java/" file))
               #t))))))
    (inputs
     (list java-hamcrest-all
           java-objenesis
           java-cglib
           java-jmock
           java-bsh
           java-junit))
    (native-inputs
     (list java-jmock-junit4))))

(define-public java-hamcrest-all
  (package (inherit java-hamcrest-core)
    (name "java-hamcrest-all")
    (arguments
     `(#:jdk ,icedtea-8
       ,@(substitute-keyword-arguments (package-arguments java-hamcrest-core)
           ((#:build-target _) "bigjar")
           ((#:phases phases)
            `(modify-phases ,phases
               ;; Some build targets override the classpath, so we need to patch
               ;; the build.xml to ensure that required dependencies are on the
               ;; classpath.
               (add-after 'unpack 'patch-classpath-for-integration
                 (lambda* (#:key inputs #:allow-other-keys)
                   (substitute* "build.xml"
                     ((" build/hamcrest-library-\\$\\{version\\}.jar" line)
                      (string-join
                       (cons line
                             (append
                              (find-files (assoc-ref inputs "java-junit") "\\.jar$")
                              (find-files (assoc-ref inputs "java-jmock") "\\.jar$")
                              (find-files (assoc-ref inputs "java-easymock") "\\.jar$")))
                       ";"))
                     (("build/hamcrest-core-\\$\\{version\\}\\.jar")
                      (car (find-files (assoc-ref inputs "java-hamcrest-core")
                                       "jar$"))))
                   #t))
               (replace 'install
                 (lambda* (#:key outputs #:allow-other-keys)
                   (let* ((target (string-append (assoc-ref outputs "out")
                                                 "/share/java/"))
                          (version-suffix
                            ,(string-append
                               "-" (package-version java-hamcrest-core) ".jar"))
                          (install-without-version-suffix
                           (lambda (jar)
                             (copy-file jar
                                        (string-append target
                                                       (basename jar version-suffix)
                                                       ".jar")))))
                     (mkdir-p target)
                     (for-each
                      install-without-version-suffix
                      (find-files "build"
                                  (lambda (name _)
                                    (and (string-suffix? ".jar" name)
                                         (not (string-suffix? "-sources.jar" name)))))))
                   #t)))))))
    (inputs
     (modify-inputs (package-inputs java-hamcrest-core)
       (prepend java-junit
                java-jmock-1
                ;; This is necessary because of what seems to be a race condition.
                ;; This package would sometimes fail to build because hamcrest-core.jar
                ;; could not be found, even though it is built as part of this package.
                ;; Adding java-hamcrest-core appears to fix this problem.  See
                ;; https://debbugs.gnu.org/31390 for more information.
                java-hamcrest-core
                java-easymock)))))

(define-public java-jopt-simple
  (package
    (name "java-jopt-simple")
    (version "5.0.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://repo1.maven.org/maven2/"
                                  "net/sf/jopt-simple/jopt-simple/"
                                  version "/jopt-simple-"
                                  version "-sources.jar"))
              (sha256
               (base32
                "1v8bzmwmw6qq20gm42xyay6vrd567dra4vqwhgjnqqjz1gs9f8qa"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f ; there are no tests
       #:jar-name "jopt-simple.jar"
       #:phases
       (modify-phases %standard-phases
         (add-before 'install 'create-pom
           (generate-pom.xml "pom.xml" "net.sf.jopt-simple" "jopt-simple" ,version))
         (replace 'install
           (install-from-pom "pom.xml")))))
    (home-page "https://jopt-simple.github.io/jopt-simple/")
    (synopsis "Java library for parsing command line options")
    (description "JOpt Simple is a Java library for parsing command line
options, such as those you might pass to an invocation of @code{javac}.  In
the interest of striving for simplicity, as closely as possible JOpt Simple
attempts to honor the command line option syntaxes of POSIX @code{getopt} and
GNU @code{getopt_long}.  It also aims to make option parser configuration and
retrieval of options and their arguments simple and expressive, without being
overly clever.")
    (license license:expat)))

;; Required by jmh
(define-public java-jopt-simple-4
  (package
    (inherit java-jopt-simple)
    (version "4.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://repo1.maven.org/maven2/"
                                  "net/sf/jopt-simple/jopt-simple/"
                                  version "/jopt-simple-"
                                  version "-sources.jar"))
              (sha256
               (base32
                "0ny82zczxkn201ld0b7rps0ifzjhfs8m1ncdmy1f50145ciszkpd"))))
    (arguments
     (substitute-keyword-arguments (package-arguments java-jopt-simple)
       ((#:phases phases)
        `(modify-phases ,phases
           (replace 'create-pom
             (generate-pom.xml "pom.xml" "net.sf.jopt-simple" "jopt-simple"
                               ,version))))))))

(define-public java-commons-math3
  (package
    (name "java-commons-math3")
    (version "3.6.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/commons/math/source/"
                                  "commons-math3-" version "-src.tar.gz"))
              (sha256
               (base32
                "19l6yp44qc5g7wg816nbn5z3zq3xxzwimvbm4a8pczgvpi4i85s6"))))
    (build-system ant-build-system)
    (arguments
     `(#:build-target "jar"
       #:test-target "test"
       #:make-flags
       ,#~(let ((hamcrest #$(this-package-native-input "java-hamcrest-core"))
                (junit    #$(this-package-native-input "java-junit")))
            (list (string-append "-Djunit.jar="
                                 (car (find-files junit "jar$")))
                  (string-append "-Dhamcrest.jar="
                                 (car (find-files hamcrest ".*.jar$")))))
       #:phases
       (modify-phases %standard-phases
         ;; We want to build the jar in the build phase and run the tests
         ;; later in a separate phase.
         (add-after 'unpack 'untangle-targets
           (lambda _
             (substitute* "build.xml"
               (("name=\"jar\" depends=\"test\"")
                "name=\"jar\" depends=\"compile\""))))
         ;; There is no install target.
         (replace 'install
           (install-from-pom "pom.xml")))))
    (native-inputs
     (list java-junit java-hamcrest-core))
    (home-page "https://commons.apache.org/math/")
    (synopsis "Apache Commons mathematics library")
    (description "Commons Math is a library of lightweight, self-contained
mathematics and statistics components addressing the most common problems not
available in the Java programming language or Commons Lang.")
    (license license:asl2.0)))

(define-public java-jmh
  (package
    (name "java-jmh")
    (version "1.32")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/openjdk/jmh")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0i7fa7l3gdqkkgz5ddayp6m46dgbj9rqlz35xffrcbyiz3gpljy0"))))
    (build-system maven-build-system)
    (arguments
     `(#:exclude
       (("org.apache.maven.plugins" .
         ("maven-source-plugin" "maven-archetype-plugin" "maven-shade-plugin"
          "maven-site-plugin" "maven-javadoc-plugin" "maven-eclipse-plugin"))
        ("com.mycila.maven-license-plugin" . ("maven-license-plugin"))
        ("org.apache.maven.wagon" . ("wagon-ssh")))
       #:maven-plugins
       (("maven-enforcer-plugin" ,maven-enforcer-plugin)
        ,@(default-maven-plugins))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-unnecessary
           (lambda _
             ;; requires org.apache.maven.archetype:archetype-packaging.
             ;; Its subprojects also require groovy, kotlin and scala,
             ;; respectively.
             (delete-file-recursively "jmh-archetypes"))))))
    (propagated-inputs
     (list java-jopt-simple-4 java-commons-math3))
    (native-inputs
     (list java-junit java-hamcrest-core))
    (home-page "https://openjdk.java.net/projects/code-tools/jmh/")
    (synopsis "Benchmark harness for the JVM")
    (description "JMH is a Java harness for building, running, and analysing
nano/micro/milli/macro benchmarks written in Java and other languages
targeting the JVM.")
    ;; GPLv2 only
    (license license:gpl2)))

(define-public java-commons-collections4
  (package
    (name "java-commons-collections4")
    (version "4.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/commons/collections/source/"
                                  "commons-collections4-" version "-src.tar.gz"))
              (sha256
               (base32
                "1krfhvggympq4avk7gh6qafzf6b9ip6r1m4lmacikyx04039m0wl"))))
    (build-system ant-build-system)
    (arguments
     `(#:test-target "test"
       #:make-flags
       ,#~(let ((hamcrest #$(this-package-native-input "java-hamcrest-core"))
                (junit    #$(this-package-native-input "java-junit"))
                (easymock #$(this-package-native-input "java-easymock")))
            (list (string-append "-Djunit.jar="
                                 (car (find-files junit "jar$")))
                  (string-append "-Dhamcrest.jar="
                                 (car (find-files hamcrest "jar$")))
                  (string-append "-Deasymock.jar=" easymock
                                 "/share/java/easymock.jar")))
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (install-jars "target")))))
    (native-inputs
     (list java-junit java-hamcrest-core java-easymock))
    (home-page "https://commons.apache.org/collections/")
    (synopsis "Collections framework")
    (description "The Java Collections Framework is the recognised standard
for collection handling in Java.  Commons-Collections seek to build upon the
JDK classes by providing new interfaces, implementations and utilities.  There
are many features, including:

@itemize
@item @code{Bag} interface for collections that have a number of copies of
  each object
@item @code{BidiMap} interface for maps that can be looked up from value to
  key as well and key to value
@item @code{MapIterator} interface to provide simple and quick iteration over
  maps
@item Transforming decorators that alter each object as it is added to the
  collection
@item Composite collections that make multiple collections look like one
@item Ordered maps and sets that retain the order elements are added in,
  including an LRU based map
@item Reference map that allows keys and/or values to be garbage collected
  under close control
@item Many comparator implementations
@item Many iterator implementations
@item Adapter classes from array and enumerations to collections
@item Utilities to test or create typical set-theory properties of collections
  such as union, intersection, and closure.
@end itemize\n")
    (license license:asl2.0)))

(define-public java-commons-collections
  (package
    (inherit java-commons-collections4)
    (name "java-commons-collections")
    (version "3.2.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/commons/collections/source/"
                                  "commons-collections-" version "-src.tar.gz"))
              (sha256
               (base32
                "055r51a5lfc3z7rkxnxmnn1npvkvda7636hjpm4qk7cnfzz98387"))
              (patches
               (search-patches "java-commons-collections-fix-java8.patch"))))
    (arguments
      (substitute-keyword-arguments (package-arguments java-commons-collections4)
        ((#:phases phases)
          `(modify-phases ,phases
            ;; The manifest is required by the build procedure
            (add-before 'build 'add-manifest
              (lambda _
                (mkdir-p "build/conf")
                (call-with-output-file "build/conf/MANIFEST.MF"
                  (lambda (file)
                    (format file "Manifest-Version: 1.0\n")))
                #t))
            (replace 'install
              (install-jars "build"))))))))

(define java-commons-collections-test-classes
  (package
    (inherit java-commons-collections)
    (arguments
     `(#:jar-name "commons-collections-test-classes.jar"
       #:source-dir "src/test"
       #:tests? #f))
    (inputs
     `(("collection" ,java-commons-collections)))))

(define-public java-commons-beanutils
  (package
    (name "java-commons-beanutils")
    (version "1.9.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/commons/beanutils/source/"
                                  "commons-beanutils-" version "-src.tar.gz"))
              (sha256
               (base32
                "03cs0bq3sl1sdc7py9g3qnf8n9h473nrkvd3d251kaqv6a2ab7qk"))))
    (build-system ant-build-system)
    (arguments
     `(#:test-target "test"
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (rename-file (string-append "dist/commons-beanutils-" ,version
                                         "-SNAPSHOT.jar")
                          "commons-beanutils.jar")
             (install-file "commons-beanutils.jar"
               (string-append (assoc-ref outputs "out") "/share/java/"))
             #t)))))
    (inputs
     `(("logging" ,java-commons-logging-minimal)
       ("collections" ,java-commons-collections)))
    (native-inputs
     `(("junit" ,java-junit)
       ("collections-test" ,java-commons-collections-test-classes)))
    (home-page "https://commons.apache.org/beanutils/")
    (synopsis "Dynamically set or get properties in Java")
    (description "BeanUtils provides a simplified interface to reflection and
introspection to set or get dynamically determined properties through their
setter and getter method.")
    (license license:asl2.0)))

(define-public java-commons-io
  (package
    (name "java-commons-io")
    (version "2.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://apache/commons/io/source/"
                           "commons-io-" version "-src.tar.gz"))
       (sha256
        (base32
         "0q5y41jrcjvx9hzs47x5kdhnasdy6rm4bzqd2jxl02w717m7a7v3"))))
    (build-system ant-build-system)
    (outputs '("out" "doc"))
    (arguments
     `(#:test-target "test"
       #:make-flags
       ,#~(list (string-append "-Djunit.jar="
                               (car (find-files #$(this-package-native-input "java-junit")
                                                "jar$"))))
       #:phases
       (modify-phases %standard-phases
         (add-after 'build 'build-javadoc ant-build-javadoc)
         (replace 'install (install-from-pom "pom.xml"))
         (add-after 'install 'install-doc (install-javadoc "target/apidocs")))))
    (native-inputs
     (list java-junit java-hamcrest-core))
    (propagated-inputs
     (list apache-commons-parent-pom-39))
    (home-page "https://commons.apache.org/io/")
    (synopsis "Common useful IO related classes")
    (description "Commons-IO contains utility classes, stream implementations,
file filters and endian classes.")
    (license license:asl2.0)))

(define-public java-commons-exec-1.1
  (package
    (name "java-commons-exec")
    (version "1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://apache/commons/exec/source/"
                           "commons-exec-" version "-src.tar.gz"))
       (sha256
        (base32
         "025dk8xgj10lxwwwqp0hng2rn7fr4vcirxzydqzx9k4dim667alk"))))
    (build-system ant-build-system)
    (arguments
     `(#:test-target "test"
       #:make-flags
       ,#~(list (string-append "-Dmaven.junit.jar="
                               (car (find-files #$(this-package-native-input "java-junit")
                                                "jar$"))))
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'delete-network-tests
           (lambda _
             (delete-file "src/test/java/org/apache/commons/exec/DefaultExecutorTest.java")
             (substitute* "src/test/java/org/apache/commons/exec/TestRunner.java"
              (("suite\\.addTestSuite\\(DefaultExecutorTest\\.class\\);") ""))
             #t))
         ;; The "build" phase automatically tests.
         (delete 'check)
         (replace 'install (install-jars "target")))))
    (native-inputs
     (list java-junit))
    (home-page "https://commons.apache.org/proper/commons-exec/")
    (synopsis "Common program execution related classes")
    (description "Commons-Exec simplifies executing external processes.")
    (license license:asl2.0)))

(define-public java-commons-exec
  (package
    (inherit java-commons-exec-1.1)
    (version "1.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://apache/commons/exec/source/"
                           "commons-exec-" version "-src.tar.gz"))
       (sha256
        (base32
         "17yb4h6f8l49c5iyyvda4z2nmw0bxrx857nrwmsr7mmpb7x441yv"))))
    (arguments
     `(#:test-target "test"
       #:make-flags
       ,#~(list (string-append "-Dmaven.junit.jar="
                               (car (find-files #$(this-package-native-input "java-junit")
                                                "jar$")))
                "-Dmaven.compiler.source=1.7"
                "-Dmaven.compiler.target=1.7")
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'delete-network-tests
           (lambda* (#:key inputs #:allow-other-keys)
             ;; This test hangs indefinitely.
             (delete-file "src/test/java/org/apache/commons/exec/issues/Exec60Test.java")
             (substitute* "src/test/java/org/apache/commons/exec/issues/Exec41Test.java"
              (("ping -c 10 127.0.0.1") "sleep 10"))
             (substitute* "src/test/java/org/apache/commons/exec/issues/Exec49Test.java"
              (("/bin/ls") "ls"))
             (call-with-output-file "src/test/scripts/ping.sh"
               (lambda (port)
                 (format port "#!~a/bin/sh\nsleep $1\n"
                              (assoc-ref inputs "bash"))))
             #t))
         ;; The "build" phase automatically tests.
         (delete 'check)
         (replace 'install (install-jars "target")))))
    (native-inputs
     (list java-junit java-hamcrest-core))))

(define-public java-commons-lang
  (package
    (name "java-commons-lang")
    (version "2.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://apache/commons/lang/source/"
                           "commons-lang-" version "-src.tar.gz"))
       (sha256
        (base32 "1mxwagqadzx1b2al7i0z1v0r235aj2njdyijf02szq0vhmqrfiq5"))))
    (build-system ant-build-system)
    (outputs '("out" "doc"))
    (arguments
     `(#:test-target "test"
       #:test-exclude (list "**/Abstract*.java" "**/Random*.java")
       #:phases
       (modify-phases %standard-phases
         (add-after 'build 'build-javadoc ant-build-javadoc)
         (add-before 'check 'disable-failing-test
           (lambda _
             ;; Disable a failing test
             (substitute* "src/test/java/org/apache/commons/lang/\
time/FastDateFormatTest.java"
               (("public void testFormat\\(\\)")
                "public void disabled_testFormat()"))
             #t))
         (replace 'install (install-jars "target"))
         (add-after 'install 'install-doc (install-javadoc "target/apidocs")))))
    (native-inputs
     (list java-junit))
    (home-page "https://commons.apache.org/lang/")
    (synopsis "Extension of the java.lang package")
    (description "The Commons Lang components contains a set of Java classes
that provide helper methods for standard Java classes, especially those found
in the @code{java.lang} package in the Sun JDK.  The following classes are
included:

@itemize
@item StringUtils - Helper for @code{java.lang.String}.
@item CharSetUtils - Methods for dealing with @code{CharSets}, which are sets
  of characters such as @code{[a-z]} and @code{[abcdez]}.
@item RandomStringUtils - Helper for creating randomised strings.
@item NumberUtils - Helper for @code{java.lang.Number} and its subclasses.
@item NumberRange - A range of numbers with an upper and lower bound.
@item ObjectUtils - Helper for @code{java.lang.Object}.
@item SerializationUtils - Helper for serializing objects.
@item SystemUtils - Utility class defining the Java system properties.
@item NestedException package - A sub-package for the creation of nested
  exceptions.
@item Enum package - A sub-package for the creation of enumerated types.
@item Builder package - A sub-package for the creation of @code{equals},
  @code{hashCode}, @code{compareTo} and @code{toString} methods.
@end itemize\n")
    (license license:asl2.0)))

(define-public java-commons-lang3
  (package
    (name "java-commons-lang3")
    (version "3.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://apache/commons/lang/source/"
                           "commons-lang3-" version "-src.tar.gz"))
       (sha256
        (base32 "09dcv1pkdx3hpf06py8p9511f1wkin6jpacdll0c8vxpbi3yfwzv"))
       (patches
        (search-patches "java-commons-lang-fix-dependency.patch"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "commons-lang3.jar"
       #:source-dir "src/main/java"
       #:tests? #f; require junit5
       #:phases
       (modify-phases %standard-phases
         (replace 'install (install-from-pom "pom.xml")))))
    (propagated-inputs
     (list apache-commons-parent-pom-48))
    (home-page "https://commons.apache.org/lang/")
    (synopsis "Extension of the java.lang package")
    (description "The Commons Lang components contains a set of Java classes
that provide helper methods for standard Java classes, especially those found
in the @code{java.lang} package.  The following classes are included:

@itemize
@item StringUtils - Helper for @code{java.lang.String}.
@item CharSetUtils - Methods for dealing with @code{CharSets}, which are sets of
  characters such as @code{[a-z]} and @code{[abcdez]}.
@item RandomStringUtils - Helper for creating randomised strings.
@item NumberUtils - Helper for @code{java.lang.Number} and its subclasses.
@item NumberRange - A range of numbers with an upper and lower bound.
@item ObjectUtils - Helper for @code{java.lang.Object}.
@item SerializationUtils - Helper for serializing objects.
@item SystemUtils - Utility class defining the Java system properties.
@item NestedException package - A sub-package for the creation of nested
   exceptions.
@item Enum package - A sub-package for the creation of enumerated types.
@item Builder package - A sub-package for the creation of @code{equals},
  @code{hashCode}, @code{compareTo} and @code{toString} methods.
@end itemize\n")
    (license license:asl2.0)))

(define-public java-commons-bsf
  (package
    (name "java-commons-bsf")
    (version "2.4.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/commons/bsf/source/bsf-src-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1sbamr8jl32p1jgf59nw0b2w9qivyg145954hm6ly54cfgsqrdas"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (for-each delete-file
                            (find-files "." "\\.jar$"))
                  #t))))
    (build-system ant-build-system)
    (arguments
     `(#:build-target "jar"
       #:tests? #f; No test file
       #:modules ((guix build ant-build-system)
                  (guix build utils)
                  (guix build java-utils)
                  (sxml simple))
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'create-properties
           (lambda _
             ;; This file is missing from the distribution
             (call-with-output-file "build-properties.xml"
               (lambda (port)
                 (sxml->xml
                  `(project (@ (basedir ".") (name "build-properties") (default ""))
                     (property (@ (name "project.name") (value "bsf")))
                     (property (@ (name "source.level") (value "1.5")))
                     (property (@ (name "build.lib") (value "build/jar")))
                     (property (@ (name "src.dir") (value "src")))
                     (property (@ (name "tests.dir") (value "src/org/apache/bsf/test")))
                     (property (@ (name "build.tests") (value "build/test-classes")))
                     (property (@ (name "build.dest") (value "build/classes"))))
                  port)))
             #t))
         (replace 'install (install-jars "build")))))
    (native-inputs
     (list java-junit))
    (inputs
     (list java-commons-logging-minimal))
    (home-page "https://commons.apache.org/proper/commons-bsf")
    (synopsis "Bean Scripting Framework")
    (description "The Bean Scripting Framework (BSF) is a set of Java classes
which provides scripting language support within Java applications, and access
to Java objects and methods from scripting languages.  BSF allows one to write
JSPs in languages other than Java while providing access to the Java class
library.  In addition, BSF permits any Java application to be implemented in
part (or dynamically extended) by a language that is embedded within it.  This
is achieved by providing an API that permits calling scripting language engines
from within Java, as well as an object registry that exposes Java objects to
these scripting language engines.")
    (license license:asl2.0)))

(define-public java-commons-jxpath
  (package
    (name "java-commons-jxpath")
    (version "1.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/commons/jxpath/source/"
                                  "commons-jxpath-" version "-src.tar.gz"))
              (sha256
               (base32
                "1rpgg31ayn9fwr4bfi2i1ij0npcg79ad2fv0w9hacvawsyc42cfs"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "commons-jxpath.jar"
       ;; tests require more dependencies, including mockrunner which depends on old software
       #:tests? #f
       #:source-dir "src/java"))
    (inputs
     `(("servlet" ,java-classpathx-servletapi)
       ("java-jdom" ,java-jdom)
       ("java-commons-beanutils" ,java-commons-beanutils)))
    (native-inputs
     (list java-junit))
    (home-page "https://commons.apache.org/jxpath/")
    (synopsis "Simple interpreter of an expression language called XPath")
    (description "The org.apache.commons.jxpath package defines a simple
interpreter of an expression language called XPath.  JXPath applies XPath
expressions to graphs of objects of all kinds: JavaBeans, Maps, Servlet
contexts, DOM etc, including mixtures thereof.")
    (license license:asl2.0)))

(define-public java-commons-pool
  (package
    (name "java-commons-pool")
    (version "2.6.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/commons/pool/source/"
                                  "commons-pool2-" version "-src.tar.gz"))
              (sha256
               (base32
                "1fi1hgqmq01bs6azbj3sfswxzadp2r8sjjfiq6ryilz1m50kvrv6"))))
    (arguments
     `(#:jar-name "common-pool.jar"
       #:source-dir "src/main/java"
       #:test-exclude
       (list "**/PerformanceTest.java")))
    (build-system ant-build-system)
    (inputs
     (list java-cglib))
    (native-inputs
     (list java-junit java-hamcrest-core java-asm java-objenesis))
    (home-page "https://commons.apache.org/proper/commons-pool/")
    (synopsis "Object-pooling API in Java")
    (description "The commons-pool package provides an object-pooling API
and a number of object pool implementations.  This package defines a
handful of pooling interfaces and some base classes that may be useful when
creating new pool implementations.")
    (license license:asl2.0)))

(define-public java-commons-dbcp
  (package
    (name "java-commons-dbcp")
    (version "2.6.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/commons/dbcp/source/"
                                  "commons-dbcp2-" version "-src.tar.gz"))
              (sha256
               (base32
                "0axbvcbnf2l70fc3ybrlp3siw2w4ka9mia2pnx4py8gz54cpw3rc"))))
    (arguments
     `(#:source-dir "src/main/java"
       #:jar-name "java-commons-dbcp.jar"
       #:tests? #f)); requires apache-geronimo
    (inputs
     `(("java-commons-pool" ,java-commons-pool)
       ("java-commons-logging" ,java-commons-logging-minimal)
       ("java-jboss-transaction-api-spec" ,java-jboss-transaction-api-spec)))
    (native-inputs
     (list java-junit))
    (build-system ant-build-system)
    (home-page "https://commons.apache.org/proper/commons-dbcp/")
    (synopsis "Database Connection Pool for Java")
    (description "Commons-dbcp allows you to share a pool of database
connections between users.  Creating a new connection for each user can be
time consuming and even unfeasible when the number of simultaneous users is
very large.  This package provides a way to share a poole of connections to
reduce that load.")
    (license license:asl2.0)))

(define-public java-commons-jcs
  (package
    (name "java-commons-jcs")
    (version "3.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/commons/jcs/source/"
                                  "commons-jcs3-dist-" version "-src.tar.gz"))
              (sha256
               (base32
                "0y1lm1xnsj99bf7y9mkvbzqfy8dr7ac8zcbkpsjgzb9vhabfsbac"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "commons-jcs.jar"
       #:source-dir "commons-jcs-core/src/main/java"
       #:test-dir "commons-jcs-core/src/test"
       #:tests? #f; requires hsqldb
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "commons-jcs-core/src/main/resources"
                               "build/classes"))))))
    (propagated-inputs
     (list java-classpathx-servletapi
           java-commons-dbcp
           java-httpcomponents-httpclient
           java-httpcomponents-httpcore
           java-log4j-api))
    (native-inputs
     (list java-junit))
    (home-page "https://commons.apache.org/proper/commons-jcs/")
    (synopsis "Distributed caching system in Java")
    (description "JCS is a distributed caching system written in Java.  It
is intended to speed up applications by providing a means to manage cached
data of various dynamic natures.  Like any caching system, JCS is most useful
for high read, low put applications.  Latency times drop sharply and
bottlenecks move away from the database in an effectively cached system.")
    (license license:asl2.0)))

(define-public java-jsr250
  (package
    (name "java-jsr250")
    (version "1.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/javaee/javax.annotation")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1g22a9d75g01s9yxgdig0ss7i30j4ysnnp08gn4krn0wly4lpqq0"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f ; no tests included
       #:jdk ,icedtea-8
       #:jar-name "jsr250.jar"
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (install-from-pom "pom.xml")))))
    (propagated-inputs (list java-jvnet-parent-pom-3))
    (home-page "https://jcp.org/en/jsr/detail?id=250")
    (synopsis "Security-related annotations")
    (description "This package provides annotations for security.  It provides
packages in the @code{javax.annotation} and @code{javax.annotation.security}
namespaces.")
    ;; either cddl or gpl2 only, with classpath exception
    (license (list license:cddl1.0
                   license:gpl2))))

(define-public java-jsr305
  (package
    (name "java-jsr305")
    (version "3.0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://repo1.maven.org/maven2/"
                                  "com/google/code/findbugs/"
                                  "jsr305/" version "/jsr305-"
                                  version "-sources.jar"))
              (sha256
               (base32
                "1rh6jin9v7jqpq3kf1swl868l8i94r636n03pzpsmgr8v0lh9j2n"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f ; no tests included
       #:jar-name "jsr305.jar"
       #:phases
       (modify-phases %standard-phases
         (add-before 'install 'create-pom
           (generate-pom.xml "pom.xml" "com.google.code.findbugs" "jsr305" ,version))
         (replace 'install
           (install-from-pom "pom.xml")))))
    (home-page "https://findbugs.sourceforge.net/")
    (synopsis "Annotations for the static analyzer called findbugs")
    (description "This package provides annotations for the findbugs package.
It provides packages in the @code{javax.annotations} namespace.")
    (license license:asl2.0)))

(define-public java-error-prone-annotations
  (package
    (name "java-error-prone-annotations")
    (version "2.18.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/google/error-prone")
                     (commit (string-append "v" version))))
              (file-name (git-file-name "java-error-prone" version))
              (sha256
               (base32
                "19sqsz0b308rhadr3ff10azdbqjq37nvrn9c06224dwpxap0931f"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f; no tests
       #:jar-name (string-append ,name "-" ,version ".jar")
       #:source-dir "annotations/src/main/java"
       #:test-dir "annotations/src/altest"
       #:phases
       (modify-phases %standard-phases
         (replace 'install (install-from-pom "annotations/pom.xml")))))
    (propagated-inputs (list java-error-prone-parent-pom java-jsr305))
    (home-page "https://errorprone.info")
    (synopsis "Java static analyzer at compile-time")
    (description "Error Prone is a static analysis tool for Java that catches
common programming mistakes at compile-time.  This package contains annotations
used by programmers to guide the static analysis.")
    (license license:asl2.0)))

(define java-error-prone-parent-pom
  (package
    (inherit java-error-prone-annotations)
    (name "java-error-prone-parent-pom")
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (replace 'install
           (install-pom-file "pom.xml")))))
    (propagated-inputs '())))

(define-public java-guava
  (package
    (name "java-guava")
    (version "31.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/google/guava/")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0sv1w5cnids9ad3l7qhrh3dh1wdqwc946iinsxryafr25wg5z1lp"))
              (patches
               (search-patches "java-guava-remove-annotation-deps.patch"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f                      ; no tests included
       #:jar-name "guava.jar"
       #:source-dir "guava/src"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'trim-sources
           (lambda _
             (with-directory-excursion "guava/src/com/google/common"
               ;; Remove annotations to avoid extra dependencies:
               ;; * "j2objc" annotations are used when converting Java to
               ;;   Objective C;
               ;; * "IgnoreJRERequirement" is used for Android.
               ;; * "Nullable" is used to catch NPE at build time.
               (substitute* (find-files "." "\\.java$")
                 (("import com.google.j2objc.*") "")
                 (("import org.checkerframework.checker.*") "")
                 (("@ReflectionSupport.*") "")
                 (("@WeakOuter") "")
                 (("@RetainedWith") "")
                 (("@Weak") "")
                 (("@J2ObjCIncompatible") "")
                 (("@IgnoreJRERequirement") "")
                 (("@Nullable") "")))))
         ;; This is required by guava, but this is just an empty stub
         (add-before 'install 'install-listenablefuture-stub
           (install-pom-file "futures/listenablefuture9999/pom.xml"))
         (replace 'install (install-from-pom "guava/pom.xml")))))
    (inputs
     (list java-error-prone-annotations java-jsr305))
    (propagated-inputs
     (list java-guava-futures-failureaccess java-guava-parent-pom))
    (home-page "https://github.com/google/guava")
    (synopsis "Google core libraries for Java")
    (description "Guava is a set of core libraries that includes new
collection types (such as multimap and multiset), immutable collections, a
graph library, functional types, an in-memory cache, and APIs/utilities for
concurrency, I/O, hashing, primitives, reflection, string processing, and much
more!")
    (license license:asl2.0)))

(define-public java-guava-futures-failureaccess
  (package
    (inherit java-guava)
    (name "java-guava-futures-failureaccess")
    (arguments
     `(#:tests? #f; no tests
       #:jar-name "guava-futures-failureaccess.jar"
       #:source-dir "futures/failureaccess/src"
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (install-from-pom "futures/failureaccess/pom.xml")))))
    (propagated-inputs
     `(("java-sonatype-oss-parent-pom" ,java-sonatype-oss-parent-pom-7)))))

(define java-guava-parent-pom
  (package
    (inherit java-guava)
    (name "java-guava-parent-pom")
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (replace 'install
           (install-pom-file "pom.xml")))))
    (propagated-inputs
     `(("java-sonatype-oss-parent-pom" ,java-sonatype-oss-parent-pom-7)))))

;; The java-commons-logging package provides adapters to many different
;; logging frameworks.  To avoid an excessive dependency graph we try to build
;; it with only a minimal set of adapters.
(define-public java-commons-logging-minimal
  (package
    (name "java-commons-logging-minimal")
    (version "1.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/commons/logging/source/"
                                  "commons-logging-" version "-src.tar.gz"))
              (sha256
               (base32
                "10bwcy5w8d7y39n0krlwhnp8ds3kj5zhmzj0zxnkw0qdlsjmsrj9"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f ; avoid dependency on logging frameworks
       #:jar-name "commons-logging-minimal.jar"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'delete-adapters-and-tests
           (lambda _
             ;; Delete all adapters except for NoOpLog, SimpleLog, and
             ;; LogFactoryImpl.  NoOpLog is required to build; LogFactoryImpl
             ;; is used by applications; SimpleLog is the only actually usable
             ;; implementation that does not depend on another logging
             ;; framework.
             (for-each
              (lambda (file)
                (delete-file (string-append
                              "src/main/java/org/apache/commons/logging/impl/" file)))
              (list "Jdk13LumberjackLogger.java"
                    "WeakHashtable.java"
                    "Log4JLogger.java"
                    "ServletContextCleaner.java"
                    "Jdk14Logger.java"
                    "AvalonLogger.java"
                    "LogKitLogger.java"))
             (delete-file-recursively "src/test")
             #t)))))
    (home-page "https://commons.apache.org/logging/")
    (synopsis "Common API for logging implementations")
    (description "The Logging package is a thin bridge between different
logging implementations.  A library that uses the commons-logging API can be
used with any logging implementation at runtime.")
    (license license:asl2.0)))

;; This is the last release of the 1.x series.
(define-public java-mockito-1
  (package
    (name "java-mockito")
    (version "1.10.19")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://repo1.maven.org/maven2/"
                                  "org/mockito/mockito-core/" version
                                  "/mockito-core-" version "-sources.jar"))
              (sha256
               (base32
                "0vmiwnwpf83g2q7kj1rislmja8fpvqkixjhawh7nxnygx6pq11kc"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "mockito.jar"
       #:tests? #f ; no tests included
       ;; FIXME: patch-and-repack does not support jars, so we have to apply
       ;; patches in build phases.
       #:phases
       (modify-phases %standard-phases
         ;; Mockito was developed against a different version of hamcrest,
         ;; which does not require matcher implementations to provide an
         ;; implementation of the "describeMismatch" method.  We add this
         ;; simple definition to pass the build with our version of hamcrest.
         (add-after 'unpack 'fix-hamcrest-build-error
           (lambda _
             (substitute* "src/org/mockito/internal/matchers/LocalizedMatcher.java"
               (("public Matcher getActualMatcher\\(\\) .*" line)
                (string-append "
    public void describeMismatch(Object item, Description description) {
        actualMatcher.describeMismatch(item, description);
    }"
                               line)))
             #t))
         ;; Mockito bundles cglib.  We have a cglib package, so let's use
         ;; that instead.
         (add-after 'unpack 'use-system-libraries
           (lambda _
             (with-directory-excursion "src/org/mockito/internal/creation/cglib"
               (substitute* '("CGLIBHacker.java"
                              "CglibMockMaker.java"
                              "ClassImposterizer.java"
                              "DelegatingMockitoMethodProxy.java"
                              "MethodInterceptorFilter.java"
                              "MockitoNamingPolicy.java"
                              "SerializableMockitoMethodProxy.java"
                              "SerializableNoOp.java")
                 (("import org.mockito.cglib") "import net.sf.cglib")))
             #t)))))
    (inputs
     (list java-junit java-objenesis java-cglib java-hamcrest-core))
    (home-page "http://mockito.org")
    (synopsis "Mockito is a mock library for Java")
    (description "Mockito is a mocking library for Java which lets you write
tests with a clean and simple API.  It generates mocks using reflection, and
it records all mock invocations, including methods arguments.")
    (license license:asl2.0)))

(define-public java-httpcomponents-httpcore
  (package
    (name "java-httpcomponents-httpcore")
    (version "4.4.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache//httpcomponents/httpcore/"
                                  "source/httpcomponents-core-"
                                  version "-src.tar.gz"))
              (sha256
               (base32
                "02bwcf38y4vgwq7kj2s6q7qrmma641r5lacivm16kgxvb2j6h1vy"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "httpcomponents-httpcore.jar"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _ (chdir "httpcore") #t)))))
    (inputs
     (list java-commons-logging-minimal java-commons-lang3))
    (native-inputs
     (list java-junit java-mockito-1))
    (home-page "https://hc.apache.org/httpcomponents-core-4.4.x/index.html")
    (synopsis "Low level HTTP transport components")
    (description "HttpCore is a set of low level HTTP transport components
that can be used to build custom client and server side HTTP services with a
minimal footprint.  HttpCore supports two I/O models: blocking I/O model based
on the classic Java I/O and non-blocking, event driven I/O model based on Java
NIO.

This package provides the blocking I/O model library.")
    (license license:asl2.0)))

(define-public java-httpcomponents-httpcore-nio
  (package (inherit java-httpcomponents-httpcore)
    (name "java-httpcomponents-httpcore-nio")
    (arguments
     `(#:jar-name "httpcomponents-httpcore-nio.jar"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _ (chdir "httpcore-nio") #t)))))
    (inputs
     (modify-inputs (package-inputs java-httpcomponents-httpcore)
       (prepend java-httpcomponents-httpcore java-hamcrest-core)))
    (description "HttpCore is a set of low level HTTP transport components
that can be used to build custom client and server side HTTP services with a
minimal footprint.  HttpCore supports two I/O models: blocking I/O model based
on the classic Java I/O and non-blocking, event driven I/O model based on Java
NIO.

This package provides the non-blocking I/O model library based on Java
NIO.")))

(define-public java-httpcomponents-httpcore-ab
  (package (inherit java-httpcomponents-httpcore)
    (name "java-httpcomponents-httpcore-ab")
    (arguments
     `(#:jar-name "httpcomponents-httpcore-ab.jar"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _ (chdir "httpcore-ab") #t)))))
    (inputs
     (modify-inputs (package-inputs java-httpcomponents-httpcore)
       (prepend java-httpcomponents-httpcore java-commons-cli
                java-hamcrest-core)))
    (synopsis "Apache HttpCore benchmarking tool")
    (description "This package provides the HttpCore benchmarking tool.  It is
an Apache AB clone based on HttpCore.")))

(define-public java-httpcomponents-httpcore-osgi
  (package (inherit java-httpcomponents-httpcore)
    (name "java-httpcomponents-httpcore-osgi")
    (arguments
     `(#:jar-name "httpcomponents-httpcore-osgi.jar"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _ (chdir "httpcore-osgi"))))))
    (inputs
     (modify-inputs (package-inputs java-httpcomponents-httpcore)
                    (prepend java-httpcomponents-httpcore
                             java-httpcomponents-httpcore-nio
                             java-hamcrest-core)))
    (native-inputs (list java-ops4j-pax-exam-core
                         java-ops4j-pax-exam-core-junit
                         java-ops4j-pax-exam-core-spi
                         java-junit))
    (description "HttpCore is a set of low level HTTP transport components
that can be used to build custom client and server side HTTP services with a
minimal footprint.  HttpCore supports two I/O models: blocking I/O model based
on the classic Java I/O and non-blocking, event driven I/O model based on Java
NIO.

This package provides... some tests.")))

(define-public java-httpcomponents-httpclient
  (package
    (name "java-httpcomponents-httpclient")
    (version "4.5.12")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/httpcomponents/httpclient/"
                                  "source/httpcomponents-client-"
                                  version "-src.tar.gz"))
              (sha256
               (base32
                "1va99m2zc2liv0v9vn72p5ja8yz4s5wq7zpahaai5nr966kvxzkb"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "httpcomponents-httpclient.jar"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _ (chdir "httpclient") #t)))))
    (inputs
     (list java-commons-logging-minimal
           java-commons-codec
           java-hamcrest-core
           java-httpcomponents-httpcore
           java-mockito-1
           java-junit))
    (home-page "https://hc.apache.org/httpcomponents-client-ga/")
    (synopsis "HTTP client library for Java")
    (description "Although the @code{java.net} package provides basic
functionality for accessing resources via HTTP, it doesn't provide the full
flexibility or functionality needed by many applications.  @code{HttpClient}
seeks to fill this void by providing an efficient, up-to-date, and
feature-rich package implementing the client side of the most recent HTTP
standards and recommendations.")
    (license license:asl2.0)))

(define-public java-httpcomponents-httpclient-cache
  (package (inherit java-httpcomponents-httpclient)
    (name "java-httpcomponents-httpclient-cache")
    (arguments
     `(#:jar-name "httpcomponents-httpclient-cache.jar"
       #:source-dir "src/main/java"
       #:test-dir "src/test"
       #:tests? #f; tests are broken with current cglib.
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'delete-unused-impls
           (lambda _
             (for-each
              delete-file-recursively
              '("src/main/java/org/apache/http/impl/client/cache/ehcache/"
                "src/main/java/org/apache/http/impl/client/cache/memcached/"
                "src/test/java/org/apache/http/impl/client/cache/ehcache/"
                "src/test/java/org/apache/http/impl/client/cache/memcached/"))))
         (add-after 'unpack 'chdir
           (lambda _ (chdir "httpclient-cache"))))))
    (inputs
     (modify-inputs (package-inputs java-httpcomponents-httpclient)
                    (prepend java-httpcomponents-httpclient
                             java-httpcomponents-httpmime
                             java-hamcrest-core)))
    (description "This package provides an API for caching accessed HTTP
resources.")))

(define-public java-httpcomponents-httpclient-osgi
  (package (inherit java-httpcomponents-httpclient)
    (name "java-httpcomponents-httpclient-osgi")
    (arguments
     `(#:jar-name "httpcomponents-httpclient-osgi.jar"
       #:tests? #f; tests are broken with current cglib.
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _ (chdir "httpclient-osgi") #t)))))
    (inputs
     (modify-inputs (package-inputs java-httpcomponents-httpclient)
                    (prepend java-httpcomponents-httpclient
                             java-httpcomponents-httpclient-cache
                             java-osgi-framework
                             java-osgi-service-cm
                             java-hamcrest-core)))))

(define-public java-httpcomponents-httpmime
  (package (inherit java-httpcomponents-httpclient)
    (name "java-httpcomponents-httpmime")
    (arguments
     `(#:jar-name "httpcomponents-httpmime.jar"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _ (chdir "httpmime") #t)))))
    (inputs
     (list java-httpcomponents-httpclient java-httpcomponents-httpcore
           java-junit java-hamcrest-core))))

(define-public java-commons-net
  (package
    (name "java-commons-net")
    (version "3.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/commons/net/source/"
                                  "commons-net-" version "-src.tar.gz"))
              (sha256
               (base32
                "0n0cmnddk9qdqhjvka8pc6hd9mn2qi3166f1s6xk32h7rfy1adxr"))))
    (build-system ant-build-system)
    (arguments
     `(;; FIXME: MainTest.java tries to read "examples.properties" (which
       ;; should be "resources/examples/examples.properties"), but gets "null"
       ;; instead.
       #:tests? #f
       #:jar-name "commons-net.jar"))
    (native-inputs
     (list java-junit java-hamcrest-core))
    (home-page "https://commons.apache.org/net/")
    (synopsis "Client library for many basic Internet protocols")
    (description "The Apache Commons Net library implements the client side of
many basic Internet protocols.  The purpose of the library is to provide
fundamental protocol access, not higher-level abstractions.")
    (license license:asl2.0)))

(define-public java-jsch
  (package
    (name "java-jsch")
    (version "0.1.55")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/jsch/jsch/"
                                  version "/jsch-" version ".zip"))
              (sha256
               (base32
                "1lxyjwvmwa723wcf3bqn816hkvc03vz4xhbsi7bvfhrz2rpgcfq6"))))
    (build-system ant-build-system)
    (arguments
     `(#:build-target "dist"
       #:tests? #f                      ; no tests included
       #:phases
       (modify-phases %standard-phases
         (replace 'install (install-jars "dist")))))
    (native-inputs
     (list unzip))
    (home-page "http://www.jcraft.com/jsch/")
    (synopsis "Pure Java implementation of SSH2")
    (description "JSch is a pure Java implementation of SSH2.  JSch allows you
to connect to an SSH server and use port forwarding, X11 forwarding, file
transfer, etc., and you can integrate its functionality into your own Java
programs.")
    (license license:bsd-3)))

(define-public java-commons-compress
  (package
    (name "java-commons-compress")
    (version "1.21")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/commons/compress/source/"
                                  "commons-compress-" version "-src.tar.gz"))
              (sha256
               (base32
                "1rkpb6xcyly1wnbx4q6iq6p5hrr0h1d0ppb5r07psc75cbmizjry"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "commons-compress.jar"
       #:source-dir "src/main/java"
       #:tests? #f; requires java-mockito-3
       #:phases
       (modify-phases %standard-phases
         (replace 'install (install-from-pom "pom.xml")))))
    (propagated-inputs
     (list java-asm-3
           java-brotli
           java-osgi-core
           java-xz
           java-zstd
           apache-commons-parent-pom-52))
    (home-page "https://commons.apache.org/proper/commons-compress/")
    (synopsis "Java library for working with compressed files")
    (description "The Apache Commons Compress library defines an API for
working with compressed files such as ar, cpio, Unix dump, tar, zip, gzip, XZ,
Pack200, bzip2, 7z, arj, lzma, snappy, DEFLATE, lz4 and Z files.")
    (license license:asl2.0)))

(define-public java-commons-csv
  (package
    (name "java-commons-csv")
    (version "1.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/commons/csv/source/"
                                  "commons-csv-" version "-src.tar.gz"))
              (sha256
               (base32
                "1l89m0fm2s3xx3v3iynvangymfg2vlyngaj6fgsi457nmsw7m7ij"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "commons-csv.jar"
       #:source-dir "src/main/java"
       #:tests? #f)); FIXME: requires java-h2
    (inputs
     `(("java-hamcrest-core" ,java-hamcrest-core)
       ("java-commons-io" ,java-commons-io)
       ("java-commons-lang3" ,java-commons-lang3)
       ("junit" ,java-junit)))
    (home-page "https://commons.apache.org/proper/commons-csv/")
    (synopsis "Read and write CSV documents")
    (description "Commons CSV reads and writes files in variations of the Comma
Separated Value (CSV) format.  The most common CSV formats are predefined in the
CSVFormat class:

@itemize
@item Microsoft Excel
@item Informix UNLOAD
@item Informix UNLOAD CSV
@item MySQL
@item RFC 4180
@item TDF
@end itemize

Custom formats can be created using a fluent style API.")
    (license license:asl2.0)))

(define-public java-osgi-annotation
  (package
    (name "java-osgi-annotation")
    (version "6.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://repo1.maven.org/maven2/"
                                  "org/osgi/org.osgi.annotation/" version "/"
                                  "org.osgi.annotation-" version "-sources.jar"))
              (sha256
               (base32
                "1q718mb7gqg726rh6pc2hcisn8v50nv35abbir0jypmffhiii85w"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f ; no tests
       #:jar-name "osgi-annotation.jar"
       #:phases
       (modify-phases %standard-phases
         (add-before 'install 'create-pom
           (generate-pom.xml "pom.xml" "osgi" "osgi-annotation" ,version))
         (replace 'install (install-from-pom "pom.xml")))))
    (home-page "https://www.osgi.org")
    (synopsis "Annotation module of OSGi framework")
    (description
     "OSGi, for Open Services Gateway initiative framework, is a module system
and service platform for the Java programming language.  This package contains
the OSGi annotation module, providing additional services to help dynamic
components.")
    (license license:asl2.0)))

(define-public java-osgi-core
  (package
    (name "java-osgi-core")
    (version "6.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://repo1.maven.org/maven2/"
                                  "org/osgi/org.osgi.core/" version "/"
                                  "org.osgi.core-" version "-sources.jar"))
              (sha256
               (base32
                "19bpf5jx32jq9789gyhin35q5v7flmw0p9mk7wbgqpxqfmxyiabv"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f ; no tests
       #:jar-name "osgi-core.jar"
       #:phases
       (modify-phases %standard-phases
         (add-before 'install 'create-pom
           (generate-pom.xml "pom.xml" "org.osgi" "org.osgi.core" ,version))
         (replace 'install (install-from-pom "pom.xml")))))
    (inputs
     (list java-osgi-annotation))
    (home-page "https://www.osgi.org")
    (synopsis "Core module of OSGi framework")
    (description
     "OSGi, for Open Services Gateway initiative framework, is a module system
and service platform for the Java programming language.  This package contains
the OSGi Core module.")
    (license license:asl2.0)))

(define-public java-osgi-service-event
  (package
    (name "java-osgi-service-event")
    (version "1.3.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://repo1.maven.org/maven2/"
                                  "org/osgi/org.osgi.service.event/"
                                  version "/org.osgi.service.event-"
                                  version "-sources.jar"))
              (sha256
               (base32
                "1nyhlgagwym75bycnjczwbnpymv2iw84zbhvvzk84g9q736i6qxm"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f ; no tests
       #:jar-name "osgi-service-event.jar"))
    (inputs
     (list java-osgi-annotation java-osgi-core))
    (home-page "https://www.osgi.org")
    (synopsis "OSGi service event module")
    (description
     "OSGi, for Open Services Gateway initiative framework, is a module system
and service platform for the Java programming language.  This package contains
the OSGi @code{org.osgi.service.event} module.")
    (license license:asl2.0)))

(define-public java-eclipse-osgi
  (package
    (name "java-eclipse-osgi")
    (version "3.11.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://repo1.maven.org/maven2/"
                                  "org/eclipse/platform/org.eclipse.osgi/"
                                  version "/org.eclipse.osgi-"
                                  version "-sources.jar"))
              (sha256
               (base32
                "00cqc6lb29n0zv68b4l842vzkwawvbr7gshfdygsk8sicvcq2c7b"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f ; no tests included
       #:jar-name "eclipse-equinox-osgi.jar"))
    (inputs
     (list java-osgi-annotation))
    (home-page "https://www.eclipse.org/equinox/")
    (synopsis "Eclipse Equinox OSGi framework")
    (description "This package provides an implementation of the OSGi Core
specification.")
    (license license:epl1.0)))

(define-public java-eclipse-equinox-common
  (package
    (name "java-eclipse-equinox-common")
    (version "3.10.200")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://repo1.maven.org/maven2/"
                                  "org/eclipse/platform/org.eclipse.equinox.common/"
                                  version "/org.eclipse.equinox.common-"
                                  version "-sources.jar"))
              (sha256
               (base32
                "1yn8ij6xsljlf35sr2l7wvyvc0ss4n1rv0ry5zkgb49dj4hyrqrj"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f ; no tests included
       #:jar-name "eclipse-equinox-common.jar"))
    (inputs
     (list java-eclipse-osgi))
    (home-page "https://www.eclipse.org/equinox/")
    (synopsis "Common Eclipse runtime")
    (description "This package provides the common Eclipse runtime.")
    (license license:epl1.0)))

(define-public java-eclipse-core-jobs
  (package
    (name "java-eclipse-core-jobs")
    (version "3.8.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://repo1.maven.org/maven2/"
                                  "org/eclipse/platform/org.eclipse.core.jobs/"
                                  version "/org.eclipse.core.jobs-"
                                  version "-sources.jar"))
              (sha256
               (base32
                "0395b8lh0km8vhzjnchvs1rii1qz48hyvb2wqfaq4yhklbwihq4b"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f ; no tests included
       #:jar-name "eclipse-core-jobs.jar"))
    (inputs
     (list java-eclipse-equinox-common java-eclipse-osgi))
    (home-page "https://www.eclipse.org/equinox/")
    (synopsis "Eclipse jobs mechanism")
    (description "This package provides the Eclipse jobs mechanism.")
    (license license:epl1.0)))

(define-public java-eclipse-equinox-registry
  (package
    (name "java-eclipse-equinox-registry")
    (version "3.6.100")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://repo1.maven.org/maven2/"
                                  "org/eclipse/platform/org.eclipse.equinox.registry/"
                                  version "/org.eclipse.equinox.registry-"
                                  version "-sources.jar"))
              (sha256
               (base32
                "1i9sgymh2fy5vdgk5y7s3qvrlbgh4l93ddqi3v4zmca7hwrlhf9k"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f ; no tests included
       #:jar-name "eclipse-equinox-registry.jar"))
    (inputs
     (list java-eclipse-core-jobs java-eclipse-equinox-common
           java-eclipse-osgi))
    (home-page "https://www.eclipse.org/equinox/")
    (synopsis "Eclipse extension registry support")
    (description "This package provides support for the Eclipse extension
registry.")
    (license license:epl1.0)))

(define-public java-eclipse-equinox-app
  (package
    (name "java-eclipse-equinox-app")
    (version "1.3.400")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://repo1.maven.org/maven2/"
                                  "org/eclipse/platform/org.eclipse.equinox.app/"
                                  version "/org.eclipse.equinox.app-"
                                  version "-sources.jar"))
              (sha256
               (base32
                "0nhvbp93y203ar7y59gb0mz3w2d3jlqhr0c9hii9bcfpmr7imdab"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f ; no tests included
       #:jar-name "eclipse-equinox-app.jar"))
    (inputs
     (list java-eclipse-equinox-common java-eclipse-equinox-registry
           java-eclipse-osgi java-osgi-service-event))
    (home-page "https://www.eclipse.org/equinox/")
    (synopsis "Equinox application container")
    (description "This package provides the Equinox application container for
Eclipse.")
    (license license:epl1.0)))

(define-public java-eclipse-equinox-preferences
  (package
    (name "java-eclipse-equinox-preferences")
    (version "3.6.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://repo1.maven.org/maven2/"
                                  "org/eclipse/platform/org.eclipse.equinox.preferences/"
                                  version "/org.eclipse.equinox.preferences-"
                                  version "-sources.jar"))
              (sha256
               (base32
                "0k7w6c141sqym4fy3af0qkwpy4pdh2vsjpjba6rp5fxyqa24v0a2"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f ; no tests included
       #:jar-name "eclipse-equinox-preferences.jar"))
    (inputs
     (list java-eclipse-equinox-common java-eclipse-equinox-registry
           java-eclipse-osgi))
    (home-page "https://www.eclipse.org/equinox/")
    (synopsis "Eclipse preferences mechanism")
    (description "This package provides the Eclipse preferences mechanism with
the module @code{org.eclipse.equinox.preferences}.")
    (license license:epl1.0)))

(define-public java-eclipse-core-contenttype
  (package
    (name "java-eclipse-core-contenttype")
    (version "3.5.100")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://repo1.maven.org/maven2/"
                                  "org/eclipse/platform/org.eclipse.core.contenttype/"
                                  version "/org.eclipse.core.contenttype-"
                                  version "-sources.jar"))
              (sha256
               (base32
                "1wcqcv7ijwv5rh748vz3x9pkmjl9w1r0k0026k56n8yjl4rrmspi"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f ; no tests included
       #:jar-name "eclipse-core-contenttype.jar"))
    (inputs
     (list java-eclipse-equinox-common java-eclipse-equinox-preferences
           java-eclipse-equinox-registry java-eclipse-osgi))
    (home-page "https://www.eclipse.org/")
    (synopsis "Eclipse content mechanism")
    (description "This package provides the Eclipse content mechanism in the
@code{org.eclipse.core.contenttype} module.")
    (license license:epl1.0)))

(define-public java-eclipse-core-runtime
  (package
    (name "java-eclipse-core-runtime")
    (version "3.15.100")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://repo1.maven.org/maven2/"
                                  "org/eclipse/platform/org.eclipse.core.runtime/"
                                  version "/org.eclipse.core.runtime-"
                                  version "-sources.jar"))
              (sha256
               (base32
                "0l8xayacsbjvz5hypx2fv47vpw2n4dspamcfb3hx30x9hj8vmg7r"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f ; no tests included
       #:jar-name "eclipse-core-runtime.jar"))
    (inputs
     (list java-eclipse-core-contenttype
           java-eclipse-core-jobs
           java-eclipse-equinox-app
           java-eclipse-equinox-common
           java-eclipse-equinox-registry
           java-eclipse-equinox-preferences
           java-eclipse-osgi))
    (home-page "https://www.eclipse.org/")
    (synopsis "Eclipse core runtime")
    (description "This package provides the Eclipse core runtime with the
module @code{org.eclipse.core.runtime}.")
    (license license:epl1.0)))

(define-public java-eclipse-core-filesystem
  (package
    (name "java-eclipse-core-filesystem")
    (version "1.6.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://repo1.maven.org/maven2/"
                                  "org/eclipse/platform/org.eclipse.core.filesystem/"
                                  version "/org.eclipse.core.filesystem-"
                                  version "-sources.jar"))
              (sha256
               (base32
                "0km1bhwjim4rfy3pkvjhvy31kgsyf2ncx0mlkmbf5n6g57pphdyj"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f ; no tests included
       #:jar-name "eclipse-core-filesystem.jar"))
    (inputs
     (list java-eclipse-equinox-common java-eclipse-equinox-registry
           java-eclipse-osgi))
    (home-page "https://www.eclipse.org/")
    (synopsis "Eclipse core file system")
    (description "This package provides the Eclipse core file system with the
module @code{org.eclipse.core.filesystem}.")
    (license license:epl1.0)))

(define-public java-eclipse-core-expressions
  (package
    (name "java-eclipse-core-expressions")
    (version "3.5.100")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://repo1.maven.org/maven2/"
                                  "org/eclipse/platform/org.eclipse.core.expressions/"
                                  version "/org.eclipse.core.expressions-"
                                  version "-sources.jar"))
              (sha256
               (base32
                "18bw2l875gmygvpagpgk9l24qzbdjia4ag12nw6fi8v8yaq4987f"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f ; no tests included
       #:jar-name "eclipse-core-expressions.jar"))
    (inputs
     (list java-eclipse-equinox-common java-eclipse-equinox-registry
           java-eclipse-equinox-preferences java-eclipse-core-runtime
           java-eclipse-osgi))
    (home-page "https://www.eclipse.org/")
    (synopsis "Eclipse core expression language")
    (description "This package provides the Eclipse core expression language
with the @code{org.eclipse.core.expressions} module.")
    (license license:epl1.0)))

(define-public java-eclipse-core-variables
  (package
    (name "java-eclipse-core-variables")
    (version "3.3.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://repo1.maven.org/maven2/"
                                  "org/eclipse/platform/org.eclipse.core.variables/"
                                  version "/org.eclipse.core.variables-"
                                  version "-sources.jar"))
              (sha256
               (base32
                "12dirh03zi4n5x5cj07vzrhkmnqy6h9q10h9j605pagmpmifyxmy"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f ; no tests included
       #:jar-name "eclipse-core-variables.jar"))
    (inputs
     (list java-eclipse-equinox-common java-eclipse-equinox-registry
           java-eclipse-equinox-preferences java-eclipse-core-runtime
           java-eclipse-osgi))
    (home-page "https://projects.eclipse.org/projects/eclipse.platform")
    (synopsis "Eclipse core variables")
    (description "This package provides the Eclipse core variables module
@code{org.eclipse.core.variables}.")
    (license license:epl1.0)))

(define-public java-eclipse-ant-core
  (package
    (name "java-eclipse-ant-core")
    (version "3.4.100")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://repo1.maven.org/maven2/"
                                  "org/eclipse/platform/org.eclipse.ant.core/"
                                  version "/org.eclipse.ant.core-"
                                  version "-sources.jar"))
              (sha256
               (base32
                "11g3if794qjlk98mz9zch22rr56sd7z63vn4i7k2icr8cq5bfqg7"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f ; no tests included
       #:jar-name "eclipse-ant-core.jar"))
    (inputs
     (list java-eclipse-equinox-app
           java-eclipse-equinox-common
           java-eclipse-equinox-registry
           java-eclipse-equinox-preferences
           java-eclipse-core-contenttype
           java-eclipse-core-runtime
           java-eclipse-core-variables
           java-eclipse-osgi))
    (home-page "https://projects.eclipse.org/projects/eclipse.platform")
    (synopsis "Ant build tool core libraries")
    (description "This package provides the ant build tool core libraries with
the module @code{org.eclipse.ant.core}.")
    (license license:epl1.0)))

(define-public java-eclipse-core-resources
  (package
    (name "java-eclipse-core-resources")
    (version "3.13.200")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://repo1.maven.org/maven2/"
                                  "org/eclipse/platform/org.eclipse.core.resources/"
                                  version "/org.eclipse.core.resources-"
                                  version "-sources.jar"))
              (sha256
               (base32
                "1sn3b6ky72hkvxcgf9b2jkpbdh3y8lbhi9xxwv1dsiddpkkq91hs"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f ; no tests included
       #:jar-name "eclipse-core-resources.jar"))
    (inputs
     (list java-eclipse-equinox-common
           java-eclipse-equinox-preferences
           java-eclipse-equinox-registry
           java-eclipse-core-contenttype
           java-eclipse-core-expressions
           java-eclipse-core-filesystem
           java-eclipse-core-jobs
           java-eclipse-core-runtime
           java-eclipse-ant-core
           java-eclipse-osgi))
    (home-page "https://www.eclipse.org/")
    (synopsis "Eclipse core resource management")
    (description "This package provides the Eclipse core resource management
module @code{org.eclipse.core.resources}.")
    (license license:epl1.0)))

(define-public java-eclipse-compare-core
  (package
    (name "java-eclipse-compare-core")
    (version "3.6.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://repo1.maven.org/maven2/"
                                  "org/eclipse/platform/org.eclipse.compare.core/"
                                  version "/org.eclipse.compare.core-"
                                  version "-sources.jar"))
              (sha256
               (base32
                "10g37r0pbiffyv2wk35c6g5lwzkdipkl0kkjp41v84dln46xm4dg"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f ; no tests included
       #:jar-name "eclipse-compare-core.jar"))
    (inputs
     (list java-eclipse-core-runtime java-eclipse-equinox-common
           java-eclipse-osgi java-icu4j))
    (home-page "https://www.eclipse.org/")
    (synopsis "Eclipse core compare support")
    (description "This package provides the Eclipse core compare support
module @code{org.eclipse.compare.core}.")
    (license license:epl1.0)))

(define-public java-eclipse-team-core
  (package
    (name "java-eclipse-team-core")
    (version "3.8.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://repo1.maven.org/maven2/"
                                  "org/eclipse/platform/org.eclipse.team.core/"
                                  version "/org.eclipse.team.core-"
                                  version "-sources.jar"))
              (sha256
               (base32
                "02j2jzqgb26zx2d5ahxmvijw6j4r0la90zl5c3i65x6z19ciyam7"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f ; no tests included
       #:jar-name "eclipse-team-core.jar"))
    (inputs
     (list java-eclipse-compare-core
           java-eclipse-core-contenttype
           java-eclipse-core-filesystem
           java-eclipse-core-jobs
           java-eclipse-core-resources
           java-eclipse-core-runtime
           java-eclipse-equinox-common
           java-eclipse-equinox-registry
           java-eclipse-equinox-preferences
           java-eclipse-osgi))
    (home-page "https://projects.eclipse.org/projects/eclipse.platform")
    (synopsis "Eclipse team support core")
    (description "This package provides the Eclipse team support core module
@code{org.eclipse.team.core}.")
    (license license:epl1.0)))

(define-public java-eclipse-core-commands
  (package
    (name "java-eclipse-core-commands")
    (version "3.8.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://repo1.maven.org/maven2/"
                                  "org/eclipse/platform/org.eclipse.core.commands/"
                                  version "/org.eclipse.core.commands-"
                                  version "-sources.jar"))
              (sha256
               (base32
                "0yjn482qndcfrsq3jd6vnhcylp16420f5aqkrwr8spsprjigjcr9"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f ; no tests included
       #:jar-name "eclipse-core-commands.jar"))
    (inputs
     (list java-eclipse-equinox-common))
    (home-page "https://projects.eclipse.org/projects/eclipse.platform")
    (synopsis "Eclipse core commands")
    (description "This package provides Eclipse core commands in the module
@code{org.eclipse.core.commands}.")
    (license license:epl1.0)))

(define-public java-eclipse-text
  (package
    (name "java-eclipse-text")
    (version "3.6.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://repo1.maven.org/maven2/"
                                  "org/eclipse/platform/org.eclipse.text/"
                                  version "/org.eclipse.text-"
                                  version "-sources.jar"))
              (sha256
               (base32
                "0scz70vzz5qs5caji9f5q01vkqnvip7dpri1q07l8wbbdcxn4cq1"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f ; no tests included
       #:jar-name "eclipse-text.jar"
       #:phases
       (modify-phases %standard-phases
         ;; When creating a new category we must make sure that the new list
         ;; matches List<Position>.  By default it seems to be too generic
         ;; (ArrayList<Object>), so we specialize it to ArrayList<Position>.
         ;; Without this we get this error:
         ;;
         ;; [javac] .../src/org/eclipse/jface/text/AbstractDocument.java:376:
         ;;      error: method put in interface Map<K,V> cannot be applied to given types;
         ;; [javac] 			fPositions.put(category, new ArrayList<>());
         ;; [javac] 			          ^
         ;; [javac]   required: String,List<Position>
         ;; [javac]   found: String,ArrayList<Object>
         ;; [javac]   reason: actual argument ArrayList<Object> cannot be converted
         ;;              to List<Position> by method invocation conversion
         ;; [javac]   where K,V are type-variables:
         ;; [javac]     K extends Object declared in interface Map
         ;; [javac]     V extends Object declared in interface Map
         ;;
         ;; I don't know if this is a good fix.  I suspect it is not, but it
         ;; seems to work.
         (add-after 'unpack 'fix-compilation-error
           (lambda _
             (substitute* "src/org/eclipse/jface/text/AbstractDocument.java"
               (("Positions.put\\(category, new ArrayList<>\\(\\)\\);")
                "Positions.put(category, new ArrayList<Position>());"))
             #t)))))
    (inputs
     (list java-eclipse-equinox-common java-eclipse-core-commands
           java-icu4j))
    (home-page "https://projects.eclipse.org/projects/eclipse.platform")
    (synopsis "Eclipse text library")
    (description "Platform Text is part of the Platform UI project and
provides the basic building blocks for text and text editors within Eclipse
and contributes the Eclipse default text editor.")
    (license license:epl1.0)))

(define-public java-eclipse-jdt-core
  (package
    (name "java-eclipse-jdt-core")
    (version "3.16.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://repo1.maven.org/maven2/"
                                  "org/eclipse/jdt/org.eclipse.jdt.core/"
                                  version "/org.eclipse.jdt.core-"
                                  version "-sources.jar"))
              (sha256
               (base32
                "1g560yr9v2kzv34gc2m3ifpgnj7krcdd6h4gd4z83pwqacwkfz0k"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f ; no tests included
       #:jar-name "eclipse-jdt-core.jar"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'move-sources
           (lambda _
             (with-directory-excursion "src/jdtCompilerAdaptersrc/"
               (for-each (lambda (file)
                           (install-file file (string-append "../" (dirname file))))
                         (find-files "." ".*")))
             (delete-file-recursively "src/jdtCompilerAdaptersrc/")
             #t))
         (add-before 'build 'copy-resources
           (lambda _
             (with-directory-excursion "src"
               (for-each (lambda (file)
                           (install-file file (string-append "../build/classes/" (dirname file))))
                         (find-files "." ".*.(props|properties|rsc)")))
             #t)))))
    (inputs
     (list java-eclipse-core-contenttype
           java-eclipse-core-filesystem
           java-eclipse-core-jobs
           java-eclipse-core-resources
           java-eclipse-core-runtime
           java-eclipse-equinox-app
           java-eclipse-equinox-common
           java-eclipse-equinox-preferences
           java-eclipse-equinox-registry
           java-eclipse-osgi
           java-eclipse-text))
    (home-page "https://www.eclipse.org/jdt")
    (synopsis "Java development tools core libraries")
    (description "This package provides the core libraries of the Eclipse Java
development tools.")
    (license license:epl1.0)))

(define-public java-eclipse-jdt-compiler-apt
  (package
    (name "java-eclipse-jdt-compiler-apt")
    (version "1.3.400")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://repo1.maven.org/maven2/"
                                  "org/eclipse/jdt/org.eclipse.jdt.compiler.apt/"
                                  version "/org.eclipse.jdt.compiler.apt-"
                                  version "-sources.jar"))
              (sha256
               (base32
                "1s285k9p2ixdqrknb40jbbvw682n9a7l5lqpn583a8pvlzg2l6r8"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f ; no tests included
       #:jar-name "eclipse-jdt-compiler-apt.jar"
       #:jdk ,openjdk11))
    (inputs
     (list java-eclipse-jdt-core))
    (home-page "https://www.eclipse.org/jdt/apt/")
    (synopsis "Annotation processing tool")
    (description "APT stands for Annotation Processing Tool.  APT provides a
means for generating files and compiling new Java classes based on annotations
found in your source code.")
    (license license:epl2.0)))

(define-public java-eclipse-lsp4j-common
  (package
    (name "java-eclipse-lsp4j-common")
    (version "0.10.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/eclipse/lsp4j")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "17srrac0pkpybwwc21rzdvn762zzl9m80rlqihc9b4l55hkqpk98"))))
    (build-system ant-build-system)
    (home-page "https://eclipse.org/lsp4j/")
    (synopsis "LSP4J common package")
    (description "Eclipse LSP4J provides Java bindings for the Language
Server Protocol and the Debug Adapter Protocol.  This package is a common
definition intended to be inherited by other packages.")
    (license license:epl2.0)))

(define-public java-eclipse-lsp4j-debug
  (package
    (inherit java-eclipse-lsp4j-common)
    (name "java-eclipse-lsp4j-debug")
    (arguments
     `(#:jar-name "eclipse-lsp4j-debug.jar"
       #:jdk ,openjdk11
       #:tests? #f; tests fail with reflection errors
       #:source-dir "org.eclipse.lsp4j.debug/src/main/java"
       #:test-dir "org.eclipse.lsp4j.debug/src/test"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-xtend
           (lambda _
             (copy-recursively "org.eclipse.lsp4j.debug/src/main/xtend-gen"
                               "org.eclipse.lsp4j.debug/src/main/java"))))))
    (native-inputs
     (list java-junit))
    (inputs
     `(("java-gson" ,java-gson-2.8.6)
       ("java-eclipse-lsp4j-generaor" ,java-eclipse-lsp4j-generator)
       ("java-eclipse-lsp4j-jsonrpc" ,java-eclipse-lsp4j-jsonrpc)
       ("java-eclipse-lsp4j-jsonrpc-debug" ,java-eclipse-lsp4j-jsonrpc-debug)
       ("java-eclipse-xtext-xbase-lib" ,java-eclipse-xtext-xbase-lib)))
    (synopsis "Eclipse LSP4J Java bindings for the Debug Server Protocol")
    (description "Eclipse LSP4J provides Java bindings for the Language
Server Protocol and the Debug Adapter Protocol.  This package contains its
LSP4J Java bindings for the Debug Server Protocol.")))

(define-public java-eclipse-lsp4j-generator
  (package
    (inherit java-eclipse-lsp4j-common)
    (name "java-eclipse-lsp4j-generator")
    (arguments
     `(#:jar-name "eclipse-lsp4j-generator.jar"
       #:jdk ,openjdk11
       #:tests? #f; no tests
       #:source-dir "org.eclipse.lsp4j.generator/src/main/java"))
    (inputs
     (list java-eclipse-lsp4j-jsonrpc))
    (synopsis "Eclipse LSP4J Generator")
    (description "Eclipse LSP4J provides Java bindings for the Language
Server Protocol and the Debug Adapter Protocol.  This package contains its
LSP4J code generator for Language Server Protocol classes.")))

(define-public java-eclipse-lsp4j-jsonrpc
  (package
    (inherit java-eclipse-lsp4j-common)
    (name "java-eclipse-lsp4j-jsonrpc")
    (arguments
     `(#:jar-name "eclipse-lsp4j-jsonrpc.jar"
       #:jdk ,openjdk11
       #:source-dir "org.eclipse.lsp4j.jsonrpc/src/main/java"
       #:test-dir "org.eclipse.lsp4j.jsonrpc/src/test"))
    (native-inputs
     (list java-junit))
    (inputs
     (list java-gson-2.8.6))
    (synopsis "Java JSON-RPC implementation")
    (description "Eclipse LSP4J provides Java bindings for the Language
Server Protocol and the Debug Adapter Protocol.  This package contains its
JSON-RPC implementation.")))

(define-public java-eclipse-lsp4j-jsonrpc-debug
  (package
    (inherit java-eclipse-lsp4j-common)
    (name "java-eclipse-lsp4j-jsonrpc-debug")
    (arguments
     `(#:jar-name "eclipse-lsp4j-jsonrpc-debug.jar"
       #:jdk ,openjdk11
       #:source-dir "org.eclipse.lsp4j.jsonrpc.debug/src/main/java"
       #:test-dir "org.eclipse.lsp4j.jsonrpc.debug/src/test"))
    (native-inputs
     (list java-junit))
    (inputs
     (list java-eclipse-lsp4j-jsonrpc java-gson-2.8.6))
    (synopsis "Java JSON-RPC implementation (debug protocol)")
    (description "Eclipse LSP4J provides Java bindings for the Language
Server Protocol and the Debug Adapter Protocol.  This package contains its
JSON-RPC implementation's debug protocol.")))

(define-public java-eclipse-xtext-xbase-lib
  (package
    (name "java-eclipse-xtext-xbase-lib")
    (version "2.25.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/eclipse/xtext-lib")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "13b9lf6lnsprkik665m1qcyyc8cs16k33xm7as4rjcfcpn4pln71"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "eclipse-xtext-xbase-lib.jar"
       #:jdk ,openjdk11
       #:tests? #f; TODO (maybe needs newer guava version?)
       #:source-dir "org.eclipse.xtext.xbase.lib/src"
       #:test-dir "org.eclipse.xtext.xbase.lib.tests/src"))
    (native-inputs
     (list java-junit))
    (inputs
     (list java-guava))
    (home-page "https://www.eclipse.org/Xtext/")
    (synopsis "Eclipse Xbase Runtime Library")
    (description "This package contains runtime libraries for Xbase languages
such as Xtend.")
    (license license:epl2.0)))

(define-public java-javax-mail
  (package
    (name "java-javax-mail")
    (version "1.5.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://repo1.maven.org/maven2/"
                                  "com/sun/mail/javax.mail/"
                                  version "/javax.mail-"
                                  version "-sources.jar"))
              (sha256
               (base32
                "0sdlfgsc2b5s89xv1261y8i0jijcja019k2x1c8ngfn582w4jly9"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f ; no tests
       #:jar-name "javax-mail.jar"))
    (home-page "https://javamail.java.net")
    (synopsis "Reference implementation of the JavaMail API")
    (description
     "This package provides versions of the JavaMail API implementation, IMAP,
SMTP, and POP3 service providers, some examples, and documentation for the
JavaMail API.")
    ;; GPLv2 only with "classpath exception".
    (license license:gpl2)))

(define-public java-log4j-api
  (package
    (name "java-log4j-api")
    (version "2.17.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/logging/log4j/" version
                                  "/apache-log4j-" version "-src.tar.gz"))
              (sha256
               (base32
                "05xssljdgxfv8ql42db8ydjfsvvbdqmsgip75phybm259ydzbsd6"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f ; tests require unpackaged software
       #:jar-name "log4j-api.jar"
       #:make-flags
       ,#~(list (string-append "-Ddist.dir=" #$output "/share/java"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'enter-dir
           (lambda _ (chdir "log4j-api") #t))
         ;; FIXME: The tests require additional software that has not been
         ;; packaged yet, such as
         ;; * org.apache.maven
         ;; * org.apache.felix
         (add-after 'enter-dir 'delete-tests
           (lambda _ (delete-file-recursively "src/test") #t)))))
    (inputs
     (list java-osgi-core java-hamcrest-core java-junit))
    (properties '((cpe-name . "log4j")))
    (home-page "https://logging.apache.org/log4j/2.x/")
    (synopsis "API module of the Log4j logging framework for Java")
    (description
     "This package provides the API module of the Log4j logging framework for
Java.")
    (license license:asl2.0)))

(define-public java-log4j-core
  (package
    (inherit java-log4j-api)
    (name "java-log4j-core")
    (inputs
     `(("java-osgi-core" ,java-osgi-core)
       ("java-hamcrest-core" ,java-hamcrest-core)
       ("java-log4j-api" ,java-log4j-api)
       ("java-mail" ,java-mail)
       ("java-jboss-jms-api-spec" ,java-jboss-jms-api-spec)
       ("java-conversant-disruptor" ,java-conversant-disruptor)
       ("java-lmax-disruptor" ,java-lmax-disruptor)
       ("java-jctools-core" ,java-jctools-core-1)
       ("java-stax2-api" ,java-stax2-api)
       ("java-jansi" ,java-jansi)
       ("java-kafka" ,java-kafka-clients)
       ("java-datanucleus-javax-persistence" ,java-datanucleus-javax-persistence)
       ("java-fasterxml-jackson-annotations" ,java-fasterxml-jackson-annotations)
       ("java-fasterxml-jackson-core" ,java-fasterxml-jackson-core)
       ("java-fasterxml-jackson-databind" ,java-fasterxml-jackson-databind)
       ("java-fasterxml-jackson-dataformat-xml" ,java-fasterxml-jackson-dataformat-xml)
       ("java-fasterxml-jackson-dataformat-yaml" ,java-fasterxml-jackson-dataformat-yaml)
       ("java-commons-compress" ,java-commons-compress)
       ("java-commons-csv" ,java-commons-csv)
       ("java-jeromq" ,java-jeromq)
       ("java-junit" ,java-junit)))
    (native-inputs
     `(("hamcrest" ,java-hamcrest-all)
       ("java-commons-io" ,java-commons-io)
       ("java-commons-lang3" ,java-commons-lang3)
       ("slf4j" ,java-slf4j-api)))
    (arguments
     `(#:tests? #f ; tests require more dependencies
       #:test-dir "src/test"
       #:source-dir "src/main/java"
       #:jar-name "log4j-core.jar"
       #:jdk ,icedtea-8
       #:make-flags
       ,#~(list (string-append "-Ddist.dir=" #$output "/share/java"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'enter-dir
           (lambda _ (chdir "log4j-core") #t)))))
    (synopsis "Core component of the Log4j framework")
    (description "This package provides the core component of the Log4j
logging framework for Java.")))

(define-public java-log4j-1.2-api
  (package
    (inherit java-log4j-api)
    (name "java-log4j-1.2-api")
    (arguments
     `(#:jar-name "java-log4j-1.2-api.jar"
       #:source-dir "log4j-1.2-api/src/main/java"
       #:jdk ,icedtea-8
       ;; Tests require maven-model (and other maven subprojects), which is a
       ;; cyclic dependency.
       #:tests? #f))
    (inputs
     `(("log4j-api" ,java-log4j-api)
       ("log4j-core" ,java-log4j-core)
       ("java-jboss-jms-api-spec" ,java-jboss-jms-api-spec)
       ("osgi-core" ,java-osgi-core)
       ("eclipse-osgi" ,java-eclipse-osgi)
       ("java-lmax-disruptor" ,java-lmax-disruptor)))))

(define-public java-commons-cli
  (package
    (name "java-commons-cli")
    (version "1.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/commons/cli/source/"
                                  "commons-cli-" version "-src.tar.gz"))
              (sha256
               (base32
                "05hgi2z01fqz374y719gl1dxzqvzci5af071zm7vxrjg9vczipm1"))))
    (build-system ant-build-system)
    ;; TODO: javadoc
    (arguments
     `(#:jar-name "commons-cli.jar"
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (install-from-pom "pom.xml")))))
    (native-inputs
     (list java-junit java-hamcrest-core))
    (home-page "https://commons.apache.org/cli/")
    (synopsis "Command line arguments and options parsing library")
    (description "The Apache Commons CLI library provides an API for parsing
command line options passed to programs.  It is also able to print help
messages detailing the options available for a command line tool.

Commons CLI supports different types of options:

@itemize
@item POSIX like options (ie. tar -zxvf foo.tar.gz)
@item GNU like long options (ie. du --human-readable --max-depth=1)
@item Java like properties (ie. java -Djava.awt.headless=true Foo)
@item Short options with value attached (ie. gcc -O2 foo.c)
@item long options with single hyphen (ie. ant -projecthelp)
@end itemize

This is a part of the Apache Commons Project.")
    (license license:asl2.0)))

(define-public java-commons-text
  (package
    (name "java-commons-text")
    (version "1.9")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/commons/text/source/"
                                  "commons-text-" version "-src.tar.gz"))
              (sha256
               (base32
                "1k99ib2dxlqxb5y94kpzf4ix8xgxz1j3n9kq3ddssqqcccsp5ik2"))))
    (build-system ant-build-system)
    (arguments
     (list #:jar-name "java-commons-text.jar"
           #:source-dir "src/main/java"
           #:test-dir "src/test"
           #:tests? #f                  ; Tests require JUnit5.
           #:phases
           #~(modify-phases %standard-phases
               (replace 'install
                 (install-from-pom "pom.xml")))))
    (inputs
     (list java-commons-io))
    (propagated-inputs
     (list java-commons-lang3
           apache-commons-parent-pom-51))
    (home-page "https://commons.apache.org/text/")
    (synopsis "Library focused on algorithms working on strings")
    (description "Apache Commons Text is a library focused on algorithms
working on strings.

This is a part of the Apache Commons Project.")
    (license license:asl2.0)))

(define-public java-commons-codec
  (package
    (name "java-commons-codec")
    (version "1.15")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/commons/codec/source/"
                                  "commons-codec-" version "-src.tar.gz"))
              (sha256
               (base32
                "01z9qmg8fd8d7p7xxipwj1vi9bmvpgqyi29kldjz2x6vzwm171jj"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "java-commons-codec.jar"
       #:source-dir "src/main/java"
       #:test-dir "src/test"
       #:test-exclude (list "**/*AbstractTest.java")
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "src/main/resources"
                               "build/classes")))
         (add-before 'check 'copy-test-resources
           (lambda _
             (copy-recursively "src/test/resources"
                               "build/test-classes")))
         (add-before 'check 'skip-ravenous-test
           (lambda _
             ;; This test admits to being "memory hungry", but reliably fails
             ;; even on a machine that should have plenty (12 GiB).  Skip it.
             (substitute*
                 "src/test/java/org/apache/commons/codec/binary/BaseNCodecTest.java"
               (("\\bassertEnsureBufferSizeExpandsToMaxBufferSize.*;")
                "return;"))))
         (replace 'install (install-from-pom "pom.xml")))))
    (native-inputs
     (list java-commons-lang3 java-junit))
    (propagated-inputs
      (list apache-commons-parent-pom-52))
    (home-page "https://commons.apache.org/codec/")
    (synopsis "Common encoders and decoders such as Base64, Hex, Phonetic and URLs")
    (description "The codec package contains simple encoder and decoders for
various formats such as Base64 and Hexadecimal.  In addition to these widely
used encoders and decoders, the codec package also maintains a collection of
phonetic encoding utilities.

This is a part of the Apache Commons Project.")
    (license license:asl2.0)))

(define-public java-commons-daemon
  (package
    (name "java-commons-daemon")
    (version "1.1.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/commons/daemon/source/"
                                  "commons-daemon-" version "-src.tar.gz"))
              (sha256
               (base32
                "141gkhfzv5v3pdhic6y4ardq2dhsa3v36j8wmmhy6f8mac48fp7n"))))
    (build-system ant-build-system)
    (arguments
     `(#:test-target "test"
       #:phases
       (modify-phases %standard-phases
         (add-after 'build 'build-javadoc ant-build-javadoc)
         (replace 'install (install-jars "dist"))
         (add-after 'install 'install-doc (install-javadoc "dist/docs/api")))))
    (native-inputs
     (list java-junit))
    (home-page "https://commons.apache.org/daemon/")
    (synopsis "Library to launch Java applications as daemons")
    (description "The Daemon package from Apache Commons can be used to
implement Java applications which can be launched as daemons.  For example the
program will be notified about a shutdown so that it can perform cleanup tasks
before its process of execution is destroyed by the operation system.

This package contains the Java library.  You will also need the actual binary
for your architecture which is provided by the jsvc package.

This is a part of the Apache Commons Project.")
    (license license:asl2.0)))

(define-public java-javaewah
  (package
    (name "java-javaewah")
    (version "1.1.6")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/lemire/javaewah/")
                     (commit (string-append "JavaEWAH-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1m8qcb1py76v7avbjjrkvyy6fhr5dk2ywy73gbsxqry04gkm2nhw"))))
    (build-system ant-build-system)
    (arguments `(#:jar-name "javaewah.jar"))
    (inputs
     (list java-junit java-hamcrest-core))
    (home-page "https://github.com/lemire/javaewah")
    (synopsis "Compressed alternative to the Java @code{BitSet} class")
    (description "This is a word-aligned compressed variant of the Java
@code{Bitset} class.  It provides both a 64-bit and a 32-bit RLE-like
compression scheme.  It can be used to implement bitmap indexes.

The goal of word-aligned compression is not to achieve the best compression,
but rather to improve query processing time.  Hence, JavaEWAH tries to save CPU
cycles, maybe at the expense of storage.  However, the EWAH scheme is always
more efficient storage-wise than an uncompressed bitmap (as implemented in the
@code{BitSet} class by Sun).")
    ;; GPL2.0 derivates are explicitly allowed.
    (license license:asl2.0)))

(define-public java-slf4j-api
  (package
    (name "java-slf4j-api")
    (version "1.7.25")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/qos-ch/slf4j")
                    (commit (string-append "v_" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "15n42zq3k1iyn752nwdcbs44hxns2rmxhglwjfr4np7lxx56apjl"))
              (modules '((guix build utils)))
              ;; Delete bundled jars.
              (snippet
               '(begin
                  (for-each delete-file (find-files "." "\\.jar$"))
                  #t))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "slf4j-api.jar"
       #:source-dir "slf4j-api/src/main"
       #:test-dir "slf4j-api/src/test"
       #:phases
       (modify-phases %standard-phases
         (add-after 'build 'regenerate-jar
           (lambda _
             ;; pom.xml ignores these files in the jar creation process. If we don't,
             ;; we get the error "This code should have never made it into slf4j-api.jar"
             (delete-file-recursively "build/classes/org/slf4j/impl")
             (invoke "jar" "-cf" "build/jar/slf4j-api.jar" "-C"
                     "build/classes" ".")))
         (add-before 'check 'dont-test-abstract-classes
           (lambda _
             ;; abstract classes are not meant to be run with junit
             (substitute* "build.xml"
               (("<include name=\"\\*\\*/\\*Test.java\" />")
                (string-append "<include name=\"**/*Test.java\" />"
                               "<exclude name=\"**/MultithreadedInitializationTest"
                               ".java\" />")))
             #t))
         (replace 'install
           (install-from-pom "slf4j-api/pom.xml")))))
    (propagated-inputs
     (list java-slf4j-parent))
    (native-inputs
     (list java-junit java-hamcrest-core))
    (home-page "https://www.slf4j.org/")
    (synopsis "Simple logging facade for Java")
    (description "The Simple Logging Facade for Java (SLF4J) serves as a
simple facade or abstraction for various logging
frameworks (e.g. @code{java.util.logging}, @code{logback}, @code{log4j})
allowing the end user to plug in the desired logging framework at deployment
time.")
    (license license:expat)))

(define java-slf4j-parent
  (package
    (inherit java-slf4j-api)
    (name "java-slf4j-parent")
    (native-inputs `())
    (propagated-inputs '())
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'build)
         (delete 'configure)
         (replace 'install
           (install-pom-file "pom.xml")))))))

(define-public java-slf4j-simple
  (package
    (name "java-slf4j-simple")
    (version "1.7.25")
    (source (package-source java-slf4j-api))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "slf4j-simple.jar"
       #:source-dir "slf4j-simple/src/main"
       #:test-dir "slf4j-simple/src/test"
       #:test-exclude (list "**/*SimpleLoggerMultithreadedInitializationTest.java")
       #:phases
       (modify-phases %standard-phases
         ;; The tests need some test classes from slf4j-api
         (add-before 'check 'build-slf4j-api-test-helpers
           (lambda _
             ;; Add current dir to CLASSPATH ...
             (setenv "CLASSPATH"
                     (string-append (getcwd) ":" (getenv "CLASSPATH")))
             ;; ... and build test helper classes here:
             (apply invoke
                    `("javac" "-d" "."
                      ,@(find-files "slf4j-api/src/test" ".*\\.java")))))
         (replace 'install
           (install-from-pom "slf4j-simple/pom.xml")))))
    (propagated-inputs
     (list java-slf4j-api))
    (native-inputs
     (list java-junit java-hamcrest-core))
    (home-page "https://www.slf4j.org/")
    (synopsis "Simple implementation of simple logging facade for Java")
    (description "SLF4J binding for the Simple implementation, which outputs
all events to System.err.  Only messages of level INFO and higher are
printed.")
    (license license:expat)))

(define-public java-slf4j-nop
  (package
    (name "java-slf4j-nop")
    (version "1.7.25")
    (source (package-source java-slf4j-api))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "slf4j-nop.jar"
       #:source-dir "slf4j-nop/src/main"
       #:test-dir "slf4j-nop/src/test"
       #:phases (modify-phases %standard-phases
                  ;; The tests need some test classes from slf4j-api
                  (add-before 'check 'build-slf4j-api-test-helpers
                    (lambda _
                      ;; Add current dir to CLASSPATH ...
                      (setenv "CLASSPATH"
                              (string-append (getcwd) ":"
                                             (getenv "CLASSPATH")))
                      ;; ... and build test helper classes here:
                      (apply invoke
                             `("javac" "-d" "."
                               ,@(find-files "slf4j-api/src/test" ".*\\.java")))))
                  (replace 'install
                    (install-from-pom "slf4j-nop/pom.xml")))))
    (propagated-inputs (list java-slf4j-api))
    (native-inputs (list java-junit java-hamcrest-core))
    (home-page "https://www.slf4j.org/")
    (synopsis "SLF4J binding that silently discards all logging messages")
    (description "Binding/provider for NOP, an implementation that silently
discards all logging messages.")
    (license license:expat)))

(define-public antlr2
  (package
    (name "antlr2")
    (version "2.7.7")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.antlr2.org/download/antlr-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1ffvcwdw73id0dk6pj2mlxjvbg0662qacx4ylayqcxgg381fnfl5"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (delete-file "antlr.jar")
                  (substitute* "lib/cpp/antlr/CharScanner.hpp"
                    (("#include <map>")
                     (string-append
                      "#include <map>\n"
                      "#define EOF (-1)\n"
                      "#include <strings.h>")))
                  (substitute* "configure"
                    (("/bin/sh") "sh"))
                  #t))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no test target
       #:imported-modules ((guix build ant-build-system)
                           ,@%default-gnu-imported-modules)
       #:modules (((guix build ant-build-system) #:prefix ant:)
                  (guix build gnu-build-system)
                  (guix build utils))
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'strip-jar-timestamps
           (assoc-ref ant:%standard-phases 'strip-jar-timestamps))
         (add-before 'configure 'fix-timestamp
           (lambda _
             (substitute* "configure"
               (("^TIMESTAMP.*") "TIMESTAMP=19700101\n"))
             #t))
         (add-after 'configure 'fix-bin-ls
           (lambda _
             (substitute* (find-files "." "Makefile")
               (("/bin/ls") "ls"))
             #t)))))
    (native-inputs
     `(("which" ,which)
       ("zip" ,zip)
       ("java" ,icedtea "jdk")))
    (inputs
     `(("java" ,icedtea)))
    (home-page "https://www.antlr2.org")
    (synopsis "Framework for constructing recognizers, compilers, and translators")
    (description "ANTLR, ANother Tool for Language Recognition, (formerly PCCTS)
is a language tool that provides a framework for constructing recognizers,
compilers, and translators from grammatical descriptions containing Java, C#,
C++, or Python actions.  ANTLR provides excellent support for tree construction,
tree walking, and translation.")
    (license license:public-domain)))

(define-public java-stringtemplate-3
  (package
    (name "java-stringtemplate")
    (version "3.2.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/antlr/website-st4/raw/"
                                  "gh-pages/download/stringtemplate-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "086yj68np1vqhkj7483diz3km6s6y4gmwqswa7524a0ca6vxn2is"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name (string-append ,name "-" ,version ".jar")
       #:test-dir "test"
       #:modules ((guix build ant-build-system)
                  (guix build utils)
                  (srfi srfi-1))
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'fix-tests
           (lambda _
             (substitute* "build.xml"
               (("\\$\\{test.home\\}/java")
                "${test.home}/org"))
             #t))
         (add-before 'build 'generate-grammar
           (lambda _
             (with-directory-excursion "src/org/antlr/stringtemplate/language/"
               (for-each (lambda (file)
                           (format #t "~a\n" file)
                           (invoke "antlr" file))
                         '("template.g" "angle.bracket.template.g" "action.g"
                           "eval.g" "group.g" "interface.g")))
             #t)))))
    (native-inputs
     `(("antlr" ,antlr2)
       ("java-junit" ,java-junit)))
    (home-page "https://www.stringtemplate.org")
    (synopsis "Template engine to generate formatted text output")
    (description "StringTemplate is a java template engine (with ports for C#,
Objective-C, JavaScript, Scala) for generating source code, web pages, emails,
or any other formatted text output.  StringTemplate is particularly good at
code generators, multiple site skins, and internationalization / localization.
StringTemplate also powers ANTLR.")
    (license license:bsd-3)))

;; antlr3 is partially written using antlr3 grammar files. It also depends on
;; ST4 (stringtemplate4), which is also partially written using antlr3 grammar
;; files and uses antlr3 at runtime. The latest version requires a recent version
;; of antlr3 at runtime.
;; Fortunately, ST4 4.0.6 can be built with an older antlr3, and we use antlr3.3.
;; This version of ST4 is sufficient for the latest antlr3.
;; We use ST4 4.0.6 to build a boostrap antlr3 (latest version), and build
;; the latest ST4 with it. Then we build our final antlr3 that will be linked
;; against the latest ST4.
;; antlr3.3 still depends on antlr3 to generate some files, so we use an
;; even older version, antlr3.1, to generate them. Fortunately antlr3.1 uses
;; only grammar files with the antlr2 syntax.
;; So we build antlr3.1 -> antlr3.3 -> ST4.0.6 -> antlr3-bootstrap -> ST4 -> antlr3.

(define-public java-stringtemplate
  (package (inherit java-stringtemplate-3)
    (name "java-stringtemplate")
    (version "4.0.8")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/antlr/stringtemplate4/archive/"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1pri8hqa95rfdkjy55icl5q1m09zwp5k67ib14abas39s4v3w087"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name (string-append ,name "-" ,version ".jar")
       #:tests? #f ; FIXME: tests fail for unknown reasons
       #:test-dir "test"
       #:modules ((guix build ant-build-system)
                  (guix build utils)
                  (srfi srfi-1))
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'fix-test-target
           (lambda _
             (substitute* "build.xml"
               (("\\$\\{test.home\\}/java") "${test.home}/")
               (("\\*Test.java") "Test*.java"))
             #t))
         (add-before 'build 'generate-grammar
           (lambda _
             (with-directory-excursion "src/org/stringtemplate/v4/compiler/"
               (for-each (lambda (file)
                           (format #t "~a\n" file)
                           (invoke "antlr3" file))
                         '("STParser.g" "Group.g" "CodeGenerator.g")))
             #t)))))
    (inputs
     `(("antlr3" ,antlr3-bootstrap)
       ("antlr2" ,antlr2)
       ("java-stringtemplate" ,java-stringtemplate-3)
       ("java-junit" ,java-junit)))))

(define java-stringtemplate-4.0.6
  (package (inherit java-stringtemplate)
    (name "java-stringtemplate")
    (version "4.0.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/antlr/stringtemplate4/archive/ST-"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0hjmh1ahdsh3w825i67mli9l4nncc4l6hdbf9ma91jvlj590sljp"))))
    (inputs
     `(("antlr3" ,antlr3-3.3)
       ("antlr2" ,antlr2)
       ("java-stringtemplate" ,java-stringtemplate-3)))))

(define-public antlr3
  (package
    (name "antlr3")
    (version "3.5.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/antlr/antlr3")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0cafavrjmzqhklghrk8c2jqxkdwxgzskm20pbrfd3r41cn00dpnf"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name (string-append ,name "-" ,version ".jar")
       #:source-dir "tool/src/main/java:runtime/Java/src/main/java:tool/src/main/antlr3"
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'bin-install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((jar (string-append (assoc-ref outputs "out") "/share/java"))
                   (bin (string-append (assoc-ref outputs "out") "/bin")))
               (mkdir-p bin)
               (with-output-to-file (string-append bin "/antlr3")
                 (lambda _
                   (display
                     (string-append "#!" (which "sh") "\n"
                                    "java -cp " jar "/" ,name "-" ,version ".jar:"
                                    (string-concatenate
                                      (find-files (assoc-ref inputs "stringtemplate")
                                                  ".*\\.jar"))
                                    ":"
                                    (string-concatenate
                                      (find-files (assoc-ref inputs "stringtemplate4")
                                                  ".*\\.jar"))
                                    ":"
                                    (string-concatenate
                                      (find-files (string-append
                                                    (assoc-ref inputs "antlr")
                                                    "/lib")
                                                  ".*\\.jar"))
                                    " org.antlr.Tool $*"))))
               (chmod (string-append bin "/antlr3") #o755))
             #t))
         (add-before 'build 'generate-grammar
           (lambda _
             (chdir "tool/src/main/antlr3/org/antlr/grammar/v3/")
             (for-each (lambda (file)
                         (display file)
                         (newline)
                         (invoke "antlr3" file))
                       '("ANTLR.g" "ANTLRTreePrinter.g" "ActionAnalysis.g"
                         "AssignTokenTypesWalker.g"
                         "ActionTranslator.g" "TreeToNFAConverter.g"
                         "ANTLRv3.g" "ANTLRv3Tree.g" "LeftRecursiveRuleWalker.g"
                         "CodeGenTreeWalker.g" "DefineGrammarItemsWalker.g"))
             (substitute* "ANTLRParser.java"
               (("public Object getTree") "public GrammarAST getTree"))
             (substitute* "ANTLRv3Parser.java"
               (("public Object getTree") "public CommonTree getTree"))
             (chdir "../../../../../java")
             (substitute* "org/antlr/tool/LeftRecursiveRuleAnalyzer.java"
               (("import org.antlr.grammar.v3.\\*;") "import org.antlr.grammar.v3.*;
import org.antlr.grammar.v3.ANTLRTreePrinter;"))
             (substitute* "org/antlr/tool/ErrorManager.java"
               (("case NO_SUCH_ATTRIBUTE_PASS_THROUGH:") ""))
             (chdir "../../../..")
             #t))
         (add-before 'build 'fix-build-xml
           (lambda _
             (substitute* "build.xml"
               (("target name=\"compile\">")
                "target name=\"compile\">
<copy todir=\"${classes.dir}\">
<fileset dir=\"tool/src/main/resources\">
<include name=\"**/*.stg\"/>
<include name=\"**/*.st\"/>
<include name=\"**/*.sti\"/>
<include name=\"**/STLexer.tokens\"/>
</fileset>
</copy>"))
             #t)))))
    (native-inputs
     `(("antlr" ,antlr2)
       ("antlr3" ,antlr3-bootstrap)))
    (inputs
     `(("junit" ,java-junit)
       ("stringtemplate" ,java-stringtemplate-3)
       ("stringtemplate4" ,java-stringtemplate)))
    (propagated-inputs
     `(("stringtemplate" ,java-stringtemplate-3)
       ("antlr" ,antlr2)
       ("stringtemplate4" ,java-stringtemplate-4.0.6)))
    (home-page "https://www.antlr3.org")
    (synopsis "Framework for constructing recognizers, compilers, and translators")
    (description "ANTLR, ANother Tool for Language Recognition, (formerly PCCTS)
is a language tool that provides a framework for constructing recognizers,
compilers, and translators from grammatical descriptions containing Java, C#,
C++, or Python actions.  ANTLR provides excellent support for tree construction,
tree walking, and translation.")
    (license license:bsd-3)))

(define antlr3-bootstrap
  (package
    (inherit antlr3)
    (name "antlr3-bootstrap")
    (native-inputs
     `(("antlr" ,antlr2)
       ("antlr3" ,antlr3-3.3)))
    (inputs
     `(("junit" ,java-junit)))))

(define-public antlr3-3.3
  (package
    (inherit antlr3)
    (name "antlr3")
    (version "3.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/antlr/website-antlr3/raw/"
                                  "gh-pages/download/antlr-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0qgg5vgsm4l1d6dj9pfbaa25dpv2ry2gny8ajy4vvgvfklw97b3m"))
              (patches
               (search-patches "antlr3-3_3-fix-java8-compilation.patch"))))
    (arguments
     `(#:jar-name (string-append ,name "-" ,version ".jar")
       #:source-dir (string-join '("tool/src/main/java"
                                   "runtime/Java/src/main/java"
                                   "tool/src/main/antlr2"
                                   "tool/src/main/antlr3")
                                 ":")
       #:tests? #f  ; FIXME: tests seem to require maven plugin
       #:modules ((guix build ant-build-system)
                  (guix build utils)
                  (srfi srfi-1))
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'bin-install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (jar (string-append out "/share/java"))
                    (bin (string-append out "/bin")))
               (mkdir-p bin)
               (with-output-to-file (string-append bin "/antlr3")
                 (lambda _
                   (display
                    (string-append
                     "#!" (which "sh") "\n"
                     "java -cp " jar "/antlr3-3.3.jar:"
                     (string-join
                      (append (find-files (assoc-ref inputs "java-stringtemplate")
                                          ".*\\.jar$")
                              (find-files (string-append (assoc-ref inputs "antlr")
                                                         "/lib")
                                          ".*\\.jar$"))
                      ":")
                     " org.antlr.Tool $*"))))
               (chmod (string-append bin "/antlr3") #o755)
               #t)))
         (add-before 'build 'generate-grammar
           (lambda _
             (substitute* "tool/src/main/java/org/antlr/tool/Grammar.java"
               (("import org.antlr.grammar.v2.\\*;")
                "import org.antlr.grammar.v2.*;\n
import org.antlr.grammar.v2.TreeToNFAConverter;\n
import org.antlr.grammar.v2.DefineGrammarItemsWalker;\n
import org.antlr.grammar.v2.ANTLRTreePrinter;"))
             (with-directory-excursion "tool/src/main/antlr2/org/antlr/grammar/v2/"
               (for-each (lambda (file)
                           (format #t "~a\n" file)
                           (invoke "antlr" file))
                         '("antlr.g" "antlr.print.g" "assign.types.g"
                           "buildnfa.g" "codegen.g" "define.g")))
             (with-directory-excursion "tool/src/main/antlr3/org/antlr/grammar/v3/"
               (for-each (lambda (file)
                           (format #t "~a\n" file)
                           (invoke "antlr3" file))
                         '("ActionAnalysis.g" "ActionTranslator.g" "ANTLRv3.g"
                           "ANTLRv3Tree.g")))
             #t))
         (add-before 'build 'fix-build-xml
           (lambda _
             (substitute* "build.xml"
               (("target name=\"compile\">")
                "target name=\"compile\">
<copy todir=\"${classes.dir}\">
<fileset dir=\"tool/src/main/resources\">
<include name=\"**/*.stg\"/>
<include name=\"**/*.st\"/>
<include name=\"**/*.sti\"/>
<include name=\"**/STLexer.tokens\"/>
</fileset>
</copy>"))
             #t)))))
    (native-inputs
     `(("antlr" ,antlr2)
       ("antlr3" ,antlr3-3.1)))
    (inputs
     `(("junit" ,java-junit)))
    (propagated-inputs
     `(("java-stringtemplate" ,java-stringtemplate-3)
       ("antlr" ,antlr2)))))

(define-public antlr3-3.1
  (package
    (inherit antlr3)
    (version "3.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/antlr/website-antlr3/raw/"
                                  "gh-pages/download/antlr-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0sfimc9cpbgrihz4giyygc8afgpma2c93yqpwb951giriri6x66z"))
              (patches
               (search-patches "antlr3-3_1-fix-java8-compilation.patch"))))
    (arguments
     `(#:jar-name (string-append "antlr3-" ,version ".jar")
       #:source-dir "src:runtime/Java/src"
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'bin-install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((jar (string-append (assoc-ref outputs "out") "/share/java"))
                   (bin (string-append (assoc-ref outputs "out") "/bin")))
               (mkdir-p bin)
               (with-output-to-file (string-append bin "/antlr3")
                 (lambda _
                   (display
                     (string-append "#!" (which "sh") "\n"
                                    "java -cp " jar "/antlr3-3.1.jar:"
                                    (string-concatenate
                                      (find-files (assoc-ref inputs "stringtemplate")
                                                  ".*\\.jar"))
                                    ":"
                                    (string-concatenate
                                      (find-files (string-append
                                                    (assoc-ref inputs "antlr")
                                                    "/lib")
                                                  ".*\\.jar"))
                                    " org.antlr.Tool $*"))))
               (chmod (string-append bin "/antlr3") #o755))
             #t))
         (add-before 'build 'generate-grammar
           (lambda _
             (let ((dir "src/org/antlr/tool/"))
               (for-each (lambda (file)
                           (display file)
                           (newline)
                           (invoke "antlr" "-o" dir (string-append dir file)))
                         '("antlr.g" "antlr.print.g" "assign.types.g"
                           "buildnfa.g" "define.g")))
             (format #t "codegen.g\n")
             (invoke "antlr" "-o" "src/org/antlr/codegen"
                     "src/org/antlr/codegen/codegen.g")
             #t))
         (add-before 'build 'fix-build-xml
           (lambda _
             (substitute* "build.xml"
               (("target name=\"compile\">")
                "target name=\"compile\">
<copy todir=\"${classes.dir}\">
<fileset dir=\"src\">
<include name=\"**/*.stg\"/>
<include name=\"**/*.st\"/>
<include name=\"**/*.sti\"/>
<include name=\"**/STLexer.tokens\"/>
</fileset>
</copy>"))
             #t)))))
    (native-inputs
     `(("antlr" ,antlr2)))
    (inputs
     `(("junit" ,java-junit)))
    (propagated-inputs
     `(("stringtemplate" ,java-stringtemplate-3)))))

(define-public java-treelayout
  (package
    (name "java-treelayout")
    (version "1.0.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/abego/treelayout")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "18my8ql9b1y0n0zrvkih7xfhf3dpgfhyfifvkcfhmwcvw3divxak"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name (string-append ,name "-" ,version ".jar")
       #:source-dir "org.abego.treelayout/src/main/java"
       #:test-dir "org.abego.treelayout/src/test"))
    (inputs
     (list java-junit))
    (native-inputs
     (list java-hamcrest-core))
    (home-page "https://treelayout.sourceforge.net")
    (synopsis "Tree Layout Algorithm in Java")
    (description "TreeLayout creates tree layouts for arbitrary trees.  It is
not restricted to a specific output or format, but can be used for any kind of
two dimensional diagram.  Examples are Swing based components, SVG files, etc.
This is possible because TreeLayout separates the layout of a tree from the
actual rendering.")
    (license license:bsd-3)))

(define-public java-antlr4-runtime
  (package
    (name "java-antlr4-runtime")
    (version "4.10.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/antlr/antlr4")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0ldvd9jm4nrivaw7i7wh1q40q6xgzmzirsywiakbhg8sppagqlv7"))
              (patches
                (search-patches "java-antlr4-Add-standalone-generator.patch"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "java-antlr4-runtime.jar"
       #:source-dir "runtime/Java/src/org"
       #:tests? #f; tests depend on java-antlr4 itself
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "runtime/Java/src/main/dot"
                               "build/classes")
             #t)))))
    (home-page "https://www.antlr.org")
    (synopsis "ANTLR runtime library")
    (description "This package contains the runtime library used with generated
sources by ANTLR.")
    (license license:bsd-3)))

(define-public java-antlr4-runtime-cpp
  (package
    (inherit java-antlr4-runtime)
    (name "java-antlr4-runtime-cpp")
    (outputs '("out" "static"))
    (build-system cmake-build-system)
    (arguments
     (list
      ;; TODO: try to run the tests under
      ;; runtime-testsuite/test/org/antlr/v4/test/runtime/cpp with antlr4.
      #:tests? #f                       ;no CMake test target
      ;; TODO: Building the tests wants to download googletest.
      #:configure-flags #~'("-DANTLR_BUILD_CPP_TESTS=OFF")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'chdir
            (lambda _
              (chdir "runtime/Cpp")))
          (add-after 'install 'move-static-library
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((static (assoc-ref outputs "static"))
                    (libantlr4-runtime.a (search-input-file
                                          outputs "lib/libantlr4-runtime.a")))
                (mkdir-p (string-append static "/lib"))
                (rename-file
                 libantlr4-runtime.a
                 (string-append static "/lib/"
                                (basename libantlr4-runtime.a)))))))))
    (native-inputs (list pkg-config))
    (inputs (list `(,util-linux "lib"))) ;libuuid
    (synopsis "ANTL C++ runtime library")
    (description "This package contains the C++ runtime library used with C++
generated sources by ANTLR.")))

(define-public java-antlr4-runtime-python
  (package
    (inherit java-antlr4-runtime)
    (name "java-antlr4-runtime-python")
    (outputs '("out"))
    (build-system python-build-system)
    (arguments
     (list
      #:tests? #f                       ; tests require antlr
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'chdir
            (lambda _
              (chdir "runtime/Python3"))))))
    (native-inputs (list pkg-config))
    (inputs (list `(,util-linux "lib"))) ; libuuid
    (synopsis "ANTLR Python runtime library")
    (description "This package contains the Python runtime library used with
Python generated sources by ANTLR.")))

(define-public antlr4
  (package
    (inherit java-antlr4-runtime)
    (name "antlr4")
    (arguments
     `(#:jar-name "antlr4.jar"
       #:source-dir "tool/src"
       #:test-dir "tool-testsuite/test:runtime-testsuite/test"
       #:test-include (list "**/Test*.java")
       #:test-exclude (list
                       ;; These have no runnable methods and fail because
                       ;; test-include above is too broad.
                        "**/TestContext.java"
                        "**/TestOutputReading.java"
                        ;; no @Test methods
                        "**/TestParserErrors.java"
                        "**/TestSemPredEvalParser.java"
                        "**/TestSets.java"
                        "**/TestListeners.java"
                        "**/TestParseTrees.java"
                        "**/TestParserExec.java"
                        "**/TestLexerErrors.java"
                        "**/TestPerformance.java"
                        "**/TestCompositeParsers.java"
                        "**/TestLexerExec.java"
                        "**/TestSemPredEvalLexer.java"
                        "**/TestLeftRecursion.java"
                        "**/TestFullContextParsing.java"
                        "**/TestCompositeLexers.java"
                        ;; Null pointer exception
                        "**/TestCompositeGrammars.java")
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'fix-build.xml
           (lambda _
             ;; tests are not in a java subdirectory
             (substitute* "build.xml"
               (("\\$\\{test.home\\}/java") "${test.home}"))
             #t))
         ;; tests require to have a working antlr4 binary
         (delete 'check)
         (add-after 'install 'check
           (lambda _
             (invoke "ant" "compile-tests")
             (invoke "ant" "check" "-Dtest.home=runtime-testsuite/test")
             (invoke "ant" "check" "-Dtest.home=tool-testsuite/test")
             #t))
         (add-before 'check 'remove-unrelated-languages
           (lambda _
             ;; There are tests for other languages that ANTLR can generate, but
             ;; we don't have the infrastructure for that yet.  Let's test Java
             ;; generation only.
             (for-each
               (lambda (language)
                 (delete-file-recursively
                   (string-append "runtime-testsuite/test/org/antlr/v4/test/runtime/"
                                  language)))
               '("cpp" "csharp" "go" "javascript" "php" "python" "python2"
                 "python3" "swift"))
             #t))
         (add-before 'check 'generate-test-parsers
           (lambda* (#:key outputs #:allow-other-keys)
             (define (run-antlr dir filename package)
               (invoke "antlr4" "-lib" dir "-visitor" "-no-listener"
                       "-package" package (string-append dir "/" filename)
                       "-Xlog"))
             (setenv "PATH" (string-append (getenv "PATH") ":"
                                           (assoc-ref outputs "out") "/bin"))
             (run-antlr "runtime-testsuite/test/org/antlr/v4/test/runtime/java/api"
                        "Java.g4" "org.antlr.v4.test.runtime.java.api")
             (run-antlr "runtime-testsuite/test/org/antlr/v4/test/runtime/java/api"
                        "VisitorBasic.g4" "org.antlr.v4.test.runtime.java.api")
             (run-antlr "runtime-testsuite/test/org/antlr/v4/test/runtime/java/api"
                        "VisitorCalc.g4" "org.antlr.v4.test.runtime.java.api")
             #t))
         (add-before 'check 'remove-graphemes
           (lambda _
             ;; When running antlr on grahemes.g4, we get a runtime exception:
             ;; set is empty.  So delete the file that depends on it.
             (delete-file
               "runtime-testsuite/test/org/antlr/v4/test/runtime/java/api/perf/TimeLexerSpeed.java")
             #t))
         (add-after 'install 'bin-install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((jar (string-append (assoc-ref outputs "out") "/share/java"))
                   (bin (string-append (assoc-ref outputs "out") "/bin")))
               (mkdir-p bin)
               (with-output-to-file (string-append bin "/antlr4")
                 (lambda _
                   (display
                     (string-append "#!" (which "sh") "\n"
                                    "java -cp " jar "/antlr4.jar:"
                                    (string-join
                                      (apply
                                        append
                                        (map
                                          (lambda (input)
                                            (find-files (assoc-ref inputs input)
                                                  ".*\\.jar"))
                                          '("antlr3" "java-stringtemplate"
                                            "java-antlr4-runtime" "java-treelayout"
                                            "java-jsonp-api" "java-icu4j")))
                                      ":")
                                    " org.antlr.v4.Tool $*"))))
               (chmod (string-append bin "/antlr4") #o755)
               #t)))
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "tool/resources/" "build/classes")
             #t))
         (add-before 'build 'generate-unicode
           (lambda _
             ;; First: build the generator
             (invoke "javac" "-cp" (getenv "CLASSPATH")
                     "tool/src/org/antlr/v4/unicode/UnicodeRenderer.java"
                     "tool/src/org/antlr/v4/unicode/UnicodeDataTemplateController.java")
             ;; Then use it
             (invoke "java" "-cp" (string-append (getenv "CLASSPATH")
                                                 ":tool/src:runtime/Java")
                     "org.antlr.v4.unicode.UnicodeRenderer"
                     "tool/resources/org/antlr/v4/tool/templates"
                     "unicodedata"
                     "tool/src/org/antlr/v4/unicode/UnicodeData.java")
             ;; It seems there is a bug with our ST4
             (substitute* "tool/src/org/antlr/v4/unicode/UnicodeData.java"
               (("\\\\>") ">"))
             ;; Remove the additional file
             (delete-file "tool/src/org/antlr/v4/unicode/UnicodeRenderer.java")
             #t))
         (add-before 'build 'generate-grammar
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "tool/src/org/antlr/v4/parse"
               (for-each (lambda (file)
                           (display file)
                           (newline)
                           (invoke "antlr3" file))
                         '("ANTLRLexer.g" "ANTLRParser.g" "BlockSetTransformer.g"
                           "GrammarTreeVisitor.g" "ATNBuilder.g"
                           "ActionSplitter.g" "LeftRecursiveRuleWalker.g")))
             (with-directory-excursion "tool/src/org/antlr/v4/codegen"
               (install-file "../parse/ANTLRParser.tokens" ".")
               (display "SourceGenTriggers.g\n")
               (invoke "antlr3" "SourceGenTriggers.g"))
             #t)))))
    (inputs
     (list antlr3
           java-antlr4-runtime
           java-icu4j
           java-jsonp-api
           java-stringtemplate
           java-treelayout))
    (native-inputs
     (list java-junit))
    (synopsis "Parser and lexer generator in Java")
    (description "ANTLR (ANother Tool for Language Recognition) is a powerful
parser generator for reading, processing, executing, or translating structured
text or binary files.  It's widely used to build languages, tools, and
frameworks.  From a grammar, ANTLR generates a parser that can build and walk
parse trees.")))

(define-public java-antlr4-runtime-4.1
  (package
    (inherit java-antlr4-runtime)
    (version "4.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/antlr/antlr4")
                     (commit version)))
              (file-name (git-file-name "antlr4" version))
              (sha256
               (base32
                "1i8hmx5an58cjyvhji0xgpvd6lq00z1k1mjys025q2wqc25wv4c1"))))
    (arguments
     (substitute-keyword-arguments (package-arguments java-antlr4-runtime)
       ((#:phases phases)
        `(modify-phases ,phases
           (add-before 'configure 'chmod
             (lambda _
               (chmod "build.xml" #o644)
               #t))))))
    (inputs
     (list java-treelayout))))

(define-public antlr4-4.1
  (package
    (inherit antlr4)
    (version (package-version java-antlr4-runtime-4.1))
    (source (package-source java-antlr4-runtime-4.1))
    (arguments
      (substitute-keyword-arguments (package-arguments antlr4)
        ((#:test-dir _)
         "tool/test")
        ((#:test-exclude excludes)
         `(list "**/TestParseErrors.java"
                "**/TestTopologicalSort.java"
                ,@excludes))
        ((#:phases phases)
         `(modify-phases ,phases
            (delete 'generate-unicode)
            (replace 'check
              (lambda _
                (invoke "ant" "check")
                #t))
            (add-before 'configure 'chmod
              (lambda _
                (chmod "build.xml" #o644)
                #t))
            (delete 'remove-graphemes)
            (delete 'remove-unrelated-languages)
            (delete 'generate-test-parsers)))))
    (inputs
      (alist-replace
        "java-antlr4-runtime" (list java-antlr4-runtime-4.1)
        (package-inputs antlr4)))))

(define-public java-tunnelvisionlabs-antlr4-runtime-annotations
  (package
    (inherit java-antlr4-runtime)
    (name "java-tunnelvisionlabs-antlr4-runtime-annotations")
    (version "4.7.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/tunnelvisionlabs/antlr4")
                     (commit (string-append version "-opt"))))
              (file-name (git-file-name "java-tunnelvisionlabs-antlr4" version))
              (sha256
               (base32
                "1mf2lvvsszpialsk23ma83pwp50nd32lrbjpa847zlm5gmranbr8"))
              (patches
                (search-patches "java-antlr4-Add-standalone-generator.patch"
                                "java-tunnelvisionlabs-antlr-code-too-large.patch"))))
    (arguments
     `(#:jar-name "java-antlr4-runtime-annotations.jar"
       #:source-dir "runtime/JavaAnnotations/src"
       #:tests? #f; no tests
       #:phases
       (modify-phases %standard-phases
         (add-after 'build 'copy-resources
           (lambda _
             (copy-recursively "runtime/JavaAnnotations/resources"
                               "build/classes")
             #t))
         (add-after 'copy-resources 'rebuild-jar
           (lambda _
             (invoke "ant" "jar")
             #t)))))
    (inputs '())
    (native-inputs '())
    (synopsis "Annotations for ANTLR's runtime library")
    (description "This package contains annotations used during the build of
the runtime library of ANTLR.")))

;; the runtime of this library requires a lexer that is generated by antlr4.
;; However, antlr4 itself requires this library at build and run-time.  We
;; use antlr4@4.1, the closest version of antlr that doesn't need this
;; bootstrap process, to generate the lexer.  The generated lexer is built
;; for the 4.1 runtime, which is slightly different from this runtime.
;; So, we build the runtime with antlr 4.1, with a broken xml lexer, that we
;; use to build antlr4.  We then re-use this antlr4 to build the runtime, and
;; the proper, working, runtime to build antlr4 again.
(define java-tunnelvisionlabs-antlr4-runtime-bootstrap
  (package
    (inherit java-antlr4-runtime)
    (name "java-tunnelvisionlabs-antlr4-runtime")
    (version (package-version java-tunnelvisionlabs-antlr4-runtime-annotations))
    (source (package-source java-tunnelvisionlabs-antlr4-runtime-annotations))
    (arguments
     `(#:jar-name "java-antlr4-runtime.jar"
       #:source-dir "runtime/Java/src"
       #:tests? #f; tests require antlr4, but antlr4 depends on this package
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'generate-xpath-lexer
           (lambda _
             (invoke "antlr4" "-lib" "runtime/Java/src/org/antlr/v4/runtime/tree/xpath"
                     "-visitor" "-no-listener"
                     "-package" "org.antlr.v4.runtime.tree.xpath"
                     "runtime/Java/src/org/antlr/v4/runtime/tree/xpath/XPathLexer.g4")
             ;; Generated code is for an incompatible version of the runtime
             (substitute* "runtime/Java/src/org/antlr/v4/runtime/tree/xpath/XPathLexer.java"
               (("LexerATNSimulator\\(this,_ATN,_decisionToDFA,_sharedContextCache\\)")
                "LexerATNSimulator(this,_ATN)"))
             #t))
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "runtime/Java/src/main/dot"
                               "build/classes")
             #t)))))
    (native-inputs
     `(("antlr4" ,antlr4-4.1)
       ("java-tunnelvisionlabs-antlr4-runtime-annotations"
        ,java-tunnelvisionlabs-antlr4-runtime-annotations)))))

(define java-tunnelvisionlabs-antlr4-bootstrap
  (package
    (inherit antlr4)
    (name "java-tunnelvisionlabs-antlr4")
    (version (package-version java-tunnelvisionlabs-antlr4-runtime-annotations))
    (source (package-source java-tunnelvisionlabs-antlr4-runtime-annotations))
    (arguments
     (substitute-keyword-arguments (package-arguments antlr4)
       ((#:test-dir _)
        "tool/test:runtime-testsuite/src")
       ((#:phases phases)
        `(modify-phases ,phases
           (delete 'remove-unrelated-languages)
           (delete 'remove-graphemes)
           (delete 'generate-test-parsers)
           (delete 'check)))))
    (native-inputs '())
    (inputs
     `(("antlr3" ,antlr3)
       ("java-antlr4-runtime" ,java-tunnelvisionlabs-antlr4-runtime-bootstrap)
       ("java-tunnelvisionlabs-antlr4-runtime-annotations"
        ,java-tunnelvisionlabs-antlr4-runtime-annotations)
       ("java-icu4j" ,java-icu4j)
       ("java-jsonp-api" ,java-jsonp-api)
       ("java-stringtemplate" ,java-stringtemplate)
       ("java-treelayout" ,java-treelayout)))))

(define-public java-tunnelvisionlabs-antlr4-runtime
  (package
    (inherit java-tunnelvisionlabs-antlr4-runtime-bootstrap)
    (native-inputs
      (alist-replace
        "antlr4" (list java-tunnelvisionlabs-antlr4-bootstrap)
        (package-native-inputs java-tunnelvisionlabs-antlr4-runtime-bootstrap)))))

(define-public java-tunnelvisionlabs-antlr4
  (package
    (inherit java-tunnelvisionlabs-antlr4-bootstrap)
    (inputs
      (alist-replace
        "java-antlr4-runtime" (list java-tunnelvisionlabs-antlr4-runtime)
        (package-inputs java-tunnelvisionlabs-antlr4-bootstrap)))))

(define-public java-commons-cli-1.2
  ;; This is a bootstrap dependency for Maven2.
  (package
    (inherit java-commons-cli)
    (version "1.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/commons/cli/source/"
                                  "commons-cli-" version "-src.tar.gz"))
              (sha256
               (base32
                "0rvfgzgv2pc1m091dfj3ih9ddsjjppr1f1wf0qmc3bk6b1kwv2dm"))))
    (arguments
     `(#:jar-name "commons-cli.jar"
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'fix-build-xml
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "build.xml"
               (("dir=\"\\$\\{test.home\\}/java\"")
                "dir=\"${test.home}\""))
             #t)))))
    (native-inputs
     (list java-junit))))

(define-public java-microemulator-cldc
  (package
    (name "java-microemulator-cldc")
    (version "2.0.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/barteo/microemu/archive/"
                                  "microemulator_"
                                  (string-map (lambda (c) (if (char=? c #\.) #\_ c))
                                              version)
                                  ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1x1apmz38gkppxnwnygwmi12j54v4p258v8ddzn6dldkk7vak1ll"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "microemulator-cldc.jar"
       #:source-dir "microemu-cldc/src/main/java"
       #:tests? #f)); Requires even older software
    (home-page "https://github.com/barteo/microemu")
    (synopsis "J2ME CLDC emulator")
    (description "Microemulator is a Java 2 Micro Edition (J2ME) CLDC/MIDP
Emulator.  It demonstrates MIDlet based applications in web browser
applet and can be run as a standalone java application.")
    (license (list license:asl2.0
                   ;; or altenatively:
                   license:lgpl2.1+))))

(define-public java-datanucleus-javax-persistence
  (package
    (name "java-datanucleus-javax-persistence")
    (version "2.2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/datanucleus/"
                                  "javax.persistence/archive/javax.persistence-"
                                  version "-release.tar.gz"))
              (sha256
               (base32
                "11jx0fjwgc2hhbqqgdd6m1pf2fplf9vslppygax0y1z5csnqjhpx"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "java-datanucleus-javax-persistence.jar"
       #:jdk ,icedtea-8
       #:source-dir "src/main/java"
       #:tests? #f)); no tests
    (home-page "https://github.com/datanucleus/javax.persistence")
    (synopsis "JPA API")
    (description "This package contains a clean definition of JPA API intended
for use with DataNucleus JPA since the JCP haven't provided an official JPA API
jar.  See @url{http://java.net/projects/jpa-spec/downloads} for the specification
used to generate this API.")
    (license (list license:edl1.0 license:epl1.0))))

(define-public java-osgi-cmpn
  (package
    (name "java-osgi-cmpn")
    (version "6.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://repo1.maven.org/maven2/"
                                  "org/osgi/osgi.cmpn/" version "/osgi.cmpn-"
                                  version "-sources.jar"))
              (sha256
               (base32
                "1lmb6xyrmkqdhv1kayf0514rlwq6ypvs4m44ibrck3snp8241wys"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "osgi-cmpn.jar"
       #:tests? #f)); no tests
    (inputs
     `(("annotation" ,java-osgi-annotation)
       ("core" ,java-osgi-core)
       ("java-datanucleus-javax-persistence" ,java-datanucleus-javax-persistence)
       ("microemulator" ,java-microemulator-cldc)
       ("servlet" ,java-classpathx-servletapi)))
    (home-page "https://www.osgi.org")
    (synopsis "Compendium specification module of OSGi framework")
    (description
      "OSGi, for Open Services Gateway initiative framework, is a module system
and service platform for the Java programming language.  This package contains
the compendium specification module, providing interfaces and classes for use
in compiling bundles.")
    (license license:asl2.0)))

(define-public java-osgi-service-component-annotations
  (package
    (name "java-osgi-service-component-annotations")
    (version "1.3.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://repo1.maven.org/maven2/org/osgi/"
                                  "org.osgi.service.component.annotations/"
                                  version "/org.osgi.service.component.annotations-"
                                  version "-sources.jar"))
              (sha256
               (base32
                "15rq9cmp4fpn74q44m4j35qsqmjf5lx3hcrk6pzvbhc08igic2f0"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "osgi-service-component-annotations.jar"
       #:tests? #f)); no tests
    (inputs
     `(("annotation" ,java-osgi-annotation)))
    (home-page "https://www.osgi.org")
    (synopsis "Support annotations for osgi-service-component")
    (description
      "OSGi, for Open Services Gateway initiative framework, is a module system
and service platform for the Java programming language.  This package contains
the support annotations for osgi-service-component.")
    (license license:asl2.0)))

(define-public java-osgi-dto
  (package
    (name "java-osgi-dto")
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://repo1.maven.org/maven2/org/osgi/"
                                  "org.osgi.dto/" version "/org.osgi.dto-"
                                  version "-sources.jar"))
              (sha256
               (base32
                "0f4bqjzadn0hwk6sd3h5gvbyfp3yci1s6r0v770cc15p0pg627yr"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "osgi-dto.jar"
       #:tests? #f)); no tests
    (inputs
     `(("annotation" ,java-osgi-annotation)))
    (home-page "https://www.osgi.org")
    (synopsis "Data Transfer Objects")
    (description
      "OSGi, for Open Services Gateway initiative framework, is a module system
and service platform for the Java programming language.  This package contains
the Data Transfer Objects.  It is easily serializable having only public fields
of primitive types and their wrapper classes, Strings, and DTOs.  List, Set,
Map and array aggregates may also be used.  The aggregates must only hold
objects of the listed types or aggregates.")
    (license license:asl2.0)))

(define-public java-osgi-resource
  (package
    (name "java-osgi-resource")
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://repo1.maven.org/maven2/org/osgi/"
                                  "org.osgi.resource/"
                                  version "/org.osgi.resource-"
                                  version "-sources.jar"))
              (sha256
               (base32
                "0hi0fsc5v99q22bd7lrkvpz1y0ds4w9arjldpwsrcpqvz2js7q2d"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "osgi-resource.jar"
       #:tests? #f)); no tests
    (inputs
     `(("annotation" ,java-osgi-annotation)
       ("dto" ,java-osgi-dto)))
    (home-page "https://www.osgi.org")
    (synopsis "OSGI Resource")
    (description
      "OSGi, for Open Services Gateway initiative framework, is a module system
and service platform for the Java programming language.  This package contains
the definition of common types in osgi packages.")
    (license license:asl2.0)))

(define-public java-osgi-namespace-contract
  (package
    (name "java-osgi-namespace-contract")
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://repo1.maven.org/maven2/org/osgi/"
                                  "org.osgi.namespace.contract/"
                                  version "/org.osgi.namespace.contract-"
                                  version "-sources.jar"))
              (sha256
               (base32
                "1iz4f2i0fvqrlq90ki9nfzcfpvy2av434ri25bglywqssx8mmp36"))))
    (build-system ant-build-system)
    (inputs
     `(("resource" ,java-osgi-resource)
       ("annotation" ,java-osgi-annotation)))
    (arguments
     `(#:jar-name "osgi-namespace-contract.jar"
       #:tests? #f)); no tests
    (home-page "https://www.osgi.org")
    (synopsis "Contract Capability and Requirement Namespace")
    (description
      "OSGi, for Open Services Gateway initiative framework, is a module system
and service platform for the Java programming language.  This package contains
the names for the attributes and directives for a namespace with contracts.")
    (license license:asl2.0)))

(define-public java-osgi-namespace-extender
  (package
    (name "java-osgi-namespace-extender")
    (version "1.0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://repo1.maven.org/maven2/org/osgi/"
                                  "org.osgi.namespace.extender/"
                                  version "/org.osgi.namespace.extender-"
                                  version "-sources.jar"))
              (sha256
               (base32
                "0jgqiak2i05qv6j3gd33xlaifzzc0ylxxk376v2x0apfg3vvixmz"))))
    (build-system ant-build-system)
    (inputs
     `(("resource" ,java-osgi-resource)
       ("annotation" ,java-osgi-annotation)))
    (arguments
     `(#:jar-name "osgi-namespace-extendent.jar"
       #:tests? #f)); no tests
    (home-page "https://www.osgi.org")
    (synopsis "Extender Capability and Requirement Namespace")
    (description
      "OSGi, for Open Services Gateway initiative framework, is a module system
and service platform for the Java programming language.  This package contains
the names for the attributes and directives for an extender namespace.")
    (license license:asl2.0)))

(define-public java-osgi-namespace-service
  (package
    (name "java-osgi-namespace-service")
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://repo1.maven.org/maven2/org/osgi/"
                                  "org.osgi.namespace.service/"
                                  version "/org.osgi.namespace.service-"
                                  version "-sources.jar"))
              (sha256
               (base32
                "0qmw8n2449nkmm56d1znz9zhazb6ya3vsimd5bf5jg23zzhgl8c8"))))
    (build-system ant-build-system)
    (inputs
     `(("resource" ,java-osgi-resource)
       ("annotation" ,java-osgi-annotation)))
    (arguments
     `(#:jar-name "osgi-namespace-service.jar"
       #:tests? #f)); no tests
    (home-page "https://www.osgi.org")
    (synopsis "Service Capability and Requirement Namespace")
    (description
      "OSGi, for Open Services Gateway initiative framework, is a module system
and service platform for the Java programming language.  This package contains
the names for the attributes and directives for a service namespace.")
    (license license:asl2.0)))

(define-public java-osgi-util-function
  (package
    (name "java-osgi-util-function")
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://repo1.maven.org/maven2/org/osgi/"
                                  "org.osgi.util.function/"
                                  version "/org.osgi.util.function-"
                                  version "-sources.jar"))
              (sha256
               (base32
                "04l7j3hwmmj28w23m7paca0afzncs42j2mdr3liqq8kvp548sc6x"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "osgi-util-function.jar"
       #:tests? #f)); no tests
    (inputs
     `(("annotation" ,java-osgi-annotation)))
    (home-page "https://www.osgi.org")
    (synopsis "OSGI Util Function")
    (description
      "OSGi, for Open Services Gateway initiative framework, is a module system
and service platform for the Java programming language.  This package contains
an interface for a function that accepts a single argument and produces a result.")
    (license license:asl2.0)))

(define-public java-osgi-util-promise
  (package
    (name "java-osgi-util-promise")
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://repo1.maven.org/maven2/org/osgi/"
                                  "org.osgi.util.promise/"
                                  version "/org.osgi.util.promise-"
                                  version "-sources.jar"))
              (sha256
               (base32
                "0y34dwiflg1c4ahvkswpf9z02xph2sr9fm04ia5493x3lshpw22c"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "osgi-util-promise.jar"
       #:tests? #f)); no tests
    (inputs
     `(("annotation" ,java-osgi-annotation)
       ("function" ,java-osgi-util-function)))
    (home-page "https://www.osgi.org")
    (synopsis "Promise of a value")
    (description
      "OSGi, for Open Services Gateway initiative framework, is a module system
and service platform for the Java programming language.  This package contains
an interface and utilitary classes for promises.  A Promise represents a future
value.  It handles the interactions for asynchronous processing.")
    (license license:asl2.0)))

(define-public java-osgi-service-metatype-annotations
  (package
    (name "java-osgi-service-metatype-annotations")
    (version "1.3.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://repo1.maven.org/maven2/org/osgi/"
                                  "org.osgi.service.metatype.annotations/"
                                  version "/org.osgi.service.metatype.annotations-"
                                  version "-sources.jar"))
              (sha256
               (base32
                "12rwm3349wk80vm88rcdgs4435m4jxkpkj5mrx326skkz2c6hyw6"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "osgi-service-metatype-annotations.jar"
       #:tests? #f)); no tests
    (inputs
     `(("annotation" ,java-osgi-annotation)))
    (home-page "https://www.osgi.org")
    (synopsis "Support annotations for metatype")
    (description
      "OSGi, for Open Services Gateway initiative framework, is a module system
and service platform for the Java programming language.  This package contains
the support annotations for metatype.")
    (license license:asl2.0)))

(define-public java-osgi-service-repository
  (package
    (name "java-osgi-service-repository")
    (version "1.1.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://repo1.maven.org/maven2/org/osgi/"
                                  "org.osgi.service.repository/"
                                  version "/org.osgi.service.repository-"
                                  version "-sources.jar"))
              (sha256
               (base32
                "1k41mhg7b58pd8nsghr2qwcjrxdnf1p9spsw9v11k4257g6rl06n"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "osgi-service-repository.jar"
       #:tests? #f)); no tests
    (inputs
     `(("annotation" ,java-osgi-annotation)
       ("promise" ,java-osgi-util-promise)
       ("resource" ,java-osgi-resource)))
    (home-page "https://www.osgi.org")
    (synopsis "OSGI service repository")
    (description
      "OSGi, for Open Services Gateway initiative framework, is a module system
and service platform for the Java programming language.  This package contains
a repository service that contains resources.")
    (license license:asl2.0)))

(define-public java-osgi-framework
  (package
    (name "java-osgi-framework")
    (version "1.8.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://repo1.maven.org/maven2/org/osgi/"
                                  "org.osgi.framework/" version "/org.osgi.framework-"
                                  version "-sources.jar"))
              (sha256
               (base32
                "1lwp2zfad3rybcc6q9bwz8xsgkc92ypzy5p6x54387f1qj65m73s"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "osgi-framework.jar"
       #:tests? #f)); no tests
    (inputs
     `(("annotation" ,java-osgi-annotation)
       ("resource" ,java-osgi-resource)
       ("dto" ,java-osgi-dto)))
    (home-page "https://www.osgi.org")
    (synopsis "OSGi framework")
    (description
      "OSGi, for Open Services Gateway initiative framework, is a module system
and service platform for the Java programming language.")
    (license license:asl2.0)))

(define-public java-osgi-service-log
  (package
    (name "java-osgi-service-log")
    (version "1.3.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://repo1.maven.org/maven2/org/osgi/"
                                  "org.osgi.service.log/"
                                  version "/org.osgi.service.log-"
                                  version "-sources.jar"))
              (sha256
               (base32
                "1029j30dzcwializzca0j3fkhwwz08kmmsha5agw1iccscimj6r0"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "osgi-service-log.jar"
       #:tests? #f)); no tests
    (inputs
     (list java-osgi-framework))
    (home-page "https://www.osgi.org")
    (synopsis "Provides methods for bundles to write messages to the log")
    (description
      "OSGi, for Open Services Gateway initiative framework, is a module system
and service platform for the Java programming language.  This package contains
the log service.")
    (license license:asl2.0)))

(define-public java-osgi-service-jdbc
  (package
    (name "java-osgi-service-jdbc")
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://repo1.maven.org/maven2/org/osgi/"
                                  "org.osgi.service.jdbc/"
                                  version "/org.osgi.service.jdbc-"
                                  version "-sources.jar"))
              (sha256
               (base32
                "11iln5v7bk469cgb9ddkrz9sa95b3733gqgaqw9xf5g6wq652yjz"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "osgi-service-jdbc.jar"
       #:tests? #f)); no tests
    (home-page "https://www.osgi.org")
    (synopsis "Factory for JDBC connection factories")
    (description
      "OSGi, for Open Services Gateway initiative framework, is a module system
and service platform for the Java programming language.  This package contains
a factory for JDBC connection factories.  There are 3 preferred connection
factories for getting JDBC connections:

@itemize
@item @code{javax.sql.DataSource};
@item @code{javax.sql.ConnectionPoolDataSource};
@item @code{javax.sql.XADataSource}.
@end itemize")
    (license license:asl2.0)))

(define-public java-osgi-service-resolver
  (package
    (name "java-osgi-service-resolver")
    (version "1.0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://repo1.maven.org/maven2/org/osgi/"
                                  "org.osgi.service.resolver/"
                                  version "/org.osgi.service.resolver-"
                                  version "-sources.jar"))
              (sha256
               (base32
                "1dzqn1ryfi2rq4zwsgp44bmj2wlfydjg1qbxw2b0z4xdjjy55vxd"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "osgi-service-resolver.jar"
       #:tests? #f)); no tests
    (inputs
     `(("annotation" ,java-osgi-annotation)
       ("resource" ,java-osgi-resource)))
    (home-page "https://www.osgi.org")
    (synopsis "OSGI Resolver service")
    (description
      "OSGi, for Open Services Gateway initiative framework, is a module system
and service platform for the Java programming language.  This package contains
a resolver service that resolves the specified resources in the context supplied
by the caller.")
    (license license:asl2.0)))

(define-public java-osgi-util-tracker
  (package
    (name "java-osgi-util-tracker")
    (version "1.5.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://repo1.maven.org/maven2/org/osgi/"
                                  "org.osgi.util.tracker/"
                                  version "/org.osgi.util.tracker-"
                                  version "-sources.jar"))
              (sha256
               (base32
                "0c4fh9vxwzsx59r8dygda0gq2gx3z5vfhc3jsphlqwf5w0h403lz"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "osgi-util-tracker.jar"
       #:tests? #f)); no tests
    (inputs
     `(("framework" ,java-osgi-framework)
       ("annotation" ,java-osgi-annotation)))
    (home-page "https://www.osgi.org")
    (synopsis "Bundle tracking")
    (description
      "OSGi, for Open Services Gateway initiative framework, is a module system
and service platform for the Java programming language.  This package contains
bundle tracking utility classes.")
    (license license:asl2.0)))

(define-public java-osgi-service-cm
  (package
    (name "java-osgi-service-cm")
    (version "1.5.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://repo1.maven.org/maven2/org/osgi/"
                                  "org.osgi.service.cm/"
                                  version "/org.osgi.service.cm-"
                                  version "-sources.jar"))
              (sha256
               (base32
                "1z8kap48y3xi0ggj8v6czglfnpnd94mmismgi2wbqhj1nl5fzbp6"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "osgi-service-cm.jar"
       #:tests? #f)); no tests
    (inputs
     `(("framework" ,java-osgi-framework)
       ("annotation" ,java-osgi-annotation)))
    (home-page "https://www.osgi.org")
    (synopsis "OSGI Configuration Management")
    (description
      "OSGi, for Open Services Gateway initiative framework, is a module system
and service platform for the Java programming language.  This package contains
utility classes for the configuration of services.")
    (license license:asl2.0)))

(define-public java-osgi-service-packageadmin
  (package
    (name "java-osgi-service-packageadmin")
    (version "1.2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://repo1.maven.org/maven2/org/osgi/"
                                  "org.osgi.service.packageadmin/"
                                  version "/org.osgi.service.packageadmin-"
                                  version "-sources.jar"))
              (sha256
               (base32
                "041mpxzi7g36wmcily6y4ccn3jx15akpdy8gmhyb7m98x7qfvn52"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "osgi-service-packageadmin.jar"
       #:tests? #f)); no tests
    (inputs
     `(("framework" ,java-osgi-framework)))
    (home-page "https://www.osgi.org")
    (synopsis "OSGI Package Administration")
    (description
      "OSGi, for Open Services Gateway initiative framework, is a module system
and service platform for the Java programming language.  This package contains
the packageadmin service.")
    (license license:asl2.0)))

(define-public java-ops4j-base-lang
  (package
    (name "java-ops4j-base-lang")
    (version "1.5.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/ops4j/org.ops4j.base/"
                                  "archive/base-" version ".tar.gz"))
              (sha256
               (base32
                "18hl3lpchgpv8yh5rlk39l2gif5dlfgb8gxjmncf39pr2dprkniw"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "java-ops4j-base-lang.jar"
       #:source-dir "ops4j-base-lang/src/main/java"
       #:tests? #f; no tests
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'add-test-file
           (lambda _
             ;; That file is required by a test in ops4j-pax-exam-core-spi
             (mkdir-p "build/classes/META-INF/maven/org.ops4j.base/ops4j-base-lang")
             (with-output-to-file "build/classes/META-INF/maven/org.ops4j.base/ops4j-base-lang/pom.properties"
               (lambda _
                 (display
                   (string-append
                     "version=" ,version "\n"
                     "groupId=org.ops4j.base"
                     "artifactId=ops4j-base-lang\n"))))
             #t)))))
    (home-page "https://ops4j1.jira.com/wiki/spaces/base/overview")
    (synopsis "Utility classes and extensions to be used in OPS4J projects")
    (description "OPS4J stands for Open Participation Software for Java.  This
package contains utilities and extensions related to @code{java.lang}.")
    (license license:asl2.0)))

(define-public java-ops4j-base-monitors
  (package
    (inherit java-ops4j-base-lang)
    (name "java-ops4j-base-monitors")
    (arguments
     `(#:jar-name "java-ops4j-base-monitors.jar"
       #:source-dir "ops4j-base-monitors/src/main/java"
       #:tests? #f)); no tests
    (inputs
     `(("lang" ,java-ops4j-base-lang)))
    (description "OPS4J stands for Open Participation Software for Java.  This
package contains utilities and extensions related to monitoring.")))

(define-public java-ops4j-base-io
  (package
    (inherit java-ops4j-base-lang)
    (name "java-ops4j-base-io")
    (arguments
     `(#:jar-name "java-ops4j-base-io.jar"
       #:source-dir "ops4j-base-io/src/main/java"
       #:test-dir "ops4j-base-io/src/test"
       #:test-exclude
       (list "**/ListerTest.java")))
    (inputs
     `(("lang" ,java-ops4j-base-monitors)
       ("lang" ,java-ops4j-base-lang)))
    (native-inputs
     `(("junit" ,java-junit)
       ("hamcrest" ,java-hamcrest-core)))
    (description "OPS4J stands for Open Participation Software for Java.  This
package contains utilities and extensions related to handling streams and files.")))

(define-public java-ops4j-base-util
  (package
    (inherit java-ops4j-base-lang)
    (name "java-ops4j-base-util")
    (arguments
     `(#:jar-name "java-ops4j-base-util.jar"
       #:source-dir "ops4j-base-util/src/main/java"
       #:test-dir "ops4j-base-util/src/test"))
    (inputs
     `(("lang" ,java-ops4j-base-lang)))
    (native-inputs
     `(("junit" ,java-junit)))
    (description "OPS4J stands for Open Participation Software for Java.  This
package contains utilities and extensions related to environment, i18n and
mime types.")))

(define-public java-ops4j-base-util-property
  (package
    (inherit java-ops4j-base-lang)
    (name "java-ops4j-base-util-property")
    (arguments
     `(#:jar-name "java-ops4j-base-util-property.jar"
       #:source-dir "ops4j-base-util-property/src/main/java"
       #:tests? #f)); no tests
    (inputs
     `(("lang" ,java-ops4j-base-lang)
       ("util" ,java-ops4j-base-util)))
    (description "OPS4J stands for Open Participation Software for Java.  This
package contains utilities and extensions related to resolving properties from
different sources.")))

(define-public java-ops4j-base-store
  (package
    (inherit java-ops4j-base-lang)
    (name "java-ops4j-base-store")
    (arguments
     `(#:jar-name "java-ops4j-base-store.jar"
       #:source-dir "ops4j-base-store/src/main/java"
       #:tests? #f)); no tests
    (inputs
     `(("lang" ,java-ops4j-base-lang)
       ("slf4j" ,java-slf4j-api)
       ("io" ,java-ops4j-base-io)))
    (description "OPS4J stands for Open Participation Software for Java.  This
package contains utilities for storing and retrieving data from an
@code{InputStream}.")))

(define-public java-ops4j-base-spi
  (package
    (inherit java-ops4j-base-lang)
    (name "java-ops4j-base-spi")
    (arguments
     `(#:jar-name "java-ops4j-base-spi.jar"
       #:source-dir "ops4j-base-spi/src/main/java"
       #:test-dir "ops4j-base-spi/src/test"))
    (native-inputs
     `(("junit" ,java-junit)
       ("hamcrest" ,java-hamcrest-core)))
    (description "OPS4J stands for Open Participation Software for Java.  This
package contains utilities for obtaining services via the Java SE 6
@code{ServiceLoader}.")))

(define-public java-aqute-bnd-annotation
  (package
    (name "java-aqute-bnd-annotation")
    (version "3.5.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/bndtools/bnd/archive/"
                                  version ".REL.tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1ggyiq0as0f6cz333a0dh98j72kmvv5pf2s47v9554yh905lfqdl"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "java-aqute-bnd-annotation.jar"
       #:source-dir "biz.aQute.bnd.annotation/src"
       #:tests? #f)); empty test dir
    (home-page "https://bnd.bndtools.org/")
    (synopsis "Tools for OSGi")
    (description "Bnd is a swiss army knife for OSGi, it creates manifest
headers based on analyzing the class code, it verifies the project settings,
it manages project dependencies, gives diffs jars, and much more.")
    (license license:asl2.0)))

(define-public java-aqute-libg
  (package
    (inherit java-aqute-bnd-annotation)
    (name "java-aqute-libg")
    (arguments
     `(#:jar-name "java-aqute-libg.jar"
       ;; The build fails when source/target more recent than 1.7. This
       ;; is a known issue. See: https://github.com/bndtools/bnd/issues/1327
       ;;
       ;; It is closed as won't fix. There is no way to change the source
       ;; so that it works on 1.8, and still works on 1.6, the upstream
       ;; target. It work fine on 1.7, so we use 1.7.
       #:make-flags (list "-Dant.build.javac.source=1.7"
                          "-Dant.build.javac.target=1.7")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'chdir
           ;; Change to aQute.libg directory, so that the relative
           ;; paths in the tests aren't broken.
           (lambda _
             (chdir "aQute.libg")
             #t))
         (add-before 'check 'create-test-directory
           ;; Copy the test directory to test/java, since that's where
           ;; ant-build-system's default project in build.xml expects to find
           ;; the test classes. Leave a copy in the original place to not
           ;; break paths in tests.
           (lambda _
             (mkdir "src/test")
             (copy-recursively "test" "src/test/java")
             #t)))))
    (inputs
     `(("slf4j" ,java-slf4j-api)
       ("osgi-annot" ,java-osgi-annotation)
       ("java-osgi-cmpn" ,java-osgi-cmpn)
       ("osgi" ,java-osgi-core)))
    (native-inputs
     `(("hamcrest" ,java-hamcrest-core)
       ("java-junit" ,java-junit)))))

(define-public java-aqute-bndlib
  (package
    (inherit java-aqute-bnd-annotation)
    (name "java-aqute-bndlib")
    (arguments
     `(#:jar-name "java-bndlib.jar"
       #:source-dir "biz.aQute.bndlib/src"
       #:tests? #f)); no tests
    (inputs
     `(("slf4j" ,java-slf4j-api)
       ("osgi-annot" ,java-osgi-annotation)
       ("java-aqute-libg" ,java-aqute-libg)
       ("java-aqute-bnd-annotation" ,java-aqute-bnd-annotation)
       ("java-osgi-service-component-annotations" ,java-osgi-service-component-annotations)
       ("java-osgi-service-repository" ,java-osgi-service-repository)
       ("java-osgi-service-log" ,java-osgi-service-log)
       ("java-osgi-service-metatype-annotations" ,java-osgi-service-metatype-annotations)
       ("java-osgi-namespace-contract" ,java-osgi-namespace-contract)
       ("java-osgi-namespace-extender" ,java-osgi-namespace-extender)
       ("java-osgi-namespace-service" ,java-osgi-namespace-service)
       ("promise" ,java-osgi-util-promise)
       ("osgi" ,java-osgi-core)))))

(define-public java-ops4j-pax-tinybundles
  (package
    (name "java-ops4j-pax-tinybundles")
    (version "2.1.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/ops4j/org.ops4j.pax.tinybundles/"
                                  "archive/tinybundles-" version ".tar.gz"))
              (sha256
               (base32
                "0y0gq3pvv0iir2b885lmlwnvr724vv7vklzhhr4fs27d7mdkj871"))))
    (arguments
     `(#:jar-name "java-ops4j-pax-tinybundles.jar"
       #:source-dir "src/main/java"
       #:test-exclude
       ;; Abstract base classes for other tests
       (list "**/BndTest.java" "**/CoreTest.java")
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'fix-version
           (lambda _
             ;; This test has a reference to an old version of bndlib we are not
             ;; packaging.  It uses the version referenced in pom.xml.  We replace
             ;; it with our own version.
             (substitute* "src/test/java/org/ops4j/pax/tinybundles/bnd/BndTest.java"
               (("[0-9][0-9]*\\.[0-9][0-9]*\\.[0-9][0-9]*\\.[0-9][0-9]*")
                ,(package-version java-aqute-bndlib)))
             #t)))))
    (inputs
     `(("lang" ,java-ops4j-base-lang)
       ("io" ,java-ops4j-base-io)
       ("store" ,java-ops4j-base-store)
       ("slf4j" ,java-slf4j-api)
       ("libg" ,java-aqute-libg)
       ("bndlib" ,java-aqute-bndlib)))
    (native-inputs
     `(("junit" ,java-junit)
       ("hamcrest" ,java-hamcrest-core)
       ("log4j" ,java-log4j-api)
       ("bndannotation" ,java-aqute-bnd-annotation)
       ("framework" ,java-osgi-framework)))
    (build-system ant-build-system)
    (home-page "https://ops4j1.jira.com/wiki/spaces/ops4j/pages/12060312/Tinybundles")
    (synopsis "Java APIs to create OSGi related artifacts")
    (description "Tinybundles is all about creating OSGi related artifacts like
Bundles, Fragments and Deployment Packages with Java Api.  It is very convenient
to create such artifacts on-the-fly inside Tests (like in Pax Exam).  On the
other hand, this library can be a foundation of real end user tools that need
to create those artifacts.")
    (license license:asl2.0)))

(define-public java-ops4j-pax-exam-core
  (package
    (name "java-ops4j-pax-exam-core")
    (version "4.11.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/ops4j/org.ops4j.pax.exam2/"
                                  "archive/exam-reactor-" version ".tar.gz"))
              (sha256
               (base32
                "08mzw8nkah3rj3vznlplnawspxhp61zgvb44ifqa1rni1cvbms2g"))))
    (arguments
     `(#:jar-name "java-ops4j-pax-exam-core.jar"
       #:source-dir "core/pax-exam/src/main/java"
       #:test-dir "core/pax-exam/src/test"))
    (inputs
     `(("slf4j" ,java-slf4j-api)
       ("lang" ,java-ops4j-base-lang)
       ("io" ,java-ops4j-base-io)
       ("util-property" ,java-ops4j-base-util-property)
       ("util-store" ,java-ops4j-base-store)
       ("java-osgi-core" ,java-osgi-core)))
    (native-inputs
     `(("junit" ,java-junit)
       ("hamcrest" ,java-hamcrest-core)))
    (build-system ant-build-system)
    (home-page "https://ops4j1.jira.com/wiki/spaces/PAXEXAM4/overview")
    (synopsis "In-Container Testing for OSGi, Java EE and CDI")
    (description "Pax Exam creates OSGi bundles for testing purposes.  It lets
the user take control of the OSGi framework, the test framework (e.g. JUnit) and
the system under test at the same time.")
    (license license:asl2.0)))

(define-public java-ops4j-pax-exam-core-spi
  (package
    (inherit java-ops4j-pax-exam-core)
    (name "java-ops4j-pax-exam-core-spi")
    (arguments
     `(#:jar-name "java-ops4j-pax-exam-spi.jar"
       #:source-dir "src/main/java"
       #:test-exclude
       (list
         ;; Abstract base class, not a test
         "**/BaseStagedReactorTest.java"
         ;; Depends on org.mortbay.jetty.testwars:test-war-dump
         "**/WarBuilderTest.java")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'chdir
           (lambda _
             ;; Tests assume we are in this directory
             (chdir "core/pax-exam-spi")
             #t))
         (add-before 'check 'fix-tests
           (lambda _
             ;; One test checks that this file is present.
             (mkdir-p "build/classes/META-INF/maven/org.ops4j.pax.exam/pax-exam-spi")
             (with-output-to-file
               "build/classes/META-INF/maven/org.ops4j.pax.exam/pax-exam-spi/pom.properties"
               (lambda _
                 (display
                   (string-append "artifactId = pax-exam-spi\n"
                                  "version = " ,(package-version java-ops4j-pax-exam-core-spi)))))
             ;; Maven puts compilation results in the target directory, while we
             ;; put them in the build directory.
             (substitute* '("src/test/java/org/ops4j/pax/exam/spi/war/WarBuilderTest.java"
                            "src/test/java/org/ops4j/pax/exam/spi/war/WarTestProbeBuilderTest.java"
                            "src/test/java/org/ops4j/pax/exam/spi/war/ZipBuilderTest.java")
               (("target") "build"))
             ;; One test is expected to fail, but it doesn't throw the expected exception
             (substitute* "src/test/java/org/ops4j/pax/exam/spi/reactors/BaseStagedReactorTest.java"
               (("AssertionError") "IllegalArgumentException"))
             #t)))))
    (inputs
     `(("java-ops4j-pax-exam-core" ,java-ops4j-pax-exam-core)
       ("lang" ,java-ops4j-base-lang)
       ("monitors" ,java-ops4j-base-monitors)
       ("store" ,java-ops4j-base-store)
       ("io" ,java-ops4j-base-io)
       ("spi" ,java-ops4j-base-spi)
       ("osgi" ,java-osgi-core)
       ("slf4j" ,java-slf4j-api)
       ("tinybundles" ,java-ops4j-pax-tinybundles)))
    (native-inputs
     `(("mockito" ,java-mockito-1)
       ("junit" ,java-junit)
       ("hamcrest" ,java-hamcrest-core)
       ("cglib" ,java-cglib)
       ("objenesis" ,java-objenesis)))))

(define-public java-ops4j-pax-exam-core-junit
  (package
    (inherit java-ops4j-pax-exam-core)
    (name "java-ops4j-pax-exam-core-junit")
    (arguments
     `(#:jar-name "ops4j-pax-exam-core-junit.jar"
       #:source-dir "drivers/pax-exam-junit4/src/main/java"
       #:tests? #f)); no tests
    (inputs
     `(("junit" ,java-junit)
       ("slf4j" ,java-slf4j-api)
       ("core" ,java-ops4j-pax-exam-core)
       ("spi" ,java-ops4j-pax-exam-core-spi)))
    (native-inputs '())))

(define-public java-fasterxml-jackson-annotations
  (package
    (name "java-fasterxml-jackson-annotations")
    (version "2.9.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/FasterXML/"
                                  "jackson-annotations/archive/"
                                  "jackson-annotations-" version ".tar.gz"))
              (sha256
               (base32
                "0mr95xd0da6a4g95zvrl1ryk5n5zv2rc696w3xnsr5hxk2gicfc4"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "jackson-annotations.jar"
       #:source-dir "src/main/java"
       #:test-dir "src/test"))
    (native-inputs
     `(("junit" ,java-junit)))
    (home-page "https://github.com/FasterXML/jackson-annotations")
    (synopsis "General purpose annotations for the Jackson Data Processor")
    (description "This package contains general purpose annotations for the
Jackson Data Processor, used on value and handler types.  The only annotations
not included are ones that require dependency to the Databind package.")
    (license license:asl2.0)))

(define-public java-fasterxml-jackson-core
  (package
    (name "java-fasterxml-jackson-core")
    (version "2.9.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/FasterXML/"
                                  "jackson-core/archive/"
                                  "jackson-core-" version ".tar.gz"))
              (sha256
               (base32
                "159hsnk17jr1gyzkf01cxvsn45srnk37g949r7364qlsr527gjgd"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "jackson-core.jar"
       #:source-dir "src/main/java"
       #:test-dir "src/test"
       #:test-exclude
       (list
         ;; Expected failure.  pom.xml excludes these
         "**/failing/**"
         ;; Base classes that have no constructor for junit
         "**/BaseTest.java"
         "**/ConcurrencyReadTest.java"
         "**/ManualCharAccessTest.java"
         "**/ManualCharAccessTest.java"
         "**/TrailingCommasTest.java"
         "**/AsyncMissingValuesInObjectTest.java"
         "**/AsyncMissingValuesInArrayTest.java")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'generate-PackageVersion.java
           (lambda _
             (let* ((out "src/main/java/com/fasterxml/jackson/core/json/PackageVersion.java")
                    (in (string-append out ".in")))
               (copy-file in out)
               (substitute* out
                 (("@package@") "com.fasterxml.jackson.core.json")
                 (("@projectversion@") ,version)
                 (("@projectgroupid@") "com.fasterxml.jackson.core")
                 (("@projectartifactid@") "jackson-core")))
             #t))
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "src/main/resources"
                               "build/classes")
             #t))
         (add-before 'check 'copy-test-resources
           (lambda _
             (copy-recursively "src/test/resources"
                               "build/test-classes")
             #t)))))
    (native-inputs
     `(("junit" ,java-junit)
       ("hamcrest" ,java-hamcrest-core)))
    (home-page "https://github.com/FasterXML/jackson-core")
    (synopsis "Low-level streaming parser and generator abstractions")
    (description "This package contains core low-level incremental
(streaming) parser and generator abstractions used by the Jackson Data
Processor.  It also includes the default implementation of handler types
(parser, generator) that handle JSON format.")
    (license license:asl2.0))); found on wiki.fasterxml.com/JacksonLicensing

(define-public java-fasterxml-jackson-databind
  (package
    (name "java-fasterxml-jackson-databind")
    (version "2.9.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/FasterXML/"
                                  "jackson-databind/archive/"
                                  "jackson-databind-" version ".tar.gz"))
              (sha256
               (base32
                "1zd2cw4z6kdkbx8za96xh9pyicv2a2l7y0rkcx2fqd8hv6d47s08"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "jackson-databind.jar"
       #:source-dir "src/main/java"
       #:tests? #f; requires javax.measures for which I can't find a free implementation
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'generate-PackageVersion.java
           (lambda _
             (let* ((out "src/main/java/com/fasterxml/jackson/databind/cfg/PackageVersion.java")
                    (in (string-append out ".in")))
               (copy-file in out)
               (substitute* out
                 (("@package@") "com.fasterxml.jackson.databind.cfg")
                 (("@projectversion@") ,version)
                 (("@projectgroupid@") "com.fasterxml.jackson.databind")
                 (("@projectartifactid@") "jackson-databind")))
             #t))
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "src/main/resources" "build/classes")
             #t)))))
    (inputs
     (list java-fasterxml-jackson-annotations java-fasterxml-jackson-core))
    (home-page "https://github.com/FasterXML/jackson-databind")
    (synopsis "Data-binding functionality and tree-model for the Jackson Data Processor")
    (description "This package contains the general-purpose data-binding
functionality and tree-model for Jackson Data Processor.  It builds on core
streaming parser/generator package, and uses Jackson Annotations for
configuration.")
    (license license:asl2.0))); found on wiki.fasterxml.com/JacksonLicensing

(define-public java-fasterxml-jackson-modules-base-jaxb
  (package
    (name "java-fasterxml-jackson-modules-base-jaxb")
    (version "2.9.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/FasterXML/"
                                  "jackson-modules-base/archive/"
                                  "jackson-modules-base-" version ".tar.gz"))
              (sha256
               (base32
                "1wws95xi8sppp6b0k2vvjdjyynl20r1a4dwrhai08lzlria6blp5"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "jackson-modules-base-jaxb.jar"
       #:source-dir "jaxb/src/main/java"
       #:test-dir "jaxb/src/test"
       #:test-exclude
       ;; Base class for tests
       (list "**/BaseJaxbTest.java")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'generate-PackageVersion.java
           (lambda _
             (let* ((out (string-append "jaxb/src/main/java/com/fasterxml/"
                                        "jackson/module/jaxb/PackageVersion.java"))
                    (in (string-append out ".in")))
               (copy-file in out)
               (substitute* out
                 (("@package@") "com.fasterxml.jackson.module.jaxb")
                 (("@projectversion@") ,version)
                 (("@projectgroupid@") "com.fasterxml.jackson.module.jaxb")
                 (("@projectartifactid@") "jackson-module-jaxb")))
             #t))
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "jaxb/src/main/resources" "build/classes")
             #t)))))
    (inputs
     (list java-fasterxml-jackson-annotations java-fasterxml-jackson-core
           java-fasterxml-jackson-databind))
    (native-inputs
     (list java-junit))
    (home-page "https://github.com/FasterXML/jackson-modules-base")
    (synopsis "Jaxb annotations jackson module")
    (description "This package is the jaxb annotations module for jackson.")
    (license license:asl2.0))); found on wiki.fasterxml.com/JacksonLicensing

(define-public java-fasterxml-jackson-modules-base-mrbean
  (package
    (name "java-fasterxml-jackson-modules-base-mrbean")
    (version "2.9.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/FasterXML/"
                                  "jackson-modules-base/archive/"
                                  "jackson-modules-base-" version ".tar.gz"))
              (sha256
               (base32
                "1wws95xi8sppp6b0k2vvjdjyynl20r1a4dwrhai08lzlria6blp5"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "jackson-modules-base-mrbean.jar"
       #:source-dir "mrbean/src/main/java"
       #:test-dir "mrbean/src/test"
       #:test-exclude
       ;; Base class for tests
       (list "**/BaseTest.java")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'generate-PackageVersion.java
           (lambda _
             (let* ((out (string-append "mrbean/src/main/java/com/fasterxml/"
                                        "jackson/module/mrbean/PackageVersion.java"))
                    (in (string-append out ".in")))
               (copy-file in out)
               (substitute* out
                 (("@package@") "com.fasterxml.jackson.module.mrbean")
                 (("@projectversion@") ,version)
                 (("@projectgroupid@") "com.fasterxml.jackson.module.mrbean")
                 (("@projectartifactid@") "jackson-module-mrbean")))
             #t)))))
    (inputs
     (list java-asm java-fasterxml-jackson-annotations
           java-fasterxml-jackson-core java-fasterxml-jackson-databind))
    (native-inputs
     (list java-junit))
    (home-page "https://github.com/FasterXML/jackson-modules-base")
    (synopsis "POJO type materialization for Java")
    (description "This package implements POJO type materialization.
Databinders can construct implementation classes for Java interfaces as part
of deserialization.")
    (license license:asl2.0)))

(define-public java-snakeyaml
  (package
    (name "java-snakeyaml")
    (version "1.18")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://bitbucket.org/asomov/snakeyaml/get/v"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0474cqcv46zgv9bhms2vgawakq1vyj0hp3h3f1bfys46msia90bh"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "java-snakeyaml.jar"
       #:source-dir "src/main/java"
       ;; Tests require velocity, a cyclic dependency, and
       ;; java-spring-framework-context which is not packaged.
       #:tests? #f))
    (home-page "https://bitbucket.org/asomov/snakeyaml")
    (synopsis "YAML processor")
    (description "SnakeYAML is a YAML processor for the Java Virtual Machine.")
    (license license:asl2.0))); found on wiki.fasterxml.com/JacksonLicensing

(define-public java-fasterxml-jackson-dataformat-yaml
  (package
    (name "java-fasterxml-jackson-dataformat-yaml")
    (version "2.9.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/FasterXML/"
                                  "jackson-dataformats-text/archive/"
                                  "jackson-dataformats-text-" version ".tar.gz"))
              (sha256
               (base32
                "1hikl06khaxbg439avf442qifcadap8w0lx13f0nnhp2vh3dkbz7"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "jackson-dataformat-yaml.jar"
       #:source-dir "yaml/src/main/java"
       #:test-dir "yaml/src/test"
       #:test-exclude (list "**/failing/**.java")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'generate-PackageVersion.java
           (lambda _
             (let* ((out "yaml/src/main/java/com/fasterxml/jackson/dataformat/yaml/PackageVersion.java")
                    (in (string-append out ".in")))
               (copy-file in out)
               (substitute* out
                 (("@package@") "com.fasterxml.jackson.dataformat.yaml")
                 (("@projectversion@") ,version)
                 (("@projectgroupid@") "com.fasterxml.jackson.dataformat.yaml")
                 (("@projectartifactid@") "jackson-dataformat-yaml")))
             #t)))))
    (inputs
     (list java-fasterxml-jackson-annotations java-fasterxml-jackson-core
           java-fasterxml-jackson-databind java-snakeyaml))
    (native-inputs
     `(("junit" ,java-junit)
       ("hamcrest" ,java-hamcrest-core)
       ("java-ops4j-pax-exam-core-spi" ,java-ops4j-pax-exam-core-spi)
       ("java-ops4j-pax-exam-core-junit" ,java-ops4j-pax-exam-core-junit)
       ("java-ops4j-pax-exam" ,java-ops4j-pax-exam-core)))
    (home-page "https://github.com/FasterXML/jackson-dataformats-text")
    (synopsis "Yaml backend for Jackson")
    (description "Dataformat backends are used to support format alternatives
to JSON, supported by default.  This is done by sub-classing Jackson core
abstractions.")
    (license license:asl2.0))); found on wiki.fasterxml.com/JacksonLicensing

(define-public java-stax2-api
  (package
    (name "java-stax2-api")
    (version "4.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/FasterXML/stax2-api/archive/"
                                  "stax2-api-" version ".tar.gz"))
              (sha256
               (base32
                "1amc1si0l0hyyw2sawmnzy4hkna3z6fp195y4nm5m9wb9ld5awkq"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "java-stax2-api.jar"
       #:source-dir "src/main/java"
       #:tests? #f)); no tests
    (home-page "https://github.com/FasterXML/stax2-api")
    (synopsis "Stax2 API")
    (description "Stax2 API is an extension to basic Stax 1.0 API that adds
significant new functionalities, such as full-featured bi-direction validation
interface and high-performance Typed Access API.")
    (license license:bsd-2)))

(define-public java-woodstox-core
  (package
    (name "java-woodstox-core")
    (version "5.0.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/FasterXML/woodstox")
                     (commit (string-append "woodstox-core-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0bfylk24a967hwxprxqbg6cdvm6n4ldcarp54yg980viwvjiglyp"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "woodstox.jar"
       #:test-exclude
       (list "**/Base*.java" "failing/**")
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'remove-msv-dep
           (lambda _
             ;; we don't need osgi, and it depends on msv
             (delete-file-recursively "src/main/java/com/ctc/wstx/osgi")
             ;; msv's latest release is from 2011 and we don't need it
             (delete-file-recursively "src/main/java/com/ctc/wstx/msv")
             (delete-file-recursively "src/test/java/wstxtest/osgi")
             (delete-file-recursively "src/test/java/wstxtest/msv")
             #t))
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "src/main/resources" "build/classes")
             #t)))))
    (inputs
     `(("stax2" ,java-stax2-api)))
    (native-inputs
     `(("junit" ,java-junit)))
    (home-page "https://github.com/FasterXML/woodstox")
    (synopsis "Stax XML API implementation")
    (description "Woodstox is a stax XML API implementation.")
    (license license:asl2.0)))

(define-public java-fasterxml-jackson-dataformat-xml
  (package
    (name "java-fasterxml-jackson-dataformat-xml")
    (version "2.9.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/FasterXML/jackson-dataformat-xml")
                     (commit (string-append "jackson-dataformat-xml-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0s1wl65mbs57c2hz2v8rnh8i04y5lpyyvnjz562j5j6b83vwwpfx"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "jackson-dataformat-xml.jar"
       #:source-dir "src/main/java"
       #:test-exclude
       (list "**/failing/**")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'generate-PackageVersion.java
           (lambda _
             (let* ((out "src/main/java/com/fasterxml/jackson/dataformat/xml/PackageVersion.java")
                    (in (string-append out ".in")))
               (copy-file in out)
               (newline)
               (substitute* out
                 (("@package@") "com.fasterxml.jackson.dataformat.xml")
                 (("@projectversion@") ,version)
                 (("@projectgroupid@") "com.fasterxml.jackson.dataformat.xml")
                 (("@projectartifactid@") "jackson-dataformat-xml")))
             #t))
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "src/main/resources" "build/classes")
             #t)))))
    (inputs
     `(("jackson-annotations" ,java-fasterxml-jackson-annotations)
       ("jackson-core" ,java-fasterxml-jackson-core)
       ("jackson-modules-base-jaxb" ,java-fasterxml-jackson-modules-base-jaxb)
       ("jackson-databind" ,java-fasterxml-jackson-databind)
       ("stax2-api" ,java-stax2-api)
       ("woodstox" ,java-woodstox-core)))
    (native-inputs
     `(("junit" ,java-junit)
       ("hamcrest" ,java-hamcrest-core)))
    (home-page "https://github.com/FasterXML/jackson-dataformat-xml")
    (synopsis "Read and write XML")
    (description "This package contains Jackson extension component for reading
and writing XML encoded data.

Further, the goal is to emulate how JAXB data-binding works with \"Code-first\"
approach (that is, no support is added for \"Schema-first\" approach).  Support
for JAXB annotations is provided by JAXB annotation module; this module
provides low-level abstractions (@code{JsonParser}, @code{JsonGenerator},
@code{JsonFactory}) as well as small number of higher level overrides needed to
make data-binding work.")
    (license license:asl2.0))); found on wiki.fasterxml.com/JacksonLicensing

(define-public java-hdrhistogram
  (package
    (name "java-hdrhistogram")
    (version "2.1.9")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/HdrHistogram/HdrHistogram")
                     (commit (string-append "HdrHistogram-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1cw8aa1vk258k42xs6wpy72m4gbai540jq032qsa7c5586iawx2d"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "java-hdrhistogram.jar"
       #:source-dir "src/main/java"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'make-files-writable
           (lambda _
             (for-each make-file-writable (find-files "."))
             #t))
         (add-before 'configure 'set-version
           (lambda _
             (let* ((version-java "src/main/java/org/HdrHistogram/Version.java")
                    (template (string-append version-java ".template")))
               (copy-file template version-java)
               (substitute* version-java
                 (("\\$VERSION\\$") ,version)
                 (("\\$BUILD_TIME\\$") "0"))
               #t))))))
    (native-inputs
     `(("junit" ,java-junit)
       ("hamcrest" ,java-hamcrest-core)))
    (home-page "https://hdrhistogram.github.io/HdrHistogram")
    (synopsis "High dynamic range histogram")
    (description "Hdrhistogram creates histograms that support
recording and analyzing sampled data value counts across a configurable integer
value range with configurable value precision within the range.  Value precision
is expressed as the number of significant digits in the value recording, and
provides control over value quantization behavior across the value range and
the subsequent value resolution at any given level.")
    (license license:public-domain)))

(define-public java-cofoja
  (package
    (name "java-cofoja")
    (version "1.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/nhatminhle/cofoja")
                    (commit (string-append "v" version))))
              (file-name (string-append "java-cofoja-" version "-checkout"))
              (sha256
               (base32
                "0p7sz8y5xgpi5rx1qwn6587fkd52qr3ha3ybh14gqcyxhikl525w"))))
    (build-system ant-build-system)
    (arguments
     `(#:build-target "dist"
       #:test-target "test"
       #:jdk ,icedtea-8
       #:make-flags
       (list "-Ddist.dir=dist")
       #:modules ((guix build ant-build-system)
                  (guix build java-utils)
                  (guix build utils)
                  (srfi srfi-1)
                  (ice-9 match))
       #:phases
       (modify-phases %standard-phases
         ;; The bulid system ignores the class path the ant-build-system sets
         ;; up and instead expects to find all dependencies in the "lib"
         ;; directory.
         (add-after 'unpack 'create-libdir
           (lambda* (#:key inputs #:allow-other-keys)
             (mkdir-p "lib")
             (for-each
              (lambda (file)
                (let ((target (string-append "lib/" (basename file))))
                  (unless (file-exists? target)
                    (symlink file target))))
              (append-map (match-lambda
                            ((label . dir)
                             (find-files dir "\\.jar$")))
                          inputs))
             #t))
         (replace 'install (install-jars "dist")))))
    (inputs
     (list java-asm))
    (native-inputs
     (list java-junit))
    (home-page "https://github.com/nhatminhle/cofoja")
    (synopsis "Contracts for Java")
    (description "Contracts for Java, or Cofoja for short, is a contract
programming framework and test tool for Java, which uses annotation processing
and bytecode instrumentation to provide run-time checking. (In particular,
this is not a static analysis tool.)")
    (license license:lgpl3+)))

(define-public java-aopalliance
  (package
    (name "java-aopalliance")
    (version "1.0")
    (source (origin
              (method git-fetch)
              ;; Note: this git repository is not official, but contains the
              ;; source code that is in the CVS repository.  Downloading the
              ;; tarball from sourceforge is undeterministic, and the cvs download
              ;; fails.
              (uri (git-reference
                     (url "https://github.com/hoverruan/aopalliance")
                     (commit "0d7757ae204e5876f69431421fe9bc2a4f01e8a0")))
              (file-name (string-append name "-" version))
              (sha256
               (base32
                "0rsg2b0v3hxlq2yk1i3m2gw3xwq689j3cwx9wbxvqfpdcjbca0qr"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "java-aopalliance.jar"
       #:jdk ,icedtea-8
       #:tests? #f; no tests
       #:source-dir "aopalliance/src/main"
       #:phases
       (modify-phases %standard-phases
         (add-before 'install 'create-pom
           (generate-pom.xml "pom.xml" "aopalliance" "aopalliance" ,version))
         (replace 'install
           (install-from-pom "pom.xml")))))
    (home-page "https://aopalliance.sourceforge.net")
    (synopsis "Aspect-Oriented Programming")
    (description "The AOP Alliance project is a joint project between several
software engineering people who are interested in Aspect-Oriented Programming
(AOP) and Java.")
    (license license:public-domain)))

(define-public java-javax-inject
  (package
    (name "java-javax-inject")
    (version "1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/javax-inject/javax-inject")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1rspl0nkvk1jif6nccikw93xic6ljj2b6kpy2mffwi2mnvc13j7x"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "java-javax-inject.jar"
       #:tests? #f; no tests
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (install-from-pom "pom.xml")))))
    (home-page "https://github.com/javax-inject/javax-inject")
    (synopsis "JSR-330: Dependency Injection for Java")
    (description "This package specifies a means for obtaining objects in such
a way as to maximize reusability, testability and maintainability compared to
traditional approaches such as constructors, factories, and service locators
(e.g., JNDI).  This process, known as dependency injection, is beneficial to
most nontrivial applications.

Many types depend on other types.  For example, a @var{Stopwatch} might depend
on a @var{TimeSource}.  The types on which a type depends are known as its
dependencies.  The process of finding an instance of a dependency to use at run
time is known as resolving the dependency.  If no such instance can be found,
the dependency is said to be unsatisfied, and the application is broken.")
    (license license:asl2.0)))

(define-public java-guice
  (package
    (name "java-guice")
    (version "4.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/google/guice")
                     (commit version)))
              (file-name (git-file-name name version))
              (modules '((guix build utils)))
              (snippet
               `(begin
                  (for-each delete-file (find-files "." ".*.jar")) #t))
              (sha256
               (base32
                "18im5hdfl4q1b9chww2s1ii60sn3ydyyar32a2sf2p2g8zlbdswq"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "java-guice.jar"
       #:jdk ,icedtea-8
       #:tests? #f; FIXME: tests are not in a java sub directory
       #:source-dir "core/src"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'make-files-writable
           (lambda _
             (for-each make-file-writable (find-files "."))
             #t))
         (replace 'install
           (install-from-pom "core/pom.xml")))))
    (propagated-inputs
     (list java-aopalliance
           java-cglib
           java-guava
           java-javax-inject
           java-guice-parent-pom))
    (home-page "https://github.com/google/guice")
    (synopsis "Lightweight dependency injection framework")
    (description "Guice is a lightweight dependency injection framework for
Java 6 and above.")
    (license license:asl2.0)))

(define-public java-guice-servlet
  (package
    (inherit java-guice)
    (name "java-guice-servlet")
    (arguments
     `(#:jar-name "guice-servlet.jar"
       #:source-dir "extensions/servlet/src/"
       #:jdk ,icedtea-8
       #:tests? #f  ; FIXME: not in a java subdir
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'make-files-writable
           (lambda _
             (for-each make-file-writable (find-files "."))
             #t)))))
    (inputs
     `(("guice" ,java-guice)
       ("servlet"  ,java-classpathx-servletapi)
       ,@(package-inputs java-guice)))))

(define java-guice-parent-pom
  (package
    (inherit java-guice)
    (name "java-guice-parent-pom")
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (add-after 'install 'install-extensions
           (install-pom-file "extensions/pom.xml"))
         (replace 'install
           (install-pom-file "pom.xml")))))
    (propagated-inputs
     `(("java-google-parent-pom" ,java-google-parent-pom-5)))))

(define java-google-parent-pom-5
  (package
    (name "java-google-parent-pom")
    (version "5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/google/google-maven-parents")
                     (commit (string-append "google-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0zb7hx24p8k8rfdvix2vsbfqn73jhrycdndvhf8j5gbii9wbqibv"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (replace 'install
           (install-pom-file "pom.xml")))))
    (home-page "https://github.com/google/google-maven-parents")
    (synopsis "Google parent pom")
    (description "This package contains the Maven parent POM for other Google
Java projects.")
    (license license:asl2.0)))

(define-public java-assertj
  (package
    (name "java-assertj")
    (version "3.8.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/joel-costigliola/assertj-core")
                     (commit (string-append "assertj-core-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1k35cg2in7pzk4pbdjryr0pll5lgk1r6ngrn0j8cdlgi7w8zh2d1"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "java-assertj.jar"
       #:jdk ,icedtea-8
       #:source-dir "src/main/java"
       #:tests? #f)); depends on tng-junit which depends on assertj
    (inputs
     `(("cglib" ,java-cglib)
       ("junit" ,java-junit)
       ("hamcrest" ,java-hamcrest-core)))
    (native-inputs
     `(("mockito" ,java-mockito-1)))
    (home-page "https://joel-costigliola.github.io/assertj/index.html")
    (synopsis "Fluent assertions for java")
    (description "AssertJ core is a Java library that provides a fluent
interface for writing assertions.  Its main goal is to improve test code
readability and make maintenance of tests easier.")
    (license license:asl2.0)))

(define-public java-jboss-javassist
  (package
    (name "java-jboss-javassist")
    (version "3.21.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/jboss-javassist/javassist")
                     (commit
                       (string-append "rel_"
                                      (string-map
                                        (lambda (x) (if (eq? x #\.) #\_ x)) version)
                                      "_ga"))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0h3zlcyqiaq01fspm69h7vki67raw305w89p4ha8vlhpzw02qifm"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (delete-file "javassist.jar")))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "java-jboss-javassist.jar"
       #:jdk ,icedtea-8
       #:source-dir "src/main"
       #:tests? #f; FIXME: requires junit-awtui and junit-swingui from junit3
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'make-files-writable
           (lambda _
             (for-each make-file-writable (find-files "."))
             #t)))))
    (native-inputs
     `(("junit" ,java-junit)))
    (home-page "https://github.com/jboss-javassist/javassist")
    (synopsis "Java bytecode engineering toolkit")
    (description "Javassist (JAVA programming ASSISTant) makes Java bytecode
manipulation simple.  It is a class library for editing bytecodes in Java; it
enables Java programs to define a new class at runtime and to modify a class
file when the JVM loads it.")
    (license (list license:gpl2 license:cddl1.0)))); either gpl2 only or cddl.

(define-public java-jcommander
  (package
    (name "java-jcommander")
    (version "1.71")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/cbeust/jcommander")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "12vcpc19sd7jhvjgp7xz1qjanfix162xb3x2q5zah93rjklj1h57"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "java-jcommander.jar"
       #:jdk ,icedtea-8
       #:tests? #f; requires testng which depends on jcommander
       #:source-dir "src/main/java"))
    (home-page "https://jcommander.org")
    (synopsis "Command line parameters parser")
    (description "JCommander is a very small Java framework that makes it
trivial to parse command line parameters.  Parameters are declared with
annotations.")
    (license license:asl2.0)))

(define-public java-bsh
  (package
    (name "java-bsh")
    (version "2.1.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/beanshell/beanshell")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1a6y46yz2ba4mnlfv4bpd5pmzhgxrzk3s10xp05jz377nbp2izwg"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Delete bundled third-party jar archives.
                  (for-each delete-file (find-files "." ".*.jar$"))
                  (for-each (lambda (file) (chmod file #o644))
                            (find-files "." "."))
                  #t))))
    (build-system ant-build-system)
    (arguments
     `(#:build-target "jarall"
       #:test-target "junit-tests-all"
       #:make-flags (list "-DDATE" "(no date for reproducibility)")
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'fix-test
           (lambda _
             (substitute* "tests/junitTests/src/bsh/Issue_55_Test.java"
               ((" BshScriptEngineFactory") " bsh.engine.BshScriptEngineFactory"))))
         (replace 'install
           (install-from-pom "pom.xml")))))
    (inputs
     (list java-classpathx-servletapi java-commons-bsf))
    (native-inputs
     (list java-junit javacc-3))
    (home-page "http://beanshell.org/")
    (synopsis "Lightweight Scripting for Java")
    (description "BeanShell is a small, free, embeddable Java source
interpreter with object scripting language features, written in Java.
BeanShell dynamically executes standard Java syntax and extends it with common
scripting conveniences such as loose types, commands, and method closures like
those in Perl and JavaScript.")
    (license license:asl2.0)))

(define-public java-fest-util
  (package
    (name "java-fest-util")
    (version "1.2.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/alexruiz/fest-util/")
                     (commit (string-append "fest-util-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "02kgal7v85snyyvcsxvn4qphid455f4smh2wri1il8d9asw0djbz"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "java-fest-util.jar"
       #:source-dir "src/main/java"))
    (native-inputs
     `(("junit" ,java-junit)
       ("hamcrest" ,java-hamcrest-core)))
    (home-page "https://github.com/alexruiz/fest-util")
    (synopsis "FEST common utilities")
    (description "Common utilities used in all FEST module.")
    (license license:asl2.0)))

(define-public java-fest-test
  (package
    (name "java-fest-test")
    (version "2.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/alexruiz/fest-test/")
                     (commit (string-append "fest-test-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0mg1d2jfh7kbx2c40dchbjr6d8pv59snsyb13mfxsr7xk5n69qbn"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "java-fest-test.jar"
       #:source-dir "src/main/java"
       #:tests? #f)); no tests
    (inputs
     `(("junit" ,java-junit)))
    (home-page "https://github.com/alexruiz/fest-test")
    (synopsis "Common FEST testing infrastructure")
    (description "Fest-test contains the common FEST testing infrastructure.")
    (license license:asl2.0)))

(define-public java-fest-assert
  (package
    (name "java-fest-assert")
    (version "2.0M10")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/alexruiz/fest-assert-2.x/")
                     (commit (string-append "fest-assert-core-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1cp8zzyag3s85fz2w68sda9zzaal1y5f9wl8g72wkm12an40w6by"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "java-fest-assert.jar"
       #:source-dir "src/main/java"
       #:test-exclude
       (list
         "**/Abstract*.java"
         "**/*BaseTest.java"
         ;; Unable to set MockitoNamingPolicy on cglib generator which creates FastClasses
         "**/MessageFormatter_format_Test.java"
         "**/internal/*/*_assert*_Test.java")))
    (inputs
     (list java-fest-util))
    (native-inputs
     (list java-junit
           java-fest-test
           java-hamcrest-core
           java-mockito-1
           java-cglib
           java-objenesis))
    (home-page "https://github.com/alexruiz/fest-assert-2.x")
    (synopsis "FEST fluent assertions")
    (description "FEST-Assert provides a fluent interface for assertions.")
    (license license:asl2.0)))

(define-public java-testng
  (package
    (name "java-testng")
    (version "6.14.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/cbeust/testng")
                    (commit version)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "0y6vq30i5g276kw0v2bhbvci22ijg7ax49ap2611yqlhbs4d6dqv"))))
    (build-system ant-build-system)
    (arguments
     (list
      #:jdk icedtea-8; java.util.function
      #:jar-name "java-testng.jar"
      #:source-dir "src/main/java"
      #:phases
      '(modify-phases %standard-phases
         (add-after 'unpack 'delete-failing-tests
           (lambda _
             ;; FIXME: I don't know why these tests fail
             (substitute* "src/test/resources/testng.xml"
               ;; expected:<[0]> but was:<[3]>
               (("<class name=\"test.configuration.github1625.TestRunnerIssue1625\"/>") "")
               ;; expected [3] but found [2]
               (("<class name=\"test.serviceloader.ServiceLoaderTest\" />") "")
               ;; This is a parallel test and we've observed that it fails
               ;; sometimes.
               (("<class name=\"test.dataprovider.DataProviderTest\"/>") ""))))
         ;; We don't have groovy
         (add-after 'unpack 'delete-groovy-tests
           (lambda _
             (delete-file-recursively "src/test/java/test/dependent/issue1648/")
             (substitute* "src/test/resources/testng.xml"
               (("<class name=\"test.dependent.issue1648.TestRunner\"/>") ""))))
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "src/main/resources" "build/classes")))
         (add-before 'check 'copy-test-resources
           (lambda _
             (copy-recursively "src/test/resources" "build/test-classes")))
         (replace 'check
           (lambda _
             (invoke "ant" "compile-tests")
             ;; we don't have groovy
             (substitute* "src/test/resources/testng.xml"
               (("<class name=\"test.groovy.GroovyTest\" />") ""))
             (invoke "java" "-cp" (string-append (getenv "CLASSPATH")
                                                 ":build/classes"
                                                 ":build/test-classes")
                     "-Dtest.resources.dir=src/test/resources"
                     "org.testng.TestNG" "src/test/resources/testng.xml"))))))
    (propagated-inputs
     (list java-junit
           java-jsr305
           java-bsh
           java-jcommander
           java-guice
           java-snakeyaml))
    (native-inputs
     (list java-guava
           java-javax-inject
           java-hamcrest-all
           java-assertj
           java-mockito-1
           java-cglib
           java-aopalliance))
    (home-page "https://testng.org")
    (synopsis "Testing framework")
    (description "TestNG is a testing framework inspired from JUnit and NUnit
but introducing some new functionalities that make it more powerful and easier
to use.")
    (license license:asl2.0)))

(define-public java-jnacl
  (let ((commit "094e819afdd63ea81a499b3bcb42a271006bebd9")
        (revision "2"))
    (package
      (name "java-jnacl")
      (version (string-append "0.1.0-" revision "." (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/neilalexander/jnacl")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1d6g6xhn83byv5943n7935wwjsk0ibk0qdvqgr699qqgqqmwisbb"))))
      (build-system ant-build-system)
      (arguments
       `(#:jar-name "jnacl.jar"
         #:source-dir "src/main/java"
         #:jdk ,icedtea-8
         #:phases
         (modify-phases %standard-phases
           (add-before 'build 'fix-tests
             (lambda _
               (substitute* '("src/test/java/com/neilalexander/jnacl/NaClTest.java"
                              "src/test/java/com/neilalexander/jnacl/NaclSecretBoxTest.java")
                 (("assertions.Assertions") "assertions.api.Assertions"))
               #t))
           (replace 'check
             (lambda _
               (invoke "ant" "compile-tests")
               (invoke "java" "-cp" (string-append (getenv "CLASSPATH")
                                                   ":build/classes"
                                                   ":build/test-classes")
                       "org.testng.TestNG" "-testclass"
                       "build/test-classes/com/neilalexander/jnacl/NaclSecretBoxTest.class")
               (invoke "java" "-cp" (string-append (getenv "CLASSPATH")
                                                   ":build/classes"
                                                   ":build/test-classes")
                       "org.testng.TestNG" "-testclass"
                       "build/test-classes/com/neilalexander/jnacl/NaClTest.class"))))))
      (native-inputs
       (list java-testng java-fest-util java-fest-assert))
      (home-page "https://github.com/neilalexander/jnacl")
      (synopsis "Java implementation of NaCl")
      (description "Pure Java implementation of the NaCl: Networking and
Cryptography library.")
      (license license:bsd-2))))

(define-public java-mvel2
  (package
    (name "java-mvel2")
    (version "2.3.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/mvel/mvel/archive/mvel2-"
                                  version ".Final.tar.gz"))
              (sha256
               (base32
                "01ph5s9gm16l2qz58lg21w6fna7xmmrj7f9bzqr1jim7h9557d3z"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "mvel2.jar"
       #:source-dir "src/main/java"
       #:test-exclude
       (list "**/Abstract*.java"
             ;; Base class with no tests
             "**/MVELThreadTest.java")
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-bin
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((bin (string-append (assoc-ref outputs "out") "/bin")))
               (mkdir-p bin)
               (with-output-to-file (string-append bin "/mvel2")
                 (lambda _
                   (display
                     (string-append
                       "#!" (which "bash") "\n"
                       "if [ \"$#\" -ne \"2\" ]; then\n"
                       "echo 'Usage: mvel2 <script> <out.dir>'\n"
                       "exit\n"
                       "fi\n"
                       "java -Dout.dir=$2 -cp " (getenv "CLASSPATH")
                       ":" (assoc-ref outputs "out") "/share/java/mvel2.jar"
                       " org.mvel2.sh.Main $1"))))
               (chmod (string-append bin "/mvel2") #o755))
             #t)))))
    (native-inputs
     `(("junit" ,java-junit)
       ("hamcrest" ,java-hamcrest-core)))
    (home-page "https://github.com/mvel/mvel")
    (synopsis "MVFLEX Expression Language")
    (description "MVEL has largely been inspired by Java syntax, but has some
fundamental differences aimed at making it more efficient as an expression
language, such as operators that directly support collection, array and string
matching, as well as regular expressions.  MVEL is used to evaluate expressions
written using Java syntax.

In addition to the expression language, MVEL serves as a templating language for
configuration and string construction.")
    (license license:asl2.0)))

(define-public java-commons-jexl-2
  (package
    (name "java-commons-jexl")
    (version "2.1.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/commons/jexl/source/"
                                  "commons-jexl-" version "-src.tar.gz"))
              (sha256
               (base32
                "1ai7632bwwaxglb0nbpblpr2jw5g20afrsaq372ipkphi3ncy1jz"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "commons-jexl-2.jar"
       #:jdk ,icedtea-8
       #:source-dir "src/main/java"
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'disable-broken-tests
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "src/test/java/org/apache/commons/jexl2/"
               (substitute* "ArithmeticTest.java"
                 (("asserter.assertExpression\\(\"3 / 0\"") "//")
                 (("asserter.assertExpression\\(\"imanull") "//"))
               ;; This test fails with "ambiguous method invocation"
               (delete-file "CacheTest.java")
               ;; This test doesn't have access to the temp directory
               (substitute* "ClassCreatorTest.java"
                 (("java.io.tmpdir") "user.dir"))
               ;; This test fails in trying to detect whether it can run.
               (substitute* "ClassCreator.java"
                 (("boolean canRun =.*") "boolean canRun = false;\n"))
               ;; ...and these tests depend on it.
               (delete-file "scripting/JexlScriptEngineOptionalTest.java")
               (delete-file "scripting/JexlScriptEngineTest.java"))
             #t))
         (add-before 'build 'run-javacc
           (lambda _
             (with-directory-excursion "src/main/java/org/apache/commons/jexl2/parser/"
               (invoke "java" "jjtree" "Parser.jjt")
               (invoke "java" "javacc" "Parser.jj"))
             #t)))))
    (inputs
     (list java-commons-logging-minimal))
    (native-inputs
     (list java-junit java-hamcrest-core javacc-4))
    (home-page "https://commons.apache.org/proper/commons-jexl/")
    (synopsis "Java Expression Language")
    (description "JEXL is a library intended to facilitate the implementation
of dynamic and scripting features in applications and frameworks written in
Java.  JEXL implements an Expression Language based on some extensions to the
JSTL Expression Language supporting most of the constructs seen in
shell-script or ECMAScript.  Its goal is to expose scripting features usable
by technical operatives or consultants working with enterprise platforms.")
    (license license:asl2.0)))

(define-public java-lz4
  (package
    (name "java-lz4")
    (version "1.4.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/lz4/lz4-java")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0ydjakhv3cz34mfvv14qrh2ksdxifgjwwagjy7r46qr3f68hnf6y"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "lz4.jar"
       #:jdk ,icedtea-8
       #:source-dir "src/java:src/java-unsafe"
       #:tests? #f; FIXME: requires more dependencies
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'make-files-writable
           (lambda _
             (for-each make-file-writable (find-files "."))
             #t))
         (add-before 'configure 'generate-source
           (lambda _
             (with-directory-excursion "src/build/source_templates"
               (invoke "mvel2" "../gen_sources.mvel" "../../java"))
             #t)))))
    (native-inputs
     `(("mvel" ,java-mvel2)))
    (home-page "https://jpountz.github.io/lz4-java")
    (synopsis "Compression algorithm")
    (description "LZ4 - Java is a Java port of the popular lz4 compression
algorithms and xxHash hashing algorithm.")
    (license license:asl2.0)))

(define-public java-bouncycastle
  (package
    (name "java-bouncycastle")
    (version "1.67")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "http://git.bouncycastle.org/repositories/bc-java")
                     ;(url "https://github.com/bcgit/bc-java")
                     (commit (string-append "r1rv" (substring version 2 4)))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1449q7fyh03s1q0bqljcrhgacwcyqmg2bbvb3z084avgapwsainz"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (for-each delete-file
                            (find-files "." "\\.jar$"))
                  #t))))
    (build-system ant-build-system)
    (arguments
     `(#:jdk ,icedtea-8
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda _
             (invoke "ant" "-f" "ant/jdk15+.xml" "build-provider")
             (invoke "ant" "-f" "ant/jdk15+.xml" "build")))
         ;; FIXME: the tests freeze.
         ;; (replace 'check
         ;;   (lambda _
         ;;     (invoke "ant" "-f" "ant/jdk15+.xml" "test")))
         (replace 'install
           (install-jars "build/artifacts/jdk1.5/jars")))))
    (inputs
     (list java-javax-mail))
    (native-inputs
     `(("unzip" ,unzip)
       ("junit" ,java-junit)
       ("java-native-access" ,java-native-access)
       ("java-native-access-platform" ,java-native-access-platform)))
    (home-page "https://www.bouncycastle.org")
    (synopsis "Cryptographic library")
    (description "Bouncy Castle is a cryptographic library for the Java
programming language.")
    (license license:expat)))

(define-public java-lmax-disruptor
  (package
    (name "java-lmax-disruptor")
    (version "3.4.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/LMAX-Exchange/disruptor")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "02c5kp3n8a73dq9ay7ar53s1k3x61z9yzc5ikqb03m6snr1wpfqn"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "java-lmax-disruptor.jar"
       #:jdk ,icedtea-8
       #:tests? #f)); tests hang
    (inputs
     `(("junit" ,java-junit)
       ("java-hdrhistogram" ,java-hdrhistogram)
       ("java-jmock" ,java-jmock)
       ("java-jmock-legacy" ,java-jmock-legacy)
       ("java-jmock-junit4" ,java-jmock-junit4)
       ("java-hamcrest-all" ,java-hamcrest-all)))
    (native-inputs
     `(("cglib" ,java-cglib)
       ("objenesis" ,java-objenesis)))
    (home-page "https://www.lmax.com/disruptor")
    (synopsis "High performance inter-thread communication")
    (description "LMAX Disruptor is a software pattern and software component
for high performance inter-thread communication that avoids the need for
message queues or resource locking.")
    (license license:asl2.0)))

(define-public java-conversant-disruptor
  (package
    (name "java-conversant-disruptor")
    (version "1.2.19")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/conversant/disruptor")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0gx1dm7sfg7pa05cs4qby10gfcplai5b5lf1f7ik1a76dh3vhl0g"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "java-conversant-disruptor.jar"
       #:source-dir "src/main/java"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-resources
           (lambda _
            (copy-recursively "src/main/resources" "build/classes")))
         (add-before 'build 'remove-module
           (lambda _
             (delete-file "src/main/java/module-info.java"))))))
    (native-inputs
     (list java-junit))
    (home-page "https://github.com/conversant/disruptor")
    (synopsis "High performance intra-thread communication")
    (description "Conversant Disruptor is the highest performing intra-thread
transfer mechanism available in Java.  Conversant Disruptor is an implementation
of this type of ring buffer that has almost no overhead and that exploits a
particularly simple design.")
    (license license:asl2.0)))

(define-public java-jctools-core-1
  (package
    (name "java-jctools-core")
    (version "1.2.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/JCTools/JCTools")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "105my29nwd4djvdllmq8s3jdzbyplbkxzwmddxiiilb4yqr1pghb"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "java-jctools-core.jar"
       #:source-dir "jctools-core/src/main/java"
       #:test-dir "jctools-core/src/test"
       ;; The tests timeout on some architectures.
       #:tests? ,(not (or (target-aarch64?)
                          (%current-target-system)))))
    (native-inputs
     (list java-junit java-hamcrest-all))
    (home-page "https://github.com/JCTools/JCTools")
    (synopsis "Concurrency tools for Java")
    (description "This library implements concurrent data structures that are
not natively available in Java.")
    (license license:asl2.0)))

(define-public java-commons-bcel
  (package
    (name "java-commons-bcel")
    (version "6.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/commons/bcel/source/bcel-"
                                  version "-src.tar.gz"))
              (sha256
               (base32
                "0j3x1rxd673k07psclk8k13rqh0x0mf2yy5qiwkiw4z3afa568jy"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "bcel.jar"
       #:jdk ,icedtea-8
       #:source-dir "src/main/java"
       #:test-dir "src/test/java"
       ;; FIXME: Tests require the unpackaged jna.
       #:tests? #f))
    (home-page "https://commons.apache.org/proper/commons-bcel/")
    (synopsis "Byte code engineering library")
    (description "The Byte Code Engineering Library (Apache Commons BCEL) is
intended to give users a convenient way to analyze, create, and
manipulate (binary) Java class files.  Classes are represented by objects
which contain all the symbolic information of the given class: methods, fields
and byte code instructions, in particular.

Such objects can be read from an existing file, be transformed by a
program (e.g. a class loader at run-time) and written to a file again.  An
even more interesting application is the creation of classes from scratch at
run-time.  The @dfn{Byte Code Engineering Library} (BCEL) may be also useful
if you want to learn about the @dfn{Java Virtual Machine} (JVM) and the format
of Java @code{.class} files.")
    (license license:asl2.0)))

(define-public java-xerial-core
  (package
    (name "java-xerial-core")
    (version "2.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/xerial/xerial-java/archive/"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0d3g863i41bgalpa4xr3vm1h140l091n8iwgq5qvby5yivns9y8d"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "xerial-core.jar"
       #:source-dir "xerial-core/src/main/java"
       #:test-dir "xerial-core/src/test"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "xerial-core/src/main/resources"
                               "build/classes")
             #t)))))
    (native-inputs
     `(("junit" ,java-junit)
       ("hamcrest" ,java-hamcrest-core)))
    (home-page "https://github.com/xerial/xerial-java")
    (synopsis "Data management libraries for Java")
    (description "Xerial is a set of data management libraries for the Java
programming language.  The ultimate goal of the Xerial project is to manage
everything as database, including class objects, text format data, data
streams, etc.")
    (license license:asl2.0)))

(define-public java-byte-buddy-dep
  (package
    (name "java-byte-buddy-dep")
    (version "1.14.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/raphw/byte-buddy")
                     (commit (string-append "byte-buddy-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "03jmsnkjb9d3z9brqs8fc512hhs5b5iab3a5wbax9zi03dskgvh2"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "byte-buddy-dep.jar"
       #:source-dir "byte-buddy-dep/src/main/java"
       #:test-dir "byte-buddy-dep/src/test"
       #:tests? #f; would build java files that are incompatible with current jdk
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'remove-annotations
           (lambda _
             (with-directory-excursion "byte-buddy-dep/src/main/java/net/bytebuddy"
               (substitute* (find-files "." ".*.java")
                 (("@EqualsAndHashCode.*") "")
                 (("import lombok.EqualsAndHashCode;") "")
                 (("@SuppressFBWarnings.*") "")
                 (("import edu.umd.cs.findbugs.annotations.SuppressFBWarnings;") ""))
               (substitute* '("description/type/TypeDescription.java"
                              "dynamic/loading/ClassInjector.java")
                 (("^  *value = .*") "")
                 (("^  *justification = .*") ""))))))))
    (inputs
      (list java-asm-9 java-asm-commons-9 java-jsr305 java-native-access))
    (home-page "http://bytebuddy.net/")
    (synopsis "Runtime code generation for the Java virtual machine")
    (description "Byte Buddy is a code generation and manipulation library for
creating and modifying Java classes during the runtime of a Java application
and without the help of a compiler.")
    (license license:asl2.0)))

(define-public java-powermock-reflect
  (package
    (name "java-powermock-reflect")
    (version "2.0.9")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/powermock/powermock")
                     (commit (string-append "powermock-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "03y8szi9iwxnv431z2mn2ivc1ak30vcvfvkyrwmfq7wq93bj2c5v"))
              (patches
                (search-patches "java-powermock-fix-java-files.patch"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "java-powermock-reflect.jar"
       #:source-dir "powermock-reflect/src/main/java"
       #:test-dir "powermock-reflect/src/test"))
    (inputs
     (list java-asm-9 java-objenesis))
    (native-inputs
     (list java-assertj java-cglib java-hamcrest-core java-junit))
    (home-page "https://github.com/powermock/powermock")
    (synopsis "Mock library extension framework")
    (description "PowerMock is a framework that extends other mock libraries
such as EasyMock with more powerful capabilities.  PowerMock uses a custom
classloader and bytecode manipulation to enable mocking of static methods,
constructors, final classes and methods, private methods, removal of static
initializers and more.  By using a custom classloader no changes need to be
done to the IDE or continuous integration servers which simplifies adoption.")
    (license license:asl2.0)))

(define-public java-powermock-core
  (package
    (inherit java-powermock-reflect)
    (name "java-powermock-core")
    (arguments
     `(#:jar-name "java-powermock-core.jar"
       #:source-dir "powermock-core/src/main/java"
       #:test-dir "powermock-core/src/test"
       #:tests? #f; requires powermock-api
       #:jdk ,icedtea-8
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "powermock-core/src/main/resources"
                               "build/classes")
             #t)))))
    (inputs
     (list java-asm-9
           java-byte-buddy-dep
           java-jboss-javassist
           java-powermock-reflect))
    (native-inputs
     (list java-assertj java-mockito-1 java-junit))))

(define-public java-powermock-api-support
  (package
    (inherit java-powermock-reflect)
    (name "java-powermock-api-support")
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "java-powermock-api-support.jar"
       #:jdk ,icedtea-8
       #:source-dir "powermock-api/powermock-api-support/src/main/java"
       #:tests? #f)); no tests
    (inputs
     (list java-powermock-core java-powermock-reflect))))

(define-public java-powermock-modules-junit4-common
  (package
    (inherit java-powermock-reflect)
    (name "java-powermock-modules-junit4-common")
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "java-powermock-modules-junit4-common.jar"
       #:jdk ,icedtea-8
       #:source-dir "powermock-modules/powermock-module-junit4-common/src/main/java"
       #:test-dir "powermock-modules/powermock-module-junit4-common/src/test"))
    (inputs
     (list java-cglib
           java-easymock
           java-hamcrest-core
           java-powermock-core
           java-powermock-reflect))))

(define-public java-powermock-modules-junit4
  (package
    (inherit java-powermock-reflect)
    (name "java-powermock-modules-junit4")
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "java-powermock-modules-junit4.jar"
       #:tests? #f; require easymock 4, which introduces a loop with testng
       #:source-dir "powermock-modules/powermock-module-junit4/src/main/java"
       #:test-dir "powermock-modules/powermock-module-junit4/src/test"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'fix-junit-detection
           (lambda _
             ;; Our junit version is 4.12-SNAPSHOT
             (substitute* (find-files "powermock-modules/powermock-module-junit4"
                                      "PowerMockJUnit4MethodValidator.java")
               (("4.12") "4.12-SNAPSHOT"))
             #t)))))
    (inputs
     (list java-cglib
           java-powermock-core
           java-powermock-reflect
           java-powermock-modules-junit4-common))
    (native-inputs
     (list java-easymock
           java-hamcrest-core
           java-junit
           java-objenesis))))

(define-public java-powermock-api-easymock
  (package
    (inherit java-powermock-reflect)
    (name "java-powermock-api-easymock")
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "java-powermock-api-easymock.jar"
       #:jdk ,icedtea-8
       #:source-dir "powermock-api/powermock-api-easymock/src/main/java"
       #:tests? #f; no tests
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'fix-file
           (lambda _
             ;; FIXME: This looks wrong, but it fixes a build error.
             (with-directory-excursion "powermock-api/powermock-api-easymock"
               (substitute* "src/main/java/org/powermock/api/easymock/PowerMock.java"
                 (("classLoader instanceof MockClassLoader") "false")
                 (("\\(\\(MockClassLoader\\) classLoader\\).*;") ";")))
             #t)))))
    (inputs
     (list java-cglib
           java-easymock
           java-powermock-api-support
           java-powermock-core
           java-powermock-reflect))))

(define-public java-jboss-jms-api-spec
  (package
    (name "java-jboss-jms-api-spec")
    (version "2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/jboss/jboss-jms-api_spec/"
                                  "archive/jboss-jms-api_" version
                                  "_spec-1.0.1.Final.tar.gz"))
              (sha256
               (base32
                "07bqblw9kq2i8q92bz70fvavq5xjfkaixl8xa0m0cypjgy82rb7m"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "java-jboss-jms-api_spec.jar"
       #:jdk ,icedtea-8
       #:source-dir "."
       #:tests? #f)); no tests
    (home-page "https://github.com/jboss/jboss-jms-api_spec")
    (synopsis "Java Message Service API specification")
    (description "Java Message Service (JMS) API is used to send messages
messages between two or more clients.  It is a messaging standard that allows
application components to create, send, receive, and read messages.")
    ; either gpl2 only with GPL Classpath Exception, or cddl.
    (license (list license:gpl2 license:cddl1.0))))

(define-public java-mail
  (package
    (name "java-mail")
    (version "1.6.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/javaee/javamail/archive/"
                                  "JAVAMAIL-1_6_0.tar.gz"))
              (sha256
               (base32
                "1b4rg7fpj50ld90a71iz2m4gm3f5cnw18p3q3rbrrryjip46kx92"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "java-mail.jar"
       #:jdk ,icedtea-8
       #:source-dir "mail/src/main/java"
       #:test-dir "mail/src/test"
       #:test-exclude
       (list "**/CollectorFormatterTest.java"
             "**/CompactFormatterTest.java"
             "**/DurationFilterTest.java"
             "**/MailHandlerTest.java"
             "**/GetLocalAddressTest.java"
             ;; SSLHandshakeException: No appropriate protocol
             "**/WriteTimeoutSocketTest.java"
             ;; FIXME: both end with:
             ;; java.lang.ClassNotFoundException:
             ;; javax.mail.internet.MimeMultipartParseTest
             "**/MimeMultipartParseTest.java"
             "**/SearchTermSerializationTest.java")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'move-version.java
           (lambda _
             (copy-file "mail/src/main/resources/javax/mail/Version.java"
                        "mail/src/main/java/javax/mail/Version.java")))
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "mail/src/main/resources/META-INF"
                               "build/classes/META-INF"))))))
    (native-inputs
     (list java-junit java-hamcrest-core))
    (home-page "https://javaee.github.io/javamail/")
    (synopsis "Mail-related functionality in Java")
    (description "The JavaMail API provides a platform-independent and
protocol-independent framework to build mail and messaging applications.")
    ;; General Public License Version 2 only ("GPL") or the Common Development
    ;; and Distribution License("CDDL")
    (license (list license:cddl1.1
                   license:gpl2)))); with classpath exception

(define-public java-mapdb
  (package
    (name "java-mapdb")
    (version "1.0.9")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/jankotek/mapdb")
                    (commit (string-append "mapdb-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1slb4h282jqvk16896lcvgm65pj3v6crcp2wvjdvad7nx7d2f1kv"))))
    (build-system ant-build-system)
    (arguments
     (list #:jar-name "mapdb.jar"
           #:source-dir "src/main/java"
           #:test-dir "src/test"
           #:test-exclude (list "**/ClosedThrowsExceptionTest.java"
                                "**/ConcurrentMapInterfaceTest.java"
                                "**/EngineTest.java"
                                "**/Issue664Test.java"
                                "**/MapInterfaceTest.java")
           #:phases #~(modify-phases %standard-phases
                        (replace 'install (install-from-pom "pom.xml")))))
    (native-inputs (list java-junit))
    (home-page "https://mapdb.org/")
    (synopsis "Concurrent data structures")
    (description "MapDB provides concurrent maps, sets and queues backed by
disk storage or off-heap memory.")
    (license license:bsd-3)))

(define-public java-jeromq
  (package
    (name "java-jeromq")
    (version "0.4.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/zeromq/jeromq")
                    (commit (string-append "v" version))))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "1gxkp7lv2ahymgrqdw94ncq54bmp4m4sw5m1x9gkp7l5bxn0xsyj"))
              (patches (search-patches "java-jeromq-fix-tests.patch"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "java-jeromq.jar"
       #:source-dir "src/main/java"
       #:jdk ,icedtea-8
       #:test-exclude
       (list
         "**/Abstract*.java"
         ;; Requires network
         "**/ZBeaconTest.java"
         ;; Failures
         "**/DealerSpecTest.java"
         "**/CustomDecoderTest.java"
         "**/CustomEncoderTest.java"
         "**/ConnectRidTest.java"
         "**/ReqSpecTest.java"
         "**/PushPullSpecTest.java"
         "**/PubSubHwmTest.java"
         "**/RouterSpecTest.java"
         "**/ProxyTest.java")))
    (inputs
     (list java-jnacl))
    (native-inputs
     `(("java-hamcrest-core" ,java-hamcrest-core)
       ("junit" ,java-junit)))
    (home-page "https://zeromq.org/bindings:java")
    (synopsis "Java binding for 0MQ")
    (description "Jeromq provides the java bindings for 0MQ.")
    (license license:mpl2.0)))

(define-public java-kafka-clients
  (package
    (name "java-kafka-clients")
    (version "1.1.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/kafka/" version "/kafka-"
                                  version "-src.tgz"))
              (sha256
               (base32
                "1jn62q7z383nwhzv4ippsddf98sa1gnkszjjncj4ii3r8rzgw566"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "java-kafka-clients.jar"
       #:jdk ,icedtea-8
       #:source-dir "clients/src/main/java"
       #:test-dir "clients/src/test"
       #:test-exclude
       (list
         ;; This file does not contain a class
         "**/IntegrationTest.java"
         ;; Requires network
         "**/ClientUtilsTest.java"
         ;; This test fails on i686
         "**/SerializationTest.java"
         ;; "protocol is disabled or cipher suites are inappropriate"
         "**/SslTransportLayerTest.java"
         ;; End with errors that seem related to our powermock
         "**/KafkaProducerTest.java"
         "**/BufferPoolTest.java"
         ;; Undeterministic failure, seems to affect mostly ci
         "**/GarbageCollectedMemoryPoolTest.java")))
    (inputs
     (list java-slf4j-api java-lz4))
    (native-inputs
     (list java-bouncycastle
           java-cglib
           java-easymock
           java-hamcrest-all
           java-jboss-javassist
           java-junit
           java-objenesis
           java-powermock-api-easymock
           java-powermock-api-support
           java-powermock-core
           java-powermock-modules-junit4
           java-powermock-modules-junit4-common
           java-snappy))
    (home-page "https://kafka.apache.org")
    (synopsis "Distributed streaming platform")
    (description "Kafka is a distributed streaming platform, which means:
@itemize
@item it can publish and subscribe to streams of records;
@item it can store streams of records in a fault-tolerant way;
@item it can process streams of records as they occur.
@end itemize")
    ;; Either cddl or gpl2 only.
    (license (list license:cddl1.1; actually cddl1.1
                   license:gpl2)))); with classpath exception

(define-public java-jdom
  (package
    (name "java-jdom")
    (version "1.1.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://jdom.org/dist/binary/archive/jdom-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "07wdpm3jwwc9q38kmdw40fvbmv6jzjrkrf8m0zqs58f79a672wfl"))))
    (build-system ant-build-system)
    (arguments
     `(#:build-target "package"
       #:tests? #f; tests are run as part of the build process
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (install-jars "build")))))
    (home-page "http://jdom.org/")
    (synopsis "Access, manipulate, and output XML data")
    (description "Java-based solution for accessing, manipulating, and
outputting XML data from Java code.")
    (license license:bsd-4)))

(define-public java-geronimo-xbean-reflect
  (package
    (name "java-geronimo-xbean-reflect")
    (version "4.5")
    (source (origin
              (method svn-fetch)
              (uri (svn-reference
                     (url "https://svn.apache.org/repos/asf/geronimo/xbean/tags/xbean-4.5/")
                     (revision 1807396)))
              (file-name (string-append name "-" version))
              (sha256
               (base32
                "0zjqmsad4xk0iar23hdyvx19nxczybd2bh0i35xrafli5cmh720k"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "geronimo-xbean-reflect.jar"
       #:source-dir "xbean-reflect/src/main/java"
       #:test-dir "xbean-reflect/src/test"
       #:jdk ,icedtea-8
       #:test-exclude
       (list "**/Abstract*.java" "**/AsmParameterNameLoaderTest.java"
             "**/ObjectRecipeTest.java" "**/ParameterNameLoaderTest.java"
             "**/RecipeHelperTest.java" "**/XbeanAsmParameterNameLoaderTest.java")
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'fix-source
           (lambda _
             (let ((dir "xbean-reflect/src/main/java/org/apache/xbean/recipe/"))
               ;; org.apache.xbean.asm6 is actually repackaged java-asm
               (substitute* (string-append dir "XbeanAsmParameterNameLoader.java")
                 (("org.apache.xbean.asm5") "org.objectweb.asm"))
               #t)))
         (replace 'install (install-from-pom "xbean-reflect/pom.xml")))))
    (inputs
     `(("asm" ,java-asm)
       ("log4j" ,java-log4j-api)
       ("log4j-1.2" ,java-log4j-1.2-api)
       ("log4j-core" ,java-log4j-core)
       ("logging" ,java-commons-logging-minimal)))
    (propagated-inputs
     (list java-geronimo-parent-pom))
    (native-inputs
     `(("junit" ,java-junit)))
    (home-page "https://geronimo.apache.org/maven/xbean/3.6/xbean-reflect/")
    (synopsis "Dependency injection helper")
    (description "Xbean-reflect provides very flexible ways to create objects
and graphs of objects for dependency injection frameworks")
    (license license:asl2.0)))

(define java-geronimo-genesis-2.1
  (package
    (name "java-geronimo-genesis")
    (version "2.1")
    (source (origin
              (method svn-fetch)
              (uri (svn-reference
                     (url (string-append "https://svn.apache.org/repos/asf/"
                                         "geronimo/genesis/tags/genesis-"
                                         version))
                     (revision 1807396)))
              (file-name (string-append name "-" version "-source"))
              (sha256
               (base32
                "1mky4zyl2xsqlgrkairaj5971byvhwk2z9bq8snsgvlr11ydc0zf"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (replace 'install
           (install-pom-file "pom.xml"))
         (add-after 'install 'install-enforcer-rules
           (install-pom-file "genesis-enforcer-rules/pom.xml"))
         (add-after 'install 'install-flava
           (install-pom-file "genesis-default-flava/pom.xml"))
         (add-after 'install 'install-packaging
           (install-pom-file "genesis-packaging/pom.xml"))
         (add-after 'install-flava 'install-flava-java4
           (install-pom-file "genesis-default-flava/genesis-java1.4-flava/pom.xml"))
         (add-after 'install-flava 'install-flava-java5
           (install-pom-file "genesis-default-flava/genesis-java5-flava/pom.xml"))
         (add-after 'install-flava 'install-flava-java6
           (install-pom-file "genesis-default-flava/genesis-java6-flava/pom.xml")))))
    (propagated-inputs
     `(("apache-parent-pom" ,apache-parent-pom-13)))
    (home-page "https://geronimo.apache.org")
    (synopsis "Collection of maven POM files for the Geronimo project")
    (description "Apache Geronimo is a server runtime.  This package contains
only pom files used by other components in the Geronimo project.")
    (license license:asl2.0)))

(define java-geronimo-parent-pom
  (package
    (inherit java-geronimo-xbean-reflect)
    (name "java-geronimo-parent-pom")
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (replace 'install
           (install-pom-file "pom.xml")))))
    (propagated-inputs
     `(("java-geronimo-genesis" ,java-geronimo-genesis-2.1)))))

(define-public java-geronimo-xbean-bundleutils
  (package
    (inherit java-geronimo-xbean-reflect)
    (name "java-geronimo-xbean-bundleutils")
    (arguments
     `(#:jar-name "geronimo-xbean-bundleutils.jar"
       #:source-dir "xbean-bundleutils/src/main/java"
       #:test-dir "xbean-bundleutils/src/test"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'fix-java
           (lambda _
             ;; We use a more recent version of osgi, so this file requires
             ;; more interface method implementations.
             (substitute* "xbean-bundleutils/src/main/java/org/apache/xbean/osgi/bundle/util/DelegatingBundleContext.java"
               (("import org.osgi.framework.ServiceRegistration;")
                "import org.osgi.framework.ServiceRegistration;
import org.osgi.framework.ServiceFactory;
import java.util.Collection;
import org.osgi.framework.ServiceObjects;")
               (("public Bundle getBundle\\(\\)")
                "@Override
public <S> ServiceObjects<S> getServiceObjects(ServiceReference<S> reference) {
 throw new UnsupportedOperationException();
}
@Override
public <S> ServiceRegistration<S> registerService(Class<S> clazz,
        ServiceFactory<S> factory, Dictionary<String, ?> properties) {
 throw new UnsupportedOperationException();
}
public Bundle getBundle()"))
             #t)))))
    (inputs
     `(("java-slf4j" ,java-slf4j-api)
       ("java-asm" ,java-asm)
       ("java-osgi-framework" ,java-osgi-framework)
       ("java-eclipse-osgi" ,java-eclipse-osgi)
       ("java-osgi-service-packageadmin" ,java-osgi-service-packageadmin)))))

(define-public java-geronimo-xbean-asm-util
  (package
    (inherit java-geronimo-xbean-reflect)
    (name "java-geronimo-xbean-asm-util")
    (arguments
     `(#:jar-name "geronimo-xbean-asm-util.jar"
       #:source-dir "xbean-asm-util/src/main/java"
       #:tests? #f)); no tests
    (inputs
     (list java-asm))
    (native-inputs '())))

(define-public java-geronimo-xbean-finder
  (package
    (inherit java-geronimo-xbean-reflect)
    (name "java-geronimo-xbean-finder")
    (arguments
     `(#:jar-name "geronimo-xbean-finder.jar"
       #:source-dir "xbean-finder/src/main/java"
       #:test-dir "xbean-finder/src/test"))
    (inputs
     (list java-slf4j-api
           java-asm
           java-geronimo-xbean-bundleutils
           java-geronimo-xbean-asm-util
           java-osgi-service-packageadmin
           java-osgi-framework))
    (native-inputs
     (list java-junit java-hamcrest-core))))

(define-public java-gson
  (package
    (name "java-gson")
    (version "2.8.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/google/gson/archive/"
                                  "gson-parent-" version ".tar.gz"))
              (sha256
               (base32
                "1j4qnp7v046q0k48c4kyf69sxaasx2h949d3cqwsm3kzxms3x0f9"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "gson.jar"
       #:source-dir "gson/src/main/java"
       #:test-dir "gson/src/test"))
    (native-inputs
     (list java-junit java-hamcrest-core))
    (home-page "https://github.com/google/gson")
    (synopsis "Java serialization/deserialization library from/to JSON")
    (description "Gson is a Java library that can be used to convert Java
Objects into their JSON representation.  It can also be used to convert a JSON
string to an equivalent Java object.  Gson can work with arbitrary Java objects
including pre-existing objects that you do not have source-code of.")
    (license license:asl2.0)))

;; This requires a different Java version than 2.8.2 above
(define-public java-gson-2.8.6
  (package
    (inherit java-gson)
    (name "java-gson")
    (version "2.8.6")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/google/gson")
                    (commit (string-append "gson-parent-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0kk5p3vichdb0ph1lzknrcpbklgnmq455mngmjpxvvj29p3rgpk3"))))
    (arguments
     `(#:jar-name "gson.jar"
       #:jdk ,openjdk11
       #:source-dir "gson/src/main/java"
       #:test-dir "gson/src/test"
       #:phases
       (modify-phases %standard-phases
         ;; avoid Maven dependency
         (add-before 'build 'fill-template
           (lambda _
             (with-directory-excursion "gson/src/main"
               (copy-file "java-templates/com/google/gson/internal/GsonBuildConfig.java"
                          "java/com/google/gson/internal/GsonBuildConfig.java")
               (substitute* "java/com/google/gson/internal/GsonBuildConfig.java"
                 (("\\$\\{project.version\\}") ,version))))))))))

(define-public java-hawtjni
  (package
    (name "java-hawtjni")
    (version "1.15")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/fusesource/hawtjni/archive/"
                                  "hawtjni-project-" version ".tar.gz"))
              (sha256
               (base32
                "1bqfd732rmh6svyx17fpw9175gc9gzkcbyps2yyrf50c3zzjas6g"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "hawtjni.jar"
       #:source-dir "hawtjni-generator/src/main/java:hawtjni-runtime/src/main/java"
       #:tests? #f; no tests
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'build-native
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((include (string-append "-I" (assoc-ref inputs "jdk") "/include/linux")))
               (with-directory-excursion "hawtjni-generator/src/main/resources/"
                 (invoke "gcc" "-c" "hawtjni.c" "-o" "hawtjni.o"
                         "-fPIC" "-O2" include)
                 (invoke "gcc" "-c" "hawtjni-callback.c" "-o" "hawtjni-callback.o"
                         "-fPIC" "-O2" include)
                 (invoke "gcc" "-o" "libhawtjni.so" "-shared"
                         "hawtjni.o" "hawtjni-callback.o")))
             #t))
         (add-after 'install 'install-native
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (lib (string-append out "/lib"))
                    (inc (string-append out "/include")))
               (with-directory-excursion "hawtjni-generator/src/main/resources/"
                 (install-file "libhawtjni.so" lib)
                 (install-file "hawtjni.h" inc)))
             #t))
         (add-before 'install 'install-parent
           (install-pom-file "pom.xml"))
         (replace 'install
           (install-from-pom "hawtjni-runtime/pom.xml")))))
    (inputs
     (list java-commons-cli java-asm java-geronimo-xbean-finder))
    (home-page "https://fusesource.github.io/hawtjni/")
    (synopsis "JNI code generator")
    (description "HawtJNI is a code generator that produces the JNI code needed
to implement Java native methods.  It is based on the jnigen code generator
that is part of the SWT Tools project.")
    (license license:asl2.0)))

(define-public java-jansi-native
  (package
    (name "java-jansi-native")
    (version "1.7")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/fusesource/jansi-native/"
                                  "archive/jansi-native-" version ".tar.gz"))
              (sha256
               (base32
                "0j2ydlgxbzbgshqkwghbxxxnbnx1mmjgd6k5fw6xfvxw1z956yqf"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "jansi-native.jar"
       #:source-dir "src/main/java"
       #:tests? #f; no tests
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'build-native
           (lambda* (#:key inputs #:allow-other-keys)
             ;; there are more required files for windows in windows/
             (with-directory-excursion "src/main/native-package/src"
               (substitute* "jansi_ttyname.c"
                 (("#include \"jansi_.*") ""))
               (invoke "gcc" "-c" "jansi_ttyname.c" "-o" "jansi_ttyname.o"
                       (string-append "-I" (assoc-ref inputs "java-hawtjni")
                                      "/include")
                       (string-append "-I" (assoc-ref inputs "jdk")
                                      "/include/linux")
                       "-fPIC" "-O2")
               (invoke "gcc" "-o" "libjansi.so" "-shared" "jansi_ttyname.o"))))
         (add-before 'build 'install-native
           (lambda _
             (let ((dir (string-append "build/classes/META-INF/native/"
                                       ,(match (%current-system)
                                          ((or "i686-linux" "armhf-linux")
                                           "linux32")
                                          ((or "x86_64-linux" "aarch64-linux"
                                               "mips64el-linux")
                                           "linux64")
                                          (_ "unknown-kernel")))))
               (install-file "src/main/native-package/src/libjansi.so" dir))
             #t))
         (add-after 'install 'install-native
           (lambda* (#:key outputs #:allow-other-keys)
             (install-file "src/main/native-package/src/jansi.h"
                           (string-append (assoc-ref outputs "out") "/include"))
             #t))
         (add-before 'install 'fix-pom
           (lambda _
             ;; pom contains variables to complete name, but we don't support that
             (substitute* "pom.xml"
               (("\\$\\{platform\\}") "native"))
             #t))
         (replace 'install
           (install-from-pom "pom.xml")))))
    (propagated-inputs
     (list java-hawtjni))
    (home-page "https://fusesource.github.io/jansi/")
    (synopsis "Native library for jansi")
    (description "This package provides the native library for jansi, a small
Java library that allows you to use ANSI escape sequences to format your
console output.")
    (license license:asl2.0)))

(define-public java-jansi
  (package
    (name "java-jansi")
    (version "2.4.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/fusesource/jansi")
                     (commit (string-append "jansi-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1s6fva06990798b5fyxqzr30zwyj1byq5wrm54j2larcydaryggf"))
              (modules '((guix build utils)))
              (snippet
                ;; contains pre-compiled libraries
                '(delete-file-recursively
                   "src/main/resources/org/fusesource/jansi/internal"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "jansi.jar"
       #:source-dir "src/main/java"
       #:test-dir "src/test"
       #:tests? #f; require junit 3
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'build-native
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "src/main/native"
               (for-each
                 (lambda (cfile)
                   (let ((cfile (basename cfile))
                         (ofile (string-append (basename cfile ".c") ".o")))
                     (invoke ,(cc-for-target) "-c" cfile "-o" ofile
                             (string-append "-I" (assoc-ref inputs "jdk")
                                            "/include/linux")
                             "-fPIC" "-O2")))
                 (find-files "." "\\.c$"))
               (apply invoke ,(cc-for-target) "-o" "libjansi.so" "-shared"
                      (find-files "." "\\.o$")))))
         (add-before 'build 'install-native
           (lambda _
             (let ((dir (string-append "build/classes/org/fusesource/"
                                       "jansi/internal/native/"
                                       ,(match (or (%current-target-system) (%current-system))
                                          ("i686-linux" "Linux/x86")
                                          ("x86_64-linux" "Linux/x86_64")
                                          ("armhf-linux" "Linux/armv7")
                                          ("aarch64-linux" "Linux/arm64")
                                          ("mips64el-linux" "Linux/mips64")
                                          (_ "unknown-kernel")))))
               (install-file "src/main/native/libjansi.so" dir))))
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "src/main/resources" "build/classes")))
         (replace 'install
           (install-from-pom "pom.xml")))))
    (home-page "https://fusesource.github.io/jansi/")
    (synopsis "Portable ANSI escape sequences")
    (description "Jansi is a Java library that allows you to use ANSI escape
sequences to format your console output which works on every platform.")
    (license license:asl2.0)))

(define-public java-jansi-1
  (package
    (inherit java-jansi)
    (version "1.16")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/fusesource/jansi")
                     (commit (string-append "jansi-project-" version))))
              (file-name (git-file-name "jansi" version))
              (sha256
               (base32
                "0ikk0x352gh30b42qn1jd89xwsjj0mavrc5kms7fss15bd8vsayx"))))
    (arguments
     `(#:jar-name "jansi.jar"
       #:source-dir "jansi/src/main/java"
       #:test-dir "jansi/src/test"
       #:phases
       (modify-phases %standard-phases
         (add-after 'check 'clear-term
           (lambda _
             (invoke "echo" "-e" "\\e[0m")))
         (add-before 'install 'install-parent
           (install-pom-file "pom.xml"))
         (add-before 'install 'fix-pom
           (lambda _
             ;; pom adds jansi native versions for different platforms, but we
             ;; only need one, so use native instead
             (substitute* "jansi/pom.xml"
               (("windows32") "native")
               (("windows64") "native")
               (("osx") "native")
               (("linux32") "native")
               (("linux64") "native")
               (("freebsd32") "native")
               (("freebsd64") "native"))
             #t))
         (replace 'install
           (install-from-pom "jansi/pom.xml")))))
    (propagated-inputs
     (list java-jansi-native))
    (native-inputs
     (list java-junit java-hamcrest-core))))

(define-public java-jboss-el-api-spec
  (package
    (name "java-jboss-el-api-spec")
    (version "3.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/jboss/jboss-el-api_spec/"
                                  "archive/jboss-el-api_" version
                                  "_spec-1.0.7.Final.tar.gz"))
              (sha256
               (base32
                "1j45ljxalwlibxl7g7iv952sjxkw275m8vyxxij8l6wdd5pf0pdh"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "java-jboss-el-api_spec.jar"
       #:phases
       (modify-phases %standard-phases
         ;; the origin of javax.el:javax.el-api is unknown, so we use this package
         ;; instead, which implements the same thing.  We override the pom file
         ;; to "rename" the package so it can be found by maven.
         (add-before 'install 'override-pom
           (generate-pom.xml "pom.xml" "javax.el" "javax.el-api" "3.0"
                             #:name "el-api"))
         (replace 'install
           (install-from-pom "pom.xml")))))
    (inputs
     (list java-junit))
    (home-page "https://github.com/jboss/jboss-el-api_spec")
    (synopsis "JSR-341 expression language 3.0 API")
    (description "This package contains an implementation of the JSR-341
specification for the expression language 3.0.  It implements an expression
language inspired by ECMAScript and XPath.  This language is used with
JavaServer Pages (JSP).")
    ;; Either GPL2 only or CDDL.
    (license (list license:gpl2 license:cddl1.1))))

(define-public java-jboss-interceptors-api-spec
  (package
    (name "java-jboss-interceptors-api-spec")
    (version "1.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/jboss/jboss-interceptors-api_spec/"
                                  "archive/jboss-interceptors-api_" version
                                  "_spec-1.0.0.Final.tar.gz"))
              (sha256
               (base32
                "0wv8x0jp9a5qxlrgkhb5jdk2gr6vi87b4j4kjb8ryxiy9gn8g51z"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "java-jboss-interceptors-api_spec.jar"
       #:jdk ,icedtea-8
       #:source-dir "."
       #:tests? #f; no tests
       #:phases
       (modify-phases %standard-phases
         ;; the origin of javax.interceptor:javax.interceptor-api is unknown,
         ;; so we use this package instead, which implements the same thing.
         ;; We override the pom file to "rename" the package so it can be found
         ;; by maven.
         (add-before 'install 'override-pom
           (generate-pom.xml "pom.xml" "javax.interceptor" "javax.interceptor-api"
                             "3.0" #:name "interceptor-api"))
         (replace 'install
           (install-from-pom "pom.xml")))))
    (home-page "https://github.com/jboss/jboss-interceptors-api_spec")
    (synopsis "Interceptors 1.2 API classes from JSR 318")
    (description "Java-jboss-interceptors-api-spec implements the Interceptors
API.  Interceptors are used to interpose on business method invocations and
specific events.")
    ;; Either GPL2 only or CDDL.
    (license (list license:gpl2 license:cddl1.1))))

(define-public java-cdi-api
  (package
    (name "java-cdi-api")
    (version "2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/cdi-spec/cdi/archive/"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1iv8b8bp07c5kmqic14jsr868vycjv4qv02lf3pkgp9z21mnfg5y"))))
    (build-system ant-build-system)
    (arguments
     `(#:source-dir "api/src/main/java"
       #:jar-name "java-cdi-api.jar"
       #:test-dir "api/src/test"
       #:jdk ,icedtea-8
       #:tests? #f; Tests fail because we don't have a CDI provider yet
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (install-from-pom "api/pom.xml")))))
    (propagated-inputs
     (list java-javax-inject java-jboss-el-api-spec
           java-jboss-interceptors-api-spec java-weld-parent-pom))
    (native-inputs
     (list java-testng java-hamcrest-core))
    (home-page "https://cdi-spec.org/")
    (synopsis "Contexts and Dependency Injection APIs")
    (description "Java-cdi-api contains the required APIs for Contexts and
Dependency Injection (CDI).")
    (license license:asl2.0)))

(define-public java-joda-convert
  (package
    (name "java-joda-convert")
    (version "1.9.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/JodaOrg/joda-convert/archive/v"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0vp346xz7dh9br4q7xazhc7hvzf76a6hf95fki9bg67q5jr0kjh7"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name (string-append ,name "-" ,version ".jar")
       #:source-dir "src/main/java"
       #:test-include (list "**/Test*.java")
       ;; Contains only interfaces and base classes (no test)
       #:test-exclude (list "**/test*/**.java")))
    (inputs
     (list java-guava))
    (native-inputs
     (list java-junit java-hamcrest-core))
    (home-page "https://www.joda.org/joda-convert/")
    (synopsis "Conversion between Objects and Strings")
    (description "Joda-Convert provides a small set of classes to aid
conversion between Objects and Strings.  It is not intended to tackle the
wider problem of Object to Object transformation.")
    (license license:asl2.0)))

(define-public java-joda-time
  (package
    (name "java-joda-time")
    (version "2.9.9")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/JodaOrg/joda-time/archive/v"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1i9x91mi7yg2pasl0k3912f1pg46n37sps6rdb0v1gs8hj9ppwc1"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "java-joda-time.jar"
       #:source-dir "src/main/java"
       #:test-include (list "**/Test*.java")
       ;; There is no runnable test in these files
       #:test-exclude (list "**/Test*Chronology.java" "**/Test*Field.java")
       #:phases
       (modify-phases %standard-phases
         (add-after 'build 'build-resources
           (lambda _
             (mkdir-p "build/classes/org/joda/time/tz/data")
             (mkdir-p "build/classes/org/joda/time/format")
             ;; This will produce the following exception:
             ;; java.io.IOException: Resource not found: "org/joda/time/tz/data/ZoneInfoMap"
             ;; which is normal, because it doesn't exist yet. It still generates
             ;; the same file as in the binary one can find on maven.
             (invoke "java" "-cp"
                     (string-append "build/classes:" (getenv "CLASSPATH"))
                     "org.joda.time.tz.ZoneInfoCompiler"
                     "-src" "src/main/java/org/joda/time/tz/src"
                     "-dst" "build/classes/org/joda/time/tz/data"
                     "africa" "antarctica" "asia" "australasia"
                     "europe" "northamerica" "southamerica"
                     "pacificnew" "etcetera" "backward" "systemv")
             (for-each (lambda (f)
                         (copy-file f (string-append
                                        "build/classes/org/joda/time/format/"
                                        (basename f))))
               (find-files "src/main/java/org/joda/time/format" ".*.properties"))
             #t))
         (add-before 'install 'regenerate-jar
           (lambda _
             ;; We need to regenerate the jar file to add generated data.
             (delete-file "build/jar/java-joda-time.jar")
             (invoke "ant" "jar")))
         (add-before 'check 'copy-test-resources
           (lambda _
             (mkdir-p "build/test-classes/org/joda/time/tz/data")
             (copy-file "src/test/resources/tzdata/ZoneInfoMap"
                        "build/test-classes/org/joda/time/tz/data/ZoneInfoMap")
             (copy-recursively "src/test/resources" "build/test-classes")
             #t)))))
    (inputs
     (list java-joda-convert))
    (native-inputs
     (list java-junit java-hamcrest-core tzdata))
    (home-page "https://www.joda.org/joda-time/")
    (synopsis "Replacement for the Java date and time classes")
    (description "Joda-Time is a replacement for the Java date and time
classes prior to Java SE 8.")
    (license license:asl2.0)))

(define-public java-xerces
  (package
    (name "java-xerces")
    (version "2.12.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://apache/xerces/j/source/"
                           "Xerces-J-src." version ".tar.gz"))
       (sha256
        (base32 "1s2fnfx5flmhs3q30bxdsa6vs52m6vbnqd3m4cc5r4wfr3afplbd"))
       (patches (search-patches
                 "java-xerces-xjavac_taskdef.patch"
                 "java-xerces-build_dont_unzip.patch"
                 "java-xerces-bootclasspath.patch"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f;; Test files are not present
       #:test-target "test"
       #:jdk ,icedtea-8
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'create-build.properties
          (lambda* (#:key inputs #:allow-other-keys)
            (let ((jaxp (assoc-ref inputs "java-jaxp"))
                  (resolver (assoc-ref inputs "java-apache-xml-commons-resolver")))
              (with-output-to-file "build.properties"
                (lambda _
                  (format #t
                   "jar.jaxp = ~a/share/java/jaxp.jar~@
                   jar.apis-ext = ~a/share/java/jaxp.jar~@
                   jar.resolver = ~a/share/java/xml-resolver.jar~%"
                   jaxp jaxp resolver)))
              ;; Make xerces use our version of jaxp in tests
              (substitute* "build.xml"
                (("xml-apis.jar")
                 (string-append jaxp "/share/java/jaxp.jar"))
                (("\\$\\{tools.dir\\}/\\$\\{jar.apis\\}")
                 "${jar.apis}")))
            #t))
         (replace 'install (install-jars "build")))))
    (inputs
     (list java-apache-xml-commons-resolver java-jaxp))
    (home-page "https://xerces.apache.org/xerces2-j/")
    (synopsis "Validating XML parser for Java with DOM level 3 support")
    (description "The Xerces2 Java parser is the reference implementation of
XNI, the Xerces Native Interface, and also a fully conforming XML Schema
processor.

Xerces2-J supports the following standards and APIs:

@itemize
@item eXtensible Markup Language (XML) 1.0 Second Edition Recommendation
@item Namespaces in XML Recommendation
@item Document Object Model (DOM) Level 2 Core, Events, and Traversal and
      Range Recommendations
@item Simple API for XML (SAX) 2.0.1 Core and Extension
@item Java APIs for XML Processing (JAXP) 1.2.01
@item XML Schema 1.0 Structures and Datatypes Recommendations
@item Experimental implementation of the Document Object Model (DOM) Level 3
      Core and Load/Save Working Drafts
@item Provides a partial implementation of the XML Inclusions (XInclude) W3C
      Candidate Recommendation
@end itemize

Xerces is now able to parse documents written according to the XML 1.1
Candidate Recommendation, except that it does not yet provide an option to
enable normalization checking as described in section 2.13 of this
specification.  It also handles namespaces according to the XML Namespaces 1.1
Candidate Recommendation, and will correctly serialize XML 1.1 documents if
the DOM level 3 load/save API's are in use.")
    (license license:asl2.0)))

(define-public java-jakarta-regexp
  (package
    (name "java-jakarta-regexp")
    (version "1.5")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
              "https://archive.apache.org/dist/jakarta/regexp/jakarta-regexp-"
              version ".tar.gz"))
        (sha256
         (base32
          "0zg9rmyif48dck0cv6ynpxv23mmcsx265am1fnnxss7brgw0ms3r"))))
    (build-system ant-build-system)
    (arguments
     `(#:test-target "test"
       #:phases
       (modify-phases %standard-phases
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (out-share (string-append out "/share/java")))
                (mkdir-p out-share)
                (for-each (lambda (name)
                            (install-file name out-share))
                          (find-files "build" "^jakarta-regexp-.*\\.jar$"))
                #t))))))
    (home-page "https://attic.apache.org/projects/jakarta-regexp.html")
    (synopsis "Regular expression parser generator for Java")
    (description "@code{jakarta-regexp} is an old regular expression parser
generator for Java.")
    (license license:asl2.0)))

(define-public java-jline
  (package
    (name "java-jline")
    (version "1.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/jline/jline1/archive/jline-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0bi3p6vrh7a6v0fbpb6rx9plpmx5zk3lr352xzdbz2jcxg499wir"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "jline.jar"
       #:source-dir "src/main/java"
       #:test-dir "src/test"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "src/main/resources" "build/classes")
             #t)))))
    (native-inputs
     (list java-junit))
    (home-page "https://jline.github.io")
    (synopsis "Console input handling library")
    (description "JLine is a Java library for handling console input.  It is
similar in functionality to BSD editline and GNU readline but with additional
features that bring it on par with the Z shell line editor.")
    (license license:bsd-3)))

(define-public java-jline-2
  (package
    (inherit java-jline)
    (version "2.14.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/jline/jline2/archive/jline-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1c6qa26mf0viw8hg4jnv72s7i1qb1gh1l8rrzcdvqhqhx82rkdlf"))))
    (arguments
     `(#:jdk ,icedtea-8
       ,@(package-arguments java-jline)))
    (inputs
     (list java-jansi-1 java-jansi-native))
    (native-inputs
     (list java-powermock-modules-junit4
           java-powermock-modules-junit4-common
           java-powermock-api-easymock
           java-powermock-api-support
           java-powermock-core
           java-powermock-reflect
           java-easymock
           java-jboss-javassist
           java-objenesis
           java-hamcrest-core
           java-cglib
           java-junit
           java-hawtjni))))

(define-public java-jline-terminal
  (package
    (name "java-jline-terminal")
    (version "3.14.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/jline/jline3")
                    (commit (string-append "jline-parent-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0ilhk9ljp0pivl1rn0bb06syshc67p6imcjhrg6vr7kv15p3w4lr"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "jline-terminal.jar"
       #:jdk ,openjdk11
       #:tests? #f; TODO: tests fail on *.caps resource lookups
       #:source-dir "terminal/src/main/java"
       #:test-dir "terminal/src/test"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-build-file
           (lambda _
             ;; Conflicts with build directory generated by ant-build-system.
             (delete-file "build")))
         (add-after 'unpack 'patch-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "terminal/src/main/java/org/jline/utils/OSUtils.java"
               (("= \"infocmp\"")
                (string-append "= \"" (assoc-ref inputs "ncurses")
                               "/bin/infocmp\""))
               (("= \"(s?tty)\"" _ cmd)
                (string-append "= \"" (assoc-ref inputs "coreutils")
                               "/bin/" cmd "\"")))))
         ;; Resources are not added to the JAR by ant-build-system.
         (add-after 'build 'add-resources
           (lambda* (#:key jar-name source-dir #:allow-other-keys)
             (let ((build (string-append (getcwd) "/build")))
               (with-directory-excursion
                   (string-append source-dir "/../resources")
                 (apply invoke "jar" "-uvf"
                        (string-append build "/jar/" jar-name)
                        (find-files ".")))))))))
    (inputs
     (list ncurses)); infocmp
    (home-page "https://github.com/jline/jline3")
    (synopsis "Java JLine Terminal API and implementations")
    (description "JLine is a Java library for handling console input.  It is
similar in functionality to BSD editline and GNU readline but with additional
features that bring it in par with ZSH line editor.  People familiar with the
readline/editline capabilities for modern shells (such as bash and tcsh) will
find most of the command editing features of JLine to be familiar.

This package includes the @var{Terminal} API and implementations.")
    (license license:bsd-3)))

(define-public java-jline-reader
  (package
    (name "java-jline-reader")
    (version "3.14.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/jline/jline3")
                    (commit (string-append "jline-parent-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0ilhk9ljp0pivl1rn0bb06syshc67p6imcjhrg6vr7kv15p3w4lr"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "jline-reader.jar"
       #:jdk ,openjdk11
       #:source-dir "reader/src/main/java"
       #:test-dir "reader/src/test"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'remove-build-file
           (lambda _
             ;; conflicts with build directory generated by ant-build-system
             (delete-file "build"))))))
    (native-inputs
     (list java-junit java-easymock))
    (inputs
     (list java-jline-terminal))
    (home-page "https://github.com/jline/jline3")
    (synopsis "Java JLine line reader")
    (description "JLine is a Java library for handling console input.  It is
similar in functionality to BSD editline and GNU readline but with additional
features that bring it in par with ZSH line editor.  People familiar with the
readline/editline capabilities for modern shells (such as bash and tcsh) will
find most of the command editing features of JLine to be familiar.

This package includes the line reader.")
    (license license:bsd-3)))

(define-public java-xmlunit
  (package
    (name "java-xmlunit")
    (version "2.5.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/xmlunit/xmlunit/archive/v"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "035rivlnmwhfqj0fzviciv0bkh1h95ps1iwnh2kjcvdbk5nccm4z"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "java-xmlunit.jar"
       #:source-dir "xmlunit-core/src/main/java"
       #:test-dir "xmlunit-core/src/test"
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'copy-test-resources
           (lambda* (#:key inputs #:allow-other-keys)
             (copy-recursively (assoc-ref inputs "resources") "../test-resources")
             #t)))))
    (native-inputs
     `(("java-junit" ,java-junit)
       ("java-mockito-1" ,java-mockito-1)
       ("java-hamcrest-all" ,java-hamcrest-all)
       ("java-objenesis" ,java-objenesis)
       ("java-cglib" ,java-cglib)
       ("resources"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                  (url "https://github.com/xmlunit/test-resources")
                  (commit "a590d2ae865c3e0455691d76ba8eefccc2215aec")))
           (file-name "java-xmlunit-test-resources")
           (sha256
            (base32
             "0r0glj37pg5l868yjz78gckr91cs8fysxxbp9p328dssssi91agr"))))))
    (home-page "https://www.xmlunit.org/")
    (synopsis "XML output testing")
    (description "XMLUnit provides you with the tools to verify the XML you
emit is the one you want to create.  It provides helpers to validate against
an XML Schema, assert the values of XPath queries or compare XML documents
against expected outcomes.")
    (license license:asl2.0)))

(define-public java-xmlunit-legacy
  (package
    (inherit java-xmlunit)
    (name "java-xmlunit-legacy")
    (arguments
     `(#:jar-name "java-xmlunit-legacy.jar"
       #:source-dir "xmlunit-legacy/src/main/java"
       #:test-dir "xmlunit-legacy/src/test"))
    (inputs
     (list java-xmlunit java-junit))
    (native-inputs
     `(("java-mockito-1" ,java-mockito-1)))))

(define-public java-xmlunit-matchers
  (package
    (inherit java-xmlunit)
    (name "java-xmlunit-matchers")
    (arguments
     `(#:jar-name "java-xmlunit-matchers.jar"
       #:source-dir "xmlunit-matchers/src/main/java"
       #:test-dir "xmlunit-matchers/src/test"
       #:test-exclude
       ;; Cannot open xsd for http://www.xmlunit.org/test-support/Book.xsd
       (list "**/ValidationMatcherTest.java")
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-test-class
           (lambda _
             (copy-file "xmlunit-core/src/test/java/org/xmlunit/TestResources.java"
                        "xmlunit-matchers/src/test/java/org/xmlunit/TestResources.java")
             #t))
         (add-before 'build 'fix-test-resources-path
           (lambda _
             (substitute* (find-files "xmlunit-matchers/src/test" ".*.java")
               (("../test-resources") "test-resources"))
             #t))
         (add-before 'check 'copy-test-resources
           (lambda* (#:key inputs #:allow-other-keys)
             (copy-recursively (assoc-ref inputs "resources") "test-resources")
             #t)))))
    (inputs
     (list java-xmlunit java-junit))))

(define-public java-openchart2
  (package
    (name "java-openchart2")
    (version "1.4.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://download.approximatrix.com/openchart2/"
                                  "openchart2-" version ".source.zip"))
              (sha256
               (base32
                "1xq96zm5r02n1blja0072jmmsifmxc40lbyfbnmcnr6mw42frh4g"))))
    (build-system ant-build-system)
    (arguments
     `(#:test-target "test"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-junit-errors
           (lambda _
             (with-directory-excursion "unittest/src/com/approximatrix/charting/"
               (substitute* '("coordsystem/ticklocator/NumericXTickLocatorTest.java"
                              "coordsystem/ticklocator/NumericYTickLocatorTest.java"
                              "coordsystem/ticklocator/ObjectXTickLocatorTest.java"
                              "model/DefaultChartDataModelConstraintsTest.java"
                              "model/MultiScatterDataModelConstraintsTest.java"
                              "model/threedimensional/DotPlotDataModelConstraintsTest.java")
                 (("(assertEquals[^;]+);" before _)
                  (string-append (string-drop-right before 2) ", 1E-6);"))))
             #t))
         (replace 'install (install-jars ".")))))
    (native-inputs
     (list unzip java-junit java-hamcrest-core))
    (home-page "https://approximatrix.com/products/openchart2/")
    (synopsis "Simple plotting for Java")
    (description "Openchart2 provides a simple, yet powerful, interface for
Java programmers to create two-dimensional charts and plots.  The library
features an assortment of graph styles, including advanced scatter plots, bar
graphs, and pie charts.")
    (license license:lgpl2.1+)))

(define-public java-commons-httpclient
  (package
    (name "java-commons-httpclient")
    (version "3.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/httpcomponents/"
                                  "commons-httpclient/source/commons-httpclient-"
                                  version "-src.tar.gz"))
              (sha256
               (base32
                "1wlpn3cfy3d4inxy6g7wxcsa8p7sshn6aldk9y4ia3lb879rd97r"))))
    (build-system ant-build-system)
    (arguments
     `(#:build-target "compile"
       #:test-target "test"
       #:tests? #f; requires junit-textui (junit 3)
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'fix-accent
           (lambda _
             (for-each (lambda (file)
                         (with-fluids ((%default-port-encoding "ISO-8859-1"))
                          (substitute* file
                            (("\\* @author Ortwin .*") "* @author Ortwin Glueck\n"))))
               '("src/java/org/apache/commons/httpclient/HttpContentTooLargeException.java"
                 "src/examples/TrivialApp.java" "src/examples/ClientApp.java"
                 "src/test/org/apache/commons/httpclient/TestHttps.java"
                 "src/test/org/apache/commons/httpclient/TestURIUtil2.java"))
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (invoke "ant" "dist"
                     (string-append "-Ddist.home=" (assoc-ref outputs "out")
                                    "/share/java"))
             #t)))))
    (propagated-inputs
     `(("java-commons-logging" ,java-commons-logging-minimal)
       ("java-commons-codec" ,java-commons-codec)))
    (home-page "https://hc.apache.org")
    (synopsis "HTTP/1.1 compliant HTTP agent implementation")
    (description "This package contains an HTTP/1.1 compliant HTTP agent
implementation.  It also provides reusable components for client-side
authentication, HTTP state management, and HTTP connection management.")
    (license license:asl2.0)))

(define-public java-commons-vfs
  (package
    (name "java-commons-vfs")
    (version "2.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/commons/vfs/source/"
                                  "commons-vfs2-distribution-" version "-src.tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1cnq1iaghbp4cslpnvwbp83i5v234x87irssqynhwpfgw7caf1s3"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (for-each delete-file
                            (find-files "." "\\.jar$"))
                  #t))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "commons-vfs.jar"
       #:source-dir "commons-vfs2/src/main/java"
       #:test-dir "commons-vfs2/src/test"
       ; FIXME: tests depend on many things: apache sshd, hadoop, ftpserver, ...
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'remove-hadoop-and-webdav
           ; Remove these files as they are not required and depend on difficult
           ; packages.
           (lambda _
             (for-each delete-file-recursively
               '("commons-vfs2/src/main/java/org/apache/commons/vfs2/provider/webdav"
                 "commons-vfs2/src/main/java/org/apache/commons/vfs2/provider/hdfs"))
             #t)))))
    (inputs
     (list java-commons-collections4
           java-commons-compress
           java-commons-httpclient
           java-commons-logging-minimal
           java-commons-net
           java-jsch))
    (home-page "https://commons.apache.org/proper/commons-vfs/")
    (synopsis "Java file system library")
    (description "Commons VFS provides a single API for accessing various
different file systems.  It presents a uniform view of the files from various
different sources, such as the files on local disk, on an HTTP server, or
inside a Zip archive.")
    (license license:asl2.0)))

(define-public java-jakarta-oro
  (package
    (name "java-jakarta-oro")
    (version "2.0.8")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/jakarta/oro/"
                                  "jakarta-oro-" version ".tar.gz"))
              (sha256
               (base32
                "0rpmnsskiwmsy8r0sckz5n5dbvh3vkxx8hpm177c754r8xy3qksc"))
              (modules '((guix build utils)))
              (snippet
               `(begin
                  (delete-file (string-append "jakarta-oro-" ,version ".jar"))
                  #t))))
    (build-system ant-build-system)
    (arguments
     `(#:build-target "package"
       #:make-flags (list "-DDATE" "(no recorded date for reproducibility)")
       #:tests? #f; tests are run as part of the build process
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (install-jars ,(string-append "jakarta-oro-" version))))))
    (home-page "https://jakarta.apache.org/oro/")
    (synopsis "Text-processing for Java")
    (description "The Jakarta-ORO Java classes are a set of text-processing
Java classes that provide Perl5 compatible regular expressions, AWK-like
regular expressions, glob expressions, and utility classes for performing
substitutions, splits, filtering filenames, etc.  This library is the successor
of the OROMatcher, AwkTools, PerlTools, and TextTools libraries originally
from ORO, Inc.")
    (license license:asl1.1)))

(define-public java-native-access
  (package
    (name "java-native-access")
    (version "4.5.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/java-native-access/jna/"
                                  "archive/" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0zrpzkib6b905i018a9pqlzkqinphywr6y4jwv6mwp63jjqvqkd9"))
              (modules '((guix build utils)))
              (snippet
                `(begin
                   (for-each delete-file (find-files "." ".*.jar"))
                   (delete-file-recursively "native/libffi")
                   (delete-file-recursively "dist")
                   #t))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f; FIXME: tests require reflections.jar
       #:test-target "test"
       #:make-flags (list "-Ddynlink.native=true")
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'fix-build.xml
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "build.xml"
               ;; Since we removed the bundled ant.jar, give the correct path
               (("lib/ant.jar") (search-input-file inputs "/lib/ant.jar"))
               ;; We removed generated native libraries. We can only rebuild one
               ;; so don't fail if we can't find a native library for another architecture.
               (("zipfileset") "zipfileset erroronmissingarchive=\"false\""))
             ;; Copy test dependencies
             (copy-file (car (find-files (assoc-ref inputs "java-junit") "jar$"))
                        "lib/junit.jar")
             (copy-file (car (find-files (assoc-ref inputs "java-hamcrest-core")
                                         "jar$"))
                        "lib/hamcrest-core.jar")
             ;; FIXME: once reflections.jar is built, copy it to lib/test.
             #t))
         (add-before 'build 'build-native
           (lambda _
             (invoke "ant" "-Ddynlink.native=true" "native")
             #t))
         (replace 'install
           (install-jars "build")))))
    (inputs
     (list libffi libx11 libxt))
    (native-inputs
     (list gcc-7 java-junit java-hamcrest-core))
    (home-page "https://github.com/java-native-access/jna")
    (synopsis "Access to native shared libraries from Java")
    (description "JNA provides Java programs easy access to native shared
libraries without writing anything but Java code - no JNI or native code is
required.  JNA allows you to call directly into native functions using natural
Java method invocation.")
    ;; Java Native Access project (JNA) is dual-licensed under 2
    ;; alternative Free licenses: LGPL 2.1 or later and Apache License 2.0.
    (license (list
               license:asl2.0
               license:lgpl2.1+))))

(define-public java-native-access-platform
  (package
    (inherit java-native-access)
    (name "java-native-access-platform")
    (arguments
     `(#:test-target "test"
       #:tests? #f; require jna-test.jar
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'chdir
           (lambda _
             (chdir "contrib/platform")
             #t))
         (add-after 'chdir 'fix-ant
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "nbproject/project.properties"
               (("../../build/jna.jar")
                (search-input-file inputs "/share/java/jna.jar"))
               (("../../lib/hamcrest-core-.*.jar")
                (car (find-files (assoc-ref inputs "java-hamcrest-core")
                                 "jar$")))
               (("../../lib/junit.jar")
                (car (find-files (assoc-ref inputs "java-junit")
                                 "jar$"))))
             #t))
         (replace 'install
           (install-jars "dist")))))
    (inputs
     (list java-native-access))
    (synopsis "Cross-platform mappings for jna")
    (description "java-native-access-platform has cross-platform mappings
and mappings for a number of commonly used platform functions, including a
large number of Win32 mappings as well as a set of utility classes that
simplify native access.")))

(define-public java-jsch-agentproxy-core
  (package
    (name "java-jsch-agentproxy-core")
    (version "0.0.8")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/ymnk/jsch-agent-proxy/archive/"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "02iqg6jbc1kxvfzqcg6wy9ygqxfm82bw5rf6vnswqy4y572niz4q"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "jsch-agentproxy-core.jar"
       #:source-dir "jsch-agent-proxy-core/src/main/java"
       #:tests? #f)); no tests
    (home-page "https://github.com/ymnk/jsch-agent-proxy")
    (synopsis "Core component of the proxy to ssh-agent and Pageant in Java")
    (description "jsch-agent-proxy is a proxy program to OpenSSH's ssh-agent
and Pageant included Putty.  It will be easily integrated into JSch, and users
will be allowed to use these programs for authentication.")
    (license license:bsd-3)))

(define-public java-jsch-agentproxy-sshagent
  (package
    (inherit java-jsch-agentproxy-core)
    (name "java-jsch-agentproxy-sshagent")
    (arguments
     `(#:jar-name "jsch-agentproxy-sshagent.jar"
       #:source-dir "jsch-agent-proxy-sshagent/src/main/java"
       #:tests? #f)); no tests
    (inputs
     (list java-jsch-agentproxy-core))
    (synopsis "Proxy to ssh-agent")
    (description "jsch-agent-proxy is a proxy program to OpenSSH's ssh-agent
and Pageant included in Putty. This component contains the code for a proxy to
ssh-agent.")))

(define-public java-jsch-agentproxy-usocket-jna
  (package
    (inherit java-jsch-agentproxy-core)
    (name "java-jsch-agentproxy-usocket-jna")
    (arguments
     `(#:jar-name "jsch-agentproxy-usocket-jna.jar"
       #:source-dir "jsch-agent-proxy-usocket-jna/src/main/java"
       #:tests? #f)); no tests
    (inputs
     (list java-jsch-agentproxy-core java-native-access))
    (synopsis "USocketFactory implementation using JNA")
    (description "jsch-agent-proxy is a proxy program to OpenSSH's ssh-agent
and Pageant included in Putty. This component contains an implementation of
USocketFactory using @dfn{JNA} (Java Native Access).")))

(define-public java-jsch-agentproxy-pageant
  (package
    (inherit java-jsch-agentproxy-core)
    (name "java-jsch-agentproxy-pageant")
    (arguments
     `(#:jar-name "jsch-agentproxy-pageant.jar"
       #:source-dir "jsch-agent-proxy-pageant/src/main/java"
       #:tests? #f)); no tests
    (inputs
     (list java-jsch-agentproxy-core java-native-access
           java-native-access-platform))
    (synopsis "Proxy to pageant")
    (description "jsch-agent-proxy is a proxy program to OpenSSH's ssh-agent
and Pageant included in Putty. This component contains the code for a proxy to
pageant.")))

(define-public java-jsch-agentproxy-usocket-nc
  (package
    (inherit java-jsch-agentproxy-core)
    (name "java-jsch-agentproxy-usocket-nc")
    (arguments
     `(#:jar-name "jsch-agentproxy-usocket-nc.jar"
       #:source-dir "jsch-agent-proxy-usocket-nc/src/main/java"
       #:tests? #f)); no tests
    (inputs
     (list java-jsch-agentproxy-core))
    (synopsis "USocketFactory implementation using netcat")
    (description "jsch-agent-proxy is a proxy program to OpenSSH's ssh-agent
and Pageant included in Putty. This component contains an implementation of
USocketFactory using netcat.")))

(define-public java-jsch-agentproxy-connector-factory
  (package
    (inherit java-jsch-agentproxy-core)
    (name "java-jsch-agentproxy-connector-factory")
    (arguments
     `(#:jar-name "jsch-agentproxy-connector-factory.jar"
       #:source-dir "jsch-agent-proxy-connector-factory/src/main/java"
       #:tests? #f)); no tests
    (inputs
     (list java-jsch-agentproxy-core java-jsch-agentproxy-sshagent
           java-jsch-agentproxy-usocket-jna java-jsch-agentproxy-pageant
           java-jsch-agentproxy-usocket-nc))
    (synopsis "Connector factory for jsch agent proxy")
    (description "jsch-agent-proxy is a proxy program to OpenSSH's ssh-agent
and Pageant included in Putty. This component contains a connector factory.")))

(define-public java-jsch-agentproxy-jsch
  (package
    (inherit java-jsch-agentproxy-core)
    (name "java-jsch-agentproxy-jsch")
    (arguments
     `(#:jar-name "jsch-agentproxy-jsch.jar"
       #:source-dir "jsch-agent-proxy-jsch/src/main/java"
       #:tests? #f)); no tests
    (inputs
     (list java-jsch java-jsch-agentproxy-core))
    (synopsis "JSch integration library for agentproxy")
    (description "jsch-agent-proxy is a proxy program to OpenSSH's ssh-agent
and Pageant included in Putty. This component contains a library to use
jsch-agent-proxy with JSch.")))

(define-public java-apache-ivy
  (package
    (name "java-apache-ivy")
    (version "2.4.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache//ant/ivy/" version
                                  "/apache-ivy-" version "-src.tar.gz"))
              (sha256
               (base32
                "1xkfn57g2m7l6y0xdq75x5rnrgk52m9jx2xah70g3ggl8750hbr0"))
              (patches
                (search-patches
                  "java-apache-ivy-port-to-latest-bouncycastle.patch"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "ivy.jar"
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'remove-example
           (lambda _
             (delete-file-recursively "src/example")
             #t))
         (add-before 'build 'copy-resources
           (lambda _
             (with-directory-excursion "src/java"
               (for-each (lambda (file)
                           (install-file file (string-append "../../build/classes/" (dirname file))))
                 (append
                   (find-files "." ".*.css")
                   (find-files "." ".*.ent")
                   (find-files "." ".*.html")
                   (find-files "." ".*.properties")
                   (find-files "." ".*.xsd")
                   (find-files "." ".*.xsl")
                   (find-files "." ".*.xml"))))
             #t))
         (add-before 'build 'fix-vfs
           (lambda _
             (substitute*
               '("src/java/org/apache/ivy/plugins/repository/vfs/VfsRepository.java"
                 "src/java/org/apache/ivy/plugins/repository/vfs/VfsResource.java")
               (("import org.apache.commons.vfs") "import org.apache.commons.vfs2"))
             #t))
         (add-before 'install 'copy-manifest
           (lambda _
             (install-file "META-INF/MANIFEST.MF" "build/classes/META-INF")
             #t))
         (add-before 'install 'repack
           (lambda _
             (invoke "jar" "-cmf" "build/classes/META-INF/MANIFEST.MF" "build/jar/ivy.jar"
                     "-C" "build/classes" ".")))
         (add-after 'install 'install-bin
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((bin (string-append (assoc-ref outputs "out") "/bin"))
                    (ivy (string-append bin "/ivy"))
                    (jar (string-append (assoc-ref outputs "out") "/share/java/ivy.jar")))
               (mkdir-p bin)
               (with-output-to-file ivy
                 (lambda _
                   (display (string-append
                              "#!" (which "sh") "\n"
                              "if [[ -z $CLASSPATH ]]; then\n"
                              "  cp=\"" (getenv "CLASSPATH") ":" jar "\"\n"
                              "else\n"
                              "  cp=\"" (getenv "CLASSPATH") ":" jar ":$CLASSPATH\"\n"
                              "fi\n"
                              (which "java") " -cp $cp org.apache.ivy.Main $@\n"))))
               (chmod ivy #o755)
               #t))))))
    (inputs
     (list java-bouncycastle
           java-commons-cli
           java-commons-collections
           java-commons-httpclient
           java-commons-lang
           java-commons-vfs
           java-jakarta-oro
           java-jsch
           java-jsch-agentproxy-core
           java-jsch-agentproxy-connector-factory
           java-jsch-agentproxy-jsch
           java-junit))
    (home-page "https://ant.apache.org/ivy")
    (synopsis "Dependency manager for the Java programming language")
    (description "Ivy is a tool for managing (recording, tracking, resolving
and reporting) project dependencies.  It is characterized by the following:

@itemize
@item flexibility and configurability - Ivy is essentially process agnostic
      and is not tied to any methodology or structure.  Instead it provides the
      necessary flexibility and configurability to be adapted to a broad range
      of dependency management and build processes.
@item tight integration with Apache Ant - while available as a standalone tool,
      Ivy works particularly well with Apache Ant providing a number of
      powerful Ant tasks ranging from dependency resolution to dependency
      reporting and publication.
@end itemize")
    (license license:asl2.0)))

(define-public java-eclipse-sisu-inject
  (package
    (name "java-eclipse-sisu-inject")
    (version "0.3.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/eclipse/sisu.inject/")
                     (commit (string-append "releases/" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1yh434b8pi8cwmpk825fbvbnkkk2cwd4gxxjaygg8i9j0q3l9zp3"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "eclipse-sisu-inject.jar"
       #:source-dir "org.eclipse.sisu.inject/src"
       #:tests? #f; no tests
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (install-from-pom "org.eclipse.sisu.inject/pom.xml")))))
    (propagated-inputs
     (list java-guice java-sisu-inject-parent-pom))
    (inputs
     (list java-guice-servlet
           java-javax-inject
           java-javaee-servletapi
           java-junit
           java-slf4j-api
           java-jsr305
           java-jsr250
           java-cdi-api
           java-osgi-framework
           java-osgi-util-tracker
           java-testng))
    (home-page "https://www.eclipse.org/sisu/")
    (synopsis "Classpath scanning, auto-binding, and dynamic auto-wiring")
    (description "Sisu is a modular JSR330-based container that supports
classpath scanning, auto-binding, and dynamic auto-wiring.  Sisu uses
Google-Guice to perform dependency injection and provide the core JSR330
support, but removes the need to write explicit bindings in Guice modules.
Integration with other containers via the Eclipse Extension Registry and the
OSGi Service Registry is a goal of this project.")
    (license license:epl1.0)))

(define java-sisu-inject-parent-pom
  (package
    (inherit java-eclipse-sisu-inject)
    (name "java-sisu-inject-parent-pom")
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (replace 'install
           (install-pom-file "pom.xml")))))
    (propagated-inputs '())))

(define-public java-eclipse-sisu-plexus
  (package
    (name "java-eclipse-sisu-plexus")
    (version "0.3.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/eclipse/sisu.plexus")
                     (commit (string-append "releases/" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0lm5h0dmh41ffcwd32qnk3a87d360am36yq7168ikkyqa8jxkx28"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (for-each delete-file (find-files "." ".*.jar"))
                  (rename-file "org.eclipse.sisu.plexus.tests/src"
                               "org.eclipse.sisu.plexus.tests/java")
                  #t))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "eclipse-sisu-plexus.jar"
       #:source-dir "org.eclipse.sisu.plexus/src"
       #:test-dir "org.eclipse.sisu.plexus.tests"
       #:test-exclude
       (list
         ;; This test fails probably because we can't generate the necessary
         ;; meta-inf files.
         "**/PlexusLoggingTest.*"
         ;; FIXME: This test fails because of some injection error
         "**/PlexusRequirementTest.*")
       #:jdk ,icedtea-8
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-resources
           (lambda _
             (install-file "org.eclipse.sisu.plexus/META-INF/plexus/components.xml"
                           "build/classes/META-INF/plexus")
             #t))
         (add-before 'check 'build-test-jar
           (lambda _
             (with-directory-excursion "org.eclipse.sisu.plexus.tests/resources/component-jar/src/main/"
               (mkdir "build")
               (with-directory-excursion "java"
                 (apply invoke "javac" "-cp"
                        (string-append (getenv "CLASSPATH")
                                       ":../../../../../build/classes")
                        (find-files "." ".*.java"))
                 (for-each (lambda (file) (install-file file (string-append "../build/" file)))
                           (find-files "." ".*.jar")))
               (mkdir-p "build/META-INF/plexus")
               (copy-file "resources/META-INF/plexus/components.xml"
                          "build/META-INF/plexus/components.xml")
               (with-directory-excursion "build"
                 (invoke "jar" "cf" "../../../component-jar-0.1.jar" ".")))
             (with-directory-excursion "org.eclipse.sisu.plexus.tests/"
               (copy-recursively "META-INF" "../build/test-classes/META-INF")
               (substitute* "java/org/eclipse/sisu/plexus/DefaultPlexusContainerTest.java"
                 (("resources/component-jar")
                  "org.eclipse.sisu.plexus.tests/resources/component-jar")))
             #t))
         (replace 'install
           (install-from-pom "org.eclipse.sisu.plexus/pom.xml")))))
    (propagated-inputs
     (list java-jsr250
           java-plexus-classworlds
           java-plexus-utils
           java-plexus-component-annotations
           java-cdi-api
           java-eclipse-sisu-inject
           java-sisu-plexus-parent-pom))
    (inputs
     (list java-osgi-framework
           java-slf4j-api
           java-javax-inject
           java-guice
           java-guava
           java-aopalliance
           java-cglib))
    (native-inputs
     (list java-junit))
    (home-page "https://www.eclipse.org/sisu/")
    (synopsis "Plexus support for the sisu container")
    (description "Sisu is a modular JSR330-based container that supports
classpath scanning, auto-binding, and dynamic auto-wiring.  This package
adds Plexus support to the Sisu-Inject container.")
    (license license:epl1.0)))

(define java-sisu-plexus-parent-pom
  (package
    (inherit java-eclipse-sisu-plexus)
    (name "java-sisu-plexus-parent-pom")
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (replace 'install
           (install-pom-file "pom.xml")))))
    (propagated-inputs
     `(("java-sonatype-oss-parent-pom-9" ,java-sonatype-oss-parent-pom-9)))))

(define-public java-commons-compiler
  (package
    (name "java-commons-compiler")
    (version "3.0.8")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/janino-compiler/janino")
                     (commit "91aa95686d1e4ca3b16a984a03a38686572331b2")))
              (file-name (string-append name "-" version))
              (sha256
               (base32
                "04hfdl59sgh20qkxzgnibvs8f9hy6n7znxwpk611y5d89977y62r"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (for-each delete-file
                            (find-files "." "\\.jar$"))
                  #t))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "commons-compiler.jar"
       #:source-dir "commons-compiler/src/main"
       #:tests? #f)); no tests
    (home-page "https://github.com/janino-compiler/janino")
    (synopsis "Java compiler")
    (description "Commons-compiler contains an API for janino, including the
@code{IExpressionEvaluator}, @code{IScriptEvaluator}, @code{IClassBodyEvaluator}
and @code{ISimpleCompiler} interfaces.")
    (license license:bsd-3)))

(define-public java-janino
  (package
    (inherit java-commons-compiler)
    (name "java-janino")
    (arguments
     `(#:jar-name "janino.jar"
       #:source-dir "src/main/java"
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'chdir
           (lambda _
             (chdir "janino")
             #t)))))
    (inputs
     (list java-commons-compiler))
    (native-inputs
     (list java-junit java-hamcrest-core))
    (description "Janino is a Java compiler.  Janino can compile a set of
source files to a set of class files like @code{javac}, but also compile a
Java expression, block, class body or source file in memory, load the bytecode
and execute it directly in the same JVM.  @code{janino} can also be used for
static code analysis or code manipulation.")))

(define-public java-logback-core
  (package
    (name "java-logback-core")
    (version "1.2.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/qos-ch/logback/")
                    (commit (string-append "v_" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "055jbfpg3l5qw7pw2snkdag0gjkb4vcxfg9110cqqyc40k2nd17z"))
              (modules '((guix build utils)))
              (snippet
               '(delete-file-recursively "logback-access/lib"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "logback.jar"
       #:source-dir "src/main/java"
       #:test-dir "src/test"
       #:test-exclude
       ;; These tests fail with Unable to set MockitoNamingPolicy on cglib generator
       ;; which creates FastClasses
       (list "**/AllCoreTest.*"
             "**/AutoFlushingObjectWriterTest.*"
             "**/PackageTest.*"
             "**/ResilientOutputStreamTest.*"
             ;; And we still don't want to run abstract classes
             "**/Abstract*.*")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'chdir
           (lambda _ (chdir "logback-core"))))))
    (inputs
     (list java-javax-mail
           java-javaee-servletapi
           java-commons-compiler
           java-janino))
    (native-inputs
     (list java-junit
           java-hamcrest-core
           java-mockito-1
           java-cglib
           java-objenesis
           java-joda-time))
    (home-page "https://logback.qos.ch")
    (synopsis "Logging for java")
    (description "Logback is intended as a successor to the popular log4j project.
This module lays the groundwork for the other two modules.")
    ;; Either epl1.0 or lgpl2.1
    (license (list license:epl1.0
                   license:lgpl2.1))))

(define-public java-logback-classic
  (package
    (inherit java-logback-core)
    (name "java-logback-classic")
    (arguments
     `(#:jar-name "logback-classic.jar"
       #:source-dir "src/main/java"
       #:test-dir "src/test"
       #:tests? #f; tests require more packages: h2, greenmail, hsql, subethamail, slf4j, log4j, felix
       #:jdk ,icedtea-8
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'chdir
           (lambda _
             (chdir "logback-classic")
             #t))
         (replace 'build
           (lambda* (#:key inputs #:allow-other-keys)
             (mkdir-p "build/classes")
             (setenv "CLASSPATH"
                     (string-join
                       (apply append (map (lambda (input)
                                            (find-files (assoc-ref inputs input)
                                                        ".*.jar"))
                                          '("java-logback-core" "java-slf4j-api"
                                            "java-commons-compiler" "java-javaee-servletapi"
                                            "groovy")))
                       ":"))
             (apply invoke "groovyc" "-d" "build/classes" "-j"
                    (find-files "src/main/" ".*\\.(groovy|java)$"))
             (invoke "ant" "jar")
             #t)))))
    (inputs
     (modify-inputs (package-inputs java-logback-core)
       (prepend java-logback-core java-slf4j-api)))
    (native-inputs
     (list groovy))
    (description "Logback is intended as a successor to the popular log4j project.
This module can be assimilated to a significantly improved version of log4j.
Moreover, @code{logback-classic} natively implements the slf4j API so that you
can readily switch back and forth between logback and other logging frameworks
such as log4j or @code{java.util.logging} (JUL).")))

(define-public java-jgit
  (package
    (name "java-jgit")
    (version "4.7.0.201704051617-r")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://repo1.maven.org/maven2/"
                                  "org/eclipse/jgit/org.eclipse.jgit/"
                                  version "/org.eclipse.jgit-"
                                  version "-sources.jar"))
              (sha256
               (base32
                "13ii4jn02ynzq6i7gsyi21k2i94jpc85wf6bcm31q4cyvzv0mk4k"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f                      ; There are no tests to run.
       #:jar-name "jgit.jar"
       ;; JGit must be built with a JDK supporting Java 8.
       #:jdk ,icedtea-8
       ;; Target our older default JDK.
       #:make-flags (list "-Dtarget=1.7")
       #:phases
       (modify-phases %standard-phases
         ;; The jar file generated by the default build.xml does not include
         ;; the text properties files, so we need to add them.
         (add-after 'build 'add-properties
           (lambda* (#:key jar-name #:allow-other-keys)
             (with-directory-excursion "src"
               (apply invoke "jar" "-uf"
                      (string-append "../build/jar/" jar-name)
                      (find-files "." "\\.properties$")))
             #t)))))
    (inputs
     (list java-classpathx-servletapi java-javaewah java-jsch
           java-slf4j-api))
    (home-page "https://eclipse.org/jgit/")
    (synopsis "Java library implementing the Git version control system")
    (description "JGit is a lightweight, pure Java library implementing the
Git version control system, providing repository access routines, support for
network protocols, and core version control algorithms.")
    (license license:edl1.0)))

;; For axoloti.
(define-public java-jgit-4.2
  (package (inherit java-jgit)
    (version "4.2.0.201601211800-r")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://repo1.maven.org/maven2/"
                                  "org/eclipse/jgit/org.eclipse.jgit/"
                                  version "/org.eclipse.jgit-"
                                  version "-sources.jar"))
              (sha256
               (base32
                "15gm537iivhnzlkjym4x3wn5jqdjdragsw9pdpzqqg21nrc817mm"))))
    (build-system ant-build-system)
    (arguments
     (substitute-keyword-arguments (package-arguments java-jgit)
       ((#:phases phases)
        `(modify-phases ,phases
           (add-after 'unpack 'use-latest-javaewah-API
             (lambda _
               (substitute* "src/org/eclipse/jgit/internal/storage/file/BitmapIndexImpl.java"
                 (("wordinbits") "WORD_IN_BITS"))))))))
    (inputs
     (list java-javaewah java-jsch java-slf4j-api))))

(define-public abcl
  (package
    (name "abcl")
    (version "1.9.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://abcl.org/releases/"
                           version "/abcl-src-" version ".tar.gz"))
       (sha256
        (base32
         "1zxjpwv98bq2yd5qg08sbkajj17m7b8xmzqhw296161s7lia215v"))
       (patches
        (search-patches
         "abcl-fix-build-xml.patch"))))
    (build-system ant-build-system)
    (native-inputs
     (list java-junit))
    (arguments
     `(#:build-target "abcl.jar"
       #:test-target "abcl.test"
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((share (string-append (assoc-ref outputs "out")
                                         "/share/java/"))
                   (bin (string-append (assoc-ref outputs "out")
                                       "/bin/")))
               (mkdir-p share)
               (install-file "dist/abcl.jar" share)
               (install-file "dist/abcl-contrib.jar" share)
               (mkdir-p bin)
               (with-output-to-file (string-append bin "abcl")
                 (lambda _
                   (let ((classpath (string-append
                                     share "abcl.jar"
                                     ":"
                                     share "abcl-contrib.jar")))
                     (display (string-append
                               "#!" (which "bash") "\n"
                               "if [[ -z $CLASSPATH ]]; then\n"
                               "  cp=\"" classpath "\"\n"
                               "else\n"
                               "  cp=\"" classpath ":$CLASSPATH\"\n"
                               "fi\n"
                               "exec " (which "java")
                               " -cp \"$cp\" org.armedbear.lisp.Main \"$@\"\n")))))
               (chmod (string-append bin "abcl") #o755)
               #t))))))
    (home-page "https://abcl.org/")
    (synopsis "Common Lisp Implementation on the JVM")
    (description
     "@dfn{Armed Bear Common Lisp} (ABCL) is a full implementation of the Common
Lisp language featuring both an interpreter and a compiler, running in the
JVM.  It supports JSR-223 (Java scripting API): it can be a scripting engine
in any Java application.  Additionally, it can be used to implement (parts of)
the application using Java to Lisp integration APIs.")
    (native-search-paths
     (list (search-path-specification
            (variable "XDG_DATA_DIRS")
            (files '("share")))))
    (license (list license:gpl2+
                   ;; named-readtables is released under 3 clause BSD
                   license:bsd-3
                   ;; jfli is released under CPL 1.0
                   license:cpl1.0))))

(define-public java-jsonp-api
  (package
    (name "java-jsonp-api")
    (version "1.1.6")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/eclipse-ee4j/jsonp")
                     (commit (string-append "1.1-" version "-RELEASE"))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0zrj03hkr3jdmqlb4ipjr37cqpp2q2814qpmxi7srlwpdqs0ibgc"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "jsonp-api.jar"
       #:tests? #f
       #:source-dir "api/src/main/java"
       #:test-dir "api/src/test"))
    (home-page "https://eclipse-ee4j.github.io/jsonp/")
    (synopsis "JSON Processing in Java")
    (description "JSON Processing (JSON-P) is a Java API to process (e.g.
parse, generate, transform and query) JSON messages.  It produces and
consumes JSON text in a streaming fashion (similar to StAX API for XML)
and allows building a Java object model for JSON text using API classes
(similar to DOM API for XML).")
    ;; either gpl2 only with classpath exception, or epl2.0.
    (license (list license:gpl2
                   license:epl2.0))))

(define-public java-jsonp-impl
  (package
    (inherit java-jsonp-api)
    (name "java-jsonp-impl")
    (arguments
     `(#:jar-name "jsonp-impl.jar"
       #:tests? #f
       #:source-dir "impl/src/main/java"
       #:test-dir "impl/src/test"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively
               "impl/src/main/resources/"
               "build/classes")
             #t)))))
    (propagated-inputs
     (list java-jsonp-api))
    (description "JSON Processing (JSON-P) is a Java API to process (e.g.
parse, generate, transform and query) JSON messages.  This package contains
a reference implementation of that API.")))

(define-public java-jakarta-json
  (package
    (name "java-jakarta-json")
    (version "2.1.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/jakartaee/jsonp-api")
                     (commit (string-append version "-RELEASE"))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1q600harqfhlf763l75j4fx7ai7ybp7ga06aiky2a2hg8mhz0s5f"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "jakarta-json.jar"
       #:source-dir "api/src/main/java"
       #:tests? #f; no tests
       #:jdk ,openjdk11))
    (home-page "https://github.com/jakartaee/jsonp-api")
    (synopsis "Portable API for JSON handling in Java")
    (description "This project contains API and Compatible Implementation of
Jakarta JSON Processing specification.  Jakarta JSON Processing provides
portable APIs to parse, generate, transform, and query JSON documents.")
    ;; with classpath exception
    (license license:epl2.0)))

(define-public java-parsson
  (package
    (name "java-parsson")
    (version "1.1.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/eclipse-ee4j/parsson")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "06vvr6qv1ihnk212gdxg4x0sd61lgxk7wf062s7gym5k2h7fms0p"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "parsson.jar"
       #:source-dir "impl/src/main/java"
       #:test-dir "impl/src/test"
       #:use-java-modules? #t
       #:jdk ,openjdk11
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'copy-resources
           (lambda _
             (copy-recursively "impl/src/main/resources"
                               "build/classes"))))))
    (inputs
      (list java-jakarta-json))
    (native-inputs
      (list java-junit))
    (home-page "https://github.com/eclipse-ee4j/parsson")
    (synopsis "Implementation of Jakarta JSON API")
    (description "Eclipse Parsson is an implementation of the Jakarta JSON
Processing specification.")
    ;; with classpath exception
    (license license:epl2.0)))

(define-public java-jakarta-annotations-api
  (package
    (name "java-jakarta-annotations-api")
    (version "2.1.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/jakartaee/common-annotations-api")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0xq2n2pijal5p75vl9dz10i6hhdmaxw5q8j3aza717xkpxxby9p6"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "jakarta-annotations-api.jar"
       #:source-dir "api/src/main/java"
       #:tests? #f; no tests
       #:jdk ,openjdk11))
    (home-page "https://github.com/jakartaee/common-annotations-api")
    (synopsis "Collection of Java annotations")
    (description "Jakarta Annotations defines a collection of annotations
representing common semantic concepts that enable a declarative style of
programming that applies across a variety of Java technologies.")
    ;; with classpath exception
    (license (list license:epl2.0
                   license:gpl2))))

(define-public java-xmp
  (package
    (name "java-xmp")
    (version "5.1.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://download.macromedia.com/pub/developer"
                                  "/xmp/sdk/XMPCoreJava-" version ".zip"))
              (sha256
               (base32
                "14nai2mmsg7l5ya2y5mx4w4lr1az3sk2fjz6hiy4zdrsavgvl1g7"))))
    (build-system ant-build-system)
    (arguments
     `(#:build-target "build"
       #:tests? #f; no tests
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _
             (chdir "XMPCore")
             #t))
         (add-before 'build 'fix-timestamp
           (lambda _
             (substitute* "build.xml"
               (("\\$\\{BuildDate\\}") "1970 Jan 01 00:00:00-GMT"))
             #t))
         (replace 'install
           (install-jars "."))
         (add-after 'install 'install-doc
           (lambda* (#:key outputs #:allow-other-keys)
             (copy-recursively
               "docs"
               (string-append (assoc-ref outputs "out") "/share/doc/java-xmp"))
             #t)))))
    (native-inputs
     (list unzip))
    (home-page "https://www.adobe.com/devnet/xmp.html")
    (synopsis "Extensible Metadat Platform (XMP) support in Java")
    (description "Adobe's Extensible Metadata Platform (XMP) is a labeling
technology that allows you to embed data about a file, known as metadata,
into the file itself.  The XMP Toolkit for Java is based on the C++ XMPCore
library and the API is similar.")
    (license license:bsd-3)))

(define-public java-args4j
  (package
    (name "java-args4j")
    (version "2.33")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/kohsuke/args4j")
                    (commit (string-append "args4j-site-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0w061fg65qrsm1a0lz0vyprsyidj31krjb459qi2lw0y78xza26s"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "args4j.jar"
       #:source-dir "args4j/src"
       #:test-dir "args4j/test"
       #:test-exclude
       (list "**/ExampleTest.*"
             "**/ExternalConfiguredTest.*" ; fails to find a file
             ;; We still don't want to run abstract classes
             "**/Abstract*.*")
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'fix-test-dir
           (lambda _
             (substitute* "build.xml"
               (("/java\">") "\">"))
             #t))
         (add-before 'build 'copy-resources
           (lambda _
             (let ((from-prefix "args4j/src/org/kohsuke/args4j/")
                   (to-prefix "build/classes/org/kohsuke/args4j/"))
               (for-each (lambda (f)
                           (install-file
                            (string-append from-prefix f)
                            (string-append to-prefix (dirname f))))
                         (list "Messages.properties"
                               "Messages_de.properties"
                               "Messages_en.properties"
                               "Messages_ru.properties"
                               "spi/Messages.properties"
                               "spi/Messages_de.properties"
                               "spi/Messages_en.properties"
                               "spi/Messages_ru.properties")))
             #t)))))
    (native-inputs
     (list java-junit))
    (home-page "https://args4j.kohsuke.org/")
    (synopsis "Command line parser library")
    (description "Args4j is a small Java class library that makes it easy to
parse command line options/arguments in your CUI application.")
    (license license:expat)))

(define-public java-argparse4j
  (package
    (name "java-argparse4j")
    (version "0.9.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/argparse4j/argparse4j")
                    (commit (string-append "argparse4j-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1i0j3zs1ln48n0g8a90vqbv6528mcswhzys6252yp0c8w1ai64fb"))))
    (build-system ant-build-system)
    (arguments
     (list #:jar-name "java-argparse4j.jar"
           #:source-dir "main/src/main/"
           #:test-dir "main/src/test/"
           #:jdk openjdk11
           #:phases #~(modify-phases %standard-phases
                        (add-after 'unpack 'copy-resources
                          (lambda _
                            (copy-recursively "main/src/test/resources"
                                              "target/test-classes")
                            (copy-recursively "main/src/main/resources"
                                              "build/classes")))
                        (replace 'install
                          (install-from-pom "pom.xml")))))
    (inputs (list java-junit))
    (home-page "https://argparse4j.github.io/")
    (synopsis "Java command-line argument parser library")
    (description "Argparse4j is a command line argument parser library for
Java based on Python's @code{argparse} module.")
    (license license:expat)))

(define-public java-metadata-extractor
  (package
    (name "java-metadata-extractor")
    (version "2.19.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/drewnoakes/metadata-extractor")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1axnw47n06a116g8bml2l6rxx92grxp935gspyhz4pz868m7hrna"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "metadata-extractor.jar"
       #:source-dir "Source"
       #:test-dir "Tests"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'fix-xmp-path
           (lambda _
             ;; This version of metadata-extractor depends on java-xmp 6, which
             ;; is non-free.  These changes allow using java-xmp 5, which is
             ;; the last free-software version of java-xmp.
             (substitute* (find-files "." ".*.java")
                (("com.adobe.internal.xmp") "com.adobe.xmp"))
             (substitute* "Source/com/drew/metadata/xmp/XmpReader.java"
                (("\\.setXMPNodesToLimit.*") ";"))
             ;; Test failure, most likely because we use a different java-xmp
             ;; version.
             (delete-file "Tests/com/drew/metadata/xmp/XmpReaderTest.java")))
         (add-before 'check 'fix-test-dir
           (lambda _
             (substitute* "build.xml"
               (("/java\">") "\">"))
             #t)))))
    (propagated-inputs
     (list java-xmp))
    (native-inputs
     (list java-hamcrest-core java-junit))
    (home-page "https://github.com/drewnoakes/metadata-extractor")
    (synopsis "Extract metadata from image and video files")
    (description "Metadata-extractor is a straightforward Java library for
reading metadata from image files.  It is able to read metadata in Exif,
IPTC, XMP, ICC and more formats.")
    (license license:asl2.0)))

(define-public java-svg-salamander
  (package
    (name "java-svg-salamander")
    (version "1.1.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/blackears/svgSalamander")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1zv3kjdkf6iqf02x6ln76254y634j2ji448y706a65lsbfjmmicf"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (for-each delete-file (find-files "." ".*.jar"))
                  #t))
              (patches
                (search-patches "java-svg-salamander-Fix-non-det.patch"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f; no tests
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _
             (chdir "svg-core")
             #t))
         (add-before 'build 'copy-jars
           (lambda* (#:key inputs #:allow-other-keys)
             (copy-file (car (find-files (assoc-ref inputs "javacc") "\\.jar$"))
                        "../libraries/javacc.jar")
             (copy-file (car (find-files (assoc-ref inputs "ant") "ant\\.jar$"))
                        "../libraries/ant.jar")
             #t))
         (replace 'install
           (install-jars "dist")))))
    (native-inputs
     (list javacc))
    (home-page "https://github.com/blackears/svgSalamander")
    (synopsis "SVG engine for Java")
    (description "SVG Salamander is an SVG engine for Java that's designed
to be small, fast, and allow programmers to use it with a minimum of fuss.
It's in particular targeted for making it easy to integrate SVG into Java
games and making it much easier for artists to design 2D game content - from
rich interactive menus to charts and graphcs to complex animations.")
    (license license:bsd-2)))

(define-public java-jboss-transaction-api-spec
  (package
    (name "java-jboss-transaction-api-spec")
    (version "1.2+1.1.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/jboss/jboss-transaction-api_spec")
                     (commit "jboss-transaction-api_1.2_spec-1.1.1.Final")))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1xbfq5hvb86izflydxrqqv3k26c1ba2m0ap6m97shqrsdi9by4wy"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "java-jboss-transaction-api_spec.jar"
       #:source-dir "src/main/java"
       #:tests? #f)); no tests
    (inputs
     (list java-cdi-api java-jboss-interceptors-api-spec))
    (home-page "https://github.com/jboss/jboss-transaction-api_spec")
    (synopsis "Generic transaction management API in Java")
    (description "Java-jboss-transaction-api-spec implements the Transactions
API.  A transaction is a unit of work containing one or more operations
involving one or more shared resources having ACID (Atomicity, Consistency,
Isolation and Durability) properties.")
    ;; either gpl2 only with classpath exception or cddl.
    (license (list license:gpl2 license:cddl1.0))))

(define-public java-picocli
  (package
    (name "java-picocli")
    (version "4.3.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/remkop/picocli")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1sxp6rxjfgjd98ly14b3d15dvxkm5wg4g46w12jyhmr0kmkaca3c"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "picocli.jar"
       #:source-dir "src/main/java"
       ;; Tests require missing dependencies (junitparams, system-rules)
       #:tests? #f))
    (home-page "https://picocli.info")
    (synopsis "REPL for the JVM")
    (description "Picocli is a framework for building command line applications
for the JVM.  It supports colors, autocompletion, subcommands, and more.  Written
in Java, usable from Groovy, Kotlin, Scala, etc.")
    (license license:asl2.0)))

(define-public java-pj
  (package
    (name "java-pj")
    (version "20150107")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.cs.rit.edu/~ark/pjsrc"
                                  version ".jar"))
              (sha256
               (base32
                "078xwaivl2qqjc07r0vk6kzpqlcb1bcar80p8r5qigh34hpr86d3"))
              (modules '((guix build utils)))
              (snippet
               '(for-each delete-file
                          (find-files "." "\\.class$")))))
    (build-system ant-build-system)
    (arguments
     (list
      #:tests? #false ;there are none
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'chdir
            (lambda _ (chdir "src/pj")))
          (add-after 'chdir 'patch-source-directory
            (lambda _
              (substitute* "compile"
                (("SRCDIR1=/home/ark/public_html/pj/lib")
                 (string-append "SRCDIR1=" (getcwd) "/lib")))))
          (replace 'build
            (lambda _
              (invoke "bash" "./compile" "linux")
              (with-directory-excursion "lib"
                (apply invoke "jar" "cf" (string-append "pj" #$version ".jar")
                       (find-files "." "\\.class$")))))
          (replace 'install (install-jars ".")))))
    (home-page "https://www.cs.rit.edu/~ark/pj.shtml")
    (synopsis "Parallel programming in Java")
    (description "Parallel Java (PJ) is an API and middleware for parallel
programming in 100% Java on shared memory multiprocessor (SMP) parallel
computers, cluster parallel computers, and hybrid SMP cluster parallel
computers.")
    (license license:gpl3+)))

(define-public java-jetbrains-annotations
  (package
    (name "java-jetbrains-annotations")
    (version "19.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/JetBrains/java-annotations")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0z6i1xs60cd5ffz23c49sq68wn5mphhs3xpar1n93ppama2ng80v"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "jetbrains-annotations.jar"
       #:source-dir "common/src/main/java:java8/src/main/java"
       #:tests? #f)); no tests
    (home-page "https://github.com/JetBrains/java-annotations")
    (synopsis "Annotations for Java and other JVM languages")
    (description "This package contains a set of Java annotations which can be
used in JVM-based languages.  They serve as an additional documentation and
can be interpreted by IDEs and static analysis tools to improve code analysis.")
    (license license:expat)))

(define-public java-javaparser
  (package
    (name "java-javaparser")
    (version "3.16.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/javaparser/javaparser")
                     (commit (string-append "javaparser-parent-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1a4jk12ffa31fa0y8vda0739vpfj1206p0nha842b7bixbvwamv9"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (for-each delete-file
                            (find-files "." "\\.jar$"))
                  #t))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f; tests require jbehave and junit5
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'fill-template
           (lambda _
             (with-directory-excursion "javaparser-core/src/main"
               (copy-file "java-templates/com/github/javaparser/JavaParserBuild.java"
                          "java/com/github/javaparser/JavaParserBuild.java")
               (substitute* "java/com/github/javaparser/JavaParserBuild.java"
                 (("\\$\\{project.version\\}") ,version)
                 (("\\$\\{project.name\\}") "javaparser")
                 (("\\$\\{project.build.finalName\\}") "javaparser")
                 (("\\$\\{maven.version\\}") "fake")
                 (("\\$\\{maven.build.version\\}") "fake")
                 (("\\$\\{build.timestamp\\}") "0")
                 (("\\$\\{java.vendor\\}") "Guix")
                 (("\\$\\{java.vendor.url\\}") "https://gnu.org/software/guix")
                 (("\\$\\{java.version\\}") "1.8")
                 (("\\$\\{os.arch\\}") "any")
                 (("\\$\\{os.name\\}") "Guix")
                 (("\\$\\{os.version\\}") "not available")))
             #t))
         (add-before 'build 'generate-javacc
           (lambda _
             (with-directory-excursion "javaparser-core/src/main/java"
               (invoke "java" "javacc" "../javacc/java.jj"))
             #t))
         (add-before 'build 'copy-javacc-support
           (lambda _
             (with-directory-excursion "javaparser-core/src/main"
               (copy-recursively "javacc-support" "java"))
             #t))
         (replace 'build
           (lambda _
             (define (build name)
               (format #t "Building ~a~%" name)
               (delete-file-recursively "build/classes")
               (mkdir-p "build/classes")
               (apply invoke "javac"
                      "-cp" (string-append (getenv "CLASSPATH") ":"
                                           (string-join (find-files "build/jar" ".")
                                                        ":"))
                      "-d" "build/classes"
                      (find-files (string-append name "/src/main/java")
                                  ".*.java"))
               (invoke "jar" "-cf" (string-append "build/jar/" name ".jar")
                       "-C" "build/classes" "."))
             (mkdir-p "build/classes")
             (mkdir-p "build/test-classes")
             (mkdir-p "build/jar")
             (build "javaparser-core")
             (build "javaparser-core-serialization")
             (build "javaparser-core-generators")
             (build "javaparser-core-metamodel-generator")
             (build "javaparser-symbol-solver-core")
             #t))
         (replace 'install
           (install-jars "build/jar")))))
    (inputs
     (list java-guava java-jboss-javassist java-jsonp-api))
    (native-inputs
     (list javacc))
    (home-page "https://javaparser.org/")
    (synopsis "Parser for Java")
    (description
     "This project contains a set of libraries implementing a Java 1.0 - Java
11 Parser with advanced analysis functionalities.")
    (license (list
               ;; either lgpl or asl
               license:lgpl3+
               license:asl2.0))))

(define-public tla2tools
  ;; This package was originally based on the "v1.8.0" tag, but that merely
  ;; points to the moving master branch.  That might be because the ‘latest
  ;; release’ at GitHub is currently 1.7.1.  We'll see!  For now, rather than
  ;; downgrade to 1.7.1 proper, use the commit that we originally dubbed 1.8.0.
  (let* ((release "1.7.1")
         (revision "0")
         (commit "6932e19083fc6df42473464857fc1280cb5aaecc"))
    (package
      (name "tla2tools")
      (version (git-version release revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/tlaplus/tlaplus")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1hhx8gmn81k8qrkx4p7ppinmygxga9fqffd626wkvhjgg2ky8lhs"))
                (patches
                 (search-patches "tla2tools-build-xml.patch"))
                (modules '((guix build utils)))
                (snippet
                 '(begin
                    ;; Remove packaged libraries (see 'replace-libs below)
                    (for-each delete-file (find-files "." ".*.jar$"))))))
      (build-system ant-build-system)
      (arguments
       (let* ((tlatools "tlatools/org.lamport.tlatools/")
              (build-xml (string-append tlatools "customBuild.xml")))
         `(#:jdk ,openjdk11
           #:modules ((guix build ant-build-system)
                      (guix build utils)
                      (ice-9 match)
                      (srfi srfi-26))
           #:make-flags '("-f" ,build-xml)
           #:phases
           (modify-phases %standard-phases
             ;; Replace packed libs with references to jars in store
             (add-after 'unpack 'replace-libs
               (lambda* (#:key inputs #:allow-other-keys)
                 (define (input-jar input)
                   (car (find-files (assoc-ref inputs input) "\\.jar$")))
                 (for-each
                  (match-lambda
                    ((file . input)
                     (symlink (input-jar input)
                              (string-append ,tlatools "/lib/" file))))
                  '(("gson/gson-2.8.6.jar" . "java-gson")
                    ("javax.mail/mailapi-1.6.3.jar" . "java-javax-mail")
                    ("jline/jline-terminal-3.14.1.jar" . "java-jline-terminal")
                    ("jline/jline-reader-3.14.1.jar" . "java-jline-reader")
                    ("lsp/org.eclipse.lsp4j.debug-0.10.0.jar" .
                     "java-eclipse-lsp4j-debug")
                    ("lsp/org.eclipse.lsp4j.jsonrpc-0.10.0.jar" .
                     "java-eclipse-lsp4j-jsonrpc")
                    ("lsp/org.eclipse.lsp4j.jsonrpc.debug-0.10.0.jar" .
                     "java-eclipse-lsp4j-jsonrpc-debug")
                    ("junit-4.12.jar" . "java-junit")
                    ("easymock-3.3.1.jar" . "java-easymock")))
                 ;; Retain a tiny subset of the original X-Git-*
                 ;; manifest values just to aid in debugging.
                 (substitute* ,build-xml
                   (("\\$\\{git.tag\\}") (string-append "v" ,release)))))
             (add-before 'check 'prepare-tests
               (lambda _
                 ;; The pcal tests write to .cfg files.
                 (for-each (cut chmod <> #o644)
                           (find-files (string-append ,tlatools
                                                      "/test-model/pcal")
                                       "\\.cfg$"))))
             (replace 'install
               (lambda* (#:key inputs outputs #:allow-other-keys)
                 (let* ((share (string-append (assoc-ref outputs "out") "/share/java"))
                        (jar-name "tla2tools.jar"); set in project.properties
                        (jar (string-append ,tlatools
                                            "/dist/" jar-name))
                        (java-cp (string-append share "/" jar-name))
                        (bin (string-append (assoc-ref outputs "out") "/bin"))
                        (java (search-input-file inputs "/bin/java")))
                   (install-file jar share)
                   (mkdir-p bin)
                   ;; Generate wrapper scripts for bin/, which invoke common
                   ;; commands within tla2tools.jar.  Users can still invoke
                   ;; tla2tools.jar for the rest.
                   (for-each
                    (match-lambda
                      ((wrapper . class)
                       (let ((file (string-append bin "/" wrapper)))
                         (begin
                           (with-output-to-file file
                             (lambda _
                               (display
                                (string-append
                                 "#!/bin/sh\n"
                                 java " -XX:+UseParallelGC " " -cp " java-cp " " class " \"$@\""))))
                           (chmod file #o755)))))
                    ;; bin/wrapper . java-class
                    '(("pcal" . "pcal.trans")
                      ("tlatex" . "tla2tex.TLA")
                      ("tla2sany" . "tla2sany.SANY")
                      ("tlc2" . "tlc2.TLC")
                      ("tlc2-repl" . "tlc2.REPL"))))))))))
      (native-inputs
       (list java-junit java-easymock))
      (inputs
       (list java-javax-mail
             java-gson-2.8.6
             java-jline-terminal
             java-jline-reader
             java-eclipse-lsp4j-jsonrpc
             java-eclipse-lsp4j-jsonrpc-debug
             java-eclipse-lsp4j-debug))
      (home-page "https://lamport.azurewebsites.net/tla/tools.html")
      (synopsis "TLA+ tools (analyzer, TLC, TLATeX, PlusCal translator)")
      (description "TLA+ is a high-level language for modeling programs and
systems---especially concurrent and distributed ones.  It's based on the idea
that the best way to describe things precisely is with simple
mathematics.  TLA+ and its tools are useful for eliminating fundamental design
errors, which are hard to find and expensive to correct in code.

The following TLA+ tools are available in this distribution:

@itemize
@item The Syntactic Analyzer: A parser and syntax checker for
  TLA+ specifications;
@item TLC: A model checker and simulator for a subclass of \"executable\" TLA+
  specifications;
@item TLATeX: A program for typesetting TLA+ specifications;
@item Beta test versions of 1-3 for the TLA+2 language; and
@item The PlusCal translator.
@end itemize")
      (license license:expat))))

;;;
;;; Avoid adding new packages to the end of this file. To reduce the chances
;;; of a merge conflict, place them above by existing packages with similar
;;; functionality or similar names.
;;;
