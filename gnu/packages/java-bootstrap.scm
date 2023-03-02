;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015, 2016, 2017, 2018, 2019, 2021 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2017 Leo Famulari <leo@famulari.name>
;;; Copyright © 2017, 2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2018, 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018 Gábor Boskovits <boskovits@gmail.com>
;;; Copyright © 2018 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2019 Andrius Štikonas <andrius@stikonas.eu>
;;; Copyright © 2020 Simon South <simon@simonsouth.net>
;;; Copyright © 2021 Guillaume Le Vaillant <glv@posteo.net>
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

(define-module (gnu packages java-bootstrap)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages texinfo)
  #:export (classpath-devel
            ecj4-javac-wrapper
            jamvm-with-ecj4
            ant-bootstrap))


;;;
;;; Java bootstrap toolchain.
;;;

;; The Java bootstrap begins with Jikes, a Java compiler written in C++.  We
;; use it to build a simple version of GNU Classpath, the Java standard
;; library.  We chose version 0.93 because it is the last version that can be
;; built with Jikes.  With Jikes and this version of GNU Classpath we can
;; build JamVM, a Java Virtual Machine.  We build version 1.5.1 because it is
;; the last version of JamVM that works with a version of GNU classpath that
;; does not require ECJ.  These three packages make up the bootstrap JDK.

;; This is sufficient to build an older version of Ant, which is needed to
;; build an older version of ECJ, an incremental Java compiler, both of which
;; are written in Java.
;;
;; ECJ is needed to build the latest release (0.99) and the development
;; version of GNU Classpath.  The development version of GNU Classpath has
;; much more support for Java 1.6 than the latest release, but we need to
;; build 0.99 first to get a working version of javah.  ECJ, the development
;; version of GNU Classpath, and the latest version of JamVM make up the
;; second stage JDK with which we can build the OpenJDK with the Icedtea 2.x
;; build framework.  We then build the more recent JDK Icedtea 3.x, and all
;; other versions of OpenJDK.

(define jikes
  (package
    (name "jikes")
    (version "1.22")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/jikes/Jikes/"
                                  version "/jikes-" version ".tar.bz2"))
              (sha256
               (base32
                "1qqldrp74pzpy5ly421srqn30qppmm9cvjiqdngk8hf47dv2rc0c"))))
    (build-system gnu-build-system)
    (home-page "https://jikes.sourceforge.net/")
    (synopsis "Compiler for the Java language")
    (description "Jikes is a compiler that translates Java source files as
defined in The Java Language Specification into the bytecoded instruction set
and binary format defined in The Java Virtual Machine Specification.")
    (license license:ibmpl1.0)))

;; This is the last version of GNU Classpath that can be built without ECJ.
(define classpath-bootstrap
  (package
    (name "classpath")
    (version "0.93")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/classpath/classpath-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0i99wf9xd3hw1sj2sazychb9prx8nadxh2clgvk3zlmb28v0jbfz"))
              (patches (search-patches "classpath-aarch64-support.patch"
                                       "classpath-miscompilation.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list (string-append "JAVAC="
                            (search-input-file %build-inputs "/bin/jikes"))
             "--disable-Werror"
             "--disable-gmp"
             "--disable-gtk-peer"
             "--disable-gconf-peer"
             "--disable-plugin"
             "--disable-dssi"
             "--disable-alsa"
             "--disable-gjdoc")
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-data
           (lambda _ (invoke "make" "install-data"))))))
    (native-inputs
     (list jikes fastjar libltdl pkg-config))
    (home-page "https://www.gnu.org/software/classpath/")
    (synopsis "Essential libraries for Java")
    (description "GNU Classpath is a project to create core class libraries
for use with runtimes, compilers and tools for the Java programming
language.")
    ;; GPLv2 or later, with special linking exception.
    (license license:gpl2+)))

;; This is the last version of JamVM that works with a version of GNU
;; classpath that does not require ECJ.
(define jamvm-1-bootstrap
  (package
    (name "jamvm")
    (version "1.5.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/jamvm/jamvm/"
                                  "JamVM%20" version "/jamvm-"
                                  version ".tar.gz"))
              (patches (search-patches "jamvm-1.5.1-aarch64-support.patch"
                                       "jamvm-1.5.1-armv7-support.patch"))
              (sha256
               (base32
                "06lhi03l3b0h48pc7x58bk9my2nrcf1flpmglvys3wyad6yraf36"))
              ;; Remove precompiled software.
              (snippet
               '(delete-file "lib/classes.zip"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list (string-append "--with-classpath-install-dir="
                            (assoc-ref %build-inputs "classpath"))
             "--disable-int-caching"
             "--enable-runtime-reloc-checks"
             "--enable-ffi")
       #:phases
       ,(if (string-prefix? "aarch64" (or (%current-system)
                                          (%current-target-system)))
            ;; Makefiles and the configure script need to be regenerated to
            ;; incorporate support for AArch64.
            '(modify-phases %standard-phases
               (replace 'bootstrap
                 (lambda _ (invoke "autoreconf" "-vif"))))
            '%standard-phases)))
    (inputs
     (list classpath-bootstrap jikes libffi
           zip zlib))
    (native-inputs
     (if (string-prefix? "aarch64" (or (%current-system)
                                       (%current-target-system)))
         ;; Additional packages needed for autoreconf.
         `(("autoconf" ,autoconf)
           ("automake" ,automake)
           ("libtool" ,libtool))
         '()))
    (home-page "https://jamvm.sourceforge.net/")
    (synopsis "Small Java Virtual Machine")
    (description "JamVM is a Java Virtual Machine conforming to the JVM
specification edition 2 (blue book).  It is extremely small.  However, unlike
other small VMs it supports the full spec, including object finalisation and
JNI.")
    (license license:gpl2+)))

(define ant-bootstrap
  (package
    (name "ant-bootstrap")
    ;; The 1.10.x series requires Java 8.  1.9.0 and later use generics, which
    ;; are not supported.  The 1.8.x series is the last to use only features
    ;; supported by Jikes.
    (version "1.8.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/"
                                  "ant/source/apache-ant-"
                                  version "-src.tar.bz2"))
              (sha256
               (base32
                "1cg0lga887qz5iizh6mlkxp01lciymrhmp7wzxpl6zpnldxmzrjx"))))
    (build-system gnu-build-system)
    (arguments
     `(#:modules ((srfi srfi-1)
                  (guix build gnu-build-system)
                  (guix build utils))
       #:tests? #f ; no "check" target
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
         (delete 'install))))
    (native-inputs
     (list jikes jamvm-1-bootstrap unzip zip))
    (home-page "https://ant.apache.org")
    (synopsis "Build tool for Java")
    (description
     "Ant is a platform-independent build tool for Java.  It is similar to
make but is implemented using the Java language, requires the Java platform,
and is best suited to building Java projects.  Ant uses XML to describe the
build process and its dependencies, whereas Make uses Makefile format.")
    (license license:asl2.0)))

;; Version 3.2.2 is the last version without a dependency on a full-fledged
;; compiler for Java 1.5.
(define ecj-bootstrap
  (package
    (name "ecj-bootstrap")
    (version "3.2.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://archive.eclipse.org/eclipse/"
                                  "downloads/drops/R-" version
                                  "-200702121330/ecjsrc.zip"))
              (sha256
               (base32
                "05hj82kxd23qaglsjkaqcj944riisjha7acf7h3ljhrjyljx8307"))))
    ;; It would be so much easier if we could use the ant-build-system, but we
    ;; cannot as we don't have ant at this point.  We use ecj for
    ;; bootstrapping the JDK.
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; there are no tests
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "CLASSPATH"
                     (string-join
                      (cons (search-input-file inputs "/lib/rt.jar")
                            (find-files (string-append
                                         (assoc-ref inputs "ant-bootstrap")
                                         "/lib")
                                        "\\.jar$"))
                      ":"))))
         (replace 'build
           (lambda* (#:key inputs #:allow-other-keys)
             ;; The unpack phase enters the "org" directory by mistake.
             (chdir "..")

             ;; Create a simple manifest to make ecj executable.
             (with-output-to-file "manifest"
               (lambda _
                 (display "Manifest-Version: 1.0
Main-Class: org.eclipse.jdt.internal.compiler.batch.Main\n")))

             ;; Compile it all!
             (and (apply invoke "jikes"
                         (find-files "." "\\.java$"))
                  (invoke "fastjar" "cvfm"
                          "ecj-bootstrap.jar" "manifest" "."))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((share (string-append (assoc-ref outputs "out")
                                         "/share/java/")))
               (mkdir-p share)
               (install-file "ecj-bootstrap.jar" share)))))))
    (native-inputs
     (list ant-bootstrap
           unzip
           jikes
           jamvm-1-bootstrap
           fastjar))
    (home-page "https://eclipse.org")
    (synopsis "Eclipse Java development tools core batch compiler")
    (description "This package provides the Eclipse Java core batch compiler
for bootstrapping purposes.  The @dfn{Eclipse compiler for Java} (ecj) is a
requirement for all GNU Classpath releases after version 0.93.")
    (license license:epl1.0)))

(define ecj-javac-wrapper
  (package (inherit ecj-bootstrap)
    (name "ecj-javac-wrapper")
    (source #f)
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       ,#~(begin
            (use-modules (guix build utils))
            (let* ((bin    (string-append #$output "/bin"))
                   (target (string-append bin "/javac"))
                   (guile  (string-append (assoc-ref %build-inputs "guile")
                                          "/bin/guile"))
                   (ecj    (string-append #$(this-package-native-input "ecj-bootstrap")
                                          "/share/java/ecj-bootstrap.jar"))
                   (java   (string-append #$(this-package-native-input "jamvm")
                                          "/bin/jamvm"))
                   (bootcp (let ((jvmlib (string-append
                                          #$(this-package-native-input "classpath")
                                          "/share/classpath")))
                             (string-append jvmlib "/glibj.zip:"
                                            jvmlib "/tools.zip"))))
              (mkdir-p bin)
              (with-output-to-file target
                (lambda _
                  (format #t "#!~a --no-auto-compile\n!#\n" guile)
                  (write
                   `(begin (use-modules (ice-9 match)
                                        (ice-9 receive)
                                        (ice-9 hash-table)
                                        (srfi srfi-1)
                                        (srfi srfi-26))
                           (define defaults
                             '(("-bootclasspath" ,bootcp)
                               ("-source" "1.5")
                               ("-target" "1.5")
                               ("-cp"     ".")))
                           (define (main args)
                             (let ((classpath (getenv "CLASSPATH")))
                               (setenv "CLASSPATH"
                                       (string-join (list ,ecj (or classpath ""))
                                                    ":")))
                             (receive (vm-args other-args)
                                 ;; Separate VM arguments from arguments to ECJ.
                                 (partition (cut string-prefix? "-J" <>)
                                            (fold (lambda (default acc)
                                                    (if (member (first default) acc)
                                                        acc (append default acc)))
                                                  args defaults))
                               (apply system* ,java
                                      (append
                                          ;; Remove "-J" prefix
                                          (map (cut string-drop <> 2) vm-args)
                                          '("org.eclipse.jdt.internal.compiler.batch.Main")
                                        (cons "-nowarn" other-args)))))
                           ;; Entry point
                           (let ((args (cdr (command-line))))
                             (if (null? args)
                                 (format (current-error-port) "javac: no arguments given!\n")
                                 (main args)))))))
              (chmod target #o755)))))
    (native-inputs
     (list guile-3.0 ecj-bootstrap jamvm-1-bootstrap classpath-bootstrap))
    (description "This package provides a wrapper around the @dfn{Eclipse
compiler for Java} (ecj) with a command line interface that is compatible with
the standard javac executable.")))

;; The classpath-bootstrap was built without a virtual machine, so it does not
;; provide a wrapper for javah.  We cannot build the development version of
;; Classpath without javah.
(define classpath-0.99
  (package (inherit classpath-bootstrap)
    (version "0.99")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/classpath/classpath-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1j7cby4k66f1nvckm48xcmh352b1d1b33qk7l6hi7dp9i9zjjagr"))
              (patches (search-patches "classpath-aarch64-support.patch"))))
    (arguments
     `(#:configure-flags
       (list (string-append "--with-ecj-jar="
                            (assoc-ref %build-inputs "ecj-bootstrap")
                            "/share/java/ecj-bootstrap.jar")
             (string-append "JAVAC="
                            (assoc-ref %build-inputs "ecj-javac-wrapper")
                            "/bin/javac")
             (string-append "JAVA="
                            (assoc-ref %build-inputs "jamvm")
                            "/bin/jamvm")
             "GCJ_JAVAC_TRUE=no"
             "ac_cv_prog_java_works=yes"  ; trust me
             "--disable-Werror"
             "--disable-gmp"
             "--disable-gtk-peer"
             "--disable-gconf-peer"
             "--disable-plugin"
             "--disable-dssi"
             "--disable-alsa"
             "--disable-gjdoc")
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-data
           (lambda _ (invoke "make" "install-data"))))))
    (native-inputs
     (list classpath-bootstrap
           ecj-bootstrap ecj-javac-wrapper
           fastjar jamvm-1-bootstrap
           libltdl pkg-config))))

;; We need this because classpath-bootstrap does not provide all of the tools
;; we need to build classpath-devel.
(define classpath-jamvm-wrappers
  (package (inherit classpath-0.99)
    (name "classpath-jamvm-wrappers")
    (source #f)
    (build-system trivial-build-system)
    (arguments
     (list
      #:modules '((guix build utils))
      #:builder
      #~(begin
          (use-modules (guix build utils))
          (let ((bash      #$(this-package-native-input "bash-minimal"))
                (jamvm     #$(this-package-native-input "jamvm"))
                (classpath #$(this-package-native-input "classpath"))
                (bin       (string-append #$output "/bin/")))
            (mkdir-p bin)
            (for-each (lambda (tool)
                        (with-output-to-file (string-append bin tool)
                          (lambda _
                            #$@(if (string-prefix? "armhf" (or (%current-system)
                                                               (%current-target-system)))
                                   `((format #t "#!~a/bin/sh
~a/bin/jamvm -Xnocompact -classpath ~a/share/classpath/tools.zip \
gnu.classpath.tools.~a.~a $@"
                                             bash jamvm classpath tool
                                             (if (string=? "native2ascii" tool)
                                                 "Native2ASCII" "Main")))
                                   `((format #t "#!~a/bin/sh
~a/bin/jamvm -Xnocompact -Xnoinlining -classpath ~a/share/classpath/tools.zip \
gnu.classpath.tools.~a.~a $@"
                                             bash jamvm classpath tool
                                             (if (string=? "native2ascii" tool)
                                                 "Native2ASCII" "Main"))))))
                        (chmod (string-append bin tool) #o755))
                      (list "javah"
                            "rmic"
                            "rmid"
                            "orbd"
                            "rmiregistry"
                            "native2ascii"))))))
    (native-inputs
     (list bash-minimal jamvm-1-bootstrap classpath-0.99))
    (inputs '())
    (synopsis "Executables from GNU Classpath")
    (description "This package provides wrappers around the tools provided by
the GNU Classpath library.  They are executed by the JamVM virtual
machine.")))

;; The last release of GNU Classpath is 0.99 and it happened in 2012.  Since
;; then Classpath has gained much more support for Java 1.6.
(define classpath-devel
  (let ((commit "e7c13ee0cf2005206fbec0eca677f8cf66d5a103")
        (revision "1"))
    (package (inherit classpath-bootstrap)
      (version (string-append "0.99-" revision "." (string-take commit 9)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://git.savannah.gnu.org/git/classpath.git")
                      (commit commit)))
                (file-name (string-append "classpath-" version "-checkout"))
                (sha256
                 (base32
                  "1v2rww76ww322mpg3s12a1kkc6gkp31bm9gcxs532h0wq285fiw4"))
                (patches (search-patches "classpath-aarch64-support.patch"))))
      (arguments
       `(#:make-flags
         ;; Ensure that the initial heap size is smaller than the maximum
         ;; size.  By default only Xmx is set, which can lead to invalid
         ;; memory settings on some machines with a lot of memory.
         '("JAVAC_MEM_OPT=-J-Xms512M -J-Xmx768M")
         #:configure-flags
         (list (string-append "--with-ecj-jar="
                              (assoc-ref %build-inputs "ecj-bootstrap")
                              "/share/java/ecj-bootstrap.jar")
               (string-append "--with-javac="
                              (assoc-ref %build-inputs "ecj-javac-wrapper")
                              "/bin/javac")
               (string-append "JAVA="
                              (assoc-ref %build-inputs "jamvm")
                              "/bin/jamvm")
               "GCJ_JAVAC_TRUE=no"
               "ac_cv_prog_java_works=yes" ; trust me
               "--disable-Werror"
               "--disable-gmp"
               "--disable-gtk-peer"
               "--disable-gconf-peer"
               "--disable-plugin"
               "--disable-dssi"
               "--disable-alsa"
               "--disable-gjdoc")
         #:phases
         (modify-phases %standard-phases
           ;; XXX The bootstrap phase executes autogen.sh, which fails after
           ;; complaining about the lack of gettext.
           (replace 'bootstrap
             (lambda _ (invoke "autoreconf" "-vif")))
           (add-after 'unpack 'remove-unsupported-annotations
             (lambda _
               (substitute* (find-files "java" "\\.java$")
                 (("@Override") ""))))
           (add-after 'install 'install-data
             (lambda _ (invoke "make" "install-data"))))))
      (native-inputs
       (list autoconf automake libltdl libtool gettext-minimal texinfo
             pkg-config
             classpath-jamvm-wrappers  ;for javah
             ecj-bootstrap ecj-javac-wrapper fastjar
             jamvm-1-bootstrap)))))

(define jamvm
  (package (inherit jamvm-1-bootstrap)
    (version "2.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/jamvm/jamvm/"
                                  "JamVM%20" version "/jamvm-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1nl0zxz8y5x8gwsrm7n32bry4dx8x70p8z3s9jbdvs8avyb8whkn"))
              (patches
               (search-patches "jamvm-2.0.0-disable-branch-patching.patch"
                               "jamvm-2.0.0-opcode-guard.patch"
                               "jamvm-2.0.0-aarch64-support.patch"))
              ;; Remove precompiled software.
              (snippet
               '(delete-file "src/classlib/gnuclasspath/lib/classes.zip"))))
    (build-system gnu-build-system)
    (arguments
     (substitute-keyword-arguments (package-arguments jamvm-1-bootstrap)
       ((#:configure-flags _)
        '(list (string-append "--with-classpath-install-dir="
                              (assoc-ref %build-inputs "classpath"))))))
    (inputs
     `(("classpath" ,classpath-devel)
       ("ecj-javac-wrapper" ,ecj-javac-wrapper)
       ("zip" ,zip)
       ("zlib" ,zlib)))))

(define ecj-javac-wrapper-final
  (package (inherit ecj-javac-wrapper)
    (native-inputs
     (list guile-3.0 ecj-bootstrap jamvm classpath-devel))))

;; We jump ahead by patching the sources of ECJ 4.2.1 so that our bootstrap
;; JDK can build it.  ECJ 4 allows us to skip the build of the first version
;; of icedtea and build icedtea 2.x directly.
(define ecj4-bootstrap
  (package
    (name "ecj-bootstrap")
    (version "4.2.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://archive.eclipse.org/eclipse/"
                                  "downloads/drops4/R-" version
                                  "-201209141800/ecjsrc-" version ".jar"))
              (sha256
               (base32
                "1x281p87m14zylvinkiz6gc23ss7pzlx419qjbql11jriwav4qfj"))))
    ;; It would be so much easier if we could use the ant-build-system, but we
    ;; cannot as we don't have ant at this point.  We use ecj for
    ;; bootstrapping the JDK.
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; there are no tests
       #:phases
       (modify-phases %standard-phases
         (replace 'unpack
           (lambda* (#:key source #:allow-other-keys)
             (mkdir "src")
             (with-directory-excursion "src"
               (invoke "gjar" "-xf" source))
             (chdir "src")))
         (replace 'configure
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "CLASSPATH"
                     (string-join
                      (cons (search-input-file inputs "/lib/rt.jar")
                            (find-files (string-append
                                         (assoc-ref inputs "ant-bootstrap")
                                         "/lib")
                                        "\\.jar$"))
                      ":"))
             ;; This directive is not supported by our simple bootstrap JDK.
             (substitute* (find-files "." "\\.java$")
               (("@Override") ""))))
         (replace 'build
           (lambda* (#:key inputs #:allow-other-keys)
             ;; We can't compile these yet, but we don't need them at this
             ;; point anyway.
             (delete-file "org/eclipse/jdt/core/JDTCompilerAdapter.java")
             (delete-file-recursively "org/eclipse/jdt/internal/antadapter")

             ;; Create a simple manifest to make ecj executable.
             (mkdir-p "META-INF")
             (with-output-to-file "META-INF/MANIFESTS.MF"
               (lambda _
                 (display "Manifest-Version: 1.0
Main-Class: org.eclipse.jdt.internal.compiler.batch.Main\n")))

             ;; Compile it all!
             (apply invoke "javac"
                    (find-files "." "\\.java$"))

             ;; Pack it all up!  We don't use "jar" here, because
             ;; it doesn't produce reproducible zip archives.
             ;; XXX: copied from (gnu build install)
             (for-each (lambda (file)
                         (let ((s (lstat file)))
                           (unless (eq? (stat:type s) 'symlink)
                             (utime file  0 0 0 0))))
                       (find-files "." #:directories? #t))

             ;; It is important that the manifest appears first.
             (apply invoke "zip" "-0" "-X" "ecj-bootstrap.jar"
                    "META-INF/MANIFESTS.MF"
                    (delete "./META-INF/MANIFESTS.MF"
                            (find-files "." ".*" #:directories? #t)))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((share (string-append (assoc-ref outputs "out")
                                         "/share/java/")))
               (mkdir-p share)
               (install-file "ecj-bootstrap.jar" share)))))))
    (native-inputs
     (list ant-bootstrap classpath-devel ecj-javac-wrapper-final jamvm
           unzip zip))
    (home-page "https://eclipse.org")
    (synopsis "Eclipse Java development tools core batch compiler")
    (description "This package provides the Eclipse Java core batch compiler
for bootstrapping purposes.  The @dfn{Eclipse compiler for Java} (ecj) is a
requirement for all GNU Classpath releases after version 0.93.  This version
supports sufficient parts of Java 7 to build Icedtea 2.x.")
    (license license:epl1.0)))

(define ecj4-javac-wrapper
  (package
    (inherit ecj-javac-wrapper)
    (native-inputs
     (list guile-3.0 ecj4-bootstrap jamvm classpath-devel))))

(define jamvm-with-ecj4
  (package
    (inherit jamvm)
    (inputs
     (modify-inputs (package-inputs jamvm)
       (replace "ecj-javac-wrapper" ecj4-javac-wrapper)))))

