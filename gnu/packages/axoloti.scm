;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2017, 2019, 2020, 2021, 2024, 2025 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2019 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (gnu packages axoloti)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system ant)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cross-base)
  #:use-module (gnu packages embedded)
  #:use-module (gnu packages flashing-tools)
  #:use-module (gnu packages java)
  #:use-module (gnu packages java-graphics)
  #:use-module (gnu packages java-xml)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages version-control))

;; XXX The patch does not apply to libusb 1.0.24.
;; See https://github.com/axoloti/axoloti/issues/464
(define libusb-for-axoloti
  (package
    (inherit libusb)
    (version "1.0.23")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://github.com/libusb/libusb/"
                          "releases/download/v" version
                          "/libusb-" version ".tar.bz2"))
      (sha256
       (base32 "13dd2a9x290d1q8nb1lqiaf36grcvns5ripk5k2xm0lajmpc04fv"))
      (patches (list (search-patch "libusb-for-axoloti.patch")))))))

(define dfu-util-for-axoloti
  (package (inherit dfu-util)
    (name "axoloti-dfu-util")
    (version "0.8")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://dfu-util.sourceforge.net/releases/"
                           "dfu-util-" version ".tar.gz"))
       (sha256
        (base32
         "0n7h08avlzin04j93m6hkq9id6hxjiiix7ff9gc2n89aw6dxxjsm"))))
    (inputs
     `(("libusb" ,libusb-for-axoloti)))))

(define-public axoloti-runtime
  (package
    (name "axoloti-runtime")
    (version "1.0.12-2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/axoloti/axoloti")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1qffis277wshldr3i939b0r2x3a2mlr53samxqmr2nk1sfm2b4w9"))
       (modules '((guix build utils)))
       ;; Remove pre-built Java binaries.
       (snippet
        '(delete-file-recursively "lib/"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f ; no check target
      #:modules '((guix build gnu-build-system)
                  (guix build utils)
                  (srfi srfi-1)
                  (srfi srfi-26)
                  (ice-9 match)
                  (ice-9 regex))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-paths
            (lambda* (#:key inputs #:allow-other-keys)
              ;; prepare ChibiOS
              (invoke "unzip" "-o" (assoc-ref inputs "chibios"))
              (invoke "mv" "ChibiOS_2.6.9" "chibios")
              (with-directory-excursion "chibios/ext"
                (invoke "unzip" "-o" "fatfs-0.9-patched.zip"))

              ;; Remove source of non-determinism in ChibiOS
              (substitute* "chibios/os/various/shell.c"
                (("#ifdef __DATE__") "#if 0"))

              ;; Patch shell paths
              (substitute* '("src/main/java/qcmds/QCmdCompileFirmware.java"
                             "src/main/java/qcmds/QCmdCompilePatch.java"
                             "src/main/java/qcmds/QCmdFlashDFU.java")
                (("/bin/sh") (which "sh")))

              ;; Override cross compiler base name
              (substitute* "firmware/Makefile.patch"
                (("arm-none-eabi-(gcc|g\\+\\+|objcopy|objdump)" tool)
                 (which tool)))

              ;; XXX: for some reason the whitespace substitution does not
              ;; work, so we disable it.
              (substitute* "firmware/Makefile.patch"
                (("^BDIR=.*") "BDIR=${axoloti_home}/build\n"))

              ;; Hardcode full path to compiler tools
              (substitute* '("firmware/Makefile"
                             "firmware/flasher/Makefile"
                             "firmware/mounter/Makefile")
                (("TRGT =.*")
                 (string-append "TRGT = "
                                (assoc-ref inputs "cross-toolchain")
                                "/bin/arm-none-eabi-\n")))

              ;; Hardcode path to "make"
              (substitute* '("firmware/compile_firmware_linux.sh"
                             "firmware/compile_patch_linux.sh")
                (("make") (which "make")))

              ;; Hardcode path to "dfu-util"
              (substitute* "platform_linux/upload_fw_dfu.sh"
                (("-f \"\\$\\{platformdir\\}/bin/dfu-util\"") "-z \"\"")
                (("\\./dfu-util") (which "dfu-util")))))
          (delete 'configure)
          (replace 'build
            ;; Build Axoloti firmware with cross-compiler
            (lambda _
              (with-directory-excursion "platform_linux"
                (invoke "sh" "compile_firmware.sh"))))
          (replace 'install
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((share (string-append #$output "/share/axoloti/"))
                     (doc   (string-append share "doc"))
                     (dir   (getcwd))
                     (pats  '("/doc/[^/]+$"
                              "/patches/[^/]+/[^/]+$"
                              "/objects/[^/]+/[^/]+$"
                              "/firmware/.+"
                              "/chibios/[^/]+$"
                              "/chibios/boards/ST_STM32F4_DISCOVERY/[^/]+$"
                              "/chibios/(ext|os|docs)/.+"
                              "/CMSIS/[^/]+/[^/]+$"
                              "/patch/[^/]+/[^/]+$"
                              "/[^/]+\\.txt$"))
                     (pattern (string-append
                               "(" (string-join
                                    (map (cut string-append dir <>)
                                         pats)
                                    "|") ")"))
                     (files   (find-files dir
                                          (lambda (file stat)
                                            (and (eq? 'regular (stat:type stat))
                                                 (string-match pattern file))))))
                (for-each (lambda (file)
                            (install-file file
                                          (string-append
                                           share
                                           (regexp-substitute
                                            #f
                                            (string-match dir (dirname file))
                                            'pre  'post))))
                          files)))))))
    (inputs
     `(("chibios"
        ,(origin
           (method url-fetch)
           (uri "mirror://sourceforge/chibios/ChibiOS%20GPL3/Version%202.6.9/ChibiOS_2.6.9.zip")
           (sha256
            (base32
             "0lb5s8pkj80mqhsy47mmq0lqk34s2a2m3xagzihalvabwd0frhlj"))))
       ;; for compiling patches
       ("make" ,gnu-make)
       ;; for compiling firmware
       ("cross-toolchain" ,(make-arm-none-eabi-nano-toolchain-4.9))
       ;; for uploading compiled patches and firmware
       ("dfu-util" ,dfu-util-for-axoloti)))
    (native-inputs
     (list unzip))
    (home-page "http://www.axoloti.com/")
    (synopsis "Audio development environment for the Axoloti core board")
    (description
     "The Axoloti patcher offers a “patcher” environment similar to Pure Data
for sketching digital audio algorithms.  The patches run on a standalone
powerful microcontroller board: Axoloti Core.  This package provides the
runtime.")
    (license license:gpl3+)))

(define-public axoloti-patcher
  (package (inherit axoloti-runtime)
    (name "axoloti-patcher")
    (version (package-version axoloti-runtime))
    (arguments
     (list
      #:tests? #f ; no check target
      #:modules '((guix build gnu-build-system)
                  ((guix build ant-build-system) #:prefix ant:)
                  (guix build utils)
                  (srfi srfi-1)
                  (srfi srfi-26)
                  (ice-9 match)
                  (ice-9 regex)
                  (sxml simple)
                  (sxml xpath)
                  (sxml transform))
      #:imported-modules `((guix build ant-build-system)
                           ,@%default-gnu-imported-modules)
       #:phases
       #~(modify-phases %standard-phases
           (delete 'configure)
           (replace 'build
             (lambda* (#:key inputs #:allow-other-keys)
               (setenv "JAVA_HOME" (assoc-ref inputs "icedtea"))
               ;; We want to use our own jar files instead of the pre-built
               ;; stuff in lib.  So we replace the zipfileset tags in the
               ;; build.xml with new ones that reference our jars.
               (let* ((build.xml (with-input-from-file "build.xml"
                                   (lambda _
                                     (xml->sxml #:trim-whitespace? #t))))
                      (jars      (append-map (match-lambda
                                               (((? (cut string-prefix? "java-" <>)
                                                    label) . directory)
                                                (find-files directory "\\.jar$"))
                                               (_ '()))
                                             inputs))
                      (classpath (string-join jars ":"))
                      (fileset   (map (lambda (jar)
                                        `(zipfileset (@ (excludes "META-INF/*.SF")
                                                        (src ,jar))))
                                      jars)))
                 (call-with-output-file "build.xml"
                   (lambda (port)
                     (sxml->xml
                      (pre-post-order
                       build.xml
                       `( ;; Remove all zipfileset tags from the "jar" tree and
                         ;; inject our own tags.
                         (jar . ,(lambda (tag . kids)
                                   `(jar ,@(append-map
                                            (filter (lambda (e)
                                                      (not (eq? 'zipfileset (car e)))))
                                            kids)
                                         ,@fileset)))
                         ;; Skip the "bundle" target (and the "-post-jar" target
                         ;; that depends on it), because we don't need it and it
                         ;; confuses sxml->xml.
                         (target . ,(lambda (tag . kids)
                                      (let ((name ((sxpath '(name *text*))
                                                   (car kids))))
                                        (if (or (member "bundle" name)
                                                (member "-post-jar" name))
                                            '() ; skip
                                            `(,tag ,@kids)))))
                         (*default*  . ,(lambda (tag . kids) `(,tag ,@kids)))
                         (*text*     . ,(lambda (_ txt)
                                          (match txt
                                            ;; Remove timestamp.
                                            ("${TODAY}" "(unknown)")
                                            (_ txt))))))
                      port)))

                 ;; Build it!
                 (invoke "ant"
                         (string-append "-Djavac.classpath=" classpath)
                         "-Dbuild.runtime=true"
                         "-Dbuild.time=01/01/1970 00:00:00"
                         "-Djavac.source=1.7"
                         "-Djavac.target=1.7"
                         (string-append "-Dtag.short.version="
                                        #$version)))))
           (replace 'install
             (lambda* (#:key inputs #:allow-other-keys)
               (let ((share (string-append #$output "/share/axoloti/")))
                 (install-file "dist/Axoloti.jar" share)

                 ;; We do this to ensure that this package retains references to
                 ;; other Java packages' jar files.
                 (install-file "build.xml" share)

                 ;; Create a launcher script
                 (mkdir (string-append #$output "/bin"))
                 (let ((target (string-append #$output "/bin/Axoloti")))
                   (with-output-to-file target
                     (lambda ()
                       (let* ((dir       (string-append #$output "/share/axoloti"))
                              (runtime   (search-input-directory inputs
                                                                 "share/axoloti"))
                              (toolchain (assoc-ref inputs "cross-toolchain"))
                              (includes  (string-append
                                          toolchain
                                          "/arm-none-eabi/include/:"
                                          toolchain
                                          "/arm-none-eabi/include/c++:"
                                          toolchain
                                          "/arm-none-eabi/include/c++/arm-none-eabi/armv7e-m")))
                         (display
                          (string-append "#!" (which "sh") "\n"
                                         "export CROSS_CPATH=" includes "\n"
                                         "export CROSS_CPLUS_INCLUDE_PATH=" includes "\n"
                                         "export CROSS_LIBRARY_PATH="
                                         toolchain "/arm-none-eabi/lib" "\n"
                                         (which "java")
                                         " -Daxoloti_release=" runtime
                                         " -Daxoloti_runtime=" runtime
                                         " -jar " dir "/Axoloti.jar")))))
                   (chmod target #o555)))))
           (add-after 'install 'strip-jar-timestamps
             (assoc-ref ant:%standard-phases 'strip-jar-timestamps)))))
    (inputs
     `(("icedtea" ,icedtea "jdk")
       ("cross-toolchain" ,(make-arm-none-eabi-nano-toolchain-4.9))
       ("java-simple-xml" ,java-simple-xml)
       ("java-rsyntaxtextarea" ,java-rsyntaxtextarea)
       ("java-usb4java" ,java-usb4java)
       ("java-jsch" ,java-jsch)
       ("java-slf4j-api" ,java-slf4j-api)
       ("java-jgit" ,java-jgit-4.2)
       ("axoloti-runtime" ,axoloti-runtime)))
    (native-inputs
     (list ant zip ; for repacking the jar
           unzip))
    (description
     "The Axoloti patcher offers a “patcher” environment similar to Pure Data
for sketching digital audio algorithms.  The patches run on a standalone
powerful microcontroller board: Axoloti Core.  This package provides the
patcher application.")))

(define-public ksoloti-runtime
  (let ((revision "0")
        (commit "b7ae4753b33532597db232285f4f3c1808f516b4"))
    (package
      (name "ksoloti-runtime")
      (version (git-version "1.1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/ksoloti/ksoloti")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0pf8zpzfx6nibwqrxbagpp3qpypfabshs7mign84dwsl9qdal1cv"))
         (modules '((guix build utils)))
         ;; Remove pre-built Java binaries.
         (snippet
          '(delete-file-recursively "lib/"))))
      (build-system gnu-build-system)
      (arguments
       (list
        #:tests? #f                       ; no check target
        #:modules '((guix build gnu-build-system)
                    (guix build utils)
                    (srfi srfi-1)
                    (srfi srfi-26)
                    (ice-9 match)
                    (ice-9 regex))
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'patch-paths
              (lambda* (#:key inputs #:allow-other-keys)
                ;; Remove source of non-determinism in ChibiOS
                (substitute* "chibios/os/various/shell.c"
                  (("#ifdef __DATE__") "#if 0"))

                (substitute* "firmware/compile_firmware.sh"
                  (("make -j8")
                   (string-append "make USE_VERBOSE_COMPILE=yes -j"
                                  (number->string (parallel-job-count)))))

                ;; Patch shell paths
                (substitute* '("src/main/java/qcmds/QCmdCompileFirmware.java"
                               "src/main/java/qcmds/QCmdCompilePatch.java"
                               "src/main/java/qcmds/QCmdFlashDFU.java")
                  (("/bin/sh") (which "sh")))

                ;; Override cross compiler base name
                (substitute* "firmware/Makefile.patch.mk"
                  (("arm-none-eabi-(gcc|g\\+\\+|objcopy|objdump|size)" tool)
                   (which tool)))

                ;; XXX: for some reason the whitespace substitution does not
                ;; work, so we disable it.
                (substitute* "firmware/Makefile.patch.mk"
                  (("^CHIBIOS +=.*") "CHIBIOS=${axoloti_firmware}/../chibios\n")
                  (("^BUILDDIR=.*") "BUILDDIR=${axoloti_libraries}/build\n"))

                ;; Hardcode full path to compiler tools
                (substitute* '("firmware/Makefile"
                               "firmware/flasher/Makefile"
                               "firmware/mounter/Makefile")
                  (("TRGT =.*")
                   (string-append "TRGT = "
                                  (assoc-ref inputs "cross-toolchain")
                                  "/bin/arm-none-eabi-\n")))

                ;; Hardcode path to "make"
                (substitute* '("firmware/compile_firmware.sh"
                               "firmware/compile_patch.sh")
                  (("make") (which "make")))

                ;; Hardcode path to "dfu-util"
                (substitute* "firmware/upload_fw_dfu.sh"
                  (("-f \"\\$\\{platformdir\\}/bin/dfu-util\"") "-z \"\"")
                  (("\\./dfu-util") (which "dfu-util")))))
            (delete 'configure)
            (replace 'build
              ;; Build Axoloti firmware with cross-compiler
              (lambda _
                (with-directory-excursion "firmware"
                  (substitute* "compile_firmware.sh"
                    (("`uname`") (string-append "`" (which "uname") "`")))
                  (invoke "sh" "compile_firmware.sh" "BOARD_KSOLOTI_CORE")
                  (invoke "sh" "compile_firmware.sh" "BOARD_AXOLOTI_CORE"))))
            (replace 'install
              (lambda* (#:key inputs #:allow-other-keys)
                (let* ((share (string-append #$output "/share/ksoloti/"))
                       (doc   (string-append share "doc"))
                       (dir   (getcwd))
                       (pats  '("/doc/[^/]+$"
                                "/patches/[^/]+/[^/]+$"
                                "/objects/[^/]+/[^/]+$"
                                "/firmware/.+"
                                "/platform_linux/.+"
                                "/chibios/[^/]+$"
                                "/chibios/boards/ST_STM32F4_DISCOVERY/[^/]+$"
                                "/chibios/(ext|os|docs)/.+"
                                "/CMSIS/[^/]+/[^/]+$"
                                "/patch/[^/]+/[^/]+$"
                                "/[^/]+\\.txt$"))
                       (pattern (string-append
                                 "(" (string-join
                                      (map (cut string-append dir <>)
                                           pats)
                                      "|") ")"))
                       (files   (find-files dir
                                            (lambda (file stat)
                                              (and (eq? 'regular (stat:type stat))
                                                   (string-match pattern file))))))
                  (for-each (lambda (file)
                              (install-file file
                                            (string-append
                                             share
                                             (regexp-substitute
                                              #f
                                              (string-match dir (dirname file))
                                              'pre  'post))))
                            files)))))))
      (inputs
       `( ;; for compiling patches
         ("make" ,gnu-make)
         ;; for compiling firmware
         ("cross-toolchain" ,(make-arm-none-eabi-nano-toolchain-9-2020-q2-update))
         ;; for uploading compiled patches and firmware
         ("dfu-util" ,dfu-util)))
      (home-page "https://ksoloti.github.io/")
      (synopsis "Audio development environment for the Ksoloti board")
      (description
       "Ksoloti is an environment for generating and processing digital audio.
It can be a programmable virtual modular synthesizer, polysynth, drone box,
sequencer, chord generator, multi effect, sample player, looper, granular
sampler, MIDI generator/processor, CV or trigger generator, anything in
between, and more.

The Ksoloti Core is a rework of the discontinued Axoloti Core board.  In
short, Ksoloti aims for maximum compatibility with the original Axoloti, but
with some layout changes and added features.

This package provides the runtime.")
      (license license:gpl3+))))

(define-public ksoloti-patcher
  (package
    (inherit ksoloti-runtime)
    (name "ksoloti-patcher")
    (version (package-version ksoloti-runtime))
    (arguments
     (list
      #:tests? #f                       ; no check target
      #:modules '((guix build gnu-build-system)
                  ((guix build ant-build-system) #:prefix ant:)
                  (guix build utils)
                  (srfi srfi-1)
                  (srfi srfi-26)
                  (ice-9 match)
                  (ice-9 regex)
                  (sxml simple)
                  (sxml xpath)
                  (sxml transform))
      #:imported-modules `((guix build ant-build-system)
                           ,@%default-gnu-imported-modules)
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (replace 'build
            (lambda* (#:key inputs #:allow-other-keys)
              (setenv "JAVA_HOME"
                      (dirname
                       (dirname (search-input-file inputs "/bin/javac"))))
              ;; We want to use our own jar files instead of the pre-built
              ;; stuff in lib.  So we replace the zipfileset tags in the
              ;; build.xml with new ones that reference our jars.
              (let* ((build.xml (with-input-from-file "build.xml"
                                  (lambda _
                                    (xml->sxml #:trim-whitespace? #t))))
                     (jars      (append-map (match-lambda
                                              (((? (cut string-prefix? "java-" <>)
                                                   label) . directory)
                                               (find-files directory "\\.jar$"))
                                              (_ '()))
                                            inputs))
                     (all-jars  (append jars (find-files "lib" "\\.jar$")))
                     (classpath (string-join all-jars ":"))
                     (fileset   (map (lambda (jar)
                                       `(zipfileset (@ (excludes "META-INF/*.SF")
                                                       (src ,jar))))
                                     all-jars)))
                (call-with-output-file "build.xml"
                  (lambda (port)
                    (sxml->xml
                     (pre-post-order
                      build.xml
                      `( ;; Remove all zipfileset tags from the "jar" tree and
                        ;; inject our own tags.
                        (jar . ,(lambda (tag . kids)
                                  `(jar ,@(append-map
                                           (filter (lambda (e)
                                                     (not (eq? 'zipfileset (car e)))))
                                           kids)
                                        ,@fileset)))
                        ;; Skip the "bundle" target (and the "-post-jar" target
                        ;; that depends on it), because we don't need it and it
                        ;; confuses sxml->xml.
                        (target . ,(lambda (tag . kids)
                                     (let ((name ((sxpath '(name *text*))
                                                  (car kids))))
                                       (if (or (member "bundle" name)
                                               (member "-post-jar" name))
                                           '() ; skip
                                           `(,tag ,@kids)))))
                        (*default*  . ,(lambda (tag . kids) `(,tag ,@kids)))
                        (*text*     . ,(lambda (_ txt)
                                         (match txt
                                           ;; Remove timestamp.
                                           ("${TODAY}" "(unknown)")
                                           (_ txt))))))
                     port)))

                ;; Build it!
                (invoke "ant"
                        (string-append "-Djavac.classpath=" classpath)
                        "-Dbuild.runtime=true"
                        "-Dbuild.time=01/01/1970 00:00:00"
                        "-Djavac.source=1.8"
                        "-Djavac.target=1.8"
                        (string-append "-Dtag.short.version="
                                       #$version)))))
          (replace 'install
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((share (string-append #$output "/share/ksoloti/")))
                (install-file "dist/Ksoloti.jar" share)

                ;; We do this to ensure that this package retains references to
                ;; other Java packages' jar files.
                (install-file "build.xml" share)

                ;; Create a launcher script
                (mkdir (string-append #$output "/bin"))
                (let ((target (string-append #$output "/bin/Ksoloti")))
                  (with-output-to-file target
                    (lambda ()
                      (let* ((dir       (string-append #$output "/share/ksoloti"))
                             (runtime   (search-input-directory inputs
                                                                "share/ksoloti"))
                             (toolchain (assoc-ref inputs "cross-toolchain"))
                             (libstdc++ (assoc-ref inputs "libstdc++"))
                             (includes  (string-append
                                         toolchain
                                         "/arm-none-eabi/include/c++:"
                                         libstdc++
                                         "/arm-none-eabi/include/c++/arm-none-eabi/thumb/v7-m/nofp:"
                                         toolchain
                                         "/arm-none-eabi/include/"))
                             (marlin.jar
                              (search-input-file inputs "/share/java/marlin.jar")))
                        (display
                         (string-append "#!" (which "sh") "\n"
                                        "export PATH=" toolchain "/bin:$PATH\n"
                                        "export CROSS_CPLUS_INCLUDE_PATH=" includes "\n"
                                        "export CROSS_LIBRARY_PATH="
                                        toolchain "/arm-none-eabi/lib" "\n"
                                        (which "java")
                                        " -Xbootclasspath/a:" marlin.jar
                                        " --add-exports=java.desktop/sun.java2d.pipe=ALL-UNNAMED"
                                        " --add-exports=java.base/sun.security.action=ALL-UNNAMED"
                                        " -Dsun.java2d.renderer=org.marlin.pisces.MarlinRenderingEngine"
                                        " -Dsun.java2d.d3d=false"
                                        " -Dsun.java2d.dpiaware=true"
                                        " -Daxoloti_platform=" runtime "/platform_linux"
                                        " -Daxoloti_firmware=" runtime "/firmware"
                                        " -jar " dir "/Ksoloti.jar")))))
                  (chmod target #o555)))))
          (add-after 'install 'strip-jar-timestamps
            (assoc-ref ant:%standard-phases 'strip-jar-timestamps)))))
    (inputs
     `(("openjdk" ,openjdk17 "jdk")
       ("cross-toolchain" ,(make-arm-none-eabi-nano-toolchain-9-2020-q2-update))
       ("java-autocomplete" ,java-autocomplete)
       ("java-flatlaf" ,java-flatlaf)
       ("java-flatlaf-intellij-themes" ,java-flatlaf-intellij-themes)
       ("java-jgit" ,java-jgit-4.2)
       ("java-jsch" ,java-jsch)
       ("java-marlin-renderer" ,java-marlin-renderer)
       ("java-rsyntaxtextarea" ,java-rsyntaxtextarea)
       ("java-simple-xml" ,java-simple-xml)
       ("java-usb4java" ,java-usb4java)
       ("java-slf4j-api" ,java-slf4j-api)
       ("ksoloti-runtime" ,ksoloti-runtime)))
    (native-inputs
     (list ant zip                      ;for repacking the jar
           unzip))
    (description
     "Ksoloti is an environment for generating and processing digital audio.
It can be a programmable virtual modular synthesizer, polysynth, drone box,
sequencer, chord generator, multi effect, sample player, looper, granular
sampler, MIDI generator/processor, CV or trigger generator, anything in
between, and more.

The Ksoloti Core is a rework of the discontinued Axoloti Core board.  In
short, Ksoloti aims for maximum compatibility with the original Axoloti, but
with some layout changes and added features.

This package provides the patcher application.")))

(define-public axoloti-patcher-next
  (deprecated-package "axoloti-patcher-next" ksoloti-patcher))
