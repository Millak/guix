;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019, 2024 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2023 Frank Pursel <frank.pursel@gmail.com>
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

(define-module (gnu packages java-graphics)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix modules)
  #:use-module (guix utils)
  #:use-module (guix build-system ant)
  #:use-module (guix build utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages batik)
  #:use-module (gnu packages java)
  #:use-module (gnu packages java-xml)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (ice-9 match))

(define-public ditaa
  (package
    (name "ditaa")
    (version "0.11.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/stathissideris/ditaa")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1y3g17wp1wvb05m56pp25avww2njpgh0gk0jsbsf25scj7hyyj26"))
              (modules '((guix build utils)))
              (snippet '(begin
                          (format #t "~%~a~%"
                                  "Finding and removing embedded jars.")
                          (for-each (lambda (jarf)
                                      (delete-file jarf)
                                      (format #t "Deleted: ~a~%" jarf))
                                    (find-files "." "\\.jar$"))))))
    (build-system ant-build-system)
    (inputs (list bash-minimal))
    (native-inputs (list
                    java-commons-cli
                    java-jericho-html
                    java-junit
                    java-libbatik
                    java-w3c-svg))
    (arguments
     `(#:build-target "release-all"
       #:phases (modify-phases %standard-phases
                  ;; Ant's buildfile and build tree need to be modified
                  ;; to provide access to the guix builds of the
                  ;; batik and the java-commons-cli
                  ;; jar files.  Also some of the source requires java7.
                  (add-before 'build 'build-prep
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let* ((batik-jar
                              (search-input-file
                               inputs
                               "share/java/batik.jar"))
                             (commons-cli-jar
                              (search-input-file
                               inputs
                               "lib/m2/commons-cli/commons-cli/1.4/commons-cli-1.4.jar")))
                        (mkdir-p "lib")
                        (copy-file batik-jar "./lib/batik.jar")
                        (copy-file commons-cli-jar "./lib/commons-cli.jar"))
                      (with-directory-excursion "build"
                        (substitute* "release.xml"
                          (("source=\"1.6\"")
                           "source=\"7\"")
                          (("<file name=\"commons-cli-1.2.jar\"/>")
                           (string-append "<file name=\"commons-cli.jar\"/>"
                                          "\n" "<file name=\"batik.jar\"/>"))))))
                  (replace 'build
                    (lambda* _
                      (setenv "ANT_OPTS"
                              (string-append "-Dversion.string="
                                             ,version))
                      (with-directory-excursion "build"
                        (invoke "ant" "-f" "release.xml" "release-jar"))))
                  (replace 'check
                    (lambda* (#:key tests? #:allow-other-keys)
                      (if tests?
                          (begin
                            (setenv "ANT_OPTS"
                                    (string-append "-Dversion.string="
                                                   ,version))
                            (mkdir-p "tests/testlib")
                            (with-directory-excursion "build"
                              (invoke "ant" "-f" "release.xml"
                                      "generate-test-images")
                              (invoke "ant" "test"))) #f)))
                  (replace 'install
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (lib (string-append out "/lib"))
                             (bin (string-append out "/bin"))
                             (bash (search-input-file inputs "bin/bash"))
                             (java (search-input-file inputs "bin/java"))
                             (jre (search-input-directory inputs "jre"))
                             (ditaa (string-append out "/bin/ditaa"))
                             (jar-name (string-append ,name
                                                      ,version ".jar")))
                        (with-directory-excursion "releases"
                          (install-file jar-name lib))
                        (mkdir-p bin)
                        (with-output-to-file ditaa
                          (lambda _
                            (format #t
                                    "#!~a~%JAVA_HOME=~a ~a -jar ~a/~a $@~%"
                                    bash
                                    jre
                                    java
                                    lib
                                    jar-name)))
                        (chmod ditaa #o755))))
                  (add-after 'install 'install-docs
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((doc (string-append (assoc-ref outputs "out")
                                                "/share/doc/")))
                        (for-each (lambda (filen)
                                    (install-file filen doc))
                                  (find-files "." ".*README\\.md"))))))))
    (home-page "https://github.com/stathissideris/ditaa")
    (synopsis "Create graphics from ascii art")
    (description
     "ditaa is a small command-line utility that converts diagrams drawn using
ascii art drawings that contain characters that resemble lines like @samp{|}
@samp{/} @samp{-}), into proper bitmap graphics.")
    (license license:lgpl3)))

(define-public java-flatlaf
  (package
    (name "java-flatlaf")
    (version "3.4.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/JFormDesigner/FlatLaf")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1qr7g8s5m89q5k31mnxk18lflz3l2hid4i055mim3b0m4vjs40pi"))
              (modules '((guix build utils)))
              (snippet
               '(for-each
                 delete-file
                 (find-files "flatlaf-core/src/main/resources/com/formdev/flatlaf/natives/"
                             ".*")))))
    (build-system ant-build-system)
    (arguments
     (list
      #:tests? #false                   ;XXX requires junit5
      #:jar-name "flatlaf.jar"
      #:source-dir '(list "flatlaf-core/src/main/java")
      #:test-dir '(list "flatlaf-core/src/test")
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'configure 'build-native-code
            (lambda* (#:key inputs #:allow-other-keys)
              (let* ((jdk (assoc-ref inputs "jdk"))
                     (suffix #$(match (%current-system)
                                 ("i686-linux" "i686")
                                 ("armhf-linux" "armhf")
                                 ("aarch64-linux" "aarch64")
                                 ("x86_64-linux" "x86_64")
                                 (_ "generic")))
                     (lib #$(match (%current-system)
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
                                 (%current-system) 6)))))
                     (filename
                      (string-append "libflatlaf-linux-" suffix ".so"))
                     (target-dir
                      (string-append (getcwd)
                                     "/flatlaf-core/src/main/resources/com/formdev/flatlaf/natives")))
                (with-directory-excursion "flatlaf-natives/flatlaf-natives-linux/"
                  (invoke "gcc" "-shared" "-O3" "-fPIC"
                          "src/main/cpp/ApiVersion.cpp"
                          "src/main/cpp/X11WmUtils.cpp"
                          "-Isrc/main/headers"
                          "-ljawt"
                          (string-append "-L" jdk lib)
                          (string-append "-I" jdk "/include")
                          (string-append "-I" jdk "/include/linux")
                          "-o" filename)
                  (install-file filename target-dir)))))
          (add-before 'build 'copy-resources
            (lambda _
              (copy-recursively "flatlaf-core/src/main/resources"
                                "build/classes"))))))
    (inputs (list libx11 libxt))
    (home-page "https://www.formdev.com/flatlaf/")
    (synopsis "Flat Look and Feel for Java Swing applications")
    (description "FlatLaf is a cross-platform Look and Feel for Java Swing
desktop applications.  It looks almost flat (no shadows or gradients), clean,
simple and elegant.  FlatLaf comes with Light, Dark, IntelliJ and Darcula
themes, scales on HiDPI displays and runs on Java 8 or newer.

The look is heavily inspired by Darcula and IntelliJ themes from IntelliJ IDEA
2019.2+ and uses almost the same colors and icons.")
    (license license:asl2.0)))

(define-public java-flatlaf-intellij-themes
  (package
    (inherit java-flatlaf)
    (name "java-flatlaf-intellij-themes")
    (arguments
     (list
      #:tests? #false                   ;there are none
      #:jar-name "flatlaf-intellij-themes.jar"
      #:source-dir '(list "flatlaf-intellij-themes/src/main/java")
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'copy-resources
            (lambda _
              (copy-recursively "flatlaf-intellij-themes/src/main/resources"
                                "build/classes"))))))
    (inputs (list java-flatlaf))
    (synopsis "FlatLaf addon with popular themes")
    (description "This addon for FlatLaf bundles many popular third party
themes from JetBrains Plugins Repository into a JAR and provides Java classes
to use them.")))

(define-public java-piccolo2d-core
  (package
    (name "java-piccolo2d-core")
    (version "3.0.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/piccolo2d/piccolo2d.java")
                    (commit (string-append "piccolo2d-complete-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1k6gw643k83516lcw04mgac2yi75phdrng44pz9xk6hz066ip21s"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "piccolo2d-core.jar"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _ (chdir "core") #t)))))
    (inputs
     (list java-junit))
    (home-page "http://piccolo2d.org")
    (synopsis "Structured 2D graphics framework")
    (description "Piccolo2D is a framework (in the Jazz ZUI tradition) to
create robust, full-featured graphical applications in Java, with features
such as zooming and multiple representation.  This package provides the core
libraries.")
    (license license:bsd-3)))

(define-public java-piccolo2d-extras
  (package (inherit java-piccolo2d-core)
    (name "java-piccolo2d-extras")
    (arguments
     `(#:jar-name "piccolo2d-extras.jar"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _ (chdir "extras") #t))
         (add-after 'chdir 'remove-failing-test
           (lambda _
             ;; TODO: These both fail with "Unable to convolve src image"
             (delete-file "src/test/java/org/piccolo2d/extras/nodes/PShadowTest.java")
             (delete-file "src/test/java/org/piccolo2d/extras/util/ShadowUtilsTest.java")
             #t))
         (add-before 'check 'start-xorg-server
           (lambda* (#:key inputs #:allow-other-keys)
             ;; The test suite requires a running X server.
             (system "Xvfb :1 -screen 0 640x480x24 &")
             (setenv "DISPLAY" ":1")
             #t)))))
    (inputs
     (list java-piccolo2d-core java-junit))
    (native-inputs
     (list xorg-server)) ; for tests
    (description "Piccolo2D is a framework (in the Jazz ZUI tradition) to
create robust, full-featured graphical applications in Java, with features
such as zooming and multiple representation.  This package provides additional
features not found in the core libraries.")))

(define-public java-marlin-renderer
  (package
    (name "java-marlin-renderer")
    (version "0.9.4.8")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/bourgesl/marlin-renderer")
                    (commit (string-append "v" (string-map (match-lambda
                                                             (#\. #\_)
                                                             (c c))
                                                           version)))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0gcqp9iq0j5n08gdssh8gp0daj3n5zrx0dll1l4ljhbj2b9jm9ym"))))
    (build-system ant-build-system)
    (arguments
     (list
      #:jar-name "marlin.jar"
      #:test-include '(list "src/test/java/RunJUnitTest.java")
      #:phases
      '(modify-phases %standard-phases
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "src/main/resources" "build/classes"))))))
    (inputs
     (list java-hamcrest-core java-junit))
    (home-page "https://github.com/bourgesl/marlin-renderer/")
    (synopsis "Rendering engine")
    (description "Marlin is a Java2D @code{RenderingEngine} optimized for
performance (improved memory usage and footprint, better multi-threading) and
better visual quality based on OpenJDK's @code{pisces} implementation.  It
handles shape rendering (@code{Graphics2D} @code{draw(Shape)} /
@code{fill(Shape)} with stroke and dash attributes.")
    ;; With Classpath Exception
    (license license:gpl2)))
