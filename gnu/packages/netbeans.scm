;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2026 Danny Milosavljevic <dannym@friendly-machines.com>
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

(define-module (gnu packages netbeans)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix build-system ant)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages java)
  #:use-module (gnu packages java-graphics)
  #:use-module (gnu packages java-xml)
  #:use-module (gnu packages web))

(define-public java-jemmy-2.3.1.1
  (package
    (name "java-jemmy")
    (version "2.3.1.1")
    (source
     (origin
       (method url-fetch)
       ;; This is the exact source payload referenced by NetBeans' own
       ;; harness/jemmy/external/binaries-list.
       (uri (string-append
             "https://netbeans.osuosl.org/binaries/"
             "49197106637CCA8C337AF16CC01BB5D9DEC7E179-jemmy-"
             version "-src.zip"))
       (sha256
        (base32 "183qppmxlnzdniw2qg92ng9g42hyn2823gbnkjxrps4kdy9blwz1"))))
    (build-system ant-build-system)
    (arguments
     (list
      #:jdk openjdk17
      #:tests? #f
      #:modules
      '((guix build utils)
        (guix build ant-build-system)
        (guix build java-utils)
        (srfi srfi-13))
      #:phases
      #~(modify-phases %standard-phases
          (replace 'build
            (lambda _
              (let ((source-files (find-files "." "\\.java$")))
                (mkdir-p "build/classes")
                (for-each
                 (lambda (file)
                   (unless (string-suffix? ".java" file)
                     (let ((destination
                            (string-append "build/classes/" file)))
                       (mkdir-p (dirname destination))
                       (copy-file file destination))))
                 (find-files "." ".*"))
                (apply invoke "javac" "-d" "build/classes" source-files)
                (invoke "jar" "-cf" "build/jemmy-2.3.1.1.jar"
                        "-C" "build/classes" ".")
                (apply invoke "javadoc"
                       "-quiet"
                       "-Xdoclint:none"
                       "-d" "build/doc"
                       source-files)
                (with-directory-excursion "build/doc"
                  (invoke "zip" "-qrX" "../jemmy-2.3.1.1-doc.zip" "."))
                (invoke "zip" "-qrX" "build/jemmy-2.3.1.1-src.zip" ".")
                #t)))
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((share-java
                     (string-append (assoc-ref outputs "out")
                                    "/share/java")))
                (mkdir-p share-java)
                (for-each
                 (lambda (file)
                   (install-file file share-java))
                 '("build/jemmy-2.3.1.1.jar"
                   "build/jemmy-2.3.1.1-doc.zip"
                   "build/jemmy-2.3.1.1-src.zip"))
                #t))))))
    (home-page "https://web.archive.org/web/20120822233211/http://java.net/projects/jemmy")
    (synopsis "Jemmy Swing UI testing library")
    (description
     "Jemmy is a Java library for automating and testing Swing and AWT user
interfaces.")
    (native-inputs
     (list unzip))
    (license license:cddl1.0)))

(define java-apache-xml-commons-resolver-netbeans-1.2
  (package
    (inherit java-apache-xml-commons-resolver)
    (name "java-apache-xml-commons-resolver-netbeans")
    (source
     (origin
       (inherit (package-source java-apache-xml-commons-resolver))
       (patches
        (search-patches
         "java-apache-xml-commons-resolver-1.2-netbeans.patch"))))))

(define-public netbeans
  (package
    (name "netbeans")
    (version "25")
    (source
     (origin
       (method url-fetch)
       (uri "https://archive.apache.org/dist/netbeans/netbeans/25/netbeans-25-source.zip")
       (sha256
        (base32 "10w1jjg722k7y4dqcazi7q07zykqhs8rzbvdrnlm0b1cpf5qr0m1"))
       (patches
        (search-patches "netbeans-25-source-only-build.patch"
                        "netbeans-25-nativeexecution-paths.patch"
                        "netbeans-25-terminal-no-login-shell.patch"
                        "netbeans-25-wayland-font-rendering.patch"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; The official source archive is clean apart from source-adjacent
           ;; build payloads; remove them and rebuild what NetBeans needs.
           (for-each delete-file
                     (append (find-files "." "\\.jar$")
                             (find-files "." "\\.class$")
                             (find-files "." "\\.zip$")
                             (find-files "." "\\.exe$")
                             (find-files "." "\\.dll$")
                             (find-files "." "\\.so$")
                             (find-files "." "\\.so\\.")
                             (find-files "." "\\.dylib$")
                             (find-files "." "\\.jnilib$")))
           #t))))
    (build-system ant-build-system)
    (arguments
     (list
      #:jdk openjdk17
      #:ant ant/java8-empty-etc
      #:tests? #f
      #:modules
      '((guix build utils)
        (guix build ant-build-system)
        (ice-9 match)
        (ice-9 rdelim)
        (srfi srfi-1)
        (srfi srfi-13))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'prepare-build
            (lambda* (#:key inputs #:allow-other-keys)
              (define platform-modules
                '("api.htmlui"
                  "applemenu"
                  "autoupdate.cli"
                  "autoupdate.services"
                  "autoupdate.ui"
                  "core.nativeaccess"
                  "core.network"
                  "htmlui"
                  "javahelp"
                  "junitlib"
                  "libs.batik.read"
                  "libs.javafx"
                  "libs.javax.inject"
                  "libs.junit4"
                  "libs.junit5"
                  "libs.testng"
                  "masterfs.macosx"
                  "masterfs.windows"
                  "net.java.html"
                  "net.java.html.boot"
                  "net.java.html.boot.fx"
                  "net.java.html.boot.script"
                  "net.java.html.geo"
                  "net.java.html.json"
                  "net.java.html.sound"
                  "netbinox"
                  "o.apache.commons.commons_io"
                  "o.apache.commons.lang3"
                  "o.apache.commons.logging"
                  "o.n.html.ko4j"
                  "o.n.html.presenters.spi"
                  "o.n.html.xhr4j"
                  "templatesui"))
              (define ide-modules
                '("bugtracking"
                  "bugtracking.bridge"
                  "bugtracking.commons"
                  "bugzilla"
                  "c.google.gson"
                  "c.google.guava"
                  "c.google.guava.failureaccess"
                  "core.browser.webview"
                  "css.editor"
                  "css.lib"
                  "css.model"
                  "css.prep"
                  "css.visual"
                  "db"
                  "db.core"
                  "db.dataview"
                  "db.drivers"
                  "db.kit"
                  "db.metadata.model"
                  "db.mysql"
                  "db.sql.editor"
                  "db.sql.visualeditor"
                  "dbapi"
                  "derby"
                  "docker.api"
                  "docker.editor"
                  "docker.ui"
                  "go.lang"
                  "html"
                  "html.custom"
                  "html.editor"
                  "html.editor.lib"
                  "html.indexing"
                  "html.lexer"
                  "html.parser"
                  "html.validation"
                  "httpserver"
                  "hudson"
                  "hudson.git"
                  "hudson.mercurial"
                  "hudson.subversion"
                  "hudson.tasklist"
                  "hudson.ui"
                  "javascript2.debug"
                  "javascript2.debug.ui"
                  "languages.go"
                  "languages.hcl"
                  "languages.toml"
                  "languages.yaml"
                  "lexer.antlr4"
                  "libs.antlr3.runtime"
                  "libs.antlr4.runtime"
                  "libs.commons_compress"
                  "libs.commons_net"
                  "libs.flexmark"
                  "libs.freemarker"
                  "libs.graalsdk"
                  "libs.graalsdk.system"
                  "libs.ini4j"
                  "libs.jaxb"
                  "libs.jcodings"
                  "libs.json_simple"
                  "libs.snakeyaml_engine"
                  "libs.svnClientAdapter"
                  "libs.svnClientAdapter.javahl"
                  "libs.tomlj"
                  "libs.tomljava"
                  "libs.truffleapi"
                  "libs.xerces"
                  "localtasks"
                  "lsp.client"
                  "markdown"
                  "mercurial"
                  "mylyn.util"
                  "o.apache.commons.httpclient"
                  "o.apache.commons.lang"
                  "o.apache.ws.commons.util"
                  "o.apache.xmlrpc"
                  "o.eclipse.core.contenttype"
                  "o.eclipse.core.jobs"
                  "o.eclipse.core.net"
                  "o.eclipse.core.runtime"
                  "o.eclipse.core.runtime.compatibility.auth"
                  "o.eclipse.equinox.app"
                  "o.eclipse.equinox.common"
                  "o.eclipse.equinox.preferences"
                  "o.eclipse.equinox.registry"
                  "o.eclipse.equinox.security"
                  "o.eclipse.jgit.lfs"
                  "o.eclipse.mylyn.bugzilla.core"
                  "o.eclipse.mylyn.commons.core"
                  "o.eclipse.mylyn.commons.net"
                  "o.eclipse.mylyn.commons.repositories.core"
                  "o.eclipse.mylyn.commons.xmlrpc"
                  "o.eclipse.mylyn.tasks.core"
                  "o.eclipse.mylyn.wikitext.confluence.core"
                  "o.eclipse.mylyn.wikitext.core"
                  "o.eclipse.mylyn.wikitext.markdown.core"
                  "o.eclipse.mylyn.wikitext.textile.core"
                  "selenium2"
                  "selenium2.server"
                  "servletapi"
                  "spellchecker"
                  "spellchecker.bindings.htmlxml"
                  "spellchecker.bindings.properties"
                  "spellchecker.dictionary_en"
                  "spellchecker.kit"
                  "subversion"
                  "team.ide"
                  "textmate.lexer"
                  "usersguide"
                  "versioning.system.cvss.installer"
                  "web.browser.api"
                  "web.common"
                  "web.common.ui"
                  "web.indent"
                  "web.webkit.debugging"
                  "xml.jaxb.api"
                  "xml.tax"
                  "xml.text.obsolete90"
                  "xml.tools"
                  "xml.wsdl.model"
                  "xsl"))
              (define extide-modules
                '("gradle"
                  "gradle.dists"
                  "gradle.editor"
                  "libs.gradle"))
              (define thirdparty-modules
                '("libs.javafx.linux"
                  "libs.javafx.linux.aarch64"
                  "libs.javafx.macosx"
                  "libs.javafx.macosx.aarch64"
                  "libs.javafx.win"))
              (define harness-modules
                '("jellytools.platform"
                  "jemmy"
                  "libs.nbi.ant"
                  "libs.nbi.engine"
                  "nbjunit"
                  "o.n.insane"))
              (define nb-modules
                '("autoupdate.pluginimporter"
                  "bugzilla.exceptionreporter"
                  "updatecenters"))
              (define cluster-references
                '("nb.cluster.3rdparty"
                  "nb.cluster.javafx"))
              (define (read-lines file)
                (call-with-input-file file
                  (lambda (port)
                    (let loop ((lines '()))
                      (let ((line (read-line port)))
                        (if (eof-object? line)
                            (reverse lines)
                            (loop (cons line lines))))))))
              (define (write-lines file lines)
                (call-with-output-file file
                  (lambda (port)
                    (for-each
                     (lambda (line)
                       (display line port)
                       (newline port))
                     lines))))
              (define (filter-lines file pred)
                (write-lines file (filter pred (read-lines file))))
              (define (drop-lines-containing file patterns)
                (filter-lines
                 file
                 (lambda (line)
                   (not (any (lambda (pattern)
                               (string-contains line pattern))
                             patterns)))))
              (define (append-string file text)
                (let ((port (open-file file "a")))
                  (display text port)
                  (close-port port)))
              (define (module-line? line module)
                (let ((trimmed (string-trim-both line)))
                  (or (string=? trimmed module)
                      (string=? trimmed (string-append module ",\\")))))
              (define (drop-module-lines file modules)
                (filter-lines
                 file
                 (lambda (line)
                   (not (any (lambda (module) (module-line? line module))
                             modules)))))
              (define (indented-line? line)
                (and (not (string-null? line))
                     (let ((c (string-ref line 0)))
                       (or (char=? c #\space)
                           (char=? c #\tab)))))
              (define (strip-list-terminator line)
                (cond
                 ((string-suffix? ",\\" line)
                  (substring line 0 (- (string-length line) 2)))
                 ((string-suffix? "," line)
                  (substring line 0 (- (string-length line) 1)))
                 (else line)))
              (define (trim-trailing-list-commas file)
                (write-lines
                 file
                 (let loop ((rest (read-lines file))
                            (out '()))
                   (if (null? rest)
                       (reverse out)
                       (let* ((line (car rest))
                              (next (and (pair? (cdr rest))
                                         (cadr rest)))
                              (fixed
                               (if (and (indented-line? line)
                                        (or (string-suffix? ",\\" line)
                                            (string-suffix? "," line))
                                        (or (not next)
                                            (string-null? next)
                                            (not (indented-line? next))))
                                   (strip-list-terminator line)
                                   line)))
                         (loop (cdr rest) (cons fixed out)))))))
              (define (remove-dependency-blocks file modules)
                (let loop ((rest (read-lines file))
                           (out '()))
                  (if (null? rest)
                      (write-lines file out)
                      (let ((line (car rest)))
                        (if (string-contains line "<dependency>")
                            (let dep-loop ((rest (cdr rest))
                                           (block (list line)))
                              (if (null? rest)
                                  (error "unterminated dependency block" file)
                                  (let ((next (car rest))
                                        (tail (cdr rest)))
                                    (if (string-contains next "</dependency>")
                                        (let* ((full-block (append block (list next)))
                                               (drop?
                                                (any (lambda (module)
                                                       (any (lambda (entry)
                                                              (string-contains
                                                               entry
                                                               (string-append
                                                                "<code-name-base>"
                                                                module
                                                                "</code-name-base>")))
                                                            full-block))
                                                     modules)))
                                          (loop tail
                                                (if drop?
                                                    out
                                                    (append out full-block))))
                                        (dep-loop tail
                                                  (append block (list next)))))))
                            (loop (cdr rest) (append out (list line))))))))
              (define (symlink-input-file input pattern destination)
                (match (find-files (assoc-ref inputs input) pattern)
                  ((file)
                   (mkdir-p (dirname destination))
                   (symlink file destination))
                  (()
                   (error "symlink-input-file: no match for" pattern
                          "in input" input))
                  (files
                   (error "symlink-input-file: multiple matches for" pattern
                          "in input" input files))))
              (define (make-ant-binary-zip)
                (let* ((ant-root (assoc-ref inputs "ant"))
                       (source-root (getcwd))
                       (work-dir (string-append (getcwd) "/build-support"))
                       (dist-dir (string-append work-dir "/apache-ant-1.10.14"))
                       (zip-file (string-append source-root
                                                "/extide/o.apache.tools.ant.module/external/apache-ant-1.10.14-bin.zip")))
                  (mkdir-p work-dir)
                  (for-each
                   (lambda (subdir)
                     (let ((source (string-append ant-root "/" subdir)))
                       (when (file-exists? source)
                         (copy-recursively source
                                           (string-append dist-dir "/" subdir)))))
                   '("bin" "etc" "lib"))
                  (mkdir-p (dirname zip-file))
                  (with-directory-excursion work-dir
                    (invoke "zip" "-qrX" zip-file
                            "apache-ant-1.10.14"))))
              (define (make-nativeexecution-binary-zip)
                (let* ((source-root (getcwd))
                       (tools-root (string-append source-root
                                                  "/ide/dlight.nativeexecution/tools"))
                       (binary-root (string-append source-root
                                                   "/ide/dlight.nativeexecution/release/bin/nativeexecution"))
                       (zip-root (string-append source-root
                                                "/build-support/nativeexecution"))
                       (platform-root (string-append zip-root "/Linux-x86_64"))
                       (zip-file (string-append source-root
                                                "/ide/dlight.nativeexecution/external/exechlp-1.2.zip"))
                       (make (or (which "gmake") (which "make"))))
                  (mkdir-p platform-root)
                  (with-directory-excursion tools-root
                    (for-each
                     (lambda (dir)
                       (with-directory-excursion dir
                         (invoke make "CONF=Linux-x86_64" "clean")
                         (invoke make "CONF=Linux-x86_64" "all")))
                     '("." "pty" "killall" "unbuffer")))
                  (for-each
                   (lambda (spec)
                     (copy-file (car spec) (cdr spec)))
                   `((,(string-append binary-root "/Linux-x86_64/process_start")
                      . ,(string-append platform-root "/process_start"))
                     (,(string-append binary-root "/Linux-x86_64/pty_open")
                      . ,(string-append platform-root "/pty_open"))
                     (,(string-append binary-root "/Linux-x86_64/sigqueue")
                      . ,(string-append platform-root "/sigqueue"))
                     (,(string-append binary-root "/Linux-x86_64/stat")
                      . ,(string-append platform-root "/stat"))
                     (,(string-append tools-root
                                      "/pty/dist/Linux-x86_64/pty")
                      . ,(string-append platform-root "/pty"))
                     (,(string-append tools-root
                                      "/killall/dist/Linux-x86_64/killall")
                      . ,(string-append platform-root "/killall"))
                     (,(string-append tools-root
                                      "/unbuffer/dist/Linux-x86_64/unbuffer.so")
                      . ,(string-append platform-root "/unbuffer.so"))))
                  (mkdir-p (dirname zip-file))
                  (with-directory-excursion zip-root
                    (invoke "zip" "-qrX" zip-file "Linux-x86_64"))))
              (let* ((cluster-properties
                      (find file-exists?
                            '("nbbuild/cluster.properties"
                              "../nbbuild/cluster.properties"
                              "../../nbbuild/cluster.properties"
                              "source/nbbuild/cluster.properties"
                              "../source/nbbuild/cluster.properties"
                              "source/source/nbbuild/cluster.properties")))
                     (source-root
                      (and cluster-properties
                           (dirname (dirname cluster-properties)))))
                (unless source-root
                  (error "unable to locate NetBeans source root"))
                (chdir source-root))
              (setenv "HOME" (getcwd))
              (setenv "JAVA_HOME" (assoc-ref inputs "jdk"))
              (setenv "PATH"
                      (string-append (assoc-ref inputs "ant") "/bin:"
                                     (assoc-ref inputs "jdk") "/bin:"
                                     (assoc-ref inputs "zip") "/bin:"
                                     (getenv "PATH")))

              ;; Never download or rely on upstream-prebuilt payloads.
              (for-each
               (lambda (file)
                 (call-with-output-file file
                   (lambda (port)
                     (display "" port))))
               (append (find-files "." "binaries-list$")
                       (find-files "." "binariesembedded-list$")))

              ;; The git module declares a hard requirement on javahelp,
              ;; which needs an external binary (jhall-2.0_05.jar).
              ;; Remove the requirement so git loads without help support.
              (substitute* "ide/git/manifest.mf"
                (("OpenIDE-Module-Requires: org\\.netbeans\\.api\\.javahelp\\.Help")
                 "OpenIDE-Module-Requires: "))

              ;; The extbrowser module references Windows DDE DLLs from
              ;; an external zip.  Remove those lines so the build does
              ;; not try to extract the zip.
              (substitute* "ide/extbrowser/nbproject/project.properties"
               (("(release\\.external/extbrowser-dlls-[^!]*!/extbrowser\\.dll)=modules/lib/extbrowser\\.dll" _ p)
                (string-append "#" p))
               (("(release\\.external/extbrowser-dlls-[^!]*!/extbrowser64\\.dll)=modules/lib/extbrowser64\\.dll" _ p)
                (string-append "#" p)))

              ;; JNA maps Native.load("c", ...) to dlopen("libc.so"),
              ;; but on Guix libc.so is a linker script that dlopen
              ;; cannot handle.  Use "libc.so.6" which dlopen finds
              ;; directly since the JVM is linked against it.
              (substitute* "platform/masterfs.linux/src/org/netbeans/modules/masterfs/watcher/linux/LinuxNotifier.java"
                (("Native\\.load\\(\"c\"")
                 "Native.load(\"libc.so.6\"")
                (("NativeLibrary\\.getInstance\\(\"c\"\\)")
                 "NativeLibrary.getInstance(\"libc.so.6\")"))

              ;; Patch libsecret backend to use absolute library paths
              ;; so JNA can find them on Guix without ld.so.cache.
              (let ((libsecret (assoc-ref inputs "libsecret"))
                    (glib (assoc-ref inputs "glib")))
                (substitute* "platform/keyring.impl/src/org/netbeans/modules/keyring/gnome/libsecret/LibSecret.java"
                  (("Native\\.load\\(\"secret-1\"")
                   (string-append "Native.load(\"" libsecret "/lib/libsecret-1.so\"")))
                (substitute* "platform/keyring.impl/src/org/netbeans/modules/keyring/gnome/libsecret/Glib.java"
                  (("Native\\.load\\(\"glib-2.0\"")
                   (string-append "Native.load(\"" glib "/lib/libglib-2.0.so\"")))
                (substitute* "platform/keyring.impl/src/org/netbeans/modules/keyring/gnome/libsecret/Gio.java"
                  (("Native\\.load\\(\"gio-2.0\"")
                   (string-append "Native.load(\"" glib "/lib/libgio-2.0.so\""))))

              (drop-module-lines "nbbuild/cluster.properties" platform-modules)
              (drop-module-lines "nbbuild/cluster.properties" ide-modules)
              (drop-module-lines "nbbuild/cluster.properties" extide-modules)
              (drop-module-lines "nbbuild/cluster.properties" thirdparty-modules)
              (drop-module-lines "nbbuild/cluster.properties" harness-modules)
              (drop-module-lines "nbbuild/cluster.properties" nb-modules)
              (drop-module-lines "nbbuild/cluster.properties" cluster-references)
              (append-string
               "nbbuild/cluster.properties"
               "
clusters.config.guix.list=\\
        ${clusters.config.platform.list},\\
        nb.cluster.ide,\\
        nb.cluster.extide,\\
        nb.cluster.nb
")
              (trim-trailing-list-commas "nbbuild/cluster.properties")
              (drop-module-lines "nbbuild/build.properties" platform-modules)
              (drop-module-lines "nbbuild/build.properties" ide-modules)
              (drop-module-lines "nbbuild/build.properties" extide-modules)
              (drop-module-lines "nbbuild/build.properties" thirdparty-modules)
              (drop-module-lines "nbbuild/build.properties" harness-modules)
              (drop-module-lines "nbbuild/build.properties" nb-modules)
              (drop-module-lines "nbbuild/build.properties" cluster-references)
              (trim-trailing-list-commas "nbbuild/build.properties")

              (remove-dependency-blocks
               "platform/core.kit/nbproject/project.xml"
               '("org.netbeans.modules.autoupdate.cli"
                 "org.netbeans.modules.autoupdate.services"
                 "org.netbeans.modules.autoupdate.ui"))
              (remove-dependency-blocks
               "ide/ide.kit/nbproject/project.xml"
               '("org.netbeans.modules.httpserver"
                 "org.netbeans.modules.usersguide"))
              (remove-dependency-blocks
               "ide/editor.kit/nbproject/project.xml"
               '("org.netbeans.modules.css.visual"
                 "org.netbeans.modules.html"
                 "org.netbeans.modules.html.parser"
                 "org.netbeans.modules.html.validation"
                 "org.netbeans.modules.languages.go"
                 "org.netbeans.modules.languages.hcl"
                 "org.netbeans.modules.languages.toml"
                 "org.netbeans.modules.languages.yaml"
                 "org.netbeans.modules.xml.tools"
                 "org.netbeans.modules.xsl"))
              (remove-dependency-blocks
               "nb/ide.branding.kit/nbproject/project.xml"
               '("org.netbeans.modules.autoupdate.pluginimporter"))

              (filter-lines
               "platform/o.n.bootstrap/nbproject/project.properties"
               (lambda (line)
                 (not (string-contains line "launcher-external-binaries-2-6c17cc6.zip!/"))))
              (filter-lines
               "harness/apisupport.harness/nbproject/project.properties"
               (lambda (line)
                 (and (not (string-contains line "release.external/bindex-2.2.jar"))
                      (not (string-contains line "release.external/launcher-external-binaries-2-6c17cc6.zip!/"))
                      (not (string-contains line "release.external/harness-launchers-8.2.zip!/")))))

              (filter-lines
               "nbbuild/build.xml"
               (lambda (line)
                 (and (not (string-contains line "platform/libs.junit4/external/binaries-list"))
                      (not (string-contains line "platform/javahelp/external/binaries-list"))
                      (not (string-contains line "platform/javahelp/external/jhall")))))
              (filter-lines
               "ide/ide.kit/manifest.mf"
               (lambda (line)
                 (not (string-contains line "OpenIDE-Module-Needs: org.netbeans.Netbinox"))))
              (drop-lines-containing
               "platform/libs.jna/nbproject/project.properties"
               '("darwin-x86-64"
                 "darwin-aarch64"
                 "linux-x86/"
                 "linux-aarch64"
                 "linux-riscv64"
                 "win32-x86-64"
                 "win32-x86/"
                 "win32-aarch64"
                 "modules/lib/amd64/jnidispatch-nb.dll"
                 "modules/lib/x86/jnidispatch-nb.dll"
                 "modules/lib/aarch64/jnidispatch-nb.dll"
                 "modules/lib/i386/linux/libjnidispatch-nb.so"
                 "modules/lib/riscv64/linux/libjnidispatch-nb.so"
                 "modules/lib/aarch64/linux/libjnidispatch-nb.so"
                 "modules/lib/x86_64/libjnidispatch-nb.jnilib"
                 "modules/lib/aarch64/libjnidispatch-nb.jnilib"))
              (trim-trailing-list-commas
               "platform/libs.jna/nbproject/project.properties")
              (drop-lines-containing
               "ide/dlight.nativeexecution/nbproject/project.properties"
               '("Linux-aarch64/"
                 "Linux-x86/"
                 "Linux-sparc_64/"
                 "Windows-x86/"
                 "Windows-x86_64/"
                 "MacOSX-x86/"
                 "MacOSX-x86_64/"
                 "MacOSX-arm_64/"
                 "SunOS-sparc_64/"
                 "SunOS-x86/"
                 "SunOS-x86_64/"
                 "nativeexecution-external-binaries-1-24aefa9.zip"))
              (trim-trailing-list-commas
               "ide/dlight.nativeexecution/nbproject/project.properties")

              ;; Strip non-Linux junixsocket native library entries.
              (drop-lines-containing
               "ide/libs.c.kohlschutter.junixsocket/nbproject/project.properties"
               '("MacOSX-"
                 "x86_64-MacOSX"
                 "aarch64-MacOSX"
                 "Windows"
                 "SunOS"
                 "FreeBSD"
                 "DragonFly"
                 "NetBSD"
                 "OpenBSD"
                 "AIX"
                 "OS400"
                 "arm-Linux"
                 "aarch64-Linux"
                 "ppc64"
                 "s390x"
                 "riscv64"
                 "nodeps"))
              (trim-trailing-list-commas
               "ide/libs.c.kohlschutter.junixsocket/nbproject/project.properties")

              ;; Strip non-Linux FlatLaf native libraries; keep
              ;; libflatlaf-linux-x86_64.so for custom window decorations.
              (drop-lines-containing
               "platform/libs.flatlaf/nbproject/project.properties"
               '("flatlaf-windows-"
                 "libflatlaf-macos-"))

              (symlink-input-file "java-flatlaf"
                               "flatlaf-[0-9.]*\\.jar$"
                               "platform/libs.flatlaf/external/flatlaf-3.5.2.jar")
              (symlink-input-file "java-jsvg"
                               "jsvg-[0-9.]*\\.jar$"
                               "platform/libs.jsvg/external/jsvg-1.6.1.jar")
              (symlink-input-file "java-json-simple"
                               "json-simple-[0-9.]*\\.jar$"
                               "nbbuild/external/json-simple-1.1.1.jar")
              (symlink-input-file "java-jsoup"
                               "jsoup-[0-9.]*\\.jar$"
                               "nbbuild/external/jsoup-1.15.3.jar")
              (symlink-input-file "java-junit"
                               "junit-[0-9.]*\\.jar$"
                               "platform/libs.junit4/external/junit-4.13.2.jar")
              (symlink-input-file "java-hamcrest-core"
                               "hamcrest-core-[0-9.]*\\.jar$"
                               "platform/libs.junit4/external/hamcrest-core-1.3.jar")
              (symlink-input-file "java-lucene-core"
                               "lucene-core-[0-9.]*\\.jar$"
                               "ide/libs.lucene/external/lucene-core-3.6.2.jar")
              (symlink-input-file "java-junixsocket-common"
                               "junixsocket-common-[0-9.]*\\.jar$"
                               "ide/libs.c.kohlschutter.junixsocket/external/junixsocket-common-2.5.1.jar")
              (symlink-input-file "java-junixsocket-native-common"
                               "junixsocket-native-common-[0-9.]*\\.jar$"
                               "ide/libs.c.kohlschutter.junixsocket/external/junixsocket-native-common-2.5.1.jar")
              (symlink-input-file "java-osgi-core"
                               "osgi\\.core-[0-9.]*\\.jar$"
                               "platform/libs.osgi/external/osgi.core-8.0.0.jar")
              (symlink-input-file "java-osgi-cmpn"
                               "osgi\\.cmpn-[0-9.]*\\.jar$"
                               "platform/libs.osgi/external/osgi.cmpn-7.0.0.jar")
              (symlink-input-file "java-bouncycastle"
                               "bcprov-jdk18on-[0-9.]*\\.jar$"
                               "ide/bcprov/external/bcprov-jdk18on-1.77.jar")
              (symlink-input-file "java-bouncycastle"
                               "bcpg-jdk18on-[0-9.]*\\.jar$"
                               "ide/bcpg/external/bcpg-jdk18on-1.77.jar")
              (symlink-input-file "java-bouncycastle"
                               "bcpkix-jdk18on-[0-9.]*\\.jar$"
                               "ide/bcpkix/external/bcpkix-jdk18on-1.77.jar")
              (symlink-input-file "java-bouncycastle"
                               "bcutil-jdk18on-[0-9.]*\\.jar$"
                               "ide/bcutil/external/bcutil-jdk18on-1.77.jar")
              (symlink-input-file "java-jgit-gpg-bc"
                               "org\\.eclipse\\.jgit\\.gpg\\.bc-[0-9.].*\\.jar$"
                               "ide/o.eclipse.jgit.gpg.bc/external/org.eclipse.jgit.gpg.bc-7.0.0.202409031743-r.jar")
              (symlink-input-file "java-commons-codec"
                               "commons-codec-[0-9.]*\\.jar$"
                               "platform/o.apache.commons.codec/external/commons-codec-1.17.1.jar")
              (symlink-input-file "java-felix-main"
                               "org\\.apache\\.felix\\.main-[0-9.]*\\.jar$"
                               "platform/libs.felix/external/org.apache.felix.main-7.0.5.jar")
              (symlink-input-file "java-jna"
                               "jna-jpms-[0-9.]*\\.jar$"
                               "platform/libs.jna/external/jna-5.14.0.jar")
              (symlink-input-file "java-jna-platform"
                               "jna-platform-jpms-[0-9.]*\\.jar$"
                               "platform/libs.jna.platform/external/jna-platform-5.14.0.jar")
              (symlink-input-file "java-jsch"
                               "jsch-[0-9.]*\\.jar$"
                               "ide/c.jcraft.jsch/external/jsch-0.1.72.jar")
              (symlink-input-file "java-asm"
                               "asm9\\.jar$"
                               "platform/libs.asm/external/asm-9.7.1.jar")
              (symlink-input-file "java-asm-tree"
                               "asm-tree\\.jar$"
                               "platform/libs.asm/external/asm-tree-9.7.1.jar")
              (symlink-input-file "java-asm-commons"
                               "asm-commons8\\.jar$"
                               "platform/libs.asm/external/asm-commons-9.7.1.jar")
              (symlink-input-file "java-jemmy"
                               "jemmy-[0-9.]*.jar$"
                               "harness/jemmy/external/jemmy-2.3.1.1.jar")
              (symlink-input-file "java-jemmy"
                               "jemmy-[0-9.]*-doc\\.zip$"
                               "harness/jemmy/external/jemmy-2.3.1.1-doc.zip")
              (symlink-input-file "java-jemmy"
                               "jemmy-.*-src\\.zip$"
                               "harness/jemmy/external/jemmy-2.3.1.1-src.zip")
              (symlink-input-file "java-apache-xml-commons-resolver-netbeans"
                               "xml-resolver\\.jar$"
                               "ide/o.apache.xml.resolver/external/resolver-1.2.jar")
              (symlink-input-file "java-simplevalidation"
                               "simplevalidation-[0-9.]*\\.jar$"
                               "ide/swing.validation/external/simplevalidation-1.14.1.jar")
              (symlink-input-file "java-simplevalidation"
                               "simplevalidation-swing-[0-9.]*\\.jar$"
                               "ide/swing.validation/external/simplevalidation-swing-1.14.1.jar")
              (symlink-input-file "java-jgit"
                               "org\\.eclipse\\.jgit-[0-9.].*\\.jar$"
                               "ide/o.eclipse.jgit/external/org.eclipse.jgit-7.0.0.202409031743-r.jar")
              (symlink-input-file "java-jgit-ssh-jsch"
                               "org\\.eclipse\\.jgit\\.ssh\\.jsch-[0-9.].*\\.jar$"
                               "ide/o.eclipse.jgit.ssh.jsch/external/org.eclipse.jgit.ssh.jsch-7.0.0.202409031743-r.jar")
              (symlink-input-file "java-jzlib"
                               "jzlib-[0-9.]*\\.jar$"
                               "ide/c.jcraft.jzlib/external/jzlib-1.1.3.jar")
              (symlink-input-file "java-javaewah"
                               "JavaEWAH-[0-9.]*\\.jar$"
                               "ide/c.googlecode.javaewah.JavaEWAH/external/JavaEWAH-1.2.3.jar")
              (symlink-input-file "java-slf4j-api"
                               "slf4j-api-[0-9.]*\\.jar$"
                               "ide/slf4j.api/external/slf4j-api-1.7.36.jar")
              (symlink-input-file "java-slf4j-jdk14"
                               "slf4j-jdk14-[0-9.]*\\.jar$"
                               "ide/slf4j.jdk14/external/slf4j-jdk14-1.7.36.jar")
              (make-nativeexecution-binary-zip)
              (make-ant-binary-zip)))
          (replace 'build
            (lambda _
              ;; FIXME: Why are tests disabled?
              (invoke "ant"
                      "-Dcluster.config=guix" ; use our config file
                      "-Ddisable.unit.tests=true"
                      "-Ddisable.qa-functional.tests=true"
                      "-Dext.binaries.downloaded=true" ; so it doesn't download
                      ; "-Dverify.checkout=false" ; FIXME why is this there
                      "-f" "nbbuild/build.xml"
                      "build-nozip")))
          (replace 'install
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (jdk (assoc-ref inputs "jdk"))
                     (runtime-path
                      (string-join
                       (map (lambda (input)
                              (string-append (assoc-ref inputs input) "/bin"))
                            '("bash-minimal"
                              "coreutils"
                              "grep"
                              "sed"
                              "gawk"))
                       ":"))
                     (share (string-append out "/share/netbeans"))
                     (bin (string-append out "/bin")))
                (mkdir-p bin)
                (copy-recursively "nbbuild/netbeans" share)
                ;; Set JVM options for proper rendering in etc/netbeans.conf.
                (let ((conf (string-append share "/etc/netbeans.conf")))
                  (when (file-exists? conf)
                    (substitute* conf
                      (("netbeans_default_options=\"")
                       (string-append
                        "netbeans_default_options=\""
                        "-J-Dawt.toolkit.name=auto "
                        "-J-Dsun.java2d.uiScale.enabled=true "
                        "-J-Dorg.netbeans.editor.aa.text=hbgr "
                        "-J-Dorg.netbeans.editor.aa.contrast=140 "
                        "-J--enable-native-access=ALL-UNNAMED ")))))
                (call-with-output-file (string-append bin "/netbeans")
                  (lambda (port)
                    (format port "#!/bin/sh~%export PATH=\"~a${PATH:+:$PATH}\"~%exec ~a/share/netbeans/bin/netbeans --jdkhome ~a \"$@\"~%"
                            runtime-path
                            out
                            (assoc-ref inputs "jbr"))))
                (chmod (string-append bin "/netbeans") #o755)))))))
    (inputs
     `(("jbr" ,jbr25 "jdk")
       ("bash-minimal" ,bash-minimal)
       ("coreutils" ,coreutils)
       ("gawk" ,gawk)
       ("glib" ,glib)
       ("grep" ,grep)
       ("libsecret" ,libsecret)
       ("sed" ,sed)))
    (native-inputs
     `(("java-json-simple" ,java-json-simple-1.1.1)
       ("java-jsoup" ,java-jsoup-1.15.3)
       ("java-osgi-core" ,java-osgi-core-8.0.0)
       ("java-osgi-cmpn" ,java-osgi-cmpn-7.0.0)
       ("java-bouncycastle" ,java-bouncycastle-1.77)
       ("java-commons-codec" ,java-commons-codec-1.17.1)
       ("java-felix-main" ,java-felix-main-7.0.5)
       ("java-junixsocket-common" ,java-junixsocket-common-2)
       ("java-junixsocket-native-common" ,java-junixsocket-native-common-2.5.1)
       ("java-jna" ,java-native-access-5.14.0)
       ("java-jna-platform" ,java-native-access-platform-5.14.0)
       ("java-jsch" ,java-jsch-0.1.72)
       ("java-junit" ,java-junit-4.13.2)
       ("java-hamcrest-core" ,java-hamcrest-core)
       ("java-lucene-core" ,java-lucene-core-3.6.2)
       ("java-asm" ,java-asm-9.7.1)
       ("java-asm-tree" ,java-asm-tree-9.7.1)
       ("java-asm-commons" ,java-asm-commons-9.7.1)
       ("java-apache-xml-commons-resolver-netbeans"
        ,java-apache-xml-commons-resolver-netbeans-1.2)
       ("java-simplevalidation" ,java-simplevalidation-1.14.1)
       ("java-jemmy" ,java-jemmy-2.3.1.1)
       ("java-flatlaf" ,java-flatlaf)
       ("java-jsvg" ,java-jsvg-1.6.1)
       ("java-jgit" ,java-jgit-7)
       ("java-jgit-gpg-bc" ,java-jgit-gpg-bc-7)
       ("java-jgit-ssh-jsch" ,java-jgit-ssh-jsch-7)
       ("java-jzlib" ,java-jzlib-1.1.3)
       ("java-javaewah" ,java-javaewah-1.2.3)
       ("java-slf4j-api" ,java-slf4j-api)
       ("java-slf4j-jdk14" ,java-slf4j-jdk14)
       ("unzip" ,unzip)))
    (home-page "https://netbeans.apache.org/")
    (synopsis "Source-only build of Apache NetBeans 25")
    (description
     "This package builds Apache NetBeans 25 from source only.

The build disables the following platform modules:
@itemize
@item api.htmlui
@item applemenu
@item autoupdate.cli
@item autoupdate.services
@item autoupdate.ui
@item core.nativeaccess
@item core.network
@item htmlui
@item javahelp
@item junitlib
@item libs.batik.read
@item libs.javafx
@item libs.javax.inject
@item libs.junit4
@item libs.junit5
@item libs.testng
@item masterfs.macosx
@item masterfs.windows
@item net.java.html
@item net.java.html.boot
@item net.java.html.boot.fx
@item net.java.html.boot.script
@item net.java.html.geo
@item net.java.html.json
@item net.java.html.sound
@item netbinox
@item o.apache.commons.commons_io
@item o.apache.commons.lang3
@item o.apache.commons.logging
@item o.n.html.ko4j
@item o.n.html.presenters.spi
@item o.n.html.xhr4j
@item templatesui
@end itemize

The build disables the following IDE modules:
@itemize
@item bugtracking
@item bugtracking.bridge
@item bugtracking.commons
@item bugzilla
@item c.google.gson
@item c.google.guava
@item c.google.guava.failureaccess
@item core.browser.webview
@item css.editor
@item css.lib
@item css.model
@item css.prep
@item css.visual
@item db
@item db.core
@item db.dataview
@item db.drivers
@item db.kit
@item db.metadata.model
@item db.mysql
@item db.sql.editor
@item db.sql.visualeditor
@item dbapi
@item derby
@item docker.api
@item docker.editor
@item docker.ui
@item go.lang
@item html
@item html.custom
@item html.editor
@item html.editor.lib
@item html.indexing
@item html.lexer
@item html.parser
@item html.validation
@item httpserver
@item hudson
@item hudson.git
@item hudson.mercurial
@item hudson.subversion
@item hudson.tasklist
@item hudson.ui
@item javascript2.debug
@item javascript2.debug.ui
@item languages.go
@item languages.hcl
@item languages.toml
@item languages.yaml
@item lexer.antlr4
@item libs.antlr3.runtime
@item libs.antlr4.runtime
@item libs.commons_compress
@item libs.commons_net
@item libs.flexmark
@item libs.freemarker
@item libs.graalsdk
@item libs.graalsdk.system
@item libs.ini4j
@item libs.jaxb
@item libs.jcodings
@item libs.json_simple
@item libs.snakeyaml_engine
@item libs.svnClientAdapter
@item libs.svnClientAdapter.javahl
@item libs.tomlj
@item libs.tomljava
@item libs.truffleapi
@item libs.xerces
@item localtasks
@item lsp.client
@item markdown
@item mercurial
@item mylyn.util
@item o.apache.commons.httpclient
@item o.apache.commons.lang
@item o.apache.ws.commons.util
@item o.apache.xmlrpc
@item o.eclipse.core.contenttype
@item o.eclipse.core.jobs
@item o.eclipse.core.net
@item o.eclipse.core.runtime
@item o.eclipse.core.runtime.compatibility.auth
@item o.eclipse.equinox.app
@item o.eclipse.equinox.common
@item o.eclipse.equinox.preferences
@item o.eclipse.equinox.registry
@item o.eclipse.equinox.security
@item o.eclipse.jgit.lfs
@item o.eclipse.mylyn.bugzilla.core
@item o.eclipse.mylyn.commons.core
@item o.eclipse.mylyn.commons.net
@item o.eclipse.mylyn.commons.repositories.core
@item o.eclipse.mylyn.commons.xmlrpc
@item o.eclipse.mylyn.tasks.core
@item o.eclipse.mylyn.wikitext.confluence.core
@item o.eclipse.mylyn.wikitext.core
@item o.eclipse.mylyn.wikitext.markdown.core
@item o.eclipse.mylyn.wikitext.textile.core
@item selenium2
@item selenium2.server
@item servletapi
@item spellchecker
@item spellchecker.bindings.htmlxml
@item spellchecker.bindings.properties
@item spellchecker.dictionary_en
@item spellchecker.kit
@item subversion
@item team.ide
@item textmate.lexer
@item usersguide
@item versioning.system.cvss.installer
@item web.browser.api
@item web.common
@item web.common.ui
@item web.indent
@item web.webkit.debugging
@item xml.jaxb.api
@item xml.tax
@item xml.text.obsolete90
@item xml.tools
@item xml.wsdl.model
@item xsl
@end itemize

The build disables the following extide modules:
@itemize
@item gradle
@item gradle.dists
@item gradle.editor
@item libs.gradle
@end itemize

The build disables the following third-party modules:
@itemize
@item libs.javafx.linux
@item libs.javafx.linux.aarch64
@item libs.javafx.macosx
@item libs.javafx.macosx.aarch64
@item libs.javafx.win
@end itemize

The build disables the following NB modules:
@itemize
@item autoupdate.pluginimporter
@item bugzilla.exceptionreporter
@item updatecenters
@end itemize

The build disables the following cluster references:
@itemize
@item nb.cluster.3rdparty
@item nb.cluster.javafx
@end itemize

The build also disables the JavaHelp bootstrap checks and the Windows
launcher download target so the resulting IDE remains fully
source-built.

This package is built with Wayland support and HiDPI support.")
    (license (list license:asl2.0
                   license:bsd-3
                   license:cddl1.0
                   license:expat))))
