;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2019 Björn Höfling <bjoern.hoefling@bjoernhoefling.de>
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

(define-module (guix build ant-build-system)
  #:use-module ((guix build gnu-build-system) #:prefix gnu:)
  #:use-module (guix build syscalls)
  #:use-module (guix build utils)
  #:use-module (sxml simple)
  #:use-module (ice-9 match)
  #:use-module (ice-9 ftw)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (%standard-phases
            ant-build))

;; Commentary:
;;
;; Builder-side code of the standard build procedure for Java packages using
;; Ant.
;;
;; Code:

(define* (default-build.xml jar-name prefix #:optional
                            (source-dir ".") (test-dir "./test") (main-class #f)
                            (test-include '("**/*Test.java"))
                            (test-exclude '("**/Abstract*Test.java")))
  "Create a simple build.xml with standard targets for Ant."
  (call-with-output-file "build.xml"
    (lambda (port)
      (sxml->xml
       `(project (@ (basedir ".")
                    (name ,jar-name))
                 (property (@ (name "classes.dir")
                              (value "${basedir}/build/classes")))
                 (property (@ (name "manifest.dir")
                              (value "${basedir}/build/manifest")))
                 (property (@ (name "manifest.file")
                              (value "${manifest.dir}/MANIFEST.MF")))
                 (property (@ (name "jar.dir")
                              (value "${basedir}/build/jar")))
                 (property (@ (name "dist.dir")
                              (value ,prefix)))
                 (property (@ (name "test.home")
                              (value ,test-dir)))
                 (property (@ (name "test.classes.dir")
                              (value "${basedir}/build/test-classes")))

                 ;; respect the CLASSPATH environment variable
                 (property (@ (name "build.sysclasspath")
                              (value "first")))
                 (property (@ (environment "env")))
                 (path (@ (id "classpath"))
                       (pathelement (@ (location "${env.CLASSPATH}"))))

                 (target (@ (name "manifest"))
                         (mkdir (@ (dir "${manifest.dir}")))
                         (manifest (@ (file "${manifest.file}"))
                                   ,(if main-class
                                       `(attribute (@ (name "Main-Class")
                                                      (value ,main-class)))
                                       "")))

                 (target (@ (name "compile"))
                         (mkdir (@ (dir "${classes.dir}")))
                         (javac (@ (includeantruntime "false")
                                   (srcdir ,source-dir)
                                   (destdir "${classes.dir}")
                                   (classpath (@ (refid "classpath"))))))

                 (target (@ (name "compile-tests"))
                         (mkdir (@ (dir "${test.classes.dir}")))
                         (javac (@ (includeantruntime "false")
                                   (srcdir ,test-dir)
                                   (destdir "${test.classes.dir}"))
                                (classpath
                                 (pathelement (@ (path "${env.CLASSPATH}")))
                                 (pathelement (@ (location "${classes.dir}")))
                                 (pathelement (@ (location "${test.classes.dir}"))))))

                 (target (@ (name "check")
                            (depends "compile-tests"))
                         (mkdir (@ (dir "${test.home}/test-reports")))
                         (junit (@ (printsummary "true")
                                   (showoutput "true")
                                   (fork "yes")
                                   (haltonfailure "yes"))
                                (classpath
                                 (pathelement (@ (path "${env.CLASSPATH}")))
                                 (pathelement (@ (location "${test.home}/resources")))
                                 (pathelement (@ (location "${classes.dir}")))
                                 (pathelement (@ (location "${test.classes.dir}"))))
                                (formatter (@ (type "plain")
                                              (usefile "true")))
                                (batchtest (@ (fork "yes")
                                              (todir "${test.home}/test-reports"))
                                           (fileset (@ (dir "${test.home}/java"))
                                                    ,@(map (lambda (file)
                                                            `(include (@ (name ,file))))
                                                       test-include)
                                                    ,@(map (lambda (file)
                                                            `(exclude (@ (name ,file))))
                                                       test-exclude)))))

                 (target (@ (name "jar")
                            (depends "compile, manifest"))
                         (mkdir (@ (dir "${jar.dir}")))
                         (jar (@ (destfile ,(string-append "${jar.dir}/" jar-name))
                                 (manifest "${manifest.file}")
                                 (basedir "${classes.dir}"))))

                 (target (@ (name "install"))
                         (copy (@ (todir "${dist.dir}"))
                               (fileset (@ (dir "${jar.dir}"))
                                        (include (@ (name "**/*.jar")))))))
       port)))
  (utime "build.xml" 0 0)
  #t)

(define (generate-classpath inputs)
  "Return a colon-separated string of full paths to jar files found among the
INPUTS."
  (string-join
   (apply append (map (match-lambda
                        ((_ . dir)
                         (find-files dir "\\.jar$")))
                      inputs)) ":"))

(define* (unpack #:key source #:allow-other-keys)
  "Unpack the jar archive SOURCE.  When SOURCE is not a jar archive fall back
to the default GNU unpack strategy."
  (if (string-suffix? ".jar" source)
      (begin
        (mkdir "src")
        (with-directory-excursion "src"
          (invoke "jar" "-xf" source))
        #t)
      ;; Use GNU unpack strategy for things that aren't jar archives.
      ((assq-ref gnu:%standard-phases 'unpack) #:source source)))

(define* (configure #:key inputs outputs (jar-name #f)
                    (source-dir "src")
                    (test-dir "src/test")
                    (main-class #f)
                    (test-include '("**/*Test.java"))
                    (test-exclude '("**/Abstract*.java")) #:allow-other-keys)
  (when jar-name
    (default-build.xml jar-name
                       (string-append (assoc-ref outputs "out")
                                      "/share/java")
                       source-dir test-dir main-class test-include test-exclude))
  (setenv "JAVA_HOME" (assoc-ref inputs "jdk"))
  (setenv "CLASSPATH" (generate-classpath inputs))
  #t)

(define* (build #:key (make-flags '()) (build-target "jar")
                #:allow-other-keys)
  (apply invoke `("ant" ,build-target ,@make-flags)))

(define (regular-jar-file-predicate file stat)
  "Predicate returning true if FILE is ending on '.jar'
and STAT indicates it is a regular file."
    (and ((file-name-predicate "\\.jar$") file stat)
         (eq? 'regular (stat:type stat))))

(define* (generate-jar-indices #:key outputs #:allow-other-keys)
  "Generate file \"META-INF/INDEX.LIST\".  This file does not use word wraps
and is preferred over \"META-INF/MANIFEST.MF\", which does use word wraps,
by Java when resolving dependencies.  So we make sure to create it so that
grafting works - and so that the garbage collector doesn't collect
dependencies of this jar file."
  (define (generate-index jar)
    (invoke "jar" "-i" jar))
  (for-each (match-lambda
              ((output . directory)
               (for-each generate-index
                         (find-files
                          directory
                          regular-jar-file-predicate))))
            outputs)
  #t)

(define* (strip-jar-timestamps #:key outputs
                               #:allow-other-keys)
  "Unpack all jar archives, reset the timestamp of all contained files, and
repack them.  This is necessary to ensure that archives are reproducible."
  (define (repack-archive jar)
    (format #t "repacking ~a\n" jar)
    (let* ((dir (mkdtemp! "jar-contents.XXXXXX"))
           (manifest (string-append dir "/META-INF/MANIFEST.MF")))
      (with-directory-excursion dir
        (invoke "jar" "xf" jar))
      (delete-file jar)
      ;; XXX: copied from (gnu build install)
      (for-each (lambda (file)
                  (let ((s (lstat file)))
                    (unless (eq? (stat:type s) 'symlink)
                      (utime file 0 0 0 0))))
                (find-files dir #:directories? #t))

      ;; The jar tool will always set the timestamp on the manifest file
      ;; and the containing directory to the current time, even when we
      ;; reuse an existing manifest file.  To avoid this we use "zip"
      ;; instead of "jar".  It is important that the manifest appears
      ;; first.
      (with-directory-excursion dir
        (let* ((files (find-files "." ".*" #:directories? #t))
               ;; To ensure that the reference scanner can detect all
               ;; store references in the jars we disable compression
               ;; with the "-0" option.
               (command (if (file-exists? manifest)
                            `("zip" "-0" "-X" ,jar ,manifest ,@files)
                            `("zip" "-0" "-X" ,jar ,@files))))
          (apply invoke command)))
      (utime jar 0 0)
      #t))

  (for-each (match-lambda
              ((output . directory)
               (for-each repack-archive
                         (find-files directory regular-jar-file-predicate))))
            outputs)
  #t)

(define* (check #:key target (make-flags '()) (tests? (not target))
                (test-target "check")
                #:allow-other-keys)
  (if tests?
      (apply invoke `("ant" ,test-target ,@make-flags))
      (format #t "test suite not run~%"))
  #t)

(define* (install #:key (make-flags '()) #:allow-other-keys)
  (apply invoke `("ant" "install" ,@make-flags)))

(define %standard-phases
  (modify-phases gnu:%standard-phases
    (replace 'unpack unpack)
    (delete 'bootstrap)
    (replace 'configure configure)
    (replace 'build build)
    (replace 'check check)
    (replace 'install install)
    (add-after 'install 'reorder-jar-content
               strip-jar-timestamps)
    (add-after 'reorder-jar-content 'generate-jar-indices generate-jar-indices)
    (add-after 'generate-jar-indices 'strip-jar-timestamps
               strip-jar-timestamps)))

(define* (ant-build #:key inputs (phases %standard-phases)
                    #:allow-other-keys #:rest args)
  "Build the given Java package, applying all of PHASES in order."
  (apply gnu:gnu-build #:inputs inputs #:phases phases args))

;;; ant-build-system.scm ends here
