;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2023 Liliana Marie Prikler <liliana.prikler@gmail.com>
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

(define-module (gnu packages java-rdf)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system ant)
  #:use-module (gnu packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages java))

(define-public java-commons-rdf-api
  (package
    (name "java-commons-rdf-api")
    (version "0.5.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/commons/rdf/source/"
                                  "apache-commons-rdf-" version
                                  "-source-release.zip"))
              (sha256
               (base32
                "14ihqgpbci6z5a5xlksbhjjxaw1kb80gx1k8l20v8w41in53rzqz"))))
    (build-system ant-build-system)
    (arguments
     (list #:jar-name "commons-rdf-api.jar"
           #:source-dir "src/main/java"
           #:test-dir "src/test"
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'chdir
                 (lambda _ (chdir "commons-rdf-api")))
               (replace 'install
                 (install-from-pom "pom.xml")))))
    (native-inputs (list java-junit java-slf4j-simple unzip))
    (home-page "https://commons.apache.org/proper/commons-rdf/")
    (synopsis "Java interfaces for RDF 1.1 concepts")
    (description "This package provides Java interfaces for RDF 1.1 concepts.")
    (license license:asl2.0)))

(define-public java-commons-rdf-api-tests
  (package
    (inherit java-commons-rdf-api)
    (name "java-commons-rdf-api-tests")
    (arguments
     (list #:tests? #f
           #:jar-name "commons-rdf-api-tests.jar"
           #:source-dir "commons-rdf-api/src/test/java"))
    (propagated-inputs (list java-commons-rdf-api
                             java-junit
                             java-slf4j-simple))
    (native-inputs (list unzip))
    (description "This package provides common test classes for packages
implementing java-commons-rdf-api.")))

(define-public java-commons-rdf-simple
  (package
    (inherit java-commons-rdf-api)
    (name "java-commons-rdf-simple")
    (arguments
     (list #:jar-name "commons-rdf-api.jar"
           #:source-dir "src/main/java"
           #:test-dir "src/test"
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'chdir
                 (lambda _ (chdir "commons-rdf-simple")))
               (add-after 'chdir 'delete-failing-tests
                 (lambda _
                   (with-directory-excursion "src/test/java/org/apache/commons/"
                     (delete-file "rdf/simple/SimpleServiceLoaderTest.java"))))
               (add-before 'install 'fix-pom
                 (lambda _
                   (substitute* "pom.xml"
                     (("\\$\\{project\\.parent\\.groupId\\}")
                      "org.apache.commons"))))
               (replace 'install
                 (install-from-pom "pom.xml")))))
    (propagated-inputs (list java-commons-rdf-api))
    (native-inputs (list java-commons-rdf-api-tests unzip))
    (synopsis "Simple implementation of RDF 1.1 concepts")
    (description "This package provides a simple implementation of RDF 1.1
concepts in Java.")))

(define %rdf4j-version "3.7.7")
(define %rdf4j-sha256
  (base32 "1lala4wjl5lbg45jdgd94rfkvdg6r4vq23k3q54bkk9q3w9v2bdx"))
(define %rdf4j-source
  (origin
   (method git-fetch)
   (uri (git-reference
         (url "https://github.com/eclipse/rdf4j")
         (commit %rdf4j-version)))
   (file-name (git-file-name "rdf4j" %rdf4j-version))
   (sha256 %rdf4j-sha256)
   (modules '((guix build utils)))
   (snippet #~(begin (delete-file-recursively "site")))))

(define (rdf4j-common-arguments jar-name directory)
  (list #:source-dir "src/main/java"
        #:test-dir "src/test/java"
        #:tests? #f                     ; tests require junit 5
        #:jar-name jar-name
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'chdir (lambda _ (chdir #$directory)))
            (add-after 'chdir 'fix-pom
              (lambda _
                (substitute* "pom.xml"
                  (("\\$\\{project\\.groupId\\}") "org.eclipse.rdf4j"))))
            (replace 'install
              (install-from-pom "pom.xml")))))

(define-public java-eclipse-rdf4j-model-api
  (package
    (name "java-eclipse-rdf4j-model-api")
    (version %rdf4j-version)
    (source %rdf4j-source)
    (build-system ant-build-system)
    (arguments
     (rdf4j-common-arguments "rdf4j-model-api.jar"
                             "core/model-api"))
    (home-page "https://rdf4j.org/")
    (synopsis "RDF4J model interfaces")
    (description "This package provides interfaces for the RDF data model used
in the RDF4J framework.")
    (license license:epl1.0)))

(define-public java-eclipse-rdf4j-model-vocabulary
  (package
    (name "java-eclipse-rdf4j-model-vocabulary")
    (version %rdf4j-version)
    (source %rdf4j-source)
    (build-system ant-build-system)
    (arguments
     (rdf4j-common-arguments "rdf4j-model-vocabulary.jar"
                             "core/model-vocabulary"))
    (propagated-inputs (list java-eclipse-rdf4j-model-api))
    (home-page "https://rdf4j.org/")
    (synopsis "Well known RDF vocabularies")
    (description "This package provides Java classes for well known
RDF vocabularies.")
    (license license:epl1.0)))
