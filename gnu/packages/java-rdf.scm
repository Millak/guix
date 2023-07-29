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
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages java))

(define-public java-jsonld-java
  (package
    (name "java-jsonld-java")
    (version "0.13.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/jsonld-java/jsonld-java")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0d113szja4p16k2n2way8mmdj1kxzanjcnnsdan65iw27qag7dr0"))))
    (build-system ant-build-system)
    (arguments
     (list #:tests? #f                  ; no tests included
           #:jar-name "jsonld-java.jar"
           #:source-dir "core/src/main/java"
           #:test-dir "core/src/test"
           #:phases
           #~(modify-phases %standard-phases
               (add-before 'install 'generate-pom.xml
                 (generate-pom.xml "guix-pom.xml"
                                   "com.github.jsonld-java"
                                   "jsonld-java" #$version))
               (replace 'install
                 (install-from-pom "guix-pom.xml")))))
    (inputs (list java-slf4j-api java-guava
                  java-commons-io
                  java-httpcomponents-httpcore
                  java-httpcomponents-httpcore-osgi
                  java-httpcomponents-httpclient
                  java-httpcomponents-httpclient-cache
                  java-httpcomponents-httpclient-osgi
                  java-fasterxml-jackson-core
                  java-fasterxml-jackson-databind))
    (home-page "https://github.com/jsonld-java/jsonld-java")
    (synopsis "Java implementation of JSON-LD")
    (description "This package provides a Java implementation of JSON-LD 1.0.")
    (license license:bsd-3)))

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

(define-public java-commons-rdf-rdf4j
  (package
    (inherit java-commons-rdf-api)
    (name "java-commons-rdf-rdf4j")
    (version "0.5.0")
    (arguments
     (list #:jar-name "commons-rdf-rdf4j.jar"
           #:source-dir "src/main/java"
           #:test-dir "src/test"
           #:test-exclude (list "**/RDF4JServiceLoaderTest.java")
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'chdir
                 (lambda _ (chdir "commons-rdf-rdf4j")))
               (add-before 'install 'fix-pom
                 (lambda _
                   (substitute* "pom.xml"
                     (("\\$\\{project\\.parent\\.groupId\\}")
                      "org.apache.commons"))))
               (replace 'install
                 (install-from-pom "pom.xml")))))
    (inputs (list java-guava
                  java-commons-io
                  java-commons-lang3
                  java-commons-text
                  java-fasterxml-jackson-annotations
                  java-fasterxml-jackson-core
                  java-fasterxml-jackson-databind
                  java-mapdb))
    (propagated-inputs (list java-commons-rdf-api
                             java-commons-rdf-simple
                             java-eclipse-rdf4j-model
                             java-eclipse-rdf4j-repository-api
                             java-eclipse-rdf4j-repository-sail
                             java-eclipse-rdf4j-rio-turtle
                             java-eclipse-rdf4j-rio-nquads
                             java-eclipse-rdf4j-rio-jsonld
                             java-eclipse-rdf4j-rio-rdfxml
                             java-eclipse-rdf4j-sail-memory
                             java-eclipse-rdf4j-sail-nativerdf))
    (native-inputs (list java-commons-rdf-api-tests unzip))
    (synopsis "Implementation RDF 1.1 concepts backed by RDF4J")
    (description "This package provides an RDF4J-based implementation of RDF 1.1
concepts.")))

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

(define-public java-eclipse-rdf4j-http-client
  (package
    (name "java-eclipse-rdf4j-http-client")
    (version %rdf4j-version)
    (source %rdf4j-source)
    (build-system ant-build-system)
    (arguments
     (substitute-keyword-arguments
         (rdf4j-common-arguments "rdf4j-http-client.jar"
                                 "core/http/client")
       ((#:phases phases)
       #~(modify-phases #$phases
           (add-before 'install 'generate-pom.xml
             (generate-pom.xml "guix-pom.xml"
                               "org.eclipse.rdf4j"
                               "rdf4j-http-client" #$version))
           (replace 'install
             (install-from-pom "guix-pom.xml"))))))
    (inputs (list java-commons-io
                  java-httpcomponents-httpcore
                  java-httpcomponents-httpclient
                  java-slf4j-api))
    (propagated-inputs (list java-eclipse-rdf4j-http-protocol
                             java-eclipse-rdf4j-repository-api
                             java-eclipse-rdf4j-queryparser-sparql
                             java-eclipse-rdf4j-queryresultio-api
                             java-eclipse-rdf4j-queryresultio-binary
                             java-eclipse-rdf4j-queryresultio-sparqlxml
                             java-eclipse-rdf4j-queryresultio-sparqljson))
    (home-page "https://rdf4j.org/")
    (synopsis "HTTP client for RDF4J")
    (description "This package provides a client for communicating with RDF4J
servers.")
    (license license:epl1.0)))

(define-public java-eclipse-rdf4j-http-protocol
  (package
    (name "java-eclipse-rdf4j-http-protocol")
    (version %rdf4j-version)
    (source %rdf4j-source)
    (build-system ant-build-system)
    (arguments
     (substitute-keyword-arguments
         (rdf4j-common-arguments "rdf4j-http-protocol.jar"
                                 "core/http/protocol")
       ((#:phases phases)
       #~(modify-phases #$phases
           (add-before 'install 'generate-pom.xml
             (generate-pom.xml "guix-pom.xml"
                               "org.eclipse.rdf4j"
                               "rdf4j-http-protocol" #$version))
           (replace 'install
             (install-from-pom "guix-pom.xml"))))))
    (propagated-inputs (list java-eclipse-rdf4j-rio-ntriples
                             java-eclipse-rdf4j-repository-api
                             java-eclipse-rdf4j-model))
    (home-page "https://rdf4j.org/")
    (synopsis "HTTP protocol for RDF4J")
    (description "This package provides a protocol for communicating RDF
resourcess over HTTP.")
    (license license:epl1.0)))

(define-public java-eclipse-rdf4j-model
  (package
    (name "java-eclipse-rdf4j-model")
    (version %rdf4j-version)
    (source %rdf4j-source)
    (build-system ant-build-system)
    (arguments
     (rdf4j-common-arguments "rdf4j-model.jar"
                             "core/model"))
    (inputs (list java-slf4j-api java-guava))
    (propagated-inputs (list java-eclipse-rdf4j-util
                             java-eclipse-rdf4j-model-api
                             java-eclipse-rdf4j-model-vocabulary))
    (home-page "https://rdf4j.org/")
    (synopsis "Implementation of RDF data model")
    (description "This package provides the RDF model implementation used by
Eclipse RDF4J.")
    (license license:epl1.0)))

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

(define-public java-eclipse-rdf4j-query
  (package
    (name "java-eclipse-rdf4j-query")
    (version %rdf4j-version)
    (source %rdf4j-source)
    (build-system ant-build-system)
    (arguments
     (substitute-keyword-arguments
      (rdf4j-common-arguments "rdf4j-query.jar"
                              "core/query")
      ((#:phases phases)
       #~(modify-phases #$phases
           (add-before 'install 'generate-pom.xml
             (generate-pom.xml "guix-pom.xml"
                               "org.eclipse.rdf4j"
                               "rdf4j-query" #$version))
           (replace 'install
             (install-from-pom "guix-pom.xml"))))))
    (inputs (list java-commons-text
                  java-slf4j-api
                  java-fasterxml-jackson-annotations
                  java-fasterxml-jackson-core
                  java-fasterxml-jackson-databind))
    (propagated-inputs (list java-eclipse-rdf4j-model
                             java-eclipse-rdf4j-rio-api))
    (home-page "https://rdf4j.org/")
    (synopsis "Querying RDF")
    (description "This package provides an interface and implementation for
querying RDF.")
    (license license:epl1.0)))

(define-public java-eclipse-rdf4j-queryalgebra-model
  (package
    (name "java-eclipse-rdf4j-queryalgebra-model")
    (version %rdf4j-version)
    (source %rdf4j-source)
    (build-system ant-build-system)
    (arguments
     (rdf4j-common-arguments "rdf4j-queryalgebra-model.jar"
                             "core/queryalgebra/model"))
    (inputs (list java-fasterxml-jackson-annotations
                  java-guava))
    (propagated-inputs (list java-eclipse-rdf4j-model
                             java-eclipse-rdf4j-query))
    (home-page "https://rdf4j.org/")
    (synopsis "RDF query algebra")
    (description "This package provides an algebra model for RDF queries.")
    (license license:epl1.0)))

(define-public java-eclipse-rdf4j-queryalgebra-evaluation
  (package
    (name "java-eclipse-rdf4j-queryalgebra-evaluation")
    (version %rdf4j-version)
    (source %rdf4j-source)
    (build-system ant-build-system)
    (arguments
     (rdf4j-common-arguments "rdf4j-queryalgebra-evaluation.jar"
                             "core/queryalgebra/evaluation"))
    (inputs (list java-guava
                  java-slf4j-api
                  java-commons-math3
                  java-mapdb))
    (propagated-inputs (list java-eclipse-rdf4j-model
                             java-eclipse-rdf4j-repository-sparql
                             java-eclipse-rdf4j-queryalgebra-model
                             java-eclipse-rdf4j-queryparser-sparql))
    (home-page "https://rdf4j.org/")
    (synopsis "RDF query algebra")
    (description "This package provides evaluation strategies and an
implementation for RDF4J's query algebra.")
    (license license:epl1.0)))

(define-public java-eclipse-rdf4j-queryparser-api
  (package
    (name "java-eclipse-rdf4j-queryparser-api")
    (version %rdf4j-version)
    (source %rdf4j-source)
    (build-system ant-build-system)
    (arguments
     (rdf4j-common-arguments "rdf4j-queryparser-api.jar"
                             "core/queryparser/api"))
    (propagated-inputs (list java-eclipse-rdf4j-query
                             java-eclipse-rdf4j-queryalgebra-model))
    (home-page "https://rdf4j.org/")
    (synopsis "Generic query parser API")
    (description "This package provides a common API for query parsers in
RDF4J.")
    (license license:epl1.0)))

(define-public java-eclipse-rdf4j-queryparser-sparql
  (package
    (name "java-eclipse-rdf4j-queryparser-sparql")
    (version %rdf4j-version)
    (source %rdf4j-source)
    (build-system ant-build-system)
    (arguments
     (rdf4j-common-arguments "rdf4j-queryparser-sparql.jar"
                             "core/queryparser/sparql"))
    (propagated-inputs (list java-eclipse-rdf4j-queryparser-api
                             java-eclipse-rdf4j-model
                             java-eclipse-rdf4j-rio-trig))
    (home-page "https://rdf4j.org/")
    (synopsis "SPARQL query parser")
    (description "This package provides a parser for SPARQL queries.")
    (license license:epl1.0)))

(define-public java-eclipse-rdf4j-queryresultio-api
  (package
    (name "java-eclipse-rdf4j-queryresultio-api")
    (version %rdf4j-version)
    (source %rdf4j-source)
    (build-system ant-build-system)
    (arguments
     (rdf4j-common-arguments "rdf4j-queryresultio-api.jar"
                             "core/queryresultio/api"))
    (propagated-inputs (list java-eclipse-rdf4j-query
                             java-eclipse-rdf4j-rio-api
                             java-eclipse-rdf4j-sail-api))
    (home-page "https://rdf4j.org/")
    (synopsis "Handling RDF query results")
    (description "This package provides an API for handling input and output
on RDF query results.")
    (license license:epl1.0)))

(define-public java-eclipse-rdf4j-queryresultio-binary
  (package
    (name "java-eclipse-rdf4j-queryresultio-binary")
    (version %rdf4j-version)
    (source %rdf4j-source)
    (build-system ant-build-system)
    (arguments
     (rdf4j-common-arguments "rdf4j-queryresultio-binary.jar"
                             "core/queryresultio/binary"))
    (inputs (list java-slf4j-api))
    (propagated-inputs (list java-eclipse-rdf4j-queryresultio-api))
    (home-page "https://rdf4j.org/")
    (synopsis "Handling RDF query results")
    (description "This package provides binary input and output for RDF
queries.")
    (license license:epl1.0)))

(define-public java-eclipse-rdf4j-queryresultio-sparqlxml
  (package
    (name "java-eclipse-rdf4j-queryresultio-sparqlxml")
    (version %rdf4j-version)
    (source %rdf4j-source)
    (build-system ant-build-system)
    (arguments
     (rdf4j-common-arguments "rdf4j-queryresultio-sparqlxml.jar"
                             "core/queryresultio/sparqlxml"))
    (inputs (list java-slf4j-api java-commons-lang3))
    (propagated-inputs (list java-eclipse-rdf4j-queryresultio-api))
    (home-page "https://rdf4j.org/")
    (synopsis "Handling RDF query results")
    (description "This package provides classes for handling SPARQL/XML-based
RDF queries.")
    (license license:epl1.0)))

(define-public java-eclipse-rdf4j-queryresultio-sparqljson
  (package
    (name "java-eclipse-rdf4j-queryresultio-sparqljson")
    (version %rdf4j-version)
    (source %rdf4j-source)
    (build-system ant-build-system)
    (arguments
     (substitute-keyword-arguments
      (rdf4j-common-arguments "rdf4j-queryresultio-sparqljson.jar"
                              "core/queryresultio/sparqljson")
      ((#:phases phases)
       #~(modify-phases #$phases
           (add-before 'install 'generate-pom.xml
             (generate-pom.xml "guix-pom.xml"
                               "org.eclipse.rdf4j"
                               "rdf4j-queryresultio-sparqljson" #$version))
           (replace 'install
             (install-from-pom "guix-pom.xml"))))))
    (inputs (list java-slf4j-api
                  java-commons-lang3
                  java-fasterxml-jackson-core))
    (propagated-inputs (list java-eclipse-rdf4j-queryresultio-api))
    (home-page "https://rdf4j.org/")
    (synopsis "Handling RDF query results")
    (description "This package provides classes for handling SPARQL/JSON-based
RDF queries.")
    (license license:epl1.0)))

(define-public java-eclipse-rdf4j-repository-api
  (package
    (name "java-eclipse-rdf4j-repository-api")
    (version %rdf4j-version)
    (source %rdf4j-source)
    (build-system ant-build-system)
    (arguments
     (rdf4j-common-arguments "rdf4j-repository-api.jar"
                             "core/repository/api"))
    (inputs (list java-slf4j-api java-commons-lang3))
    (propagated-inputs (list java-eclipse-rdf4j-model
                             java-eclipse-rdf4j-rio-api
                             java-eclipse-rdf4j-query))
    (home-page "https://rdf4j.org/")
    (synopsis "Interact with RDF repositories")
    (description "This package provides an API for interacting with repositories
of RDF data.")
    (license license:epl1.0)))

(define-public java-eclipse-rdf4j-repository-sparql
  (package
    (name "java-eclipse-rdf4j-repository-sparql")
    (version %rdf4j-version)
    (source %rdf4j-source)
    (build-system ant-build-system)
    (arguments
     (rdf4j-common-arguments "rdf4j-repository-sparql.jar"
                             "core/repository/sparql"))
    (inputs (list java-httpcomponents-httpclient
                  java-slf4j-api))
    (propagated-inputs (list java-eclipse-rdf4j-http-client
                             java-eclipse-rdf4j-queryparser-sparql
                             java-eclipse-rdf4j-repository-api))
    (home-page "https://rdf4j.org/")
    (synopsis "Repository based on SPARQL")
    (description "This package provides a repository implementation that SPARQL.")
    (license license:epl1.0)))

(define-public java-eclipse-rdf4j-repository-sail
  (package
    (name "java-eclipse-rdf4j-repository-sail")
    (version %rdf4j-version)
    (source %rdf4j-source)
    (build-system ant-build-system)
    (arguments
     (rdf4j-common-arguments "rdf4j-repository-sail.jar"
                             "core/repository/sail"))
    (inputs (list java-httpcomponents-httpclient
                  java-slf4j-api))
    (propagated-inputs (list java-eclipse-rdf4j-queryparser-sparql
                             java-eclipse-rdf4j-queryalgebra-evaluation
                             java-eclipse-rdf4j-repository-api
                             java-eclipse-rdf4j-sail-api))
    (home-page "https://rdf4j.org/")
    (synopsis "Repository based on SAIL")
    (description "This package provides a repository implementation that uses
an RDF4J SAIL stack.")
    (license license:epl1.0)))

(define-public java-eclipse-rdf4j-rio-api
  (package
    (name "java-eclipse-rdf4j-rio-api")
    (version %rdf4j-version)
    (source %rdf4j-source)
    (build-system ant-build-system)
    (arguments
     (substitute-keyword-arguments
         (rdf4j-common-arguments "rdf4j-rio-api.jar"
                                 "core/rio/api")
       ((#:phases phases)
        #~(modify-phases #$phases
            (add-before 'install 'generate-pom.xml
              (generate-pom.xml "guix-pom.xml"
                                "org.eclipse.rdf4j"
                                "rdf4j-rio-api" #$version))
            (replace 'install
              (install-from-pom "guix-pom.xml"))))))
    (inputs (list java-commons-codec java-commons-io java-slf4j-api
                  java-jsonld-java))
    (propagated-inputs (list java-eclipse-rdf4j-model))
    (home-page "https://rdf4j.org/")
    (synopsis "RDF Input/Output API")
    (description "This package provides an API for parsers and writers of
various RDF formats.")
    (license license:epl1.0)))

(define-public java-eclipse-rdf4j-rio-datatypes
  (package
    (name "java-eclipse-rdf4j-rio-datatypes")
    (version %rdf4j-version)
    (source %rdf4j-source)
    (build-system ant-build-system)
    (arguments
     (rdf4j-common-arguments "rdf4j-rio-datatypes.jar"
                             "core/rio/datatypes"))
    (propagated-inputs (list java-eclipse-rdf4j-model
                             java-eclipse-rdf4j-rio-api))
    (home-page "https://rdf4j.org/")
    (synopsis "RDF datatype handlers")
    (description "This package provides datatype handlers used in RDF4J.")
    (license license:epl1.0)))

(define-public java-eclipse-rdf4j-rio-jsonld
  (package
    (name "java-eclipse-rdf4j-rio-jsonld")
    (version %rdf4j-version)
    (source %rdf4j-source)
    (build-system ant-build-system)
    (arguments
     (substitute-keyword-arguments
         (rdf4j-common-arguments "rdf4j-rio-jsonld.jar"
                                 "core/rio/jsonld")
       ((#:phases phases)
        #~(modify-phases #$phases
            (add-before 'install 'generate-pom.xml
              (generate-pom.xml "guix-pom.xml"
                                "org.eclipse.rdf4j"
                                "rdf4j-rio-jsonld" #$version))
            (replace 'install
              (install-from-pom "guix-pom.xml"))))))
    (inputs (list java-commons-io
                  java-slf4j-api
                  java-fasterxml-jackson-core
                  java-fasterxml-jackson-databind
                  java-jsonld-java))
    (propagated-inputs (list java-eclipse-rdf4j-rio-api
                             java-eclipse-rdf4j-rio-datatypes
                             java-eclipse-rdf4j-rio-languages
                             java-eclipse-rdf4j-model))
    (home-page "https://rdf4j.org/")
    (synopsis "RDF JSON-LD serialization")
    (description "This package provides an implementation of the RDF4J Rio API,
which reads and writes JSON-LD.")
    (license license:epl1.0)))

(define-public java-eclipse-rdf4j-rio-languages
  (package
    (name "java-eclipse-rdf4j-rio-languages")
    (version %rdf4j-version)
    (source %rdf4j-source)
    (build-system ant-build-system)
    (arguments
     (rdf4j-common-arguments "rdf4j-rio-languages.jar"
                             "core/rio/languages"))
    (propagated-inputs (list java-eclipse-rdf4j-model
                             java-eclipse-rdf4j-rio-api))
    (home-page "https://rdf4j.org/")
    (synopsis "RDF language handlers")
    (description "This package provides language handlers used in RDF4J.")
    (license license:epl1.0)))

(define-public java-eclipse-rdf4j-rio-ntriples
  (package
    (name "java-eclipse-rdf4j-rio-ntriples")
    (version %rdf4j-version)
    (source %rdf4j-source)
    (build-system ant-build-system)
    (arguments
     (rdf4j-common-arguments "rdf4j-rio-ntriples.jar"
                             "core/rio/ntriples"))
    (inputs (list java-commons-io java-slf4j-api))
    (propagated-inputs (list java-eclipse-rdf4j-rio-api
                             java-eclipse-rdf4j-rio-datatypes
                             java-eclipse-rdf4j-rio-languages
                             java-eclipse-rdf4j-model))
    (home-page "https://rdf4j.org/")
    (synopsis "RDF NTriples serialization")
    (description "This package provides an implementation of the RDF4J Rio API,
which reads and writes NTriples.")
    (license license:epl1.0)))

(define-public java-eclipse-rdf4j-rio-nquads
  (package
    (name "java-eclipse-rdf4j-rio-nquads")
    (version %rdf4j-version)
    (source %rdf4j-source)
    (build-system ant-build-system)
    (arguments
     (rdf4j-common-arguments "rdf4j-rio-nquads.jar"
                             "core/rio/nquads"))
    (inputs (list java-commons-io java-slf4j-api))
    (propagated-inputs (list java-eclipse-rdf4j-rio-api
                             java-eclipse-rdf4j-rio-datatypes
                             java-eclipse-rdf4j-rio-languages
                             java-eclipse-rdf4j-rio-ntriples
                             java-eclipse-rdf4j-model))
    (home-page "https://rdf4j.org/")
    (synopsis "RDF NQuads serialization")
    (description "This package provides an implementation of the RDF4J Rio API,
which reads and writes NQuads.")
    (license license:epl1.0)))

(define-public java-eclipse-rdf4j-rio-rdfxml
  (package
    (name "java-eclipse-rdf4j-rio-rdfxml")
    (version %rdf4j-version)
    (source %rdf4j-source)
    (build-system ant-build-system)
    (arguments
     (rdf4j-common-arguments "rdf4j-rio-rdfxml.jar"
                             "core/rio/rdfxml"))
    (inputs (list java-commons-io java-slf4j-api))
    (propagated-inputs (list java-eclipse-rdf4j-rio-api
                             java-eclipse-rdf4j-rio-datatypes
                             java-eclipse-rdf4j-rio-languages
                             java-eclipse-rdf4j-model))
    (home-page "https://rdf4j.org/")
    (synopsis "RDF XML serialization")
    (description "This package provides an implementation of the RDF4J Rio API,
which reads and writes XML.")
    (license license:epl1.0)))

(define-public java-eclipse-rdf4j-rio-turtle
  (package
    (name "java-eclipse-rdf4j-rio-turtle")
    (version %rdf4j-version)
    (source %rdf4j-source)
    (build-system ant-build-system)
    (arguments
     (rdf4j-common-arguments "rdf4j-rio-turtle.jar"
                             "core/rio/turtle"))
    (inputs (list java-commons-io java-slf4j-api))
    (propagated-inputs (list java-eclipse-rdf4j-rio-api
                             java-eclipse-rdf4j-rio-datatypes
                             java-eclipse-rdf4j-rio-languages
                             java-eclipse-rdf4j-model))
    (home-page "https://rdf4j.org/")
    (synopsis "RDF Turtle serialization")
    (description "This package provides an implementation of the RDF4J Rio API,
which reads and writes Turtle.")
    (license license:epl1.0)))

(define-public java-eclipse-rdf4j-rio-trig
  (package
    (name "java-eclipse-rdf4j-rio-trig")
    (version %rdf4j-version)
    (source %rdf4j-source)
    (build-system ant-build-system)
    (arguments
     (rdf4j-common-arguments "rdf4j-rio-trig.jar"
                             "core/rio/trig"))
    (inputs (list java-commons-io java-slf4j-api))
    (propagated-inputs (list java-eclipse-rdf4j-rio-api
                             java-eclipse-rdf4j-rio-datatypes
                             java-eclipse-rdf4j-rio-languages
                             java-eclipse-rdf4j-rio-turtle
                             java-eclipse-rdf4j-model))
    (home-page "https://rdf4j.org/")
    (synopsis "RDF TriG serialization")
    (description "This package provides an implementation of the RDF4J Rio API,
which reads and writes TriG.")
    (license license:epl1.0)))

(define-public java-eclipse-rdf4j-sail-api
  (package
    (name "java-eclipse-rdf4j-sail-api")
    (version %rdf4j-version)
    (source %rdf4j-source)
    (build-system ant-build-system)
    (arguments
     (rdf4j-common-arguments "rdf4j-sail-api.jar"
                             "core/sail/api"))
    (inputs (list java-slf4j-api))
    (propagated-inputs (list java-eclipse-rdf4j-query
                             java-eclipse-rdf4j-queryalgebra-model
                             java-eclipse-rdf4j-model))
    (home-page "https://rdf4j.org/")
    (synopsis "Interfaces for low-level access to RDF data")
    (description "The @acronym{SAIL, Storage And Interface Layer} API
decouples database implementations and the functional modules of the RDF4J
framework.")
    (license license:epl1.0)))

(define-public java-eclipse-rdf4j-sail-base
  (package
    (name "java-eclipse-rdf4j-sail-base")
    (version %rdf4j-version)
    (source %rdf4j-source)
    (build-system ant-build-system)
    (arguments
     (rdf4j-common-arguments "rdf4j-sail-base.jar"
                             "core/sail/base"))
    (inputs (list java-slf4j-api))
    (propagated-inputs (list java-eclipse-rdf4j-sail-api
                             java-eclipse-rdf4j-queryalgebra-evaluation
                             java-eclipse-rdf4j-queryalgebra-model
                             java-eclipse-rdf4j-query
                             java-eclipse-rdf4j-repository-sail))
    (home-page "https://rdf4j.org/")
    (synopsis "Base implementations of RD4J's storage API")
    (description "This package provides basic implementation of RDF4J's
@acronym{SAIL, Storage And Interface Layer} API.")
    (license license:epl1.0)))

(define-public java-eclipse-rdf4j-sail-memory
  (package
    (name "java-eclipse-rdf4j-sail-memory")
    (version %rdf4j-version)
    (source %rdf4j-source)
    (build-system ant-build-system)
    (arguments
     (rdf4j-common-arguments "rdf4j-sail-memory.jar"
                             "core/sail/memory"))
    (inputs (list java-slf4j-api
                  java-commons-lang3
                  java-guava))
    (propagated-inputs (list java-eclipse-rdf4j-sail-api
                             java-eclipse-rdf4j-sail-base
                             java-eclipse-rdf4j-model
                             java-eclipse-rdf4j-rio-turtle
                             java-eclipse-rdf4j-query
                             java-eclipse-rdf4j-queryalgebra-model
                             java-eclipse-rdf4j-queryalgebra-evaluation))
    (home-page "https://rdf4j.org/")
    (synopsis "Memory-based implementations of RD4J's storage API")
    (description "This package provides an implementation of RDF4J's
@acronym{SAIL, Storage And Interface Layer} API, which stores data in RAM.")
    (license license:epl1.0)))

(define-public java-eclipse-rdf4j-sail-nativerdf
  (package
    (name "java-eclipse-rdf4j-sail-nativerdf")
    (version %rdf4j-version)
    (source %rdf4j-source)
    (build-system ant-build-system)
    (arguments
     (rdf4j-common-arguments "rdf4j-sail-nativerdf.jar"
                             "core/sail/nativerdf"))
    (inputs (list java-slf4j-api
                  java-commons-io
                  java-guava))
    (propagated-inputs (list java-eclipse-rdf4j-sail-api
                             java-eclipse-rdf4j-sail-base))
    (home-page "https://rdf4j.org/")
    (synopsis "File-based implementations of RD4J's storage API")
    (description "This package provides an implementation of RDF4J's
@acronym{SAIL, Storage And Interface Layer} API, which stores data on disk
in various formats.")
    (license license:epl1.0)))

(define-public java-eclipse-rdf4j-util
  (package
    (name "java-eclipse-rdf4j-util")
    (version %rdf4j-version)
    (source %rdf4j-source)
    (build-system ant-build-system)
    (arguments
     (rdf4j-common-arguments "rdf4j-util.jar"
                             "core/util"))
    (inputs (list java-slf4j-api))
    (propagated-inputs (list java-eclipse-rdf4j-model-api))
    (home-page "https://rdf4j.org/")
    (synopsis "RDF4J model interfaces")
    (description "This package provides utility classes used throughout the
RDF4J framework.")
    (license license:epl1.0)))
