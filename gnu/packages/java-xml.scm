;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2021, 2023 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017, 2018, 2021 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2018 Gábor Boskovits <boskovits@gmail.com>
;;; Copyright © 2019, 2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020 Vincent Legoll <vincent.legoll@gmail.com>
;;; Copyright © 2021 Léo Le Bouter <lle-bout@zaclys.net>
;;; Copyright © 2022 Christopher Baines <mail@cbaines.net>
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

(define-module (gnu packages java-xml)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages java)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix bzr-download)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system ant)
  #:use-module (guix utils))

(define-public java-jericho-html
  (package
    (name "java-jericho-html")
    (version "3.4")
    (source (origin
              (method bzr-fetch)
              (uri (bzr-reference (url (string-append
                                        "http://jerichohtml.bzr.sourceforge.net/"
                                        "bzr/jerichohtml"))
                                  (revision (string-append "tag:" version))))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "1zsf4i33jn05pma4y1658d6avhw7x4c12ggs96szhc06b7bxs8j0"))
              (modules '((guix build utils)))
              (snippet '(begin
                          (format #t "~%~a~%" "Removing sourced jar files.")
                          (for-each (lambda (jarf)
                                      (delete-file jarf)
                                      (format #t "Deleted: ~a~%" jarf))
                                    (find-files "." "\\.jar$"))))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name (string-append ,name ".jar")
       #:phases (modify-phases %standard-phases
                  (add-before 'build 'add-ant-env-options
                    (lambda* _
                      (setenv "ANT_OPTS" "-Dfile.encoding=iso-8859-1")
                      (let ((match-str (string-append "jerichohtml-"
                                                      ,version)))
                        (substitute* "build.xml"
                          ((match-str)
                           "")))))
                  (add-after 'build 'check-prep
                    (lambda* (#:key source #:allow-other-keys)
                      (mkdir-p "src/test/java"))))))
    (native-inputs (list
                    java-commons-logging-minimal
                    java-junit
                    java-log4j-api
                    java-slf4j-api))
    (home-page "http://jericho.htmlparser.net/docs/index.html")
    (synopsis "Java HTML Parser library")
    (description
     "This Java library allowing analysis and manipulation of
parts of an HTML document, including server-side tags, while
reproducing verbatim any unrecognised or invalid HTML.  It also
provides high-level HTML form manipulation functions.")
    (license (list license:lgpl2.1+ license:asl2.0 license:epl1.0))))

(define-public java-simple-xml
  (package
    (name "java-simple-xml")
    (version "2.7.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/simple/simple-xml-"
                                  version ".zip"))
              (sha256
               (base32
                "0w19k1awslmihpwsxwjbg89hv0vjhk4k3i0vrfchy3mqknd988y5"))
              (patches (search-patches "java-simple-xml-fix-tests.patch"))))
    (build-system ant-build-system)
    (arguments
     `(#:build-target "build"
       #:test-target "test"
       #:phases
       (modify-phases %standard-phases
         (replace 'install (install-jars "jar"))
         (add-before 'check 'disable-failing-test
           (lambda _
             ;; This test sometimes fails with an out of memory exception
             (delete-file
              "test/src/org/simpleframework/xml/core/NoAnnotationsRequiredTest.java"))))))
    (native-inputs
     (list unzip))
    (home-page "https://simple.sourceforge.net/")
    (synopsis "XML serialization framework for Java")
    (description "Simple is a high performance XML serialization and
configuration framework for Java.  Its goal is to provide an XML framework
that enables rapid development of XML configuration and communication systems.
This framework aids the development of XML systems with minimal effort and
reduced errors.  It offers full object serialization and deserialization,
maintaining each reference encountered.")
    (license license:asl2.0)))

;; TODO: Debian builds several jars out of this: jaxp-1.4.jar,
;; xml-apis.jar and xml-apis-1.4.01.jar.
(define-public java-jaxp
  (package
    (name "java-jaxp")
    (version "1.4.01")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://apache/xerces/xml-commons/source/"
                           "xml-commons-external-" version "-src.tar.gz"))
       (sha256
        (base32 "0rhq32a7dl9yik7zx9h0naz2iz068qgcdiayak91wp4wr26xhjyk"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "jaxp.jar"
       #:jdk ,icedtea-8
       #:source-dir ".."
       #:tests? #f)); no tests
    (home-page "https://xerces.apache.org/xml-commons/")
    (synopsis "Java XML parser and transformer APIs (DOM, SAX, JAXP, TrAX)")
    (description "Jaxp from the Apache XML Commons project is used by
the Xerces-J XML parser and Xalan-J XSLT processor and specifies these APIs:

@itemize
@item Document Object Model (DOM)
@item Simple API for XML (SAX)
@item Java APIs for XML Processing (JAXP)
@item Transformation API for XML (TrAX)
@item Document Object Model (DOM) Load and Save
@item JSR 206 Java API for XML Processing
@end itemize")
    (license (list license:asl2.0
                   license:w3c ;; Files under org.w3c
                   license:public-domain)))) ;; org.xml.sax

(define-public java-apache-xml-commons-resolver
  (package
    (name "java-apache-xml-commons-resolver")
    (version "1.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://apache/xerces/xml-commons/"
                           "xml-commons-resolver-" version ".tar.gz"))
       (sha256
        (base32 "1zhy4anc3fg9f8y348bj88vmab15aavrg6nf419ifb25asyygnsm"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           (for-each delete-file (find-files "." ".*\\.(jar|zip)"))
           #t))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name (string-append "xml-resolver.jar")
       #:tests? #f)); no tests
    (inputs
     (list java-junit))
    (home-page "https://xerces.apache.org/xml-commons/")
    (synopsis "Catalog-based entity and URI resolution")
    (description "The resolver class implements the full semantics of OASIS Technical
Resolution 9401:1997 (Amendment 2 to TR 9401) catalogs and the 06 Aug
2001 Committee Specification of OASIS XML Catalogs.

It also includes a framework of classes designed to read catalog files
in a number of formats:

@itemize
@item The plain-text flavor described by TR9401.
@item The XCatalog XML format defined by John Cowan
@item The XML Catalog format defined by the OASIS Entity Resolution
      Technical Committee.
@end itemize")
    (license license:asl2.0)))

;; Jaxen requires java-dom4j and java-xom that in turn require jaxen.
;; This package is a bootstrap version without dependencies on dom4j and xom.
(define java-jaxen-bootstrap
  (package
    (name "java-jaxen-bootstrap")
    (version "1.1.6")
    (source (origin
              (method url-fetch)
              ;; No release on github
              (uri (string-append "https://repo1.maven.org/maven2/jaxen/jaxen/"
                                  version "/jaxen-" version "-sources.jar"))
              (sha256
               (base32
                "18pa8mks3gfhazmkyil8wsp6j1g1x7rggqxfv4k2mnixkrj5x1kx"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "jaxen.jar"
       #:source-dir "src"
       #:tests? #f; no tests
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'remove-dom4j
           (lambda _
             (delete-file-recursively "src/org/jaxen/dom4j")
             (delete-file-recursively "src/org/jaxen/xom")
             #t)))))
    (inputs
     `(("java-jdom" ,java-jdom)))
    (home-page "https://github.com/jaxen-xpath/jaxen")
    (synopsis "XPath library")
    (description "Jaxen is an XPath library written in Java.  It is adaptable
to many different object models, including DOM, XOM, dom4j, and JDOM.  It is
also possible to write adapters that treat non-XML trees such as compiled
Java byte code or Java beans as XML, thus enabling you to query these trees
with XPath too.")
    (license license:bsd-3)))

(define-public java-jaxen
  (package
    (inherit java-jaxen-bootstrap)
    (name "java-jaxen")
    (inputs
     (list java-jdom java-xom java-dom4j))))

(define-public java-xom
  (package
    (name "java-xom")
    (version "127")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/elharo/xom")
                    (commit (string-append "XOM_" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1jh6y03g5zzdhsb5jm6ms1xnamr460qmn96y3w6aw0ikfwqlg0bq"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (for-each delete-file
                            (find-files "." "\\.jar$"))
                  #t))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "xom.jar"
       #:jdk ,icedtea-8
       #:tests? #f                      ; no tests
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'make-git-checkout-writable
           (lambda _
             (for-each make-file-writable (find-files "."))
             #t))
         (add-before 'configure 'fix-tagsoup-dep
           (lambda _
             ;; FIXME: Where is tagsoup source?
             (delete-file "src/nu/xom/tools/XHTMLJavaDoc.java")
             #t)))))
    (inputs
     (list java-jdom java-junit java-classpathx-servletapi
           java-jaxen-bootstrap java-xerces))
    (home-page "https://xom.nu/")
    (synopsis "XML Object Model")
    (description "XOM is a new XML Object Model for processing XML with Java 
that strives for correctness and simplicity.")
    ;; 2.1 only
    (license license:lgpl2.1)))

(define-public java-xsdlib
  (package
    (name "java-xsdlib")
    (version "2013.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://repo1.maven.org/maven2/com/sun/msv/"
                                  "datatype/xsd/xsdlib/" version "/xsdlib-"
                                  version "-sources.jar"))
              (sha256
               (base32
                "185i48p1xp09wbq03i9zgfl701qa262rq46yf4cajzmk3336kqim"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f; no tests
       #:jar-name "xsdlib.jar"
       #:jdk ,icedtea-8))
    (inputs
     (list java-xerces))
    (home-page (string-append "https://web.archive.org/web/20161127144537/"
                              "https://msv.java.net//"))
    (synopsis "Sun Multi-Schema Validator")
    (description "Xsdlib contains an implementation of sun.com.msv, an XML
validator.")
    (license license:bsd-2)))

(define-public java-xpp3
  (package
    (name "java-xpp3")
    (version "1.1.4")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://ci.guix.gnu.org/file/"
                    "xpp3-1.1.4_src.tgz"
                    "/sha256/"
                    "1b99zrhyij5qwyhilyjdl1ykxvhk902vsvflh6gx4fir8hfvdl5p"))
              (file-name (string-append name "-" version "_src.tgz"))
              (sha256
               (base32
                "1b99zrhyij5qwyhilyjdl1ykxvhk902vsvflh6gx4fir8hfvdl5p"))
              (modules '((guix build utils)))
              (snippet
                '(begin ;; Delete bundled jar archives.
                   (for-each delete-file (find-files "." ".*\\.jar"))
                   #t))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f; no tests
       #:build-target "jar"
       #:phases
       (modify-phases %standard-phases
         (replace 'install (install-jars "build")))))
    (home-page "http://www.extreme.indiana.edu/xgws/xsoap/xpp/")
    (synopsis "Streaming pull XML parser")
    (description "Xml Pull Parser (in short XPP) is a streaming pull XML
parser and should be used when there is a need to process quickly and
efficiently all input elements (for example in SOAP processors). This
package is a stable XmlPull parsing engine that is based on ideas from XPP
and in particular XPP2 but completely revised and rewritten to take the best
advantage of JIT JVMs.")
    (license (license:non-copyleft "file://LICENSE.txt"))))

(define-public java-xmlpull2
  (package
    (name "java-xmlpull2")
    (version "2.1.10")
    (source (origin
              (method url-fetch)
              ;; Unfortunately, archive.org does not have a copy of this file.
              (uri (string-append "https://ftp.fau.de/gentoo/distfiles/"
                                  "PullParser" version ".tgz"))
              (sha256
               (base32
                "1kw9nhyqb7bzhn2zjbwlpi5vp5rzj89amzi3hadw2acyh2dmd0md"))
              (modules '((guix build utils)))
              (snippet
                '(begin ;; Delete bundled jar archives.
                   (for-each delete-file (find-files "." ".*\\.jar"))
                   #t))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f; no tests
       #:build-target "impl"
       #:phases
       (modify-phases %standard-phases
         (replace 'install (install-jars "build/lib")))))
    (home-page (string-append "https://web.archive.org/web/20210225153105/"
                              "https://www.extreme.indiana.edu/"))
    (synopsis "Streaming pull XML parser")
    (description "Xml Pull Parser (in short XPP) is a streaming pull XML
parser and should be used when there is a need to process quickly and
efficiently all input elements (for example in SOAP processors).  This
package is in maintenance mode.")
    (license (license:non-copyleft "file:///LICENSE.txt"))))

(define-public java-xmlpull-api-v1
  (package
    (name "java-xmlpull-api-v1")
    (version "1.1.3.4b")
    (source (origin
              ;; The package is originally from Extreme! Lab, but the website
              ;; is now gone.  This repositories contains the sources of the
              ;; latest version.
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/aslom/xmlpull-api-v1")
                     ;; No releases, this is the latest commit
                     (commit "abeaa4aa87b2625af70c32f658f44e11355fe568")))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "15bdqxfncnakskna4m9gsh4f9iczxy83qxn2anqiqd15z406a5ih"))
              (modules '((guix build utils)))
              (snippet
               `(begin
                  (delete-file-recursively "lib")
                  (mkdir-p "lib")
                  ;; prevents a failure in "dist_lite"
                  (substitute* "build.xml"
                    (("README.html") "README.md"))))))
    (build-system ant-build-system)
    (arguments
     `(#:test-target "junit"
       #:build-target "dist"
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (doc (string-append out "/share/doc/" ,name "-" ,version))
                    (java (string-append out "/share/java"))
                    (project (string-append
                               "xmlpull_"
                               ,(string-join (string-split version #\.) "_"))))
               (mkdir-p doc)
               (copy-recursively (string-append "build/dist/" project "/doc/")
                                 doc)
               (mkdir-p java)
               (copy-file (string-append "build/dist/" project "/" project ".jar")
                          (string-append java "/" project ".jar")))
             )))))
    (home-page "https://github.com/aslom/xmlpull-api-v1")
    (synopsis "XML pull parsing API")
    (description "XmlPull v1 API is a simple to use XML pull parsing API.  XML
pull parsing allows incremental (sometimes called streaming) parsing of XML
where application is in control - the parsing can be interrupted at any given
moment and resumed when application is ready to consume more input.")
    (license license:public-domain)))

(define-public java-dom4j
  (package
    (name "java-dom4j")
    (version "2.1.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/dom4j/dom4j")
                    (commit (string-append "version-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0q907srj9v4hwicpcrn4slyld5npf2jv7hzchsgrg29q2xmbwkdl"))
              (modules '((guix build utils)))
              (snippet
                '(begin ;; Delete bundled jar archives.
                   (for-each delete-file (find-files "." ".*\\.jar"))
                   #t))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "dom4j.jar"
       #:jdk ,icedtea-8
       #:source-dir "src/main/java"
       ;; FIXME: Requires xalan, but xalan depends on java-cup which has a
       ;; dependency on itself through jflex.
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-jaxen-sources
           ;; java-jaxen-bootstrap is not enough. These files have a circular
           ;; dependency and there is no subset of dom4j that would allow
           ;; breaking the circle.
           (lambda* (#:key inputs #:allow-other-keys)
             (mkdir-p "jaxen-sources")
             (with-directory-excursion "jaxen-sources"
               (system* "jar" "xf" (assoc-ref inputs "java-jaxen-sources")))
             (mkdir-p "src/main/java/org/jaxen/dom4j")
             (copy-file "jaxen-sources/org/jaxen/dom4j/DocumentNavigator.java"
                        "src/main/java/org/jaxen/dom4j/DocumentNavigator.java")
             (copy-file "jaxen-sources/org/jaxen/dom4j/Dom4jXPath.java"
                        "src/main/java/org/jaxen/dom4j/Dom4jXPath.java")
             #t))
         (add-before 'build 'fix-old-xpp2
           (lambda _
             ;; This package normally depends on xpp2 2.0, but version 2.1.10
             ;; is the only version whose source code is published.
             (substitute* "src/main/java/org/dom4j/xpp/ProxyXmlStartTag.java"
               (("public void resetStartTag")
                "public boolean removeAttributeByRawName(String name) {\n
  return false;\n
}\n
public boolean removeAttributeByName(String name, String name2) {\n
  return false;\n
}\n\npublic void resetStartTag")
               (("Atttribute") "Attribute"))
             #t)))))
    (inputs
     `(("java-jaxen-bootstrap" ,java-jaxen-bootstrap)
       ("java-jaxen-sources" ,(package-source java-jaxen-bootstrap))
       ("java-xmlpull2" ,java-xmlpull2)
       ("java-xpp3" ,java-xpp3)
       ("java-xsdlib" ,java-xsdlib)))
    (native-inputs
     (list java-testng java-xerces))
    (home-page "https://dom4j.github.io/")
    (synopsis "Flexible XML framework for Java")
    (description "Dom4j is a flexible XML framework for Java.  DOM4J works
with DOM, SAX, XPath, and XSLT.  It can parse large XML documents with very
low memory footprint.")
    ;; some BSD-like 5-clause license
    (license (license:non-copyleft "file://LICENSE"))))

(define-public java-kxml2
  (package
    (name "java-kxml2")
    (version "2.4.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/stefanhaustein/kxml2")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0g6d8c9r9sh3x04sf4wdpgwvhkqvk11k3kq9skx91i60h4vn01hg"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "kxml2.jar"
       #:source-dir "src/main/java"
       #:test-include (list "TestWb.java")
       ;; Test failure: it was expected to get an XML entity but got the
       ;; equivalent Unicode character instead.
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'make-git-checkout-writable
           (lambda _
             (for-each make-file-writable (find-files "."))
             #t))
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "src/main/resources" "build/classes")
             #t)))))
    (inputs
     (list java-xpp3))
    (native-inputs
     (list java-junit))
    (home-page "http://kxml.org")
    (synopsis "XML pull parser")
    (description "kXML is a small XML pull parser, specially designed for
constrained environments such as Applets, Personal Java or devices compliant
with the Mobile Information Device Profile (MIDP).")
    (license license:expat)))

(define-public java-stax
  (package
    (name "java-stax")
    (version "1.2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://repo1.maven.org/maven2/stax/stax/"
                                  version "/stax-" version "-sources.jar"))
              (sha256
               (base32
                "04ba4qvbrps45j8bldbakxq31k7gjlsay9pppa9yn13fr00q586z"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "stax.jar"
       #:tests? #f; no tests
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'fix-utf8
           (lambda _
             ;; This file is ISO-8859-1 but java expects UTF-8.
             ;; Remove special characters in comments.
             (with-fluids ((%default-port-encoding "ISO-8859-1"))
               (substitute* "src/com/wutka/dtd/Scanner.java"
                 (("//.*") "\n")))
             #t)))))
    (home-page "https://repo1.maven.org/maven2/stax/stax/")
    (synopsis "Streaming API for XML")
    (description "This package provides the reference implementation of the
@dfn{Streaming API for XML} (StAX).  It is used for streaming XML data to
and from a Java application.  It provides a standard pull parser interface.")
    (license license:asl2.0)))

(define-public java-jettison
  (package
    (name "java-jettison")
    (version "1.3.7")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/codehaus/jettison")
                    (commit (string-append "jettison-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "15sydmi5chdh4126qc7v8bsrp7fp4ldaya8a05iby4pq2324q0qw"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "jettison.jar"
       #:source-dir "src/main/java"
       #:test-exclude (list "**/Abstract*.java"
                            ;; Abstract classes
                            "**/DOMTest.java"
                            "**/BadgerFishDOMTest.java"
                            "**/MappedDOMTest.java")))
    (native-inputs
     (list java-junit))
    (home-page "https://github.com/codehaus/jettison")
    (synopsis "StAX implementation for JSON")
    (description "Jettison is a Java library for converting XML to JSON and
vice-versa with the help of the @dfn{Streaming API for XML} (StAX).  It
implements @code{XMLStreamWriter} and @code{XMLStreamReader} and supports
@code{Mapped} and @code{BadgerFish} conventions.")
    (license license:asl2.0)))

(define-public java-jdom2
  (package
    (name "java-jdom")
    (version "2.0.6.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/hunterhacker/jdom")
                    (commit (string-append "JDOM-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1r4pwl0z7hm45v9l2wbq3fjmqi13zmwzbrggyqizrwv31kghhx56"))))
    (build-system ant-build-system)
    (arguments
     `(#:build-target "package"
       #:tests? #f                ; tests are run as part of the build process
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (install-jars "build")))))
    (home-page "http://jdom.org/")
    (synopsis "Access, manipulate, and output XML data")
    (description "Jdom is a Java-based solution for accessing, manipulating, and
outputting XML data from Java code.")
    (license license:bsd-4)))

(define-public java-xstream
  (package
    (name "java-xstream")
    (version "1.4.16")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/x-stream/xstream")
             (commit (string-append
                      "XSTREAM_"
                      (string-map (lambda (x) (if (eq? x #\.) #\_ x))
                                  version)))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "16k2mc63h2fw7lxv74qmhg4p8q9hfrw114daa6nxwnpv08cnq755"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "xstream.jar"
       ;; FIXME: Tests are not in a java subdirectory as assumed by ant-build-system.
       #:tests? #f
       #:jdk ,icedtea-8
       #:source-dir "xstream/src/java"))
    (inputs
     `(("java-jdom" ,java-jdom)
       ("java-jdom2" ,java-jdom2)
       ("java-cglib" ,java-cglib)
       ("java-joda-time" ,java-joda-time)
       ("java-jettison" ,java-jettison)
       ("java-xom" ,java-xom)
       ("java-mxparser" ,java-mxparser)
       ("java-xpp3" ,java-xpp3)
       ("java-dom4j" ,java-dom4j)
       ("java-stax2-api" ,java-stax2-api)
       ("java-woodstox-core" ,java-woodstox-core)
       ("java-kxml2" ,java-kxml2)
       ("java-stax" ,java-stax)))
    (home-page "https://x-stream.github.io")
    (synopsis "XML serialization library")
    (description "XStream is a simple library to serialize Java objects to XML
and back again.")
    (license license:bsd-3)))

(define-public java-mxparser
  (package
    (name "java-mxparser")
    (version "1.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/x-stream/mxparser")
              (commit (string-append "v-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0i3jrjbz4hgf62fm1ix7nlcmhi4kcv4flqsfvh7a3l2v7nsp5ryb"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "mxparser.jar"
       #:source-dir "src/main/java"
       #:test-dir "src/test"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "src/main/resources" "build/classes")
             #t)))))
    (propagated-inputs
     (list java-xmlpull-api-v1))
    (native-inputs
     (list java-junit))
    (home-page "https://github.com/x-stream/mxparser")
    (synopsis "Streaming pull XML parser forked from @code{java-xpp3}")
    (description "Xml Pull Parser (in short XPP) is a streaming pull XML
parser and should be used when there is a need to process quickly and
efficiently all input elements (for example in SOAP processors). This
package is a stable XmlPull parsing engine that is based on ideas from XPP
and in particular XPP2 but completely revised and rewritten to take the best
advantage of JIT JVMs.

MXParser is a fork of xpp3_min 1.1.7 containing only the parser with merged
changes of the Plexus fork. It is an implementation of the XMLPULL V1 API
(parser only).")
    (license (license:non-copyleft "file://LICENSE.txt"))))










