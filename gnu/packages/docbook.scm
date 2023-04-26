;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2021-2022 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2016 Mathieu Lirzin <mthl@gnu.org>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020 Marius Bakke <marius@gnu.org>
;;; Copyright © 2021, 2022, 2023 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2021 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2021 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2021 Andrew Whatson <whatson@gmail.com>
;;; Copyright © 2023 Bruno Victal <mirai@makinata.eu>
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

(define-module (gnu packages docbook)
  #:use-module (gnu packages)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages inkscape)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages base)
  #:use-module (gnu packages web-browsers)
  #:use-module (gnu packages xfig)
  #:use-module (gnu packages xml)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system python))

(define-public docbook-xml
  (package
    (name "docbook-xml")
    (version "5.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://docbook.org/xml/" version
                                  "/docbook-v" version "-os.zip"))
              (sha256
               (base32
                "0zqy9prj9wam9dn7v3mgr7ld1axqxdhgrmv06dviwg00ahv43wxk"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:modules '((guix build copy-build-system)
                  (guix build utils)
                  (srfi srfi-26))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-permissions
            (lambda _
              ;; XXX: These files do not need 0755 permission.
              (for-each (cut chmod <> #o644) (find-files "."))))
          (add-before 'install 'patch-catalog-xml
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((xsltproc (search-input-file inputs "/bin/xsltproc"))
                    (dtd-path (string-append #$output "/xml/dtd/docbook")))
                (invoke xsltproc "--nonet" "--noout"
                        "--stringparam" "prefix" dtd-path
                        "--output" "catalog.xml.new"
                        #$(local-file
                           (search-auxiliary-file "xml/patch-catalog-xml.xsl"))
                        "catalog.xml")
                (rename-file "catalog.xml.new" "catalog.xml"))))
          (replace 'install
            (lambda _
              (let ((dtd-path (string-append #$output "/xml/dtd/docbook")))
                (copy-recursively "." dtd-path)))))))
    (native-inputs (list libxslt unzip))
    (home-page "https://docbook.org")
    (synopsis "DocBook XML DTDs for document authoring")
    (description
     "DocBook is general purpose XML and SGML document type particularly well
suited to books and papers about computer hardware and software (though it is
by no means limited to these applications.)  This package provides XML DTDs.")
    (license (license:x11-style "" "See file headers."))))

(define-public docbook-xml-4.5
  (package
    (inherit docbook-xml)
    (version "4.5")
    (source (origin
              (method url-fetch/zipbomb)
              (uri (string-append "https://docbook.org/xml/" version
                                  "/docbook-xml-" version ".zip"))
              (sha256
               (base32
                "1d671lcjckjri28xfbf6dq7y3xnkppa910w1jin8rjc35dx06kjf"))))))

(define-public docbook-xml-4.4
  (package (inherit docbook-xml)
    (version "4.4")
    (source (origin
              (method url-fetch/zipbomb)
              (uri (string-append "https://docbook.org/xml/" version
                                  "/docbook-xml-" version ".zip"))
              (sha256
               (base32
                "141h4zsyc71sfi2zzd89v4bb4qqq9ca1ri9ix2als9f4i3mmkw82"))))))

(define-public docbook-xml-4.3
  (package (inherit docbook-xml)
    (version "4.3")
    (source (origin
              (method url-fetch/zipbomb)
              (uri (string-append "https://docbook.org/xml/" version
                                  "/docbook-xml-" version ".zip"))
              (sha256
               (base32
                "0r1l2if1z4wm2v664sqdizm4gak6db1kx9y50jq89m3gxaa8l1i3"))))))

(define-public docbook-xml-4.2
  (package (inherit docbook-xml)
    (version "4.2")
    (source (origin
              (method url-fetch/zipbomb)
              (uri (string-append "https://docbook.org/xml/" version
                                  "/docbook-xml-" version ".zip"))
              (sha256
               (base32
                "18hgwvmywh6a5jh38szjmg3hg2r4v5lb6r3ydc3rd8cp9wg61i5c"))))))

(define-public docbook-xml-4.1.2
  (package
    (inherit docbook-xml)
    (version "4.1.2")
    (source (origin
              (method url-fetch/zipbomb)
              (uri (string-append "https://docbook.org/xml/" version
                                  "/docbkx412.zip"))
              (sha256
               (base32
                "0wkp5rvnqj0ghxia0558mnn4c7s3n501j99q2isp3sp0ci069w1h"))))
    (arguments
     (substitute-keyword-arguments (package-arguments docbook-xml)
       ((#:phases phases)
        #~(modify-phases #$phases
            (add-after 'unpack 'copy-catalog-file
              ;; docbook-xml-4.1.2 is unique in the fact that it doesn't come
              ;; with a catalog.xml file, requiring it to be generated by hand
              ;; from the docbook.cat SGML catalog. We could automatically
              ;; generate it here at the cost of enlarging the package
              ;; definition with a rudimentary (PEG) parser for the SGML
              ;; catalog but this is overkill since this file is unlikely to
              ;; change, therefore we ship a pre-generated catalog.xml.
              (lambda _
                (copy-file
                 #$(local-file
                    (search-auxiliary-file
                     "xml/docbook-xml/catalog-4.1.2.xml"))
                 "catalog.xml")))
            (add-after 'patch-catalog-xml 'add-rewrite-entries
              (lambda* (#:key inputs #:allow-other-keys)
                (let ((xmlcatalog (search-input-file inputs "/bin/xmlcatalog"))
                      (dtd-path (string-append #$output "/xml/dtd/docbook")))
                  (for-each
                   (lambda (type)
                     (invoke xmlcatalog "--noout"
                             "--add" type
                             "http://www.oasis-open.org/docbook/xml/4.1.2/"
                             (string-append "file://" dtd-path "/")
                             "catalog.xml"))
                   (list "rewriteSystem" "rewriteURI")))))))))
    (native-inputs
     (modify-inputs (package-native-inputs docbook-xml)
       (prepend libxml2)))))

;;; There's an issue in docbook-xsl 1.79.2 that causes manpages to be
;;; generated incorrectly and embed raw nroff syntax such as '.PP' when there
;;; is a namespace/non-namespace mismatch between the sources and the
;;; stylesheets used (see:
;;; https://github.com/docbook/xslt10-stylesheets/issues/109).
(define-public docbook-xsl
  (let ((commit "fe16c90013b64e316c3e21ef92d1e8813c10f88c")
        (revision "0")
        (base-version "1.79.2"))
    (package
      (name "docbook-xsl")
      (version (git-version base-version revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/docbook/xslt10-stylesheets")
                      (commit commit)))
                (file-name (git-file-name name version))
                (modules '((guix build utils)))
                (snippet
                 #~(begin
                     ;; Multiple .jar files are bundled with the sources.
                     (for-each delete-file
                               (find-files "." "\\.jar$"))
                     ;; Do not build webhelp files, as they require a Saxon from
                     ;; 2005, which is not packaged in Guix.
                     (substitute* "xsl/Makefile"
                       ((" webhelp") ""))))
                (sha256
                 (base32
                  "1bl8dwrcy7skrlh80fpsmiw045bv2j0aym231ikcv3hvm2pi98dj"))))
      (build-system gnu-build-system)
      (arguments
       (list
        #:make-flags #~(list "XSLTENGINE=xsltproc")
        #:modules '((guix build gnu-build-system)
                    (guix build utils)
                    (sxml simple))
        #:phases
        #~(let ((dest-path (format #f "~a/xml/xsl/~a-~a"
                                   #$output #$name #$version)))
            (modify-phases %standard-phases
              (replace 'configure
                (lambda _
                  ;; The build systems insist on a ~/.xmlc, and it is simpler to
                  ;; create a dummy config file than to patch it into
                  ;; submission.
                  (setenv "HOME" "/tmp")
                  (call-with-output-file "/tmp/.xmlc"
                    (lambda (port)
                      (sxml->xml
                       '(*TOP*
                         (*PI* xml "version='1.0'")
                         (config
                          (java (@ (xml:id "bigmem"))
                                (java-options (@ (name "Xmx512m"))))
                          (xsltproc (@ (xml:id "xsltproc")
                                       (exec "xsltproc")))
                          (xmllint (@ (xml:id "xmllint")
                                      (exec "xmllint")))))
                       port)))))
              (add-before 'install 'generate-catalog.xml
                (lambda* (#:key make-flags #:allow-other-keys)
                  (apply invoke "make" "-C" "xsl" "catalog.xml" make-flags)))
              (add-before 'install 'patch-catalog-xml
                (lambda* (#:key inputs #:allow-other-keys)
                  (let ((xmlcatalog (search-input-file inputs
                                                       "/bin/xmlcatalog"))
                        (catalog-files (find-files "." "catalog\\.xml$"))
                        (store-uri (string-append "file://" dest-path "/")))
                    (for-each
                     (lambda (catalog)
                       ;; Replace /snapshot/ reference with one based on
                       ;; BASE-VERSION.
                       (let ((versioned-uri
                              (format
                               #f "https://cdn.docbook.org/release/xsl/~a/"
                               #$base-version)))
                         (invoke xmlcatalog "--noout"
                                 "--del"
                                 "https://cdn.docbook.org/release/xsl/snapshot/"
                                 catalog)
                         (for-each
                          (lambda (type)
                            (invoke xmlcatalog "--noout"
                                    "--add" type
                                    versioned-uri
                                    store-uri
                                    catalog))
                          (list "rewriteSystem" "rewriteURI")))

                       ;; Patch /current/ references to point to /gnu/store/….
                       (for-each
                        (lambda (type)
                          (invoke xmlcatalog "--noout"
                                  "--add" type
                                  "https://cdn.docbook.org/release/xsl/current/"
                                  store-uri
                                  catalog))
                        (list "rewriteSystem" "rewriteURI"))

                       ;; Re-add the no longer present compatibility entries for
                       ;; v.1.79.1 or earlier URIs.
                       (for-each
                        (lambda (type)
                          (invoke xmlcatalog "--noout"
                                  "--add" type
                                  "http://docbook.sourceforge.net/release/xsl/current/"
                                  store-uri
                                  catalog))
                        (list "rewriteSystem" "rewriteURI")))
                     catalog-files))))
            (replace 'install
              (lambda _
                (let ((select-rx (make-regexp
                                  "(\\.xml$|\\.xsl$|\\.dtd$|\\.ent$)")))
                  ;; Install catalog.
                  (chdir "xsl")
                  (install-file "catalog.xml" dest-path)
                  (install-file "VERSION.xsl" dest-path)
                  ;; Install style sheets.
                  (for-each
                   (lambda (dir)
                     (for-each (lambda (f)
                                 (install-file
                                  f
                                  (string-append dest-path "/" (dirname f))))
                               (find-files dir select-rx)))
                   '("assembly" "common" "eclipse" "epub" "epub3" "fo"
                     "highlighting" "html" "htmlhelp" "javahelp" "lib"
                     "manpages" "params" "profiling" "roundtrip"
                     "template" "website"
                     "xhtml" "xhtml-1_1" "xhtml5")))))))))
      (native-inputs (list docbook-xml-4.4  ; for tests
                           libxml2
                           libxslt
                           perl
                           perl-xml-xpath))
      (home-page "https://docbook.org")
      (synopsis "DocBook XSL style sheets for document authoring")
      (description
       "This package provides XSL style sheets for DocBook.")
      (license (license:x11-style "" "See 'COPYING' file.")))))

(define-public docbook-xsl-1.79.1
  (package
    (name "docbook-xsl")
    (version "1.79.1")
    (source (origin
              (method url-fetch)
              ;; At the time, the non namespaced version was still the
              ;; default; our latest docbook-xsl is namespaced, so for
              ;; consistency preserves this property for older versions too.
              (uri (string-append "mirror://sourceforge/docbook/"
                                  name "-ns/" version "/"
                                  name "-ns-" version ".tar.bz2"))
              (sha256
               (base32
                "170ggf5dgjar65kkn5n33kvjr3pdinpj66nnxfx8b2avw0k91jin"))
              (modules '((guix build utils)))
              ;; Bundled binary files.
              (snippet
               #~(delete-file-recursively "tools"))))
    (build-system copy-build-system)
    (outputs '("out" "doc"))
    (arguments
     (list
      #:install-plan
      (let ((target (format #f "xml/xsl/~a-~a/" name version))
            (select-rx '("\\.xml$" "\\.xsl$" "\\.dtd$" "\\.ent$")))
        #~`(#$@(map
                (lambda (directory)
                  ;; XXX: When filters are used, the source basename
                  ;; isn't kept under the target path, append it again.
                  (let ((target* (string-append target directory)))
                    (list directory target* #:include-regexp select-rx)))
                (list "assembly" "common" "eclipse" "epub" "epub3" "fo"
                      "highlighting" "html" "htmlhelp" "javahelp" "lib"
                      "manpages" "params" "profiling" "roundtrip"
                      "template" "website"
                      "xhtml" "xhtml-1_1" "xhtml5"))
            ("catalog.xml" #$target)
            ("VERSION.xsl" #$target)))
      #:phases
      #~(let ((dest-path (format #f "~a/xml/xsl/~a-~a"
                                 #$output #$name #$version)))
          (modify-phases %standard-phases
            (add-before 'install 'patch-catalog-xml
              (lambda* (#:key inputs #:allow-other-keys)
                (let ((xmlcatalog (search-input-file inputs
                                                     "/bin/xmlcatalog"))
                      (catalog-files (find-files "." "catalog\\.xml$"))
                      (store-uri (string-append "file://" dest-path "/")))
                  (for-each
                   (lambda (catalog)
                     (for-each
                      (lambda (type)
                        ;; Patch /current/ references to point to /gnu/store/….
                        (invoke xmlcatalog "--noout"
                                "--add" type
                                "http://docbook.sourceforge.net/release/xsl-ns/current/"
                                store-uri
                                catalog)
                        ;; Patch versioned references to point to /gnu/store/….
                        (invoke xmlcatalog "--noout"
                                "--add" type
                                (format
                                 #f "http://docbook.sourceforge.net/release/xsl-ns/~a/"
                                 #$version)
                                store-uri
                                catalog))
                      (list "rewriteSystem" "rewriteURI")))
                   catalog-files))))
            ;; XXX: The copy-build-system doesn't seem to allow installing to a
            ;; different output.
            (add-after 'install 'install-doc
              (lambda _
                (let ((doc (format #f "~a/share/doc/~a-~a"
                                   #$output:doc #$name #$version)))
                  (install-file "NEWS" doc)
                  (install-file "RELEASE-NOTES.html" doc)
                  (copy-recursively "slides" doc)
                  (copy-recursively "webhelp" doc))))))))
    (native-inputs (list libxml2))
    (home-page "https://docbook.org")
    (synopsis "DocBook XSL namespaced style sheets for document authoring")
    (description "This package provides the @emph{namespaced} XSL style sheets
for DocBook.")
    (license (license:x11-style "" "See 'COPYING' file."))))

(define-public docbook-dsssl
  (package
    (name "docbook-dsssl")
    (version "1.79")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/docbook/"
                                  name "/" version "/"
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "1g72y2yyc2k89kzs0lvrb9n7hjayw1hdskfpplpz97pf1c99wcig"))))
    (build-system trivial-build-system)
    (outputs '("out" "doc"))
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((source (assoc-ref %build-inputs "source"))
               (dtd (string-append (assoc-ref %outputs "out")
                                   "/sgml/dtd/docbook"))
               (docbook-dsssl-doc (assoc-ref %build-inputs "docbook-dsssl-doc"))
               (doc (assoc-ref %outputs "doc"))
               (tar (assoc-ref %build-inputs "tar"))
               (bzip2 (assoc-ref %build-inputs "bzip2")))
           (setenv "PATH" (string-append tar "/bin" ":" bzip2 "/bin"))
           (mkdir-p dtd)
           (invoke "tar" "-xf" source "-C" dtd)
           ;; The doc output contains 1.4 MiB of HTML documentation.
           (symlink docbook-dsssl-doc doc)))))
    (inputs
     (list docbook-dsssl-doc))
    (native-inputs
     (list bzip2 tar))
    (home-page "https://docbook.org/")
    (synopsis "DSSSL style sheets for DocBook")
    (description "This package provides DSSSL style sheets for DocBook.")
    (license (license:non-copyleft "file://README"))))

;;; Private variable, used as the 'doc' output of the docbook-dsssl package.
(define docbook-dsssl-doc
  (package
    (name "docbook-dsssl-doc")
    (version "1.79")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/docbook/"
                                  name "/" version "/"
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "1plp5ngc96pbna4rwglp9glcadnirbm3hlcjb4gjvq1f8biic9lz"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((source (assoc-ref %build-inputs "source"))
               (docdir (string-append (assoc-ref %outputs "out")
                                      "/share/doc/" "docbook-dsssl-" ,version))
               (tar (assoc-ref %build-inputs "tar"))
               (bzip2 (assoc-ref %build-inputs "bzip2")))
           (setenv "PATH" (string-append tar "/bin" ":" bzip2 "/bin"))
           (mkdir-p docdir)
           ;; Extract the "doc" subdirectory.
           (invoke "tar" "-xf" source "--strip-components=2"
                   "--no-same-owner" "-C" docdir
                   (string-append "docbook-dsssl-" ,version "/doc"))))))
    (native-inputs
     `(("bzip2" ,bzip2)
       ("tar" ,tar)))
    (home-page "https://docbook.org/")
    (synopsis "DocBook DSSSL style sheets documentation")
    (description "Documentation for the DocBook DSSSL style sheets.")
    (license (license:non-copyleft "file://doc/LEGALNOTICE.htm"))))

(define-public docbook-sgml-4.2
  (package
    (name "docbook-sgml")
    (version "4.2")
    (source (origin
              (method url-fetch/zipbomb)
              (uri (string-append
                    "https://www.oasis-open.org/docbook/sgml/4.2/docbook-"
                    version ".zip"))
              (sha256
               (base32
                "1hrm4qmmzi285bkxkc74lxvjvw2gbl7ycbaxhv31h9rl9g4x5sv7"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:modules '((guix build copy-build-system)
                  (guix build utils)
                  (srfi srfi-26))
      #:install-plan
      #~`(("./" "sgml/dtd/docbook"
           #:exclude-regexp ("catalog\\.xml$"
                             "ChangeLog$"
                             "README$"
                             "\\.txt$")))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-permissions
            (lambda _
              (for-each (cut chmod <> #o644) (find-files "."))))
          (add-before 'install 'patch-iso-entities
            (lambda* (#:key inputs #:allow-other-keys)
              ;; Reference the ISO 8879 character entities.
              ;; e.g. "iso-lat1.gml" --> "<iso-entities-dir>/ISOlat1"
              (let ((iso-entities-dir
                     (assoc-ref %build-inputs "iso-8879-entities")))
                (substitute* "docbook.cat"
                  (("\"iso-(.*)\\.gml\"" _ name)
                   (string-append "\"" iso-entities-dir "/ISO" name "\"")))))))))
    (native-inputs
     (list unzip))
    (inputs
     (list iso-8879-entities))
    (home-page "https://docbook.org")
    (synopsis "DocBook SGML style sheets for document authoring")
    (description "This package provides SGML style sheets for DocBook.")
    (license (license:x11-style "" "See file headers."))))

(define-public docbook-sgml-4.1
  (package
    (inherit docbook-sgml-4.2)
    (version "4.1")
    (source (origin
              (method url-fetch/zipbomb)
              (uri (string-append "https://www.oasis-open.org/docbook/sgml/"
                                  version "/docbk41.zip"))
              (sha256
               (base32
                "04b3gp4zkh9c5g9kvnywdkdfkcqx3kjc04j4mpkr4xk7lgqgrany"))))))

(define-public docbook-sgml docbook-sgml-4.1)

(define-public docbook-sgml-3.1
  (package
    (inherit docbook-sgml)
    (version "3.1")
    (source (origin
              (method url-fetch/zipbomb)
              (uri (string-append "https://www.oasis-open.org/docbook/sgml/"
                                  version "/docbk31.zip"))
              (sha256
               (base32
                "0f25ch7bywwhdxb1qa0hl28mgq1blqdap3rxzamm585rf4kis9i0"))))))

;;; Private package referenced by docbook-sgml.
(define iso-8879-entities
  (package
    (name "iso-8879-entities")
    (version "0.0")                     ;no proper version
    (source (origin
              (method url-fetch/zipbomb)
              (uri "https://www.oasis-open.org/cover/ISOEnts.zip")
              (sha256
               (base32
                "1clrkaqnvc1ja4lj8blr0rdlphngkcda3snm7b9jzvcn76d3br6w"))))
    (build-system copy-build-system)
    (native-inputs (list unzip))
    (home-page "https://www.oasis-open.org/")
    (synopsis "ISO 8879 character entities")
    (description "ISO 8879 character entities that are typically used in
the in DocBook SGML DTDs.")
    (license (license:x11-style "" "See file headers."))))

(define-public dblatex
  (package
    (name "dblatex")
    (version "0.3.12")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/dblatex/dblatex/"
                                  "dblatex-" version "/dblatex3-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "0yd09nypswy3q4scri1dg7dr99d7gd6r2dwx0xm81l9f4y32gs0n"))))
    (build-system python-build-system)
    (arguments
     (list
      ;; Using setuptools causes an invalid "package_base" path in
      ;; out/bin/.dblatex-real due to a missing leading '/'.  This is caused
      ;; by dblatex's setup.py stripping the root path when creating the
      ;; script.  (dblatex's setup.py still uses distutils and thus has to
      ;; create the script by itself. The feature for creating scripts is one
      ;; of setuptools' features.)
      ;; See this thread for details:
      ;; https://lists.gnu.org/archive/html/guix-devel/2016-12/msg00030.html
      #:use-setuptools? #f
      #:tests? #f                       ;no test suite
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'wrap 'set-path
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((path (map (lambda (x)
                                 (string-append (assoc-ref inputs x)
                                                "/bin"))
                               (list "libxslt"
                                     "imagemagick" "inkscape"
                                     "texlive-updmap.cfg"))))
                ;; dblatex executes helper programs at runtime.
                (wrap-program (string-append #$output "/bin/dblatex")
                  `("PATH" ":" prefix ,path))))))))
    (inputs
     (list (texlive-updmap.cfg (list texlive-anysize
                                             texlive-appendix
                                             texlive-changebar
                                             texlive-fancybox
                                             texlive-fancyvrb
                                             texlive-float
                                             texlive-footmisc
                                             texlive-jknapltx
                                             texlive-listings
                                             texlive-multirow
                                             texlive-overpic
                                             texlive-pdfpages
                                             texlive-refcount
                                             texlive-rsfs
                                             texlive-stmaryrd
                                             texlive-subfigure
                                             texlive-titlesec
                                             texlive-wasysym))
           ;; FIXME: transfig causes the build to fail.
           ;;transfig                   ;for fig2dev
           imagemagick                  ;for convert
           inkscape/stable              ;for svg conversion
           docbook-xml
           libxslt))                    ;for xsltproc
    (home-page "https://dblatex.sourceforge.net")
    (synopsis "DocBook to LaTeX Publishing")
    (description
     "DocBook to LaTeX Publishing transforms your SGML/XML DocBook documents
to DVI, PostScript or PDF by translating them in pure LaTeX as a first
process.  MathML 2.0 markups are supported too.  It started as a clone of
DB2LaTeX.")
    ;; lib/contrib/which is under an X11 license
    (license license:gpl2+)))

;; This is a variant of the 'dblatex' package that is not updated often.  It
;; is intended to be used as a native-input at build-time only, e.g. by
;; 'gtk-doc' for generating package documentation.  This allows the main
;; 'dblatex' and 'imagemagick' packages to be freely updated on the 'master'
;; branch without triggering an excessive number of rebuilds.
(define-public dblatex/stable
  (hidden-package
   (package/inherit dblatex
     (inputs (modify-inputs (package-inputs dblatex)
               (replace "imagemagick" imagemagick/stable))))))

(define-public docbook-utils
  (package
    (name "docbook-utils")
    (version "0.6.14")
    (source (origin
              (method url-fetch)
              ;; The original sources are not accessible anymore.
              (uri (string-append "http://deb.debian.org/debian/pool/main/"
                                  "d/docbook-utils/docbook-utils_"
                                  version ".orig.tar.gz"))
              (sha256
               (base32
                "1scj5vgw1xz872pq54a89blcxqqm11p90yzv8a9mqq57x27apyj8"))
              (modules '((guix build utils)))
              (snippet
               #~(begin
                   ;; Patch build system.
                   (substitute* (find-files "." "\\.in$")
                     ;; Do not hard-code SGML_CATALOG_FILES.
                     ((".*SGML_CATALOG_FILES=/etc/sgml/catalog.*") "")
                     ;; Use OpenSP and OpenJade.
                     (("\\bjade\\b")
                      "openjade")
                     (("\\bnsgmls\\b")
                      "onsgmls"))

                   ;; Do not override the SGML_CATALOG_FILES environment
                   ;; variable.
                   (substitute* "bin/jw.in"
                     ((".*SGML_CATALOG_FILES=`find.*")
                      "")
                     (("SGML_CATALOG_FILES=`echo.*")
                      ":\n")
                     (("SGML_CATALOG_FILES=\"\"")
                      ":")
                     (("\\bwhich\\b")
                      "command -v"))

                   ;; Locate lynx, links or w3m from the PATH, not from
                   ;; /usr/bin.
                   (substitute* "backends/txt"
                     (("CONVERT=/usr/bin/")
                      "CONVERT=")
                     (("\\[ -x /usr/bin/([^ ]+) \\]" _ command)
                      (format #f "command -v ~a > /dev/null" command)))))))
    (build-system gnu-build-system)
    ;; Propagated for convenience.  All these tools are used at run time to
    ;; provide the complete functionality of the docbook-utils commands.
    (propagated-inputs
     (list texlive-jadetex
           docbook-sgml-3.1
           docbook-dsssl
           openjade
           opensp
           lynx
           perl-sgmls))
    (home-page "https://packages.debian.org/sid/docbook-utils")
    (synopsis "DocBook converter to other formats")
    (description "The docbook-utils package is a collection of utilities
intended to ease the use of SGML and XML.
@table @command
@item jw
Convert a SGML DocBook file to other formats such as Hyper Text Markup
Language (HTML), Rich Text Format (RTF), PostScript (PS), man, Portable
Document Format (PDF), TeX, Texinfo or plain text (txt).  It can be used
more conveniently via the following wrappers:
@table @command
@item docbook2dvi
Convert a SGML DocBook file to the DVI format.
@item docbook2html
Convert a SGML DocBook file to an HTML document.
@item docbook2man
Convert a SGML DocBook file a man page.
@item docbook2pdf
@itemx docbook2ps
@itemx docbook2rtf
@itemx docbook2tex
@itemx docbook2texi
Convert a SGML DocBook file to a PDF/PS/RTF/TeX document.
@item docbook2txt
Convert a SGML DocBook file to a plain text document.
@end table
@item sgmldiff
Detect the differences in markup between two SGML files.
@end table")
    (license license:gpl2+)))

(define-public docbook2x
  (package
    (name "docbook2x")
    (version "0.8.8")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/docbook2x/docbook2x/"
                                  version "/docbook2X-" version ".tar.gz"))
              (sha256
               (base32
                "0ifwzk99rzjws0ixzimbvs83x6cxqk1xzmg84wa1p7bs6rypaxs0"))))
    (build-system gnu-build-system)
    (inputs
     (list bash-minimal
           docbook-xml-4.5
           perl
           perl-xml-namespacesupport
           perl-xml-parser
           perl-xml-sax
           perl-xml-sax-base
           texinfo
           libxslt))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'configure 'patch-sources
            (lambda* (#:key inputs #:allow-other-keys)
              ;; Fix failed substitution in config.pl
              (substitute* "perl/config.pl"
                (("\\$\\{prefix\\}")
                 #$output))
              ;; Fix a failing test (maybe it worked with old texinfo?)
              (substitute* "test/complete-manuals/at1.xml"
                (("<bridgehead>")
                 "<bridgehead renderas=\"sect2\">"))
              ;; Patch all the tests use DocBook 4.5
              (substitute* (find-files "test" "\\.xml$")
                (("\"-//OASIS//DTD DocBook XML V4\\..+//EN\"")
                 "\"-//OASIS//DTD DocBook XML V4.5//EN\"")
                (("\"http://www\\.oasis-open\\.org/docbook/xml/4\\..+/docbookx.dtd\"")
                 "\"http://www.oasis-open.org/docbook/xml/4.5/docbookx.dtd\""))
              ;; Set XML catalogs for tests to pass
              (setenv "XML_CATALOG_FILES"
                      (string-append (assoc-ref inputs "docbook-xml")
                                     "/xml/dtd/docbook/catalog.xml"))))
          (add-after 'install 'wrap-programs
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((programs
                      (map (lambda (p)
                             (search-input-file outputs
                                                (string-append "bin/" p)))
                           '("db2x_manxml" "db2x_texixml" "db2x_xsltproc"
                             "docbook2man" "docbook2texi")))
                     (perl5lib
                      '#$(map (lambda (i)
                                (file-append (this-package-input i)
                                             "/lib/perl5/site_perl"))
                              '("perl-xml-namespacesupport"
                                "perl-xml-parser"
                                "perl-xml-sax"
                                "perl-xml-sax-base")))
                     (xml-catalog-files
                      (list (search-input-file
                             inputs "xml/dtd/docbook/catalog.xml"))))
                (map (lambda (program)
                       (wrap-program program
                         `("PERL5LIB" ":" prefix ,perl5lib)
                         `("XML_CATALOG_FILES" " " prefix ,xml-catalog-files)))
                     programs))))
          (add-after 'install 'create-symlinks
            (lambda _
              ;; Create db2x_* symlinks to satisfy some configure scripts
              ;; which use these names to differentiate from an older
              ;; docbook2man script provided by docbook-utils.
              (map (lambda (prog)
                     (symlink prog (string-append #$output
                                                  "/bin/db2x_" prog)))
                   '("docbook2man" "docbook2texi")))))))
    (home-page "https://docbook2x.sourceforge.net")
    (synopsis "Convert DocBook to man page and Texinfo format")
    (description
     "docbook2X is a software package that converts DocBook documents into the
traditional Unix man page format and the GNU Texinfo format.  Notable features
include table support for man pages, internationalization support, and easy
customization of the output using XSLT.")
    (license license:expat)))
