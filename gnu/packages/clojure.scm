;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Alex Vong <alexvong1995@gmail.com>
;;; Copyright © 2018 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Jesse Gibbons <jgibbons2357+guix@gmail.com>
;;; Copyright © 2020 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu packages clojure)
  #:use-module (gnu packages)
  #:use-module (gnu packages java)
  #:use-module (gnu packages maven)
  #:use-module (gnu packages readline)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system ant)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system clojure)
  #:use-module (ice-9 match))

(define-public clojure
  (let* ((lib (lambda (prefix version hash)
                (origin (method url-fetch)
                        (uri (string-append "https://github.com/clojure/"
                                            prefix version ".tar.gz"))
                        (sha256 (base32 hash)))))
         ;; The libraries below are needed to run the tests.
         (libraries
          `(("core-specs-alpha-src"
             ,(lib "core.specs.alpha/archive/v"
                   "0.2.62"
                   "0v6nhghsigpzm8y7dykfm318q5dvk5l8sykmn1hr0qgs1jsjqh9j"))
            ("data-generators-src"
             ,(lib "data.generators/archive/data.generators-"
                   "1.0.0"
                   "0s3hf1njvs68b8igasikvzagzqxl0gbri7w2qhzsypkhfh60v2cp"))
            ("java-classpath-src"
             ,(lib "java.classpath/archive/java.classpath-"
                   "1.0.0"
                   "178zajjsc9phk5l61r8w9hcpk0wgc9a811pl7kjgvn7rg4l7fh7j"))
            ("spec-alpha-src"
             ,(lib "spec.alpha/archive/v"
                   "0.3.218"
                   "0h5nd9xlind1a2vmllr2yfhnirgj2pm5dndgqzrly78l5iwcc3wa"))
            ("test-check-src"
             ,(lib "test.check/archive/v"
                   "1.1.1"
                   "0kx8l79mhpnn94rpsgc7nac7gb222g7a47mzrycj8crfc54wf0c1"))
            ("test-generative-src"
             ,(lib "test.generative/archive/test.generative-"
                   "1.0.0"
                   "0yy2vc38s4j5n94jdcjx1v7l2gdq0lywam31id1jh07sx37lv5il"))
            ("tools-namespace-src"
             ,(lib "tools.namespace/archive/tools.namespace-"
                   "1.0.0"
                   "1ifpk93m33rj2xm1qnnninlsdvm1liqmsp9igr63pjjwwwjw1cnn"))
            ("tools-reader-src"
             ,(lib "tools.reader/archive/tools.reader-"
                   "1.3.2"
                   "1n4dhg61iyypnjbxmihhqjb7lfpc0lzfvlk4jd8w0yr6za414f3a"))))
         (library-names (match libraries
                          (((library-name _) ...)
                           library-name))))

    (package
      (name "clojure")
      (version "1.11.1")
      (source (let ((name+version (string-append name "-" version)))
                (origin
                  (method git-fetch)
                  (uri (git-reference
                        (url "https://github.com/clojure/clojure")
                        (commit name+version)))
                  (file-name (string-append name+version "-checkout"))
                  (sha256
                   (base32 "1xbab21rm9zvhmw1i2h5lqm7612vrdkxprq0rgb2i3sbgsxcdsn4")))))
      (build-system ant-build-system)
      (inputs
       `(("jre" ,icedtea)))
      (arguments
       `(#:imported-modules ((guix build clojure-utils)
                             (guix build clojure-build-system)
                             (guix build guile-build-system)
                             ,@%ant-build-system-modules)
         #:modules ((guix build ant-build-system)
                    ((guix build clojure-build-system) #:prefix clj:)
                    (guix build clojure-utils)
                    (guix build java-utils)
                    (guix build utils)
                    (guix build syscalls)
                    (ice-9 match)
                    (ice-9 regex)
                    (srfi srfi-26))
         #:test-target "test"
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'unpack-library-sources
             (lambda* (#:key inputs #:allow-other-keys)
               (define (extract-library name)
                 (mkdir-p name)
                 (with-directory-excursion name
                   (invoke "tar"
                           "--extract"
                           "--verbose"
                           "--file" (assoc-ref inputs name)
                           "--strip-components=1")))
               (for-each extract-library ',library-names)
               (copy-recursively "core-specs-alpha-src/src/main/clojure"
                                 "src/clj/")
               (copy-recursively "spec-alpha-src/src/main/clojure"
                                 "src/clj/")
               #t))
           (add-after 'unpack-library-sources 'fix-manifest-classpath
             (lambda _
               (substitute* "build.xml"
                 (("<attribute name=\"Class-Path\" value=\".\"/>") ""))
               #t))
           (add-after 'unpack-library-sources 'clojure-spec-skip-macros
             ;; Disable spec macro instrumentation when compiling clojure.spec
             ;; See: https://clojure.atlassian.net/browse/CLJ-2254
             (lambda _
               (substitute* "build.xml"
                 (("<sysproperty key=\"java.awt.headless\" value=\"true\"/>")
                  ,(string-join
                    '("<sysproperty key=\"java.awt.headless\" value=\"true\"/>"
                      "<sysproperty key=\"clojure.spec.skip-macros\" value=\"true\"/>\n")
                    "\n")))
               #t))
           (add-after 'unpack-library-sources 'clojure-spec-compile
             ;; Compile and include clojure.spec.alpha & clojure.core.specs.alpha
             (lambda _
               (substitute* "build.xml"
                 (("<arg value=\"clojure.math\"/>")
                  ,(string-join
                    '("<arg value=\"clojure.math\"/>"
                      "<arg value=\"clojure.spec.alpha\"/>"
                      "<arg value=\"clojure.spec.gen.alpha\"/>"
                      "<arg value=\"clojure.spec.test.alpha\"/>"
                      "<arg value=\"clojure.core.specs.alpha\"/>"))))
               #t))
           (add-before 'build 'maven-classpath-properties
             (lambda _
               (define (make-classpath libraries)
                 (string-join (map (lambda (library)
                                     (string-append library "/src/main/clojure"))
                                   libraries) ":"))
               (with-output-to-file "maven-classpath.properties"
                 (lambda ()
                   (let ((classpath (make-classpath ',library-names)))
                     (display (string-append "maven.compile.classpath=" classpath "\n"))
                     (display (string-append "maven.test.classpath=" classpath "\n")))))
               #t))
           (add-after 'build 'build-javadoc ant-build-javadoc)
           (replace 'install (install-jars "./"))
           (add-after 'install-license-files 'install-doc
             (cut install-doc #:doc-dirs '("doc/clojure/") <...>))
           (add-after 'install-doc 'install-javadoc
             (install-javadoc "target/javadoc/"))
           (add-after 'reset-gzip-timestamps 'reset-class-timestamps clj:reset-class-timestamps))))
      (native-inputs libraries)
      (home-page "https://clojure.org/")
      (synopsis "Lisp dialect running on the JVM")
      (description "Clojure is a dynamic, general-purpose programming language,
combining the approachability and interactive development of a scripting
language with an efficient and robust infrastructure for multithreaded
programming.  Clojure is a compiled language, yet remains completely dynamic
– every feature supported by Clojure is supported at runtime.  Clojure
provides easy access to the Java frameworks, with optional type hints and type
inference, to ensure that calls to Java can avoid reflection.

Clojure is a dialect of Lisp, and shares with Lisp the code-as-data philosophy
and a powerful macro system.  Clojure is predominantly a functional programming
language, and features a rich set of immutable, persistent data structures.
When mutable state is needed, Clojure offers a software transactional memory
system and reactive Agent system that ensure clean, correct, multithreaded
designs.")
      ;; Clojure is licensed under EPL1.0
      ;; ASM bytecode manipulation library is licensed under BSD-3
      ;; Guava Murmur3 hash implementation is licensed under APL2.0
      ;; src/clj/repl.clj is licensed under CPL1.0

      ;; See readme.html or readme.txt for details.
      (license (list license:epl1.0
                     license:bsd-3
                     license:asl2.0
                     license:cpl1.0)))))

(define-public clojure-tools
  (package
    (name "clojure-tools")
    (version "1.11.1.1149")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://download.clojure.org/install/clojure-tools-"
                           version
                           ".tar.gz"))
       (sha256 (base32 "196wl87br8zg3npfwpr5c1q0knxd1810vzgb0b6h195hyjf6i210"))
       ;; Remove AOT compiled JAR.  The other JAR only contains uncompiled
       ;; Clojure source code.
       (snippet
        `(delete-file ,(string-append "clojure-tools-" version ".jar")))))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan
       '(("deps.edn" "lib/clojure/")
         ("example-deps.edn" "lib/clojure/")
         ("tools.edn" "lib/clojure/")
         ("exec.jar" "lib/clojure/libexec/")
         ("clojure" "bin/")
         ("clj" "bin/"))
       #:modules ((guix build copy-build-system)
                  (guix build utils)
                  (srfi srfi-1)
                  (ice-9 match))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-paths
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "clojure"
               (("PREFIX") (string-append (assoc-ref outputs "out") "/lib/clojure")))
             (substitute* "clj"
               (("BINDIR") (string-append (assoc-ref outputs "out") "/bin"))
               (("rlwrap") (which "rlwrap")))))
         (add-after 'fix-paths 'copy-tools-deps-alpha-jar
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (substitute* "clojure"
               (("\\$install_dir/libexec/clojure-tools-\\$version\\.jar")
                (string-join
                 (append-map (match-lambda
                               ((label . dir)
                                (find-files dir "\\.jar$")))
                             inputs)
                 ":"))))))))
    (inputs (list rlwrap
                  clojure
                  clojure-tools-deps-alpha
                  java-commons-logging-minimal))
    (home-page "https://clojure.org/releases/tools")
    (synopsis "CLI tools for the Clojure programming language")
    (description "The Clojure command line tools can be used to start a
Clojure repl, use Clojure and Java libraries, and start Clojure programs.")
    (license license:epl1.0)))

(define-public clojure-algo-generic
  (package
    (name "clojure-algo-generic")
    (version "0.1.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/clojure/algo.generic")
             (commit (string-append "algo.generic-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1s6q10qp276dcpzv06bq1q3bvkvlw03qhmncqcs9cc6p9lc0w4p4"))))
    (build-system clojure-build-system)
    (arguments
     '(#:source-dirs '("src/main/clojure/")
       #:test-dirs '("src/test/clojure/")
       #:doc-dirs '()
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-import
           (lambda _
             (substitute*
                 "src/main/clojure/clojure/algo/generic/math_functions.clj"
               (("clojure.algo.generic.math-functions")
                "clojure.algo.generic.math-functions\n(:refer-clojure :exclude [abs])")))))))
    (synopsis "Generic versions of common functions")
    (description
     "Generic versions of commonly used functions, implemented as multimethods
that can be implemented for any data type.")
    (home-page "https://github.com/clojure/algo.generic")
    (license license:epl1.0)))

(define-public clojure-algo-monads
  (package
    (name "clojure-algo-monads")
    (version "0.1.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/clojure/algo.monads")
             (commit (string-append "algo.monads-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0mv3ba72hyhgasg2k3zy83ij61gak6cs4d6qgh8123z3j02mbh8p"))))
    (build-system clojure-build-system)
    (arguments
     '(#:source-dirs '("src/main/clojure/")
       #:test-dirs '("src/test/clojure/")
       #:doc-dirs '()))
    (native-inputs
     (list clojure-tools-macro))
    (synopsis
     "Monad Macros and Definitions")
    (description
     "This library contains the most commonly used monads as well as macros for
defining and using monads and useful monadic functions.")
    (home-page "https://github.com/clojure/algo.monads")
    (license license:epl1.0)))

(define-public clojure-core-match
  (package
    (name "clojure-core-match")
    (version "1.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/clojure/core.match")
                    (commit (string-append "core.match-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0ajpxjv4yc282cm0jw8819fay2j6jqp9nfy69k7vll09q7vqsd22"))))
    (build-system clojure-build-system)
    (arguments
     '(#:source-dirs '("src/main/clojure")
       #:test-dirs '("src/test/clojure")
       #:doc-dirs '()))
    (synopsis "Optimized pattern matching for Clojure")
    (description
     "An optimized pattern matching library for Clojure.
It supports Clojure 1.5.1 and later as well as ClojureScript.")
    (home-page "https://github.com/clojure/core.match")
    (license license:epl1.0)))

(define-public clojure-data-codec
  (package
    (name "clojure-data-codec")
    (version "0.1.1")
    (home-page "https://github.com/clojure/data.codec")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "data.codec-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "192df1dmbwvf1x837mi731n9x94bdypaz18va45plzgdsh4xx6dr"))))
    (build-system clojure-build-system)
    (arguments
     '(#:source-dirs '("src/main/clojure")
       #:test-dirs '("src/test/clojure")
       #:doc-dirs '()))
    (native-inputs (list java-commons-codec
                         clojure-test-check))
    (synopsis "Native codec implementations for Clojure")
    (description "Native codec implementations for Clojure.  Currently only
base64 has been implemented.  Implements the standard base64 encoding
character set, but does not yet support automatic fixed line-length encoding.
All operations work on either byte arrays or Input/OutputStreams.  Performance
is on par with Java implementations, e.g., Apache commons-codec.")
    (license license:epl1.0)))

(define-public clojure-data-xml
  (package
    (name "clojure-data-xml")
    (version "0.2.0-alpha6")
    (home-page "https://github.com/clojure/data.xml")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "data.xml-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "08vglcapq7sd9zhw8dw1y7dcdks7f21w1pw9p05i475i3bw4cf94"))))
    (build-system clojure-build-system)
    (arguments
     '(#:source-dirs '("src/main/clojure")
       #:test-dirs '("src/test/clojure" "src/test/resources")
       #:doc-dirs '()
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'delete-cljs-tests
                     (lambda _
                       (delete-file "src/test/resources/clojure/data/xml/cljs_testsuite.clj")
                       (delete-file "src/test/clojure/clojure/data/xml/test_cljs.clj"))))))
    (propagated-inputs (list clojure-data-codec))
    (synopsis "Clojure library for reading and writing XML data")
    (description "@code{data.xml} is a Clojure library for reading and writing
XML data. @code{data.xml} has the following features:

Parses XML documents into Clojure data structures
Emits XML from Clojure data structures
No additional dependencies if using JDK >= 1.6
Uses StAX internally
lazy - should allow parsing and emitting of large XML documents")
    (license license:epl1.0)))

(define-public clojure-instaparse
  (let ((version "1.4.12"))
    (package
      (name "clojure-instaparse")
      (version version)
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/Engelberg/instaparse")
                      (commit (string-append "v" version))))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1xdiwsv1dc8mvrmvgs4xdqk3z6ddsammc6brhcb771yhimx8jjcr"))))
      (build-system clojure-build-system)
      (arguments
       '(;; Disabled AOT, because of failing test: No implementation of
         ;; method: :conj-flat of protocol:
         ;; #'instaparse.auto-flatten-seq/ConjFlat found for class:
         ;; instaparse.auto_flatten_seq.AutoFlattenSeq
         #:aot-exclude '(#:all)
         #:doc-dirs '("docs/")
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-import
             (lambda _
               (substitute*
                   "test/instaparse/defparser_test.cljc"
                 (("AssertionError")
                  "Exception")))))))
      (synopsis "No grammar left behind")
      (description
       "Instaparse aims to be the simplest way to build parsers in Clojure.

@itemize
@item Turns @emph{standard EBNF or ABNF notation} for context-free grammars
into an executable parser that takes a string as an input and produces a parse
tree for that string.

@item @dfn{No Grammar Left Behind}: Works for @emph{any} context-free grammar,
including @emph{left-recursive}, @emph{right-recursive}, and @emph{ambiguous}
grammars.

@item Extends the power of context-free grammars with PEG-like syntax for
lookahead and negative lookahead.

@item Supports both of Clojure's most popular tree formats (hiccup and enlive)
as output targets

@item Detailed reporting of parse errors.

@item Optionally produces lazy sequence of all parses (especially useful for
diagnosing and debugging ambiguous grammars).

@item ``Total parsing'' mode where leftover string is embedded in the parse
tree.

@item Optional combinator library for building grammars programmatically.

@item Performant.
@end itemize")
      (home-page "https://github.com/Engelberg/instaparse")
      (license license:epl1.0))))

(define-public clojure-test-check
  (package
    (name "clojure-test-check")
    (version "1.1.1")
    (home-page "https://github.com/clojure/test.check")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "09jvlibnxhjv0l57y0sa7yy5in67gq4sssag77hv2d980mwdnls6"))))
    (build-system clojure-build-system)
    (arguments
     '(#:source-dirs '("src/main/clojure")
       #:test-dirs '("src/test/clojure")
       #:doc-dirs '()))
    (synopsis "QuickCheck for Clojure")
    (description "@code{test.check} is a Clojure property-based testing tool
inspired by QuickCheck.  The core idea of @code{test.check} is that instead of
enumerating expected input and output for unit tests, you write properties
about your function that should hold true for all inputs.  This lets you write
concise, powerful tests.")
    (license license:epl1.0)))

(define-public clojure-tools-macro
  (package
    (name "clojure-tools-macro")
    (version "0.1.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/clojure/tools.macro")
             (commit (string-append "tools.macro-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "14mdxqkwja0cffmyfav5pbcli2qvw1mjdgz0n619a2z2036andx8"))))
    (build-system clojure-build-system)
    (arguments
     '(#:source-dirs '("src/main/clojure/")
       #:test-dirs '("src/test/clojure/")
       #:doc-dirs '()))
    (synopsis "Utilities for macro writers")
    (description "Tools for writing macros.")
    (home-page "https://github.com/clojure/tools.macro")
    (license license:epl1.0)))

(define-public clojure-tools-cli
  (package
    (name "clojure-tools-cli")
    (version "1.0.206")
    (home-page "https://github.com/clojure/tools.cli")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "tools.cli-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32 "1hvk5zacl3fr8lfcbfgckaicqjx697j0kzw2x5hwj8j5xlr8ri2r"))))
    (build-system clojure-build-system)
    (arguments
     '(#:source-dirs '("src/main/clojure/")
       #:test-dirs '("src/test/clojure/")
       #:doc-dirs '()))
    (synopsis "Clojure library for working with command-line arguments")
    (description
     "The @code{tools.cli} library provides Clojure programmers with tools to
work with command-line arguments.")
    (license license:epl1.0)))

(define-public clojure-tools-deps-alpha
  (package
    (name "clojure-tools-deps-alpha")
    (version "0.14.1212")
    (home-page "https://github.com/clojure/tools.deps.alpha")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0r8qfihii6cf95kl86x6zfldnm7wlkgda2qmq3340j1x03v244dd"))))
    (build-system clojure-build-system)
    (arguments
     `(#:source-dirs '("src/main/clojure" "src/main/resources")
       #:test-dirs '("src/test/clojure")
       #:doc-dirs '()
       ;; FIXME: Could not initialize class org.eclipse.aether.transport.http.SslSocketFactory
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         ;; FIXME: Currently, the S3 transporter depends on ClojureScript,
         ;; which is very difficult to package due to dependencies on Java
         ;; libraries with non-standard build systems. Instead of actually
         ;; packaging these libraries, we just remove the S3 transporter that
         ;; depends on them.
         (add-after 'unpack 'remove-s3-transporter
           (lambda _
             (for-each delete-file
                       (list
                        (string-append
                         "src/main/clojure/clojure/"
                         "tools/deps/alpha/util/s3_aws_client.clj")
                        (string-append
                         "src/main/clojure/clojure/"
                         "tools/deps/alpha/util/s3_transporter.clj")
                        (string-append
                         "src/test/clojure/clojure/"
                         "tools/deps/alpha/util/test_s3_transporter.clj")))
             (substitute*
                 "src/main/clojure/clojure/tools/deps/alpha/util/maven.clj"
               (("clojure.tools.deps.alpha.util.s3-transporter")
                "")))))))
    (propagated-inputs (list maven-resolver-api
                             maven-resolver-spi
                             maven-resolver-impl
                             maven-resolver-util
                             maven-resolver-connector-basic
                             maven-resolver-provider
                             maven-core
                             maven-resolver-transport-http
                             maven-resolver-transport-file
                             clojure-tools-gitlibs
                             clojure-tools-cli
                             clojure-data-xml))
    (synopsis "Clojure library supporting clojure-tools")
    (description "This package provides a functional API for transitive
dependency graph expansion and the creation of classpaths.")
    (license license:epl1.0)))

(define-public clojure-tools-gitlibs
  (package
    (name "clojure-tools-gitlibs")
    (version "2.4.181")
    (home-page "https://github.com/clojure/tools.gitlibs")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1d8c79f33axghadwqa955yjfsxa6fgl8jq4nfll2zrp6sjw0597k"))))
    (build-system clojure-build-system)
    (arguments
     '(#:source-dirs '("src/main/clojure")
       #:test-dirs '("src/test/clojure")
       #:doc-dirs '()
       ;; Tests attempt to clone git repositories from the internet.
       #:tests? #f))
    (synopsis "Retrieve, cache, and programmatically access git libraries")
    (description "To access git dependencies (for example, via
@code{tools.deps}), one must download git directories and working trees as
indicated by git SHAs.  This library provides this functionality and also
keeps a cache of git directories and working trees that can be reused.")
    (license license:epl1.0)))
