;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Alex Vong <alexvong1995@gmail.com>
;;; Copyright © 2018 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Jesse Gibbons <jgibbons2357+guix@gmail.com>
;;; Copyright © 2020 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2024 Roman Scherer <roman@burningswell.com>
;;; Copyright © 2025, 2026 Mathieu Lirzin <mthl@gnu.org>
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

(define-public clojure-spec-alpha
  (package
    (name "clojure-spec-alpha")
    (version "0.6.249")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/clojure/spec.alpha")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1rzy7mjzchsjpwkn3b5vg5hnmsj5h9ljxdjn48j92bgh7vl3k2r5"))))
    (build-system clojure-build-system)
    (arguments '(#:source-dirs '("src/main/clojure")
                 #:test-dirs '("src/test/clojure")
                 #:doc-dirs '()))
    (native-inputs (list clojure-test-check))
    (synopsis
     "Clojure library to describe the structure of data and functions")
    (description
     "This package can be used to validate data, conform (destructure) data, explain
invalid data, generate examples that conform to the specs, and automatically
use generative testing to test functions.

Clojure depends on this library and provides it.  Thus it is not recommended
to add a direct dependency on this package.")
    (home-page "https://github.com/clojure/spec.alpha")
    (license license:epl1.0)))

(define-public clojure-core-specs-alpha
  (package
    (name "clojure-core-specs-alpha")
    (version "0.5.81")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/clojure/core.specs.alpha")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1hy060mb2hxirynkk36plx1rvpj6i2ldn8mvnwfd02wvjvh57dad"))))
    (build-system clojure-build-system)
    (arguments '(#:source-dirs '("src/main/clojure")
                 #:test-dirs '()
                 #:doc-dirs '()))
    (synopsis "Describe clojure.core macros and functions")
    (description
     "This package contains specs to describe Clojure core macros and functions.

Clojure depends on this library and provides it.  Thus it is not recommended
to add a direct dependency on this package.")
    (home-page "https://github.com/clojure/core.specs.alpha")
    (license license:epl1.0)))

(define-public clojure-java-classpath
  (package
    (name "clojure-java-classpath")
    (version "1.1.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/clojure/java.classpath")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1k34yrx3gmvf46984zdgn2vb98ixf47q1vgh9p78bgmpyhrwzx2z"))))
    (build-system clojure-build-system)
    (arguments '(#:source-dirs '("src/main/clojure")
                 #:test-dirs '("src/test/clojure")
                 #:doc-dirs '()))
    (synopsis "Examine the Java classpath from Clojure programs")
    (description
     "This package provides utilities for dealing with the JVM's classpath from
Clojure.")
    (home-page "https://github.com/clojure/java.classpath")
    (license license:epl1.0)))

(define-public clojure-data-generators
  (package
    (name "clojure-data-generators")
    (version "1.1.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/clojure/data.generators")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1ynazp55v15zv5jwz1wh2p8cawjcmn0bwzgfs2dwrp6aq231jafm"))))
    (build-system clojure-build-system)
    (arguments '(#:source-dirs '("src/main/clojure")
                 #:test-dirs '("src/test/clojure")
                 #:doc-dirs '()))
    (synopsis "Generators for random Clojure data")
    (description
     "This package contains various functions to generate random clojure data.")
    (home-page "https://github.com/clojure/data.generators")
    (license license:epl1.0)))

(define-public clojure-tools-namespace
  (package
    (name "clojure-tools-namespace")
    (version "1.5.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/clojure/tools.namespace")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0j1lz3xnpgvyi0mcg9mjz7hrchf3hwhbhxi2163d739dxdx60s22"))))
    (build-system clojure-build-system)
    (arguments '(#:source-dirs '("src/main/clojure")
                 #:test-dirs '("src/test/clojure")
                 #:doc-dirs '()))
    (propagated-inputs (list clojure-java-classpath
                             clojure-tools-reader))
    (synopsis "Tools for managing namespaces in Clojure")
    (description
     "This package parses ns declarations from source files, extract their
dependencies, build a graph of namespace dependencies within a project, update
that graph as files change, and reload files in the correct order.

This is only about namespace dependencies within a single project.  It has
nothing to do with Leiningen, Maven, JAR files, or repositories.")
    (home-page "https://github.com/clojure/tools.namespace")
    (license license:epl1.0)))

(define-public clojure-test-generative
  (package
    (name "clojure-test-generative")
    (version "1.1.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/clojure/test.generative")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0lb72cr4s6rgpg18v3jr67ps6wx4p7j7mfzwnfgmm0v8rldlcycf"))))
    (build-system clojure-build-system)
    (arguments '(#:source-dirs '("src/main/clojure")
                 #:test-dirs '("src/examples/clojure")
                 #:doc-dirs '()))
    (propagated-inputs (list clojure-tools-namespace
                             clojure-data-generators))
    (synopsis "Generative test runner")
    (description
     "This package defines generators functions and property based testing macros.")
    (home-page "https://github.com/clojure/test.generative")
    (license license:epl1.0)))

(define-public clojure-test-check
  (package
    (name "clojure-test-check")
    (version "1.1.3")
    (home-page "https://github.com/clojure/test.check")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1j1x80rv0pli3g91wy01panlqbhgf7zqm7jdr2rzjqf2yyksc70n"))))
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

(define-public clojure-tools-reader
  (package
    (name "clojure-tools-reader")
    (version "1.6.0")
    (home-page "https://github.com/clojure/tools.reader")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "006zy7dpk678rzawqyqcypccw48bl4b9s4xjrwzhgq8z5s8v6lnd"))))
    (build-system clojure-build-system)
    (arguments
     '(#:doc-dirs '()
       #:source-dirs '("src/main/clojure")
       #:test-dirs '("src/test/clojure")
       #:test-exclude '(clojure.tools.common-tests))) ; Loaded by other tests.
    (synopsis "Clojure reader written in Clojure")
    (description "The clojure.tools.reader library offers all functionality
provided by the Clojure Core reader and more.  It adds metadata such as column
and line numbers not only to lists, but also to symbols, vectors and maps.")
    (license license:epl1.0)))

(define (package-sources . packages)
  (map package-source packages))

(define-public clojure
  (let ((provided-libraries (package-sources clojure-core-specs-alpha
                                             clojure-spec-alpha))
        (test-libraries (package-sources clojure-data-generators
                                         clojure-java-classpath
                                         clojure-test-check
                                         clojure-test-generative
                                         clojure-tools-namespace
                                         clojure-tools-reader)))
    (package
      (name "clojure")
      (version "1.12.4")
      (source (let ((name+version (string-append name "-" version)))
                (origin
                  (method git-fetch)
                  (uri (git-reference
                         (url "https://github.com/clojure/clojure")
                         (commit name+version)))
                  (file-name (string-append name+version "-checkout"))
                  (sha256
                   (base32
                    "072dv6s2gxcg8snlgkpjk6bp1cb17bgfshdq6ijsa4yslpqbf9wc")))))
      (build-system ant-build-system)
      (arguments
       `(#:imported-modules ((guix build clojure-utils)
                             (guix build clojure-build-system)
                             ,@%ant-build-system-modules)
         #:modules ((guix build ant-build-system)
                    ((guix build clojure-build-system) #:prefix clj:)
                    (guix build clojure-utils)
                    (guix build java-utils)
                    (guix build utils)
                    (srfi srfi-26))
         #:test-target "test"
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'unpack-library-sources
             (lambda* (#:key inputs #:allow-other-keys)
               (define (extract-provided-library name)
                 (copy-recursively
                  (string-append (assoc-ref inputs name) "/src/main/clojure")
                  "src/clj/"))
               (for-each extract-provided-library
                         ',(map origin-actual-file-name provided-libraries))))
           (add-after 'unpack-library-sources 'fix-manifest-classpath
             (lambda _
               (substitute* "build.xml"
                 (("<attribute name=\"Class-Path\" value=\".\"/>") ""))))
           (add-after 'unpack-library-sources 'clojure-spec-skip-macros
             ;; Disable spec macro instrumentation when compiling clojure.spec
             ;; See: https://clojure.atlassian.net/browse/CLJ-2254
             (lambda _
               (substitute* "build.xml"
                 (("<sysproperty key=\"java.awt.headless\" value=\"true\"/>")
                  ,(string-join
                    '("<sysproperty key=\"java.awt.headless\" value=\"true\"/>"
                      "<sysproperty key=\"clojure.spec.skip-macros\" value=\"true\"/>\n")
                    "\n")))))
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
                      "<arg value=\"clojure.core.specs.alpha\"/>"))))))
           (add-before 'build 'maven-classpath-properties
             (lambda* (#:key inputs #:allow-other-keys)
               (define (source-dir library)
                 (string-append (assoc-ref inputs library) "/src/main/clojure"))
               (let* ((libraries ',(map origin-actual-file-name test-libraries))
                      (test-classpath (map source-dir libraries)))
                 (with-output-to-file "maven-classpath.properties"
                   (lambda _
                     (display "maven.compile.classpath=\n")
                     (display (string-append "maven.test.classpath="
                                             (string-join test-classpath ":")
                                             "\n")))))))
           (add-before 'check 'fix-test-classpath
             ;; Some java test files need access to compiled classes.
             (lambda _
               (substitute* "build.xml"
                 (("javac srcdir=\"\\$\\{jtestsrc\\}\"" cmd)
                  (string-append cmd " classpath=\"${build}\"")))))
           (add-after 'build 'build-javadoc ant-build-javadoc)
           (replace 'install (install-jars "./"))
           (add-after 'install-license-files 'install-doc
             (cut install-doc #:doc-dirs '("doc/clojure/") <...>))
           (add-after 'install-doc 'install-javadoc
             (install-javadoc "target/javadoc/"))
           (add-after 'reset-gzip-timestamps 'reset-class-timestamps
             clj:reset-class-timestamps))))
      (native-inputs (append provided-libraries test-libraries))
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
    (version "1.12.4.1582")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://download.clojure.org/install/clojure-tools-"
                           version
                           ".tar.gz"))
       (sha256 (base32 "08gzfblnz0zhnk6pwr9vcm6y168psgrwmqww3wqk1v7j5gr68n7x"))
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
                  clojure-tools-deps
                  java-commons-logging-minimal
                  java-slf4j-nop))
    (home-page "https://clojure.org/releases/tools")
    (synopsis "CLI tools for the Clojure programming language")
    (description "The Clojure command line tools can be used to start a
Clojure repl, use Clojure and Java libraries, and start Clojure programs.")
    (license license:epl1.0)))

(define-public clojure-algo-generic
  (package
    (name "clojure-algo-generic")
    (version "1.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/clojure/algo.generic")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0i10rxk5jxsw1cm2a8rsq9zgl04173cbpaj02vvc9nb4ig219a8y"))))
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
    (version "0.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/clojure/algo.monads")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1l420fzqr1mw45w1lz6iqx0s61m8w3r3ad8zmsg17cqahnfakh00"))))
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

(define-public clojure-core-async
  (package
    (name "clojure-core-async")
    (version "1.8.741")
    (home-page "https://github.com/clojure/core.async")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1m6lir2xr119yxz483cz0rfw4k3a40qmmsiwl2y7kyxy65vab60z"))))
    (build-system clojure-build-system)
    (arguments
     '(#:source-dirs '("src/main/clojure")
       #:test-dirs '("src/test/clojure")
       #:doc-dirs '()
       #:phases
       (modify-phases %standard-phases
         ;; Remove ClojureScript code, we are only supporting Clojure for now.
         (add-after 'unpack 'delete-cljs
           (lambda _
             (delete-file-recursively "src/main/clojure/cljs")
             (delete-file-recursively "src/test/cljs"))))))
    (propagated-inputs (list clojure-tools-analyzer-jvm))
    (synopsis "Facilities for async programming and communication in Clojure")
    (description "The core.async library adds support for asynchronous
programming using channels to Clojure.  It provides facilities for independent
threads of activity, communicating via queue-like channels inspired by Hoare’s
work on Communicating Sequential Processes (CSP).")
    (license license:epl1.0)))

(define-public clojure-core-cache
  (package
    (name "clojure-core-cache")
    (version "1.2.249")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/clojure/core.cache")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0hdwv5dsx49i12riz3frzshwkgw26ah43667lzga2n6zgpmrh8ii"))))
    (build-system clojure-build-system)
    (arguments
     '(#:source-dirs '("src/main/clojure")
       #:test-dirs '("src/test/clojure")
       #:doc-dirs '("docs")))
    (propagated-inputs (list clojure-data-priority-map))
    (synopsis "Clojure caching library")
    (description
     "Caching library for Clojure implementing various cache strategies such
as First-in-first-out, Least-recently-used, Least-used, Time-to-live, Naive
cache and Naive cache backed with soft references.")
    (home-page "https://github.com/clojure/core.cache")
    (license license:epl1.0)))

(define-public clojure-core-match
  (package
    (name "clojure-core-match")
    (version "1.1.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/clojure/core.match")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "09kijs81js7iy810qkzn7fi996gmf7bmlm2ax571zy0yr38i2hbr"))))
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

(define-public clojure-core-memoize
  (package
    (name "clojure-core-memoize")
    (version "1.2.273")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/clojure/core.memoize")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "009fj19y464an66pryygmqbnx2dqk5wkp40rbkbgkir0ax0k6j3y"))))
    (build-system clojure-build-system)
    (arguments
     '(#:source-dirs '("src/main/clojure")
       #:test-dirs '("src/test/clojure")
       #:doc-dirs '("docs")))
    (propagated-inputs (list clojure-core-cache))
    (synopsis "Memoization framework for Clojure")
    (description
     "A manipulable, pluggable, memoization framework for Clojure implementing
some common memoization caching strategies, such as First-in-first-out,
Least-recently-used, Least-used and Time-to-live.")
    (home-page "https://github.com/clojure/core.memoize")
    (license license:epl1.0)))

(define-public clojure-data-codec
  (package
    (name "clojure-data-codec")
    (version "0.2.1")
    (home-page "https://github.com/clojure/data.codec")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1wxqwkbpaxjchh03lwy3ngyx6dwv7x7l1qhac42dl6bg213izil5"))))
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

(define-public clojure-data-csv
  (package
    (name "clojure-data-csv")
    (version "1.1.1")
    (home-page "https://github.com/clojure/data.csv")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1i43qzjn29a0xb3q54sfrn8f77nmxd0v0fn9361as07yclh8jzif"))))
    (build-system clojure-build-system)
    (arguments
     '(#:source-dirs '("src/main/clojure")
       #:test-dirs '("src/test/clojure")
       #:doc-dirs '()))
    (synopsis "Clojure library for reading and writing CSV data")
    (description "@code{data.csv} is a Clojure library for reading and writing
CSV data.  @code{data.csv} follows the RFC4180 specification but is more
relaxed.")
    (license license:epl1.0)))

(define-public clojure-data-json
  (package
    (name "clojure-data-json")
    (version "2.5.2")
    (home-page "https://github.com/clojure/data.json")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0xkcd6h5g7yhr839wci6dsynmbijj9r5mrrj70gx849mkhj98411"))))
    (build-system clojure-build-system)
    (arguments
     '(#:source-dirs '("src/main/clojure")
       #:test-dirs '("src/test/clojure")
       #:doc-dirs '()))
    (native-inputs (list clojure-test-check))
    (synopsis "Clojure library for reading and writing JSON data")
    (description "@code{data.json} is a Clojure library for reading and
writing JSON data.  @code{data.xml} is compliant with the JSON spec and has no
external dependencies")
    (license license:epl1.0)))

(define-public clojure-data-priority-map
  (package
    (name "clojure-data-priority-map")
    (version "1.2.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/clojure/data.priority-map")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0yswqr6855n0rg0mfmjfdx0npzasm654m7sz4x0wl13grwgg220k"))))
    (build-system clojure-build-system)
    (arguments
     '(#:source-dirs '("src/main/clojure")
       #:test-dirs '("src/test/clojure")
       #:doc-dirs '()))
    (synopsis "Priority map implementation in Clojure")
    (description
     "A priority map is very similar to a sorted map, but whereas a sorted map
produces a sequence of the entries sorted by key, a priority map produces the
entries sorted by value.  In addition to supporting all the functions a sorted
map supports, a priority map can also be thought of as a queue of [item
priority] pairs.  To support usage as a versatile priority queue, priority
maps also support conj/peek/pop operations.")
    (home-page "https://github.com/clojure/data.priority-map")
    (license license:epl1.0)))

(define-public clojure-data-xml
  (package
    (name "clojure-data-xml")
    (version "0.2.0-alpha10")
    (home-page "https://github.com/clojure/data.xml")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0mnhgrh20p2wwmq8a4631dkpxbgbijwi6sfhw5v784hzkjkfgkh6"))))
    (build-system clojure-build-system)
    (arguments
     '(#:source-dirs '("src/main/clojure")
       #:test-dirs '("src/test/clojure" "src/test/resources")
       #:doc-dirs '()
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'delete-cljs-tests
           (lambda _
             (delete-file "src/test/resources/clojure/data/xml/cljs_repl_nashorn.clj")
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

(define-public clojure-data-zip
  (package
    (name "clojure-data-zip")
    (version "1.1.1")
    (home-page "https://github.com/clojure/data.zip")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0l13xs0jlv2cyh3rqdp1f5r0jsff9p1h7dh49112xxrkcvzck876"))))
    (build-system clojure-build-system)
    (arguments
     '(#:source-dirs '("src/main/clojure")
       #:test-dirs '("src/test/clojure")
       #:doc-dirs '()))
    (synopsis
     "Clojure library for filtering trees, and XML trees in particular")
    (description
     "@code{data.zip} is a Clojure library for filtering trees using the zipper
abstraction.")
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

(define-public clojure-tools-analyzer
  (package
    (name "clojure-tools-analyzer")
    (version "1.2.1")
    (home-page "https://github.com/clojure/tools.analyzer")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0apa3gc45rmdj19plbvglpj6i9dadkgvvidj5mcnkzi9y3h0nr27"))))
    (build-system clojure-build-system)
    (arguments
     '(#:source-dirs '("src/main/clojure")
       #:test-dirs '("src/test/clojure")
       #:doc-dirs '()))
    (synopsis "Analyzer for Clojure code")
    (description "Analyzer for Clojure code, written in Clojure, which
produces an abstract syntax tree in the EDN ( Extensible Data Notation)
format.")
    (license license:epl1.0)))

(define-public clojure-tools-analyzer-jvm
  (package
    (name "clojure-tools-analyzer-jvm")
    (version "1.3.4")
    (home-page "https://github.com/clojure/tools.analyzer.jvm")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0pk9z8zf9bgzj0wi98yx130l7570arnxfsg9508g1jvq83djpn21"))))
    (build-system clojure-build-system)
    (arguments
     '(#:source-dirs '("src/main/clojure")
       #:test-dirs '("src/test/clojure")
       #:doc-dirs '("docs")))
    (propagated-inputs (list clojure-tools-analyzer
                             clojure-tools-reader
                             clojure-core-memoize
                             java-asm))
    (synopsis "Analyzer for Clojure code targeting the JVM")
    (description "Analyzer for Clojure code, written on top of
tools.analyzer, providing additional JVM-specific passes.")
    (license license:epl1.0)))

(define-public clojure-tools-macro
  (package
    (name "clojure-tools-macro")
    (version "0.2.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/clojure/tools.macro")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "09xxp5vpf86akmn95mvjmd34a1pc48cjxbp6q4lvi3q8rhpkjfd5"))))
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
    (version "1.3.250")
    (home-page "https://github.com/clojure/tools.cli")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32 "1nyw33w83gd7xlbpma03p4jwcdcp38h8y99x2rqw4zcnw19gsz4p"))))
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

(define-public clojure-tools-deps
  (package
    (name "clojure-tools-deps")
    (version "0.28.1578")
    (home-page "https://github.com/clojure/tools.deps")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1w3f8cl81q4kln6iz74fnfzms4c4x279ivh1962pcg22q8l1w8d5"))))
    (build-system clojure-build-system)
    (arguments
     `(#:source-dirs '("src/main/clojure" "src/main/resources")
       #:java-source-dirs '("src/main/java")
       #:test-dirs '("src/test/clojure")
       #:doc-dirs '()
       ;; FIXME: Could not initialize class
       ;; org.eclipse.aether.transport.http.SslSocketFactory
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'reference-clojure-jar-input
           ;; Use static clojure jar from build input at runtime by default.
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "src/main/resources/clojure/tools/deps/deps.edn"
               (("org\\.clojure/clojure.*$")
                (string-append "org.clojure/clojure {:local/root \""
                               (assoc-ref inputs "clojure")
                               "/share/java/clojure.jar\"}"))))))))
    (propagated-inputs (list maven-3.8-core
                             maven-resolver-1.6-connector-basic
                             maven-resolver-1.6-transport-http
                             maven-resolver-1.6-transport-file
                             clojure-tools-gitlibs
                             clojure-tools-cli
                             clojure-data-xml
                             cognitect-aws-api))
    (synopsis "Clojure library supporting clojure-tools")
    (description "This package provides a functional API for transitive
dependency graph expansion and the creation of classpaths.")
    (license license:epl1.0)))

(define-public clojure-tools-deps-alpha
  ;; this was superseded by clojure-tools-deps
  ;; https://github.com/clojure/tools.deps.alpha
  ;; Keeping it to give downstream packages a chance to upgrade
  (package
    (name "clojure-tools-deps-alpha")
    (version "0.15.1254")
    (home-page "https://github.com/clojure/tools.deps.alpha")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "17zvizzw637az7facwipsrg9b1kjci03aycg6w5pnjpg6b6nd5m6"))))
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
    (version "2.6.212")
    (home-page "https://github.com/clojure/tools.gitlibs")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "199n58dwh44rlb0m514swh6zx3flckq3lccxv9dwbypbv29n9ghq"))))
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

(define-public clojure-tools-logging
  (package
    (name "clojure-tools-logging")
    (version "1.3.1")
    (home-page "https://github.com/clojure/tools.logging")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1jdk7b6zm73m7nki76123ik6px9rl5g5wb06298v34h56qyck8lr"))))
    (build-system clojure-build-system)
    (arguments
     '(#:doc-dirs '()
       #:source-dirs '("src/main/clojure")
       #:test-dirs '("src/test/clojure")
       #:phases
       (modify-phases %standard-phases
         ;; These tests should throw a ClassCastException, but they don't
         ;; under AOT. Adjust them :/
         (add-after 'unpack 'disable-failing-tests
           (lambda _
             (substitute* (string-append "src/test/clojure/clojure/tools"
                                         "/logging/test_readable.clj")
               (((string-append "\\(thrown\\? ClassCastException \\(logf "
                                ":debug \\(Exception\\.\\)\\)\\)"))
                "(nil? (logf :debug (Exception.)))"))
             (substitute* "src/test/clojure/clojure/tools/test_logging.clj"
               (((string-append "\\(thrown\\? ClassCastException \\(logf "
                                ":debug \\(Exception\\.\\)\\)\\)"))
                "(nil? (logf :debug (Exception.)))")))))))
    (native-inputs
     (list java-commons-logging-minimal
           java-log4j-1.2-api
           java-log4j-api
           java-log4j-core
           java-slf4j-api
           java-slf4j-simple))
    (synopsis "Clojure logging library")
    (description "Logging macros which delegate to a specific logging
implementation, selected at runtime when the clojure.tools.logging namespace
is first loaded.")
    (license license:epl1.0)))

(define-public http-kit
  (package
    (name "http-kit")
    (version "2.8.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/http-kit/http-kit")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1fqfl7a7ygfw43xjmmlwrl55xbz34qxpzc8230bpqvjh9n417h74"))))
    (build-system clojure-build-system)
    (arguments
     '(#:java-source-dirs '("src/java")
       #:source-dirs '("src")
       #:doc-dirs '()
       #:tests? #f))                    ;XXX: too many unpackaged dependencies
    (synopsis
     "High-performance, event-driven HTTP client and server for Clojure")
    (description "This package provides a minimalist, event-driven,
high-performance Clojure HTTP client and server library with WebSocket and
asynchronous support.")
    (home-page "https://github.com/http-kit/http-kit")
    (license license:asl2.0)))

(define-public cognitect-aws-api
  (package
    (name "cognitect-aws-api")
    (version "0.8.774")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/cognitect-labs/aws-api")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1y3impishicd8niy1vddx6c80pnbpz24zfr7vysi4r56p1vqnwmb"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (for-each delete-file
                            (list
                             ;; Require Cognitect http client
                             "src/cognitect/aws/http/cognitect.clj"
                             ;; Requires Babashka.
                             "test/src/bb_test_runner.clj"
                             ;; Requires  AWS Java SDK v2.
                             "test/src/cognitect/aws/jdk_v2.clj"
                             "test/src/cognitect/aws/signers_test.clj"
                             ;; Remove integration tests.
                             "test/src/cognitect/aws/client/shared_test.clj"
                             "test/src/cognitect/aws/api_test.clj"
                             "test/src/cognitect/client/impl_test.clj"
                             "test/src/cognitect/client/test_double_test.clj"))
                  ;; Remove integration tests
                  (delete-file-recursively "test/src/cognitect/aws/integration")))))
    (build-system clojure-build-system)
    (propagated-inputs (list clojure-core-async
                             clojure-tools-logging
                             clojure-data-json
                             clojure-data-xml))
    (native-inputs (list clojure-test-check
                         http-kit))
    (arguments `(#:source-dirs '("src")
                 #:doc-dirs '()
                 #:test-dirs '("test/src" "test/resources")
                 ;; Allow using java.net.http client.
                 #:jdk ,openjdk11))
    (synopsis
     "Programmatic access to AWS services from Clojure programs")
    (description
     "This package is an idiomatic, data-oriented Clojure library for invoking AWS
APIs.  While the library offers some helper and documentation functions you'll
use at development time, the only functions you ever need at runtime are
client, which creates a client for a given service and invoke, which invokes
an operation on the service. invoke takes a map and returns a map, and works
the same way for every operation on every service.")
    (home-page "https://github.com/cognitect-labs/aws-api")
    (license license:asl2.0)))
