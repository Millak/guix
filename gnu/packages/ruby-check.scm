;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015-2018 Ben Woodcroft <donttrustben@gmail.com>
;;; Copyright © 2015 David Thompson <davet@gnu.org>
;;; Copyright © 2015 Pjotr Prins <pjotr.guix@thebird.nl>
;;; Copyright © 2015, 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016 Muriithi Frederick Muriuki <fredmanglis@gmail.com>
;;; Copyright © 2017-2020, 2023 Christopher Baines <mail@cbaines.net>
;;; Copyright © 2017, 2021 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017 nikita <nikita@n0.is>
;;; Copyright © 2018 Danny Milosavljevic <dannym@scratchpost.org>
;;; Copyright © 2018 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2019 Björn Höfling <bjoern.hoefling@bjoernhoefling.de>
;;; Copyright © 2019 Brian Leung <bkleung89@gmail.com>
;;; Copyright © 2019 Collin J. Doering <collin@rekahsoft.ca>
;;; Copyright © 2019 Jelle Licht <jlicht@fsfe.org>
;;; Copyright © 2019 Mikhail Kirillov <w96k.ru@gmail.com>
;;; Copyright © 2020 Holger Peters <holger.peters@posteo.de>
;;; Copyright © 2020, 2021, 2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2020, 2023 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2020 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2021 EuAndreh <eu@euandre.org>
;;; Copyright © 2021 Giacomo Leidi <goodoldpaul@autistici.org>
;;; Copyright © 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2021 Mathieu Othacehe <othacehe@gnu.org>
;;; Copyright © 2022, 2024 Danny Milosavljevic <dannym@scratchpost.org>
;;; Copyright © 2022 Maxime Devos <maximedevos@telenet.be>
;;; Copyright © 2022 Philip McGrath <philip@philipmcgrath.com>
;;; Copyright © 2022 Stephen Paul Weber <singpolyma@singpolyma.net>
;;; Copyright © 2022 Taiju HIGASHI <higashi@taiju.info>
;;; Copyright © 2022 Tom Fitzhenry <tom@tom-fitzhenry.me.uk>
;;; Copyright © 2023, 2025 gemmaro <gemmaro.dev@gmail.com>
;;; Copyright © 2023 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2023 Yovan Naumovski <yovan@gorski.stream>
;;; Copyright © 2025 Nicolas Graves <ngraves@ngraves.fr>
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

(define-module (gnu packages ruby-check)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system ruby)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages ruby))

;;; Commentary:
;;;
;;; Please: Try to add new module packages in alphabetic order.
;;;
;;; Code:


;; Bundler is yet another source of circular dependencies, so we must disable
;; its test suite as well.
(define-public bundler
  (package
    (name "bundler")
    (version "2.6.9")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "bundler" version))
              (sha256
               (base32
                "1sy9alf2pqjpkjwmkfwax242bxjc1c91xk36cwcf2nh5ppzpamm2"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ; avoid dependency cycles
    (synopsis "Ruby gem bundler")
    (description "Bundler automatically downloads and installs a list of gems
specified in a \"Gemfile\", as well as their dependencies.")
    (home-page "https://bundler.io/")
    (license license:expat)))

(define-public ruby-asciidoctor/minimal
  (hidden-package
   (package
     (name "ruby-asciidoctor")
     (version "2.0.20")
     (source
      (origin
        (method git-fetch)               ;the gem release lacks a Rakefile
        (uri (git-reference
              (url "https://github.com/asciidoctor/asciidoctor")
              (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "19qvilhwa9plg80ppspn5ys0ybl8qfyaicqbl9w316hk5ldwi1jq"))))
     (build-system ruby-build-system)
     (arguments (list #:tests? #f))
     (synopsis "Converter from AsciiDoc content to other formats")
     (description "Asciidoctor is a text processor and publishing toolchain for
converting AsciiDoc content to HTML5, DocBook 5, PDF, and other formats.")
     (home-page "https://asciidoctor.org")
     (license license:expat))))

(define-public ruby-builder
  (package
    (name "ruby-builder")
    (version "3.3.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "builder" version))
              (sha256
               (base32
                "0pw3r2lyagsxkm71bf44v5b74f7l9r7di22brbyji9fwz791hya9"))))
    (build-system ruby-build-system)
    (synopsis "Ruby library to create structured data")
    (description "Builder provides a number of builder objects that make it
easy to create structured data.  Currently the following builder objects are
supported: XML Markup and XML Events.")
    (home-page "https://github.com/rails/builder")
    (license license:expat)))

(define-public ruby-cucumber-ci-environment
  (package
    (name "ruby-cucumber-ci-environment")
    (version "9.1.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "cucumber-ci-environment" version))
              (sha256
               (base32
                "1nmn2hfrjlbazgcryr3hwvsa5v4csfbjqxb4q7wbjhaxl9xxn0k7"))))
    (build-system ruby-build-system)
    (arguments (list #:phases #~(modify-phases %standard-phases
                                  (replace 'check
                                    (lambda* (#:key tests? #:allow-other-keys)
                                      (when tests?
                                        (invoke "rspec")))))))
    (native-inputs (list ruby-rspec))
    (synopsis "Detect CI Environment from environment variables")
    (description "This is a Ruby utility library for Cucumber that detects a
CI environment from environment variables.")
    (home-page "https://github.com/cucumber/ci-environment")
    (license license:expat)))

(define-public ruby-cucumber-compatibility-kit
  (package
    (name "ruby-cucumber-compatibility-kit")
    (version "11.2.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "cucumber-compatibility-kit" version))
              (sha256
               (base32
                "17c8zx0yn68rcpfbw4nb1gzvh9fzpwsi1y0qivb99ahdlgzcdp8q"))))
    (build-system ruby-build-system)
    (arguments (list #:phases #~(modify-phases %standard-phases
                                  (replace 'check
                                    (lambda* (#:key tests? #:allow-other-keys)
                                      (when tests?
                                        (invoke "rspec")))))))
    (propagated-inputs (list ruby-cucumber-messages ruby-rake ruby-rspec))
    (synopsis "Cucumber compatibility verification utility")
    (description "The Cucumber Compatibility Kit (CCK) aims to validate a
Cucumber implementation's support for the Cucumber Messages protocol.")
    (home-page "https://github.com/cucumber/compatibility-kit")
    (license license:expat)))

;;; Variant package to break a cycle with ruby-cucumber-messages.
(define ruby-cucumber-compatibility-kit-bootstrap
  (package/inherit ruby-cucumber-compatibility-kit
    (arguments (list #:tests? #f))
    (propagated-inputs (modify-inputs (package-propagated-inputs
                                       ruby-cucumber-compatibility-kit)
                         (delete "ruby-cucumber-messages")))))

(define-public ruby-cucumber-core
  (package
    (name "ruby-cucumber-core")
    (version "11.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cucumber/cucumber-ruby-core")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0lf2inlam0951djc2qz81x0nkffmw2dpj44iadw1fw31m7r8wqvh"))))
    (build-system ruby-build-system)
    (arguments (list #:test-target "spec"
                     #:phases
                     #~(modify-phases %standard-phases
                         (add-after 'extract-gemspec 'relax-version-requirements
                           (lambda _
                             (substitute* "cucumber-core.gemspec"
                               (("'cucumber-tag-expressions',.*")
                                "'cucumber-tag-expressions', '>=4.1.0'\n")))))))
    (native-inputs
     (list ruby-rspec
           ruby-rubocop/minimal
           ruby-simplecov
           ruby-unindent))
    (propagated-inputs
     (list ruby-cucumber-gherkin
           ruby-cucumber-messages
           ruby-cucumber-tag-expressions))
    (synopsis "Core library for the Cucumber BDD app")
    (description "Cucumber is a tool for running automated tests
written in plain language.  Because they're written in plain language,
they can be read by anyone on your team.  Because they can be read by
anyone, you can use them to help improve communication, collaboration
and trust on your team.")
    (home-page "https://cucumber.io/")
    (license license:expat)))

(define-public ruby-cucumber-expressions
  (package
    (name "ruby-cucumber-expressions")
    (version "16.1.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cucumber/cucumber-expressions")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1dhq88k9x2x8svam5bc7rrcd166fqymda8wxryqkbkffhnzla0id"))))
    (build-system ruby-build-system)
    (arguments
     (list #:test-target "spec"
           #:phases #~(modify-phases %standard-phases
                        (add-after 'unpack 'chdir
                          (lambda _
                            (chdir "ruby"))))))
    (native-inputs (list ruby-rspec ruby-simplecov))
    (synopsis "Simpler alternative to Regular Expressions")
    (description "Cucumber Expressions offer similar functionality to Regular
Expressions, with a syntax that is easier to read and write.  Cucumber
Expressions are extensible with parameter types.")
    (home-page "https://github.com/cucumber/cucumber-expressions/")
    (license license:expat)))

(define-public ruby-cucumber-gherkin
  (package
    (name "ruby-cucumber-gherkin")
    (version "26.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/cucumber/gherkin")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1rsannfcg5rqh5a3d3paw10kf6mmqjrgbq3k235px4swbyqysmgn"))))
    (build-system ruby-build-system)
    (arguments (list #:test-target "spec"
                     #:phases #~(modify-phases %standard-phases
                                  (add-after 'unpack 'chdir
                                    (lambda _
                                      (chdir "ruby"))))))
    (native-inputs (list ruby-rspec))
    (propagated-inputs (list ruby-cucumber-messages))
    (synopsis "Gherkin parser for Ruby")
    (description "Gherkin is a parser and compiler for the Gherkin language.
It is intended be used by all Cucumber implementations to parse
@file{.feature} files.")
    (home-page "https://github.com/cucumber/gherkin")
    (license license:expat)))

(define-public ruby-cucumber-html-formatter
  (package
    (name "ruby-cucumber-html-formatter")
    (version "20.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "cucumber-html-formatter" version))
       (sha256
        (base32
         "0c7r9mfmph4c6yzc7y3dkr92rhwvpyksr0mdhpqp67xmmr8z1br4"))))
    (build-system ruby-build-system)
    (arguments
     (list #:phases #~(modify-phases %standard-phases
                        (add-after 'extract-gemspec 'relax-requirements
                          (lambda _
                            (substitute* ".gemspec"
                              (("~> 18.0") "~> 21.0")))) ;cucumber-messages
                        (replace 'check
                          (lambda* (#:key tests? #:allow-other-keys)
                            (when tests?
                              (invoke "rspec")))))))
    (native-inputs (list ruby-cucumber-compatibility-kit ruby-rspec))
    (propagated-inputs (list ruby-cucumber-messages))
    (synopsis "HTML formatter for Cucumber")
    (description "Cucumber HTML Formatter produces a HTML report for Cucumber
runs.  It is built on top of cucumber-react and works with any Cucumber
implementation with a protocol buffer formatter that outputs Cucumber
messages.")
    (home-page "https://github.com/cucumber/html-formatter")
    (license license:expat)))

(define-public ruby-cucumber-messages
  (package
    (name "ruby-cucumber-messages")
    (version "21.0.1")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "cucumber-messages" version))
              (sha256
               (base32
                "0482a63y7my0arn2bv208g401dq8525f0gwhnwaa11mhv6ph0q5i"))))
    (build-system ruby-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               ;; The test suite requires the gem to be installed, so move it
               ;; after the install phase.
               (delete 'check)
               (add-after 'install 'check
                 (lambda* (#:key tests? #:allow-other-keys)
                   (setenv "GEM_PATH" (string-append
                                       (getenv "GEM_PATH") ":"
                                       #$output "/lib/ruby/vendor_ruby"))
                   (when tests?
                     (invoke "rspec")))))))
    (native-inputs
     (list ruby-cucumber-compatibility-kit-bootstrap ruby-rspec))
    (home-page "https://github.com/cucumber/messages/")
    (synopsis "Cucumber Messages for Ruby (Protocol Buffers)")
    (description "Cucumber Messages for Ruby is a library which allows
serialization and deserialization of the protocol buffer messages used in
Cucumber.")
    (license license:expat)))

(define-public ruby-cucumber-tag-expressions
  (package
    (name "ruby-cucumber-tag-expressions")
    (version "5.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cucumber/tag-expressions")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1ziq30szn8m5y29hsdpx4dn1a8sy29h01nvcldm8nr1mx4b7dj1w"))))
    (build-system ruby-build-system)
    (arguments
     (list #:test-target "spec"
           #:phases #~(modify-phases %standard-phases
                        (add-after 'unpack 'chdir
                          (lambda _
                            (chdir "ruby"))))))
    (native-inputs (list ruby-rspec))
    (synopsis "Cucumber tag expressions for Ruby")
    (description "Cucumber tag expression parser for Ruby.  A tag expression
is an infix boolean expression used by Cucumber.")
    (home-page "https://github.com/cucumber/tag-expressions")
    (license license:expat)))

(define-public ruby-cucumber-wire
  (package
    (name "ruby-cucumber-wire")
    (version "6.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "cucumber-wire" version))
       (sha256
        (base32
         "1pmydrh9lcckj7p0cn67jw7msxdkgr9zir86cs19h3mf2zlcv7b9"))))
    (build-system ruby-build-system)
    (arguments
     (list #:tests? #f))                ;tests use cucumber, causing a cycle
    (propagated-inputs
     (list ruby-cucumber-core ruby-cucumber-expressions
           ruby-cucumber-messages))
    (synopsis "Cucumber wire protocol plugin")
    (description "Cucumber's wire protocol allows step definitions to be
implemented and invoked on any platform.")
    (home-page "https://github.com/cucumber/cucumber-ruby-wire")
    (license license:expat)))

(define-public ruby-diff-lcs
  (package
    (name "ruby-diff-lcs")
    (version "1.3")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "diff-lcs" version))
              (sha256
               (base32
                "18w22bjz424gzafv6nzv98h0aqkwz3d9xhm7cbr1wfbyas8zayza"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ; avoid dependency cycles
    (synopsis "Compute the difference between two Enumerable sequences")
    (description "Diff::LCS computes the difference between two Enumerable
sequences using the McIlroy-Hunt longest common subsequence (LCS) algorithm.
It includes utilities to create a simple HTML diff output format and a
standard diff-like tool.")
    (home-page "https://github.com/halostatue/diff-lcs")
    (license license:expat)))

(define-public ruby-docile
  (package
    (name "ruby-docile")
    (version "1.1.5")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "docile" version))
       (sha256
        (base32
         "0m8j31whq7bm5ljgmsrlfkiqvacrw6iz9wq10r3gwrv5785y8gjx"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ; needs github-markup, among others
    (synopsis "Ruby EDSL helper library")
    (description "Docile is a Ruby library that provides an interface for
creating embedded domain specific languages (EDSLs) that manipulate existing
Ruby classes.")
    (home-page "https://ms-ati.github.io/docile/")
    (license license:expat)))

(define-public ruby-fivemat
  (package
    (name "ruby-fivemat")
    (version "1.3.7")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "fivemat" version))
       (sha256
        (base32
         "0pzlycasvwmg4bbx7plllpqnhd9zlmmff8l2w3yii86nrm2nvf9n"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f)) ; no tests
    (synopsis "Each test file given its own line of dots")
    (description
     "Fivemat is a MiniTest/RSpec/Cucumber formatter that gives each test file
its own line of dots during testing.  It aims to provide test output that is
neither too verbose nor too minimal.")
    (home-page "https://github.com/tpope/fivemat")
    (license license:expat)))

(define-public ruby-given-core
  (package
    (name "ruby-given-core")
    (version "3.8.2")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "given_core" version))
       (sha256
        (base32
         "0w1pyhgb2am7c267s8v06dpd9qhmsk2x4hfr2aq8l8lh49ma227s"))))
    (build-system ruby-build-system)
    (arguments '(#:tests? #f))          ;no test suite for the core package
    (propagated-inputs
     (list ruby-sorcerer))
    (synopsis "Core abstractions used by rspec-given and minitest-given")
    (description "Given_core is the basic functionality behind rspec-given and
minitest-given, extensions that allow the use of Given/When/Then terminology
when defining specifications.")
    (home-page "https://github.com/rspec-given/rspec-given")
    (license license:expat)))

(define-public ruby-hoe
  (package
    (name "ruby-hoe")
    (version "4.2.2")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "hoe" version))
              (sha256
               (base32
                "1rhj1zs02mpdw6f4fh3mpfmj0p5pfar7rfxm758pk7l931mm8pyn"))))
    (build-system ruby-build-system)
    (arguments
     (list
      ;; Circular dependency with minitest
      #:tests? #f))
    (synopsis "Ruby project management helper")
    (description
     "Hoe is a rake/rubygems helper for project Rakefiles.  It helps manage,
maintain, and release projects and includes a dynamic plug-in system allowing
for easy extensibility.  Hoe ships with plug-ins for all the usual project
tasks including rdoc generation, testing, packaging, deployment, and
announcement.")
    (home-page "https://www.zenspider.com/projects/hoe.html")
    (license license:expat)))

(define-public ruby-hoe-3
  (package
    (inherit ruby-hoe)
    (version "3.26.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "hoe" version))
              (sha256
               (base32
                "02vmphnfzna1dbb1l5nczcvlvvsg4flr26bdhmvdyf447bpswa63"))))))

(define-public ruby-json
  (package
    (name "ruby-json")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "json" version))
       (sha256
        (base32
         "01v6jjpvh3gnq6sgllpfqahlgxzj50ailwhj9b3cd20hi2dx0vxp"))))
    (build-system ruby-build-system)
    (arguments '(#:tests? #f)) ; dependency cycle with sdoc
    (synopsis "JSON library for Ruby")
    (description "This Ruby library provides a JSON implementation written as
a native C extension.")
    (home-page "http://json-jruby.rubyforge.org/")
    (license (list license:ruby license:gpl2)))) ; GPL2 only

(define-public ruby-mime-types
  (package
    (name "ruby-mime-types")
    (version "3.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "mime-types" version))
       (sha256
        (base32
         "0ipw892jbksbxxcrlx9g5ljq60qx47pm24ywgfbyjskbcl78pkvb"))))
    (build-system ruby-build-system)
    (propagated-inputs
     (list ruby-mime-types-data))
    (native-inputs
     (list ruby-hoe
           ruby-fivemat
           ruby-minitest-focus
           ruby-minitest-bonus-assertions
           ruby-minitest-hooks))
    (synopsis "Library and registry for MIME content type definitions")
    (description "The mime-types library provides a library and registry for
information about Multipurpose Internet Mail Extensions (MIME) content type
definitions.  It can be used to determine defined filename extensions for MIME
types, or to use filename extensions to look up the likely MIME type
definitions.")
    (home-page "https://github.com/mime-types/ruby-mime-types")
    (license license:expat)))

(define-public ruby-mime-types-data
  (package
    (name "ruby-mime-types-data")
    (version "3.2016.0521")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "mime-types-data" version))
       (sha256
        (base32
         "04my3746hwa4yvbx1ranhfaqkgf6vavi1kyijjnw8w3dy37vqhkm"))))
    (build-system ruby-build-system)
    (native-inputs
     (list ruby-hoe))
    (synopsis "Registry for information about MIME media type definitions")
    (description
     "@code{mime-types-data} provides a registry for information about
Multipurpose Internet Mail Extensions (MIME) media type definitions.  It can
be used with the Ruby mime-types library or other software to determine
defined filename extensions for MIME types, or to use filename extensions to
look up the likely MIME type definitions.")
    (home-page "https://github.com/mime-types/mime-types-data/")
    (license license:expat)))

(define-public ruby-mini-portile
  (package
    (name "ruby-mini-portile")
    (version "0.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "mini_portile" version))
       (sha256
        (base32
         "0h3xinmacscrnkczq44s6pnhrp4nqma7k056x5wv5xixvf2wsq2w"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ; tests require network access
    (synopsis "Ports system for Ruby developers")
    (description "Mini-portile is a port/recipe system for Ruby developers.
It provides a standard way to compile against specific versions of libraries
to reproduce user environments.")
    (home-page "https://github.com/flavorjones/mini_portile")
    (license license:expat)))

(define-public ruby-mini-portile-2
  (package
    (inherit ruby-mini-portile)
    (version "2.8.2")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "mini_portile2" version))
              (sha256
               (base32
                "0z7f38iq37h376n9xbl4gajdrnwzq284c9v1py4imw3gri2d5cj6"))))))

(define-public ruby-minitest
  (package
    (name "ruby-minitest")
    (version "5.19.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "minitest" version))
              (sha256
               (base32
                "0jnpsbb2dbcs95p4is4431l2pw1l5pn7dfg3vkgb4ga464j0c5l6"))))
    (build-system ruby-build-system)
    (native-inputs (list ruby-hoe))
    (home-page "https://github.com/minitest/minitest")
    (synopsis "Small test suite library for Ruby")
    (description "Minitest provides a complete suite of Ruby testing
facilities supporting TDD, BDD, mocking, and benchmarking.")
    (license license:expat)))

(define-public ruby-minitest-bonus-assertions
  (package
    (name "ruby-minitest-bonus-assertions")
    (version "3.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "minitest-bonus-assertions" version))
       (sha256
        (base32
         "1hbq9jk904xkz868yha1bqcm6azm7kmjsll2k4pn2nrcib508h2a"))))
    (build-system ruby-build-system)
    (arguments
     (list
      #:tests? #f          ; Test suite has bitrotted.
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'clean-dependencies
            (lambda _
              ;; Remove unneeded require statement that would entail another
              ;; dependency.
              (substitute* "test/minitest_config.rb"
                (("require 'minitest/bisect'") "")))))))
    (native-inputs
     (list ruby-hoe
           ruby-minitest-focus
           ruby-minitest-moar))
    (synopsis "Bonus assertions for @code{Minitest}")
    (description
     "Minitest bonus assertions provides extra MiniTest assertions.  For
instance, it provides @code{assert_true}, @code{assert_false} and
@code{assert_set_equal}.")
    (home-page "https://github.com/halostatue/minitest-bonus-assertions")
    (license license:expat)))

(define-public ruby-minitest-focus
  (package
    (name "ruby-minitest-focus")
    (version "1.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "minitest-focus" version))
       (sha256
        (base32
         "13kd2dkd9akfb99ziqndz9mir5iynyfyj2l45mcibab6mq5k8g67"))))
    (build-system ruby-build-system)
    (propagated-inputs
     (list ruby-minitest))
    (native-inputs
     (list ruby-hoe))
    (synopsis "Allows a few specific tests to be focused on")
    (description
     "@code{minitest-focus} gives the ability focus on a few tests with ease
without having to use command-line arguments.  It introduces a @code{focus}
class method for use in testing classes, specifying that the next defined test
is to be run.")
    (home-page "https://github.com/seattlerb/minitest-focus")
    (license license:expat)))

(define-public ruby-minitest-hooks
  (package
    (name "ruby-minitest-hooks")
    (version "1.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "minitest-hooks" version))
       (sha256
        (base32 "11jb31dl5kbpyl3kgxql0p7da9066r2aqw54y5q6cni9nmld3zf5"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f))  ; No tests bundled.
    (native-inputs
     (list ruby-sequel ;ruby-sqlite3
           ))
    (synopsis "Hooks for the minitest framework")
    (description
     "Minitest-hooks adds @code{around}, @code{before_all}, @code{after_all},
@code{around_all} hooks for Minitest.  This allows, for instance, running each
suite of specs inside a database transaction, running each spec inside its own
savepoint inside that transaction.  This can significantly speed up testing
for specs that share expensive database setup code.")
    (home-page "https://github.com/jeremyevans/minitest-hooks")
    (license license:expat)))

(define-public ruby-minitest-moar
  (package
    (name "ruby-minitest-moar")
    (version "0.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "minitest-moar" version))
       (sha256
        (base32
         "0nb83blrsab92gcy6nfpw39njys7zisia8pw4igzzfzfl51cis0x"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'clean-dependencies
           (lambda _
             ;; Remove all gems defined in the Gemfile because these are not
             ;; truly needed.
             (substitute* "Gemfile"
               (("gem .*") ""))
             ;; Remove byebug as not needed to run tests.
             (substitute* "test/test_helper.rb"
               (("require 'byebug'") "")))))))
    (native-inputs
     (list bundler ruby-minitest))
    (synopsis "Extra features and changes to MiniTest")
    (description "@code{MiniTest Moar} add some additional features and
changes some default behaviours in MiniTest.  For instance, Moar replaces the
MiniTest @code{Object#stub} with a global @code{stub} method.")
    (home-page "https://github.com/dockyard/minitest-moar")
    (license license:expat)))

(define-public ruby-multi-test
  (package
    (name "ruby-multi-test")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "multi_test" version))
       (sha256
        (base32
         "1sx356q81plr67hg16jfwz9hcqvnk03bd9n75pmdw8pfxjfy1yxd"))))
    (build-system ruby-build-system)
    (arguments
     '(;; Tests require different sets of specific gem versions to be available,
       ;; and there is no gemfile that specifies the newest versions of
       ;; dependencies to be tested.
       #:tests? #f))
    (synopsis
     "Interface to testing libraries loaded into a running Ruby process")
    (description
     "@code{multi_test} provides a uniform interface onto whatever testing
libraries that have been loaded into a running Ruby process to help control
rogue test/unit/autorun requires.")
    (home-page "https://github.com/cucumber/multi_test")
    (license license:expat)))

(define-public ruby-rake
  (package
    (name "ruby-rake")
    (version "13.3.0")
    (source
     (origin
       (method git-fetch)               ;for tests
       (uri (git-reference
             (url "https://github.com/ruby/rake")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "01ixsz1k6y9ckzmyjmspbi5s213m9b7imglb9iypjmf1nrmsvgkx"))))
    (build-system ruby-build-system)
    (native-inputs
     (list bundler))
    (synopsis "Rake is a Make-like program implemented in Ruby")
    (description
     "Rake is a Make-like program where tasks and dependencies are specified
in standard Ruby syntax.")
    (home-page "https://github.com/ruby/rake")
    (license license:expat)))

(define-public ruby-rake-compiler
  (package
    (name "ruby-rake-compiler")
    (version "1.2.9")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "rake-compiler" version))
              (sha256
               (base32
                "01rnl94p1sr84xkbnh66db42qsndykbfx2z2fggxyxx9vnji6cjs"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ; needs cucumber
    (synopsis "Building and packaging helper for Ruby native extensions")
    (description "Rake-compiler provides a framework for building and
packaging native C and Java extensions in Ruby.")
    (home-page "https://github.com/rake-compiler/rake-compiler")
    (license license:expat)))

(define-public ruby-rake-compiler-dock
  (package
    (name "ruby-rake-compiler-dock")
    (version "1.3.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "rake-compiler-dock" version))
              (sha256
               (base32
                "0yr5f72irvhmnk12q7bbr4qw0xwy7diqkbcvb4lygjbg7rvk3k8k"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))      ;test suite requires docker
    (synopsis "Cross compiler environment for building Ruby gems")
    (description "The code{rake-compiler-dock} gem provides a cross compiler
environment for building gems on a variety of platforms (GNU/Linux, JRuby,
Windows and Mac).")
    (home-page "https://github.com/rake-compiler/rake-compiler-dock")
    (license license:expat)))

(define-public ruby-rubocop/minimal
  (hidden-package
   (package
     (name "ruby-rubocop")
     (version "1.68.0")
     (source
      (origin
        (method git-fetch)               ;no tests in distributed gem
        (uri (git-reference
              (url "https://github.com/rubocop/rubocop")
              (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0pfsrgkg2dhb6a2rknciqskgxgmb9kf48rvbkhay9n8n6m712v2w"))))
     (build-system ruby-build-system)
     (arguments
      (list #:tests? #f
            #:phases
            #~(modify-phases %standard-phases
                (add-after 'unpack 'remove-runtime-dependencies
                  (lambda _
                    (substitute* "rubocop.gemspec"
                      (("s\\.add_dependency.*") "")))))))
     (synopsis "Ruby code style checking tool")
     (description
      "@code{rubocop} is a Ruby code style checking tool.  It aims to enforce
the community-driven Ruby Style Guide.")
     (home-page "https://github.com/rubocop/rubocop")
     (license license:expat))))

(define-public ruby-rspec
  (package
    (name "ruby-rspec")
    (version "3.13.1")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "rspec" version))
              (sha256
               (base32
                "0h11wynaki22a40rfq3ahcs4r36jdpz9acbb3m5dkf0mm67sbydr"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ; avoid dependency cycles
    (propagated-inputs
     (list ruby-rspec-core ruby-rspec-expectations ruby-rspec-mocks))
    (synopsis "Behavior-driven development framework for Ruby")
    (description "RSpec is a behavior-driven development (BDD) framework for
Ruby.  This meta-package includes the RSpec test runner, along with the
expectations and mocks frameworks.")
    (home-page "https://rspec.info/")
    (license license:expat)))

(define-public ruby-rspec-2
  (package (inherit ruby-rspec)
    (version "2.14.1")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "rspec" version))
              (sha256
               (base32
                "134y4wzk1prninb5a0bhxgm30kqfzl8dg06af4js5ylnhv2wd7sg"))))
    (propagated-inputs
     (list ruby-rspec-core-2 ruby-rspec-mocks-2 ruby-rspec-expectations-2))))

(define-public ruby-rspec-core
  (package
    (name "ruby-rspec-core")
    (version "3.13.2")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "rspec-core" version))
              (sha256
               (base32
                "001kazj244cb6fbkmh7ap74csbr78717qaskqzqpir1q8xpdmywl"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ; avoid dependency cycles
    (propagated-inputs
     (list ruby-rspec-support))
    (synopsis "RSpec core library")
    (description "Rspec-core provides the RSpec test runner and example
groups.")
    (home-page "https://github.com/rspec/rspec-core")
    (license license:expat)))

(define-public ruby-rspec-core-2
  (package (inherit ruby-rspec-core)
    (version "2.14.8")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "rspec-core" version))
              (sha256
               (base32
                "0psjy5kdlz3ph39br0m01w65i1ikagnqlg39f8p65jh5q7dz8hwc"))))
    (arguments
     (cons*
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch
            (lambda _
              (substitute* "lib/rspec/core/ruby_project.rb"
                (("File\\.exists\\?") "File.exist?")))))
      (package-arguments ruby-rspec-core)))
    (propagated-inputs `())))

(define-public ruby-rspec-expectations
  (package
    (name "ruby-rspec-expectations")
    (version "3.13.3")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "rspec-expectations" version))
              (sha256
               (base32
                "0n3cyrhsa75x5wwvskrrqk56jbjgdi2q1zx0irllf0chkgsmlsqf"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ; avoid dependency cycles
    (propagated-inputs
     (list ruby-diff-lcs ruby-rspec-support))
    (synopsis "RSpec expectations library")
    (description "Rspec-expectations provides a simple API to express expected
outcomes of a code example.")
    (home-page "https://github.com/rspec/rspec-expectations")
    (license license:expat)))

(define-public ruby-rspec-expectations-2
  (package (inherit ruby-rspec-expectations)
    (version "2.14.5")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "rspec-expectations" version))
              (sha256
               (base32
                "1ni8kw8kjv76jvwjzi4jba00k3qzj9f8wd94vm6inz0jz3gwjqf9"))))
    (propagated-inputs
     (list ruby-diff-lcs))))

(define-public ruby-rspec-mocks
  (package
    (name "ruby-rspec-mocks")
    (version "3.13.2")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "rspec-mocks" version))
              (sha256
               (base32
                "1vxxkb2sf2b36d8ca2nq84kjf85fz4x7wqcvb8r6a5hfxxfk69r3"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ; avoid dependency cycles
    (propagated-inputs
     (list ruby-diff-lcs ruby-rspec-support))
    (synopsis "RSpec stubbing and mocking library")
    (description "Rspec-mocks provides RSpec's \"test double\" framework, with
support for stubbing and mocking.")
    (home-page "https://github.com/rspec/rspec-mocks")
    (license license:expat)))

(define-public ruby-rspec-mocks-2
  (package (inherit ruby-rspec-mocks)
    (version "2.14.6")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "rspec-mocks" version))
              (sha256
               (base32
                "1fwsmijd6w6cmqyh4ky2nq89jrpzh56hzmndx9wgkmdgfhfakv30"))))
    (propagated-inputs
     (list ruby-diff-lcs))))

;; RSpec is the dominant testing library for Ruby projects.  Even RSpec's
;; dependencies use RSpec for their test suites!  To avoid these circular
;; dependencies, we disable tests for all of the RSpec-related packages.
(define-public ruby-rspec-support
  (package
    (name "ruby-rspec-support")
    (version "3.13.2")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "rspec-support" version))
              (sha256
               (base32
                "1v6v6xvxcpkrrsrv7v1xgf7sl0d71vcfz1cnrjflpf6r7x3a58yf"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ; avoid dependency cycles
    (synopsis "RSpec support library")
    (description "Support utilities for RSpec gems.")
    (home-page "https://github.com/rspec/rspec-support")
    (license license:expat)))

(define-public ruby-sequel
  (package
    (name "ruby-sequel")
    (version "5.47.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "sequel" version))
       (sha256
        (base32
         "03pmhj4kc3ga75wy397l57bvd18jxxmrk3qsznjw93b993qgvj3z"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ; Avoid dependency loop with ruby-minitest-hooks.
    (synopsis "Database toolkit for Ruby")
    (description "Sequel provides thread safety, connection pooling and a
concise DSL for constructing SQL queries and table schemas.  It includes a
comprehensive ORM layer for mapping records to Ruby objects and handling
associated records.")
    (home-page "https://sequel.jeremyevans.net")
    (license license:expat)))

(define-public ruby-simplecov-json-formatter
  (package
    (name "ruby-simplecov-json-formatter")
    (version "0.1.4")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "simplecov_json_formatter" version))
              (sha256
               (base32
                "0a5l0733hj7sk51j81ykfmlk2vd5vaijlq9d5fn165yyx3xii52j"))))
    (build-system ruby-build-system)
    ;; The test suite is disabled because it requires simplecov, which
    ;; requires this, introducing a dependency cycle.
    (arguments (list #:tests? #f))
    (synopsis "JSON formatter for SimpleCov")
    (description "This package provides a JSON formatter for SimpleCov, the
Ruby code coverage tool.")
    (home-page
     "https://github.com/codeclimate-community/simplecov_json_formatter")
    (license license:expat)))

(define-public ruby-simplecov
  (package
    (name "ruby-simplecov")
    (version "0.22.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "simplecov" version))
              (sha256
               (base32
                "198kcbrjxhhzca19yrdcd6jjj9sb51aaic3b0sc3pwjghg3j49py"))))
    (build-system ruby-build-system)
    ;; Simplecov depends on rubocop for code style checking at build time.
    ;; Rubocop needs simplecov at build time.
    (arguments `(#:tests? #f))
    (propagated-inputs
     (list ruby-json
           ruby-docile
           ruby-simplecov-html
           ruby-simplecov-json-formatter))
    (synopsis "Code coverage framework for Ruby")
    (description "SimpleCov is a code coverage framework for Ruby with a
powerful configuration library and automatic merging of coverage across test
suites.")
    (home-page "https://github.com/simplecov-ruby/simplecov")
    (license license:expat)))

(define-public ruby-simplecov-html
  (package
    (name "ruby-simplecov-html")
    (version "0.12.3")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "simplecov-html" version))
              (sha256
               (base32
                "0yx01bxa8pbf9ip4hagqkp5m0mqfnwnw2xk8kjraiywz4lrss6jb"))))
    (build-system ruby-build-system)
    (arguments `(#:tests? #f)) ; there are no tests
    (native-inputs
     (list bundler))
    (synopsis "Default HTML formatter for SimpleCov code coverage tool")
    (description "This package provides the default HTML formatter for
the SimpleCov code coverage tool for Ruby version 1.9 and above.")
    (home-page "https://github.com/simplecov-ruby/simplecov-html")
    (license license:expat)))

(define-public ruby-sorcerer
  (package
    (name "ruby-sorcerer")
    (version "2.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "sorcerer" version))
        (sha256
          (base32
            "0d32ha9pp9slpmsm027pkdpbr9vc5jn2m8rl6hwwx6a87m8cr58h"))))
    (build-system ruby-build-system)
    (synopsis "Ripper-style abstract syntax tree to Ruby source generator")
    (description "Sorcerer generates Ruby code from a Ripper-like abstract
syntax tree (i.e. S-Expressions).  Sorcerer is targeted mainly at small
snippets of Ruby code, expressible in a single line.  Longer examples may be
re-sourced, but they will be rendered in a single-line format.")
    (home-page "https://github.com/rspec-given/sorcerer")
    (license license:expat)))

(define-public ruby-unindent
  (package
  (name "ruby-unindent")
  (version "1.0")
  (source
    (origin
      (method url-fetch)
      (uri (rubygems-uri "unindent" version))
      (sha256
        (base32
          "1wqh3rzv8589yzibigminxx3qpmj2nqj28f90xy1sczk1pijmcrd"))))
  (build-system ruby-build-system)
  (synopsis "Ruby method to unindent strings")
  (description "This module provides a @code{String#unindent} Ruby method to
unindent strings, which can be useful to unindent multiline strings embedded
in already-indented code.")
  (home-page "https://github.com/mynyml/unindent")
  (license license:expat)))

;;;
;;; Avoid adding new packages to the end of this file. To reduce the chances
;;; of a merge conflict, place them above in alphabetic order.
;;;
