;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015 Pjotr Prins <pjotr.guix@thebird.nl>
;;; Copyright © 2014-2017, 2021-2022, 2024 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014, 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014, 2015 David Thompson <davet@gnu.org>
;;; Copyright © 2015, 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015, 2016, 2017 Ben Woodcroft <donttrustben@gmail.com>
;;; Copyright © 2017 Nikita <nikita@n0.is>
;;; Copyright © 2017, 2019-2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2017-2025 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017, 2018, 2020, 2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2017, 2018, 2019 Christopher Baines <mail@cbaines.net>
;;; Copyright © 2018 Vasile Dumitrascu <va511e@yahoo.com>
;;; Copyright © 2018 Alex Vong <alexvong1995@gmail.com>
;;; Copyright © 2019 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2019 Mikhail Kirillov <w96k.ru@gmail.com>
;;; Copyright © 2019 Jelle Licht <jlicht@fsfe.org>
;;; Copyright © 2019 Brian Leung <bkleung89@gmail.com>
;;; Copyright © 2019 Collin J. Doering <collin@rekahsoft.ca>
;;; Copyright © 2019 Diego N. Barbato <dnbarbato@posteo.de>
;;; Copyright © 2019 Brett Gilio <brettg@posteo.de>
;;; Copyright © 2020, 2023 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2020, 2021 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2020 Michael Rohleder <mike@rohleder.de>
;;; Copyright © 2020 Holgr Peters <holger.peters@posteo.de>
;;; Copyright © 2020 Giacomo Leidi <goodoldpaul@autistici.org>
;;; Copyright © 2021 EuAndreh <eu@euandre.org>
;;; Copyright © 2020 Tomás Ortín Fernández <tomasortin@mailbox.org>
;;; Copyright © 2021 Giovanni Biscuolo <g@xelera.eu>
;;; Copyright © 2022 Philip McGrath <philip@philipmcgrath.com>
;;; Copyright © 2022-2024 Remco van 't Veer <remco@remworks.net>
;;; Copyright © 2022 Taiju HIGASHI <higashi@taiju.info>
;;; Copyright © 2023 Yovan Naumovski <yovan@gorski.stream>
;;; Copyright © 2023, 2024 gemmaro <gemmaro.dev@gmail.com>
;;; Copyright © 2023, 2024 Janneke Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2023, 2024 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2023, 2024 Hartmut Goebel <h.goebel@crazy-compilers.com>
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

(define-module (gnu packages ruby)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages c)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages dbm)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages rails)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages java)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages libidn)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lsof)
  #:use-module (gnu packages man)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages node)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages ragel)
  #:use-module (gnu packages rsync)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages valgrind)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages virtualization)
  #:use-module (gnu packages web-browsers)
  #:use-module (gnu packages serialization)
  #:use-module (guix packages)
  #:use-module (guix deprecation)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages web)
  #:use-module (guix build-system ruby)
  #:use-module ((srfi srfi-1) #:select (alist-delete)))

(define %prawn-project-licenses
  ;; This set of licenses applies to most (all?) components of the Prawn
  ;; project (it is triple licensed).
  (list license:ruby
        license:gpl2+
        license:gpl3+))

(define-public ruby-2.6
  (package
    (name "ruby")
    (version "2.6.10")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://cache.ruby-lang.org/pub/ruby/"
                           (version-major+minor version)
                           "/ruby-" version ".tar.xz"))
       (sha256
        (base32
         "1wn12klc44hn2nh5v1lkqbdyvljip6qhwjqvkkf8zf112gaxxn2z"))
       (modules '((guix build utils)))
       (snippet `(begin
                   ;; Remove bundled libffi
                   (delete-file-recursively "ext/fiddle/libffi-3.2.1")
                   #t))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"
       #:configure-flags
       ,(if (%current-target-system)
            '(list (string-append
                    "LDFLAGS=-Wl,-rpath="
                    (assoc-ref %outputs "out") "/lib")
                   "--enable-shared")
            ''("--enable-shared")) ; dynamic linking
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'replace-bin-sh-and-remove-libffi
           (lambda _
             (substitute* '("Makefile.in"
                            "ext/pty/pty.c"
                            "io.c"
                            "lib/mkmf.rb"
                            "process.c"
                            "test/rubygems/test_gem_ext_configure_builder.rb"
                            "test/rdoc/test_rdoc_parser.rb"
                            "test/ruby/test_rubyoptions.rb"
                            "test/ruby/test_process.rb"
                            "test/ruby/test_system.rb"
                            "tool/rbinstall.rb")
               (("/bin/sh") (which "sh")))
             #t)))))
    (native-inputs (if (%current-target-system)
                       (list this-package)
                       '()))
    (inputs
     (list readline openssl-1.1 libffi gdbm))
    (propagated-inputs
     (list zlib))
    (native-search-paths
     (list (search-path-specification
            (variable "GEM_PATH")
            (files (list (string-append "lib/ruby/vendor_ruby"))))))
    (synopsis "Programming language interpreter")
    (description "Ruby is a dynamic object-oriented programming language with
a focus on simplicity and productivity.")
    (home-page "https://www.ruby-lang.org")
    (license license:ruby)))

(define-public ruby-2.7
  (package
    (inherit ruby-2.6)
    (version "2.7.8")
    (source
     (origin
       (inherit (package-source ruby-2.6))
       (uri (string-append "https://cache.ruby-lang.org/pub/ruby/"
                           (version-major+minor version)
                           "/ruby-" version ".tar.gz"))
       (sha256
        (base32
         "182vni66djmiqagwzfsd0za7x9k3zag43b88c590aalgphybdnn2"))))
    (arguments
     `(#:test-target "test"
       #:configure-flags
       ,(if (%current-target-system)
            '(list (string-append
                    "LDFLAGS=-Wl,-rpath="
                    (assoc-ref %outputs "out") "/lib")
                   "--enable-shared")
            ''("--enable-shared")) ; dynamic linking
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'replace-bin-sh-and-remove-libffi
           (lambda _
             (substitute* '("configure.ac"
                            "template/Makefile.in"
                            "lib/rubygems/installer.rb"
                            "ext/pty/pty.c"
                            "io.c"
                            "lib/mkmf.rb"
                            "process.c"
                            "test/rubygems/test_gem_ext_configure_builder.rb"
                            "test/rdoc/test_rdoc_parser.rb"
                            "test/ruby/test_rubyoptions.rb"
                            "test/ruby/test_process.rb"
                            "test/ruby/test_system.rb"
                            "tool/rbinstall.rb")
               (("/bin/sh") (which "sh")))
             #t))
         ,@(if (system-hurd?)
               '((add-after 'unpack 'skip-tests
                   (lambda _
                     (delete-file "bootstraptest/test_io.rb")
                     (delete-file "test/ruby/test_io.rb"))))
               '()))))
    (native-inputs
     (append (if (%current-target-system)
                 (list this-package)
                 '())
             (list autoconf)))))

(define-public ruby-3.0
  (package
    (inherit ruby-2.7)
    (version "3.0.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://cache.ruby-lang.org/pub/ruby/"
                           (version-major+minor version)
                           "/ruby-" version ".tar.xz"))
       (sha256
        (base32
         "1lfvgm6jbspmwmc3haww83l1l8vl1airzi08ljrcz19dws9yxjxm"))))
    (inputs
     (modify-inputs (package-inputs ruby-2.7)
       (replace "openssl" openssl)))))

(define-public ruby-3.1
  (package
    (inherit ruby-3.0)
    (version "3.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://cache.ruby-lang.org/pub/ruby/"
                           (version-major+minor version)
                           "/ruby-" version ".tar.xz"))
       (sha256
        (base32
         "0kzr792rk9n9yrqlyrkc1a0cmbk5y194f7v7p4vwjdk0ww860v8v"))))))

(define-public ruby-3.2
  (package
    (inherit ruby-3.1)
    (version "3.2.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://cache.ruby-lang.org/pub/ruby/"
                           (version-major+minor version)
                           "/ruby-" version ".tar.xz"))
       (sha256
        (base32
         "0ss7pb7f62sakq5ywpw3dl0v586cl61cd91qlm1i094c9fak3cng"))))
    (inputs
     (modify-inputs (package-inputs ruby-3.1)
       (prepend libyaml)))))

(define-public ruby-3.3
  (package
    (inherit ruby-3.2)
    (version "3.3.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://cache.ruby-lang.org/pub/ruby/"
                           (version-major+minor version)
                           "/ruby-" version ".tar.xz"))
       (sha256
        (base32
         "07pwf3zkf7idl95agfjbv2lvamcp0spp0znqp9arb71ri19rkh43"))))))

(define-public ruby ruby-3.1)

(define-public mruby
  (package
    (name "mruby")
    (version "3.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mruby/mruby")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0zynr6dk0zxdip53il0qr0rhyzmjicpkxs63l77acpx8b05h8amc"))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after 'unpack 'enable-verbose-tests
           (lambda _
             (substitute* "Makefile"
               (("ruby ./minirake" m)
                (string-append m " --verbose")))
             #t))
         (add-after 'unpack 'disable-broken-tests
           (lambda _
             (substitute* "mrbgems/mruby-io/test/io.rb"
               (("assert\\('IO.popen.+$" m)
                (string-append m "skip \"Hangs in the Guix build environment\"\n"))
               ;; This one is really weird.  The *expected* output is all wrong.
               (("assert\\('`cmd`.*" m)
                (string-append m "skip \"Disable for Guix\"\n")))
             #t))
         ;; There is no install target
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (lib (string-append out "/lib"))
                    (inc (string-append out "/include")))
               (mkdir-p bin)
               (copy-recursively "build/host/bin" bin)
               (mkdir-p lib)
               (copy-recursively "build/host/lib" lib)
               (mkdir-p inc)
               (copy-recursively "include" inc))
             #t)))))
    (native-inputs
     (list ruby bison))
    (home-page "https://github.com/mruby/mruby")
    (synopsis "Lightweight Ruby")
    (description "mruby is the lightweight implementation of the Ruby
language.  Its syntax is Ruby 3.x compatible except for pattern
matching.  mruby can be linked and embedded within your application.")
    (license license:expat)))

(define-public ruby-commander
  (package
    (name "ruby-commander")
    (version "4.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "commander" version))
       (sha256
        (base32
         "1n8k547hqq9hvbyqbx2qi08g0bky20bbjca1df8cqq5frhzxq7bx"))))
    (build-system ruby-build-system)
    (arguments
     (list
      #:test-target "spec"
      #:phases
      #~(modify-phases %standard-phases
          ;; Don't run or require rubocop, the code linting tool, as this is a
          ;; bit unnecessary.
          (add-after 'unpack 'dont-run-rubocop
            (lambda _
              (substitute* "Rakefile"
                ((".*rubocop.*") "")
                ((".*RuboCop.*") "")))))))
    (propagated-inputs
     (list ruby-highline))
    (native-inputs
     (list bundler ruby-rspec-core ruby-rspec-expectations
           ruby-rspec-mocks ruby-simplecov))
    (home-page "https://github.com/commander-rb/commander")
    (synopsis "Library for building Ruby command-line executables")
    (description
     "Commander aims to be a complete solution for Ruby command-line
executables.  Commander bridges the gap between other terminal related
libraries (OptionParser, HighLine), while providing many new features, and an
elegant API.")
    (license license:expat)))

(define-public ruby-highline
  (package
    (name "ruby-highline")
    (version "2.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "highline" version))
       (sha256
        (base32
         "0gr6pckj2jayxw1gdgh9193j5jag5zrrqqlrnl4jvcwpyd3sn2zc"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f)) ;; TODO: NameError: uninitialized constant SPEC
    (native-inputs
     (list bundler ruby-code-statistics))
    (synopsis
     "HighLine helps you build command-line interfaces")
    (description
     "HighLine provides a high-level IO library that provides validation,
type conversion, and more for command-line interfaces.  HighLine also includes
a menu system for providing multiple options to the user.")
    (home-page "https://github.com/JEG2/highline")
    (license (list license:gpl2 license:ruby))))

(define-public ruby-hoe
  (package
    (name "ruby-hoe")
    (version "4.0.4")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "hoe" version))
              (sha256
               (base32
                "0r2hy7mq9jd9hsbvskd9sxfbagc92adnn7abzxbda05sscbf46hn"))))
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

(define-public ruby-rake-compiler
  (package
    (name "ruby-rake-compiler")
    (version "1.1.1")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "rake-compiler" version))
              (sha256
               (base32
                "11sxgw10jrd6a4irb51jjwam9wikixn5kss11pw4b80cmh32yvpf"))))
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

(define-public ruby-rsync
  (package
    (name "ruby-rsync")
    (version "1.0.9")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "rsync" version))
       (sha256
        (base32
         "0p8b27q1gvxilqfq2528xpwglzcm2myikkjxpqk7mwbwg9r6knxv"))))
    (build-system ruby-build-system)
    (arguments
     '(#:test-target "spec"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-coveralls-requirement
           (lambda _
             (substitute* "spec/spec_helper.rb"
               (("require 'coveralls'") "")
               (("Coveralls.wear!") ""))
             #t)))))
    (native-inputs
     (list bundler rsync ruby-rspec-core ruby-rspec-expectations
           ruby-rspec-mocks))
    (home-page "https://github.com/jbussdieker/ruby-rsync")
    (synopsis "Ruby wrapper around rsync")
    (description
     "Ruby Rsync is a Ruby library that can synchronize files between remote
hosts by wrapping the @file{rsync} binary.")
    (license license:expat)))

(define-public ruby-i18n
  (package
    (name "ruby-i18n")
    (version "1.13.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "i18n" version))
              (sha256
               (base32
                "1yk33slipi3i1kydzrrchbi7cgisaxym6pgwlzx7ir8vjk6wl90x"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ; no tests
    (propagated-inputs `(("concurrent-ruby" ,ruby-concurrent)))
    (synopsis "Internationalization library for Ruby")
    (description "Ruby i18n is an internationalization and localization
solution for Ruby programs.  It features translation and localization,
interpolation of values to translations, pluralization, customizable
transliteration to ASCII, flexible defaults, bulk lookup, lambdas as
translation data, custom key/scope separator, custom exception handlers, and
an extensible architecture with a swappable backend.")
    (home-page "https://github.com/ruby-i18n/i18n")
    (license license:expat)))

(define-public ruby-io-console
  (package
    (name "ruby-io-console")
    (version "0.6.0")
    (source (origin
              (method git-fetch)        ;for tests
              (uri (git-reference
                    (url "https://github.com/ruby/io-console/")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0gwxrp29b6awkid1sf85sbh529mnq6hb86m8c2443cm6nc4vr8qb"))))
    (build-system ruby-build-system)
    (arguments
     (list #:phases #~(modify-phases %standard-phases
                        (add-after 'unpack 'delete-rakelib-files
                          (lambda _
                            ;; These depend on git and other extraneous
                            ;; dependencies, and are loaded by rake.
                            (delete-file-recursively "rakelib"))))))
    (native-inputs (list ruby-rake-compiler))
    (synopsis "Console capabilities library for IO instances")
    (description "IO.console adds console capabilities to Ruby IO instances.")
    (home-page "https://github.com/ruby/io-console")
    (license license:bsd-2)))

(define-public ruby-irb
  (package
    (name "ruby-irb")
    (version "1.6.3")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "irb" version))
              (sha256
               (base32
                "1h9s07n5v3z029v18924ws9vdkdc80n6llp9ccx77yg1krv2g0f3"))))
    (build-system ruby-build-system)
    ;; XXX: Disable the test suite, as it requires debug, which requires this
    ;; package (dependency cycle).
    (arguments (list #:tests? #f))
    (propagated-inputs (list ruby-reline))
    (synopsis "Ruby command-line tool for REPL (Read Eval Print Loop)")
    (description "IRB is an interactive Ruby command-line tool for REPL (Read
Eval Print Loop).")
    (home-page "https://github.com/ruby/irb")
    (license license:bsd-2)))

(define-public ruby-irb-1.1.1
  (package
    (inherit ruby-irb)
    (version "1.1.1")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "irb" version))
              (sha256
               (base32
                "0h605w798s2bg9wg681ynvvzgdz1yy69gh387bl0khw9ll7wkn8v"))))))

(define-public ruby-iruby
  (package
    (name "ruby-iruby")
    (version "0.3")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "iruby" version))
       (sha256
        (base32
         "1wdf2c0x8y6cya0n3y0p3p7b1sxkb2fdavdn2k58rf4rs37s7rzn"))))
    (build-system ruby-build-system)
    (arguments
     ;; TODO: Tests currently fail.
     ;;
     ;; Finished in 1.764405s, 1.1335 runs/s, 5.1009 assertions/s.
     ;;
     ;;   1) Failure:
     ;; IntegrationTest#test_interaction [/tmp/guix-build-ruby-iruby-0.3.drv-0/gem/test/integration_test.rb:25]:
     ;; In [ expected
     ;;
     ;; 2 runs, 9 assertions, 1 failures, 0 errors, 0 skips
     '(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-ipython
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "lib/iruby/command.rb"
               (("version = `")
                (string-append
                 "version = `"
                 (assoc-ref inputs "python-ipython")
                 "/bin/"))
               (("Kernel\\.exec\\('")
                (string-append
                 "Kernel.exec('"
                 (assoc-ref inputs "python-ipython")
                 "/bin/")))
             #t)))))
    (inputs
     (list python-ipython))
    (propagated-inputs
     (list ruby-bond
           ruby-data_uri
           ruby-mimemagic
           ruby-multi-json
           ruby-cztop
           ;; Optional inputs
           ruby-pry))
    (synopsis "Ruby kernel for Jupyter/IPython")
    (description
     "This package provides a Ruby kernel for Jupyter/IPython frontends (e.g.
notebook).")
    (home-page "https://github.com/SciRuby/iruby")
    (license license:expat)))

;; RSpec is the dominant testing library for Ruby projects.  Even RSpec's
;; dependencies use RSpec for their test suites!  To avoid these circular
;; dependencies, we disable tests for all of the RSpec-related packages.
(define-public ruby-rspec-support
  (package
    (name "ruby-rspec-support")
    (version "3.12.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "rspec-support" version))
              (sha256
               (base32
                "12y52zwwb3xr7h91dy9k3ndmyyhr3mjcayk0nnarnrzz8yr48kfx"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ; avoid dependency cycles
    (synopsis "RSpec support library")
    (description "Support utilities for RSpec gems.")
    (home-page "https://github.com/rspec/rspec-support")
    (license license:expat)))

(define-public ruby-rspec-core
  (package
    (name "ruby-rspec-core")
    (version "3.12.1")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "rspec-core" version))
              (sha256
               (base32
                "0da45cvllbv39sdbsl65vp5djb2xf5m10mxc9jm7rsqyyxjw4h1f"))))
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

(define-public ruby-date
  (package
    (name "ruby-date")
    (version "3.3.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ruby/date")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1jiqjnaap1jk1r8z37iicnzqha1rhc713qmcir17f4vnz8ac8v75"))))
    (build-system ruby-build-system)
    (arguments (list #:test-target "default"))
    (native-inputs (list ruby-rake-compiler))
    (synopsis "Ruby @code{Object} subclass with date comparison capability")
    (description "This package provides a subclass of @code{Object} that
includes the @code{Comparable} module for handling dates.")
    (home-page "https://github.com/ruby/date")
    (license license:bsd-2)))

(define-public ruby-time
  (package
    (name "ruby-time")
    (version "0.3.0")
    (source (origin
              (method git-fetch)  ; for tests
              (uri (git-reference
                    (url "https://github.com/ruby/time")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0jd6df2lxd60wcxyaf37j8v3nnfn952d5xhg6aap9zlcdmkk4g2n"))))
    (build-system ruby-build-system)
    (propagated-inputs (list ruby-date))
    (native-inputs (list ruby-test-unit-ruby-core))
    (synopsis
     "Extends the Time class with methods for parsing and conversion")
    (description
     "When this gem is @code{require}d, it extends the Time class with with
additional methods for parsing and converting Times.")
    (home-page "https://github.com/ruby/time")
    (license license:bsd-2)))

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

(define-public ruby-rspec-expectations
  (package
    (name "ruby-rspec-expectations")
    (version "3.12.2")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "rspec-expectations" version))
              (sha256
               (base32
                "03ba3lfdsj9zl00v1yvwgcx87lbadf87livlfa5kgqssn9qdnll6"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ; avoid dependency cycles
    (propagated-inputs
     (list ruby-rspec-support ruby-diff-lcs))
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

(define-public ruby-sorted-set
  (package
    (name "ruby-sorted-set")
    (version "1.0.3")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "sorted_set" version))
              (sha256
               (base32
                "0brpwv68d7m9qbf5js4bg8bmg4v7h4ghz312jv9cnnccdvp8nasg"))))
    (build-system ruby-build-system)
    (propagated-inputs
     (list ruby-rbtree ruby-set))
    (synopsis
     "Ruby Set variant whose elements are sorted in ascending order")
    (description
     "This package implements a variant of Set whose elements are sorted in
ascending order")
    (home-page "https://github.com/knu/sorted_set")
    (license license:bsd-2)))

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

(define-public ruby-rspec-given
  (package
    (name "ruby-rspec-given")
    (version "3.8.2")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "rspec-given" version))
       (sha256
        (base32
         "0xzzxjjzwrsp84p12sd6ab3jbm9kh7sbnqpxgc9mlfq3s3ll0fdj"))))
    (build-system ruby-build-system)
    (arguments
     `(#:test-target "rs"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-rakefile
           (lambda _
             (substitute* '("Rakefile" "rakelib/gemspec.rake")
               (("require '\\./lib/given/.*") "")
               (("Given::VERSION") (format #f "~s" ,version))
               ;; Fix the error: "cannot load such file -- example_helper"
               (("sh \"rspec")
                "sh \"rspec -Ilib:examples"))))
         (add-after 'extract-gemspec 'delete-failing-tests
           ;; See: https://github.com/jimweirich/rspec-given/issues/57.
           (lambda _
             (substitute* ".gemspec"
               (("\"spec/lib/given/natural_assertion_spec.rb\".freeze, ")
                "")
               (("\"examples/integration/failing_messages_spec.rb\".freeze, ")
                ""))
             (delete-file "spec/lib/given/natural_assertion_spec.rb")
             (delete-file "examples/integration/failing_messages_spec.rb"))))))
    (native-inputs
     (list ruby-rspec ruby-minitest))
    (propagated-inputs
     (list ruby-given-core ruby-rspec))
    (synopsis "Given/When/Then for RSpec and Minitest")
    (description "Given is an RSpec extension that allows the use of
Given/When/Then terminology when defining specifications, in a way similar to
the Cucumber Gherkin language.")
    (home-page "https://github.com/rspec-given/rspec-given")
    (license license:expat)))

(define-public ruby-rspec-its
  (package
    (name "ruby-rspec-its")
    (version "1.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rspec/rspec-its")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "02mlsc9d4d1cjj5vahi8v3q8hyn9fyiv8nnlidhgfh186qp20g1p"))))
    (build-system ruby-build-system)
    (arguments
     `(#:test-target "spec"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'dont-install-gems-from-gemfile
           (lambda _
             (substitute* "Gemfile"
               (("rspec rspec-core rspec-expectations rspec-mocks rspec-support")
                ""))
             #t))
         (add-before 'build 'loosen-ffi-requirement
           (lambda _
             ;; Accept any version of ruby-ffi.
             (substitute* "Gemfile"
               (("  gem 'ffi', '~> 1\\.9\\.25'")
                "  gem 'ffi'"))
             #t))
         (add-before 'build 'remove-unnecessary-dependency-versions-from-gemfile
           (lambda _
             (substitute* "rspec-its.gemspec"
               (("rake.*") "rake'\n")
               (("spec.add_development_dependency 'cucumber'.*")
                "spec.add_development_dependency 'cucumber'\n")
               (("bundler.*") "bundler'\n")
               (("\"aruba.*") "'aruba'\n"))
             #t)))))
    (propagated-inputs
     (list ruby-rspec-core ruby-rspec-expectations))
    (native-inputs
     (list bundler ruby-cucumber ruby-ffi ruby-aruba))
    (synopsis "RSpec extension that provides the @code{its} method")
    (description
     "RSpec::Its provides the its method as a short-hand to specify the expected
value of an attribute.  For example, one can use @code{its(:size)\\{should
eq(1)\\}}.")
    (home-page "https://github.com/rspec/rspec-its")
    (license license:expat)))

;;; This variant is used to break a cycle with ruby-protobuf.
(define-public ruby-rspec-its-minimal
  (hidden-package
   (package
     (inherit ruby-rspec-its)
     (arguments
      (substitute-keyword-arguments (package-arguments ruby-rspec-its)
        ((#:tests? _ #f) #f)))
     (native-inputs '()))))

(define-public ruby-rspec-mocks
  (package
    (name "ruby-rspec-mocks")
    (version "3.12.4")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "rspec-mocks" version))
              (sha256
               (base32
                "1dcfh85m3ksir6n8gydsal4d85chpww1b2nahb05nl8xhgh0r2ij"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ; avoid dependency cycles
    (propagated-inputs
     (list ruby-rspec-support ruby-diff-lcs))
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

(define-public ruby-rspec-block-is-expected
  (package
    (name "ruby-rspec-block-is-expected")
    (version "1.0.5")
    (source (origin
              (method git-fetch)        ;for tests
              (uri (git-reference
                    (url "https://github.com/pboling/rspec-block_is_expected")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1zi5z12lkw3fiwgr7g61845wj73asr2vzw4zsjv45klnnfspwass"))))
    (build-system ruby-build-system)
    (arguments
     (list #:phases #~(modify-phases %standard-phases
                        (add-after 'unpack 'relax-requirements
                          (lambda _
                            (substitute* "Rakefile"
                              (("require 'rubocop/rake_task'") "")
                              (("RuboCop::RakeTask.new") ""))
                            ;; Contains extraneous requirements not actually
                            ;; needed for the test suite.
                            (delete-file "Gemfile")))
                        (add-before 'build 'drop-signing-key-requirement
                          (lambda _
                            (substitute* "rspec-block_is_expected.gemspec"
                              (("spec.signing_key =.*")
                               "spec.signing_key = nil")))))))
    (native-inputs (list ruby-rspec-pending-for ruby-rspec-expectations))
    (propagated-inputs (list ruby-rspec-core))
    (synopsis "Simplify testing of blocks in RSpec")
    (description "This RSpec plugin allows you to use @code{block_is_expected}
similarly to how you would use @code{is_expected} if a block was wrapping the
subject.")
    (home-page "https://github.com/pboling/rspec-block_is_expected")
    (license license:expat)))

(define-public ruby-rspec-pending-for
  (package
    (name "ruby-rspec-pending-for")
    (version "0.1.16")
    (source (origin
              (method git-fetch)        ;for tests
              (uri (git-reference
                    (url "https://github.com/pboling/rspec-pending_for")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "107l560vm0kx25w5iy5rsk9ly8dxzr81b31284j514f4hkd0qv3m"))))
    (build-system ruby-build-system)
    (native-inputs (list ruby-rspec ruby-simplecov))
    (propagated-inputs (list ruby-rspec-core ruby-ruby-engine
                             ruby-ruby-version))
    (synopsis "Skip RSpec tests for specific Ruby engines or versions")
    (description "This RSpec plugin makes it easy to mark test cases as
pending or skipped for a specific Ruby engine (e.g. MRI or JRuby) or version
combinations.")
    (home-page "https://github.com/pboling/rspec-pending_for")
    (license license:expat)))

(define-public ruby-rspec-rerun
  (package
    (name "ruby-rspec-rerun")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "rspec-rerun" version))
       (sha256
        (base32
         "1gy7znkcaqhpccfnk2nvaqbsvgxy3q57cmjwkl9fi1zabaq5lbkj"))))
    (build-system ruby-build-system)
    (arguments
     '(;; No included tests
       #:tests? #f))
    (propagated-inputs (list ruby-rspec))
    (synopsis "Track failed RSpec tests to re-run them")
    (description
     "This package provides an automated way to track, and then re-run failed
RSpec tests.")
    (home-page "https://github.com/dblock/rspec-rerun")
    (license license:expat)))

(define-public ruby-rspec-stubbed-env
  ;; There is no release nor tag (see:
  ;; https://github.com/pboling/rspec-stubbed_env/issues/7).
  (let ((revision "0")
        (commit "9d767dec77a6d130f6ad83c48a00a5c81b14b9fa"))
    (package
      (name "ruby-rspec-stubbed-env")
      (version (git-version "1.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/pboling/rspec-stubbed_env")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1dy4m04h613dp0s59rknjd6h4lqs1h17mffc5kd8kh046mk8nr1p"))))
      (build-system ruby-build-system)
      (arguments
       (list #:test-target "spec"
             #:phases #~(modify-phases %standard-phases
                          (add-after 'unpack 'streamline-requirements
                            (lambda _
                              ;; Remove extraneous development dependencies.
                              (substitute* "rspec-stubbed_env.gemspec"
                                ((".*bundler.*") "")
                                ((".*rubocop.*") "")))))))
      (native-inputs (list ruby-simplecov))
      (propagated-inputs (list ruby-rspec))
      (synopsis "RSpec plugin to stub environment variables")
      (description
       "This RSpec plugin can be used to stub environment variables in a scoped
context for testing.")
      (home-page "https://github.com/pboling/rspec-stubbed_env")
      (license license:expat))))

(define-public ruby-rspec-wait
  (package
    (name "ruby-rspec-wait")
    (version "0.0.9")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "rspec-wait" version))
        (sha256
         (base32
          "0gvj1bp5ccx001dyvcgk2j49s5sl6vs9fdaqqb08z3bd1554hsww"))))
    (build-system ruby-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (invoke "rake" "spec"))))))
    (native-inputs
     (list bundler))
    (propagated-inputs
     (list ruby-rspec))
    (home-page "https://github.com/laserlemon/rspec-wait")
    (synopsis "Wait for conditions in RSpec")
    (description
     "RSpec::Wait strives to make it easier to test asynchronous or slow
interactions.")
    (license license:expat)))

(define-public ruby-rspec
  (package
    (name "ruby-rspec")
    (version "3.12.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "rspec" version))
              (sha256
               (base32
                "171rc90vcgjl8p1bdrqa92ymrj8a87qf6w20x05xq29mljcigi6c"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ; avoid dependency cycles
    (propagated-inputs
     (list ruby-rspec-core ruby-rspec-mocks ruby-rspec-expectations))
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

(define-public ruby-rspec-debug
  (package
    (name "ruby-rspec-debug")
    (version "0.2.0")
    (source (origin
              (method git-fetch)        ;for tests
              (uri (git-reference
                    (url "https://github.com/ko1/rspec-debug")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "125p1zxjznkk765nyqvkksw8x1nbm7xk4sjc1wza2fyp5hvyiddn"))))
    (build-system ruby-build-system)
    (arguments
     (list #:test-target "spec"
           #:phases #~(modify-phases %standard-phases
                        (add-after 'extract-gemspec 'relax-dependencies
                          (lambda _
                            (substitute* "Gemfile"
                              (("~>") ">=")))))))
    (native-inputs (list ruby-rspec))
    (propagated-inputs (list ruby-debug))
    (synopsis "Invoke Ruby debugger when spec fails")
    (description "This package can be used to have the execution stopped for
inspection in the Ruby debugger upon encountering a failure.  To use it, set
the @env{RSPEC_DEBUG} environment variable to @samp{true} then invoke the
@command{rspec} command as usual.")
    (home-page "https://github.com/ko1/rspec-debug")
    (license license:expat)))

(define-public ruby-specinfra
  (package
    (name "ruby-specinfra")
    (version "2.88.1")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "specinfra" version))
              (sha256
               (base32
                "07lap3sknncffpq9jw1x1mn9c5xxd058wxs5vnyz1y0lawdjfnsf"))))
    (build-system ruby-build-system)
    (propagated-inputs (list ruby-net-scp ruby-net-ssh ruby-net-telnet
                             ruby-sfl))
    (arguments
     (list
      #:test-target "spec"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'extract-gemspec 'relax-dependencies
            (lambda _
              (substitute* "specinfra.gemspec"
                (("%q<net-telnet>.freeze, \\[.*\\]")
                 "%q<net-telnet>.freeze, [\">= 0\"]")))))))
    (synopsis "Common layer for serverspec and itamae")
    (description "This Gem provides a common layer for serverspec and
itamae.")
    (home-page "https://github.com/mizzy/specinfra")
    (license license:expat)))

(define-public ruby-serverspec
  (package
    (name "ruby-serverspec")
    (version "2.42.3")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "serverspec" version))
              (sha256
               (base32
                "0kfaqrqynly8n3dy5qrbjvx4lx6mk9a5vynwb7xwqj8bixm0mab4"))))
    (build-system ruby-build-system)
    (propagated-inputs (list ruby-multi-json ruby-rspec ruby-rspec-its
                             ruby-specinfra))
    (arguments
     (list #:test-target "spec"))
    (synopsis
     "RSpec tests for servers configured by Puppet, Chef, Itamae, etc")
    (description
     "With Serverspec, you can write RSpec tests for checking your servers are
configured correctly.

Serverspec tests your servers’ actual state by executing command locally, via
SSH, via WinRM, via Docker API and so on.  So you don’t need to install any
agent softwares on your servers and can use any configuration management
tools, Puppet, Ansible, CFEngine, Itamae and so on.

But the true aim of Serverspec is to help refactoring infrastructure code.")
    (home-page "https://serverspec.org/")
    (license license:expat)))

;; Bundler is yet another source of circular dependencies, so we must disable
;; its test suite as well.
(define-public bundler
  (package
    (name "bundler")
    (version "2.4.18")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "bundler" version))
              (sha256
               (base32
                "03ppd60cbwzlrhsidi7frj826ssmxzwd954ikjk7966l45qx5xxn"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ; avoid dependency cycles
    (synopsis "Ruby gem bundler")
    (description "Bundler automatically downloads and installs a list of gems
specified in a \"Gemfile\", as well as their dependencies.")
    (home-page "https://bundler.io/")
    (license license:expat)))

(define-public ruby-builder
  (package
    (name "ruby-builder")
    (version "3.2.4")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "builder" version))
              (sha256
               (base32
                "045wzckxpwcqzrjr353cxnyaxgf0qg22jh00dcx7z38cys5g1jlr"))))
    (build-system ruby-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch
            (lambda _
              (substitute* "rakelib/tags.rake"
                (("File\\.exists\\?") "File.exist?"))

              ;; TODO This test is broken
              ;; https://github.com/tenderlove/builder/issues/13
              (substitute* "test/test_blankslate.rb"
                (("test_late_included_module_in_kernel_is_ok")
                 "test_late_included_module_in_kernel_is_ok
    skip(\"test expected to fail\")
"))
              (substitute* "rakelib/tags.rake"
                (("RVM_GEMDIR = .*") "RVM_GEMDIR = 'no-rvm-please'\n")))))))
    (synopsis "Ruby library to create structured data")
    (description "Builder provides a number of builder objects that make it
easy to create structured data.  Currently the following builder objects are
supported: XML Markup and XML Events.")
    (home-page "https://github.com/tenderlove/builder")
    (license license:expat)))

(define-public ruby-bump
  (package
    (name "ruby-bump")
    (version "0.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "bump" version))
       (sha256
        (base32
         "1xinbr9rzh6cj75x24niwgqcnbhdxc68a8bc41lk8xv6fd906fym"))))
    (build-system ruby-build-system)
    (arguments
     '(;; No included tests
       #:tests? #f))
    (synopsis "Tool for working with Rubygems")
    (description
     "Bump provides commands to manage Rubygem versioning, updating to the
next patch version for example.")
    (home-page "https://github.com/gregorym/bump")
    (license license:expat)))

(define-public ruby-rjb
  (package
    (name "ruby-rjb")
    (version "1.6.7")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "rjb" version))
              (sha256
               (base32
                "0ck802bm8cklhmqsgzhsa0y8lg80qy52dp3m8rlld3zc5gv1rsb9"))))
    (build-system ruby-build-system)
    (arguments
     (list
      #:tests? #f ; no rakefile
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'set-java-home
            (lambda* (#:key inputs #:allow-other-keys)
              (setenv "JAVA_HOME" (assoc-ref inputs "jdk")))))))
    (native-inputs
     `(("jdk" ,icedtea "jdk")))
    (synopsis "Ruby-to-Java bridge using the Java Native Interface")
    (description "RJB is a bridge program that connects Ruby and Java via the
Java Native Interface.")
    (home-page "https://www.artonx.org/collabo/backyard/?RubyJavaBridge")
    (license license:lgpl2.1+)))

(define-public ruby-log4r
  (package
    (name "ruby-log4r")
    (version "1.1.10")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "log4r" version))
        (sha256
          (base32
            "0ri90q0frfmigkirqv5ihyrj59xm8pq5zcmf156cbdv4r4l2jicv"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ; no Rakefile in gem
    (synopsis "Flexible logging library for Ruby")
    (description "Comprehensive and flexible logging library written
in Ruby for use in Ruby programs.  It features a hierarchical logging
system of any number of levels, custom level names, logger
inheritance, multiple output destinations per log event, execution
tracing, custom formatting, thread safteyness, XML and YAML
configuration, and more.")
     (home-page "http://log4r.rubyforge.org/")
     (license license:bsd-3)))

(define-public ruby-atoulme-antwrap
  (package
    (name "ruby-atoulme-antwrap")
    (version "0.7.5")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "atoulme-Antwrap" version))
              (sha256
               (base32
                "05s3iw44lqa81f8nfy5f0xjj808600h82zb9bsh46b9kcq2w2kmz"))))
    (build-system ruby-build-system)
    ;; Test data required for most of the tests are not included.
    (arguments `(#:tests? #f))
    (native-inputs
     (list ruby-hoe))
    (inputs
     (list ruby-rjb))
    (synopsis "Ruby wrapper for the Ant build tool")
    (description "Antwrap is a Ruby module that wraps the Apache Ant build
tool.  Antwrap can be used to invoke Ant tasks from a Ruby or a JRuby
script.")
    (home-page "http://rubyforge.org/projects/antwrap/")
    (license license:expat)))

(define-public ruby-atoulme-saikuro
  (package
    (name "ruby-atoulme-saikuro")
    (version "1.2.1")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "atoulme-Saikuro" version))
              (sha256
               (base32
                "0kvd2nsxffbza61d3q4j94wrbnbv50r1zy3a7q26f6k706fw1f19"))))
    (build-system ruby-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-module-resolution
            (lambda _
              (substitute* "lib/saikuro.rb"
                ;; irb 1.2.0 or later doesn't have RubyToken
                (("require 'irb/ruby-lex'")
                 "require 'rubygems'\ngem 'irb', '=1.1.1'\nrequire 'irb/ruby-lex'"))))
          (delete 'check)
          (add-after 'install 'check
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (saikuro (string-append out "/bin/saikuro")))
                (setenv "GEM_PATH" (string-append
                                    (getenv "GEM_PATH") ":"
                                    #$output "/lib/ruby/vendor_ruby"))
                (invoke saikuro "--cyclo" "--token" "--input_directory" "tests")))))))
    (propagated-inputs (list ruby-irb-1.1.1
                             ruby-e2mmap)) ;required by rubygems
    (synopsis "Cyclomatic complexity analyzer")
    (description "Saikuro is a Ruby cyclomatic complexity analyzer.  When
given Ruby source code Saikuro will generate a report listing the cyclomatic
complexity of each method found.  In addition, Saikuro counts the number of
lines per method and can generate a listing of the number of tokens on each
line of code.")
    (home-page "http://www.github.com/atoulme/Saikuro")
    ;; File headers contain the BSD-3 license and the README.rdoc says that
    ;; "Saikuro uses the BSD license", but the LICENSE file contains the text
    ;; of the Expat license.
    (license license:bsd-3)))

(define-public ruby-awesome-print
  (package
    (name "ruby-awesome-print")
    (version "1.9.2")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "awesome_print" version))
       (sha256
        (base32
         "0vkq6c8y2jvaw03ynds5vjzl1v9wg608cimkd3bidzxc0jvk56z9"))))
    (build-system ruby-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda _
              ;; Remove failing test.
              (for-each delete-file
                        '("spec/ext/nokogiri_spec.rb"
                          "spec/colors_spec.rb"
                          "spec/formats_spec.rb"
                          "spec/methods_spec.rb"
                          "spec/misc_spec.rb"
                          "spec/objects_spec.rb"))
              (invoke "rspec" "-c" "spec"))))))
    (native-inputs
     (list ruby-nokogiri ruby-rspec ruby-simplecov))
    (synopsis "Pretty print Ruby objects to visualize their structure")
    (description
     "Ruby dubugging companion: pretty print Ruby objects to visualize their
structure.  Supports custom object formatting via plugins.")
    (home-page "https://github.com/awesome-print/awesome_print")
    (license license:expat)))

(define-public ruby-pandoc-ruby
  (package
    (name "ruby-pandoc-ruby")
    (version "2.1.4")
    (source
     (origin
       (method git-fetch)               ;the gem lacks many test files
       (uri (git-reference
             (url "https://github.com/xwmx/pandoc-ruby")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "03a11clhycyn0jhc7g9davpqd83sn60jqwjy1y145ag9sq6sp935"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f ; Disable tests since they depend on pandoc behavior
                   ; and there are no upstream releases.
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-pandoc-path
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((pandoc (search-input-file inputs "/bin/pandoc")))
               (substitute* "lib/pandoc-ruby.rb"
                 (("@@pandoc_path = 'pandoc'")
                  (format #f "@@pandoc_path = '~a'" pandoc)))
               (substitute* "test/test_pandoc_ruby.rb"
                 (("('|\")pandoc" _ quote)
                  (string-append quote pandoc))
                 (("\\^pandoc")
                  ".*pandoc")))))
         (add-after 'unpack 'adjust-tests
           ;; The tests expect filenames with spaces.  Because they don't have
           ;; spaces the quotes around the output are dropped automatically.
           (lambda _
             (substitute* "test/test_pandoc_ruby.rb"
               (("\\\\\"#\\{file\\.path\\}\\\\\"") "#{file.path}"))))
         (add-after 'extract-gemspec 'remove-Gemfile.lock
           (lambda _
             (delete-file "Gemfile.lock")
             (substitute* "pandoc-ruby.gemspec"
               (("Gemfile\\.lock") "")))))))
    (native-inputs
     (list ruby-mocha))
    (inputs
     (list pandoc))
    (synopsis "Ruby wrapper for Pandoc")
    (description "PandocRuby is a wrapper for Pandoc, a Haskell library with
command line tools for converting one markup format to another.  Pandoc can
convert documents from a variety of formats including markdown,
reStructuredText, textile, HTML, DocBook, LaTeX, and MediaWiki markup to a
variety of other formats, including markdown, reStructuredText, HTML, LaTeX,
ConTeXt, PDF, RTF, DocBook XML, OpenDocument XML, ODT, GNU Texinfo, MediaWiki
markup, groff man pages, HTML slide shows, EPUB, Microsoft Word docx, and
more.")
    (home-page "https://github.com/xwmx/pandoc-ruby")
    (license license:expat)))

(define-public ruby-patron
  (package
    (name "ruby-patron")
    (version "0.13.3")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "patron" version))
       (sha256
        (base32
         "0523gddx88zql2mq6655k60gy2ac8vybpzkcf90lmd9nx7wl3fi9"))))
    (build-system ruby-build-system)
    (inputs
     (list curl))
    (arguments
     `(#:tests? #f))                    ; no included tests
    (synopsis "Ruby HTTP client library based on @code{libcurl}")
    (description
     "Patron is a Ruby HTTP client library based on @code{libcurl}.  It does
not try to expose the full power (read complexity) of @code{libcurl} but
instead tries to provide a sane API while taking advantage of @code{libcurl}
under the hood.")
    (home-page "https://github.com/toland/patron")
    (license license:expat)))

(define-public ruby-slim
  (package
    (name "ruby-slim")
    (version "5.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "slim" version))
       (sha256
        (base32
         "1rp437r8hr9kdgabb7c96yw4z2wyrajl4cxiij038y10f8i6hbn4"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; See: https://github.com/slim-template/slim/issues/857 and
         ;; https://github.com/slim-template/slim/issues/858.
         (add-after 'unpack 'skip-broken-tests
           (lambda _
             (substitute* "test/core/test_embedded_engines.rb"
               (("def test_render_with_markdown")
                "def skipped_test_render_with_markdown"))
             (substitute* "test/translator/test_translator.rb"
               (("raise (\"Missing test for.*)" _ tail)
                (string-append "print " tail)))))
         ;; See: https://salsa.debian.org/ruby-team/ruby-slim/-/commit/
         ;; 824862bd99d1675bc699d8fc71ba965a785c1f44.
         (add-after 'unpack 'prevent-bundler-interference
           (lambda _
             (substitute* "Rakefile"
               (("require 'bundler/setup'") "nil")
               (("Bundler::GemHelper\\.install_tasks") "nil")))))))
    (native-inputs
     (list ruby-rack-test ruby-rspec-core ruby-sinatra))
    (propagated-inputs
     (list ruby-temple ruby-tilt))
    (synopsis "Minimalist template language for Ruby")
    (description "Slim is a template language for Ruby that aims to reduce the
syntax to the minimum while remaining clear.")
    (home-page "http://slim-lang.com/")
    (license license:expat)))

(define-public ruby-asciidoctor
  (package
    (name "ruby-asciidoctor")
    (version "2.0.18")
    (source
     (origin
       (method git-fetch)               ;the gem release lacks a Rakefile
       (uri (git-reference
             (url "https://github.com/asciidoctor/asciidoctor")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1mpk3y69lqz9ywfkjmr40dm3mkabrnf92bb011qq1axj73yyrajv"))))
    (build-system ruby-build-system)
    (arguments
     (list
      #:test-target "test:all"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'extract-gemspec 'strip-version-requirements
            (lambda _
              (delete-file "Gemfile")
              (substitute* "asciidoctor.gemspec"
                (("(.*add_.*dependency '[_A-Za-z0-9-]+').*" _ stripped)
                 (string-append stripped "\n")))))
          (add-after 'install 'install-man-page
            (lambda* (#:key outputs #:allow-other-keys)
              (install-file (search-input-file
                             outputs (string-append "lib/ruby/vendor_ruby/"
                                                    "gems/asciidoctor-"
                                                    #$version
                                                    "/man/asciidoctor.1"))
                            (string-append #$output "/share/man/man1")))))))
    (native-inputs
     (list ruby-asciimath
           ruby-coderay
           ruby-cucumber
           ruby-erubis
           ruby-haml
           ruby-minitest
           ruby-nokogiri
           ruby-open-uri-cached
           ruby-rouge
           ruby-rspec-expectations
           ruby-simplecov
           ruby-slim
           ruby-tilt
           ruby-erubi))
    (synopsis "Converter from AsciiDoc content to other formats")
    (description "Asciidoctor is a text processor and publishing toolchain for
converting AsciiDoc content to HTML5, DocBook 5, PDF, and other formats.")
    (home-page "https://asciidoctor.org")
    (license license:expat)))

(define-public ruby-asciidoctor/minimal
  (hidden-package
   (package
     (inherit ruby-asciidoctor)
     (arguments
      (ensure-keyword-arguments
       (package-arguments ruby-asciidoctor)
       (list #:tests? #f)))
     (native-inputs '()))))

(define-public ruby-asciidoctor-multipage
  (package
    (name "ruby-asciidoctor-multipage")
    (version "0.0.16")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/owenh000/asciidoctor-multipage")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0rnz7qxdw5qbi3qjplihhk468kv690njdi06yllgylc75k62ar1p"))))
    (propagated-inputs (list ruby-asciidoctor ruby-slim))
    (build-system ruby-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-after 'extract-gemspec 'strip-version-requirements
                    (lambda _
                      (delete-file "Gemfile")
                      (substitute* "asciidoctor-multipage.gemspec"
                        (("(.*add_.*dependency '[_A-Za-z0-9-]+').*" _ stripped)
                         (string-append stripped "\n"))))))))
    (synopsis
     "Asciidoctor extension for generating HTML output using multiple pages")
    (description
     "Asciidoctor generates single-page documents.  This extension
splits documents up into multiple HTML pages according to their headings, with
configurable levels.")
    (license license:expat)
    (home-page "https://github.com/owenh000/asciidoctor-multipage")))

(define-public ruby-prawn-icon
  (package
    (name "ruby-prawn-icon")
    (version "3.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "prawn-icon" version))
       (sha256
        (base32
         "049k42bqy4iq9hddf7jah83b6qr8ka63w1d63illh1mf4f4dihdk"))))
    (build-system ruby-build-system)
    (arguments
     `(#:test-target "spec"
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'remove-unnecessary-dependencies
                    (lambda _
                      (substitute* '("Rakefile" "spec/spec_helper.rb")
                        ((".*[Bb]undler.*") "")
                        (("^require 'rubocop.*") "")
                        (("^RuboCop.*") "")))))))
    (native-inputs
     (list ruby-pdf-inspector ruby-pdf-reader ruby-rspec ruby-simplecov))
    (propagated-inputs
     (list ruby-prawn))
    (synopsis "Icon fonts for use with the Prawn PDF toolkit")
    (description "@code{Prawn::Icon} provides various icon fonts including
FontAwesome, PaymentFont and Foundation Icons for use with the Prawn PDF
toolkit.")
    (home-page "https://github.com/jessedoyle/prawn-icon/")
    (license %prawn-project-licenses)))

(define-public ruby-css-parser
  (package
    (name "ruby-css-parser")
    (version "1.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "css_parser" version))
       (sha256
        (base32
         "04c4dl8cm5rjr50k9qa6yl9r05fk9zcb1zxh0y0cdahxlsgcydfw"))))
    (build-system ruby-build-system)
    (arguments `(#:tests? #f))          ;gem doesn't ship with test suite
    (propagated-inputs
     (list ruby-addressable))
    (synopsis "Ruby Cascading Style Sheets (CSS) parser")
    (description "This package allows loading, parsing and cascading Cascading
Style Sheets (CSS) rule sets in Ruby.")
    (home-page "https://github.com/premailer/css_parser")
    (license license:expat)))

(define-public ruby-prism
  (package
    (name "ruby-prism")
    (version "1.3.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/ruby/prism.git")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "03bs2gbackc3c3k4p979l2p9v215jb1m5h7b44n6yzh18kaimc85"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda _
             (setenv "CC" ,(cc-for-target))
             (invoke "rake" "compile")))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (setenv "GEM_HOME" (string-append (assoc-ref outputs "out")
                                               "/lib/ruby/vendor_ruby"))
             (invoke "rake" "install")
             ;; Make build reproducible.
             (for-each delete-file
                       (find-files (string-append (assoc-ref outputs "out")
                                                  "/lib/ruby/vendor_ruby")
                                   "gem_make.out$")))))))
    (native-inputs
     (list ruby-rake ruby-rake-compiler))
    (synopsis "Parser for Ruby source code")
    (description "This package provides a parser for Ruby source code,
written in C.")
    (home-page "https://ruby.github.io/prism/")
    (license license:expat)))

(define-public ruby-prawn-svg
  (package
    (name "ruby-prawn-svg")
    (version "0.32.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "prawn-svg" version))
       (sha256
        (base32
         "0mbxzw7r7hv43db9422flc24ib9d8bdy1nasbni2h998jc5a5lb6"))))
    (build-system ruby-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'do-not-use-bundler
                 (lambda _
                   (substitute* "spec/spec_helper.rb"
                     ((".*[Bb]undler.*") ""))))
               (replace 'check
                 (lambda* (#:key tests? #:allow-other-keys)
                   (when tests?
                     ;; This test fails due to the recent rspec 3.12 used
                     ;; (see: https://github.com/mogest/prawn-svg/issues/151).
                     (delete-file "spec/prawn/svg/interface_spec.rb")
                     (invoke "rspec" "-Ilib" "-rprawn-svg")))))))
    (native-inputs (list ruby-rspec))
    (propagated-inputs (list ruby-css-parser ruby-prawn))
    (synopsis "SVG renderer for the Prawn PDF library")
    (description "This library allows rendering Scalable Vector Graphics (SVG)
graphics directly into a Portable Document Format (PDF) document using the
Prawn module.")
    (home-page "https://github.com/mogest/prawn-svg")
    (license license:expat)))

(define-public ruby-prawn-templates
  (package
    (name "ruby-prawn-templates")
    (version "0.1.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/prawnpdf/prawn-templates")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0wll54wxxwixpwazfn4ffbqvqbfrl01cfsv8y11vnlzy7isx5xvl"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-after 'unpack 'do-not-use-bundler
                    (lambda _
                      (substitute* "spec/spec_helper.rb"
                        ((".*[Bb]undler.*") ""))
                      #t))
                  (replace 'check
                    (lambda* (#:key tests? #:allow-other-keys)
                      (when tests?
                        (invoke "rspec"))
                      #t)))))
    (native-inputs
     (list ruby-pdf-inspector ruby-rspec))
    (propagated-inputs
     (list ruby-pdf-reader ruby-prawn))
    (synopsis "Prawn extension to include or combine PDF documents")
    (description "This @strong{unmaintained} package provides a Prawn
extension that allows including other Portable Document Format (PDF) documents
as background or combining several PDF documents into one.  This functionality
used to be part of Prawn itself, but was extracted from Prawn 0.15.0 because
of its many longstanding issues.")
    (home-page "https://github.com/prawnpdf/prawn-templates")
    (license %prawn-project-licenses)))

(define-public ruby-polyglot
  (package
    (name "ruby-polyglot")
    (version "0.3.5")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "polyglot" version))
       (sha256
        (base32
         "1bqnxwyip623d8pr29rg6m8r0hdg08fpr2yb74f46rn1wgsnxmjr"))))
    (build-system ruby-build-system)
    (arguments `(#:tests? #f))           ;no test suite
    (synopsis "Augment @code{require} to load non-Ruby file types")
    (description "The Polyglot library allows a Ruby module to register a
loader for the file type associated with a filename extension, and it augments
@code{require} to find and load matching files.")
    (home-page "https://github.com/cjheath/polyglot")
    (license license:expat)))

(define-public ruby-treetop
  (package
    (name "ruby-treetop")
    (version "1.6.10")
    (source
     (origin
       (method git-fetch)               ;no test suite in distributed gem
       (uri (git-reference
             (url "https://github.com/cjheath/treetop")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1dmk94z6ivhrz5hsq68vl5vgydhkz89n394rha1ymddw3rymbfcv"))))
    (build-system ruby-build-system)
    (arguments
     `(#:test-target "spec"))
    (native-inputs
     (list ruby-activesupport ruby-rr ruby-rspec))
    (propagated-inputs
     (list ruby-polyglot))
    (synopsis "Ruby-based parsing DSL based on parsing expression grammars")
    (description "This package provides a Ruby-based Parsing Expression
Grammar (PEG) parser generator Domain Specific Language (DSL).")
    (home-page "https://github.com/cjheath/treetop")
    (license license:expat)))

(define-public ruby-typhoeus
  (package
    (name "ruby-typhoeus")
    (version "1.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "typhoeus" version))
       (sha256
        (base32
         "1m22yrkmbj81rzhlny81j427qdvz57yk5wbcf3km0nf3bl6qiygz"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))                    ; no included tests
    (propagated-inputs
     (list ruby-ethon))
    (synopsis "@code{libcurl} wrapper in Ruby")
    (description
     "Like a modern code version of the mythical beast with 100 serpent heads,
Typhoeus runs HTTP requests in parallel while cleanly encapsulating handling
logic.")
    (home-page "https://github.com/typhoeus/typhoeus")
    (license license:expat)))

;;; A minimal variant used to build ruby-rubocop itself.
(define ruby-rubocop-capybara-minimal
  (package
    (name "ruby-rubocop-capybara")
    (version "2.17.1")
    (source (origin
              (method git-fetch)        ;for tests
              (uri (git-reference
                    (url "https://github.com/rubocop/rubocop-capybara")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "137y21b6g0kj1001zp95gwchx2cvgz8pglw2ik1cw647lh77qdsp"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (synopsis "Capybara plugin for RuboCop")
    (description "This package provides a RuboCop plugin that can be used for
code style checking of Capybara test files (RSpec, Cucumber, Minitest).")
    (home-page "https://github.com/rubocop/rubocop-capybara")
    (license license:expat)))

(define-public ruby-rubocop-capybara
  (package
    (inherit ruby-rubocop-capybara-minimal)
    (arguments
     (list #:test-target "spec"
           #:phases #~(modify-phases %standard-phases
                        (add-after 'unpack 'relax-requirements
                          (lambda _
                            (substitute* "Gemfile"
                              (("gem 'rubocop-rspec', '~> 2.16.0'")
                               "gem 'rubocop-rspec', '>= 2.16.0'")))))))
    (native-inputs
     (list ruby-bump
           ruby-rack
           ruby-rake
           ruby-rspec
           ruby-rubocop
           ruby-rubocop-performance-minimal
           ruby-rubocop-rake-minimal
           ruby-rubocop-rspec-minimal
           ruby-simplecov
           ruby-yard))))

;;; A minimal variant used to build ruby-rubocop itself.
(define ruby-rubocop-rake-minimal
  (package
    (name "ruby-rubocop-rake")
    (version "0.6.0")
    (source (origin
              (method git-fetch)        ;for tests
              (uri (git-reference
                    (url "https://github.com/rubocop/rubocop-rake")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1r53szwglikbir1fvpz4i51p915khrrkl6rp61zcx3dcrclkr3ld"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))      ;avoid extra dependencies
    (synopsis "RuboCop plugin for Rake")
    (description "This package provides a RuboCop plugin for Rake.")
    (home-page "https://github.com/rubocop/rubocop-rake")
    (license license:expat)))

(define-public ruby-rubocop-rake
  (package
    (inherit ruby-rubocop-rake-minimal)
    (arguments
     (list #:test-target "spec"))
    (native-inputs
     (list ruby-rake
           ruby-rspec
           ruby-rubocop
           ruby-rubocop-rspec))
    (propagated-inputs
     (list ruby-rubocop))))

;;; A minimal variant used to build ruby-rubocop itself.
(define ruby-rubocop-rspec-minimal
  (package
    (name "ruby-rubocop-rspec")
    (version "2.19.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rubocop/rubocop-rspec")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0mgjyrzi8r44v3gb8xscdwspirz9kqkaf7zlsjhhlxr0di0rlj2r"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))      ;avoid extra dependencies
    (synopsis "Code style checking for RSpec files")
    (description "This package provides a plugin for the RuboCop code style
enforcing & linting tool.")
    (home-page "https://github.com/rubocop/rubocop-rspec")
    (license license:expat)))

(define-public ruby-rubocop-rspec
  (package
    (inherit ruby-rubocop-rspec-minimal)
    (arguments '(#:test-target "spec"))
    (native-inputs
     (list ruby-bump
           ruby-rack
           ruby-rspec
           ruby-rubocop-performance-minimal
           ruby-rubocop-rake-minimal
           ruby-simplecov
           ruby-yard))
    (propagated-inputs
     (list ruby-rubocop
           ruby-rubocop-ast
           ruby-rubocop-capybara))))

(define-public ruby-rubocop-packaging
  (package
    (name "ruby-rubocop-packaging")
    (version "0.5.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/utkarsh2102/rubocop-packaging")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "08jsfp42z0aj32002z2hz8vkmza0jvnrqk9rk2v0xb8qdxkgbx3l"))))
    (build-system ruby-build-system)
    (arguments
     (list #:test-target "spec"))
    (propagated-inputs
     (list ruby-rubocop))
    (native-inputs
     (list ruby-rspec
           ruby-yard
           ruby-bump))
    (synopsis
     "Collection of RuboCop checks for downstream compatibility issues")
    (description
     "This package provides a collection of RuboCop cops to check for
downstream compatibility issues in the Ruby code.")
    (home-page "https://github.com/utkarsh2102/rubocop-packaging")
    (license license:expat)))

(define-public ruby-rubocop-performance
  (package
    (name "ruby-rubocop-performance")
    (version "1.16.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/rubocop/rubocop-performance")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1axafki2mpdz38y7i0afmnxcan5wj54l8crp8pbs7h1cip7y4s49"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f  ; tests require a git checkout of rubocop's source code.
       #:phases
       (modify-phases %standard-phases
         (replace 'replace-git-ls-files
           (lambda _
             (substitute* "rubocop-performance.gemspec"
               (("`git ls-files -z config lib LICENSE.txt README.md`")
                "`find config lib LICENSE.txt README.md \
-type f -print0 |sort -z`"))))
         (add-before 'check 'set-HOME
           (lambda _
             (setenv "HOME" "/tmp"))))))
    (propagated-inputs
     (list ruby-rubocop ruby-rubocop-ast))
    (native-inputs
     (list ruby-bump ruby-yard))
    (synopsis "Performance optimizations checkers for Ruby code")
    (description "This package provides a collection of RuboCop cops to check
for performance optimizations in Ruby code.")
    (home-page "https://docs.rubocop.org/rubocop-performance/")
    (license license:expat)))

(define-public ruby-rubocop-performance-minimal
  (hidden-package
   (package
     (inherit ruby-rubocop-performance)
     (arguments
      (substitute-keyword-arguments (package-arguments ruby-rubocop-performance)
        ((#:tests? _ #f) #f)))
     (propagated-inputs '())
     (native-inputs '()))))

(define-public ruby-gimme
  (let ((revision "1")
        (commit "4e71f0236f1271871916dd403261d26533db34c0"))
    (package
      (name "ruby-gimme")
      (version (git-version "0.5.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/searls/gimme")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0hrd32ygvf3i7h47ak8f623cz8ns9q7g60nnnvvlnywbggjaz3h6"))))
      (build-system ruby-build-system)
      (native-inputs
       (list ruby-coveralls ruby-cucumber ruby-pry ruby-simplecov
             ruby-rspec-given))
      (arguments
       `(;; The cucumber task fails with error: "index 3 out of matches
         ;; (IndexError)", apparently due to our newer Cucumber version.
         ;; TODO: Try the "default" task with a future release.
         #:test-target "spec"
         #:phases
         (modify-phases %standard-phases
           (add-after 'extract-gemspec 'prepare-for-tests
             (lambda _
               ;; Delete failing tests (possibly due to our newer rspec
               ;; version).
               (delete-file "spec/gimme/gives_class_methods_spec.rb")
               (delete-file "spec/gimme/rspec_adapter_spec.rb")
               (delete-file "spec/gimme/verifies_class_methods_spec.rb")
               ;; Fix duplicate version requirements and de-register files.
               (delete-file "Gemfile")
               (delete-file "Gemfile.lock")
               (substitute* "gimme.gemspec"
                 ((".*\"Gemfile\".*") "")
                 ((".*\"Gemfile\\.lock\",.*") "")
                 ((".*(rspec|cucumber).*\">= 0\".*") "")
                 (("\"spec/gimme/gives_class_methods_spec.rb\",") "")
                 (("\"spec/gimme/rspec_adapter_spec.rb\",") "")
                 (("\"spec/gimme/verifies_class_methods_spec.rb\",") "")
                 ;; All of these gems relate to development, and are
                 ;; unnecessary when running the tests.
                 ((".*(add|gem).*guard-.*") "")
                 ((".*(add|gem).*jeweler.*") "")
                 ((".*(add|gem).*pry.*") "")
                 ((".*(add|gem).*growl.*") "")
                 ((".*(add|gem).*rb-fsevent.*") ""))
               #t)))))
      (synopsis "Lightweight test double library for Ruby")
      (description "Gimme is a very lightweight test double library for Ruby,
based on Mockito (a mocking framework for Java).  It is an opinionated (but
not noisy) means to facilitate test-driving by enabling the authors to specify
only what they care about.")
      (home-page "https://github.com/searls/gimme")
      (license license:expat))))

(define-public ruby-stud
  (package
    (name "ruby-stud")
    (version "0.0.23")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "stud" version))
              (sha256
               (base32
                "0qpb57cbpm9rwgsygqxifca0zma87drnlacv49cqs2n5iyi6z8kb"))))
    (build-system ruby-build-system)
    (native-inputs (list ruby-rspec))
    (arguments
     (list #:phases #~(modify-phases %standard-phases
                        ;; No Rakefile is included, so run rspec directly.
                        (replace 'check
                          (lambda* (#:key tests? #:allow-other-keys)
                            (when tests?
                              (invoke "rspec")))))))
    (synopsis "Retries, worker supervision, resource pools and more for Ruby")
    (description "The Stud Ruby library adds a few things missing from the
standard Ruby library such as:
@table @code
@item {Stud::Try}
Retry on failure, with back-off, where failure is any exception.
@item {Stud::Pool}
Generic resource pools.
@item {Stud::Task}
Tasks (threads that can return values, exceptions, etc.)
@item {Stud.interval}
Interval execution (do X every N seconds).
@item {Stud::Buffer}
Batch and flush behavior.
@end itemize")
    (home-page "https://github.com/jordansissel/ruby-stud")
    (license license:asl2.0)))

(define-public ruby-standard
  (package
    (name "ruby-standard")
    (version "1.25.3")
    (source
     (origin
       (method git-fetch)               ;no test suite in distributed gem
       (uri (git-reference
             (url "https://github.com/testdouble/standard")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0q42gv7wgrc818a5hm599sy07vjq69hbijzpkpgh6jws6x7wzyh3"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'set-HOME
           (lambda _
             ;; Some tests fail otherwise.
             (setenv "HOME" "/tmp")))
         (add-after 'unpack 'relax-requirements
           (lambda _
             (substitute* "standard.gemspec"
               (("\"rubocop\", \"1.44.1\"")
                "\"rubocop\", \">= 1.44.1\"")
               (("\"rubocop-performance\", \"1.15.2\"")
                "\"rubocop-performance\", \">= 1.15.2\""))))
         (add-after 'unpack 'delete-problematic-tests
           ;; These tests fail for unknown reasons (see:
           ;; https://github.com/testdouble/standard/issues/532).
           (lambda _
             (for-each
              delete-file
              '("test/standard_test.rb"
                "test/standard/cop/block_single_line_braces_test.rb")))))))
    (native-inputs
     (list ruby-gimme
           ruby-pry
           ruby-simplecov))
    (propagated-inputs
     (list ruby-language-server-protocol
           ruby-rubocop
           ruby-rubocop-performance))
    (synopsis "Ruby Style Guide, with linter & automatic code fixer")
    (description "Standard is a port of StandardJS.  Like StandardJS, it aims
to save time in the following ways:
@itemize
@item No configuration.
@item Automatically format code.
@item Catch style issues and programmer errors early.
@end itemize")
    (home-page "https://github.com/testdouble/standard")
    (license license:expat)))

(define-public ruby-chunky-png
  (package
    (name "ruby-chunky-png")
    (version "1.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/wvanbergen/chunky_png")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "05qwj72dy2fcy0n2jnf3bfriybfj36m7s6pv9xash6295dbcp901"))))
    (build-system ruby-build-system)
    (arguments
     (list
      #:test-target "spec"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'disable-bundler
            (lambda _
              (substitute* (find-files "." "\\.rb$")
                (("require.*bundler/setup.*") "")))))))
    (native-inputs
     (list bundler ruby-rspec ruby-standard ruby-yard))
    (synopsis "Ruby library to handle PNG images")
    (description "ChunkyPNG is a pure Ruby library that can read and write
Portable Network Graphics (PNG) images without depending on an external image
library.  It tries to be memory efficient and reasonably fast.  It has
features such as:
@itemize
@item
Decoding support for any image that the PNG standard allows.  This includes all
standard color modes, all bit depths, all transparency, and interlacing and
filtering options.
@item
Encoding support for images of all color modes (true color, grayscale, and
indexed) and transparency for all these color modes.  The best color mode is
chosen automatically, based on the amount of used colors.
@item Read/write access to the image's pixels.
@item Read/write access to all image metadata that is stored in chunks.
@item
Memory efficiency: @code{fixnum} are used, i.e. 4 or 8 bytes of memory per
pixel, depending on the hardware).
@item
Performance: ChunkyPNG is reasonably fast for Ruby standards, by only using
integer math and a highly optimized saving routine.
@item Interoperability with RMagick.
@end itemize

ChunkyPNG is vulnerable to decompression bombs and can run out of memory when
loading a specifically crafted PNG file.  This is hard to fix in pure Ruby.
Deal with untrusted images in a separate process, e.g., by using @code{fork}
or a background processing library.")
    (home-page "https://github.com/wvanbergen/chunky_png/wiki")
    (license license:expat)))

(define-public ruby-text-hyphen
  (package
    (name "ruby-text-hyphen")
    (version "1.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "text-hyphen" version))
        (sha256
          (base32
            "01js0wxz84cc5hzxgqbcqnsa0y6crhdi6plmgkzyfm55p0rlajn4"))))
    (build-system ruby-build-system)
    (native-inputs
     (list ruby-hoe))
    (synopsis "Ruby library to hyphenate words in various languages")
    (description "Text::Hyphen is a Ruby library to hyphenate words in various
languages using Ruby-fied versions of TeX hyphenation patterns.  It will
properly hyphenate various words according to the rules of the language the
word is written in.  The algorithm is based on that of the TeX typesetting
system by Donald E.  Knuth.")
    (home-page "https://github.com/halostatue/text-hyphen")
    ;; The whole is licensed under the Expat license, but parts use various
    ;; versions of the LaTeX Project Public License.
    (license license:expat)))

(define-public ruby-open-uri-cached
  (package
    (name "ruby-open-uri-cached")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "open-uri-cached" version))
       (sha256
        (base32
         "03v0if3jlvbclnd6jgjk94fbhf0h2fq1wxr0mbx7018sxzm0biwr"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))                    ;no test suite
    (synopsis "OpenURI with transparent disk caching")
    (description "OpenURI with transparent disk caching, which is
useful to avoid making excessive queries, for example when scraping
web pages.")
    (home-page "https://github.com/tigris/open-uri-cached")
    (license license:expat)))

(define-public ruby-asciidoctor-pdf
  (package
    (name "ruby-asciidoctor-pdf")
    (version "2.3.4")
    (source
     (origin
       (method git-fetch)               ;no test suite in the distributed gem
       (uri (git-reference
             (url "https://github.com/asciidoctor/asciidoctor-pdf")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "07krhpj2ylz7h7hy8vg0js8yv828qxh3mkhx0bsrfh0p24xwbjrm"))))
    (build-system ruby-build-system)
    (arguments
     (list
      #:test-target "spec"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'extract-gemspec 'strip-version-requirements
            (lambda _
              (substitute* "asciidoctor-pdf.gemspec"
                (("(.*add_.*dependency '[_A-Za-z0-9-]+').*" _ stripped)
                 (string-append stripped "\n")))))
          ;; The tests rely on the Gem being installed, so move the check
          ;; phase after the install phase.
          (delete 'check)
          (add-after 'install 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (setenv "GEM_PATH" (string-append
                                  (getenv "GEM_PATH") ":"
                                  #$output "/lib/ruby/vendor_ruby"))
              (when tests?
                (invoke "rspec" "-t" "~visual" "-t" "~cli"
                        "-t" "~network")))))))
    (native-inputs
     (list ruby-chunky-png
           ruby-coderay
           ruby-pdf-inspector
           ruby-rouge
           ruby-rspec))
    (propagated-inputs
     (list ruby-asciidoctor
           ruby-concurrent
           ruby-open-uri-cached
           ruby-prawn
           ruby-prawn-icon
           ruby-prawn-svg
           ruby-prawn-table
           ruby-prawn-templates
           ruby-safe-yaml
           ruby-text-hyphen
           ruby-thread-safe
           ruby-treetop
           ruby-ttfunk))
    (synopsis"AsciiDoc to Portable Document Format (PDF)} converter")
    (description "Asciidoctor PDF is an extension for Asciidoctor that
converts AsciiDoc documents to Portable Document Format (PDF) using the Prawn
PDF library.  It has features such as:
@itemize
@item Direct AsciiDoc to PDF conversion
@item Configuration-driven theme (style and layout)
@item Scalable Vector Graphics (SVG) support
@item PDF document outline (i.e., bookmarks)
@item Table of contents page(s)
@item Document metadata (title, authors, subject, keywords, etc.)
@item Internal cross reference links
@item Syntax highlighting with Rouge, Pygments, or CodeRay
@item Page numbering
@item Customizable running content (header and footer)
@item
“Keep together” blocks (i.e., page breaks avoided in certain block content)
@item Orphaned section titles avoided
@item Autofit verbatim blocks (as permitted by base_font_size_min setting)
@item Table border settings honored
@item Font-based icons
@item Custom TrueType (TTF) fonts
@item Double-sided printing mode (margins alternate on recto and verso pages)
@end itemize")
    (home-page "https://asciidoctor.org/docs/asciidoctor-pdf")
    (license license:expat)))

(define-public ruby-ast
  (package
    (name "ruby-ast")
    (version "2.4.2")
    (source
     (origin
       (method git-fetch)               ;no test included in gem from v2.4.1
       (uri (git-reference
             (url "https://github.com/whitequark/ast")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0vm94yml8rknr7z034vg6s3fpx6lml2prz9fn3hr67cx0143bb4h"))))
    (build-system ruby-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-coveralls-requirement
           (lambda _
             (substitute* "test/helper.rb"
               (("require 'coveralls'") "")
               (("Coveralls::SimpleCov::Formatter") ""))
             #t))
         (add-after 'extract-gemspec 'remove-unnecessary-requirements
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "ast.gemspec"
               ((".*coveralls.*") "\n")
               (("%q<rest-client>.*") "%q<rest-client>.freeze, [\">= 0\"])\n")
               (("%q<mime-types>.*") "%q<mime-types>.freeze, [\">= 0\"])\n")
               (("%q<rake>.*") "%q<rake>.freeze, [\">= 0\"])\n")
               (("12\\.3") "13.0"))
             #t)))))
    (native-inputs
     (list bundler
           ruby-bacon
           ruby-bacon-colored-output
           ruby-json-pure
           ruby-kramdown
           ruby-mime-types
           ruby-racc
           ruby-rest-client
           ruby-simplecov
           ruby-yard))
    (synopsis "Library for working with Abstract Syntax Trees")
    (description
     "@code{ast} is a Ruby library for working with Abstract Syntax Trees.
It does this through immutable data structures.")
    (home-page "https://whitequark.github.io/ast/")
    (license license:expat)))

(define-public ruby-sporkmonger-rack-mount
  ;; Testing the addressable gem requires a newer commit than that released, so
  ;; use an up to date version.
  (let ((revision "1")
        (commit "076aa2c47d9a4c081f1e9bcb56a826a9e72bd5c3"))
    (package
      (name "ruby-sporkmonger-rack-mount")
      (version (git-version "0.8.3" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/sporkmonger/rack-mount")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1scx273g3xd93424x9lxc4zyvcp2niknbw5mkz6wkivpf7xsyxdq"))))
      (build-system ruby-build-system)
      (arguments
       ;; Tests currently fail so disable them.
       ;; https://github.com/sporkmonger/rack-mount/pull/1
       `(#:tests? #f))
      (propagated-inputs (list ruby-rack))
      (synopsis "Stackable dynamic tree based Rack router")
      (description
       "@code{Rack::Mount} supports Rack's @code{X-Cascade} convention to
continue trying routes if the response returns pass.  This allows multiple
routes to be nested or stacked on top of each other.")
      (home-page "https://github.com/sporkmonger/rack-mount")
      (license license:expat))))

(define-public ruby-ci-reporter
  (package
    (name "ruby-ci-reporter")
    (version "2.0.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "ci_reporter" version))
              (sha256
               (base32
                "17fm20jmw3ajdryhkkxpjahcfx7bgswqzxrskivlkns2718ayyyg"))))
    (build-system ruby-build-system)
    (arguments
     `(#:test-target "rspec"))
    (propagated-inputs
     (list ruby-builder))
    (native-inputs
     (list bundler ruby-rspec))
    (synopsis "Generate XML reports of runs test")
    (description
     "@code{CI::Reporter} is an add-on to Ruby testing frameworks that allows
you to generate XML reports of your test runs.  The resulting files can be
read by a continuous integration system that understands Ant's JUnit report
format.")
    (home-page "https://github.com/nicksieger/ci_reporter")
    (license license:expat)))

(define-public ruby-console
  (package
    (name "ruby-console")
    (version "1.16.2")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "console" version))
              (sha256
               (base32
                "0y1bv3kd1l9p0k5n3anvvjxdrcq113pyngz2g29i9mvdgbbx7kq2"))))
    (build-system ruby-build-system)
    ;; XXX: Disable test suite to avoid dependency cycles with ruby-samovar.
    (arguments (list #:tests? #f))
    (propagated-inputs (list ruby-fiber-local))
    (synopsis "Console logging library for Ruby")
    (description "This gem provides beautiful console logging for Ruby
applications.  It implements fast, buffered log output and has the following
features:
@itemize
@item Thread safe global logger with per-fiber context
@item Carry along context with nested loggers
@item Enable/disable log levels per class
@item Detailed logging of exceptions
@item Beautiful logging to the terminal or structured logging using JSON.
@end itemize")
    (home-page "https://github.com/socketry/console")
    (license license:expat)))

(define-public ruby-contracts
  (package
    (name "ruby-contracts")
    (version "0.17")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "contracts" version))
       (sha256
        (base32
         "0gfybfsb6kqxvvcrv1q7bfjaxmq73pf3vqy4bbzarkbajil05ii5"))))
    (build-system ruby-build-system)
    (arguments
     (list
      #:test-target "spec"
      #:phases
      #~(modify-phases %standard-phases
          ;; Don't run or require rubocop, the code linting tool, as this is a
          ;; bit unnecessary.
          (add-after 'unpack 'dont-run-rubocop
            (lambda _
              (substitute* "Rakefile"
                ((".*rubocop.*") "")
                ((".*RuboCop.*") "")))))))
    (native-inputs
     (list ruby-rspec))
    (synopsis "Method contracts for Ruby")
    (description
     "This library provides contracts for Ruby.  A contract describes the
correct inputs and output for a method, and will raise an error if a incorrect
value is found.")
    (home-page "https://github.com/egonSchiele/contracts.ruby")
    (license license:bsd-2)))

(define-public ruby-crack
  (package
    (name "ruby-crack")
    (version "0.4.5")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "crack" version))
       (sha256
        (base32
         "1cr1kfpw3vkhysvkk3wg7c54m75kd68mbm9rs5azdjdq57xid13r"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (for-each (lambda (file)
                           (display file)(display "\n")
                           (invoke "ruby" "-Ilib" "-Itest" "-rrubygems" file))
                         (find-files "test" ".*rb$")))
             #t)))))
    (synopsis "Simple JSON and XML parsing for Ruby")
    (description
     "@code{crack} provides really simple JSON and XML parsing, extracted from
code in Merb and Rails.")
    (home-page "https://github.com/jnunemaker/crack")
    (license license:expat)))

(define-public ruby-clamp
  (package
    (name "ruby-clamp")
    (version "1.3.2")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "clamp" version))
              (sha256
               (base32
                "08m0syh06bhx8dqn560ivjg96l5cs5s3l9jh2szsnlcdcyl9jsjg"))))
    (build-system ruby-build-system)
    (arguments
     (list #:test-target "spec"
           #:phases #~(modify-phases %standard-phases
                        (add-after 'unpack 'do-not-require-rubocop
                          (lambda _
                            (substitute* "Rakefile"
                              (("require \"rubocop/rake_task\"")
                               "")
                              (("RuboCop::RakeTask.new")
                               "")))))))
    (native-inputs (list ruby-rspec))
    (synopsis "Command-line parsing library for Ruby")
    (description "Clamp provides an object-model for command-line utilities.
It handles parsing of command-line options, and generation of usage help.")
    (home-page "https://github.com/mdub/clamp")
    (license license:expat)))

(define-public ruby-cliver
  (package
    (name "ruby-cliver")
    (version "0.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "cliver" version))
       (sha256
        (base32
         "096f4rj7virwvqxhkavy0v55rax10r4jqf8cymbvn4n631948xc7"))))
    (build-system ruby-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         ;; Avoid a incompatibility between rspec@2 and rake. Using rspec@3
         ;; would be nice, but the tests look to be incompatible:
         ;;
         ;; NoMethodError: undefined method `last_comment'
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "rspec"))
             #t)))))
    (native-inputs
     (list bundler ruby-rspec-2))
    (synopsis "Assertions for command-line dependencies in Ruby")
    (description
     "@code{cliver} provides a way to detect missing command-line
dependencies, including versions.")
    (home-page "https://github.com/yaauie/cliver")
    (license license:expat)))

(define-public ruby-czmq-ffi-gen
  (package
    (name "ruby-czmq-ffi-gen")
    (version "0.13.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "czmq-ffi-gen" version))
       (sha256
        (base32
         "1yf719dmf4mwks1hqdsy6i5kzfvlsha69sfnhb2fr2cgk2snbys3"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f ;; Tests are not included in the release on rubygems.org
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-lib_dirs
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "lib/czmq-ffi-gen/czmq/ffi.rb"
               (("lib\\_dirs = \\[.*\\]")
                (string-append "lib_dirs = ['"
                               (assoc-ref inputs "czmq") "/lib"
                               "']")))
             (substitute* "lib/czmq-ffi-gen/libzmq.rb"
               (("lib\\_dirs = \\[.*\\]")
                (string-append "lib_dirs = ['"
                               (assoc-ref inputs "zeromq") "/lib"
                               "']"))))))))
    (inputs
     (list zeromq czmq))
    (propagated-inputs (list ruby-ffi))
    (synopsis "Low-level Ruby bindings for CZMQ (generated using zproject)")
    (description
     "These Ruby bindings are not intended to be directly used, but rather
used by higher level bindings like those provided by CZTop.")
    (home-page
     "https://github.com/paddor/czmq-ffi-gen")
    (license license:isc)))

(define-public ruby-cztop
  (package
    (name "ruby-cztop")
    (version "0.12.2")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "cztop" version))
       (sha256
        (base32
         "0yqbpaiw5d7f271d73lyrsh8xpx6n4zi6xqwfgi00dacxrq3s3fa"))))
    (build-system ruby-build-system)
    (arguments
     '(#:test-target "spec"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-lib_paths
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "lib/cztop/poller/zmq.rb"
               (("lib\\_paths = \\[.*\\]")
                (string-append "lib_paths = ['"
                               (assoc-ref inputs "zeromq") "/lib"
                               "']"))))))))
    (native-inputs
     (list bundler ruby-rspec))
    (inputs
     (list zeromq))
    (propagated-inputs
     (list ruby-czmq-ffi-gen))
    (synopsis "CZMQ Ruby bindings")
    (description
     "CZMQ Ruby bindings, based on the generated low-level FFI bindings of
CZMQ.  The focus of of CZTop is on being easy to use and providing first class
support for security mechanisms.")
    (home-page "https://github.com/paddor/cztop")
    (license license:isc)))

(define-public ruby-saikuro-treemap
  (package
    (name "ruby-saikuro-treemap")
    (version "0.2.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "saikuro_treemap" version))
              (sha256
               (base32
                "0w70nmh43mwfbpq20iindl61siqqr8acmf7p3m7n5ipd61c24950"))))
    (build-system ruby-build-system)
    ;; Some of the tests fail because the generated JSON has keys in a
    ;; different order.  This is a problem with the test suite rather than any
    ;; of the involved libraries.
    (arguments `(#:tests? #f))
    (propagated-inputs
     (list ruby-json-pure ruby-atoulme-saikuro))
    (synopsis "Generate complexity treemap based on saikuro analysis")
    (description
     "This gem generates a treemap showing the complexity of Ruby code on
which it is run.  It uses Saikuro under the covers to analyze Ruby code
complexity.")
    (home-page "https://github.com/ThoughtWorksStudios/saikuro_treemap")
    (license license:expat)))

(define-public ruby-oauth2
  (package
    (name "ruby-oauth2")
    (version "2.0.9")
    (source (origin
              (method git-fetch)        ;for tests
              (uri (git-reference
                    (url "https://gitlab.com/oauth-xx/oauth2")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "191j1f4gjw8wij1jy2fvddgi8cv1mm0ki7v0b0795clix1avnj29"))))
    (build-system ruby-build-system)
    (arguments
     (list #:phases #~(modify-phases %standard-phases
                        (add-after 'unpack 'relax-requirements
                          (lambda _
                            (substitute* "Gemfile"
                              (("^linting = .*")
                               "linting = false\n")
                              (("^coverage = .*")
                               "coverage = false\n")
                              (("^debug = .*")
                               "debug = false\n"))
                            (substitute* "spec/spec_helper.rb"
                              (("^RUN_COVERAGE = .*")
                               "RUN_COVERAGE = false\n")
                              (("^ALL_FORMATTERS = .*")
                               "ALL_FORMATTERS = false\n")))))))
    (native-inputs
     (list ruby-addressable
           ruby-backports
           ruby-rexml
           ruby-rspec-block-is-expected
           ruby-rspec-pending-for
           ruby-rspec-stubbed-env
           ruby-silent-stream))
    (propagated-inputs
     (list ruby-faraday
           ruby-jwt
           ruby-multi-json
           ruby-multi-xml
           ruby-rack
           ruby-snaky-hash))
    (synopsis "Ruby wrapper for the OAuth 2.0")
    (description
     "This package provides a Ruby wrapper for the OAuth 2.0 protocol built
with a similar style to the original OAuth spec.")
    (home-page "https://github.com/oauth-xx/oauth2")
    (license license:expat)))

(define-public ruby-omniauth
  (package
    (name "ruby-omniauth")
    (version "2.1.1")
    (source (origin
              (method git-fetch)        ;for tests
              (uri (git-reference
                    (url "https://github.com/omniauth/omniauth")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1mm7a4ll7ymamrbsl63yi6i34qpwmh2nh5a9kj961gja1iz2gyd1"))))
    (build-system ruby-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'relax-requirements
                 (lambda _
                   (substitute* "spec/helper.rb"
                     ;; This condition is used to require coveralls and
                     ;; simplecov; override it to avoid these extraneous
                     ;; requirements.
                     (("RUBY_VERSION >= '1.9'")
                      "false")
                     (("require 'rack/freeze'") "")))))))
    (native-inputs (list ruby-rspec))
    (propagated-inputs (list ruby-hashie ruby-rack ruby-rack-test
                             ruby-rack-protection))
    (synopsis "Generalized Rack framework for multiple-provider authentication")
    (description
     "This package provides a generalized Rack framework for multiple-provider
authentication.")
    (home-page "https://github.com/omniauth/omniauth")
    (license license:expat)))

(define-public ruby-omniauth-oauth2
  (package
    (name "ruby-omniauth-oauth2")
    (version "1.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "omniauth-oauth2" version))
       (sha256
        (base32
         "0y4y122xm8zgrxn5nnzwg6w39dnjss8pcq2ppbpx9qn7kiayky5j"))))
    (build-system ruby-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'relax-requirements
                 (lambda _
                   (substitute* "spec/helper.rb"
                     ;; This condition is used to require coveralls and
                     ;; simplecov; override it to avoid these extraneous
                     ;; requirements.
                     (("RUBY_VERSION >= \"1.9\"")
                      "false")))))))
    (propagated-inputs (list ruby-oauth2 ruby-omniauth))
    (native-inputs (list ruby-rspec ruby-rack-test ruby-webmock))
    (synopsis "Abstract OAuth2 strategy for OmniAuth")
    (description
     "This library provides a generic OAuth2 strategy for OmniAuth.  It
doesn't provide a way to gather user information, so should be used as a
building block for authentication strategies.")
    (home-page "https://github.com/omniauth/omniauth-oauth2")
    (license license:expat)))

(define-public ruby-open4
  (package
  (name "ruby-open4")
  (version "1.3.4")
  (source
    (origin
      (method url-fetch)
      (uri (rubygems-uri "open4" version))
      (sha256
        (base32
          "1cgls3f9dlrpil846q0w7h66vsc33jqn84nql4gcqkk221rh7px1"))))
  (build-system ruby-build-system)
  (arguments
   '(#:phases
     (modify-phases %standard-phases
       (add-after 'unpack 'patch
         (lambda _
           (substitute* "rakefile"
             ;; Update the Rakefile so it works
             (("-rubygems") "-rrubygems")
             (("Config") "RbConfig"))
           #t))
       (add-before 'check 'set-LIB
         (lambda _
           ;; This is used in the rakefile when running the tests
           (setenv "LIB" "open4")
           #t)))))
  (synopsis "Open child processes from Ruby and manage them easily")
  (description
    "@code{Open4} is a Ruby library to run child processes and manage their
input and output.")
  (home-page "https://github.com/ahoward/open4")
  (license license:ruby)))

(define-public ruby-options
  (package
    (name "ruby-options")
    (version "2.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "options" version))
       (sha256
        (base32
         "1s650nwnabx66w584m1cyw82icyym6hv5kzfsbp38cinkr5klh9j"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f ;; TODO: NameError: uninitialized constant Config
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'set-LIB
           (lambda _
             ;; This is used in the Rakefile, and setting it avoids an issue
             ;; with running the tests.
             (setenv "LIB" "options")
             #t)))))
    (synopsis "Ruby library to parse options from *args cleanly")
    (description
     "The @code{options} library helps with parsing keyword options in Ruby
functions.")
    (home-page "https://github.com/ahoward/options")
    (license license:ruby)))

(define-public ruby-erubi
  (package
    (name "ruby-erubi")
    (version "1.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "erubi" version))
       (sha256
        (base32
         "1kagnf6ziahj0d781s6ryy6fwqwa3ad4xbzzj84p9m4nv4c2jir1"))))
    (build-system ruby-build-system)
    (synopsis "ERB template engine for Ruby")
    (description
     "Erubi is a ERB template engine for Ruby.  It is a simplified fork of
Erubis")
    (home-page "https://github.com/jeremyevans/erubi")
    (license license:expat)))

(define-public ruby-erubis
  (package
    (name "ruby-erubis")
    (version "2.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "erubis" version))
       (sha256
        (base32
         "1fj827xqjs91yqsydf0zmfyw9p4l2jz5yikg3mppz6d7fi8kyrb3"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ; tests do not run properly with Ruby 2.0
    (synopsis "Implementation of embedded Ruby (eRuby)")
    (description
     "Erubis is a fast implementation of embedded Ruby (eRuby) with several
features such as multi-language support, auto escaping, auto trimming spaces
around @code{<% %>}, a changeable embedded pattern, and Ruby on Rails
support.")
    (home-page "http://www.kuwata-lab.com/erubis/")
    (license license:expat)))

(define-public ruby-ethon
  (package
    (name "ruby-ethon")
    (version "0.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "ethon" version))
       (sha256
        (base32
         "0gggrgkcq839mamx7a8jbnp2h7x2ykfn34ixwskwb0lzx2ak17g9"))))
    (build-system ruby-build-system)
    (arguments
     (list
      #:tests? #f  ; no included tests
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'libcurl-use-absolute-reference
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "lib/ethon/curls/settings.rb"
                (("libcurl', 'libcurl\\.so\\.4")
                 (search-input-file inputs "/lib/libcurl.so"))))))))
    (inputs
     (list curl))
    (propagated-inputs
     (list ruby-ffi))
    (synopsis "Very lightweight @code{libcurl} wrapper")
    (description
     "Ethon is a very basic @code{libcurl} wrapper using ffi.")
    (home-page "https://github.com/typhoeus/ethon")
    (license license:expat)))

(define-public ruby-execjs
  (package
    (name "ruby-execjs")
    (version "2.7.0")
    (source
     (origin
       ;; fetch from github as the gem does not contain testing code
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rails/execjs")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0c0vd2mmqq3ar4plbwi2wsbr31vn4h45i19r5km66skydnnbp1y6"))))
    (build-system ruby-build-system)
    (native-inputs
     (list bundler
           ;; The test suite tests all the available backends. Currently, this just
           ;; means the node backend.
           ;;
           ;; PASSED: test:node
           ;; SKIPPED: test:duktape, ;; test:javascriptcore, test:jscript,
           ;; test:miniracer, test:rubyracer, ;; test:rubyrhino, test:v8
           node-lts))
    (synopsis "Run JavaScript code from Ruby")
    (description
     "ExecJS lets you run JavaScript code from Ruby.  It automatically picks a
runtime to evaluate your JavaScript program, then returns the result to you as
a Ruby object.")
    (home-page "https://github.com/rails/execjs")
    (license license:expat)))

(define-public ruby-fakefs
  (package
    (name "ruby-fakefs")
    (version "1.2.2")
    (home-page "https://github.com/fakefs/fakefs")
    (source (origin
              ;; The Rubygems release does not contain tests.
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "008dq9knyip2bfbl0mrk8b8r7bv0k3bf128wcfqsgy1rqal4mgwk"))))
    (build-system ruby-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (replace 'replace-git-ls-files
                    (lambda _
                      (substitute* "fakefs.gemspec"
                        (("`git ls-files lib README.md LICENSE`")
                         "`find lib README.md LICENSE -type f | sort`"))
                      #t))
                  (add-before 'check 'remove-version-constraints
                    (lambda _
                      ;; Drop hard version requirements for test dependencies.
                      (substitute* "fakefs.gemspec"
                        (("(.*add_development_dependency .*), .*" _ dep)
                         (string-append dep "\n")))
                      #t)))))
    (native-inputs
     (list ruby-bump ruby-maxitest ruby-rubocop ruby-rspec))
    (synopsis "Fake file system for Ruby")
    (description
     "This package provides a fake file system for use in test suites.  It
avoids the need for manually creating temporary directories, or dealing
with platform intricacies in @code{File} and @code{FileUtils}.")
    (license license:expat)))

(define-public ruby-orderedhash
  (package
    (name "ruby-orderedhash")
    (version "0.0.6")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "orderedhash" version))
              (sha256
               (base32
                "0fryy7f9jbpx33jq5m402yqj01zcg563k9fsxlqbhmq638p4bzd7"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ; no test suite
    (synopsis "Ruby library providing an order-preserving hash")
    (description "Orderedhash is a Ruby library providing a hash
implementation that preserves the order of items and features some array-like
extensions.")
    (home-page "http://codeforpeople.com/lib/ruby/orderedhash/")
    (license license:public-domain)))

(define-public ruby-libxml
  (package
    (name "ruby-libxml")
    (version "3.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "libxml-ruby" version))
       (sha256
        (base32
         "0xy8wmjwjcnv36zi042678ncjzpxvy351ccbv7mzkns2n3kxfp54"))))
    (build-system ruby-build-system)
    (inputs
     (list zlib libxml2))
    (arguments
     '(#:tests? #f ; test suite hangs for unknown reason
       #:gem-flags
       (list "--no-document"            ; TODO: Re-enable when documentation
                                        ; generation works
             "--"
             (string-append "--with-xml2-include="
                            (assoc-ref %build-inputs "libxml2")
                            "/include/libxml2" ))))
    (synopsis "Ruby bindings for GNOME Libxml2")
    (description "The Libxml-Ruby project provides Ruby language bindings for
the GNOME Libxml2 XML toolkit.")
    (home-page "https://xml4r.github.com/libxml-ruby")
    (license license:expat)))

(define-public ruby-lino
  (package
    (name "ruby-lino")
    (version "3.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "lino" version))
       (sha256
        (base32
         "1zq9dza040fgjvr9imh7z2lgxrcyc5ac100rqimsnsf9bpfz3fsm"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ; No included tests
    (propagated-inputs
     (list ruby-hamster ruby-open4))
    (synopsis "Build and execute commands in Ruby")
    (description
     "@code{Lino} provides an interface to run external commands.  It provides
an interface to add options as well as managing the standard input, output and
error streams.")
    (home-page "https://github.com/tobyclemson/lino")
    (license license:expat)))

(define-public ruby-x25519
  (package
    (name "ruby-x25519")
    (version "1.0.10")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/RubyCrypto/x25519")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1g0311ly32f6hfn4q5fvkbjbl2bhv1l9fx6s0kglxfsrwq51926y"))
              (patches
               (search-patches
                "ruby-x25519-automatic-fallback-non-x86_64.patch"))))
    (build-system ruby-build-system)
    (arguments
     (list #:test-target "spec"
           #:phases #~(modify-phases %standard-phases
                        (add-after 'unpack 'remove-unnecessary-dependencies
                          (lambda _
                            (substitute* "Gemfile"
                              ((".*rubocop.*")
                               ""))
                            (substitute* "Rakefile"
                              (("require \"rubocop/rake_task\"")
                               "")
                              (("RuboCop::RakeTask.new")
                               ""))))
                        (add-before 'build 'compile
                          (lambda _
                            (invoke "rake" "compile"))))))
    (native-inputs (list ruby-rake-compiler ruby-rspec))
    (synopsis "Cryptography library for Ruby providing the X25519
Diffie-Hellman function")
    (description
     "The x25519 gem is an efficient public key cryptography library for
Ruby providing key exchange/agreement via the X25519 (as known as
Curve25519) Elliptic Curve Diffie-Hellman function as described in
@url{https://www.ietf.org/rfc/rfc7748.txt, RFC 7748}.")
    (home-page "https://github.com/RubyCrypto/x25519")
    (license license:bsd-3)))

(define-public ruby-xml-simple
  (package
    (name "ruby-xml-simple")
    (version "1.1.5")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "xml-simple" version))
              (sha256
               (base32
                "0xlqplda3fix5pcykzsyzwgnbamb3qrqkgbrhhfz2a2fxhrkvhw8"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ; no test suite
    (synopsis "Simple Ruby library for XML processing")
    (description "This library provides a simple API for XML processing in
Ruby.")
    (home-page "https://github.com/maik/xml-simple")
    (license license:ruby)))

(define-public ruby-xpath
  (package
    (name "ruby-xpath")
    (version "3.2.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "xpath" version))
              (sha256
               (base32
                "0bh8lk9hvlpn7vmi6h4hkcwjzvs2y0cmkk3yjjdr8fxvj6fsgzbd"))))
    (build-system ruby-build-system)
    (arguments (list #:phases #~(modify-phases %standard-phases
                                  (replace 'check
                                    (lambda* (#:key tests? #:allow-other-keys)
                                      (when tests?
                                        (invoke "rspec" "spec" )))))))
    (native-inputs (list ruby-pry ruby-rspec))
    (propagated-inputs (list ruby-nokogiri))
    (synopsis "Ruby DSL for generating XPath expressions")
    (description "XPath is a Ruby domain-specific language (DSL) for
generating XPath expressions.")
    (home-page "https://github.com/teamcapybara/xpath")
    (license license:expat)))

(define-public ruby-thor
  (package
    (name "ruby-thor")
    (version "1.2.2")
    (source (origin
              ;; Pull from git because the gem has no tests.
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/rails/thor")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1k3z2mlhaig5ycapjxwybb19z7ca0q1876i6csfmv2j0hf1hnc0z"))))
    (build-system ruby-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-coveralls-dependency
            (lambda _
              ;; Do not hook the test suite into the online coveralls service.
              (substitute* "Gemfile"
                ((".*coveralls.*") ""))
              (substitute* "spec/helper.rb"
                (("require \"coveralls\"") "")
                (("Coveralls::SimpleCov::Formatter") ""))))
          (add-after 'unpack 'disable-problematic-tests
            (lambda _
              ;; These tests attempt to check the git repository for
              ;; tabs vs spaces, double vs single quotes, etc, and
              ;; depend on the git checkout.
              (delete-file "spec/quality_spec.rb")
              (substitute* "spec/parser/options_spec.rb"
                ;; This test fails for unknown reasons (see:
                ;; https://github.com/rails/thor/issues/814).
                (("it \"raises an error for unknown switches" all)
                 (string-append "x" all)))))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "rspec" "spec" )))))))
    (native-inputs (list ruby-rspec ruby-simplecov ruby-webmock))
    (synopsis "Ruby toolkit for building command-line interfaces")
    (description "Thor is a toolkit for building powerful command-line
interfaces.")
    (home-page "http://whatisthor.com/")
    (license license:expat)))

(define-public ruby-lumberjack
  (package
    (name "ruby-lumberjack")
    (version "1.0.13")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "lumberjack" version))
              (sha256
               (base32
                "06im7gcg42x77yhz2w5da2ly9xz0n0c36y5ks7xs53v0l9g0vf5n"))))
    (build-system ruby-build-system)
    (native-inputs
     (list ruby-rspec ruby-timecop))
    (synopsis "Logging utility library for Ruby")
    (description "Lumberjack is a simple logging utility that can be a drop in
replacement for Logger or ActiveSupport::BufferedLogger.  It provides support
for automatically rolling log files even with multiple processes writing the
same log file.")
    (home-page "https://github.com/bdurand/lumberjack")
    (license license:expat)))

(define-public ruby-rbnacl
  (package
    (name "ruby-rbnacl")
    (version "7.1.1")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "rbnacl" version))
              (sha256
               (base32
                "0y8yzianlkc9w6sbqy8iy8l0yym0y6x7p5rjflkfixq76fqmhvzk"))))
    (build-system ruby-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-unnecessary-dependencies
            (lambda _
              ;; Coveralls relates to a network service, and Rubocop to code
              ;; linting and both are unnecessary to run the tests
              (substitute* "Gemfile"
                ((".*rubocop.*")
                 "\n")
                ((".*guard-rspec.*")
                 "\n")
                ((".*coveralls.*")
                 "\n"))
              (substitute* "spec/spec_helper.rb"
                (("require \"coveralls\"")
                 "")
                (("Coveralls.wear!")
                 ""))))
          (add-after 'unpack 'use-libsodium-from-store
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* '("lib/rbnacl/init.rb"
                             "lib/rbnacl/sodium.rb")
                (("ffi_lib \\[.+\\]")
                 (string-append "ffi_lib [\""
                                (assoc-ref inputs "libsodium")
                                "/lib/libsodium.so" "\"]")))))
          ;; Run Rspec directly to avoid the Rubocop dependency in the
          ;; Rakefile
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "rspec")))))))
    (propagated-inputs (list ruby-ffi))
    (inputs (list libsodium))
    (native-inputs (list bundler ruby-rspec))
    (synopsis "Ruby FFI binding to libsodium")
    (description
     "This package provides Ruby FFI bindings to the Networking and
Cryptography (NaCl) library, also known as libsodium.  This provides a
high-level toolkit for building cryptographic systems and protocols.")
    (home-page "https://github.com/RubyCrypto/rbnacl")
    (license license:expat)))

(define-public ruby-rbtree
  (package
    (name "ruby-rbtree")
    (version "0.4.6")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "rbtree" version))
              (sha256
               (base32
                "1z0h1x7fpkzxamnvbw1nry64qd6n0nqkwprfair29z94kd3a9vhl"))))
    (build-system ruby-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'extconf
            (lambda _
              (invoke "ruby" "extconf.rb")
              (invoke "make" "install" (string-append "prefix=" #$output))))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "ruby" "-I." "test.rb")))))))
    (synopsis "Ruby implementation of a sorted associative collection")
    (description
     "This package provides a RBTree is a sorted associative collection that
is implemented with a Red-Black Tree.  It maps keys to values like a Hash, but
maintains its elements in ascending key order.  The interface is the almost
identical to that of Hash.")
    (home-page "http://rbtree.rubyforge.org/")
    (license license:expat)))

(define-public ruby-rgl
  (package
    (name "ruby-rgl")
    (version "0.6.6")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "rgl" version))
              (sha256
               (base32
                "0dji1k9knrf8cxm5psd3pgd9i8f7cfq182jwjpi1pwxw15axf496"))))
    (build-system ruby-build-system)
  (arguments
   (list
    #:phases
    #~(modify-phases %standard-phases
        (add-after 'unpack 'remove-unnecessary-dependencies
          (lambda _
            (substitute* "Gemfile"
              ;; Caring about coverage is a not a packager's task but a
              ;; developer's
              ;;(("gem \"simplecov\"") "")
              ;; CodeClimate is an online service, and is unnecessary for
              ;; running the tests
              (("gem \"codeclimate-test-reporter\", .*") "\n")))))))
    (native-inputs (list ruby-test-unit ruby-simplecov ruby-yard graphviz-minimal))
    (propagated-inputs (list ruby-pairing-heap ruby-rexml ruby-stream))
    (synopsis "Framework for graph data structures and algorithms")
    (description "RGL is a framework for graph data structures and algorithms.
The design of the library is much influenced by the Boost Graph Library (BGL)
which is written in C++.")
    (home-page "https://github.com/monora/rgl")
    (license license:bsd-2)))

(define-public ruby-hkdf
  (package
    (name "ruby-hkdf")
    (version "1.0.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/jtdowney/hkdf")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1xqwdmxfnhagivwgb5v9ilwpb4jxlsqwj7pnj43d65zzg5m8p9r5"))))
    (build-system ruby-build-system)
    (arguments
     `(#:test-target "default"))
    (native-inputs
     (list ruby-rspec))
    (synopsis "HMAC-based Key Derivation Function")
    (description
     "This package provides a Ruby implementation of RFC5869: @acronym{HKDF,
HMAC-based Extract-and-Expand Key Derivation Function}.  The goal of HKDF is to
take some source key material and generate suitable cryptographic keys from it.")
    (home-page "https://github.com/jtdowney/hkdf")
    (license license:expat)))

(define-public ruby-nenv
  (package
    (name "ruby-nenv")
    (version "0.3.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "nenv" version))
              (sha256
               (base32
                "0r97jzknll9bhd8yyg2bngnnkj8rjhal667n7d32h8h7ny7nvpnr"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f)) ; no tests included
    (native-inputs
     (list ruby-rspec bundler))
    (synopsis "Ruby interface for modifying the environment")
    (description "Nenv provides a convenient wrapper for Ruby's ENV to modify
and inspect the environment.")
    (home-page "https://github.com/e2/nenv")
    (license license:expat)))

(define-public ruby-ptools
  (package
    (name "ruby-ptools")
    (version "1.5.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "ptools" version))
              (sha256
               (base32
                "0damllbshkxycrwjv80sz78h76dw7r9z54d17mb5cbha1daq9q2d"))))
    (build-system ruby-build-system)
    (arguments
     (list
      #:test-target "spec:all"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch
            (lambda _
              (substitute* "Rakefile"
                ;; Don't require rubocop
                (("require 'rubocop/rake_task'") "")
                (("RuboCop::RakeTask.new") "")
                ;; Do not attempt to sign the gem.
                (("spec\\.signing_key = .*") ""))

              (substitute* "spec/binary_spec.rb"
                (("/bin/ls")    (which "ls"))
                (("/bin/cat")   (which "cat"))
                (("/bin/chmod") (which "chmod"))
                (("/bin/df")    (which "df"))))))))
    (native-inputs
     (list ruby-rspec))
    (synopsis "Extra methods for Ruby's @code{File} class")
    (description
     "The @dfn{ptools} (power tools) library extends Ruby's core @code{File}
class with many additional methods modelled after common POSIX tools, such as
@code{File.which} for finding executables, @code{File.tail} to print the last
lines of a file, @code{File.wc} to count words, and so on.")
    (home-page "https://github.com/djberg96/ptools")
    (license license:artistic2.0)))

(define-public ruby-permutation
  (package
    (name "ruby-permutation")
    (version "0.1.8")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "permutation" version))
              (sha256
               (base32
                "13crwk2vfbzv99czva7881027dbcnidihmvx2jc58z2vm3bp9sl8"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-rakefile
          (lambda _
            (substitute* "Rakefile"
              (("require 'rake/gempackagetask'")
               "require 'rubygems/package_task'")
              (("include Config") ""))
            #t))
         (replace 'check
          (lambda _
            (invoke "ruby" "-Ilib" "test/test.rb"))))))
    (synopsis "Library to perform operations with sequence permutations")
    (description "This package provides a Ruby library to perform different
operations with permutations of sequences, such as strings and arrays.")
    (home-page "https://flori.github.io/permutation")
    (license license:gpl2))) ; GPL 2 only

(define-public ruby-shellany
  (package
    (name "ruby-shellany")
    (version "0.0.1")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "shellany" version))
              (sha256
               (base32
                "1ryyzrj1kxmnpdzhlv4ys3dnl2r5r3d2rs2jwzbnd1v96a8pl4hf"))))
    (build-system ruby-build-system)
    (arguments
     `(#:test-target "default"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-version-test
          (lambda _
            (substitute* "spec/shellany_spec.rb"
              (("^RSpec") "require \"shellany\"\nRSpec"))
            #t)))))
    (native-inputs
     (list ruby-rspec ruby-nenv bundler))
    (synopsis "Capture command output")
    (description "Shellany is a Ruby library providing functions to capture
the output produced by running shell commands.")
    (home-page "https://rubygems.org/gems/shellany")
    (license license:expat)))

(define-public ruby-notiffany
  (package
    (name "ruby-notiffany")
    (version "0.1.3")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "notiffany" version))
              (sha256
               (base32
                "0f47h3bmg1apr4x51szqfv3rh2vq58z3grh4w02cp3bzbdh6jxnk"))))
    (build-system ruby-build-system)
    ;; Tests are not included in the gem.
    (arguments `(#:tests? #f))
    (propagated-inputs
     (list ruby-shellany ruby-nenv))
    (native-inputs
     (list bundler))
    (synopsis "Wrapper library for notification libraries")
    (description "Notiffany is a Ruby wrapper library for notification
libraries such as Libnotify.")
    (home-page "https://github.com/guard/notiffany")
    (license license:expat)))

(define-public ruby-forking-test-runner
  (package
    (name "ruby-forking-test-runner")
    (version "1.6.0")
    (home-page "https://github.com/grosser/forking_test_runner")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page)
                                  (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1mrglzkj2nrgisccf2f30zbfmcs0awv1g3lw994b2az90fl39x8m"))))
    (build-system ruby-build-system)
    (arguments
     '(#:test-target "spec"
       ;; FIXME: ActiveRecord depends on sqlite3 1.3.6, but Guix has
       ;; 1.4.1, which in turn breaks the tests that use ActiveRecord.
       #:tests? #f
       #:phases (modify-phases %standard-phases
                  (replace 'replace-git-ls-files
                    (lambda _
                      (substitute* "forking_test_runner.gemspec"
                        (("`git ls-files lib/ bin/ MIT-LICENSE`")
                         "`find lib/ bin/ MIT-LICENSE -type f | sort`"))
                      #t))
                  (add-before 'check 'remove-version-constraints
                    (lambda _
                      ;; Ignore hard coded version constraints for the tests.
                      (delete-file "Gemfile.lock")
                      #t))
                  (add-before 'check 'set-HOME
                    (lambda _
                      ;; Many tests invoke Bundler, and fails when Bundler
                      ;; warns that /homeless-shelter does not exist.
                      (setenv "HOME" "/tmp")
                      #t)))))
    (native-inputs
     (list ruby-activerecord ruby-bump ruby-rspec ruby-sqlite3 ruby-wwtd))
    (propagated-inputs
     (list ruby-parallel-tests))
    (synopsis "Run every test in a fork")
    (description
     "This package is a wrapper around @code{parallel_tests} that runs every
test in a fork to avoid pollution and get clean output per test.")
    (license license:expat)))

(define-public ruby-formatador
  (package
    (name "ruby-formatador")
    (version "0.2.5")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "formatador" version))
              (sha256
               (base32
                "1gc26phrwlmlqrmz4bagq1wd5b7g64avpx0ghxr9xdxcvmlii0l0"))))
    (build-system ruby-build-system)
    ;; Circular dependency: Tests require ruby-shindo, which requires
    ;; ruby-formatador at runtime.
    (arguments `(#:tests? #f))
    (synopsis "Ruby library to format text on stdout")
    (description "Formatador is a Ruby library to format text printed to the
standard output stream.")
    (home-page "https://github.com/geemus/formatador")
    (license license:expat)))

(define-public ruby-fuubar
  (package
    (name "ruby-fuubar")
    (version "2.3.2")
    (source
     (origin
       ;; Fetch from the git repository, as the gem package doesn't include
       ;; the tests.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/thekompanee/fuubar")
             (commit (string-append "releases/v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0jm1x2xp13csbnadixaikj7mlkp5yk4byx51npm56zi13izp7259"))))
    (build-system ruby-build-system)
    (arguments
     '(;; TODO: Some tests fail, unsure why.
       ;; 21 examples, 7 failures
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'delete-certificate
           (lambda _
             ;; Remove 's.cert_chain' as we do not build with a private key
             (substitute* "fuubar.gemspec"
               ((".*cert_chain.*") "")
               ((".*signing_key.*") ""))
             #t))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "rspec"))
             #t)))))
    (native-inputs
     (list bundler))
    (propagated-inputs
     (list ruby-rspec-core ruby-progressbar))
    (synopsis "Fuubar is an RSpec formatter that uses a progress bar")
    (description
     "Fuubar is an RSpec formatter that uses a progress bar instead of a
string of letters and dots as feedback.  It also stops on the first test
failure.")
    (home-page "https://github.com/thekompanee/fuubar")
    (license license:expat)))

(define-public ruby-haml
  (package
    (name "ruby-haml")
    (version "5.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "haml" version))
       (sha256
        (base32
         "1q0a9fvqh8kn6wm97fcks6qzbjd400bv8bx748w8v87m7p4klhac"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ; No included tests
    (propagated-inputs
     (list ruby-tilt ruby-temple))
    (synopsis "Haml is a Ruby library to generate HTML documents")
    (description
     "@acronym{Haml, HTML Abstraction Markup Language} is a layer on top of
HTML or XML that is designed to express the structure of documents using
indentation rather than closing tags.  It was originally envisioned as a
plugin for Ruby on Rails, but it can function as a stand-alone templating
engine.")
    (home-page "https://haml.info/")
    (license license:expat)))

(define-public ruby-hamster
  (package
  (name "ruby-hamster")
  (version "3.0.0")
  (source
    (origin
      (method url-fetch)
      (uri (rubygems-uri "hamster" version))
      (sha256
        (base32
          "1n1lsh96vnyc1pnzyd30f9prcsclmvmkdb3nm5aahnyizyiy6lar"))))
  (build-system ruby-build-system)
  (arguments
   (list
    ;; Only supports Ruby 2 currently
    #:ruby ruby-2.7
    #:phases
    #~(modify-phases %standard-phases
        (add-after 'unpack 'remove-unnecessary-dependencies
          (lambda _
            ;; pry is a debugging tool, and is unnecessary when running the
            ;; tests
            (substitute* "spec/lib/hamster/vector/insert_spec.rb"
              (("require 'pry'") ""))
            (substitute* "spec/spec_helper.rb"
              (("require \"pry\"") "")
              ;; CodeClimate is an online service, and is unnecessary for
              ;; running the tests
              (("require \"codeclimate-test-reporter\"") "")
              (("CodeClimate.*\n") ""))))
        ;; No Rakefile is included, so run rspec directly.
        (replace 'check
          (lambda* (#:key tests? #:allow-other-keys)
            (when tests?
              (invoke "ruby" (which "rspec"))))))))
  (propagated-inputs
   (list ruby-concurrent))
  (native-inputs
   (list ruby-rspec))
  (synopsis "Efficient, immutable, thread-safe collection classes for Ruby")
  (description
    "Hamster provides 6 persistent data structures: @code{Hash}, @code{Vector},
@code{Set}, @code{SortedSet}, @code{List}, and @code{Deque} (which works as an
immutable queue or stack).")
  (home-page "https://github.com/hamstergem/hamster")
  (license license:expat)))

(define-public ruby-hashdiff
  (package
    (name "ruby-hashdiff")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "hashdiff" version))
       (sha256
        (base32
         "1nynpl0xbj0nphqx1qlmyggq58ms1phf5i03hk64wcc0a17x1m1c"))))
    (build-system ruby-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         ;; Run tests directly via rspec to avoid depending on rubocop.
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "rspec"))
             #t)))))
    (native-inputs
     (list bundler ruby-rspec-2))
    (synopsis "HashDiff computes the smallest difference between two hashes")
    (description
     "HashDiff is a Ruby library to compute the smallest difference between
two hashes.")
    (home-page "https://github.com/liufengyun/hashdiff")
    (license license:expat)))

(define-public ruby-hydra-minimal
  ;; No releases yet.
  (let ((commit "5abfa378743756ae4d9306cc134bcc482f5c9525")
        (revision "0"))
    (package
      (name "ruby-hydra-minimal")
      (version (git-version "0.0" revision commit))
      (home-page "https://github.com/hyphenation/hydra")
      (source (origin
                (method git-fetch)
                (uri (git-reference (url home-page) (commit commit)))
                (file-name (git-file-name name version))
                ;; byebug is a non-essential debugging utility that brings in
                ;; many dependencies.
                (patches (search-patches "ruby-hydra-minimal-no-byebug.patch"))
                (sha256
                 (base32
                  "1cik398l2765y3d9sdhjzki3303hkry58ac6jlkiy7iy62nm529f"))))
      (build-system ruby-build-system)
      (arguments
       ;; Avoid rspec dependency.
       '(#:tests? #f))
      (synopsis "Ruby hyphenation patterns")
      (description
       "ruby-hydra-minimal is a Ruby library for working with hyphenation patterns.
It is a low-dependency variant of ruby-hydra.")
      (license license:expat))))

;; Pinned variant for use by texlive
(define-public ruby-hydra-minimal/pinned
  (hidden-package
   (package
     (inherit ruby-hydra-minimal)
     (arguments
      (cons* #:ruby ruby-2.7
             (package-arguments ruby-hydra-minimal))))))

(define-public ruby-hydra
  (package
    (inherit ruby-hydra-minimal)
    (name "ruby-hydra")
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'make-files-writable
            (lambda _
              (for-each make-file-writable (find-files "."))))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "rspec")))))))
    (native-inputs
     (list ruby-rspec))
    (description
     "ruby-hydra is a Ruby library for working with hyphenation patterns.")))

(define-public ruby-shindo
  (package
    (name "ruby-shindo")
    (version "0.3.10")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "shindo" version))
              (sha256
               (base32
                "0qnqixhi0g8v44v13f3gynpbvvw6xqi1wajsxdjsx5rhzizfsj91"))))
    (build-system ruby-build-system)
    (arguments
     (list
      #:test-target "shindo_tests"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-tests
            (lambda _
              (substitute* "tests/tests_helper.rb"
                (("-rubygems") ""))
              (substitute* "Rakefile"
                (("system \"shindo") "system \"./bin/shindo")
                ;; This test doesn't work, so we disable it.
                (("fail \"The build_error test should fail") "#")
                ((" -rubygems") "")))))))
    (propagated-inputs
     (list ruby-formatador))
    (synopsis "Simple depth first Ruby testing")
    (description "Shindo is a simple depth first testing library for Ruby.")
    (home-page "https://github.com/geemus/shindo")
    (license license:expat)))

(define-public ruby-rubygems-tasks
  (package
    (name "ruby-rubygems-tasks")
    (version "0.2.5")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "rubygems-tasks" version))
              (sha256
               (base32
                "1x3sz3n2dlknd3v7w1mrq6f0ag6pwzhjvg7z29p75w3p42ma1gbx"))))
    (build-system ruby-build-system)
    ;; Tests need Internet access.
    (arguments `(#:tests? #f))
    (synopsis "Rake tasks for managing and releasing Ruby Gems")
    (description "Rubygems-task provides Rake tasks for managing and releasing
Ruby Gems.")
    (home-page "https://github.com/postmodern/rubygems-tasks")
    (license license:expat)))

(define-public ruby-rubyzip
  (package
    (name "ruby-rubyzip")
    (version "2.3.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/rubyzip/rubyzip")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "03p8c990n6c1r4g64w0vv7z2iaswisl07l2f1lbh1s78cvmlmfxx"))))
    (build-system ruby-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'disable-rubocop
           (lambda _
             (substitute* "Rakefile"
               (("require 'rubocop/rake_task'") "")
               (("RuboCop::RakeTask.new") ""))))
         (add-before 'check 'patch-tests
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "test/gentestfiles.rb"
               (("/usr/bin/zip") (which "zip")))))
         (add-after 'patch-source-shebangs 'unpatch-some-source-shebangs
           (lambda _
             ;; The tests compare zipped files with data test files; since the
             ;; zip files do not have their shebangs patched, the data files
             ;; compared with their extracted version must also be left
             ;; un-patched.
             (substitute* (find-files "test/data" "\\.(txt|rb)$")
               (((which "ruby"))
                "/usr/bin/env ruby")))))))
    (native-inputs
     (list bundler ruby-simplecov zip unzip))
    (synopsis "Ruby module is for reading and writing zip files")
    (description
     "The rubyzip module provides ways to read from and create zip files.")
    (home-page "https://github.com/rubyzip/rubyzip")
    (license license:bsd-2)))

(define-public ruby-silent-stream
  (package
    (name "ruby-silent-stream")
    (version "1.0.6")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "silent_stream" version))
              (sha256
               (base32
                "10381fpvjzfjvhgfyv700607fpa29firgf52w5w5536m4fh6x63m"))))
    (build-system ruby-build-system)
    (synopsis "ActiveSupport stream silencing without ActiveSupport")
    (description "SilentStream is an extraction of some parts of
ActiveSupport's Kernel Reporting Core Extensions around silencing IO
streams.")
    (home-page "https://github.com/pboling/silent_stream")
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

(define-public ruby-simplecov-lcov
  (package
    (name "ruby-simplecov-lcov")
    (version "0.8.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "simplecov-lcov" version))
              (sha256
               (base32
                "1h8kswnshgb9zidvc88f4zjy4gflgz3854sx9wrw8ppgnwfg6581"))))
    (build-system ruby-build-system)
    ;; The test suite fails half of its tests; it seems to rely on older
    ;; versions of simplecov, rspec, possibly others (see:
    ;; https://github.com/fortissimo1997/simplecov-lcov/issues/29).
    (arguments (list #:tests? #f
                     #:test-target "spec"))
    (native-inputs
     (list ruby-activesupport
           ruby-coveralls
           ruby-rspec
           ruby-simplecov))
    (synopsis "SimpleCov formatter to generate a lcov style coverage")
    (description "This package provides a SimpleCov formatter to generate a
lcov-style coverage report.")
    (home-page "https://github.com/fortissimo1997/simplecov-lcov")
    (license license:expat)))

(define-public ruby-snaky-hash
  (package
    (name "ruby-snaky-hash")
    (version "2.0.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference       ;for tests
                    (url "https://gitlab.com/oauth-xx/snaky_hash")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0zmixxzi2g2d75zii65bq037j4g67p25l6aqddbmmwizspsp5az6"))))
    (build-system ruby-build-system)
    (arguments
     (list #:phases #~(modify-phases %standard-phases
                        (add-after 'unpack 'relax-requirements
                          (lambda _
                            (substitute* "Gemfile"
                              (("^linting = .*")
                               "linting = false\n")
                              (("^coverage = .*")
                               "coverage = false\n")
                              (("^debug = .*")
                               "debug = false\n"))
                            (substitute* "spec/spec_helper.rb"
                              (("^RUN_COVERAGE = .*")
                               "RUN_COVERAGE = false\n")
                              (("^ALL_FORMATTERS = .*")
                               "ALL_FORMATTERS = false\n"))))
                        (add-before 'build 'drop-signing-key-requirement
                          (lambda _
                            (substitute* "snaky_hash.gemspec"
                              (("spec.signing_key =.*")
                               "spec.signing_key = nil")))))))
    (native-inputs (list ruby-rspec ruby-rspec-block-is-expected))
    (propagated-inputs (list ruby-hashie ruby-version-gem))
    (synopsis "Hash keys and look-ups normalization Ruby library")
    (description "The SnakyHash Ruby library provides classes for normalizing
hash keys and look-ups, and a pseudo-object interface.")
    (home-page "https://gitlab.com/oauth-xx/snaky_hash")
    (license license:expat)))

(define-public ruby-useragent
  (package
    (name "ruby-useragent")
    (version "0.16.10")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "useragent" version))
              (sha256
               (base32
                "1fv5kvq494swy0p17h9qya9r50w15xsi9zmvhzb8gh55kq6ki50p"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ; no test suite
    (synopsis "HTTP user agent parser for Ruby")
    (description "UserAgent is a Ruby library that parses and compares HTTP
User Agents.")
    (home-page "https://github.com/gshutler/useragent")
    (license license:expat)))

(define-public ruby-backports
  (package
  (name "ruby-backports")
  (version "3.11.4")
  (source
    (origin
      (method url-fetch)
      (uri (rubygems-uri "backports" version))
      (sha256
        (base32
          "1hshjxww2h7s0dk57njrygq4zpp0nlqrjfya7zwm27iq3rhc3y8g"))))
  (build-system ruby-build-system)
  (arguments
   '(;; TODO: This should be default, but there is one test failure
     #:test-target "all_spec"))
  (native-inputs
   (list ruby-mspec ruby-activesupport))
  (synopsis "Backports of the features in newer Ruby versions")
  (description
    "Backports enables more compatibility across Ruby versions by providing
backports of some features.")
  (home-page "https://github.com/marcandre/backports")
  (license license:expat)))

(define-public ruby-bacon
  (package
    (name "ruby-bacon")
    (version "1.2.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "bacon" version))
              (sha256
               (base32
                "1f06gdj77bmwzc1k5iragl1595hbn67yc7sqvs56ca8plrr2vmai"))))
    (build-system ruby-build-system)
    (synopsis "Small RSpec clone")
    (description "Bacon is a small RSpec clone providing all essential
features.")
    (home-page "https://github.com/chneukirchen/bacon")
    (license license:expat)))

(define-public ruby-bacon-bits
  (package
    (name "ruby-bacon-bits")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "bacon-bits" version))
       (sha256
        (base32
         "1ghpj8ja94lhi8rgi872hqk4fd2amz2k7g9znd64z5dj7v6l0dmx"))))
    (build-system ruby-build-system)
    (arguments
     ;; No tests
     '(#:tests? #f))
    (propagated-inputs (list ruby-bacon))
    (synopsis "Extensions to Bacon, for disabling tests, before and after
blocks and more")
    (description
     "This extends the bacon testing framework with useful extensions to
disable tests, have before and after blocks that run once and more.")
    (home-page "https://github.com/cldwalker/bacon-bits")
    (license license:expat)))

(define-public ruby-bacon-colored-output
  (package
    (name "ruby-bacon-colored-output")
    (version "1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "bacon-colored_output" version))
       (sha256
        (base32
         "1znyh3vkfdlmf19p3k4zip88ibym41dn5g4p4n5hmks2iznb7qpx"))))
    (build-system ruby-build-system)
    (arguments
     '(;; No included tests
       #:tests? #f))
    (propagated-inputs
     (list ruby-bacon))
    (synopsis "Colored output for Bacon test framework")
    (description
     "This package adds color through ANSI escape codes to Bacon test
output.")
    (home-page "https://github.com/whitequark/bacon-colored_output")
    (license license:expat)))

(define-public ruby-bake
  (package
    (name "ruby-bake")
    (version "0.18.2")
    (source (origin
              (method git-fetch)        ;for tests
              (uri (git-reference
                    (url "https://github.com/ioquatix/bake")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "19yi1fxzz9n580gig3p3j6nxbgcfcassa6b0q07jkqrzxdqn7xhn"))))
    (build-system ruby-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'sanitize-dependencies
                 (lambda _
                   ;; These dependencies are not needed to build and run tests
                   ;; and contain circular dependencies.
                   (substitute* "gems.rb"
                     ((".*'bake-modernize'.*") "")
                     ((".*'bake-gem'.*") "")
                     ((".*'bake-github-pages'.*") "")
                     ((".*'utopia-project'.*") ""))))
               (add-before 'build 'drop-signing-key-requirement
                 (lambda _
                   (substitute* "bake.gemspec"
                     (("spec.signing_key =.*")
                      "spec.signing_key = nil"))))
               (replace 'check
                 (lambda* (#:key tests? #:allow-other-keys)
                   (when tests?
                     (invoke "rspec")))))))
    (native-inputs (list ruby-covered ruby-rspec))
    (propagated-inputs (list ruby-samovar))
    (synopsis "Replacement for rake with a simpler syntax")
    (description "Bake is a task execution tool, inspired by Rake, but
codifying many of the use cases which are typically implemented in an ad-hoc
manner.")
    (home-page "https://github.com/ioquatix/bake")
    (license license:expat)))

(define-public ruby-bake-test
  (package
    (name "ruby-bake-test")
    (version "0.2.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "bake-test" version))
              (sha256
               (base32
                "1p6kfpncj0s4zyynrrq6c735jvh0dnwyv7kfqym4rpyka4f85qdp"))))
    (build-system ruby-build-system)
    ;; XXX: Disable the test suite to avoid a circular dependency with
    ;; ruby-sus.
    (arguments (list #:tests? #f))
    (propagated-inputs (list ruby-bake))
    (synopsis "Test suite automatic runner for Ruby")
    (description "@command{bake-test} automatically discovers how to run local
test suites for Ruby projects.  It supports @command{rspec}, @command{sus}, as
well as @samp{rake}.")
    (home-page "https://github.com/ioquatix/bake-test")
    (license license:expat)))

(define-public ruby-bake-test-external
  (package
    (name "ruby-bake-test-external")
    (version "0.3.2")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "bake-test-external" version))
              (sha256
               (base32
                "0749xc7jkz1c5gsq1giwhrqy6s6xqm48bdvs414372881wki2jmh"))))
    (build-system ruby-build-system)
    ;; The test suite relies on git and network access to clone external
    ;; repositories.
    (arguments (list #:tests? #f))
    (propagated-inputs (list ruby-bake))
    (synopsis "Continuous integration extension for Bake")
    (description "Bake Test External adds a @samp{test:external} action to the
@command{bake} command to run the test suites of dependent projects to check
for breakage.")
    (home-page "https://github.com/ioquatix/bake-test-external")
    (license license:expat)))

(define-public ruby-connection-pool
  (package
    (name "ruby-connection-pool")
    (version "2.4.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/mperham/connection_pool")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1iijshb1n9xl5knvpzzx0vqlw7v7mskiw1cpfj1cmdmssavyhsx5"))))
    (build-system ruby-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch
            (lambda _
              (substitute* "Rakefile"
                (("require \"standard/rake\"") "")
                ((":\"standard:fix\",") "")))))))
    (native-inputs
     (list bundler))
    (synopsis "Generic connection pool for Ruby")
    (description "Connection_pool provides a generic connection pooling
interface for Ruby programs.")
    (home-page "https://github.com/mperham/connection_pool")
    (license license:expat)))

(define-public ruby-fast-gettext
  (package
    (name "ruby-fast-gettext")
    (version "2.3.0")
    (home-page "https://github.com/grosser/fast_gettext")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page)
                                  (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0ba1wv96qarvvh19s8m1cgd26a9jgil4wl8nwgv4sl9fg5sqgksm"))))
    (build-system ruby-build-system)
    (arguments
     (list
      #:test-target "spec"
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'remove-version-constraints
            (lambda _
              (delete-file "Gemfile.lock"))))))
    (native-inputs
     (list ;; For tests.
           ruby-activerecord
           ruby-activesupport
           ruby-bump
           ruby-forking-test-runner
           ruby-i18n
           ruby-rubocop
           ruby-rubocop-packaging
           ruby-rspec
           ruby-single-cov
           ruby-sqlite3
           ruby-wwtd))
    (synopsis "Fast implementation of @code{GetText}")
    (description
     "This package provides an alternative implementation of the Ruby
@code{GetText} library that is approximately 12x faster yet thread safe.")
    ;; Some parts are covered by the Ruby license, see file headers.
    (license (list license:expat license:ruby))))

(define-public ruby-fiber-local
  (package
    (name "ruby-fiber-local")
    (version "1.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/socketry/fiber-local")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0pp5b81h0lysdnphgprkixh1az0fkrgir5sbcp0mm8arxf3f8m90"))))
    (build-system ruby-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-after 'extract-gemspec 'sanitize-dependencies
                 (lambda _
                   ;; This pulls in extraneous maintenance dependencies.
                   (delete-file "gems.rb")
                   ;; Depending on ruby-covered would introduce a dependency
                   ;; cycle with it.
                   (substitute* '("fiber-local.gemspec" "spec/spec_helper.rb")
                     ((".*covered.*") ""))))
               (replace 'check
                 (lambda* (#:key tests? #:allow-other-keys)
                   (when tests?
                     (invoke "rspec")))))))
    (native-inputs (list ruby-rspec))
    (synopsis "Ruby module to simplify fiber-local state management")
    (description "This package provides a class-level mixin to make managing
fiber-local state easy.  It provides easy access to a fiber-local state from a
fiber, and defaults to a shared thread-local state.")
    (home-page "https://github.com/socketry/fiber-local")
    (license license:expat)))

(define-public ruby-flores
  (package
    (name "ruby-flores")
    (version "0.0.8")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "flores" version))
              (sha256
               (base32
                "0pd8gqgy67rp1baq5r7himl0r9jzv5kqlhdmqh8wngynv548w2ai"))))
    (build-system ruby-build-system)
    (arguments
     (list #:ruby ruby-2.7
           #:phases
           #~(modify-phases %standard-phases
               (replace 'check
                 (lambda* (#:key tests? #:allow-other-keys)
                   (when tests?
                     (invoke "ruby" (which "rspec"))))))))
    (native-inputs (list ruby-rspec ruby-simplecov))
    (synopsis "Fuzzing, randomization, and stress testing library")
    (description "Flores is a fuzzing, randomization, and stress library to
help tests uncover more bugs.")
    (home-page "https://github.com/jordansissel/ruby-flores")
    (license license:asl2.0)))

(define-public ruby-ipaddr
  (package
    (name "ruby-ipaddr")
    (version "1.2.6")
    (source (origin
              (method git-fetch)  ;for tests
              (uri (git-reference
                    (url "https://github.com/ruby/ipaddr")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0h3z8i1fa8s4gx48322fflhpkzghd4bmd9109hglsgdkic7b0dyp"))))
    (build-system ruby-build-system)
    (native-inputs (list ruby-test-unit-ruby-core))
    (synopsis "Manipulate IP addresses")
    (description "This package provides a set of methods to manipulate an IP
address.  Both IPv4 and IPv6 are supported.")
    (home-page "https://github.com/ruby/ipaddr")
    (license license:bsd-2)))

(define-public ruby-fake-ftp
  (package
    (name "ruby-fake-ftp")
    (version "0.3.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "fake_ftp" version))
              (sha256
               (base32
                "1zl9q9m4x7lz9890g0h1qqj7hcxnwzpjfnfbxadjblps7b5054q4"))))
    (build-system ruby-build-system)
    (native-inputs (list ruby-rspec ruby-rubocop ruby-simplecov))
    (arguments
     '(#:test-target "spec"))
    (synopsis "Fake FTP server for use with ruby tests")
    (description "This package allows you to test FTP implementations in ruby.
It is a minimal single-client FTP server that can be bound to any arbitrary
port on localhost.")
    (home-page "https://rubygems.org/gems/fake_ftp")
    (license license:expat)))

(define-public ruby-net-telnet
  (package
    (name "ruby-net-telnet")
    (version "0.2.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "net-telnet" version))
              (sha256
               (base32
                "16nkxc79nqm7fd6w1fba4kb98vpgwnyfnlwxarpdcgywz300fc15"))))
    (build-system ruby-build-system)
    (synopsis "Telnet client functionality")
    (description "This package provides telnet client functionality.")
    (home-page "https://github.com/ruby/net-telnet")
    (license license:bsd-2)))

(define-public ruby-net-ftp
  (package
    (name "ruby-net-ftp")
    (version "0.3.4")
    (source (origin
              (method git-fetch)  ;for tests
              (uri (git-reference
                    (url "https://github.com/ruby/net-ftp")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "11b1sw7c4c7xrhn5li5m0wylw42hp52jp6pqacyb43hkw1m5zr36"))))
    (build-system ruby-build-system)
    (propagated-inputs (list ruby-net-protocol ruby-time))
    (synopsis "File Transfer Protocol client library")
    (description "This class implements the File Transfer Protocol.  If you
have used a command-line FTP program, and are familiar with the commands, you
will be able to use this class easily.  Some extra features are included to
take advantage of Ruby's style and strengths.")
    (home-page "https://github.com/ruby/net-ftp")
    (license license:bsd-2)))

(define-public ruby-net-http-persistent
  (package
    (name "ruby-net-http-persistent")
    (version "4.0.2")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "net-http-persistent" version))
              (sha256
               (base32
                "0i1as2lgnw7b4jid0gw5glv5hnxz36nmfsbr9rmxbcap72ijgy03"))))
    (build-system ruby-build-system)
    (native-inputs
     (list ruby-connection-pool
           ruby-hoe
           ruby-rake-manifest))
    (synopsis "Persistent HTTP connection manager")
    (description "Net::HTTP::Persistent manages persistent HTTP connections
using Net::HTTP, supporting reconnection and retry according to RFC 2616.")
    (home-page "https://github.com/drbrain/net-http-persistent")
    (license license:expat)))

(define-public ruby-net-imap
  (package
    (name "ruby-net-imap")
    (version "0.3.4")
    (source (origin
              (method git-fetch)        ;for tests
              (uri (git-reference
                    (url "https://github.com/ruby/net-imap")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0nx49i1n9q1wpancqaac2srrpb8mb43mc8wryyqyhpgki2grwyxw"))))
    (build-system ruby-build-system)
    (arguments
     ;; The test suite appears to rely on RFCs it tries fetching from the
     ;; network (see: https://github.com/ruby/net-imap/issues/136).
     (list #:tests? #f
           #:phases
           #~(modify-phases %standard-phases
               (add-before 'replace-git-ls-files 'adjust-for-git-ls-files
                 (lambda _
                   ;; Adjust the git ls-files invocation so that it matches
                   ;; the expected pattern.
                   (substitute* "net-imap.gemspec"
                     (("`git ls-files -z 2>/dev/null`")
                      "`git ls-files -z`")))))))
    (propagated-inputs (list ruby-date ruby-net-protocol))
    (synopsis "Ruby client api for Internet Message Access Protocol")
    (description "@code{Net::IMAP} implements Internet Message Access
Protocol (IMAP) client functionality.  The protocol is described in
@url{https://tools.ietf.org/html/rfc3501, IMAP}.")
    (home-page "https://github.com/ruby/net-imap")
    (license license:bsd-2)))

(define-public ruby-net-pop
  (package
    (name "ruby-net-pop")
    (version "0.1.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ruby/net-pop")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "05vz6a56va2rbr7ld78gdzwy1j5mzs98cz82ax5aqa83pzzk6jld"))))
    (build-system ruby-build-system)
    (propagated-inputs (list ruby-net-protocol))
    (synopsis "Ruby client library for POP3")
    (description "This library provides functionality for retrieving email via
POP3, the Post Office Protocol version 3, as specified by
@url{http://www.ietf.org/rfc/rfc1939.txt, RFC1939}.")
    (home-page "https://github.com/ruby/net-pop")
    (license license:bsd-2)))

(define-public ruby-net-smtp
  (package
    (name "ruby-net-smtp")
    (version "0.3.3")
    (source (origin
              (method git-fetch)        ;for tests
              (uri (git-reference
                    (url "https://github.com/ruby/net-smtp")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0ca2wh45xvc09rv6v6sz3vbnkzrjzk5c4l6dk50zk4dwxvghma8r"))))
    (build-system ruby-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'regenerate-certificate
            ;; On version 0.5.0 a Makefile was introduced to regenerated
            ;; the certificates, and instead of calling openssl directory
            ;; we could do (with-directory-excursion "test/net/fixtures"
            ;; (invoke "make" "regen_certs"). However the certificate is
            ;; expired versions before 0.5.0 as well.
            (lambda _
              (with-directory-excursion "test/net/fixtures"
                (invoke
                 "openssl" "req" "-new" "-key" "server.key" "-out"
                 "server.csr" "-subj"
                 "/C=JP/ST=Shimane/O=Ruby Core Team/OU=Ruby Test/CN=localhost")
                (invoke "openssl" "req" "-new" "-x509" "-days" "3650"
                        "-key" "server.key" "-out" "cacert.pem" "-subj"
                        (string-append
                         "/C=JP/ST=Shimane/L=Matz-e city/O=Ruby "
                         "Core Team/CN=Ruby Test "
                         "CA/emailAddress=security@ruby-lang.org"))
                (invoke "openssl" "x509" "-days" "3650" "-CA" "cacert.pem"
                        "-CAkey" "server.key" "-set_serial" "00" "-in"
                        "server.csr" "-req" "-out" "server.crt")))))))
    (native-inputs (list openssl))
    (propagated-inputs (list ruby-net-protocol))
    (synopsis "Simple Mail Transfer Protocol client library for Ruby")
    (description "This library provides functionality to send Internet mail
via SMTP, the Simple Mail Transfer Protocol.  The SMTP protocol specification
is known as @url{http://www.ietf.org/rfc/rfc2821.txt, RFC2821}.")
    (home-page "https://github.com/ruby/net-smtp")
    (license license:bsd-2)))

(define-public ruby-pleaserun
  (package
    (name "ruby-pleaserun")
    (version "0.0.32")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "pleaserun" version))
              (sha256
               (base32
                "1aykf0l8327bqkkf5xd9jcglsib973zpy37cfnlf4j0vp0cdpn2d"))))
    (build-system ruby-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-after 'extract-gemspec 'relax-requirements
                 (lambda _
                   (substitute* "pleaserun.gemspec"
                     ;; Mustache is pinned at 0.99.8, for portability with
                     ;; older Rubies.
                     (("dependency\\(%q<mustache>.freeze.*")
                      "dependency(%q<mustache>.freeze)\n"))))
               (replace 'check
                 (lambda* (#:key tests? #:allow-other-keys)
                   (when tests?
                     ;; The cli_spec.rb test fails non-deterministically with
                     ;; a Errno::EISDIR error (see:
                     ;; https://github.com/jordansissel/pleaserun/issues/155)
                     (invoke "rspec" "--exclude-pattern" "cli_spec.rb")))))))
    (native-inputs (list ruby-flores ruby-rspec))
    (propagated-inputs (list ruby-cabin
                             ruby-clamp
                             ruby-dotenv
                             ruby-insist
                             ruby-mustache
                             ruby-stud))
    (synopsis "Init scripts and service definitions generation tool")
    (description "Pleaserun is a tool to generate startup scripts and service
definitions.  It targets service managers such as systemd, Upstart, launchd,
sysv init, and runit.")
    (home-page "https://github.com/jordansissel/pleaserun")
    (license license:asl2.0)))

(define-public ruby-power-assert
  (package
    (name "ruby-power-assert")
    (version "1.1.5")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "power_assert" version))
              (sha256
               (base32
                "1dii0wkfa0jm8sk9b20zl1z4980dmrjh0zqnii058485pp3ws10s"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f))                    ; No included tests
    (native-inputs
     (list bundler))
    (synopsis "Assert library with descriptive assertion messages")
    (description "Power-assert is an assertion library providing descriptive
assertion messages for tests.")
    (home-page "https://github.com/k-tsj/power_assert")
    (license (list license:bsd-2 license:ruby))))

(define-public ruby-powerpack
  (package
    (name "ruby-powerpack")
    (version "0.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "powerpack" version))
       (sha256
        (base32
         "1f71axvlhnxja0k17qqxdi4qh5ck807hqg4i3j6cgy8fgzmyg7rg"))))
    (build-system ruby-build-system)
    (arguments
     (list #:test-target "spec"))
    (native-inputs
     (list bundler ruby-rspec ruby-yard))
    (synopsis "Useful extensions to core Ruby classes")
    (description
     "This package provides a few useful extensions to core Ruby classes,
including @code{Array}, @code{Enumerable}, @code{Hash}, @code{Numeric}, and
@code{String}.")
    (home-page "https://github.com/bbatsov/powerpack")
    (license license:expat)))

(define-public ruby-locale
  (package
    (name "ruby-locale")
    (version "2.1.2")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "locale" version))
              (sha256
               (base32
                "1sls9bq4krx0fmnzmlbn64dw23c4d6pz46ynjzrn9k8zyassdd0x"))))
    (build-system ruby-build-system)
    ;; ruby-test-unit is required to run tests, but that needs ruby-packnga,
    ;; which needs ruby-gettext, which needs ruby-locale.  To break the
    ;; dependency cycle we disable tests.
    (arguments `(#:tests? #f))
    (native-inputs
     (list bundler ruby-yard/minimal))
    (synopsis "Ruby library providing basic localization APIs")
    (description
     "Ruby-Locale is the pure ruby library which provides basic APIs for
localization.")
    (home-page "https://github.com/ruby-gettext/locale")
    (license (list license:lgpl3+ license:ruby))))

(define-public ruby-temple
  (package
    (name "ruby-temple")
    (version "0.10.0")
    (source (origin
              (method git-fetch)        ;for tests
              (uri (git-reference
                    (url "https://github.com/judofyr/temple")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0rr9fnlcgj9nm3b6hzzjsvcw8x3y7z48j7slk7xxff2mh8s7y3y0"))))
    (build-system ruby-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-after 'replace-git-ls-files 'replace-more-git-ls-files
                 (lambda _
                   (substitute* "temple.gemspec"
                     ;; There no longer are test, spec or features
                     ;; directories.
                     ((".*`git ls-files -- \\{test,spec,features}/\\*`.*")
                      "")
                     ;; There isn't any bin directory either.
                     ((".*`git ls-files -- bin/\\*`.*")
                      ""))))
               (replace 'check
                 (lambda* (#:key tests? #:allow-other-keys)
                   (when tests?
                     (invoke "rspec")))))))
    (native-inputs (list ruby-bacon ruby-erubi ruby-rspec ruby-tilt))
    (synopsis "Template compilation framework in Ruby")
    (description "Temple is an abstraction and framework for compiling
templates to pure Ruby.")
    (home-page "https://github.com/judofyr/temple")
    (license license:expat)))

(define-public ruby-text
  (package
    (name "ruby-text")
    (version "1.3.1")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "text" version))
              (sha256
               (base32
                "1x6kkmsr49y3rnrin91rv8mpc3dhrf3ql08kbccw8yffq61brfrg"))))
    (build-system ruby-build-system)
    (synopsis "Collection of text algorithms for Ruby")
    (description
     "This package provides a collection of text algorithms: Levenshtein,
Soundex, Metaphone, Double Metaphone, Porter Stemming.")
    (home-page "https://github.com/threedaymonk/text")
    (license license:expat)))

(define-public ruby-gettext
  (package
    (name "ruby-gettext")
    (version "3.4.4")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "gettext" version))
              (sha256
               (base32
                "11hlxkk2yr9wjwd3nf8kgmsjcd8wf35yqlxi9wpvrgmvrk9n9a2k"))))
    (build-system ruby-build-system)
    ;; ruby-test-unit is required to run tests, but that needs ruby-packnga,
    ;; which needs ruby-gettext.  To break the dependency cycle we disable
    ;; tests.
    (arguments `(#:tests? #f))
    (propagated-inputs
     (list ruby-locale ruby-text ruby-erubi))
    (native-inputs
     (list bundler ruby-yard/minimal))
    (synopsis "GNU gettext-like program for Ruby")
    (description
     "Gettext is a GNU gettext-like program for Ruby.  The catalog
file (po-file) used is the same as that used by GNU gettext, allowing you to
use GNU gettext tools for maintenance.")
    (home-page "https://ruby-gettext.github.com/")
    (license (list license:lgpl3+ license:ruby))))

(define-public ruby-packnga
  (package
    (name "ruby-packnga")
    (version "1.0.4")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "packnga" version))
              (sha256
               (base32
                "1vv2j0i43s4xid2km5hgrrxqlqpwgq8nlm8kaxfg2531c1vwfsd4"))))
    (build-system ruby-build-system)
    ;; ruby-test-unit is required to run tests, but that needs ruby-packnga.
    ;; To break the dependency cycle we disable tests.
    (arguments `(#:tests? #f))
    (propagated-inputs
     (list ruby-gettext ruby-yard))
    (native-inputs
     (list bundler))
    (synopsis "Utility library to package internationalized libraries")
    (description
     "Packnga is a library to translate to many languages using YARD.")
    (home-page "https://ranguba.org/packnga/")
    (license license:lgpl2.0+)))

(define-public ruby-test-construct
  (package
    (name "ruby-test-construct")
    (version "2.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "test_construct" version))
       (sha256
        (base32
         "17q7rw92l7r4zh6rkvzrn4dyl8p8p77217vaa1wf7nsv8k5541vy"))))
    (build-system ruby-build-system)
    (native-inputs
     (list bundler ruby-mocha-1 ruby-rspec))
    (synopsis "Creates temporary files and directories for testing")
    (description
     "TestConstruct is a @acronym{DSL, Domain Specific Language} for creating
temporary files and directories during tests.")
    (home-page "https://github.com/bhb/test_construct")
    (license license:expat)))

(define-public ruby-test-unit
  (package
    (name "ruby-test-unit")
    (version "3.6.0")
    (source (origin
              (method git-fetch)        ;for tests
              (uri (git-reference
                    (url "https://github.com/test-unit/test-unit")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0w1m432q3y5v9lkak8yyxadak3z17bsp6afni97i4zjdgfz7niz2"))))
    (build-system ruby-build-system)
    (propagated-inputs
     (list ruby-power-assert))
    (native-inputs
     (list bundler ruby-packnga ruby-yard))
    (synopsis "Unit testing framework for Ruby")
    (description "@code{Test::Unit} is unit testing framework for Ruby, based
on xUnit principles.  These were originally designed by Kent Beck, creator of
extreme programming software development methodology, for Smalltalk's SUnit.
It allows writing tests, checking results and automated testing in Ruby.")
    (home-page "https://test-unit.github.io/")
    (license (list license:psfl license:ruby))))

(define-public ruby-test-unit/minimal
  (hidden-package
   (package
     (inherit ruby-test-unit)
     (arguments
      (ensure-keyword-arguments
       (package-arguments ruby-test-unit)
       (list #:tests? #f)))
     (native-inputs '()))))

(define-public ruby-test-unit-ruby-core
  (package
    (name "ruby-test-unit-ruby-core")
    (version "1.0.5")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "test-unit-ruby-core" version))
       (sha256
        (base32 "1i7fa4hlj6xiqvjaikagwrmiyc21jzyswvd4grjbfqysziwsxygc"))))
    (build-system ruby-build-system)
    (arguments
     (list #:tests? #f))  ; contains no tests
    (synopsis "Additional test assertions for Ruby standard libraries")
    (description "This package provides additional test assertions for Ruby
standard libraries.")
    (home-page "https://github.com/ruby/test-unit-ruby-core")
    (license license:ruby)))

(define-public ruby-mapping
  (package
    (name "ruby-mapping")
    (version "1.1.1")
    (source (origin
              (method git-fetch)        ;for tests
              (uri (git-reference
                    (url "https://github.com/ioquatix/mapping")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0yhmqp8mprjqf9m7wzc4hhi50qbfax86r89w852csns0ijaffjjs"))))
    (build-system ruby-build-system)
    (arguments
     (list
      #:test-target "spec"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch
            (lambda _
              (substitute* "spec/mapping/model_spec.rb"
                ;; From https://github.com/ioquatix/mapping/pull/2
                (("offset:") "offset =")))))))
    (native-inputs (list ruby-rspec))
    (synopsis "Map model objects based on their class to a given output model")
    (description "The @code{mapping} gem maps model objects based on their
class to a given output model.  It is useful for versioning external
interfaces (e.g. JSON APIs) or processing structured data from one format to
another.")
    (home-page "https://github.com/ioquatix/mapping")
    (license license:expat)))

(define-public ruby-markaby
  (package
    (name "ruby-markaby")
    (version "0.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "markaby" version))
       (sha256
        (base32
         "1j4jc31ycydbkh5h3q6zwidzpavg3g5mbb5lqyaczd3jrq78rd7i"))))
    (build-system ruby-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          ;; Run rspec manually without using the Rakefile, as the versions of
          ;; Rake and RSpec 2 are incompatible:
          ;;
          ;; NoMethodError: undefined method `last_comment'
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              ;; Skip some broken tests, this should be redundant with the
              ;; next release
              (delete-file "spec/markaby/markaby_test_unit_spec.rb")
              (substitute* "spec/markaby/markaby_spec.rb"
                (("generated.should == str")
                 "# Test broken: generated.should == str"))

              (when tests?
                (invoke "rspec")))))))
    (propagated-inputs
     (list ruby-builder))
    (native-inputs
     (list bundler ruby-rspec-2))
    (synopsis "Write HTML pages in pure Ruby")
    (description
     "Markaby allows writing HTML packages in pure Ruby.  This is similar to
the functionality provided by @acronym{ERB, Embedded Ruby}, but without the
mixture of HTML and additional ERB syntax.")
    (home-page "https://markaby.github.io/")
    (license license:expat)))

(define-public ruby-maruku
  (package
    (name "ruby-maruku")
    (version "0.7.3")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "maruku" version))
       (sha256
        (base32
         "1r7bxpgnx2hp3g12bjrmdrpv663dfqxsdp0af69kjhxmaxpia56x"))))
    (build-system ruby-build-system)
    (arguments
     '(;; TODO: 3 tests seem to fail due to HTML encoding issues
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "rspec"))
             #t)))))
    (native-inputs
     (list ruby-rspec ruby-simplecov ruby-nokogiri-diff))
    (synopsis "Markdown interpreter in Ruby")
    (description
     "Maruku is a Markdown interpreter in Ruby.  It can export Markdown to
HTML, and PDF through LaTeX.")
    (home-page "https://github.com/bhollis/maruku")
    (license license:expat)))

(define-public ruby-metaclass
  (package
    (name "ruby-metaclass")
    (version "0.0.4")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "metaclass" version))
              (sha256
               (base32
                "0hp99y2b1nh0nr8pc398n3f8lakgci6pkrg4bf2b2211j1f6hsc5"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'add-test-unit-to-search-path
           (lambda* (#:key inputs #:allow-other-keys)
             (let* ((test-unit (assoc-ref inputs "ruby-test-unit")))
               (substitute* "Rakefile"
                 (("t\\.libs << \"test\"" line)
                  (string-append line "; t.libs << \""
                                 test-unit "/lib/ruby/vendor_ruby"
                                 "/gems/test-unit-"
                                 ,(package-version ruby-test-unit)
                                 "/lib\""))))
             #t)))))
    (native-inputs
     (list bundler ruby-test-unit/minimal))
    (synopsis "Ruby library adding metaclass method to all objects")
    (description
     "Metaclass is a Ruby library adding a @code{metaclass} method to all Ruby
objects.")
    (home-page "https://github.com/floehopper/metaclass")
    (license license:expat)))

(define-public ruby-mkmf-lite
  (package
    (name "ruby-mkmf-lite")
    (version "0.5.2")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "mkmf-lite" version))
              (sha256
               (base32
                "0rqa5kzswhqkj7r9mqrqz4mjd2vdxsblgybb52gj3mwr1gwvl4c5"))))
    (build-system ruby-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          ;; Avoid rubocop dependency
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "rspec")))))))
    (propagated-inputs
     (list ruby-ptools))
    (native-inputs
     (list ruby-rspec))
    (synopsis "Lightweight alternative to @code{mkmf}")
    (description
     "@code{mkmf-lite} is a light version of Ruby's @code{mkmf.rb} designed
for use as a library.  It does not create packages, builds, or log files of
any kind.  Instead, it provides mixin methods that you can use in FFI or tests
to check for the presence of header files, constants, and so on.")
    (home-page "https://github.com/djberg96/mkmf-lite")
    (license license:asl2.0)))

(define-public ruby-msgpack
  (package
    (name "ruby-msgpack")
    (version "1.6.1")
    (source (origin
              (method git-fetch)        ;for tests
              (uri (git-reference
                    (url "https://github.com/msgpack/msgpack-ruby")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "08wi853nv02clrdwx8s6dg9lmcyzq5fk84l4rb94pglps76rlvz7"))))
    (build-system ruby-build-system)
    (arguments (list #:test-target "spec"))
    (native-inputs
     (list ruby-rake-compiler
           ruby-ruby-memcheck
           ruby-rspec
           ruby-yard))
    (synopsis "Efficient object serialization library for Ruby")
    (description "MessagePack is a binary-based efficient object serialization
library.  It enables to exchange structured objects between many languages
like JSON.  Unlike JSON, it is very fast and small.")
    (home-page "https://msgpack.org/")
    (license license:asl2.0)))

(define-public ruby-mspec
  (package
    (name "ruby-mspec")
    (version "1.9.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "mspec" version))
       (sha256
        (base32
         "0wmyh2n40m4srwdx9z6h6g6p46k02pzyhcsja3hqcw5h5b0hfmhd"))))
    (build-system ruby-build-system)
    (arguments
     '(;; TODO: 3 test failures
       ;; ./spec/mocks/mock_spec.rb:82
       ;; ./spec/utils/name_map_spec.rb:151
       ;; ./spec/utils/name_map_spec.rb:155
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'extract-gemspec 'change-dependency-constraints
           (lambda _
             (substitute* "mspec.gemspec"
               (("rake.*") "rake>)\n")
               (("rspec.*") "rspec>)\n"))
             #t))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "rspec" "spec"))
             #t)))))
    (native-inputs
     (list bundler ruby-rake ruby-rspec))
    (synopsis "MSpec is a specialized framework for RubySpec")
    (description
     "MSpec is a specialized framework that is syntax-compatible with RSpec 2
for basic features.  MSpec contains additional features that assist in writing
specs for Ruby implementations in ruby/spec.")
    (home-page "http://rubyspec.org")
    (license license:expat)))

(define-public ruby-mysql2
  (package
    (name "ruby-mysql2")
    (version "0.5.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/brianmario/mysql2")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "11lvfgc2rmvkm52jp0nbi6pvhk06klznghr7llldfw8basl9n5wv"))))
    (build-system ruby-build-system)
    (arguments
     '(;; TODO: Tests require a running MySQL/MariaDB service
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'replace-git-ls-files
           (lambda _
             (substitute* "mysql2.gemspec"
               (("git ls-files .*`") "find . -type f |sort`"))
             #t))
         (add-before 'install 'set-MAKEFLAGS
           (lambda* (#:key outputs #:allow-other-keys)
             (setenv "MAKEFLAGS"
                     (string-append
                      "V=1 "
                      "prefix=" (assoc-ref outputs "out")))
             #t))
         ;; Move the 'check phase to after 'install, as then you can test
         ;; using the installed mysql2 gem in the store.
         (delete 'check)
         (add-after 'install 'check
           (lambda* (#:key outputs tests? #:allow-other-keys)
             (setenv "GEM_PATH"
                     (string-append
                      (getenv "GEM_PATH")
                      ":"
                      (assoc-ref outputs "out") "/lib/ruby/vendor_ruby"))
             (when tests?
               (invoke "rspec"))
             #t)))))
    (inputs
     `(("mariadb-dev" ,mariadb "dev")
       ("zlib" ,zlib)))
    (native-inputs
     (list ruby-rspec ruby-rake-compiler))
    (synopsis "MySQL library for Ruby, binding to libmysql")
    (description
     "This package provides a simple, fast MySQL library for Ruby, binding to
libmysql.")
    (home-page "https://github.com/brianmario/mysql2")
    (license license:expat)))

(define-public ruby-blankslate
  (package
    (name "ruby-blankslate")
    (version "3.1.3")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "blankslate" version))
              (sha256
               (base32
                "0fwkb4d1j9gc7vdwn2nxvwgy2g5wlag4c4bp7bl85jvq0kgp6cyx"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
          (lambda _ (invoke "rspec" "spec/"))))))
    (native-inputs
     (list bundler ruby-rspec))
    (synopsis "Abstract base class with no predefined methods")
    (description
     "BlankSlate provides an abstract base class with no predefined
methods (except for @code{__send__} and @code{__id__}).  BlankSlate is useful
as a base class when writing classes that depend upon
@code{method_missing} (e.g. dynamic proxies).")
    (home-page "https://github.com/masover/blankslate")
    (license license:expat)))

(define-public ruby-bond
  (package
    (name "ruby-bond")
    (version "0.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "bond" version))
       (sha256
        (base32
         "1r19ifc4skyl2gxnifrxa5jvbbay9fb2in79ppgv02b6n4bhsw90"))))
    (build-system ruby-build-system)
    (arguments
     ;; The test suite fails (see:
     ;; https://github.com/cldwalker/bond/issues/46).
     `(#:tests? #f))
    (native-inputs
     (list ruby-bacon ruby-bacon-bits ruby-mocha-on-bacon))
    (synopsis "Bond can provide custom autocompletion for arguments, methods
and more")
    (description
     "Bond can autocomplete argument(s) to methods, uniquely completing per
module, per method and per argument.  Bond provides a configuration system and
a DSL for creating custom completions and completion rules.  Bond can also
load completions that ship with gems.  Bond is able to offer more than irb's
completion since it uses the full line of input when completing as opposed to
irb's last-word approach.")
    (home-page "http://tagaholic.me/bond/")
    (license license:expat)))

(define-public ruby-idn-ruby
  (package
    (name "ruby-idn-ruby")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "idn-ruby" version))
       (sha256
        (base32
         "07vblcyk3g72sbq12xz7xj28snpxnh3sbcnxy8bglqbfqqhvmawr"))))
    (build-system ruby-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (delete 'check)
         (add-after 'install 'check
           (lambda* (#:key tests? outputs #:allow-other-keys)
             (when tests?
               (let* ((gem-file (cadr (find-files "." "\\.gem")))
                      (name-and-version (basename gem-file ".gem")))
                 (apply invoke
                        "ruby" "--verbose"
                        (string-append "-I"
                                       (assoc-ref outputs "out")
                                       "/lib/ruby/vendor_ruby/gems/"
                                       name-and-version
                                       "/lib")
                        (find-files "./test" ".*\\.rb"))))
             #t)))))
    (inputs
     (list libidn))
    (synopsis "Ruby Bindings for the GNU LibIDN library")
    (description
     "Ruby Bindings for the GNU LibIDN library, an implementation of the
Stringprep, Punycode and IDNA specifications.  These are used to encode and
decode internationalized domain + names according to the IDNA2003
specifications.

Included are the most important parts of the Stringprep, Punycode and IDNA
APIs like performing Stringprep processings, encoding to and decoding from
Punycode strings and converting entire domain names to and from the ACE
encoded form.")
    (home-page "https://github.com/deepfryed/idn-ruby")
    (license license:asl2.0)))

(define-public ruby-insist
  (package
    (name "ruby-insist")
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "insist" version))
              (sha256
               (base32
                "0bw3bdwns14mapbgb8cbjmr0amvwz8y72gyclq04xp43wpp5jrvg"))))
    (build-system ruby-build-system)
    (arguments (list #:phases #~(modify-phases %standard-phases
                                  (replace 'check
                                    (lambda* (#:key tests? #:allow-other-keys)
                                      (when tests?
                                        (invoke "ruby" "test/testing.rb")))))))
    (synopsis "Testing tool for Ruby")
    (description "This package provides a simple block-driven assertion
library for both testing and for production code that attempts to make test
definitions more readable.")
    (home-page "https://github.com/jordansissel/ruby-insist/")
    (license license:asl2.0)))

(define-public ruby-introspection
  (package
    (name "ruby-introspection")
    (version "0.0.4")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "introspection" version))
              (sha256
               (base32
                "1y2nbijkc0zlfmn9ss6588ilarq2kbn2i7w7pwwsli66dj84zgca"))))
    (build-system ruby-build-system)
    (propagated-inputs
     (list ruby-metaclass))
    (native-inputs
     (list bundler
           ruby-blankslate))
    (synopsis "Dynamic inspection of the method hierarchy on a Ruby object")
    (description
     "Introspection provides tools to inspect the hierarchy of method
definitions on a Ruby object.")
    (home-page "https://github.com/floehopper/introspection")
    (license license:expat)))

(define-public ruby-redcarpet
  (package
    (name "ruby-redcarpet")
    (version "3.5.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "redcarpet" version))
              (sha256
               (base32
                "0skcyx1h8b5ms0rp2zm3ql6g322b8c1adnkwkqyv7z3kypb4bm7k"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; The gem archive does not include the conformance tests.
         (add-after 'unpack 'disable-conformance-tests
          (lambda _
            (substitute* "Rakefile"
              (("task :test => %w\\[test:unit test:conformance\\]")
               "task :test => %w[test:unit]"))
            #t)))))
    (native-inputs
     (list bundler ruby-test-unit ruby-rake-compiler))
    (synopsis "Extensible Markdown to (X)HTML converter")
    (description
     "Redcarpet is an extensible Ruby library for Markdown processing and
conversion to (X)HTML.")
    (home-page "https://github.com/vmg/redcarpet")
    (license license:expat)))

(define-public ruby-reline
  (package
    (name "ruby-reline")
    (version "0.3.3")
    (source (origin
              (method git-fetch)        ;for tests
              (uri (git-reference
                    (url "https://github.com/ruby/reline")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1papa4f4prjml9qk6yydi4k5a4zgbzmxmbhd8fz9kfg1i34s35cw"))))
    (build-system ruby-build-system)
    (propagated-inputs (list ruby-io-console))
    (synopsis "GNU Readline or Editline implementation in Ruby")
    (description "Reline is a pure Ruby alternative GNU Readline or Editline
implementation.")
    (home-page "https://github.com/ruby/reline")
    (license (list license:bsd-2 license:ruby)))) ;dual license

(define-public ruby-rerun
  (package
  (name "ruby-rerun")
  (version "0.13.0")
  (source
    (origin
      (method url-fetch)
      (uri (rubygems-uri "rerun" version))
      (sha256
        (base32
          "1cskvxk8z8vmfail8na7hj91hs0qnvds9nydj04zi3dbddgnbmvz"))))
  (build-system ruby-build-system)
  (arguments
   '(#:tests? #f)) ; No included tests
  (propagated-inputs
   (list ruby-listen))
  (synopsis "Run a process, and restart when some monitored files change")
  (description
    "Rerun is a tool to launch programs, then monitor the file system, and
restart the program when any of the monitored files change.  It's written in
Ruby, but can be used for all programs.")
  (home-page "https://github.com/alexch/rerun/")
  (license license:expat)))

(define-public ruby-maxitest
  (package
    (name "ruby-maxitest")
    (version "5.1.0")
    (home-page "https://github.com/grosser/maxitest")
    (source (origin
              ;; Pull from git because the gem does not contain tests.
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0qj410krfm497ggmf71xpnabbb6814y0585by4nlzyjvg9hpgg3m"))))
    (build-system ruby-build-system)
    (arguments
     (list
      #:test-target "default"
      #:phases
      #~(modify-phases %standard-phases
          (replace 'replace-git-ls-files
            (lambda _
              (substitute* "maxitest.gemspec"
                (("`git ls-files lib/ bin/ MIT-LICENSE Readme.md`")
                 "`find lib/ bin/ MIT-LICENSE Readme.md -type f | sort`"))))
          (add-before 'check 'remove-version-constraints
            (lambda _
              ;; Don't use specific versions of dependencies, instead
              ;; take whatever is available in Guix.
              (delete-file "Gemfile.lock")))
          (add-before 'check 'add-mtest-on-PATH
            (lambda _
              ;; Tests use 'mtest' which is not automatically added on
              ;; PATH.
              (setenv "PATH" (string-append (getcwd) "/bin:"
                                            (getenv "PATH"))))))))
    (native-inputs
     (list procps
           ruby-bump
           ruby-rspec
           ruby-wwtd))
    (propagated-inputs
     (list ruby-minitest))
    (synopsis "Minitest with extra features")
    (description
     "Maxitest is a wrapper around Minitest with extra functionality such
as timeouts, an @command{mtest} executable that can run tests by line
number, support for interrupted tests, better backtraces, and more.")
    (license license:expat)))

(define-public ruby-mocha
  (package
    (name "ruby-mocha")
    (version "2.0.4")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "mocha" version))
              (sha256
               (base32
                "18xn9gm9yypavy9yck71fplan19hy5697mwd1rwzz7vizh3ip7bd"))))
    (build-system ruby-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'remove-rubocop-dependency
            (lambda _
              ;; Disable dependency on Rubocop, which is just a linter,
              ;; and would introduce a circular dependency.
              (substitute* "Gemfile"
                ((".*rubocop.*") "")))))))
    (propagated-inputs
     (list ruby-ruby2-keywords))
    (native-inputs
     (list ruby-psych-3
           ruby-introspection))
    (synopsis "Mocking and stubbing library for Ruby")
    (description
     "Mocha is a mocking and stubbing library with JMock/SchMock syntax, which
allows mocking and stubbing of methods on real (non-mock) classes.")
    (home-page "https://mocha.jamesmead.org/")
    ;; Mocha can be used with either license at the users choice.
    (license (list license:expat license:ruby))))

(define-public ruby-mocha-1
  (package
    (inherit ruby-mocha)
    (version "1.13.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "mocha" version))
              (sha256
               (base32
                "15s53ggsykk69kxqvs4416s8yxdhz6caggva55n8sjgy4ixzwp10"))))
    (arguments
     '(#:tests? #f))))

(define-public ruby-mocha-on-bacon
  (package
    (name "ruby-mocha-on-bacon")
    (version "0.2.3")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "mocha-on-bacon" version))
       (sha256
        (base32
         "1h49b33rq889hn8x3wp9byczl91va16jh1w4d2wyy4yj23icdrcp"))))
    (build-system ruby-build-system)
    (arguments
     ;; rubygems.org release missing tests
     '(#:tests? #f))
    (propagated-inputs (list ruby-mocha))
    (synopsis "Mocha adapter for Bacon")
    (description
     "This package provides a Mocha adapter for Bacon, allowing you to use the
Mocha stubbing and mocking library with Bacon, a small RSpec clone.")
    (home-page
     "https://github.com/alloy/mocha-on-bacon")
    (license license:expat)))

(define-public ruby-net-ssh
  (package
    (name "ruby-net-ssh")
    (version "7.1.0")
    (source (origin
              (method git-fetch) ;for tests
              (uri (git-reference
                    (url "https://github.com/net-ssh/net-ssh")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1dkbyzpl31jygnnva5sa754vk42q1fih4qz5ipqw5gqiafrrlb91"))))
    (build-system ruby-build-system)
    (native-inputs
     (list bundler
           ruby-bcrypt-pbkdf
           ruby-ed25519
           ruby-mocha
           ruby-rbnacl
           ruby-test-unit
           ruby-x25519))
    (synopsis "Ruby implementation of the SSH2 client protocol")
    (description "@code{Net::SSH} is a pure-Ruby implementation of the SSH2
client protocol.  It allows you to write programs that invoke and interact
with processes on remote servers, via SSH2.")
    (home-page "https://github.com/net-ssh/net-ssh")
    (license license:expat)))

(define-public ruby-net-scp
  (package
    (name "ruby-net-scp")
    (version "4.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/net-ssh/net-scp")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1mdxh81z2hkcc359g6z96fywbr57azlv2yj4zq76adn5lyqq4hgw"))))
    (build-system ruby-build-system)
    (native-inputs
     (list bundler ruby-test-unit ruby-mocha-1))
    (propagated-inputs
     (list ruby-net-ssh))
    (synopsis "Pure-Ruby SCP client library")
    (description "@code{Net::SCP} is a pure-Ruby implementation of the SCP
client protocol.")
    (home-page "https://github.com/net-ssh/net-scp")
    (license license:expat)))

(define-public ruby-net-sftp
  (package
    (name "ruby-net-sftp")
    (version "4.0.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "net-sftp" version))
              (sha256
               (base32
                "0r33aa2d61hv1psm0l0mm6ik3ycsnq8symv7h84kpyf2b7493fv5"))))
    (build-system ruby-build-system)
    (propagated-inputs (list ruby-net-ssh))
    (synopsis "Pure Ruby implementation of the SFTP client protocol")
    (description
     "@code{Net::SFTP} is a pure Ruby implementation of the SFTP
protocol (specifically, versions 1 through 6 of the SFTP protocol).  Note that
this is the “Secure File Transfer Protocol”, typically run over an SSH
connection, and has nothing to do with the FTP protocol.")
    (home-page "https://github.com/net-ssh/net-sftp")
    (license license:expat)))

(define-public ruby-minima
  (package
    (name "ruby-minima")
    (version "2.5.1")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "minima" version))
              (sha256
               (base32
                "1gk7jmriiswda1ykjzpsw9cpiya4m9n0yrh0h6xnrc8zcfy543jj"))))
    (arguments
     (list #:phases #~(modify-phases %standard-phases
                        (replace 'check
                          (lambda* (#:key tests? #:allow-other-keys)
                            (when tests?
                              (invoke "jekyll" "build"))
                            ;; Without the following, an attempt to remove
                            ;; minima-<version>.gem is made during installation,
                            ;; which will fail.
                            (delete-file #$(string-append "_site/minima-"
                                                          version ".gem")))))))
    (build-system ruby-build-system)
    (propagated-inputs (list jekyll ruby-jekyll-feed ruby-jekyll-seo-tag))
    (synopsis "Beautiful, minimal theme for Jekyll")
    (description
     "Minima is a one-size-fits-all Jekyll theme for writers.  It's Jekyll's
default (and first) theme.  It's what you get when you run @code{jekyll new}.")
    (home-page "https://github.com/jekyll/minima")
    (license license:expat)))

(define-public ruby-minitest
  (package
    (name "ruby-minitest")
    (version "5.18.1")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "minitest" version))
              (sha256
               (base32
                "1kg9wh7jlc9zsr3hkhpzkbn0ynf4np5ap9m2d8xdrb8shy0y6pmb"))))
    (build-system ruby-build-system)
    (native-inputs (list ruby-hoe))
    (home-page "https://github.com/minitest/minitest")
    (synopsis "Small test suite library for Ruby")
    (description "Minitest provides a complete suite of Ruby testing
facilities supporting TDD, BDD, mocking, and benchmarking.")
    (license license:expat)))

;; This is the last release of Minitest 4, which is used by some packages.
(define-public ruby-minitest-4
  (package
    (inherit ruby-minitest)
    (version "4.7.5")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "minitest" version))
              (sha256
               (base32
                "03p6iban9gcpcflzp4z901s1hgj9369p6515h967ny6hlqhcf2iy"))))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-unsupported-method
            (lambda _
              (substitute* "Rakefile"
                (("self\\.rubyforge_name = .*") ""))))
          (add-after 'build 'patch-tests
            (lambda _
              ;; test_no_method_error_on_unexpected_methods
              ;; This test fails due to some extra information in the message
              (substitute* "test/minitest/test_minitest_mock.rb"
                (("assert_equal expected, e.message")
                 "assert_equal expected, e.message.lines.first.strip"))
              ;; Some tests are failing on Ruby 2.4 due to the deprecation of
              ;; Fixnum.
              (delete-file "test/minitest/test_minitest_spec.rb"))))))
    (native-inputs
     (list ruby-minitest
           ruby-hoe))))

(define-public ruby-minitest-around
  (package
    (name "ruby-minitest-around")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "minitest-around" version))
       (sha256
        (base32
         "15ywnqx0719jl9c25yqfshmwcir57i5f4hr1ra9v9vay9ylcwndr"))))
    (build-system ruby-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'extract-gemspec 'remove-unnecessary-dependency-versions
           (lambda _
             (substitute* "minitest-around.gemspec"
               (("%q<cucumber>.*") "%q<cucumber>, [\">= 0\"])\n"))
             #t)))))
    (propagated-inputs
     (list ruby-minitest))
    (native-inputs
     (list bundler ruby-cucumber ruby-bump ruby-test-construct))
    (synopsis "Run code around tests in Minitest")
    (description
     "This library provides a way to run code around tests in Minitest,
written using either the unit test or spec style.")
    (home-page "https://github.com/splattael/minitest-around")
    (license license:expat)))

(define-public ruby-minitest-retry
  (package
    (name "ruby-minitest-retry")
    (version "0.2.2")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "minitest-retry" version))
              (sha256
               (base32
                "1c1zq0b1a9y0hbqphq4ybijnjldlis1g6kyhx92q2ynnqfmzhfga"))))
    (build-system ruby-build-system)
    (propagated-inputs (list ruby-minitest))
    (synopsis "Minitest extension to re-run failing tests")
    (description "This package provides the @code{Minitest::Retry} class,
which extends Minitest to allow retrying tests when they fail.")
    (home-page "https://github.com/y-yagi/minitest-retry")
    (license license:expat)))

(define-public ruby-minitest-sprint
  (package
    (name "ruby-minitest-sprint")
    (version "1.1.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "minitest-sprint" version))
              (sha256
               (base32
                "179d6pj56l9xzm46fqsqj10mzjkr1f9fv4cxa8wvchs97hqz33w1"))))
    (build-system ruby-build-system)
    (native-inputs
     (list ruby-hoe ruby-minitest))
    (synopsis "Fast test suite runner for minitest")
    (description "Minitest-sprint is a test runner for minitest that makes it
easier to re-run individual failing tests.")
    (home-page "https://github.com/seattlerb/minitest-sprint")
    (license license:expat)))

(define-public ruby-minitest-stub-const
  (package
    (name "ruby-minitest-stub-const")
    (version "0.6")                     ;for Rakefile
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/adammck/minitest-stub-const")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0cz4r9fxplx94c7wakx0998n2gv7l21prn8pfpw6z2c33k3g2xar"))))
    (build-system ruby-build-system)
    (synopsis "Stub constants for the duration of a block in MiniTest")
    (description "This package provides a MiniTest extension to stub constants
for the duration of a block in MiniTest.")
    (home-page "https://github.com/adammck/minitest-stub-const")
    (license license:expat)))

(define-public ruby-minitest-bacon
  (package
    (name "ruby-minitest-bacon")
    (version "1.0.3")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "minitest-bacon" version))
              (sha256
               (base32
                "0zhdwcl6bgha61qiyfvr7zs7ywaxc33wmj9xhxl8jdmpdvifvfaj"))))
    (build-system ruby-build-system)
    (native-inputs
     (list ruby-hoe))
    (inputs
     (list ruby-minitest))
    (synopsis "Bacon compatibility library for minitest")
    (description "Minitest-bacon extends minitest with bacon-like
functionality, making it easier to migrate test suites from bacon to minitest.")
    (home-page "https://github.com/seattlerb/minitest-bacon")
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

(define-public ruby-minitest-power-assert
  (package
    (name "ruby-minitest-power-assert")
    (version "0.3.1")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "minitest-power_assert" version))
              (sha256
               (base32
                "1dazl6nbxjvvmi5jamrsygkz396s29b7cd841ni6qy4izk8jd9b7"))))
    (build-system ruby-build-system)
    (propagated-inputs (list ruby-minitest ruby-power-assert))
    (synopsis "Power Assert implementation for Minitest")
    (description "This gem provides a Power Assert implementation for
Minitest.  It is inspired by the @code{test-unit-power_assert} gem.")
    (home-page "https://github.com/hsbt/minitest-power_assert")
    (license license:bsd-2)))

(define-public ruby-minitest-profile
  (package
    (name "ruby-minitest-profile")
    (version "0.0.2")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "minitest-profile" version))
              (sha256
               (base32
                "13h4nwbq6yv7hsaa7dpj90lry4rc5qqnpzvm9n2s57mm2xi31xfa"))))
    (build-system ruby-build-system)
    (synopsis "Display the slowest tests in a MiniTest suite")
    (description "This package provides a MiniTest plugin for displaying the
slowest tests in a minitest suite.")
    (home-page "https://github.com/nmeans/minitest-profile")
    (license license:expat)))

(define-public ruby-minitest-pretty-diff
  ;; Use git reference because gem is out of date and does not contain testing
  ;; script.  There are no releases on GitHub.
  (let ((commit "11f32e930f574225432f42e5e1ef6e7471efe572"))
    (package
      (name "ruby-minitest-pretty-diff")
      (version (string-append "0.1-1." (string-take commit 8)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/adammck/minitest-pretty_diff")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "13y5dhmcckhzd83gj1nfwh41iykbjcm2w7y4pr6j6rpqa5as122r"))))
      (build-system ruby-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (replace 'check
             (lambda* (#:key tests? #:allow-other-keys)
               (when tests?
                 (invoke "sh" "script/test")))))))
      (native-inputs
       (list bundler ruby-turn))
      (synopsis "Pretty-print hashes and arrays in MiniTest")
      (description
       "@code{minitest-pretty_diff} monkey-patches
@code{MiniTest::Assertions#mu_pp} to pretty-print hashes and arrays before
diffing them.  This makes it easier to spot differences between nested
structures when tests fail.")
      (home-page "https://github.com/adammck/minitest-pretty_diff")
      (license license:expat))))

(define-public ruby-minitest-proveit
  (package
    (name "ruby-minitest-proveit")
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "minitest-proveit" version))
              (sha256
               (base32
                "0k1hpr8lgkgygfivgcsnnib7xjlf9ribgpn7yidvb4q0l0q2yfmr"))))
    (build-system ruby-build-system)
    (native-inputs (list ruby-hoe))
    (propagated-inputs (list ruby-minitest))
    (synopsis "Assertion-based tests extension for MiniTest")
    (description "The @code{minitest-proveit} MiniTest extension ensures all
tests to prove success (via at least one assertion) rather than rely on the
absence of failure.")
    (home-page "https://github.com/seattlerb/minitest-proveit")
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
               (("require 'byebug'") ""))
             #t)))))
    (native-inputs
     (list bundler ruby-minitest))
    (synopsis "Extra features and changes to MiniTest")
    (description "@code{MiniTest Moar} add some additional features and
changes some default behaviours in MiniTest.  For instance, Moar replaces the
MiniTest @code{Object#stub} with a global @code{stub} method.")
    (home-page "https://github.com/dockyard/minitest-moar")
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

(define-public ruby-minitest-reporters
  (package
    (name "ruby-minitest-reporters")
    (version "1.3.6")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "minitest-reporters" version))
       (sha256
        (base32
         "1a3das80rwgys5rj48i5ly144nvszyqyi748bk9bss74jblcf5ay"))))
    (build-system ruby-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         ;; Remove the requirement on Rubocop, as it isn't useful to run, and
         ;; including it as an input can lead to circular dependencies.
         (add-after 'unpack 'remove-rubocop-from-Rakefile
           (lambda _
             (substitute* "Rakefile"
               (("require 'rubocop/rake\\_task'") "")
               (("RuboCop::RakeTask\\.new\\(:rubocop\\)") "[].each"))
             #t))
         (add-after 'extract-gemspec 'remove-rubocop-from-gemspec
           (lambda _
             (substitute* "minitest-reporters.gemspec"
               ((".*%q<rubocop>.*") "\n"))
             #t)))))
    (propagated-inputs
     (list ruby-ansi ruby-builder ruby-minitest ruby-progressbar))
    (native-inputs
     (list bundler ruby-maruku))
    (synopsis "Enhanced reporting for Minitest tests")
    (description
     "@code{minitest/reporters} provides a custom Minitest runner to improve
how the test state is reported.  A number of different reporters are
available, including a spec reporter, progress bar reporter, a HTML
reporter.")
    (home-page "https://github.com/kern/minitest-reporters")
    (license license:expat)))

(define-public ruby-minitest-rg
  (package
    (name "ruby-minitest-rg")
    (version "5.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "minitest-rg" version))
       (sha256
        (base32
         "0sq509ax1x62rd0w10b0hcydcxyk5bxxr3fwrgxv02r8drq2r354"))))
    (build-system ruby-build-system)
    (arguments
     ;; Some tests fail even outside Guix, so disable tests.
     ;; https://github.com/blowmage/minitest-rg/issues/12
     ;; https://github.com/blowmage/minitest-rg/pull/13
     `(#:tests? #f))
    (propagated-inputs
     (list ruby-minitest))
    (synopsis "Coloured output for Minitest")
    (description
     "@code{minitest-rg} changes the colour of the output from Minitest.")
    (home-page "https://blowmage.com/minitest-rg/")
    (license license:expat)))

(define-public ruby-minitest-global-expectations
  (package
    (name "ruby-minitest-global-expectations")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "minitest-global_expectations"
                          version))
       (sha256
        (base32
         "1pp3k2608spj4kvqy2y16hs18an917g6vwgvphrfhjviac83090x"))))
    (build-system ruby-build-system)
    (propagated-inputs
     (list ruby-minitest))
    (synopsis "Adjust minitest behaviour for calling expectation methods")
    (description
     "Minitest-global_expectations allows continued use of expectation methods
on all objects.  Calling expectation methods on all objects was deprecated in
minitest 5.12, and is planned to be removed from minitest 6.")
    (home-page "https://github.com/jeremyevans/minitest-global_expectations")
    (license license:expat)))

(define-public ruby-minitest-hooks
  (package
    (name "ruby-minitest-hooks")
    (version "1.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "minitest-hooks" version))
       (sha256
        (base32
         "05z8r6sw3fz4s44fs1150ndlcmcy82vlxmhps5nncg8vk59k3gmf"))))
    (build-system ruby-build-system)
    (arguments
     '(#:test-target "spec"
       ;; Test suite is incompatible with ruby-2.7.
       ;; https://github.com/jeremyevans/minitest-hooks/issues/19
       #:tests? #f))
    (native-inputs
     (list ruby-sequel ruby-sqlite3))
    (synopsis "Hooks for the minitest framework")
    (description
     "Minitest-hooks adds @code{around}, @code{before_all}, @code{after_all},
@code{around_all} hooks for Minitest.  This allows, for instance, running each
suite of specs inside a database transaction, running each spec inside its own
savepoint inside that transaction.  This can significantly speed up testing
for specs that share expensive database setup code.")
    (home-page "https://github.com/jeremyevans/minitest-hooks")
    (license license:expat)))

(define-public ruby-daemons
  (package
    (name "ruby-daemons")
    (version "1.2.5")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "daemons" version))
              (sha256
               (base32
                "15smbsg0gxb7nf0nrlnplc68y0cdy13dm6fviavpmw7c630sring"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f)) ; no test suite
    (synopsis "Daemonize Ruby programs")
    (description "Daemons provides a way to wrap existing Ruby scripts to be
run as a daemon and to be controlled by simple start/stop/restart commands.")
    (home-page "https://github.com/thuehlinger/daemons")
    (license license:expat)))

(define-public ruby-dalli
  (package
    (name "ruby-dalli")
    (version "3.2.4")
    (source (origin
              (method git-fetch)        ;for tests
              (uri (git-reference
                    (url "https://github.com/petergoldstein/dalli")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1hhqqv1j3zd2y0vr6maaabsflrvkf1x0i6z8n725mhqydp3f9gwp"))))
    (build-system ruby-build-system)
    ;; Disable tests to avoid a dependency cycle with ruby-activesupport,
    ;; through ruby-ruby-prof.
    (arguments (list #:tests? #f))
    (synopsis "High performance memcached client for Ruby")
    (description "Dalli is a high performance pure Ruby client for accessing
memcached servers.  Dalli supports:

@itemize
@item Simple and complex memcached configurations
@item Fail-over between memcached instances
@item Fine-grained control of data serialization and compression
@item Thread-safe operation
@item SSL/TLS connections to memcached
@item SASL authentication.
@end itemize

The name is a variant of Salvador Dali for his famous painting The Persistence
of Memory.")
    (home-page "https://github.com/petergoldstein/dalli")
    (license license:expat)))

(define-public ruby-data_uri
  (package
    (name "ruby-data_uri")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "data_uri" version))
       (sha256
        (base32
         "0fzkxgdxrlbfl4537y3n9mjxbm28kir639gcw3x47ffchwsgdcky"))))
    (build-system ruby-build-system)
    (synopsis "URI class for parsing data URIs")
    (description
     "Data @acronym{URI, universal resource identifier}s allow resources to be
embedded inside a URI.  The URI::Data class provides support for parsing these
URIs using the normal URI.parse method.")
    (home-page "https://github.com/dball/data_uri")
    (license license:expat)))

(define-public ruby-debug
  (package
    (name "ruby-debug")
    (version "1.7.2")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "debug" version))
              (sha256
               (base32
                "0x59508j69w9p275gabysv521n210pd3n060gqfgsiqjms1h0ldf"))))
    (build-system ruby-build-system)
    (arguments (list #:test-target "test_all"))
    (propagated-inputs (list ruby-irb ruby-reline))
    (synopsis "Debugging functionality for Ruby")
    (description "Debugging functionality for Ruby.  This is completely
rewritten debug.rb which was contained by the ancient Ruby versions.  It is
included with Ruby itself, but this package is made available so that the
latest version can be made available independently from Ruby.")
    (home-page "https://github.com/ruby/debug")
    (license license:bsd-2)))

(define-public ruby-deep-merge
  (package
    (name "ruby-deep-merge")
    (version "1.2.1")
    (home-page "https://github.com/danielsdeleo/deep_merge")
    ;; The Rubygem source does not contain the gemspec required for tests.
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page) (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0c9rk23ilhc0n4489y6lda2wzphpzh6ish6fahlbpjhxn82wb931"))))
    (build-system ruby-build-system)
    (native-inputs
     (list ruby-minitest))
    (synopsis "Recursively merge hashes")
    (description
     "Deep Merge is a set of utility functions for @code{Hash}.  It permits
you to merge elements inside a hash together recursively.")
    (license license:expat)))

(define-public ruby-delayed-job
  (package
    (name "ruby-delayed-job")
    (version "4.1.11")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "delayed_job" version))
              (sha256
               (base32
                "0s2xg72ljg4cwmr05zi67vcyz8zib46gvvf7rmrdhsyq387m2qcq"))))
    (build-system ruby-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'disable-bundler
                 (lambda _
                   (substitute* "Rakefile"
                     (("require 'bundler/setup'") "")
                     (("Bundler::GemHelper\\.install_tasks") ""))))
               (add-after 'unpack 'disable-rubocop
                 (lambda _
                   (substitute* "Rakefile"
                     (("require 'rubocop/rake_task'") "")
                     (("RuboCop::RakeTask.new") ""))))
               (add-after 'extract-gemspec 'remove-dependency-on-actionmailer
                 (lambda _
                   (substitute* "spec/helper.rb"
                     (("require 'action_mailer'") ""))
                   (substitute* "delayed_job.gemspec"
                     (("\"spec/performable_mailer_spec.rb\".freeze, ") ""))
                   (delete-file "spec/performable_mailer_spec.rb"))))))
    (native-inputs
     (list ruby-activerecord
           ruby-rspec
           ruby-simplecov
           ruby-simplecov-lcov
           ruby-zeitwerk
           ruby-mini-portile-2))
    (propagated-inputs
     (list ruby-activesupport))
    (synopsis "Asynchronous background tasks execution library")
    (description "Delayed_job (or DJ) encapsulates the common pattern of
asynchronously executing longer tasks in the background.  It is a direct
extraction from Shopify where the job table is responsible for a multitude of
core tasks.")
    (home-page "https://github.com/collectiveidea/delayed_job")
    (license license:expat)))

(define-public ruby-git
  (package
    (name "ruby-git")
    (version "1.3.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "git" version))
              (sha256
               (base32
                "1waikaggw7a1d24nw0sh8fd419gbf7awh000qhsf411valycj6q3"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f ; no tests
       #:phases (modify-phases %standard-phases
                  (add-after 'install 'patch-git-binary
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      ;; Make the default git binary an absolute path to the
                      ;; store.
                      (let ((git    (search-input-file inputs "/bin/git"))
                            (config (string-append
                                     (assoc-ref outputs "out")
                                     "/lib/ruby/vendor_ruby/gems/git-"
                                     ,version "/lib/git/config.rb")))
                        (substitute* (list config)
                          (("'git'")
                           (string-append "'" git "'")))
                        #t))))))
    (inputs
     (list git))
    (synopsis "Ruby wrappers for Git")
    (description "Ruby/Git is a Ruby library that can be used to create, read
and manipulate Git repositories by wrapping system calls to the git binary.")
    (home-page "https://github.com/schacon/ruby-git")
    (license license:expat)))

(define-public ruby-hocon
  (package
    (name "ruby-hocon")
    (version "1.4.0")
    (home-page "https://github.com/puppetlabs/ruby-hocon")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page) (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "04wgv0pwrghawnl6qp346z59fvp9v37jymq8p0lsrzxa6nvrykmk"))))
    (build-system ruby-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (if tests?
                  (invoke "rspec")
                  (format #t "test suite not run~%")))))))
    (native-inputs
     (list bundler ruby-rspec))
    (synopsis "HOCON config library")
    (description
     "This package provides Ruby support for the @acronym{HOCON,
Human-Optimized Config Object Notation} configuration file format.  It
supports parsing and modifying HOCON and JSON files, and rendering parsed
objects back to a @code{String}.")
    (license license:asl2.0)))

(define-public ruby-slop
  (package
    (name "ruby-slop")
    (version "4.10.1")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "slop" version))
              (sha256
               (base32
                "1iyrjskgxyn8i1679qwkzns85p909aq77cgx2m4fs5ygzysj4hw4"))))
    (build-system ruby-build-system)
    (synopsis "Ruby command line option parser")
    (description "Slop provides a Ruby domain specific language for gathering
options and parsing command line flags.")
    (home-page "https://github.com/leejarvis/slop")
    (license license:expat)))

(define-public ruby-slop-3
  (package (inherit ruby-slop)
    (version "3.6.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "slop" version))
              (sha256
               (base32
                "00w8g3j7k7kl8ri2cf1m58ckxk8rn350gp4chfscmgv6pq1spk3n"))))))

(define-public ruby-multi-xml
  (package
    (name "ruby-multi-xml")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "multi_xml" version))
       (sha256
        (base32
         "0lmd4f401mvravi1i1yq7b2qjjli0yq7dfc4p1nj5nwajp7r6hyj"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ; No included tests
    (synopsis "Swappable XML backends for Ruby")
    (description
     "@code{MultiXml} provides swappable XML backends utilizing either LibXML,
Nokogiri, Ox, or REXML.")
    (home-page "https://github.com/sferik/multi_xml")
    (license license:expat)))

(define-public ruby-multipart-parser
  (package
    (name "ruby-multipart-parser")
    (version "0.1.1")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "multipart-parser" version))
              (sha256
               (base32
                "0xb4p475yrfm883h9kn80a021myn17dvs50wpa1djzcmlq7p0882"))))
    (build-system ruby-build-system)
    (arguments
     (list
      #:phases #~(modify-phases %standard-phases
                   (add-after 'unpack 'skip-failing-test
                     ;; One test fails for unknown reasons (see:
                     ;; https://github.com/danabr/multipart-parser/issues/7).
                     (lambda _
                       (substitute* "test/multipart_parser/reader_test.rb"
                         (("def test_long" all)
                          (string-append all "\n      return true"))))))))
    (synopsis "Parser for multipart MIME messages")
    (description "@code{multipart-parser} is a simple parser for multipart
MIME messages, written in Ruby, based on felixge/node-formidable's parser.  It
has the following characteristics:
@itemize
@item Pure Ruby
@item Event-driven API
@item Only supports one level of multipart parsing
@item Does not perform I/O
@item Does not depend on any other library.
@end itemize")
    (home-page "https://github.com/danabr/multipart-parser")
    (license license:expat)))

(define-public ruby-multipart-post
  (package
    (name "ruby-multipart-post")
    (version "2.0.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "multipart-post" version))
              (sha256
               (base32
                "09k0b3cybqilk1gwrwwain95rdypixb2q9w65gd44gfzsd84xi1x"))))
    (build-system ruby-build-system)
    (native-inputs
     (list bundler))
    (synopsis "Multipart POST library for Ruby")
    (description "Multipart-Post Adds multipart POST capability to Ruby's
net/http library.")
    (home-page "https://github.com/nicksieger/multipart-post")
    (license license:expat)))

(define-public ruby-multi-json
  (package
    (name "ruby-multi-json")
    (version "1.15.0")
    (source
     (origin
       (method git-fetch)
       ;; Tests are not distributed at rubygems.org so download from GitHub
       ;; instead.
       (uri (git-reference
              (url "https://github.com/intridea/multi_json")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0mkdvy6i00yyksjvnv6znh7wf89j9506qzzjq6bsbmbkyqrszp4d"))))
    (build-system ruby-build-system)
    (arguments
     (list
      ;; TODO Tests don't currently work with Ruby 3
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'pre-check
            (lambda _
              ;; As seen in the .travis.yml file.
              (setenv "SKIP_ADAPTERS" "gson,jr_jackson,nsjsonserialization"))))))
    (native-inputs
     (list ruby-rspec ruby-json-pure ruby-oj ruby-yajl-ruby))
    (synopsis "Common interface to multiple JSON libraries for Ruby")
    (description
     "This package provides a common interface to multiple JSON libraries,
including Oj, Yajl, the JSON gem (with C-extensions), the pure-Ruby JSON gem,
NSJSONSerialization, gson.rb, JrJackson, and OkJson.")
    (home-page "https://github.com/intridea/multi_json")
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

(define-public ruby-arr-pm
  (package
    (name "ruby-arr-pm")
    (version "0.0.12")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "arr-pm" version))
              (sha256
               (base32
                "0fddw0vwdrr7v3a0lfqbmnd664j48a9psrjd3wh3k4i3flplizzx"))))
    (build-system ruby-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (replace 'check
                 (lambda* (#:key tests? #:allow-other-keys)
                   (when tests?
                     (invoke "rspec")))))))
    (native-inputs (list ruby-flores ruby-insist ruby-rspec ruby-stud))
    (synopsis "RPM reader/writer library written in Ruby")
    (description "This library allows reading and writing RPM packages.  It is
used by the @command{fpm} tool.  It is written in pure Ruby because
@code{librpm} is not available on all systems and requires many
dependencies.")
    (home-page "https://github.com/jordansissel/ruby-arr-pm")
    (license license:asl2.0)))

(define-public ruby-arel
  (package
    (name "ruby-arel")
    (version "9.0.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "arel" version))
              (sha256
               (base32
                "1jk7wlmkr61f6g36w9s2sn46nmdg6wn2jfssrhbhirv5x9n95nk0"))))
    (build-system ruby-build-system)
    (arguments '(#:tests? #f)) ; no tests
    (home-page "https://github.com/rails/arel")
    (synopsis "SQL AST manager for Ruby")
    (description "Arel is an SQL @dfn{Abstract Syntax Tree} (AST) manager for
Ruby.  It simplifies the generation of complex SQL queries and adapts to
various relational database implementations.")
    (license license:expat)))

(define-public ruby-minitar
  ;; We package from the GitHub source to fix the security issue reported at
  ;; https://github.com/halostatue/minitar/issues/16.
  (let ((commit "e25205ecbb6277ae8a3df1e6a306d7ed4458b6e4"))
    (package
      (name "ruby-minitar")
      (version (string-append "0.5.4-1." (string-take commit 8)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/halostatue/minitar")
               (commit commit)))
         (file-name (string-append name "-" version "-checkout"))
         (sha256
          (base32
           "1iywfx07jgjqcmixzkxk9zdwfmij1fyg1z2jlwzj15cj7s99qlfv"))))
      (build-system ruby-build-system)
      (arguments
       '(#:tests? #f)) ; missing a gemspec
      (synopsis "Ruby library and utility for handling tar archives")
      (description
       "Archive::Tar::Minitar is a pure-Ruby library and command-line utility
that provides the ability to deal with POSIX tar archive files.")
      (home-page "http://www.github.com/atoulme/minitar")
      (license (list license:gpl2+ license:ruby)))))

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

(define-public ruby-nokogiri
  (package
    (name "ruby-nokogiri")
    (version "1.15.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/sparklemotion/nokogiri")
                    (commit "a6ad20b3edc8f020043ccfe5d9ec6ae9af103720")))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1n0vlg6v58jw3qzgyihz1dh5fvp4js1qgdh75j0kn47nvyiw3jxj"))
              (patches (search-patches "ruby-nokogiri.patch"))))
    (build-system ruby-build-system)
    (arguments
     (list #:gem-flags #~(list "--" "--use-system-libraries"
                               (string-append "--with-xml2-include="
                                              #$(this-package-input "libxml2")
                                              "/include/libxml2"))
           #:phases #~(modify-phases %standard-phases
                        (add-after 'install 'delete-mkmf.log
                          (lambda _
                            ;; Rubygems installs build log files that embed volatile file
                            ;; names (see:
                            ;; https://github.com/rubygems/rubygems/issues/6259).
                            (for-each delete-file
                                      (find-files #$output "^mkmf\\.log$"))))
                        (delete 'check)
                        (add-after 'install 'check
                          (lambda* (#:key tests? #:allow-other-keys)
                            (setenv "GEM_PATH" (string-append
                                                (getenv "GEM_PATH") ":"
                                                #$output "/lib/ruby/vendor_ruby"))
                            (when tests?
                              (for-each (lambda (file)
                                          (invoke "ruby" "-Itest" file))
                                        (find-files "test" "^test_.*\\.rb"))))))))
    (native-inputs (list ruby-hoe ruby-rubyzip))
    (inputs (list zlib libxml2 libxslt))
    (propagated-inputs (list ruby-mini-portile-2 ruby-pkg-config))
    (synopsis "HTML, XML, SAX, and Reader parser for Ruby")
    (description "Nokogiri (鋸) parses and searches XML/HTML, and features
both CSS3 selector and XPath 1.0 support.")
    (home-page "https://nokogiri.org/")
    (license license:expat)))

(define-public ruby-method-source
  (package
    (name "ruby-method-source")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "method_source" version))
       (sha256
        (base32
         "1pnyh44qycnf9mzi1j6fywd5fkskv3x7nmsqrrws0rjn5dd4ayfp"))))
    (build-system ruby-build-system)
    (arguments
     `(#:test-target "spec"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-git-ls-files
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "Rakefile"
               (("git ls-files") "find . -type f"))
             #t)))))
    (native-inputs
     (list ruby-rspec))
    (synopsis "Retrieve the source code for Ruby methods")
    (description "Method_source retrieves the source code for Ruby methods.
Additionally, it can extract source code from Proc and Lambda objects or just
extract comments.")
    (home-page "https://github.com/banister/method_source")
    (license license:expat)))

(define-public ruby-coderay
  (package
    (name "ruby-coderay")
    (version "1.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "coderay" version))
       (sha256
        (base32
         "15vav4bhcc2x3jmi3izb11l4d9f3xv8hp2fszb7iqmpsccv1pz4y"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ; missing test files
    (synopsis "Ruby syntax highlighting library")
    (description "Coderay is a Ruby library that provides syntax highlighting
for select languages.")
    (home-page "http://coderay.rubychan.de")
    (license license:expat)))

(define-public ruby-cuke-modeler
  (package
    (name "ruby-cuke-modeler")
    (version "3.19.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/enkessler/cuke_modeler")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0bizla3k124lj4r7f2k5cdfm2sawzd6rdmb6rgbkbng2fygxsjib"))))
    (build-system ruby-build-system)
    (arguments
     (list #:test-target "default"
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'relax-requirements
                 (lambda _
                   (substitute* "Gemfile"
                     ;; Cucumber appears pinned to an older version for no
                     ;; particular reason (see:
                     ;; https://github.com/enkessler/cuke_modeler/issues/14).
                     (("'cucumber', '2.2.0'")
                      "'cucumber', '>= 2.2.0'"))
                   ;; Disable Bundler.
                   (substitute* "bin/console"
                     (("require 'bundler/setup'") ""))
                   (substitute* "rakefiles/testing_tasks.rb"
                     (("'bundle', 'exec', ") ""))
                   ;; Remove extraneous dependencies.
                   (substitute* "cuke_modeler.gemspec"
                     ((".*bundler.*") "")
                     ((".*rubocop.*") "")
                     ((".*yard.*") ""))
                   (substitute* "Rakefile"
                     (("Rainbow.enabled = true") "")
                     (("require_relative 'rakefiles/documentation_tasks'") "")
                     (("require_relative 'rakefiles/other_tasks'") "")
                     (("require_relative 'rakefiles/release_tasks'") "")))))))
    (native-inputs
     (list ruby-childprocess
           ruby-cucumber
           ruby-rainbow
           ruby-rspec
           ruby-simplecov
           ruby-simplecov-lcov))
    (propagated-inputs (list ruby-cucumber-gherkin))
    (synopsis "Gherkin test suite analysis tool")
    (description "CukeModeler facilitates modeling a test suite that is
written in Gherkin (e.g.  Cucumber, SpecFlow, Lettuce, etc.).  It does this by
providing an abstraction layer on top of the Abstract Syntax Tree (AST) that
the @code{cucumber-gherkin} generates when parsing features, as well as
providing models for feature files and directories in order to be able to have
a fully traversable model tree of a test suite's structure.  These models can
then be analyzed or manipulated more easily than the underlying AST layer.")
    (home-page "https://github.com/enkessler/cuke_modeler")
    (license license:expat)))

(define-public ruby-parallel-tests
  (package
    (name "ruby-parallel-tests")
    (version "4.2.0")
    (home-page "https://github.com/grosser/parallel_tests")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (file-name (string-append name version))
              (sha256
               (base32
                "00gbg5q36ayspkzd6r0kg4gk46lsw9s6misx14rczxaf9kqcdrmv"))))
    (build-system ruby-build-system)
    (arguments
     '(#:test-target "spec"             ;avoid rubocop dependency
       #:phases (modify-phases %standard-phases
                  (add-after 'patch-source-shebangs 'patch-shell-invocations
                    (lambda _
                      (substitute* '("lib/parallel_tests/tasks.rb"
                                     "spec/parallel_tests/tasks_spec.rb")
                        (("/bin/sh") (which "sh"))
                        (("/bin/bash") (which "bash")))))
                  (add-before 'check 'relax-requirements
                    (lambda _
                      ;; Remove hard coded version constraints, instead just
                      ;; use whatever versions are available in Guix.
                      (delete-file "Gemfile.lock")
                      (substitute* "Gemfile"
                        (("'minitest',.*")
                         "'minitest'\n")
                        (("'cucumber',.*")
                         "'cucumber'\n")
                        ;; Do not depend on a git-fetched spinach version.
                        (("gem 'spinach',.*")
                         "gem 'spinach'\n")
                        ((".*rubocop.*") ""))))
                  (add-before 'check 'disable-rails-test
                    (lambda _
                      ;; XXX: This test attempts to download and run the test
                      ;; suites of multiple Rails versions(!) directly.
                      (delete-file "spec/rails_spec.rb")))
                  (add-before 'check 'disable-problematic-tests
                    (lambda _
                      ;; This test fails, probably because of the newer
                      ;; Cucumber version used here.
                      (delete-file "spec/parallel_tests/cucumber/\
failure_logger_spec.rb")                      ))
                  (add-before 'check 'set-HOME
                    (lambda _
                      ;; Some tests check the output of Bundler, and fail when
                      ;; Bundler warns that /homeless-shelter does not exist.
                      (setenv "HOME" "/tmp"))))))
    (native-inputs
     (list ruby-bump
           ruby-cucumber
           ruby-cuke-modeler
           ruby-minitest
           ruby-rake
           ruby-rspec
           ruby-spinach))
    (propagated-inputs
     (list ruby-parallel))
    (synopsis "Run tests in parallel")
    (description
     "This package can speed up @code{Test::Unit}, @code{RSpec},
@code{Cucumber}, and @code{Spinach} tests by running them concurrently
across multiple CPU cores.")
    (license license:expat)))

(define-public ruby-parser
  (package
    (name "ruby-parser")
    (version "3.2.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "parser" version))
       (sha256
        (base32
         "0s5afi89p76k8vpwiqvh343pm5l23ijqlpszhz65afym3zpkxhzx"))))
    (build-system ruby-build-system)
    (arguments '(#:tests? #f))          ; tests not included in gem
    (native-inputs (list bundler ruby-cliver ruby-simplecov ruby-racc))
    (inputs (list ragel))
    (propagated-inputs (list ruby-ast))
    (synopsis "Ruby parser written in pure Ruby")
    (description
     "This package provides a Ruby parser written in pure Ruby.")
    (home-page "https://github.com/whitequark/parser")
    (license license:expat)))

(define-public ruby-set
  (package
    (name "ruby-set")
    (version "1.0.3")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "set" version))
              (sha256
               (base32
                "07kc057nrkddrybqmlbmgf9x7nsmbc3ni6gy1z6xjx5b838vlj33"))))
    (build-system ruby-build-system)
    (synopsis
     "Ruby class to deal with collections of unordered, unique values")
    (description
     "This package provides a class to deal with collections of unordered,
unique values")
    (home-page "https://github.com/ruby/set")
    (license license:bsd-2)))

(define-public ruby-sexp-processor
  (package
    (name "ruby-sexp-processor")
    (version "4.17.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "sexp_processor" version))
       (sha256
        (base32
         "0vzz9mhg4kkdqf179pm30i204h7iskanxrk53j0csf0qrrs4iajd"))))
    (build-system ruby-build-system)
    (native-inputs
     (list ruby-hoe
           ruby-minitest
           ruby-minitest-proveit))
    (synopsis "ParseTree fork which includes generic S-exp processing tools")
    (description "The sexp_processor package is derived from ParseTree, but
contrary to ParseTree, it includes all the generic S-exp processing tools.
Amongst the included tools are @code{Sexp}, @code{SexpProcessor} and
@code{Environment}")
    (home-page "https://github.com/seattlerb/sexp_processor")
    (license license:expat)))

(define-public ruby-ruby-parser
  (package
    (name "ruby-ruby-parser")
    (version "3.20.2")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "ruby_parser" version))
       (sha256
        (base32
         "0q851n8654wkjrq8jawq8vi5yhr1y9vpyr2vj7cnn3sa4ikg6d3z"))))
    (build-system ruby-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch
            (lambda _
              (substitute* "Rakefile"
                (("`which bison`")
                 (string-append "\"" (which "bison") "\""))
                (("which unifdef")
                 (which "unifdef"))))))))
    (native-inputs
     (list ruby-hoe
           ruby-racc
           unifdef
           bison
           ruby-minitest))
    (propagated-inputs
     (list ruby-sexp-processor))
    (home-page "https://github.com/seattlerb/ruby_parser/")
    (synopsis "Ruby parser written in pure Ruby")
    (description "The ruby_parser (RP) package provides a Ruby parser written
in pure Ruby.  It outputs S-expressions which can be manipulated and converted
back to Ruby via the @code{ruby2ruby} library.")
    (license license:expat)))

(define-public ruby-ruby-version
  (package
    (name "ruby-ruby-version")
    (version "1.0.2")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "ruby_version" version))
              (sha256
               (base32
                "0lvc7bd5ps3w2vq2wb02i0pi3vfcx2rnckx2ix4rjym1qf52kb2j"))))
    (build-system ruby-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-after 'extract-gemspec 'delete-gem-files
                 ;; There are some pre-built files in the source, and
                 ;; registered in the .gemspec (see:
                 ;; https://github.com/janlelis/ruby_version/issues/1).
                 (lambda _
                   (delete-file-recursively "pkg")
                   (substitute* "ruby_version.gemspec"
                     (("\"pkg/ruby_version-1.0.0.gem\".freeze, ")
                      "")
                     (("\"pkg/ruby_version-1.0.1.gem\".freeze, ")
                      ""))))
               (add-after 'extract-gemspec 'relax-requirements
                 (lambda _
                   (delete-file "Gemfile.lock")
                   (substitute* "ruby_version.gemspec"
                     (("\"Gemfile.lock\".freeze, ") "")
                     ;; Allow a newers versions of development dependencies.
                     (("~>") ">=")))))))
    (native-inputs (list ruby-rdoc ruby-rubygems-tasks ruby-rspec))
    (synopsis "Ruby class for checking the Ruby version")
    (description "This package provides a @code{RubyVersion} class which
offers a convenient Domain Specific Language (DSL) for checking for the right
Ruby version.")
    (home-page "https://github.com/janlelis/ruby_version")
    (license license:expat)))

(define-public ruby-prawn-manual-builder
  (package
    (name "ruby-prawn-manual-builder")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "prawn-manual_builder" version))
       (sha256
        (base32 "1vlg5w7wq43g2hgpgra2nrcxj1kb4ayqliz4gmja2rhs037j2vzs"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f                      ; no included tests
       #:phases
       (modify-phases %standard-phases
         (add-after 'extract-gemspec 'patch-gemspec
           (lambda _
             (substitute* ".gemspec"
               ;; Loosen the requirement for pdf-inspector
               (("~> 1\\.0\\.7") ">= 0")))))))
    (propagated-inputs
     (list ruby-coderay))
    (synopsis "Tool for writing manuals for Prawn and Prawn accessories")
    (description
     "This package provides a tool for writing manuals for Prawn and Prawn
accessories")
    (home-page "https://github.com/prawnpdf/prawn-manual_builder")
    (license %prawn-project-licenses)))

(define-public ruby-progress_bar
  (package
    (name "ruby-progress_bar")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "progress_bar" version))
       (sha256
        (base32
         "1qc40mr6p1z9a3vlpnsg1zfgk1qswviql2a31y63wpv3vr6b5f48"))))
    (build-system ruby-build-system)
    (arguments
     '(#:test-target "spec"))
    (propagated-inputs
     (list ruby-highline ruby-options))
    (native-inputs
     (list bundler ruby-rspec ruby-timecop))
    (synopsis
     "Ruby library for displaying progress bars")
    (description
     "ProgressBar is a simple library for displaying progress bars.  The
maximum value is configurable, and additional information can be displayed
like the percentage completion, estimated time remaining, elapsed time and
rate.")
    (home-page "https://github.com/paul/progress_bar")
    (license license:wtfpl2)))

(define-public ruby-dep
  (package
    (name "ruby-dep")
    (version "1.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "ruby_dep" version))
       (sha256
        (base32
         "1c1bkl97i9mkcvkn1jks346ksnvnnp84cs22gwl0vd7radybrgy5"))))
    (build-system ruby-build-system)
    (arguments
     (list
      #:ruby ruby-2.7
      #:tests? #f)) ; No included tests
    (synopsis "Creates a version constraint of supported Rubies")
    (description
     "This package helps create a version constraint of supported Rubies,
suitable for a gemspec file.")
    (home-page "https://github.com/e2/ruby_dep")
    (license license:expat)))

(define-public ruby-progressbar
  (package
    (name "ruby-progressbar")
    (version "1.10.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "ruby-progressbar" version))
       (sha256
        (base32 "1k77i0d4wsn23ggdd2msrcwfy0i376cglfqypkk2q77r2l3408zf"))))
    (build-system ruby-build-system)
    (arguments
     '(;; TODO: There looks to be a circular dependency with ruby-fuubar.
       #:tests? #f))
    (synopsis "Text progress bar library for Ruby")
    (description
     "Ruby/ProgressBar is an flexible text progress bar library for Ruby.
The output can be customized with a formatting system.")
    (home-page "https://github.com/jfelchner/ruby-progressbar")
    (license license:expat)))

(define-public ruby-latest-ruby
  (package
    (name "ruby-latest-ruby")
    (version "3.1.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "latest_ruby" version))
              (sha256
               (base32
                "15rqwgxzpnkzdiz8m02jra0zq5sx0fiz61vkfrj1ls6slqfhnzqg"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ; No Rakefile
    (synopsis "Answers the question of what the latest Ruby version is")
    (description "Knows about MRI, Rubinius, JRuby, MagLev and MacRuby.")
    (home-page "https://github.com/kyrylo/latest_ruby")
    (license license:zlib)))

(define-public ruby-pry
  (package
    (name "ruby-pry")
    (version "0.14.2")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "pry" version))
       (sha256
        (base32
         "0k9kqkd9nps1w1r1rb7wjr31hqzkka2bhi8b518x78dcxppm9zn4"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ; no tests
    (propagated-inputs
     (list ruby-coderay ruby-method-source))
    (synopsis "Ruby REPL")
    (description "Pry is an IRB alternative and runtime developer console for
Ruby.  It features syntax highlighting, a plugin architecture, runtime
invocation, and source and documentation browsing.")
    (home-page "https://cobaltbluemedia.com/pryrepl/")
    (license license:expat)))

(define-public ruby-pry-doc
  (package
    (name "ruby-pry-doc")
    (version "1.4.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "pry-doc" version))
              (sha256
               (base32
                "1pp43n69p6bjvc640wgcz295w1q2v9awcqgbwcqn082dbvq5xvnx"))))
    (build-system ruby-build-system)
    (propagated-inputs (list ruby-pry ruby-yard))
    (native-inputs (list ruby-latest-ruby ruby-rspec ruby-rake)) ;for tests
    (synopsis "Provides YARD and extended documentation support for Pry")
    (description
     "Pry Doc is a Pry REPL plugin.  It provides extended documentation
support for the REPL by means of improving the @code{show-doc} and
@code{show-source} commands.  With help of the plugin the commands are
be able to display the source code and the docs of Ruby methods and
classes implemented in C.")
    (home-page "https://github.com/pry/pry-doc")
    (license license:expat)))

(define-public ruby-single-cov
  (package
    (name "ruby-single-cov")
    (version "1.9.1")
    (home-page "https://github.com/grosser/single_cov")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page)
                                  (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1w4k81f2mdg620m6pwkrqayddnz9mr8qx0myqn33mw8k6imfip05"))))
    (build-system ruby-build-system)
    (arguments
     '(#:test-target "spec"             ;to avoid rubocop requirement
       #:phases (modify-phases %standard-phases
                  (replace 'replace-git-ls-files
                    (lambda _
                      (substitute* "single_cov.gemspec"
                        (("`git ls-files lib/ bin/ MIT-LICENSE`")
                         ;; There no longer appear to be a 'bin'
                         ;; sub-directory.
                         "`find lib/ MIT-LICENSE -type f | sort`"))))
                  (add-before 'check 'remove-version-constraints
                    (lambda _
                      (delete-file "Gemfile.lock")))
                  (add-before 'check 'relax-requirements
                    (lambda _
                      ;; Remove extraneous requirements.
                      (substitute* "Rakefile"
                        ((".*require.*bump.*") ""))
                      (substitute* "Gemfile"
                        ((".*gem \"bump\".*") "")
                        ((".*gem \"rubocop\".*") ""))))
                  (add-before 'check 'disable-failing-test
                    (lambda _
                      ;; XXX: This test copies assets from minitest, but can
                      ;; not cope with the files being read-only.  Just skip
                      ;; it for now.
                      (substitute* "specs/single_cov_spec.rb"
                        (("it \"complains when coverage is bad\"")
                         "xit \"complains when coverage is bad\"")))))))
    (native-inputs (list ruby-minitest ruby-rspec ruby-simplecov))
    (synopsis "Code coverage reporting tool")
    (description
     "This package provides actionable code coverage reports for Ruby
projects.  It has very little overhead and can be easily integrated with
development tools to catch coverage problems early.")
    (license license:expat)))

(define-public ruby-oedipus-lex
  (package
    (name "ruby-oedipus-lex")
    (version "2.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "oedipus_lex" version))
       (sha256
        (base32
         "06l4d3l75vhdcmnavnkzr7bd39rb0njxhkbmwrw6ni64z2hlj7w7"))))
    (build-system ruby-build-system)
    (native-inputs
     (list ruby-hoe
           ruby-minitest))
    (synopsis "Ruby lexer")
    (description
     "Oedipus Lex is a lexer generator in the same family as Rexical and Rex.
It is based primarily on generating code much like you would a hand-written
lexer.  It uses StrScanner within a multi-level case statement.  As such,
Oedipus matches on the first match, not the longest.")
    (home-page "https://github.com/seattlerb/oedipus_lex")
    (license license:expat)))

(define-public ruby-guard
  (package
    (name "ruby-guard")
    (version "2.13.0")
    (source (origin
              (method git-fetch)
              ;; The gem does not include a Rakefile, nor does it contain a
              ;; gemspec file, nor does it come with the tests.  This is why
              ;; we fetch the tarball from Github.
              (uri (git-reference
                     (url "https://github.com/guard/guard")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "16pxcszr0g2jnl3090didxh1d8z5m2mly14m3w4rspb8fmclsnjs"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f ; tests require cucumber
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-git-ls-files
          (lambda* (#:key outputs #:allow-other-keys)
            (substitute* "guard.gemspec"
              (("git ls-files -z") "find . -type f -print0"))
            #t))
         (replace 'build
          (lambda _
            (invoke "gem" "build" "guard.gemspec"))))))
    (propagated-inputs
     (list ruby-formatador
           ruby-listen
           ruby-lumberjack
           ruby-nenv
           ruby-notiffany
           ruby-pry
           ruby-shellany
           ruby-thor))
    (native-inputs
     (list bundler ruby-rspec))
    (synopsis "Tool to handle events on file system modifications")
    (description
     "Guard is a command line tool to easily handle events on file system
modifications.  Guard automates various tasks by running custom rules whenever
file or directories are modified.")
    (home-page "https://guardgem.org/")
    (license license:expat)))

(define-public ruby-spinach
  (package
    (name "ruby-spinach")
    (version "0.11.0")
    (home-page "https://github.com/codegram/spinach")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "spinach" version))
              (sha256
               (base32
                "1mv053mqz9c8ngqa6wp1ymk2fax6j0yqzax6918akrdr7c3fx3c6"))))
    (build-system ruby-build-system)
    (arguments
       ;; FIXME: Disable tests altogether because they depend on 'capybara'
       ;; which in turn depends on many other unpackaged gems.  Enable once
       ;; capybara is available.
       '(#:tests? #f))
    (propagated-inputs
     (list ruby-colorize ruby-gherkin-ruby ruby-json))
    (synopsis "Gherkin-based BDD framework")
    (description
     "Spinach is a high-level @acronym{BDD, Behavior-driven development}
framework that leverages the expressive @code{Gherkin} language to help you
define executable specifications of your code.")
    (license license:expat)))

(define-public ruby-timers
  (package
    (name "ruby-timers")
    (version "4.3.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/socketry/timers")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1vvahlhk6i1xks1bsha6s64pjjxhagmzvvf1q9h6z3lpcba43rpx"))))
    (build-system ruby-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-before 'build 'prune-gems.rb
                 (lambda _
                   (substitute* "gems.rb"
                     ;; These are only required for maintenance.
                     ((".*gem \"bake-modernize\".*") "")
                     ((".*gem \"bake-gem\".*") "")
                     ;; Not actually required by the tests.
                     ((".*gem 'benchmark-ips'.*") "")
                     ((".*gem \"ruby-prof\".*") ""))))
               (add-before 'build 'remove-missing-signing-key
                 (lambda _
                   ;; Otherwise, the build fails with ENOENT.
                   (substitute* "timers.gemspec"
                     ((".*spec.signing_key.*") ""))))
               (replace 'check
                 (lambda* (#:key tests? #:allow-other-keys)
                   (when tests?
                     (invoke "bake" "test")))))))
    (native-inputs
     (list ruby-covered
           ruby-bake-test
           ruby-bake-test-external
           ruby-sus))
    (synopsis "Collection of Ruby timer classes")
    (description "Timers offers a collections of one-shot and periodic timers,
intended for use with event loops such as async.")
    (home-page "https://github.com/socketry/timers")
    (license license:expat)))

(define-public ruby-tilt
  (package
    (name "ruby-tilt")
    (version "2.0.11")
    (source
     (origin
       (method git-fetch)               ;the distributed gem lacks tests
       (uri (git-reference
             (url "https://github.com/rtomayko/tilt")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0a75s6ci2rwai5q1bnlqbz8kxqnfp2497jhkcry1n4g29lcxq9ja"))))
    (build-system ruby-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-tests
            (lambda _
              ;; Patch some tests
              (substitute* "test/tilt_sasstemplate_test.rb"
                (("}\",") "}
\","))))
          (add-after 'unpack 'remove-some-dependencies
            (lambda _
              (substitute* "Gemfile"
                (("gem 'less'") "\n")
                (("gem 'coffee-script'") "\n")
                (("gem 'livescript'") "\n")
                (("gem 'babel-transpiler'") "\n")
                (("gem 'typescript-node'") "\n")
                (("gem 'typescript-node'") "\n")
                (("gem 'duktape'.*") "\n")
                ;; TODO ronn is used for generating the manual
                (("gem 'ronn'.*") "\n")
                ;; ruby-haml has a runtime dependency on ruby-tilt, so don't
                ;; pass it in as a native-input
                (("gem 'haml'.*") "\n")
                ;; TODO Not all of these gems are packaged for Guix yet:
                ;; less, coffee-script, livescript, babel-transpiler,
                ;; typescript-node
                (("if can_execjs") "if false")
                ;; Disable the secondary group to reduce the number of
                ;; dependencies. None of the normal approaches work, so patch
                ;; the Gemfile instead.
                (("group :secondary") "[].each"))))
          (add-before 'check 'set-SASS_IMPLEMENTATION
            (lambda _
              (setenv "SASS_IMPLEMENTATION" "sassc"))))))
    (propagated-inputs
     (list ruby-pandoc-ruby ruby-sassc))
    (native-inputs
     (list bundler ruby-yard ruby-builder ruby-erubis ruby-markaby))
    (synopsis "Generic interface to multiple Ruby template engines")
    (description
     "Tilt is a thin interface over a number of different Ruby template
engines in an attempt to make their usage as generic as possible.")
    (home-page "https://github.com/rtomayko/tilt/")
    (license license:expat)))

(define-public ruby-thread-safe
  (package
    (name "ruby-thread-safe")
    (version "0.3.6")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "thread_safe" version))
       (sha256
        (base32
         "0nmhcgq6cgz44srylra07bmaw99f5271l0dpsvl5f75m44l0gmwy"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ; needs simplecov, among others
    (synopsis "Thread-safe utilities for Ruby")
    (description "The thread_safe library provides thread-safe collections and
utilities for Ruby.")
    (home-page "https://github.com/ruby-concurrency/thread_safe")
    (license license:asl2.0)))

(define-public ruby-tzinfo
  (package
    (name "ruby-tzinfo")
    (version "2.0.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              ;; Pull from git because the gem has no tests.
              (url "https://github.com/tzinfo/tzinfo")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1n1gzjqwwnx209h8d054miva0y7x17db2ahy7jav5r25ibhh7rgm"))))
    (build-system ruby-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'skip-safe-tests
            (lambda _
              (substitute* "test/test_utils.rb"
                (("def safe_test\\(options = \\{\\}\\)")
                 "def safe_test(options = {})
      skip('The Guix build environment has an unsafe load path')"))))
          (add-before 'check 'pre-check
            (lambda _
              (setenv "HOME" (getcwd))
              (substitute* "Gemfile"
                (("simplecov.*") "simplecov'\n"))))
          (replace 'check
            (lambda* (#:key tests? test-target #:allow-other-keys)
              (when tests?
                (invoke "bundler" "exec" "rake" test-target)))))))
    (propagated-inputs
     (list ruby-concurrent))
    (native-inputs
     (list ruby-simplecov))
    (synopsis "Time zone library for Ruby")
    (description "TZInfo is a Ruby library that provides daylight savings
aware transformations between times in different time zones.")
    (home-page "https://tzinfo.github.io")
    (license license:expat)))

(define-public ruby-tzinfo-data
  (package
    (name "ruby-tzinfo-data")
    (version "1.2023.3")
    (source
     (origin
       (method git-fetch)
       ;; Download from GitHub because the rubygems version does not contain
       ;; Rakefile or tests.
       (uri (git-reference
              (url "https://github.com/tzinfo/tzinfo-data")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1v3fpfmw485lsc9bfqfcasb9j25g9ywfpmmk648l2vdsh7nipilf"))))
    (build-system ruby-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-source
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "Rakefile"
                (("URI\\.parse\\(url\\)\\.open")
                 "URI.open(url)")
                (("https://data.iana.org/time-zones/releases")
                 (assoc-ref inputs "tzdata")))))
          (add-before 'check 'pre-check
            (lambda _
              (setenv "HOME" (getcwd))
              (substitute* "Rakefile"
                ;; Don't need gpg, and it may break after a time.
                (("gpg ") "echo ")
                (("    sh\\(\\\"make -C" text)
                 (string-append "    sh(\"sed -i 's@/bin/sh@sh@' #{tzdb_combined_path}/Makefile \")\n"
                                "    sh(\"sed -i 's@cc=@cc?=@' #{tzdb_combined_path}/Makefile \")\n" text)))
              (setenv "cc" #$(cc-for-target)))))))
    (propagated-inputs
     (list ruby-tzinfo))
    (native-inputs
     `(("tzdata"
        ,(file-union "tzdata-for-ruby-tzdata-info"
           `(("tzdata2023c.tar.gz"
              ,(origin
                 (method url-fetch)
                 (uri "https://data.iana.org/time-zones/releases/tzdata2023c.tar.gz")
                 (sha256
                  (base32
                   "0p4nvp5bdxxdqh269nvvcfrpycbbfwm31al5whwbpsaa3dfhnl9z"))))
             ("tzdata2023c.tar.gz.asc"
              ,(origin
                 (method url-fetch)
                 (uri "https://data.iana.org/time-zones/releases/tzdata2023c.tar.gz.asc")
                 (sha256
                  (base32
                   "0mrmhczs5qnj1zp6gh4pg6fm0iblr2jmzy0fgh9slinwxmn7pv6m"))))
             ("tzcode2023c.tar.gz"
              ,(origin
                 (method url-fetch)
                 (uri "https://data.iana.org/time-zones/releases/tzcode2023c.tar.gz")
                 (sha256
                  (base32
                   "1rqln88ki0jagi372nqyn7bs03rf2l33081sy2835mwsn4mpzla6"))))
             ("tzcode2023c.tar.gz.asc"
              ,(origin
                 (method url-fetch)
                 (uri "https://data.iana.org/time-zones/releases/tzcode2023c.tar.gz.asc")
                 (sha256
                  (base32
                   "0jbx8xjv75qfh7bxa2xmrf97r37057y89rhmrq1gz8s6b8qlzb2i")))))))))
    (synopsis "Data from the IANA Time Zone database")
    (description
     "This library provides @code{TZInfo::Data}, which contains data from the
IANA Time Zone database packaged as Ruby modules for use with @code{TZInfo}.")
    (home-page "https://tzinfo.github.io")
    (license license:expat)))

(define-public ruby-rb-inotify
  (package
    (name "ruby-rb-inotify")
    (version "0.10.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "rb-inotify" version))
       (sha256
        (base32
         "1jm76h8f8hji38z3ggf4bzi8vps6p7sagxn3ab57qc0xyga64005"))))
    (build-system ruby-build-system)
    (arguments
     (list
      #:tests? #f ; there are no tests
      #:phases
      #~(modify-phases %standard-phases
          ;; Building the gemspec with rake is not working here since it is
          ;; generated with Jeweler.  It is also unnecessary because the
          ;; existing gemspec does not use any development tools to generate a
          ;; list of files.
          (replace 'build
            (lambda _
              (invoke "gem" "build" "rb-inotify.gemspec"))))))
    (propagated-inputs
     (list ruby-ffi))
    (native-inputs
     (list ruby-yard))
    (synopsis "Ruby wrapper for Linux's inotify")
    (description "rb-inotify is a simple wrapper over the @code{inotify} Linux
kernel subsystem for monitoring changes to files and directories.")
    (home-page "https://github.com/nex3/rb-inotify")
    (license license:expat)))

(define-public ruby-pry-editline
  (package
    (name "ruby-pry-editline")
    (version "1.1.2")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "pry-editline" version))
              (sha256
               (base32
                "1pjxyvdxvw41xw3yyl18pwzix8hbvn6lgics7qcfhjfsf1zs8x1z"))))
    (build-system ruby-build-system)
    (arguments `(#:tests? #f)) ; no tests included
    (native-inputs
     (list bundler))
    (synopsis "Open the current REPL line in an editor")
    (description
     "This gem provides a plugin for the Ruby REPL to enable opening the
current line in an external editor.")
    (home-page "https://github.com/tpope/pry-editline")
    (license license:expat)))

(define-public ruby-sdoc
  (package
    (name "ruby-sdoc")
    (version "1.1.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "sdoc" version))
              (sha256
               (base32
                "1am73dldx1fqlw2xny5vyk00pgkisg6bvs0pa8jjd7c19drjczrd"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'set-rubylib-and-patch-gemfile
          (lambda _
            (setenv "RUBYLIB" "lib")
            (substitute* "sdoc.gemspec"
              (("s.add_runtime_dependency.*") "\n")
              (("s.add_dependency.*") "\n"))
            (substitute* "Gemfile"
              (("gem \"rake\".*")
               "gem 'rake'\ngem 'rdoc'\ngem 'json'\n"))
            #t)))))
    (propagated-inputs
     (list ruby-json))
    (native-inputs
     (list bundler ruby-minitest ruby-hoe))
    (synopsis "Generate searchable RDoc documentation")
    (description
     "SDoc is an RDoc documentation generator to build searchable HTML
documentation for Ruby code.")
    (home-page "https://github.com/voloko/sdoc")
    (license license:expat)))

(define-public ruby-tins
  (package
    (name "ruby-tins")
    (version "1.29.1")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "tins" version))
              (sha256
               (base32
                "0nzp88y19rqlcizp1nw8m44fvfxs9g3bhjpscz44dwfawfrmr0cb"))))
    (build-system ruby-build-system)
    ;; This gem needs gem-hadar at development time, but gem-hadar needs tins
    ;; at runtime.  To avoid the dependency on gem-hadar we disable rebuilding
    ;; the gemspec.
    (arguments
     `(#:tests? #f ; there are no tests
       #:phases
       (modify-phases %standard-phases
         (replace 'build
          (lambda _
            ;; "lib/spruz" is a symlink.  Leaving it in the gemspec file
            ;; causes an error.
            (substitute* "tins.gemspec"
              (("\"lib/spruz\", ") ""))
            (invoke "gem" "build" "tins.gemspec"))))))
    (propagated-inputs
     (list ruby-sync))
    (synopsis "Assorted tools for Ruby")
    (description "Tins is a Ruby library providing assorted tools.")
    (home-page "https://github.com/flori/tins")
    (license license:expat)))

(define-public ruby-gemtext
  (package
    (name "ruby-gemtext")
    (version "1.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "gemtext" version))
       (sha256
        (base32 "1z559f5z0zvwnqgnny0zf4mppiary020ljxwpw3bdxi2hr9aj3gp"))))
    (build-system ruby-build-system)
    (synopsis "Gemtext parser for Ruby")
    (description
     "This package is a Ruby parser library for Gemtext (hypertext format
which is intended to serve as the native response format of the Gemini
file transfer protocol) and produces a document object of various
nodes.")
    (home-page "https://github.com/exastencil/gemtext")
    (license license:expat)))

(define-public ruby-gem-hadar
  (package
    (name "ruby-gem-hadar")
    (version "1.11.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "gem_hadar" version))
              (sha256
               (base32
                "160abb3l4n3gkhd86f22n981bhqxkbf5ym6fhsk796pix6696pd5"))))
    (build-system ruby-build-system)
    ;; This gem needs itself at development time. We disable rebuilding of the
    ;; gemspec to avoid this loop.
    (arguments
     `(#:tests? #f ; there are no tests
       #:phases
       (modify-phases %standard-phases
         (replace 'build
          (lambda _
            (invoke "gem" "build" "gem_hadar.gemspec"))))))
    (propagated-inputs
     (list git-minimal/pinned ruby-tins ruby-yard))
    (synopsis "Library for the development of Ruby gems")
    (description
     "This library contains some useful functionality to support the
development of Ruby gems.")
    (home-page "https://github.com/flori/gem_hadar")
    (license license:expat)))

(define-public ruby-minitest-tu-shim
  (package
    (name "ruby-minitest-tu-shim")
    (version "1.3.3")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "minitest_tu_shim" version))
              (sha256
               (base32
                "0xlyh94iirvssix157ng2akr9nqhdygdd0c6094hhv7dqcfrn9fn"))))
    (build-system ruby-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-test-include-path
            (lambda* (#:key inputs #:allow-other-keys)
              (let* ((minitest (assoc-ref inputs "ruby-minitest")))
                (substitute* "Rakefile"
                  (("Hoe\\.add_include_dirs .*")
                   (string-append "Hoe.add_include_dirs \""
                                  minitest "/lib/ruby/vendor_ruby"
                                  "/gems/minitest-"
                                  #$(package-version ruby-minitest-4)
                                  "/lib" "\""))))))
          (add-before 'check 'fix-test-assumptions
            (lambda _
              ;; The test output includes the file name, so a couple of tests
              ;; fail.  Changing the regular expressions slightly fixes this
              ;; problem.
              (substitute* "test/test_mini_test.rb"
                (("output.sub!\\(.*, 'FILE:LINE'\\)")
                 "output.sub!(/\\/.+-[\\w\\/\\.]+:\\d+/, 'FILE:LINE')")
                (("gsub\\(/.*, 'FILE:LINE'\\)")
                 "gsub(/\\/.+-[\\w\\/\\.]+:\\d+/, 'FILE:LINE')")))))))
    (propagated-inputs
     (list ruby-minitest-4))
    (native-inputs
     (list ruby-hoe-3))
    (synopsis "Adapter library between minitest and test/unit")
    (description
     "This library bridges the gap between the small and fast minitest and
Ruby's large and slower test/unit.")
    (home-page "https://rubygems.org/gems/minitest_tu_shim")
    (license license:expat)))

(define-public ruby-term-ansicolor
  (package
    (name "ruby-term-ansicolor")
    (version "1.7.1")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "term-ansicolor" version))
              (sha256
               (base32
                "1xq5kci9215skdh27npyd3y55p812v4qb4x2hv3xsjvwqzz9ycwj"))))
    (build-system ruby-build-system)
    ;; Rebuilding the gemspec seems to require git, even though this is not a
    ;; git repository, so we just build the gem from the existing gemspec.
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-test
            (lambda -
              (substitute* "tests/hsl_triple_test.rb"
                (("0\\\\\\.0%")
                 "0\\.?0?%"))))
          (replace 'build
            (lambda _
              (invoke "gem" "build" "term-ansicolor.gemspec"))))))
    (propagated-inputs
     (list ruby-tins))
    (native-inputs
     (list ruby-gem-hadar))
    (synopsis "Ruby library to control the attributes of terminal output")
    (description
     "This Ruby library uses ANSI escape sequences to control the attributes
of terminal output.")
    (home-page "https://flori.github.io/term-ansicolor/")
    ;; There is no mention of the "or later" clause.
    (license license:gpl2)))

(define-public ruby-immutable-struct
  (package
    (name "ruby-immutable-struct")
    (version "2.4.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/stitchfix/immutable-struct")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "17hlmy9jfwn3i5h2rwv832ycwcdqwxq7dkkd2yly28klwj0l52rq"))))
    (build-system ruby-build-system)
    (arguments
     (list
      ;; Issues with the lack of Set in Ruby 3
      #:ruby ruby-2.7
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "ruby" (which "rspec"))))))))
    (native-inputs
     (list ruby-rspec))
    (synopsis "Ruby library for creating immutable struct classes")
    (description
     "Easily create value objects without the pain of Ruby's Struct (or its setters)")
    (home-page "https://stitchfix.github.io/immutable-struct/")
    (license license:expat)))

(define-public ruby-faker
  (package
    (name "ruby-faker")
    (version "3.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/faker-ruby/faker")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1wwdwh5qwaxnd9dl6732mj6b953l5r32r4936pj5680963iagq59"))))
    (build-system ruby-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-rubocop-from-rakefile
            (lambda _
              (substitute* "Rakefile"
                (("require 'rubocop/rake_task'") "")
                (("RuboCop::RakeTask\\.new") "")))))))
    (native-inputs (list ruby-yard ruby-simplecov ruby-timecop))
    (propagated-inputs (list ruby-i18n))
    (synopsis "Library for generating fake data")
    (description "Faker is a port of Data::Faker from Perl. It is used to
easily generate fake data: names, addresses, phone numbers, etc.")
    (home-page "https://github.com/faker-ruby/faker")
    (license license:expat)))

(define-public ruby-terraform
  (package
  (name "ruby-terraform")
  (version "1.7.0")
  (source
   (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/infrablocks/ruby_terraform")
           (commit (string-append "v" version))))
     (file-name (git-file-name name version))
     (sha256
      (base32
       "18d1qkf2rbbvc2f0dxni85i2l2g8zn5kzh0v8zr1b86r1wjy6rvd"))))
  (build-system ruby-build-system)
  (arguments
   (list
    #:test-target "spec"
    #:phases
    #~(modify-phases %standard-phases
        (add-after 'unpack 'disable-bundler
          (lambda _
            (substitute* "spec/spec_helper.rb"
              (("require 'bundler/setup'") ""))))
        (add-before 'check 'disable-falinig-tests
          (lambda _
            (substitute* "spec/ruby_terraform/commands/plan_spec.rb"
              (("it 'logs an error raised when running the command'")
               "xit 'logs an error raised when running the command'")
              (("it 'raises execution error when an error occurs running the command'")
               "xit 'raises execution error when an error occurs running the command'"))))
        (replace 'check
          (lambda* (#:key tests? #:allow-other-keys)
            (when tests?
              (invoke "rspec")))))))
  (native-inputs
   (list ruby-rspec
         ruby-faker
         ruby-simplecov))
  (propagated-inputs
   (list ruby-lino ruby-immutable-struct))
  (synopsis "Ruby wrapper around the Terraform command line interface")
  (description
   "This package provides a Ruby wrapper around the Terraform command line
interface so that Terraform can be more easily invoked from Ruby code.")
  (home-page "https://github.com/infrablocks/ruby_terraform")
  (license license:expat)))

(define-public ruby-pstree
  (package
    (name "ruby-pstree")
    (version "0.3.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "pstree" version))
              (sha256
               (base32
                "0ld3ng37y92kv9vqnachw1l3n07hsc8hrnjs9d840liw0mysf1vp"))))
    (build-system ruby-build-system)
    (native-inputs
     (list ruby-gem-hadar bundler))
    (synopsis "Create a process tree data structure")
    (description
     "This library uses the output of the @code{ps} command to create a
process tree data structure for the current host.")
    (home-page "https://github.com/flori/pstree")
    ;; There is no mention of the "or later" clause.
    (license license:gpl2)))

(define-public ruby-psych
  (package
    (name "ruby-psych")
    (version "5.1.0")
    (source (origin
              (method git-fetch)        ;for tests
              (uri (git-reference
                    (url "https://github.com/ruby/psych")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0m3668y79jcv2h9p7w74awwdyz13rpfr24w4nzh3iz96kxwssz83"))))
    (build-system ruby-build-system)
    (inputs
     (list libyaml))
    (native-inputs
     (list ruby-rake-compiler))
    (synopsis "Ruby YAML parser and emitter")
    (description
     "Psych is a YAML parser and emitter.  Psych leverages libyaml for its
YAML parsing and emitting capabilities.  In addition to wrapping libyaml,
Psych also knows how to serialize and de-serialize most Ruby objects to and
from the YAML format.")
    (home-page "https://github.com/ruby/psych")
    (license license:expat)))

(define-public ruby-psych-3
  (package
    (inherit ruby-psych)
    (version "3.3.4")
    (source (origin
              (method git-fetch)        ;for tests
              (uri (git-reference
                    (url "https://github.com/ruby/psych")
                    (commit (string-append "v" version))))
              (file-name (git-file-name "ruby-psych" version))
              (sha256
               (base32
                "11f7bxbhaj5697izap7hfbiln6lfk5cks78a498mkyhs2ylhl0fc"))))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "bundle" "exec" "rake" "default")))))))))

(define-public ruby-utils
  (package
    (name "ruby-utils")
    (version "0.9.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "utils" version))
              (sha256
               (base32
                "196zhgcygrnx09bb9mh22qas03rl9avzx8qs0wnxznpin4pffwcl"))))
    (build-system ruby-build-system)
    (propagated-inputs
     (list ruby-tins ruby-term-ansicolor ruby-pstree ruby-pry-editline))
    (native-inputs
     (list ruby-gem-hadar bundler))
    (synopsis "Command line tools for working with Ruby")
    (description
     "This package provides assorted command line tools that may be useful
when working with Ruby code.")
    (home-page "https://github.com/flori/utils")
    ;; There is no mention of the "or later" clause.
    (license license:gpl2)))

(define-public ruby-jaro-winkler
  (package
    (name "ruby-jaro-winkler")
    (version "1.5.4")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "jaro_winkler" version))
       (sha256
        (base32 "1y8l6k34svmdyqxya3iahpwbpvmn3fswhwsvrz0nk1wyb8yfihsh"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f))                    ; no included tests
    (synopsis "Ruby implementation of Jaro-Winkler distance algorithm")
    (description
     "@code{jaro_winkler} is an implementation of Jaro-Winkler distance
algorithm.  It is written as a C extension and will fallback to a pure Ruby
implementation on platforms where this is unsupported.")
    (home-page "https://github.com/tonytonyjan/jaro_winkler")
    (license license:expat)))

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

(define-public ruby-json-pure
  (package
    (name "ruby-json-pure")
    (version "2.6.3")
    (source
     (origin
       ;; For tests
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/flori/json.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0551269c98a07m6bl594syh5vknrm3c636a4dxis9jpsb7vf7lfx"))))
    (build-system ruby-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-rakefile
            (lambda _
              (substitute* "Rakefile"
                ;; Since this is not a git repository, do not call 'git'.
                (("`git ls-files`") "`find . -type f |sort`")))))))
    (native-inputs
     (list bundler ragel ruby-simplecov ruby-test-unit which))
    (synopsis "JSON implementation in pure Ruby")
    (description
     "This package provides a JSON implementation written in pure Ruby.")
    (home-page "https://flori.github.io/json/")
    (license license:ruby)))

(define-public ruby-jwt
  (package
    (name "ruby-jwt")
    (version "2.7.1")
    (source
     (origin
       ;; For tests
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jwt/ruby-jwt")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "12ss6knfis6a6a41qndalnlvq3yykhpg6igzll8qyssnnwi9zdw7"))))
    (build-system ruby-build-system)
    (arguments
     (list
      #:test-target "test"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-unnecessary-dependencies
            (lambda _
              (substitute* "ruby-jwt.gemspec"
                (("spec\\.add_development_dependency 'appraisal'") "")
                (("spec\\.add_development_dependency 'simplecov'") ""))
              (substitute* "Gemfile"
                (("gem 'rubocop'.*") ""))
              (substitute* "Rakefile"
                (("require 'rubocop/rake_task'") "")
                (("RuboCop::RakeTask\\.new\\(:rubocop\\)") ""))
              (substitute* "spec/spec_helper.rb"
                (("require 'simplecov.*") "\n")
                ;; Use [].each to disable running the SimpleCov configuration
                ;; block
                (("SimpleCov\\.configure") "[].each")
                (("require 'codeclimate-test-reporter'") "")
                (("require 'codacy-coverage'") "")
                (("Codacy::Reporter\\.start") "")))))))
    (native-inputs
     (list bundler ruby-rspec ruby-rbnacl))
    (synopsis "Ruby implementation of the JSON Web Token standard")
    (description
     "This package provides a pure Ruby implementation of the RFC 7519 OAuth
@acronym{JWT, JSON Web Token} standard.")
    (home-page "https://github.com/jwt/ruby-jwt")
    (license license:expat)))

;; Even though this package only provides bindings for a Mac OSX API it is
;; required by "ruby-listen" at runtime.
(define-public ruby-rb-fsevent
  (package
    (name "ruby-rb-fsevent")
    (version "0.10.3")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "rb-fsevent" version))
              (sha256
               (base32
                "1lm1k7wpz69jx7jrc92w3ggczkjyjbfziq5mg62vjnxmzs383xx8"))))
    (build-system ruby-build-system)
    ;; Tests need "guard-rspec", which needs "guard".  However, "guard" needs
    ;; "listen", which needs "rb-fsevent" at runtime.
    (arguments `(#:tests? #f))
    (synopsis "FSEvents API with signals catching")
    (description
     "This library provides Ruby bindings for the Mac OSX FSEvents API.")
    (home-page "https://rubygems.org/gems/rb-fsevent")
    (license license:expat)))

(define-public ruby-listen
  (package
    (name "ruby-listen")
    (version "3.8.0")
    (source
     (origin
       ;; The gem does not include a Rakefile, so fetch from the Git
       ;; repository.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/guard/listen")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1skkglml094dw1xr4742in1rwwa84ld0mz4nkw6qa8pwhx48x2n5"))))
    (build-system ruby-build-system)
    (arguments
     (list
      #:test-target "spec"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-files-in-gemspec
            (lambda _
              (substitute* "listen.gemspec"
                (("`git ls-files -z`")
                 "`find . -type f -printf '%P\\\\0' |sort -z`"))))
          (add-before 'check 'remove-unnecessary-dependencies'
            (lambda _
              (substitute* "Rakefile"
                ;; Rubocop is for code linting, and is unnecessary for running
                ;; the tests.
                ((".*rubocop.*") "")))))))
    (native-inputs
     (list bundler ruby-rspec))
    (inputs
     (list ;; ruby-thor is used for the command line interface, and is referenced
           ;; in the wrapper, and therefore just needs to be an input.
           ruby-thor))
    (propagated-inputs
     (list ruby-rb-fsevent ruby-rb-inotify ruby-dep))
    (synopsis "Listen to file modifications")
    (description "The Listen gem listens to file modifications and notifies
you about the changes.")
    (home-page "https://github.com/guard/listen")
    (license license:expat)))

(define-public ruby-loofah
  (package
    (name "ruby-loofah")
    (version "2.21.3")
    (home-page "https://github.com/flavorjones/loofah")
    (source
     (origin
       ;; Build from git because the gem lacks tests.
       (method git-fetch)
       (uri (git-reference (url home-page)
                           (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1lh7cf56y1b0h090ahphvz7grq581phsamdl0rq59y0q9bqwrhg0"))))
    (build-system ruby-build-system)
    (native-inputs
     (list ruby-hoe ruby-hoe-markdown ruby-rr))
    (propagated-inputs
     (list ruby-nokogiri ruby-crass))
    (synopsis "Ruby library for manipulating and transforming HTML/XML")
    (description
     "Loofah is a general library for manipulating and transforming HTML/XML
documents and fragments.  It's built on top of Nokogiri and libxml2.")
    (license license:expat)))

(define-public ruby-crass
  (package
    (name "ruby-crass")
    (version "1.0.6")
    (home-page "https://github.com/rgrove/crass")
    (source (origin
              ;; The gem does not contain tests, so pull from git.
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1gbsb81psgb6xhnwpx4s409jc0mk0gijh039sy5xyi8jpaaadp40"))))
    (build-system ruby-build-system)
    (synopsis "Pure Ruby CSS parser")
    (description
     "Crass is a pure Ruby CSS parser based on the CSS Syntax Level 3 spec.")
    (license license:expat)))

;;; The ruby-nokogumbo package has been absorbed into ruby-nokogiri.
(define-public ruby-nokogumbo
  (deprecated-package "ruby-nokogumbo" ruby-nokogiri))

(define-public ruby-samovar
  (package
    (name "ruby-samovar")
    (version "2.1.4")
    (source (origin
              (method git-fetch)        ;for tests
              (uri (git-reference
                    (url "https://github.com/ioquatix/samovar")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "05pc5xlbv44anx0sfqssh0xhxg297bvl5slsl7k1vnka4k5fhax6"))))
    (build-system ruby-build-system)
    (native-inputs (list ruby-covered ruby-rspec))
    (propagated-inputs (list ruby-console ruby-mapping))
    (synopsis "Flexible option parser for Ruby")
    (description "Samovar is a modern framework for building command-line
tools and applications.  It provides a declarative class-based DSL for
building command-line parsers that include automatic documentation generation.
It helps you keep your functionality clean and isolated where possible.")
    (home-page "https://github.com/ioquatix/samovar")
    (license license:expat)))

(define-public ruby-sanitize
  (package
    (name "ruby-sanitize")
    (version "6.0.0")
    (home-page "https://github.com/rgrove/sanitize")
    (source (origin
              (method git-fetch)
              ;; The gem does not include the Rakefile, so we download the
              ;; source from Github.
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0p1a28vx95vscy9xzzyyddzgb9496x42a5i2ka39cpxbl5f3gkl0"))))
    (build-system ruby-build-system)
    (propagated-inputs (list ruby-crass ruby-nokogiri))
    (native-inputs (list ruby-minitest))
    (synopsis "Whitelist-based HTML and CSS sanitizer")
    (description
     "Sanitize is a whitelist-based HTML and CSS sanitizer.  Given a list of
acceptable elements, attributes, and CSS properties, Sanitize will remove all
unacceptable HTML and/or CSS from a string.")
    (license license:expat)))

(define-public ruby-sync
  (package
    (name "ruby-sync")
    (version "0.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "sync" version))
        (sha256
         (base32
          "1z9qlq4icyiv3hz1znvsq1wz2ccqjb1zwd6gkvnwg6n50z65d0v6"))))
    (build-system ruby-build-system)
    (synopsis "Ruby module with a two-phase lock and counter")
    (description "This package provides a Ruby module that provides a two-phase
lock with a counter.")
    (home-page "https://github.com/ruby/sync")
    (license license:bsd-2)))

(define-public ruby-oj
  (package
    (name "ruby-oj")
    (version "3.13.9")
    (source
     (origin
       (method git-fetch)
       ;; Version on rubygems.org does not contain Rakefile, so download from
       ;; GitHub instead.
       (uri (git-reference
              (url "https://github.com/ohler55/oj")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0a24zd28y58nyhjxgrpn2k9p72vzj3zbmdrcsbhwbdycj7nn9fpd"))))
    (build-system ruby-build-system)
    (arguments
     '(#:test-target "test_all"
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'disable-bundler
           (lambda _
             (substitute* "Rakefile"
               (("Bundler\\.with_clean_env") "1.times")
               (("bundle exec ") "")))))))
    (native-inputs
     (list bundler ruby-rspec ruby-rake-compiler))
    (synopsis "JSON parser for Ruby optimized for speed")
    (description
     "Oj is a JSON parser and generator for Ruby, where the encoding and
decoding of JSON is implemented as a C extension to Ruby.")
    (home-page "https://www.ohler.com/oj/")
    (license (list license:expat     ; Ruby code
                   license:bsd-3)))) ; extension code

(define-public ruby-ox
  (package
    (name "ruby-ox")
    (version "2.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "ox" version))
       (sha256
        (base32
         "0fmk62b1h2i79dfzjj8wmf8qid1rv5nhwfc17l489ywnga91xl83"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ; no tests
    (synopsis "Optimized XML library for Ruby")
    (description
     "Optimized XML (Ox) is a fast XML parser and object serializer for Ruby
written as a native C extension.  It was designed to be an alternative to
Nokogiri and other Ruby XML parsers for generic XML parsing and as an
alternative to Marshal for Object serialization.")
    (home-page "https://www.ohler.com/ox")
    (license license:expat)))

(define-public ruby-redcloth
  (package
    (name "ruby-redcloth")
    (version "4.3.2")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "RedCloth" version))
              (sha256
               (base32
                "0m9dv7ya9q93r8x1pg2gi15rxlbck8m178j1fz7r5v6wr1avrrqy"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f ; no tests
       #:phases
       (modify-phases %standard-phases
         ;; Redcloth has complicated rake tasks to build various versions for
         ;; multiple targets using RVM.  We don't want this so we just use the
         ;; existing gemspec.
         (replace 'build
          (lambda _
            (invoke "gem" "build" "redcloth.gemspec"))))))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-diff-lcs" ,ruby-diff-lcs)
       ("ruby-rspec-2" ,ruby-rspec-2)))
    (synopsis "Textile markup language parser for Ruby")
    (description
     "RedCloth is a Ruby parser for the Textile markup language.")
    (home-page "http://redcloth.org")
    (license license:expat)))

(define-public ruby-pg
  (package
    (name "ruby-pg")
    (version "1.4.6")
    (home-page "https://github.com/ged/ruby-pg")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0k7jgx7x7p6gbsbrv2l5rq27nff2nphnls1sdq525d82b068qnfm"))))
    (build-system ruby-build-system)
    (arguments
     (list
      #:test-target "spec"
      #:phases
      #~(modify-phases %standard-phases
               (add-before 'build 'compile
                 (lambda _
                   (invoke "rake" "compile")))
               ;; Some tests rely on postgresql_lib_path.rb, but it is not
               ;; available until the gem is installed.  Run tests after
               ;; installing to work around it.
               (delete 'check)
               (add-after 'install 'check
                 (lambda* (#:key tests? #:allow-other-keys)
                   (let ((new-gem (string-append #$output
                                                 "/lib/ruby/vendor_ruby")))
                     (setenv "GEM_PATH"
                             (string-append (getenv "GEM_PATH") ":" new-gem))
                     (when tests?
                       (invoke "rspec"))))))))
    (native-inputs (list ruby-rake-compiler ruby-hoe ruby-rspec))
    (inputs (list postgresql))
    (synopsis "Ruby interface to PostgreSQL")
    (description "Pg is the Ruby interface to the PostgreSQL RDBMS.  It works
with PostgreSQL 9.3 and later.")
    (license license:ruby)))

(define-public ruby-byebug
  (package
    (name "ruby-byebug")
    (version "11.1.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/deivid-rodriguez/byebug")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0vyy3k2s7dcndngj6m8kxhs1vxc2c93dw8b3yyand3srsg9ffpij"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Remove wrappers that try to setup a bundle environment.
           (with-directory-excursion "bin"
             (for-each delete-file '("bundle" "rake" "rubocop"))
             ;; ruby-minitest doesn't come with a launcher, so fix the one
             ;; provided.
             (substitute* "minitest"
               (("load File\\.expand_path\\(\"bundle\".*") "")
               (("require \"bundler/setup\".*") "")))))))
    (build-system ruby-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'sanitize-dependencies
            (lambda _
              (substitute* "Rakefile"
                ((".*chandler/tasks.*") ""))))
          (add-after 'unpack 'skip-tmp-path-sensitive-test
            (lambda _
              (substitute* "test/commands/where_test.rb"
                (("unless /cygwin\\|mswin\\|mingw\\|darwin/.*")
                 "unless true\n"))))
          (add-before 'build 'compile
            (lambda _
              (invoke "rake" "compile")))
          (add-before 'check 'patch-tests
            (lambda _
              ;; srand': no implicit conversion of nil into Integer (TypeError)
              (delete-file "test/minitest_runner_test.rb")
              ;; Expects 5, gets 162. From a file containing ~10 lines.
              (substitute* "test/commands/finish_test.rb"
                (("test_finish_inside_autoloaded_files")
                 "finish_inside_autoloaded_files"))))
          (add-before 'check 'set-home
            (lambda _
              (setenv "HOME" (getcwd)))))))
    (native-inputs
     (list bundler
           ruby-minitest
           ruby-pry
           ruby-rake-compiler
           ruby-rubocop
           ruby-yard))
    (synopsis "Debugger for Ruby 2")
    (description "Byebug is a Ruby 2 debugger implemented using the Ruby 2
TracePoint C API for execution control and the Debug Inspector C API for call
stack navigation.  The core component provides support that front-ends can
build on.  It provides breakpoint handling and bindings for stack frames among
other things and it comes with a command line interface.")
    (home-page "https://github.com/deivid-rodriguez/byebug")
    (license license:bsd-2)))

(define-public ruby-netrc
  (package
    (name "ruby-netrc")
    (version "0.11.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "netrc" version))
              (sha256
               (base32
                "0gzfmcywp1da8nzfqsql2zqi648mfnx6qwkig3cv36n9m0yy676y"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           ;; There is no Rakefile and minitest can only run one file at once,
           ;; so we have to iterate over all test files.
           (lambda _
             (for-each (lambda (file)
                         (invoke "ruby" "-Itest" file))
                       (find-files "./test" "test_.*\\.rb"))))
         (add-before 'check 'patch-tests-for-newer-ruby
           (lambda _
             (substitute* "test/test_netrc.rb"
               (("Dir.pwd, '.netrc'") "Netrc.home_path, '.netrc'")))))))
    (native-inputs
     (list ruby-minitest))
    (synopsis "Library to read and update netrc files")
    (description
     "This library can read and update netrc files, preserving formatting
including comments and whitespace.")
    (home-page "https://github.com/geemus/netrc")
    (license license:expat)))

(define-public ruby-unf-ext
  (package
    (name "ruby-unf-ext")
    (version "0.0.8.2")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "unf_ext" version))
              (sha256
               (base32
                "1yj2nz2l101vr1x9w2k83a0fag1xgnmjwp8w8rw4ik2rwcz65fch"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'build 'build-ext
           (lambda _ (invoke "rake" "compile:unf_ext")))
         (add-before 'check 'lose-rake-compiler-dock-dependency
           (lambda _
             ;; rake-compiler-dock is listed in the gemspec, but only
             ;; required when cross-compiling.
             (substitute* "unf_ext.gemspec"
               ((".*rake-compiler-dock.*") "")))))))
    (native-inputs
     (list bundler ruby-rake-compiler ruby-test-unit))
    (synopsis "Unicode normalization form support library")
    (description
     "This package provides unicode normalization form support for Ruby.")
    (home-page "https://github.com/knu/ruby-unf_ext")
    (license license:expat)))

(define-public ruby-tdiff
  ;; Use a newer than released snapshot so that rspec-2 is not required.
  (let ((commit "b662a6048f08abc45c1a834e5f34dd1c662935e2"))
    (package
      (name "ruby-tdiff")
      (version (string-append "0.3.3-1." (string-take commit 8)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/postmodern/tdiff")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "0n3gq8rx49f7ln6zqlshqfg2mgqyy30rsdjlnki5mv307ykc7ad4"))))
      (build-system ruby-build-system)
      (native-inputs
       (list ruby-rspec ruby-yard/minimal ruby-rubygems-tasks))
      (synopsis "Calculate the differences between two tree-like structures")
      (description
       "This library provides functions to calculate the differences between two
tree-like structures.  It is similar to Ruby's built-in @code{TSort} module.")
      (home-page "https://github.com/postmodern/tdiff")
      (license license:expat))))

(define-public ruby-nokogiri-diff
  ;; Use a newer than released snapshot so that rspec-2 is not required.
  (let ((commit "a38491e4d8709b7406f2cae11a50226d927d06f5"))
    (package
      (name "ruby-nokogiri-diff")
      (version (string-append "0.2.0-1." (string-take commit 8)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/postmodern/nokogiri-diff")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "1ah2sfjh9n1p0ln2wkqzfl448ml7j4zfy6dhp1qgzq2m41php6rf"))))
      (build-system ruby-build-system)
      (propagated-inputs
       (list ruby-tdiff
             ruby-nokogiri))
      (native-inputs
       (list ruby-rspec ruby-yard/minimal ruby-rubygems-tasks))
      (synopsis "Calculate the differences between two XML/HTML documents")
      (description
       "@code{Nokogiri::Diff} adds the ability to calculate the
differences (added or removed nodes) between two XML/HTML documents.")
      (home-page "https://github.com/postmodern/nokogiri-diff")
      (license license:expat))))

(define-public ruby-racc
  (package
    (name "ruby-racc")
    (version "1.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "racc" version))
       (sha256
        (base32
         "178k7r0xn689spviqzhvazzvxfq6fyjldxb3ywjbgipbfi4s8j1g"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))            ; Fails while parsing test instructions.
    (native-inputs
     (list ruby-hoe ruby-rake-compiler))
    (synopsis "LALR(1) parser generator for Ruby")
    (description
     "Racc is a LALR(1) parser generator.  It is written in Ruby itself, and
generates Ruby program.")
    (home-page "https://i.loveruby.net/en/projects/racc/")
    (license (list
              ;; Generally licensed under the LGPL2.1, and some files also
              ;; available under the same license as Ruby.
              license:lgpl2.1
              license:ruby))))

(define-public ruby-rack
  (package
    (name "ruby-rack")
    ;; Do not upgrade to version 3, as Rails doesn't support it yet.
    (version "2.2.7")
    (source
     (origin
       (method git-fetch)               ;for tests
       (uri (git-reference
             (url "https://github.com/rack/rack")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "02r41fr61x0jfhraizc6bsgs40p8mlpvnzix71zwmcvibg384ify"))))
    (build-system ruby-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'fix-tests
            (lambda _
              ;; This test fails in the build environment (and passes outside
              ;; of it).
              (substitute* "test/spec_files.rb"
                (("res.body.must_equal expected_body") ""))
              ;; A few of the tests use the length of a file on disk for
              ;; Content-Length and Content-Range headers.  However, this file
              ;; has a shebang in it which an earlier phase patches, growing
              ;; the file size from 193 to 239 bytes when the store prefix is
              ;; "/gnu/store".
              (let ((size-diff (- (string-length (which "ruby"))
                                  (string-length "/usr/bin/env ruby"))))
                (substitute* '("test/spec_files.rb")
                  (("208" bytes)
                   (number->string (+ (string->number bytes) size-diff)))
                  (("bytes(.)22-33" all delimiter)
                   (string-append "bytes"
                                  delimiter
                                  (number->string (+ 22 size-diff))
                                  "-"
                                  (number->string (+ 33 size-diff)))))))))))
    (native-inputs
     (list ruby-minitest
           ruby-minitest-global-expectations
           ruby-webrick))
    (synopsis "Unified web application interface for Ruby")
    (description "Rack provides a minimal, modular and adaptable interface for
developing web applications in Ruby.  By wrapping HTTP requests and responses,
it unifies the API for web servers, web frameworks, and software in between
into a single method call.")
    (home-page "https://github.com/rack/rack")
    (license license:expat)))

(define-public ruby-rack-next
  (package
    (inherit ruby-rack)
    (name "ruby-rack")
    (version "3.0.7")
    (source (origin
              (method git-fetch)        ;for tests
              (uri (git-reference
                    (url "https://github.com/rack/rack")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0msf14655nfcq1kgmib6932lgzm9nw3nb0m3c7nh6nj4sx30yxfr"))))
    (arguments '())))

(define-public ruby-rackup
  (package
    (name "ruby-rackup")
    (version "2.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/rack/rackup")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "19b7sy700zjwaw7x47qpxvhnnz5hrp5bhrxfyljgagrli824dajy"))))
    (build-system ruby-build-system)
    (native-inputs (list ruby-minitest-global-expectations))
    (inputs (list ruby-rack-next ruby-webrick))
    (synopsis "Command line interface (CLI) for running for Rack applications")
    (description "This package provides a command line interface for running
for Rack applications.")
    (home-page "https://github.com/rack/rackup")
    (license license:expat)))

(define-public ruby-rack-cache
  (package
    (name "ruby-rack-cache")
    (version "1.13.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "rack-cache" version))
              (sha256
               (base32
                "1cqpax628h2mhnsjfg91c3klxwx2pkvaj061cisb0saqa99b0jgm"))))
    (build-system ruby-build-system)
    (arguments
     (list
      ;; The test suite depends on ruby-memcached, which is not available in
      ;; Guix and bundles a very dated copy of memcached (undesirable).
      #:tests? #f))
    (propagated-inputs (list ruby-rack))
    (synopsis "Component to enable HTTP caching for Rack-based applications")
    (description "Rack::Cache is suitable as a drop-in component to enable
HTTP caching for Rack-based applications that produce freshness (Expires,
Cache-Control) and/or validation (Last-Modified, ETag) information.")
    (home-page "https://github.com/rtomayko/rack-cache")
    (license license:expat)))

(define-public ruby-rack-test
  (package
    (name "ruby-rack-test")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "rack-test" version))
       (sha256
        (base32
         "1ysx29gk9k14a14zsp5a8czys140wacvp91fja8xcja0j1hzqq8c"))))
    (build-system ruby-build-system)
    (arguments
     ;; Disable tests because of circular dependencies: requires sinatra,
     ;; which requires rack-protection, which requires rack-test.  Instead
     ;; simply require the library.
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "ruby" "-Ilib" "-r" "rack/test")))))))
    (propagated-inputs
     (list ruby-rack))
    (synopsis "Testing API for Rack applications")
    (description
     "Rack::Test is a small, simple testing API for Rack applications.  It can
be used on its own or as a reusable starting point for Web frameworks and
testing libraries to build on.")
    (home-page "https://github.com/rack/rack-test")
    (license license:expat)))

(define-public ruby-rack-session
  (package
    (name "ruby-rack-session")
    ;; Stay on version 1 until all the rack users such as Rails can use rack 3
    ;; (rack-session 2 requires rack 3).
    (version "1.0.1")
    (source (origin
              (method git-fetch)        ;for tests
              (uri (git-reference
                    (url "https://github.com/rack/rack-session")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0rv955wd7ckp5jgy5c229wmajh48jpcy8s0iv5i8ma61wf7qw0i1"))))
    (build-system ruby-build-system)
    (native-inputs
     (list ruby-minitest-global-expectations
           ruby-minitest-sprint))
    (propagated-inputs
     (list ruby-rack))
    (synopsis "Session management for Rack")
    (description "This package provides a session management implementation
for Rack.")
    (home-page "https://github.com/rack/rack-session")
    (license license:expat)))

(define-public ruby-rack-protection
  (package
    (name "ruby-rack-protection")
    (version "3.0.5")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "rack-protection" version))
       (sha256
        (base32
         "1a12m1mv8dc0g90fs1myvis8vsgr427k1arg1q4a9qlfw6fqyhis"))))
    (build-system ruby-build-system)
    (arguments
     '(;; Tests missing from the gem.
       #:tests? #f))
    (propagated-inputs
     (list ruby-rack))
    (native-inputs
     (list bundler ruby-rspec-2 ruby-rack-test))
    (synopsis "Rack middleware that protects against typical web attacks")
    (description "Rack middleware that can be used to protect against typical
web attacks.  It can protect all Rack apps, including Rails.  For instance, it
protects against cross site request forgery, cross site scripting,
clickjacking, directory traversal, session hijacking and IP spoofing.")
    (home-page "https://github.com/sinatra/sinatra/tree/master/rack-protection")
    (license license:expat)))

(define-public ruby-rainbow
  (package
    (name "ruby-rainbow")
    (version "3.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "rainbow" version))
       (sha256
        (base32
         "0bb2fpjspydr6x0s8pn1pqkzmxszvkfapv0p4627mywl7ky4zkhk"))))
    (build-system ruby-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         ;; Run rspec directly, to avoid requiring Rubocop which is used from
         ;; the Rakefile.
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "rspec"))
             #t)))))
    (native-inputs
     (list bundler ruby-rspec))
    (synopsis "Colorize printed text on ANSI terminals")
    (description
     "@code{rainbow} provides a string presenter object to colorize strings by
wrapping them in ANSI escape codes.")
    (home-page "https://github.com/sickill/rainbow")
    (license license:expat)))

(define-public ruby-rr
  (package
    (name "ruby-rr")
    (version "1.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "rr" version))
       (sha256
        (base32
         "1n9g78ba4c2zzmz8cdb97c38h1xm0clircag00vbcxwqs4dq0ymp"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ; test files not included
    (native-inputs
     (list bundler ruby-rspec))
    (synopsis "Ruby test double framework")
    (description
     "RR is a test double framework that features a rich selection of double
techniques and a terse syntax.")
    (home-page "https://rr.github.io/rr/")
    (license license:expat)))

(define-public ruby-rest-client
  (package
    (name "ruby-rest-client")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "rest-client" version))
       (sha256
        (base32
         "1qs74yzl58agzx9dgjhcpgmzfn61fqkk33k1js2y5yhlvc5l19im"))))
    (build-system ruby-build-system)
    (arguments
     (list
      ;; TODO Some tests are currently broken
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'remove-unnecessary-development-dependencies
            (lambda _
              (substitute* "rest-client.gemspec"
                ;; Remove rubocop as it's unused. Rubocop also indirectly
                ;; depends on this package through ruby-parser and ruby-ast so
                ;; this avoids a dependency loop.
                ((".*rubocop.*") "\n")
                ;; Remove pry as it's unused, it's a debugging tool
                ((".*pry.*") "\n")
                ;; Remove an unnecessarily strict rdoc dependency
                ((".*rdoc.*") "\n"))))
          (add-before 'check 'delete-network-dependent-tests
            (lambda _
              (delete-file "spec/integration/request_spec.rb")
              (delete-file "spec/integration/httpbin_spec.rb")))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "rspec")))))))
    (propagated-inputs
     (list ruby-http-accept-1
           ruby-http-cookie
           ruby-mime-types
           ruby-netrc))
    (native-inputs
     (list bundler
           ruby-webmock-2
           ruby-rspec))
    (synopsis "Simple HTTP and REST client for Ruby")
    (description
     "@code{rest-client} provides a simple HTTP and REST client for Ruby,
inspired by the Sinatra microframework style of specifying actions:
@code{get}, @code{put}, @code{post}, @code{delete}.")
    (home-page "https://github.com/rest-client/rest-client")
    (license license:expat)))

(define-public ruby-rubocop-ast
  (package
    (name "ruby-rubocop-ast")
    (version "1.28.0")
    (source
     (origin
       (method git-fetch)               ;no test suite in distributed gem
       (uri (git-reference
             (url "https://github.com/rubocop/rubocop-ast")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1dp09gcmahmdfi3s6xsksr3ka9dddjpy9ymhr9wjwv67y1falffr"))))
    (build-system ruby-build-system)
    (arguments
     `(#:test-target "spec"
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'relax-dependencies
                    (lambda _
                      (substitute* "Gemfile"
                        (("gem 'simplecov', '~> 0.10', '< 0.18'")
                         "gem 'simplecov', '~> 0.10'"))))
                  (add-before 'build 'generate-lexer
                    (lambda _
                      (setenv "RUBOCOP_VERSION" "none")
                      (invoke "rake" "generate")))
                  (replace 'replace-git-ls-files
                    (lambda _
                      (substitute* "rubocop-ast.gemspec"
                        (("`git ls-files(.*)`" _ files)
                         (format #f "`find ~a -type f| sort`" files))))))))
    (native-inputs
     (list ruby-bump
           ruby-oedipus-lex
           ruby-pry
           ruby-racc
           ruby-rake
           ruby-rspec
           ruby-simplecov))
    (propagated-inputs
     (list ruby-parser))
    (synopsis "RuboCop's AST extensions and NodePattern functionality")
    (description "Rubocop::AST extends @code{ruby-parser} with classes used
by RuboCop to deal with Ruby's Abstract Syntax Tree (AST), in particular:
@itemize
@item @code{RuboCop::AST::Node}
@item @code{RuboCop::AST::NodePattern}
@end itemize")
    (home-page "https://rubocop.org/")
    (license license:expat)))

(define-public ruby-rexml
  (package
    (name "ruby-rexml")
    (version "3.2.5")
    (source
     (origin
       (method git-fetch)               ;no tests in distributed gem
       (uri (git-reference
             (url "https://github.com/ruby/rexml")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "13n6vaa80drqic2wri4q6k22qzvsn683vp5s8c9dllil6x04kn0x"))))
    (build-system ruby-build-system)
    (synopsis "XML toolkit for Ruby")
    (description "Inspired by Electric XML library for Java, REXML aims to be
easy-to-use API, small and fast.  It supports both tree and stream document
parsing.")
    (home-page "https://github.com/ruby/rexml")
    (license license:bsd-2)))

(define-public ruby-character-set
  (package
    (name "ruby-character-set")
    (version "1.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "character_set" version))
        (sha256
          (base32
            "0affq9n77vwy897ri2zhmfinfagf37hcwwimrccy1bcxan9mj3h3"))))
    (build-system ruby-build-system)
    (arguments '(#:tests? #f))          ;avoid a cycle with ruby-regexp-parser
    (synopsis "Ruby library to manipulate Unicode")
    (description "CharacterSet is a C-extended Ruby library to work with sets
of Unicode code points.  It can read and write these sets in various formats
and implements the @code{stdlib} @code{Set} interface for them.  It also
offers an alternate paradigm of @code{String} processing which grants much
better performance than @code{Regexp} and @code{String} methods from the
@code{stdlib} where applicable.  Many parts can be used independently, e.g.:
@itemize
@item @code{CharacterSet::Character}
@item @code{CharacterSet::Parser}
@item @code{CharacterSet::Writer}
@item @code{RangeCompressor}
@end itemize")
    (home-page "https://github.com/jaynetics/character_set")
    (license license:expat)))

(define-public ruby-range-compressor
  (package
    (name "ruby-range-compressor")
    (version "1.2.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/janosch-x/range_compressor")
              (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1zmc44si5ac2h7r1x4f1j8z5yr6wz528c7dssh0g70fmczs3pfga"))))
    (build-system ruby-build-system)
    (arguments
     (list
      #:test-target "spec"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'extract-gemspec 'strip-version-requirements
            (lambda _
              (substitute* "range_compressor.gemspec"
                (("(.*add_.*dependency '[_A-Za-z0-9-]+').*" _ stripped)
                 (string-append stripped "\n"))))))))
    (native-inputs
     (list ruby-rspec
           ruby-sorted-set))
    (synopsis "Simple arrays of objects to arrays of ranges compressor")
    (description "RangeCompresses is a tiny library that allows compressing
arrays of objects into arrays of ranges.  For example, it can turn the
following: @code{[1, 2, 3, 4, 6, 8, 9, 10]} into @code{[1..4, 6..6, 8..10]}.")
    (home-page "https://github.com/janosch-x/range_compressor")
    (license license:expat)))

(define-public ruby-regexp-property-values
  (let ((commit "03007a66c912949a7130b973cc0eca109c20811f")
        (revision "1"))
    (package
      (name "ruby-regexp-property-values")
      (version (git-version "1.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference              ;no test suite in distributed gem
               (url "https://github.com/jaynetics/regexp_property_values")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1zsax784p16zdkf60lyq9z924zvsafhx9ckxx9srsgkyiqrifi1s"))))
      (build-system ruby-build-system)
      (arguments
       '(#:test-target "default"))
      (native-inputs
       (list ruby-character-set ruby-rake ruby-rake-compiler
             ruby-range-compressor ruby-rspec))
      (synopsis "Inspect Ruby's regex engine property values")
      (description "This small library lets you see which property values are
supported by the regular expression engine of the Ruby version you are running
and can directly read out their code point ranges.  In other words, it
determines all supported values for @code{\\p{value}} expressions and what
they match.")
      (home-page "https://github.com/jaynetics/regexp_property_values")
      (license license:expat))))

(define-public ruby-regexp-parser
  (package
    (name "ruby-regexp-parser")
    (version "2.0.0")
    (source
     (origin
       (method git-fetch)               ;bin/test missing from gem
       (uri (git-reference
             (url "https://github.com/ammar/regexp_parser")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "09ddxdwlml30q6j4rqf06bbjj1mwx00rs0bksnyblhv85anrqz3k"))))
    (build-system ruby-build-system)
    (arguments
     '(#:test-target "default"
       #:phases (modify-phases %standard-phases
                  (add-before 'build 'compile-scanner.rb
                    (lambda _
                      (invoke "rake" "build")
                      ;; XXX: This is needed otherwise the install
                      ;; phase fails to delete the installed cached
                      ;; gem file.
                      (delete-file-recursively "pkg")
                      #t)))))
    (native-inputs
     (list ragel ruby-regexp-property-values ruby-rspec))
    (synopsis "Regular expression parser library for Ruby")
    (description "A Ruby gem for tokenizing, parsing, and transforming regular
expressions.  It comprises the following components:
@itemize
@item A scanner/tokenizer based on Ragel,
@item A lexer that produces a stream of token objects,
@item A parser that produces a tree of Expression objects.
@end itemize")
    (home-page "https://github.com/ammar/regexp_parser")
    (license license:expat)))

(define-public ruby-test-queue
  (package
    (name "ruby-test-queue")
    (version "0.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "test-queue" version))
       (sha256
        (base32
         "0hvm3n1qrqxqilhqk4rjivw3gcrd08zz1i6am9qzg1ppjxv6f36f"))))
    (build-system ruby-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "rspec"))
             #t)))))
    (native-inputs
     (list ruby-rspec))
    (synopsis "Minitest/RSpec parallel test runner for CI environments")
    (description "The test-queue module is a parallel test runner,
built using a centralized queue to ensure optimal distribution of
tests between workers.  It is specifically optimized for Continuous
Integration (CI) environments: build statistics from each run are
stored locally and used to sort the queue at the beginning of the next
run.")
    (home-page "https://github.com/tmm1/test-queue")
    (license license:expat)))

(define-public ruby-rubocop
  (package
    (name "ruby-rubocop")
    (version "1.48.1")
    (source
     (origin
       (method git-fetch)               ;no tests in distributed gem
       (uri (git-reference
             (url "https://github.com/rubocop/rubocop")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1l4j99mbrdjy2bzcnky30pjgjv8sxjr187jzliyqmldvpf7dizbp"))))
    (build-system ruby-build-system)
    (arguments
     `(#:test-target "default"
       ;; TODO: Figure out why test hangs.
       #:tests? ,(not (or (%current-target-system)
                          (target-riscv64?)))
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'set-home
           (lambda _
             (setenv "HOME" (getcwd))))
         ;; Rubocop depends on itself for tests, directly and indirectly. By
         ;; regenerating the TODO list we test rubocop against itself and
         ;; forgo adjusting the test suite to our environment each release.
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (make-file-writable ".rubocop_todo.yml")
               (invoke "./exe/rubocop" "--auto-gen-config")))))))
    (native-inputs
     (list ruby-pry
           ruby-rake
           ruby-rspec
           ruby-rubocop-ast
           ruby-rubocop-capybara-minimal
           ruby-rubocop-minimal
           ruby-rubocop-performance-minimal
           ruby-rubocop-rake-minimal
           ruby-rubocop-rspec-minimal
           ruby-simplecov
           ruby-stackprof
           ruby-test-queue
           ruby-webmock
           ruby-yard))
    (propagated-inputs
     (list ruby-json
           ruby-parallel
           ruby-parser
           ruby-progressbar
           ruby-rainbow
           ruby-regexp-parser
           ruby-rexml
           ruby-rubocop-ast
           ruby-unicode-display-width))
    (synopsis "Ruby code style checking tool")
    (description
     "@code{rubocop} is a Ruby code style checking tool.  It aims to enforce
the community-driven Ruby Style Guide.")
    (home-page "https://github.com/rubocop/rubocop")
    (license license:expat)))

(define-public ruby-rubocop-minimal
  (hidden-package
   (package
     (inherit ruby-rubocop)
     (arguments
      (substitute-keyword-arguments (package-arguments ruby-rubocop)
        ((#:tests? _ #f) #f)))
     (propagated-inputs '())
     (native-inputs '()))))

(define-public ruby-contest
  (package
    (name "ruby-contest")
    (version "0.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "contest" version))
       (sha256
        (base32
         "1p9f2292b7b0fbrcjswvj9v01z7ig5ig52328wyqcabgb553qsdf"))))
    (build-system ruby-build-system)
    (synopsis "Write declarative tests using nested contexts")
    (description
     "Contest allows writing declarative @code{Test::Unit} tests using nested
contexts without performance penalties.")
    (home-page "https://github.com/citrusbyte/contest")
    (license license:expat)))

(define-public ruby-creole
  (package
    (name "ruby-creole")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "creole" version))
       (sha256
        (base32
         "00rcscz16idp6dx0dk5yi5i0fz593i3r6anbn5bg2q07v3i025wm"))))
    (build-system ruby-build-system)
    (native-inputs
     (list ruby-bacon))
    (synopsis "Creole markup language converter")
    (description
     "Creole is a lightweight markup language and this library for converting
creole to @code{HTML}.")
    (home-page "https://github.com/minad/creole")
    (license license:ruby)))

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

(define-public ruby-middleware
  (package
    (name "ruby-middleware")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "middleware" version))
       (sha256
        (base32
         "0703nkf2v371wqr41c04x5qid7ww45cxqv3hnlg07if3b3xrm9xl"))))
    (build-system ruby-build-system)
    (arguments '(#:tests? #f))          ;no test suite
    (synopsis "Implementation of a middleware abstraction for Ruby")
    (description "Middleware is a generalized implementation of a middleware
abstraction for Ruby.")
    (home-page "https://github.com/mitchellh/middleware")
    (license license:expat)))

(define-public ruby-benchmark-ips
  (package
    (name "ruby-benchmark-ips")
    (version "2.12.0")
    (source
     (origin
       (method git-fetch)               ;no tests in distributed gem
       (uri (git-reference
             (url "https://github.com/evanphx/benchmark-ips")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "19pa2a1lgjzrxcz6vxwfiq5qq337vr15bbbpc2mfwzljdlx5059s"))))
    (build-system ruby-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch
            (lambda _
              (substitute* "Gemfile"
                (("gem 'rake'.*")
                 "gem 'rake'"))
              (substitute* "benchmark-ips.gemspec"
                (("git ls-files -- examples lib")
                 "find examples lib -type f | sort")))))))
    (native-inputs
     (list ruby-hoe))
    (synopsis "Iterations per second enhancement for the Ruby Benchmark module")
    (description "Benchmark-ips enhances the Ruby Benchmark module with the
iterations per second count.  For short snippets of code, it can automatically
figure out how many times to run the code to get interesting data.")
    (home-page "https://github.com/evanphx/benchmark-ips")
    (license license:expat)))

(define-public ruby-ffi-rzmq-core
  (package
    (name "ruby-ffi-rzmq-core")
    (version "1.0.7")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "ffi-rzmq-core" version))
       (sha256
        (base32
         "0amkbvljpjfnv0jpdmz71p1i3mqbhyrnhamjn566w0c01xd64hb5"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
                  (add-after 'unpack 'patch-libzmq-search-path
                    (lambda* (#:key inputs #:allow-other-keys)
                      (let ((zeromq (assoc-ref inputs "zeromq")))
                        (substitute* "lib/ffi-rzmq-core/libzmq.rb"
                          (("/usr/local/lib")
                           (string-append zeromq "/lib")))
                        #t)))
                  (replace 'check
                    (lambda _
                      (invoke "rspec"))))))
    (native-inputs
     (list ruby-rspec))
    (inputs
     (list zeromq))
    (propagated-inputs (list ruby-ffi))
    (synopsis "Low-level Ruby FFI wrapper for the ZeroMQ networking library")
    (description "This library only provides the FFI wrapper for the ZeroMQ
networking library.  It can be used to implement a Ruby API for the ZeroMQ
library.")
    (home-page "https://github.com/chuckremes/ffi-rzmq-core")
    (license license:expat)))

(define-public ruby-ffi-rzmq
  (package
    (name "ruby-ffi-rzmq")
    (version "2.0.7")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "ffi-rzmq" version))
       (sha256
        (base32
         "14a5kxfnf8l3ngyk8hgmk30z07aj1324ll8i48z67ps6pz2kpsrg"))))
    (build-system ruby-build-system)
    (arguments '(#:phases (modify-phases %standard-phases
                            (replace 'check
                              (lambda* (#:key tests? #:allow-other-keys)
                                (when tests?
                                  (invoke "rspec")))))))
    (native-inputs
     (list ruby-rspec))
    (propagated-inputs
     (list ruby-ffi-rzmq-core))
    (synopsis "High-level Ruby wrapper for the ZeroMQ networking library")
    (description "This library provides a high-level API that wraps the ZeroMQ
networking library using the Ruby foreign function interface (FFI).  It is a
pure Ruby wrapper, hence is compatible with any Ruby runtime that has support
for FFI.")
    (home-page "https://github.com/chuckremes/ffi-rzmq")
    (license license:expat)))

(define-public ruby-sawyer
  (package
    (name "ruby-sawyer")
    (version "0.9.2")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "sawyer" version))
       (sha256
        (base32
         "1jks1qjbmqm8f9kvwa81vqj39avaj9wdnzc531xm29a55bb74fps"))))
    (build-system ruby-build-system)
    (propagated-inputs
     (list ruby-addressable ruby-faraday))
    (synopsis "Experimental hypermedia agent for Ruby")
    (description "Sawyer is an experimental hypermedia agent for Ruby built on
top of Faraday.")
    (home-page "https://github.com/lostisland/sawyer")
    (license license:expat)))

(define-public ruby-octokit
  (package
    (name "ruby-octokit")
    (version "6.1.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/octokit/octokit.rb")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "02bcmh0b0v80cis1l80lhzxw8adb69xkz6qgg4m7qcmj3y5arwmk"))))
    (build-system ruby-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-unnecessary-dependencies
            (lambda _
              (substitute* "spec/spec_helper.rb"
                (("require 'pry-byebug'") "")))))))
    (native-inputs
     (list ruby-faraday-multipart
           ruby-jwt
           ruby-mime-types
           ruby-multi-json
           ruby-netrc
           ruby-rbnacl
           ruby-rspec
           ruby-simplecov
           ruby-webmock
           ruby-vcr-expat))
    (propagated-inputs (list ruby-faraday ruby-sawyer))
    (synopsis "Ruby toolkit for the GitHub API")
    (description "Octokit wraps the GitHub API in a flat API client that
follows Ruby conventions and requires little knowledge of REST.")
    (home-page "https://github.com/octokit/octokit.rb")
    (license license:expat)))

(define-public ruby-hashicorp-checkpoint
  (package
    (name "ruby-hashicorp-checkpoint")
    (version "0.1.5")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "hashicorp-checkpoint" version))
              (sha256
               (base32
                "1z6mwzvd7p2wqhmk07dwrhvm0ncgqm7pxn0pr2k025rwsspp9bsd"))))
    (build-system ruby-build-system)
    (arguments
     (list #:tests? #f)) ;; no need to test, useless outside HashiCorp anyway
    (synopsis "Internal HashiCorp service to check version information")
    (description "This package is probably useless outside of internal
HashiCorp use.  It is open source for disclosure and because HashiCorp's open
source projects must be able to link to it.")
    (home-page "https://github.com/hashicorp/ruby-checkpoint")
    (license license:mpl2.0)))

(define-public ruby-vagrant-cloud
  (package
    (name "ruby-vagrant-cloud")
    (version "3.1.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/hashicorp/vagrant_cloud")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0bnjd8b86lrgj5ar1l7pg5if95bv0sxa75mz7x2ikqyz6q8rmjb3"))))
    (build-system ruby-build-system)
    (arguments
     `(#:test-target "spec"))
    (native-inputs (list ruby-rspec ruby-webmock))
    (propagated-inputs (list ruby-excon ruby-log4r ruby-rexml))
    (synopsis "Vagrant Cloud API library")
    (description "This library provides the functionality to create, modify,
and delete boxes, versions, and providers on HashiCorp's Vagrant Cloud.")
    (home-page "https://github.com/hashicorp/vagrant_cloud")
    (license license:asl2.0)))

(define-public ruby-libvirt
  (package
    (name "ruby-libvirt")
    (version "0.8.2")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "ruby-libvirt" version))
              (sha256
               (base32
                "0v6vj5vs9v01zr00bflqpfczhwcyc6jdf8k2dqn42lq6d87si77d"))))
    (build-system ruby-build-system)
    (arguments
     (list
      #:tests? #f)) ; tests require access to libvirt socket
    (native-inputs (list pkg-config))
    (inputs (list libvirt))
    (synopsis "Ruby bindings for libvirt")
    (description "This package provides Ruby language binding for libvirt's
native C API.")
    (home-page "https://ruby.libvirt.org/")
    (license license:lgpl2.1+)))

(define-public ruby-fog-core
  (package
    (name "ruby-fog-core")
    (version "2.4.0")
    (source (origin
              (method git-fetch)        ; for tests
              (uri (git-reference
                    (url "https://github.com/fog/fog-core")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "184vpi81az7raz98652m7d98ikabdl9di37dgal0adr76q57j03c"))))
    (build-system ruby-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'set-home
            (lambda _
              (setenv "HOME" "/tmp"))))))
    (native-inputs (list ruby-minitest-stub-const))
    (propagated-inputs (list ruby-builder ruby-excon ruby-formatador
                             ruby-mime-types))
    (synopsis "Shared classes and tests for fog providers and services")
    (description "@code{fog} is a Ruby cloud services library.  This package
provides shared classes and tests for @code{fog} providers and services.")
    (home-page "https://github.com/fog/fog-core")
    (license license:expat)))

(define-public ruby-fog-json
  (package
    (name "ruby-fog-json")
    (version "1.2.0")
    (source (origin
              (method git-fetch)        ; for tests
              (uri (git-reference
                    (url "https://github.com/fog/fog-json")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0f4hbmhy22b1gbkzd3pnj5xvljp6bl7inc2y4fxh29nrmcn4pgb0"))))
    (build-system ruby-build-system)
    (native-inputs (list ruby-minitest))
    (propagated-inputs (list ruby-fog-core ruby-multi-json))
    (synopsis "JSON parsing tools used by @code{fog} providers")
    (description "This package containse the JSON parsing tools shared between
a number of providers in the @code{fog} gem.  @code{fog} is a Ruby cloud
services library.")
    (home-page "https://github.com/fog/fog-json")
    (license license:expat)))

(define-public ruby-fog-xml
  (package
    (name "ruby-fog-xml")
    (version "0.1.4")
    (source (origin
              (method git-fetch)        ; for tests
              (uri (git-reference
                    (url "https://github.com/fog/fog-xml")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0d0n201qzcjxis5wb26bi3s7yfhlmqkwsl6lb9w4szq3b8l1xbwn"))))
    (build-system ruby-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          ;; Run tests via bundler so rake picks up the minitest gem from
          ;; native-inputs, not the one installed otherwise.  This is required
          ;; since turn@0.9.7 needs minitest@4 and can not be upgraded to
          ;; minitest@5.
          (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "bundle" "exec" "rake")))))))
    (native-inputs (list ruby-minitest-4 ruby-turn ruby-pry ruby-coveralls))
    (propagated-inputs (list ruby-fog-core ruby-nokogiri))
    (synopsis "XML parsing tools used by @code{fog} providers")
    (description "This package containse the XML parsing tools shared between
a number of providers in the @code{fog} gem.  @code{fog} is a Ruby cloud
services library.")
    (home-page "https://github.com/fog/fog-xml")
    (license license:expat)))

(define-public ruby-fog-libvirt
  (package
    (name "ruby-fog-libvirt")
    (version "0.12.0")
    (source (origin
              (method git-fetch)        ; for tests
              (uri (git-reference
                    (url "https://github.com/fog/fog-libvirt")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0b1qafb0l24anmr8fviwhp9vj14g0fic0mkg9n1i11h68zhqkj2v"))))
    (build-system ruby-build-system)
    (native-inputs (list ruby-minitest-stub-const ruby-mocha ruby-net-ssh
                         ruby-netrc ruby-octokit ruby-pry ruby-rubocop
                         ruby-shindo ruby-simplecov ruby-yard ))
    (propagated-inputs (list ruby-fog-core ruby-fog-json ruby-fog-xml
                             ruby-json ruby-libvirt))
    (synopsis "Ruby libvirt provider, either standalone or as a module for
@code{fog}")
    (description "This library can be used as a module for @code{fog} or as
standalone libvirt provider.  @code{fog} is a Ruby cloud services library.")
    (home-page "https://github.com/fog/fog-libvirt")
    (license license:expat)))

(define-public ruby-pry-byebug
  (package
    (name "ruby-pry-byebug")
    (version "3.10.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/deivid-rodriguez/pry-byebug")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0z8rhvmr9qmlbk8c8h6jbig5qd5xbdg9qihvx3g0cv1whqzbfikq"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-after 'unpack 'sanitize-dependencies
                    (lambda _
                      (substitute* "Rakefile"
                        ((".*chandler/tasks.*") ""))))
                  (add-before 'check 'set-home
                    (lambda _
                      (setenv "HOME" (getcwd)))))))
    (native-inputs (list ruby-rubocop ruby-simplecov))
    (propagated-inputs (list ruby-byebug ruby-pry))
    (synopsis "Step-by-step debugging and stack navigation in Pry")
    (description "This package adds step-by-step debugging and stack
navigation capabilities to @code{pry}, using @code{byebug}.")
    (home-page "https://github.com/deivid-rodriguez/pry-byebug")
    (license license:expat)))

(define-public ruby-stackprof
  (package
    (name "ruby-stackprof")
    (version "0.2.26")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "stackprof" version))
       (sha256
        (base32 "1gdqqwnampxmc54nf6zfy9apkmkpdavzipvfssmjlhnrrjy8qh7f"))))
    (build-system ruby-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'patch-gemspec
            (lambda _
              (substitute* "stackprof.gemspec"
                (("rake-compiler.*")
                 "rake-compiler>.freeze, [\"> 0.9\"])\n")
                (("mocha.*")
                 "mocha>.freeze, [\"> 0.14\"])\n"))))
          (add-before 'check 'skip-dubious-test
            (lambda _
              (substitute* "test/test_stackprof.rb"
                ;; This unreliable test can fail with "Expected 0 to be >= 1."
                (("def test_(cputime)" _ name)
                 (string-append "def skip_" name))
                ;; This test often fails
                (("def test_gc") "def skip_test_gc")
                ;; This test is known to fail on 32-bit systems.
                ;; /gnu/store/...-stackprof-0.2.26.gem
                (("def test_raw") "def skip_test_raw"))))
          (add-before 'check 'build-tests
            (lambda _
              (invoke "rake" "compile")))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "bundle" "exec" "rake" "default")))))))
    (native-inputs
     (list bundler ruby-mocha-1 ruby-rake-compiler))
    (synopsis "Sampling profiler for Ruby code")
    (description
     "@code{stackprof} is a fast sampling profiler for Ruby code, with cpu,
wallclock and object allocation samplers.")
    (home-page "https://github.com/tmm1/stackprof")
    (license license:expat)))

(define-public ruby-binding-of-caller
  (package
    (name "ruby-binding-of-caller")
    (version "0.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "binding_of_caller" version))
       (sha256
        (base32
         "05syqlks7463zsy1jdfbbdravdhj9hpj5pv2m74blqpv8bq4vv5g"))))
    (build-system ruby-build-system)
    ;; Attempting to run the test suite fails with a rake deprecation error
    ;; (see: https://github.com/banister/binding_of_caller/issues/76).
    (arguments '(#:tests? #f))
    (propagated-inputs
     (list ruby-debug-inspector))
    (synopsis "Retrieve the binding of a method's caller")
    (description "The @code{binding_of_caller} module provides the
@code{Binding#of_caller} method.  It allows accessing bindings from upper
frames in the call stack and can evaluate code in that context.")
    (home-page "https://github.com/banister/binding_of_caller")
    (license license:expat)))

(define-public ruby-pry-stack-explorer
  (package
    (name "ruby-pry-stack-explorer")
    (version "0.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "pry-stack_explorer" version))
       (sha256
        (base32
         "157rd2n9pfvcmmicm0xkq8z4p6famaj13syrpra6b4032qpb1wn0"))))
    (build-system ruby-build-system)
    (arguments '(#:tests? #f))          ;no test suite in gem release
    (propagated-inputs
     (list ruby-binding-of-caller ruby-pry))
    (synopsis "Call-stack navigation plugin for the Pry REPL")
    (description "@code{pry-stack_explorer} is a plugin for the Pry REPL that
add support to navigate the call-stack.")
    (home-page "https://github.com/pry/pry-stack_explorer")
    (license license:expat)))

(define-public ruby-varint
  (package
    (name "ruby-varint")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "varint" version))
       (sha256
        (base32
         "1y0l2qc64cwsyv76ygg9bbjnk86riz2kq73kmn87gdrlmpiyrdac"))))
    (build-system ruby-build-system)
    (arguments '(#:tests? #f))          ;no test suite
    (synopsis "Variable length integers (varint) C extension for Ruby")
    (description "This package provides a small C extension to speed up
variable length integers (varint) in Ruby Protocol Buffers.")
    (home-page "https://github.com/liquidm/varint")
    (license license:bsd-3)))

(define-public ruby-version-gem
  (package
    (name "ruby-version-gem")
    (version "1.1.3")
    (source (origin
              (method git-fetch)        ;for tests
              (uri (git-reference
                    (url "https://gitlab.com/oauth-xx/version_gem")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1wazx2jr9vx5wm48fy8bccvfwhg7y2s8shfw9q81dhb4yvwk6gbf"))))
    (build-system ruby-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'relax-requirements
            (lambda _
              (substitute* "Gemfile"
                (("^linting = .*")
                 "linting = false\n")
                (("^coverage = .*")
                 "coverage = false\n")
                (("^debug = .*")
                 "debug = false\n"))
              (substitute* "spec/spec_helper.rb"
                (("^RUN_COVERAGE = .*")
                 "RUN_COVERAGE = false\n")
                (("^ALL_FORMATTERS = .*")
                 "ALL_FORMATTERS = false\n"))))
          (add-before 'build 'drop-signing-key-requirement
            (lambda _
              (substitute* "version_gem.gemspec"
                (("spec.signing_key =.*")
                 "spec.signing_key = nil")))))))
    (native-inputs (list ruby-rspec ruby-rspec-block-is-expected))
    (synopsis "Improved @code{Version} module for Ruby")
    (description "VersionGem aims to provide introspection of a @code{Version}
module based on a @code{Version::VERSION} constant string wile not interfering
with gemspec parsing where the @code{VERSION} string is traditionally used.")
    (home-page "https://gitlab.com/oauth-xx/version_gem")
    (license license:expat)))

;;; Note: Do NOT update to a newer version; this is the last commit that is
;;; still licensed as free software, the project having switched to the
;;; Hippocratic license afterward (see:
;;; https://github.com/vcr/vcr/issues/959).
(define-public ruby-vcr-expat
  (let ((revision "0")
        (commit-dont-touch "842b2bf89099dc91f2c643d0d85d1abd54eb7e85")) ;
    (package
      (name "ruby-vcr-expat")
      (version (git-version "5.0.0" revision commit-dont-touch))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/vcr/vcr")
                      (commit commit-dont-touch)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "02mzifd2655kjh35bbry01n66jjcjrsw6ncqlybfkjcaqrw2zalv"))))
      (build-system ruby-build-system)
      (arguments (list #:tests? #f))    ;avoid all dependencies
      (home-page "https://github.com/vcr/vcr")
      (synopsis "HTTP interaction recorder [old version]")
      (description "Record your test suite's HTTP interactions and replay them
during future test runs for fast, deterministic, accurate tests.  This is an
older version of VCR that is free software under the Expat license.  The
project later switched to the Hippocratic license, which is non-free.
@emph{Do not use it in new free software projects}.")
      (license license:expat))))

(define-public ruby-ruby-prof
  (package
    (name "ruby-ruby-prof")
    (version "1.4.5")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "ruby-prof" version))
       (sha256
        (base32
         "09n13bzm1p956z318xx1v7ikqdp2i971v7p3kwf3170axz368ccy"))))
    (build-system ruby-build-system)
    (arguments
      ;; FIXME: Investigate why the tests fail on i686-linux.
     `(#:tests? ,(not (or (%current-target-system)
                          (target-x86-32?)))
       #:phases
       (modify-phases %standard-phases
         ;; The LineNumbersTest test fails non-deterministically (see:
         ;; https://github.com/ruby-prof/ruby-prof/issues/276).
         (add-after 'extract-gemspec 'delete-flaky-test
           (lambda _
             (delete-file "test/line_number_test.rb")
             (substitute* "ruby-prof.gemspec"
               (("\"test/line_number_test\\.rb\"\\.freeze, ") ""))))
         (add-before 'check 'compile
          (lambda _
            (invoke "rake" "compile"))))))
    (native-inputs (list bundler ruby-minitest ruby-rake-compiler ruby-rdoc))
    (synopsis "Fast code profiler for Ruby")
    (description "RubyProf is a fast code profiler for Ruby.  Its features
include:
@table @asis
@item Speed
Being a C extension, it is many times faster than the standard Ruby profiler.
@item Measurement Modes
It can measure program wall time, process time, object allocations and memory
usage.
@item Reports
A variety of text and cross-referenced HTML reports can be generated.
@item Threads
Profiling multiple threads simultaneously is supported.
@end table")
    (home-page "https://github.com/ruby-prof/ruby-prof")
    (license license:bsd-2)))

(define-public ruby-ruby-memcheck
  (package
    (name "ruby-ruby-memcheck")
    (version "1.3.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Shopify/ruby_memcheck")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0fj4j4d062sw2kx2qlj877gjbj1xbb691njr8x9nbah6615idlni"))))
    (build-system ruby-build-system)
    (arguments
     (list
      ;; The tests seem to fail on 32bit x86
      #:tests? (not (or (target-x86-32?)
                        (%current-target-system)))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-valgrind-path
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "lib/ruby_memcheck/configuration.rb"
                (("DEFAULT_VALGRIND = \"valgrind\"")
                 (format #f "DEFAULT_VALGRIND = ~s"
                         (search-input-file inputs "bin/valgrind"))))))
          (add-before 'replace-git-ls-files 'standardize-git-ls-files
            (lambda _
              (substitute* "ruby_memcheck.gemspec"
                (("%x\\(git ls-files -z)")
                 "`git ls-files -z`")))))))
    (native-inputs (list ruby-rake-compiler ruby-rspec))
    (inputs (list valgrind/pinned))
    (propagated-inputs (list ruby-nokogiri))
    (synopsis "Valgrind memcheck tool for Ruby")
    (description "The @code{ruby_memcheck} gem provides a sane way to use
Valgrind's memcheck on your native extension gem, that filters out all the
false positives caused by Ruby not freeing all of the memory it allocates
during shutdown.")
    (home-page "https://github.com/Shopify/ruby_memcheck")
    (license license:expat)))

(define-public ruby-memory-profiler
  (package
    (name "ruby-memory-profiler")
    (version "1.0.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/SamSaffron/memory_profiler")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1z1x0rymfv45gh1y3s46w5pga5y8cvgn228jiwlnhc8hin3zig84"))))
    (build-system ruby-build-system)
    (synopsis "Memory profiling routines for Ruby")
    (description
     "This package provides memory profiling routines for Ruby.")
    (home-page "https://github.com/SamSaffron/memory_profiler")
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

(define-deprecated ruby-gherkin ruby-cucumber-gherkin)

(define-public ruby-gherkin-ruby
  (package
    (name "ruby-gherkin-ruby")
    (version "0.3.2")
    (home-page "https://github.com/codegram/gherkin-ruby")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "gherkin-ruby" version))
              (sha256
               (base32
                "18ay7yiibf4sl9n94k7mbi4k5zj2igl4j71qcmkswv69znyx0sn1"))))
    (build-system ruby-build-system)
    (synopsis "Pure Ruby Gherkin parser")
    (description
     "Gherkin-ruby is a Gherkin parser written in pure Ruby and less than
200 lines of code.")
    ;; XXX: No license information anywhere but Readme.md.
    (license license:expat)))

(define-public ruby-aruba
  (package
    (name "ruby-aruba")
    (version "2.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/cucumber/aruba")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1mmlgqhi6yww3z34hmrrnha2rygkv6kx0q962z31dqxjkcv23yfd"))))
    (build-system ruby-build-system)
    (arguments
     (list
      ;; XXX: Only run the "spec" target and not the "cucumber" one, as it is
      ;; slow and has multiple unexplained test failures.
      #:test-target "spec"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-paths
            (lambda _
              ;; This test file relies on a dynamically generated script;
              ;; patch its #!/bin/bash shebang.
              (substitute* "spec/aruba/api/commands_spec.rb"
                (("/bin/bash")
                 (which "bash")))))
          (add-before 'check 'relax-requirements
            ;; Many development requirements are not actually needed.
            (lambda _
              (substitute* "aruba.gemspec"
                (("\\[\">= 0.18.0\", \"< 0.22.0\"]") ;simplecov
                 "\">= 0.18.0\"")
                ((".*appraisal.*") "")
                ((".*pry.*") "")
                ((".*kramdown.*") "")
                ((".*rubocop.*") "")
                ((".*yard-junk.*") ""))
              (substitute* "Rakefile"
                ((".*require \"rubocop/rake_task\".*") "")
                ((".*require \"yard-junk/rake\".*") "")
                ((".*RuboCop::RakeTask.new.*") "")
                ((".*YardJunk::Rake.define_task.*") ""))))
          ;; The tests rely on the Gem being installed, so move the check
          ;; phase after the install phase.
          (delete 'check)
          (add-after 'install 'check
            (assoc-ref %standard-phases 'check))
          (add-before 'check 'set-GEM_PATH
            (lambda _
              (setenv "GEM_PATH" (string-append
                                  (getenv "GEM_PATH") ":"
                                  #$output "/lib/ruby/vendor_ruby"))))
          (add-before 'check 'set-home
            (lambda _
              (setenv "HOME" "/tmp"))))))
    (native-inputs
     (list ruby-rake-manifest
           ruby-rspec
           ruby-simplecov))
    (propagated-inputs
     (list bundler
           ruby-childprocess
           ruby-contracts
           ruby-cucumber
           ruby-rspec-expectations
           ruby-thor))
    (synopsis "Test command-line applications with Cucumber, RSpec or Minitest")
    (description
     "Aruba is an extension for Cucumber, RSpec and Minitest for testing
command-line applications.  It supports applications written in any
language.")
    (home-page "https://github.com/cucumber/aruba")
    (license license:expat)))

(define-public ruby-sys-uname
  (package
  (name "ruby-sys-uname")
  (version "1.2.1")
  (source
    (origin
      (method url-fetch)
      (uri (rubygems-uri "sys-uname" version))
      (sha256
        (base32
          "00p3wwvkdbg6pl38bchaagncv3i4fq4y0ks470imwykjanpy2ic0"))))
  (build-system ruby-build-system)
  (arguments
   `(#:test-target "spec"))
  (propagated-inputs (list ruby-ffi))
  (native-inputs (list ruby-rspec))
  (synopsis "Ruby interface for gathering system information")
  (description "The sys-uname library provides an interface for gathering
information about your current platform.  It allows retrieving information
such as the OS name, OS version, system name, etc.")
  (home-page "https://github.com/djberg96/sys-uname")
  (license license:asl2.0)))

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

(define-public ruby-cucumber
  (package
    (name "ruby-cucumber")
    (version "8.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cucumber/cucumber-ruby")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1dz880fdz6rfbh1nwwcq21v65byik46jnf9gppnrqf3p5k61i55r"))))
    (build-system ruby-build-system)
    (arguments
     (list #:test-target "spec"
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'disable-rubocop
                 ;; Remove extraneous Lint checks.
                 (lambda _
                   (substitute* "Rakefile"
                     (("require 'rubocop/rake_task'")
                      "")
                     (("RuboCop::RakeTask.new")
                      ""))))
               (add-after 'extract-gemspec 'strip-version-requirements
                 (lambda _
                   (delete-file "Gemfile") ;do not use Bundler
                   (substitute* "cucumber.gemspec"
                     ;; The dependency specifications are often trailing
                     ;; behind and appear stricter than necessary, since the
                     ;; test suite passes with the newer component versions.
                     (("(.*add_.*dependency '[_A-Za-z0-9-]+')(.*)"
                       _ stripped rest)
                      (string-append stripped "   # " rest "\n")))))
               (add-before 'check 'set-home
                 (lambda _
                   (setenv "HOME" (getcwd)))))))
    (propagated-inputs
     (list ruby-builder
           ruby-cucumber-ci-environment
           ruby-cucumber-core
           ruby-cucumber-gherkin
           ruby-cucumber-html-formatter
           ruby-cucumber-messages
           ruby-cucumber-wire
           ruby-diff-lcs
           ruby-mime-types
           ruby-multi-test
           ruby-sys-uname))
    (native-inputs
     (list ruby-cucumber-compatibility-kit
           ruby-nokogiri
           ruby-pry
           ruby-webrick
           ruby-rspec))
    (synopsis "Describe automated tests in plain language")
    (description "Cucumber is a tool for running automated tests written in
plain language.  It's designed to support a Behaviour Driven Development (BDD)
software development workflow.")
    (home-page "https://cucumber.io/")
    (license license:expat)))

(define ruby-cucumber-without-tests
  (package (inherit ruby-cucumber)
    (arguments
     '(#:tests? #f))
    (native-inputs
     '())))

(define-public ruby-covered
  (package
    (name "ruby-covered")
    (version "0.20.2")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "covered" version))
              (sha256
               (base32
                "04fpj493jn23ah5fq93956a5h2xj3z0hrckvc26fxcfsg5pbwypa"))))
    (build-system ruby-build-system)
    (arguments
    ;; XXX: The test suite is disabled to avoid dependency cycles with
    ;; ruby-samovar, through ruby-bake.
     (list #:tests? #f
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'extract-gemspec 'relax-requirements
                 (lambda _
                   (substitute* ".gemspec"
                     (("\">= 3.2\"")
                      "\">= 2.7\"")))))))
    (propagated-inputs (list ruby-console ruby-msgpack))
    (synopsis "Modern approach to code coverage in Ruby")
    (description "Covered uses modern Ruby features to generate comprehensive
coverage, including support for templates which are compiled into Ruby.  It
has the following features:
@itemize
@item
Incremental coverage -- if you run your full test suite, and the run a subset,
it will still report the correct coverage - so you can incrementally work on
improving coverage.
@item
Integration with RSpec, Minitest, Travis & Coveralls - no need to configure
anything - out of the box support for these platforms.
@item
It supports coverage of views -- templates compiled to Ruby code can be
tracked for coverage reporting.
@end itemize")
    (home-page "https://github.com/ioquatix/covered")
    (license license:expat)))

(define-public ruby-coveralls
  (package
    (name "ruby-coveralls")
    (version "0.8.23")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "coveralls" version))
       (sha256
        (base32
         "1mv4fn5lfxhy7bc2f1lpnc5yp9mvv97az77j4r7jgrxcqwn8fqxc"))))
    (build-system ruby-build-system)
    (arguments
     (list #:test-target "default"
           #:phases #~(modify-phases %standard-phases
                        (add-after 'extract-gemspec 'disable-problematic-tests
                          (lambda _
                            (substitute* "spec/coveralls/coveralls_spec.rb"
                              ;; This test fails for unknown reasons (perhaps
                              ;; networking); skip it.
                              (("it \"sends existing test results\"" all)
                               (string-append "x" all)))
                            ;; 3 simplecov tests fail, perhaps due to using a
                            ;; newer simplecov version.
                            (delete-file "spec/coveralls/simplecov_spec.rb")
                            (substitute* "coveralls-ruby.gemspec"
                              (("\"spec/coveralls/simplecov_spec.rb\".freeze, ")
                               ""))))
                        (add-after 'extract-gemspec 'relax-requirements
                          (lambda _
                            (substitute* "coveralls-ruby.gemspec"
                              (("%q<simplecov>.freeze, \\[\"~> 0.16.1\"]")
                               "%q<simplecov>.freeze")))))))
    (native-inputs
     (list git-minimal/pinned
           ruby-pry
           ruby-rspec
           ruby-truthy
           ruby-vcr-expat
           ruby-webmock))
    (propagated-inputs
     (list ruby-json
           ruby-term-ansicolor
           ruby-thor
           ruby-tins
           ruby-simplecov))
    (synopsis "Ruby implementation of the Coveralls API")
    (description "This package provides a Ruby implementation of the Coveralls
API.")
    (home-page "https://coveralls.io")
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
           ruby-coveralls
           ruby-rubocop
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

(define-public ruby-skiptrace
  (package
    (name "ruby-skiptrace")
    (version "0.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "skiptrace" version))
       (sha256
        (base32
         "1qpjy6pqd8hx4w7bai64jsr10mwbpnnb65wcbssyqcnalimi1s12"))))
    (build-system ruby-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-git-from-gemspec
            (lambda _
              (substitute* "skiptrace.gemspec"
                (("`git ls-files -z`") "`find . -type f -print0 |sort -z`"))))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "bundle" "exec" "rake" "default")))))))
    (native-inputs
     (list bundler ruby-rake-compiler))
    (synopsis "Provides access for bindings relating to Ruby exceptions")
    (description
     "@code{skiptrace} provides a way to access the bindings that relate to
exceptions in Ruby, providing more information about the context in which the
exception occurred.")
    (home-page "https://github.com/gsamokovarov/skiptrace")
    (license license:expat)))

(define-public ruby-bio-logger
  (package
    (name "ruby-bio-logger")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "bio-logger" version))
       (sha256
        (base32
         "02pylfy8nkdqzyzplvnhn1crzmfkj1zmi3qjhrj2f2imlxvycd28"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f)) ; rake errors, missing shoulda
    (propagated-inputs
     (list ruby-log4r))
    (synopsis "Log4r wrapper for Ruby")
    (description "Bio-logger is a wrapper around Log4r adding extra logging
features such as filtering and fine grained logging.")
    (home-page "https://github.com/pjotrp/bioruby-logger-plugin")
    (license license:expat)))

(define-public ruby-yajl-ruby
  (package
    (name "ruby-yajl-ruby")
    (version "1.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "yajl-ruby" version))
       (sha256
        (base32
         "1lni4jbyrlph7sz8y49q84pb0sbj82lgwvnjnsiv01xf26f4v5wc"))))
    (build-system ruby-build-system)
    (arguments
     (list
      #:test-target "spec"
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'patch-test-to-update-load-path
            (lambda _
              (substitute* "spec/parsing/large_number_spec.rb"
                (("require \"yajl\"")
                 "$LOAD_PATH << 'lib'; require 'yajl'")))))))
     (native-inputs
      (list ruby-rake-compiler ruby-rspec))
     (synopsis "Streaming JSON parsing and encoding library for Ruby")
     (description
      "Ruby C bindings to the Yajl JSON stream-based parser library.  The API
is compatible with the JSON gem, so yajl-ruby can act as a drop in
replacement.

A modified copy of yajl is used, and included in the package.")
     (home-page "https://github.com/brianmario/yajl-ruby")
     (license (list license:expat     ; Ruby code, yajl_ext.c and yajl_ext.h
                    license:bsd-3)))) ; Included, modified copy of yajl

(define-public ruby-yard
  (package
    (name "ruby-yard")
    (version "0.9.34")
    (source
     (origin
       (method git-fetch)
       ;; Tests do not pass if we build from the distributed gem.
       (uri (git-reference
             (url "https://github.com/lsegal/yard")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "10jq0hyzyy0d6l63jxld32g36fhrclkb3rwnyp47igcik73kbagb"))))
    (build-system ruby-build-system)
    (arguments
     (list
      #:test-target "default"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'do-not-set-date-in-gemspec
            ;; Fix a reproducibility issue (see:
            ;; https://github.com/lsegal/yard/issues/1343).
            (lambda _
              (substitute* "yard.gemspec"
                ((".*s\\.date.*") ""))))
          (add-before 'check 'prepare-for-tests
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (substitute* "Rakefile"
                  ((".*[Ss]amus.*") ""))
                ;; Delete the Gemfile to avoid errors relating to it.
                (delete-file "Gemfile")
                ;; $HOME needs to be set to somewhere writeable for tests to
                ;; run.
                (setenv "HOME" "/tmp")
                ;; This test fails
                ;; #<File (class)> received :open with unexpected arguments
                ;; expected: ("gem1.gem", "rb")
                ;; got: ("/gnu/store/.../lib/ruby/vendor_ruby/specifications/asciidoctor-2.0.18.gemspec", "r:UTF-8:-")
                (substitute* "spec/cli/diff_spec.rb"
                  (("it \"searches for .gem file")
                   "xit \"searches for .gem file"))))))))
    (native-inputs
     (list ruby-rspec
           ruby-rack
           ruby-redcloth
           ruby-webrick
           ruby-asciidoctor/minimal))
    (synopsis "Documentation generation tool for Ruby")
    (description "YARD is a documentation generation tool for the Ruby
programming language.  It enables the user to generate consistent, usable
documentation that can be exported to a number of formats very easily, and
also supports extending for custom Ruby constructs such as custom class level
definitions.")
    (home-page "https://yardoc.org")
    (license license:expat)))

(define-public ruby-yard/minimal
  (hidden-package
   (package
     (inherit ruby-yard)
     (arguments
      (ensure-keyword-arguments
       (package-arguments ruby-yard)
       (list #:tests? #f)))
     (native-inputs '()))))

(define-public ruby-spectroscope
  (package
    (name "ruby-spectroscope")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "spectroscope" version))
       (sha256
        (base32
         "0iiid9sm110qhx0i1zkds710cvsnmhd308wbqa7slkzbq2akrb3y"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (with-output-to-file ".test"
               (lambda _
                 (display
                  "\
require 'ae/should'
require 'rspec'

include RSpec

Test.run :default do |run|
  run.files << 'spec/*_spec.rb'
end")))
             (invoke "ruby" "-Ilib" "-rrubytest" ".test"))))))
    (native-inputs
     (list ruby-ae ruby-rspec))
    (propagated-inputs
     (list ruby-rubytest))
    (synopsis "Behavior-Driven Development (BDD) framework built on RubyTest")
    (description "Spectroscope is a Behavior-Driven Development (BDD)
framework built on RubyTest, designed to emulate RSpec in most respects.  It
is assertion framework independent so any number of assertion systems can be
used, such as Assay or AE.")
    (home-page "http://rubyworks.github.com/spectroscope/")
    (license license:bsd-2)))

(define-public ruby-tomparse
  (package
    (name "ruby-tomparse")
    (version "0.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "tomparse" version))
       (sha256
        (base32
         "06xakk41f1kgj6j1ahkwn4r6cvidixvm4phhlrvmwb7c3pr8ygc8"))))
    (build-system ruby-build-system)
    ;; TODO: Tests require citron and rulebow, not yet packaged.
    (arguments '(#:tests? #f))
    (synopsis "TomDoc parser for Ruby")
    (description "TomParse is a TomDoc parser for Ruby.  It takes a code
comment as input and parses it into a convenient object-oriented structure in
accordance with the TomDoc standard.  See
@url{https://github.com/mojombo/tomdoc, TomDoc} for more information about the
TomDoc format.")
    (home-page "http://rubyworks.github.com/tomparse/")
    (license license:bsd-2)))

(define-public ruby-yard-tomdoc
  (package
    (name "ruby-yard-tomdoc")
    (version "0.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "yard-tomdoc" version))
       (sha256
        (base32
         "1725gs8b8klpwhrvnf2wwp7dw3zxs9vz2la983l2d8c4r4fn1j2z"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (replace 'check
                    (lambda _
                      (invoke "rubytest" "-Ilib" "-Itest" "test/"))))))
    (native-inputs
     (list ruby-rubytest-cli ruby-spectroscope ruby-ae))
    (propagated-inputs
     (list ruby-tomparse ruby-yard))
    (synopsis "TomDoc syntax for YARD")
    (description "This module adds support for the TomDoc documentation format
to YARD, a documentation generation tool for Ruby.")
    (home-page "http://rubyworks.github.com/yard-tomdoc/")
    (license license:expat)))

(define-public ruby-clap
  (package
    (name "ruby-clap")
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "clap" version))
              (sha256
               (base32
                "190m05k3pca72c1h8k0fnvby15m303zi0lpb9c478ad19wqawa5q"))))
    (build-system ruby-build-system)
    ;; Clap needs cutest for running tests, but cutest needs clap.
    (arguments `(#:tests? #f))
    (synopsis "Command line argument parsing for simple applications")
    (description
     "Clap provides command line argument parsing features.  It covers the
simple case of executing code based on the flags or parameters passed.")
    (home-page "https://github.com/djanowski/cutest")
    (license license:expat)))

(define-public ruby-cutest
  (package
    (name "ruby-cutest")
    (version "1.2.2")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "cutest" version))
              (sha256
               (base32
                "1mldhjn62g53vx4gq2qdqg2lgjvyrqxa8d0khf8347bbfgi16d32"))))
    (build-system ruby-build-system)
    (propagated-inputs
     (list ruby-clap))
    (synopsis "Run tests in separate processes")
    (description
     "Cutest runs tests in separate processes to avoid shared state.")
    (home-page "https://github.com/djanowski/cutest")
    (license license:expat)))

(define-public ruby-pygmentize
  (package
    (name "ruby-pygmentize")
    (version "0.0.3")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "pygmentize" version))
              (sha256
               (base32
                "1pxryhkiwvsz6xzda3bvqwz5z8ggzl1cdglf8qbcf4bb7akirdpb"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-pygmentize-path
          (lambda _
            (substitute* "lib/pygmentize.rb"
              (("\"/usr/bin/env python.*")
               (string-append "\"" (which "pygmentize") "\"\n")))
            #t))
         (add-after 'build 'do-not-use-vendor-directory
          (lambda _
            ;; Remove bundled pygments sources
            ;; FIXME: ruby-build-system does not support snippets.
            (delete-file-recursively "vendor")
            (substitute* "pygmentize.gemspec"
              (("\"vendor/\\*\\*/\\*\",") ""))
            #t)))))
    (inputs
     `(("pygments" ,python-pygments)))
    (native-inputs
     (list ruby-cutest ruby-nokogiri))
    (synopsis "Thin Ruby wrapper around pygmentize")
    (description
     "Pygmentize provides a simple way to call pygmentize from within a Ruby
application.")
    (home-page "https://github.com/djanowski/pygmentize")
    (license license:expat)))

(define-public ruby-event-emitter
  (package
    (name "ruby-event-emitter")
    (version "0.2.6")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "event_emitter" version))
              (sha256
               (base32
                "148k9qv8102l3m6klc24dwip79f9y4bjr5z19dckd7ffbjyrf9n7"))))
    (build-system ruby-build-system)
    (arguments
     (list #:phases #~(modify-phases %standard-phases
                        (add-after 'extract-gemspec 'relax-requirements
                          (lambda _
                            (substitute* "event_emitter.gemspec"
                              (("~> 1.15") ">= 1.15")))))))
    (synopsis "Ruby port of EventEmitter from Node.js")
    (description
     "This package provides a Ruby port of EventEmitter from Node.js.")
    (home-page "https://shokai.github.io/event_emitter/")
    (license license:expat)))

(define-public ruby-eventmachine
  (package
    (name "ruby-eventmachine")
    (version "1.2.7")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "eventmachine" version))
       (sha256
        (base32
         "0wh9aqb0skz80fhfn66lbpr4f86ya2z5rx6gm5xlfhd05bj1ch4r"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f))               ; test suite tries to connect to google.com
    (inputs
     (list openssl))
    (native-inputs
     (list ruby-rake-compiler))
    (synopsis "Single-threaded network event framework for Ruby")
    (description
     "EventMachine implements a single-threaded engine for arbitrary network
communications.  EventMachine wraps all interactions with sockets, allowing
programs to concentrate on the implementation of network protocols.  It can be
used to create both network servers and clients.")
    ;; The ‘official’ rubyeventmachine.com domain is now registrar-squatted.
    (home-page "https://github.com/eventmachine/eventmachine")
    (license (list license:ruby license:gpl3)))) ; GPLv3 only AFAICT

(define-public ruby-ruby-engine
  (package
    (name "ruby-ruby-engine")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "ruby_engine" version))
       (sha256
        (base32
         "0wqdcv8gxybp1y7kjhh18g3r9dczacs62d4ahcvyhz32bih8c9fm"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'extract-gemspec 'clean-up
           (lambda _
             (delete-file "Gemfile.lock")
             (substitute* "ruby_engine.gemspec"
               ;; Remove unnecessary imports that would entail further
               ;; dependencies.
               ((".*<rdoc.*") "")
               ((".*<rubygems-tasks.*") "")
               ;; Remove extraneous .gem file
               (("\"pkg/ruby_engine-[0-9.]+\\.gem\".freeze, ") "")
               (("\"Gemfile.lock\".freeze, ") "")
               ;; Soften rake dependency
               (("%q<rake>.freeze, \\[\"~> 10.0\"\\]")
                "%q<rake>.freeze, [\">= 10.0\"]")
               ;; Soften the rspec dependency
               (("%q<rspec>.freeze, \\[\"~> 2.4\"\\]")
                "%q<rspec>.freeze, [\">= 2.4\"]"))
             (substitute* "Rakefile"
               (("require 'rubygems/tasks'") "")
               (("Gem::Tasks.new") ""))
             ;; Remove extraneous .gem file that otherwise gets installed.
             (delete-file-recursively "pkg")
             #t)))))
    (native-inputs
     (list bundler ruby-rake ruby-rspec))
    (synopsis "Simplifies checking for Ruby implementation")
    (description
     "@code{ruby_engine} provides an RubyEngine class that can be used to
check which implementation of Ruby is in use.  It can provide the interpreter
name and provides query methods such as @{RubyEngine.mri?}.")
    (home-page "https://github.com/janlelis/ruby_engine")
    (license license:expat)))

(define-public ruby-turn
  (package
    (name "ruby-turn")
    (version "0.9.7")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "turn" version))
       (sha256
        (base32
         "1691rc2sq04cw8mxxh340k2j04ll90kwgcy8ddrp6rligmfrf8fw"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; Tests fail because turn changes its environment so can no longer
         ;; find test/unit.  Instead simply test if the executable runs
         ;; without issue.
         (replace 'check
           (lambda _
             (invoke "ruby" "-Ilib" "bin/turn" "-h"))))))
    (propagated-inputs
     (list ruby-ansi ruby-minitest-4))
    (synopsis "Alternate set of alternative runners for MiniTest")
    (description
     "TURN provides a set of alternative runners for MiniTest which are both
colorful and informative.  TURN displays each test on a separate line with
failures being displayed immediately instead of at the end of the tests.  Note
that TURN is no longer being maintained.")
    (home-page "https://rubygems.org/gems/turn")
    (license license:expat)))

(define-public ruby-mimemagic
  (package
    (name "ruby-mimemagic")
    (version "0.3.3")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "mimemagic" version))
       (sha256
        (base32 "04cp5sfbh1qx82yqxn0q75c7hlcx8y1dr5g3kyzwm4mx6wi2gifw"))))
    (build-system ruby-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         ;; This phase breaks the tests, as it patches some of the test data.
         (delete 'patch-source-shebangs))))
    (native-inputs
     (list ruby-bacon))
    (synopsis "Ruby library for MIME detection by extension or content")
    (description
     "@acronym{MIME, Multipurpose Internet Mail Extensions} detection by
extension or content, using the freedesktop.org.xml shared-mime-info
database.")
    (home-page "https://github.com/minad/mimemagic")
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

(define-public ruby-mini-mime
  (package
    (name "ruby-mini-mime")
    (version "1.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "mini_mime" version))
        (sha256
         (base32
          "0lbim375gw2dk6383qirz13hgdmxlan0vc5da2l072j3qw6fqjm5"))))
    (build-system ruby-build-system)
    (synopsis "Lightweight mime type lookup toy")
    (description "This package provides a lightweight mime type lookup toy.")
    (home-page "https://github.com/discourse/mini_mime")
    (license license:expat)))

(define-public ruby-fileutils
  (package
    (name "ruby-fileutils")
    (version "1.7.0")
    (source (origin
              (method git-fetch)        ;for tests
              (uri (git-reference
                    (url "https://github.com/ruby/fileutils")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0m68gaqclnc2wflilmrnr19kky2gr6fjf6k3yq02sf9scs281kid"))))
    (build-system ruby-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'disable-problematic-tests
            (lambda _
              (substitute* "test/fileutils/test_fileutils.rb"
                ;; The 'test_chown' tests depends on /etc/groups, which
                ;; doesn't exist in the build environment.
                (("def test_chown.*" all)
                 (string-append all "    return true\n"))
                ;; The 'test_cp_r_dev' expects a RuntimeError to be raised
                ;; when copying a char device to a directory, but this is not
                ;; triggered in the build environment, for reasons unknown.
                (("def test_cp_r_dev.*" all)
                 (string-append all "    return true\n"))))))))
    (synopsis "Ruby utility library to manipulate files")
    (description "The FileUtils Ruby library includes Several file utility
methods for copying, moving, removing, etc.")
    (home-page "https://github.com/ruby/fileutils")
    (license license:bsd-2)))

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

(define-public ruby-sqlite3
  (package
    (name "ruby-sqlite3")
    (version "1.6.3")
    (source
     (origin
       (method git-fetch)        ;for tests
       (uri (git-reference
             (url "https://github.com/sparklemotion/sqlite3-ruby")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0ijj8z8fpk2lczydkxv71k250g5gd8ip8klsscxc9f16b01gh9qs"))))
    (build-system ruby-build-system)
    (arguments
     (list
      #:gem-flags #~(list "--" "--enable-system-libraries")
      #:phases
      #~(modify-phases %standard-phases
          (delete 'check)
          (add-after 'install 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (setenv "GEM_PATH"
                        (string-append (getenv "GEM_PATH") ":"
                                       #$output "/lib/ruby/vendor_ruby"))
                (invoke "rake" "test")))))))
    (propagated-inputs
     (list ruby-mini-portile-2))
    (inputs
     (list sqlite))
    (native-inputs
     (list ruby-hoe
           ruby-ruby-memcheck
           ruby-rake-compiler
           ruby-rake-compiler-dock))
    (synopsis "Interface with SQLite3 databases")
    (description
     "This module allows Ruby programs to interface with the SQLite3 database
engine.")
    (home-page
     "https://github.com/sparklemotion/sqlite3-ruby")
    (license license:bsd-3)))

(define-public ruby-shoulda-context
  (package
    (name "ruby-shoulda-context")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "shoulda-context" version))
       (sha256
        (base32
         "0d1clcp92jv8756h09kbc55qiqncn666alx0s83za06q5hs4bpvs"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               ;; Do not run tests to avoid circular dependence with rails.
               ;; Instead just import the library to test.
               (invoke "ruby" "-Ilib" "-r" "shoulda-context")))))))
    (synopsis "Test::Unit context framework extracted from Shoulda")
    (description
     "@code{shoulda-context} is the context framework extracted from Shoulda.
Instead of writing Ruby methods with lots_of_underscores, shoulda-context adds
context, setup, and should blocks combine to produce natural test method
names.")
    (home-page "https://github.com/thoughtbot/shoulda-context")
    (license license:expat)))

(define-public ruby-shoulda-matchers
  (package
    (name "ruby-shoulda-matchers")
    (version "5.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "shoulda-matchers" version))
       (sha256
        (base32
         "11igjgh16dl5pwqizdmclzlzpv7mbmnh8fx7m9b5kfsjhwxqdfpn"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               ;; Do not run tests to avoid circular dependence with rails.  Instead
               ;; just import the library to test.
               (invoke "ruby" "-Ilib" "-r" "shoulda-matchers")))))))
    (propagated-inputs
     (list ruby-activesupport))
    (synopsis "Collection of testing matchers extracted from Shoulda")
    (description
     "Shoulda Matchers provides RSpec- and Minitest-compatible one-liners that
test common Rails functionality.  These tests would otherwise be much longer,
more complex, and error-prone.")
    (home-page "https://github.com/thoughtbot/shoulda-matchers")
    (license license:expat)))

(define-public ruby-shoulda
  (package
    (name "ruby-shoulda")
    (version "4.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "shoulda" version))
       (sha256
        (base32
         "02lww34kn1g6lidp4rx4rs6bqvirrzxlfw1y2wm11aif8f622xz6"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               ;; Don't run tests to avoid circular dependence with rails.
               ;; Instead just import the library to test.
               (invoke "ruby" "-Ilib" "-r" "shoulda"))))
         (add-after 'extract-gemspec 'relax-requirements
           (lambda _
             (substitute* "shoulda.gemspec"
               ;; An older version of shoulda-matchers (4.0) is used, out of
               ;; little maintenance rather than because of an real
               ;; incompatibility (see:
               ;; https://github.com/thoughtbot/shoulda/issues/275).
               ((", \\[\"~> 4.0\"]") "")))))))
    (propagated-inputs
     (list ruby-shoulda-context ruby-shoulda-matchers))
    (synopsis "Context framework and matchers for testing")
    (description
     "@code{shoulda} is a meta-package combining @code{shoulda-context} and
@code{shoulda-matchers} providing tools for writing tests.")
    (home-page "https://github.com/thoughtbot/shoulda")
    (license license:expat)))

(define-public ruby-unf
  (package
    (name "ruby-unf")
    (version "0.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "unf" version))
       (sha256
        (base32
         "0bh2cf73i2ffh4fcpdn9ir4mhq8zi50ik0zqa1braahzadx536a9"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'add-dependency-to-bundler
           (lambda _
             ;; test-unit is required but not provided by the bundler
             ;; environment.  This is fixed in the upstream repository but fix
             ;; has not been released.
             (substitute* "Gemfile"
               (("^gemspec") "gem 'test-unit'\ngemspec"))
             #t)))))
    (propagated-inputs
     (list ruby-unf-ext))
    (native-inputs
     (list ruby-shoulda bundler ruby-test-unit))
    (synopsis "Unicode Normalization Form support to Ruby and JRuby")
    (description
     "@code{ruby-unf} is a wrapper library to bring Unicode Normalization Form
support to both Ruby and JRuby.  It uses @code{unf_ext} on CRuby and
@code{java.text.Normalizer} on JRuby.")
    (home-page "https://github.com/knu/ruby-unf")
    (license license:bsd-2)))

(define-public ruby-warden
  (package
    (name "ruby-warden")
    (version "1.2.8")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "warden" version))
       (sha256
        (base32
         "1fr9n9i9r82xb6i61fdw4xgc7zjv7fsdrr4k0njchy87iw9fl454"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ; No included tests
    (propagated-inputs
     (list ruby-rack))
    (synopsis "Rack middleware providing authentication")
    (description
     "Warden is a Rack-based middleware that provides a mechanism for
authentication in Ruby web applications.")
    (home-page "https://github.com/wardencommunity/warden")
    (license license:expat)))

(define-public ruby-warden-oauth2
  (package
    (name "ruby-warden-oauth2")
    (version "0.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "warden-oauth2" version))
       (sha256
        (base32
         "1z9154lvzrnnfjbjkmirh4n811nygp6pm2fa6ikr7y1ysa4zv3cz"))))
    (build-system ruby-build-system)
    (arguments
     '(#:test-target "spec"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-unnecessary-dependencies
           (lambda _
             (substitute* "Gemfile"
               ;; All of these gems relate to development, and are unnecessary
               ;; when running the tests
               (("gem 'guard-bundler'") "")
               (("gem 'guard'") "")
               (("gem 'guard-rspec'") "")
               (("gem 'rb-fsevent'") "")
               (("gem 'pry'") "")
               (("gem 'growl'") ""))
             #t))
         ;; The test suite doesn't work with rspec@2, and this is incompatible
         ;; with the current version of Rake, so invoke Rspec directly
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "bundle" "exec" "rspec"))
             #t)))))
    (propagated-inputs
     (list ruby-warden))
    (native-inputs
     (list bundler ruby-rspec-2 ruby-rack-test))
    (synopsis "OAuth 2.0 strategies for Warden")
    (description
     "This library extends Warden to support OAuth 2.0 authorized API
requests.")
    (home-page "https://github.com/opperator/warden-oauth2")
    (license license:expat)))

(define-public ruby-webmock
  (package
    (name "ruby-webmock")
    (version "3.11.2")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "webmock" version))
       (sha256
        (base32
         "1hdlbvfw316lkz251qnfk79drmaay7l51kidvicz41nhvw12xz8v"))))
    (build-system ruby-build-system)
    (native-inputs
     (list bundler ruby-rspec))
    (propagated-inputs
     (list ruby-addressable ruby-crack ruby-hashdiff))
    (synopsis "Allows stubbing and setting expectations on HTTP requests")
    (description
     "WebMock allows stubbing HTTP requests and setting expectations on HTTP
requests.  This is useful when testing software.")
    (home-page "https://github.com/bblimke/webmock")
    (license license:expat)))

(define-public ruby-webmock-2
  (package
    (inherit ruby-webmock)
    (name "ruby-webmock")
    (version "2.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "webmock" version))
       (sha256
        (base32
         "04hkcqsmbfnp8g237pisnc834vpgildklicbjbyikqg0bg1rwcy5"))))))

(define-public ruby-unicode-display-width
  (package
    (name "ruby-unicode-display-width")
    (version "2.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "unicode-display_width" version))
       (sha256
        (base32
         "1gi82k102q7bkmfi7ggn9ciypn897ylln1jk9q67kjhr39fj043a"))))
    (build-system ruby-build-system)
    (arguments
     '(;; Test data not included.
       #:tests? #f))
    (synopsis "Determine the monospace display width of Ruby strings")
    (description
     "@code{Unicode::DisplayWidth} is a Ruby library which can determine the
display width of strings in Ruby.")
    (home-page "https://github.com/janlelis/unicode-display_width")
    (license license:expat)))

;; There is another gem called 'ruby-version' so we use an underscore in this
;; name
(define-public ruby_version
  (package
    (name "ruby_version")
    (version "1.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "ruby_version" version))
       (sha256
        (base32
         "0lvc7bd5ps3w2vq2wb02i0pi3vfcx2rnckx2ix4rjym1qf52kb2j"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'fix-dependencies
           (lambda _
             ;; Remove the Gemfile.lock, as we want to use Guix packages at
             ;; whatever versions.
             (delete-file "Gemfile.lock")
             ;; Remove the included gem files as they unnecessary.
             (delete-file-recursively "pkg/")
             ;; Accept any version of rake, rdoc and rspec
             (substitute* "ruby_version.gemspec"
               (("%q<rake.*") "%q<rake>)\n")
               (("%q<rdoc.*") "%q<rdoc>)\n")
               (("%q<rspec.*") "%q<rspec>)\n"))
             ;; Do not use bundler.
             (substitute* "Rakefile"
               (("Bundler\\.setup.*") "nil\n"))
             #t)))))
    (native-inputs
     (list ruby-rdoc ruby-rspec ruby-rubygems-tasks))
    (synopsis "Ruby library to help check the Ruby version")
    (description "@code{ruby_version} provides a @code{RubyVersion} module to simplify
checking for the right Ruby version in software.")
    (home-page "https://github.com/janlelis/ruby_version")
    (license license:expat)))

(define-public ruby-websocket-client-simple
  (package
    (name "ruby-websocket-client-simple")
    (version "0.6.1")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "websocket-client-simple" version))
              (sha256
               (base32
                "1ypl4xvlh5c99zbn20sifv7gv04zi20ly464vsgikfrpn5f37bid"))))
    (build-system ruby-build-system)
    (arguments (list #:test-target "default"))
    (native-inputs (list ruby-eventmachine ruby-websocket-eventmachine-server))
    (propagated-inputs (list ruby-event-emitter ruby-websocket))
    (synopsis "Simple WebSocket client for Ruby")
    (description "This package provides a simple WebSocket client for Ruby.")
    (home-page "https://github.com/ruby-jp/websocket-client-simple")
    (license license:expat)))

(define-public ruby-websocket-eventmachine-base
  (package
    (name "ruby-websocket-eventmachine-base")
    (version "1.2.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "websocket-eventmachine-base" version))
              (sha256
               (base32
                "0wiz61mcwgs3k21cdr5l9b4jpg29gl6mflfampww2v525yc3hr1r"))))
    (build-system ruby-build-system)
    (arguments
     (list #:tests? #f)) ;no test suite
    (propagated-inputs (list ruby-eventmachine ruby-websocket
                             ruby-websocket-native))
    (synopsis "WebSocket base for Ruby client and server")
    (description "This package provides a WebSocket base for a Ruby client and
server.")
    (home-page "https://github.com/imanel/websocket-eventmachine-base")
    (license license:expat)))

(define-public ruby-websocket-eventmachine-server
  (package
    (name "ruby-websocket-eventmachine-server")
    (version "1.0.1")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "websocket-eventmachine-server" version))
              (sha256
               (base32
                "0iqpzc8s028nck1flqaj784gvyn64wy1h3svpa4y2847wklg8sms"))))
    (build-system ruby-build-system)
    ;; TODO: The test suite requires Autobahn, not yet packaged in Guix.
    (arguments (list #:tests? #f))
    (propagated-inputs (list ruby-websocket-eventmachine-base))
    (synopsis "WebSocket server for Ruby")
    (description "This package provides a WebSocket server for Ruby.")
    (home-page "https://github.com/imanel/websocket-eventmachine-server")
    (license license:expat)))

(define-public ruby-websocket-native
  (package
    (name "ruby-websocket-native")
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "websocket-native" version))
              (sha256
               (base32
                "1kgvd3gyzh7hk0ddzn85jrs4csxm54qnvla95ldyn6rzgfbjchdn"))))
    (build-system ruby-build-system)
    (arguments
     (list #:test-target "spec"
           #:phases #~(modify-phases %standard-phases
                        (add-before 'check 'disable-problematic-tests
                          (lambda _
                            (substitute* "spec/websocket_spec.rb"
                              (("it \"should have mask_native defined\"" all)
                               (string-append "x" all)))))
                        (add-after 'build 'compile
                          (lambda _
                            (invoke "rake" "compile"))))))
    (native-inputs (list ruby-rake-compiler ruby-rspec))
    (synopsis "Native Ruby extension for the WebSocket gem")
    (description "This package provides a native extension that can increase
performance by about 25% compared to the pure Ruby WebSocket implementation.")
    (home-page "https://github.com/imanel/websocket-ruby-native")
    (license license:expat)))

(define-public ruby-websocket-driver
  (package
   (name "ruby-websocket-driver")
   (version "0.7.1")
   (source
    (origin
     (method url-fetch)
     (uri (rubygems-uri "websocket-driver" version))
     (sha256
      (base32 "1bxamwqldmy98hxs5pqby3andws14hl36ch78g0s81gaz9b91nj2"))))
   (build-system ruby-build-system)
   (arguments
    '(#:tests? #f))                     ; no included tests
   (propagated-inputs
    (list ruby-websocket-extensions))
   (synopsis "WebSocket protocol handler with pluggable I/O")
   (description
    "@code{websocket-driver} provides a complete implementation of the
WebSocket protocols that can be hooked up to any TCP library")
   (home-page "https://github.com/faye/websocket-driver-ruby")
   (license license:expat)))

(define-public ruby-websocket-extensions
  (package
    (name "ruby-websocket-extensions")
    (version "0.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "websocket-extensions" version))
       (sha256
        (base32
         "034sdr7fd34yag5l6y156rkbhiqgmy395m231dwhlpcswhs6d270"))))
    (build-system ruby-build-system)
    (arguments
     '(;; No included tests
       #:tests? #f))
    (synopsis "Generic extension manager for WebSocket connections")
    (description
     "@code{websocket-extensions} provides a container for registering
extension plugins.")
    (home-page "https://github.com/faye/websocket-extensions-ruby")
    (license license:expat)))

(define-public ruby-domain-name
  (package
    (name "ruby-domain-name")
    (version "0.5.20190701")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "domain_name" version))
       (sha256
        (base32 "0lcqjsmixjp52bnlgzh4lg9ppsk52x9hpwdjd53k8jnbah2602h0"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'fix-versions
           (lambda _
             ;; Fix NameError that appears to already be fixed upstream.
             (substitute* "Rakefile"
               (("DomainName::VERSION")
                "Bundler::GemHelper.gemspec.version"))
             ;; Loosen unnecessarily strict test-unit version specification.
             (substitute* "domain_name.gemspec"
               (("<test-unit>.freeze, \\[\\\"~> 2.5.5") "<test-unit>, [\">0"))
             #t)))))
    (propagated-inputs
     (list ruby-unf))
    (native-inputs
     (list ruby-shoulda bundler ruby-test-unit))
    (synopsis "Domain name manipulation library")
    (description
     "@code{domain_name} is a Domain name manipulation library.  It parses a
domain name ready for extracting the registered domain and TLD (Top Level
Domain).  It can also be used for cookie domain validation based on the Public
Suffix List.")
    (home-page "https://github.com/knu/ruby-domain_name")
    (license license:bsd-2)))

(define-public ruby-dotenv
  (package
    (name "ruby-dotenv")
    (version "2.8.1")
    (source (origin
              (method git-fetch)        ;for the tests
              (uri (git-reference
                    (url "https://github.com/bkeepers/dotenv")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0s1a71jxppa20fsm2rd1vym099ib48m039rmhggmz99hc3z1fvvr"))))
    (build-system ruby-build-system)
    (arguments
     (list
      #:test-target "spec"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'do-not-build-dotenv-rails
            (lambda _
              ;; The repository includes the definitions of two packages,
              ;; 'dotenv' and 'dotenv-rails'.  Since we want to package them
              ;; separately, remove the dotenv-rails and associated Rake
              ;; tasks.
              (delete-file "dotenv-rails.gemspec")
              (delete-file "spec/dotenv/rails_spec.rb")
              (substitute* "Rakefile"
                (("DotenvRailsGemHelper.install_tasks name: \"dotenv-rails\"")
                 "")
                ((", \"dotenv-rails:.*\"")
                 ""))))
          (replace 'replace-git-ls-files
            (lambda _
              (substitute* "dotenv.gemspec"
                (("`git ls-files README.md LICENSE lib bin \\| grep -v rails`")
                 "`find README.md LICENSE lib bin -type f | sort | \
grep -v rails`")))))))
    (native-inputs (list ruby-standard ruby-rspec))
    (synopsis "Ruby library for setting environment variables")
    (description "Dotenv is a Ruby library for setting environment variables
defined in a @file{.env} file.")
    (home-page "https://github.com/bkeepers/dotenv")
    (license license:expat)))

(define-public ruby-dotenv-rails
  (package
    (inherit ruby-dotenv)
    (name "ruby-dotenv-rails")
    (arguments
     (substitute-keyword-arguments (package-arguments ruby-dotenv)
       ((#:phases phases '%standard-phases)
        #~(modify-phases #$phases
            (delete 'do-not-build-dotenv-rails)
            (add-after 'unpack 'delete-Gemfile
              (lambda _
                ;; It defines extraneous dependencies; remove it.
                (delete-file "Gemfile")))
            (add-after 'unpack 'remove-extraneous-gemspec
              (lambda _
                (delete-file "dotenv.gemspec")
                (substitute* "Gemfile"
                  ((".*\"dotenv\".*") ""))
                (substitute* "Rakefile"
                  ;; Remove the dotenv-related Rake tasks.
                  (("Bundler::GemHelper.install_tasks name: \"dotenv\"")
                   "")
                  (("\"dotenv:[^\"]*\", ")
                   ""))))
            (replace 'replace-git-ls-files
              (lambda _
                (substitute* "dotenv-rails.gemspec"
                  (("`git ls-files lib \\| grep rails`")
                   "`find lib -type f | sort | grep rails`"))))))))
    (native-inputs (list ruby-rspec ruby-spring ruby-standard))
    (propagated-inputs (list ruby-dotenv ruby-railties))
    (synopsis "Ruby library for setting environment variables in Rails project")
    (description "Dotenv is a Ruby library for setting environment variables
defined in a @file{.env} file.  This is the Rails variant, adapted for use
with Ruby on Rails projects.")))

(define-public ruby-http-accept
  (package
    (name "ruby-http-accept")
    (version "2.2.0")
    (source (origin
              (method git-fetch)        ;for the tests
              (uri (git-reference
                    (url "https://github.com/socketry/http-accept")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1d69cy12hqbcqrhw4dibvdq5pqklxsa59kih6pzl479nxq79rgs7"))))
    (build-system ruby-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-missing-key-directive
            ;; This seem to be a common problem in Ruby projects (see:
            ;; https://github.com/prawnpdf/ttfunk/issues/99).
            (lambda _
              (substitute* "http-accept.gemspec"
                ((".*spec.signing_key.*") ""))))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "rspec")))))))
    (native-inputs
     (list ruby-rspec
           ruby-covered))
    (synopsis "Parse Accept and Accept-Language HTTP headers")
    (description
     "This package provides a set of parsers for Accept and Accept-Language
HTTP headers.")
    (home-page "https://github.com/socketry/http-accept")
    (license license:expat)))

(define-public ruby-http-accept-1
  (package
    (inherit ruby-http-accept)
    (version "1.7.0")
    (source (origin
              (method git-fetch)        ;for the tests
              (uri (git-reference
                    (url "https://github.com/ioquatix/http-accept")
                    (commit (string-append "v" version))))
              (file-name (git-file-name "ruby-http-accept" version))
              (sha256
               (base32
                "1hnqmqpa135s3xgcvv30qzqm8zp88my1aj05m72d2q9cvc31g92z"))))))

(define-public ruby-http-cookie
  (package
    (name "ruby-http-cookie")
    (version "1.0.5")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "http-cookie" version))
       (sha256
        (base32
         "13rilvlv8kwbzqfb644qp6hrbsj82cbqmnzcvqip1p6vqx36sxbk"))))
    (build-system ruby-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'add-dependency-to-bundler
            (lambda _
              ;; Fix NameError
              (substitute* "Rakefile"
                (("HTTP::Cookie::VERSION")
                 "Bundler::GemHelper.gemspec.version")))))))
    (propagated-inputs
     (list ruby-domain-name))
    (native-inputs
     (list ruby-simplecov
           bundler
           ruby-sqlite3
           ruby-test-unit))
    (synopsis "Handle HTTP Cookies based on RFC 6265")
    (description
     "@code{HTTP::Cookie} is a Ruby library to handle HTTP Cookies based on
RFC 6265.  It has been designed with security, standards compliance and
compatibility in mind, to behave just the same as today's major web browsers.
It has built-in support for the legacy @code{cookies.txt} and
@code{cookies.sqlite} formats of Mozilla Firefox.")
    (home-page "https://github.com/sparklemotion/http-cookie")
    (license license:expat)))

(define-public ruby-httpclient
  (package
    (name "ruby-httpclient")
    (version "2.8.3")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "httpclient" version))
       (sha256
        (base32
         "19mxmvghp7ki3klsxwrlwr431li7hm1lczhhj8z4qihl2acy8l99"))))
    (build-system ruby-build-system)
    (arguments
     '(;; TODO: Some tests currently fail
       ;; ------
       ;; 211 tests, 729 assertions, 13 failures, 4 errors, 0 pendings,
       ;; 2 omissions, 0 notifications
       ;; 91.866% passed
       ;; ------
       ;; 6.49 tests/s, 22.41 assertions/s
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (if tests?
                 (invoke "ruby"
                         "-Ilib"
                         "test/runner.rb")
                 #t))))))
    (native-inputs
     (list ruby-rack))
    (synopsis
     "Make HTTP requests with support for HTTPS, Cookies, authentication and more")
    (description
     "The @code{httpclient} ruby library provides functionality related to
HTTP.  Compared to the @code{net/http} library, @{httpclient} also provides
Cookie, multithreading and authentication (digest, NTLM) support.

Also provided is a @command{httpclient} command, which can perform HTTP
requests either using arguments or with an interactive prompt.")
    (home-page "https://github.com/nahi/httpclient")
    (license license:ruby)))

(define-public ruby-ansi
  (package
    (name "ruby-ansi")
    (version "1.5.0")
    (source
     (origin
       (method git-fetch)
       ;; Fetch from GitHub as the gem does not contain testing code.
       (uri (git-reference
              (url "https://github.com/rubyworks/ansi")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1wsz7xxwl3vkh277jb7fd7akqnqqgbvalxzpjwniiqk8ghfprbi5"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; Disable testing to break the cycle ansi, ae, ansi, as well as the
         ;; cycle ansi, qed, ansi.  Instead simply test that the library can
         ;; be require'd.
         (replace 'check
           (lambda _
             (invoke "ruby" "-Ilib" "-r" "ansi")))
         (add-before 'validate-runpath 'replace-broken-symlink
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (file (string-append
                           out "/lib/ruby/vendor_ruby/gems/ansi-"
                           ,version "/lib/ansi.yml")))
               ;; XXX: This symlink is broken since ruby 2.4.
               ;; https://lists.gnu.org/archive/html/guix-devel/2017-06/msg00034.html
               (delete-file file)
               (symlink "../.index" file)
               #t))))))
    (synopsis "ANSI escape code related libraries")
    (description
     "This package is a collection of ANSI escape code related libraries
enabling ANSI colorization and stylization of console output.  Included in the
library are the @code{Code} module, which defines ANSI codes as constants and
methods, a @code{Mixin} module for including color methods, a @code{Logger}, a
@code{ProgressBar}, and a @code{String} subclass.  The library also includes a
@code{Terminal} module which provides information about the current output
device.")
    (home-page "https://rubyworks.github.io/ansi/")
    (license license:bsd-2)))

(define-public ruby-systemu
  (package
    (name "ruby-systemu")
    (version "2.6.5")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "systemu" version))
       (sha256
        (base32
         "0gmkbakhfci5wnmbfx5i54f25j9zsvbw858yg3jjhfs5n4ad1xq1"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'set-version
           (lambda _
             (setenv "VERSION" ,version)
             #t)))))
    (synopsis "Capture of stdout/stderr and handling of child processes")
    (description
     "Systemu can be used on any platform to return status, stdout, and stderr
of any command.  Unlike other methods like @code{open3} and @code{popen4}
there is no danger of full pipes or threading issues hanging your process or
subprocess.")
    (home-page "https://github.com/ahoward/systemu")
    (license license:ruby)))

(define-public ruby-bcrypt
  (package
    (name "ruby-bcrypt")
    (version "3.1.18")
    ;; FIXME: Unbundle the bcrypt library used.
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "bcrypt" version))
              (sha256
               (base32
                "048z3fvcknqx7ikkhrcrykxlqmf9bzc7l0y5h1cnvrc9n2qf0k8m"))))
    (build-system ruby-build-system)
    (arguments (list #:test-target "default")) ;compile + spec
    (native-inputs (list ruby-rake-compiler ruby-rspec))
    (synopsis  "Ruby bcrypt wrapper")
    (description "This Ruby library provides a simple wrapper to bcrypt, a
secure hash algorithm for hashing passwords.")
    (home-page "https://github.com/bcrypt-ruby/bcrypt-ruby")
    (license license:expat)))

(define-public ruby-bcrypt-pbkdf
  (package
    (name "ruby-bcrypt-pbkdf")
    (version "1.1.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "bcrypt_pbkdf" version))
              (sha256
               (base32
                "0ndamfaivnkhc6hy0yqyk2gkwr6f3bz6216lh74hsiiyk3axz445"))))
    (build-system ruby-build-system)
    (arguments
     (list #:phases #~(modify-phases %standard-phases
                        (add-before 'build 'compile
                          (lambda _
                            (invoke "rake" "compile"))))))
    (native-inputs (list ruby-minitest ruby-rake-compiler
                         ruby-rake-compiler-dock ruby-rdoc))
    (synopsis "Reading password encrypted Ed25519 keys in Ruby")
    (description
     "This gem implements @samp{bcrypt_pbkdf}, which is a variant of
PBKDF2 (Password-Based Key Derivation Function 2) with bcrypt-based
pseudorandom function.  This is currently used by @samp{net-ssh} to
read password encrypted Ed25519 keys.")
    (home-page "https://github.com/net-ssh/bcrypt_pbkdf-ruby")
    (license license:expat)))

(define-public ruby-bio-commandeer
  (package
    (name "ruby-bio-commandeer")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "bio-commandeer" version))
       (sha256
        (base32
         "0khpfw1yl5l3d2m8nxpkk32ybc4c3pa5hic3agd160jdfjjjnlni"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           ;; Run test without calling 'rake' so that jeweler is
           ;; not required as an input.
           (lambda _
             (invoke "rspec" "spec/bio-commandeer_spec.rb"))))))
    (propagated-inputs
     (list ruby-bio-logger ruby-systemu))
    (native-inputs
     (list bundler ruby-rspec))
    (synopsis "Simplified running of shell commands from within Ruby")
    (description
     "Bio-commandeer provides an opinionated method of running shell commands
from within Ruby.  The advantage of bio-commandeer over other methods of
running external commands is that when something goes wrong, messages printed
to the @code{STDOUT} and @code{STDERR} streams are reported, giving extra
detail to ease debugging.")
    (home-page "https://github.com/wwood/bioruby-commandeer")
    (license license:expat)))

(define-public ruby-rubytest
  (package
    (name "ruby-rubytest")
    (version "0.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "rubytest" version))
       (sha256
        (base32
         "19jydsdnkl81i9dhdcr4dc34j0ilm68ff2ngnka1hi38xiw4p5qz"))))
    (build-system ruby-build-system)
    (arguments
     ;; Disable regular testing to break the cycle rubytest, qed, brass,
     ;; rubytest, as well as the cycle rubytest, qed, ansi, rubytest.  Instead
     ;; simply test that the library can be require'd.
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (invoke "ruby" "-Ilib" "-r" "rubytest"))))))
    (propagated-inputs
     (list ruby-ansi))
    (synopsis "Universal test harness for Ruby")
    (description
     "Rubytest is a testing meta-framework for Ruby.  It can handle any
compliant test framework and can run tests from multiple frameworks in a
single pass.")
    (home-page "https://rubyworks.github.io/rubytest")
    (license license:bsd-2)))

(define-public ruby-brass
  (package
    (name "ruby-brass")
    (version "1.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "brass" version))
       (sha256
        (base32
         "154lp8rp1vmg60ri1j4cb8hqlw37z7bn575h899v8hzxwi11sxka"))))
    (build-system ruby-build-system)
    (arguments
     ;; Disable tests to break the cycle brass, lemon, ae, qed, brass.
     ;; Instead simply test that the library can be require'd.
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (invoke "ruby" "-Ilib" "-r" "brass"))))))
    (synopsis "Basic foundational assertions framework")
    (description
     "BRASS (Bare-Metal Ruby Assertion System Standard) is a basic
foundational assertions framework for other assertion and test frameworks to
make use of.")
    (home-page "https://rubyworks.github.io/brass")
    (license license:bsd-2)))

(define-public ruby-qed
  (package
    (name "ruby-qed")
    (version "2.9.2")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "qed" version))
       (sha256
        (base32
         "03h4lmlxpcya8j7s2cnyscqlx8v3xl1xgsw5y1wk1scxcgz2vbmr"))))
    (build-system ruby-build-system)
    (arguments
     ;; Disable testing to break the cycle qed, ansi, qed, among others.
     ;; Instead simply test that the executable runs using --copyright.
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (invoke "ruby" "-Ilib" "bin/qed" "--copyright"))))))
    (propagated-inputs
     (list ruby-ansi ruby-brass))
    (synopsis "Test framework utilizing literate programming techniques")
    (description
     "@dfn{Quality Ensured Demonstrations} (QED) is a test framework for
@dfn{Test Driven Development} (TDD) and @dfn{Behaviour Driven
Development} (BDD) utilizing Literate Programming techniques.  QED sits
somewhere between lower-level testing tools like @code{Test::Unit} and
requirement specifications systems like Cucumber.")
    (home-page "https://rubyworks.github.io/qed")
    (license license:bsd-2)))

(define-public ruby-que
  (package
    (name "ruby-que")
    (version "1.0.0.beta3")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "que" version))
       (sha256
        (base32
         "0gr9pb814d4qj3ds98g6cjrdk7wv0yg8aqbm7c1lmgl87jkg8q04"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ; No included tests
    (synopsis "Job queue using PostgreSQL written in Ruby")
    (description
     "This package provides a job queue that uses PostgreSQL for storing jobs
and locking between worker processes.")
    (home-page "https://github.com/chanks/que")
    (license license:expat)))

(define-public ruby-queue-classic
  (package
    (name "ruby-queue-classic")
    (version "4.0.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "queue_classic" version))
              (sha256
               (base32
                "0npyhajf2fc80apkw9s2kj0n254w5lcl4xpjidg5d5w1fb19abh6"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))      ;tests require a postgresql server
    (native-inputs (list ruby-activerecord ruby-minitest-reporters))
    (propagated-inputs (list ruby-pg))
    (synopsis "Queuing library for Ruby")
    (description "@code{queue_classic} is a queuing library for Ruby
applications (Rails, Sinatra, etc.)  @code{queue_classic} features
asynchronous job polling, database maintained locks and has a single
dependency, @code{pg}.")
    (home-page "https://github.com/QueueClassic/queue_classic")
    (license license:expat)))

(define-public ruby-pairing-heap
  (package
    (name "ruby-pairing-heap")
    (version "3.1.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "pairing_heap" version))
              (sha256
               (base32
                "059kqpw53cancnp0bp7y1s74y1955riw33w3lqfbnms4b4mdh5zj"))))
    (build-system ruby-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch
            (lambda _
              (substitute* "Rakefile"
                (("require \"standard/rake\"") "")
                ((":\"standard:fix\",") "")))))))
    (synopsis "Priority queue in pure Ruby")
    (description "This package provides a performant priority queue in pure
ruby with support for changing priority using pairing heap data structure")
    (home-page "https://github.com/mhib/pairing_heap")
    (license license:expat)))

(define-public ruby-ae
  (package
    (name "ruby-ae")
    (version "1.8.2")
    (source
     (origin
       (method git-fetch)
       ;; Fetch from github so tests are included.
       (uri (git-reference
              (url "https://github.com/rubyworks/ae")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "11299jj5ma8mi7b4majkyjy70y6zlqpgl8aql1c5lvfjavlpwmlp"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _ (invoke "qed")))
         (add-before 'validate-runpath 'replace-broken-symlink
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (file (string-append
                           out "/lib/ruby/vendor_ruby/gems/ae-"
                           ,version "/lib/ae.yml")))
               ;; XXX: This symlink is broken since ruby 2.4.
               ;; https://lists.gnu.org/archive/html/guix-devel/2017-06/msg00034.html
               (delete-file file)
               (symlink "../.index" file)
               #t))))))
    (propagated-inputs
     (list ruby-ansi))
    (native-inputs
     (list ruby-qed))
    (synopsis "Assertions library")
    (description
     "Assertive Expressive (AE) is an assertions library specifically designed
for reuse by other test frameworks.")
    (home-page "https://rubyworks.github.io/ae/")
    (license license:bsd-2)))

(define-public ruby-lemon
  (package
    (name "ruby-lemon")
    (version "0.9.1")
    (source
    (origin
      (method url-fetch)
      (uri (rubygems-uri "lemon" version))
      (sha256
       (base32
        "0gqhpgjavgpvx23rqpfqcv3d5bs8gc7lr9yvj8kxgp7mfbdc2jcm"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check (lambda _ (invoke "qed"))))))
    (propagated-inputs
     (list ruby-ae ruby-ansi ruby-rubytest))
    (native-inputs
     (list ruby-qed))
    (synopsis "Test framework correlating code structure and test unit")
    (description
     "Lemon is a unit testing framework that enforces highly formal
case-to-class and unit-to-method test construction.  This enforcement can help
focus concern on individual units of behavior.")
    (home-page "https://rubyworks.github.io/lemon")
    (license license:bsd-2)))

(define-public ruby-rubytest-cli
  (package
    (name "ruby-rubytest-cli")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "rubytest-cli" version))
       (sha256
        (base32
         "0n7hv4k1ba4fm3i98c6ydbsqhkxgbp52mhi70ba1x3mqzfvk438p"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f)) ; no tests
    (propagated-inputs
     (list ruby-ansi ruby-rubytest))
    (synopsis "Command-line interface for rubytest")
    (description
     "Rubytest CLI is a command-line interface for running tests for
Rubytest-based test frameworks.  It provides the @code{rubytest} executable.")
    (home-page "https://rubyworks.github.io/rubytest-cli")
    (license license:bsd-2)))

(define-public ruby-hashery
  (package
    (name "ruby-hashery")
    (version "2.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "hashery" version))
       (sha256
        (base32
         "0qj8815bf7q6q7llm5rzdz279gzmpqmqqicxnzv066a020iwqffj"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (invoke "qed")
             (invoke "rubytest" "-Ilib" "-Itest" "test/"))))))
    (native-inputs
     (list ruby-rubytest-cli ruby-qed ruby-lemon))
    (synopsis "Hash-like classes with extra features")
    (description
     "The Hashery is a tight collection of @code{Hash}-like classes.
Included are the auto-sorting @code{Dictionary} class, the efficient
@code{LRUHash}, the flexible @code{OpenHash} and the convenient
@code{KeyHash}.  Nearly every class is a subclass of the @code{CRUDHash} which
defines a CRUD (Create, Read, Update and Delete) model on top of Ruby's
standard @code{Hash} making it possible to subclass and augment to fit any
specific use case.")
    (home-page "https://rubyworks.github.io/hashery")
    (license license:bsd-2)))

(define-public ruby-rc4
  (package
    (name "ruby-rc4")
    (version "0.1.5")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "ruby-rc4" version))
       (sha256
        (base32
         "00vci475258mmbvsdqkmqadlwn6gj9m01sp7b5a3zd90knil1k00"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (invoke "rspec" "spec/rc4_spec.rb"))))))
    (native-inputs
     (list ruby-rspec-2))
    (synopsis "Implementation of the RC4 algorithm")
    (description
     "RubyRC4 is a pure Ruby implementation of the RC4 algorithm.")
    (home-page "https://github.com/caiges/Ruby-RC4")
    (license license:expat)))

(define-public ruby-afm
  (package
    (name "ruby-afm")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "afm" version))
       (sha256
        (base32
         "06kj9hgd0z8pj27bxp2diwqh6fv7qhwwm17z64rhdc4sfn76jgn8"))))
    (build-system ruby-build-system)
    (native-inputs
     (list bundler))
    (synopsis "Read Adobe Font Metrics (afm) files")
    (description
     "This library provides methods to read @dfn{Adobe Font Metrics} (afm)
files and use the data therein.")
    (home-page "https://github.com/halfbyte/afm")
    (license license:expat)))

(define-public ruby-ascii85
  (package
    (name "ruby-ascii85")
    (version "1.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "Ascii85" version))
       (sha256
        (base32
         "0658m37jjjn6drzqg1gk4p6c205mgp7g1jh2d00n4ngghgmz5qvs"))))
    (build-system ruby-build-system)
    (native-inputs
     (list bundler))
    (synopsis "Encode and decode Ascii85 binary-to-text encoding")
    (description
     "This library provides methods to encode and decode Ascii85
binary-to-text encoding.  The main modern use of Ascii85 is in PostScript and
@dfn{Portable Document Format} (PDF) file formats.")
    (home-page "https://github.com/datawraith/ascii85gem")
    (license license:expat)))

(define-public ruby-ttfunk
  (package
    (name "ruby-ttfunk")
    (version "1.7.0")
    (source
     (origin
       (method git-fetch)
       ;; Fetch from github as the gem does not contain testing code.
       (uri (git-reference
             (url "https://github.com/prawnpdf/ttfunk")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1jyxn928mpyb1sikjd93s3v8fmh33232pq41ziaph513j7am6fi5"))))
    (build-system ruby-build-system)
    (arguments
     (list #:test-target "spec"         ;avoid the rubocop target
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'remove-missing-key-directive
                 ;; This seem to be a common problem in Ruby projects (see:
                 ;; https://github.com/prawnpdf/ttfunk/issues/99).
                 (lambda _
                   (substitute* "ttfunk.gemspec"
                     ((".*spec.signing_key.*") "")))))))
    (native-inputs (list ruby-prawn-dev))
    (synopsis "Font metrics parser for the Prawn PDF generator")
    (description
     "TTFunk is a TrueType font parser written in pure Ruby.  It is used as
part of the Prawn PDF generator.")
    (home-page "https://github.com/prawnpdf/ttfunk")
    ;; From the README: "Matz's terms for Ruby, GPLv2, or GPLv3. See LICENSE
    ;; for details."
    (license %prawn-project-licenses)))

(define-public ruby-puma
  (package
    (name "ruby-puma")
    (version "6.3.0")
    (source
     (origin
       (method git-fetch)               ;for tests
       (uri (git-reference
             (url "https://github.com/puma/puma")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0qnayzgyr23w87jc849r00394hv1gw2rk9080nws43ilnycagzxq"))))
    (build-system ruby-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-Gemfile
            (lambda _
              (substitute* "Gemfile"
                (("gem \"rake-compiler\".*")
                 "gem 'rake-compiler'\n"))))
          (add-after 'unpack 'disable-rubocop
            (lambda _
              (setenv "PUMA_NO_RUBOCOP" "1")))
          (add-after 'unpack 'use-rack-2
            (lambda _
              (setenv "PUMA_CI_RACK" "rack2")
              (setenv "PUMA_CI_RACK_2" "1")))
          (add-before 'build 'increase-resource-limits
            (lambda _
              ;; The test suite requires a higher number of open files.  Try
              ;; increasing the soft resource limit of max open files to 2048,
              ;; or equal to the hard limit, whichever is lower.
              (call-with-values (lambda () (getrlimit 'nofile))
                (lambda (soft hard)
                  (when (and soft (< soft 2048))
                    (if hard
                        (setrlimit 'nofile (min hard 2048) hard)
                        (setrlimit 'nofile 2048 #f))
                    (format
                     #t "increased maximum number of open files from ~d to ~d~%"
                     soft (if hard (min hard 2048) 2048)))))))
          (add-before 'build 'fix-gemspec
            (lambda _
              (substitute* "puma.gemspec"
                (("`git ls-files -- bin docs ext lib tools`")
                 "`find bin docs ext lib tools -type f |sort`"))))
          (delete 'check)               ;moved after install
          (add-after 'install 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "bundle" "exec" "rake" "test"))))
          (add-after 'install 'delete-mkmf.log
            (lambda _
              ;; Rubygems installs build log files that embed volatile file
              ;; names (see:
              ;; https://github.com/rubygems/rubygems/issues/6259).
              (for-each delete-file (find-files #$output "^mkmf\\.log$"))))
          (add-before 'check 'disable-problematic-tests
            (lambda _
              (let-syntax ((skip-tests
                            (syntax-rules ()
                              ((_ file test ...)
                               (substitute* file
                                 (((string-append "def " test ".*") all)
                                  (string-append
                                   all "    skip('fails on guix')\n")) ...)))))
                ;; The test failures were reported at:
                ;; https://github.com/puma/puma/issues/3093, but appear to be
                ;; caused by the Guix build container, perhaps the lack of
                ;; zombie process reaping (see:
                ;; https://issues.guix.gnu.org/30948).
                ;;  All the tests in the 'test_worker_gem_independence.rb'
                ;;  module fail with "Expected false to be truthy.".
                (delete-file "test/test_worker_gem_independence.rb")
                (skip-tests "test/test_integration_ssl_session.rb"
                            ;; The TLS 1.2 test fails for unknown reasons.
                            "test_off_tls1_2")
                (skip-tests "test/test_integration_cluster.rb"
                            "test_fork_worker_on_refork"
                            "test_hot_restart_does_not_drop_connections"
                            "test_culling_strategy_oldest_fork_worker"
                            "test_usr1_fork_worker")
                (skip-tests "test/test_integration_pumactl.rb"
                            "test_refork_cluster")
                ;; The Openssl certificate has expired, causing these tests to fail.
                (skip-tests "test/test_puma_server_ssl.rb"
                            "test_verify_fail_if_client_expired_cert"
                            "test_verify_client_cert"
                            "test_server_ssl_with_cert_pem_and_key_pem")
                (skip-tests "test/test_integration_ssl.rb"
                            "test_ssl_run_with_curl_client"))))
          (add-before 'check 'relax-test-case-timeout
            (lambda _
              ;; The default value is 45 s and easily causes timeouts.
              (setenv "TEST_CASE_TIMEOUT" "600")))
          (add-before 'check 'set-home
            (lambda _
              ;; Some tests fail if the cannot write to HOME.
              (setenv "HOME" "/tmp")))
          (add-before 'check 'set-paths
            (lambda _
              ;; The test suite requires the 'puma' command to be on PATH.
              (setenv "PATH" (string-append (getenv "PATH") ":"
                                            #$output "/bin"))
              (setenv "GEM_PATH" (string-append
                                  (getenv "GEM_PATH") ":"
                                  #$output "/lib/ruby/vendor_ruby")))))))
    (native-inputs
     (list bundler
           curl
           ruby-json
           ruby-localhost
           ruby-m
           ruby-minitest-proveit
           ruby-minitest-retry
           ruby-minitest-stub-const
           ruby-rack
           ruby-rake-compiler
           ruby-webrick))
    (inputs
     (list openssl
           ruby-nio4r))
    (synopsis "Simple, concurrent HTTP server for Ruby/Rack")
    (description
     "Puma is a simple, fast, threaded, and highly concurrent HTTP 1.1 server
for Ruby/Rack applications.  Puma is intended for use in both development and
production environments.  In order to get the best throughput, it is highly
recommended that you use a Ruby implementation with real threads like Rubinius
or JRuby.")
    (home-page "https://puma.io/")
    (license license:expat)))

(define-public ruby-hoe-git
  (package
    (name "ruby-hoe-git")
    (version "1.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "hoe-git" version))
       (sha256
        (base32
         "10jmmbjm0lkglwxbn4rpqghgg1ipjxrswm117n50adhmy8yij650"))))
    (build-system ruby-build-system)
    (propagated-inputs
     (list ruby-hoe-3))
    (synopsis "Hoe plugins for tighter Git integration")
    (description
     "This package provides a set of Hoe plugins for tighter Git integration.
It provides tasks to automate release tagging and pushing and changelog
generation.")
    (home-page "https://github.com/jbarnette/hoe-git")
    (license license:expat)))

(define-public ruby-hoe-markdown
  (package
    (name "ruby-hoe-markdown")
    (version "1.4.0")
    (home-page "https://github.com/flavorjones/hoe-markdown")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0wb0yjdx9gx9r0cahpx42pblvglgh1i9pdfxjavq7f40nan2g076"))))
    (build-system ruby-build-system)
    (arguments
     '(#:test-target "spec"
       #:phases (modify-phases %standard-phases
                  (add-before 'check 'disable-bundler-dependency
                    (lambda _
                      (substitute* "spec/spec_helper.rb"
                        (("require.*bundler/setup.*")
                         "")))))))
    (native-inputs
     (list ruby-rspec))
    (propagated-inputs
     (list ruby-rake))
    (synopsis "Hoe plugin with Markdown helpers")
    (description
     "This package provides a Hoe plugin with various Markdown helpers, which
can be used to e.g. hyperlink Markdown documentation between project files.")
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

(define-public ruby-timecop
  (package
    (name "ruby-timecop")
    (version "0.9.6")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "timecop" version))
       (sha256
        (base32
         "0dlx4gx0zh836i7nzna03xdl7fc233s5z6plnr6k3kw46ah8d1fc"))))
    (build-system ruby-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'set-check-rubylib
            (lambda _
              ;; Set RUBYLIB so timecop tests finds its own lib.
              (setenv "RUBYLIB" "lib"))))))
    (native-inputs
     (list bundler
           ruby-minitest-rg
           ruby-mocha
           ruby-activesupport
           ruby-pry))
    (synopsis "Test mocks for time-dependent functions")
    (description
     "Timecop provides \"time travel\" and \"time freezing\" capabilities,
making it easier to test time-dependent code.  It provides a unified method to
mock @code{Time.now}, @code{Date.today}, and @code{DateTime.now} in a single
call.")
    (home-page "https://github.com/travisjeffery/timecop")
    (license license:expat)))

(define-public ruby-concurrent-ruby
  (package
    (name "ruby-concurrent-ruby")
    (version "1.2.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference       ;for tests
                    (url "https://github.com/ruby-concurrency/concurrent-ruby")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1vz4rl0nplq14dk9nx45g59i1sk2h53w1mjlrdiyjf780q4a1i38"))))
    (build-system ruby-build-system)
    (arguments
     (list
      #:tests? #f  ;the test suite is run in ruby-concurrent-ruby-edge
      #:phases #~(modify-phases %standard-phases
                   (add-after 'unpack 'delete-unwanted-gemspecs
                     (lambda _
                       (for-each delete-file
                                 '("concurrent-ruby-ext.gemspec"
                                   "concurrent-ruby-edge.gemspec"))))
                   (add-after 'unpack 'do-not-install-concurrent_ruby.jar
                     (lambda _
                       ;; This file is only built when building the Java
                       ;; extension.
                       (substitute* "concurrent-ruby.gemspec"
                         (("'lib/concurrent-ruby/concurrent/concurrent_ruby.jar'")
                          "")))))))
    (synopsis "Concurrency library for Ruby")
    (description "Concurrent Ruby includes concurrency tools such as agents,
futures, promises, thread pools, actors, supervisors, and more.  It is
inspired by Erlang, Clojure, Go, JavaScript, actors, and classic concurrency
patterns.")
    (home-page "https://github.com/ruby-concurrency/concurrent-ruby")
    (license license:expat)))

;;; The 'gem' is called 'concurrent-ruby'; reversing its name was confusing
;;; and failed to be picked by the gem importer (which led to this newer
;;; package).
(define-public ruby-concurrent
  (deprecated-package "ruby-concurrent" ruby-concurrent-ruby))

(define-public ruby-concurrent-ruby-ext
  (package
    (inherit ruby-concurrent-ruby)
    (name "ruby-concurrent-ruby-ext")
    (arguments
     (list
      #:tests? #f                      ;tested as part of concurrent-ruby-edge
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'delete-unwanted-gemspecs
            (lambda _
              (for-each delete-file
                        '("concurrent-ruby.gemspec"
                          "concurrent-ruby-edge.gemspec")))))))
    (native-inputs (list ruby-rake-compiler))
    (propagated-inputs (list ruby-concurrent-ruby))
    (synopsis "C extensions for concurrent-ruby")
    (description "This package provides C extensions to optimize the
concurrent-ruby gem when running under the Matz's Ruby Interpreter (MRI, also
known as CRuby).")))

(define-public ruby-concurrent-ruby-edge
  (package
    (inherit ruby-concurrent-ruby)
    (name "ruby-concurrent-ruby-edge")
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'delete-unwanted-gemspecs
            (lambda _
              (for-each delete-file
                        '("concurrent-ruby.gemspec"
                          "concurrent-ruby-ext.gemspec"))))
          ;; The tests rely on the Gem being installed, so move the check
          ;; phase after the install phase.
          (delete 'check)
          (add-after 'install 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (setenv "GEM_PATH" (string-append
                                  (getenv "GEM_PATH") ":"
                                  #$output "/lib/ruby/vendor_ruby"))
              (when tests?
                (invoke "rake" "ci")))))))
    (native-inputs
     (list ruby-rake-compiler
           ruby-rake-compiler-dock
           ruby-rspec
           ruby-timecop
           ruby-yard))
    (propagated-inputs
     (list ruby-concurrent-ruby ruby-concurrent-ruby-ext))
    (synopsis "Edge features and additions to the @code{concurrent-ruby} gem")
    (description "The @code{concurrent-ruby-edge} gem includes
@code{concurrent-ruby} features that are under active development and may
change frequently.  They are expected not to keep backward
compatibility (there may also lack tests and documentation), although semantic
versions are obeyed though.  Features developed in @code{concurrent-ruby-edge}
are expected to move to @code{concurrent-ruby} when final.")))

(define-public ruby-pkg-config
  (package
    (name "ruby-pkg-config")
    (version "1.2.5")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "pkg-config" version))
       (sha256
        (base32
         "056mzqdh4yjznsg36fi0xiq76f24vxlhzh2n4az919l3x5k318ar"))))
    (build-system ruby-build-system)
    (arguments
     ;; Tests require extra files not included in the gem.
     `(#:tests? #f))
    (synopsis "Detect libraries for compiling Ruby native extensions")
    (description
     "@code{pkg-config} can be used in your extconf.rb to properly detect need
libraries for compiling Ruby native extensions.")
    (home-page "https://github.com/ruby-gnome2/pkg-config")
    (license license:lgpl2.0+)))

(define-public ruby-net-http-digest-auth
  (package
    (name "ruby-net-http-digest-auth")
    (version "1.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "net-http-digest_auth" version))
       (sha256
        (base32
         "1nq859b0gh2vjhvl1qh1zrk09pc7p54r9i6nnn6sb06iv07db2jb"))))
    (build-system ruby-build-system)
    (native-inputs
     (list ruby-hoe
           ruby-minitest))
    (synopsis "RFC 2617 HTTP digest authentication library")
    (description
     "This library implements HTTP's digest authentication scheme based on
RFC 2617.  This enables the use of the digest authentication scheme instead
of the more insecure basic authentication scheme.")
    (home-page "https://github.com/drbrain/net-http-digest_auth")
    (license license:expat)))

(define-public ruby-mail
  (package
    (name "ruby-mail")
    (version "2.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "mail" version))
       (sha256
        (base32
         "00wwz6ys0502dpk8xprwcqfwyf3hmnx6lgxaiq6vj43mkx43sapc"))))
    (build-system ruby-build-system)
    (propagated-inputs
     (list ruby-mini-mime))
    (arguments
     '(#:tests? #f)) ; no rakefile
    (synopsis "Mail library for Ruby")
    (description
     "Mail is an internet library for Ruby that is designed to handle email
generation, parsing and sending.  The purpose of this library is to provide
a single point of access to handle all email functions, including sending
and receiving emails.  All network type actions are done through proxy
methods to @code{Net::SMTP}, @code{Net::POP3} etc.

Mail has been designed with a very simple object oriented system that
really opens up the email messages you are parsing, if you know what you
are doing, you can fiddle with every last bit of your email directly.")
    (home-page "https://github.com/mikel/mail")
    (license license:expat)))

(define-public ruby-net-protocol
  (package
    (name "ruby-net-protocol")
    (version "0.2.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ruby/net-protocol")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0niivmjx7bc6vsylc1ag17mw6mnvjxw02s7cin1f0422xyr8msq9"))))
    (build-system ruby-build-system)
    (propagated-inputs (list ruby-timeout))
    (synopsis "Abstract interface for Ruby network clients")
    (description "This Ruby library provides an abstract interface for network
clients.")
    (home-page "https://github.com/ruby/net-protocol")
    (license (list license:bsd-2))))

(define-public ruby-email-reply-trimmer
  (package
    (name "ruby-email-reply-trimmer")
    (version "0.1.13")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "email_reply_trimmer" version))
        (sha256
         (base32
          "1jgcxifm48xq5dz9k47q43pqm5bfnf14l62l3bqhmv8f6z8dw4ki"))))
    (build-system ruby-build-system)
    (synopsis "Trim replies from plain text email")
    (description "EmailReplyTrimmer is a Ruby small library to trim replies
from plain text email.")
    (home-page "https://github.com/discourse/email_reply_trimmer")
    (license license:expat)))

(define-public ruby-mathn
  (package
    (name "ruby-mathn")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "mathn" version))
       (sha256
        (base32
         "1wn812llln9jzgybz2d7536q39z3gi99i6fi0j1dapcpzvhgrr0p"))))
    (build-system ruby-build-system)
    (native-inputs
     (list bundler ruby-rake-compiler))
    (synopsis "Extends math operations for increased precision")
    (description
     "This gem makes mathematical operations more precise in Ruby and
integrates other mathematical standard libraries.  Prior to Ruby 2.5,
@code{mathn} was part of the Ruby standard library.")
    (home-page "https://github.com/ruby/mathn")
    (license license:bsd-2)))

(define-public ruby-code-statistics
  (package
    (name "ruby-code-statistics")
    (version "0.2.13")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "code_statistics" version))
       (sha256
        (base32
         "07rdpsbwbmh4vp8nxyh308cj7am2pbrfhv9v5xr2d5gq8hnnsm93"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f)) ; Not all test code is included in gem.
    (synopsis "Port of the rails 'rake stats' method")
    (description
     "This gem is a port of the rails 'rake stats' method so it can be made
more robust and work for non rails projects.")
    (home-page "https://github.com/danmayer/code_statistics")
    (license license:expat)))

(define-public ruby-ruby2-keywords
  (package
    (name "ruby-ruby2-keywords")
    (version "0.0.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ruby/ruby2_keywords")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1jhqb152zfr2yrxj6j8rzakkwdkg5viggwnnqrrfxwwy63msdi97"))))
    (build-system ruby-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'delete-extraneous-rake-files
            (lambda _
              (for-each delete-file '("rakelib/changelogs.rake"
                                      "rakelib/epoch.rake"
                                      "rakelib/version.rake"))))
          (add-after 'extract-gemspec 'adjust-files
            (lambda _
              (substitute* "ruby2_keywords.gemspec"
                ;; This file is not present in the git checkout.
                ((".*\"ChangeLog\",.*") "")))))))
    (synopsis "Shim library for Module#ruby2_keywords")
    (description "Provides empty @code{Module#ruby2_keywords} method, for the
forward source-level compatibility against @command{ruby2.7} and
@command{ruby3}.")
    (home-page "https://github.com/ruby/ruby2_keywords")
    (license license:bsd-2)))

(define-public ruby-rubypants
  (package
    (name "ruby-rubypants")
    (version "0.6.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "rubypants" version))
              (sha256
               (base32
                "0xpqkslan2wkyal2h9qhplkr5d4sdn7q6csigrhnljjpp8j4qfsh"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ; need Codecov
    (synopsis "Port of the smart-quotes library SmartyPants")
    (description
     "RubyPants is a Ruby port of the smart-quotes library SmartyPants.  The
original SmartyPants is a web publishing plug-in for Movable Type, Blosxom,
and BBEdit that easily translates plain ASCII punctuation characters into
smart typographic punctuation HTML entities.")
    (home-page "https://github.com/jmcnevin/rubypants")
    (license license:bsd-2)))

(define-public ruby-org-ruby
  (package
    (name "ruby-org-ruby")
    (version "0.9.12")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "org-ruby" version))
              (sha256
               (base32
                "0x69s7aysfiwlcpd9hkvksfyld34d8kxr62adb59vjvh8hxfrjwk"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ; no rakefile
    (propagated-inputs
     (list ruby-rubypants))
    (synopsis "Org-mode parser written in Ruby")
    (description
     "Org-ruby is an org-mode parser written in Ruby.  The most significant
thing this library does today is convert org-mode files to HTML or Textile or
Markdown.")
    (home-page "https://github.com/wallyqs/org-ruby")
    (license license:expat)))

(define-public ruby-rake
  (package
    (name "ruby-rake")
    (version "13.0.6")
    (source
     (origin
       (method git-fetch)               ;for tests
       (uri (git-reference
             (url "https://github.com/ruby/rake")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0c77xyl677s966f8yvk8yv7l31c1ffa1yl4kcwiram219h0s76in"))))
    (build-system ruby-build-system)
    (native-inputs
     (list bundler))
    (synopsis "Rake is a Make-like program implemented in Ruby")
    (description
     "Rake is a Make-like program where tasks and dependencies are specified
in standard Ruby syntax.")
    (home-page "https://github.com/ruby/rake")
    (license license:expat)))

(define-public ruby-rake-manifest
  (package
    (name "ruby-rake-manifest")
    (version "0.2.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/mvz/rake-manifest")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "16k2yvg773c25kp2vhzhp01rhf53k0nhrcmpv34k1fridw90r2k8"))))
    (build-system ruby-build-system)
    (arguments
     (list
      #:test-target "default"
      #:phases
      #~(modify-phases %standard-phases
          (delete 'check)               ;moved after the install phase
          (add-after 'install 'check
            (assoc-ref %standard-phases 'check))
          (add-before 'check 'set-GEM_PATH
            (lambda _
              (setenv "GEM_PATH" (string-append
                                  (getenv "GEM_PATH") ":"
                                  #$output "/lib/ruby/vendor_ruby")))))))
    (native-inputs (list ruby-rspec ruby-simplecov))
    (synopsis "Rake tasks to generate and check a manifest file")
    (description "This package provides Rake tasks to generate and check a
manifest file.")
    (home-page "https://github.com/mvz/rake-manifest")
    (license license:expat)))

(define-public ruby-sfl
  (package
    (name "ruby-sfl")
    (version "2.3")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "sfl" version))
              (sha256
               (base32
                "1qm4hvhq9pszi9zs1cl9qgwx1n4wxq0af0hq9sbf6qihqd8rwwwr"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f  ;; some tests fail, gem is a dummy for ruby >= 1.9 anyway
       #:test-target "spec"))
    (synopsis "Spawn for Ruby 1.8")
    (description "This pure ruby library provides @code{spawn()} which is
almost perfectly compatible with ruby 1.9's.")
    (home-page "https://github.com/ujihisa/spawn-for-legacy")
    (license license:bsd-2)))

(define-public ruby-childprocess
  (package
    (name "ruby-childprocess")
    (version "4.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "childprocess" version))
       (sha256
        (base32
         "1lvcp8bsd35g57f7wz4jigcw2sryzzwrpcgjwwf3chmjrjcww5in"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f  ;; one failing test, even with fixes below
       #:test-target "spec"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch
           (lambda _
             (substitute* "spec/spec_helper.rb"
               (("#!/bin/sh\\\\n") (string-append "#!" (which "sh") "\\n"))))))))
    (native-inputs
     (list ruby-coveralls ruby-rspec))
    (synopsis "Control external programs running in the background, in Ruby")
    (description "@code{childprocess} provides a gem to control external
programs running in the background, in Ruby.")
    (home-page "https://github.com/enkessler/childprocess")
    (license license:expat)))

(define-public ruby-public-suffix
  (package
    (name "ruby-public-suffix")
    (version "4.0.5")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "public_suffix" version))
              (sha256
               (base32
                "0vywld400fzi17cszwrchrzcqys4qm6sshbv73wy5mwcixmrgg7g"))))
    (build-system ruby-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         ;; Remove the requirement on Rubocop, as it isn't useful to run, and
         ;; including it as an input can lead to circular dependencies.
         (add-after 'unpack 'remove-rubocop-from-Rakefile
           (lambda _
             (substitute* "Rakefile"
               (("require \"rubocop/rake\\_task\"") "")
               (("RuboCop::RakeTask\\.new") ""))
             #t)))))
    (native-inputs
     (list bundler ruby-yard/minimal ruby-mocha ruby-minitest-reporters))
    (home-page "https://simonecarletti.com/code/publicsuffix-ruby/")
    (synopsis "Domain name parser")
    (description "The gem @code{public_suffix} is a domain name parser,
written in Ruby, and based on the @dfn{Public Suffix List}.  A public suffix
is one under which Internet users can (or historically could) directly
register names.  Some examples of public suffixes are @code{.com},
@code{.co.uk} and @code{pvt.k12.ma.us}.  The Public Suffix List is a list of
all known public suffixes.")
    (license license:expat)))

(define-public ruby-addressable
  (package
    (name "ruby-addressable")
    (version "2.8.1")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "addressable" version))
              (sha256
               (base32
                "1ypdmpdn20hxp5vwxz3zc04r5xcwqc25qszdlg41h8ghdqbllwmw"))))
    (build-system ruby-build-system)
    (arguments
     '(#:test-target "spec"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'disable-bundler
           (lambda _
             (substitute* "spec/spec_helper.rb"
               (("require 'bundler/setup'") ""))))
         (add-after 'unpack 'remove-unnecessary-dependencies-from-Gemfile
           (lambda _
             (substitute* "Gemfile"
               (("git: 'https://github.com/sporkmonger/rack-mount.git',") "")
               ((".*launchy.*") "")
               ((".*rake.*") "gem 'rake'\n")
               ((".*redcarpet.*") ""))))
         (add-before 'check 'delete-network-dependent-test
           (lambda _
             (delete-file "spec/addressable/net_http_compat_spec.rb"))))))
    (native-inputs
     (list bundler
           ruby-idn-ruby
           ruby-rspec
           ruby-rspec-its-minimal
           ruby-simplecov
           ruby-sporkmonger-rack-mount
           ruby-yard/minimal))
    (propagated-inputs
     (list ruby-public-suffix))
    (home-page "https://github.com/sporkmonger/addressable")
    (synopsis "Alternative URI implementation")
    (description "Addressable is a replacement for the URI implementation that
is part of Ruby's standard library.  It more closely conforms to RFC 3986,
RFC 3987, and RFC 6570 (level 4), providing support for IRIs and URI templates.")
    (license license:asl2.0)))

(define-public ruby-colorize
  (package
    (name "ruby-colorize")
    (version "0.8.1")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "colorize" version))
              (sha256
               (base32
                "133rqj85n400qk6g3dhf2bmfws34mak1wqihvh3bgy9jhajw580b"))))
    (build-system ruby-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (add-before 'check 'remove-codeclimate-dependency
                    (lambda _
                      (substitute* "test/test_colorize.rb"
                        ;; Do not hook the tests into the online CodeClimate
                        ;; service which is unnecessary for these tests.
                        (("require 'codeclimate-test-reporter'")
                         "")
                        (("CodeClimate.*") ""))
                      #t)))))
    (synopsis "Add color effects to the @code{String} class")
    (description
     "This package extends the @code{String} class and adds a
@code{ColorizedString} with methods to set text color, background color,
and text effects.")
    (home-page "https://github.com/fazibear/colorize")
    (license license:gpl2+)))

(define-public ruby-colorator
  (package
    (name "ruby-colorator")
    (version "1.1.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "colorator" version))
              (sha256
               (base32
                "0f7wvpam948cglrciyqd798gdc6z3cfijciavd0dfixgaypmvy72"))))
    (build-system ruby-build-system)
    (arguments
     ;; No test target
     `(#:tests? #f))
    (home-page "http://octopress.org/colorator/")
    (synopsis "Terminal color library")
    (description "Colorator is a Ruby gem that helps you colorize your text
for the terminal.")
    (license license:expat)))

(define-public ruby-command-line-reporter
  (package
    (name "ruby-command-line-reporter")
    (version "4.0.1")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "command_line_reporter" version))
              (sha256
               (base32
                "1l0zxkh5n9dxfw46lpkg416ljpldlq1bgdhqh0d118dk338nz4ll"))))
    (build-system ruby-build-system)
    (arguments
     ;; No Rakefile
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'fix-dependencies
           (lambda _
             (substitute* ".gemspec"
               ;; colored is unmaintained
               (("colored") "colorator")
               ;; colorator version
               (("= 1.2") "= 1.1"))
             #t)))))
    (propagated-inputs (list ruby-colorator))
    (home-page "https://github.com/wbailey/command_line_reporter")
    (synopsis "Report production while executing Ruby scripts")
    (description "This gem provides a DSL that makes it easy to write reports
of various types in ruby.  It eliminates the need to litter your source with
puts statements, instead providing a more readable, expressive interface to
your application.")
    (license license:asl2.0)))

(define-public ruby-command-line-reporter-3
  (package
    (inherit ruby-command-line-reporter)
    (version "3.3.6")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "command_line_reporter" version))
              (sha256
               (base32
                "1h39zqqxp3k4qk49ajpx0jps1vmvxgkh43mqkb6znk583bl0fv71"))))))

(define-public ruby-kpeg
  (package
  (name "ruby-kpeg")
  (version "1.3.3")
  (source
    (origin
      (method url-fetch)
      (uri (rubygems-uri "kpeg" version))
      (sha256
        (base32
          "0jxddpyb23digcd8b1b02gn94057a7mw17680c3c8s3bcb5xqfnp"))))
  (build-system ruby-build-system)
  (native-inputs
   (list ruby-hoe))
  (synopsis "PEG library for Ruby")
  (description "KPeg is a simple PEG library for Ruby.  It provides an API as
well as native grammar to build the grammar.  KPeg supports direct left
recursion of rules via the
@uref{http://www.vpri.org/pdf/tr2008003_experimenting.pdf,OMeta memoization}
technique.")
  (home-page "https://github.com/evanphx/kpeg")
  (license license:expat)))

(define-public ruby-rdoc
  (package
    (name "ruby-rdoc")
    (version "6.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ruby/rdoc")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0dy997zi7k17c2yjlq1y7zl9yaiym1f4jgfh84qqzhwl1qm6v41j"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'generate
           ;; 'gem build' doesn't honor Rakefile dependencies (see:
           ;; https://github.com/ruby/rdoc/issues/432#issuecomment-650808977).
           (lambda _
             (invoke "rake" "generate"))))))
    (native-inputs (list bundler ruby-kpeg ruby-racc ruby-rubocop
                         ruby-test-unit-ruby-core))
    (home-page "https://ruby.github.io/rdoc/")
    (synopsis "HTML and command-line documentation utility")
    (description "RDoc produces HTML and command-line documentation for Ruby
projects.  RDoc includes the +rdoc+ and +ri+ tools for generating and displaying
documentation from the command-line.")
    (license license:gpl2+)))

(define-public ruby-sass-listen
  (package
    (name "ruby-sass-listen")
    (version "4.0.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "sass-listen" version))
              (sha256
               (base32
                "0xw3q46cmahkgyldid5hwyiwacp590zj2vmswlll68ryvmvcp7df"))))
    (build-system ruby-build-system)
    (arguments
     ;; No test target
     `(#:tests? #f))
    (propagated-inputs
     (list ruby-rb-fsevent ruby-rb-inotify))
    (home-page "https://github.com/sass/listen")
    (synopsis "File modification notification library")
    (description "The Listen gem listens to file modifications and notifies you
about the changes.")
    (license license:expat)))

(define-public ruby-terminfo
  (package
    (name "ruby-terminfo")
    (version "0.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
              "http://www.a-k-r.org/" name "/" name "-" version ".tar.gz"))
        (sha256
          (base32
            "1n59dw351z6nzylgj0gpx4rpz6qhf8lrhzcbp1xqfpqvryhaxrjh"))))
    (build-system ruby-build-system)
    (arguments
     (list
      #:test-target "test"
      #:phases
      #~(modify-phases %standard-phases
          (delete 'replace-git-ls-files)
          (replace 'build
            (lambda _
              (invoke "ruby" "extconf.rb")
              (invoke "make")))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (for-each (lambda (f)
                            (invoke "ruby" "-I" "test" f))
                          (find-files "test" "^test_.*\\.rb$")))))
          (replace 'install
            (lambda _
              (invoke "make" "install" (string-append "prefix=" #$output)))))))
    (inputs
     (list ncurses))
    (native-inputs
     (list ruby-rubygems-tasks ruby-rdoc))
    (home-page "http://www.a-k-r.org/ruby-terminfo/")
    (synopsis "Terminfo binding for Ruby")
    (description "Ruby-terminfo provides terminfo binding for Ruby.")
    (license license:bsd-3)))

(define-public ruby-diffy
  (package
    (name "ruby-diffy")
    (version "3.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "diffy" version))
        (sha256
          (base32
            "119imrkn01agwhx5raxhknsi331y5i4yda7r0ws0an6905ximzjg"))))
    (build-system ruby-build-system)
    (arguments
     ;; No tests
     `(#:tests? #f))
    (native-inputs
     (list ruby-rspec))
    (home-page "https://github.com/samg/diffy")
    (synopsis "Convenient diffing in ruby")
    (description "Diffy provides a convenient way to generate a diff from two
strings or files.")
    (license license:expat)))

(define-public ruby-sass-spec
  (package
    (name "ruby-sass-spec")
    (version "3.5.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/sass/sass-spec")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1zsw66830w0xlc7kxz6fm4b5nyb44vdsdgm9mgy06s5aixx83pwr"))))
    (build-system ruby-build-system)
    (propagated-inputs
     (list ruby-command-line-reporter-3
           ruby-diffy))
    (arguments
     (list
      ;; This package contains tests for a sass implementation, and the to
      ;; avoid any circular dependencies, the tests are not run here
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-test
            (lambda _
              (delete-file "spec/values/colors/alpha_hex-3.5/error")
              (substitute* "spec/values/colors/alpha_hex-3.5/expected_output.css"
                (("string") "color")))))))
    (home-page "https://github.com/sass/sass-spec")
    (synopsis "Test suite for Sass")
    (description "Sass Spec is a test suite for Sass.  Test cases are all in
the @file{spec} directory.")
    (license license:expat)))

(define-public ruby-sass
  (package
    (name "ruby-sass")
    (version "3.7.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/sass/ruby-sass")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "03215h9jkni3l9w6lq28p8adaj3qzb47qgxd20l6kldjnm1a1yky"))))
    (build-system ruby-build-system)
    (arguments
     (list #:test-target "test:ruby"))
    (propagated-inputs
     (list ruby-sass-listen))
    (native-inputs
     (list ruby-sass-spec ruby-mathn ruby-cmath))
    (home-page "https://sass-lang.com/")
    (synopsis "CSS extension language")
    (description "Sass is a CSS extension language.  It extends CSS with
features that don't exist yet like variables, nesting, mixins and inheritance.")
    (license license:expat)))

(define-public ruby-sassc
  (package
    (name "ruby-sassc")
    (version "2.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "sassc" version))
       (sha256
        (base32
         "0gpqv48xhl8mb8qqhcifcp0pixn206a7imc07g48armklfqa4q2c"))))
    (build-system ruby-build-system)
    (arguments
     '(#:modules ((guix build ruby-build-system)
                  (guix build utils)
                  (ice-9 textual-ports))
       #:phases
       (modify-phases %standard-phases
         ;; TODO: This would be better as a snippet, but the ruby-build-system
         ;; doesn't seem to support that
         (add-after 'unpack 'remove-libsass
           (lambda _
             (delete-file-recursively "ext")
             (with-atomic-file-replacement "sassc.gemspec"
               (lambda (in out)
                 (let* ((gemspec (get-string-all in))
                        (index (string-contains gemspec "libsass_dir")))
                   (display (string-append
                             (string-take gemspec index)
                             "\nend\n")
                            out))))
             #t))
         (add-after 'unpack 'dont-check-the-libsass-version
           (lambda _
             (substitute* "test/native_test.rb"
               (("assert_equal.*Native\\.version") ""))
             #t))
         (add-after 'unpack 'remove-git-from-gemspec
           (lambda _
             (substitute* "sassc.gemspec"
               (("`git ls-files -z`") "`find . -type f -print0 |sort -z`"))
             #t))
         (add-after 'unpack 'remove-extensions-from-gemspec
           (lambda _
             (substitute* "sassc.gemspec"
               (("\\[\"ext/extconf.rb\"\\]") "[]"))
             #t))
         (add-after 'unpack 'fix-Rakefile
           (lambda _
             (substitute* "Rakefile"
               (("test: 'compile:libsass'") ":test"))
             #t))
         (add-after 'unpack 'remove-unnecessary-dependencies
           (lambda _
             (substitute* "test/test_helper.rb"
               (("require \"pry\"") ""))
             #t))
         (add-before 'build 'patch-native.rb
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "lib/sassc/native.rb"
               ((".*gem_root = spec.gem_dir") "")
               (("ffi_lib .*\n")
                (string-append
                 "ffi_lib '" (assoc-ref inputs "libsass") "/lib/libsass.so'")))
             #t))
         ;; The gemspec still references the libsass files, so just keep the
         ;; one in the gem.
         (delete 'extract-gemspec))))
    (propagated-inputs
     (list ruby-ffi ruby-rake))
    (inputs
     (list libsass))
    (native-inputs
     (list bundler ruby-rake-compiler ruby-minitest-around
           ruby-test-construct))
    (synopsis "Use libsss from Ruby")
    (description
     "This library provides Ruby q@acronym{FFI, Foreign Function Interface}
bindings to the libsass library.  This enables rendering
@acronym{SASS,Syntactically awesome style sheets} from Ruby code.")
    (home-page "https://github.com/sass/sassc-ruby")
    (license license:expat)))

(define-public ruby-jekyll-sass-converter
  (package
    (name "ruby-jekyll-sass-converter")
    (version "2.1.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "jekyll-sass-converter" version))
              (sha256
               (base32
                "04ncr44wrilz26ayqwlg7379yjnkb29mvx4j04i62b7czmdrc9dv"))))
    (build-system ruby-build-system)
    (propagated-inputs
     (list ruby-sass))
    (arguments
     ;; No rakefile
     `(#:tests? #f))
    (home-page "https://github.com/jekyll/jekyll-sass-converter")
    (synopsis "Sass converter for Jekyll")
    (description "This gem provide built-in support for the Sass converter
in Jekyll.")
    (license license:expat)))

(define-public ruby-jekyll-watch
  (package
    (name "ruby-jekyll-watch")
    (version "2.1.2")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "jekyll-watch" version))
              (sha256
               (base32
                "1s9ly83sp8albvgdff12xy2h4xd8lm6z2fah4lzmk2yvp85jzdzv"))))
    (build-system ruby-build-system)
    (propagated-inputs
     (list ruby-listen))
    (arguments
     ;; No rakefile
     `(#:tests? #f))
    (home-page "https://github.com/jekyll/jekyll-watch")
    (synopsis "Jekyll auto-rebuild support")
    (description "This gems add the @code{--watch} switch to the jekyll CLI
interface.  It allows Jekyll to rebuild your site when a file changes.")
    (license license:expat)))

(define-public ruby-parallel
  (package
    (name "ruby-parallel")
    (version "1.21.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/grosser/parallel")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1xqjcvl3gq3shvrqp8wc3fbqibzh4mf1yczq6np9gd79558dwj3w"))))
    (build-system ruby-build-system)
    (arguments
     `(;; TODO 3 test failures
       ;; rspec ./spec/parallel_spec.rb:190 # Parallel.in_processes does not
       ;; open unnecessary pipes
       ;; rspec './spec/parallel_spec.rb[1:9:7]' # Parallel.each works with
       ;; SQLite in processes
       ;; rspec './spec/parallel_spec.rb[1:9:16]' # Parallel.each works with
       ;; SQLite in threads
       #:tests? #f
       #:test-target "rspec-rerun:spec"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-Gemfile
           (lambda _
             (substitute* "Gemfile"
               (("gem 'rspec-legacy_formatters'") "")
               (("gem 'activerecord.*$") "gem 'activerecord'\n"))))
         (add-before 'check 'delete-Gemfile.lock
           (lambda _
             ;; Bundler isn't being used for fetching dependencies, so
             ;; delete the Gemfile.lock
             (delete-file "Gemfile.lock")
             #t))
         (add-before 'build 'patch-gemspec
           (lambda _
             (substitute* "parallel.gemspec"
               (("git ls-files") "find"))
             #t)))))
    (native-inputs
     (list ruby-rspec
           ruby-rspec-rerun
           bundler
           ruby-activerecord
           ruby-progressbar
           ruby-bump
           procps
           lsof
           ruby-mysql2
           ruby-sqlite3
           ruby-i18n))
    (home-page "https://github.com/grosser/parallel")
    (synopsis "Parallel processing in Ruby")
    (description "Parallel allows you to run any code in parallel Processes
(to use all CPUs) or Threads(to speedup blocking operations).  It is best
suited for map-reduce or e.g. parallel downloads/uploads.")
    (license license:expat)))

(define-public ruby-cabin
  (package
    (name "ruby-cabin")
    (version "0.9.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "cabin" version))
              (sha256
               (base32
                "0b3b8j3iqnagjfn1261b9ncaac9g44zrx1kcg81yg4z9i513kici"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))      ;no Rakefile in released gem
    (synopsis "Structured and contextual logging experiments in Ruby")
    (description "This Ruby library provides an experimental logging system
that tries to make logging more flexible and more consumable than plain-text
logging.")
    (home-page "https://github.com/jordansissel/ruby-cabin")
    (license license:asl2.0)))

(define-public ruby-capybara
  (package
    (name "ruby-capybara")
    (version "3.38.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "capybara" version))
              (sha256
               (base32
                "123198zk2ak8mziwa5jc3ckgpmsg08zn064n3aywnqm9s1bwjv3v"))))
    (build-system ruby-build-system)
    (arguments
     (list #:tests? #f                ;sinatra is currently broken with rack 3
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'extract-gemspec 'remove-extraneous-requirements
                 (lambda _
                   (substitute* "spec/spec_helper.rb"
                     ((".*require 'selenium_statistics'.*") "")
                     ((".*SeleniumStatistics.print_results.*") ""))))
               (replace 'check
                 (lambda* (#:key tests? #:allow-other-keys)
                   (when tests?
                     (invoke "rspec" "spec")))))))
    (native-inputs
     (list ruby-puma
           ruby-rspec
           ruby-selenium-webdriver
           ruby-sinatra))
    (propagated-inputs
     (list ruby-addressable
           ruby-launchy
           ruby-matrix
           ruby-mini-mime
           ruby-nokogiri
           ruby-rack
           ruby-rack-test
           ruby-regexp-parser
           ruby-xpath))
    (synopsis "Integration testing tool for rack-based web applications")
    (description "Capybara is an integration testing tool for rack based web
applications.  It simulates how a user would interact with a website.")
    (home-page "https://github.com/teamcapybara/capybara")
    (license license:expat)))

(define-public ruby-cane
  (package
    (name "ruby-cane")
    (version "3.0.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "cane" version))
              (sha256
               (base32
                "0yf5za3l7lhrqa3g56sah73wh33lbxy5y3cb7ij0a2bp1b4kwhih"))))
    (build-system ruby-build-system)
    (arguments `(#:tests? #f)); No rakefile
    (home-page "https://github.com/square/cane")
    (propagated-inputs
     (list ruby-parallel))
    (synopsis "Code quality threshold checking")
    (description "Cane fails your build if code quality thresholds are not met.")
    (license license:asl2.0)))

(define-public ruby-morecane
  (package
    (name "ruby-morecane")
    (version "0.2.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "morecane" version))
              (sha256
               (base32
                "0w70vb8z5bdhvr21h660aa43m5948pv0bd27z7ngai2iwdvqd771"))))
    (build-system ruby-build-system)
    (home-page "https://github.com/yob/morecane")
    (arguments `(#:tests? #f)); No rakefile
    (propagated-inputs
     (list ruby-parallel))
    (synopsis "Extra checks for cane")
    (description "The cane gem provides a great framework for running quality
checks over your ruby project as part of continuous integration build.  It
comes with a few checks out of the box, but also provides an API for loading
custom checks.  This gem provides a set of additional checks.")
    (license license:expat)))

(define-public ruby-pdf-reader
  (package
    (name "ruby-pdf-reader")
    (version "2.4.0")
    (source (origin
              (method git-fetch)        ;no test in distributed gem archive
              (uri (git-reference
                    (url "https://github.com/yob/pdf-reader")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1yh8yrlssf5ppnkvk4m78vmh5r5vqwdcd0gm3lqipw162llz0rai"))))
    (build-system ruby-build-system)
    (arguments `(#:test-target "spec"
                 #:phases (modify-phases %standard-phases
                            (add-after 'unpack 'do-not-use-bundler
                              (lambda _
                                (substitute* "spec/spec_helper.rb"
                                  ((".*[Bb]undler.*") ""))
                                #t)))))
    (native-inputs
     (list ruby-rspec ruby-cane ruby-morecane))
    (propagated-inputs
     (list ruby-afm ruby-ascii85 ruby-hashery ruby-rc4 ruby-ttfunk))
    (home-page "https://github.com/yob/pdf-reader")
    (synopsis "PDF parser in Ruby")
    (description "The PDF::Reader library implements a PDF parser conforming as
much as possible to the PDF specification from Adobe.  It provides programmatic
access to the contents of a PDF file with a high degree of flexibility.")
    (license license:gpl3+)))

(define-public ruby-pdf-inspector
  (let ((revision "1")
        (commit "00ee4c92ff917118785ebec188e81effc968abeb"))
    (package
      (name "ruby-pdf-inspector")
      (version (git-version "1.3.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/prawnpdf/pdf-inspector")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0h9w81ddd0gvkh5n2cvny9ddb5qiac1si0dhinkk0xxh5382qs0m"))))
      (build-system ruby-build-system)
      (arguments
       `(#:test-target "spec"
         #:phases (modify-phases %standard-phases
                    (add-before 'build 'drop-signing-key-requirement
                      (lambda _
                        (substitute* "pdf-inspector.gemspec"
                          (("spec.signing_key =.*")
                           "spec.signing_key = nil"))
                        #t))
                    (replace 'check
                      (lambda _
                        (substitute* "pdf-inspector.gemspec"
                          ((".*rubocop.*") "")
                          ((".*yard.*") ""))
                        (invoke "rspec"))))))
      (native-inputs
       (list ruby-rspec))
      (propagated-inputs
       (list ruby-pdf-reader))
      (home-page "https://github.com/prawnpdf/pdf-inspector")
      (synopsis "Analysis classes for inspecting PDF output")
      (description "This library provides a number of PDF::Reader based tools for
use in testing PDF output.  Presently, the primary purpose of this tool is to
support the tests found in Prawn, a pure Ruby PDF generation library.")
      (license %prawn-project-licenses))))

(define-public ruby-pdf-core
  (package
    (name "ruby-pdf-core")
    (version "0.9.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "pdf-core" version))
              (sha256
               (base32
                "1fz0yj4zrlii2j08kaw11j769s373ayz8jrdhxwwjzmm28pqndjg"))))
    (build-system ruby-build-system)
    (arguments
     ; No test target
     `(#:tests? #f))
    (home-page "https://github.com/prawnpdf/pdf-core")
    (synopsis "Low level PDF features for Prawn")
    (description "This is an experimental gem that extracts low-level PDF
functionality from Prawn.")
    (license license:gpl3+)))

(define-public ruby-prawn-dev
  (package
    (name "ruby-prawn-dev")
    (version "0.3.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "prawn-dev" version))
              (sha256
               (base32
                "1hbzzgm0nwc6h8pyv8h9xx068bf676rispxcz4a0sm8nykz54z4x"))))
    (build-system ruby-build-system)
    (arguments
     (list
      #:tests? #f                       ;no test suite
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'extract-gemspec 'drop-rubocop-dependency
            ;; Rubocop depends on Prawn.  Remove it to avoid the
            ;; dependency cycle when using this tool to build
            ;; Prawn components.
            (lambda _
              (substitute* "lib/prawn/dev/tasks.rb"
                (("require 'rubocop/rake_task'")
                 "")
                (("RuboCop::RakeTask.new")
                 ""))
              (substitute* ".gemspec"
                ((".*add.*dependency.*(rubocop|simplecov).*")
                 "")))))))
    (propagated-inputs (list ruby-rake ruby-rspec ruby-yard))
    (synopsis "Shared tools for Prawn projects development")
    (description "Prawn-dev contains tools to aid the development of the
various Prawn projects.")
    (home-page "https://prawnpdf.org/")
    (license license:expat)))

(define-public ruby-prawn
  ;; There hasn't been a new release since 2017/03/17.
  (package
    (name "ruby-prawn")
    (version "2.4.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/prawnpdf/prawn")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1h1gww12wcdscij0lnd21p9zcbwrwc3miini5ppannc2birmj9ja"))))
    (build-system ruby-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'drop-signing-key-requirement
            (lambda _
              (substitute* "prawn.gemspec"
                (("spec.signing_key =.*")
                 "spec.signing_key = nil"))))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                ;; The Prawn manual test fails (see:
                ;; https://github.com/prawnpdf/prawn/issues/1163), so exclude
                ;; it.
                (invoke "rspec"
                        "--exclude-pattern" "prawn_manual_spec.rb")))))))
    (propagated-inputs
     (list ruby-matrix
           ruby-pdf-core
           ruby-ttfunk))
    (native-inputs
     (list ruby-pdf-inspector
           ruby-prawn-manual-builder
           ruby-prawn-dev))
    (home-page "https://prawnpdf.org/api-docs/2.0/")
    (synopsis "PDF generation for Ruby")
    (description "Prawn is a pure Ruby PDF generation library.")
    (license %prawn-project-licenses)))

(define-public ruby-prawn-table
  (package
    (name "ruby-prawn-table")
    (version "0.2.2")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "prawn-table" version))
              (sha256
               (base32
                "1nxd6qmxqwl850icp18wjh5k0s3amxcajdrkjyzpfgq0kvilcv9k"))))
    (build-system ruby-build-system)
    (propagated-inputs
     (list ruby-prawn ruby-pdf-inspector))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-yard" ,ruby-yard)
       ("ruby-mocha" ,ruby-mocha)
       ("ruby-coderay" ,ruby-coderay)
       ("ruby-prawn-manual-builder" ,ruby-prawn-manual-builder)
       ("ruby-simplecov" ,ruby-simplecov)
       ("ruby-rspec-2" ,ruby-rspec-2)))
    (arguments
     '(;; TODO: 1 test fails
       ;; Failure/Error: pdf.page_count.should == 1
       ;;   expected: 1
       ;;        got: 2 (using ==)
       ;; # ./spec/table_spec.rb:1308
       ;;
       ;; 225 examples, 1 failure
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'patch-gemspec
           (lambda _
             (substitute* "prawn-table.gemspec"
               ;; Loosen the requirement for pdf-inspector
               (("~> 1\\.1\\.0") ">= 0")
               ;; Loosen the requirement for pdf-reader
               (("~> 1\\.2") ">= 0"))))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "rspec"))
             #t)))))
    (home-page "https://github.com/prawnpdf/prawn-table")
    (synopsis "Tables support for Prawn")
    (description "This gem provides tables support for Prawn.")
    (license license:gpl3+)))

(define-public ruby-kramdown
  (package
    (name "ruby-kramdown")
    (version "2.3.1")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "kramdown" version))
              (sha256
               (base32
                "0jdbcjv4v7sj888bv3vc6d1dg4ackkh7ywlmn9ln2g9alk7kisar"))))
    (build-system ruby-build-system)
    (arguments `(#:tests? #f)); FIXME: some test failures
    (native-inputs
     (list ruby-prawn ruby-prawn-table))
    (home-page "https://kramdown.gettalong.org/")
    (synopsis "Markdown parsing and converting library")
    (description "Kramdown is a library for parsing and converting a superset
of Markdown.  It is completely written in Ruby, supports standard Markdown
(with some minor modifications) and various extensions that have been made
popular by the PHP @code{Markdown Extra} package and @code{Maruku}.")
    (license license:expat)))

(define-public ruby-kramdown-parser-gfm
  (package
    (name "ruby-kramdown-parser-gfm")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "kramdown-parser-gfm" version))
       (sha256
        (base32 "0a8pb3v951f4x7h968rqfsa19c8arz21zw1vaj42jza22rap8fgv"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))                    ;no rakefile
    (propagated-inputs
     (list ruby-kramdown))
    (synopsis "Kramdown parser for the GFM dialect of Markdown")
    (description
     "This is a parser for kramdown that converts Markdown documents in the
GFM dialect to HTML.")
    (home-page "https://github.com/kramdown/parser-gfm")
    (license license:expat)))

(define-public ruby-http-parser.rb
  (package
    (name "ruby-http-parser.rb")
    (version "0.6.0")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "http_parser.rb" version))
        (sha256
          (base32
            "15nidriy0v5yqfjsgsra51wmknxci2n2grliz78sf9pga3n0l7gi"))))
    (build-system ruby-build-system)
    (arguments
     ;; No tests
     `(#:tests? #f))
    (native-inputs
     (list ruby-rake-compiler ruby-rspec))
    (home-page "https://github.com/tmm1/http_parser.rb")
    (synopsis "HTTP parser un Ruby")
    (description "This gem is a simple callback-based HTTP request/response
parser for writing http servers, clients and proxies.")
    (license license:expat)))

(define-public ruby-excon
  (package
    (name "ruby-excon")
    (version "0.109.0")
    (source (origin
              (method git-fetch)        ;for tests
              (uri (git-reference
                    (url "https://github.com/excon/excon")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "199niqbpzj70k3n6ybg4vbcw3qm76kwic4nl9747l1n0v49aaj24"))))
    (build-system ruby-build-system)
    (arguments
     (list
      #:tests? #f  ;; some tests require DNS
      #:phases
      #~(modify-phases %standard-phases
          (replace 'replace-git-ls-files
            (lambda _
              (substitute* "excon.gemspec"
                (("`git ls-files -- data/. lib/.`")
                 "`find data lib -type f`"))))
          (add-before 'check 'disable-server-spec-checks
            (lambda _ ;; TODO: Remove this if ruby-unicorn is available.
              ;; Some of the tests in this file require ruby-unicorn, which is
              ;; not yet packaged for guix and would pull in a lot of other
              ;; dependencies.
              (delete-file "spec/excon/test/server_spec.rb"))))))
    (native-inputs
     (list
      ruby-activesupport
      ruby-eventmachine
      ruby-json
      ruby-open4
      ruby-puma
      ruby-rspec
      ruby-shindo
      ruby-sinatra
      ruby-webrick))
    (synopsis "Usable, fast, simple Ruby HTTP 1.1")
    (description "Excon was designed to be simple, fast and performant.  It
works great as a general HTTP(s) client and is particularly well suited to
usage in API clients.")
    (home-page "https://github.com/excon/excon")
    (license license:expat)))

(define-public ruby-em-websocket
  (package
    (name "ruby-em-websocket")
    (version "0.5.1")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "em-websocket" version))
        (sha256
          (base32
            "1bsw8vjz0z267j40nhbmrvfz7dvacq4p0pagvyp17jif6mj6v7n3"))))
    (build-system ruby-build-system)
    (arguments
     ;; No tests
     `(#:tests? #f))
    (propagated-inputs
      (list ruby-eventmachine ruby-http-parser.rb))
    (native-inputs
     (list bundler ruby-rspec))
    (home-page "https://github.com/igrigorik/em-websocket")
    (synopsis "EventMachine based WebSocket server")
    (description "Em-websocket is an EventMachine based WebSocket server
implementation.")
    (license license:expat)))

(define-public ruby-rouge
  (package
    (name "ruby-rouge")
    (version "3.26.1")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "rouge" version))
              (sha256
               (base32
                "197k0vskf72wxx0gzwld2jzg27bb7982xlvnzy9adlvkzp7nh8vf"))))
    (build-system ruby-build-system)
    (arguments `(#:tests? #f)); No rakefile
    (home-page "http://rouge.jneen.net/")
    (synopsis "Code highlighter")
    (description "Rouge is a code highlighter written in Ruby.  It supports more
than 100 languages and outputs HTML or ANSI 256-color text.  Its HTML output
is compatible with stylesheets designed for pygments.")
    (license (list
               ;; rouge is licensed under expat
               license:expat
               ;; pygments is licensed under bsd-2
               license:bsd-2))))

(define-public ruby-hashie
  (package
    (name "ruby-hashie")
    (version "5.0.0")
    (source (origin
              (method git-fetch)        ;for tests
              (uri (git-reference
                    (url "https://github.com/hashie/hashie")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0ihami0cdn71cvwzwgr3vxqvqi0ifqsna0vlyqiqlhsnf93w0cm8"))))
    (build-system ruby-build-system)
    (arguments
     (list #:test-target "spec"
           #:phases #~(modify-phases %standard-phases
                        (add-after 'unpack 'disable-bundler
                          (lambda _
                            (substitute* "Rakefile"
                              ((".*require 'bundler'.*") "")
                              ((".*Bundler.setup.*") "")
                              (("Bundler::GemHelper\\.install_tasks") ""))))
                        (add-after 'unpack 'disable-rubocop
                          (lambda _
                            (substitute* "Rakefile"
                              (("require 'rubocop/rake_task'") "")
                              (("RuboCop::RakeTask.new") ""))))
                        (add-after 'unpack 'relax-requirements
                          (lambda _
                            ;; Contains multiple extraneous dependencies.
                            (delete-file "Gemfile"))))))
    (native-inputs (list ruby-json ruby-pry ruby-rspec ruby-rspec-pending-for))
    (home-page "https://github.com/hashie/hashie")
    (synopsis "Extensions to Ruby Hashes")
    (description "Hashie is a collection of classes and mixins that make Ruby
hashes more powerful.")
    (license license:expat)))

(define-public ruby-heredoc-unindent
  (package
    (name "ruby-heredoc-unindent")
    (version "1.2.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "heredoc_unindent" version))
              (sha256
               (base32
                "14ijr2fsjwhrkjkcaz81d5xnfa4vvgvcflrff83avqw9klm011yw"))))
    (build-system ruby-build-system)
    (native-inputs
     (list ruby-hoe-3))
    (home-page "https://github.com/adrianomitre/heredoc_unindent")
    (synopsis "Heredoc indentation cleaner")
    (description "This gem removes common margin from indented strings, such
as the ones produced by indented heredocs.  In other words, it strips out
leading whitespace chars at the beginning of each line, but only as much as
the line with the smallest margin.

It is acknowledged that many strings defined by heredocs are just code and
fact is that most parsers are insensitive to indentation.  If, however, the
strings are to be used otherwise, be it for printing or testing, the extra
indentation will probably be an issue and hence this gem.")
    (license license:expat)))

(define-public ruby-safe-yaml
  (package
    (name "ruby-safe-yaml")
    (version "1.0.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dtao/safe_yaml")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1a0wh7y3va2m7bjza95na2snw0vrdh9syz40mpjvjphbc4ph3pzg"))))
    (build-system ruby-build-system)
    (native-inputs
     (list ruby-rspec ruby-hashie ruby-heredoc-unindent))
    (arguments
     (list
      #:ruby ruby-2.7
      #:test-target "spec"
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'set-TZ
            (lambda _
              ;; This test is dependent on the timezone
              ;; spec/transform/to_date_spec.rb:35
              ;; # SafeYAML::Transform::ToDate converts times to the local
              ;; timezone
              (setenv "TZ" "UTC-11"))))))
    (home-page "https://github.com/dtao/safe_yaml")
    (synopsis "YAML parser")
    (description "The SafeYAML gem provides an alternative implementation of
YAML.load suitable for accepting user input in Ruby applications.")
    (license license:expat)))

(define-public ruby-yaml-lint
  (package
    (name "ruby-yaml-lint")
    (version "0.0.10")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/Pryz/yaml-lint")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1jz26mxjkdyjbgqp7f9isnzd1i6vkizsswyj1v639nmq31hwfh0d"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "rspec"))
             #t)))))
    (native-inputs
     (list ruby-coveralls ruby-rspec ruby-simplecov))
    (synopsis "Simple YAML check tool")
    (description
     "@code{yaml-lint} will simply try to load the YAML file with the built-in
Ruby yaml library.")
    (home-page "https://github.com/Pryz/yaml-lint")
    (license license:expat)))

(define-public ruby-matrix
  (package
    (name "ruby-matrix")
    (version "0.4.2")
    (source (origin
              (method git-fetch)        ;for tests
              (uri (git-reference
                    (url "https://github.com/ruby/matrix")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1asxr0jzh39lk5f8a9wm5avykrcy0v2wgd1bl3cszjczh99xy5k2"))))
    (build-system ruby-build-system)
    (synopsis "@code{Matrix} and @code{Vector} classes implementation for Ruby")
    (description "This Ruby library provides an implementation of the
@code{Matrix} and @code{Vector} classes.  The @code{Matrix} class represents a
mathematical matrix.  It provides methods for creating matrices, operating on
them arithmetically and algebraically, and determining their mathematical
properties (trace, rank, inverse, determinant, eigensystem, etc.).  The
@code{Vector} class represents a mathematical vector, which is useful in its
own right, and also constitutes a row or column of a @code{Matrix}.")
    (home-page "https://github.com/ruby/matrix")
    (license license:bsd-2)))

(define-public ruby-m
  (package
    (name "ruby-m")
    (version "1.6.1")
    (source (origin
              (method git-fetch)        ;for tests
              (uri (git-reference
                    (url "https://github.com/qrush/m")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1aycfc8l1bsln1y300fv75fknn4amjcvc4rm2kd8hb6cqivjq5rg"))))
    (build-system ruby-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'sanitize-dependencies
            (lambda _
              (delete-file "Gemfile")
              (delete-file "Gemfile.lock")
              ;; Rocco is unmaintained as of 2023/01/08; avoid depending on
              ;; it.
              (substitute* "m.gemspec"
                ;; The rdiscount and rocco dependencies are used for
                ;; generating the documentation.
                ((".*rdiscount.*") "")
                ((".*rocco.*") "")
                ((".*appraisal.*") "")
                ((".*coveralls.*") ""))
              (substitute* "Rakefile"
                ;; ruby-appraisal is not packaged, and is used to test against
                ;; various dependencies; circumvent its use.
                ((".*require 'appraisal'.*") "")
                ((".*require 'coveralls'.*") "")
                (("appraisal [:graphic:]+ rake")
                 "rake")
                (("Coveralls.push!") ""))))
          (add-before 'replace-git-ls-files 'pre-replace-git-ls-files
            (lambda _
              (substitute* "m.gemspec"
                (("git ls-files -- bin/\\*")
                 "find bin -type f -not -regex '.*\\.gem$' | sort")
                (("git ls-files -- \\{test,spec,features}/\\*")
                 "find test -type f -not -regex '.*\\.gem$' | sort"))))
          (delete 'check)
          (add-after 'install 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (setenv "GEM_PATH" (string-append
                                  (getenv "GEM_PATH") ":"
                                  #$output "/lib/ruby/vendor_ruby"))
              (when tests?
                (invoke "rake" "test")))))))
    (native-inputs (list ruby-activesupport))
    (propagated-inputs (list ruby-method-source ruby-rake))
    (synopsis "Ruby test runner that can run tests by line number")
    (description "@code{m} stands for metal, a better test/unit and
@code{minitest} test runner that can run tests by line number.")
    (home-page "https://github.com/qrush/m")
    (license license:expat)))

(define-public ruby-mercenary
  (package
    (name "ruby-mercenary")
    (version "0.4.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "mercenary" version))
              (sha256
               (base32
                "0f2i827w4lmsizrxixsrv2ssa3gk1b7lmqh8brk8ijmdb551wnmj"))))
    (build-system ruby-build-system)
    (arguments `(#:test-target "spec"))
    (native-inputs
     (list bundler))
    (home-page "https://github.com/jekyll/mercenary")
    (synopsis "Command-line apps library in Ruby")
    (description "Mercenary is a lightweight and flexible library for writing
command-line apps in Ruby.")
    (license license:expat)))

(define-public ruby-launchy
  (package
    (name "ruby-launchy")
    (version "2.5.2")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "launchy" version))
              (sha256
               (base32
                "06r43899384das2bkbrpsdxsafyyqa94il7111053idfalb4984a"))))
    (build-system ruby-build-system)
    (native-inputs (list curl links ruby-simplecov))
    (propagated-inputs (list ruby-addressable))
    (synopsis "Ruby helper class for launching applications")
    (description
     "Launchy is helper class for launching applications in a fire and forget
manner.  The aim of Launchy is to provide a common approach to launching
external applications from within Ruby programs.")
    (home-page "https://github.com/copiousfreetime/launchy")
    (license license:isc)))

(define-public ruby-liquid
  (package
    (name "ruby-liquid")
    (version "5.4.0")
    (source (origin
              (method git-fetch)        ;for tests
              (uri (git-reference
                    (url "https://github.com/Shopify/liquid")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1qdnvd1f9zs6wyilcgxyh93wis7ikbpimjxfpbkpk2ngr1m2c8la"))))
    (build-system ruby-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (delete 'check)               ;moved after the install phase
          (add-after 'install 'check
            (assoc-ref %standard-phases 'check))
          (add-before 'check 'set-GEM_PATH
            (lambda _
              (setenv "GEM_PATH" (string-append
                                  (getenv "GEM_PATH") ":"
                                  #$output "/lib/ruby/vendor_ruby"))))
          (add-before 'check 'delete-problematic-tests
            (lambda _
              ;; The following test fails with 'Unknown tag' errors (see:
              ;; https://github.com/Shopify/liquid/issues/1699).
              (delete-file "test/integration/tags/inline_comment_test.rb"))))))
    (native-inputs (list ruby-liquid-c-bootstrap ruby-rspec ruby-stackprof))
    (home-page "https://shopify.github.io/liquid/")
    (synopsis "Template language")
    (description "Liquid is a template language written in Ruby.  It is used
to load dynamic content on storefronts.")
    (license license:expat)))

(define-public ruby-liquid-4
  (package
    (inherit ruby-liquid)
    (name "ruby-liquid")
    (version "4.0.4")
    (source (origin
              (method git-fetch)        ;for tests
              (uri (git-reference
                    (url "https://github.com/Shopify/liquid")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0cr321nd0zkbxirgdfmz37xx7j26zfnicjh585fi20vx60frry83"))))
    (arguments (list #:tests? #f))))    ;avoid required an older ruby-liquid-c

;;; This variant is purposefully incomplete, lacking ruby-liquid so that it
;;; can be used for ruby-liquid's test suite.
(define ruby-liquid-c-bootstrap
  (package
    (name "ruby-liquid-c-bootstrap")
    (version "4.1.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "liquid-c" version))
              (sha256
               (base32
                "0jl37jz9hbfbhknryx4myxqx4n1f5dzyzmf1sapkcbw93xyrmkch"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))
    (native-inputs (list ruby-rake-compiler))
    (synopsis "Liquid performance extension in C")
    (description "This package provides a Partial native implementation of the
liquid ruby gem in C that makes it operate about three times faster.")
    (home-page "https://github.com/shopify/liquid-c")
    (license license:expat)))

(define-public ruby-liquid-c
  (package/inherit ruby-liquid-c-bootstrap
    (name "ruby-liquid-c")
    (arguments
     (list
      ;; Only run the unit tests, because the test:integration target fails
      ;; with "File does not exist: test_helper" (see:
      ;; https://github.com/Shopify/liquid-c/issues/188).
      #:test-target "test:unit"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'extract-gemspec 'relax-requirements
            (lambda _
              (substitute* "Gemfile"
                ;; Do not attempt to fetch a gem from git.
                (("git_source\\(:github) do \\|repo_name\\|")
                 "if false")
                ((", github: \"Shopify/liquid\", ref: \"master\"")
                 "")
                ;; Remove extraneous dependencies.
                ((".*byebug.*") "")
                ((".*rubocop.*") "")
                ;; Relax spy version specification.
                (("gem \"spy\", \"0.4.1\"")
                 "gem \"spy\", \">= 0.4.1\"")))))))
    (native-inputs
     (list ruby-benchmark-ips
           ruby-rake-compiler
           ruby-ruby-memcheck
           ruby-spy
           ruby-stackprof))
    (propagated-inputs
     (list ruby-liquid))))

(define-public ruby-localhost
  (package
    (name "ruby-localhost")
    (version "1.1.10")
    (source (origin
              (method git-fetch)        ;for tests
              (uri (git-reference
                    (url "https://github.com/socketry/localhost")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1yp70w15wpfk613ap5f4y15yx4n2qqwa67vqc2f4lv7npf3llcz0"))))
    (build-system ruby-build-system)
    (arguments
     ;; XXX: The test suite requires sus-fixtures-async, which requires async,
     ;; only available for Ruby 3.0.
     (list #:tests? #f
           #:phases
           #~(modify-phases %standard-phases
               (add-before 'build 'remove-missing-signing-key
                 (lambda _
                   ;; Otherwise, the build fails with ENOENT.
                   (substitute* "localhost.gemspec"
                     ((".*spec.signing_key.*") "")))))))
    (synopsis "API for generating per-user self-signed root certificates")
    (description "This package provides @code{localhost}, a Ruby library for
Managing a local certificate authority for self-signed, localhost development
servers.")
    (home-page "https://github.com/socketry/localhost")
    (license license:expat)))

(define-public ruby-forwardable-extended
  (package
    (name "ruby-forwardable-extended")
    (version "2.6.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "forwardable-extended" version))
              (sha256
               (base32
                "15zcqfxfvsnprwm8agia85x64vjzr2w0xn9vxfnxzgcv8s699v0v"))))
    (build-system ruby-build-system)
    (arguments `(#:tests? #f)); Cyclic dependency on luna-rspec-formatters
    (home-page "https://github.com/envygeeks/forwardable-extended")
    (synopsis "Delegation to hashes and instance variables in Forwardable")
    (description "Forwardable Extended provides more @code{Forwardable}
methods for your source as @code{Forwardable::Extended}.")
    (license license:expat)))

(define-public ruby-pathutil
  (package
    (name "ruby-pathutil")
    (version "0.16.2")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "pathutil" version))
              (sha256
               (base32
                "12fm93ljw9fbxmv2krki5k5wkvr7560qy8p4spvb9jiiaqv78fz4"))))
    (build-system ruby-build-system)
    (propagated-inputs
     (list ruby-forwardable-extended))
    (native-inputs
     (list bundler ruby-rspec))
    ;; Fails with: cannot load such file --
    ;; /tmp/guix-build-ruby-pathutil-0.16.0.drv-0/gem/benchmark/support/task
    (arguments `(#:tests? #f))
    (home-page "https://github.com/envygeeks/pathutil")
    (synopsis "Extended implementation of Pathname")
    (description "Pathutil tries to be a faster pure Ruby implementation of
Pathname.")
    (license license:expat)))

(define-public ruby-terminal-table
  (package
    (name "ruby-terminal-table")
    (version "3.0.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/tj/terminal-table")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1c3f7ng3lxq962n8sbmlsvjx6srh5i801wzsyhxmfz2g880f5jps"))))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'remove-unnecessary-dependencies
           (lambda _
             (substitute* "terminal-table.gemspec"
               (("s.add_runtime_dependency.*") "\n")
               (("s.add_development_dependency.*") "\n"))
             (substitute* "Gemfile"
               ((".*tins.*") "\n"))))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "rspec")))))))
    (build-system ruby-build-system)
    (propagated-inputs
     (list ruby-unicode-display-width))
    (native-inputs
     (list ruby-rspec ruby-term-ansicolor))
    (home-page "https://github.com/tj/terminal-table")
    (synopsis "Simple, feature rich ASCII table generation library")
    (description
     "Terminal Table is a fast and simple, yet feature rich
table generator written in Ruby.  It supports ASCII and
Unicode formatted tables.")
    (license license:expat)))

(define-public jekyll
  (package
    (name "jekyll")
    (version "4.3.2")
    (source (origin
              (method git-fetch)        ;for tests
              (uri (git-reference
                    (url "https://github.com/jekyll/jekyll")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1d588d7zhp526r21f9mnm204m8qy0c8h3lq3ghyg6qp8mj6hnwj4"))))
    (build-system ruby-build-system)
    (arguments
     (list #:modules '((guix build ruby-build-system)
                       (guix build utils)
                       (ice-9 ftw)
                       (srfi srfi-26))
           ;; The cucumber acceptance suite is not run as it depends on an old
           ;; version (5).
           #:test-target "spec"
           #:phases
           #~(modify-phases %standard-phases
               (add-before 'check 'disable-problematic-tests
                 ;; TODO: Package the missing test inputs.
                 (lambda _
                   (with-directory-excursion "test"
                     ;; Requires 'jekyll-coffeescript'.
                     (delete-file "test_coffeescript.rb")
                     ;; Requires 'tomlrb'.
                     (delete-file "test_configuration.rb")
                     (substitute* "test_filters.rb"
                       ;; The sassify tests fail due to white space
                       ;; differences (see:
                       ;; https://github.com/jekyll/jekyll/issues/9322).
                       ((".*s?ssify with simple string.*" all)
                        (string-append all
                                       "      skip('fails on guix')\n")))
                     ;; Requires kramdown-syntax-coderay.
                     (delete-file "test_kramdown.rb")
                     ;; Requires 'test-theme', usually made available from the
                     ;; local checkout via Bundler (not used here).
                     (delete-file "test_layout_reader.rb")
                     ;; Requires a large amount of un-packaged dependencies.
                     (delete-file "test_plugin_manager.rb")
                     ;; Requires 'classifier-reborn'.
                     (delete-file "test_related_posts.rb")
                     ;; This one causes a test failure similar to the ones for
                     ;; sassify above.
                     (delete-file "test_sass.rb")
                     ;; This would require running the tests via 'bundle
                     ;; exec', but the Gemfile contains too many (extraneous)
                     ;; dependencies.
                     (delete-file "test_site.rb")
                     ;; Delete the theme tests, as they require 'test-theme',
                     ;; usually made available from the local checkout via the
                     ;; Gemfile/bundler (not used here).
                     (for-each delete-file
                               (scandir
                                "." (cut string-prefix? "test_theme" <>)))
                     ;; This one also relies on 'test-theme'.
                     (delete-file "test_liquid_renderer.rb")
                     ;; This test assumes internet connectivity, negate it, as
                     ;; there's no Internet in the build container.
                     (substitute* "test_utils.rb"
                       (("assert Utils::Internet\\.connected\\?")
                        "refute Utils::Internet.connected?"))
                     ;; These tests fail non-deterministically (see:
                     ;; https://github.com/jekyll/jekyll/issues/9323).
                     (delete-file "test_new_command.rb")
                     (delete-file "test_collections.rb"))))
               (replace 'check
                 (lambda* (#:key tests? #:allow-other-keys)
                   (when tests?
                     ;; Invoke the test scripts manually, as 'rake test'
                     ;; doesn't show any failure details, making debugging
                     ;; needlessly difficult.
                     (for-each (lambda (f)
                                 (invoke "ruby" "-I" "test" f))
                               (find-files "test" "^test_.*\\.rb$"))))))))
    (native-inputs
     (list bundler
           ruby-httpclient
           ruby-minitest-profile
           ruby-minitest-reporters
           ruby-nokogiri
           ruby-rspec
           ruby-rspec-mocks
           ruby-shoulda
           ruby-simplecov))
    (propagated-inputs
     (list ruby-addressable
           ruby-colorator
           ruby-em-websocket
           ruby-i18n
           ruby-jekyll-sass-converter
           ruby-jekyll-watch
           ruby-kramdown-parser-gfm
           ruby-liquid-4
           ruby-mercenary
           ruby-pathutil
           ruby-rouge
           ruby-safe-yaml
           ruby-sassc
           ruby-terminal-table
           ruby-webrick))
    (home-page "https://jekyllrb.com/")
    (synopsis "Static site generator")
    (description "Jekyll is a simple, blog aware, static site generator.")
    (license license:expat)))

(define-public ruby-jekyll-paginate-v2
  (package
    (name "ruby-jekyll-paginate-v2")
    (version "3.0.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "jekyll-paginate-v2" version))
              (sha256
               (base32
                "1qzlqhpiqz28624fp0ak76hfy7908w6kpx62v7z43aiwjv0yc6q0"))))
    (build-system ruby-build-system)
    (propagated-inputs
     (list jekyll))
    (home-page "https://github.com/sverrirs/jekyll-paginate-v2")
    (synopsis "Pagination Generator for Jekyll 3")
    (description "The Pagination Generator forms the core of the pagination
logic in Jekyll.  It calculates and generates the pagination pages.")
    (license license:expat)))

(define-public ruby-faraday-net-http
  (package
    (name "ruby-faraday-net-http")
    (version "3.0.2")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "faraday-net_http" version))
              (sha256
               (base32
                "13byv3mp1gsjyv8k0ih4612y6vw5kqva6i03wcg4w2fqpsd950k8"))))
    (build-system ruby-build-system)
    ;; Do not run the test suite here as it would introduce a dependency cycle
    ;; with ruby-faraday, which uses it as part of its test suite.
    (arguments (list #:tests? #f))
    (synopsis "Faraday adapter for Net::HTTP")
    (description "This gem is a Faraday adapter for the @code{Net::HTTP}
library.  Faraday is an HTTP client library that provides a common interface
over many adapters.")
    (home-page "https://github.com/lostisland/faraday-net_http")
    (license license:expat)))

(define-public ruby-faraday
  (package
    (name "ruby-faraday")
    (version "2.7.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/lostisland/faraday")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0ya6jqa7ryr4i62mmzjjxzd8i8y0pyw0cbhifd758rs6lvkzmxa3"))))
    (build-system ruby-build-system)
    (arguments (list #:test-target "spec"))
    (native-inputs
     (list ruby-coveralls
           ruby-pry
           ruby-rack
           ruby-rspec
           ruby-simplecov
           ruby-webmock))
    ;; Propagate faraday-net-http as this is the default adapter used, and
    ;; many Ruby projects assumes it is available.
    (propagated-inputs (list ruby-ruby2-keywords ruby-faraday-net-http))
    (synopsis "Ruby HTTP/REST API client library")
    (description "Faraday is a HTTP/REST API client library which provides a
common interface over different adapters.")
    (home-page "https://github.com/lostisland/faraday")
    (license license:expat)))

(define-public ruby-nio4r
  (package
   (name "ruby-nio4r")
   (version "2.5.2")
   (source
    (origin
     (method url-fetch)
     (uri (rubygems-uri "nio4r" version))
     (sha256
      (base32
       "0gnmvbryr521r135yz5bv8354m7xn6miiapfgpg1bnwsvxz8xj6c"))))
   (build-system ruby-build-system)
   (arguments
    '(#:phases
      (modify-phases %standard-phases
        (add-after 'unpack 'remove-unnecessary-dependencies
          (lambda _
            (substitute* "spec/spec_helper.rb"
              ;; Coveralls is for uploading test coverage information to an
              ;; online service, and thus unnecessary for building the Guix
              ;; package
              (("require \"coveralls\"") "")
              (("Coveralls\\.wear!") "")
              ;; Remove rspec/retry as we are not retrying the tests
              (("require \"rspec/retry\"") "")
              (("config\\.display_try_failure_messages = true") "")
              (("config\\.verbose_retry = true") ""))
            #t))
        (add-before 'check 'compile
          (lambda _
            (invoke "rake" "compile")
            #t))
        (replace 'check
          (lambda* (#:key tests? #:allow-other-keys)
            (when tests?
              (invoke "rspec"))
            #t)))))
   (native-inputs
    (list bundler ruby-rake-compiler ruby-rspec ruby-rubocop))
   (synopsis "New I/O for Ruby")
   (description
    "@code{nio} provides cross-platform asynchronous I/O primitives in Ruby
for scalable network clients and servers.")
   (home-page "https://github.com/socketry/nio4r")
   (license license:expat)))

(define-public ruby-language-server-protocol
  (package
    (name "ruby-language-server-protocol")
    (version "3.17.0.3")
    (source (origin
              (method git-fetch)        ;for tests
              (uri (git-reference
                    (url "https://github.com/mtsmfm/language_server-protocol-ruby")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0f2g301fz99c6nkca39s9227brlycznv8a9r4b4i99rg25m91lc6"))))
    (build-system ruby-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-unnecessary-dependencies
            (lambda _
              (substitute* "Gemfile"
                (("gem \"pry-byebug\"") ""))))
          (replace 'replace-git-ls-files
            (lambda _
              (substitute* "language_server-protocol.gemspec"
                (("git ls-files -z([^`]*)" _ files)
                 (string-append "find " files
                                " -type f -not -regex '.*\\.gem$'"
                                " -print0 | sort -z"))))))))
    (native-inputs
     (list ruby-activesupport
           ruby-benchmark-ips
           ruby-m
           ruby-minitest
           ruby-minitest-power-assert))
    (synopsis "Language Server Protocol (LSP) development kit for Ruby")
    (description "This package provides a Language Server Protocol (LSP)
development kit for Ruby.")
    (home-page "https://github.com/mtsmfm/language_server-protocol-ruby")
    (license license:expat)))

(define-public ruby-spy
  (package
    (name "ruby-spy")
    (version "1.0.5")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "spy" version))
              (sha256
               (base32
                "0g2mma8q17m26k5s864ndlvvqllhcivwg2wdigjvb7z06iw17gds"))))
    (build-system ruby-build-system)
    (arguments
     (list #:phases #~(modify-phases %standard-phases
                        (add-after 'extract-gemspec 'relax-requirements
                          (lambda _
                            (substitute* "spy.gemspec"
                              ((".*pry-byebug.*") ""))
                            (substitute* "test/test_helper.rb"
                              ((".*pry-byebug.*") ""))
                            (substitute* "Gemfile"
                              ((".*redcarpet.*") "")
                              ((".*yard.*") "")))))))
    (native-inputs
     (list ruby-coveralls
           ruby-minitest-reporters
           ruby-pry
           ruby-rspec-core
           ruby-rspec-expectations))
    (synopsis "Mocking library for Ruby")
    (description "Spy is a mocking library.  By default, it will raise an
error if you attempt to stub a method that doesn't exist or call the stubbed
method with the wrong arity.")
    (home-page "https://github.com/ryanong/spy")
    (license license:expat)))

(define-public ruby-subprocess
  (package
    (name "ruby-subprocess")
    (version "1.5.6")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "subprocess" version))
              (sha256
               (base32
                "0v49ahfx9b75qg42sl8a3l367g2vihc16g8z5f2raxpxjl1wh2s2"))))
    (build-system ruby-build-system)
    ;; Do not run the test suite, as there its test dependency ruby-sord would
    ;; introduce a cycle with ruby-sorbet-runtime.
    (arguments (list #:tests? #f))
    (native-inputs (list ruby-minitest ruby-pry))
    (synopsis "Ruby library to control and communicate with spawned processes")
    (description "This Ruby library is controlling and communicating with
spawned processes.  It is designed after Python's @code{subprocess} module.")
    (home-page "https://github.com/stripe/subprocess")
    (license license:expat)))

(define-public ruby-sus
  (package
    (name "ruby-sus")
    (version "0.20.3")
    (source (origin
              (method git-fetch)        ;for gems.rb
              (uri (git-reference
                    (url "https://github.com/ioquatix/sus")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0j4rkh9li79674h3lfkxlcdygscmb22l77i7hwhxl3gw103gkpdr"))))
    (build-system ruby-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-before 'build 'prune-gems.rb
                 (lambda _
                   (substitute* "gems.rb"
                     (("gem \"bake-modernize\"") "")
                     (("gem \"bake-gem\"") "")
                     (("gem \"utopia-project\"") ""))))
               (add-before 'build 'remove-missing-signing-key
                 (lambda _
                   ;; Otherwise, the build fails with ENOENT.
                   (substitute* "sus.gemspec"
                     ((".*spec.signing_key.*") ""))))
               (delete 'check)          ;moved after install
               (add-after 'install 'check
                 (lambda* (#:key tests? #:allow-other-keys)
                   (when tests?
                     (setenv "CONSOLE_LEVEL" "debug")
                     (setenv "HOME" "/tmp")
                     ;; 'bundle exec' must be used to workaround a problem
                     ;; when using bake test and GEM_PATH (see:
                     ;; https://github.com/ioquatix/bake/issues/11).
                     (invoke "bundle" "exec" "bake" "test"))))
               (add-before 'check 'set-paths
                 (lambda _
                   (setenv "PATH" (string-append (getenv "PATH") ":"
                                                 #$output "/bin"))
                   (setenv "GEM_PATH" (string-append
                                       (getenv "GEM_PATH") ":"
                                       #$output "/lib/ruby/vendor_ruby")))))))
    (native-inputs (list ruby-bake-test ruby-bake-test-external ruby-covered))
    (synopsis "Fast and scalable test runner for Ruby")
    (description "This package provides a fast and scalable test runner for Ruby.")
    (home-page "https://github.com/ioquatix/sus")
    (license license:expat)))

(define-public ruby-syntax-tree
  (package
    (name "ruby-syntax-tree")
    (version "6.1.1")
    (source (origin
              (method git-fetch)        ;for tests
              (uri (git-reference
                    (url "https://github.com/ruby-syntax-tree/syntax_tree")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0356lgvqp22nkqlrgszf7myfdg4arclg278awh34zyby1cx6hb2k"))))
    (build-system ruby-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          ;; The tests rely on the Gem being installed, so move the check
          ;; phase after the install phase.
          (delete 'check)
          (add-after 'install 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (setenv "GEM_PATH" (string-append
                                  #$output "/lib/ruby/vendor_ruby:"
                                  (getenv "GEM_PATH")))
              (when tests?
                (invoke "rake" "test")))))))
    (native-inputs (list ruby-rubocop-ast ruby-simplecov))
    (propagated-inputs (list ruby-prettier-print))
    (synopsis "Fast Ruby parser and formatter")
    (description "Syntax Tree is a suite of tools built on top of the internal
CRuby parser.  It provides the ability to generate a syntax tree from source,
as well as the tools necessary to inspect and manipulate that syntax tree.  It
can be used to build formatters, linters, language servers, and more.")
    (home-page "https://github.com/ruby-syntax-tree/syntax_tree")
    (license license:expat)))

(define-public ruby-stringio
  (package
    (name "ruby-stringio")
    (version "3.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ruby/stringio")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1jgi2w5y0z0x9mfapr2pdlag4wvn03fpf5kbai8bscyh8nn79yka"))))
    (build-system ruby-build-system)
    (native-inputs (list ruby-rake-compiler ruby-test-unit-ruby-core))
    (synopsis "Pseudo `IO` class from and to `String`")
    (description "Pseudo `IO` class from and to `String`.")
    (home-page "https://github.com/ruby/stringio")
    (license license:bsd-2)))

(define-public ruby-stream
  (package
    (name "ruby-stream")
    (version "0.5.5")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "stream" version))
              (sha256
               (base32
                "016m9v81vpj14d8g5ins91zc4pzl7vf5f1gxl7jhfsfy601k7cv2"))))
    (build-system ruby-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'remove-version-constraints
           (lambda _
             (delete-file "Gemfile.lock"))))))
    (native-inputs (list bundler ruby-stringio ruby-webrick ruby-yard))
    (synopsis "Interface for external iterators")
    (description "Module Stream defines an interface for external iterators.")
    (home-page "https://github.com/monora/stream")
    (license license:bsd-2)))

(define sorbet-version "0.5.10610.20230106174520-1fa668010")

(define sorbet-monorepo
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/sorbet/sorbet")
          (commit sorbet-version)))
    (file-name (string-append "sorbet-" sorbet-version "-checkout"))
    (sha256
     (base32
      "0f21dl06alxwn6xgdxyrkd58plmmsv04z2bcls9ld4cfzsrs5537"))))

(define (make-sorbet-gem-source gem)
  "Return the source of GEM, a sub-directory."
  (computed-file
   (string-append "ruby-sorbet-" gem "-" sorbet-version "-checkout")
   (with-imported-modules (source-module-closure '((guix build utils)))
     #~(begin
         (use-modules (guix build utils))
         (copy-recursively (string-append #$sorbet-monorepo
                                          "/gems/sorbet-" #$gem)
                           #$output)))))

(define-public ruby-sorbet-runtime
  (package
    (name "ruby-sorbet-runtime")
    (version sorbet-version)
    (source (make-sorbet-gem-source "runtime"))
    (build-system ruby-build-system)
    ;; 25 out of 841 tests currently fail, seemingly due to invalid
    ;; assumptions about file names in the build environment (see:
    ;; https://github.com/sorbet/sorbet/issues/6650).
    (arguments (list #:tests? #f))
    (native-inputs
     (list ruby-minitest
           ruby-mocha
           ruby-rubocop
           ruby-rubocop-performance
           ruby-concurrent-ruby
           ruby-pry
           ruby-parser
           ruby-subprocess))
    (synopsis "Runtime type checking component for Sorbet")
    (description "Sorbet's runtime type checking component.  Sorbet is a
powerful type checker for Ruby.")
    (home-page "https://sorbet.org")
    (license license:asl2.0)))

(define-public ruby-mustache
  (package
    (name "ruby-mustache")
    (version "1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "mustache" version))
       (sha256
        (base32 "1l0p4wx15mi3wnamfv92ipkia4nsx8qi132c6g51jfdma3fiz2ch"))))
    (build-system ruby-build-system)
    (native-inputs
     `(("ruby-simplecov" ,ruby-simplecov)
       ("test-patch"
        ,(search-patch "ruby-mustache-1.1.1-fix-race-condition-tests.patch"))))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-tests
           (lambda* (#:key inputs #:allow-other-keys)
             (invoke "patch" "-p1" "--batch" "-i"
                     (assoc-ref inputs "test-patch")))))))
    (synopsis "Framework-agnostic way to render logic-free views")
    (description
     "Mustache is a framework-agnostic way to render logic-free views.
Think of Mustache as a replacement for your views.  Instead of views
consisting of ERB or HAML with random helpers and arbitrary logic,
your views are broken into two parts: a Ruby class and an HTML
template.")
    (home-page "https://github.com/mustache/mustache")
    (license license:expat)))

(define-public ruby-mustermann
  (package
    (name "ruby-mustermann")
    (version "3.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "mustermann" version))
       (sha256
        (base32
         "0rwbq20s2gdh8dljjsgj5s6wqqfmnbclhvv2c2608brv7jm6jdbd"))))
    (build-system ruby-build-system)
    (arguments
     ;; No tests.
     '(#:tests? #f))
    (synopsis "Library implementing patterns that behave like regular expressions")
    (description "Given a string pattern, Mustermann will turn it into an
object that behaves like a regular expression and has comparable performance
characteristics.")
    (home-page "https://github.com/sinatra/mustermann")
    (license license:expat)))

(define-public ruby-prettier-print
  (package
    (name "ruby-prettier-print")
    (version "1.2.1")
    (source (origin
              (method git-fetch)        ;for tests
              (uri (git-reference
                    (url "https://github.com/ruby-syntax-tree/prettier_print")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "00cg40jc0il1hpsrpsrqwhsxmx7day9lxp1ksrm08zxzsrz9ykqz"))))
    (build-system ruby-build-system)
    (native-inputs (list ruby-simplecov))
    (synopsis "Compatible and featureful implementation of @code{prettyprint}")
    (description "This package provides a drop-in replacement for the
@code{prettyprint} gem, with more functionality.")
    (home-page "https://github.com/ruby-syntax-tree/prettier_print")
    (license license:expat)))

(define-public ruby-html-proofer
  (package
    (name "ruby-html-proofer")
    (version "5.0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gjtorikian/html-proofer")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "01ksss3ikppc45z2q33bx8bb9785bqlp1rdqascaqg9mhs392adk"))))
    (build-system ruby-build-system)
    (arguments
     (list
      ;; Tests require vcr, which is under the Hippocratic license, which is
      ;; not a free software license (see:
      ;; https://www.gnu.org/licenses/license-list.html#hippocratic).
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'extract-gemspec 'relax-requirements
            (lambda _
              (substitute* "html-proofer.gemspec"
                (("required_ruby_version = \\[\">= 3.1\"")
                 "required_ruby_version = [\">= 2.6\""))))
          (replace 'replace-git-ls-files
            (lambda _
              ;; The html-proofer.gemspec file contains 'all_files = %x(git
              ;; ls-files -z).split("\x0")', but the original phase matches on
              ;; `git ls-files -z`.
              ;; TODO: Improve ruby-build-system patterns on core-updates.
              (substitute* "html-proofer.gemspec"
                (("git ls-files -z")
                 "find . -type f -not -regex '.*\\.gem$' -print0 \
| sort -z | cut -zc3-")))))))
    (propagated-inputs
     (list ruby-addressable
           ruby-mercenary
           ruby-nokogiri
           ruby-parallel
           ruby-rainbow
           ruby-typhoeus
           ruby-yell))
    (synopsis "Test your rendered HTML files to make sure they're accurate")
    (description
     "HTMLProofer is a set of tests to validate your HTML output.  These
tests check if your image references are legitimate, if they have alt tags,
if your internal links are working, and so on.  It's intended to be an
all-in-one checker for your output.")
    (home-page "https://github.com/gjtorikian/html-proofer")
    (license license:expat)))

(define-public ruby-htmlentities
  (package
    (name "ruby-htmlentities")
    (version "4.3.4")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "htmlentities" version))
       (sha256
        (base32
         "1nkklqsn8ir8wizzlakncfv42i32wc0w9hxp00hvdlgjr7376nhj"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (map (lambda (file)
                    (invoke "ruby" "-Itest" file))
                  (find-files "./test" ".*_test\\.rb")))))))
    (synopsis "Encode and decode (X)HTML entities")
    (description
     "This package provides a module for encoding and decoding (X)HTML
entities.")
    (home-page "https://github.com/threedaymonk/htmlentities")
    (license license:expat)))

;;; FIXME: Currently broken with rack 3, awaiting the merge for
;;; https://github.com/sinatra/sinatra/pull/1857 and a new release.
(define-public ruby-sinatra
  (package
    (name "ruby-sinatra")
    (version "3.0.5")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "sinatra" version))
       (sha256
        (base32
         "1ryfja9yd3fq8n1p5yi3qnd0pjk7bkycmxxmbb1bj0axlr1pdv20"))))
    (build-system ruby-build-system)
    (propagated-inputs
     (list ruby-mustermann ruby-rack ruby-rack-protection ruby-tilt))
    (synopsis "DSL for quick web applications creation in Ruby")
    (description
     "Sinatra is a DSL for quickly creating web applications in Ruby with
minimal effort.")
    (home-page "https://sinatrarb.com/")
    (license license:expat)))

(define-public ruby-timeout
  (package
    (name "ruby-timeout")
    (version "0.3.2")
    (source (origin
              (method git-fetch)        ;for tests
              (uri (git-reference
                    (url "https://github.com/ruby/timeout")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0lzhs2c4znzg781w146dhvczhbx7h3wkb90i4v6h68zvm2zfylgj"))))
    (build-system ruby-build-system)
    (synopsis "Timeout library for Ruby")
    (description "Timeout provides a way to auto-terminate a potentially
long-running operation if it hasn't finished in a fixed amount of time.")
    (home-page "https://github.com/ruby/timeout")
    (license (list license:bsd-2))))

(define-public ruby-thin
  (package
    (name "ruby-thin")
    (version "1.7.2")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "thin" version))
       (sha256
        (base32
         "0nagbf9pwy1vg09k6j4xqhbjjzrg5dwzvkn4ffvlj76fsn6vv61f"))))
    (build-system ruby-build-system)
    (arguments
     ;; No tests.
     '(#:tests? #f))
    (propagated-inputs
     (list ruby-daemons ruby-eventmachine ruby-rack))
    (synopsis "Thin and fast web server for Ruby")
    (description "Thin is a Ruby web server that glues together 3 Ruby libraries:
@itemize
@item the Mongrel parser,
@item Event Machine, a network I/O library with high scalability, performance
and stability,
@item Rack, a minimal interface between webservers and Ruby frameworks.
@end itemize\n")
    (home-page "https://github.com/macournoyer/thin")
    (license license:ruby)))

(define-public ruby-truthy
  (package
    (name "ruby-truthy")
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "truthy" version))
              (sha256
               (base32
                "19silgd65j3qwfk5w891p9wcmzdmi9ddm2kg5zbvvqn2h9lkfzmd"))))
    (build-system ruby-build-system)
    (arguments (list #:phases #~(modify-phases %standard-phases
                                  (replace 'check
                                    (lambda* (#:key tests? #:allow-other-keys)
                                      (when tests?
                                        (substitute* "spec/spec_helper.rb"
                                          (("require 'spec'")
                                           "require 'rspec'"))
                                        (invoke "rspec")))))))
    (native-inputs (list ruby-rspec))
    (synopsis "Object truthiness-related Ruby library")
    (description "This library makes it easier to discover the truth values of
various Ruby objects.")
    (home-page "https://github.com/ymendel/truthy")
    (license license:expat)))

(define-public ruby-skinny
  (package
    (name "ruby-skinny")
    (version "0.2.4")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "skinny" version))
       (sha256
        (base32
         "1y3yvx88ylgz4d2s1wskjk5rkmrcr15q3ibzp1q88qwzr5y493a9"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f ; No included tests
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'patch-gemspec
           (lambda _
             (substitute* ".gemspec"
               (("<eventmachine>.freeze, \\[\\\"~> 1.0.0\"")
                "<eventmachine>, [\">= 1.0.0\"")
               (("<thin>.freeze, \\[\\\"< 1.7\", ") "<thin>, ["))
             #t)))))
    (propagated-inputs
     (list ruby-eventmachine ruby-thin))
    (synopsis "Simple, upgradable WebSockets for Ruby Thin")
    (description "Skinny is a simple, upgradable WebSockets for Ruby, using
the Thin library.")
    (home-page "https://github.com/sj26/skinny")
    (license license:expat)))

(define-public ruby-sys-filesystem
  (package
    (name "ruby-sys-filesystem")
    (version "1.3.4")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "sys-filesystem" version))
              (sha256
               (base32
                "0mizqnsiagagmracadr16s5na2ks2j3ih1w0f3gp4ssrda6szl01"))))
    (build-system ruby-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (add-before 'check 'set-HOME
                    (lambda _
                      ;; Some tests attempt to stat $HOME.  Let them.
                      (setenv "HOME" "/tmp")
                      #t)))))
    (propagated-inputs
     (list ruby-ffi))
    (native-inputs
     (list ruby-mkmf-lite))
    (synopsis "Gather file system information")
    (description
     "The @code{sys-filesystem} library provides a cross-platform interface
for gathering file system information, such as disk space and mount points.")
    (home-page "https://github.com/djberg96/sys-filesystem")
    (license license:asl2.0)))

(define-public mailcatcher
  (package
    (name "mailcatcher")
    (version "0.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "mailcatcher" version))
       (sha256
        (base32
         "02w1ycyfv7x0sh9799lz7xa65p5qvl5z4pa8a7prb68h2zwkfq0n"))))
    (build-system ruby-build-system)
    (arguments
     ;; Tests require web/assets which is not included in the output.  We
     ;; might be able to fix this by adding the Git repository to the GEM_PATH
     ;; of the tests.  See ruby-mysql2.
     '(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'patch-gemspec
           (lambda _
             (substitute* ".gemspec"
               (("<eventmachine>.freeze, \\[\\\"= 1.0.9.1")
                "<eventmachine>, [\">= 1.0.9.1")
               (("<rack>.freeze, \\[\\\"~> 1.5") "<rack>, [\">= 1.5")
               (("<thin>.freeze, \\[\\\"~> 1.5.0") "<thin>, [\">= 1.5.0")
               (("<sinatra>.freeze, \\[\\\"~> 1.2") "<sinatra>, [\">= 1.2"))
             #t))
         (add-before 'build 'loosen-dependency-contraint
             (lambda _
               (substitute* "lib/mail_catcher.rb"
                 (("\"eventmachine\", \"1.0.9.1\"") "\"eventmachine\", \">= 1.0.9.1\"")
                 (("\"rack\", \"~> 1.5\"") "\"rack\", \">= 1.5\"")
                 (("\"thin\", \"~> 1.5.0\"") "\"thin\", \">= 1.5.0\"")
                 (("\"sinatra\", \"~> 1.2\"") "\"sinatra\", \">= 1.2\""))
               #t)))))
    (inputs
     (list ruby-eventmachine
           ruby-mail
           ruby-rack
           ruby-sinatra
           ruby-skinny
           ruby-sqlite3
           ruby-thin))
    (synopsis "SMTP server which catches messages to display them a browser")
    (description
     "MailCatcher runs a super simple SMTP server which catches any message
sent to it to display in a web interface.  Run mailcatcher, set your favourite
app to deliver to smtp://127.0.0.1:1025 instead of your default SMTP server,
then check out http://127.0.0.1:1080 to see the mail.")
    (home-page "https://mailcatcher.me")
    (license license:expat)))

(define-public ruby-backport
  (package
    (name "ruby-backport")
    (version "1.1.2")
    (source
     (origin
       ;; The gem does not include test code, so fetch from the Git repository.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/castwide/backport")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "18fpg1n7n2z02ykz9v1x1q0cqa2lvivf8ygka768s01q1r9wfwv2"))))
    (build-system ruby-build-system)
    (arguments
     `(#:test-target "spec"))
    (native-inputs
     (list bundler ruby-rspec))
    (inputs
     (list ruby-simplecov))
    (synopsis "Pure Ruby library for event-driven IO")
    (description
     "This package provides a pure Ruby library for event-driven IO.")
    (home-page "https://github.com/castwide/backport")
    (license license:expat)))

(define-public ruby-json-schema
  (package
    (name "ruby-json-schema")
    (version "2.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "json-schema" version))
       (sha256
        (base32
         "1yv5lfmr2nzd14af498xqd5p89f3g080q8wk0klr3vxgypsikkb5"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f ; no tests
       #:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda _
             (invoke "gem" "build" ".gemspec"))))))
    (propagated-inputs
     (list ruby-addressable))
    (synopsis "Ruby JSON Schema Validator")
    (description "This library provides Ruby with an interface for validating
JSON objects against a JSON schema conforming to JSON Schema Draft 4.  Legacy
support for JSON Schema Draft 3, JSON Schema Draft 2, and JSON Schema Draft 1
is also included.")
    (home-page "https://github.com/ruby-json-schema/json-schema")
    (license license:expat)))

(define-public swagger-diff
  (package
    (name "swagger-diff")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "swagger-diff" version))
       (sha256
        (base32
         "18kbrijkafs3vfsbaqz0cqfj7jrz3aj8xr4fgn5if63wlximybv2"))))
    (build-system ruby-build-system)
    (arguments
     (list
      #:test-target "spec"
      #:phases
      #~(modify-phases %standard-phases
          ;; Don't run or require rubocop, the code linting tool, as this is a
          ;; bit unnecessary.
          (add-after 'unpack 'dont-run-rubocop
            (lambda _
              (substitute* "Rakefile"
                ((".*rubocop.*") "")
                ((".*RuboCop.*") "")))))))
    (propagated-inputs
     (list ruby-json-schema))
    (native-inputs
     (list bundler ruby-rspec-core ruby-rspec-expectations))
    (synopsis
     "Compare Open API Initiative specification files")
    (description
     "Swagger::Diff is a utility for comparing two different Open API
Initiative (OAI) specifications (formerly known as Swagger specifications).
It is intended to determine whether a newer API specification is
backwards-compatible with an older API specification.")
    (home-page "https://github.com/civisanalytics/swagger-diff")
    (license license:bsd-3)))

(define-public ruby-reverse-markdown
  (package
    (name "ruby-reverse-markdown")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "reverse_markdown" version))
       (sha256
        (base32
         "0w7y5n74daajvl9gixr91nh8670d7mkgspkk3ql71m8azq3nffbg"))))
    (build-system ruby-build-system)
    (propagated-inputs
     (list ruby-nokogiri))
    (native-inputs
     (list bundler ruby-rspec ruby-kramdown ruby-simplecov))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "rspec"))
             #t)))))
    (synopsis "Convert HTML into Markdown")
    (description
     "This Ruby module allows you to map simple HTML back into
Markdown---e.g., if you want to import existing HTML data in your
application.")
    (home-page "https://github.com/xijo/reverse_markdown")
    (license license:wtfpl2)))

(define-public ruby-solargraph
  (package
    (name "ruby-solargraph")
    (version "0.40.3")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "solargraph" version))
       (sha256
        (base32
         "1gf049rm0yvw4r8r5yyi890idbfg8qh0dikqx5prvkh11srl73bz"))))
    (build-system ruby-build-system)
    (propagated-inputs
     (list ruby-backport
           bundler
           ruby-benchmark
           ruby-e2mmap
           ruby-jaro-winkler
           ruby-kramdown
           ruby-kramdown-parser-gfm
           ruby-maruku
           ruby-nokogiri
           ruby-parser
           ruby-reverse-markdown
           ruby-rubocop
           ruby-thor
           ruby-tilt
           ruby-yard))
    (native-inputs
     (list ruby-rspec ruby-pry ruby-simplecov ruby-webmock))
    ;; FIXME: can't figure out how to run the tests properly:

    ;; An error occurred while loading spec_helper.
    ;; Failure/Error: return gem_original_require(path)
    ;; LoadError:
    ;; cannot load such file -- spec_helper
    (arguments
     '(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "rspec"))
             #t)))))
    (synopsis
     "IDE tools for code completion, inline documentation, and static analysis")
    (description
     "Solargraph provides a comprehensive suite of tools for Ruby
programming: intellisense, diagnostics, inline documentation, and type
checking.")
    (home-page "https://solargraph.org/")
    (license license:expat)))

(define-public ruby-wayback-machine-downloader
  (package
    (name "ruby-wayback-machine-downloader")
    (version "2.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri
             "wayback_machine_downloader"
             version))
       (sha256
        (base32
         "12kb1qmvmmsaihqab1prn6cmynkn6cgb4vf41mgv22wkcgv5wgk2"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ; no tests
    (synopsis "Download archived websites from the Wayback Machine")
    (description
     "Wayback Machine Downloader is a command line tool for downloading
websites from the Internet Archive's Wayback Machine (archive.org).
It allows fine grained control over what to download by specifying
which snapshots to consider and what files to include.")
    (home-page
     "https://github.com/hartator/wayback-machine-downloader")
    (license license:expat)))

(define-public ruby-zeitwerk
  (package
    (name "ruby-zeitwerk")
    (version "2.6.7")
    (source
      (origin
        (method git-fetch)              ;for tests
        (uri (git-reference
               (url "https://github.com/fxn/zeitwerk")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "10p1ycv72yas1fdqrmdyz1aiqf8axj6q1kyllni2wknhk059jvi0"))))
    (build-system ruby-build-system)
    (native-inputs
     (list ruby-minitest
           ruby-minitest-focus
           ruby-minitest-proveit
           ruby-minitest-reporters))
    (synopsis "Efficient and thread-safe code loader for Ruby")
    (description
     "Zeitwerk implements constant autoloading with Ruby semantics.  Each gem
and application may have their own independent autoloader, with its own
configuration, inflector, and logger.  Supports autoloading, reloading, and
eager loading.")
    (home-page "https://github.com/fxn/zeitwerk")
    (license license:expat)))

(define-public ruby-wwtd
  (package
    (name "ruby-wwtd")
    (version "1.4.1")
    (home-page "https://github.com/grosser/wwtd")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0gw7vfnbb41cy67yw82zji3jkhfsgmzcgzaszm99ax77y18wclf2"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Remove bundled library.
                  (delete-file "spec/rake-12.3.0.gem")
                  #t))))
    (build-system ruby-build-system)
    (arguments
     '(;; XXX: Tests need multiple versions of ruby, wants to run
       ;; `bundle install`, etc.
       #:tests? #f
       #:phases (modify-phases %standard-phases
                  (replace 'replace-git-ls-files
                    (lambda _
                      (substitute* "wwtd.gemspec"
                        (("git ls-files lib/ bin/`")
                         "find lib/ bin/ -type f |sort`"))
                      #t))
                  (add-before 'check 'remove-version-constraints
                    (lambda _
                      (delete-file "Gemfile.lock")
                      #t))
                  (replace 'check
                    (lambda* (#:key tests? #:allow-other-keys)
                      (if tests?
                          (invoke "rspec" "spec/")
                          (format #t "test suite not run~%"))
                      #t)))))
    (native-inputs
     (list ruby-bump ruby-rspec))
    (synopsis "Run @file{.travis.yml} files locally")
    (description
     "WWTD is a @dfn{Travis Simulator} that lets you run test matrices
defined in @file{.travis.yml} on your local machine, using @code{rvm},
@code{rbenv}, or @code{chruby} to test different versions of Ruby.")
    (license license:expat)))

(define-public ruby-rugged
  ;; The last release is old and doesn't build anymore (see:
  ;; https://github.com/libgit2/rugged/issues/951).
  (let ((commit "6379f23cedd5f527cf6a5c229627e366b590a22d")
        (revision "0"))
    (package
      (name "ruby-rugged")
      (version (git-version "1.6.2" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/libgit2/rugged")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0yac7vm0l2jsdsxf2k7xbny4iyzsy8fhiy2g5sphhffp7xgynny8"))))
      (build-system ruby-build-system)
      (arguments
       (list #:gem-flags
             #~(list "--" "--use-system-libraries")
             #:phases
             #~(modify-phases %standard-phases
                 (add-after 'unpack 'adjust-extconf.rb
                   (lambda _
                     ;; Neither using --with-git2-dir=$prefix nor providing
                     ;; pkg-config allows locating the libgit2 prefix (see:
                     ;; https://github.com/libgit2/rugged/issues/955).
                     (substitute* "ext/rugged/extconf.rb"
                       (("LIBGIT2_DIR = File.join.*'vendor', 'libgit2'.*")
                        (format #f "LIBGIT2_DIR = ~s~%"
                                #$(this-package-input "libgit2"))))))
                 (delete 'check)        ;moved after the install phase
                 (add-after 'install 'check
                   (assoc-ref %standard-phases 'check))
                 (add-before 'check 'set-GEM_PATH
                   (lambda _
                     (setenv "GEM_PATH" (string-append
                                         (getenv "GEM_PATH") ":"
                                         #$output "/lib/ruby/vendor_ruby"))))
                 (add-before 'check 'disable-problematic-tests
                   (lambda _
                     (with-directory-excursion "test"
                       (for-each delete-file
                                 ;; These tests require an actual libgit2 git
                                 ;; repository checkout.
                                 '("blame_test.rb"
                                   "blob_test.rb"
                                   "cherrypick_test.rb"
                                   "config_test.rb"
                                   "commit_test.rb"
                                   "diff_test.rb"
                                   "index_test.rb"
                                   "merge_test.rb"
                                   "note_test.rb"
                                   "object_test.rb"
                                   "patch_test.rb"
                                   "rebase_test.rb"
                                   "reference_test.rb"
                                   "remote_test.rb"
                                   "repo_apply_test.rb"
                                   "repo_ignore_test.rb"
                                   "repo_pack_test.rb"
                                   "repo_reset_test.rb"
                                   "repo_test.rb"
                                   "revert_test.rb"
                                   "settings_test.rb"
                                   "status_test.rb"
                                   "submodule_test.rb"
                                   "tag_test.rb"
                                   "tree_test.rb"
                                   "walker_test.rb"))
                       (delete-file-recursively "online")))))))
      (native-inputs (list git-minimal/pinned ruby-rake-compiler))
      (inputs (list libgit2))
      (synopsis "Ruby bindings to the libgit2 linkable C Git library")
      (description "Rugged is a library for accessing libgit2 in Ruby.  It gives
you the speed and portability of libgit2 with the beauty of the Ruby
language.")
      (home-page "https://www.rubydoc.info/gems/rugged")
      (license license:expat))))

(define-public ruby-yell
  (package
    (name "ruby-yell")
    (version "2.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "yell" version))
       (sha256
        (base32
         "1g16kcdhdfvczn7x81jiq6afg3bdxmb73skqjyjlkp5nqcy6y5hx"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (invoke "rake" "examples")))))) ; there is no test target.
    (synopsis
     "Extensible logging library for Ruby")
    (description
     "Yell is a comprehensive logging replacement for Ruby.  It defines
multiple adapters, various log level combinations and message formatting
options.")
    (home-page "https://github.com/rudionrails/yell")
    (license license:expat)))

(define-public ruby-e2mmap
  (package
    (name "ruby-e2mmap")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "e2mmap" version))
       (sha256
        (base32
         "0n8gxjb63dck3vrmsdcqqll7xs7f3wk78mw8w0gdk9wp5nx6pvj5"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f)) ;; There is a rakefile but no tests
    (synopsis
     "Module for defining custom exceptions with specific messages")
    (description
     "Exception2MessageMapper (e2mmap) is a helper module for easily defining
exceptions with predefined messages.")
    (home-page "https://github.com/ruby/e2mmap")
    (license license:bsd-2)))

(define-public ruby-benchmark
  (package
    (name "ruby-benchmark")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "benchmark" version))
       (sha256
        (base32
         "1jvrl7400fv7v2jjri1r7ilj3sri36hzipwwgpn5psib4c9c59c6"))))
    (build-system ruby-build-system)
    (synopsis "Performance benchmarking library")
    (description "This package provides methods for benchmarking Ruby code,
giving detailed reports on the time taken for each task.")
    (home-page "https://github.com/ruby/benchmark")
    (license license:bsd-2)))

(define-public ruby-jekyll-feed
  (package
    (name "ruby-jekyll-feed")
    (version "0.15.1")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "jekyll-feed" version))
        (sha256
          (base32
            "1zxqkrnix0xiw98531h5ga6h69jhzlx2jh9qhvcl67p8nq3sgza9"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #false))     ;there are none
    (propagated-inputs
      (list jekyll))
    (synopsis
      "Jekyll plugin to generate an Atom feed of your Jekyll posts")
    (description
      "This package provides a Jekyll plugin to generate an Atom feed
of your Jekyll posts.")
    (home-page
      "https://github.com/jekyll/jekyll-feed")
    (license license:expat)))

(define-public ruby-jekyll-sitemap
  (package
    (name "ruby-jekyll-sitemap")
    (version "1.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "jekyll-sitemap" version))
        (sha256
          (base32
            "0622rwsn5i0m5xcyzdn86l68wgydqwji03lqixdfm1f1xdfqrq0d"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #false))     ;there are none
    (propagated-inputs
      (list jekyll))
    (synopsis
      "Automatically generate a sitemap.xml for your Jekyll site")
    (description
      "This package provides a Jekyll plugin to silently generate
a sitemaps.org compliant sitemap for your Jekyll site.")
    (home-page
      "https://github.com/jekyll/jekyll-sitemap")
    (license license:expat)))

(define-public ruby-jekyll-seo-tag
  (package
   (name "ruby-jekyll-seo-tag")
   (version "2.7.1")
   (source
    (origin
     (method url-fetch)
     (uri (rubygems-uri "jekyll-seo-tag" version))
     (sha256
      (base32
       "0fsi75hymk2wswy216fs224p5ycrzjw1kshw1bsl5czhv42wr2w3"))))
   (build-system ruby-build-system)
   (arguments
    `(#:tests? #false))
   (propagated-inputs
    (list jekyll))
   (synopsis
    "Jekyll plugin to add metadata tags for search engines and social networks")
   (description
    "This package provides a Jekyll plugin to add metadata tags for search engines
and social networks to better index and display your site's content.")
   (home-page
    "https://github.com/jekyll/jekyll-seo-tag")
   (license license:expat)))

(define-public ruby-taskjuggler
  (package
    (name "ruby-taskjuggler")
    (version "3.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "taskjuggler" version))
       (sha256
        (base32
         "1jrsajzhzpnfa8hj6lbf7adn8hls56dz3yw1gvzgz9y4zkka3k9v"))))
    (build-system ruby-build-system)
    (native-inputs (list tzdata-for-tests))
    (propagated-inputs
     (list ruby-mail ruby-term-ansicolor))
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (replace 'replace-git-ls-files
                    (lambda _
                      (substitute* "tasks/rdoc.rake"
                        (("`git ls-files -- lib`")
                         "`find lib/ -type f |sort`"))
                      #t))
                  (add-before 'check 'tzdir-setup
                    (lambda* (#:key inputs #:allow-other-keys)
                      (setenv "TZDIR"
                              (string-append (assoc-ref inputs "tzdata")
                                             "/share/zoneinfo"))
                      #t))
                  (add-before 'check 'delete-test-BatchProcessor
                    ;; test_BatchProcessor fails with exception:
                    ;; run> terminated with exception (report_on_exception is true)
                    (lambda _
                      (delete-file "test/test_BatchProcessor.rb")
                      #t)))))
    (synopsis
     "Project management command line tool with a domain specific language")
    (description
     "TaskJuggler (tj3) is a project management tool for project planning and
tracking using a domain specific language; projects are plain text files
written using your favourite text editor.  It includes reporting in HTML, CSV
or iCalendar format and an email based status tracking system to send and
receive time sheets from collaborators.

It covers the complete spectrum of project management tasks from the first
idea to the completion of the project.  It assists you during project scoping,
resource assignment, cost and revenue planning, risk and communication
management, status tracking and reporting.")
    (home-page "https://taskjuggler.org")
    (license license:gpl2)))

(define-public ruby-cmath
  (package
    (name "ruby-cmath")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "cmath" version))
       (sha256
        (base32
         "1xkz6xyhpkjbdvpdib8450w62rls1mjryz0gzbbnadxkxn82nb8m"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #false))
    (native-inputs
     (list bundler ruby-rake-compiler))
    (synopsis "Trigonometric functions for complex numbers")
    (description
     "This gem is a library that provides trigonometric and transcendental
functions for complex numbers.  The functions in this module accept integers,
floating-point numbers or complex numbers as arguments.")
    (home-page "https://github.com/ruby/cmath")
    (license license:bsd-2)))

(define-public ruby-sucker-punch
  (package
    (name "ruby-sucker-punch")
    (version "3.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "sucker_punch" version))
       (sha256
        (base32 "12by9vx8q6l4i56q62k1s1ymaxbpg4rny5zngj5i5h09kyh2yp4p"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'extract-gemspec 'less-strict-dependencies
           (lambda _
             (substitute* "sucker_punch.gemspec"
               (("1.0.0") "1.0")))))))
    (native-inputs
     (list
      ruby-pry))
    (propagated-inputs
     (list
      ruby-concurrent))
    (home-page "https://github.com/brandonhilkert/sucker_punch")
    (synopsis "Asynchronous processing library for Ruby")
    (description "Sucker Punch is a single-process Ruby asynchronous processing
library.  It is perfect for asynchronous processes like emailing, data crunching
or social platform manipulation; and generally recommended for jobs that are
fast and non-mission critical like logs, emails, etc.")
    (license license:expat)))

(define-public ruby-countdownlatch
  (package
    (name "ruby-countdownlatch")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "countdownlatch" version))
        (sha256
          (base32 "1v6pbay6z07fp7yvnba1hmyacbicvmjndd8rn2h1b5rmpcb5s0j3"))))
    (build-system ruby-build-system)
    (home-page "https://github.com/benlangfeld/countdownlatch")
    (synopsis "Thread synchronization aid Ruby")
    (description "CountDownLatch is a synchronization aid that allows one or
more threads to wait until a set of operations being performed in other threads
completes.")
    (license license:expat)))

(define-public ruby-value-semantics
  (package
    (name "ruby-value-semantics")
    (version "3.6.1")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "value_semantics" version))
        (sha256
          (base32 "1vdwai8wf6r1fkvdpyz1vzxm89q7ghjvb3pqpg2kvwibwzd99dnx"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "rspec")))))))
    (native-inputs
     (list
      ruby-rspec))
    (home-page "https://github.com/tomdalling/value_semantics")
    (synopsis "Ruby gem for making value classes")
    (description "ValueSemantics generates modules that provide conventional
value semantics for a given set of attributes.  The behaviour is similar to an
immutable Struct class, plus extensible, lightweight validation and coercion.")
    (license license:expat)))

(define-public ruby-promise
  (package
    (name "ruby-promise")
    (version "0.7.4")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "promise.rb" version))
        (sha256
          (base32 "0a819sikcqvhi8hck1y10d1nv2qkjvmmm553626fmrh51h2i089d"))))
    (build-system ruby-build-system)
    (arguments
     `(#:test-target "spec"
       #:phases
       (modify-phases %standard-phases
         (add-after 'extract-gemspec 'less-strict-dependencies
           (lambda _
             (substitute* "Rakefile"
               (("if Gem.ruby_version.*") "if false\n"))
             (substitute* "spec/spec_helper.rb"
               ((".*devtools/spec_helper.*") "\n")))))))
    (native-inputs
     (list
      ruby-rspec
      ruby-rspec-its
      ruby-awesome-print
      ruby-fuubar))
    (home-page "https://github.com/lgierth/promise.rb")
    (synopsis "Asynchronous operation library for Ruby")
    (description "Promise is a Ruby implementation of the Promises/A+
specification.  It provides 100% mutation coverage, tested on MRI 1.9, 2.0, 2.1,
2.2, Rubinius, and JRuby.")
    (license license:unlicense)))

(define-public ruby-multicodecs
  (package
    (name "ruby-multicodecs")
    (version "0.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "multicodecs" version))
        (sha256
          (base32 "0drq267di57l9zqw6zvqqimilz42rbc8z7392dwkk8wslq30s7v8"))))
    (build-system ruby-build-system)
    (home-page "https://github.com/SleeplessByte/ruby-multicodec")
    (synopsis "Ruby implementation of multiformats/multicodec")
    (description "Multicodecs is the ruby implementation of
multiformats/multicodec, a canonical table of of codecs used by various
multiformats.")
    (license license:expat)))

(define-public ruby-multihashes
  (package
    (name "ruby-multihashes")
    (version "0.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "multihashes" version))
        (sha256
          (base32 "17wiyy3fiv8rpgdv9ca01yncsmaaf8yg15bg18wc7m9frss1vgqg"))))
    (build-system ruby-build-system)
    (propagated-inputs
     (list
      ruby-multicodecs))
    (home-page "https://github.com/multiformats/ruby-multihash")
    (synopsis "Multihash implementation for Ruby")
    (description "Multihashes provides a simple, low-level multihash
implementation for Ruby.  A multihash is a digest with an embedded hash function
code")
    (license license:expat)))

(define-public ruby-lazy-object
  (package
    (name "ruby-lazy-object")
    (version "0.0.3")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "lazy_object" version))
        (sha256
          (base32 "08px15lahc28ik9smvw1hgamf792gd6gq0s4k94yq1h7jq25wjn8"))))
    (build-system ruby-build-system)
    (arguments
     `(#:test-target "spec"))
    (home-page "https://github.com/HornsAndHooves/lazy_object")
    (synopsis "Object wrapper that forwards all calls to the reference object")
    (description "LazyObject is an object wrapper that forwards all calls to the
reference object.  This object is not created until the first method dispatch.")
    (license license:expat)))

(define-public ruby-citrus
  (package
    (name "ruby-citrus")
    (version "3.0.2")
    (source
     (origin
       (method git-fetch)
       ;; Download from GitHub because the rubygems version does not contain
       ;; files needed for tests.
       (uri (git-reference
             (url "https://github.com/mjackson/citrus")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "197wrgqrddgm1xs3yvjvd8vkvil4h4mdrcp16jmd4b57rxrrr769"))))
    (build-system ruby-build-system)
    (home-page "https://mjackson.github.io/citrus/")
    (synopsis "Parsing Expressions for Ruby")
    (description "Citrus is a parsing library for Ruby that combines the
expressiveness of the language with the parsing expressions.")
    (license license:expat)))

(define-public ruby-cbor
  (package
    (name "ruby-cbor")
    (version "0.5.9.6")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "cbor" version))
        (sha256
          (base32 "0511idr8xps9625nh3kxr68sdy6l3xy2kcz7r57g47fxb1v18jj3"))))
    (build-system ruby-build-system)
    (arguments
     `(#:test-target "spec"))
    (native-inputs
     (list
      ruby-rspec
      ruby-rake-compiler
      ruby-yard))
    (home-page "https://cbor.io/")
    (synopsis "Concise Binary Object Representation")
    (description "CBOR is a library for the
@acronym{CBOR, Concise Binary Object Representation} format, based on
Sadayuki Furuhashi's MessagePack library.")
    (license license:asl2.0)))

(define-public ruby-gem-release
  (package
    (name "ruby-gem-release")
    (version "2.2.2")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "gem-release" version))
        (sha256
          (base32 "108rrfaiayi14zrqbb6z0cbwcxh8n15am5ry2a86v7c8c3niysq9"))))
    (build-system ruby-build-system)
    (arguments
     ;; No rakefile
     `(#:tests? #f))
    (home-page "https://github.com/svenfuchs/gem-release")
    (synopsis "Ruby gem plugin for release management")
    (description "GemRelease is a gem plugin that aims at making gem development
easier by automating repetitive work based on conventions, configuration, and
templates.")
    (license license:expat)))

(define-public ruby-base32
  (package
    (name "ruby-base32")
    (version "0.3.4")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "base32" version))
        (sha256
          (base32 "1fjs0l3c5g9qxwp43kcnhc45slx29yjb6m6jxbb2x1krgjmi166b"))))
    (build-system ruby-build-system)
    (native-inputs
     (list
      ruby-gem-release))
    (home-page "https://github.com/stesla/base32")
    (synopsis "Ruby extension for base32 encoding and decoding")
    (description "Base32 is a library which provides base32 decoding and
encoding.")
    (license license:expat)))

(define-public ruby-dhall
  (package
    (name "ruby-dhall")
    (version "0.5.2")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "dhall" version))
        (sha256
          (base32 "09wcq8xc1ynld04r2f332bx8cn7rjc4afaq8hm1dr2fc35jlpn6m"))))
    (build-system ruby-build-system)
    (arguments
     ;; No test in gem archive
     `(#:tests? #f))
    (propagated-inputs
      (list
       ruby-base32
       ruby-cbor
       ruby-citrus
       ruby-lazy-object
       ruby-multihashes
       ruby-promise
       ruby-value-semantics))
    (home-page "https://git.sr.ht/~singpolyma/dhall-ruby")
    (synopsis "Ruby implementation of the Dhall configuration language")
    (description "Dhall.rb is a Ruby implementation of the Dhall configuration
language.  Dhall is a memory safe and non-Turing-complete configuration
language.")
    (license license:gpl3+)))

(define-public ruby-money
  (package
    (name "ruby-money")
    (version "6.16.0")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "money" version))
        (sha256
          (base32 "0jkmsj5ymadik7bvl670bqwmvhsdyv7hjr8gq9z293hq35gnyiyg"))))
    (build-system ruby-build-system)
    (arguments
     ;; No rakefile.
     `(#:tests? #f))
    (propagated-inputs
     (list
      ruby-i18n))
    (home-page "https://rubymoney.github.io/money/")
    (synopsis "Currency conversion library for Ruby")
    (description "RubyMoney provides a library for dealing with money and
currency conversion.  Its features are:
@itemize
@item
Provides a Money class which encapsulates all information about a certain
amount of money, such as its value and its currency.
@item
Provides a Money::Currency class which encapsulates all information about a
monetary unit.
@item
Represents monetary values as integers, in cents; so avoids floating point
rounding errors.
@item
Represents currency as Money::Currency instances providing a high level of
flexibility.
@item Provides APIs for exchanging money from one currency to another.
@end itemize")
    (license license:expat)))

(define-public ruby-monetize
  (package
    (name "ruby-monetize")
    (version "1.11.0")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "monetize" version))
        (sha256
          (base32 "0cna2myxdbwfq0gn6k2hgrh368dq7wld3jklm96443ysykd0difn"))))
    (build-system ruby-build-system)
    (arguments
     `(#:test-target "spec"))
    (native-inputs
     (list
      ruby-rspec))
    (propagated-inputs
     (list
      ruby-money))
    (home-page "https://github.com/RubyMoney/monetize")
    (synopsis "Convert various objects into Money objects")
    (description "Monetize provides a library for converting various objects
into Money objects.")
    (license license:expat)))

(define-public ruby-money-open-exchange-rates
  (package
    (name "ruby-money-open-exchange-rates")
    (version "1.4.0")
    (source
     (origin
       (method git-fetch)
       ;; Download from GitHub because the rubygems version does not contain
       ;; Rakefile.
       (uri (git-reference
             (url "https://github.com/spk/money-open-exchange-rates")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "11xwqli8snr19k48yh8h77sal5vxd4snzq9gxg08v61f0574m3gw"))))
    (build-system ruby-build-system)
    (native-inputs
     (list
      ruby-minitest
      ruby-mocha
      ruby-monetize
      ruby-rake
      ruby-rubocop
      ruby-timecop
      ruby-webmock))
    (propagated-inputs
     (list
      ruby-money))
    (home-page "https://spk.github.io/money-open-exchange-rates/")
    (synopsis "Money open exchange rates for Ruby")
    (description "This package provides a gem that calculates the exchange rate
using published rates from open-exchange-rates.  Compatible with the money gem.")
    (license license:expat)))

(define-public ruby-roda
  (package
    (name "ruby-roda")
    (version "3.57.0")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "roda" version))
        (sha256
          (base32 "0nkfxnbcfnriywvx9kpamp850cwjmqv8ssajc95d0aiyjr4kdrfy"))))
    (build-system ruby-build-system)
    (arguments
     ;; No rakefile
     `(#:tests? #f))
    (propagated-inputs (list ruby-rack))
    (home-page "https://roda.jeremyevans.net")
    (synopsis "Routing Tree Web Toolkit")
    (description "Roda is a routing tree web toolkit, designed for building fast
and maintainable web applications in ruby.")
    (license license:expat)))

(define-public ruby-nori
  (package
    (name "ruby-nori")
    (version "2.6.0")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "nori" version))
        (sha256
          (base32 "066wc774a2zp4vrq3k7k8p0fhv30ymqmxma1jj7yg5735zls8agn"))))
    (build-system ruby-build-system)
    (arguments
     ;; Tests require too old version of rspec
     `(#:tests? #f))
    (native-inputs
     (list ruby-nokogiri
            ruby-rake
            ruby-rspec))
    (home-page "https://github.com/savonrb/nori")
    (synopsis "XML to Hash translator")
    (description "Nori is a simple XML parsing ripped from Crack which in-turn
ripped from Merb.  It supports pluggable parsers and ships with both REXML and
Nokogiri implementations.")
    (license license:expat)))

;; This package is deprecated per upstream and should be phased out.
(define-public ruby-faraday-middleware
  (package
    (name "ruby-faraday-middleware")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "faraday_middleware" version))
       (sha256
        (base32 "1bw8mfh4yin2xk7138rg3fhb2p5g2dlmdma88k82psah9mbmvlfy"))))
    (build-system ruby-build-system)
    (arguments `(#:tests? #f))
    (propagated-inputs (list ruby-faraday))
    (home-page "https://github.com/lostisland/faraday_middleware")
    (synopsis "Various middleware for Faraday")
    (description "Faraday_Middleware is a collection of middleware for the
Faraday-based API wrappers.")
    (license license:expat)))

(define-public ruby-faraday-multipart
  (package
    (name "ruby-faraday-multipart")
    (version "1.0.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/lostisland/faraday-multipart")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0ywxhff40a688n50lxrn4d8y096l8sbrwp1jfz4zd3kdiiygclka"))))
    (build-system ruby-build-system)
    (arguments (list #:test-target "spec"))
    (native-inputs (list ruby-faraday ruby-multipart-parser ruby-rspec))
    (propagated-inputs (list ruby-multipart-post))
    (synopsis "Multipart-post requests extension for Faraday")
    (description "This Ruby gem extends Faraday to perform multipart-post
requests.")
    (home-page "https://github.com/lostisland/faraday-multipart")
    (license license:expat)))

(define-public ruby-bandwidth-iris
  (package
    (name "ruby-bandwidth-iris")
    (version "7.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "ruby-bandwidth-iris" version))
       (sha256
        (base32 "131c4jhyvnrwbhizkks17fi9g85cwsq5f1p8zi408zyf63n7230d"))))
    (build-system ruby-build-system)
    (arguments
     ;; XXX: Tests don't require helper for some reason, so all fail.
     `(#:tests? #f))
    (native-inputs
     (list ruby-rspec
           ruby-yard))
    (propagated-inputs
     (list ruby-activesupport
           ruby-builder
           ruby-faraday
           ruby-faraday-middleware
           ruby-nori))
    (home-page "https://github.com/Bandwidth/ruby-bandwidth-iris")
    (synopsis "Gem for integrating to Bandwidth's Iris API")
    (description "Bandwidth IRIS is a Ruby SDK for Bandwidth Phone Number
Dashboard.  It is a Ruby Client library for IRIS / BBS API.")
    (license license:expat)))

(define-public ruby-selenium-webdriver
  (package
    (name "ruby-selenium-webdriver")
    (version "4.8.5")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "selenium-webdriver" version))
              (sha256
               (base32
                "0wh44vpsyz4mgyq4h482prgiv7hqa5jsj4i7i5hnvv39jb0rfiwm"))))
    (build-system ruby-build-system)
    ;; FIXME: The gem release lacks test files, and the git checkout lacks
    ;; JavaScript source that is generated using Bazel, which isn't available
    ;; in Guix yet, so disable the test suite for now.
    (arguments (list #:tests? #f))
    (propagated-inputs (list ruby-rexml ruby-rubyzip ruby-websocket))
    (synopsis "Selenium browser automation bindings for Ruby")
    (description "Selenium implements the W3C WebDriver protocol to automate
popular browsers.  It aims to mimic the behaviour of a real user as it
interacts with the application's HTML.  It's primarily intended for web
application testing, but any web-based task can be automated.  This package
provides the Ruby bindings of Selenium.")
    (home-page "https://www.selenium.dev/")
    (license license:asl2.0)))

(define-public ruby-sentry-core
  (package
    (name "ruby-sentry-core")
    (version "5.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "sentry-ruby-core" version))
        (sha256
          (base32 "141mrw8wghhsjvln9m6ld3hap3xc5v901jjiz007xywy25725hyd"))))
    (build-system ruby-build-system)
    (arguments
     ;; No rakefile in gem.
     `(#:tests? #f))
    (propagated-inputs
      (list ruby-concurrent
             ruby-faraday))
    (home-page "https://sentry.io/for/ruby/")
    (synopsis "Client interface for the Sentry error logger")
    (description "Sentry-Core provides a gem that provides a client
interface for the Sentry error logger.")
    (license license:expat)))

(define-public ruby-sentry
  (package
    (name "ruby-sentry")
    (version "5.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "sentry-ruby" version))
        (sha256
          (base32 "0by9mvw8rklzpyx59vfija8h3ssfvxvf5nbqxfmygfy6lm1vdngz"))))
    (build-system ruby-build-system)
    (arguments
     ;; No rakefile in gem
     `(#:tests? #f))
    (propagated-inputs
      (list ruby-concurrent
             ruby-faraday
             ruby-sentry-core))
    (home-page "https://sentry.io/for/ruby/")
    (synopsis "Client interface for the Sentry error logger")
    (description "Sentry provides a gem that provides a client
interface for the Sentry error logger.")
    (license license:expat)))

(define-public ruby-webrick
  (package
    (name "ruby-webrick")
    (version "1.8.1")
    (source (origin
              (method git-fetch)        ;for tests
              (uri (git-reference
                    (url "https://github.com/ruby/webrick")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1xb0mk3cghdir65nmj0mblprbf21blli7267b6yyvxclh307yp6s"))))
    (build-system ruby-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-after 'extract-gemspec 'delete-problematic-tests
                 (lambda _
                   ;; The httresponse tests fail for
                   ;; unknown reasons (see:
                   ;; https://github.com/ruby/webrick/issues/112).
                   (delete-file "test/webrick/test_httpresponse.rb"))))))
    (home-page "https://github.com/ruby/webrick")
    (synopsis "HTTP server toolkit")
    (description "WEBrick is an HTTP server toolkit that can be configured as an
HTTPS server, a proxy server, and a virtual-host server.")
    (license license:bsd-2)))

(define-public ruby-websocket
  (let ((commit "950e416a19a76c7e6a673a7e5baa6283476dbec1")
        (revision "1"))
    (package
      (name "ruby-websocket")
      (version (git-version "1.2.9" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/imanel/websocket-ruby")
                      (commit commit)))
                (sha256
                 (base32
                  "1i6r0glpxy47zdf76aqgcpjgcgydla0733hfdhp628pmrinnkgwv"))
                (file-name (git-file-name name version))))
      (build-system ruby-build-system)
      (arguments (list #:test-target "spec"
                       #:phases #~(modify-phases %standard-phases
                                    (add-after 'unpack 'disable-rubocop
                                      (lambda _
                                        (substitute* "Rakefile"
                                          (("require 'rubocop/rake_task'") "")
                                          (("RuboCop::RakeTask.new") "")))))))
      (native-inputs
       (list ruby-rspec
             ruby-webrick))
      (synopsis "WebSocket protocol Ruby library")
      (description "This package provides a Ruby library to handle the WebSocket
protocol.")
      (home-page "https://github.com/imanel/websocket-ruby")
      (license license:expat))))

(define-public ruby-interception
  (package
    (name "ruby-interception")
    (version "0.5")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "interception" version))
        (sha256
          (base32 "01vrkn28psdx1ysh5js3hn17nfp1nvvv46wc1pwqsakm6vb1hf55"))))
    (build-system ruby-build-system)
    (native-inputs (list ruby-rspec))
    (home-page "https://github.com/ConradIrwin/interception")
    (synopsis "Listen to raise in Ruby")
    (description "Interception provides a cross-platform ability to intercept all
exceptions as they are raised.")
    (license license:expat)))

(define-public ruby-pry-rescue
  (package
    (name "ruby-pry-rescue")
    (version "1.5.2")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "pry-rescue" version))
        (sha256
          (base32 "1wn72y8y3d3g0ng350ld92nyjln012432q2z2iy9lhwzjc4dwi65"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'nuke-bad-test
           (lambda _
             (substitute* "spec/source_location_spec.rb"
               (("time = Time.now") "skip")))))))
    (native-inputs
     (list ruby-rspec
            ruby-pry-stack-explorer))
    (propagated-inputs
     (list ruby-interception
            ruby-pry))
    (home-page
      "https://github.com/ConradIrwin/pry-rescue")
    (synopsis "Start Pry session for rescue")
    (description "Pry-Rescue allows you to wrap code, to open a pry session at
any unhandled exceptions.")
    (license license:expat)))

(define-public ruby-braintree
  (package
    (name "ruby-braintree")
    (version "4.12.0")
    (source
     (origin
       (method git-fetch)               ;for tests
       (uri (git-reference
             (url "https://github.com/braintree/braintree_ruby")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0gfgkymy3655drwgs42bj9ap9qib1l30sajxmypmp6s75m9w3gsh"))))
    (build-system ruby-build-system)
    (arguments
     (list
      #:test-target "test:unit"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'disable-rubocop
            (lambda _
              (substitute* "Rakefile"
                (("sh \"rubocop\"") ""))))
          (add-after 'unpack 'relax-requirements
            (lambda _
              (substitute* "Gemfile"
                (("gem \"pry\".*") "gem 'pry'\n")
                (("gem \"rake\".*") "gem 'rake'\n")
                (("gem \"libxml-ruby\", \"3.2.0\"")
                 "gem \"libxml-ruby\", \"~> 3.0.0\"")
                (("gem \"rspec\", \"3.9.0\"")
                 "gem \"rspec\", \">= 3.9.0\"")
                (("gem \"webrick\", \"~>1.7.0\"")
                 "gem \"webrick\", \">=1.7.0\"")
                ((".*gem \"rubocop\".*") "")
                ((".*gem \"rspec_junit_formatter\".*") "")))))))
    (native-inputs
     (list ruby-libxml
           ruby-pry
           ruby-rake
           ruby-rspec
           ruby-webrick))
    (propagated-inputs
     (list ruby-builder
           ruby-rexml))
    (home-page "https://www.braintreepayments.com/")
    (synopsis "Integration access to the Braintree Gateway")
    (description "Braintree provides resources and tools for developers to
integrate Braintree's global payments platform.")
    (license license:expat)))

(define-public ruby-niceogiri
  (package
   (name "ruby-niceogiri")
   (version "1.1.2")
   (source
    (origin
     (method url-fetch)
     (uri (rubygems-uri "niceogiri" version))
     (sha256
      (base32 "1ha93211bc9cvh23s9w89zz7rq8irpf64ccd9arvg8v1sxg2798a"))))
   (build-system ruby-build-system)
   (arguments
    `(#:test-target "spec"
      #:phases
      (modify-phases %standard-phases
        (add-after 'extract-gemspec 'less-strict-dependencies
          (lambda _
            (substitute* "niceogiri.gemspec"
              (("2\\.7") "3.8")      ;rspec
              ((".*dependency.*bundler.*") "\n")
              ((".*dependency.*guard-rspec.*") "\n")))))))
   (native-inputs
    (list ruby-rspec
           ruby-yard))
   (propagated-inputs (list ruby-nokogiri))
   (home-page "https://github.com/benlangfeld/Niceogiri")
   (synopsis "Supplement for Nokogiri")
   (description "Niceogiri provides wrappers and helpers for XML manipulation
using Nokogiri.")
   (license license:expat)))

(define-public ruby-blather
  (package
    (name "ruby-blather")
    (version "2.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "blather" version))
        (sha256
          (base32 "05ry2x835fj4pzk61282pcz86n018cr39zbgwbi213md74i90s7c"))))
    (build-system ruby-build-system)
    (arguments
     ;; XXX: Tests require too old version of rspec.
     `(#:tests? #f))
    (native-inputs
     (list ruby-countdownlatch
            ruby-mocha
            ruby-rb-fsevent
            ruby-rspec
            ruby-yard))
    (propagated-inputs
     (list ruby-activesupport
            ruby-eventmachine
            ruby-niceogiri
            ruby-nokogiri
            ruby-sucker-punch))
    (home-page "https://github.com/adhearsion/blather")
    (synopsis "XMPP Domain Specific Language for Ruby")
    (description "Blather is a XMPP DSL for Ruby written on top of EventMachine
and Nokogiri.")
    (license license:expat)))

(define-public ruby-wapiti
  (package
    (name "ruby-wapiti")
    (version "2.1.0")
    ;; the gem archive lacks tests
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/inukshuk/wapiti-ruby")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1273dqvn6flq2qv9qbp104rgc7zp1gqx4096s0v0z5f0qnhzc7d6"))))
    (build-system ruby-build-system)
    (propagated-inputs
     (list ruby-builder
           ruby-rexml))
    (native-inputs
     (list ruby-pry
           ruby-rake-compiler
           ruby-rspec
           ruby-simplecov))
    (arguments
     (list
      #:test-target "spec"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'replace-git-ls-files 'replace-another-git-ls-files
            (lambda args
              (substitute* "wapiti.gemspec"
                (("`git ls-files spec`")
                 "`find spec -type f | sort`"))))
          (add-before 'build 'compile
            (lambda args
              (invoke "rake" "compile"))))))
    (home-page "https://github.com/inukshuk/wapiti-ruby")
    (synopsis "Wicked fast Conditional Random Fields for Ruby")
    (description
     "The Wapiti-Ruby gem provides a wicked fast linear-chain @acronym{CRF,
Conditional Random Fields} API for sequence segmentation and labelling.  It is
based on the codebase of @url{https://wapiti.limsi.fr, Wapiti}.")
    (license license:bsd-2)))

(define-public ruby-namae
  (package
    (name "ruby-namae")
    (version "1.1.1")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "namae" version))
              (sha256
               (base32
                "1j3nl1klkx3gymrdxfc1hlq4a8qlvhhl9aj5v1v08b9fz27sky0l"))))
    (build-system ruby-build-system)
    (native-inputs
     (list ruby-cucumber
           ruby-rspec
           ruby-simplecov))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'extract-gemspec 'allow-newer-cucumber
            (lambda args
              (substitute* "Gemfile"
                (("'cucumber', '[^']*'")
                 "'cucumber'"))))
          (replace 'check
            ;; Avoid 'rake' so we don't need jeweler.
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (apply invoke
                       "rspec"
                       (find-files "spec" "_spec\\.rb$"))))))))
    (home-page "https://github.com/berkmancenter/namae")
    (synopsis "Parser for human names")
    (description
     "Namae (名前) is a parser for human names.  It recognizes personal names
of various cultural backgrounds and tries to split them into their component
parts (e.g., given and family names, honorifics etc.).")
    (license (list license:bsd-2 license:agpl3+))))

(define-public ruby-ritex
  (package
    (name "ruby-ritex")
    (version "1.0.1")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "ritex" version))
              (sha256
               (base32
                "07rlm3nyz9sxzy1srxs6a31hw81r6w7swrb85fiwi393z8npwc3a"))))
    (build-system ruby-build-system)
    (native-inputs
     (list itex2mml))
    (arguments
     ;; thanks to the Gentoo packagers for figuring this out
     (list
      #:ruby ruby-2.7
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'extract-gemspec 'fix-tests
            (lambda* (#:key native-inputs inputs #:allow-other-keys)
              (substitute* "test/mathml.rb"
                (("\\./itex2MML")
                 ;; don't use the absolute path to avoid keeping a reference
                 "itex2MML")
                (("cmp ',\\\\,\\\\,,,\\\\,'" orig)
                 (string-append "# " orig " # patched for Guix")))
              (substitute* "test/answer-key.yaml"
                (("- ,\\\\,\\\\,,,\\\\," orig)
                 (string-append "# " orig " # patched for Guix")))))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "ruby" "-Ilib:." "test/all.rb")))))))
    (home-page "https://rubygems.org/gems/ritex")
    (synopsis "Convert expressions from WebTeX into MathML")
    (description
     "Ritex converts expressions from WebTeX into MathML.  WebTeX is an
adaptation of TeX math syntax for web display.  Ritex makes inserting math
into HTML pages easy.  It supports most TeX math syntax as well as macros.")
    ;; doesn't clearly state -only vs -or-later
    (license license:gpl2)))

(define-public ruby-latex-decode
  (package
    (name "ruby-latex-decode")
    (version "0.4.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/inukshuk/latex-decode")
                    (commit (string-append "v" version))))
              (sha256
               (base32
                "1f5j67ayd04pjkmzvn0hk7cr8yqvn0gyg9ns6a0vhzj2gwna9ihy"))
              (file-name (git-file-name name version))))
    (build-system ruby-build-system)
    (native-inputs
     (list ruby-cucumber
           ruby-ritex
           ruby-rspec))
    (arguments
     (list
      #:test-target "cucumber"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'extract-gemspec 'avoid-bundler
            (lambda args
              (substitute* "Rakefile"
                (("require 'bundler" orig)
                 (string-append "# " orig " # patched for Guix"))
                (("Cucumber::Rake::Task\\.new[(]:cucumber[)]" orig)
                 (string-append orig " do |c|\n"
                                "  c.bundler = false # patched for Guix\n"
                                "end"))
                (("Bundler\\.setup" orig)
                 (string-append "true # " orig " # patched for Guix")))
              (substitute* "cucumber.yml"
                ;; thanks to avoiding bundler, we can't use this option
                ((" --publish-quiet")
                 ""))))
          (add-after 'replace-git-ls-files 'replace-another-git-ls-files
            (lambda args
              (substitute* "latex-decode.gemspec"
                (("`git ls-files -- [{]test,spec,features[}]/\\*`")
                 "`find {test,spec,features} -type f | sort`")))))))
    (home-page "https://github.com/inukshuk/latex-decode")
    (synopsis "Convert LaTeX to Unicode")
    (description
     "This package provides a gem to convert LaTeX input to Unicode.  Its
original use was as an input filter for BibTeX-Ruby, but it can be used
independently to decode LaTeX.  Many of the patterns used by this Ruby gem are
based on François Charette's equivalent Perl module @code{LaTeX::Decode}.")
    (license license:gpl3+)))

(define-public ruby-link-header
  (package
    (name "ruby-link-header")
    (version "0.0.8")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "link_header" version))
              (sha256
               (base32
                "1yamrdq4rywmnpdhbygnkkl9fdy249fg5r851nrkkxr97gj5rihm"))))
    (build-system ruby-build-system)
    (home-page "https://github.com/asplake/link_header")
    (synopsis "Parse and format HTTP @code{Link} headers")
    (description
     "This gem provides the classes @code{LinkHeader} and
@code{LinkHeader::Link}, which represent HTTP @code{Link} headers conforming
to RFC 5988.  Objects can be constructed from and converted to text or a
JSON-friendly @code{Array} representation.  They can also be used to generate
corresponding HTML @code{link} elements.")
    (license license:expat)))

(define-public ruby-rdf
  (package
    (name "ruby-rdf")
    (version "3.2.8")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "rdf" version))
              (sha256
               (base32
                "1cj0k8ryd8hgbkgqb5swvy6fiygxny3y5bln0my5gv6dbfv3gm20"))))
    (build-system ruby-build-system)
    (propagated-inputs (list ruby-link-header))
    (arguments
     (list #:tests? #f)) ;; tests have many cyclic dependencies
    (home-page "https://ruby-rdf.github.io/")
    (synopsis "Linked Data for Ruby")
    (description
     "This gem contains the core algorithms and classes used for doing basic
programming with @acronym{RDF, Resource Description Framework} data,
implemented in pure Ruby.")
    (license license:unlicense)))

(define-public ruby-rdf-vocab
  (package
    (name "ruby-rdf-vocab")
    (version "3.2.1")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "rdf-vocab" version))
              (sha256
               (base32
                "1bqmp9rfjvd56ajjz68ij6jla1wjf1fqg7bi4dpnjrsmn4pwaq7l"))))
    (build-system ruby-build-system)
    (propagated-inputs
     (list ruby-rdf))
    (arguments
     (list #:tests? #f)) ;; tests have many cyclic dependencies
    (home-page "https://github.com/ruby-rdf/rdf-vocab")
    (synopsis "Common RDF vocabularies")
    (description
     "This gem extends @code{ruby-rdf} with several common @acronym{RDF,
Resource Description Framework} vocabularies.")
    (license license:unlicense)))

(define-public ruby-rdiscount
  (package
    (name "ruby-rdiscount")
    (version "2.2.7")
    (source (origin
              (method git-fetch)        ;for the full test suite
              (uri (git-reference
                    (url "https://github.com/davidfstr/rdiscount")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1lpfxq3gv0dgmnki9jgfnc8n9k4x9vyq9miqdxv6g4kp90qyfifc"))))
    (build-system ruby-build-system)
    (native-inputs (list perl))
    (synopsis "Discount Markdown Processor for Ruby")
    (description "Discount is an implementation of John Gruber's Markdown
markup language in C.  It implements all of the language described in the
markdown syntax document and passes the Markdown 1.0 test suite.")
    (home-page "https://dafoster.net/projects/rdiscount/")
    (license license:bsd-3)))

(define-public ruby-bibtex-ruby
  (package
    (name "ruby-bibtex-ruby")
    (version "6.0.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "bibtex-ruby" version))
              (sha256
               (base32
                "0vynqa8q9hwghw6sdljr304b5gh11nqzy5nwqqwxmgy7pqyf7qw5"))))
    (build-system ruby-build-system)
    (propagated-inputs
     (list ruby-latex-decode
           ruby-rdf
           ruby-rdf-vocab))
    (native-inputs
     (list ruby-cucumber
           ruby-minitest
           ruby-yard))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'extract-gemspec 'avoid-bundler
            (lambda args
              (substitute* "Rakefile"
                (("require 'bundler" orig)
                 (string-append "# " orig " # patched for Guix"))
                (("Bundler\\.setup" orig)
                 (string-append "true # " orig " # patched for Guix"))))))))
    (home-page "https://github.com/inukshuk/bibtex-ruby")
    (synopsis "Rubyist's Swiss Army knife for all things BibTeX")
    (description
     "BibTeX-Ruby is the Rubyist's Swiss Army knife for all things BibTeX.
It includes a parser for all common BibTeX objects and a sophisticated name
parser that tokenizes correctly formatted names.  BibTeX-Ruby recognizes
BibTeX string replacements, joins values containing multiple strings or
variables, supports cross-references, and decodes common LaTeX formatting
instructions to unicode.  If you are in a hurry, it also allows for easy
export/conversion to formats such as YAML, JSON, CSL, and XML (BibTeXML).")
    (license license:gpl3+)))

(define-public ruby-unicode-scripts
  (package
    (name "ruby-unicode-scripts")
    (version "1.8.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "unicode-scripts" version))
              (sha256
               (base32
                "0rl6mn908yryhrg8j3mmna54gnrid2nph2kym00lbz6jwdih2a1b"))))
    (build-system ruby-build-system)
    (native-inputs (list ruby-minitest))
    (arguments (list #:test-target "spec"))
    (home-page "https://github.com/janlelis/unicode-scripts")
    (synopsis "Unicode script classification library")
    (description "This gem provides a simple interface for classifying Ruby
strings using the Unicode @code{Script} and @code{Script_Extensions}
properties.")
    (license license:expat)))

(define-public ruby-citeproc
  (package
    (name "ruby-citeproc")
    (version "1.0.10")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "citeproc" version))
              (sha256
               (base32
                "13vl5sjmksk5a8kjcqnjxh7kn9gn1n4f9p1rvqfgsfhs54p0m6l2"))))
    (build-system ruby-build-system)
    (propagated-inputs
     (list ruby-namae))
    (arguments
     (list #:tests? #f)) ;; tests have a cyclic dependency
    (home-page "https://github.com/inukshuk/citeproc")
    (synopsis "Interface for Ruby citation processors")
    (description
     "CiteProc is a citation processor interface and citation data API based
on the @acronym{CSL, Citation Style Language} specifications.  To actually
process citations, a dedicated processor engine is required: a pure Ruby
engine is available in the @code{citeproc-ruby} gem.")
    (license (list license:agpl3+ license:bsd-2))))

(define-public ruby-ed25519
  (package
    (name "ruby-ed25519")
    (version "1.3.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/RubyCrypto/ed25519")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1jm5w2dyhyrndcx0d02v0gjbzl1abhbx2wkp3gxzwcndghmkz98r"))))
    (build-system ruby-build-system)
    (arguments
     (list
      #:test-target "spec"
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'compile
            (lambda _
              (invoke "rake" "compile")))
          (add-after 'unpack 'remove-unnecessary-dependencies
            (lambda _
              ;; Coveralls relates to a network service, and RuboCop to code
              ;; linting and both are unnecessary to run the tests
              (substitute* "Gemfile"
                ((".*coveralls.*")
                 "\n")
                ((".*rubocop.*")
                 "\n"))
              (substitute* "spec/spec_helper.rb"
                (("require \"coveralls\"")
                 "")
                (("Coveralls.wear!")
                 ""))
              (substitute* "Rakefile"
                (("require \"rubocop/rake_task\"")
                 "")
                (("RuboCop::RakeTask.new")
                 "")))))))
    (native-inputs (list ruby-rake-compiler ruby-rspec))
    (synopsis
     "Ruby binding to the Ed25519 elliptic curve public-key signature system")
    (description
     "This package provides a Ruby binding to the Ed25519 elliptic curve
public-key signature system described in
@url{https://www.ietf.org/rfc/rfc8032.txt, RFC 8032}.")
    (home-page "https://github.com/RubyCrypto/ed25519")
    (license license:expat)))

(define-public ruby-edtf
  (package
    (name "ruby-edtf")
    (version "3.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/inukshuk/edtf-ruby")
                    (commit (string-append "v" version))))
              (sha256
               (base32
                "18j8xq8zmrn41cs2gpd1i87agi9905asvnjqndky2cqb5zg3q14g"))
              (snippet
               ;; remove generated file
               #~(delete-file "lib/edtf/parser.rb"))
              (file-name (git-file-name name version))))
    (build-system ruby-build-system)
    (propagated-inputs
     (list ruby-activesupport))
    (native-inputs
     (list ruby-cucumber
           ruby-rspec))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'extract-gemspec 'avoid-bundler
            (lambda args
              (substitute* "Rakefile"
                (("require 'bundler" orig)
                 (string-append "# " orig " # patched for Guix"))
                (("bundle exec racc")
                 "racc")
                (("Cucumber::Rake::Task\\.new[(]:cucumber[)]" orig)
                 (string-append orig " do |c|\n"
                                "  c.bundler = false # patched for Guix\n"
                                "end"))
                (("Bundler\\.setup" orig)
                 (string-append "true # " orig " # patched for Guix")))))
          (add-after 'avoid-bundler 'patch-cucumber-options
            (lambda args
              (substitute* "cucumber.yml"
                ;; this option is not supported, at least in our configuration
                ((" --publish-quiet")
                 ""))))
          (add-before 'build 'compile
            (lambda args
              (invoke "rake" "racc")))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "rake")))))))
    (home-page "https://github.com/inukshuk/edtf-ruby")
    (synopsis "Ruby implementation of Extended Date/Time Format")
    (description
     "EDTF-Ruby provides a parser and an API for the @acronym{EDTF, Extended
Date/Time Format} standard, implemented as an extension to Ruby's @code{Date}
class.")
    (license license:bsd-2)))

(define-public ruby-gli
  (package
    (name "ruby-gli")
    (version "2.21.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/davetron5000/gli")
                    (commit (string-append "v" version))))
              (sha256
               (base32
                "09b1r9hlx4dy2yq036nk7hc2nbswhia6q3na9v11z94yibc8mgja"))
              (file-name (git-file-name name version))))
    (build-system ruby-build-system)
    (native-inputs
     (list ruby-minitest
           ruby-rainbow
           ruby-rdoc
           ruby-sdoc))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'extract-gemspec 'patch-gemspec-version
            (lambda args
              (substitute* "gli.gemspec"
                ;; this trick fails in our build environment
                (("require File\\.join[(]\\[" orig)
                 (string-append "# patched for Guix # " orig))
                (("s\\.version = GLI::VERSION")
                 #$(string-append "s.version = '"
                                  (package-version this-package)
                                  "' # patched for Guix")))))
          (add-after 'replace-git-ls-files 'replace-another-git-ls-files
            (lambda args
              (substitute* "gli.gemspec"
                (("`git ls-files -- [{]test,spec,features[}]/\\*`")
                 "`find {test,spec,features} -type f | sort`"))))
          (add-after 'replace-another-git-ls-files 'fix-rubyopt
            (lambda args
              (substitute* "Rakefile"
                (("ENV\\[\"RUBYOPT\"]")
                 "(ENV['RUBYOPT'] || '')")))))))
    (home-page "https://davetron5000.github.io/gli/")
    (synopsis "Git-Like Interface command-line parser")
    (description
     "GLI allows you to create command-line applications in Ruby with Git-Like
Interfaces: that is, they take subcommands in the style of @command{git} and
@command{gem}.  GLI uses a simple domain-specific language, but retains all
the power of the built-in @code{OptionParser}.")
    (license license:asl2.0)))

(define-public ruby-anystyle-data
  (package
    (name "ruby-anystyle-data")
    (version "1.2.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/inukshuk/anystyle-data")
                    (commit version)))
              (sha256
               (base32
                "025mxa7r9d7izqn6bc1wr40ijp64da0jh211prlpjl6svilgd6rm"))
              (snippet
               ;; remove pre-built file
               #~(delete-file "lib/anystyle/data/dict.txt.gz"))
              (patches
               (search-patches "ruby-anystyle-data-immutable-install.patch"))
              (file-name (git-file-name name version))))
    (build-system ruby-build-system)
    (arguments
     (list
      #:tests? #f ;; there are none
      #:modules
      `((guix build ruby-build-system)
        (guix build utils)
        (srfi srfi-26))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'replace-git-ls-files 'replace-another-git-ls-files
            (lambda args
              (substitute* "anystyle-data.gemspec"
                (("`git ls-files lib README\\.md LICENSE`\\.split[(][^)]*[)]")
                 (string-append
                  "["
                  (string-join
                   (map (cut string-append "\"" <> "\"")
                        `("README.md"
                          "LICENSE"
                          "lib/anystyle/data.rb"
                          "lib/anystyle/data/dict.txt.gz"
                          "lib/anystyle/data/setup.rb"
                          "lib/anystyle/data/version.rb"))
                   ", ")
                  "]")))))
          (add-before 'build 'compile-dict
            (lambda args
              (invoke "rake" "compile"))))))
    (home-page "https://anystyle.io")
    (synopsis "AnyStyle parser dictionary data")
    (description
     "This gem provides parser dictionary data for AnyStyle.")
    (license license:bsd-2)))

(define-public ruby-anystyle
  (let ((commit "50f1dd547d28ab4b830e45d70e840cb1898a37b0")
        (revision "1"))
    ;; Releases point to specific commits, but recent releases haven't been
    ;; tagged in Git.  Meanwhile, the rubygems archive lacks tests.
    (package
      (name "ruby-anystyle")
      (version (git-version "1.3.14" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/inukshuk/anystyle")
                      (commit commit)))
                (sha256
                 (base32
                  "0f4qcrywl1kl6qysn24lj3yp85ln4i7za7b7ld2fglyzwcggxwb0"))
                (snippet
                 ;; There is an optional dependency on
                 ;; <https://github.com/feedbackmine/language_detector>, which
                 ;; seems like it was intended to be free software, but
                 ;; doesn't have a clear license statement.  Maybe someone can
                 ;; do more sleuthing, or else find a replacement?  See also
                 ;; <https://github.com/inukshuk/anystyle/issues/186>.  For
                 ;; now, patch it out, but leave a pointer to follow up.
                 #~(begin
                     (use-modules (guix build utils))
                     (substitute* "Gemfile"
                       (("gem 'language_detector', github: '[^']*'" orig)
                        (string-append "# " orig " # unclear license")))
                     (substitute* "spec/anystyle/parser_spec.rb"
                       (("language: 'en'," orig)
                        (string-append "# " orig " # no lanugage_detector")))))
                (patches
                 (search-patches
                  "ruby-anystyle-fix-dictionary-populate.patch"))
                (file-name (git-file-name name version))))
      (build-system ruby-build-system)
      (propagated-inputs
       (list ruby-anystyle-data
             ruby-bibtex-ruby
             ruby-namae
             ruby-wapiti))
      (native-inputs
       (list ruby-byebug
             ruby-citeproc
             ruby-edtf
             ruby-rspec
             ruby-unicode-scripts))
      (arguments
       (list
        #:test-target "spec"
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'extract-gemspec 'avoid-bundler
              (lambda args
                (substitute* "Rakefile"
                  (("require 'bundler" orig)
                   (string-append "# " orig " # patched for Guix"))
                  (("Bundler\\.setup" orig)
                   (string-append "true # " orig " # patched for Guix")))))
            (add-after 'replace-git-ls-files 'replace-another-git-ls-files
              (lambda args
                (substitute* "anystyle.gemspec"
                  (("`git ls-files spec`")
                   "`find spec -type f | sort`"))))
            (add-after 'wrap 'populate-dictionaries
              (lambda args
                ;; We must initialize these files here, or they will never be
                ;; usable with the default settings. A more flexible approach
                ;; might use something like `Gem.find_files()` or
                ;; XDG_DATA_DIRS.
                (with-output-to-file "initialize-dictionaries.rb"
                  (lambda ()
                    (display "
require 'anystyle/dictionary' # must come before 'anystyle/data'
require 'anystyle/data'
[:marshal, :gdbm].each do |adapter|
  AnyStyle::Dictionary.create({adapter: adapter}).open().close()
end
")))
                (let* ((old-gems (getenv "GEM_PATH"))
                       (new-gems (string-append #$output
                                                "/lib/ruby/vendor_ruby:"
                                                old-gems)))
                  (dynamic-wind
                    (lambda ()
                      (setenv "GEM_PATH" new-gems))
                    (lambda ()
                      (invoke "ruby" "initialize-dictionaries.rb"))
                    (lambda ()
                      (setenv "GEM_PATH" old-gems)))))))))
      (home-page "https://anystyle.io")
      (synopsis "Fast and smart citation reference parsing (Ruby library)")
      (description
       "AnyStyle is a very fast and smart parser for academic reference lists
and bibliographies.  AnyStyle uses powerful machine learning heuristics based
on Conditional Random Fields and aims to make it easy to train the model with
data that is relevant to your parsing needs.

This package provides the Ruby module @code{AnyStyle}.  AnyStyle can also be
used via the @command{anystyle} command-line utility or a web application,
though the later has not yet been packaged for Guix.")
      (license license:bsd-2))))

(define-public anystyle
  (package
    (name "anystyle")
    (version "1.3.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/inukshuk/anystyle-cli")
                    (commit (string-append "v" version))))
              (sha256
               (base32
                "0yigfyn0n255nc53nx40yqak11dm4fva46hx5db177jh7mnksjd6"))
              (file-name (git-file-name name version))))
    (build-system ruby-build-system)
    (propagated-inputs
     (list ruby-anystyle
           ruby-bibtex-ruby
           ruby-gli))
    (native-inputs
     (list txt2man))
    (arguments
     (list
      #:modules
      `((guix build ruby-build-system)
        (ice-9 popen)
        (srfi srfi-1)
        (guix build utils))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'extract-gemspec 'less-strict-dependencies
            (lambda args
              (substitute* "anystyle-cli.gemspec"
                (("'bibtex-ruby', '[^']*'")
                 "'bibtex-ruby'"))))
          (add-before 'build 'change-default-dictionary-adapter
            (lambda args
              ;; Since we always have gdbm available, using it will give a
              ;; faster startup time, which is particularly worth-while for
              ;; a command-line tool.
              (substitute* "bin/anystyle"
                (("default_value: 'ruby',")
                 "default_value: 'gdbm', # patched for Guix"))))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              ;; There are no tests, but let's use this opportunity to do a
              ;; basic test of our own that things run ok. It works especially
              ;; well to test this here since we know the 'ruby-anystile'
              ;; package is in its final, immutable installed form.
              (when tests?
                (let ((common
                       `("require 'anystyle'"
                         ,(string-append
                           "pp AnyStyle.parse 'Derrida, J. (1967). L’écriture"
                           " et la différence (1 éd.). Paris: Éditions du"
                           " Seuil.'"))))
                  (for-each
                   (lambda (lines)
                     (apply invoke "ruby"
                            (fold-right (lambda (line lst)
                                          (cons* "-e" line lst))
                                        '()
                                        lines)))
                   `(,common
                     ("require 'anystyle/dictionary'"
                      "AnyStyle::Dictionary.defaults[:adapter] = :gdbm"
                      ,@common)))))))
          (add-after 'wrap 'check-cli
            (lambda* (#:key tests? outputs #:allow-other-keys)
              (when tests?
                (with-output-to-file "check-cli.in"
                  (lambda ()
                    (for-each
                     display
                     '("Derrida, J. (1967). L’écriture et la différence "
                       "(1 éd.). Paris: Éditions du Seuil.\n"))))
                (invoke (search-input-file outputs "/bin/anystyle")
                        "parse"
                        "check-cli.in"))))
          (add-after 'wrap 'generate-man-page
            ;; generating a man page also tests that the command actually runs
            (lambda args
              (define (run-with-output-file file command . args)
                (format (current-output-port)
                        "running: ~s\nwith output to: ~s\n"
                        (cons command args)
                        file)
                (unless (zero?
                         (with-output-to-file file
                           (lambda ()
                             (status:exit-val
                              (close-pipe
                               (apply open-pipe* OPEN_WRITE command args))))))
                  (error "command failed")))
              (let ((anystyle (string-append #$output "/bin/anystyle")))
                (run-with-output-file "intro.txt"
                                      anystyle "--help")
                (for-each (lambda (cmd)
                            (let ((file (string-append cmd ".txt")))
                              (run-with-output-file file
                                                    anystyle cmd "--help")
                              ;; indent headings to create subsections
                              (substitute* file
                                (("^[A-Z]" orig)
                                 (string-append " " orig)))
                              ;; generate a section heading
                              (call-with-output-file
                                  (string-append "section-" file)
                                (lambda (out)
                                  (format out "\n\n~a COMMAND\n\n"
                                          (string-upcase cmd))))))
                          '("check" "find" "parse" "train"))
                (substitute* `("intro.txt"
                               "check.txt" "find.txt" "parse.txt" "train.txt")
                  ;; format "tag list" for txt2man"
                  ((" - ")
                   "    ")
                  ;; restore formatting of the "name" sections
                  (("(anystyle|check|find|parse|train)    ([A-Z])" _ cmd post)
                   (string-append cmd " - " post)))
                (run-with-output-file "anystyle.txt"
                                      "cat"
                                      "intro.txt"
                                      "section-check.txt" "check.txt"
                                      "section-find.txt" "find.txt"
                                      "section-parse.txt" "parse.txt"
                                      "section-train.txt" "train.txt")
                (run-with-output-file
                 "anystyle.1"
                 "txt2man"
                 "-v" "General Commands Manual" "-t" "anystyle" "-s" "1"
                 "-r" #$(string-append "anystyle-cli "
                                       (package-version this-package))
                 "-B" "check" "-B" "find" "-B" "parse" "-B" "train"
                 "anystyle.txt")
                (install-file "anystyle.1"
                              (string-append #$output "/share/man/man1"))))))))
    (home-page "https://anystyle.io")
    (synopsis "Fast and smart citation reference parsing")
    (description
     "AnyStyle is a very fast and smart parser for academic reference lists
and bibliographies.  AnyStyle uses powerful machine learning heuristics based
on Conditional Random Fields and aims to make it easy to train the model with
data that is relevant to your parsing needs.

This package provides the @command{anystyle} command-line utility.  AnyStyle
can also be used as a Ruby library or as a web application, though the later
has not yet been packaged for Guix.")
    (license license:bsd-2)
    (properties `((upstream-name . "anystyle-cli")))))

(define-public ruby-google-protobuf
  (package
    (name "ruby-google-protobuf")
    (version "3.25.3")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "google-protobuf" version))
              (sha256
               (base32
                "1mnxzcq8kmyfb9bkzqnp019d1hx1vprip3yzdkkha6b3qz5rgg9r"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))  ;; has no tests
    (native-inputs (list ruby-rake))
    (synopsis "Protocol Buffers are Google's data interchange format")
    (description "This library contains the Ruby extension that implements
Protocol Buffers functionality in Ruby.

The Ruby extension makes use of generated Ruby code that defines message and
enum types in a Ruby DSL.  You may write definitions in this DSL directly, but
we recommend using protoc's Ruby generation support with @code{.proto} files.
The build process in this directory only installs the extension; you need to
install @code{protoc} (in package ruby-grpc-tools) as well to have Ruby code
generation functionality.")
    (home-page "https://protobuf.dev")
    (license license:bsd-3)))

(define-public ruby-googleapis-common-protos-types
  (package
    (name "ruby-googleapis-common-protos-types")
    (version "1.13.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "googleapis-common-protos-types" version))
              (sha256
               (base32
                "1zrxnv9s2q39f2nh32x7nbfi8lpwzmmn3ji4adglg8dlfr1xrz16"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))  ;; has no tests
    (propagated-inputs (list ruby-google-protobuf))
    (synopsis "Common protocol buffer types used by Google APIs")
    (description "Common protocol buffer types used by Google APIs")
    (home-page "https://github.com/googleapis/common-protos-ruby")
    (license license:asl2.0)))

(define-public ruby-grpc
  (package
    (name "ruby-grpc")
    (version "1.62.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "grpc" version))
              (sha256
               (base32
                "03z8yq0z228g6xxxq6s2mmslpv6psrdmi30dpmhysr4px16d897n"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))  ;; has no tests
    ;; TODO remove third-party sources (zlib, upb, utf8-range, re2, c-ares,
    ;; boringssl-with-bazel, address_sorting, abseil-cpp), see Makefile
    (propagated-inputs (list ruby-google-protobuf
                             ruby-googleapis-common-protos-types))
    (synopsis "GRPC system in Ruby")
    (description "GRPC is a high performance, open-source universal RPC
framework.  This package provides a ruby interface for it.")
    (home-page "https://github.com/grpc/grpc/tree/master/src/ruby")
    (license license:asl2.0)))

;;;
;;; Avoid adding new packages to the end of this file. To reduce the chances
;;; of a merge conflict, place them above by existing packages with similar
;;; functionality or similar names.
;;;
