;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Matthew Jordan <matthewjordandevops@yandex.com>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019, 2021, 2022 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2023 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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

(define-module (gnu packages rails)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages node)
  #:use-module (gnu packages ruby)
  #:use-module (guix build-system ruby))

(define %ruby-rails-version "7.0.4.3")

(define ruby-rails-monorepo
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/rails/rails")
          (commit (string-append "v" %ruby-rails-version))))
    (file-name (git-file-name "ruby-rails" %ruby-rails-version))
    (sha256
     (base32
      "0f5f8r8wdmdmbyl07b0z555arai4ys2j8dj3fy0mq63y9bfhcqqk"))))

(define-public ruby-activesupport
  (package
    (name "ruby-activesupport")
    (version %ruby-rails-version)
    (source ruby-rails-monorepo)
    (build-system ruby-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'delete-gemfiles
            (lambda _
              (delete-file "Gemfile")
              (delete-file "Gemfile.lock")))
          (add-after 'delete-gemfiles 'chdir
            (lambda _
              (chdir "activesupport")))
          (add-before 'check 'check-setup
            (lambda* (#:key native-inputs inputs #:allow-other-keys)
              ;; Multiple tests require to set the timezone.
              (setenv "TZDIR" (search-input-directory (or native-inputs inputs)
                                                      "share/zoneinfo"))
              ;; The test suite requires a memcached and a redis server.
              (invoke "memcached" "-d")
              (invoke "redis-server" "--daemonize" "yes")))
          (add-before 'check 'delete-problematic-tests
            (lambda _
              ;; These tests fail non-deterministically.
              (substitute* "test/cache/behaviors.rb"
                ((".*behaviors/cache_store_behavior.*")
                 "")
                ((".*behaviors/encoded_key_cache_behavior.*")
                 ""))
              (delete-file "test/evented_file_update_checker_test.rb")
              ;; These tests require cache_store_behavior, disabled above.
              (delete-file "test/cache/stores/file_store_test.rb")
              (delete-file "test/cache/stores/mem_cache_store_test.rb")
              (delete-file "test/cache/stores/memory_store_test.rb")
              (delete-file "test/cache/stores/redis_cache_store_test.rb"))))))
    (native-inputs
     (list memcached
           redis
           ruby-builder
           ruby-connection-pool
           ruby-dalli
           ruby-hiredis
           ruby-libxml
           ruby-listen
           ruby-rack
           ruby-redis
           ruby-rexml
           tzdata-for-tests))
    (propagated-inputs
     (list ruby-concurrent
           ruby-i18n
           ;; This is sub-optimal, but apparently necessary (see:
           ;; https://github.com/rails/rails/commit/
           ;; 9766eb4a833c26c64012230b96dd1157ebb8e8a2).
           ruby-minitest-5.15
           ruby-tzinfo
           ruby-tzinfo-data))
    (synopsis "Ruby on Rails utility library")
    (description "ActiveSupport is a toolkit of support libraries and Ruby
core extensions extracted from the Rails framework.  It includes support for
multibyte strings, internationalization, time zones, and testing.")
    (home-page "https://rubyonrails.org/")
    (license license:expat)))

(define-public ruby-spring
  (package
    (name "ruby-spring")
    (version "1.7.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/rails/spring")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0smwrndjmnr7g7jjskw05zin3gh6kx5db6yrkiqi6i9wl5mrn9n5"))))
    (build-system ruby-build-system)
    (arguments
     `(#:test-target "test:unit"
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'remove-bump
           (lambda _
             (substitute* "spring.gemspec"
               (("gem.add_development_dependency 'bump'") "")
               (("gem.add_development_dependency 'activesupport'.*")
                "gem.add_development_dependency 'activesupport'\n"))
             (substitute* "Rakefile"
               (("require \\\"bump/tasks\\\"") ""))
             #t)))))
    (native-inputs
     (list bundler ruby-activesupport))
    (synopsis "Ruby on Rails application preloader")
    (description
     "Spring is a Ruby on Rails application preloader.  It speeds up
development by keeping your application running in the background so the
application does need to boot it every time you run a test, rake task or
migration.")
    (home-page "https://github.com/rails/spring")
    (license license:expat)))

(define-public ruby-sass-rails
  (package
    (name "ruby-sass-rails")
    (version "5.0.7")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "sass-rails" version))
       (sha256
        (base32
         "1wa63sbsimrsf7nfm8h0m1wbsllkfxvd7naph5d1j6pbc555ma7s"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ; No included tests
    (propagated-inputs
     (list ruby-railties ruby-sass ruby-sprockets ruby-sprockets-rails
           ruby-tilt))
    (synopsis "Sass adapter for the Rails asset pipeline")
    (description
     "This library integrates the SASS stylesheet language into Ruby on
Rails.")
    (home-page "https://github.com/rails/sass-rails")
    (license license:expat)))

(define-public ruby-debug-inspector
  (package
    (name "ruby-debug-inspector")
    (version "0.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "debug_inspector" version))
       (sha256
        (base32
         "0vxr0xa1mfbkfcrn71n7c4f2dj7la5hvphn904vh20j3x4j5lrx0"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (invoke "rake" "compile")
             (invoke "ruby" "-Ilib" "-e"
                     (string-append
                      "require 'debug_inspector'; RubyVM::DebugInspector."
                      "open{|dc| p dc.backtrace_locations}")))))))
    (synopsis "Ruby wrapper for the MRI 2.0 debug_inspector API")
    (description
     "This package provides a Ruby wrapper for the MRI 2.0 debug_inspector
API.")
    (home-page
     "https://github.com/banister/debug_inspector")
    (license license:expat)))

(define-public ruby-autoprefixer-rails
  (package
    (name "ruby-autoprefixer-rails")
    (version "9.4.7")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "autoprefixer-rails" version))
       (sha256
        (base32
         "0fxbfl3xrrjj84n98x24yzxbz4nvm6c492dxj41kkrl9z97ga13i"))))
    (build-system ruby-build-system)
    (arguments
     '(#:test-target "spec"
       #:phases
       (modify-phases %standard-phases
         (add-after 'extract-gemspec 'remove-unnecessary-dependencies
           (lambda _
             ;; Remove the testing of compass, as its use is deprecated, and
             ;; it's unpackaged for Guix.
             (substitute* "autoprefixer-rails.gemspec"
               ((".*%q<compass>.*") "\n")
               (("\"spec/compass_spec\\.rb\"\\.freeze, ") ""))
             (delete-file "spec/compass_spec.rb")

             (substitute* "Gemfile"
               ;; Remove overly strict requirement on sprockets
               ((", '>= 4\\.0\\.0\\.beta1'") "")
               ;; The mini_racer gem isn't packaged yet, and it's not directly
               ;; required, as other backends for ruby-execjs can be used.
               (("gem 'mini_racer'") "")
               ;; For some reason, this is required for the gems to be picked
               ;; up
               (("gemspec") "gemspec\ngem 'tzinfo-data'\ngem 'sass'"))
             #t)))))
    (native-inputs
     (list bundler
           ruby-rails
           ruby-rspec-rails
           ;; This is needed for a test, but I'm unsure why
           ruby-sass
           ;; This is used as the ruby-execjs runtime
           node))
    (propagated-inputs
     (list ruby-execjs))
    (synopsis "Parse CSS and add vendor prefixes to CSS rules")
    (description
     "This gem provides Ruby and Ruby on Rails integration with Autoprefixer,
which can parse CSS and add vendor prefixes to CSS rules using values from the
Can I Use website.")
    (home-page "https://github.com/ai/autoprefixer-rails")
    (license license:expat)))

(define-public ruby-activemodel
  (package
    (name "ruby-activemodel")
    (version %ruby-rails-version)
    (source ruby-rails-monorepo)
    (build-system ruby-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'extract-gemspec 'chdir
            (lambda _
              (chdir "activemodel")))
          (add-after 'chdir 'delete-problematic-tests
            (lambda _
              ;; We do not want to depend on ruby-railties at this stage.
              (delete-file "test/cases/railtie_test.rb"))))))
    (native-inputs (list ruby-bcrypt))
    (propagated-inputs (list ruby-activesupport))
    (synopsis "Toolkit for building modeling frameworks like Active Record")
    (description
     "This package provides a toolkit for building modeling frameworks like
Active Record.  ActiveSupport handles attributes, callbacks, validations,
serialization, internationalization, and testing.")
    (home-page "https://rubyonrails.org/")
    (license license:expat)))

(define-public ruby-activerecord
  (package
    (name "ruby-activerecord")
    (version %ruby-rails-version)
    (source ruby-rails-monorepo)
    (build-system ruby-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'extract-gemspec 'chdir
            (lambda _
              (chdir "activerecord")))
          (delete 'check)
          (add-after 'install 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                ;; Avoid running the database tests, which require railties
                ;; and/or database servers.
                (invoke "ruby" "-Itest" "test/cases/base_test.rb"))))
          (add-before 'check 'set-GEM_PATH
            (lambda _
              (setenv "GEM_PATH" (string-append
                                  (getenv "GEM_PATH") ":"
                                  #$output "/lib/ruby/vendor_ruby"))))
          (add-before 'check 'check-setup
            (lambda* (#:key native-inputs inputs #:allow-other-keys)
              ;; A few tests require to set the timezone.
              (setenv "TZDIR" (search-input-directory (or native-inputs inputs)
                                                      "share/zoneinfo")))))))
    (native-inputs (list tzdata-for-tests))
    (propagated-inputs (list ruby-activemodel ruby-activesupport ruby-sqlite3))
    (synopsis "Ruby library to connect to relational databases")
    (description
     "Active Record connects classes to relational database table to establish
an almost zero-configuration persistence layer for applications.")
    (home-page "https://rubyonrails.org")
    (license license:expat)))

(define-public ruby-rspec-rails
  (package
    (name "ruby-rspec-rails")
    (version "3.8.2")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "rspec-rails" version))
       (sha256
        (base32
         "1pf6n9l4sw1arlax1bdbm1znsvl8cgna2n6k6yk1bi8vz2n73ls1"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ; No included tests
    (propagated-inputs
     (list ruby-actionpack
           ruby-activesupport
           ruby-railties
           ruby-rspec-core
           ruby-rspec-expectations
           ruby-rspec-mocks
           ruby-rspec-support))
    (synopsis "Use RSpec to test Ruby on Rails applications")
    (description
     "This package provides support for using RSpec to test Ruby on Rails
applications, in pace of the default Minitest testing library.")
    (home-page "https://github.com/rspec/rspec-rails")
    (license license:expat)))

(define-public ruby-rails-html-sanitizer
  (package
    (name "ruby-rails-html-sanitizer")
    (version "1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "rails-html-sanitizer" version))
       (sha256
        (base32
         "1icpqmxbppl4ynzmn6dx7wdil5hhq6fz707m9ya6d86c7ys8sd4f"))))
    (build-system ruby-build-system)
    (arguments
     '(;; No included tests
       #:tests? #f))
    (propagated-inputs
     (list ruby-loofah))
    (synopsis "HTML sanitization for Rails applications")
    (description
     "This gem is used to handle HTML sanitization in Rails applications.  If
you need similar functionality in non Rails apps consider using Loofah
directly.")
    (home-page "https://github.com/rails/rails-html-sanitizer")
    (license license:expat)))

(define-public ruby-rails-dom-testing
  (package
   (name "ruby-rails-dom-testing")
   (version "2.0.3")
   (source
    (origin
      (method git-fetch)
      (uri (git-reference
            (url "https://github.com/rails/rails-dom-testing")
            (commit (string-append "v" version))))
      (file-name (git-file-name name version))
      (sha256
       (base32
        "17vdh273cmmfpzy5m546dd13zqmimv54jjx0f7sl0zi5lwz0gnck"))))
   (build-system ruby-build-system)
   (arguments
    (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'delete-gemfile.lock
            (lambda _
              (delete-file "Gemfile.lock"))))))
   (native-inputs
    (list bundler))
   (propagated-inputs
    (list ruby-activesupport ruby-nokogiri))
   (synopsis "Compare HTML DOMs and assert certain elements exists")
   (description
    "This gem can compare HTML and assert certain elements exists.  This is
useful when writing tests.")
   (home-page "https://github.com/rails/rails-dom-testing")
   (license license:expat)))

(define-public ruby-actiontext
  (package
    (name "ruby-actiontext")
    (version "6.1.3")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "actiontext" version))
        (sha256
         (base32
          "04k4z4xj40sbzbgx0x9m6i8k0nc22jb6dkrlslj16p2z2dfnwhqg"))))
    (build-system ruby-build-system)
   (arguments
    '(;; No included tests
      #:tests? #f))
    (propagated-inputs
     (list ruby-actionpack ruby-activerecord ruby-activestorage
           ruby-activesupport ruby-nokogiri))
    (synopsis "Edit and display rich text in Rails applications")
    (description
     "ActionText edits and displays rich text in Rails applications.")
    (home-page "https://rubyonrails.org")
    (license license:expat)))

(define-public ruby-actionview
  (package
   (name "ruby-actionview")
   (version "6.1.3")
   (source
    (origin
     (method url-fetch)
     (uri (rubygems-uri "actionview" version))
     (sha256
      (base32
       "1s5kc1abi7id1g54lz1npgc42zl7pbz172wp8pi7j3s7qljafzw5"))))
   (build-system ruby-build-system)
   (arguments
    '(;; No included tests
      #:tests? #f))
   (propagated-inputs
    (list ruby-activesupport ruby-builder ruby-erubi
          ruby-rails-dom-testing ruby-rails-html-sanitizer))
   (synopsis "Conventions and helpers for building web pages")
   (description
    "ActionView provides conventions and helpers for building web pages in
Ruby.")
   (home-page "https://rubyonrails.org/")
   (license license:expat)))

(define-public ruby-actionpack
  (package
   (name "ruby-actionpack")
   (version "6.1.3")
   (source
    (origin
     (method url-fetch)
     (uri (rubygems-uri "actionpack" version))
     (sha256
      (base32
       "030yyaskzlic5cp4d9zbwwr3rhf4k6hsls44a7ihsfd6r8mlivq5"))))
   (build-system ruby-build-system)
   (arguments
    '(;; No included tests
      #:tests? #f))
   (propagated-inputs
    (list ruby-actionview
          ruby-activesupport
          ruby-rack
          ruby-rack-test
          ruby-rails-dom-testing
          ruby-rails-html-sanitizer))
   (synopsis "Conventions for building and testing MVC web applications")
   (description
    "ActionPack provides conventions for building and testing MVC web
applications.  These work with any Rack-compatible server.")
   (home-page "https://rubyonrails.org/")
   (license license:expat)))

(define-public ruby-actioncable
  (package
   (name "ruby-actioncable")
   (version "6.1.3")
   (source
    (origin
     (method url-fetch)
     (uri (rubygems-uri "actioncable" version))
     (sha256
      (base32
       "1cgb1l0gml1vklxka2djpi5q5b4bgzgm5pahzfjvvgm5vzvrvi9v"))))
   (build-system ruby-build-system)
   (arguments
    '(;; No included tests
      #:tests? #f))
   (propagated-inputs
    (list ruby-actionpack ruby-activesupport ruby-nio4r
          ruby-websocket-driver))
   (synopsis "Integrate integrates WebSockets with Rails applications")
   (description
    "Action Cable integrates WebSockets with Rails applications.  Through
WebSockets it allows for real-time features in web applications.")
   (home-page "https://rubyonrails.org/")
   (license license:expat)))

(define-public ruby-activejob
  (package
   (name "ruby-activejob")
   (version "6.1.3")
   (source
    (origin
     (method url-fetch)
     (uri (rubygems-uri "activejob" version))
     (sha256
      (base32
       "175d8q0achdlsxjsvq0w9znvfqfkgbj75kbmdrvg4fb277wwplmf"))))
   (build-system ruby-build-system)
   (arguments
    '(;; No included tests
      #:tests? #f))
   (propagated-inputs
    (list ruby-activesupport ruby-globalid))
   (synopsis "Declare job classes for multiple backends")
   (description
    "ActiveJob allows declaring job classes in a common way across Rails
applications.")
   (home-page "https://rubyonrails.org/")
   (license license:expat)))

(define-public ruby-activestorage
  (package
    (name "ruby-activestorage")
    (version "6.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "activestorage" version))
       (sha256
        (base32
         "0gkxvbi5w8zmdxpiyz3b10kzz8cxqqh9bj81sjl3fp8wa3v2ld4i"))))
    (build-system ruby-build-system)
    (arguments
     '(;; No included tests
       #:tests? #f))
    (propagated-inputs
     (list ruby-actionpack
           ruby-activejob
           ruby-activerecord
           ruby-activesupport
           ruby-marcel
           ruby-mimemagic))
    (synopsis "Integrate file storage services in to Rails applications")
    (description
     "ActiveStorage integrates file storage services with Rails applications,
allowing files to be attached to ActiveRecord models.")
    (home-page "https://rubyonrails.org/")
    (license license:expat)))

(define-public ruby-actionmailbox
  (package
    (name "ruby-actionmailbox")
    (version "6.1.3")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "actionmailbox" version))
        (sha256
         (base32
         "0wv2p24xn4f0kj8kiyagkn934hzrcp98vzjqxwd4r75qq0cijadp"))))
    (build-system ruby-build-system)
   (arguments
    '(;; No included tests
      #:tests? #f))
    (propagated-inputs
     (list ruby-actionpack
           ruby-activejob
           ruby-activerecord
           ruby-activestorage
           ruby-activesupport
           ruby-mail))
    (synopsis "Receive and process incoming emails in Rails applications")
    (description
     "ActionMailbox receives and processes incoming emails in Rails applications.")
    (home-page "https://rubyonrails.org")
    (license license:expat)))

(define-public ruby-actionmailer
  (package
   (name "ruby-actionmailer")
   (version "6.1.3")
   (source
    (origin
     (method url-fetch)
     (uri (rubygems-uri "actionmailer" version))
     (sha256
      (base32
       "0lic4mc6wqi3p9ipdqljl64vd9ndabm0k8hww0m07sfdhwsl5ba9"))))
   (build-system ruby-build-system)
   (arguments
    '(;; No included tests
      #:tests? #f))
   (propagated-inputs
    (list ruby-actionpack
          ruby-actionview
          ruby-activejob
          ruby-activesupport
          ruby-mail
          ruby-rails-dom-testing))
   (synopsis "Work with emails using the controller/view pattern")
   (description
    "Compose, deliver, receive, and test emails using the controller/view
pattern.  Including support for multipart email and attachments.")
   (home-page "https://rubyonrails.org/")
   (license license:expat)))

(define-public ruby-railties
  (package
    (name "ruby-railties")
    (version %ruby-rails-version)
    (source ruby-rails-monorepo)
    (build-system ruby-build-system)
    (arguments
     (list #:tests? #f                  ;requires rails to be installed
           #:phases #~(modify-phases %standard-phases
                        (add-after 'delete-gemfiles 'chdir
                          (lambda _
                            (chdir "railties"))))))
    (propagated-inputs (list ruby-actionpack
                             ruby-activesupport
                             ruby-method-source
                             ruby-rake
                             ruby-thor
                             ruby-zeitwerk))
    (synopsis "Rails internals, including application bootup and generators")
    (description "@code{railties} provides the core Rails internals including
handling application bootup, plugins, generators, and Rake tasks.")
    (home-page "https://rubyonrails.org")
    (license license:expat)))

(define-public ruby-sprockets-rails
  (package
   (name "ruby-sprockets-rails")
   (version "3.2.1")
   (source
    (origin
     (method url-fetch)
     (uri (rubygems-uri "sprockets-rails" version))
     (sha256
      (base32
       "0ab42pm8p5zxpv3sfraq45b9lj39cz9mrpdirm30vywzrwwkm5p1"))))
   (build-system ruby-build-system)
   (arguments
    '(;; No included tests
      #:tests? #f))
   (propagated-inputs
    (list ruby-actionpack ruby-activesupport ruby-sprockets))
   (synopsis "Sprockets Rails integration")
   (description
    "Provides Sprockets implementation for the Rails Asset Pipeline.")
   (home-page
    "https://github.com/rails/sprockets-rails")
   (license license:expat)))

(define-public ruby-web-console
  (package
    (name "ruby-web-console")
    (version "4.1.0")
    (source
     (origin
       ;; Download from GitHub as test files are not provided in the gem.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rails/web-console")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0azk8nmimnjbh74vxgwcj9jr588rj7kb5rrlclcjfjsw9jqjzckc"))))
    (build-system ruby-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-Gemfile
           (lambda _
             (substitute* "Gemfile"
               ;; Remove the github bit from the Gemfile, so that the Guix
               ;; packages are used.
               ((", github: .*") "\n")
               ;; The usual methods of not loading this group don't work, so
               ;; patch the Gemfile.
               (("group :development") "[].each")
               ;; tzinfo-data is propagated by ruby-activesupport, but it
               ;; needs to be in the Gemfile to become available.
               (("group :test do") "group :test do\n  gem 'tzinfo-data'"))
             #t)))))
    (propagated-inputs
     (list ruby-actionview ruby-activemodel ruby-bindex ruby-railties))
    (native-inputs
     (list bundler ruby-rails ruby-mocha ruby-simplecov))
    (synopsis "Debugging tool for your Ruby on Rails applications")
    (description
     "This package allows you to create an interactive Ruby session in your
browser.  Those sessions are launched automatically in case of an error and
can also be launched manually in any page.")
    (home-page "https://github.com/rails/web-console")
    (license license:expat)))

(define-public ruby-with-advisory-lock
  (package
    (name "ruby-with-advisory-lock")
    (version "4.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "with_advisory_lock" version))
       (sha256
        (base32
         "1k37hxgmaqgsd54gplm5xim9nw3ghvqsbzaw7q4q64ha1nbd9a41"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ; TODO Tests require a running MySQL service
    (propagated-inputs
     (list ruby-activerecord))
    (native-inputs
     (list bundler ruby-yard ruby-mysql2))
    (synopsis "Advisory locking for ActiveRecord")
    (description
     "The With advisory lock gem adds advisory locking to ActiveRecord for
PostgreSQL and MySQL.  SQLite is also supported, but this uses the file system
for locks.")
    (home-page "https://closuretree.github.io/with_advisory_lock/")
    (license license:expat)))

(define-public ruby-rails
  (package
   (name "ruby-rails")
   (version "6.1.3")
   (source
    (origin
     (method url-fetch)
     (uri (rubygems-uri "rails" version))
     (sha256
      (base32
       "0hdancysa617lzyy5gmrcmnpgyb1mz1lawy0l34ycz2wary7y2bz"))))
   (build-system ruby-build-system)
   (arguments
    '(#:phases
      (modify-phases %standard-phases
        ;; This gem acts as glue between the gems that actually make up
        ;; Rails. The important thing to check is that the gemspec matches up
        ;; with the Guix packages and Rubygems can successfully activate the
        ;; Rails gem.
        ;;
        ;; The following check phase tests this.
        (delete 'check)
        (add-after 'install 'check
          (lambda* (#:key tests? outputs #:allow-other-keys)
            (setenv "GEM_PATH"
                    (string-append
                     (getenv "GEM_PATH")
                     ":"
                     (assoc-ref outputs "out") "/lib/ruby/vendor_ruby"))
            (when tests?
              (invoke "ruby" "-e" "gem 'rails'"))
            #t)))))
   (propagated-inputs
    (list ruby-actioncable
          ruby-actionmailbox
          ruby-actionmailer
          ruby-actionpack
          ruby-actiontext
          ruby-actionview
          ruby-activejob
          ruby-activemodel
          ruby-activerecord
          ruby-activestorage
          ruby-activesupport
          bundler
          ruby-railties
          ruby-sprockets-rails))
   (synopsis "Full-stack web framework optimized for programmer happiness")
   (description
    "Ruby on Rails is a full-stack web framework optimized for programmer
happiness and sustainable productivity.  It encourages beautiful code by
favoring convention over configuration.")
   (home-page "https://rubyonrails.org/")
   (license license:expat)))
