;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Matthew Jordan <matthewjordandevops@yandex.com>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019, 2021, 2022 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2019 Christopher Baines <mail@cbaines.net>
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
  #:use-module (guix utils)
  #:use-module (gnu packages base)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages node)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages version-control)
  #:use-module (guix build-system ruby))

(define %ruby-rails-version "7.0.5.1")

(define ruby-rails-monorepo
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/rails/rails")
          (commit (string-append "v" %ruby-rails-version))))
    (file-name (git-file-name "ruby-rails" %ruby-rails-version))
    (sha256
     (base32
      "0s16i73rqzlrx5icn848mf2nmblmgxk06wj9576dkadsb8pspv0l"))))

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
              (when #$(target-x86-32?)
                ;; This test fails on i686 (see:
                ;; https://github.com/rails/rails/issues/47832).
                (substitute* "test/core_ext/duration_test.rb"
                  (("def test_iso8601_output_and_reparsing.*" all)
                   (string-append all "    skip('fails on i686')\n"))))
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
           ruby-minitest
           ruby-tzinfo
           ruby-tzinfo-data))
    (synopsis "Ruby on Rails utility library")
    (description "ActiveSupport is a toolkit of support libraries and Ruby
core extensions extracted from the Rails framework.  It includes support for
multibyte strings, internationalization, time zones, and testing.")
    (home-page "https://rubyonrails.org/")
    (license license:expat)))

(define-public ruby-globalid
  (package
    (name "ruby-globalid")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "globalid" version))
       (sha256
        (base32
         "0kqm5ndzaybpnpxqiqkc41k4ksyxl41ln8qqr6kb130cdxsf2dxk"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f))                    ;no included tests
    (propagated-inputs
     (list ruby-activesupport))
    (synopsis "Generate URIs idenfitying model instances in Ruby")
    (description
     "@code{GlobalID} provides a way to generate URIs from a model in Ruby that
uniquely identify it.")
    (home-page "https://rubyonrails.org/")
    (license license:expat)))

(define-public ruby-spring
  (package
    (name "ruby-spring")
    (version "4.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rails/spring")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0p8hidxqnk8s1gfm1s1xb06gbbahdxjmzy6x3ybi25nkmdp0anb6"))))
    (build-system ruby-build-system)
    (arguments
     (list #:test-target "test:unit"
           #:phases
           #~(modify-phases %standard-phases
               (add-before 'check 'remove-bump
                 (lambda _
                   (substitute* "spring.gemspec"
                     (("gem.add_development_dependency 'bump'") ""))
                   (substitute* "Rakefile"
                     (("require \\\"bump/tasks\\\"") "")))))))
    (native-inputs (list bundler ruby-activesupport))
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
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "debug_inspector" version))
       (sha256
        (base32
         "01l678ng12rby6660pmwagmyg8nccvjfgs3487xna7ay378a59ga"))))
    (build-system ruby-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda _
              (invoke "rake" "compile")
              (invoke "ruby" "-Ilib" "-e"
                      (string-append
                       "require 'debug_inspector'; RubyVM::DebugInspector."
                       "open{|dc| p dc.backtrace_locations}")))))))
    (native-inputs
     (list ruby-rake-compiler))
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
    (version "10.4.13.0")
    (source (origin
              (method git-fetch)        ;for tests
              (uri (git-reference
                    (url "https://github.com/ai/autoprefixer-rails")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1i34apjlav1qz8mdg2fyf0hvs5z32inv1snycdkhmqpkfj2ri2hh"))))
    (build-system ruby-build-system)
    (arguments
     '(#:test-target "spec"
       #:phases
       (modify-phases %standard-phases
         (add-after 'extract-gemspec 'relax-requirements
           (lambda _
             (substitute* "Gemfile"
               ;; Remove overly strict requirement on sprockets
               ((", '>= 4\\.0\\.0\\.beta1'") "")
               ;; The mini_racer gem isn't packaged yet, and it's not directly
               ;; required, as other backends for ruby-execjs can be used.
               (("gem \"mini_racer\"") "")
               ;; For some reason, this is required for the tzinfo-data gem to
               ;; be picked up.
               (("gemspec") "gemspec\ngem 'tzinfo-data'\n"))
             (substitute* "autoprefixer-rails.gemspec"
               ((".*rubocop.*") ""))))))) ;provided by 'standard'
    (native-inputs
     (list bundler
           ruby-rails
           ruby-rspec-rails
           ruby-sassc-rails
           ruby-sprockets
           ruby-standard
           ;; This is used at runtime by ruby-execjs.
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
    (native-inputs
     (list tzdata-for-tests
           ruby-mini-portile-2))
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
    (version "6.0.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/rspec/rspec-rails")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0wmrpwv2vgrwmby01pld6r6sdfa265lb6pd3fp2kifs40nn7ff6b"))))
    (build-system ruby-build-system)
    (arguments
     (list
      ;; Run the 'spec' instead of the 'default' Rake target to avoid running
      ;; the acceptance test suite, which doesn't seem to allow being run
      ;; offline (see: https://github.com/rspec/rspec-rails/issues/2660).
      #:test-target "spec"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'extract-gemspec 'relax-requirements
            (lambda _
              (substitute* "Gemfile"
                ;; Remove a few extraneous requirements.
                ((".*yard.*") "")
                ((".*github-markup.*") "")
                ((".*redcarpet.*") "")
                ((".*relish.*") "")
                ((".*rubocop.*") ""))
              (substitute* "Gemfile-rspec-dependencies"
                ((", :git => \"https://github.com/rspec.*")
                 "\n"))
              (substitute* "Gemfile-rails-dependencies"
                (("gem 'puma', '< 6.0.0'")
                 "gem 'puma', '>= 6.0.0'"))
              (substitute* "rspec-rails.gemspec"
                (("'aruba',    '~> 0.14.12'")
                 "'aruba',    '>= 0.14.12'")
                (("'cucumber', '~> 7.0'")
                 "'cucumber', '>= 7.0'"))))
          (replace 'replace-git-ls-files
            (lambda _
              (substitute* "rspec-rails.gemspec"
                (("`git ls-files -- lib/\\*`")
                 "`find lib -type f |sort`"))))
          (add-before 'check 'patch-tests
            (lambda _
              (substitute* "spec/rspec/rails_spec.rb"
                (("`git ls-files -z`")
                 "`find . -type f -not -regex '.*\\.gem$' -print0 | \
sort -z | cut -zc3-`")))))))
    (native-inputs
     (list ruby-ammeter-bootstrap
           ruby-aruba
           ruby-capybara
           ruby-cucumber
           ruby-puma
           ruby-rails
           ruby-rspec
           ruby-selenium-webdriver
           ruby-sqlite3))
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
    (version %ruby-rails-version)
    (source ruby-rails-monorepo)
    (build-system ruby-build-system)
    (arguments
     (list
      #:tests? #f                       ;avoid a cycle with ruby-rails
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'extract-gemspec 'chdir
            (lambda _
              (chdir "actiontext"))))))
    (propagated-inputs
     (list ruby-actionpack
           ruby-activerecord
           ruby-activestorage
           ruby-activesupport
           ruby-nokogiri))
    (synopsis "Edit and display rich text in Rails applications")
    (description
     "ActionText edits and displays rich text in Rails applications.")
    (home-page "https://rubyonrails.org")
    (license license:expat)))

(define-public ruby-actionview
  (package
   (name "ruby-actionview")
    (version %ruby-rails-version)
    (source ruby-rails-monorepo)
    (build-system ruby-build-system)
    (arguments
     (list
      ;; XXX: This gem appears to load action_controller, provided by
      ;; ruby-actionpack, but actionpack propagates ruby-actionview,
      ;; introducing a circular dependency.
      #:tests? #f
      #:test-target "test:template"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'extract-gemspec 'chdir
            (lambda _
              (chdir "actionview"))))))
   (propagated-inputs
    (list ruby-activesupport
          ruby-builder
          ruby-erubi
          ruby-rails-dom-testing
          ruby-rails-html-sanitizer))
   (synopsis "Conventions and helpers for building web pages")
   (description
    "ActionView provides conventions and helpers for building web pages in
Ruby.")
   (home-page "https://rubyonrails.org/")
   (license license:expat)))

(define-public ruby-actionpack
  (package
    (name "ruby-actionpack")
    (version %ruby-rails-version)
    (source ruby-rails-monorepo)
    (build-system ruby-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'extract-gemspec 'chdir
            (lambda _
              (chdir "actionpack")))
          (add-before 'check 'delete-problematic-tests
            (lambda _
              (let-syntax ((skip-tests
                            (syntax-rules ()
                              ((_ file test ...)
                               (substitute* file
                                 ;; ActiveSupport test case.
                                 (((string-append "test \"" test "\".*") all)
                                  (string-append
                                   all "    skip    'fails on guix'\n")) ...
                                 ;; MiniTest test case.
                                 (((string-append "def " test ".*") all)
                                  (string-append
                                   all "    skip('fails on guix')\n")) ...)))))

                (with-directory-excursion "test"
                  (for-each delete-file
                            ;; These tests depend on rails, which depends on
                            ;; this package.
                            '("dispatch/mount_test.rb"
                              "dispatch/prefix_generation_test.rb"
                              "dispatch/routing_assertions_test.rb"
                              "dispatch/routing/inspector_test.rb"
                              "controller/live_stream_test.rb"
                              "controller/integration_test.rb"
                              "controller/test_case_test.rb"))

                  ;; The following test failures have been reported upstream
                  ;; (see: https://github.com/rails/rails/issues/47615).
                  (skip-tests "controller/new_base/render_streaming_test.rb"
                              ;; These tests fail due to white space
                              ;; characters in the compared strings.
                              "rendering with streaming no layout"
                              "rendering with streaming enabled at the \
class level"
                              "rendering with streaming given to render"
                              "rendering with layout exception"
                              "rendering with template exception"
                              "rendering with streaming do not override \
explicit cache control given to render")

                  (skip-tests "dispatch/system_testing/driver_test.rb"
                              ;; These tests require Firefox.
                              "define extra capabilities using headless_firefox"
                              "define extra capabilities using firefox")

                  (skip-tests "dispatch/session/cache_store_test.rb"
                              ;; This test fails with: "NoMethodError:
                              ;; undefined method `hash_for' for
                              ;; #<Rack::Test::CookieJar:0x0000000003572170>".
                              "test_getting_session_value_after_session_reset"))))))))
    (native-inputs
     (list ruby-activemodel
           ruby-capybara
           ruby-selenium-webdriver
           ruby-zeitwerk))
    (propagated-inputs
     (list ruby-actionview
           ruby-activesupport
           ruby-rack
           ruby-rack-cache
           ruby-rack-session
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
    (version %ruby-rails-version)
    (source ruby-rails-monorepo)
    (build-system ruby-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'extract-gemspec 'chdir
            (lambda _
              (chdir "actioncable")))
          (delete 'check)               ;moved after install phase
          (add-after 'install 'check
            (assoc-ref %standard-phases 'check))
          (add-before 'check 'set-GEM_PATH
            (lambda _
              (setenv "GEM_PATH" (string-append
                                  (getenv "GEM_PATH") ":"
                                  #$output "/lib/ruby/vendor_ruby"))))
          (add-before 'check 'disable-problematic-tests
            (lambda _
              ;; There are multiple client test failures (see:
              ;; https://github.com/rails/rails/issues/47617).
              (delete-file "test/client_test.rb")))
          (add-before 'check 'start-redis
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "redis-server" "--daemonize" "yes")))))))
    (native-inputs
     (list redis
           ruby-activerecord
           ruby-pg
           ruby-puma
           ruby-redis
           ruby-websocket-client-simple))
    (propagated-inputs
     (list ruby-actionpack
           ruby-activesupport
           ruby-nio4r
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
    (version %ruby-rails-version)
    (source ruby-rails-monorepo)
    (build-system ruby-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'extract-gemspec 'chdir
            (lambda _
              (chdir "activejob")))
          (add-after 'chdir 'delete-problematic-tests
            (lambda _
              (substitute* "Rakefile"
                ;; Remove the adapters that aren't yet packaged or would
                ;; introduce cyclic dependencies.
                (("backburner ") "")
                (("resque ") "")
                (("sidekiq ") "")
                (("sneakers ") "")
                (("sucker_punch ") ""))
              (substitute* "test/cases/exceptions_test.rb"
                (("ActiveJob::QueueAdapters::SneakersAdapter") "")))))))
    (native-inputs
     (list ruby-queue-classic
           ruby-delayed-job
           ruby-que
           ruby-zeitwerk))
    (propagated-inputs
     (list ruby-activesupport
           ruby-globalid))
    (synopsis "Declare job classes for multiple backends")
    (description "ActiveJob allows declaring job classes in a common way
across Rails applications.")
    (home-page "https://rubyonrails.org/")
    (license license:expat)))

(define-public ruby-activestorage
  (package
    (name "ruby-activestorage")
    (version %ruby-rails-version)
    (source ruby-rails-monorepo)
    (build-system ruby-build-system)
    (arguments
     (list
      ;; The test suite is disabled, because it activestorage requires
      ;; 'rails', which would introduce a dependency cycle.
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'extract-gemspec 'chdir
            (lambda _
              (chdir "activestorage"))))))
    (propagated-inputs
     (list ruby-actionpack
           ruby-activejob
           ruby-activerecord
           ruby-activesupport
           ruby-marcel
           ruby-mini-mime))
    (synopsis "Integrate file storage services in to Rails applications")
    (description
     "ActiveStorage integrates file storage services with Rails applications,
allowing files to be attached to ActiveRecord models.")
    (home-page "https://rubyonrails.org/")
    (license license:expat)))

(define-public ruby-actionmailbox
  (package
    (name "ruby-actionmailbox")
    (version %ruby-rails-version)
    (source ruby-rails-monorepo)
    (build-system ruby-build-system)
    (arguments
     (list
      #:tests? #f                       ;avoid a cycle with ruby-rails
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'extract-gemspec 'chdir
            (lambda _
              (chdir "actionmailbox"))))))
    (propagated-inputs
     (list ruby-actionpack
           ruby-activejob
           ruby-activerecord
           ruby-activestorage
           ruby-activesupport
           ruby-mail
           ruby-net-imap
           ruby-net-pop
           ruby-net-smtp))
    (synopsis "Receive and process incoming emails in Rails applications")
    (description
     "ActionMailbox receives and processes incoming emails in Rails applications.")
    (home-page "https://rubyonrails.org")
    (license license:expat)))

(define-public ruby-actionmailer
  (package
    (name "ruby-actionmailer")
    (version %ruby-rails-version)
    (source ruby-rails-monorepo)
    (build-system ruby-build-system)
    (arguments
     (list
      #:tests? #f                       ;avoid a cycle with ruby-rails
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'extract-gemspec 'chdir
            (lambda _
              (chdir "actionmailer"))))))
    (propagated-inputs
     (list ruby-actionpack
           ruby-actionview
           ruby-activejob
           ruby-activesupport
           ruby-mail
           ruby-net-imap
           ruby-net-pop
           ruby-net-smtp
           ruby-rails-dom-testing))
    (synopsis "Work with emails using the controller/view pattern")
    (description
     "Compose, deliver, receive, and test emails using the controller/view
pattern.  Including support for multipart email and attachments.")
    (home-page "https://rubyonrails.org/")
    (license license:expat)))

;; A variant where the ruby-rspec-rails dependency purposefully omitted to
;; avoid a dependency cycle with that same package.
(define ruby-ammeter-bootstrap
  (package
    (name "ruby-ammeter-bootstrap")
    (version "1.1.5")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "ammeter" version))
              (sha256
               (base32
                "1bcslj6y3lgaknd9fpj32m1r4is7blyxygxzmwidq9cjwkrn4msh"))))
    (build-system ruby-build-system)
    (arguments
     (list #:tests? #f
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'extract-gemspec 'remove-rails-requirement
                 (lambda _
                   (substitute* "Gemfile"
                     (("gem 'rspec-rails', rspec_version")
                      "")
                     (("gem 'rails', rails_version")
                      "")))))))
    (propagated-inputs (list ruby-activesupport ruby-railties))
    (synopsis "Write specs for your Rails 3+ generators")
    (description "The @code{ammeter} gem makes it easy to write specs for
Rails generators.  An existing user is @code{rspec-rails}, which uses
@code{ammeter} to spec its own generators.")
    (home-page "https://github.com/alexrothenberg/ammeter")
    (license license:expat)))

(define-public ruby-ammeter
  (package/inherit ruby-ammeter-bootstrap
    (name "ruby-ammeter")
    ;; TODO: The test suite requires multiple packages which are not packaged
    ;; yet.
    (arguments (list #:tests? #f))
    (propagated-inputs
     (modify-inputs (package-propagated-inputs ruby-ammeter-bootstrap)
       (append ruby-rspec-rails)))))

(define-public ruby-bootsnap
  (package
    (name "ruby-bootsnap")
    (version "1.16.0")
    (source (origin
              (method git-fetch)        ;for tests
              (uri (git-reference
                    (url "https://github.com/Shopify/bootsnap")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1gaih5v4jjndrkn0crrr5mxnwc3cd0f3i955n62ghk29zabvd7wf"))))
    (build-system ruby-build-system)
    (arguments
     (list
      #:test-target "default"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'extract-gemspec 'relax-requirements
            (lambda _
              (substitute* "Gemfile"
                ;; Rubocop and byebug are not actually needed to run the
                ;; tests.
                ((".*rubocop.*") "")
                ((".*byebug.*") ""))))
          (replace 'replace-git-ls-files
            (lambda _
              (substitute* "bootsnap.gemspec"
                (("`git ls-files -z ext lib`")
                 "`find ext lib -type f -print0 | sort -z`")))))))
    (native-inputs (list ruby-mocha-1 ruby-rake-compiler))
    (propagated-inputs (list ruby-msgpack))
    (synopsis "Accelerator for large Ruby/Rails application")
    (description "Bootsnap is a library that plugs into Ruby, with optional
support for YAML, to optimize and cache expensive computations.")
    (home-page "https://github.com/Shopify/bootsnap")
    (license license:expat)))

;;; A private variant used to bootstrap railties.
(define ruby-importmap-rails-bootstrap
  (package
    (name "ruby-importmap-rails-bootstrap")
    (version "1.1.5")
    (source (origin
              (method git-fetch)        ;for tests
              (uri (git-reference
                    (url "https://github.com/rails/importmap-rails")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1d8pqqqrvsnm8rpr7qkpcxpscif61xymi509v1c62laadvhcmklg"))))
    (build-system ruby-build-system)
    (arguments (list #:tests? #f))             ;avoid all extra dependencies
    ;; Leave out ruby-railties, for bootstrapping purposes.
    (propagated-inputs (list ruby-actionpack))
    (synopsis "Tool to manage modern JavaScript in Rails")
    (description "Import maps can import JavaScript modules using logical
names that map to versioned/digested files -- directly from the browser.  It
makes it possible to build modern JavaScript applications using JavaScript
libraries made for ES modules (ESM) without the need for transpiling or
bundling, which removes the need for Webpack, Yarn, npm, or any other part of
the JavaScript toolchain.  All that is needed is the asset pipeline that is
already included in Rails.")
    (home-page "https://github.com/rails/importmap-rails")
    (license license:expat)))

(define-public ruby-importmap-rails
  (package/inherit ruby-importmap-rails-bootstrap
    (name "ruby-importmap-rails")
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-after 'extract-gemspec 'relax-requirements
                 (lambda _
                   (delete-file "gemfiles/rails_7_propshaft.gemfile.lock")
                   (substitute* "gemfiles/rails_7_propshaft.gemfile"
                     ((".*gem \"byebug\".*") "")
                     ;; Remove appraisal, and add tzinfo-data, which needs to
                     ;; be in the Gemfile to become available.
                     ((".*appraisal.*")
                      "gem 'tzinfo-data'\n")
                     ;; This gem is for managing *installation* of
                     ;; webdrivers... we do not want that.
                     ((".*gem \"webdrivers\".*") ""))))
               (add-before 'check 'set-BUNDLE_GEMFILE
                 (lambda _
                   ;; The default Gemfile is for Rails 6.
                   (setenv "BUNDLE_GEMFILE"
                           "gemfiles/rails_7_propshaft.gemfile")))
               (add-before 'check 'disable-problematic-tests
                 (lambda _
                   ;; The integration tests require networking; disable them.
                   (delete-file "test/npm_integration_test.rb")
                   (delete-file "test/packager_integration_test.rb"))))))
    (native-inputs
     (list ruby-capybara
           ruby-propshaft
           ruby-rails
           ruby-rexml
           ruby-selenium-webdriver
           ruby-sqlite3
           ruby-stimulus-rails
           ruby-turbo-rails
           ruby-tzinfo
           ruby-tzinfo-data))
    (propagated-inputs
     (list ruby-actionpack
           ruby-railties))))

(define-public ruby-marcel
  (package
    (name "ruby-marcel")
    (version "1.0.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/rails/marcel")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1i1x24afmn09n48fj4yz2pdm6vlfnq14gism0cgxsyqmlrvsxajn"))))
    (build-system ruby-build-system)
    (arguments
     (list
      #:test-target "default"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch
            (lambda _
              ;; Remove byebug dependency
              (substitute* "test/test_helper.rb"
                (("require 'byebug'") ""))))
          (add-before 'check 'disable-problematic-tests
            (lambda _
              (substitute* "test/mime_type_test.rb"
                ;; One test fails because of the newer rack
                ;; version used (see:
                ;; https://github.com/rails/marcel/issues/91).
                (("test \"gets content type.*" all)
                 (string-append
                  all "    skip('fails on guix')\n"))))))))
    (native-inputs (list ruby-nokogiri ruby-rack))
    (propagated-inputs (list ruby-mimemagic))
    (synopsis "MIME type detection using magic numbers, filenames and extensions")
    (description
     "@code{marcel} provides @acronym{MIME, Multipurpose Internet Mail
Extensions} type detection using magic numbers, filenames, and extensions")
    (home-page "https://github.com/rails/marcel")
    (license license:expat)))

(define-public ruby-propshaft
  (package
    (name "ruby-propshaft")
    (version "0.7.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "propshaft" version))
              (sha256
               (base32
                "19s5qvfady49b9b6dcvz6nsna1lvckw509ddh3ihmdz0w4qrjy49"))))
    (build-system ruby-build-system)
    (propagated-inputs (list ruby-actionpack ruby-activesupport ruby-rack
                             ruby-railties))
    (synopsis "Asset pipeline library for Rails")
    (description "Propshaft is an asset pipeline library for Rails.  It's
built for an era where bundling assets to save on HTTP connections is no
longer urgent, where JavaScript and CSS are either compiled by dedicated
Node.js bundlers or served directly to the browsers, and where increases in
bandwidth have made the need for minification less pressing.  These factors
allow for a dramatically simpler and faster asset pipeline compared to
previous options, like Sprockets.")
    (home-page "https://github.com/rails/propshaft")
    (license license:expat)))

;;; Pro-tip: to get a summary of the failures, run
;;; 'M-x occur [1-9][0-9]* \(failures\|errors\)' on the build log.
(define-public ruby-railties
  (package
    (name "ruby-railties")
    (version %ruby-rails-version)
    (source ruby-rails-monorepo)
    (build-system ruby-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'delete-gemfiles
            (lambda _
              ;; Delete Gemfile and Gemfile.lock, as they contains too many
              ;; dependencies not actually useful here.
              (delete-file "Gemfile")
              (delete-file "Gemfile.lock")))
          (add-after 'extract-gemspec 'chdir
            (lambda _
              (chdir "railties")))
          (add-after 'chdir 'disable-bundler
            (lambda _
              (substitute* (append (list "Rakefile")
                                   (find-files "test" "\\.rb$")
                                   (find-files "lib" "\\.tt$"))
                ;; Do not use Bundler, which causes errors such as not finding
                ;; the gem of this package (railties), or preferring the other
                ;; in-source gems.
                (("`bundle exec") "`")
                ((".*require \"bundler/setup\".*") "")
                ((".*Bundler.require.*") ""))
              ;; Adjust a runtime substitution that uses a removed
              ;; Bundler.require in its pattern; instead of matching
              ;; "Bundler.require", it now appends to the 'require
              ;; "rails/all"' line in the generated 'application.rb' template
              ;; generated from
              ;; "lib/rails/generators/rails/app/templates/config/application.rb.tt".
              (substitute* "test/isolation/abstract_unit.rb"
                (("contents.sub!\\(/\\^Bundler\\\\.require\\.\\*/, \"([^\"]*)"
                  _ replacement)
                 (format #f "contents.sub!('require \"rails/all\"', \"\\\\0\\n~a"
                         replacement)))))
          (add-after 'chdir 'do-not-load-other-gems-from-source
            (lambda _
              ;; The Rakefile adds '-I' Ruby options so that the other Rails
              ;; libraries are loaded from source; since they are already
              ;; packaged separately, use these instead.
              (substitute* "Rakefile"
                ((".*\"\\.\\./activesupport/lib\",.*") "")
                ((".*\"\\.\\./actionpack/lib\",.*") "")
                ((".*\"\\.\\./actionview/lib\",.*") "")
                ((".*\"\\.\\./activemodel/lib\".*") ""))))
          (add-after 'chdir 'patch-paths
            (lambda _
              (substitute* "lib/rails/generators/base.rb"
                (("/usr/bin/env") (which "env")))))
          (delete 'check)               ;moved after install phase
          (add-after 'install 'check
            (assoc-ref %standard-phases 'check))
          (add-before 'check 'prepare-for-tests
            (lambda _
              (define (touch file-name)
                (call-with-output-file file-name (const #t)))
              ;; Otherwise, the test suite attempts to use yarn to fetch
              ;; NodeJS modules.
              (mkdir-p "../actionview/lib/assets/compiled")
              (touch "../actionview/lib/assets/compiled/rails-ujs.js")
              (mkdir-p "test/isolation/assets/node_modules")
              ;; Git requires to be able to write to HOME.
              (setenv "HOME" "/tmp")))
          (add-before 'check 'disable-problematic-tests
            (lambda _
              (let-syntax ((skip-tests
                            (syntax-rules ()
                              ((_ file test ...)
                               (substitute* file
                                 ;; ActiveSupport test case.
                                 (((string-append "test \"" test "\".*") all)
                                  (string-append
                                   all "    skip    'fails on guix'\n")) ...
                                   ;; MiniTest test case.
                                 (((string-append "def " test ".*") all)
                                  (string-append
                                   all "    skip('fails on guix')\n")) ...)))))
                (with-directory-excursion "test"
                  ;; This test requires 'rails' and Bundler.
                  (delete-file "application/server_test.rb")
                  ;; These tests are incompatible with MiniTest 5.17 (see:
                  ;; https://github.com/rails/rails/issues/47657).
                  (skip-tests "generators_test.rb"
                              "test_invoke_with_config_values"
                              "test_simple_invoke"
                              "test_should_give_higher_preference_to_rails_generators"
                              "test_nested_fallbacks_for_generators"
                              "test_fallbacks_for_generators_on_invoke"
                              "test_invoke_with_default_values"
                              "test_invoke_with_nested_namespaces")
                  ;; These tests requires the assets which we lack.
                  (delete-file "application/assets_test.rb")
                  (delete-file "railties/generators_test.rb")
                  (skip-tests "generators/shared_generator_tests.rb"
                              ;; This test checks that bin/rails has /usr/bin/env has a
                              ;; shebang and fails.
                              "test_shebang_when_is_the_same_as_default_use_env")
                  (skip-tests "generators/app_generator_test.rb"
                              ;; This test requires networking.
                              "test_template_from_url"
                              ;; This test requires Bundler.
                              "test_generation_use_original_bundle_environment"
                              ;; This test requires assets.
                              "test_css_option_with_cssbundling_gem"
                              ;; These tests require the rails/command
                              ;; namespace provided by the 'ruby-rails'
                              ;; package, which depends on this one.
                              "test_css_option_with_asset_pipeline_tailwind"
                              "test_hotwire")
                  (skip-tests
                   "generators/plugin_generator_test.rb"
                   ;; These tests require assets.
                   "test_model_with_existent_application_record_in_mountable_engine"
                   "test_dummy_application_loads_plugin"
                   "test_generate_application_mailer_when_does_not_exist_in_\
mountable_engine"
                   "test_generate_mailer_layouts_when_does_not_exist_in_mountable_engine"
                   "test_ensure_that_migration_tasks_work_with_mountable_option"
                   "test_generating_controller_inside_mountable_engine"
                   "test_generate_application_job_when_does_not_exist_in_mountable_engine"
                   "test_run_default"
                   ;; This test expects a /usr/bin/env shebang.
                   "test_shebang")
                  ;; The following generator tests require assets.
                  (skip-tests "generators/plugin_test_runner_test.rb"
                              "test_run_default")
                  (skip-tests
                   "generators/scaffold_controller_generator_test.rb"
                   "test_controller_tests_pass_by_default_inside_full_engine"
                   "test_controller_tests_pass_by_default_inside_mountable_engine")
                  (skip-tests
                   "generators/scaffold_generator_test.rb"
                   "test_scaffold_tests_pass_by_default_inside_mountable_engine"
                   "test_scaffold_tests_pass_by_default_inside_api_mountable_engine"
                   "test_scaffold_tests_pass_by_default_inside_api_full_engine"
                   "test_scaffold_on_invoke_inside_mountable_engine"
                   "test_scaffold_tests_pass_by_default_inside_full_engine"
                   "test_scaffold_tests_pass_by_default_inside_namespaced_\
mountable_engine")
                  (skip-tests "generators/test_runner_in_engine_test.rb"
                              "test_run_default"
                              "test_rerun_snippet_is_relative_path")
                  ;; The actions_test tests depend on assets or the rails gem.
                  (delete-file "generators/actions_test.rb")
                  (skip-tests "engine/commands_test.rb"
                              "test_server_command_work_inside_engine"
                              "test_runner_command_work_inside_engine")
                  ;; These tests fails because of cleanup code
                  ;; when the environment lacks a PTY device (see:
                  ;; https://github.com/rails/rails/issues/47656).
                  (delete-file "engine/commands_test.rb")
                  ;; The following tests require the 'rails' gem.
                  (skip-tests "application/test_runner_test.rb"
                              "test_run_app_without_rails_loaded"
                              "test_generated_scaffold_works_with_rails_test"
                              "test_load_fixtures_when_running_test_suites"
                              "test_run_in_parallel_with_unmarshable_exception"
                              "test_run_in_parallel_with_unknown_object")
                  (skip-tests
                   "application/test_test.rb"
                   "automatically synchronizes test schema after rollback"
                   "hooks for plugins"
                   "sql structure migrations when adding column to existing table"
                   "sql structure migrations"
                   "ruby schema migrations")
                  ;; These tests require a PostgreSQL server accepting
                  ;; connections under /var/run/postgresql.
                  (skip-tests
                   "application/rake_test.rb"
                   "test_not_protected_when_previous_migration_was_not_production")
                  (delete-file "application/rake/dbs_test.rb")
                  (delete-file "application/rake/migrations_test.rb")
                  (delete-file "application/rake/multi_dbs_test.rb")
                  (skip-tests "engine/test_test.rb"
                              "automatically synchronize test schema")
                  (skip-tests "isolation/abstract_unit.rb" "use_postgresql")
                  (skip-tests "railties/engine_test.rb"
                              "active_storage:install task works within engine"
                              "active_storage:update task works within engine"
                              "rake environment can be called in the engine"
                              "mountable engine should copy migrations within engine_path"
                              ;; This test fails because we do not use the
                              ;; in-source active/action gems.
                              "i18n files have lower priority than application ones"
                              ;; This test fails when not using Bundler.
                              "setting priority for engines with config.railties_order")
                  ;; This test requires a database server or networking.
                  (delete-file "application/bin_setup_test.rb")
                  (skip-tests "application/middleware/cache_test.rb"
                              ;; This test produces "miss, store" instead of
                              ;; "fresh".
                              "test_cache_works_with_expires"
                              ;; This one produces "miss" instead of "stale,
                              ;; valid, store".
                              "test_cache_works_with_etags"
                              ;; Likewise.
                              "test_cache_works_with_last_modified")
                  (skip-tests "application/initializers/frameworks_test.rb"
                              ;; These tests are either broken, or rely on
                              ;; database availability
                              "expire schema cache dump if the version can't be checked because the database is unhealthy"
                              "does not expire schema cache dump if check_schema_cache_dump_version is false and the database unhealthy"
                              "does not expire schema cache dump if check_schema_cache_dump_version is false")))))
          (add-before 'check 'set-paths
            (lambda _
              (setenv "PATH" (string-append (getenv "PATH") ":"
                                            #$output "/bin"))
              (setenv "GEM_PATH" (string-append
                                  (getenv "GEM_PATH") ":"
                                  #$output "/lib/ruby/vendor_ruby")))))))
    (native-inputs
     (list git-minimal/pinned
           ruby-actioncable
           ruby-actionmailbox
           ruby-actionmailer
           ruby-actiontext
           ruby-actionview
           ruby-activejob
           ruby-activemodel
           ruby-activerecord
           ruby-activestorage
           ruby-bcrypt
           ruby-bootsnap
           ruby-capybara
           ruby-dalli
           ruby-importmap-rails-bootstrap
           ruby-listen
           ruby-minitest-retry
           ruby-mysql2
           ruby-pg
           ruby-selenium-webdriver
           ruby-sprockets-rails
           ruby-webrick
           sqlite))
    (propagated-inputs
     (list ruby-actionpack
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

(define-public ruby-sassc-rails
  (package
    (name "ruby-sassc-rails")
    (version "2.1.2")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "sassc-rails" version))
              (sha256
               (base32
                "1d9djmwn36a5m8a83bpycs48g8kh1n2xkyvghn7dr6zwh4wdyksz"))))
    (build-system ruby-build-system)
    ;; The test suite currently fails with multiple "FrozenError: can't modify
    ;; frozen Array: []" errors, apparently caused by Rails 7 (see:
    ;; https://github.com/sass/sassc-rails/pull/178/files).
    (arguments (list #:tests? #f
                     #:phases #~(modify-phases %standard-phases
                                  (add-after 'extract-gemspec 'relax-requirements
                                    (lambda _
                                      (substitute* "sassc-rails.gemspec"
                                        (("%q<rake>.freeze, \\[\"~> 10.0\"]")
                                         "%q<rake>.freeze, [\">= 10.0\"]")))))))
    (native-inputs (list ruby-mocha ruby-pry ruby-tzinfo-data))
    (propagated-inputs (list ruby-railties ruby-sassc ruby-sprockets
                             ruby-sprockets-rails ruby-tilt))
    (synopsis "SassC-Ruby integration with Rails")
    (description "This Ruby library integrates SassC-Ruby into Rails.")
    (home-page "https://github.com/sass/sassc-rails")
    (license license:expat)))

(define-public ruby-sprockets
  (package
    (name "ruby-sprockets")
    (version "4.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "sprockets" version))
       (sha256
        (base32
         "0k0236g4h3ax7v6vp9k0l2fa0w6f1wqp7dn060zm4isw4n3k89sw"))))
    (build-system ruby-build-system)
    (arguments
     '(;; No included tests
       #:tests? #f))
    (propagated-inputs
     (list ruby-concurrent ruby-rack))
    (synopsis "Sprockets is a Rack-based asset packaging system")
    (description
     "Sprockets is a Rack-based asset packaging system that concatenates and
serves JavaScript, CoffeeScript, CSS, LESS, Sass, and SCSS.")
    (home-page "https://github.com/rails/sprockets")
    (license license:expat)))

(define-public ruby-sprockets-rails
  (package
   (name "ruby-sprockets-rails")
   (version "3.4.2")
   (source
    (origin
     (method url-fetch)
     (uri (rubygems-uri "sprockets-rails" version))
     (sha256
      (base32
       "1b9i14qb27zs56hlcc2hf139l0ghbqnjpmfi0054dxycaxvk5min"))))
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

(define-public ruby-stimulus-rails
  (package
    (name "ruby-stimulus-rails")
    (version "1.2.1")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "stimulus-rails" version))
              (sha256
               (base32
                "12hfdzh6cwahbd6p4r0r0d14n3laj3cqasx9w18wga9fq70bq6w3"))))
    (build-system ruby-build-system)
    (propagated-inputs (list ruby-railties))
    (synopsis "Modest JavaScript framework for Rails")
    (description "This package provides a modest JavaScript framework for the
HTML you already have.")
    (home-page "https://stimulus.hotwired.dev")
    (license license:expat)))

(define-public ruby-turbo-rails
  (package
    (name "ruby-turbo-rails")
    (version "1.4.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "turbo-rails" version))
              (sha256
               (base32
                "0vm3iqgr3kxyyz5i09lhvfszp4pw1gw5j5rhhv1gmasv4kq2p3qh"))))
    (build-system ruby-build-system)
    ;; The test suite depends on JavaScript modules fetched via 'yarn'.
    (arguments (list #:tests? #f))
    (propagated-inputs (list ruby-actionpack ruby-activejob ruby-railties))
    (synopsis "High performance web application framework")
    (description
     "Turbo aims to be as fast as single-page web application without having
to write any JavaScript.  Turbo accelerates links and form submissions without
requiring server-side changes to the generated HTML.  It allows carving up a
page into independent frames, which can be lazy-loaded and operated as
independent components.  Finally, it helps making partial page updates using
just HTML and a set of CRUD-like container tags.  These three techniques
reduce the amount of custom JavaScript that many web applications need to
write by an order of magnitude.  And for the few dynamic bits that are left,
Stimulus can be used.")
    (home-page "https://github.com/hotwired/turbo-rails")
    (license license:expat)))

(define-public ruby-web-console
  (package
    (name "ruby-web-console")
    (version "4.2.0")
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
         "07mg9nq7h48n01hps1m0g2nk94zknab6mrcxsv8x2vaf2xfgjilg"))))
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
               (("group :test do") "group :test do\n  gem 'tzinfo-data'")))))))
    (propagated-inputs
     (list ruby-actionview ruby-activemodel ruby-arel ruby-skiptrace ruby-railties))
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

;;; This is a meta-package which propagates all the individual Rails
;;; components.
(define-public ruby-rails
  (package
    (name "ruby-rails")
    (version %ruby-rails-version)
    (source ruby-rails-monorepo)
    (build-system ruby-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'delete-extraneous-gemspec-files
            (lambda _
              ;; They would otherwise be picked up instead of rails.gemspec at
              ;; the root of the repository.
              (for-each (lambda (f)
                          (unless (string-suffix? "rails.gemspec" f)
                            (delete-file f)))
                        (find-files "." "\\.gemspec"))))
          ;; This gem acts as glue between the gems that actually make up
          ;; Rails. The important thing to check is that the gemspec matches
          ;; up with the Guix packages and Rubygems can successfully activate
          ;; the Rails gem.
          ;;
          ;; The following check phase tests this.
          (delete 'check)
          (add-after 'install 'check
            (lambda* (#:key tests? outputs #:allow-other-keys)
              (when tests?
                (setenv "GEM_PATH"
                        (string-append (getenv "GEM_PATH") ":" #$output
                                       "/lib/ruby/vendor_ruby"))
                (invoke "ruby" "-e" "gem 'rails'")))))))
    (propagated-inputs
     (list bundler
           ruby-actioncable
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
           ruby-railties
           ruby-sprockets-rails))
    (synopsis "Full-stack web framework optimized for programmer happiness")
    (description
     "Ruby on Rails is a full-stack web framework optimized for programmer
happiness and sustainable productivity.  It encourages beautiful code by
favoring convention over configuration.")
    (home-page "https://rubyonrails.org/")
    (license license:expat)))
