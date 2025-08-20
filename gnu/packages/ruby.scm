;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015 Pjotr Prins <pjotr.guix@thebird.nl>
;;; Copyright © 2014, 2015 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014, 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015-2018 Ben Woodcroft <donttrustben@gmail.com>
;;; Copyright © 2015 David Thompson <davet@gnu.org>
;;; Copyright © 2015, 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2018 Danny Milosavljevic <dannym@scratchpost.org>
;;; Copyright © 2019, 2020 Christopher Baines <mail@cbaines.net>
;;; Copyright © 2019 Jelle Licht <jlicht@fsfe.org>
;;; Copyright © 2020, 2023 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2020 Ryan Prior <rprior@protonmail.com>
;;; Copyright © 2020 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2021 Marius Bakke <marius@gnu.org>
;;; Copyright © 2023 Christopher Baines <mail@cbaines.net>
;;; Copyright © 2023 Janneke Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2023 Zheng Junjie <873216071@qq.com>
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
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages web)
  #:use-module (guix build-system ruby))

(define-public ruby-3.4
  (package
    (name "ruby")
    (version "3.4.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://cache.ruby-lang.org/pub/ruby/"
                           (version-major+minor version)
                           "/ruby-" version ".tar.xz"))
       (sha256
        (base32
         "1x18dr3qhr1mypbvxc7yr46z06l11if3cx3babcfv7a9x7pn6vgp"))))
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
               (("/bin/sh") (which "sh"))))))))
    (native-inputs
     (append (if (%current-target-system)
                 (list this-package)
                 '())
             (list autoconf libyaml)))
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

(define-public ruby-3.3
  (package
    (inherit ruby-3.4)
    (version "3.3.8")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://cache.ruby-lang.org/pub/ruby/"
                           (version-major+minor version)
                           "/ruby-" version ".tar.xz"))
       (sha256
        (base32
         "0hl6bi27w711hva96f95hg02vqzb30kabdw9hbj3rnj3w3z71bj4"))))))

(define-public ruby-2.7
  (package
    (name "ruby")
    (version "2.7.8")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://cache.ruby-lang.org/pub/ruby/"
                           (version-major+minor version)
                           "/ruby-" version ".tar.gz"))
       (sha256
        (base32
         "182vni66djmiqagwzfsd0za7x9k3zag43b88c590aalgphybdnn2"))
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
             (list autoconf)))
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

(define-public ruby ruby-3.3)

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
                (string-append m " --verbose")))))
         (add-after 'unpack 'disable-broken-tests
           (lambda _
             (substitute* "mrbgems/mruby-io/test/io.rb"
               (("assert\\('IO.popen.+$" m)
                (string-append m "skip \"Hangs in the Guix build environment\"\n"))
               ;; This one is really weird.  The *expected* output is all wrong.
               (("assert\\('`cmd`.*" m)
                (string-append m "skip \"Disable for Guix\"\n")))))
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
               (copy-recursively "include" inc)))))))
    (native-inputs
     (list ruby bison))
    (home-page "https://github.com/mruby/mruby")
    (synopsis "Lightweight Ruby")
    (description "mruby is the lightweight implementation of the Ruby
language.  Its syntax is Ruby 3.x compatible except for pattern
matching.  mruby can be linked and embedded within your application.")
    (license license:expat)))

;;;
;;; Avoid adding new packages to the end of this file. To reduce the chances
;;; of a merge conflict, place them above by existing packages with similar
;;; functionality or similar names.
;;;
