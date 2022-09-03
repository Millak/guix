;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Steve Sprang <scs@stevesprang.com>
;;; Copyright © 2016, 2017 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016, 2017 Pjotr Prins <pjotr.guix@thebird.nl>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Nikita <nikita@n0.is>
;;; Copyright © 2020-2022 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2021 Oskar Köök <oskar@maatriks.ee>
;;; Copyright © 2021 Cees de Groot <cg@evrl.com>
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

(define-module (gnu packages erlang)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system emacs)
  #:use-module (guix build-system rebar)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages wxwidgets))

(define-public erlang
  (package
    (name "erlang")
    (version "25.0.4")
    (source (origin
              (method git-fetch)
              ;; The tarball from http://erlang.org/download contains many
              ;; pre-compiled files, so we use this snapshot of the source
              ;; repository.
              (uri (git-reference
                    (url "https://github.com/erlang/otp")
                    (commit (string-append "OTP-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0n39pd5d8mmyj03d8fdan0zb2pcc383pmqf0v7s356fgirdbsrcm"))
              (patches (search-patches "erlang-man-path.patch"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("perl" ,perl)

       ;; Erlang's documentation is distributed in a separate tarball.
       ("erlang-manpages"
        ,(origin
           (method url-fetch)
           (uri (string-append "http://erlang.org/download/otp_doc_man_"
                               (version-major+minor version) ".tar.gz"))
           (sha256
            (base32
             "17ap4kawlbqmcl13c543gh54p1ng8ivxmbn6lbbij07k81ry5p1y"))))))
    (inputs
     (list ncurses openssl wxwidgets))
    (propagated-inputs
     (list fontconfig glu mesa))
    (arguments
     `(#:test-target "release_tests"
       #:configure-flags
       (list "--disable-saved-compile-time"
             "--enable-dynamic-ssl-lib"
             "--enable-native-libs"
             "--enable-shared-zlib"
             "--enable-smp-support"
             "--enable-threads"
             "--enable-wx"
             (string-append "--with-ssl=" (assoc-ref %build-inputs "openssl")))
       #:modules ((srfi srfi-19)        ; make-time, et cetera.
                  (guix build utils)
                  (guix build gnu-build-system))
       #:phases
       (modify-phases %standard-phases
         (delete 'bootstrap)
         ;; The are several code fragments that embed timestamps into the
         ;; output. Here, we alter those fragments to use the value of
         ;; SOURCE_DATE_EPOCH instead.
         (add-after 'unpack 'remove-timestamps
           (lambda _
             (let ((source-date-epoch
                    (time-utc->date
                     (make-time time-utc 0 (string->number
                                            (getenv "SOURCE_DATE_EPOCH"))))))
               (substitute* "lib/reltool/src/reltool_target.erl"
                 (("Date = date\\(\\),")
                  (string-append "Date = "
                                 (date->string source-date-epoch
                                               "'{~Y,~m,~d}',"))))
               (substitute* "lib/reltool/src/reltool_target.erl"
                 (("Time = time\\(\\),")
                  (string-append "Time = "
                                 (date->string source-date-epoch
                                               "'{~H,~M,~S}',"))))
               (substitute* '("lib/reltool/src/reltool_target.erl"
                              "lib/sasl/src/systools_make.erl")
                 (("date\\(\\), time\\(\\),")
                  (date->string source-date-epoch
                                "{~Y,~m,~d}, {~H,~M,~S},")))
               (substitute* "lib/dialyzer/test/small_SUITE_data/src/gs_make.erl"
                 (("tuple_to_list\\(date\\(\\)\\),tuple_to_list\\(time\\(\\)\\)")
                  (date->string
                   source-date-epoch
                   "tuple_to_list({~Y,~m,~d}), tuple_to_list({~H,~M,~S})")))
               (substitute* "lib/snmp/src/compile/snmpc_mib_to_hrl.erl"
                 (("\\{Y,Mo,D\\} = date\\(\\),")
                  (date->string source-date-epoch
                                "{Y,Mo,D} = {~Y,~m,~d},")))
               (substitute* "lib/snmp/src/compile/snmpc_mib_to_hrl.erl"
                 (("\\{H,Mi,S\\} = time\\(\\),")
                  (date->string source-date-epoch
                                "{H,Mi,S} = {~H,~M,~S},"))))))
         (add-after 'unpack 'patch-/bin/sh
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((sh (search-input-file inputs "/bin/sh")))
               (substitute* "erts/etc/unix/run_erl.c"
                 (("sh = \"/bin/sh\";")
                  (string-append "sh = \"" sh "\";")))
               (substitute* "erts/emulator/sys/unix/sys_drivers.c"
                 (("SHELL \"/bin/sh\"")
                  (string-append "SHELL \"" sh "\"")))
               (substitute* "erts/emulator/sys/unix/erl_child_setup.c"
                 (("SHELL \"/bin/sh\"")
                  (string-append "SHELL \"" sh "\"")))
               (substitute* "lib/kernel/src/os.erl"
                 (("/bin/sh") sh)))))
         (add-after 'patch-source-shebangs 'patch-source-env
           (lambda _
             (let ((escripts
                    (append
                        (find-files "." "\\.escript")
                        (find-files "lib/stdlib/test/escript_SUITE_data/")
                      '("erts/lib_src/utils/make_atomics_api"
                        "erts/preloaded/src/add_abstract_code"
                        "lib/diameter/bin/diameterc"
                        "lib/reltool/examples/display_args"
                        "lib/reltool/examples/mnesia_core_dump_viewer"
                        "lib/snmp/src/compile/snmpc.src"
                        "make/verify_runtime_dependencies"
                        "make/emd2exml.in"))))
               (substitute* escripts
                 (("/usr/bin/env") (which "env"))))))
         (add-before 'configure 'set-erl-top
           (lambda _
             (setenv "ERL_TOP" (getcwd))))
         (add-after 'install 'patch-erl
           ;; This only works after install.
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute* (string-append out "/bin/erl")
                 (("sed") (which "sed"))))))
         (add-after 'install 'install-doc
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (manpages (assoc-ref inputs "erlang-manpages"))
                    (share (string-append out "/share/")))
               (mkdir-p share)
               (with-directory-excursion share
                 (invoke "tar" "xvf" manpages))))))))
    (home-page "https://www.erlang.org/")
    (synopsis "The Erlang programming language")
    (description
     "Erlang is a programming language used to build massively
scalable soft real-time systems with requirements on high
availability.  Some of its uses are in telecoms, banking, e-commerce,
computer telephony and instant messaging.  Erlang's runtime system has
built-in support for concurrency, distribution and fault tolerance.")
    ;; Erlang is distributed under the Apache License 2.0, but some components
    ;; have other licenses. See 'system/COPYRIGHT' in the source distribution.
    (license (list license:asl2.0 license:bsd-2 license:bsd-3 license:expat
                   license:lgpl2.0+ license:tcl/tk license:zlib))))

(define-public emacs-erlang
  (package
    (name "emacs-erlang")
    (version (package-version erlang))
    (source (package-source erlang))
    (build-system emacs-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'expand-load-path 'change-working-directory
           (lambda _ (chdir "lib/tools/emacs") #t)))))
    (home-page "https://www.erlang.org/")
    (synopsis "Erlang major mode for Emacs")
    (description
     "This package provides an Emacs major mode for editing Erlang source
files.")
    (license license:asl2.0)))

(define-public erlang-bbmustache
  (package
    (name "erlang-bbmustache")
    (version "1.12.2")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "bbmustache" version))
       (sha256
        (base32 "0fvvaxdpziygxl30j59g98qkh2n47xlb7w5dfpsm2bfcsnj372v8"))))
    (build-system rebar-build-system)
    (inputs
     (list erlang-getopt rebar3-git-vsn
           erlang-edown))  ; for building the docs
    (arguments
     `(#:tests? #f ;; requires mustache specification file
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'build-more
           (lambda _
             (invoke "rebar3" "as" "dev" "escriptize")))
         (add-after 'install 'install-escript
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out")))
               (install-file "_build/dev/bin/bbmustache"
                             (string-append out "/bin"))))))))
    (home-page "https://github.com/soranoba/bbmustache/")
    (synopsis "Binary pattern match Based Mustache template engine for Erlang")
    (description "This Erlang library provides a Binary pattern match Based
Mustache template engine")
    (license license:expat)))

(define-public erlang-certifi
  (package
    (name "erlang-certifi")
    (version "2.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "certifi" version))
       (sha256
        (base32 "0ha6vmf5p3xlbf5w1msa89frhvfk535rnyfybz9wdmh6vdms8v96"))))
    (build-system rebar-build-system)
    (arguments
     `(#:tests? #f)) ;; have not been updated for latest cert bundle
    (home-page "https://github.com/certifi/erlang-certifi/")
    (synopsis "Erlang CA certificate bundle")
    (description "This Erlang library contains a CA bundle that you can
reference in your Erlang application.  This is useful for systems that do not
have CA bundles that Erlang can find itself, or where a uniform set of CAs is
valuable.

This an Erlang specific port of certifi.  The CA bundle is derived from
Mozilla's canonical set.")
    (license license:bsd-3)))

(define-public erlang-cf
  (package
    (name "erlang-cf")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "cf" version))
       (sha256
        (base32 "0wknz4xkqkhgvlx4vx5619p8m65v7g87lfgsvfy04jrsgm28spii"))))
    (build-system rebar-build-system)
    (home-page "https://github.com/project-fifo/cf")
    (synopsis "Terminal colour helper for Erlang io and io_lib")
    (description "This package provides a helper library for termial colour
printing extending the io:format syntax to add colours.")
    (license license:expat)))

(define-public erlang-covertool
  (package
    (name "erlang-covertool")
    (version "2.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "covertool" version))
       (sha256
        (base32 "1p0c1n3nl4063xwi1sv176l1x68xqf07qwvj444a5z888fx6i5aw"))))
    (build-system rebar-build-system)
    (home-page "https://github.com/covertool/covertool")
    (synopsis "Convert code-coverage data generated by @code{cover} into
Cobertura XML reports")
    (description "This package provides a build tool and plugin to convert
exported Erlang @code{cover} data sets into Cobertura XML reports, which can
then be feed to the Jenkins Cobertura plug-in.

On @emph{hex.pm}, this plugin was previously called @code{rebar_covertool}.")
    (license license:bsd-2)))

(define-public erlang-cth-readable
  (package
    (name "erlang-cth-readable")
    (version "1.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "cth_readable" version))
       (sha256
        (base32 "104xgybb6iciy6i28pyyrarqzliddi8kjyq43ajaav7y5si42rb8"))))
    (build-system rebar-build-system)
    (propagated-inputs
     (list erlang-cf))
    (arguments
     `(#:tests? #f)) ;; no test-suite in hex-pm package
    (home-page "https://github.com/ferd/cth_readable")
    (synopsis "Common Test hooks for more readable logs for Erlang")
    (description "This package provides an OTP library to be used for CT log
outputs you want to be readable around all that noise they contain.")
    (license license:bsd-3)))

(define-public erlang-edown
  (package
    (name "erlang-edown")
    (version "0.8.4")
    (source
      (origin
        (method url-fetch)
        (uri (hexpm-uri "edown" version))
        (sha256
          (base32 "0ij47gvgs6yfqphj0f54qjzj18crj8y1dsjjlzpp3dp8pscqzbqw"))))
    (build-system rebar-build-system)
    (home-page "https://github.com/uwiger/edown")
    (synopsis "Markdown extension for EDoc")
    (description "This package provides an extension for EDoc for generating
Markdown.")
    (license license:asl2.0)))

(define-public erlang-erlware-commons
  (package
    (name "erlang-erlware-commons")
    (version "1.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "erlware_commons" version))
       (sha256
        (base32 "18qam9xdzi74wppb0cj4zc8161i0i8djr79z8662m6d276f2jz5m"))))
    (build-system rebar-build-system)
    (propagated-inputs
     (list erlang-cf))
    (native-inputs
     (list git-minimal/fixed))  ;; Required for tests
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'check-setup
           (lambda _
             (setenv "TERM" "xterm")))))) ; enable color in logs
    (home-page "http://erlware.github.io/erlware_commons/")
    (synopsis "Additional standard library for Erlang")
    (description "Erlware Commons is an Erlware project focused on all aspects
of reusable Erlang components.")
    (license license:expat)))

(define-public erlang-eunit-formatters
  (package
    (name "erlang-eunit-formatters")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "eunit_formatters" version))
       (sha256
        (base32 "1jb3hzb216r29x2h4pcjwfmx1k81431rgh5v0mp4x5146hhvmj6n"))))
    (build-system rebar-build-system)
    (home-page "https://github.com/seancribbs/eunit_formatters")
    (synopsis "Better output for eunit suites")
    (description "This package provides a better output for Erlang eunits.")
    (license license:asl2.0)))

(define-public erlang-getopt
  (package
    (name "erlang-getopt")
    (version "1.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "getopt" version))
       (sha256
        (base32 "09pasi7ki1rivw9sl7xndj5qgjbdqvcscxk83yk85yr28gm9l0m0"))))
    (build-system rebar-build-system)
    (home-page "https://github.com/jcomellas/getopt")
    (synopsis "Command-line options parser for Erlang")
    (description "This package provides an Erlang module to parse command line
arguments using the GNU getopt syntax.")
    (license license:bsd-3)))

(define-public erlang-hex-core
  (package
    (name "erlang-hex-core")
    (version "0.8.4")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "hex_core" version))
       (sha256
        (base32 "06p65hlm29ky03vs3fq3qz6px2ylwp8b0f2y75wdf5cm0kx2332b"))))
    (build-system rebar-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "rebar3" "as" "test" "proper")))))))
    (inputs
     (list erlang-proper rebar3-proper))
    (home-page "https://github.com/hexpm/hex_core")
    (synopsis "Reference implementation of Hex specifications")
    (description "This package provides the reference implementation of Hex
specifications.")
    (license license:asl2.0)))

(define-public erlang-jsone
  (package
    (name "erlang-jsone")
    (version "1.7.0")
    (source
      (origin
        (method url-fetch)
        (uri (hexpm-uri "jsone" version))
        (sha256
          (base32 "1gaxiw76syjp3s9rygskm32y9799b917q752rw8bxj3bxq93g8x3"))))
    (build-system rebar-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'disable-covertool
           ;; no need to generate a coverage report
           (lambda _
             (substitute* "rebar.config"
               (("\\{project_plugins, \\[covertool\\]\\}\\." _) "")))))))
    (home-page "https://github.com/sile/jsone/")
    (synopsis "Erlang JSON Library")
    (description "An Erlang library for encoding and decoding JSON data.")
    (license license:expat)))

(define-public erlang-parse-trans
  (package
    (name "erlang-parse-trans")
    (version "3.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "parse_trans" version))
       (sha256
        (base32 "16p4c2xjrvz16kzpr9pmcvi6nxq6rwckqi9fp0ksibaxwxn402k2"))))
    (build-system rebar-build-system)
    (inputs
     (list erlang-getopt))
    (home-page "https://github.com/uwiger/parse_trans")
    (synopsis "Parse transform utilities for Erlang")
    (description "This package captures some useful patterns in parse
transformation and code generation for Erlang.

For example generating standardized accessor functions for records or
evaluating an expression at compile-time and substitute the result as a
compile-time constant.")
    (license license:asl2.0)))

(define-public erlang-proper
  (package
    (name "erlang-proper")
    (version "1.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (hexpm-uri "proper" version))
        (sha256
          (base32 "1fwcas4a9kz3w3z1jqdk9lw8822srfjk9lcpvbxkxlsv3115ha0q"))))
    (build-system rebar-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'disable-covertool
           ;; no need to generate a coverage report
           (lambda _
             (substitute* "rebar.config"
               (("\\{plugins, \\[covertool\\]\\}\\." _) "")))))))
    (home-page "https://proper-testing.github.io/")
    (synopsis "QuickCheck-inspired property-based testing tool for Erlang")
    (description "PropEr is a tool for the automated, semi-random,
property-based testing of Erlang programs.  It is fully integrated with
Erlang's type language, and can also be used for the model-based random
testing of stateful systems.")
    (license license:gpl3+)))

(define-public erlang-providers
  (package
    (name "erlang-providers")
    (version "1.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "providers" version))
       (sha256
        (base32 "05y0kz3xgx77hzn1l05byaisvmk8bgds7c22hrh0a5ba81sfi1yj"))))
    (build-system rebar-build-system)
    (propagated-inputs
     (list erlang-erlware-commons erlang-getopt))
    (home-page "https://github.com/tsloughter/providers")
    (synopsis "Erlang providers library")
    (description "This package provides an Erlang providers library.")
    (license license:asl2.0)))

(define-public erlang-relx
  (package
    (name "erlang-relx")
    (version "4.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "relx" version))
       (sha256
        (base32 "02gmfx1vxg9m3mq4njsqhs4972l4nb8m5p1pdcf64g09ccf17y1g"))))
    (build-system rebar-build-system)
    (propagated-inputs
     (list erlang-bbmustache))
    (home-page "https://erlware.github.io/relx/")
    (synopsis "Release assembler for Erlang/OTP Releases")
    (description "Relx assembles releases for an Erlang/OTP release.  Given a
release specification and a list of directories in which to search for OTP
applications it will generate a release output.  That output depends heavily on
what plugins available and what options are defined, but usually it is simply
a well configured release directory.")
    (license license:asl2.0)))

(define-public erlang-ssl-verify-fun
  (package
    (name "erlang-ssl-verify-fun")
    (version "1.1.6")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "ssl_verify_fun" version))
       (sha256
        (base32 "1026l1z1jh25z8bfrhaw0ryk5gprhrpnirq877zqhg253x3x5c5x"))))
    (build-system rebar-build-system)
    (home-page "https://github.com/deadtrickster/ssl_verify_fun.erl")
    (synopsis "SSL verification functions for Erlang")
    (description "This package provides SSL verification functions for
Erlang.")
    (license license:expat)))

(define-public rebar3
  (package
    (name "rebar3")
    (version "3.18.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/erlang/rebar3")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "09648hzc2mnjwf9klm20cg4hb5rn2xv2gmzcg98ffv37p5yfl327"))))
    (build-system gnu-build-system)
    ;; TODO: remove vendored modules, install man-page, install lib(?)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'bootstrap)
         (add-after 'unpack 'unpack-dependency-sources
           (lambda* (#:key inputs #:allow-other-keys)
             (for-each
              (lambda (pkgname)
                (let* ((src (string-append pkgname "-source"))
                       (input (assoc-ref inputs src))
                       (checkouts-dir (string-append "_checkouts/" pkgname))
                       (lib-dir (string-append "_build/default/lib/" pkgname)))
                  (mkdir-p checkouts-dir)
                  (invoke "tar" "-xf" input "-C" checkouts-dir)
                  (invoke "tar" "-xzf"
                          (pk (string-append checkouts-dir "/contents.tar.gz"))
                          "-C" checkouts-dir)
                  (mkdir-p lib-dir)
                  (copy-recursively checkouts-dir lib-dir)))
              (list "bbmustache" "certifi" "cf" "cth_readable"
                    "eunit_formatters" "getopt" "hex_core" "erlware_commons"
                    "parse_trans" "relx" "ssl_verify_fun" "providers"))))
         (delete 'configure)
         (replace 'build
           (lambda _
             (setenv "HOME" (getcwd))
             (invoke "./bootstrap")))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out")))
               (install-file "rebar3" (string-append out "/bin")))))
         (delete 'check))))
    (native-inputs
     (list erlang))
    (inputs
     `(("bbmustache-source" ,(package-source erlang-bbmustache))
       ("certifi-source" ,(package-source erlang-certifi))
       ("cf-source" ,(package-source erlang-cf))
       ("cth_readable-source" ,(package-source erlang-cth-readable))
       ("erlware_commons-source" ,(package-source erlang-erlware-commons))
       ("eunit_formatters-source" ,(package-source erlang-eunit-formatters))
       ("getopt-source" ,(package-source erlang-getopt))
       ("hex_core-source" ,(package-source erlang-hex-core))
       ("parse_trans-source" ,(package-source erlang-parse-trans))
       ("relx-source" ,(package-source erlang-relx))
       ("ssl_verify_fun-source" ,(package-source erlang-ssl-verify-fun))
       ("providers-source" ,(package-source erlang-providers))))
    (home-page "https://rebar3.org/")
    (synopsis "Sophisticated build-tool for Erlang projects that follows OTP
principles")
    (description "@code{rebar3} is an Erlang build tool that makes it easy to
compile and test Erlang applications, port drivers and releases.

@code{rebar3} is a self-contained Erlang script, so it's easy to distribute or
even embed directly in a project.  Where possible, rebar uses standard
Erlang/OTP conventions for project structures, thus minimizing the amount of
build configuration work.  @code{rebar3} also provides dependency management,
enabling application writers to easily re-use common libraries from a variety
of locations (git, hg, etc).")
    (license license:asl2.0)))

(define-public rebar3-raw-deps
  (package
    (name "rebar3-raw-deps")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "rebar3_raw_deps" version))
       (sha256
        (base32 "1pzmm3m8gb2s9jn8fp6shzgfmy4mvh2vdci0z6nsm74ma3ffh1i3"))))
    (build-system rebar-build-system)
    (home-page "https://github.com/soranoba/rebar3_raw_deps")
    (synopsis "Rebar3 plugin for supporting \"raw\" dependencies")
    (description "This plugin provides support for handling non-OTP
applications as a dependent libraries.")
    (license license:expat)))

(define-public rebar3-git-vsn
  (package
    (name "rebar3-git-vsn")
    (version "1.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (hexpm-uri "rebar3_git_vsn" version))
        (sha256
          (base32 "1dfz56034pa25axly9vqdzv3phkn8ll0qwrkws96pbgcprhky1hx"))))
    (build-system rebar-build-system)
    (inputs
     (list git-minimal/fixed))
    (arguments
     `(;; Running the tests require binary artifact (tar-file containing
       ;; samples git repos)  TODO: remove these from the source
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-path
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((git (assoc-ref inputs "git-minimal")))
               (substitute* "src/rebar3_git_vsn.erl"
                 (("rebar_utils:sh\\(\"git " _)
                  (string-append "rebar_utils:sh(\"" git "/bin/git ")))))))))
    (home-page "https://github.com/soranoba/rebar3_git_vsn")
    (synopsis "Rebar3 plugin for generating the version from git")
    (description "This plugin adds support for generating the version from
a git checkout.")
    (license license:expat)))

(define-public rebar3-proper
  (package
    (name "rebar3-proper")
    (version "0.12.1")
    (source
      (origin
        (method url-fetch)
        (uri (hexpm-uri "rebar3_proper" version))
        (sha256
          (base32 "1f174fb6h2071wr7qbw9aqqvnglzsjlylmyi8215fhrmi38w94b6"))))
    (build-system rebar-build-system)
    (home-page "https://github.com/ferd/rebar3_proper")
    (synopsis "Rebar3 PropEr plugin")
    (description "This plugin allows running PropEr test suites from within
rebar3.")
    (license license:bsd-3)))
