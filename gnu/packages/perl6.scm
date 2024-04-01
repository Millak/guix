;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019, 2024 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2022 Paul A. Patience <paul@apatience.com>
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

(define-module (gnu packages perl6)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system rakudo)
  #:use-module (gnu packages bdw-gc)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages tls))

(define-public moarvm
  (package
    (name "moarvm")
    (version "2022.04")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://moarvm.org/releases/MoarVM-"
                           version ".tar.gz"))
       (sha256
        (base32 "0128fxqaz7cwjf6amaz2cgd7xl52zvi5fr7bwnj229snll5za1mf"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;(delete-file-recursively "3rdparty/dynasm") ; JIT
           (delete-file-recursively "3rdparty/dyncall")
           (delete-file-recursively "3rdparty/freebsd")
           (delete-file-recursively "3rdparty/libatomicops")
           (delete-file-recursively "3rdparty/libuv")
           (delete-file-recursively "3rdparty/libtommath")
           (delete-file-recursively "3rdparty/msinttypes")))))
    (build-system perl-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-build
           (lambda _
             (substitute* "build/Makefile.in"
               (("^ +3rdparty/freebsd/.*") ""))))
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (pkg-config (assoc-ref inputs "pkg-config")))
               (setenv "CFLAGS" "-fcommon")
               (setenv "LDFLAGS"
                       ,@(if (target-ppc32?)
                           `((string-append "-Wl,-rpath=" out "/lib" " -latomic"))
                           `((string-append "-Wl,-rpath=" out "/lib"))))
               (invoke "perl" "Configure.pl"
                       "--prefix" out
                       "--pkgconfig" (string-append pkg-config "/bin/pkg-config")
                       "--has-libtommath"
                       "--has-libatomic_ops"
                       "--has-libffi"
                       "--has-libuv")))))))
    (home-page "https://moarvm.org/")
    (native-inputs
     (list pkg-config))
    ;; These should be inputs but moar.h can't find them when building Rakudo.
    (propagated-inputs
     (list libatomic-ops libffi libtommath libuv))
    (synopsis "VM for NQP And Rakudo Perl 6")
    (description
     "Short for \"Metamodel On A Runtime\", MoarVM is a modern virtual machine
built for the Rakudo Perl 6 compiler and the NQP Compiler Toolchain.  Highlights
include:

@itemize
@item Great Unicode support, with strings represented at grapheme level
@item Dynamic analysis of running code to identify hot functions and loops, and
perform a range of optimizations, including type specialization and inlining
@item Support for threads, a range of concurrency control constructs, and
asynchronous sockets, timers, processes, and more
@item Generational, parallel, garbage collection
@item Support for numerous language features, including first class functions,
exceptions, continuations, runtime loading of code, big integers and interfacing
with native libraries.
@end itemize")
    (license license:artistic2.0)))

(define-public nqp-configure
  (let ((commit "9b98931e0bfb8c4aac61590edf5074e63aa8ea4b"))
    (package
      (name "nqp-configure")
      ;; NQP and Rakudo use the same version of nqp-configure.
      ;; We may as well set nqp-configure's version to the same as theirs.
      (version "2022.04")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Raku/nqp-configure")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1vc1q11kjb964jal9dhgf5vwp371a3rfw7gj987n33kzli7a10n0"))))
      (build-system perl-build-system)
      (arguments
       '(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'create-makefile-and-manifest
             (lambda _
               (call-with-output-file "Makefile.PL"
                 (lambda (port)
                   (format port "
use ExtUtils::MakeMaker;
WriteMakefile(NAME => 'NQP::Config');\n")))
               (call-with-output-file "MANIFEST"
                 (lambda (port)
                   (format port "
LICENSE
MANIFEST
Makefile.PL
README.md
bin/make.nqp
doc/Macros.md
doc/NQP-Config.md
lib/NQP/Config.pm
lib/NQP/Config/Test.pm
lib/NQP/Macros.pm
t/10-config.t
t/20-macros.t
t/30-if-macro.t\n")))))
           (add-after 'patch-source-shebangs 'patch-more-shebangs
             (lambda _
               (substitute* '("bin/make.nqp"
                              "lib/NQP/Config.pm")
                 (("/bin/sh") (which "sh"))))))))
      (home-page "https://github.com/Raku/nqp-configure")
      (synopsis "Configuration and build modules for NQP")
      (description "This library provides support modules for NQP and Rakudo
@file{Configure.pl} scripts.")
      (license license:artistic2.0))))

(define-public nqp
  (package
    (name "nqp")
    (version "2022.04")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/Raku/nqp/releases/download/"
                           version "/nqp-" version ".tar.gz"))
       (sha256
        (base32 "1777shxr8qw6m2492ckb0r301qdx5gls6kphz554dh6k4n74avam"))
       (modules '((guix build utils)))
       (snippet
        '(delete-file-recursively "3rdparty"))))
    (build-system perl-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-calls-to-git
           (lambda _
             (invoke "perl" "-ni" "-e"
                     "print if not /^BEGIN {/ .. /^}/"
                     "Configure.pl")))
         (add-after 'remove-calls-to-git 'fix-paths
           (lambda _
             (substitute* "tools/build/gen-version.pl"
               (("catfile\\(\\$libdir, 'MAST', \\$_\\)")
                (string-append "catfile('"
                               (assoc-ref %build-inputs "moarvm")
                               "/share/nqp/lib"
                               "', 'MAST', $_)")))))
         (add-after 'patch-source-shebangs 'patch-more-shebangs
           (lambda _
             (substitute* '("t/nqp/111-spawnprocasync.t"
                            "t/nqp/113-run-command.t"
                            "tools/build/gen-js-cross-runner.pl"
                            "tools/build/gen-js-runner.pl"
                            "tools/build/install-js-runner.pl"
                            "tools/build/install-jvm-runner.pl.in")
               (("/bin/sh") (which "sh")))))
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (moar (assoc-ref inputs "moarvm")))
               (invoke "perl" "Configure.pl"
                       "--backends=moar"
                       "--with-moar" (string-append moar "/bin/moar")
                       "--prefix" out)))))))
    (native-inputs
     (list nqp-configure))
    (inputs
     (list moarvm))
    (home-page "https://github.com/Raku/nqp")
    (synopsis "Not Quite Perl")
    (description "This is \"Not Quite Perl\" -- a lightweight Raku-like
environment for virtual machines.  The key feature of NQP is that it's
designed to be a very small environment (as compared with, say, Rakudo) and is
focused on being a high-level way to create compilers and libraries for
virtual machines like MoarVM, the JVM, and others.

Unlike a full-fledged implementation of Raku, NQP strives to have as small a
runtime footprint as it can, while still providing a Raku object model and
regular expression engine for the virtual machine.")
    (license license:artistic2.0)))

(define-public rakudo
  (package
    (name "rakudo")
    (version "2022.04")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/rakudo/rakudo/releases/download/"
                           version "/rakudo-" version ".tar.gz"))
       (sha256
        (base32 "0x0w5b8g5kna1mlvsli9dqmnwvqalrar3cgpixmyiyvyjb6ah4vy"))
       (modules '((guix build utils)))
       (snippet
        '(delete-file-recursively "3rdparty"))))
    (build-system perl-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-calls-to-git
           (lambda _
             (invoke "perl" "-ni" "-e" "print if not /^BEGIN {/ .. /^}/"
                     "Configure.pl")))
         (add-after 'remove-calls-to-git 'fix-paths
           (lambda _
             (substitute* "tools/templates/Makefile-common-macros.in"
               (("NQP_CONFIG_DIR = .*")
                (string-append "NQP_CONFIG_DIR = "
                               (assoc-ref %build-inputs "nqp-configure")
                               "/lib/perl5/site_perl/"
                               ,(package-version perl)
                               "\n")))))
         ;; These tests pass when run manually.
         (add-after 'fix-paths 'disable-failing-tests
           (lambda _
             (substitute* "t/02-rakudo/repl.t"
               (("^plan 47;\n") "plan 46;\n"))
             (invoke "perl" "-ni" "-e"
                     "printf if not /^    \\(temp %\\*ENV\\)/ .. /^    }/"
                     "t/02-rakudo/repl.t")
             (substitute* "t/09-moar/01-profilers.t"
               (("^plan 12;\n") "plan 10;\n")
               (("^ok \\$htmlpath\\.IO\\.f, .*") "")
               (("^ok \\(try \\$htmlpath\\.IO\\.s .*") ""))))
         (add-after 'patch-source-shebangs 'patch-more-shebangs
           (lambda _
             (substitute* '("src/core.c/Proc.pm6"
                            "t/spec/S29-os/system.t"
                            "tools/build/create-js-runner.pl"
                            "tools/build/create-jvm-runner.pl")
               (("/bin/sh") (which "sh")))))
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (nqp (assoc-ref inputs "nqp")))
               (invoke "perl" "Configure.pl"
                       "--backend=moar"
                       "--with-nqp" (string-append nqp "/bin/nqp")
                       "--prefix" out))))
         ;; This is the recommended tool for distro maintainers to install Raku
         ;; modules systemwide.  See: https://github.com/ugexe/zef/issues/117
         (add-after 'install 'install-dist-tool
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (dest (string-append out "/share/perl6/tools")))
               (install-file "tools/install-dist.raku" dest)
               (substitute* (string-append dest "/install-dist.raku")
                 (("/usr/bin/env raku")
                  (string-append out "/bin/raku")))))))))
    (native-inputs
     (list nqp-configure))
    (inputs
     (list moarvm nqp openssl))
    (home-page "https://rakudo.org/")
    (native-search-paths
     (list (search-path-specification
            (variable "PERL6LIB")
            (separator ",")
            (files '("share/perl6/lib"
                     "share/perl6/site/lib"
                     "share/perl6/vendor/lib")))))
    (synopsis "Raku Compiler")
    (description "Rakudo is a compiler that implements the Raku specification
and runs on top of several virtual machines.")
    (license license:artistic2.0)))

(define-public perl6-format-lisp
  (let ((commit "1ae31e2a9366b287a83a9d451295fc9e3fb6d36f")
        (revision "0"))
    (package
      (name "perl6-format-lisp")
      (version (git-version "0.0.2" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/raku-community-modules/Format-Lisp")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1rhkd11xdyw99vl6ickjin0x7nhlgkq3qhpf8zp9jky0k711gxmr"))))
      (build-system rakudo-build-system)
      (arguments '(#:with-zef? #f))
      (home-page "https://github.com/raku-community-modules/Format-Lisp")
      (synopsis "Common Lisp's format call in Raku")
      (description "This Raku module implements Common Lisp's @code{format}
language.")
      (license license:artistic2.0))))

(define-public perl6-grammar-debugger
  ;; Last commit was September 2017.
  (let ((commit "0375008027c8caa216bd869476ce59ae09b2a702")
        (revision "1"))
    (package
      (name "perl6-grammar-debugger")
      (version (git-version "1.0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/jnthn/grammar-debugger")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0y826z3m276n7ia810hgcb3div67nxmx125m2fzlc16994zd5vm5"))))
      (build-system rakudo-build-system)
      (propagated-inputs
       (list perl6-terminal-ansicolor))
      (home-page "https://github.com/jnthn/grammar-debugger")
      (synopsis "Simple tracing and debugging support for Raku grammars")
      (description "This module provides a simple debugger for grammars.  Just
@code{use} it: use @code{Grammar::Debugger;} and any grammar in the lexical
scope of the use statement will automatically have debugging enabled. The
debugger will break execution when you first enter the grammar, and provide a
prompt.")
      (license license:artistic2.0))))

(define-public perl6-grammar-profiler-simple
  ;; Last commit was June 2017.
  (let ((commit "c0aca5fab323b2974821dabd6b89330c609e0b7d")
        (revision "1"))
    (package
      (name "perl6-grammar-profiler-simple")
      (version (git-version "0.02" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/perlpilot/Grammar-Profiler-Simple")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1qcsa4lmcilp3vp0jng0hrgzyzxin9ayg2wjvkcd0k6h7djx9dff"))))
      (build-system rakudo-build-system)
      (arguments '(#:with-zef? #f))
      (home-page "https://github.com/perlpilot/Grammar-Profiler-Simple")
      (synopsis "Simple rule profiling for Raku grammars")
      (description "This module provides a simple profiler for Raku grammars.
To enable profiling simply add use @code{Grammar::Profiler::Simple;} to your
code.  Any grammar in the lexical scope of the use statement will automatically
have profiling information collected when the grammar is used.")
      (license license:artistic2.0))))

(define-public perl6-json
  ;; The commit where 1.0 was “tagged”.
  (let ((commit "a5ef8c179350dae44ce7fb1abb684fc62c1c2b99"))
    (package
      (name "perl6-json")
      (version "1.0")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/moritz/json")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1kzryxkqyr129rcckd4jh0dfxdgzv71qx8dpkpm1divbfjyknlay"))))
      (build-system rakudo-build-system)
      (arguments '(#:with-zef? #f))
      (home-page "https://github.com/moritz/json")
      (synopsis "Minimal JSON (de)serializer")
      (description "This module is a simple Raku module for serializing and
deserializing JSON.")
      (license license:artistic2.0))))

(define-public perl6-json-class
  (package
    (name "perl6-json-class")
    (version "0.0.18")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jonathanstowe/JSON-Class")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0686qpcb6p5isjcsksrgsr1rh926cnhs6r091p5p3lh2fd575g1d"))))
    (build-system rakudo-build-system)
    (native-inputs
     (list perl6-json-fast))
    (propagated-inputs
     (list perl6-json-marshal perl6-json-unmarshal))
    (home-page "https://github.com/jonathanstowe/JSON-Class")
    (synopsis "Provide simple serialization/deserialization of objects to/from JSON")
    (description "This is a simple role that provides methods to instantiate a
class from a JSON string that (hopefully) represents it, and to serialize an
object of the class to a JSON string.  The JSON created from an instance
should round trip to a new instance with the same values for the public
attributes.  Private attributes (that is ones without accessors,) will be
ignored for both serialization and deserialization.  The exact behaviour
depends on that of @code{JSON::Marshal} and @code{JSON::Unmarshal}
respectively.")
    (license license:artistic2.0)))

(define-public perl6-json-fast
  (package
    (name "perl6-json-fast")
    (version "0.17")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/timo/json_fast")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1gckca9m6jkz6s59rpwf59721q6icfm45jj49sw3vdjr6jh2dgdb"))))
    (build-system rakudo-build-system)
    (arguments '(#:with-zef? #f))
    (home-page "https://github.com/timo/json_fast")
    (synopsis "Raku JSON parser")
    (description "A naive imperative JSON parser in pure Raku (but with direct
access to @code{nqp::} ops), to evaluate performance against @code{JSON::Tiny}.
It is a drop-in replacement for @code{JSON::Tiny}'s @code{from-json} and
@code{to-json} subs, but it offers a few extra features.")
    (license license:artistic2.0)))

(define-public perl6-json-marshal
  (package
    (name "perl6-json-marshal")
    (version "0.0.23")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jonathanstowe/JSON-Marshal")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "057xmdadhy5wkbhr6w1iybv2j7qf4bf8d4cp8m4qdcgk6pqzpkgc"))))
    (build-system rakudo-build-system)
    (native-inputs
     (list perl6-json-fast))
    (propagated-inputs
     (list perl6-json-fast perl6-json-name))
    (home-page "https://github.com/jonathanstowe/JSON-Marshal")
    (synopsis "Simple serialization of objects to JSON")
    (description "This library provides a single exported subroutine to create
a JSON representation of an object.  It should round trip back into an object
of the same class using @code{JSON::Unmarshal}.")
    (license license:artistic2.0)))

(define-public perl6-json-name
  (package
    (name "perl6-json-name")
    (version "0.0.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jonathanstowe/JSON-Name")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0fbwp3av4kh7zlbrmgbka4x6s3d6fsw2qxafi9hyq0y896wsaf94"))))
    (build-system rakudo-build-system)
    (arguments '(#:with-zef? #f))
    (propagated-inputs
     (list perl6-json-optin))
    (home-page "https://github.com/jonathanstowe/JSON-Name")
    (synopsis "Provide a trait to store an alternative JSON name")
    (description "This is released as a dependency of @code{JSON::Marshal} and
@code{JSON::Unmarshal} in order to save duplication, it is intended to store a
separate JSON name for an attribute where the name of the JSON attribute might be
changed, either for aesthetic reasons or the name is not a valid Perl identifier.
It will of course also be needed in classes thar are going to use
@code{JSON::Marshal} or @code{JSON::Unmarshal} for serialization/deserialization.")
    (license license:artistic2.0)))

(define-public perl6-json-optin
  (package
    (name "perl6-json-optin")
    (version "0.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jonathanstowe/JSON-OptIn")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0iz1wd3rfcb3kdc8cylwfxr8p02bpivk7kcf3dmm2l4mmdhyk7np"))))
    (build-system rakudo-build-system)
    (arguments '(#:with-zef? #f))
    (home-page "https://github.com/jonathanstowe/JSON-OptIn")
    (synopsis "Provide a trait and role to identify opt-in JSON serialization")
    (description "This provides a trait and a role for the identification of
`opt in' serializable attributes in, e.g., @code{JSON::Marshal}.  It's
probably not necessary to use this directly as it will be required by, e.g.,
@code{JSON::Name}, but it's just more convenient to package it separately.")
    (license license:artistic2.0)))

(define-public perl6-json-unmarshal
  ;; Last commit was May 2017.
  (let ((commit "e1b6288c5f3165058f36c0f4e171cdf2dfd640da")
        (revision "1"))
    (package
      (name "perl6-json-unmarshal")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/tadzik/JSON-Unmarshal")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "14azsmzmwdn8k0gqcpvballharcvzylmlyrx2wmv4kpqfnz29fjc"))))
      (build-system rakudo-build-system)
      (propagated-inputs
       (list perl6-json-fast perl6-json-name))
      (home-page "https://github.com/tadzik/JSON-Unmarshal")
      (synopsis "Make JSON from an Object")
      (description "This library provides a single exported subroutine to
create an object from a JSON representation of an object.")
      (license license:expat))))

(define-public perl6-license-spdx
  (package
    (name "perl6-license-spdx")
    (version "3.16.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jonathanstowe/License-SPDX")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0sv9rrrfn437crynmbs3aks5xcqchjkbr1k31fq0dphfaw88k5v3"))))
    (build-system rakudo-build-system)
    (propagated-inputs
     (list perl6-json-class))
    (home-page "https://github.com/jonathanstowe/License-SPDX")
    (synopsis "Abstraction over the SPDX License List")
    (description "This provides an abstraction over the SPDX License List as
provided in JSON format.  Its primary raison d'être is to help the licence
checking of @code{Test::META} and to allow for the warning about deprecated
licences therein.")
    (license license:artistic2.0)))

(define-public perl6-meta6
  (package
    (name "perl6-meta6")
    (version "0.0.26")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jonathanstowe/META6")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1jg8z0y29df9sy6za285jy38c3v6dl54ar54dp6marzcih02payx"))))
    (build-system rakudo-build-system)
    (native-inputs
     (list perl6-json-fast))
    (propagated-inputs
     (list perl6-json-class))
    (home-page "https://github.com/jonathanstowe/META6")
    (synopsis "Do things with Raku META files")
    (description "This provides a representation of the Raku META files
specification -- the META file data can be read, created, parsed and written
in a manner that is conformant with the specification.

Where they are known about, it also makes allowance for customary usage in
existing software (such as installers and so forth).

The intent of this is to allow the generation and testing of META files for
module authors, so it can provide meta-information about whether the
attributes are mandatory as per the spec and, where known, the places that
customary attributes are used.")
    (license license:artistic2.0)))

(define-public perl6-mime-base64
  (package
    (name "perl6-mime-base64")
    (version "1.2.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/raku-community-modules/MIME-Base64")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "19bndwbjr05gg8rrq2msmw1ik8xw2r3gbd77vxrp8l6c674zgy95"))))
    (build-system rakudo-build-system)
    (arguments '(#:with-zef? #f))
    (home-page "https://github.com/raku-community-modules/MIME-Base64")
    (synopsis "Encoding and decoding Base64 ASCII strings")
    (description "This Raku module implements encoding and decoding to and
from base64.")
    (license license:artistic2.0)))

(define-public perl6-oo-monitors
  (package
    (name "perl6-oo-monitors")
    (version "1.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jnthn/oo-monitors")
             (commit (string-append "release-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1aqb0x2vnmpi3knkf5v5aix6h29ln80gpxwa181i8kwfpxs8ir9f"))))
    (build-system rakudo-build-system)
    (arguments '(#:with-zef? #f))
    (home-page "https://github.com/jnthn/oo-monitors")
    (synopsis "Monitors with condition variables for Raku")
    (description "A monitor provides per-instance mutual exclusion for objects.
This means that for a given object instance, only one thread can ever be inside
its methods at a time.  This is achieved by a lock being associated with each
object.  The lock is acquired automatically at the entry to each method in the
monitor.  Condition variables are also supported.")
    (license license:artistic2.0)))

(define-public perl6-svg
  ;; Latest commit, basically unchanged since August 2015.
  (let ((commit "07190c0602aa276e5319f06aa0012452dbff3582")
        (revision "1"))
    (package
      (name "perl6-svg")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/moritz/svg")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0mkjdhg7ajksdn61n8fqhyzfd7ly9myazsvpsm02a5c2q73hdygg"))))
      (build-system rakudo-build-system)
      (propagated-inputs
       (list perl6-xml-writer))
      (home-page "https://github.com/moritz/svg")
      (synopsis "Raku module to generate SVG")
      (description "This is a Raku module that makes it easy to write
@dfn{Scalable Vector Graphic files} (SVG).  Right now it is a shallow wrapper
around @code{XML::Writer}, adding only the @samp{xmlns} attributes that
identify an XML file as SVG.")
      (license license:artistic2.0))))

(define-public perl6-svg-plot
  ;; Latest commit.
  (let ((commit "062570a78fd38c3c6baba29dfe2fbb8ca014f4de")
        (revision "1"))
    (package
      (name "perl6-svg-plot")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/moritz/svg-plot")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "095ga5hbg92jnmczxvhk1hjz14yr334zyf8cph4w5w5frcza44my"))))
      (build-system rakudo-build-system)
      (propagated-inputs
       (list perl6-svg))
      (home-page "https://github.com/moritz/svg-plot")
      (synopsis "Raku charting and plotting library that produces SVG output")
      (description "@code{SVG::Plot} is a simple 2D chart plotter for Raku.
It currently supports bars, stacked bars, lines and points (both equally spaced
with optional labels, or xy plots).")
      (license license:artistic2.0))))

(define-public perl6-tap-harness
  (package
    (name "perl6-tap-harness")
    (version "0.3.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Raku/tap-harness6")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "162crdy8g6xhnry26pjma2panm0c79n26igmljg79al4bqj9lyc9"))))
    (build-system rakudo-build-system)
    (arguments
     '(#:with-zef? #f
       #:with-prove6? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (apply invoke "raku" "-MTAP" "-Ilib" (find-files "t" "\\.t$")))))))
    (home-page "https://github.com/Raku/tap-harness6")
    (synopsis "TAP harness for Raku")
    (description "This module provides the @command{prove6} command which runs a
TAP based test suite and prints a report.  The @command{prove6} command is a
minimal wrapper around an instance of this module.")
    (license license:artistic2.0)))

(define-public perl6-terminal-ansicolor
  ;; The commit where 0.5 was “tagged”.
  (let ((commit "edded4a7116ce11cbc9fb5a83669c7ba119d0212"))
    (package
      (name "perl6-terminal-ansicolor")
      (version "0.5")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/tadzik/Terminal-ANSIColor")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1apm999azkyg5s35gid12wq019aqnvzrkz7qjmipd74mdxgr00x7"))))
      (build-system rakudo-build-system)
      (arguments '(#:with-zef? #f))
      (home-page "https://github.com/tadzik/Terminal-ANSIColor")
      (synopsis "Colorize terminal output")
      (description "This is a @code{Terminal::ANSIColor} module for Raku.")
      (license license:expat))))

(define-public perl6-test-meta
  (package
    (name "perl6-test-meta")
    (version "0.0.17")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jonathanstowe/Test-META")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0kd4yixx6lac240lcx0f5pfswspvpp43034mq2vs65zxghsvvcmf"))))
    (build-system rakudo-build-system)
    (propagated-inputs
     (list perl6-license-spdx perl6-meta6 perl6-uri))
    (home-page "https://github.com/jonathanstowe/Test-META")
    (synopsis "Test a distribution's META file")
    (description "This provides a simple mechanism for module authors to have
some confidence that they have a working distribution META description file.")
    (license license:artistic2.0)))

(define-public perl6-test-mock
  (let ((commit "1130427f4d9f3866fa39e113251bb142d7fa5cc8")
        (revision "0"))
    (package
      (name "perl6-test-mock")
      (version (git-version "1.5" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/jnthn/test-mock")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "03qq6z9ilw0ak3dx9i24ygr4c21wf4xvli8px02hmshpkhavclj5"))))
      (build-system rakudo-build-system)
      (propagated-inputs
       (list perl6-oo-monitors))
      (home-page "https://github.com/jnthn/test-mock")
      (synopsis "Module for simply generating and checking mock objects")
      (description "@code{Test::Mock} is a module that works alongside the
standard Test module to help you write tests when you want to verify what
methods are called on an object, while still having calls to undefined methods
die.  You get started just as normal with the test file, but also add a
@code{use} statement for @code{Test::Mock}.")
      (license license:artistic2.0))))

(define-public perl6-uri
  ;; The commit where 0.3.5 was “tagged”.
  (let ((commit "2e62d0f117c95a43e61ddb018dcacba8036aa3d5"))
    (package
      (name "perl6-uri")
      (version "0.3.5")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/raku-community-modules/URI")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1a03r80zxpvnsjf2z5r8lk434v2ysrpaymqa6xq975l4124gckkx"))))
      (build-system rakudo-build-system)
      (arguments '(#:with-zef? #f))
      (home-page "https://github.com/raku-community-modules/URI")
      (synopsis "URI implementation using Raku")
      (description "A URI implementation using Raku grammars to implement RFC
3986 BNF.  Currently only implements parsing.  Includes @code{URI::Escape} to
(un?)escape characters that aren't otherwise allowed in a URI with % and a hex
character numbering.")
      (license license:artistic2.0))))

(define-public perl6-xml-writer
  ;; Last commit was May 2017
  (let ((commit "4d30a9d8e06033ca97387971b653817becd5a759")
        (revision "1"))
    (package
      (name "perl6-xml-writer")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/masak/xml-writer")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1kwrf7akp70kyzw1b90khp71a6hpilihwndy2jsjpffcd4hd4m4z"))))
      (build-system rakudo-build-system)
      (arguments '(#:with-zef? #f))
      (home-page "https://github.com/masak/xml-writer")
      (synopsis "Raku module to generate XML")
      (description "@code{XML::Writer} is a module for creating XML in Raku.")
      (license license:artistic2.0))))

(define-public perl6-zef
  (package
    (name "perl6-zef")
    (version "0.13.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ugexe/zef")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0zmx2gavi975811kskv5j4j2zsysnxfklwx0azn8rffraadnhscm"))))
    (build-system rakudo-build-system)
    (arguments
     '(#:with-zef? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (setenv "HOME" "/tmp")
             (invoke "raku" "-I." "bin/zef" "--debug"
                     "--tap-harness" "test" "."))))))
    (home-page "https://github.com/ugexe/zef")
    (synopsis "Raku Module Management")
    (description "Zef is a Raku package (module) manager.  It can be used to
download and install Raku modules in your home directory or as a system-wide
module.")
    (license license:artistic2.0)))
