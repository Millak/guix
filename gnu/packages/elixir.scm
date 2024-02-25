;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016, 2017 Pjotr Prins <pjotr.guix@thebird.nl>
;;; Copyright © 2016 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017 nee <nee.git@cock.li>
;;; Copyright © 2018, 2019, 2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Nikita <nikita@n0.is>
;;; Copyright © 2021 Oskar Köök <oskar@maatriks.ee>
;;; Copyright © 2021 Cees de Groot <cg@evrl.com>
;;; Copyright © 2024 Andrew Tropin <andrew@trop.in>
;;; Copyright © 2024 Ivan Sokolov <ivan-p-sokolov@ya.ru>
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

(define-module (gnu packages elixir)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system gnu)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages erlang)
  #:use-module (gnu packages version-control))

(define-public elixir
  (package
    (name "elixir")
    (version "1.14.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/elixir-lang/elixir")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "16rc4qaykddda6ax5f8zw70yhapgwraqbgx5gp3f40dvfax3d51l"))
       (patches (search-patches "elixir-path-length.patch"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:test-target "test"
      #:parallel-tests? #f ;see <https://debbugs.gnu.org/cgi/bugreport.cgi?bug=32171#23>
      #:make-flags #~(list (string-append "PREFIX=" #$output))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'make-git-checkout-writable
            (lambda _
              (for-each make-file-writable (find-files "."))))
          (add-after 'make-git-checkout-writable 'replace-paths
            (lambda* (#:key inputs #:allow-other-keys)
              ;; Note: references end up obfuscated in binary BEAM files where
              ;; they may be invisible to the GC and graft code:
              ;; <https://issues.guix.gnu.org/54304#11>.
              (substitute* '("lib/mix/lib/mix/release.ex"
                             "lib/mix/lib/mix/tasks/release.init.ex")
                (("#!/bin/sh")
                 (string-append "#!" (search-input-file inputs "/bin/sh"))))
              (substitute* "bin/elixir"
                (("ERTS_BIN=\n")
                 (string-append
                  "ERTS_BIN="
                  ;; Elixir Releases will prepend to ERTS_BIN the path of
                  ;; a copy of erl.  We detect if a release is being generated
                  ;; by checking the initial ERTS_BIN value: if it's empty, we
                  ;; are not in release mode and can point to the actual erl
                  ;; binary in Guix store.
                  "\nif [ -z \"$ERTS_BIN\" ]; then ERTS_BIN="
                  (string-drop-right (search-input-file inputs "/bin/erl") 3)
                  "; fi\n")))
              (substitute* "bin/mix"
                (("#!/usr/bin/env elixir")
                 (string-append "#!" #$output "/bin/elixir")))))
          (add-before 'build 'make-current
            ;; The Elixir compiler checks whether or not to compile files by
            ;; inspecting their timestamps.  When the timestamp is equal to the
            ;; epoch no compilation will be performed.  Some tests fail when
            ;; files are older than Jan 1, 2000.
            (lambda _
              (for-each (lambda (file)
                          (let ((recent 1400000000))
                            (utime file recent recent 0 0)))
                        (find-files "." ".*"))))
          (add-before 'check 'set-home
            (lambda* (#:key inputs #:allow-other-keys)
              ;; Some tests require access to a home directory.
              (setenv "HOME" "/tmp")))
          (delete 'configure)
          (add-after 'install 'wrap-programs
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (programs '("elixir" "elixirc" "iex")))
                ;; mix can be sourced as an elixir script by other elixir
                ;; program, for example `iex -S mix`, so we should not wrap
                ;; mix into shell script.
                (substitute* (string-append out "/bin/mix")
                  (("Mix.start\\(\\)")
                   (format #f "\
~~w[GUIX_ELIXIR_LIBS ERL_LIBS]
|> Enum.map(&System.get_env/1)
|> Enum.reject(&is_nil/1)
|> Enum.join(\":\")
|> case do \"\" -> :ok; erl_libs -> System.put_env(\"ERL_LIBS\", erl_libs) end
System.put_env(\"MIX_REBAR3\", System.get_env(\"MIX_REBAR3\", \"~a\"))
Mix.start()"
                           (search-input-file inputs "/bin/rebar3"))))
                (for-each (lambda (program)
                            (wrap-program (string-append out "/bin/" program)
                              '("ERL_LIBS" prefix ("${GUIX_ELIXIR_LIBS}"))))
                          programs)))))))
    (inputs (list erlang rebar3 git))
    (native-search-paths
     (list (search-path-specification
            (variable "GUIX_ELIXIR_LIBS")
            (files (list (string-append "lib/elixir/" (version-major+minor version)))))))
    (home-page "https://elixir-lang.org/")
    (synopsis "Elixir programming language")
    (description "Elixir is a dynamic, functional language used to build
scalable and maintainable applications.  Elixir leverages the Erlang VM, known
for running low-latency, distributed and fault-tolerant systems, while also
being successfully used in web development and the embedded software domain.")
    (license license:asl2.0)))

(define-public elixir-hex
  (package
    (name "elixir-hex")
    (version "2.0.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hexpm/hex.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1kvczwvij58kgkhak68004ap81pl26600bczg21mymy2sypkgxmj"))))
    ;; The mix-build-system assumes that Hex exists.
    ;; We build Hex using the gnu-build-system.
    ;; Other Elixir packages use the mix-build-system.
    (build-system gnu-build-system)
    (inputs (list elixir))
    (arguments
     (list
      ;; Hex is needed to build packages used to test Hex.
      ;; To avoid this circularity, we disable tests.
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'bootstrap)
          (delete 'configure)
          (replace 'build
            (lambda* (#:key inputs #:allow-other-keys)
              (setenv "MIX_ENV" "prod")
              (invoke "mix" "compile")))
          (replace 'install
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (define X.Y #$(version-major+minor (package-version elixir)))
              (define out (string-append (assoc-ref outputs "out") "/lib/elixir/" X.Y "/hex"))
              (mkdir-p out)
              (copy-recursively "_build/prod/lib/hex" out))))))
    (synopsis "Package manager for the Erlang VM")
    (description
     "This project provides tasks that integrate with Mix, Elixir's build
tool.")
    (home-page "https://hexdocs.pm/makeup_elixir/")
    (license license:bsd-2)))
