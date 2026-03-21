;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2026 Arun Isaac <arunisaac@systemreboot.net>
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

(define-module (gnu packages datalog)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages python)
  #:use-module (gnu packages sqlite)
  #:use-module (guix build-system cmake)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))

(define-public souffle
  (package
    (name "souffle")
    (version "2.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/souffle-lang/souffle")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1lrw69g02b17vxxz4g7cj8hbmc39wlzl80vl5fwy40a6b9pxwrsj"))
              (snippet
               #~(begin
                   ;; Delete bundled dependencies. souffle also uses a forked
                   ;; and modified version of json11. We cannot unbundle
                   ;; that.
                   (for-each delete-file
                             (list "licenses/TCBRINDLE SPAN.txt"
                                   "licenses/TINYFORMAT.txt"
                                   "src/include/souffle/utility/span.h"
                                   "src/include/souffle/utility/tinyformat.h"))))))
    (build-system cmake-build-system)
    (arguments
     (list #:configure-flags #~(list "-DSOUFFLE_GIT=OFF"
                                     (string-append "-DPACKAGE_VERSION="
                                                    #$version))
           ;; We disable these tests since they fail with gcc 14. This issue
           ;; has been reported upstream:
           ;; https://github.com/souffle-lang/souffle/issues/2576
           #:test-exclude (string-join
                           (list "syntactic/cpp_keywords_run_souffle"
                                 "syntactic/cpp_keywords_c_run_souffle"
                                 "syntactic/cpp_keywords_compare_std_outputs"
                                 "syntactic/cpp_keywords_compare_csv"
                                 "syntactic/cpp_keywords_c_compare_std_outputs"
                                 "syntactic/cpp_keywords_c_compare_csv")
                           "|")
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'replace-bundled-libraries
                 (lambda _
                   ;; Patch souffle to use tinyformat from Guix.
                   (substitute* (find-files "src" "\\.(cpp|h)$")
                     (("#include \"souffle/utility/tinyformat.h\"")
                      "#include <tinyformat.h>"))
                   ;; Replace span.h with a header that uses span from Guix
                   ;; and provides the same namespace.
                   (call-with-output-file "src/include/souffle/utility/span.h"
                     (lambda (port)
                       (display "#pragma once
#include <span.hpp>
namespace souffle {
  using tcb::span;
}
"
                                port))))))))
    (inputs
     (list libffi
           ncurses
           python
           span
           sqlite
           zlib))
    (native-inputs
     (list bison
           flex
           tinyformat))
    (home-page "https://souffle-lang.github.io/")
    (synopsis "Datalog variant")
    (description "Soufflé is a variant of Datalog for tool designers crafting
analyses in Horn clauses.  Soufflé synthesizes a native parallel C++ program
from a logic specification.  The Soufflé language is similar to Datalog (but
has terms known as records), and is frequently used as a domain-specific
language for analysis problems.")
    (license (list license:upl          ; main license
                   ;; src/include/souffle/profile/htmlJsChartistPlugin.h
                   ;; src/include/souffle/utility/json11.h
                   license:expat))))
