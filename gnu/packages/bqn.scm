;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2022 Christopher Rodriguez <yewscion@gmail.com>
;;; Copyright © 2022 Liliana Marie Prikler <liliana.prikler@gmail.com>
;;; Copyright © 2025 Lee Thompson <lee.p.thomp@gmail.com>
;;; © 2025 case_lambda <case_lambda@disroot.org>
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

(define-module (gnu packages bqn)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages base)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages java)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages game-development))

(define-public dbqn
  (package
    (name "dbqn")
    (version "0.2.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dzaima/BQN")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1zy3y9wbmaw0mrd2sp7d1r912gvs9k0mzw5d3drgmbzkbvpd6iq1"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:imported-modules `((guix build ant-build-system)
                           ,@%default-gnu-imported-modules)
      #:modules '((guix build gnu-build-system)
                  ((guix build ant-build-system)
                   #:prefix ant:)
                  (guix build utils))
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (replace 'build
            (lambda _
              (invoke "./build")))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (if tests?
                  (invoke "./BQN" "./test/test")
                  (begin
                    (display "test suite not run")
                    (newline)))))
          (add-after 'install 'reorder-jar-content
            (assoc-ref ant:%standard-phases
                       'reorder-jar-content))
          (add-after 'reorder-jar-content 'generate-jar-indices
            (assoc-ref ant:%standard-phases
                       'generate-jar-indices))
          (add-after 'generate-jar-indices 'strip-jar-timestamps
            (assoc-ref ant:%standard-phases
                       'strip-jar-timestamps))
          (replace 'install
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((dest-jar (string-append #$output "/share/java")))
                (rename-file "BQN" "dbqn")
                (substitute* "dbqn"
                  (("java")
                   (search-input-file inputs "/bin/java"))
                  (("BQN.jar")
                   (string-append dest-jar "/BQN.jar")))
                (install-file "BQN.jar" dest-jar)
                (install-file "dbqn"
                              (string-append #$output "/bin"))))))))
    (native-inputs (list `(,icedtea-8 "jdk") zip))
    (inputs (list bash-minimal icedtea-8))
    (home-page "https://github.com/dzaima/BQN")
    (synopsis "BQN implementation based on dzaima/APL")
    (description
     "dbqn is a Java implementation of the
@uref{https://mlochbaum.github.io/BQN/, BQN programming language} that does
not need to be bootstrapped, based on an earlier Java implementation of APL by
the same author.")
    (license license:expat)))

(define bqn-sources
  ;; Aside from dbqn above, the main bqn repository is used by other
  ;; implementations as a "known good" set of sources.
    (let ((commit "3d8f6876d9fb2a055e164137ab058ba70bffda1c"))
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/mlochbaum/BQN")
              (commit commit)))
        (file-name (git-file-name "bqn-sources" commit))
        (sha256
         (base32 "1znxdn6k7xq85z1hz63syx540lwl6ak6zjikwdss01wbya5vwvp3")))))

(define %cbqn-version "0.12.0")

(define cbqn-sources
  (let ((version %cbqn-version))
    (origin
      (method git-fetch)
      (uri (git-reference
            (url "https://github.com/dzaima/CBQN")
            (commit (string-append "v" version))))
      (file-name (git-file-name "cbqn" version))
      (sha256
       (base32 "1rgvmdzmdvnf1ivf0x7hxdahr7dcf1vf01n6iphfks8336xal2s1")))))

(define %replxx-commit "5e3bd870699007b9536d29f60e3a2b0a68ce0a7a")

(define replxx-sources
  (let ((commit %replxx-commit))
    (origin
      (method git-fetch)
      (uri
       (git-reference
         (url "https://github.com/dzaima/replxx")
         (commit commit)))
      (file-name (git-file-name "replxx" commit))
      (sha256
       (base32 "0va3in0aswmvs49plkw5rlnd898j4lvw3y5mjl76s4xqj9vpalq7")))))

(define %singeli-commit "5187ed3aa22a2257b63c04a5498ec756fa6263cb")

(define singeli-sources
  (let ((commit %singeli-commit))
    (origin
      (method git-fetch)
      (uri
       (git-reference
         (url "https://github.com/mlochbaum/Singeli")
         (commit commit)))
      (file-name (git-file-name "singeli" commit))
      (sha256
       (base32 "1qal8yvcm346sg4bwnc4bmx4v1fsh0x1mdm1jrqvc4gidwlrgl7f")))))

(define (cbqn-make-flags . goals)
  #~(list (string-append "CC=" #$(cc-for-target))
          (string-append "version=" #$%cbqn-version)
          "nogit=1"
          (string-append "j=" (number->string (parallel-job-count)))
          #$@goals))

(define cbqn-bootstrap
  (package
    (name "cbqn-bootstrap")
    (version %cbqn-version)
    (source cbqn-sources)
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f ;skipping tests for bootstrap
      ;; `make for-bootstrap' implicitly disables REPLXX and Singeli.
      #:make-flags (cbqn-make-flags "for-bootstrap")
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (mkdir-p (string-append #$output "/bin"))
              (chmod "BQN" #o755)
              (rename-file "BQN" "bqn")
              (install-file "bqn"
                            (string-append #$output "/bin")))))))
    (inputs (list libffi))
    (home-page "https://mlochbaum.github.io/BQN/")
    (synopsis "BQN implementation in C")
    (description
     "This package provides the reference implementation of
@uref{https://mlochbaum.github.io/BQN/, BQN}, a programming language inspired
by APL.")
    ;; Upstream explains licensing situation
    (license (list license:asl2.0       ;src/utils/ryu*
                   license:boost1.0
                   license:expat        ;src/builtins/sortTemplate.h
                   license:lgpl3+       ;Everything else except the above
                   license:gpl3+
                   license:mpl2.0))))

(define* (cbqn-combined-source name version #:key cbqn-sources replxx-sources singeli-sources)
  (origin
    (method (@@ (guix packages) computed-origin-method))
    (file-name (string-append name "-" version ".tar.gz"))
    (sha256 #f)
    (uri
     (delay
       (with-imported-modules '((guix build utils))
         #~(begin
             (use-modules (guix build utils))
             (let* ((dir (string-append "cbqn" #$version))
                    (replxx-local-dir "build/replxxLocal")
                    (singeli-local-dir "build/singeliLocal"))

               (set-path-environment-variable
                "PATH" '("bin")
                (list #+(canonical-package bash)
                      #+(canonical-package coreutils)
                      #+(canonical-package gzip)
                      #+(canonical-package tar)))
               (copy-recursively #+cbqn-sources dir)

               (with-directory-excursion dir
                 ;; Copy in local replxx and Singeli code
                 (mkdir replxx-local-dir)
                 (mkdir singeli-local-dir)
                 (copy-recursively #+replxx-sources replxx-local-dir)
                 (copy-recursively #+singeli-sources singeli-local-dir))

               (invoke "tar" "cvfa" #$output
                       "--mtime=@0"
                       "--owner=root:0"
                       "--group=root:0"
                       "--sort=name"
                       "--hard-dereference"
                       dir))))))))

(define-public cbqn
  (package
    (inherit cbqn-bootstrap)
    (name "cbqn")
    (outputs '("out" "lib"))
    (source
     (cbqn-combined-source name
                           %cbqn-version
                           #:cbqn-sources cbqn-sources
                           #:replxx-sources replxx-sources
                           #:singeli-sources singeli-sources))
    (arguments
     (list
      ;; The CBQN makefile dispatches to build/src/build.bqn, which
      ;; parallelizes internally via `j='.
      #:parallel-build? #f
      #:make-flags (cbqn-make-flags "for-build" "shared-o3" "o3")
      #:modules '((guix build utils)
                  (guix build gnu-build-system)
                  ((ice-9 regex)
                   #:select (match:substring))
                  ((srfi srfi-1)
                   #:select (any)))
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          ;; Build bytecode using bootstrap CBQN
          (add-before 'build 'generate-bytecode
            (lambda* (#:key inputs #:allow-other-keys)
              (let* ((self-hosted-source (dirname (search-input-file inputs
                                                   "bqn.bqn"))))
                (mkdir-p "build/bytecodeLocal/gen")
                (invoke "bqn" "build/bootstrap.bqn" self-hosted-source))))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                ;; The reference test suite of the BQN language.
                (invoke "./BQN" "-M" "1000"
                        (string-append #+bqn-sources "/test/this.bqn"))
                ;; Run tests applicable to release builds.  Exclude tests
                ;; requiring “-DTEST_UTILS” (disabled for release builds),
                ;; usz32 (nonstandard 32-bit integer array length), and tests
                ;; for unsupported platforms and build configurations.
                (let* ((excluded-files '("test/cases/fuzz/bitarr-fill.bqn"
                                         "test/cases/fuzz/bitcpy.bqn"
                                         "test/cases/proper-fills.bqn"
                                         "test/cases/usz32.bqn"))
                       (excluded-directories '("test/cases/build-specific/"
                                               "test/cases/unnecessary/"
                                               "test/cases/windows/"))
                       (test-pred
                        (lambda (file _)
                          (and (string-suffix? ".bqn" file)
                               (not (member file excluded-files))
                               (not (any (lambda (directory)
                                           (string-prefix? directory file))
                                         excluded-directories)))))
                       (test-regexp (make-regexp "^test/cases/(.*)\\.bqn$")))
                  (apply invoke "./BQN" "test/run.bqn" "bin=./BQN"
                         (map
                          (lambda (file)
                            ;; Remove the directory and extension for the CLI.
                            (match:substring (regexp-exec test-regexp file)
                                             1))
                          (find-files "test/cases" test-pred))))
                (invoke "make" "-C" "test/ffi" "test"
                        (string-append "CC="
                                       #$(cc-for-target))))))
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((bin (string-append (assoc-ref outputs "out") "/bin"))
                     (lib (string-append (assoc-ref outputs "lib") "/lib"))
                     (include (string-append (assoc-ref outputs "lib")
                                             "/include")))
                (rename-file "BQN" "bqn")
                (install-file "bqn" bin)
                (install-file "libcbqn.so" lib)
                (install-file "include/bqnffi.h" include)))))))
    (native-inputs (list cbqn-bootstrap libffi))
    (inputs (modify-inputs inputs
              (prepend bqn-sources)))
    (license (append (package-license cbqn-bootstrap)
                     (list license:isc ;Singeli module
                           license:bsd-3 ;REPLXX module
                           license:unicode)))
    (properties `((tunable? . #t)))))

(define-public bqn-libs
  (let ((commit "32f07809431af37e60ca1b6faeb9315b871f4a5d")
        (revision "1"))
    (package
      (name "bqn-libs")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/mlochbaum/bqn-libs")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0a6r3hqbqp8pccjsw3ck7pv1f0akcpb8iag727j1lr0vpvd1i5qh"))))
      (build-system copy-build-system)
      (arguments
       (list
        #:phases
        #~(modify-phases %standard-phases
            (add-before 'install 'check
              (lambda* (#:key tests? #:allow-other-keys)
                (when tests?
                  (invoke "bqn" "test/main.bqn")))))
        #:install-plan
        #~'(("." "share/bqn-libs/"
             #:include-regexp ("\\.bqn$"))
            ("README.md" "share/doc/bqn-libs/")
            ("LICENSE" "share/licenses/bqn-libs/"))))
      (native-inputs (list cbqn))
      (home-page "https://github.com/mlochbaum/bqn-libs")
      (synopsis "Informal collection of BQN utilities")
      (description
       "This package provides an informal collection of BQN utility scripts,
covering string handling, JSON, CSV and XML parsing, hash maps, prime
sieves, matrix and polynomial operations, arbitrary-precision arithmetic,
and date and time handling.  Upstream considers these scripts experimental
and subject to change.")
      (license license:bsd-0))))

(define-public bqn-raylib
  (let ((commit "b4a397578c874d4585f4be1ad8ffd7be05a4e169")
        (revision "1"))
    (package
      (name "bqn-raylib")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Brian-ED/raylib-bqn")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "00imjdj7170wbkg0qy7x86gfar4ycdd35hz3k2gm2jf9yaprihbi"))))
      (build-system copy-build-system)
      (arguments
       (list
        #:phases
        #~(modify-phases %standard-phases
            (add-before 'install 'patch-raylib-path
              (lambda* (#:key inputs #:allow-other-keys)
                (let ((libraylib (search-input-file inputs "/lib/libraylib.so")))
                  (substitute* "raylib.bqn"
                    (("#INSERT-CUSTOM-RAYLIB-PATH-HERE")
                     (string-append "\"" libraylib "\"")))))))
        #:install-plan
        #~'(("raylib.bqn" "share/bqn-raylib/")
            ("README.md" "share/doc/bqn-raylib/")
            ("LICENSE" "share/licenses/bqn-raylib/"))))
      (inputs (list raylib))
      (home-page "https://github.com/Brian-ED/raylib-bqn")
      (synopsis "Raylib bindings for BQN")
      (description
       "This package provides autogenerated BQN bindings for the raylib C library.")
      (license license:expat))))
