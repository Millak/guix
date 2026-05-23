;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2026 Daniel Patrick Fahey <dpf@helmcontrol.ltd>
;;; Copyright © 2026 Florian Pelz <pelzflorian@pelzflorian.de>
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

(define-module (gnu packages opencog)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages language)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages tls)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix utils))

(define-public cogutil
  ;; The last release was in 2016.  Other OpenCog packages require a later
  ;; version.
  (let ((commit "64dca9083dcfa485dcb70fd6fe7ba1f80e0f4082")
        (revision "2"))
    (package
      (name "cogutil")
      (version (git-version "2.0.3" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/opencog/cogutil")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "01rr39sq9wakw1wnfzg7m4zl7g8qh3d35ww6q39irx19d6nq66h9"))))
      (build-system cmake-build-system)
      (arguments
       (list
        ;; Skip ldconfig, which requires root and is unnecessary in Guix.
        #:configure-flags
        #~(list "-DSKIP_LDCONF=ON")
        #:modules '((guix build cmake-build-system)
                    ((guix build gnu-build-system) #:prefix gnu:)
                    (guix build utils))
        #:phases
        #~(modify-phases %standard-phases
            (replace 'check
              (lambda* (#:key tests? #:allow-other-keys #:rest args)
                (when tests?
                  (apply (assoc-ref gnu:%standard-phases 'check)
                         #:tests? tests? #:test-target "tests" args)
                  (for-each
                   (lambda (file)
                     (invoke file))
                   (find-files "tests" "UTest$"))))))))
      (inputs
       (list boost))
      (native-inputs
       `(("cxxtest" ,cxxtest)
         ("python" ,python-minimal)
         ("pkg-config" ,pkg-config)))
      (home-page "https://github.com/opencog/cogutil/")
      (synopsis "Low-level C++ programming utilities used by OpenCog components")
      (description "The OpenCog utilities is a miscellaneous collection of C++
utilities use for typical programming tasks in multiple OpenCog projects.")
      ;; Either of these licenses.
      (license (list license:agpl3 license:asl2.0)))))

(define-public atomspace
  ;; The last release was in 2016.
  (let ((commit "c8d633bf272b838c2132c21b7c8ddf169a18dd22")
        (revision "2"))
    (package
      (name "atomspace")
      (version (git-version "5.0.3" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/opencog/atomspace")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0fdqmskjh9d7363ldwmh3zhwal847shm15nhb2nan4g4aqdmckrz"))))
      (build-system cmake-build-system)
      (arguments
       (list
        #:configure-flags
        #~(list "-DSKIP_LDCONF=ON"
                (string-append "-DGUILE_INCLUDE_DIR=" #$guile-3.0.11
                               "/include/guile/3.0/")
                (string-append "-DGUILE_SITE_DIR=" #$output
                               "/share/guile/site/3.0/")
                (string-append "-DGUILE_CCACHE_DIR=" #$output
                               "/lib/guile/3.0/site-ccache")
                (string-append "-DPYTHON_INSTALL_PREFIX=" #$output
                               "/lib/python"
                               #$(version-major+minor (package-version python))
                               "/site-packages"))
        #:modules '((guix build cmake-build-system)
                    ((guix build gnu-build-system) #:prefix gnu:)
                    (guix build utils))
        #:phases
        #~(modify-phases %standard-phases
            (replace 'check
              (lambda* (#:key tests? #:allow-other-keys #:rest args)
                (when tests?
                  (apply (assoc-ref gnu:%standard-phases 'check)
                         #:tests? tests? #:test-target "tests" args)
                  ;; Failing tests.
                  (for-each delete-file
                            '("tests/scm/MultiAtomSpaceUTest"))
                  (setenv "GUILE_LOAD_PATH" ".:opencog/scm")
                  (for-each invoke
                            (find-files "tests" "UTest$"))))))))
      (inputs
       (list boost cogutil gmp guile-3.0-latest postgresql))
      (native-inputs
       `(("cxxtest" ,cxxtest)
         ("python-cython" ,python-cython)
         ("python" ,python-minimal)
         ("pkg-config" ,pkg-config)))
      (home-page "https://github.com/opencog/atomspace/")
      (synopsis "OpenCog hypergraph database, query system and rule engine")
      (description "The OpenCog AtomSpace is an in-RAM @dfn{knowledge
representation} (KR) database, an associated query engine and graph-re-writing
system, and a rule-driven inferencing engine that can apply and manipulate
sequences of rules to perform reasoning.  It is a layer that sits on top of
ordinary distributed (graph) databases, providing a large variety of advanced
features not otherwise available.")
      (license license:agpl3))))

(define-public atomspace-storage
  (let ((commit "ecd88d673258e51f65cc0605400003affdf4d694")
        (revision "1"))
    (package
      (name "atomspace-storage")
      (version (git-version "4.3.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/opencog/atomspace-storage")
               (commit commit)))
         (sha256
          (base32 "12ssnc313cm5n8bc6zw53dkjm9crrry90lpsknzxafnm61mzcgx3"))
         (file-name (git-file-name name version))))
      (build-system cmake-build-system)
      (arguments
       (list
        #:configure-flags
        #~(map (lambda (strings)
                 (apply string-append strings))
               `(("-DGUILE_LIBRARIES=" ,(dirname (search-input-file
                                                  %build-inputs
                                                  "lib/libguile-3.0.so")))
                 ("-DGUILE_INCLUDE_DIRS=" ,(search-input-directory
                                            %build-inputs "include/guile/3.0"))
                 ("-DGUILE_SITE_DIR=" ,#$output "/share/guile/site/3.0/")
                 ("-DPYTHON_INSTALL_PREFIX=" ,#$output "/lib/python"
                  ,#$(version-major+minor (package-version (this-package-input
                                                            "python-minimal")))
                  "/site-packages/")))
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'build 'build-tests
              (lambda* (#:key make-flags #:allow-other-keys)
                (begin
                  (apply invoke "make" "tests" make-flags)))))))
      (inputs (list atomspace cogutil gmp guile-3.0-latest python-minimal))
      (native-inputs (list cxxtest pkg-config python-cython))
      (home-page "https://github.com/opencog/atomspace-storage")
      (synopsis "StorageNode API for AtomSpace")
      (description
       "This AtomSpace module contains the StorageNode base class.  The
StorageNode API is the original, primary API for I/O storage and movement
of Atoms into, out of and between AtomSpaces.  It is mandatory for the
RocksStorageNode which stores Atoms and AtomSpaces to disk, via RocksDB, and
to CogStorageNode, which moves Atoms between AtomSpaces on different network
nodes, and ProxyNodes which provides mirroring, routing, caching and filtering
between AtomSpaces.")
      (license license:agpl3))))

(define-public cogserver
  ;; There are no releases.
  (let ((commit "3c4da0bd786262bd69a8306f9da88b13f0add1f2")
        (revision "3"))
    (package
      (name "cogserver")
      (version (git-version "0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/opencog/cogserver")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0hfz9z44wpzrabs9fgxhv02jzn0i40di2ncbiizn03g7gccawds5"))))
      (build-system cmake-build-system)
      (arguments
       (list
        #:tests? #f ;tests start network server and hang
        #:configure-flags
        #~(list "-DSKIP_LDCONF=ON"
                (string-append "-DGUILE_INCLUDE_DIR=" #$guile-3.0.11
                               "/include/guile/3.0/")
                (string-append "-DGUILE_SITE_DIR=" #$output
                               "/share/guile/site/3.0/")
                (string-append "-DGUILE_CCACHE_DIR=" #$output
                               "/lib/guile/3.0/site-ccache")
                (string-append "-DPYTHON_INSTALL_PREFIX=" #$output
                               "/lib/python"
                               #$(version-major+minor (package-version python))
                               "/site-packages"))
        #:modules '((guix build cmake-build-system)
                    ((guix build gnu-build-system) #:prefix gnu:)
                    (guix build utils))
        #:phases
        #~(modify-phases %standard-phases
            (replace 'check
              (lambda* (#:key tests? #:allow-other-keys #:rest args)
                (when tests?
                  (apply (assoc-ref gnu:%standard-phases 'check)
                         #:tests? tests? #:test-target "tests" args)
                  ;; Failing test.
                  (delete-file "tests/shell/ShellUTest")
                  (for-each invoke
                            (find-files "tests" "UTest$"))))))))
      (inputs
       (list asio atomspace atomspace-storage boost cogutil
             gmp guile-3.0-latest jsoncpp openssl))
      (native-inputs
       `(("cxxtest" ,cxxtest)
         ("netcat" ,netcat-openbsd)
         ("python" ,python)
         ("python-cython" ,python-cython)
         ("pkg-config" ,pkg-config)))
      (home-page "https://github.com/opencog/cogserver/")
      (synopsis "OpenCog network server")
      (description "The OpenCog Cogserver is a network and job server for the
OpenCog framework.")
      (license license:agpl3))))


(define-public agi-bio
  ;; There are no releases.
  (let ((commit "2f723ad79afc38df9dce73d340ee24c38efb2199")
        (revision "2"))
    (package
      (name "agi-bio")
      (version (git-version "0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/opencog/agi-bio")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1gqmwjbqdjr0qz4wpsxh0k8wam59fvd3pnjxhfswdpvapcl9syk0"))))
      (build-system cmake-build-system)
      (arguments
       `(#:tests? #f ; there are none
         #:configure-flags
         (list
          (string-append "-DGUILE_INCLUDE_DIR="
                         (assoc-ref %build-inputs "guile")
                         "/include/guile/3.0/")
          (string-append "-DGUILE_SITE_DIR="
                         (assoc-ref %outputs "out")
                         "/share/guile/site/3.0/"))
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-unqualified-load
             (lambda* (#:key outputs #:allow-other-keys)
               (substitute* "bioscience/bioscience.scm"
                 (("\\(load \"bioscience/types/bioscience_types.scm\"\\)")
                  (format #f "(load \"~a/bioscience/types/bioscience_types.scm\")"
                          (string-append (assoc-ref outputs "out")
                                         "/share/guile/site/3.0/opencog")))))))))
      (inputs
       (list atomspace cogutil gmp guile-3.0-latest))
      (native-inputs
       `(("cxxtest" ,cxxtest)
         ("python" ,python-minimal)
         ("pkg-config" ,pkg-config)))
      (home-page "https://github.com/opencog/agi-bio")
      (synopsis "Genomic and proteomic data exploration and pattern mining")
      (description "This is a package for genomic and proteomic research using
the OpenCog toolset with Guile.  This includes experiments in applying pattern
mining and other OpenCog components.")
      (license license:agpl3))))
