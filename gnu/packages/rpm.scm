;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021, 2022 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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

(define-module (gnu packages rpm)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system meson)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages man)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages popt)
  #:use-module (gnu packages python)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xml))

(define-public drpm
  (package
    (name "drpm")
    (version "0.5.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/rpm-software-management/drpm")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0czg69crs2idnd8zsr7p01cd02f981gb5hq15m3qkwd25zxxd1l0"))))
    (build-system cmake-build-system)
    (native-inputs
     (list cmocka pkg-config))
    (inputs
     (list bzip2
           lzlib
           openssl
           popt
           rpm
           xz
           zlib
           `(,zstd "lib")))
    (home-page "https://github.com/rpm-software-management/drpm")
    (synopsis "Delta RPM library")
    (description "This package provides a library for making, reading and
applying deltarpms, compatible with the original deltarpm packages.")
    (license license:lgpl2.1+)))

(define-public libmodulemd
  (package
    (name "libmodulemd")
    (version "2.13.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/fedora-modularity/"
                                  "libmodulemd/releases/download/" version
                                  "/modulemd-" version ".tar.xz"))
              (sha256
               (base32
                "1g4wizr2wwl5x77ni5j46nfcax8fbb7nqq5nr7va9sccyigwwwnc"))))
    (build-system meson-build-system)
    (outputs '("out" "doc"))            ;2.6 MiB of HTML documentation
    (arguments
     (list
      #:configure-flags
      #~(list (string-append "-Dgobject_overrides_dir_py3="
                             (python:site-packages %build-inputs %outputs)))
      #:imported-modules `(,@%meson-build-system-modules
                           (guix build python-build-system))
      #:modules '((guix build meson-build-system)
                  ((guix build python-build-system) #:prefix python:)
                  (guix build utils))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-glib-doc-prefix
            (lambda* (#:key native-inputs inputs #:allow-other-keys)
              (substitute* "meson.build"
                (("glib_docpath = .*")
                 (format #f "glib_docpath = '~a'~%"
                         (search-input-directory (or native-inputs inputs)
                                                 "share/gtk-doc/html"))))))
          (add-after 'unpack 'fix-docbook-references
            ;; gtk-doc doesn't seem to honor DocBook 4.1.2's docbook.cat's
            ;; catalog file, even when adding it to XML_CATALOG_FILES.  Work
            ;; around it by adjusting the DocBook references directly.
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "modulemd/modulemd-docs.xml"
                (("http://www.oasis-open.org/docbook/xml/4.1.2/docbookx.dtd")
                 (search-input-file inputs "xml/dtd/docbook/docbookx.dtd")))))
          (add-after 'install 'move-documentation
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((dst (string-append #$output:doc "/share/gtk-doc")))
                (mkdir-p (dirname dst))
                (rename-file (search-input-directory outputs "share/gtk-doc")
                             dst)))))))
    (native-inputs
     (list docbook-xml-4.1.2
           `(,glib "bin")
           `(,glib-with-documentation "doc")
           gobject-introspection        ;for g-ir-scanner
           gtk-doc
           help2man
           pkg-config
           python))                     ;for 'site-packages' call
    (inputs
     (list gtk+
           python-pygobject))
    (propagated-inputs
     ;; glib and gobject are listed as 'Requires' in modulemd-2.0.pc.
     (list glib
           ;; yaml and rpm are listed as 'Requires.private' in modulemd-2.0.pc.
           libyaml rpm))
    (home-page "https://github.com/fedora-modularity/libmodulemd")
    (synopsis "C library for manipulating module metadata files")
    (description "The libmodulemd API provides a number of convenience tools
for interacting with repodata (that is, streams of YAML that contains
information on multiple streams, default data and translations).")
    (license license:expat)))

(define-public createrepo-c
  (package
    (name "createrepo-c")
    (version "0.20.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/rpm-software-management/createrepo_c")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "07d89s6kknf79phzan3d2vy1rq64ih49qk2w51hsmfn73qins9wy"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:imported-modules `(,@%cmake-build-system-modules
                           (guix build python-build-system))
      #:modules '((guix build cmake-build-system)
                  ((guix build python-build-system) #:prefix python:)
                  (guix build utils))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-python-site-prefix
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (substitute* "src/python/CMakeLists.txt"
                (("EXECUTE_PROCESS.*OUTPUT_VARIABLE PYTHON_INSTALL_DIR.*")
                 (format #f "set (PYTHON_INSTALL_DIR ~a)~%"
                         (python:site-packages inputs outputs))))))
          (add-after 'unpack 'fix-bash-completion-prefix
            (lambda _
              (substitute* "CMakeLists.txt"
                (("execute_process.*OUTPUT_VARIABLE BASHCOMP_DIR.*")
                 (format #f "set (BASHCOMP_DIR ~a\
/share/bash-completion/completions)~%" #$output))))))))
    (native-inputs
     (list bash-completion pkg-config python))
    (inputs
     (list bzip2
           curl
           drpm
           libmodulemd
           libxml2
           openssl
           sqlite
           zchunk))
    (home-page "https://rpm-software-management.github.io/createrepo_c/")
    (synopsis "C implementation of the createrepo tool")
    (description "This package provides the @command{createrepo} command,
which can be used to create RPM repositories.")
    (license license:gpl2+)))
