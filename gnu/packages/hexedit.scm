;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2017 Gábor Boskovits <boskovits@gmail.com>
;;; Copyright © 2018, 2020, 2022 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2024 Artyom V. Poptsov <poptsov.artyom@gmail.com>
;;; Copyright © 2025 Sergio Pastor Pérez <sergio.pastorperez@gmail.com>
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

(define-module (gnu packages hexedit)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages man)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages python)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages antivirus)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages engineering)
  #:use-module (gnu packages pretty-print)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake))

(define-public hexedit
  (package
    (name "hexedit")
    (version "1.6")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/pixel/hexedit")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "00l8vazbjzdg09azp6w3nzq4rl7qyh06i65dh621r6zaprp0z23w"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no check target
       #:phases
       (modify-phases %standard-phases
         ;; Make F1 open the man page even if man-db is not in the profile.
         (add-after 'unpack 'patch-man-path
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (substitute* "interact.c"
               (("\"man\"")
                (string-append "\"" (assoc-ref inputs "man-db") "/bin/man\""))
               (("\"hexedit\"")
                (string-append "\"" (assoc-ref outputs "out")
                               "/share/man/man1/hexedit.1.gz\""))))))))
    (native-inputs
     (list autoconf automake))
    (inputs
     (list man-db ncurses))
    (synopsis "View and edit files or devices in hexadecimal or ASCII")
    (description "hexedit shows a file both in ASCII and in hexadecimal.  The
file can be a device as the file is read a piece at a time.  You can modify
the file and search through it.")
    (home-page "http://rigaux.org/hexedit.html")
    (license license:gpl2+)))

(define-public dhex
  (package
    (name "dhex")
    (version "0.69")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.dettus.net/dhex/dhex_"
                                  version ".tar.gz"))
              (sha256
               (base32
                "06y4lrp29f2fh303ijk1xhspa1d4x4dm6hnyw3dd8szi3k6hnwsj"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f ;no tests provided
      #:make-flags
      #~(list (string-append "CC=" #$(cc-for-target)))
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure) ;no configure script
          (replace 'install ;multiple issues with provided 'make install'
            (lambda _
              (let ((bin (string-append #$output "/bin"))
                    (man1 (string-append #$output
                                         "/share/man/man1"))
                    (man5 (string-append #$output
                                         "/share/man/man5")))
                (install-file "dhex" bin)
                (install-file "dhex.1" man1)
                (install-file "dhexrc.5" man5)
                (install-file "dhex_markers.5" man5)
                (install-file "dhex_searchlog.5" man5)))))))
    (inputs (list ncurses))
    (home-page "https://www.dettus.net/dhex/")
    (synopsis "View, edit, and diff files in hexadecimal")
    (description
     "Dhex is hex editor which includes a diff mode, which can be used to
easily and conveniently compare two binary files.  It is based on Ncurses
and is themeable.")
    (license license:gpl2+)))

(define-public ht
  (package
    (name "ht")
    (version "2.1.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://sourceforge.net/projects/hte/files/ht-source/"
                    name "-" version ".tar.bz2"))
              (sha256
               (base32
                "0w2xnw3z9ws9qrdpb80q55h6ynhh3aziixcfn45x91bzrbifix9i"))))
    (build-system gnu-build-system)
    (inputs
     (list lzo ncurses))
    (synopsis "Viewer, editor, and analyzer for executable binaries")
    (description
     "ht is a terminal-based program to view, edit, and analyze any file, but
with a special focus on executable binaries.  Its goal is to combine the
low-level functionality of a debugger with the usability of an @dfn{Integrated
Development Environment} (IDE).")
    (home-page "https://hte.sourceforge.net/")
    (license license:gpl2)))

;; NOTE: The install target of imhex-pattern-language falls short in a few areas
;; that make this package difficult to use outside of ImHex.  Neither header
;; files nor package information (using e.g. pkg-config or CMake files) are
;; currently available.
(define-public imhex-pattern-language
  (package
    (name "imhex-pattern-language")
    (version "1.37.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/WerWolv/PatternLanguage")
             (commit (string-append "ImHex-v" version))
             (recursive? #t)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "13mlbclg2r3axv6vz4dsyry5azc6xavcbdkvwan6zyaq9ngk7r3r"))
       (modules '((guix build utils) (ice-9 ftw)))
       (snippet #~(begin
                    ;; NOTE: the `throwing-ptr' hasn't been updated in 6 years
                    ;; and the testsuite expects to use an outdated version of
                    ;; Conan, since this library if not interesting to have it
                    ;; in Guix it will remain bundled.
                    ;; NOTE: `libwolf' does not have an install target. Until
                    ;; the maintainers create one, it will be bundled.
                    (with-directory-excursion "external"
                      (for-each
                       (lambda (dir)
                         (unless (member dir '("." ".." "libwolv" "throwing_ptr"))
                           (delete-file-recursively dir)))
                       (scandir ".")))))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:configure-flags ''("-DLIBPL_SHARED_LIBRARY=ON"
                           "-DLIBPL_ENABLE_TESTS=ON"
                           "-DUSE_SYSTEM_NLOHMANN_JSON=ON"
                           "-DUSE_SYSTEM_CLI11=ON"
                           "-DUSE_SYSTEM_FMT=ON")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-rel-paths
            (lambda _
              (substitute* "tests/include/test_patterns/test_pattern_format.hpp"
                (("../tests/files/export/")
                 "../source/tests/files/export/"))))
          (add-after 'build 'build-tests
            (lambda _
              (invoke "make" "pattern_language_tests" "plcli")))
          (add-before 'check 'plcli-integration-tests
            (lambda _
              (with-directory-excursion "../source"
                (invoke "python3" "tests/integration/integration.py"
                        "../build/cli/plcli")))))))
    (native-inputs (list cli11 gcc-14 fmt-10 nlohmann-json python))
    (home-page "https://imhex.werwolv.net")
    (synopsis "Pattern language used by the ImHex Hex Editor")
    (description "This package provides a C-like domain-specific language used
for specifying patterns in the ImHex Hex Editor.")
    (license license:lgpl2.1)))

;; imhex-patterns can not currently be built by imhex-pattern-language alone,
;; see above for a rationale.  Thus, we only return an origin.
(define (make-imhex-patterns version sha256-hash)
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/WerWolv/ImHex-Patterns")
          (commit (string-append "ImHex-v" version))))
    (file-name (git-file-name "imhex-patterns" version))
    (sha256 sha256-hash)))

(define-public imhex
  (let* ((version "1.37.4")
         (imhex-patterns
          (make-imhex-patterns
           version
           (base32 "0m9g93fzmj2rsgaq25y4mmfigjh1xxyh41zjs6lp5ydsl5hhrn6q"))))
    (package
      (name "imhex")
      (version version)
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/WerWolv/ImHex")
               (commit (string-append "v" version))
               (recursive? #t)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0l3fpizkz2ykdirbn9alddnnsg75w6kwpp92nvmird13l80z1sdr"))
         (modules '((guix build utils) (ice-9 ftw) (ice-9 match)))
         (snippet
          #~(begin
              ;; XXX: imhex-pattern-language is missing the necessary packaging
              ;; structure to be linked independently.  See the comment on its
              ;; package.
              (copy-recursively #$imhex-pattern-language
                                "lib/external/pattern_language")
              ;; Drop `imhex-patterns' in the source tree.
              (copy-recursively #$imhex-patterns "ImHex-Patterns")

              ;; NOTE: `libwolv' does not have an install target. Until the
              ;; maintainers create one, it will remain bundled.
              ;; NOTE: `libromfs' it's meant to be bundled and cannot be
              ;; compiled independently. Until the maintainers support
              ;; building it independently, it will remain bundled.
              ;; NOTE: `hashlibplus' has been left as a submodule since it's a
              ;; fork which only makes sense within ImHex.
              ;; XXX: `imgui' is a fork with several experimental features.
              ;; Unbundle once they're upstreamed.
              (with-directory-excursion "lib/third_party"
                (for-each
                 (lambda (dir)
                   (unless (member dir '("." ".."
                                         "HashLibPlus" "libwolv" "libromfs"
                                         "imgui"
                                         ;; Needs source to include miniaudio.h
                                         "miniaudio"
                                         "microtar")) ; XXX: unbundle
                     (delete-file-recursively dir)))
                 (scandir "."))

                ;; Force use of system miniaudio.
                (delete-file "miniaudio/include/miniaudio.h")

                (for-each
                 (match-lambda
                   ((src . dst) (copy-recursively src dst)))
                 `((#$(package-source xdgpp) . "xdgpp"))))

              ;; Adjust CMake build scripts accordingly.
              (substitute* "cmake/build_helpers.cmake"
                (("add_subdirectory\\(\\$\\{THIRD_PARTY_.*\\}/jthread .*\\)")
                 "")
                (("set\\(JTHREAD_LIBRARIES .*\\)") ""))

              ;; Update sources.
              (substitute* (append (find-files "lib/libimhex" "\\.[ch](pp)?")
                                   (find-files "main" "\\.[ch](pp)?")
                                   (find-files "plugins" "\\.[ch](pp)?"))
                (("#include <jthread\\.hpp>") ""))
              (substitute* "plugins/ui/source/ui/menu_items.cpp"
                (("\\.\\./\\.\\./\\.\\./\\.\\./lib/libimhex/include/")
                 ""))))))
      (build-system cmake-build-system)
      (arguments
       (list
        ;; NOTE: there is an issue with the way the test library is linked
        ;; with the output binaries. Tests are intrusive and should not be
        ;; shipped with the release, when the issue is fixed we could add a
        ;; phase which builds the package with the tests and runs the
        ;; testsuite and a second build phase which prepares the output
        ;; binary.
        #:configure-flags
        ''("-DIMHEX_ENABLE_UNIT_TESTS=ON"
           "-DIMHEX_OFFLINE_BUILD=ON"
           ;; NOTE: required for the `validate-runpath' phase.
           ;; If OFF, the pluggings won't be able to find `libimhex.so'.
           "-DIMHEX_PLUGIN_ADD_INSTALL_PREFIX_TO_RPATH=ON"

           "-DUSE_SYSTEM_NLOHMANN_JSON=ON"
           "-DUSE_SYSTEM_CAPSTONE=ON"
           "-DUSE_SYSTEM_LUNASVG=ON"
           "-DUSE_SYSTEM_CLI11=ON"
           "-DUSE_SYSTEM_BOOST=ON"
           "-DUSE_SYSTEM_EDLIB=ON"
           "-DUSE_SYSTEM_YARA=ON"
           "-DUSE_SYSTEM_LLVM=ON"
           "-DUSE_SYSTEM_FMT=ON"
           "-DUSE_SYSTEM_NFD=ON")
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'fix-paths
              (lambda _
                (substitute* "main/gui/source/window/linux_window.cpp"
                  (("dbus-send")
                   #$(file-append dbus "/bin/dbus-send")))
                (substitute* "cmake/build_helpers.cmake"
                  (("Boost REQUIRED")
                   "Boost COMPONENTS regex REQUIRED"))))
            (add-before 'check 'build-tests
              (lambda _
                (invoke "make" "unit_tests"))))))
      (native-inputs
       (list cli11
             nlohmann-json
             pkg-config
             python
             python-wrapper
             ruby
             gcc-14))
      (inputs
       (list yara
             boost
             capstone
             curl
             edlib
             fmt
             glfw
             libarchive
             libffi
             llvm-17
             lunasvg
             plutovg
             mbedtls
             mesa
             miniaudio
             nativefiledialog-extended
             xz
             fontconfig
             lz4
             `(,zstd "lib")
             zlib
             freetype))
      (home-page "https://imhex.werwolv.net")
      (synopsis "Hex Editor to display, decode and analyze binary data")
      (description "ImHex is a hex editor with many advanced features that can
often only be found in paid applications.  Such features are a completely
custom binary template and pattern language to decode and highlight structures
in the data, a graphical node-based data processor to pre-process values
before they're displayed, a disassembler, diffing support, bookmarks and much
much more.")
      (license license:gpl2))))

(define-public bvi
  (package
    (name "bvi")
    (version "1.4.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://sourceforge/bvi/bvi/" version
                    "/bvi-" version ".src.tar.gz"))
              (sha256
               (base32
                "1wllrvs8r8bdm11sx01095j8cj4drjmw0dlkjfdkm5lnnk11dfjb"))))
    (build-system gnu-build-system)
    (arguments '(#:tests? #f))          ; no check target
    (inputs
     (list ncurses))
    (synopsis "Binary file editor")
    (description "@command{bvi} is a display-oriented editor for binary files,
based on the @command{vi} text editor.")
    (home-page "https://bvi.sourceforge.net/")
    (license license:gpl3+)))
