;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2016, 2017, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2017, 2021 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017–2022 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2018, 2019 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2019 Vagrant Cascadian <vagrant@reproducible-builds.org>
;;; Copyright © 2022, 2023 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (gnu packages diffoscope)
  #:use-module (gnu packages)
  #:use-module (gnu packages acl)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages android)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bootloaders)
  #:use-module (gnu packages cdrom)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpio)
  #:use-module (gnu packages dbm)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages haskell)
  #:use-module (gnu packages image)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages java)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages man)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages ocaml)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages pascal)
  #:use-module (gnu packages patchutils)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages video)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages web)
  #:use-module (guix build-system python)
  #:use-module (guix build-system pyproject)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (ice-9 match))

(define-public diffoscope
  (package
    (name "diffoscope")
    (version "296")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://salsa.debian.org/reproducible-builds/diffoscope.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "18h81vnk5hvm6z1rk05ndfksykf3xh75m8pdsmkp7i0fnhh7p22c"))))
    (build-system python-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          ;; These tests are broken because our `file` package has a
          ;; bug in berkeley-db and wasm file type detection.
          (add-after 'unpack 'remove-broken-file-type-detection-test
            (lambda _
              (delete-file "tests/comparators/test_berkeley_db.py")
              (delete-file "tests/comparators/test_wasm.py")))
          (add-after 'unpack 'embed-tool-references
            (lambda* (#:key inputs #:allow-other-keys)
              (define (bin command)
                (search-input-file inputs (string-append "bin/" command)))
              (substitute* "diffoscope/comparators/utils/compare.py"
                (("\\[\"(xxd)\"," _ command)
                 (string-append "[\"" (bin command) "\",")))
              (substitute* "diffoscope/diff.py"
                (("@tool_required\\(\"diff\"\\)") "")
                (("get_tool_name\\(\"(diff)\"\\)" _ command)
                 (string-append "get_tool_name(\"" (bin command) "\")")))
              (substitute* "diffoscope/comparators/directory.py"
                (("@tool_required\\(\"stat\"\\)") "")
                (("@tool_required\\(\"getfacl\"\\)") "")
                (("\\[\"(stat)\"," _ command)
                 (string-append "[\"" (bin command) "\","))
                (("\\[\"(getfacl)\"," _ command)
                 (string-append "[\"" (bin command) "\",")))))
          (add-after 'build 'build-man-page
            (lambda _
              (invoke "make" "-C" "doc")))
          (add-before 'check 'writable-test-data
            (lambda _
              ;; Tests may need write access to tests directory.
              (for-each make-file-writable (find-files "tests"))))
          (add-before 'check 'fix-failing-test
            (lambda _
              ;; There is no user name mapping in the build environment.
              ;; Pytest made it so much harder than should be necessary,
              ;; so I'm leaving… this here in case I ever need it again:
              ;; (substitute* "tests/comparators/test_squashfs.py"
              ;;   (("^def test_symlink_root.*" match)     ; no, I don't
              ;;    (string-append                         ; know Python
              ;;     match "\n    raise ValueError("       ; why do you
              ;;     "differences_root[1].unified_diff)\n"))) ; ask
              (substitute* "tests/data/squashfs_root_expected_diff"
                (("root/root")
                 '"0/0      "))))
          (add-before 'check 'delete-failing-test
            ;; Please add new tests to fix-failing-test and not here ;-)
            (lambda _
              ;; This requires /sbin to be in $PATH.
              (delete-file "tests/test_tools.py")))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               ;; Increase verbosity of tests and provide a summary
               (invoke "pytest" "-vv" "-r" "sxX"))))
          (add-after 'install 'install-man-page
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (man (string-append out "/share/man/man1")))
                (install-file "doc/diffoscope.1" man)))))))
    (inputs (list rpm                   ;for rpm-python
                  python-debian
                  python-libarchive-c
                  python-magic
                  python-tlsh
                  acl                   ;for getfacl
                  coreutils             ;for stat
                  diffutils             ;for diff
                  xxd))
    (native-inputs
     (append
      (list help2man

            ;; Below are packages used for tests.
            binwalk
            python-pytest
            python-chardet
            python-h5py
            python-pypdf
            python-progressbar33

            abootimg
            bdb
            binutils
            bzip2
            cdrkit-libre
            colord
            cpio
            docx2txt
            dtc
            e2fsprogs
            ffmpeg)

      (match (%current-system)
        ;; fpc is only available on x86 currently.
        ((or "x86_64-linux" "i686-linux")
         (list fpc))
        (_ '()))

      (list gettext-minimal
            ghostscript
            `(,giflib "bin")
            gnumeric
            gnupg
            hdf5
            html2text
            imagemagick
            libarchive
            llvm
            lz4
            lzip
            ocaml
            odt2txt
            openssh
            openssl
            p7zip
            perl
            pgpdump
            poppler
            python-jsbeautifier
            r-minimal
            rpm
            sng
            sqlite
            squashfs-tools
            tcpdump
            u-boot-tools
            unzip
            wabt
            xxd
            xz
            zip
            zstd)

      ;; Also for tests.  The test suite skips tests when these are missing.
      (match (%current-system)
        ;; ghc is only available on x86 currently.
        ((or "x86_64-linux" "i686-linux")
         (list ghc))
        (_ '()))
      (match (%current-system)
        ;; openjdk and dependent packages are only
        ;; available on x86_64 currently.
        ((or "x86_64-linux")
         ;; No unversioned openjdk available.
         (list `(,openjdk12 "jdk")
               enjarify))
        (_ '()))))
    (home-page "https://diffoscope.org/")
    (synopsis "Compare files, archives, and directories in depth")
    (description
     "Diffoscope tries to get to the bottom of what makes files or directories
different.  It recursively unpacks archives of many kinds and transforms
various binary formats into more human readable forms to compare them.  It can
compare two tarballs, ISO images, or PDFs just as easily.

Diffoscope has many optional dependencies; @code{diffoscope
--list-missing-tools guix} will display optional packages to
install.")
    (license license:gpl3+)))

(define-public reprotest
  (package
    (name "reprotest")
    (version "0.7.29")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://salsa.debian.org/reproducible-builds/reprotest.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "17n7pdqil3jmpwcshr6dm5qsbpim3847smgxa82wy33kl2bz1ai8"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'compress-documentation 'make-virt-files-executable
            ;; The autopkgtest-virt- files need to be marked executable for
            ;; reprotest to function correctly.
            (lambda _
              (for-each (lambda (file)
                          (chmod file #o755))
                        (find-files #$output "autopkgtest-virt-.*"))))
          ;; Adjust use of importlib.resources to use python 3.10 compatible
          ;; syntax, which requires an argument.
          ;; Drop when switching to python 3.12+.
          (add-after 'unpack 'adjust-importlib-resources-for-old-python
            (lambda _
              (substitute* "reprotest/__init__.py"
                (("importlib.resources.files\\(\\)")
                  "importlib.resources.files(package='reprotest')"))))
          (add-after 'unpack 'skip-most-tests
            ;; These tests require functionality not available in the guix
            ;; build environment
            (lambda _
              (substitute* "tests/test_reprotest.py"
                (("def (test_simple_build|test_self_build|test_variations)"
                  def)
                 (string-append
                  "@pytest.mark.skip(reason='guix environment lacks permissions')
"
                  def)))))
          (add-after 'install 'install-doc
            (lambda _
              (let* ((mandir1 (string-append
                               #$output "/share/man/man1"))
                     (docdir (string-append
                              #$output "/share/doc/" #$name "-" #$version)))
                (invoke "make" "-C" "doc")
                (mkdir-p mandir1)
                (install-file "doc/reprotest.1" mandir1)
                (mkdir-p docdir)
                (install-file "./README.rst" docdir)
                (install-file "./README-dev.rst" docdir)))))))
    (native-inputs
     (list diffoscope
           help2man
           libfaketime
           python-docutils
           python-magic
           python-pytest
           python-setuptools
           python-tlsh
           python-wheel
           unzip
           xxd))
    (propagated-inputs
     (list python-debian
           python-distro
           python-libarchive-c
           python-rstr))
    (home-page "https://salsa.debian.org/reproducible-builds/reprotest")
    (synopsis "Build software and check it for reproducibility")
    (description "Reprotest builds the same source code twice in different
environments, and then checks the binaries produced by each build for
differences.  If any are found, then diffoscope or diff is used to display
them in detail for later analysis.")
    (license (list license:gpl3+ license:gpl2+))))

(define-public trydiffoscope
  (package
    (name "trydiffoscope")
    (version "67.0.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://salsa.debian.org/reproducible-builds/trydiffoscope.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0k698g4fws63rnav4pvfsf1hfds867xan59mmv5zw71r58lm6cxb"))))
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-after 'install 'install-doc
                 (lambda _
                   (let* ((share (string-append #$output "/share/")))
                     (mkdir-p (string-append share "/man/man1/"))
                     (invoke "rst2man.py" "trydiffoscope.1.rst"
                             (string-append share "/man/man1/trydiffoscope.1"))
                     (mkdir-p (string-append
                               share "/doc/" #$name "-" #$version))
                     (install-file
                      "./README.rst"
                      (string-append share "/doc/" #$name "-" #$version))))))))
    (propagated-inputs
     (list python-requests))
    (native-inputs
     (list gzip python-docutils))
    (build-system python-build-system)
    (home-page "https://try.diffoscope.org")
    (synopsis "Client for remote diffoscope service")
    (description "This is a client for the @url{https://try.diffoscope.org,
remote diffoscope service}.

Diffoscope tries to get to the bottom of what makes files or directories
different.  It recursively unpacks archives of many kinds and transforms
various binary formats into more human readable forms to compare them.  It can
compare two tarballs, ISO images, or PDFs just as easily.

Results are displayed by default, stored as local text or html files, or made
available via a URL on @url{https://try.diffoscope.org}.  Results stored on the
server are purged after 30 days.")
    (license license:gpl3+)))
