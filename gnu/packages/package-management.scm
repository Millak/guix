;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2016, 2017, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015, 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017 Muriithi Frederick Muriuki <fredmanglis@gmail.com>
;;; Copyright © 2017, 2018 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2017 Roel Janssen <roel@gnu.org>
;;; Copyright © 2017, 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2018, 2019 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2018 Sou Bunnbu <iyzsong@member.fsf.org>
;;; Copyright © 2018, 2019 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2019 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2019 Jonathan Brielmaier <jonathan.brielmaier@web.de>
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

(define-module (gnu packages package-management)
  #:use-module (gnu packages)
  #:use-module (gnu packages acl)
  #:use-module (gnu packages attr)
  #:use-module (gnu packages avahi)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bdw-gc)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages bootstrap)          ;for 'bootstrap-guile-origin'
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpio)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages dbm)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages file)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lisp)
  #:use-module (gnu packages man)
  #:use-module (gnu packages nettle)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages patchutils)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-check)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages popt)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages virtualization)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system python)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1))

(define (boot-guile-uri arch)
  "Return the URI for the bootstrap Guile tarball for ARCH."
  (cond ((string=? "armhf" arch)
         (string-append "http://alpha.gnu.org/gnu/guix/bootstrap/"
                        arch "-linux"
                        "/20150101/guile-2.0.11.tar.xz"))
        ((string=? "aarch64" arch)
         (string-append "http://alpha.gnu.org/gnu/guix/bootstrap/"
                        arch "-linux/20170217/guile-2.0.14.tar.xz"))
        (else
         (string-append "http://alpha.gnu.org/gnu/guix/bootstrap/"
                        arch "-linux"
                        "/20131110/guile-2.0.9.tar.xz"))))

(define-public guix
  ;; Latest version of Guix, which may or may not correspond to a release.
  ;; Note: the 'update-guix-package.scm' script expects this definition to
  ;; start precisely like this.
  (let ((version "1.0.1")
        (commit "41b4b713f4892918a9a1950acdd89f33b977d143")
        (revision 10))
    (package
      (name "guix")

      (version (if (zero? revision)
                   version
                   (string-append version "-"
                                  (number->string revision)
                                  "." (string-take commit 7))))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://git.savannah.gnu.org/r/guix.git")
                      (commit commit)))
                (sha256
                 (base32
                  "08sblj4xy78va6zlxmxdq2id58pjr8rjqxxycd77hiacsqbjh9g6"))
                (file-name (string-append "guix-" version "-checkout"))))
      (build-system gnu-build-system)
      (arguments
       `(#:configure-flags (list
                            "--localstatedir=/var"
                            "--sysconfdir=/etc"
                            (string-append "--with-bash-completion-dir="
                                           (assoc-ref %outputs "out")
                                           "/etc/bash_completion.d")

                            ;; Set 'DOT_USER_PROGRAM' to the empty string so
                            ;; we don't keep a reference to Graphviz, whose
                            ;; closure is pretty big (too big for the Guix
                            ;; system installation image.)
                            "ac_cv_path_DOT_USER_PROGRAM=dot"

                            ;; To avoid problems with the length of shebangs,
                            ;; choose a fixed-width and short directory name
                            ;; for tests.
                            "ac_cv_guix_test_root=/tmp/guix-tests")
         #:parallel-tests? #f         ;work around <http://bugs.gnu.org/21097>

         #:modules ((guix build gnu-build-system)
                    (guix build utils)
                    (srfi srfi-26)
                    (ice-9 popen)
                    (ice-9 rdelim))

         #:phases (modify-phases %standard-phases
                    (replace 'bootstrap
                      (lambda _
                        ;; Make sure 'msgmerge' can modify the PO files.
                        (for-each (lambda (po)
                                    (chmod po #o666))
                                  (find-files "." "\\.po$"))

                        (patch-shebang "build-aux/git-version-gen")

                        (call-with-output-file ".tarball-version"
                          (lambda (port)
                            (display ,version port)))

                        (invoke "sh" "bootstrap")))
                    (add-before 'check 'copy-bootstrap-guile
                      (lambda* (#:key system inputs #:allow-other-keys)
                        ;; Copy the bootstrap guile tarball in the store used
                        ;; by the test suite.
                        (define (intern file recursive?)
                          (let ((base (strip-store-file-name file)))
                            ;; Note: don't use 'guix download' here because we
                            ;; need to set the 'recursive?' argument.
                            (invoke "./test-env" "guile" "-c"
                                    (object->string
                                     `(begin
                                        (use-modules (guix))
                                        (with-store store
                                          (add-to-store store ,base ,recursive?
                                                        "sha256" ,file)))))))

                        (intern (assoc-ref inputs "boot-guile") #f)

                        ;; On x86_64 some tests need the i686 Guile.
                        ,@(if (and (not (%current-target-system))
                                   (string=? (%current-system)
                                             "x86_64-linux"))
                              '((intern (assoc-ref inputs "boot-guile/i686") #f))
                              '())

                        ;; Copy the bootstrap executables.
                        (for-each (lambda (input)
                                    (intern (assoc-ref inputs input) #t))
                                  '("bootstrap/bash" "bootstrap/mkdir"
                                    "bootstrap/tar" "bootstrap/xz"))
                        #t))
                    (add-after 'unpack 'disable-failing-tests
                      ;; XXX FIXME: These tests fail within the build container.
                      (lambda _
                        (substitute* "tests/syscalls.scm"
                          (("^\\(test-(assert|equal) \"(clone|setns|pivot-root)\"" all)
                           (string-append "(test-skip 1)\n" all)))
                        (substitute* "tests/containers.scm"
                          (("^\\(test-(assert|equal)" all)
                           (string-append "(test-skip 1)\n" all)))
                        (when (file-exists? "tests/guix-environment-container.sh")
                          (substitute* "tests/guix-environment-container.sh"
                            (("guix environment --version")
                             "exit 77\n")))
                        #t))
                    (add-before 'check 'set-SHELL
                      (lambda _
                        ;; 'guix environment' tests rely on 'SHELL' having a
                        ;; correct value, so set it.
                        (setenv "SHELL" (which "sh"))
                        #t))
                    (add-after 'install 'wrap-program
                      (lambda* (#:key inputs outputs #:allow-other-keys)
                        ;; Make sure the 'guix' command finds GnuTLS,
                        ;; Guile-JSON, and Guile-Git automatically.
                        (let* ((out    (assoc-ref outputs "out"))
                               (guile  (assoc-ref inputs "guile"))
                               (gcrypt (assoc-ref inputs "guile-gcrypt"))
                               (json   (assoc-ref inputs "guile-json"))
                               (sqlite (assoc-ref inputs "guile-sqlite3"))
                               (git    (assoc-ref inputs "guile-git"))
                               (bs     (assoc-ref inputs
                                                  "guile-bytestructures"))
                               (ssh    (assoc-ref inputs "guile-ssh"))
                               (gnutls (assoc-ref inputs "gnutls"))
                               (locales (assoc-ref inputs "glibc-utf8-locales"))
                               (deps   (list gcrypt json sqlite gnutls
                                             git bs ssh))
                               (effective
                                (read-line
                                 (open-pipe* OPEN_READ
                                             (string-append guile "/bin/guile")
                                             "-c" "(display (effective-version))")))
                               (path   (string-join
                                        (map (cut string-append <>
                                                  "/share/guile/site/"
                                                  effective)
                                             (delete #f deps))
                                        ":"))
                               (gopath (string-join
                                        (map (cut string-append <>
                                                  "/lib/guile/" effective
                                                  "/site-ccache")
                                             (delete #f deps))
                                        ":"))
                               (locpath (string-append locales "/lib/locale")))

                          (wrap-program (string-append out "/bin/guix")
                            `("GUILE_LOAD_PATH" ":" prefix (,path))
                            `("GUILE_LOAD_COMPILED_PATH" ":" prefix (,gopath))
                            `("GUIX_LOCPATH" ":" suffix (,locpath)))

                          #t))))))
      (native-inputs `(("pkg-config" ,pkg-config)

                       ;; XXX: Keep the development inputs here even though
                       ;; they're unnecessary, just so that 'guix environment
                       ;; guix' always contains them.
                       ("autoconf" ,autoconf-wrapper)
                       ("automake" ,automake)
                       ("gettext" ,gettext-minimal)
                       ("texinfo" ,texinfo)
                       ("graphviz" ,graphviz)
                       ("help2man" ,help2man)
                       ("po4a" ,po4a)))
      (inputs
       `(("bzip2" ,bzip2)
         ("gzip" ,gzip)
         ("zlib" ,zlib)              ;for 'guix publish'
         ("lzlib" ,lzlib)            ;for 'guix publish' and 'guix substitute'

         ("sqlite" ,sqlite)
         ("libgcrypt" ,libgcrypt)

         ("guile" ,guile-2.2)

         ;; Some of the tests use "unshare" when it is available.
         ("util-linux" ,util-linux)

         ;; Many tests rely on the 'guile-bootstrap' package, which is why we
         ;; have it here.
         ("boot-guile" ,(bootstrap-guile-origin (%current-system)))
         ,@(if (and (not (%current-target-system))
                    (string=? (%current-system) "x86_64-linux"))
               `(("boot-guile/i686" ,(bootstrap-guile-origin "i686-linux")))
               '())

         ;; Tests also rely on these bootstrap executables.
         ("bootstrap/bash" ,(bootstrap-executable "bash" (%current-system)))
         ("bootstrap/mkdir" ,(bootstrap-executable "mkdir" (%current-system)))
         ("bootstrap/tar" ,(bootstrap-executable "tar" (%current-system)))
         ("bootstrap/xz" ,(bootstrap-executable "xz" (%current-system)))

         ("glibc-utf8-locales" ,glibc-utf8-locales)))
      (propagated-inputs
       `(("gnutls" ,gnutls)
         ("guile-gcrypt" ,guile-gcrypt)
         ("guile-json" ,guile-json-3)
         ("guile-sqlite3" ,guile-sqlite3)
         ("guile-ssh" ,guile-ssh)
         ("guile-git" ,guile-git)))

      (home-page "https://www.gnu.org/software/guix/")
      (synopsis "Functional package manager for installed software packages and versions")
      (description
       "GNU Guix is a functional package manager for the GNU system, and is
also a distribution thereof.  It includes a virtual machine image.  Besides
the usual package management features, it also supports transactional
upgrades and roll-backs, per-user profiles, and much more.  It is based on
the Nix package manager.")
      (license license:gpl3+)
      (properties '((ftp-server . "alpha.gnu.org"))))))

(define-public guix-daemon
  ;; This package is for internal consumption: it allows us to quickly build
  ;; the 'guix-daemon' program and use that in (guix self), used by 'guix
  ;; pull'.
  (package
    (inherit guix)
    (properties `((hidden? . #t)))
    (name "guix-daemon")

    ;; Use a minimum set of dependencies.
    (native-inputs
     (fold alist-delete (package-native-inputs guix)
           '("po4a" "graphviz" "help2man")))
    (inputs
     `(("gnutls" ,gnutls)
       ("guile-git" ,guile-git)
       ("guile-json" ,guile-json-3)
       ("guile-gcrypt" ,guile-gcrypt)
       ,@(fold alist-delete (package-inputs guix)
               '("boot-guile" "boot-guile/i686" "util-linux"))))

    (propagated-inputs '())

    (arguments
     (substitute-keyword-arguments (package-arguments guix)
       ((#:configure-flags flags '())
        ;; Pretend we have those libraries; we don't actually need them.
        `(append ,flags
                 '("guix_cv_have_recent_guile_sqlite3=yes"
                   "guix_cv_have_recent_guile_ssh=yes")))
       ((#:tests? #f #f)
        #f)
       ((#:phases phases '%standard-phases)
        `(modify-phases ,phases
           (add-after 'unpack 'change-default-guix
             (lambda _
               ;; We need to tell 'guix-daemon' which 'guix' command to use.
               ;; Here we use a questionable hack where we hard-code root's
               ;; current guix, which could be wrong (XXX).  Note that scripts
               ;; like 'guix perform-download' do not run as root so we assume
               ;; that they have access to /var/guix/profiles/per-user/root.
               (substitute* "nix/libstore/globals.cc"
                 (("guixProgram = (.*)nixBinDir + \"/guix\"" _ before)
                  (string-append "guixProgram = " before
                                 "/var/guix/profiles/per-user/root\
/current-guix/bin/guix")))
               #t))
           (replace 'build
             (lambda _
               (invoke "make" "nix/libstore/schema.sql.hh")
               (invoke "make" "-j" (number->string
                                    (parallel-job-count))
                       "guix-daemon")))
           (delete 'copy-bootstrap-guile)
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (invoke "make" "install-binPROGRAMS")))
           (delete 'wrap-program)))))))

(define-public guile2.0-guix
  (deprecated-package "guile2.0-guix" guix))

(define-public guix-minimal
  ;; A version of Guix which is built with the minimal set of dependencies, as
  ;; outlined in the README "Requirements" section.  Intended as a CI job, so
  ;; marked as hidden.
  (hidden-package
   (package
     (inherit guix)
     (name "guix-minimal")
     (inputs
      `(("guile" ,guile-2.2)
        ,@(alist-delete "guile" (package-inputs guix))))
     (propagated-inputs
      (fold alist-delete
            (package-propagated-inputs guix)
            '("guile-ssh"))))))

(define (source-file? file stat)
  "Return true if FILE is likely a source file, false if it is a typical
generated file."
  (define (wrong-extension? file)
    (or (string-suffix? "~" file)
        (member (file-extension file)
                '("o" "a" "lo" "so" "go"))))

  (match (basename file)
    ((or ".git" "autom4te.cache" "configure" "Makefile" "Makefile.in" ".libs")
     #f)
    ((? wrong-extension?)
     #f)
    (_
     #t)))

(define-public current-guix-package
  ;; This parameter allows callers to override the package that 'current-guix'
  ;; returns.  This is useful when 'current-guix' cannot compute it by itself,
  ;; for instance because it's not running from a source code checkout.
  (make-parameter #f))

(define-public current-guix
  (let* ((repository-root (delay (canonicalize-path
                                  (string-append (current-source-directory)
                                                 "/../.."))))
         (select? (delay (or (git-predicate (force repository-root))
                             source-file?))))
    (lambda ()
      "Return a package representing Guix built from the current source tree.
This works by adding the current source tree to the store (after filtering it
out) and returning a package that uses that as its 'source'."
      (or (current-guix-package)
          (package
            (inherit guix)
            (version (string-append (package-version guix) "+"))
            (source (local-file (force repository-root) "guix-current"
                                #:recursive? #t
                                #:select? (force select?))))))))


;;;
;;; Other tools.
;;;

(define-public nix
  (package
    (name "nix")
    (version "2.0.4")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://nixos.org/releases/nix/nix-"
                                 version "/nix-" version ".tar.xz"))
             (sha256
              (base32
               "0ss9svxlh1pvrdmnqjvjyqjmbqmrdbyfarvbb14i9d4bggzl0r8n"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)))
    (inputs `(("curl" ,curl)
              ("bzip2" ,bzip2)
              ("libgc" ,libgc)
              ("libseccomp" ,libseccomp)
              ("libsodium" ,libsodium)
              ("openssl" ,openssl)
              ("sqlite" ,sqlite)
              ("xz" ,xz)))
    (home-page "https://nixos.org/nix/")
    (synopsis "The Nix package manager")
    (description
     "Nix is a purely functional package manager.  This means that it treats
packages like values in purely functional programming languages such as
Haskell—they are built by functions that don't have side-effects, and they
never change after they have been built.  Nix stores packages in the Nix
store, usually the directory /nix/store, where each package has its own unique
sub-directory.")
    (license license:lgpl2.1+)))

(define-public stow
  (package
    (name "stow")
    (version "2.3.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/stow/stow-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0jrxy12ywn7smdzdnvwzjw77l6knx6jkj2rckgykg1dpf6bdkm89"))))
    (build-system gnu-build-system)
    (inputs
     `(("perl" ,perl)))
    (native-inputs
     `(("perl-test-simple" ,perl-test-simple)
       ("perl-test-output" ,perl-test-output)
       ("perl-capture-tiny" ,perl-capture-tiny)
       ("perl-io-stringy" ,perl-io-stringy)))
    (home-page "https://www.gnu.org/software/stow/")
    (synopsis "Managing installed software packages")
    (description
     "GNU Stow is a symlink manager.  It generates symlinks to directories
of data and makes them appear to be merged into the same directory.  It is
typically used for managing software packages installed from source, by
letting you install them apart in distinct directories and then create
symlinks to the files in a common directory such as /usr/local.")
    (license license:gpl3+)))

(define-public rpm
  (package
    (name "rpm")
    (version "4.14.2.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://ftp.rpm.org/releases/rpm-"
                                  (version-major+minor version) ".x/rpm-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "1nmck2fq9h85fgs3zhh6w1avlw5y16cbz5khd459ry3jfd5w4f8i"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--with-external-db"   ;use the system's bdb
                           "--enable-python"
                           "--without-lua")
       #:phases (modify-phases %standard-phases
                  (add-before 'configure 'set-nss-library-path
                    (lambda* (#:key inputs #:allow-other-keys)
                      (let ((nss (assoc-ref inputs "nss")))
                        (setenv "LIBRARY_PATH"
                                (string-append (getenv "LIBRARY_PATH") ":"
                                               nss "/lib/nss"))
                        #t))))))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("python" ,python)
       ("xz" ,xz)
       ("bdb" ,bdb)
       ("popt" ,popt)
       ("nss" ,nss)
       ("nspr" ,nspr)
       ("libarchive" ,libarchive)
       ("file" ,file)
       ("bzip2" ,bzip2)
       ("zlib" ,zlib)
       ("cpio" ,cpio)))
    (home-page "https://rpm.org/")
    (synopsis "The RPM Package Manager")
    (description
     "The RPM Package Manager (RPM) is a command-line driven package
management system capable of installing, uninstalling, verifying, querying,
and updating computer software packages.  Each software package consists of an
archive of files along with information about the package like its version, a
description.  There is also a library permitting developers to manage such
transactions from C or Python.")

    ;; The whole is GPLv2+; librpm itself is dual-licensed LGPLv2+ | GPLv2+.
    (license license:gpl2+)))

(define-public python-anaconda-client
  (package
    (name "python-anaconda-client")
    (version "1.6.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/Anaconda-Platform/"
                           "anaconda-client/archive/" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1wv4wi6k5jz7rlwfgvgfdizv77x3cr1wa2aj0k1595g7fbhkjhz2"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-pyyaml" ,python-pyyaml)
       ("python-requests" ,python-requests)
       ("python-clyent" ,python-clyent)))
    (native-inputs
     `(("python-pytz" ,python-pytz)
       ("python-dateutil" ,python-dateutil)
       ("python-mock" ,python-mock)
       ("python-coverage" ,python-coverage)
       ("python-pillow" ,python-pillow)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; This is needed for some tests.
         (add-before 'check 'set-up-home
           (lambda* _ (setenv "HOME" "/tmp") #t))
         (add-before 'check 'remove-network-tests
           (lambda* _
             ;; Remove tests requiring a network connection
             (let ((network-tests '("tests/test_upload.py"
                                    "tests/test_authorizations.py"
                                    "tests/test_login.py"
                                    "tests/test_whoami.py"
                                    "utils/notebook/tests/test_data_uri.py"
                                    "utils/notebook/tests/test_base.py"
                                    "utils/notebook/tests/test_downloader.py"
                                    "inspect_package/tests/test_conda.py")))
               (with-directory-excursion "binstar_client"
                 (for-each delete-file network-tests)))
             #t)))))
    (home-page "https://github.com/Anaconda-Platform/anaconda-client")
    (synopsis "Anaconda Cloud command line client library")
    (description
     "Anaconda Cloud command line client library provides an interface to
Anaconda Cloud.  Anaconda Cloud is useful for sharing packages, notebooks and
environments.")
    (license license:bsd-3)))

(define-public python2-anaconda-client
  (package-with-python2 python-anaconda-client))

(define-public python-conda
  (package
    (name "python-conda")
    (version "4.3.16")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/conda/conda/archive/"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1jq8hyrc5npb5sf4vw6s6by4602yj8f79vzpbwdfgpkn02nfk1dv"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'create-version-file
           (lambda _
             (with-output-to-file "conda/.version"
               (lambda () (display ,version)))
             #t))
         (add-before 'check 'remove-failing-tests
           (lambda _
             ;; These tests require internet/network access
             (let ((network-tests '("test_cli.py"
                                    "test_create.py"
                                    "test_export.py"
                                    "test_fetch.py"
                                    "test_history.py"
                                    "test_info.py"
                                    "test_install.py"
                                    "test_priority.py"
                                    "conda_env/test_cli.py"
                                    "conda_env/test_create.py"
                                    "conda_env/specs/test_notebook.py"
                                    "conda_env/utils/test_notebooks.py"
                                    "core/test_index.py"
                                    "core/test_repodata.py")))
               (with-directory-excursion "tests"
                 (for-each delete-file network-tests)

                 ;; FIXME: This test creates a file, then deletes it and tests
                 ;; that the file was deleted.  For some reason it fails when
                 ;; building with guix, but does not when you run it in the
                 ;; directory left when you build with the --keep-failed
                 ;; option
                 (delete-file "gateways/disk/test_delete.py")
                 #t))))
         (replace 'check
           (lambda _
             (setenv "HOME" "/tmp")
             (invoke "py.test"))))))
    (native-inputs
     `(("python-cytoolz" ,python-cytoolz)
       ("python-ruamel.yaml" ,python-ruamel.yaml)
       ("python-requests" ,python-requests)
       ("python-pycosat" ,python-pycosat)
       ("python-pytest" ,python-pytest)
       ("python-responses" ,python-responses)
       ("python-pyyaml" ,python-pyyaml)
       ("python-anaconda-client" ,python-anaconda-client)))
    (home-page "https://github.com/conda/conda")
    (synopsis "Cross-platform, OS-agnostic, system-level binary package manager")
    (description
     "Conda is a cross-platform, Python-agnostic binary package manager.  It
is the package manager used by Anaconda installations, but it may be used for
other systems as well.  Conda makes environments first-class citizens, making
it easy to create independent environments even for C libraries.  Conda is
written entirely in Python.

This package provides Conda as a library.")
    (license license:bsd-3)))

(define-public python2-conda
  (let ((base (package-with-python2
               (strip-python2-variant python-conda))))
    (package (inherit base)
             (native-inputs
              `(("python2-enum34" ,python2-enum34)
                ,@(package-native-inputs base))))))

(define-public conda
  (package (inherit python-conda)
    (name "conda")
    (arguments
     (substitute-keyword-arguments (package-arguments python-conda)
       ((#:phases phases)
        `(modify-phases ,phases
           (replace 'build
             (lambda* (#:key outputs #:allow-other-keys)
               ;; This test fails when run before installation.
               (delete-file "tests/test_activate.py")

               ;; Fix broken defaults
               (substitute* "conda/base/context.py"
                 (("return sys.prefix")
                  (string-append "return \"" (assoc-ref outputs "out") "\""))
                 (("return (prefix_is_writable\\(self.root_prefix\\))" _ match)
                  (string-append "return False if self.root_prefix == self.conda_prefix else "
                                 match)))

               ;; The util/setup-testing.py is used to build conda in
               ;; application form, rather than the default, library form.
               ;; With this, we are able to run commands like `conda --help`
               ;; directly on the command line
               (invoke "python" "utils/setup-testing.py" "build_py")))
           (replace 'install
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (target (string-append out "/lib/python"
                                             (python-version
                                              (assoc-ref inputs "python"))
                                             "/site-packages/")))
                 ;; The installer aborts if the target directory is not on
                 ;; PYTHONPATH.
                 (setenv "PYTHONPATH"
                         (string-append target ":" (getenv "PYTHONPATH")))

                 ;; And it aborts if the directory doesn't exist.
                 (mkdir-p target)
                 (invoke "python" "utils/setup-testing.py" "install"
                         (string-append "--prefix=" out)))))
           ;; The "activate" and "deactivate" scripts don't need wrapping.
           ;; They also break when they are renamed.
           (add-after 'wrap 'undo-wrap
             (lambda* (#:key outputs #:allow-other-keys)
               (with-directory-excursion (string-append (assoc-ref outputs "out") "/bin/")
                 (delete-file "deactivate")
                 (rename-file ".deactivate-real" "deactivate")
                 (delete-file "activate")
                 (rename-file ".activate-real" "activate")
                 #t)))))))
    (description
     "Conda is a cross-platform, Python-agnostic binary package manager.  It
is the package manager used by Anaconda installations, but it may be used for
other systems as well.  Conda makes environments first-class citizens, making
it easy to create independent environments even for C libraries.  Conda is
written entirely in Python.")))

(define-public gwl
  (package
    (name "gwl")
    (version "0.1.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.guixwl.org/releases/gwl-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "06pm967mq1wyggx7l0nfapw5s0k5qc5r9lawk2v3db868br779a7"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("guile" ,guile-2.2)))
    (propagated-inputs
     `(("guix" ,guix)
       ("guile-commonmark" ,guile-commonmark)))
    (home-page "https://www.guixwl.org")
    (synopsis "Workflow management extension for GNU Guix")
    (description "This project provides two subcommands to GNU Guix and
introduces two record types that provide a workflow management extension built
on top of GNU Guix.")
    ;; The Scheme modules in guix/ and gnu/ are licensed GPL3+,
    ;; the web interface modules in gwl/ are licensed AGPL3+,
    ;; and the fonts included in this package are licensed OFL1.1.
    (license (list license:gpl3+ license:agpl3+ license:silofl1.1))))

(define-public guix-jupyter
  (package
    (name "guix-jupyter")
    (version "0.1.0")
    (home-page "https://gitlab.inria.fr/guix-hpc/guix-kernel")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page)
                                  (commit (string-append "v" version))))
              (sha256
               (base32
                "01z7jjkc7r7lj6637rcgpz40v8xqqyfp6871h94yvcnwm7zy9h1n"))
              (file-name (string-append "guix-jupyter-" version "-checkout"))))
    (build-system gnu-build-system)
    (arguments
     `(#:modules ((srfi srfi-26)
                  (ice-9 match)
                  (ice-9 popen)
                  (ice-9 rdelim)
                  (guix build utils)
                  (guix build gnu-build-system))
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'sed-kernel-json
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out   (assoc-ref outputs "out"))
                    (guix  (assoc-ref inputs  "guix"))
                    (guile (assoc-ref inputs  "guile"))
                    (json  (assoc-ref inputs  "guile-json"))
                    (git   (assoc-ref inputs  "guile-git"))
                    (bs    (assoc-ref inputs  "guile-bytestructures"))
                    (s-zmq (assoc-ref inputs  "guile-simple-zmq"))
                    (gcrypt (assoc-ref inputs  "guile-gcrypt"))
                    (deps  (list out s-zmq guix json git bs gcrypt))
                    (effective
                     (read-line
                      (open-pipe* OPEN_READ
                                  (string-append guile "/bin/guile")
                                  "-c" "(display (effective-version))")))
                    (path (map (cut string-append "-L\", \"" <>
                                    "/share/guile/site/"
                                    effective)
                               deps))
                    (gopath (map (cut string-append "-C\", \"" <>
                                      "/lib/guile/" effective
                                      "/site-ccache")
                                 deps))
                    (kernel-dir (string-append out "/share/jupyter/kernels/guix/")))
               (substitute* (string-append kernel-dir "kernel.json")
                 (("-s")
                  (string-join
                   (list (string-join path "\",\n\t\t\"")
                         (string-join gopath "\",\n\t\t\"")
                         "-s")
                   "\",\n\t\t\""))
                 (("guix-jupyter-kernel.scm")
                  (string-append out "/share/guile/site/2.2/"
                                 "guix-jupyter-kernel.scm")))
               #t))))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("pkg-config" ,pkg-config)

       ;; For testing.
       ("jupyter" ,jupyter)
       ("python-ipython" ,python-ipython)
       ("python-ipykernel" ,python-ipykernel)))
    (inputs
     `(("guix" ,guix)
       ("guile" ,guile-2.2)))
    (propagated-inputs
     `(("guile-json" ,guile-json-3)
       ("guile-simple-zmq" ,guile-simple-zmq)
       ("guile-gcrypt" ,guile-gcrypt)))
    (synopsis "Guix kernel for Jupyter")
    (description
     "Guix-Jupyter is a Jupyter kernel.  It allows you to annotate notebooks
with information about their software dependencies, such that code is executed
in the right software environment.  Guix-Jupyter spawns the actual kernels
such as @code{python-ipykernel} on behalf of the notebook user and runs them
in an isolated environment, in separate namespaces.")
    (license license:gpl3+)))

(define-public gcab
  (package
    (name "gcab")
    (version "1.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/gcab/"
                                  version "/gcab-" version ".tar.xz"))
              (sha256
               (base32
                "038h5kk41si2hc9d9169rrlvp8xgsxq27kri7hv2vr39gvz9cbas"))))
    (build-system meson-build-system)
    (native-inputs
     `(("glib:bin" ,glib "bin")         ; for glib-mkenums
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)
       ("vala" ,vala)))
    (inputs
     `(("glib" ,glib)
       ("zlib" ,zlib)))
    (arguments
     `(#:configure-flags
       ;; XXX This ‘documentation’ is for developers, and fails informatively:
       ;; Error in gtkdoc helper script: 'gtkdoc-mkhtml' failed with status 5
       (list "-Ddocs=false"
             "-Dintrospection=false")))
    (home-page "https://wiki.gnome.org/msitools") ; no dedicated home page
    (synopsis "Microsoft Cabinet file manipulation library")
    (description
     "The libgcab library provides GObject functions to read, write, and modify
Microsoft cabinet (.@dfn{CAB}) files.")
    (license (list license:gpl2+        ; tests/testsuite.at
                   license:lgpl2.1+)))) ; the rest

(define-public msitools
  (package
    (name "msitools")
    (version "0.100")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/msitools/"
                                  version "/msitools-" version ".tar.xz"))
              (sha256
               (base32
                "1skq17qr2ic4qr3779j49byfm8rncwbsq9rj1a33ncn2m7isdwdv"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("bison" ,bison)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("gcab" ,gcab)
       ("glib" ,glib)
       ("libgsf" ,libgsf)
       ("libxml2" ,libxml2)
       ("uuid" ,util-linux)))
    (home-page "https://wiki.gnome.org/msitools")
    (synopsis "Windows Installer file manipulation tool")
    (description
     "msitools is a collection of command-line tools to inspect, extract, build,
and sign Windows@tie{}Installer (.@dfn{MSI}) files.  It aims to be a solution
for packaging and deployment of cross-compiled Windows applications.")
    (license license:lgpl2.1+)))

(define-public libostree
  (package
    (name "libostree")
    (version "2019.3")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/ostreedev/ostree/releases/download/v"
                    (version-major+minor version) "/libostree-" version ".tar.xz"))
              (sha256
               (base32
                "1r07yqbc9iiq0lzv1pryppd35fv695ym8r040msbfc93pmiy77y0"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'pre-check
           (lambda _
             ;; Don't try to use the non-existing '/var/tmp' as test
             ;; directory.
             (setenv "TEST_TMPDIR" (getenv "TMPDIR"))
             #t)))
       ;; XXX: fails with:
       ;;     tap-driver.sh: missing test plan
       ;;     tap-driver.sh: internal error getting exit status
       ;;     tap-driver.sh: fatal: I/O or internal error
       #:tests? #f))
    (native-inputs
     `(("attr" ,attr)                   ; for tests
       ("bison" ,bison)
       ("glib:bin" ,glib "bin")         ; for 'glib-mkenums'
       ("gobject-introspection" ,gobject-introspection)
       ("pkg-config" ,pkg-config)
       ("xsltproc" ,libxslt)))
    (inputs
     `(("avahi" ,avahi)
       ("docbook-xml" ,docbook-xml-4.2)
       ("docbook-xsl" ,docbook-xsl)
       ("e2fsprogs" ,e2fsprogs)
       ("fuse" ,fuse)
       ("glib" ,glib)
       ("gpgme" ,gpgme)
       ("libarchive" ,libarchive)
       ("libsoup" ,libsoup)
       ("util-linux" ,util-linux)))
    (home-page "https://ostree.readthedocs.io/en/latest/")
    (synopsis "Operating system and container binary deployment and upgrades")
    (description
     "@code{libostree} is both a shared library and suite of command line
tools that combines a \"git-like\" model for committing and downloading
bootable file system trees, along with a layer for deploying them and managing
the boot loader configuration.")
    (license license:lgpl2.0+)))

(define-public flatpak
  (package
   (name "flatpak")
   (version "1.4.3")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "https://github.com/flatpak/flatpak/releases/download/"
                         version "/flatpak-" version ".tar.xz"))
     (sha256
      (base32
       "11bfxmv8pxlb5x0lb2rsl45615fzfvq5r6wldf0l6ab2ngryd7i7"))))

   ;; Wrap 'flatpak' so that GIO_EXTRA_MODULES is set, thereby allowing GIO to
   ;; find the TLS backend in glib-networking.
   (build-system glib-or-gtk-build-system)

   (arguments
    '(#:tests? #f ;; Tests fail due to trying to create files where it can't.
      #:configure-flags (list
                         "--enable-documentation=no" ;; FIXME
                         "--enable-system-helper=no"
                         "--localstatedir=/var"
                         (string-append "--with-system-bubblewrap="
                                        (assoc-ref %build-inputs "bubblewrap")
                                        "/bin/bwrap"))))
   (native-inputs `(("bison" ,bison)
                    ("gettext" ,gnu-gettext)
                    ("glib:bin" ,glib "bin") ; for glib-mkenums + gdbus-codegen
                    ("gobject-introspection" ,gobject-introspection)
                    ("libcap" ,libcap)
                    ("pkg-config" ,pkg-config)))
   (propagated-inputs `(("glib-networking" ,glib-networking)
                        ("gnupg" ,gnupg)
                        ("gsettings-desktop-schemas"
                         ,gsettings-desktop-schemas)))
   (inputs `(("appstream-glib" ,appstream-glib)
             ("bubblewrap" ,bubblewrap)
             ("dconf" ,dconf)
             ("fuse" ,fuse)
             ("gdk-pixbuf" ,gdk-pixbuf)
             ("gpgme" ,gpgme)
             ("json-glib" ,json-glib)
             ("libarchive" ,libarchive)
             ("libostree" ,libostree)
             ("libseccomp" ,libseccomp)
             ("libsoup" ,libsoup)
             ("libxau" ,libxau)
             ("libxml2" ,libxml2)
             ("util-linux" ,util-linux)))
   (home-page "https://flatpak.org")
   (synopsis "System for building, distributing, and running sandboxed desktop
applications")
   (description "Flatpak is a system for building, distributing, and running
sandboxed desktop applications on GNU/Linux.")
   (license license:lgpl2.1+)))
