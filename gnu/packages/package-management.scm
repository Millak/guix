;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013-2025 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015, 2017, 2020, 2021, 2022, 2023 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017 Muriithi Frederick Muriuki <fredmanglis@gmail.com>
;;; Copyright © 2017, 2018 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2017 Roel Janssen <roel@gnu.org>
;;; Copyright © 2017–2022 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2018, 2019 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2018 Sou Bunnbu <iyzsong@member.fsf.org>
;;; Copyright © 2018, 2019 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2019-2024 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2019 Jonathan Brielmaier <jonathan.brielmaier@web.de>
;;; Copyright © 2020 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2020, 2023, 2024 Janneke Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2020 Giacomo Leidi <therewasa@fishinthecalculator.me>
;;; Copyright © 2020 Jesse Gibbons <jgibbons2357+guix@gmail.com>
;;; Copyright © 2020 Martin Becze <mjbecze@riseup.net>
;;; Copyright © 2020 Vincent Legoll <vincent.legoll@gmail.com>
;;; Copyright © 2021 Ivan Gankevich <i.gankevich@spbu.ru>
;;; Copyright © 2021, 2022, 2023, 2025 Maxim Cournoyer <maxim@guixotic.coop>
;;; Copyright © 2021 John Kehayias <john.kehayias@protonmail.com>
;;; Copyright © 2022, 2023 Zhu Zihao <all_but_last@163.com>
;;; Copyright © 2023 jgart <jgart@dismail.de>
;;; Copyright © 2023 Wojtek Kosior <koszko@koszko.org>
;;; Copyright © 2023 Mădălin Ionel Patrașcu <madalinionel.patrascu@mdc-berlin.de>
;;; Copyright © 2024 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2024 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2024 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2025 aurtzy <aurtzy@gmail.com>
;;; Copyright © 2025 Tomás Ortín Fernández <quanrong@mailbox.org>
;;; Copyright © 2025 Sharlatan Hellseher <sharlatanus@gmail.com>
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
  #:use-module (gnu artwork)
  #:use-module (gnu packages)
  #:use-module (gnu packages acl)
  #:use-module (gnu packages attr)
  #:use-module (gnu packages avahi)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bdw-gc)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages bootstrap)          ;for 'bootstrap-guile-origin'
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages containers)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages cpio)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages debian)
  #:use-module (gnu packages dejagnu)
  #:use-module (gnu packages dbm)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages file)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-check)
  #:use-module (gnu packages golang-vcs)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages hardware)
  #:use-module (gnu packages hurd)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages jupyter)
  #:use-module (gnu packages less)
  #:use-module (gnu packages libedit)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lisp)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages man)
  #:use-module (gnu packages markup)
  #:use-module (gnu packages nettle)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages ninja)
  #:use-module (gnu packages node)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages patchutils)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-check)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages polkit)
  #:use-module (gnu packages popt)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-compression)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages ruby-check)
  #:use-module (gnu packages ruby-xyz)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages virtualization)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages version-control)
  #:autoload   (guix build-system channel) (channel-build-system)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (guix build-system guile)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (guix build-system ruby)
  #:use-module (guix build-system trivial)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:autoload   (guix describe) (current-channels)
  #:autoload   (guix channels) (channel?
                                guix-channel?
                                repository->guix-channel)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages selinux)
  #:use-module (gnu packages elf)
  #:use-module ((guix search-paths) #:select ($SSL_CERT_DIR $SSL_CERT_FILE $GUIX_EXTENSIONS_PATH))
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

;; NOTE: The commit IDs used here form a linked list threaded through the git
;; history. In a phenomenon known as boot-stripping, not only the head of this
;; list is used, but also a few older versions, when a guix from this package is
;; used to build something also depending on guix.
;;
;; Therefore, if, by accident, you set this package to a non-existent commit ID,
;; it is insufficient to simply correct it with the latest commit.
;; Instead, please push one commit that rolls back Guix to before the mistake,
;; and then another that points to the first one. That way, the faulty commit
;; won't appear on the linked list.
;;
;; If you are updating this package because it fails to build, you need to
;; actually update it *twice*, as the installer is pointing to the N-1 guix
;; package revision.
(define-public guix
  ;; Latest version of Guix, which may or may not correspond to a release.
  ;; Note: the 'update-guix-package.scm' script expects this definition to
  ;; start precisely like this.
  (let ((version "1.5.0")
        (commit "230aa373f315f247852ee07dff34146e9b480aec")
        (revision 0))
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
                      (url "https://git.guix.gnu.org/guix.git")
                      (commit commit)))
                (sha256
                 (base32
                  "00398yxjhq9pa03knkjjrih4vc4n5xg9xjykiscndr4c84qhj3zy"))
                (file-name (string-append "guix-" version "-checkout"))))
      (build-system gnu-build-system)
      (arguments
       `(;; For reproducibility, see <https://issues.guix.gnu.org/74204>.
         #:parallel-build? #false
         #:configure-flags (list

                            ;; Provide channel metadata for 'guix describe'.
                            ;; Don't pass '--with-channel-url' and
                            ;; '--with-channel-introduction' and instead use
                            ;; the defaults.
                            ,(string-append "--with-channel-commit=" commit)

                            "--localstatedir=/var"
                            "--sysconfdir=/etc"
                            (string-append "--with-bash-completion-dir="
                                           (assoc-ref %outputs "out")
                                           "/etc/bash_completion.d")
                            (string-append "--with-apparmor-profile-dir="
                                           (assoc-ref %outputs "out")
                                           "/etc/apparmor.d")

                            ;; Set 'DOT_USER_PROGRAM' to the empty string so
                            ;; we don't keep a reference to Graphviz, whose
                            ;; closure is pretty big (too big for the Guix
                            ;; system installation image.)
                            "ac_cv_path_DOT_USER_PROGRAM=dot"

                            ;; When cross-compiling, 'git' is not in $PATH
                            ;; (because it's not a native input).  Thus,
                            ;; always explicitly pass its file name.
                            (string-append "ac_cv_path_GIT="
                                           (search-input-file %build-inputs
                                                              "/bin/git"))

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

                        ;; Install SysV init files to $(prefix)/etc rather
                        ;; than to /etc.
                        (substitute* "nix/local.mk"
                          (("^sysvinitservicedir = .*$")
                           (string-append "sysvinitservicedir = \
$(prefix)/etc/init.d\n")))

                        ;; Install OpenRC init files to $(prefix)/etc rather
                        ;; than to /etc.
                        (substitute* "nix/local.mk"
                          (("^openrcservicedir = .*$")
                           (string-append "openrcservicedir = \
$(prefix)/etc/openrc\n")))

                        (invoke "sh" "bootstrap")))
                    ,@(if (target-riscv64?)
                        `((add-after 'unpack 'use-correct-guile-version-for-tests
                            (lambda _
                              (substitute* "tests/gexp.scm"
                                (("2\\.0") "3.0")))))
                        '())
                    ,@(if (system-hurd?)
                          `((add-after 'unpack 'disable-tests/hurd
                              (lambda _
                                (substitute* "Makefile.am"
                                  (("tests/derivations.scm") "")
                                  (("tests/grafts.scm") "")
                                  (("tests/graph.scm") "")
                                  (("tests/lint.scm") "")
                                  (("tests/nar.scm") "")
                                  (("tests/offload.scm") "")
                                  (("tests/pack.scm") "")
                                  (("tests/packages.scm") "")
                                  (("tests/processes.scm") "")
                                  (("tests/publish.scm") "")
                                  (("tests/pypi.scm") "")
                                  (("tests/size.scm") "")
                                  (("tests/store.scm") "")
                                  (("tests/substitute.scm") "")
                                  (("tests/syscalls.scm") "")
                                  (("tests/union.scm") "")
                                  (("tests/guix-build.sh") "")
                                  (("tests/guix-build-branch.sh") "")
                                  (("tests/guix-hash.sh") "")
                                  (("tests/guix-locate.sh") "")
                                  (("tests/guix-pack.sh") "")
                                  (("tests/guix-pack-relocatable.sh") "")
                                  (("tests/guix-package-aliases.sh") "")
                                  (("tests/guix-package-net.sh") "")
                                  (("tests/guix-home.sh") "")
                                  (("tests/guix-archive.sh") "")
                                  (("tests/guix-environment.sh") "")
                                  (("tests/guix-package.sh") "")
                                  (("tests/guix-refresh.sh") "")
                                  (("tests/guix-shell.sh") "")
                                  (("tests/guix-shell-export-manifest.sh") "")
                                  (("tests/guix-system.sh") "")
                                  (("tests/guix-graph.sh") "")
                                  (("tests/guix-gc.sh") "")
                                  (("tests/guix-daemon.sh") "")))))
                        '())
                    (add-before 'build 'use-host-compressors
                      (lambda* (#:key inputs target #:allow-other-keys)
                        (when target
                          ;; Use host compressors.
                          (let ((bzip2 (assoc-ref inputs "bzip2"))
                                (gzip (assoc-ref inputs "gzip"))
                                (xz (assoc-ref inputs "xz")))
                            (substitute* "guix/config.scm"
                              (("\"[^\"]*/bin/bzip2")
                               (string-append "\"" bzip2 "/bin/bzip2"))
                              (("\"[^\"]*/bin/gzip") gzip
                               (string-append "\"" gzip "/bin/gzip"))
                              (("\"[^\"]*/bin//xz")
                               (string-append "\"" xz "/bin/xz")))))))
                    (add-before 'build 'set-font-path
                      (lambda* (#:key native-inputs inputs #:allow-other-keys)
                        ;; Tell 'dot' where to look for fonts.
                        (setenv "XDG_DATA_DIRS"
                                (dirname
                                 (search-input-directory (or native-inputs inputs)
                                                         "share/fonts")))))
                    (add-before 'check 'copy-bootstrap-guile
                      (lambda* (#:key system target inputs #:allow-other-keys)
                        ;; Copy the bootstrap guile tarball in the store
                        ;; used by the test suite.
                        (define (intern file recursive?)
                          ;; Note: don't use 'guix download' here because we
                          ;; need to set the 'recursive?' argument.
                          (define base
                            (strip-store-file-name file))

                          (define code
                            `(begin
                               (use-modules (guix))
                               (with-store store
                                 (let* ((item (add-to-store store ,base
                                                            ,recursive?
                                                            "sha256" ,file))
                                        (root (string-append "/tmp/gc-root-"
                                                             (basename item))))
                                   ;; Register a root so that the GC tests
                                   ;; don't delete those.
                                   (symlink item root)
                                   (add-indirect-root store root)))))

                          (invoke "./test-env" "guile" "-c"
                                  (object->string code)))

                        (unless target
                          (intern (assoc-ref inputs "boot-guile") #f)

                          ;; On x86_64 some tests need the i686 Guile.
                          (when (and (not target)
                                     (string=? system "x86_64-linux"))
                            (intern (assoc-ref inputs "boot-guile/i686") #f))

                          ;; Copy the bootstrap executables.
                          (for-each (lambda (input)
                                      (intern (assoc-ref inputs input) #t))
                                    '("bootstrap/bash" "bootstrap/mkdir"
                                      "bootstrap/tar" "bootstrap/xz")))))
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
                             "exit 77\n")))))
                    ,@(if (target-arm32?)
                          `((add-after
                                'disable-failing-tests
                                'disable-failing-tests-on-arm32
                              ;; XXX FIXME: These tests fail on armhf architecture,
                              ;; see <https://codeberg.org/guix/guix/issues/5078>.
                              (lambda _
                                (substitute* "tests/syscalls.scm"
                                  (("^\\(test-equal \"safe-clone and unshare succeeds\"" all)
                                   (string-append "(test-skip 1)\n" all))
                                  (("^\\(test-equal \"clone and unshare triggers EINVAL\"" all)
                                   (string-append "(test-skip 1)\n" all))))))
                          '())
                    (add-before 'check 'set-SHELL
                      (lambda _
                        ;; 'guix environment' tests rely on 'SHELL' having a
                        ;; correct value, so set it.
                        (setenv "SHELL" (which "sh"))))
                    (add-after 'install 'wrap-program
                      (lambda* (#:key inputs native-inputs outputs target
                                #:allow-other-keys)
                        ;; Make sure the 'guix' command finds GnuTLS,
                        ;; Guile-JSON, and Guile-Git automatically.
                        (let* ((out    (assoc-ref outputs "out"))
                               (guile  (assoc-ref (or native-inputs inputs)
                                                  "guile"))
                               (avahi  (assoc-ref inputs "guile-avahi"))
                               (gcrypt (assoc-ref inputs "guile-gcrypt"))
                               (guile-lib   (assoc-ref inputs "guile-lib"))
                               (json   (assoc-ref inputs "guile-json"))
                               (sqlite (assoc-ref inputs "guile-sqlite3"))
                               (zlib   (assoc-ref inputs "guile-zlib"))
                               (lzlib  (assoc-ref inputs "guile-lzlib"))
                               (zstd   (assoc-ref inputs "guile-zstd"))
                               (git    (assoc-ref inputs "guile-git"))
                               (bs     (assoc-ref inputs
                                                  "guile-bytestructures"))
                               (ssh    (assoc-ref inputs "guile-ssh"))
                               (gnutls (assoc-ref inputs "guile-gnutls"))
                               (disarchive (assoc-ref inputs "disarchive"))
                               (bzip2 (assoc-ref inputs "guile-bzip2"))
                               (lzma (assoc-ref inputs "guile-lzma"))
                               (locales (assoc-ref inputs "glibc-utf8-locales"))
                               (deps   (list gcrypt json sqlite gnutls git
                                             bs ssh zlib lzlib zstd guile-lib
                                             disarchive bzip2 lzma))
                               (deps*  (if avahi (cons avahi deps) deps))
                               (effective
                                (read-line
                                 (open-pipe* OPEN_READ
                                             (string-append guile "/bin/guile")
                                             "-c" "(display (effective-version))")))
                               (path   (map (cut string-append <>
                                                 "/share/guile/site/"
                                                 effective)
                                            (delete #f deps*)))
                               (gopath (map (cut string-append <>
                                                 "/lib/guile/" effective
                                                 "/site-ccache")
                                            (delete #f deps*)))
                               (locpath (string-append locales "/lib/locale")))

                          ;; Modify 'guix' directly instead of using
                          ;; 'wrap-program'.  This avoids the indirection
                          ;; through Bash, which in turn avoids getting Bash's
                          ;; own locale warnings.
                          (substitute* (string-append out "/bin/guix")
                            (("!#")
                             (string-append
                              "!#\n\n"
                              (object->string
                               `(set! %load-path (append ',path %load-path)))
                              "\n"
                              (object->string
                               `(set! %load-compiled-path
                                  (append ',gopath %load-compiled-path)))
                              "\n"
                              (object->string
                               `(let ((path (getenv "GUIX_LOCPATH")))
                                  (setenv "GUIX_LOCPATH"
                                          (if path
                                              (string-append path ":" ,locpath)
                                              ,locpath))))
                              "\n\n"))))))

                    ;; The 'guix' executable has 'OUT/libexec/guix/guile' as
                    ;; its shebang; that should remain unchanged, thus remove
                    ;; the 'patch-shebangs' phase, which would otherwise
                    ;; change it to 'GUILE/bin/guile'.
                    (delete 'patch-shebangs))))
      (native-inputs `(("locales" ,(libc-utf8-locales-for-target
                                    (%current-system)))
                       ("pkg-config" ,pkg-config)

                       ;; Guile libraries are needed here for
                       ;; cross-compilation.
                       ("guile" ,guile-3.0-latest) ;for faster builds
                       ("guile-gnutls" ,guile-gnutls)
                       ,@(if (target-hurd?)
                             '()
                             `(("guile-avahi" ,guile-avahi)))
                       ("guile-gcrypt" ,guile-gcrypt)
                       ("guile-json" ,guile-json-4)
                       ("guile-lib" ,guile-lib)
                       ("guile-sqlite3" ,guile-sqlite3)
                       ("guile-zlib" ,guile-zlib)
                       ("guile-lzlib" ,guile-lzlib)
                       ("guile-zstd" ,guile-zstd)
                       ("guile-ssh" ,guile-ssh)
                       ("guile-git" ,guile-git)
                       ("guile-semver" ,guile-semver)

                       ;; XXX: Keep the development inputs here even though
                       ;; they're unnecessary, just so that 'guix environment
                       ;; guix' always contains them.
                       ("autoconf" ,autoconf)
                       ("automake" ,automake)
                       ("gettext" ,gettext-minimal)
                       ("texinfo" ,texinfo)
                       ("graphviz" ,graphviz-minimal)
                       ("font-ghostscript" ,font-ghostscript) ;fonts for 'dot'
                       ("help2man" ,help2man)
                       ("po4a" ,po4a-minimal)))
      (inputs
       `(("bash-minimal", bash-minimal)
         ("bzip2" ,bzip2)
         ("gzip" ,gzip)
         ("sqlite" ,sqlite)
         ("libgcrypt" ,libgcrypt)
         ("zlib" ,zlib)

         ("guile" ,guile-3.0-latest)

         ;; Some of the tests use "unshare" when it is available.
         ("util-linux" ,util-linux)
         ,@(if (target-linux?)
               `(("slirp4netns" ,slirp4netns))
               '())

         ;; Many tests rely on the 'guile-bootstrap' package, which is why we
         ;; have it here.
         ("boot-guile" ,(bootstrap-guile-origin (%current-system)))
         ,@(if (and (not (%current-target-system))
                    (string=? (%current-system) "x86_64-linux"))
               `(("boot-guile/i686" ,(bootstrap-guile-origin "i686-linux")))
               '())
         ,@(if (%current-target-system)
               `(("xz" ,xz))
               '())

         ;; Tests also rely on these bootstrap executables.
         ("bootstrap/bash" ,(bootstrap-executable "bash" (%current-system)))
         ("bootstrap/mkdir" ,(bootstrap-executable "mkdir" (%current-system)))
         ("bootstrap/tar" ,(bootstrap-executable "tar" (%current-system)))
         ("bootstrap/xz" ,(bootstrap-executable "xz" (%current-system)))

         ("disarchive" ,disarchive)               ;for 'guix perform-download'
         ("guile-bzip2" ,guile-bzip2)             ;for Disarchive
         ("guile-lzma" ,guile-lzma)               ;for Disarchive

         ("git-minimal" ,git-minimal)             ;for 'guix perform-download'

         ("glibc-utf8-locales" ,(libc-utf8-locales-for-target))))
      (propagated-inputs
       `(("guile-gnutls" ,guile-gnutls)
         ;; Avahi requires "glib" which doesn't cross-compile yet.
         ,@(if (target-hurd?)
               '()
               `(("guile-avahi" ,guile-avahi)))
         ("guile-gcrypt" ,guile-gcrypt)
         ("guile-json" ,guile-json-4)
         ("guile-lib" ,guile-lib)
         ("guile-semver" ,guile-semver)
         ("guile-sqlite3" ,guile-sqlite3)
         ("guile-ssh" ,guile-ssh)
         ("guile-git" ,guile-git)
         ("guile-zlib" ,guile-zlib)
         ("guile-lzlib" ,guile-lzlib)
         ("guile-zstd" ,guile-zstd)))
      (native-search-paths
       (list (search-path-specification
              (variable "GUIX_EXTENSIONS_PATH")
              (files '("share/guix/extensions")))
             ;; (guix git) and (guix build download) honor this variable whose
             ;; name comes from OpenSSL.
             $SSL_CERT_DIR))
      (home-page "https://www.gnu.org/software/guix/")
      (synopsis "Functional package manager for installed software packages and versions")
      (description
       "GNU Guix is a functional package manager for the GNU system, and is
also a distribution thereof.  It includes a virtual machine image.  Besides
the usual package management features, it also supports transactional
upgrades and roll-backs, per-user profiles, and much more.  It is based on
the Nix package manager.")
      (license license:gpl3+))))

(define* (channel-source->package source #:key commit)
  "Return a package for the given channel SOURCE, a lowerable object."
  (package
    (inherit guix)
    (version (string-append (package-version guix) "."
                            (if commit (string-take commit 7) "")))
    (build-system channel-build-system)
    (arguments `(#:source ,source
                 #:commit ,commit))
    (inputs '())
    (native-inputs '())
    (propagated-inputs '())))

(export channel-source->package)

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
     (modify-inputs (package-native-inputs guix)
       (delete "po4a" "graphviz" "font-ghostscript" "help2man")))
    (inputs
     (modify-inputs (package-inputs guix)
       (delete "boot-guile" "boot-guile/i686" "util-linux")
       (prepend guile-gnutls guile-git guile-json-3 guile-gcrypt)))

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
           (delete 'set-font-path)
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

(define-public guix-minimal
  ;; A version of Guix which is built with the minimal set of dependencies, as
  ;; outlined in the README "Requirements" section.  Intended as a CI job, so
  ;; marked as hidden.
  (hidden-package
   (package
     (inherit guix)
     (name "guix-minimal")
     (native-inputs
      (modify-inputs (package-native-inputs guix)
        (delete "guile-ssh")))
     (propagated-inputs
      (modify-inputs (package-propagated-inputs guix)
        (delete "guile-ssh"))))))

(define-public (guix-for-channels channels)
  "Return a package corresponding to CHANNELS."
  (package
    (inherit guix)
    (source (find guix-channel? channels))
    (build-system channel-build-system)
    (arguments
     `(#:channels ,(remove guix-channel? channels)))
    (inputs '())
    (native-inputs '())
    (propagated-inputs '())))

(define-public current-guix-package
  ;; This parameter allows callers to override the package that 'current-guix'
  ;; returns.  This is useful when 'current-guix' cannot compute it by itself,
  ;; for instance because it's not running from a source code checkout.
  ;;
  ;; The default value is obtained by creating a package from the 'guix'
  ;; channel returned by 'current-channels' or, if that's the empty list, that
  ;; returned by 'repository->guix-channel' for the current directory (which
  ;; assumes that we're running from a Git checkout).  Delay computation so
  ;; that the relevant modules can be loaded lazily.
  (make-parameter
   (delay (match (or (find guix-channel? (current-channels))
                     (repository->guix-channel
                      (current-source-directory)))
            ((? channel? source)
             (package
               (inherit guix)
               (source source)
               (build-system channel-build-system)
               (inputs '())
               (native-inputs '())
               (propagated-inputs '())))
            (#f #f)))))

(define-public current-guix
  (lambda ()
    "Return a package representing the currently-used Guix.  It can be
overridden by setting the 'current-guix-package' parameter."
    (match (current-guix-package)
      ((? promise? package) (force package))
      (package package))))

(define-public guix-icons
  (package
    (name "guix-icons")
    (version "0.1")
    (source %artwork-repository)
    (build-system trivial-build-system)
    (native-inputs
     (list imagemagick))
    (arguments
     `(#:modules ((guix build utils)
                  (gnu build svg))

       ;; There's no point in cross-compiling: a native build gives the same
       ;; result, independently of the system type.
       #:target #f

       #:builder
       ,(with-extensions (list guile-rsvg guile-cairo)
          #~(begin
              (use-modules (guix build utils)
                           (gnu build svg))
              (let* ((logo (string-append #$source "/logo/Guix.svg"))
                     (logo-white
                      (string-append #$source
                                     "/logo/Guix-horizontal-white.svg"))
                     (theme "hicolor")
                     (category "apps")
                     (sizes '(16 24 32 48 64 72 96 128 256 512 1024))
                     (icons
                      (string-append #$output "/share/icons/" theme))
                     (scalable-dir
                      (string-append icons "/scalable/" category)))
                (setenv "XDG_CACHE_HOME" (getcwd))

                ;; Create the scalable icon files.
                (mkdir-p scalable-dir)
                (copy-file logo
                           (string-append scalable-dir "/guix-icon.svg"))
                (copy-file logo-white
                           (string-append scalable-dir
                                          "/guix-white-icon.svg"))

                ;; Create the fixed dimensions icon files.
                (for-each
                 (lambda (size)
                   (let* ((dimension
                           (format #f "~ax~a" size size))
                          (file
                           (string-append icons "/" dimension "/" category
                                          "/guix-icon.png")))
                     (mkdir-p (dirname file))
                     (svg->png logo file
                               #:width size
                               #:height size)))
                 sizes))))))
    (home-page "https://www.gnu.org/software/guix/")
    (synopsis "GNU Guix icons")
    (description "This package contains GNU Guix icons organized according to
the Icon Theme Specification.  They can be used by applications querying the
GTK icon cache for instance.")
    (license license:cc-by-sa4.0)))

(define-public guix-backgrounds
  (package
    (name "guix-backgrounds")
    (version "0.1")
    (source %artwork-repository)
    (build-system copy-build-system)
    (arguments
     (list #:install-plan
           #~'(("backgrounds" "share/backgrounds/guix" #:exclude ("README")))))
    (home-page "https://www.gnu.org/software/guix/")
    (synopsis "Background images for GNU Guix")
    (description "The SVG files in this directory are intended to be used as
backgrounds for different components of the GNU system like login managers and
desktop environments.  The backgrounds are available in different aspect ratios
which are indicated in the file name.")
    (license (list license:public-domain license:cc-by-sa4.0))))

(define-public guix-modules
  (package
    (name "guix-modules")
    (version "0.3.0")
    (home-page "https://codeberg.org/guix-science/guix-modules")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page)
                                  (commit (string-append "v" version))))
              (file-name (string-append "guix-modules-" version "-checkout"))
              (sha256
               (base32
                "1cdpxmhrbbazr4z53rpk6ssk3clpv4j16ivha7lr4nqg48mxha9q"))))
    (build-system guile-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (add-after 'build 'move-to-extension-directory
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (target (string-append
                                      out
                                      "/share/guix/extensions/module.scm")))
                        (mkdir-p (dirname target))
                        (rename-file (car (find-files out "module.scm"))
                                     target)))))))
    (native-inputs (list (lookup-package-input guix "guile") guix))
    (synopsis "Generate environment modules from Guix packages")
    (description
     "Guix-Modules is an extension of Guix that provides a new @command{guix
module} command.  The @command{guix module create} sub-command creates
@dfn{environment modules}, allowing you to manipulate software environments
with the @command{module} command commonly found on @acronym{HPC,
high-performance computing} clusters.")
    (license license:gpl3+)))

(define-public guix-xsearch
  (package
    (name "guix-xsearch")
    (version "2.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://codeberg.org/Baleine/guix-xsearch.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1rx1984841jv6y4wkhlfgdjylfffl6zl07scl1l7wgm5kmaqc6br"))))
    (build-system guile-build-system)
    (arguments
     (list
      #:source-directory "src"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'build 'add-extension-to-search-path
            (lambda _
              (with-directory-excursion #$output
                (mkdir-p "share/guix/extensions")
                (symlink (string-append #$output
                          "/share/guile/site/3.0/guix/extensions/xsearch.scm")
                         "share/guix/extensions/xsearch.scm")))))))
    ;; Avoid setting guix as propagated so that we use the user’s profile.
    (native-inputs (list guile-3.0
                         guile-xapian
                         guix))
    (propagated-inputs (list guile-xapian))
    ;; This is very important since we want the extension to be available
    ;; without having to add a vanilla guix to the current profile.
    (native-search-paths
     (list $GUIX_EXTENSIONS_PATH))
    (home-page "https://codeberg.org/Baleine/guix-xsearch")
    (synopsis "Extension for Guix to provide faster search using Xapian")
    (description
     "The Guix Xsearch extension is a new implementation of Guix search sped up
by using a Xapian cache.")
    (license (list license:gpl3+ license:cc0))))


;;;
;;; Other tools.
;;;

(define-public nix
  (package
    (name "nix")
    (version "2.25.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/NixOS/nix")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "150678l5bw8d8gaagsqy9fxlrxhvdx3cmphbn6b2l2c7pa3d06pp"))
       (patches
        (search-patches "nix-dont-build-html-doc.diff"))))
    (build-system gnu-build-system)
    (arguments
     (list
      ;;; Only run functional tests.
      #:test-target "installcheck"
      #:configure-flags #~(list "--sysconfdir=/etc" "--enable-gc")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'inject-config.sub
            (lambda* (#:key inputs #:allow-other-keys)
              (symlink (search-input-file inputs "bin/config.sub")
                       "config/config.sub")))
          (replace 'install
            ;; Don't try & fail to create subdirectories in /etc, but keep them
            ;; in the output as examples.
            (lambda* (#:key (make-flags '()) outputs #:allow-other-keys)
              (let ((etc (string-append #$output "/etc")))
                (apply invoke "make" "install"
                       (string-append "sysconfdir=" etc)
                       (string-append "profiledir=" etc "/profile.d")
                       make-flags))))
          (delete 'check)
          (add-after 'install 'check
            (lambda args
              ;; A few tests expect the environment variable NIX_STORE to be
              ;; "/nix/store"
              (let ((original-NIX_STORE (getenv "NIX_STORE")))
                (dynamic-wind
                  (lambda ()
                    (setenv "NIX_STORE" "/nix/store"))
                  (lambda ()
                    (apply (assoc-ref %standard-phases 'check) args))
                  (lambda ()
                    (setenv "NIX_STORE" original-NIX_STORE))))))
          (add-after 'unpack 'skip-failing-tests
            (lambda _
              (substitute* "Makefile"
                (("tests/functional/git-hashing/local.mk")
                 ""))
              (substitute* "tests/functional/local.mk"
                (((string-append " (" (string-join
                                       '("chroot-store"
                                         "debugger"
                                         "fmt"
                                         "nix-profile"
                                         "plugins"
                                         "shell")
                                       "|") ")\\.sh"))
                 ""))
              (substitute* "tests/functional/flakes/local.mk"
                ((".*config\\.sh.*")
                 "")))))))
    (native-inputs
     (list autoconf
           autoconf-archive
           automake
           bison
           config
           flex
           googletest
           jq
           libtool
           man-db
           pkg-config
           rapidcheck
           util-linux)) ; for unshare
    (inputs
     (append (list boost
                   brotli
                   bzip2
                   curl
                   editline
                   libarchive
                   libblake3
                   libgc
                   libgit2-1.9
                   libseccomp
                   libsodium
                   lowdown
                   nlohmann-json
                   openssl
                   sqlite
                   toml11
                   xz
                   zlib)
             (if (or (target-x86-64?)
                     (target-x86-32?))
                 (list libcpuid)
                 '())))
    (home-page "https://nixos.org/")
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
    (version "2.4.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/stow/stow-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "02vqi0mwvs3z3bgyn2411bgnjxlw2qip56kax2zh6wr0zisiwrra"))))
    (build-system gnu-build-system)
    (inputs
     (list perl))
    (native-inputs
     (list perl perl-test-simple perl-test-output perl-capture-tiny
           perl-io-stringy))
    (home-page "https://www.gnu.org/software/stow/")
    (synopsis "Managing installed software packages")
    (description
     "GNU Stow is a symlink manager.  It generates symlinks to directories
of data and makes them appear to be merged into the same directory.  It is
typically used for managing software packages installed from source, by
letting you install them apart in distinct directories and then create
symlinks to the files in a common directory such as /usr/local.")
    (license license:gpl3+)))

(define-public xstow
  (package
    (name "xstow")
    (version "1.0.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/xstow/xstow-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "1vy6lcswpkixh7h5mvsmq2wbcih6lpsmcva3m7v6f5npllciy13g"))))
    (build-system gnu-build-system)
    (synopsis "Replacement of GNU Stow written in C++")
    (description
     "XStow is a replacement of GNU Stow written in C++.  It supports all
features of Stow with some extensions.")
    (home-page "https://xstow.sourceforge.net/")
    (license license:gpl2)))

(define-public rpm
  (package
    (name "rpm")
    (version "4.20.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://ftp.rpm.org/releases/rpm-"
                                  (version-major+minor version) ".x/rpm-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "0kqjc4k679h4s47gx4wxi049yvy9hpjcijvinqx56r43cc97wr2j"))))
    (outputs '("out" "debug"))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f                       ;tests require FHS environment
      #:configure-flags
      #~(list
         ;; TODO: Add rpm-sequoia
         "-DWITH_SEQUOIA=OFF"
         #$@(if (this-package-native-input "rpmpgp_legacy")
                #~("-DWITH_LEGACY_OPENPGP=ON")
                #~()))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'unpack-rpmpgp_legacy
            (lambda _
              (copy-recursively
               #$(this-package-native-input "rpmpgp_legacy")
               "rpmio/rpmpgp_legacy")))
          (add-after 'unpack 'fix-install
            (lambda _
              (let ((site
                     (string-append
                      #$output
                      "/lib/python"
                      #$(version-major+minor
                         (package-version
                          (this-package-native-input "python")))
                      "/site-packages")))
                (substitute* "python/CMakeLists.txt"
                  (("\\$[{]Python3_SITEARCH[}]")
                   site)))
              (substitute* "plugins/CMakeLists.txt"
                (("\\$[{]dbus-1_DATADIR[}]")
                 (string-append #$output "/share")))))
          (add-after 'install 'install-macros-file
            (lambda _
              (define macros (string-append #$output "/etc/rpm/macros"))
              (mkdir-p (dirname macros))
              (call-with-output-file macros
                (lambda (p)
                  (format p "\
%_var			/var
%_localstatedir		/var~%"))))))))
    (native-inputs
     (list pkg-config
           python
           gettext-minimal
           (origin
             (method git-fetch)
             (uri (git-reference
                    (url "https://github.com/rpm-software-management/rpmpgp_legacy")
                    (commit "1.1")))
             (file-name "rpmpgp_legacy")
             (sha256
              (base32 "0z29bdjfd5p0ygn2g9w3xjc4fx4ba8rcikk28acjl7xwfgfsa4pd")))))
    (inputs
     (list bzip2
           readline
           file
           libarchive
           libgcrypt
           dbus
           lua
           sqlite
           xz
           zlib
           elfutils
           acl
           audit
           libselinux
           libcap
           `(,zstd "lib")))
    (propagated-inputs
     ;; popt is listed in the 'Requires' of rpm.pc.
     (list popt))
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

(define-public bffe
  (let ((commit "e3226ffbdb9b615957b4815956a70bc6ef9ab175")
        (revision "16"))
    (package
      (name "bffe")
      (version (git-version "0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://codeberg.org/guix/bffe.git")
                      (commit commit)))
                (sha256
                 (base32
                  "0d66xj0671rrrflln25yjvk4scmiw9946h344zj9mm35rm3mw207"))
                (file-name (string-append name "-" version "-checkout"))))
      (build-system gnu-build-system)
      (native-inputs
       (list pkg-config
             autoconf
             automake

             ;; Guile libraries are needed here for cross-compilation.
             guile-next
             guile-gnutls
             guile-json-4
             guix
             guix-data-service
             guix-build-coordinator
             guile-fibers-next
             guile-knots
             guile-pfds
             guile-prometheus
             guile-lib))
      (propagated-inputs
       (list guile-gnutls
             guile-json-4
             guix
             guix-data-service
             guix-build-coordinator
             guile-fibers-next
             guile-knots
             guile-pfds
             guile-prometheus
             guile-lib))
      (home-page "https://codeberg.org/guix/bffe.git")
      (synopsis "Build Farm Front-end for Guix")
      (description
       "The BFFE of Build Farm Front-end is an experimental frontend for Guix
build farms.  It works together with the Guix Data Service and Guix Build
Coordinator to submit builds and monitor the activity.

It functions as a Guile library, with the @code{run-bffe-service} procedure in
the @code{(bffe)} module as the entry point.")
      (license license:gpl3+))))

(define-public hatch
  (package
    (name "hatch")
    (version "1.9.7")   ;XXX: higher versions require python-uv
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "hatch" version))
       (sha256
        (base32 "0k32wv4kayb34wwda0xibbawm45bcmzalxmdvgfjlkzrg4hvi9qr"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; tests: 1778 passed, 107 skipped, 25 deselected
      #:test-flags
      #~(list
         ;; Tests requiring Cargo.
         ;; OSError: Executable `cargo` could not be found on PATH
         #$@(map (lambda (test)
                   (string-append "--deselect="
                                  "tests/backend/builders/test_app.py::"
                                  "TestBuildBootstrap::"
                                  test))
                 (list "test_default"
                       "test_default_build_target"
                       "test_scripts"
                       "test_scripts_build_target"
                       "test_python_version"
                       "test_pyapp_version"
                       "test_verbosity"
                       "test_local_build_with_build_target"
                       "test_local_build_no_build_target"))
         ;; Tests failing to validate date format.
         ;; assert (1980, 1, 2, 0, 0, 0) == (2020, 2, 2, 0, 0, 0)
         "--deselect=tests/backend/builders/test_custom.py::test_default"
         "--deselect=tests/backend/builders/test_custom.py::test_explicit_path"
         #$@(map (lambda (test)
                   (string-append "--deselect="
                                  "tests/backend/builders/test_wheel.py::"
                                  "TestBuildStandard::"
                                  test))
                 (list "test_default_auto_detection"
                       "test_editable_default"
                       "test_editable_default_extra_dependencies"
                       "test_editable_default_force_include"
                       "test_editable_default_force_include_option"
                       "test_editable_default_symlink"
                       "test_editable_exact"
                       "test_editable_exact_extra_dependencies"
                       "test_editable_exact_force_include"
                       "test_editable_exact_force_include_option"
                       "test_editable_exact_force_include_build_data_precedence"
                       "test_editable_pth"))
         ;; Two tests failed to compare project temporary location.
         "-k" (string-append
               "not test_project_location_basic_set_first_project"
               " and not test_project_location_complex_set_first_project"))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'pre-check
            (lambda _
              (setenv "HOME" "/tmp"))))))
    (native-inputs
     (list git-minimal
           nss-certs-for-test
           python-hatch-vcs
           python-pytest
           python-pytest-mock
           python-pytest-xdist))
    (inputs
     (list python-click
           python-hatchling-for-hatch
           python-httpx
           python-hyperlink
           python-keyring
           python-packaging
           python-pexpect
           python-platformdirs
           python-rich
           python-shellingham
           python-tomli-w
           python-tomlkit
           python-userpath
           python-virtualenv-for-hatch
           python-zstandard))
    (home-page "https://hatch.pypa.io/latest/")
    (synopsis "Python project management")
    (description "Hatch is a modern, extensible Python project manager.

Features

@itemize
@item Standardized build system with reproducible builds by default
@item Robust environment management with support for custom scripts
@item Configurable Python distribution management
@item Easy publishing to PyPI or other indexes
@item Version management
@item Configurable project generation with sane defaults
@item Responsive CLI, ~2-3x faster than equivalent tools
@end itemize")
    (license license:expat)))

(define-public pipx
  (package
    (name "pipx")
    (version "1.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pipx" version))
       (sha256
        (base32 "0q23b1iqs03hbxzr2d7mmspldx6fbqi6s5j54vljnikaw4sf2bbn"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:tests? #f))        ;no tests in PyPI archive
    (native-inputs
     (list python-hatch-vcs
           python-hatchling))
    (inputs
     (list python-argcomplete
           python-colorama
           python-packaging
           python-platformdirs
           python-tomli
           python-userpath))
    (home-page "https://pypa.github.io/pipx/")
    (synopsis "Install and run Python applications in isolated environments")
    (description
     "@code{pipx} is a tool to help you install and run end-user applications
written in Python.  It's roughly similar to JavaScript's @code{npx}, and
Trisquel's @code{apt}.")
    (license license:expat)))

(define-public poetry
  (package
    (name "poetry")
    (version "2.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "poetry" version))
       (sha256
        (base32 "1505snny5sgz9zf0wx1zqw6kqly3pi9lbsl8dnbpb55ij1kvvjgj"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; tests: 1295 passed, 9 skipped
      #:test-flags
      #~(list "--numprocesses" (number->string (min 8 (parallel-job-count)))
              ;; Network access is required.
              "--ignore=tests/console/commands/test_add.py"
              "--ignore=tests/console/commands/test_search.py"
              "--ignore=tests/console/commands/test_show.py"
              "--ignore=tests/installation/test_chef.py"
              "--ignore=tests/installation/test_chooser.py"
              "--ignore=tests/installation/test_executor.py"
              "--ignore=tests/installation/test_installer.py"
              "--ignore=tests/packages/test_direct_origin.py"
              "--ignore=tests/publishing/test_uploader.py"
              "--ignore=tests/puzzle/test_solver.py"
              "--ignore=tests/repositories/test_legacy_repository.py"
              "--ignore=tests/repositories/test_pypi_repository.py"
              "--ignore=tests/repositories/test_repository_pool.py"
              "--ignore=tests/utils/test_authenticator.py"
              "--ignore=tests/utils/test_dependency_specification.py"
              "--ignore=tests/utils/test_helpers.py"
              ;; XXX: Various incompatibility faileurs.
              "--ignore=tests/console/commands/env/test_activate.py"
              "--ignore=tests/console/commands/python/test_python_list.py"
              "--ignore=tests/console/commands/test_publish.py"
              "--ignore=tests/console/test_application_command_not_found.py"
              "--ignore=tests/inspection/test_info.py"
              "--ignore=tests/inspection/test_lazy_wheel.py"
              "--ignore=tests/puzzle/test_provider.py"
              "--ignore=tests/utils/env/python/test_manager.py"
              "--ignore=tests/utils/env/test_env.py"
              "--ignore=tests/utils/test_isolated_build.py"
              "--ignore=tests/vcs/git/test_backend.py"
              "-k" (string-join
                    (list "not test_builder_setup_generation_runs_with_pip_editable"
                          "test_check_invalid"
                          "test_create_poetry_fails_on_invalid_configuration"
                          "test_installer_with_pypi_repository"
                          "test_shell"
                          ;; RuntimeError: No lockfile found. Unable to read
                          ;; locked packages
                          "test_not_fresh_lock"
                          ;; assert False is True
                          "test_env_system_packages_are_relative_to_lib"
                          ;; poetry.inspection.info.PackageInfoError: Unable
                          ;; to determine package info for path
                          "test_info_setup_complex_calls_script")
                    " and not "))
      #:phases
      #~(modify-phases %standard-phases
          ;; Almost every dependency is pinned too strictly.
          (delete 'sanity-check))))
    (native-inputs
     (list nss-certs-for-test
           python-deepdiff
           python-httpretty
           python-pytest
           python-pytest-mock
           python-pytest-randomly
           python-pytest-xdist))
    (propagated-inputs
     (list python-cachecontrol
           python-cleo
           python-crashtest
           python-dulwich
           python-entrypoints
           python-fastjsonschema
           python-findpython
           python-importlib-metadata
           python-installer
           python-keyring
           python-packaging
           python-pbs-installer
           python-pexpect
           python-pip
           python-pkginfo
           python-platformdirs
           python-poetry-core
           python-poetry-plugin-export
           python-pypa-build
           python-pyproject-hooks
           python-requests
           python-requests-toolbelt
           python-shellingham
           python-tomli
           python-tomlkit
           python-trove-classifiers
           python-virtualenv
           python-xattr))
    (home-page "https://python-poetry.org")
    (synopsis "Python dependency management and packaging made easy")
    (description
     "Poetry is a tool for dependency management and packaging in Python.  It
allows you to declare the libraries your project depends on and it will
manage (install/update) them for you.")
    (license license:expat)))

(define-public conda
  (package
    (name "conda")
    (version "25.9.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/conda/conda")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1s8xxc8rfayfq6p3iwgp9v3hbanp30ciw7cznppn1qk1l9fy7nxj"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; tests: 1616 passed, 169 skipped, 12 xfailed, 673 warnings
      #:test-flags
      #~(list "-m" "not integration"
              "--numprocesses" (number->string (min 8 (parallel-job-count)))
              ;; Tests try to search for system level packages and fail with
              ;; error: The following packages are missing from the target
              ;; environment: patch, ca-certificates, zlib ...
              "--ignore=tests/cli/test_subcommands.py"
              ;; ModuleNotFoundError: No module named 'conda_libmamba_solver'
              "--ignore=tests/test_solvers.py"
              ;; Network access is required.
              "--ignore=tests/trust/test_signature_verification.py"
              "-k" (string-join
                    ;; XXX: Issues salad: network access, can't detect Conda
                    ;; environemnt, assertion failed; review if they may be
                    ;; fixed.
                    (list "not test_PrefixData_return_value_contract"
                          "test__get_python_info"
                          "test_auto_update_conda"
                          "test_build_version_shows_as_changed "
                          "test_cannot_rename_base_env_by_name"
                          "test_cannot_rename_base_env_by_path"
                          "test_conda_doctor_happy_path"
                          "test_conda_doctor_happy_path_verbose"
                          "test_conda_pip_interop_dependency_satisfied_by_pip"
                          "test_create_env_json"
                          "test_denylist_channels"
                          "test_dont_update_packages_with_version_constraints"
                          "test_environment"
                          "test_exit_codes"
                          "test_explicit_missing_cache_entries"
                          "test_explicit_packages"
                          "test_export_explicit_format_validation_errors"
                          "test_export_ignore_channels_flag"
                          "test_export_no_builds_format"
                          "test_export_override_channels_and_ignore_channels_"
                          "test_export_package_alphabetical_ordering"
                          "test_export_pip_dependencies_handling"
                          "test_export_preserves_channels_from_installed_"
                          "test_export_regular_format_consistency"
                          "test_export_with_pip_dependencies_integration"
                          "test_extrapolate"
                          "test_fields_all"
                          "test_fields_invalid"
                          "test_file_locking_not_supported"
                          "test_file_locking_supported"
                          "test_frozen_env_cep22"
                          "test_get_packages_behavior_with_interoperability"
                          "test_get_python_info"
                          "test_health_check_ran"
                          "test_info_envs_frozen"
                          "test_install_1"
                          "test_install_from_extracted_package"
                          "test_installer_installs_explicit"
                          "test_is_active_prefix"
                          "test_list_all_known_prefixes_with_none_values_error"
                          "test_list_argument_variations"
                          "test_list_json"
                          "test_list_package"
                          "test_list_revisions"
                          "test_load_entrypoints_importerror"
                          "test_multiline_run_command"
                          "test_post_solve_action_raises_exception"
                          "test_post_solve_invoked"
                          "test_post_transaction_raises_exception"
                          "test_pre_solve_invoked"
                          "test_pre_transaction_raises_exception"
                          "test_print_unexpected_error_message_upload_2"
                          "test_protected_dirs_error_for_rename"
                          "test_remove_all"
                          "test_remove_all_keep_env"
                          "test_run_readonly_env"
                          "test_run_returns_int"
                          "test_run_returns_zero_errorlevel"
                          "test_solve_1"
                          "test_transaction_hooks_invoked"
                          "test_update"
                          "test_validate_subdir_config"
                          "test_validate_subdir_config_invalid_subdir")
                    " and not "))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-pytest-config
            (lambda _
              (substitute* "pyproject.toml"
                (("--cov.*") ""))))
          (add-after 'unpack 'correct-python-executable-name
            (lambda _
              (let ((python #$(this-package-input "python-wrapper")))
                (substitute* "conda/core/initialize.py"
                  (("python_exe = join")
                   (format #f "python_exe = \"~a/bin/python\" #"
                           python))))))
          (add-after 'unpack 'do-not-use-python-root-as-prefix
            ;; XXX: Proper wrap CONDA_EXE, CONDA_ENVS_PATH, CONDA_PKGS_DIRS.
            (lambda _
              (let ((python #$(this-package-input "python-wrapper")))
                (substitute* "tests/core/test_initialize.py"
                  (("\"\"\"\\) % conda_prefix")
                   (format #f "\"\"\") % ~s" python))
                  (("CONDA_PYTHON_EXE \"%s\"' % join\\(conda_prefix")
                   (format #f "CONDA_PYTHON_EXE \"%s\"' % join(~s"
                           python))
                  (("conda_prefix = abspath\\(sys.prefix\\)")
                   (format #f "conda_prefix = abspath(~s)" #$output)))
                (substitute* "conda/base/context.py"
                  (("os.chdir\\(sys.prefix\\)")
                   (format #f "os.chdir(~s)" #$output))
                  (("sys.prefix, '.condarc'")
                   (format #f "~s, '.condarc'" #$output))
                  (("return abspath\\(sys.prefix\\)")
                   (format #f "return abspath(~s)" #$output))
                  (("os.path.join\\(sys.prefix, bin_dir, exe\\)")
                   (format #f "\"~a/bin/conda\"" #$output))
                  (("'CONDA_EXE', sys.executable")
                   (format #f "'CONDA_EXE', \"~a/bin/conda\"" #$output))))))
          (add-after 'create-entrypoints 'init
            ;; This writes a whole bunch of shell initialization files to the
            ;; prefix directory.  Many features of conda can only be used
            ;; after running "conda init".
            (lambda _
              (setenv "HOME" "/tmp")
              (invoke (string-append #$output "/bin/conda") "init")))
          (add-after 'unpack 'set-default-solver-to-classic
            ;; XXX: Remove when conda-libmamba-solver is packaged.
            (lambda _
              (substitute* "conda/base/constants.py"
                (("DEFAULT_SOLVER: Final = \"libmamba\"")
                 "DEFAULT_SOLVER: Final = \"classic\""))))
          (add-before 'build 'set-version
            (lambda _
              (setenv "SETUPTOOLS_SCM_PRETEND_VERSION" #$version)))
          (add-before 'check 'pre-check
            (lambda _
              ;; TODO: Package libsolv, libmamba, and conda-libmamba-solver:
              ;; <https://github.com/openSUSE/libsolv>,
              ;; <https://github.com/conda/conda-libmamba-solver>,
              ;; <https://github.com/mamba-org/mamba>.
              (setenv "CONDA_CLASSIC_SOLVER" "classic")
              (setenv "CONDA_DEFAULT_SOLVER" "classic")
              (setenv "CONDA_TEST_SOLVERS" "classic")
              (setenv "HOME" "/tmp")))
          (add-after 'wrap 'wrap-executable
            (lambda _
              (wrap-program (string-append #$output "/bin/conda")
                `("CONDA_EXE" prefix
                  (,(string-append #$output "/bin/conda")))
                `("CONDA_ENVS_PATH" prefix
                  (,(string-append "$HOME/.conda/envs")))
                `("CONDA_PKGS_DIRS" prefix
                  (,(string-append "$HOME/.conda/pkgs")))))))))
    (native-inputs
     (list nss-certs-for-test
           patchelf
           python-hatch-vcs
           python-anaconda-client
           python-flaky
           python-flask
           python-hatchling
           python-importlib-resources
           python-pexpect
           python-pytest
           python-pytest-mock
           python-pytest-rerunfailures
           python-pytest-split
           python-pytest-timeout
           python-pytest-xdist
           python-pytest-xprocess
           python-responses
           python-werkzeug))
    (inputs
     (list python-archspec
           python-boltons
           python-boto3
           python-charset-normalizer
           ;; python-conda-libmamba-solver       ;TODO: not packaged yet
           python-conda-package-handling
           python-conda-content-trust
           python-distro
           python-frozendict
           python-jsonpatch
           python-menuinst
           python-packaging
           python-platformdirs
           python-pluggy
           python-pycosat
           python-requests
           python-ruamel.yaml
           python-setuptools
           python-tqdm
           python-truststore
           python-wrapper               ;XXX: Check if it's actually required
           python-zstandard))
    (home-page "https://github.com/conda/conda")
    (synopsis "Cross-platform, OS-agnostic, system-level binary package manager")
    (description
     "Conda is a cross-platform, Python-agnostic binary package manager.  It
is the package manager used by Anaconda installations, but it may be used for
other systems as well.  Conda makes environments first-class citizens, making
it easy to create independent environments even for C libraries.  Conda is
written entirely in Python.")
    (license license:bsd-3)))

(define-public conan
  (package
    (name "conan")
    (version "2.7.1")
    (source
     (origin
       (method git-fetch)               ; no tests in PyPI archive
       (uri (git-reference
             (url "https://github.com/conan-io/conan")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "00mrx1ighvf6r6fy2iqxr286w3jfd0gwlzcqsw15cm9axblx5av9"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:modules '((guix build pyproject-build-system)
                  (guix build utils)
                  (ice-9 format))
      #:test-flags
      (let ((system (or (%current-target-system)
                        (%current-system))))
        #~(list "-n" (number->string (parallel-job-count))
                "-m" "not slow"
                "--ignore=test/performance/"
                ;; E   ModuleNotFoundError: No module named 'docker'
                "--ignore=test/functional/command/runner_test.py"
                ;; Disable problematic tests.
                "-k"
                (string-append
                 ;; These tests rely on networking.
                 "not download_retries_errors "
                 "and not ftp "
                 ;; These tests are for old versions of cmake.
                 "and not test_custom_cmake_3_16 "
                 "and not test_custom_cmake_3_17 "
                 "and not test_custom_cmake_3_19 "
                 ;; Guix sets PKG_CONFIG_PATH itself, which is not
                 ;; expected by the following test.
                 "and not pkg_config_path "
                 "and not compare " ;caused by newer node-semver?
                 ;; This test hard-codes a compiler version.
                 "and not test_toolchain "
                 ;; The 'test_list' tests may fail
                 ;; non-deterministically (see:
                 ;; https://github.com/conan-io/conan/issues/13583).
                 "and not test_list "
                 ;; These tests fail when Autoconf attempt to load a
                 ;; shared library in the same directory (see:
                 ;; https://github.com/conan-io/conan/issues/13577).
                 "and not test_other_client_can_link_autotools "
                 "and not test_autotools_lib_template "
                 ;; Sometimes fail: https://github.com/conan-io/conan/issues/15936
                 "and not test_basic_parallel_install "
                 ;; These tests require additional build tools
                 "and not test_premake "
                 "and not test_sconsdeps "
                 ;; Unclear why libc is not found properly
                 "and not test_profile_detect_libc "
                 #$(if (not (string-prefix? "x86_64" system))
                       ;; These tests either assume the machine is
                       ;; x86_64, or require a cross-compiler to target
                       ;; it.
                       (string-append
                        "and not cpp_package "
                        "and not exclude_code_analysis "
                        "and not cmakedeps_multi "
                        "and not locally_build_linux "
                        "and not custom_configuration "
                        "and not package_from_system "
                        "and not cross_build_command "
                        "and not test_package "
                        "and not test_same ")
                       "")
                 #$(if (not (or (string-prefix? "x86_64" system)
                                (string-prefix? "i686" system)))
                       ;; This test only works with default arch "x86",
                       ;; "x86_64", "sparc" or "sparcv9".
                       "and not settings_as_a_dict_conanfile "
                       ""))))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-paths
            (lambda* (#:key inputs #:allow-other-keys)
              ;; It seems that PATH is manipulated, as printenv is not found
              ;; during tests.  Patch in its exact location.
              (substitute* "conan/tools/env/environment.py"
                (("printenv")
                 (search-input-file inputs "bin/printenv")))))
          (add-after 'unpack 'patch-hard-coded-GCC-references
            (lambda _
              ;; The test suite expects GCC 9 to be used (see:
              ;; https://github.com/conan-io/conan/issues/13575).  Render the
              ;; check version agnostic.
              (substitute* "test/functional/toolchains/meson/_base.py"
                (("__GNUC__9")
                 "__GNUC__"))))
          (add-after 'unpack 'use-current-cmake-for-tests
            (lambda _
              (substitute* (find-files "test" "\\.py$")
                (("@pytest.mark.tool\\(\"cmake\", \".*\")")
                 "@pytest.mark.tool(\"cmake\")"))))
          (add-before 'check 'configure-tests
            (lambda _
              (let* ((cmake-version #$(version-major+minor
                                       (package-version
                                        (this-package-native-input "cmake"))))
                     (pkg-config-version #$(version-major+minor
                                            (package-version pkg-config))))
                (call-with-output-file "test/conftest_user.py"
                  (lambda (port)
                    (format port "\
tools_locations = {
    'apt_get': {'disabled': True},
    'bazel': {'disabled': True},
    'cmake': {'default': '~a',
              '3.15': {'disabled': True},
              '3.16': {'disabled': True},
              '3.17': {'disabled': True},
              '3.19': {'disabled': True},
              '~:*~a': {}},
    'pkg_config': {'exe': 'pkg-config',
                   'default': '~a',
                   '~:*~a': {}},
    'svn': {'disabled': True}}~%" cmake-version pkg-config-version))))))
          (add-before 'check 'set-home
            (lambda _
              (setenv "HOME" "/tmp")))
          (add-before 'check 'configure-tests
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (setenv "CONFIG_SHELL" (which "sh"))
                (setenv "PATH" (string-append (getenv "PATH") ":"
                                              #$output "/bin"))))))))
    (propagated-inputs
     (list python-bottle
           python-colorama
           python-dateutil
           python-distro
           python-fasteners
           python-future
           python-jinja2
           python-node-semver
           python-patch-ng
           python-pluginbase
           python-pygments
           python-pyjwt
           python-pyyaml
           python-requests
           python-six
           python-tqdm
           python-urllib3-1.26))
    (inputs
     (list coreutils))                  ;for printenv
    (native-inputs
     (list autoconf-wrapper
           automake
           ;; If cmake is replaced with cmake-minimal then Guix errors when
           ;; listing the dependents of rapidjson.
           cmake
           git-minimal
           libtool
           meson
           ninja
           pkg-config
           python-bottle
           python-mock
           python-parameterized
           python-pytest
           python-pytest-xdist
           python-setuptools
           python-webtest
           python-wheel
           which))
    (home-page "https://conan.io")
    (synopsis "Decentralized C/C++ package manager")
    (description "Conan is a package manager for C and C++ developers that
boasts the following features:
@itemize
@item
It is fully decentralized.  Users can host their packages on their own private
servers.
@item
It can create, upload and download binaries for any configuration and
platform, including cross-compiled ones.
@item
It integrates with any build system, including CMake, Makefiles, Meson, etc.
@item
It is extensible; its Python-based recipes, together with extensions points
allow for great power and flexibility.
@end itemize")
    (license license:expat)))

(define-public gwl
  (package
    (name "gwl")
    (version "0.5.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/gwl/gwl-" version ".tar.gz"))
              (sha256
               (base32
                "08h76ib7hmqyj354aazxqyz0galhywz4093f8hc4py7hbg0rcm27"))))
    (build-system gnu-build-system)
    (arguments
     `(#:parallel-build? #false ; for reproducibility
       #:make-flags
       '("GUILE_AUTO_COMPILE=0" "GWL_SKIP_INTEGRATION_TESTS=1")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'disable-test
           (lambda _
             ;; This test loads a workflow, which requires a working Guix installation.
             (substitute* "tests/cache.scm"
               (("\\(test-assert \"workflows with same file name have different cache prefixes\"" m)
                (string-append "#;" m))))))))
    (native-inputs
     (list autoconf automake pkg-config texinfo graphviz))
    (inputs
     (let ((p (package-input-rewriting
               `((,guile-3.0 . ,guile-3.0-latest))
               #:deep? #false)))
       (list guix
             guile-3.0-latest
             (p guile-commonmark)
             (p guile-config)
             (p guile-drmaa)
             (p guile-gcrypt)
             (p guile-pfds)
             (p guile-syntax-highlight)
             (p guile-wisp))))
    (home-page "https://workflows.guix.info")
    (synopsis "Workflow management extension for GNU Guix")
    (description "The @dfn{Guix Workflow Language} (GWL) provides an
extension to GNU Guix's declarative language for package management to
automate the execution of programs in scientific workflows.  The GWL
can use process engines to integrate with various computing
environments.")
    ;; The Scheme modules in guix/ and gnu/ are licensed GPL3+,
    ;; the web interface modules in gwl/ are licensed AGPL3+,
    ;; and the fonts included in this package are licensed OFL1.1.
    (license (list license:gpl3+ license:agpl3+ license:silofl1.1))))

(define-public gwl/next
  (let ((commit "343a454a427ae049cc0ac5fc5d6de94ebe5d65d2")
        (revision "0"))
    (package
      (inherit gwl)
      (name "gwl-next")
      (version (git-version "0.5.1" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://git.savannah.gnu.org/git/gwl.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "153bgcjqw6jp0yl0vj0k67k9c8lw82vac6b0cnacrjrcb24lcpdq")))))))

(define-public guix-build-coordinator
  (let ((commit "a1c18b175ecd7f42336de555746824ec003b600b")
        (revision "136"))
    (package
      (name "guix-build-coordinator")
      (version (git-version "0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://codeberg.org/guix/build-coordinator.git")
                      (commit commit)))
                (sha256
                 (base32
                  "1adpr1s5c6s1sk5vbph2275v5fnz8mqh0s7ynvpdxag93rfczrjz"))
                (file-name (string-append name "-" version "-checkout"))))
      (build-system gnu-build-system)
      (arguments
       (list
        #:modules `(((guix build guile-build-system)
                     #:select (target-guile-effective-version))
                    ,@%default-gnu-modules)
        #:imported-modules `((guix build guile-build-system)
                             ,@%default-gnu-imported-modules)
        #:phases
        #~(modify-phases %standard-phases
            (add-before 'build 'set-GUILE_AUTO_COMPILE
              (lambda _
                ;; To avoid warnings relating to 'guild'.
                (setenv "GUILE_AUTO_COMPILE" "0")))
            (add-after 'install 'wrap-executable
              (lambda* (#:key inputs outputs target #:allow-other-keys)
                (let* ((out (assoc-ref outputs "out"))
                       (bin (string-append out "/bin"))
                       (guile (assoc-ref inputs "guile"))
                       (version (target-guile-effective-version))
                       (scm (string-append out "/share/guile/site/" version))
                       (go  (string-append out "/lib/guile/" version "/site-ccache")))
                  (for-each
                   (lambda (file)
                     (simple-format (current-error-port) "wrapping: ~A\n" file)
                     (let ((guile-inputs (list
                                          "guile-json"
                                          "guile-gcrypt"
                                          "guix"
                                          "guile-prometheus"
                                          "guile-lib"
                                          "guile-lzlib"
                                          "guile-zlib"
                                          "guile-sqlite3"
                                          "guile-gnutls"
                                          #$@(if (target-hurd?)
                                                 '()
                                                 '("guile-fibers-next"
                                                   "guile-knots")))))
                       (wrap-program file
                         `("PATH" ":" prefix
                           (,bin
                            ;; Support building without sqitch as an input, as it
                            ;; can't be cross-compiled yet
                            ,@(or (and=> (assoc-ref inputs "sqitch")
                                         list)
                                  '())))
                         `("GUIX_LOCPATH" ":" prefix
                           (,(string-append (assoc-ref inputs "glibc-utf8-locales")
                                            "/lib/locale")))
                         `("GUILE_LOAD_PATH" ":" prefix
                           (,scm ,(string-join
                                   (map (lambda (input)
                                          (simple-format
                                           #f "~A/share/guile/site/~A"
                                           (assoc-ref inputs input)
                                           version))
                                        guile-inputs)
                                   ":")))
                         `("GUILE_LOAD_COMPILED_PATH" ":" prefix
                           (,go ,(string-join
                                  (map (lambda (input)
                                         (simple-format
                                          #f "~A/lib/guile/~A/site-ccache"
                                          (assoc-ref inputs input)
                                          version))
                                       guile-inputs)
                                  ":"))))))
                   (find-files bin)))))
            (delete 'strip))))             ; As the .go files aren't compatible
      (native-inputs
       (list pkg-config
             autoconf
             automake
             guile-gnutls

             ;; Guile libraries are needed here for cross-compilation.
             guile-json-4
             guile-gcrypt
             guix
             guile-prometheus
             guile-fibers-next
             guile-knots
             guile-lib
             guile-next))
      (inputs
       (list guile-next
             sqlite-next
             bash-minimal
             (libc-utf8-locales-for-target)
             sqitch))
      (propagated-inputs
       (list guile-prometheus
             guile-gcrypt
             guile-json-4
             guile-lib
             guile-lzlib
             guile-zlib
             guile-sqlite3
             guix
             guile-gnutls
             guile-fibers-next
             guile-knots))
      (home-page "https://git.cbaines.net/guix/build-coordinator/")
      (synopsis "Tool to help build derivations")
      (description
       "The Guix Build Coordinator helps with performing lots of builds across
potentially many machines, and with doing something with the results and
outputs of those builds.")
      (license license:gpl3+))))

(define-public guix-build-coordinator/agent-only
  (package
    (inherit guix-build-coordinator)
    (name "guix-build-coordinator-agent-only")
    (arguments
     (substitute-keyword-arguments (package-arguments guix-build-coordinator)
       ((#:phases phases #~%standard-phases)
        #~(modify-phases #$phases
            (add-after 'install 'strip-non-agent-files
              (lambda _
                (delete-file-recursively
                 (string-append #$output "/share/guix-build-coordinator"))
                (delete-file
                 (string-append #$output "/bin/guix-build-coordinator"))))))))
    (native-inputs
     (list pkg-config
           autoconf
           automake
           guile-gnutls

           ;; Guile libraries are needed here for cross-compilation.
           guile-json-4
           guile-gcrypt
           guix
           guile-prometheus
           guile-lib
           guile-next))
    (inputs
     (list guile-next
           guix
           guile-prometheus
           guile-gcrypt
           guile-json-4
           guile-lib
           guile-lzlib
           guile-zlib
           guile-gnutls
           bash-minimal
           (libc-utf8-locales-for-target)))
    (propagated-inputs
     '())
    (description
     "The Guix Build Coordinator helps with performing lots of builds across
potentially many machines, and with doing something with the results and
outputs of those builds.

This package just includes the agent component.")))

;; XXX: Maybe move to jupyter module.
(define-public guix-jupyter
  (package
    (name "guix-jupyter")
    (version "0.3.1")
    (home-page "https://codeberg.org/guix-science/guix-jupyter")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page)
                                  (commit (string-append "v" version))))
              (sha256
               (base32
                "1yvrmaj4qcb9vn2nfjz1q0cil830hvmxpp8cgi76aylbnv36aask"))
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
                  (string-append out "/share/guile/site/" effective
                                 "/guix-jupyter-kernel.scm")))))))))
    (native-inputs
     (list autoconf
           automake
           pkg-config
           ;; For testing.
           jupyter
           python-ipython
           python-ipykernel))
    (inputs
     (list guix (lookup-package-native-input guix "guile")))
    (propagated-inputs
     (list guile-json-4 guile-simple-zmq guile-gcrypt))
    (synopsis "Guix kernel for Jupyter")
    (description
     "Guix-Jupyter is a Jupyter kernel.  It allows you to annotate notebooks
with information about their software dependencies, such that code is executed
in the right software environment.  Guix-Jupyter spawns the actual kernels
such as @code{python-ipykernel} on behalf of the notebook user and runs them
in an isolated environment, in separate namespaces.")
    (license license:gpl3+)))

(define-public nar-herder
  (let ((commit "24c1914efa9d65c629e8d2d06e1f98afa4219400")
        (revision "51"))
    (package
      (name "nar-herder")
      (version (git-version "0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://codeberg.org/guix/nar-herder.git")
                      (commit commit)))
                (sha256
                 (base32
                  "178c7l27m76snqnsn4rk1hylibr5fh8h7bch8xpcfa7y4xicf8jr"))
                (file-name (string-append name "-" version "-checkout"))))
      (build-system gnu-build-system)
      (arguments
       (list
        #:modules `(((guix build guile-build-system)
                     #:select (target-guile-effective-version))
                    ,@%default-gnu-modules)
        #:imported-modules `((guix build guile-build-system)
                             ,@%default-gnu-imported-modules)
        #:phases
        #~(modify-phases %standard-phases
            (add-before 'build 'set-GUILE_AUTO_COMPILE
              (lambda _
                ;; To avoid warnings relating to 'guild'.
                (setenv "GUILE_AUTO_COMPILE" "0")))
            (add-after 'install 'wrap-executable
              (lambda* (#:key inputs outputs target #:allow-other-keys)
                (let* ((out (assoc-ref outputs "out"))
                       (bin (string-append out "/bin"))
                       (guile (assoc-ref inputs "guile"))
                       (version (target-guile-effective-version))
                       (scm (string-append out "/share/guile/site/" version))
                       (go  (string-append out "/lib/guile/" version "/site-ccache")))
                  (for-each
                   (lambda (file)
                     (simple-format (current-error-port) "wrapping: ~A\n" file)
                     (let ((guile-inputs (list
                                          "guile-json"
                                          "guile-gcrypt"
                                          "guix"
                                          "guile-lib"
                                          "guile-lzlib"
                                          "guile-zstd"
                                          "guile-prometheus"
                                          "guile-sqlite3"
                                          "guile-gnutls"
                                          "guile-fibers-next"
                                          "guile-knots")))
                       (wrap-program file
                         `("GUILE_LOAD_PATH" ":" prefix
                           (,scm ,(string-join
                                   (map (lambda (input)
                                          (string-append
                                           (assoc-ref inputs input)
                                           "/share/guile/site/"
                                           version))
                                        guile-inputs)
                                   ":")))
                         `("GUILE_LOAD_COMPILED_PATH" ":" prefix
                           (,go ,(string-join
                                  (map (lambda (input)
                                         (string-append
                                          (assoc-ref inputs input)
                                          "/lib/guile/" version "/site-ccache"))
                                       guile-inputs)
                                  ":"))))))
                   (find-files bin)))))
            (delete 'strip))))           ; As the .go files aren't compatible
      (native-inputs
       (list pkg-config
             autoconf
             automake
             guile-gnutls

             ;; Guile libraries are needed here for cross-compilation.
             guile-next
             guile-json-4
             guile-gcrypt
             guix
             guile-fibers-next
             guile-knots
             guile-prometheus
             guile-lib
             guile-lzlib
             guile-zstd
             guile-sqlite3))
      (inputs
       (list bash-minimal
             guile-next))
      (propagated-inputs
       (list guile-json-4
             guile-gcrypt
             guix
             guile-fibers-next
             guile-knots
             guile-prometheus
             guile-lib
             guile-lzlib
             guile-zstd
             guile-sqlite3
             guile-gnutls))
      (home-page "https://git.cbaines.net/guix/nar-herder")
      (synopsis "Utility for managing and serving nars")
      (description
       "The Nar Herder is a utility for managing a collection of
nars (normalized archives, in the context of Guix) along with the
corresponding narinfo files which contain some signed metadata.

It can assist in serving a collection of nars, moving them between machines,
or mirroring an existing collection of nars.

It's currently a working prototype, many designed features aren't implemented,
and the error handling is very rough.")
      (license license:agpl3+))))

(define-public gcab
  (package
    (name "gcab")
    (version "1.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/gcab/"
                                  version "/gcab-" version ".tar.xz"))
              (sha256
               (base32
                "02sngv40zwadajsiav1paahyfgkccbh9s7r5ks82chbwawarc31g"))))
    (build-system meson-build-system)
    (native-inputs
     (list `(,glib "bin")               ; for glib-mkenums
           intltool
           pkg-config
           vala))
    (inputs
     (list glib zlib))
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
     (list bison pkg-config))
    (inputs
     (list gcab glib libgsf libxml2
           `(,util-linux "lib")))
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
    (version "2025.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/ostreedev/ostree/releases/download/v"
             (version-major+minor version) "/libostree-" version ".tar.xz"))
       (sha256
        (base32 "0lc3y710wknwmx40ssi5h7ndmf30niz7x0grd4lf96nn9ws15hzj"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'pre-check
           (lambda _
             ;; Don't try to use the non-existing '/var/tmp' as test
             ;; directory.
             (setenv "TEST_TMPDIR" (getenv "TMPDIR")))))
       ;; XXX: fails with:
       ;;     tap-driver.sh: missing test plan
       ;;     tap-driver.sh: internal error getting exit status
       ;;     tap-driver.sh: fatal: I/O or internal error
       #:tests? #f))
    (native-inputs
     (list attr ; for tests
           bison
           `(,glib "bin") ; for 'glib-mkenums'
           gobject-introspection
           pkg-config
           libxslt))
    (inputs
     (list avahi
           docbook-xml
           docbook-xsl
           e2fsprogs
           fuse-2
           glib
           gpgme
           libarchive
           libsoup-minimal
           util-linux))
    (home-page "https://ostreedev.github.io/ostree/")
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
    (version "1.16.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/flatpak/flatpak/releases/download/"
                           version "/flatpak-" version ".tar.xz"))
       (sha256
        (base32 "1ss3v25bq0ybks6yl3q13cxxf18virgi3w6xws0qzil51ldjs5v9"))
       (patches
        (search-patches "flatpak-fix-fonts-icons.patch"
                        "flatpak-fix-icon-validation.patch"
                        "flatpak-unset-gdk-pixbuf-for-sandbox.patch"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list
         "-Dsystem_helper=disabled"
         "-Dwayland_security_context=enabled"
         "-Dlocalstatedir=/var"
         (string-append "-Dsystem_bubblewrap="
                        (assoc-ref %build-inputs "bubblewrap")
                        "/bin/bwrap")
         (string-append "-Dsystem_dbus_proxy="
                        (assoc-ref %build-inputs "xdg-dbus-proxy")
                        "/bin/xdg-dbus-proxy"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'disable-failing-tests
            (lambda _
              (substitute* "tests/test-matrix/meson.build"
                ;; The following tests fail with error message related to fusermount3
                ;; failing an unmount operation ("No such file or directory").
                (("^.*test-http-utils.*$") "")
                (("^.*test-summaries@system.wrap.*$") "")
                (("^.*test-prune.*$") ""))))
          (add-after 'unpack 'fix-tests
            (lambda* (#:key inputs #:allow-other-keys)
              (copy-recursively
               (search-input-directory inputs "lib/locale")
               "/tmp/locale")
              (for-each make-file-writable (find-files "/tmp"))
              (substitute* "tests/make-test-runtime.sh"
                (("cp `which.*") "echo guix\n")
                (("cp -r /usr/lib/locale/C\\.\\*")
                 (string-append "mkdir ${DIR}/usr/lib/locale/en_US; \
cp -r /tmp/locale/*/en_US.*")))
              (substitute* "tests/libtest.sh"
                (("/bin/kill") (which "kill"))
                (("/usr/bin/python3") (which "python3")))
              #t))
          (add-after 'unpack 'p11-kit-fix
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((p11-path (search-input-file inputs "/bin/p11-kit")))
                (substitute* "session-helper/flatpak-session-helper.c"
                  (("\"p11-kit\",")
                   (string-append "\"" p11-path "\","))
                  (("if \\(g_find_program_in_path \\(\"p11-kit\"\\)\\)")
                   (string-append "if (g_find_program_in_path (\""
                                  p11-path "\"))"))))))
          (add-after 'unpack 'fix-icon-validation
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (store (dirname out)))
                (substitute* "icon-validator/validate-icon.c"
                  (("@storeDir@") store)))))
          (add-before 'check 'pre-check
            (lambda _
              ;; Set $HOME to writable location for testcommon tests.
              (setenv "HOME" "/tmp")))
          (add-after 'install 'wrap-flatpak
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((flatpak (string-append #$output "/bin/flatpak"))
                    (glib-networking (assoc-ref inputs "glib-networking")))
                (wrap-program flatpak
                  ;; Prevent error:
                  ;; "No GSettings schemas are installed on the system"
                  `("GSETTINGS_SCHEMA_DIR" =
                    (,(string-append
                       #$(this-package-input "gsettings-desktop-schemas")
                       "/share/glib-2.0/schemas")))
                  ;; Allow GIO to find TLS backend.
                  `("GIO_EXTRA_MODULES" prefix
                    (,(string-append glib-networking "/lib/gio/modules"))))))))))
    (native-inputs
     (list bison
           dbus ; for dbus-daemon
           gettext-minimal
           `(,glib "bin") ; for glib-mkenums + gdbus-codegen
           gtk-doc/stable
           (libc-utf8-locales-for-target)
           gobject-introspection
           libcap
           pkg-config
           python
           python-pyparsing
           socat
           wayland ; wayland-scanner
           wayland-protocols
           which))
    (inputs
     (list appstream
           appstream-glib
           bash-minimal
           bubblewrap
           curl
           fuse
           gsettings-desktop-schemas
           gdk-pixbuf
           libcap
           libostree
           libsoup-minimal-2
           libxml2
           p11-kit
           polkit
           util-linux
           wayland
           xdg-dbus-proxy
           zstd))
    (propagated-inputs (list glib-networking
                             gnupg
                             gsettings-desktop-schemas
                             ;; The following are listed in Requires.private of
                             ;; `flatpak.pc'.
                             curl
                             dconf
                             gpgme
                             json-glib
                             libarchive
                             libseccomp
                             libxau))
    (native-search-paths
     (list ;; Flatpak creates desktop files on its own.
           ;; If those desktop files contain DBusActivatable=true, the application
           ;; will be invoked by using dbus activation.  But dbus activation
           ;; doesn't use $PATH but rather does execve while the working directory
           ;; is "/".  That means, if the Exec entry contains just "flatpak",
           ;; that won't be ever found.
           ;; When flatpak creates desktop files, it uses a path from
           ;; $FLATPAK_BINARY if set.
           ;; See <https://codeberg.org/guix/guix/issues/438>.
           (search-path-specification
            (variable "FLATPAK_BINARY")
            (separator #f)
            (files '("bin/flatpak"))
            (file-type 'regular))))
    (home-page "https://flatpak.org")
    (synopsis "System for building, distributing, and running sandboxed desktop
applications")
    (description "Flatpak is a system for building, distributing, and running
sandboxed desktop applications on GNU/Linux.")
    (license license:lgpl2.1+)))

(define-public fpm
  (package
    (name "fpm")
    (version "1.16.0")
    (source (origin
              (method git-fetch)        ;for tests
              (uri (git-reference
                    (url "https://github.com/jordansissel/fpm")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0h4yw57y0p0x335767y058lbv46f6xjf3jvv49vxw5vfzzhqbl3c"))))
    (build-system ruby-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-after 'extract-gemspec 'patch-paths
                 (lambda* (#:key inputs #:allow-other-keys)
                   (substitute* '("lib/fpm/util.rb"
                                  "spec/fpm/util_spec.rb"
                                  "spec/fpm/package/rpm_spec.rb")
                     (("\"/bin/sh\"")
                      (string-append "\"" (search-input-file inputs "bin/sh")
                                     "\"")))))
               (add-after 'extract-gemspec 'relax-requirements
                 (lambda _
                   (substitute* "fpm.gemspec"
                     (("\"clamp\", \"~> 1.0.0\"")
                      "\"clamp\", \">= 1.0.0\""))))
               (add-after 'extract-gemspec 'disable-problematic-tests
                 ;; Disable some tests which are failing (see:
                 ;; https://github.com/jordansissel/fpm/issues/2000).
                 (lambda _
                   ;; There are 4 'NoMethodError' test failures in the
                   ;; command_spec suite, for unknown reasons.
                   (delete-file "spec/fpm/command_spec.rb")
                   (substitute* "spec/fpm/package_spec.rb"
                     (("@oldtmp = ENV\\[\"TMP\"]" all)
                      "skip('fails with guix')"))
                   (substitute* "spec/fpm/package/cpan_spec.rb"
                     ;; This test is marked as expected to fail (pending) when
                     ;; TRAVIS_OS_NAME is set, but passes with Guix; skip it.
                     (("it \"should unpack tarball containing" all)
                      (string-append "x" all)))
                   (substitute* "spec/fpm/package/gem_spec.rb"
                     ;; This test fails for unknown reason; perhaps a patched
                     ;; shebang.
                     (("it 'should not change the shebang'" all)
                      (string-append "x" all)))))
               (replace 'check
                 (lambda* (#:key tests? #:allow-other-keys)
                   (when tests?
                     ;; Set TRAVIS_OS_NAME to skip tests known to cause
                     ;; problems in minimal environments.
                     (setenv "TRAVIS_OS_NAME" "GNU Guix")
                     (invoke "rspec")))))))
    (native-inputs
     (list dpkg
           libarchive
           node-lts
           perl-app-cpanminus
           python
           ruby-rspec
           squashfs-tools
           zstd))
    (inputs
     (list bash-minimal
           ruby-arr-pm
           ruby-backports
           ruby-cabin
           ruby-clamp
           ruby-pleaserun
           ruby-rexml
           ruby-stud))
    (home-page "https://github.com/jordansissel/fpm/")
    (synopsis "Package building and mangling tool")
    (description "@command{fpm} is a command to convert directories, RPMs,
Python eggs, Ruby gems, and more to RPMs, debs, Solaris packages and more.")
    (license license:expat)))

(define-public akku
  (package
    (name "akku")
    (version "1.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.com/akkuscm/akku.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256 (base32 "1pi18aamg1fd6f9ynfl7zx92052xzf0zwmhi2pwcwjs1kbah19f5"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (replace 'bootstrap
                    (lambda* (#:key outputs #:allow-other-keys)
                      (for-each patch-shebang
                                '("bootstrap"
                                  ".akku/env"))
                      (let* ((home "/tmp")
                             (datadir (string-append home "/.local/share/akku/")))
                        (mkdir-p datadir)
                        (invoke "touch" (string-append datadir "index.db"))
                        (setenv "HOME" home))
                      (invoke "./bootstrap")))
                  (add-after 'install 'wrap-executables
                    (lambda* (#:key outputs inputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out"))
                            (curl (assoc-ref inputs "curl")))
                        (wrap-program (string-append out "/bin/akku")
                          `("LD_LIBRARY_PATH" ":" prefix
                            (,(string-append curl "/lib"))))))))))
    (native-inputs
     (list which autoconf automake pkg-config))
    (inputs
     (list bash-minimal guile-3.0 curl))
    (home-page "https://akkuscm.org/")
    (synopsis "Language package manager for Scheme")
    (description
     "Akku.scm is a project-based language package manager for R6RS and R7RS Scheme.
It is mainly meant for programmers who develop portable programs or libraries in Scheme,
but could potentially work for end-users of those programs.  It also has a translator
from R7RS, which allows most R7RS code to run on R6RS implementations.")
    (license license:gpl3+)))

(define-public asdf-vm
  (package
    (name "asdf-vm")
    (version "0.18.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/asdf-vm/asdf")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "09p4d41l3f0dzm4bdvb45lxzsvlh3b99rgbdv8rk6jj84hr7w5q4"))))
    (build-system go-build-system)
    (arguments
     (list
      #:install-source? #f
      #:import-path "github.com/asdf-vm/asdf/cmd/asdf"
      #:unpack-path "github.com/asdf-vm/asdf"
      #:embed-files
      #~(list "bash_autocomplete"
              "powershell_autocomplete.ps1"
              "zsh_autocomplete")
      #:test-subdirs ;Test only those subdirs that don't include integration tests
      #~(list "../../internal/cli/set"
              "../../internal/completions"
              "../../internal/config"
              "../../internal/data"
              "../../internal/exec"
              "../../internal/execute"
              "../../internal/hook"
              "../../internal/paths"
              "../../internal/toolversions")))
    (native-inputs
     (list go-github-com-go-git-go-git-v5
           go-github-com-otiai10-copy
           go-github-com-rogpeppe-go-internal
           go-github-com-stretchr-testify
           go-github-com-urfave-cli-v3
           go-golang-org-x-sys
           go-gopkg-in-ini-v1
           go-honnef-co-go-tools
           go-mvdan-cc-gofumpt))
    (home-page "https://asdf-vm.com")
    (synopsis "Multi language extendible version manager")
    (description
     "ASDF is a version manager for programming language runtimes and tools.
Versions can be defined and managed on a per-project basis.  It features a
plugin system that allows it to be extended to support further languages.  It
currently supports a many popular runtimes, such as Ruby, Node.js, Elixir, and
Erlang.")
    (license license:expat)))

(define-public modules
  (package
    (name "modules")
    (version "4.8.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://sourceforge/modules/Modules/modules-"
                            version "/modules-" version ".tar.bz2"))
        (sha256 (base32 "1amz8qdqbvfdc8jv0j4720vywbz2gi7l3sr1lh37ilfbxy9lq9g9"))))
    (build-system gnu-build-system)
    (arguments
      `(#:configure-flags
        (list (string-append "--with-bin-search-path="
                             (assoc-ref %build-inputs "tcl") "/bin" ":"
                             (assoc-ref %build-inputs "procps") "/bin" ":"
                             (assoc-ref %build-inputs "less") "/bin" ":"
                             (assoc-ref %build-inputs "coreutils") "/bin")
              (string-append "--with-tcl=" (assoc-ref %build-inputs "tcl") "/lib")
              "--disable-compat-version")
        #:test-target "test"
        #:phases
        (modify-phases %standard-phases
          (add-before 'configure 'patch-add-modules
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((coreutils (assoc-ref inputs "coreutils")))
                (substitute* "script/add.modules.in"
                  (("/bin/(cat|cp|rm)" _ command)
                   (string-append coreutils "/bin/" command))
                  (("/bin/echo")
                   "echo")))))
          (add-before 'configure 'patch-scripts-for-python-3
            (lambda _
              ;; Patch the script for python-3.
              (substitute* "script/createmodule.py.in"
                (("pathkeys.sort\\(\\)") "pathkeys = sorted(pathkeys)")
                (("print\\(\"\\\\t\"\\*") "print(\"\\t\"*int")
                (("@PYTHON@") (which "python3")))))
          (add-before 'check 'patch-/bin/sh-and-nixbld-groups-in-tests
            (lambda _
              (use-modules (srfi srfi-1))
              (let* ((groups-file (string-append (getcwd) "/nixbld-groups"))
                     (groups-file-z (string-append groups-file "-z"))
                     (nixbld-groups
                       (fold
                         (lambda (id prev)
                           (catch #t
                             (lambda () (cons (group:name (getgrnam id)) prev))
                             (lambda _ prev)))
                         '()
                         (vector->list (getgroups)))))
                ;; Simulate "id -G -n" command output.
                (call-with-output-file groups-file
                  (lambda (port)
                    (display (string-join nixbld-groups " ") port)
                    (display #\newline port)))
                ;; Simulate "id -G -n -z" command output.
                (call-with-output-file groups-file-z
                  (lambda (port)
                    (for-each
                      (lambda (group-name)
                        (display group-name port)
                        (display #\null port))
                      nixbld-groups)))
                ;; Generate "modulecmd-test.tcl" before running "make test".
                (invoke "make" "modulecmd-test.tcl")
                ;; Substitute shell.
                (substitute*
                  '("modulecmd-test.tcl"
                    "modulecmd.tcl"
                    "testsuite/modules.70-maint/380-edit.exp"
                    "compat/init/filter")
                  (("/bin/sh") (which "sh")))
                ;; Skip tests that use supplementary groups.
                (for-each
                  delete-file
                  '("testsuite/modules.20-locate/112-hide-user-group.exp"
                    "testsuite/modules.20-locate/117-forbid-user-group.exp"
                    "testsuite/modules.20-locate/119-hide-cascading.exp"
                    "testsuite/modules.50-cmds/140-system.exp"
                    "testsuite/modules.50-cmds/287-info-usergroups.exp"
                    "testsuite/modules.50-cmds/440-module-tag.exp"
                    "testsuite/modules.70-maint/220-config.exp"))
                (for-each
                  (lambda (file)
                    (substitute* file
                      (("/bin/sh") (which "bash"))
                      ;; For some reason "kvm" group cannot be resolved for
                      ;; "nixbld" user. We replace "id ..." commands with
                      ;; "cat ..." that simulates them.
                      (("exec id -G -n -z") (string-append "exec cat " groups-file-z))
                      (("exec id -G -n") (string-append "exec cat " groups-file))))
                  '("testsuite/modules.00-init/005-init_ts.exp"
                    "testsuite/install.00-init/005-init_ts.exp"
                    "modulecmd-test.tcl"))))))))
    (native-inputs
      (list dejagnu autoconf which))
    (inputs
      (list tcl less procps coreutils python-3))
    (home-page "https://modules.sourceforge.net/")
    (synopsis "Shell environment variables and aliases management")
    (description "Modules simplify shell initialization and let users
modify their environment during the session with modulefiles.  Modules are
used on high-performance clusters to dynamically add and remove paths
to specific versions of applications.")
    (license license:gpl2+)))

(define-public gnome-packagekit
  (package
    (name "gnome-packagekit")
    (version "43.0")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "mirror://gnome/sources/" name "/"
                              (version-major version) "/"
                              name "-" version ".tar.xz"))
              (sha256
               (base32
                "15giqmk8w375kkyzmwzfc9xnyafqzp86ncbh5zmb48x9aak5b96d"))))
    (build-system meson-build-system)
    (arguments
     (list #:configure-flags
           #~(list "-Dsystemd=false")
           #:phases
           #~(modify-phases %standard-phases
               (add-before 'check 'start-xorg-server
                 (lambda _
                    (system "Xvfb :1 &")
                    (setenv "DISPLAY" ":1")))
               (add-before 'install 'setenv
                 (lambda _
                   ;; Prevent gtk-update-icon-cache, glib-compile-schemas,
                   ;; update-desktop-database (since we are doing it ourselves with
                   ;; glib-or-gtk phases).
                   (setenv "DESTDIR" "/"))))
           #:glib-or-gtk? #t))
    (native-inputs
     (list gettext-minimal pkg-config (list glib "bin") xorg-server-for-tests))
    (inputs
     (list glib gtk+ packagekit))
    (synopsis "GNOME frontend for PackageKit")
    (description "This package provides a PackageKit frontend for GNOME.
PackageKit is a common unified interface for package managers.")
    (home-page "https://gitlab.gnome.org/GNOME/gnome-packagekit")
    (license license:gpl2+)))
