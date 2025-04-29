#!/bin/sh
# -*- mode: scheme; -*-
# Extra care is taken here to ensure this script can run in most environments,
# since it is invoked by 'git send-email'.
pre_inst_env_maybe=
command -v guix > /dev/null || pre_inst_env_maybe=./pre-inst-env
exec $pre_inst_env_maybe guix repl -- "$0" "$@"
!#

;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2022-2024 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2022 Mathieu Othacehe <othacehe@gnu.org>
;;; Copyright © 2022, 2023 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2022 Simon Tournier <zimon.toutoune@gmail.com>
;;; Copyright © 2025 Jelle Licht <jlicht@fsfe.org>
;;; Copyright © 2025 Cayetano Santos <csantosb@inventati.org>
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

;;; Commentary:

;; This code defines development teams and team members, as well as their
;; scope.

;;; Code:

(use-modules (srfi srfi-1)
             (srfi srfi-9)
             (srfi srfi-26)
             (ice-9 format)
             (ice-9 regex)
             (ice-9 match)
             (ice-9 rdelim)
             (guix ui)
             (git))

(define-record-type <regexp*>
  (%make-regexp* pat flag rx)
  regexp*?
  (pat regexp*-pattern)
  (flag regexp*-flag)
  (rx regexp*-rx))

;;; Work around regexp implementation.
;;; This record allows to track the regexp pattern and then display it.
(define* (make-regexp* pat #:optional (flag regexp/extended))
  "Alternative to `make-regexp' producing annotated <regexp*> objects."
  (%make-regexp* pat flag (make-regexp pat flag)))

(define (regexp*-exec rx* str)
  "Execute the RX* regexp, a <regexp*> object."
  (regexp-exec (regexp*-rx rx*) str))

(define-record-type <team>
  (make-team id name description members scope)
  team?
  (id          team-id)
  (name        team-name)
  (description team-description)
  (members     team-members set-team-members!)
  (scope       team-scope))

(define-record-type <person>
  (make-person name email)
  person?
  (name    person-name)
  (email   person-email))

(define* (person name #:optional email)
  (make-person name email))

(define* (team id #:key name description (members '())
               (scope '()))
  (make-team id
             (or name (symbol->string id))
             description
             members
             scope))

(define %teams
  (make-hash-table))

(define-syntax define-team
  (lambda (x)
    (syntax-case x ()
      ((_ id value)
       #`(begin
           (define-public id value)
           (hash-set! %teams 'id id))))))

(define-syntax-rule (define-member person teams ...)
  (let ((p person))
    (for-each (lambda (team-id)
                (let ((team
                       (hash-ref %teams team-id
                                 (lambda ()
                                   (error (format #false
                                                  "Unknown team ~a for ~a~%"
                                                  team-id p))))))
                  (set-team-members!
                   team (cons p (team-members team)))))
              (quote (teams ...)))))



(define-team audio
  (team 'audio
        #:name "Audio team"
        #:description "Audio related packages."
        #:scope (list "gnu/packages/audio.scm")))

(define-team bootstrap
  (team 'bootstrap
        #:name "Bootstrap"
        #:scope (list "gnu/packages/commencement.scm"
                      "gnu/packages/mes.scm")))

(define-team c++
  (team 'c++
        #:name "C/C++ team"
        #:description
        "C and C++ libraries and tools."
        #:scope (list "gnu/build-system/cmake.scm"
                      "gnu/build/cmake-build-system.scm"
                      "gnu/packages/c.scm"
                      "gnu/packages/cmake.scm"
                      "gnu/packages/cpp.scm"
                      "gnu/packages/ninja.scm"
                      "gnu/packages/valgrind.scm")))

(define-team core
  (team 'core
        #:name "Core / Tools / Internals"
        #:scope
        (list "guix/avahi.scm"
              "guix/base16.scm"
              "guix/base32.scm"
              "guix/base64.scm"
              "guix/bzr-download.scm"
              "guix/cache.scm"
              "guix/channels.scm"
              "guix/ci.scm"
              "guix/colors.scm"
              "guix/combinators.scm"
              "guix/config.scm"
              "guix/cpio.scm"
              "guix/cpu.scm"
              "guix/cve.scm"
              "guix/cvs-download.scm"
              "guix/deprecation.scm"
              "guix/derivations.scm"
              "guix/describe.scm"
              "guix/diagnostics.scm"
              "guix/discovery.scm"
              "guix/docker.scm"
              "guix/download.scm"
              "guix/elf.scm"
              "guix/ftp-client.scm"
              "guix/gexp.scm"
              "guix/git-authenticate.scm"
              "guix/git-download.scm"
              "guix/git.scm"
              "guix/glob.scm"
              "guix/gnu-maintenance.scm"
              "guix/gnupg.scm"
              "guix/grafts.scm"
              "guix/graph.scm"
              "guix/hash.scm"
              "guix/hg-download.scm"
              "guix/http-client.scm"
              "guix/i18n.scm"
              "guix/inferior.scm"
              "guix/ipfs.scm"
              "guix/least-authority.scm"
              "guix/licenses.scm"
              "guix/lint.scm"
              "guix/man-db.scm"
              "guix/memoization.scm"
              "guix/modules.scm"
              "guix/monad-repl.scm"
              "guix/monads.scm"
              "guix/narinfo.scm"
              "guix/nar.scm"
              "guix/openpgp.scm"
              "guix/packages.scm"
              "guix/pki.scm"
              "guix/platform.scm"
              "guix/profiles.scm"
              "guix/profiling.scm"
              "guix/progress.scm"
              "guix/quirks.scm"
              "guix/read-print.scm"
              "guix/records.scm"
              "guix/remote.scm"
              "guix/repl.scm"
              "guix/search-paths.scm"
              "guix/self.scm"
              "guix/serialization.scm"
              "guix/sets.scm"
              "guix/ssh.scm"
              "guix/status.scm"
              "guix/store.scm"
              "guix/substitutes.scm"
              "guix/svn-download.scm"
              "guix/swh.scm"
              "guix/tests.scm"
              "guix/transformations.scm"
              "guix/ui.scm"
              "guix/upstream.scm"
              "guix/utils.scm"
              "guix/workers.scm"
              (make-regexp* "^guix/platforms/")
              (make-regexp* "^guix/scripts/")
              (make-regexp* "^guix/store/"))))

(define-team core-packages
  (team 'core-packages
        #:name "Core packages"
        #:description "Core packages: the GNU tool chain, Guile, Coreutils, etc."
        #:scope (list "gnu/packages/base.scm"
                      "gnu/packages/bootstrap.scm"
                      "gnu/packages/commencement.scm"
                      "gnu/packages/cross-base.scm"
                      "gnu/packages/gcc.scm"
                      "gnu/packages/guile.scm"
                      "gnu/packages/ld-wrapper.in"
                      "gnu/packages/make-bootstrap.scm"
                      "gnu/packages/multiprecision.scm"
                      "guix/build/gnu-build-system.scm"
                      "guix/build/utils.scm"
                      "guix/build-system/gnu.scm")))

(define-team documentation
  (team 'documentation
        #:name "Documentation"
        #:description "Documentation: the manual and cookbook."
        #:scope (list (make-regexp* "\\.texi$")
                      "doc/build.scm"
                      "gnu/system/examples/bare-bones.tmpl"
                      "gnu/system/examples/lightweight-desktop.tmpl"
                      "gnu/system/examples/desktop.tmpl")))

(define-team electronics
  (team 'electronics
        #:name "Electronics team"
        #:description "Electronics and hardware related packages."
        #:scope (list "gnu/packages/fpga.scm"
                      "gnu/packages/electronics.scm"
                      "gnu/packages/libftdi.scm"
                      "gnu/packages/engineering.scm")))

(define-team emacs
  (team 'emacs
        #:name "Emacs team"
        #:description "The extensible, customizable text editor and its
ecosystem."
        #:scope (list "gnu/packages/aux-files/emacs/guix-emacs.el"
                      (make-regexp* "^gnu/packages/emacs(-.+|)\\.scm$")
                      "gnu/packages/tree-sitter.scm"
                      "guix/build/emacs-build-system.scm"
                      "guix/build/emacs-utils.scm"
                      "guix/build-system/emacs.scm"
                      "guix/import/elpa.scm"
                      "guix/scripts/import/elpa.scm"
                      "tests/elpa.scm")))

(define-team embedded
  (team 'embedded
        #:name "Embedded"
        #:scope (list "gnu/packages/bootloaders.scm"
                      "gnu/packages/firmware.scm")))

(define-team games
  (team 'games
        #:name "Games and Toys"
        #:description "Packaging programs for amusement."
        #:scope (list "gnu/packages/emulators.scm"
                      "gnu/packages/games.scm"
                      "gnu/packages/game-development.scm"
                      "gnu/packages/minetest.scm"
                      "gnu/packages/esolangs.scm" ; granted, rather niche
                      "gnu/packages/motti.scm"
                      "guix/build/minetest-build-system.scm")))

(define-team gnome
  (team 'gnome
        #:name "Gnome team"
        #:description
        "The Gnome desktop environment, along with core technologies such as
GLib/GIO, GTK, GStreamer and Webkit."
        #:scope (list "gnu/packages/glib.scm"
                      "gnu/packages/gstreamer.scm"
                      "gnu/packages/gtk.scm"
                      "gnu/packages/gnome.scm"
                      "gnu/packages/gnome-xyz.scm"
                      "gnu/packages/webkit.scm"
                      "gnu/services/desktop.scm"
                      "guix/build/glib-or-gtk-build-system.scm"
                      "guix/build/meson-build-system.scm")))

(define-team go
  (team 'go
        #:name "Go team"
        #:scope (list "gnu/packages/configuration-management.scm"
                      (make-regexp* "gnu/packages/golang(-.+|)\\.scm$")
                      "gnu/packages/syncthing.scm"
                      "gnu/packages/terraform.scm"
                      "guix/build-system/go.scm"
                      "guix/build/go-build-system.scm"
                      "guix/import/go.scm"
                      "guix/scripts/import/go.scm"
                      "tests/go.scm")))

(define-team haskell
  (team 'haskell
        #:name "Haskell team"
        #:description
        "GHC, Hugs, Haskell packages, the \"hackage\" and \"stackage\" importers, and
the haskell-build-system."
        #:scope
        (list "gnu/packages/dhall.scm"
              ;; Match haskell.scm and haskell-*.scm.
              (make-regexp* "^gnu/packages/haskell(-.+|)\\.scm$")
              "gnu/packages/purescript.scm"
              "guix/build/haskell-build-system.scm"
              "guix/build-system/haskell.scm"
              "guix/import/cabal.scm"
              "guix/import/hackage.scm"
              "guix/import/stackage.scm"
              "guix/scripts/import/hackage.scm")))

(define-team home
  (team 'home
        #:name "Team for \"Guix Home\""
        #:scope (list (make-regexp* "^(gnu|guix/scripts)/home(\\.scm$|/)")
                      "tests/guix-home.sh"
                      "tests/home-import.scm"
                      "tests/home-services.scm")))

(define-team hurd
  (team 'hurd
        #:name "Team for the Hurd"
        #:scope (list "gnu/system/hurd.scm"
                      "gnu/system/images/hurd.scm"
                      "gnu/build/hurd-boot.scm"
                      "gnu/services/hurd.scm"
                      "gnu/packages/hurd.scm")))

(define-team installer
  (team 'installer
        #:name "Installer script and system installer"
        #:scope (list (make-regexp* "^gnu/installer(\\.scm$|/)"))))

(define-team java
  (team 'java
        #:name "Java and Maven team"
        #:description
        "The JDK and JRE, the Maven build system, Java packages, the ant-build-system,
and the maven-build-system."
        #:scope
        (list ;; Match java.scm and java-*.scm.
              (make-regexp* "^gnu/packages/java(-.+|)\\.scm$")
              ;; Match maven.scm and maven-*.scm
              (make-regexp* "^gnu/packages/maven(-.+|)\\.scm$")
              "guix/build/ant-build-system.scm"
              "guix/build/java-utils.scm"
              "guix/build/maven-build-system.scm"
              ;; The maven directory
              (make-regexp* "^guix/build/maven/")
              "guix/build-system/ant.scm"
              "guix/build-system/maven.scm")))

(define-team javascript
  (team 'javascript
        #:name "JavaScript team"
        #:description
        "JavaScript/Node.js packages, the node build system."
        #:scope (list "gnu/packages/node-xyz.scm"
                      "gnu/packages/node.scm"
                      "guix/build-system/node.scm"
                      "guix/build/node-build-system.scm"
                      "guix/import/npm-binary.scm"
                      "guix/scripts/import/npm-binary.scm")))

(define-team julia
  (team 'julia
        #:name "Julia team"
        #:description
        "The Julia language, Julia packages, and the julia-build-system."
        #:scope (list (make-regexp* "^gnu/packages/julia(-.+|)\\.scm$")
                      "guix/build/julia-build-system.scm"
                      "guix/build-system/julia.scm")))

(define-team kde
  (team 'kde
        #:name "KDE team"
        #:description
        "The plasma desktop environment, and KDE Applications."
        #:scope (list (make-regexp* "^gnu/packages/(kde)(-.+|)\\.scm$")
                      "gnu/packages/education.scm")))

(define-team kernel
  (team 'kernel
        #:name "Linux-libre kernel team"
        #:scope (list "gnu/build/linux-modules.scm"
                      "gnu/packages/linux.scm"
                      "gnu/tests/linux-modules.scm"
                      "guix/build/linux-module-build-system.scm"
                      "guix/build-system/linux-module.scm")))

(define-team lisp
  (team 'lisp
        #:name "Lisp team"
        #:description
        "Common Lisp and similar languages, Common Lisp packages and the
asdf-build-system."
        #:scope (list (make-regexp* "^gnu/packages/lisp(-.+|)\\.scm$")
                      "guix/build/asdf-build-system.scm"
                      "guix/build/lisp-utils.scm"
                      "guix/build-system/asdf.scm")))

(define-team localization
  (team 'localization
        #:name "Localization (l10n) team"
        #:description
        "Localization of your system to specific languages."
        #:scope (list "gnu/packages/anthy.scm"
                      "gnu/packages/fcitx5.scm"
                      "gnu/packages/fcitx.scm"
                      "gnu/packages/fonts.scm"
                      "gnu/packages/ibus.scm")))

(define-team lxqt
  (team 'lxqt
        #:name "LXQt team"
        #:description "LXQt desktop environment."
        #:scope (list "gnu/packages/lxqt.scm")))

(define-team mentors
  (team 'mentors
        #:name "Mentors"
        #:description
        "A group of mentors who chaperone contributions by newcomers."))

(define-team mozilla
  (team 'mozilla
        #:name "Mozilla"
        #:description
        "Taking care of Icedove and Web Browsers based on Mozilla Thunderbird
and Firefox."
        #:scope (list "gnu/build/icecat-extension.scm"
                      "gnu/packages/browser-extensions.scm"
                      "gnu/packages/gnuzilla.scm"
                      "gnu/packages/librewolf.scm"
                      "gnu/packages/tor-browsers.scm")))

(define-team ocaml
  (team 'ocaml
        #:name "OCaml and Dune team"
        #:description
        "The OCaml language, the Dune build system, OCaml packages, the \"opam\"
importer, and the ocaml-build-system."
        #:scope
        (list "gnu/packages/ocaml.scm"
              "gnu/packages/coq.scm"
              "guix/build/ocaml-build-system.scm"
              "guix/build/dune-build-system.scm"
              "guix/build-system/ocaml.scm"
              "guix/build-system/dune.scm"
              "guix/import/opam.scm"
              "guix/scripts/import/opam.scm"
              "tests/opam.scm")))

(define-team python
  (team 'python
        #:name "Python team"
        #:description
        "Python, Python packages, the \"pypi\" importer, and the python-build-system."
        #:scope
        (list "gnu/packages/django.scm"
              "gnu/packages/jupyter.scm"
              (make-regexp* "^gnu/packages/python(-.+|)\\.scm$")
              "gnu/packages/sphinx.scm"
              "gnu/packages/tryton.scm"
              "guix/build/pyproject-build-system.scm"
              "guix/build-system/pyproject.scm"
              "guix/build/python-build-system.scm"
              "guix/build-system/python.scm"
              "guix/import/pypi.scm"
              "guix/scripts/import/pypi.scm"
              "tests/pypi.scm")))

(define-team qt
  (team 'qt
        #:name "Qt team"
        #:description
        "The Qt toolkit/library and the qt-build-system,
as well as some packages using Qt."
        #:scope (list "gnu/packages/qt.scm"
                      "guix/build-system/qt.scm"
                      "guix/build/qt-build-system.scm"
                      "guix/build/qt-utils.scm")))

(define-team r
  (team 'r
        #:name "R team"
        #:description
        "The R language, CRAN and Bioconductor repositories, the \"cran\" importer,
and the r-build-system."
        #:scope (list "gnu/packages/bioconductor.scm"
                      "gnu/packages/cran.scm"
                      "guix/build/r-build-system.scm"
                      "guix/build-system/r.scm"
                      "guix/import/cran.scm"
                      "guix/scripts/import/cran.scm"
                      "tests/cran.scm")))

(define-team racket
  (team 'racket
        #:name "Racket team"
        #:description
        "The Racket language and Racket-based languages, Racket packages,
Racket's variant of Chez Scheme, and development of a Racket build system and
importer."
        #:scope (list "gnu/packages/chez.scm"
                      "gnu/packages/racket.scm")))

(define-team reproduciblebuilds
  (team 'reproduciblebuilds
        #:name "Reproducible Builds team"
        #:description
        "Reproducible Builds tooling and issues that affect any guix packages."
        #:scope (list "gnu/packages/diffoscope.scm")))

(define-team ruby
  (team 'ruby
        #:name "Ruby team"
        #:scope (list "gnu/packages/ruby.scm"
                      "guix/build/ruby-build-system.scm"
                      "guix/build-system/ruby.scm"
                      "guix/import/gem.scm"
                      "guix/scripts/import/gem.scm"
                      "tests/gem.scm")))

(define-team rust
  (team 'rust
        #:name "Rust"
        #:scope (list (make-regexp* "^gnu/packages/(crates|rust)(-.+|)\\.scm$")
                      "gnu/packages/c2rust.scm"
                      "gnu/packages/sequoia.scm"
                      "guix/build/cargo-build-system.scm"
                      "guix/build/cargo-utils.scm"
                      "guix/build-system/cargo.scm"
                      "guix/import/crate.scm"
                      "guix/scripts/import/crate.scm"
                      "tests/crate.scm")))

(define-team science
  (team 'science
        #:name "Science team"
        #:description "The main science disciplines and fields related
packages (e.g. Astronomy, Chemistry, Math, Physics etc.)"
        #:scope (list "gnu/packages/algebra.scm"
                      "gnu/packages/astronomy.scm"
                      "gnu/packages/geo.scm"
                      "gnu/packages/chemistry.scm"
                      "gnu/packages/maths.scm")))

(define-team sugar
  (team 'sugar
        #:name "Sugar team"
        #:description
        "Everything related to the Sugar Desktop and learning environment."
        #:scope (list "gnu/packages/sugar.scm")))

(define-team sysadmin
  (team 'sysadmin
        #:name "Sysadmin team"
        #:description
        "Networking, server clustering, high availability."
        #:scope (list "gnu/packages/admin.scm"
                      "gnu/packages/acl.scm"
                      "gnu/packages/adns.scm"
                      "gnu/packages/antivirus.scm"
                      "gnu/packages/apparmor.scm"
                      "gnu/packages/authentication.scm"
                      "gnu/packages/cluster.scm"
                      "gnu/packages/configuration-management"
                      "gnu/packages/databases.scm"
                      "gnu/packages/distributed.scm"
                      "gnu/packages/dns.scm"
                      "gnu/packages/high-availability.scm"
                      "gnu/packages/kerberos.scm"
                      "gnu/packages/logging.scm"
                      "gnu/packages/monitoring.scm"
                      "gnu/packages/nfs.scm"
                      "gnu/packages/openldap.scm"
                      "gnu/packages/openstack.scm"
                      "gnu/packages/prometheus.scm"
                      "gnu/packages/selinux.scm"
                      "gnu/packages/storage.scm"
                      "gnu/packages/task-runners.scm"
                      "gnu/packages/terraform.scm"
                      "gnu/packages/virtualization.scm")))

(define-team telephony
  (team 'telephony
        #:name "Telephony team"
        #:description
        "Telephony packages and services such as Jami, Linphone, etc."
        #:scope (list "gnu/build/jami-service.scm"
                      "gnu/packages/jami.scm"
                      "gnu/packages/linphone.scm"
                      "gnu/packages/telephony.scm"
                      "gnu/services/telephony.scm"
                      "gnu/tests/data/jami-dummy-account.dat"
                      "gnu/tests/telephony.scm"
                      "tests/services/telephony.scm")))

(define-team tex
  (team 'tex
        #:name "TeX team"
        #:description
        "TeX, LaTeX, XeLaTeX, LuaTeX, TeXLive, the texlive-build-system, and
the \"texlive\" importer."
        #:scope (list "gnu/packages/tex.scm"
                      "gnu/packages/texlive.scm"
                      "guix/build/texlive-build-system.scm"
                      "guix/build-system/texlive.scm"
                      "guix/import/texlive.scm"
                      "guix/scripts/import/texlive.scm"
                      "tests/texlive.scm")))

(define-team translations
  (team 'translations
        #:name "Translations"
        #:scope (list "etc/news.scm"
                      (make-regexp* "^po/"))))

(define-team xfce
  (team 'xfce
        #:name "Xfce team"
        #:description "Xfce desktop environment."
        #:scope (list "gnu/packages/xfce.scm")))

(define-team zig
  (team 'zig
        #:name "Zig team"
        #:description "Zig, Zig packages, and the zig-build system"
        #:scope (list "gnu/packages/zig.scm"
                      "gnu/packages/zig-xyz.scm"
                      "guix/build/zig-build-system.scm"
                      "guix/build-system/zig.scm")))


(define-member (person "Eric Bavier"
                       "bavier@posteo.net")
  science)

(define-member (person "Lars-Dominik Braun"
                       "lars@6xq.net")
  python haskell)

(define-member (person "Jonathan Brielmaier"
                       "jonathan.brielmaier@web.de")
  mozilla)

(define-member (person "Ludovic Courtès"
                       "ludo@gnu.org")
  core home bootstrap core-packages installer
  documentation mentors)

(define-member (person "Andreas Enge"
                       "andreas@enge.fr")
  bootstrap core-packages lxqt science tex)

(define-member (person "Tanguy Le Carrour"
                       "tanguy@bioneland.org")
  python home)

(define-member (person "Tobias Geerinckx-Rice"
                       "me@tobias.gr")
  core mentors)

(define-member (person "Steve George"
                       "steve@futurile.net")
  rust)

(define-member (person "Leo Famulari"
                       "leo@famulari.name")
  kernel)

(define-member (person "Efraim Flashner"
                       "efraim@flashner.co.il")
  embedded bootstrap rust)

(define-member (person "jgart"
                       "jgart@dismail.de")
  lisp mentors)

(define-member (person "Guillaume Le Vaillant"
                       "glv@posteo.net")
  lisp)

(define-member (person "Julien Lepiller"
                       "julien@lepiller.eu")
  java ocaml translations)

(define-member (person "Philip McGrath"
                       "philip@philipmcgrath.com")
  racket)

(define-member (person "Mathieu Othacehe"
                       "othacehe@gnu.org")
  core installer mentors)

(define-member (person "Florian Pelz"
                       "pelzflorian@pelzflorian.de")
  translations)

(define-member (person "Liliana Marie Prikler"
                       "liliana.prikler@gmail.com")
  emacs games gnome)

(define-member (person "Ricardo Wurmus"
                       "rekado@elephly.net")
  r sugar)

(define-member (person "Christopher Baines"
                       "guix@cbaines.net")
  core mentors ruby)

(define-member (person "Andrew Tropin"
                       "andrew@trop.in")
  home emacs)

(define-member (person "pukkamustard"
                       "pukkamustard@posteo.net")
  ocaml)

(define-member (person "Josselin Poiret"
                       "dev@jpoiret.xyz")
  core installer)

(define-member (person "("
                       "paren@disroot.org")
  )

(define-member (person "Simon Tournier"
                       "zimon.toutoune@gmail.com")
  julia core mentors r)

(define-member (person "宋文武"
                       "iyzsong@envs.net")
  games localization lxqt qt xfce)

(define-member (person "Vagrant Cascadian"
                       "vagrant@debian.org")
  embedded)

(define-member (person "Vagrant Cascadian"
                       "vagrant@reproducible-builds.org")
  reproduciblebuilds)

(define-member (person "Maxim Cournoyer"
                       "maxim.cournoyer@gmail.com")
  documentation gnome qt telephony electronics)

(define-member (person "Katherine Cox-Buday"
                       "cox.katherine.e+guix@gmail.com")
  emacs go lisp)

(define-member (person "Munyoki Kilyungi"
                       "me@bonfacemunyoki.com")
  python lisp)

(define-member (person "Gabriel Wicki"
                       "gabriel@erlikon.ch")
  audio documentation electronics embedded)

(define-member (person "Ekaitz Zarraga"
                       "ekaitz@elenq.tech")
  bootstrap zig electronics)

(define-member (person "Divya Ranjan Pattanaik"
                       "divya@subvertising.org")
  emacs rust haskell)

(define-member (person "Clément Lassieur"
                       "clement@lassieur.org")
  mozilla)

(define-member (person "Sharlatan Hellseher"
                       "sharlatanus@gmail.com")
  go lisp python science sysadmin)

(define-member (person "Vivien Kraus"
                       "vivien@planete-kraus.eu")
  gnome)

(define-member (person "Wilko Meyer"
                       "w@wmeyer.eu")
  kernel)

(define-member (person "Mark H Weaver"
                       "mhw@netris.org")
  mozilla)

(define-member (person "Adam Faiz"
                       "adam.faiz@disroot.org")
  games)

(define-member (person "Laurent Gatto"
                       "laurent.gatto@gmail.com")
  r)

(define-member (person "Nicolas Goaziou"
                       "guix@nicolasgoaziou.fr")
  tex)

(define-member (person "André Batista"
                       "nandre@riseup.net")
  mozilla)

(define-member (person "Janneke Nieuwenhuizen"
                       "janneke@gnu.org")
  bootstrap core-packages home hurd installer)

(define-member (person "Ian Eure"
                       "ian@retrospec.tv")
  mozilla emacs)

(define-member (person "Zheng Junjie"
                       "z572@z572.online")
  core-packages qt kde)

(define-member (person "Sughosha"
                       "sughosha@disroot.org")
  kde)

(define-member (person "Jelle Licht"
                       "jlicht@fsfe.org")
  javascript)

(define-member (person "Cayetano Santos"
                       "csantosb@inventati.org")
  emacs electronics)

(define-member (person "Greg Hogan"
                       "code@greghogan.com")
  c++)

(define-member (person "Hilton Chain"
                       "hako@ultrarare.space")
  emacs home localization mozilla rust zig)

(define-member (person "Noé Lopez"
                       "noelopez@free.fr")
  gnome)

(define (find-team name)
  (or (hash-ref %teams (string->symbol name))
      (error (format #false
                     "no such team: ~a~%" name))))

(define (find-team-by-scope files)
  "Return the team(s) which scope matches at least one of the FILES, as list
of file names as string."
  (hash-fold
   (lambda (key team acc)
     (if (any (lambda (file)
                (any (match-lambda
                       ((? string? scope)
                        (string=? scope file))
                       ((? regexp*? scope)
                        (regexp*-exec scope file)))
                     (team-scope team)))
              files)
         (cons team acc)
         acc))
   '()
   %teams))

(define (cc . teams)
  "Return arguments for `git send-email' to notify the members of the given
TEAMS when a patch is received by Debbugs."
  (let ((members (append-map team-members teams)))
    (unless (null? members)
      (format #true "--add-header=\"X-Debbugs-Cc: ~{~a~^, ~}\""
              (map person-email (sort-members members))))))

(define (sort-members members)
  "Deduplicate and sort MEMBERS alphabetically by their name."
  (sort (delete-duplicates members equal?)
        (lambda (m1 m2)
          (string<? (person-name m1) (person-name m2)))))

(define (member->string member)
  "Return the 'email <name>' string representation of MEMBER."
  (let* ((name (person-name member))
         (quoted-name/maybe (if (string-contains name ",")
                                (string-append "\"" name "\"")
                                name)))
    (format #false "~a <~a>" quoted-name/maybe (person-email member))))

(define* (list-members team #:key (prefix ""))
  "Print the members of the given TEAM."
  (for-each (lambda (member)
              (format #t "~a~a~%" prefix (member->string member)))
            (sort-members (team-members team))))

(define (print-team team)
  "Print TEAM, a <team> record object."
  (format #t
          "\
id: ~a
name: ~a
description: ~a
~amembers:
"
          (team-id team)
          (team-name team)
          (or (and=> (team-description team)
                     (lambda (text)
                       (string->recutils
                        (fill-paragraph text (%text-width)
                                        (string-length "description: ")))))
              "<none>")
          (match (team-scope team)
            (() "")
            (scope (format #f "scope:~%~{+ ~a~^~%~}~%"
                           (sort (map (match-lambda
                                        ((? regexp*? rx)
                                         (regexp*-pattern rx))
                                        (item item))
                                      scope)
                                 string<?)))))
  (list-members team #:prefix "+ ")
  (newline))

(define (sort-teams teams)
  "Sort TEAMS, a list of <team> record objects."
  (sort teams
        (lambda (team1 team2)
          (string<? (symbol->string (team-id team1))
                    (symbol->string (team-id team2))))))

(define* (list-teams #:optional team-names)
  "Print all teams, their scope and their members."
  (for-each print-team
            (sort-teams
             (if team-names
                 (map find-team team-names)
                 (hash-map->list (lambda (_ value) value) %teams)))))


(define (diff-revisions rev-start rev-end)
  "Return the list of added, modified or removed files between REV-START
and REV-END, two git revision strings."
  (let* ((repository (repository-open (getcwd)))
         (commit1 (commit-lookup repository
                                 (object-id
                                  (revparse-single repository rev-start))))
         (commit2 (commit-lookup repository
                                 (object-id
                                  (revparse-single repository rev-end))))
         (diff (diff-tree-to-tree repository
                                  (commit-tree commit1)
                                  (commit-tree commit2)))
         (files '()))
    (diff-foreach
     diff
     (lambda (delta progress)
       (set! files
             (cons (diff-file-path (diff-delta-old-file delta)) files))
       0)
     (const 0)
     (const 0)
     (const 0))
    files))

(define (git-patch->commit-id file)
  "Parse the commit ID from FILE, a patch produced with git."
  (call-with-input-file file
    (lambda (port)
      (let loop ((line (read-line port)))
        (when (eof-object? line)
          (error "could not find 'from' commit in patch" file))
        (let ((m (string-match "^From ([0-9a-f]{40})" line)))
          (if m
           (match:substring m 1)
           (loop (read-line port))))))))

(define (git-patch->revisions file)
  "Return the start and end revisions of FILE, a patch file produced with git."
  (let* ((rev-end (git-patch->commit-id file))
         (rev-start (string-append rev-end "^")))
    (list rev-start rev-end)))

(define (patch->teams patch-file)
  "Return the name of the teams in scope for the changes in PATCH-FILE."
  (map (compose symbol->string team-id)
       (find-team-by-scope (apply diff-revisions
                                  (git-patch->revisions patch-file)))))


(define (main . args)
  (match args
    (("cc" . team-names)
     (apply cc (map find-team team-names)))
    (("cc-members" patch-file)
     (unless (file-exists? patch-file)
       (error "patch file does not exist:" patch-file))
     (apply main "cc-members" (git-patch->revisions patch-file)))
    (("cc-members" rev-start rev-end)
     (apply cc (find-team-by-scope
                (diff-revisions rev-start rev-end))))
    (("cc-members-header-cmd" patch-file)
     (let* ((teams (map find-team (patch->teams patch-file)))
            (members (sort-members (append-map team-members teams))))
       (unless (null? members)
         (format #true "X-Debbugs-Cc: ~{~a~^, ~}"
                 (map member->string members)))))
    (("cc-mentors-header-cmd" patch-file)
     (format #true "X-Debbugs-Cc: ~{~a~^, ~}"
             (map member->string
                  (sort-members (team-members (find-team "mentors"))))))
    (("get-maintainer" patch-file)
     (apply main "list-members" (patch->teams patch-file)))
    (("list-teams" . args)
     (list-teams))
    (("list-members" . team-names)
     (for-each
      (lambda (team-name)
        (list-members (find-team team-name)))
      team-names))
    (("show" . team-names)
     (list-teams team-names))
    (anything
     (format (current-error-port)
             "Usage: etc/teams.scm <command> [<args>]

Commands:
  cc <team-name>
      get git send-email flags for cc-ing <team-name>
  cc-members <start> <end> | <patch>
      cc teams related to files changed between revisions or in a patch file
  cc-members-header-cmd <patch>
      cc-members variant for use with 'git send-email --header-cmd'
  cc-mentors-header-cmd <patch>
      command to use with 'git send-email --header-cmd' to notify mentors
  list-teams
      list teams and their members
  list-members <team-name>
      list members belonging to <team-name>
  get-maintainer <patch>
      compatibility mode with Linux get_maintainer.pl
  show <team-name>
      display <team-name> properties~%"))))

(apply main (cdr (command-line)))
