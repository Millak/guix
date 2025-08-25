;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017-2025 Janneke Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2017, 2018 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020, 2021, 2022 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2021 Xinglu Chen <public@yoctocell.xyz>
;;; Copyright © 2021 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2025 Felix Lechner <felix.lechner@lease-up.com>
;;; Copyright © 2025 Andreas Enge <andreas@enge.fr>
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

(define-module (gnu packages mes)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages cross-base)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages man)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages texinfo)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public nyacc-1.08.1
  (package
    (name "nyacc")
    (version "1.08.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://savannah/nyacc/nyacc-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1vrz3pnlr3njwk6ksz85slcwawi8ngiqbw94wd9x3mgv85vsfmys"))
              (modules '((guix build utils)))
              (snippet
               '(substitute* "configure"
                  (("GUILE_GLOBAL_SITE=\\$prefix.*")
                   "GUILE_GLOBAL_SITE=\
$prefix/share/guile/site/$GUILE_EFFECTIVE_VERSION\n")))))
    (build-system gnu-build-system)
    (native-inputs (list pkg-config))
    (inputs (list guile-3.0))
    (propagated-inputs (list guile-bytestructures))
    (home-page "https://savannah.nongnu.org/projects/nyacc")
    (synopsis "LALR(1) Parser Generator in Guile")
    (description
     "@acronym{NYACC, Not Yet Another Compiler Compiler} is set of Guile modules
for generating parsers and lexical analyzers.  It provides sample parsers,
pretty-printers using SXML trees as an intermediate representation, a decent C
parser and an `FFI Helper' tool to help create Guile Scheme bindings for C-based
libraries.  It also provides (partially implemented) compilers based on these
parsers to allow execution with Guile as extension languages.")
    (license (list gpl3+ lgpl3+))))

(define-public nyacc-1.00.2
  ;; The source of this package is used for bootstrapping in
  ;; commencement.scm. Otherwise it could be removed.
  (package
    (inherit nyacc-1.08.1)
    (version "1.00.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://savannah/nyacc/nyacc-"
                                  version ".tar.gz"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (substitute* (find-files "." "^Makefile\\.in$")
                    (("^SITE_SCM_DIR =.*")
                     "SITE_SCM_DIR = \
@prefix@/share/guile/site/@GUILE_EFFECTIVE_VERSION@\n")
                    (("^SITE_SCM_GO_DIR =.*")
                     "SITE_SCM_GO_DIR = \
@prefix@/lib/guile/@GUILE_EFFECTIVE_VERSION@/site-ccache\n")
                    (("^INFODIR =.*")
                     "INFODIR = @prefix@/share/info\n")
                    (("^DOCDIR =.*")
                     "DOCDIR = @prefix@/share/doc/$(PACKAGE_TARNAME)\n"))
                  #t))
              (sha256
               (base32
                "065ksalfllbdrzl12dz9d9dcxrv97wqxblslngsc6kajvnvlyvpk"))))

    ;; XXX: Remove GUILE-BYTESTRUCTURES, an optional dependency needed to
    ;; build the FFI helper, because it fails to build.
    (propagated-inputs '())

    (inputs (list guile-3.0))))

(define-public nyacc-2.01
  (package
    (inherit nyacc-1.00.2)
    (version "2.01.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://savannah/nyacc/nyacc-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0dp1439j7db3zhpyqiah3vf3s193y4ip8mh7mc5lz0abgml3x3vg"))
              (modules '((guix build utils)))
              (snippet
               '(substitute* "configure"
                  (("GUILE_GLOBAL_SITE=\\$prefix.*")
                   "GUILE_GLOBAL_SITE=\
$prefix/share/guile/site/$GUILE_EFFECTIVE_VERSION\n")))))))

(define-public nyacc
  (package
    (inherit nyacc-1.00.2)
    (version "2.02.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://savannah/nyacc/nyacc-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "01829c24v531036rj8grcwx4hmiy3f0jznc9zbfa4wrslmq566k9"))
              (modules '((guix build utils)))
              (snippet
               '(substitute* "configure"
                  (("GUILE_GLOBAL_SITE=\\$prefix.*")
                   "GUILE_GLOBAL_SITE=\
$prefix/share/guile/site/$GUILE_EFFECTIVE_VERSION\n")))))))

(define-public mes
  (package
    (name "mes")
    (version "0.27.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/mes/"
                                  "mes-" version ".tar.gz"))
              (sha256
               (base32
                "0pgjzlynfzdfq5xrxirvsrj4sdvnwq99s6xxwfhzhjga8zm40fhq"))))
    (supported-systems '("armhf-linux" "i686-linux"
                         "x86_64-linux" "riscv64-linux"))
    (propagated-inputs (list mescc-tools nyacc))
    (native-inputs
     (append (list guile-3.0)
         (let ((target-system (or (%current-target-system)
                                  (%current-system))))
           (cond
            ((string-prefix? "x86_64-linux" target-system)
             ;; Use cross-compiler rather than #:system "i686-linux" to get
             ;; MesCC 64 bit .go files installed ready for use with Guile.
             (list (cross-binutils "i686-unknown-linux-gnu")
                   (cross-gcc "i686-unknown-linux-gnu")))
         ((string-prefix? "aarch64-linux" target-system)
          ;; Use cross-compiler rather than #:system "armhf-linux" to get
          ;; MesCC 64 bit .go files installed ready for use with Guile.
          (let ((triplet "arm-linux-gnueabihf"))
            (list (cross-binutils triplet) (cross-gcc triplet))))
         (else
          '())))
       (list graphviz help2man
             m2-planet
             perl                               ;build-aux/gitlog-to-changelog
             texinfo)))
    (build-system gnu-build-system)
    (arguments
     `(#:strip-binaries? #f))  ; binutil's strip b0rkes MesCC/M1/hex2 binaries
    (native-search-paths
     (list (search-path-specification
            (variable "C_INCLUDE_PATH")
            (files '("include")))
           (search-path-specification
            (variable "LIBRARY_PATH")
            (files '("lib")))
           (search-path-specification
            (variable "MES_PREFIX")
            (separator #f)
            (files '("")))))
    (synopsis "Scheme interpreter and C compiler for full source bootstrapping")
    (description
     "GNU Mes--Maxwell Equations of Software--brings the Reduced Binary Seed
bootstrap to Guix and aims to help create full source bootstrapping for
GNU/Linux distributions.  It consists of a mutual self-hosting Scheme
interpreter in C and a Nyacc-based C compiler in Scheme and is compatible with
Guile.")
    (home-page "https://www.gnu.org/software/mes/")
    (license gpl3+)))

(define-public mescc-tools
  (package
    (name "mescc-tools")
    (version "1.7.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://savannah/" name "/"
                                  name "-" version ".tar.gz"))
              (sha256
               (base32
                "0p2pnvci9vglmf7zas12ay2v5gnrwafqsqqw1dfyb2bgayzzg0mn"))))
    (build-system gnu-build-system)
    (supported-systems '("i686-linux" "x86_64-linux"
                         "armhf-linux" "aarch64-linux"
                         "riscv64-linux"))
    (arguments
     (list
      #:make-flags #~(list (string-append "PREFIX=" #$output))
      #:test-target "test"
      #:phases #~(modify-phases %standard-phases
                   (delete 'configure))))
    (native-inputs (list which))
    (synopsis "Tools for the full source bootstrapping process")
    (description
     "Mescc-tools is a collection of tools for use in a full source
bootstrapping process.  It consists of the M1 macro assembler, the hex2
linker, the blood-elf symbol table generator, the kaem shell, exec_enable and
get_machine.")
    (home-page "https://savannah.nongnu.org/projects/mescc-tools")
    (license gpl3+)))

(define-public m2-planet
  (package
    (name "m2-planet")
    (version "1.12.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/oriansj/M2-Planet/releases/download/"
                    "Release_" version "/" name "-" version ".tar.gz"))
              (sha256
               (base32
                "16vgad5wa5lmh6mjnkid4qn2xs7hfcfn43z9gd8iljzvsxl2i8z7"))))
    (native-inputs (list mescc-tools))
    (build-system gnu-build-system)
    (supported-systems '("i686-linux" "x86_64-linux"
                         "armhf-linux" "aarch64-linux"
                         "riscv64-linux"))
    (arguments
     `(#:make-flags (list (string-append "PREFIX=" (assoc-ref %outputs "out"))
                          ,(string-append "CC=" (cc-for-target)))
       #:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'bootstrap)
                  (delete 'configure))))
    (synopsis "The PLAtform NEutral Transpiler")
    (description
     "M2-Planet, the PLAtform NEutral Transpiler, when combined with
mescc-tools, compiles a subset of the C language into working binaries with
introspective steps in between.  It is self-hosting and for bootstrapping it
also has an implementation in the M1 macro assembly language.  M2-Planet is
built as Phase-5 of the full source bootstrapping process and is capable of
building GNU Mes.")
    (home-page "https://github.com/oriansj/m2-planet")
    (license gpl3+)))

