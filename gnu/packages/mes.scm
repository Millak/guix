;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2018, 2019 Jan Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2017, 2018 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
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
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages man)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages texinfo)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public nyacc-0.86
  ;; Nyacc used for bootstrap.
  (package
    (name "nyacc")
    (version "0.86.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://savannah/nyacc/"
                                  name "-" version ".tar.gz"))
              (patches (search-patches "nyacc-binary-literals.patch"))
              (sha256
               (base32
                "0lkd9lyspvhxlfs0496gsllwinh62jk9wij6gpadvx9gwz6yavd9"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("guile" ,guile-2.2)))
    (synopsis "LALR(1) Parser Generator in Guile")
    (description
     "NYACC is an LALR(1) parser generator implemented in Guile.
The syntax and nomenclature should be considered not stable.  It comes with
extensive examples, including parsers for the Javascript and C99 languages.")
    (home-page "https://savannah.nongnu.org/projects/nyacc")
    (license (list gpl3+ lgpl3+))))

(define-public nyacc
  (package
    (inherit nyacc-0.86)
    (version "0.99.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://savannah/nyacc/nyacc-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0hl5qxx19i4x1r0839sxm19ziqq65g4hy97yik81cc2yb9yvgyv3"))
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
                  #t))))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("guile" ,guile-2.2)))))

(define-public mes-0.19
  ;; Mes used for bootstrap.
  (package
    (name "mes")
    (version "0.19")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/mes/"
                                  "mes-" version ".tar.gz"))
              (sha256
               (base32
                "15h4yhaywdc0djpjlin2jz1kzahpqxfki0r0aav1qm9nxxmnp1l0"))))
    (build-system gnu-build-system)
    (supported-systems '("i686-linux" "x86_64-linux"))
    (propagated-inputs
     `(("mescc-tools" ,mescc-tools-0.5.2)
       ("nyacc" ,nyacc-0.86)))
    (native-inputs
     `(("guile" ,guile-2.2)
       ,@(let ((target-system (or (%current-target-system)
                                  (%current-system))))
           (cond
            ((string-prefix? "x86_64-linux" target-system)
             ;; Use cross-compiler rather than #:system "i686-linux" to get
             ;; MesCC 64 bit .go files installed ready for use with Guile.
             `(("i686-linux-binutils" ,(cross-binutils "i686-unknown-linux-gnu"))
               ("i686-linux-gcc" ,(cross-gcc "i686-unknown-linux-gnu"))))
            (else
             '())))
       ("graphviz" ,graphviz)
       ("help2man" ,help2man)
       ("perl" ,perl)                 ; build-aux/gitlog-to-changelog
       ("texinfo" ,texinfo)))
    (arguments
     `(#:strip-binaries? #f))  ; binutil's strip b0rkes MesCC/M1/hex2 binaries
    (synopsis "Scheme interpreter and C compiler for full source bootstrapping")
    (description
     "GNU Mes--Maxwell Equations of Software--brings the Reduced Binary Seed
bootstrap to Guix and aims to help create full source bootstrapping for
GNU/Linux distributions.  It consists of a mutual self-hosting Scheme
interpreter in C and a Nyacc-based C compiler in Scheme and is compatible with
Guile.")
    (home-page "https://gnu.org/software/mes")
    (license gpl3+)))

(define-public mes
  (package
    (inherit mes-0.19)
    (version "0.21")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/mes/"
                                  "mes-" version ".tar.gz"))
              (sha256
               (base32
                "104qxngxyl7pql8vqrnli3wfyx0ayfaqg8gjfhmk4qzrafs46slm"))))
    (propagated-inputs
     `(("mescc-tools" ,mescc-tools)
       ("nyacc" ,nyacc)))))

(define-public mescc-tools-0.5.2
  ;; Mescc-tools used for bootstrap.
  (let ((commit "bb062b0da7bf2724ca40f9002b121579898d4ef7")
        (revision "0")
        (version "0.5.2"))
    (package
      (name "mescc-tools")
      (version (git-version version revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://git.savannah.nongnu.org/r/mescc-tools.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1nc6rnax66vmhqsjg0kgx23pihdcxmww6v325ywf59vsq1jqjvff"))))
      (build-system gnu-build-system)
      (supported-systems '("i686-linux" "x86_64-linux"))
      (arguments
       `(#:make-flags (list (string-append "PREFIX=" (assoc-ref %outputs "out")))
         #:test-target "test"
         #:phases (modify-phases %standard-phases
                    (delete 'configure))))
      (synopsis "Tools for the full source bootstrapping process")
      (description
       "Mescc-tools is a collection of tools for use in a full source
bootstrapping process.  It consists of the M1 macro assembler, the hex2
linker, the blood-elf symbol table generator, the kaem shell, exec_enable and
get_machine.")
    (home-page "https://savannah.nongnu.org/projects/mescc-tools")
    (license gpl3+))))

(define-public mescc-tools
  (package
    (inherit mescc-tools-0.5.2)
    (name "mescc-tools")
    (version "0.6.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://git.savannah.nongnu.org/r/mescc-tools.git")
                    (commit (string-append "Release_" version))))
              (file-name (string-append "mescc-tools-" version "-checkout"))
              (sha256
               (base32
                "1cgxcdza6ws725x84i31la7jxmlk5a3nsij5shz1zljg0i36kj99"))))
    (arguments
     (substitute-keyword-arguments (package-arguments mescc-tools-0.5.2)
       ((#:make-flags _)
        `(list (string-append "PREFIX=" (assoc-ref %outputs "out"))
               "CC=gcc"))))))

(define-public m2-planet
  (let ((commit "b87ddb0051b168ea45f8d49a610dcd069263336a")
        (revision "2"))
    (package
      (name "m2-planet")
      (version (string-append "1.4.0-" revision "." (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/oriansj/m2-planet.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0yyc0fcbbxi9jqa1n76x0rwspdrwmc8g09jlmsw9c35nflrhmz8q"))))
      (native-inputs
       `(("mescc-tools" ,mescc-tools)))
      (build-system gnu-build-system)
      (arguments
       `(#:make-flags (list (string-append "PREFIX=" (assoc-ref %outputs "out")))
         #:tests? #f
         #:phases (modify-phases %standard-phases
                    (delete 'bootstrap)
                    (delete 'configure))))
      (synopsis "The PLAtform NEutral Transpiler")
      (description
       "M2-Planet, the PLAtform NEutral Transpiler, when combined with
mescc-tools, compiles a subset of the C language into working binaries with
introspective steps inbetween.  It is self-hosting and for bootstrapping it
also has an implementation in the M1 macro assembly language.  M2-Planet is
built as Phase-5 of the full source bootstrapping process and is capable of
building GNU Mes.")
      (home-page "https://github.com/oriansj/m2-planet")
      (license gpl3+))))
