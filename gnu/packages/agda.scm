;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Alex ter Weele <alex.ter.weele@gmail.com>
;;; Copyright © 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2018 Alex Vong <alexvong1995@gmail.com>
;;; Copyright © 2018, 2022 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 John Soo <jsoo1@asu.edu>
;;; Copyright © 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2023 Josselin Poiret <dev@jpoiret.xyz>
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

(define-module (gnu packages agda)
  #:use-module (gnu packages)
  #:use-module (gnu packages haskell-check)
  #:use-module (gnu packages haskell-web)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages python)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages texinfo)
  #:use-module (guix build-system agda)
  #:use-module (guix build-system emacs)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system haskell)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))

(define-public agda
  (package
    (name "agda")
    (version "2.6.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/agda/agda.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0n4avd58j45rdcmnwgrmz5s0ril0z4n2z711mwwbahl50f7359ky"))
       (patches (search-patches "agda-libdirs-env-variable.patch"
                                "agda-use-sphinx-5.patch"))))
    (build-system haskell-build-system)
    (inputs
     (list ghc-aeson
           ghc-alex
           ghc-ansi-terminal
           ghc-async
           ghc-blaze-html
           ghc-boxes
           ghc-case-insensitive
           ghc-data-hash
           ghc-edit-distance
           ghc-equivalence
           ghc-gitrev
           ghc-happy
           ghc-hashable
           ghc-hashtables
           ghc-monad-control
           ghc-murmur-hash
           ghc-parallel
           ghc-peano
           ghc-regex-tdfa
           ghc-split
           ghc-strict
           ghc-text-icu
           ghc-unordered-containers
           ghc-uri-encode
           ghc-vector-hashtables
           ghc-zlib))
    (native-inputs
     (list python
           python-sphinx
           python-sphinx-rtd-theme
           texinfo
           imagemagick))
    (arguments
     (list #:modules `((guix build haskell-build-system)
                       (guix build utils)
                       (srfi srfi-26)
                       (ice-9 match))
           #:configure-flags #~(list "-foptimise-heavily" "-fenable-cluster-counting")
           #:phases
           #~(modify-phases %standard-phases
               ;; This allows us to call the 'agda' binary before installing.
               (add-after 'unpack 'set-ld-library-path
                 (lambda _
                   (setenv "LD_LIBRARY_PATH" (string-append (getcwd) "/dist/build"))))
               (add-after 'build 'agda-compile
                 (lambda* (#:key outputs #:allow-other-keys)
                   (let ((agda-compiler (string-append #$output "/bin/agda")))
                     (for-each (cut invoke agda-compiler <>)
                               (find-files (string-append #$output "/share")
                                           "\\.agda$")))))
               (add-after 'agda-compile 'install-info
                 (lambda _
                   (with-directory-excursion "doc/user-manual"
                     (invoke "sphinx-build" "-b" "texinfo"
                             "." "_build_texinfo")
                     (with-directory-excursion "_build_texinfo"
                       (setenv "infodir" (string-append #$output
                                                        "/share/info"))
                       (invoke "make" "install-info"))))))))
    (search-paths
     (list (search-path-specification
            (variable "AGDA_LIBDIRS")
            (files (list "lib/agda")))))
    (native-search-paths
     search-paths)
    (home-page "https://wiki.portal.chalmers.se/agda/")
    (synopsis
     "Dependently typed functional programming language and proof assistant")
    (description
     "Agda is a dependently typed functional programming language: it has
inductive families, which are similar to Haskell's GADTs, but they can be
indexed by values and not just types.  It also has parameterised modules,
mixfix operators, Unicode characters, and an interactive Emacs interface (the
type checker can assist in the development of your code).  Agda is also a
proof assistant: it is an interactive system for writing and checking proofs.
Agda is based on intuitionistic type theory, a foundational system for
constructive mathematics developed by the Swedish logician Per Martin-Löf.  It
has many similarities with other proof assistants based on dependent types,
such as Coq, Epigram and NuPRL.")
    ;; Agda is distributed under the MIT license, and a couple of
    ;; source files are BSD-3.  See LICENSE for details.
    (license (list license:expat license:bsd-3))))

(define-public emacs-agda2-mode
  (package
    (name "emacs-agda2-mode")
    (version (package-version agda))
    (source (package-source agda))
    (build-system emacs-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'enter-elisp-dir
            (lambda _ (chdir "src/data/emacs-mode"))))))
    (home-page "https://agda.readthedocs.io/en/latest/tools/emacs-mode.html")
    (synopsis "Emacs mode for Agda")
    (description "This Emacs mode enables interactive development with
Agda.  It also aids the input of Unicode characters.")
    (license (package-license agda))))

(define-public agda-ial
  (let ((revision "1")
        ;; There hasn't been a release in a long time, and the last one
        ;; doesn't build with Agda 2.6.
        (commit "ded30c410d5d40142249686572aa1acd1b2f8cc7"))
   (package
     (name "agda-ial")
     (version (git-version "1.5.0" revision commit))
     (source (origin
               (method git-fetch)
               (uri (git-reference (url "https://github.com/cedille/ial")
                                   (commit commit)))
               (file-name (git-file-name name version))
               (sha256
                (base32
                 "0xn6zvp1wnm0i84pz1rfbzfmayd15ch4i5s11ycd88d22pxd55dc"))))
     (build-system agda-build-system)
     (arguments
      (list
       #:gnu-and-haskell? #t
       #:phases
       #~(modify-phases %standard-phases
           (add-before 'build 'patch-dependencies
             (lambda _ (patch-shebang "find-deps.sh")))
           (replace 'build
             (lambda _
               (invoke "make"))))))
     (home-page "https://github.com/cedille/ial")
     (synopsis "The Iowa Agda Library")
     (description
      "The goal is to provide a concrete library focused on verification
examples, as opposed to mathematics.  The library has a good number
of theorems for booleans, natural numbers, and lists.  It also has
trees, tries, vectors, and rudimentary IO.  A number of good ideas
come from Agda's standard library.")
     (license license:expat))))

(define-public agda-stdlib
  (package
    (name "agda-stdlib")
    (version "1.7.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/agda/agda-stdlib")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0y6rns64rrkh8hw7mamcf6797329pi4ravpak5zijpnkzdagmlmy"))))
    (build-system agda-build-system)
    (arguments
     (list
      #:plan '(("^\\./README.agda$" "-i."))
      #:gnu-and-haskell? #t
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'generate-everything
            (lambda* (#:key inputs native-inputs #:allow-other-keys)
              (invoke
               (search-input-file (or native-inputs inputs) "/bin/runhaskell")
               "GenerateEverything.hs"))))))
    (native-inputs (list ghc-filemanip))
    (synopsis "The Agda Standard Library")
    (description
     "The standard library aims to contain all the tools needed to write
both programs and proofs easily.  While we always try and write efficient
code, we prioritize ease of proof over type-checking and normalization
performance.  If computational performance is important to you, then perhaps
try agda-prelude instead.")
    (home-page "https://wiki.portal.chalmers.se/agda/pmwiki.php")
    (license license:expat)))

(define-public agda-categories
  (package
    (name "agda-categories")
    (version "0.1.7.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/agda/agda-categories.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0xwgm2mfl2pxipsv31bin8p14y1yhd9n27lv3clvsxd4z9yc034m"))
              (patches (search-patches "agda-categories-remove-incompatible-flags.patch"
                                       "agda-categories-use-find.patch"
                                       "agda-categories-use-stdlib-1.7.3.patch"))))
    (build-system agda-build-system)
    (arguments
     (list
      #:gnu-and-haskell? #t
      #:phases
      #~(modify-phases %standard-phases
          (replace 'build
            (lambda _
              (invoke "make"))))))
    (propagated-inputs
     (list agda-stdlib))
    (synopsis "New Categories library for Agda")
    (description "A new Categories library for Agda")
    (home-page "https://github.com/agda/agda-categories")
    (license license:expat)))

(define-public agda-cubical
  (package
    (name "agda-cubical")
    (version "0.6")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/agda/cubical.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0zq0z328zcjmm43mrv2ks27i1dnbylcf8mhzja2hd4gvz1kq1ays"))))
    (build-system agda-build-system)
    (arguments
     (list
      #:gnu-and-haskell? #t
      #:phases
      #~(modify-phases %standard-phases
          (replace 'build
            (lambda _
              (invoke "make"))))))
    (synopsis "Standard library for Cubical Agda")
    (description "A standard library for Cubical Agda, comparable to
agda-stdlib but using cubical methods.")
    (home-page "https://github.com/agda/cubical")
    (license license:expat)))

(define-public agda-1lab
  ;; Upstream doesn't do releases (yet).  Use a commit that builds with 2.6.4,
  ;; since they use Agda HEAD.
  (let* ((revision "2")
         (commit "549fdb1c948a975e90e70f871993a4a4239aa280"))
    (package
      (name "agda-1lab")
      (version (git-version "0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/plt-amy/1lab.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1k4zj8dibyplakpxaw4a8hpsaqhakynjb83dqxrva4h4ssj6gkqj"))))
      (build-system agda-build-system)
      (arguments
       ;; Check files individually first, to avoid running out of heap :(
       (list #:plan '(("src/.+/.+\\.lagda\\.md$")
                      ("src/index\\.lagda\\.md$"))))
      (synopsis "Reference resource for mathematics done in Homotopy Type Theory")
      (description "A formalised, cross-linked reference resource for
mathematics done in Homotopy Type Theory.  Unlike the HoTT book, the 1lab is
not a “linear” resource: Concepts are presented as a directed graph, with
links indicating dependencies.")
      (home-page "https://1lab.dev")
      (license license:agpl3))))
