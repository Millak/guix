;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Federico Beffa <beffa@fbengineering.ch>
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

(define-module (test-hackage)
  #:use-module (guix import cabal)
  #:use-module (guix import hackage)
  #:use-module (guix tests)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 match))

(define test-cabal-1
  "name: foo
version: 1.0.0
homepage: http://test.org
synopsis: synopsis
description: description
license: BSD3
executable cabal
  build-depends:
    HTTP       >= 4000.2.5 && < 4000.3,
    mtl        >= 2.0      && < 3
")

(define test-cabal-2
  "name: foo
version: 1.0.0
homepage: http://test.org
synopsis: synopsis
description: description
license: BSD3
executable cabal {
build-depends:
  HTTP       >= 4000.2.5 && < 4000.3,
  mtl        >= 2.0      && < 3
}
")

;; Check compiler implementation test with and without spaces.
(define test-cabal-3
  "name: foo
version: 1.0.0
homepage: http://test.org
synopsis: synopsis
description: description
license: BSD3
library
  if impl(ghc >= 7.2 && < 7.6)
    Build-depends: ghc-a
  if impl(ghc>=7.2&&<7.6)
    Build-depends: ghc-b
  if impl(ghc == 7.8)
    Build-depends: 
      HTTP       >= 4000.2.5 && < 4000.3,
      mtl        >= 2.0      && < 3
")

;; A fragment of a real Cabal file with minor modification to check precedence
;; of 'and' over 'or', missing final newline, spaces between keywords and
;; parentheses and between key and column.
(define test-read-cabal-1
  "name: test-me
library
  -- Choose which library versions to use.
  if flag(base4point8)
    Build-depends: base >= 4.8 && < 5
  else
    if flag(base4)
      Build-depends: base >= 4 && < 4.8
    else
      if flag(base3)
        Build-depends: base >= 3 && < 4
      else
        Build-depends: base < 3
  if flag(base4point8) || flag (base4) && flag(base3)
    Build-depends: random
  Build-depends : containers

  -- Modules that are always built.
  Exposed-Modules:
    Test.QuickCheck.Exception")

(test-begin "hackage")

(define* (eval-test-with-cabal test-cabal #:key (cabal-environment '()))
  (mock
   ((guix import hackage) hackage-fetch
    (lambda (name-version)
      (call-with-input-string test-cabal
        read-cabal)))
   (match (hackage->guix-package "foo" #:cabal-environment cabal-environment)
     (('package
        ('name "ghc-foo")
        ('version "1.0.0")
        ('source
         ('origin
           ('method 'url-fetch)
           ('uri ('string-append
                  "https://hackage.haskell.org/package/foo/foo-"
                  'version
                  ".tar.gz"))
           ('sha256
            ('base32
             (? string? hash)))))
        ('build-system 'haskell-build-system)
        ('inputs
         ('quasiquote
          (("ghc-http" ('unquote 'ghc-http))
           ("ghc-mtl" ('unquote 'ghc-mtl)))))
        ('home-page "http://test.org")
        ('synopsis (? string?))
        ('description (? string?))
        ('license 'bsd-3))
      #t)
     (x
      (pk 'fail x #f)))))

(test-assert "hackage->guix-package test 1"
  (eval-test-with-cabal test-cabal-1))

(test-assert "hackage->guix-package test 2"
  (eval-test-with-cabal test-cabal-2))

(test-assert "hackage->guix-package test 3"
  (eval-test-with-cabal test-cabal-3
                        #:cabal-environment '(("impl" . "ghc-7.8"))))

(test-assert "read-cabal test 1"
  (match (call-with-input-string test-read-cabal-1 read-cabal)
    ((("name" ("test-me"))
      ('section 'library
               (('if ('flag "base4point8")
                    (("build-depends" ("base >= 4.8 && < 5")))
                    (('if ('flag "base4")
                         (("build-depends" ("base >= 4 && < 4.8")))
                         (('if ('flag "base3")
                              (("build-depends" ("base >= 3 && < 4")))
                              (("build-depends" ("base < 3"))))))))
                ('if ('or ('flag "base4point8")
                          ('and ('flag "base4") ('flag "base3")))
                    (("build-depends" ("random")))
                    ())
                ("build-depends" ("containers"))
                ("exposed-modules" ("Test.QuickCheck.Exception")))))
     #t)
    (x (pk 'fail x #f))))

(test-end "hackage")
