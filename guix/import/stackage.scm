;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2020 Martin Becze <mjbecze@riseup.net>
;;; Copyright © 2021 Xinglu Chem <public@yoctocell.xyz>
;;; Copyright © 2021, 2023, 2023 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2022 Hartmut Goebel <h.goebel@crazy-compilers.com>
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

(define-module (guix import stackage)
  #:use-module (ice-9 match)
  #:use-module (json)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (guix import json)
  #:use-module (guix import hackage)
  #:autoload   (guix import cabal) (eval-cabal)
  #:use-module (guix import utils)
  #:use-module (guix memoization)
  #:use-module (guix packages)
  #:use-module (guix upstream)
  #:use-module (guix diagnostics)
  #:use-module (guix i18n)
  #:export (%stackage-url
            stackage->guix-package
            stackage-recursive-import
            %stackage-updater))


;;;
;;; Stackage info fetcher and access functions
;;;

(define %stackage-url
  (make-parameter "https://www.stackage.org"))

;; Latest LTS version compatible with current GHC.
(define %default-lts-version "20.26")

(define-json-mapping <stackage-lts> make-stackage-lts
  stackage-lts?
  json->stackage-lts
  (snapshot    stackage-lts-snapshot "snapshot" json->snapshot)
  (packages    stackage-lts-packages "packages"
               (lambda (vector)
                 (map json->stackage-package (vector->list vector)))))

(define-json-mapping <snapshot> make-snapshot
  stackage-snapshot?
  json->snapshot
  (name        snapshot-name)
  (ghc-version snapshot-ghc-version)
  (compiler    snapshot-compiler))

(define-json-mapping <stackage-package> make-stackage-package
  stackage-package?
  json->stackage-package
  (origin      stackage-package-origin)
  (name        stackage-package-name)
  (version     stackage-package-version)
  (synopsis    stackage-package-synopsis))

(define stackage-lts-info-fetch
  ;; "Retrieve the information about the LTS Stackage release VERSION."
  (memoize
   (lambda* (#:optional (version ""))
     (let* ((url (string-append (%stackage-url)
                                "/lts-" (if (string-null? version)
                                            %default-lts-version
                                            version)))
            (lts-info (and=> (json-fetch url) json->stackage-lts)))
       (or lts-info
           (raise (formatted-message (G_ "LTS release version not found: ~a")
                                     version)))))))

(define (lts-package-version packages name)
  "Return the version of the package with upstream NAME included in PACKAGES."
  (let ((pkg (find (lambda (pkg) (string=? (stackage-package-name pkg) name))
                   packages)))
    (and=> pkg stackage-package-version)))


;;;
;;; Importer entry point
;;;

(define (hackage-name-version name version)
  (and version (string-append  name "@" version)))

(define stackage->guix-package
  (memoize
   (lambda* (package-name ; upstream name
             #:key
             (include-test-dependencies? #t)
             (lts-version %default-lts-version)
             (packages
              (stackage-lts-packages
               (stackage-lts-info-fetch lts-version)))
             #:allow-other-keys)
     "Fetch Cabal file for PACKAGE-NAME from hackage.haskell.org.  The retrieved
version corresponds to the version of PACKAGE-NAME specified in the LTS-VERSION
release at stackage.org.  Return the `package' S-expression corresponding to
that package, or #f on failure.  PACKAGES-INFO is the alist with the packages
included in the Stackage LTS release."
     (let* ((version (lts-package-version packages package-name))
            (name-version (hackage-name-version package-name version)))
       (if name-version
           (hackage->guix-package name-version
                                  #:include-test-dependencies?
                                  include-test-dependencies?)
           (raise (formatted-message (G_ "~a: Stackage package not found")
                                     package-name)))))))

(define (stackage-recursive-import package-name . args)
  (recursive-import package-name
                    #:repo->guix-package (lambda* (name #:key version #:allow-other-keys)
                                           (apply stackage->guix-package (cons name args)))
                    #:guix-name hackage-name->package-name))


;;;
;;; Updater
;;;

(define latest-lts-release
  (let ((packages
         (mlambda ()
           (stackage-lts-packages
            (stackage-lts-info-fetch %default-lts-version)))))
    (lambda* (pkg #:key (version #f))
      "Return an <upstream-source> for the latest Stackage LTS release of
PACKAGE or #f if the package is not included in the Stackage LTS release."
      (when version
        (raise
         (formatted-message
          (G_ "~a updater doesn't support updating to a specific version, sorry.")
          "stackage")))
      (let* ((hackage-name (package-upstream-name* pkg))
             (version (lts-package-version (packages) hackage-name))
             (name-version (hackage-name-version hackage-name version)))
        (match (and=> name-version hackage-fetch)
          (#f
           (warning (G_ "failed to parse ~a~%")
                    (hackage-cabal-url hackage-name))
           #f)
          (_ (let ((url (hackage-source-url hackage-name version)))
               (upstream-source
                (package (package-name pkg))
                (version version)
                (urls (list url))
                (inputs
                 (let ((cabal (eval-cabal (hackage-fetch name-version) '())))
                   (cabal-package-inputs cabal)))))))))))

(define (stackage-lts-package? package)
  "Return whether PACKAGE is available on the default Stackage LTS release."
  (and (hackage-package? package)
       (false-if-networking-error
        (let ((packages (stackage-lts-packages
                         (stackage-lts-info-fetch %default-lts-version)))
              (hackage-name (package-upstream-name* package)))
          (find (lambda (package)
                  (string=? (stackage-package-name package) hackage-name))
                packages)))))

(define %stackage-updater
  (upstream-updater
   (name 'stackage)
   (description "Updater for Stackage LTS packages")
   (pred stackage-lts-package?)
   (import latest-lts-release)))

;;; stackage.scm ends here
