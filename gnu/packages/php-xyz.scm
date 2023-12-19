;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Julien Lepiller <julien@lepiller.eu>
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

(define-module (gnu packages php-xyz)
  #:use-module (gnu packages)
  #:use-module (gnu packages php)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system copy)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:))

(define-public composer-classloader
  (package
    (name "composer-classloader")
    (version "1.9.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/composer/composer")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0127zmmg3yx84ljngfs86q7kjhyypybkf4d1ihfrfnzgynzxfxdf"))))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan
       '(("src/Composer/Autoload/ClassLoader.php" "/share/web/composer/"))))
    (home-page "https://getcomposer.org")
    (synopsis "PHP class loader extracted from the composer package")
    (description "This package contains the class loader class used by Composer to
build its autoloading feature.  This package is used by the composer-build-system
to build its own store-aware autoloading feature.")
    (license license:expat)))
