;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025 Danny Milosavljevic <dannym@friendly-machines.com>
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

(define-module (gnu packages swift)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages ninja)
  #:use-module (gnu packages python))

(define %swift-bootstrap-version "5.7.3")

(define-public swift-cmark
  (package
    (name "swift-cmark")
    (version %swift-bootstrap-version)
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/apple/swift-cmark.git")
                    (commit (string-append "swift-" %swift-bootstrap-version
                                           "-RELEASE"))))
              (file-name (git-file-name "swift-cmark" %swift-bootstrap-version))
              (sha256
               (base32
                "0340j9x2n40yx61ma2pgqfbn3a9ijrh20iwzd1zxqq87rr76hh3z"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f
      #:build-type "Release"
      #:configure-flags
      #~(list (string-append "-DCMAKE_INSTALL_PREFIX=" #$output))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'install-cmake-exports
            (lambda _
              (install-file "src/cmarkTargets.cmake"
                            (string-append #$output "/src"))
              (substitute* (string-append #$output "/src/cmarkTargets.cmake")
                (("/tmp/guix-build-swift-cmark-[^/]+/source/src")
                 #$output)
                (("/tmp/guix-build-swift-cmark-[^/]+/build/src")
                 (string-append #$output "/lib")))
              #t)))))
    (native-inputs
     (list cmake ninja python-3))
    (home-page "https://swift.org/")
    (synopsis "CommonMark parsing and rendering library for Swift")
    (description
     "This is Apple's fork of cmark (CommonMark implementation) with
Swift-specific modifications, required to build Swift 4.2.4.")
    (license license:bsd-2)))
