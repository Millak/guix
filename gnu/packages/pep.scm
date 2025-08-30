;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2020, 2021 Hartmut Goebel <h.goebel@crazy-compilers.com>
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

(define-module (gnu packages pep)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix build-system pyproject)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages java)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages mail) ; for libetpan
  #:use-module (gnu packages nettle)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages sequoia)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xml))

(define-public yml2
  (package
    (name "yml2")
    (version "2.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitea.pep.foundation/fdik/yml2")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0fm1x1fv4lwcpbn59s55idzf7x173n59xpz8rlrxalmi6gvsjijr"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; XXX: There is no testing framework, only a samples directory.
      #:tests? #f))
    (native-inputs (list python-setuptools python-wheel))
    (propagated-inputs (list python-lxml))
    (home-page "https://fdik.org/yml/")
    (synopsis "Use a Domain Specific Language for XML without defining
a grammar")
    (description
     "The YML compiler is a small Python script.  It provides the command line
front end yml2c.  As default, it compiles your script and outputs to stdout,
that usually is the terminal.  Your shell provides options to redirect the
output into a pipe or a file.")
    (license license:gpl2)))

(define fdik-libetpan
  ;; pEp Engine requires libetpan with a set of patches that have not been
  ;; upstreamed yet.
  (let ((commit "0b80c39dd1504462ba3a39dc53db7c960c3a63f3") ; 2020-11-27
        (checksum "0gv3ivaziadybjlf6rfpv1j5z5418243v5cvl4swlxd2njsh7gjk")
        (revision "6"))
   (package
    (inherit libetpan)
    (name "fdik-libetpan")
    (version (string-append "1.6-" revision "." (string-take commit 8)))
    (source
     (origin
       (inherit (package-source libetpan))
       (method git-fetch)
       (uri (git-reference
             (url "https://gitea.pep.foundation/pEp.foundation/libetpan")
             (commit commit)))
       (file-name (string-append name "-" version))
       (sha256 (base32 checksum)))))))

