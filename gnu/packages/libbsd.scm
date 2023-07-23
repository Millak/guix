;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Leo Famulari <leo@famulari.name>
;;; Copyright © 2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2023 Janneke Nieuwenhuizen <janneke@gnu.org>
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

(define-module (gnu packages libbsd)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (gnu packages crypto))

(define-public libbsd
  (package
    (name "libbsd")
    (version "0.11.7")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://libbsd.freedesktop.org/releases/"
                                  "libbsd-" version ".tar.xz"))
              (sha256
               (base32
                "0q82iaynmal3dn132jgjq21p27x3zn8zks88cg02bgzbb5h1ialv"))))
    (build-system gnu-build-system)
    (arguments
     (list #:configure-flags #~'("--disable-static")
           #:phases #~(modify-phases %standard-phases
                        (add-after 'install 'embed-absolute-libmd-references
                          (lambda* (#:key inputs #:allow-other-keys)
                            (let ((libmd (search-input-file inputs
                                                            "lib/libmd.so")))
                              ;; Add absolute references to libmd so it
                              ;; does not need to be propagated.
                              (with-directory-excursion #$output
                                (substitute* "lib/libbsd.so"
                                  (("^GROUP")
                                   (string-append "SEARCH_DIR("
                                                  (dirname libmd)
                                                  ")\nGROUP")))
                                (substitute* (find-files "lib/pkgconfig"
                                                         "\\.pc$")
                                  (("-lmd")
                                   (string-append "-L" (dirname libmd)
                                                  " -lmd")))))))
                        (add-before 'check 'disable-pwcache-test
                          (lambda _
                            ;; This test expects the presence of a root
                            ;; user and group, which do not exist in the
                            ;; build container.
                            (substitute* "test/Makefile"
                              (("pwcache\\$\\(EXEEXT\\) ")
                               ""))))
                        #$@(if (system-hurd?)
                               #~((add-after 'unpack 'skip-tests
                                  (lambda _
                                    (substitute* "test/explicit_bzero.c"
                                      (("(^| )main *\\(.*" all)
                                       (string-append all
                                                      "{\n  exit (77);//"))))))
                               #~()))))
    (inputs
     (list libmd))
    (synopsis "Utility functions from BSD systems")
    (description "This library provides useful functions commonly found on BSD
systems, and lacking on others like GNU systems, thus making it easier to port
projects with strong BSD origins, without needing to embed the same code over
and over again on each project.")
    (home-page "https://libbsd.freedesktop.org/wiki/")
    ;; This package is a collection of third-party functions that were
    ;; originally released under various non-copyleft licenses.
    (license (list bsd-2 bsd-3 bsd-4 expat isc public-domain
                   (non-copyleft "file://COPYING"
                                 "See COPYING in the distribution.")))))
