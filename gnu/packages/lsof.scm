;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2017 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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

(define-module (gnu packages lsof)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages groff)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages linux))

(define-public lsof
  (package
    (name "lsof")
    (version "4.98.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/lsof-org/lsof")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0cjmhd01p5a9cy52lirv1rkidrzhyn366f4h212jcf1cmp8xh0hd"))))
    (build-system gnu-build-system)
    (native-inputs (list automake
                         autoconf
                         groff ;for soelim
                         perl
                         pkg-config
                         procps ;for ps
                         util-linux)) ;for unshare
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-before 'bootstrap 'disable-failing-tests
                    (lambda _
                      (substitute* "Makefile.am"
                        ;; Fails with ‘ERROR!!! client gethostbyaddr() failure’.
                        (("(TESTS \\+=.*) tests/LTsock" _ prefix)
                         prefix)
                        ;; Fails because /proc not mounted in sandbox
                        (("\tdialects/linux/tests/case-20-epoll.bash \\\\")
                         "\\")))))))
    (synopsis "Display information about open files")
    (description
     "Lsof stands for LiSt Open Files, and it does just that.
It lists information about files that are open by the processes running
on the system.")
    (license (license:fsf-free
              "file://00FAQ"
              "License inspired by zlib, see point 1.9 of 00FAQ in the distribution."))
    (home-page "https://people.freebsd.org/~abe/")))
