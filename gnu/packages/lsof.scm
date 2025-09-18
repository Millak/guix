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
  #:use-module (guix gexp)
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
    (version "4.99.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/lsof-org/lsof")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1v32407al4j0hhcph95lv4xvr9h012lii29iyq41iwj39zwfavax"))
              (patches (search-patches "lsof-compat-linux-6.9.patch"))))
    (build-system gnu-build-system)
    (native-inputs (list automake
                         autoconf
                         groff ;for soelim
                         libtool
                         perl
                         pkg-config
                         procps ;for ps
                         util-linux)) ;for unshare
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'fix-configure-ac-version
                 ;; see https://github.com/lsof-org/lsof/commit/932a0b3b1992497e23fd9b8d31116b9ca9b0f98d
                 ;; to fix tests/case-01-version.bash test fail.
                 (lambda _
                   (substitute* "configure.ac"
                     (("4\\.99\\.0")
                      "4.99.3"))))
               (add-before 'bootstrap 'disable-failing-tests
                 (lambda _
                   (substitute* "Makefile.am"
                     ;; Fails with ‘ERROR!!! client gethostbyaddr() failure’.
                     (("(TESTS \\+=.*) tests/LTsock" _ prefix)
                      prefix)
                     ;; Fails on CoW filesystems (eg. btrfs)
                     (("tests/LTlock") "")
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
