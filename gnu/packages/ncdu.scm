;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 John Darrington <jmd@gnu.org>
;;; Copyright © 2018–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2022-2025 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (gnu packages ncdu)
  #:use-module (gnu packages)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages zig)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system zig))

(define-public ncdu-1
  ;; This old version is ‘LTS’.  Version 2 works fine and has more features,
  ;; but Zig is still a fast-moving target and doesn't support cross-compilation
  ;; yet, so we'll keep both for just a little longer.
  (package
    (name "ncdu")
    (version "1.22")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://dev.yorhel.nl/download/ncdu-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0sqp39lkryjljvvrwv0x37a9fklg3g060iqhh42i5m84vjbc1mha"))))
    (build-system gnu-build-system)
    (arguments
     (list #:configure-flags
           ;; Configure default shell for spawning shell when $SHELL is not set
           #~(list (string-append "--with-shell="
                                  #$(this-package-input "bash-minimal")
                                  "/bin/sh"))))
    (native-inputs (list pkg-config))
    (inputs (list bash-minimal ncurses))
    (synopsis "Ncurses-based disk usage analyzer")
    (description
     "Ncdu is a disk usage analyzer with an ncurses interface, aimed to be
run on a remote server where you don't have an entire graphical setup, but have
to do with a simple SSH connection.  ncdu aims to be fast, simple and easy to
use, and should be able to run in any minimal POSIX-like environment with
ncurses installed.")
    (license (x11-style "file://COPYING"))
    (home-page "https://dev.yorhel.nl/ncdu")))

(define-public ncdu
  (package
    (inherit ncdu-1)
    (name "ncdu")
    (version "2.8.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://dev.yorhel.nl/download/ncdu-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0g815ix1pk54dqzaxlb0paiwjhyz0wbq6368rmx7jyassdjsfbq2"))))
    (build-system zig-build-system)
    (arguments
     (list #:zig zig-0.14
           #:install-source? #f
           #:zig-release-type "safe"
           #:zig-build-flags
           #~(list "-Dpie")))
    (inputs (list ncurses `(,zstd "lib")))
    (native-inputs (list pkg-config))
    (properties `((tunable? . #t)))))

(define-public ncdu-2
  (deprecated-package "ncdu2" ncdu))
