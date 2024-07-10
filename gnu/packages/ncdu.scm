;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 John Darrington <jmd@gnu.org>
;;; Copyright © 2018–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2022-2024 Efraim Flashner <efraim@flashner.co.il>
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
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages perl)
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
    (version "1.20")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://dev.yorhel.nl/download/ncdu-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0wlmpq8gzcl1fim8jba3g0q0bbn4jcrbkns2n95kfwmy3a2bpqjz"))))
    (build-system gnu-build-system)
    (inputs (list ncurses))
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
    (version "2.2.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://dev.yorhel.nl/download/ncdu-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "14zrmcxnrczamqjrib99jga05ixk0dzfav3pd6s1h8vm9q121nch"))
              (modules '((guix build utils)))
              (snippet
               #~(begin
                   ;; Delete a pregenerated man page.  We'll build it ourselves.
                   (delete-file "ncdu.1")))))
    (build-system zig-build-system)
    (arguments
     (list
       #:phases
       #~(modify-phases %standard-phases
           (delete 'validate-runpath)
           (add-after 'build 'build-manpage
             (lambda _
               (invoke "make" "doc"))))))
    (native-inputs (list perl))
    (properties `((tunable? . #t)))))

(define-public ncdu-2
  (deprecated-package "ncdu2" ncdu))
