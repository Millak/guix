;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 José Miguel Sánchez García <jmi2k@openmailbox.com>
;;; Copyright © 2019 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (gnu packages simh)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix utils)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages compression))

(define-public simh
  (package
    (name "simh")
    (version "3.12-4")
    (source
     (origin
       (method url-fetch)
       (uri (list (string-append
                   "http://simh.trailing-edge.com/sources/simhv"
                   (string-delete #\. version) ".zip")
                  (string-append
                   "http://simh.trailing-edge.com/sources/archive/simhv"
                   (string-delete #\. version) ".zip")))
       (sha256
        (base32 "1i78c1x8xjiwy9dd2ss0mk3f1v9pmcjb4zc37ikqnjarsfqj2nm0"))))
    (build-system gnu-build-system)
    (native-inputs
     (list unzip))
    (inputs
     (list libpcap))
    (arguments
     (list #:tests? #f
           #:make-flags
           #~(list (string-append "GCC=" #$(cc-for-target) " -fcommon"))
           #:modules `((ice-9 string-fun)
                       ,@%gnu-build-system-modules)
           #:phases
           #~(modify-phases %standard-phases
               (delete 'configure)
               (add-before 'build 'prepare-build
                 (lambda _
                   (substitute* "makefile"
                     (("LIBPATH:=/usr/lib")
                      (string-append "LIBPATH:="
                                     (string-replace-substring
                                      (getenv "LIBRARY_PATH") ":" " ")))
                     (("export LIBRARY_PATH = .*") ""))
                   (mkdir "BIN")))
               (replace 'install
                 (lambda* (#:key outputs #:allow-other-keys)
                   (let* ((out (assoc-ref outputs "out"))
                          (bin (string-append out "/bin/"))
                          (lib (string-append out "/lib/simh/")))
                     (mkdir-p bin)
                     (mkdir-p lib)
                     (for-each (lambda (file)
                                 (copy-file file
                                            (string-append bin "simh-"
                                                           (basename file))))
                               (find-files "BIN"))
                     (for-each (lambda (file)
                                 (copy-file file
                                            (string-append lib
                                                           (basename file))))
                               (find-files "VAX" "bin$"))))))))
    (home-page "http://simh.trailing-edge.com")
    (synopsis "Collection of simulators from The Computer History Simulation
Project")
    (description
     "SIMH is a highly portable, multi-system simulator.  SIMH implements
simulators for:

@itemize
@item Data General Nova, Eclipse.
@item Digital Equipment Corporation PDP-1, PDP-4, PDP-7, PDP-8, PDP-9, PDP-10,
PDP-11, PDP-15, VAX.
@item GRI Corporation GRI-909, GRI-99.
@item IBM 1401, 1620, 1130, 7090/7094, System 3.
@item Interdata (Perkin-Elmer) 16b and 32b systems.
@item Hewlett-Packard 2114, 2115, 2116, 2100, 21MX, 1000.
@item Honeywell H316/H516.
@item MITS Altair 8800, with both 8080 and Z80.
@item Royal-Mcbee LGP-30, LGP-21.
@item Scientific Data Systems SDS 940.
@item SWTP 6800.
@end itemize")
    (license license:expat)))
