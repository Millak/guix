;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2022 B. Wilson <elaexuotee@wilsonb.com>
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

(define-module (gnu packages apple)
  #:use-module (gnu packages bison)
  #:use-module (guix build-system gnu)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:))

(define-public apple-bash
  (package
    (name "apple-bash")
    (version "125")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/apple-oss-distributions/bash.git")
               (commit (string-append "bash-" version))))
        (sha256
          (base32 "1d4s1z57yiyhgb6i22kb9al31j1fv9a4rp1bb747ny6grdvc4919"))
        (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (native-inputs (list bison))
    (arguments
      '(#:phases
        (modify-phases %standard-phases
          (add-before 'configure 'pre-configure
            (lambda _
              ;; An XCode project wraps the actual bash source.
              (chdir "bash-3.2")
              ;; The xlocale.h header was removed in glibc 2.26.
              (substitute* "lib/glob/smatch.c"
                (("xlocale.h") "locale.h"))
              ;; EBADEXEC is XNU-specific.
              (substitute* "execute_cmd.c"
                (("EBADEXEC") "ENOEXEC")))))))
    (home-page "https://github.com/apple-oss-distributions/bash/")
    (synopsis "Bash used on Apple macOS systems")
    (description
     "This is the version of Bash, forked from GNU Bash 3.2, released on Apple
macOS systems.")
    (license license:gpl2+)))
