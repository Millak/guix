;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2023, 2024 Rodion Goritskov <rodion.goritskov@gmail.com>
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

(define-module (gnu packages clifm)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages acl)
  #:use-module (gnu packages file)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages readline))

(define-public clifm
  (package
    (name "clifm")
    (version "1.26")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/leo-arch/clifm")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0mdh24qjx4cghghrl6sbp49a6kv8birxa5vxrsmj8ddnya2j1n3q"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags (list (string-append "CC="
                                         ,(cc-for-target))
                          (string-append "PREFIX="
                                         (assoc-ref %outputs "out")))
       #:phases (modify-phases %standard-phases
                  (delete 'configure)
                  (delete 'build)
                  (delete 'check)
                  (add-after 'unpack 'fix-bash-completion-directory
                    (lambda _
                      (substitute* "Makefile"
                        (("\\$\\(DATADIR\\)/bash-completion/completions")
                         "$(PREFIX)/etc/bash_completion.d")))))))
    (inputs (list acl file libcap readline))
    (home-page "https://github.com/leo-arch/clifm")
    (synopsis "Command-line file manager")
    (description
     "Clifm is a shell-like, text-based terminal file manager
that sits on the command line.

It is built with command line principles in mind: instead of navigating
through a big menu of files, it lets you type, exactly as you do in your
regular shell, but easier and faster.")
    (license license:gpl2+)))
