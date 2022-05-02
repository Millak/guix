;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2014, 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2017, 2019 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2022 Paul A. Patience <paul@apatience.com>
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

(define-module (gnu packages apl)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix svn-download)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages sqlite))

(define-public apl
  (let ((revision 1550))
    (package
      (name "apl")
      (version (string-append "1.8-r" (number->string revision)))
      (source
       (origin
         (method svn-fetch)
         (uri (svn-reference
               (url "svn://svn.savannah.gnu.org/apl/trunk")
               (revision revision)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1bgc3a09f35zrqq2irhm1hspppnxjqas0fmcw14hkc7910br9ip3"))))
      (build-system gnu-build-system)
      (home-page "https://www.gnu.org/software/apl/")
      (inputs
       (list gettext-minimal
             lapack
             pcre2
             readline
             sqlite))
      (arguments
       `(#:configure-flags (list (string-append
                                  "--with-sqlite3="
                                  (assoc-ref %build-inputs "sqlite")))
         #:phases
         (modify-phases %standard-phases
           (add-before 'configure 'fix-configure
             (lambda _
               (substitute* "buildtag.sh"
                 ;; Don't exit on failed SVN-related calls.
                 (("^ +return 0\n") "")
                 ;; Manually set the SVN revision, since the directory is
                 ;; unversioned and we know it anyway.
                 (("^SVNINFO=.*")
                  ,(string-append "SVNINFO=" (number->string revision) "\n"))
                 ;; Requires running ‘svn info’ on a versioned directory.
                 (("\\\\\"\\$ARCHIVE_SVNINFO\\\\\"") "\\\"\\\"")))))))
      (synopsis "APL interpreter")
      (description
       "GNU APL is a free interpreter for the programming language APL.  It is
an implementation of the ISO standard 13751.")
      (license license:gpl3+))))
