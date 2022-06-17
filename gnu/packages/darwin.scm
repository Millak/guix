;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2022 Philip McGrath <philip@philipmcgrath.com>
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

(define-module (gnu packages darwin)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages gnustep)
  #:use-module (gnu packages llvm)
  #:use-module (guix build-system gnu)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:))

(define-public cctools
  (let ((cctools-version "973.0.1")
        (ld64-version "609")
        (revision "0")
        (commit "04663295d0425abfac90a42440a7ec02d7155fea"))
    (package
      (name "cctools")
      (version (git-version (string-append cctools-version
                                           "-ld64-"
                                           ld64-version)
                            revision
                            commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/tpoechtrager/cctools-port")
               (commit commit)))
         (sha256
          (base32 "0vihfa8y64vvd3pxy8qh4mhcnzinxh9flpz9dvw4wch4zj2nnfjs"))
         (file-name (git-file-name name version))
         (snippet
          #~(begin
              (use-modules (guix build utils))
              (with-directory-excursion "cctools"
                ;; use system libobjc2
                (substitute* "configure.ac"
                  (("AC_CONFIG_FILES[(]\\[libobjc2/Makefile][)]")
                   ""))
                (substitute* "Makefile.am"
                  (("SUBDIRS=libobjc2 ")
                   "SUBDIRS="))
                (substitute* "otool/Makefile.am"
                  (("\\$[(]top_builddir[)]/libobjc2/libobjc\\.la")
                   "-lobjc")
                  (("-I\\$[(]top_srcdir[)]/libobjc2")
                   ""))
                ;; delete files
                (for-each (lambda (pth)
                            (when (file-exists? pth)
                              (delete-file-recursively pth)))
                          `("include/gnu/symseg.h" ;; obsolete
                            "libobjc2" ;; unbundle
                            ;; generated files:
                            "compile"
                            "config.guess"
                            "config.sub"
                            "configure"
                            "install-sh"
                            "ltmain.sh"
                            "missing"
                            ,@(find-files "." "^Makefile\\.in$"))))))))
      (inputs (list libobjc2
                    clang-toolchain))
      (native-inputs (list libtool
                           autoconf
                           automake
                           clang-toolchain))
      (build-system gnu-build-system)
      (arguments
       (list
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'chdir
              (lambda args
                (chdir "cctools"))))))
      (home-page "https://github.com/tpoechtrager/cctools-port")
      (synopsis "Darwin's @code{cctools} and @code{ld64}")
      ;; Confusingly enough, the program is called ld64, but the command is
      ;; just ld (with no symlink), so @command{ld64} would be wrong.
      (description
       "Darwin's @code{cctools} are a set of tools somewhat similar in purpose
to GNU Binutils, but for Mach-O files targeting Darwin.  The suite includes
@command{install_name_tool}, @command{dyldinfo}, and other specialized tools
in addition to standard utilities like @command{ld} and @command{as}.  This
package provides portable versions of the tools.")
      (license license:apsl2))))
