;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021, 2023 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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

(define-module (guix build pack)
  #:use-module (gnu build install)
  #:use-module (guix build utils)
  #:use-module (guix build store-copy)
  #:use-module ((guix build union) #:select (relative-file-name))
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (tar-base-options
            populate-profile-root
            build-self-contained-tarball))

;;; Commentary:

;;; This module contains build-side common procedures used by the host-side
;;; (guix scripts pack) module, mostly to allow for code reuse.  Due to making
;;; use of the (guix build store-copy) module, it transitively requires the
;;; sqlite and gcrypt extensions to be available.

;;; Code:

(define* (tar-base-options #:key tar compressor)
  "Return the base GNU tar options required to produce deterministic archives
deterministically.  When TAR, a GNU tar command file name, is provided, the
`--sort' option is used only if supported.  When COMPRESSOR, a command such as
'(\"gzip\" \"-9n\"), is provided, the compressor is explicitly specified via
the `-I' option."
  (define (tar-supports-sort? tar)
    (with-error-to-port (%make-void-port "w")
      (lambda ()
        (zero? (system* tar "cf" "/dev/null" "--files-from=/dev/null"
                        "--sort=name")))))

  `(,@(if compressor
          (list "-I" (string-join compressor))
          '())
    ;; The --sort option was added to GNU tar in version 1.28, released
    ;; 2014-07-28.  For testing, we use the bootstrap tar, which is older
    ;; and doesn't support it.
    ,@(if (and=> tar tar-supports-sort?)
          '("--sort=name")
          '())
    ;; Use GNU format so there's no file name length limitation.
    "--format=gnu"
    "--mtime=@1"
    "--owner=root:0"
    "--group=root:0"
    ;; The 'nlink' of the store item files leads tar to store hard links
    ;; instead of actual copies.  However, the 'nlink' count depends on
    ;; deduplication in the store; it's an "implicit input" to the build
    ;; process.  Use '--hard-dereference' to eliminate it.
    "--hard-dereference"
    "--check-links"))

(define (assert-utf8-locale)
  "Verify the current process is using the en_US.utf8 locale."
  (unless (string=? "unset for tests" (getenv "GUIX_LOCPATH"))
    (unless (false-if-exception (setlocale LC_ALL "en_US.utf8"))
      (error "environment not configured for en_US.utf8 locale"))))

(define* (populate-profile-root profile
                                #:key (profile-name "guix-profile")
                                localstatedir?
                                store-database
                                deduplicate?
                                (symlinks '()))
  "Populate the root profile directory with SYMLINKS and a Guix database, when
LOCALSTATEDIR? is set, and a pre-computed STORE-DATABASE is provided.  The
directory is created as \"root\" in the current working directory.  When
DEDUPLICATE? is true, deduplicate the store items, which relies on hard
links.  It needs to run in an environment where "
  (define symlink->directives
    ;; Return "populate directives" to make the given symlink and its
    ;; parent directories.
    (match-lambda
      ((source '-> target)
       (let ((target (string-append profile "/" target))
             (parent (dirname source)))
         ;; Never add a 'directory' directive for "/" so as to
         ;; preserve its ownership when extracting the archive (see
         ;; below), and also because this would lead to adding the
         ;; same entries twice in the tarball.
         `(,@(if (string=? parent "/")
                 '()
                 `((directory ,parent)))
           ;; Use a relative file name for compatibility with
           ;; relocatable packs.
           (,source -> ,(relative-file-name parent target)))))))

  (define directives
    ;; Fully-qualified symlinks.
    (append-map symlink->directives symlinks))

  (define %root "root")

  (when localstatedir?
    (unless store-database
      (error "missing STORE-DATABASE argument")))

  (assert-utf8-locale)

  ;; Note: there is not much to gain here with deduplication and there
  ;; is the overhead of the '.links' directory, so turn it off by
  ;; default.  Furthermore GNU tar < 1.30 sometimes fails to extract
  ;; tarballs with hard links:
  ;; <http://lists.gnu.org/archive/html/bug-tar/2017-11/msg00009.html>.
  (populate-store (list "profile") %root #:deduplicate? deduplicate?)

  (when localstatedir?
    (install-database-and-gc-roots %root store-database
                                   profile #:profile-name profile-name))

  ;; Create SYMLINKS.
  (for-each (cut evaluate-populate-directive <> %root) directives))

(define* (build-self-contained-tarball profile
                                       tarball-file-name
                                       #:key (profile-name "guix-profile")
                                       localstatedir?
                                       store-database
                                       deduplicate?
                                       symlinks
                                       compressor-command)
  "Create a self-contained tarball TARBALL-FILE-NAME from PROFILE, optionally
compressing it with COMPRESSOR-COMMAND, the complete command-line string to
use for the compressor."
  (populate-profile-root profile
                         #:profile-name profile-name
                         #:localstatedir? localstatedir?
                         #:store-database store-database
                         #:deduplicate? deduplicate?
                         #:symlinks symlinks)

  (assert-utf8-locale)

  ;; GNU Tar recurses directories by default.  Simply add the whole root
  ;; directory, which contains all the files to be archived.  This avoids
  ;; creating duplicate files in the archives that would be stored as hard
  ;; links by GNU Tar.
  (apply invoke "tar" "-cvf" tarball-file-name "-C" "root" "."
         (tar-base-options
          #:tar "tar"
          #:compressor compressor-command)))
