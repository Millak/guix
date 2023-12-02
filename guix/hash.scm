;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 Sarah Morgensen <iskarian@mgsn.dev>
;;; Copyright © 2022 Maxime Devos <maximedevos@telenet.be>
;;; Copyright © 2023 Simon Tournier <zimon.toutoune@gmail.com>
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

(define-module (guix hash)
  #:use-module (gcrypt hash)
  #:use-module (guix serialization)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:export (vcs-file?
            vcs-file-predicate
            file-hash*))

(define %vcs-directories
  ;; Directory used for determining the kind of VCS.
  (list ".bzr" ".git" ".hg" ".svn" "CVS"))

(define* (vcs-file? file stat
                    #:optional
                    (vcs-directories %vcs-directories))
  "Return true if FILE matches a version control system from the list
VCSES-DIRECTORIES."
  (case (stat:type stat)
    ((directory)
     (member (basename file) vcs-directories))
    ((regular)
     (if (member ".git" vcs-directories)
         ;; Git sub-modules have a '.git' file that is a regular text file.
         (string=? (basename file) ".git")
         #f))
    (else
     #f)))

(define (vcs-file-predicate directory)
  "Return a two-argument procedure that returns true when version-control
metadata directories such as '.git' is found in DIRECTORY."
  (define vcs-directories
    (filter (lambda (vcs)
              (file-exists? (in-vicinity directory vcs)))
            %vcs-directories))

  (lambda (file stat)
    (vcs-file? file stat vcs-directories)))

(define* (file-hash* file #:key
                     (algorithm (hash-algorithm sha256))
                     (recursive? 'auto)
                     (select? (negate (lambda (file stat)
                                        (vcs-file? file stat)))))
  "Compute the hash of FILE with ALGORITHM.

Symbolic links are only dereferenced if RECURSIVE? is false.
Directories are only supported if RECURSIVE? is #true or 'auto'.
The executable bit is only recorded if RECURSIVE? is #true.
If FILE is a symbolic link, it is only followed if RECURSIVE? is false.

For regular files, there are two different hashes when the executable
hash isn't recorded: the regular hash and the nar hash. In most situations,
the regular hash is desired and setting RECURSIVE? to 'auto' does the right
thing for both regular files and directories.

This procedure must only be used under controlled circumstances;
the detection of symbolic links in FILE is racy.

When FILE is a directory, the procedure SELECT? called as (SELECT? FILE STAT)
decides which files to include. By default, version control files are
excluded. To include everything, SELECT? can be set to (const #true)."
  (if (or (eq? recursive? #true)
          (and (eq? recursive? 'auto)
               ;; Don't change this to (eq? 'directory ...), because otherwise
               ;; if 'file' denotes a symbolic link, the 'file-hash' below
               ;; would dereference it -- dereferencing symbolic links would
               ;; open an avoidable can of potential worms.
               (not (eq? 'regular (stat:type (lstat file))))))
      (let-values (((port get-hash)
                    (open-hash-port algorithm)))
        (write-file file port #:select? select?)
        (force-output port)
        (get-hash))
      (file-hash algorithm file)))
