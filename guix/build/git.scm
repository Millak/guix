;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2016, 2019, 2023-2024 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2023 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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

(define-module (guix build git)
  #:use-module (guix build utils)
  #:use-module ((guix build download)
                #:select (download-method-enabled?))
  #:autoload   (guix build download-nar) (download-nar)
  #:autoload   (guix swh) (%verify-swh-certificate?
                           swh-download
                           swh-download-directory-by-nar-hash)
  #:use-module (srfi srfi-34)
  #:use-module (ice-9 format)
  #:export (git-fetch
            git-fetch-with-fallback))

;;; Commentary:
;;;
;;; This is the build-side support code of (guix git-download).  It allows a
;;; Git repository to be cloned and checked out at a specific commit.
;;;
;;; Code:

(define* (git-fetch url commit directory
                    #:key (git-command "git")
                    lfs? recursive?)
  "Fetch COMMIT from URL into DIRECTORY.  COMMIT must be a valid Git commit
identifier.  When LFS? is true, configure Git to also fetch Large File
Storage (LFS) files; it assumes that the @code{git-lfs} extension is available
in the environment.  When RECURSIVE? is true, all the sub-modules of URL are
fetched, recursively.  Return #t on success, #f otherwise."

  ;; Disable TLS certificate verification.  The hash of the checkout is known
  ;; in advance anyway.
  (setenv "GIT_SSL_NO_VERIFY" "true")

  (mkdir-p directory)

  (guard (c ((invoke-error? c)
             (format (current-error-port)
                     "git-fetch: '~a~{ ~a~}' failed with exit code ~a~%"
                     (invoke-error-program c)
                     (invoke-error-arguments c)
                     (or (invoke-error-exit-status c) ;XXX: not quite accurate
                         (invoke-error-stop-signal c)
                         (invoke-error-term-signal c)))
             (delete-file-recursively directory)
             #f))
    (with-directory-excursion directory
      (invoke git-command "init" "--initial-branch=main")
      (invoke git-command "remote" "add" "origin" url)

      (when lfs?
        (setenv "HOME" "/tmp")
        (invoke git-command "lfs" "install"))

      (if (zero? (system* git-command "fetch" "--depth" "1" "origin" commit))
          (invoke git-command "checkout" "FETCH_HEAD")
          (begin
            (setvbuf (current-output-port) 'line)
            (format #t "Failed to do a shallow fetch; retrying a full fetch...~%")
            (invoke git-command "fetch" "origin")
            (invoke git-command "checkout" commit)))
      (when recursive?
        ;; Now is the time to fetch sub-modules.
        (invoke git-command "submodule" "update" "--init" "--recursive")

        ;; In sub-modules, '.git' is a flat file, not a directory,
        ;; so we can use 'find-files' here.
        (for-each delete-file-recursively
                  (find-files directory "^\\.git$")))

      ;; The contents of '.git' vary as a function of the current
      ;; status of the Git repo.  Since we want a fixed output, this
      ;; directory needs to be taken out.
      (delete-file-recursively ".git")
      #t)))


(define* (git-fetch-with-fallback url commit directory
                                  #:key (item directory)
                                  (git-command "git")
                                  hash hash-algorithm
                                  lfs? recursive?)
  "Like 'git-fetch', fetch COMMIT from URL into DIRECTORY, but fall back to
alternative methods when fetching from URL fails: attempt to download a nar
for ITEM, and if that also fails, download from the Software Heritage archive.
When HASH and HASH-ALGORITHM are provided, they are interpreted as the nar
hash of the directory of interested and are used as its content address at
SWH."
  (or (and (download-method-enabled? 'upstream)
           (git-fetch url commit directory
                      #:lfs? lfs?
                      #:recursive? recursive?
                      #:git-command git-command))
      (and (download-method-enabled? 'nar)
           (download-nar item directory))

      ;; As a last resort, attempt to download from Software Heritage.
      ;; Disable X.509 certificate verification to avoid depending
      ;; on nss-certs--we're authenticating the checkout anyway.
      ;; XXX: Currently recursive checkouts are not supported.
      (and (not recursive?)
           (download-method-enabled? 'swh)
           (parameterize ((%verify-swh-certificate? #f))
             (format (current-error-port)
                     "Trying to download from Software Heritage...~%")

             ;; First try to look up and download the directory corresponding
             ;; to HASH: this is fundamentally more reliable than looking up
             ;; COMMIT, especially when COMMIT denotes a tag.
             (or (and hash hash-algorithm
                      (swh-download-directory-by-nar-hash hash hash-algorithm
                                                          directory))
                 (swh-download url commit directory))

             (when (file-exists?
                    (string-append directory "/.gitattributes"))
               ;; Perform CR/LF conversion and other changes
               ;; specificied by '.gitattributes'.
               (invoke git-command "-C" directory "init")
               (invoke git-command "-C" directory "config" "--local"
                       "user.email" "you@example.org")
               (invoke git-command "-C" directory "config" "--local"
                       "user.name" "Your Name")
               (invoke git-command "-C" directory "add" ".")
               (invoke git-command "-C" directory "commit" "-am" "init")
               (invoke git-command "-C" directory "read-tree" "--empty")
               (invoke git-command "-C" directory "reset" "--hard")
               (delete-file-recursively
                (string-append directory "/.git")))))))

;;; git.scm ends here
