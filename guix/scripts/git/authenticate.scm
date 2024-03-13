;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020, 2024 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix scripts git authenticate)
  #:use-module (git)
  #:use-module (guix ui)
  #:use-module (guix scripts)
  #:use-module (guix git-authenticate)
  #:autoload   (guix openpgp) (openpgp-format-fingerprint
                               openpgp-public-key-fingerprint)
  #:use-module ((guix channels) #:select (openpgp-fingerprint))
  #:use-module ((guix git) #:select (with-git-error-handling))
  #:use-module (guix progress)
  #:use-module (guix base64)
  #:autoload   (rnrs bytevectors) (bytevector-length)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-37)
  #:use-module (srfi srfi-71)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:export (guix-git-authenticate))

;;; Commentary:
;;;
;;; Authenticate a Git checkout by reading '.guix-authorizations' files and
;;; following the "authorizations invariant" also used by (guix channels).
;;;
;;; Code:

(define %options
  ;; Specifications of the command-line options.
  (list (option '(#\h "help") #f #f
                (lambda args
                  (show-help)
                  (exit 0)))
        (option '(#\V "version") #f #f
                (lambda args
                  (show-version-and-exit "guix git authenticate")))

        (option '(#\r "repository") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'directory arg result)))
        (option '(#\e "end") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'end-commit (string->oid arg) result)))
        (option '(#\k "keyring") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'keyring-reference arg result)))
        (option '("cache-key") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'cache-key arg result)))
        (option '("historical-authorizations") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'historical-authorizations arg
                              result)))
        (option '("stats") #f #f
                (lambda (opt name arg result)
                  (alist-cons 'show-stats? #t result)))))

(define %default-options
  '())

(define (current-branch repository)
  "Return the name of the checked out branch of REPOSITORY or #f if it could
not be determined."
  (and (not (repository-head-detached? repository))
       (let* ((head (repository-head repository))
              (name (reference-name head)))
         (and (string-prefix? "refs/heads/" name)
              (string-drop name (string-length "refs/heads/"))))))

(define (config-value repository key)
  "Return the config value associated with KEY in the 'guix.authentication' or
'guix.authentication-BRANCH' name space in REPOSITORY, or #f if no such config
was found."
  (let-syntax ((false-if-git-error
                (syntax-rules ()
                  ((_ exp)
                   (catch 'git-error (lambda () exp) (const #f))))))
    (let* ((config (repository-config repository))
           (branch (current-branch repository)))
      ;; First try the BRANCH-specific value, then the generic one.`
      (or (and branch
               (false-if-git-error
                (config-entry-value
                 (config-get-entry config
                                   (string-append "guix.authentication-"
                                                  branch "." key)))))
          (false-if-git-error
           (config-entry-value
            (config-get-entry config
                              (string-append "guix.authentication."
                                             key))))))))

(define (configured-introduction repository)
  "Return two values: the commit and signer fingerprint (strings) as
configured in REPOSITORY.  Error out if one or both were missing."
  (let* ((commit (config-value repository "introduction-commit"))
         (signer (config-value repository "introduction-signer")))
    (unless (and commit signer)
      (leave (G_ "unknown introductory commit and signer~%")))
    (values commit signer)))

(define (configured-keyring-reference repository)
  "Return the keyring reference configured in REPOSITORY or #f if missing."
  (config-value repository "keyring"))

(define (configured? repository)
  "Return true if REPOSITORY already container introduction info in its
'config' file."
  (and (config-value repository "introduction-commit")
       (config-value repository "introduction-signer")))

(define* (record-configuration repository
                               #:key commit signer keyring-reference)
  "Record COMMIT, SIGNER, and KEYRING-REFERENCE in the 'config' file of
REPOSITORY."
  (define config
    (repository-config repository))

  ;; Guile-Git < 0.7.0 lacks 'set-config-string'.
  (if (module-defined? (resolve-interface '(git)) 'set-config-string)
      (begin
        (set-config-string config "guix.authentication.introduction-commit"
                           commit)
        (set-config-string config "guix.authentication.introduction-signer"
                           signer)
        (set-config-string config "guix.authentication.keyring"
                           keyring-reference)
        (info (G_ "introduction and keyring recorded \
in repository configuration file~%")))
      (warning (G_ "could not record introduction and keyring configuration\
 (Guile-Git too old?)~%"))))

(define (install-hooks repository)
  "Attempt to install in REPOSITORY hooks that invoke 'guix git authenticate'.
Bail out if one of these already exists."
  ;; Guile-Git < 0.7.0 lacks 'repository-common-directory'.
  (if (module-defined? (resolve-interface '(git))
                       'repository-common-directory)
      (let ()
        (define directory
          (repository-common-directory repository))

        (define pre-push-hook
          (in-vicinity directory "hooks/pre-push"))

        (define post-merge-hook
          (in-vicinity directory "hooks/post-merge"))

        (if (or (file-exists? pre-push-hook)
                (file-exists? post-merge-hook))
            (begin
              (warning (G_ "not overriding pre-existing hooks '~a' and '~a'~%")
                       pre-push-hook post-merge-hook)
              (display-hint (G_ "Consider running @command{guix git authenticate}
from your pre-push and post-merge hooks so your repository is automatically
authenticated before you push and when you pull updates.")))
            (begin
              (call-with-output-file pre-push-hook
                (lambda (port)
                  (format port "#!/bin/sh
# Installed by 'guix git authenticate'.
set -e
while read local_ref local_oid remote_ref remote_oid
do
  guix git authenticate --end=\"$local_oid\"
done\n")
                  (chmod port #o755)))
              (call-with-output-file post-merge-hook
                (lambda (port)
                  (format port "#!/bin/sh
# Installed by 'guix git authenticate'.
exec guix git authenticate\n")
                  (chmod port #o755)))
              (info (G_ "installed hooks '~a' and '~a'~%")
                    pre-push-hook post-merge-hook))))
      (warning (G_ "cannot determine where to install hooks\
 (Guile-Git too old?)~%"))))

(define (show-stats stats)
  "Display STATS, an alist containing commit signing stats as returned by
'authenticate-repository'."
  (format #t (G_ "Signing statistics:~%"))
  (for-each (match-lambda
              ((signer . count)
               (format #t "  ~a ~10d~%"
                       (openpgp-format-fingerprint
                        (openpgp-public-key-fingerprint signer))
                       count)))
            (sort stats
                  (match-lambda*
                    (((_ . count1) (_ . count2))
                     (> count1 count2))))))

(define (show-help)
  (display (G_ "Usage: guix git authenticate COMMIT SIGNER [OPTIONS...]
Authenticate the given Git checkout using COMMIT/SIGNER as its introduction.\n"))
  (display (G_ "
  -r, --repository=DIRECTORY
                         open the Git repository at DIRECTORY"))
  (display (G_ "
  -k, --keyring=REFERENCE
                         load keyring from REFERENCE, a Git branch"))
  (display (G_ "
      --end=COMMIT       authenticate revisions up to COMMIT"))
  (display (G_ "
      --stats            display commit signing statistics upon completion"))
  (display (G_ "
      --cache-key=KEY    cache authenticated commits under KEY"))
  (display (G_ "
      --historical-authorizations=FILE
                         read historical authorizations from FILE"))
  (newline)
  (display (G_ "
  -h, --help             display this help and exit"))
  (display (G_ "
  -V, --version          display version information and exit"))
  (newline)
  (show-bug-report-information))


;;;
;;; Entry point.
;;;

(define (guix-git-authenticate . args)
  (define options
    (parse-command-line args %options (list %default-options)
                        #:build-options? #f))

  (define (command-line-arguments lst)
    (reverse (filter-map (match-lambda
                           (('argument . arg) arg)
                           (_ #f))
                         lst)))

  (define commit-short-id
    (compose (cut string-take <> 7) oid->string commit-id))

  (define (openpgp-fingerprint* str)
    (unless (string-every (char-set-union char-set:hex-digit
                                          char-set:whitespace)
                          str)
      (leave (G_ "~a: invalid OpenPGP fingerprint~%") str))
    (let ((fingerprint (openpgp-fingerprint str)))
      (unless (= 20 (bytevector-length fingerprint))
        (leave (G_ "~a: wrong length for OpenPGP fingerprint~%") str))
      fingerprint))

  (define (make-reporter start-commit end-commit commits)
    (format (current-error-port)
            (G_ "Authenticating commits ~a to ~a (~h new \
commits)...~%")
            (commit-short-id start-commit)
            (commit-short-id end-commit)
            (length commits))

    (if (isatty? (current-error-port))
        (progress-reporter/bar (length commits))
        progress-reporter/silent))

  (define (missing-arguments)
    (leave (G_ "wrong number of arguments; \
expected COMMIT and SIGNER~%")))

  (with-error-handling
    (with-git-error-handling
     (let* ((show-stats? (assoc-ref options 'show-stats?))
            (repository  (repository-open (or (assoc-ref options 'directory)
                                              (repository-discover "."))))
            (commit signer (match (command-line-arguments options)
                             ((commit signer)
                              (values commit signer))
                             (()
                              (configured-introduction repository))
                             (_
                              (missing-arguments))))
            (keyring     (or (assoc-ref options 'keyring-reference)
                             (configured-keyring-reference repository)
                             "keyring"))
            (end         (match (assoc-ref options 'end-commit)
                           (#f  (reference-target
                                 (repository-head repository)))
                           (oid oid)))
            (history     (match (assoc-ref options 'historical-authorizations)
                           (#f '())
                           (file (call-with-input-file file
                                   read-authorizations))))
            (cache-key   (or (assoc-ref options 'cache-key)
                             (repository-cache-key repository))))
       (define stats
         (authenticate-repository repository (string->oid commit)
                                  (openpgp-fingerprint* signer)
                                  #:end end
                                  #:keyring-reference keyring
                                  #:historical-authorizations history
                                  #:cache-key cache-key
                                  #:make-reporter make-reporter))

       (unless (configured? repository)
         (record-configuration repository
                               #:commit commit #:signer signer
                               #:keyring-reference keyring)
         (install-hooks repository))

       (when (and show-stats? (not (null? stats)))
         (show-stats stats))

       (info (G_ "successfully authenticated commit ~a~%")
             (oid->string end))))))
