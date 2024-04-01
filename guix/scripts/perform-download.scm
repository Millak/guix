;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016-2018, 2020, 2023-2024 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix scripts perform-download)
  #:use-module (guix ui)
  #:use-module (guix scripts)
  #:use-module (guix derivations)
  #:use-module ((guix store) #:select (derivation-path? store-path?))
  #:autoload   (guix build download) (%download-methods url-fetch)
  #:autoload   (guix build git) (git-fetch-with-fallback)
  #:autoload   (guix config) (%git)
  #:use-module (ice-9 match)
  #:export (guix-perform-download))

;; This program is a helper for the daemon's 'download' built-in builder.

(define-syntax derivation-let
  (syntax-rules ()
    ((_ drv ((id name) rest ...) body ...)
     (let ((id (assoc-ref (derivation-builder-environment-vars drv)
                          name)))
       (derivation-let drv (rest ...) body ...)))
    ((_ drv () body ...)
     (begin body ...))))

(define %user-module
  ;; Module in which content-address mirror procedures are evaluated.
  (let ((module (make-fresh-user-module)))
    (module-use! module (resolve-interface '(guix base32)))
    module))

(define* (perform-download drv output
                           #:key print-build-trace?)
  "Perform the download described by DRV, a fixed-output derivation, to
OUTPUT.

Note: OUTPUT may differ from the 'out' value of DRV, notably for 'bmCheck' or
'bmRepair' builds."
  (derivation-let drv ((url "url")
                       (executable "executable")
                       (mirrors "mirrors")
                       (content-addressed-mirrors "content-addressed-mirrors")
                       (disarchive-mirrors "disarchive-mirrors")
                       (download-methods "download-methods"))
    (unless url
      (leave (G_ "~a: missing URL~%") (derivation-file-name drv)))

    (let* ((url        (call-with-input-string url read))
           (drv-output (assoc-ref (derivation-outputs drv) "out"))
           (algo       (derivation-output-hash-algo drv-output))
           (hash       (derivation-output-hash drv-output)))
      ;; We're invoked by the daemon, which gives us write access to OUTPUT.
      (when (parameterize ((%download-methods
                            (and download-methods
                                 (call-with-input-string download-methods
                                   read))))
              (url-fetch url output
                         #:print-build-trace? print-build-trace?
                         #:mirrors (if mirrors
                                       (call-with-input-file mirrors read)
                                       '())
                         #:content-addressed-mirrors
                         (if content-addressed-mirrors
                             (call-with-input-file content-addressed-mirrors
                               (lambda (port)
                                 (eval (read port) %user-module)))
                             '())
                         #:disarchive-mirrors
                         (if disarchive-mirrors
                             (call-with-input-file disarchive-mirrors read)
                             '())
                         #:hashes `((,algo . ,hash))

                         ;; Since DRV's output hash is known, X.509 certificate
                         ;; validation is pointless.
                         #:verify-certificate? #f))
        (when (and executable (string=? executable "1"))
          (chmod output #o755))))))

(define* (perform-git-download drv output
                               #:key print-build-trace?)
  "Perform the download described by DRV, a fixed-output derivation, to
OUTPUT.

Note: OUTPUT may differ from the 'out' value of DRV, notably for 'bmCheck' or
'bmRepair' builds."
  (derivation-let drv ((url "url")
                       (commit "commit")
                       (recursive? "recursive?")
                       (download-methods "download-methods"))
    (unless url
      (leave (G_ "~a: missing Git URL~%") (derivation-file-name drv)))
    (unless commit
      (leave (G_ "~a: missing Git commit~%") (derivation-file-name drv)))

    (let* ((url        (call-with-input-string url read))
           (recursive? (and recursive?
                            (call-with-input-string recursive? read)))
           (drv-output (assoc-ref (derivation-outputs drv) "out"))
           (algo       (derivation-output-hash-algo drv-output))
           (hash       (derivation-output-hash drv-output)))
      ;; Commands such as 'git submodule' expect Coreutils and sed (among
      ;; others) to be in $PATH.  The 'git' package in Guix should address it
      ;; with wrappers but packages on other distros such as Debian may rely
      ;; on ambient authority, hence the PATH value below.
      (setenv "PATH" "/run/current-system/profile/bin:/bin:/usr/bin")

      (parameterize ((%download-methods
                      (and download-methods
                           (call-with-input-string download-methods
                             read))))
        ;; Note: When doing a '--check' build, DRV-OUTPUT and OUTPUT are
        ;; different, hence the #:item argument below.
        (git-fetch-with-fallback url commit output
                                 #:hash hash
                                 #:hash-algorithm algo
                                 #:recursive? recursive?
                                 #:item (derivation-output-path drv-output)
                                 #:git-command %git)))))

(define (assert-low-privileges)
  (when (zero? (getuid))
    (leave (G_ "refusing to run with elevated privileges (UID ~a)~%")
           (getuid))))

(define-command (guix-perform-download . args)
  (category internal)
  (synopsis "perform download described by fixed-output derivations")

  ;; This is an "out-of-band" download in that this code is executed directly
  ;; by the daemon and not explicitly described as an input of the derivation.
  ;; This allows us to sidestep bootstrapping problems, such as downloading
  ;; the source code of GnuTLS over HTTPS before we have built GnuTLS.  See
  ;; <https://bugs.gnu.org/22774>.

  (define print-build-trace?
    (match (getenv "_NIX_OPTIONS")
      (#f #f)
      (str (string-contains str "print-extended-build-trace=1"))))

  ;; This program must be invoked by guix-daemon under an unprivileged UID to
  ;; prevent things downloading from 'file:///etc/shadow' or arbitrary code
  ;; execution via the content-addressed mirror procedures.  (That means we
  ;; exclude users who did not pass '--build-users-group'.)
  (with-error-handling
    (match args
      (((? derivation-path? drv) (? store-path? output))
       (assert-low-privileges)
       (let* ((drv (read-derivation-from-file drv))
              (download (match (derivation-builder drv)
                          ("builtin:download" perform-download)
                          ("builtin:git-download" perform-git-download)
                          (unknown (leave (G_ "~a: unknown builtin builder")
                                          unknown))))
              (drv-output (assoc-ref (derivation-outputs drv) "out"))
              (algo       (derivation-output-hash-algo drv-output))
              (hash       (derivation-output-hash drv-output)))
         (unless (and hash algo)
           (leave (G_ "~a is not a fixed-output derivation~%")
                  (derivation-file-name drv)))

         (download drv output #:print-build-trace? print-build-trace?)))
      (("--version")
       (show-version-and-exit))
      (x
       (leave
        (G_ "fixed-output derivation and output file name expected~%"))))))

;; Local Variables:
;; eval: (put 'derivation-let 'scheme-indent-function 2)
;; End:

;; perform-download.scm ends here
