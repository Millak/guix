;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2017 Ludovic Courtès <ludo@gnu.org>
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
  #:use-module (guix derivations)
  #:use-module ((guix store) #:select (derivation-path? store-path?))
  #:use-module (guix build download)
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

(define* (perform-download drv #:optional output)
  "Perform the download described by DRV, a fixed-output derivation, to
OUTPUT.

Note: Unless OUTPUT is #f, we don't read the value of 'out' in DRV since the
actual output is different from that when we're doing a 'bmCheck' or
'bmRepair' build."
  (derivation-let drv ((url "url")
                       (output* "out")
                       (executable "executable")
                       (mirrors "mirrors")
                       (content-addressed-mirrors "content-addressed-mirrors"))
    (unless url
      (leave (G_ "~a: missing URL~%") (derivation-file-name drv)))

    (let* ((output     (or output output*))
           (url        (call-with-input-string url read))
           (drv-output (assoc-ref (derivation-outputs drv) "out"))
           (algo       (derivation-output-hash-algo drv-output))
           (hash       (derivation-output-hash drv-output)))
      (unless (and algo hash)
        (leave (G_ "~a is not a fixed-output derivation~%")
               (derivation-file-name drv)))

      ;; We're invoked by the daemon, which gives us write access to OUTPUT.
      (when (url-fetch url output
                       #:mirrors (if mirrors
                                     (call-with-input-file mirrors read)
                                     '())
                       #:content-addressed-mirrors
                       (if content-addressed-mirrors
                           (call-with-input-file content-addressed-mirrors
                             (lambda (port)
                               (eval (read port) %user-module)))
                           '())
                       #:hashes `((,algo . ,hash))

                       ;; Since DRV's output hash is known, X.509 certificate
                       ;; validation is pointless.
                       #:verify-certificate? #f)
        (when (and executable (string=? executable "1"))
          (chmod output #o755))))))

(define (assert-low-privileges)
  (when (zero? (getuid))
    (leave (G_ "refusing to run with elevated privileges (UID ~a)~%")
           (getuid))))

(define (guix-perform-download . args)
  "Perform the download described by the given fixed-output derivation.

This is an \"out-of-band\" download in that this code is executed directly by
the daemon and not explicitly described as an input of the derivation.  This
allows us to sidestep bootstrapping problems, such downloading the source code
of GnuTLS over HTTPS, before we have built GnuTLS.  See
<http://bugs.gnu.org/22774>."

  ;; This program must be invoked by guix-daemon under an unprivileged UID to
  ;; prevent things downloading from 'file:///etc/shadow' or arbitrary code
  ;; execution via the content-addressed mirror procedures.  (That means we
  ;; exclude users who did not pass '--build-users-group'.)
  (with-error-handling
    (match args
      (((? derivation-path? drv) (? store-path? output))
       (assert-low-privileges)
       (perform-download (call-with-input-file drv read-derivation)
                         output))
      (((? derivation-path? drv))                 ;backward compatibility
       (assert-low-privileges)
       (perform-download (call-with-input-file drv read-derivation)))
      (("--version")
       (show-version-and-exit))
      (x
       (leave
        (G_ "fixed-output derivation and output file name expected~%"))))))

;; Local Variables:
;; eval: (put 'derivation-let 'scheme-indent-function 2)
;; End:

;; perform-download.scm ends here
