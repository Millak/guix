;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Caleb Ristvedt <caleb.ristvedt@cune.org>
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

;;; This houses stuff we do to files when they arrive at the store - resetting
;;; timestamps, deduplicating, etc.

(define-module (guix store deduplication)
  #:use-module (guix hash)
  #:use-module (guix build utils)
  #:use-module (guix base16)
  #:use-module (srfi srfi-11)
  #:use-module (rnrs io ports)
  #:use-module (ice-9 ftw)
  #:use-module (guix serialization)
  #:export (nar-sha256
            deduplicate
            reset-timestamps
            counting-wrapper-port))

;; Would it be better to just make WRITE-FILE give size as well? I question
;; the general utility of this approach.
(define (counting-wrapper-port output-port)
  "Some custom ports don't implement GET-POSITION at all. But if we want to
figure out how many bytes are being written, we will want to use that. So this
makes a wrapper around a port which implements GET-POSITION."
  (let ((byte-count 0))
    (make-custom-binary-output-port "counting-wrapper"
                                    (lambda (bytes offset count)
                                      (set! byte-count
                                        (+ byte-count count))
                                      (put-bytevector output-port bytes
                                                      offset count)
                                      count)
                                    (lambda ()
                                      byte-count)
                                    #f
                                    (lambda ()
                                      (close-port output-port)))))


(define (nar-sha256 file)
  "Gives the sha256 hash of a file and the size of the file in nar form."
  (let-values (((port get-hash) (open-sha256-port)))
    (let ((wrapper (counting-wrapper-port port)))
      (write-file file wrapper)
      (force-output wrapper)
      (force-output port)
      (let ((hash (get-hash))
            (size (port-position wrapper)))
        (close-port wrapper)
        (values hash
                size)))))


; Taken from stdlib.h - this should be the same range as the C++ deduplication
; stuff this way.
(define rand-max 2147483647)

(define (tempname-in directory)
  "Gives an unused temporary name under DIRECTORY. Not guaranteed to still be
unused by the time you create anything with that name, but a good shot."
  (let ((const-part (string-append directory
                                   "/.tmp-link-"
                                   (number->string (getpid)))))
    (let try-again ((guess-part (number->string (random rand-max))))
      (if (file-exists? (string-append const-part "-" guess-part))
          (try-again (number->string (random rand-max)))
          (string-append const-part "-" guess-part)))))

(define* (get-temp-link target #:optional (link-prefix (dirname target)))
  "Like mkstemp!, but instead of creating a new file and giving you the name,
it creates a new hardlink to TARGET and gives you the name. Since
cross-filesystem hardlinks don't work, the temp link must be created on the
same filesystem - where in that filesystem it is can be controlled by
LINK-PREFIX."
  (let try-again ((tempname (tempname-in link-prefix)))
    (catch
      'system-error
      (lambda ()
        (link target tempname)
        tempname)
      (lambda args
        (if (= (system-error-errno args) EEXIST)
            (try-again (tempname-in link-prefix))
            (throw args))))))

;; There are 3 main kinds of errors we can get from hardlinking: "Too many
;; things link to this" (EMLINK), "this link already exists" (EEXIST), and
;; "can't fit more stuff in this directory" (ENOSPC). 

(define (replace-with-link target to-replace)
  "Replace the file TO-REPLACE with a link to TARGET. Note: assumes that
TARGET and TO-REPLACE are on the same filesystem. If they aren't, bad things
will happen!"
  (let ((temp-link (get-temp-link target (dirname to-replace))))
    (rename-file temp-link to-replace)))

(define-syntax ignore-system-errors
  (syntax-rules ()
    "Given a list of system error codes to ignore, evaluates EXPS and returns
#f if any of the system error codes in the given list are thrown."
    ((ignore-system-errors (errors ...) exps ...)
     (catch 'system-error
       (lambda ()
         exps ...)
       (lambda args
         (case (system-error-errno args)
           ((errors ...) #f)
           (else (throw args))))))))

;; Under what conditions would PATH be on a separate filesystem from the
;; .links directory? Any instance of that as far as I can tell would be a
;; misuse of DEDUPLICATE, such as specifying a PATH that isn't in STORE. 
(define* (deduplicate path hash #:optional (store %store-directory))
  "Checks if a store item with hash HASH already exists. If so, replaces PATH
with a hardlink to the already-existing one. If not, it registers PATH so that
future duplicates can hardlink to it. If PATH isn't under the default
%store-directory, the directory it is under must be given as STORE. "
  (let* ((links-directory (string-append store
                                         "/.links"))
         (link-file (string-append links-directory "/"
                                   (bytevector->base16-string hash))))
    (mkdir-p links-directory)
    (if (file-is-directory? path)
        ;; Can't hardlink directories, gotta hardlink their atoms.
        (for-each (lambda (file)
                    (deduplicate file (nar-sha256 file) store))
                  (scandir path))
        (if (file-exists? link-file)
            (ignore-system-errors (EMLINK)
                                  (replace-with-link path link-file))
            (catch 'system-error
              (lambda ()
                (link path link-file))
              (lambda args
                (case (system-error-errno args)
                  ((EEXIST)
                   ;; Someone else put an entry for PATH in links-directory
                   ;; before we could! Let's use it!
                   (ignore-system-errors (EMLINK)
                                         (replace-with-link path link-file)))
                  ;; It's fine if there's not enough room in the directory
                  ;; index or whatever for more entries in .links, we just
                  ;; need to stop in that case.
                  ((ENOSPC) #f)
                  ;; EMLINK is an error - we got here because initially there
                  ;; wasn't an entry in .links, and suddenly there are so many
                  ;; things linked to the original file that we can't make another
                  ;; one? Sounds like an error! Anything we haven't anticipated,
                  ;; too.
                  (else (throw args)))))))))

(define (reset-timestamps directory)
  "Reset the timestamps of all the files under DIRECTORY, so that they appear
as created and modified at the Epoch."
  (display "clearing file timestamps...\n")
  (for-each (lambda (file)
              (let ((s (lstat file)))
                ;; XXX: Guile uses libc's 'utime' function (not 'futime'), so
                ;; the timestamp of symlinks cannot be changed, and there are
                ;; symlinks here pointing to /gnu/store, which is the host,
                ;; read-only store.
                (unless (eq? (stat:type s) 'symlink)
                  (utime file 0 0 0 0))))
            (find-files directory #:directories? #t)))
