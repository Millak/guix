;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2024 Reepca Russelstein <reepca@russelstein.xyz>
;;; Copyright © 2024 Ludovic Courtès <ludo@gnu.org>
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

(define-module (test-store-deadlock)
  #:use-module (guix tests)
  #:use-module (guix store)
  #:use-module (guix derivations)
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (gnu packages bootstrap)
  #:use-module (ice-9 threads)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64))

(define input-drvs
  (map (lambda (n)
         (computed-file (string-append "drv" (number->string n))
                        #~(begin
                            #$(random-text)
                            (sleep 4)
                            (mkdir #$output))
                        #:guile %bootstrap-guile))
       (iota 15)))

(define top-drv
  (computed-file "top-drv"
                 #~(begin
                     #$(random-text)
                     (sleep 3)
                     (pk 'deps: #$@input-drvs)
                     (mkdir #$output))
                 #:guile %bootstrap-guile))

(%graft? #f)


(test-begin "store-deadlock")

(test-equal "no deadlock"                   ;https://issues.guix.gnu.org/31785
  '(thread1 thread2 thread3)

  ;; This test checks for the absence of a deadlock when guix-daemon spawns
  ;; several child processes (one for each client) that compete to build the
  ;; same set of derivations.  See <https://issues.guix.gnu.org/31785>.
  (let* ((drvs (cons top-drv input-drvs))
         (builder (lambda (name lst)
                    (call-with-new-thread
                     (lambda ()
                       (with-store store
                         (set-build-options store
                                            #:verbosity 3
                                            #:max-build-jobs 1)
                         (run-with-store store
                           (mlet %store-monad ((lst (mapm %store-monad
                                                          lower-object lst)))
                             (mbegin %store-monad
                               (built-derivations lst)
                               (return name)))))))))
         (thread1 (builder 'thread1 drvs))
         (thread2 (begin
                    (sleep 1)
                    (builder 'thread2 drvs)))
         (thread3 (begin
                    (sleep 1)
                    (builder 'thread3 drvs))))

    ;; This test should complete in less than a minute; if not, there's a
    ;; deadlock.
    (let ((deadline (+ (current-time) 120)))
      (list (join-thread thread1 deadline 'timeout)
            (join-thread thread2 deadline 'timeout)
            (join-thread thread3 deadline 'timeout)))))

(test-end)
