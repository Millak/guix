;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016-2020, 2022, 2024 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2018 Clément Lassieur <clement@lassieur.org>
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

(define-module (gnu tests mcron)
  #:use-module (gnu tests)
  #:use-module (gnu system)
  #:use-module (gnu system vm)
  #:use-module (gnu services)
  #:use-module (gnu services mcron)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (guix monads)
  #:export (%test-mcron))

;;;
;;; Mcron.
;;;

(define %mcron-os
  ;; System with an mcron service, with one mcron job for "root" and one mcron
  ;; job for an unprivileged user.
  (let ((job1 #~(job '(next-second '(0 5 10 15 20 25 30 35 40 45 50 55))
                     (lambda ()
                       (unless (file-exists? "witness")
                        (call-with-output-file "witness"
                          (lambda (port)
                            (display (list (getuid) (getgid)) port)))))))
        (job2 #~(job next-second-from
                     (lambda ()
                       (call-with-output-file "witness"
                         (lambda (port)
                           (display (list (getuid) (getgid)) port))))
                     #:user "alice"))
        (job3 #~(job next-second-from             ;to test $PATH
                     "touch witness-touch")))
    (simple-operating-system
     (service mcron-service-type
              (mcron-configuration (jobs (list job1 job2 job3)))))))

(define (run-mcron-test name)
  (define os
    (marionette-operating-system
     %mcron-os
     #:imported-modules '((gnu services herd)
                          (guix combinators))))

  (define test
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (gnu build marionette)
                       (srfi srfi-64)
                       (ice-9 match))

          (define marionette
            (make-marionette (list #$(virtual-machine os))))

          (test-runner-current (system-test-runner #$output))
          (test-begin "mcron")

          (test-assert "service running"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (start-service 'mcron))
             marionette))

          ;; Make sure root's mcron job runs, has its cwd set to "/root", and
          ;; runs with the right UID/GID.
          (test-equal "root's job"
            '(0 0)
            (wait-for-file "/root/witness" marionette))

          ;; Likewise for Alice's job.  We cannot know what its GID is since
          ;; it's chosen by 'groupadd', but it's strictly positive.
          (test-assert "alice's job"
            (match (wait-for-file "/home/alice/witness" marionette)
              ((1000 gid)
               (>= gid 100))))

          ;; Last, the job that uses a command; allows us to test whether
          ;; $PATH is sane.
          (test-equal "root's job with command"
            ""
            (wait-for-file "/root/witness-touch" marionette
                           #:read '(@ (ice-9 rdelim) read-string)))

          ;; Make sure the 'schedule' action is accepted.
          (test-equal "schedule action"
            '(#t)                                 ;one value, #t
            (marionette-eval '(with-shepherd-action 'mcron ('schedule) result
                                result)
                             marionette))

          (test-end))))

  (gexp->derivation name test))

(define %test-mcron
  (system-test
   (name "mcron")
   (description "Make sure the mcron service works as advertised.")
   (value (run-mcron-test name))))
