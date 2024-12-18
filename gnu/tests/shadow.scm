;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2024 Giacomo Leidi <goodoldpaul@autistici.org>
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

(define-module (gnu tests shadow)
  #:use-module (gnu packages base)
  #:use-module (gnu packages containers)
  #:use-module (gnu tests)
  #:use-module (gnu services)
  #:use-module (gnu system)
  #:use-module (gnu system accounts)
  #:use-module (gnu system shadow)
  #:use-module (gnu system vm)
  #:use-module (guix gexp)
  #:export (%test-subids))


(define %subids-os
  (simple-operating-system
   (simple-service
    'simple-profile
    profile-service-type
    (list podman))
   (simple-service
    'simple-subids
    subids-service-type
    (subids-extension
     (subgids
      (list
       (subid-range
        (name "alice"))
       (subid-range
        (name "bob")
        (start 100700))))
     (subuids
      (list
       (subid-range
        (name "alice"))))))))

(define (run-subids-test)
  "Run IMAGE as an OCI backed Shepherd service, inside OS."

  (define os
    (marionette-operating-system
     (operating-system-with-gc-roots
      %subids-os
      (list))
     #:imported-modules '((gnu services herd)
                          (guix combinators))))

  (define vm
    (virtual-machine
     (operating-system os)
     (volatile? #f)
     (memory-size 1024)
     (disk-image-size (* 3000 (expt 2 20)))
     (port-forwardings '())))

  (define test
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (srfi srfi-11) (srfi srfi-64)
                       (gnu build marionette))

          (define marionette
            ;; Relax timeout to accommodate older systems and
            ;; allow for pulling the image.
            (make-marionette (list #$vm) #:timeout 60))

          (test-runner-current (system-test-runner #$output))
          (test-begin "subids")

          (test-equal "/etc/subid and /etc/subgid are created and their content is sound"
            '("root:100000:700\nbob:100700:65536\nalice:166236:65536\n"
              "root:100000:65536\nalice:165536:65536\n")
            (marionette-eval
             `(begin
                (use-modules (ice-9 textual-ports))

                (define (read-file file-name)
                  (call-with-input-file file-name get-string-all))

                (let* ((response1 (read-file "/etc/subgid"))
                       (response2 (read-file "/etc/subuid")))
                  (list response1 response2)))
             marionette))

          (test-equal "podman unshare runs for unprivileged users"
            "         0       1000          1\n         1     165536      65536"
            (marionette-eval
             `(begin
                (use-modules (srfi srfi-1)
                             (ice-9 popen)
                             (ice-9 match)
                             (ice-9 rdelim)
                             (ice-9 textual-ports))
                (define out-dir "/tmp")
                (define (read-file file-name)
                  (call-with-input-file file-name get-string-all))

                (define (wait-for-file file)
                  ;; Wait until FILE shows up.
                  (let loop ((i 60))
                    (cond ((file-exists? file)
                           #t)
                          ((zero? i)
                           (error "file didn't show up" file))
                          (else
                           (sleep 1)
                           (loop (- i 1))))))

                (define (read-lines file-or-port)
                  (define (loop-lines port)
                    (let loop ((lines '()))
                      (match (read-line port)
                        ((? eof-object?)
                         (reverse lines))
                        (line
                         (loop (cons line lines))))))

                  (if (port? file-or-port)
                      (loop-lines file-or-port)
                      (call-with-input-file file-or-port
                        loop-lines)))

                (define slurp
                  (lambda args
                    (let* ((port (apply open-pipe* OPEN_READ
                                        (list "sh" "-l" "-c"
                                              (string-join
                                               args
                                               " "))))
                           (output (read-lines port))
                           (status (close-pipe port)))
                      output)))

                (match (primitive-fork)
                  (0
                   (dynamic-wind
                     (const #f)
                     (lambda ()
                       (setgid (passwd:gid (getpwnam "alice")))
                       (setuid (passwd:uid (getpw "alice")))

                       (let* ((response1 (slurp
                                          "podman" "unshare" "cat" "/proc/self/uid_map")))
                         (call-with-output-file (string-append out-dir "/response1")
                           (lambda (port)
                             (display (string-join response1 "\n") port)))))
                     (lambda ()
                       (primitive-exit 127))))
                  (pid
                   (cdr (waitpid pid))))
                (wait-for-file (string-append out-dir "/response1"))
                (read-file (string-append out-dir "/response1")))
             marionette))

          (test-end))))

  (gexp->derivation "subids-test" test))

(define %test-subids
  (system-test
   (name "subids")
   (description "Test sub UIDs and sub GIDs provisioning service.")
   (value (run-subids-test))))
