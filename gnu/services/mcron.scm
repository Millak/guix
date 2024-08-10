;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016-2020, 2023 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2022 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2023 Bruno Victal <mirai@makinata.eu>
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

(define-module (gnu services mcron)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu packages guile-xyz)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (ice-9 vlist)
  #:export (mcron-configuration
            mcron-configuration?
            mcron-configuration-mcron
            mcron-configuration-jobs
            mcron-configuration-log?
            mcron-configuration-log-file
            mcron-configuration-log-format
            mcron-configuration-date-format
            mcron-configuration-home-service?

            mcron-service-type))

;;; Commentary:
;;;
;;; This module implements a service that to run instances of GNU mcron, a
;;; periodic job execution daemon.  Example of a service:
;;
;;  (service mcron-service-type
;;           (mcron-configuration
;;            (jobs (list #~(job next-second-from
;;                               (lambda ()
;;                                 (call-with-output-file "/dev/console"
;;                                   (lambda (port)
;;                                     (display "hello!\n" port)))))))))
;;;
;;; Code:

;; Configuration of mcron.
;; XXX: 'define-configuration' cannot be used here due to the need for
;; 'thunked' and 'innate' fields as well as 'this-mcron-configuration'.
(define-record-type* <mcron-configuration> mcron-configuration
  make-mcron-configuration
  mcron-configuration?
  this-mcron-configuration

  (mcron       mcron-configuration-mcron          ;file-like
               (default mcron))
  (jobs        mcron-configuration-jobs           ;list of gexps
               (default '()))
  (log?        mcron-configuration-log?           ;Boolean
               (default #t))
  (log-file    mcron-configuration-log-file       ;string | gexp
               (thunked)
               (default
                 (if (mcron-configuration-home-service?
                      this-mcron-configuration)
                     #~(string-append %user-log-dir "/mcron.log")
                     "/var/log/mcron.log")))
  (log-format  mcron-configuration-log-format     ;string
               (default "~1@*~a ~a: ~a~%"))
  (date-format mcron-configuration-date-format    ;string | #f
               (default #f))

  (home-service? mcron-configuration-home-service?
                 (default for-home?) (innate)))

(define (job-files mcron jobs)
  "Return a list of file-like object for JOBS, a list of gexps."
  (define (validated-file job)
    ;; This procedure behaves like 'scheme-file' but it runs 'mcron
    ;; --schedule' to detect any error in JOB.
    (computed-file "mcron-job"
                   (with-imported-modules '((guix build utils))
                     #~(begin
                         (use-modules (guix build utils))

                         (call-with-output-file "prologue"
                           (lambda (port)
                             ;; This prologue allows 'mcron --schedule' to
                             ;; proceed no matter what #:user option is passed
                             ;; to 'job'.
                             (write '(set! getpw
                                       (const (getpwuid (getuid))))
                                    port)))

                         (call-with-output-file "job"
                           (lambda (port)
                             (write '#$job port)))

                         (invoke #+(file-append mcron "/bin/mcron")
                                 "--schedule=20" "prologue" "job")
                         (copy-file "job" #$output)))
                   #:options '(#:env-vars (("COLUMNS" . "150")))))

  (map validated-file jobs))

(define (shepherd-schedule-action mcron files)
  "Return a Shepherd action that runs MCRON with '--schedule' for the given
files."
  (shepherd-action
   (name 'schedule)
   (documentation
    "Display jobs that are going to be scheduled.")
   (procedure
    #~(lambda* (_ #:optional (n "5"))
        ;; XXX: This is a global side effect.
        (setenv "GUILE_AUTO_COMPILE" "0")

        ;; Run 'mcron' in a pipe so we can explicitly redirect its output to
        ;; 'current-output-port', which at this stage is bound to the client
        ;; connection.
        (let ((pipe (open-pipe* OPEN_READ
                                #$(file-append mcron "/bin/mcron")
                                (string-append "--schedule=" n)
                                #$@files)))
          (let loop ()
            (match (read-line pipe 'concat)
              ((? eof-object?)
               (catch 'system-error
                 (lambda ()
                   (zero? (close-pipe pipe)))
                 (lambda args
                   ;; There's a race with the SIGCHLD handler, which
                   ;; could call 'waitpid' before 'close-pipe' above does.  If
                   ;; we get ECHILD, that means we lost the race, but that's
                   ;; fine.
                   (or (= ECHILD (system-error-errno args))
                       (apply throw args)))))
              (line
               (display line)
               (loop)))))))))

(define (mcron-shepherd-services config)
  (match-record config <mcron-configuration>
    (mcron jobs log? log-file log-format date-format home-service?)
    (if (eq? jobs '())
        '()                             ;nothing to do
        (let ((files (job-files mcron jobs)))
          (list (shepherd-service
                 (provision '(mcron))
                 (requirement (if home-service?
                                  '()
                                  '(user-processes)))
                 (modules `((srfi srfi-1)
                            (srfi srfi-26)
                            (ice-9 popen) ;for the 'schedule' action
                            (ice-9 rdelim)
                            (ice-9 match)
                            ((shepherd support) #:hide (mkdir-p)) ;for '%user-log-dir'
                            ,@%default-modules))
                 (start #~(make-forkexec-constructor
                           (list #$(file-append mcron "/bin/mcron")
                                 #$@(if log?
                                        `("--log" "--log-format" ,log-format
                                          ,@(if date-format
                                                (list "--date-format"
                                                      date-format)
                                                '()))
                                        '())
                                 #$@files)

                           ;; Disable auto-compilation of the job files and
                           ;; set a sane value for 'PATH'.
                           #:environment-variables
                           (cons* "GUILE_AUTO_COMPILE=0"
                                  #$(if home-service?
                                        '(environ)
                                        '(cons*
                                          "PATH=/run/current-system/profile/bin"
                                          (remove (cut string-prefix? "PATH=" <>)
                                                  (environ)))))

                           #:log-file #$log-file))
                 (stop #~(make-kill-destructor))
                 (actions
                  (list (shepherd-schedule-action mcron files)))))))))

(define mcron-service-type
  (service-type (name 'mcron)
                (description
                 "Run the mcron job scheduling daemon.")
                (extensions
                 (list (service-extension shepherd-root-service-type
                                          mcron-shepherd-services)
                       (service-extension profile-service-type
                                          (compose list
                                                   mcron-configuration-mcron))))
                (compose concatenate)
                (extend (lambda (config jobs)
                          (mcron-configuration
                           (inherit config)
                           (home-service?
                            (mcron-configuration-home-service? config))
                           (jobs (append (mcron-configuration-jobs config)
                                         jobs)))))
                (default-value (mcron-configuration)))) ;empty job list

;;; mcron.scm ends here
