;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2024, 2025 Giacomo Leidi <goodoldpaul@autistici.org>
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

(define-module (gnu services backup)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages bash)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (guix build-system copy)
  #:use-module (guix gexp)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix modules)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (srfi srfi-1)
  #:export (restic-backup-job
            restic-backup-job?
            restic-backup-job-fields
            restic-backup-job-restic
            restic-backup-job-user
            restic-backup-job-group
            restic-backup-job-log-file
            restic-backup-job-max-duration
            restic-backup-job-wait-for-termination?
            restic-backup-job-name
            restic-backup-job-repository
            restic-backup-job-password-file
            restic-backup-job-schedule
            restic-backup-job-files
            restic-backup-job-requirement
            restic-backup-job-verbose?
            restic-backup-job-extra-flags

            lower-restic-backup-job

            restic-backup-configuration
            restic-backup-configuration?
            restic-backup-configuration-jobs

            restic-backup-job-program
            restic-backup-job->shepherd-service
            restic-guix
            restic-guix-wrapper-package
            restic-backup-service-profile
            restic-program
            restic-job-log-file
            restic-backup-job-command
            restic-backup-job-modules
            restic-backup-service-type))

(define (gexp-or-string? value)
  (or (gexp? value)
      (string? value)))

(define (lowerable? value)
  (or (file-like? value)
      (gexp-or-string? value)))

(define list-of-lowerables?
  (list-of lowerable?))

(define list-of-symbols?
  (list-of symbol?))

(define-maybe/no-serialization string)
(define-maybe/no-serialization number)
(define-maybe/no-serialization symbol)
(define-maybe/no-serialization list-of-symbols)

(define-configuration/no-serialization restic-backup-job
  (restic
   (package restic)
   "The restic package to be used for the current job.")
  (user
   (string "root")
   "The user used for running the current job.")
  (group
   (string "root")
   "The group used for running the current job.")
  (log-file
   (maybe-string)
   "The file system path to the log file for this job.  By default the file will
have be @file{/var/log/restic-backup/@var{job-name}.log}, where @var{job-name} is the
name defined in the @code{name} field.  For Guix Home services it defaults to
@file{$XDG_STATE_HOME/shepherd/restic-backup/@var{job-name}.log}.")
  (max-duration
   (maybe-number)
   "The maximum duration in seconds that a job may last.  Past
@code{max-duration} seconds, the job is forcefully terminated.")
  (wait-for-termination?
   (boolean #f)
   "Wait until the job has finished before considering executing it again;
otherwise, perform it strictly on every occurrence of event, at the risk of
having multiple instances running concurrently.")
  (name
   (string)
   "A string denoting a name for this job.")
  (repository
   (string)
   "The restic repository target of this job.")
  (password-file
   (string)
   "Name of the password file, readable by the configured @code{user}, that
will be used to set the @code{RESTIC_PASSWORD} environment variable for the
current job.")
  (schedule
   (gexp-or-string)
   "A string or a gexp representing the frequency of the backup.  Gexp must
evaluate to @code{calendar-event} records or to strings.  Strings must contain
Vixie cron date lines.")
  (requirement
   (maybe-list-of-symbols)
   "The list of Shepherd services that this backup job depends upon.  When unset it
defaults to @code{'()}, for Guix Home.  Otherwise to
@code{'(user-processes file-systems)}.")
  (files
   (list-of-lowerables '())
   "The list of files or directories to be backed up.  It must be a list of
values that can be lowered to strings.")
  (verbose?
   (boolean #f)
   "Whether to enable verbose output for the current backup job.")
  (extra-flags
   (list-of-lowerables '())
   "A list of values that are lowered to strings.  These will be passed as
command-line arguments to the current job @command{restic backup} invocation."))

;; (for-home (restic-backup-configuration ...)) is not able to replace for-home? with #t,
;; pk prints #f. Once for-home will be able to work with (gnu services configuration) the
;; record can be migrated back to define-configuration.
(define-record-type* <restic-backup-configuration>
  restic-backup-configuration
  make-restic-backup-configuration
  restic-backup-configuration?
  this-restic-backup-configuration

  (jobs  restic-backup-configuration-jobs  (default '()))     ; list of restic-backup-job
  (home-service? restic-backup-configuration-home-service?
                 (default for-home?) (innate)))

(define (lower-restic-backup-job config)
  (let ((restic
         (file-append (restic-backup-job-restic config) "/bin/restic"))
        (repository
         (restic-backup-job-repository config))
        (password-file
         (restic-backup-job-password-file config))
        (files
         (restic-backup-job-files config))
        (extra-flags
         (restic-backup-job-extra-flags config))
        (verbose?
         (if (restic-backup-job-verbose? config)
             '("--verbose")
             '())))
    #~(list (list #$@files) #$restic #$repository #$password-file
            (list #$@verbose?) (list #$@extra-flags))))

(define restic-program
  #~(lambda (action action-args job-restic repository password-file verbose? extra-flags)
      (use-modules (ice-9 format))
      ;; This can be extended later, i.e. to have a
      ;; centrally defined restic package.
      ;; See https://issues.guix.gnu.org/71639
      (define restic job-restic)

      (define command
        `(,restic ,@verbose?
          "-r" ,repository
          ,@extra-flags
          ,action ,@action-args))

      (setenv "RESTIC_PASSWORD_FILE" password-file)

      (when (> (length verbose?) 0)
        (format #t "Running~{ ~a~}~%" command))

      (apply execlp `(,restic ,@command))))

(define (restic-backup-job-program config)
  (program-file
   "restic-backup"
   #~(let ((restic-exec
            #$restic-program)
           (job #$(lower-restic-backup-job config)))

       (apply restic-exec `("backup" ,@job)))))

(define (restic-guix jobs)
  (program-file
   "restic-guix"
   #~(begin
       (use-modules (ice-9 match)
                    (srfi srfi-1))

       (define names '#$(map restic-backup-job-name jobs))
       (define programs '#$(map restic-backup-job-program jobs))

       (define (get-program name)
         (define idx
           (list-index (lambda (n) (string=? n name)) names))
         (unless idx
           (error (string-append "Unknown job name " name "\n\n"
                                 "Possible job names are: "
                                 (string-join names " "))))
         (list-ref programs idx))

       (define (backup args)
         (define name (third args))
         (define program (get-program name))
         (execlp program program))

       (define (validate-args args)
         (when (not (>= (length args) 3))
           (error (string-append "Usage: " (basename (car args))
                                 " backup NAME"))))

       (define (main args)
         (validate-args args)
         (define action (second args))
         (match action
           ("backup"
            (backup args))
           (_
            (error (string-append "Unknown action: " action)))))

       (main (command-line)))))

(define* (restic-job-log-file job #:key (home-service? #f))
  (let ((name (restic-backup-job-name job))
        (log-file (restic-backup-job-log-file job)))
    (if (maybe-value-set? log-file)
        log-file
        (if home-service?
            #~(begin
                (use-modules (shepherd support))
                (string-append %user-log-dir "/restic-backup/" #$name ".log"))
            (string-append "/var/log/restic-backup/" name ".log")))))

(define* (restic-backup-job-command name files #:key (home-service? #f))
  (if home-service?
      #~(list
         "restic-guix" "backup" #$name)
      ;; We go through bash, instead of executing
      ;; restic-guix directly, because the login shell
      ;; gives us the correct user environment that some
      ;; backends require, such as rclone.
      #~(list
          (string-append #$bash-minimal "/bin/bash")
          "-l" "-c"
          (string-append "restic-guix backup " #$name))))

(define* (restic-job-requirement config #:key (home-service? #f))
  (define maybe-requirement (restic-backup-job-requirement config))
  (if (maybe-value-set? maybe-requirement)
      maybe-requirement
      (if home-service?
          '()
          '(user-processes file-systems))))

(define* (restic-backup-job-modules #:key (home-service? #f))
 `((shepherd service timer)
   ,@(if home-service?
         ;;for %user-log-dir
         '((shepherd support))
         '())))

(define* (restic-backup-job->shepherd-service config #:key (home-service? #f))
  (let ((schedule (restic-backup-job-schedule config))
        (name (restic-backup-job-name config))
        (files (restic-backup-job-files config))
        (user (restic-backup-job-user config))
        (group (restic-backup-job-group config))
        (max-duration (restic-backup-job-max-duration config))
        (wait-for-termination? (restic-backup-job-wait-for-termination? config))
        (log-file (restic-job-log-file
                   config #:home-service? home-service?))
        (requirement
         (restic-job-requirement config #:home-service? home-service?)))
    (shepherd-service (provision `(,(string->symbol name)))
                      (requirement requirement)
                      (documentation
                       "Run restic backed backups on a regular basis.")
                      (modules (restic-backup-job-modules
                                #:home-service? home-service?))
                      (start
                       #~(make-timer-constructor
                          (if (string? #$schedule)
                              (cron-string->calendar-event #$schedule)
                              #$schedule)
                          (command
                           #$(restic-backup-job-command
                              name files #:home-service? home-service?)
                           #$@(if home-service? '() (list #:user user))
                           #$@(if home-service? '() (list #:group group))
                           #$@(if home-service? '()
                                  (list
                                   #:environment-variables
                                   #~(list
                                      (string-append
                                       "HOME=" (passwd:dir (getpwnam #$user)))))))
                          #:log-file #$log-file
                          #:wait-for-termination? #$wait-for-termination?
                          #:max-duration #$(and (maybe-value-set? max-duration)
                                                max-duration)))
                      (stop
                       #~(make-timer-destructor))
                      (actions (list (shepherd-action
                                      (inherit shepherd-trigger-action)
                                      (documentation "Manually trigger a backup,
without waiting for the scheduled time.")))))))

(define (restic-guix-wrapper-package jobs)
  (package
    (name "restic-backup-service-wrapper")
    (version "0.0.0")
    (source (restic-guix jobs))
    (build-system copy-build-system)
    (arguments
     (list #:install-plan #~'(("./" "/bin"))))
    (home-page "https://restic.net")
    (synopsis
     "Easily interact from the CLI with Guix configured backups")
    (description
     "This package provides a simple wrapper around @code{restic}, handled
by the @code{restic-backup-service-type}.  It allows for easily interacting
with Guix configured backup jobs, for example for manually triggering a backup
without waiting for the scheduled job to run.")
    (license license:gpl3+)))

(define restic-backup-service-profile
  (lambda (config)
    (define jobs (restic-backup-configuration-jobs config))
    (if (> (length jobs) 0)
        (list
         (restic-guix-wrapper-package jobs))
        '())))

(define restic-backup-service-type
  (service-type (name 'restic-backup)
                (extensions
                 (list
                  (service-extension profile-service-type
                                     restic-backup-service-profile)
                  (service-extension shepherd-root-service-type
                                     (match-record-lambda <restic-backup-configuration>
                                         (jobs home-service?)
                                       (map (lambda (job)
                                              (restic-backup-job->shepherd-service
                                               job #:home-service? home-service?))
                                            jobs)))))
                (compose concatenate)
                (extend
                 (lambda (config jobs)
                   (restic-backup-configuration
                    (inherit config)
                    (jobs (append (restic-backup-configuration-jobs config)
                                  jobs)))))
                (default-value (restic-backup-configuration))
                (description
                 "This service configures Shepherd timers for running backups
with restic.")))
