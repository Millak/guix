;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2022 ( <paren@disroot.org>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (gnu home services pm)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu packages monitoring)
  #:use-module (gnu services shepherd)

  #:export (home-batsignal-configuration
            home-batsignal-service-type))

;;;
;;; batsignal
;;;
;;; Daemon for running commands and displaying notifications on
;;; battery events.
;;;

(define-record-type* <home-batsignal-configuration>
  home-batsignal-configuration make-home-batsignal-configuration
  home-batsignal-configuration?
  (warning-level batsignal-warning-level                    ;integer
                 (default 15))
  (warning-message batsignal-warning-message                ;string | #f
                   (default #f))
  (critical-level batsignal-critical-level                  ;integer
                 (default 5))
  (critical-message batsignal-critical-message              ;string | #f
                    (default #f))
  (danger-level batsignal-danger-level                      ;integer
                (default 2))
  (danger-command batsignal-danger-command                  ;file-like | string | #f
                  (default #f))
  (full-level batsignal-full-level                          ;integer | #f
              (default #f))
  (full-message batsignal-full-message                      ;string | #f
                (default #f))
  (batteries batsignal-batteries                            ;list of string
             (default '()))
  (poll-delay batsignal-poll-delay                          ;integer
              (default 60))
  (icon batsignal-icon                                      ;file-like | #f
        (default #f))
  (notifications? batsignal-notifications?                  ;boolean
                  (default #t))
  (notifications-expire? batsignal-notifications-expire?    ;boolean
                         (default #f))
  (notification-command batsignal-notification-command      ;string | #f
                        (default #f))
  (ignore-missing? batsignal-ignore-missing?                ;boolean
                   (default #f)))

(define (home-batsignal-shepherd-services config)
  (let ((warning-level (batsignal-warning-level config))
        (warning-message (batsignal-warning-message config))
        (critical-level (batsignal-critical-level config))
        (critical-message (batsignal-critical-message config))
        (danger-level (batsignal-danger-level config))
        (danger-command (batsignal-danger-command config))
        (full-level (batsignal-full-level config))
        (full-message (batsignal-full-message config))
        (batteries (batsignal-batteries config))
        (poll-delay (batsignal-poll-delay config))
        (icon (batsignal-icon config))
        (notifications? (batsignal-notifications? config))
        (notifications-expire? (batsignal-notifications-expire? config))
        (notification-command (batsignal-notification-command config))
        (ignore-missing? (batsignal-ignore-missing? config)))
    (list (shepherd-service
           (provision '(batsignal))
           (documentation "Run the batsignal battery-watching daemon.")
           (start #~(make-forkexec-constructor
                     (append (list #$(file-append batsignal "/bin/batsignal")
                                   "-w" (number->string #$warning-level)
                                   "-c" (number->string #$critical-level)
                                   "-d" (number->string #$danger-level)
                                   "-m" (number->string #$poll-delay))
                             (if #$warning-message
                                 (list "-W" #$warning-message)
                                 (list))
                             (if #$critical-message
                                 (list "-C" #$critical-message)
                                 (list))
                             (if #$danger-command
                                 (list "-D" #$danger-command)
                                 (list))
                             (if #$full-level
                                 (list "-f" (number->string #$full-level))
                                 (list))
                             (if #$full-message
                                 (list "-F" #$full-message)
                                 (list))
                             (if (null? (list #$@batteries))
                                 (list)
                                 (list "-n" (string-join (list #$@batteries) ",")))
                             (if #$icon
                                 (list "-I" #$icon)
                                 (list))
                             (if #$notifications?
                                 (list)
                                 (list "-N"))
                             (if #$notifications-expire?
                                 (list "-e")
                                 (list))
                             (if #$notification-command
                                 (list "-M" #$notification-command)
                                 (list))
                             (if #$ignore-missing?
                                 (list "-i")
                                 (list)))
                     #:log-file (string-append
                                 (or (getenv "XDG_STATE_HOME")
                                     (format #f "~a/.local/state"
                                             (getenv "HOME")))
                                 "/log/batsignal.log")))
           (stop #~(make-kill-destructor))))))

(define home-batsignal-service-type
  (service-type
   (name 'home-batsignal)
   (extensions
    (list (service-extension home-shepherd-service-type
                             home-batsignal-shepherd-services)))
   (default-value (home-batsignal-configuration))
   (description
    "Run batsignal, a battery watching and notification daemon.")))
