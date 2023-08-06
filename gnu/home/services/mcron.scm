;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021, 2023 Andrew Tropin <andrew@trop.in>
;;; Copyright © 2021 Xinglu Chen <public@yoctocell.xyz>
;;; Copyright © 2022 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2023 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu home services mcron)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu services mcron)               ;for the service mapping
  #:export (home-mcron-configuration
            home-mcron-service-type))

;;; Commentary:
;;
;; Service for the GNU mcron cron job manager.
;;
;; Example configuration, the first job runs mbsync once every ten
;; minutes, the second one writes "Mcron service" to ~/mcron-file once
;; every minute.
;;
;; (service home-mcron-service-type
;;            (home-mcron-configuration
;;             (jobs (list #~(job '(next-minute
;;                                  (range 0 60 10))
;;                                (lambda ()
;;                                  (system* "mbsync" "--all")))
;;                         #~(job next-minute-from
;;                                (lambda ()
;;                                  (call-with-output-file (string-append (getenv "HOME")
;;                                                                        "/mcron-file")
;;                                    (lambda (port)
;;                                      (display "Mcron service" port)))))))))
;;
;;; Code:

(define-syntax-rule (home-mcron-configuration fields ...)
  ;; Macro provided for backward compatibility.
  (for-home (mcron-configuration fields ...)))

(define home-mcron-service-type
  (service-type
   (inherit (system->home-service-type mcron-service-type))
   (default-value (for-home (mcron-configuration)))))

(define-service-type-mapping
  mcron-service-type => home-mcron-service-type)

;;; mcron.scm ends here
