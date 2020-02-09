;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2019, 2020 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu installer utils)
  #:use-module (guix utils)
  #:use-module (guix build utils)
  #:use-module (guix i18n)
  #:use-module (srfi srfi-34)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 textual-ports)
  #:export (read-lines
            read-all
            nearest-exact-integer
            read-percentage
            run-shell-command

            with-server-socket
            current-server-socket
            current-clients
            send-to-clients))

(define* (read-lines #:optional (port (current-input-port)))
  "Read lines from PORT and return them as a list."
  (let loop ((line (read-line port))
             (lines '()))
    (if (eof-object? line)
        (reverse lines)
        (loop (read-line port)
              (cons line lines)))))

(define (read-all file)
  "Return the content of the given FILE as a string."
  (call-with-input-file file
    get-string-all))

(define (nearest-exact-integer x)
  "Given a real number X, return the nearest exact integer, with ties going to
the nearest exact even integer."
  (inexact->exact (round x)))

(define (read-percentage percentage)
  "Read PERCENTAGE string and return the corresponding percentage as a
number. If no percentage is found, return #f"
  (let ((result (string-match "^([0-9]+)%$" percentage)))
    (and result
         (string->number (match:substring result 1)))))

(define* (run-shell-command command #:key locale)
  "Run COMMAND, a string, with Bash, and in the given LOCALE.  Return true if
COMMAND exited successfully, #f otherwise."
  (define (pause)
    (format #t (G_ "Press Enter to continue.~%"))
    (send-to-clients '(pause))
    (match (select (cons (current-input-port) (current-clients))
             '() '())
      (((port _ ...) _ _)
       (read-line port))))

  (call-with-temporary-output-file
   (lambda (file port)
     (when locale
       (let ((supported? (false-if-exception
                          (setlocale LC_ALL locale))))
         ;; If LOCALE is not supported, then set LANGUAGE, which might at
         ;; least give us translated messages.
         (if supported?
             (format port "export LC_ALL=\"~a\"~%" locale)
             (format port "export LANGUAGE=\"~a\"~%"
                     (string-take locale
                                  (string-index locale #\_))))))

     (format port "exec ~a~%" command)
     (close port)

     (guard (c ((invoke-error? c)
                (newline)
                (format (current-error-port)
                        (G_ "Command failed with exit code ~a.~%")
                        (invoke-error-exit-status c))
                (pause)
                #f))
       (invoke "bash" "--init-file" file)
       (newline)
       (pause)
       #t))))


;;;
;;; Client protocol.
;;;

(define %client-socket-file
  ;; Unix-domain socket where the installer accepts connections.
  "/var/guix/installer-socket")

(define current-server-socket
  ;; Socket on which the installer is currently accepting connections, or #f.
  (make-parameter #f))

(define current-clients
  ;; List of currently connected clients.
  (make-parameter '()))

(define* (open-server-socket
          #:optional (socket-file %client-socket-file))
  "Open SOCKET-FILE as a Unix-domain socket to accept incoming connections and
return it."
  (mkdir-p (dirname socket-file))
  (when (file-exists? socket-file)
    (delete-file socket-file))
  (let ((sock (socket AF_UNIX SOCK_STREAM 0)))
    (bind sock AF_UNIX socket-file)
    (listen sock 0)
    sock))

(define (call-with-server-socket thunk)
  (if (current-server-socket)
      (thunk)
      (let ((socket (open-server-socket)))
        (dynamic-wind
          (const #t)
          (lambda ()
            (parameterize ((current-server-socket socket))
              (thunk)))
          (lambda ()
            (close-port socket))))))

(define-syntax-rule (with-server-socket exp ...)
  "Evaluate EXP with 'current-server-socket' parameterized to a currently
accepting socket."
  (call-with-server-socket (lambda () exp ...)))

(define (send-to-clients exp)
  "Send EXP to all the current clients."
  (for-each (lambda (client)
              (write exp client)
              (newline client)
              (force-output client))
            (current-clients)))
