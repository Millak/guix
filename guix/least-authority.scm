;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2022-2023 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix least-authority)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module ((guix store) #:select (%store-prefix))
  #:autoload   (gnu build linux-container) (%namespaces)
  #:autoload   (gnu system file-systems) (file-system-mapping
                                          file-system-mapping-source
                                          spec->file-system
                                          file-system->spec
                                          file-system-mapping->bind-mount)
  #:export (least-authority-wrapper))

;;; Commentary:
;;;
;;; This module provides tools to execute programs with the least authority
;;; necessary, using Linux namespaces.
;;;
;;; Code:

(define %precious-variables
  ;; Environment variables preserved by the wrapper by default.
  '("HOME" "USER" "LOGNAME" "DISPLAY" "XAUTHORITY" "TERM" "TZ" "PAGER"
    "LISTEN_PID" "LISTEN_FDS" "LISTEN_FDNAMES")) ;for make-systemd-constructor

(define* (least-authority-wrapper program
                                  #:key (name "pola-wrapper")
                                  (user #f)
                                  (group #f)
                                  (guest-uid 1000)
                                  (guest-gid 1000)
                                  (mappings '())
                                  (namespaces %namespaces)
                                  (directory "/")
                                  (preserved-environment-variables
                                   %precious-variables))
  "Return a wrapper of PROGRAM that executes it with the least authority.

PROGRAM is executed in separate namespaces according to NAMESPACES, a list of
symbols; it runs with GUEST-UID and GUEST-GID.  MAPPINGS is a list of
<file-system-mapping> records indicating directories mirrored inside the
execution environment of PROGRAM.  DIRECTORY is the working directory of the
wrapped process.  Each environment listed in PRESERVED-ENVIRONMENT-VARIABLES
is preserved; other environment variables are erased.

When USER and GROUP are set and NAMESPACES does not include 'user, change UIDs
and GIDs to these prior to executing PROGRAM.  This usually requires that the
resulting wrapper be executed as root so it can call setgid(2) and setuid(2)."
  (define code
    (with-imported-modules (source-module-closure
                            '((gnu system file-systems)
                              (gnu build shepherd)
                              (gnu build linux-container)))
      #~(begin
          (use-modules (gnu system file-systems)
                       (gnu build linux-container)
                       ((gnu build shepherd) #:select (default-mounts))
                       (srfi srfi-1))

          (define variables
            (filter-map (lambda (variable)
                          (let ((value (getenv variable)))
                            (and value
                                 (string-append variable "=" value))))
                        '#$preserved-environment-variables))

          (define (read-file file)
            (call-with-input-file file read))

          (define references
            (delete-duplicates
             (append-map read-file
                         '#$(map references-file
                                 (cons program
                                       (map file-system-mapping-source
                                            mappings))))))

          (define (store? file-system)
            (string=? (file-system-mount-point file-system)
                      #$(%store-prefix)))

          (define mounts
            (append (map (lambda (item)
                           (file-system-mapping->bind-mount
                            (file-system-mapping (source item)
                                                 (target item))))
                         references)
                    (remove store?
                            (default-mounts
                              #:namespaces '#$namespaces))
                    (map spec->file-system
                         '#$(map (compose file-system->spec
                                          file-system-mapping->bind-mount)
                                 mappings))))

          (define (reify-exit-status status)
            (cond ((status:exit-val status) => exit)
                  ((or (status:term-sig status)
                       (status:stop-sig status))
                   => (lambda (signal)
                        (format (current-error-port)
                                "~a terminated with signal ~a~%"
                                #$program signal)
                        (exit (+ 128 signal))))))

          (define namespaces '#$namespaces)
          (define host-group '#$group)
          (define host-user '#$user)

          ;; Note: 'call-with-container' creates a sub-process that this one
          ;; waits for.  This might seem suboptimal but unshare(2) isn't
          ;; really applicable: the process would still run in the same PID
          ;; namespace.

          (reify-exit-status
           (call-with-container mounts
             (lambda ()
               (chdir #$directory)
               (environ variables)

               (unless (memq 'user namespaces)
                 ;; This process lives in its parent user namespace,
                 ;; presumably as root; now is the time to setgid/setuid if
                 ;; asked for it (the 'clone' call would fail with EPERM if we
                 ;; changed UIDs/GIDs beforehand).
                 (when host-group
                   (setgid (group:gid (getgr host-group))))
                 (when host-user
                   (setuid (passwd:uid (getpw host-user)))))

               (apply execl #$program #$program (cdr (command-line))))

             ;; Don't assume PROGRAM can behave as an init process.
             #:child-is-pid1? #f

             #:guest-uid #$guest-uid
             #:guest-gid #$guest-gid
             #:namespaces '#$namespaces)))))

  (program-file name code))
