;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix status)
  #:use-module (guix records)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-19)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (ice-9 binary-ports)
  #:use-module (rnrs bytevectors)
  #:export (build-event-output-port

            build-status
            build-status?

            build-status-updater
            print-build-status))

(define %newline
  (char-set #\return #\newline))

(define (build-event-output-port proc seed)
  "Return an output port for use as 'current-build-output-port' that calls
PROC with its current state value, initialized with SEED, on every build
event.  Build events passed to PROC are tuples corresponding to the \"build
traces\" produced by the daemon:

  (build-started \"/gnu/store/...-foo.drv\" ...)
  (substituter-started \"/gnu/store/...-foo\" ...)

and so on. "
  (define %fragments
    ;; Line fragments received so far.
    '())

  (define %state
    ;; Current state for PROC.
    seed)

  (define (process-line line)
    (when (string-prefix? "@ " line)
      (match (string-tokenize (string-drop line 2))
        (((= string->symbol event-name) args ...)
         (set! %state
           (proc (cons event-name args)
                 %state))))))

  (define (write! bv offset count)
    (let loop ((str (utf8->string bv)))
      (match (string-index str %newline)
        ((? integer? cr)
         (let ((tail (string-take str cr)))
           (process-line (string-concatenate-reverse
                          (cons tail %fragments)))
           (set! %fragments '())
           (loop (string-drop str (+ 1 cr)))))
        (#f
         (set! %fragments (cons str %fragments))
         count))))

  (make-custom-binary-output-port "filtering-input-port"
                                  write!
                                  #f #f
                                  #f))


;;;
;;; Build status tracking.
;;;

;; Builds and substitutions performed by the daemon.
(define-record-type* <build-status> build-status make-build-status
  build-status?
  (building     build-status-building             ;list of drv
                (default '()))
  (substituting build-status-substituting         ;list of <ongoing-download>
                (default '()))
  (builds-completed build-status-builds-completed ;list of drv
                    (default '()))
  (substitutes-completed build-status-substitutes-completed ;list of store items
                         (default '())))

(define-record-type <ongoing-download>
  (ongoing-download item uri size start transferred)
  ongoing-download?
  (item         ongoing-download-item)            ;store item
  (uri          ongoing-download-uri)             ;string | #f
  (size         ongoing-download-size)            ;integer | #f
  (start        ongoing-download-start)           ;<time>
  (transferred  ongoing-download-transferred))    ;integer

(define (matching-download item)
  (lambda (ongoing)
    (string=? item (ongoing-download-item ongoing))))

(define (compute-status event status)
  "Given EVENT, a tuple like (build-started \"/gnu/store/...-foo.drv\" ...),
compute a new status based on STATUS."
  (match event
    (('build-started drv _ ...)
     (build-status
      (inherit status)
      (building (cons drv (build-status-building status)))))
    (('build-succeeded drv _ ...)
     (build-status
      (inherit status)
      (building (delete drv (build-status-building status)))
      (builds-completed (cons drv (build-status-builds-completed status)))))
    (('substituter-started item _ ...)
     (build-status
      (inherit status)
      (substituting (cons (ongoing-download item #f #f (current-time) 0)
                          (build-status-substituting status)))))
    (('substituter-succeeded item _ ...)
     (build-status
      (inherit status)
      (substituting (remove (matching-download item)
                            (build-status-substituting status)))
      (substitutes-completed
       (cons item (build-status-substitutes-completed status)))))
    (('download-progress item uri
                         (= string->number size)
                         (= string->number transferred))
     (let ((downloads (remove (matching-download item)
                              (build-status-substituting status)))
           (current   (find (matching-download item)
                            (build-status-substituting status))))
       (build-status
        (inherit status)
        (substituting (cons (ongoing-download item uri size
                                              (ongoing-download-start current)
                                              transferred)
                            downloads)))))
    (_
     (pk 'unhandled event)
     status)))

(define (simultaneous-jobs status)
  (+ (length (build-status-building status))
     (length (build-status-substituting status))))

(define (cursor-up n port)
  (format port "\r\x1b[K\x1b~a[A" n)              ;erase in line + up
  (force-output port))

(define* (print-build-status event old-status status #:optional
                             (port (current-error-port)))
  (define (new-download? download)
    (zero? (ongoing-download-transferred download)))

  ;; (let loop ((n (lines-printed old-status)))
  ;;   (when (> n 1)
  ;;     (cursor-up port)))

  (match event
    (('substituter-succeeded _ ...)
     (newline port))
    (_ #t))

  (match (build-status-building status)
    (() #t)
    ((building ...)
     (format port "building~{ ~a~}~%" building)))

  (match (build-status-substituting status)
    (() #t)
    ((download rest ...)
     (if (and (<= (simultaneous-jobs old-status) 1)
              (= 1 (simultaneous-jobs status)))
         (let* ((now      (current-time))
                (elapsed  (time-difference now
                                           (ongoing-download-start download)))
                (throughput (if (zero? (time-second elapsed))
                                0
                                (/ (ongoing-download-transferred download)
                                   1024.
                                   (time-second elapsed)))))
           (if (zero? (ongoing-download-transferred download))
               (format port "downloading ~a..."
                       (ongoing-download-item download))
               (format port "\r\x1b[K~a ~a KiB | ~6,1f KiB/s"
                       (ongoing-download-uri download)
                       (ongoing-download-transferred download)
                       throughput))
           (force-output port))
         (let ((downloads (filter new-download? (cons download rest))))
           (unless (null? downloads)
             (format port "downloading ~{ ~a~}~%"
                     (map ongoing-download-item downloads))
             (force-output port)))))))

(define* (build-status-updater #:optional (on-change (const #t)))
  (lambda (event status)
    (let ((new (compute-status event status)))
      (on-change event status new)
      new)))

;; (define current-build-output-port
;;   ;; The port where build output is sent.
;;   (make-parameter (filtering-output-port (current-error-port))))
