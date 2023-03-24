;;; GNU Guix --- Functional package management for GNU
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

(define-module (gnu home services sound)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:export (home-pulseaudio-rtp-sink-service-type
            home-pulseaudio-rtp-source-service-type
            %pulseaudio-rtp-multicast-address))


;;;
;;; PulseAudio support.
;;;

(define (with-pulseaudio-connection sock exp)
  ;; Wrap EXP in an expression where SOCK is bound to a socket connected to
  ;; the user's PulseAudio command-line interface socket.
  #~(let* ((#$sock (socket AF_UNIX SOCK_STREAM 0))
           (pulse-user-file
            (lambda (name)
              (string-append "/run/user/" (number->string (getuid))
                             "/pulse/" name)))
           (file (pulse-user-file "cli")))
      (let loop ((tries 0))
        (catch #t
          (lambda ()
            (connect #$sock AF_UNIX file)
            (let ((result #$exp))
              (close-port #$sock)
              result))
          (lambda (key . args)
            (if (and (eq? key 'system-error)
                     (= ENOENT (system-error-errno (cons key args)))
                     (< tries 3))
                ;; The CLI socket doesn't exist yet, so send pulseaudio
                ;; SIGUSR2 so that it creates it and listens to it.
                (let ((pid (call-with-input-file (pulse-user-file "pid")
                             read)))
                  (when (and (integer? pid) (> pid 1))
                    (kill pid SIGUSR2))
                  ((@ (fibers) sleep) 1)
                  (loop (+ tries 1)))
                (begin
                  (close-port #$sock)
                  (apply throw key args))))))))

(define %pulseaudio-rtp-multicast-address
  ;; Default address used by 'module-rtp-sink' and 'module-rtp-recv'.  This is
  ;; a multicast address, for the Session Announcement Protocol (SAP) and the
  ;; Session Description Protocol (SDP).
  "224.0.0.56")

(define (pulseaudio-rtp-sink-shepherd-services destination-ip)
  (list (shepherd-service
         (provision '(pulseaudio-rtp-sink))
         (start
          #~(lambda* (#:optional (destination-ip #$destination-ip))
              #$(with-pulseaudio-connection
                 #~sock
                 #~(begin
                     (display "\
load-module module-null-sink \
sink_name=rtp sink_properties=\"device.description='RTP network output'\"\n"
                              sock)
                     (display (string-append "\
load-module module-rtp-send source=rtp.monitor"
                                             (if destination-ip
                                                 (string-append
                                                  " destination_ip="
                                                  destination-ip)
                                                 "")
                                             "\n")
                              sock)
                     #t))))
         (stop
          #~(lambda (_)
              #$(with-pulseaudio-connection
                 #~sock
                 #~(begin
                     (display "unload-module module-rtp-send\n"
                              sock)
                     (display "unload-module module-null-sink\n"
                              sock)
                     #f))))
         (auto-start? #f))))

(define home-pulseaudio-rtp-sink-service-type
  (service-type
   (name 'pulseaudio-rtp-sink)
   (extensions
    (list (service-extension home-shepherd-service-type
                             pulseaudio-rtp-sink-shepherd-services)))
   (description
    "Define a PulseAudio sink to broadcast audio output over RTP, which can
then by played by another PulseAudio instance.")

   ;; By default, send to the SAP multicast address, 224.0.0.56, which can be
   ;; network-intensive.
   (default-value %pulseaudio-rtp-multicast-address)))

(define (pulseaudio-rtp-source-shepherd-services source-ip)
  (list (shepherd-service
         (provision '(pulseaudio-rtp-source))
         (start
          #~(lambda* (#:optional (source-ip #$source-ip))
              #$(with-pulseaudio-connection
                 #~sock
                 #~(begin
                     (format sock "\
load-module module-rtp-recv sap_address=~a\n" source-ip)
                     #t))))
         (stop
          #~(lambda (_)
              #$(with-pulseaudio-connection
                 #~sock
                 #~(begin
                     (display "unload-module module-rtp-recv\n"
                              sock)
                     #f))))
         (auto-start? #f))))

(define home-pulseaudio-rtp-source-service-type
  (service-type
   (name 'pulseaudio-rtp-source)
   (extensions
    (list (service-extension home-shepherd-service-type
                             pulseaudio-rtp-source-shepherd-services)))
   (description
    "Define a PulseAudio source to receive audio broadcasted over RTP by
another PulseAudio instance.")
   (default-value %pulseaudio-rtp-multicast-address)))
