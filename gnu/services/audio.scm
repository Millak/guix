;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Peter Mikkelsen <petermikkelsen10@gmail.com>
;;; Copyright © 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2020 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2022 Bruno Victal <mirai@makinata.eu>
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

(define-module (gnu services audio)
  #:use-module (guix gexp)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages mpd)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (mpd-output
            mpd-output?
            mpd-configuration
            mpd-configuration?
            mpd-service-type))

;;; Commentary:
;;;
;;; Audio related services
;;;
;;; Code:

(define (uglify-field-name field-name)
  (let ((str (symbol->string field-name)))
    (string-join (string-split (if (string-suffix? "?" str)
                                   (string-drop-right str 1)
                                   str)
                               #\-) "_")))

(define (mpd-serialize-field field-name value)
  #~(format #f "~a ~s~%" #$(if (string? field-name)
                               field-name
                               (uglify-field-name field-name))
            #$(if (string? value)
                  value
                  (object->string value))))

(define (mpd-serialize-alist field-name value)
  #~(string-append #$@(generic-serialize-alist list mpd-serialize-field
                                               value)))

(define mpd-serialize-string mpd-serialize-field)

(define (mpd-serialize-boolean field-name value)
  (mpd-serialize-field field-name (if value "yes" "no")))

(define (mpd-serialize-list-of-mpd-output field-name value)
  #~(string-append "\naudio_output {\n"
                   #$@(map (cut serialize-configuration <>
                                mpd-output-fields)
                           value)
                   "}\n"))

(define (mpd-serialize-configuration configuration)
  (mixed-text-file
   "mpd.conf"
   (serialize-configuration configuration mpd-configuration-fields)))

(define-configuration mpd-output
  (name
   (string "MPD")
   "The name of the audio output.")

  (type
   (string "pulse")
   "The type of audio output.")

  (enabled?
   (boolean #t)
   "Specifies whether this audio output is enabled when MPD is started. By
default, all audio outputs are enabled. This is just the default
setting when there is no state file; with a state file, the previous
state is restored.")

  (tags?
   (boolean #t)
   "If set to @code{#f}, then MPD will not send tags to this output. This
is only useful for output plugins that can receive tags, for example the
@code{httpd} output plugin.")

  (always-on?
   (boolean #f)
   "If set to @code{#t}, then MPD attempts to keep this audio output always
open. This may be useful for streaming servers, when you don’t want to
disconnect all listeners even when playback is accidentally stopped.")

  (mixer-type
   (string "none")
   "This field accepts a symbol that specifies which mixer should be used
for this audio output: the @code{hardware} mixer, the @code{software}
mixer, the @code{null} mixer (allows setting the volume, but with no
effect; this can be used as a trick to implement an external mixer
External Mixer) or no mixer (@code{none}).")

  (extra-options
   (alist '())
   "An association list of option symbols to string values to be appended to
the audio output configuration.")

  (prefix mpd-))

(define list-of-mpd-output?
  (list-of mpd-output?))

(define-configuration mpd-configuration
  (user
   (string "mpd")
   "The user to run mpd as.")

  (music-dir
   (string "~/Music")
   "The directory to scan for music files."
   (lambda (_ x)
     (mpd-serialize-field "music_directory" x)))

  (playlist-dir
   (string "~/.mpd/playlists")
   "The directory to store playlists."
   (lambda (_ x)
     (mpd-serialize-field "playlist_directory" x)))

  (db-file
   (string "~/.mpd/tag_cache")
   "The location of the music database.")

  (state-file
   (string "~/.mpd/state")
   "The location of the file that stores current MPD's state.")

  (sticker-file
   (string "~/.mpd/sticker.sql")
   "The location of the sticker database.")

  (port
   (string "6600")
   "The port to run mpd on.")

  (address
   (string "any")
   "The address that mpd will bind to.
To use a Unix domain socket, an absolute path can be specified here."
   (lambda (_ x)
     (mpd-serialize-field "bind_to_address" x)))

  (outputs
   (list-of-mpd-output (list (mpd-output)))
   "The audio outputs that MPD can use.
By default this is a single output using pulseaudio.")

  (prefix mpd-))

(define (mpd-file-name config file)
  "Return a path in /var/run/mpd/ that is writable
   by @code{user} from @code{config}."
  (string-append "/var/run/mpd/"
                 (mpd-configuration-user config)
                 "/" file))

(define (mpd-shepherd-service config)
  (shepherd-service
   (documentation "Run the MPD (Music Player Daemon)")
   (requirement '(user-processes))
   (provision '(mpd))
   (start #~(make-forkexec-constructor
             (list #$(file-append mpd "/bin/mpd")
                   "--no-daemon"
                   #$(mpd-serialize-configuration config))
             #:environment-variables
             ;; Required to detect PulseAudio when run under a user account.
             (list (string-append
                    "XDG_RUNTIME_DIR=/run/user/"
                    (number->string
                     (passwd:uid
                      (getpwnam #$(mpd-configuration-user config))))))
             #:log-file #$(mpd-file-name config "log")))
   (stop  #~(make-kill-destructor))))

(define (mpd-service-activation config)
  (with-imported-modules '((guix build utils))
    #~(begin
        (use-modules (guix build utils))
        (define %user
          (getpw #$(mpd-configuration-user config)))

        (let ((directory #$(mpd-file-name config ".mpd")))
          (mkdir-p directory)
          (chown directory (passwd:uid %user) (passwd:gid %user))

          ;; Make /var/run/mpd/USER user-owned as well.
          (chown (dirname directory)
                 (passwd:uid %user) (passwd:gid %user))))))


(define %mpd-accounts
  ;; Default account and group for MPD.
  (list (user-group (name "mpd") (system? #t))
        (user-account
         (name "mpd")
         (group "mpd")
         (system? #t)
         (comment "Music Player Daemon (MPD) user")

         ;; Note: /var/run/mpd hosts one sub-directory per user, of which
         ;; /var/run/mpd/mpd corresponds to the "mpd" user.
         (home-directory "/var/run/mpd/mpd")

         (shell (file-append shadow "/sbin/nologin")))))

(define mpd-service-type
  (service-type
   (name 'mpd)
   (description "Run the Music Player Daemon (MPD).")
   (extensions
    (list (service-extension shepherd-root-service-type
                             (compose list mpd-shepherd-service))
          (service-extension account-service-type
                             (const %mpd-accounts))
          (service-extension activation-service-type
                             mpd-service-activation)))
   (default-value (mpd-configuration))))
