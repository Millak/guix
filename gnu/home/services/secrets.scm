;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025 Lilah Tascheter <lilah@lunabee.space>
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

(define-module (gnu home services secrets)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu packages password-utils)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:export (wrap-himitsu-prompter
            home-himitsu-configuration
            home-himitsu-service-type

            home-himitsu-ssh-configuration
            home-himitsu-ssh-service-type

            home-himitsu-secret-service-configuration
            home-himitsu-secret-service-type))

;;
;; himitsu
;;

(define (wrap-himitsu-prompter prompter)
  "Wraps a prompter to autodetect DISPLAY and WAYLAND_DISPLAY, necessary since
shepherd typically starts before a graphical session."
  (program-file "himitsu-wrapped-prompter"
    #~(begin (use-modules (ice-9 ftw) (ice-9 match) (srfi srfi-26))
        (unless (or (getenv "DISPLAY") (getenv "WAYLAND_DISPLAY"))
          (for-each (lambda (dir pat cont)
                      (match (scandir dir (cut regexp-exec pat <>) string<?)
                        ((file . _) (cont file)) (_ #f)))
            (list "/tmp/.X11-unix" (or (getenv "XDG_RUNTIME_DIR")
                                     (string-append "/run/user/" (getuid))))
            (map make-regexp '("^X[0-9]+$" "^wayland-[0-9]+$"))
            (list (lambda (f)
                    (setenv "DISPLAY" (string-append ":" (string-drop f 1))))
                  (cut setenv "WAYLAND_DISPLAY" <>))))
        (apply execl #$prompter #$prompter (cdr (command-line))))))

(define-maybe string (prefix himitsu-))

(define (string-or-gexp? s)
  (or (string? s) (gexp? s)))

(define list-of-strings-or-gexps? (list-of string-or-gexp?))

(define (himitsu-serialize-string label val)
  #~(format #f "~a=~a~%" '#$label #$val))

(define himitsu-serialize-file-like himitsu-serialize-string)

(define (himitsu-serialize-list-of-strings-or-gexps label vals)
  #~(string-join (list #$@vals) "\n" 'suffix))

(define-configuration home-himitsu-configuration
  (package (file-like himitsu) "himitsu package to use." empty-serializer)
  (notify-reuse maybe-string "Command run to notify the user upon use of a
secret it was already authorized to use. Limited shell expansion available.")
  (prompter (file-like (wrap-himitsu-prompter
                         (file-append hiprompt-gtk "/bin/hiprompt-gtk")))
"himitsu prompter to use to request user authorization for secret access.")
  (extra-options (list-of-strings-or-gexps '()) "A list of extra options, one
per line, to put verbatim into the config file.")
  (prefix himitsu-))

(define (himitsu-serialize config)
  (mixed-text-file "himitsu.ini" "[himitsud]\n"
    (serialize-configuration config home-himitsu-configuration-fields)))

(define (himitsu-shepherd-service config)
  (define start
    #~(lambda _
        (let ((data (or (getenv "XDG_DATA_HOME")
                        (string-append user-homedir "/.local/share"))))
          (if (directory-exists? (string-append data "/himitsu"))
            (fork+exec-command
              (list #$(let ((pkg (home-himitsu-configuration-package config)))
                        (file-append pkg "/bin/himitsud"))
                    "-C" #$(himitsu-serialize config))
              #:log-file (string-append %user-log-dir "/himitsu.log"))
            (error "Himitsu store must exist before the service can run! Try \
running 'himitsu-store -i' first?")))))
  (list (shepherd-service
          (documentation "Run the Himitsu secret service manager.")
          (provision '(himitsud))
          (modules (cons* '(shepherd support) %default-modules))
          (start start)
          (stop #~(make-kill-destructor)))))

;; we need to install the file or else himitsu-store will gen it
(define (himitsu-xdg-configuration-service config)
  `(("himitsu/config.ini" ,(himitsu-serialize config))))

(define home-himitsu-service-type
  (service-type
    (name 'himitsu)
    (extensions
      (list (service-extension home-shepherd-service-type
                               himitsu-shepherd-service)
            (service-extension home-xdg-configuration-files-service-type
                               himitsu-xdg-configuration-service)
            (service-extension home-profile-service-type
                               (const (list himitsu)))))
    (compose concatenate)
    (extend (lambda (config exts)
              (let ((opts (home-himitsu-configuration-extra-options config)))
                (home-himitsu-configuration
                  (inherit config)
                  (extra-options (append exts opts))))))
    (default-value (home-himitsu-configuration))
    (description "Run the Himitsu secret storage manager upon login. You
should create the Himitsu database in advance.")))

;;
;; himitsu-ssh
;;

(define (remember-option? opt)
  (or (number? opt) (memq opt '(session skip refuse))))

(define list-of-remember-options? (list-of remember-option?))

(define (himitsu-serialize-list-of-remember-options label val)
  (himitsu-serialize-string label
    (string-join (map (lambda (x) (cond ((number? x) (number->string x))
                                        ((symbol? x) (symbol->string x))))
                      val)
                 ",")))

(define-configuration home-himitsu-ssh-configuration
  (package (file-like himitsu-ssh) "himitsu-ssh package to use."
           empty-serializer)
  (persist (list-of-remember-options '(session 300 refuse)) "List of options
given when prompting to allow himitsu-ssh to see your keys. The option chosen
decides how long himitsu-ssh has this access. Options can be either 'session,
'refuse, 'skip, or a timeout in seconds. 'session means until the daemon
closes, 'refuse to decline and never ask again, and 'skip means to ask again
next use.")
  (disclose (list-of-remember-options '(skip session 300)) "List of options
given when prompting to allow himitsu-ssh use of your keys. The format is the
same has persist.")
  (prefix himitsu-))

(define (himitsu-ssh-shepherd-service config)
  (let* ((package (home-himitsu-ssh-configuration-package config))
         (binary (file-append package "/bin/hissh-agent")))
    (list (shepherd-service
            (documentation "Start the Himitsu ssh-agent implementation.")
            (provision '(himitsu-ssh ssh-agent))
            (requirement '(himitsud))
            (start #~(make-forkexec-constructor (list #$binary)))
            (stop #~(make-kill-destructor))))))

(define (himitsu-ssh-himitsu.ini config)
  (list "[ssh.remember]"
        (serialize-configuration config home-himitsu-ssh-configuration-fields)))

(define (himitsu-ssh-environment-variables _)
  '(("SSH_AUTH_SOCK" . "${XDG_RUNTIME_DIR:-/run/user/$UID}/hissh-agent")))

(define home-himitsu-ssh-service-type
  (service-type
    (name 'himitsu-ssh)
    (extensions (list (service-extension home-shepherd-service-type
                                         himitsu-ssh-shepherd-service)
                      (service-extension home-himitsu-service-type
                                         himitsu-ssh-himitsu.ini)
                      (service-extension home-environment-variables-service-type
                                         himitsu-ssh-environment-variables)
                      (service-extension home-profile-service-type
                                         (const (list himitsu-ssh)))))
    (default-value (home-himitsu-ssh-configuration))
    (description "Add support for ssh to store keys in Himitsu.")))

;;
;; himitsu-secret-service
;;

(define-configuration home-himitsu-secret-service-configuration
  (package (file-like himitsu-secret-service) "himitsu-secret-service package to
use." empty-serializer)
  (prefix himitsu-))

(define (himitsu-secret-service-shepherd-service config)
  (let* ((package (home-himitsu-secret-service-configuration-package config))
         (binary (file-append package "/bin/hisecrets-agent")))
    (list (shepherd-service
            (documentation "Start the Himitsu secret-service implementation.")
            (provision '(himitsu-secret-service secret-service))
            (requirement '(himitsud dbus))
            (start #~(make-forkexec-constructor (list #$binary)))
            (stop #~(make-kill-destructor))))))

(define home-himitsu-secret-service-type
  (service-type
    (name 'himitsu-secret-service)
    (extensions
      (list (service-extension home-shepherd-service-type
                               himitsu-secret-service-shepherd-service)))
    (default-value (home-himitsu-secret-service-configuration))
    (description "Add support to Himitsu for the freedesktop.org
secret-service protocol.")))
