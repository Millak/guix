;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2022 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2025 Maxim Cournoyer <maxim@guixotic.coop>
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

(define-module (gnu services games)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages games)
  #:use-module (gnu packages luanti)
  #:use-module ((gnu services base) #:select (udev-service-type))
  #:use-module (gnu system shadow)
  #:use-module ((gnu system file-systems) #:select (file-system-mapping))
  #:use-module (gnu build linux-container)
  #:use-module (guix build utils)
  #:autoload   (guix least-authority) (%default-preserved-environment-variables
                                       least-authority-wrapper)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (joycond-configuration
            joycond-configuration?
            joycond-service-type

            luanti-configuration
            luanti-configuration?
            luanti-configuration-game-configuration
            luanti-configuration-game
            luanti-configuration-mods
            luanti-configuration-log-file
            luanti-configuration-luanti
            luanti-configuration-port
            luanti-configuration-verbose?
            luanti-configuration-world
            luanti-service-type

            wesnothd-configuration
            wesnothd-configuration?
            wesnothd-service-type))

;;;
;;; Joycond
;;;

(define-configuration/no-serialization joycond-configuration
  (package (package joycond) "The joycond package to use"))

(define (joycond-shepherd-service config)
  (let ((joycond (joycond-configuration-package config)))
    (list (shepherd-service
           (documentation "Run joycond.")
           (provision '(joycond))
           (requirement '(user-processes bluetooth))
           (start #~(make-forkexec-constructor
                     (list #$(file-append joycond "/bin/joycond"))))
           (stop #~(make-kill-destructor))))))

(define joycond-service-type
  (service-type
   (name 'joycond)
   (description
    "Run @command{joycond} for pairing Nintendo joycons via Bluetooth and
install udev rules required to use the controller as an unprivileged user.")
   (extensions
    (list (service-extension shepherd-root-service-type
                             joycond-shepherd-service)
          (service-extension udev-service-type
                             (compose list joycond-configuration-package))))
   (default-value (joycond-configuration))))


;;;
;;; Luanti.
;;;

(define list-of-file-likes?
  (list-of file-like?))

(define-maybe/no-serialization list-of-file-likes)

(define-maybe/no-serialization file-like)

(define-maybe/no-serialization string)

(define (port? x)
  (and (number? x)
       (and (>= 0) (<= x 65535))))

(define-configuration/no-serialization luanti-configuration
  (luanti
   (file-like luanti-server)
   "The Luanti package to use.")
  (game
   (file-like luanti-mineclonia)
   "The Luanti game package to serve.")
  (game-configuration
   maybe-file-like
   "A configuration file to use for the selected Luanti game, which
corresponds to the @file{minetest.conf} file.")
  (mods
   maybe-list-of-file-likes
   "A list of Luanti mod packages to use.  Note that using mods is complicated
by the requirements of Luanti to 1) manually enable the mod and any of its
dependent mods in the @file{world.rt} file of the world used and 2) to
register the mod names and those of its dependents via a
@samp{secure.trusted_mods} @code{game-configuration} directive.  Consult the
example below for more precise directions.")
  (log-file
   (maybe-string "/var/log/luanti.log")
   "The log file to log to.  To disable logging, set this to
@code{%unset-value}.")
  (verbose?
   (boolean #f)
   "Print more detailed information.")
  (port
   (port 30000)
   "The UDP port the server should listen to.")
  (world
   maybe-string
   "An existing Luanti world directory to serve.  If omitted, a new world is
created under the @file{/var/lib/luanti/.minetest/worlds/world} directory.  If
an absolute file name is provided, it is used directly.  Otherwise, it is
expected to be a directory under @file{/var/lib/luanti/.minetest/worlds/}."))

(define %luanti-account
  (list (user-group
          (name "luanti")
          (system? #t))
        (user-account
          (name "luanti")
          (group "luanti")
          (system? #t)
          (comment "Luanti server user")
          (home-directory "/var/lib/luanti"))))

(define (luanti-activation config)
  "Activation script for the Luanti server."
  (match-record config <luanti-configuration> (world)
    #~(begin
        (use-modules (guix build utils)
                     (srfi srfi-34))

        (define user (getpwnam "luanti"))
        (define* (sanitize-permissions file #:optional (mode #o400))
          (guard (c (#t #t))
            (chown file (passwd:uid user) (passwd:gid user))
            (chmod file mode)))

        (mkdir-p/perms "/var/lib/luanti" (getpwnam "luanti") #o755)

        ;; Sanitize the permissions of a provided pre-populated world
        ;; directory.
        (when #$(and (maybe-value-set? world)
                     (absolute-file-name? world))
          (for-each sanitize-permissions
                    (find-files #$world #:directories? #t))))))

(define (transitive-mods mods)
  "Return the transitive list of mods in MODS, these included."
  (append-map (lambda (m)
                (if (package? m)
                    (cons m (map second ;drop label
                                 (package-transitive-propagated-inputs m)))
                    (list m)))
              (if (maybe-value-set? mods)
                  mods
                  '())))

(define (luanti-wrapper config)
  "Return a least-authority wrapper for 'luantiserver', based on CONFIG, a
<luanti-configuration> object."
  (match-record config <luanti-configuration>
                (luanti game game-configuration log-file mods world)
    (let ((mods (transitive-mods mods)))
      (least-authority-wrapper
       (file-append luanti "/bin/luantiserver")
       #:name "luantiserver-pola-wrapper"
       #:mappings
       (let ((readable (filter maybe-value-set?
                               (append (list luanti game game-configuration)
                                       mods)))
             (writable (filter maybe-value-set?
                               (append (list "/var/lib/luanti" log-file)
                                       (if (and (maybe-value-set? world)
                                                (absolute-file-name? world))
                                           (list world)
                                           '())))))
         (append (map (lambda (r)
                        (file-system-mapping
                          (source r)
                          (target source)))
                      readable)
                 (map (lambda (w)
                        (file-system-mapping
                          (source w)
                          (target source)
                          (writable? #t)))
                      writable)))
       #:user "luanti"
       #:group "luanti"
       ;; XXX: The user namespace must be shared otherwise the UID is different
       ;; in the container and Luanti fails to create its data directory.
       #:namespaces (fold delq %namespaces '(user net))
       #:preserved-environment-variables
       (cons* "LUANTI_GAME_PATH" "LUANTI_MOD_PATH"
              %default-preserved-environment-variables)))))

(define (luanti-shepherd-service config)
  "Return the <shepherd-service> object of Luanti."
  (match-record config <luanti-configuration>
                ( luanti game game-configuration log-file mods verbose?
                  port world)
    ;; Some mods have dependencies on other mods; we need to ensure these gets
    ;; added to the LUANTI_MOD_PATH as well.
    (let ((mods (transitive-mods mods)))
      (list (shepherd-service
              (provision '(luanti))
              (requirement '(user-processes))
              (start #~(make-forkexec-constructor
                        (append (list #$(luanti-wrapper config)
                                      "--port" (number->string #$port))
                                (if #$(maybe-value-set? game-configuration)
                                    '("--config" #$game-configuration)
                                    '())
                                (if #$verbose?
                                    '("--verbose")
                                    '())
                                (if #$(maybe-value-set? world)
                                    (if (absolute-file-name? #$world)
                                        '("--world" #$world)
                                        '("--worldname" #$world))
                                    '()))
                        #:environment-variables
                        (append
                         (list "HOME=/var/lib/luanti"
                               (string-append "LUANTI_GAME_PATH="
                                              #$game "/share/luanti/games")
                               (string-append
                                "LUANTI_MOD_PATH="
                                (list->search-path-as-string
                                 (search-path-as-list '("share/luanti/mods")
                                                      '#$mods)
                                 ":"))))
                        #:log-file #$(and (maybe-value-set? log-file)
                                          log-file)))
              (stop  #~(make-kill-destructor)))))))

(define luanti-service-type
  (service-type
    (name 'luanti)
    (extensions
     (list (service-extension shepherd-root-service-type
                              luanti-shepherd-service)
           (service-extension profile-service-type
                              (match-record-lambda <luanti-configuration>
                                  (luanti game)
                                (list luanti game)))
           (service-extension account-service-type
                              (const %luanti-account))
           (service-extension activation-service-type
                              luanti-activation)))
    (default-value (luanti-configuration))
    (description
     "Run @url{https://www.luanti.org/en/, Luanti}, the voxel game engine, as a
server.")))


;;;
;;; The Battle for Wesnoth server
;;;

(define-record-type* <wesnothd-configuration>
  wesnothd-configuration make-wesnothd-configuration wesnothd-configuration?
  (package wesnothd-configuration-package
           (default wesnoth-server))
  (port wesnothd-configuration-port
        (default 15000)))

(define %wesnothd-accounts
  (list (user-account
         (name "wesnothd")
         (group "wesnothd")
         (system? #t)
         (comment "Wesnoth daemon user")
         (home-directory "/var/empty")
         (shell (file-append shadow "/sbin/nologin")))
        (user-group
         (name "wesnothd")
         (system? #t))))

(define wesnothd-shepherd-service
  (match-lambda
    (($ <wesnothd-configuration> package port)
     (let ((wesnothd (least-authority-wrapper
                      (file-append package "/bin/wesnothd")
                      #:name "wesnothd"
                      #:mappings (list (file-system-mapping
                                        (source "/var/run/wesnothd")
                                        (target source)
                                        (writable? #t)))
                      #:namespaces (delq 'net %namespaces))))
       (shepherd-service
        (documentation "The Battle for Wesnoth server")
        (provision '(wesnoth-daemon))
        (requirement '(user-processes networking))
        (start #~(make-forkexec-constructor
                  (list #$wesnothd "-p" #$(number->string port))
                  #:user "wesnothd" #:group "wesnothd"))
        (stop #~(make-kill-destructor)))))))

(define wesnothd-activation
  (with-imported-modules '((guix build utils))
    #~(begin
        (use-modules (guix build utils))

        (let* ((user (getpw "wesnothd"))
               (directory "/var/run/wesnothd"))
          ;; wesnothd creates a Unix-domain socket in DIRECTORY.
          (mkdir-p directory)
          (chown directory (passwd:uid user) (passwd:gid user))))))

(define wesnothd-service-type
  (service-type
   (name 'wesnothd)
   (description
    "Run The Battle for Wesnoth server @command{wesnothd}.")
   (extensions
    (list (service-extension account-service-type
                             (const %wesnothd-accounts))
          (service-extension activation-service-type
                             (const wesnothd-activation))
          (service-extension shepherd-root-service-type
                             (compose list wesnothd-shepherd-service))))
   (default-value (wesnothd-configuration))))
