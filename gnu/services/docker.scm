;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Danny Milosavljevic <dannym@scratchpost.org>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2020, 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2020 Jesse Dowell <jessedowell@gmail.com>
;;; Copyright © 2021 Brice Waegeneire <brice@waegenei.re>
;;; Copyright © 2023, 2024, 2025 Giacomo Leidi <goodoldpaul@autistici.org>
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

(define-module (gnu services docker)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu services containers)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system privilege)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages docker)
  #:use-module (gnu packages linux)               ;singularity
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:re-export (oci-image                          ;for backwards compatibility, until the
               oci-image?                         ;oci-container-service-type is fully deprecated
               oci-image-fields
               oci-image-repository
               oci-image-tag
               oci-image-value
               oci-image-pack-options
               oci-image-target
               oci-image-system
               oci-image-grafts?
               oci-container-configuration
               oci-container-configuration?
               oci-container-configuration-fields
               oci-container-configuration-user
               oci-container-configuration-group
               oci-container-configuration-command
               oci-container-configuration-entrypoint
               oci-container-configuration-host-environment
               oci-container-configuration-environment
               oci-container-configuration-image
               oci-container-configuration-provision
               oci-container-configuration-requirement
               oci-container-configuration-log-file
               oci-container-configuration-auto-start?
               oci-container-configuration-respawn?
               oci-container-configuration-shepherd-actions
               oci-container-configuration-network
               oci-container-configuration-ports
               oci-container-configuration-volumes
               oci-container-configuration-container-user
               oci-container-configuration-workdir
               oci-container-configuration-extra-arguments
               oci-container-shepherd-service
               %oci-container-accounts)

  #:export (containerd-configuration
            containerd-service-type
            docker-configuration
            docker-service-type
            singularity-service-type
            oci-container-service-type))

(define-maybe file-like)

(define-configuration docker-configuration
  (docker
   (file-like docker)
   "Docker daemon package.")
  (docker-cli
   (file-like docker-cli)
   "Docker client package.")
  (containerd
   (file-like containerd)
   "Deprecated.  Do not use.")
  (proxy
   (file-like docker-libnetwork-cmd-proxy)
   "The proxy package to support inter-container and outside-container
loop-back communications.")
  (enable-proxy?
   (boolean #t)
   "Enable or disable the user-land proxy (enabled by default).")
  (debug?
   (boolean #f)
   "Enable or disable debug output.")
  (enable-iptables?
   (boolean #t)
   "Enable addition of iptables rules (enabled by default).")
  (environment-variables
   (list '())
   "Environment variables to set for dockerd")
  (config-file
   (maybe-file-like)
   "JSON configuration file to pass to dockerd")
  (no-serialization))

(define-configuration containerd-configuration
  (containerd
   (file-like containerd)
   "containerd package.")
  (debug?
   (boolean #f)
   "Enable or disable debug output.")
  (environment-variables
   (list '())
   "Environment variables to set for containerd.")
  (no-serialization))

(define %docker-accounts
  (list (user-group (name "docker") (system? #t))))

(define (%containerd-activation config)
  (let ((state-dir "/var/lib/containerd"))
    #~(begin
        (use-modules (guix build utils))
        (mkdir-p #$state-dir))))

(define (%docker-activation config)
  (%containerd-activation config)
  (let ((state-dir "/var/lib/docker"))
    #~(begin
        (use-modules (guix build utils))
        (mkdir-p #$state-dir))))

(define (containerd-shepherd-service config)
  (match-record config <containerd-configuration>
                (containerd debug? environment-variables)
    (shepherd-service
     (documentation "containerd daemon.")
     (provision '(containerd))
     (requirement '(user-processes))
     (start #~(make-forkexec-constructor
               (list (string-append #$containerd "/bin/containerd")
                     #$@(if debug?
                            '("--log-level=debug")
                            '()))
               ;; For finding containerd-shim binary.
               #:environment-variables
               (list #$@environment-variables
                     (string-append "PATH=" #$containerd "/bin"))
               #:pid-file "/run/containerd/containerd.pid"
               #:pid-file-timeout 300
               #:log-file "/var/log/containerd.log"))
     (stop #~(make-kill-destructor)))))

(define containerd-service-type
  (service-type (name 'containerd)
                (description "Run containerd container runtime.")
                (extensions
                 (list
                  ;; Make sure the 'ctr' command is available.
                  (service-extension profile-service-type
                                     (compose list containerd-configuration-containerd))
                  (service-extension shepherd-root-service-type
                                     (lambda (config)
                                       (list (containerd-shepherd-service config))))))
                (default-value (containerd-configuration))))

(define (docker-shepherd-service config)
  (let* ((docker (docker-configuration-docker config))
         (enable-proxy? (docker-configuration-enable-proxy? config))
         (enable-iptables? (docker-configuration-enable-iptables? config))
         (environment-variables (docker-configuration-environment-variables config))
         (proxy (docker-configuration-proxy config))
         (debug? (docker-configuration-debug? config))
         (config-file (docker-configuration-config-file config)))
    (shepherd-service
           (documentation "Docker daemon.")
           (provision '(dockerd))
           (requirement '(user-processes
                          containerd
                          dbus-system
                          elogind
                          file-system-/sys/fs/cgroup
                          networking
                          udev))
           (start #~(make-forkexec-constructor
                     (list (string-append #$docker "/bin/dockerd")
                           "-p" "/var/run/docker.pid"
                           #$@(if (not (eq? config-file %unset-value))
                                  (list #~(string-append
                                           "--config-file=" #$config-file))
                                  '())
                           #$@(if debug?
                                  '("--debug" "--log-level=debug")
                                  '())
                           #$@(if enable-proxy?
                                  (list "--userland-proxy=true"
                                        #~(string-append
                                           "--userland-proxy-path=" #$proxy "/bin/proxy"))
                                  '("--userland-proxy=false"))
                           (if #$enable-iptables?
                               "--iptables"
                               "--iptables=false")
                           "--containerd" "/run/containerd/containerd.sock")
                     #:environment-variables
                     (list #$@environment-variables)
                     #:pid-file "/var/run/docker.pid"
                     #:log-file "/var/log/docker.log"))
           (stop #~(make-kill-destructor)))))

(define docker-service-type
  (service-type (name 'docker)
                (description "Provide capability to run Docker application
bundles in Docker containers.")
                (extensions
                 (list
                  ;; Make sure the 'docker' command is available.
                  (service-extension profile-service-type
                                     (compose list docker-configuration-docker-cli))
                  (service-extension activation-service-type
                                     %docker-activation)
                  (service-extension shepherd-root-service-type
                                     (lambda (config)
                                       (list (docker-shepherd-service config))))
                  (service-extension account-service-type
                                     (const %docker-accounts))))
                (default-value (docker-configuration))))


;;;
;;; Singularity.
;;;

(define %singularity-activation
  (with-imported-modules '((guix build utils))
    #~(begin
        (use-modules (guix build utils))

        (define %mount-directory
          "/var/singularity/mnt/")

        ;; Create the directories that Singularity 2.6 expects to find.  Make
        ;; them #o755 like the 'install-data-hook' rule in 'Makefile.am' of
        ;; Singularity 2.6.1.
        (for-each (lambda (directory)
                    (let ((directory (string-append %mount-directory
                                                    directory)))
                      (mkdir-p directory)
                      (chmod directory #o755)))
                  '("container" "final" "overlay" "session"))
        (chmod %mount-directory #o755))))

(define (singularity-privileged-programs singularity)
  "Return the privileged programs that SINGULARITY needs."
  (define helpers
    ;; The helpers, under a meaningful name.
    (computed-file "singularity-privileged-helpers"
                   #~(begin
                       (mkdir #$output)
                       (for-each (lambda (program)
                                   (symlink (string-append #$singularity
                                                           "/libexec/singularity"
                                                           "/bin/"
                                                           program "-suid")
                                            (string-append #$output
                                                           "/singularity-"
                                                           program
                                                           "-helper")))
                                 '("action" "mount" "start")))))

  (map file-like->setuid-program
       (list (file-append helpers "/singularity-action-helper")
             (file-append helpers "/singularity-mount-helper")
             (file-append helpers "/singularity-start-helper"))))

(define singularity-service-type
  (service-type (name 'singularity)
                (description
                 "Install the Singularity application bundle tool.")
                (extensions
                 (list (service-extension privileged-program-service-type
                                          singularity-privileged-programs)
                       (service-extension activation-service-type
                                          (const %singularity-activation))))
                (default-value singularity)))


;;;
;;; OCI container.
;;;

(define (configs->shepherd-services configs)
  (map oci-container-shepherd-service configs))

(define oci-container-service-type
  (service-type (name 'oci-container)
                (extensions (list (service-extension profile-service-type
                                                     (lambda _ (list docker-cli)))
                                  (service-extension account-service-type
                                                     (const %oci-container-accounts))
                                  (service-extension shepherd-root-service-type
                                                     configs->shepherd-services)))
                (default-value '())
                (extend append)
                (compose concatenate)
                (description
                 "This service allows the management of OCI
containers as Shepherd services.")))
