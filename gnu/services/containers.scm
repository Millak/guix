;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2024 Giacomo Leidi <goodoldpaul@autistici.org>
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

(define-module (gnu services containers)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages containers)
  #:use-module (gnu packages file-systems)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system accounts)
  #:use-module (gnu system shadow)
  #:use-module (gnu system pam)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (srfi srfi-1)
  #:export (rootless-podman-configuration
            rootless-podman-configuration?
            rootless-podman-configuration-fields
            rootless-podman-configuration-podman
            rootless-podman-configuration-group-name
            rootless-podman-configuration-containers-registries
            rootless-podman-configuration-containers-storage
            rootless-podman-configuration-containers-policy
            rootless-podman-configuration-pam-limits
            rootless-podman-configuration-subgids
            rootless-podman-configuration-subuids

            rootless-podman-service-subids
            rootless-podman-service-accounts
            rootless-podman-service-profile
            rootless-podman-shepherd-services
            rootless-podman-service-etc

            rootless-podman-service-type))

(define (gexp-or-string? value)
  (or (gexp? value)
      (string? value)))

(define (lowerable? value)
  (or (file-like? value)
      (gexp-or-string? value)))

(define list-of-pam-limits-entries?
  (list-of pam-limits-entry?))

(define list-of-subid-ranges?
  (list-of subid-range?))

(define (package-or-#f? val)
  (or (not val)
      (package? val)))

(define-configuration/no-serialization rootless-podman-configuration
  (podman
   (package-or-#f podman)
   "The Podman package that will be installed in the system profile.
@code{#f} can be passed to suppress the installation.")
  (group-name
   (string "cgroup")
   "The name of the group that will own /sys/fs/cgroup resources.  Users that
want to use rootless Podman have to be in this group.")
  (containers-registries
   (lowerable
    (plain-file "registries.conf"
                (string-append "unqualified-search-registries = ['docker.io','"
                               "registry.fedora.org','registry.opensuse.org']")))
   "A string or a gexp evaluating to the path of Podman's
@code{containers/registries.conf} configuration file.")
  (containers-storage
   (lowerable
    (plain-file "storage.conf"
                "[storage]
driver = \"overlay\""))
   "A string or a gexp evaluating to the path of Podman's
@code{containers/storage.conf} configuration file.")
  (containers-policy
   (lowerable
    (plain-file "policy.json"
                "{\"default\": [{\"type\": \"insecureAcceptAnything\"}]}"))
   "A string or a gexp evaluating to the path of Podman's
@code{containers/policy.json} configuration file.")
  (pam-limits
   (list-of-pam-limits-entries
    (list (pam-limits-entry "*" 'both 'nofile 100000)))
   "The PAM limits to be set for rootless Podman.")
  (subgids
   (list-of-subid-ranges '())
   "A list of subid ranges representing the subgids that will be
available for each configured user.")
  (subuids
   (list-of-subid-ranges '())
   "A list of subid ranges representing the subuids that will be
available for each configured user."))

(define rootless-podman-service-profile
  (lambda (config)
    (or (and=> (rootless-podman-configuration-podman config) list)
        (list))))

(define rootless-podman-service-etc
  (lambda (config)
    (list `("containers/registries.conf"
            ,(rootless-podman-configuration-containers-registries config))
          `("containers/storage.conf"
            ,(rootless-podman-configuration-containers-storage config))
          `("containers/policy.json"
            ,(rootless-podman-configuration-containers-policy config)))))

(define rootless-podman-service-subids
  (lambda (config)
    (subids-extension
     (subgids (rootless-podman-configuration-subgids config))
     (subuids (rootless-podman-configuration-subuids config)))))

(define rootless-podman-service-accounts
  (lambda (config)
    (list (user-group (name (rootless-podman-configuration-group-name config))
                      (system? #t)))))

(define (cgroups-fs-owner-entrypoint config)
  (define group
    (rootless-podman-configuration-group-name config))
  (program-file "cgroups2-fs-owner-entrypoint"
                #~(system*
                   (string-append #+bash-minimal "/bin/bash") "-l" "-c"
                   (string-append "echo Setting /sys/fs/cgroup "
                                  "group ownership to " #$group " && chown -v "
                                  "root:" #$group " /sys/fs/cgroup && "
                                  "chmod -v 775 /sys/fs/cgroup && chown -v "
                                  "root:" #$group " /sys/fs/cgroup/cgroup."
                                  "{procs,subtree_control,threads} && "
                                  "chmod -v 664 /sys/fs/cgroup/cgroup."
                                  "{procs,subtree_control,threads}"))))

(define (rootless-podman-cgroups-fs-owner-service config)
  (shepherd-service (provision '(cgroups2-fs-owner))
                    (requirement
                     '(user-processes
                       dbus-system
                       elogind
                       file-system-/sys/fs/cgroup
                       networking
                       udev
                       cgroups2-limits))
                    (one-shot? #t)
                    (documentation
                     "Set ownership of /sys/fs/cgroup to the configured group.")
                    (start
                     #~(make-forkexec-constructor
                        (list
                         #$(cgroups-fs-owner-entrypoint config))))
                    (stop
                     #~(make-kill-destructor))))

(define cgroups-limits-entrypoint
  (program-file "cgroups2-limits-entrypoint"
                #~(system*
                   (string-append #+bash-minimal "/bin/bash") "-c"
                   (string-append "echo Setting cgroups v2 limits && "
                                  "echo +cpu +cpuset +io +memory +pids"
                                  " >> /sys/fs/cgroup/cgroup.subtree_control"))))

(define (rootless-podman-cgroups-limits-service config)
  (shepherd-service (provision '(cgroups2-limits))
                    (requirement
                     '(user-processes
                       dbus-system
                       elogind
                       networking
                       udev
                       file-system-/sys/fs/cgroup
                       rootless-podman-shared-root-fs))
                    (one-shot? #t)
                    (documentation
                     "Allow setting cgroups limits: cpu, cpuset, memory and
pids.")
                    (start
                     #~(make-forkexec-constructor
                        (list
                         #$cgroups-limits-entrypoint)))
                    (stop
                     #~(make-kill-destructor))))

(define rootless-podman-shared-root-fs-entrypoint
  (program-file "rootless-podman-shared-root-fs-entrypoint"
                #~(system*
                   "/run/privileged/bin/mount" "--make-shared" "/")))

(define (rootless-podman-shared-root-fs-service config)
  (shepherd-service (provision '(rootless-podman-shared-root-fs))
                    (requirement
                     '(user-processes))
                    (one-shot? #t)
                    (documentation
                     "Buildah/Podman running as rootless expects the bind mount
to be shared.  This service sets it so.")
                    (start
                     #~(make-forkexec-constructor
                        (list
                         #$rootless-podman-shared-root-fs-entrypoint)))
                    (stop
                     #~(make-kill-destructor))))

(define (rootless-podman-shepherd-services config)
  (list
   (rootless-podman-shared-root-fs-service config)
   (rootless-podman-cgroups-limits-service config)
   (rootless-podman-cgroups-fs-owner-service config)))

(define rootless-podman-service-type
  (service-type (name 'rootless-podman)
                (extensions
                 (list
                  (service-extension subids-service-type
                                     rootless-podman-service-subids)
                  (service-extension account-service-type
                                     rootless-podman-service-accounts)
                  (service-extension profile-service-type
                                     rootless-podman-service-profile)
                  (service-extension shepherd-root-service-type
                                     rootless-podman-shepherd-services)
                  (service-extension pam-limits-service-type
                                     rootless-podman-configuration-pam-limits)
                  (service-extension etc-service-type
                                     rootless-podman-service-etc)))
                (default-value (rootless-podman-configuration))
                (description
                 "This service configures rootless @code{podman} on the Guix System.")))
