;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Danny Milosavljevic <dannym@scratchpost.org>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2020, 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2020 Jesse Dowell <jessedowell@gmail.com>
;;; Copyright © 2021 Brice Waegeneire <brice@waegenei.re>
;;; Copyright © 2023, 2024 Giacomo Leidi <goodoldpaul@autistici.org>
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
  #:use-module (gnu image)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu services base)
  #:use-module (gnu services dbus)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system)
  #:use-module (gnu system image)
  #:use-module (gnu system setuid)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages admin)               ;shadow
  #:use-module (gnu packages docker)
  #:use-module (gnu packages linux)               ;singularity
  #:use-module (guix records)
  #:use-module (guix diagnostics)
  #:use-module (guix gexp)
  #:use-module (guix i18n)
  #:use-module (guix monads)
  #:use-module (guix packages)
  #:use-module (guix profiles)
  #:use-module ((guix scripts pack) #:prefix pack:)
  #:use-module (guix store)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)

  #:export (containerd-configuration
            containerd-service-type
            docker-configuration
            docker-service-type
            singularity-service-type
            oci-image
            oci-image?
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
            oci-container-service-type
            oci-container-shepherd-service
            %oci-container-accounts))

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
           (requirement '(containerd
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

(define (singularity-setuid-programs singularity)
  "Return the setuid-root programs that SINGULARITY needs."
  (define helpers
    ;; The helpers, under a meaningful name.
    (computed-file "singularity-setuid-helpers"
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
                 (list (service-extension setuid-program-service-type
                                          singularity-setuid-programs)
                       (service-extension activation-service-type
                                          (const %singularity-activation))))
                (default-value singularity)))


;;;
;;; OCI container.
;;;

(define (oci-sanitize-pair pair delimiter)
  (define (valid? member)
    (or (string? member)
        (gexp? member)
        (file-like? member)))
  (match pair
    (((? valid? key) . (? valid? value))
     #~(string-append #$key #$delimiter #$value))
    (_
     (raise
      (formatted-message
       (G_ "pair members must contain only strings, gexps or file-like objects
but ~a was found")
       pair)))))

(define (oci-sanitize-mixed-list name value delimiter)
  (map
   (lambda (el)
     (cond ((string? el) el)
           ((pair? el) (oci-sanitize-pair el delimiter))
           (else
            (raise
             (formatted-message
              (G_ "~a members must be either a string or a pair but ~a was
found!")
              name el)))))
   value))

(define (oci-sanitize-host-environment value)
  ;; Expected spec format:
  ;; '(("HOME" . "/home/nobody") "JAVA_HOME=/java")
  (oci-sanitize-mixed-list "host-environment" value "="))

(define (oci-sanitize-environment value)
  ;; Expected spec format:
  ;; '(("HOME" . "/home/nobody") "JAVA_HOME=/java")
  (oci-sanitize-mixed-list "environment" value "="))

(define (oci-sanitize-ports value)
  ;; Expected spec format:
  ;; '(("8088" . "80") "2022:22")
  (oci-sanitize-mixed-list "ports" value ":"))

(define (oci-sanitize-volumes value)
  ;; Expected spec format:
  ;; '(("/mnt/dir" . "/dir") "/run/current-system/profile:/java")
  (oci-sanitize-mixed-list "volumes" value ":"))

(define (oci-sanitize-shepherd-actions value)
  (map
   (lambda (el)
     (if (shepherd-action? el)
         el
         (raise
          (formatted-message
           (G_ "shepherd-actions may only be shepherd-action records
but ~a was found") el))))
   value))

(define (oci-sanitize-extra-arguments value)
  (define (valid? member)
    (or (string? member)
        (gexp? member)
        (file-like? member)))
  (map
   (lambda (el)
     (if (valid? el)
         el
         (raise
          (formatted-message
           (G_ "extra arguments may only be strings, gexps or file-like objects
but ~a was found") el))))
   value))

(define (oci-image-reference image)
  (if (string? image)
      image
      (string-append (oci-image-repository image)
                     ":" (oci-image-tag image))))

(define (oci-lowerable-image? image)
  (or (manifest? image)
      (operating-system? image)
      (gexp? image)
      (file-like? image)))

(define (string-or-oci-image? image)
  (or (string? image)
      (oci-image? image)))

(define list-of-symbols?
  (list-of symbol?))

(define-maybe/no-serialization string)

(define-configuration/no-serialization oci-image
  (repository
   (string)
   "A string like @code{myregistry.local:5000/testing/test-image} that names
the OCI image.")
  (tag
   (string "latest")
   "A string representing the OCI image tag. Defaults to @code{latest}.")
  (value
   (oci-lowerable-image)
   "A @code{manifest} or @code{operating-system} record that will be lowered
into an OCI compatible tarball.  Otherwise this field's value can be a gexp
or a file-like object that evaluates to an OCI compatible tarball.")
  (pack-options
   (list '())
   "An optional set of keyword arguments that will be passed to the
@code{docker-image} procedure from @code{guix scripts pack}.  They can be used
to replicate @command{guix pack} behavior:

@lisp
(oci-image
  (repository \"guile\")
  (tag \"3\")
  (manifest (specifications->manifest '(\"guile\")))
  (pack-options
    '(#:symlinks ((\"/bin/guile\" -> \"bin/guile\"))
      #:max-layers 2)))
@end lisp

If the @code{value} field is an @code{operating-system} record, this field's
value will be ignored.")
  (system
   (maybe-string)
   "Attempt to build for a given system, e.g. \"i686-linux\"")
  (target
   (maybe-string)
   "Attempt to cross-build for a given triple, e.g. \"aarch64-linux-gnu\"")
  (grafts?
   (boolean #f)
   "Whether to allow grafting or not in the pack build."))

(define-configuration/no-serialization oci-container-configuration
  (user
   (string "oci-container")
   "The user under whose authority docker commands will be run.")
  (group
   (string "docker")
   "The group under whose authority docker commands will be run.")
  (command
   (list-of-strings '())
   "Overwrite the default command (@code{CMD}) of the image.")
  (entrypoint
   (maybe-string)
   "Overwrite the default entrypoint (@code{ENTRYPOINT}) of the image.")
  (host-environment
   (list '())
   "Set environment variables in the host environment where @command{docker run}
is invoked.  This is especially useful to pass secrets from the host to the
container without having them on the @command{docker run}'s command line: by
setting the @code{MYSQL_PASSWORD} on the host and by passing
@code{--env MYSQL_PASSWORD} through the @code{extra-arguments} field, it is
possible to securely set values in the container environment.  This field's
value can be a list of pairs or strings, even mixed:

@lisp
(list '(\"LANGUAGE\" . \"eo:ca:eu\")
      \"JAVA_HOME=/opt/java\")
@end lisp

Pair members can be strings, gexps or file-like objects. Strings are passed
directly to @code{make-forkexec-constructor}."
   (sanitizer oci-sanitize-host-environment))
  (environment
   (list '())
   "Set environment variables inside the container.  This can be a list of pairs
or strings, even mixed:

@lisp
(list '(\"LANGUAGE\" . \"eo:ca:eu\")
      \"JAVA_HOME=/opt/java\")
@end lisp

Pair members can be strings, gexps or file-like objects. Strings are passed
directly to the Docker CLI.  You can refer to the
@url{https://docs.docker.com/engine/reference/commandline/run/#env,upstream}
documentation for semantics."
   (sanitizer oci-sanitize-environment))
  (image
   (string-or-oci-image)
   "The image used to build the container.  It can be a string or an
@code{oci-image} record.  Strings are resolved by the Docker
Engine, and follow the usual format
@code{myregistry.local:5000/testing/test-image:tag}.")
  (provision
   (maybe-string)
   "Set the name of the provisioned Shepherd service.")
  (requirement
   (list-of-symbols '())
   "Set additional Shepherd services dependencies to the provisioned Shepherd
service.")
  (log-file
   (maybe-string)
   "When @code{log-file} is set, it names the file to which the service’s
standard output and standard error are redirected.  @code{log-file} is created
if it does not exist, otherwise it is appended to.")
  (auto-start?
   (boolean #t)
   "Whether this service should be started automatically by the Shepherd.  If it
is @code{#f} the service has to be started manually with @command{herd start}.")
  (respawn?
   (boolean #f)
   "Whether to restart the service when it stops, for instance when the
underlying process dies.")
  (shepherd-actions
   (list '())
   "This is a list of @code{shepherd-action} records defining actions supported
by the service."
   (sanitizer oci-sanitize-shepherd-actions))
  (network
   (maybe-string)
   "Set a Docker network for the spawned container.")
  (ports
   (list '())
   "Set the port or port ranges to expose from the spawned container.  This can
be a list of pairs or strings, even mixed:

@lisp
(list '(\"8080\" . \"80\")
      \"10443:443\")
@end lisp

Pair members can be strings, gexps or file-like objects. Strings are passed
directly to the Docker CLI.  You can refer to the
@url{https://docs.docker.com/engine/reference/commandline/run/#publish,upstream}
documentation for semantics."
   (sanitizer oci-sanitize-ports))
  (volumes
   (list '())
   "Set volume mappings for the spawned container.  This can be a
list of pairs or strings, even mixed:

@lisp
(list '(\"/root/data/grafana\" . \"/var/lib/grafana\")
      \"/gnu/store:/gnu/store\")
@end lisp

Pair members can be strings, gexps or file-like objects. Strings are passed
directly to the Docker CLI.  You can refer to the
@url{https://docs.docker.com/engine/reference/commandline/run/#volume,upstream}
documentation for semantics."
   (sanitizer oci-sanitize-volumes))
  (container-user
   (maybe-string)
   "Set the current user inside the spawned container.  You can refer to the
@url{https://docs.docker.com/engine/reference/run/#user,upstream}
documentation for semantics.")
  (workdir
   (maybe-string)
   "Set the current working for the spawned Shepherd service.
You can refer to the
@url{https://docs.docker.com/engine/reference/run/#workdir,upstream}
documentation for semantics.")
  (extra-arguments
   (list '())
   "A list of strings, gexps or file-like objects that will be directly passed
to the @command{docker run} invokation."
   (sanitizer oci-sanitize-extra-arguments)))

(define oci-container-configuration->options
  (lambda (config)
    (let ((entrypoint
           (oci-container-configuration-entrypoint config))
          (network
           (oci-container-configuration-network config))
          (user
           (oci-container-configuration-container-user config))
          (workdir
           (oci-container-configuration-workdir config)))
      (apply append
             (filter (compose not unspecified?)
                     `(,(if (maybe-value-set? entrypoint)
                            `("--entrypoint" ,entrypoint)
                            '())
                       ,(append-map
                         (lambda (spec)
                           (list "--env" spec))
                         (oci-container-configuration-environment config))
                       ,(if (maybe-value-set? network)
                            `("--network" ,network)
                            '())
                       ,(if (maybe-value-set? user)
                            `("--user" ,user)
                            '())
                       ,(if (maybe-value-set? workdir)
                            `("--workdir" ,workdir)
                            '())
                       ,(append-map
                         (lambda (spec)
                           (list "-p" spec))
                         (oci-container-configuration-ports config))
                       ,(append-map
                         (lambda (spec)
                           (list "-v" spec))
                         (oci-container-configuration-volumes config))))))))

(define* (get-keyword-value args keyword #:key (default #f))
  (let ((kv (memq keyword args)))
    (if (and kv (>= (length kv) 2))
        (cadr kv)
        default)))

(define (lower-operating-system os target system)
  (mlet* %store-monad
      ((tarball
        (lower-object
         (system-image (os->image os #:type docker-image-type))
         system
         #:target target)))
    (return tarball)))

(define (lower-manifest name image target system)
  (define value (oci-image-value image))
  (define options (oci-image-pack-options image))
  (define image-reference
    (oci-image-reference image))
  (define image-tag
    (let* ((extra-options
            (get-keyword-value options #:extra-options))
           (image-tag-option
            (and extra-options
                 (get-keyword-value extra-options #:image-tag))))
      (if image-tag-option
          '()
          `(#:extra-options (#:image-tag ,image-reference)))))

  (mlet* %store-monad
      ((_ (set-grafting
           (oci-image-grafts? image)))
       (guile (set-guile-for-build (default-guile)))
       (profile
        (profile-derivation value
                            #:target target
                            #:system system
                            #:hooks '()
                            #:locales? #f))
       (tarball (apply pack:docker-image
                       `(,name ,profile
                         ,@options
                         ,@image-tag
                         #:localstatedir? #t))))
    (return tarball)))

(define (lower-oci-image name image)
  (define value (oci-image-value image))
  (define image-target (oci-image-target image))
  (define image-system (oci-image-system image))
  (define target
    (if (maybe-value-set? image-target)
        image-target
        (%current-target-system)))
  (define system
    (if (maybe-value-set? image-system)
        image-system
        (%current-system)))
  (with-store store
   (run-with-store store
     (match value
       ((? manifest? value)
        (lower-manifest name image target system))
       ((? operating-system? value)
        (lower-operating-system value target system))
       ((or (? gexp? value)
            (? file-like? value))
        value)
       (_
        (raise
         (formatted-message
          (G_ "oci-image value must contain only manifest,
operating-system, gexp or file-like records but ~a was found")
          value))))
     #:target target
     #:system system)))

(define (%oci-image-loader name image tag)
  (let ((docker (file-append docker-cli "/bin/docker"))
        (tarball (lower-oci-image name image)))
    (with-imported-modules '((guix build utils))
      (program-file (format #f "~a-image-loader" name)
       #~(begin
           (use-modules (guix build utils)
                        (ice-9 popen)
                        (ice-9 rdelim))

           (format #t "Loading image for ~a from ~a...~%" #$name #$tarball)
           (define line
             (read-line
              (open-input-pipe
               (string-append #$docker " load -i " #$tarball))))

           (unless (or (eof-object? line)
                       (string-null? line))
             (format #t "~a~%" line)
             (let ((repository&tag
                    (string-drop line
                                 (string-length
                                   "Loaded image: "))))

               (invoke #$docker "tag" repository&tag #$tag)
               (format #t "Tagged ~a with ~a...~%" #$tarball #$tag))))))))

(define (oci-container-shepherd-service config)
  (define (guess-name name image)
    (if (maybe-value-set? name)
        name
        (string-append "docker-"
                       (basename
                        (if (string? image)
                            (first (string-split image #\:))
                            (oci-image-repository image))))))

  (let* ((docker (file-append docker-cli "/bin/docker"))
         (actions (oci-container-configuration-shepherd-actions config))
         (auto-start?
          (oci-container-configuration-auto-start? config))
         (user (oci-container-configuration-user config))
         (group (oci-container-configuration-group config))
         (host-environment
          (oci-container-configuration-host-environment config))
         (command (oci-container-configuration-command config))
         (log-file (oci-container-configuration-log-file config))
         (provision (oci-container-configuration-provision config))
         (requirement (oci-container-configuration-requirement config))
         (respawn?
          (oci-container-configuration-respawn? config))
         (image (oci-container-configuration-image config))
         (image-reference (oci-image-reference image))
         (options (oci-container-configuration->options config))
         (name (guess-name provision image))
         (extra-arguments
          (oci-container-configuration-extra-arguments config)))

    (shepherd-service (provision `(,(string->symbol name)))
                      (requirement `(dockerd user-processes ,@requirement))
                      (respawn? respawn?)
                      (auto-start? auto-start?)
                      (documentation
                       (string-append
                        "Docker backed Shepherd service for "
                        (if (oci-image? image) name image) "."))
                      (start
                       #~(lambda ()
                           #$@(if (oci-image? image)
                                  #~((invoke #$(%oci-image-loader
                                                name image image-reference)))
                                  #~())
                           (fork+exec-command
                            ;; docker run [OPTIONS] IMAGE [COMMAND] [ARG...]
                            (list #$docker "run" "--rm" "--name" #$name
                                  #$@options #$@extra-arguments
                                  #$image-reference #$@command)
                            #:user #$user
                            #:group #$group
                            #$@(if (maybe-value-set? log-file)
                                   (list #:log-file log-file)
                                   '())
                            #:environment-variables
                            (list #$@host-environment))))
                      (stop
                       #~(lambda _
                           (invoke #$docker "rm" "-f" #$name)))
                      (actions
                       (if (oci-image? image)
                           '()
                           (append
                            (list
                             (shepherd-action
                              (name 'pull)
                              (documentation
                               (format #f "Pull ~a's image (~a)."
                                       name image))
                              (procedure
                               #~(lambda _
                                   (invoke #$docker "pull" #$image)))))
                            actions))))))

(define %oci-container-accounts
  (list (user-account
         (name "oci-container")
         (comment "OCI services account")
         (group "docker")
         (system? #t)
         (home-directory "/var/empty")
         (shell (file-append shadow "/sbin/nologin")))))

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
