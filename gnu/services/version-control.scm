;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Nikita <nikita@n0.is>
;;; Copyright © 2016 Sou Bunnbu <iyzsong@member.fsf.org>
;;; Copyright © 2017 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2017 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2018 Christopher Baines <mail@cbaines.net>
;;; Copyright © 2021 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2025 Tomas Volf <~@wolfsden.cz>
;;; Copyright © 2025 Evgeny Pisemsky <mail@pisemsky.site>
;;; Copyright © 2026 Nguyễn Gia Phong <cnx@loang.net>
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

(define-module (gnu services version-control)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services web)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages admin)
  #:use-module (guix deprecation)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:export (git-daemon-service
            git-daemon-service-type
            git-daemon-configuration
            git-daemon-configuration?

            git-http-configuration
            git-http-configuration?
            git-http-nginx-location-configuration

            <gitolite-configuration>
            gitolite-configuration
            gitolite-configuration-package
            gitolite-configuration-user
            gitolite-configuration-rc-file
            gitolite-configuration-admin-pubkey
            gitolite-configuration-admin-name

            <gitolite-rc-file>
            gitolite-rc-file
            gitolite-rc-file-local-code
            gitolite-rc-file-umask
            gitolite-rc-file-unsafe-pattern
            gitolite-rc-file-git-config-keys
            gitolite-rc-file-log-extra
            gitolite-rc-file-host-name
            gitolite-rc-file-roles
            gitolite-rc-file-enable
            gitolite-rc-file-extra-content
            gitolite-rc-file-default-enable

            <gitolite-git-configuration>
            gitolite-git-configuration
            gitolite-git-configuration?
            gitolite-git-configuration-name
            gitolite-git-configuration-email
            gitolite-git-configuration-default-branch
            gitolite-git-configuration-receive-fsck-objects
            gitolite-git-configuration-extra-content

            gitolite-service-type

            gitile-configuration
            gitile-configuration-package
            gitile-configuration-host
            gitile-configuration-port
            gitile-configuration-database
            gitile-configuration-repositories
            gitile-configuration-git-owner-validation?
            gitile-configuration-base-git-url
            gitile-configuration-index-title
            gitile-configuration-intro
            gitile-configuration-footer
            gitile-configuration-nginx

            gitile-service-type

            fossil-configuration
            fossil-configuration-fields
            fossil-configuration?
            fossil-configuration-package
            fossil-configuration-user
            fossil-configuration-group
            fossil-configuration-log-file
            fossil-configuration-repository
            fossil-configuration-acme?
            fossil-configuration-base-url
            fossil-configuration-chroot
            fossil-configuration-ckout-alias
            fossil-configuration-compress?
            fossil-configuration-create?
            fossil-configuration-error-log-file
            fossil-configuration-ext-root
            fossil-configuration-files
            fossil-configuration-from
            fossil-configuration-jail?
            fossil-configuration-js-mode
            fossil-configuration-https?
            fossil-configuration-ip
            fossil-configuration-local-authentication?
            fossil-configuration-main-menu
            fossil-configuration-max-latency
            fossil-configuration-port
            fossil-configuration-list-repositories?
            fossil-configuration-redirect-to-https?
            fossil-configuration-skin
            fossil-configuration-socket-file
            fossil-configuration-socket-mode
            fossil-configuration-th-trace?
            fossil-configuration-tls-certificate
            fossil-configuration-tls-private-key

            fossil-service-type))

;;; Commentary:
;;;
;;; Version Control related services.
;;;
;;; Code:


;;;
;;; Git daemon.
;;;

(define-record-type* <git-daemon-configuration>
  git-daemon-configuration
  make-git-daemon-configuration
  git-daemon-configuration?
  (package          git-daemon-configuration-package        ;file-like
                    (default git))
  (export-all?      git-daemon-configuration-export-all     ;boolean
                    (default #f))
  (base-path        git-daemon-configuration-base-path      ;string | #f
                    (default "/srv/git"))
  (user-path        git-daemon-configuration-user-path      ;string | #f
                    (default #f))
  (listen           git-daemon-configuration-listen         ;list of string
                    (default '()))
  (port             git-daemon-configuration-port           ;number | #f
                    (default #f))
  (whitelist        git-daemon-configuration-whitelist      ;list of string
                    (default '()))
  (extra-options    git-daemon-configuration-extra-options  ;list of string
                    (default '())))

(define git-daemon-shepherd-service
  (match-lambda
    (($ <git-daemon-configuration>
        package export-all? base-path user-path
        listen port whitelist extra-options)
     (let* ((git     (file-append package "/bin/git"))
            (command `(,git
                       "daemon" "--syslog" "--reuseaddr"
                       ,@(if export-all?
                             '("--export-all")
                             '())
                       ,@(if base-path
                             `(,(string-append "--base-path=" base-path))
                             '())
                       ,@(if user-path
                             `(,(string-append "--user-path=" user-path))
                             '())
                       ,@(map (cut string-append "--listen=" <>) listen)
                       ,@(if port
                             `(,(string-append
                                 "--port=" (number->string port)))
                             '())
                       ,@extra-options
                       ,@whitelist)))
       (list (shepherd-service
              (documentation "Run the git-daemon.")
              (requirement '(user-processes networking))
              (provision '(git-daemon))
              (start #~(make-forkexec-constructor '#$command
                                                  #:user "git-daemon"
                                                  #:group "git-daemon"))
              (stop #~(make-kill-destructor))))))))

(define %git-daemon-accounts
  ;; User account and group for git-daemon.
  (list (user-group
         (name "git-daemon")
         (system? #t))
        (user-account
         (name "git-daemon")
         (system? #t)
         (group "git-daemon")
         (comment "Git daemon user")
         (home-directory "/var/empty")
         (shell (file-append shadow "/sbin/nologin")))))

(define (git-daemon-activation config)
  "Return the activation gexp for git-daemon using CONFIG."
  (let ((base-path (git-daemon-configuration-base-path config)))
    #~(begin
        (use-modules (guix build utils))
        ;; Create the 'base-path' directory when it's not '#f'.
        (and=> #$base-path mkdir-p))))

(define git-daemon-service-type
  (service-type
   (name 'git-daemon)
   (extensions
    (list (service-extension shepherd-root-service-type
                             git-daemon-shepherd-service)
          (service-extension account-service-type
                             (const %git-daemon-accounts))
          (service-extension activation-service-type
                             git-daemon-activation)))
   (description
    "Expose Git repositories over the insecure @code{git://} TCP-based
protocol.")
   (default-value (git-daemon-configuration))))

(define-deprecated (git-daemon-service #:key (config (git-daemon-configuration)))
  git-daemon-service-type
  "Return a service that runs @command{git daemon}, a simple TCP server to
expose repositories over the Git protocol for anonymous access.

The optional @var{config} argument should be a
@code{<git-daemon-configuration>} object, by default it allows read-only
access to exported repositories under @file{/srv/git}."
  (service git-daemon-service-type config))


;;;
;;; HTTP access.  Add the result of calling
;;; git-http-nginx-location-configuration to an nginx-server-configuration's
;;; "locations" field.
;;;

(define-record-type* <git-http-configuration>
  git-http-configuration
  make-git-http-configuration
  git-http-configuration?
  (package          git-http-configuration-package        ;file-like
                    (default git))
  (git-root         git-http-configuration-git-root       ;string
                    (default "/srv/git"))
  (export-all?      git-http-configuration-export-all?    ;boolean
                    (default #f))
  (uri-path         git-http-configuration-uri-path       ;string
                    (default "/git/"))
  (fcgiwrap-socket  git-http-configuration-fcgiwrap-socket ;string
                    (default "127.0.0.1:9000")))

(define* (git-http-nginx-location-configuration #:optional
                                                (config
                                                 (git-http-configuration)))
  (match config
    (($ <git-http-configuration> package git-root export-all?
                                 uri-path fcgiwrap-socket)
     (nginx-location-configuration
      (uri (string-append "~ /" (string-trim-both uri-path #\/) "(/.*)"))
      (body
       (list
        (list "fastcgi_pass " fcgiwrap-socket ";")
        (list "fastcgi_param SCRIPT_FILENAME "
              package "/libexec/git-core/git-http-backend"
              ";")
        "fastcgi_param QUERY_STRING $query_string;"
        "fastcgi_param REQUEST_METHOD $request_method;"
        "fastcgi_param CONTENT_TYPE $content_type;"
        "fastcgi_param CONTENT_LENGTH $content_length;"
        (if export-all?
            "fastcgi_param GIT_HTTP_EXPORT_ALL \"\";"
            "")
        (list "fastcgi_param GIT_PROJECT_ROOT " git-root ";")
        (list "fastcgi_param GIT_CONFIG_GLOBAL "
              (plain-file "gitconfig"
                          (string-append "[safe]\n\tdirectory = " git-root "/*\n"))
              ";")
        "fastcgi_param PATH_INFO $1;"))))))


;;;
;;; Gitolite
;;;

(define gitolite-rc-file-default-enable
  '("help"
    "desc"
    "info"
    "perms"
    "writable"
    "ssh-authkeys"
    "git-config"
    "daemon"
    "gitweb"))

(define-record-type* <gitolite-rc-file>
  gitolite-rc-file make-gitolite-rc-file
  gitolite-rc-file?
  (umask           gitolite-rc-file-umask
                   (default #o0077))
  (local-code      gitolite-rc-file-local-code
                   (default "$rc{GL_ADMIN_BASE}/local"))
  (unsafe-pattern  gitolite-rc-file-unsafe-pattern
                   (default #f))
  (git-config-keys gitolite-rc-file-git-config-keys
                   (default ""))
  (log-extra       gitolite-rc-file-log-extra
                   (default #f))
  (host-name       gitolite-rc-file-host-name
                   (default #f))
  (roles           gitolite-rc-file-roles
                   (default '(("READERS" . 1)
                              ("WRITERS" . 1))))
  (enable          gitolite-rc-file-enable
                   (default gitolite-rc-file-default-enable))
  (extra-content   gitolite-rc-extra-content
                   (default "")))

(define-gexp-compiler (gitolite-rc-file-compiler
                       (file <gitolite-rc-file>) system target)
  (match-record file <gitolite-rc-file>
                ( umask local-code unsafe-pattern git-config-keys log-extra
                  host-name roles enable extra-content)
    (apply text-file* "gitolite.rc"
           `("%RC = (\n"
             "    UMASK => " ,(format #f "~4,'0o" umask) ",\n"
             "    GIT_CONFIG_KEYS => '" ,git-config-keys "',\n"
             ,(if local-code
                  (simple-format #f "    LOCAL_CODE => \"~A\",\n" local-code)
                  "")
             ,(if log-extra
                  "    LOG_EXTRA => 1,\n"
                  "")
             ,(if host-name
                  (simple-format #f "    HOSTNAME => \"~A\",\n" host-name)
                  "")
             "    ROLES => {\n"
             ,@(map (match-lambda
                      ((role . value)
                       (simple-format #f "        ~A => ~A,\n" role value)))
                    roles)
             "    },\n"
             "\n"
             "    ENABLE => [\n"
             ,@(map (lambda (value)
                      (simple-format #f "        '~A',\n" value))
                    enable)
             "    ],\n"
             ,extra-content "\n"
             ");\n"
             "\n"
             ,(if unsafe-pattern
                  (string-append "$UNSAFE_PATT = qr(" unsafe-pattern ");")
                  "")
             "1;\n"
             "# Local variables:\n"
             "# mode: perl\n"
             "# End:\n"
             "# vim: set syn=perl:\n"))))

(define-record-type* <gitolite-git-configuration>
  gitolite-git-configuration make-gitolite-git-configuration
  gitolite-git-configuration?
  (name                 gitolite-git-configuration-name
                        (default "GNU Guix"))
  (email                gitolite-git-configuration-email
                        (default "guix@localhost"))
  (default-branch       gitolite-git-configuration-default-branch
                        (default #f))
  (receive-fsck-objects gitolite-git-configuration-receive-fsck-objects
                        (default #f))
  (extra-content        gitolite-git-configuration-extra-content
                        (default "")))

(define-gexp-compiler (gitolite-git-configuration-compiler
                       (config <gitolite-git-configuration>) system target)
  (match-record config <gitolite-git-configuration>
                (name email default-branch receive-fsck-objects extra-content)
    (apply text-file* "gitconfig"
           `("[user]\n"
             "name  = " ,name  "\n"
             "email = " ,email "\n"
             ,@(if default-branch
                   `("[init]\n"
                     "defaultBranch = " ,default-branch "\n")
                   '())
             ,@(if receive-fsck-objects
                   `("[receive]\n"
                     "fsckObjects = true\n")
                   '())
             ,extra-content "\n"))))

(define-record-type* <gitolite-configuration>
  gitolite-configuration make-gitolite-configuration
  gitolite-configuration?
  (package        gitolite-configuration-package
                  (default gitolite))
  (user           gitolite-configuration-user
                  (default "git"))
  (group          gitolite-configuration-group
                  (default "git"))
  (home-directory gitolite-configuration-home-directory
                  (default "/var/lib/gitolite"))
  (rc-file        gitolite-configuration-rc-file
                  (default (gitolite-rc-file)))
  (git-config     gitolite-configuration-git-config
                  (default (gitolite-git-configuration)))
  (admin-pubkey   gitolite-configuration-admin-pubkey)
  (admin-name     gitolite-configuration-admin-name
                  (default #f)))

(define (gitolite-accounts config)
  (match-record config <gitolite-configuration>
                (user group home-directory)
    ;; User group and account to run Gitolite.
    (list (user-group
           (name group)
           (system? #t))
          (user-account
           (name user)
           (group group)
           (system? #t)
           (comment "Gitolite user")
           (home-directory home-directory)))))

(define (gitolite-activation config)
  (match-record config <gitolite-configuration>
                ( package user group home-directory rc-file admin-pubkey
                  admin-name git-config)
    #~(begin
        (use-modules (ice-9 match)
                     (guix build utils))

        (let* ((user-info (getpwnam #$user))
               (admin-pubkey #$admin-pubkey)
               (pubkey-file (if #$admin-name
                                (string-append #$admin-name ".pub")
                                (string-append
                                 #$home-directory "/"
                                 (basename
                                  (strip-store-file-name admin-pubkey)))))
               (rc-file #$(string-append home-directory "/.gitolite.rc")))

          ;; activate-users+groups in (gnu build activation) sets the
          ;; permission flags of home directories to #o700 and mentions that
          ;; services needing looser permissions should chmod it during
          ;; service activation.  We also want the git group to be able to
          ;; read from the gitolite home directory, so a chmod'ing we will
          ;; go!
          (chmod #$home-directory #o750)

          (simple-format #t "guix: gitolite: installing ~A\n" #$rc-file)
          (copy-file #$rc-file rc-file)
          ;; ensure gitolite's user can read the configuration
          (chown rc-file
                 (passwd:uid user-info)
                 (passwd:gid user-info))

          ;; The key must be writable, so copy it from the store
          (copy-file admin-pubkey pubkey-file)

          (chmod pubkey-file #o500)
          (chown pubkey-file
                 (passwd:uid user-info)
                 (passwd:gid user-info))

          ;; Set the git configuration, to avoid gitolite trying to use
          ;; the hostname command, as the network might not be up yet
          (copy-file #$git-config
                     #$(string-append home-directory "/.gitconfig"))

          ;; Run Gitolite setup, as this updates the hooks and include the
          ;; admin pubkey if specified. The admin pubkey is required for
          ;; initial setup, and will replace the previous key if run after
          ;; initial setup
          (match (primitive-fork)
            (0
             ;; Exit with a non-zero status code if an exception is thrown.
             (dynamic-wind
               (const #t)
               (lambda ()
                 (setenv "HOME" (passwd:dir user-info))
                 (setenv "USER" #$user)
                 (setgid (passwd:gid user-info))
                 (setuid (passwd:uid user-info))
                 (primitive-exit
                  (system* #$(file-append package "/bin/gitolite")
                           "setup"
                           "-m" "gitolite setup by GNU Guix"
                           "-pk" pubkey-file)))
               (lambda ()
                 (primitive-exit 1))))
            (pid (waitpid pid)))

          (when (file-exists? pubkey-file)
            (delete-file pubkey-file))))))

(define gitolite-service-type
  (service-type
   (name 'gitolite)
   (extensions
    (list (service-extension activation-service-type
                             gitolite-activation)
          (service-extension account-service-type
                             gitolite-accounts)))
   (description
    "Set up @command{gitolite}, a Git hosting tool providing access over SSH.
By default, the @code{git} user is used, but this is configurable.
Additionally, Gitolite can integrate with with tools like gitweb or cgit to
provide a web interface to view selected repositories.")))

;;;
;;; Gitile
;;;

(define-record-type* <gitile-configuration>
  gitile-configuration make-gitile-configuration gitile-configuration?
  (package gitile-configuration-package
           (default gitile))
  (host gitile-configuration-host
        (default "127.0.0.1"))
  (port gitile-configuration-port
        (default 8080))
  (database gitile-configuration-database
            (default "/var/lib/gitile/gitile-db.sql"))
  (repositories gitile-configuration-repositories
                (default "/var/lib/gitolite/repositories"))
  (git-owner-validation? gitile-configuration-git-owner-validation?
                         (default #t))
  (base-git-url gitile-configuration-base-git-url)
  (index-title gitile-configuration-index-title
               (default "Index"))
  (intro gitile-configuration-intro
         (default '()))
  (footer gitile-configuration-footer
          (default '()))
  (nginx gitile-configuration-nginx))

(define (gitile-config-file host port database repositories
                            git-owner-validation? base-git-url
                            index-title intro footer)
  (define build
    #~(write `(config
                (port #$port)
                (host #$host)
                (database #$database)
                (repositories #$repositories)
                (git-owner-validation? #$git-owner-validation?)
                (base-git-url #$base-git-url)
                (index-title #$index-title)
                (intro #$intro)
                (footer #$footer))
             (open-output-file #$output)))

  (computed-file "gitile.conf" build))

(define gitile-nginx-server-block
  (match-lambda
    (($ <gitile-configuration> package host port database repositories
        git-owner-validation? base-git-url index-title intro footer nginx)
     (list (nginx-server-configuration
             (inherit nginx)
             (locations
               (append
                 (list
                   (nginx-location-configuration
                            (uri "/")
                            (body
                              (list
                                #~(string-append "proxy_pass http://" #$host
                                                 ":" (number->string #$port)
                                                 "/;")))))
                 (map
                   (lambda (loc)
                     (nginx-location-configuration
                       (uri loc)
                       (body
                         (list
                           #~(string-append "root " #$package "/share/gitile/assets;")))))
                   '("/css" "/js" "/images"))
                 (nginx-server-configuration-locations nginx))))))))

(define gitile-shepherd-service
  (match-lambda
    (($ <gitile-configuration> package host port database repositories
        git-owner-validation? base-git-url index-title intro footer nginx)
     (list (shepherd-service
             (provision '(gitile))
             (requirement '(loopback))
             (documentation "gitile")
             (start (let ((gitile (file-append package "/bin/gitile")))
                          #~(make-forkexec-constructor
                              `(,#$gitile "-c" #$(gitile-config-file
                                                   host port database
                                                   repositories
                                                   git-owner-validation?
                                                   base-git-url index-title
                                                   intro footer))
                              #:user "gitile"
                              #:group "git")))
             (stop #~(make-kill-destructor)))))))

(define %gitile-accounts
  (list (user-group
         (name "git")
         (system? #t))
        (user-account
          (name "gitile")
          (group "git")
          (system? #t)
          (comment "Gitile user")
          (home-directory "/var/empty")
          (shell (file-append shadow "/sbin/nologin")))))

(define gitile-service-type
  (service-type
    (name 'gitile)
    (description "Run Gitile, a small Git forge.  Expose public repositories
on the web.")
    (extensions
      (list (service-extension account-service-type
                               (const %gitile-accounts))
            (service-extension shepherd-root-service-type
                               gitile-shepherd-service)
            (service-extension nginx-service-type
                               gitile-nginx-server-block)))))


;;;
;;; Fossil HTTP server.
;;;

(define (port-number? n)
  (and (integer? n)
       (> n 0)
       (< n (expt 2 16))))

(define (mode-number? n)
  (and (integer? n)
       (>= n 0)
       (<= n #o777)))

(define (fossil-js-mode? x)
  (and (memq x '(inline separate bundled))
       #t))

(define-maybe/no-serialization number)
(define-maybe/no-serialization string)
(define-maybe/no-serialization list-of-strings)
(define-maybe/no-serialization fossil-js-mode)

(define-configuration/no-serialization fossil-configuration
  (package (package fossil)
           "The Fossil package to use.")
  (user (string "fossil")
        "The user running the Fossil server.")
  (group (string "fossil")
         "The user group running the Fossil server.")
  (log-file (string "/var/log/fossil.log")
            "The path to the server's log.")
  (repository (string "/var/lib/fossil")
              "The name of the Fossil repository to be served, or a directory
containing one or more repositories with names ending in @code{.fossil}.

In the latter case, a prefix of the URL pathname is used
to search the directory for an appropriate repository.
Files not matching the pattern @code{*.fossil*}
will be served as static content.  Invoke @command{fossil server --help}
for more information.")
  (acme? (boolean #f)
         "Deliver files from the @code{.well-known} subdirectory.")
  (base-url maybe-string
            "The URL used as the base (useful for reverse proxies)")
  (chroot maybe-string
          "The directory to use for chroot instead of @code{repository}.")
  (ckout-alias maybe-string
               "The @var{name} for @code{/doc/@var{name}/...}
to be treated as @code{/doc/ckout/...}.")
  (compress? (boolean #t) "Compress HTTP response.")
  (create? (boolean #f)
           "Create a new @code{repository} if it does not already exist.")
  (error-log-file maybe-string "The path for HTTP error log.")
  (ext-root maybe-string "The document root for the /ext extension mechanism.")
  (files maybe-list-of-strings "The glob patterns for static files.")
  (from maybe-string
        "The path to be used as the diff baseline for the /ckout page.")
  (jail? (boolean #t)
         "Whether to enter the chroot jail after dropping root privileges.")
  (js-mode maybe-fossil-js-mode
           "How JavaScript is delivered with pages, either @code{'inline}
at the end of the HTML file, as @code{'separate} HTTP requests,
or one single HTTP request for all JavaScript @code{'bundled} together.

Depending on the needs of any given page, @code{'inline}
and @code{'bundled} modes might result in a single amalgamated script
or several, but both approaches result in fewer HTTP requests
than the @code{'separate} mode.")
  (https? (boolean #f)
          "Indicate that the requests are coming through a reverse proxy
that has already translated HTTPS into HTTP.")
  (ip maybe-string "The IP for the server to listen on.")
  (local-authentication? (boolean #f)
                         "Enable automatic login for requests from localhost.")
  (localhost? (boolean #f) "Listen on @code{127.0.0.1} only.")
  (main-menu maybe-string               ;TODO: structure
             "The file whose contents is to override
the repository's @code{mainmenu} setting.")
  (max-latency maybe-number
               "The maximum latency in seconds for a single HTTP request.")
  (port (port-number 8080) "The port number for the server to listen on.")
  (list-repositories? (boolean #f)
                      "If @code{repository} is dir, URL @code{/} lists repos.")
  (redirect-to-https? (boolean #t)
                      "If set to @code{#f}, do not force redirects to HTTPS
regardless of the repository setting @code{redirect-to-https}.")
  (scgi? (boolean #f) "Accept SCGI rather than HTTP.")
  (skin maybe-string "The skin label to use, overriding repository settings.")
  (socket-file maybe-string
               "The unix-domain socket to use instead of TCP/IP.")
  (socket-mode (mode-number #o640)
               "The file permissions to set for the unix socket.")
  (th-trace? (boolean #f)
             "Trace TH1 execution (for debugging purposes).")
  (tls-certificate maybe-string
                   "The certicate file (@file{fullchain.pem})
with which to enable TLS (HTTPS) encryption.")
  (tls-private-key maybe-string "The file storing the TLS private key."))

(define (fossil-accounts config)
  (match-record config <fossil-configuration> (user group)
    (list (user-group (name group)
                      (system? #t))
          (user-account (name user)
                        (group group)
                        (system? #t)
                        (comment "Fossil server user")
                        (home-directory "/var/empty")
                        (shell (file-append shadow "/sbin/nologin"))))))

(define (fossil-activation config)
  (match-record config <fossil-configuration> (user create? repository)
    (with-imported-modules '((guix build utils))
      #~(begin
          (use-modules (guix build utils))
          (let* ((pw (getpwnam #$user))
                 (uid (passwd:uid pw))
                 (gid (passwd:gid pw)))
            (unless #$create? (chown #$repository uid gid)))))))

(define (fossil-shepherd-service config)
  (match-record config <fossil-configuration>
                (package user group log-file repository acme? base-url
                 chroot ckout-alias compress? create? error-log-file ext-root
                 files from https? ip jail? js-mode list-repositories?
                 local-authentication? localhost? main-menu max-latency port
                 redirect-to-https? scgi? skin socket-file socket-mode
                 th-trace? tls-certificate tls-private-key)
    (shepherd-service
     (provision '(fossil))
     (requirement '(user-processes networking))
     (start #~(make-forkexec-constructor
               (list #$(file-append package "/bin/fossil")
                     "server"
                     #$@(if acme? '("--acme") '())
                     #$@(if (maybe-value-set? base-url)
                            (list "--baseurl" base-url)
                            '())
                     #$@(if (maybe-value-set? chroot)
                            (list "--chroot" chroot)
                            '())
                     #$@(if (maybe-value-set? ckout-alias)
                            (list "--ckout-alias" ckout-alias)
                            '())
                     #$@(if compress? '() '("--nocompress"))
                     #$@(if create? '("--create") '())
                     #$@(if (maybe-value-set? error-log-file)
                            (list "--errorlog" error-log-file)
                            '())
                     #$@(if (maybe-value-set? ext-root)
                            (list "--extroot" ext-root)
                            '())
                     #$@(if (maybe-value-set? files)
                            (list  "--files" (string-join files ","))
                            '())
                     #$@(if (maybe-value-set? from) (list "--from" from) '())
                     #$@(if https? '("--https") '())
                     #$@(if jail? '() '("--nojail"))
                     #$@(if (maybe-value-set? js-mode)
                            (list "--jsmode" (symbol->string js-mode))
                            '())
                     #$@(if local-authentication? '("--localauth") '())
                     #$@(if localhost? '("--localhost") '())
                     #$@(if (maybe-value-set? main-menu)
                            (list "--mainmenu" main-menu)
                            '())
                     #$@(if (maybe-value-set? max-latency)
                            (list "--max-latency"
                                  (number->string max-latency))
                            '())
                     #$@(if redirect-to-https? '() '("--nossl"))
                     #$@(if scgi? '("--scgi") '())
                     #$@(if list-repositories? '("--repolist") '())
                     #$@(if (maybe-value-set? skin) (list "--skin" skin) '())
                     #$@(if (maybe-value-set? socket-file)
                            (list "--socket-name" socket-file
                                  "--socket-mode" socket-mode
                                  "--socket-owner"
                                  (simple-format #f "~a:~a" user group))
                            (list "--port"
                                  (if (maybe-value-set? ip)
                                      (simple-format #f "~a:~a" ip port)
                                      (number->string port))))
                     #$@(if th-trace? '("--th-trace") '())
                     #$@(if (maybe-value-set? tls-certificate)
                            (list "--cert" tls-certificate)
                            '())
                     #$@(if (maybe-value-set? tls-private-key)
                            (list "--pkey" tls-private-key)
                            '())
                     "--user" #$user
                     #$repository)
               #:user #$user
               #:group #$group
               #:log-file #$log-file))
     (stop #~(make-kill-destructor))
     (documentation
      "Run the HTTP server
for the Fossil software configuration management system."))))

(define fossil-service-type
  (service-type
    (name 'fossil)
    (extensions
     (list (service-extension account-service-type fossil-accounts)
           (service-extension activation-service-type fossil-activation)
           (service-extension shepherd-root-service-type
                              (compose list fossil-shepherd-service))))
    (description
     "Run the HTTP server for the Fossil software configuration management
system.  In addition to distributed version control, Fossil also supports
bug tracking, wiki, forum, email alerts, chat, and technotes.")))
