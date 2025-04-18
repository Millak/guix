;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021, 2023 Andrew Tropin <andrew@trop.in>
;;; Copyright © 2021 Xinglu Chen <public@yoctocell.xyz>
;;; Copyright © 2021 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2021 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2022-2023 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2022 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2022 Antero Mejr <antero@mailbox.org>
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

(define-module (guix scripts home)
  #:use-module ((gnu services) #:hide (delete))
  #:autoload   (gnu packages base) (coreutils)
  #:autoload   (gnu packages bash) (bash)
  #:autoload   (gnu packages gnupg) (guile-gcrypt)
  #:autoload   (gnu packages shells) (fish gash zsh)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:autoload   (gnu home services shepherd) (home-shepherd-service-type
                                             home-shepherd-configuration-services
                                             shepherd-service-requirement)
  #:autoload   (guix modules) (source-module-closure)
  #:autoload   (gnu build linux-container) (call-with-container %namespaces)
  #:use-module ((gnu system) #:select (operating-system?
                                       operating-system-user-services))
  #:autoload   (gnu system linux-container) (eval/container)
  #:autoload   (gnu system file-systems) (file-system-mapping
                                          file-system-mapping-source
                                          file-system-mapping->bind-mount
                                          specification->file-system-mapping
                                          %network-file-mappings)
  #:autoload   (guix self) (make-config.scm)
  #:use-module (guix channels)
  #:use-module (guix derivations)
  #:use-module (guix ui)
  #:autoload   (guix colors) (supports-hyperlinks? file-hyperlink)
  #:use-module (guix packages)
  #:use-module (guix profiles)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:autoload   (guix graph) (lookup-backend export-graph)
  #:use-module (guix scripts)
  #:use-module (guix scripts package)
  #:use-module (guix scripts build)
  #:autoload   (guix scripts system search) (service-type->recutils)
  #:use-module (guix scripts system reconfigure)
  #:autoload   (guix scripts pull) (channel-commit-hyperlink)
  #:autoload   (guix scripts system) (service-node-type
                                      shepherd-service-node-type)
  #:autoload   (guix scripts home edit) (guix-home-edit)
  #:autoload   (guix scripts home import) (import-manifest)
  #:use-module ((guix status) #:select (with-status-verbosity))
  #:use-module ((guix build utils) #:select (mkdir-p switch-symlinks))
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (srfi srfi-37)
  #:use-module (srfi srfi-71)
  #:use-module (ice-9 match)
  #:export (guix-home))


;;;
;;; Options.
;;;

(define %user-module
  (make-user-module '((gnu home))))

(define %guix-home
  (string-append %profile-directory "/guix-home"))

(define (show-help)
  (display (G_ "Usage: guix home [OPTION ...] ACTION [ARG ...] [FILE]
Build the home environment declared in FILE according to ACTION.
Some ACTIONS support additional ARGS.\n"))
    (newline)
  (display (G_ "The valid values for ACTION are:\n"))
  (newline)
  (display (G_ "\
   search             search for existing service types\n"))
  (display (G_ "\
   edit               edit the definition of an existing service type\n"))
  (display (G_ "
   container          run the home environment configuration in a container\n"))
  (display (G_ "\
   reconfigure        switch to a new home environment configuration\n"))
  (display (G_ "\
   roll-back          switch to the previous home environment configuration\n"))
  (display (G_ "\
   describe           describe the current home environment\n"))
  (display (G_ "\
   list-generations   list the home environment generations\n"))
  (display (G_ "\
   switch-generation  switch to an existing home environment configuration\n"))
  (display (G_ "\
   delete-generations delete old home environment generations\n"))
  (display (G_ "\
   build              build the home environment without installing anything\n"))
  (display (G_ "\
   import             generates a home environment definition from dotfiles\n"))
  (display (G_ "\
   extension-graph    emit the service extension graph\n"))
  (display (G_ "\
   shepherd-graph     emit the graph of shepherd services\n"))

  (show-build-options-help)
  (display (G_ "
  -e, --expression=EXPR  consider the home-environment EXPR evaluates to
                         instead of reading FILE, when applicable"))
  (display (G_ "
      --allow-downgrades for 'reconfigure', allow downgrades to earlier
                         channel revisions"))
  (newline)
  (display (G_ "
  -N, --network          allow containers to access the network"))
  (display (G_ "
      --share=SPEC       for containers, share writable host file system
                         according to SPEC"))
  (display (G_ "
      --expose=SPEC      for containers, expose read-only host file system
                         according to SPEC"))
  (newline)
  (display (G_ "
  -v, --verbosity=LEVEL  use the given verbosity LEVEL"))
  (display (G_ "
      --graph-backend=BACKEND
                         use BACKEND for 'extension-graph' and 'shepherd-graph'"))
  (newline)
  (display (G_ "
  -I, --list-installed[=REGEXP]
                         for 'describe' or 'list-generations', list installed
                         packages matching REGEXP"))
  (newline)
  (display (G_ "
  -h, --help             display this help and exit"))
  (display (G_ "
  -V, --version          display version information and exit"))
  (newline)
  (show-bug-report-information))

(define (verbosity-level opts)
  "Return the verbosity level based on OPTS, the alist of parsed options."
  (or (assoc-ref opts 'verbosity)
      (if (eq? (assoc-ref opts 'action) 'build)
          3 1)))

(define %options
  ;; Specification of the command-line options.
  (cons* (option '(#\h "help") #f #f
                 (lambda args
                   (leave-on-EPIPE (show-help))
                   (exit 0)))
         (option '(#\n "dry-run") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'dry-run? #t result)))
         (option '(#\V "version") #f #f
                 (lambda args
                   (show-version-and-exit "guix home")))
         (option '(#\v "verbosity") #t #f
                 (lambda (opt name arg result)
                   (let ((level (string->number* arg)))
                     (alist-cons 'verbosity level
                                 (alist-delete 'verbosity result)))))
         (option '(#\e "expression") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'expression arg result)))
         (option '("allow-downgrades") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'validate-reconfigure
                               warn-about-backward-reconfigure
                               result)))
         (option '("graph-backend") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'graph-backend arg result)))
         (option '(#\I "list-installed") #f #t
                 (lambda (opt name arg result)
                   (alist-cons 'list-installed (or arg "") result)))

         ;; Container options.
         (option '(#\N "network") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'network? #t result)))
         (option '("share") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'file-system-mapping
                               (specification->file-system-mapping arg #t)
                               result)))
         (option '("expose") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'file-system-mapping
                               (specification->file-system-mapping arg #f)
                               result)))

         %standard-build-options))

(define %default-options
  `((graft? . #t)
    (substitutes? . #t)
    (offload? . #t)
    (print-build-trace? . #t)
    (print-extended-build-trace? . #t)
    (multiplexed-build-output? . #t)
    (verbosity . #f)                              ;default
    (debug . 0)
    (validate-reconfigure . ,ensure-forward-reconfigure)
    (graph-backend . "graphviz")))


;;;
;;; Container.
;;;

(define not-config?
  ;; Select (guix …) and (gnu …) modules, except (guix config).
  (match-lambda
    (('guix 'config) #f)
    (('guix _ ...) #t)
    (('gnu _ ...) #t)
    (_ #f)))

(define (user-shell)
  (match (and=> (or (getenv "SHELL")
                    (passwd:shell (getpwuid (getuid))))
                basename)
    ("zsh"  (file-append zsh "/bin/zsh"))
    ("fish" (file-append fish "/bin/fish"))
    ("gash" (file-append gash "/bin/gash"))
    (_      (file-append bash "/bin/bash"))))

(define %default-system-profile
  ;; The "system" profile available when running 'guix home container'.  The
  ;; activation script currently expects to run "env -0" (XXX), so provide
  ;; Coreutils by default.
  (delay (profile
          (name "home-system-profile")
          (content (packages->manifest (list coreutils))))))

(define* (spawn-home-container home
                               #:key
                               network?
                               (command '())
                               (mappings '())
                               (system-profile
                                (force %default-system-profile)))
  "Spawn a login shell within a container running HOME, a home environment.
When COMMAND is a non-empty list, execute it in the container and exit
immediately.  Return the exit status of the process in the container."
  (define passwd (getpwuid (getuid)))
  (define home-directory (or (getenv "HOME") (passwd:dir passwd)))
  (define host (gethostname))
  (define uid 1000)
  (define gid 1000)
  (define user-name (passwd:name passwd))
  (define user-real-name (passwd:gecos passwd))

  (define (optional-mapping mapping)
    (and (file-exists? (file-system-mapping-source mapping))
         mapping))

  (define network-mappings
    (if network?
        (filter-map optional-mapping %network-file-mappings)
        '()))

  (eval/container
   (with-extensions (list guile-gcrypt)
     (with-imported-modules `(((guix config) => ,(make-config.scm))
                              ,@(source-module-closure
                                 '((gnu build accounts)
                                   (guix profiles)
                                   (guix build utils)
                                   (guix build syscalls))
                                 #:select? not-config?))
       #~(begin
           (use-modules (guix build utils)
                        (gnu build accounts)
                        ((guix build syscalls)
                         #:select (set-network-interface-up)))

           (define shell
             #$(user-shell))

           (define term
             #$(getenv "TERM"))

           (define passwd
             (password-entry
              (name #$user-name)
              (real-name #$user-real-name)
              (uid #$uid) (gid #$gid) (shell shell)
              (directory #$home-directory)))

           (define groups
             (list (group-entry (name "users") (gid #$gid))
                   (group-entry (gid 65534)       ;the overflow GID
                                (name "overflow"))))

           ;; (guix profiles) loads (guix utils), which calls 'getpw' from the
           ;; top level.  Thus, arrange so that it's loaded after /etc/passwd
           ;; has been created.
           (module-autoload! (current-module)
                             '(guix profiles) '(load-profile))

           ;; Create /etc/passwd for applications that need it, such as mcron.
           (mkdir-p "/etc")
           (write-passwd (list passwd))
           (write-group groups)

           (unless #$network?
             ;; When isolated from the network, provide a minimal /etc/hosts
             ;; to resolve "localhost".
             (call-with-output-file "/etc/hosts"
               (lambda (port)
                 (display "127.0.0.1 localhost\n" port)
                 (chmod port #o444))))

           ;; Create /tmp; bits of code expect it, such as
           ;; 'least-authority-wrapper'.
           (mkdir-p "/tmp")

           ;; Set PATH for things that the activation script might expect, such
           ;; as "env".
           (load-profile #$system-profile)

           (mkdir-p #$home-directory)
           (setenv "HOME" #$home-directory)
           (setenv "GUIX_NEW_HOME" #$home)
           (primitive-load (string-append #$home "/activate"))
           (setenv "GUIX_NEW_HOME" #f)

           (when term
             ;; Preserve TERM for proper interactive use.
             (setenv "TERM" term))

           (chdir #$home-directory)

           ;; Invoke SHELL with argv[0] starting with "-": that's how shells
           ;; figure out that they are login shells!
           (execl shell (string-append "-" (basename shell))
                  #$@(match command
                       (() #~())
                       ((_ ...)
                        #~("-c" #$(string-join command))))))))

   #:namespaces (if network?
                    (delq 'net %namespaces)       ; share host network
                    %namespaces)
   #:mappings (append network-mappings mappings)
   #:guest-uid uid
   #:guest-gid gid))


;;;
;;; Actions.
;;;

(define* (export-extension-graph home port
                                 #:key (backend (lookup-backend "graphviz")))
  "Export the service extension graph of HOME to PORT using BACKEND."
  (let* ((services (home-environment-services home))
         (home     (find (lambda (service)
                           (eq? (service-kind service) home-service-type))
                         services)))
    (export-graph (list home) port
                  #:backend backend
                  #:node-type (service-node-type services)
                  #:reverse-edges? #t)))

(define* (export-shepherd-graph home port
                                #:key (backend (lookup-backend "graphviz")))
  "Export the graph of shepherd services of HOME to PORT using BACKEND."
  (let* ((services  (home-environment-services home))
         (root      (fold-services services
                                   #:target-type home-shepherd-service-type))
         ;; Get the list of <shepherd-service>.
         (shepherds (home-shepherd-configuration-services
                     (service-value root)))
         (sinks     (filter (lambda (service)
                              (null? (shepherd-service-requirement service)))
                            shepherds)))
    (export-graph sinks port
                  #:backend backend
                  #:node-type (shepherd-service-node-type shepherds)
                  #:reverse-edges? #t)))

(define* (perform-action action he
                         #:key
                         dry-run?
                         derivations-only?
                         use-substitutes?
                         (graph-backend "graphviz")
                         (validate-reconfigure ensure-forward-reconfigure)

                         ;; Container options.
                         (file-system-mappings '())
                         (container-command '())
                         network?)
  "Perform ACTION for home environment. "

  (ensure-profile-directory)
  (define println
    (cut format #t "~a~%" <>))

  (when (eq? action 'reconfigure)
    (check-forward-update validate-reconfigure
                          #:current-channels (home-provenance %guix-home)))

  (case action
    ((extension-graph)
     (export-extension-graph he (current-output-port)
                             #:backend (lookup-backend graph-backend)))
    ((shepherd-graph)
     (export-shepherd-graph he (current-output-port)
                            #:backend (lookup-backend graph-backend)))
    (else
     (mlet* %store-monad
         ((he-drv   (home-environment-derivation he))
          (drvs     (mapm/accumulate-builds lower-object (list he-drv)))
          (%        (if derivations-only?
                        (return
                         (for-each (compose println derivation-file-name) drvs))
                        (built-derivations drvs)))

          (he-out-path -> (derivation->output-path he-drv)))
       (if (or dry-run? derivations-only?)
           (return #f)
           (case action
             ((reconfigure)
              (let* ((number (generation-number %guix-home))
                     (generation (generation-file-name
                                  %guix-home (+ 1 number))))

                (switch-symlinks generation he-out-path)
                (switch-symlinks %guix-home generation)
                (setenv "GUIX_NEW_HOME" he-out-path)
                (primitive-load (string-append he-out-path "/activate"))
                (setenv "GUIX_NEW_HOME" #f)
                (return he-out-path)))
             ((container)
              (mlet %store-monad ((status (spawn-home-container
                                           he
                                           #:network? network?
                                           #:mappings file-system-mappings
                                           #:command
                                           container-command)))
                (match (status:exit-val status)
                  (0 (return #t))
                  ((? integer? n) (return (exit n)))
                  (#f
                   (if (status:term-sig status)
                       (leave (G_ "process terminated with signal ~a~%")
                              (status:term-sig status))
                       (leave (G_ "process stopped with signal ~a~%")
                              (status:stop-sig status)))))))
             (else
              (for-each (compose println derivation->output-path) drvs)
              (return he-out-path))))))))

(define (process-action action args opts)
  "Process ACTION, a sub-command, with the arguments are listed in ARGS.
ACTION must be one of the sub-commands that takes a home environment
declaration as an argument (a file name.)  OPTS is the raw alist of options
resulting from command-line parsing."
  (define (ensure-home-environment file-or-exp obj)
    (let* ((username
            (or (getenv "USER")
                (passwd:name (getpwnam (getuid)))))
           (os-home-env-config
            (and (operating-system? obj)
                 (and=> (find (lambda (%service)
                                (eq? (service-type-name (service-kind %service))
                                     'guix-home))
                              (operating-system-user-services obj))
                        service-value)))
           (os-home-env
            (and os-home-env-config
                 (and=> (find (lambda (home-env-config)
                                (string=? (first home-env-config) username))
                              os-home-env-config)
                        second)))
           (home-env
            (or os-home-env obj)))
      (unless (home-environment? home-env)
        (if (operating-system? obj)
            (leave (G_ "'~a' does not contain a home environment for user '~a'~%")
                   file-or-exp username)
            (leave (G_ "'~a' does not return a home environment~%")
                   file-or-exp)))
      home-env))

  (let* ((file   (match args
                   (() #f)
                   ((x . _) x)))
         (expr   (assoc-ref opts 'expression))
         (system (assoc-ref opts 'system))

         (transform   (lambda (obj)
                        (home-environment-with-provenance obj file)))

         (home-environment
          (transform
           (ensure-home-environment
            (or file expr)
            (cond
             ((and expr file)
              (leave
               (G_ "both file and expression cannot be specified~%")))
             (expr
              (read/eval expr))
             (file
              (load* file %user-module
                     #:on-error (assoc-ref opts 'on-error)))
             (else
              (leave (G_ "no configuration specified~%")))))))

         (mappings    (filter-map (match-lambda
                                    (('file-system-mapping . mapping) mapping)
                                    (_ #f))
                                  opts))
         (dry?        (assoc-ref opts 'dry-run?)))

    (with-store store
      (set-build-options-from-command-line store opts)
      (with-build-handler (build-notifier #:use-substitutes?
                                          (assoc-ref opts 'substitutes?)
                                          #:verbosity
                                          (verbosity-level opts)
                                          #:dry-run?
                                          (assoc-ref opts 'dry-run?))

        (run-with-store store
          (mbegin %store-monad
            (set-guile-for-build (default-guile))

            (perform-action action home-environment
                            #:dry-run? dry?
                            #:derivations-only? (assoc-ref opts 'derivations-only?)
                            #:use-substitutes? (assoc-ref opts 'substitutes?)
                            #:validate-reconfigure
                            (assoc-ref opts 'validate-reconfigure)
                            #:graph-backend
                            (assoc-ref opts 'graph-backend)
                            #:network? (assoc-ref opts 'network?)
                            #:file-system-mappings mappings
                            #:container-command
                            (or (assoc-ref opts 'container-command) '()))))))
    (warn-about-disk-space)))


(define (process-command command args opts)
  "Process COMMAND, one of the 'guix home' sub-commands.  ARGS is its
argument list and OPTS is the option alist."
  (define-syntax-rule (with-store* store exp ...)
    (with-store store
      (set-build-options-from-command-line store opts)
      exp ...))
  (case command
    ;; The following commands do not need to use the store, and they do not need
    ;; an home environment file.
    ((search)
     (apply search args))
    ((edit)
     (apply guix-home-edit args))
    ((import)
     (let* ((profiles (delete-duplicates
                       (match (filter-map (match-lambda
                                            (('profile . p) p)
                                            (_              #f))
                                          opts)
                         (() (list %current-profile))
                         (lst (reverse lst)))))
            (manifest (concatenate-manifests
                       (map profile-manifest profiles)))
            (destination (match args
                           ((destination) destination)
                           (_ (leave (G_ "wrong number of arguments~%"))))))
       (unless (file-exists? destination)
         (mkdir-p destination))
       (call-with-output-file
           (string-append destination "/home-configuration.scm")
         (cut import-manifest manifest destination <>))
       (info (G_ "'~a' populated with all the Home configuration files~%")
             destination)
       (display-hint (G_ "\
Run @command{guix home reconfigure ~a/home-configuration.scm} to effectively
deploy the home environment described by these files.\n")
                     destination)))
    ((describe)
     (let ((list-installed-regex (assoc-ref opts 'list-installed)))
       (match (generation-number %guix-home)
         (0
          (leave (G_ "no home environment generation, nothing to describe~%")))
         (generation
          (display-home-environment-generation
           generation #:list-installed-regex list-installed-regex)))))
    ((list-generations)
     (let ((list-installed-regex (assoc-ref opts 'list-installed))
           (pattern (match args
                      (() #f)
                      ((pattern) pattern)
                      (x (leave (G_ "wrong number of arguments~%"))))))
       (list-generations pattern #:list-installed-regex list-installed-regex)))
    ((switch-generation)
     (let ((pattern (match args
                      ((pattern) pattern)
                      (x (leave (G_ "wrong number of arguments~%"))))))
       (with-store* store
                    (switch-to-home-environment-generation store pattern))))
    ((roll-back)
     (let ((pattern (match args
                      (() "")
                      (x (leave (G_ "wrong number of arguments~%"))))))
       (with-store* store
                    (roll-back-home-environment store))))
    ((delete-generations)
     (let ((pattern (match args
                      (() #f)
                      ((pattern) pattern)
                      (x (leave (G_ "wrong number of arguments~%"))))))
       (with-store*
        store
        (delete-matching-generations store %guix-home pattern))))
    (else (process-action command args opts))))

(define-command (guix-home . args)
  (synopsis "build and deploy home environments")

  (define (parse-sub-command arg result)
    ;; Parse sub-command ARG and augment RESULT accordingly.
    (if (assoc-ref result 'action)
        (alist-cons 'argument arg result)
        (let ((action (string->symbol arg)))
          (case action
            ((build
              reconfigure
              extension-graph shepherd-graph
              list-generations describe
              delete-generations roll-back
              switch-generation search edit
              import container)
             (alist-cons 'action action result))
            (else (leave (G_ "~a: unknown action~%") action))))))

  (define (match-pair car)
    ;; Return a procedure that matches a pair with CAR.
    (match-lambda
      ((head . tail)
       (and (eq? car head) tail))
      (_ #f)))

  (define (option-arguments opts)
    ;; Extract the plain arguments from OPTS.
    (let* ((args   (reverse (filter-map (match-pair 'argument) opts)))
           (count  (length args))
           (action (assoc-ref opts 'action))
           (expr   (assoc-ref opts 'expression)))
      (define (fail)
        (leave (G_ "wrong number of arguments for action '~a'~%")
               action))

      (unless action
        (format (current-error-port)
                (G_ "guix home: missing command name~%"))
        (format (current-error-port)
                (G_ "Try 'guix home --help' for more information.~%"))
        (exit 1))

      (case action
        ((build reconfigure)
         (unless (or (= count 1)
                     (and expr (= count 0)))
           (fail)))
        ((init)
         (unless (= count 2)
           (fail))))
      args))

  (define (parse-args args)
    ;; Parse the list of command line arguments ARGS.

    ;; The '--' token is used to separate the command to run from the rest of
    ;; the operands.
    (let* ((args rest (break (cut string=? "--" <>) args))
           (opts (parse-command-line args %options (list %default-options)
                                     #:argument-handler
                                     parse-sub-command)))
      (match rest
        (() opts)
        (("--") opts)
        (("--" command ...)
         (match (assoc-ref opts 'action)
           ('container
            (alist-cons 'container-command command opts))
           (_
            (leave (G_ "~a: extraneous command~%")
                   (string-join command))))))))

  (with-error-handling
    (let* ((opts     (parse-args args))
           (args     (option-arguments opts))
           (command  (assoc-ref opts 'action)))
      (parameterize ((%graft? (assoc-ref opts 'graft?)))
        (with-status-verbosity (verbosity-level opts)
          (process-command command args opts))))))


;;;
;;; Searching.
;;;

(define service-type-name*
  (compose symbol->string service-type-name))

(define (service-type-description-string type)
  "Return the rendered and localised description of TYPE, a service type."
  (and=> (service-type-description type)
         (compose texi->plain-text G_)))

(define %service-type-metrics
  ;; Metrics used to estimate the relevance of a search result.
  `((,service-type-name* . 3)
    (,service-type-description-string . 2)
    (,(lambda (type)
        (match (and=> (service-type-location type) location-file)
          ((? string? file)
           (basename file ".scm"))
          (#f
           "")))
     . 1)))

(define (find-service-types regexps)
  "Return a list of service type/score pairs: service types whose name or
description matches REGEXPS sorted by relevance, and their score."
  (let ((matches (fold-home-service-types
                  (lambda (type result)
                    (match (relevance type regexps
                                      %service-type-metrics)
                      ((? zero?)
                       result)
                      (score
                       (cons (cons type score) result))))
                  '())))
    (sort matches
          (lambda (m1 m2)
            (match m1
              ((type1 . score1)
               (match m2
                 ((type2 . score2)
                  (if (= score1 score2)
                      (string>? (service-type-name* type1)
                                (service-type-name* type2))
                      (> score1 score2))))))))))

(define (search . args)
  (with-error-handling
    (let* ((regexps (map (cut make-regexp* <> regexp/icase) args))
           (matches (find-service-types regexps)))
      (leave-on-EPIPE
       (display-search-results matches (current-output-port)
                               #:print service-type->recutils
                               #:regexps regexps
                               #:command "guix home search")))))


;;;
;;; Generations.
;;;

(define* (display-home-environment-generation
          number
          #:optional (profile %guix-home)
          #:key (list-installed-regex #f))
  "Display a summary of home-environment generation NUMBER in a human-readable
format.  List packages in that home environment that match
LIST-INSTALLED-REGEX."
  (define (display-channel channel)
    (format #t     "    ~a:~%" (channel-name channel))
    (format #t (G_ "      repository URL: ~a~%") (channel-url channel))
    (when (channel-branch channel)
      (format #t (G_ "      branch: ~a~%") (channel-branch channel)))
    (format #t (G_ "      commit: ~a~%")
            (if (supports-hyperlinks?)
                (channel-commit-hyperlink channel)
                (channel-commit channel))))

  (unless (zero? number)
    (let* ((generation  (generation-file-name profile number)))
      (define-values (channels config-file)
        ;; The function will work for home environments too, we just
        ;; need to keep provenance file.
        (system-provenance generation))

      (display-generation profile number)
      (format #t (G_ "  file name: ~a~%") generation)
      (format #t (G_ "  canonical file name: ~a~%") (readlink* generation))
      ;; TRANSLATORS: Please preserve the two-space indentation.

      (unless (null? channels)
        ;; TRANSLATORS: Here "channel" is the same terminology as used in
        ;; "guix describe" and "guix pull --channels".
        (format #t (G_ "  channels:~%"))
        (for-each display-channel channels))
      (when config-file
        (format #t (G_ "  configuration file: ~a~%")
                (if (supports-hyperlinks?)
                    (file-hyperlink config-file)
                    config-file)))
      (when list-installed-regex
        (format #t (G_ "  packages:\n"))
        (pretty-print-table (list-installed
                             list-installed-regex
                             (list (string-append generation "/profile")))
                            #:left-pad 4)))))

(define* (list-generations pattern #:optional (profile %guix-home)
                           #:key (list-installed-regex #f))
  "Display in a human-readable format all the home environment generations
matching PATTERN, a string.  When PATTERN is #f, display all the home
environment generations.  List installed packages that match
LIST-INSTALLED-REGEX."
  (cond ((not (file-exists? profile))             ; XXX: race condition
         (raise (condition (&profile-not-found-error
                            (profile profile)))))
        ((not pattern)
         (for-each (cut display-home-environment-generation <>
                        #:list-installed-regex list-installed-regex)
                   (profile-generations profile)))
        ((matching-generations pattern profile)
         =>
         (lambda (numbers)
           (if (null-list? numbers)
               (exit 1)
               (leave-on-EPIPE (for-each
                                (cut display-home-environment-generation <>
                                     #:list-installed-regex list-installed-regex)
                                numbers)))))))


;;;
;;; Switch generations.
;;;

;; TODO: Make it public in (guix scripts system)
(define-syntax-rule (unless-file-not-found exp)
  (catch 'system-error
    (lambda ()
      exp)
    (lambda args
      (if (= ENOENT (system-error-errno args))
          #f
          (apply throw args)))))

(define (switch-to-home-environment-generation store spec)
  "Switch the home-environment profile to the generation specified by
SPEC.  STORE is an open connection to the store."
  (let* ((number (relative-generation-spec->number %guix-home spec))
         (generation (generation-file-name %guix-home number))
         (activate (string-append generation "/activate")))
    (if number
        (begin
          (setenv "GUIX_NEW_HOME" (readlink generation))
          (switch-to-generation* %guix-home number)
          (unless-file-not-found (primitive-load activate))
          (setenv "GUIX_NEW_HOME" #f))
        (leave (G_ "cannot switch to home environment generation '~a'~%") spec))))


;;;
;;; Roll-back.
;;;

(define (roll-back-home-environment store)
  "Roll back the home-environment profile to its previous generation.
STORE is an open connection to the store."
  (switch-to-home-environment-generation store "-1"))
