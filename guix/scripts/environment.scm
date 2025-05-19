;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015, 2018 David Thompson <davet@gnu.org>
;;; Copyright © 2015-2025 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2018 Mike Gerwitz <mtg@gnu.org>
;;; Copyright © 2022, 2023 John Kehayias <john.kehayias@protonmail.com>
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

(define-module (guix scripts environment)
  #:use-module (guix ui)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module ((guix status) #:select (with-status-verbosity))
  #:use-module (guix derivations)
  #:use-module (guix packages)
  #:use-module (guix profiles)
  #:use-module (guix search-paths)
  #:use-module (guix build utils)
  #:use-module (guix monads)
  #:use-module ((guix gexp) #:select (lower-object))
  #:autoload   (guix describe) (current-profile current-channels)
  #:autoload   (guix channels) (guix-channel? channel-commit)
  #:use-module (guix scripts)
  #:use-module (guix scripts build)
  #:autoload   (guix scripts pack) (symlink-spec-option-parser)
  #:use-module (guix transformations)
  #:autoload   (ice-9 ftw) (scandir)
  #:autoload   (gnu build install) (evaluate-populate-directive)
  #:autoload   (gnu build linux-container) (call-with-container %namespaces
                                            user-namespace-supported?
                                            unprivileged-user-namespace-supported?
                                            setgroups-supported?)
  #:autoload   (gnu build accounts) (password-entry group-entry
                                     password-entry-name password-entry-directory
                                     write-passwd write-group)
  #:autoload   (guix build syscalls) (set-network-interface-up openpty login-tty)
  #:use-module (gnu system file-systems)
  #:autoload   (gnu packages) (specification->package+output)
  #:autoload   (gnu packages bash) (bash)
  #:autoload   (gnu packages bootstrap) (bootstrap-executable %bootstrap-guile)
  #:autoload   (gnu packages package-management) (guix)
  #:use-module (ice-9 match)
  #:autoload   (ice-9 rdelim) (read-line)
  #:use-module (ice-9 vlist)
  #:autoload   (web uri) (string->uri uri-scheme)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-37)
  #:use-module (srfi srfi-98)
  #:export (assert-container-features
            load-manifest
            guix-environment
            guix-environment*
            show-environment-options-help
            (%options . %environment-options)
            (%default-options . %environment-default-options)))

(define %default-shell
  (or (getenv "SHELL") "/bin/sh"))

(define* (show-search-paths profile manifest #:key pure?)
  "Display the search paths of MANIFEST applied to PROFILE.  When PURE? is #t,
do not augment existing environment variables with additional search paths."
  (for-each (match-lambda
              ((search-path . value)
               (display
                (search-path-definition search-path value
                                        #:kind (if pure? 'exact 'prefix)))
               (newline)))
            (profile-search-paths profile manifest)))

(define (show-environment-options-help)
  "Print help about options shared between 'guix environment' and 'guix
shell'."
  (display (G_ "
  -e, --expression=EXPR  create environment for the package that EXPR
                         evaluates to"))
  (display (G_ "
  -m, --manifest=FILE    create environment with the manifest from FILE"))
  (display (G_ "
  -p, --profile=PATH     create environment from profile at PATH"))
  (display (G_ "
      --check            check if the shell clobbers environment variables"))
  (display (G_ "
      --pure             unset existing environment variables"))
  (display (G_ "
  -E, --preserve=REGEXP  preserve environment variables that match REGEXP"))
  (display (G_ "
      --search-paths     display needed environment variable definitions"))
  (display (G_ "
  -r, --root=FILE        make FILE a symlink to the result, and register it
                         as a garbage collector root"))
  (display (G_ "
  -C, --container        run command within an isolated container"))
  (display (G_ "
  -N, --network          allow containers to access the network"))
  (display (G_ "
  -P, --link-profile     link environment profile to ~/.guix-profile within
                         an isolated container"))
  (display (G_ "
  -W, --nesting          make Guix available within the container"))
  (display (G_ "
  -u, --user=USER        instead of copying the name and home of the current
                         user into an isolated container, use the name USER
                         with home directory /home/USER"))
  (display (G_ "
      --no-cwd           do not share current working directory with an
                         isolated container"))
  (display (G_ "
      --writable-root    make the container's root file system writable"))

  (display (G_ "
      --share=SPEC       for containers, share writable host file system
                         according to SPEC"))
  (display (G_ "
      --expose=SPEC      for containers, expose read-only host file system
                         according to SPEC"))
  (display (G_ "
  -S, --symlink=SPEC     for containers, add symlinks to the profile according
                         to SPEC, e.g. \"/usr/bin/env=bin/env\"."))
  (display (G_ "
  -v, --verbosity=LEVEL  use the given verbosity LEVEL"))
  (display (G_ "
      --bootstrap        use bootstrap binaries to build the environment")))

(define (show-help)
  (display (G_ "Usage: guix environment [OPTION]... PACKAGE... [-- COMMAND...]
Build an environment that includes the dependencies of PACKAGE and execute
COMMAND or an interactive shell in that environment.\n"))
  (warning (G_ "This command is deprecated in favor of 'guix shell'.\n"))
  (newline)

  ;; These two options are left out in 'guix shell'.
  (display (G_ "
  -l, --load=FILE        create environment for the package that the code within
                         FILE evaluates to"))
  (display (G_ "
      --ad-hoc           include all specified packages in the environment instead
                         of only their inputs"))

  (show-environment-options-help)
  (newline)
  (show-build-options-help)
  (newline)
  (show-native-build-options-help)
  (newline)
  (show-transformation-options-help)
  (newline)
  (display (G_ "
  -h, --help             display this help and exit"))
  (display (G_ "
  -V, --version          display version information and exit"))
  (newline)
  (show-bug-report-information))

(define %default-options
  `((system . ,(%current-system))
    (substitutes? . #t)
    (symlinks . ())
    (offload? . #t)
    (graft? . #t)
    (print-build-trace? . #t)
    (print-extended-build-trace? . #t)
    (multiplexed-build-output? . #t)
    (debug . 0)
    (verbosity . 1)))

(define (tag-package-arg opts arg)
  "Return a two-element list with the form (TAG ARG) that tags ARG with either
'ad-hoc' in OPTS has the 'ad-hoc?' key set to #t, or 'inputs' otherwise."
  ;; Normally, the transitive inputs to a package are added to an environment,
  ;; but the ad-hoc? flag changes the meaning of a package argument such that
  ;; the package itself is added to the environment instead.
  (if (assoc-ref opts 'ad-hoc?)
      `(ad-hoc-package ,arg)
      `(package ,arg)))

(define %options
  ;; Specification of the command-line options.
  (cons* (option '(#\h "help") #f #f
                 (lambda args
                   (show-help)
                   (exit 0)))
         (option '(#\V "version") #f #f
                 (lambda args
                   (show-version-and-exit "guix environment")))
         (option '("check") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'check? #t result)))
         (option '("pure") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'pure #t result)))
         (option '(#\E "preserve") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'inherit-regexp
                               (make-regexp* arg)
                               result)))
         (option '("inherit") #t #f               ;deprecated
                 (lambda (opt name arg result)
                   (warning (G_ "'--inherit' is deprecated, \
use '--preserve' instead~%"))
                   (alist-cons 'inherit-regexp
                               (make-regexp* arg)
                               result)))
         (option '("search-paths") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'search-paths #t result)))
         (option '(#\l "load") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'load
                               (tag-package-arg result arg)
                               result)))
         (option '(#\e "expression") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'expression
                               (tag-package-arg result arg)
                               result)))
         (option '(#\m "manifest") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'manifest
                               arg
                               result)))
         (option '("ad-hoc") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'ad-hoc? #t result)))
         (option '(#\n "dry-run") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'dry-run? #t result)))
         (option '(#\C "container") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'container? #t result)))
         (option '(#\N "network") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'network? #t result)))
         (option '(#\W "nesting") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'nesting? #t result)))
         (option '(#\P "link-profile") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'link-profile? #t result)))
         (option '(#\p "profile") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'profile arg
                               (alist-delete 'profile result eq?))))
         (option '(#\u "user") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'user arg
                               (alist-delete 'user result eq?))))
         (option '("no-cwd") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'no-cwd? #t result)))
         (option '("writable-root") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'writable-root? #t result)))
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
         (option '(#\S "symlink") #t #f
                 (lambda (opt name arg result)
                   ;; Delay call to avoid auto-loading (guix scripts pack)
                   ;; when unnecessary.
                   (symlink-spec-option-parser opt name arg result)))
         (option '(#\r "root") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'gc-root arg result)))
         (option '(#\v "verbosity") #t #f
                 (lambda (opt name arg result)
                   (let ((level (string->number* arg)))
                     (alist-cons 'verbosity level
                                 (alist-delete 'verbosity result)))))
         (option '("bootstrap") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'bootstrap? #t result)))

         (append %transformation-options
                 %standard-build-options
                 %standard-native-build-options)))

(define (pick-all alist key)
  "Return a list of values in ALIST associated with KEY."
  (define same-key? (cut eq? key <>))

  (fold (lambda (pair memo)
          (match pair
            (((? same-key? k) . v)
             (cons v memo))
            (_ memo)))
        '() alist))

(define (load-manifest file)                      ;TODO: factorize
  "Load the user-profile manifest (Scheme code) from FILE and return it."
  (let ((user-module (make-user-module '((guix profiles) (gnu)))))
    (load* file user-module)))

(define (options/resolve-packages store opts)
  "Return OPTS with package specification strings replaced by manifest entries
for the corresponding packages."
  (define system
    (assoc-ref opts 'system))

  (define (manifest-entry=? e1 e2)
    (and (eq? (manifest-entry-item e1) (manifest-entry-item e2))
         (string=? (manifest-entry-output e1)
                   (manifest-entry-output e2))))

  (define transform
    (options->transformation opts))

  (define* (package->manifest-entry* package #:optional (output "out"))
    (package->manifest-entry (transform package) output))

  (define (packages->outputs packages mode)
    (match packages
      ((? package? package)
       (if (eq? mode 'ad-hoc-package)
           (list (package->manifest-entry* package))
           (manifest-entries (package->development-manifest package system))))
      (((? package? package) (? string? output))
       (if (eq? mode 'ad-hoc-package)
           (list (package->manifest-entry* package output))
           (manifest-entries (package->development-manifest package system))))
      ((lst ...)
       (append-map (cut packages->outputs <> mode) lst))))

  (manifest
   (delete-duplicates
    (append-map (match-lambda
                  (('package 'ad-hoc-package (? string? spec))
                   (let-values (((package output)
                                 (specification->package+output spec)))
                     (list (package->manifest-entry* package output))))
                  (('package 'package (? string? spec))
                   (manifest-entries
                    (package->development-manifest
                     (transform (specification->package+output spec))
                     system)))
                  (('expression mode str)
                   ;; Add all the outputs of the package STR evaluates to.
                   (packages->outputs (read/eval str) mode))
                  (('load mode file)
                   ;; Add all the outputs of the package defined in FILE.
                   (let ((module (make-user-module '())))
                     (packages->outputs (load* file module) mode)))
                  (('manifest . file)
                   (manifest-entries (load-manifest file)))
                  (('nesting? . #t)
                   (if (assoc-ref opts 'profile)
                       '()
                       (let ((profile (and=> (current-profile) readlink*)))
                         (if (or (not profile) (not (store-path? profile)))
                             (begin
                               (warning (G_ "\
could not add current Guix to the profile~%"))
                               '())
                             (list (manifest-entry
                                     (name "guix")
                                     (version
                                      (or (any (lambda (channel)
                                                 (and (guix-channel? channel)
                                                      (channel-commit channel)))
                                               (current-channels))
                                          "0"))
                                     (item profile)
                                     (search-paths
                                      (package-native-search-paths guix))))))))
                  (_ '()))
                opts)
    manifest-entry=?)))

(define (manifest->derivation manifest system bootstrap?)
  "Return the derivation for a profile of MANIFEST.
BOOTSTRAP? specifies whether to use the bootstrap Guile to build the profile."
  (profile-derivation manifest
                      #:system system

                      ;; Packages can have conflicting inputs, or explicit
                      ;; inputs that conflict with implicit inputs (e.g., gcc,
                      ;; gzip, etc.).  Thus, do not error out when we
                      ;; encounter collision.
                      #:allow-collisions? #t

                      #:hooks (if bootstrap?
                                  '()
                                  %default-profile-hooks)
                      #:locales? (not bootstrap?)))

(define requisites* (store-lift requisites))

(define (inputs->requisites inputs)
  "Convert INPUTS, a list of input tuples or store path strings, into a set of
requisite store items i.e. the union closure of all the inputs."
  (define (input->requisites input)
    (requisites*
     (match input
       ((drv output)
        (list (derivation->output-path drv output)))
       ((drv)
        (list (derivation->output-path drv)))
       ((? direct-store-path? path)
        (list path)))))

  (mlet %store-monad ((reqs (mapm %store-monad
                                  input->requisites inputs)))
    (return (delete-duplicates (concatenate reqs)))))

(define (setup-fhs profile)
  "Setup the FHS container by creating and linking expected directories from
PROFILE (other bind mounts are done in LAUNCH-ENVIRONMENT/CONTAINER),
providing a symlink for CC if GCC is in the container PROFILE, and writing
/etc/ld.so.conf."
  ;; Additional symlinks for an FHS container.
  (define fhs-symlinks
    `(("/lib" . "/usr/lib")
      ,(if (target-64bit?)
           '("/lib" . "/lib64")
           '("/lib" . "/lib32"))
      ("/bin" . "/usr/bin")
      ("/sbin" . "/usr/sbin")))

  ;; A procedure to symlink the contents (at the top level) of a directory,
  ;; excluding the directory itself and parent, along with any others provided
  ;; in EXCLUDE.
  (define* (link-contents dir #:key (exclude '()))
    (for-each (lambda (file)
                (symlink (string-append profile dir "/" file)
                         (string-append dir "/" file)))
              (scandir (string-append profile dir)
                       (negate (cut member <>
                                    (append exclude '("." ".." )))))))

  ;; The FHS container sets up the expected filesystem through MAPPINGS with
  ;; FHS-MAPPINGS (in LAUNCH-ENVIRONMENT/CONTAINER), the symlinks through
  ;; FHS-SYMLINKS, and linking the contents of PROFILE/bin and PROFILE/etc
  ;; using LINK-CONTENTS, as these both have or will have contents for a
  ;; non-FHS container so must be handled separately.
  (mkdir-p "/usr")
  (for-each (lambda (link)
              (if (file-exists? (car link))
                  (symlink (car link) (cdr link))))
            fhs-symlinks)
  (link-contents "/bin" #:exclude '("sh"))
  (mkdir-p "/etc")
  (link-contents "/etc")

  ;; Provide a frequently expected 'cc' symlink to gcc (in case it is in
  ;; PROFILE), though this could also be done by the user in the container,
  ;; e.g. in $HOME/.local/bin and adding that to $PATH.  Note: we do this in
  ;; /bin since that already has the sh symlink and the other (optional) FHS
  ;; bin directories will link to /bin.
  (let ((gcc-path (string-append profile "/bin/gcc")))
    (when (file-exists? gcc-path)
      (catch 'system-error
        (lambda ()
          (symlink gcc-path "/bin/cc"))
        (lambda args
          ;; If /bin/cc already exists because it was provided by another
          ;; package in PROFILE, such as 'clang-toolchain', leave it.
          (unless (= EEXIST (system-error-errno args))
            (apply throw args))))))

  ;; Guix's ldconfig doesn't search in FHS default locations, so provide a
  ;; minimal ld.so.conf.
  (call-with-output-file "/etc/ld.so.conf"
    (lambda (port)
      (for-each (lambda (directory)
                  (display directory port)
                  (newline port))
                ;; /lib/nss is needed as Guix's nss puts libraries
                ;; there rather than in the lib directory.
                '("/lib" "/lib/nss"))))

  ;; Create /etc/ld.so.cache.
  (invoke "/sbin/ldconfig" "-X"))

(define (status->exit-code status)
  "Compute the exit code made from STATUS, a value as returned by 'waitpid',
and suitable for 'exit'."
  ;; See <bits/waitstatus.h>.
  (or (status:exit-val status)
      (logior #x80 (status:term-sig status))))

(define exit/status (compose exit status->exit-code))
(define primitive-exit/status (compose primitive-exit status->exit-code))

(define* (launch-environment command profile manifest
                             #:key pure? (white-list '())
                             emulate-fhs?)
  "Load the environment of PROFILE, which corresponds to MANIFEST, and execute
COMMAND.  When PURE?, pre-existing environment variables are cleared before
setting the new ones, except those matching the regexps in WHITE-LIST.  When
EMULATE-FHS?, first set up an FHS environment with $PATH and generate the LD
cache."
  ;; Properly handle SIGINT, so pressing C-c in an interactive terminal
  ;; application works.
  (sigaction SIGINT SIG_DFL)
  ;; Restore original action for SIGPIPE.
  (sigaction SIGPIPE SIG_DFL)
  (load-profile profile manifest
                #:pure? pure? #:white-list-regexps white-list)

  ;; Give users a way to know that they're in 'guix environment', so they can
  ;; adjust 'PS1' accordingly, for instance.  Set it to PROFILE so users can
  ;; conveniently access its contents.
  (setenv "GUIX_ENVIRONMENT" profile)

  (match command
    ((program . args)
     (catch 'system-error
       (lambda ()
         (when emulate-fhs?
           ;; When running in a container with EMULATE-FHS?, augment $PATH
           ;; (optional, but to better match FHS expectations), and generate
           ;; /etc/ld.so.cache.
           (setenv "PATH" (string-append "/bin:/usr/bin:/sbin:/usr/sbin"
                                         (if (getenv "PATH")
                                             (string-append ":" (getenv "PATH"))
                                             ""))))
         (apply execlp program program args))
       (lambda _
         ;; Report the error from here because the parent process cannot
         ;; distinguish between the conventional 127 exit code and a process
         ;; that exited with 127 for other reasons (e.g., "sh -c xyz").
         (report-error (G_ "~a: command not found~%") program)
         (suggest-command-name profile command)

         ;; Following established convention, exit with 127 (aka. EX_NOTFOUND)
         ;; upon ENOENT.
         (primitive-_exit 127))))))

(define (child-shell-environment shell profile manifest)
  "Create a child process, load PROFILE and MANIFEST, and then run SHELL in
interactive mode in it.  Return a name/value vhash for all the variables shown
by running 'set' in the shell."
  (define-values (controller inferior)
    (openpty))

  (define script
    ;; Script to obtain the list of environment variable values.  On a POSIX
    ;; shell we can rely on 'set', but on fish we have to use 'env' (fish's
    ;; 'set' truncates values and prints them in a different format.)
    "env || /usr/bin/env || set; echo GUIX-CHECK-DONE; read x; exit\n")

  (define lines
    (match (primitive-fork)
      (0
       (catch #t
         (lambda ()
           (load-profile profile manifest #:pure? #t)

           ;; Mark the terminal as "unknown" do avoid ANSI escape codes such
           ;; as bracketed paste that would mess up the output of the script.
           (setenv "TERM" "")

           (setenv "GUIX_ENVIRONMENT" profile)
           (close-fdes controller)
           (login-tty inferior)
           (execl shell shell))
         (lambda _
           (primitive-exit 127))))
      (pid
       (close-fdes inferior)
       (let* ((port   (fdopen controller "r+l"))
              (result (begin
                        (display script port)
                        (let loop ((lines '()))
                          (match (read-line port)
                            ((? eof-object?) (reverse lines))
                            ("GUIX-CHECK-DONE\r"
                             (display "done\n" port)
                             (reverse lines))
                            (line
                             ;; Drop the '\r' from LINE.
                             (loop (cons (string-drop-right line 1)
                                         lines))))))))
         (close-port port)
         (waitpid pid)
         result))))

  (fold (lambda (line table)
          ;; Note: 'set' in fish outputs "NAME VALUE" instead of "NAME=VALUE"
          ;; but it also truncates values anyway, so don't try to support it.
          (let ((index (string-index line #\=)))
            (if index
                (vhash-cons (string-take line index)
                            (string-drop line (+ 1 index))
                            table)
                table)))
        vlist-null
        lines))

(define* (validate-child-shell-environment profile manifest
                                           #:optional (shell %default-shell))
  "Run SHELL in interactive mode in an environment for PROFILE and MANIFEST
and report clobbered environment variables."
  (define warned? #f)
  (define-syntax-rule (warn exp ...)
    (begin
      (set! warned? #t)
      (warning exp ...)))

  (info (G_ "checking the environment variables visible from shell '~a'...~%")
        shell)
  (let ((actual (child-shell-environment shell profile manifest)))
    (when (vlist-null? actual)
      (leave (G_ "failed to determine environment of shell '~a'~%")
             shell))
    (for-each (match-lambda
                ((spec . expected)
                 (let ((name (search-path-specification-variable spec)))
                   (match (vhash-assoc name actual)
                     (#f
                      (warn (G_ "variable '~a' is missing from shell \
environment~%")
                            name))
                     ((_ . actual)
                      (cond ((string=? expected actual)
                             #t)
                            ((string-prefix? expected actual)
                             (warn (G_ "variable '~a' has unexpected \
suffix '~a'~%")
                                   name
                                   (string-drop actual
                                                (string-length expected))))
                            (else
                             (warn (G_ "variable '~a' is clobbered: '~a'~%")
                                   name actual))))))))
              (profile-search-paths profile manifest))

    ;; Special case.
    (match (vhash-assoc "GUIX_ENVIRONMENT" actual)
      (#f
       (warn (G_ "'GUIX_ENVIRONMENT' is missing from the shell \
environment~%")))
      ((_ . value)
       (unless (string=? value profile)
         (warn (G_ "'GUIX_ENVIRONMENT' is set to '~a' instead of '~a'~%")
               value profile))))

    ;; Check the prompt unless we have more important warnings.
    (unless warned?
      (match (vhash-assoc "PS1" actual)
        (#f #f)
        ((_ . str)
         (when (and (getenv "PS1") (string=? str (getenv "PS1"))

                    ;; 'PS1' might be conditional on 'GUIX_ENVIRONMENT', as
                    ;; shown in the hint below.
                    (not (or (string-contains str "$GUIX_ENVIRONMENT")
                             (string-contains str "${GUIX_ENVIRONMENT"))))
           (warning (G_ "'PS1' is the same in sub-shell~%"))
           (display-hint (G_ "Consider setting a different prompt for
environment shells to make them distinguishable.

If you are using Bash, you can do that by adding these lines to
@file{~/.bashrc}:

@example
PS1='\\u@@\\h \\w${GUIX_ENVIRONMENT:+ [env]}\\$ '
@end example
"))))))

    (if warned?
        (begin
          (display-hint (G_ "One or more environment variables have a
different value in the shell than the one we set.  This means that you may
find yourself running code in an environment different from the one you asked
Guix to prepare.

This usually indicates that your shell startup files are unexpectedly
modifying those environment variables.  For example, if you are using Bash,
make sure that environment variables are set or modified in
@file{~/.bash_profile} and @emph{not} in @file{~/.bashrc}.  For more
information on Bash startup files, run:

@example
info \"(bash) Bash Startup Files\"
@end example

Alternatively, you can avoid the problem by passing the @option{--container}
or @option{-C} option.  That will give you a fully isolated environment
running in a \"container\", immune to the issue described above."))
          (exit 1))
        (info (G_ "All is good!  The shell gets correct environment \
variables.~%")))))

(define (suggest-command-name profile command)
  "COMMAND was not found in PROFILE so display a hint suggesting the closest
command name."
  (define not-dot?
    (match-lambda
      ((or "." "..") #f)
      (_ #t)))

  (match (scandir (string-append profile "/bin") not-dot?)
    ((or #f ()) #f)
    (available
     (match command
       ((executable _ ...)
        ;; Look for a suggestion with a high threshold: a suggestion is
        ;; usually better than no suggestion.
        (let ((closest (string-closest executable available
                                       #:threshold 12)))
          (unless (or (not closest) (string=? closest executable))
            (display-hint (G_ "Did you mean '~a'?~%")
                          closest))))))))

(define* (launch-environment/fork command profile manifest
                                  #:key pure? (white-list '()))
  "Run COMMAND in a new process with an environment containing PROFILE, with
the search paths specified by MANIFEST.  When PURE?, pre-existing environment
variables are cleared before setting the new ones, except those matching the
regexps in WHITE-LIST."
  (match (primitive-fork)
    (0 (launch-environment command profile manifest
                           #:pure? pure?
                           #:white-list white-list))
    (pid (match (waitpid pid)
           ((_ . status)
            status)))))

(define* (launch-environment/container #:key command bash user user-mappings
                                       profile manifest link-profile? network?
                                       map-cwd? emulate-fhs? nesting?
                                       writable-root?
                                       (setup-hook #f)
                                       (symlinks '()) (white-list '()))
  "Run COMMAND within a container that features the software in PROFILE.
Environment variables are set according to the search paths of MANIFEST.  The
global shell is BASH, a file name for a GNU Bash binary in the store.  When
NETWORK?, access to the host system network is permitted.  USER-MAPPINGS, a
list of file system mappings, contains the user-specified host file systems to
mount inside the container.  If USER is not #f, each target of USER-MAPPINGS
will be re-written relative to '/home/USER', and USER will be used for the
passwd entry.

When EMULATE-FHS?, set up the container to follow the Filesystem Hierarchy
Standard and provide a glibc that reads the cache from /etc/ld.so.cache.
SETUP-HOOK is an additional setup procedure to be called, currently only used
with the EMULATE-FHS? option.

When NESTING? is true, share all the store with the container and add Guix to
its profile, allowing its use from within the container.

LINK-PROFILE? creates a symbolic link from ~/.guix-profile to the
environment profile.

SYMLINKS must be a list of (SOURCE -> TARGET) tuples denoting symlinks to be
added to the container.

Preserve environment variables whose name matches the one of the regexps in
WHILE-LIST."
  (define tmpfs
    (file-system
      (device "none")
      (mount-point "/tmp")
      (type "tmpfs")
      (check? #f)))

  (define (optional-mapping->fs mapping)
    (and (file-exists? (file-system-mapping-source mapping))
         (file-system-mapping->bind-mount mapping)))

  ;; File system mappings for an FHS container, where the entire directory can
  ;; be mapped.  Others (bin and etc) will already have contents and need to
  ;; use LINK-CONTENTS (defined in SETUP-FHS) to symlink the directory
  ;; contents.
  (define fhs-mappings
    (map (lambda (mapping)
           (file-system-mapping
            (source (string-append profile (car mapping)))
            (target (cdr mapping))))
         '(("/lib"     . "/lib")
           ("/include" . "/usr/include")
           ("/sbin"    . "/sbin")
           ("/libexec" . "/usr/libexec")
           ("/share"   . "/usr/share"))))

  (define (nesting-mappings)
    ;; Files shared with the host when enabling nesting.
    (cons* (file-system-mapping
            (source (%store-prefix))
            (target source))
           (file-system-mapping
            (source (cache-directory))
            (target source)
            (writable? #t))
           (let ((uri (string->uri (%daemon-socket-uri))))
             (if (or (not uri) (eq? 'file (uri-scheme uri)))
                 (list (file-system-mapping
                        (source (%daemon-socket-uri))
                        (target source)))
                 '()))))

  (mlet %store-monad ((reqs (if nesting?
                                (return '())
                                (inputs->requisites
                                 (list (direct-store-path bash) profile)))))
    (return
     (let* ((cwd      (getcwd))
            (home     (getenv "HOME"))
            (uid      (if user 1000 (getuid)))
            (gid      (if user 1000 (getgid)))

            ;; On a foreign distro, the name service switch might be
            ;; dysfunctional and 'getpwuid' throws.  Don't let that hamper
            ;; operations.
            (passwd   (let ((pwd (false-if-exception (getpwuid (getuid)))))
                        (password-entry
                         (name (or user
                                   (and=> pwd passwd:name)
                                   (getenv "USER")
                                   "charlie"))
                         (real-name (if (or user (not pwd))
                                        ""
                                        (passwd:gecos pwd)))
                         (uid uid) (gid gid) (shell bash)
                         (directory (if (or user (not pwd))
                                        (string-append "/home/" name)
                                        (passwd:dir pwd))))))
            (groups   (list (group-entry (name "users") (gid gid))
                            (group-entry (gid 65534) ;the overflow GID
                                         (name "overflow"))))
            (home-dir (password-entry-directory passwd))
            (logname  (password-entry-name passwd))
            (environ  (filter (match-lambda
                                ((variable . value)
                                 (find (cut regexp-exec <> variable)
                                       white-list)))
                              (get-environment-variables)))
            ;; Bind-mount all requisite store items, user-specified mappings,
            ;; /bin/sh, the current working directory, and possibly networking
            ;; configuration files within the container.
            (mappings
             (append
              (override-user-mappings
               user home
               (append
                ;; Share current working directory, unless asked not to.
                (if map-cwd?
                    (list (file-system-mapping
                           (source cwd)
                           (target cwd)
                           (writable? #t)))
                    '())
                ;; Add the user mappings *after* the current working directory
                ;; so that a user can layer bind mounts on top of it.
                user-mappings))
              ;; Mappings for the union closure of all inputs.
              (map (lambda (dir)
                     (file-system-mapping
                      (source dir)
                      (target dir)
                      (writable? #f)))
                   reqs)))
            (file-systems (append %container-file-systems
                                  (list tmpfs        ; RW /tmp
                                        (file-system ; RW /run
                                          (inherit tmpfs)
                                          (mount-point
                                           (string-append "/run/user/"
                                                          (number->string uid))))
                                        (file-system ; RW ~
                                          (device "none")
                                          (mount-point
                                           (or (and=> user user-override-home)
                                               home))
                                          (type "tmpfs")
                                          (check? #f)))
                                  (if network?
                                      (filter-map optional-mapping->fs
                                                  %network-file-mappings)
                                      '())
                                  (if emulate-fhs?
                                      (filter-map optional-mapping->fs
                                                  fhs-mappings)
                                      '())
                                  (if nesting?
                                      (filter-map optional-mapping->fs
                                                  (nesting-mappings))
                                      '())
                                  (map file-system-mapping->bind-mount
                                       mappings))))

       ;; Trigger autoload now: the child process may lack (gnu build install)
       ;; in its file system view.
       (identity evaluate-populate-directive)

       (exit/status
        (call-with-container file-systems
          (lambda ()
            ;; Set a reasonable default PS1.
            (setenv "PS1" "\\u@\\h \\w [env]\\$ ")

            (for-each (lambda (var)
                        (setenv var "/tmp"))
                      ;; The same variables as in Nix's 'build.cc'.
                      '("TMPDIR" "TEMPDIR" "TMP" "TEMP"))

            ;; Some programs expect USER and/or LOGNAME to be set.
            (setenv "LOGNAME" logname)
            (setenv "USER" logname)

            (setenv "HOME" home-dir)

            ;; For convenience, start in the user's current working
            ;; directory or, if unmapped, the home directory.
            (chdir (if map-cwd?
                       (override-user-dir user home cwd)
                       home-dir))

            ;; Set environment variables that match WHITE-LIST.
            (for-each (match-lambda
                        ((variable . value)
                         (setenv variable value)))
                      environ)

            (primitive-exit/status
             ;; A container's environment is already purified, so no need to
             ;; request it be purified again.
             (launch-environment command
                                 (if link-profile?
                                     (string-append home-dir "/.guix-profile")
                                     profile)
                                 manifest #:pure? #f
                                 #:emulate-fhs? emulate-fhs?)))
          #:populate-file-system
          (lambda ()
            ;; Setup global shell.
            (mkdir-p "/bin")
            (symlink bash "/bin/sh")

            ;; Setup directory for temporary files.
            (mkdir-p "/tmp")

            ;; Create a dummy home directory.
            (mkdir-p home-dir)

            ;; Create symlinks.
            (let ((symlink->directives
                   (match-lambda
                     ((source '-> target)
                      `((directory ,(dirname source))
                        (,source -> ,(string-append profile "/" target)))))))
              (for-each (cut evaluate-populate-directive <> ".")
                        (append-map symlink->directives symlinks)))

            ;; If requested, link $GUIX_ENVIRONMENT to $HOME/.guix-profile;
            ;; this allows programs expecting that path to continue working as
            ;; expected within a container.
            (when link-profile? (link-environment profile home-dir))

            ;; Create a dummy /etc/passwd to satisfy applications that demand
            ;; to read it, such as 'git clone' over SSH, a valid use-case when
            ;; sharing the host's network namespace.
            (mkdir-p "/etc")
            (write-passwd (list passwd))
            (write-group groups)

            ;; Call an additional setup procedure, if provided.
            (when setup-hook
              (setup-hook profile)))
          #:guest-uid uid
          #:guest-gid gid
          #:writable-root? writable-root?
          #:namespaces (if network?
                           (delq 'net %namespaces) ; share host network
                           %namespaces)))))))

(define (user-override-home user)
  "Return home directory for override user USER."
  (string-append "/home/" user))

(define (file-name-equal-or-under? file-name directory)
  "Is @var{file-name} equal to or under @var{directory}?"
  (or (string=? directory file-name)
      (and (string-prefix? directory file-name)
           (char=? #\/ (string-ref file-name (string-length directory))))))

(define (override-user-mappings user home mappings)
  "If a username USER is provided, rewrite each HOME prefix in file system
mappings MAPPINGS to a home directory determined by 'override-user-dir';
otherwise, return MAPPINGS."
  (if (not user)
      mappings
      (map (lambda (mapping)
             (let ((target (file-system-mapping-target mapping)))
               (if (file-name-equal-or-under? target home)
                   (file-system-mapping
                    (inherit mapping)
                    (target (override-user-dir user home target)))
                   mapping)))
           mappings)))

(define (override-user-dir user home dir)
  "If username USER is provided, overwrite string prefix HOME in DIR with a
directory determined by 'user-override-home'; otherwise, return DIR."
  (if (and user (file-name-equal-or-under? dir home))
      (string-append (user-override-home user)
                     (substring dir (string-length home)))
      dir))

(define (link-environment profile home-dir)
  "Create a symbolic link from HOME-DIR/.guix-profile to PROFILE."
  (let ((profile-dir (string-append home-dir "/.guix-profile")))
    (catch 'system-error
      (lambda ()
        (symlink profile profile-dir))
      (lambda args
        (if (= EEXIST (system-error-errno args))
            (leave (G_ "cannot link profile: '~a' already exists within container~%")
                   profile-dir)
            (apply throw args))))))

(define (environment-bash container? bootstrap? system)
  "Return a monadic value in the store monad for the version of GNU Bash
needed in the environment for SYSTEM, if any.  If CONTAINER? is #f, return #f.
If CONTAINER? and BOOTSTRAP?, return the store path for the bootstrap Bash.
Otherwise, return the derivation for the Bash package."
  (with-monad %store-monad
    (cond
     ((and container? (not bootstrap?))
      (package->derivation bash))
     ;; Use the bootstrap Bash instead.
     ((and container? bootstrap?)
      (lower-object (bootstrap-executable "bash" system)))
     (else
      (return #f)))))

(define (parse-args args)
  "Parse the list of command line arguments ARGS."
  (define (handle-argument arg result)
    (alist-cons 'package (tag-package-arg result arg) result))

  ;; The '--' token is used to separate the command to run from the rest of
  ;; the operands.
  (let-values (((args command) (break (cut string=? "--" <>) args)))
    (let ((opts (parse-command-line args %options (list %default-options)
                                    #:argument-handler handle-argument)))
      (match command
        (() opts)
        (("--") opts)
        (("--" command ...) (alist-cons 'exec command opts))))))

(define (assert-container-features)
  "Check if containers can be created and exit with an informative error
message if any test fails."
  (unless (user-namespace-supported?)
    (report-error (G_ "cannot create container: user namespaces unavailable\n"))
    (leave (G_ "is your kernel version < 3.10?\n")))

  (unless (unprivileged-user-namespace-supported?)
    (report-error (G_ "cannot create container: unprivileged user cannot create user namespaces\n"))
    (leave (G_ "please set /proc/sys/kernel/unprivileged_userns_clone to \"1\"\n")))

  (unless (setgroups-supported?)
    (report-error (G_ "cannot create container: /proc/self/setgroups does not exist\n"))
    (leave (G_ "is your kernel version < 3.19?\n"))))

(define (register-gc-root target root)
  "Make ROOT an indirect root to TARGET.  This is procedure is idempotent."
  (let* ((root (if (string-prefix? "/" root)
                   root
                   (string-append (canonicalize-path (dirname root))
                                  "/" (basename root)))))
    (catch 'system-error
      (lambda ()
        (symlink target root)
        ((store-lift add-indirect-root) root))
      (lambda args
        (if (and (= EEXIST (system-error-errno args))
                 (equal? (false-if-exception (readlink root)) target))
            (with-monad %store-monad
              (return #t))
            (apply throw args))))))


;;;
;;; Entry point.
;;;

(define-command (guix-environment . args)
  (category development)
  (synopsis "spawn one-off software environments (deprecated)")

  (with-error-handling
    (guix-environment* (parse-args args))))

(define (guix-environment* opts)
  "Run the 'guix environment' command on OPTS, an alist resulting for
command-line option processing with 'parse-command-line'."
  (let* ((pure?        (assoc-ref opts 'pure))
         (container?   (assoc-ref opts 'container?))
         (link-prof?   (assoc-ref opts 'link-profile?))
         (symlinks     (assoc-ref opts 'symlinks))
         (network?     (assoc-ref opts 'network?))
         (no-cwd?      (assoc-ref opts 'no-cwd?))
         (writable-root? (assoc-ref opts 'writable-root?))
         (emulate-fhs? (assoc-ref opts 'emulate-fhs?))
         (nesting?     (assoc-ref opts 'nesting?))
         (user         (assoc-ref opts 'user))
         (bootstrap?   (assoc-ref opts 'bootstrap?))
         (system       (assoc-ref opts 'system))
         (profile      (assoc-ref opts 'profile))
         (command  (or (assoc-ref opts 'exec)
                       ;; Spawn a shell if the user didn't specify
                       ;; anything in particular.
                       (if container?
                           ;; The user's shell is likely not available
                           ;; within the container.
                           '("/bin/sh")
                           (list %default-shell))))
         (mappings   (pick-all opts 'file-system-mapping))
         (white-list (pick-all opts 'inherit-regexp)))

    (define store-needed?
      ;; Whether connecting to the daemon is needed.
      (or container? (not profile)))

    (define-syntax-rule (with-store/maybe store exp ...)
      ;; Evaluate EXP... with STORE bound to a connection, unless
      ;; STORE-NEEDED? is false, in which case STORE is bound to #f.
      (let ((proc (lambda (store) exp ...)))
        (parameterize ((%graft? (assoc-ref opts 'graft?)))
          (if store-needed?
              (with-store s
                (set-build-options-from-command-line s opts)
                (with-build-handler (build-notifier #:use-substitutes?
                                                    (assoc-ref opts 'substitutes?)
                                                    #:verbosity
                                                    (assoc-ref opts 'verbosity)
                                                    #:dry-run?
                                                    (assoc-ref opts 'dry-run?))
                  (proc s)))
              (proc #f)))))

    (when container? (assert-container-features))

    (when (not container?)
      (when link-prof?
        (leave (G_ "'--link-profile' cannot be used without '--container'~%")))
      (when user
        (leave (G_ "'--user' cannot be used without '--container'~%")))
      (when no-cwd?
        (leave (G_ "--no-cwd cannot be used without '--container'~%")))
      (when writable-root?
        (leave (G_ "'--writable-root' cannot be used without '--container'~%")))
      (when emulate-fhs?
        (leave (G_ "'--emulate-fhs' cannot be used without '--container'~%")))
      (when nesting?
        (leave (G_ "'--nesting' cannot be used without '--container'~%")))
      (when (pair? symlinks)
        (leave (G_ "'--symlink' cannot be used without '--container'~%"))))

    (with-status-verbosity (assoc-ref opts 'verbosity)
      (with-store/maybe store
        (define manifest-from-opts
          (options/resolve-packages store opts))

        (define manifest
          (if profile
              (profile-manifest profile)
              manifest-from-opts))

        (when (and profile
                   (> (length (manifest-entries manifest-from-opts)) 0))
          (leave (G_ "'--profile' cannot be used with package options~%")))

        (when (null? (manifest-entries manifest))
          (warning (G_ "no packages specified; creating an empty environment~%")))

        ;; Use the bootstrap Guile when requested.
        (parameterize ((%guile-for-build
                        (and store-needed?
                             (package-derivation
                              store
                              (if bootstrap?
                                  %bootstrap-guile
                                  (default-guile))
                              system))))
          (run-with-store store
            ;; Containers need a Bourne shell at /bin/sh.
            (mlet* %store-monad ((bash       (environment-bash container?
                                                               bootstrap?
                                                               system))
                                 (prof-drv   (if profile
                                                 (return #f)
                                                 (manifest->derivation
                                                  manifest system bootstrap?)))
                                 (profile -> (if profile
                                                 (readlink* profile)
                                                 (derivation->output-path prof-drv)))
                                 (gc-root -> (assoc-ref opts 'gc-root)))

              ;; First build the inputs.  This is necessary even for
              ;; --search-paths.  Additionally, we might need to build bash for
              ;; a container.
              (mbegin %store-monad
                (mwhen store-needed?
                  (built-derivations (append
                                      (if prof-drv (list prof-drv) '())
                                      (if (derivation? bash) (list bash) '()))))
                (mwhen gc-root
                  (register-gc-root profile gc-root))

                (mwhen (assoc-ref opts 'check?)
                  (return
                   (if container?
                       (warning (G_ "'--check' is unnecessary \
when using '--container'; doing nothing~%"))
                       (validate-child-shell-environment profile manifest))))

                (cond
                 ((assoc-ref opts 'search-paths)
                  (show-search-paths profile manifest #:pure? pure?)
                  (return #t))
                 (container?
                  (let ((bash-binary
                         (if bootstrap?
                             (derivation->output-path bash)
                             (string-append (derivation->output-path bash)
                                            "/bin/sh"))))
                    (launch-environment/container #:command command
                                                  #:bash bash-binary
                                                  #:user user
                                                  #:user-mappings mappings
                                                  #:profile profile
                                                  #:manifest manifest
                                                  #:white-list white-list
                                                  #:link-profile? link-prof?
                                                  #:network? network?
                                                  #:map-cwd? (not no-cwd?)
                                                  #:writable-root? writable-root?
                                                  #:emulate-fhs? emulate-fhs?
                                                  #:nesting? nesting?
                                                  #:symlinks symlinks
                                                  #:setup-hook
                                                  (and emulate-fhs?
                                                       setup-fhs))))

                 (else
                  (return
                   (exit/status
                    (launch-environment/fork command profile manifest
                                             #:white-list white-list
                                             #:pure? pure?)))))))))))))

;;; Local Variables:
;;; eval: (put 'with-store/maybe 'scheme-indent-function 1)
;;; End:
