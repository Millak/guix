;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013-2020, 2022-2023, 2025 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2020 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2020, 2023 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2020 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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

(define-module (gnu system shadow)
  #:use-module ((guix diagnostics) #:select (formatted-message))
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (guix modules)
  #:use-module (guix sets)
  #:use-module (guix ui)
  #:use-module ((gnu build accounts)
                #:select (%subordinate-id-count
                          %subordinate-id-max
                          %subordinate-id-min))
  #:use-module (gnu system accounts)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module ((gnu system file-systems)
                #:select (%tty-gid))
  #:use-module ((gnu packages admin)
                #:select (shadow))
  #:use-module (gnu packages bash)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)

  ;; Re-export these bindings for backward compatibility.
  #:re-export (user-account
               user-account?
               user-account-name
               user-account-password
               user-account-uid
               user-account-group
               user-account-supplementary-groups
               user-account-comment
               user-account-home-directory
               user-account-create-home-directory?
               user-account-shell
               user-account-system?

               user-group
               user-group?
               user-group-name
               user-group-password
               user-group-id
               user-group-system?)

  #:export (%default-bashrc
            %default-bash-profile
            %default-zprofile
            %default-xdefaults
            %default-gdbinit
            %default-nanorc
            %default-dotguile
            %default-skeleton-home-config
            default-skeletons
            skeleton-directory
            %base-groups
            %base-user-accounts

            account-service-type
            account-service

            subids-configuration
            subids-configuration?
            subids-configuration-add-root?
            subids-configuration-subgids
            subids-configuration-subuids

            subids-extension
            subids-extension?
            subids-extension-subgids
            subids-extension-subuids

            subids-service-type))

;;; Commentary:
;;;
;;; Utilities for configuring the Shadow tool suite ('login', 'passwd', etc.)
;;;
;;; Code:

;; Change the default shell used by new <user-account> records.
(default-shell (file-append bash "/bin/bash"))

(define %base-groups
  ;; Default set of groups.
  (let-syntax ((system-group (syntax-rules ()
                               ((_ args ...)
                                (user-group (system? #t) args ...)))))
    (list (system-group (name "root") (id 0))
          (system-group (name "wheel"))           ; root-like users
          (system-group (name "users"))           ; normal users
          (system-group (name "nogroup"))         ; for daemons etc.

          ;; The following groups are conventionally used by things like udev to
          ;; control access to hardware devices.
          (system-group (name "tty") (id %tty-gid))
          (system-group (name "dialout"))
          (system-group (name "kmem"))
          (system-group (name "input"))           ; input devices, from udev
          (system-group (name "video"))
          (system-group (name "audio"))
          (system-group (name "netdev"))          ; used in avahi-dbus.conf
          (system-group (name "lp"))
          (system-group (name "disk"))
          (system-group (name "floppy"))
          (system-group (name "cdrom"))
          (system-group (name "tape"))
          (system-group (name "kvm"))          ; for /dev/kvm
          (system-group (name "sgx")))))

(define %base-user-accounts
  ;; List of standard user accounts.  Note that "root" is a special case, so
  ;; it's not listed here.
  (list (user-account
         (name "nobody")
         (uid 65534)
         (group "nogroup")
         (shell (file-append shadow "/sbin/nologin"))
         (home-directory "/nonexistent")
         (create-home-directory? #f)
         (system? #t))))

(define %default-bashrc
  (plain-file "bashrc" "\
# Bash initialization for interactive non-login shells and
# for remote shells (info \"(bash) Bash Startup Files\").

# Export 'SHELL' to child processes.  Programs such as 'screen'
# honor it and otherwise use /bin/sh.
export SHELL

if [[ $- != *i* ]]
then
    # We are being invoked from a non-interactive shell.  If this
    # is an SSH session (as in \"ssh host command\"), source
    # /etc/profile so we get PATH and other essential variables.
    [[ -n \"$SSH_CLIENT\" ]] && source /etc/profile

    # Don't do anything else.
    return
fi

alias ls='ls -p --color=auto'
alias ll='ls -l'
alias grep='grep --color=auto'
alias ip='ip -color=auto'\n"))

(define %default-bash-profile
  (plain-file "bash_profile" "\
# Set up Guix Home profile
if [ -f ~/.profile ]; then . ~/.profile; fi

# Honor per-interactive-shell startup file
if [ -f ~/.bashrc ]; then . ~/.bashrc; fi

# Merge search-paths from multiple profiles, the order matters.
eval \"$(guix package --search-paths \\
-p $HOME/.config/guix/current \\
-p $HOME/.guix-home/profile \\
-p $HOME/.guix-profile \\
-p /run/current-system/profile)\"

# Prepend setuid programs.
export PATH=/run/setuid-programs:$PATH
"))

(define %default-zprofile
  (plain-file "zprofile" "\
# Set up the system, user profile, and related variables.
source /etc/profile
# Set up the home environment profile.
source ~/.profile
"))

(define %default-xdefaults
  (plain-file "Xdefaults" "\
XTerm*utf8: always
XTerm*metaSendsEscape: true\n"))

(define %default-gdbinit
  (plain-file "gdbinit"
              "# Tell GDB where to look for separate debugging files.
guile
(use-modules (gdb))
(execute (string-append \"set debug-file-directory \"
                        (string-join
                          (filter file-exists?
                                  (append
                                    (if (getenv \"GDB_DEBUG_FILE_DIRECTORY\")
                                      (list (getenv \"GDB_DEBUG_FILE_DIRECTORY\"))
                                      '())
                                    (list \"~/.guix-home/profile/lib/debug\"
                                          \"~/.guix-profile/lib/debug\"
                                          \"/run/current-system/profile/lib/debug\")))
                          \":\")))
end

# Authorize extensions found in the store, such as the
# pretty-printers of libstdc++.
set auto-load safe-path /gnu/store/*/lib\n"))

(define %default-nanorc
  (plain-file "nanorc"
              "# Include all the syntax highlighting modules.
include /run/current-system/profile/share/nano/*.nanorc\n"))

(define %default-dotguile
  (plain-file "dot-guile"
              "(cond ((false-if-exception (resolve-interface '(ice-9 readline)))
       =>
       (lambda (module)
         ;; Enable completion and input history at the REPL.
         ((module-ref module 'activate-readline))))
      (else
       (display \"Consider installing the 'guile-readline' package for
convenient interactive line editing and input history.\\n\\n\")))

      (unless (getenv \"INSIDE_EMACS\")
        (cond ((false-if-exception (resolve-interface '(ice-9 colorized)))
               =>
               (lambda (module)
                 ;; Enable completion and input history at the REPL.
                 ((module-ref module 'activate-colorized))))
              (else
               (display \"Consider installing the 'guile-colorized' package
for a colorful Guile experience.\\n\\n\"))))\n"))

(define %default-skeleton-home-config
  (plain-file "default-home-config" "\
;; This is a sample Guix Home configuration which can help setup your
;; home directory in the same declarative manner as Guix System.
;; For more information, see the Home Configuration section of the manual.
(define-module (guix-home-config)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services shells)
  #:use-module (gnu services)
  #:use-module (gnu system shadow))

(define home-config
  (home-environment
    (services
      (append
        (list
          ;; Uncomment the shell you wish to use for your user:
          ;(service home-bash-service-type)
          ;(service home-fish-service-type)
          ;(service home-zsh-service-type)

          (service home-files-service-type
           `((\".guile\" ,%default-dotguile)
             (\".Xdefaults\" ,%default-xdefaults)))

          (service home-xdg-configuration-files-service-type
           `((\"gdb/gdbinit\" ,%default-gdbinit)
             (\"nano/nanorc\" ,%default-nanorc))))

        %base-home-services))))

home-config"))

(define (default-skeletons)
  "Return the default skeleton files for /etc/skel.  These files are copied by
'useradd' in the home directory of newly created user accounts."

  (let ((profile   %default-bash-profile)
        (bashrc    %default-bashrc)
        (zprofile  %default-zprofile)
        (xdefaults %default-xdefaults)
        (gdbinit   %default-gdbinit))
    `((".bash_profile" ,profile)
      (".bashrc" ,bashrc)
      ;; Zsh sources ~/.zprofile before ~/.zshrc, and it sources ~/.zlogin
      ;; after ~/.zshrc.  To avoid interfering with any customizations a user
      ;; may have made in their ~/.zshrc, put this in .zprofile, not .zlogin.
      (".zprofile" ,zprofile)
      (".nanorc" ,%default-nanorc)
      (".Xdefaults" ,xdefaults)
      (".guile" ,%default-dotguile)
      (".gdbinit" ,gdbinit)
      ("guix-home-config.scm" ,%default-skeleton-home-config))))

(define (skeleton-directory skeletons)
  "Return a directory containing SKELETONS, a list of name/derivation tuples."
  (computed-file "skel"
                 (with-imported-modules '((guix build utils))
                   #~(begin
                       (use-modules (ice-9 match)
                                    (guix build utils))

                       (mkdir #$output)
                       (chdir #$output)

                       ;; Note: copy the skeletons instead of symlinking
                       ;; them like 'file-union' does, because 'useradd'
                       ;; would just copy the symlinks as is.
                       (for-each (match-lambda
                                   ((target source)
                                    (copy-recursively source target)))
                                 '#$skeletons)
                       ;; Make nanorc respect XDG_CONFIG_HOME.
                       (when (file-exists? ".nanorc")
                         (mkdir-p ".config/nano")
                         (rename-file ".nanorc" ".config/nano/nanorc"))
                       (when (file-exists? ".gdbinit")
                         (mkdir-p ".config/gdb")
                         (rename-file ".gdbinit" ".config/gdb/gdbinit"))
                       #t))))

(define (find-duplicates list)
  "Find duplicate entries in @var{list}.
Two entries are considered duplicates, if they are @code{equal?} to each other.
This implementation is made asymptotically faster than @code{delete-duplicates}
through the internal use of hash tables."
  (let loop ((list list)
             ;; We actually modify table in-place, but still allocate it here
             ;; so that we only need one level of indentation.
             (table (make-hash-table)))
    (match list
      (()
       (hash-fold (lambda (key value seed)
                    (if (> value 1)
                        (cons key seed)
                        seed))
                  '()
                  table))
      ((first . rest)
       (hash-set! table first
                  (1+ (hash-ref table first 0)))
       (loop rest table)))))

(define (assert-unique-account-names users)
  (match (find-duplicates (map user-account-name users))
    (() *unspecified*)
    (duplicates
     (warning
      (G_ "the following accounts appear more than once:~{ ~a~}~%")
      duplicates))))

(define (assert-unique-group-names groups)
  (match (find-duplicates (map user-group-name groups))
    (() *unspecified*)
    (duplicates
     (warning
      (G_ "the following groups appear more than once:~{ ~a~}~%")
      duplicates))))

(define (assert-valid-users/groups users groups)
  "Raise an error if USERS refer to groups not listed in GROUPS."
  (let ((groups (list->set (map user-group-name groups))))
    (define (validate-supplementary-group user group)
      (unless (set-contains? groups group)
        (raise (condition
                (&message
                 (message
                  (format #f (G_ "supplementary group '~a' \
of user '~a' is undeclared")
                          group
                          (user-account-name user))))))))

    (for-each (lambda (user)
                (unless (set-contains? groups (user-account-group user))
                  (raise (condition
                          (&message
                           (message
                            (format #f (G_ "primary group '~a' \
of user '~a' is undeclared")
                                    (user-account-group user)
                                    (user-account-name user)))))))

                (for-each (cut validate-supplementary-group user <>)
                          (user-account-supplementary-groups user)))
              users)))


;;;
;;; Accounts Service.
;;;

(define (user-group->gexp group)
  "Turn GROUP, a <user-group> object, into a list-valued gexp suitable for
'active-groups'."
  #~(list #$(user-group-name group)
          #$(user-group-password group)
          #$(user-group-id group)
          #$(user-group-system? group)))

(define (user-account->gexp account)
  "Turn ACCOUNT, a <user-account> object, into a list-valued gexp suitable for
'activate-users'."
  #~`(#$(user-account-name account)
      #$(user-account-uid account)
      #$(user-account-group account)
      #$(user-account-supplementary-groups account)
      #$(user-account-comment account)
      #$(user-account-home-directory account)
      #$(user-account-create-home-directory? account)
      ,#$(user-account-shell account)             ; this one is a gexp
      #$(user-account-password account)
      #$(user-account-system? account)))

(define (account-activation accounts+groups)
  "Return a gexp that activates ACCOUNTS+GROUPS, a list of <user-account> and
<user-group> objects.  Raise an error if a user account refers to a undefined
group."
  (define accounts
    (delete-duplicates (filter user-account? accounts+groups) eq?))

  (define user-specs
    (map user-account->gexp accounts))

  (define groups
    (delete-duplicates (filter user-group? accounts+groups) eq?))

  (define group-specs
    (map user-group->gexp groups))

  (assert-unique-account-names accounts)
  (assert-unique-group-names groups)
  (assert-valid-users/groups accounts groups)

  ;; Add users and user groups.
  (with-imported-modules (source-module-closure '((gnu system accounts)))
    #~(begin
        (use-modules (gnu system accounts))

        (activate-users+groups (map sexp->user-account (list #$@user-specs))
                               (map sexp->user-group (list #$@group-specs))))))

(define (account-shepherd-service accounts+groups)
  "Return a Shepherd service that creates the home directories for the user
accounts among ACCOUNTS+GROUPS."
  (define accounts
    (filter user-account? accounts+groups))

  (define regular-account-names
    (filter-map (lambda (account)
                  (and (not (user-account-system? account))
                       (user-account-name account)))
                accounts))

  ;; Create home directories only once 'file-systems' is up.  This makes sure
  ;; they are created in the right place if /home lives on a separate
  ;; partition.
  ;;
  ;; XXX: We arrange for this service to stop right after it's done its job so
  ;; that 'guix system reconfigure' knows that it can reload it fearlessly
  ;; (and thus create new home directories).
  (list (shepherd-service
         (requirement '(file-systems))
         (provision '(user-homes))
         (one-shot? #t)
         (modules '((gnu build activation)
                    (gnu system accounts)))
         (start (with-imported-modules (source-module-closure
                                        '((gnu build activation)
                                          (gnu system accounts)))
                  #~(lambda ()
                      (activate-user-home
                       (map sexp->user-account
                            (list #$@(map user-account->gexp accounts))))

                      ;; Create the user's profile directory upfront:
                      ;; guix-daemon lacks permissions to create it when it is
                      ;; running as an unprivileged user.
                      (for-each (lambda (account)
                                  (let ((profile (in-vicinity
                                                  "/var/guix/profiles/per-user"
                                                  account))
                                        (owner (getpwnam account)))
                                    (mkdir-p/perms profile owner #o755)))
                                '#$regular-account-names)
                      #t)))                       ;success
         (documentation "Create user home directories."))))

(define (shells-file shells)
  "Return a file-like object that builds a shell list for use as /etc/shells
based on SHELLS.  /etc/shells is used by xterm, polkit, and other programs."
  (computed-file "shells"
                 #~(begin
                     (use-modules (srfi srfi-1))

                     (define shells
                       (delete-duplicates (list #$@shells)))

                     (call-with-output-file #$output
                       (lambda (port)
                         (display "\
/bin/sh
/run/current-system/profile/bin/sh
/run/current-system/profile/bin/bash\n" port)
                         (for-each (lambda (shell)
                                     (display shell port)
                                     (newline port))
                                   shells))))))
(define (etc-files arguments)
  "Filter out among ARGUMENTS things corresponding to skeletons, and return
the /etc/skel directory for those."
  (let ((skels (filter pair? arguments))
        (users (filter user-account? arguments)))
    `(("skel" ,(skeleton-directory skels))
      ("shells" ,(shells-file (map user-account-shell users))))))

(define account-service-type
  (service-type (name 'account)

                ;; Concatenate <user-account>, <user-group>, and skeleton
                ;; lists.
                (compose concatenate)
                (extend append)

                (extensions
                 (list (service-extension activation-service-type
                                          account-activation)
                       (service-extension shepherd-root-service-type
                                          account-shepherd-service)
                       ;; Have 'user-processes' depend on 'user-homes' so that
                       ;; daemons start after their home directory has been
                       ;; created.
                       (service-extension user-processes-service-type
                                          (const '(user-homes)))
                       (service-extension etc-service-type
                                          etc-files)))
                (default-value '())
                (description
                 "Ensure the specified user accounts and groups exist, as well
as each account home directory.")))

(define (account-service accounts+groups skeletons)
  "Return a <service> that takes care of user accounts and user groups, with
ACCOUNTS+GROUPS as its initial list of accounts and groups."
  (service account-service-type
           (append skeletons accounts+groups)))


;;;
;;; Subids Service.
;;;

(define* (%root-subid #:optional (start %subordinate-id-min) (count %subordinate-id-count))
  (subid-range
   (name "root")
   (start start)
   (count count)))

(define-record-type* <subids-configuration>
  subids-configuration make-subids-configuration
  subids-configuration?
  this-subids-configuration

  (add-root?   subids-configuration-add-root?                 ; boolean
               (default #t))
  (subgids     subids-configuration-subgids                   ; list of <subid-range>
               (default '()))
  (subuids     subids-configuration-subuids                   ; list of <subid-range>
               (default '())))

(define (subid-range->gexp range)
  "Turn RANGE, a <subid-range> object, into a list-valued gexp suitable for
'activate-subuids+subgids'."
  (define count (subid-range-count range))
  #~`(#$(subid-range-name range)
      #$(subid-range-start range)
      #$(if (and (number? count)
                 (> count 0))
            count
            %subordinate-id-count)))

(define (assert-valid-subids ranges)
  (cond ((>= (fold + 0 (map subid-range-count ranges))
             (- %subordinate-id-max %subordinate-id-min -1))
         (raise
          (formatted-message
           (G_
            "The configured ranges are more than the ~a max allowed.")
           (- %subordinate-id-max %subordinate-id-min -1))))
        ((any (lambda (r)
                (define start (subid-range-start r))
                (and start
                     (< start %subordinate-id-min)))
              ranges)
         (raise
          (formatted-message
           (G_
            "One subid-range starts before the minimum allowed sub id ~a.")
           %subordinate-id-min)))
        ((any (lambda (r)
                (define end (subid-range-end r))
                (and end
                     (> end %subordinate-id-max)))
              ranges)
         (raise
          (formatted-message
           (G_
            "One subid-range ends after the maximum allowed sub id ~a.")
           %subordinate-id-max)))
        ((any (compose null? subid-range-name)
              ranges)
         (raise
          (formatted-message
           (G_
            "One subid-range has a null name."))))
        ((any (compose string-null? subid-range-name)
              ranges)
         (raise
          (formatted-message
           (G_
            "One subid-range has a name equal to the empty string."))))
        (else #t)))

(define (delete-duplicate-ranges ranges)
  (delete-duplicates ranges
                     (lambda args
                       (apply string=? (map subid-range-name ranges)))))

(define (subids-activation config)
  "Return a gexp that activates SUBUIDS+SUBGIDS, a list of <subid-range>
objects."
  (define (add-root-when-missing ranges)
    (define sorted-ranges
      (sort-list ranges subid-range-less))
    (define root-missing?
      (not
       (find (lambda (r)
               (string=? "root"
                         (subid-range-name r)))
             sorted-ranges)))
    (define first-start
      (and (> (length sorted-ranges) 0)
           (subid-range-start (first sorted-ranges))))
    (define first-has-start?
      (number? first-start))
    (define root-start
      (if first-has-start?
          (and
           (> first-start %subordinate-id-min)
           %subordinate-id-min)
          %subordinate-id-min))
    (define root-count
      (if first-has-start?
          (- first-start %subordinate-id-min)
          %subordinate-id-count))
    (if (and root-missing?
             (subids-configuration-add-root? config))
        (append (list (%root-subid root-start root-count))
                sorted-ranges)
        sorted-ranges))

  (define subuids
    (delete-duplicate-ranges (subids-configuration-subuids config)))

  (define subuids-specs
    (map subid-range->gexp (add-root-when-missing subuids)))

  (define subgids
    (delete-duplicate-ranges (subids-configuration-subgids config)))

  (define subgids-specs
    (map subid-range->gexp (add-root-when-missing subgids)))

  (assert-valid-subids subgids)
  (assert-valid-subids subuids)

  ;; Add subuids and subgids.
  (with-imported-modules (source-module-closure '((gnu system accounts)))
    #~(begin
        (use-modules (gnu system accounts))

        (activate-subuids+subgids (map sexp->subid-range (list #$@subuids-specs))
                                  (map sexp->subid-range (list #$@subgids-specs))))))

(define-record-type* <subids-extension>
  subids-extension make-subids-extension
  subids-extension?
  this-subids-extension

  (subgids   subids-extension-subgids                   ; list of <subid-range>
             (default '()))
  (subuids   subids-extension-subuids                   ; list of <subid-range>
             (default '())))

(define append-subid-ranges
  (lambda args
    (delete-duplicate-ranges
     (apply append args))))

(define (subids-extension-merge a b)
  (subids-extension
   (subgids (append-subid-ranges
             (subids-extension-subgids a)
             (subids-extension-subgids b)))
   (subuids (append-subid-ranges
             (subids-extension-subuids a)
             (subids-extension-subuids b)))))

(define subids-service-type
  (service-type (name 'subids)
                ;; Concatenate <subid-range> lists.
                (compose (lambda (args)
                           (fold subids-extension-merge
                                 (subids-extension)
                                 args)))
                (extend
                 (lambda (config extension)
                   (subids-configuration
                    (inherit config)
                    (subgids
                     (append-subid-ranges
                      (subids-configuration-subgids config)
                      (subids-extension-subgids extension)))
                    (subuids
                     (append-subid-ranges
                      (subids-configuration-subuids config)
                      (subids-extension-subuids extension))))))
                (extensions
                 (list (service-extension activation-service-type
                                          subids-activation)))
                (default-value
                  (subids-configuration))
                (description
                 "Ensure the specified sub UIDs and sub GIDs exist in
/etc/subuid and /etc/subgid.")))

;;; shadow.scm ends here
