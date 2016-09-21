;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2016 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
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

(define-module (gnu build activation)
  #:use-module (gnu build linux-boot)
  #:use-module (guix build utils)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (activate-users+groups
            activate-etc
            activate-setuid-programs
            activate-/bin/sh
            activate-modprobe
            activate-firmware
            activate-ptrace-attach
            activate-current-system))

;;; Commentary:
;;;
;;; This module provides "activation" helpers.  Activation is the process that
;;; consists in setting up system-wide files and directories so that an
;;; 'operating-system' configuration becomes active.
;;;
;;; Code:

(define (enumerate thunk)
  "Return the list of values returned by THUNK until it returned #f."
  (let loop ((entry  (thunk))
             (result '()))
    (if (not entry)
        (reverse result)
        (loop (thunk) (cons entry result)))))

(define (current-users)
  "Return the passwd entries for all the currently defined user accounts."
  (setpw)
  (enumerate getpwent))

(define (current-groups)
  "Return the group entries for all the currently defined user groups."
  (setgr)
  (enumerate getgrent))

(define* (add-group name #:key gid password system?
                    (log-port (current-error-port)))
  "Add NAME as a user group, with the given numeric GID if specified."
  ;; Use 'groupadd' from the Shadow package.
  (format log-port "adding group '~a'...~%" name)
  (let ((args `(,@(if gid `("-g" ,(number->string gid)) '())
                ,@(if password `("-p" ,password) '())
                ,@(if system? `("--system") '())
                ,name)))
    (zero? (apply system* "groupadd" args))))

(define %skeleton-directory
  ;; Directory containing skeleton files for new accounts.
  ;; Note: keep the trailing '/' so that 'scandir' enters it.
  "/etc/skel/")

(define (dot-or-dot-dot? file)
  (member file '("." "..")))

(define (make-file-writable file)
  "Make FILE writable for its owner.."
  (let ((stat (lstat file)))                      ;XXX: symlinks
    (chmod file (logior #o600 (stat:perms stat)))))

(define* (copy-account-skeletons home
                                 #:optional (directory %skeleton-directory))
  "Copy the account skeletons from DIRECTORY to HOME."
  (let ((files (scandir directory (negate dot-or-dot-dot?)
                        string<?)))
    (mkdir-p home)
    (for-each (lambda (file)
                (let ((target (string-append home "/" file)))
                  (copy-recursively (string-append directory "/" file)
                                    target
                                    #:log (%make-void-port "w"))
                  (make-file-writable target)))
              files)))

(define* (make-skeletons-writable home
                                  #:optional (directory %skeleton-directory))
  "Make sure that the files that have been copied from DIRECTORY to HOME are
owner-writable in HOME."
  (let ((files (scandir directory (negate dot-or-dot-dot?)
                        string<?)))
    (for-each (lambda (file)
                (let ((target (string-append home "/" file)))
                  (when (file-exists? target)
                    (make-file-writable target))))
              files)))

(define* (add-user name group
                   #:key uid comment home create-home?
                   shell password system?
                   (supplementary-groups '())
                   (log-port (current-error-port)))
  "Create an account for user NAME part of GROUP, with the specified
properties.  Return #t on success."
  (format log-port "adding user '~a'...~%" name)

  (if (and uid (zero? uid))

      ;; 'useradd' fails with "Cannot determine your user name" if the root
      ;; account doesn't exist.  Thus, for bootstrapping purposes, create that
      ;; one manually.
      (begin
        (call-with-output-file "/etc/shadow"
          (cut format <> "~a::::::::~%" name))
        (call-with-output-file "/etc/passwd"
          (cut format <> "~a:x:~a:~a:~a:~a:~a~%"
               name "0" "0" comment home shell))
        (chmod "/etc/shadow" #o600)
        (copy-account-skeletons (or home "/root"))
        #t)

      ;; Use 'useradd' from the Shadow package.
      (let ((args `(,@(if uid `("-u" ,(number->string uid)) '())
                    "-g" ,(if (number? group) (number->string group) group)
                    ,@(if (pair? supplementary-groups)
                          `("-G" ,(string-join supplementary-groups ","))
                          '())
                    ,@(if comment `("-c" ,comment) '())
                    ,@(if (and home create-home?)
                          (if (file-exists? home)
                              `("-d" ,home)     ; avoid warning from 'useradd'
                              `("-d" ,home "--create-home"))
                          '())
                    ,@(if shell `("-s" ,shell) '())
                    ,@(if password `("-p" ,password) '())
                    ,@(if system? '("--system") '())
                    ,name)))
        (and (zero? (apply system* "useradd" args))
             (begin
               ;; Since /etc/skel is a link to a directory in the store where
               ;; all files have the writable bit cleared, and since 'useradd'
               ;; preserves permissions when it copies them, explicitly make
               ;; them writable.
               (make-skeletons-writable home)
               #t)))))

(define* (modify-user name group
                      #:key uid comment home create-home?
                      shell password system?
                      (supplementary-groups '())
                      (log-port (current-error-port)))
  "Modify user account NAME to have all the given settings."
  ;; Use 'usermod' from the Shadow package.
  (let ((args `(,@(if uid `("-u" ,(number->string uid)) '())
                "-g" ,(if (number? group) (number->string group) group)
                ,@(if (pair? supplementary-groups)
                      `("-G" ,(string-join supplementary-groups ","))
                      '())
                ,@(if comment `("-c" ,comment) '())
                ;; Don't use '--move-home', so ignore HOME.
                ,@(if shell `("-s" ,shell) '())
                ,name)))
    (zero? (apply system* "usermod" args))))

(define* (delete-user name #:key (log-port (current-error-port)))
  "Remove user account NAME.  Return #t on success.  This may fail if NAME is
logged in."
  (format log-port "deleting user '~a'...~%" name)
  (zero? (system* "userdel" name)))

(define* (delete-group name #:key (log-port (current-error-port)))
  "Remove group NAME.  Return #t on success."
  (format log-port "deleting group '~a'...~%" name)
  (zero? (system* "groupdel" name)))

(define* (ensure-user name group
                      #:key uid comment home create-home?
                      shell password system?
                      (supplementary-groups '())
                      (log-port (current-error-port))
                      #:rest rest)
  "Make sure user NAME exists and has the relevant settings."
  (if (false-if-exception (getpwnam name))
      (apply modify-user name group rest)
      (apply add-user name group rest)))

(define (activate-users+groups users groups)
  "Make sure the accounts listed in USERS and the user groups listed in GROUPS
are all available.

Each item in USERS is a list of all the characteristics of a user account;
each item in GROUPS is a tuple with the group name, group password or #f, and
numeric gid or #f."
  (define (touch file)
    (close-port (open-file file "a0b")))

  (define activate-user
    (match-lambda
     ((name uid group supplementary-groups comment home create-home?
       shell password system?)
      (let ((profile-dir (string-append "/var/guix/profiles/per-user/"
                                        name)))
        (ensure-user name group
                     #:uid uid
                     #:system? system?
                     #:supplementary-groups supplementary-groups
                     #:comment comment
                     #:home home
                     #:create-home? create-home?
                     #:shell shell
                     #:password password)

        (unless system?
          ;; Create the profile directory for the new account.
          (let ((pw (getpwnam name)))
            (mkdir-p profile-dir)
            (chown profile-dir (passwd:uid pw) (passwd:gid pw))))))))

  ;; 'groupadd' aborts if the file doesn't already exist.
  (touch "/etc/group")

  ;; Allow home directories to be created under /var/lib.
  (mkdir-p "/var/lib")

  ;; Create the root account so we can use 'useradd' and 'groupadd'.
  (activate-user (find (match-lambda
                        ((name (? zero?) _ ...) #t)
                        (_ #f))
                       users))

  ;; Then create the groups.
  (for-each (match-lambda
             ((name password gid system?)
              (unless (false-if-exception (getgrnam name))
                (add-group name
                           #:gid gid #:password password
                           #:system? system?))))
            groups)

  ;; Create the other user accounts.
  (for-each activate-user users)

  ;; Finally, delete extra user accounts and groups.
  (for-each delete-user
            (lset-difference string=?
                             (map passwd:name (current-users))
                             (match users
                               (((names . _) ...)
                                names))))
  (for-each delete-group
            (lset-difference string=?
                             (map group:name (current-groups))
                             (match groups
                               (((names . _) ...)
                                names)))))

(define (activate-etc etc)
  "Install ETC, a directory in the store, as the source of static files for
/etc."

  ;; /etc is a mixture of static and dynamic settings.  Here is where we
  ;; initialize it from the static part.

  (define (rm-f file)
    (false-if-exception (delete-file file)))

  (format #t "populating /etc from ~a...~%" etc)

  ;; Create the /etc/ssl -> /run/current-system/profile/etc/ssl symlink.  This
  ;; symlink, to a target outside of the store, probably doesn't belong in the
  ;; static 'etc' store directory.  However, if it were to be put there,
  ;; beware that if /run/current-system/profile/etc/ssl doesn't exist at the
  ;; time of activation (e.g. when installing a fresh system), the call to
  ;; 'file-is-directory?' below will fail because it uses 'stat', not 'lstat'.
  (rm-f "/etc/ssl")
  (symlink "/run/current-system/profile/etc/ssl" "/etc/ssl")

  (rm-f "/etc/static")
  (symlink etc "/etc/static")
  (for-each (lambda (file)
              (let ((target (string-append "/etc/" file))
                    (source (string-append "/etc/static/" file)))
                (rm-f target)

                ;; Things such as /etc/sudoers must be regular files, not
                ;; symlinks; furthermore, they could be modified behind our
                ;; back---e.g., with 'visudo'.  Thus, make a copy instead of
                ;; symlinking them.
                (if (file-is-directory? source)
                    (symlink source target)
                    (copy-file source target))

                ;; XXX: Dirty hack to meet sudo's expectations.
                (when (string=? (basename target) "sudoers")
                  (chmod target #o440))))
            (scandir etc (negate dot-or-dot-dot?)

                     ;; The default is 'string-locale<?', but we don't have
                     ;; it when run from the initrd's statically-linked
                     ;; Guile.
                     string<?)))

(define %setuid-directory
  ;; Place where setuid programs are stored.
  "/run/setuid-programs")

(define (link-or-copy source target)
  "Attempt to make TARGET a hard link to SOURCE; if it fails, fall back to
copy SOURCE to TARGET."
  (catch 'system-error
    (lambda ()
      (link source target))
    (lambda args
      ;; Perhaps SOURCE and TARGET live in a different file system, so copy
      ;; SOURCE.
      (copy-file source target))))

(define (activate-setuid-programs programs)
  "Turn PROGRAMS, a list of file names, into setuid programs stored under
%SETUID-DIRECTORY."
  (define (make-setuid-program prog)
    (let ((target (string-append %setuid-directory
                                 "/" (basename prog))))
      (link-or-copy prog target)
      (chown target 0 0)
      (chmod target #o6555)))

  (format #t "setting up setuid programs in '~a'...~%"
          %setuid-directory)
  (if (file-exists? %setuid-directory)
      (for-each (compose delete-file
                         (cut string-append %setuid-directory "/" <>))
                (scandir %setuid-directory
                         (lambda (file)
                           (not (member file '("." ".."))))
                         string<?))
      (mkdir-p %setuid-directory))

  (for-each make-setuid-program programs))

(define (activate-/bin/sh shell)
  "Change /bin/sh to point to SHELL."
  (symlink shell "/bin/sh.new")
  (rename-file "/bin/sh.new" "/bin/sh"))

(define (activate-modprobe modprobe)
  "Tell the kernel to use MODPROBE to load modules."
  (call-with-output-file "/proc/sys/kernel/modprobe"
    (lambda (port)
      (display modprobe port))))

(define (activate-firmware directory)
  "Tell the kernel to look for device firmware under DIRECTORY.  This
mechanism bypasses udev: it allows Linux to handle firmware loading directly
by itself, without having to resort to a \"user helper\"."
  (call-with-output-file "/sys/module/firmware_class/parameters/path"
    (lambda (port)
      (display directory port))))

(define (activate-ptrace-attach)
  "Allow users to PTRACE_ATTACH their own processes.

This works around a regression introduced in the default \"security\" policy
found in Linux 3.4 onward that prevents users from attaching to their own
processes--see Yama.txt in the Linux source tree for the rationale.  This
sounds like an unacceptable restriction for little or no security
improvement."
  (let ((file "/proc/sys/kernel/yama/ptrace_scope"))
    (when (file-exists? file)
      (call-with-output-file file
        (lambda (port)
          (display 0 port))))))


(define %current-system
  ;; The system that is current (a symlink.)  This is not necessarily the same
  ;; as the system we booted (aka. /run/booted-system) because we can re-build
  ;; a new system configuration and activate it, without rebooting.
  "/run/current-system")

(define (boot-time-system)
  "Return the '--system' argument passed on the kernel command line."
  (find-long-option "--system" (linux-command-line)))

(define* (activate-current-system
          #:optional (system (or (getenv "GUIX_NEW_SYSTEM")
                                 (boot-time-system))))
  "Atomically make SYSTEM the current system."
  ;; The 'GUIX_NEW_SYSTEM' environment variable is used as a way for 'guix
  ;; system reconfigure' to pass the file name of the new system.

  (format #t "making '~a' the current system...~%" system)

  ;; Atomically make SYSTEM current.
  (let ((new (string-append %current-system ".new")))
    (symlink system new)
    (rename-file new %current-system)))

;;; activation.scm ends here
