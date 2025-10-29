;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 David Thompson <davet@gnu.org>
;;; Copyright © 2017-2019, 2022-2023, 2025 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu build linux-container)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-98)
  #:use-module (guix build utils)
  #:use-module (guix build syscalls)
  #:use-module (gnu system file-systems)          ;<file-system>
  #:use-module ((gnu build file-systems) #:select (mount-file-system))
  #:export (user-namespace-supported?
            unprivileged-user-namespace-supported?
            setgroups-supported?
            %namespaces
            %writable-/tmp
            run-container
            call-with-container
            container-excursion
            container-excursion*))

(define (user-namespace-supported?)
  "Return #t if user namespaces are supported on this system."
  (file-exists? "/proc/self/ns/user"))

(define (unprivileged-user-namespace-supported?)
  "Return #t if user namespaces can be created by unprivileged users."
  (let ((userns-file "/proc/sys/kernel/unprivileged_userns_clone"))
    (if (file-exists? userns-file)
        (eqv? #\1 (call-with-input-file userns-file read-char))
        (user-namespace-supported?))))

(define (setgroups-supported?)
  "Return #t if the setgroups proc file, introduced in Linux-libre 3.19,
exists."
  (file-exists? "/proc/self/setgroups"))

(define %namespaces
  '(cgroup mnt pid ipc uts user net))

(define (call-with-clean-exit thunk)
  "Apply THUNK, but exit with a status code of 1 if it fails."
  (dynamic-wind
    (const #t)
    (lambda ()
      (thunk)

      ;; XXX: Somehow we sometimes get EBADF from write(2) or close(2) upon
      ;; exit (coming from fd finalizers) when used by the Shepherd.  To work
      ;; around that, exit forcefully so fd finalizers don't have a chance to
      ;; run and fail.
      (primitive-_exit 0))
    (lambda ()
      (primitive-_exit 1))))

(define (purify-environment)
  "Unset all environment variables."
  (for-each unsetenv
            (match (get-environment-variables)
              (((names . _) ...) names))))

(define (remount-read-only mount-point)
  (mount mount-point mount-point "none"
         (logior MS_BIND MS_REMOUNT MS_RDONLY)))

;; The container setup procedure closely resembles that of the Docker
;; specification:
;; https://raw.githubusercontent.com/docker/libcontainer/master/SPEC.md
(define* (mount-file-systems root mounts #:key mount-/sys? mount-/proc?
                             (populate-file-system (const #t))
                             writable-root?)
  "Mount the essential file systems and those in MOUNTS, a list of
<file-system> objects, relative to ROOT; then make ROOT the new root directory
for the process."
  (define (scope dir)
    (string-append root dir))

  (define (touch file-name)
    (call-with-output-file file-name (const #t)))

  (define (bind-mount src dest)
    (mount src dest "none" MS_BIND))

  ;; Like mount, but creates the mount point if it doesn't exist.
  (define* (mount* source target type #:optional (flags 0) options
                   #:key (update-mtab? #f))
    (mkdir-p target)
    (mount source target type flags options #:update-mtab? update-mtab?))

  ;; The container's file system is completely ephemeral, sans directories
  ;; bind-mounted from the host.
  (mount "none" root "tmpfs")

  ;; A proc mount requires a new pid namespace.
  (when mount-/proc?
    (mount* "none" (scope "/proc") "proc"
            (logior MS_NOEXEC MS_NOSUID MS_NODEV)))

  ;; A sysfs mount requires the user to have the CAP_SYS_ADMIN capability in
  ;; the current network namespace.
  (when mount-/sys?
    (catch 'system-error
      (lambda ()
        (mount* "none" (scope "/sys") "sysfs"
                (logior MS_NOEXEC MS_NOSUID MS_NODEV MS_RDONLY)))
      (lambda args
        ;; EPERM means that CAP_SYS_ADMIN is missing.  Ignore.
        (unless (= EPERM (system-error-errno args))
          (apply throw args)))))

  (mount* "none" (scope "/dev") "tmpfs"
          (logior MS_NOEXEC MS_STRICTATIME)
          "mode=755")

  ;; Create essential device nodes via bind-mounting them from the
  ;; host, because a process within a user namespace cannot create
  ;; device nodes.
  (for-each (lambda (device)
              (when (file-exists? device)
                ;; Create the mount point file.
                (touch (scope device))
                (bind-mount device (scope device))))
            '("/dev/null"
              "/dev/zero"
              "/dev/full"
              "/dev/random"
              "/dev/urandom"
              "/dev/tty"
              "/dev/fuse"))

  ;; Mount a new devpts instance on /dev/pts.
  (when (file-exists? "/dev/ptmx")
    (mount* "none" (scope "/dev/pts") "devpts" 0
            "newinstance,mode=0620")
    (symlink "/dev/pts/ptmx" (scope "/dev/ptmx")))

  ;; Setup the container's /dev/console by bind mounting the pseudo-terminal
  ;; associated with standard input when there is one.
  (let* ((in      (current-input-port))
         (tty     (catch 'system-error
                    (lambda ()
                      ;; This call throws if IN does not correspond to a tty.
                      ;; This is more reliable than 'isatty?'.
                      (ttyname in))
                    (const #f)))
         (console (scope "/dev/console")))
    (when tty
      (touch console)
      (chmod console #o600)
      (bind-mount tty console)))

  ;; Setup standard input/output/error.
  (symlink "/proc/self/fd"   (scope "/dev/fd"))
  (symlink "/proc/self/fd/0" (scope "/dev/stdin"))
  (symlink "/proc/self/fd/1" (scope "/dev/stdout"))
  (symlink "/proc/self/fd/2" (scope "/dev/stderr"))

  ;; Mount user-specified file systems.
  (for-each (lambda (file-system)
              (mount-file-system file-system #:root root))
            mounts)

  ;; Jail the process inside the container's root file system.
  (let ((put-old (string-append root "/real-root")))
    (mkdir put-old)
    (pivot-root root put-old)
    (chdir "/")
    (umount "real-root" MNT_DETACH)
    (rmdir "real-root")
    (populate-file-system)
    (chmod "/" #o755)
    (unless writable-root?
      (remount-read-only "/"))))

(define* (initialize-user-namespace pid host-uids
                                    #:key
                                    (host-uid (getuid))
                                    (host-gid (getgid))
                                    (guest-uid 0) (guest-gid 0))
  "Configure the user namespace for PID.  HOST-UIDS specifies the number of
host user identifiers to map into the user namespace.  GUEST-UID and GUEST-GID
specify the first UID (respectively GID) that host UIDs (respectively GIDs)
map to in the namespace."
  (define proc-dir
    (string-append "/proc/" (number->string pid)))

  (define (scope file)
    (string-append proc-dir file))

  ;; Only root can write to the gid map without first disabling the
  ;; setgroups syscall.
  (unless (and (zero? host-uid) (zero? host-gid))
    (call-with-output-file (scope "/setgroups")
      (lambda (port)
        (display "deny" port))))

  ;; Map the user/group that created the container to the root user
  ;; within the container.
  (call-with-output-file (scope "/uid_map")
    (lambda (port)
      (format port "~d ~d ~d" guest-uid host-uid host-uids)))
  (call-with-output-file (scope "/gid_map")
    (lambda (port)
      (format port "~d ~d ~d" guest-gid host-gid host-uids))))

(define (namespaces->bit-mask namespaces)
  "Return the number suitable for the 'flags' argument of 'clone' that
corresponds to the symbols in NAMESPACES."
  ;; Use the same flags as fork(3) in addition to the namespace flags.
  (apply logior SIGCHLD
         (map (match-lambda
               ('cgroup  CLONE_NEWCGROUP)
               ('mnt  CLONE_NEWNS)
               ('uts  CLONE_NEWUTS)
               ('ipc  CLONE_NEWIPC)
               ('user CLONE_NEWUSER)
               ('pid  CLONE_NEWPID)
               ('net  CLONE_NEWNET))
              namespaces)))

(define* (run-container root mounts namespaces host-uids thunk
                        #:key (guest-uid 0) (guest-gid 0)
                        (populate-file-system (const #t))
                        (loopback-network? #t)
                        (lock-mounts? #t)
                        writable-root?)
  "Run THUNK in a new container process and return its PID.  ROOT specifies
the root directory for the container.  MOUNTS is a list of <file-system>
objects that specify file systems to mount inside the container.  NAMESPACES
is a list of symbols that correspond to the possible Linux namespaces: mnt,
ipc, uts, user, and net.  When LOCK-MOUNTS? is true, arrange so that none of
MOUNTS can be unmounted or remounted individually from within THUNK.

When LOOPBACK-NETWORK? is true and 'net is amount NAMESPACES, set up the
loopback device (\"lo\") and a minimal /etc/hosts.

When WRITABLE-ROOT? is false, remount the container's root as read-only before
calling THUNK.  Call POPULATE-FILE-SYSTEM before the root is (potentially)
made read-only.

HOST-UIDS specifies the number of host user identifiers to map into the user
namespace.  GUEST-UID and GUEST-GID specify the first UID (respectively GID)
that host UIDs (respectively GIDs) map to in the namespace."
  ;; The parent process must initialize the user namespace for the child
  ;; before it can boot.  To negotiate this, a pipe is used such that the
  ;; child process blocks until the parent writes to it.
  (match (socketpair PF_UNIX (logior SOCK_CLOEXEC SOCK_STREAM) 0)
    ((child . parent)
     (let ((flags (namespaces->bit-mask namespaces)))
       (match (clone flags)
         (0
          (call-with-clean-exit
           (lambda ()
             (close-port parent)
             ;; Wait for parent to set things up.
             (match (read child)
               ('ready
                (purify-environment)
                (when (and (memq 'mnt namespaces)
                           (not (string=? root "/")))
                  (catch #t
                    (lambda ()
                      (mount-file-systems root mounts
                                          #:mount-/proc? (memq 'pid namespaces)
                                          #:mount-/sys?  (memq 'net
                                                               namespaces)
                                          #:populate-file-system
                                          (lambda ()
                                            (populate-file-system)
                                            (when (and (memq 'net namespaces)
                                                       loopback-network?)
                                              (set-network-interface-up "lo")

                                              ;; When isolated from the
                                              ;; network, provide a minimal
                                              ;; /etc/hosts to resolve
                                              ;; "localhost".
                                              (mkdir-p "/etc")
                                              (call-with-output-file "/etc/hosts"
                                                (lambda (port)
                                                  (display "127.0.0.1 localhost\n" port)
                                                  (chmod port #o444)))))
                                          #:writable-root?
                                          (or writable-root?
                                              (not (memq 'mnt namespaces)))))
                    (lambda args
                      ;; Forward the exception to the parent process.
                      ;; FIXME: SRFI-35 conditions and non-trivial objects
                      ;; cannot be 'read' so they shouldn't be written as is.
                      (write args child)
                      (primitive-exit 3))))

                (when (and lock-mounts?
                           (memq 'mnt namespaces)
                           (memq 'user namespaces))
                  ;; Create a new mount namespace owned by a new user
                  ;; namespace to "lock" together previous mounts, such that
                  ;; they cannot be unmounted or remounted separately--see
                  ;; mount_namespaces(7).
                  ;;
                  ;; Note: at this point, the process is single-threaded (no
                  ;; GC mark threads, no finalization thread, etc.) which is
                  ;; why unshare(CLONE_NEWUSER) can be used.
                  (let ((uid (getuid)) (gid (getgid)))
                    (unshare (logior CLONE_NEWUSER CLONE_NEWNS))
                    (when (file-exists? "/proc/self")
                      (initialize-user-namespace (getpid)
                                                 host-uids
                                                 #:host-uid uid
                                                 #:host-gid gid
                                                 #:guest-uid guest-uid
                                                 #:guest-gid guest-gid))))

                ;; TODO: Manage capabilities.
                (write 'ready child)
                (close-port child)
                (thunk))
               (_                                 ;parent died or something
                (primitive-exit 2))))))
         (pid
          (close-port child)
          (when (memq 'user namespaces)
            (initialize-user-namespace pid host-uids
                                       #:guest-uid guest-uid
                                       #:guest-gid guest-gid))
          ;; TODO: Initialize cgroups.
          (write 'ready parent)
          (newline parent)

          ;; Check whether the child process' setup phase succeeded.
          (let ((message (read parent)))
            (close-port parent)
            (match message
              ('ready                             ;success
               pid)
              (((? symbol? key) args ...)         ;exception
               (apply throw key args))
              (_                                  ;unexpected termination
               #f)))))))))

;; FIXME: This is copied from (guix utils), which we cannot use because it
;; would pull (guix config) and all.
(define (call-with-temporary-directory proc)
  "Call PROC with a name of a temporary directory; close the directory and
delete it when leaving the dynamic extent of this call."
  (let* ((directory (or (getenv "TMPDIR") "/tmp"))
         (template  (string-append directory "/guix-directory.XXXXXX"))
         (tmp-dir   (mkdtemp! template)))
    (dynamic-wind
      (const #t)
      (lambda ()
        (proc tmp-dir))
      (lambda ()
        (false-if-exception (delete-file-recursively tmp-dir))))))

(define (wait-child-process)
  "Wait for one child process and return a pair, like 'waitpid', or return #f
if there are no child processes left."
  (catch 'system-error
    (lambda ()
      (waitpid WAIT_ANY))
    (lambda args
      (if (= ECHILD (system-error-errno args))
          #f
          (apply throw args)))))

(define (status->exit-status status)
  "Reify STATUS as an exit status."
  (or (status:exit-val status)
      ;; See <http://www.tldp.org/LDP/abs/html/exitcodes.html#EXITCODESREF>.
      (+ 128 (or (status:term-sig status)
                 (status:stop-sig status)))))

(define %writable-/tmp
  ;; Writable and volatile /tmp.
  (file-system
    (device "none")
    (mount-point "/tmp")
    (type "tmpfs")
    (flags '(no-suid no-dev))
    (options "mode=755,size=10%")
    (check? #f)))

(define* (call-with-container mounts thunk #:key (namespaces %namespaces)
                              (host-uids 1) (guest-uid 0) (guest-gid 0)
                              (lock-mounts? #t)
                              (relayed-signals (list SIGINT SIGTERM))
                              (child-is-pid1? #t)
                              (populate-file-system (const #t))
                              (loopback-network? #t)
                              writable-root?
                              (process-spawned-hook (const #t)))
  "Run THUNK in a new container process and return its exit status; call
PROCESS-SPAWNED-HOOK with the PID of the new process that has been spawned.
MOUNTS is a list of <file-system> objects that specify file systems to mount
inside the container.  NAMESPACES is a list of symbols corresponding to
the identifiers for Linux namespaces: mnt, ipc, uts, pid, user, and net.  By
default, all namespaces are used.

HOST-UIDS is the number of host user identifiers to map into the container's
user namespace, if there is one.  By default, only a single uid/gid, that of
the current user, is mapped into the container.  The host user that creates
the container is the root user (uid/gid 0) within the container.  Only root
can map more than a single uid/gid.

GUEST-UID and GUEST-GID specify the first UID (respectively GID) that host
UIDs (respectively GIDs) map to in the namespace.

RELAYED-SIGNALS is the list of signals that are \"relayed\" to the container
process when caught by its parent.

When LOOPBACK-NETWORK? is true and 'net is amount NAMESPACES, set up the
loopback device (\"lo\") and a minimal /etc/hosts.

When WRITABLE-ROOT? is false, remount the container's root as read-only before
calling THUNK.  Call POPULATE-FILE-SYSTEM before the root is (potentially)
made read-only.

When CHILD-IS-PID1? is true, and if NAMESPACES contains 'pid', then the child
process runs directly as PID 1.  As such, it is responsible for (1) installing
signal handlers and (2) reaping terminated processes by calling 'waitpid'.
When CHILD-IS-PID1? is false, a new intermediate process is created instead
that takes this responsibility.

Note that if THUNK needs to load any additional Guile modules, the relevant
module files must be present in one of the mappings in MOUNTS and the Guile
load path must be adjusted as needed."
  (define thunk*
    (if (and (memq 'pid namespaces)
             (not child-is-pid1?))
        (lambda ()
          ;; Behave like an init process: create a sub-process that calls
          ;; THUNK, and wait for child processes.  Furthermore, forward
          ;; RELAYED-SIGNALS to the child process.
          (match (primitive-fork)
            (0
             (call-with-clean-exit thunk))
            (pid
             (install-signal-handlers pid)
             (let loop ()
               (match (wait-child-process)
                 ((child . status)
                  (if (= child pid)
                      (primitive-exit (status->exit-status status))
                      (loop)))
                 (#f
                  (primitive-exit 128)))))))      ;cannot happen
        thunk))

  (define (periodically-schedule-asyncs)
    ;; XXX: In Guile there's a time window where a signal-handling async could
    ;; be queued without being processed by the time we enter a blocking
    ;; syscall like waitpid(2) (info "(guile) Signals").  This terrible hack
    ;; ensures pending asyncs get a chance to run periodically.
    (sigaction SIGALRM (lambda _ (alarm 1)))
    (alarm 1))

  (define (install-signal-handlers pid)
    ;; Install handlers that forward signals to PID.
    (define (relay-signal signal)
      (false-if-exception (kill pid signal)))

    (periodically-schedule-asyncs)
    (for-each (lambda (signal)
                (sigaction signal relay-signal))
              relayed-signals))

  (call-with-temporary-directory
   (lambda (root)
     (let ((pid (run-container root mounts namespaces host-uids thunk*
                               #:lock-mounts? lock-mounts?
                               #:guest-uid guest-uid
                               #:guest-gid guest-gid
                               #:populate-file-system populate-file-system
                               #:loopback-network? loopback-network?
                               #:writable-root? writable-root?)))
       (install-signal-handlers pid)
       (process-spawned-hook pid)
       (match (waitpid pid)
         ((_ . status) status))))))

(define (container-excursion pid thunk)
  "Run THUNK as a child process within the namespaces of process PID and
return the exit status, an integer as returned by 'waitpid'."
  (define (namespace-file pid namespace)
    (string-append "/proc/" (number->string pid) "/ns/" namespace))

  (match (primitive-fork)
    (0
     (call-with-clean-exit
      (lambda ()
        ;; First, determine the user namespace that owns the pid namespace and
        ;; join that user namespace (the assumption is that it also owns all
        ;; the other namespaces).  It's important that the user namespace is
        ;; joined first, so that the user will have the privileges to join the
        ;; other namespaces.
        (let* ((pid-ns (open-fdes (namespace-file pid "pid")
                                  (logior O_CLOEXEC O_RDONLY)))
               (user-ns (get-user-ns pid-ns)))
          (close-fdes pid-ns)
          (unless (equal? (stat user-ns)
                          (stat (namespace-file (getpid) "user")))
            (setns user-ns 0))
          (close-fdes user-ns)

          ;; Then join all the remaining namespaces.
          (for-each (lambda (ns)
                      (let ((source (namespace-file (getpid) ns))
                            (target (namespace-file pid ns)))
                        ;; Joining the namespace that the process already
                        ;; belongs to would throw an error so avoid that.
                        ;; XXX: This /proc interface leads to TOCTTOU.
                        (unless (string=? (readlink source) (readlink target))
                          (call-with-input-file target
                            (lambda (new-ns-port)
                              (setns (fileno new-ns-port) 0))))))
                    ;; It's important that the mount namespace is joined last,
                    ;; otherwise the /proc mount point would no longer be
                    ;; accessible.
                    '("ipc" "uts" "net" "pid" "mnt")))
        (purify-environment)
        (chdir "/")

        ;; Per setns(2), changing the PID namespace only applies to child
        ;; processes, not to the process itself.  Thus fork so that THUNK runs
        ;; in the right PID namespace, which also gives it access to /proc.
        (match (primitive-fork)
          (0 (call-with-clean-exit thunk))
          (pid (primitive-exit
                (match (waitpid pid)
                  ((_ . status)
                   (or (status:exit-val status) 127)))))))))
    (pid
     (match (waitpid pid)
       ((_ . status)
        status)))))

(define (container-excursion* pid thunk)
  "Like 'container-excursion', but return the return value of THUNK."
  (match (pipe)
    ((in . out)
     ;; Make sure IN and OUT are not inherited if THUNK forks + execs.
     (fcntl in F_SETFD FD_CLOEXEC)
     (fcntl out F_SETFD FD_CLOEXEC)

     (match (container-excursion pid
              (lambda ()
                (close-port in)
                (write (thunk) out)
                (close-port out)))
       (0
        (close-port out)
        (let ((result (read in)))
          (close-port in)
          result))
       (_                                         ;maybe PID died already
        (close-port out)
        (close-port in)
        #f)))))
