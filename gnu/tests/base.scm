;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016-2020, 2022, 2024 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2018 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2022 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2024 Dariqq <dariqq@posteo.net>
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

(define-module (gnu tests base)
  #:use-module (gnu tests)
  #:use-module (gnu image)
  #:use-module (gnu system)
  #:autoload   (gnu system image) (system-image)
  #:use-module (gnu system privilege)
  #:use-module (gnu system shadow)
  #:use-module (gnu system vm)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services networking)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages ocr)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages tmux)
  #:use-module (gnu packages virtualization)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (guix monads)
  #:use-module (guix modules)
  #:use-module (guix packages)
  #:use-module (guix profiles)
  #:use-module (guix utils)
  #:use-module ((srfi srfi-1) #:hide (partition))
  #:use-module (ice-9 match)
  #:export (run-basic-test
            %test-basic-os
            %test-linux-libre-5.15
            %test-linux-libre-5.10
            %test-linux-libre-5.4
            %test-halt
            %test-root-unmount
            %test-cleanup
            %test-activation

            %hello-dependencies-manifest
            guix-daemon-test-cases
            %test-guix-daemon))

(define %simple-os
  (simple-operating-system))


(define* (run-basic-test os command #:optional (name "basic")
                         #:key
                         initialization
                         root-password
                         desktop?)
  "Return a derivation called NAME that tests basic features of the OS started
using COMMAND, a gexp that evaluates to a list of strings.  Compare some
properties of running system to what's declared in OS, an <operating-system>.

When INITIALIZATION is true, it must be a one-argument procedure that is
passed a gexp denoting the marionette, and it must return gexp that is
inserted before the first test.  This is used to introduce an extra
initialization step, such as entering a LUKS passphrase.

When ROOT-PASSWORD is true, enter it as the root password when logging in.
Otherwise assume that there is no password for root."
  (define special-files
    (service-value
     (fold-services (operating-system-services os)
                    #:target-type special-files-service-type)))

  (define guix&co
    (match (package-transitive-propagated-inputs guix)
      (((labels packages) ...)
       (cons guix packages))))

  (define test
    (with-imported-modules '((gnu build marionette)
                             (guix build syscalls))
      #~(begin
          (use-modules (gnu build marionette)
                       (guix build syscalls)
                       (srfi srfi-1)
                       (srfi srfi-19)
                       (srfi srfi-26)
                       (srfi srfi-64)
                       (ice-9 match))

          (define marionette
            (make-marionette #$command))

          (test-runner-current (system-test-runner #$output))
          (test-begin "basic")

          #$(and initialization
                 (initialization #~marionette))

          (test-assert "uname"
            (match (marionette-eval '(uname) marionette)
              (#("Linux" host-name version _ architecture)
               (and (string=? host-name
                              #$(operating-system-host-name os))
                    (string-prefix? #$(package-version
                                       (operating-system-kernel os))
                                    version)
                    (string-prefix? architecture %host-type)))))

          ;; Shepherd reads the config file *before* binding its control
          ;; socket, so /var/run/shepherd/socket might not exist yet when the
          ;; 'marionette' service is started.
          (test-assert "shepherd socket ready"
            (marionette-eval
             `(begin
                (use-modules (gnu services herd))
                (let loop ((i 10))
                  (cond ((file-exists? (%shepherd-socket-file))
                         #t)
                        ((> i 0)
                         (sleep 1)
                         (loop (- i 1)))
                        (else
                         #f))))
             marionette))

          (test-eq "stdin is /dev/null"
            'eof
            ;; Make sure services can no longer read from stdin once the
            ;; system has booted.
            (marionette-eval
             `(begin
                (use-modules (gnu services herd))
                (start-service 'user-processes)
                ((@@ (gnu services herd) eval-there)
                 '(let ((result (read (current-input-port))))
                    (if (eof-object? result)
                        'eof
                        result))))
             marionette))

          (test-assert "shell and user commands"
            ;; Is everything in $PATH?
            (zero? (marionette-eval '(system "
. /etc/profile
set -e -x
guix --version
ls --version
grep --version
info --version")
                                    marionette)))

          (test-equal "special files"
            '#$special-files
            (marionette-eval
             '(begin
                (use-modules (ice-9 match))

                (map (match-lambda
                       ((file target)
                        (list file (readlink file))))
                     '#$special-files))
             marionette))

          (test-assert "accounts"
            (let ((users (marionette-eval '(begin
                                             (use-modules (ice-9 match))
                                             (let loop ((result '()))
                                               (match (getpw)
                                                 (#f (reverse result))
                                                 (x  (loop (cons x result))))))
                                          marionette)))
              (lset= equal?
                     (map (lambda (user)
                            (list (passwd:name user)
                                  (passwd:dir user)))
                          users)
                     (list
                      #$@(map (lambda (account)
                                `(list ,(user-account-name account)
                                       ,(user-account-home-directory account)))
                              (operating-system-user-accounts os))))))

          (test-assert "shepherd services"
            (let ((services (marionette-eval
                             '(begin
                                (use-modules (gnu services herd)
                                             (srfi srfi-1))

                                (append-map live-service-provision
                                            (current-services)))
                             marionette)))
              (lset= eq?
                     (pk 'services services)
                     '(root
                       shepherd
                       #$@(operating-system-shepherd-service-names os)))))

          (test-equal "libc honors /etc/localtime"
            -7200          ;CEST = GMT+2
            ;; Assume OS is configured to have a CEST timezone.
            (let* ((sept-2021 (time-second
                               (date->time-utc
                                (make-date 0 0 00 12 01 09 2021 7200)))))
              (marionette-eval
               `(tm:gmtoff (localtime ,sept-2021))
               marionette)))

          (test-equal "/var/log/messages is not world-readable"
            #o640                                ;<https://bugs.gnu.org/40405>
            (begin
              (wait-for-file "/var/log/messages" marionette
                             #:read 'get-u8)
              (marionette-eval '(stat:perms (lstat "/var/log/messages"))
                               marionette)))

          (test-assert "homes"
            (let ((homes
                   '#$(map user-account-home-directory
                           (filter user-account-create-home-directory?
                                   (operating-system-user-accounts os)))))
              (marionette-eval
               `(begin
                  (use-modules (gnu services herd) (srfi srfi-1))

                  ;; Home directories are supposed to exist once 'user-homes'
                  ;; has been started.
                  (start-service 'user-homes)

                  (every (lambda (home)
                           (and (file-exists? home)
                                (file-is-directory? home)))
                         ',homes))
               marionette)))

          (test-assert "skeletons in home directories"
            (let ((users+homes
                   '#$(filter-map (lambda (account)
                                    (and (user-account-create-home-directory?
                                          account)
                                         (not (user-account-system? account))
                                         (list (user-account-name account)
                                               (user-account-home-directory
                                                account))))
                                  (operating-system-user-accounts os))))
              (marionette-eval
               `(begin
                  (use-modules (guix build utils) (srfi srfi-1)
                               (ice-9 ftw) (ice-9 match))

                  (every (match-lambda
                           ((user home)
                            ;; Make sure HOME has all the skeletons...
                            (and (null? (lset-difference string=?
                                                         (scandir "/etc/skel/")
                                                         (scandir home)))

                                 ;; ... and that everything is user-owned.
                                 (let* ((pw  (getpwnam user))
                                        (uid (passwd:uid pw))
                                        (gid (passwd:gid pw))
                                        (st  (lstat home)))
                                   (define (user-owned? file)
                                     (= uid (stat:uid (lstat file))))

                                   (and (= uid (stat:uid st))
                                        (eq? 'directory (stat:type st))
                                        (every user-owned?
                                               (find-files home
                                                           #:directories? #t)))))))
                         ',users+homes))
               marionette)))

          (test-equal "permissions on /root"
            #o700
            (let ((root-home #$(any (lambda (account)
                                      (and (zero? (user-account-uid account))
                                           (user-account-home-directory
                                            account)))
                                    (operating-system-user-accounts os))))
              (stat:perms (marionette-eval `(stat ,root-home) marionette))))

          (test-equal "permissions on /tmp"
            #o1777
            (stat:perms (marionette-eval '(lstat "/tmp") marionette)))

          (test-equal "ownership and permissions of /var/empty"
            '(0 0 #o555)
            (let ((st (marionette-eval `(stat "/var/empty") marionette)))
              (list (stat:uid st) (stat:gid st)
                    (stat:perms st))))

          (test-equal "no extra home directories"
            '()

            ;; Make sure the home directories that are not supposed to be
            ;; created are indeed not created.
            (let ((nonexistent
                   '#$(filter-map (lambda (user)
                                    (and (not
                                          (user-account-create-home-directory?
                                           user))
                                         (user-account-home-directory user)))
                                  (operating-system-user-accounts os))))
              (marionette-eval
               `(begin
                  (use-modules (srfi srfi-1))

                  ;; Note: Do not flag "/var/empty".
                  (filter file-exists?
                          ',(remove (cut string-prefix? "/var/" <>)
                                    nonexistent)))
               marionette)))

          (test-equal "login on tty1"
            "root\n"
            (begin
              ;; XXX: On desktop, GDM3 will switch to TTY7. If this happens
              ;; after we switched to TTY1, we won't be able to login. Make
              ;; sure to wait long enough before switching to TTY1.
              (when #$desktop?
                (sleep 30))

              (marionette-control "sendkey ctrl-alt-f1" marionette)
              ;; Wait for the 'term-tty1' service to be running (using
              ;; 'start-service' is the simplest and most reliable way to do
              ;; that.)
              (marionette-eval
               '(begin
                  (use-modules (gnu services herd))
                  (start-service 'term-tty1))
               marionette)

              ;; Now we can type.
              (let ((password #$root-password))
                (if password
                    (begin
                      (marionette-type "root\n" marionette)
                      (wait-for-screen-text marionette
                                            (lambda (text)
                                              (string-contains text "Password"))
                                            #:ocr
                                            #$(file-append ocrad "/bin/ocrad"))
                      (marionette-type (string-append password "\n\n")
                                       marionette))
                    (marionette-type "root\n\n" marionette)))
              (marionette-type "id -un > logged-in\n" marionette)

              ;; It can take a while before the shell commands are executed.
              (marionette-eval '(use-modules (rnrs io ports)) marionette)
              (wait-for-file "/root/logged-in" marionette
                             #:read 'get-string-all
                             #:timeout 30)))

          (test-equal "getlogin on tty1"
            "\"root\""
            (begin
              ;; Assume we logged in in the previous test and type.
              (marionette-type "guile -c '(write (getlogin))' > /root/login-id.tmp\n"
                               marionette)
              (marionette-type "mv /root/login-id{.tmp,}\n"
                               marionette)

              ;; It can take a while before the shell commands are executed.
              (marionette-eval '(use-modules (rnrs io ports)) marionette)
              (wait-for-file "/root/login-id" marionette
                             #:read 'get-string-all
                             #:timeout 30)))

          ;; There should be one utmpx entry for the user logged in on tty1.
          (test-equal "utmpx entry"
            '(("root" "tty1" #f))
            (marionette-eval
             '(begin
                (use-modules (guix build syscalls)
                             (srfi srfi-1))

                (filter-map (lambda (entry)
                              (and (equal? (login-type USER_PROCESS)
                                           (utmpx-login-type entry))
                                   (list (utmpx-user entry) (utmpx-line entry)
                                         (utmpx-host entry))))
                            (utmpx-entries)))
             marionette))

          ;; Likewise for /var/log/wtmp (used by 'last').
          (test-assert "wtmp entry"
            (match (marionette-eval
                    '(begin
                       (use-modules (guix build syscalls)
                                    (srfi srfi-1))

                       (define (entry->list entry)
                         (list (utmpx-user entry) (utmpx-line entry)
                               (utmpx-host entry) (utmpx-login-type entry)))

                       (call-with-input-file "/var/log/wtmp"
                         (lambda (port)
                           (let loop ((result '()))
                             (if (eof-object? (peek-char port))
                                 (map entry->list (reverse result))
                                 (loop (cons (read-utmpx port) result)))))))
                    marionette)
              (((users lines hosts types) ..1)
               (every (lambda (type)
                        (eqv? type (login-type LOGIN_PROCESS)))
                      types))))

          (test-assert "host name resolution"
            (match (marionette-eval
                    '(begin
                       ;; Wait for nscd or our requests go through it.
                       (use-modules (gnu services herd))
                       (start-service 'nscd)

                       (list (getaddrinfo "localhost")
                             (getaddrinfo #$(operating-system-host-name os))))
                    marionette)
              ((((? vector?) ..1) ((? vector?) ..1))
               #t)
              (x
               (pk 'failure x #f))))

          (test-assert "nscd configuration action"
            (marionette-eval '(with-shepherd-action 'nscd ('configuration)
                                                    results
                                (file-exists? (car results)))
                             marionette))

          (test-equal "nscd invalidate action"
            '(#t)                                 ;one value, #t
            (marionette-eval '(with-shepherd-action 'nscd ('invalidate "hosts")
                                                    result
                                                    result)
                             marionette))

          ;; FIXME: The 'invalidate' action can't reliably obtain the exit
          ;; code of 'nscd' so skip this test.
          (test-skip 1)
          (test-equal "nscd invalidate action, wrong table"
            '(#f)                                 ;one value, #f
            (marionette-eval '(with-shepherd-action 'nscd ('invalidate "xyz")
                                                    result
                                                    result)
                             marionette))

          (test-equal "host not found"
            #f
            (marionette-eval
             '(false-if-exception (getaddrinfo "does-not-exist"))
             marionette))

          (test-equal "locale"
            "en_US.utf8"
            (marionette-eval '(let ((before (setlocale LC_ALL "en_US.utf8")))
                                (setlocale LC_ALL before))
                             marionette))

          (test-eq "/run/current-system is a GC root"
            'success!
            (marionette-eval '(begin
                                ;; Make sure the (guix …) modules are found.
                                (eval-when (expand load eval)
                                  (set! %load-path
                                    (append (map (lambda (package)
                                                   (string-append package
                                                                  "/share/guile/site/"
                                                                  (effective-version)))
                                                 '#$guix&co)
                                            %load-path)))

                                (use-modules (srfi srfi-34) (guix store))

                                (let ((system (readlink "/run/current-system")))
                                  (guard (c ((store-protocol-error? c)
                                             (and (file-exists? system)
                                                  'success!)))
                                    (with-store store
                                      (delete-paths store (list system))
                                      #f))))
                             marionette))

          ;; This symlink is currently unused, but better have it point to the
          ;; right place.  See
          ;; <https://lists.gnu.org/archive/html/guix-devel/2016-08/msg01641.html>.
          (test-equal "/var/guix/gcroots/profiles is a valid symlink"
            "/var/guix/profiles"
            (marionette-eval '(readlink "/var/guix/gcroots/profiles")
                             marionette))

          (test-equal "guix-daemon set-http-proxy action"
            '(#t)                                 ;one value, #t
            (marionette-eval '(with-shepherd-action 'guix-daemon
                                  ('set-http-proxy "http://localhost:8118")
                                  result
                                result)
                             marionette))

          (test-equal "guix-daemon set-http-proxy action, clear"
            '(#t)                                 ;one value, #t
            (marionette-eval '(with-shepherd-action 'guix-daemon
                                  ('set-http-proxy)
                                  result
                                result)
                             marionette))

          (test-assert "screendump"
            (begin
              (let ((capture
                     (string-append #$output "/tty1.ppm")))
                (marionette-control
                 (string-append "screendump " capture) marionette)
                (file-exists? capture))))

          (test-assert "screen text"
            (wait-for-screen-text
             marionette
             (lambda (text)
               ;; Check whether the welcome message and shell prompt are
               ;; displayed.  Note: OCR confuses "y" and "V" for instance, so
               ;; we cannot reliably match the whole text.
               (and (string-contains text "This is the GNU")
                    (string-contains text
                                     (string-append
                                      "root@"
                                      #$(operating-system-host-name os)))))
             #:ocr #$(file-append ocrad "/bin/ocrad")))

          (test-end))))

  (gexp->derivation name test))

(define* (test-basic-os #:optional (kernel linux-libre))
  (system-test
   (name (if (eq? kernel linux-libre)
             "basic"
             (string-append (package-name kernel) "-"
                            (version-major+minor (package-version kernel)))))
   (description
    "Instrument %SIMPLE-OS, run it in a VM, and run a series of basic
functionality tests, using the given KERNEL.")
   (value
    (let* ((os  (marionette-operating-system
                 (operating-system
                   (inherit %simple-os)
                   (kernel kernel))
                 #:imported-modules '((gnu services herd)
                                      (guix combinators))))
           (vm  (virtual-machine os)))
      ;; XXX: Add call to 'virtualized-operating-system' to get the exact same
      ;; set of services as the OS produced by
      ;; 'system-qemu-image/shared-store-script'.
      (run-basic-test (virtualized-operating-system os '())
                      #~(list #$vm)
                      name)))))

(define %test-basic-os
  (test-basic-os))

;; Ensure the LTS kernels are up to snuff, too.
(define %test-linux-libre-6.12
  (test-basic-os linux-libre-6.12))

(define %test-linux-libre-6.6
  (test-basic-os linux-libre-6.6))

(define %test-linux-libre-6.1
  (test-basic-os linux-libre-6.1))

(define %test-linux-libre-5.15
  (test-basic-os linux-libre-5.15))

(define %test-linux-libre-5.10
  (test-basic-os linux-libre-5.10))

(define %test-linux-libre-5.4
  (test-basic-os linux-libre-5.4))


;;;
;;; Halt.
;;;

(define (run-halt-test vm)
  ;; As reported in <http://bugs.gnu.org/26931>, running tmux would previously
  ;; lead the 'stop' method of 'user-processes' to an infinite loop, with the
  ;; tmux server process as a zombie that remains in the list of processes.
  ;; This test reproduces this scenario.
  (define test
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (gnu build marionette))

          (define marionette
            (make-marionette '(#$vm)))

          (define ocrad
            #$(file-append ocrad "/bin/ocrad"))

          ;; Wait for tty1 and log in.
          (marionette-eval '(begin
                              (use-modules (gnu services herd))
                              (start-service 'term-tty1))
                           marionette)
          (marionette-type "root\n" marionette)

          ;; Start tmux and wait for it to be ready.
          (marionette-type "tmux new-session 'echo 1 > /ready; bash'\n"
                           marionette)
          (wait-for-file "/ready" marionette)

          ;; Make sure to stop the test after a while.
          (sigaction SIGALRM (lambda _
                               (format (current-error-port)
                                       "FAIL: Time is up, but VM still running.\n")
                               (primitive-exit 1)))
          (alarm 10)

          ;; Get debugging info.
          (marionette-eval '(current-output-port
                             (open-file "/dev/console" "w0"))
                           marionette)
          (marionette-eval '(system* #$(file-append procps "/bin/ps")
                                     "-eo" "pid,ppid,stat,comm")
                           marionette)

          ;; See if 'halt' actually works.
          (marionette-eval '(system* "/run/current-system/profile/sbin/halt")
                           marionette)

          ;; If we reach this line, that means the VM was properly stopped in
          ;; a timely fashion.
          (alarm 0)
          (call-with-output-file #$output
            (lambda (port)
              (display "success!" port))))))

  (gexp->derivation "halt" test))

(define %test-halt
  (system-test
   (name "halt")
   (description
    "Use the 'halt' command and make sure it succeeds and does not get stuck
in a loop.  See <http://bugs.gnu.org/26931>.")
   (value
    (let ((os (marionette-operating-system
               (operating-system
                 (inherit %simple-os)
                 (packages (cons tmux %base-packages)))
               #:imported-modules '((gnu services herd)
                                    (guix combinators)))))
      (run-halt-test (virtual-machine os))))))


;;;
;;; Root cleanly unmounted.
;;;

(define (run-root-unmount-test os)
  (define test-image
    (image (operating-system os)
           (format 'compressed-qcow2)
           (volatile-root? #f)
           (shared-store? #f)
           (partition-table-type 'mbr)
           (partitions
            (list (partition
                   (size 'guess)
                   (offset (* 512 2048))          ;leave room for GRUB
                   (flags '(boot))
                   (label "root-under-test")))))) ;max 16 characters!

  (define observer-os
    (marionette-operating-system
     %simple-os
     #:imported-modules
     (source-module-closure '((guix build syscalls)
                              (gnu build file-systems)))))

  (define test
    (with-imported-modules (source-module-closure
                            '((gnu build marionette)
                              (guix build utils)))
      #~(begin
          (use-modules (gnu build marionette)
                       (guix build utils)
                       (srfi srfi-64)
                       (ice-9 ftw))

          (define image
            "/tmp/writable-image.qcow2")

          (define (test-system-marionette)
            ;; Return a marionette on a system where we'll run 'halt'.
            (invoke #$(file-append qemu-minimal "/bin/qemu-img")
                    "create" "-f" "qcow2" image "3G"
                    "-b" #$(system-image test-image) "-F" "qcow2")
            (make-marionette
             `(,(string-append #$qemu-minimal "/bin/" (qemu-command))
               ,@(if (file-exists? "/dev/kvm")
                     '("-enable-kvm")
                     '())
               "-no-reboot"
               "-m" "1024"                        ;memory size, in MiB
               "-drive" ,(format #f "file=~a,if=virtio" image))))

          (define witness-size
            ;; Size of the /witness file.
            (* 20 (expt 2 20)))

          (test-runner-current (system-test-runner #$output))
          (test-begin "root-unmount")

          (let ((marionette (test-system-marionette)))
            (test-assert "file created"
              (marionette-eval `(begin
                                  (use-modules (guix build utils))
                                  (call-with-output-file "/witness"
                                    (lambda (port)
                                      (call-with-input-file "/dev/random"
                                        (lambda (input)
                                          (dump-port input port
                                                     ,witness-size))))))
                               marionette))

            ;; Halt the system.
            (marionette-eval '(system* "/run/current-system/profile/sbin/halt")
                             marionette)

            (display "waiting for marionette to complete...")
            (force-output)
            (false-if-exception (waitpid (marionette-pid marionette)))
            (display " done\n")
            (force-output))

          ;; Remove the sockets used by the marionette above to avoid
          ;; EADDRINUSE.
          (for-each delete-file
                    (find-files "/tmp" (lambda (file stat)
                                         (eq? (stat:type stat) 'socket))))

          ;; Now boot another system and check whether the root file system of
          ;; the first one was cleanly unmounted.

          (let ((observer
                 (make-marionette (list #$(virtual-machine observer-os)
                                        "-drive"
                                        (format #f "file=~a,if=virtio" image)))))
            (test-assert "partitions"
              (marionette-eval '(begin
                                  (use-modules (gnu build file-systems))
                                  (disk-partitions))
                               observer))

            (test-assert "partition found"
              (marionette-eval '(find-partition-by-label "root-under-test")
                               observer))

            (test-assert "root file system is clean"
              (marionette-eval '(cleanly-unmounted-ext2?
                                 (find-partition-by-label "root-under-test"))
                               observer))

            (test-equal "root file system contains /witness"
              witness-size
              (let ((files (marionette-eval
                            '(begin
                               (use-modules (guix build syscalls)
                                            (ice-9 ftw))
                               (mount (find-partition-by-label "root-under-test")
                                      "/mnt" "ext4" MS_RDONLY)
                               (scandir "/mnt"))
                            observer)))
                (if (member "witness" files)
                    (marionette-eval '(stat:size (stat "/mnt/witness"))
                                     observer)
                    files))))

          (test-end))))

  (gexp->derivation "root-unmount" test))

(define %test-root-unmount
  (system-test
   (name "root-unmount")
   (description
    "Make sure the root file system is cleanly unmounted when the system is
halted.")
   (value
    (let ((os (marionette-operating-system %simple-os)))
      (run-root-unmount-test os)))))


;;;
;;; Cleanup of /tmp, /var/run, etc.
;;;

(define %cleanup-os
  (simple-operating-system
   (simple-service 'dirty-things
                   boot-service-type
                   (let ((script (plain-file
                                  "create-utf8-file.sh"
                                  (string-append
                                   "echo $0: dirtying /tmp...\n"
                                   "set -e; set -x\n"
                                   "touch /witness\n"
                                   "exec touch /tmp/λαμβδα"))))
                     (with-imported-modules '((guix build utils))
                       #~(begin
                           (setenv "PATH"
                                   #$(file-append coreutils "/bin"))
                           (invoke #$(file-append bash "/bin/sh")
                                   #$script)))))))

(define (run-cleanup-test name)
  (define os
    (marionette-operating-system %cleanup-os
                                 #:imported-modules '((gnu services herd)
                                                      (guix combinators))))
  (define test
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (gnu build marionette)
                       (srfi srfi-64)
                       (ice-9 match))

          (define marionette
            (make-marionette (list #$(virtual-machine os))))

          (test-runner-current (system-test-runner #$output))
          (test-begin "cleanup")

          (test-assert "dirty service worked"
            (marionette-eval '(file-exists? "/witness") marionette))

          (test-equal "/tmp cleaned up"
            '("." "..")
            (marionette-eval '(begin
                                (use-modules (ice-9 ftw))
                                (scandir "/tmp"))
                             marionette))

          (test-end))))

  (gexp->derivation "cleanup" test))

(define %test-cleanup
  ;; See <https://bugs.gnu.org/26353>.
  (system-test
   (name "cleanup")
   (description "Make sure the 'cleanup' service can remove files with
non-ASCII names from /tmp.")
   (value (run-cleanup-test name))))


;;;
;;; Activation: Order of activation scripts
;;; Create accounts before running scripts using them

(define %activation-os
  ;; System with a new user/group, a setuid/setgid binary and an activation script
  (let* ((%hello-accounts
           (list (user-group (name "hello") (system? #t))
                 (user-account
                  (name "hello")
                  (group "hello")
                  (system? #t)
                  (comment "")
                  (home-directory "/var/empty"))))
         (%hello-privileged
          (list
           (privileged-program
            (program (file-append hello "/bin/hello"))
            (setuid? #t)
            (setgid? #t)
            (user "hello")
            (group "hello"))))
         (%hello-activation
          (with-imported-modules (source-module-closure
                                  '((gnu build activation)))
            #~(begin
	        (use-modules (gnu build activation))

	        (let ((user (getpwnam "hello")))
	          (mkdir-p/perms "/run/hello" user #o755)))))

         (hello-service-type
          (service-type
           (name 'hello)
           (extensions
            (list (service-extension account-service-type
                                     (const %hello-accounts))
                  (service-extension activation-service-type
                                     (const %hello-activation))
	          (service-extension privileged-program-service-type
                                     (const %hello-privileged))))
           (default-value #f)
           (description ""))))

    (operating-system
      (inherit %simple-os)
      (services
       (cons* (service hello-service-type)
              (operating-system-user-services
               %simple-os))))))

(define (run-activation-test name)
  (define os
    (marionette-operating-system
     %activation-os))

  (define test
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (gnu build marionette)
                       (srfi srfi-64))

          (define marionette
            (make-marionette (list #$(virtual-machine os))))

          (test-runner-current (system-test-runner #$output))
          (test-begin "activation")

          (test-assert "directory exists"
            (marionette-eval
             '(file-exists? "/run/hello")
             marionette))

          (test-assert "directory correct permissions and owner"
            (marionette-eval
             '(let ((dir (stat "/run/hello"))
                    (user (getpwnam "hello")))
                (and (eqv? (stat:uid dir)
                           (passwd:uid user))
                     (eqv? (stat:gid dir)
                           (passwd:gid user))
                     (=    (stat:perms dir)
                           #o0755)))
             marionette))

          (test-assert "privileged-program exists"
            (marionette-eval
             '(file-exists? "/run/privileged/bin/hello")
             marionette))

          (test-assert "privileged-program correct permissions and owner"
            (marionette-eval
             '(let ((binary (stat "/run/privileged/bin/hello"))
                    (user (getpwnam "hello"))
                    (group (getgrnam "hello")))
                (and (eqv? (stat:uid binary)
                           (passwd:uid user))
                     (eqv? (stat:gid binary)
                           (group:gid group))
                     (=    (stat:perms binary)
                           (+ #o0555     ;; base
                              #o4000     ;; setuid
                              #o2000)))) ;; setgid
             marionette))

          (test-end))))

  (gexp->derivation name test))

(define %test-activation
  (system-test
   (name "activation")
   (description "Test that activation scripts are run in the correct order")
   (value (run-activation-test name))))


;;;
;;; Build daemon.
;;;

(define %daemon-os
  (operating-system-with-console-syslog
   (simple-operating-system)))

(define (manifest-entry-without-grafts entry)
  "Return ENTRY with grafts disabled on its contents."
  (manifest-entry
    (inherit entry)
    (item (with-parameters ((%graft? #f))
            (manifest-entry-item entry)))))

(define %hello-dependencies-manifest
  ;; Build dependencies of 'hello' needed to test 'guix build hello'.
  (concatenate-manifests
   (list (map-manifest-entries
          manifest-entry-without-grafts
          (package->development-manifest hello))

         ;; Add the source of 'hello'.
         (manifest
          (list (manifest-entry
                  (name "hello-source")
                  (version (package-version hello))
                  (item (let ((file (origin-actual-file-name
                                     (package-source hello))))
                          (computed-file
                           "hello-source"
                           #~(begin
                               ;; Put the tarball in a subdirectory since
                               ;; profile union crashes otherwise.
                               (mkdir #$output)
                               (mkdir (in-vicinity #$output "src"))
                               (symlink #$(package-source hello)
                                        (in-vicinity #$output
                                                     (string-append "src/"
                                                                    #$file))))))))))

         ;; Include 'guile-final', which is needed when building derivations
         ;; such as that of 'hello' but missing from the development manifest.
         ;; Add '%bootstrap-guile', used by 'guix install --bootstrap'.
         (map-manifest-entries
          manifest-entry-without-grafts
          (packages->manifest (list (canonical-package guile-3.0)
                                    %bootstrap-guile))))))

(define (guix-daemon-test-cases marionette)
  "Return a gexp with SRFI-64 test cases testing guix-daemon.  Those test are
evaluated in MARIONETTE, a gexp denoting a marionette (system under test).
Assume that an unprivileged account for 'user' exists on the system under
test."
  #~(begin
      (test-equal "guix describe"
        0
        (marionette-eval '(system* "guix" "describe")
                         #$marionette))

      (test-equal "hello not already built"
        #f
        ;; Check that the next test will really build 'hello'.
        (marionette-eval '(file-exists?
                           #$(with-parameters ((%graft? #f))
                               hello))
                         #$marionette))

      (test-equal "guix build hello"
        0
        ;; Check that guix-daemon is up and running and that the build
        ;; environment is properly set up (build users, etc.).
        (marionette-eval '(system* "guix" "build" "hello" "--no-grafts")
                         #$marionette))

      (test-assert "hello indeed built"
        (marionette-eval '(file-exists?
                           #$(with-parameters ((%graft? #f))
                               hello))
                         #$marionette))

      (test-equal "guix install hello"
        0
        ;; Check that ~/.guix-profile & co. are properly created.
        (marionette-eval '(let ((pw (getpwuid (getuid))))
                            (setenv "USER" (passwd:name pw))
                            (setenv "HOME" (pk 'home (passwd:dir pw)))
                            (system* "guix" "install" "hello"
                                     "--no-grafts" "--bootstrap"))
                         #$marionette))

      (test-equal "user profile created"
        0
        (marionette-eval '(system "ls -lad ~/.guix-profile")
                         #$marionette))

      (test-equal "hello"
        0
        (marionette-eval '(system "~/.guix-profile/bin/hello")
                         #$marionette))

      (test-equal "guix install hello, unprivileged user"
        0
        ;; Check that 'guix' is in $PATH for new users and that
        ;; ~user/.guix-profile also gets created, assuming that 'user' exists
        ;; as an unprivileged user account.
        (marionette-eval '(system "su - user -c \
'guix install hello --no-grafts --bootstrap'")
                         #$marionette))

      (test-equal "user hello"
        0
        (marionette-eval '(system "~user/.guix-profile/bin/hello")
                         #$marionette))

      (test-equal "unprivileged user profile created"
        0
        (marionette-eval '(system "ls -lad ~user/.guix-profile")
                         #$marionette))

      (test-equal "store is read-only"
        EROFS
        (marionette-eval '(catch 'system-error
                            (lambda ()
                              (mkdir (in-vicinity #$(%store-prefix)
                                                  "whatever"))
                              0)
                            (lambda args
                              (system-error-errno args)))
                         #$marionette))))

(define (run-guix-daemon-test os)
  (define test-image
    (image (operating-system os)
           (format 'compressed-qcow2)
           (volatile-root? #f)
           (shared-store? #f)
           (partition-table-type 'mbr)
           (partitions
            (list (partition
                   (size (* 4 (expt 2 30)))
                   (offset (* 512 2048))          ;leave room for GRUB
                   (flags '(boot))
                   (label "root"))))))

  (define test
    (with-imported-modules (source-module-closure
                            '((gnu build marionette)
                              (guix build utils)))
      #~(begin
          (use-modules (gnu build marionette)
                       (guix build utils)
                       (srfi srfi-64))

          (define marionette
            (make-marionette
             (list (string-append #$qemu-minimal "/bin/" (qemu-command))
                   #$@(common-qemu-options (system-image test-image) '()
                                           #:image-format "qcow2"
                                           #:rw-image? #t)
                   "-m" "512"
                   "-nographic" "-serial" "stdio"
                   "-snapshot")))

          (test-runner-current (system-test-runner #$output))
          (test-begin "guix-daemon")

          #$(guix-daemon-test-cases #~marionette)

          (test-end))))

  (gexp->derivation "guix-daemon-test" test))

(define %test-guix-daemon
  (system-test
   (name "guix-daemon")
   (description
    "Test 'guix-daemon' behavior on a multi-user system.")
   (value
    (let ((os (marionette-operating-system
               (operating-system
                 (inherit (operating-system-with-gc-roots
                           %daemon-os
                           (list (profile
                                  (name "hello-build-dependencies")
                                  (content %hello-dependencies-manifest)))))
                 (kernel-arguments '("console=ttyS0"))
                 (users (cons (user-account
                               (name "user")
                               (group "users"))
                              %base-user-accounts)))
               #:imported-modules '((gnu services herd)
                                    (guix combinators)))))
      (run-guix-daemon-test os)))))
