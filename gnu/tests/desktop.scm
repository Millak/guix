;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2021, 2023 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2021 muradm <mail@muradm.net>
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

(define-module (gnu tests desktop)
  #:use-module (gnu tests)
  #:use-module (gnu packages shells)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services dbus)
  #:use-module (gnu services desktop)
  #:use-module (gnu system)
  #:use-module (gnu system vm)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:export (%test-elogind
            %test-minimal-desktop))


;;;
;;; Elogind.
;;;

(define (run-elogind-test vm)
  (define test
    (with-imported-modules '((gnu build marionette)
                             (guix build syscalls))
      #~(begin
          (use-modules (gnu build marionette)
                       (guix build syscalls)
                       (srfi srfi-64))

          (define marionette
            (make-marionette '(#$vm)))

          (test-runner-current (system-test-runner #$output))
          (test-begin "elogind")

          ;; Log in as root on tty1, and check what 'loginctl' returns.
          (test-equal "login on tty1"
            '(("c1" "0" "root" "seat0" "tty1" "active" "no" "-") ;session
              ("seat0")                                          ;seat
              ("0" "root" "no" "active"))                        ;user

            (begin
              ;; Wait for tty1.
              (marionette-eval
               '(begin
                  (use-modules (gnu services herd))
                  (start-service 'term-tty1)
                  (start-service 'elogind))
               marionette)
              (marionette-control "sendkey ctrl-alt-f1" marionette)

              ;; Now we can type.
              (marionette-type "root\n" marionette)
              (marionette-type "loginctl list-users --no-legend > users\n"
                               marionette)
              (marionette-type "loginctl list-seats --no-legend > seats\n"
                               marionette)
              (marionette-type "loginctl list-sessions --no-legend > sessions\n"
                               marionette)


              ;; Read the three files.
              (marionette-eval '(use-modules (rnrs io ports)) marionette)
              (let ((guest-file (lambda (file)
                                  (string-tokenize
                                   (wait-for-file file marionette
                                                  #:read 'get-string-all)))))
                (list (guest-file "/root/sessions")
                      (guest-file "/root/seats")
                      (guest-file "/root/users")))))

          (test-assert "screendump"
            (begin
              (let ((capture (string-append #$output "/tty1.ppm")))
                (marionette-control
                 (string-append "screendump " capture) marionette)
                (file-exists? capture))))

          (test-end))))

  (gexp->derivation "elogind" test))

(define %test-elogind
  (system-test
   (name "elogind")
   (description
    "Test whether we can log in when elogind is enabled, and whether
'loginctl' reports accurate user, session, and seat information.")
   (value
    (let ((os (marionette-operating-system
               (simple-operating-system
                (service elogind-service-type)
                (service polkit-service-type)
                (service dbus-root-service-type))
               #:imported-modules '((gnu services herd)
                                    (guix combinators)))))
      (run-elogind-test (virtual-machine os))))))


;;;
;;; Seatd/greetd based minimal desktop
;;;

(define %minimal-services
  (append
   (modify-services %base-services
     ;; greetd-service-type provides "greetd" PAM service
     (delete login-service-type)
     ;; and can be used in place of mingetty-service-type
     (delete mingetty-service-type))
   (list
    (service seatd-service-type)
    (service greetd-service-type
             (greetd-configuration
              (greeter-supplementary-groups '("input" "video"))
              (terminals
               (list
                ;; we can make any terminal active by default
                (greetd-terminal-configuration (terminal-vt "1") (terminal-switch #t))
                ;; we can make environment without XDG_RUNTIME_DIR set
                ;; even provide our own environment variables
                (greetd-terminal-configuration
                 (terminal-vt "2")
                 (default-session-command
                   (greetd-agreety-session
                    (command
                     (greetd-user-session
                      (extra-env '(("MY_VAR" . "1")))
                      (xdg-env? #f))))))
                ;; we can use different shell instead of default bash
                (greetd-terminal-configuration
                 (terminal-vt "3")
                 (default-session-command
                   (greetd-agreety-session
                    (command
                     (greetd-user-session
                      (command (file-append zsh "/bin/zsh"))
                      (command-args '("-l"))
                      (extra-env '(("MY_VAR" . "1")))
                      (xdg-env? #f))))))
                ;; we can use any other executable command as greeter
                (greetd-terminal-configuration
                 (terminal-vt "4")
                 (default-session-command (program-file "my-noop-greeter" #~(exit))))
                (greetd-terminal-configuration (terminal-vt "5"))
                (greetd-terminal-configuration (terminal-vt "6"))))))
    ;; mingetty-service-type can be used in parallel
    ;; if needed to do so, do not (delete login-service-type)
    ;; as illustrated above
    #| (service mingetty-service-type (mingetty-configuration (tty "tty8"))) |#)))

(define-syntax-rule (minimal-operating-system user-services ...)
  "Return an operating system that includes USER-SERVICES in addition to
minimal %BASE-SERVICES."
  (operating-system (inherit %simple-os)
                    (services (cons* user-services ... %minimal-services))))

(define (run-minimal-desktop-test os vm)
  (define test
    (with-imported-modules '((gnu build marionette)
                             (guix build syscalls))
      #~(begin
          (use-modules (gnu build marionette)
                       (guix build syscalls)
                       (srfi srfi-1)
                       (srfi srfi-64)
                       (ice-9 pretty-print))

          (define marionette
            (make-marionette #$vm))

          (define (file-get-all-strings fname)
            (marionette-eval '(use-modules (rnrs io ports)) marionette)
            (wait-for-file fname marionette #:read 'get-string-all))

          (define (wait-for-unix-socket-m socket)
            (wait-for-unix-socket socket marionette))

          (mkdir #$output)
          (chdir #$output)

          (test-runner-current (system-test-runner #$output))
          (test-begin "minimal-desktop")

          (test-assert "seatd is ready"
            (wait-for-unix-socket-m "/run/seatd.sock"))

          (test-equal "login user on tty1"
            "alice\n"
            (begin
              ;; Wait for tty1.
              (marionette-eval
               '(begin
                  (use-modules (gnu services herd))
                  (start-service 'term-tty1))
               marionette)
              (marionette-control "sendkey ctrl-alt-f1" marionette)

              ;; login as root change alice password and exit
              ;; then login as alice
              (for-each
               (lambda (cmd) (marionette-type cmd marionette) (sleep 1))
               (list
                "root\n"
                "passwd alice\n"
                "alice\n"
                "alice\n"
                "exit\n"
                "alice\n"
                "alice\n"
                "id -un > logged-in\n"))

              (file-get-all-strings "/home/alice/logged-in")))

          (test-equal "validate user environment"
            '("SEATD_SOCK=/run/seatd.sock"
              "XDG_RUNTIME_DIR=/run/user/1000"
              "XDG_SEAT=seat0"
              "XDG_VTNR=1")

            (begin
              (marionette-type "env > env\n" marionette)
              (sleep 1)

              (define user-env (string-tokenize
                                (file-get-all-strings "/home/alice/env")))

              (define (expected-var var)
                (any (lambda (s) (string-contains var s))
                     '("SEATD_SOCK"
                       "XDG_RUNTIME_DIR"
                       "XDG_SEAT"
                       "XDG_VTNR")))

              (sort (filter expected-var user-env) string<?)))

          (test-assert "validate SEATD_SOCK"
            (begin
              (marionette-type "env > env\n" marionette)
              (sleep 1)

              (define (sock-var? var)
                (any (lambda (s) (string-contains var s))
                     '("SEATD_SOCK")))

              (define (sock-var-sock var)
                (car (cdr (string-split var #\=))))

              (let*
                  ((out (file-get-all-strings "/home/alice/env"))
                   (out (string-tokenize out))
                   (out (filter sock-var? out))
                   (socks (map sock-var-sock out))
                   (socks (map wait-for-unix-socket-m socks)))
                (and (= 1 (length socks)) (every identity socks)))))

          (test-equal "seatd.sock ownership"
            '("root" "seat")
            `(,(marionette-eval
                '(passwd:name (getpwuid (stat:uid (stat "/run/seatd.sock"))))
                marionette)
              ,(marionette-eval
                '(group:name (getgrgid (stat:gid (stat "/run/seatd.sock"))))
                marionette)))

          (test-assert "greetd is ready"
            (begin
              (marionette-type "ps -C greetd -o pid,args --no-headers > ps-greetd\n"
                               marionette)
              (sleep 1)

              (define (greetd-daemon? cmd)
                (string-contains cmd "config"))

              (define (greetd-cmd-to-pid cmd)
                (car (string-split cmd #\space)))

              (define (greetd-pid-to-sock pid)
                (string-append "/run/greetd-" pid ".sock"))

              (let* ((out (file-get-all-strings "/home/alice/ps-greetd"))
                     (out (string-split out #\newline))
                     (out (map string-trim-both out))
                     (out (filter greetd-daemon? out))
                     (pids (map greetd-cmd-to-pid out))
                     (socks (map greetd-pid-to-sock pids))
                     (socks (map wait-for-unix-socket-m socks)))
                (every identity socks))))

          ;; a bit weak, but tests everything at once actually
          (test-equal "check /run/user/<uid> mounted and writable"
            "alice\n"
            (begin
              (marionette-type "echo alice > /run/user/1000/test\n" marionette)
              (file-get-all-strings "/run/user/1000/test")))

          (test-equal "check greeter user has correct groups"
            "greeter input video\n"
            (begin
              (marionette-type "id -Gn greeter > /run/user/1000/greeter-groups\n"
                               marionette)
              (file-get-all-strings "/run/user/1000/greeter-groups")))

          (test-assert "screendump"
            (begin
              (marionette-control (string-append "screendump " #$output
                                                 "/tty1.ppm")
                                  marionette)
              (file-exists? "tty1.ppm")))

          (test-end))))

  (gexp->derivation "minimal-desktop" test))

(define %test-minimal-desktop
  (system-test
   (name "minimal-desktop")
   (description
    "Test whether we can log in when seatd and greetd is enabled")
   (value
    (let* ((os (marionette-operating-system
                (minimal-operating-system)
                #:imported-modules '((gnu services herd)
                                     (guix combinators))))
           (vm (virtual-machine os)))
      (run-minimal-desktop-test (virtualized-operating-system os '())
                                #~(list #$vm))))))
