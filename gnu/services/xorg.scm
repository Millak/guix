;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Andy Wingo <wingo@igalia.com>
;;; Copyright © 2013-2017, 2019-2020, 2022, 2024 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2018, 2019 Timothy Sample <samplet@ngyro.com>
;;; Copyright © 2019 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2019 Tim Gesthuizen <tim.gesthuizen@yahoo.de>
;;; Copyright © 2020 shtwzrd <shtwzrd@protonmail.com>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2020 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2021 Brice Waegeneire <brice@waegenei.re>
;;; Copyright © 2021 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2021 Josselin Poiret <josselin.poiret@protonmail.ch>
;;; Copyright © 2022 Chris Marusich <cmmarusich@gmail.com>
;;; Copyright © 2022 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2023 muradm <mail@muradm.net>
;;; Copyright © 2024 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2024 Tomas Volf <~@wolfsden.cz>
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

(define-module (gnu services xorg)
  #:autoload   (gnu services sddm) (sddm-service-type)
  #:use-module (gnu artwork)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system keyboard)
  #:use-module (gnu system pam)
  #:use-module (gnu system privilege)
  #:use-module (gnu services base)
  #:use-module (gnu services dbus)
  #:use-module (gnu packages base)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages display-managers)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gnustep)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages linux)
  #:use-module (gnu system shadow)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system trivial)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module ((guix modules) #:select (source-module-closure))
  #:use-module (guix packages)
  #:use-module (guix derivations)
  #:use-module (guix records)
  #:use-module (guix deprecation)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:export (xorg-configuration
            xorg-configuration?
            xorg-configuration-modules
            xorg-configuration-fonts
            xorg-configuration-drivers
            xorg-configuration-resolutions
            xorg-configuration-extra-config
            xorg-configuration-server
            xorg-configuration-server-arguments
            xorg-configuration-keyboard-layout

            %default-xorg-modules
            %default-xorg-fonts
            %default-xorg-server-arguments

            xorg-wrapper
            xorg-start-command
            xorg-start-command-xinit
            xinitrc
            xorg-server-service-type
            startx-command-service-type

            %default-slim-theme
            %default-slim-theme-name

            slim-configuration
            slim-configuration?
            slim-configuration-slim
            slim-configuration-allow-empty-passwords?
            slim-configuration-auto-login?
            slim-configuration-default-user
            slim-configuration-theme
            slim-configuration-theme-name
            slim-configuration-xauth
            slim-configuration-shepherd
            slim-configuration-auto-login-session
            slim-configuration-xorg
            slim-configuration-display
            slim-configuration-vt
            slim-configuration-sessreg

            slim-service-type

            screen-locker-configuration
            screen-locker-configuration?
            screen-locker-configuration-name
            screen-locker-configuration-program
            screen-locker-configuration-allow-empty-password?
            screen-locker-configuration-using-pam?
            screen-locker-configuration-using-setuid?
            screen-locker-service-type
            screen-locker-service  ; deprecated

            localed-configuration
            localed-configuration?
            localed-service-type

            dconf-keyfile
            dconf-profile
            dconf-profile-name
            dconf-profile-content
            dconf-profile-keyfile
            dconf-service-type

            gdm-configuration
            gdm-service-type

            handle-xorg-configuration
            set-xorg-configuration))

;;; Commentary:
;;;
;;; Services that relate to the X Window System.
;;;
;;; Code:

(define %default-xorg-modules
  ;; Default list of modules loaded by the server.  When multiple drivers
  ;; match, the first one in the list is loaded.
  (list xf86-video-vesa
        xf86-video-fbdev
        xf86-video-amdgpu
        xf86-video-ati
        xf86-video-cirrus
        xf86-video-intel
        xf86-video-mach64
        xf86-video-nouveau
        xf86-video-nv
        xf86-video-sis

        ;; Libinput is the new thing and is recommended over evdev/synaptics:
        ;; <http://who-t.blogspot.fr/2015/01/xf86-input-libinput-compatibility-with.html>.
        xf86-input-libinput
        xf86-input-evdev
        xf86-input-mouse))

(define %default-xorg-fonts
  ;; Default list of fonts available to the X server.
  (list (file-append font-alias "/share/fonts/X11/75dpi")
        (file-append font-alias "/share/fonts/X11/100dpi")
        (file-append font-alias "/share/fonts/X11/misc")
        (file-append font-alias "/share/fonts/X11/cyrillic")
        (file-append font-misc-misc               ;default fonts for xterm
                     "/share/fonts/X11/misc")
        (file-append font-adobe75dpi "/share/fonts/X11/75dpi")))

(define %default-xorg-server-arguments
  ;; Default command-line arguments for X.
  '("-nolisten" "tcp"))

;; Configuration of an Xorg server.
(define-record-type* <xorg-configuration>
  xorg-configuration make-xorg-configuration
  xorg-configuration?
  (modules          xorg-configuration-modules    ;list of file-like
                    (thunked)
                    ; filter out modules not supported on current system
                    (default (filter
                              (lambda (p)
                                (member (%current-system)
                                        (package-supported-systems p)))
                              %default-xorg-modules)))
  (fonts            xorg-configuration-fonts      ;list of packges
                    (default %default-xorg-fonts))
  (drivers          xorg-configuration-drivers    ;list of strings
                    (default '()))
  (resolutions      xorg-configuration-resolutions ;list of tuples
                    (default '()))
  (keyboard-layout  xorg-configuration-keyboard-layout ;#f | <keyboard-layout>
                    (default #f))
  (extra-config     xorg-configuration-extra-config ;list of strings
                    (default '()))
  (server           xorg-configuration-server     ;file-like
                    (default xorg-server))
  (server-arguments xorg-configuration-server-arguments ;list of strings
                    (default %default-xorg-server-arguments)))

(define (xorg-configuration->file config)
  "Compute an Xorg configuration file corresponding to CONFIG, an
<xorg-configuration> record."
  (let ((xorg-server (xorg-configuration-server config)))
    (define all-modules
      ;; 'xorg-server' provides 'fbdevhw.so' etc.
      (append (xorg-configuration-modules config)
              (list xorg-server)))

    (define build
      #~(begin
          (use-modules (ice-9 match)
                       (srfi srfi-1)
                       (srfi srfi-26))

          (call-with-output-file #$output
            (lambda (port)
              (define drivers
                '#$(xorg-configuration-drivers config))

              (define (device-section driver)
                (string-append "
Section \"Device\"
  Identifier \"device-" driver "\"
  Driver \"" driver "\"
EndSection"))

              (define (screen-section driver resolutions)
                (string-append "
Section \"Screen\"
  Identifier \"screen-" driver "\"
  Device \"device-" driver "\"
  SubSection \"Display\"
    Modes "
  (string-join (map (match-lambda
                      ((x y)
                       (string-append "\"" (number->string x)
                                      "x" (number->string y) "\"")))
                    resolutions)) "
  EndSubSection
EndSection"))

              (define (input-class-section layout variant model options)
                (string-append "
Section \"InputClass\"
  Identifier \"evdev keyboard catchall\"
  MatchIsKeyboard \"on\"
  Option \"XkbLayout\" " (object->string layout)
  (if variant
      (string-append "  Option \"XkbVariant\" \""
                     variant "\"")
      "")
  (if model
      (string-append "  Option \"XkbModel\" \""
                     model "\"")
      "")
  (match options
    (()
     "")
    (_
     (string-append "  Option \"XkbOptions\" \""
                    (string-join options ",") "\""))) "

  MatchDevicePath \"/dev/input/event*\"
  Driver \"evdev\"
EndSection\n"))

              (define (expand modules)
                ;; Append to MODULES the relevant /lib/xorg/modules
                ;; sub-directories.
                (append-map (lambda (module)
                              (filter-map (lambda (directory)
                                            (let ((full (string-append module
                                                                       directory)))
                                              (and (file-exists? full)
                                                   full)))
                                          '("/lib/xorg/modules/drivers"
                                            "/lib/xorg/modules/input"
                                            "/lib/xorg/modules/multimedia"
                                            "/lib/xorg/modules/extensions")))
                            modules))

              (display "Section \"Files\"\n" port)
              (for-each (lambda (font)
                          (format port "  FontPath \"~a\"~%" font))
                        '#$(xorg-configuration-fonts config))
              (for-each (lambda (module)
                          (format port
                                  "  ModulePath \"~a\"~%"
                                  module))
                        (append (expand '#$all-modules)

                                ;; For fbdevhw.so and so on.
                                (list #$(file-append xorg-server
                                                     "/lib/xorg/modules"))))
              (display "EndSection\n" port)
              (display "
Section \"ServerFlags\"
  Option \"AllowMouseOpenFail\" \"on\"
EndSection\n" port)

              (display (string-join (map device-section drivers) "\n")
                       port)
              (newline port)
              (display (string-join
                        (map (cut screen-section <>
                                  '#$(xorg-configuration-resolutions config))
                             drivers)
                        "\n")
                       port)
              (newline port)

              (let ((layout  #$(and=> (xorg-configuration-keyboard-layout config)
                                      keyboard-layout-name))
                    (variant #$(and=> (xorg-configuration-keyboard-layout config)
                                      keyboard-layout-variant))
                    (model   #$(and=> (xorg-configuration-keyboard-layout config)
                                      keyboard-layout-model))
                    (options '#$(and=> (xorg-configuration-keyboard-layout config)
                                       keyboard-layout-options)))
                (when layout
                  (display (input-class-section layout variant model options)
                           port)
                  (newline port)))

              (for-each (lambda (config)
                          (display config port))
                        '#$(xorg-configuration-extra-config config))))))

    (computed-file "xserver.conf" build)))

(define (xorg-configuration-directory modules)
  "Return a directory that contains the @code{.conf} files for X.org that
includes the @code{share/X11/xorg.conf.d} directories of each package listed
in @var{modules}."
  (with-imported-modules '((guix build utils))
    (computed-file "xorg.conf.d"
                   #~(begin
                       (use-modules (guix build utils)
                                    (srfi srfi-1))

                       (define files
                         (append-map (lambda (module)
                                       (find-files (string-append
                                                    module
                                                    "/share/X11/xorg.conf.d")
                                                   "\\.conf$"))
                                     (list #$@modules)))

                       (mkdir #$output)
                       (for-each (lambda (file)
                                   (symlink file
                                            (string-append #$output "/"
                                                           (basename file))))
                                 files)
                       #t))))

(define (xorg-configuration-server-package-path config input path)
  "Lookup the direct @var{input} in the xorg server package of @var{config}
and append @var{path} to it."
  (let* ((server (xorg-configuration-server config))
         (package (lookup-package-direct-input server input)))
    (when package (file-append package path))))

(define (xorg-configuration-dri-driver-path config)
  (xorg-configuration-server-package-path config "mesa" "/lib/dri"))

(define (xorg-configuration-xkb-bin-dir config)
  (xorg-configuration-server-package-path config "xkbcomp" "/bin"))

(define (xorg-configuration-xkb-dir config)
  (xorg-configuration-server-package-path config "xkeyboard-config" "/share/X11/xkb"))

(define* (xorg-wrapper #:optional (config (xorg-configuration)))
  "Return a derivation that builds a script to start the X server with the
given @var{config}.  The resulting script should be used in place of
@code{/usr/bin/X}."
  (define exp
    ;; Write a small wrapper around the X server.
    #~(begin
        (setenv "XORG_DRI_DRIVER_PATH"
                #$(xorg-configuration-dri-driver-path config))
        (setenv "XKB_BINDIR" #$(xorg-configuration-xkb-bin-dir config))

        (let ((X (string-append #$(xorg-configuration-server config) "/bin/X")))
          (apply execl X X
                 "-xkbdir" #$(xorg-configuration-xkb-dir config)
                 "-config" #$(xorg-configuration->file config)
                 "-configdir" #$(xorg-configuration-directory
                                 (xorg-configuration-modules config))
                 (cdr (command-line))))))

  (program-file "X-wrapper" exp))

(define* (xorg-start-command #:optional (config (xorg-configuration)))
  "Return a @code{startx} script in which the modules, fonts, etc. specified
in @var{config}, are available.  The result should be used in place of
@code{startx}."
  (define X
    (xorg-wrapper config))

  (define exp
    ;; Write a small wrapper around the X server.
    #~(apply execl #$X #$X ;; Second #$X is for argv[0].
             "-logverbose" "-verbose" "-terminate"
             #$@(xorg-configuration-server-arguments config)
              (cdr (command-line))))

  (program-file "startx" exp))

(define* (xorg-start-command-xinit #:optional (config (xorg-configuration)))
  "Return a @code{startx} script in which the modules, fonts, etc. specified
in @var{config}, are available.  The result should be used in place of
@code{startx}.  Compared to the @code{xorg-start-command} it calls xinit,
therefore it works well when executed from tty."
  (define X
    (xorg-wrapper config))

  (define exp
    ;; Small wrapper providing subset of functionality of typical startx
    ;; script from distributions like alpine.
    (with-imported-modules (source-module-closure '((guix build utils)))
      #~(begin
          (use-modules (guix build utils)
                       (ice-9 popen)
                       (ice-9 textual-ports))

          (define (capture-stdout . prog+args)
            (let* ((port (apply open-pipe* OPEN_READ prog+args))
                   (data (get-string-all port)))
              (if (zero? (status:exit-val (close-pipe port)))
                  (string-trim-right data #\newline)
                  (error "Command failed: " prog+args))))

          (define (determine-unused-display n)
            (let ((lock-file (format #f "/tmp/.X~a-lock" n))
                  (sock-file (format #f "/tmp/.X11-unix/X~a" n)))
              (if (or (file-exists? lock-file)
                      (false-if-exception
                       (eq? 'socket (stat:type (stat sock-file)))))
                  (determine-unused-display (+ n 1))
                  (format #f ":~a" n))))

          (define (vty-from-fd0)
            (let ((fd0 (readlink "/proc/self/fd/0"))
                  (pref "/dev/tty"))
              (if (string-prefix? pref fd0)
                  (substring fd0 (string-length pref))
                  (error (format #f "Cannot determine VT from: ~a" fd0)))))

          (define (determine-vty)
            (string-append "vt" (or (getenv "XDG_VTNR") (vty-from-fd0))))

          (define (enable-xauth server-auth-file display)
            ;; Configure and enable X authority
            (or (getenv "XAUTHORITY")
                (setenv "XAUTHORITY" (string-append (getenv "HOME") "/.Xauthority")))

            (let* ((bin/xauth #$(file-append xauth "/bin/xauth"))
                   (bin/mcookie #$(file-append util-linux "/bin/mcookie"))
                   (mcookie (capture-stdout bin/mcookie)))
              (invoke bin/xauth "-qf" server-auth-file
                      "add" display "." mcookie)
              (invoke bin/xauth "-q"
                      "add" display "." mcookie)))

          (let* ((xinit #$(file-append xinit "/bin/xinit"))
                 (display (determine-unused-display 0))
                 (vty (determine-vty))
                 (server-auth-port (mkstemp "/tmp/serverauth.XXXXXX"))
                 (server-auth-file (port-filename server-auth-port)))
            (close-port server-auth-port)
            (enable-xauth server-auth-file display)
            (apply execl
                   xinit
                   xinit
                   "--"
                   #$X
                   display
                   vty
                   "-keeptty"
                   "-auth" server-auth-file
                   ;; These are set by xorg-start-command, so do the same to keep
                   ;; it consistent.
                   "-logverbose" "-verbose" "-terminate"
                   #$@(xorg-configuration-server-arguments config)
                   (cdr (command-line)))))))

  (program-file "startx" exp))

(define (startx-command-profile-service config)
  ;; XXX: profile-service-type only accepts <package> objects.
  (package
    (name "startx-profile-package")
    (version "0")
    (source (xorg-start-command-xinit config))
    (build-system trivial-build-system)
    (arguments
     (list
      #:modules '((guix build utils))
      #:builder
      #~(begin
          (use-modules (guix build utils))
          (let ((bin (string-append #$output "/bin")))
            (mkdir-p bin)
            (symlink #$source (string-append bin "/startx"))))))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))

(define startx-command-service-type
  (service-type
   (name 'startx-command)
   (extensions
    (list (service-extension profile-service-type
                             (compose list startx-command-profile-service))))
   (default-value (xorg-configuration))
   (description "Add @command{startx} to the system profile.")))



(define* (xinitrc #:key fallback-session)
  "Return a system-wide xinitrc script that starts the specified X session,
which should be passed to this script as the first argument.  If not, the
@var{fallback-session} will be used or, if @var{fallback-session} is false, a
desktop session from the system or user profile will be used."
  (define builder
    #~(begin
        (use-modules (ice-9 match)
                     (ice-9 regex)
                     (ice-9 ftw)
                     (ice-9 rdelim)
                     (srfi srfi-1)
                     (srfi srfi-26))

        (define (close-all-fdes)
          ;; Close all the open file descriptors except 0 to 2.
          (let loop ((fd 3))
            (when (< fd 4096)               ;FIXME: use sysconf + _SC_OPEN_MAX
              (false-if-exception (close-fdes fd))
              (loop (+ 1 fd)))))

        (define (exec-from-login-shell command . args)
          ;; Run COMMAND from a login shell so that it gets to see the same
          ;; environment variables that one gets when logging in on a tty, for
          ;; instance.
          (let* ((pw    (getpw (getuid)))
                 (shell (passwd:shell pw)))
            ;; Close any open file descriptors.  This is all the more
            ;; important that SLiM itself exec's us directly without closing
            ;; its own file descriptors!
            (close-all-fdes)

            ;; The '--login' option is supported at least by Bash and zsh.
            (execl shell shell "--login" "-c"
                   (string-join (cons command args)))))

        (define system-profile
          "/run/current-system/profile")

        (define user-profile
          (and=> (getpw (getuid))
                 (lambda (pw)
                   (string-append (passwd:dir pw) "/.guix-profile"))))

        (define (xsession-command desktop-file)
          ;; Read from DESKTOP-FILE its X session command and return it as a
          ;; list.
          (define exec-regexp
            (make-regexp "^[[:blank:]]*Exec=(.*)$"))

          (call-with-input-file desktop-file
            (lambda (port)
              (let loop ()
                (match (read-line port)
                  ((? eof-object?) #f)
                  ((= (cut regexp-exec exec-regexp <>) result)
                   (if result
                       (string-tokenize (match:substring result 1))
                       (loop))))))))

        (define (find-session profile)
          ;; Return an X session command from PROFILE or #f if none was found.
          (let ((directory (string-append profile "/share/xsessions")))
            (match (scandir directory
                            (cut string-suffix? ".desktop" <>))
              ((or () #f)
               #f)
              ((sessions ...)
               (any xsession-command
                    (map (cut string-append directory "/" <>)
                         sessions))))))

        (let* ((home          (getenv "HOME"))
               (xsession-file (string-append home "/.xsession"))
               (session       (match (command-line)
                                ((_)
                                 #$(if fallback-session
                                       #~(list #$fallback-session)
                                       #f))
                                ((_ x ..1)
                                 x))))
          (if (file-exists? xsession-file)
              ;; Run ~/.xsession when it exists.
              (apply exec-from-login-shell xsession-file
                     (or session '()))
              ;; Otherwise, start the specified session or a fallback.
              (apply exec-from-login-shell
                     (or session
                         (find-session user-profile)
                         (find-session system-profile)))))))

  (program-file "xinitrc" builder))

(define-syntax handle-xorg-configuration
  (syntax-rules ()
    "Generate the `compose' and `extend' entries of a login manager
`service-type' to handle specifying the `xorg-configuration' through
a `service-extension', as used by `set-xorg-configuration'."
    ((_ configuration-record service-type-definition)
     (service-type
       (inherit service-type-definition)
       (compose (lambda (extensions)
                  (match extensions
                    (() #f)
                    ((config . _) config))))
       (extend (lambda (config xorg-configuration)
                 (if xorg-configuration
                     (configuration-record
                      (inherit config)
                      (xorg-configuration xorg-configuration))
                     config)))))))

(define (xorg-server-profile-service config)
  ;; XXX: profile-service-type only accepts <package> objects.
  (list
   (package
     (name "xorg-wrapper")
     (version (package-version xorg-server))
     (source (xorg-wrapper config))
     (build-system trivial-build-system)
     (arguments
      '(#:modules ((guix build utils))
        #:builder
        (begin
          (use-modules (guix build utils))
          (let* ((source (assoc-ref %build-inputs "source"))
                 (out (assoc-ref %outputs "out"))
                 (bin (string-append out "/bin")))
            (mkdir-p bin)
            (symlink source (string-append bin "/X"))
            (symlink source (string-append bin "/Xorg"))
            #t))))
     (home-page (package-home-page xorg-server))
     (synopsis (package-synopsis xorg-server))
     (description (package-description xorg-server))
     (license (package-license xorg-server)))))

(define xorg-server-service-type
  (service-type
   (name 'xorg-server)
   (extensions
    (list (service-extension profile-service-type
                             xorg-server-profile-service)))
   (default-value (xorg-configuration))
   (description "Add @command{X} to the system profile, to be used with
@command{sx} or @command{xinit}.")))


;;;
;;; SLiM log-in manager.
;;;

(define %default-slim-theme
  ;; Theme based on work by Felipe López.
  (file-append %artwork-repository "/slim"))

(define %default-slim-theme-name
  ;; This must be the name of the sub-directory in %DEFAULT-SLIM-THEME that
  ;; contains the actual theme files.
  "1.x")

(define-record-type* <slim-configuration>
  slim-configuration make-slim-configuration
  slim-configuration?
  (slim slim-configuration-slim
        (default slim))
  (allow-empty-passwords? slim-configuration-allow-empty-passwords?
                          (default #t))
  (gnupg? slim-configuration-gnupg?
          (default #f))
  (auto-login? slim-configuration-auto-login?
               (default #f))
  (default-user slim-configuration-default-user
                (default ""))
  (theme slim-configuration-theme
         (default %default-slim-theme))
  (theme-name slim-configuration-theme-name
              (default %default-slim-theme-name))
  (xauth slim-configuration-xauth
         (default xauth))
  (shepherd slim-configuration-shepherd
            (default shepherd))
  (auto-login-session slim-configuration-auto-login-session
                      (default #f))
  (xorg-configuration slim-configuration-xorg
                      (default (xorg-configuration)))
  (display slim-configuration-display
           (default ":0"))
  (vt slim-configuration-vt
      (default "vt7"))
  (sessreg slim-configuration-sessreg
           (default sessreg)))

(define (slim-pam-service config)
  "Return a PAM service for @command{slim}."
  (list (unix-pam-service
         "slim"
         #:login-uid? #t
         #:allow-empty-passwords?
         (slim-configuration-allow-empty-passwords? config)
         #:gnupg?
         (slim-configuration-gnupg? config))))

(define (slim-shepherd-service config)
  (let* ((xinitrc (xinitrc #:fallback-session
                           (slim-configuration-auto-login-session config)))
         (xauth   (slim-configuration-xauth config))
         (startx  (xorg-start-command (slim-configuration-xorg config)))
         (display (slim-configuration-display config))
         (vt (slim-configuration-vt config))
         (shepherd   (slim-configuration-shepherd config))
         (theme-name (slim-configuration-theme-name config))
         (sessreg (slim-configuration-sessreg config))
         (lockfile (string-append "/var/run/slim-" vt ".lock")))
    (define slim.cfg
      (mixed-text-file "slim.cfg"  "
default_path /run/current-system/profile/bin
default_xserver " startx "
display_name " display "
xserver_arguments " vt "
xauth_path " xauth "/bin/xauth
authfile /var/run/slim-" vt ".auth
lockfile " lockfile "
logfile /var/log/slim-" vt ".log

# The login command.  '%session' is replaced by the chosen session name, one
# of the names specified in the 'sessions' setting: 'wmaker', 'xfce', etc.
login_cmd  exec " xinitrc " %session
sessiondir /run/current-system/profile/share/xsessions
session_msg session (F1 to change):
sessionstart_cmd " sessreg "/bin/sessreg -a -l $DISPLAY %user
sessionstop_cmd " sessreg "/bin/sessreg -d -l $DISPLAY %user

halt_cmd " shepherd "/sbin/halt
reboot_cmd " shepherd "/sbin/reboot\n"
(if (slim-configuration-auto-login? config)
    (string-append "auto_login yes\ndefault_user "
                   (slim-configuration-default-user config) "\n")
    "")
(if theme-name
    (string-append "current_theme " theme-name "\n")
    "")))

    (define theme
      (slim-configuration-theme config))

    (list (shepherd-service
           (documentation "Xorg display server")
           (provision (append
                       ;; For compatibility, also provide 'xorg-server'.
                       (if (string=? vt "vt7")
                           '(xorg-server)
                           '())

                       (list (symbol-append 'xorg-server-
                                            (string->symbol vt)))))
           (requirement '(pam user-processes host-name udev))
           (start
            #~(lambda ()
                ;; A stale lock file can prevent SLiM from starting, so remove it to
                ;; be on the safe side.
                (false-if-exception (delete-file lockfile))

                (fork+exec-command
                 (list (string-append #$(slim-configuration-slim config)
                                      "/bin/slim")
                       "-nodaemon")
                 #:environment-variables
                 (list (string-append "SLIM_CFGFILE=" #$slim.cfg)
                       #$@(if theme
                              (list #~(string-append "SLIM_THEMESDIR=" #$theme))
                              #~())))))
           (stop #~(make-kill-destructor))
           (respawn? #t)))))

(define slim-service-type
  (handle-xorg-configuration slim-configuration
    (service-type (name 'slim)
                  (extensions
                   (list (service-extension shepherd-root-service-type
                                            slim-shepherd-service)
                         (service-extension pam-root-service-type
                                            slim-pam-service)))
                  (default-value (slim-configuration))
                  (description
                   "Run the SLiM graphical login manager for X11."))))


;;;
;;; Screen lockers & co.
;;;

(define-configuration/no-serialization screen-locker-configuration
  (name
   string
   "Name of the screen locker.")
  (program
   file-like
   "Path to the executable for the screen locker as a G-Expression.")
  (allow-empty-password?
   (boolean #f)
   "Whether to allow empty passwords.")
  (using-pam?
   (boolean #t)
   "Whether to setup PAM entry.")
  (using-setuid?
   (boolean #t)
   "Whether to setup program as setuid binary."))

(define (screen-locker-pam-services config)
  (match-record config <screen-locker-configuration>
    (name allow-empty-password? using-pam?)
    (if using-pam?
        (list (unix-pam-service name
                                #:allow-empty-passwords?
                                allow-empty-password?))
        '())))

(define (screen-locker-privileged-programs config)
  (match-record config <screen-locker-configuration>
    (name program using-setuid?)
    (if using-setuid?
        (list (privileged-program
               (program program)
               (setuid? #t)))
        '())))

(define screen-locker-service-type
  (service-type (name 'screen-locker)
                (extensions
                 (list (service-extension pam-root-service-type
                                          screen-locker-pam-services)
                       (service-extension privileged-program-service-type
                                          screen-locker-privileged-programs)))
                (description
                 "Allow the given program to be used as a screen locker for
the graphical server by making it setuid-root, so it can authenticate users,
and by creating a PAM service for it.")))

(define (screen-locker-generate-doc)
  (configuration->documentation 'screen-locker-configuration))

(define-deprecated (screen-locker-service package
                                          #:optional
                                          (program (package-name package))
                                          #:key allow-empty-passwords?)
  screen-locker-service-type
  "Add @var{package}, a package for a screen locker or screen saver whose
command is @var{program}, to the set of setuid programs and add a PAM entry
for it.  For example:

@lisp
 (screen-locker-service xlockmore \"xlock\")
@end lisp

makes the good ol' XlockMore usable."
  (service screen-locker-service-type
           (screen-locker-configuration
            (name program)
            (program (file-append package "/bin/" program))
            (allow-empty-password? allow-empty-passwords?))))


;;;
;;; Locale service.
;;;

(define-record-type* <localed-configuration>
  localed-configuration make-localed-configuration
  localed-configuration?
  (localed         localed-configuration-localed
                   (default localed))
  (keyboard-layout localed-configuration-keyboard-layout
                   (default #f)))

(define (localed-dbus-service config)
  "Return the 'localed' D-Bus service for @var{config}, a
@code{<localed-configuration>} record."
  (define keyboard-layout
    (localed-configuration-keyboard-layout config))

  ;; The primary purpose of 'localed' is to tell GDM what the "current" Xorg
  ;; keyboard layout is.  If 'localed' is missing, or if it's unable to
  ;; determine the current XKB layout, then GDM forcefully installs its
  ;; default XKB config (US English).  Here we communicate the configured
  ;; layout through environment variables.

  (if keyboard-layout
      (let* ((layout  (keyboard-layout-name keyboard-layout))
             (variant (keyboard-layout-variant keyboard-layout))
             (model   (keyboard-layout-model keyboard-layout))
             (options (keyboard-layout-options keyboard-layout)))
        (list (wrapped-dbus-service
               (localed-configuration-localed config)
               "libexec/localed/localed"
               `(("GUIX_XKB_LAYOUT" ,layout)
                 ,@(if variant
                       `(("GUIX_XKB_VARIANT" ,variant))
                       '())
                 ,@(if model
                       `(("GUIX_XKB_MODEL" ,model))
                       '())
                 ,@(if (null? options)
                       '()
                       `(("GUIX_XKB_OPTIONS"
                          ,(string-join options ","))))))))
      '()))

(define localed-service-type
  (let ((package (lambda (config)
                   ;; Don't bother if the user didn't specify any keyboard
                   ;; layout.
                   (if (localed-configuration-keyboard-layout config)
                       (list (localed-configuration-localed config))
                       '()))))
    (service-type (name 'localed)
                  (extensions
                   (list (service-extension dbus-root-service-type
                                            localed-dbus-service)
                         (service-extension udev-service-type package)
                         (service-extension polkit-service-type package)

                         ;; Add 'localectl' to the profile.
                         (service-extension profile-service-type package)))

                  ;; This service can be extended, typically by the X login
                  ;; manager, to communicate the chosen Xorg keyboard layout.
                  (compose (lambda (extensions)
                             (find keyboard-layout? extensions)))
                  (extend (lambda (config keyboard-layout)
                            (localed-configuration
                             (inherit config)
                             (keyboard-layout keyboard-layout))))
                  (description
                   "Run the locale daemon, @command{localed}, which can be used
to control the system locale and keyboard mapping from user programs such as
the GNOME desktop environment.")
                  (default-value (localed-configuration)))))


;;;
;;; Dconf.
;;;

(define-maybe text-config)

(define-configuration/no-serialization dconf-keyfile
  (name string
        "The file name of the associated keyfile, e.g. \"00-login-screen\".")
  (content text-config "The content of the associated keyfile."))

(define-configuration/no-serialization dconf-profile
  (name string "The file name of the dconf system profile, which should match
the name of a user for which the profile is to be used with.  To have the
profile used, the environment variable \"DCONF_PROFILE\" should be set to the
profile file, e.g.:
@example
 export DCONF_PROFILE=/etc/dconf/profile/gdm
@end example")
  (content maybe-text-config "The content of the Dconf profile.  Unless
provided, it defaults to include the user database (\"user-db:NAME\") as well
as the system database (\"system-db:NAME\"), which corresponds to the
generated database, @file{/etc/dconf/db/NAME}.")
  (keyfile dconf-keyfile "The keyfile associated with the profile"))

(define dconf-profiles?
  (list-of dconf-profile?))

(define-configuration/no-serialization dconf-configuration
  (profiles dconf-profiles "The list of <dconf-profile> objects to populate."))

(define (dconf-profile->profile-file profile)
  "Given PROFILE, a <dconf-profile> object, return a dconf profile file."
  (let ((name (dconf-profile-name profile))
        (content (dconf-profile-content profile)))
    (apply mixed-text-file
           name
           (if (maybe-value-set? content)
               (interpose content "\n" 'suffix)
               (interpose (list (string-append "user-db:" name)
                                (string-append "system-db:" name))
                          "\n" 'suffix)))))

(define (dconf-profile->db-keyfile profile)
  "Given PROFILE, a <dconf-profile> object, return a dconf profile file."
  (let ((keyfile (dconf-profile-keyfile profile)))
    (apply mixed-text-file (dconf-keyfile-name keyfile)
           (interpose (dconf-keyfile-content keyfile) "\n" 'suffix))))

(define (dconf-profile->db-keyfile-dir profile)
  "Wrap the keyfile in a directory, to satisfy 'dconf compile'."
  (let ((name (dconf-profile-name profile))
        (keyfile-name (dconf-keyfile-name (dconf-profile-keyfile profile))))
    (computed-file name
                   #~(begin
                       (mkdir #$output)
                       (symlink #$(dconf-profile->db-keyfile profile)
                                (string-append #$output "/" #$keyfile-name))))))

(define (dconf-profile->db profile)
  "Compile the a <dconf-profile> object into a GVariant Database file."
  (let ((name (dconf-profile-name profile)))
    (computed-file
     name
     (with-imported-modules '((guix build utils))
       #~(begin
           (use-modules (guix build utils))
           (setenv "DCONF_PROFILE" #$(dconf-profile->profile-file profile))
           (invoke #$(file-append dconf "/bin/dconf") "compile"
                   #$output #$(dconf-profile->db-keyfile-dir profile)))))))

(define (dconf-profile->files profile)
  "Given PROFILE, a <dconf-profile> object, return a dconf directory
containing the associated profile, keyfile and database files to be assembled
under /etc."
  (let ((name (dconf-profile-name profile))
        (keyfile-name (dconf-keyfile-name (dconf-profile-keyfile profile))))
    (list (list (string-append "dconf/profile/" name)
                (dconf-profile->profile-file profile))
          (list (string-append "dconf/db/" name ".d/" keyfile-name)
                (dconf-profile->db-keyfile profile))
          (list (string-append "dconf/db/" name)
                (dconf-profile->db profile)))))

(define dconf-service-type
  (service-type
   (name 'dconf-profile)
   (extensions
    (list (service-extension etc-service-type
                             (lambda (dconf-profiles)
                               (append-map dconf-profile->files
                                           dconf-profiles)))))
   (compose concatenate)
   (extend append)
   (default-value '())
   (description "Extend the @code{etc-service-type} to populate the file
hierarchy under @file{/etc/dconf} with the <dconf-profile> objects provided as
argument.")))


;;;
;;; GNOME Desktop Manager.
;;;

(define %gdm-accounts
  (list (user-group (name "gdm") (system? #t))
        (user-account
         (name "gdm")
         (group "gdm")
         (supplementary-groups '("video"))
         (system? #t)
         (comment "GNOME Display Manager user")
         (home-directory "/var/lib/gdm")
         (shell (file-append shadow "/sbin/nologin")))))

(define dbus-daemon-wrapper
  (program-file
   "gdm-dbus-wrapper"
   #~(begin
       (use-modules (srfi srfi-26))

       (define system-profile
         "/run/current-system/profile")

       (define user-profile
         (and=> (getpw (getuid))
                (lambda (pw)
                  (string-append (passwd:dir pw) "/.guix-profile"))))

       (define home-profile
         (and=> (getpw (getuid))
                (lambda (pw)
                  (string-append (passwd:dir pw) "/.guix-home/profile"))))

       ;; If we are able to find the user's profile, we can add it to
       ;; the search paths set below.  We need to do this so that D-Bus
       ;; can start services installed by the user.  This allows
       ;; applications that require session D-Bus services (e.g,
       ;; 'evolution') to work even if those services are only available
       ;; in the user's profile.  See <https://bugs.gnu.org/35267>.
       (define profiles
         (append (if home-profile
                     (list home-profile)
                     '())
                 (if user-profile
                     (list user-profile)
                     '())
                 (list system-profile)))

       (setenv "XDG_CONFIG_DIRS"
               (string-join (map (cut string-append <> "/etc/xdg") profiles)
                            ":"))
       (setenv "XDG_DATA_DIRS"
               (string-join (map (cut string-append <> "/share") profiles)
                            ":"))
       (apply execl (string-append #$dbus "/bin/dbus-daemon")
              (program-arguments)))))

;; Wrapper script for Wayland sessions, similar to Xsession.
;;
;; See `xinitrc`.  By default, it launches the specified session through a
;; login shell.  With the default Guix configuration, this should source
;; /etc/profile, setting up the Guix profile environment variables.  However,
;; gdm launches its own graphical session through the same method, so we need
;; to ignore this case, since `gdm` doesn't have a login shell.
(define gdm-wayland-session-wrapper
  (program-file
   "gdm-wayland-session-wrapper"
   #~((let* ((user (getpw (getuid)))
	    (name (passwd:name user))
	    (shell (passwd:shell user))
	    (args (cdr (command-line))))
        (if (string=? name "gdm")
	    (apply execl (cons (car args) args))
	    (execl shell shell "--login" "-c" (string-join args)))))))

(define-record-type* <gdm-configuration>
  gdm-configuration make-gdm-configuration
  gdm-configuration?
  (gdm gdm-configuration-gdm (default gdm))
  (allow-empty-passwords? gdm-configuration-allow-empty-passwords? (default #t))
  (auto-login? gdm-configuration-auto-login? (default #f))
  (auto-suspend? gdm-configuration-auto-suspend? (default #t))
  (dbus-daemon gdm-configuration-dbus-daemon (default dbus-daemon-wrapper))
  (debug? gdm-configuration-debug? (default #f))
  (default-user gdm-configuration-default-user (default #f))
  (gnome-shell-assets gdm-configuration-gnome-shell-assets
                      (default (list adwaita-icon-theme font-abattis-cantarell)))
  (xorg-configuration gdm-configuration-xorg
                      (default (xorg-configuration)))
  (x-session gdm-configuration-x-session
             (default (xinitrc)))
  (xdmcp? gdm-configuration-xdmcp?
          (default #f))
  (wayland? gdm-configuration-wayland? (default #t))
  (wayland-session gdm-configuration-wayland-session
                   (default gdm-wayland-session-wrapper)))

(define (gdm-dconf-profiles config)
  (if (gdm-configuration-auto-suspend? config)
      '()
      ;; This custom gconf profile works around a lack of configuration option
      ;; to disable auto-suspend when no users are physically logged in (see:
      ;; https://gitlab.gnome.org/GNOME/gnome-control-center/-/issues/22).
      (list (dconf-profile
             (name "gdm")
             (content (list #~(begin
                                (use-modules (ice-9 textual-ports))
                                (string-trim
                                 (call-with-input-file
                                     #$(file-append gdm "/share/dconf/profile/gdm")
                                   get-string-all)))
                            "system-db:gdm"))
             (keyfile (dconf-keyfile
                       (name "00-disable-suspend")
                       (content
                        (list "[org/gnome/settings-daemon/plugins/power]"
                              "sleep-inactive-ac-type='nothing'"
                              "sleep-inactive-battery-type='nothing'"
                              "sleep-inactive-ac-timeout=0"
                              "sleep-inactive-battery-timeout=0"))))))))

(define (gdm-configuration-file config)
  (mixed-text-file "gdm-custom.conf"
                   "[daemon]\n"
                   "#User=gdm\n"
                   "#Group=gdm\n"
                   (if (gdm-configuration-auto-login? config)
                       (string-append
                        "AutomaticLoginEnable=true\n"
                        "AutomaticLogin="
                        (or (gdm-configuration-default-user config)
                            (error "missing default user for auto-login"))
                        "\n")
                       (string-append
                        "AutomaticLoginEnable=false\n"
                        "#AutomaticLogin=\n"))
                   "#TimedLoginEnable=false\n"
                   "#TimedLogin=\n"
                   "#TimedLoginDelay=0\n"
                   ;; Disable initial system setup inside GDM.
                   ;; Whatever settings are set there should already be
                   ;; taken care of through `guix system'.
                   ;; See also
                   ;; <https://debbugs.gnu.org/cgi/bugreport.cgi?bug=39281>.
                   "InitialSetupEnable=false\n"
                   (format #f "WaylandEnable=~:[false~;true~]~%"
                           (gdm-configuration-wayland? config))
                   "\n"
                   "[debug]\n"
                   (format #f "Enable=~:[false~;true~]~%"
                           (gdm-configuration-debug? config))
                   "\n"
                   "[security]\n"
                   "#DisallowTCP=true\n"
                   "#AllowRemoteAutoLogin=false\n"
                   "\n"
                   "[xdmcp]\n"
                   (format #f "Enable=~:[false~;true~]~%"
                           (gdm-configuration-xdmcp? config))))

(define (gdm-pam-service config)
  "Return a PAM service for @command{gdm}."
  (list
   (pam-service
    (inherit (unix-pam-service "gdm-autologin"
                               #:login-uid? #t))
    (auth (list (pam-entry
                 (control "optional")
                 (module (file-append (gdm-configuration-gdm config)
                                      "/lib/security/pam_gdm.so")))
                (pam-entry
                 (control "sufficient")
                 (module "pam_permit.so")))))
   (pam-service
    (inherit (unix-pam-service "gdm-launch-environment"))
    (auth (list (pam-entry
                 (control "required")
                 (module "pam_permit.so")))))
   (unix-pam-service "gdm-password"
                     #:login-uid? #t
                     #:allow-empty-passwords?
                     (gdm-configuration-allow-empty-passwords? config))))

(define (gdm-shepherd-service config)
  (define config-file
    (gdm-configuration-file config))

  (list (shepherd-service
         (documentation "Xorg display server (GDM)")
         (provision '(xorg-server))
         (requirement '(dbus-system pam user-processes host-name udev elogind))
         (start #~(make-forkexec-constructor
                   '(#$(file-append (gdm-configuration-gdm config) "/bin/gdm"))
                   #:environment-variables
                   (list #$@(if (gdm-configuration-auto-suspend? config)
                                #~()
                                #~("DCONF_PROFILE=/etc/dconf/profile/gdm"))
                         (string-append "GDM_CUSTOM_CONF=" #$config-file)
                         (string-append
                          "GDM_DBUS_DAEMON="
                          #$(gdm-configuration-dbus-daemon config))
                         (string-append
                          "GDM_X_SERVER="
                          #$(xorg-wrapper
                             (gdm-configuration-xorg config)))
                         (string-append
                          "GDM_X_SESSION="
                          #$(gdm-configuration-x-session config))
                         (string-append
                          "XDG_DATA_DIRS="
                          ((lambda (ls) (string-join ls ":"))
                           (map (lambda (path)
                                  (string-append path "/share"))
                                ;; XXX: Remove gnome-shell below when GDM
                                ;; can depend on GNOME Shell directly.
                                (cons #$gnome-shell
                                      '#$(gdm-configuration-gnome-shell-assets
                                          config)))))
                         ;; Add XCURSOR_PATH so that mutter can find its
                         ;; cursors.  gdm doesn't login so doesn't source
                         ;; the corresponding line in /etc/profile.
                         "XCURSOR_PATH=/run/current-system/profile/share/icons"
                         (string-append
                          "GUIX_GDK_PIXBUF_MODULE_FILES="
                          #$gnome-shell "/" #$%gdk-pixbuf-loaders-cache-file)
                         (string-append
                          "GDM_WAYLAND_SESSION="
                          #$(gdm-configuration-wayland-session config)))))
         (stop #~(make-kill-destructor))
         (actions (list (shepherd-configuration-action config-file)))
         (respawn? #t))))

(define gdm-polkit-rules
  (lambda (config)
    (if (gdm-configuration-xdmcp? config)
        ;; Allow remote (XDMCP) users to use colord; otherwise an
        ;; authentication dialog would appear on the GDM screen (see the
        ;; upstream bug:
        ;; https://gitlab.gnome.org/GNOME/gnome-settings-daemon/-/issues/273).
        (list (computed-file
               "02-allow-colord.rules"
               (with-imported-modules '((guix build utils))
                 #~(begin
                     (use-modules (guix build utils))

                     (let* ((rules.d
                             (string-append #$output
                                            "/share/polkit-1"
                                            "/rules.d"))
                            (allow-colord.rules (string-append
                                                 rules.d
                                                 "/02-allow-colord.rules")))
                       (mkdir-p rules.d)
                       (call-with-output-file allow-colord.rules
                         (lambda (port)
                           ;; This workaround enables any local or remote in
                           ;; the "users" group to use colord (see:
                           ;; https://c-nergy.be/blog/?p=12073).
                           (format port "\
polkit.addRule(function(action, subject) {
   if (action.id.match(\"org.freedesktop.color-manager\")) {
      polkit.log(\"POLKIT DEBUG returning YES for action: \" + action);
      return polkit.Result.YES;
   }
});~%"))))))))
        '())))

(define gdm-service-type
  (handle-xorg-configuration gdm-configuration
    (service-type (name 'gdm)
                  (extensions
                   (list (service-extension shepherd-root-service-type
                                            gdm-shepherd-service)
                         (service-extension account-service-type
                                            (const %gdm-accounts))
                         (service-extension dconf-service-type
                                            gdm-dconf-profiles)
                         (service-extension pam-root-service-type
                                            gdm-pam-service)
                         (service-extension polkit-service-type
                                            gdm-polkit-rules)
                         (service-extension profile-service-type
                                            gdm-configuration-gnome-shell-assets)
                         (service-extension dbus-root-service-type
                                            (compose list
                                                     gdm-configuration-gdm))
                         (service-extension localed-service-type
                                            (compose
                                             xorg-configuration-keyboard-layout
                                             gdm-configuration-xorg))))
                  (default-value (gdm-configuration))
                  (description
                   "Run the GNOME Desktop Manager (GDM), a program that allows
you to log in in a graphical session, whether or not you use GNOME."))))

;; Since GDM depends on Rust and Rust is not available on all platforms,
;; use SDDM as the fall-back display manager.
;; TODO: Switch the condition to take into account if Rust is supported and
;; match the configuration in desktop-services-for-system.
(define* (set-xorg-configuration config
                                 #:optional
                                 (login-manager-service-type
                                  (if (target-x86-64?)
                                      gdm-service-type
                                      sddm-service-type)))
  "Tell the log-in manager (of type @var{login-manager-service-type}) to use
@var{config}, an <xorg-configuration> record."
  (simple-service 'set-xorg-configuration
                  login-manager-service-type
                  config))

;;; xorg.scm ends here
