;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2024 Arnaud Daby-Seesaram <ds-ac@nanein.fr>
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

(define-module (gnu home services sway)
  #:use-module (guix modules)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (guix packages)
  #:use-module (gnu system keyboard)
  #:use-module (gnu services configuration)
  #:use-module (gnu home services)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages terminals)
  #:export (;; Event codes
            %ev-code-mouse-left
            %ev-code-mouse-right
            %ev-code-mouse-scroll-click

            ;; Configuration records.
            sway-configuration
            sway-bar
            sway-output
            sway-input
            point
            sway-color
            sway-border-color
            home-sway-service-type
            sway-configuration->file
            sway-mode

            ;; Default values.
            %sway-default-variables
            %sway-default-gestures
            %sway-default-modes
            %sway-default-keybindings
            %sway-default-startup-programs
            %sway-default-packages))

;; Helper function.
(define (flatmap f l)
  (let loop ((lst (reverse l)) (acc '()))
    (match lst
      (() acc)
      ((head . tail)
       (let* ((h (f head))
              (acc (append h acc)))
         (loop tail acc))))))


;;;
;;; Definition of configurations.
;;;

(define (string-ish? s)
  (or (gexp? s)
      (file-like? s)
      (string? s)))

(define (string-or-gexp? s)
  (or (gexp? s)
      (string? s)))

(define (list-of-string-ish? lst)
  (every string-ish? lst))

(define (list-of-packages? lst)
  (every package? lst))

(define (bar-position? p)
  (member p '(top bottom)))

(define (hidden-state? st)
  (member st '(hide show)))

(define (string-or-symbol? s)
  (or (string? s)
      (symbol? s)))

(define (strings? lst)
  (every string? lst))

(define (extra-content? extra)
  (every string-or-gexp? extra))

(define (make-alist-predicate key? val?)
  (lambda (lst)
    (every
     (lambda (item)
       (match item
         ((k . v)
          (and (key? k)
               (val? v)))
         (_ #f)))
     lst)))

(define bindings?
  (make-alist-predicate symbol? string-or-gexp?))

(define mouse-bindings?
  (make-alist-predicate integer? string-or-gexp?))

(define (variables? lst)
  (make-alist-predicate symbol? string-ish?))

(define-maybe string (no-serialization))
(define-maybe strings (no-serialization))
(define-maybe boolean (no-serialization))
(define-maybe keyboard-layout (no-serialization))

(define-configuration/no-serialization sway-input
  (identifier
   (string-or-symbol '*)
   "Identifier of the input.")
  (layout
   maybe-keyboard-layout
   "Keyboard layout of the input.")
  (disable-while-typing
   maybe-boolean
   "If `#t', disable the input while typing; if `#f' do not.")
  (disable-while-trackpointing
   maybe-boolean
   "If `#t', disable the input while using a trackpoint; if `#f' do not.")
  (tap
   maybe-boolean
   "Enable or disable tap.")
  (extra-content
   (extra-content '())
   "Lines to add at the end of the configuration file."))

(define (sway-inputs? lst)
  (every sway-input? lst))

(define-configuration/no-serialization sway-border-color
  (border
   string
   "Border color.")
  (background
   string
   "Background color.")
  (text
   string
   "Text color."))

(define-maybe sway-border-color (no-serialization))

(define-configuration/no-serialization sway-color
  (background
   maybe-string
   "Background color of the bar.")
  (statusline
   maybe-string
   "Text color of the status line.")
  (focused-background
   maybe-string
   "Background color of the bar on the currently focused monitor.")
  (focused-statusline
   maybe-string
   "Text color of the statusline on the currently focused monitor.")
  (focused-workspace
   maybe-sway-border-color
   "Color scheme for focused workspaces.")
  (active-workspace
   maybe-sway-border-color
   "Color scheme for active workspaces.")
  (inactive-workspace
   maybe-sway-border-color
   "Color scheme for inactive workspaces.")
  (urgent-workspace
   maybe-sway-border-color
   "Color scheme for workspaces containing `urgent' windows.")
  (binding-mode
   maybe-sway-border-color
   "Color scheme for the binding mode indicator."))

(define-maybe sway-color (no-serialization))

(define (status-command? c)
  (or (string? c)
      (file-like? c)
      (gexp? c)))

(define-maybe bar-position (no-serialization))
(define-maybe hidden-state (no-serialization))
(define-maybe status-command (no-serialization))

(define-configuration/no-serialization sway-bar
  (identifier
   (symbol 'bar0)
   "Identifier of the bar.")
  (position
   maybe-bar-position
   "Position of the bar.")
  (hidden-state
   maybe-hidden-state
   "Hidden state.")
  (binding-mode-indicator
   maybe-boolean
   "Binding indicator.")
  (colors
   maybe-sway-color
   "Color palette of the bar.")
  (status-command
   maybe-status-command
   "Status command.")
  (mouse-bindings
   (mouse-bindings '())
   "Actions triggered by mouse events.")
  (extra-content
   (extra-content '())
   "Extra configuration lines."))

(define-maybe sway-bar (no-serialization))

(define-configuration/no-serialization point
  (x integer "X coordinate.")
  (y integer "Y coordinate."))

(define-maybe point (no-serialization))

(define (background? bg)
  (or (string-ish? bg)
      (and (pair? bg)
           (string-ish? (car bg))
           (member (cdr bg) '(stretch fill fit center tile)))))

(define-maybe background (no-serialization))

(define-configuration/no-serialization sway-output
  (identifier
   (string-or-symbol '*)
   "Identifier of the output.")
  (resolution
   maybe-string
   "Mode of the monitor.")
  (position
   maybe-point
   "Position of the monitor.")
  (background
   maybe-background
   "Background image.")
  (extra-content
   (extra-content '())
   "Extra lines."))

(define (sway-outputs? lst)
  (every sway-output? lst))

(define-configuration/no-serialization sway-mode
  (mode-name
   (string "default")
   "Name of the mode.")
  (keybindings
   (bindings '())
   "Keybindings.")
  (mouse-bindings
   (mouse-bindings '())
   "Mouse bindings."))

(define (sway-modes? lst)
  (every sway-mode? lst))

(define-configuration/no-serialization sway-configuration
  (keybindings
   (bindings %sway-default-keybindings)
   "Keybindings.")
  (gestures
   (bindings %sway-default-gestures)
   "Gestures.")
  (packages
   (list-of-packages
    %sway-default-packages)
   "List of packages to add to the profile.")
  (variables
   (variables %sway-default-variables)
   "Variables declared at the beginning of the file.")
  (inputs
   (sway-inputs '())
   "Inputs.")
  (outputs
   (sway-outputs '())
   "Outputs.")
  (bar
   maybe-sway-bar
   "Bar configuration.")
  (modes
   (sway-modes %sway-default-modes)
   "Additional modes.")
  (startup+reload-programs
   (list-of-string-ish '())
   "Programs to execute at startup time.")
  (startup-programs
   (list-of-string-ish %sway-default-startup-programs)
   "Programs to execute at startup time.")
  (extra-content
   (extra-content '())
   "Lines to add at the end of the configuration file."))


;;;
;;; Default settings and useful constants.
;;;

(define sway-menu
  (program-file
   "sway-menu.scm"
   (with-imported-modules
       (source-module-closure '((guix build utils)))
     #~(begin
         (use-modules (ice-9 ftw)
                      (ice-9 popen)
                      (ice-9 receive)
                      (ice-9 rdelim)
                      (guix build utils)
                      (srfi srfi-1))

         (define (directory->files dir)
           (define (executable-file? f)
             ;; Cf. `(@ (guix build utils) executable-file?)' for an
             ;; explanation of `(zero? ...)'.
             (and=> (and (not (eq? (string-ref f 0) #\.))
                         (stat f))
                    (lambda (s)
                      (not (or
                            (zero? (logand (stat:mode s) #o100))
                            (eq? (stat:type s) 'directory))))))
           (with-directory-excursion dir
             (scandir "." executable-file?)))

         (let ((path (string-append (getenv "HOME")
                                    "/.guix-home/profile/bin"))
               (wmenu #$(file-append wmenu "/bin/wmenu"))
               (swaymsg #$(file-append sway "/bin/swaymsg")))
           (receive (from to pids)
               (pipeline `((,wmenu)))
             (for-each
              (lambda (c) (format to "~a~%" c))
              (directory->files path))
             (close to)
             (let ((choice (read-line from)))
               (close from)
               (waitpid (first pids))
               (when (string? choice) ;do not attempt to launch if no choice
                                      ;was given (e.g. if Escape is pressed in
                                      ;wmenu).
                 (execl swaymsg swaymsg "exec" "--"
                        choice)))))))))

(define %ev-code-mouse-left 272)
(define %ev-code-mouse-right 273)
(define %ev-code-mouse-scroll-click 274)

(define %sway-default-modes
  (list (sway-mode
         (mode-name "resize")
         (keybindings
          '(($left  . "resize shrink width 10px")
            ($down  . "resize grow height 10px")
            ($up    . "resize shrink height 10px")
            ($right . "resize grow width 10px")
            (Left   . "resize shrink width 10px")
            (Down   . "resize grow height 10px")
            (Up     . "resize shrink height 10px")
            (Right  . "resize grow width 10px")
            (Return . "mode \"default\"")
            (Escape . "mode \"default\""))))))

(define %sway-default-packages
  (list sway))

(define %sway-default-variables
  `((mod   . "Mod4")
    (left  . "h")
    (down  . "j")
    (up    . "k")
    (right . "l")
    (term  . ,(file-append foot "/bin/foot"))
    (menu  . ,sway-menu)))

(define %sway-default-gestures
  '((swipe:3:right . "workspace next_on_output")
    (swipe:3:left  . "workspace prev_on_output")
    (swipe:3:down  . "move to scratchpad")
    (swipe:3:up    . "scratchpad show")))

(define %sway-default-keybindings
  `(($mod+Return . "exec $term")
    ($mod+Shift+q . "kill")
    ($mod+d . "exec $menu")
    ($mod+Shift+c . "reload")
    ($mod+Shift+e
     . ,#~(string-append
           "exec " #$sway "/bin/swaynag -t warning -m \\\n    "
           "'You pressed the exit shortcut.  Do you really want to exit sway?"
           " This will end your Wayland session.' \\\n    "
           "-B 'Yes, exit sway' \\\n    '"
           #$sway "/bin/swaymsg exit'"))
    ($mod+$left . "focus left")
    ($mod+$down . "focus down")
    ($mod+$up . "focus up")
    ($mod+$right . "focus right")
    ($mod+Left . "focus left")
    ($mod+Down . "focus down")
    ($mod+Up . "focus up")
    ($mod+Right . "focus right")
    ($mod+Shift+$left . "move left")
    ($mod+Shift+$down . "move down")
    ($mod+Shift+$up . "move up")
    ($mod+Shift+$right . "move right")
    ($mod+Shift+Left . "move left")
    ($mod+Shift+Down . "move down")
    ($mod+Shift+Up . "move up")
    ($mod+Shift+Right . "move right")
    ($mod+1 . "workspace number 1")
    ($mod+2 . "workspace number 2")
    ($mod+3 . "workspace number 3")
    ($mod+4 . "workspace number 4")
    ($mod+5 . "workspace number 5")
    ($mod+6 . "workspace number 6")
    ($mod+7 . "workspace number 7")
    ($mod+8 . "workspace number 8")
    ($mod+9 . "workspace number 9")
    ($mod+0 . "workspace number 10")
    ($mod+Shift+1 . "move container to workspace number 1")
    ($mod+Shift+2 . "move container to workspace number 2")
    ($mod+Shift+3 . "move container to workspace number 3")
    ($mod+Shift+4 . "move container to workspace number 4")
    ($mod+Shift+5 . "move container to workspace number 5")
    ($mod+Shift+6 . "move container to workspace number 6")
    ($mod+Shift+7 . "move container to workspace number 7")
    ($mod+Shift+8 . "move container to workspace number 8")
    ($mod+Shift+9 . "move container to workspace number 9")
    ($mod+Shift+0 . "move container to workspace number 10")
    ($mod+b . "splith")
    ($mod+v . "splitv")
    ($mod+s . "layout stacking")
    ($mod+w . "layout tabbed")
    ($mod+e . "layout toggle split")
    ($mod+f . "fullscreen")
    ($mod+Shift+space . "floating toggle")
    ($mod+space . "focus mode_toggle")
    ($mod+a . "focus parent")
    ($mod+Shift+minus . "move scratchpad")
    ($mod+minus . "scratchpad show")
    ($mod+r . "mode \"resize\"")))

(define %sway-default-startup-programs
  (list
   #~(string-append
      #$swayidle "/bin/swayidle -w \\\n    "
      ;; 300: lock screen.
      "timeout 300 '" #$swaylock "/bin/swaylock "
      "--indicator-radius 75 \\\n    "
      "-i " #$sway
      "/share/backgrounds/sway/Sway_Wallpaper_Blue_1920x1080.png \\\n    "
      "-f -c 000000' \\\n    "
      ;; 600: lock + screen off.
      "timeout 600 '" #$sway "/bin/swaymsg \"output * power off\"' \\\n    "
      ;; Resume + sleep.
      "resume '" #$sway "/bin/swaymsg \"output * power on\"' \\\n    "
      "before-sleep '" #$swaylock "/bin/swaylock -f -c 000000'")))


;;;
;;; Serialization functions.
;;;

;; The main serialization code is defined in `sway-configuration->file' below.
;; In this function, the configuration is seen as a list of lines and blocks.
;;
;; The other serialization functions of this files are helpers that will build
;; the above list.  Each function either returns a list, or elements that will
;; be put in one.  The elements of these lists will either be:
;; - strings,
;;   In this case, the string is seen as a line to add to the configuration
;;   file.
;; - a pair (cons 'begin-block string),
;;   In this case, a line "string {" is added to the configuration file, and
;;   the indentation level is increased by four.
;; - the symbol 'end-block.
;;   In which case the indentation level is decreased by four, and the current
;;   configuration block is closed (with the line "}").

;; A few helper functions:

(define* (add-line-if field value
                      #:key (serializer %unset-value)
                      (suffix %unset-value))
  (if (eq? %unset-value value)
      %unset-value
      #~(string-append #$field " "
                       #$(if (eq? serializer %unset-value)
                             value
                             (serializer value))
                       #$(if (eq? suffix %unset-value)
                             ""
                             suffix))))

(define (add-block name content)
  (let ((content (filter
                  (lambda (elt) (not (eq? elt %unset-value)))
                  content)))
    (if (equal? content '())
        '()
        (append
         (list #~(cons 'begin-block #$name))
         content
         (list #~'end-block)))))

(define-syntax add-block*
  (syntax-rules ()
    ((add-block* name elt ...)
     (add-block name (append elt ...)))))

;; Serialization functions:

(define (box str)
  (let* ((len (string-length str))
         (line (make-string (+ 8 len) #\#)))
    (list
     line
     (string-append "### " str " ###")
     line)))

(define (with-heading str lst)
  (define (heading str)
    (let* ((len (string-length str))
           (line (make-string (+ 2 len) #\#)))
      (list
       "" ;add an empty line before the configuration section.
       (string-append "# " str)
       line)))
  (if (equal? lst '())
      '() ;if the configuration block is empty, do not add the heading.
      (append (heading str) lst)))

(define-inlinable (serialize-boolean-yn b)
  (if b "yes" "no"))
(define-inlinable (serialize-boolean-ed b)
  (if b "enable" "disable"))

(define-inlinable (serialize-binding binder key value)
  #~(string-append #$binder #$key " " #$value))

(define (serialize-mouse-binding var)
  (let* ((ev (car var))
         (ev-code (number->string ev))
         (command (cdr var)))
    (serialize-binding "bindcode " ev-code command)))

(define (serialize-keybinding var)
  (let ((name (symbol->string (car var)))
        (value (cdr var)))
    (serialize-binding "bindsym " name value)))

(define (serialize-gesture var)
  (let ((name (symbol->string (car var)))
        (value (cdr var)))
    (serialize-binding "bindgesture " name value)))

(define (serialize-variable var)
  (let ((name (symbol->string (car var)))
        (value (cdr var)))
    (serialize-binding "set $" name value)))

(define (serialize-exec b)
  (if b
      (lambda (exe)
        #~(string-append "exec_always " #$exe))
      (lambda (exe)
        #~(string-append "exec " #$exe))))

(define (serialize-output out)
  (let* ((pre-ident (sway-output-identifier out))
         (ident (if (symbol? pre-ident)
                    (symbol->string pre-ident)
                    (string-append "\"" pre-ident "\"")))
         (background (let ((bg (sway-output-background out)))
                       (if (pair? bg)
                           bg
                           (cons bg 'fill))))
         (resolution (sway-output-resolution out))
         (position (sway-output-position out))
         (extra-content (sway-output-extra-content out)))
    (add-block
     (string-append "output " ident)
     (cons*
      ;; Optional elements.
      (add-line-if "bg" (car background)
                   #:suffix
                   (string-append " " (symbol->string (cdr background))))
      (add-line-if "resolution" resolution)
      (add-line-if "position" position
                   #:serializer
                   (lambda (p)
                     (string-append (number->string (point-x p))
                                    " "
                                    (number->string (point-y p)))))
      ;; Extra-content: inlined as-is.
      extra-content))))

(define (serialize-input input)
  (define-inlinable (fetch-arg layout acc)
    (if (eq? layout %unset-value)
        %unset-value
        (acc layout)))

  (define-inlinable (unfalse f)
    (lambda (arg)
      (let ((res (f arg)))
        (if res res %unset-value))))

  (define-inlinable (unnil f)
    (lambda (arg)
      (let ((res (f arg)))
        (if (nil? res) %unset-value res))))

  (let* ((pre-ident (sway-input-identifier input))
         (ident (if (symbol? pre-ident)
                    (symbol->string pre-ident)
                    (string-append "\"" pre-ident "\"")))

         ;; unpack the `layout' field.
         (layout (sway-input-layout input))
         (xkb-layout (fetch-arg layout keyboard-layout-name))
         (xkb-variant (fetch-arg layout (unfalse keyboard-layout-variant)))
         (xkb-model (fetch-arg layout (unfalse keyboard-layout-model)))
         (xkb-options (fetch-arg layout (unnil keyboard-layout-options)))

         (tap (sway-input-tap input))
         (dwt (sway-input-disable-while-typing input))
         (dwtp (sway-input-disable-while-trackpointing input))
         (extra-content (sway-input-extra-content input)))
    (add-block
     (string-append "input " ident)
     (cons*
      ;; Optional.
      (add-line-if "xkb_layout" xkb-layout)
      (add-line-if "xkb_model" xkb-model)
      (add-line-if "xkb_variant" xkb-variant)
      (add-line-if "xkb_options" xkb-options
                   #:serializer (lambda (l) (string-join l ",")))
      (add-line-if "dwt" dwt
                   #:serializer serialize-boolean-ed)
      (add-line-if "dwtp" dwtp
                   #:serializer serialize-boolean-ed)
      (add-line-if "tap" tap
                   #:serializer serialize-boolean-ed)
      ;; extra-content inlined as-is.
      extra-content))))

(define (serialize-colors colors)
  (define (border-serializer val)
    (string-append (sway-border-color-border val)
                   " " (sway-border-color-background val)
                   " " (sway-border-color-text val)))
  (if (eq? %unset-value colors)
      '()
      (let ((background (sway-color-background colors))
            (statusline (sway-color-statusline colors))
            (focused-background (sway-color-focused-background colors))
            (focused-statusline (sway-color-focused-statusline colors))
            (focused-workspace (sway-color-focused-workspace colors))
            (active-workspace (sway-color-active-workspace colors))
            (inactive-workspace (sway-color-inactive-workspace colors))
            (urgent-workspace (sway-color-urgent-workspace colors))
            (binding-mode (sway-color-binding-mode colors)))
        (add-block
         "colors"
         (list
          (add-line-if "background" background)
          (add-line-if "statusline" statusline)
          (add-line-if "focused_background" focused-background)
          (add-line-if "focused_statusline" focused-statusline)
          (add-line-if "focused_workspace" focused-workspace
                       #:serializer border-serializer)
          (add-line-if "active_workspace" active-workspace
                       #:serializer border-serializer)
          (add-line-if "inactive_workspace" inactive-workspace
                       #:serializer border-serializer)
          (add-line-if "urgent_workspace" urgent-workspace
                       #:serializer border-serializer)
          (add-line-if "binding_mode" binding-mode
                       #:serializer border-serializer))))))

(define (serialize-mode mode)
  (let ((name (sway-mode-mode-name mode))
        (keys (sway-mode-keybindings mode))
        (clicks (sway-mode-mouse-bindings mode)))
    (add-block*
     (string-append "mode \"" name "\"")
     (map serialize-keybinding keys)
     (map serialize-mouse-binding clicks))))

(define (serialize-bar bar)
  (define serialize-symbol
    symbol->string)

  (let ((identifier (symbol->string (sway-bar-identifier bar)))
        (position (sway-bar-position bar))
        (hidden-state (sway-bar-hidden-state bar))
        (status-command (sway-bar-status-command bar))
        (binding-mode-indicator (sway-bar-binding-mode-indicator bar))
        (mouse-bindings (sway-bar-mouse-bindings bar))
        (extra-content (sway-bar-extra-content bar))
        (colors (sway-bar-colors bar)))
    (add-block*
     (string-append "bar " identifier)

     (if (eq? colors %unset-value)
         '()
         (serialize-colors colors))
     (list
      (add-line-if "position" position
                   #:serializer serialize-symbol)
      (add-line-if "hidden_state" hidden-state
                   #:serializer serialize-symbol)
      (add-line-if "status_command" status-command)
      (add-line-if "binding_mode_indicator" binding-mode-indicator
                   #:serializer serialize-boolean-yn))
     ;; Mouse-bindings and extra-content
     (map serialize-mouse-binding mouse-bindings)
     extra-content)))

(define (sway-configuration->file conf)
  (let* ((extra (sway-configuration-extra-content conf))
         (bar (sway-configuration-bar conf)))
    (computed-file
     "sway-config"
     #~(begin
         (use-modules (ice-9 format) (ice-9 match) 
                      (srfi srfi-1))

         (call-with-output-file #$output
           (lambda (port)

             ;; Add the (indented) line "s" to the output file.
             (define (line s)
               (lambda (i)
                 (format port "~a~a~%" i s)
                 i))

             ;; Begin a block "name" and adjust the indentation.
             (define (begin-block name)
               (lambda (i)
                 (format port "~a~a {~%" i name)
                 (string-append "    " i)))

             ;; Ends an open block and adjust the indentation.
             ;; Note: we must currently be in a configuration block.
             ;; Otherwise, `string-drop' might fail.
             (define (end-block)
               (lambda (i)
                 (let ((i (string-drop i 4)))
                   (format port "~a}~%" i)
                   i)))

             ;; Helper function.  The configuration is represented as a list
             ;; of actions (alter the indentation level, add a line, ...).
             ;; This function recognises the action and calls the right
             ;; function among those defined above.
             (define (serializer-dispatch-m arg)
               (match arg
                 ;; Special cases:
                 (('begin-block . str) (begin-block str))
                 ('end-block (end-block))
                 ;; Default case: `arg' is assumed to be a string.
                 (_ (line arg))))

             (define (serializer-dispatch elt i)
               ((serializer-dispatch-m elt) i))

             (fold
              ;; Dispatch function: depending on its argument, it will change
              ;; the indentation level or add a line to the output file.
              serializer-dispatch

              ;; Initial indentation string.  This string is prepended to
              ;; lines before their serialization.
              ""
              ;; List of lines or indentation modifiers.
              (list
               ;; Header.
               #$@(box "Auto-generated configuration")
               "# DO NOT EDIT MANUALLY."

               ;; Variables.
               #$@(with-heading "Variables."
                                (map serialize-variable
                                     (sway-configuration-variables conf)))

               ;; Outputs.
               #$@(with-heading "Outputs."
                                (flatmap serialize-output
                                         (sway-configuration-outputs conf)))

               ;; Inputs.
               #$@(with-heading "Inputs."
                                (flatmap serialize-input
                                         (sway-configuration-inputs conf)))

               ;; Bar configuration:
               ;; If the bar is unset, do not include anything.
               #$@(if (eq? bar %unset-value)
                      '()
                      (with-heading "Bar configuration."
                                    (serialize-bar bar)))

               ;; Keybindings.
               #$@(with-heading "Keybindings."
                                (map serialize-keybinding
                                     (sway-configuration-keybindings conf)))
               ;; Gestures.
               #$@(with-heading "Gestures."
                                (map serialize-gesture
                                     (sway-configuration-gestures conf)))

               ;; Modes.
               #$@(with-heading "Modes."
                                (flatmap serialize-mode
                                         (sway-configuration-modes conf)))

               ;; Startup-Programs.
               #$@(with-heading
                   "Programs to execute (at startup)."
                   (map (serialize-exec #f)
                        (sway-configuration-startup-programs conf)))
               ;; startup+reload-programs.
               #$@(with-heading
                   "Programs to execute (at startup & after reload)."
                   (map (serialize-exec #t)
                        (sway-configuration-startup+reload-programs conf)))

               ;; Extra-content.
               #$@(with-heading "Extra-content" extra)))))))))


;;;
;;; Definition of the Home Service.
;;;

(define (sway-configuration->files sway-conf)
  `((".config/sway/config" ,(sway-configuration->file sway-conf))))

(define home-sway-service-type
  (service-type
   (name 'home-sway-config)
   (extensions
    (list (service-extension home-files-service-type
                             sway-configuration->files)
          (service-extension home-profile-service-type
                             sway-configuration-packages)))
   (description "Configure Sway by providing a file
@file{~/.config/sway/config}.")
   (default-value (sway-configuration))))
