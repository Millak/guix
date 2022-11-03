;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2020, 2022 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2022 Florian Pelz <pelzflorian@pelzflorian.de>
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
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (gnu installer newt welcome)
  #:use-module ((gnu build linux-modules)
                #:select (modules-loaded
                          pci-devices))
  #:use-module (gnu installer dump)
  #:use-module (gnu installer hardware)
  #:use-module (gnu installer steps)
  #:use-module (gnu installer utils)
  #:use-module (gnu installer newt page)
  #:use-module (gnu installer newt utils)
  #:use-module (guix build syscalls)
  #:use-module (guix i18n)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (srfi srfi-71)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (newt)
  #:export (run-welcome-page))

;; Expected width and height for the logo.
(define logo-width (make-parameter 43))
(define logo-height (make-parameter 19))

(define info-textbox-width (make-parameter 70))
(define options-listbox-height (make-parameter 5))

(define (display-logo?)
  (> (screen-rows) 35))

(define* (run-menu-page title info-text logo
                        #:key
                        listbox-items
                        listbox-item->text)
  "Run a page with the given TITLE, to ask the user to choose between
LISTBOX-ITEMS displayed in a listbox. The listbox items are converted to text
using LISTBOX-ITEM->TEXT procedure. Display the textual LOGO in the center of
the page. Contrary to other pages, we cannot resort to grid layouts, because
we want this page to occupy all the screen space available."
  (define (fill-listbox listbox items)
    (map (lambda (item)
           (let* ((text (listbox-item->text item))
                  (key (append-entry-to-listbox listbox text)))
             (cons key item)))
         items))

  (let* ((logo-textbox
          (make-textbox -1 -1
                        (if (display-logo?) (logo-width) 0)
                        (if (display-logo?) (logo-height) 0)
                        0))
         (info-textbox
          (make-reflowed-textbox -1 -1
                                 info-text
                                 (info-textbox-width)))
         (options-listbox
          (make-listbox -1 -1
                        (options-listbox-height)
                        (logior FLAG-BORDER FLAG-RETURNEXIT)))
         (keys (fill-listbox options-listbox listbox-items))
         (grid (vertically-stacked-grid
                GRID-ELEMENT-COMPONENT logo-textbox
                GRID-ELEMENT-COMPONENT info-textbox
                GRID-ELEMENT-COMPONENT options-listbox))
         (form (make-form)))

    (define (choice->item str)
      ;; Return the item that corresponds to STR.
      (match (find (match-lambda
                     ((key . item)
                      (string=? str (listbox-item->text item))))
                   keys)
        ((key . item) item)
        (#f (abort-to-prompt 'installer-step 'abort))))

    (set-textbox-text logo-textbox (read-all logo))

    (add-form-to-grid grid form #t)
    (make-wrapped-grid-window grid title)

    (receive (exit-reason argument)
        (run-form-with-clients form
                               `(menu (title ,title)
                                      (text ,info-text)
                                      (items
                                       ,(map listbox-item->text
                                             listbox-items))))
      (dynamic-wind
        (const #t)
        (lambda ()
          (match exit-reason
            ('exit-component
             (let* ((entry (current-listbox-entry options-listbox))
                    (item (assoc-ref keys entry)))
               (match item
                 ((text . proc)
                  (proc)))))
            ('exit-fd-ready
             (let* ((choice argument)
                    (item   (choice->item choice)))
               (match item
                 ((text . proc)
                  (proc)))))))
        (lambda ()
          (destroy-form-and-pop form))))))

(define (check-hardware-support pci-database)
  "Warn about unsupported devices."
  (when (member "uvesafb" (modules-loaded))
    (run-error-page (G_ "\
This may be a false alarm, but possibly your graphics hardware does not
work well with only free software.  Expect trouble.  If after installation,
the system does not boot, perhaps you will need to add nomodeset to the
kernel arguments and need to configure the uvesafb kernel module.")
                    (G_ "Pre-install warning")))

  (let ((devices (pci-devices)))
    (match (filter unsupported-pci-device? devices)
      (()                                         ;no unsupported device
       #t)
      (unsupported
       (run-error-page (format #f (G_ "\
Devices not supported by free software were found on your computer:

~{  - ~a~%~}
Unfortunately, it means those devices will not be usable.

To address it, we recommend choosing hardware that respects your freedom as a \
user--hardware for which free drivers and firmware exist.  See \"Hardware \
Considerations\" in the manual for more information.")
                               (map (pci-device-description pci-database)
                                    unsupported))
                       (G_ "Hardware support warning")
                       #:width 76)))))

(define* (run-welcome-page logo #:key pci-database)
  "Run a welcome page with the given textual LOGO displayed at the center of
the page. Ask the user to choose between manual installation, graphical
installation and reboot."
  (when (file-exists? %core-dump)
    (match (choice-window
            (G_ "Previous installation failed")
            (G_ "Continue")
            (G_ "Report the failure")
            (G_ "It seems that the previous installation exited unexpectedly \
and generated a core dump.  Do you want to continue or to report the failure \
first?"))
      (1 #t)
      (2 (raise
          (condition
           (&user-abort-error))))))

  (run-menu-page
   (G_ "GNU Guix install")
   (G_ "Welcome to GNU Guix system installer!

You will be guided through a graphical installation program.

If you are familiar with GNU/Linux and you want tight control over \
the installation process, you can instead choose manual installation.  \
Documentation is accessible at any time by pressing Ctrl-Alt-F2.")
   logo
   #:listbox-items
   `((,(G_ "Graphical install using a terminal based interface")
      .
      ,(lambda ()
         (check-hardware-support pci-database)))
     (,(G_ "Install using the shell based process")
      .
      ,(lambda ()
         (check-hardware-support pci-database)
         ;; Switch to TTY3, where a root shell is available for shell based
         ;; install. The other root TTY's would have been ok too.
         (system* "chvt" "3")
         (run-welcome-page logo #:pci-database pci-database)))
     (,(G_ "Reboot")
      .
      ,(lambda ()
         (newt-finish)
         (reboot))))
   #:listbox-item->text car))
