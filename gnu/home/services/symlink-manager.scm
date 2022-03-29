;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 Andrew Tropin <andrew@trop.in>
;;; Copyright © 2021 Xinglu Chen <public@yoctocell.xyz>
;;; Copyright © 2022 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu home services symlink-manager)
  #:use-module (gnu home services)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:export (home-symlink-manager-service-type))

;;; Comment:
;;;
;;; symlink-manager cares about xdg configurations and other files: it backs
;;; up files created by user, removes symlinks and directories created by a
;;; previous generation, and creates new directories and symlinks to files
;;; according to the content of directories (created by home-files-service) of
;;; the current home environment generation.
;;;
;;; Code:

(define (update-symlinks-script)
  (program-file
   "update-symlinks"
   (with-imported-modules (source-module-closure
                           '((guix build utils)
                             (guix i18n)))
     #~(begin
         (use-modules (ice-9 ftw)
                      (ice-9 match)
                      (srfi srfi-1)
                      (guix i18n)
                      (guix build utils))

         (define home-directory
           (getenv "HOME"))

         (define xdg-config-home
           (or (getenv "XDG_CONFIG_HOME")
               (string-append (getenv "HOME") "/.config")))

         (define xdg-data-home
           (or (getenv "XDG_DATA_HOME")
               (string-append (getenv "HOME") "/.local/share")))

         (define backup-directory
           (string-append home-directory "/" (number->string (current-time))
                          "-guix-home-legacy-configs-backup"))

         (define (preprocess-file file)
           "If file is in XDG-CONFIGURATION-FILES-DIRECTORY use
subdirectory from XDG_CONFIG_HOME to generate a target path."
           (cond
            ((string-prefix? #$xdg-configuration-files-directory file)
             (string-append
              (substring xdg-config-home
                         (1+ (string-length home-directory)))
              (substring file
                         (string-length #$xdg-configuration-files-directory))))
            ((string-prefix? #$xdg-data-files-directory file)
             (string-append
              (substring xdg-data-home
                         (1+ (string-length home-directory)))
              (substring file
                         (string-length #$xdg-data-files-directory))))
            (else file)))

         (define (target-file file)
           ;; Return the target of FILE, a config file name sans leading dot
           ;; such as "config/fontconfig/fonts.conf" or "bashrc".
           (string-append home-directory "/" (preprocess-file file)))

         (define (no-follow-file-exists? file)
           "Return #t if file exists, even if it's a dangling symlink."
           (->bool (false-if-exception (lstat file))))

         (define (symlink-to-store? file)
           (catch 'system-error
             (lambda ()
               (store-file-name? (readlink file)))
             (lambda args
               (if (= EINVAL (system-error-errno args))
                   #f
                   (apply throw args)))))

         (define (backup-file file)
           (define backup
             (string-append backup-directory "/" (preprocess-file file)))

           (mkdir-p backup-directory)
           (format #t (G_ "Backing up ~a...") (target-file file))
           (mkdir-p (dirname backup))
           (rename-file (target-file file) backup)
           (display (G_ " done\n")))

         (define (cleanup-symlinks home-generation)
           ;; Delete from $HOME files that originate in HOME-GENERATION, the
           ;; store item containing a home generation.
           (define config-file-directory
             ;; Note: Trailing slash is needed because "files" is a symlink.
             (string-append home-generation "/" #$home-files-directory "/"))

           (define (strip file)
             (string-drop file
                          (+ 1 (string-length config-file-directory))))

           (format #t (G_ "Cleaning up symlinks from previous home at ~a.~%")
                   home-generation)
           (newline)

           (file-system-fold
            (const #t)
            (lambda (file stat _)                 ;leaf
              (let ((file (target-file (strip file))))
                (when (no-follow-file-exists? file)
                  ;; DO NOT remove the file if it is no longer a symlink to
                  ;; the store, it will be backed up later during
                  ;; create-symlinks phase.
                  (if (symlink-to-store? file)
                      (begin
                        (format #t (G_ "Removing ~a...") file)
                        (delete-file file)
                        (display (G_ " done\n")))
                      (format
                       #t
                       (G_ "Skipping ~a (not a symlink to store)... done\n")
                       file)))))

            (const #t)                            ;down
            (lambda (directory stat _)            ;up
              (unless (string=? directory config-file-directory)
                (let ((directory (target-file (strip directory))))
                  (catch 'system-error
                    (lambda ()
                      (rmdir directory)
                      (format #t (G_ "Removed ~a.\n") directory))
                    (lambda args
                      (let ((errno (system-error-errno args)))
                        (cond
                         ((= ENOTEMPTY errno)
                          (format
                           #t
                           (G_ "Skipping ~a (not an empty directory)... done\n")
                           directory))
                         ((= ENOENT errno) #t)
                         ((= ENOTDIR errno) #t)
                         (else
                          (apply throw args)))))))))
            (const #t)                            ;skip
            (const #t)                            ;error
            #t                                    ;init
            config-file-directory
            lstat)

           (display (G_ "Cleanup finished.\n\n")))

         (define (create-symlinks home-generation)
           ;; Create in $HOME symlinks for the files in HOME-GENERATION.
           (define config-file-directory
             ;; Note: Trailing slash is needed because "files" is a symlink.
             (string-append home-generation "/" #$home-files-directory "/"))

           (define (strip file)
             (string-drop file
                          (+ 1 (string-length config-file-directory))))

           (define (source-file file)
             (readlink (string-append config-file-directory file)))

           (file-system-fold
            (const #t)                            ;enter?
            (lambda (file stat result)            ;leaf
              (let ((source (source-file (strip file)))
                    (target (target-file (strip file))))
                (when (no-follow-file-exists? target)
                  (backup-file (strip file)))
                (format #t (G_ "Symlinking ~a -> ~a...")
                        target source)
                (symlink source target)
                (display (G_ " done\n"))))
            (lambda (directory stat result)       ;down
              (unless (string=? directory config-file-directory)
                (let ((target (target-file (strip directory))))
                  (when (and (no-follow-file-exists? target)
                             (not (file-is-directory? target)))
                    (backup-file (strip directory)))

                  (catch 'system-error
                    (lambda ()
                      (mkdir target))
                    (lambda args
                      (let ((errno (system-error-errno args)))
                        (unless (= EEXIST errno)
                          (format #t (G_ "failed to create directory ~a: ~s~%")
                                  target (strerror errno))
                          (apply throw args))))))))
            (const #t)                            ;up
            (const #t)                            ;skip
            (const #t)                            ;error
            #t                                    ;init
            config-file-directory))

         #$%initialize-gettext

         (let* ((home     (string-append home-directory "/.guix-home"))
                (pivot    (string-append home ".new"))
                (new-home (getenv "GUIX_NEW_HOME"))
                (old-home (getenv "GUIX_OLD_HOME")))
           (when old-home
             (cleanup-symlinks old-home))

           (create-symlinks new-home)

           (symlink new-home pivot)
           (rename-file pivot home)

           (display (G_" done\nFinished updating symlinks.\n\n")))))))

(define (update-symlinks-gexp _)
  #~(primitive-load #$(update-symlinks-script)))

(define home-symlink-manager-service-type
  (service-type (name 'home-symlink-manager)
                (extensions
                 (list
                  (service-extension
                   home-activation-service-type
                   update-symlinks-gexp)))
                (default-value #f)
                (description "Provide an @code{update-symlinks}
script, which creates symlinks to configuration files and directories
on every activation.  If an existing file would be overwritten by a
symlink, backs up that file first.")))
