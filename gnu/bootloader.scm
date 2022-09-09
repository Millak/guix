;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 David Craven <david@craven.ch>
;;; Copyright © 2017, 2020, 2022 Mathieu Othacehe <othacehe@gnu.org>
;;; Copyright © 2017 Leo Famulari <leo@famulari.name>
;;; Copyright © 2019, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2020 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2022 Josselin Poiret <dev@jpoiret.xyz>
;;; Copyright © 2022 Reza Alizadeh Majd <r.majd@pantherx.org>
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

(define-module (gnu bootloader)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system uuid)
  #:use-module (guix discovery)
  #:use-module (guix gexp)
  #:use-module (guix profiles)
  #:use-module (guix records)
  #:use-module (guix deprecation)
  #:use-module ((guix ui) #:select (warn-about-load-error))
  #:use-module (guix diagnostics)
  #:use-module (guix i18n)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (ice-9 match)
  #:export (menu-entry
            menu-entry?
            menu-entry-label
            menu-entry-device
            menu-entry-linux
            menu-entry-linux-arguments
            menu-entry-initrd
            menu-entry-device-mount-point
            menu-entry-multiboot-kernel
            menu-entry-multiboot-arguments
            menu-entry-multiboot-modules
            menu-entry-chain-loader

            menu-entry->sexp
            sexp->menu-entry

            bootloader
            bootloader?
            bootloader-name
            bootloader-package
            bootloader-installer
            bootloader-disk-image-installer
            bootloader-configuration-file
            bootloader-configuration-file-generator

            bootloader-configuration
            bootloader-configuration?
            bootloader-configuration-bootloader
            bootloader-configuration-target ;deprecated
            bootloader-configuration-targets
            bootloader-configuration-menu-entries
            bootloader-configuration-default-entry
            bootloader-configuration-timeout
            bootloader-configuration-keyboard-layout
            bootloader-configuration-theme
            bootloader-configuration-terminal-outputs
            bootloader-configuration-terminal-inputs
            bootloader-configuration-serial-unit
            bootloader-configuration-serial-speed
            bootloader-configuration-device-tree-support?

            %bootloaders
            lookup-bootloader-by-name

            efi-bootloader-chain))


;;;
;;; Menu-entry record.
;;;

(define-record-type* <menu-entry>
  menu-entry make-menu-entry
  menu-entry?
  (label           menu-entry-label)
  (device          menu-entry-device       ; file system uuid, label, or #f
                   (default #f))
  (device-mount-point menu-entry-device-mount-point
                   (default #f))
  (linux           menu-entry-linux
                   (default #f))
  (linux-arguments menu-entry-linux-arguments
                   (default '()))          ; list of string-valued gexps
  (initrd          menu-entry-initrd       ; file name of the initrd as a gexp
                   (default #f))
  (multiboot-kernel menu-entry-multiboot-kernel
                    (default #f))
  (multiboot-arguments menu-entry-multiboot-arguments
                       (default '()))      ; list of string-valued gexps
  (multiboot-modules menu-entry-multiboot-modules
                     (default '()))        ; list of multiboot commands, where
                                           ; a command is a list of <string>
  (chain-loader     menu-entry-chain-loader
                    (default #f)))         ; string, path of efi file

(define (report-menu-entry-error menu-entry)
  (raise
   (condition
    (&message
     (message
      (format #f (G_ "invalid menu-entry: ~a") menu-entry)))
    (&fix-hint
     (hint
      (G_ "Please chose only one of:
@enumerate
@item direct boot by specifying fields @code{linux},
@code{linux-arguments} and @code{linux-modules},
@item multiboot by specifying fields @code{multiboot-kernel},
@code{multiboot-arguments} and @code{multiboot-modules},
@item chain-loader by specifying field @code{chain-loader}.
@end enumerate"))))))

(define (menu-entry->sexp entry)
  "Return ENTRY serialized as an sexp."
  (define (device->sexp device)
    (match device
      ((? uuid? uuid)
       `(uuid ,(uuid-type uuid) ,(uuid->string uuid)))
      ((? file-system-label? label)
       `(label ,(file-system-label->string label)))
      (_ device)))
  (match entry
    (($ <menu-entry> label device mount-point
                     (? identity linux) linux-arguments (? identity initrd)
                     #f () () #f)
     `(menu-entry (version 0)
                  (label ,label)
                  (device ,(device->sexp device))
                  (device-mount-point ,mount-point)
                  (linux ,linux)
                  (linux-arguments ,linux-arguments)
                  (initrd ,initrd)))
    (($ <menu-entry> label device mount-point #f () #f
                     (? identity multiboot-kernel) multiboot-arguments
                     multiboot-modules #f)
     `(menu-entry (version 0)
                  (label ,label)
                  (device ,(device->sexp device))
                  (device-mount-point ,mount-point)
                  (multiboot-kernel ,multiboot-kernel)
                  (multiboot-arguments ,multiboot-arguments)
                  (multiboot-modules ,multiboot-modules)))
    (($ <menu-entry> label device mount-point #f () #f #f () ()
                     (? identity chain-loader))
     `(menu-entry (version 0)
                  (label ,label)
                  (device ,(device->sexp device))
                  (device-mount-point ,mount-point)
                  (chain-loader ,chain-loader)))
    (_ (report-menu-entry-error entry))))

(define (sexp->menu-entry sexp)
  "Turn SEXP, an sexp as returned by 'menu-entry->sexp', into a <menu-entry>
record."
  (define (sexp->device device-sexp)
    (match device-sexp
      (('uuid type uuid-string)
       (uuid uuid-string type))
      (('label label)
       (file-system-label label))
      (_ device-sexp)))
  (match sexp
    (('menu-entry ('version 0)
                  ('label label) ('device device)
                  ('device-mount-point mount-point)
                  ('linux linux) ('linux-arguments linux-arguments)
                  ('initrd initrd) _ ...)
     (menu-entry
      (label label)
      (device (sexp->device device))
      (device-mount-point mount-point)
      (linux linux)
      (linux-arguments linux-arguments)
      (initrd initrd)))
    (('menu-entry ('version 0)
                  ('label label) ('device device)
                  ('device-mount-point mount-point)
                  ('multiboot-kernel multiboot-kernel)
                  ('multiboot-arguments multiboot-arguments)
                  ('multiboot-modules multiboot-modules) _ ...)
     (menu-entry
      (label label)
      (device (sexp->device device))
      (device-mount-point mount-point)
      (multiboot-kernel multiboot-kernel)
      (multiboot-arguments multiboot-arguments)
      (multiboot-modules multiboot-modules)))
    (('menu-entry ('version 0)
                  ('label label) ('device device)
                  ('device-mount-point mount-point)
                  ('chain-loader chain-loader) _ ...)
     (menu-entry
      (label label)
      (device (sexp->device device))
      (device-mount-point mount-point)
      (chain-loader chain-loader)))))


;;;
;;; Bootloader record.
;;;

;; The <bootloader> record contains fields expressing how the bootloader
;; should be installed. Every bootloader in gnu/bootloader/ directory
;; has to be described by this record.

(define-record-type* <bootloader>
  bootloader make-bootloader
  bootloader?
  (name                            bootloader-name)
  (package                         bootloader-package)
  (installer                       bootloader-installer)
  (disk-image-installer            bootloader-disk-image-installer
                                   (default #f))
  (configuration-file              bootloader-configuration-file)
  (configuration-file-generator    bootloader-configuration-file-generator))


;;;
;;; Bootloader configuration record.
;;;

;; The <bootloader-configuration> record contains bootloader independant
;; configuration used to fill bootloader configuration file.

(define-with-syntax-properties (warn-target-field-deprecation
                                (value properties))
  (when value
    (warning (source-properties->location properties)
             (G_ "the 'target' field is deprecated, please use 'targets' \
instead~%")))
  value)

(define-record-type* <bootloader-configuration>
  bootloader-configuration make-bootloader-configuration
  bootloader-configuration?
  (bootloader
   bootloader-configuration-bootloader) ;<bootloader>
  (targets               %bootloader-configuration-targets
                         (default #f))     ;list of strings
  (target                %bootloader-configuration-target ;deprecated
                         (default #f)
                         (sanitize warn-target-field-deprecation))
  (menu-entries          bootloader-configuration-menu-entries
                         (default '()))   ;list of <menu-entry>
  (default-entry         bootloader-configuration-default-entry
                         (default 0))     ;integer
  (timeout               bootloader-configuration-timeout
                         (default 5))     ;seconds as integer
  (keyboard-layout       bootloader-configuration-keyboard-layout
                         (default #f))    ;<keyboard-layout> | #f
  (theme                 bootloader-configuration-theme
                         (default #f))    ;bootloader-specific theme
  (terminal-outputs      bootloader-configuration-terminal-outputs
                         (default '(gfxterm)))   ;list of symbols
  (terminal-inputs       bootloader-configuration-terminal-inputs
                         (default '()))   ;list of symbols
  (serial-unit           bootloader-configuration-serial-unit
                         (default #f))    ;integer | #f
  (serial-speed          bootloader-configuration-serial-speed
                         (default #f))    ;integer | #f
  (device-tree-support?  bootloader-configuration-device-tree-support?
                         (default #t)))   ;boolean

(define-deprecated (bootloader-configuration-target config)
  bootloader-configuration-targets
  (%bootloader-configuration-target config))

(define (bootloader-configuration-targets config)
  (or (%bootloader-configuration-targets config)
      ;; TODO: Remove after the deprecated 'target' field is removed.
      (list (%bootloader-configuration-target config))
      ;; XXX: At least the GRUB installer (see (gnu bootloader grub)) has this
      ;; peculiar behavior of installing fonts and GRUB modules when DEVICE is #f,
      ;; hence the default value of '(#f) rather than '().
      (list #f)))


;;;
;;; Bootloaders.
;;;

(define (bootloader-modules)
  "Return the list of bootloader modules."
  (all-modules (map (lambda (entry)
                      `(,entry . "gnu/bootloader"))
                    %load-path)
               #:warn warn-about-load-error))

(define %bootloaders
  ;; The list of publically-known bootloaders.
  (delay (fold-module-public-variables (lambda (obj result)
                                         (if (bootloader? obj)
                                             (cons obj result)
                                             result))
                                       '()
                                       (bootloader-modules))))

(define (lookup-bootloader-by-name name)
  "Return the bootloader called NAME."
  (or (find (lambda (bootloader)
              (eq? name (bootloader-name bootloader)))
            (force %bootloaders))
      (leave (G_ "~a: no such bootloader~%") name)))

(define (efi-bootloader-profile files bootloader-package hooks)
  "Creates a profile with BOOTLOADER-PACKAGE and a directory collection/ with
links to additional FILES from the store.  This collection is meant to be used
by the bootloader installer.

FILES is a list of file or directory names from the store, which will be
symlinked into the collection/ directory.  If a directory name ends with '/',
then the directory content instead of the directory itself will be symlinked
into the collection/ directory.

FILES may contain file like objects produced by functions like plain-file,
local-file, etc., or package contents produced with file-append.

HOOKS lists additional hook functions to modify the profile."
  (define (bootloader-collection manifest)
    (define build
        (with-imported-modules '((guix build utils)
                                 (ice-9 ftw)
                                 (srfi srfi-1)
                                 (srfi srfi-26))
          #~(begin
            (use-modules ((guix build utils)
                          #:select (mkdir-p strip-store-file-name))
                         ((ice-9 ftw)
                          #:select (scandir))
                         ((srfi srfi-1)
                          #:select (append-map every remove))
                         ((srfi srfi-26)
                          #:select (cut)))
            (define (symlink-to file directory transform)
              "Creates a symlink to FILE named (TRANSFORM FILE) in DIRECTORY."
              (symlink file (string-append directory "/" (transform file))))
            (define (directory-content directory)
              "Creates a list of absolute path names inside DIRECTORY."
              (map (lambda (name)
                     (string-append directory name))
                   (or (scandir directory (lambda (name)
                                            (not (member name '("." "..")))))
                       '())))
            (define name-ends-with-/? (cut string-suffix? "/" <>))
            (define (name-is-store-entry? name)
              "Return #t if NAME is a direct store entry and nothing inside."
              (not (string-index (strip-store-file-name name) #\/)))
            (let* ((collection (string-append #$output "/collection"))
                   (files '#$files)
                   (directories (filter name-ends-with-/? files))
                   (names-from-directories
                    (append-map (lambda (directory)
                                  (directory-content directory))
                                directories))
                   (names (append names-from-directories
                                  (remove name-ends-with-/? files))))
              (mkdir-p collection)
              (if (every file-exists? names)
                  (begin
                    (for-each (lambda (name)
                               (symlink-to name collection
                                            (if (name-is-store-entry? name)
                                                strip-store-file-name
                                                basename)))
                              names)
                    #t)
                  #f)))))

    (gexp->derivation "bootloader-collection"
                      build
                      #:local-build? #t
                      #:substitutable? #f
                      #:properties
                      `((type . profile-hook)
                        (hook . bootloader-collection))))

  (profile (content (packages->manifest (list bootloader-package)))
           (name "bootloader-profile")
           (hooks (append (list bootloader-collection) hooks))
           (locales? #f)
           (allow-collisions? #f)
           (relative-symlinks? #f)))

(define* (efi-bootloader-chain files
                               final-bootloader
                               #:key
                               (hooks '())
                               installer)
  "Define a bootloader chain with FINAL-BOOTLOADER as the final bootloader and
certain directories and files from the store given in the list of FILES.

FILES may contain file like objects produced by functions like plain-file,
local-file, etc., or package contents produced with file-append.  They will be
collected inside a directory collection/ inside a generated bootloader profile,
which will be passed to the INSTALLER.

If a directory name in FILES ends with '/', then the directory content instead
of the directory itself will be symlinked into the collection/ directory.

The procedures in the HOOKS list can be used to further modify the bootloader
profile.  It is possible to pass a single function instead of a list.

If the INSTALLER argument is used, then this function will be called to install
the bootloader.  Otherwise the installer of the FINAL-BOOTLOADER will be called."
  (let* ((final-installer (or installer
                              (bootloader-installer final-bootloader)))
         (profile (efi-bootloader-profile files
                                          (bootloader-package final-bootloader)
                                          (if (list? hooks)
                                              hooks
                                              (list hooks)))))
    (bootloader
     (inherit final-bootloader)
     (package profile)
     (installer
      #~(lambda (bootloader target mount-point)
          (#$final-installer bootloader target mount-point)
          (copy-recursively
           (string-append bootloader "/collection")
           (string-append mount-point target)
           #:follow-symlinks? #t
           #:log (%make-void-port "w")))))))
