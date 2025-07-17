;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 David Craven <david@craven.ch>
;;; Copyright © 2019, 2020, 2021 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2021, 2022, 2023, 2024 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2021 Brendan Tildesley <mail@brendan.scot>
;;; Copyright © 2024 Zheng Junjie <873216071@qq.com>
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

(define-module (guix build qt-utils)
  #:use-module (guix build utils)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-71)
  #:export (wrap-qt-program
            wrap-all-qt-programs
            %qt-wrap-excluded-inputs))

(define %default-qt-major-version "5")

(define %qt-wrap-excluded-inputs
  '(list "cmake" "extra-cmake-modules" "qttools"))

;; NOTE: Apart from standard subdirectories of /share, Qt also provides
;; facilities for per-application data directories, such as
;; /share/quassel. Thus, we include the output directory even if it doesn't
;; contain any of the standard subdirectories.
(define* (variables-for-wrapping base-directories output-directory
                                 #:key
                                 (qt-major-version %default-qt-major-version))

  (define (collect-sub-dirs base-directories file-type subdirectory selectors)
    ;; Append SUBDIRECTORY and each of BASE-DIRECTORIES, and return the subset
    ;; that exists and has at least one of the SELECTORS sub-directories,
    ;; unless SELECTORS is the empty list.  FILE-TYPE should by 'directory or
    ;; 'regular file.  For the later, it allows searching for plain files
    ;; rather than directories.
    (define exists? (match file-type
                      ('directory directory-exists?)
                      ('regular file-exists?)))

    (filter-map (lambda (dir)
                  (let ((directory (string-append dir subdirectory)))
                    (and (exists? directory)
                         (or (null? selectors)
                             (any (lambda (selector)
                                    (exists?
                                     (string-append directory selector)))
                                  selectors))
                         directory)))
                base-directories))

  (filter-map
   (match-lambda
     ((variable type file-type directory selectors ...)
      (match (collect-sub-dirs base-directories file-type directory selectors)
        (()
         #f)
        (directories
         `(,variable ,type ,directories)))))
   ;; These shall match the search-path-specification for Qt and KDE
   ;; libraries.
   (list
    ;; The XDG environment variables are defined with the 'suffix type, which
    ;; allows the users to override or extend their value, so that custom icon
    ;; themes can be honored, for example.
    '("XDG_DATA_DIRS" suffix directory "/share"
      ;; These are "selectors": consider /share if and only if at least
      ;; one of these sub-directories exist.  This avoids adding
      ;; irrelevant packages to XDG_DATA_DIRS just because they have a
      ;; /share sub-directory.
      "/applications" "/cursors" "/fonts" "/icons" "/glib-2.0/schemas"
      "/mime" "/sounds" "/themes" "/wallpapers")
    '("XDG_CONFIG_DIRS" suffix directory "/etc/xdg")
    `("QT_PLUGIN_PATH" prefix directory
      ,(format #f "/lib/qt~a/plugins" qt-major-version))
    `(,(if (>= (string->number qt-major-version) 6)
           "QML_IMPORT_PATH"
           "QML2_IMPORT_PATH")
      prefix directory ,(format #f "/lib/qt~a/qml" qt-major-version)))))

(define* (wrap-qt-program* program #:key sh inputs output-dir
                           qt-wrap-excluded-inputs
                           (qt-major-version %default-qt-major-version))

  (define input-directories
    (filter-map
     (match-lambda
      ((label . directory)
       (and (not (member label qt-wrap-excluded-inputs))
            directory)))
     inputs))

  (let ((vars-to-wrap (variables-for-wrapping
                       (cons output-dir input-directories)
                       output-dir
                       #:qt-major-version qt-major-version)))
    (when (not (null? vars-to-wrap))
      (apply wrap-program program #:sh sh vars-to-wrap))))

(define* (wrap-qt-program program-name #:key (sh (which "bash")) inputs output
                          (qt-wrap-excluded-inputs %qt-wrap-excluded-inputs)
                          (qt-major-version %default-qt-major-version))
  "Wrap the specified program (which must reside in the OUTPUT's \"/bin\"
directory) with suitably set environment variables.

This is like qt-build-systems's phase \"qt-wrap\", but only the named program
is wrapped."
  (wrap-qt-program* (string-append output "/bin/" program-name)
                    #:sh sh
                    #:output-dir output #:inputs inputs
                    #:qt-wrap-excluded-inputs qt-wrap-excluded-inputs
                    #:qt-major-version qt-major-version))

(define* (wrap-all-qt-programs #:key (sh (which "bash")) inputs outputs
                               qtbase
                               (qt-wrap-excluded-outputs '())
                               (qt-wrap-excluded-inputs %qt-wrap-excluded-inputs)
                               #:allow-other-keys)
  "Implement qt-build-systems's phase \"qt-wrap\": look for executables in
\"bin\", \"sbin\" and \"libexec\" of all outputs and create wrappers with
suitably set environment variables if found.

Wrapping is not applied to outputs whose name is listed in
QT-WRAP-EXCLUDED-OUTPUTS.  This is useful when an output is known not
to contain any Qt binaries, and where wrapping would gratuitously
add a dependency of that output on Qt."
  (define qt-major-version
    (if qtbase
        (let ((_ version (package-name->name+version
                          (strip-store-file-name qtbase))))
          (first (string-split version #\.)))
        ;; Provide a fall-back for build systems not having a #:qtbase
        ;; argument.
        %default-qt-major-version))

  (define (find-files-to-wrap output-dir)
    (append-map
     (lambda (dir)
       (if (directory-exists? dir)
           (find-files dir (lambda (file stat)
                             (not (wrapped-program? file))))
           (list)))
     (list (string-append output-dir "/bin")
           (string-append output-dir "/sbin")
           (string-append output-dir "/libexec")
           (string-append output-dir "/lib/libexec"))))

  (define handle-output
    (match-lambda
     ((output . output-dir)
      (unless (member output qt-wrap-excluded-outputs)
        (for-each (cut wrap-qt-program* <>
                       #:sh sh
                       #:output-dir output-dir #:inputs inputs
                       #:qt-wrap-excluded-inputs qt-wrap-excluded-inputs
                       #:qt-major-version qt-major-version)
                  (find-files-to-wrap output-dir))))))

  (for-each handle-output outputs))
