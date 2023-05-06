;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2021 Thiago Jung Bauermann <bauermann@kolabnow.com>
;;; Copyright © 2023 Nicolas Goaziou <mail@nicolasgoaziou.fr>
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

(define-module (guix build texlive-build-system)
  #:use-module ((guix build gnu-build-system) #:prefix gnu:)
  #:use-module (guix build utils)
  #:use-module (guix build union)
  #:use-module (ice-9 format)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (%standard-phases
            texlive-build))

;; Commentary:
;;
;; Builder-side code of the standard build procedure for TeX Live packages.
;;
;; Code:

(define (runfiles-root-directories)
  "Return list of root directories containing runfiles."
  (scandir "."
           (negate
            (cut member <> '("." ".." "build" "doc" "source")))))

(define* (delete-drv-files #:rest _)
  "Delete pre-generated \".drv\" files in order to prevent build failures."
  (when (file-exists? "source")
    (for-each delete-file (find-files "source" "\\.drv$"))))

(define (compile-with-latex engine format output file)
  (invoke engine
          "-interaction=nonstopmode"
          (string-append "-output-directory=" output)
          (if format (string-append "&" format) "-ini")
          file))

(define* (build #:key inputs build-targets tex-engine tex-format
                #:allow-other-keys)
  (let ((targets
         (cond
          (build-targets
           ;; Collect the relative file names of all the specified targets.
           (append-map (lambda (target)
                         (find-files "source"
                                     (lambda (f _)
                                       (string-suffix? (string-append "/" target)
                                                       f))))
                       build-targets))
          ((directory-exists? "source")
           ;; Prioritize ".ins" files over ".dtx" files.  There's no
           ;; scientific reasoning here; it just seems to work better.
           (match (find-files "source" "\\.ins$")
             (() (find-files "source" "\\.dtx$"))
             (files files)))
          (else '()))))
    (unless (null? targets)
      (let ((output (string-append (getcwd) "/build")))
        (mkdir-p output)
        (for-each (lambda (target)
                    (with-directory-excursion (dirname target)
                      (compile-with-latex tex-engine
                                          tex-format
                                          output
                                          (basename target))))
                  targets))
      ;; Now move generated files from the "build" directory into the rest of
      ;; the source tree, effectively replacing downloaded files.

      ;; Documentation may have been generated, but replace only runfiles,
      ;; i.e., files that belong neither to "doc" nor "source" trees.
      ;;
      ;; In TeX Live, all packages are fully pre-generated.  As a consequence,
      ;; a generated file from the "build" top directory absent from the rest
      ;; of the tree is deemed unnecessary and can safely be ignored.
      (let ((runfiles (append-map (cut find-files <>)
                                  (runfiles-root-directories))))
        (for-each (lambda (file)
                    (match (filter
                            (cut string-suffix?
                                 (string-drop file (string-length "build"))
                                 <>)
                            runfiles)
                      ;; Current file is not a runfile.  Ignore it.
                      (() #f)
                      ;; One candidate only.  Replace it with the one just
                      ;; generated.
                      ((destination)
                       (let ((target (dirname destination)))
                         (install-file file target)
                         (format #t "re-generated file ~s in ~s~%"
                                 (basename file)
                                 target)))
                      ;; Multiple candidates!  Not much can be done.
                      ;; Hopefully, this should never happen.
                      (_
                       (format (current-error-port)
                               "warning: ambiguous localization of file ~s; \
ignoring it~%"
                               (basename file)))))
                  ;; Preserve the relative file name of the generated file in
                  ;; order to be more accurate when looking for the
                  ;; corresponding runfile in the tree.
                  (find-files "build"))))))

(define* (install #:key outputs #:allow-other-keys)
  (let ((out (assoc-ref outputs "out"))
        (doc (assoc-ref outputs "doc")))
    ;; Take care of documentation.
    (when (directory-exists? "doc")
      (unless doc
        (format (current-error-port)
                "warning: missing 'doc' output for package documentation~%"))
      (let ((doc-dir (string-append (or doc out) "/share/texmf-dist/doc")))
        (mkdir-p doc-dir)
        (copy-recursively "doc" doc-dir)))
    ;; Handle runfiles.
    (let ((texmf (string-append (assoc-ref outputs "out") "/share/texmf-dist")))
      (for-each (lambda (root)
                  (let ((destination (string-append texmf "/" root)))
                    (mkdir-p destination)
                    (copy-recursively root destination)))
                (runfiles-root-directories)))))

(define %standard-phases
  (modify-phases gnu:%standard-phases
    (delete 'bootstrap)
    (delete 'configure)
    (add-before 'build 'delete-drv-files delete-drv-files)
    (replace 'build build)
    (delete 'check)
    (replace 'install install)))

(define* (texlive-build #:key inputs (phases %standard-phases)
                        #:allow-other-keys #:rest args)
  "Build the given TeX Live package, applying all of PHASES in order."
  (apply gnu:gnu-build #:inputs inputs #:phases phases args))

;;; texlive-build-system.scm ends here
