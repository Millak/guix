;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2021 Thiago Jung Bauermann <bauermann@kolabnow.com>
;;; Copyright © 2023-2025 Nicolas Goaziou <mail@nicolasgoaziou.fr>
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
  #:use-module (srfi srfi-2)
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

(define (texlive-input? input)
  "Return #t if INPUT is a texlive input, #f otherwise."
  (match input
    (((or "source" (? (cut string-prefix? "texlive-" <>))) . _) #t)
    (_ #f)))

(define (install-as-runfiles dir regexp)
  "Install files under DIR matching REGEXP on top of existing runfiles in the
current tree.  Sub-directories below DIR are preserved when looking for the
runfile to replace.  If a file has no matching runfile, it is ignored."
  (let ((runfiles (append-map (cut find-files <>)
                              (runfiles-root-directories))))
    (for-each (lambda (file)
                (match (filter
                        (cut string-suffix?
                             (string-drop file (string-length dir))
                             <>)
                        runfiles)
                  ;; Current file is not a runfile.  Ignore it.
                  (() #f)
                  ;; One candidate only.  Replace it with the one from DIR.
                  ((destination)
                   (let ((target (dirname destination)))
                     (install-file file target)
                     (format #t "re-generated file ~s in ~s~%"
                             (basename file)
                             target)))
                  ;; Multiple candidates!  Not much can be done.  Hopefully,
                  ;; this should never happen.
                  (_
                   (format (current-error-port)
                           "warning: ambiguous location for file ~s; ignoring it~%"
                           (basename file)))))
              (find-files dir regexp))))

(define* (patch-shell-scripts #:rest _)
  "Expand filenames for usual tools in shell scripts."
  (when (file-exists? "scripts")
    (let* ((commands '("awk" "basename" "cat" "grep" "mkdir" "rm" "sed" "sort"
                       "uname"))
           (command-regexp (format #f
                                   "\\b(~a)\\b"
                                   (string-join commands "|"))))
      (substitute* (find-files "scripts" "\\.sh$")
        ((command-regexp _ command)
         (which command))))))

(define* (configure-texmf #:rest _)
  "Ensure TEXMFVAR is writable and \"ls-R\" database is not required."
  ;; Default TEXMFVAR value is relative to $HOME, which is not set during
  ;; build.  This location is used for generating font metrics or building
  ;; documentation.
  (setenv "TEXMFVAR" (string-append (getcwd) "/texmf-var"))
  ;; By default, a "ls-R" file must exist in TEXMFDIST.  However, it isn't
  ;; generated in the TeX Live tree used to build a package.  Consequently,
  ;; relax this requirement.
  (setenv "TEXMF" "{$TEXMFSYSVAR,$TEXMFDIST}"))

(define* (delete-drv-files #:rest _)
  "Delete pre-generated \".drv\" files in order to prevent build failures."
  (when (file-exists? "source")
    (for-each delete-file (find-files "source" "\\.drv$"))))

(define* (generate-font-metrics #:key native-inputs inputs #:allow-other-keys)
  ;; Decide what Metafont files to build by comparing them to the expected
  ;; font metrics base names.  Keep only files for which the two base names
  ;; do match.
  (define (font-metrics root)
    (and (file-exists? root)
         (map (cut basename <> ".tfm") (find-files root "\\.tfm$"))))
  (define (font-files directory metrics)
    (if (file-exists? directory)
        (delete-duplicates
         (filter (lambda (f)
                   (or (not metrics)
                       (member (basename f ".mf") metrics)))
                 (find-files directory "\\.mf$")))
        '()))
  ;; Metafont files could be scattered across multiple directories.  Treat
  ;; each sub-directory as a separate font source.
  (define (font-sources root metrics)
    (delete-duplicates (map dirname (font-files root metrics))))
  (and-let* ((local-metrics (font-metrics "fonts/tfm"))
             (local-sources (font-sources "fonts/source" local-metrics))
             ((not (null? local-sources))) ;nothing to generate: bail out
             (root (getcwd))
             (metafont
              (cond ((assoc-ref (or native-inputs inputs) "texlive-metafont") =>
                     (cut string-append <> "/share/texmf-dist"))
                    (else
                     (error "Missing 'texlive-metafont' native input"))))
             ;; Collect all font source files from texlive (native-)inputs so
             ;; "mf" can know where to look for them.
             (font-inputs
              (delete-duplicates
               (append-map (match-lambda
                             ((? (negate texlive-input?)) '())
                             (("texlive-bin" . _) '())
                             (("texlive-metafont" . _)
                              (list (string-append metafont "/metafont/base")))
                             ((_ . input)
                              (font-sources input #f)))
                           (or native-inputs inputs)))))
    ;; Tell mf where to find "mf.base".
    (setenv "MFBASES" (string-append metafont "/web2c/"))
    (mkdir-p "build")
    (for-each
     (lambda (source)
       ;; Tell "mf" where are the font source files.  In case current package
       ;; provides multiple sources, treat them separately.
       (setenv "MFINPUTS"
               (string-join (cons (string-append root "/" source)
                                  font-inputs)
                            ":"))
       ;; Build font metrics (tfm).
       (with-directory-excursion source
         (for-each (lambda (font)
                     (format #t "building font ~a~%" font)
                     (invoke "mf" "-progname=mf"
                             (string-append "-output-directory="
                                            root "/build")
                             (string-append "\\"
                                            "mode:=ljfour; "
                                            "mag:=1; "
                                            "batchmode; "
                                            "input "
                                            (basename font ".mf"))))
                   (font-files "." local-metrics)))
       ;; Refresh font metrics at the appropriate location.
       (install-as-runfiles "build" "\\.tfm$"))
     local-sources)))

(define* (create-formats #:key create-formats inputs #:allow-other-keys)
  (define (collect-locations inputs pred)
    (delete-duplicates
     (append-map (match-lambda
                   ((? (negate texlive-input?)) '())
                   ((_ . dir)
                    (if pred
                        (map dirname (find-files dir pred))
                        (list dir))))
                 inputs)))
  (when create-formats
    (setenv "TFMFONTS"
            (string-join (collect-locations inputs "\\.tfm$") ":"))
    (setenv "TEXINPUTS"
            (string-join (collect-locations inputs #f) "//:" 'suffix))
    (setenv "LUAINPUTS"
            (string-join (collect-locations inputs "\\.lua$") ":"))
    (mkdir-p "web2c")
    (for-each (cut invoke "fmtutil-sys" "--byfmt" <> "--fmtdir=web2c")
              create-formats)
    ;; Remove cruft.
    (for-each delete-file (find-files "web2c" "\\.log$"))))

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
           ;; Prioritize ".ins" files over ".dtx" files.  The former only
           ;; generate runfiles whereas the latter usually also rebuild
           ;; documentation, which is not regenerated during the build process
           ;; as it would introduce some bootstrapping issues.
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
      ;;
      ;; Documentation may have been generated, but replace only runfiles,
      ;; i.e., files that belong neither to "doc" nor "source" trees.
      ;;
      ;; In TeX Live, all packages are fully pre-generated.  As a consequence,
      ;; a generated file from the "build" top directory absent from the rest of
      ;; the tree is deemed unnecessary and can safely be ignored.
      (install-as-runfiles "build" "."))))

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
    ;; Install runfiles.  The package may not contain any, though.  Create
    ;; #$output anyway to handle this situation gracefully.
    (mkdir-p out)
    (let ((texmf (string-append out "/share/texmf-dist")))
      (for-each (lambda (root)
                  (let ((destination (string-append texmf "/" root)))
                    (mkdir-p destination)
                    (copy-recursively root destination)))
                (runfiles-root-directories)))))

(define* (link-scripts #:key link-scripts outputs #:allow-other-keys)
  (when (pair? link-scripts)
    (unless (file-exists? "scripts")
      (error "missing \"scripts\" directory: no script to link"))
    (let ((bin (string-append (assoc-ref outputs "out") "/bin"))
          (filenames
           (filter (lambda (f) (any (cut string-suffix? <> f) link-scripts))
                   (find-files "scripts"))))
      ;; Sanity check: make sure no provided script is ignored.
      (let ((unknown (lset-difference string=?
                                      (map basename link-scripts)
                                      (map basename filenames))))
        (when (pair? unknown)
          (error (format #f "cannot find script(s): ~a~%"
                         (string-join unknown)))))
      ;; All lights are green.  Create "bin/" and the symlinks.
      (mkdir-p bin)
      (for-each
       (lambda (script)
         ;; Remove extension, if any.
         (let ((name (match (string-split (basename script) #\.)
                       ((name) name)
                       (tokens (string-join (drop-right tokens 1)))))
               (origin (string-append "../share/texmf-dist/" script)))
           (format #t "linking bin/~s to ~s~%" name origin)
           (symlink origin (string-append bin "/" name))))
       filenames))))

(define %standard-phases
  (modify-phases gnu:%standard-phases
    (delete 'bootstrap)
    (delete 'configure)
    (add-after 'unpack 'patch-shell-scripts patch-shell-scripts)
    (add-before 'build 'configure-texmf configure-texmf)
    (add-before 'build 'delete-drv-files delete-drv-files)
    (add-after 'delete-drv-files 'generate-font-metrics generate-font-metrics)
    (replace 'build build)
    (add-after 'build 'create-formats create-formats)
    (delete 'check)
    (replace 'install install)
    (add-after 'install 'link-scripts link-scripts)))

(define* (texlive-build #:key inputs (phases %standard-phases)
                        #:allow-other-keys #:rest args)
  "Build the given TeX Live package, applying all of PHASES in order."
  (apply gnu:gnu-build #:inputs inputs #:phases phases args))

;;; texlive-build-system.scm ends here
