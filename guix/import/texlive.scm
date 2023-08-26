;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2021, 2022, 2023 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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

(define-module (guix import texlive)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (gcrypt hash)
  #:use-module (guix derivations)
  #:use-module (guix memoization)
  #:use-module (guix monads)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (guix base32)
  #:use-module (guix serialization)
  #:use-module (guix svn-download)
  #:use-module (guix import utils)
  #:use-module (guix build-system texlive)
  #:export (files-differ?
            texlive->guix-package
            texlive-recursive-import))

;;; Commentary:
;;;
;;; Generate a package declaration template for corresponding package in the
;;; Tex Live Package Database (tlpdb).  We fetch all sources from different
;;; locations in the SVN repository of the Texlive project.
;;;
;;; Code:

;; Generic locations are parts of the tree shared by multiple packages.
;; Package definitions should single out files stored there, or all files in
;; the directory from all involved packages would be downloaded.
(define texlive-generic-locations
  (list "doc/info/"
        "doc/man/man1/"
        "doc/man/man5/"
        "doc/web2c/"
        "scripts/context/lua/"
        "scripts/context/perl/"
        "scripts/texlive/"
        "scripts/texlive-extra/"
        "tex/generic/config/"
        "tex/generic/hyphen/"
        "web2c/"))

(define string->license
  (match-lambda
    ("artistic2" 'artistic2.0)
    ("apache2" 'asl2.0)
    ("gpl" 'gpl3+)
    ("gpl1" 'gpl1)
    ("gpl1+" 'gpl1+)
    ("gpl2" 'gpl2)
    ("gpl2+" 'gpl2+)
    ("gpl3" 'gpl3)
    ("gpl3+" 'gpl3+)
    ("lgpl2.1" 'lgpl2.1)
    ("lgpl3" 'lgpl3)
    ("knuth" 'knuth)
    ("pd" 'public-domain)
    ("bsd2" 'bsd-2)
    ("bsd3" 'bsd-3)
    ("bsd4" 'bsd-4)
    ("opl" 'opl1.0+)
    ("ofl" 'silofl1.1)

    ("lpplgpl" `(list lppl gpl1+))
    ("lppl" 'lppl)
    ("lppl1" 'lppl1.0+)                 ; usually means "or later"
    ("lppl1.2" 'lppl1.2+)               ; usually means "or later"
    ("lppl1.3" 'lppl1.3+)               ; usually means "or later"
    ("lppl1.3a" 'lppl1.3a)
    ("lppl1.3b" 'lppl1.3b)
    ("lppl1.3c" 'lppl1.3c)
    ("cc0" 'cc0)
    ("cc-by-2" 'cc-by2.0)
    ("cc-by-3" 'cc-by3.0)
    ("cc-by-4" 'cc-by4.0)
    ("cc-by-sa-2" 'cc-by-sa2.0)
    ("cc-by-sa-3" 'cc-by-sa3.0)
    ("cc-by-sa-4" 'cc-by-sa4.0)
    ("mit" 'expat)
    ("x11" 'x11)
    ("fdl" 'fdl1.3+)
    ;; The GUST Font Nosource License, which is legally equivalent to
    ;; lppl1.3c+, is no longer in use (per
    ;; <https://www.gust.org.pl/projects/e-foundry/licenses>).  It has de
    ;; facto become GUST Font License 1.0.
    ((or "gfl" "gfsl") 'gfl1.0)
    ("isc" 'isc)

    ;; These are known non-free licenses
    ("noinfo" 'unknown)
    ("nosell" 'non-free)
    ("shareware" 'non-free)
    ("nosource" 'non-free)
    ("nocommercial" 'non-free)
    ("cc-by-nc-nd-1" 'non-free)
    ("cc-by-nc-nd-2" 'non-free)
    ("cc-by-nc-nd-2.5" 'non-free)
    ("cc-by-nc-nd-3" 'non-free)
    ("cc-by-nc-nd-4" 'non-free)
    ((? string? x) (string->license (string-split x #\space)))
    ((x) `(error unknown-license ,x))
    ((lst ...) `(list ,@(map string->license lst)))
    (x `(error unknown-license ,x))))

(define (guix-name name)
  "Return a Guix package name for a given Texlive package NAME."
  (string-append "texlive-"
                 (string-map (match-lambda
                               (#\_ #\-)
                               (#\. #\-)
                               (chr (char-downcase chr)))
                             name)))

(define* (translate-depends depends #:optional texlive-only)
  "Translate TeX Live packages DEPENDS into their equivalent Guix names
in `(gnu packages tex)' module, without \"texlive-\" prefix.  The function
also removes packages not necessary in Guix.

When TEXLIVE-ONLY is true, only TeX Live packages are returned."
  (delete-duplicates
   (filter-map (match-lambda
                 ;; Hyphenation.  Every TeX Live package is replaced with
                 ;; "hyphen-complete", unless "hyphen-base" is the sole
                 ;; dependency.
                 ("hyphen-base"
                  (and (not (member "hyph-utf8" depends))
                       "hyphen-base"))
                 ((or (? (cut string-prefix? "hyphen-" <>))
                      "hyph-utf8" "dehyph" "dehyph-exptl" "ruhyphen" "ukrhyph")
                  (and (not texlive-only) "hyphen-complete"))
                 ;; Binaries placeholders are ignored.
                 ((? (cut string-suffix? ".ARCH" <>)) #f)
                 ;; So are TeX Live specific packages.
                 ((or (? (cut string-prefix? "texlive-" <>))
                      "tlshell" "texlive.infra")
                  #f)
                 ;; And also development packages, which should inherit from
                 ;; the current package anyway.
                 ((? (cut string-suffix? "-dev" <>)) #f)
                 ;; Guix does not use Asymptote from TeX Live.  Ignore it.
                 ("asymptote" #f)
                 ;; TeXworks in TeX Live is only for Windows.  Don't bother.
                 ((or "texworks" "collection-texworks") #f)
                 ;; Others.
                 (name name))
               depends)))

(define (tlpdb-file)
  (define texlive-scripts
    ;; Resolve this variable lazily so that (gnu packages ...) does not end up
    ;; in the closure of this module.
    (module-ref (resolve-interface '(gnu packages tex))
                'texlive-scripts))

  (with-store store
    (run-with-store store
      (mlet* %store-monad
          ((drv (lower-object texlive-scripts))
           (built (built-derivations (list drv))))
        (match (derivation->output-paths drv)
          (((names . items) ...)
           (return (string-append (second items) ;"out"
                                  "/share/tlpkg/texlive.tlpdb"))))))))

(define tlpdb
  (memoize
   (lambda ()
     (let ((file (tlpdb-file))
           (fields
            '((name     . string)
              (shortdesc . string)
              (longdesc . string)
              (catalogue . string)
              (catalogue-license . string)
              (catalogue-ctan . string)
              (srcfiles . list)
              (runfiles . list)
              (docfiles . list)
              (binfiles . list)
              (depend   . simple-list)
              (execute  . simple-list)))
           (record
            (lambda* (key value alist #:optional (type 'string))
              (let ((new
                     (or (and=> (assoc-ref alist key)
                                (lambda (existing)
                                  (cond
                                   ((eq? type 'string)
                                    (string-append existing " " value))
                                   ((or (eq? type 'list) (eq? type 'simple-list))
                                    (cons value existing)))))
                         (cond
                          ((eq? type 'string)
                           value)
                          ((or (eq? type 'list) (eq? type 'simple-list))
                           (list value))))))
                (acons key new (alist-delete key alist))))))
       (call-with-input-file file
         (lambda (port)
           (let loop ((all (list))
                      (current (list))
                      (last-property #false))
             (let ((line (read-line port)))
               (cond
                ((eof-object? line) all)

                ;; End of record.
                ((string-null? line)
                 (loop (cons (cons (assoc-ref current 'name) current)
                             all)
                       (list) #false))

                ;; Continuation of a list
                ((and (zero? (string-index line #\space)) last-property)
                 ;; Erase optional second part of list values like
                 ;; "details=Readme" for files
                 (let ((plain-value (first
                                     (string-split
                                      (string-trim-both line) #\space))))
                   (loop all (record last-property
                                     plain-value
                                     current
                                     'list)
                         last-property)))
                (else
                 (or (and-let* ((space (string-index line #\space))
                                (key   (string->symbol (string-take line space)))
                                (value (string-drop line (1+ space)))
                                (field-type (assoc-ref fields key)))
                       ;; Erase second part of list keys like "size=29"
                       (cond
                        ((eq? field-type 'list)
                         (loop all current key))
                        (else
                         (loop all (record key value current field-type) key))))
                     (loop all current #false))))))))))))

;; Packages listed below are used to build "latex-bin" package, and therefore
;; cannot provide it automatically as a native input.  Consequently, the
;; importer sets TEXLIVE-LATEX-BIN? argument to #F for all of them.
(define latex-bin-dependency-tree
  (memoize
   (lambda (package-database)
     ;; Start out with "latex-bin", but also provide native inputs, which do
     ;; not appear as dependents, as roots for the search.
     (let loop ((packages
                 (list "latex-bin" "metafont" "modes" "tex"))
                (deps '()))
       (if (null? packages)
           ;; `translate-depends' will always translate "hyphen-base" into
           ;; "hyphen-complete".  Make sure plain hyphen-base appears in the
           ;; dependency tree.
           (cons "hyphen-base" (translate-depends deps))
           (loop (append-map (lambda (name)
                               (let ((data (assoc-ref package-database name)))
                                 (or (assoc-ref data 'depend)
                                     '())))
                             packages)
                 (append packages deps)))))))

(define (formats package-data)
  "Return a list of formats to build according to PACKAGE-DATA."
  (and=> (assoc-ref package-data 'execute)
         (lambda (actions)
           (delete-duplicates
            (filter-map
             (lambda (action)
               (match (string-split action #\space)
                 (("AddFormat" fmt . _)
                  (string-drop fmt (string-length "name=")))
                 (_ #f)))
             ;; Get the right (alphabetic) order.
             (reverse actions))))))

(define (linked-scripts name package-database)
  "Return a list of script names to symlink from \"bin/\" directory for
package NAME according to PACKAGE-DATABASE.  Consider as scripts files with
\".lua\", \".pl\", \".py\", \".rb\", \".sh\", \".tcl\", \".texlua\", \".tlu\"
extensions, and files without extension."
  (and-let* ((data (assoc-ref package-database name))
             ;; Check if binaries are associated to the package.
             (depend (assoc-ref data 'depend))
             ((member (string-append name ".ARCH") depend))
             ;; List those binaries.
             (bin-data (assoc-ref package-database
                                  ;; Any *nix-like architecture will do.
                                  (string-append name ".x86_64-linux")))
             (binaries (map basename (assoc-ref bin-data 'binfiles)))
             ;; List scripts candidates.  Bail out if there are none.
             (runfiles (assoc-ref data 'runfiles))
             (scripts (filter (cut string-prefix? "texmf-dist/scripts/" <>)
                              runfiles))
             ((pair? scripts)))
    (filter-map (lambda (script)
                  (and (any (lambda (ext)
                              (member (basename script ext) binaries))
                            '(".lua" ".pl" ".py" ".rb" ".sh" ".tcl" ".texlua"
                              ".tlu"))
                       (basename script)))
                ;; Get the right (alphabetic) order.
                (reverse scripts))))

(define* (files-differ? directory package-name
                        #:key
                        (package-database tlpdb)
                        (type #false)
                        (direction 'missing))
  "Return a list of files in DIRECTORY that differ from the expected installed
files for PACKAGE-NAME according to the PACKAGE-DATABASE.  By default all
files considered, but this can be restricted by setting TYPE to 'runfiles,
'docfiles, or 'srcfiles.  The names of files that are missing from DIRECTORY
are returned; by setting DIRECTION to anything other than 'missing, the names
of those files are returned that are unexpectedly installed."
  (define (strip-directory-prefix file-name)
    (string-drop file-name (1+ (string-length directory))))
  (let* ((data (or (assoc-ref (package-database) package-name)
                   (error (format #false
                                  "~a is not a valid package name in the TeX Live package database."
                                  package-name))))
         (files (if type
                    (or (assoc-ref data type) (list))
                    (append (or (assoc-ref data 'runfiles) (list))
                            (or (assoc-ref data 'docfiles) (list))
                            (or (assoc-ref data 'srcfiles) (list)))))
         (existing (file-system-fold
                    (const #true)                             ;enter?
                    (lambda (path stat result) (cons path result)) ;leaf
                    (lambda (path stat result) result)             ;down
                    (lambda (path stat result) result)             ;up
                    (lambda (path stat result) result)             ;skip
                    (lambda (path stat errno result) result)       ;error
                    (list)
                    directory)))
    (if (eq? direction 'missing)
        (lset-difference string=?
                         files (map strip-directory-prefix existing))
        ;; List files that are installed but should not be.
        (lset-difference string=?
                         (map strip-directory-prefix existing) files))))

(define (files->locations files)
  (define (trim-filename entry)
    (string-join (drop-right (string-split entry #\/) 1) "/" 'suffix))
  ;; Generic locations are shared by multiple packages.  Provide the full file
  ;; name to make so as to extract only the files related to the package being
  ;; imported.
  (let-values (((generic specific)
                (partition (lambda (f)
                             ;; Only grab files from generic locations, not
                             ;; sub-directories.
                             (any (cut string=? <> (trim-filename f))
                                  texlive-generic-locations))
                           files)))
    (append generic
            ;; Remove sub-directories, i.e., more specific entries with the
            ;; same prefix.
            (delete-duplicates (sort (map trim-filename specific) string<)
                               string-prefix?))))

(define (tlpdb->package name version package-database)
  (and-let* ((data (assoc-ref package-database name))
             (locs (files->locations
                    (filter-map (lambda (file)
                                  ;; Ignore any file not starting with the
                                  ;; expected prefix.  Nothing good can come
                                  ;; from this.
                                  (and (string-prefix? "texmf-dist/" file)
                                       (string-drop file (string-length "texmf-dist/"))))
                                (append (or (assoc-ref data 'docfiles) (list))
                                        (or (assoc-ref data 'runfiles) (list))
                                        (or (assoc-ref data 'srcfiles) (list))))))
             (texlive-name name)
             (name (guix-name name))
             ;; TODO: we're ignoring the VERSION argument because that
             ;; information is distributed across %texlive-tag and
             ;; %texlive-revision.
             (ref (svn-multi-reference
                   (url (string-append "svn://www.tug.org/texlive/tags/"
                                       %texlive-tag "/Master/texmf-dist"))
                   (locations locs)
                   (revision %texlive-revision)))
             ;; Ignore arch-dependent packages.
             (depends (or (assoc-ref data 'depend) '()))
             (source (with-store store
                       (download-multi-svn-to-store
                        store ref (string-append name "-svn-multi-checkout")))))
    (let* ((scripts (linked-scripts texlive-name package-database))
           (tex-formats (formats data))
           (meta-package? (null? locs))
           (empty-package? (and meta-package? (not (pair? tex-formats)))))
      (values
       `(package
          (name ,name)
          (version (number->string %texlive-revision))
          (source ,(and (not meta-package?)
                        `(texlive-origin
                          name version
                          (list ,@(sort locs string<))
                          (base32
                           ,(bytevector->nix-base32-string
                             (let-values (((port get-hash) (open-sha256-port)))
                               (write-file source port)
                               (force-output port)
                               (get-hash)))))))
          ,@(if (assoc-ref data 'docfiles)
                '((outputs '("out" "doc")))
                '())
          ;; Set build-system.
          ;;
          ;; Use trivial build system only when the package contains no files,
          ;; and no TeX format file is expected to be built.
          (build-system ,(if empty-package?
                             'trivial-build-system
                             'texlive-build-system))
          ;; Generate arguments field.
          ,@(let* ((latex-bin-dependency?
                    (member texlive-name
                            (latex-bin-dependency-tree package-database)))
                   (arguments
                    (append (if empty-package?
                                '(#:builder #~(mkdir #$output))
                                '())
                            (if latex-bin-dependency?
                                '(#:texlive-latex-bin? #f)
                                '())
                            (if (pair? scripts)
                                `(#:link-scripts #~(list ,@scripts))
                                '())
                            (if (pair? tex-formats)
                                `(#:create-formats #~(list ,@tex-formats))
                                '()))))
              (if (pair? arguments)
                  `((arguments (list ,@arguments)))
                  '()))
          ;; Native inputs.
          ;;
          ;; Texlive build system generates font metrics whenever a font
          ;; metrics file has the same base name as a Metafont file.  In this
          ;; case, provide `texlive-metafont'.
          ,@(or (and-let* ((runfiles (assoc-ref data 'runfiles))
                           (metrics
                            (filter-map (lambda (f)
                                          (and (string-suffix? ".tfm" f)
                                               (basename f ".tfm")))
                                        runfiles))
                           ((not (null? metrics)))
                           ((any (lambda (f)
                                   (and (string-suffix? ".mf" f)
                                        (member (basename f ".mf") metrics)))
                                 runfiles)))
                  '((native-inputs (list texlive-metafont))))
                '())
          ;; Inputs.
          ,@(match (append-map (lambda (s)
                                 (cond ((string-suffix? ".pl" s) '(perl))
                                       ((string-suffix? ".py" s) '(python))
                                       ((string-suffix? ".rb" s) '(ruby))
                                       ((string-suffix? ".tcl" s) '(tcl tk))
                                       (else '())))
                               (or scripts '()))
              (() '())
              (inputs `((inputs (list ,@(delete-duplicates inputs eq?))))))
          ;; Propagated inputs.
          ,@(match (translate-depends depends)
              (() '())
              (inputs
               `((propagated-inputs
                  (list ,@(map (compose string->symbol guix-name)
                               (sort inputs string<?)))))))
          (home-page
           ,(cond
             (meta-package? "https://www.tug.org/texlive/")
             ((or (assoc-ref data 'catalogue) (assoc-ref data 'name)) =>
              (cut string-append "https://ctan.org/pkg/" <>))
             (else "https://www.tug.org/texlive/")))
          (synopsis ,(assoc-ref data 'shortdesc))
          (description ,(and=> (assoc-ref data 'longdesc) beautify-description))
          (license
           ,(cond
             (meta-package?
              '(fsf-free "https://www.tug.org/texlive/copying.html"))
             ((assoc-ref data 'catalogue-license) => string->license)
             (else #f))))
       (translate-depends depends #t)))))

(define texlive->guix-package
  (memoize
   (lambda* (name #:key
                  (version (number->string %texlive-revision))
                  (package-database tlpdb)
                  #:allow-other-keys)
     "Find the metadata for NAME in the tlpdb and return the `package'
s-expression corresponding to that package, or #f on failure."
     (tlpdb->package name version (package-database)))))

(define* (texlive-recursive-import name #:key repo version)
  (recursive-import name
                    #:repo repo
                    #:version version
                    #:repo->guix-package texlive->guix-package
                    #:guix-name guix-name))

;;; texlive.scm ends here
