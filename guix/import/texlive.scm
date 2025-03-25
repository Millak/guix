;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2021, 2022, 2023 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2021, 2024 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2024 Nicolas Goaziou <mail@nicolasgoaziou.fr>
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
  #:use-module (gcrypt hash)
  #:use-module (guix base32)
  #:use-module (guix build-system)
  #:use-module (guix build-system texlive)
  #:use-module (guix derivations)
  #:use-module (guix diagnostics)
  #:use-module (guix gexp)
  #:use-module (guix i18n)
  #:use-module (guix import utils)
  #:use-module (guix memoization)
  #:use-module (guix monads)
  #:use-module (guix packages)
  #:use-module ((guix serialization) #:select (write-file))
  #:use-module (guix store)
  #:use-module (guix svn-download)
  #:use-module (guix upstream)
  #:use-module ((guix utils) #:select (downstream-package-name
                                       version>? version-prefix?))
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  #:export (texlive->guix-package
            texlive-recursive-import
            %texlive-updater))

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

;; The following packages do not have any auxiliary "-bin" package to
;; propagate, even if they do have a corresponding ".ARCH" entry in the TeX
;; Live package database.  They fall into 3 categories:
;;
;; 1. Associated entries in NAME.ARCH are already provided by TEXLIVE-BIN.
;;
;; 2. Associated entries in NAME.ARCH are symlinks to binaries provided by
;; TEXLIVE-BIN.
;;
;; 3. They fool the (naive) algorithm for "-bin" propagation and generate
;; false positives.  This generally happens when the package creates multiple
;; symlinks to a script it bundles.
(define no-bin-propagation-packages
  (list
   ;; Category 1.
   "ctie"
   "cweb"
   "luahbtex"
   "luatex"
   "metafont"
   "pdftex"
   "pdftosrc"
   "synctex"
   "tex"
   "tie"
   "web"
   ;; Category 2.
   "amstex"
   "csplain"
   "eplain"
   "jadetex"
   "latex-bin"
   "lollipop"
   "mex"
   "mltex"
   "optex"
   "platex"
   "uplatex"
   "texsis"
   "xmltex"
   ;; Category 3.
   "biber"
   "context"
   "cluttex"
   "esptopdf"
   "pdfcrop"
   "texdef"))

;; Guix introduces two specific packages based on TEXLIVE-BUILD-SYSTEM.  Add
;; an entry for them in the package database, so they can be imported, and
;; updated, like any other regular TeX Live package.
(define tlpdb-guix-packages
  '(("hyphen-complete"
     (docfiles "texmf-dist/doc/generic/dehyph-exptl/"
               "texmf-dist/doc/generic/elhyphen/"
               "texmf-dist/doc/generic/huhyphen/"
               "texmf-dist/doc/generic/hyph-utf8/"
               "texmf-dist/doc/luatex/hyph-utf8/"
               "texmf-dist/doc/generic/ukrhyph/")
     (runfiles "texmf-dist/tex/generic/config/"
               "texmf-dist/tex/generic/dehyph/"
               "texmf-dist/tex/generic/dehyph-exptl/"
               "texmf-dist/tex/generic/hyph-utf8/"
               "texmf-dist/tex/generic/hyphen/"
               "texmf-dist/tex/generic/ruhyphen/"
               "texmf-dist/tex/generic/ukrhyph/"
               "texmf-dist/tex/luatex/hyph-utf8/")
     (srcfiles "texmf-dist/source/generic/hyph-utf8/"
               "texmf-dist/source/luatex/hyph-utf8/"
               "texmf-dist/source/generic/ruhyphen/")
     (shortdesc . "Hyphenation patterns expressed in UTF-8")
     (longdesc . "Modern native UTF-8 engines such as XeTeX and LuaTeX
need hyphenation patterns in UTF-8 format, whereas older systems require
hyphenation patterns in the 8-bit encoding of the font in use (such encodings
are codified in the LaTeX scheme with names like OT1, T2A, TS1, OML, LY1,
etc).  The present package offers a collection of conversions of existing
patterns to UTF-8 format, together with converters for use with 8-bit fonts in
older systems.

This Guix-specific package provides hyphenation patterns for all languages
supported in TeX Live.  It is a strict super-set of code{hyphen-base} package
and should be preferred to it whenever a package would otherwise depend on
@code{hyph-utf8}."))
    ("scripts"
     (shortdesc . "TeX Live infrastructure programs")
     (longdesc . "This package provides core TeX Live scripts such as updmap,
fmtutil, and tlmgr.  It is automatically installed alongside texlive-bin.")
     (docfiles "texmf-dist/doc/man/man1/fmtutil-sys.1"
               "texmf-dist/doc/man/man1/fmtutil-sys.man1.pdf"
               "texmf-dist/doc/man/man1/fmtutil-user.1"
               "texmf-dist/doc/man/man1/fmtutil-user.man1.pdf"
               "texmf-dist/doc/man/man1/fmtutil.1"
               "texmf-dist/doc/man/man1/fmtutil.man1.pdf"
               "texmf-dist/doc/man/man1/install-tl.1"
               "texmf-dist/doc/man/man1/install-tl.man1.pdf"
               "texmf-dist/doc/man/man1/mktexfmt.1"
               "texmf-dist/doc/man/man1/mktexfmt.man1.pdf"
               "texmf-dist/doc/man/man1/mktexlsr.1"
               "texmf-dist/doc/man/man1/mktexlsr.man1.pdf"
               "texmf-dist/doc/man/man1/mktexmf.1"
               "texmf-dist/doc/man/man1/mktexmf.man1.pdf"
               "texmf-dist/doc/man/man1/mktexpk.1"
               "texmf-dist/doc/man/man1/mktexpk.man1.pdf"
               "texmf-dist/doc/man/man1/mktextfm.1"
               "texmf-dist/doc/man/man1/mktextfm.man1.pdf"
               "texmf-dist/doc/man/man1/texhash.1"
               "texmf-dist/doc/man/man1/texhash.man1.pdf"
               "texmf-dist/doc/man/man1/tlmgr.1"
               "texmf-dist/doc/man/man1/tlmgr.man1.pdf"
               "texmf-dist/doc/man/man1/updmap-sys.1"
               "texmf-dist/doc/man/man1/updmap-sys.man1.pdf"
               "texmf-dist/doc/man/man1/updmap-user.1"
               "texmf-dist/doc/man/man1/updmap-user.man1.pdf"
               "texmf-dist/doc/man/man1/updmap.1"
               "texmf-dist/doc/man/man1/updmap.man1.pdf"
               "texmf-dist/doc/man/man5/fmtutil.cnf.5"
               "texmf-dist/doc/man/man5/fmtutil.cnf.man5.pdf"
               "texmf-dist/doc/man/man5/updmap.cfg.5"
               "texmf-dist/doc/man/man5/updmap.cfg.man5.pdf")
     (runfiles "texmf-dist/dvips/tetex/"
               "texmf-dist/fonts/enc/dvips/tetex/"
               "texmf-dist/fonts/map/dvips/tetex/"
               "texmf-dist/scripts/texlive/fmtutil-sys.sh"
               "texmf-dist/scripts/texlive/fmtutil-user.sh"
               "texmf-dist/scripts/texlive/fmtutil.pl"
               "texmf-dist/scripts/texlive/mktexlsr.pl"
               "texmf-dist/scripts/texlive/mktexmf"
               "texmf-dist/scripts/texlive/mktexpk"
               "texmf-dist/scripts/texlive/mktextfm"
               "texmf-dist/scripts/texlive/tlmgr.pl"
               "texmf-dist/scripts/texlive/updmap-sys.sh"
               "texmf-dist/scripts/texlive/updmap-user.sh"
               "texmf-dist/scripts/texlive/updmap.pl"
               "texmf-dist/web2c/fmtutil-hdr.cnf"
               "texmf-dist/web2c/updmap-hdr.cfg"
               "texmf-dist/web2c/updmap.cfg"
               "tlpkg/gpg/"
               "tlpkg/installer/config.guess"
               "tlpkg/installer/curl/curl-ca-bundle.crt"
               "tlpkg/TeXLive/"
               "tlpkg/texlive.tlpdb"))
    ("source"
     (shortdesc . "Source code for all TeX Live programs")
     (longdesc . "This package fetches the source for all TeX Live programs
provided by the TeX Live repository.  It is meant to be used as a source-only
package; it should not be installed in a profile.")
     (runfiles "./"))))

(define (svn-command . args)
  "Execute \"svn\" command with arguments ARGS, provided as strings, and
return its output as a string.  Raise an error if the command execution did
not succeed."
  (define subversion
    ;; Resolve this variable lazily so that (gnu packages ...) does not end up
    ;; in the closure of this module.
    (module-ref (resolve-interface '(gnu packages version-control))
                'subversion))
  (let* ((svn
          (with-store store
            (run-with-store store
              (mlet* %store-monad
                  ((drv (lower-object subversion))
                   (built (built-derivations (list drv))))
                (match (derivation->output-paths drv)
                  (((names . locations) ...)
                   (return (string-append (first locations) "/bin/svn"))))))))
         (command (string-append svn (string-join args " " 'prefix)))
         (pipe (open-input-pipe command))
         (output (read-string pipe)))
    ;; Output from these commands is memoized.  Raising an error prevent from
    ;; storing bogus values in memory.
    (unless (zero? (status:exit-val (close-pipe pipe)))
      (report-error (G_ "failed to run command: '~a'") command))
    output))

(define version->revision
  ;; Return revision, as a number, associated to string VERSION.
  (lambda (version)
    (let ((url (string-append %texlive-repository "tags/texlive-" version)))
      (string->number
       (svn-command
        "info" url "--show-item 'last-changed-revision'" "--no-newline")))))

(define (current-day)
  "Return number of days since Epoch."
  (floor (/ (time-second (current-time)) (* 24 60 60))))

(define texlive-tags
  (memoize
   (lambda* (#:key (day (current-day)))
     "Return all tags found in for the TeX Live tags in repository, from
latest to oldest.  The argument refers to current day, so memoization is only
active a single day, as the repository may have been updated between two
calls."
     (let* ((output (svn-command
                     "ls" (string-append %texlive-repository "tags") "-v"))
            (lines (string-split output #\newline)))
       ;; Each line look like "70951 karl april 15 18:11 texlive-2024.2/\n\n".
       (filter-map (lambda (l)
                     (and=> (string-match "texlive-([^/]+)/\n*$" l)
                            (cut match:substring <> 1)))
                   lines)))))

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

(define* (filter-depends depends #:optional texlive-only)
  "Filter upstream package names DEPENDS to include only their equivalent Guix
package names, without \"texlive-\" prefix.  When TEXLIVE-ONLY is true, ignore
Guix-specific packages."
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

(define (tlpdb version)
  "Return the TeX Live database associated to VERSION repository tag.  The
function fetches the requested \"texlive.tlpdb\" file and parses it as
association list."
  (let* ((fields
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
              (acons key new (alist-delete key alist)))))
         (database-url
          (string-append %texlive-repository "tags/texlive-" version
                         "/Master/tlpkg/texlive.tlpdb")))
    (call-with-input-string (svn-command "cat" database-url)
      (lambda (port)
        (let loop
            ;; Store the SVN revision of the packages database.
            ((all (list (cons 'database-revision (version->revision version))))
             (current (list))
             (last-property #false))
          (let ((line (read-line port)))
            (cond
             ;; End of file.  Don't forget to include Guix-specific package.
             ((eof-object? line) (values (append tlpdb-guix-packages all)))

             ;; End of record.
             ((string-null? line)
              (loop (cons (cons (assoc-ref current 'name) current)
                          all)
                    (list)
                    #false))
             ;; Continuation of a list
             ((and (zero? (string-index line #\space)) last-property)
              ;; Erase optional second part of list values like
              ;; "details=Readme" for files
              (let ((plain-value (first (string-split (string-trim-both line)
                                                      #\space))))
                (loop all
                      (record last-property plain-value current 'list)
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
                  (loop all current #false))))))))))

(define tlpdb/cached (memoize tlpdb))

(define latex-bin-dependency-tree
  ;; Return a list of packages used to build "latex-bin" package.  Those
  ;; cannot provide it as a native input.  Consequently, the importer sets
  ;; TEXLIVE-LATEX-BIN? argument to #F for all of them.
  (memoize
   (lambda (package-database)
     ;; Start out with "latex-bin", but also provide native inputs, which do
     ;; not appear as dependents, as roots for the search.
     (let loop ((packages
                 (list "latex-bin" "metafont" "modes" "tex"))
                (deps '()))
       (if (null? packages)
           ;; `filter-depends' will always translate "hyphen-base" into
           ;; "hyphen-complete".  Make sure plain hyphen-base appears in the
           ;; dependency tree.
           (cons "hyphen-base" (filter-depends deps))
           (loop (append-map (lambda (name)
                               (let ((data (assoc-ref package-database name)))
                                 (or (assoc-ref data 'depend)
                                     '())))
                             packages)
                 (append packages deps)))))))

(define (list-formats package-data)
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

(define (list-binfiles name package-database)
  "Return the list of \"binfiles\", i.e., files meant to be installed in
\"bin/\" directory, for package NAME according to PACKAGE-DATABASE."
  (or (and-let* ((data (assoc-ref package-database name))
                 (depend (assoc-ref data 'depend))
                 ((member (string-append name ".ARCH") depend))
                 (bin-data (assoc-ref package-database
                                      ;; Any *nix-like architecture will do.
                                      (string-append name ".x86_64-linux"))))
        (map basename (assoc-ref bin-data 'binfiles)))
      '()))

(define (list-linked-scripts name package-database)
  "Return a list of script names to symlink from \"bin/\" directory for
package NAME according to PACKAGE-DATABASE.  Consider as scripts files with
\".lua\", \".pl\", \".py\", \".rb\", \".sh\", \".sno\", \".tcl\", \".texlua\",
\".tlu\" extensions, and files without extension."
  (or (and-let* ((data (assoc-ref package-database name))
                 ;; List scripts candidates.  Bail out if there are none.
                 (runfiles (assoc-ref data 'runfiles))
                 (scripts (filter (cut string-prefix? "texmf-dist/scripts/" <>)
                                  runfiles))
                 ((pair? scripts))
                 (binfiles (list-binfiles name package-database)))
        (filter-map (lambda (script)
                      (and (any (lambda (ext)
                                  (member (basename script ext) binfiles))
                                '(".lua" ".pl" ".py" ".rb" ".sh" ".sno" ".tcl"
                                  ".texlua" ".tlu"))
                           (basename script)))
                    ;; Get the right (alphabetic) order.
                    (reverse scripts)))
      '()))

(define (list-upstream-inputs upstream-name version database)
  "Return the list of <upstream-input> corresponding to all the dependencies
of package with UPSTREAM-NAME in VERSION."
  (let ((package-data (assoc-ref database upstream-name))
        (scripts (list-linked-scripts upstream-name database)))
    (append
     ;; Native inputs.
     ;;
     ;; Texlive build system generates font metrics whenever a font metrics
     ;; file has the same base name as a Metafont file.  In this case, provide
     ;; TEXLIVE-METAFONT.
     (or (and-let* ((runfiles (assoc-ref package-data 'runfiles))
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
           (list (upstream-input
                  (name "metafont")
                  (downstream-name "texlive-metafont")
                  (type 'native))))
         '())
     ;; Regular inputs.
     ;;
     ;; Those may be required by scripts associated to the package.
     (match (append-map (lambda (s)
                          (cond ((string-suffix? ".pl" s) '("perl"))
                                ((string-suffix? ".py" s) '("python"))
                                ((string-suffix? ".rb" s) '("ruby"))
                                ((string-suffix? ".tcl" s) '("tcl" "tk"))
                                (else '())))
                        scripts)
       (() '())
       (inputs (map (lambda (input-name)
                      (upstream-input
                       (name input-name)
                       (downstream-name input-name)
                       (type 'regular)))
                    (delete-duplicates inputs string=))))
     ;; Propagated inputs.
     ;;
     ;; Return the "depend" references given in the TeX Live database.  Also
     ;; check if the package has associated binaries built from
     ;; TEXLIVE-SOURCE.  In that case, add a Guix-specific NAME-bin propagated
     ;; input.
     (let ((binfiles (list-binfiles upstream-name database)))
       (map (lambda (input-name)
              (upstream-input
               (name input-name)
               (downstream-name (downstream-package-name "texlive-"
                                                         input-name))
               (type 'propagated)))
            (sort (append
                   (filter-depends (or (assoc-ref package-data 'depend) '()))
                   ;; Check if propagation of binaries is necessary.  It
                   ;; happens when binfiles outnumber the scripts, if any.
                   (if (and (> (length binfiles) (length scripts))
                            (not (member upstream-name
                                         no-bin-propagation-packages)))
                       ;; LIBKPATHSEA contains the executables for KPATHSEA.
                       ;; There is no KPATHSEA-BIN.
                       (list (if (equal? upstream-name "kpathsea")
                                 "libkpathsea"
                                 (string-append upstream-name "-bin")))
                       '()))
                  string<?))))))

(define (upstream-inputs->texlive-inputs upstream-inputs type)
  (map (compose string->symbol upstream-input-downstream-name)
       (filter (upstream-input-type-predicate type)
               upstream-inputs)))

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

(define (texlive->svn-multi-reference upstream-name version database)
  "Return <svn-multi-reference> object for TeX Live package with UPSTREAM-NAME
at VERSION."
  (let* ((data (assoc-ref database upstream-name))
         (files (append (or (assoc-ref data 'docfiles) (list))
                        (or (assoc-ref data 'runfiles) (list))
                        (or (assoc-ref data 'srcfiles) (list))))
         (locations
          ;; Drop "texmf-dist/" prefix from files.  Special case
          ;; TEXLIVE-SCRIPTS and TEXLIVE-SOURCE, where files are not always
          ;; exported from "texmf-dist/".
          (if (member upstream-name '("scripts" "source"))
              files
              (files->locations
               ;; Ignore any file not starting with the expected prefix, such
               ;; as tlpkg/tlpostcode/...  Nothing good can come from this.
               (filter-map
                (lambda (file)
                  (and (string-prefix? "texmf-dist/" file)
                       (string-drop file (string-length "texmf-dist/"))))
                files)))))
    (svn-multi-reference
     (url (match upstream-name
            ("scripts"
             (string-append
              %texlive-repository "tags/texlive-" version "/Master"))
            ("source"
             (string-append %texlive-repository
                            "tags/texlive-" version "/Build/source"))
            (_
             (texlive-packages-repository version))))
     (locations (sort locations string<))
     (revision (assoc-ref database 'database-revision)))))

(define (tlpdb->package upstream-name version database)
  (and-let* ((data (assoc-ref database upstream-name))
             (name (downstream-package-name "texlive-" upstream-name))
             (reference
              (texlive->svn-multi-reference upstream-name version database))
             (source (with-store store
                       (download-multi-svn-to-store
                        store reference
                        (format #f "~a-~a-svn-multi-checkout" name version)))))
    (let* ((scripts (list-linked-scripts upstream-name database))
           (upstream-inputs
            (list-upstream-inputs upstream-name version database))
           (tex-formats (list-formats data))
           (meta-package? (null? (svn-multi-reference-locations reference)))
           (empty-package? (and meta-package? (not (pair? tex-formats)))))
      (values
       `(package
          (name ,name)
          (version ,(if empty-package?
                        '(package-version texlive-source)
                        version))
          (source
           ,(and (not meta-package?)
                 `(origin
                    (method svn-multi-fetch)
                    (uri (svn-multi-reference
                          (url
                           ,(match upstream-name
                              ("scripts"
                               '(string-append
                                 %texlive-repository "tags/texlive-" version
                                 "/Master"))
                              ("source"
                               '(string-append
                                 %texlive-repository "tags/texlive-" version
                                 "/Build/source"))
                              (_
                               '(texlive-packages-repository version))))
                          (revision ,(svn-multi-reference-revision reference))
                          (locations
                           (list ,@(svn-multi-reference-locations reference)))))
                    (file-name (git-file-name name version))
                    (sha256
                     (base32
                      ,(bytevector->nix-base32-string
                        (let-values (((port get-hash) (open-sha256-port)))
                          (write-file source port)
                          (force-output port)
                          (get-hash))))))))
          ,@(if (assoc-ref data 'docfiles)
                '((outputs '("out" "doc")))
                '())
          ,@(if (string= upstream-name
                         (string-drop name (string-length "texlive-")))
                '()
                `((properties '((upstream-name . ,upstream-name)))))
          ;; Build system.
          ;;
          ;; Use trivial build system only when the package contains no files,
          ;; and no TeX format file is expected to be built.
          (build-system ,(if empty-package?
                             'trivial-build-system
                             'texlive-build-system))
          ;; Arguments.
          ,@(let* ((latex-bin-dependency?
                    (member upstream-name (latex-bin-dependency-tree database)))
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
          ;; Inputs.
          ,@(match (upstream-inputs->texlive-inputs upstream-inputs 'native)
              (() '())
              (inputs `((native-inputs (list ,@inputs)))))
          ,@(match (upstream-inputs->texlive-inputs upstream-inputs 'regular)
              (() '())
              (inputs `((inputs (list ,@inputs)))))
          ,@(match (upstream-inputs->texlive-inputs upstream-inputs 'propagated)
              (() '())
              (inputs `((propagated-inputs (list ,@inputs)))))
          ;; Home page, synopsis, description and license.
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
       ;; List of pure TeX Live dependencies for recursive calls.
       (filter-depends (or (assoc-ref data 'depend) '()) #t)))))

(define texlive->guix-package
  (lambda* (name #:key version database #:allow-other-keys)
    "Find the metadata for NAME in the TeX Live database and return the
associated Guix package, or #f on failure.  Fetch metadata for a specific
version whenever VERSION keyword is specified.  Otherwise, grab package latest
release.  When DATABASE is provided, fetch metadata from there, ignoring
VERSION."
    (let ((version (or version (first (texlive-tags)))))
      (tlpdb->package name version (or database (tlpdb/cached version))))))

(define* (texlive-recursive-import name #:key repo version)
  (recursive-import name
                    #:repo repo
                    #:version version
                    #:repo->guix-package texlive->guix-package
                    #:guix-name downstream-package-name))

;;;
;;; Updates.
;;;

(define (package-from-texlive-repository? package)
  (let ((name (package-name package)))
    ;; TEXLIVE-SCRIPTS and TEXLIVE-SOURCE do not use TEXLIVE-BUILD-SYSTEM, but
    ;; package's structure is sufficiently regular to benefit from
    ;; auto-updates.
    (or (member name '("texlive-scripts" "texlive-source"))
        (and (string-prefix? "texlive-" (package-name package))
             (eq? 'texlive
                  (build-system-name (package-build-system package)))))))

(define* (latest-release package #:key version partial-version?)
  "Return an <upstream-source> for the latest release of PACKAGE.  Optionally
include a VERSION string to fetch a specific version, which may be a partial
prefix when PARTIAL-VERSION? is #t."
  (let* ((version (find-version (texlive-tags) version partial-version?))
         (database (tlpdb/cached version))
         (upstream-name (package-upstream-name* package)))
    (and version (assoc-ref database upstream-name)
         (upstream-source
          (package upstream-name)
          (version version)
          (urls (texlive->svn-multi-reference upstream-name version database))
          (inputs (list-upstream-inputs upstream-name version database))))))

(define %texlive-updater
  ;; The TeX Live updater.  It is restricted to TeX Live releases (2023.0,
  ;; 2024.2, ...); it doesn't include revision bumps for individual packages.
  (upstream-updater
   (name 'texlive)
   (description "Updater for TeX Live packages")
   (pred package-from-texlive-repository?)
   (import latest-release)))

;;; texlive.scm ends here
