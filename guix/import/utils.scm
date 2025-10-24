;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2018, 2019, 2020, 2023 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016 Jelle Licht <jlicht@fsfe.org>
;;; Copyright © 2016 David Craven <david@craven.ch>
;;; Copyright © 2017, 2019, 2020, 2022, 2023, 2024, 2025 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2018 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2019 Robert Vollmert <rob@vllmrt.net>
;;; Copyright © 2020 Helio Machado <0x2b3bfa0+guix@googlemail.com>
;;; Copyright © 2020 Martin Becze <mjbecze@riseup.net>
;;; Copyright © 2021, 2024 Maxim Cournoyer <maxim@guixotic.coop>
;;; Copyright © 2021 Sarah Morgensen <iskarian@mgsn.dev>
;;; Copyright © 2021 Xinglu Chen <public@yoctocell.xyz>
;;; Copyright © 2022 Alice Brenon <alice.brenon@ens-lyon.fr>
;;; Copyright © 2022 Kyle Meyer <kyle@kyleam.com>
;;; Copyright © 2022 Philip McGrath <philip@philipmcgrath.com>
;;; Copyright © 2023, 2025 Nicolas Graves <ngraves@ngraves.fr>
;;; Copyright © 2025 Cayetano Santos <csantosb@inventati.org>
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

(define-module (guix import utils)
  #:autoload   (git structs) (git-error-message)
  #:use-module (guix base32)
  #:use-module ((guix build download) #:prefix build:)
  #:use-module ((gcrypt hash) #:hide (sha256))
  #:use-module (guix http-client)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix deprecation)
  #:use-module (guix diagnostics)
  #:use-module (guix discovery)
  #:use-module (guix build-system)
  #:use-module (guix git)
  #:use-module (guix hash)
  #:use-module ((guix i18n) #:select (G_))
  #:use-module (guix store)
  #:use-module (guix download)
  #:use-module (guix sets)
  #:use-module ((guix ui) #:select (fill-paragraph))
  #:use-module (gnu packages)
  #:autoload   (ice-9 control) (let/ec)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-71)
  #:export (factorize-uri

            flatten
            false-if-networking-error

            url-fetch
            guix-hash-url

            peekable-lambda
            peek-body

            download-git-repository
            git-origin
            git->origin
            default-git-error

            package-names->package-inputs
            maybe-inputs
            maybe-native-inputs
            maybe-propagated-inputs
            package->definition

            spdx-string->license
            license->symbol

            snake-case
            beautify-description
            beautify-synopsis

            alist->package

            read-lines
            chunk-lines

            guix-name

            find-version

            recursive-import))

(define (factorize-uri uri version)
  "Factorize URI, a package tarball URI as a string, such that any occurrences
of the string VERSION is replaced by the symbol 'version."
  (let ((version-rx (make-regexp (regexp-quote version))))
    (match (regexp-exec version-rx uri)
      (#f
       uri)
      (_
       (let ((indices (fold-matches version-rx uri
                                    '((0))
                                    (lambda (m result)
                                      (match result
                                        (((start) rest ...)
                                         `((,(match:end m))
                                           (,start . ,(match:start m))
                                           ,@rest)))))))
         (fold (lambda (index result)
                 (match index
                   ((start)
                    (cons (substring uri start)
                          result))
                   ((start . end)
                    (cons* (substring uri start end)
                           'version
                           result))))
               '()
               indices))))))

(define (flatten lst)
  "Return a list that recursively concatenates all sub-lists of LST."
  (fold-right
   (match-lambda*
    (((sub-list ...) memo)
     (append (flatten sub-list) memo))
    ((elem memo)
     (cons elem memo)))
   '() lst))

(define (call-with-networking-exception-handler thunk)
  "Invoke THUNK, returning #f if one of the usual networking exception is
thrown."
  (let/ec return
    (with-exception-handler
        (lambda (exception)
          (cond ((http-get-error? exception)
                 (return #f))
                (((exception-predicate &exception-with-kind-and-args) exception)
                 ;; Return false and move on upon connection failures and bogus
                 ;; HTTP servers.
                 (if (memq (exception-kind exception)
                           '(gnutls-error tls-certificate-error
                                          system-error getaddrinfo-error
                                          bad-header bad-header-component))
                     (return #f)
                     (raise-exception exception)))
                (else
                 (raise-exception exception))))
      thunk

      ;; Do not unwind to preserve meaningful backtraces.
      #:unwind? #f)))

(define-syntax-rule (false-if-networking-error exp)
  "Evaluate EXP, returning #f if a networking-related exception is thrown."
  (call-with-networking-exception-handler (lambda () exp)))

(define (url-fetch url file-name)
  "Save the contents of URL to FILE-NAME.  Return #f on failure."
  (parameterize ((current-output-port (current-error-port)))
    (build:url-fetch url file-name)))

(define (guix-hash-url filename)
  "Return the hash of FILENAME in nix-base32 format."
  (bytevector->nix-base32-string (file-sha256 filename)))

(define-syntax-rule (peekable-lambda args this-body)
  (lambda args
    #((body . this-body))
    this-body))

(define (peek-body proc)
  (procedure-property proc 'body))

(define (download-git-repository url ref)
  "Fetch the given REF from the Git repository at URL.  Return three values :
the commit hash, the downloaded directory and its content hash."
  (with-store store
    (let (((values checkout commit-hash)
           (latest-repository-commit store url #:ref ref)))
      (values commit-hash
              checkout
              (bytevector->nix-base32-string
               (query-path-hash store checkout))))))

(define (git-origin url commit hash)
  "Simple helper to generate a Git origin s-expression."
  `(origin
     (method git-fetch)
     (uri (git-reference
            (url ,(and (not (eq? url 'null)) url))
            (commit ,commit)))
     (file-name (git-file-name name version))
     (sha256
      (base32 ,hash))))

(define* (git->origin url proc #:key ref #:rest rest)
  "Return a generated `origin' block of a package depending on the Git version
control system, and the directory in the store where the package has been
downloaded, in case further processing is necessary.

Unless overwritten with REF, the ref (as defined by the (guix git) module)
is calculated from the evaluation of PROC with trailing arguments.  PROC must
be a procedure with a 'body property, used to generate the origin sexp."
  (let* ((args (strip-keyword-arguments '(#:ref) rest))
         (commit (apply proc args))
         (ref (or ref (and commit `(tag-or-commit . ,commit))))
         (_ directory hash
            (if (or ref commit)
                (download-git-repository url ref)
                (values #f #f #f))))
    (values (git-origin url (peek-body proc) hash) directory)))

(define* (default-git-error home-page #:optional location)
  "Return a procedure to be passed to a `git-error' `catch' for HOME-PAGE at
LOCATION."
  (match-lambda*
    (('git-error error)
     (warning location
              (G_ "failed to download Git repository ~a: ~a~%")
              home-page
              (git-error-message error))
     #f)
    (_
     #f)))

(define %spdx-license-identifiers
  ;; https://spdx.org/licenses/
  ;; The gfl1.0, nmap, repoze
  ;; licenses doesn't have SPDX identifiers
  ;;
  ;; Please update guix/licenses.scm when modifying
  ;; this list to avoid mismatches.
  ;;
  ;; "GPL-N+" has been deprecated in favour of "GPL-N-or-later".  "GPL-N" has
  ;; been deprecated in favour of "GPL-N-only" or "GPL-N-or-later" as
  ;; appropriate.  Likewise for LGPL and AGPL.  However, we list the
  ;; deprecated forms here (with and without the "+" operator) to get better
  ;; results from old license expressions.
  '(("AGPL-1.0"                   . license:agpl1)
    ("AGPL-1.0-only"              . license:agpl1)
    ("AGPL-3.0"                   . license:agpl3)
    ("AGPL-3.0-only"              . license:agpl3)
    ("AGPL-3.0-or-later"          . license:agpl3+)
    ("Arphic-1999"                . license:arphic-1999)
    ("Apache-1.1"                 . license:asl1.1)
    ("Apache-2.0"                 . license:asl2.0)
    ("APSL-2.0"                   . license:apsl2)
    ("BlueOak-1.0.0"              . license:blue-oak1.0.0)
    ("BSL-1.0"                    . license:boost1.0)
    ("0BSD"                       . license:bsd-0)
    ("BSD-2-Clause"               . license:bsd-2)
    ("BSD-2-Clause-FreeBSD"       . license:bsd-2)     ;flagged as deprecated on spdx
    ("BSD-3-Clause"               . license:bsd-3)
    ("BSD-4-Clause"               . license:bsd-4)
    ("CC0-1.0"                    . license:cc0)
    ("CC-BY-2.0"                  . license:cc-by2.0)
    ("CC-BY-3.0"                  . license:cc-by3.0)
    ("CC-BY-4.0"                  . license:cc-by4.0)
    ("CC-BY-SA-2.0"               . license:cc-by-sa2.0)
    ("CC-BY-SA-3.0"               . license:cc-by-sa3.0)
    ("CC-BY-SA-4.0"               . license:cc-by-sa4.0)
    ("CDDL-1.0"                   . license:cddl1.0)
    ("CDDL-1.1"                   . license:cddl1.1)
    ("CECILL-2.1"                 . license:cecill)
    ("CECILL-B"                   . license:cecill-b)
    ("CECILL-C"                   . license:cecill-c)
    ("Artistic-2.0"               . license:artistic2.0)
    ("ClArtistic"                 . license:clarified-artistic)
    ("copyleft-next-0.3.0"        . license:copyleft-next)
    ("CPL-1.0"                    . license:cpl1.0)
    ("EPL-1.0"                    . license:epl1.0)
    ("EPL-2.0"                    . license:epl2.0)
    ("EUPL-1.1"                   . license:eupl1.1)
    ("EUPL-1.2"                   . license:eupl1.2)
    ("MIT"                        . license:expat)
    ("MIT-0"                      . license:expat-0)
    ("FTL"                        . license:freetype)
    ("FreeBSD-DOC"                . license:freebsd-doc)
    ("Freetype"                   . license:freetype)
    ("FSFAP"                      . license:fsf-free)
    ("FSFUL"                      . license:fsf-free)
    ("GFDL-1.1"                   . license:fdl1.1+)
    ("GFDL-1.1-or-later"          . license:fdl1.1+)
    ("GFDL-1.2"                   . license:fdl1.2+)
    ("GFDL-1.2-or-later"          . license:fdl1.2+)
    ("GFDL-1.3"                   . license:fdl1.3+)
    ("GFDL-1.3-or-later"          . license:fdl1.3+)
    ("Giftware"                   . license:giftware)
    ("GPL-1.0"                    . license:gpl1)
    ("GPL-1.0-only"               . license:gpl1)
    ("GPL-1.0+"                   . license:gpl1+)
    ("GPL-1.0-or-later"           . license:gpl1+)
    ("GPL-2.0"                    . license:gpl2)
    ("GPL-2.0-only"               . license:gpl2)
    ("GPL-2.0+"                   . license:gpl2+)
    ("GPL-2.0-or-later"           . license:gpl2+)
    ("GPL-3.0"                    . license:gpl3)
    ("GPL-3.0-only"               . license:gpl3)
    ("GPL-3.0+"                   . license:gpl3+)
    ("GPL-3.0-or-later"           . license:gpl3+)
    ("HPND"                       . license:hpnd)
    ("ISC"                        . license:isc)
    ("IJG"                        . license:ijg)
    ("Imlib2"                     . license:imlib2)
    ("IPA"                        . license:ipa)
    ("IPL-1.0"                    . license:ibmpl1.0)
    ("LAL-1.3"                    . license:lal1.3)
    ("LGPL-2.0"                   . license:lgpl2.0)
    ("LGPL-2.0-only"              . license:lgpl2.0)
    ("LGPL-2.0+"                  . license:lgpl2.0+)
    ("LGPL-2.0-or-later"          . license:lgpl2.0+)
    ("LGPL-2.1"                   . license:lgpl2.1)
    ("LGPL-2.1-only"              . license:lgpl2.1)
    ("LGPL-2.1+"                  . license:lgpl2.1+)
    ("LGPL-2.1-or-later"          . license:lgpl2.1+)
    ("LGPL-3.0"                   . license:lgpl3)
    ("LGPL-3.0-only"              . license:lgpl3)
    ("LGPL-3.0+"                  . license:lgpl3+)
    ("LGPL-3.0-or-later"          . license:lgpl3+)
    ("LPL-1.02"                   . license:lpl1.02)
    ("LPPL-1.0"                   . license:lppl)
    ("LPPL-1.1"                   . license:lppl)
    ("LPPL-1.2"                   . license:lppl1.2)
    ("LPPL-1.3a"                  . license:lppl1.3a)
    ("LPPL-1.3c"                  . license:lppl1.3c)
    ("MirOS"                      . license:miros)
    ("MPL-1.0"                    . license:mpl1.0)
    ("MPL-1.1"                    . license:mpl1.1)
    ("MPL-2.0"                    . license:mpl2.0)
    ("MS-PL"                      . license:ms-pl)
    ("NCSA"                       . license:ncsa)
    ("OGL-UK-1.0"                 . license:ogl-psi1.0)
    ("CERN-OHL-S-2.0"             . license:ohl2-s)
    ("CERN-OHL-P-2.0"             . license:ohl2-p)
    ("CERN-OHL-W-2.0"             . license:ohl2-w)
    ("OpenSSL"                    . license:openssl)
    ("OLDAP-2.8"                  . license:openldap2.8)
    ("OPL-1.0"                    . license:opl1.0+)
    ("CUA-OPL-1.0"                . license:cua-opl1.0)
    ("PSF-2.0"                    . license:psfl)
    ("OSL-2.1"                    . license:osl2.1)
    ("QPL-1.0"                    . license:qpl)
    ("Ruby"                       . license:ruby)
    ("SGI-B-2.0"                  . license:sgifreeb2.0)
    ("OFL-1.1"                    . license:silofl1.1)
    ("Sleepycat"                  . license:sleepycat)
    ("TCL"                        . license:tcl/tk)
    ("Unlicense"                  . license:unlicense)
    ("Vim"                        . license:vim)
    ("W3C"                        . license:w3c)
    ("WTFPL"                      . license:wtfpl2)
    ("wxWindow"                   . license:wxwindows3.1+)         ;flagged as deprecated on spdx
    ("X11"                        . license:x11)
    ("ZPL-2.1"                    . license:zpl2.1)
    ("Zlib"                       . license:zlib)))

(define (spdx-string->license str)
  "Convert STR, an SPDX license identifier (possibly with a postfix +
operator), to a symbol like 'license:gpl3+ giving the prefixed name of a
license object exported from (guix licenses).  Return #f if STR does not match
any known SPDX license identifiers.  Per the SPDX specification, license
identifiers are compared case-insensitively."
  ;; https://spdx.github.io/spdx-spec/v2.3/SPDX-license-expressions/#d2-case-sensitivity
  ;; Operators AND, OR, and WITH are case-sensitive, but identifiers are
  ;; case-insensitive for matching, though the canonical case is used in URIs.
  (match (assoc str %spdx-license-identifiers string-ci=?)
    ((_ . license)
     license)
    (#f
     (and (string-suffix? "+" str)
          ;; We try the form with the + to support deprecated identifiers for
          ;; GNU licenses (see above).  Here, we handle other uses of +.
          (spdx-string->license (string-drop-right str 1))))))

(define (license->symbol license)
  "Convert LICENSE object to a prefixed symbol representing the variable the
object is bound to in the (guix licenses) module, such as 'license:gpl3+, or
#f if there is no such known license."
  (define licenses
    (module-map (lambda (sym var) `(,(variable-ref var) . ,sym))
                (resolve-interface '(guix licenses) #:prefix 'license:)))
  (assoc-ref licenses license))

(define (snake-case str)
  "Return a downcased version of the string STR where underscores and periods
are replaced with dashes."
  (string-map (match-lambda
                ((or #\_ #\.) #\-)
                (chr chr))
              (string-downcase str)))

(define* (beautify-description description #:optional (length 80))
  "Improve the package DESCRIPTION by turning a beginning sentence fragment into
a proper sentence and by using two spaces between sentences, and wrap lines at
LENGTH characters."
  (if (or (not (string? description)) (string=? (string-trim-both description) ""))
      (G_ "This package lacks a description.  Run \
\"info '(guix) Synopses and Descriptions'\" for more information.")

  (let* ((fix-word
          (lambda (word)
            (fold (lambda (proc acc) (proc acc)) word
                  (list
                   ;; Remove wrapping in single quotes, common in R packages.
                   (cut string-trim-both <> #\')
                   ;; Escape single @ to prevent it from being understood as
                   ;; invalid Texinfo syntax.
                   (cut regexp-substitute/global #f "@" <> 'pre "@@" 'post)
                   ;; Wrap camelCase, PascalCase words, text followed
                   ;; immediately by "()", or text starting with "@@" in @code{...}.
                   (lambda (word)
                     (let ((pattern
                            (make-regexp
                             "((@@)?[A-Z][a-z]+[A-Z]|(@@)?[a-z]+[A-Z]|(@@)?.+\\(\\))")))
                       (match (list-matches pattern word)
                         (() word)
                         ((m . rest)
                          ;; Do not include leading or trailing punctuation,
                          ;; unless its "()".
                          (let* ((last-text (if (string-suffix? "()" (match:substring m 1))
                                                (string-length (match:substring m 1))
                                                (or (and=> (string-skip-right word char-set:punctuation) 1+)
                                                    (string-length word))))
                                 (inner (substring word (match:start m) last-text))
                                 (pre (string-take word (match:start m)))
                                 (post (substring word last-text (string-length word))))
                            (string-append pre "@code{" inner "}" post))))))))))
         (words
          (string-tokenize (string-trim-both description)
                           (char-set-complement
                            (char-set #\space #\newline))))
         (new-words
          (match words
            (((and (or "A" "Classes" "Functions" "Methods" "Tools")
                   first) . rest)
             (cons* "This" "package" "provides"
                    (string-downcase first) rest))
            (((and (or "Contains"
                       "Creates"
                       "Performs"
                       "Provides"
                       "Produces"
                       "Implements"
                       "Infers") first) . rest)
             (cons* "This" "package"
                    (string-downcase first) rest))
            (_ words)))
         (new-words
           (match new-words
             ((rest ... last)
              (reverse (cons (if (or (string-suffix? "." last)
                                     (string-suffix? "!" last)
                                     (string-suffix? "?" last))
                               last
                               (string-append last "."))
                             (reverse rest))))
             (() new-words))) ;; No description in package
         (cleaned
          (string-join (map fix-word new-words))))
    ;; Use double spacing between sentences
    (fill-paragraph (regexp-substitute/global #f "\\. \\b"
                                              cleaned 'pre
                                              (lambda (m)
                                                (let ((pre (match:prefix m))
                                                      (abbrevs '("Dr" "Mr" "Mrs"
                                                                 "Ms" "Prof" "vs"
                                                                 "e.g")))
                                                  (if (and (> (string-length pre) 0)
                                                           (or (any (cut string-suffix? <> pre) abbrevs)
                                                               (char-upper-case?
                                                                (string-ref pre (1- (string-length pre))))))
                                                      ". "
                                                      ".  ")))
                                              'post)
                    length))))

(define (beautify-synopsis synopsis)
  "Improve the package SYNOPSIS."
  (let ((cleaned (cond
                  ((not (string? synopsis))
                   (G_ "This package lacks a synopsis.  Run \
\"info '(guix) Synopses and Descriptions'\" for more information."))
                  ((string-prefix? "A " synopsis)
                   (substring synopsis 1))
                  ;; Remove trailing period.
                  ((string-suffix? "." synopsis)
                   (substring synopsis 0
                              (1- (string-length synopsis))))
                  (else synopsis))))
    ;; Escape single @ to prevent it from being understood as invalid Texinfo
    ;; syntax.
    (regexp-substitute/global #f "@"
                              (string-trim-both cleaned)
                              'pre "@@" 'post)))

(define* (package-names->package-inputs names #:optional (output #f))
  "Given a list of PACKAGE-NAMES or (PACKAGE-NAME VERSION) pairs, and an
optional OUTPUT, tries to generate a quoted list of inputs, as suitable to
use in an 'inputs' field of a package definition."
  (define (make-input input version)
    (let ((name (if version (string-append input "-" version) input)))
      (if output
          (list (string->symbol name) output)
          (string->symbol name))))

  (map (match-lambda
         ((input version) (make-input input version))
         (input (make-input input #f)))
       names))

(define* (maybe-inputs package-names #:optional (output #f)
                       #:key (type #f))
  "Given a list of PACKAGE-NAMES, tries to generate the 'inputs' field of a
package definition.  TYPE can be used to specify the type of the inputs;
either the 'native or 'propagated symbols are accepted.  Left unspecified, the
snippet generated is for regular inputs."
  (let ((field-name (match type
                      ('native 'native-inputs)
                      ('propagated 'propagated-inputs)
                      (_ 'inputs))))
    (match (package-names->package-inputs package-names output)
      (()
       '())
      ((package-inputs ...)
       `((,field-name (list ,@package-inputs)))))))

(define* (maybe-native-inputs package-names #:optional (output #f))
  "Same as MAYBE-INPUTS, but for native inputs."
  (maybe-inputs package-names output #:type 'native))

(define* (maybe-propagated-inputs package-names #:optional (output #f))
  "Same as MAYBE-INPUTS, but for propagated inputs."
  (maybe-inputs package-names output #:type 'propagated))

(define* (package->definition guix-package #:optional append-version?/string)
  "If APPEND-VERSION?/STRING is #t, append the package's major+minor version.
If it is the symbol 'full, append the package's complete version.  If
APPEND-VERSION?/string is a string, append this string."
  (match guix-package
    ((or
      ('package ('name name) ('version version) . rest)
      ('let _ ('package ('name name) ('version version) . rest)))
     `(define-public ,(string->symbol
                       (cond
                        ((string? append-version?/string)
                         (string-append name "-" append-version?/string))
                        ((eq? append-version?/string #t)
                         (string-append name "-" (version-major+minor version)))
                        ((eq? 'full append-version?/string)
                         (string-append name "-" version))
                        (else name)))
        ,guix-package))))

(define (build-system-modules)
  (all-modules (map (lambda (entry)
                      `(,entry . "guix/build-system"))
                    %load-path)))

(define (lookup-build-system-by-name name)
  "Return a <build-system> value for the symbol NAME, representing the name of
the build system."
  (fold-module-public-variables (lambda (obj result)
                                  (if (and (build-system? obj)
                                           (eq? name (build-system-name obj)))
                                      obj result))
                                #f
                                (build-system-modules)))

(define (specs->package-lists specs)
  "Convert each string in the SPECS list to a list of a package label and a
package value."
  (map (lambda (spec)
         (let-values (((pkg out) (specification->package+output spec)))
           (match out
             ("out" (list (package-name pkg) pkg))
             (_ (list (package-name pkg) pkg out)))))
       specs))

(define (source-spec->object source)
  "Generate an <origin> object from a SOURCE specification.  The SOURCE can
either be a simple URL string, #F, or an alist containing entries for each of
the expected fields of an <origin> object."
  (match source
    ((? string? source-url)
     (let ((tarball (with-store store (download-to-store store source-url))))
       (origin
         (method url-fetch)
         (uri source-url)
         (sha256 (base32 (guix-hash-url tarball))))))
    (#f #f)
    (orig (let ((sha (match (assoc-ref orig "sha256")
                       ((("base32" . value))
                        (base32 value))
                       (_ #f))))
            (origin
              (method (match (assoc-ref orig "method")
                        ("url-fetch" (@ (guix download) url-fetch))
                        ("git-fetch" (@ (guix git-download) git-fetch))
                        ("svn-fetch" (@ (guix svn-download) svn-fetch))
                        ("hg-fetch"  (@ (guix hg-download) hg-fetch))
                        (_ #f)))
              (uri (assoc-ref orig "uri"))
              (sha256 sha))))))

(define* (alist->package meta #:optional (known-inputs '()))
  "Return a package value generated from the alist META.  If the list of
strings KNOWN-INPUTS is provided, do not treat the mentioned inputs as
specifications to look up and replace them with plain symbols instead."
  (define (process-inputs which)
    (let-values (((regular known)
                  (lset-diff+intersection
                   string=?
                   (vector->list (or (assoc-ref meta which) #()))
                   known-inputs)))
      (append (specs->package-lists regular)
              (map string->symbol known))))
  (define (process-arguments arguments)
    (append-map (match-lambda
                  ((key . value)
                   (list (symbol->keyword (string->symbol key)) value)))
                arguments))
  (define (process-properties properties)
    (map (match-lambda
           ((key . value)
            (cons (string->symbol key) value)))
         properties))

  (package
    (name (assoc-ref meta "name"))
    (version (assoc-ref meta "version"))
    (source (source-spec->object (assoc-ref meta "source")))
    (properties
     (or (and=> (assoc-ref meta "properties")
                process-properties)
         '()))
    (build-system
      (lookup-build-system-by-name
       (string->symbol (assoc-ref meta "build-system"))))
    (arguments
     (or (and=> (assoc-ref meta "arguments")
                process-arguments)
         '()))
    (native-inputs (process-inputs "native-inputs"))
    (inputs (process-inputs "inputs"))
    (propagated-inputs (process-inputs "propagated-inputs"))
    (home-page
     (assoc-ref meta "home-page"))
    (synopsis
     (assoc-ref meta "synopsis"))
    (description
     (assoc-ref meta "description"))
    (license
     (match (assoc-ref meta "license")
       (#f #f)
       (l
        (or (false-if-exception
             (module-ref (resolve-interface '(guix licenses))
                         (string->symbol l)))
            (false-if-exception
             (module-ref (resolve-interface '(guix licenses) #:prefix 'license:)
                         (spdx-string->license l)))
            (license:fsdg-compatible l)))))))

(define* (read-lines #:optional (port (current-input-port)))
  "Read lines from PORT and return them as a list."
  (let loop ((line (read-line port))
             (lines '()))
    (if (eof-object? line)
        (reverse lines)
        (loop (read-line port)
              (cons line lines)))))

(define* (chunk-lines lines #:optional (pred string-null?))
  "Return a list of chunks, each of which is a list of lines.  The chunks are
separated by PRED."
  (let loop ((rest lines)
             (parts '()))
    (receive (before after)
        (break pred rest)
      (let ((res (cons before parts)))
        (if (null? after)
            (reverse res)
            (loop (cdr after) res))))))

(define-deprecated/alias guix-name downstream-package-name)

(define* (find-version versions #:optional version partial?)
  "Find VERSION amongst VERSIONS.  When VERSION is not provided, return the
latest version.  When PARTIAL? is #t, VERSION is treated as a version prefix;
e.g. finding version \"0.1\" may return \"0.1.8\" if it is the newest \"0.1\"
prefixed version found in VERSIONS.  Return #f when VERSION could not be
found."
  (let ((versions (sort versions version>?)))
    (cond
     ((and version partial?)            ;partial version
      (find (cut version-prefix? version <>) versions))
     ((and version (not partial?))      ;exact version
      (find (cut string=? version <>) versions))
     ((not (null? versions))            ;latest version
      (first versions))
     (else #f))))                       ;should not happen

(define (topological-sort nodes
                          node-dependencies
                          node-name)
  "Perform a breadth-first traversal of the graph rooted at NODES, a list of
nodes, and return the list of nodes sorted in topological order.  Call
NODE-DEPENDENCIES to obtain the dependencies of a node, and NODE-NAME to
obtain a node's uniquely identifying \"key\"."
  (let loop ((nodes nodes)
             (result '())
             (visited (set)))
    (match nodes
      (()
       result)
      ((head . tail)
       (if (set-contains? visited (node-name head))
           (loop tail result visited)
           (let ((dependencies (node-dependencies head)))
             (loop (append dependencies tail)
                   (cons head result)
                   (set-insert (node-name head) visited))))))))

(define* (recursive-import package-name
                           #:key repo->guix-package guix-name version
                           #:allow-other-keys #:rest rest)
  "Return a list of package expressions for PACKAGE-NAME and all its
dependencies, sorted in topological order.  For each package,
call (REPO->GUIX-PACKAGE NAME #:version V), which should return a
package expression and a list of dependencies; call (GUIX-NAME PACKAGE-NAME)
to obtain the Guix package name corresponding to the upstream name."
  (define-record-type <node>
    (make-node name version package dependencies)
    node?
    (name         node-name)
    (version      node-version)
    (package      node-package)
    (dependencies node-dependencies))

  (define (exists? name version)
    (not (null? (find-packages-by-name (guix-name name) version))))

  (define (lookup-node name version)
    (let* ((pre post (break (cut eq? #:version <>) rest))
           (post* (match post
                    ((#:version v . more) more)
                    (_ post)))
           (args (append pre (list #:version version) post*))
           (package dependencies (apply repo->guix-package name args))
           (normalized-deps (map (match-lambda
                                   ((name version) (list name version))
                                   (name (list name #f))) dependencies)))
      (make-node name version package normalized-deps)))

  (filter-map
   node-package
   (topological-sort (list (lookup-node package-name version))
                     (lambda (node)
                       (map (lambda (name-version)
                              (apply lookup-node name-version))
                            (remove (lambda (name-version)
                                      (apply exists? name-version))
                                    (node-dependencies node))))
                     (lambda (node)
                       (string-append
                        (node-name node)
                        (or (node-version node) ""))))))
