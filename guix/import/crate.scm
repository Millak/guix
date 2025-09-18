;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 David Craven <david@craven.ch>
;;; Copyright © 2019, 2020, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2019, 2020 Martin Becze <mjbecze@riseup.net>
;;; Copyright © 2021 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2022 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2023 Simon Tournier <zimon.toutoune@gmail.com>
;;; Copyright © 2023, 2024 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2023, 2024 David Elsing <david.elsing@posteo.net>
;;; Copyright © 2024 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2025 Herman Rimm <herman@rimm.ee>
;;; Copyright © 2024 Murilo <murilo@disroot.org>
;;; Copyright © 2024-2025 Luis Guilherme Coelho <lgcoelho@disroot.org>
;;; Copyright © 2025 Hilton Chain <hako@ultrarare.space>
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

(define-module (guix import crate)
  #:use-module (guix base16)
  #:use-module (guix base32)
  #:use-module ((guix build-system cargo) #:hide (crate-source))
  #:use-module (guix diagnostics)
  #:use-module (gcrypt hash)
  #:use-module (guix http-client)
  #:use-module (guix i18n)
  #:use-module (guix import crate cargo-lock)
  #:use-module (guix import json)
  #:use-module (guix import utils)
  #:use-module (guix memoization)
  #:use-module (guix packages)
  #:use-module (guix read-print)
  #:use-module (guix upstream)
  #:use-module (guix utils)
  #:use-module (guix scripts download)
  #:use-module (gnu packages)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 textual-ports)
  #:use-module (json)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-69)
  #:use-module (srfi srfi-71)
  #:export (crate->guix-package
            guix-package->crate-name
            string->license
            crate-recursive-import
            cargo-lock->expressions
            cargo-inputs-from-lockfile
            find-cargo-inputs-location
            extract-cargo-inputs
            %crate-updater))


;;;
;;; Interface to https://crates.io/api/v1.
;;;

;; Crates.  A crate is essentially a "package".  It can have several
;; "versions", each of which has its own set of dependencies, license,
;; etc.--see <crate-version> below.
(define-json-mapping <crate> make-crate crate?
  json->crate
  (name          crate-name)                      ;string
  (latest-version crate-latest-version "max_version") ;string
  (home-page     crate-home-page "homepage")      ;string | #nil
  (repository    crate-repository)                ;string
  (description   crate-description)               ;string
  (keywords      crate-keywords                   ;list of strings
                 "keywords" vector->list)
  (categories    crate-categories                 ;list of strings
                 "categories" vector->list)
  (versions      crate-versions "actual_versions" ;list of <crate-version>
                 (lambda (vector)
                   (map json->crate-version
                        (vector->list vector))))
  (links         crate-links))                    ;alist

;; Crate version.
(define-json-mapping <crate-version> make-crate-version crate-version?
  json->crate-version
  (id            crate-version-id)                ;integer
  (number        crate-version-number "num")      ;string
  (download-path crate-version-download-path "dl_path") ;string
  (readme-path   crate-version-readme-path "readme_path") ;string
  (license       crate-version-license "license"  ;string | #f
                 (match-lambda
                   ('null #f)
                   ((? string? str) str)))
  (yanked?       crate-version-yanked? "yanked")  ;boolean
  (links         crate-version-links))            ;alist

;; Autoload Guile-Semver so we only have a soft dependency.
(module-autoload! (current-module)
		  '(semver) '(string->semver semver->string semver>?))
(module-autoload! (current-module)
		  '(semver ranges) '(string->semver-range semver-range-contains?))

(define (lookup-crate name)
  "Look up NAME on https://crates.io and return the corresponding <crate>
record or #f if it was not found."
  (let ((json (json-fetch (string-append (%crate-base-url) "/api/v1/crates/"
                                         name))))
    (and=> (and json (assoc-ref json "crate"))
           (lambda (alist)
             ;; The "versions" field of ALIST is simply a list of version IDs
             ;; (integers).  Here, we squeeze in the actual version
             ;; dictionaries that are not part of ALIST but are just more
             ;; convenient handled this way.
             (let ((versions (or (assoc-ref json "versions") '#())))
               (json->crate `(,@alist
                              ("actual_versions" . ,versions))))))))

(define lookup-crate* (memoize lookup-crate))


;;;
;;; Converting crates to Guix packages.
;;;

(define* (make-crate-sexp #:key name version
                          home-page synopsis description license yanked?)
  "Return the `package' s-expression for a rust package with the given NAME,
VERSION, HOME-PAGE, SYNOPSIS, DESCRIPTION and LICENSE."
  (let* ((port (http-fetch (crate-uri name version)))
         (guix-name (downstream-package-name "" name))
         (description (beautify-description description))
         (pkg `(package
                   (name ,guix-name)
                   (version ,version)
                   ,@(if yanked?
                         `(,(comment "; This version was yanked!\n" #t))
                         '())
                   (source (origin
                             (method url-fetch)
                             (uri (crate-uri ,name version))
                             (file-name
                              ,@(if yanked?
                                    `((string-append name "-" version "-yanked.tar.gz"))
                                    `((string-append name "-" version ".tar.gz"))))
                             (sha256
                              (base32
                               ,(bytevector->nix-base32-string (port-sha256 port))))))
                   ,@(if yanked?
                         `((properties '((crate-version-yanked? . #t))))
                         '())
                   (build-system cargo-build-system)
                   (inputs (cargo-inputs ',(string->symbol guix-name)))
                   (home-page ,home-page)
                   (synopsis ,(beautify-synopsis synopsis))
                   (description ,(if (string-prefix? "This" description)
                                     description
                                     (string-append "This package provides "
                                                    description)))
                   (license ,(match license
                               (() #f)
                               (#f #f)
                               ((license) license)
                               (_ `(list ,@license)))))))
         (close-port port)
         (package->definition pkg)))

(define (string->license string)
  (filter-map (lambda (license)
                (and (not (string-null? license))
                     (not (any (lambda (elem) (string=? elem license))
                               '("AND" "OR" "WITH")))
                     (or (spdx-string->license license)
                         'unknown-license!)))
              (string-split string (string->char-set " /"))))

(define (min-element l less)
  "Returns the smallest element of l according to less or #f if l is empty."

  (let loop ((curr #f)
             (remaining l))
    (if (null-list? remaining)
        curr
        (let ((next (car remaining))
              (remaining (cdr remaining)))
          (if (and curr
                   (not (less next curr)))
              (loop curr remaining)
              (loop next remaining))))))

(define (max-crate-version-of-semver semver-range versions)
  "Returns the <crate-version> of the highest version found in VERSIONS that
satisfies SEMVER-RANGE."

  (define (crate->semver crate)
    (string->semver (crate-version-number crate)))

  (min-element
   (filter (lambda (crate)
             (semver-range-contains? semver-range (crate->semver crate)))
           versions)
   (lambda args
     (apply semver>? (map crate->semver args)))))

(define (nonyanked-crate-versions crate)
  "Returns a list of <crate-version>s which are not yanked by upstream."
  (filter (lambda (entry)
            (not (crate-version-yanked? entry)))
          (crate-versions crate)))

(define* (crate->guix-package
          crate-name #:key version allow-yanked? #:allow-other-keys)
  "Fetch the metadata for CRATE-NAME from crates.io, and return the
`package' s-expression corresponding to that package, or #f on failure.
When VERSION is specified, convert it into a semver range and attempt to fetch
the latest version matching this semver range; otherwise fetch the latest
version of CRATE-NAME."
  (define crate
    (lookup-crate* crate-name))

  (define version-number
    (and crate
         (or version
             (crate-latest-version crate))))

  ;; Find the highest version of a crate that fulfills the semver <range>.
  ;; If no matching non-yanked version has been found and allow-yanked? is #t,
  ;; also consider yanked packages.
  (define (find-crate-version crate range)
    (let ((semver-range (string->semver-range range))
          (versions     (nonyanked-crate-versions crate)))
      (or (and (not (null-list? versions))
               (max-crate-version-of-semver semver-range versions))
          (and allow-yanked?
               (not (null-list? (crate-versions crate)))
               (max-crate-version-of-semver semver-range
                                            (crate-versions crate))))))

  (define version*
    (and crate
         (or (find-crate-version crate version-number)
             (leave (G_ "~A: version ~a not found~%") crate-name version-number))))

  (if (and crate version*)
      (make-crate-sexp #:yanked? (crate-version-yanked? version*)
                       #:name crate-name
                       #:version (crate-version-number version*)
                       #:home-page
                       (let ((home-page (crate-home-page crate)))
                         (if (string? home-page)
                             home-page
                             (let ((repository (crate-repository crate)))
                               (if (string? repository)
                                   repository
                                   ""))))
                       #:synopsis (crate-description crate)
                       #:description (crate-description crate)
                       #:license (and=> (crate-version-license version*)
                                        string->license))
      (values #f '())))

(define (guix-package->crate-name package)
  "Return the crate name of PACKAGE."
  (and-let* ((origin (package-source package))
             (uri (origin-uri origin))
             (crate-url? uri)
             (len (string-length crate-url))
             (path (xsubstring uri len))
             (parts (string-split path #\/)))
    (match parts
      ((name _ ...) name))))


;;;
;;; Convert ‘Cargo.lock’ to Guix sources.
;;;

(define (cargo-lock->expressions lockfile package-name)
  "Given LOCKFILE, a 'Cargo.lock' file, import its content as source
expressions.  Return a source list and a Cargo inputs entry for PACKAGE-NAME
referencing all imported sources."
  (define (crate->guix-source crate)
    (match crate
      (('crate
        ('crate-name name)
        ('crate-version version)
        ('crate-source _)
        ('crate-checksum checksum))
       `(define
          ,(string->symbol
            (string-append (crate-name->package-name name) "-" version))
          ,@(if (or (string-suffix? "src" name)
                    (string-suffix? "sys" name))
                (list (comment ";; TODO: Check bundled sources.\n" #f))
                '())
          (crate-source ,name ,version
                        ,(bytevector->nix-base32-string
                          (base16-string->bytevector checksum)))))
      ;; Git snapshot.
      (('crate
        ('crate-name name)
        ('crate-version version)
        ('crate-source source))
       (begin
         (let* ((src (string-split source (char-set #\+ #\? #\#)))
                (url (second src))
                (commit (last src))
                (version (string-append version "." (string-take commit 7)))
                (checksum
                 (second
                  (string-split
                   (with-output-to-string
                     (lambda _
                       (guix-download "-g" url
                                      (string-append "--commit=" commit))))
                   #\newline))))
           `(define
              ,(string->symbol
                (string-append (crate-name->package-name name) "-" version))
              ,(comment
                ";; TODO: Define standalone package if this is a workspace.\n"
                #f)
              (origin
                (method git-fetch)
                (uri (git-reference
                      (url ,url)
                      (commit ,commit)))
                (file-name
                 (git-file-name ,(crate-name->package-name name) ,version))
                (sha256 (base32 ,checksum)))))))
      ;; Cargo workspace member.
      (else #f)))

  (let* ((source-expressions
          (filter-map crate->guix-source
                      (cargo-lock-string->scm
                       (call-with-input-file lockfile get-string-all))))
         (cargo-inputs-entry
          `(,(string->symbol package-name) =>
            (list ,@(map second source-expressions)))))
    (values source-expressions cargo-inputs-entry)))

(define* (cargo-inputs-from-lockfile #:optional (lockfile "Cargo.lock"))
  "Given LOCKFILE (default to \"Cargo.lock\" in current directory), return a
source list imported from it, to be used as package inputs.  This procedure
can be used for adding a manifest file within the source tree of a Rust
application."
  (let ((source-expressions
         cargo-inputs-entry
         (cargo-lock->expressions lockfile "cargo-inputs-temporary")))
    (eval-string
     (call-with-output-string
       (lambda (port)
         (for-each
          (cut pretty-print-with-comments port <>)
          `((use-modules (guix build-system cargo))
            ,@source-expressions
            (define-cargo-inputs lookup-cargo-inputs ,cargo-inputs-entry)
            (lookup-cargo-inputs 'cargo-inputs-temporary))))))))

(define (find-cargo-inputs-location file)
  "Search in FILE for a top-level definition of Cargo inputs.  Return the
location if found, or #f otherwise."
  (find-definition-location file 'lookup-cargo-inputs
                            #:define-prefix 'define-cargo-inputs))

(define* (extract-cargo-inputs file #:key exclude)
  "Search in FILE for a top-level definition of Cargo inputs.  If found,
return its entries excluding EXCLUDE, or an empty list otherwise."
  (call-with-input-file file
    (lambda (port)
      (do ((syntax (read-syntax port)
                   (read-syntax port)))
          ((match (syntax->datum syntax)
             (('define-cargo-inputs 'lookup-cargo-inputs _ ...) #t)
             ((? eof-object?) #t)
             (_ #f))
           (or (and (not (eof-object? syntax))
                    (match (syntax->datum syntax)
                      (('define-cargo-inputs 'lookup-cargo-inputs inputs ...)
                       (remove (lambda (cargo-input-entry)
                                 (eq? exclude (first cargo-input-entry)))
                               inputs))))
               '()))))))


;;;
;;; Updater
;;;

(define crate-package?
  (url-predicate crate-url?))

(define* (import-release package #:key version partial-version?)
  "Return an <upstream-source> for the latest release of PACKAGE.  Optionally
include a VERSION string to fetch a specific version, which may be a partial
prefix when PARTIAL-VERSION? is #t."
  (let* ((crate-name (guix-package->crate-name package))
         (crate      (lookup-crate crate-name))
         (versions (delay (nonyanked-crate-versions crate)))
         (find-max-minor-patch-version (lambda (base-version)
                                         (max-crate-version-of-semver
                                          (string->semver-range
                                           (string-append
                                            "^" base-version))
                                          (force versions))))
         (version (cond
                   ((and version partial-version?) ;partial version
                    (and=> (find-max-minor-patch-version version)
                           crate-version-number))
                   ((and version (not partial-version?)) ;exact version
                    version)
                   (else                ;latest version
                    (and=> (find-max-minor-patch-version
                            (package-version package))
                           crate-version-number)))))
    (and version
         (upstream-source
          (package (package-name package))
          (version version)
          (urls (list (crate-uri crate-name version)))))))

(define %crate-updater
  (upstream-updater
   (name 'crate)
   (description "Updater for crates.io packages")
   (pred crate-package?)
   (import import-release)))
