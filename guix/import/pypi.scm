;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 David Thompson <davet@gnu.org>
;;; Copyright © 2015 Cyril Roelandt <tipecaml@gmail.com>
;;; Copyright © 2015-2017, 2019-2024 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2017 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2018, 2023 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2019, 2024 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2020 Lars-Dominik Braun <ldb@leibniz-psychology.org>
;;; Copyright © 2020 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2020 Martin Becze <mjbecze@riseup.net>
;;; Copyright © 2021 Xinglu Chen <public@yoctocell.xyz>
;;; Copyright © 2021 Marius Bakke <marius@gnu.org>
;;; Copyright © 2022 Vivien Kraus <vivien@planete-kraus.eu>
;;; Copyright © 2021 Simon Tournier <zimon.toutoune@gmail.com>
;;; Copyright © 2022 Hartmut Goebel <h.goebel@crazy-compilers.com>
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

(define-module (guix import pypi)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module ((ice-9 rdelim) #:select (read-line))
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (srfi srfi-71)
  #:autoload   (gcrypt hash) (port-sha256)
  #:autoload   (guix base16) (base16-string->bytevector)
  #:autoload   (guix base32) (bytevector->nix-base32-string)
  #:autoload   (guix http-client) (http-fetch)
  #:use-module (guix utils)
  #:use-module (guix memoization)
  #:use-module (guix diagnostics)
  #:use-module (guix i18n)
  #:use-module ((guix ui) #:select (display-hint))
  #:use-module ((guix build utils)
                #:select ((package-name->name+version
                           . hyphen-package-name->name+version)
                          find-files
                          invoke
                          call-with-temporary-output-file))
  #:use-module (guix import utils)
  #:use-module (guix import json)
  #:use-module (json)
  #:use-module (guix build toml)
  #:use-module (guix packages)
  #:use-module (guix upstream)
  #:use-module ((guix licenses) #:prefix license:)
  #:export (%pypi-base-url
            parse-requires.txt
            parse-wheel-metadata
            specification->requirement-name
            guix-package->pypi-name
            pypi-recursive-import
            find-project-url
            pypi->guix-package
            %pypi-updater))

;; The PyPI API (notice the rhyme) is "documented" at:
;; <https://warehouse.readthedocs.io/api-reference/json/>.

(define %pypi-base-url
  ;; Base URL of the PyPI API.
  (make-parameter "https://pypi.org/pypi/"))

(define non-empty-string-or-false
  (match-lambda
    ("" #f)
    ((? string? str) str)
    ((or 'null #f) #f)))

;; PyPI project.
(define-json-mapping <pypi-project> make-pypi-project pypi-project?
  json->pypi-project
  (info          pypi-project-info "info" json->project-info) ;<project-info>
  (last-serial   pypi-project-last-serial "last_serial")      ;integer
  (releases      pypi-project-releases "releases" ;string/<distribution>* pairs
                 (match-lambda
                   (((versions . dictionaries) ...)
                    (map (lambda (version vector)
                           (cons version
                                 (map json->distribution
                                      (vector->list vector))))
                         versions dictionaries))))
  (distributions pypi-project-distributions "urls" ;<distribution>*
                 (lambda (vector)
                   (map json->distribution (vector->list vector)))))

;; Project metadata.
(define-json-mapping <project-info> make-project-info project-info?
  json->project-info
  (name         project-info-name)                ;string
  (author       project-info-author)              ;string
  (maintainer   project-info-maintainer)          ;string
  (classifiers  project-info-classifiers          ;list of strings
                "classifiers" vector->list)
  (description  project-info-description)         ;string
  (summary      project-info-summary)             ;string
  (keywords     project-info-keywords)            ;string
  (license      project-info-license)             ;string
  (download-url project-info-download-url         ;string | #f
                "download_url" non-empty-string-or-false)
  (home-page    project-info-home-page            ;string | #f
                "home_page" non-empty-string-or-false)
  (url          project-info-url "project_url")   ;string
  (release-url  project-info-release-url "release_url") ;string
  (version      project-info-version))            ;string

;; Distribution: a URL along with cryptographic hashes and metadata.
(define-json-mapping <distribution> make-distribution distribution?
  json->distribution
  (url          distribution-url)                  ;string
  (digests      distribution-digests)              ;list of string pairs
  (file-name    distribution-file-name "filename") ;string
  (has-signature? distribution-has-signature? "has_sig") ;Boolean
  (package-type distribution-package-type "packagetype") ;"bdist_wheel" | ...
  (python-version distribution-package-python-version
                  "python_version"))

(define (distribution-sha256 distribution)
  "Return the SHA256 hash of DISTRIBUTION as a bytevector, or #f."
  (match (assoc-ref (distribution-digests distribution) "sha256")
    (#f #f)
    (str (base16-string->bytevector str))))

(define (pypi-fetch name)
  "Return a <pypi-project> record for package NAME, or #f on failure."
  (and=> (json-fetch (string-append (%pypi-base-url) name "/json"))
         json->pypi-project))

;; For packages found on PyPI that lack a source distribution.
(define-condition-type &missing-source-error &error
  missing-source-error?
  (package  missing-source-error-package))

(define (latest-version project)
  "Return the latest version of PROJECT, a <pypi-project> record."
  (project-info-version (pypi-project-info project)))

(define* (source-release pypi-package
                         #:optional (version (latest-version pypi-package)))
  "Return the source release of VERSION for PYPI-PACKAGE, a <pypi-project>
record, by default the latest version."
  (let ((releases (or (assoc-ref (pypi-project-releases pypi-package) version)
                      '())))
    (or (find (lambda (release)
                (string=? "sdist" (distribution-package-type release)))
              releases)
        (raise (condition (&missing-source-error
                           (package pypi-package)))))))

(define* (wheel-release pypi-package
                        #:optional (version (latest-version pypi-package)))
  "Return the url of the wheel for the latest release of pypi-package,
or #f if there isn't any."
  (let ((releases (assoc-ref (pypi-project-releases pypi-package) version)))
    (find (lambda (release)
            (string=? "bdist_wheel" (distribution-package-type release)))
          releases)))

(define (python->package-name name)
  "Given the NAME of a package on PyPI, return a Guix-compliant name for the
package."
  (cond
   ((string-prefix? "python-" name) (snake-case name))
   ((string-suffix? "-python" name)
    (string-append "python-" (string-drop-right name 7)))
   ((or (string=? "trytond" name)
        (string-prefix? "trytond-" name)) (snake-case name))
   (else (string-append "python-" (snake-case name)))))

(define (guix-package->pypi-name package)
  "Given a Python PACKAGE built from pypi.org, return the name of the
package on PyPI."
  (define (url->pypi-name url)
    (hyphen-package-name->name+version
     (basename (file-sans-extension url))))

  (or (assoc-ref (package-properties package) 'upstream-name)
      (match (and=> (package-source package) origin-uri)
        ((? string? url)
         (url->pypi-name url))
        ((lst ...)
         (any url->pypi-name lst))
        (#f #f))))

(define (wheel-url->extracted-directory wheel-url)
  (match (string-split (basename wheel-url) #\-)
    ((name version _ ...)
     (string-append name "-" version ".dist-info"))))

(define (maybe-inputs package-inputs input-type)
  "Given a list of PACKAGE-INPUTS, tries to generate the 'inputs' field of a
package definition.  INPUT-TYPE, a symbol, is used to populate the name of
the input field."
  (match package-inputs
    (()
     '())
    ((package-inputs ...)
     `((,input-type (list ,@(map (compose string->symbol
                                          upstream-input-downstream-name)
                                 package-inputs)))))))

(define %requirement-name-regexp
  ;; Regexp to match the requirement name in a requirement specification.

  ;; Some grammar, taken from PEP-0508 (see:
  ;; https://www.python.org/dev/peps/pep-0508/).

  ;; Using this grammar makes the PEP-0508 regexp easier to understand for
  ;; humans.  The use of a regexp is preferred to more primitive string
  ;; manipulations because we can more directly match what upstream uses
  ;; (again, per PEP-0508).  The regexp approach is also easier to extend,
  ;; should we want to implement more completely the grammar of PEP-0508.

  ;; The unified rule can be expressed as:
  ;; specification = wsp* ( url_req | name_req ) wsp*

  ;; where url_req is:
  ;; url_req = name wsp* extras? wsp* urlspec wsp+ quoted_marker?

  ;; and where name_req is:
  ;; name_req = name wsp* extras? wsp* versionspec? wsp* quoted_marker?

  ;; Thus, we need only matching NAME, which is expressed as:
  ;; identifer_end = letterOrDigit | (('-' | '_' | '.' )* letterOrDigit)
  ;; identifier    = letterOrDigit identifier_end*
  ;; name          = identifier
  (let* ((letter-or-digit "[A-Za-z0-9]")
         (identifier-end (string-append "(" letter-or-digit "|"
                                        "[-_.]*" letter-or-digit ")"))
         (identifier (string-append "^" letter-or-digit identifier-end "*"))
         (name identifier))
    (make-regexp name)))

(define (specification->requirement-name spec)
  "Given a specification SPEC, return the requirement name."
  (match:substring
   (or (regexp-exec %requirement-name-regexp spec)
       (error (G_ "Could not extract requirement name in spec:") spec))))

(define (test-section? name)
  "Return #t if the section name contains 'test' or 'dev'."
  (any (cut string-contains-ci name <>)
       '("test" "dev")))

(define (parse-requires.txt requires.txt)
  "Given REQUIRES.TXT, a path to a Setuptools requires.txt file, return a list
of lists of requirements.

The first list contains the required dependencies while the second the
optional test dependencies.  Note that currently, optional, non-test
dependencies are omitted since these can be difficult or expensive to
satisfy."

  (define (comment? line)
    ;; Return #t if the given LINE is a comment, #f otherwise.
    (string-prefix? "#" (string-trim line)))

  (define (section-header? line)
    ;; Return #t if the given LINE is a section header, #f otherwise.
    (string-prefix? "[" (string-trim line)))

  (call-with-input-file requires.txt
    (lambda (port)
      (let loop ((required-deps '())
                 (test-deps '())
                 (inside-test-section? #f)
                 (optional? #f))
        (let ((line (read-line port)))
          (cond
           ((eof-object? line)
            (list (reverse required-deps)
                  (reverse test-deps)))
           ((or (string-null? line) (comment? line))
            (loop required-deps test-deps inside-test-section? optional?))
           ((section-header? line)
            ;; Encountering a section means that all the requirements
            ;; listed below are optional. Since we want to pick only the
            ;; test dependencies from the optional dependencies, we must
            ;; track those separately.
            (loop required-deps test-deps (test-section? line) #t))
           (inside-test-section?
            (loop required-deps
                  (cons (specification->requirement-name line)
                        test-deps)
                  inside-test-section? optional?))
           ((not optional?)
            (loop (cons (specification->requirement-name line)
                        required-deps)
                  test-deps inside-test-section? optional?))
           (optional?
            ;; Skip optional items.
            (loop required-deps test-deps inside-test-section? optional?))
           (else
            (warning (G_ "parse-requires.txt reached an unexpected \
condition on line ~a~%") line))))))))

(define (parse-wheel-metadata metadata)
  "Given METADATA, a Wheel metadata file, return a list of lists of
requirements.

Refer to the documentation of PARSE-REQUIRES.TXT for a description of the
returned value."
  ;; METADATA is a RFC-2822-like, header based file.

  (define (requires-dist-header? line)
    ;; Return #t if the given LINE is a Requires-Dist header.
    (string-match "^Requires-Dist: " line))

  (define (requires-dist-value line)
    (string-drop line (string-length "Requires-Dist: ")))

  (define (extra? line)
    ;; Return #t if the given LINE is an "extra" requirement.
    (string-match "extra == '(.*)'" line))

  (define (test-requirement? line)
    (and=> (match:substring (extra? line) 1) test-section?))

  (call-with-input-file metadata
    (lambda (port)
      (let loop ((required-deps '())
                 (test-deps '()))
        (let ((line (read-line port)))
          (cond
           ((eof-object? line)
            (list (reverse required-deps)
                  (reverse test-deps)))
           ((and (requires-dist-header? line) (not (extra? line)))
            (loop (cons (specification->requirement-name
                         (requires-dist-value line))
                        required-deps)
                  test-deps))
           ((and (requires-dist-header? line) (test-requirement? line))
            (loop required-deps
                  (cons (specification->requirement-name (requires-dist-value line))
                        test-deps)))
           (else
            (loop required-deps test-deps)))))))) ;skip line

(define (guess-requirements source-url wheel-url archive)
  "Given SOURCE-URL, WHEEL-URL and an ARCHIVE of the package, return a list
of the required packages specified in the requirements.txt file.  ARCHIVE will
be extracted in a temporary directory."

  (define (read-wheel-metadata wheel-archive)
    ;; Given WHEEL-ARCHIVE, a ZIP Python wheel archive, return the package's
    ;; requirements, or #f if the metadata file contained therein couldn't be
    ;; extracted.
    (let* ((dirname (wheel-url->extracted-directory wheel-url))
           (metadata (string-append dirname "/METADATA")))
      (call-with-temporary-directory
       (lambda (dir)
         (if (zero?
              (parameterize ((current-error-port (%make-void-port "rw+"))
                             (current-output-port (%make-void-port "rw+")))
                (system* "unzip" wheel-archive "-d" dir metadata)))
             (parse-wheel-metadata (string-append dir "/" metadata))
             (begin
               (warning
                (G_ "Failed to extract file: ~a from wheel.~%") metadata)
               #f))))))

  (define (guess-requirements-from-wheel)
    ;; Return the package's requirements using the wheel, or #f if an error
    ;; occurs.
    (call-with-temporary-output-file
     (lambda (temp port)
       (if wheel-url
           (and (url-fetch wheel-url temp)
                (read-wheel-metadata temp))
           (list '() '())))))

  (define (guess-requirements-from-pyproject.toml dir)
    (let* ((pyproject.toml-files (find-files dir (lambda (abs-file-name _)
                                          (string-match "/pyproject.toml$"
                                          abs-file-name))))
          (pyproject.toml (match pyproject.toml-files
                            (()
                              (warning (G_ "Cannot guess requirements from \
pyproject.toml file, because it does not exist.~%"))
                              '())
                            (else (parse-toml-file (first pyproject.toml-files)))))
          (pyproject-build-requirements
           (or (recursive-assoc-ref pyproject.toml '("build-system" "requires")) '()))
          (pyproject-dependencies
           (or (recursive-assoc-ref pyproject.toml '("project" "dependencies")) '()))
          ;; This is more of a convention, since optional-dependencies is a table of arbitrary values.
          (pyproject-test-dependencies
           (or (recursive-assoc-ref pyproject.toml '("project" "optional-dependencies" "test")) '())))
      (if (null? pyproject.toml)
        #f
        (list (map specification->requirement-name pyproject-dependencies)
              (map specification->requirement-name
                   (append pyproject-build-requirements
                           pyproject-test-dependencies))))))

  (define (guess-requirements-from-requires.txt dir)
    (let ((requires.txt-files (find-files dir (lambda (abs-file-name _)
		                                          (string-match "\\.egg-info/requires.txt$"
                                                  abs-file-name)))))
     (match requires.txt-files
       (()
        (warning (G_ "Cannot guess requirements from source archive: \
no requires.txt file found.~%"))
        #f)
       (else (parse-requires.txt (first requires.txt-files))))))

  (define (guess-requirements-from-source)
    ;; Return the package's requirements by guessing them from the source.
    (if (compressed-file? source-url)
        (call-with-temporary-directory
         (lambda (dir)
           (parameterize ((current-error-port (%make-void-port "rw+"))
                          (current-output-port (%make-void-port "rw+")))
             (if (string=? "zip" (file-extension source-url))
                 (invoke "unzip" archive "-d" dir)
                 (invoke "tar" "xf" archive "-C" dir)))
               (list (guess-requirements-from-pyproject.toml dir)
                     (guess-requirements-from-requires.txt dir))))
        (begin
          (warning (G_ "Unsupported archive format; \
cannot determine package dependencies from source archive: ~a~%")
                   (basename source-url))
          (list #f #f))))

  (define (merge a b)
    "Given lists A and B with two iteams each, combine A1 and B1, as well as A2 and B2."
    (match (list a b)
      (((first-propagated first-native) (second-propagated second-native))
       (list (append first-propagated second-propagated) (append first-native second-native)))))

  (define default-pyproject.toml-dependencies
    ;; If there is no pyproject.toml, we assume it’s an old-style setuptools-based project.
    '(() ("setuptools")))

  ;; requires.txt and the metadata of a wheel contain redundant information,
  ;; so fetch only one of them, preferring requires.txt from the source
  ;; distribution, which we always fetch, since the source tarball also
  ;; contains pyproject.toml.
  (match (guess-requirements-from-source)
    ((from-pyproject.toml #f)
      (merge (or from-pyproject.toml default-pyproject.toml-dependencies)
             (or (guess-requirements-from-wheel) '(() ()))))
    ((from-pyproject.toml from-requires.txt)
      (merge (or from-pyproject.toml default-pyproject.toml-dependencies)
             from-requires.txt))))

(define (compute-inputs source-url wheel-url archive)
  "Given the SOURCE-URL and WHEEL-URL of an already downloaded ARCHIVE, return
the corresponding list of <upstream-input> records."
  (define (requirements->upstream-inputs deps type)
    (filter-map (match-lambda
                  ("argparse" #f)
                  (name (upstream-input
                         (name name)
                         (downstream-name (python->package-name name))
                         (type type))))
                (sort deps string-ci<?)))

  (define (add-missing-native-inputs inputs)
    ;; setuptools cannot build wheels without the python-wheel.
    (if (member "setuptools" inputs)
      (cons "wheel" inputs)
      inputs))

  ;; TODO: Record version number ranges in <upstream-input>.
  (let ((dependencies (guess-requirements source-url wheel-url archive)))
    (match dependencies
      ((propagated native)
       (append (requirements->upstream-inputs (delete-duplicates propagated)
                                              'propagated)
               (requirements->upstream-inputs (delete-duplicates (add-missing-native-inputs native))
                                              'native))))))

(define* (pypi-package-inputs pypi-package #:optional version)
  "Return the list of <upstream-input> for PYPI-PACKAGE.  This procedure
downloads the source and possibly the wheel of PYPI-PACKAGE."
  (let* ((info       (pypi-project-info pypi-package))
         (version    (or version (project-info-version info)))
         (dist       (source-release pypi-package version))
         (source-url (distribution-url dist))
         (wheel-url  (and=> (wheel-release pypi-package version)
                            distribution-url)))
    (call-with-temporary-output-file
     (lambda (archive port)
       (and (url-fetch source-url archive)
            (compute-inputs source-url wheel-url archive))))))

(define (find-project-url name pypi-url)
  "Try different project name substitution until the result is found in
pypi-uri.  Downcase is required for \"uWSGI\", and
underscores are required for flake8-array-spacing."
  ;; XXX: Each tool producing wheels and sdists appear to have their own,
  ;; distinct, naming scheme.
  (or (find (cut string-contains pypi-url <>)
            (list name
                  (string-downcase name)
                  (string-replace-substring name "-" "_")
                  (string-replace-substring name "." "_")))
      (begin
        (warning
         (G_ "project name ~a does not appear verbatim in the PyPI URI~%")
         name)
        (display-hint
         (format #f (G_ "The PyPI URI is: @url{~a}.  You should review the
pypi-uri declaration in the generated package. You may need to replace ~s with
a substring of the PyPI URI that identifies the package.")  pypi-url name))
name)))

(define* (pypi-package->upstream-source pypi-package
                                        #:optional version partial-version?)
  "Return the upstream source for the given VERSION of PYPI-PACKAGE, a
<pypi-project> record.  If VERSION is omitted or #f, use the latest version.
If PARTIAL-VERSION? is #t, use the latest version found that is prefixed by
VERSION."
  (let* ((info       (pypi-project-info pypi-package))
         (versions   (map (match-lambda
                            ((version . _) version))
                          (pypi-project-releases pypi-package)))
         (version    (find-version versions version partial-version?))
         (dist       (source-release pypi-package version))
         (source-url (distribution-url dist))
         (wheel-url  (and=> (wheel-release pypi-package version)
                            distribution-url)))
    (let ((extra-inputs (if (string-suffix? ".zip" source-url)
                            (list (upstream-input
                                   (name "zip")
                                   (downstream-name "zip")
                                   (type 'native)))
                            '())))
      (upstream-source
       (urls (list source-url))
       (signature-urls
        (if (distribution-has-signature? dist)
            (list (string-append source-url ".asc"))
            #f))
       (inputs (append (pypi-package-inputs pypi-package)
                       extra-inputs))
       (package (project-info-name info))
       (version version)))))

(define* (make-pypi-sexp pypi-package
                         #:optional (version (latest-version pypi-package)))
  "Return the `package' s-expression the given VERSION of PYPI-PACKAGE, a
<pypi-project> record."
  (define (maybe-upstream-name name)
    (if (string-match ".*\\-[0-9]+" name)
        `((properties ,`'(("upstream-name" . ,name))))
        '()))

  (let* ((info (pypi-project-info pypi-package))
         (name (project-info-name info))
         (source-url (and=> (source-release pypi-package version)
                            distribution-url))
         (sha256 (and=> (source-release pypi-package version)
                        distribution-sha256))
         (source (pypi-package->upstream-source pypi-package version)))
    (values
     `(package
        (name ,(python->package-name name))
        (version ,version)
        (source
         (origin
           (method url-fetch)
           (uri (pypi-uri
                 ,(find-project-url name source-url)
                 version
                 ;; Some packages have been released as `.zip`
                 ;; instead of the more common `.tar.gz`. For
                 ;; example, see "path-and-address".
                 ,@(if (string-suffix? ".zip" source-url)
                       '(".zip")
                       '())))
           (sha256 (base32
                    ,(and=> (or sha256
                                (let* ((port (http-fetch source-url))
                                       (hash (port-sha256 port)))
                                  (close-port port)
                                  hash))
                            bytevector->nix-base32-string)))))
        ,@(maybe-upstream-name name)
        (build-system pyproject-build-system)
        ,@(maybe-inputs (upstream-source-propagated-inputs source)
                        'propagated-inputs)
        ,@(maybe-inputs (upstream-source-native-inputs source)
                        'native-inputs)
        (home-page ,(project-info-home-page info))
        (synopsis ,(project-info-summary info))
        (description ,(and=> (non-empty-string-or-false
                              (project-info-summary info))
                             beautify-description))
        (license ,(license->symbol
                   (string->license
                    (project-info-license info)))))
     (map upstream-input-name (upstream-source-inputs source)))))

(define pypi->guix-package
  (memoize
   (lambda* (package-name #:key version #:allow-other-keys)
     "Fetch the metadata for PACKAGE-NAME from pypi.org, and return the
`package' s-expression corresponding to that package, or #f on failure."
     (let* ((project (pypi-fetch package-name))
            (info    (and=> project pypi-project-info))
            (version (or version (and=> project latest-version))))
       (if project
           (guard (c ((missing-source-error? c)
                      (let ((package (missing-source-error-package c)))
                        (raise
                         (apply
                          make-compound-condition
                          (formatted-message
                           (G_ "no source release for pypi package ~a ~a~%")
                           (project-info-name info) version)
                          (match (project-info-home-page info)
                            ((or #f "") '())
                            (url
                             (list
                              (condition
                               (&fix-hint
                                (hint (format #f (G_ "This indicates that the
package is available on PyPI, but only as a \"wheel\" containing binaries, not
source.  To build it from source, refer to the upstream repository at
@uref{~a}.")
                                              url))))))))))))
             (make-pypi-sexp project version))
           (values #f '()))))))

(define* (pypi-recursive-import package-name #:optional version)
  (recursive-import package-name
                    #:version version
                    #:repo->guix-package pypi->guix-package
                    #:guix-name python->package-name))

(define (string->license str)
  "Convert the string STR into a license object."
  (match str
    ("GNU LGPL" license:lgpl2.0)
    ("GPL" license:gpl3)
    ((or "BSD" "BSD-3" "BSD License") license:bsd-3)
    ("BSD-2-Clause" license:bsd-2)
    ((or "MIT" "MIT license" "MIT License" "Expat license") license:expat)
    ("Public domain" license:public-domain)
    ((or "Apache License, Version 2.0" "Apache 2.0") license:asl2.0)
    ("MPL 2.0" license:mpl2.0)
    (_ #f)))

(define pypi-package?
  (url-predicate
   (lambda (url)
     (or (string-prefix? "https://pypi.org/" url)
         (string-prefix? "https://pypi.python.org/" url)
         (string-prefix? "https://pypi.org/packages" url)
         (string-prefix? "https://files.pythonhosted.org/packages" url)))))

(define* (import-release package #:key version partial-version?)
  "Return an <upstream-source> for the latest release of PACKAGE. Optionally
include a VERSION string to fetch a specific version."
  (and-let* ((pypi-name    (guix-package->pypi-name package))
             (pypi-package (pypi-fetch pypi-name)))
    (guard (c ((missing-source-error? c) #f))
      (pypi-package->upstream-source pypi-package
                                     version partial-version?))))

(define %pypi-updater
  (upstream-updater
   (name 'pypi)
   (description "Updater for PyPI packages")
   (pred pypi-package?)
   (import import-release)))
