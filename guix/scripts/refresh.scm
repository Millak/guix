;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013-2023 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2014 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2015 Alex Kost <alezost@gmail.com>
;;; Copyright © 2016 Ben Woodcroft <donttrustben@gmail.com>
;;; Copyright © 2017 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2018 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2019, 2023 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2020 Simon Tournier <zimon.toutoune@gmail.com>
;;; Copyright © 2021 Sarah Morgensen <iskarian@mgsn.dev>
;;; Copyright © 2022 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2023-2025 Maxim Cournoyer maxim.cournoyer@gmail.com>
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

(define-module (guix scripts refresh)
  #:use-module (guix ui)
  #:use-module (guix scripts)
  #:use-module ((guix scripts build) #:select (%standard-build-options))
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (guix discovery)
  #:use-module (guix packages)
  #:use-module (guix profiles)
  #:use-module (guix upstream)
  #:use-module (guix graph)
  #:use-module (guix scripts graph)
  #:use-module (guix monads)
  #:use-module (guix gnupg)
  #:use-module (guix hash)
  #:use-module (gnu packages)
  #:use-module ((gnu packages base) #:select (%final-inputs))
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-37)
  #:use-module (srfi srfi-71)
  #:export (guix-refresh))


;;;
;;; Command-line options.
;;;

(define %default-options
  ;; Alist of default option values.
  '((key-download . auto)))

(define %options
  ;; Specification of the command-line options.
  (list (option '(#\u "update") #f #f
                (lambda (opt name arg result)
                  (alist-cons 'update? #t result)))
        (option '(#\s "select") #t #f
                (lambda (opt name arg result)
                  (match arg
                    ((or "core" "non-core")
                     (alist-cons 'select (string->symbol arg)
                                 result))
                    ((? (cut string-prefix? "module:" <>))
                     (let ((mod (cond
                                 ;; Shorthand name: "module:guile".
                                 ((string-match "module:([^\( ]+)$" arg) =>
                                  (lambda (m)
                                    `(gnu packages ,(string->symbol
                                                     (match:substring m 1)))))
                                 ;; Full name : "module:(gnu packages guile)".
                                 ((string-match "module:\\(([^)]+)\\)$" arg) =>
                                  (lambda (m)
                                    (map string->symbol
                                         (string-split
                                          (match:substring m 1) #\space))))
                                 (else (leave (G_ "invalid module: ~a~%") arg)))))
                       (alist-cons 'select (cons 'module mod) result)))
                    (x
                     (leave (G_ "~a: invalid selection; expected `core', `non-core' or `module:NAME'~%")
                            arg)))))
        (option '(#\t "type") #t #f
                (lambda (opt name arg result)
                  (let* ((not-comma (char-set-complement (char-set #\,)))
                         (names (map string->symbol
                                     (string-tokenize arg not-comma))))
                    (alist-cons 'updaters names result))))
        (find (lambda (option)
                (member "load-path" (option-names option)))
              %standard-build-options)
        (option '("list-updaters") #f #f
                (lambda args
                  (list-updaters-and-exit)))
        (option '(#\m "manifest") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'manifest arg result)))
        (option '("target-version") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'target-version arg result)))
        (option '(#\e "expression") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'expression arg result)))
        (option '(#\l "list-dependent") #f #f
                (lambda (opt name arg result)
                  (alist-cons 'list-dependent? #t result)))
        (option '(#\r "recursive") #f #f
                (lambda (opt name arg result)
                  (alist-cons 'recursive? #t result)))
        (option '(#\T "list-transitive") #f #f
                (lambda (opt name arg result)
                  (alist-cons 'list-transitive? #t result)))

        (option '("keyring") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'keyring arg result)))
        (option '("key-server") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'key-server arg result)))
        (option '("gpg") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'gpg-command arg result)))
        (option '("key-download") #t #f
                (lambda (opt name arg result)
                  (match arg
                    ((or "auto" "interactive" "always" "never")
                     (alist-cons 'key-download (string->symbol arg)
                                 result))
                    (x
                     (leave (G_ "unsupported policy: ~a~%")
                            arg)))))

        (option '(#\h "help") #f #f
                (lambda args
                  (leave-on-EPIPE (show-help))
                  (exit 0)))
        (option '(#\V "version") #f #f
                (lambda args
                  (show-version-and-exit "guix refresh")))))

(define (show-help)
  (display (G_ "Usage: guix refresh [OPTION]... [PACKAGE]...
Update package definitions to match the latest upstream version.

When PACKAGE... is given, update only the specified packages.  Otherwise
update all the packages of the distribution, or the subset thereof
specified with `--select'.\n"))
  (display (G_ "
  -e, --expression=EXPR  consider the package EXPR evaluates to"))
  (display (G_ "
  -u, --update           update source files in place"))
  (display (G_ "
  -s, --select=SUBSET    select all the packages in SUBSET, one of `core`,
                         `non-core' or `module:NAME' (eg: module:guile)
                         the module can also be fully specified as
                         'module:(gnu packages guile)'"))
  (display (G_ "
  -m, --manifest=FILE    select all the packages from the manifest in FILE"))
  (display (G_ "
      --target-version=VERSION
                         update the package or packages to VERSION
                         VERSION may be partially specified, e.g. as 6
                         or 6.4 instead of 6.4.3"))
  (display (G_ "
  -t, --type=UPDATER,... restrict to updates from the specified updaters
                         (e.g., 'gnu')"))
  (display (G_ "
      --list-updaters    list available updaters and exit"))
  (display (G_ "
  -l, --list-dependent   list top-level dependent packages that would need to
                         be rebuilt as a result of upgrading PACKAGE..."))
  (display (G_ "
  -r, --recursive        check the PACKAGE and its inputs for upgrades"))
  (display (G_ "
  -T, --list-transitive  list all the packages that PACKAGE depends on"))
  (newline)
  (display (G_ "
      --keyring=FILE     use FILE as the keyring of upstream OpenPGP keys"))
  (display (G_ "
      --key-server=HOST  use HOST as the OpenPGP key server"))
  (display (G_ "
      --gpg=COMMAND      use COMMAND as the GnuPG 2.x command"))
  (display (G_ "
      --key-download=POLICY
                         handle missing OpenPGP keys according to POLICY:
                         'auto', 'always', 'never', and 'interactive'.
                         When left unspecified, the default policy is 'auto',
                         which automatically selects interactive or always."))
  (newline)
  (display (G_ "
  -L, --load-path=DIR    prepend DIR to the package module search path"))
  (newline)
  (display (G_ "
  -h, --help             display this help and exit"))
  (display (G_ "
  -V, --version          display version information and exit"))
  (newline)
  (show-bug-report-information))


;;;
;;; Utilities.
;;;

(define-record-type <update-spec>
  (%update-spec package version partial?)
  update?
  (package update-spec-package)
  (version update-spec-version)
  (partial? update-spec-partial?))

(define* (update-spec package #:optional version partial?)
  (%update-spec package version partial?))

(define (update-specification->update-spec spec fallback-version)
  "Given SPEC, a package name like \"guile@2.0=2.0.8\", return a <update>
record with two fields: the package to upgrade, and the target version.  When
SPEC lacks a version, use FALLBACK-VERSION."
  (match (string-rindex spec #\=)
    (#f  (update-spec (specification->package spec) fallback-version
                      (not (not fallback-version))))
    (idx (let ((version (substring spec (1+ idx)))
               (package (specification->package (substring spec 0 idx))))
           (if (string-prefix? "~" version)
               (update-spec package (string-drop version 1) #t) ;partial
               (update-spec package version))))))

(define (options->update-specs opts)
  "Return the list of <update-spec> records requested by OPTS, honoring
options like '--recursive'."
  (define target-version (assoc-ref opts 'target-version))

  (define core-package?
    (let* ((input->package (match-lambda
                             ((name (? package? package) _ ...) package)
                             (_ #f)))
           (final-inputs   (map input->package
                                (%final-inputs (%current-system))))
           (core           (append final-inputs
                                   (append-map (compose (cut filter-map input->package <>)
                                                        package-transitive-inputs)
                                               final-inputs)))
           (names          (delete-duplicates (map package-name core))))
      (lambda (package)
        "Return true if PACKAGE is likely a \"core package\"---i.e., one whose
update would trigger a complete rebuild."
        ;; Compare by name because packages in base.scm basically inherit
        ;; other packages.  So, even if those packages are not core packages
        ;; themselves, updating them would also update those who inherit from
        ;; them.
        ;; XXX: Fails to catch MPFR/MPC, whose *source* is used as input.
        (member (package-name package) names))))

  (define (keep-newest package lst)
    ;; If a newer version of PACKAGE is already in LST, return LST; otherwise
    ;; return LST minus the other version of PACKAGE in it, plus PACKAGE.
    (let ((name (package-name package)))
      (match (find (lambda (p)
                     (string=? (package-name p) name))
                   lst)
        ((? package? other)
         (if (version>? (package-version other) (package-version package))
             lst
             (cons package (delq other lst))))
        (_
         (cons package lst)))))

  (define update-specs
    ;; Update specs explicitly passed as command-line arguments.
    (match (append-map (match-lambda
                         (('argument . spec)
                          ;; Take either the specified version or the latest
                          ;; one.  The version specified as part of a spec
                          ;; takes precedence, with the command-line specified
                          ;; --target-version used as a fallback.
                          (list (update-specification->update-spec
                                 spec target-version)))
                         (('expression . exp)
                          (list (update-spec (read/eval-package-expression exp)
                                             target-version #t)))
                         (('manifest . manifest)
                          (map (cut update-spec <> target-version #t)
                               (packages-from-manifest manifest)))
                         (_
                          '()))
                       opts)
      (()                                         ;default to all packages
       (let ((select? (match (assoc-ref opts 'select)
                        ('core core-package?)
                        ('non-core (negate core-package?))
                        (_ (const #t))))
             (modules (match (assoc-ref opts 'select)
                         (('module . mod)
                          (list (resolve-interface mod)))
                         (_ (all-modules (%package-module-path)
                                         #:warn
                                         warn-about-load-error)))))
         (map update-spec
              (fold-packages (lambda (package result)
                               (if (select? package)
                                   (keep-newest package result)
                                   result))
                             '()
                             modules))))
      (some                                       ;user-specified packages
       some)))

  (if (assoc-ref opts 'recursive?)
      (mlet* %store-monad ((edges (node-edges %bag-node-type (all-packages)))
                           (packages -> (node-transitive-edges
                                         (map update-spec-package update-specs)
                                         edges)))
        ;; FIXME: The 'version' field of each update spec is lost.
        (return (map update-spec packages)))
      (with-monad %store-monad
        (return update-specs))))


;;;
;;; Updates.
;;;

(define (lookup-updater-by-name name)
  "Return the updater called NAME."
  (or (find (lambda (updater)
              (eq? name (upstream-updater-name updater)))
            (force %updaters))
      (leave (G_ "~a: no such updater~%") name)))

(define (list-updaters-and-exit)
  "Display available updaters and exit."
  (format #t (G_ "Available updaters:~%"))
  (newline)

  (let* ((packages (fold-packages cons '()))
         (total    (length packages)))
    (define uncovered
      (fold (lambda (updater uncovered)
              (let ((matches (filter (upstream-updater-predicate updater)
                                     packages)))
                ;; TRANSLATORS: The parenthetical expression here is rendered
                ;; like "(42% coverage)" and denotes the fraction of packages
                ;; covered by the given updater.
                (format #t (G_ "  - ~a: ~a (~2,1f% coverage)~%")
                        (upstream-updater-name updater)
                        (G_ (upstream-updater-description updater))
                        (* 100. (/ (length matches) total)))
                (lset-difference eq? uncovered matches)))
            packages
            (force %updaters)))

    (newline)
    (format #t (G_ "~2,1f% of the packages are covered by these updaters.~%")
            (* 100. (/ (- total (length uncovered)) total))))
  (exit 0))

(define (warn-no-updater package)
  (warning (package-location package)
           (G_ "no updater for ~a~%")
           (package-name package)))

(define* (update-package store update-spec updaters
                         #:key (key-download 'auto) key-server
                         warn?)
  "Update the source file that correspond to the package in UPDATE-SPEC,
an <update-spec> object.  KEY-DOWNLOAD specifies a download policy for
missing OpenPGP keys; allowed values: 'auto' (default), 'interactive',
'always', and 'never'.  When WARN? is true, warn about packages that
have no matching updater.  PARTIAL-VERSION? is provided to the
underlying `package-update' call; see its documentation for the
details."
  (match update-spec
    (($ <update-spec> package version partial?)
     (if (lookup-updater package updaters)
         (let ((version output source
                        (package-update store package updaters
                                        #:version version
                                        #:partial-version? partial?
                                        #:key-download key-download
                                        #:key-server key-server))
               (loc (or (package-field-location package 'version)
                        (package-location package))))
           (when version
             (if (and=> output file-exists?)
                 (begin
                   (info loc
                         (G_ "~a: updating from version ~a to version ~a...~%")
                         (package-name package)
                         (package-version package) version)
                   (let ((hash (file-hash* output)))
                     (update-package-source package source hash)))
                 (warning (G_ "~a: version ~a could not be \
downloaded and authenticated; not updating~%")
                          (package-name package) version))))
         (when warn?
           (warn-no-updater package))))))

(define* (check-for-package-update update-spec updaters #:key warn?)
  "Check whether UPDATE-SPEC is feasible, and print a message.
When WARN? is true and no updater exists for PACKAGE, print a warning."
  (match update-spec
    (($ <update-spec> package version partial?)
     (match (package-latest-release package updaters
                                    #:version version
                                    #:partial-version? partial?)
       ((? upstream-source? source)
        (let ((loc (or (package-field-location package 'version)
                       (package-location package))))
          (case (version-compare (upstream-source-version source)
                                 (package-version package))
            ((>)
             (info loc
                   (G_ "~a would be upgraded from ~a to ~a~%")
                   (package-name package) (package-version package)
                   (upstream-source-version source)))
            ((=)
             (when warn?
               (info loc
                     (G_ "~a is already the latest version of ~a~%")
                     (package-version package)
                     (package-name package))))
            (else
             (if version
                 (info loc
                       (G_ "~a would be downgraded from ~a to ~a~%")
                       (package-name package)
                       (package-version package)
                       (upstream-source-version source))
                 (when warn?
                   (warning loc
                            (G_ "~a is greater than \
the latest known version of ~a (~a)~%")
                            (package-version package)
                            (package-name package)
                            (upstream-source-version source))))))))
       (#f
        (when warn?
          ;; Distinguish between "no updater" and "failing updater."
          (match (lookup-updater package updaters)
            ((? upstream-updater? updater)
             (if version
                 (warning (G_ "'~a' updater failed to find version ~a of '~a'~%")
                          (upstream-updater-name updater)
                          version
                          (package-name package))
                 (warning (package-location package)
                          (G_ "'~a' updater failed to determine available \
releases for ~a~%")
                          (upstream-updater-name updater)
                          (package-name package))))
            (#f
             (warn-no-updater package)))))))))


;;;
;;; Dependents.
;;;

(define (list-dependents packages)
  "List all the things that would need to be rebuilt if PACKAGES are changed."
  ;; Using %BAG-NODE-TYPE is more accurate than using %PACKAGE-NODE-TYPE
  ;; because it includes implicit dependencies.
  (define (full-name package)
    (string-append (package-name package) "@"
                   (package-version package)))

  (mlet %store-monad ((edges (node-back-edges %bag-node-type
                                              (package-closure (all-packages)))))
    (let* ((dependents (node-transitive-edges packages edges))
           (covering   (filter (lambda (node)
                                 (null? (edges node)))
                               dependents)))
      (match dependents
        (()
         (format (current-output-port)
                 (N_ "No dependents other than itself: ~{~a~}~%"
                     "No dependents other than themselves: ~{~a~^ ~}~%"
                     (length packages))
                 (map full-name packages)))

        ((x)
         (format (current-output-port)
                 (G_ "A single dependent package: ~a~%")
                 (full-name x)))
        (lst
         (format (current-output-port)
                 (N_ "Building the following ~d package would ensure ~d \
dependent packages are rebuilt: ~{~a~^ ~}~%"
                     "Building the following ~d packages would ensure ~d \
dependent packages are rebuilt: ~{~a~^ ~}~%"
                     (length covering))
                 (length covering) (length dependents)
                 (map full-name covering))))
      (return #t))))

(define (list-transitive packages)
  "List all the packages that would cause PACKAGES to be rebuilt if they are changed."
  ;; Using %BAG-NODE-TYPE is more accurate than using %PACKAGE-NODE-TYPE
  ;; because it includes implicit dependencies.
  (define (full-name package)
    (string-append (package-name package) "@"
                   (package-version package)))

  (mlet %store-monad ((edges (node-edges %bag-node-type
                                         ;; Here we don't want the -boot0 packages.
                                         (fold-packages cons '()))))
    (let ((dependent (node-transitive-edges packages edges)))
      (match packages
        ((x)
         (format (current-output-port)
                 (G_ "~a depends on the following ~d packages: ~{~a~^ ~}~%.")
                 (full-name x) (length dependent) (map full-name dependent)))
        (lst
         (format (current-output-port)
                 (G_ "The following ~d packages \
all are dependent packages: ~{~a~^ ~}~%")
                 (length dependent) (map full-name dependent))))
      (return #t))))


;;;
;;; Manifest.
;;;

(define (manifest->packages manifest)
  "Return the list of packages in MANIFEST."
  (filter-map (lambda (entry)
                (let ((item (manifest-entry-item entry)))
                  (if (package? item) item #f)))
              (manifest-entries manifest)))

(define (packages-from-manifest manifest)
  "Return the list of packages in loaded MANIFEST."
  (let* ((user-module (make-user-module '((guix profiles) (gnu))))
         (manifest    (load* manifest user-module)))
    (manifest->packages manifest)))


;;;
;;; Entry point.
;;;

(define-command (guix-refresh . args)
  (category packaging)
  (synopsis "update existing package definitions")

  (define (parse-options)
    ;; Return the alist of option values.
    (parse-command-line args %options (list %default-options)
                        #:build-options? #f))

  (define (options->updaters opts)
    ;; Return the list of updaters to use.
    (match (filter-map (match-lambda
                         (('updaters . names)
                          (map lookup-updater-by-name names))
                         (_ #f))
                       opts)
      (()
       ;; Use the default updaters.
       (force %updaters))
      (lists
       (concatenate lists))))

  ;; Sort SEQUENCE by consecutive application of all tests to selected
  ;; elements in SEQUENCE.  Every item in LESS+SELECT is a pair consisting of
  ;; a binary comparison procedure and a selector, e.g (cons < car).
  (define (cascade-sort sequence . less+select)
    (define (make-test less? select)
      (lambda (a b)
        (less? (select a) (select b))))
    (define (combine-tests procs)
      (lambda (a b)
        (eq? 'less
             (any (lambda (test)
                    (cond
                     ((test a b) 'less)
                     ((test b a) 'greater)
                     (else #false)))
                  procs))))
    (let ((tests (match less+select
                   (((less? . select) ...)
                    (combine-tests
                     (map make-test less? select))))))
      (sort sequence tests)))

  (let* ((opts            (parse-options))
         (update?         (assoc-ref opts 'update?))
         (updaters        (options->updaters opts))
         (recursive?      (assoc-ref opts 'recursive?))
         (list-dependent? (assoc-ref opts 'list-dependent?))
         (list-transitive? (assoc-ref opts 'list-transitive?))
         (key-download    (assoc-ref opts 'key-download))

         ;; Warn about missing updaters when a package is explicitly given on
         ;; the command line.
         (warn?           (and (or (assoc-ref opts 'argument)
                                   (assoc-ref opts 'expression)
                                   (assoc-ref opts 'manifest))
                               (not recursive?))))
    (with-error-handling
      (with-store store
        (run-with-store store
          (mlet %store-monad ((update-specs (options->update-specs opts)))
            (cond
             (list-dependent?
              (list-dependents (map update-spec-package update-specs)))
             (list-transitive?
              (list-transitive (map update-spec-package update-specs)))
             (update?
              (parameterize ((%openpgp-key-server
                              (or (assoc-ref opts 'key-server)
                                  (%openpgp-key-server)))
                             (%gpg-command
                              (or (assoc-ref opts 'gpg-command)
                                  (%gpg-command)))
                             (current-keyring
                              (or (assoc-ref opts 'keyring)
                                  (string-append (config-directory)
                                                 "/upstream/trustedkeys.kbx"))))
                (let* ((spec->location
                        (compose package-location
                                 update-spec-package))
                       ;; Sort the specs so that we update packages from the
                       ;; bottom of the file to the top.  This way we can be
                       ;; sure that the package locations are always correct
                       ;; and never shifted due to previous edits.
                       (sorted-update-specs
                        (cascade-sort update-specs
                                      (cons string<
                                            (compose location-file
                                                     spec->location))
                                      (cons >
                                            (compose location-line
                                                     spec->location)))))
                  (for-each
                   (lambda (spec)
                     (update-package store
                                     spec
                                     updaters
                                     #:key-server (%openpgp-key-server)
                                     #:key-download key-download
                                     #:warn? warn?))
                   sorted-update-specs))
                (return #t)))
             (else
              (for-each (cut check-for-package-update <> updaters
                             #:warn? warn?)
                        update-specs)
              (return #t)))))))))
