;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014, 2015, 2016, 2017 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014, 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2016 Alex Kost <alezost@gmail.com>
;;; Copyright © 2017 Andy Wingo <wingo@pobox.com>
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

(define-module (guix potluck packages)
  #:use-module (gnu packages)
  #:use-module (guix base32)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix potluck build-systems)
  #:use-module (guix potluck licenses)
  #:use-module (guix records)
  #:use-module (guix utils)
  #:use-module ((guix ui) #:select (package-specification->name+version+output))
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)
  #:use-module ((srfi srfi-1) #:select (concatenate delete-duplicates))
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (web uri)
  #:export (potluck-source
            potluck-source?
            potluck-source-git-uri
            potluck-source-git-commit
            potluck-source-sha256
            potluck-source-snippet

            potluck-package
            potluck-package?
            potluck-package-name
            potluck-package-version
            potluck-package-source
            potluck-package-build-system
            potluck-package-arguments
            potluck-package-inputs
            potluck-package-native-inputs
            potluck-package-propagated-inputs
            potluck-package-synopsis
            potluck-package-description
            potluck-package-license
            potluck-package-home-page
            potluck-package-location
            potluck-package-field-location

            pretty-print-potluck-source
            pretty-print-potluck-package

            load-potluck-package

            validate-potluck-package

            lower-potluck-source
            lower-potluck-package

            lower-potluck-package-to-module))

;;; Commentary:
;;;
;;; This module provides a facility to define "potluck packages" in a
;;; Guix-based distribution, and a facility to translate those packages to
;;; "normal" Guix packages.
;;;
;;; Code:

(define-record-type* <potluck-source>
  potluck-source make-potluck-source
  potluck-source?
  (git-uri    potluck-source-git-uri)               ; uri string
  (git-commit potluck-source-git-commit)            ; git sha1 string
  (sha256     potluck-source-sha256)                ; base32 string
  (snippet    potluck-source-snippet (default #f))) ; sexp or #f

(define-record-type* <potluck-package>
  potluck-package make-potluck-package
  potluck-package?
  (name               potluck-package-name)         ; string
  (version            potluck-package-version)      ; string
  (source             potluck-package-source)       ; <potluck-source>
                                                    ; instance
  (build-system       potluck-package-build-system) ; build system name as
                                                    ; symbol
  (arguments          potluck-package-arguments     ; arguments for the build
                                                    ; method
                      (default '()) (thunked))
  (inputs             potluck-package-inputs        ; input packages or
                                                    ; derivations
                      (default '()) (thunked))
  (propagated-inputs  potluck-package-propagated-inputs ; same, but propagated
                      (default '()) (thunked))
  (native-inputs      potluck-package-native-inputs ; native input packages or
                                                    ; derivations
                      (default '()) (thunked))
  (synopsis           potluck-package-synopsis)     ; one-line description
  (description        potluck-package-description)  ; one or two paragraphs
  (license            potluck-package-license)
  (home-page          potluck-package-home-page)
  (location           potluck-package-location
                      (default (and=> (current-source-location)
                                      source-properties->location))
                      (innate)))

;; Printers.

(define (print-potluck-source potluck-source port)
  "Write a concise representation of POTLUCK-SOURCE to PORT."
  (match potluck-source
    (($ <potluck-source> git-uri git-commit sha256 snippet)
     (simple-format port "#<potluck-source ~a@~a ~a ~a>"
                    git-uri git-commit sha256
                    (number->string (object-address potluck-source) 16)))))

(define (print-potluck-package package port)
  (let ((loc    (potluck-package-location package))
        (format simple-format))
    (format port "#<potluck-package ~a@~a ~a~a>"
            (potluck-package-name package)
            (potluck-package-version package)
            (if loc
                (format #f "~a:~a "
                        (location-file loc)
                        (location-line loc))
                "")
            (number->string (object-address
                             package)
                            16))))

(set-record-type-printer! <potluck-source> print-potluck-source)
(set-record-type-printer! <potluck-package> print-potluck-package)

;; Pretty-printers.

(define* (pretty-print-potluck-source port source #:key (prefix "")
                                      (suffix "\n"))
  (let ((uri (potluck-source-git-uri source))
        (commit (potluck-source-git-commit source))
        (sha256 (potluck-source-sha256 source))
        (snippet (potluck-source-snippet source)))
    (format port "~a(potluck-source" prefix)
    (format port "\n~a  (git-uri ~s)" prefix uri)
    (format port "\n~a  (git-commit ~s)" prefix commit)
    (format port "\n~a  (sha256 ~s)" prefix sha256)
    (when snippet
      (format port "\n~a  (snippet '~s)" prefix snippet))
    (format port ")~a" suffix)))

(define* (pretty-print-potluck-package port pkg #:key (prefix ""))
  (let ((name (potluck-package-name pkg))
        (version (potluck-package-version pkg))
        (source (potluck-package-source pkg))
        (build-system (potluck-package-build-system pkg))
        (inputs (potluck-package-inputs pkg))
        (native-inputs (potluck-package-native-inputs pkg))
        (propagated-inputs (potluck-package-propagated-inputs pkg))
        (arguments (potluck-package-arguments pkg))
        (home-page (potluck-package-home-page pkg))
        (synopsis (potluck-package-synopsis pkg))
        (description (potluck-package-description pkg))
        (license (potluck-package-license pkg)))
    (format port "~a(potluck-package\n" prefix)
    (format port "~a  (name ~s)\n" prefix name)
    (format port "~a  (version ~s)\n" prefix version)
    (format port "~a  (source\n" prefix)
    (pretty-print-potluck-source port source #:prefix
                                 (string-append prefix "    ")
                                 #:suffix ")\n")
    (format port "~a  (build-system '~s)\n" prefix build-system)
    (format port "~a  (inputs '~s)\n" prefix inputs)
    (format port "~a  (native-inputs '~s)\n" prefix native-inputs)
    (format port "~a  (propagated-inputs '~s)\n" prefix propagated-inputs)
    (match arguments
      (()
       (format port "~a  (arguments '())\n" prefix))
      (arguments
       (pretty-print `(arguments ',arguments) port
                     #:per-line-prefix (format #f "~a  " prefix))))
    (format port "~a  (home-page ~s)\n" prefix home-page)
    (format port "~a  (synopsis ~s)\n" prefix synopsis)
    (format port "~a  (description ~s)\n" prefix description)
    (format port "~a  (license '~s))\n" prefix license)))

;; Safely loading potluck files.
(define (make-potluck-sandbox-module)
  "Return a fresh module that only imports the potluck environment."
  (let ((m (make-fresh-user-module)))
    (purify-module! m)
    (module-use! m (resolve-interface '(guix potluck environment)))
    m))

(define eval-in-sandbox
  (delay
    (cond
     ((false-if-exception (resolve-interface '(ice-9 sandbox)))
      => (lambda (m)
           (module-ref m 'eval-in-sandbox)))
     ((getenv "GUIX_POTLUCK_NO_SANDBOX")
      (warn "No sandbox available; be warned!!!")
      (lambda* (exp #:key time-limit allocation-limit module)
        (eval exp module)))
     (else
      (error "sandbox facility unavailable")))))

;; Because potluck package definitions come from untrusted parties, they need
;; to be sandboxed to prevent them from harming the host system.
(define* (load-potluck-package file #:key
                               (time-limit 1)
                               (allocation-limit 50e6))
  "Read a sequence of Scheme expressions from @var{file} and evaluate them in
a potluck sandbox.  The result of evaluating that expression sequence should
be a potluck package.  Any syntax error reading the expressions or run-time
error evaluating the expressions will throw an exception.  The resulting
potluck package will be validated with @code{validate-potluck-package}."
  (define (read-expressions port)
    (match (read port)
      ((? eof-object?) '())
      (exp (cons exp (read-expressions port)))))
  (call-with-input-file file
    (lambda (port)
      (let ((exp (match (read-expressions port)
                   (() (error "no expressions in file" file))
                   (exps (cons 'begin exps))))
            (mod (make-potluck-sandbox-module)))
        (call-with-values
            (lambda ()
              ((force eval-in-sandbox) exp
               #:time-limit time-limit
               #:allocation-limit allocation-limit
               #:module mod))
          (lambda vals
            (match vals
              (() (error "no return values"))
              ((val)
               (unless (potluck-package? val)
                 (error "not a potluck package" val))
               (validate-potluck-package val)
               val)
              (_ (error "too many return values" vals)))))))))

;; Editing.

(define (potluck-package-field-location package field)
  "Return the source code location of the definition of FIELD for PACKAGE, or
#f if it could not be determined."
  (define (goto port line column)
    (unless (and (= (port-column port) (- column 1))
                 (= (port-line port) (- line 1)))
      (unless (eof-object? (read-char port))
        (goto port line column))))

  (match (potluck-package-location package)
    (($ <location> file line column)
     (catch 'system
       (lambda ()
         ;; In general we want to keep relative file names for modules.
         (with-fluids ((%file-port-name-canonicalization 'relative))
           (call-with-input-file (search-path %load-path file)
             (lambda (port)
               (goto port line column)
               (match (read port)
                 (('potluck-package inits ...)
                  (let ((field (assoc field inits)))
                    (match field
                      ((_ value)
                       ;; Put the `or' here, and not in the first argument of
                       ;; `and=>', to work around a compiler bug in 2.0.5.
                       (or (and=> (source-properties value)
                                  source-properties->location)
                           (and=> (source-properties field)
                                  source-properties->location)))
                      (_
                       #f))))
                 (_
                  #f))))))
       (lambda _
         #f)))
    (_ #f)))

;; Lower potluck packages to Guix packages.

(define-condition-type &potluck-package-error &error
  potluck-package-error?
  (potluck-package potluck-package-error-potluck-package))

(define-condition-type &potluck-package-validation-error &potluck-package-error
  potluck-package-validation-error?
  (field-name potluck-package-validation-error-field-name)
  (assertion potluck-package-validation-error-assertion)
  (value potluck-package-validation-error-value))

(define (assertion-failed pkg field-name assertion value)
  (raise (condition (&potluck-package-validation-error
                     (potluck-package pkg)
                     (field-name field-name)
                     (assertion assertion)
                     (value value)))))

(define* (validate-public-uri pkg field-name str #:key (schemes '(http https)))
  (define (public-host? host)
    ;; There are other ways to spell "localhost" using raw IPv4 or IPv6
    ;; addresses; this is just a sanity check.
    (not (member host '("localhost" "127.0.0.1" "[::1]"))))
  (let ((uri (and (string? str) (string->uri str))))
    (unless (and uri
                 (memq (uri-scheme uri) schemes)
                 (not (uri-fragment uri))
                 (public-host? (uri-host uri)))
      (assertion-failed pkg field-name "public URI" str))))

(define (validate-git-commit pkg field-name commit)
  (unless (and (string? commit)
               (= (string-length commit) 40)
               (string-every (string->char-set "abcdef0123456789") commit))
    (assertion-failed pkg field-name "full git commit SHA1 hash" commit)))

(define (validate-base32-sha256 pkg field-name str)
  (unless (and (string? str)
               (= (string-length str) 52)
               (false-if-exception (nix-base32-string->bytevector str)))
    (assertion-failed pkg field-name "sha256 hash as a base32 string" str)))

(define (validate-potluck-source pkg field-name source)
  (validate-public-uri pkg field-name (potluck-source-git-uri source)
                       #:schemes '(git http https))
  (validate-git-commit pkg field-name (potluck-source-git-commit source))
  (validate-base32-sha256 pkg field-name (potluck-source-sha256 source))
  (validate-snippet pkg field-name (potluck-source-snippet source)))

(define (validate-snippet pkg field-name snippet)
  (match snippet
    (#f #t)
    ((_ ...) #t)
    (_ (assertion-failed pkg field-name "valid snippet" snippet))))

(define (validate-non-empty-string pkg field-name str)
  (unless (and (string? str)
               (not (string-null? str)))
    (assertion-failed pkg field-name "non-empty string" str)))

(define (validate-build-system pkg field-name sym)
  (unless (build-system-by-name sym)
    (assertion-failed pkg field-name "build system name as symbol" sym)))

(define (validate-package-list pkg field-name l)
  (unless (and (list? l) (and-map string? l))
    (assertion-failed pkg field-name
                      "list of package or package@version strings" l)))

(define* (validate-keyword-arguments pkg field-name l #:optional (valid-kw? (const #t)))
  (define validate-1
    (case-lambda
      (() #t)
      ((k v . rest)
       (unless (and (keyword? k) (valid-kw? k))
         (assertion-failed pkg field-name "keyword" k))
       (apply validate-1 rest))
      (_ (assertion-failed pkg field-name "keyword argument list" l))))
  (apply validate-1 l))

(define (validate-arguments pkg field-name arguments)
  (validate-keyword-arguments pkg field-name arguments))

(define (validate-synopsis pkg field-name str)
  (validate-non-empty-string pkg field-name str)
  ;; The synopsis set by "guix potluck init".
  (when (equal? str "Declarative synopsis here")
    (assertion-failed pkg field-name "updated synopsis" str)))

(define (validate-description pkg field-name str)
  (validate-non-empty-string pkg field-name str)
  ;; The description set by "guix potluck init".
  (when (string-suffix? "..." str)
    (assertion-failed pkg field-name "updated description" str)))

(define (validate-license pkg field-name sym)
  (unless (license-by-name sym)
    (assertion-failed pkg field-name "license name as symbol" sym)))

(define (validate-potluck-package pkg)
  (validate-non-empty-string pkg 'name (potluck-package-name pkg))
  (validate-non-empty-string pkg 'version (potluck-package-version pkg))
  (validate-potluck-source pkg 'source (potluck-package-source pkg))
  (validate-build-system pkg 'build-system (potluck-package-build-system pkg))
  (validate-package-list pkg 'inputs (potluck-package-inputs pkg))
  (validate-package-list pkg 'native-inputs
                         (potluck-package-native-inputs pkg))
  (validate-package-list pkg 'propagated-inputs
                         (potluck-package-propagated-inputs pkg))
  (validate-arguments pkg 'arguments (potluck-package-arguments pkg))
  (validate-public-uri pkg 'home-page (potluck-package-home-page pkg))
  (validate-synopsis pkg 'synopsis (potluck-package-synopsis pkg))
  (validate-description pkg 'description (potluck-package-description pkg))
  (validate-license pkg 'license (potluck-package-license pkg)))

(define (lower-potluck-source o)
  (let ((uri (potluck-source-git-uri o))
        (commit (potluck-source-git-commit o))
        (sha256 (potluck-source-sha256 o))
        (snippet (potluck-source-snippet o)))
    (origin
      (method git-fetch)
      (uri (git-reference
            (url uri)
            (commit commit)))
      (snippet snippet)
      (sha256 (base32 sha256)))))

(define (lower-input input)
  (call-with-values (lambda () (specification->package+output input))
    (lambda (pkg output)
      (cons* (package-name pkg) pkg
             (if (equal? output "out")
                 '()
                 (list output))))))

(define (lower-inputs inputs)
  (map lower-input inputs))

(define (lower-potluck-package pkg)
  (validate-potluck-package pkg)
  (let ((name (potluck-package-name pkg))
        (version (potluck-package-version pkg))
        (source (potluck-package-source pkg))
        (build-system (potluck-package-build-system pkg))
        (inputs (potluck-package-inputs pkg))
        (native-inputs (potluck-package-native-inputs pkg))
        (propagated-inputs (potluck-package-propagated-inputs pkg))
        (arguments (potluck-package-arguments pkg))
        (home-page (potluck-package-home-page pkg))
        (synopsis (potluck-package-synopsis pkg))
        (description (potluck-package-description pkg))
        (license (potluck-package-license pkg)))
    (package
      (name name)
      (version version)
      (source (lower-potluck-source source))
      (build-system (build-system-by-name build-system))
      (inputs (lower-inputs inputs))
      (native-inputs (lower-inputs native-inputs))
      (propagated-inputs (lower-inputs propagated-inputs))
      (arguments arguments)
      (home-page home-page)
      (synopsis synopsis)
      (description description)
      (license (license-by-name license)))))

(define (lower-potluck-package-to-module port lowered-module-name pkg)
  (let ((lowered (lower-potluck-package pkg))
        ;; specification -> exp
        (spec->binding (make-hash-table))
        ;; mod-name -> (sym ...)
        (imports (make-hash-table))
        ;; sym -> specification
        (imported-syms (make-hash-table))
        (needs-runtime-lookup? #f))
    (define (add-bindings spec)
      (unless (hash-ref spec->binding spec)
        (match (false-if-exception (lower-input spec))
          ((name pkg . outputs)
           ;; Given that we found the pkg, surely we should find its binding
           ;; also.
           (call-with-values (lambda () (find-package-binding pkg))
             (lambda (module-name sym)
               ;; Currently we import these bindings using their original
               ;; names.  We need to make sure that names don't collide.
               ;; Ideally we should also ensure that they don't collide with
               ;; other bindings that we import.
               (when (hashq-ref imported-syms sym)
                 (error "duplicate import name" sym))
               (hashq-set! imported-syms sym spec)
               (hash-set! spec->binding spec
                          `(list ,name ,sym . ,outputs))
               (hash-set! imports module-name
                          (cons sym (hash-ref imports module-name '()))))))
          (#f
           (warn "could not resolve package specification" spec)
           (call-with-values
               (lambda ()
                 (package-specification->name+version+output spec))
             (lambda (name version . outputs)
               (hash-set! spec->binding spec
                          `(list ,name (specification->package ,spec) .
                                 ,(if (equal? outputs '("out")) '() outputs)))
               (set! needs-runtime-lookup? #t)))))))

    (for-each add-bindings (potluck-package-inputs pkg))
    (for-each add-bindings (potluck-package-native-inputs pkg))
    (for-each add-bindings (potluck-package-propagated-inputs pkg))

    (format port "(define-module ~a" lowered-module-name)
    (format port "~%  #:pure")
    ;; Because we're pure, we have to import these.
    (format port "~%  #:use-module ((guile) #:select (list quote quasiquote unquote))")
    (when needs-runtime-lookup?
      (format port "~%  #:use-module ((gnu packages) #:select (specification->package))"))
    (format port "~%  #:use-module ((guix packages) #:select (package origin base32))")
    (format port "~%  #:use-module ((guix git-download) #:select (git-fetch git-reference))")
    (format port "~%  #:use-module ((guix licenses) #:select ((~a . license:~a)))"
            (potluck-package-license pkg) (potluck-package-license pkg))
    (format port "~%  #:use-module ((guix build-system ~a) #:select (~a-build-system))"
            (potluck-package-build-system pkg) (potluck-package-build-system pkg))
    (for-each (match-lambda
                ((module-name . syms)
                 (format port "~%  #:use-module (~a #:select ~a)"
                         module-name syms)))
              (hash-map->list cons imports))
    (format port ")~%")

    (format port "(package\n")
    (format port "  (name ~s)\n" (potluck-package-name pkg))
    (format port "  (version ~s)\n" (potluck-package-version pkg))
    (format port "  (source\n")

    (let ((source (potluck-package-source pkg)))
      (format port "    (origin\n")
      (format port "      (method git-fetch)\n")
      (format port "      (uri (git-reference\n")
      (format port "            (url ~s)\n" (potluck-source-git-uri source))
      (format port "            (commit ~s)))\n"
              (potluck-source-git-commit source))
      (when (potluck-source-snippet source)
        (pretty-print `(snippet ',(potluck-source-snippet source)) port
                      #:per-line-prefix "      "))
      (format port "      (sha256 (base32 ~s))))\n"
              (potluck-source-sha256 source)))

    (format port "  (build-system ~s-build-system)\n"
            (potluck-package-build-system pkg))

    (for-each
     (match-lambda
       ((name)
        ;; No inputs; do nothing.
        #t)
       ((name . specs)
        (pretty-print
         `(,name (list ,@(map (lambda (spec)
                                (or (hash-ref spec->binding spec)
                                    (error "internal error" spec)))
                              specs)))
         port #:per-line-prefix "  ")))
     `((inputs . ,(potluck-package-inputs pkg))
       (native-inputs . ,(potluck-package-native-inputs pkg))
       (propagated-inputs . ,(potluck-package-propagated-inputs pkg))))

    (match (potluck-package-arguments pkg)
      (() #t)
      (arguments
       (pretty-print `(arguments ',arguments) port #:per-line-prefix "  ")))

    (format port "  (home-page ~s)\n" (potluck-package-home-page pkg))
    (format port "  (synopsis ~s)\n" (potluck-package-synopsis pkg))
    (format port "  (description ~s)\n" (potluck-package-description pkg))
    (format port "  (license license:~s))\n" (potluck-package-license pkg))
    (force-output port)))
