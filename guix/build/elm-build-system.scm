;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2022 Philip McGrath <philip@philipmcgrath.com>
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

(define-module (guix build elm-build-system)
  #:use-module ((guix build gnu-build-system) #:prefix gnu:)
  #:use-module (guix build utils)
  #:use-module (guix build json)
  #:use-module (guix build union)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 vlist)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-71)
  #:export (%standard-phases
            patch-application-dependencies
            patch-json-string-escapes
            read-offline-registry->vhash
            elm-build))

;;; Commentary:
;;;
;;; Elm draws a sharp distinction between "projects" with `{"type":"package"}`
;;; vs. `{"type":"application"}` in the "elm.json" file: see
;;; <https://github.com/elm/compiler/blob/master/docs/elm.json/package.md> and
;;; <https://github.com/elm/compiler/blob/master/docs/elm.json/application.md>.
;;; For now, `elm-build-system` is designed for "package"s: packaging
;;; "application"s requires ad-hoc replacements for some phases---but see
;;; `patch-application-dependencies`, which helps to work around a known issue
;;; discussed below.  It would be nice to add more streamlined support for
;;; "application"s one we have more experience building them in Guix.  For
;;; example, we could incorporate the `uglifyjs` advice from
;;; <https://github.com/elm/compiler/blob/master/hints/optimize.md>.
;;;
;;; We want building an Elm "package" to produce:
;;;
;;;   - a "docs.json" file with extracted documentation; and
;;;
;;;   - an "artifacts.dat" file with compilation results for use in building
;;;     "package"s and "application"s.
;;;
;;; Unfortunately, there isn't an entry point to the Elm compiler that builds
;;; those files directly.  Building with `elm make` does something different,
;;; more oriented toward development, testing, and building "application"s.
;;; We work around this limitation by staging the "package" we're building as
;;; though it were already installed in ELM_HOME, generating a trivial Elm
;;; "application" that depends on the "package", and building the
;;; "application", which causes the files for the "package" to be built.
;;;
;;; Much of the ceremony involved is to avoid using `elm` in ways that would
;;; make it try to do network IO beyond the bare minimum functionality for
;;; which we've patched a replacement into our `elm`.  On the other hand, we
;;; get to take advantage of the very regular structure required of Elm
;;; packages.
;;;
;;; *Known issue:* Elm itself supports multiple versions of "package"s
;;; coexisting simultaneously under ELM_HOME, but we do not support this yet.
;;; Sometimes, parallel versions coexisting causes `elm` to try to write to
;;; built "artifacts.dat" files.  For now, two workarounds are possible:
;;;
;;;  - Use `patch-application-dependencies` to rewrite an "application"'s
;;;    "elm.json" file to refer to the versions of its inputs actually
;;;    packaged in Guix.
;;;
;;;  - Use a Guix package transformation to rewrite your "application"'s
;;;    dependencies recursively, so that only one version of each Elm
;;;    "package" is included in your "application"'s build environment.
;;;
;;; Patching `elm` more extensively---perhaps adding an `elm guix`
;;; subcommand`---might let us address these issues more directly.
;;;
;;; Code:
;;;

(define %essential-elm-packages
  ;; elm/json isn't essential in a fundamental sense,
  ;; but it's required for a {"type":"application"},
  ;; which we are generating to trigger the build
  '("elm/core" "elm/json"))

(define* (target-elm-version #:optional elm)
  "Return the version of ELM or whichever 'elm' is in $PATH.
Return #false if it cannot be determined."
  (let* ((pipe (open-pipe* OPEN_READ
                           (or elm "elm")
                           "--version"))
         (line (read-line pipe)))
    (and (zero? (close-pipe pipe))
         (string? line)
         line)))

(define* (prepare-elm-home #:key native-inputs inputs #:allow-other-keys)
  "Set the ELM_HOME environment variable and populate the indicated directory
with the union of the Elm \"package\" inputs.  Also, set GUIX_ELM_VERSION to
the version of the Elm compiler in use."
  (let* ((elm (search-input-file (or native-inputs inputs) "/bin/elm"))
         (elm-version (target-elm-version elm)))
    (setenv "GUIX_ELM_VERSION" elm-version)
    (mkdir "../elm-home")
    (with-directory-excursion "../elm-home"
      (union-build elm-version
                   (search-path-as-list
                    (list (string-append "share/elm/" elm-version))
                    (map cdr inputs))
                   #:create-all-directories? #t)
      (setenv "ELM_HOME" (getcwd)))))

(define* (stage #:key native-inputs inputs  #:allow-other-keys)
  "Extract the installable files from the Elm \"package\" into a staging
directory and link it into the ELM_HOME tree.  Also, set GUIX_ELM_PKG_NAME and
GUIX_ELM_PKG_VERSION to the name and version, respectively, of the Elm package
being built, as defined in its \"elm.json\" file."
  (let* ((elm-version (getenv "GUIX_ELM_VERSION"))
         (elm-home (getenv "ELM_HOME"))
         (info (match (call-with-input-file "elm.json" read-json)
                 (('@ . alist) alist)))
         (name (assoc-ref info "name"))
         (version (assoc-ref info "version"))
         (rel-dir (string-append elm-version "/packages/" name "/" version))
         (staged-dir (string-append elm-home "/../staged/" rel-dir)))
    (setenv "GUIX_ELM_PKG_NAME" name)
    (setenv "GUIX_ELM_PKG_VERSION" version)
    (mkdir-p staged-dir)
    (mkdir-p (string-append elm-home "/" (dirname rel-dir)))
    (symlink staged-dir
             (string-append elm-home "/" rel-dir))
    (copy-recursively "src" (string-append staged-dir "/src"))
    (install-file "elm.json" staged-dir)
    (install-file "README.md" staged-dir)
    (when (file-exists? "LICENSE")
      (install-file "LICENSE" staged-dir))))

(define (patch-json-string-escapes file)
  "Work around a bug in the Elm compiler's JSON parser by attempting to
replace REVERSE-SOLIDUS--SOLIDUS escape sequences in FILE with unescaped
SOLIDUS characters."
  ;; https://github.com/elm/compiler/issues/2255
  (substitute* file
    (("\\\\/")
     "/")))

(define (directory-list dir)
  "Like DIRECTORY-LIST from 'racket/base': lists the contents of DIR, not
including the special \".\" and \"..\" entries."
  (scandir dir (lambda (f)
                 (not (member f '("." ".."))))))

(define* (make-offline-registry-file #:key inputs #:allow-other-keys)
  "Generate an \"offline-package-registry.json\" file and set
GUIX_ELM_OFFLINE_REGISTRY_FILE to its path, cooperating with a patch to `elm`
to avoid attempting to download a list of all published Elm package names and
versions from the internet."
  (let* ((elm-home (getenv "ELM_HOME"))
         (elm-version (getenv "GUIX_ELM_VERSION"))
         (registry-file
          (string-append elm-home "/../offline-package-registry.json"))
         (registry-alist
          ;; here, we don't need to look up entries, so we build the
          ;; alist directly, rather than using a vhash
          (with-directory-excursion
              (string-append elm-home "/" elm-version "/packages")
            (append-map (lambda (org)
                          (with-directory-excursion org
                            (map (lambda (repo)
                                   (cons (string-append org "/" repo)
                                         (directory-list repo)))
                                 (directory-list "."))))
                        (directory-list ".")))))
    (call-with-output-file registry-file
      (lambda (out)
        (write-json `(@ ,@registry-alist) out)))
    (patch-json-string-escapes registry-file)
    (setenv "GUIX_ELM_OFFLINE_REGISTRY_FILE" registry-file)))

(define (read-offline-registry->vhash)
  "Return a vhash mapping Elm \"package\" names to lists of available version
strings."
  (alist->vhash
   (match (call-with-input-file (getenv "GUIX_ELM_OFFLINE_REGISTRY_FILE")
            read-json)
     (('@ . alist) alist))))

(define (find-indirect-dependencies registry-vhash root-pkg root-version)
  "Return the recursive dependencies of ROOT-PKG, an Elm \"package\" name, at
version ROOT-VERSION as an alist mapping Elm \"package\" names to (single)
versions.  The resulting alist will not include entries for
%ESSENTIAL-ELM-PACKAGES or for ROOT-PKG itself.  The REGISTRY-VHASH is used in
conjunction with the ELM_HOME environment variable to find dependencies."
  (with-directory-excursion
      (string-append (getenv "ELM_HOME")
                     "/" (getenv "GUIX_ELM_VERSION")
                     "/packages")
    (define (get-dependencies pkg version acc)
      (let* ((elm-json-alist
              (match (call-with-input-file
                         (string-append pkg "/" version "/elm.json")
                       read-json)
                (('@ . alist) alist)))
             (deps-alist
              (match (assoc-ref elm-json-alist "dependencies")
                (('@ . alist) alist)))
             (deps-names
              (filter-map (match-lambda
                            ((name . range)
                             (and (not (member name %essential-elm-packages))
                                  name)))
                          deps-alist)))
        (fold register-dependency acc deps-names)))
    (define (register-dependency pkg acc)
      ;; Using vhash-cons unconditionally would add duplicate entries,
      ;; which would then cause problems when we must emit JSON.
      ;; Plus, we can avoid needlessly duplicating work.
      (if (vhash-assoc pkg acc)
          acc
          (match (vhash-assoc pkg registry-vhash)
            ((_ version . _)
             ;; in the rare case that multiple versions are present,
             ;; just picking an arbitrary one seems to work well enough for now
             (get-dependencies pkg version (vhash-cons pkg version acc))))))
    (vlist->list
     (get-dependencies root-pkg root-version vlist-null))))

(define* (patch-application-dependencies #:key inputs #:allow-other-keys)
  "Rewrites the \"elm.json\" file in the working directory---which must be of
`\"type\":\"application\"`, not `\"type\":\"package\"`---to refer to the
dependency versions actually provided via Guix.  The
GUIX_ELM_OFFLINE_REGISTRY_FILE environment variable is used to find available
versions."
  (let* ((registry-vhash (read-offline-registry->vhash))
         (rewrite-dep-version
          (match-lambda
            ((name . _)
             (cons name (match (vhash-assoc name registry-vhash)
                          ((_ version) ;; no dot
                           version))))))
         (rewrite-direct/indirect
          (match-lambda
            ;; a little checking to avoid confusing misuse with "package"
            ;; project dependencies, which have a different shape
            (((and key (or "direct" "indirect"))
              '@ . alist)
             `(,key @ ,@(map rewrite-dep-version alist)))))
         (rewrite-json-section
          (match-lambda
            (((and key (or "dependencies" "test-dependencies"))
              '@ . alist)
             `(,key @ ,@(map rewrite-direct/indirect alist)))
            ((k . v)
             (cons k v))))
         (rewrite-elm-json
          (match-lambda
            (('@ . alist)
             `(@ ,@(map rewrite-json-section alist))))))
    (with-atomic-file-replacement "elm.json"
      (lambda (in out)
        (write-json (rewrite-elm-json (read-json in))
                    out)))
    (patch-json-string-escapes "elm.json")))

(define* (configure #:key native-inputs inputs #:allow-other-keys)
  "Generate a trivial Elm \"application\" with a direct dependency on the Elm
\"package\" currently being built."
  (let* ((info (match (call-with-input-file "elm.json" read-json)
                 (('@ . alist) alist)))
         (name (getenv "GUIX_ELM_PKG_NAME"))
         (version (getenv "GUIX_ELM_PKG_VERSION"))
         (elm-home (getenv "ELM_HOME"))
         (registry-vhash (read-offline-registry->vhash))
         (app-dir (string-append elm-home "/../fake-app")))
    (mkdir-p (string-append app-dir "/src"))
    (with-directory-excursion app-dir
      (call-with-output-file "elm.json"
        (lambda (out)
          (write-json
           `(@ ("type" . "application")
               ("source-directories" "src") ;; intentionally no dot
               ("elm-version" . ,(getenv "GUIX_ELM_VERSION"))
               ("dependencies"
                @ ("direct"
                   @ ,@(map (lambda (pkg)
                              (match (vhash-assoc pkg registry-vhash)
                                ((_ pkg-version . _)
                                 (cons pkg
                                       (if (equal? pkg name)
                                           version
                                           pkg-version)))))
                            (if (member name %essential-elm-packages)
                                %essential-elm-packages
                                (cons name %essential-elm-packages))))
                  ("indirect"
                   @ ,@(if (member name %essential-elm-packages)
                           '()
                           (find-indirect-dependencies registry-vhash
                                                       name
                                                       version))))
               ("test-dependencies"
                @ ("direct" @)
                  ("indirect" @)))
           out)))
      (patch-json-string-escapes  "elm.json")
      (with-output-to-file "src/Main.elm"
        ;; the most trivial possible elm program
        (lambda ()
          (display "module Main exposing (..)
main : Program () () ()
main = Platform.worker
 { init = \\_ -> ( (), Cmd.none )
 , update = \\_ -> \\_ -> ( (), Cmd.none )
 , subscriptions = \\_ -> Sub.none }"))))))

(define* (build #:key native-inputs inputs #:allow-other-keys)
  "Run `elm make` to build the Elm \"application\" generated by CONFIGURE."
  (with-directory-excursion (string-append (getenv "ELM_HOME") "/../fake-app")
    (invoke (search-input-file (or native-inputs inputs) "/bin/elm")
            "make"
            "src/Main.elm")))

(define* (check #:key tests? #:allow-other-keys)
  "Does nothing, because the `elm-test` executable has not yet been packaged
for Guix."
  (when tests?
    (display "elm-test has not yet been packaged for Guix\n")))

(define* (install #:key outputs #:allow-other-keys)
  "Installs the contents of the directory generated by STAGE, including any
files added by BUILD, to the Guix package output."
  (copy-recursively
   (string-append (getenv "ELM_HOME") "/../staged")
   (string-append (assoc-ref outputs "out") "/share/elm")))

(define* (validate-compiled #:key outputs #:allow-other-keys)
  "Checks that the files \"artifacts.dat\" and \"docs.json\" have been
installed."
  (let ((base (string-append "/share/elm/"
                             (getenv "GUIX_ELM_VERSION")
                             "/packages/"
                             (getenv "GUIX_ELM_PKG_NAME")
                             "/"
                             (getenv "GUIX_ELM_PKG_VERSION")))
        (expected '("artifacts.dat" "docs.json")))
    (for-each (lambda (name)
                (search-input-file outputs (string-append base "/" name)))
              expected)))

(define %standard-phases
  (modify-phases gnu:%standard-phases
    (add-after 'unpack 'prepare-elm-home prepare-elm-home)
    (delete 'bootstrap)
    (add-after 'patch-source-shebangs 'stage stage)
    (add-after 'stage 'make-offline-registry-file make-offline-registry-file)
    (replace 'configure configure)
    (delete 'patch-generated-file-shebangs)
    (replace 'build build)
    (replace 'check check)
    (replace 'install install)
    (add-before 'validate-documentation-location 'validate-compiled
      validate-compiled)))

(define* (elm-build #:key inputs (phases %standard-phases)
                    #:allow-other-keys #:rest args)
  "Builds the given Elm project, applying all of the PHASES in order."
  (apply gnu:gnu-build #:inputs inputs #:phases phases args))
