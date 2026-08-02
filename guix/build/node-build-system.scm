;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 David Thompson <davet@gnu.org>
;;; Copyright © 2016, 2020 Jelle Licht <jlicht@fsfe.org>
;;; Copyright © 2019, 2021 Timothy Sample <samplet@ngyro.com>
;;; Copyright © 2021, 2022 Philip McGrath <philip@philipmcgrath.com>
;;; Copyright © 2022 Liliana Marie Prikler <liliana.prikler@gmail.com>
;;; Copyright © 2024 Daniel Khodabakhsh <d.khodabakhsh@gmail.com>
;;; Copyright © 2025 Nicolas Graves <ngraves@ngraves.fr>
;;; Copyright © 2026 Maxim Cournoyer <maxim@guixotic.coop>
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

(define-module (guix build node-build-system)
  #:use-module ((guix build gnu-build-system) #:prefix gnu:)
  #:use-module (guix deprecation)
  #:use-module (guix build json-utils)
  #:use-module (guix build utils)
  #:use-module (ice-9 format)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 match)
  #:use-module (json)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-71)
  #:export (%standard-phases
            delete-dependencies
            delete-dependencies/except
            delete-dev-dependencies
            delete-dev-dependencies/except
            node-build

            ;; The following are deprecated and have been moved to the (guix
            ;; build json-utils) module.
            ;; TODO: Remove after 2027/08.
            delete-fields
            replace-fields
            add-fields)
  ;; Also deprecated.
  #:re-export (with-atomic-json-file-replacement
               modify-json))

;;;
;;; Helpers
;;;

(define %dependency-keys
  '("devDependencies"
    "dependencies"
    "peerDependencies"
    "optionalDependencies"))

(define %dev-dependency-keys
  '("devDependencies"
    "peerDependencies"))

(define-deprecated/alias modify-json-fields
  (@ (guix build json-utils) modify-json-fields))

(define-deprecated/alias delete-fields
  (@ (guix build json-utils) delete-json-fields))

(define-deprecated/alias replace-fields
  (@ (guix build json-utils) replace-json-fields))

(define-deprecated/alias add-fields
  (@ (guix build json-utils) add-json-fields))

(define* (delete-dependencies dependencies-to-remove
                              #:key negate?
                              (dependency-keys %dependency-keys))
  "Rewrite 'package.json' to allow the build to proceed without packages
listed in 'dependencies-to-remove', a list of strings naming npm packages.  To
negate its effect, and keep DEPENDENCIES-TO-REMOVE instead of removing them,
set NEGATE? to #t.  DEPENDENCY-KEYS can be used to adjust which dependency
fields are targeted.

To prevent the deleted dependencies from being reintroduced, use this function
only after the 'patch-dependencies' phase."
  (let ((predicate (lambda (dependency)
                     (member (car dependency) dependencies-to-remove)))
        (dependency? (cut member <> dependency-keys)))
    (lambda (pkg-meta)
      (map (match-lambda
             (((? dependency? key) . dependencies)
              (let* (((values removed kept)
                      (partition ((if negate? negate identity)
                                  predicate)
                                 dependencies)))
                (unless (null? removed)
                  (format #t "deleting ~s dependencies: ~y~%" key removed))
                (cons key kept)))
             (otherwise otherwise))
           pkg-meta))))

(define* (delete-dependencies/except dependencies-to-preserve
                                     #:key (dependency-keys %dependency-keys))
  "Like `delete-dependencies', but deleting all dependencies except those
listed in DEPENDENCIES-TO-PRESERVE."
  (delete-dependencies dependencies-to-preserve #:negate? #t
                       #:dependency-keys dependency-keys))

(define* (delete-dev-dependencies/except dependencies-to-preserve)
  "Like `delete-dependencies/except' but only acting on development
dependencies."
  (delete-dependencies/except dependencies-to-preserve
                              #:dependency-keys %dev-dependency-keys))

(define (delete-dev-dependencies)
  (delete-json-fields (list "devDependencies" "peerDependencies")
                      #:strict? #f))

;;;
;;; Phases.
;;;

(define (set-home . _)
  (with-directory-excursion ".."
    (let loop ((i 0))
      (let ((dir (string-append "npm-home-" (number->string i))))
        (if (directory-exists? dir)
            (loop (1+ i))
            (begin
              (mkdir dir)
              (setenv "HOME" (string-append (getcwd) "/" dir))
              (format #t "set HOME to ~s~%" (getenv "HOME"))))))))

(define (module-name module)
  (let* ((package.json (string-append module "/package.json"))
         (package-meta (call-with-input-file package.json json->scm)))
    (assoc-ref package-meta "name")))

(define (index-modules input-paths)
  (define (list-modules directory)
    (append-map (lambda (x)
                  (if (string-prefix? "@" x)
                      (list-modules (string-append directory "/" x))
                      (list (string-append directory "/" x))))
                (filter (lambda (x)
                          (not (member x '("." ".."))))
                        (or (scandir directory) '()))))
  (let ((index (make-hash-table (* 2 (length input-paths)))))
    (for-each (lambda (dir)
                (let ((nm (string-append dir "/lib/node_modules")))
                  (for-each (lambda (module)
                              (hash-set! index (module-name module)
                                         (string-append "file://" module)))
                            (list-modules nm))))
              input-paths)
    index))

(define* (patch-dependencies #:key inputs #:allow-other-keys)
  "Replace versions by paths when found among INPUTS in `package.json'."

  (define resolve-dependencies
    (let ((index (index-modules (map cdr inputs))))
      (cut map
           (match-lambda
             ((dependency . version)
              (cons dependency (hash-ref index dependency version))))
           <>)))

  (define (resolve key getter)
    (lambda (pkg-meta)
      (assoc-set! pkg-meta key
                  (resolve-dependencies (getter pkg-meta)))))

  (modify-json "package.json"
   (resolve "devDependencies"
            (lambda (pkg-meta)
              (or (assoc-ref pkg-meta "devDependencies") '())))
   (resolve "dependencies"
            ;; Combined "peerDependencies" and "dependencies" dependencies
            ;; with "dependencies" taking precedent.
            (lambda (pkg-meta)
              (fold
               (lambda (dependency dependencies)
                 (assoc-set! dependencies
                             (car dependency) (cdr dependency)))
               (or (assoc-ref pkg-meta "peerDependencies") '())
               (or (assoc-ref pkg-meta "dependencies") '()))))))

(define* (delete-lockfiles #:key lockfiles #:allow-other-keys)
  "Delete LOCKFILES if they exist."
  (for-each (lambda (pth)
              (when (file-exists? pth)
                (delete-file pth)))
            lockfiles))

(define* (configure #:key inputs #:allow-other-keys)
  (let ((npm (string-append (assoc-ref inputs "node") "/bin/npm")))
    (invoke npm "--offline"
            "--ignore-scripts"
            "--install-links"
            "--no-audit"
            "install")))

(define* (build #:key inputs #:allow-other-keys)
  (let* ((package-meta (call-with-input-file "package.json" json->scm))
         (scripts (assoc-ref package-meta "scripts")))
    (if (and scripts (assoc-ref scripts "build"))
        (let ((npm (string-append (assoc-ref inputs "node") "/bin/npm")))
          (invoke npm "run" "build"))
        (format #t "there is no build script to run~%"))))

(define* (check #:key tests? inputs test-target #:allow-other-keys)
  "Run 'npm run TEST-TARGET' if TESTS?"
  (if tests?
      (let ((npm (string-append (assoc-ref inputs "node") "/bin/npm")))
        (invoke npm "run" test-target))
      (format #t "test suite not run~%")))

(define* (repack #:key inputs #:allow-other-keys)
  (invoke "tar"
          ;; Add options suggested by https://reproducible-builds.org/docs/archives/
          "--sort=name"
          (string-append "--mtime=@" (getenv "SOURCE_DATE_EPOCH"))
          "--owner=0"
          "--group=0"
          "--numeric-owner"
          "-czf" "../package.tgz" "."))

(define* (install #:key outputs inputs #:allow-other-keys)
  "Install the node module to the output store item."
  (let ((out (assoc-ref outputs "out"))
        (npm (string-append (assoc-ref inputs "node") "/bin/npm")))
    (invoke npm "--prefix" out
            "--global"
            "--offline"
            "--loglevel" "info"
            "--production"
            "--install-links"
            "install" "../package.tgz")))

(define* (avoid-node-gyp-rebuild #:key outputs #:allow-other-keys)
  "Adjust the installed 'package.json' to remove an 'install' script that
would try to run 'node-gyp rebuild'."
  ;; We want to take advantage of `npm install`'s automatic support for
  ;; building native addons with node-gyp: in particular, it helps us avoid
  ;; hard-coding the specifics of how npm's internal copy of node-gyp is
  ;; currently packaged. However, the mechanism by which the automatic support
  ;; is implemented causes problems for us.
  ;;
  ;; If a package contains a 'binding.gyp' file and does not define an
  ;; 'install' or 'preinstall' script, 'npm install' runs a default install
  ;; script consisting of 'node-gyp rebuild'. In our 'install' phase, this
  ;; implicit 'install' script, if it is applicable, is explicitly added to
  ;; the "package.json" file. However, if another Guix package were to use a
  ;; Node.js package with such an 'install' script, the dependent package's
  ;; build process would fail, because 'node-gyp rebuild' would try to write
  ;; to the store.
  ;;
  ;; Here, if the installed "package.json" defines scripts.install as
  ;; "node-gyp rebuild", we replace it with a no-op. Importantly, deleting the
  ;; install script definition would not be enough, because the default
  ;; install script would cause the same problem.
  ;;
  ;; For further details, see:
  ;; - https://docs.npmjs.com/cli/v8/configuring-npm/package-json#default-values
  ;; - https://docs.npmjs.com/cli/v8/using-npm/scripts#best-practices
  (let* ((installed-package.json
          (search-input-file outputs (string-append "/lib/node_modules/"
                                                    (module-name ".")
                                                    "/package.json")))
         ;; We don't want to use an atomic replacement here, because we often
         ;; don't even need to overwrite this file.  Therefore, let's use some
         ;; helpers that we'd otherwise not need.
         (pkg-meta (call-with-input-file installed-package.json json->scm))
         (scripts (or (assoc-ref pkg-meta "scripts") '())))
    (when (equal? "node-gyp rebuild" (assoc-ref scripts "install"))
      (call-with-output-file installed-package.json
        (lambda (out)
          (scm->json
           (assoc-set! pkg-meta
                       "scripts"
                       (assoc-set! scripts
                                   "install"
                                   "echo Guix: avoiding node-gyp rebuild"))
           out
           #:pretty #t))))))

(define %standard-phases
  (modify-phases gnu:%standard-phases
    (add-after 'unpack 'set-home set-home)
    (add-before 'configure 'patch-dependencies patch-dependencies)
    (add-after 'patch-dependencies 'delete-lockfiles delete-lockfiles)
    (replace 'configure configure)
    (replace 'build build)
    (replace 'check check)
    (add-before 'install 'repack repack)
    (replace 'install install)
    (add-after 'install 'avoid-node-gyp-rebuild avoid-node-gyp-rebuild)))

(define* (node-build #:key inputs (phases %standard-phases)
                     #:allow-other-keys #:rest args)
  (apply gnu:gnu-build #:inputs inputs #:phases phases args))
