;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 David Thompson <davet@gnu.org>
;;; Copyright © 2016, 2020 Jelle Licht <jlicht@fsfe.org>
;;; Copyright © 2019, 2021 Timothy Sample <samplet@ngyro.com>
;;; Copyright © 2021, 2022 Philip McGrath <philip@philipmcgrath.com>
;;; Copyright © 2022 Liliana Marie Prikler <liliana.prikler@gmail.com>
;;; Copyright © 2024 Daniel Khodabakhsh <d.khodabakhsh@gmail.com>
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
  #:use-module (guix build utils)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 match)
  #:use-module (json)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-71)
  #:export (%standard-phases
            with-atomic-json-file-replacement
            delete-dependencies
            node-build))

(define* (with-atomic-json-file-replacement proc
  #:optional (file "package.json"))
  "Like 'with-atomic-file-replacement', but PROC is called with a single
argument---the result of parsing FILE's contents as json---and should a value
to be written as json to the replacement FILE."
  (with-atomic-file-replacement file
    (lambda (in out)
      (scm->json (proc (json->scm in)) out))))

(define* (assoc-ref* alist key #:optional default)
  "Like assoc-ref, but return DEFAULT instead of #f if no value exists."
  (match (assoc key alist)
    (#f default)
    ((_ . value) value)))

(define* (alist-pop alist key #:optional (= equal?))
  "Return two values, the first pair in ALIST with key KEY, and the other
elements.  Equality calls are made as (= KEY ALISTCAR)."
  (define (found? pair)
    (= key (car pair)))

  (let ((before after (break found? alist)))
    (if (pair? after)
        (values (car after) (append before (cdr after)))
        (values #f before))))

(define* (alist-update alist key proc #:optional (= equal?))
  "Return an association list like ALIST, but with KEY mapped to the result of
PROC applied to the first value found under the comparison (= KEY ALISTCAR).
If no such value exists, return the list unchanged.
Unlike acons, this removes the previous association of KEY (assuming it is
unique), but the result may still share storage with ALIST."
  (let ((pair rest (alist-pop alist key =)))
    (if (pair? pair)
      (acons key (proc (cdr pair)) rest)
      alist)))

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
              (format #t "set HOME to ~s~%" (getenv "HOME")))))))
  #t)

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
                              (hash-set! index (module-name module) module))
                            (list-modules nm))))
              input-paths)
    index))

(define* (patch-dependencies #:key inputs #:allow-other-keys)

  (define index (index-modules (map cdr inputs)))

  (define (resolve-dependencies dependencies)
    (map
      (match-lambda
        ((dependency . version)
          (cons dependency (hash-ref index dependency version))))
      dependencies))

  (with-atomic-json-file-replacement
    (lambda (pkg-meta)
      (fold
        (lambda (proc pkg-meta) (proc pkg-meta))
        pkg-meta
        (list
          (lambda (pkg-meta)
            (alist-update pkg-meta "devDependencies" resolve-dependencies))
          (lambda (pkg-meta)
            (assoc-set!
              pkg-meta
              "dependencies"
              (resolve-dependencies
                ; Combined "peerDependencies" and "dependencies" dependencies
                ; with "dependencies" taking precedent.
                (fold
                  (lambda (dependency dependencies)
                    (assoc-set! dependencies (car dependency) (cdr dependency)))
                  (assoc-ref* pkg-meta "peerDependencies" '())
                  (assoc-ref* pkg-meta "dependencies" '())))))))))
  #t)

(define (delete-dependencies dependencies-to-remove)
  "Rewrite 'package.json' to allow the build to proceed without packages
listed in 'dependencies-to-remove', a list of strings naming npm packages.

To prevent the deleted dependencies from being reintroduced, use this function
only after the 'patch-dependencies' phase."
  (with-atomic-json-file-replacement
    (lambda (pkg-meta)
      (fold
        (lambda (dependency-key pkg-meta)
          (alist-update
            pkg-meta
            dependency-key
            (lambda (dependencies)
              (remove
                (lambda (dependency)
                  (member (car dependency) dependencies-to-remove))
                dependencies))))
        pkg-meta
        (list
          "devDependencies"
          "dependencies"
          "peerDependencies"
          "optionalDependencies")))))

(define* (delete-lockfiles #:key inputs #:allow-other-keys)
  "Delete 'package-lock.json', 'yarn.lock', and 'npm-shrinkwrap.json', if they
exist."
  (for-each (lambda (pth)
              (when (file-exists? pth)
                (delete-file pth)))
            '("package-lock.json"
              "yarn.lock"
              "npm-shrinkwrap.json"))
  #t)

(define* (configure #:key outputs inputs #:allow-other-keys)
  (let ((npm (string-append (assoc-ref inputs "node") "/bin/npm")))
    (invoke npm "--offline" "--ignore-scripts" "--install-links" "install")
    #t))

(define* (build #:key inputs #:allow-other-keys)
  (let ((package-meta (call-with-input-file "package.json" json->scm)))
    (if (assoc-ref* (assoc-ref* package-meta "scripts" '()) "build" #f)
        (let ((npm (string-append (assoc-ref inputs "node") "/bin/npm")))
          (invoke npm "run" "build"))
        (format #t "there is no build script to run~%"))
    #t))

(define* (check #:key tests? inputs #:allow-other-keys)
  "Run 'npm test' if TESTS?"
  (if tests?
      (let ((npm (string-append (assoc-ref inputs "node") "/bin/npm")))
        (invoke npm "test"))
      (format #t "test suite not run~%"))
  #t)

(define* (repack #:key inputs #:allow-other-keys)
  (invoke "tar"
          ;; Add options suggested by https://reproducible-builds.org/docs/archives/
          "--sort=name"
          (string-append "--mtime=@" (getenv "SOURCE_DATE_EPOCH"))
          "--owner=0"
          "--group=0"
          "--numeric-owner"
          "-czf" "../package.tgz" ".")
  #t)

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
            "install" "../package.tgz")
    #t))

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
  (define installed-package.json
    (search-input-file outputs (string-append "/lib/node_modules/"
                                              (module-name ".")
                                              "/package.json")))
  ;; We don't want to use an atomic replacement here, because we often don't
  ;; even need to overwrite this file.  Therefore, let's use some helpers
  ;; that we'd otherwise not need.
  (define pkg-meta
    (call-with-input-file installed-package.json json->scm))
  (define scripts
    (assoc-ref* pkg-meta "scripts" '()))

  (when (equal? "node-gyp rebuild" (assoc-ref* scripts "install" #f))
    (call-with-output-file installed-package.json
      (lambda (out)
        (scm->json
          (assoc-set! pkg-meta
                      "scripts"
                      (assoc-set! scripts
                                  "install"
                                  "echo Guix: avoiding node-gyp rebuild"))
          out)))))

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
