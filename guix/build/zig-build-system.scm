;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2023 Ekaitz Zarraga <ekaitz@elenq.tech>
;;; Copyright © 2024 Hilton Chain <hako@ultrarare.space>
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

(define-module (guix build zig-build-system)
  #:use-module ((guix build gnu-build-system) #:prefix gnu:)
  #:use-module (guix build utils)
  #:use-module (guix build zig-utils)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (%standard-phases
            zig-build
            zig-source-install-path))

;; Interesting guide here:
;; https://github.com/riverwm/river/blob/master/PACKAGING.md

(define (zig-source-install-path output)
  (string-append output "/src/zig/" (strip-store-file-name output)))

(define (zig-input-install-path input)
  (zig-source-install-path
   (dirname (dirname (dirname (canonicalize-path input))))))

(define (zig-arguments)
  (define version-major+minor
    (let* ((port (open-input-pipe "zig version"))
           (str  (read-line port)))
      (close-pipe port)
      (take (string-split str #\.) 2)))
  (define (version>=? a b-major b-minor)
    (let ((a-major (string->number (first a)))
          (a-minor (string->number (second a))))
      (or (> a-major b-major)
          (and (= a-major b-major)
               (>= a-minor b-minor)))))
  `(("parallel-jobs" .
     ,(lambda (jobs)
        (cond
         ((version>=? version-major+minor 0 11)
          (list (string-append "-j" (number->string jobs))))
         (else '()))))
    ("release-type" .
     ,(lambda (type)
        (cond
         ((version>=? version-major+minor 0 11)
          (list (string-append "--release=" type)))
         (else
          (list (string-append "-Drelease-" type))))))))

;; Notes on Zig package manager (`build.zig.zon')
;; 1. Dependency definition (name -> URL + hash)
;; - Dependency names are not necessarily consistent across packages.
;; - `zig fetch <url> --name=<name>' fetches <url> to Zig cache, computes its
;; hash, then overwrites dependency <name> with <url> and the computed hash.
;; 2. Dependency lookup
;; Lookup hash in Zig cache, fetch URL when missing.
;; 3. Recursive dependency is possible
;; `build.zig.zon' -> dependency (`build.zig.zon' -> next dependency).
;;
;; With our naming convention and different expectation on package sources,
;; we'll need to change dependency names and hashes for nearly every Zig
;; package.
;; - `rename-zig-dependencies' in `(gnu packages zig)' takes care of names.
;; - `unpack-dependencies' below is for hashes.  Ideally we can parse
;; `build.zig.zon' and only `zig fetch' required dependencies for current
;; package, this way recursive dependencies are resolved by fetching store paths
;; defined in dependency URLs.  However Zig package manager currently doesn't
;; support file URLs[0] so we are instead fetching all dependencies needed for
;; the build process and all of them are added to involved `build.zig.zon' as a
;; side effect.
;; [0]: https://github.com/ziglang/zig/pull/21931
(define (unpack-dependencies . _)
  "Unpack Zig dependencies from GUIX_ZIG_PACKAGE_PATH."
  (define zig-inputs
    (append-map
     (lambda (directory)
       (map (lambda (input-name)
              (cons input-name
                    (in-vicinity directory input-name)))
            (scandir directory (negate (cut member <> '("." ".."))))))
     (or (and=> (getenv "GUIX_ZIG_PACKAGE_PATH")
                (cut string-split <> #\:))
         '())))
  (define (strip-version input)
    (let* ((name+version (string-split input #\-))
           (penult (first (take-right name+version 2)))
           (ultima (last name+version)))
      (string-join
       (drop-right name+version
                   (cond
                    ;; aaa-0.0.0
                    ((= 3 (length (string-split ultima #\.)))
                     1)
                    ;; bbb-0.0.0-0.0000000-output
                    ((and (= 2 (length (string-split penult #\.)))
                          (not (string-contains ultima ".")))
                     3)
                    ;; ccc-0.0.0-output
                    ;; ddd-0.0.0-0.0000000
                    (else 2)))
       "-")))
  (for-each
   (lambda (build.zig.zon)
     (format #t "unpacking dependencies for ~s...~%" build.zig.zon)
     (with-directory-excursion (dirname build.zig.zon)
       (false-if-exception
        (for-each
         (match-lambda
           ((input-name . input-path)
            (let ((call `("zig" "fetch"
                          ,(zig-input-install-path input-path)
                          ,(string-append "--save=" (strip-version input-name)))))
              (format #t "running: ~s~%" call)
              (apply invoke call))))
         (reverse zig-inputs)))))
   (find-files "." "^build\\.zig\\.zon$")))

(define* (build #:key
                zig-build-flags
                zig-build-target
                ;; "safe", "fast" or "small", empty for a "debug" build.
                zig-release-type
                parallel-build?
                skip-build?
                #:allow-other-keys)
  "Build a given Zig package."
  (when (not skip-build?)
    (setenv "DESTDIR" "out")
    (let* ((arguments (zig-arguments))
           (call `("zig" "build"
                   "--prefix"             ""            ;; Don't add /usr
                   "--prefix-lib-dir"     "lib"
                   "--prefix-exe-dir"     "bin"
                   "--prefix-include-dir" "include"
                   "--verbose"
                   ,(string-append "-Dtarget=" (zig-target zig-build-target))
                   ,@(if parallel-build?
                         ((assoc-ref arguments "parallel-jobs")
                          (parallel-job-count))
                         ((assoc-ref arguments "parallel-jobs")
                          1))
                   ,@(if zig-release-type
                         ((assoc-ref arguments "release-type")
                          zig-release-type)
                         '())
                   ,@zig-build-flags)))
      (format #t "running: ~s~%" call)
      (apply invoke call))))

(define* (check #:key tests?
                test-target
                zig-test-flags
                target
                parallel-tests?
                #:allow-other-keys)
  "Run all the tests"
  (when (and tests? (not target))
    (let ((old-destdir (getenv "DESTDIR")))
      (setenv "DESTDIR" "test-out") ;; Avoid colisions with the build output
      (let* ((arguments (zig-arguments))
             (call `("zig" "build" ,(or test-target "test") "--verbose"
                     ,@(if parallel-tests?
                           ((assoc-ref arguments "parallel-jobs")
                            (parallel-job-count))
                           ((assoc-ref arguments "parallel-jobs")
                            1))
                     ,@zig-test-flags)))
        (format #t "running: ~s~%" call)
        (apply invoke call))
      (if old-destdir
        (setenv "DESTDIR" old-destdir)
        (unsetenv "DESTDIR")))))

(define* (install #:key outputs install-source? #:allow-other-keys)
  "Install a given Zig package."
  (let* ((out (assoc-ref outputs "out"))
         (source-install-path (zig-source-install-path out)))
    (when (file-exists? "out")
      (copy-recursively "out" out)
      (delete-file-recursively "out"))
    (when install-source?
      (mkdir-p source-install-path)
      (copy-recursively "." source-install-path))))

(define %standard-phases
  (modify-phases gnu:%standard-phases
    (delete 'bootstrap)
    (replace 'configure zig-configure)
    (add-after 'configure 'unpack-dependencies unpack-dependencies)
    (replace 'build build)
    (replace 'check check)
    (replace 'install install)))


(define* (zig-build #:key inputs (phases %standard-phases)
                      #:allow-other-keys #:rest args)
  "Build the given Zig package, applying all of PHASES in order."
  (apply gnu:gnu-build #:inputs inputs #:phases phases args))
