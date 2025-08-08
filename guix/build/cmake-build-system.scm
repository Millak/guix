;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2018 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013 Cyril Roelandt <tipecaml@gmail.com>
;;; Copyright © 2014, 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2017 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2024 Greg Hogan <code@greghogan.com>
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

(define-module (guix build cmake-build-system)
  #:use-module ((guix build gnu-build-system) #:prefix gnu:)
  #:use-module (guix build utils)
  #:use-module (ice-9 match)
  #:use-module (ice-9 threads)
  #:use-module (srfi srfi-34)
  #:export (%standard-phases
            cmake-build))

;; Commentary:
;;
;; Builder-side code of the standard cmake build procedure.
;;
;; Code:

(define* (configure #:key outputs (configure-flags '()) (out-of-source? #t)
                    build-type target generator (tests? #t)
                    #:allow-other-keys)
  "Configure the given package."
  (let* ((out        (assoc-ref outputs "out"))
         (abs-srcdir (getcwd))
         (srcdir     (if out-of-source?
                         (string-append "../" (basename abs-srcdir))
                         ".")))
    (format #t "source directory: ~s (relative from build: ~s)~%"
            abs-srcdir srcdir)
    (when out-of-source?
      (mkdir "../build")
      (chdir "../build"))
    (format #t "build directory: ~s~%" (getcwd))

    (call-with-temporary-output-file
      (lambda (temp port)
        (let ((args `(,srcdir
                      ;; Load variables into the the cache to prevent
                      ;; warnings about unused manually-specified variables.
                      ,(string-append "-C " temp)
                      ,@(if generator
                            (list (string-append "-G" generator))
                            '())
                      ,@configure-flags)))

          (define save-to-cache
            (lambda* (name value)
              ;; <type> and <docstring> arguments are used only by CMake GUIs.
              (format port "set(~a \"~a\" CACHE STRING \"\")~%" name value)))

          (if build-type
              (save-to-cache "CMAKE_BUILD_TYPE" build-type))
          (save-to-cache "CMAKE_INSTALL_PREFIX" out)
          ;; Ensure that the libraries are installed into /lib.
          (save-to-cache "CMAKE_INSTALL_LIBDIR" "lib")
          ;; Add input libraries to rpath.
          (save-to-cache "CMAKE_INSTALL_RPATH_USE_LINK_PATH" "TRUE")
          ;; Add (other) libraries of the project itself to rpath.
          (save-to-cache "CMAKE_INSTALL_RPATH" (string-append out "/lib"))
          ;; Enable verbose output from builds.
          (save-to-cache "CMAKE_VERBOSE_MAKEFILE" "ON")
          ;; Enable colored compiler diagnostics.
          (save-to-cache "CMAKE_COLOR_DIAGNOSTICS" "ON")
          ;; BUILD_TESTING in an option of CMake's CTest module.
          (save-to-cache "BUILD_TESTING" (if tests? "ON" "OFF"))

          (close-port port)
          (format #t "running 'cmake' with arguments ~s~%" args)
          (apply invoke "cmake" args))))))

(define* (build #:key (make-flags '()) (parallel-build? #t)
                #:allow-other-keys)
  (apply invoke "cmake"
         `("--build"
           "."
           ,@(if parallel-build?
                 `("-j" ,(number->string (parallel-job-count)))
                 ;; When unset CMake defers to the build system.
                 '("-j" "1"))
           ;; Pass the following options to the native tool.
           "--"
           ,@(if parallel-build?
                 ;; Set load average limit for Make and Ninja.
                 `("-l" ,(number->string (total-processor-count)))
                 '())
           ,@make-flags)))

(define %test-suite-log-regexp
  ;; Name of test suite log files as commonly found in CMake.
  "^LastTest\\.log$")

(define* (check #:key (tests? #t) (test-exclude "")
                (parallel-tests? #t)
                (test-repeat-until-pass? #t)
                (test-repeat-until-pass-count 5)
                (test-suite-log-regexp %test-suite-log-regexp)
                #:allow-other-keys)
  (if tests?
      (guard (c ((invoke-error? c)
                 ;; Dump the test suite log to facilitate debugging.
                 (display "\nTest suite failed, dumping logs.\n"
                          (current-error-port))
                 (gnu:dump-file-contents "." test-suite-log-regexp)
                 (raise c)))
        (apply invoke "ctest" "--output-on-failure" "--no-tests=error"
               `(,@(if (string-null? test-exclude)
                       '()
                       `("--exclude-regex" ,test-exclude))
                 ,@(if parallel-tests?
                       `("-j" ,(number->string (parallel-job-count))
                         ;; ctest waits to start tests when the CPU load is
                         ;; above this threshold. Set a lower bound for low-core
                         ;; machines to prevent stalling as may occur due to
                         ;; system tasks even when no builds are running.
                         "--test-load"
                         ,(number->string (max 4 (total-processor-count))))
                       ;; When unset CMake defers to the build system.
                       '("-j" "1"))
                 ,@(if test-repeat-until-pass?
                       `("--repeat"
                         ,(string-append "until-pass:"
                                         (number->string test-repeat-until-pass-count)))
                       '()))))
      (format #t "test suite not run~%")))

(define* (install #:rest args)
  (invoke "cmake" "--install" "."))

(define %standard-phases
  (modify-phases gnu:%standard-phases
    (delete 'bootstrap)
    (replace 'build build)
    (replace 'check check)
    (replace 'configure configure)
    (replace 'install install)))

(define* (cmake-build #:key inputs (phases %standard-phases)
                      #:allow-other-keys #:rest args)
  "Build the given package, applying all of PHASES in order."
  (apply gnu:gnu-build #:inputs inputs #:phases phases args))

;;; cmake-build-system.scm ends here
