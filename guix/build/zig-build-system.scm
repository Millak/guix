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
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (rnrs io ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (%standard-phases
            zig-build))

;; Interesting guide here:
;; https://github.com/riverwm/river/blob/master/PACKAGING.md
(define global-cache-dir "zig-cache")

(define* (configure #:key inputs target #:allow-other-keys)
  ;; Set cache dir, otherwise Zig looks for `$HOME/.cache'.
  (setenv "ZIG_GLOBAL_CACHE_DIR" global-cache-dir)

  ;; Libc paths for target.
  (let ((libc (assoc-ref inputs (if target "cross-libc" "libc")))
        (port (open-file "/tmp/guix-zig-libc-paths" "w" #:encoding "utf8")))
    (display
     (string-append "\
include_dir=" libc "/include
sys_include_dir=" libc "/include
crt_dir=" libc "/lib
msvc_lib_dir=
kernel32_lib_dir=
gcc_dir=")
     port)
    (close-port port))
  (setenv "ZIG_LIBC" "/tmp/guix-zig-libc-paths"))

(define (zig-target target)
  (cond
   ((string=? target "i586-pc-gnu")
    "x86-hurd-gnu")
   ((string=? target "i686-linux-gnu")
    "x86-linux-gnu")
   ((string=? target "i686-w64-mingw32")
    "x86-windows-gnu")
   ((string=? target "mips64el-linux-gnu")
    "mips64el-linux-gnuabi64")
   ((string=? target "powerpc-linux-gnu")
    "powerpc-linux-gnueabi")
   ((string=? target "x86_64-pc-gnu")
    "x86_64-hurd-gnu")
   ((string=? target "x86_64-w64-mingw32")
    "x86_64-windows-gnu")
   (else target)))

(define* (build #:key
                zig-build-flags
                zig-release-type       ;; "safe", "fast" or "small" empty for a
                                       ;; debug build"
                target
                #:allow-other-keys)
  "Build a given Zig package."

  (setenv "DESTDIR" "out")
  (let ((call `("zig" "build"
                     "--prefix"             ""            ;; Don't add /usr
                     "--prefix-lib-dir"     "lib"
                     "--prefix-exe-dir"     "bin"
                     "--prefix-include-dir" "include"
                     ,@(if target
                         (list (string-append "-Dtarget=" (zig-target target)))
                         '())
                     ,@(if zig-release-type
                         (list (string-append "-Drelease-" zig-release-type))
                         '())
                     ,@zig-build-flags)))
  (format #t "running: ~s~%" call)
  (apply invoke call)))

(define* (check #:key tests?
                zig-test-flags
                target
                #:allow-other-keys)
  "Run all the tests"
  (when (and tests? (not target))
    (let ((old-destdir (getenv "DESTDIR")))
      (setenv "DESTDIR" "test-out") ;; Avoid colisions with the build output
      (let ((call `("zig" "build" "test"
                    ,@zig-test-flags)))
        (format #t "running: ~s~%" call)
        (apply invoke call))
      (if old-destdir
        (setenv "DESTDIR" old-destdir)
        (unsetenv "DESTDIR")))))

(define* (install #:key inputs outputs #:allow-other-keys)
  "Install a given Zig package."
  (let ((out (assoc-ref outputs "out")))
    (copy-recursively "out" out)))

(define %standard-phases
  (modify-phases gnu:%standard-phases
    (delete 'bootstrap)
    (replace 'configure configure)
    (replace 'build build)
    (replace 'check check)
    (replace 'install install)))


(define* (zig-build #:key inputs (phases %standard-phases)
                      #:allow-other-keys #:rest args)
  "Build the given Zig package, applying all of PHASES in order."
  (apply gnu:gnu-build #:inputs inputs #:phases phases args))
