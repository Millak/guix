;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2024 Hilton Chain <hako@ultrarare.space>
;;; Copyright © 2024 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (guix build zig-utils)
  #:use-module (guix build utils)
  #:export (zig-configure
            zig-target))

;;;
;;;  Common procedures for zig and zig-build-system.
;;;

(define* (zig-configure #:key inputs target #:allow-other-keys)
  ;; Set cache dir, otherwise Zig looks for `$HOME/.cache'.
  (setenv "ZIG_GLOBAL_CACHE_DIR" "/tmp/zig-cache")
  (setenv "ZIG_LOCAL_CACHE_DIR"  "/tmp/zig-cache")

  ;; XXX: Required for unpatched intermediade versions?
  ;; Set CC, since the stage 2 zig relies on it to find the libc
  ;; installation, and otherwise silently links against its own.
  (setenv "CC"
          (if target
              (string-append target "-gcc")
              "gcc"))

  (setenv "PKG_CONFIG"
          (if target
              (string-append target "-pkg-config")
              "pkg-config"))

  ;; Libc paths for target.
  (let ((libc (assoc-ref inputs (if target "cross-libc" "libc")))
        (port (open-file "/tmp/guix-zig-libc-paths" "w" #:encoding "utf8")))
    (when libc
      (display
       (string-append "\
include_dir=" libc "/include
sys_include_dir=" libc "/include
crt_dir=" libc "/lib
msvc_lib_dir=
kernel32_lib_dir=
gcc_dir=")
       port)
      (setenv "ZIG_LIBC" "/tmp/guix-zig-libc-paths"))
    (close-port port)))

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
