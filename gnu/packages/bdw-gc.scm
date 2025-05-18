;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014, 2016, 2017, 2020, 2021, 2023 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016, 2018 Leo Famulari <leo@famulari.name>
;;; Copyright © 2017 Rene Saavedra <rennes@openmailbox.org>
;;; Copyright © 2019, 2020, 2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2022, 2023 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2025 Janneke Nieuwenhuizen <janneke@gnu.org>
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

(define-module (gnu packages bdw-gc)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages hurd))

(define-public libgc
  (package
   (name "libgc")
   (version "8.2.4")
   (source (origin
            (method url-fetch)
            (uri (list (string-append "https://github.com/ivmai/bdwgc/releases"
                                      "/download/v" version
                                      "/gc-" version ".tar.gz")
                       (string-append "https://www.hboehm.info/gc/gc_source"
                                      "/gc-" version ".tar.gz")))
            (sha256
             (base32
              "1hlgqkg9arc8sqf7wamvzmp3shb3np5z0h5v0qqksh3pw3dkq39x"))))
   (build-system gnu-build-system)
   (arguments
    (append
     (list
     #:configure-flags
     #~(list
        ;; Install gc_cpp.h et al.
        "--enable-cplusplus"

        ;; In GNU/Hurd systems during the 'check' phase,
        ;; there is a deadlock caused by the 'gctest' test.
        ;; To disable the error set "--disable-gcj-support"
        ;; to configure script. See bug report and discussion:
        ;; <https://lists.opendylan.org/pipermail/bdwgc/2017-April/006275.html>
        ;; <https://lists.gnu.org/archive/html/bug-hurd/2017-01/msg00008.html>
        #$@(if (target-hurd? (or (%current-system)
                                 (%current-target-system)))
               #~("--disable-gcj-support")
               #~())
        #$@(if (target-mingw?)
               #~("--enable-threads=pthreads")
               #~())))
     (cond
       ((target-ppc64le?)
        (list #:make-flags
              ;; This is a known workaround upstream.
              ;; https://github.com/ivmai/bdwgc/issues/479
              #~(list "CFLAGS_EXTRA=-DNO_SOFT_VDB")))
       ((target-ppc32?)
        (list #:make-flags
              ;; Similar to above.
              #~(list "CFLAGS_EXTRA=-DNO_MPROTECT_VDB")))
       (else '()))))
   (native-inputs (list pkg-config))
   (propagated-inputs
    (if (%current-target-system)
        ;; The build system refuses to check for compiler intrinsics when
        ;; cross-compiling, and demands using libatomic-ops instead.
        (list libatomic-ops)
        '()))
   (outputs '("out" "debug"))
   (properties
    '((release-monitoring-url . "https://www.hboehm.info/gc/gc_source/")
      (upstream-name . "gc")))
   (synopsis "The Boehm-Demers-Weiser conservative garbage collector
for C and C++")
   (description
    "The Boehm-Demers-Weiser conservative garbage collector can be used
as a garbage collecting replacement for C malloc or C++ new.  It allows
you to allocate memory basically as you normally would, without
explicitly deallocating memory that is no longer useful.  The collector
automatically recycles memory when it determines that it can no longer
be otherwise accessed.

The collector is also used by a number of programming language
implementations that either use C as intermediate code, want to
facilitate easier interoperation with C libraries, or just prefer the
simple collector interface.

Alternatively, the garbage collector may be used as a leak detector for
C or C++ programs, though that is not its primary goal.")
   (home-page "https://www.hboehm.info/gc/")

   (license (x11-style (string-append home-page "license.txt")))))

;; TODO: Add a static output in libgc in the next rebuild cycle.
(define-public libgc/static-libs
  (package/inherit
   libgc
   (arguments
    (substitute-keyword-arguments (package-arguments libgc)
      ((#:configure-flags flags #~'())
       #~(cons "--enable-static" #$flags))))

   (properties '((hidden? . #t)))))

(define-public libgc-7
  (package
   (inherit libgc)
   (version "7.6.12")
   (source (origin
             (method url-fetch)
             (uri (string-append "https://github.com/ivmai/bdwgc/releases"
                                 "/download/v" version "/gc-" version ".tar.gz"))
             (sha256
              (base32
               "10jhhi79d5brwlsyhwgpnrmc8nhlf7aan2lk9xhgihk5jc6srbvc"))))
   (propagated-inputs (list libatomic-ops))))

(define-public libgc/back-pointers
  (package/inherit
    libgc
    (name "libgc-back-pointers")
    (arguments
     (substitute-keyword-arguments (package-arguments libgc)
       ((#:make-flags _ #~'())
        #~(list "CPPFLAGS=-DKEEP_BACK_PTRS=1"))))
    (synopsis "The BDW garbage collector, with back-pointer tracking")))

(define-public libatomic-ops
  (package
    (name "libatomic-ops")
    (version "7.6.12")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/ivmai/libatomic_ops/releases/download/v"
                    version "/libatomic_ops-" version ".tar.gz"))
              (sha256
               (base32
                "0glzah695wsf6c27hs5wwlw4mnq1vfivdshz1rb8pq7w4mp5dazh"))))
    (build-system gnu-build-system)
    (outputs '("out" "debug"))
    (native-inputs (if (target-loongarch64?)
                       (list config)
                       '()))
    (arguments (if (target-loongarch64?)
                   (list #:phases
                         #~(modify-phases %standard-phases
                             (add-after 'unpack 'update-config-scripts
                               (lambda* (#:key inputs native-inputs #:allow-other-keys)
                                 ;; Replace outdated config.guess and config.sub.
                                 (for-each (lambda (file)
                                             (install-file
                                              (search-input-file
                                               (or native-inputs inputs)
                                               (string-append "/bin/" file)) "."))
                                           '("config.guess" "config.sub"))))))
                   '()))
    (synopsis "Accessing hardware atomic memory update operations")
    (description
     "This C library provides semi-portable access to hardware-provided atomic
memory update operations on a number of architectures.  These might allow you to
write code that does more interesting things in signal handlers, write
lock-free code, experiment with thread programming paradigms, etc.")
    (home-page "https://github.com/ivmai/libatomic_ops/")

    ;; Some source files are X11-style, others are GPLv2+.
    (license gpl2+)))
