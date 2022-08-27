;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Brett Gilio <brettg@gnu.org>
;;; Copyright © 2020 Brendan Tildesley <mail@brendan.scot>
;;; Copyright © 2021, 2022 Philip McGrath <philip@philipmcgrath.com>
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

(define-module (gnu packages chez)
  #:use-module (gnu packages)
  #:use-module ((guix licenses)
                #:select (gpl2+ gpl3+ lgpl2.0+ lgpl2.1+ asl2.0 bsd-3 expat
                          public-domain))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system copy)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages netpbm)
  #:use-module (gnu packages racket)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages image)
  #:use-module (gnu packages xorg)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (chez-scheme-for-system
            racket-cs-native-supported-system?
            nix-system->pbarch-machine-type
            unpack-nanopass+stex))

;; Commentary:
;;
;; The bootstrapping paths for Chez Scheme and Racket are closely
;; entwined. Racket CS (the default Racket implementation) is based on (a fork
;; of) Chez Scheme. Racket's variant of Chez Scheme shares sources for
;; nanopass and stex with upstream Chez Scheme.
;;
;; Racket's variant of Chez Scheme can be bootstrapped by an older Racket
;; implementation, Racket BC, which can be bootstrapped from C. Porting that
;; code to work with upstream Chez Scheme (or finding an old version that
;; does) is our best hope for some day bootstrapping upstream Chez Scheme from
;; source.
;;
;; Code:

(define* (chez-scheme-for-system #:optional
                                 (system (or (%current-target-system)
                                             (%current-system))))
  "Return 'chez-scheme' if it fully supports SYSTEM, including support for
bootstrapping and native threads.  Otherwise, return
'chez-scheme-for-racket'."
  (if (and=> (chez-upstream-features-for-system system)
             (lambda (features)
               (every (cut memq <> features)
                      '(threads
                        ;; We can cross-compile for platforms without
                        ;; bootstrap bootfiles, but we can't self-host
                        ;; on them short of adding more binary seeds.
                        bootstrap-bootfiles))))
      chez-scheme
      chez-scheme-for-racket))

(define* (target-chez-arch #:optional (system
                                       (or (%current-target-system)
                                           (%current-system))))
  "Return a string representing the architecture of SYSTEM as used in Chez
Scheme machine types, or '#f' if none is defined."
  (cond
   ((target-x86-64? system)
    "a6")
   ((target-x86-32? system)
    "i3")
   ((target-aarch64? system)
    "arm64")
   ((target-arm32? system)
    "arm32")
   ((target-ppc64le? system)
    #f)
   ((target-ppc32? system)
    "ppc32")
   ((target-riscv64? system)
    #f)
   (else
    #f)))

(define* (target-chez-os #:optional (system (or (%current-target-system)
                                                (%current-system))))
  "Return a string representing the operating system kernel of SYSTEM as used
in Chez Scheme machine types, or '#f' if none is defined."
  ;; e.g. "le" includes both GNU/Linux and Android
  (cond
   ((target-linux? system)
    "le")
   ((target-hurd? system)
    "gnu")
   ((target-mingw? system)
    "nt")
   ;; missing (guix utils) predicates
   ;; cf. https://github.com/NixOS/nixpkgs/blob/master/lib/systems/doubles.nix
   ((string-suffix? "-darwin" system)
    "osx")
   ((string-suffix? "-freebsd" system)
    "fb")
   ((string-suffix? "-openbsd" system)
    "ob")
   ((string-suffix? "-netbsd" system)
    "nb")
   ;; Nix says "x86_64-solaris", but accommodate "-solaris2"
   ((string-contains system "solaris")
    "s2")
   ((string-suffix? "-qnx" system)
    "qnx")
   ;; unknown
   (else
    #f)))

(define %chez-features-table
  ;; An alist of alists mapping:
  ;;   os -> arch -> (or/c #f (listof symbol?))
  ;; where:
  ;;  - `os` is a string for the OS part of a Chez Scheme machine type; and
  ;;  - `arch` is a string for the architecture part of a Chez machine type.
  ;;
  ;; The absence of an entry for a given arch--os pair means that neither
  ;; upstream Chez Scheme nor the Racket variant can generate native code for
  ;; that system.  (The Racket variant can still provide support via its
  ;; ``portable bytecode'' backends and optional compilation to C.)  A value
  ;; of `#f` means that upstream Chez Scheme does not support the arch--os
  ;; pair at all, but the Racket variant does.  A list has the same meaning as
  ;; a result from `chez-upstream-features-for-system`.
  ;;
  ;; The arch--os pairs marked "commented out" have been commented out in the
  ;; STeX source for the upstream release notes since the initial release as
  ;; free software, but they are reported to work and/or have been described
  ;; as supported by upstream maintainers.
  ;;
  ;; For this overall approach to make sense, we assume that Racket's variant
  ;; of Chez Scheme can generate native code for a superset of the platforms
  ;; supported upstream, supports threads on all platforms it supports at all
  ;; (because they are needed for Racket), and doesn't need bootstrap
  ;; bootfiles.  Those assumptions have held for several years.
  '(;; Linux
    ("le"
     ("i3" threads bootstrap-bootfiles)
     ("a6" threads bootstrap-bootfiles)
     ("arm32" bootstrap-bootfiles)
     ("arm64" . #f)
     ("ppc32" threads))
    ;; Hurd
    ("gnu"
     ("i3" . #f))
    ;; FreeBSD
    ("fb"
     ("i3" threads) ;; commented out
     ("a6" threads) ;; commented out
     ("arm32" . #f)
     ("arm64" . #f)
     ("ppc32" . #f))
    ;; OpenBSD
    ("ob"
     ("i3" threads) ;; commented out
     ("a6" threads) ;; commented out
     ("arm32" . #f)
     ("arm64" . #f)
     ("ppc32" . #f))
    ;; NetBSD
    ("nb"
     ("i3" threads) ;; commented out
     ("a6" threads) ;; commented out
     ("arm32" . #f)
     ("arm64" . #f)
     ("ppc32" . #f))
    ;; OpenSolaris / OpenIndiana / Illumos
    ("s2"
     ("i3" threads) ;; commented out
     ("a6" threads)) ;; commented out
    ;; QNX
    ("qnx"
     ("i3" . #f))
    ;; Windows
    ("nt"
     ("i3" threads bootstrap-bootfiles)
     ("a6" threads bootstrap-bootfiles)
     ;; ^ threads "experiemental", but reportedly fine
     ("arm64" . #f))
    ;; Darwin
    ("osx"
     ("i3" threads bootstrap-bootfiles)
     ("a6" threads bootstrap-bootfiles)
     ("arm64" . #f)
     ("ppc32" . #f))))

(define* (chez-upstream-features-for-system #:optional
                                            (system
                                             (or (%current-target-system)
                                                 (%current-system))))
  "Return a list of symbols naming features supported by upstream Chez Scheme
for the Nix system identifier SYSTEM, or @code{#f} if upstream Chez Scheme
does not support SYSTEM at all.

If native threads are supported, the returned list will include
@code{'threads}.  If bootstrap bootfiles for SYSTEM are distributed in the
upstream Chez Scheme repository, the returned list will include
@code{'bootstrap-bootfiles}.  Other feature symbols may be added in the
future."
  (let ((chez-arch (target-chez-arch system))
        (chez-os (target-chez-os system)))
    (and=> (assoc-ref %chez-features-table chez-os)
           (cut assoc-ref <> chez-arch))))

(define* (nix-system->pbarch-machine-type #:optional
                                          (system
                                           (or (%current-target-system)
                                               (%current-system)))
                                          #:key (threads? #t))
  "Return a string naming the pseudo–machine type used by Racket's variant of
Chez Scheme to represent the appropriate ``pbarch'' backend for SYSTEM: that
is, the ``portable bytecode'' backend specialized for SYSTEM's word size and
endianness.  The result will name the threaded machine type unless THREADS? is
provided and is #f."
  (string-append (if threads?
                     "t"
                     "")
                 "pb"
                 (if (target-64bit? system)
                     "64"
                     "32")
                 ;; missing (guix utils) predicate target-little-endian?
                 (if (target-ppc32? system)
                     "b"
                     "l")))

(define* (racket-cs-native-supported-system? #:optional
                                             (system
                                              (or (%current-target-system)
                                                  (%current-system))))
  "Can Racket's variant of Chez Scheme generate native code for SYSTEM?  If
so, return the applicable machine type as a string.  Otherwise, when SYSTEM
can use only the ``portable bytecode'' backends, return #f."
  (let ((chez-arch (target-chez-arch system))
        (chez-os (target-chez-os system)))
    (and (and=> (assoc-ref %chez-features-table chez-os)
                ;; NOT assoc-ref: supported even if cdr is #f
                (cut assoc chez-arch <>))
         (string-append "t" chez-arch chez-os))))

;;
;; Chez Scheme:
;;


(define unpack-nanopass+stex
  #~(begin
      (copy-recursively
       (dirname (search-input-file %build-inputs
                                   "lib/chez-scheme/nanopass.ss"))
       "nanopass"
       #:keep-mtime? #t)
      (mkdir-p "stex")
      (with-output-to-file "stex/Mf-stex"
        (lambda ()
          ;; otherwise, it will try to download submodules
          (display "# to placate ../configure")))))

(define-public chez-scheme
  (package
    (name "chez-scheme")
    ;; The version should match `(scheme-version-number)`.
    ;; See s/cmacros.ss c. line 360.
    (version "9.5.8")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/cisco/ChezScheme")
                    (commit (string-append "v" version))))
              (sha256
               (base32
                "0xchqq8cm0ka5wgpn18sjs0hh15rc3nb7xrjqbbc9al3asq0d7gc"))
              (file-name (git-file-name name version))
              (patches (search-patches "chez-scheme-bin-sh.patch"))
              (snippet #~(begin
                           (use-modules (guix build utils))
                           ;; TODO: consider putting this in a (guix ...) or
                           ;; (guix build ...)  module so it can be shared
                           ;; with the Racket origin without cyclic issues.
                           (for-each (lambda (dir)
                                       (when (directory-exists? dir)
                                         (delete-file-recursively dir)))
                                     '("stex"
                                       "nanopass"
                                       "lz4"
                                       "zlib"))))))
    (build-system gnu-build-system)
    (inputs
     (list
      chez-scheme-bootstrap-bootfiles
      `(,util-linux "lib") ;<-- libuuid
      zlib
      lz4
      ncurses ;<-- for expeditor
      ;; for X11 clipboard support in expeditor:
      ;; https://github.com/cisco/ChezScheme/issues/9#issuecomment-222057232
      libx11))
    (native-inputs (list chez-nanopass-bootstrap
                         stex-bootstrap))
    (native-search-paths
     (list (search-path-specification
            (variable "CHEZSCHEMELIBDIRS")
            (files '("lib/chez-scheme")))))
    (outputs '("out" "doc"))
    (arguments
     (list
      #:modules
      '((guix build gnu-build-system)
        (guix build utils)
        (ice-9 ftw)
        (ice-9 match))
      #:test-target "test"
      #:configure-flags
      #~`(,(string-append "--installprefix=" #$output)
          #$@(if (and=> (chez-upstream-features-for-system)
                        (cut memq 'threads <>))
                 #~("--threads")
                 #~())
          "ZLIB=-lz"
          "LZ4=-llz4"
          "--libkernel"
          ;; Guix will do 'compress-man-pages',
          ;; and letting Chez try causes an error
          "--nogzip-man-pages")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'unpack-nanopass+stex
            (lambda args
              #$unpack-nanopass+stex))
          (add-after 'unpack-nanopass+stex 'unpack-bootfiles
            (lambda* (#:key native-inputs inputs #:allow-other-keys)
              (when (directory-exists? "boot")
                (delete-file-recursively "boot"))
              (copy-recursively
               (search-input-directory inputs
                                       "lib/chez-scheme-bootfiles")
               "boot")))
          ;; NOTE: The custom Chez 'configure' script doesn't allow
          ;; unrecognized flags, such as those automatically added
          ;; by `gnu-build-system`. This replacement phase uses only
          ;; the explicitly provided `#:configure-flags`.
          (replace 'configure
            (lambda* (#:key inputs (configure-flags '()) out-of-source?
                            #:allow-other-keys)
              (let* ((abs-srcdir (getcwd))
                     (srcdir (if out-of-source?
                                 (string-append "../" (basename abs-srcdir))
                                 ".")))
                (format #t "source directory: ~s (relative from build: ~s)~%"
                        abs-srcdir srcdir)
                (if out-of-source?
                    (begin
                      (mkdir "../build")
                      (chdir "../build")))
                (format #t "build directory: ~s~%" (getcwd))
                (format #t "configure flags: ~s~%" configure-flags)
                (apply invoke
                       (string-append srcdir "/configure")
                       configure-flags))))
          (add-after 'configure 'configure-environment-variables
            (lambda args
              ;; Some makefiles (for tests) don't seem to propagate CC
              ;; properly, so we take it out of their hands:
              (setenv "CC" #$(cc-for-target))
              ;; Likewise, some tests have needed HOME to be set:
              (setenv "HOME" "/tmp")))
          ;; The binary file name is called "scheme" as is the one from
          ;; MIT/GNU Scheme.  We add a symlink to use in case both are
          ;; installed.
          (add-after 'install 'install-symlink
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((scheme (search-input-file outputs "/bin/scheme"))
                     (bin-dir (dirname scheme)))
                (symlink scheme
                         (string-append bin-dir "/chez-scheme"))
                (match (find-files (string-append bin-dir "/../lib")
                                   "scheme.boot")
                  ((scheme.boot)
                   (symlink scheme.boot
                            (string-append (dirname scheme.boot)
                                           "/chez-scheme.boot")))))))
          ;; Building the documentation requires stex and a running scheme.
          (add-after 'install-symlink 'install-docs
            (lambda* (#:key native-inputs inputs outputs #:allow-other-keys)
              (let* ((doc-prefix (or (assoc-ref outputs "doc")
                                     (assoc-ref outputs "out")))
                     (chez+version (strip-store-file-name #$output))
                     (scheme (search-input-file outputs "/bin/scheme"))
                     (stexlib (search-input-directory (or native-inputs
                                                          inputs)
                                                      "/lib/stex"))
                     (doc-dir (string-append doc-prefix
                                             "/share/doc/"
                                             chez+version)))
                (define* (stex-make #:optional (suffix ""))
                  (invoke "make" "install"
                          (string-append "Scheme=" scheme)
                          (string-append "STEXLIB=" stexlib)
                          (string-append "installdir=" doc-dir suffix)))
                (with-directory-excursion "csug"
                  (stex-make "/csug"))
                (with-directory-excursion "release_notes"
                  (stex-make "/release_notes")))))
          (add-after 'install-docs 'link-doc-pdfs
            ;; otherwise, it's hard to notice them in a forest of HTML files
            (lambda* (#:key outputs #:allow-other-keys)
              (with-directory-excursion
                  (string-append (or (assoc-ref outputs "doc")
                                     (assoc-ref outputs "out"))
                                 "/share/doc/"
                                 (strip-store-file-name #$output))
                (symlink "release_notes/release_notes.pdf"
                         "release_notes.pdf")
                (match (find-files "csug"
                                   "csug.*\\.pdf$" ;; embeded version number
                                   #:fail-on-error? #t)
                  ((pth)
                   (symlink pth
                            "csug.pdf")))))))))
    (supported-systems
     (delete
      "armhf-linux" ;; XXX reportedly broken, needs checking
      (filter chez-upstream-features-for-system
              %supported-systems)))
    (home-page "https://cisco.github.io/ChezScheme/")
    (synopsis "R6RS Scheme compiler and run-time")
    (description
     "Chez Scheme is a compiler and run-time system for the language of the
Revised^6 Report on Scheme (R6RS), with numerous extensions.  The compiler
generates native code for each target processor, with support for x86, x86_64,
and 32-bit PowerPC architectures.")
    (license asl2.0)))

(define-public chez-scheme-for-racket
  (package
    (inherit chez-scheme)
    (name "chez-scheme-for-racket")
    (version "9.5.9.2")
    ;; The version should match `(scheme-fork-version-number)`.
    ;; See racket/src/ChezScheme/s/cmacros.ss c. line 360.
    ;; It will always be different than the upstream version!
    ;; When updating, remember to also update %racket-version in racket.scm.
    (source #f) ; avoid problematic cycle with racket.scm
    (inputs
     (let ((inputs (modify-inputs (package-inputs chez-scheme)
                     (replace "chez-scheme-bootstrap-bootfiles"
                       chez-scheme-for-racket-bootstrap-bootfiles)
                     (delete "libx11" "util-linux:lib"))))
       (if (racket-cs-native-supported-system?)
           inputs
           (modify-inputs inputs
             (prepend libffi)))))
    (native-inputs
     (let ((native-inputs (modify-inputs (package-native-inputs chez-scheme)
                            (prepend zuo))))
       (if (%current-target-system)
           (modify-inputs native-inputs
             (prepend this-package))
           native-inputs)))
    (arguments
     (substitute-keyword-arguments (package-arguments chez-scheme)
       ((#:out-of-source? _ #f)
        #t)
       ((#:tests? _ #t)
        ;; FIXME: There have been some flaky test failures. Some have been
        ;; fixed upstream post-release but have proven non-trivial to
        ;; backport; at least one issue remains. Re-enable tests once
        ;; https://github.com/racket/racket/issues/4359 is fixed.
        #f)
       ((#:configure-flags cfg-flags #~'())
        #~`("--disable-x11"
            "--threads" ;; ok to potentially duplicate
            #$(string-append "-m=" (or (racket-cs-native-supported-system?)
                                       (nix-system->pbarch-machine-type)))
            ;; ^ could skip -m= for non-cross non-pbarch builds
            #$@(if (racket-cs-native-supported-system?)
                   #~()
                   ;; not inferred on non-native platforms: see
                   ;; https://racket.discourse.group/t/950/9
                   #~("--enable-libffi"
                      "CFLAGS=-g -O2 -D_REENTRANT -pthread"
                      "LIBS=-lm -ldl -lrt -lffi -lncurses"))
            #$@(if (%current-target-system)
                   (list (string-append "--toolprefix="
                                        (%current-target-system)
                                        "-"))
                   '())
            ,@(let* ((chez+version (strip-store-file-name #$output))
                     (doc-prefix (assoc-ref %outputs "doc"))
                     (doc-dir (string-append doc-prefix
                                             "/share/doc/"
                                             chez+version)))
                (list (string-append "--installcsug="
                                     doc-dir
                                     "/csug")
                      (string-append "--installreleasenotes="
                                     doc-dir
                                     "/release_notes")))
            ,@#$cfg-flags))
       ((#:make-flags mk-flags #~'())
        #~(cons* (string-append "ZUO="
                                #+(this-package-native-input "zuo")
                                "/bin/zuo")
                 (string-append "STEXLIB="
                                #+(this-package-native-input "stex")
                                "/lib/stex")
                 #$mk-flags))
       ((#:phases those-phases #~%standard-phases)
        #~(let* ((those-phases #$those-phases)
                 (gnu:unpack (assoc-ref those-phases 'unpack))
                 (gnu:build (assoc-ref those-phases 'build)))
            (modify-phases those-phases
              (replace 'build
                ;; need to override target for cross-compilation
                ;; https://racket.discourse.group/t/950/19
                (lambda* (#:key target (make-flags '()) (parallel-build? #t)
                          #:allow-other-keys)
                  (gnu:build #:make-flags (if target
                                              (cons "kernel" make-flags)
                                              make-flags)
                             #:parallel-build? parallel-build?)))
              (replace 'install-docs
                (lambda* (#:key native-inputs (make-flags '())
                          #:allow-other-keys)
                  ;; The tests for 'native-inputs' are cross-compilation
                  ;; workarounds that would be better to address upstream:
                  ;; see <https://racket.discourse.group/t/950/20>.
                  (when native-inputs
                    (substitute* "Makefile"
                      (("install-docs: build \\$[(]ZUO[)]")
                       "install-docs: $(ZUO)")))
                  (apply invoke
                         "make"
                         "install-docs"
                         (if native-inputs
                             (cons (string-append
                                    "Scheme="
                                    (search-input-file native-inputs
                                                       "/bin/scheme"))
                                   make-flags)
                             make-flags))))
              (replace 'unpack
                (lambda args
                  (gnu:unpack #:source #$(or (package-source this-package)
                                             (package-source racket-vm-bc)))))
              (add-after 'unpack 'chdir
                (lambda args
                  (chdir "racket/src/ChezScheme"))))))))
    (supported-systems %supported-systems)
    (home-page "https://github.com/racket/ChezScheme")
    ;; ^ This is downstream of https://github.com/racket/racket,
    ;; but it's designed to be a friendly landing place for people
    ;; who want a ChezScheme-shaped repositroy.
    (synopsis "Variant of Chez Scheme extended for Racket")
    (description "This variant of Chez Scheme is extended to support the
implementation of Racket.  It may be useful on platforms that are not yet
supported by upstream Chez Scheme.

Main additions to Chez Scheme in the Racket variant:
@itemize @bullet
@item
AArch64 code generation
@item
Portable bytecode (@code{pb}) mode, which is mainly useful for bootstrapping a
build on any platform, but can also be used on platforms without native-code
generation, compiled via Emscripten, linked with @code{libffi}, or used with
bytecode partially compiled to C
@item
Unboxed floating-point arithmetic and flvectors
@item
Faster multiplication and division for large exact numbers
@item
Type reconstruction during optimization (especially for safe code)
@item
Continuation attachments
@item
Parallel garbage collection, in-place garbage collection for old-generation
objects (instead of always copying), and reachability-based memory
accounting
@item
Ordered finalization, immobile (but collectable) objects, weak/ephemeron
generic hash tables, and reference bytevectors
@end itemize")
    (license asl2.0)))

;;
;; Bootfiles:
;;

(define-public chez-scheme-bootstrap-bootfiles
  (package
    (inherit chez-scheme)
    (name "chez-scheme-bootstrap-bootfiles")
    (inputs '())
    (native-inputs '())
    (outputs '("out"))
    (build-system copy-build-system)
    ;; TODO: cross compilation
    (arguments
     (list #:install-plan
           #~`(("boot/" "lib/chez-scheme-bootfiles"))))
    (supported-systems
     (filter (lambda (system)
               (and=> (chez-upstream-features-for-system system)
                      (cut memq 'bootstrap-bootfiles <>)))
             %supported-systems))
    (synopsis "Chez Scheme bootfiles (binary seed)")
    (description
     "Chez Scheme is a self-hosting compiler: building it requires
``bootfiles'' containing the Scheme-implemented portions compiled for the
current platform.  (Chez can then cross-compile bootfiles for all other
supported platforms.)

This package provides bootstrap bootfiles for upstream Chez Scheme.
Currently, it simply packages the binaries checked in to the upstream
repository.  Hopefully we can eventually adapt Racket's @code{cs-bootstrap} to
work with upstream Chez Scheme so that we can bootstrap these files from
source.")))

(define-public chez-scheme-for-racket-bootstrap-bootfiles
  (package
    (inherit chez-scheme-bootstrap-bootfiles)
    (name "chez-scheme-for-racket-bootstrap-bootfiles")
    (version (package-version chez-scheme-for-racket))
    (source #f) ; avoid problematic cycle with racket.scm
    (native-inputs
     (cons* chez-nanopass-bootstrap
            (if (%current-target-system)
                (list zuo
                      chez-scheme-for-racket)
                (list racket-vm-bc))))
    (arguments
     (substitute-keyword-arguments
         (package-arguments chez-scheme-bootstrap-bootfiles)
       ((#:phases those-phases #~%standard-phases)
        #~(let* ((those-phases #$those-phases)
                 (unpack (assoc-ref those-phases 'unpack)))
            (modify-phases those-phases
              (replace 'unpack
                (lambda args
                  (unpack #:source #$(or (package-source this-package)
                                         (package-source racket-vm-bc)))))
              (add-after 'unpack 'chdir
                (lambda args
                  (chdir "racket/src/ChezScheme")))
              (add-after 'chdir 'unpack-nanopass+stex
                (lambda args
                  #$unpack-nanopass+stex))
              (add-before 'install 'build
                (lambda* (#:key native-inputs inputs #:allow-other-keys)
                  #$(cond
                     ((%current-target-system)
                      ;; cross-compiling
                      #~(invoke
                         (search-input-file (or native-inputs inputs)
                                            "/bin/zuo")
                         "makefiles/boot.zuo"
                         (search-input-file (or native-inputs inputs)
                                            "/bin/scheme")
                         #$(or (racket-cs-native-supported-system?)
                               (nix-system->pbarch-machine-type))))
                     (else
                      ;; bootstrapping
                      #~(invoke
                         (search-input-file (or native-inputs inputs)
                                            "/opt/racket-vm/bin/racket")
                         "rktboot/main.rkt"
                         #$@(if (racket-cs-native-supported-system?)
                                #~()
                                (let ((m (nix-system->pbarch-machine-type)))
                                  #~("--machine" #$m)))))))))))))
    (supported-systems
     (package-supported-systems chez-scheme-for-racket))
    (home-page "https://github.com/racket/ChezScheme")
    ;; ^ This is downstream of https://github.com/racket/racket,
    ;; but it's designed to be a friendly landing place for people
    ;; who want a ChezScheme-shaped repositroy.
    (synopsis "Chez Scheme bootfiles bootstrapped by Racket")
    (description "Chez Scheme is a self-hosting compiler: building it
requires ``bootfiles'' containing the Scheme-implemented portions compiled for
the current platform.  (Chez can then cross-compile bootfiles for all other
supported platforms.)

The Racket package @code{cs-bootstrap} (part of the main Racket Git
repository) implements enough of a Chez Scheme simulation to load the Chez
Scheme compiler purely from source into Racket and apply the compiler to
itself, thus bootstrapping Chez Scheme.  Bootstrapping takes about 10 times as
long as using an existing Chez Scheme, but @code{cs-bootstrap} supports Racket
7.1 and later, including the Racket BC variant.

Note that the generated bootfiles are specific to Racket's fork of Chez
Scheme, and @code{cs-bootstrap} does not currently support building upstream
Chez Scheme.")))

;;
;; Chez's bootstrap dependencies:
;;

(define-public stex-bootstrap
  ;; This commit includes a fix which we would otherwise want to use as
  ;; patch.  Let's revert to tagged releases as soon as one becomes available.
  (let ((commit "afa607564a5662ffd748e824801277a6b5a3d11c")
        (revision "2"))
    (hidden-package
     (package
       (name "stex")
       ;; ^ Debian calls this "stex", not "chez-stex". It is a set of
       ;; command-line tools, and there isn't a Scheme API, let alone a
       ;; Chez-specific one, except perhaps that the Scheme examples are
       ;; assumed to be Chez-compatible.
       (version (git-version "1.2.2" revision commit))
       (source
        (origin
          (method git-fetch)
          (uri (git-reference
                (url "https://github.com/dybvig/stex")
                (commit commit)))
          (sha256
           (base32 "0n6dryv5j7cw2qmsj55wqb0ph901h83a2hl4j891ppxp0xx18nkp"))
          (file-name (git-file-name name version))
          (patches
           ;; submitted upstream in https://github.com/dybvig/stex/pull/6
           (search-patches "stex-copy-from-immutable-store.patch"))
          (snippet
           #~(for-each delete-file
                       '("sbin/install" "doc/stex.pdf" "doc/stex.html")))))
       (outputs '("out"))
       (build-system copy-build-system)
       ;; N.B. Upstream does not seem to support cross-compilation,
       ;; though it would probably be easy to add.
       (propagated-inputs
        (list xorg-rgb
              (texlive-updmap.cfg
               (list texlive-dvips-l3backend
                     texlive-hyperref
                     texlive-bibtex
                     texlive-epsf
                     texlive-fonts-ec
                     texlive-oberdiek))
              ghostscript
              netpbm))
       ;; Debian uses a versionless path for STEXLIB,
       ;; which is much more convienient.
       (arguments
        (list
         #:install-plan #~`(("inputs" "lib/stex/")
                            ("gifs" "lib/stex/")
                            ("math" "lib/stex/")
                            ("src" "lib/stex/")
                            ("Mf-stex" "lib/stex/")
                            ("Makefile.template" "lib/stex/"))
         #:modules
         '((guix build copy-build-system)
           (guix build utils)
           (ice-9 popen))
         #:phases
         #~(modify-phases %standard-phases
             (add-before 'install 'patch-sources
               (lambda* (#:key native-inputs inputs outputs #:allow-other-keys)
                 (define scheme
                   (false-if-exception
                    (search-input-file inputs "/bin/scheme")))
                 (when scheme
                   (setenv "Scheme" scheme))
                 (substitute* '("Makefile.template"
                                "doc/Makefile")
                   (("STEXLIB=[^\n]*")
                    (string-append "STEXLIB=" #$output "/lib/stex"))
                   (("Scheme=[^\n]*")
                    (string-append "Scheme=" (or scheme "scheme"))))
                 (substitute* '("Mf-stex"
                                "math/Makefile")
                   (("/bin/rm")
                    "rm"))
                 (substitute* "Mf-stex"
                   (("SHELL=bash")
                    ;; avoid Solaris workaround
                    "#SHELL=bash"))))
             (add-after 'install 'maybe-compile
               (lambda* (#:key native-inputs inputs outputs #:allow-other-keys)
                 (cond
                  ((getenv "Scheme")
                   => (lambda (scheme)
                        (define makefile
                          (string-append (getcwd) "/Makefile"))
                        (define machine
                          (let ((pipe (open-pipe* OPEN_BOTH scheme "-q")))
                            ;; try to not be wrong for cross-compilation
                            ;; (avoid #% reader abbreviation for Guile)
                            (write '(($primitive $target-machine)) pipe)
                            (force-output pipe)
                            (let ((sym (read pipe)))
                              (close-pipe pipe)
                              (symbol->string sym))))
                        (with-directory-excursion
                            (search-input-directory outputs "/lib/stex")
                          (invoke "make"
                                  "-f" makefile
                                  (string-append "Scheme=" scheme))
                          (for-each delete-file
                                    (find-files machine "\\.")))))
                  (else
                   ;; for bootstrapping, can run without ahead-of-time
                   ;; compilation
                   (format #t "not compiling~%")))))
             (add-after 'maybe-compile 'maybe-make-docs
               (lambda* (#:key native-inputs inputs outputs #:allow-other-keys)
                 (cond
                  ((assoc-ref outputs "doc")
                   => (lambda (doc-prefix)
                        (define doc-dir
                          (string-append doc-prefix "/share/doc/stex"))
                        ;; the Makefile is referenced in the documentation
                        (copy-recursively "doc" doc-dir)
                        (install-file "ReadMe" doc-dir)
                        (with-directory-excursion "doc"
                          (invoke "make")
                          (install-file "stex.html" doc-dir)
                          (install-file "stex.pdf" doc-dir))))
                  (else
                   (format #t "not making docs~%"))))))))
       (home-page "https://github.com/dybvig/stex")
       (synopsis "LaTeX with embedded Scheme code and HTML generation")
       (description "The @code{stex} package extends LaTeX with a handful of
commands for including Scheme code (or pretty much any other kind of code, as
long as you don't plan to use the Scheme-specific transcript support) in a
document.  It provides the programs @code{scheme-prep} and @code{html-prep} to
convert @code{stex} documents to LaTeX and HTML, respectively, plus makefile
templates, style files, and other resources.  The @code{stex} system is used
to typeset @cite{The Scheme Programming Language} and the @cite{Chez Scheme
User's Guix}, among other documents.")
       (license expat)))))

(define-public stex
  (package/inherit stex-bootstrap
    (inputs (modify-inputs (package-inputs stex-bootstrap)
              (prepend chez-scheme)))
    (outputs '("out" "doc"))
    (properties '())))

(define-public chez-nanopass-bootstrap
  (hidden-package
   (package
     (name "chez-nanopass")
     (version "1.9.2")
     (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/nanopass/nanopass-framework-scheme")
              (commit (string-append "v" version))))
        (sha256
         (base32 "16vjsik9rrzbabbhbxbaha51ppi3f9n8rk59pc6zdyffs0vziy4i"))
        (file-name (git-file-name "nanopass-framework-scheme" version))
        (snippet
         #~(begin
             (use-modules (guix build utils))
             (when (file-exists? "doc/user-guide.pdf")
               (delete-file "doc/user-guide.pdf"))
             (substitute* "doc/Makefile"
               (("include ~/stex/Mf-stex")
                "include $(STEXLIB)/Mf-stex"))))))
     (build-system copy-build-system)
     (arguments
      (list #:install-plan
            #~`(("nanopass.ss" "lib/chez-scheme/")
                ("nanopass" "lib/chez-scheme/"))))
     (home-page "https://nanopass.org")
     (synopsis "DSL for compiler development")
     (description "The Nanopass framework is an embedded domain-specific
language for writing compilers composed of several simple passes that
operate over well-defined intermediate languages.  The goal of this
organization is both to simplify the understanding of each pass, because it
is responsible for a single task, and to simplify the addition of new passes
anywhere in the compiler.  Nanopass reduces the boilerplate required to
create compilers, making them easier to understand and maintain.")
     (license expat))))

(define-public chez-nanopass
  (package/inherit chez-nanopass-bootstrap
    (properties '())
    ;; TODO: cross-compilation
    (native-inputs (list chez-scheme stex))
    (arguments
     (substitute-keyword-arguments (package-arguments chez-nanopass-bootstrap)
       ((#:install-plan base-plan)
        #~`(("nanopass.so" "lib/chez-scheme/")
            ("doc/user-guide.pdf" #$(string-append
                                     "share/doc/"
                                     (package-name this-package)
                                     "-"
                                     (package-version this-package)
                                     "/"))
            ,@#$base-plan))
       ((#:phases base-phases #~%standard-phases)
        #~(modify-phases #$base-phases
            (add-before 'install 'compile-and-test
              (lambda args
                (invoke "scheme"
                        "--compile-imported-libraries"
                        "--program" "test-all.ss")))
            (add-after 'compile-and-test 'build-doc
              (lambda* (#:key native-inputs inputs #:allow-other-keys)
                (with-directory-excursion "doc"
                  (invoke "make"
                          (string-append "Scheme="
                                         (search-input-file
                                          (or native-inputs inputs)
                                          "/bin/scheme"))
                          (string-append "STEXLIB="
                                         (search-input-directory
                                          (or native-inputs inputs)
                                          "/lib/stex"))))))))))))

;;
;; Other Chez packages:
;;

;; Help function for Chez Scheme to add the current path to
;; CHEZSCHEMELIBDIRS.
(define chez-configure
  #~(lambda _
      (let ((chez-env (getenv "CHEZSCHEMELIBDIRS")))
        (setenv "CHEZSCHEMELIBDIRS"
                (if chez-env
                    (string-append ".:" chez-env)
                    ".")))))

;; Help function to define make flags for some Chez Scheme custom make
;; files.
(define (chez-make-flags name version)
  #~(let ((out #$output))
      (list
       ;; Set 'schemedir' so that libraries are installed in
       ;; 'lib/chez-scheme' like Chez's 'native-search-paths' expects.
       (string-append "schemedir=" out "/lib/chez-scheme")
       (string-append "PREFIX=" out)
       (string-append "DOCDIR=" out "/share/doc/" #$name "-" #$version))))

(define-public chez-srfi
  (package
    (name "chez-srfi")
    (version "1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/fedeinthemix/chez-srfi")
             (commit (string-append "v" version))))
       (sha256
        (base32 "1vgn984mj2q4w6r2q66h7qklp2hrh85wwh4k9yisga5fi0ps7myf"))
       (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (native-inputs
     (list (chez-scheme-for-system)))
    (arguments
     (list #:make-flags (chez-make-flags name version)
           #:test-target "test"
           #:phases #~(modify-phases %standard-phases
                        (replace 'configure
                          #$chez-configure))))
    (home-page "https://github.com/fedeinthemix/chez-srfi")
    (synopsis "SRFI libraries for Chez Scheme")
    (description
     "This package provides a collection of SRFI libraries for Chez Scheme.")
    (license expat)))

(define-public chez-web
  (let ((commit "5fd177fe53f31f466bf88720d03c95a3711a8bea")
        (revision "1"))
    (package
      (name "chez-web")
      ;; Release 2.0 is different and doesn't work.
      (version (git-version "2.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/arcfide/ChezWEB")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1dq25qygyncbfq4kwwqqgyyakfqjwhp5q23vrf3bff1p66nyfl3b"))))
      (build-system gnu-build-system)
      (native-inputs
       (list (chez-scheme-for-system)
             ghostscript
             ;; FIXME: This package fails to build with the error:
             ;;     mktexpk: don't know how to create bitmap font for bchr8r
             ;; Replacing the following with `texlive` fixes it.
             ;; What is missing?
             (texlive-updmap.cfg (list texlive-oberdiek
                                       texlive-epsf
                                       texlive-metapost
                                       texlive-charter
                                       texlive-pdftex
                                       texlive-context
                                       texlive-cm
                                       texlive-tex-plain))))
      (arguments
       (list
        #:make-flags
        #~(list (string-append "PREFIX=" #$output)
                (string-append "DOCDIR=" #$output "/share/doc/"
                               #$name "-" #$version)
                ;; lib/chez-scheme/chezweb ???
                (string-append "LIBDIR=" #$output "/lib/chezweb")
                (string-append "TEXDIR=" #$output "/share/texmf-local"))
        #:tests? #f ; no tests
        #:phases
        #~(modify-phases %standard-phases
            ;; This package has a custom "bootstrap" script that
            ;; is meant to be run from the Makefile.
            (delete 'bootstrap)
            (replace 'configure
              (lambda* _
                (copy-file "config.mk.template" "config.mk")
                (substitute* "tangleit"
                  (("\\./cheztangle\\.ss" all)
                   (string-append "scheme --program " all)))
                (substitute* "weaveit"
                  (("mpost chezweb\\.mp")
                   "mpost --tex=tex chezweb.mp")
                  (("\\./chezweave" all)
                   (string-append "scheme --program " all)))
                (substitute* "installit"
                  (("-g \\$GROUP -o \\$OWNER") "")))))))
      (home-page "https://github.com/arcfide/ChezWEB")
      (synopsis "Hygienic Literate Programming for Chez Scheme")
      (description "ChezWEB is a system for doing Knuthian style WEB
programming in Scheme.")
      (license expat))))

(define-public chez-sockets
  (let ((commit "bce96881c06bd69a6757a6bff139744153924140")
        (revision "1"))
    (package
      (name "chez-sockets")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/arcfide/chez-sockets")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1n5fbwwz51fdzvjackgmnsgh363g9inyxv7kmzi0469cwavwcx5m"))))
      (build-system gnu-build-system)
      (native-inputs
       (list (chez-scheme-for-system)
             chez-web
             (texlive-updmap.cfg (list texlive-pdftex))))
      (arguments
       (list
        #:tests? #f ; no tests
        #:phases
        #~(modify-phases %standard-phases
            (replace 'configure
              (lambda* (#:key native-inputs inputs #:allow-other-keys)
                (let* ((scheme (search-input-file (or native-inputs inputs)
                                                  "/bin/scheme"))
                       (lib (string-append (dirname scheme) "/../lib"))
                       (header-file (car (find-files lib "scheme\\.h")))
                       (include-dir (dirname header-file)))
                  (substitute* "Makefile"
                    (("(SCHEMEH=).*$" _ var)
                     (string-append var include-dir))))))
            (add-before 'build 'tangle
              (lambda* (#:key inputs #:allow-other-keys)
                (setenv "TEXINPUTS"
                        (string-append
                         (getcwd) ":"
                         (assoc-ref inputs "chez-web")
                         "/share/texmf-local/tex/generic:"
                         ":"))
                ;; just using "make" tries to build the .c files before
                ;; they are created.
                (and (invoke "make" "sockets")
                     (invoke "make"))))
            (replace 'build
              (lambda args
                (let ((chez-site (string-append #$output
                                                "/lib/chez-scheme/arcfide")))
                  ;; make sure Chez Scheme can find the shared libraries.
                  (substitute* "sockets.ss"
                    (("(object \")(socket-ffi-values\\.[sd][oy][^\"]*)(\")"
                      _ pre file post)
                     (string-append pre chez-site "/" file post))
                    (("(\")(sockets-stub\\.[sd][oy][^\"]*)(\")"
                      _ pre file post)
                     (string-append pre chez-site "/" file post)))
                  ;; to compile chez-sockets, the .so files must be
                  ;; installed (because of the absolute path we
                  ;; inserted above).
                  (for-each (lambda (f)
                              (install-file f chez-site))
                            '("socket-ffi-values.so"
                              "sockets-stub.so"))
                  (invoke "bash"
                          "-c"
                          (format #f "echo '~s' | scheme -q"
                                  '(compile-file "sockets.sls"))))))
            (replace 'install
              (lambda args
                (install-file "sockets.so"
                              (string-append #$output
                                             "/lib/chez-scheme/arcfide"))
                (install-file "sockets.pdf"
                              (string-append #$output
                                             "/share/doc/"
                                             #$name "-" #$version)))))))
      (home-page "https://github.com/arcfide/chez-sockets")
      (synopsis "Extensible sockets library for Chez Scheme")
      (description "Chez-sockets is an extensible sockets library for
Chez Scheme.")
      (license expat))))

(define-public chez-matchable
  (package
    (name "chez-matchable")
    (version "20160306")
    (home-page "https://github.com/fedeinthemix/chez-matchable")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit (string-append "v" version))))
       (sha256
        (base32 "02qn7x348p23z1x5lwhkyj7i8z6mgwpzpnwr8dyina0yzsdkr71s"))
       (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (inputs
     (list chez-srfi)) ; for tests
    (native-inputs
     (list (chez-scheme-for-system)))
    (arguments
     (list #:make-flags (chez-make-flags name version)
           #:test-target "test"
           #:phases #~(modify-phases %standard-phases
                        (replace 'configure
                          #$chez-configure))))
    (synopsis "Portable hygienic pattern matcher for Scheme")
    (description "This package provides a superset of the popular Scheme
@code{match} package by Andrew Wright, written in fully portable
@code{syntax-rules} and thus preserving hygiene.")
    (license public-domain)))

(define-public chez-irregex
  (package
    (name "chez-irregex")
    (version "0.9.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/fedeinthemix/chez-irregex")
             (commit (string-append "v" version))))
       (sha256
        (base32 "0jh6piylw545j81llay9wfivgpv6lcnwd81gm4w17lkasslir50q"))
       (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (inputs
     (list chez-matchable)) ; for tests
    (propagated-inputs
     (list chez-srfi)) ; for irregex-utils
    (native-inputs
     (list (chez-scheme-for-system)))
    (arguments
     (list #:make-flags (chez-make-flags name version)
           #:test-target "test"
           #:phases #~(modify-phases %standard-phases
                        (replace 'configure
                          #$chez-configure))))
    (home-page "https://github.com/fedeinthemix/chez-irregex")
    (synopsis "Portable regular expression library for Scheme")
    (description "This package provides a portable and efficient
R[4567]RS implementation of regular expressions, supporting both POSIX
syntax with various (irregular) PCRE extensions, as well as SCSH's SRE
syntax, with various aliases for commonly used patterns.")
    (license bsd-3)))

(define-public chez-fmt
  (package
    (name "chez-fmt")
    (version "0.8.11")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://synthcode.com/scheme/fmt/fmt-" version ".tar.gz"))
       (sha256
        (base32 "1zxqlw1jyg85yzclylh8bp2b3fwcy3l3xal68jw837n5illvsjcl"))
       (file-name (string-append name "-" version ".tar.gz"))))
    (build-system gnu-build-system)
    (propagated-inputs
     (list chez-srfi)) ; for irregex-utils
    (native-inputs
     (list (chez-scheme-for-system)))
    (arguments
     (list #:make-flags (chez-make-flags name version)
           #:test-target "chez-check"
           #:phases
           #~(modify-phases %standard-phases
               (replace 'configure
                 #$chez-configure)
               (replace 'build
                 (lambda* (#:key (make-flags '()) #:allow-other-keys)
                   (apply invoke "make" "chez-build" make-flags)))
               (replace 'install
                 (lambda* (#:key (make-flags '()) #:allow-other-keys)
                   (apply invoke "make" "chez-install" make-flags))))))
    (home-page "http://synthcode.com/scheme/fmt")
    (synopsis "Combinator formatting library for Chez Scheme")
    (description "This package provides a library of procedures for
formatting Scheme objects to text in various ways, and for easily
concatenating, composing and extending these formatters efficiently
without resorting to capturing and manipulating intermediate
strings.")
    (license bsd-3)))

(define-public chez-mit
  (package
    (name "chez-mit")
    (version "0.1")
    (home-page "https://github.com/fedeinthemix/chez-mit")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit (string-append "v" version))))
       (sha256
        (base32 "0c7i3b6i90xk96nmxn1pc9272a4yal4v40dm1a4ybdi87x53zkk0"))
       (file-name (git-file-name name version))
       (snippet
        ;; Workaround for chez-scheme-for-racket.
        ;; See: https://github.com/racket/racket/issues/4151
        #~(begin
            (use-modules (guix build utils))
            (substitute* "mit/core.sls"
              (("[(]import ")
               "(import (only (chezscheme) import)\n")
              (("[(]define string->uninterned-symbol gensym[)]")
               (format #f "~s"
                       '(begin
                          (import (only (chezscheme)
                                        meta-cond
                                        library-exports))
                          (meta-cond
                           ((memq 'string->uninterned-symbol
                                  (library-exports '(chezscheme)))
                            (import (only (chezscheme)
                                          string->uninterned-symbol)))
                           (else
                            (define string->uninterned-symbol
                              gensym)))))))))))
    (build-system gnu-build-system)
    (inputs
     (list chez-srfi))       ; for tests
    (native-inputs
     (list (chez-scheme-for-system)))
    (arguments
     (list #:make-flags (chez-make-flags name version)
           #:test-target "test"
           #:phases #~(modify-phases %standard-phases
                        (replace 'configure
                          #$chez-configure))))
    (synopsis "MIT/GNU Scheme compatibility library for Chez Scheme")
    (description "This package provides a set of MIT/GNU Scheme compatibility
libraries for Chez Scheme.  The main goal was to provide the functionality
required to port the program @code{Scmutils} to Chez Scheme.")
    (license gpl3+)))

(define-public chez-scmutils
  (package
    (name "chez-scmutils")
    (version "0.1")
    (home-page "https://github.com/fedeinthemix/chez-scmutils")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit (string-append "v" version))))
       (sha256
        (base32 "0lb05wlf8qpgg8y0gdsyaxg1nbfx1qbaqdjvygrp64ndn8fnhq7l"))
       (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (inputs
     (list chez-srfi))       ; for tests
    (native-inputs
     (list (chez-scheme-for-system)))
    (propagated-inputs
     (list chez-mit chez-srfi))
    (arguments
     (list
      #:make-flags (chez-make-flags name version)
      #:tests? #f  ; no test suite
      #:phases
      #~(modify-phases %standard-phases
          (replace 'configure
            #$chez-configure)
          ;; Since the documentation is lacking, we install the source
          ;; code.  For things to work correctly we have to replace
          ;; relative paths by absolute ones in 'include' forms.  This
          ;; in turn requires us to compile the files in the final
          ;; destination.
          (delete 'build)
          (add-after 'install 'install-src
            (lambda* (#:key (make-flags '()) #:allow-other-keys)
              (apply invoke "make" "install-src" make-flags)))
          (add-after 'install-src 'absolute-path-in-scm-files
            (lambda* (#:key #:allow-other-keys)
              (for-each (lambda (file)
                          (substitute* file
                            (("include +\"\\./scmutils")
                             (string-append "include \"" (dirname file)))))
                        (find-files #$output "\\.sls"))
              (for-each (lambda (file)
                          (substitute* file
                            (("include +\"\\./scmutils/simplify")
                             (string-append "include \"" (dirname file)))))
                        (find-files #$output "fbe-syntax\\.scm"))))
          (add-after 'absolute-path-in-scm-files 'build
            (lambda* (#:key (make-flags '()) #:allow-other-keys)
              (let ((mk-file (car (find-files #$output "Makefile"))))
                (with-directory-excursion (dirname mk-file)
                  (apply invoke "make" "build" make-flags)))))
          (add-after 'build 'clean-up
            (lambda args
              (for-each delete-file
                        (find-files #$output
                                    "Makefile|compile-all\\.ss")))))))
    (synopsis "Port of MIT/GNU Scheme Scmutils to Chez Scheme")
    (description "This package provides a port of the MIT/GNU Scheme
Scmutils program to Chez Scheme.  The port consists of a set of
libraries providing most of the functionality of the original.")
    (license gpl3+)))
