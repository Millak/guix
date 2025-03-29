;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2016, 2024 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Brett Gilio <brettg@gnu.org>
;;; Copyright © 2020 Brendan Tildesley <mail@brendan.scot>
;;; Copyright © 2021-2024 Philip McGrath <philip@philipmcgrath.com>
;;; Copyright © 2024 Ashish SHUKLA <ashish.is@lostca.se>
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
  #:use-module (guix deprecation)
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
            nix-system->native-chez-machine-type
            nix-system->pbarch-machine-type
            unpack-nanopass))

;; Commentary:
;;
;; The bootstrapping paths for Chez Scheme and Racket are closely
;; entwined. See "Bootstrapping Racket" in the commentary on "racket.scm" for
;; details on the Racket portion of Chez Scheme's bootstrapping path.
;;
;; Chez Scheme is a self-hosting compiler. A small kernel implemented in C
;; loads "boot files" (in a custom object file format) compiled from the parts
;; of the system implemented in Chez Scheme. (While Chez Scheme generates
;; native machine code, it implements its own linker and loader.)
;;
;; As of Chez Scheme 10.0.0 (and the pre-release versions that preceded it on
;; the Racket branch), there are several ways to obtain boot files:
;;
;;  1. The Racket package "cs-bootstrap" (in the "racket/src/rktboot/"
;;     directory of the main Racket Git repository) simulates enough of Chez
;;     Scheme to load the Chez Scheme compiler purely from source into Racket
;;     and apply the compiler to itself, producing the needed boot files
;;     (albeit very slowly).
;;     Any variant of Racket since version 7.1 can run the simulation. Using
;;     the older Racket BC implementation, which does not depend on Chez
;;     Scheme, breaks the dependency cycle.
;;     However, the simulation relies on implementation details of Chez
;;     Scheme, so a given version of Chez Scheme can only be bootstrapped by
;;     the corresponding version of the "cs-bootstrap" package.
;;
;;  2. The Chez Scheme makefile provides a "re.boot" target for bootstrapping
;;     via a different version of Chez Scheme (9.5.4 or later).
;;     This path manages potential differences in implementation details
;;     across Chez Scheme versions using a strategy similar to "cs-bootstrap",
;;     but the compatibility shim is maintained with the Chez Scheme source
;;     code (in "s/reboot.ss"). Also, it's faster, since less indirection is
;;     needed.
;;
;;  3. For cross-compilation, or with an extremely similar Chez Scheme, the
;;     makefile provides "cross.boot" and related targets.
;;
;;  4. The Chez Scheme Git repository includes pre-built "pb" (portable
;;     bytecode) boot files, which can be used for bootstrapping on any
;;     platform, but these binary files are removed from the source Guix uses.
;;
;; Concretely, we use racket-vm-bc to bootstrap chez-scheme-for-racket, which
;; we then use to bootstrap both chez-scheme and racket-vm-cs.
;;
;; In principle, it would be possible instead to use chez-scheme to bootstrap
;; chez-scheme-for-racket. However, since Racket is ultimately used for
;; bootstrapping, chez-scheme would still need to be rebuilt when Racket
;; changes, whereas treating chez-scheme as a leaf avoids having to rebuild
;; Racket when upstream Chez Scheme changes. Furthermore, since "cs-bootstrap"
;; is developed in the Racket source repository, we don't have to look for the
;; version of "cs-bootstrap" compatible with the upstream Chez Scheme release.
;;
;; Code:

(define-deprecated (chez-scheme-for-system #:optional system) chez-scheme
  "Returns 'chez-scheme'."
  chez-scheme)

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
    "rv64")
   ((string-prefix? "loongarch64-" system)
    "la64")
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

(define-syntax define-machine-types
  (lambda (stx)
    (syntax-case stx (any)
      ((_ any id0 id ...)
       #`(define #,(datum->syntax #'id0 '%machine-types)
           '(id0 id ...))))))
;; The following is copied from s/cmacros.ss, line 36, in the Chez source
(define-machine-types
  any
  pb        tpb
  pb32l     tpb32l
  pb32b     tpb32b
  pb64l     tpb64l
  pb64b     tpb64b
  i3nt      ti3nt
  i3osx     ti3osx
  i3le      ti3le
  i3fb      ti3fb
  i3ob      ti3ob
  i3nb      ti3nb
  i3s2      ti3s2
  i3qnx     ti3qnx
  i3gnu     ti3gnu
  a6nt      ta6nt
  a6osx     ta6osx
  a6le      ta6le
  a6fb      ta6fb
  a6ob      ta6ob
  a6nb      ta6nb
  a6s2      ta6s2
  ppc32osx  tppc32osx
  ppc32le   tppc32le
  ppc32fb   tppc32fb
  ppc32ob   tppc32ob
  ppc32nb   tppc32nb
  arm32le   tarm32le
  arm32fb   tarm32fb
  arm32ob   tarm32ob
  arm32nb   tarm32nb
  arm64nt   tarm64nt
  arm64osx  tarm64osx
  arm64le   tarm64le
  arm64fb   tarm64fb
  arm64ob   tarm64ob
  arm64nb   tarm64nb
  rv64le    trv64le
  rv64fb    trv64fb
  rv64ob    trv64ob
  rv64nb    trv64nb
  la64le    tla64le
)

(define* (nix-system->pbarch-machine-type #:optional
                                          (system
                                           (or (%current-target-system)
                                               (%current-system)))
                                          #:key (threads? #t))
  "Return a string naming the Chez Scheme machine type of the appropriate
``pbarch'' backend for SYSTEM: that is, the ``portable bytecode'' backend
specialized for SYSTEM's word size and endianness.  The result will name the
threaded machine type unless THREADS? is provided as #f."
  (string-append (if threads?
                     "t"
                     "")
                 "pb"
                 (if (target-64bit? system)
                     "64"
                     "32")
                 (if (target-little-endian? system)
                     "l"
                     "b")))

(define* (nix-system->native-chez-machine-type #:optional
                                               (system
                                                (or (%current-target-system)
                                                    (%current-system)))
                                               #:key (threads? #t))
  "Return a string naming the Chez Scheme machine type of the native-code
backend for SYSTEM, if such a native-code backend exists.  Otherwise, when
SYSTEM can use only the ``portable bytecode'' backends, return #f.  The result
will name the threaded machine type unless THREADS? is provided as #f."
  (let* ((chez-arch (target-chez-arch system))
         (chez-os (target-chez-os system))
         (machine
          (and chez-arch chez-os
               (string-append (if threads? "t" "") chez-arch chez-os))))
    (and machine
         (memq (string->symbol machine) %machine-types)
         machine)))
;;
;; Chez Scheme:
;;

(define unpack-nanopass
  #~(lambda* (#:key inputs native-inputs #:allow-other-keys)
      (with-directory-excursion (if (directory-exists? "racket/src/ChezScheme")
                                    "racket/src/ChezScheme"
                                    ".")
        (symlink (dirname (search-input-file (or native-inputs inputs)
                                             "lib/chez-scheme/nanopass.ss"))
                 "nanopass"))))

(define chez-configure
  ;; The custom Chez 'configure' script doesn't allow unrecognized flags, such
  ;; as those automatically added by `gnu-build-system`. This replacement
  ;; phase uses only the explicitly provided `#:configure-flags`.
  #~(lambda* (#:key inputs (configure-flags '()) out-of-source?
              #:allow-other-keys)
      (let* ((abs-srcdir (getcwd))
             (srcdir (if out-of-source?
                         (string-append "../" (basename abs-srcdir))
                         ".")))
        (format #t "source directory: ~s (relative from build: ~s)~%"
                abs-srcdir srcdir)
        (when out-of-source?
            (begin
              (mkdir "../build")
              (chdir "../build")))
        (format #t "build directory: ~s~%" (getcwd))
        (format #t "configure flags: ~s~%" configure-flags)
        (apply invoke
               (string-append srcdir "/configure")
               configure-flags))))

(define-public chez-scheme-for-racket
  (package
    (name "chez-scheme-for-racket")
    ;; The version should match `(scheme-version #t)`.
    ;; See s/cmacros.ss c. line 360.
    (version "10.1.0-pre-release.3")
    (source #f)
    (build-system gnu-build-system)
    (inputs `(,@(if (nix-system->native-chez-machine-type)
                    '()
                    (list libffi))
              ,chez-scheme-for-racket-bootstrap-bootfiles
              ,lz4
              ,ncurses ;<-- for expeditor
              ,zlib))
    (native-inputs `(,@(if (%current-target-system)
                           (list this-package
                                 `(,this-package "doc"))
                           (list stex-bootstrap
                                 (texlive-local-tree
                                  (list texlive-enumitem
                                        texlive-etoolbox))))
                     ,chez-nanopass-bootstrap
                     ,zuo))
    (native-search-paths
     (list (search-path-specification
            (variable "CHEZSCHEMELIBDIRS")
            (files '("lib/chez-scheme")))))
    (outputs '("out" "debug" "doc"))
    (arguments
     (list
      #:modules
      '((guix build gnu-build-system)
        (guix build utils)
        (ice-9 ftw)
        (ice-9 match)
        (srfi srfi-34))
      #:out-of-source? #t
      #:test-target "test" ; test-one test-some-fast test-some test test-more
      ;; Tests take more than 30 hours on riscv64.
      #:tests? (and (not (target-riscv64?))
                    (not (%current-target-system)))
      #:configure-flags
      #~`(,@(let* ((chez+version (strip-store-file-name #$output))
                   (doc-dir (string-append #$output:doc
                                           "/share/doc/"
                                           chez+version)))
              (list (string-append "--installcsug="
                                   doc-dir
                                   "/csug")
                    (string-append "--installreleasenotes="
                                   doc-dir
                                   "/release_notes")))
          ,(string-append "--installprefix=" #$output)
          #$@(if (%current-target-system)
                 (list (string-append "--toolprefix="
                                      (%current-target-system)
                                      "-"))
                 '())
          ,@(if (false-if-exception
                 (search-input-directory %build-inputs "/include/X11"))
                '()
                '("--disable-x11"))
          #$(string-append "-m=" (or (nix-system->native-chez-machine-type)
                                     (nix-system->pbarch-machine-type)))
          ;; ^ could skip -m= for non-cross non-pbarch builds
          #$@(if (nix-system->native-chez-machine-type)
                 #~()
                 ;; not inferred on non-native platforms: see
                 ;; https://racket.discourse.group/t/950/9
                 #~("--enable-libffi"
                    "CFLAGS+=-g -D_REENTRANT -pthread"
                    "LIBS+=-lm -ldl -lrt -lncurses"))
          ,(string-append "STEXLIB="
                          (or (false-if-exception
                               (search-input-directory %build-inputs
                                                       "/lib/stex"))
                              "/GuixNotUsingStex"))
          "ZUO=zuo"
          "ZLIB=-lz"
          "LZ4=-llz4"
          ;; Guix will do 'compress-man-pages',
          ;; and letting Chez try caused an error (at one point)
          "--nogzip-man-pages")
      #:phases
      #~(modify-phases %standard-phases
          (replace 'unpack
            (let ((unpack (assoc-ref %standard-phases 'unpack)))
              (lambda args
                (unpack #:source #$(or (package-source this-package)
                                       (package-source racket-vm-bc))))))
          (add-after 'unpack 'unpack-nanopass
            #$unpack-nanopass)
          (add-after 'unpack-nanopass 'maybe-chdir
            (lambda args
              (when (directory-exists? "racket/src/ChezScheme")
                (chdir "racket/src/ChezScheme"))))
          (add-after 'maybe-chdir 'unpack-bootfiles
            (lambda* (#:key native-inputs inputs #:allow-other-keys)
              (when (directory-exists? "boot")
                (delete-file-recursively "boot"))
              (copy-recursively
               (search-input-directory inputs "lib/chez-scheme-bootfiles")
               "boot")))
          (replace 'configure
            #$chez-configure)
          (add-after 'configure 'configure-environment-variables
            (lambda args
              ;; mats/6.ms needs HOME to be set:
              (setenv "HOME" "/tmp")))
          (replace 'build
            ;; need to override target for cross-compilation
            ;; https://racket.discourse.group/t/950/19
            (let ((gnu:build (assoc-ref %standard-phases 'build)))
              (lambda* (#:key target (make-flags '()) (parallel-build? #t)
                        #:allow-other-keys)
                (gnu:build #:make-flags (if target
                                            (cons "kernel" make-flags)
                                            make-flags)
                           #:parallel-build? parallel-build?))))
          (add-before 'check 'build-docs
            ;; Building the documentation requires stex and a running scheme.
            ;; This comes BEFORE 'check because the test suite may take on the
            ;; order of an hour (without parallelism), so we want to get any
            ;; other errors first.
            ;; TODO: Improve cross support upstream: currently, it tries to
            ;; run the cross-compiled scheme.
            (lambda* (#:key native-inputs (make-flags '()) #:allow-other-keys)
              #$(if (%current-target-system)
                    #~(format #t "not building docs for cross~%")
                    #~(apply invoke "make" "docs" make-flags))))
          ;; The binary file name is called "scheme" as is the one from
          ;; MIT/GNU Scheme.  We add a symlink to use in case both are
          ;; installed.
          (add-after 'install 'install-symlink
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((bin-dir
                      (dirname (search-input-file outputs "/bin/scheme")))
                     (boot-dir
                      (match (find-files (string-append bin-dir "/../lib")
                                         "scheme.boot")
                        ((scheme.boot)
                         (dirname scheme.boot)))))
                (for-each (lambda (dir)
                            (with-directory-excursion dir
                              (symlink "./scheme" "chez-scheme")
                              (when (file-exists? "scheme.boot")
                                (symlink "./scheme.boot" "chez-scheme.boot"))))
                          (list bin-dir boot-dir)))))
          (add-after 'install-symlink 'install-docs
            ;; TODO: Improve cross support upstream.
            ;; The `install-docs` target has a Zuo dependency on the `docs`
            ;; target, so we have the same problem as the build-docs phase.
            (lambda* (#:key native-inputs (make-flags '()) #:allow-other-keys)
              #$(if (%current-target-system)
                    #~(let* ((rel
                              (string-append "share/doc/"
                                             (strip-store-file-name #$output)))
                             (found/csug
                              (search-input-directory
                               native-inputs (string-append rel "/csug")))
                             (found (substring found/csug
                                               0
                                               (- (string-length found/csug)
                                                  (string-length "/csug"))))
                             (dest (string-append #$output:doc "/" rel)))
                        (mkdir-p dest)
                        (with-directory-excursion dest
                          (for-each (lambda (f)
                                      (symlink (string-append found "/" f)
                                               f))
                                    '("csug" "csug.pdf"
                                      "release_notes" "release_notes.pdf"))))
                    #~(apply invoke "make" "install-docs" make-flags))))
          (add-after 'install-docs 'link-doc-pdfs
            ;; otherwise, it's hard to notice them in a forest of HTML files
            ;; TODO: improve cross support upstream.
            (lambda* (#:key outputs #:allow-other-keys)
              #$(if (%current-target-system)
                    #~(format #t "nothing to be done for cross~%")
                    #~(with-directory-excursion
                          (string-append (or (assoc-ref outputs "doc")
                                             (assoc-ref outputs "out"))
                                         "/share/doc/"
                                         (strip-store-file-name #$output))
                        (symlink "release_notes/release_notes.pdf"
                                 "release_notes.pdf")
                        (match (find-files
                                "csug"
                                "csug.*\\.pdf$" ;; embedded version number
                                #:fail-on-error? #t)
                          ((pth)
                           (symlink pth
                                    "csug.pdf"))))))))))
    (home-page "https://cisco.github.io/ChezScheme/")
    (synopsis "Bootstrapping version of Chez Scheme")
    (description
     "This is the precise pre-release version of Chez Scheme from a specific
Racket release.  It is used to build Racket and to bootstrap the released
version of Chez Scheme.")
    (license asl2.0)))

(define-public chez-scheme
  (package
    (inherit chez-scheme-for-racket)
    (name "chez-scheme")
    ;; The version should match `(scheme-version-number #t)`.
    ;; See s/cmacros.ss c. line 360.
    (version "10.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/cisco/ChezScheme")
                    (commit (string-append "v" version))))
              (sha256
               (base32
                "0y07n2has2592g41zgl275as2wbw1fqd2y7a34llpbqnfpc7923s"))
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
                                     '("boot"
                                       "lz4"
                                       "nanopass"
                                       "stex"
                                       "zlib"
                                       "zuo"))))))
    (build-system gnu-build-system)
    (inputs
     (modify-inputs (package-inputs chez-scheme-for-racket)
       (replace "chez-scheme-for-racket-bootstrap-bootfiles"
         chez-scheme-bootstrap-bootfiles)
       ;; for X11 clipboard support in expeditor:
       ;; https://github.com/cisco/ChezScheme/issues/9#issuecomment-222057232
       (prepend libx11)))
    ;; replace unpack phase?
    (home-page "https://cisco.github.io/ChezScheme/")
    (synopsis "R6RS Scheme compiler and run-time")
    (description
     "Chez Scheme is both a programming language and a high-performance
implementation of that language.  The language is a superset of R6RS Scheme
with numerous extensions, including native threads, non-blocking I/O, local
modules, and much more.  Chez Scheme compiles source expressions incrementally
to machine code, providing the speed of compiled code in an interactive
system.  The system is intended to be as reliable and efficient as possible,
with reliability taking precedence over efficiency if necessary.")
    (license asl2.0)))

;;
;; Bootfiles:
;;

(define-public chez-scheme-for-racket-bootstrap-bootfiles
  (package
    (name "chez-scheme-for-racket-bootstrap-bootfiles")
    (version (package-version chez-scheme-for-racket))
    (source #f) ; avoid problematic cycle with racket.scm
    (native-inputs
     (cons chez-nanopass-bootstrap
           (if (%current-target-system)
               (list zuo
                     chez-scheme-for-racket)
               (list racket-vm-bc))))
    (build-system copy-build-system)
    (arguments
     (list
      #:install-plan
      #~`(("boot/" "lib/chez-scheme-bootfiles"))
      #:phases
      #~(modify-phases %standard-phases
          (replace 'unpack
            (let ((unpack (assoc-ref %standard-phases 'unpack)))
              (lambda args
                (unpack #:source #$(or (package-source this-package)
                                       (package-source racket-vm-bc))))))
          (add-after 'unpack 'unpack-nanopass
            #$unpack-nanopass)
          (add-after 'unpack-nanopass 'chdir
            (lambda args
              (chdir "racket/src/ChezScheme")))
          (add-before 'install 'build
            #$(cond
               ((%current-target-system)
                ;; cross-compiling
                ;; TODO: share more of this with upstream, once we figure out
                ;; how best to manage running Chez as a cross-compiler and the
                ;; unfortate cycle with %racket-origin.
                #~(lambda* (#:key native-inputs inputs (parallel-build? #t)
                            #:allow-other-keys)
                    (invoke "./configure"
                            "--force" ; don't complain about missing bootfiles
                            #$(string-append
                               "-m=" (or (nix-system->native-chez-machine-type)
                                         (nix-system->pbarch-machine-type)))
                            "ZUO=zuo"
                            ;; ignore submodules:
                            "ZLIB=-lz"
                            "LZ4=-llz4"
                            "STEXLIB=/GuixNotUsingStex")
                    (apply invoke
                           "make"
                           `(,@(if parallel-build?
                                   `("-j" ,(number->string
                                            (parallel-job-count)))
                                   '())
                             ,(string-append "SCHEME="
                                             (search-input-file
                                              (or native-inputs inputs)
                                              "/bin/scheme"))
                             "cross.boot"))))
               (else
                ;; bootstrapping
                #~(lambda* (#:key native-inputs inputs #:allow-other-keys)
                     ;; Make sure we're building for the correct machine type.
                     (setenv "MACH"
                             #$@(if (nix-system->native-chez-machine-type)
                                    #~(#$(nix-system->native-chez-machine-type))
                                    #~(#$(nix-system->pbarch-machine-type))))
                    (invoke
                     (search-input-file (or native-inputs inputs)
                                        "/opt/racket-vm/bin/racket")
                     "../rktboot/main.rkt"))))))))
    (home-page "https://pkgs.racket-lang.org/package/cs-bootstrap")
    (synopsis "Chez Scheme boot files bootstrapped by Racket")
    (description "Chez Scheme is a self-hosting compiler: building it requires
``boot files'' containing the Scheme-implemented portions compiled for the
current platform.  (Chez can then cross-compile boot files for all other
supported platforms.)

The Racket package @code{cs-bootstrap} (part of the main Racket Git
repository) implements enough of a Chez Scheme simulation to load the Chez
Scheme compiler purely from source into Racket and apply the compiler to
itself, thus bootstrapping Chez Scheme.  Bootstrapping takes about 10 times as
long as using an existing Chez Scheme, but @code{cs-bootstrap} supports Racket
7.1 and later, including the Racket BC variant.")
    (license asl2.0)))

(define-public chez-scheme-bootstrap-bootfiles
  (package
    (name "chez-scheme-bootstrap-bootfiles")
    (version (package-version chez-scheme))
    (source (package-source chez-scheme))
    (native-inputs (list chez-nanopass-bootstrap
                         (if (%current-target-system)
                             chez-scheme
                             chez-scheme-for-racket)
                         zuo))
    (outputs '("out"))
    (build-system gnu-build-system)
    (arguments
     (list
      #:configure-flags
      #~`("--force" ; don't complain about missing bootfiles
          "ZLIB=-lz" "LZ4=-llz4" "STEXLIB=/GuixNotUsingStex" ; ignore submods
          "ZUO=zuo"
          ;; could skip -m= for non-cross non-pbarch builds
          #$(string-append "-m=" (or (nix-system->native-chez-machine-type)
                                     (nix-system->pbarch-machine-type))))
      #:make-flags
      #~(list (string-append "SCHEME="
                             (search-input-file %build-inputs "/bin/scheme"))
              #$(if (%current-target-system)
                    "cross.boot"
                    "re.boot"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'unpack-nanopass
            #$unpack-nanopass)
          (replace 'configure
            #$chez-configure)
          (delete 'check)
          (replace 'install
            (lambda args
              (mkdir-p (string-append #$output "/lib"))
              (copy-recursively
               "boot"
               (string-append #$output "/lib/chez-scheme-bootfiles")))))))
    (home-page "https://cisco.github.io/ChezScheme/")
    (synopsis "Bootstrapped Chez Scheme boot files")
    (description
     "Chez Scheme is a self-hosting compiler: building it requires ``boot
files'' containing the Scheme-implemented portions compiled for the current
platform.  (Chez can then cross-compile bootfiles for all other supported
platforms.)

This package provides boot files for the released version of Chez Scheme
bootstrapped by @code{chez-scheme-for-racket}.  Chez Scheme 9.5.4 or any later
version can be used for bootstrapping.  Guix ultimately uses the Racket
package @code{cs-bootstrap} to bootstrap its initial version of Chez Scheme.")
    (license asl2.0)))

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
              (texlive-local-tree (list texlive-epsf))
              ghostscript
              netpbm))
       ;; Debian uses a versionless path for STEXLIB,
       ;; which is much more convenient.
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
(define configure-chezschemelibdirs
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
     (list chez-scheme))
    (arguments
     (list #:make-flags (chez-make-flags name version)
           #:test-target "test"
           #:phases #~(modify-phases %standard-phases
                        (replace 'configure
                          #$configure-chezschemelibdirs))))
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
       (list chez-scheme
             ghostscript
             (texlive-local-tree
              (list texlive-charter
                    texlive-context
                    texlive-cweb
                    texlive-metapost))))
      (arguments
       (list
        #:make-flags
        #~(list (string-append "PREFIX=" #$output)
                (string-append "DOCDIR=" #$output "/share/doc/"
                               #$name "-" #$version)
                ;; lib/chez-scheme/chezweb ???
                (string-append "LIBDIR=" #$output "/lib/chezweb")
                (string-append "TEXDIR=" #$output "/share/texmf-local"))
        #:tests? #f                     ; no tests
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'fix-tex-input
              (lambda _
                ;; Fix "I can't find file `supp-pdf'." error.
                (substitute* "chezweb.w"
                  (("supp-pdf") "supp-pdf.mkii"))
                ;; Recent cweb packages do not include "\acrofalse".  Remove
                ;; it.
                (substitute* "doc/cwebman.tex"
                  (("\\acrofalse.*") ""))))
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
       (list chez-scheme
             chez-web
             (texlive-local-tree)))
      (arguments
       (list
        #:tests? #f                     ; no tests
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
     (list chez-scheme))
    (arguments
     (list #:make-flags (chez-make-flags name version)
           #:test-target "test"
           #:phases #~(modify-phases %standard-phases
                        (replace 'configure
                          #$configure-chezschemelibdirs))))
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
     (list chez-scheme))
    (arguments
     (list #:make-flags (chez-make-flags name version)
           #:test-target "test"
           #:phases #~(modify-phases %standard-phases
                        (replace 'configure
                          #$configure-chezschemelibdirs))))
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
     (list chez-scheme))
    (arguments
     (list #:make-flags (chez-make-flags name version)
           #:test-target "chez-check"
           #:phases
           #~(modify-phases %standard-phases
               (replace 'configure
                 #$configure-chezschemelibdirs)
               (replace 'build
                 (lambda* (#:key (make-flags '()) #:allow-other-keys)
                   (apply invoke "make" "chez-build" make-flags)))
               (replace 'install
                 (lambda* (#:key (make-flags '()) #:allow-other-keys)
                   (apply invoke "make" "chez-install" make-flags))))))
    (home-page "https://synthcode.com/scheme/fmt")
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
     (list chez-scheme))
    (arguments
     (list #:make-flags (chez-make-flags name version)
           #:test-target "test"
           #:phases #~(modify-phases %standard-phases
                        (replace 'configure
                          #$configure-chezschemelibdirs))))
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
     (list chez-scheme))
    (propagated-inputs
     (list chez-mit chez-srfi))
    (arguments
     (list
      #:make-flags (chez-make-flags name version)
      #:tests? #f  ; no test suite
      #:phases
      #~(modify-phases %standard-phases
          (replace 'configure
            #$configure-chezschemelibdirs)
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
