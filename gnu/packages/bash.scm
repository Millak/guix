;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012-2017, 2019-2020, 2022, 2023 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014, 2015, 2018 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015, 2017 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016, 2017, 2018, 2019, 2022 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018, 2019, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2020 Zhu Zihao <all_but_last@163.com>
;;; Copyright © 2021 Marius Bakke <marius@gnu.org>
;;; Copyright © 2024 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2024 Janneke Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2024 chris <chris@bumblehead.com>
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

(define-module (gnu packages bash)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages less)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (guix store)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:autoload   (guix gnupg) (gnupg-verify*)
  #:autoload   (guix base32) (bytevector->nix-base32-string)

  ;; See <https://bugs.gnu.org/41457> for why not #:autoload here.
  #:use-module ((gcrypt hash) #:select (port-sha256))

  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 format))

(define (patch-url seqno)
  "Return the URL of Bash patch number SEQNO."
  (format #f "mirror://gnu/bash/bash-5.1-patches/bash51-~3,'0d" seqno))

(define (bash-patch seqno sha256-bv)
  "Return the origin of Bash patch SEQNO, with expected hash SHA256-BV."
  (origin
    (method url-fetch)
    (uri (patch-url seqno))
    (sha256 sha256-bv)))

(define-syntax-rule (patch-series (seqno hash) ...)
  (list (bash-patch seqno (base32 hash))
        ...))

(define %patch-series-5.1
  ;; This is the current patches series for 5.1, generated using
  ;; 'download-patches' below.
  (patch-series
   (1 "1ymm8ppss6gyh9ifznjwiabrb4k91npd09c10y7mk66xp8yppc7b")
   (2 "1gjx9zqcm407am3n2sh44b8dxm48kgm15rzfiijqxr01m0hn3shm")
   (3 "1cdnpbfc64yhvkjj4d12s9ywp11g195vzfl1cab24sq55wkcrwi2")
   (4 "11iwhy6v562bv0kk7lwj7f5jj65ma9bblivy0v02h3ggcibbdbls")
   (5 "19bdyigdr81824nxvqr6a7k0cax60wq7376j6b91afbnwvlvbjyc")
   (6 "051x8wlwrqk0yr0zg378vh824iklfl5g9pkmcdf62qp8gn9pvqbm")
   (7 "0fir80pp1gmlpadmqcgkrv4y119pc7xllchjzg05fd7px73viz5c")
   (8 "1lfjgshk8i9vch92p5wgc9r90j3phw79aa7gbai89w183b2z6b7j")
   (9 "1vn36dzd9g4y1h3jiss6418crla0rbcd0d6wwsyv9d5l7aaxlp74")
   (10 "0amfmvbzsand7bdypylkjdpcp88fa3cplfshn7vyzv2ff2rdgj52")
   (11 "0yq24abb4fzfxqnwl20b330sxl9lr9ds0nc4yi30f81l94b1y6aq")
   (12 "165bff97ffih49vfs4mkr5w3z5gn1w6zfyrf773iajkw6v48kw8h")
   (13 "1bfmgv3lagbk3aq9a831d29xv7jz4sjq7jhn9hq89limyinvdb67")
   (14 "1l43dw4kpddn7l41i8wmj406z9abxky1wb3rk8krcys33g4f0kka")
   (15 "1w40vzadzx019v0zhs4q6yqycrk04x1k8xs6qb73vk7ny4p6jdqv")
   (16 "0krqqljz4bkp9wrdnwfx51bxkb8rkwf8ivc93as1znx5fr7i96c8")))

(define (download-patches store count)
  "Download COUNT Bash patches into store.  Return a list of
number/base32-hash tuples, directly usable in the 'patch-series' form."
  (unfold (cut > <> count)
          (lambda (number)
            (let* ((patch  (download-to-store store (patch-url number)))
                   (sig    (download-to-store store
                                              (string-append (patch-url number)
                                                             ".sig"))))
              (unless (eq? 'valid-signature (gnupg-verify* sig patch))
                (error "failed to verify signature" patch))

              (list number
                    (bytevector->nix-base32-string
                     (call-with-input-file patch port-sha256)))))
          1+
          1))

(define-public bash
  (let* ((cppflags (string-join '("-DDEFAULT_PATH_VALUE='\"/no-such-path\"'"
                                  "-DSTANDARD_UTILS_PATH='\"/no-such-path\"'"
                                  "-DNON_INTERACTIVE_LOGIN_SHELLS"
                                  "-DSSH_SOURCE_BASHRC")
                                " "))
         (configure-flags
          ``("--without-bash-malloc"
             "--with-installed-readline"
             ,,(string-append "CPPFLAGS=" cppflags)
             ,(string-append
               "LDFLAGS=-Wl,-rpath -Wl,"
               (assoc-ref %build-inputs "readline")
               "/lib"
               " -Wl,-rpath -Wl,"
               (assoc-ref %build-inputs "ncurses")
               "/lib")))
         (version "5.1"))
    (package
      (name "bash")
      (source (origin
                (method url-fetch)
                (uri (string-append
                      "mirror://gnu/bash/bash-" version ".tar.gz"))
                (sha256
                 (base32
                  "1alv68wplnfdm6mh39hm57060xgssb9vqca4yr1cyva0c342n0fc"))
                (patch-flags '("-p0"))
                (patches (cons (search-patch "bash-linux-pgrp-pipe.patch")
                               %patch-series-5.1))))
      (version (string-append version "." (number->string (length %patch-series-5.1))))
      (build-system gnu-build-system)

      (outputs '("out"
                 "doc"                  ;1.7 MiB of HTML and extra files
                 "include"))            ;headers used by extensions
      (native-inputs (if (target-loongarch64?)
                         (list config)
                         '()))
      (inputs (list readline ncurses))  ;TODO: add texinfo
      (arguments
       `( ;; When cross-compiling, `configure' incorrectly guesses that job
         ;; control is missing.
         #:configure-flags ,(if (%current-target-system)
                                `(cons* "bash_cv_job_control_missing=no"
                                        ,configure-flags)
                                configure-flags)

         ;; Bash is reportedly not parallel-safe.  See, for instance,
         ;; <http://patches.openembedded.org/patch/32745/> and
         ;; <http://git.buildroot.net/buildroot/commit/?h=79e2d802a>.
         #:parallel-build? #f
         #:parallel-tests? #f

         ;; XXX: The tests have a lot of hard-coded paths, so disable them
         ;; for now.
         #:tests? #f

         #:modules ((srfi srfi-26)
                    (guix build utils)
                    (guix build gnu-build-system))

         #:phases
         (modify-phases %standard-phases
           ,@(if (and (target-hurd?) (not (system-hurd?)))
                 `((add-after 'configure 'create-pipesize.h
                     ;; The Bash Makefile mentions how PIPESIZE calculation is
                     ;; "technically wrong" when cross-compiling, and offers no
                     ;; way to override it.  On the 64bit Hurd, it can make
                     ;; bash hang.
                     (lambda _
                       (with-directory-excursion "builtins"
                         (with-output-to-file "psize.aux"
                           (lambda _ (display "dummy to pacify make\n")))
                         (with-output-to-file "pipesize.h"
                           (lambda _ (display "#define PIPESIZE 16384\n")))))))
                 '())

           (add-after 'install 'install-sh-symlink
             (lambda* (#:key outputs #:allow-other-keys)
               ;; Add a `sh' -> `bash' link.
               (let ((out (assoc-ref outputs "out")))
                 (with-directory-excursion (string-append out "/bin")
                   (symlink "bash" "sh")
                   #t))))

           (add-after 'install 'move-development-files
             (lambda* (#:key outputs #:allow-other-keys)
               ;; Move 'Makefile.inc' and 'bash.pc' to "include" to avoid
               ;; circular references among the outputs.
               (let ((out     (assoc-ref outputs "out"))
                     (include (assoc-ref outputs "include"))
                     (lib     (cut string-append <> "/lib/bash")))
                 (mkdir-p (lib include))
                 (rename-file (string-append (lib out)
                                             "/Makefile.inc")
                              (string-append (lib include)
                                             "/Makefile.inc"))
                 (rename-file (string-append out "/lib/pkgconfig")
                              (string-append include
                                             "/lib/pkgconfig"))

                 ;; Don't capture the absolute file name of 'install' to avoid
                 ;; retaining a dependency on Coreutils.
                 (substitute* (string-append (lib include)
                                             "/Makefile.inc")
                   (("^INSTALL =.*")
                    "INSTALL = install -c\n"))
                 #t)))
           ,@(if (target-loongarch64?)
                 `((add-after 'unpack 'update-config-scripts
                     (lambda* (#:key inputs native-inputs #:allow-other-keys)
                       ;; Replace outdated config.guess and config.sub.
                       (for-each (lambda (file)
                                   (install-file
                                    (search-input-file
                                     (or native-inputs inputs)
                                     (string-append "/bin/" file)) "./support"))
                                 '("config.guess" "config.sub")))))
                 '()))))

      (native-search-paths
       (list (search-path-specification ;new in 4.4
              (variable "BASH_LOADABLES_PATH")
              (files '("lib/bash")))))

      (synopsis "The GNU Bourne-Again SHell")
      (description
       "Bash is the shell, or command-line interpreter, of the GNU system.  It
is compatible with the Bourne Shell, but it also integrates useful features
from the Korn Shell and the C Shell and new improvements of its own.  It
allows command-line editing, unlimited command history, shell functions and
aliases, and job control while still allowing most sh scripts to be run
without modification.")
      (license license:gpl3+)
      (home-page "https://www.gnu.org/software/bash/"))))

(define-public bash-minimal
  ;; A stripped-down Bash for non-interactive use.
  (package/inherit bash
    (name "bash-minimal")
    (inputs '())                                ; no readline, no curses

    ;; No "include" output because there's no support for loadable modules.
    (outputs (delete "include" (package-outputs bash)))

    (arguments
     (substitute-keyword-arguments (package-arguments bash)
       ((#:modules _ '())
        '((guix build gnu-build-system)
          (guix build utils)
          (srfi srfi-1)
          (srfi srfi-26)))
       ((#:configure-flags flags '())
        `(list "--without-bash-malloc"
               "--disable-readline"
               "--disable-history"
               "--disable-help-builtin"
               "--disable-progcomp"
               "--disable-net-redirections"
               "--disable-nls"

               ;; Pretend 'dlopen' is missing so we don't build loadable
               ;; modules and related code.
               "ac_cv_func_dlopen=no"

               ,@(if (or (target-hurd64?) (%current-target-system))
                     ;; gcc-14 implictly uses -Wimplicit-function-declaration
                     ;; which together with -Werror causes:
                     ;; ./enable.def:492:11: error: implicit declaration of function ‘dlclose’;
                     '("CFLAGS=-g -O2 -Wno-implicit-function-declaration")
                     '())

               ,@(if (%current-target-system)
                     '("bash_cv_job_control_missing=no"
                       "bash_cv_getcwd_malloc=yes")
                     '())))
       ((#:phases phases)
        `(modify-phases ,phases
           ;; No loadable modules.
           (delete 'move-development-files)))))
    (synopsis "The GNU Bourne-Again SHell, stripped down for non-interactive use")))

(define-public static-bash
  ;; Statically-linked Bash that contains nothing but the 'bash' binary and
  ;; 'sh' symlink, without any reference.
  (let ((bash (static-package bash-minimal)))
    (package/inherit bash
      (name "bash-static")
      (arguments
       (substitute-keyword-arguments
           `(#:allowed-references ("out") ,@(package-arguments bash))
         ((#:phases phases)
          #~(modify-phases #$phases
              (add-after 'strip 'remove-everything-but-the-binary
                (lambda* (#:key outputs #:allow-other-keys)
                  (let* ((out (assoc-ref outputs "out"))
                         (bin (string-append out "/bin")))
                    (remove-store-references (string-append bin "/bash"))
                    (delete-file (string-append bin "/bashbug"))
                    (delete-file-recursively (string-append out "/share"))
                    #t))))))))))

(define-public bash-with-syslog
  (package/inherit bash
    (name "bash-with-syslog")
    (arguments
     (substitute-keyword-arguments (package-arguments bash)
       ((#:phases phases '%standard-phases)
        `(modify-phases ,phases
           (add-after 'unpack 'enable-syslogging
             (lambda _
               (substitute* "config-top.h"
                 (("/\\* #define SYSLOG_HISTORY \\*/")
                  "#define SYSLOG_HISTORY"))))))))
    (description
     "Bash is the shell, or command-line interpreter, of the GNU system.  This
variant logs the history to syslog.")))

(define-public bash-completion
  (package
    (name "bash-completion")
    (version "2.11")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/scop/" name "/releases/download/"
                    version "/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "1b0iz7da1sgifx1a5wdyx1kxbzys53v0kyk8nhxfipllmm5qka3k"))
              (patches
               (search-patches "bash-completion-directories.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f))    ; Unclear how to make tests pass.
    (synopsis "Bash completions for common commands")
    (description
     "This package provides extensions that allow Bash to provide adapted
completion for many common commands.")
    (home-page "https://github.com/scop/bash-completion")
    (license license:gpl2+)))

(define-public bash-tap
  (package
    (name "bash-tap")
    (version "1.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/illusori/bash-tap")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "13zz9h6bhhnk3hiwhlpafrnf2isws249h3fz785dcgymk02arz9c"))))
    ;; There is no compilation process to use this package, however, the bash
    ;; scripts installed by this package start with "#!/bin/bash".  To fix
    ;; these lines, we use the patch-shebangs of the GNU build system.  The
    ;; project does not use a Makefile.
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; There is no test suite.
       #:phases
       (modify-phases %standard-phases
         ;; Because there are no configure scripts or Makefile, we can
         ;; remove these phases.
         (delete 'configure)
         (delete 'build)
         ;; The installation involves manually copying the files to a location.
         ;; To make them easily accessible by setting PATH, we add the scripts
         ;; to the "bin" folder.
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((bin (string-append (assoc-ref outputs "out") "/bin")))
               (install-file "bash-tap" bin)
               (install-file "bash-tap-bootstrap" bin)
               (install-file "bash-tap-mock" bin)))))))
    (home-page "https://www.illusori.co.uk/projects/bash-tap/")
    (synopsis "Bash port of a Test::More/Test::Builder-style TAP-compliant
test library")
    (description "Bash TAP is a TAP-compliant Test::More-style testing library
for Bash shell scripts and functions.  Along with the Test::More-style testing
helpers it provides helper functions for mocking commands and in-process output
capturing.")
    (license license:expat)))

(define-public bats
  (package
    (name "bats")
    (version "1.11.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/bats-core/bats-core")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1dmgxcqq87vs1l23hb7ghx319w2nvn0w8z2kdxggs2b8n22wi0c2"))))
    (inputs
     (list bash coreutils guile-3.0 ;for wrap-script
           grep))
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (copy-recursively (assoc-ref %build-inputs "source") ".")
         (setenv "PATH"
                 (string-append (assoc-ref %build-inputs "bash") "/bin"
                                ":" (assoc-ref %build-inputs "coreutils") "/bin"
                                ":" (assoc-ref %build-inputs "grep") "/bin"
                                ":" (assoc-ref %build-inputs "guile") "/bin"
                                ":" (getenv "PATH")))
         (for-each patch-shebang
                   (find-files "."
                               (lambda (file stat)
                                 ;; Filter out symlinks.
                                 (eq? 'regular (stat:type stat)))
                               #:stat lstat))
         (substitute* "bin/bats"
           (("export BATS_ROOT" line)
            (string-append "BATS_ROOT=\"${BATS_ROOT:-" %output "/libexec/bats-core}\"\n"
                           line)))
         ;; Install phase
         (invoke "./install.sh" %output)
         (wrap-script (string-append %output "/bin/bats")
                      #:guile (search-input-file %build-inputs "bin/guile")
                      (list "PATH" 'prefix (string-split (getenv "PATH")
                                                         #\:))))))
    (build-system trivial-build-system)
    (home-page "https://github.com/bats-core/bats-core/")
    (synopsis "Bash Automated Testing System")
    (description
     "Bats is a @acronym{TAP, Test Anything Protocol}-compliant testing
framework for Bash.  It provides a simple way to verify that the UNIX programs
you write behave as expected.  Bats is most useful when testing software written
in Bash, but you can use it to test any UNIX program.")
    (license license:expat)))

(define-public bash-ctypes
  (package
    (name "bash-ctypes")
    (version "1.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/taviso/ctypes.sh/releases/download/v"
                           version "/ctypes-sh-" version ".tar.gz"))
       (sha256
        (base32 "0s1sifqzqmr0dnciv06yqrpzgj11d7n0gy5zaxh6b3x8bx7k75l8"))))
    (build-system gnu-build-system)
    (inputs
     (list elfutils
           libelf
           libffi
           zlib
           ;; Require a bash with C plugin support to build.
           bash))
    (native-inputs
     (list pkg-config))
    (home-page "https://github.com/taviso/ctypes.sh")
    (synopsis "Foreign function interface for Bash")
    (description "Bash-ctypes is a Bash plugin that provides a foreign
function interface (FFI) directly in your shell.  In other words, it allows
you to call routines in shared libraries from within Bash.")
    (license license:expat)))

(define-public blesh
  (package
    (name "blesh")
    (version "0.4.0-devel3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/akinomyoga/ble.sh")
                    (commit (string-append "v" version))
                    (recursive? #t)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "19y9rmj9srl7akx33gl34l5qgz2ww0vlmi4j2r11029p8sn4s418"))))
    (arguments
     (list #:make-flags #~(list (string-append "PREFIX="
                                               #$output))
           #:phases #~(modify-phases %standard-phases
                        (add-after 'unpack 'pretend-.git-exists
                          (lambda _
                            (substitute* "ble.pp"
                              (("#%\\[commit_hash =.*")
                               (string-append "#%[commit_hash = " #$version "]\n")))
                            (mkdir-p ".git")))
                        (delete 'configure) ;no configure
                        (add-before 'check 'use-LANG-for-tests
                          (lambda _
                            (setenv "LANG"
                                    (getenv "LC_ALL"))
                            (unsetenv "LC_ALL"))))))
    (build-system gnu-build-system)
    (native-inputs (list git less))
    (home-page "https://github.com/akinomyoga/ble.sh")
    (synopsis "Bash Line Editor")
    (description
     "Bash Line Editor (ble.sh) is a command line editor written in pure Bash
which replaces the default GNU Readline.  It adds syntax highlighting, auto
suggestions, vim modes, and more to Bash interactive sessions.")
    (license license:bsd-3)))

(define-public bash-unit
  (package
    (name "bash-unit")
    (version "2.3.2")
    (home-page "https://github.com/pgrange/bash_unit")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1gb6vfr5m84bn1gbv7w3jr06fspylj1i3yqkvgxq0n3bncvs35wz"))))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan '(("bash_unit" "bin/"))
       #:phases (modify-phases %standard-phases
                  (add-before 'install 'check
                    (lambda* (#:key tests? #:allow-other-keys)
                      (when tests?
                        (invoke "./bash_unit" "tests/test_core.sh")))))))
    (inputs (list bash-minimal))
    (synopsis "Bash unit testing framework")
    (description
     "@command{bash_unit} allows you to write unit tests, run them and, in case
of failure, display the stack trace with source file and line number indications
to locate the problem.")
    (license license:gpl3+)))
