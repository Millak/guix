;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2012-2018, 2020-2022 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015 Mathieu Lirzin <mthl@openmailbox.org>
;;; Copyright © 2014 Manolis Fragkiskos Ragkousis <manolis837@gmail.com>
;;; Copyright © 2015, 2017, 2018 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016 David Thompson <davet@gnu.org>
;;; Copyright © 2017 Nikita <nikita@n0.is>
;;; Copyright © 2017, 2019, 2021, 2022 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2019 Pierre-Moana Levesque <pierre.moana.levesque@gmail.com>
;;; Copyright © 2020, 2023, 2024, 2025 Janneke Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2022 Marius Bakke <marius@gnu.org>
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

(define-module (gnu packages autotools)
  #:use-module (guix licenses)
  #:use-module (gnu packages)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages man)
  #:use-module (gnu packages bash)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (ice-9 match)
  #:export (autoconf-wrapper))

(define-public autoconf-2.69
  (package
    (name "autoconf")
    (version "2.69")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnu/autoconf/autoconf-"
                           version ".tar.xz"))
       (sha256
        (base32
         "113nlmidxy9kjr45kg9x3ngar4951mvag1js2a3j8nxcz34wxsv4"))))
    (build-system gnu-build-system)
    (inputs
     (list bash-minimal m4 perl))
    (native-inputs
     (list perl m4))
    (arguments
     (list
      ;; XXX: testsuite: 209 and 279 failed.  The latter is an impurity.  It
      ;; should use our own "cpp" instead of "/lib/cpp".
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          #$@(if (%current-target-system)
                 #~((add-after 'install 'patch-non-shebang-references
                      (lambda* (#:key build inputs #:allow-other-keys)
                        ;; `patch-shebangs' patches shebangs only, and the Perl
                        ;; scripts use a re-exec feature that references the
                        ;; build hosts' perl.  Also, BASH and M4 store references
                        ;; hide in the scripts.
                        (let ((bash (dirname (dirname
                                              (search-input-file inputs "bin/bash"))))
                              (m4 (dirname (dirname
                                            (search-input-file inputs "bin/m4"))))
                              (perl (dirname (dirname
                                              (search-input-file inputs "bin/perl"))))
                              (store-directory (%store-directory)))
                          (substitute* (find-files (string-append #$output "/bin"))
                            (((string-append store-directory "/[^/]*-bash-[^/]*"))
                             bash)
                            (((string-append store-directory "/[^/]*-m4-[^/]*"))
                             m4)
                            (((string-append store-directory "/[^/]*-perl-[^/]*"))
                             perl))))))
                 '())
          (add-after 'install 'unpatch-shebangs
            (lambda _
              ;; Scripts that "autoconf -i" installs (config.guess,
              ;; config.sub, and install-sh) must use a regular shebang
              ;; rather than a reference to the store.  Restore it.
              (let ((build-aux (string-append #$output
                                              "/share/autoconf/build-aux")))
                (substitute* (find-files build-aux)
                  (("^#!.*/bin/sh") "#!/bin/sh"))))))))
    (home-page "https://www.gnu.org/software/autoconf/")
    (synopsis "Create source code configuration scripts")
    (description
     "Autoconf offers the developer a robust set of M4 macros which expand
into shell code to test the features of Unix-like systems and to adapt
automatically their software package to these systems.  The resulting shell
scripts are self-contained and portable, freeing the user from needing to
know anything about Autoconf or M4.")
    (license gpl3+))) ; some files are under GPLv2+

;; This is the renaissance version, which is not widely supported yet.
(define-public autoconf-2.71
  (package
    (inherit autoconf-2.69)
    (version "2.71")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnu/autoconf/autoconf-"
                           version ".tar.xz"))
       (sha256
        (base32
         "197sl23irn6s9pd54rxj5vcp5y8dv65jb9yfqgr2g56cxg7q6k7i"))))
    (arguments
     (substitute-keyword-arguments (package-arguments autoconf-2.69)
       ((#:tests? _ #f)
        ;; FIXME: To run the test suite, fix all the instances where scripts
        ;; generates "#! /bin/sh" shebangs.
        #f)
       ((#:phases phases #~%standard-phases)
        #~(modify-phases #$phases
            (add-before 'check 'prepare-tests
              (lambda _
                (for-each patch-shebang
                          (append (find-files "tests"
                                              (lambda (file stat)
                                                (executable-file? file)))
                                  (find-files "bin"
                                              (lambda (file stat)
                                                (executable-file? file)))))))))))))

(define-public autoconf-2.72
  (package (inherit autoconf-2.71)
    (version "2.72")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/autoconf/autoconf-"
                          version ".tar.xz"))
      (sha256
       (base32
        "0niz4852fgyavfh3gr4h4kzalk01nk70v6vfsja6r3ap349mr25s"))))))

(define-public autoconf autoconf-2.69)

(define-public autoconf-2.68
  (package (inherit autoconf)
    (version "2.68")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/autoconf/autoconf-"
                          version ".tar.xz"))
      (sha256
       (base32
        "1fjm21k2na07f3vasf288a0zx66lbv0hd3l9bvv3q8p62s3pg569"))))))

(define-public autoconf-2.64
  (package (inherit autoconf)
    (version "2.64")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/autoconf/autoconf-"
                          version ".tar.xz"))
      (sha256
       (base32
        "0j3jdjpf5ly39dlp0bg70h72nzqr059k0x8iqxvaxf106chpgn9j"))))))

(define-public autoconf-2.13
  ;; GNU IceCat 52.x requires autoconf-2.13 to build!
  (package (inherit autoconf)
    (version "2.13")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/autoconf/autoconf-"
                          version ".tar.gz"))
      (sha256
       (base32
        "07krzl4czczdsgzrrw9fiqx35xcf32naf751khg821g5pqv12qgh"))))
    (arguments
     `(#:tests? #f
       #:phases
       ;; The 'configure' script in autoconf-2.13 can't cope with "SHELL=" and
       ;; "CONFIG_SHELL=" arguments, so we set them as environment variables
       ;; and pass a simplified set of arguments.
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key build inputs outputs #:allow-other-keys)
             (let ((bash (which "bash"))
                   (out  (assoc-ref outputs "out")))
               (setenv "CONFIG_SHELL" bash)
               (setenv "SHELL" bash)
               (invoke bash "./configure"
                       (string-append "--prefix=" out)
                       (string-append "--build=" build))))))))))


(define (make-autoconf-wrapper autoconf)
  "Return a wrapper around AUTOCONF that generates `configure' scripts that
use our own Bash instead of /bin/sh in shebangs.  For that reason, it should
only be used internally---users should not end up distributing `configure'
files with a system-specific shebang."
  (package
    (inherit autoconf)
    (name (string-append (package-name autoconf) "-wrapper"))
    (build-system trivial-build-system)
    (inputs
     (list
      ;; XXX: Kludge to hide the circular dependency.
      (module-ref (resolve-interface '(gnu packages guile))
                  'guile-3.0/pinned)
      autoconf
      bash-minimal))
    (arguments
     (list
      #:modules '((guix build utils))
      #:builder
      #~(begin
          (use-modules (guix build utils))
          (let ((bin      (string-append #$output "/bin"))
                (autoconf (search-input-file %build-inputs "/bin/autoconf"))
                (guile    (search-input-file %build-inputs "/bin/guile"))
                (sh       (search-input-file %build-inputs "/bin/sh"))
                (modules  ((compose dirname dirname dirname)
                           (search-path %load-path
                                        "guix/build/utils.scm"))))
            (mkdir-p bin)

            ;; Symlink all the binaries but `autoconf'.
            (with-directory-excursion bin
              (for-each (lambda (file)
                          (unless (string=? (basename file) "autoconf")
                            (symlink file (basename file))))
                        (find-files (dirname autoconf) ".*")))

            ;; Add an `autoconf' binary that wraps the real one.
            (call-with-output-file (string-append bin "/autoconf")
              (lambda (port)
                ;; Shamefully, Guile can be used in shebangs only if a
                ;; single argument is passed (-ds); otherwise it gets
                ;; them all as a single argument and fails to parse them.
                (format port "#!~a
export GUILE_LOAD_PATH=\"~a\"
export GUILE_LOAD_COMPILED_PATH=\"~a\"
exec ~a --no-auto-compile \"$0\" \"$@\"
!#~%"
                        sh modules modules guile)
                (write
                 `(begin
                    (use-modules (guix build utils))
                    (let ((result (apply system* ,autoconf
                                         (cdr (command-line)))))
                      (when (and (file-exists? "configure")
                                 (not (file-exists? "/bin/sh")))
                        ;; Patch regardless of RESULT, because `autoconf
                        ;; -Werror' can both create a `configure' file and
                        ;; return a non-zero exit code.
                        (patch-shebang "configure"))
                      (exit (status:exit-val result))))
                 port)))
            (chmod (string-append bin "/autoconf") #o555)))))

    ;; Do not show it in the UI since it's meant for internal use.
    (properties '((hidden? . #t)))))

;; Only use this package when autoconf is not usable,
;; see <https://issues.guix.gnu.org/46564#1>.
(define-public autoconf-wrapper
  (make-autoconf-wrapper autoconf))

(define-public autoconf-archive
  (package
    (name "autoconf-archive")
    (version "2023.02.20")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/autoconf-archive/autoconf-archive-"
                          version ".tar.xz"))
      (sha256
       (base32
        "0cqsqdnmjdyybzw8wqhwkgs1vh7r5pbw66a6g7sz2a5fg6209m3i"))))
    (build-system gnu-build-system)
    (home-page "https://www.gnu.org/software/autoconf-archive/")
    (synopsis "Collection of freely reusable Autoconf macros")
    (description
     "Autoconf Archive is a collection of over 450 new macros for Autoconf,
greatly expanding the domain of its functionality.  These macros have been
contributed as free software by the community.")
    (license gpl3+)))

(define-public autobuild
  (package
    (name "autobuild")
    (version "5.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://savannah/autobuild/autobuild-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0gv7g61ja9q9zg1m30k4snqwwy1kq7b4df6sb7d2qra7kbdq8af1"))))
    (build-system gnu-build-system)
    (inputs (list perl))
    (synopsis "Process generated build logs")
    (description "Autobuild is a package that processes build logs generated
when building software.  Autobuild is primarily focused on packages using
Autoconf and Automake, but can be used with other build systems too.
Autobuild generates an HTML summary file, containing links to each build log.
The summary includes project name, version, build hostname, host type (cross
compile aware), date of build, and indication of success or failure.  The
output is indexed in many ways to simplify browsing.")
    (home-page "https://josefsson.org/autobuild/")
    (license gpl3+)))

(define-public automake-1.16.5
  (package
    (name "automake")
    (version "1.16.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/automake/automake-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "0sdl32qxdy7m06iggmkkvf7j520rmmgbsjzbm7fgnxwxdp6mh7gh"))
              (patches
               (search-patches "automake-skip-amhello-tests.patch"))))
    (build-system gnu-build-system)
    (inputs
     (list autoconf-wrapper bash-minimal perl))
    (native-inputs
     (list autoconf-wrapper perl))
    (native-search-paths
     (list (search-path-specification
            (variable "ACLOCAL_PATH")
            (files '("share/aclocal")))))
    (arguments
     (list
      #:tests? #f ;with gcc-14, 23 compiler "tap" tests fail.
      #:modules '((guix build gnu-build-system)
                  (guix build utils)
                  (srfi srfi-1)
                  (srfi srfi-26)
                  (rnrs io ports))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'patch-source-shebangs 'patch-tests-shebangs
            (lambda* (#:key native-inputs inputs #:allow-other-keys)
              (let ((sh (search-input-file (or native-inputs inputs) "bin/sh")))
                (substitute* (find-files "t" "\\.(sh|tap)$")
                  (("#![[:blank:]]?/bin/sh")
                   (string-append "#!" sh)))

                ;; Set these variables for all the `configure' runs
                ;; that occur during the test suite.
                (setenv "SHELL" sh)
                (setenv "CONFIG_SHELL" sh))))

          #$@(if (%current-target-system)
                 #~((add-after 'install 'patch-non-shebang-references
                      (lambda* (#:key inputs #:allow-other-keys)
                        ;; `patch-shebangs' patches shebangs only, and the Perl
                        ;; scripts use a re-exec feature that references the
                        ;; build hosts' perl.  Also, AUTOCONF and BASH store
                        ;; references hide in the scripts.
                        (let ((autoconf
                               (dirname (dirname
                                         (search-input-file inputs "bin/autoconf"))))
                              (bash
                               (dirname (dirname
                                         (search-input-file inputs "bin/bash"))))
                              (perl
                               (dirname (dirname
                                         (search-input-file inputs "bin/perl"))))
                              (store-directory (%store-directory)))
                          (substitute* (find-files (string-append #$output "/bin"))
                            (((string-append store-directory "/[^/]*-autoconf-[^/]*"))
                             autoconf)
                            (((string-append store-directory "/[^/]*-bash-[^/]*"))
                             bash)
                            (((string-append store-directory "/[^/]*-perl-[^/]*"))
                             perl))))))
                 '())

          ;; Files like `install-sh', `mdate.sh', etc. must use
          ;; #!/bin/sh, otherwise users could leak erroneous shebangs
          ;; in the wild.  See <http://bugs.gnu.org/14201> for an
          ;; example.
          (add-after 'install 'unpatch-shebangs
            (lambda _
              (let ((dir (string-append #$output "/share")))
                (define (starts-with-shebang? file)
                  (equal? (call-with-input-file file
                            (lambda (p)
                              (list (get-u8 p) (get-u8 p))))
                          (map char->integer '(#\# #\!))))

                (for-each (lambda (file)
                            (when (and (starts-with-shebang? file)
                                       (executable-file? file))
                              (format #t "restoring shebang on `~a'~%"
                                      file)
                              (substitute* file
                                (("^#!.*/bin/sh")
                                 "#!/bin/sh")
                                (("^#!.*/bin/env(.*)$" _ args)
                                 (string-append "#!/usr/bin/env"
                                                args)))))
                          (find-files dir ".*"))))))))
    (home-page "https://www.gnu.org/software/automake/")
    (synopsis "Making GNU standards-compliant Makefiles")
    (description
     "Automake the part of the GNU build system for producing
standards-compliant Makefiles.  Build requirements are entered in an
intuitive format and then Automake works with Autoconf to produce a robust
Makefile, simplifying the entire process for the developer.")
    (license gpl2+)))                      ; some files are under GPLv3+

(define-public automake
  (package/inherit automake-1.16.5
    (name "automake")
    (version "1.17")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/automake/automake-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "146rkdcwri2dkwn3pjrjs9v0wm4xyav9vvq4yw5vj4qy87yc2849"))
              (patches
               (search-patches "automake-skip-amhello-tests.patch"))))
    (arguments
     (substitute-keyword-arguments (package-arguments automake-1.16.5)
       ((#:tests? tests?)
        #t)
       ((#:phases phases)
        #~(modify-phases #$phases
            (add-before 'check 'skip-test
              (lambda _
                (substitute*
                    ;; This test requires 'etags' and fails if it's missing.
                    '("t/tags-lisp-space.sh"
                      ;; This test fails, probably a timestamp thing:
                      ;; make: Nothing to be done for 'all'.
                      "t/remake-aclocal-version-mismatch.sh")
                  (("^#!.*" all)
                   (string-append all "exit 77;\n")))))))))))

(define-public libtool
  (package
    (name "libtool")
    (version "2.4.7")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/libtool/libtool-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "0pb3l4x37k6fj1lwnpzws55gi3pxl0hx56jm4bzmbrkw0mzj2zsg"))
              (patches (search-patches "libtool-skip-tests2.patch"
                                       "libtool-grep-compat.patch"))))
    (build-system gnu-build-system)
    (propagated-inputs (list m4))
    (native-inputs
     (list m4
           perl
           ;; XXX: this shouldn't be necessary, but without it test
           ;; 102 fails because it cannot find ltdl/libltdl.la.
           libltdl
           help2man             ; because we modify ltmain.sh
           automake             ; some tests rely on 'aclocal'
           autoconf-wrapper))   ; others on 'autom4te'

    (arguments
     `(;; Libltdl is provided as a separate package, so don't install it here.
       #:configure-flags '("--disable-ltdl-install")

       ;; XXX: There are test failures on mips64el-linux starting from 2.4.4:
       ;; <http://hydra.gnu.org/build/181662>.
       ;; Also, do not run tests when cross compiling
       #:tests? ,(not (or (%current-target-system)
                          (string-prefix? "mips64"
                                          (%current-system))))

       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'pre-check
           (lambda* (#:key inputs native-inputs parallel-tests? #:allow-other-keys)
             ;; Run the test suite in parallel, if possible.
             (setenv "TESTSUITEFLAGS"
                     (string-append
                      "-j"
                      (if parallel-tests?
                        (number->string (parallel-job-count))
                        "1")))
           ;; Patch references to /bin/sh.
           (let ((/bin/sh (search-input-file (or native-inputs inputs) "bin/sh")))
             (substitute* "tests/testsuite"
               (("/bin/sh") /bin/sh)))))
         ;; These files may be copied into source trees by libtoolize,
         ;; therefore they must not point to store file names that would be
         ;; leaked with tarballs generated by make dist.
         (add-after 'install 'restore-build-aux-shebang
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (dir (string-append out "/share/libtool/build-aux")))
               (for-each (lambda (file)
                           (format #t "restoring shebang on `~a'~%" file)
                           (substitute* file
                             (("^#!.*/bin/sh") "#!/bin/sh")))
                         (find-files dir))))))))

    (synopsis "Generic shared library support tools")
    (description
     "GNU Libtool helps in the creation and use of shared libraries, by
presenting a single consistent, portable interface that hides the usual
complexity of working with shared libraries across platforms.")
    (license gpl3+)
    (home-page "https://www.gnu.org/software/libtool/")))

(define-public config
  (let ((revision "1")
        (commit "c8ddc8472f8efcadafc1ef53ca1d863415fddd5f"))
    (package
      (name "config")
      (version (git-version "0.0.0" revision commit)) ;no release tag
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://git.savannah.gnu.org/git/config.git/")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0x6ycvkmmhhhag97wsf0pw8n5fvh12rjvrck90rz17my4ys16qwv"))))
      (build-system gnu-build-system)
      (arguments
       `(#:phases (modify-phases %standard-phases
                    (add-after 'unpack 'patch-/bin/sh
                      (lambda _
                        (substitute* "testsuite/config-guess.sh"
                          (("#!/bin/sh")
                           (string-append "#!" (which "sh"))))
                        #t))
                    (replace 'build
                      (lambda _
                        (invoke "make" "manpages")))
                    (delete 'configure)
                    (replace 'install
                      (lambda* (#:key outputs #:allow-other-keys)
                        (let* ((out (assoc-ref outputs "out"))
                               (bin (string-append out "/bin"))
                               (man1 (string-append out "/share/man/man1")))
                          (install-file "config.guess" bin)
                          (install-file "config.sub" bin)
                          (install-file "doc/config.guess.1" man1)
                          (install-file "doc/config.sub.1" man1)
                          #t))))))
      (native-inputs
       (list help2man))
      (home-page "https://savannah.gnu.org/projects/config")
      (synopsis "Ubiquitous config.guess and config.sub scripts")
      (description "The `config.guess' script tries to guess a canonical system triple,
and `config.sub' validates and canonicalizes.  These are used as part of
configuration in nearly all GNU packages (and many others).")
      (license gpl2+))))

(define-public libltdl
  ;; This is a libltdl package separate from the libtool package.  This is
  ;; useful because, unlike libtool, it has zero extra dependencies (making it
  ;; readily usable during bootstrap), and it builds very quickly since
  ;; Libtool's extensive test suite isn't run.
  (package
    (name "libltdl")
    (version "2.4.7")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/libtool/libtool-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "0pb3l4x37k6fj1lwnpzws55gi3pxl0hx56jm4bzmbrkw0mzj2zsg"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--enable-ltdl-install") ;really install it
       #:phases (modify-phases %standard-phases
                  (add-before 'configure 'change-directory
                    (lambda _ (chdir "libltdl"))))))

    (synopsis "System-independent dlopen wrapper of GNU libtool")
    (description (package-description libtool))
    (home-page (package-home-page libtool))
    (license lgpl2.1+)))

(define-public pyconfigure
  (package
    (name "pyconfigure")
    (version "0.2.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/pyconfigure/pyconfigure-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0kxi9bg7l6ric39vbz9ykz4a21xlihhh2zcc3297db8amvhqwhrp"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'patch-python
           (lambda _
             (substitute* "pyconf.in"
               (("/usr/bin/env python") (which "python3")))
             #t)))))
    (inputs
     (list python-3))
    (synopsis "@command{configure} interface for Python-based packages")
    (description
     "GNU pyconfigure provides template files for easily implementing
standards-compliant configure scripts and Makefiles for Python-based packages.
It is designed to work alongside existing Python setup scripts, making it easy
to integrate into existing projects.  Powerful and flexible Autoconf macros
are available, allowing you to easily make adjustments to the installation
procedure based on the capabilities of the target computer.")
    (home-page "https://www.gnu.org/software/pyconfigure/manual/")
    (license
     (fsf-free
      "https://www.gnu.org/prep/maintain/html_node/License-Notices-for-Other-Files.html"))))
