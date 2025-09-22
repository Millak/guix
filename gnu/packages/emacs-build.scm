;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025 Nicolas Graves <ngraves@ngraves.fr>
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

(define-module (gnu packages emacs-build)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system emacs)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages version-control))

(define-public emacs-ansi
  (let ((commit "2367fba7b3b2340364a30cd6de7f3eb6bb9898a3")
        (revision "2"))
    (package
      (name "emacs-ansi")
      (version (git-version "0.4.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/rejeep/ansi.el")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1n7h6l4icm6lks3zpvd83j1fzrnspw19rmz7c96vy7pdh1y4v3p3"))))
      (build-system emacs-build-system)
      ;; Tests for emacs-ansi have a circular dependency with ert-runner, and
      ;; therefore cannot be run
      (arguments (list #:tests? #f))
      (home-page "https://github.com/rejeep/ansi.el")
      (synopsis "Convert strings to ANSI")
      (description "@code{emacs-ansi} defines functions that turns simple
strings to ANSI strings.  Turning a string into an ANSI string can be to add
color to a text, add color in the background of a text or adding a style, such
as bold, underscore or italic.")
      (license license:gpl3+))))

(define-public emacs-buttercup
  (package
    (name "emacs-buttercup")
    (version "1.37")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jorgenschaefer/emacs-buttercup")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0gkw1lmy8ynralrs4xwqsd06ww09xk5yqjdgw4r5c0zhp5dxn4ky"))))
    (build-system emacs-build-system)
    (arguments
     (list
      #:test-command #~(list "make" "test")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'install-bin
            (lambda _
              (install-file "bin/buttercup"
                            (string-append #$output "/bin")))))))
    (home-page "https://github.com/jorgenschaefer/emacs-buttercup")
    (synopsis "Behavior driven emacs lisp testing framework")
    (description "Buttercup is a behavior-driven development framework for
testing Emacs Lisp code.  It groups related tests so they can share
common set-up and tear-down code, and allows the programmer to \"spy\" on
functions to ensure they are called with the right arguments during testing.")
    (license license:gpl3+)))

(define-public emacs-compat
  (package
    (name "emacs-compat")
    (version "30.0.2.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/emacs-compat/compat")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "020rn3l2sn0vzfhx30k47jd2cgvsddk0zfbizgb68hbajcxqfsl4"))))
    (build-system emacs-build-system)
    (home-page "https://elpa.gnu.org/packages/compat.html")
    (synopsis "Emacs Lisp compatibility library")
    (description
     "To allow for the usage of Emacs functions and macros that are defined
in newer versions of Emacs, @code{compat.el} provides definitions that
are installed ONLY if necessary.  These reimplementations of functions
and macros are at least subsets of the actual implementations.  Be
sure to read the documentation string to make sure.

Not every function provided in newer versions of Emacs is provided
here.  Some depend on new features from the core, others cannot be
implemented to a meaningful degree.  The main audience for this
library are not regular users, but package maintainers.  Therefore
commands and user options are usually not implemented here.")
    (license license:gpl3+)))

(define-public emacs-commander
  (package
    (name "emacs-commander")
    (version "0.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rejeep/commander.el")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1j6hhyzww7wfwk6bllbb5mk4hw4qs8hsgfbfdifsam9c6i4spm45"))))
    (build-system emacs-build-system)
    ;; Tests for emacs-commander have a circular dependency with ert-runner, and
    ;; therefore cannot be run
    (arguments (list #:tests? #f))
    (propagated-inputs
     (list emacs-dash emacs-f emacs-s))
    (home-page "https://github.com/rejeep/commander.el")
    (synopsis "Emacs command line parser")
    (description "@code{emacs-commander} provides command line parsing for
Emacs.")
    (license license:gpl3+)))

(define-public emacs-dash
  (package
    (name "emacs-dash")
    (version "2.20.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/magnars/dash.el")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "092kf61bi6dwl42yng69g3y55ni8afycqbpaqx9wzf8frx9myg6m"))))
    (build-system emacs-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'disable-byte-compile-error-on-warn
                 (lambda _
                   (substitute* "Makefile"
                     (("\\(setq byte-compile-error-on-warn t\\)")
                      "(setq byte-compile-error-on-warn nil)")))))))
    (home-page "https://github.com/magnars/dash.el")
    (synopsis "Modern list library for Emacs")
    (description "This package provides a modern list API library for Emacs.")
    (license license:gpl3+)))

(define-public emacs-ecukes
  (package
    (name "emacs-ecukes")
    (version "0.6.18")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ecukes/ecukes")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "182qgddfv8nd89y1l55rs5vm5i61ayc8cxbplb8zx0alnid9xrw1"))))
    (build-system emacs-build-system)
    (arguments
     `(#:include (cons* "^feature/" "^reporters/" "^templates/" %default-include)
       ;; 4 unexpected results:
       ;;   FAILED  ecukes-run-test/run-step-async-callbacked-no-arg
       ;;   FAILED  ecukes-run-test/run-step-async-callbacked-with-arg
       ;;   FAILED  ecukes-run-test/run-step-async-callbacked-with-arg-and-args
       ;;   FAILED  ecukes-run-test/run-step-async-with-timeout
       #:tests? #f))
    (propagated-inputs
     (list emacs-ansi
           emacs-commander
           emacs-dash
           emacs-espuds
           emacs-f
           emacs-s))
    (home-page "https://github.com/ecukes/ecukes")
    (synopsis "Cucumber for Emacs")
    (description
     "This package provides Ecukes, a Cucumber-inspired integration testing
tool for Emacs.  Ecukes is not a complete clone of Cucumber and is not
intended to be.")
    (license license:gpl3+)))

(define-public emacs-eldev
  (package
    (name "emacs-eldev")
    (version "1.11.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/emacs-eldev/eldev")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0sf8xyzblc0fs2d65jgcycavnzmrp1wg0sfr29gjkq1kvzyl7phb"))))
    (build-system emacs-build-system)
    (arguments
     (list
      #:test-command #~(list "./bin/eldev" "-p" "-dtTC" "test")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'prepare-for-tests
            (lambda _
              (setenv "ELDEV_LOCAL" (getcwd))
              (make-file-writable "test/project-i/project-i-autoloads.el")))
          (add-after 'unpack 'skip-failing-tests
            ;; FIXME: 2 tests are failing.  Skip them for now.
            (lambda _
              (delete-file "test/upgrade-self.el")))
          (add-after 'install 'install-eldev-executable
            ;; This constructs the eldev executable from templates and
            ;; installs it in the specified directory.
            (lambda _
              (let ((bin (string-append #$output "/bin"))
                    (site-lisp (elpa-directory #$output)))
                (mkdir-p bin)
                (setenv "HOME" (getcwd))
                (invoke "./install.sh" bin)
                (substitute* (string-append bin "/eldev")
                  ;; Point ELDEV_LOCAL to the installation directory so that
                  ;; eldev doesn't try to bootstrap itself from MELPA when
                  ;; invoked.
                  (("export ELDEV_EMACS.*" all)
                   (string-append "export ELDEV_LOCAL=" site-lisp "\n" all)))))))))
    (native-inputs
     (list texinfo))                    ;for tests
    (home-page "https://github.com/emacs-eldev/eldev/")
    (synopsis "Emacs-based build tool for Elisp")
    (description "Eldev (Elisp Development Tool) is an Emacs-based build tool,
targeted solely at Elisp projects.  It is an alternative to Cask.  Unlike
Cask, Eldev itself is fully written in Elisp and its configuration files are
also Elisp programs.  For those familiar with the Java world, Cask can be seen
as a parallel to Maven — it uses project description, while Eldev is sort of a
parallel to Gradle — its configuration is a program on its own.")
    (license license:gpl3+)))

(define-public emacs-el-mock
  (let ((commit "6cfbc9de8f1927295dca6864907fe4156bd71910")
        (revision "1"))
    (package
      (name "emacs-el-mock")
      (version (git-version "1.25.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/rejeep/el-mock.el")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "09c3a1771v6kliwj0bn953pxxyjlk6q9kp31cxcr0nraik7d0mhk"))))
      (build-system emacs-build-system)
      (native-inputs (list emacs-ert-runner
                           emacs-ert-expectations
                           emacs-undercover))
      (home-page "https://github.com/rejeep/el-mock.el")
      (synopsis "Tiny mock and stub framework in Emacs Lisp")
      (description
       "Emacs Lisp Mock is a library for mocking and stubbing using readable
syntax.  Most commonly Emacs Lisp Mock is used in conjunction with Emacs Lisp
Expectations, but it can be used in other contexts.")
      (license license:gpl3+))))

(define-public emacs-ert-async
  (package
    (name "emacs-ert-async")
    (version "0.1.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rejeep/ert-async.el")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0hn9i405nfhjd1h9vnwj43nxbbz00khrwkjq0acfyxjaz1shfac9"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/rejeep/ert-async.el")
    (synopsis "Async support for ERT")
    (description "This package allows ERT to work with asynchronous tests.")
    (license license:gpl3+)))

(define-public emacs-ert-expectations
  (package
    (name "emacs-ert-expectations")
    (version "0.2")
    (source
     (origin
       (method url-fetch)
       (uri "https://www.emacswiki.org/emacs/download/ert-expectations.el")
       (sha256
        (base32
         "0cwy3ilsid90abzzjb7ha2blq9kmv3gfp3icwwfcz6qczgirq6g7"))))
    (build-system emacs-build-system)
    (home-page "https://www.emacswiki.org/emacs/ert-expectations.el")
    (synopsis "Simple unit test framework for Emacs Lisp")
    (description "@code{emacs-ert-expectations} is a simple unit test
framework for Emacs Lisp to be used with @code{ert}.")
    (license license:gpl3+)))

(define-public emacs-ert-runner
  (package
    (name "emacs-ert-runner")
    (version "0.8.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rejeep/ert-runner.el")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "08gygn9fjank5gpi4v6ynrkn0jbknxbwsn7md4p9ndygdbmnkf98"))))
    (build-system emacs-build-system)
    (inputs
     (list bash-minimal
           emacs-ansi
           emacs-commander
           emacs-dash
           emacs-f
           emacs-s
           emacs-shut-up))
    ;; Tests for ert-runner have a circular dependency with ecukes, and therefore
    ;; cannot be run
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-executable
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (source-directory (string-append
                                      (getenv "TMPDIR") "/source")))
               (substitute* "bin/ert-runner"
                 (("ERT_RUNNER=\"\\$\\(dirname \\$\\(dirname \\$0\\)\\)")
                  (string-append "ERT_RUNNER=\"" (elpa-directory out))))
               (install-file "bin/ert-runner" (string-append out "/bin"))
               (wrap-program (string-append out "/bin/ert-runner")
                 (list "EMACSLOADPATH" ":" 'prefix
                       ;; Do not capture the transient source directory in
                       ;; the wrapper.
                       (delete source-directory
                               (string-split (getenv "EMACSLOADPATH")
                                             #\:))))))))
       #:include (cons* "^reporters/.*\\.el$" %default-include)))
    (home-page "https://github.com/rejeep/ert-runner.el")
    (synopsis "Opinionated Ert testing workflow")
    (description "@code{ert-runner} is a tool for Emacs projects tested
using ERT.  It assumes a certain test structure setup and can therefore make
running tests easier.")
    (license license:gpl3+)))

(define-public emacs-espuds
  (package
    (name "emacs-espuds")
    (version "0.3.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ecukes/espuds")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "16r4j27j9yfdiy841w9q5ykkc6n3wrm7hvfacagb32mydk821ijg"))))
    (build-system emacs-build-system)
    (arguments (list #:tests? #f))      ; XXX: test defined twice
    (propagated-inputs
     (list emacs-s emacs-dash emacs-f))
    (home-page "https://github.com/ecukes/espuds")
    (synopsis "Common step definitions for Ecukes")
    (description "Espuds is a collection of the most commonly used step
definitions for testing with the Ecukes framework.")
    (license license:gpl3+)))

(define-public emacs-f
  (package
    (name "emacs-f")
    (version "0.21.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/rejeep/f.el")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0ccrcfhqfbv9qff38sfym69mai7k7z89yndi6nip8wi5hpd2addc"))))
    (build-system emacs-build-system)
    (arguments (list #:tests? #f))      ; circular dependency on ert-runner
    (propagated-inputs
     (list emacs-s emacs-dash))
    (home-page "https://github.com/rejeep/f.el")
    (synopsis "Emacs API for working with files and directories")
    (description "This package provides an Emacs library for working with
files and directories.")
    (license license:gpl3+)))

(define-public emacs-package-lint
  (package
    (name "emacs-package-lint")
    (version "0.26")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/purcell/package-lint")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0x1ikwfz1zr8bl8a67kgcbyzrqxnn6bslbz6pdfaczmshy7ry757"))))
    (arguments
     (list #:include #~(cons "^data/" %default-include)
           #:tests? #f                  ; XXX: 8/92 tests failing
           #:test-command #~(list "make" "test" "INIT_PACKAGES=t")))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-compat))
    (home-page "https://github.com/purcell/package-lint")
    (synopsis "Linting library for Elisp package authors")
    (description
     "This provides a list of issues with the Emacs package metadata of a file,
e.g., the package dependencies it requires.  Checks will currently be enabled
only if a @samp{Package-Requires:} or @samp{Package-Version:} header is
present in the file.")
    (license license:gpl3+)))

(define-public emacs-s
  (package
    (name "emacs-s")
    (version "1.13.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/magnars/s.el")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "010i92kagqbfis46n1ffa28fgkdkjp55n13b6f4izar5r7ixm6wx"))))
    (build-system emacs-build-system)
    (arguments
     `(#:test-command '("./run-tests.sh")))
    (home-page "https://github.com/magnars/s.el")
    (synopsis "Emacs string manipulation library")
    (description "This package provides an Emacs library for manipulating
strings.")
    (license license:gpl3+)))

(define-public emacs-shut-up
  (package
    (name "emacs-shut-up")
    (version "0.3.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cask/shut-up")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1bnmrwrhra6cpc3jjgwwzrydj5ps7q2dlkh2ag4j7rkyv4dlk351"))))
    (build-system emacs-build-system)
    (arguments (list #:tests? #f))      ; circular dependency on ert-runner
    (home-page "https://github.com/cask/shut-up")
    (synopsis "Silence Emacs")
    (description "This package silences most output of Emacs when running an
Emacs shell script.")
    (license license:expat)))

(define-public emacs-undercover
  (package
    (name "emacs-undercover")
    (version "0.8.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/sviridov/undercover.el")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0qmvyy3xg5qi7ws8zcs934d6afsappr1a6pgfp796xpa9vdr4y6j"))))
    (build-system emacs-build-system)
    (propagated-inputs
     (list emacs-dash emacs-shut-up))
    (home-page "https://github.com/sviridov/undercover.el")
    (synopsis "Test coverage library for Emacs Lisp")
    (description
     "Undercover is a test coverage library for software written in Emacs
Lisp.")
    (license license:expat)))

(define-public makel
  (package
    (name "makel")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/DamienCassou/makel")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "143bdy4c81jbmp5sk1arnlmpc4dsw85n601x9rii2dgyn186l8si"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:modules
      #~((guix build copy-build-system)
         (guix build utils)
         (srfi srfi-1)
         (srfi srfi-26)
         (ice-9 ftw))
      #:install-plan
      #~'(("makel.mk" "include/")
          ("README.org" "share/docs/"))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'install 'inject-deps
            (lambda _
              (substitute* "makel.mk"
                (("^MAKEL_LOAD_PATH=(.*)$" all rest)
                 (string-append
                  "MAKEL_LOAD_PATH=-L "
                  (string-join
                   (filter-map
                    (lambda (dir)
                      (let ((path (scandir dir)))
                        (and (eq? (length path) 3) ; removes emacs
                             (string-append dir "/" (last path)))))
                    (string-split (getenv "EMACSLOADPATH") #\:))
                   " -L ")
                  " " rest)))))
          (add-before 'install 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (if tests?
                  (with-directory-excursion "test"
                    (invoke "bash" "run-tests.sh"))
                  (format #f "Test suite not run.~%")))))))
    (native-inputs
     (list bash-minimal emacs-minimal))
    (inputs
     (list emacs-buttercup emacs-package-lint))
    (home-page "https://github.com/DamienCassou/makel")
    (synopsis "Makefile to help checking Emacs packages")
    (description
     "This package provides a Makefile to help checking Emacs packages.")
    (license license:gpl3+)))

;; This is an alternative version patches for internal Guix tests.
;; The user-facing version is in emacs-xyz.scm
(define-public makem-minimal
  (package
    (name "makem")
    (version "0.7.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/alphapapa/makem.sh")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1xqwfxkxgpcm3k2m2shwkwrm4asl59ik8867c3k1iwfj8xzfp089"))
       (modules '((guix build utils)))
       (snippet #~(begin
                    (delete-file-recursively "images")))))
    (build-system emacs-build-system)
    (arguments
     (let ((patch (local-file
                   (car (search-patches "makem-replace-git.patch")))))
       (list
        #:tests? #f
        #:modules
        '((guix build emacs-build-system)
          (guix build utils)
          (guix build emacs-utils)
          (srfi srfi-26))
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'patch-file
              (lambda* (#:key inputs #:allow-other-keys)
                (invoke (search-input-file inputs "bin/patch")
                        "--force" "--no-backup-if-mismatch"
                        "-p1" "--input" #$patch)))
            (add-after 'install 'install-bin
              (lambda _
                (let ((bin (string-append #$output "/bin")))
                  (mkdir-p bin)
                  (install-file "makem.sh" bin))))
            (add-after 'install-bin 'wrap-script
              (lambda* (#:key inputs outputs #:allow-other-keys)
                (wrap-program (string-append #$output "/bin/makem.sh")
                  `("PATH" ":" prefix
                    ,(map dirname
                          (map (cut search-input-file inputs <>)
                               (list "bin/find"
                                     "bin/grep"
                                     "bin/sed"
                                     ;; for util-linux
                                     "bin/uuidgen")))))))))))
    (native-inputs (list patch))
    (inputs
     (list coreutils-minimal grep sed util-linux))  ; for getopt
    (home-page "https://github.com/alphapapa/makem.sh")
    (synopsis "Makefile-like script to help checking Emacs packages")
    (description "This package provides a Makefile-like script and a transient
menu for linting and testing Emacs packages.")
    (license license:gpl3+)))
