;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Cyril Roelandt <tipecaml@gmail.com>
;;; Copyright © 2014, 2015 David Thompson <davet@gnu.org>
;;; Copyright © 2014 Kevin Lemonnier <lemonnierk@ulrar.net>
;;; Copyright © 2015 Jeff Mickey <j@codemac.net>
;;; Copyright © 2016–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2016 Stefan Reichör <stefan@xsteve.at>
;;; Copyright © 2017, 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017, 2018 Nikita <nikita@n0.is>
;;; Copyright © 2017, 2018 Leo Famulari <leo@famulari.name>
;;; Copyright © 2017, 2021 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2019 Meiyo Peng <meiyo.peng@gmail.com>
;;; Copyright © 2019 Timothy Sample <samplet@ngyro.com>
;;; Copyright © 2019 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2019, 2020, 2023 Janneke Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2020 Brice Waegeneire <brice@waegenei.re>
;;; Copyright © 2020 Ryan Prior <rprior@protonmail.com>
;;; Copyright © 2020, 2022, 2024 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2020, 2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2021, 2022 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2021, 2022 Felix Gruber <felgru@posteo.net>
;;; Copyright © 2022 Andrew Tropin <andrew@trop.in>
;;; Copyright © 2023 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2023 David Pflug <david@pflug.io>
;;; Copyright © 2023 Jaeme Sifat <jaeme@runbox.com>
;;; Copyright © 2024 Tanguy Le Carrour <tanguy@bioneland.org>
;;; Copyright © 2024 Vinicius Monego <monego@posteo.net>
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

(define-module (gnu packages shells)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages c)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crates-apple)
  #:use-module (gnu packages crates-crypto)
  #:use-module (gnu packages crates-graphics)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages crates-tls)
  #:use-module (gnu packages crates-web)
  #:use-module (gnu packages crates-windows)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages groff)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages libbsd)
  #:use-module (gnu packages libedit)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages scheme)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages texinfo)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (guix build-system trivial)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public dash
  (package
    (name "dash")
    (version "0.5.12")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://gondor.apana.org.au/~herbert/dash/files/"
                           "dash-" version ".tar.gz"))
       (sha256
        (base32 "12pjm2j0q0q88nvqbcyqjwr8s1c29ilxyq2cdj8k42wbdv24liva"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; The man page hails from BSD, where (d)ash is the default shell.
           ;; This isn't the case on Guix or indeed most other GNU systems.
           (substitute* "src/dash.1"
             (("the standard command interpreter for the system")
              "a command interpreter based on the original Bourne shell"))))))
    (build-system gnu-build-system)
    (inputs
     (list libedit))
    (arguments
     '(#:configure-flags '("--with-libedit")))
    (home-page "http://gondor.apana.org.au/~herbert/dash")
    (synopsis "POSIX-compliant shell optimised for size")
    (description
     "Dash is a POSIX-compliant @command{/bin/sh} implementation that aims to be
as small as possible, often without sacrificing speed.  It is faster than the
GNU Bourne-Again Shell (@command{bash}) at most scripted tasks.  Dash is a
direct descendant of NetBSD's Almquist Shell (@command{ash}).")
    (license (list license:bsd-3
                   license:gpl2+))))    ; mksignames.c

(define-public fish
  (package
    (name "fish")
    (version "3.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/fish-shell/fish-shell/"
                           "releases/download/" version "/"
                           "fish-" version ".tar.xz"))
       (sha256
        (base32 "1c9slg6azhc9jn1sb75wip4hl9zyibjy9nay505nkw0lnxw766yz"))))
    (build-system cmake-build-system)
    (inputs
     (list fish-foreign-env ncurses pcre2
           python))  ; for fish_config and manpage completions
    (native-inputs
     (list doxygen groff ; for 'fish --help'
           procps))             ; for the test suite
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'set-env
           (lambda _
             ;; some tests write to $HOME
             (setenv "HOME" (getcwd))
             #t))
         (add-after 'unpack 'patch-tests
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((coreutils (assoc-ref inputs "coreutils"))
                   (bash (assoc-ref inputs "bash")))
               ;; This test sporadically fails in the build container
               ;; because of leftover zombie processes, which are not
               ;; reaped automatically:
;; "Found existing zombie processes. Clean up zombies before running this test."
               ;; Disabling parallel tests does not reliably prevent it.
               (delete-file "tests/checks/jobs.fish")
               ;; This test fails.
               (delete-file "tests/checks/pipeline-pgroup.fish")
               ;; This one tries to open a terminal & can't simply be deleted.
               (substitute* "cmake/Tests.cmake"
                 ((".* interactive\\.fish.*") ""))
               ;; This one needs to chdir successfully.
               (substitute* "tests/checks/vars_as_commands.fish"
                 (("/usr/bin") "/tmp"))
               ;; These contain absolute path references.
               (substitute* "src/fish_tests.cpp"
                 (("/bin/echo" echo) (string-append coreutils echo))
                 (("/bin/ca" ca) (string-append coreutils ca))
                 (("\"(/bin/c)\"" _ c) (string-append "\"" coreutils c "\""))
                 (("/bin/ls_not_a_path" ls-not-a-path)
                  (string-append coreutils ls-not-a-path))
                 (("/bin/ls" ls) (string-append coreutils ls))
                 (("(/bin/)\"" _ bin) (string-append coreutils bin "\""))
                 (("/bin -" bin) (string-append coreutils bin))
                 (((string-append
                    "do_test\\(is_potential_path\\("
                    "L\"/usr\", wds, vars, PATH_REQUIRE_DIR\\)\\);"))
                  "")
                 ;; Not all mentions of /usr... need to exist, but these do.
                 (("\"/usr(|/lib)\"" _ subdirectory)
                  (string-append "\"/tmp" subdirectory "\"")))
               (substitute*
                 (append (find-files "tests" ".*\\.(in|out|err)$")
                         (find-files "tests/checks" ".*\\.fish"))
                 (("/bin/pwd" pwd) (string-append coreutils pwd))
                 (("/bin/echo" echo) (string-append coreutils echo))
                 (("/bin/sh" sh) (string-append bash sh))
                 (("/bin/ls" ls) (string-append coreutils ls)))
               (substitute* (find-files "tests" ".*\\.(in|out|err)$")
                 (("/usr/bin") (string-append coreutils "/bin")))
               #t)))
         ;; Source /etc/fish/config.fish from $__fish_sysconf_dir/config.fish.
         (add-after 'patch-tests 'patch-fish-config
           (lambda _
             (let ((port (open-file "etc/config.fish" "a")))
               (display (string-append
                         "\n\n"
                         "# Patched by Guix.\n"
                         "# Source /etc/fish/config.fish.\n"
                         "if test -f /etc/fish/config.fish\n"
                         "    source /etc/fish/config.fish\n"
                         "end\n")
                        port)
               (close-port port))
             #t))
         ;; Embed absolute paths.
         (add-before 'install 'embed-absolute-paths
           (lambda _
             (substitute* "share/functions/__fish_print_help.fish"
               (("nroff") (which "nroff")))
             #t))
         ;; Enable completions, functions and configurations in user's and
         ;; system's guix profiles by adding them to __extra_* variables.
         (add-before 'install 'patch-fish-extra-paths
           (lambda _
             (let ((port (open-file "share/__fish_build_paths.fish" "a")))
               (display
                (string-append
                 "\n\n"
                 "# Patched by Guix.\n"
                 "# Enable completions, functions and configurations in user's"
                 " and system's guix profiles by adding them to __extra_*"
                 " variables.\n"
                 "set -l __guix_profile_paths ~/.guix-profile"
                 " /run/current-system/profile\n"
                 "set __extra_completionsdir"
                 " $__guix_profile_paths\"/etc/fish/completions\""
                 " $__guix_profile_paths\"/share/fish/vendor_completions.d\""
                 " $__extra_completionsdir\n"
                 "set __extra_functionsdir"
                 " $__guix_profile_paths\"/etc/fish/functions\""
                 " $__guix_profile_paths\"/share/fish/vendor_functions.d\""
                 " $__extra_functionsdir\n"
                 "set __extra_confdir"
                 " $__guix_profile_paths\"/etc/fish/conf.d\""
                 " $__guix_profile_paths\"/share/fish/vendor_conf.d\""
                 " $__extra_confdir\n")
                port)
               (close-port port))
             #t))
         ;; Use fish-foreign-env to source /etc/profile.
         (add-before 'install 'source-etc-profile
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((port (open-file "share/__fish_build_paths.fish" "a")))
               (display
                (string-append
                 "\n\n"
                 "# Patched by Guix.\n"
                 "# Use fish-foreign-env to source /etc/profile.\n"
                 "if status is-login\n"
                 "    set fish_function_path "
                 (assoc-ref inputs "fish-foreign-env") "/share/fish/functions"
                 " $__fish_datadir/functions\n"
                 "    fenv source /etc/profile\n"
                 "    set -e fish_function_path\n"
                 "end\n")
                port)
               (close-port port))
             #t)))))
    (synopsis "The friendly interactive shell")
    (description
     "Fish (friendly interactive shell) is a shell focused on interactive use,
discoverability, and friendliness.  Fish has very user-friendly and powerful
tab-completion, including descriptions of every completion, completion of
strings with wildcards, and many completions for specific commands.  It also
has extensive and discoverable help.  A special @command{help} command gives
access to all the fish documentation in your web browser.  Other features
include smart terminal handling based on terminfo, an easy to search history,
and syntax highlighting.")
    (home-page "https://fishshell.com/")
    (license license:gpl2)))

(define-public fish-foreign-env
  (package
    (name "fish-foreign-env")
    (version "0.20230823")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/oh-my-fish/plugin-foreign-env")
             (commit "7f0cf099ae1e1e4ab38f46350ed6757d54471de7")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0d16mdgjdwln41zk44qa5vcilmlia4w15r8z2rc3p49i5ankksg3"))))
    (build-system trivial-build-system)
    (arguments
     '(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((source (assoc-ref %build-inputs "source"))
                (out (assoc-ref %outputs "out"))
                (func-path (string-append out "/share/fish/functions")))
           (mkdir-p func-path)
           (copy-recursively (string-append source "/functions")
                             func-path)

           ;; Embed absolute paths.
           (substitute* `(,(string-append func-path "/fenv.fish")
                          ,(string-append func-path "/fenv.main.fish"))
             (("bash")
              (search-input-file %build-inputs "/bin/bash"))
             (("sed")
              (search-input-file %build-inputs "/bin/sed"))
             ((" tr ")
              (string-append " "
                             (search-input-file %build-inputs "/bin/tr")
                             " ")))))))
    (inputs
     (list bash coreutils sed))
    (home-page "https://github.com/oh-my-fish/plugin-foreign-env")
    (synopsis "Foreign environment interface for fish shell")
    (description "@code{fish-foreign-env} wraps bash script execution in a way
that environment variables that are exported or modified get imported back
into fish.")
    (license license:expat)))

(define-public rc
  (package
    (name "rc")
    (version "1.7.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/rakitzis/rc")
                    (commit (string-append "v" version))))
              (sha256
               (base32
                "0vj1h4pcg13vxsiydmmk87dr2sra9h4gwx0c4q6fjsiw4in78rrd"))
              (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       '("--with-edit=gnu")
       #:phases
       (modify-phases %standard-phases
         (add-before 'bootstrap 'patch-trip.rc
          (lambda _
            (substitute* "trip.rc"
              (("/bin/pwd") (which "pwd"))
              (("/bin/sh")  (which "sh"))
              (("/bin/rm")  (which "rm"))
              (("/bin\\)")  (string-append (dirname (which "rm")) ")")))
            #t)))))
    (inputs (list readline perl))
    (native-inputs (list autoconf automake libtool pkg-config))
    (synopsis "Alternative implementation of the rc shell by Byron Rakitzis")
    (description
     "This is a reimplementation by Byron Rakitzis of the Plan 9 shell.  It
has a small feature set similar to a traditional Bourne shell.")
    (home-page "https://github.com/rakitzis/rc")
    (license license:zlib)))

(define-public es
  (package
    (name "es")
    (version "0.9.2")
    (source
     (origin
       (method url-fetch/tarbomb)
       (uri (string-append "https://github.com/wryun/es-shell/releases/"
                           "download/v" version "/es-" version ".tar.gz"))
       (sha256
        (base32 "1pgmqhsk14wyvl489sxdy7kdl2gwrsq1xvkip0z90kh888mlh9n9"))
       (file-name (string-append name "-" version ".tar.gz"))))
    (build-system gnu-build-system)
    (arguments
     (list #:test-target "test"))
    (inputs
     (list readline))
    (native-inputs
     (list bison))
    (synopsis "Extensible shell with higher-order functions")
    (description
     "Es is an extensible shell.  The language was derived from the Plan 9
shell, rc, and was influenced by functional programming languages, such as
Scheme, and the Tcl embeddable programming language.  This implementation is
derived from Byron Rakitzis's public domain implementation of rc, and was
written by Paul Haahr and Byron Rakitzis.")
    (home-page "https://wryun.github.io/es-shell/")
    (license license:public-domain)))

(define-public tcsh
  (package
    (name "tcsh")
    (version "6.24.01")
    (source (origin
              (method url-fetch)
              ;; Old tarballs are moved to old/.
              (uri (list (string-append "https://astron.com/pub/tcsh/"
                                        "tcsh-" version ".tar.gz")
                         (string-append "https://astron.com/pub/tcsh/"
                                        "old/tcsh-" version ".tar.gz")))
              (sha256
               (base32
                "0zhxp4m1fxyd3a2qyvs97gzlrb0h0ah1gjrqcbilgydiffws2nan"))
              (patches (search-patches "tcsh-fix-autotest.patch"))
              (patch-flags '("-p0"))))
    (build-system gnu-build-system)
    (native-inputs
     (append (if (target-riscv64?)
                 (list config)
                 '())
             (list autoconf perl)))
    (inputs
     (list libxcrypt ncurses))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          #$@(if (%current-target-system)
                 #~((add-before 'configure 'set-cross-cc
                      (lambda _
                        (substitute* "configure"
                          (("CC_FOR_GETHOST=\"cc\"")
                           "CC_FOR_GETHOST=\"gcc\"")))))
                 #~())
          #$@(if (system-hurd?)
                 #~((add-after 'unpack 'skip-tests
                      (lambda _
                        (substitute* "tests/testsuite.at"
                          (("m4_include\\(\\[subst.at\\]\\)" all)
                           (string-append "# " all))))))
                 #~())
          (add-before 'check 'patch-test-scripts
            (lambda _
              ;; Take care of pwd
              (substitute* '("tests/commands.at" "tests/variables.at")
                (("/bin/pwd") (which "pwd")))
              (substitute* "Makefile"
                ;; Likewise for /usr/bin/env.
                (("/usr/bin/env") "env")
                ;; Don't reset the environment (PATH, etc).
                (("\\$\\(ENVCMD\\) -") "$(ENVCMD)"))
              ;; This test does not expect the home directory from
              ;; /etc/passwd to be '/'.
              (substitute* "tests/subst.at"
                (("\\$\\(id -un\\)/foo")
                 "$(id -un)//foo"))
              ;; The .at files create shell scripts without shebangs. Erk.
              (substitute* "tests/commands.at"
                (("./output.sh") "/bin/sh output.sh"))
              (substitute* "tests/syntax.at"
                (("; other_script.csh") "; /bin/sh other_script.csh"))
              ;; Now, let's generate the test suite and patch it
              (invoke "make" "tests/testsuite")

              ;; This file is ISO-8859-1 encoded.
              (with-fluids ((%default-port-encoding #f))
                (substitute* "tests/testsuite"
                  (("/bin/sh") (which "sh"))))))
          (add-after 'install 'post-install
            (lambda _
              (with-directory-excursion (string-append #$output "/bin")
                (symlink "tcsh" "csh")))))))
    (home-page "https://www.tcsh.org/")
    (synopsis "Unix shell based on csh")
    (description
     "Tcsh is an enhanced, but completely compatible version of the Berkeley
UNIX C shell (csh).  It is a command language interpreter usable both as an
interactive login shell and a shell script command processor.  It includes a
command-line editor, programmable word completion, spelling correction, a
history mechanism, job control and a C-like syntax.")
    (license license:bsd-4)))

(define-public zsh
  (package
    (name "zsh")
    (version "5.9")
    (source (origin
              (method url-fetch)
              (uri (list (string-append
                           "https://www.zsh.org/pub/zsh-" version
                           ".tar.xz")
                         (string-append
                           "https://www.zsh.org/pub/old/zsh-" version
                           ".tar.xz")))
              (sha256
               (base32
                "1mdc8lnq8qxq1ahxp8610n799pd7a9kqg3liy7xq2pjvvp71x3cv"))
              (patches (search-patches "zsh-egrep-failing-test.patch"))))
    (build-system gnu-build-system)
    (arguments `(#:configure-flags
                 `("--with-tcsetpgrp"
                  "--enable-pcre"
                  "--enable-maildir-support"
                  ;; share/zsh/site-functions isn't populated
                  "--disable-site-fndir"
                  ,(string-append
                    "--enable-additional-fpath="
                    "/usr/local/share/zsh/site-functions," ; for foreign OS
                    "/run/current-system/profile/share/zsh/site-functions"))
                 #:phases
                 (modify-phases %standard-phases
                   (add-before 'configure 'fix-sh
                     (lambda _
                       ;; Some of the files are ISO-8859-1 encoded.
                       (with-fluids ((%default-port-encoding #f))
                                    (substitute*
                                        '("configure"
                                          "configure.ac"
                                          "Src/exec.c"
                                          "Src/mkmakemod.sh"
                                          "Config/installfns.sh"
                                          "Config/defs.mk.in"
                                          "Test/E01options.ztst"
                                          "Test/A05execution.ztst"
                                          "Test/A01grammar.ztst"
                                          "Test/A06assign.ztst"
                                          "Test/B02typeset.ztst"
                                          "Completion/Unix/Command/_init_d"
                                          "Util/preconfig")
                                      (("/bin/sh") (which "sh"))))))
                   (add-before 'check 'patch-test
                     (lambda _
                       ;; In Zsh, `command -p` searches a predefined set of
                       ;; paths that don't exist in the build environment. See
                       ;; the assignment of 'path' in Src/init.c'
                       (substitute* "Test/A01grammar.ztst"
                         (("command -pv") "command -v")
                         (("command -p") "command ")
                         (("'command' -p") "'command' "))))
                   (add-after 'build 'make-info
                     (lambda _ (invoke "make" "info")))
                   (add-after 'build 'install-info
                     (lambda _ (invoke "make" "install.info"))))))
    (native-inputs (list autoconf texinfo))
    (inputs (list ncurses pcre perl))
    (synopsis "Powerful shell for interactive use and scripting")
    (description "The Z shell (zsh) is a Unix shell that can be used
as an interactive login shell and as a powerful command interpreter
for shell scripting.  Zsh can be thought of as an extended Bourne shell
with a large number of improvements, including some features of bash,
ksh, and tcsh.")
    (home-page "https://www.zsh.org/")

    ;; The whole thing is under an MIT/X11-style license, but there's one
    ;; command, 'Completion/Unix/Command/_darcs', which is under GPLv2+.
    (license license:gpl2+)))

(define-public xonsh
  (package
    (name "xonsh")
    (version "0.18.2")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "xonsh" version))
        (sha256
          (base32
           "0fx5ywsckwdxk4l5dmaf12z1b4nfd4zqdx5gapc11hhl9waks1y5"))
        (modules '((guix build utils)))
        (snippet
         #~(begin
             (substitute* "pyproject.toml"
               (("\"xonsh\\.parsers\\.ply\",") ""))
             ;; Use our properly packaged PLY instead.
             (substitute* (list "setup.py"
                                "tests/parsers/test_lexer.py"
                                "xonsh/parsers/base.py"
                                "xonsh/parsers/completion_context.py"
                                "xonsh/parsers/lexer.py"
                                "xonsh/parsers/v310.py"
                                "xonsh/xonfig.py")
               (("from xonsh\\.parsers\\.ply") "from ply")
               (("from xonsh\\.parsers import ply") "import ply"))
             (delete-file-recursively "xonsh/parsers/ply")))))
    (build-system pyproject-build-system)
    (arguments
     ;; Some tests are failing for reasons like not accessing parent directory
     ;; with os.getcwd(), not activating virtual environments, not finding
     ;; some commands (man, echo), and not running subprocesses.
     (list #:test-flags
           #~(list "-k"
                   (string-append
                    "not "
                    (string-join
                     (list "test_aliases_print"
                           "test_argv0"
                           "test_bash_and_is_alias_is_only_functional_alias"
                           "test_bash_completer"
                           "test_bash_completer_empty_prefix"
                           "test_complete_command"
                           "test_complete_dots"
                           "test_dirty_working_directory"
                           "test_equal_sign_arg"
                           "test_man_completion"
                           "test_parser_show"
                           "test_printfile"
                           "test_printname"
                           "test_quote_handling"
                           "test_script"
                           "test_skipper_command"
                           "test_sourcefile"
                           "test_spec_decorator_alias_output_format"
                           "test_spec_modifier_alias_output_format"
                           "test_vc_get_branch"
                           "test_xonsh_activator"
                           "test_xonsh_lexer")
                     " and not ")))
           #:phases
           #~(modify-phases %standard-phases
               (replace 'install
                 (lambda _
                   (invoke "python" "-m" "compileall"
                           "--invalidation-mode=unchecked-hash" #$output)
                   (invoke "python" "setup.py" "install" "--root=/"
                           (string-append "--prefix=" #$output))))
               ;; Some tests run os.mkdir().
               (add-before 'check 'writable-home
                 (lambda _
                   (setenv "HOME" "/tmp"))))))
    (native-inputs
     (list git-minimal
           python-pyte
           python-pytest
           python-pytest-mock
           python-pytest-rerunfailures
           python-pytest-subprocess
           python-pytest-timeout
           python-requests
           python-setuptools                      ;needed at build time
           python-wheel))
    (inputs
     (list python-distro
           python-ply
           python-prompt-toolkit
           python-pygments
           python-pyperclip
           python-setproctitle))
    (home-page "https://xon.sh/")
    (synopsis "Python-ish shell")
    (description
     "Xonsh is a Python-ish, BASHwards-looking shell language and command
prompt.  The language is a superset of Python 3.4+ with additional shell
primitives that you are used to from Bash and IPython.  It works on all major
systems including Linux, Mac OSX, and Windows.  Xonsh is meant for the daily
use of experts and novices alike.")
    (license license:bsd-2)))

(define-public scsh
  (let ((commit "4acf6e4ed7b65b46186ef0c9c2a1e10bef8dc052")
        (revision "0"))
    (package
      (name "scsh")
      (version (git-version "0.7" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/scheme/scsh")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1czrp808v5gs0ci5lmkp3wr3gfkrb3vd5b2iw2hz1bpqgaf6bxpv"))
         (patches (search-patches "scsh-nonstring-search-path.patch"))))
      (build-system gnu-build-system)
      (arguments
       (list
        #:test-target "test"
        #:phases
        #~(modify-phases %standard-phases
            (add-before 'configure 'replace-rx
              (lambda _
                (let ((rxpath (string-append #$scheme48-rx
                                             "/share/scheme48-"
                                             #$(package-version scheme48)
                                             "/rx")))
                  (delete-file-recursively "rx")
                  (symlink rxpath "rx")))))))
      (inputs
       (list scheme48 scheme48-rx))
      (native-inputs
       (list autoconf automake))
      (native-search-paths
       (list (search-path-specification
               (variable "SCSH_LIB_DIRS")
               (separator " ")
               (files '("share/scsh-0.7")))))
      (home-page "https://github.com/scheme/scsh")
      (synopsis "Unix shell embedded in Scheme")
      (description
       "Scsh is a Unix shell embedded in Scheme.  Scsh has two main
components: a process notation for running programs and setting up pipelines
and redirections, and a complete syscall library for low-level access to the
operating system.")
      (license license:bsd-3))))

(define-public linenoise
  (let ((commit "93b2db9bd4968f76148dd62cdadf050ed50b84b3")
        (revision "2"))
    (package
      (name "linenoise")
      (version (string-append "1.0-" revision "." (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/antirez/linenoise")
               (commit commit)))
         (file-name (string-append name "-" version "-checkout"))
         (sha256
          (base32
           "102gwq6bzjq2b1lf55wrpgym58yfhry56hkajbj339m0bs1xijhs"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f                    ; no tests are included
         #:make-flags
         (list ,(string-append "CC=" (cc-for-target)))
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               ;; At the moment there is no 'make install' in upstream.
               (let* ((out (assoc-ref outputs "out")))
                 (install-file "linenoise.h"
                               (string-append out "/include/linenoise"))
                 (install-file "linenoise.c"
                               (string-append out "/include/linenoise"))
                 #t))))))
      (home-page "https://github.com/antirez/linenoise")
      (synopsis "Minimal zero-config readline replacement")
      (description
       "Linenoise is a minimal, zero-config, readline replacement.
Its features include:

@enumerate
@item Single and multi line editing mode with the usual key bindings
@item History handling
@item Completion
@item Hints (suggestions at the right of the prompt as you type)
@item A subset of VT100 escapes, ANSI.SYS compatible
@end enumerate\n")
      (license license:bsd-2))))

(define-public s-shell
  (let ((commit "da2e5c20c0c5f477ec3426dc2584889a789b1659")
        (revision "2"))
    (package
      (name "s-shell")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/rain-1/s")
               (commit commit)))
         (file-name (string-append name "-" version "-checkout"))
         (sha256
          (base32
           "0qiny71ww5nhzy4mnc8652hn0mlxyb67h333gbdxp4j4qxsi13q4"))))
      (build-system gnu-build-system)
      (inputs
       (list linenoise))
      (arguments
       `(#:tests? #f
         #:make-flags (list "CC=gcc"
                            (string-append "PREFIX="
                                           (assoc-ref %outputs "out")))
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'install-directory-fix
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (bin (string-append out "/bin")))
                 (substitute* "Makefile"
                   (("out") bin))
                 #t)))
           (add-after 'install 'manpage
             (lambda* (#:key outputs #:allow-other-keys)
               (install-file "s.1" (string-append (assoc-ref outputs "out")
                                                  "/share/man/man1"))))
           (replace 'configure
             (lambda* (#:key inputs outputs #:allow-other-keys)
               ;; At this point linenoise is meant to be included,
               ;; so we have to really copy it into the working directory
               ;; of s.
               (let* ((linenoise (assoc-ref inputs "linenoise"))
                      (noisepath (string-append linenoise "/include/linenoise"))
                      (out (assoc-ref outputs "out")))
                 (copy-recursively noisepath "linenoise")
                 (substitute* "s.c"
                   (("/bin/s") (string-append out "/bin/s")))
                 #t))))))
      (home-page "https://github.com/rain-1/s")
      (synopsis "Extremely minimal shell with the simplest syntax possible")
      (description
       "S is a new shell that aims to be extremely simple.  It does not
implement the POSIX shell standard.

There are no globs or \"splatting\" where a variable $FOO turns into multiple
command line arguments.  One token stays one token forever.
This is a \"no surprises\" straightforward approach.

There are no redirection operators > in the shell language, they are added as
extra programs.  > is just another unix command, < is essentially cat(1).
A @code{andglob} program is also provided along with s.")
      (license license:bsd-3))))

(define-public oksh
  (package
    (name "oksh")
    (version "0.5.9")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://connochaetos.org/oksh/oksh-"
                           version ".tar.gz"))
       (sha256
        (base32
         "0ln9yf6pxngsviqszv8klnnvn8vcpplvj1njdn8xr2y8frkbw8r3"))))
    (build-system gnu-build-system)
    (arguments
     `(; The test files are not part of the distributed tarball.
       #:tests? #f))
    (home-page "https://connochaetos.org/oksh")
    (synopsis "Port of OpenBSD Korn Shell")
    (description
     "Oksh is a port of the OpenBSD Korn Shell.
The OpenBSD Korn Shell is a cleaned up and enhanced ksh.")
    (license license:gpl3+)))

(define-public loksh
  (package
    (name "loksh")
    (version "7.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/dimkr/loksh")
                    (commit version)
                    ;; Include the ‘lolibc’ submodule, a static compatibility library
                    ;; created for and currently used only by loksh.
                    (recursive? #t)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0arbncmgs3wzkwlqzp5za8rwh9px2r5mn3i979rabc4cms1bs0l1"))))
    (build-system meson-build-system)
    (inputs (list ncurses))
    (native-inputs (list pkg-config))
    (arguments
     `(#:tests? #f)) ;no tests included
    (home-page "https://github.com/dimkr/loksh")
    (synopsis "Korn Shell from OpenBSD")
    (description
     "loksh is a Linux port of OpenBSD's @command{ksh}.  It is a small,
interactive POSIX shell targeted at resource-constrained systems.")
    ;; The file 'LEGAL' says it is the public domain, and the 2
    ;; exceptions which are listed are not included in this port.
    (license license:public-domain)))

(define-public mksh
  (package
    (name "mksh")
    (version "59c")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.mirbsd.org/MirOS/dist/mir/mksh/mksh-R"
                           version ".tgz"))
       (sha256
        (base32 "01n5ggw33bw4jv4d3148wlw9n4aj7vdn3ffnc66c9w9pldjidbkp"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; tests require access to /dev/tty
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'build
           (lambda _
             (setenv "CC" "gcc")
             (invoke (which "sh") "Build.sh")))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (man (string-append out "/share/man/man1")))
               (install-file "mksh" bin)
               (with-directory-excursion bin
                 (symlink "mksh" "ksh"))
               (install-file "mksh.1" man)))))))
    (home-page "https://www.mirbsd.org/mksh.htm")
    (synopsis "Korn Shell from MirBSD")
    (description "mksh is an actively developed free implementation of the
Korn Shell programming language and a successor to the Public Domain Korn
Shell (pdksh).")
    (license (list license:miros
                   license:isc))))              ; strlcpy.c

(define-public oil
  (package
    (name "oil")
    (version "0.20.0")
    (source
     ;; oil's sources contain a modified version of CPython 2.7.13.
     ;; According to https://www.oilshell.org/blog/2017/05/05.html
     ;; this bundles version of CPython had certain unused parts removed
     ;; and its build system has been replaced by a custom one.
     ;; This would probably make it quite complicated to replace the
     ;; bundled CPython with the one from the python2 package.
     (origin
       (method url-fetch)
       (uri (string-append "https://www.oilshell.org/download/oil-"
                           version ".tar.gz"))
       (sha256
        (base32 "1jpxhixwq29ik01jx372g9acib59wmww8lrdlcypq7jpg5b0b7pi"))))
    (build-system gnu-build-system)
    (arguments
     (list #:strip-binaries? #f         ; strip breaks the binary
           #:phases
           #~(modify-phases %standard-phases
               (replace 'configure
                 (lambda _
                   (setenv "CC" #$(cc-for-target))
                   (substitute* "configure"
                     ((" cc ") " $CC "))
                   (invoke "./configure" (string-append "--prefix=" #$output)
                           "--with-readline")))
               (replace 'check
                 ;; The tests are not distributed in the tarballs but upstream
                 ;; recommends running this smoke test.
                 ;; https://github.com/oilshell/oil/blob/release/0.8.0/INSTALL.txt#L38-L48
                 (lambda* (#:key tests? #:allow-other-keys)
                   (when tests?
                     (let* ((oil "_bin/oil.ovm"))
                       (invoke/quiet oil "osh" "-c" "echo hi")
                       (invoke/quiet oil "osh" "-n" "configure"))))))))
    (inputs
     (list readline))
    (home-page "https://www.oilshell.org")
    (synopsis "Programming language and Bash-compatible Unix shell")
    (description "Oil is a programming language with automatic translation for
Bash.  It includes osh, a Unix/POSIX shell that runs unmodified Bash
scripts.")
    (license (list license:psfl                 ; tarball includes python2.7
                   license:asl2.0))))

(define-public gash
  (package
    (name "gash")
    (version "0.3.0")
    (source
     (origin (method url-fetch)
             (uri (string-append "mirror://savannah/gash/gash-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "1af2jz4a6rzsshi379wzw4b8d04zvfamdhfzip2pgmk821lyqsjl"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list guile-3.0))
    (home-page "https://savannah.nongnu.org/projects/gash/")
    (synopsis "POSIX-compatible shell written in Guile Scheme")
    (description "Gash is a POSIX-compatible shell written in Guile
Scheme.  It provides both the shell interface, as well as a Guile
library for parsing shell scripts.  Gash is designed to bootstrap Bash
as part of the Guix bootstrap process.")
    (license license:gpl3+)))

(define-public gash-utils
  (package
    (name "gash-utils")
    (version "0.2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://savannah/gash/gash-utils-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "18ylb54l9lmaynapbncc1zhbsirhihznrxihhxgqrpqgyjkfbap6"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:phases #~(modify-phases %standard-phases
                  (add-after 'unpack 'skip-failing-tests
                    (lambda _
                      (substitute* "Makefile.am"
                        (("tests/sort\\.org") ""))
                      (for-each delete-file '("configure" "Makefile.in")))))))
    (native-inputs
     (list autoconf automake pkg-config))
    (inputs
     (list guile-3.0 gash))
    (home-page "https://savannah.nongnu.org/projects/gash/")
    (synopsis "Core POSIX utilities written in Guile Scheme")
    (description "Gash-Utils provides Scheme implementations of many
common POSIX utilities (there are about 40 of them, ranging in
complexity from @command{false} to @command{awk}).  The utilities are
designed to be capable of bootstrapping their standard GNU counterparts.
Underpinning these utilities are many Scheme interfaces for manipulating
files and text.")
    (license license:gpl3+)))

(define-public nushell
  (package
    (name "nushell")
    (version "0.91.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1zrw4jv57iwijd684vf43rw5sc9r0aq38shaizj96jqrgb8g9nja"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags
       '("--release" "--"
         ;; Not all files included
         "--skip=tests::test_config_path::test_alternate_config_path"
         "--skip=tests::test_config_path::test_default_config_path"
         "--skip=tests::test_config_path::test_default_config_path_symlinked_config_files"
         "--skip=tests::test_config_path::test_default_symlink_config_path_broken_symlink_config_files"
         "--skip=tests::test_config_path::test_default_symlinked_config_path_empty"
         ;; Path not available inside build environment
         "--skip=path::canonicalize::canonicalize_tilde"
         "--skip=path::canonicalize::canonicalize_tilde_relative_to"
         ;; could not get mutex lock: PoisonError { .. }
         "--skip=plugins::config::closure"
         "--skip=plugins::config::none"
         "--skip=plugins::config::record"
         "--skip=plugins::core_inc::by_one_with_field_passed"
         "--skip=plugins::core_inc::by_one_with_no_field_passed"
         "--skip=plugins::core_inc::chooses_highest_increment_if_given_more_than_one"
         "--skip=plugins::core_inc::explicit_flag"
         "--skip=plugins::core_inc::semversion_major_inc"
         "--skip=plugins::core_inc::semversion_minor_inc"
         "--skip=plugins::core_inc::semversion_patch_inc"
         "--skip=plugins::core_inc::semversion_without_passing_field"
         "--skip=plugins::custom_values::can_generate_and_updated_multiple_types_of_custom_values"
         "--skip=plugins::custom_values::can_get_custom_value_from_plugin_and_instantly_collapse_it"
         "--skip=plugins::custom_values::can_get_custom_value_from_plugin_and_pass_it_over"
         "--skip=plugins::custom_values::can_get_custom_value_from_plugin_and_pass_it_over_as_an_argument"
         "--skip=plugins::custom_values::can_get_describe_plugin_custom_values"
         "--skip=plugins::custom_values::fails_if_passing_custom_values_across_plugins"
         "--skip=plugins::custom_values::fails_if_passing_engine_custom_values_to_plugins"
         "--skip=plugins::formats::eml::from_eml_get_another_header_field"
         "--skip=plugins::formats::eml::from_eml_get_replyto_field"
         "--skip=plugins::formats::eml::from_eml_get_subject_field"
         "--skip=plugins::formats::eml::from_eml_get_to_field"
         "--skip=plugins::formats::ics::from_ics_text_to_table"
         "--skip=plugins::formats::ics::from_ics_text_with_linebreak_to_table"
         "--skip=plugins::formats::ics::infers_types"
         "--skip=plugins::formats::ini::parses_ini"
         "--skip=plugins::formats::ini::parses_utf16_ini"
         "--skip=plugins::formats::ini::read_ini_with_missing_session"
         "--skip=plugins::formats::vcf::from_vcf_text_to_table"
         "--skip=plugins::formats::vcf::from_vcf_text_with_linebreak_to_table"
         "--skip=plugins::formats::vcf::infers_types"
         "--skip=plugins::register::help"
         "--skip=plugins::register::search_terms"
         "--skip=plugins::stream::collect_external_accepts_list_of_binary"
         "--skip=plugins::stream::collect_external_accepts_list_of_string"
         "--skip=plugins::stream::collect_external_big_stream"
         "--skip=plugins::stream::collect_external_produces_raw_input"
         "--skip=plugins::stream::seq_big_stream"
         "--skip=plugins::stream::seq_describe_no_collect_succeeds_without_error"
         "--skip=plugins::stream::seq_produces_stream"
         "--skip=plugins::stream::seq_stream_collects_to_correct_list"
         "--skip=plugins::stream::sum_accepts_list_of_float"
         "--skip=plugins::stream::sum_accepts_list_of_int"
         "--skip=plugins::stream::sum_accepts_stream_of_float"
         "--skip=plugins::stream::sum_accepts_stream_of_int"
         "--skip=plugins::stream::sum_big_stream")
       #:features '("extra")
       #:install-source? #f
       #:cargo-inputs
       (("rust-crossterm" ,rust-crossterm-0.27)
        ("rust-ctrlc" ,rust-ctrlc-3)
        ("rust-log" ,rust-log-0.4)
        ("rust-miette" ,rust-miette-7)
        ("rust-mimalloc" ,rust-mimalloc-0.1)
        ("rust-nix" ,rust-nix-0.27)
        ("rust-nu-cli" ,rust-nu-cli-0.91)
        ("rust-nu-cmd-base" ,rust-nu-cmd-base-0.91)
        ("rust-nu-cmd-dataframe" ,rust-nu-cmd-dataframe-0.91)
        ("rust-nu-cmd-extra" ,rust-nu-cmd-extra-0.91)
        ("rust-nu-cmd-lang" ,rust-nu-cmd-lang-0.91)
        ("rust-nu-command" ,rust-nu-command-0.91)
        ("rust-nu-engine" ,rust-nu-engine-0.91)
        ("rust-nu-explore" ,rust-nu-explore-0.91)
        ("rust-nu-lsp" ,rust-nu-lsp-0.91)
        ("rust-nu-parser" ,rust-nu-parser-0.91)
        ("rust-nu-path" ,rust-nu-path-0.91)
        ("rust-nu-plugin" ,rust-nu-plugin-0.91)
        ("rust-nu-protocol" ,rust-nu-protocol-0.91)
        ("rust-nu-std" ,rust-nu-std-0.91)
        ("rust-nu-utils" ,rust-nu-utils-0.91)
        ("rust-openssl" ,rust-openssl-0.10)
        ("rust-reedline" ,rust-reedline-0.30)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-simplelog" ,rust-simplelog-0.12)
        ("rust-time" ,rust-time-0.3)
        ("rust-winresource" ,rust-winresource-0.1))
       #:cargo-development-inputs
       (("rust-assert-cmd" ,rust-assert-cmd-2)
        ("rust-divan" ,rust-divan-0.1)
        ("rust-nu-test-support" ,rust-nu-test-support-0.91)
        ("rust-pretty-assertions" ,rust-pretty-assertions-1)
        ("rust-rstest" ,rust-rstest-0.18)
        ("rust-serial-test" ,rust-serial-test-3)
        ("rust-tempfile" ,rust-tempfile-3))))
    (native-inputs (list pkg-config))
    (inputs (list mimalloc openssl))
    (home-page "https://www.nushell.sh")
    (synopsis "Shell with a structured approach to the command line")
    (description
     "Nu draws inspiration from projects like PowerShell, functional
programming languages, and modern CLI tools.  Rather than thinking of files
and services as raw streams of text, Nu looks at each input as something with
structure.  For example, when you list the contents of a directory, what you
get back is a table of rows, where each row represents an item in that
directory.  These values can be piped through a series of steps, in a series
of commands called a ``pipeline''.")
    (license license:expat)))

(define-public rust-nu-ansi-term-0.50
  (package
    (name "rust-nu-ansi-term")
    (version "0.50.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu-ansi-term" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "057zhc62hcaxc73zfjwq7gir5bfilmx652hnh9krdp0babhh0a6x"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-serde" ,rust-serde-1)
                       ("rust-windows-sys" ,rust-windows-sys-0.48))
       #:cargo-development-inputs (("rust-doc-comment" ,rust-doc-comment-0.3)
                                   ("rust-serde-json" ,rust-serde-json-1))))
    (home-page "https://github.com/nushell/nu-ansi-term")
    (synopsis "Library for ANSI terminal colors and styles (bold, underline)")
    (description
     "This package is a library for ANSI terminal colors and styles (bold,
underline).")
    (license license:expat)))

(define-public rust-nu-ansi-term-0.49
  (package
    (inherit rust-nu-ansi-term-0.50)
    (name "rust-nu-ansi-term")
    (version "0.49.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu-ansi-term" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0s2svfnircd9jp06wk55qcbb9v5cadkfcjfg99vm21qdjg0x6wy0"))))
    (arguments
     `(#:cargo-inputs
       (("rust-serde" ,rust-serde-1)
        ("rust-windows-sys" ,rust-windows-sys-0.48))
       #:cargo-development-inputs
       (("rust-doc-comment" ,rust-doc-comment-0.3)
        ("rust-serde-json" ,rust-serde-json-1))))))

(define-public rust-nu-ansi-term-0.46
  (package
    (inherit rust-nu-ansi-term-0.49)
    (name "rust-nu-ansi-term")
    (version "0.46.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "nu-ansi-term" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32 "115sywxh53p190lyw97alm14nc004qj5jm5lvdj608z84rbida3p"))))
    (arguments
     `(#:cargo-inputs
       (("rust-overload" ,rust-overload-0.1)
        ("rust-serde" ,rust-serde-1)
        ("rust-winapi" ,rust-winapi-0.3))
       #:cargo-development-inputs
       (("rust-doc-comment" ,rust-doc-comment-0.3)
        ("rust-regex" ,rust-regex-1)
        ("rust-serde-json" ,rust-serde-json-1))))))

(define-public rust-nu-ansi-term-0.44
  (package
    (inherit rust-nu-ansi-term-0.49)
    (name "rust-nu-ansi-term")
    (version "0.44.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu-ansi-term" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1lmc9rdqnw586gv4a0c2gbg3x4a04fy65xk3fczby8lq84rz41i3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-doc-comment" ,rust-doc-comment-0.3)
        ("rust-overload" ,rust-overload-0.1)
        ("rust-serde" ,rust-serde-1)
        ("rust-winapi" ,rust-winapi-0.3))))))

(define-public rust-nu-cli-0.91
  (package
    (name "rust-nu-cli")
    (version "0.91.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu-cli" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1v7xz13n5gzhwkn4d8wq1v22zbdvfqd2zxnqf2q3k8cnnj51ql63"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f      ; 42 of 45 of the completions tests fail.
       #:cargo-inputs (("rust-chrono" ,rust-chrono-0.4)
                       ("rust-crossterm" ,rust-crossterm-0.27)
                       ("rust-fancy-regex" ,rust-fancy-regex-0.13)
                       ("rust-fuzzy-matcher" ,rust-fuzzy-matcher-0.3)
                       ("rust-is-executable" ,rust-is-executable-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-lscolors" ,rust-lscolors-0.17)
                       ("rust-miette" ,rust-miette-7)
                       ("rust-nu-ansi-term" ,rust-nu-ansi-term-0.50)
                       ("rust-nu-cmd-base" ,rust-nu-cmd-base-0.91)
                       ("rust-nu-color-config" ,rust-nu-color-config-0.91)
                       ("rust-nu-engine" ,rust-nu-engine-0.91)
                       ("rust-nu-parser" ,rust-nu-parser-0.91)
                       ("rust-nu-path" ,rust-nu-path-0.91)
                       ("rust-nu-protocol" ,rust-nu-protocol-0.91)
                       ("rust-nu-utils" ,rust-nu-utils-0.91)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-pathdiff" ,rust-pathdiff-0.2)
                       ("rust-percent-encoding" ,rust-percent-encoding-2)
                       ("rust-reedline" ,rust-reedline-0.30)
                       ("rust-sysinfo" ,rust-sysinfo-0.30)
                       ("rust-unicode-segmentation" ,rust-unicode-segmentation-1)
                       ("rust-uuid" ,rust-uuid-1)
                       ("rust-which" ,rust-which-6))
       #:cargo-development-inputs
       (("rust-nu-cmd-lang" ,rust-nu-cmd-lang-0.91)
        ("rust-nu-command" ,rust-nu-command-0.91)
        ("rust-nu-test-support" ,rust-nu-test-support-0.91)
        ("rust-rstest" ,rust-rstest-0.18))))
    (native-inputs (list pkg-config))
    (inputs (list openssl))
    (home-page "https://github.com/nushell/nushell/tree/main/crates/nu-cli")
    (synopsis "CLI-related functionality for Nushell")
    (description "This package provides CLI-related functionality for Nushell.")
    (license license:expat)))

(define-public rust-nu-cmd-base-0.91
  (package
    (name "rust-nu-cmd-base")
    (version "0.91.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu-cmd-base" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0i2bdvhl1qmpzrip4b45xr4vg0himfsi120xq5al9vs5y80x2lla"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-indexmap" ,rust-indexmap-2)
                       ("rust-miette" ,rust-miette-7)
                       ("rust-nu-engine" ,rust-nu-engine-0.91)
                       ("rust-nu-parser" ,rust-nu-parser-0.91)
                       ("rust-nu-path" ,rust-nu-path-0.91)
                       ("rust-nu-protocol" ,rust-nu-protocol-0.91))))
    (home-page
     "https://github.com/nushell/nushell/tree/main/crates/nu-cmd-base")
    (synopsis "Foundation tools to build Nushell commands")
    (description
     "This package provides the foundation tools to build Nushell commands.")
    (license license:expat)))

(define-public rust-nu-cmd-dataframe-0.91
  (package
    (name "rust-nu-cmd-dataframe")
    (version "0.91.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu-cmd-dataframe" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1avsx50kr0snbm62l91v7a7wfq05igv5aagwhczm1g4xdpl448x9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-chrono" ,rust-chrono-0.4)
                       ("rust-chrono-tz" ,rust-chrono-tz-0.8)
                       ("rust-fancy-regex" ,rust-fancy-regex-0.13)
                       ("rust-indexmap" ,rust-indexmap-2)
                       ("rust-nu-engine" ,rust-nu-engine-0.91)
                       ("rust-nu-parser" ,rust-nu-parser-0.91)
                       ("rust-nu-protocol" ,rust-nu-protocol-0.91)
                       ("rust-num" ,rust-num-0.4)
                       ("rust-polars" ,rust-polars-0.37)
                       ("rust-polars-arrow" ,rust-polars-arrow-0.37)
                       ("rust-polars-io" ,rust-polars-io-0.37)
                       ("rust-polars-ops" ,rust-polars-ops-0.37)
                       ("rust-polars-plan" ,rust-polars-plan-0.37)
                       ("rust-polars-utils" ,rust-polars-utils-0.37)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-sqlparser" ,rust-sqlparser-0.43))
       #:cargo-development-inputs (("rust-nu-cmd-lang" ,rust-nu-cmd-lang-0.91))))
    (home-page
     "https://github.com/nushell/nushell/tree/main/crates/nu-cmd-dataframe")
    (synopsis "Nushell's dataframe commands based on polars")
    (description
     "This package contains nushell's dataframe commands based on polars.")
    (license license:expat)))

(define-public rust-nu-cmd-extra-0.91
  (package
    (name "rust-nu-cmd-extra")
    (version "0.91.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu-cmd-extra" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0x905m6yci5n3ir89arq7vdvx0czqpjvr3j8i32f7bqh0z3jisc3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-fancy-regex" ,rust-fancy-regex-0.13)
                       ("rust-heck" ,rust-heck-0.4)
                       ("rust-itertools" ,rust-itertools-0.12)
                       ("rust-nu-ansi-term" ,rust-nu-ansi-term-0.50)
                       ("rust-nu-cmd-base" ,rust-nu-cmd-base-0.91)
                       ("rust-nu-engine" ,rust-nu-engine-0.91)
                       ("rust-nu-json" ,rust-nu-json-0.91)
                       ("rust-nu-parser" ,rust-nu-parser-0.91)
                       ("rust-nu-pretty-hex" ,rust-nu-pretty-hex-0.91)
                       ("rust-nu-protocol" ,rust-nu-protocol-0.91)
                       ("rust-nu-utils" ,rust-nu-utils-0.91)
                       ("rust-num-traits" ,rust-num-traits-0.2)
                       ("rust-rust-embed" ,rust-rust-embed-8)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-urlencoded" ,rust-serde-urlencoded-0.7)
                       ("rust-v-htmlescape" ,rust-v-htmlescape-0.15))
       #:cargo-development-inputs
       (("rust-nu-cmd-lang" ,rust-nu-cmd-lang-0.91)
        ("rust-nu-command" ,rust-nu-command-0.91)
        ("rust-nu-test-support" ,rust-nu-test-support-0.91))))
    (native-inputs (list pkg-config))
    (inputs (list openssl))
    (home-page
     "https://github.com/nushell/nushell/tree/main/crates/nu-cmd-extra")
    (synopsis "Nushell's extra commands")
    (description "This package contains nushell's extra commands that are not
part of the 1.0 api standard.")
    (license license:expat)))

(define-public rust-nu-cmd-lang-0.91
  (package
    (name "rust-nu-cmd-lang")
    (version "0.91.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu-cmd-lang" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0zwdw3y4jn6s6h41jnwljpj9cfyhr68av61idikjkhi7l6hygy5c"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-itertools" ,rust-itertools-0.12)
                       ("rust-nu-engine" ,rust-nu-engine-0.91)
                       ("rust-nu-parser" ,rust-nu-parser-0.91)
                       ("rust-nu-protocol" ,rust-nu-protocol-0.91)
                       ("rust-nu-utils" ,rust-nu-utils-0.91)
                       ("rust-shadow-rs" ,rust-shadow-rs-0.26))))
    (home-page
     "https://github.com/nushell/nushell/tree/main/crates/nu-cmd-lang")
    (synopsis "Nushell's core language commands")
    (description "This package provides nushell's core language commands.")
    (license license:expat)))

(define-public rust-nu-color-config-0.91
  (package
    (name "rust-nu-color-config")
    (version "0.91.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu-color-config" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "115bha7r4sy19w80vbbfc3av9g0pa1fcksdaqznm0yjlykv49czy"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags
       '("--release" "--"
         "--skip=style_computer::test_computable_style_closure_basic"
         "--skip=style_computer::test_computable_style_closure_errors")
       #:cargo-inputs (("rust-nu-ansi-term" ,rust-nu-ansi-term-0.50)
                       ("rust-nu-engine" ,rust-nu-engine-0.91)
                       ("rust-nu-json" ,rust-nu-json-0.91)
                       ("rust-nu-protocol" ,rust-nu-protocol-0.91)
                       ("rust-serde" ,rust-serde-1))
       #:cargo-development-inputs
       (("rust-nu-test-support" ,rust-nu-test-support-0.91))))
    (home-page
     "https://github.com/nushell/nushell/tree/main/crates/nu-color-config")
    (synopsis "Color configuration code used by Nushell")
    (description "This package contains color configuration code used by Nushell.")
    (license license:expat)))

(define-public rust-nu-command-0.91
  (package
    (name "rust-nu-command")
    (version "0.91.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu-command" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0jiz6bndkwfnhs4cc74am8krnhyb5kyq310nf7ma5038q6vqs8q9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f      ; Not all files included.
       #:cargo-inputs (("rust-alphanumeric-sort" ,rust-alphanumeric-sort-1)
                       ("rust-base64" ,rust-base64-0.21)
                       ("rust-bracoxide" ,rust-bracoxide-0.1)
                       ("rust-byteorder" ,rust-byteorder-1)
                       ("rust-bytesize" ,rust-bytesize-1)
                       ("rust-calamine" ,rust-calamine-0.24)
                       ("rust-chardetng" ,rust-chardetng-0.1)
                       ("rust-chrono" ,rust-chrono-0.4)
                       ("rust-chrono-humanize" ,rust-chrono-humanize-0.2)
                       ("rust-chrono-tz" ,rust-chrono-tz-0.8)
                       ("rust-crossterm" ,rust-crossterm-0.27)
                       ("rust-csv" ,rust-csv-1)
                       ("rust-dialoguer" ,rust-dialoguer-0.11)
                       ("rust-digest" ,rust-digest-0.10)
                       ("rust-dtparse" ,rust-dtparse-2)
                       ("rust-encoding-rs" ,rust-encoding-rs-0.8)
                       ("rust-fancy-regex" ,rust-fancy-regex-0.13)
                       ("rust-filesize" ,rust-filesize-0.2)
                       ("rust-filetime" ,rust-filetime-0.2)
                       ("rust-fs-extra" ,rust-fs-extra-1)
                       ("rust-human-date-parser" ,rust-human-date-parser-0.1)
                       ("rust-indexmap" ,rust-indexmap-2)
                       ("rust-indicatif" ,rust-indicatif-0.17)
                       ("rust-itertools" ,rust-itertools-0.12)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-lscolors" ,rust-lscolors-0.17)
                       ("rust-md-5" ,rust-md-5-0.10)
                       ("rust-mime" ,rust-mime-0.3)
                       ("rust-mime-guess" ,rust-mime-guess-2)
                       ("rust-native-tls" ,rust-native-tls-0.2)
                       ("rust-nix" ,rust-nix-0.27)
                       ("rust-notify-debouncer-full" ,rust-notify-debouncer-full-0.3)
                       ("rust-nu-ansi-term" ,rust-nu-ansi-term-0.50)
                       ("rust-nu-cmd-base" ,rust-nu-cmd-base-0.91)
                       ("rust-nu-color-config" ,rust-nu-color-config-0.91)
                       ("rust-nu-engine" ,rust-nu-engine-0.91)
                       ("rust-nu-glob" ,rust-nu-glob-0.91)
                       ("rust-nu-json" ,rust-nu-json-0.91)
                       ("rust-nu-parser" ,rust-nu-parser-0.91)
                       ("rust-nu-path" ,rust-nu-path-0.91)
                       ("rust-nu-pretty-hex" ,rust-nu-pretty-hex-0.91)
                       ("rust-nu-protocol" ,rust-nu-protocol-0.91)
                       ("rust-nu-system" ,rust-nu-system-0.91)
                       ("rust-nu-table" ,rust-nu-table-0.91)
                       ("rust-nu-term-grid" ,rust-nu-term-grid-0.91)
                       ("rust-nu-utils" ,rust-nu-utils-0.91)
                       ("rust-num-format" ,rust-num-format-0.4)
                       ("rust-num-traits" ,rust-num-traits-0.2)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-open" ,rust-open-5)
                       ("rust-os-pipe" ,rust-os-pipe-1)
                       ("rust-pathdiff" ,rust-pathdiff-0.2)
                       ("rust-percent-encoding" ,rust-percent-encoding-2)
                       ("rust-print-positions" ,rust-print-positions-0.6)
                       ("rust-procfs" ,rust-procfs-0.16)
                       ("rust-quick-xml" ,rust-quick-xml-0.31)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-rayon" ,rust-rayon-1)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-roxmltree" ,rust-roxmltree-0.19)
                       ("rust-rusqlite" ,rust-rusqlite-0.31)
                       ("rust-same-file" ,rust-same-file-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-serde-urlencoded" ,rust-serde-urlencoded-0.7)
                       ("rust-serde-yaml" ,rust-serde-yaml-0.9)
                       ("rust-sha2" ,rust-sha2-0.10)
                       ("rust-sysinfo" ,rust-sysinfo-0.30)
                       ("rust-tabled" ,rust-tabled-0.14)
                       ("rust-terminal-size" ,rust-terminal-size-0.3)
                       ("rust-titlecase" ,rust-titlecase-2)
                       ("rust-toml" ,rust-toml-0.8)
                       ("rust-trash" ,rust-trash-3)
                       ("rust-umask" ,rust-umask-2)
                       ("rust-unicode-segmentation" ,rust-unicode-segmentation-1)
                       ("rust-ureq" ,rust-ureq-2)
                       ("rust-url" ,rust-url-2)
                       ("rust-uu-cp" ,rust-uu-cp-0.0.23)
                       ("rust-uu-mkdir" ,rust-uu-mkdir-0.0.23)
                       ("rust-uu-mktemp" ,rust-uu-mktemp-0.0.23)
                       ("rust-uu-mv" ,rust-uu-mv-0.0.23)
                       ("rust-uu-whoami" ,rust-uu-whoami-0.0.23)
                       ("rust-uuid" ,rust-uuid-1)
                       ("rust-v-htmlescape" ,rust-v-htmlescape-0.15)
                       ("rust-wax" ,rust-wax-0.6)
                       ("rust-which" ,rust-which-6)
                       ("rust-windows" ,rust-windows-0.52)
                       ("rust-winreg" ,rust-winreg-0.52))
       #:cargo-development-inputs
       (("rust-dirs-next" ,rust-dirs-next-2)
        ("rust-mockito" ,rust-mockito-1)
        ("rust-nu-cmd-lang" ,rust-nu-cmd-lang-0.91)
        ("rust-nu-test-support" ,rust-nu-test-support-0.91)
        ("rust-quickcheck" ,rust-quickcheck-1)
        ("rust-quickcheck-macros" ,rust-quickcheck-macros-1)
        ("rust-rstest" ,rust-rstest-0.18))))
    (native-inputs (list pkg-config))
    (inputs (list openssl))
    (home-page
     "https://github.com/nushell/nushell/tree/main/crates/nu-command")
    (synopsis "Nushell's built-in commands")
    (description "This package contains nushell's built-in commands.")
    (license license:expat)))

(define-public rust-nu-engine-0.91
  (package
    (name "rust-nu-engine")
    (version "0.91.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu-engine" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0j4g3nhg9yw7nilnf3n1k4yfn5glmd3vbap1zxwzz24xw7ap62c7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-nu-glob" ,rust-nu-glob-0.91)
                       ("rust-nu-path" ,rust-nu-path-0.91)
                       ("rust-nu-protocol" ,rust-nu-protocol-0.91)
                       ("rust-nu-utils" ,rust-nu-utils-0.91))))
    (home-page "https://github.com/nushell/nushell/tree/main/crates/nu-engine")
    (synopsis "Nushell's evaluation engine")
    (description "This package provides nushell's evaluation engine.")
    (license license:expat)))

(define-public rust-nu-explore-0.91
  (package
    (name "rust-nu-explore")
    (version "0.91.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu-explore" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0j1xry4idjxrnyfz9l227s5hd82635dqc72gyw4zwq35izjrgqmm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-ansi-str" ,rust-ansi-str-0.8)
                       ("rust-crossterm" ,rust-crossterm-0.27)
                       ("rust-lscolors" ,rust-lscolors-0.17)
                       ("rust-nu-ansi-term" ,rust-nu-ansi-term-0.50)
                       ("rust-nu-color-config" ,rust-nu-color-config-0.91)
                       ("rust-nu-engine" ,rust-nu-engine-0.91)
                       ("rust-nu-json" ,rust-nu-json-0.91)
                       ("rust-nu-parser" ,rust-nu-parser-0.91)
                       ("rust-nu-protocol" ,rust-nu-protocol-0.91)
                       ("rust-nu-table" ,rust-nu-table-0.91)
                       ("rust-nu-utils" ,rust-nu-utils-0.91)
                       ("rust-ratatui" ,rust-ratatui-0.26)
                       ("rust-strip-ansi-escapes" ,rust-strip-ansi-escapes-0.2)
                       ("rust-terminal-size" ,rust-terminal-size-0.3)
                       ("rust-unicode-width" ,rust-unicode-width-0.1))))
    (home-page
     "https://github.com/nushell/nushell/tree/main/crates/nu-explore")
    (synopsis "Nushell table pager")
    (description "This package contains the nushell table pager.")
    (license license:expat)))

(define-public rust-nu-glob-0.91
  (package
    (name "rust-nu-glob")
    (version "0.91.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu-glob" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "094jkfb7rlcl0dxs5gnw8x30zv75s372l72zsg1wmv8lblzbfybx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags '("--release" "--"
                            "--skip=test::test_iteration_errors")
       #:cargo-development-inputs (("rust-doc-comment" ,rust-doc-comment-0.3))))
    (home-page "https://github.com/nushell/nushell/tree/main/crates/nu-glob")
    (synopsis "Support for matching file paths against Unix shell style patterns")
    (description
     "This package provides support for matching file paths against Unix shell
style patterns.")
    (license (list license:expat license:asl2.0))))

(define-public rust-nu-json-0.91
  (package
    (name "rust-nu-json")
    (version "0.91.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu-json" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1ga6kmmavd3rxjkk3j7jm6kjg2ny066a713ccca9nj0i9gbm6b1h"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-linked-hash-map" ,rust-linked-hash-map-0.5)
                       ("rust-num-traits" ,rust-num-traits-0.2)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/nushell/nushell/tree/main/crates/nu-json")
    (synopsis "Human JSON (Hjson) serialization file format")
    (description "This crate is a Rust library for parsing and generating Human
JSON Hjson.  It is built upon Serde, a high performance generic serialization
framework.")
    (license license:expat)))

(define-public rust-nu-lsp-0.91
  (package
    (name "rust-nu-lsp")
    (version "0.91.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu-lsp" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1sj61lnw74jrd7az9b5367gk4qry06s783k5vqgjznx4nqvr80xj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags
       '("--release" "--"
         ;; Not all files included.
         "--skip=diagnostics::tests::publish_diagnostics_fixed_unknown_variable"
         "--skip=diagnostics::tests::publish_diagnostics_variable_does_not_exists"
         "--skip=notification::tests::hover_correct_documentation_on_let"
         "--skip=notification::tests::hover_on_command_after_full_content_change"
         "--skip=notification::tests::hover_on_command_after_partial_content_change"
         "--skip=notification::tests::open_document_with_utf_char"
         "--skip=tests::complete_command_with_space"
         "--skip=tests::complete_command_with_utf_line"
         "--skip=tests::complete_on_variable"
         "--skip=tests::goto_definition_of_command"
         "--skip=tests::goto_definition_of_command_parameter"
         "--skip=tests::goto_definition_of_variable"
         "--skip=tests::hover_on_command"
         "--skip=tests::hover_on_variable")
       #:cargo-inputs (("rust-crossbeam-channel" ,rust-crossbeam-channel-0.5)
                       ("rust-lsp-server" ,rust-lsp-server-0.7)
                       ("rust-lsp-types" ,rust-lsp-types-0.95)
                       ("rust-miette" ,rust-miette-7)
                       ("rust-nu-cli" ,rust-nu-cli-0.91)
                       ("rust-nu-parser" ,rust-nu-parser-0.91)
                       ("rust-nu-protocol" ,rust-nu-protocol-0.91)
                       ("rust-reedline" ,rust-reedline-0.30)
                       ("rust-ropey" ,rust-ropey-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1))
       #:cargo-development-inputs
       (("rust-assert-json-diff" ,rust-assert-json-diff-2)
        ("rust-nu-cmd-lang" ,rust-nu-cmd-lang-0.91)
        ("rust-nu-command" ,rust-nu-command-0.91)
        ("rust-nu-test-support" ,rust-nu-test-support-0.91))))
    (native-inputs (list pkg-config))
    (inputs (list openssl))
    (home-page "https://github.com/nushell/nushell/tree/main/crates/nu-lsp")
    (synopsis "Nushell's integrated LSP server")
    (description "This package contains nushell'e integrated LSP server.")
    (license license:expat)))

(define-public rust-nu-parser-0.91
  (package
    (name "rust-nu-parser")
    (version "0.91.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu-parser" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "110jgz6lr0bi6904k63yqbsrcgfrpn044j2xz9if8akprk4p5w4z"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bytesize" ,rust-bytesize-1)
                       ("rust-chrono" ,rust-chrono-0.4)
                       ("rust-itertools" ,rust-itertools-0.12)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-nu-engine" ,rust-nu-engine-0.91)
                       ("rust-nu-path" ,rust-nu-path-0.91)
                       ("rust-nu-plugin" ,rust-nu-plugin-0.91)
                       ("rust-nu-protocol" ,rust-nu-protocol-0.91)
                       ("rust-serde-json" ,rust-serde-json-1))
       #:cargo-development-inputs (("rust-rstest" ,rust-rstest-0.18))))
    (home-page "https://github.com/nushell/nushell/tree/main/crates/nu-parser")
    (synopsis "Nushell's parser")
    (description "This package contains nushell's parser")
    (license license:expat)))

(define-public rust-nu-path-0.91
  (package
    (name "rust-nu-path")
    (version "0.91.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu-path" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "146lm48vna9w5kr46dclqmzl1cbz5k7j1zz6jl8i6d83np4nn1sa"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-dirs-next" ,rust-dirs-next-2)
                       ("rust-omnipath" ,rust-omnipath-0.1)
                       ("rust-pwd" ,rust-pwd-1))))
    (home-page "https://github.com/nushell/nushell/tree/main/crates/nu-path")
    (synopsis "Path handling library for Nushell")
    (description "This package provides path handling library for Nushell.")
    (license license:expat)))

(define-public rust-nu-plugin-0.91
  (package
    (name "rust-nu-plugin")
    (version "0.91.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu-plugin" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "054hmmz78njl6qhpcbbifip5r879ipa2j3y5ndlj588b9qaijvva"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bincode" ,rust-bincode-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-miette" ,rust-miette-7)
                       ("rust-nu-engine" ,rust-nu-engine-0.91)
                       ("rust-nu-protocol" ,rust-nu-protocol-0.91)
                       ("rust-rmp-serde" ,rust-rmp-serde-1)
                       ("rust-semver" ,rust-semver-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-typetag" ,rust-typetag-0.2))))
    (home-page "https://github.com/nushell/nushell/tree/main/crates/nu-plugin")
    (synopsis "Functionality for building Nushell plugins")
    (description
     "This package contains functionality for building Nushell plugins.")
    (license license:expat)))

(define-public rust-nu-pretty-hex-0.91
  (package
    (name "rust-nu-pretty-hex")
    (version "0.91.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu-pretty-hex" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1iq8amp5hqf2xxp5n74l5sgqv2bj204zwbjcnarhy88ijzjicrl6"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-nu-ansi-term" ,rust-nu-ansi-term-0.50))
                       #:cargo-development-inputs
                       (("rust-heapless" ,rust-heapless-0.8)
                        ("rust-rand" ,rust-rand-0.8))))
    (home-page
     "https://github.com/nushell/nushell/tree/main/crates/nu-pretty-hex")
    (synopsis "Pretty hex dump of bytes slice in the common style")
    (description
     "This package provides pretty hex dump of bytes slice in the common style.")
    (license license:expat)))

(define-public rust-nu-protocol-0.91
  (package
    (name "rust-nu-protocol")
    (version "0.91.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu-protocol" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0h6ikglmx79w5izcb7jv66s7cdsq6302pgwzyjyaxyw8jyprvx2g"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags
       '("--release" "--"
         ;; Not all files included.
         "--skip=config_add_unsupported_key"
         "--skip=config_add_unsupported_type"
         "--skip=config_add_unsupported_value"
         "--skip=config_affected_when_deep_mutated"
         "--skip=config_affected_when_mutated"
         "--skip=config_is_mutable"
         "--skip=config_preserved_after_do"
         "--skip=fancy_default_errors"
         "--skip=filesize_format_auto_metric_false"
         "--skip=filesize_format_auto_metric_true"
         "--skip=filesize_metric_false"
         "--skip=filesize_metric_overrides_format"
         "--skip=filesize_metric_true"
         "--skip=narratable_errors"
         "--skip=plugins")
       #:cargo-inputs (("rust-byte-unit" ,rust-byte-unit-5)
                       ("rust-chrono" ,rust-chrono-0.4)
                       ("rust-chrono-humanize" ,rust-chrono-humanize-0.2)
                       ("rust-fancy-regex" ,rust-fancy-regex-0.13)
                       ("rust-indexmap" ,rust-indexmap-2)
                       ("rust-lru" ,rust-lru-0.12)
                       ("rust-miette" ,rust-miette-7)
                       ("rust-nu-path" ,rust-nu-path-0.91)
                       ("rust-nu-system" ,rust-nu-system-0.91)
                       ("rust-nu-utils" ,rust-nu-utils-0.91)
                       ("rust-num-format" ,rust-num-format-0.4)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-typetag" ,rust-typetag-0.2))
       #:cargo-development-inputs
       (("rust-nu-test-support" ,rust-nu-test-support-0.91)
        ("rust-rstest" ,rust-rstest-0.18)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-strum" ,rust-strum-0.25)
        ("rust-strum-macros" ,rust-strum-macros-0.26))))
    (home-page
     "https://github.com/nushell/nushell/tree/main/crates/nu-protocol")
    (synopsis "Nushell's internal protocols, including its abstract syntax tree")
    (description
     "This package provides nushell's internal protocols, including its abstract
syntax tree.")
    (license license:expat)))

(define-public rust-nu-std-0.91
  (package
    (name "rust-nu-std")
    (version "0.91.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu-std" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1igdid80qbfgqdmcg6szq2rsi7i5qlyhplw74byh81vkqsn5z74w"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-miette" ,rust-miette-7)
                       ("rust-nu-engine" ,rust-nu-engine-0.91)
                       ("rust-nu-parser" ,rust-nu-parser-0.91)
                       ("rust-nu-protocol" ,rust-nu-protocol-0.91))))
    (home-page "https://github.com/nushell/nushell/tree/main/crates/nu-std")
    (synopsis "Standard library of Nushell")
    (description "This package provides the standard library of Nushell.")
    (license license:expat)))

(define-public rust-nu-system-0.91
  (package
    (name "rust-nu-system")
    (version "0.91.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu-system" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1jq87rjbmgpkf2cr2ajfs12f3wzpsh43m0drmrgj7b8lk2g6q9by"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-chrono" ,rust-chrono-0.4)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-libproc" ,rust-libproc-0.14)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-mach2" ,rust-mach2-0.4)
                       ("rust-nix" ,rust-nix-0.27)
                       ("rust-ntapi" ,rust-ntapi-0.4)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-procfs" ,rust-procfs-0.16)
                       ("rust-sysinfo" ,rust-sysinfo-0.30)
                       ("rust-windows" ,rust-windows-0.52))))
    (home-page "https://github.com/nushell/nushell/tree/main/crates/nu-system")
    (synopsis "Nushell system querying")
    (description "Nushell system querying")
    (license license:expat)))

(define-public rust-nu-table-0.91
  (package
    (name "rust-nu-table")
    (version "0.91.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu-table" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0r3b0lvkmjfkkcpxq6pls0sc5jp08a25ykfi0hifn0lsb9nady9m"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-fancy-regex" ,rust-fancy-regex-0.13)
                       ("rust-nu-ansi-term" ,rust-nu-ansi-term-0.50)
                       ("rust-nu-color-config" ,rust-nu-color-config-0.91)
                       ("rust-nu-engine" ,rust-nu-engine-0.91)
                       ("rust-nu-protocol" ,rust-nu-protocol-0.91)
                       ("rust-nu-utils" ,rust-nu-utils-0.91)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-tabled" ,rust-tabled-0.14))))
    (home-page "https://github.com/nushell/nushell/tree/main/crates/nu-table")
    (synopsis "Nushell table printing")
    (description "This package provides nushell table printing.")
    (license license:expat)))

(define-public rust-nu-term-grid-0.91
  (package
    (name "rust-nu-term-grid")
    (version "0.91.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu-term-grid" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "12f0i7m6flpkf1valkjfg6chalifpb65cknq91p22sii4dx0x89r"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-nu-utils" ,rust-nu-utils-0.91)
                       ("rust-unicode-width" ,rust-unicode-width-0.1))))
    (home-page
     "https://github.com/nushell/nushell/tree/main/crates/nu-term-grid")
    (synopsis "Nushell grid printing")
    (description "This package provides nushell grid printing.")
    (license license:expat)))

(define-public rust-nu-test-support-0.91
  (package
    (name "rust-nu-test-support")
    (version "0.91.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu-test-support" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "146ncw3318hcbhb7cpz3bdz0ypd8x4cpzhhl0q55r2mxxci7ik38"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags
       '("--release" "--"
         ;; Not all files included.
         "--skip=playground::tests::current_working_directory_back_to_root_from_anywhere"
         "--skip=playground::tests::current_working_directory_in_sandbox_directory_created")
       #:cargo-inputs (("rust-hamcrest2" ,rust-hamcrest2-0.3)
                       ("rust-nu-glob" ,rust-nu-glob-0.91)
                       ("rust-nu-path" ,rust-nu-path-0.91)
                       ("rust-nu-utils" ,rust-nu-utils-0.91)
                       ("rust-num-format" ,rust-num-format-0.4)
                       ("rust-tempfile" ,rust-tempfile-3)
                       ("rust-which" ,rust-which-6))))
    (home-page
     "https://github.com/nushell/nushell/tree/main/crates/nu-test-support")
    (synopsis "Support for writing Nushell tests")
    (description "This package contains support for writing Nushell tests.")
    (license license:expat)))

(define-public rust-nu-utils-0.91
  (package
    (name "rust-nu-utils")
    (version "0.91.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu-utils" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0xlnhli0zmv4nxznmjb2azq62ywq252zqvpx4668xv70japd74ag"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-crossterm-winapi" ,rust-crossterm-winapi-0.9)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-lscolors" ,rust-lscolors-0.17)
                       ("rust-num-format" ,rust-num-format-0.4)
                       ("rust-strip-ansi-escapes" ,rust-strip-ansi-escapes-0.2)
                       ("rust-sys-locale" ,rust-sys-locale-0.3)
                       ("rust-unicase" ,rust-unicase-2))))
    (home-page "https://github.com/nushell/nushell/tree/main/crates/nu-utils")
    (synopsis "Nushell utility functions")
    (description "This package contains utility functions for nushell.")
    (license license:expat)))
