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
;;; Copyright © 2024 Luís Henriques <henrix@camandro.org>
;;; Copyright © 2024 Giacomo Leidi <goodoldpaul@autistici.org>
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
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
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
  #:use-module (gnu packages scheme)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages texinfo)
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
    (version "3.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/fish-shell/fish-shell/"
                           "releases/download/" version "/"
                           "fish-" version ".tar.xz"))
       (sha256
        (base32 "0l5jlg0vplqln7ijqwirp1xl4j9npimzm58k77grj1yd8db9yk31"))))
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
                 "# Enable completions, functions and configurations in user's,"
                 " home's and system's guix profiles by adding them to __extra_*"
                 " variables.\n"
                 "set -l __guix_profile_paths ~/.guix-profile ~/.guix-home/profile"
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
    (version "0.19.2")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "xonsh" version))
        (sha256
          (base32
           "1c30p5qvn6c2l1jq7lby86m0cg9s2k3xrbbcznpc78jlv600dpfg"))
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
                           "test_ptk_prompt"
                           "test_quote_handling"
                           "test_script"
                           "test_shebang_cr"
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
           python-pip
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
    (version "7.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/ibara/oksh/releases/download/oksh-"
             version "/oksh-" version ".tar.gz"))
       (sha256
        (base32
         "0pgdxvy8jgydsyzk7vcc93pm09bihqvrn3i35gz1ncg9z31rbf20"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f   ; there are no tests available
       #:license-file-regexp "LEGAL"))
    (home-page "https://github.com/ibara/oksh")
    (synopsis "Portable OpenBSD Korn Shell")
    (description
     "Oksh is a portable OpenBSD ksh.  Not an official OpenBSD project.
Unlike other ports of OpenBSD ksh, this port is entirely self-contained
and aims to be maximally portable across operating systems and C compilers.")
    (license (list license:public-domain
                   ;; asprintf.c, issetugid.c, reallocarray.c, sh.1,
                   ;; strlcat.c, strlcpy.c, strtonum.c
                   license:isc
                   ;; confstr.c, siglist.c, signame.c, sys-queue.h, unvis.c,
                   ;; vis.c, vis.h
                   license:bsd-3))))

(define-public loksh
  (package
    (name "loksh")
    (version "7.7")
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
                "1dv2rlp806xcd0k3r4g5mjb3dm7fhqxfq5b31zjx6gqiki4kn507"))))
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

(define-public oils
  (package
    (name "oils")
    (version "0.22.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.oilshell.org/download/oils-for-unix-"
                           version ".tar.gz"))
       (sha256
        (base32 "0pylgbxbnp683g51lcbmmd0y149jm7q7vh8g67yviagsa7clmmks"))))
    (build-system gnu-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (replace 'configure
                 (lambda _
                   (setenv "CC" #$(cc-for-target))
                   (substitute* "configure"
                     ((" cc ") " $CC "))
                   (invoke "./configure" (string-append "--prefix=" #$output)
                           "--with-readline")))
               (replace 'build
                 (lambda _
                   (invoke "_build/oils.sh")))
               (replace 'install
                 (lambda _
                   (setenv "PREFIX" #$output)
                   (invoke "./install")))
               (replace 'check
                 ;; The tests are not distributed in the tarballs but upstream
                 ;; recommends running this smoke test.
                 ;; https://github.com/oilshell/oil/blob/release/0.22.0/INSTALL.txt#L30-L50
                 (lambda* (#:key tests? #:allow-other-keys)
                   (when tests?
                     (let ((osh "_bin/cxx-opt-sh/osh")
                           (ysh "_bin/cxx-opt-sh/ysh"))
                       (invoke/quiet osh "-c" "echo hi")
                       (invoke/quiet osh "-n" "configure")
                       (invoke/quiet ysh "-c" "echo hi")
                       (invoke/quiet ysh "-c"
                                     "json write ({x: 42})"))))))))
    (inputs
     (list readline))
    (home-page "https://www.oilshell.org")
    (synopsis "Programming language and Bash-compatible Unix shell")
    (description "Oils is a programming language with automatic translation for
Bash.  It includes OSH, a Unix/POSIX shell that runs unmodified Bash
scripts and YSH is a legacy-free shell, with structured data for Python and
JavaScript users who avoid shell.")
    (license (list license:asl2.0))))

(define-public oil
  ;; Since release 0.16.0 the former Oil project has been renamed to Oils:
  ;; <https://www.oilshell.org/blog/2023/03/rename.html>.
  (deprecated-package "oil" oils))

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
