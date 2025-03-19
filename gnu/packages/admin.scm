;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012-2025 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013 Cyril Roelandt <tipecaml@gmail.com>
;;; Copyright © 2014, 2015, 2016, 2018, 2019, 2020 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014, 2015-2018, 2020-2023 Eric Bavier <bavier@posteo.net>
;;; Copyright © 2015, 2016 Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>
;;; Copyright © 2015 Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
;;; Copyright © 2015 Eric Dvorsak <eric@dvorsak.fr>
;;; Copyright © 2016, 2017, 2020 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016 Pjotr Prins <pjotr.guix@thebird.nl>
;;; Copyright © 2016, 2017, 2023 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016-2025 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Peter Feigl <peter.feigl@nexoid.at>
;;; Copyright © 2016 John J. Foerch <jjfoerch@earthlink.net>
;;; Copyright © 2016, 2017 Nikita <nikita@n0.is>
;;; Copyright © 2016–2022 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2016 John Darrington <jmd@gnu.org>
;;; Copyright © 2017 Ben Sturmfels <ben@sturm.com.au>
;;; Copyright © 2017 Ethan R. Jones <doubleplusgood23@gmail.com>
;;; Copyright © 2017 Christine Lemmer-Webber <cwebber@dustycloud.org>
;;; Copyright © 2017, 2018, 2020 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2018, 2019 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2018 Pierre-Antoine Rouby <pierre-antoine.rouby@inria.fr>
;;; Copyright © 2018 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2018 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2019 Brett Gilio <brettg@gnu.org>
;;; Copyright © 2019,2020 Björn Höfling <bjoern.hoefling@bjoernhoefling.de>
;;; Copyright © 2019 Jakob L. Kreuze <zerodaysfordays@sdf.org>
;;; Copyright © 2019 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2019 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2019, 2021, 2022 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2019, 2020, 2021 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2020 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2020, 2023, 2024, 2025 Janneke Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2020, 2021, 2022 Michael Rohleder <mike@rohleder.de>
;;; Copyright © 2020 Vincent Legoll <vincent.legoll@gmail.com>
;;; Copyright © 2020 Morgan Smith <Morgan.J.Smith@outlook.com>
;;; Copyright © 2021-2025 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2021, 2023, 2024 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2021 Stefan Reichör <stefan@xsteve.at>
;;; Copyright © 2021 qblade <qblade@protonmail.com>
;;; Copyright © 2021 Hyunseok Kim <lasnesne@lagunposprasihopre.org>
;;; Copyright © 2021 David Larsson <david.larsson@selfhosted.xyz>
;;; Copyright © 2021 WinterHound <winterhound@yandex.com>
;;; Copyright © 2021 Brice Waegeneire <brice@waegenei.re>
;;; Copyright © 2021 Maxime Devos <maximedevos@telenet.be>
;;; Copyright © 2021 Roel Janssen <roel@gnu.org>
;;; Copyright © 2021, 2025 muradm <mail@muradm.net>
;;; Copyright © 2021 pineapples <guixuser6392@protonmail.com>
;;; Copyright © 2021 Petr Hodina <phodina@protonmail.com>
;;; Copyright © 2021-2025 Artyom V. Poptsov <poptsov.artyom@gmail.com>
;;; Copyright © 2022 Wamm K. D. <jaft.r@outlook.com>
;;; Copyright © 2022 Roman Riabenko <roman@riabenko.com>
;;; Copyright © 2022 Petr Hodina <phodina@protonmail.com>
;;; Copyright © 2022 Andreas Rammhold <andreas@rammhold.de>
;;; Copyright © 2022 ( <paren@disroot.org>
;;; Copyright © 2022, 2023 Matthew James Kraai <kraai@ftbfs.org>
;;; Copyright © 2022 jgart <jgart@dismail.de>
;;; Copyright © 2023 Juliana Sims <juli@incana.org>
;;; Copyright © 2023 Lu Hui <luhux76@gmail.com>
;;; Copyright © 2023 Yovan Naumovski <yovan@gorski.stream>
;;; Copyright © 2023 Alexey Abramov <levenson@mmer.org>
;;; Copyright © 2023 Bruno Victal <mirai@makinata.eu>
;;; Copyright © 2023 Tobias Kortkamp <tobias.kortkamp@gmail.com>
;;; Copyright © 2023 Jaeme Sifat <jaeme@runbox.com>
;;; Copyright © 2023 Nicolas Graves <ngraves@ngraves.fr>
;;; Copyright © 2023 Tomás Ortín Fernández <tomasortin@mailbox.org>
;;; Copyright © 2024 dan <i@dan.games>
;;; Copyright © 2024 gemmaro <gemmaro.dev@gmail.com>
;;; Copyright © 2024 Richard Sent <richard@freakingpenguin.com>
;;; Copyright © 2024 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2024 nathan <nathan_mail@nborghese.com>
;;; Copyright © 2024 Nikita Domnitskii <nikita@domnitskii.me>
;;; Copyright © 2024 Roman Scherer <roman@burningswell.com>
;;; Copyright © 2024 Ashish SHUKLA <ashish.is@lostca.se>
;;; Copyright © 2024 Ashvith Shetty <ashvithshetty10@gmail.com>
;;; Copyright © 2025 Dariqq <dariqq@posteo.net>
;;; Copyright © 2024 nik gaffney <nik@fo.am>
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

(define-module (gnu packages admin)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system emacs)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system python)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system qt)
  #:use-module (guix build-system ruby)
  #:use-module (guix build-system trivial)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix platform)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages acl)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages attr)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages c)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages crates-graphics)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages crates-windows)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages cryptsetup)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages cyrus-sasl)
  #:use-module (gnu packages datastructures)
  #:use-module (gnu packages debian)
  #:use-module (gnu packages dns)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages file)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-check)
  #:use-module (gnu packages golang-compression)
  #:use-module (gnu packages golang-crypto)
  #:use-module (gnu packages golang-web)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages groff)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages image)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages inkscape)
  #:use-module (gnu packages kerberos)
  #:use-module (gnu packages libbsd)
  #:use-module (gnu packages libunwind)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages logging)
  #:use-module (gnu packages lsof)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages mail)
  #:use-module (gnu packages man)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages markup)
  #:use-module (gnu packages mcrypt)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages nettle)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages openldap)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages patchutils)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-check)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages polkit)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages selinux)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg))

;; This package uses su instead of sudo (because of SpaceFM).
(define-public ktsuss
  (package
    (name "ktsuss")
    (version "2.1")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/nomius/ktsuss")
         (commit version)))
       (sha256
        (base32 "0q9931f9hp47v1n8scli4bdg2rkjpf5jf8v7jj2gdn83aia1r2hz"))
       (file-name (git-file-name name version))))
    (build-system glib-or-gtk-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-file-names
           (lambda _
             (substitute* "configure.ac"
               (("supath=`which su 2>/dev/null`")
                "supath=/run/privileged/bin/su"))
             #t)))))
    (native-inputs
     (list autoconf automake libtool pkg-config))
    (inputs
     (list glib gtk+-2))
    (synopsis "Graphical front end for @command{su}")
    (description
     "Ktsuss stands for ``Keep the @command{su} simple, stupid''.
It is a graphical version of @command{su} written in C and GTK+ 2, with
simplicity in mind.")
    (home-page "https://github.com/nomius/ktsuss")
    (license license:bsd-3)))

(define-public aide
  (package
    (name "aide")
    (version "0.18.8")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/aide/aide/releases/download/v"
                           version "/aide-" version ".tar.gz"))
       (sha256
        (base32 "0q1sp0vwrwbmw6ymw1kwd4i8walijwppa0dq61b2qzni6b32srhn"))))
    (build-system gnu-build-system)
    (arguments
     (list #:configure-flags #~(list "--with-posix-acl"
                                     "--with-selinux"
                                     "--with-xattr"
                                     "--with-config-file=/etc/aide.conf")))
    (native-inputs
     (list bison flex pkg-config))
    (inputs
     (list acl
           attr
           libgcrypt
           libgpg-error
           libmhash
           libselinux
           pcre2
           `(,zlib "static")
           zlib))
    (synopsis "File and directory integrity checker")
    (description
     "AIDE (Advanced Intrusion Detection Environment) is a file and directory
integrity checker.  It creates a database from the regular expression rules
that it finds from its configuration files.  Once this database is initialized
it can be used to verify the integrity of the files.  It has several message
digest algorithms that are used to check the integrity of files.  All of the
usual file attributes can be checked for inconsistencies.")
    (home-page "https://aide.github.io/")
    (license license:gpl2+)))

(define-public hetznercloud-cli
  (package
    (name "hetznercloud-cli")
    (version "1.49.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hetznercloud/cli")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0mgd1rv0i18h7jbzl034ffpfxvnjirp60qwxsjpfy42jh1d8xbjm"))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #f ; XXX: figure out hot to enable them
      #:install-source? #f
      #:import-path "github.com/hetznercloud/cli/cmd/hcloud"
      #:unpack-path "github.com/hetznercloud/cli"))
    (native-inputs
     (list go-github-com-burntsushi-toml
           go-github-com-cheggaaa-pb-v3
           go-github-com-dustin-go-humanize
           go-github-com-fatih-color
           go-github-com-fatih-structs
           go-github-com-goccy-go-yaml
           go-github-com-guptarohit-asciigraph
           go-github-com-hetznercloud-hcloud-go-v2
           go-github-com-jedib0t-go-pretty-v6
           go-github-com-spf13-cast
           go-github-com-spf13-cobra
           go-github-com-spf13-pflag
           go-github-com-spf13-viper
           go-github-com-stretchr-testify
           go-github-com-swaggest-assertjson
           go-go-uber-org-mock
           go-golang-org-x-crypto
           go-golang-org-x-term))
    (home-page "https://github.com/hetznercloud/cli")
    (synopsis "Command-line interface for the Hetzner Cloud service")
    (description
     "This package provides the @code{hcloud} binary, a command-line interface
for interacting with the @url{https://www.hetzner.com/,Hetzner Cloud}
service.")
    (license license:expat)))

(define-public progress
  (package
    (name "progress")
    (version "0.17")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Xfennec/progress")
             (commit (string-append "v" version))))
       (sha256
        (base32 "1cg1vdk2891sdcbn7yc9a6mzdxplm63qsk1kq0jr4j8ym28v09xf"))
       (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config which))
    (inputs
     (list ncurses))
    (arguments
     (list #:tests? #f                      ; no test suite
           #:make-flags
           #~(list (string-append "CC=" #$(cc-for-target))
                   (string-append "PKG_CONFIG=" #$(pkg-config-for-target))
                   (string-append "PREFIX=" #$output))
           #:phases
           #~(modify-phases %standard-phases
               (delete 'configure))))         ; no configure script
    (home-page "https://github.com/Xfennec/progress")
    (synopsis "Program to view the progress of the coreutils commands")
    (description "A program that looks for coreutils basic commands (cp, mv,
dd, tar, gzip/gunzip, cat, etc.) currently running on your system and displays
the percentage of copied data.  It can also show estimated time and throughput,
and provides a \"top-like\" mode (monitoring).")
    (license license:gpl3+)))

(define-public shepherd-0.8
  (package
    (name "shepherd")
    (version "0.8.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/shepherd/shepherd-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0x9zr0x3xvk4qkb6jnda451d5iyrl06cz1bjzjsm0lxvjj3fabyk"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Build with -O1 to work around <https://bugs.gnu.org/48368>.
                  (substitute* "Makefile.in"
                    (("compile --target")
                     "compile -O1 --target"))))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--localstatedir=/var")
       #:make-flags '("GUILE_AUTO_COMPILE=0")))
    (native-inputs
     (list pkg-config
           ;; This is the Guile we use as a cross-compiler...
           guile-3.0))
    (inputs
     ;; ... and this is the one that appears in shebangs when cross-compiling.
     (list guile-3.0
           ;; The 'shepherd' command uses Readline when used interactively.  It's
           ;; an unusual use case though, so we don't propagate it.
           guile-readline))
    (synopsis "System service manager")
    (description
     "The GNU Shepherd is a daemon-managing daemon, meaning that it supervises
the execution of system services, replacing similar functionality found in
typical init systems.  It provides dependency-handling through a convenient
interface and is based on GNU Guile.")
    (license license:gpl3+)
    (home-page "https://www.gnu.org/software/shepherd/")))

(define-public shepherd-0.9
  (package
    (inherit shepherd-0.8)
    (version "0.9.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/shepherd/shepherd-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0qy2yq13xhf05an5ilz7grighdxicx56211yaarqq5qigiiybc32"))))
    (arguments
     (list #:configure-flags #~'("--localstatedir=/var")
           #:make-flags #~'("GUILE_AUTO_COMPILE=0")
           #:phases (if (%current-target-system)
                        #~(modify-phases %standard-phases
                            (add-before 'configure 'set-fibers-directory
                              (lambda _
                                ;; When cross-compiling, refer to the target
                                ;; Fibers, not the native one.
                                (substitute* '("herd.in" "shepherd.in")
                                  (("%FIBERS_SOURCE_DIRECTORY%")
                                   #$(file-append
                                      (this-package-input "guile-fibers")
                                      "/share/guile/site/3.0"))
                                  (("%FIBERS_OBJECT_DIRECTORY%")
                                   #$(file-append
                                      (this-package-input "guile-fibers")
                                      "/lib/guile/3.0/site-ccache"))))))
                        #~%standard-phases)))

    (native-inputs (list pkg-config guile-3.0
                         guile-fibers-1.1))       ;for cross-compilation
    (inputs (list guile-3.0 guile-fibers-1.1))))

(define-public shepherd-0.10
  (package
    (inherit shepherd-0.9)
    (version "0.10.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/shepherd/shepherd-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0k40n9qm5r5rqf94isa1857ghd4329zc5rjf3ll2572gpiw3ij4x"))))
    (native-inputs (modify-inputs (package-native-inputs shepherd-0.9)
                     (replace "guile-fibers"
                       ;; Work around
                       ;; <https://github.com/wingo/fibers/issues/89>.  This
                       ;; affects any system without a functional real-time
                       ;; clock (RTC), but in practice these are typically Arm
                       ;; single-board computers.
                       (if (or (target-arm?)
                               (target-riscv64?))
                           guile-fibers-1.1
                           guile-fibers))))
    (inputs (modify-inputs (package-inputs shepherd-0.9)
              (replace "guile-fibers"
                (this-package-native-input "guile-fibers"))))))

(define-public shepherd-1.0
  (package
    (inherit shepherd-0.10)
    (version "1.0.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/shepherd/shepherd-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1n7k1d8zb4v0h5s1kq2k51381a8dgz9k75fybklgnxpxbzmpkrs0"))))
    (arguments
     (substitute-keyword-arguments (package-arguments shepherd-0.10)
       ((#:configure-flags flags #~'())
        #~(list "--localstatedir=/var"

                ;; Gzip and zstd are used by the log rotation service.
                (string-append "--with-gzip=" #$(this-package-input "gzip")
                               "/bin/gzip")
                (string-append "--with-zstd=" #$(this-package-input "zstd")
                               "/bin/zstd")))))
    (inputs (modify-inputs (package-inputs shepherd-0.10)
              (append gzip zstd)))))

(define-public shepherd shepherd-0.10)

(define-public guile2.2-shepherd
  (package
    (inherit shepherd-0.10)
    (name "guile2.2-shepherd")
    (native-inputs (list pkg-config guile-2.2))
    (inputs (list guile-2.2 guile2.2-fibers))))

(define-public shepherd-run
  (package
    (name "shepherd-run")
    (version "0.2.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://git.sr.ht/~efraim/shepherd-run")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0mvn5qi4bq9nsb1462pbzssb1z5w2s160lqd2ps3rjjc4z3f3gjj"))))
    (build-system gnu-build-system)
    (arguments
     (list
       #:make-flags #~(list (string-append "PREFIX=" #$output))
       #:phases
       #~(modify-phases %standard-phases
           (delete 'configure)          ; No configure script.
           ;; First 'check checks the shell script which loads the gawk code.
           ;; This 'check checks the installed gawk script.
           (add-after 'patch-shebangs 'check-again
             (lambda args
               (apply (assoc-ref %standard-phases 'check)
                      (append args
                              (list #:make-flags
                                    (list (string-append "BINARY=" %output
                                                         "/bin/shepherd-run"))))))))))
    (native-inputs (list diffutils help2man))
    (inputs (list gawk))
    (synopsis "Create GNU Shepherd services from the command line")
    (description
     "Shepherd-run is a script which assists in creating one-off shepherd
services from the command line.  It is meant to partially fill the void left
by @command{systemd-run}, since GNU Guix uses GNU Shepherd as its system service
manager.")
    (home-page "https://git.sr.ht/~efraim/shepherd-run")
    (license license:gpl3+)))

(define-public swineherd
  (package
    (name "swineherd")
    (version "0.0.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/BIMSBbioinfo/swineherd")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0iij1pl0y410k1dk1ifa56dxmjb1blv0y3k5rxy794gwg6w6c480"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--localstatedir=/var")
       #:make-flags '("GUILE_AUTO_COMPILE=0")))
    (native-inputs
     (list autoconf automake guile-3.0 pkg-config texinfo))
    (propagated-inputs
     (list btrfs-progs
           guile-config
           guile-fibers-1.3
           guile-netlink
           guile-3.0
           guix
           shepherd-0.10))
    (home-page "https://github.com/BIMSBbioinfo/swineherd")
    (synopsis "System container manager")
    (description
     "This project aims to provide an extension to the Shepherd, retraining it
as a swineherd, a manager of crude system containers.  It does this by
providing a Shepherd service @code{swineherd} that talks to the Shepherd
process to create Guix System containers as Shepherd services.  It also comes
with an optional HTTP API server.")
    (license license:gpl3+)))

(define-public cfm
  (package
    (name "cfm")
    (version "0.6.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/WillEccles/cfm")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "14gapia902f29wa4dlrrj8jcwcff9bfvyhjccw9ddy2gxx2g8wmr"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no test suite
       #:make-flags
       (list (string-append "CC=" ,(cc-for-target))
             (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         ;; Keeping xdg-open optional avoids a size increase of 293%.
         (delete 'configure))))         ; no configure script
    (home-page "https://eccles.dev/cfm/")
    (synopsis
     "Simple terminal file manager with @command{vi}-inspired key bindings")
    (description
     "The Cactus File Manager (@command{cfm}) helps you manage your files
visually from a text terminal.  It aims to be simple and fast, with key bindings
inspired by @command{vi}.")
    (license license:mpl2.0)))

(define-public cloud-utils
  (package
    (name "cloud-utils")
    (version "0.33")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://launchpad.net/cloud-utils/trunk/"
             version "/+download/cloud-utils-" version ".tar.gz"))
       (sha256
        (base32
         "0xxdi55lzw7j91zfajw7jhd2ilsqj2dy04i9brlk8j3pvb5ma8hk"))))
    (build-system gnu-build-system)
    (arguments
     '(#:modules
       ((guix build gnu-build-system)
        (guix build utils)
        (srfi srfi-26))
       #:make-flags
       (let ((out (assoc-ref %outputs "out")))
         (list (string-append "BINDIR=" out "/bin")
               (string-append "MANDIR=" out "/share/man/man1")
               (string-append "DOCDIR=" out "/share/doc")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'check)
         (add-after 'install 'wrap
           (lambda* (#:key outputs inputs #:allow-other-keys)
             (let ((growpart (string-append (assoc-ref outputs "out")
                                            "/bin/growpart")))
               (wrap-program growpart
                 `("PATH" ":" prefix
                   ,(map dirname
                         (map (cut search-input-file inputs <>)
                              (list "bin/readlink"
                                    "sbin/sfdisk"
                                    "bin/sed"
                                    "bin/grep"
                                    "bin/awk"
                                    "bin/udevadm"
                                    "bin/flock")))))))))))
    (inputs
     (list bash-minimal                 ;for wrap-program
           coreutils                    ;for readlink and cat
           sed                          ;growpart
           grep                         ;growpart
           gawk                         ;awk for growpart
           eudev                        ;udevadm for growpart
           python
           util-linux))                 ;sfdisk, flock, partx, blkid for growpart
    (home-page "https://launchpad.net/cloud-utils")
    (synopsis "Set of utilities for cloud computing environments")
    (description
     "This package contains a set of utilities for cloud computing
environments:

@itemize @bullet
@item @command{cloud-localds} Create a disk for cloud-init to utilize nocloud
@item @command{cloud-publish-image} Wrapper for cloud image publishing
@item @command{cloud-publish-tarball} Wrapper for publishing cloud tarballs
@item @command{cloud-publish-ubuntu} Import a Ubuntu cloud image
@item @command{ec2metadata} Query and display @acronym{EC2,Amazon Elastic
  Compute Cloud} metadata
@item @command{growpart} Grow a partition to fill the device
@item @command{mount-image-callback} Mount a file and run a command
@item @command{resize-part-image} Resize a partition image to a new size
@item @command{ubuntu-cloudimg-query} Get the latest Ubuntu
  @acronym{AMI,Amazon Machine Image}
@item @command{ubuntu-ec2-run} Run a @acronym{EC2,Amazon Elastic Compute
  Cloud} instance using Ubuntu
@item @command{vcs-run} Obtain a repository, and run a command
@item @command{write-mime-multipart} Handle multipart
  @acronym{MIME,Multipurpose Internet Mail Extensions} messages
@end itemize")
    (license license:gpl3)))

(define-public collectl
  (package
    (name "collectl")
    (version "4.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://sourceforge/collectl/collectl/collectl-" version
             "/collectl-" version ".src.tar.gz"))
       (sha256
        (base32 "1wc9k3rmhqzh6cx5dcpqhlc3xcpadsn2ic54r19scdjbjx6jd1r1"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f ; There are no tests.
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build) ; There's nothing to build.
          (replace 'configure
            (lambda _
              (substitute* "INSTALL"
                (("DESTDIR:=\"/\"") (format #f "DESTDIR:=~s" #$output))
                (("DESTDIR/usr") "DESTDIR"))))
          (replace 'install
            (lambda _
              (substitute* "collectl"
                (("\\$configFile='';")
                 (string-append "$configFile='" #$output "/etc';")))
              (invoke "./INSTALL"))))))
    (inputs
     (list perl))
    (home-page "http://collectl.sourceforge.net")
    (synopsis "Performance data collector")
    (description
     "This package provides a program that collects various performance
measurement data like CPU, memory, disk and network performance numbers.")
    (license license:artistic2.0)))

(define-public daemontools
  (package
    (name "daemontools")
    (version "0.76")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://cr.yp.to/daemontools/"
                    "daemontools-" version ".tar.gz"))
              (sha256
               (base32
                "07scvw88faxkscxi91031pjkpccql6wspk4yrlnsbrrb5c0kamd5"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ;; No tests as far as I can tell.
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _
             (chdir ,(string-append "daemontools-" version))
             #t))
         (delete 'configure)
         (add-before 'build 'patch
           (lambda _
             (substitute* "src/error.h"
               (("extern int errno;")
                "#include <errno.h>"))
             #t))
         (replace 'build
           (lambda _
             (invoke "package/compile")))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (for-each (lambda (file)
                           (install-file file bin))
                         (find-files "command")))
             #t)))))
    (synopsis "Tools for managing UNIX style services")
    (description
     "@code{daemontools} is a collection of tools for managing UNIX
services.")
    (license license:public-domain)
    (home-page "https://cr.yp.to/daemontools.html")))

(define-public daemonize
  (package
    (name "daemonize")
    (version "1.7.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/bmc/daemonize")
             (commit (string-append "release-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0w4g0iyssyw7dd0061881z8s5czcl01mz6v00znax57zfxjqpvnm"))))
    (build-system gnu-build-system)
    (arguments '(#:tests? #f))          ; No tests available.
    (home-page "https://software.clapper.org/daemonize/")
    (synopsis "Command line utility to run a program as a daemon")
    (description
     "daemonize runs a command as a Unix daemon.  It will close all open file
descriptors, change working directory of the process to the root filesystem,
reset its umask, run in the background, ignore I/O signals, handle
@code{SIGCHLD}, etc.  Most programs that are designed to be run as daemons do
that work for themselves.  However, you’ll occasionally run across one that
does not.  When you must run a daemon program that does not properly make
itself into a true Unix daemon, you can use daemonize to force it to run as a
true daemon.")
    (license license:bsd-3)))

(define-public dfc
  (package
   (name "dfc")
   (version "3.1.1")
   (source
    (origin
     (method url-fetch)
      (uri (string-append
            "https://projects.gw-computing.net/attachments/download/615/dfc-"
            version ".tar.gz"))
      (sha256
       (base32
        "0m1fd7l85ckb7bq4c5c3g257bkjglm8gq7x42pkmpp87fkknc94n"))))
   (build-system cmake-build-system)
   (arguments '(#:tests? #f)) ; There are no tests.
   (native-inputs (list gettext-minimal))
   (home-page "https://projects.gw-computing.net/projects/dfc")
   (synopsis "Display file system space usage using graphs and colors")
   (description
    "dfc (df color) is a modern version of df.  It uses colors, draws pretty
graphs and can export its output to different formats.")
   (license license:bsd-3)))

(define-public facter
  (package
    (name "facter")
    (version "4.4.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/puppetlabs/facter")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "080v0ml2svw2vbzfa659v8718pmhh2kav0l0q1jjvc6mm8sgnmmn"))))
    (build-system ruby-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'delete-facter-ng-gemspec
            (lambda _
              ;; XXX: ruby-build-system incorrectly finds
              ;; facter-ng.gemspec from this directory and tries to
              ;; build that instead of the proper facter.gemspec.
              ;; Just delete it as a workaround, as it appears to
              ;; only exist for backwards-compatibility after the
              ;; facter-ng->facter rename.
              (delete-file "agent/facter-ng.gemspec")))
          (add-after 'unpack 'embed-absolute-references
            ;; Refer to absolute executable file names to avoid propagation.
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* (find-files "lib/facter/resolvers" "\\.rb$")
                (("execute\\('(which |)([^ ']+)" _ _ name)
                 (string-append "execute('" (or (which name)
                                                name))))))
          (delete 'check)
          (add-after 'wrap 'check
            (lambda* (#:key tests? outputs #:allow-other-keys)
              ;; XXX: The test suite wants to run Bundler and
              ;; complains that the gemspec is invalid.  For now
              ;; just make sure that we can run the wrapped
              ;; executable directly.
              (if tests?
                  (invoke (string-append (assoc-ref outputs "out")
                                         "/bin/facter")
                          ;; Many facts depend on /sys, /etc/os-release,
                          ;; etc, so we only run a small sample.
                          "facterversion" "architecture"
                          "kernel" "kernelversion")
                  (format #t "tests disabled~%")))))))
    (inputs
     (list ruby-hocon
           ruby-sys-filesystem
           ruby-thor

           ;; For ‘embed-absolute-references’.
           dmidecode
           inetutils                    ; for ‘hostname’
           iproute
           pciutils
           util-linux))
    (synopsis "Collect and display system facts")
    (description
     "Facter is a tool that gathers basic facts about nodes (systems) such
as hardware details, network settings, OS type and version, and more.  These
facts can be collected on the command line with the @command{facter} command
or via the @code{facter} Ruby library.")
    (home-page "https://github.com/puppetlabs/facter")
    (license license:expat)))

(define-public ttyload
  (let ((revision "1")
        (commit "f9495372801ce4b4dad98ad854203e694c31c1eb"))
    (package
      (name "ttyload")
      (version (git-version "0.5.3" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/lindes/ttyload")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0ldb7a13b9v876c6cbrs78pkizj64drnqx95z5shfbwgpwfhr4im"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f      ; no tests
         #:make-flags
         (list (string-append "CC=" ,(cc-for-target)))
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (bin (string-append out "/bin")))
                 (install-file "ttyload" bin)))))))
      (home-page "https://www.daveltd.com/src/util/ttyload/")
      (synopsis "Console based color-coded graphs of CPU load average")
      (description
       "Show graphs for 1 minute, 5 minute, 15 minute load averages on the
console.")
      ;; This package uses a modified version of the "ISC License".
      (license (license:non-copyleft "file://LICENSE")))))

(define-public btop
  (package
    (name "btop")
    (version "1.4.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/aristocratos/btop")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0vgw6hwqh6zbzrvrn3i0xwi9ykm1qdvhqcyz3mjakd7w303lx603"))
              (patches
               (search-patches "btop-fix-segfault-on-intel-gpus.patch"))))
    (build-system gnu-build-system)
    (native-inputs (list lowdown))
    (arguments
     (list #:tests? #f ;no test suite
           #:make-flags #~(list (string-append "PREFIX=" #$output)
                                (string-append "CC=" #$(cc-for-target)))
           #:phases #~(modify-phases %standard-phases
                        (delete 'configure))))
    (home-page "https://github.com/aristocratos/btop")
    (synopsis "Resource monitor")
    (description "Btop++ provides unified monitoring of CPU, memory, network
and processes.")
    (license license:asl2.0)))

(define-public smem
  (package
    (name "smem")
    (version "1.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://selenic.com/repo/smem/archive/"
                                  version ".tar.bz2"))
              (file-name
               (string-append name "-" version ".tar.bz2"))
              (sha256
               (base32
                "19ibv1byxf2b68186ysrgrhy5shkc5mc69abark1h18yigp3j34m"))))
    (build-system gnu-build-system)
    (arguments
     (list #:tests? #f ; There is no test suite.
           #:make-flags #~(list "smemcap")
           #:phases
           #~(modify-phases %standard-phases
               (delete 'configure)
               (replace 'build
                 (lambda _
                   (let* ((system #$(cond ((target-x86?) "X86")
                                          ((target-arm?) "ARM")
                                          ((target-powerpc?) "POWER")
                                          (else "CROSS_FINGERS"))))
                     (format #t "Building for ~a~%" system)
                     (invoke #$(cc-for-target) "-o" "smemcap" "smemcap.c"
                             "-g" "-Wall" "-D" system))))
               (replace 'install
                 (lambda _
                   (let ((bin (string-append #$output "/bin"))
                         (man1 (string-append #$output "/share/man/man8")))
                     (install-file "smemcap" bin)
                     (install-file "smem" bin)
                     (mkdir-p man1)
                     (copy-file "smem.8" (string-append man1 "/smem.8"))))))))
    (native-inputs (list python-minimal-wrapper))
    (home-page "https://www.selenic.com/smem/")
    (synopsis "Memory reporting tool")
    (description
     "This package provides a command line tool that can give numerous reports
on memory usage on GNU/Linux systems.")
    (license license:gpl2+)))

(define-public htop
  (package
    (name "htop")
    (version "3.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/htop-dev/htop")
             (commit version)))
       (sha256
        (base32 "0g2rpp9plblmd9khic2f06089hfh0iy521dqqnr3vkin6s9m0f58"))
       (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (inputs
     (list ncurses))
    (native-inputs
     (list autoconf automake python-minimal-wrapper))     ; for scripts/MakeHeader.py
    (home-page "https://htop.dev")
    (synopsis "Interactive process viewer")
    (description
     "This is htop, an interactive process viewer.  It is a text-mode
application (for console or X terminals) and requires ncurses.")
    (license license:gpl2)))

(define-public bashtop
  (package
    (name "bashtop")
    (version "0.9.25")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/aristocratos/bashtop")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "07nlr6vmyb7yihaxj1fp424lmhwkdjl6mls92v90f6gsvikpa13v"))))
    (build-system gnu-build-system)
    (arguments
     '(#:make-flags (list (string-append "PREFIX=" %output))
       #:tests? #f      ; bats test fails with loading load.bash
       #:phases (modify-phases %standard-phases (delete 'configure))))
    (home-page "https://github.com/aristocratos/bashtop")
    (synopsis "Linux/OSX/FreeBSD resource monitor")
    (description "Resource monitor that shows usage and stats for processor,
memory, disks, network and processes.")
    (license license:asl2.0)))

(define-public bpytop
  (package
    (name "bpytop")
    (version "1.0.68")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "bpytop" version))
       (sha256
        (base32 "1clvajbv7pzlya9s1xs6dvjic8rv3kx7aqiwnjxapiypx246gdjk"))))
    (build-system python-build-system)
    (inputs
     (list python-psutil))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; sanity-check phase fail, but the application seems to be working
         (delete 'sanity-check)
         (add-after 'install 'install-themes
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((themes (string-append (assoc-ref outputs "out")
                                          "/lib/python"
                                          ,(version-major+minor
                                            (package-version python))
                                          "/site-packages/bpytop-themes")))
               (mkdir-p themes)
               (copy-recursively "themes" themes)))))))
    (home-page
     "https://github.com/aristocratos/bpytop")
    (synopsis "Resource monitor")
    (description "Resource monitor that shows usage and stats for processor,
memory, disks, network and processes.  It's a Python port and continuation of
@command{bashtop}.")
    (license license:asl2.0)))

(define-public pies
  (package
    (name "pies")
    (version "1.8")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnu/pies/pies-"
                           version ".tar.bz2"))
       (sha256
        (base32
         "0v0xcq0mfil440xq2pa5mjkyva5c9ahqda54z5w2ksl2d78v8a35"))
       (snippet
        #~(begin
            (use-modules (guix build utils))
            (for-each delete-file
                      (append
                        ;; Generated by flex.
                        (find-files "gres/src" "lex\\.c$")
                        ;; Generated by bison.
                        (find-files "gres/src" "-gram\\.[ch]$")))))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (add-before 'build 'patch-/bin/sh
                    (lambda* (#:key inputs #:allow-other-keys)
                      ;; Use the right shell when executing user-provided
                      ;; shell commands.
                      (let ((bash (assoc-ref inputs "bash")))
                        (substitute* '("src/progman.c" "src/comp.c")
                          (("\"/bin/sh\"")
                           (string-append "\"" bash "/bin/sh\"")))))))))
    (native-inputs (list bison flex))
    (inputs (list libxcrypt))
    (home-page "https://www.gnu.org.ua/software/pies/")
    (synopsis "Program invocation and execution supervisor")
    (description
     "GNU pies is a program that supervises the invocation and execution of
other programs.  It reads the list of programs to be started from its
configuration file, executes them, and then monitors their status,
re-executing them as necessary.")
    (license license:gpl3+)))

(define-public inetutils
  (package
    (name "inetutils")
    (version "2.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/inetutils/inetutils-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0q1257ci22g2jbdiqs00mharc1lqkbibdlkhj23f3si6qjxkn17s"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list "--localstatedir=/var"

              ;; Make sure 'PATH_PROCNET_DEV' gets defined when
              ;; cross-compiling (by default it does not.)
              #$@(if (%current-target-system)
                     '("--with-path-procnet-dev=/proc/net/dev")
                     '())
              #$@(if (target-hurd?)
                     '("--disable-rcp"
                       "--disable-rexec"
                       "--disable-rexecd"
                       "--disable-rlogin"
                       "--disable-rlogind"
                       "--disable-rsh"
                       "--disable-rshd"
                       "--disable-uucpd"
                       "--disable-whois")
                     '()))
      ;; Make sure that canonical "coreutils" package is not referred.
      #:make-flags
      #~(list (string-append "CPPFLAGS=-DPATHDEF_CP=\\\""
                             (search-input-file %build-inputs "bin/cp")
                             "\\\""))
      ;; On some systems, 'libls.sh' may fail with an error such as:
      ;; "Failed to tell switch -a apart from -A".
      #:parallel-tests? #f
      #:phases (if (target-hurd64?)
                   #~(modify-phases %standard-phases
                       (add-after 'unpack 'apply-hurd64-patch
                         (lambda _
                           (let ((patch
                                  #$(local-file
                                     (search-patch
                                      "inetutils-hurd64.patch"))))
                             (invoke "patch" "--force" "-p1" "-i" patch)))))
                   #~%standard-phases)))
    (inputs
     (list coreutils
           shadow                     ;for login (used in telnetd and rlogind)
           libxcrypt
           ncurses
           readline))                   ;for 'ftp'
    (native-inputs
     (if (member (%current-system)
                 (package-supported-systems net-tools))
         (list net-tools)               ;for tests
         '()))
    (home-page "https://www.gnu.org/software/inetutils/")
    (synopsis "Basic networking utilities")
    (description
     "Inetutils is a collection of common network programs, such as an ftp
client and server, a telnet client and server, an rsh client and server, and
hostname.")
    (license license:gpl3+)))

(define-public shadow
  (package
    (name "shadow")
    (version "4.13")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/shadow-maint/shadow/releases/"
                    "download/" version "/shadow-" version ".tar.xz"))
              (sha256
               (base32
                "0b6xz415b4y3y5nk3pw9xibv05kln4cjbmhybyncmrx2g5fj9zls"))))
    (build-system gnu-build-system)
    (arguments
     `(;; Assume System V `setpgrp (void)', which is the default on GNU
       ;; variants (`AC_FUNC_SETPGRP' is not cross-compilation capable.)
       #:configure-flags
       '(,@(if (target-hurd?)
             '()
             '("--with-libpam"))
          "shadow_cv_logdir=/var/log"
          "ac_cv_func_setpgrp_void=yes")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-linking-to-pam
           (lambda _
             ;; There's a build system problem in 4.9 that causes link
             ;; failures with the pam libraries (see:
             ;; https://github.com/shadow-maint/shadow/issues/407).
             (substitute* "libsubid/Makefile.in"
               (("\\$\\(LIBTCB\\)" all)
                (string-append all " $(LIBPAM)")))))
         ,@(if (%current-target-system)
               '((add-before 'configure 'set-runtime-shell
                   (lambda* (#:key inputs #:allow-other-keys)
                     (let ((shell (search-input-file inputs "/bin/bash")))
                       (setenv "RUNTIME_SHELL" shell)
                       (substitute* "configure.ac"
                         (("\\$SHELL")
                          "$RUNTIME_SHELL"))))))
               '())
         (add-before 'build 'set-nscd-file-name
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Use the right file name for nscd.
             (let ((libc (assoc-ref inputs
                                    ,(if (%current-target-system)
                                         "cross-libc"
                                         "libc"))))
               (substitute* "lib/nscd.c"
                 (("/usr/sbin/nscd")
                  (string-append libc "/sbin/nscd"))))))
         (add-after 'install 'install-man-pages
           (lambda _
             ;; The top-level Makefile.am wrongfully has "SUBDIRS += man"
             ;; under "if ENABLE_REGENERATE_MAN", even though prebuilt man
             ;; pages are available.  Thus, install them manually.
             (invoke "make" "-C" "man" "install")))
         (add-after 'install-man-pages 'remove-groups
           (lambda* (#:key outputs #:allow-other-keys)
             ;; Remove `groups', which is already provided by Coreutils.
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (man (string-append out "/share/man")))
               (delete-file (string-append bin "/groups"))
               (for-each delete-file (find-files man "^groups\\."))))))))
    (inputs
     (append (if (target-hurd?)
                 '()
                 (list linux-pam))
             (if (%current-target-system)
                 (list bash-minimal)
                 '())
             (list libxcrypt)))
    (home-page "https://github.com/shadow-maint/shadow")
    (synopsis "Authentication-related tools such as passwd, su, and login")
    (description
     "Shadow provides a number of authentication-related tools, including:
login, passwd, su, groupadd, and useradd.")

    ;; The `vipw' program is GPLv2+.
    ;; libmisc/salt.c is public domain.
    (license license:bsd-3)))

(define-public mingetty
  (package
    (name "mingetty")
    (version "1.08")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://sourceforge/mingetty/mingetty/"
                                 version "/mingetty-" version ".tar.gz"))
             (sha256
              (base32
               "05yxrp44ky2kg6qknk1ih0kvwkgbn9fbz77r3vci7agslh5wjm8g"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key inputs outputs target #:allow-other-keys)
             (let* ((out    (assoc-ref outputs "out"))
                    (man8   (string-append out "/share/man/man8"))
                    (sbin   (string-append out "/sbin"))
                    (shadow (assoc-ref inputs "shadow"))
                    (login  (string-append shadow "/bin/login")))
               (substitute* "Makefile"
                 ,@(if (%current-target-system)
                       '((("CC=.*$")
                          (string-append "CC=" target "-gcc\n")))
                       '())
                 (("^SBINDIR.*")
                  (string-append "SBINDIR = " out
                                 "/sbin\n"))
                 (("^MANDIR.*")
                  (string-append "MANDIR = " out
                                 "/share/man/man8\n")))

               ;; Pick the right 'login' by default.
               (substitute* "mingetty.c"
                 (("\"/bin/login\"")
                  (string-append "\"" login "\"")))

               (mkdir-p sbin)
               (mkdir-p man8))
             #t)))
       #:tests? #f))                              ; no tests
    (inputs (list shadow))

    (home-page "https://sourceforge.net/projects/mingetty")
    (synopsis "Getty for the text console")
    (description
     "Small console getty that is started on the Linux text console,
asks for a login name and then transfers over to @code{login}.  It is extended
to allow automatic login and starting any app.")
    (license license:gpl2+)))

(define-public net-base
  (package
    (name "net-base")
    (version "5.3")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "mirror://debian/pool/main/n/netbase/netbase_"
                   version ".tar.xz"))
             (sha256
              (base32
               "12xqjwg3p4rzmmh2iib6sigm9l29y3dgk74mmnw64k84jnbwdxl1"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       ;; This package consists solely of architecture-independent
       ;; tables. Cross-compilation is pointless! Make sure we'll
       ;; always get the same derivation.
       #:target #f
       #:allowed-references ()
       #:builder (begin
                   (use-modules (guix build utils)
                                (srfi srfi-26))

                   (let* ((source (assoc-ref %build-inputs "source"))
                          (tar    (assoc-ref %build-inputs "tar"))
                          (xz     (assoc-ref %build-inputs "xz"))
                          (output (assoc-ref %outputs "out"))
                          (etc    (string-append output "/etc")))
                     (setenv "PATH" (string-append xz "/bin"))
                     (invoke (string-append tar "/bin/tar") "xvf"
                             source)
                     (chdir ,(string-append "netbase-" version))
                     (mkdir-p etc)
                     (for-each copy-file
                               '("etc-services" "etc-protocols" "etc-rpc")
                               (map (cut string-append etc "/" <>)
                                    '("services" "protocols" "rpc")))
                     #t))))
    (native-inputs (list tar xz))
    (synopsis "IANA protocol, port, and RPC number assignments")
    (description
     "This package provides the /etc/services, /etc/protocols, and /etc/rpc
files, which contain information about the IANA-assigned port, protocol, and
ONC RPC numbers.")
    (home-page "https://packages.debian.org/sid/netbase")
    (license license:gpl2)))

(define-public netcat
  (package
    (name "netcat")
    (version "0.7.1")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://sourceforge/netcat/netcat/" version
                                 "/netcat-" version ".tar.bz2"))
             (sha256
              (base32
               "1frjcdkhkpzk0f84hx6hmw5l0ynpmji8vcbaxg8h5k2svyxz0nmm"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       ;; By default, man and info pages are put in PREFIX/{man,info},
       ;; but we want them in PREFIX/share/{man,info}.
       (let ((out (assoc-ref %outputs "out")))
         (list (string-append "--mandir=" out "/share/man")
               (string-append "--infodir=" out "/share/info")))))
    (home-page "https://netcat.sourceforge.net")
    (synopsis "Read and write data over TCP/IP")
    (description
     "Netcat is a featured networking utility which reads and writes data
across network connections, using the TCP/IP protocol.  It is designed to be a
reliable \"back-end\" tool that can be used directly or easily driven by other
programs and scripts.  At the same time, it is a feature-rich network debugging
and exploration tool, since it can create almost any kind of connection you
would need and has several interesting built-in capabilities.")
    (license license:gpl2+)))

(define-public netcat-openbsd
  (package
    (name "netcat-openbsd")
    (version "1.219-1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://salsa.debian.org/debian/netcat-openbsd.git")
                    (commit (string-append "debian/" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1fhrmnbdl6bgsjk02vi78zy9i486mmniymbbbhdkzl8zfjbjkpxc"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no test suite
       #:make-flags
       (list (string-append "CC=" ,(cc-for-target)))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'build 'patch
           (lambda _
             (setenv "QUILT_PATCHES" "debian/patches")
             (invoke "quilt" "push" "-a")))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (man (string-append out "/share/man/man1"))
                    (doc (string-append out "/share/doc/netcat-openbsd-" ,version))
                    (examples (string-append doc "/examples")))
               (install-file "nc" bin)
               (install-file "nc.1" man)
               (install-file "debian/copyright" doc)
               (copy-recursively "debian/examples" examples)))))))
    (inputs (list libbsd))
    (native-inputs (list pkg-config quilt))
    (home-page "https://packages.debian.org/sid/netcat-openbsd")
    (synopsis "Read and write data over TCP/IP")
    (description
     "Netcat is a simple Unix utility which reads and writes data across
network connections using TCP or UDP protocol.  It is designed to be a reliable
\"back-end\" tool that can be used directly or easily driven by other programs
and scripts.  At the same time it is a feature-rich network debugging and
exploration tool, since it can create almost any kind of connection you would
need and has several interesting built-in capabilities.

This package contains the OpenBSD rewrite of netcat, including support for
IPv6, proxies, and Unix sockets.")
    (license (list license:bsd-3
                   license:bsd-2))))  ; atomicio.*, socks.c

(define-public nmon
  (package
    (name "nmon")
    (version "16p")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/nmon/lmon" version ".c"))
       (sha256
        (base32 "0akbkv70zffdmc5p51r02rlxd8b3jvkgl64rjsd29qr5cxgh9ijx"))))
    (build-system gnu-build-system)
    (arguments
     (list #:tests? #f                  ; no test suite
           #:phases
           #~(modify-phases %standard-phases
               (replace 'unpack
                 (lambda _
                   (copy-file #$(package-source this-package) "lmon.c")))
               (delete 'configure)      ; no build system
               (replace 'build
                 ;; There is an example ‘Makefile’ in the .c file.
                 (lambda _
                   ;; These #defines aren't well-documented and, e.g., POWER was
                   ;; not actually tested on every possible TARGET-POWERPC?.
                   (let* ((system #$(cond ((target-x86?) "X86")
                                          ((target-arm?) "ARM")
                                          ((target-powerpc?) "POWER")
                                          (else "CROSS_FINGERS"))))
                     (format #t "Building for ~a~%" system)
                     (invoke #$(cc-for-target) "-o" "nmon" "lmon.c"
                             "-g" "-Wall" "-D" system
                             "-lncurses" "-lm"))))
               (replace 'install
                 (lambda _
                   (let ((bin (string-append #$output "/bin"))
                         (man1 (string-append #$output "/share/man/man1")))
                     (install-file "nmon" bin)
                     (mkdir-p man1)
                     (copy-file #$(this-package-native-input "man-page")
                                (string-append man1 "/nmon.1"))))))))
    (native-inputs
     (list `("man-page"
             ,(origin
                ;; There is no man page upstream, so install Debian's.
                (method url-fetch)
                (uri (string-append "https://salsa.debian.org/carnil/nmon/"
                                    "-/raw/debian/" version "+debian-1/"
                                    "debian/nmon.1"))
                (sha256
                 (base32
                  "1gpvd2kjyhs18sh6sga5bk9wj8s78blfd4c0m38r0wl92jx2yv1b"))))))
    (inputs
     (list ncurses))
    (home-page "https://nmon.sourceforge.net/")
    (synopsis
     "Monitor system performance in a terminal or to a @file{.csv} log file")
    (description
     "@acronym{Nmon, Nigel's performance monitor} is yet another system monitor
useful in systems administration, debugging, tuning, and benchmarking.

The configurable ncurses interface displays all the classic resource usage
statistics (CPU, memory, network, disk, ...) as real-time graphs or numbers.
It can also list the processes responsible in a @command{top}-like table.

A less common nmon feature is its ability to create highly detailed log files
in @acronym{CSV, comma-separated values} format.  These can be imported into
spreadsheets or fed straight into an @acronym{RRD, round-robin database} using
@command{rrdtool} for further analysis, or to create colourful graphs.")
    (license license:gpl3+)))

(define-public sipcalc
  (package
    (name "sipcalc")
    (version "1.1.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://www.routemeister.net/projects"
                           "/sipcalc/files/sipcalc" "-" version ".tar.gz"))
       (sha256
        (base32
         "0mv3wndj4z2bsshh2k8d5sy3j8wxzgf8mzmmkvj1k8gpcz37dm6g"))))
    (build-system gnu-build-system)
    (home-page "https://www.routemeister.net/projects/sipcalc/")
    (synopsis "Command-line IP subnet calculator")
    (description
     "Sipcalc is an advanced command-line IP subnet calculator.  It can take
multiple forms of input (IPv4/IPv6/interface/hostname) and output a multitude
of information about a given subnet.

Features include:

@itemize @bullet
@item IPv4
@itemize
@item Retrieving of address information from interfaces.
@item Classfull and CIDR output.
@item Multiple address and netmask input and output formats (dotted quad, hex,
number of bits).
@item Output of broadcast address, network class, Cisco wildcard,
hosts/range, network range.
@item The ability to split a network based on a smaller netmask, now also with
recursive runs on the generated subnets.  (also IPv6)
@end itemize
@item IPv6
@itemize
@item Compressed and expanded input and output addresses.
@item Standard IPv6 network output.
@item v4 in v6 output.
@item Reverse DNS address generation.
@end itemize
@end itemize\n")
    (license license:bsd-3)))

(define-public prips
  (package
    (name "prips")
    (version "1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://devel.ringlet.net/files/sys/"
                           name "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "1a33vbl4w603mk6mm5r3vhk87fy3dfk5wdpch0yd3ncbkg3fmvqn"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags (list (string-append "CC=" ,(cc-for-target)))
       #:test-target "test"
       #:phases (modify-phases %standard-phases
                  (delete 'configure)
                  (replace 'install
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out")))
                        (install-file "prips"
                                      (string-append out "/bin"))))))))
    (native-inputs (list perl-test-harness))
    (synopsis "Tool that prints the IP addresses in a given range")
    (description "Prips can be used to print all of the IP addresses in
 a given range.  This allows the enhancement of tools only work
 on one host at a time (e.g. whois).")
    (home-page "https://devel.ringlet.net/sysutils/prips/")
    (license license:gpl2+)))

(define-public alive
  (package
    (name "alive")
    (version "2.0.5")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/alive/alive-"
                                 version ".tar.lz"))
             (sha256
              (base32
               "12ahlxbbrynm6md8qc872qr795lqpfkr8kwlsig40i4nznzkvkwl"))))
    (build-system gnu-build-system)
    (arguments '(#:configure-flags '("alive_cv_nice_ping=yes")))
    (inputs
     (list guile-3.0
           inetutils))
    (native-inputs
     (list lzip))
    (home-page "https://www.gnu.org/software/alive/")
    (synopsis "Autologin and keep-alive daemon")
    (description
     "GNU Alive sends periodic pings to a server, generally to keep a
connection alive.")
    (license license:gpl3+)))

(define-public isc-dhcp
  (let* ((bind-major-version "9")
         (bind-minor-version "11")
         (bind-patch-version "37")
         (bind-release-type "")         ; for patch release, use "-P"
         (bind-release-version "")      ; for patch release, e.g. "6"
         (bind-version (string-append bind-major-version
                                      "."
                                      bind-minor-version
                                      "."
                                      bind-patch-version
                                      bind-release-type
                                      bind-release-version)))
    (package
      (name "isc-dhcp")
      (version "4.4.3-P1")
      (source (origin
                (method url-fetch)
                (uri (string-append "https://ftp.isc.org/isc/dhcp/"
                                    version "/dhcp-" version ".tar.gz"))
                (sha256
                 (base32
                  "1ivkvhhvqxap6c51cli7pa6xn76ngxri1zbl45ishz4ranxidi0a"))
                (patches (search-patches
                           "dhclient-script-resolvconf-support.patch"))))
      (build-system gnu-build-system)
      (arguments
       `(#:parallel-build? #f
         #:configure-flags '("--with-randomdev=/dev/random")
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'replace-bundled-bind
             (lambda* (#:key inputs native-inputs #:allow-other-keys)
               (delete-file "bind/bind.tar.gz")
               (copy-file (assoc-ref inputs "bind-source-tarball")
                          "bind/bind.tar.gz")
               (chmod "bind/bind.tar.gz" #o644)
               (substitute* "bind/version.tmp"
                 (("^MAJORVER=.*")
                  (format #f "MAJORVER=~a\n" ,bind-major-version))
                 (("^MINORVER=.*")
                  (format #f "MINORVER=~a\n" ,bind-minor-version))
                 (("^PATCHVER=.*")
                  (format #f "PATCHVER=~a\n" ,bind-patch-version))
                 (("^RELEASETYPE=.*")
                  (format #f "RELEASETYPE=~a\n" ,bind-release-type))
                 (("^RELEASEVER=.*")
                  (format #f "RELEASEVER=~a\n" ,bind-release-version)))))
           ,@(if (%current-target-system)
                 `((add-before 'configure 'fix-bind-cross-compilation
                     (lambda _
                       (substitute* "configure"
                         (("--host=\\$host")
                          "--host=$host_alias --build=$build_alias"))
                       ;; BIND needs a native compiler because the DHCP
                       ;; build system uses the built 'gen' executable.
                       (setenv "BUILD_CC" "gcc")
                       ;; powerpc-linux needs to be told to use -latomic.
                       ,@(if (target-ppc32?)
                           `((setenv "LIBS" "-latomic"))
                           '()))))
                 '())
           (add-before 'configure 'update-config-scripts
             (lambda* (#:key native-inputs inputs #:allow-other-keys)
               (for-each (lambda (file)
                               (install-file
                                 (search-input-file
                                   (or native-inputs inputs)
                                   (string-append "/bin/" file)) "."))
                         '("config.guess" "config.sub"))))
           (add-before 'build 'update-config-scripts-for-bind
             (lambda* (#:key native-inputs inputs #:allow-other-keys)
               (for-each (lambda (file)
                               (install-file
                                 (search-input-file
                                   (or native-inputs inputs)
                                   (string-append "/bin/" file))
                                 (string-append "bind/bind-" ,bind-version)))
                         '("config.guess" "config.sub"))))
           (add-after 'configure 'post-configure
             (lambda* (#:key outputs #:allow-other-keys)
               ;; Point to the right client script, which will be
               ;; installed in a later phase.
               (substitute* "includes/dhcpd.h"
                 (("#define[[:blank:]]+_PATH_DHCLIENT_SCRIPT.*")
                  (let ((out (assoc-ref outputs "out")))
                    (string-append "#define _PATH_DHCLIENT_SCRIPT \""
                                   out "/libexec/dhclient-script"
                                   "\"\n"))))

               ;; During the 'build' phase, 'bind.tar.gz' is extracted, so
               ;; we must patch shebangs in there and make sure the right
               ;; shell is used.
               (with-directory-excursion "bind"
                 (substitute* "Makefile"
                   (("\\./configure ")
                    (let ((sh (which "sh")))
                      (string-append "./configure CONFIG_SHELL="
                                     sh " SHELL=" sh " "))))

                 (let ((bind-directory (string-append "bind-" ,bind-version)))
                   (invoke "tar" "xf" "bind.tar.gz")
                   (for-each patch-shebang
                             (find-files bind-directory ".*"))
                   (substitute* (string-append bind-directory "/configure")
                     (("/usr/bin/file")
                      (which "file")))
                   (invoke "tar" "cf" "bind.tar.gz"
                           bind-directory
                           ;; avoid non-determinism in the archive
                           "--sort=name"
                           "--mtime=@0"
                           "--owner=root:0"
                           "--group=root:0")))))
           (add-after 'install 'post-install
             (lambda* (#:key inputs outputs #:allow-other-keys)
               ;; Install the dhclient script for GNU/Linux and make sure
               ;; if finds all the programs it needs.
               (let* ((out        (assoc-ref outputs "out"))
                      (libexec    (string-append out "/libexec"))
                      (coreutils  (assoc-ref inputs "coreutils*"))
                      (inetutils  (assoc-ref inputs "inetutils"))
                      (grep       (assoc-ref inputs "grep*"))
                      (resolvconf (assoc-ref inputs "resolvconf*"))
                      (sed        (assoc-ref inputs "sed*"))
                      (debianutils (assoc-ref inputs "debianutils")))
                 (substitute* "client/scripts/linux"
                   (("/sbin/ip")
                    (search-input-file inputs "/sbin/ip")))

                 (mkdir-p libexec)
                 (copy-file "client/scripts/linux"
                            (string-append libexec "/dhclient-script"))

                 (wrap-program
                     (string-append libexec "/dhclient-script")
                   `("PATH" ":" prefix
                     ,(map (lambda (dir)
                             (string-append dir "/bin:"
                                            dir "/sbin"))
                           (list inetutils coreutils grep sed resolvconf
                                 debianutils))))))))))

      (native-inputs
       (list config perl file))

      (inputs `(("inetutils" ,inetutils)
                ("bash" ,bash-minimal)
                ,@(if (target-hurd?)
                      '()
                      `(("iproute" ,iproute)))

                ;; dhclient-script provides hooks to users and uses run-parts in
                ;; order to list users defined hooks.
                ("debianutils" ,debianutils)

                ;; isc-dhcp bundles a copy of BIND, which has proved vulnerable
                ;; in the past.  Use a BIND-VERSION of our choosing instead.
                ("bind-source-tarball"
                 ,(origin
                    (method url-fetch)
                    (uri (string-append "https://ftp.isc.org/isc/bind9/"
                                        bind-version
                                        "/bind-" bind-version ".tar.gz"))
                    (sha256
                     (base32
                      "1zsszgxs9043dfpxb6xs1iwk9jg7nxkl5pbawj8dlshnxkkzp3hd"))))

                ("coreutils*" ,coreutils)
                ("grep*" ,grep)
                ("resolvconf*" ,openresolv)
                ("sed*" ,sed)))

      (home-page "https://www.isc.org/dhcp/")
      (synopsis "Dynamic Host Configuration Protocol (DHCP) tools")
      (description
       "ISC's Dynamic Host Configuration Protocol (DHCP) distribution provides a
reference implementation of all aspects of DHCP, through a suite of DHCP
tools: server, client, and relay agent.

This software is @emph{end-of-life}!  ISC does not intend to issue any further
maintenance releases.")
      (license license:mpl2.0)
      (properties '((cpe-name . "dhcp"))))))

(define-public dhcpcd
  (package
    (name "dhcpcd")
    (version "10.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/NetworkConfiguration/dhcpcd")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "038ycdflmj4dyrvh5gy3qz5781hs5ks088a0f0k348rcqf5ibps4"))))
    (inputs (list bash-minimal coreutils-minimal eudev sed))
    (build-system gnu-build-system)
    (arguments
     (list
      #:test-target "test"
      #:configure-flags #~(list "--enable-ipv6"
                                "--enable-privsep"
                                "--privsepuser=dhcpcd"
                                (string-append "--dbdir=" "/var/db/dhcpcd")
                                (string-append "--rundir=" "/var/run/dhcpcd")
                                (string-append "CC=" #$(cc-for-target)))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'do-not-create-dbdir
            (lambda _
              ;; Make sure that the Makefile doesn't attempt to create
              ;; /var/db/dhcpcd for which it doesn't have permissions.
              (substitute* "src/Makefile"
                (("\\$\\{INSTALL\\} -m \\$\\{DBMODE\\} -d\
 \\$\\{DESTDIR\\}\\$\\{DBDIR\\}")
                 ""))))
          (add-before 'build 'setenv
            (lambda _
              (setenv "HOST_SH" (which "sh"))))
          (add-after 'install 'wrap-hooks
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((sed (search-input-file inputs "/bin/sed"))
                     (rm (search-input-file inputs "/bin/rm")))
                (wrap-program (string-append
                               #$output "/libexec/dhcpcd-run-hooks")
                  `("PATH" ":" suffix
                    (,(dirname sed)
                     ,(dirname rm))))))))))
    (home-page "https://roy.marples.name/projects/dhcpcd")
    (synopsis "Feature-rich DHCP and DHCPv6 client")
    (description
     "Provides a DHCP and a DHCPv6 client.  Additionally,
dhcpcd is also an IPv4LL (aka ZeroConf) client.  In layperson's terms,
dhcpcd runs on your machine and silently configures your computer to work
on the attached networks without trouble and mostly without configuration.")
    (license license:bsd-2)))

(define-public radvd
  (package
    (name "radvd")
    (version "2.20")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/radvd-project/radvd")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "090b8953cq7pvxf8i5wsippsi3zc8jxy559k6jpfjjmbbvl8zlmk"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf
           automake
           bison
           check
           flex
           pkg-config))
    (inputs (list libbsd))
    (arguments
     (if (%current-target-system)
         (list)
         (list #:configure-flags #~(list "--with-check"))))
    (home-page "https://radvd.litech.org/")
    (synopsis "IPv6 Router Advertisement Daemon")
    (description
     "The Router Advertisement Daemon (radvd) is run on systems acting as IPv6
routers.  It sends Router Advertisement messages specified by RFC 2461
periodically and when requested by a node sending a Router Solicitation
message.  These messages are required for IPv6 stateless autoconfiguration.")
    (license (license:non-copyleft "file://COPYRIGHT"))))

(define-public ndppd
  (package
    (name "ndppd")
    (version "0.2.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/DanielAdolfsson/ndppd")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0niri5q9qyyyw5lmjpxk19pv3v4srjvmvyd5k6ks99mvqczjx9c0"))))
    (build-system gnu-build-system)
    (arguments
     (list #:tests? #f ; There are no tests
           #:make-flags #~(list (string-append "PREFIX=" #$output))
           #:phases
           #~(modify-phases %standard-phases
               (delete 'configure)
               (add-after 'unpack 'fix-paths
                 (lambda _
                   (substitute* "Makefile"
                     (("/bin/gzip") "gzip")))))))
    (synopsis "NDP Proxy Daemon")
    (description
     "The Neighbor Discovery Protocol Proxy Daemon (ndppd) proxies some IPv6
NDP messages between interfaces to allow IPv6 routing between machines that
are in the same network but not on the same local link.  It currently only
supports Neighbor Solicitation and Neighbor Advertisement messages.")
    (home-page "https://github.com/DanielAdolfsson/ndppd")
    (license license:gpl3+)))

(define-public libpcap
  (package
    (name "libpcap")
    (version "1.10.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.tcpdump.org/release/libpcap-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1m5x26vlbymp90k1qh0w3nj2nxzyvfrmfmwpj17k81dgri55ya7d"))))
    (build-system gnu-build-system)
    (native-inputs
     (list bison flex))
    (arguments
     ;; There are some tests in testprogs/, but no automated test suite.
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'omit-static-library
           ;; Neither build nor install libpcap.a.
           (lambda _
             (substitute* "Makefile.in"
               ((" libpcap\\.a") "")
               ((" install-archive ") " ")))))))
    (home-page "https://www.tcpdump.org")
    (synopsis "Network packet capture library")
    (description
     "libpcap is an interface for user-level packet capture.  It provides a
portable framework for low-level network monitoring.  Applications include
network statistics collection, security monitoring, network debugging, etc.")
    (license (list license:bsd-4        ; fad-*.c and several other source files
                   license:bsd-3        ; pcap/, sockutils.* & others
                   license:bsd-2))))    ; the rest

(define-public tcpdump
  (package
    (name "tcpdump")
    (version "4.99.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.tcpdump.org/release/tcpdump-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1slzwjk5f8sygwxqci4vkbas0qqcgs5a0w3f8br6p7gjn8dj6ch2"))))
    (build-system gnu-build-system)
    (inputs (list libpcap openssl))
    (native-inputs (list perl))        ; for tests
    (home-page "https://www.tcpdump.org/")
    (synopsis "Network packet analyzer")
    (description
     "Tcpdump is a command-line tool to analyze network traffic passing
through the network interface controller.")
    (license license:bsd-3)))

(define-public jnettop
  (package
    (name "jnettop")
    (version "0.13.0")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "https://web.archive.org/web/20161221100811/"
                              "http://jnettop.kubs.info/dist/jnettop-"
                              version ".tar.gz"))
              (sha256
               (base32
                "1855np7c4b0bqzhf1l1dyzxb90fpnvrirdisajhci5am6als31z9"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list glib ncurses libpcap))
    (home-page
     "https://web.archive.org/web/20160703195221/http://jnettop.kubs.info/wiki/")
    (synopsis "Visualize network traffic by bandwidth use")
    (description
     "Jnettop is a traffic visualiser, which captures traffic going
through the host it is running from and displays streams sorted
by bandwidth they use.")
    (license license:gpl2+)))

(define-public clusterssh
  (package
    (name "clusterssh")
    (version "4.13.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/clusterssh/"
                                  "2.%20ClusterSSH%20Series%204/"
                                  "App-ClusterSSH-v" version ".tar.gz"))
              (sha256
               (base32
                "0rmk2p3f2wz1h092anidjclh212rv3gxyk0c641qk3frlrjnw6mp"))))
    (build-system perl-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'refer-to-inputs
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* (list "lib/App/ClusterSSH/Config.pm"
                                "t/15config.t")
               (("xterm")
                (which "xterm")))))
         (add-before 'check 'delete-failing-tests
           (lambda _
             ;; This checks whether all code is nicely formatted.  The above
             ;; ‘refer-to-inputs’ phase breaks this pedantry, so disable it.
             (delete-file "t/perltidy.t")
             ;; Update the manifest so t/manifest.t happily passes.
             (substitute* "MANIFEST"
               (("t/perltidy.t\n") ""))))
         (add-after 'install 'augment-library-path
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (with-directory-excursion bin
                 (for-each
                  (lambda (program)
                    (wrap-program program
                      `("PERL5LIB" ":" prefix
                        ,(map (lambda (file-name)
                                (string-append file-name
                                               "/lib/perl5/site_perl"))
                              (cons out
                                    (map (lambda (input)
                                           (assoc-ref inputs input))
                                         ;; These may be propagated and hence
                                         ;; not explicitly listed as inputs.
                                         (list "perl-class-data-inheritable"
                                               "perl-devel-stacktrace"
                                               "perl-exception-class"
                                               "perl-tk"
                                               "perl-try-tiny"
                                               "perl-x11-protocol"
                                               "perl-x11-protocol-other")))))))
                  (find-files "." ".*")))))))))
    (native-inputs
     (list perl-cpan-changes
           perl-file-slurp
           perl-file-which
           perl-module-build
           perl-readonly
           perl-test-differences
           perl-test-distmanifest
           perl-test-perltidy
           perl-test-pod
           perl-test-pod-coverage
           perl-test-trap
           perltidy))
    (inputs
     (list bash-minimal                 ;for wrap-program
           perl-exception-class
           perl-sort-naturally
           perl-tk
           perl-try-tiny
           perl-x11-protocol
           perl-x11-protocol-other
           xterm))
    ;; The clusterssh.sourceforge.net address requires login to view
    (home-page "https://sourceforge.net/projects/clusterssh/")
    (synopsis "Secure concurrent multi-server terminal control")
    (description
     "ClusterSSH controls a number of xterm windows via a single graphical
console window to allow commands to be interactively run on multiple servers
over ssh connections.")
    (license license:gpl2+)))

(define-public realmd
  (package
    (name "realmd")
    (version "0.17.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/freedesktop/realmd")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "063cf4jkpfj548a7dxmffrpbh3j413nq3zy1zzj20lcfffnnaqwn"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--with-systemd-unit-dir=no"
                           "--with-systemd-journal=no"
                           "--with-distro=GNU guix"
                           "--disable-doc")
       #:phases
       (modify-phases %standard-phases
         (add-before 'install 'fix-service
           (lambda* (#:key outputs #:allow-other-keys)
             ;; GNU Guix does not have service config file, therefore we remove
             ;; the line that copies the file.
             (substitute* "Makefile"
               ((".*/service/realmd-.*") "")))))))
    (native-inputs
     (list autoconf
           automake
           `(,glib "bin")
           intltool
           pkg-config
           python))
    (inputs
     (list glib mit-krb5 openldap polkit))
    (synopsis "DBus service for network authentication")
    (description "This package provides an on demand system DBus service.
It allows callers to configure network authentication and domain membership
in a standard way.  Realmd discovers information about the domain or realm
automatically and does not require complicated configuration in order to join
a domain or realm.  Dbus system service that manages discovery and enrollment in
realms/domains like Active Directory or IPA.")
    (home-page "https://www.freedesktop.org/software/realmd/")
    (license license:lgpl2.1+)))

(define-public rename
  (package
    (name "rename")
    (version "2.02")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://cpan/authors/id/R/RM/RMBARKER/File-Rename-"
                    version ".tar.gz"))
              (sha256
               (base32
                "1pr0qmsb9gb5xqwpicabnr5jcxdbbvz1mgdqw3d3d0dn12060ysk"))))
    (build-system perl-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'find-itself
           ;; Fix run-time 'Can't locate File/Rename.pm in @INC' failure.
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (with-directory-excursion bin
                 (for-each
                  (lambda (program)
                    (wrap-program program
                      `("PERL5LIB" ":" prefix
                        (,(string-append out "/lib/perl5/site_perl")))))
                  (find-files "." ".*")))))))))
    (native-inputs
     (list perl-module-build perl-test-pod perl-test-pod-coverage))
    (inputs (list bash-minimal))        ;for wrap-program
    (home-page "https://metacpan.org/pod/distribution/File-Rename/rename.PL")
    (synopsis "Perl extension for renaming multiple files")
    (description
     "This package provides a Perl interface (@code{Perl::Rename}) as well
as a command-line utility (@command{rename}) that can rename multiple files
at once based on a Perl regular expression.")
    (license license:perl-license)))

(define-public rottlog
  (package
    (name "rottlog")
    (version "0.72.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/rottlog/rottlog-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0751mb9l2f0jrk3vj6q8ilanifd121dliwk0c34g8k0dlzsv3kd7"))
              (modules '((guix build utils)))
              (patches (search-patches "rottlog-direntry.patch"))
              (snippet
               '(begin
                  ;; Delete outdated Autotools build system files.
                  (for-each delete-file
                            (list "Makefile.in"
                                  "config.guess"
                                  "config.sub"
                                  "configure"
                                  "depcomp"
                                  "install-sh"
                                  "mdate-sh"
                                  "missing"
                                  "mkinstalldirs"
                                  "texinfo.tex"))
                  (substitute* "Makefile.am"
                    (("mkdir -p \\$\\(ROTT_STATDIR\\)")
                     ;; Don't attempt to create /var/lib/rottlog.
                     "true"))))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:configure-flags #~(list "ROTT_ETCDIR=/etc/rottlog" ;rc file location
                                "--localstatedir=/var")
      ;; Install example config files in OUT/etc.
      #:make-flags #~(list (string-append "ROTT_ETCDIR=" #$output "/etc")
                           ;; Avoid the default -o root -g root arguments,
                           ;; which fail due to not running as root.
                           "INSTALL_RC=install"
                           "INSTALL_SCRIPT=install")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-paths
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "rc/rc"
                (("/usr/sbin/sendmail")
                 (search-input-file inputs "/bin/mail")))
              (with-fluids ((%default-port-encoding "ISO-8859-1"))
                (substitute* "src/rottlog"
                  (("awk")
                   (search-input-file inputs "/bin/awk"))))))
          (add-after 'build 'set-packdir
            (lambda _
              ;; Set a default location for archived logs.
              (substitute* "rc/rc"
                (("packdir=\"\"")
                 "packdir=\"/var/log\""))))
          (add-before 'install 'tweak-rc-weekly
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "rc/weekly"
                (("/bin/kill")
                 (search-input-file inputs "/bin/kill"))
                (("syslogd\\.pid")
                 ;; The file is called 'syslog.pid' (no 'd').
                 "syslog.pid"))))
          (add-after 'install 'install-info
            (lambda _
              (invoke "make" "install-info"))))))
    (native-inputs (list autoconf automake texinfo util-linux)) ; for 'cal'
    (inputs (list coreutils gawk mailutils))
    (home-page "https://www.gnu.org/software/rottlog/")
    (synopsis "Log rotation and management")
    (description
     "GNU Rot[t]log is a program for managing log files.  It is used to
automatically rotate out log files when they have reached a given size or
according to a given schedule.  It can also be used to automatically compress
and archive such logs.  Rot[t]log will mail reports of its activity to the
system administrator.")
    (license license:gpl3+)))

(define-public sudo
  (package
    (name "sudo")
    (version "1.9.16")
    (source (origin
              (method url-fetch)
              (uri
               (list (string-append "https://www.sudo.ws/sudo/dist/sudo-"
                                    version ".tar.gz")
                     (string-append "ftp://ftp.sudo.ws/pub/sudo/OLD/sudo-"
                                    version ".tar.gz")))
              (sha256
               (base32
                "0gd0pyycc3jnbgq5f8056fyc4a1ix257j2rxazy35dq6gxwlvn60"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (delete-file-recursively "lib/zlib")))))
    (build-system gnu-build-system)
    (outputs (list "out"))
    (arguments
     (list #:configure-flags
           #~(list (string-append "--docdir=" #$output
                                  "/share/doc/" #$name "-" #$version)

                   "--with-logpath=/var/log/sudo.log"
                   "--with-rundir=/var/run/sudo" ; must be cleaned up at boot time
                   "--with-vardir=/var/db/sudo"
                   "--with-iologdir=/var/log/sudo-io"

                   ;; 'visudo.c' expects _PATH_MV to be defined, but glibc doesn't
                   ;; provide it.
                   (string-append "CPPFLAGS=-D_PATH_MV=\\\""
                                  (search-input-file %build-inputs "/bin/mv")
                                  "\\\"")

                   ;; When cross-compiling, assume we have a working 'snprintf' and
                   ;; 'vsnprintf' (which we do, when using glibc).  The default
                   ;; choice fails with undefined references to 'sudo_snprintf' &
                   ;; co. when linking.
                   #$@(if (%current-target-system)
                          '("ac_cv_have_working_snprintf=yes"
                            "ac_cv_have_working_vsnprintf=yes")
                          '()))

           ;; Avoid non-determinism; see <http://bugs.gnu.org/21918>.
           #:parallel-build? #f

           #:phases
           #~(modify-phases %standard-phases
               (add-before 'configure 'pre-configure
                 (lambda _
                   (substitute* "src/sudo_usage.h.in"
                     ;; Do not capture 'configure' arguments since we would
                     ;; unduly retain references, and also because the
                     ;; CPPFLAGS above would close the string literal
                     ;; prematurely.
                     (("@CONFIGURE_ARGS@")
                      "\"\""))
                   (substitute* (find-files "." "Makefile\\.in")
                     ;; Allow installation as non-root.
                     (("-o [[:graph:]]+ -g [[:graph:]]+")
                      "")
                     ;; Don't try to create /etc/sudoers.
                     (("^install: (.*)install-sudoers(.*)" _ before
                       after)
                      (string-append "install: " before after "\n"))
                     ;; Don't try to create /run/sudo.
                     (("\\$\\(DESTDIR\\)\\$\\(rundir\\)")
                      "$(TMPDIR)/dummy")
                     ;; Install example sudo{,_logsrvd}.conf to the right place.
                     (("\\$\\(DESTDIR\\)\\$\\(sysconfdir\\)")
                      "$(DESTDIR)/$(docdir)/examples")
                     ;; Don't try to create /var/db/sudo.
                     (("\\$\\(DESTDIR\\)\\$\\(vardir\\)")
                      "$(TMPDIR)/dummy"))

                   ;; ‘Checking existing [/etc/]sudoers file for syntax errors’ is
                   ;; not the task of the build system, and fails.
                   (substitute* "plugins/sudoers/Makefile.in"
                     (("^pre-install:" match)
                      (string-append match "\ndisabled-" match))))))

           ;; XXX: The 'testsudoers' test series expects user 'root' to exist, but
           ;; the chroot's /etc/passwd doesn't have it.  Turn off the tests.
           #:tests? #f))
    (native-inputs
     (list groff))
    (inputs
     (append (list coreutils zlib)
             (if (target-hurd?)
                 '()
                 (list linux-pam))))
    (home-page "https://www.sudo.ws/")
    (synopsis "Run commands as root")
    (description
     "Sudo (su \"do\") allows a system administrator to delegate authority to
give certain users (or groups of users) the ability to run some (or all)
commands as root or another user while providing an audit trail of the
commands and their arguments.")

    ;; See <http://www.sudo.ws/sudo/license.html>.
    (license license:x11)))

(define-public opendoas
  (package
    (name "opendoas")
    (version "6.8.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Duncaen/OpenDoas")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1qrin7x9vcprk5pwjbr3w8z2qj8hk6xbvxicdhlk27xr6vcr1qzn"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'pre-configure
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "GNUmakefile"
               (("^\tchown.*$") ""))
             ;; OpenDoas looks for binaries in safepath when a rule specifies a
             ;; relative command, such as “permit keepenv :wheel cmd guix”.
             (substitute* "doas.c"
               (("safepath =" match)
                (string-append match " \""
                               "/run/privileged/bin:"
                               "/run/current-system/profile/bin:"
                               "/run/current-system/profile/sbin:"
                               "\" ")))))
         (replace 'configure
           ;; The configure script doesn't accept most of the default flags.
           (lambda* (#:key configure-flags #:allow-other-keys)
             ;; The configure script can be told which compiler to use only
             ;; through environment variables.
             (setenv "CC" ,(cc-for-target))
             (apply invoke "./configure" configure-flags))))
       #:configure-flags
       (list (string-append "--prefix=" (assoc-ref %outputs "out"))
             "--with-timestamp")
       ;; Compiler choice is not carried over from the configure script.
       #:make-flags
       (list (string-append "CC=" ,(cc-for-target)))
       #:tests? #f))                 ; no test suite
    (native-inputs
     (list bison))
    (inputs
     (list libxcrypt))
    (home-page "https://github.com/Duncaen/OpenDoas")
    (synopsis "Portable version of OpenBSD's doas command")
    (description "Doas is a minimal replacement for the venerable sudo.  It was
initially written by Ted Unangst of the OpenBSD project to provide 95% of the
features of sudo with a fraction of the codebase.")
    (license (list license:bsd-3        ; libbsd/*
                   license:isc))))      ; everything else

(define-public wpa-supplicant-minimal
  (package
    (name "wpa-supplicant-minimal")
    (version "2.10")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://w1.fi/releases/wpa_supplicant-"
                    version ".tar.gz"))
              (sha256
               (base32
                "0bvvw7bx149a57llzrwzlpggyym84f8jdd4abwsk0f2b2pjpmpr0"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (substitute* "wpa_supplicant/defconfig"
                    ;; Disable D-Bus to save ~14MiB on the closure size.
                    (("^CONFIG_CTRL_IFACE_DBUS" line _)
                     (string-append "#" line)))
                  #t))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda _
             (chdir "wpa_supplicant")
             (copy-file "defconfig" ".config")
             (let ((port (open-file ".config" "al")))
               (display "
      CONFIG_DEBUG_SYSLOG=y

      CONFIG_TLS=openssl

      CONFIG_DRIVER_NL80211=y
      CFLAGS += $(shell pkg-config libnl-3.0 --cflags)
      CONFIG_LIBNL32=y
      CONFIG_READLINE=y\n" port)
               (close-port port))
             ;; Make sure we have a pkg-config when cross compiling
             (substitute* '(".config"
                            "Android.mk"
                            "Makefile"
                            "dbus/Makefile")
               (("pkg-config")
                (or (which "pkg-config")
                    (which (string-append ,(%current-target-system)
                                          "-pkg-config")))))
             #t))
         (add-after 'install 'install-documentation
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out  (assoc-ref outputs "out"))
                    (doc  (string-append out "/share/doc/wpa-supplicant"))
                    (man  (string-append out "/share/man"))
                    (man5 (string-append man "/man5"))
                    (man8 (string-append man "/man8")))
               (define (copy-man-page target)
                 (lambda (file)
                   (install-file file target)))

               (mkdir-p man5) (mkdir man8)
               (for-each (copy-man-page man5)
                         (find-files "doc/docbook" "\\.5"))
               (for-each (copy-man-page man8)
                         (find-files "doc/docbook" "\\.8"))

               ;; wpa_supplicant.conf(5) does not explain all configuration
               ;; options but refers to the example config file, so install it
               ;; along with READMEs.
               (for-each (lambda (file)
                           (install-file file doc))
                         '("README" "README-DPP" "README-HS20"
                           "README-P2P" "README-WPS"
                           "wpa_supplicant.conf"))
               #t))))

      #:make-flags (list (string-append "CC=" ,(cc-for-target))
                         (string-append "BINDIR=" (assoc-ref %outputs "out")
                                        "/sbin")
                         (string-append "LIBDIR=" (assoc-ref %outputs "out")
                                        "/lib"))
      #:tests? #f))
    (inputs
     (list readline libnl openssl))
    (native-inputs
     (list pkg-config))
    (home-page "https://w1.fi/wpa_supplicant/")
    (synopsis "Connecting to WPA and WPA2-protected wireless networks")
    (description
     "wpa_supplicant is a WPA Supplicant with support for WPA and WPA2 (IEEE
802.11i / RSN).  Supplicant is the IEEE 802.1X/WPA component that is used in
the client stations.  It implements key negotiation with a WPA Authenticator
and it controls the roaming and IEEE 802.11 authentication/association of the
WLAN driver.

This package provides the @code{wpa_supplicant} daemon and the @code{wpa_cli}
command.")

    ;; In practice, this is linked against Readline, which makes it GPLv3+.
    (license license:bsd-3)

    (properties `((cpe-name . "wpa_supplicant")))))

(define-public wpa-supplicant
  (package (inherit wpa-supplicant-minimal)
    (name "wpa-supplicant")
    (inputs (modify-inputs (package-inputs wpa-supplicant-minimal)
              (prepend dbus)))
    (source (origin
              (inherit (package-source wpa-supplicant-minimal))
              (patches (search-patches
                        "wpa-supplicant-dbus-group-policy.patch"))))
    (arguments
     (substitute-keyword-arguments (package-arguments wpa-supplicant-minimal)
       ((#:phases phases)
        `(modify-phases ,phases
           (add-after 'configure 'set-config-options
             (lambda _
               (let ((port (open-file ".config" "al")))
                 ;; Enable Opportunistic Wireless Encryption (OWE) and D-Bus
                 ;; support.
                 (display "
      CONFIG_OWE=y
      CONFIG_CTRL_IFACE_DBUS_NEW=y
      CONFIG_CTRL_IFACE_DBUS_INTRO=y\n" port)
                 (close-port port))))
          (add-after 'install-documentation 'install-dbus-conf
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (interfaces (string-append out "/etc/dbus-1/system.d"))
                     (services (string-append out
                                              "/share/dbus-1/system-services")))
                (mkdir-p interfaces)
                (copy-file "dbus/dbus-wpa_supplicant.conf"
                           (string-append interfaces "/wpa_supplicant.conf"))
                (mkdir-p services)
                (copy-file "dbus/fi.w1.wpa_supplicant1.service"
                           (string-append services
                                          "/fi.w1.wpa_supplicant1.service")))
              #t))))))))

(define-public wpa-supplicant-gui
  (package
    (inherit wpa-supplicant)
    (name "wpa-supplicant-gui")
    (inputs (modify-inputs (package-inputs wpa-supplicant)
              (prepend qtbase-5 qtsvg-5 qtwayland-5)))
    (native-inputs
     ;; For icons.
     (modify-inputs (package-native-inputs wpa-supplicant)
       (prepend imagemagick/stable
                inkscape/stable)))
    (build-system qt-build-system)
    (arguments
     (list
      ;; Make sure the (rarely updated) package 'imagemagick/stable'
      ;; does not end up in the closure.
      #:disallowed-references (list imagemagick/stable)
      #:test-target "check"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'chdir
            (lambda _ (chdir "wpa_supplicant/wpa_gui-qt4")))
          (replace 'configure
            (lambda _ (invoke "qmake" "wpa_gui.pro")))
          (add-after 'build 'build-icons
            (lambda _
              ;; Inkscape complains (but works) without a writable $HOME.
              (setenv "HOME" "/tmp")
              (invoke "make" "-C" "icons")))
          (replace 'install
            (lambda _
              (install-file "wpa_gui" (string-append #$output "/bin"))
              (install-file "wpa_gui.desktop"
                            (string-append #$output
                                           "/share/applications"))
              (copy-recursively "icons/hicolor"
                                (string-append #$output
                                               "/share/icons/hicolor")))))))
    (synopsis "Graphical user interface for WPA supplicant")))

(define-public hostapd
  (package
    (name "hostapd")
    (version "2.10")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://w1.fi/releases/hostapd-" version
                                  ".tar.gz"))
              (sha256
               (base32
                "0pcik0a6yin9nib02frjhaglmg44hwik086iwg1751b7kdwpqvi0"))))
    (build-system gnu-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (replace 'configure
                 (lambda _
                   ;; This is mostly copied from 'wpa-supplicant' above.
                   (chdir "hostapd")
                   (copy-file "defconfig" ".config")
                   (let ((port (open-file ".config" "al")))
                     (display "CONFIG_LIBNL32=y
                               CONFIG_IEEE80211R=y
                               CONFIG_IEEE80211N=y
                               CONFIG_IEEE80211AC=y
                               CONFIG_FULL_DYNAMIC_VLAN=y
                               CONFIG_ACS=y\n" port)
                     (close-port port))))
               (add-after 'unpack 'patch-pkg-config
                 (lambda _
                   (substitute* "src/drivers/drivers.mak"
                     (("pkg-config")
                      #$(pkg-config-for-target)))))
               (add-after 'install 'install-man-pages
                 (lambda* (#:key outputs #:allow-other-keys)
                   (let* ((man  (string-append #$output "/share/man"))
                          (man1 (string-append man "/man1"))
                          (man8 (string-append man "/man8")))
                     (define (copy-man-page target)
                       (lambda (file)
                         (install-file file target)))
                     (for-each (copy-man-page man1)
                               (find-files "." "\\.1"))
                     (for-each (copy-man-page man8)
                               (find-files "." "\\.8"))))))
           #:make-flags
           #~(list (string-append "CC=" #$(cc-for-target))
                   (string-append "BINDIR=" #$output "/sbin")
                   (string-append "LIBDIR=" #$output "/lib"))
           #:tests? #f))
    (native-inputs (list pkg-config))

    ;; There's an optional dependency on SQLite.
    (inputs (list openssl libnl))
    (home-page "https://w1.fi/hostapd/")
    (synopsis "Daemon for Wi-Fi access points and authentication servers")
    (description
     "hostapd is a user-space daemon for WiFi access points and authentication
servers.  It implements IEEE 802.11 access point management, IEEE
802.1X/WPA/WPA2/EAP Authenticators, RADIUS client, EAP server, and RADIUS
authentication server.")

    ;; Same license as wpa_supplicant.
    (license license:bsd-3)))

(define-public wakelan
  (package
    (name "wakelan")
    (version "1.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "ftp://ftp.gwdg.de/pub/linux/metalab/system/network/misc/wakelan-"
                    version ".tar.gz"))
              (sha256
               (base32
                "0vydqpf44146ir6k87gmqaq6xy66xhc1gkr3nsd7jj3nhy7ypx9x"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (mkdir-p (string-append out "/bin"))
               (mkdir-p (string-append out "/share/man/man1"))

               ;; It's an old configure script that doesn't understand
               ;; the extra options we pass.
               (setenv "CONFIG_SHELL" (which "bash"))
               (invoke "./configure"
                       (string-append "--prefix=" out)
                       (string-append "--mandir=" out
                                      "/share/man"))))))
       #:tests? #f))
    (home-page "https://www.kernel.org") ; really, no home page
    (synopsis "Send a wake-on-LAN packet")
    (description
     "WakeLan broadcasts a properly formatted UDP packet across the local area
network, which causes enabled computers to power on.")
    (license license:gpl2+)))

(define-public dmidecode
  (package
    (name "dmidecode")
    (version "3.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://savannah/dmidecode/dmidecode-"
                           version ".tar.xz"))
       (sha256
        (base32 "1blbvmsxba71fmjxckh0cn7x68kim7qlx6ilv0df7brxxkrna374"))))
    (build-system gnu-build-system)
    (arguments
     (list #:tests? #f                  ; no 'check' target
           #:make-flags
           #~(list (string-append "CC=" #$(cc-for-target))
                   (string-append "prefix=" #$output))
           #:phases
           #~(modify-phases %standard-phases
               (delete 'configure))))   ; no configure script
    (home-page "https://www.nongnu.org/dmidecode/")
    (synopsis "Read hardware information from the BIOS")
    (description
     "Dmidecode reports information about your system's hardware as described
in your system BIOS according to the SMBIOS/DMI standard.  This typically
includes system manufacturer, model name, serial number, BIOS version, asset
tag as well as a lot of other details of varying level of interest and
reliability depending on the manufacturer.  This will often include usage
status for the CPU sockets, expansion slots (e.g. AGP, PCI, ISA) and memory
module slots, and the list of I/O ports (e.g. serial, parallel, USB).")
    (license license:gpl2+)))

(define-public acpica
  (package
    (name "acpica")
    (version "20240827")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/acpica/acpica")
             (commit (string-append "version-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0dzrdhdgmmr6kqm95avvhr295kj8xi6iwm510lfwaylxzh34ln26"))))
    (build-system gnu-build-system)
    (native-inputs (list flex bison))
    (arguments
     (list
      #:make-flags
      #~(list (string-append "PREFIX=" #$output)
              (string-append "CC=" #$(cc-for-target))
              "HOST=_LINUX"
              "OPT_CFLAGS=-Wall -fno-strict-aliasing")
      #:tests? #f                       ;no test suite
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure))))        ;no configure script
    (home-page "https://acpica.org/")
    (synopsis "Tools for the development and debugging of ACPI tables")
    (description
     "The @acronym{ACPICA, ACPI Component Architecture} project provides an
OS-independent reference implementation of the @acronym{ACPI, Advanced
Configuration and Power Interface} specification.  ACPICA code contains those
portions of ACPI meant to be directly integrated into the host OS as a
kernel-resident subsystem, and a small set of tools to assist in developing and
debugging ACPI tables.

This package contains only the user-space tools needed for ACPI table
development, not the kernel implementation of ACPI.")
    (license license:gpl2)))            ;dual GPLv2/ACPICA Licence

(define-public s-tui
  (package
    (name "s-tui")
    (version "1.1.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "s-tui" version))
       (sha256
        (base32 "0mvvqg0pr8k0cy0mbvi25yqm6zsf8mipdbq97xjqfvifryf6j9wx"))))
    (build-system python-build-system)
    (inputs
     (list python-psutil python-urwid))
    (home-page "https://github.com/amanusk/s-tui")
    (synopsis "Interactive terminal stress test and monitoring tool")
    (description
     "The Stress Terminal UI displays graphs of the CPU frequency,
utilization, temperature and power.")
    (license license:gpl2+)))

(define-public stress
  (package
    (name "stress")
    (version "1.0.7")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://debian/pool/main/s/stress/stress_"
                                  version ".orig.tar.gz"))
              (sha256
               (base32
                "1cg0mklfrwfyzwqkzidd0151r8n2jgbiiqz1v0p3w4q62mkmdand"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake))
    (home-page "https://packages.debian.org/sid/stress")
    (synopsis "Impose load on and stress test a computer system")
    (description
     "Stress is a tool that imposes a configurable amount of CPU, memory, I/O,
or disk stress on a POSIX-compliant operating system and reports any errors it
detects.

Stress is not a benchmark.  It is a tool used by system administrators to
evaluate how well their systems will scale, by kernel programmers to evaluate
perceived performance characteristics, and by systems programmers to expose
the classes of bugs which only or more frequently manifest themselves when the
system is under heavy load.")
    (license license:gpl2+)))

(define-public stress-ng
  (package
    (name "stress-ng")
    (version "0.18.04")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ColinIanKing/stress-ng")
             (commit (string-append "V" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "100w4qkrzpg7jjl4dw0c376xi811qnjmlbffiy43i945f9vl3dc7"))))
    (build-system gnu-build-system)
    (arguments
     ;; XXX The test suite seems to cause instability on the VisionFive 2
     ;; build machines, maybe it's stressing them as intended but this is
     ;; unhelpful.
     (list #:tests? (and (not (%current-target-system))
                         (not (target-riscv64?)))
           #:make-flags
           #~(list (string-append "CC=" #$(cc-for-target))
                   (string-append "BINDIR=" #$output "/bin")
                   ;; XXX Really: MAN1DIR, or man pages won't be found.
                   (string-append "MANDIR=" #$output "/share/man/man1")
                   (string-append "JOBDIR=" #$output
                                  "/share/stress-ng/example-jobs")
                   (string-append "BASHDIR=" #$output
                                  "/share/bash-completion/completions"))
           ;; There is also a fast-test-all that very briefly runs every single
           ;; stressor, but its utility is questionable compared to its cost.
           ;; See this package's git history for details & which tests to skip.
           #:test-target "lite-test"
           #:phases
           #~(modify-phases %standard-phases
               (delete 'configure))))   ;no configure script
    (inputs
     (list keyutils
           kmod
           libaio
           libbsd
           libcap
           libgcrypt
           zlib))
    (home-page "https://github.com/ColinIanKing/stress-ng")
    (synopsis "Load and stress-test a computer system in various ways")
    (description
     "stress-ng stress-tests a computer system by exercising both physical
subsystems as operating system kernel interfaces.  It can stress the CPU, cache,
disk, memory, socket and pipe I/O, scheduling, and much more, in various
selectable ways.  This can trip hardware issues such as thermal overruns as well
as operating system bugs that occur only when a system is being thrashed hard.

You can also measure test throughput rates, which can be useful to observe
performance changes across different operating system releases or types of
hardware.  However, stress-ng is not a benchmark.  Use it with caution: some of
the tests can make poorly designed hardware run dangerously hot or make the
whole system lock up.

Compared to its inspiration, @command{stress}, @command{stress-ng} offers many
additional options such as the number of bogo operations to run, execution
metrics, verification of memory and computational operations, and considerably
more stress mechanisms.")
    (license license:gpl2+)))

(define-public detox
  (package
    (name "detox")
    (version "1.4.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/dharple/detox")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "116bgpbkh3c96h6vq0880rmnpb5kbnnlvvkpsrcib6928bj8lfvi"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake flex))
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-after 'unpack 'delete-configure
                    ;; The "configure" script is present, but otherwise the
                    ;; project is not bootstrapped: missing install-sh and
                    ;; Makefile.in, so delete it so the bootstrap phase will
                    ;; take over.
                    (lambda _ (delete-file "configure") #t))
                  (replace 'check
                    (lambda _
                      (invoke "./tests/test.sh" "src/detox"))))))
    (home-page "https://github.com/dharple/detox")
    (synopsis "Clean up file names")
    (description
     "Detox is a program that renames files to make them easier to work with
under Unix and related operating systems.  Spaces and various other unsafe
characters (such as \"$\") get replaced with \"_\".  ISO 8859-1 (Latin-1)
characters can be replaced as well, as can UTF-8 characters.")
    (license license:bsd-3)))

(define-public tree
  (package
    (name "tree")
    (version "2.2.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://oldmanprogrammer.net/tar/tree/"
                                  "tree-" version ".tgz"))
              (sha256
               (base32 "1v5j8igc5yjzs7w63010p8il5rw5qb1zy032l2ni3hy0g3f4bb38"))))
    (build-system gnu-build-system)
    (arguments
     (list
       #:phases
       #~(modify-phases %standard-phases
           (delete 'configure))         ; No configure script.
       #:tests? #f                      ; No check target.
       #:make-flags
       #~(list (string-append "PREFIX=" #$output)
               (string-append "CC=" #$(cc-for-target)))))
    (synopsis "Recursively list the contents of a directory")
    (description
     "Tree is a recursive directory listing command that produces a depth
indented listing of files, which is colorized ala dircolors if the LS_COLORS
environment variable is set and output is to tty.")
    (home-page "https://oldmanprogrammer.net/source.php?dir=projects/tree")
    (license license:gpl2+)))

(define-public lr
  (package
    (name "lr")
    (version "1.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.vuxu.org/lr/")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0qixmvxikyz02348xc0a718m9b1pzcazvf36rjbdk6ayn66g9hsd"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; There are no tests.
       #:make-flags (list (string-append "CC=" ,(cc-for-target))
                          (string-append "PREFIX=" %output))
       #:phases (modify-phases %standard-phases
                  (delete 'configure))))
    (synopsis "Tool to generate customized file listings")
    (description
     "lr is a tool for generating file listings, which includes the best
features of ls(1), find(1), stat(1) and du(1).")
    (home-page "https://git.vuxu.org/lr/about")
    (license license:expat)))

(define-public direvent
  (package
    (name "direvent")
    (version "5.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/direvent/direvent-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1flmswj1by9afqal55hc70l2hshcawyn0j2if92y6rxb58cwdfqx"))))
    (build-system gnu-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-before 'build 'substitute-file-names
                 (lambda* (#:key inputs #:allow-other-keys)
                   ;; Use the right shell when executing the watcher and
                   ;; user-provided shell commands.
                   (let ((bash (assoc-ref inputs "bash")))
                     (substitute* '("src/direvent.c" "src/progman.c")
                       (("\"/bin/sh\"")
                        (string-append "\"" bash "/bin/sh\""))))

                   ;; Adjust the test suite similarly.
                   (substitute* "tests/testsuite"
                     (("(SHELL=|#![[:space:]]*)/bin/sh" _ prefix)
                      (string-append prefix (which "sh")))
                     (("/bin/kill")
                      (which "kill"))))))))
    (home-page "https://www.gnu.org.ua/software/direvent/")
    (synopsis "Daemon to monitor directories for events such as file removal")
    (description
     "A daemon that monitors directories for events, such as creating,
deleting or modifying files.  It can monitor different sets of directories for
different events.  When an event is detected, direvent calls a specified
external program with information about the event, such as the location
within the file system where it occurred.  Thus, \"direvent\" provides an
easy way to react immediately if given files undergo changes, for example, to
track changes in important system configuration files.")
    (license license:gpl3+)))

(define-public libcap-ng
  (package
    (name "libcap-ng")
    (version "0.8.5")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://people.redhat.com/sgrubb/libcap-ng/libcap-ng-"
                    version ".tar.gz"))
              (sha256
               (base32
                "1zy6ga6cqi6qvy2p3091i8zbillymyv01g6gmapriymx3i6jk99v"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list "--disable-static"
             "--without-python")))
    (home-page "https://people.redhat.com/sgrubb/libcap-ng/")
    (synopsis "Library for more easily working with POSIX capabilities")
    (description
     "The libcap-ng library is intended to make programming with POSIX
capabilities easier than the traditional libcap library.  It includes
utilities that can analyse all currently running applications and print out
any capabilities and whether or not it has an open ended bounding set.  The
included utilities are designed to let admins and developers spot apps from
various ways that may be running with too much privilege.")
    ;; The library is lgpl2.1+, but also ships some utils which are gpl2+.
    (license (list license:lgpl2.1+ license:gpl2+))))

(define-public smartmontools
  (package
    (name "smartmontools")
    (version "7.4")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://sourceforge/smartmontools/smartmontools/"
                    version "/smartmontools-" version ".tar.gz"))
              (sha256
               (base32
                "0gcrzcb4g7f994n6nws26g6x15yjija1gyzd359sjv7r3xj1z9p9"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:make-flags
      #~(list "BUILD_INFO=\"(Guix)\"")
      #:configure-flags
      #~(list (format #f "--with-scriptpath=~{~a:~}$PATH"
                      (map (lambda (pkg)
                             (in-vicinity pkg "bin"))
                           '#$(list (this-package-input "coreutils-minimal")
                                    (this-package-input "sed")))))))
    (inputs (list coreutils-minimal
                  libcap-ng
                  sed))
    (home-page "https://www.smartmontools.org/")
    (synopsis "S.M.A.R.T. harddisk control and monitoring tools")
    (description
     "The smartmontools package contains utility programs to control and
monitor storage systems using the Self-Monitoring, Analysis and Reporting
Technology System (@dfn{S.M.A.R.T.}) built into most modern ATA and SCSI hard
disks.  In many cases, these utilities will provide advanced warning of disk
degradation and failure.")
    (license license:gpl2+)))

(define-public fdupes
  (package
    (name "fdupes")
    (version "2.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/adrianlopezroche/fdupes/"
                           "releases/download/v" version "/"
                           "fdupes-" version ".tar.gz"))
       (sha256
        (base32 "1irab60gkjmnvk1l0x92cb5vh9zw9g0vgr3s82mir93zpvn8v3c0"))))
    (build-system gnu-build-system)
    (inputs
     (list ncurses pcre2 sqlite))
    (home-page "https://github.com/adrianlopezroche/fdupes")
    (synopsis "Identify duplicate files")
    (description
     "fdupes is a program for identifying duplicate files residing within
specified directories.")
    (license license:expat)))

(define-public ansible-core
  (package
    (name "ansible-core")
    ;; XXX: Starting from 2.18.1, Ansible requires Python 3.11 or newer on the
    ;; controller, this is the latest version supporting 3.10.
    (version "2.17.7")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ansible_core" version))
       (sha256
        (base32 "1kysajyc0kh885dlba6aj0a2mnpcq06q09n3kcixdqn4sqsvgais"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:modules '((guix build pyproject-build-system)
                  (guix build utils)
                  (ice-9 ftw))
      #:test-flags
      #~(list "units"
              "--exclude" "test/units/cli/test_adhoc.py"
              "--exclude" "test/units/galaxy/test_collection_install.py"
              "--num-workers" (number->string (parallel-job-count)))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'relax-requirements
            (lambda _
              (substitute* "requirements.txt"
                ;; resolvelib >= 0.5.3, < 1.1.0
                ((">= 0.5.3, < 1.1.0") ""))))
          ;; Several ansible commands (ansible-config, ansible-console, etc.)
          ;; are just symlinks to a single ansible executable.  The ansible
          ;; executable behaves differently based on the value of sys.argv[0].
          ;; This does not work well with our wrap phase, and therefore the
          ;; following two phases are required as a workaround.
          (add-after 'unpack 'hide-wrapping
            (lambda _
              ;; Overwrite sys.argv[0] to hide the wrapper script from it.
              (substitute* "bin/ansible"
                (("import traceback" all)
                 (string-append all "
import re
sys.argv[0] = re.sub(r'\\.([^/]*)-real$', r'\\1', sys.argv[0])
")))))
          (add-after 'install 'replace-symlinks
            (lambda _
              ;; Replace symlinks with duplicate copies of the ansible
              ;; executable so that sys.argv[0] has the correct value.
              (with-directory-excursion (string-append #$output "/bin")
                (for-each
                 (lambda (ansible-symlink)
                   (delete-file ansible-symlink)
                   (copy-file "ansible" ansible-symlink))
                 (scandir "." (lambda (x)
                                (and (eq? 'symlink (stat:type (lstat x)))
                                     (string-prefix? "ansible-" x)
                                     (string=? "ansible" (readlink x)))))))))
          (add-after 'unpack 'patch-paths
            (lambda _
              (substitute* "lib/ansible/module_utils/compat/selinux.py"
                (("libselinux.so.1" name)
                 (string-append #$(this-package-input "libselinux")
                                "/lib/" name)))
              (substitute* "test/lib/ansible_test/_internal/ansible_util.py"
                (("PYTHONPATH=get_ansible_python_path\\(args\\)" all)
                 (string-append all "+ ':' + os.environ['GUIX_PYTHONPATH']")))
              (substitute* "test/lib/ansible_test/_internal/commands/units/__init__.py"
                (("PYTHONPATH=get_units_ansible_python_path\\(args, test_context)" all)
                 (string-append all "+ ':' + os.environ['GUIX_PYTHONPATH']")))
              (substitute* "test/units/modules/test_async_wrapper.py"
                (("/usr/bin/python")
                 (which "python")))))
         (replace 'check
           (lambda* (#:key inputs outputs tests? test-flags #:allow-other-keys)
             (when tests?
               ;; Otherwise Ansible fails to create its config directory.
               (setenv "HOME" "/tmp")
               ;; The test suite needs to be run with 'ansible-test', which
               ;; does some extra environment setup.  Taken from
               ;; https://raw.githubusercontent.com/ansible/ansible/\
               ;; devel/test/utils/shippable/shippable.sh.
               (apply invoke "python" "bin/ansible-test" test-flags)))))))
    (native-inputs
     (list openssh
           openssl
           python-mock
           python-pycryptodome
           python-pytest
           python-pytest-forked
           python-pytest-mock
           python-pytest-xdist
           python-pytz
           python-setuptools
           python-wheel))
    (inputs                    ;optional dependencies captured in wrap scripts
     (list libselinux
           sshpass))
    (propagated-inputs      ;core dependencies listed in egg-info/requires.txt
     (list python-cryptography
           python-jinja2
           python-packaging             ;for version number parsing
           python-paramiko
           python-passlib
           python-pexpect
           python-pyyaml
           python-resolvelib))
    (home-page "https://www.ansible.com/")
    (synopsis "Radically simple IT automation")
    (description "Ansible aims to be a radically simple IT automation system.
It handles configuration management, application deployment, cloud
provisioning, ad-hoc task execution, network automation, and multi-node
orchestration.  Ansible facilitates complex changes like zero-downtime rolling
updates with load balancers.  This package is the core of Ansible, which
provides the following commands:
@itemize
@item ansible
@item ansible-config
@item ansible-connection
@item ansible-console
@item ansible-doc
@item ansible-galaxy
@item ansible-inventory
@item ansible-playbook
@item ansible-pull
@item ansible-test
@item ansible-vault
@end itemize")
    (license license:gpl3+)))

(define-public ansible
  (package
    (name "ansible")
    (version "10.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ansible" version))
       (sha256
        (base32 "0apj783acx4jzkf3bnibn4y5jc6jd8ly7l0rdqq8f1jpgxal933x"))))
    (build-system python-build-system)
    (propagated-inputs (list ansible-core))
    ;; The Ansible collections are found by ansible-core via the Python search
    ;; path; the following search path ensures that they are found even when
    ;; Python is not present in the profile.
    (native-search-paths
     ;; XXX: Attempting to use (package-native-search-paths python)
     ;; here would cause an error about python being an unbound
     ;; variable in the tests/cpan.scm test.
     (list (search-path-specification
            (variable "GUIX_PYTHONPATH")
            (files (list "lib/python3.10/site-packages")))))
    (home-page "https://www.ansible.com/")
    (synopsis "Radically simple IT automation")
    (description "Ansible aims to be a radically simple IT automation system.
It handles configuration management, application deployment, cloud
provisioning, ad-hoc task execution, network automation, and multi-node
orchestration.  Ansible facilitates complex changes like zero-downtime rolling
updates with load balancers.  This package provides a curated set of
community-maintained Ansible collections, which contain playbooks, roles,
modules and plugins that extend Ansible.")
    ;; Those actually concern the Jenkins Ansible plugin, rather than the
    ;; Ansible Jenkins plugin.
    (properties `((lint-hidden-cve . ("CVE-2023-32982" "CVE-2023-32983"))))
    (license license:gpl3+)))

(define-public debops
  (package
    (name "debops")
    (version "3.2.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/debops/debops")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0y7bmrnynbw0hz88shfv301a9fsank2cx86fvb7jx6g6kkbsa9pz"))
       (patches
        (search-patches "debops-setup-py-avoid-git.patch"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-setuptools python-wheel))
    (inputs
     (list ansible
           encfs
           fuse-2
           util-linux ;; for umount
           findutils
           git
           git-crypt
           gnupg
           which))
    (propagated-inputs
     (list python-distro
           python-dotenv
           python-future
           python-gitpython
           python-jinja2
           python-pyyaml
           python-pyxdg
           python-toml))
    (arguments
     (list
      #:modules '((guix build pyproject-build-system)
                  (guix build utils)
                  (srfi srfi-26))
      #:phases #~(modify-phases %standard-phases
                   (add-after 'sanity-check 'wrap-script
                     (lambda* (#:key inputs #:allow-other-keys)
                       (wrap-program (string-append #$output "/bin/debops")
                         `("PATH" ":" prefix
                           ,(map dirname
                                 (map (cut search-input-file inputs <>)
                                      (list "bin/ansible"
                                            "bin/gpg"
                                            "bin/git"
                                            "bin/git-crypt"
                                            "bin/umount"))))))))))
    (home-page "https://www.debops.org/")
    (synopsis "Collection of general-purpose Ansible roles")
    (description "The Ansible roles provided by that can be used to manage
Debian or Ubuntu hosts.  In addition, a default set of Ansible playbooks can
be used to apply the provided roles in a controlled way, using Ansible
inventory groups.

The roles are written with a high customization in mind, which can be done
using Ansible inventory.  This way the role and playbook code can be shared
between multiple environments, with different configuration in to each one.

Services can be managed on a single host, or spread between multiple hosts.
DebOps provides support for different SQL and NoSQL databases, web servers,
programming languages and specialized applications useful in a data center
environment or in a cluster.  The project can also be used to deploy
virtualization environments using KVM/libvirt, Docker or LXC technologies to
manage virtual machines and/or containers.")
    (license license:gpl3+)))

(define-public emacs-ansible-doc
  (let ((commit "86083a7bb2ed0468ca64e52076b06441a2f8e9e0"))
    (package
      (name "emacs-ansible-doc")
      (version (git-version "0.4" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/lunaryorn/ansible-doc.el")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0lap404ch74w99n3xip176jr42b38xhyzkfnkyqg0g3wk2cd3aq8"))))
      (build-system emacs-build-system)
      ;; Unmaintained by upstream.
      (home-page "https://github.com/lunaryorn/ansible-doc.el")
      (synopsis "Ansible documentation for Emacs")
      (description
       "This package provides an Ansible documentation for GNU Emacs.

@code{ansible-doc} allows you to view the documentation of an Ansible
module and @code{ansible-doc-mode} minor mode adds documentation
lookup to YAML Mode.  You could enable the mode with @code{(add-hook
'yaml-mode-hook #'ansible-doc-mode)}.")
      (license license:gpl3+))))

(define-public cpulimit
  (package
    (name "cpulimit")
    (version "0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/opsengine/cpulimit")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1dz045yhcsw1rdamzpz4bk8mw888in7fyqk1q1b3m1yk4pd1ahkh"))
       (patches (search-patches "cpulimit-with-glib-2.32.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (delete 'configure)
                  (replace 'build
                    (lambda* (#:key make-flags #:allow-other-keys)
                      (apply invoke "make" "-Csrc" make-flags)))
                  (replace 'check
                    (lambda* (#:key tests? make-flags #:allow-other-keys)
                      (when tests?
                        (apply invoke "make" "-Ctests" make-flags))
                      #t))
                  (replace 'install
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (bin (string-append out "/bin")))
                        (install-file "src/cpulimit" bin))
                      #t)))
       #:make-flags (list (string-append "CC=" ,(cc-for-target)))))
    (home-page "https://github.com/opsengine/cpulimit")
    (synopsis "Limit CPU usage")
    (description
     "Cpulimit limits the CPU usage of a process.  It does not change the nice
value or other scheduling priority settings, but the real CPU usage, and is
able to adapt itself dynamically to the overall system load.  Children
processes and threads of the specified process may optionally share the same
limits.")
    (license license:gpl2+)))

(define-public corectrl
  (package
    (name "corectrl")
    (version "1.4.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/corectrl/corectrl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0qpc04xxzv4jbqqlraqriipix4ph7bm1hfiry807jjp668i9n25d"))
       (patches (search-patches "corectrl-polkit-install-dir.patch"))))
    (build-system qt-build-system)
    (arguments
     (list
      #:configure-flags #~(list "-DINSTALL_DBUS_FILES_IN_PREFIX=true"
                                (string-append "-DPOLKIT_POLICY_INSTALL_DIR="
                                               #$output
                                               "/share/polkit-1/actions")
                                (string-append "-DWITH_PCI_IDS_PATH="
                                               #$(this-package-input "hwdata")
                                               "/share/hwdata/pci.ids"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'embed-absolute-references
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "src/core/info/common/cpuinfolscpu.cpp"
                (("\"lscpu\"")
                 (string-append
                  "\"" (search-input-file inputs "bin/lscpu") "\"")))
              (substitute* "src/core/info/common/gpuinfovulkan.cpp"
                (("\"vulkaninfo\"")
                 (string-append
                  "\"" (search-input-file inputs "bin/vulkaninfo") "\"")))
              (substitute* (list "src/core/info/common/swinfomesa.cpp"
                                 "src/core/info/common/gpuinfoopengl.cpp")
                (("\"glxinfo\"")
                 (string-append
                  "\"" (search-input-file inputs "bin/glxinfo") "\""))))))))
    ;; Text formatting only supported since C++20, which is available in gcc-13.
    ;; https://en.cppreference.com/w/cpp/compiler_support#cpp_lib_format_201907L
    (native-inputs (list catch2-3
                         gcc-13
                         pkg-config
                         qttools-5))
    (inputs (list dbus
                  botan
                  hwdata
                  mesa-utils
                  polkit
                  procps
                  pugixml
                  qtcharts-5
                  qtdeclarative-5
                  qtquickcontrols2-5
                  qtsvg-5
                  qtwayland-5
                  quazip
                  spdlog
                  trompeloeil
                  units
                  util-linux
                  vulkan-tools
                  zlib))
    (home-page "https://gitlab.com/corectrl/corectrl")
    (synopsis "Profile based system control utility")
    (description
     "CoreCtrl allows you to control with ease your computer hardware using
application profiles.")
    (license (list license:gpl3))))

(define-public autojump
  (package
    (name "autojump")
    (version "22.5.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/wting/autojump")
             (commit (string-append "release-v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1rgpsh70manr2dydna9da4x7p8ahii7dgdgwir5fka340n1wrcws"))))
    (build-system gnu-build-system)
    (native-inputs                      ; for tests
     (list python-mock python-pytest))
    (inputs
     `(("python" ,python-wrapper)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'make-git-checkout-writable
           ;; ‘install.py’ modifies files before installing them.
           (lambda _
             (for-each make-file-writable (find-files "."))
             #t))
         (delete 'configure)
         (delete 'build)
         (replace 'check
           (lambda _
             (invoke "python" "tests/unit/autojump_utils_test.py")))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (setenv "SHELL" (which "bash"))
             (invoke "python" "install.py"
                     (string-append "--destdir="
                                    (assoc-ref outputs "out"))))))))
    (home-page "https://github.com/wting/autojump")
    (synopsis "Shell extension for file system navigation")
    (description
     "Autojump provides a faster way to navigate your file system, with a \"cd
command that learns\".  It works by maintaining a database of the directories
you use the most from the command line and allows you to \"jump\" to
frequently used directories by typing only a small pattern.")
    (license license:gpl3+)))

(define-public fasd
  (package
    (name "fasd")
    (version "1.0.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/whjvenyl/fasd")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0q72a54dcahc9pan5qkmnsvpqiqgjjxwdxzzm8pxzylpr329jjyh"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (delete 'configure))  ;no configuration
       #:tests? #f                      ;no tests
       #:make-flags (list (string-append "PREFIX=" %output))))
    (home-page "https://github.com/whjvenyl/fasd")
    (synopsis "Quick access to files and directories for shells")
    (description
     "Fasd (pronounced similar to \"fast\") is a command-line productivity booster.
Fasd offers quick access to files and directories for POSIX shells.  It is inspired
by tools like @code{autojump}, @code{z}, and @code{v}.  Fasd keeps track of files and
directories you have accessed so that you can quickly reference them in the command
line.")
    (license license:expat)))

(define-public iftop
  (package
    (name "iftop")
    (version "1.0pre4")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.ex-parrot.com/~pdw/iftop/download"
                                  "/iftop-" version ".tar.gz"))
              (sha256
               (base32
                "15sgkdyijb7vbxpxjavh5qm5nvyii3fqcg9mzvw7fx8s6zmfwczp"))))
    (build-system gnu-build-system)
    (inputs
      (list libpcap ncurses))
    (arguments
      ;; Fix build failure with GCC 10
     '(#:configure-flags '("CFLAGS=-O2 -g -fcommon")))
    (synopsis "Monitor network usage")
    (description "Iftop does for network usage what @command{top} does
for CPU usage.  It listens to network traffic on a named interface and
displays a table of current bandwidth usage by pairs of hosts.")
    (home-page "http://www.ex-parrot.com/~pdw/iftop/")
    (license license:gpl2+)))

(define-public nettop
  (let ((revision "0")
        (commit "689d6557196e9fcc92cffba82e00fac0386419e5"))
    (package
      (name "nettop")
      (version (git-version "0.5" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Emanem/nettop")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0f6qkj4p4c0gap16ncnhkm802vi6dr7z9rjfz7gzvzhm7jg08aj8"))
         (modules '((guix build utils)))
         (snippet
          #~(begin
             (substitute* "Makefile"
               (("-lcurses" _) "-lncurses"))))))
      (build-system gnu-build-system)
      (arguments
       (list
        #:tests? #f                     ; no make check
        #:make-flags #~(list "release")
        #:phases
        #~(modify-phases %standard-phases
            (delete 'configure)         ; no configure script
            (replace 'install           ; no install target
              (lambda _
                (let ((out (string-append #$output "/bin")))
                  (install-file "nettop" out)))))))
      (inputs (list libpcap ncurses))
      (synopsis "Monitor network usage by process and host")
      (description "nettop is a traffic visualizer for the terminal that
summarizes network bandwidth by process and remote host.")
      (home-page "https://github.com/Emanem/nettop")
      (license license:gpl3+))))

(define-public munge
  (package
    (name "munge")
    (version "0.5.14")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/dun/munge/releases/"
                                  "download/munge-" version "/munge-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "0h06sghb4rqvv1ywyd6mzsmbcgh712v6ygrff0gzm440y4ca41k6"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Don't insist on write access to /var.
                  (substitute* "src/etc/Makefile.in"
                    (("\\$\\(INSTALL\\)(.*)localstatedir" _ middle)
                     (string-append "-$(INSTALL)" middle "localstatedir"))
                    (("\\$\\(MKDIR_P\\) .*(local|run)statedir.*")
                     ""))
                  #t))))
    (inputs
     (list openssl libgcrypt))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list "--localstatedir=/var"
             (string-append "--with-pkgconfigdir="
                            (assoc-ref %outputs "out") "/lib/pkgconfig")
             (string-append "--with-libgcrypt-prefix="
                            (assoc-ref %build-inputs "libgcrypt"))
             ,@(if (%current-target-system)
                 ;; Assume yes on pipes when cross compiling.
                 `("ac_cv_file__dev_spx=yes"
                   "x_ac_cv_check_fifo_recvfd=yes")
                 '()))
       #:phases
       (modify-phases %standard-phases
         ;; XXX Many test series fail.  Some might be fixable, others do no-no
         ;; things like invoking ‘sudo’.
         (add-after 'unpack 'skip-failing-tests
           (lambda _
             (for-each (lambda (test)
                         (substitute* "t/Makefile.in"
                           (((string-append test "\\.t ")) "")))
                       (list "0100-munged-lock"
                             "0010-basic"
                             "0011-munged-cmdline"
                             "0012-munge-cmdline"
                             "0013-unmunge-cmdline"
                             "0101-munged-security-socket"
                             "0102-munged-security-keyfile"
                             "0103-munged-security-logfile"
                             "0110-munged-origin-addr"))
             #t)))))
    (home-page "https://dun.github.io/munge/")
    (synopsis "Cluster computing authentication service")
    (description
     "Munge is an authentication service for creating and validating
credentials.  It allows a process to authenticate the UID and GID of another
local or remote process within a group of hosts having common users and
groups.  These hosts form a security realm that is defined by a shared
cryptographic key.  Clients within this security realm can create and validate
credentials without the use of root privileges, reserved ports, or
platform-specific methods.")
    (license license:gpl3+)))

(define-public audit
  (package
    (name "audit")
    (home-page "https://people.redhat.com/sgrubb/audit/")
    (version "3.0.9")
    (source (origin
              (method url-fetch)
              (uri (string-append home-page "audit-" version ".tar.gz"))
              (sha256
               (base32
                "0y5w8pl91xapi49ih1pw7h48lac201cj7fm89hkklmzi9m2715gx"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags (list "--with-python=no"
                               "--disable-static")))
    (inputs
     (list openldap gnutls cyrus-sasl))
    (synopsis "User-space component to the Linux auditing system")
    (description
     "This is the user-space component to the Linux auditing system, which
allows logging of system calls made by user-land processes.  @command{auditd} is
responsible for writing audit records to the disk.  Viewing the logs is done
with the @code{ausearch} or @code{aureport} utilities.  Configuring the audit
rules is done with the @code{auditctl} utility.")
    (license license:gpl2+)))

(define-public nmap
  (package
    (name "nmap")
    (version "7.93")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://nmap.org/dist/nmap-" version
                                  ".tar.bz2"))
              (sha256
               (base32
                "0lb6s4nmmicfnc221mzgx2w51dcd4b2dhx22pabcqnp2jd3zxg2m"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (for-each delete-file-recursively
                            ;; Remove bundled lua, pcap, and pcre libraries.
                            ;; FIXME: Remove bundled liblinear once packaged.
                            '("liblua"
                              "libpcap"
                              "libpcre"
                              ;; Remove pre-compiled binares.
                              "mswin32"))))))
    (build-system gnu-build-system)
    (outputs '("out" "ndiff"))          ; TODO Add zenmap output
    (arguments
     `(#:configure-flags '("--without-zenmap")
       #:phases
       (modify-phases %standard-phases
         (add-after 'configure 'patch-Makefile
           (lambda _
             (substitute* "Makefile"
               ;; Do not attempt to build lua.
               (("build-dnet build-lua") "build-dnet"))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (define (make out . args)
               (apply invoke "make"
                      (string-append "prefix=" out)
                      args))
             (define (python-path dir)
               (string-append dir "/lib/python"
                              ,(version-major+minor
                                 (package-version python))
                              "/site-packages"))
             (let ((out (assoc-ref outputs "out"))
                   (ndiff (assoc-ref outputs "ndiff")))
               (for-each mkdir-p (list out ndiff))
               (make out
                 "install-nmap"
                 "install-nse"
                 "install-ncat"
                 "install-nping")
               (make ndiff "install-ndiff")
               (wrap-program (string-append ndiff "/bin/ndiff")
                 `("GUIX_PYTHONPATH" prefix
                   (,(python-path ndiff)))))))
         ;; These are the tests that do not require network access.
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "make"
                       "check-nse"
                       "check-ndiff"
                       "check-dns")))))
       ;; Nmap can't cope with out-of-source building.
       #:out-of-source? #f))
    (inputs
     (list bash-minimal                 ;for wrap-program
           libpcap
           lua
           openssl-3.0
           pcre
           zlib                         ; for NSE compression
           python-2))                   ; for ndiff
    (home-page "https://nmap.org/")
    (synopsis "Network discovery and security auditing tool")
    (description
     "Nmap (\"Network Mapper\") is a network discovery and security auditing
tool.  It is also useful for tasks such as network inventory, managing service
upgrade schedules, and monitoring host or service uptime.  It also provides an
advanced netcat implementation (ncat), a utility for comparing scan
results (ndiff), and a packet generation and response analysis tool (nping).")
    ;; See <https://github.com/nmap/nmap/issues/2199#issuecomment-1380592744>.
    ;; This package uses nmap's bundled versions of libdnet and liblinear, which
    ;; both use a 3-clause BSD license.
    (license (list license:nmap license:bsd-3))))

(define-public dstat
  (package
    (name "dstat")
    (version "0.7.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dstat-real/dstat")
             (commit (string-append "v" version))))
       (file-name (git-file-name "dstat" version))
       (sha256
        (base32 "1qnmkhqmjd1m3if05jj29dvr5hn6kayq9bkkkh881w472c0zhp8v"))
       (patches (search-patches "dstat-fix-crash-when-specifying-delay.patch"
                                "dstat-skip-devices-without-io.patch"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f                       ; no make check
      #:make-flags
      #~(list (string-append "prefix=" #$output))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-python3-DeprecationWarning
            (lambda _
              (substitute* "dstat"
                (("collections") "collections.abc"))))
          (delete 'configure)           ; no configure script
          (add-after 'install 'wrap
            (lambda _
              (wrap-program (string-append #$output "/bin/dstat")
                `("GUIX_PYTHONPATH" ":" prefix
                  (,(getenv "GUIX_PYTHONPATH")))))))))
    (inputs
     (list bash-minimal                 ;for wrap-program
           python-wrapper
           python-six))
    (synopsis "Versatile resource statistics tool")
    (description "Dstat is a versatile replacement for @command{vmstat},
@command{iostat}, @command{netstat}, and @command{ifstat}.  Dstat overcomes
some of their limitations and adds some extra features, more counters and
flexibility.  Dstat is handy for monitoring systems during performance tuning
tests, benchmarks or troubleshooting.

Dstat allows you to view all of your system resources in real-time, you can,
e.g., compare disk utilization in combination with interrupts from your IDE
controller, or compare the network bandwidth numbers directly with the disk
throughput (in the same interval).")
    (home-page "http://dag.wiee.rs/home-made/dstat/")
    (license license:gpl2+)))

(define-public dool
  (package
    (name "dool")
    (version "1.3.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/scottchiefbaker/dool")
             (commit (string-append "v" version))))
       (file-name (git-file-name "dool" version))
       (sha256
        (base32 "0y5y5c07hgj6v2nvimnwc8myx43li8ib40hdvz7q4q1pdqx3r0jl"))))
    (build-system python-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-symlinks-and-snap-packaging
            ;; Remove symlinks that make 'ensure-no-mtimes-pre-1980 fail.
            (lambda _
              (delete-file-recursively "packaging/snap")))
          (delete 'build)
          (replace 'install
            (lambda _
              (substitute* "install.py"
                (("(bin_dir *= ?).*" _ prefix)
                 (string-append prefix  "\"" #$output "/bin/\"\n"))
                (("(plugin_dir *= ?).*" _ prefix)
                 (string-append prefix "\"" #$output "/share/dool/\"\n"))
                (("(manpage_dir *= ?).*" _ prefix)
                 (string-append prefix "\"" #$output "/share/man/man1/\"\n")))
              (invoke "python" "install.py" "--root")))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "./dool" "--version")
                (invoke "./dool" "-ta" "1" "5")))))))
    (synopsis "Command line system resource monitoring tool")
    (description "Dool is a command line tool to monitor many aspects of your
system: CPU, Memory, Network, Load Average, etc.  It also includes a robust
plug-in architecture to allow monitoring other system metrics.")
    (home-page "https://github.com/scottchiefbaker/dool")
    (license license:gpl2+)))

(define-public thefuck
  (package
    (name "thefuck")
    (version "3.32")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nvbn/thefuck")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "18ipa1bm6q1n5drbi8i65726hhqhl1g41390lfqrc11hkbvv443d"))
       (patches (search-patches "thefuck-test-environ.patch"
                                "thefuck-remove-broken-tests.patch"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (delete 'check)
         (add-after 'install 'check
           (lambda* (#:key inputs outputs #:allow-other-keys)
             ;; Tests look for installed package
             (add-installed-pythonpath inputs outputs)
             ;; Some tests need write access to $HOME.
             (setenv "HOME" "/tmp")
             ;; Even with that, this function tries to mkdir /.config.
             (substitute* "tests/test_utils.py"
               (("settings\\.init\\(\\)") ""))
             (invoke "py.test" "-v"))))))
    (propagated-inputs
     (list python-colorama python-decorator python-psutil python-pyte
           python-six))
    (native-inputs
     (list go python-mock python-pytest python-pytest-mock))
    (home-page "https://github.com/nvbn/thefuck")
    (synopsis "Correct mistyped console command")
    (description
     "The Fuck tries to match a rule for a previous, mistyped command, creates
a new command using the matched rule, and runs it.")
    (license license:x11)))

(define-public di
  (package
    (name "di")
    (version "4.53")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/diskinfo-di/"
                           "di-" version ".tar.gz"))
       (sha256
        (base32 "0gp806m7jk2rfymy5r62a2lfd8jq879qy94blrjqvb0xq7pmpp80"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f                       ; obscure test failures
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)           ; no configure script
          (add-before 'build 'override-environment
            (lambda _
              (setenv "CC" #$(cc-for-target))
              (setenv "prefix" #$output))))
      #:make-flags
      #~(list "--environment-overrides")))
    (home-page "https://gentoo.com/di/")
    (synopsis "Advanced df like disk information utility")
    (description
     "@code{di} is a disk information utility, displaying everything that your
@code{df} command does and more.  It features the ability to display your disk
usage in whatever format you prefer.  It is designed to be highly portable and
produce uniform output across heterogeneous networks.")
    (license license:zlib)))

(define-public cbatticon
  (package
    (name "cbatticon")
    (version "1.6.13")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/valr/cbatticon")
             (commit version)))
       (sha256
        (base32 "1xs37xrycvk0021r5l3xs4ijgf3lm25d2zhm8dppb5kx66xcj22m"))
       (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no tests
       #:make-flags
       (list (string-append "PREFIX=" (assoc-ref %outputs "out"))
             ,(string-append "CC=" (cc-for-target)))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))         ; no configure script
    (inputs
     `(("gtk+" ,gtk+)
       ("gettext" ,gettext-minimal)
       ("libnotify" ,libnotify)))
    (native-inputs
     (list pkg-config))
    (synopsis "Lightweight battery icon for the system tray")
    (description "cbatticon is a lightweight battery icon that displays
the status of your battery in the system tray.")
    (home-page "https://github.com/valr/cbatticon")
    (license license:gpl2+)))

(define-public interrobang
  (let ((revision "1")
        (commit "896543735e1c99144765fdbd7b6e6b5afbd8b881"))
    (package
      (name "interrobang")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/TrilbyWhite/interrobang")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1n13m70p1hfba5dy3i8hfclbr6k9q3d9dai3dg4jvhdhmxcpjzdf"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f                    ; no tests
         #:phases
         (modify-phases %standard-phases
           (delete 'configure))         ; no configure script
         #:make-flags (list (string-append "PREFIX="
                                           (assoc-ref %outputs "out")))))
      (inputs
       (list libx11))
      (native-inputs
       (list pkg-config))
      (synopsis "Scriptable launcher menu")
      (description "Interrobang is a scriptable launcher menu with a customizable
shortcut syntax and completion options.")
      (home-page "https://github.com/TrilbyWhite/interrobang")
      (license license:gpl3+))))

(define-public pam-hooks
  (package
    (name "pam-hooks")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://upsilon.cc/~zack/hacking/software/"
                           name "/" name "_" version ".tar.gz"))
       (sha256
        (base32 "102azxhvs53akkhgg2dkqg8m0jd6804fx8v7gq7pp03qm3m7bhqi"))))
    (build-system gnu-build-system)
    (arguments
     (list #:make-flags #~`("CC=gcc"
                            ,(string-append "DESTDIR=" #$output))
           #:phases #~(modify-phases %standard-phases
                        (delete 'configure))
           ;; There is a test directory, but it's unclear how to run them.
           #:tests? #f))
    (inputs (list linux-pam))
    (home-page "https://upsilon.cc/~zack/hacking/software/pam-hooks/")
    (synopsis "Linux-PAM module for login/logout hooks")
    (description "@code{pam-hooks} is a tiny PAM module enabling the
execution of hook scripts when a PAM session is opened or closed.  The
typical use case is the need of doing some per-user set-up when a user
logs via a PAM-aware login mechanism and/or the need of doing some
per-user clean-up when the user logs out.")
    (license license:gpl3+)))

(define-public pam-krb5
  (package
    (name "pam-krb5")
    (version "4.8")
    (source (origin
              (method url-fetch)
              (uri
                (list (string-append
                        "https://archives.eyrie.org/software/kerberos/"
                        "pam-krb5-" version ".tar.xz")
                      (string-append
                        "https://archives.eyrie.org/software/ARCHIVE/"
                        "pam-krb5/pam-krb5-" version ".tar.xz")))
              (patches (search-patches "pam-krb5-CVE-2020-10595.patch"))
              (sha256
               (base32
                "1qjp8i1s9bz7g6kiqrkzzkxn5pfspa4sy53b6z40fqmdf9przdfb"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'disable-tests
           (lambda _
             ;; The build container seems to interfere with some tests.
             (substitute* "tests/TESTS"
               (("module/basic\n")  ""))
             (substitute* "tests/TESTS"
               (("pam-util/vector\n")  ""))
             #t)))))
    (inputs
     (list linux-pam mit-krb5))
    (native-inputs
     (list perl perl-test-pod)) ; required for tests
    (synopsis "Kerberos PAM module")
    (description
     "Pam-krb5 is a Kerberos PAM module for either MIT Kerberos or Heimdal.
It supports ticket refreshing by screen savers, configurable
authorization handling, authentication of non-local accounts for network
services, password changing, and password expiration, as well as all the
standard expected PAM features.  It works correctly with OpenSSH, even
with @code{ChallengeResponseAuthentication} and @code{PrivilegeSeparation}
enabled, and supports extensive configuration either by PAM options or in
krb5.conf or both.  PKINIT is supported with recent versions of both MIT
Kerberos and Heimdal and FAST is supported with recent MIT Kerberos.")
    (home-page "https://www.eyrie.org/~eagle/software/pam-krb5/")
    ;; Dual licenced under  a homebrew non-copyleft OR GPL (any version)
    ;; However, the tarball does not contain a copy of the GPL,  so unless
    ;; we put one in, we cannot distribute it under GPL without violating
    ;; clause requiring us to give all recipients a copy.
    (license license:gpl1+)))

(define (sunxi-tools-source version)
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/linux-sunxi/sunxi-tools")
             (commit (string-append "v" version))))
       (sha256
        (base32 "04f3jqg8ww4jxsf9c6ddcdgy2xbhkyp0b3l5f1hvvbv94p81rjxd"))
       (patches
        (search-patches "sunxi-tools-remove-sys-io.patch"))
       (modules '((guix build utils)))
       (snippet
        ;; Remove binaries contained in the tarball which are only for the
        ;; target and can be regenerated anyway.
        '(begin
           (delete-file-recursively "bin")
           #t))
       (file-name (git-file-name "sunxi-tools" version))))

(define sunxi-target-tools
  (package
    (name "sunxi-target-tools")
    (version "1.4.2")
    (build-system gnu-build-system)
    (source
     (sunxi-tools-source version))
    (arguments
     `(#:system "armhf-linux"
       #:tests? #f
       #:make-flags (list (string-append "PREFIX="
                                         (assoc-ref %outputs "out"))
                          (string-append "CROSS_COMPILE=")
                          "CC=gcc")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'build
           (lambda* (#:key make-flags #:allow-other-keys)
             (apply invoke "make" "target-tools" make-flags)))
         (replace 'install
           (lambda* (#:key make-flags #:allow-other-keys)
             (apply invoke "make" "install-target-tools"
                    make-flags))))))
    (home-page "https://github.com/linux-sunxi/sunxi-tools")
    (synopsis "Hardware management tools for Allwinner computers")
    (description "This package contains tools for Allwinner devices:
@enumerate
@item @command{sunxi-meminfo}: Prints memory bus settings.
@end enumerate")
    (license license:gpl2+)))

(define-public sunxi-tools
  (package
    (name "sunxi-tools")
    (version "1.4.2")
    (source
     (sunxi-tools-source version))
    (native-inputs
     (list sunxi-target-tools pkg-config))
    (inputs
     (list libusb))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no tests exist
       #:make-flags (list (string-append "PREFIX="
                                         (assoc-ref %outputs "out"))
                          (string-append "CROSS_COMPILE=disabled")
                          "CC=gcc")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'build
           (lambda* (#:key make-flags #:allow-other-keys)
             (apply invoke "make" "tools" "misc" make-flags)))
         (replace 'install
           (lambda* (#:key inputs outputs make-flags #:allow-other-keys)
             ;; Those tools have been built for armhf but are part of the
             ;; installation in the upstream package.  So do the same
             ;; here.
             (copy-recursively (assoc-ref inputs "sunxi-target-tools")
                               (assoc-ref outputs "out"))
             (apply invoke "make" "install-tools" "install-misc"
                    make-flags))))))
    (home-page "https://github.com/linux-sunxi/sunxi-tools")
    (synopsis "Hardware management tools for Allwinner computers")
    (description "This package contains tools for Allwinner devices:
@enumerate
@item @command{sunxi-fexc}, @command{bin2fex}, @command{fex2bin}: Compile
a textual description of a board (.fex) to a binary representation (.bin).
@item @command{sunxi-fel}: Puts an Allwinner device into FEL mode which
makes it register as a special USB device (rather than USB host).
You can then connect it to another computer and flash it from there.
@item @command{sunxi-nand-part}: Partitions NAND flash.
@item @command{sunxi-bootinfo}: Reads out boot0 and boot1 (Allwinner
bootloader) parameters.
@item @command{sunxi-pio}: Sets GPIO parameters and oscillates a GPIO
in order to be able to find it.
@item @command{sunxi-meminfo}: Prints memory bus settings.
@item @command{sunxi-nand-image-builder}: Prepares raw NAND images.
@end enumerate")
    (license license:gpl2+)))

(define-public xfel
  (package
    (name "xfel")
    (version "1.2.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/xboot/xfel.git")
              (commit (string-append "v" version))))
       (sha256
         (base32 "0gs37w5zjfmyadm49hdalq6vr6gidc683agz3shncgj93x2hxx02"))
       (file-name (git-file-name name version))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list libusb))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; No tests exist
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-installation-target
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute* "Makefile"
                (("/usr/local") out)
                (("/usr") out)
                (("/etc/udev/rules.d")
                 (string-append out "/lib/udev/rules.d"))
                (("udevadm control --reload") ; next version will remove this
                  "")))))
         (delete 'configure))))
    (home-page "https://github.com/xboot/xfel")
    (synopsis "Remote debugging tool for Allwinner devices")
    (description "This package contains a debugging tool for Allwinner devices
(connects via USB OTG).")
    (license license:expat)))

(define-public sedsed
  (package
    (name "sedsed")
    (version "1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/aureliojargas/sedsed")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "05cl35mwljdb9ynbbsfa8zx6ig8r0xncbg2cir9vwn5manndjj18"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f                      ; no tests
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-sed-in
           (lambda _
             (substitute* "sedsed.py"
               (("sedbin = 'sed'")
                (string-append "sedbin = '" (which "sed") "'")))
             #t))
         (delete 'build)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               ;; Just one file to copy around
               (install-file "sedsed.py" bin)
               #t)))
         (add-after 'wrap 'symlink
           ;; Create 'sedsed' symlink to "sedsed.py".
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (sed (string-append bin "/sedsed"))
                    (sedpy (string-append bin "/sedsed.py")))
               (symlink sedpy sed)
               #t))))))
    (home-page "https://aurelio.net/projects/sedsed")
    (synopsis "Sed sed scripts")
    (description
     "@code{sedsed} can debug, indent, tokenize and HTMLize your @command{sed}
script.

In debug mode, it reads your script and adds extra commands to it.  When
executed you can see the data flow between the commands, revealing all the
magic sed performs on its internal buffers.

In indent mode, your script is reformatted with standard spacing.

In tokenize mode, you can see the elements of every command you use.

In HTMLize mode, your script is converted to a beautiful colored HTML file,
with all the commands and parameters identified for your viewing pleasure.

With sedsed you can master any sed script.  No more secrets, no more hidden
buffers.")
    (license license:expat)))

(define-public igt-gpu-tools
  (package
    (name "igt-gpu-tools")
    (version "1.28")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.freedesktop.org/drm/igt-gpu-tools.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "15mnsgzlpd4jkr2zy3jzx0b021g89fa61b8sdm8rjp27gxqkl8mm"))))
    (build-system meson-build-system)
    (arguments
     `(#:tests? #f              ; many of the tests try to load kernel modules
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'find-rst2man.py
           (lambda _
             (substitute* "man/meson.build"
               (("'rst2man'") "'rst2man.py'")))))))
    (inputs
     (list cairo
           elfutils ; libdw
           eudev
           kmod
           libdrm
           libpciaccess
           libunwind
           procps
           python))
    (native-inputs
     (list bison flex pkg-config python-docutils))
    (home-page "https://gitlab.freedesktop.org/drm/igt-gpu-tools")
    (synopsis "Tools for development and testing of the Intel DRM driver")
    (description "IGT GPU Tools is a collection of tools for development and
testing of the Intel DRM driver.  There are many macro-level test suites that
get used against the driver, including xtest, rendercheck, piglit, and
oglconform, but failures from those can be difficult to track down to kernel
changes, and many require complicated build procedures or specific testing
environments to get useful results.  Therefore, IGT GPU Tools includes
low-level tools and tests specifically for development and testing of the
Intel DRM Driver.")
    (supported-systems '("i686-linux" "x86_64-linux"))
    (license license:expat)))

(define-public neofetch
  (package
    (name "neofetch")
    (version "7.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/dylanaraps/neofetch")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0i7wpisipwzk0j62pzaigbiq42y1mn4sbraz4my2jlz6ahwf00kv"))))
    (build-system gnu-build-system)
    (arguments
     (list #:tests? #f                      ; there are no tests
           #:make-flags
           #~(list (string-append "PREFIX=" #$output))
           #:phases
           #~(modify-phases %standard-phases
               (delete 'configure))))         ; no configure script
    (home-page "https://github.com/dylanaraps/neofetch")
    (synopsis "System information script")
    (description "Neofetch is a command-line system information tool written in
Bash.  Neofetch displays information about your system next to an image, your OS
logo, or any ASCII file of your choice.  The main purpose of Neofetch is to be
used in screenshots to show other users what operating system or distribution
you are running, what theme or icon set you are using, etc.")
    (license license:expat)))

(define-public hyfetch
  (package
    (name "hyfetch")
    (version "1.99.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hykilpikonna/hyfetch")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "02bc6dhwvf1daqlsw3am9y2wjfkhs8lpw3vgdxw74jg0w9bpzg8q"))))
    (build-system python-build-system)
    (arguments (list #:tests? #f))      ;no tests
    (inputs (list python-typing-extensions))
    (home-page "https://github.com/hykilpikonna/HyFetch")
    (synopsis "@code{neofetch} with pride flags <3")
    (description "HyFetch is a command-line system information tool fork of
@code{neofetch}.  HyFetch displays information about your system next to your
OS logo in ASCII representation.  The ASCII representation is then colored in
the pattern of the pride flag of your choice.  The main purpose of HyFetch is to
be used in screenshots to show other users what operating system or distribution
you are running, what theme or icon set you are using, etc.")
    (license license:expat)))

(define-public uwufetch
  (package
    (name "uwufetch")
    (version "2.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/TheDarkBug/uwufetch")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "182jwkm4vacz2bhyn7v4jl9bxs7md51az958r0qfp9ky71m2q3vh"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f ;no tests
      #:make-flags
      #~(list (string-append "DESTDIR=" #$output)
              (string-append "ETC_DIR=" #$output "/etc")
              (string-append "CC=" #$(cc-for-target)))
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (add-before 'build 'patch-source-paths
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((grep (search-input-file inputs "/bin/grep"))
                    (awk (search-input-file inputs "/bin/awk")))
                (substitute* "fetch.c"
                  (("grep") grep)
                  (("awk") awk))
                (substitute* "uwufetch.c"
                  (("(/usr(/local)?)(.*;)" all _ _ rest)
                   (string-append #$output rest))))))
          ;; TODO this will be fixed in the next release of uwufetch
          (add-before 'install 'make-include-dir
            (lambda _
              (mkdir-p (string-append #$output "/include")))))))
    (inputs (list lshw
                  gawk
                  grep
                  ;; viu XXX not yet packaged in Guix
                  xwininfo))
    (home-page "https://github.com/TheDarkBug/uwufetch")
    (synopsis "Meme system info tool based on Nyan/UwU trend")
    (description
     "UwUFetch is a system information tool in the lineage of NeoFetch,
PFetch, HyFetch, and the like.  It prints ASCII art of your system's logo as
well as a summary of system information.  UwUFetch's unique contribution is the
uwu-ification of various words used in the description.  For example, Guix
becomes gUwUix.")
    (license license:gpl3+)))

(define-public screenfetch
  (package
    (name "screenfetch")
    (version "3.9.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/KittyKatt/screenFetch")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "04l8aqr474pb115nagn9f6y48jw92n1qfszgw7dbhgl4mpn95lcr"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((source (assoc-ref %build-inputs "source"))
               (out    (assoc-ref %outputs "out")))
           (mkdir-p (string-append out "/bin/"))
           (copy-file (string-append source "/screenfetch-dev")
                      (string-append out "/bin/screenfetch"))
           (install-file (string-append source "/screenfetch.1")
                         (string-append out "/man/man1/"))
           (install-file (string-append source "/COPYING")
                         (string-append out "/share/doc/" ,name "-" ,version))
           (substitute* (string-append out "/bin/screenfetch")
             (("/usr/bin/env bash")
              (search-input-file %build-inputs "/bin/bash")))
           (wrap-program
               (string-append out "/bin/screenfetch")
             `("PATH" ":" prefix
               (,(string-append (assoc-ref %build-inputs "bc") "/bin:"
                                (assoc-ref %build-inputs "scrot") "/bin:"
                                (assoc-ref %build-inputs "xdpyinfo") "/bin"
                                (assoc-ref %build-inputs "xprop") "/bin"))))
           (substitute* (string-append out "/bin/screenfetch")
             (("#!#f")
              (string-append "#!"
                             (search-input-file %build-inputs
                                                "/bin/bash"))))))))
    (inputs
     (list bash bc scrot xdpyinfo xprop))
    (home-page "https://github.com/KittyKatt/screenFetch")
    (synopsis "System information script")
    (description "Bash screenshot information tool which can be used to
generate those nifty terminal theme information and ASCII distribution logos in
everyone's screenshots nowadays.")
    (license license:gpl3)))

(define-public ufetch
  (let ((commit "12b68fa35510a063582d626ccd1abc48f301b6b1")
        (revision "0"))
    (package
      (name "ufetch")
      (version "0.3")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://gitlab.com/jschx/ufetch.git")
                      (commit (string-append "v" version))))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0sv17zmvhp0vfdscs8yras7am10ah7rpfyfia608sx74k845bfyl"))))
      (build-system trivial-build-system)
      (inputs
       `(("bash" ,bash)
         ("tput" ,ncurses)))
      (arguments
       `(#:modules ((guix build utils))
         #:builder
         (begin
           (use-modules (guix build utils))
           (let* ((source (assoc-ref %build-inputs "source"))
                  (output (assoc-ref %outputs "out"))
                  (bindir (string-append output "/bin"))
                  (docdir (string-append output "/share/doc/ufetch-" ,version))
                  (tput   (search-input-file %build-inputs "/bin/tput")))
             (install-file (string-append source "/LICENSE") docdir)
             (setenv "PATH" (string-append (assoc-ref %build-inputs "bash") "/bin"))
             (mkdir-p bindir)
             (for-each (lambda (src)
                         (let ((dst (string-append bindir "/" (basename src))))
                           (copy-file src dst)
                           (patch-shebang dst)
                           (substitute* dst (("tput") tput))))
                       (find-files source "ufetch-[[:alpha:]]*$"))
             ;; Note: the `ufetch` we create below will only work if run under
             ;; the Guix System.  I.e. a user trying to run `ufetch` on a
             ;; foreign distro will not get great results.  The `screenfetch`
             ;; program does actual runtime detection of the operating system,
             ;; and would be a better choice in such a situation.
             (symlink "ufetch-guix" (string-append bindir "/ufetch"))))))
      (home-page "https://gitlab.com/jschx/ufetch")
      (synopsis "Tiny system info")
      (description "This package provides a tiny system info utility.")
      (license license:expat))))

(define-public pfetch
  (let ((commit "a906ff89680c78cec9785f3ff49ca8b272a0f96b")
        (revision "1"))
    (package
      (name "pfetch")
      (version (git-version "0.7.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/dylanaraps/pfetch")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1yhf8mxjn58gjfdii3bpn8522gfaicd8jxjxvmwi2jz7fgvp0zpn"))))
      (build-system trivial-build-system)
      (inputs (list bash))
      (arguments
       `(#:modules ((guix build utils))
         #:builder
         (begin
           (use-modules (guix build utils))
           (let* ((source (lambda (f)
                            (string-append (assoc-ref %build-inputs "source") "/" f)))
                  (output (assoc-ref %outputs "out"))
                  (docdir (string-append output "/share/doc/pfetch-" ,version)))
             (install-file (source "LICENSE.md") docdir)
             (install-file (source "README.md") docdir)
             (install-file (source "pfetch") (string-append output "/bin"))
             (patch-shebang
              (string-append output "/bin/pfetch")
              (list (string-append (assoc-ref %build-inputs "bash") "/bin")))))))
      (home-page "https://github.com/dylanaraps/pfetch")
      (synopsis "System information tool")
      (description "This package provides a simple, configurable system
information tool.")
      (license license:expat))))

(define-public fastfetch
  (package
    (name "fastfetch")
    (version "2.36.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/fastfetch-cli/fastfetch")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "13999l229v2awcl29y3fgijhg8hbk8i9gz6j31z7p9xhkrhn3y42"))
       (modules '((guix build utils)))
       (snippet '(begin
                   (delete-file-recursively "src/3rdparty")))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:configure-flags #~(list "-DENABLE_SYSTEM_YYJSON=ON"
                                "-DBUILD_FLASHFETCH=OFF"
                                "-DBUILD_TESTS=ON"
                                "-DINSTALL_LICENSE=OFF"
                                "-DBINARY_LINK_TYPE=dynamic"
                                "-DENABLE_DIRECTX_HEADERS=OFF"
                                (string-append "-DCUSTOM_PCI_IDS_PATH="
                                               #$(this-package-input "hwdata")
                                               "/share/hwdata/pci.ids")
                                (string-append "-DCUSTOM_AMDGPU_IDS_PATH="
                                               #$(this-package-input "libdrm")
                                               "/share/libdrm/amdgpu.ids"))))
    (inputs (list dbus
                  glib
                  hwdata
                  imagemagick
                  libdrm
                  libxcb
                  mesa
                  wayland
                  yyjson
                  zlib)) ;for imagemagick and an #ifdef
    (native-inputs (list pkg-config python-minimal))
    (home-page "https://github.com/fastfetch-cli/fastfetch")
    (synopsis "Display system information in a stylized manner")
    (description
     "Fastfetch is a tool for fetching system information and displaying it in
a stylized way.  Fastfetch displays this information next to a logo of the
system distribution, akin to many similar tools.")
    (license license:expat)))

(define-public nnn
  (package
    (name "nnn")
    (version "5.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/jarun/nnn/releases/download/v"
                           version "/nnn-v" version ".tar.gz"))
       (sha256
        (base32 "084m08fcnpjd8gdfvvmgz558lmc29wj7dxg23m98fdmvhp3dd0ms"))))
    (build-system gnu-build-system)
    (inputs
     (list ncurses readline))
    (native-inputs
     (list pkg-config))
    (arguments
     (list #:tests? #f                  ; no tests
           #:make-flags
           #~(list
              (string-append "PREFIX=" #$output)
              (string-append "CC=" #$(cc-for-target))
              (string-append "PKG_CONFIG=" #$(pkg-config-for-target)))
           #:phases
           #~(modify-phases %standard-phases
               (delete 'configure))))       ; no configure script
    (home-page "https://github.com/jarun/nnn")
    (synopsis "Terminal file browser")
    (description
     "@command{nnn} is a fork of @command{noice}, a fast and minimal text
terminal file browser with keyboard shortcuts for navigation, opening files and
running tasks.  There is no configuration file and MIME associations are
hard-coded.")
    (license license:bsd-2)))

(define-public thermald
  (package
    (name "thermald")
    (version "2.5.3")
    (source
     (origin
      (method git-fetch)
      (uri (git-reference
             (url "https://github.com/intel/thermal_daemon")
             (commit (string-append "v" version))))
      (file-name (git-file-name name version))
      (sha256
       (base32 "0hpk7pjlrnj62m5zdmih7gf3nihhyphyslh5xakdjb64cvx5z25d"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (let ((out (assoc-ref %outputs "out")))
         (list (string-append "--with-dbus-sys-dir="
                              out "/etc/dbus-1/system.d")
               "--localstatedir=/var"
               "--disable-werror"))
       #:make-flags
       (list "V=1")                     ; log build commands
       #:phases
       (modify-phases %standard-phases
         (add-before 'bootstrap 'no-early-./configure
           (lambda _
             (setenv "NO_CONFIGURE" "yet"))))))
    (native-inputs
     (list autoconf
           autoconf-archive
           automake
           `(,glib "bin") ; for glib-genmarshal, etc.
           gtk-doc/stable
           pkg-config))
    (inputs
     (list dbus-glib libevdev libxml2 upower xz))
    (home-page "https://01.org/linux-thermal-daemon/")
    (synopsis "CPU scaling for thermal management")
    (description "The Linux Thermal Daemon helps monitor and control temperature
on systems running the Linux kernel.")
    ;; arm and aarch64 don't have cpuid.h.
    (supported-systems '("i686-linux" "x86_64-linux"))
    (license license:gpl2)))

(define-public tcptrack
  (package
    (name "tcptrack")
    (version "1.4.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/bchretien/tcptrack")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "08lh3l67wn4kq9q0nfspc7rj0jvp9dzwjgxpvqliwcif8cy5mi45"))))
    (build-system gnu-build-system)
    (inputs (list libpcap ncurses))
    (synopsis "TCP connections sniffer")
    (description
     "Tcptrack is a sniffer which displays information about TCP connections
it sees on a network interface.  This is a fork of Steve Benson’s tcptrack.")
    (home-page "https://github.com/bchretien/tcptrack")
    (license license:lgpl2.1+)))

(define-public masscan
  (package
    (name "masscan")
    (version "1.0.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/robertdavidgraham/masscan")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0q0c7bsf0pbl8napry1qyg0gl4pd8wn872h4mz9b56dx4rx90vqg"))))
    (build-system gnu-build-system)
    (inputs
     (list libpcap))
    (arguments
     `(#:test-target "regress"
       #:make-flags
       (list (string-append "CC=" ,(cc-for-target))
             (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)            ; no ./configure script
         (add-after 'unpack 'patch-path
           (lambda* (#:key outputs inputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (pcap (assoc-ref inputs "libpcap")))
               (substitute* "src/rawsock-pcap.c"
                 (("libpcap.so") (string-append pcap "/lib/libpcap.so")))
               #t))))))
    (synopsis "TCP port scanner")
    (description "MASSCAN is an asynchronous TCP port scanner.  It can detect
open ports, and also complete the TCP connection and interact with the remote
application, collecting the information received.")
    (home-page "https://github.com/robertdavidgraham/masscan")
    ;; 'src/siphash24.c' is the SipHash reference implementation, which
    ;; bears a CC0 Public Domain Dedication.
    (license license:agpl3+)))

(define-public hungrycat
  (package
    (name "hungrycat")
    (version "0.4.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/jwilk/hungrycat/"
                                  "releases/download/" version "/"
                                  "hungrycat-" version ".tar.gz"))
              (sha256
               (base32
                "03fc1zsrf99lvxa7b4ps6pbi43304wbxh1f6ci4q0vkal370yfwh"))))
    (build-system gnu-build-system)
    (native-inputs
     ;; For tests.
     `(("python" ,python-wrapper)
       ("python-nose" ,python-nose)))
    (arguments
     `(#:test-target "test"))
    (synopsis "Single tool that combines @command{cat} & @command{rm}")
    (description
     "hungrycat prints the contents of a file to standard output, while
simultaneously freeing the disk space it occupied.  It is useful if you need
to process a large file, don't have enough space to store both the input and
output files, and don't need the input file afterwards.
While similar in principle to running @command{cat} immediately followed by
@command{rm}, @command{hungrycat} actually frees blocks as soon as they are
printed instead of after the entire file has been read, which is often too
late.")
    (home-page "https://jwilk.net/software/hungrycat")
    (license license:expat)))

(define-public launchmon
  (package
    (name "launchmon")
    (version "1.0.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/LLNL/LaunchMON/releases/download/v"
                    version "/launchmon-v" version ".tar.gz"))
              (sha256
               (base32
                "0fm3nd9mydm9v2bf7bh01dbgrfnpwkapxa3dsvy3x1z0rz61qc0x"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Fix build failure with GCC 7 due to a conversion error.
                  ;; Remove for versions > 1.0.2.
                  (substitute* "launchmon/src/linux/lmon_api/lmon_coloc_spawner.cxx"
                    ((" lmonpl = '\\\\0'")
                     " *lmonpl = '\\0'"))
                  #t))))
    (build-system gnu-build-system)
    (arguments
     ;; GCC 11 defaults to c++17 but this package needs something older.
     (list #:configure-flags #~'("CXXFLAGS=-std=c++14 -O2 -g")))
    (inputs
     (list openmpi
           munge
           boost
           libelf
           libgcrypt
           libgpg-error))
    (synopsis "Infrastructure for large-scale tool daemon launching")
    (description
     "LaunchMON is a software infrastructure that enables HPC run-time
tools to co-locate tool daemons with a parallel job.  Its API allows a
tool to identify all the remote processes of a job and to scalably
launch daemons into the relevant nodes.")
    (home-page "https://github.com/LLNL/LaunchMON")
    (supported-systems '("i686-linux" "x86_64-linux"))
    (license license:lgpl2.1)))

(define-public spindle
  (package
    (name "spindle")
    (version "0.13")
    (source (origin
              ;; We use git checkout to avoid github auto-generated tarballs
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/hpc/Spindle")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1z594nhash1him9v00qmyqv9jvikzrs4wxqy1cvnfwqwnrrkp707"))))
    (build-system gnu-build-system)
    (arguments '(#:configure-flags '("--enable-sec-launchmon"
                                     "--enable-sec-munge"
                                     "--enable-sec-none"
                                     ;; Fails to build as c++17.
                                     "CXXFLAGS=-std=c++14 -O2 -g")))
    (inputs
     (list openmpi munge launchmon libgcrypt))
    (synopsis "Scalable library loading in HPC environments")
    (description
     "Spindle is a tool for improving the performance of dynamic library and
Python loading in HPC environments.")
    (home-page "https://github.com/hpc/Spindle")
    ;; This package supports x86_64 and PowerPC64
    (supported-systems '("x86_64-linux"))
    (license license:lgpl2.1)))

(define-public inxi-minimal
  (let ((real-name "inxi"))
    (package
      (name "inxi-minimal")
      (version "3.3.31-2")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/smxi/inxi")
               (commit version)))
         (file-name (git-file-name real-name version))
         (sha256
          (base32 "1fca5minalpmizbxh5kmjiv8xrl7k6g91zn8d84fxmbhsk8vn3kk"))))
      (build-system trivial-build-system)
      (inputs
       (list bash-minimal
             perl
             procps))
      (native-inputs
       (list gzip))
      (arguments
       `(#:modules ((guix build utils))
         #:builder
         (begin
           (use-modules (guix build utils)
                        (ice-9 match)
                        (srfi srfi-26))
           (setenv "PATH" (string-join
                           (map (lambda (file)
                                  (dirname (search-input-file %build-inputs
                                                              file)))
                                (list "bin/bash"
                                      "bin/gzip"
                                      "bin/perl"))
                           ":"))
           (copy-recursively (assoc-ref %build-inputs "source")
                             ,(string-append real-name "-" version))
           (with-directory-excursion ,(string-append real-name "-" version)
             (with-fluids ((%default-port-encoding #f))
               (substitute* "inxi" (("/usr/bin/env perl") (which "perl"))))
             (let ((bin (string-append %output "/bin")))
               (install-file "inxi" bin)
               (wrap-program (string-append bin "/inxi")
                 `("PATH" ":" =
                   ("$PATH"
                    ,@(map (lambda (input)
                             (match input
                               ((name . store)
                                (let ((store-append
                                       (cut string-append store <>)))
                                  (cond
                                   ((member name '("util-linux"))
                                    (string-append (store-append "/bin") ":"
                                                   (store-append "/sbin")))
                                   ((member name '("dmidecode" "iproute2"))
                                    (store-append "/sbin"))
                                   (else (store-append "/bin")))))))
                           %build-inputs)))
                 `("PERL5LIB" ":" =
                   ,(delete
                     ""
                     (map (match-lambda
                            (((? (cut string-prefix? "perl-" <>) name) . dir)
                             (string-append dir "/lib/perl5/site_perl"))
                            (_ ""))
                          %build-inputs)))))
             (invoke "gzip" "-n" "inxi.1")
             (install-file "inxi.1.gz"
                           (string-append %output "/share/man/man1"))))))
      (home-page "https://smxi.org/docs/inxi.htm")
      (synopsis "Full-featured system information script")
      (description "Inxi is a system information script that can display
various things about your hardware and software to users in an IRC chatroom or
support forum.  It runs with the @code{/exec} command in most IRC clients.")
      (license license:gpl3+))))

(define-public inxi
  (package
    (inherit inxi-minimal)
    (name "inxi")
    (inputs
     `(("dmidecode" ,dmidecode)
       ("file" ,file)
       ("bind:utils" ,isc-bind "utils") ; dig
       ("gzip" ,gzip)
       ("iproute2" ,iproute)            ; ip
       ("kmod" ,kmod)                   ; modinfo
       ("lm-sensors" ,lm-sensors)
       ("mesa-utils" ,mesa-utils)
       ("pciutils" ,pciutils)
       ("tar" ,tar)
       ("tree" ,tree)
       ("util-linux" ,util-linux)       ; lsblk
       ("usbutils" ,usbutils)           ; lsusb
       ("wmctrl" ,wmctrl)
       ("xdpyinfo" ,xdpyinfo)
       ("xprop" ,xprop)
       ("xrandr" ,xrandr)
       ("coreutils" ,coreutils)         ; uptime
       ("inetutils" ,inetutils)         ; ifconfig
       ("perl-cpanel-json-xs" ,perl-cpanel-json-xs)
       ("perl-http-tiny" ,perl-http-tiny)
       ("perl-io-socket-ssl" ,perl-io-socket-ssl)
       ("perl-json-xs" ,perl-json-xs)
       ("perl-time-hires" ,perl-time-hires)
       ("lvm2" ,lvm2)                   ; lvs
       ("mdadm" ,mdadm)
       ;; TODO: Add more inputs:
       ;; ipmi-sensors
       ;; hddtemp
       ;; perl-xml-dumper
       ;; ipmitool
       ,@(package-inputs inxi-minimal)))))

(define-public pscircle
  (package
    (name "pscircle")
    (version "1.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/mildlyparallel/pscircle.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1sm99423hh90kr4wdjqi9sdrrpk65j2vz2hzj65zcxfxyr6khjci"))))
    (build-system meson-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list cairo libpng libx11))
    (home-page "https://gitlab.com/mildlyparallel/pscircle")
    (synopsis "Visualize Linux processes in a form of radial tree")
    (description
     "@code{pscircle} visualizes Linux processes in the form of a radial tree.")
    (license license:gpl2+)))

(define-public python-pyudev
  (package
    (name "python-pyudev")
    (version "0.22.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "pyudev" version))
        (sha256
          (base32
            "0xmj6l08iih2js9skjqpv4w7y0dhxyg91zmrs6v5aa65gbmipfv9"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f ; Tests require /sys
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-ctypes-udev
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((eudev (assoc-ref inputs "eudev")))
               (substitute* "src/pyudev/core.py"
                (("'udev'")
                 (string-append "'" eudev "/lib/libudev.so'")))
               (substitute* "src/pyudev/_ctypeslib/utils.py"
                ;; Use absolute paths instead of keys.
                (("= find_library") "= "))
               #t))))))
    (inputs
     (list eudev))
    (propagated-inputs
     (list python-six))
    (native-inputs
     (list python-docutils python-hypothesis python-mock python-pytest
           python-sphinx))
    (home-page "https://pyudev.readthedocs.io/")
    (synopsis "Python udev binding")
    (description "This package provides @code{udev} bindings for Python.")
    (license license:lgpl2.1)))

(define-public vmtouch
  (package
    (name "vmtouch")
    (version "1.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hoytech/vmtouch/")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "08da6apzfkfjwasn4dxrlfxqfx7arl28apdzac5nvm0fhvws0dxk"))))
    (build-system gnu-build-system)
    (native-inputs
     (list perl))
    (arguments
     `(#:tests? #f                      ; no tests
       #:make-flags
       (list
        (string-append "CC=" ,(cc-for-target))
        (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (home-page "https://github.com/hoytech/vmtouch/")
    (synopsis "Portable file system cache diagnostics and control")
    (description
     "vmtouch is a tool for learning about and controlling the file system
cache of unix and unix-like systems.")
    (license license:bsd-3)))

(define-public solaar
  (package
    (name "solaar")
    (version "1.1.13")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/pwr-Solaar/Solaar")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1fz3qgjx3ygr4clgh7iryxgvvjy510rgy8ixr2xld2wr0xa6p0mi"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'setenv-PATH
           (lambda _
             (setenv "PYTHONPATH" "lib"))))))
    (native-inputs (list python-pytest))
    (propagated-inputs
     (list python-pygobject
           python-pyudev
           python-dbus-python
           python-evdev
           ;; For GUI.
           python-pyyaml
           python-psutil
           python-xlib
           gtk+
           python-pygobject))
    (home-page "https://pwr-solaar.github.io/Solaar/")
    (synopsis "Linux devices manager for the Logitech Unifying Receiver")
    (description "This package provides tools to manage clients of the
Logitech Unifying Receiver.")
    (license license:gpl2)))

(define-public lynis
  (package
    (name "lynis")
    ;; Also update the ‘lynis-sdk’ input to the commit matching this release.
    (version "3.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/CISOfy/lynis")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "05bh16i916xz9w8p8fz8flzj9ayyzg7wpbi7q61ylrlahhc03nqd"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Remove proprietary plugins. As of now, all plugins supplied with
           ;; lynis are proprietary. In the future, if free plugins are
           ;; provided, whitelist them from deletion.
           (for-each delete-file (find-files "plugins"))))))
    (build-system gnu-build-system)
    (native-inputs
     `(;; For tests
       ("lynis-sdk"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/CISOfy/lynis-sdk")
                 (commit "f4f885f1f049f59940487a6ffc2d53806c729d12")))
           (file-name (git-file-name "lynis-sdk" version))
           (sha256
            (base32 "09d98wmvan7nlchm056kls5xm939d1231pwsvlp4q2aznz8cmg42"))))))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (replace 'unpack
            ;; XXX Remove after fixing <https://issues.guix.gnu.org/55287>.
            (lambda* (#:key source #:allow-other-keys)
              (mkdir "source")
              (chdir "source")
              (copy-recursively source "."
                                #:keep-mtime? #t)))
          (replace 'configure
            (lambda _
              (substitute* "lynis"
                (("/usr/share/lynis")
                 (string-append #$output "/share/lynis")))
              (substitute* "include/functions"
                (("/usr/local/etc/lynis")
                 (string-append #$output "/etc/lynis")))))
          (delete 'build)
          (replace 'install
            (lambda _
              (install-file "lynis" (string-append #$output "/bin/"))
              (install-file "default.prf" (string-append #$output "/etc/lynis"))
              (for-each
               (lambda (dir)
                 (copy-recursively
                  dir (string-append #$output "/share/lynis/" dir)))
               (list "db" "include" "plugins"))
              (install-file "lynis.8"
                            (string-append #$output "/share/man/man8"))))
          (replace 'check
            (lambda _
              (copy-recursively #$(this-package-native-input "lynis-sdk")
                                "../lynis-sdk")
              (setenv "LANG" "en_US.UTF-8")
              (let ((lynis-dir (getcwd)))
                (with-directory-excursion "../lynis-sdk"
                  (substitute* "config"
                    (("\\.\\./lynis") lynis-dir))
                  (substitute* "unit-tests/tests-language-translations.sh"
                    (("\\.\\./lynis") lynis-dir))
                  (invoke "sh" "lynis-devkit" "run" "unit-tests"))))))))
    (home-page "https://cisofy.com/lynis/")
    (synopsis "Security auditing tool")
    (description "Lynis is a security auditing tool.  It performs an in-depth
security scan and runs on the system itself.  The primary goal is to test
security defenses and provide tips for further system hardening.  It will also
scan for general system information, vulnerable software packages, and
possible configuration issues.")
    (license license:gpl3+)))

(define-public ngrep
  (package
    (name "ngrep")
    (version "1.47")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jpr5/ngrep/")
             (commit (string-append "V" (string-replace-substring version "." "_")))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1x2fyd7wdqlj1r76ilal06cl2wmbz0ws6i3ys204sbjh1cj6dcl7"))))
    (build-system gnu-build-system)
    (inputs
     (list libpcap))
    (arguments
     `(#:tests? #f ;; No tests.
       #:configure-flags (list (string-append "--with-pcap-includes="
                                              (assoc-ref %build-inputs "libpcap")
                                              "/include/pcap"))))
    (home-page "https://github.com/jpr5/ngrep/")
    (synopsis "Grep-like utility to search for network packets on an interface")
    (description "@command{ngrep} is like GNU grep applied to the network
layer.  It's a PCAP-based tool that allows you to specify an extended regular
or hexadecimal expression to match against data payloads of packets.  It
understands many kinds of protocols, including IPv4/6, TCP, UDP, ICMPv4/6,
IGMP and Raw, across a wide variety of interface types, and understands BPF
filter logic in the same fashion as more common packet sniffing tools, such as
tcpdump and snoop.")
    (license license:bsd-3)))

(define-public pam-mount
  (package
    (name "pam-mount")
    (version "2.20")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://inai.de/files/pam_mount/pam_mount-"
                           version ".tar.xz"))
       (sha256
        (base32 "1vbc6fd826qgj5qq5g06hc64x6n372xhb92bfvhzi02n91x209jl"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list (string-append "--with-slibdir=" #$output "/lib")
              (string-append "--with-ssbindir=" #$output "/sbin")
              "--with-cryptsetup")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-file-names
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let ((out (assoc-ref outputs "out")))
                (substitute* "src/mtcrypt.c"
                  (("\"(mount|umount)\";" _ command)
                   (format #f "\"~a\";"
                           (search-input-file inputs
                                              (string-append "bin/" command))))
                  (("\"(fsck)\"," _ command)
                   (format #f "\"~a\","
                           (search-input-file inputs
                                              (string-append "sbin/" command)))))
                (substitute* "src/rdconf1.c"
                  (("\"(mount|umount)\", \"" _ command)
                   (format #f "\"~a\", \""
                           (search-input-file inputs
                                              (string-append "bin/" command))))
                  (("\"(fsck)\", \"" _ command)
                   (format #f "\"~a\", \""
                           (search-input-file inputs
                                              (string-append "sbin/" command))))
                  (("\"pmvarrun\", \"")
                   (format #f "\"~a/sbin/pmvarrun\", \"" out)))))))))
    (native-inputs
     (list perl pkg-config))
    (inputs
     (append
      (cons cryptsetup (libcryptsetup-propagated-inputs))
      (list libhx
            libxml2
            linux-pam
            openssl
            pcre2
            util-linux
            eudev)))
    (home-page "https://inai.de/projects/pam_mount/")
    (synopsis "PAM module to mount volumes for a user session")
    (description
     "Pam-mount is a PAM module to mount volumes when a user logs in.
It can mount all local file systems supported by @command{mount}, as well as
LUKS volumes encrypted with the user's log-in password.")
    (license (list license:gpl2+ license:lgpl2.1+))))

(define-public pam-uaccess
  (let ((commit "54fbf043c63cc500b4850b0b4a12ea14078f2b53")
        (revision "0"))
    (package
      (name "pam-uaccess")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://git.sr.ht/~kennylevinsen/pam_uaccess")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "08068cw4nvcanym8b5dyccnnb3qc3f09pbvi6fcfiz227yx73npc"))))
      (build-system meson-build-system)
      (native-inputs (list pkg-config))
      (inputs (list acl eudev linux-pam))
      (home-page "https://git.sr.ht/~kennylevinsen/pam_uaccess")
      (synopsis
       "PAM module that grants access to devices tagged @code{uaccess} in udev")
      (description
       "@code{pam_uaccess} is a PAM module that grants access to devices tagged
@code{uaccess} in udev for the duration of the users' session, replacing
elogind's uaccess feature.")
      (license license:expat))))

(define-public jc
  (package
    (name "jc")
    (version "1.25.4")
    (source
     (origin
       ;; The PyPI tarball lacks the test suite.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/kellyjonbrazil/jc")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0lb7608h3vaw5gqlaf1ryd84m2mirfl7gdnhzadrjlh6h8b3lkgp"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               ;; XXX Guix's America/Los_Angeles time zone is somehow broken.
               (add-before 'check 'hack-time-zone
                 (lambda _
                   (setenv "TZ" "PST8PDT")
                   (substitute* (find-files "tests" "^test.*\\.py$")
                     (("America/Los_Angeles") "PST8PDT")))))))
    (native-inputs
     (list python-pytest
           python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-pygments
           python-ruamel.yaml
           python-xmltodict))
    (home-page "https://github.com/kellyjonbrazil/jc")
    (synopsis "Convert the output of command-line tools to JSON")
    (description "@code{jc} JSONifies the output of many CLI tools and
file-types for easier parsing in scripts.")
    (license license:expat)))

(define-public jtbl
  (package
    (name "jtbl")
    (version "1.1.7")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/kellyjonbrazil/jtbl")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "19i21fqz2m40cds9pb17brjxkczqagmx2f7mfb0xdvbygaply5wz"))))
    (build-system python-build-system)
    (inputs
     (list python-tabulate))
    (home-page "https://github.com/kellyjonbrazil/jtbl")
    (synopsis "Command-line tool to print JSON data as a table in the terminal")
    (description "@code{jtbl} accepts piped JSON data from stdin and outputs a
text table representation to stdout.")
    (license license:expat)))

(define-public hosts
  (package
    (name "hosts")
    (version "3.6.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/xwmx/hosts")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1ni4z89kxzgwm26hhx908g04f2h0fypy7lgfa0rvsz8d0wslgcsn"))))
    (build-system trivial-build-system)
    (inputs
     `(("bats" ,bats) ;for test
       ("awk" ,gawk)
       ("bash" ,bash)
       ("coreutils" ,coreutils)
       ("diffutils" ,diffutils)
       ("grep" ,grep)
       ("ncurses" ,ncurses) ;tput
       ("sed" ,sed)))
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         ;; copy source
         (copy-recursively (assoc-ref %build-inputs "source") ".")
         ;; patch-shebang phase
         (setenv "PATH"
                 (string-append (assoc-ref %build-inputs "bash") "/bin"
                                ":" (assoc-ref %build-inputs "awk") "/bin"
                                ":" (assoc-ref %build-inputs "coreutils") "/bin"
                                ":" (assoc-ref %build-inputs "diffutils") "/bin"
                                ":" (assoc-ref %build-inputs "grep") "/bin"
                                ":" (assoc-ref %build-inputs "ncurses") "/bin"
                                ":" (assoc-ref %build-inputs "sed") "/bin"
                                ":" "/run/privileged/bin"
                                ":" (getenv "PATH")))
         (substitute* "hosts"
           (("#!/usr/bin/env bash")
            (string-append "#!" (which "bash")
                           "\nPATH=" (getenv "PATH"))))
         ;; check phase
         (setenv "TERM" "linux") ;set to tty for test
         (invoke (search-input-file %build-inputs "/bin/bats")
                 "test")
         ;; install phase
         (install-file "hosts" (string-append %output "/bin"))
         (let ((bash-completion
                (string-append %output "/etc/bash_completion.d")))
           (mkdir-p bash-completion)
           (copy-file "etc/hosts-completion.bash"
                      (string-append bash-completion "/hosts")))
         (let ((zsh-completion
                (string-append %output "/share/zsh/site-functions")))
           (mkdir-p zsh-completion)
           (copy-file "etc/hosts-completion.zsh"
                      (string-append zsh-completion "/_hosts")))
         (let ((doc (string-append %output "/share/doc/" ,name "-" ,version)))
           (mkdir-p doc)
           (install-file "LICENSE" doc)
           (install-file "README.md" doc))
         #t)))
    (home-page "https://github.com/xwmx/hosts/")
    (synopsis "Script for editing a foreign distro's @file{/etc/hosts} file")
    (description "Hosts is a command line program for managing
@file{/etc/hosts} entries.  On Guix System, @file{/etc/hosts} is managed from
the system configuration; hosts only works when using the Guix package manager
on a foreign distro.  @command{hosts} works with existing hosts files and
entries, providing commands to add, remove, comment, and search.")
    (license license:expat)))

(define-public nmrpflash
  (package
    (name "nmrpflash")
    (version "0.9.21")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/jclehner/nmrpflash")
         (commit (string-append "v" version))))
       (sha256
        (base32 "183nvxqdn8klin5f14f4cv9vjymj0izy0qmj1l76igmlcq7ravwx"))
       (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f                       ; none exist
      #:make-flags
      #~(list (string-append "CC=" #$(cc-for-target))
              (string-append "PREFIX=" #$output))
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (add-before 'install 'prepare-install
            (lambda _
              (mkdir-p (string-append #$output "/bin")))))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list libnl libpcap))
    (home-page "https://github.com/jclehner/nmrpflash")
    (synopsis "Reflash (``unbrick'') Netgear devices with corrupted firmware")
    (description "This package provides a utility to flash a new firmware
image to a Netgear device.  It has been tested on Netgear EX2700, EX6120,
EX6150v2, DNG3700v2, R6100, R6220, R7000, D7000, WNR3500, R6400, R6800,
R8000, R8500, WNDR3800, but is likely to be compatible with many other
Netgear devices.")
    (license license:gpl3+)))

(define-public atop
  (package
    (name "atop")
    (version "2.11.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.atoptool.nl/download/atop-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "083fckjn2s3276fqyjb3rcwqrws7qc3fgk1f82zzgzrfc1kcd54v"))
              (snippet
               ;; The 'mkdate' script generates a new 'versdate.h' header
               ;; containing the build date.  That makes builds
               ;; non-reproducible so remove it.
               #~(delete-file "mkdate"))))
    (build-system gnu-build-system)
    (arguments
     (list
       #:tests? #f              ; no test suite
       #:make-flags
       #~(list (string-append "CC=" #$(cc-for-target))
               (string-append "DESTDIR=" #$output)
               (string-append "SYSDPATH=/etc/systemd/system")
               (string-append "PMPATHD=/etc/systemd/system-sleep")
               ;; Or else it tries to create /var/log/atop...
               (string-append "LOGPATH="))
       #:phases
       #~(modify-phases %standard-phases
           (delete 'configure)  ; No ./configure script
           (add-before 'build 'patch-build
             (lambda _
               (substitute* "Makefile"
                 ;; Don't use /usr as a prefix.
                 (("/usr") "")
                 ;; Otherwise, it creates a blank configuration file as a "default".
                 (("touch.*DEFPATH)/atop") "")
                 (("chmod.*DEFPATH)/atop") "")))))))
    (native-inputs (list pkg-config))
    (inputs
     (list glib
           ncurses
           python-wrapper       ; for `atopgpud`
           zlib))
    (home-page "https://www.atoptool.nl/")
    (synopsis "Linux performance monitoring console")
    (description "Atop is an ASCII full-screen performance monitor for Linux
that is capable of reporting the activity of all processes (even processes have
finished during the monitoring interval), daily logging of system and process
activity for long-term analysis, highlighting overloaded system resources by
using colors, etc.  At regular intervals, it shows system-level activity related
to the CPU, memory, swap, disks (including LVM) and network layers, and for
every process (and thread) it shows e.g. the CPU utilization, memory growth,
disk utilization, priority, username, state, and exit code.")
    (properties
     `((release-monitoring-url . "https://www.atoptool.nl/downloadatop.php")))
    (license license:gpl2+)))

;; TODO: Pack u-root for: forth, and some tests.
(define-public fiano
   (package
     (name "fiano")
    ;; The versioning count has been changed since commit <2021-12-01>
    ;; 1eb599564549691603589219c2be34f966a32ff1.
    (version "1.2.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/linuxboot/fiano.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0s5fx4lhgb68qbx4ql34rcm678qdf0c4xl97bgc8dx9xwwqifza1"))))
    (build-system go-build-system)
    (arguments
     (list
      #:install-source? #f
      #:import-path "github.com/linuxboot/fiano"
      #:phases
      #~(modify-phases %standard-phases
          ;; XXX: Replace this part when it's implemented in go-build-system.
          (replace 'build
            (lambda* (#:key import-path #:allow-other-keys)
              (for-each
               (lambda (cmd)
                 (invoke "go" "build" "-v" "-x" "-ldflags=-s -w" "-trimpath"
                         (string-append import-path "/cmds/" cmd)))
               (list "cbfs"
                     "create-ffs"
                     "fittool"
                     "fmap"
                     "fspinfo"
                     "glzma"
                     "guid2english"
                     "microcode"
                     "utk"))))
          (replace 'check
            (lambda* (#:key import-path tests? #:allow-other-keys)
              (when tests?
                (for-each
                 (lambda (dir)
                   (invoke "go" "test" "-v"
                           (string-append import-path dir "/...")))
                 (list "/pkg/bytes"
                       "/pkg/amd"
                       "/pkg/cbfs"
                       "/pkg/compression"
                       "/pkg/fmap"
                       "/pkg/fsp"
                       "/pkg/guid"
                       "/pkg/guid2english"
                       "/pkg/intel"
                       "/pkg/knownguids"
                       "/pkg/log"
                       "/pkg/uefi"
                       "/pkg/unicode"
                       "/pkg/utk"
                       "/pkg/visitors"
                       "/cmds/cbfs"
                       "/cmds/create-ffs"
                       ;; TODO: Not packed yet in Guix, long journey:
                       ;; - github.com/u-root/u-root
                       ;;
                       ;; "/cmds/fmap"
                       ;; "/cmds/fittool"
                       "/cmds/fspinfo"
                       "/cmds/glzma"
                       "/cmds/guid2english"
                       "/cmds/microcode"
                       "/cmds/utk")))))
          (replace 'install
            (lambda _
              (let ((bindir (string-append #$output "/bin")))
                (for-each
                 (lambda (cmd)
                   (install-file cmd bindir))
                 (list "cbfs"
                       "create-ffs"
                       "fittool"
                       "fmap"
                       "fspinfo"
                       "glzma"
                       "guid2english"
                       "microcode"
                       "utk"))))))))
    (inputs
     (list go-github-com-dustin-go-humanize
           go-github-com-fatih-camelcase
           go-github-com-hashicorp-go-multierror
           go-github-com-jedib0t-go-pretty-v6
           go-github-com-jessevdk-go-flags
           go-github-com-klauspost-compress
           go-github-com-pierrec-lz4
           go-github-com-spf13-pflag
           go-github-com-stretchr-testify
           go-github-com-tjfoc-gmsm
           go-github-com-ulikunitz-xz
           go-github-com-xaionaro-go-bytesextra
           go-github-com-xaionaro-gosrc
           go-golang-org-x-text))
    (home-page "https://github.com/linuxboot/fiano")
    (synopsis "UEFI image editor")
    (description
     "This package provides a command-line UEFI image editor, including cbfs,
create-ffs, fmap, fspinfo, glzma, guid2english, microcode and utk CLI
utilities.")
    (license license:bsd-3)))

(define-public novena-eeprom
  (package
    (name "novena-eeprom")
    (version "2.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/xobs/novena-eeprom.git")
                    (commit (string-append "v" version))))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "00pd71mg0g20v0820ggp3ghf9nyj5s4wavaz9mkmrmsr91hcnf7i"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f   ; No tests exist
       #:make-flags
       (list (string-append "CC=" ,(cc-for-target)))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (out-bin (string-append out "/bin"))
                    (out-share-man (string-append out "/share/man/man8")))
               (install-file "novena-eeprom" out-bin)
               (install-file "novena-eeprom.8" out-share-man)))))))
    (inputs
     (list i2c-tools-3))
    (synopsis "Novena EEPROM editor")
    (description "This package provides an editor for the Novena EEPROM.
Novena boards contain a device-dependent descriptive EEPROM that defines
various parameters such as serial number, MAC address, and featureset.
This program allows you to view and manipulate this EEPROM list.")
    (home-page "https://github.com/xobs/novena-eeprom/")
    (supported-systems '("armhf-linux"))
    (license license:bsd-3)))

(define-public lrzsz
  (package
    (name "lrzsz")
    (version "0.12.20")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.ohse.de/uwe/releases/lrzsz-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1wcgfa9fsigf1gri74gq0pa7pyajk12m4z69x7ci9c6x9fqkd2y2"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (setenv "CONFIG_SHELL" (which "bash"))
             (invoke "./configure"
              (string-append "--prefix="
                             (assoc-ref outputs "out"))))))))
    (synopsis "Implementation of XMODEM/YMODEM/ZMODEM transfer protocols")
    (description "This package provides programs that transfer files using
the XMODEM/YMODEM/ZMODEM file transfer protocols.")
    (home-page "https://ohse.de/uwe/software/lrzsz.html")
    (license license:gpl2+)))

(define-public nq
  (package
    (name "nq")
    (version "0.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/leahneukirchen/nq")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0szbndvwl6ghwjzb165s09977s0r1yq8h0ziknm0h6xndkcjspl3"))))
    (build-system gnu-build-system)
    (native-inputs
     (list perl))
    (arguments
     (list #:make-flags
           #~(list (string-append "CC=" #$(cc-for-target))
                   (string-append "PREFIX=" #$output))
           #:phases
           #~(modify-phases %standard-phases
               (delete 'configure))))
    (synopsis "Unix command line queue utility")
    (description
     "@code{nq} can create very lightweight job queue systems which require no
setup, maintenance, supervision, or any long-running processes.")
    (home-page "https://github.com/leahneukirchen/nq")
    (license license:public-domain)))

(define-public lsofgraph
  (let ((commit "1d414bdc727c00a8c6cbfffc3c43128c60d6f0de")
        (revision "1"))
    (package
      (name "lsofgraph")
      (version (git-version "0.0.1" revision commit)) ;no upstream release
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/zevv/lsofgraph")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "058x04yp6bc77hbl3qchqm7pa8f9vqfl9jryr88m8pzl7kvpif54"))))
      (build-system trivial-build-system)
      (inputs
       (list lua))
      (arguments
       `(#:modules ((guix build utils))
         #:builder
         (begin
           (use-modules (guix build utils))
           ;; copy source
           (copy-recursively (assoc-ref %build-inputs "source") ".")
           ;; patch-shebang phase
           (setenv "PATH"
                   (string-append (assoc-ref %build-inputs "lua") "/bin"
                                  ":" (getenv "PATH")))
           (substitute* "lsofgraph"
             (("#!/usr/bin/env lua")
              (string-append "#!" (which "lua"))))
           ;; install phase
           (install-file "lsofgraph" (string-append %output "/bin"))
           (let ((doc (string-append
                       %output "/share/doc/" ,name "-" ,version)))
             (mkdir-p doc)
             (install-file "LICENSE" doc)
             (install-file "README.md" doc))
           #t)))
      (home-page "https://github.com/zevv/lsofgraph")
      (synopsis "Convert @code{lsof} output to @code{graphviz}")
      (description "Utility to convert @code{lsof} output to a graph showing
FIFO and UNIX interprocess communication.")
      (license license:bsd-2))))

(define-public runitor
  (package
    (name "runitor")
    (version "1.3.0-build.4")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/bdd/runitor")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "00l83jcjmf5kcq8yzq575kk6ljkkr2xhm5cx27zzb1yhxn93xj7n"))))
    (build-system go-build-system)
    (arguments
     `(#:unpack-path "bdd.fi/x/runitor"
       #:build-flags '(,(string-append "-ldflags=-X main.Version=" version))
       #:import-path "bdd.fi/x/runitor/cmd/runitor"
       #:install-source? #f))
    (home-page "https://github.com/bdd/runitor")
    (synopsis "Command runner with healthchecks.io integration")
    (description
      "Runitor runs the supplied command, captures its output, and based on its
exit code reports successful or failed execution to
@url{https://healthchecks.io,https://healthchecks.io} or your private instance.")
    (license license:bsd-0)))

(define-public udpcast
  (package
    (name "udpcast")
    (version "20211207")
    (source
     (origin
       (method url-fetch)
       ;; XXX: Original server is at https://www.udpcast.linux.lu is not
       ;; reliable.
       (uri (list (string-append
                   "https://fossies.org/linux/privat/udpcast-"
                   version ".tar.gz")
                  (string-append
                   "https://www.udpcast.linux.lu/download/udpcast-"
                   version ".tar.gz")))
       (sha256
        (base32 "0l6hck694szrrvz85nm48rwb7mzvg2z2bwa50v51pkvym3kvxkm3"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake m4 perl))
    (arguments `(#:tests? #f))                    ;no test suite
    (synopsis "Multicast file transfer tool")
    (description
     "UDPcast is a file transfer tool that can send data simultaneously to
many destinations on a LAN.  This can for instance be used to install entire
classrooms of PC's at once.  The advantage of UDPcast over using other
methods (nfs, ftp, whatever) is that UDPcast uses UDP's multicast abilities:
it won't take longer to install 15 machines than it would to install just 2.")
    (home-page "https://www.udpcast.linux.lu")
    (license license:gpl2+)))

(define-public greetd
  (package
    (name "greetd")
    (version "0.10.3")
    (home-page "https://git.sr.ht/~kennylevinsen/greetd")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32 "1j3c7skby9scsq6p1f6nacbiy9b26y1sswchdsp8p3vv7fgdh2wf"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-async-trait" ,rust-async-trait-0.1)
        ("rust-enquote" ,rust-enquote-1)
        ("rust-getopts" ,rust-getopts-0.2)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-nix" ,rust-nix-0.27)
        ("rust-pam-sys" ,rust-pam-sys-0.5)
        ("rust-rpassword" ,rust-rpassword-5)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-thiserror" ,rust-thiserror-1)
        ("rust-tokio" ,rust-tokio-1))
       #:install-source? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-/bin/sh
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "greetd/src/session/worker.rs"
               (("/bin/sh") (search-input-file inputs "/bin/sh")))))
         (add-after 'build 'build-man-pages
           (lambda* (#:key inputs #:allow-other-keys)
             (define (scdoc-cmd doc lvl)
               (system (string-append "scdoc < "
                                      doc "-" lvl ".scd > "
                                      doc "." lvl)))
             (with-directory-excursion "man"
               (scdoc-cmd "greetd" "1")
               (scdoc-cmd "greetd" "5")
               (scdoc-cmd "greetd-ipc" "7")
               (scdoc-cmd "agreety" "1"))))
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (sbin (string-append out "/sbin"))
                    (share (string-append out "/share"))
                    (man (string-append share "/man"))
                    (man1 (string-append man "/man1"))
                    (man5 (string-append man "/man5"))
                    (man7 (string-append man "/man7"))
                    (release ,(if (%current-target-system)
                                  (string-append
                                    "target/"
                                    (platform-rust-target
                                      (lookup-platform-by-target
                                        (%current-target-system)))
                                    "/release")
                                  "target/release"))
                    (greetd-bin (string-append release "/greetd"))
                    (agreety-bin (string-append release "/agreety")))
               (install-file greetd-bin sbin)
               (install-file agreety-bin bin)
               (with-directory-excursion "man"
                 (install-file "greetd.1" man1)
                 (install-file "greetd.5" man5)
                 (install-file "greetd-ipc.7" man7)
                 (install-file "agreety.1" man1))))))))
    (inputs
     ;; Full bash, not bash-minimal.  See https://issues.guix.gnu.org/76105.
     (list bash linux-pam))
    (native-inputs
     (list scdoc))
    (synopsis "Minimal and flexible login manager daemon")
    (description
     "greetd is a minimal and flexible login manager daemon
that makes no assumptions about what you want to launch.

If you can run it from your shell in a TTY, greetd can start it.

If it can be taught to speak a simple JSON-based IPC protocol,
then it can be a greeter.")
    (license license:gpl3+)))

(define-public greetd-pam-mount
  (package
    (inherit pam-mount)
    (name "greetd-pam-mount")
    (arguments
     (substitute-keyword-arguments (package-arguments pam-mount)
       ((#:configure-flags flags ''())
        #~(cons* "--with-rundir=/run/greetd" #$flags))
       ((#:phases phases)
        #~(modify-phases #$phases
           (add-after 'unpack 'patch-config-file-name
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "src/pam_mount.c"
                 ((".*define CONFIGFILE .*$")
                  "#define CONFIGFILE \"/etc/security/greetd_pam_mount.conf.xml\"\n")
                 (("pam_mount_config") "greetd_pam_mount_config")
                 (("pam_mount_system_authtok") "greetd_pam_mount_system_authtok"))))))))
    (synopsis "PAM module to mount volumes for a user session (greetd variant)")
    (description
     "Pam-mount is a PAM module to mount volumes when a user logs in.
It can mount all local file systems supported by @command{mount}, as well as
LUKS volumes encrypted with the user's log-in password.

This package inherits pam-mount but is compiled specifically for use with
the @command{greetd} log-in manager.  It uses a different configuration
location and PAM name space from the original.

This allows greetd-pam-mount to auto-(un)mount @env{XDG_RUNTIME_DIR} without
interfering with any pam-mount configuration.")))

(define-public wlgreet
  (package
    (name "wlgreet")
    (version "0.5.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://git.sr.ht/~kennylevinsen/wlgreet")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0d7lfx5jg2w7fp7llwrirnbsp27nv74f21mhrspd9ilk2cacf12d"))))
    (build-system cargo-build-system)
    (arguments
     (list #:cargo-inputs
           `(("rust-chrono" ,rust-chrono-0.4)
             ("rust-getopts" ,rust-getopts-0.2)
             ("rust-greetd-ipc" ,rust-greetd-ipc-0.10)
             ("rust-lazy-static" ,rust-lazy-static-1)
             ("rust-memmap2" ,rust-memmap2-0.3)
             ("rust-nix" ,rust-nix-0.25)
             ("rust-os-pipe" ,rust-os-pipe-1)
             ("rust-rusttype" ,rust-rusttype-0.9)
             ("rust-serde" ,rust-serde-1)
             ("rust-toml" ,rust-toml-0.5)
             ("rust-wayland-client" ,rust-wayland-client-0.29)
             ("rust-smithay-client-toolkit" ,rust-smithay-client-toolkit-0.15)
             ("rust-wayland-protocols" ,rust-wayland-protocols-0.29))
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'remove-bundled-fonts
                 (lambda _
                   (delete-file-recursively "fonts")))
               (add-after 'remove-bundled-fonts 'fix-font-references
                 (lambda* (#:key inputs #:allow-other-keys)
                   (substitute* "src/draw.rs"
                     (("\\.\\./fonts/dejavu/DejaVuSansMono\\.ttf")
                      (search-input-file
                       inputs
                       "share/fonts/truetype/DejaVuSansMono.ttf"))
                     (("\\.\\./fonts/Roboto-Regular\\.ttf")
                      (search-input-file
                       inputs
                       "share/fonts/truetype/Roboto-Regular.ttf")))))
               (add-after 'configure 'fix-library-references
                 (lambda* (#:key inputs vendor-dir #:allow-other-keys)
                   (substitute* (find-files vendor-dir "\\.rs$")
                     (("lib(wayland-.*|xkbcommon)\\.so" so-file)
                      (search-input-file
                       inputs
                       (string-append "lib/" so-file)))))))))
    (inputs
     (list font-dejavu
           font-google-roboto
           libxkbcommon
           wayland))
    (home-page "https://git.sr.ht/~kennylevinsen/wlgreet")
    (synopsis "Bare-bones Wayland-based greeter for @command{greetd}")
    (description
     "@command{wlgreet} provides a @command{greetd} greeter
that runs on a Wayland compositor such as @command{sway}.  It
is implemented with pure Wayland APIs, so it does not depend
on a GUI toolkit.")
    (license license:gpl3)))

(define-public gtkgreet
  (package
    (name "gtkgreet")
    (version "0.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.sr.ht/~kennylevinsen/gtkgreet")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0bqxz39lc8vh6bkirvrbn2lgf1qz5b04lfwgp5xa1ki1bnm5i80q"))))
    (build-system meson-build-system)
    (native-inputs (list pkg-config scdoc))
    (inputs (list gtk+ gtk-layer-shell json-c))
    (synopsis "GTK-based greeter for greetd")
    (description
     "GTK-based greeter for greetd, to be run under a compositor such as Cage.")
    (home-page "https://git.sr.ht/~kennylevinsen/gtkgreet")
    (license license:gpl3+)))

(define-public libseat
  (package
    (name "libseat")
    (version "0.9.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://git.sr.ht/~kennylevinsen/seatd")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1q1ih1f9v5240nlas1gz44giwq4k88p3yikfq7w0a4sw58yr6pz8"))))
    (build-system meson-build-system)
    (arguments
     (list #:configure-flags #~(list "-Dlibseat-logind=elogind"
                                     "-Dserver=disabled")))
    (native-inputs
     (list pkg-config))
    (propagated-inputs
     (list elogind))
    (home-page "https://sr.ht/~kennylevinsen/seatd")
    (synopsis "Seat management library")
    (description
     "This package provides a universal seat management library that
allows applications to use whatever seat management is available.")
    (license license:expat)))

(define-public seatd
  (package
    (inherit libseat)
    (name "seatd")
    (arguments
     (list #:configure-flags #~(list "-Dlibseat-logind=elogind")
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'install 'remove-libs
                 (lambda* (#:key outputs #:allow-other-keys)
                   (with-directory-excursion (assoc-ref outputs "out")
                     (for-each delete-file-recursively '("lib" "include"))))))))
    (native-inputs
     (list pkg-config
           scdoc))
    (inputs '())
    (synopsis "Seat management daemon")
    (description
     "This package provides a minimal seat management daemon whose task is to
mediate access to shared devices, such as graphics and input, for applications
that require it.")
    (license license:expat)))

(define-public sysdig
  (package
    (name "sysdig")
    (version "0.40.0-alpha6")           ;for the 0.20 patch to apply
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/draios/sysdig")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0pxmx3by0lckw7zv54wrg0cr13j1mhk2z0x4qachrf2mz5qjq2cd"))))
    (build-system cmake-build-system)
    (arguments
     (list #:tests? #f                  ;no test suite
           #:configure-flags
           #~(list "-DUSE_BUNDLED_DEPS=OFF"
                   ;; Already built and part of falcosecurity-libs, but
                   ;; needed for the 'HAS_MODERN_BPF' define.
                   "-DBUILD_SYSDIG_MODERN_BPF=ON"
                   #$(string-append "-DSYSDIG_VERSION=" version))))
    (native-inputs (list pkg-config))
    (inputs
     (list falcosecurity-libs
           luajit
           ncurses
           nlohmann-json
           yaml-cpp
           zlib))
    (home-page "https://github.com/draios/sysdig")
    (synopsis "System exploration and troubleshooting tool")
    (description "Sysdig is a simple tool for deep system visibility, with
native support for containers.  It combines features of multiple system
administration tools such as the @command{strace}, @command{tcpdump},
@command{htop}, @command{iftop} and @command{lsof} into a single interface.
The novel architecture of the tool means that the performance impact of the
tracing on the system is very light, compared to the likes of
@command{strace}.  The @command{sysdig} command has an interface similar to
@command{strace}, while the @command{csysdig} command is better suited for
interactive used, and has a user interface similar to @command{htop}.

If you use Guix System, the kernel Linux has @acronym{BPF, Berkeley Packet
Filter} support, and you should launch this tool using the @samp{--modern-bpf}
argument of the @command{sysdig} or @command{csysdig} commands.  The following
Bash aliases can be added to your @file{~/.bash_profile} file, for example:

alias sysdig=sudo sysdig --modern-bpf
alias cysdig=sudo csysdig --modern-bpf
")                                      ;XXX no @example Texinfo support
    (license license:asl2.0)))

(define-public fail2ban
  (package
    (name "fail2ban")
    (version "0.11.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/fail2ban/fail2ban")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "00d9q8m284q2wy6q462nipzszplfbvrs9fhgn0y3imwsc24kv1db"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Get rid of absolute file names.
                  (substitute* "setup.py"
                    (("/etc/fail2ban")
                     "etc/fail2ban")
                    (("/var/lib/fail2ban")
                     "var/lib/fail2ban")
                    (("\"/usr/bin/\"")
                     "\"usr/bin/\"")
                    (("\"/usr/lib/fail2ban/\"")
                     "\"usr/lib/fail2ban/\"")
                    (("'/usr/share/doc/fail2ban'")
                     "'usr/share/doc/fail2ban'"))
                  ;; disable tests performing unacceptable side-effects
                  (let ((make-suite (lambda (t)
                                      (string-append
                                       "tests.addTest.unittest.makeSuite."
                                       t ".."))))
                    (substitute* "fail2ban/tests/utils.py"
                      (((make-suite "actiontestcase.CommandActionTest"))
                       "")
                      (((make-suite "misctestcase.SetupTest"))
                       "")
                      (((make-suite
                         "filtertestcase.DNSUtilsNetworkTests"))
                       "")
                      (((make-suite "filtertestcase.IgnoreIPDNS"))
                       "")
                      (((make-suite "filtertestcase.GetFailures"))
                       "")
                      (((make-suite
                         "fail2banclienttestcase.Fail2banServerTest"))
                       "")
                      (((make-suite
                         "servertestcase.ServerConfigReaderTests"))
                       "")))))
              (patches (search-patches
                        "fail2ban-0.11.2_fix-setuptools-drop-2to3.patch"
                        "fail2ban-python310-server-action.patch"
                        "fail2ban-python310-server-actions.patch"
                        "fail2ban-python310-server-jails.patch"
                        "fail2ban-0.11.2_fix-test-suite.patch"
                        "fail2ban-0.11.2_CVE-2021-32749.patch"
                        "fail2ban-paths-guix-conf.patch"))))
    (build-system python-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (add-before 'build 'invoke-2to3
                    (lambda _
                      (invoke "./fail2ban-2to3")))
                  (add-before 'install 'fix-default-config
                    (lambda* (#:key outputs #:allow-other-keys)
                      (substitute* '("config/paths-common.conf"
                                     "fail2ban/tests/utils.py"
                                     "fail2ban/client/configreader.py"
                                     "fail2ban/client/fail2bancmdline.py"
                                     "fail2ban/client/fail2banregex.py")
                        (("/etc/fail2ban")
                         (string-append (assoc-ref outputs "out")
                                        "/etc/fail2ban")))))
                  (add-after 'fix-default-config 'set-action-dependencies
                    (lambda* (#:key inputs #:allow-other-keys)
                      ;; deleting things that are not feasible to fix
                      ;; or won't be used any way
                      (with-directory-excursion "config"
                        (for-each delete-file
                                  '("paths-arch.conf"
                                    "paths-debian.conf"
                                    "paths-fedora.conf"
                                    "paths-freebsd.conf"
                                    "paths-opensuse.conf"
                                    "paths-osx.conf")))
                      (with-directory-excursion "config/action.d"
                        (for-each delete-file
                                  '("apf.conf"
                                    "bsd-ipfw.conf"
                                    "dshield.conf"
                                    "ipfilter.conf"
                                    "ipfw.conf"
                                    "firewallcmd-allports.conf"
                                    "firewallcmd-common.conf"
                                    "firewallcmd-ipset.conf"
                                    "firewallcmd-multiport.conf"
                                    "firewallcmd-new.conf"
                                    "firewallcmd-rich-logging.conf"
                                    "firewallcmd-rich-rules.conf"
                                    "osx-afctl.conf"
                                    "osx-ipfw.conf"
                                    "pf.conf"
                                    "nginx-block-map.conf"
                                    "npf.conf"
                                    "shorewall.conf"
                                    "shorewall-ipset-proto6.conf"
                                    "ufw.conf")))
                      (let* ((lookup-cmd (lambda (i)
                                           (search-input-file inputs i)))
                             (bin (lambda (i)
                                    (lookup-cmd (string-append "/bin/" i))))
                             (sbin (lambda (i)
                                     (lookup-cmd (string-append "/sbin/" i))))
                             (ip (sbin "ip"))
                             (sendmail (sbin "sendmail")))
                        (substitute* (find-files "config/action.d" "\\.conf$")
                          ;; TODO: deal with geoiplookup ..
                          (("(awk|curl|dig|jq)" all cmd)
                           (bin cmd))
                          (("(cat|echo|grep|head|printf|wc) " all
                            cmd)
                           (string-append (bin cmd) " "))
                          ((" (date|rm|sed|tail|touch|tr) " all
                            cmd)
                           (string-append " "
                                          (bin cmd) " "))
                          (("cut -d")
                           (string-append (bin "cut") " -d"))
                          (("`date`")
                           (string-append "`"
                                          (bin "date") "`"))
                          (("id -")
                           (string-append (bin "id") " -"))
                          (("ip -([46]) addr" all ver)
                           (string-append ip " -" ver " addr"))
                          (("ip route")
                           (string-append ip " route"))
                          (("ipset ")
                           (string-append (sbin "ipset") " "))
                          (("(iptables|ip6tables) <" all cmd)
                           (string-append (sbin cmd) " <"))
                          (("/usr/bin/nsupdate")
                           (bin "nsupdate"))
                          (("mail -E")
                           (string-append sendmail " -E"))
                          (("nftables = nft")
                           (string-append "nftables = " (sbin "nft")))
                          (("perl -e")
                           (string-append (bin "perl") " -e"))
                          (("/usr/sbin/sendmail")
                           sendmail)
                          (("test -e")
                           (string-append (bin "test") " -e"))
                          (("_whois = whois")
                           (string-append "_whois = " (bin "whois")))))
                      (substitute* "config/jail.conf"
                        (("before = paths-debian.conf")
                         "before = paths-guix.conf"))))
                  (add-after 'install 'copy-man-pages
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let* ((man (string-append (assoc-ref outputs "out")
                                                 "/man"))
                             (install-man (lambda (m)
                                            (lambda (f)
                                              (install-file (string-append f
                                                             "." m)
                                                            (string-append man
                                                             "/man" m)))))
                             (install-man1 (install-man "1"))
                             (install-man5 (install-man "5")))
                        (with-directory-excursion "man"
                          (for-each install-man1
                                    '("fail2ban"
                                      "fail2ban-client"
                                      "fail2ban-python"
                                      "fail2ban-regex"
                                      "fail2ban-server"
                                      "fail2ban-testcases"))
                          (for-each install-man5
                                    '("jail.conf")))))))))
    (inputs (list gawk
                  coreutils-minimal
                  curl
                  grep
                  jq
                  iproute
                  ipset
                  iptables
                  `(,isc-bind "utils")
                  nftables
                  perl
                  python-pyinotify
                  sed
                  sendmail
                  sqlite
                  whois))
    (home-page "http://www.fail2ban.org")
    (synopsis "Daemon to ban hosts that cause multiple authentication errors")
    (description
     "Fail2Ban scans log files like @file{/var/log/auth.log} and bans IP
addresses conducting too many failed login attempts.  It does this by updating
system firewall rules to reject new connections from those IP addresses, for a
configurable amount of time.  Fail2Ban comes out-of-the-box ready to read many
standard log files, such as those for sshd and Apache, and is easily
configured to read any log file of your choosing, for any error you wish.

Though Fail2Ban is able to reduce the rate of incorrect authentication
attempts, it cannot eliminate the risk presented by weak authentication.  Set
up services to use only two factor, or public/private authentication
mechanisms if you really want to protect services.")
    (license license:gpl2+)))

(define-public restartd
  (let* ((commit "7044125ac55056f2663536f7137170edf92ebd75")
         ;; Version is 0.2.4 in the version file in the repo
         ;; but not in github tags.
         ;; It is released as 0.2.3-1.1 for other distributions.
         ;; Probably because of the lack of activity upstream.
         (revision "1"))
    (package
      (name "restartd")
      (version (git-version "0.2.3" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/ajraymond/restartd")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1m1np00b4zvvwx63gzysbi38i5vj1jsjvh2s0p9czl6dzyz582z0"))
         (patches (search-patches "restartd-update-robust.patch"))))
      (build-system gnu-build-system)
      (arguments
       (list
        #:tests? #f ; no tests
        #:make-flags
        #~(list (string-append "CC=" #$(cc-for-target)))
        #:phases
        #~(modify-phases %standard-phases
            (delete  'configure)
            (replace 'install
              (lambda _
                (install-file "restartd.conf" (string-append #$output "/etc"))
                (install-file "restartd" (string-append #$output "/sbin"))
                (install-file "restartd.8"
                              (string-append #$output "/share/man/man8"))
                (mkdir-p (string-append #$output "/share/man/fr/man8"))
                (copy-file
                 "restartd.fr.8"
                 (string-append #$output "/share/man/fr/man8/restartd.8")))))))
      (home-page "https://launchpad.net/debian/+source/restartd")
      (synopsis "Daemon for restarting processes")
      (description "This package provides a daemon for checking running and
not running processes.  It reads the @file{/proc} directory every @var{n}
seconds and does a POSIX regexp on the process names.  The daemon runs a
user-provided script when it detects a program in the running processes, or an
alternate script if it doesn't detect the program.  The daemon can only be
called by the root user, but can use @command{sudo -u user} in the process
called if needed.")
      (license license:gpl2+))))

(define-public rex
  (package
    (name "rex")
    (version "4.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://download.gnu.org.ua/pub/releases/rex/rex-"
                    version ".tar.gz"))
              (sha256
               (base32
                "1arb8z602invwavskq36nhwy42a3v14iyhi06iqlngfai2k93fai"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f                       ; no tests
      #:phases
      #~(modify-phases %standard-phases
          ;; No configure script and Makefile.
          (delete 'configure)
          (delete 'build)
          (add-before 'install 'patch-exec-expect
            (lambda _
              (substitute* "rex"
                (("exec expect") (string-append "exec " (which "expect"))))))
          (replace 'install
            (lambda _
              (invoke "./install"
                      (string-append "--prefix=" #$output)))))))
    (inputs (list expect))
    (home-page "https://www.gnu.org.ua/software/rex/")
    (synopsis "Remote execution utility")
    (description "@command{rex} runs a supplied command or shell script on
several hosts in succession or in parallel.  It can also be used to copy a
file or files to several hosts.")
    (license license:gpl3+)))

(define-public doctl
  (package
    (name "doctl")
    (version "1.120.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/digitalocean/doctl")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "12fgymgiv6894ghar7ljg69hb7mi18pa2a74sp7fyymqvyhiv6z9"))
              (snippet
               ;; TODO: Unbundle more.
               #~(begin (use-modules (guix build utils))
                        (for-each delete-file-recursively
                                  (list "vendor/golang.org"))))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/digitalocean/doctl/cmd/doctl"
           #:unpack-path "github.com/digitalocean/doctl"
           #:build-flags
           #~(list (string-append "-ldflags=-X github.com/digitalocean/doctl.Label=release"
                                  " -X github.com/digitalocean/doctl.Major="
                                  (car (string-split #$version #\.))
                                  " -X github.com/digitalocean/doctl.Minor="
                                  (cadr (string-split #$version #\.))
                                  " -X github.com/digitalocean/doctl.Patch="
                                  (caddr (string-split #$version #\.))))
           #:install-source? #f
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'install 'install-completions
                 (lambda _
                   (define (install-completion shell file)
                     (let ((file (string-append #$output file)))
                       (mkdir-p (dirname file))
                       (with-output-to-file file
                         (lambda _
                           (invoke (string-append #$output "/bin/doctl")
                                   "completion" shell)))))
                   (install-completion "bash" "/etc/bash_completion.d/doctl")
                   (install-completion "fish"
                                       "/etc/fish/completions/doctl.fish")
                   (install-completion "zsh"
                                       "/etc/zsh/site-functions/_doctl"))))))
    (native-inputs
     (list go-golang-org-x-crypto
           go-golang-org-x-mod
           go-golang-org-x-net
           go-golang-org-x-oauth2
           go-golang-org-x-sync
           go-golang-org-x-sys
           go-golang-org-x-term
           go-golang-org-x-text
           go-golang-org-x-time
           go-golang-org-x-tools))
    (home-page "https://github.com/digitalocean/doctl")
    (synopsis "Command line client for DigitalOcean")
    (description
     "@code{doctl} provides a unified command line interface to the DigitalOcean API.")
    (license license:asl2.0)))

(define-public du-dust
  (package
    (name "du-dust")
    (version "1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "du-dust" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0qr6ikq2ds8bq35iw480qyhf3d43dj61wiwp8587n3mgqf5djx8w"))))
    (build-system cargo-build-system)
    (arguments
     (list #:install-source? #f
           #:cargo-inputs `(("rust-ansi-term" ,rust-ansi-term-0.12)
                            ("rust-chrono" ,rust-chrono-0.4)
                            ("rust-clap" ,rust-clap-4)
                            ("rust-clap-complete" ,rust-clap-complete-4)
                            ("rust-clap-mangen" ,rust-clap-mangen-0.2)
                            ("rust-config-file" ,rust-config-file-0.2)
                            ("rust-ctrlc" ,rust-ctrlc-3)
                            ("rust-directories" ,rust-directories-4)
                            ("rust-filesize" ,rust-filesize-0.2)
                            ("rust-lscolors" ,rust-lscolors-0.13)
                            ("rust-rayon" ,rust-rayon-1)
                            ("rust-regex" ,rust-regex-1)
                            ("rust-serde" ,rust-serde-1)
                            ("rust-serde-json" ,rust-serde-json-1)
                            ("rust-stfu8" ,rust-stfu8-0.2)
                            ("rust-sysinfo" ,rust-sysinfo-0.27)
                            ("rust-terminal-size" ,rust-terminal-size-0.2)
                            ("rust-thousands" ,rust-thousands-0.2)
                            ("rust-unicode-width" ,rust-unicode-width-0.1)
                            ("rust-winapi-util" ,rust-winapi-util-0.1))
           #:cargo-development-inputs `(("rust-assert-cmd" ,rust-assert-cmd-2)
                                        ("rust-tempfile" ,rust-tempfile-3))
           #:phases #~(modify-phases %standard-phases
                        (add-after 'install 'install-extras
                          (lambda* (#:key outputs #:allow-other-keys)
                            (let* ((out (assoc-ref outputs "out"))
                                   (share (string-append out "/share")))
                              (install-file "man-page/dust.1"
                                            (string-append share "/man/man1"))
                              (mkdir-p (string-append out
                                        "/etc/bash_completion.d"))
                              (copy-file "completions/dust.bash"
                                         (string-append out
                                          "/etc/bash_completion.d/dust"))
                              (install-file "completions/dust.fish"
                                            (string-append share
                                             "/fish/vendor_completions.d"))
                              (install-file "completions/_dust"
                                            (string-append share
                                             "/zsh/site-functions"))))))))
    (home-page "https://github.com/bootandy/dust")
    (synopsis "Graphical disk usage analyzer")
    (description "This package provides a graphical disk usage analyzer in
text mode.")
    (license license:asl2.0)))

(define-public mactelnet
  (package
    (name "mactelnet")
    (version "0.4.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/haakonnessjoen/MAC-Telnet")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1z63dz22crrvrm0sh2cwpyqb7wqd9m45m6f2641mwmyp6hcpf4k4"))
              (patches (search-patches "mactelnet-remove-init.patch"))
              (modules '((guix build utils)))
              (snippet
               #~(begin
                   (delete-file "src/utlist.h")
                   (substitute* (find-files "src/" "\\.c$")
                     (("\"utlist\\.h\"") "<utlist.h>"))))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f))  ; no tests
    (native-inputs (list autoconf automake gettext-minimal))
    (inputs (list uthash))
    (synopsis "MAC-Telnet utilities for communicating with RouterOS devices")
    (description "This package provides an implementation of the MAC-Telnet protocol
used by RouterOS devices.  It provides the following commands:
@table @command
@item{macping}
Ping RouterOS devices or @command{mactelnetd} hosts.
@item{mactelnetd}
MAC-Telnet daemon.
@item{mactelnet}
MAC-Telnet client.
@item{mndp}
Discover other RouterOS devices or @command{mactelnetd} hosts.
@end table")
    (home-page "https://lunatic.no/2010/10/routeros-mac-telnet-application-for-linux-users/")
    (license
     (list license:gpl2+
           ;; Note: applies to src/md5.{c,h}
           ;; This file is likely to be gone in the next release.
           license:zlib))))

(define-public bfs
  (package
    (name "bfs")
    (version "3.0.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/tavianator/bfs")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0n2y9m81278j85m8vk242m9nsxdcw62rxsar4hzwszs6p5cjz5ny"))))
    (build-system gnu-build-system)
    (arguments
     (list #:make-flags #~(list (string-append "CC="
                                               #$(cc-for-target))
                                (string-append "PREFIX="
                                               #$output) "bfs")
           #:phases #~(modify-phases %standard-phases
                        (delete 'configure)
                        (add-before 'check 'disable-exec-no-path-test
                          (lambda _
                            ;; This test unsets PATH. It then probably cannot find
                            ;; echo since it's not inside _PATH_STDPATH (?). We
                            ;; delete the test to disable it.
                            (delete-file "tests/posix/exec_nopath.sh"))))))
    (inputs (list acl attr libcap oniguruma))
    (synopsis "Breadth-first search for your files")
    (description
     "Bfs is a variant of the UNIX @command{find} command that operates
breadth-first rather than depth-first.  It is otherwise compatible with many
versions of @command{find}, including POSIX, GNU, and *BSD find.")
    (home-page "https://tavianator.com/projects/bfs.html")
    (license license:bsd-0)))

(define-public rdfind
  (package
    (name "rdfind")
    (version "1.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://rdfind.pauldreik.se/" name "-" version
                           ".tar.gz"))
       (sha256
        (base32 "0y9j1w3nbgjks0k4kgm6qq92yrwgv66n212ncmlmhsl8y676wh3s"))))
    (build-system gnu-build-system)
    (native-inputs (list which))
    (inputs (list nettle))
    (arguments
     (list
      #:phases #~(modify-phases %standard-phases
                   (add-before 'check 'patch-tests
                     (lambda _
                       (display (which "echo"))
                       (substitute* "testcases/common_funcs.sh"
                         (("/bin/echo")
                          (which "echo"))))))))
    (home-page "https://rdfind.pauldreik.se")
    (synopsis "Find duplicate files")
    (description
     "Rdfind is a command line tool that finds duplicate files based on
their content instead of their file names.  It is useful for compressing
backup directories or just finding duplicate files.")
    (license license:gpl2+)))

(define-public sshguard
  (package
    (name "sshguard")
    (version "2.4.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://bitbucket.org/sshguard/sshguard")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1dkijr287zpwdz1bjdchxzmwf1sk6vzpkycz1skm25lkaba6nd9r"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake bison flex python-docutils))
    (home-page "https://sshguard.net/")
    (synopsis "Daemon to blocks SSH brute-force attacks")
    (description
     "SSHGuard protects hosts from brute-force attacks against SSH and other
services.  It aggregates system logs and blocks repeat offenders using one of
several firewall backends.")
    (license license:isc)))

(define-public px
  (package
    (name "px")
    (version "3.6.9")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/walles/px.git")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0kqwi1qb6hvk4si1dynz4q56lxy5161b50fgsvlfk9dnb6gwln6i"))))
    (build-system python-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'patch-git
                 (lambda* (#:key inputs #:allow-other-keys)
                   (substitute* (find-files "px" "\\.py$")
                    ;; We don't have MacOS X programs.
                    (("px_exec_util[.]run[(]\\[\"(vm_stat)\"" all x)
                     (string-append "px_exec_util.run([\""
                                    "/bin/false"
                                    "\""))
                    ;; Patch names of executables in "sbin".
                    (("px_exec_util[.]run[(]\\[\"(sysctl)\"" all x)
                     (string-append "px_exec_util.run([\""
                                    (search-input-file inputs
                                                       (string-append "sbin/" x))
                                    "\""))
                    ;; Patch names of executables in "bin".
                    (("px_exec_util[.]run[(]\\[\"([^/\"]*)\"" all x)
                     (string-append "px_exec_util.run([\""
                                    (search-input-file inputs
                                                       (string-append "bin/" x))
                                    "\"")))
                   (substitute* "px/px_process.py"
                    ;; Patch path name of "ps" executable.
                    (("\"/bin/ps\"") (string-append "\""
                                                    (assoc-ref inputs "procps")
                                                    "/bin/ps\"")))
                   (substitute* "setup.py"
                    ;; Patch "git describe", replacing it by its result.
                    (("\\[\"git\", \"describe\", \"--dirty\"\\]")
                     (string-append "[\"echo\", \"" #$version "\"]")))))
               (add-before 'check 'prepare-check
                 (lambda _
                   (substitute* "tests/px_terminal_test.py"
                    ;; We don't have /etc/passwd so the output will not say "root".
                    (("root") "0   "))
                   (substitute* "tests/px_process_test.py"
                    ;; Our containers don't have the kernel visible.
                    (("len[(]all_processes[)] >= 4")
                     "len(all_processes) >= 3")
                    ;; We don't have /etc/passwd so the output will not say "root".
                    (("\"root\"") "\"0\""))
                   (setenv "PYTEST_ADDOPTS"
                           (string-append "-vv -k \"not "
                                          (string-join
                                           '(;; Network tests cannot succeed.
                                             "test_stdfds_ipc_and_network"
                                             ;; Network tests cannot succeed.
                                             "test_str_resolve"
                                             ;; Tiny difference in color.
                                             "test_to_screen_lines_unbounded")
                                           " and not ")
                                          "\"")))))))
    (native-inputs
     (list pkg-config python-setuptools python-wheel python-pytest
           python-pytest-runner python-dateutil))
    (inputs
     (list lsof net-tools procps sysstat util-linux))
    (synopsis "ps, top and pstree for human beings")
    (description "This package provides a way to figure out which processes
communicate with which other processes.  It provides more usable versions
of ps, top and pstree.")
    (home-page "https://github.com/walles/px")
    (license license:expat)))
