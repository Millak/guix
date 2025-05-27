;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2016, 2019, 2020, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013, 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2013 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2014, 2015, 2016, 2017, 2018, 2021 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016, 2020, 2021, 2023, 2024 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Lukas Gradl <lgradl@openmailbox.org>
;;; Copyright © 2017, 2018, 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017 Petter <petter@mykolab.ch>
;;; Copyright © 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Alex Vong <alexvong1995@gmail.com>
;;; Copyright © 2019-2023, 2025 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2019 Giacomo Leidi <goodoldpaul@autistici.org>
;;; Copyright © 2019, 2020, 2021 Marius Bakke <marius@gnu.org>
;;; Copyright © 2020 Nicolò Balzarotti <nicolo@nixo.xyz>
;;; Copyright © 2020 Florian Pelz <pelzflorian@pelzflorian.de>
;;; Copyright © 2020, 2023 Janneke Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2020 Arthur Margerit <ruhtra.mar@gmail.com>
;;; Copyright © 2021 Maxime Devos <maximedevos@telenet.be>
;;; Copyright © 2022 Petr Hodina <phodina@protonmail.com>
;;; Copyright © 2023 Saku Laesvuori <saku@laesvuori.fi>
;;; Copyright © 2024 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2024 Remco van 't Veer <remco@remworks.net>
;;; Copyright © 2024 dan <i@dan.games>
;;; Copyright © 2025 John Kehayias <john.kehayias@protonmail.com>
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

(define-module (gnu packages glib)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages enlightenment)
  #:use-module (gnu packages file)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages libunwind)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages nettle)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-check)
  #:use-module (gnu packages popt)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pretty-print)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system python)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-26)
  #:use-module ((srfi srfi-1) #:hide (zip))

  ;; Export variables up-front to allow circular dependency with the 'xorg'
  ;; module.
  #:export (dbus
            glib
            gobject-introspection
            dbus-glib
            intltool
            itstool
            libsigc++
            glibmm
            telepathy-glib
            perl-net-dbus
            perl-net-dbus-glib))

(define dbus
  (package
    (name "dbus")
    (version "1.15.8")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://dbus.freedesktop.org/releases/dbus/dbus-"
                    version ".tar.xz"))
              (sha256
               (base32
                "016j3rqc8m62bg0h7z4rpvbvm5bg0hbjrld733f0aby8drz5kz44"))
              (patches (search-patches "dbus-helper-search-path.patch"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list
         ;; Install the system bus socket under /var.
         "--localstatedir=/var"

         ;; Install the session bus socket under /tmp.
         "--with-session-socket-dir=/tmp"

         ;; Build shared libraries only.
         "--disable-static"

         ;; Use /etc/dbus-1 for system-wide config.
         ;; Look for configuration file under
         ;; /etc/dbus-1.  This is notably required by
         ;; 'dbus-daemon-launch-helper', which looks for
         ;; the 'system.conf' file in that place,
         ;; regardless of what '--config-file' was
         ;; passed to 'dbus-daemon' on the command line;
         ;; see <https://bugs.freedesktop.org/show_bug.cgi?id=92458>.
         "--sysconfdir=/etc")
      #:phases
      #~(modify-phases %standard-phases
          (replace 'install
            (lambda _
              ;; Don't try to create /var and /etc.
              (invoke "make"
                      "localstatedir=/tmp/dummy"
                      "sysconfdir=/tmp/dummy"
                      "install"))))))
    (native-inputs
     ;; Some dependencies are required to generate the documentation.  Also,
     ;; quoting NEWS for 1.15.8: “Autotools-generated files are no longer
     ;; included in the tarball release.”
     (list autoconf
           autoconf-archive
           automake
           docbook-xml-4.4
           docbook-xsl
           doxygen
           libtool
           libxslt
           which
           xmlto
           yelp-tools
           pkg-config))
    (inputs
     (list expat
           ;; Add a dependency on libx11 so that 'dbus-launch' has support for
           ;; '--autolaunch'.
           libx11))
    (outputs '("out" "doc"))            ;22 MiB of HTML doc
    (home-page "https://www.freedesktop.org/wiki/Software/dbus/")
    (synopsis "Message bus for inter-process communication (IPC)")
    (description
     "D-Bus is a message bus system, a simple way for applications to
talk to one another.  In addition to interprocess communication, D-Bus
helps coordinate process lifecycle; it makes it simple and reliable to
code a \"single instance\" application or daemon, and to launch
applications and daemons on demand when their services are needed.

D-Bus supplies both a system daemon (for events such as \"new hardware
device added\" or \"printer queue changed\") and a
per-user-login-session daemon (for general IPC needs among user
applications).  Also, the message bus is built on top of a general
one-to-one message passing framework, which can be used by any two apps
to communicate directly (without going through the message bus
daemon).  Currently the communicating applications are on one computer,
or through unencrypted TCP/IP suitable for use behind a firewall with
shared NFS home directories.")
    (license license:gpl2+)))                     ; or Academic Free License 2.1

;;; This variant is used for the Jami service: it provides an entry point to
;;; further customize the configuration of the D-Bus instance run by the
;;; jami-dbus-session service.
(define-public dbus-for-jami
  (hidden-package
   (package/inherit dbus
     (name "dbus-for-jami")
     (arguments
      (substitute-keyword-arguments (package-arguments dbus)
        ((#:phases phases)
         #~(modify-phases #$phases
             (add-after 'unpack 'customize-config
               (lambda _
                 (substitute* "bus/session.conf.in"
                   (("@SYSCONFDIR_FROM_PKGDATADIR@/dbus-1/session-local.conf")
                    "/var/run/jami/session-local.conf")))))))))))

(define-public dbus-1.15.0
  ;; Dbus 1.15.2 has a breaking change.
  (hidden-package
   (package/inherit dbus
     (version "1.15.0")
     (source (origin
               (method url-fetch)
               (uri (string-append
                     "https://dbus.freedesktop.org/releases/dbus/dbus-"
                     version ".tar.xz"))
               (sha256
                (base32
                 "02k4zm5h24clwp4csp2r3xp2lxib31jlk3xkgdj2c0njkb5whwsh"))
               (patches (search-patches "dbus-helper-search-path.patch")))))))

;;; The reason this is not enabled in the regular dbus package is because it
;;; impacts the performance of D-Bus (including its library) as a whole, even
;;; when the DBUS_VERBOSE environment variable is not set.
(define-public dbus-verbose
  (package/inherit dbus
    (name "dbus-verbose")
    (arguments (substitute-keyword-arguments (package-arguments dbus)
                 ((#:configure-flags flags '())
                  #~(cons "--enable-verbose-mode" #$flags))))
    (synopsis "D-Bus with verbose mode enabled for debugging")
    (description "This variant D-Bus package is built with verbose mode, which
eases debugging of D-Bus services by printing various debug information when
the @code{DBUS_VERBOSE} environment variable is set to @samp{1}.  For more
information, refer to the @samp{dbus-daemon(1)} man page.")))

(define glib-minimal
  (package
    (name "glib")
    (version "2.82.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/"
                       name "/" (string-take version 4) "/"
                       name "-" version ".tar.xz"))
       (sha256
        (base32 "19l98kdv6d4363minliw0imvxh4qfdw5im988knf8bpm1d2391j7"))
       (patches
        (search-patches "glib-appinfo-watch.patch"
                        "glib-skip-failing-test.patch"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           (substitute* "glib/tests/spawn-test.c"
             (("/bin/sh") "sh"))))))
    (build-system meson-build-system)
    (outputs '("out"                    ;libraries, locales, etc
               "static"                 ;static libraries
               "bin"                    ;executables; depends on Python
               "debug"))
    (arguments
     (list
      #:disallowed-references
      (cons tzdata-for-tests
            ;; Verify glib-mkenums, gtester, ... use the cross-compiled
            ;; python.
            (if (%current-target-system)
                (map (cut gexp-input <> #:native? #t)
                     `(,(this-package-native-input "python")
                       ,(this-package-native-input "python-wrapper")))
                '()))
      #:configure-flags #~(list "--default-library=both"
                                "-Dman-pages=disabled"
                                "-Dselinux=disabled"
                                (string-append "--bindir="
                                               #$output:bin "/bin"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'set-G_TEST_SRCDIR
            (lambda _
              (setenv "G_TEST_SRCDIR" (string-append (getcwd) "/gio/tests"))))
          ;; Needed to pass the test phase on slower ARM and i686 machines.
          (add-after 'unpack 'increase-test-timeout
            (lambda _
              (substitute* "meson.build"
                (("(test_timeout.*) = ([[:digit:]]+)" all first second)
                 (string-append first " = " second "0")))))
          (add-after 'unpack 'disable-failing-tests
            (lambda _
              (substitute* "gio/tests/meson.build"
                ((".*'testfilemonitor'.*") ;marked as flaky
                 ""))
              (with-directory-excursion "glib/tests"
                (substitute* '("unix.c" "utils.c")
                  (("[ \t]*g_test_add_func.*;") "")))
              (with-directory-excursion "gio/tests"
                (substitute* '("contenttype.c"
                               "gdbus-address-get-session.c"
                               "gdbus-server-auth.c"
                               "gdbus-peer.c"
                               "appinfo.c"
                               "desktop-app-info.c")
                  (("[ \t]*g_test_add_func.*;") ""))
                (unless (which "update-desktop-database")
                  (substitute* "file.c"
                    (("[ \t]*g_test_add_func.*query-default-handler.*;") "")))
                (substitute* '("portal-support-snap.c")
                  (("g_test_init .*")
                   "return EXIT_SUCCESS;")))

              #$@(if (target-x86-32?)
                     ;; Comment out parts of timer.c that fail on i686 due to
                     ;; excess precision when building with GCC 10:
                     ;; <https://gitlab.gnome.org/GNOME/glib/-/issues/820>.
                     '((substitute* "glib/tests/timer.c"
                         (("^  g_assert_cmpuint \\(micros.*" all)
                          (string-append "//" all "\n"))
                         (("^  g_assert_cmpfloat \\(elapsed, ==.*" all)
                          (string-append "//" all "\n"))))
                     '())
              #$@(if (target-ppc32?)
                     ;; assertion failed (last_thread_id <= thread_id): (3 <= 2)
                     #~((substitute* "glib/tests/thread-pool-slow.c"
                          (("^   g_assert_cmpint \\(last_thread_id.*" all)
                          (string-append "//" all "\n"))))
                     #~())
              #$@(if (system-hurd?)
                     '((with-directory-excursion "gio/tests"
                         ;; TIMEOUT after 600s
                         (substitute* '("actions.c"
                                        "dbus-appinfo.c"
                                        "debugcontroller.c"
                                        "gdbus-bz627724.c"
                                        "gdbus-connection-slow.c"
                                        "gdbus-exit-on-close.c"
                                        "gdbus-export.c"
                                        "gdbus-introspection.c"
                                        "gdbus-method-invocation.c"
                                        "gdbus-non-socket.c"
                                        "gdbus-proxy-threads.c"
                                        "gdbus-proxy-unique-name.c"
                                        "gdbus-proxy-well-known-name.c"
                                        "gdbus-proxy.c"
                                        "gdbus-test-codegen.c"
                                        "gmenumodel.c"
                                        "gnotification.c"
                                        "stream-rw_all.c")
                           (("return (g_test_run|session_bus_run)" all call)
                            (string-append "return 0;// " call))
                           ((" (ret|rtv|result) = (g_test_run|session_bus_run)"
                             all var call)
                            (string-append " " var " = 0;// " call))
                           (("[ \t]*g_test_add_func.*;") ""))

                         ;; commenting-out g_assert, g_test_add_func, g_test_run
                         ;; does not help; special-case short-circuit.
                         (substitute* "gdbus-connection-loss.c" ;; TODO?
                           (("  gchar \\*path;.*" all)
                            (string-append all "  return 0;\n")))

                         ;; FAIL
                         (substitute* '("appmonitor.c"
                                        "async-splice-output-stream.c"
                                        "autoptr.c"
                                        "contexts.c"
                                        "converter-stream.c"
                                        "file.c"
                                        "g-file-info.c"
                                        "g-file.c"
                                        "g-icon.c"
                                        "gapplication.c"
                                        "gdbus-connection-flush.c"
                                        "gdbus-connection.c"
                                        "gdbus-names.c"
                                        "gdbus-server-auth.c"
                                        "gsocketclient-slow.c"
                                        "gsubprocess.c"
                                        "io-stream.c"
                                        "live-g-file.c"
                                        "memory-monitor.c"
                                        "mimeapps.c"
                                        "network-monitor-race.c"
                                        "network-monitor.c"
                                        "pollable.c"
                                        "power-profile-monitor.c"
                                        "readwrite.c"
                                        "resources.c"
                                        "socket-service.c"
                                        "socket.c"
                                        "tls-bindings.c"
                                        "tls-certificate.c"
                                        "tls-database.c"
                                        "trash.c"
                                        "vfs.c")
                           (("return (g_test_run|session_bus_run)" all call)
                            (string-append "return 0;// " call))
                           ((" (ret|rtv|result) = (g_test_run|session_bus_run)"
                             all var call)
                            (string-append " " var " = 0;// " call))
                           (("[ \t]*g_test_add_func.*;") ""))

                         ;; commenting-out g_test_add_func, g_test_run does
                         ;; not help; special-case short-circuit.
                         (substitute* "gsettings.c"
                           (("#ifdef TEST_LOCALE_PATH" all)
                            (string-append "  return 0;\n" all)))

                         ;; commenting-out g_test_add_func, ;; g_test_run does
                         ;; not help; special-case short-circuit.
                         (substitute* "proxy-test.c"
                           (("  gint result.*;" all)
                            (string-append all "  return 0;\n")))

                         ;; commenting-out g_test_add_func, g_test_run
                         ;; does not help; special-case short-circuit.
                         (substitute* "volumemonitor.c"
                           (("  gboolean ret;" all)
                            (string-append all "  return 0;\n"))))

                       (with-directory-excursion "glib/tests"
                         ;; TIMEOUT after 600s
                         (substitute* "thread-pool.c"
                           (("[ \t]*g_test_add_func.*;") ""))

                         ;; FAIL
                         (substitute* "fileutils.c"
                           (("[ \t]*g_test_add_func.*;") ""))))
                     '())))
          ;; Python references are not being patched in patch-phase of build,
          ;; despite using python-wrapper as input. So we patch them manually.
          ;;
          ;; These python scripts are both used during build and installed,
          ;; so at first, use a python from 'native-inputs', not 'inputs'. When
          ;; cross-compiling, the 'patch-shebangs' phase will replace
          ;; the native python with a python from 'inputs'.
          (add-after 'unpack 'patch-python-references
            (lambda* (#:key native-inputs inputs #:allow-other-keys)
              (substitute* '("gio/gdbus-2.0/codegen/gdbus-codegen.in"
                             "glib/gtester-report.in"
                             "gobject/glib-genmarshal.in"
                             "gobject/glib-mkenums.in")
                (("@PYTHON@")
                 (search-input-file (or native-inputs inputs)
                                    (string-append
                                     "/bin/python"
                                     #$(version-major+minor
                                        (package-version python))))))))
          (add-before 'check 'pre-check
            (lambda* (#:key native-inputs inputs outputs #:allow-other-keys)
              ;; For tests/gdatetime.c.
              (setenv "TZDIR"
                      (search-input-directory (or native-inputs inputs)
                                              "share/zoneinfo"))
              ;; Some tests want write access there.
              (setenv "HOME" (getcwd))
              (setenv "XDG_CACHE_HOME" (getcwd))))
          (add-after 'install 'move-static-libraries
            (lambda _
              (mkdir-p (string-append #$output:static "/lib"))
              (for-each (lambda (a)
                          (rename-file a (string-append #$output:static "/lib/"
                                                        (basename a))))
                        (find-files #$output "\\.a$"))))
          (add-after 'install 'patch-pkg-config-files
            (lambda* (#:key outputs #:allow-other-keys)
              ;; Do not refer to "bindir", which points to "${prefix}/bin".
              ;; We don't patch "bindir" to point to "$bin/bin", because that
              ;; would create a reference cycle between the "out" and "bin"
              ;; outputs.
              (substitute*
                  (list (search-input-file outputs "lib/pkgconfig/gio-2.0.pc")
                        (search-input-file outputs "lib/pkgconfig/glib-2.0.pc"))
                (("^bindir=.*")
                 "")
                (("=\\$\\{bindir\\}/")
                 "=")))))))
    (native-inputs
     (list dbus
           gettext-minimal
           m4                           ;for installing m4 macros
           perl                         ;needed by GIO tests
           pkg-config
           python                       ;for 'patch-python-references
           python-wrapper
           tzdata-for-tests))           ;for tests/gdatetime.c
    (inputs
     (list
      ;; "python", "python-wrapper" and "bash-minimal"
      ;; are for the 'patch-shebangs' phase, to make
      ;; sure the installed scripts end up with a correct shebang
      ;; when cross-compiling.
      bash-minimal
      python
      python-wrapper))
    (propagated-inputs
     (list libffi            ; in the Requires.private field of gobject-2.0.pc
           pcre2             ; in the Requires.private field of glib-2.0.pc
           `(,util-linux "lib") ;for libmount
           zlib))               ; in the Requires.private field of glib-2.0.pc
    (native-search-paths
     ;; This variable is not really "owned" by GLib, but several related
     ;; packages refer to it: gobject-introspection's tools use it as a search
     ;; path for .gir files, and it's also a search path for schemas produced
     ;; by 'glib-compile-schemas'.
     (list
      (search-path-specification
       (variable "XDG_DATA_DIRS")
       (files '("share")))
      ;; To load extra gio modules from glib-networking, etc.
      (search-path-specification
       (variable "GIO_EXTRA_MODULES")
       (files '("lib/gio/modules")))))
    (search-paths native-search-paths)
    (synopsis "Low-level core library for GNOME projects")
    (description "GLib provides the core application building blocks for
libraries and applications written in C.  It provides the core object system
used in GNOME, the main loop implementation, and a large set of utility
functions for strings and common data structures.")
    (home-page "https://wiki.gnome.org/Projects/GLib")
    (license license:lgpl2.1+)
    (properties '((hidden? . #t)))))

(define glib
  (let ((base glib-minimal))
    (package/inherit base
      (native-inputs
       (modify-inputs (package-native-inputs base)
         (prepend gobject-introspection-minimal)))
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ((#:phases phases)
          #~(modify-phases #$phases
              ;; GI tests require installed libraries
              (delete 'check)
              (add-after 'install 'check
                (assoc-ref #$phases 'check)))))))))

(define-public glib-with-documentation
  ;; glib's doc must be built in a separate package since it requires gtk-doc,
  ;; which in turn depends on glib.
  (let ((base glib))
    (package/inherit base
      (properties (alist-delete 'hidden? (package-properties base)))
      (outputs (cons "doc" (package-outputs base)))
      (native-inputs
       (modify-inputs (package-native-inputs base)
         (append gi-docgen python-docutils)))
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ((#:configure-flags flags ''())
          #~(cons "-Ddocumentation=true"
                  (delete "-Dman-pages=disabled" #$flags)))
         ((#:phases phases)
          #~(modify-phases #$phases
              (add-after 'install 'move-doc
                (lambda _
                  (let ((doc "/share/doc"))
                    (mkdir-p (string-append #$output:doc "/share"))
                    (rename-file
                     (string-append #$output doc)
                     (string-append #$output:doc doc))))))))))))

(define (python-extension-suffix python triplet)
  "Determine the suffix for C extensions for PYTHON when compiled
for TRIPLET."
  ;; python uses strings like 'x86_64-linux-gnu' instead of
  ;; 'x86_64-unknown-linux-gnu'.
  (define normalised-system
    (string-replace-substring triplet "-unknown-" "-"))
  (define major.minor (version-major+minor (package-version python)))
  (define majorminor (string-delete #\. major.minor))
  (string-append
    ;; If guix' python package used "--with-pydebug", a #\d would
    ;; need to be added, likewise "--with-pymalloc" and "--with-wide-unicode"
    ;; would require a #\m and #\u, see cpython's configure.ac.
    ".cpython-" majorminor "-" normalised-system
    (if (target-mingw? triplet)
        ".dll"
        ".so")))

(define (correct-library-name-phase python name)
  "Return a G-exp evaluating to a phase renaming the python extension NAME
from what Meson thinks its name should be to what python expects its name
to be.  NAME must not include the platform-specific suffix.  This can only
be used when cross-compiling."
  #~(lambda _
      (define name #$name)
      (define native-suffix
        #$(python-extension-suffix python
                                   (nix-system->gnu-triplet (%current-system))))
      (define target-suffix
        #$(python-extension-suffix python (%current-target-system)))
      (define native-name
        (string-append name native-suffix))
      (define target-name
        (string-append name target-suffix))
      (rename-file native-name target-name)))

(define gobject-introspection-minimal
  (package
    (name "gobject-introspection")
    (version "1.82.0")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnome/sources/"
                   "gobject-introspection/" (version-major+minor version)
                   "/gobject-introspection-" version ".tar.xz"))
             (sha256
              (base32 "029gr80q8749dhcpmf5x1w48adinihb634qyqimz4js210clqnhg"))
             (patches (search-patches
                       "gobject-introspection-cc.patch"
                       "gobject-introspection-girepository.patch"
                       "gobject-introspection-absolute-shlib-path.patch"))))
    (build-system meson-build-system)
    (arguments
     `(,@(if (%current-target-system)
             `(#:configure-flags
               '("-Dgi_cross_use_prebuilt_gi=true"
                 ;; Building introspection data requires running binaries
                 ;; for ‘host’ on ‘build’, so don't do that.
                 ;;
                 ;; TODO: it would be nice to have introspection data anyways
                 ;; as discussed here: https://issues.guix.gnu.org/50201#60.
                 "-Dbuild_introspection_data=false"))
             '())
       #:phases
       ,#~
       (modify-phases %standard-phases
         #$@(if (%current-target-system)
                ;; 'typelibs' is undefined.
                `((add-after 'unpack 'set-typelibs
                    (lambda _
                      (substitute* "meson.build"
                        (("\\bsources: typelibs\\b")
                         "sources: []")))))
                '())
         (add-after 'unpack 'do-not-use-/usr/bin/env
           (lambda _
             (substitute* "tools/g-ir-tool-template.in"
               (("#!@PYTHON_CMD@")
                (string-append "#!" (which "python3"))))))
         #$@(if (%current-target-system)
               ;; Meson gives python extensions an incorrect name, see
               ;; <https://github.com/mesonbuild/meson/issues/7049>.
                #~((add-after 'install 'rename-library
                     #$(correct-library-name-phase
                         (this-package-input "python")
                         #~(string-append #$output
                                          "/lib/gobject-introspection/giscanner"
                                          "/_giscanner"))))
                #~()))))
    (native-inputs
     `(,@(if (%current-target-system)
           `(("python" ,python))
           '())
       ("glib" ,glib-minimal "bin")
       ("pkg-config" ,pkg-config)
       ("bison" ,bison)
       ("flex" ,flex)))
    (inputs
     (list python zlib))
    (propagated-inputs
     (list glib-minimal
           ;; In practice, GIR users will need libffi when using
           ;; gobject-introspection.
           libffi))
    (native-search-paths
     (list
      (search-path-specification
       (variable "GI_TYPELIB_PATH")
       (files '("lib/girepository-1.0")))))
    (search-paths native-search-paths)
    (synopsis "GObject introspection tools and libraries")
    (description "GObject introspection is a middleware layer between
C libraries (using GObject) and language bindings.  The C library can be scanned
at compile time and generate metadata files, in addition to the actual native
C library.  Then language bindings can read this metadata and automatically
provide bindings to call into the C library.")
    (home-page "https://wiki.gnome.org/Projects/GObjectIntrospection")
    (license
     (list
      ;; For library.
      license:lgpl2.0+
      ;; For tools.
      license:gpl2+))))

(define gobject-introspection
  (let ((base gobject-introspection-minimal))
    (package/inherit base
      (native-inputs
       (modify-inputs (package-native-inputs base)
         (replace "glib" glib)))
      (propagated-inputs
       (modify-inputs (package-propagated-inputs base)
         (replace "glib" glib))))))

(define intltool
  (package
    (name "intltool")
    (version "0.51.0")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://launchpad.net/intltool/trunk/"
                                 version "/+download/intltool-"
                                 version ".tar.gz"))
             (patches (search-patches "intltool-perl-compatibility.patch"))
             (sha256
              (base32
               "1karx4sb7bnm2j67q0q74hspkfn6lqprpy5r99vkn5bb36a4viv7"))))
    (build-system gnu-build-system)
    (inputs
     (list file))
    (propagated-inputs
     `(;; Propagate gettext because users expect it to be there, and so does
       ;; the `intltool-update' script.
       ("gettext" ,gettext-minimal)

       ("perl-xml-parser" ,perl-xml-parser)
       ("perl" ,perl)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-file-references
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((file (assoc-ref inputs "file")))
               (substitute* "intltool-update.in"
                 (("`file") (string-append "`" file "/bin/file")))
               #t))))))
    (home-page "https://launchpad.net/intltool/+download")
    (synopsis "Tools to centralise translations of different file formats")
    (description
     "Intltool is a set of tools to centralise translations of many different
file formats using GNU gettext-compatible PO files.

The intltool collection can be used to do these things:

    Extract translatable strings from various source files (.xml.in,
    glade, .desktop.in, .server.in, .oaf.in).

    Collect the extracted strings together with messages from traditional
    source files (.c, .h) in po/$(PACKAGE).pot.

    Merge back the translations from .po files into .xml, .desktop and
    oaf files.  This merge step will happen at build resp. installation time.")
    (license license:gpl2+)))

(define itstool
  (package
    (name "itstool")
    (version "2.0.7")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://files.itstool.org/itstool/itstool-"
                                 version ".tar.bz2"))
             (sha256
              (base32
               "1jl7gsr7aclb9nvqazr039m86y7f7ivfhl2pixcrbfqjkb97r6kb"))))
    (build-system gnu-build-system)
    (inputs
     (list bash-minimal libxml2 python-libxml2 python))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'wrap-program
            (lambda _
              (wrap-program (string-append #$output "/bin/itstool")
                `("GUIX_PYTHONPATH" = (,(getenv "GUIX_PYTHONPATH")))))))))
    (home-page "https://itstool.org")
    (synopsis "Tool to translate XML documents with PO files")
    (description
     "ITS Tool allows you to translate your XML documents with PO files, using
rules from the W3C Internationalization Tag Set (ITS) to determine what to
translate and how to separate it into PO file messages.

PO files are the standard translation format for GNU and other Unix-like
systems.  They present translatable information as discrete messages, allowing
each message to be translated independently.  In contrast to whole-page
translation, translating with a message-based format like PO means you can
easily track changes to the source document down to the paragraph.  When new
strings are added or existing strings are modified, you only need to update the
corresponding messages.

ITS Tool is designed to make XML documents translatable through PO files by
applying standard ITS rules, as well as extension rules specific to ITS Tool.
ITS also provides an industry standard way for authors to override translation
information in their documents, such as whether a particular element should be
translated.")
    (license license:gpl3+)))

(define dbus-glib
  (package
    (name "dbus-glib")
    (version "0.112")
    (source (origin
             (method url-fetch)
             (uri
              (string-append "https://dbus.freedesktop.org/releases/dbus-glib/dbus-glib-"
                             version ".tar.gz"))
             (sha256
              (base32
               "0fhlkdqyzbh89bdslwsfc5fbdvkiv6g840ami4rnwa6dvz60smbx"))))
    (build-system gnu-build-system)
    (arguments
     (if (%current-target-system)
         `(#:configure-flags
           ;; Run a native 'dbus-binding-tool' instead of a cross-compiled
           ;; 'dbus-binding-tool' when cross-compiling.
           ,#~(list
               (string-append
                "--with-dbus-binding-tool="
                #+(file-append this-package "/bin/dbus-binding-tool"))))
         '()))
    (propagated-inputs ; according to dbus-glib-1.pc
     (list dbus glib))
    (inputs
     (list expat))
    (native-inputs
     (list `(,glib "bin") pkg-config))
    (home-page "https://dbus.freedesktop.org/doc/dbus-glib/")
    (synopsis "D-Bus GLib bindings")
    (description
     "GLib bindings for D-Bus.  The package is obsolete and superseded
by GDBus included in Glib.")
    (license license:gpl2)))                     ; or Academic Free License 2.1

(define-public libaccounts-glib
  (package
    (name "libaccounts-glib")
    (version "1.26")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.com/accounts-sso/libaccounts-glib")
                    (commit (string-append "VERSION_" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1fdvvzbz23q5c0jnzryinkmcymd0zcs2pdn4fvibg34pvybb4li9"))))
    (build-system meson-build-system)
    (native-inputs (list dbus
                         `(,glib "bin")
                         gobject-introspection
                         gtk-doc
                         pkg-config
                         vala))
    (inputs (list check python python-pygobject))
    (propagated-inputs (list glib libxml2 sqlite))
    (arguments
     (list #:tests? #f                  ;one test fails.
           #:imported-modules `((guix build python-build-system)
                                ,@%meson-build-system-modules)
           #:modules '(((guix build python-build-system)
                        #:select (python-version))
                       (guix build meson-build-system)
                       (guix build utils))
           ;; don't try installing to python store path.
           #:configure-flags
           #~(list (string-append "-Dpy-overrides-dir="
                                  #$output "/lib/python"
                                  (python-version #$(this-package-input
                                                     "python"))
                                  "/site-packages/gi/overrides"))
           #:phases #~(modify-phases %standard-phases
                        (replace 'check
                          (lambda* (#:key tests? #:allow-other-keys)
                            (when tests?
                              (invoke "dbus-run-session" "--" "meson" "test"
                                      "--print-errorlogs")))))))
    (home-page "https://accounts-sso.gitlab.io/")
    (synopsis "Accounts SSO (Single Sign-On) management library for GLib
applications")
    (description
     "Accounts SSO is a framework for application developers who
wish to acquire, use and store web account details and credentials.  It
handles the authentication process of an account and securely stores the
credentials and service-specific settings.")
    (license license:lgpl2.1+)))

(define libsigc++
  (package
    (name "libsigc++")
    (version "3.6.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/libsigc++/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "05qf10lp5vxsi5fbzdphqhbzmys12mxvg4gh14p9zqynvwvkpln3"))))
    (build-system meson-build-system)
    (outputs '("out" "doc"))
    (arguments
     (list #:configure-flags #~(list "-Dbuild-documentation=true")
           #:phases #~(modify-phases %standard-phases
                        (add-after 'install 'move-doc
                          (lambda _
                            (mkdir-p (string-append #$output:doc "/share"))
                            (rename-file
                             (string-append #$output "/share/doc")
                             (string-append #$output:doc "/share/doc")))))))
    (native-inputs
     (list docbook-xml-4.1.2
           docbook-xsl
           graphviz
           doxygen
           m4
           mm-common
           perl
           pkg-config
           libxml2
           libxslt))
    (inputs (list boost))
    (home-page "https://libsigcplusplus.github.io/libsigcplusplus/")
    (synopsis "Type-safe callback system for standard C++")
    (description
     "Libsigc++ implements a type-safe callback system for standard C++.  It
allows you to define signals and to connect those signals to any callback
function, either global or a member function, regardless of whether it is
static or virtual.  It also contains adaptor classes for connection of
dissimilar callbacks and has an ease of use unmatched by other C++ callback
libraries.")
    (license license:lgpl3+)))

(define-public libsigc++-2
  (package
    (inherit libsigc++)
    (name "libsigc++")
    (version "2.9.3")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/libsigc++/"
                       (version-major+minor version)
                       "/libsigc++-" version ".tar.xz"))
       (sha256
        (base32 "0zq963d0sss82q62fdfjs7l9iwbdch51albck18cb631ml0v7y8b"))))
    (build-system gnu-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-after 'install 'move-doc
                 (lambda _
                   (mkdir-p (string-append #$output:doc "/share"))
                   (rename-file
                    (string-append #$output "/share/doc")
                    (string-append #$output:doc "/share/doc")))))))))

(define glibmm
  (package
    (name "glibmm")
    (version "2.82.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/glibmm/"
                                  (version-major+minor version)
                                  "/glibmm-" version ".tar.xz"))
              (sha256
               (base32
                "1dlwm6gmhnz1p84vkn86algdb6b2y439iymqcxf62wvj67zlqs1q"))))
    (build-system meson-build-system)
    (outputs '("out" "doc"))
    (arguments
     (list
      #:configure-flags #~(list "-Dbuild-documentation=true")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'disable-failing-tests
            (lambda _
              (substitute* "tests/meson.build"
                ;; This test uses /etc/fstab as an example file to read from;
                ;; disable it.
                (("[ \t]*.*giomm_simple.*$") "")
                ;; This test does a DNS lookup, and then expects to be able to
                ;; open a TLS session; just skip it.
                (("[ \t]*.*giomm_tls_client.*$") ""))))
          (add-after 'install 'move-doc
            (lambda _
              (mkdir-p (string-append #$output:doc "/share"))
              (rename-file
               (string-append #$output "/share/doc")
               (string-append #$output:doc "/share/doc")))))))
    (native-inputs
     (list graphviz
           doxygen
           `(,glib "bin")
           m4
           mm-common
           perl
           pkg-config
           libxslt))
    (propagated-inputs
     (list libsigc++ glib))
    (home-page "https://gtkmm.org/")
    (synopsis "C++ interface to the GLib library")
    (description
     "Glibmm provides a C++ programming interface to the part of GLib that are
useful for C++.")
    (license license:lgpl2.1+)))

(define-public glibmm-2.76
  (package
    (inherit glibmm)
    (name "glibmm")
    (version "2.76.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/glibmm/"
                                  (version-major+minor version)
                                  "/glibmm-" version ".tar.xz"))
              (sha256
               (base32
                "1cia8vrpwzn8zwalws42mga5hi965840m5s8dvfzv55xx86dhdw6"))))))

 (define-public glibmm-2.66
   (package
    (inherit glibmm)
    (name "glibmm")
    (version "2.66.6")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/glibmm/"
                       (version-major+minor version)
                       "/glibmm-" version ".tar.xz"))
       (sha256
        (base32 "0bqm9vqwhas69q6n89wd2xgxvrlkpxra13dzsx8m67hqk0jp8n2k"))))
     (propagated-inputs
      (modify-inputs (package-propagated-inputs glibmm)
        (replace "libsigc++" libsigc++-2)))))

(define-public python-pygobject
  (package
    (name "python-pygobject")
    (version "3.50.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/pygobject/"
                           (version-major+minor version)
                           "/pygobject-" version ".tar.xz"))
       (sha256
        (base32
         "04i28xrb9fxkmn9j2mmsl0lbmk9blgjcl8hnxrbx90d8nmsnx0wd"))
       (modules '((guix build utils)))
       (snippet
        ;; We disable these tests in a snippet so that they are inherited
        ;; by the Python 2 variant which is built differently.
        #~(with-directory-excursion "tests"
            ;; FIXME: These tests require Gdk and/or Gtk 4.
            (for-each delete-file
                      '("test_atoms.py" "test_overrides_gtk.py"
                        "test_overrides_gdk.py"))))))
    (build-system meson-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                ;; The default 90 seconds can be too low on slower machines.
                (invoke "meson" "test" "--timeout-multiplier" "5")))))))
    (native-inputs
     (list `(,glib "bin")
           pkg-config
           python-pytest
           python-wrapper)) ; For patching shebangs
    (inputs
     (list python python-pycairo gobject-introspection))
    (propagated-inputs
     ;; pygobject-3.0.pc refers to all these.
     (list glib libffi))
    ;; For finding typelib files, since gobject-introscpetion isn't propagated.
    (native-search-paths (package-native-search-paths gobject-introspection))
    (home-page "https://live.gnome.org/PyGObject")
    (synopsis "Python bindings for GObject")
    (description
     "Python bindings for GLib, GObject, and GIO.")
    (properties
     '((upstream-name . "pygobject")))
    (license license:lgpl2.1+)))

(define-public perl-glib
  (package
    (name "perl-glib")
    (version "1.3294")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://cpan/authors/id/X/XA/XAOC/Glib-"
                    version ".tar.gz"))
              (sha256
               (base32
                "1fsw9sjfz1irlhnsk5n1xpb181vvq1dsxrw5vrsp066cdflga5fp"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-extutils-depends perl-extutils-pkgconfig))
    (propagated-inputs
     (list glib))
    (home-page "https://metacpan.org/release/Glib")
    (synopsis "Perl wrappers for the GLib utility and Object libraries")
    (description "This module provides perl access to GLib and GLib's GObject
libraries.  GLib is a portability and utility library; GObject provides a
generic type system with inheritance and a powerful signal system.  Together
these libraries are used as the foundation for many of the libraries that make
up the Gnome environment, and are used in many unrelated projects.")
    (license license:lgpl2.1+)))

(define-public perl-glib-object-introspection
  (package
    (name "perl-glib-object-introspection")
    (version "0.051")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/X/XA/XAOC/"
                           "Glib-Object-Introspection-" version ".tar.gz"))
       (sha256
        (base32 "12802l87mx65lswiwlc394fkb74cmsqn88n2qy119b40rhfn2sb5"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-extutils-depends perl-extutils-pkgconfig))
    (propagated-inputs
     (list gobject-introspection perl-cairo-gobject perl-glib))
    (home-page "https://metacpan.org/dist/Glib-Object-Introspection")
    (synopsis "Dynamically create Perl language bindings")
    (description "Glib::Object::Introspection uses the gobject-introspection and
libffi projects to dynamically create Perl bindings for a wide variety of
libraries.  Examples include gtk+, webkit, libsoup and many more.")
    (license license:lgpl2.1+)))

(define-public telepathy-glib
  (package
    (name "telepathy-glib")
    (version "0.24.2")
    (source
     (origin
      (method url-fetch)
       (uri
        (string-append
         "https://telepathy.freedesktop.org/releases/telepathy-glib/"
         "telepathy-glib-" version ".tar.gz"))
       (patches (search-patches "telepathy-glib-fix-test.patch"))
       (sha256
        (base32
         "1w3kja8j3gz2apal79bi3hq44xk5g78aphrqbw983l6df7bp98xh"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--enable-vala-bindings")
       ;; When spawned in parallel, the dbus daemons may fail to shut down
       ;; cleanly.  This issue appears to have been closed upstream due to low
       ;; information, but still continues to haunt folks.  See also
       ;; <https://gitlab.freedesktop.org/telepathy/telepathy-glib/-/issues/134>.
       #:parallel-tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'disable-failing-tests
           (lambda _
             ;; None of the tests below are able to find the org.gtk.vfs.Daemon
             ;; service file provided by gvfs.
             (substitute* "tests/dbus/Makefile.in"
               (("test-contacts\\$\\(EXEEXT\\)") "")
               (("test-file-transfer-channel\\$\\(EXEEXT\\)") "")
               (("test-stream-tube\\$\\(EXEEXT\\)") "")))))))
    (native-inputs
     (list `(,glib "bin") ; uses glib-mkenums
           gobject-introspection
           pkg-config
           python-minimal-wrapper
           vala
           libxslt))
    (propagated-inputs
     ;; There are all in the Requires.private field of telepathy-glib.pc.
     (list dbus dbus-glib glib))
    (home-page "https://telepathy.freedesktop.org/wiki/")
    (synopsis "GLib Real-time communications framework over D-Bus")
    (description "Telepathy is a flexible, modular communications framework
that enables real-time communication over D-Bus via pluggable protocol
backends.  Telepathy is a communications service that can be accessed by
many applications simultaneously.

This package provides the library for GLib applications.")
    (license license:lgpl2.1+)))

(define-public dbus-c++
  (package
    (name "dbus-c++")
    (version "0.9.0")
    (source (origin
              (method url-fetch)
              (uri
               (string-append
                "mirror://sourceforge/dbus-cplusplus/dbus-c%2B%2B/"
                version "/libdbus-c%2B%2B-" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (patches (search-patches "dbus-c++-gcc-compat.patch"
                                       "dbus-c++-threading-mutex.patch"))
              (sha256
               (base32
                "0qafmy2i6dzx4n1dqp6pygyy6gjljnb7hwjcj2z11c1wgclsq4dw"))))
    (build-system gnu-build-system)
    (propagated-inputs
     (list dbus))                      ;mentioned in the pkg-config file
    (inputs
     (list efl expat glib libunwind))
    (native-inputs
     (list pkg-config))
    (arguments
     `(;; The 'configure' machinery fails to detect that it needs -lpthread.
       #:configure-flags (list "LDFLAGS=-lpthread")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'add-missing-header
           (lambda _
             (substitute* "include/dbus-c++/eventloop-integration.h"
               (("#include <errno.h>")
                "#include <errno.h>\n#include <unistd.h>"))
             #t)))))
    (synopsis "D-Bus API for C++")
    (description "This package provides D-Bus client API bindings for the C++
programming language.  It also provides the @command{dbusxx-xml2cpp} and
@command{dbusxx-introspect} commands.")
    (home-page "https://sourceforge.net/projects/dbus-cplusplus/")
    (license license:lgpl2.1+)))

(define-public dbus-cxx
  (package
    (name "dbus-cxx")
    (version "2.4.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/dbus-cxx/dbus-cxx")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0c9q2bjs4m66zq0qysyip8fnkvvjpj46rkjcvw15nhmfhzbq16ag"))
              (modules '((guix build utils)))
              (snippet '(delete-file-recursively "tools/libcppgenerate"))))
    (build-system cmake-build-system)
    (arguments
     (list #:configure-flags #~(list "-DBUILD_TESTING=ON"
                                     "-DENABLE_TOOLS=ON"
                                     "-DENABLE_GLIB_SUPPORT=ON"
                                     "-DTOOLS_BUNDLED_CPPGENERATE=OFF")
           #:phases
           #~(modify-phases %standard-phases
               (replace 'check
                 (lambda* (#:key tests? #:allow-other-keys)
                   (when tests?
                     ;; There is no /etc/machine-id file in the build
                     ;; environment.
                     (invoke "ctest" "-E" "test-machine-uuid-method")))))))
    ;; These are propagated due to being referenced in headers and pkg-config
    ;; .pc files.
    (propagated-inputs (list glib libsigc++))
    (inputs (list dbus expat libcppgenerate popt))
    (native-inputs (list pkg-config))
    (synopsis "C++ wrapper for dbus")
    (description "Dbus-cxx is a C++ wrapper for dbus.\n
It exposes the C API to allow direct manipulation and
relies on sigc++ to provide an Oriented Object interface.\n
This package provide 2 utils:
@enumerate
@item @command{dbus-cxx-xml2cpp} to generate proxy and adapter
@item @command{dbus-cxx-introspect} to introspect a dbus interface
@end enumerate

Some codes examples can be find at:
@url{https://dbus-cxx.github.io/examples.html}")
    (home-page "https://dbus-cxx.github.io/")
    (license (list license:lgpl3+ license:bsd-3)))) ;dual licensed

(define-public sdbus-c++
  (package
    (name "sdbus-c++")
    (version "2.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Kistler-Group/sdbus-cpp")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1s6vhzln0rvac2r3v8nq08hsjhyz3y46fsy18i23ppjm30apkiav"))))
    (build-system cmake-build-system)
    (arguments
     (list
      ;; Avoid the integration test, which requires a system bus.
      #:test-target "sdbus-c++-unit-tests"
      #:configure-flags #~(list "-DSDBUSCPP_BUILD_CODEGEN=ON"
                                "-DSDBUSCPP_BUILD_TESTS=ON"
                                ;; Do not install tests.
                                "-DSDBUSCPP_TESTS_INSTALL_PATH=/tmp"
                                "-DCMAKE_VERBOSE_MAKEFILE=ON")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'do-not-install-tests
            (lambda _
              (substitute* "tests/CMakeLists.txt"
                (("/etc/dbus-1/system.d") "/tmp")))))))
    (native-inputs (list googletest pkg-config))
    (inputs (list expat))
    (propagated-inputs (list elogind)) ;required by sdbus-c++.pc
    (home-page "https://github.com/Kistler-Group/sdbus-cpp")
    (synopsis "High-level C++ D-Bus library")
    (description "@code{sdbus-c++} is a high-level C++ D-Bus library designed
to provide easy-to-use yet powerful API in modern C++.  It adds another layer
of abstraction on top of @code{sd-bus}, the C D-Bus implementation by systemd.")
    (license license:lgpl2.1+)))

;; TODO: Remove once libjami can use newer sdbus-c++.
(define-public sdbus-c++-1.4.0
  (package
    (inherit sdbus-c++)
    (name "sdbus-c++")
    (version "1.4.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Kistler-Group/sdbus-cpp")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "111l2rl0pg9r5cdrhqpac4v22cnq41skxxfk3cng81l0n05v1sh0"))))
    (arguments
     (substitute-keyword-arguments (package-arguments sdbus-c++)
       ((#:configure-flags flags ''())
        #~(list "-DBUILD_CODE_GEN=ON"
                "-DBUILD_TESTS=ON"
                ;; Do not install tests.
                "-DTESTS_INSTALL_PATH=/tmp"
                "-DCMAKE_VERBOSE_MAKEFILE=ON"))
       ((#:phases phases)
        #~(modify-phases #$phases
            (add-after 'unpack 'fix-elogind-requirement
              (lambda _
                ;; sdbus-c++.pc requires 'elogind', but it should
                ;; require 'libelogind'. Fixed after 1.4.0 with
                ;; fb9e4ae37152648a67814458d3ff673b1d3ca089
                (substitute* "pkgconfig/sdbus-c++.pc.in"
                  (("@LIBSYSTEMD@")
                   "libelogind"))))))))))

(define-public appstream-glib
  (package
    (name "appstream-glib")
    (version "0.8.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://people.freedesktop.org/~hughsient/"
                                  "appstream-glib/releases/"
                                  "appstream-glib-" version ".tar.xz"))
              (sha256
               (base32
                "04fgm19p4qf970dvj5phk1bml8zwai1wc78mmghsdz30qmj40xc4"))))
    (build-system meson-build-system)
    (native-inputs
     (list gettext-minimal
           `(,glib "bin") ;for glib-compile-resources
           gsettings-desktop-schemas ;for ‘org.gnome.system.proxy’
           pkg-config))
    (propagated-inputs
     (list gcab ;for .pc file
           gdk-pixbuf ;same
           `(,util-linux "lib"))) ;libuuid, for .pc file
    (inputs
     (list curl
           gperf
           gtk+
           json-glib
           libarchive
           glib))
    (arguments
     (list
      #:configure-flags
      #~(list "-Ddep11=false"
              "-Dintrospection=false"    ; avoid g-ir-scanner dependency
              "-Drpm=false")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-tests
            (lambda _
              (substitute* "libappstream-glib/as-self-test.c"
                (("g_test_add_func.*as_test_store_local_appdata_func);") ""))))
          (add-before 'check 'set-home
            (lambda _
              ;; Some tests want write access there.
              (setenv "HOME" "/tmp"))))))
    (home-page "https://github.com/hughsie/appstream-glib")
    (synopsis "Library for reading and writing AppStream metadata")
    (description
     "This library provides objects and helper methods to help
reading and writing
@uref{https://www.freedesktop.org/wiki/Distributions/AppStream,AppStream}
metadata.")
    (license license:lgpl2.1+)))

(define perl-net-dbus
  (package
    (name "perl-net-dbus")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/D/DA/DANBERR/Net-DBus-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1g0w8i5scmh7kfy9mmvv8q326627qf38z26mvczmn8x1yjgar8g7"))))
    (build-system perl-build-system)
    (native-inputs
     (list pkg-config perl-test-pod perl-test-pod-coverage))
    (inputs
     (list dbus))
    (propagated-inputs
     (list perl-xml-twig))
    (home-page "https://metacpan.org/release/Net-DBus")
    (synopsis "Extension for the DBus bindings")
    (description "@code{Net::DBus} provides a Perl XS API to the DBus
inter-application messaging system.  The Perl API covers the core base level
of the DBus APIs, not concerning itself yet with the GLib or QT wrappers.")
    (license license:perl-license)))

(define perl-net-dbus-glib
  (package
    (name "perl-net-dbus-glib")
    (version "0.33.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/D/DA/DANBERR/"
                           "Net-DBus-GLib-" version ".tar.gz"))
       (sha256
        (base32
         "1z4mbv8z0rad604xahijpg5szzi8qak07hbahh230z4jf96fkxvj"))))
    (build-system perl-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list dbus-glib))
    (home-page "https://metacpan.org/release/Net-DBus-GLib")
    (synopsis "Perl extension for the DBus GLib bindings")
    (description "This package provides an extension to the @code{Net::DBus}
module allowing integration with the GLib mainloop.  To integrate with the
main loop, simply get a connection to the bus via the methods in
@code{Net::DBus::GLib} rather than the usual @code{Net::DBus} module.  Every
other API remains the same.")
    (license license:gpl2+)))

(define-public template-glib
  (package
    (name "template-glib")
    (version "3.36.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0j4rc4jvxk5pzmx5831s90m9g4cfyp10hxd1ndiyg34806jg6800"))))
    (build-system meson-build-system)
    (arguments
     (list #:configure-flags #~'("-D" "gtk_doc=true")))
    (inputs (list gettext-minimal glib gobject-introspection))
    (native-inputs
     (list bison
           flex
           `(,glib "bin") ;for glib-mkenums
           gtk-doc/stable
           pkg-config
           vala))
    (home-page "https://gitlab.gnome.org/GNOME/template-glib")
    (synopsis "Library for template expansion")
    (description
     "Template-GLib is a library to help you generate text based on a template and
user defined state.  Template-GLib does not use a language runtime, so it is
safe to use from any GObject-Introspectable language.

Template-GLib allows you to access properties on GObjects as well as call
simple methods via GObject-Introspection.")
    (license license:lgpl2.1+)))

(define-public xdg-dbus-proxy
  (package
    (name "xdg-dbus-proxy")
    (version "0.1.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/flatpak/xdg-dbus-proxy"
                                  "/releases/download/" version
                                  "/xdg-dbus-proxy-" version ".tar.xz"))
              (sha256
               (base32
                "1yv10v7gpv5z0iii7p3rs2h9wx6sigldycjlkpyyal06iapwy786"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config
           ;; For tests.
           dbus
           ;; These are required to build the manual.
           docbook-xml-4.3
           docbook-xsl
           libxml2
           libxslt))
    (inputs
     (list glib))
    (home-page "https://github.com/flatpak/xdg-dbus-proxy")
    (synopsis "D-Bus connection proxy")
    (description
     "xdg-dbus-proxy is a filtering proxy for D-Bus connections.  It can be
used to create D-Bus sockets inside a Linux container that forwards requests
to the host system, optionally with filters applied.")
    (license license:lgpl2.1+)))

(define-public dbus-test-runner
  (package
    (name "dbus-test-runner")
    (version "19.04.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://launchpad.net/dbus-test-runner/"
                    (version-major+minor version) "/" version
                    "/+download/dbus-test-runner-" version ".tar.gz"))
              (sha256
               (base32
                "0xnbay58xn0hav208mdsg8dd176w57dcpw1q2k0g5fh9v7xk4nk4"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'fix-test-paths
           ;; add missing space
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "Makefile.in"
               (("#!/bin/bash") (string-append "#!" (which "bash"))))
             (substitute* "tests/Makefile.in"
               (("/bin/sh") (which "sh"))
               (("#!/bin/bash") (string-append "#!" (which "bash")))
               (("echo cat") (string-append "echo " (which "cat")))
               (("/bin/true") (which "true")))
             #t)))))
    (inputs
     (list gtk+ glib dbus-glib))
    (native-inputs
     `(("glib:bin" ,glib "bin")
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)
       ;; following used for tests
       ("python" ,python)
       ("python-dbusmock" ,python-dbusmock)
       ("xvfb" ,xorg-server-for-tests)))
    (home-page "https://launchpad.net/dbus-test-runner")
    (synopsis "Run a executables under a new DBus session for testing")
    (description "A small little utility to run a couple of executables under a
new DBus session for testing.")
    (license license:gpl3)))

(define-public libdex
  (package
    (name "libdex")
    (version "0.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/" name "/"
                           (version-major+minor version) "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32 "183qsc46n0pf3whlamfrbckbsxzfnmj54hvhdxpvvaj37snpam4m"))))
    (build-system meson-build-system)
    (arguments
     (list #:configure-flags #~'("-D" "docs=true")))
    (native-inputs
     (list gobject-introspection
           gi-docgen
           pkg-config
           vala))
    (inputs
     (list glib
           libsoup))
    (home-page "https://gitlab.gnome.org/GNOME/libdex")
    (synopsis "Library for future-based programming with glib")
    (description
     "Dex provides Future-based programming for GLib-based applications.

It both integrates with and brings new features for application and library
authors who want to manage concurrent code.

Dex also provides Fibers which allow writing synchronous looking code in C
that uses asynchronous and future-based APIs.")
    (license license:lgpl2.1+)))

(define-public cppgir
  (package
    (name "cppgir")
    (version "2.0")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://gitlab.com/mnauw/cppgir")
         (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0cj4myqzb28hgb7zlxlba9y8n4ysxkvv2y9wy6f7ps58mr18h7bl"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list "-DINTERNAL_EXPECTED=OFF")))
    (inputs (list boost fmt expected-lite))
    (home-page "https://gitlab.com/mnauw/cppgir")
    (synopsis "C++ bindings generator for GObject introspection")
    (description "cppgir processes @file{.gir} files derived from GObject
introspection annotations into a set of C++ files defining suitable
namespaces, classes and other types that together form a C++ binding.")
    (license license:expat)))

;; telegram-desktop requires a more recent version of cppgir
(define-public cppgir-for-telegram-desktop
  (let ((commit "2e96cab8ed40df326815b87b1e4b449e0c1a5947")
        (revision "0"))
    (package
      (inherit cppgir)
      (name "cppgir-for-telegram-desktop")
      (version (git-version "2.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://gitlab.com/mnauw/cppgir")
           (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0rdcgnriw8s5fqyx2v4218ii647l4fl1s9crnm9ihzf9bpl2p5p9")))))))
