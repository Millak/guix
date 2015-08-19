;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013, 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2013 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2014, 2015 Mark H Weaver <mhw@netris.org>
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
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages file)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages m4)

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
            telepathy-glib))

(define dbus
  (package
    (name "dbus")
    (version "1.8.16")
    (source (origin
             (method url-fetch)
             (uri
              (string-append "http://dbus.freedesktop.org/releases/dbus/dbus-"
                             version ".tar.gz"))
             (sha256
              (base32
               "01rba8mp8kqvmy6ibdmi806kjr3m14swnskqk02gyhykxxl54ybz"))
             (patches (list (search-patch "dbus-localstatedir.patch")))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags (list ;; Install the system bus socket under /var.
                               "--localstatedir=/var"

                               ;; XXX: Fix the following to allow system-wide
                               ;; config.
                               ;; "--sysconfdir=/etc"

                               "--with-session-socket-dir=/tmp")
       #:phases (alist-cons-after
                 'install 'post-install
                 (lambda* (#:key outputs #:allow-other-keys)
                   ;; 'dbus-launch' bails out if the 'session.d' directory
                   ;; below is missing, so create it along with its companion.
                   (let ((out (assoc-ref outputs "out")))
                     (mkdir (string-append out "/etc/dbus-1/session.d"))
                     (mkdir (string-append out "/etc/dbus-1/system.d"))
                     #t))
                 %standard-phases)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("expat" ,expat)

       ;; Add a dependency on libx11 so that 'dbus-launch' has support for
       ;; '--autolaunch'.
       ("libx11" ,libx11)))

    (home-page "http://dbus.freedesktop.org/")
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

;; XXX This fixed version is needed only for 'dbus-daemon-launch-helper'.
;; FIXME: Integrate this change into the main 'dbus' package in the next
;; core-updates cycle.
(define dbus-fixed
  (package
    (inherit dbus)
    (arguments
     (substitute-keyword-arguments (package-arguments dbus)
       ((#:phases phases)
        `(modify-phases ,phases
           (add-after
            'unpack 'add-standard-system-service-dir
            (lambda _
              (substitute* "dbus/dbus-sysdeps-util-unix.c"
                (("standard_search_path\\[\\] =" all)
                 (format #f "~a ~s" all "/run/current-system/profile/share:")))
              #t))))))))

(define glib
  (package
   (name "glib")
   (version "2.44.0")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnome/sources/"
                                name "/" (string-take version 4) "/"
                                name "-" version ".tar.xz"))
            (sha256
             (base32
              "1fgmjv3yzxgbks31h42201x2izpw0sd84h8dfw0si3x00sqn5lzj"))
            (patches (list (search-patch "glib-tests-homedir.patch")
                           (search-patch "glib-tests-desktop.patch")
                           (search-patch "glib-tests-prlimit.patch")
                           (search-patch "glib-tests-timer.patch")
                           (search-patch "glib-tests-gapplication.patch")))))
   (build-system gnu-build-system)
   (outputs '("out"           ; everything
              "bin"           ; glib-mkenums, gtester, etc.; depends on Python
              "doc"))         ; 20 MiB of GTK-Doc reference
   (inputs
    `(("coreutils" ,coreutils)
      ("libffi" ,libffi)
      ("zlib" ,zlib)
      ("tzdata" ,tzdata)))     ; for tests/gdatetime.c
   (native-inputs
    `(("gettext" ,gnu-gettext)
      ("dbus" ,dbus)                              ; for GDBus tests
      ("pkg-config" ,pkg-config)
      ("python" ,python-wrapper)
      ("perl" ,perl)                              ; needed by GIO tests
      ("bash" ,bash)))
   (arguments
    '(#:phases (alist-cons-before
                'build 'pre-build
                (lambda* (#:key inputs outputs #:allow-other-keys)
                  ;; For tests/gdatetime.c.
                  (setenv "TZDIR"
                          (string-append (assoc-ref inputs "tzdata")
                                         "/share/zoneinfo"))

                  ;; Some tests want write access there.
                  (setenv "XDG_CACHE_HOME" (getcwd))

                  (substitute* '("glib/gspawn.c"
                                 "glib/tests/utils.c"
                                 "tests/spawn-test.c")
                    (("/bin/sh")
                     (string-append (assoc-ref inputs "bash") "/bin/sh")))

                  ;; Disable a test that requires dbus.
                  (substitute* "gio/tests/gdbus-serialization.c"
                    (("g_test_add_func \\(\"/gdbus/message-serialize/double-array\", test_double_array\\);" all)
                     (string-append "/* " all " */"))))
                %standard-phases)

      ;; Note: `--docdir' and `--htmldir' are not honored, so work around it.
      #:configure-flags (list (string-append "--with-html-dir="
                                             (assoc-ref %outputs "doc")
                                             "/share/gtk-doc/html"))

      ;; In 'gio/tests', 'gdbus-test-codegen-generated.h' is #included in a
      ;; file that gets compiled possibly before it has been fully generated.
      #:parallel-tests? #f))

   (native-search-paths
    ;; This variable is not really "owned" by GLib, but several related
    ;; packages refer to it: gobject-introspection's tools use it as a search
    ;; path for .gir files, and it's also a search path for schemas produced
    ;; by 'glib-compile-schemas'.
    (list (search-path-specification
           (variable "XDG_DATA_DIRS")
           (files '("share")))
          ;; To load extra gio modules from glib-networking, etc.
          (search-path-specification
           (variable "GIO_EXTRA_MODULES")
           (files '("lib/gio/modules")))))
   (search-paths native-search-paths)

   (synopsis "Thread-safe general utility library; basis of GTK+ and GNOME")
   (description
    "GLib provides data structure handling for C, portability wrappers,
and interfaces for such runtime functionality as an event loop, threads,
dynamic loading, and an object system.")
   (home-page "http://developer.gnome.org/glib/")
   (license license:lgpl2.0+)))                        ; some files are under lgpl2.1+

(define gobject-introspection
  (package
    (name "gobject-introspection")
    (version "1.44.0")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnome/sources/"
                   "gobject-introspection/" (version-major+minor version)
                   "/gobject-introspection-" version ".tar.xz"))
             (sha256
              (base32 "1b972qg2yb51sdavfvb6kc19akwc15c1bwnbg81vadxamql2q33g"))
             (patches (list
                       (search-patch "gobject-introspection-cc.patch")
                       (search-patch
                        "gobject-introspection-girepository.patch")
                       (search-patch
                        "gobject-introspection-absolute-shlib-path.patch")))))
    (build-system gnu-build-system)
    (inputs
     `(("bison" ,bison)
       ("cairo" ,cairo)
       ("flex" ,flex)
       ("glib" ,glib)
       ("pkg-config" ,pkg-config)
       ("python-2" ,python-2)))
    (native-inputs
     `(("glib" ,glib "bin")))
    (propagated-inputs
     `(;; In practice, GIR users will need libffi when using
       ;; gobject-introspection.
       ("libffi" ,libffi)))
    (native-search-paths
     (list (search-path-specification
            (variable "GI_TYPELIB_PATH")
            (files '("lib/girepository-1.0")))))
    (search-paths native-search-paths)
    (arguments
     `(;; The patch 'gobject-introspection-absolute-shlib-path.patch' causes
       ;; some tests to fail.
       #:tests? #f))
    (home-page "https://wiki.gnome.org/GObjectIntrospection")
    (synopsis "Generate interface introspection data for GObject libraries")
    (description
     "GObject introspection is a middleware layer between C libraries (using
GObject) and language bindings.  The C library can be scanned at compile time
and generate a metadata file, in addition to the actual native C library.  Then
at runtime, language bindings can read this metadata and automatically provide
bindings to call into the C library.")
    ; Some bits are distributed under the LGPL2+, others under the GPL2+
    (license license:gpl2+)))

(define intltool
  (package
    (name "intltool")
    (version "0.50.2")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://launchpad.net/intltool/trunk/"
                                 version "/+download/intltool-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "01j4yd7i84n9nk4ccs6yifg84pp68nr9by57jdbhj7dpdxf5rwk7"))))
    (build-system gnu-build-system)
    (inputs
     `(("file" ,file)))
    (propagated-inputs
     `(;; Propagate gettext because users expect it to be there, and so does
       ;; the `intltool-update' script.
       ("gettext" ,gnu-gettext)

       ("perl-xml-parser" ,perl-xml-parser)
       ("perl" ,perl)))
    (arguments
     `(#:phases (alist-cons-after
                 'unpack 'patch-file-references
                 (lambda* (#:key inputs #:allow-other-keys)
                   (let ((file (assoc-ref inputs "file")))
                     (substitute* "intltool-update.in"
                       (("`file") (string-append "`" file "/bin/file")))))
                 %standard-phases)))
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
    (version "2.0.2")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://files.itstool.org/itstool/itstool-"
                                 version ".tar.bz2"))
             (sha256
              (base32
               "0fh34wi52i0qikgvlmrcpf1vx6gc1xqdad4539l4d9hikfsrz45z"))))
    (build-system gnu-build-system)
    (propagated-inputs
     `(("libxml2" ,libxml2)
       ("python2-libxml2" ,python2-libxml2)
       ("python-2" ,python-2)))
    (home-page "http://www.itstool.org")
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
    (version "0.104")
    (source (origin
             (method url-fetch)
             (uri
              (string-append "http://dbus.freedesktop.org/releases/dbus-glib/dbus-glib-"
                             version ".tar.gz"))
             (sha256
              (base32
               "1xi1v1msz75qs0s4lkyf1psrksdppa3hwkg0mznc6gpw5flg3hdz"))))
    (build-system gnu-build-system)
    (propagated-inputs ; according to dbus-glib-1.pc
     `(("dbus" ,dbus)
       ("glib" ,glib)))
    (inputs
     `(("expat" ,expat)))
    (native-inputs
     `(("glib" ,glib "bin")
       ("pkg-config" ,pkg-config)))
    (home-page "http://dbus.freedesktop.org/doc/dbus-glib/")
    (synopsis "D-Bus GLib bindings")
    (description
     "GLib bindings for D-Bus.  The package is obsolete and superseded
by GDBus included in Glib.")
    (license license:gpl2)))                     ; or Academic Free License 2.1

(define libsigc++
  (package
    (name "libsigc++")
    (version "2.4.1")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnome/sources/libsigc++/"
                                 (version-major+minor version) "/"
                                 name "-" version ".tar.xz"))
             (sha256
              (base32
               "1v0rvkzglzmf67y9nkcppwjwi68j1cy5yhldvcq7xrv8594l612l"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)
                     ("m4" ,m4)))
    (home-page "http://libsigc.sourceforge.net/")
    (synopsis "Type-safe callback system for standard C++")
    (description
     "Libsigc++ implements a type-safe callback system for standard C++.  It
allows you to define signals and to connect those signals to any callback
function, either global or a member function, regardless of whether it is
static or virtual.

It also contains adaptor classes for connection of dissimilar callbacks and
has an ease of use unmatched by other C++ callback libraries.")
    (license license:lgpl2.1+)))

(define glibmm
  (package
    (name "glibmm")
    (version "2.44.0")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnome/sources/glibmm/"
                                 (version-major+minor version)
                                 "/glibmm-" version ".tar.xz"))
             (sha256
              (base32
               "1a1fczy7hcpn24fglyn4i79f4yjc8s50is70q03mb294bm1c02hv"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases (alist-cons-before
                 'build 'pre-build
                 (lambda _
                   ;; This test uses /etc/fstab as an example file to read
                   ;; from; choose a better example.
                   (substitute* "tests/giomm_simple/main.cc"
                     (("/etc/fstab")
                      (string-append (getcwd)
                                     "/tests/giomm_simple/main.cc")))

                   ;; This test does a DNS lookup, and then expects to be able
                   ;; to open a TLS session; just skip it.
                   (substitute* "tests/giomm_tls_client/main.cc"
                     (("Gio::init.*$")
                      "return 77;\n")))
                 %standard-phases)))
    (native-inputs `(("pkg-config" ,pkg-config)
                     ("glib" ,glib "bin")))
    (propagated-inputs
     `(("libsigc++" ,libsigc++)
       ("glib" ,glib)))
    (home-page "http://gtkmm.org/")
    (synopsis "C++ interface to the GLib library")
    (description
     "Glibmm provides a C++ programming interface to the part of GLib that are
useful for C++.")
    (license license:lgpl2.1+)))

(define-public python2-pygobject-2
  (package
    (name "python2-pygobject")
    ;; This was the last version to declare the 2.0 platform number, i.e. its
    ;; pkg-config files were named pygobject-2.0.pc
    (version "2.28.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/pygobject/"
                           (version-major+minor version)
                           "/pygobject-" version ".tar.xz"))
       (sha256
        (base32
         "1f5dfxjnil2glfwxnqr14d2cjfbkghsbsn8n04js2c2icr7iv2pv"))
       (patches
        (list (search-patch
               "python2-pygobject-2-gi-info-type-error-domain.patch")))))
    (build-system gnu-build-system)
    (native-inputs
     `(("which" ,which)
       ("glib-bin" ,glib "bin")         ;for tests: glib-compile-schemas
       ("pkg-config" ,pkg-config)
       ("dbus" ,dbus)))                 ;for tests
    (inputs
     `(("python" ,python-2)
       ("glib"   ,glib)
       ("python2-pycairo" ,python2-pycairo)
       ("gobject-introspection" ,gobject-introspection)))
    (propagated-inputs
     `(("libffi" ,libffi)))             ;mentioned in pygobject-2.0.pc
    (arguments
     `(#:tests? #f                      ;segfaults during tests
       #:configure-flags '("LIBS=-lcairo-gobject")))
    (home-page "https://pypi.python.org/pypi/PyGObject")
    (synopsis "Python bindings for GObject")
    (description
     "Python bindings for GLib, GObject, and GIO.")
    (license license:lgpl2.1+)))

(define-public python-pygobject
  (package
    (name "python-pygobject")
    (version "3.16.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/pygobject/"
                           (version-major+minor version)
                           "/pygobject-" version ".tar.xz"))
       (sha256
        (base32
         "1hqyma73w0lnjcgx68kawhnq84aq92xlkdqphrlc2ppia38dm5kx"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("which" ,which)
       ("glib-bin" ,glib "bin")         ;for tests: glib-compile-schemas
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("python" ,python)
       ("glib"   ,glib)
       ("python-pycairo" ,python-pycairo)
       ("gobject-introspection" ,gobject-introspection)
       ("libffi" ,libffi)))
    (arguments
     ;; TODO: failing tests: test_native_calls_async
     ;; test_native_calls_async_errors test_native_calls_sync
     ;; test_native_calls_sync_errors test_python_calls_async
     ;; test_python_calls_async_error test_python_calls_async_error_result
     ;; test_python_calls_sync test_python_calls_sync_errors
     ;; test_python_calls_sync_noargs test_callback_user_data_middle_none
     ;; test_callback_user_data_middle_single
     ;; test_callback_user_data_middle_tuple
     '(#:tests? #f))
    (home-page "https://live.gnome.org/PyGObject")
    (synopsis "Python bindings for GObject")
    (description
     "Python bindings for GLib, GObject, and GIO.")
    (license license:lgpl2.1+)))

(define-public python2-pygobject
  (package (inherit python-pygobject)
    (name "python2-pygobject")
    (inputs
     `(("python" ,python-2)
       ("glib" ,glib)
       ("python-pycairo" ,python2-pycairo)
       ("gobject-introspection" ,gobject-introspection)
       ("libffi" ,libffi)))))

(define telepathy-glib
  (package
    (name "telepathy-glib")
    (version "0.24.1")
    (source
     (origin
      (method url-fetch)
       (uri
        (string-append
         "http://telepathy.freedesktop.org/releases/telepathy-glib/"
         "telepathy-glib-" version ".tar.gz"))
       (sha256
        (base32
         "1symyzbjmxvksn2ifdkk50lafjm2llf2sbmky062gq2pz3cg23cy"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("glib" ,glib "bin") ; uses glib-mkenums
       ("pkg-config" ,pkg-config)
       ("python" ,python-2)))
    (inputs
     `(("dbus" ,dbus)
       ("dbus-glib" ,dbus-glib)
       ("glib" ,glib)
       ("gobject-introspection" ,gobject-introspection)
       ("libxslt" ,libxslt)))
    (home-page "http://telepathy.freedesktop.org/wiki/")
    (synopsis "GLib Real-time communications framework over D-Bus")
    (description "Telepathy is a flexible, modular communications framework
that enables real-time communication over D-Bus via pluggable protocol
backends.  Telepathy is a communications service that can be accessed by
many applications simultaneously.

This package provides the library for GLib applications.")
    (license license:lgpl2.1+)))
