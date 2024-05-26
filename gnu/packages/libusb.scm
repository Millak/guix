;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2015 Andy Wingo <wingo@igalia.com>
;;; Copyright © 2015, 2016, 2017, 2018, 2019, 2024 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Theodoros Foradis <theodoros@foradis.org>
;;; Copyright © 2017 Jonathan Brielmaier <jonathan.brielmaier@web.de>
;;; Copyright © 2018, 2020–2022 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Vagrant Cascadian <vagrant@debian.org>
;;; Copyright © 2020 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2020 Christopher Howard <christopher@librehacker.com>
;;; Copyright © 2021 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2021 Martin Becze <mjbecze@riseup.net>
;;; Copyright © 2022 Jacob Hrbek <kreyren@rixotstudio.cz>
;;; Copyright © 2023 Sharlatan Hellseher <sharlatanus@gmail.com>
;;; Copyright © 2023 Foundation Devices, Inc. <hello@foundationdevices.com>
;;; Copyright © 2024 hapster <o.rojon@posteo.net>
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

(define-module (gnu packages libusb)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system ant)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages java)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xiph))

(define-public libusb
  (package
    (name "libusb")
    (version "1.0.25")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://github.com/libusb/libusb/"
                          "releases/download/v" version
                          "/libusb-" version ".tar.bz2"))
      (sha256
       (base32 "0j88ym7afy4wj3x789zzxsr04asyjy0mw29gf31blzkrg8cyya4a"))))
    (build-system gnu-build-system)

    ;; XXX: Enabling udev is now recommended, but eudev indirectly depends on
    ;; libusb.
    (arguments `(#:configure-flags '("--disable-udev")))
    ;; (inputs `(("eudev" ,eudev)))

    (home-page "https://libusb.info")
    (synopsis "User-space USB library")
    (description
     "Libusb is a library that gives applications easy access to USB
devices on various operating systems.")
    (license license:lgpl2.1+)))

(define-public libusb-compat
  (package
    (name "libusb-compat")
    (version "0.1.8")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://sourceforge/libusb/"
                          "libusb-compat-" (version-major+minor version) "/"
                          "libusb-compat-" version "/"
                          "libusb-compat-" version ".tar.bz2"))
      (sha256
       (base32 "09q8w00djrkaxbiklcgjwya1w0n3aqavsz06fl0ixv1x9x47d339"))))
    (build-system gnu-build-system)
    (arguments
     (list #:configure-flags
           #~(list "--disable-static"
                   ;; Provide the absolute file name of libusb-1.0.so so
                   ;; dlopen works.
                   (string-append "LIBUSB_1_0_SONAME="
                                  #$(this-package-input "libusb")
                                  "/lib/libusb-1.0.so"))))
    (native-inputs (list autoconf automake libtool pkg-config))
    (inputs (list libusb))
    (home-page "https://libusb.info")
    (synopsis "Compatibility shim for libusb")
    (description
     "Libusb-compat provides a shim allowing applications based on older
version of libusb to run with newer libusb.")
    (license license:lgpl2.1+)))

;; required by 0xffff, which compiles with libusb-compat, but executes only
;; with libusb-0.1
(define-public libusb-0.1
  (package (inherit libusb)
    (version "0.1.12")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://sourceforge/libusb/libusb-0.1 (LEGACY)/"
                          version "/libusb-" version ".tar.gz"))
      (sha256
       (base32
        "0i4bacxkyr7xyqxbmb00ypkrv4swkgm0mghbzjsnw6blvvczgxip"))
      (patches (search-patches "libusb-0.1-disable-tests.patch"))))
    (arguments `(#:configure-flags (list "CFLAGS=-Wno-error")))))

(define-public libusb4java
  ;; There is no public release so we take the latest version from git.
  (let ((commit "0842e8104d8772da873314e233aa625f5651fd34")
        (revision "1"))
    (package
      (name "libusb4java")
      (version (git-version "1.3.1" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/usb4java/libusb4java")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "16hz0h8fvrr764gwj90yny1kxpf0y7p2czr7pdrw3qby21fqkzrq"))))
      (build-system cmake-build-system)
      (arguments
       (list
        #:tests? #f                     ;there are no tests
        #:phases
        #~(modify-phases %standard-phases
            (add-before 'configure 'set-JAVA_HOME
              (lambda _
                (setenv "JAVA_HOME" #$(this-package-native-input "jdk")))))))
      (inputs
       (list libusb))
      (native-inputs
       `(("jdk" ,icedtea "jdk")))
      (home-page "https://github.com/usb4java/libusb4java/")
      (synopsis "JNI bindings to libusb")
      (description
       "This package provides Java JNI bindings to the libusb library for use
with usb4java.")
      (license license:expat))))

(define-public go-github-com-google-gousb
  (package
    ;; See <https://github.com/google/gousb/issues/124> for picking up the
    ;; correct version.
    (name "go-github-com-google-gousb")
    (version "1.1.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/google/gousb")
             (commit (string-append "v" version))))
       (sha256
        (base32 "1rl43y2nn1fysnlvkkcba2rb4d4pqbab8v4v9zw0xv9j4x2r5hv1"))
       (file-name (git-file-name name version))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/google/gousb"))
    (native-inputs
     (list pkg-config))
    ;; It's for purpose to prevent failing of missing libusb when this package
    ;; is included as inputs to build others.
    (propagated-inputs
     (list libusb))
    (home-page "https://github.com/google/gousb")
    (synopsis "Low-level interface for accessing USB devices in Golang")
    (description
     "The gousb package is an attempt at wrapping the libusb library into a
Go-like binding.")
    (license license:asl2.0)))

(define-public java-usb4java
  (package
    (name "java-usb4java")
    (version "1.3.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/usb4java/usb4java")
                     (commit (string-append "usb4java-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0fwf8d2swgm8pmvssy53ixnc0pb5bfvc8iy42mf3dwgvr1zzvgmv"))))
    (build-system ant-build-system)
    (arguments
     (list
      #:jar-name "usb4java.jar"
      #:phases
      #~(modify-phases %standard-phases
          ;; Usually, native libusb4java libraries for all supported systems
          ;; would be included in the jar and extracted at runtime.  Since we
          ;; build everything from source we cannot just bundle pre-built
          ;; binaries for other systems.  Instead, we patch the loader to
          ;; directly return the appropriate library for this system.  The
          ;; downside is that the jar will only work on the same architecture
          ;; that it was built on.
          (add-after 'unpack 'copy-libusb4java
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "src/main/java/org/usb4java/Loader.java"
                (("private static String extractLibrary" line)
                 (string-append
                  line "(final String a, final String b) {"
                  "return \""
                  (search-input-file inputs "/lib/libusb4java.so")
                  "\"; }\n"
                  "private static String _extractLibrary")))))
          (add-after 'unpack 'disable-broken-tests
            (lambda _
              (with-directory-excursion "src/test/java/org/usb4java"
                ;; These tests should only be run when USB devices are present.
                (substitute* '("LibUsbGlobalTest.java"
                               "TransferTest.java")
                  (("this.context = new Context\\(\\);")
                   "this.context = null;"))))))))
    (inputs
     (list libusb4java java-commons-lang3 java-junit java-hamcrest-core))
    (home-page "http://usb4java.org/")
    (synopsis "USB library for Java")
    (description
     "This package provides a USB library for Java based on libusb and
implementing @code{javax.usb} (JSR-80).")
    (license license:expat)))

(define-public python-libusb1
  (package
    (name "python-libusb1")
    (version "3.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "libusb1" version))
       (sha256
        (base32
         "0f45rjgkq4wgyav6dz57ggj34p2l00c9n3d4639ia3z4zvgak4jp"))))
    (build-system python-build-system)
    (arguments
     '(#:modules ((srfi srfi-1)
                  (guix build utils)
                  (guix build python-build-system))
       #:phases
       (modify-phases %standard-phases
         (add-before 'install-license-files 'remove-incorrect-license
           (lambda* (#:key out #:allow-other-keys)
             ;; Was relicensed to LGPL 2.1+, but old COPYING file still left
             ;; in source. Remove it so it does not get installed.
             (delete-file "COPYING")))
         (add-after 'unpack 'fix-libusb-reference
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "usb1/_libusb1.py"
               (("libusb_path = ctypes\\.util\\.find_library\\(base_name\\)")
                (string-append
                 "libusb_path = \""
                 (find (negate symbolic-link?)
                       (find-files (assoc-ref inputs "libusb")
                                   "^libusb.*\\.so\\..*"))
                 "\""))))))))
    (propagated-inputs (list libusb))
    (home-page "https://github.com/vpelletier/python-libusb1")
    (synopsis "Pure-python wrapper for libusb-1.0")
    (description "Libusb is a library that gives applications easy access to
USB devices on various operating systems.  This package provides a Python
wrapper for accessing libusb-1.0.")
    (license license:lgpl2.1+)))

(define-public python-pyusb
  (package
    (name "python-pyusb")
    (version "1.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyusb" version))
       (sha256
        (base32
         "1fg7knfzybzija2b01pzrzhzsj989scl12sb2ra4f503l8279k54"))))
    (build-system python-build-system)
    (arguments
     (list #:modules '((srfi srfi-1)
                       (srfi srfi-26)
                       (guix build utils)
                       (guix build python-build-system))
           #:phases
           #~(modify-phases %standard-phases
               ;; Repurpose the candidates parameter to be the path to the
               ;; library, then on each backend we substitute the candidates
               ;; with the full path to the .so library or with None if not
               ;; supported.
               ;;
               ;; While most applications could use a single back-end this
               ;; library allows to manually select the back-end so it is
               ;; appropriate to provide as much back-ends as possible.
               (add-after 'unpack 'fix-libusb-reference
                 (lambda* (#:key inputs #:allow-other-keys)
                   (let ((libusb0 (find
                                    (negate symbolic-link?)
                                    (find-files (assoc-ref inputs "libusb-compat")
                                                "^libusb-.*\\.so\\..*")))
                         (libusb1 (find
                                    (negate symbolic-link?)
                                    (find-files (assoc-ref inputs "libusb")
                                                "^libusb-.*\\.so\\..*"))))
                     (substitute* "usb/libloader.py"
                       (("lib = locate_library\\(candidates, find_library\\)")
                        "lib = candidates"))
                     (substitute* "usb/backend/libusb0.py"
                       (("\\('usb-0\\.1', 'usb', 'libusb0'\\)")
                        (format #f "~s" libusb0)))
                     (substitute* "usb/backend/libusb1.py"
                       (("\\('usb-1\\.0', 'libusb-1\\.0', 'usb'\\)")
                        (format #f "~s" libusb1)))
                     ;; FIXME: OpenUSB is not packaged for GNU Guix.
                     (substitute* "usb/backend/openusb.py"
                       (("\\('openusb',\\)") "None")))))
               ;; Note: tests seems to succeed with libusb-compat as libusb
               ;; fails because it doesn't have a usbfs present in the build
               ;; environment.
               (replace 'check
                 (lambda* (#:key tests? #:allow-other-keys)
                   (when tests?
                     (with-directory-excursion "tests"
                       (setenv "PYUSB_DEBUG" "debug")
                       (setenv "LIBUSB_DEBUG" "4")
                       (invoke "python" "testall.py"))))))))
    (native-inputs
     (list python-setuptools-scm))
    (inputs
     (list libusb libusb-compat))
    (home-page "https://pyusb.github.io/pyusb/")
    (synopsis "Python bindings to the libusb library")
    (description
     "PyUSB aims to be an easy to use Python module to access USB devices.")
    (license license:bsd-3)))

(define-public python-capablerobot-usbhub
  (package
    (name "python-capablerobot-usbhub")
    (version "0.5.0")
    (source
     (origin
       ;; PyPI tarball fails to build.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/CapableRobot/CapableRobot_USBHub_Driver")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1nfd12612z9a9hby5dxg7lfqw5jcv3wcyqqagbg5izragni646mc"))))
    (build-system pyproject-build-system)
    (arguments
     `(#:tests? #f ; No tests provided.
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'use-poetry-core
           (lambda _
             ;; Patch to use the core poetry API.
             (substitute* "pyproject.toml"
               (("poetry.masonry.api")
                "poetry.core.masonry.api"))))
         (add-after 'install 'install-udev-rules
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (mkdir-p (string-append out "/lib/udev/rules.d"))
               (copy-file "50-capablerobot-usbhub.rules"
                          (string-append out
                                         "/lib/udev/rules.d/"
                                         "50-capablerobot-usbhub.rules"))))))))
    (native-inputs
     (list python-poetry-core))
    (propagated-inputs
     (list python-click-7 python-construct python-pyusb python-pyyaml-5))
    (home-page
     "https://github.com/CapableRobot/CapableRobot_USBHub_Driver")
    (synopsis
     "Host side driver for the Capable Robot Programmable USB Hub")
    (description
     "This package provides access to the internal state of the Capable Robot
USB Hub, allowing you to monitor and control the Hub from an upstream
computer.  It also creates a transparent CircuitPython Bridge, allowing
unmodified CircuitPython code to run on the host computer and interact with
I2C and SPI devices attached to the USB Hub.")
    (license license:expat)))

(define-public ideviceinstaller
  (package
    (name "ideviceinstaller")
    (version "1.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/libimobiledevice/ideviceinstaller")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1xp0sjgfx2z19x9mxihn18ybsmrnrcfc55zbh5a44g3vrmagmlzz"))))
    (build-system gnu-build-system)
    (native-inputs (list autoconf automake libtool pkg-config))
    (inputs (list libimobiledevice libzip))
    (home-page "https://libimobiledevice.org/")
    (synopsis "CLI Tool to manage apps and app archives on iOS devices")
    (description "This package provides an interface to manage IPA format
files and applications for iOS devices, it's written in C")
    (license license:gpl2)))

(define-public libirecovery
  (package
    (name "libirecovery")
    (version "1.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/libimobiledevice/libirecovery")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0p9ncqnz5kb7qisw00ynvasw1hax5qx241h9nwppi2g544i9lbnr"))))
    (build-system gnu-build-system)
    (inputs (list readline libusb))
    (native-inputs (list autoconf automake libtool pkg-config))
    (home-page "https://libimobiledevice.org/")
    (synopsis "Communication library with iBoot/iBSS of iOS devices via USB")
    (description "Libirecovery is a cross-platform library which implements
communication to iBoot/iBSS found on Apple's iOS devices via USB.")
    (license license:lgpl2.1)))

(define-public idevicerestore
  (package
    (name "idevicerestore")
    (version "1.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/libimobiledevice/idevicerestore")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1w7ywp77xc6v4hifi3j9ywrj447vv7fkwg2w26w0lq95f3bkblqr"))))
    (build-system gnu-build-system)
    (inputs (list libusb libirecovery libimobiledevice libzip curl zlib))
    (native-inputs (list autoconf automake libtool pkg-config))
    (home-page "https://libimobiledevice.org/")
    (synopsis "CLI tool to restore firmware files to iOS devices")
    (description "This utility is used to restore bricked or otherwise
broken iOS devices.")
    (license license:lgpl3+)))

(define-public libplist
  (package
    (name "libplist")
    (version "2.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/libimobiledevice"
                           "/libplist/releases/download/" version
                           "/libplist-" version ".tar.bz2"))
       (sha256
        (base32 "16mxdwaw01x9a3adf0yj3bqjc7afpf2vm1n5hkgj3i3y6zjifmaa"))))
    (build-system gnu-build-system)
    (arguments
     `(;; Tests fail randomly when run in parallel because several of them write
       ;; and read to/from the same file--e.g., "4.plist" is accessed by
       ;; 'large.test' and 'largecmp.test'.
       #:parallel-tests? #f))
    (inputs
     (list python))
    (native-inputs
     (list autoconf automake libtool pkg-config python-cython)) ; to build Python bindings
    (home-page "https://libimobiledevice.org/")
    (synopsis "C library to handle Apple Property List files")
    (description "This package provides a small portable C library to handle
Apple Property List files in binary or XML.")
    (license license:lgpl2.1+)))

(define-public libusbmuxd
  (package
    (name "libusbmuxd")
    (version "2.0.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/libimobiledevice"
                                  "/libusbmuxd/releases/download/" version
                                  "/libusbmuxd-" version ".tar.bz2"))
              (sha256
               (base32
                "084vg570g1qb506jd7axg6c080mfsmbf52v3lngzlknsaf2q0snc"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config libplist))
    (home-page "https://libimobiledevice.org/")
    (synopsis "Library to multiplex connections from and to iOS devices")
    (description "This package provides a client library to multiplex
connections from and to iOS devices by connecting to a socket provided by a
@code{usbmuxd} daemon.")
    (license license:lgpl2.1+)))

(define-public libimobiledevice
  (package
    (name "libimobiledevice")
    (version "1.3.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/libimobiledevice"
                                  "/libimobiledevice/releases/download/"
                                  version "/libimobiledevice-" version
                                  ".tar.bz2"))
              (sha256
               (base32
                "1xmhfnypg6j7shl73wfkrrn4mj9dh8qzaj3258q9zkb5cc669wjk"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("PYTHON_VERSION=3")))
    (propagated-inputs
     (list openssl libplist libusbmuxd))
    (inputs
     (list python))
    (native-inputs
     (list pkg-config python-cython))
    (home-page "https://libimobiledevice.org/")
    (synopsis "Protocol library and tools to communicate with Apple devices")
    (description "libimobiledevice is a software library that talks the
protocols to support Apple devices.  It allows other software to easily access
the device's file system, retrieve information about the device and its
internals, backup/restore the device, manage installed applications, retrieve
address books, calendars, notes, and bookmarks, and (using libgpod) synchronize
music and video to the device.")
    (license license:lgpl2.1+)))

(define-public ifuse
  (package
    (name "ifuse")
    (version "1.1.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/libimobiledevice"
                                  "/ifuse/releases/download/" version
                                  "/ifuse-" version ".tar.bz2"))
              (sha256
               (base32
                "11wdv44qwia77sh38n36809ysaib52rwd4fvqwb5ybsbz4p70l1m"))))
    (inputs
     (list fuse-2 libimobiledevice))
    (native-inputs
     (list pkg-config))
    (build-system gnu-build-system)
    (home-page "https://libimobiledevice.org/")
    (synopsis "Mount iOS devices")
    (description "This package provides @command{ifuse}, a command to mount
iOS devices and access their contents.")
    (license license:lgpl2.1+)))

(define-public usbmuxd
  (package
    (name "usbmuxd")
    (version "1.1.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/libimobiledevice"
                                  "/usbmuxd/releases/download/" version
                                  "/usbmuxd-" version ".tar.bz2"))
              (sha256
               (base32
                "17idzpxrvkbff0jpynf35df95lh7wsm8vndynp63bxib2w09gv60"))))
    (inputs
     (list libplist libusb libimobiledevice))
    (native-inputs
     (list pkg-config))
    (build-system gnu-build-system)
    (home-page "https://libimobiledevice.org/")
    (synopsis "Multiplex connections over USB to an iOS device")
    (description "This package provides the @code{usbmuxd} daemon
which multiplexes connections over USB to an iOS device.  To
users, it means you can sync your music, contacts, photos, etc.
over USB.")
    (license license:gpl2+)))

(define-public libmtp
  (package
    (name "libmtp")
    (version "1.1.21")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://sourceforge/libmtp/libmtp/" version
                                 "/libmtp-" version ".tar.gz"))
             (sha256
              (base32
               "19vj10la88lrhdfdcpkad7aiii01q59y5wj700dwjj4gijmsbzy4"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list libgcrypt))
    (propagated-inputs
     ;; libmtp.pc refers to all these.
     (list libusb))
    (arguments
     (list #:configure-flags
           #~(list "--disable-static"
                   (string-append "--with-udev=" #$output "/lib/udev"))))
    (home-page "https://libmtp.sourceforge.net/")
    (synopsis "Library implementing the Media Transfer Protocol")
    (description "Libmtp implements an MTP (Media Transfer Protocol)
initiator, which means that it initiates MTP sessions with devices.  The
devices responding are known as MTP responders.  Libmtp runs on devices
with a USB host controller interface.  It implements MTP Basic, which was
proposed for standardization.")
    ;; COPYING contains lgpl2.1, while files headers give
    ;; "GNU Lesser General Public License as published by the Free Software
    ;; Foundation; either version 2 of the License, or (at your option) any
    ;; later version."
    (license license:lgpl2.1+)))

(define-public gmtp
  (package
    (name "gmtp")
    (version "1.3.11")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/gmtp/gMTP-" version
                                  "/gmtp-" version ".tar.gz"))
              (sha256
               (base32
                "04q6byyq002fhzkc2rkkahwh5b6272xakaj4m3vwm8la8jf0r0ss"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     '(#:configure-flags
       (let ((libid3tag (assoc-ref %build-inputs "libid3tag")))
         (list
          "CFLAGS=-fcommon"
          ;; libid3tag provides no .pc file, so pkg-config fails to find them.
          (string-append "ID3TAG_CFLAGS=-I" libid3tag "/include")
          (string-append "ID3TAG_LIBS=-L" libid3tag "/lib -lid3tag -lz")))))
    (inputs
     (list gtk+ flac libvorbis libid3tag libmtp))
    (native-inputs
     (list pkg-config))
    (home-page "https://gmtp.sourceforge.net/")
    (synopsis "Simple graphical MTP client")
    (description "gMTP is a simple graphical client for the Media Transfer Protocol
  (MTP), which allows media files to be transferred to and from many portable
devices.")
    (license license:bsd-3)))

(define-public hidapi
  (package
    (name "hidapi")
    (version "0.14.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/libusb/hidapi")
             (commit (string-append "hidapi-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "121laqsml0104d1h4hp115gp21qiqi0r9dgcaqdi9ismmq3b6yx7"))))
    (build-system gnu-build-system)
    (inputs
     (list libusb eudev))
    (native-inputs
     (list autoconf automake libtool pkg-config))
    (home-page "https://github.com/libusb/hidapi")
    (synopsis "HID API library")
    (description
     "HIDAPI is a library which allows an application to interface with USB and Bluetooth
HID-Class devices.")
    ;; HIDAPI can be used under one of three licenses.
    (license (list license:gpl3
                   license:bsd-3
                   (license:non-copyleft "file://LICENSE-orig.txt")))))

(define-public python-hid
  (package
    (name "python-hid")
    (version "1.0.5")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "hid" version))
              (sha256
               (base32
                "1s5hvfbmnlmifswr5514f4xxn5rcd429bdcdqzgwkdxrg9zlx58y"))))
    (build-system pyproject-build-system)
    (arguments
     ;; No tests present on the source tree, without this compilation fails
     ;; because it "requires" the python-nose package, but it is not really
     ;; necessary.
     (list #:tests? #f
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'add-extra-library-paths
                 (lambda _
                   (let ((libhidapi-hidraw.so
                           #$(file-append hidapi "/lib/libhidapi-hidraw.so"))
                         (libhidapi-libusb.so
                           #$(file-append hidapi "/lib/libhidapi-libusb.so")))
                     (substitute* "hid/__init__.py"
                       (("library_paths = \\(.*$" all)
                        (string-append
                          all
                          "    '" libhidapi-hidraw.so "',\n"
                          "    '" libhidapi-libusb.so "',\n")))))))))
    (inputs (list hidapi))
    (home-page "https://github.com/apmorton/pyhidapi")
    (synopsis "Python @code{ctypes} bindings for HIDAPI library")
    (description "Python @code{ctypes} bindings for HIDAPI library.")
    (license license:expat)))

(define-public python-hidapi
  (package
    (name "python-hidapi")
    (version "0.7.99.post21")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "hidapi" version))
       (sha256
        (base32
         "15ws59zdrxahf3k7z5rcrwc4jgv1307anif8ixm2cyb9ask1mgp0"))
       (modules '((guix build utils)))
       (snippet
        ;; Remove bundled libraries.
        '(begin
           (delete-file-recursively "hidapi")
           #t))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-configuration
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "setup.py"
               (("'/usr/include/libusb-1.0'")
                (string-append "'" (assoc-ref inputs "libusb")
                               "/include/libusb-1.0'"))
               (("'/usr/include/hidapi'")
                (string-append "'" (assoc-ref inputs "hidapi")
                               "/include/hidapi'")))
             #t))
         ;; XXX Necessary because python-build-system drops the arguments.
         (replace 'build
           (lambda _
             (invoke "python" "setup.py" "build" "--with-system-hidapi")))
         (replace 'check
           (lambda _
             (invoke "python" "setup.py" "test" "--with-system-hidapi")))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (invoke "python" "setup.py" "install" "--with-system-hidapi"
                     (string-append "--prefix=" (assoc-ref outputs "out"))
                     "--single-version-externally-managed" "--root=/"))))))
    (inputs
     (list hidapi libusb eudev))
    (native-inputs
     (list python-cython))
    (home-page "https://github.com/trezor/cython-hidapi")
    (synopsis "Cython interface to hidapi")
    (description "This package provides a Cython interface to @code{hidapi}.")
    ;; The library can be used under either of these licenses.
    (license (list license:gpl3
                   license:bsd-3
                   (license:non-copyleft
                    "https://github.com/trezor/cython-hidapi/blob/master/LICENSE-orig.txt"
                    "You are free to use cython-hidapi code for any purpose.")))))
