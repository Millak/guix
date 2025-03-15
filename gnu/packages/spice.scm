;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 David Craven <david@craven.ch>
;;; Copyright © 2018–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2019, 2020, 2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2022, 2023 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2023 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (gnu packages spice)
  #:use-module (gnu packages)
  #:use-module (gnu packages acl)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cyrus-sasl)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages polkit)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages security-token)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages virtualization)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xml)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils))

(define-public usbredir
  (package
    (name "usbredir")
    (home-page "https://spice-space.org")
    (version "0.15.0")
    (source (origin
              (method url-fetch)
              (uri (string-append home-page "/download/" name "/" name "-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "0q890pzi0744isnw72lxk59aaqvhmg1dli8j35la123n4y0a7hkd"))))
    (build-system meson-build-system)
    (propagated-inputs (list libusb))
    (inputs (list glib))
    (native-inputs (list pkg-config))
    (synopsis "Tools for sending USB device traffic over a network")
    (description
     "Usbredir is a network protocol for sending USB device traffic over a
network connection.  It can be used to redirect traffic from a USB device to a
different (virtual) machine than the one to which the USB device is attached.")
    (license (list license:gpl2+ license:lgpl2.0+ license:lgpl2.1+))))

(define-public virglrenderer
  (package
    (name "virglrenderer")
    (version "1.0.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.freedesktop.org/virgl/virglrenderer.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0xw2qk8557gqpm1ssgk3ccshgljm6sh3wbbwpsp9cl0h4hdf2wq2"))))
    (build-system meson-build-system)
    (inputs (list libepoxy mesa))
    (native-inputs (list pkg-config python))
    (synopsis "Virtual 3D GPU library")
    (description "A virtual 3D GPU library that enables a virtualized operating
system to use the host GPU to accelerate 3D rendering.")
    (home-page "https://gitlab.freedesktop.org/virgl/virglrenderer")
    (license (list license:expat license:bsd-3))))

(define-public spice-protocol
  (package
    (name "spice-protocol")
    (version "0.14.4")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://www.spice-space.org/download/releases/"
                    "spice-protocol-" version ".tar.xz"))
              (sha256
               (base32
                "04nr2w6ymy5jinfi3lj6205yd5h0swss3ykxqk7l3m4z1mhvmzq4"))))
    (build-system meson-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'install-documentation
                 (lambda _
                   (install-file "COPYING"
                                 (string-append #$output "/share/doc/"
                                                #$name "-" #$version)))))))
    (synopsis "Protocol headers for the SPICE protocol")
    (description "SPICE (the Simple Protocol for Independent Computing
Environments) is a remote-display system built for virtual environments
which allows users to view a desktop computing environment.")
    (home-page "https://www.spice-space.org")
    (license (list license:bsd-3 license:lgpl2.1+))))

(define-public spice-gtk
  (package
    (name "spice-gtk")
    (version "0.42")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://spice-space.org/download/gtk/"
                    "spice-gtk-" version ".tar.xz"))
              (sha256
               (base32
                "0n3s1rn7yzs28hnl9k6ql3a90qlv8w16djqj32m1zb8i31zi304k"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'disable-problematic-tests
            (lambda _
              ;; XXX: Disable the session and cd-emu tests, because they
              ;; require USB support, which is not available in the build
              ;; container.
              (substitute* "tests/meson.build"
                ((".*'session.c',.*") "")
                (("tests_sources \\+= 'cd-emu.c'" all)
                 (string-append "# " all)))))
          (add-after 'unpack 'adjust-default-acl-helper-path
            (lambda _
              ;; The USB ACL helper used to allow USB redirection as a
              ;; non-privileged user needs to be setuid, as configured by the
              ;; gnome-desktop-service-type.  A user can still change the
              ;; location by specifying the SPICE_USB_ACL_BINARY environment
              ;; variable.
              (substitute* "src/usb-acl-helper.c"
                (("ACL_HELPER_PATH\"/spice-client-glib-usb-acl-helper\"")
                 "\"/run/privileged/bin/spice-client-glib-usb-acl-helper\""))))
          (add-before 'configure 'correct-polkit-dir
            (lambda _
              (substitute* "meson.build"
                (("d.get_variable\\(pkgconfig: 'policydir')")
                 (string-append "'" #$output "/share/polkit-1/actions'")))))
          (add-before 'install 'fake-pkexec
            (lambda _ (setenv "PKEXEC_UID" "-1")))
          (add-after 'install 'wrap-spicy
            (lambda* (#:key outputs #:allow-other-keys)
              (wrap-program (search-input-file outputs "bin/spicy")
                `("GST_PLUGIN_SYSTEM_PATH" ":"
                  prefix (,(getenv "GST_PLUGIN_SYSTEM_PATH")))))))))
    (native-inputs
     (list `(,glib "bin")
           intltool
           pkg-config
           python
           python-pyparsing
           python-six
           vala))
    (inputs
     (list bash-minimal
           glib-networking
           gobject-introspection
           json-glib
           acl
           libcap-ng
           libepoxy
           libxcb
           mesa
           polkit
           pulseaudio
           usbutils))
    (propagated-inputs
     (list gstreamer
           gst-plugins-base
           gst-plugins-good
           spice-protocol
           ;; These are required by the pkg-config files (needed for example
           ;; when building GNOME Boxes).
           cyrus-sasl
           gtk+
           openssl-1.1
           opus
           libcacard
           libjpeg-turbo
           lz4
           phodav
           pixman
           usbredir))
    (synopsis "Gtk client and libraries for SPICE remote desktop servers")
    (description "Gtk client and libraries for SPICE remote desktop servers.")
    (home-page "https://www.spice-space.org")
    (license (list license:lgpl2.1+ license:lgpl2.0+))))

(define-public spice
  (package
    (name "spice")
    (version "0.15.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                "https://www.spice-space.org/download/releases/"
                "spice-server/spice-" version ".tar.bz2"))
              (sha256
               (base32
                "10av05vk60jzqjhqbsw5zdwqmx6gpr50045wqiqp9483gw8vd7kd"))))
    (build-system gnu-build-system)
    (propagated-inputs
      (list openssl-1.1 pixman spice-protocol))
    (inputs
      (list cyrus-sasl
            glib
            libjpeg-turbo
            libcacard ; smartcard support
            lz4
            opus
            orc
            zlib))
    (native-inputs
      (list pkg-config
            python
            spice-gtk
            ;; These are needed for the server listen tests.
            glib-networking
            gsettings-desktop-schemas))
    (arguments
      `(#:configure-flags
        '("--enable-lz4"
          "--enable-automated-tests")

        #:phases
        (modify-phases %standard-phases
          ;; XXX: Otherwise the server listen tests fails with
          ;;   Failed to create /homeless-shelter/.config/glib-2.0/settings
          (add-before 'check 'set-XDG_CONFIG_HOME
            (lambda _
              (setenv "XDG_CONFIG_HOME" "/tmp"))))

        ;; Several tests appear to be opening the same sockets concurrently.
        #:parallel-tests? #f))
    (synopsis "Server implementation of the SPICE protocol")
    (description "SPICE is a remote display system built for virtual
environments which allows you to view a computing @code{desktop} environment
not only on the machine where it is running, but from anywhere on the
Internet and from a wide variety of machine architectures.")
    (home-page "https://www.spice-space.org")
    (license (list license:lgpl2.1+ license:lgpl2.0+))))

(define-public spice-vdagent
  (package
    (name "spice-vdagent")
    (version "0.22.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                "http://www.spice-space.org/download/releases/"
                "spice-vdagent-" version ".tar.bz2"))
              (sha256
               (base32
                "18472sqr0gibzgzi48dpcbnvm78l05qrl5wv6xywqqj7r9dd3c4k"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       '("--localstatedir=/var")
       ;; The test-session-info test fails for unknown reasons (see:
       ;; https://gitlab.freedesktop.org/spice/linux/vd_agent/-/issues/24).
       #:make-flags '("XFAIL_TESTS=tests/test-session-info")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-makefile.in
           (lambda _
             (substitute* "Makefile.in"
               (((string-append "\\$\\(mkdir_p\\) \\$\\(DESTDIR\\)"
                                "\\$\\(localstatedir\\)/run/spice-vdagentd"))
                 "-$(mkdir_p) $(DESTDIR)$(localstatedir)/run/spice-vdagentd"))))
         (add-after 'unpack 'patch-spice-vdagent.desktop
           (lambda* (#:key outputs #:allow-other-keys)
            (substitute* "data/spice-vdagent.desktop"
              (("Exec=/usr/bin/spice-vdagent")
               (string-append "Exec=" (assoc-ref outputs "out")
                              "/bin/spice-vdagent")))))
         (add-after 'unpack 'fix-test-termination
           (lambda _
             ;; The termination tests depend on finding the socket file name
             ;; in the spice-vdagent command line it launched, but by default
             ;; ps truncates its output, which causes the test to fail (see:
             ;; https://gitlab.freedesktop.org/spice/linux/vd_agent/-/merge_requests/36).
             (substitute* "tests/test-termination.c"
               (("ps -ef")
                "ps -efww")))))))
    (inputs
      (list alsa-lib
            dbus
            glib
            gtk+
            libdrm
            libpciaccess
            libx11
            libxext
            libxfixes
            libxinerama
            libxrandr
            spice-protocol))
    (native-inputs
     (list pkg-config procps))             ;tests use 'ps'
    (synopsis "Spice agent for Linux")
    (description "Spice-vdagent enables sharing the clipboard and guest display
resolution scaling on graphical console window resize.")
    (home-page "https://www.spice-space.org")
    (license license:gpl3+)))

(define-public libcacard
  (package
    (name "libcacard")
    (version "2.8.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://gitlab.freedesktop.org/spice/libcacard/uploads/"
                    "13b249e695a0d9aa7cb501b1a85ebab1"
                    "/libcacard-" version ".tar.xz"))
              (sha256
               (base32
                "1rrjlclm6ad63gah1fa4yfwrz4z6vgq2yrybbvzvvdbxrgl4vgzv"))))
    (build-system meson-build-system)
    (arguments
     (list #:configure-flags
           ;; Otherwise, the validate-runpath phase fails.
           #~(list (string-append "-Dc_link_args=-Wl,-rpath="
                                  (search-input-directory %build-inputs
                                                          "lib/nss")))))
    (propagated-inputs
     ;; The following inputs are required in the pkg-config file.
     (list glib
           nss
           pcsc-lite))
    (native-inputs
     (list openssl
           `(,nss "bin")
           opensc
           gnutls
           pkg-config
           which))
    (synopsis "Emulate and share smart cards with virtual machines")
    (description
     "The @acronym{CAC,Common Access Card} library can be used to emulate and
share smart cards from client system to local or remote virtual machines.")
    (home-page "https://gitlab.freedesktop.org/spice/libcacard")
    (license license:lgpl2.1+)))

(define-public virt-viewer
  (package
    (name "virt-viewer")
    ;; XXX Remove the 'build-with-recent-meson phase when updating.
    (version "11.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://virt-manager.org/download/sources/virt-viewer/"
             "virt-viewer-" version ".tar.xz"))
       (sha256
        (base32 "1l5bv6x6j21l487mk3n93ai121gg62n6b069r2jpf72cbhra4gx4"))))
    (build-system meson-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'build-with-recent-meson
                 ;; Fix ‘ERROR: Function does not take positional arguments.’
                 (lambda _
                   (substitute* "data/meson.build"
                     (("i18n\\.merge_file \\(.*" match)
                      (string-append match "#"))))))))
    (native-inputs
     (list `(,glib "bin")
           gettext-minimal
           perl                         ;for pod2man
           pkg-config
           python))
    (inputs
     (list bash-completion
           gtk+
           gtk-vnc
           libcap
           libgovirt
           libvirt-glib
           libxml2
           spice-gtk
           vte/gtk+-3))
    (synopsis "Graphical console client for virtual machines")
    (description "Graphical console client for virtual machines using SPICE or
VNC.")
    (home-page "https://virt-manager.org")
    (license license:gpl2+)))
