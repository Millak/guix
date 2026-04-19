;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025 Tomas Volf <~@wolfsden.cz>
;;; Copyright © 2023 Raven Hallsby <karl@hallsby.com>
;;; Copyright © 2026 Giacomo Leidi <therewasa@fishinthecalculator.me>
;;; Copyright © 2026 Nguyễn Gia Phong <cnx@loang.net>
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

;;;; Commentary:

;;; Power-related packages.

;;;; Code:

(define-module (gnu packages power)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages check)
  #:use-module (gnu packages disk)
  #:use-module (gnu packages file-systems)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages instrumentation)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages man)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages ruby-xyz)
  #:use-module (gnu packages virtualization)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system pyproject)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))

(define-public apcupsd
  (package
    (name "apcupsd")
    (version "3.14.14")
    (source (origin
              (method url-fetch)
              (uri
               (string-append
                "mirror://sourceforge/" name "/" name " - Stable/" version
                "/" name "-" version ".tar.gz"))
              (sha256
               (base32
                "0rwqiyzlg9p0szf3x6q1ppvrw6f6dbpn2rc5z623fk3bkdalhxyv"))))
    (outputs '("out" "doc"))
    (build-system gnu-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list
         ;; The configure script ignores --prefix for most of the file names.
         (string-append "--exec-prefix=" #$output)
         (string-append "--mandir=" #$output "/share/man")
         (string-append "--sbindir=" #$output "/sbin")
         (string-append "--sysconfdir=" #$output "/etc/apcupsd")
         (string-append "--with-halpolicydir=" #$output "/share/halpolicy")

         ;; Put us into the version string.
         "--with-distname=GNU Guix"
         "--disable-install-distdir"

         ;; State directories.
         "--localstatedir=/var"
         "--with-log-dir=/var/log"
         "--with-pid-dir=/run"
         "--with-lock-dir=/run/apcupsd/lock"
         "--with-nologin=/run/apcupsd"
         "--with-pwrfail-dir=/run/apcupsd"

         ;; Configure requires these, but we do not use the generated
         ;; apcupsd.conf, so in order to reduce dependencies of the package,
         ;; provide fake values.
         (string-append "ac_cv_path_SHUTDOWN=/nope")
         (string-append "ac_cv_path_APCUPSD_MAIL=/nope")
         ;; While `wall' is not expanded anywhere, it still is searched for.
         ;; See https://sourceforge.net/p/apcupsd/mailman/message/59128628/ .
         (string-append "ac_cv_path_WALL=/nope")

         ;; Enable additional drivers.
         "--enable-usb"
         "--enable-modbus-usb")
      #:tests? #f                       ; There are no tests.
      #:modules (cons '(ice-9 ftw) %default-gnu-modules)
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'configure 'remove-time-from-manual
            (lambda _
              ;; Do not bake the date and time of the build into the manual.
              (substitute* "doc/manual/manual.rst"
                (("\\| \\|date\\| \\|time\\|") ""))
              (substitute* "autoconf/variables.mak.in"
                (("^(RST2HTMLOPTS = .*) --time (.*)" all pref suff)
                 (string-append pref " " suff)))))
          (add-after 'build 'build-manual
            (lambda _
              (invoke "make" "-C" "doc/manual" "manual.html")))
          (add-after 'install-license-files 'move-doc
            (lambda _
              (let ((target (string-append #$output:doc
                                           "/share/doc/"
                                           (strip-store-file-name #$output))))
                (mkdir-p target)
                (for-each (lambda (f)
                            (copy-file (string-append "doc/manual/" f)
                                       (string-append target "/" f)))
                          (scandir "doc/manual"
                                   (lambda (f)
                                     (or (string-suffix? ".png" f)
                                         (string-suffix? ".html" f))))))))
          ;; If sending mails is required, use proper mail program.
          (add-after 'install 'remove-smtp
            (lambda _
              (delete-file (string-append #$output "/sbin/smtp"))))
          ;; The configuration files and scripts are not really suitable for
          ;; Guix, and our service provides its own version anyway.  So delete
          ;; these to make sure `apcupsd' and `apctest' executed without any
          ;; arguments fail.  `apctest' actually segfaults, but only after
          ;; printing an error.
          (add-after 'install 'remove-etc-apcupsd
            (lambda _
              (delete-file-recursively
               (string-append #$output "/etc/apcupsd")))))))
    (native-inputs (list mandoc pkg-config python-docutils-0.19 util-linux))
    (inputs (list libusb libusb-compat))
    (home-page "http://www.apcupsd.org")
    (synopsis "Daemon for controlling APC UPSes")
    (description "@command{apcupsd} can be used for power management and
controlling most of @acronym{APC, American Power Conversion}’s @acronym{UPS,
Uninterruptible Power Supply} models.  @command{apcupsd} works with most of
APC’s Smart-UPS models as well as most simple signalling models such a
Back-UPS, and BackUPS-Office.")
    (license license:gpl2)))

(define-public tuned-minimal
  (package
    (name "tuned-minimal")
    (version "2.27.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/redhat-performance/tuned")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "11h65i65y0ly3iw8kkhzq6mv8csw9jkgy57k51jf3g99w57pcl9y"))))
    (build-system pyproject-build-system)
    (arguments
       (list
        ;; tests: 105 passed, 4 deselected
        #:test-flags #~'("-k" "not udev and not get_device")
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'patch-paths
              (lambda* (#:key inputs outputs #:allow-other-keys)
                (substitute* "Makefile"
                  (("^PYTHON_SITELIB = .*")
                   (simple-format #f "PYTHON_SITELIB = ~a\n"
                     (site-packages inputs outputs)))
                  ;; These directories do not make sense on Guix.
                  (("mkdir -p \\$\\(DESTDIR\\)/var/lib/tuned") "")
                  (("mkdir -p \\$\\(DESTDIR\\)/var/log/tuned") "")
                  (("mkdir -p \\$\\(DESTDIR\\)/run/tuned") "")
                  ;; Drop systemD services.
                  ((".*\\$\\(DESTDIR\\)\\$\\(UNITDIR\\).*\\.service.*") "")
                  ;; tuned-gui depends on systemctl being available.  Drop it
                  ;; until it'll be compatible with other init systems.
                  ((".*\\$\\(DESTDIR\\).*tuned-gui.*") ""))
                ;; Substitute FHS paths with Guix ones.
                (substitute* (cons*
                              "experiments/powertop2tuned.py"
                              "tuned-adm.bash"
                              "tuned/consts.py"
                              (find-files "profiles"
                                          (lambda (name stat)
                                            (and (string-suffix? ".sh" name)
                                                 (eq? 'regular (stat:type stat))
                                                 (access? name X_OK)))))
                  (("/etc/tuned")
                   (string-append #$output "/etc/tuned"))
                  (("/usr/lib/tuned")
                   (string-append #$output "/lib/tuned")))
                (substitute* "experiments/powertop2tuned.py"
                  (("/usr/sbin/powertop")
                   (search-input-file inputs "sbin/powertop")))
                ;; TuneD really wants to write to
                ;; /etc/modprobe.d/tuned.conf . Guix manages this file
                ;; declaratively, so the default location for TuneD is
                ;; is changed to a file where it can actually write.
                (substitute* "tuned/consts.py"
                  (("/etc/modprobe\\.d/tuned\\.conf")
                   "/etc/tuned/modprobe.d/tuned.conf"))))
            ;; There is nothing to build except documentation.
            ;; https://github.com/redhat-performance/tuned/blob/v2.27.0/INSTALL#L4
            ;; https://github.com/redhat-performance/tuned/blob/v2.27.0/tuned.spec
            (replace 'build
              (lambda* (#:key inputs #:allow-other-keys)
                (invoke "make" "html"
                        (string-append
                         "PYTHON=" (search-input-file inputs "bin/python3")))))
            (replace 'install
              (lambda _
                (invoke "make" "install" "install-ppd" "install-html"
                        (string-append "PREFIX=" #$output)
                        (string-append "SYSCONFDIR=" #$output "/etc")
                        (string-append "TMPFILESDIR=" #$output "/run/tuned"))))
            (add-before 'check 'set-check-pythonpath
              (lambda _
                (setenv "PYTHONPATH" "tests/unit")))
            (add-before 'wrap 'wrap-gi-and-path
              (lambda* (#:key inputs #:allow-other-keys)
                (for-each
                 (lambda (binary)
                   (wrap-program binary
                     `("GI_TYPELIB_PATH" =
                       (,(string-append #$(this-package-input "glib")
                                        "/lib/girepository-1.0")))))
                 (find-files (string-append #$output "/sbin"))))))))
    (native-inputs (list python-pytest
                         ruby-asciidoctor))
    (inputs
     (list bash-minimal
           ethtool
           gawk
           glib
           hdparm
           kmod
           iproute
           powertop
           python-dbus
           python-pyinotify
           python-linux-procfs
           python-pygobject
           python-pyudev
           systemtap
           util-linux
           virt-what))
    (synopsis "Dynamic adaptive system tuning daemon")
    (description
     "The TuneD package contains a daemon that tunes system settings
dynamically.  It does so by monitoring the usage of several system components
periodically.  Based on that information components will then be put into lower
or higher power saving modes to adapt to the current usage.")
    (home-page "https://tuned-project.org")
    (license (list license:gpl2+ license:cc-by-sa3.0))))

(define-public tuned
  (package
    (inherit tuned-minimal)
    (name "tuned")
    (inputs
     (modify-inputs inputs
       (prepend dmidecode perf wireless-tools)))))
