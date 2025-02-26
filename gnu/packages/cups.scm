;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2019, 2021 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015, 2016, 2017, 2019, 2021, 2023 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015-2018, 2023 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Danny Milosavljevic <dannym@scratchpost.org>
;;; Copyright © 2017 Leo Famulari <leo@famulari.name>
;;; Copyright © 2017 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2017–2022 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2021, 2023, 2024 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2023 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2025 Lukas Gradl <lgradl@posteo.net>
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

(define-module (gnu packages cups)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages avahi)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages fonts)     ; font-dejavu
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages groff)
  #:use-module (gnu packages image)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages photo)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages polkit)
  #:use-module (gnu packages pretty-print)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages scanner)
  #:use-module (gnu packages tls)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system python)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match))

(define-public brlaser
  (let ((commit "2a49e3287c70c254e7e3ac9dabe9d6a07218c3fa")
        (revision "2"))
    (package
      (name "brlaser")
      (version (git-version "6" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/pdewacht/brlaser")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "033g461qzwrzi6x24pfasyx9g7fkn5iy5f8c3h8bczg2bvscxyym"))))
      (build-system cmake-build-system)
      (arguments
       (list
        #:configure-flags
        #~(list (string-append "-DCUPS_DATA_DIR=" #$output "/share/cups")
                (string-append "-DCUPS_SERVER_BIN=" #$output "/lib/cups"))))
      (inputs
       (list ghostscript cups zlib))
      (home-page "https://github.com/pdewacht/brlaser")
      (synopsis "Brother laser printer driver")
      (description "Brlaser is a CUPS driver for Brother laser printers.  This
driver is known to work with these printers:

@enumerate
@item Brother DCP-1510 series
@item Brother DCP-1600 series
@item Brother DCP-7030
@item Brother DCP-7040
@item Brother DCP-7055
@item Brother DCP-7055W
@item Brother DCP-7060D
@item Brother DCP-7065DN
@item Brother DCP-7080
@item Brother DCP-L2500D series
@item Brother DCP-L2520D series
@item Brother DCP-L2540DW series
@item Brother HL-1110 series
@item Brother HL-1200 series
@item Brother HL-2030 series
@item Brother HL-2140 series
@item Brother HL-2220 series
@item Brother HL-2270DW series
@item Brother HL-5030 series
@item Brother HL-L2300D series
@item Brother HL-L2320D series
@item Brother HL-L2340D series
@item Brother HL-L2360D series
@item Brother MFC-1910W
@item Brother MFC-7240
@item Brother MFC-7360N
@item Brother MFC-7365DN
@item Brother MFC-7840W
@item Brother MFC-L2710DW series
@item Lenovo M7605D
@end enumerate")
      (license license:gpl2+))))

(define-public cups-filters
  (package
    (name "cups-filters")
    (version "1.28.16")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://openprinting.org/download/cups-filters/"
                       "cups-filters-" version ".tar.xz"))
       (sha256
        (base32 "1h4s35xkbxhxpid39yaiy0gmaj3lck8lhzfdfl3h58hxfpx0nh1s"))
       (modules '((guix build utils)))
       (snippet
        ;; Install backends, banners and filters to cups-filters output
        ;; directory, not the cups server directory.
        #~(begin
            (substitute* "Makefile.in"
              (("CUPS_DATADIR = @CUPS_DATADIR@")
               "CUPS_DATADIR = $(PREFIX)/share/cups")
              (("pkgcupsserverrootdir = \\$\\(CUPS_SERVERROOT\\)")
               "pkgcupsserverrootdir = $(PREFIX)")
              ;; Choose standard directories notably so that binaries are
              ;; stripped.
              (("^pkg(.*)dir = \\$\\(CUPS_SERVERBIN\\)/(.*)" _ type suffix)
               (format #f "pkg~adir = $(PREFIX)/lib/cups/~a" type suffix)))
            ;; Find bannertopdf data such as the print test page in our
            ;; output directory, not CUPS's prefix.
            (substitute* "configure"
              (("\\{CUPS_DATADIR\\}/data")
               "{prefix}/share/cups/data"))))))
    (build-system gnu-build-system)
    (arguments
     (list #:make-flags
           #~(list (string-append "PREFIX=" #$output))
           #:configure-flags
           #~(list "--disable-mutool"  ; needs yet another PDF library (mupdf)

                   ;; Look for the "domain socket of CUPS" in /var/run/cups.
                   "--localstatedir=/var"

                   ;; Free software for the win.
                   "--with-acroread-path=evince"

                   (string-append "--with-test-font-path="
                                  #$(this-package-input "font-dejavu")
                                  "/share/fonts/truetype/DejaVuSans.ttf")
                   (string-append "--with-gs-path="
                                  #$(this-package-input "ghostscript-with-cups")
                                  "/bin/gsc")
                   (string-append "--with-shell="
                                  (assoc-ref %build-inputs "bash")
                                  "/bin/bash")
                   (string-append "--with-rcdir="
                                  #$output "/etc/rc.d")
                   ;; QPDF headers include C++17 libraries such as
                   ;; std::string_view.
                   "CFLAGS=-std=gnu17"
                   "CXXFLAGS=-std=gnu++17")
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'patch-foomatic-hardcoded-file-names
                 (lambda _
                   ;; Foomatic has hard-coded file names we need to fix.
                   (substitute* "filter/foomatic-rip/foomaticrip.c"
                     (("/usr/local(/lib/cups/filter)" _ file)
                      (string-append #$output file)))))
               (add-after 'install 'wrap-filters
                 (lambda _
                   ;; Some filters expect to find things in $PATH.  We cannot
                   ;; just hard-code all absolute file names in the source
                   ;; because foomatic-rip, for example, has tests like
                   ;; 'startswith(cmd, "gs")'.
                   (for-each
                    (lambda (file)
                      (wrap-program file
                        `("PATH" ":" prefix
                          (,(string-append
                             #$(this-package-input "coreutils") "/bin:"
                             #$(this-package-input "ghostscript-with-cups")
                             "/bin:"
                             #$(this-package-input "grep") "/bin:"
                             #$(this-package-input "sed") "/bin")))))
                    (find-files (string-append #$output
                                               "/lib/cups/filter"))))))))
    (native-inputs
     (append (if (%current-target-system)
                 (list cups-minimal)
                 '())
             (list `(,glib "bin")               ; for gdbus-codegen
                   pkg-config)))
    (inputs
     (list avahi
           bash-minimal
           coreutils
           cups-minimal
           dbus
           font-dejavu                  ;also needed by test suite
           fontconfig
           freetype
           ghostscript/cups
           glib
           grep
           ijs
           lcms
           libexif
           libjpeg-turbo
           libpng
           libtiff
           poppler
           qpdf
           sed))
    (home-page "https://wiki.linuxfoundation.org/openprinting/cups-filters")
    (synopsis "OpenPrinting CUPS filters and backends")
    (description
     "Contains backends, filters, and other software that was once part of the
core CUPS distribution but is no longer maintained by Apple Inc.  In addition
it contains additional filters developed independently of Apple, especially
filters for the PDF-centric printing workflow introduced by OpenPrinting.")
    ;; Different filters and backends have different licenses; see COPYING for
    ;; details
    (license (list license:gpl2
                   license:gpl2+
                   license:gpl3
                   license:gpl3+
                   license:lgpl2.0+
                   license:expat))))

;; CUPS on non-MacOS systems requires cups-filters.  Since cups-filters also
;; depends on CUPS libraries and binaries, cups-minimal has been added to
;; satisfy this dependency.
(define-public cups-minimal
  (package
    (name "cups-minimal")
    (version "2.4.9")
    (replacement cups-minimal/fixed)
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/OpenPrinting/cups")
             (commit (string-append "v" version))))
       ;; Avoid NAME confusion: these are the complete CUPS sources.
       (file-name (git-file-name "cups" version))
       (sha256
        (base32 "08wjd1flyaslhnwvxl39403qi3g675rk532ysiyk6cda4r8ks1g1"))))
    (build-system gnu-build-system)
    (arguments
     (list #:configure-flags
           ;; This package isn't maximally minimal: "--with-components=libcups"
           ;; breaks cups-filters.  Disable some other unnecessary features.
           #~(list "--without-icondir"
                   "--without-languages"
                   "--without-menudir")
           ;; Seven tests fail, mostly because of files that are provided by the
           ;; cups-filters package.
           #:tests? #f
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'update-config-scripts
                 (lambda* (#:key native-inputs inputs #:allow-other-keys)
                   (for-each (lambda (file)
                               (install-file
                                (search-input-file
                                 (or native-inputs inputs)
                                 (string-append "/bin/" file)) "."))
                             '("config.guess" "config.sub"))))
               (add-after 'unpack 'never-cupsAdminGetServerSettings
                 ;; Rather than just ask the daemon, this part of CUPS assumes
                 ;; that (1) it has access to a cupsd.conf under CUPS_SERVERROOT
                 ;; and (2) the file's contents apply to the running daemon.
                 ;; (1) is false at least on Guix Systems resulting in extremely
                 ;; long delays when loading the Web interface's /admin page.
                 ;; (2) is never valid: it ignores, e.g., -c FILE.  Upstream
                 ;; considers this code on ‘life support’ so just neuter it.
	         (lambda _
	           (substitute* "cgi-bin/admin.c"
	             (("!cupsAdminGetServerSettings" match)
		      (string-append "0 && " match)))))
               (add-after 'unpack 'remove-Web-UI-server-settings
                 ;; The /admin page's server configuration form is questionable
                 ;; for the same reason as cupsAdminGetServerSettings, and won't
                 ;; work at all on Guix Systems.  Remove it entirely.
                 (lambda _
                   ;; SUBSTITUTE* & patches both have (dis)advantages.  This is
                   ;; shorter & should ensure that no translation is forgotten.
                   (substitute* (find-files "templates" "^admin\\.tmpl$")
                     ((" class=\"halves\"") "")
                     (("<FORM.* ACTION=\"/jobs.*</FORM>" match)
                      (string-append match "</P>{BROKEN? "))
                     (("</FORM>}" match)
                      (string-append match "}")))))
               (add-before 'configure 'patch-makedefs
                 (lambda _
                   (substitute* "Makedefs.in"
                     (("INITDIR.*=.*@INITDIR@") "INITDIR = @prefix@/@INITDIR@")
                     (("/bin/sh") (which "sh")))))
               (add-before 'check 'skip-failing-tests
                 (lambda _
                   (substitute* "test/run-stp-tests.sh"
                     ;; The number of error/warning lines differs, probably due
                     ;; to a missing font.  Substitute the last observed count.
                     (("(\\$count != )33" _ prefix)
                      (string-append prefix "39"))))))))
    (native-inputs (cons* config pkg-config
                          (if (%current-target-system)
                              (list this-package)
                              '())))
    (inputs (list zlib gnutls libxcrypt))
    (home-page "https://openprinting.github.io/cups")
    (synopsis "The Common Unix Printing System")
    (description
     "CUPS is a printing system that uses @acronym{IPP, the Internet Printing
Protocol} to talk to printers and network clients.  It also provides the old
@command{lp} and @command{lpr} commands, a Web interface, and a C programming
interface to manage printers and print jobs.

CUPS can print to both local (USB, serial, even parallel) and networked
printers.  Almost any modern printer supports IPP@tie{}Everywhere, sometimes
sold as AirPrint, and is supported out of the box.  Older printers can be
supported through legacy PPD-based printer drivers called ``printer
applications''.  These must be installed separately.")
    ;; CUPS is Apache 2.0 with exceptions, see the NOTICE file.
    (license license:asl2.0)))

(define cups-minimal/fixed
  (package
    (inherit cups-minimal)
    (source
     (origin
       (inherit (package-source cups-minimal))
       (patches
        (search-patches "cups-minimal-Address-PPD-injection-issues.patch"))))))

(define-public cups
  (package/inherit cups-minimal
    (name "cups")
    (arguments
     (substitute-keyword-arguments
       (strip-keyword-arguments
         '(#:tests?)
         (package-arguments cups-minimal))
       ((#:configure-flags flags #~'())
        #~(append #$flags
                  (list "--with-languages=all"))) ; no ‘=all’ means none(!)
       ((#:phases phases #~%standard-phases)
        #~(modify-phases #$phases
            (add-before 'check 'patch-tests
              (lambda* (#:key outputs #:allow-other-keys)
                (let ((filters #$(this-package-input "cups-filters"))
                      (catpath (string-append
                                #$(this-package-input "coreutils") "/bin/"))
                      (testdir (string-append (getcwd) "/tmp/")))
                  (mkdir testdir)
                  (substitute* "test/run-stp-tests.sh"
                    ((" *BASE=/tmp/") (string-append "BASE=" testdir))

                    ;; Allow installation of filters from the output directory
                    ;; and from cups-filters.
                    (("for dir in /usr/libexec/cups/filter /usr/lib/cups/filter")
                     (string-append
                      "for dir in "
                      (assoc-ref outputs "out") "/lib/cups/filter "
                      filters "/lib/cups/filter"))

                    ;; Check for charsets in the default cups-filters output.
                    (("/usr/share/cups/charsets")
                     (string-append filters "/share/cups/charsets"))

                    ;; Install additional required filters.
                    (("instfilter texttopdf texttopdf pdf")
                     (string-append
                      "instfilter texttopdf texttopdf pdf;"
                      "instfilter imagetoraster imagetoraster raster;"
                      "instfilter gstoraster gstoraster raster;"
                      "instfilter urftopdf urftopdf pdf;"
                      "instfilter rastertopdf rastertopdf pdf;"
                      "instfilter pstopdf pstopdf pdf"))

                    ;; Specify the location of the lpstat binary.
                    (("description=\"`lpstat -l")
                     "description=\"`../systemv/lpstat -l")

                    ;; Patch the shebangs of embedded scripts.
                    (("#!/bin/sh") (string-append "#!" (which "sh")))

                    ;; Also link MIME definitions from cups-filters
                    ;; to enable the additional filters for the test suite.
                    (("ln -s \\$root/conf/mime\\.types")
                     (string-append
                      "ln -s " filters
                      "/share/cups/mime/cupsfilters.types $BASE/share/mime; "
                      "ln -s $root/conf/mime.types"))
                    (("ln -s \\$root/conf/mime\\.convs")
                     (string-append
                      "ln -s " filters
                      "/share/cups/mime/cupsfilters.convs $BASE/share/mime; "
                      "ln -s $root/conf/mime.convs")))

                  ;; Fix the search path for the "cat" command.
                  (substitute* "cups/testfile.c"
                    (("cupsFileFind\\(\"cat\", \"/bin\"")
                     (string-append "cupsFileFind(\"cat\", \"" catpath "\""))
                    (("cupsFileFind\\(\"cat\", \"/bin:/usr/bin\"")
                     (string-append "cupsFileFind(\"cat\", \"" catpath "\""))))))
            (add-after 'install 'install-cups-filters-symlinks
              (lambda* (#:key inputs outputs #:allow-other-keys)
                (let ((out (assoc-ref outputs "out"))
                      (cups-filters #$(this-package-input "cups-filters")))
                  ;; Charsets.
                  (symlink
                   (string-append cups-filters "/share/cups/charsets")
                   (string-append out "/share/charsets"))

                  ;; MIME types, driver files, and PPDs.
                  (for-each
                   (lambda (f)
                     (symlink (string-append cups-filters f)
                              (string-append out f)))
                   '("/share/cups/mime/cupsfilters.types"
                     "/share/cups/mime/cupsfilters.convs"
                     "/share/cups/drv/cupsfilters.drv"
                     "/share/ppd"))

                  ;; Filters.
                  (for-each
                   (lambda (f)
                     (symlink f
                              (string-append out "/lib/cups/filter"
                                             (basename f))))
                   (find-files (string-append cups-filters "/lib/cups/filter")))

                  ;; Backends.
                  (for-each
                   (lambda (f)
                     (symlink (string-append cups-filters f)
                              (string-append out "/lib/cups/backend/"
                                             (basename f))))
                   '("/lib/cups/backend/parallel"
                     "/lib/cups/backend/serial"))

                  ;; Banners.
                  (let ((banners "/share/cups/banners"))
                    (delete-file-recursively (string-append out banners))
                    (symlink (string-append cups-filters banners)
                             (string-append out banners)))

                  ;; Assorted data.
                  (let ((data "/share/cups/data"))
                    (delete-file-recursively (string-append out data))
                    (symlink (string-append cups-filters data)
                             (string-append out data))))))))))
    (inputs
     (list avahi
           coreutils
           cups-filters
           gnutls
           linux-pam
           zlib))))

(define-public cups-pk-helper
  (package
    (name "cups-pk-helper")
    (version "0.2.7")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://freedesktop.org/software/"
                                  name "/releases/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "0cg8wbxpkz9bkpasz973cdazi02svqpbw9mafvpgrscg8kdhs1v6"))))
    (build-system meson-build-system)
    (arguments
     ;; XXX The tests require a running D-Bus and CUPS daemon, of course.
     (list #:tests? #f
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'install 'install-compatibility-symlink
                 ;; XXX Upstream (and, presumably, the world) has moved to
                 ;; /share/dbus-1 over /etc/dbus-1, but Guix System's
                 ;; dbus-configuration-directory has yet to catch up.
                 ;; TODO It should be properly fixed and this phase removed.
                 (lambda* (#:key outputs #:allow-other-keys)
                   (with-directory-excursion (assoc-ref outputs "out")
                     (mkdir-p "etc")
                     (symlink "../share/dbus-1" "etc/dbus-1")))))))
    (native-inputs
     (list intltool pkg-config `(,glib "bin")))
    (inputs
     (list glib polkit cups-minimal))
    (home-page "https://www.freedesktop.org/wiki/Software/cups-pk-helper/")
    (synopsis "PolicyKit helper to configure CUPS with fine-grained privileges")
    (description
     "This package provides the org.opensuse.CupsPkHelper.Mechanism DBus
system service which uses @file{cups-pk-helper-mechanism}.  This package
should only be used as part of the Guix cups-pk-helper service.")
    (license license:gpl2+)))

(define-public hplip
  (package
    (name "hplip")
    (version "3.24.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/hplip/hplip/" version
                                  "/hplip-" version ".tar.gz"))
              (sha256
               (base32
                "1yzil1fn9ib2hxmqh9in0apmmznvln0xahlxvyny59ck321l6xjx"))
              (patches (search-patches "hplip-usb-timeout.patch"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Delete non-free blobs: .so files, pre-compiled
                  ;; 'locatedriver' executable, etc.
                  (for-each delete-file
                            (find-files "."
                                        (lambda (file stat)
                                          (elf-file? file))))

                  ;; Now remove some broken references to them.
                  (delete-file "prnt/hpcups/ImageProcessor.h")
                  (substitute* "Makefile.in"
                    ((" -lImageProcessor ") " ")
                    ;; Turn shell commands inside an if…fi into harmless no-ops.
                    (("^(\\@HPLIP_BUILD_TRUE\\@[[:blank:]]*).*libImageProcessor.*"
                      _ prefix)
                     (string-append prefix ": ; \\\n"))
                    ;; Remove the lines adding file targets altogether.
                    (("^\\@FULL_BUILD_TRUE\\@.*libImageProcessor.*")
                     ""))

                  ;; Install binaries under libexec/hplip instead of
                  ;; share/hplip; that'll at least ensure they get stripped.
                  ;; It's not even clear that they're of any use though...
                  (substitute* "Makefile.in"
                    (("^dat2drvdir =.*")
                     "dat2drvdir = $(pkglibexecdir)\n")
                    (("^locatedriverdir =.*")
                     "locatedriverdir = $(pkglibexecdir)\n"))))))
    (outputs (list "out" "ppd"))
    (build-system gnu-build-system)
    (arguments
     (list
      #:imported-modules `((guix build python-build-system)
                           ,@%default-gnu-imported-modules)
      #:modules '((guix build gnu-build-system)
                  (guix build utils)
                  ((guix build python-build-system) #:prefix python:))
      #:configure-flags
      #~(list "--disable-imageProcessor-build"
              (string-append "--prefix=" #$output)
              (string-append "--sysconfdir=" #$output "/etc")
              (string-append "LDFLAGS=-Wl,-rpath=" #$output "/lib")
              ;; Disable until mime.types merging works (FIXME).
              "--disable-fax-build"
              "--enable-new-hpcups"
              ;; TODO add foomatic drv install eventually.
              ;; TODO --enable-policykit eventually.
              (string-append "--with-cupsfilterdir=" #$output
                             "/lib/cups/filter")
              (string-append "--with-cupsbackenddir=" #$output
                             "/lib/cups/backend")
              (string-append "--with-hpppddir=" #$output:ppd "/share/ppd/HP")
              (string-append "--with-icondir=" #$output "/share/applications")
              (string-append "--with-systraydir=" #$output "/etc/xdg")
              "--enable-qt5"
              "--disable-qt4")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-hard-coded-file-names
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let ((out #$output)
                    ;; FIXME: use merged ppds (I think actually only
                    ;; drvs need to be merged).
                    (cupsdir #$(this-package-input "cups-minimal")))
                (substitute* (find-files "." "\\.py$")
                  ;; Refer to the correct default configuration file name.
                  (("/etc/hp/hplip.conf")
                   (string-append out "/etc/hp/hplip.conf")))
                (substitute* "base/g.py"
                  (("'/usr/share;[^']*'")
                   (string-append "'" cupsdir "/share'"))
                  (("'/etc/hp/hplip.conf'")
                   (string-append "'" out "/etc/hp/hplip.conf" "'")))

                (substitute* "Makefile.in"
                  (("[[:blank:]]check-plugin\\.py[[:blank:]]") " ")
                  ;; FIXME Use beginning-of-word in regexp.
                  (("[[:blank:]]plugin\\.py[[:blank:]]") " ")
                  (("/usr/include/libusb-1.0")
                   (search-input-directory inputs "/include/libusb-1.0"))
                  (("hplip_statedir =.*$")
                   ;; Don't bail out while trying to create
                   ;; /var/lib/hplip.  We can safely change its value
                   ;; here because it's hard-coded in the code anyway.
                   "hplip_statedir = $(prefix)\n")
                  (("hplip_confdir = /etc/hp")
                   ;; This is only used for installing the default config.
                   (string-append "hplip_confdir = " out "/etc/hp"))
                  (("halpredir = /usr/share/hal/fdi/preprobe/10osvendor")
                   ;; We don't use hal.
                   (string-append "halpredir = " out
                                  "/share/hal/fdi/preprobe/10osvendor"))
                  (("rulesdir = /etc/udev/rules.d")
                   ;; udev rules will be merged by base service.
                   (string-append "rulesdir = " out "/lib/udev/rules.d"))
                  (("rulessystemdir = /usr/lib/systemd/system")
                   ;; We don't use systemd.
                   (string-append "rulessystemdir = " out "/lib/systemd/system"))
                  (("/etc/sane.d")
                   (string-append out "/etc/sane.d"))))))
          (add-after 'install 'install-models-dat
            (lambda* (#:key outputs #:allow-other-keys)
              (install-file "data/models/models.dat"
                            (string-append #$output "/share/hplip/data/models"))))
          (add-after 'install 'wrap-binaries
            ;; Scripts in /bin are all symlinks to .py files in /share/hplip.
            ;; Symlinks are immune to the Python build system's 'WRAP phase,
            ;; and the .py files can't be wrapped because they are reused as
            ;; modules.  Replacing the symlinks in /bin with copies and
            ;; wrapping them also doesn't work (“ModuleNotFoundError:
            ;; No module named 'base'”).  Behold: a custom WRAP-PROGRAM.
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (bin (string-append out "/bin"))
                     (site (python:site-packages inputs outputs)))
                (with-directory-excursion bin
                  (for-each (lambda (file)
                              (let ((target (readlink file)))
                                (delete-file file)
                                (with-output-to-file file
                                  (lambda _
                                    (format #t
                                            "#!~a~@
                                           export GUIX_PYTHONPATH=\"~a:~a\"~@
                                           exec -a \"$0\" \"~a/~a\" \"$@\"~%"
                                            (which "bash")
                                            site
                                            (getenv "GUIX_PYTHONPATH")
                                            bin target)))
                                (chmod file #o755)))
                            (find-files "." (lambda (file stat)
                                              (eq? 'symlink (stat:type stat))))))))))))
    ;; Note that the error messages printed by the tools in the case of
    ;; missing dependencies are often downright misleading.
    ;; TODO: hp-toolbox still fails to start with:
    ;;   from dbus.mainloop.pyqt5 import DBusQtMainLoop
    ;;   ModuleNotFoundError: No module named 'dbus.mainloop.pyqt5'
    (native-inputs (list perl pkg-config))
    (inputs
     (list cups-minimal
           dbus
           libjpeg-turbo
           libusb
           python
           python-dbus
           python-pygobject
           python-pyqt
           python-wrapper
           sane-backends-minimal
           net-snmp
           openssl
           avahi
           zlib))
    (home-page "https://developers.hp.com/hp-linux-imaging-and-printing")
    (synopsis "HP printer drivers")
    (description
     "Hewlett-Packard printer drivers and PostScript Printer Descriptions
(@dfn{PPD}s).")
    ;; The 'COPYING' file lists directories where each of these 3 licenses
    ;; applies.
    (license (list license:gpl2+ license:bsd-3 license:expat))))

(define-public hplip-minimal
  (package/inherit hplip
    (name "hplip-minimal")
    (arguments
     (substitute-keyword-arguments (package-arguments hplip)
       ((#:configure-flags cf)
        ;; Produce a "light build", meaning that only the printer (CUPS) and
        ;; scanner (SANE) support gets built, without all the 'hp-*'
        ;; command-line tools.
        #~(cons "--enable-lite-build"
                (delete "--enable-qt5" #$cf)))
       ((#:phases phases)
        ;; The 'wrap-binaries' is not needed here since the 'hp-*' programs
        ;; are not installed.
        #~(alist-delete 'wrap-binaries #$phases))))
    (inputs (remove (match-lambda
                      ((label . _)
                       (string-prefix? "python" label)))
                    (package-inputs hplip)))
    (synopsis "GUI-less version of hplip")))

(define-public foomatic-filters
  (package
    (name "foomatic-filters")
    (version "4.0.17")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://www.openprinting.org/download/foomatic/"
                    "foomatic-filters-" version ".tar.gz"))
              (sha256
               (base32
                "1qrkgbm5jay2r7sh9qbyf0aiyrsl1mdc844hxf7fhw95a0zfbqm2"))
              (patches
               (search-patches "foomatic-filters-CVE-2015-8327.patch"
                               "foomatic-filters-CVE-2015-8560.patch"))))
    (build-system gnu-build-system)
    (home-page "https://openprinting.github.io/projects/02-foomatic/")
    (native-inputs
     (list perl pkg-config))
    (inputs
     (list dbus a2ps))
    (arguments
     '( ;; Specify the installation directories.
       #:configure-flags (list (string-append "ac_cv_path_CUPS_BACKENDS="
                                              (assoc-ref %outputs "out")
                                              "/lib/cups/backend")
                               (string-append "ac_cv_path_CUPS_FILTERS="
                                              (assoc-ref %outputs "out")
                                              "/lib/cups/filter")
                               (string-append "ac_cv_path_PPR_INTERFACES="
                                              (assoc-ref %outputs "out")
                                              "/lib/ppr/interfaces")
                               (string-append "ac_cv_path_PPR_LIB="
                                              (assoc-ref %outputs "out")
                                              "/lib/ppr/lib")
                               "CFLAGS=-fcommon"
                               ;; For some reason these are misdiagnosed.
                               "ac_cv_func_malloc_0_nonnull=yes"
                               "ac_cv_func_realloc_0_nonnull=yes")
       #:test-target "tests"))
    (synopsis "Convert PostScript to the printer's native format")
    (description
     "This package contains filter scripts used by the printer spoolers to
convert the incoming PostScript data into the printer's native format using a
printer/driver specific, but spooler-independent PPD file.")
    (license license:gpl2+)))

(define-public foo2zjs
  (package
    (name "foo2zjs")
    (version "20200610.1")
    (source
     (origin
       (method url-fetch)
       ;; The upstream tarball is unversioned: use a stable snapshot.
       (uri (string-append "https://web.archive.org/web/20210224094943if_/"
                           "http://foo2zjs.rkkda.com/foo2zjs.tar.gz"))
       (sha256
        (base32 "03ncif50n7ck7drggqxbc7w0kgzdb90ha0dbvqk98ky8lw3k76xd"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (replace 'configure
                    (lambda* (#:key outputs #:allow-other-keys)
                      (substitute* (find-files "." "^Makefile$")
                        ;; Set the installation directory.
                        (("^PREFIX[[:blank:]]*=.*$")
                         (string-append "PREFIX = "
                                        (assoc-ref outputs "out")
                                        "\n"))
                        (("^UDEVBIN[[:blank:]]*=.*$")
                         "UDEVBIN = $(PREFIX)/bin\n")
                        ;; Don't try to chown/chgrp the installed files.
                        (("-oroot")
                         "")
                        (("-glp")
                         "")
                        ;; Placate the dependency checks.
                        (("/usr/include/stdio.h")
                         "/etc/passwd")
                        (("/usr/")
                         "$(PREFIX)/")
                        ;; Ensure fixed timestamps in man pages.
                        (("^MODTIME[[:blank:]]*=.*$")
                         "MODTIME = echo Thu Jan 01 01:00:00 1970\n"))
                      #t))
                  (add-before 'install 'make-install-dirs
                    (lambda* (#:key outputs #:allow-other-keys)
                      ;; Make missing install dirs
                      (let ((out (assoc-ref outputs "out"))
                            (dirs '("/share/cups/model"
                                    "/share/foomatic/db/source/opt"
                                    "/share/foomatic/db/source/printer"
                                    "/share/foomatic/db/source/driver"
                                    "/lib/cups/filter")))
                        (for-each (lambda (dir)
                                    (mkdir-p (string-append out dir)))
                                  dirs))))
                  (add-after 'install 'wrap-wrappers
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out"))
                            (ghostscript (assoc-ref inputs "ghostscript"))
                            (coreutils (assoc-ref inputs "coreutils"))
                            (sed (assoc-ref inputs "sed")))
                        (for-each (lambda (file)
                                    (wrap-program file
                                      `("PATH" ":" prefix
                                        (,(string-append ghostscript "/bin:"
                                                         coreutils "/bin:"
                                                         sed "/bin")))))
                                  (find-files (string-append
                                               out "/bin") "wrapper$")))))
                  (add-after 'install 'install-cups-filters-symlinks
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out")))
                        (for-each
                         (lambda (file)
                           (symlink file
                                    (string-append out "/lib/cups/filter/"
                                                   (basename file))))
                         (find-files (string-append out "/bin"))))))
                  (add-after 'install 'remove-pdf
                    (lambda* (#:key outputs #:allow-other-keys)
                      ;; Remove 'manual.pdf' which is (1) useless (it's a
                      ;; concatenation of man pages), and (2) not
                      ;; bit-reproducible due to <https://bugs.gnu.org/27593>.
                      (let ((out (assoc-ref outputs "out")))
                        (for-each delete-file
                                  (find-files out "^manual\\.pdf$"))
                        #t))))
       #:parallel-build? #f                       ;broken makefile
       #:tests? #f                                ;no tests
       #:make-flags '("CC=gcc")))
    (inputs
     (list bash-minimal
           coreutils
           sed
           ghostscript
           foomatic-filters))           ;for 'foomatic-rip'
    (native-inputs
     (list bc groff))
    ;; The domain has expired and no one has meaningfully taken up the torch.
    (home-page (string-append "https://web.archive.org/web/20210129024712/"
                              "http://foo2zjs.rkkda.com/"))
    (synopsis "Printer driver for ZjStream-based printers")
    (description
     "foo2zjs is a printer driver for printers that use the Zenographics
ZjStream wire protocol for their print data, often erroneously referred to as
winprinters or GDI printers.

It supports Minolta/QMS@tie{}Magicolor, Minolta@tie{}Color@tie{}PageWorks/Pro,
HP@tie{}LaserJet, and possibly other printers.  See @file{README} for details.")
    (license (list license:expat        ; icc2ps/*.[ch]
                   license:gpl2+))))    ; everything else

(define-public epson-inkjet-printer-escpr
  (package
    (name "epson-inkjet-printer-escpr")
    (version "1.7.24")
    ;; XXX: This currently works.  But it will break as soon as a newer version
    ;; is available since the URLs for older versions are not preserved.  Since
    ;; 1.8.0, source tarballs have been discontinued and only a ‘source RPM’ is
    ;; available.
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://download3.ebz.epson.net/dsc/f/03/00/14/31/"
                           "90/d2f5b28fcfaa0a1d1753eac5703aa5d88004ce06/"
                           "epson-inkjet-printer-escpr-1.7.24-1lsb3.2.tar.gz"))
       (sha256
        (base32 "0bwff3p6d0xgghf3bicylbxkv9vxz3gjjbr0iafyxz23kalzz9qj"))))
    (build-system gnu-build-system)
    (arguments
     (list #:modules
           `((srfi srfi-26)
             ,@%default-gnu-modules)
           #:configure-flags
           #~(list "--disable-static"
                   (string-append "--prefix=" #$output)
                   (string-append "--with-cupsfilterdir=" #$output "/lib/cups/filter")
                   (string-append "--with-cupsppddir=" #$output "/share/cups/model"))
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'patch-autotools-version-requirement
                 (lambda _
                   (substitute* "aclocal.m4"
                     (("1\\.15")
                      #$(package-version automake)))
                   (substitute* "configure"
                     (("^(ACLOCAL=).*" _ match)
                      (string-append match "aclocal"))
                     (("^(AUTOMAKE=).*" _ match)
                      (string-append match "automake")))))
               (add-after 'install 'compress-PPDs
                 (lambda _
                   (with-directory-excursion #$output
                     (for-each (cut invoke "gzip" "-9" <>)
                               (find-files "share/cups" "\\.ppd$"))))))))
    (native-inputs
     (list autoconf automake))
    (inputs
     (list cups-minimal))
    (synopsis "ESC/P-R printer driver")
    (description
     "This package provides a filter for @acronym{CUPS, the Common UNIX Printing
System} that offers high-quality printing with Seiko@tie{}Epson color ink jet
printers.  It can be used only with printers that support the Epson@tie{}ESC/P-R
language.")
    (home-page "https://download.ebz.epson.net/dsc/search/01/search/?OSC=LX")
    (license license:gpl2+)))

(define-public splix
  ;; Last released in 2009 <https://sourceforge.net/projects/splix/files/>.
  ;; Last SVN commit was 2013 <https://svn.code.sf.net/p/splix/code/splix/>.
  ;; Use a more maintained fork with several bug fixes and support for newer
  ;; printer models.
  (let ((commit "76268c4dd7dbc8218ea7426401104c3b40cc707a")
        (revision "315"))
    (package
      (name "splix")
      (version (git-version "2.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://gitlab.com/ScumCoder/splix")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1mxsvllwwr1v8sgrax0b7gkajjhnm0l06s67spmaxz47lyll1qab"))))
      (build-system gnu-build-system)
      ;; PPDs have been obsolete since CUPS 1.2 and make up 90% of total size.
      (outputs (list "out" "ppd"))
      (arguments
       `(#:modules
         ((srfi srfi-26)
          ,@%default-gnu-modules)
         #:make-flags
         (list (string-append "CUPSDRV="
                              (assoc-ref %outputs "out") "/share/cups/drv")
               (string-append "CUPSFILTER="
                              (assoc-ref %outputs "out") "/lib/cups/filter")
               (string-append "CUPSPPD="
                              (assoc-ref %outputs "ppd") "/share/cups/model")
               "CACHESIZE=100"          ; pages in RAM, ±300 KiB each
               "THREADS=4")             ; compress and print faster
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'enter-subdirectory
             ;; The git repository replicates the top-level SVN layout.
             (lambda _
               (chdir "splix")
               #t))
           (delete 'configure)          ; no configure script
           (add-before 'build 'build-.drv-files
             (lambda* (#:key make-flags #:allow-other-keys)
               (apply invoke "make" "drv" make-flags)))
           (add-after 'install 'install-.drv-files
             (lambda* (#:key make-flags #:allow-other-keys)
               (apply invoke "make" "install" "DRV_ONLY=1" make-flags)))
           (add-after 'install 'compress-PPDs
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((ppd (assoc-ref outputs "ppd")))
                 (for-each (cut invoke "gzip" "-9" <>)
                           (find-files ppd "\\.ppd$"))))))
         #:tests? #f))                  ; no test suite
      (inputs
       `(("cups" ,cups-minimal)
         ("jbigkit" ,jbigkit)
         ("zlib" ,zlib)))
      (synopsis "QPDL (SPL2) printer driver")
      (description
       "SpliX is a set of CUPS drivers for printers that speak @acronym{QPDL,
Quick Page Description Language}, also called @acronym{SPL2, Samsung Printer
Language version 2}.  These include many laser printers sold by Samsung,
Xerox, Lexmark, Toshiba, and Dell.

Colour printers need colour profile files to get better results.  These
@file{cms} files are provided by the printer's manufacturer and must be
obtained and installed separately.")
      (home-page "http://splix.ap2c.org/")
      (license license:gpl2))))

(define-public python-pycups
  (package
    (name "python-pycups")
    (version "2.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pycups" version ".tar.bz2"))
       (sha256
        (base32
         "140c7073bkhx8w9qpaynllhynkkg0rzj3a4wjh9fnj15yvjlqhsp"))))
    (build-system python-build-system)
    (arguments
     '(;; Tests require CUPS to be running
       #:tests? #f))
    (inputs
     (list cups))
    (home-page "https://github.com/zdohnal/pycups")
    (synopsis "Python bindings for libcups")
    (description
     "This package provides Python bindings for libcups, wrapping the CUPS
API.")
    (license license:gpl2+)))
