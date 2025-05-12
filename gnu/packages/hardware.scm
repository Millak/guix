;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018–2022 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2020 Brice Waegeneire <brice@waegenei.re>
;;; Copyright © 2021, 2023-2025 Evgeny Pisemsky <mail@pisemsky.site>
;;; Copyright © 2021 Léo Le Bouter <lle-bout@zaclys.net>
;;; Copyright © 2021 Denis Carikli <GNUtoo@cyberdimension.org>
;;; Copyright © 2021, 2022 Petr Hodina <phodina@protonmail.com>
;;; Copyright © 2021 Raghav Gururajan <rg@raghavgururajan.name>
;;; Copyright © 2021 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2021, 2022 John Kehayias <john.kehayias@protonmail.com>
;;; Copyright © 2022 Zhu Zihao <all_but_last@163.com>
;;; Copyright © 2022 Maxime Devos <maximedevos@telenet.be>
;;; Copyright © 2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2022 Marcel Kupiec <formbi@protonmail.com>
;;; Copyright © 2022 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2022 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2023 Spencer Skylar Chan <schan12@umd.edu>
;;; Copyright © 2023 Foundation Devices, Inc. <hello@foundationdevices.com>
;;; Copyright © 2024 Giacomo Leidi <goodoldpaul@autistici.org>
;;; Copyright © 2024 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2024 Jakob Kirsch <jakob.kirsch@web.de>
;;; Copyright © 2025 Eric Bavier <bavier@posteo.net>
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

(define-module (gnu packages hardware)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages avahi)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages bootloaders)
  #:use-module (gnu packages cdrom)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages debian)
  #:use-module (gnu packages disk)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages high-availability)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages lxqt)
  #:use-module (gnu packages messaging)
  #:use-module (gnu packages mtools)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages openldap)
  #:use-module (gnu packages onc-rpc)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages polkit)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages scanner)
  #:use-module (gnu packages security-token)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages video)
  #:use-module (gnu packages virtualization)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (guix build-system qt)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix svn-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1))

;; This is a module for packages related to physical hardware that don't (yet)
;; have a more specific home like gps.scm, security-token.scm, &c.

(define-public envytools
  (let ((commit "9014a51b1436461c7b3b005bdae72bf4912f4e72")
        (revision "1"))
    (package
      (name "envytools")
      (version (git-version "0.1" revision commit))
      (home-page "https://github.com/envytools/envytools")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url home-page)
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1lqh73yxd5jgv7b770m37zimzhyn4f3053jybkixkhvm93zka8vd"))))
      (build-system cmake-build-system)
      (native-inputs (list bison flex pkg-config))
      (inputs (list libxml2 python))
      (synopsis "Reverse-engineering tools for Nvidia's proprietary GPU drivers")
      (description
       "This package provides tools for exploring Nvidia's proprietary GPU
drivers, including an assembler and a disassembler for several GPU instruction
sets, and tools to deal with register databases.")
      (license license:expat))))

(define-public brillo
  (package
    (name "brillo")
    (version "1.4.12")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.com/cameronnemo/brillo")
                    (commit (string-append "v" version))))
              (sha256
               (base32
                "16n4j45mfhd1zxwbpl8342vyqf8rj3plhcl90xp02m46hn58v8bl"))
              (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:make-flags #~(list (string-append "CC="
                                          #$(cc-for-target))
                           (string-append "AADIR=" #$output "/etc/apparmor.d")
                           (string-append "PREFIX=" #$output))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-udev-rules
            (lambda _
              (substitute* "contrib/udev.in"
                (("/bin/chgrp")
                 (string-append #$coreutils
                                "/bin/chgrp"))
                (("/bin/chmod")
                 (string-append #$coreutils
                                "/bin/chmod")))))
          (delete 'configure)
          ;; Tests must be run on real hardware.
          (delete 'check)
          (add-after 'install 'install-udev-polkit
            (lambda* (#:key make-flags #:allow-other-keys)
              (map (lambda (target)
                     (apply invoke "make" target make-flags))
                   '("install.udev" "install.polkit")))))))
    (native-inputs (list go-github-com-go-md2man))
    (supported-systems
     (lset-difference string=? %supported-systems %hurd-systems))
    (home-page "https://gitlab.com/cameronnemo/brillo")
    (synopsis "Controls the brightness of backlight and LED devices on Linux")
    (description
     "Brillo is a command line tool to control the brightness of backlight and
LED devices on Linux.

Notable features include:

@itemize

@item Automatic best controller detection
@item Smooth transitions and natural brightness adjustments
@item Ability to save and restore brightness across boots
@item Directly using sysfs to set brightness without relying on X
@item Unprivileged access with no new setuid binaries
@item Containment with AppArmor

@end itemize")
    (license (list license:bsd-0 license:gpl3+))))

(define-public hw-probe
  (package
    (name "hw-probe")
    (version "1.6.5")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/linuxhw/hw-probe")
         (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1sbp0scdi54zwgvb1s3ki3cw8xnxaxzm5cicq2nn3a2b6n1d4ljs"))))
    (build-system perl-build-system)
    (arguments
     (list
      #:tests? #f                       ;no test suite
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-source
            (lambda* (#:key inputs outputs #:allow-other-keys)
              ;; Correct install prefix.
              (substitute* "Makefile"
                (("/usr") #$output))

              (define preserve
                ;; Either not available in Guix or better left untouched.
                '("$HWInfoCmd" "$CurlCmd" "$NvidiaSmi_Path" ;perl variables
                  "vblank_mode=0" "DRI_PRIME=1" ;environment variables
                  "sha512"                      ;fall-back to sha512sum
                  ;; hp-probe comes from the full 'hplib' package, which would
                  ;; pull Qt and increase the size of the closure by 600 MiB.
                  "hp-probe"

                  ;; Android.
                  "apk" "getprop"

                  ;; BSD-related.
                  "atactl" "acpiconf"
                  "bsdhwmon" "camcontrol"
                  "devinfo" "diskinfo" "disklabel"
                  "freebsd-version" "ghostbsd-version"
                  "hwstat" "kldstat" "mfiutil" "modstat" "mport"
                  "ofwdump" "opnsense-version"
                  "pcictl" "pcidump" "pciconf" "pkg" "pkg_info" "pkgin"
                  "start-hello" "sysinfo" "usbconfig" "usbdevs"

                  ;; Package managers.
                  "eopkg" "pacman" "swupd"

                  ;; Not packaged in Guix (TODO).
                  "apm"                                      ;apmd
                  "drm_info"                                 ;drm_info
                  "megactl"                                  ;megactl
                  "lspnp"                                    ;pnputils
                  "lsb_release"                              ;lsb-release
                  "lsinitrd"                                 ;dracut
                  "optirun"                                  ;bumblebee
                  "usbctl"                                   ;usbctl
                  "monitor-get-edid"                         ;monitor-edid
                  "journalctl" "systemctl" "systemd-analyze" ;systemd
                  "superiotool"                              ;superiotool
                  "x86info"                                  ;x86info

                  ;; Other.
                  "arcconf"               ;proprietary
                  "config"                ;unknown origin (Linux-related)
                  "dkms"                  ;unknown origin (Linux-related)
                  "amdconfig" "fglrxinfo" ;proprietary/obsolete
                  "geom"                  ;unknown origin
                  "hciconfig" "hcitool"   ;deprecated from bluez
                  "nm-tool"))           ;replaced by nmcli in network-manager

              (substitute* "hw-probe.pl"
                (("(check|find|run)Cmd\\(\"([^\" ]+)" _ prefix command)
                 (string-append
                  prefix "Cmd(\""
                  (if (member command preserve)
                      command
                      (or (false-if-exception
                           (search-input-file
                            inputs (string-append "bin/" command)))
                          (search-input-file
                           inputs (string-append "sbin/" command))))))
                (("(my \\$HWInfoCmd = \")hwinfo" _ head)
                 (string-append head (search-input-file inputs "sbin/hwinfo")))
                (("(my \\$CurlCmd = \")curl" _ head)
                 (string-append head (search-input-file inputs "bin/curl")))
                (("(\\$LsblkCmd = \")lsblk" _ head)
                 (string-append head (search-input-file inputs "bin/lsblk")))
                (("(\\$SmartctlCmd = \")smartctl" _ head)
                 (string-append head (search-input-file inputs "sbin/smartctl")))
                (("(my \\$FindmntCmd = \")findmnt" _ head)
                 (string-append head (search-input-file inputs "bin/findmnt")))
                (("(\\$DDCUtilCmd = \")ddcutil" _ head)
                 (string-append head (search-input-file inputs "bin/ddcutil")))
                (("(my \\$VaInfoCmd = \")vainfo" _ head)
                 (string-append head (search-input-file inputs "bin/vainfo")))
                (("(\\$CheckHddCmd = \")hdparm" _ head)
                 (string-append head (search-input-file inputs "sbin/hdparm")))
                (("(\\$USE_DIGEST_ALT = \")sha512sum" _ head)
                 (string-append head (search-input-file inputs "bin/sha512sum"))))))
          (delete 'configure)
          (add-after 'install 'wrap
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (define hw-probe (search-input-file outputs "bin/hw-probe"))
              ;; 'NeedProgs' core utilities are specially checked for
              ;; availability.  It's easier to wrap them in PATH than patching
              ;; their references.
              (define need-progs (list "sbin/dmidecode" "sbin/smartctl"
                                       "bin/lspci" "bin/lsusb" "bin/edid-decode"))
              (wrap-script hw-probe
                (list "PERL5LIB" 'prefix (list (getenv "PERL5LIB")))
                (list "PATH" 'prefix
                      (map (lambda (command)
                             (dirname (search-input-file inputs command)))
                           need-progs))))))))
    (inputs
     (list acpi
           acpica
           alsa-utils
           avahi
           bash-minimal
           coreutils
           cpuid
           cpupower
           curl
           ddcutil
           dmidecode
           dpkg
           edac-utils
           edid-decode
           efibootmgr
           efivar
           ethtool
           eudev
           findutils
           gpart
           grep
           guile-3.0                    ;for wrap-script
           hddtemp
           hdparm
           i2c-tools
           inxi
           iproute
           iw
           libva-utils
           lm-sensors
           mcelog
           memtester
           mesa-utils
           modem-manager
           module-init-tools
           neofetch
           net-tools
           network-manager
           numactl
           nvme-cli
           opensc
           openssl
           p7zip
           pciutils
           perl-data-dumper
           perl-digest-sha
           perl-libwww
           procps
           psmisc                       ;for pstree
           rpm
           sane-backends
           smartmontools
           sysstat
           upower
           usbutils
           util-linux
           wireless-tools
           vdpauinfo
           vulkan-tools
           xdpyinfo
           xinput
           xrandr
           xvinfo))
    (propagated-inputs (list hwinfo))
    (home-page "https://linux-hardware.org")
    (synopsis "Hardware Probe")
    (description "Hardware Probe is a tool to probe for hardware, check its
operability and find drivers.")
    (license (list license:lgpl2.1+ license:bsd-4)))) ;dual-licensed

(define-public hwinfo
  (package
    (name "hwinfo")
    (version "23.2")
    (home-page "https://github.com/openSUSE/hwinfo")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url home-page)
         (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0d9nhhi64d3i9x1bh3ksj0h5z2p4pwa0z88bc0jra9s39nf6q230"))
       (modules
        '((guix build utils)))
       (snippet
        #~(begin
            ;; Remove git2log program file.
            (delete-file "git2log")
            ;; Remove variables that depend on git2log.
            (substitute* "Makefile"
              (("GIT2LOG.*\\:=.*$") "")
              (("GITDEPS.*\\:=.*$") "")
              (("BRANCH.*\\:=.*$") ""))
            ;; Create version file.
            (call-with-output-file "VERSION"
              (lambda (port) (format port #$version)))))))
    (build-system gnu-build-system)
    (outputs '("out" "lib" "doc"))
    (arguments
     (list
      #:tests? #f                       ; no test-suite available
      #:make-flags
      #~(list (string-append "CC=" #$(cc-for-target))
              (string-append "LIBDIR=" #$output:lib "/lib")
              (string-append "VERSION=" #$version))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch
            (lambda _
              (let ((include (string-append #$output:lib "/include"))
                    (lib (string-append #$output:lib "/lib"))
                    (sbin (string-append #$output "/sbin"))
                    (share (string-append #$output "/share"))
                    (doc (string-append #$output:doc "/share/doc")))
                ;; Generate HTML documentation in the "doc" output.
                (mkdir-p doc)
                (substitute* "doc/libhd.doxy"
                  (("OUTPUT_DIRECTORY.*=.*libhd")
                   (string-append "OUTPUT_DIRECTORY = " doc "/libhd")))
                ;; Correct values of the version and install directories.
                (substitute* "Makefile"
                  (("/usr/include") include)
                  (("/(usr|var)/(lib|lib64)") lib)
                  (("/usr/sbin") sbin)
                  (("/usr/share") share)
                  (("\\$\\(DESTDIR\\)/sbin ") ""))
                ;; Add the "lib" output to the run-path.
                (substitute* "Makefile.common"
                  (("-Lsrc")
                   (string-append "-Lsrc " "-Wl,-rpath=" lib)))
                ;; Correct program name of the lexical analyzer.
                (substitute* "src/isdn/cdb/Makefile"
                  (("lex isdn_cdb.lex") "flex isdn_cdb.lex"))
                ;; Patch pkg-config file to point to the "lib" output.
                (substitute* "hwinfo.pc.in"
                  (("/usr") #$output:lib)))))
          (delete 'configure)
          (replace 'build
            (lambda* (#:key make-flags #:allow-other-keys)
              (apply invoke "make" "shared" make-flags)
              (apply invoke "make" "doc" make-flags)))
          (add-after 'install 'install-man-pages
            (lambda _
              (for-each
               (lambda (file)
                 (install-file file (string-append #$output "/share/man/man"
                                                   (string-take-right file 1))))
               (find-files "doc" "\\.[0-9]$")))))))
    (native-inputs
     (list doxygen flex perl pkg-config))
    (inputs
     (list libx86emu `(,util-linux "lib")))
    (synopsis "Hardware information tool")
    (description "HwInfo is used to probe for the hardware present in the system.
It can be used to generate a system overview log which can be later used for
support.")
    (license license:gpl2+)))

(define-public ckb-next
    (package
      (name "ckb-next")
      (version "0.6.0")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/ckb-next/ckb-next")
                      (commit (string-append "v" version))))
                (sha256
                 (base32
                  "1s6xz2d631rds0bsxk26smqjs9jg3lwnjrjh2sw8hc7h7l8jyiqv"))
                (file-name (git-file-name name version))))
      (build-system cmake-build-system)
      (arguments
       `(#:modules ((guix build cmake-build-system) (guix build qt-utils)
                    (guix build utils))
         #:imported-modules (,@%cmake-build-system-modules
                             (guix build qt-utils))
         #:tests? #f
         #:phases
         (modify-phases %standard-phases
           (add-before 'build 'patch-lib-udev
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (substitute* "src/daemon/cmake_install.cmake"
                 (("/lib/udev")
                  (string-append (assoc-ref outputs "out")
                                 "/lib/udev")))))
           (add-after 'install 'wrap-qt
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 (wrap-qt-program "ckb-next"
                                  #:output out
                                  #:inputs inputs)))))))
      (native-inputs (list qttools-5 pkg-config))
      (inputs (list qtbase-5
                    zlib
                    libdbusmenu-qt
                    quazip-5
                    pulseaudio
                    libxcb
                    xcb-util-wm
                    qtx11extras
                    eudev
                    bash-minimal))
      (home-page "https://github.com/ckb-next/ckb-next")
      (synopsis "Driver for Corsair keyboards and mice")
      (description
       "ckb-next is a driver for Corsair keyboards and mice.  It aims to bring
the features of Corsair's proprietary software to Linux-based operating
systems.  It already supports much of the same functionality, including full
RGB animations.")
      (license license:gpl2)))

(define-public ddcutil
  (package
    (name "ddcutil")
    (version "2.1.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rockowitz/ddcutil")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0wk82cdg7vddk6pbnn6qq3p71j4bppq13is2ck40glig08ax1bg5"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake libtool pkg-config))
    (inputs
     (list eudev
           glib
           jansson
           kmod
           i2c-tools
           libdrm                       ;enhanced diagnostics
           libusb                       ;support USB monitors
           libx11                       ;enhanced diagnostics
           libxrandr
           zlib))
    (home-page "https://www.ddcutil.com/")
    (synopsis "Control external monitor settings")
    (description
     "ddcutil can query and modify most external monitors' settings, such as
brightness, colour levels, and input sources.  Generally speaking, any setting
that can be changed by pressing buttons on the monitor can be modified by
ddcutil.

ddcutil communicates directly with monitors implementing the Monitor Control
Command Set (@dfn{MCCS}).  It usually does so through the the Display Data
Channel Command Interface (@dfn{DDC/CI}) protocol on the I2C bus, but can also
communicate over USB as per the USB Monitor Control Class Specification.

One particular use case is in colour profile management.  Monitor calibration
is relative to the monitor colour settings currently in effect, e.g. red gain.
ddcutil allows colour-related settings to be saved at the time a monitor is
calibrated, and restored when the calibration is applied.

This package includes udev rules that can be used by adding this package to
the @code{rules} field of the @code{udev-configuration} record.  It gives
read/write access to i2c devices to users in the @samp{i2c} group.")
    (license (list license:bsd-3        ; FindDDCUtil.cmake
                   license:gpl2+))))    ; everything else

(define-public ddcui
  (package
    (name "ddcui")
    (version "0.5.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rockowitz/ddcui")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1hq19gdy9ybraclkqvv1nlf46irql5b6wrc1y6wi0ihkqly20vgz"))))
    (build-system qt-build-system)
    (arguments
     (list #:tests? #f))                    ; No test suite
    (native-inputs
     (list pkg-config qttools-5))
    (inputs
     (list ddcutil glib qtbase-5 qtwayland-5))
    (home-page "https://www.ddcutil.com/")
    (synopsis "Graphical user interface for ddcutil")
    (description "ddcui is a graphical user interface for ddcutil, implemented
using Qt.  It provide a dynamic way to inspect and configure external monitors
through the Display Data Channel Command Interface (@dfn{DDC/CI}) protocol.")
    (license (list license:gpl2+))))

(define-public edid-decode
  (let ((commit "3d635499e4aca3319f0796ba787213c981c5a770") ; 2024-04-02
        (revision "1"))
    (package
      (name "edid-decode")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (file-name (git-file-name name version))
         (uri (git-reference
               (url "git://linuxtv.org/edid-decode.git")
               (commit commit)))
         (sha256
          (base32 "18s2pwm45mzgm0rfw3wf0m349p6381i6iwbylxypizqcsvgwxb3f"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f                     ; No test suite
         #:make-flags
         (list (string-append "DESTDIR=" (assoc-ref %outputs "out"))
               "bindir=/bin" "mandir=/share/man")
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-cross-compilation
             (lambda* (#:key native-inputs target #:allow-other-keys)
               (when target
                 (substitute* "Makefile"
                   (("\\$\\(CXX\\)")
                    (string-append target "-g++"))))
               #t))
           (delete 'configure))))
      (home-page "https://git.linuxtv.org/edid-decode.git/")
      (synopsis "Decode @dfn{EDID} data in human-readable format")
      (description "edid-decode decodes @dfn{EDID} monitor description data in
human-readable format and checks if it conforms to the standards.")
      (license license:expat))))

(define-public h-client
  (let ((commit "e6c78b16e034ccf78ae9cb4c29268c2f57a30bfc")
        (revision "1"))
    (package
      (name "h-client")
      (version (git-version "0.0a0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://git.savannah.gnu.org/git/h-client.git/")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0hm86d51kj5r3yxq4c23aa57cs8igz3wrkbjn20z4frx75rpf46m"))))
      (build-system python-build-system)
      (arguments
       (list
        #:imported-modules `(,@%python-build-system-modules
                             ,@%glib-or-gtk-build-system-modules)
        #:modules '(((guix build glib-or-gtk-build-system) #:prefix glib:)
                    (guix build python-build-system)
                    (guix build utils))
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'generate-gdk-pixbuf-loaders-cache-file
              (assoc-ref glib:%standard-phases
                         'generate-gdk-pixbuf-loaders-cache-file))
            (add-after 'install 'glib-or-gtk-compile-schemas
              (assoc-ref glib:%standard-phases 'glib-or-gtk-compile-schemas))
            (add-after 'install 'glib-or-gtk-wrap
              (assoc-ref glib:%standard-phases 'glib-or-gtk-wrap))
            (add-after 'glib-or-gtk-wrap 'wrap-more
              (lambda* (#:key inputs outputs #:allow-other-keys)
                (wrap-script (search-input-file outputs "bin/h-client")
                  ;; Wrap GI_TYPELIB_PATH to avoid the error "ValueError:
                  ;; Namespace GdkPixbuf not available".
                  `("GI_TYPELIB_PATH" = (,(getenv "GI_TYPELIB_PATH")))
                  `("PATH" = (,(dirname (search-input-file
                                         inputs "bin/lspci"))
                              ,(dirname (search-input-file
                                         inputs "bin/lsusb"))))))))))
      (inputs
       (list gdk-pixbuf
             gobject-introspection      ;for GI_TYPELIB_PATH
             guile-3.0
             gtk+
             pciutils
             python-pycurl
             python-pygobject
             usbutils))
      (synopsis "Graphical client for the h-node hardware database project")
      (description
       "The h-node project (https://www.h-node.org) aims to build a database of
hardware that works with fully free operating systems.  h-client is a GTK+
graphical client that is able to retrieves information on the hardware inside
the computer it's running on, and on peripherals connected to it, and helps
you submit that information to the h-node project along with whether the
hardware works with a fully free operating system or not.")
      (home-page "https://savannah.nongnu.org/projects/h-source/")
      (license license:gpl3+))))

(define-public headsetcontrol
  (package
    (name "headsetcontrol")
    (version "2.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Sapd/HeadsetControl")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0l8bvdaj70s6qf8pds8bl367355j9lqb9mvc4lskz0n4ya8xs2dl"))))
    (build-system cmake-build-system)
    (inputs
     (list hidapi))
    (home-page "https://github.com/Sapd/HeadsetControl")
    (synopsis "Sidetone and Battery status for USB headsets")
    (description
     "Headsetcontrol is a tool to control certain aspects of USB-connected
headsets.  Currently, support is provided for adjusting sidetone, getting
battery state, controlling LEDs, and setting the inactive time.")
    (license license:gpl3+)))

(define-public hueplusplus
  (package
    (name "hueplusplus")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/enwi/hueplusplus")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1jy8m2a0h0kf0aw8jbniz069q9j7cx67b1zlv2vz1ymq921qk0pm"))
       (patches
        (search-patches "hueplusplus-mbedtls.patch"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f)) ;; Tests require Google's gtest and gmock
    (inputs
     (list mbedtls-lts))
    (synopsis "C++ library to control Philips Hue lights")
    (description "Hueplusplus is a library for controlling Philips Hue lights.
Features:

@itemize
@item find bridges with SSDP or set an ip manually
@item all common light functions (brightness, color, temperature)
@item extended @code{alert()} functions, which alert in a specific
color (good for notifications)
@item supports sensors, rules, groups, scenes and schedules
@item streaming with entertainment mode
@item documented with doxygen
@end itemize")
    (home-page "https://github.com/enwi/hueplusplus")
    (license license:lgpl3+)))

(define-public i7z
  (let ((revision "0")
        (commit "1a41ff13db747e962456ddbb5ccb2b7fc43ca0cb"))
    (package
      (name "i7z")
      (version (git-version "0.28" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/afontenot/i7z")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0jxm63a8y1mfl1sa4mzzfs3bgnym6achj1yc0jglmp05xal16lm1"))
         (patches
          (search-patches "i7z-gcc-10.patch"))
         (modules '((guix build utils)))
         (snippet
          '(begin
             (for-each delete-file-recursively
                       (list "src/GUI"
                             "src/perfmon-i7z"
                             "scripts"))))))
      (build-system gnu-build-system)
      (arguments
       `(#:make-flags
         (list (string-append "prefix=" (assoc-ref %outputs "out"))
               (string-append "CC=" ,(cc-for-target)))
         #:tests? #f                    ; no test suite
         #:phases
         (modify-phases %standard-phases
           (delete 'configure))))       ; no configure script
      (inputs
       (list ncurses))
      (home-page "https://github.com/afontenot/i7z")
      (synopsis "Thermal and C-state reporting on older Intel Core CPUs")
      (description
       "The @command{i7z} utility accurately measures the current frequency
and temperature of older Intel Core (i3, i5, and i7) processors including the
Nehalem, Sandy Bridge, and Ivy Bridge generations.  Reliable support for newer
CPUs is not guaranteed, as this package has not seen significant development
since 2013.

If your processor is supported, you'll get detailed reports on Turbo Boost and
clock multipliers, core voltage, and time spent in different C-states.  This
information can be viewed in real time and/or logged to a file.")
      (supported-systems (list "x86_64-linux"))
      (license license:gpl2))))

(define-public libsmbios
  (package
    (name "libsmbios")
    (version "2.4.3")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url (string-append "https://github.com/dell/" name))
         (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0krwwydyvb9224r884y1mlmzyxhlfrcqw73vi1j8787rl0gl5a2i"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("gettext" ,gettext-minimal)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)
       ("perl" ,perl)
       ("python" ,python)))
    (inputs
     (list libxml2))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'bootstrap
           (lambda _ (invoke "autoreconf" "-vfi"))))))
    (synopsis "Library for interacting with Dell SMBIOS tables")
    (description
     "libsmbios provides a library to interface with the SMBIOS tables.  It
also provides extensions for proprietary methods of interfacing with Dell
specific SMBIOS tables.")
    (home-page "https://github.com/dell/libsmbios")
    (supported-systems (list "i686-linux" "x86_64-linux"))
    (license
     (list license:osl2.1 license:gpl2+ license:bsd-3 license:boost1.0))))

(define-public liquidctl
  (package
    (name "liquidctl")
    (version "1.13.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/liquidctl/liquidctl")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0hpxkrfxm9c4v5ld7bh6qs9fmq9imz8s5i9l0l78l47bcm12nkrd"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-before 'check 'set-runtime-dir
                 (lambda _
                   (setenv "XDG_RUNTIME_DIR" "/tmp"))))))
    (native-inputs (list python-pytest))
    (propagated-inputs
     (list python-colorlog
           python-crcmod
           python-docopt
           python-hidapi
           python-pillow
           python-pyusb
           python-smbus))
    (home-page "https://github.com/liquidctl/liquidctl")
    (synopsis "Drivers and tools for liquid cooling equipment")
    (description "Liquidctl is a package with tools, drivers and a Python
library to work with liquid cooling equipment such as @acronym{AIO, All-In-One}
coolers, fan controllers and other devices.")
    (license license:gpl3+)))

;; Distinct from memtest86, which is obsolete.
(define-public memtest86+
  (package
    (name "memtest86+")
    (version "7.20")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/memtest86plus/memtest86plus")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ifly0jbq4xy81mc6k621a8rr0ipjzn7783v2b535s1s55xrz7i5"))
       (patches
        (search-patches "memtest86+-build-reproducibly.patch"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f                       ; no way to test this
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-broken-Makefiles
            (lambda _
              (substitute* (list "build32/Makefile"
                                 "build64/Makefile")
                (("/sbin/(mkdosfs)" _ command)
                 command))))
          (delete 'configure)           ; no configure script
          (add-before 'build 'enter-build-directory
            (lambda _
              (chdir #$(if (target-x86-32?)
                           "build32"
                           "build64"))))
          (replace 'build
            (lambda* (#:key inputs make-flags #:allow-other-keys)
              (apply invoke
                     "make" "all" "grub-iso" ; more options than memtest.iso
                     (string-append "GRUB_FONT_DIR="
                                    (search-input-directory inputs
                                                            "share/grub"))
                     (string-append "GRUB_LIB_DIR="
                                    (search-input-directory inputs
                                                            "lib/grub"))
                     make-flags)))
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (lib (string-append out "/lib/memtest86+"))
                     (doc (string-append out "/share/doc/memtest86+-"
                                         #$version)))
                (for-each
                 (lambda (file)
                   (install-file file lib))
                 (list "grub-memtest.iso"
                       "memtest.bin"
                       "memtest.efi"))
                (chdir "..")
                (install-file "README.md" doc)))))))
    (native-inputs
     (list dosfstools grub-hybrid mtools xorriso))
    (supported-systems (list "i686-linux" "x86_64-linux"))
    (home-page "https://www.memtest.org/")
    (synopsis "Thorough real-mode memory tester")
    (description
     "Memtest86+ is a thorough, stand-alone memory test for x86 systems.  It
repeatedly writes different patterns to all memory locations, reads them back
again, and verifies whether the result is the same as what was written.  This
can help debug even intermittent and non-deterministic errors.

It runs independently of any operating system, at computer boot-up, so that it
can scan as much of your RAM as possible for hardware defects.")
    (license license:gpl2)))

(define-public memtester
  (package
    (name "memtester")
    (version "4.6.0")
    (source
     (origin
       (method url-fetch)
       ;; Even the latest release is available under 'old-versions/'.
       (uri (list
             (string-append "https://pyropus.ca/software/memtester/old-versions/"
                            "memtester-" version ".tar.gz")
             ;; XXX ‘pyropus.ca’ redirects to ‘pyropus.ca.’.  Valid, but wreaks
             ;; havoc with Guile's Web stack & TLS verification.
             ;; Remove this random mirror when that changes.
             (string-append "https://ftp.dimensiondata.com/mirrors/"
                            "ftp.gentoo.org/distfiles/3e/"
                            "memtester-" version ".tar.gz")))
       (sha256
        (base32 "0bmv7n7gj02pda8mwif08xk63xc20r65q1pr099fz30cx2vlxzn9"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list ,(string-append "CC=" (cc-for-target)))
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           ;; This is a home-brewed configuration system where the cc/ld command
           ;; lines are stored in one-line files.
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out")))
               (substitute* (list "conf-cc" "conf-ld")
                 (("^cc") "gcc"))
               (substitute* "Makefile"
                 (("(INSTALLPATH.*=).*" _ assignment)
                  (string-append assignment out)))
               #t)))
         (replace 'check
           ;; There is no test suite. Test some RAM for a single iteration.
           (lambda _
             (invoke "./memtester" "64K" "1"))))))
    (home-page "http://pyropus.ca/software/memtester/")
    (synopsis "User-space memory subsystem tester")
    (description
     "Memtester stress-tests the memory subsystem of your operating system and
computer.  It repeatedly writes different patterns to all memory locations,
reads them back again, and verifies whether the result is the same as what was
written.  This can help debug even intermittent and non-deterministic errors.

Memtester runs entirely in user space.  This means that you don't need to reboot
to test your memory, but also that it's not possible to test all of the RAM
installed in the system.

It can also be told to test memory starting at a particular physical address.")
    (license license:gpl2)))

(define-public msr-tools
  (package
    (name "msr-tools")
    (version "1.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://01.org/sites/default/files/downloads/"
                           name "/" name "-" version ".zip"))
       (sha256
        (base32 "07hxmddg0l31kjfmaq84ni142lbbvgq6391r8bd79wpm819pnigr"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list (string-append "sbindir=" (assoc-ref %outputs "out") "/sbin"))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)            ; no configure script
         (add-before 'install 'create-output-directory
           (lambda* (#:key outputs #:allow-other-keys)
             ;; 'make install' assumes that sbindir exists.
             (let* ((out  (assoc-ref outputs "out"))
                    (sbin (string-append out "/sbin")))
               (mkdir-p sbin)
               #t))))
       #:tests? #f))                    ; no test suite
    (native-inputs
     (list unzip))
    ;; These registers and the CPUID instruction only exist on (most) x86 chips.
    (supported-systems (list "i686-linux" "x86_64-linux"))
    (home-page "https://01.org/msr-tools/")
    (synopsis "Read and write Model-Specific Registers (@dfn{MSR})")
    (description
     "The MSR Tools project provides console utilities to directly access the
Model-Specific Registers (@dfn{MSR}s) and CPU ID of Intel-compatible processors:

@itemize
@item @command{cpuid}: show identification and feature information of any CPU
@item @command{rdmsr}: read MSRs from any CPU or all CPUs
@item @command{wrmsr}: write to MSRs on any CPU or all CPUs
@end itemize

These tools can be used to query and modify certain low-level CPU parameters,
such as the Turbo Boost ratio and Thermal Design Power (@dfn{TDP}) limits.

MSR addresses differ (greatly) between processors, and any such modification can
be dangerous and may void your CPU or system board's warranty.")
    (license license:gpl2)))     ; cpuid.c is gpl2, {rd,wr}msr.c are gpl2+

(define-public openhmd
  (package
    (name "openhmd")
    (version "0.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/OpenHMD/OpenHMD")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1hkpdl4zgycag5k8njvqpx01apxmm8m8pvhlsxgxpqiqy9a38ccg"))))
    (build-system meson-build-system)
    (arguments
     `(#:tests? #f)) ; no test target although there is a test folder
    (native-inputs
     (list pkg-config))
    (inputs
     (list hidapi))
    (home-page "http://www.openhmd.net/")
    (synopsis "API and drivers for immersive technology")
    (description "OpenHMD aims to provide an API and drivers for immersive
technology, such as head mounted displays with built in head tracking.")
    (license license:boost1.0)))

(define-public openrgb
  (package
    (name "openrgb")
    (version "0.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/CalcProgrammer1/OpenRGB")
             (commit (string-append "release_" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0rdh87w4j47dr0vakva94fhcbdc67d9aad0p3najg9zf8zhf64jw"))
       (patches
        (search-patches "openrgb-unbundle-hueplusplus.patch"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Delete many of the bundled libraries.
           (for-each delete-file-recursively
                     (list "dependencies/hidapi-win"
                           "dependencies/hueplusplus-1.0.0"
                           "dependencies/json"
                           "dependencies/libusb-1.0.22"
                           "dependencies/macUSPCIO"
                           "dependencies/mbedtls-2.24.0"
                           "dependencies/NVFC"
                           "dependencies/openrazer-win32"
                           "dependencies/winring0"
                           ;; Some bundled appimages
                           "scripts/tools"))))))
    (build-system qt-build-system)
    (arguments
     (list
       #:tests? #f ; doesn't have tests
       #:make-flags
       #~(list (string-append "INSTALL_ROOT=" #$output ))
       #:phases
       #~(modify-phases %standard-phases
           (add-after 'unpack 'unbundle
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "OpenRGB.pro"
                 (("dependencies/hueplusplus-1.0.0/include/hueplusplus")
                  (string-append #$(this-package-input "hueplusplus")
                                 "/include/hueplusplus"))
                 (("dependencies/json")
                  (string-append #$(this-package-input "nlohmann-json")
                                 "/include/nlohmann")))))
           (add-after 'unpack 'patch-chmod
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "scripts/build-udev-rules.sh"
                (("/bin/chmod") (string-append (assoc-ref inputs "coreutils")
                                               "/bin/chmod")))))
           ;; Call qmake instead of configure to create a Makefile.
           (replace 'configure
             (lambda _ (invoke "qmake" "PREFIX=/" "OpenRGB.pro"))))))
    (inputs
     (list coreutils
           hidapi
           hueplusplus
           nlohmann-json
           libusb
           mbedtls-lts
           qtbase-5
           qtwayland-5))
    (native-inputs
     (list pkg-config
           qttools-5))
    (synopsis "RGB lighting control")
    (description
     "OpenRGB is lighting control that doesn't depend on manufacturer software.
ASUS, ASRock, Corsair, G.Skill, Gigabyte, HyperX, MSI, Razer, ThermalTake, and more
supported.

Features:

@itemize
@item Set colors and select effect modes for a wide variety of RGB hardware
@item Save and load profiles
@item Control lighting from third party software using the OpenRGB SDK
@item Command line interface
@item Connect multiple instances of OpenRGB to synchronize lighting across multiple PCs
@item Can operate standalone or in a client/headless server configuration
@item View device information
@item No official/manufacturer software required
@item Graphical view of device LEDs makes creating custom patterns easy
@end itemize")
    (home-page "https://openrgb.org/")
    (license license:gpl2))) ; Included libccmmk is lgpl3+, CRC is bsd-3

(define-public wavemon
  (package
    (name "wavemon")
    (version "0.9.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/uoaerg/wavemon")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0s3yz15vzx90fxyb8bgryksn0cr2gpz9inbcx4qjrgs7zfbm4pgh"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list "CC=gcc"
             ;; Makefile.in (ab)uses $(datadir) as $(docdir). Set it to Guix's
             ;; standard --docdir since it's only used as such.
             (string-append "datadir=" (assoc-ref %outputs "out")
                            "/share/doc/" ,name "-" ,version))
       #:tests? #f))                    ; no tests
    (native-inputs
     (list pkg-config))
    (inputs
     (list libcap libnl ncurses))
    (home-page "https://github.com/uoaerg/wavemon")
    (synopsis "Wireless network device monitor")
    (description
     "Wavemon is a wireless device monitor with an interactive ncurses terminal
interface.  It can display and plot signal and noise levels in real time.  It
also reports packet statistics, device configuration, network parameters, and
access points and other wireless clients of your wireless network hardware.

Wavemon should work (with varying levels of detail and features) with any device
supported by the Linux kernel.")
    ;; Source file headers still say GPL2+, but the authorial intent
    ;; (from COPYING and the F9 'about' screen) is clearly GPL3+.
    (license license:gpl3+)))

(define-public rkdeveloptool
  (let ((commit "6e92ebcf8b1812da02663494a68972f956e490d3")
        (revision "0"))
    (package
      (name "rkdeveloptool")
      (version (git-version "1.3" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/rockchip-linux/rkdeveloptool")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0zwrkqfxd671iy69v3q0844gfdpm1yk51i9qh2rqc969bd8glxga"))
         (snippet
          #~(begin
              ;; https://github.com/rockchip-linux/rkdeveloptool/pull/57
              (use-modules (guix build utils))
              (substitute* "main.cpp"
                (("snprintf\\(buffer, sizeof\\(buffer\\), \"\\%s\", chip)")
                 "memccpy(buffer, chip, '\\0', sizeof(buffer))"))))))
      (build-system gnu-build-system)
      (native-inputs
       (list autoconf automake pkg-config))
      (inputs
       (list libusb))
      (home-page "https://github.com/rockchip-linux/rkdeveloptool")
      (synopsis "Read from and write to RockChicp devices over USB")
      (description
       "Rkdeveloptool can read from and write to RockChip devices over USB, such
as the Pinebook Pro.")
      (license license:gpl2+))))

(define-public usbguard
  (package
    (name "usbguard")
    (version "1.1.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/USBGuard/usbguard")
                    (commit (string-append "usbguard-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32 "10qqjk7hsycc6hk51abwcld7i48038zqi1jzli59cfvc76ikrxj5"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-bootstrap-script
            (lambda _
              ;; Don't attempt to fetch git submodules.
              (substitute* "autogen.sh"
                (("^git submodule.*")
                 ""))))
          (add-after 'bootstrap 'patch-build-scripts
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "configure"
                (("/usr/include/catch")
                 (dirname (search-input-file inputs "include/catch.hpp"))))
              ;; Do not create log directory.
              (substitute* "Makefile.in" ((".*/log/usbguard.*") ""))
              ;; Disable LDAP tests: they use 'sudo'.
              (substitute* "src/Tests/Makefile.in"
                (("\\$\\(am__append_2\\)") ""))))
          (add-after 'install 'delete-static-library
            (lambda args
              ;; It can't be directly disabled since it's needed for the tests.
              (delete-file (string-append #$output
                                          "/lib/libusbguard.a"))))
          (add-after 'install 'install-zsh-completion
            (lambda args
              (let ((site-functions
                     (string-append #$output "/share/zsh/site-functions")))
                (mkdir-p site-functions)
                (copy-file "scripts/usbguard-zsh-completion"
                           (string-append site-functions "/_usbguard"))))))
      #:make-flags
      #~(list (string-append "BASH_COMPLETION_DIR="
                             #$output
                             "/etc/bash_completion.d"))
      #:configure-flags
      #~(list
         "--localstatedir=/var"
         "--enable-systemd=no"
         "--with-ldap"
         "--with-dbus"
         "--with-polkit")))
    (inputs
     (list audit
           catch-framework
           dbus-glib
           openldap
           libcap-ng
           libseccomp
           libsodium
           pegtl
           polkit
           protobuf
           libqb))
    (native-inputs
     (list asciidoc
           autoconf
           automake
           libtool
           bash-completion
           `(,glib "bin")
           umockdev
           libxml2
           libxslt
           pkg-config))
    (home-page "https://usbguard.github.io")
    (synopsis "Helps to protect your computer against rogue USB devices (a.k.a. BadUSB)")
    (description "USBGuard is a software framework for implementing USB device
authorization policies (what kind of USB devices are authorized) as well as
method of use policies (how a USB device may interact with the system).
Simply put, it is a USB device whitelisting tool.")
    (license license:gpl2)))

(define-public screentest
  (package
    (name "screentest")
    (version "3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/TobiX/screentest")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "00422jbl8l10iw82m9m8sikaa9fwlnj7mgzvmnnazyq383aa1dkm"))))
    (build-system meson-build-system)
    (inputs (list glib gtk+))
    (native-inputs (list gettext-minimal pkg-config))
    (synopsis "Simple screen testing tool")
    (description
     "This is a program for testing the quality of CRT/LCD
screens.  It displays various patterns and allows you to estimate the quality
of your CRT/LCD monitor.")
    (home-page "https://github.com/TobiX/screentest")
    (license license:gpl2)))

(define-public tpm2-tss
  (package
    (name "tpm2-tss")
    (version "3.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/tpm2-software/tpm2-tss"
                           "/releases/download/" version "/tpm2-tss-" version
                           ".tar.gz"))
       (sha256
        (base32 "05xynpwq851fp8f5fy7ac0blvz8mr5m5cbqj3gslgbwv63kjnfbq"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list curl json-c openssl))
    (home-page "https://tpm2-software.github.io/")
    (synopsis "OSS Implementation of the TCG TPM2 Software Stack (TSS2)")
    (description
     "This package provides the @acronym{TCG, Trusted Computing Group}
@acronym{TSS2, TPM2 Software Stack}.  The stack contains libtss2-fapi,
libtss2-esys, libtss2-sys, libtss2-mu, libtss2-tcti-device, libtss2-tcti-swtpm
and libtss2-tcti-mssim.")
    (license license:bsd-2)))

(define-public tpm2-tools
  (package
    (name "tpm2-tools")
    (version "5.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/tpm2-software/tpm2-tools/"
                           "releases/download/" version "/"
                           "tpm2-tools-" version ".tar.gz"))
       (sha256
        (base32 "08y16q92dh7frsyw0zlm3q9gsfqyls0li248s2pgsysk633lknqz"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf
           automake
           curl
           libtool
           gnu-gettext
           openssl
           pandoc
           pkg-config
           tpm2-tss))
    (home-page "https://github.com/tpm2-software/tpm2-tools")
    (synopsis "Tools for the Trusted Platform Module (TPM 2.0)")
    (description
     "This package provides user tools for the Trusted Computing Group's (TCG)
TPM2 Software Stack (TSS).  These programs help with common tasks such as key
management, attestation, encryption, and signing.")
    (license license:bsd-3)))

(define-public libcpuid
  ;; We need to remove blobs from the source, first we have to isolate the blob
  ;; source in build system.
  ;; See https://github.com/anrieff/libcpuid/pull/159.
  (let ((commit "2e61160983f32ba840b2246d3c3850c44626ab0d")
        (revision "1"))
    (package
      (name "libcpuid")
      (version (git-version "0.5.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/anrieff/libcpuid")
               (commit commit)))
         (sha256
          (base32 "1mphvkiqq6z33sq6i490fq27sbyylacwrf8bg7ccvpcjms208sww"))
         (modules '((guix build utils)))
         (snippet
          ;; Now remove blobs.
          #~(begin
              (delete-file "libcpuid/msrdriver.c")
              (delete-file-recursively "contrib/MSR Driver")))
         (file-name (git-file-name name version))))
      (build-system cmake-build-system)
      (arguments
       (list
        #:configure-flags #~(list "-DLIBCPUID_TESTS=ON")
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'absolutize
              (lambda* (#:key inputs #:allow-other-keys)
                ;; Linux specific
                (when #$(target-linux?)
                  (substitute* "libcpuid/rdmsr.c"
                    (("modprobe") (which "modprobe")))))))))
      (inputs
       (if (target-linux?)
           (list kmod)
           '()))
      (native-inputs (list python-3))   ;required by tests
      (supported-systems
       (filter (lambda (t) (or (target-x86-64? t) (target-x86-32? t)))
               %supported-systems))
      (home-page "https://libcpuid.sourceforge.net/")
      (synopsis "Small library for x86 CPU detection and feature extraction")
      (description "Libcpuid is a small C library to get vendor, model, branding
string, code name and other information from x86 CPU. This library is not to be
confused with the @code{cpuid} command line utility from package @code{cpuid}.")
      (license license:bsd-2))))

(define-public liblxi
  (package
    (name "liblxi")
    (version "1.20")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/lxi-tools/liblxi")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1cc95ggs64jqq9lk5c8fm4nk6fdnv1x7lr3k4znamj0vv6w22bcd"))))
    (build-system meson-build-system)
    (native-inputs
     (list cmake pkg-config))
    (inputs
     (list avahi libtirpc libxml2))
    (home-page "https://lxi-tools.github.io/")
    (synopsis "@acronym{LXI, LAN eXtensions for Instrumentation} library")
    (description
     "This library offers a simple API for communicating with instruments
compatible with the @acronym{LXI, LAN eXtensions for Instrumentation} standard
that defines communication protocols for instrumentation and data acquisition
systems using Ethernet.  Applications can use liblxi to discover instruments on
your network, send SCPI commands, and receive responses.")
    (license license:bsd-3)))

(define-public lxi-tools
  (package
    (name "lxi-tools")
    (version "2.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/lxi-tools/lxi-tools")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1xc99xhca386az73rpsrf3z0j7y0hrv0xcwj1dr2ahr7lhnjznqp"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:glib-or-gtk? #true
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'skip-gtk-update-icon-cache
            (lambda _
              (substitute* "build-aux/meson/postinstall.py"
                (("gtk-update-icon-cache") (which "true"))
                (("update-desktop-database") (which "true"))))))))
    (native-inputs
     (list bash-completion
           cmake
           (list glib "bin")
           pkg-config
           python
           readline))
    (inputs
     (list glib
           gtk
           gtksourceview
           json-glib
           libadwaita
           liblxi
           lua))
    (home-page "https://lxi-tools.github.io/")
    (synopsis "LAN eXtensions for Instrumentation tools")
    (description
     "This package provides tools for LAN eXtensions for Instrumentation based
on the LXI Consortium standard which defines the communication protocols for
modern instrumentation and data acquision systems using Ethernet.")
    (license license:bsd-3)))

(define-public usbrelay
  (package
    (name "usbrelay")
    (version "1.2.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/darrylb123/usbrelay")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1xw2fqx4drmkvv587vkz3aicp6pw1mzxr8bjz8wad9j4c0r24cgn"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:phases #~(modify-phases %standard-phases
                   ;; No configure script.
                   (delete 'configure)
                   (add-after 'install 'install-manpage
                     (lambda _
                       (install-file "usbrelay.1"
                                     (string-append #$output "/share/man/man1"))))
                   (add-after 'install-manpage 'install-udev-rules
                     (lambda _
                       (install-file "50-usbrelay.rules"
                                     (string-append #$output "/lib/udev/rules.d")))))
      #:make-flags #~(list (string-append "CC=" #$(cc-for-target))
                           (string-append "PREFIX=" #$output)
                           (string-append "LDFLAGS=-Wl,-rpath="
                                          (string-append #$output "/lib"))
                           "LDCONFIG=true"
                           (string-append "USBMAJOR=" #$version)
                           (string-append "USBLIBVER=" #$version)
                           (string-append "VERSION=" #$version))
      ;; No test suite.
      #:tests? #f))
    (inputs (list hidapi))
    (home-page "https://github.com/darrylb123/usbrelay")
    (synopsis "Control USB relay modules")
    (description
     "This is a Linux driver based on hidapi for a variety of inexpensive
HID compatible USB relay modules available with different number of
output relays.")
    (license license:gpl2+)))

(define-public python-usbrelay
  (package
    (inherit usbrelay)
    (name "python-usbrelay")
    (build-system pyproject-build-system)
    (native-inputs
     (list python-setuptools
           python-wheel))
    (inputs
     (list usbrelay))
    (propagated-inputs
     (list python-paho-mqtt))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'install-daemon
            (lambda _
              (install-file "usbrelayd.8"
                            (string-append #$output "/share/man/man8"))
              (install-file "usbrelayd"
                            (string-append #$output "/sbin"))
              (chmod (string-append #$output "/sbin/usbrelayd") #o555)))
          (add-after 'install-daemon 'chdir
            (lambda _
              (chdir "usbrelay_py"))))))
    (synopsis "Python library to control USB relay modules")
    (description
     "This is the Python extension to @code{usbrelay}, a Linux driver based on
hidapi for a variety of inexpensive HID compatible USB relay modules.  This
package also includes @code{usbrelayd}.")))
