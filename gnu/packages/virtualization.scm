;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013-2017, 2020-2022 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015, 2016, 2017, 2018 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016-2021, 2023-2025 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016, 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017 Alex Vong <alexvong1995@gmail.com>
;;; Copyright © 2017 Andy Patterson <ajpatter@uwaterloo.ca>
;;; Copyright © 2017, 2018, 2019 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2017–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Danny Milosavljevic <dannym@scratchpost.org>
;;; Copyright © 2018 Sou Bunnbu <iyzsong@member.fsf.org>
;;; Copyright © 2018 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2019 Guy Fleury Iteriteka <hoonandon@gmail.com>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2020, 2021 Brice Waegeneire <brice@waegenei.re>
;;; Copyright © 2020 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2020, 2021, 2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2020-2025 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2020 Brett Gilio <brettg@gnu.org>
;;; Copyright © 2021 Leo Famulari <leo@famulari.name>
;;; Copyright © 2021, 2022 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2021 Dion Mendel <guix@dm9.info>
;;; Copyright © 2021 Andrew Whatson <whatson@gmail.com>
;;; Copyright © 2021 Vincent Legoll <vincent.legoll@gmail.com>
;;; Copyright © 2021 Petr Hodina <phodina@protonmail.com>
;;; Copyright © 2021 Raghav Gururajan <rg@raghavgururajan.name>
;;; Copyright © 2022, 2024 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2022, 2023 Ekaitz Zarraga <ekaitz@elenq.tech>
;;; Copyright © 2022 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2022 Zhu Zihao <all_but_last@163.com>
;;; Copyright © 2023 Juliana Sims <juli@incana.org>
;;; Copyright © 2023 Ahmad Draidi <a.r.draidi@redscript.org>
;;; Copyright © 2023 Sharlatan Hellseher <sharlatanus@gmail.com>
;;; Copyright © 2023, 2024 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2024 Nicolas Graves <ngraves@ngraves.fr>
;;; Copyright © 2024 Janneke Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2024 Raven Hallsby <karl@hallsby.com>
;;; Copyright © 2024 jgart <jgart@dismail.de>
;;; Copyright © 2024 Ashish SHUKLA <ashish.is@lostca.se>
;;; Copyright © 2024 Jakob Kirsch <jakob.kirsch@web.de>
;;; Copyright © 2024 Giacomo Leidi <goodoldpaul@autistici.org>
;;; Copyright © 2024 Artyom V. Poptsov <poptsov.artyom@gmail.com>
;;; Copyright © 2025 Karl Hallsby <karl@hallsby.com>
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

(define-module (gnu packages virtualization)
  #:use-module (gnu packages)
  #:use-module (gnu packages acl)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages apparmor)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages attr)
  #:use-module (gnu packages apparmor)
  #:use-module (gnu packages augeas)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages bootloaders)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages cdrom)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cluster)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages containers)
  #:use-module (gnu packages cpio)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages cross-base)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages cryptsetup)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages cyrus-sasl)
  #:use-module (gnu packages dbm)
  #:use-module (gnu packages debian)
  #:use-module (gnu packages disk)
  #:use-module (gnu packages dns)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages figlet)
  #:use-module (gnu packages file)
  #:use-module (gnu packages firmware)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages java)
  #:use-module (gnu packages haskell)
  #:use-module (gnu packages haskell-apps)
  #:use-module (gnu packages haskell-check)
  #:use-module (gnu packages haskell-crypto)
  #:use-module (gnu packages haskell-web)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages image)
  #:use-module (gnu packages libbsd)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages man)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages nettle)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages ninja)
  #:use-module (gnu packages ocaml)
  #:use-module (gnu packages onc-rpc)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages polkit)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages rsync)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages selinux)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages spice)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages web)
  #:use-module (gnu packages wget)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (guix build-system ruby)
  #:use-module (guix build-system trivial)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix modules)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match))

(define-public qemu
  (package
    (name "qemu")
    (version "9.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://download.qemu.org/qemu-"
                           version ".tar.xz"))
       (sha256
        (base32 "12dc3fpv6c6qvw89amjjbb6dgc2f1c1alfgn2nab7a8kxnh7f2j8"))
       (patches (search-patches "qemu-build-info-manual.patch"
                                "qemu-disable-bios-tables-test.patch"
                                "qemu-disable-migration-test.patch"
                                "qemu-fix-agent-paths.patch"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; TODO: Scrub all firmwares from this directory!
           (with-directory-excursion "pc-bios"
             ;; Delete firmwares provided by SeaBIOS.
             (for-each delete-file (find-files "." "^(bios|vgabios).*\\.bin$"))
             ;; Delete ppc64 OpenBIOS.  TODO: Remove sparc32 and sparc64 too
             ;; once they are supported in Guix.
             (delete-file "openbios-ppc")
             ;; Delete riscv64 OpenSBI.  TODO: Remove riscv32 when supported
             ;; in Guix.
             (delete-file "opensbi-riscv64-generic-fw_dynamic.bin")
             ;; Delete iPXE firmwares.
             (for-each delete-file (find-files "." "^(efi|pxe)-.*\\.rom$")))
           ;; Delete bundled code that we provide externally.
           (for-each delete-file-recursively
                     '("roms/u-boot/scripts/dtc"
                       "roms/ipxe"
                       "roms/openbios"
                       "roms/opensbi"
                       "roms/seabios"))))))
    (outputs '("out" "static" "doc"))   ;5.3 MiB of HTML docs
    (build-system gnu-build-system)
    (arguments
     (list
      ;; FIXME: Disable tests on i686 to work around
      ;; <https://bugs.gnu.org/40527>.
      #:tests? (or (%current-target-system)
                   (not (string=? "i686-linux" (%current-system))))
      #:parallel-tests? (not (or (target-arm32?)
                                 (target-riscv64?)))
      #:configure-flags
      #~(let ((gcc (search-input-file %build-inputs "/bin/gcc"))
              (openbios (search-input-file %build-inputs
                                           "share/qemu/openbios-ppc"))
              (opensbi (search-input-file
                        %build-inputs
                        "share/qemu/opensbi-riscv64-generic-fw_dynamic.bin"))
              (seabios (search-input-file %build-inputs
                                          "share/qemu/bios.bin"))
              (ipxe
               #$@(if (this-package-input "ipxe-qemu")
                      #~((search-input-file %build-inputs
                                            "share/qemu/pxe-virtio.rom"))
                      #~((string-append #$output "/share/qemu"))))
              (out #$output))
          (list (string-append "--cc=" gcc)
                ;; Some architectures insist on using HOST_CC.
                (string-append "--host-cc=" gcc)
                (string-append "--prefix=" out)
                "--sysconfdir=/etc"
                "--enable-fdt=system"
                (string-append "--firmwarepath=" out "/share/qemu:"
                               (dirname seabios) ":"
                               (dirname ipxe) ":"
                               (dirname openbios) ":"
                               (dirname opensbi))
                (string-append "--smbd=" out "/libexec/samba-wrapper")
                "--disable-debug-info"  ;for space considerations
                ;; The binaries need to be linked against -lrt.
                (string-append "--extra-ldflags=-lrt")))
      ;; Make build and test output verbose to facilitate investigation upon failure.
      #:make-flags #~'("V=1")
      #:modules `((srfi srfi-1)
                  (srfi srfi-26)
                  (ice-9 ftw)
                  (ice-9 match)
                  ,@%default-gnu-modules)
      #:phases
      #~(modify-phases %standard-phases
          ;; Since we removed the bundled firmwares above, many tests
          ;; can't work.  Re-add them here.
          (add-after 'unpack 'replace-firmwares
            (lambda* (#:key inputs #:allow-other-keys)
              (let* ((seabios (dirname (search-input-file
                                        inputs "share/qemu/bios.bin")))
                     (seabios-firmwares (find-files seabios "\\.bin$"))
                     (ipxe
                      #$@(if (this-package-input "ipxe-qemu")
                             #~((dirname (search-input-file
                                          inputs "share/qemu/pxe-virtio.rom")))
                             #~((string-append #$output "/share/qemu"))))
                     (ipxe-firmwares (find-files ipxe "\\.rom$"))
                     (openbios (search-input-file
                                inputs "share/qemu/openbios-ppc"))
                     (opensbi-riscv64
                      (search-input-file
                       inputs
                       "share/qemu/opensbi-riscv64-generic-fw_dynamic.bin"))
                     (allowed-differences
                      ;; Ignore minor differences (addresses etc) in the firmware
                      ;; data tables compared to what the test suite expects.
                      '("tests/data/acpi/pc/SSDT.dimmpxm"
                        "tests/data/acpi/pc/DSDT.dimmpxm"
                        "tests/data/acpi/pc/ERST.acpierst"
                        "tests/data/acpi/q35/ERST.acpierst"
                        "tests/data/acpi/q35/DSDT.cxl"))
                     (allowed-differences-whitelist
                      (open-file "tests/qtest/bios-tables-test-allowed-diff.h"
                                 "a")))
                (with-directory-excursion "pc-bios"
                  (for-each (lambda (file)
                              (symlink file (basename file)))
                            (append seabios-firmwares ipxe-firmwares
                                    (list openbios opensbi-riscv64))))
                (for-each (lambda (file)
                            (format allowed-differences-whitelist
                                    "\"~a\",~%" file))
                          allowed-differences)
                (close-port allowed-differences-whitelist))))
          ;; If the ipxe firmware isn't available, remove it from the list
          ;; of files expected to be available and remove some of the tests.
          #$@(if (not (this-package-input "ipxe-qemu"))
                 #~((add-after 'unpack 'dont-require-ipxe-firmware
                      (lambda _
                        (substitute* "pc-bios/meson.build"
                          ((".*(pxe|efi)-.*") ""))
                        (substitute* "tests/qtest/meson.build"
                          ((".*qom-test.*") "")
                          ((".*qos-test.*") "")
                          ((".*test-hmp.*") "")
                          ((".*'pxe-test':.*") "")
                          ((",? ?'boot-serial-test',?") "")
                          ((",? ?'endianness-test',?") "")
                          ((",? ?'prom-env-test',?") "")
                          ((",? ?'pxe-test',?") "")
                          ((",? ?'test-filter-mirror',?") "")
                          ((",? ?'test-filter-redirector',?") "")
                          ((",? ?'test-netfilter',?") "")
                          ;; Fix the slow_qtests array after the substitutions
                          (("  : .*") "")))))
                 #~())
          (add-after 'unpack 'extend-test-time-outs
            (lambda _
              ;; These tests can time out on heavily-loaded and/or slow storage.
              (substitute* (cons* "tests/qemu-iotests/common.qemu"
                                  (find-files "tests/qemu-iotests" "^[0-9]+$"))
                (("QEMU_COMM_TIMEOUT=[0-9]+" match)
                 (string-append match "9")))))
          (add-after 'unpack 'disable-unusable-tests
            (lambda _
              (substitute* "tests/unit/meson.build"
                ;; Comment out the test-qga test, which needs /sys and
                ;; fails within the build environment.
                (("tests.*test-qga.*$" all)
                 (string-append "# " all))
                ;; Comment out the test-char test, which needs networking and
                ;; fails within the build environment.
                ((".*'test-char':.*" all)
                 (string-append "# " all)))
              (substitute* "tests/qtest/meson.build"
                ;; These tests fail to get the expected number of tests
                ;; on arm platforms.
                (("'arm-cpu-features',") "")
                ;; This test is known to be flaky.
                ;; See <https://gitlab.com/qemu-project/qemu/-/issues/2121>.
                (("\\['ahci-test'\\]") "[]"))
              ;; This test appears to be flaky as well, probably resulting
              ;; from a race condition.
              (delete-file "tests/qemu-iotests/tests/copy-before-write")))
          #$@(cond
              ((target-riscv64?)
               #~((add-after 'unpack 'disable-some-tests
                    (lambda _
                      ;; Extend the test timeout for this test:
                      (substitute* "tests/unit/meson.build"
                        (("test-crypto-tlssession': 90")
                         "test-crypto-tlssession': 180"))

                      ;; qemu.qmp.QMPConnectError:
                      ;; Unexpected empty reply from server
                      (delete-file "tests/qemu-iotests/040")
                      (delete-file "tests/qemu-iotests/041")
                      (delete-file "tests/qemu-iotests/256")

                      ;; No 'PCI' bus found for device 'virtio-scsi-pci'
                      (delete-file "tests/qemu-iotests/127")
                      (delete-file "tests/qemu-iotests/267")

                      ;; This test takes too long.
                      (delete-file "tests/qemu-iotests/tests/iothreads-stream")))))
              ((target-arm32?)
               #~((add-after 'unpack 'disable-some-tests
                    (lambda _
                      ;; failed to allocate memory for stack: Cannot allocate memory
                      (substitute* "tests/qtest/meson.build"
                        ((".*qtests_aspeed :.*") ""))))))
              (else '()))
          (add-after 'patch-source-shebangs 'patch-embedded-shebangs
            (lambda* (#:key native-inputs inputs #:allow-other-keys)
              ;; Ensure the executables created by these source files reference
              ;; /bin/sh from the store so they work inside the build container.
              (substitute* '("block/cloop.c"
                             "migration/exec.c"
                             "migration/migration.c"
                             "net/tap.c"
                             "util/envlist.c")
                (("/bin/sh")
                 (search-input-file inputs "/bin/sh")))
              ;; For tests, use the native /bin/sh is available.
              (substitute* '("tests/qtest/libqtest.c"
                             "tests/qtest/vhost-user-blk-test.c")
                (("/bin/sh")
                 (search-input-file (or native-inputs inputs) "/bin/sh")))
              (substitute* "tests/qemu-iotests/testenv.py"
                (("#!/usr/bin/env python3")
                 (string-append "#!" (search-input-file (or native-inputs inputs)
                                                        "/bin/python3"))))))
          (add-before 'configure 'fix-optionrom-makefile
            (lambda _
              ;; Work around the inability of the rules defined in this
              ;; Makefile to locate the firmware files (e.g.: No rule to make
              ;; target 'multiboot.bin') by extending the VPATH.
              (substitute* "pc-bios/optionrom/Makefile"
                (("^VPATH = \\$\\(SRC_DIR\\)")
                 "VPATH = $(SRC_DIR):$(TOPSRC_DIR)/pc-bios"))))
          ;; XXX ./configure is being re-run at beginning of build phase...
          (replace 'configure
            (lambda* (#:key inputs configure-flags #:allow-other-keys)
              ;; The `configure' script doesn't understand some of the
              ;; GNU options.  Thus, add a new phase that's compatible.
              (setenv "SHELL" (which "bash"))
              ;; Ensure config.status gets the correct shebang off the bat.
              ;; The build system gets confused if we change it later and
              ;; attempts to re-run the whole configuration, and fails.
              (substitute* "configure"
                (("#!/bin/sh")
                 (string-append "#!" (which "sh"))))
              (mkdir-p "b/qemu")
              (chdir "b/qemu")
              (apply invoke "../../configure" configure-flags)))

          ;; Configure, build and install QEMU user-emulation static binaries.
          (add-after 'configure 'configure-user-static
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((gcc (search-input-file inputs "/bin/gcc"))
                     ;; This is the common set of configure flags; it is
                     ;; duplicated here to isolate this phase from manipulations
                     ;; to the #:configure-flags build argument, as done in
                     ;; derived packages such as qemu-minimal.
                     (configure-flags (list (string-append "--cc=" gcc)
                                            (string-append "--host-cc=" gcc)
                                            "--sysconfdir=/etc"
                                            "--disable-debug-info"))
                     (static (assoc-ref outputs "static")))
                (mkdir-p "../user-static")
                (with-directory-excursion "../user-static"
                  (apply invoke "../../configure"
                         "--static"
                         "--disable-docs" ;already built
                         "--disable-system"
                         "--enable-linux-user"
                         (string-append "--prefix=" static)
                         configure-flags)))))
          (add-after 'build 'build-user-static
            (lambda args
              (with-directory-excursion "../user-static"
                (apply (assoc-ref %standard-phases 'build) args))))
          (add-after 'install 'install-user-static
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((static-bin (string-append
                                 (assoc-ref outputs "static") ;see bug#70611
                                 "/bin")))
                (with-directory-excursion "../user-static"
                  (for-each
                   (cut install-file <> static-bin)
                   (find-files "."
                               (lambda (name stat)
                                 ;; Select 'qemu-' prefixed executables.
                                 (and (string-prefix? "./qemu-" name)
                                      (eq? 'regular (stat:type stat))
                                      (logtest #o100 (stat:perms stat))))))))))
          (add-before 'check 'set-SOCK_DIR
            (lambda _
              ;; The default value for SOCK_DIR is TMPDIR, which can be long
              ;; in the build chroot (e.g.:
              ;; /tmp/guix-build-qemu-minimal-drv-0); set it to SOCK_DIR to
              ;; avoid using more than 109 characters for socket files (the
              ;; limit when using the kernel Linux).
              (setenv "SOCK_DIR" "/tmp")))
          (add-after 'install 'delete-firmwares
            (lambda _
              ;; Delete firmares that are accessible on --firmwarepath.
              ;; For some reason tests fail if we simply remove them from
              ;; pc-bios/meson.build, hence this roundabout way.
              (with-directory-excursion (string-append #$output "/share/qemu")
                (for-each delete-file
                          (append
                           '("openbios-ppc"
                             "opensbi-riscv64-generic-fw_dynamic.bin")
                           (find-files "." "^(vga)?bios(-[a-z0-9-]+)?\\.bin$")
                           (find-files "." "^(efi|pxe)-.*\\.rom$"))))))
          ;; Create a wrapper for Samba. This allows QEMU to use Samba without
          ;; pulling it in as an input. Note that you need to explicitly install
          ;; Samba in your Guix profile for Samba support.
          (add-after 'install 'create-samba-wrapper
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((libexec (string-append #$output "/libexec")))
                (call-with-output-file "samba-wrapper"
                  (lambda (port)
                    (format port "#!/bin/sh
exec smbd $@")))
                (chmod "samba-wrapper" #o755)
                (install-file "samba-wrapper" libexec))))
          (add-after 'install 'move-html-doc
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((out #$output)
                     (doc (assoc-ref outputs "doc")) ;see bug#70611
                     (qemu-doc (string-append doc "/share/doc/qemu-"
                                              #$(package-version this-package))))
                (mkdir-p qemu-doc)
                (rename-file (string-append out "/share/doc/qemu")
                             (string-append qemu-doc "/html"))))))))
    (inputs
     (append
      (if (supported-package? ipxe-qemu)
          (list ipxe-qemu)
          '())
      (list alsa-lib
            bash-minimal
            dtc
            glib
            gnutls                      ;for qcow2 disk encryption
            gtk+
            libaio
            libcacard                   ;smartcard support
            attr libcap-ng              ;VirtFS support
            libdrm
            libepoxy
            libjpeg-turbo
            libpng
            libseccomp
            libslirp
            liburing
            libusb                      ;USB pass-through support
            mesa
            ncurses
            openbios-qemu-ppc
            opensbi-qemu
            pixman
            pulseaudio
            sdl2
            seabios-qemu
            spice
            usbredir
            util-linux
            vde2
            virglrenderer
            zlib
            `(,zstd "lib"))))
    (native-inputs
     ;; Note: acpica is here only to pretty-print firmware differences with IASL
     ;; (see the replace-firmwares phase above).
     (list acpica
           bison
           flex
           gettext-minimal
           `(,glib "bin")               ;gtester, etc.
           meson
           ninja
           perl
           pkg-config
           python-wrapper
           python-sphinx
           python-sphinx-rtd-theme
           python-tomli
           texinfo
           ;; The following static libraries are required to build
           ;; the static output of QEMU.
           `(,glib "static")
           `(,pcre2 "static")
           `(,zlib "static")))
    (home-page "https://www.qemu.org")
    (synopsis "Machine emulator and virtualizer")
    (description
     "QEMU is a generic machine emulator and virtualizer.

When used as a machine emulator, QEMU can run OSes and programs made for one
machine (e.g. an ARM board) on a different machine---e.g., your own PC.  By
using dynamic translation, it achieves very good performance.

When used as a virtualizer, QEMU achieves near native performances by
executing the guest code directly on the host CPU.  QEMU supports
virtualization when executing under the Xen hypervisor or using
the KVM kernel module in Linux.  When using KVM, QEMU can virtualize x86,
server and embedded PowerPC, and S390 guests.")

    ;; Many files are GPLv2+, but some are GPLv2-only---e.g., `memory.c'.
    (license license:gpl2)

    ;; Several tests fail on MIPS; see <http://hydra.gnu.org/build/117914>.
    (supported-systems (fold delete %supported-systems
                             '("mips64el-linux" "i586-gnu" "x86_64-gnu")))))

(define-public qemu-minimal
  ;; QEMU without GUI support, only supporting the host's architecture
  (package/inherit qemu
    (name "qemu-minimal")
    (outputs '("out" "doc"))
    (synopsis
     "Machine emulator and virtualizer (without GUI or docs) for the host architecture")
    (arguments
     (substitute-keyword-arguments (package-arguments qemu)
       ((#:configure-flags configure-flags #~'())
        ;; Restrict to the host's architecture.
        (let* ((system (or (%current-target-system)
                           (%current-system)))
               (target-list-arg
                (match system
                  ((? (cut string-prefix? "i686" <>))
                   "--target-list=i386-softmmu")
                  ((? (cut string-prefix? "x86_64" <>))
                   "--target-list=i386-softmmu,x86_64-softmmu")
                  ((? (cut string-prefix? "mips64" <>))
                   (string-append "--target-list=mips-softmmu,mipsel-softmmu,"
                                  "mips64-softmmu,mips64el-softmmu"))
                  ((? (cut string-prefix? "mips" <>))
                   "--target-list=mips-softmmu,mipsel-softmmu")
                  ((? (cut string-prefix? "aarch64" <>))
                   "--target-list=arm-softmmu,aarch64-softmmu")
                  ((? (cut string-prefix? "arm" <>))
                   "--target-list=arm-softmmu")
                  ((? (cut string-prefix? "alpha" <>))
                   "--target-list=alpha-softmmu")
                  ((? (cut string-prefix? "powerpc64" <>))
                   "--target-list=ppc-softmmu,ppc64-softmmu")
                  ((? (cut string-prefix? "powerpc" <>))
                   "--target-list=ppc-softmmu")
                  ((? (cut string-prefix? "s390" <>))
                   "--target-list=s390x-softmmu")
                  ((? (cut string-prefix? "riscv" <>))
                   "--target-list=riscv32-softmmu,riscv64-softmmu")
                  (else       ; An empty list actually builds all the targets.
                   '()))))
          #~(cons #$target-list-arg #$configure-flags)))
       ((#:phases phases #~'%standard-phases)
        #~(modify-phases #$phases
            (delete 'configure-user-static)
            (delete 'build-user-static)
            (delete 'install-user-static)
            ;; We cannot fully remove the "doc" output due to the gexp in qemu.
            (replace 'move-html-doc
              (lambda _
                (mkdir-p #$output:doc)))))))

    ;; Remove dependencies on optional libraries, notably GUI libraries.
    (native-inputs (filter (lambda (input)
                             (match input
                               ;; Work around the fact that modify-inputs can not
                               ;; delete specific outputs; i.e. here we should keep
                               ;; `(,glib "bin"), but not `(,glib "static").
                               ((label package output)
                                (not (string=? "static" output)))
                               (_ input)))
                           (modify-inputs (package-native-inputs qemu)
                             (delete "gettext-minimal"
                                     "python-sphinx"
                                     "python-sphinx-rtd-theme"))))
    (inputs (modify-inputs (package-inputs qemu)
              (delete "libusb"
                      "mesa"
                      "sdl2"
                      "spice"
                      "virglrenderer"
                      "gtk+"
                      "usbredir"
                      "libdrm"
                      "libepoxy"
                      "pulseaudio"
                      "vde2"
                      "libcacard")))))

(define (system->qemu-target system)
  (cond
   ((string-prefix? "i686" system)
    "qemu-system-i386")
   ((string-prefix? "arm" system)
    "qemu-system-arm")
   (else
    (string-append "qemu-system-" (match (string-split system #\-)
                                    ((arch kernel) arch)
                                    (_ system))))))

(define-public libx86emu
  (package
    (name "libx86emu")
    (version "3.5")
    (home-page "https://github.com/wfeldt/libx86emu")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32 "11nj3y7maz9ch15b1c2b69gd8d7mpaha377zpdbvfsmg5w9zz93l"))
              (modules '((guix build utils)))
              (snippet `(begin
                          ;; Remove git2log program file.
                          (delete-file "git2log")
                          ;; Remove variables that depends on git2log.
                          (substitute* "Makefile"
                            (("GIT2LOG.*=.*$") "")
                            (("GITDEPS.*=.*$") "")
                            (("BRANCH.*=.*$") ""))))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"
       ;; sys/io.h is not present from glibc on non-x86 systems.
       #:tests? ,(and (target-x86?)
                      (not (%current-target-system)))
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'patch
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (include (string-append out "/include"))
                             (lib (string-append out "/lib")))
                        ;; Correct the values of version and install directories.
                        (substitute* "Makefile"
                          (("VERSION.*=.*$")
                           (string-append "VERSION := "
                                          ,version "\n"))
                          (("PREFIX.*=.*$")
                           (string-append "PREFIX := " out "\n"))
                          (("MAJOR_VERSION.*=.*$")
                           (string-append "MAJOR_VERSION := "
                                          ,(version-major version) "\n"))
                          (("LIBDIR.*=.*$")
                           (string-append "LIBDIR = " lib "\n"))
                          (("/usr/include")
                           include)))))
                  ;; XXX: "make -C test" fails because it cannot find "x86test"
                  ;; executable due to parallel tests.  So we need to make sure that
                  ;; this file is built before running the tests.
                  (add-before 'check 'make-x86test
                    (lambda _
                      (invoke "make" "-C" "test" "x86test")))
                  (delete 'configure)))) ;no configure script
    (native-inputs (list nasm perl))
    (synopsis "Library for x86 emulation")
    (description
     "Libx86emu is a small library to emulate x86 instructions.  The
focus here is not a complete emulation but to cover enough for typical
firmware blobs.  You can
@enumerate
@item intercept any memory access or directly map real memory ranges
@item intercept any i/o access, map real i/o ports, or block any real i/o
@item intercept any interrupt
@item add a hook to run after each instruction
@item recognize a special x86 instruction that can trigger logging
@item use integrated logging
@end enumerate")
    (license (license:x11-style "file://LICENSE"))))

(define-public ganeti
  (package
    (name "ganeti")
    (version "3.0.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ganeti/ganeti")
                    (commit (string-append "v" version))))
              (sha256
               (base32 "1xw7rm0k411aj0a4hrxz9drn7827bihp6bwizbapfx8k4c3125k4"))
              (file-name (git-file-name name version))
              (patches (search-patches "ganeti-shepherd-support.patch"
                                       "ganeti-shepherd-master-failover.patch"
                                       "ganeti-haskell-pythondir.patch"
                                       "ganeti-pyyaml-compat.patch"
                                       "ganeti-procps-compat.patch"
                                       "ganeti-disable-version-symlinks.patch"
                                       "ganeti-lens-compat.patch"
                                       "ganeti-openssh-test-fix.patch"
                                       "ganeti-template-haskell-2.17.patch"
                                       "ganeti-template-haskell-2.18.patch"
                                       "ganeti-reorder-arbitrary-definitions.patch"
                                       "ganeti-relax-dependencies.patch"
                                       "ganeti-sphinx-import.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:imported-modules (,@%default-gnu-imported-modules
                           (guix build haskell-build-system)
                           (guix build python-build-system))
       #:modules (,@%default-gnu-modules
                  ((guix build haskell-build-system) #:prefix haskell:)
                  ((guix build python-build-system) #:select (site-packages))
                  (srfi srfi-1)
                  (srfi srfi-26)
                  (ice-9 match)
                  (ice-9 rdelim))

       ;; The default test target includes a lot of checks that are only really
       ;; relevant for developers such as NEWS file checking, line lengths, etc.
       ;; We are only interested in the "py-tests" and "hs-tests" targets: this
       ;; is the closest we've got even though it includes a little more.
       #:test-target "check-TESTS"

       #:configure-flags
       (list "--localstatedir=/var"
             "--sharedstatedir=/var"
             "--sysconfdir=/etc"
             "--enable-haskell-tests"

             ;; By default, the build system installs everything to versioned
             ;; directories such as $libdir/3.0 and relies on a $libdir/default
             ;; symlink pointed from /etc/ganeti/{lib,share} to actually function.
             ;; This is done to accommodate installing multiple versions in
             ;; parallel, but is of little use to us as Guix users can just
             ;; roll back and forth.  Thus, disable it for simplicity.
             "--disable-version-links"

             ;; Ganeti can optionally take control over SSH host keys and
             ;; distribute them to nodes as they are added, and also rotate keys
             ;; with 'gnt-cluster renew-crypto --new-ssh-keys'.  Thus it needs to
             ;; know how to restart the SSH daemon.
             "--with-sshd-restart-command='herd restart ssh-daemon'"

             ;; Look for OS definitions in this directory by default.  It can
             ;; be changed in the cluster configuration.
             "--with-os-search-path=/run/current-system/profile/share/ganeti/os"

             ;; The default QEMU executable to use.  We don't use the package
             ;; here because this entry is stored in the cluster configuration.
             (string-append "--with-kvm-path=/run/current-system/profile/bin/"
                            ,(system->qemu-target (%current-system))))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'create-vcs-version
           (lambda _
             ;; If we are building from a git checkout, we need to create a
             ;; 'vcs-version' file manually because the build system does
             ;; not have access to the git repository information.
             (unless (file-exists? "vcs-version")
               (call-with-output-file "vcs-version"
                 (lambda (port)
                   (format port "v~a~%" ,version))))))
         (add-after 'unpack 'patch-absolute-file-names
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* '("lib/utils/process.py"
                            "lib/utils/text.py"
                            "src/Ganeti/Constants.hs"
                            "src/Ganeti/HTools/CLI.hs"
                            "test/py/ganeti.config_unittest.py"
                            "test/py/ganeti.hooks_unittest.py"
                            "test/py/ganeti.utils.process_unittest.py"
                            "test/py/ganeti.utils.text_unittest.py"
                            "test/py/ganeti.utils.wrapper_unittest.py")
               (("/bin/sh") (search-input-file inputs "/bin/sh"))
               (("/bin/bash") (search-input-file inputs "/bin/bash"))
               (("/usr/bin/env") (search-input-file inputs "/bin/env"))
               (("/bin/true") (search-input-file inputs "/bin/true")))

             ;; This script is called by the node daemon at startup to perform
             ;; sanity checks on the cluster IP addresses, and it is also used
             ;; in a master-failover scenario.  Add absolute references to
             ;; avoid propagating these executables.
             (substitute* "tools/master-ip-setup"
               (("arping") (search-input-file inputs "/bin/arping"))
               (("ndisc6") (search-input-file inputs "/bin/ndisc6"))
               (("fping") (search-input-file inputs "/sbin/fping"))
               (("grep") (search-input-file inputs "/bin/grep"))
               (("ip addr") (string-append (search-input-file inputs "/sbin/ip")
                                           " addr")))))
         (add-after 'unpack 'override-builtin-PATH
           (lambda _
             ;; Ganeti runs OS install scripts and similar with a built-in
             ;; hard coded PATH.  Patch so it works on Guix System.
             (substitute* "src/Ganeti/Constants.hs"
               (("/sbin:/bin:/usr/sbin:/usr/bin")
                "/run/privileged/bin:/run/current-system/profile/sbin:\
/run/current-system/profile/bin"))))
         (add-after 'bootstrap 'patch-sphinx-version-detection
           (lambda _
             ;; The build system runs 'sphinx-build --version' to verify that
             ;; the Sphinx is recent enough, but does not expect the
             ;; .sphinx-build-real executable name created by the Sphinx wrapper.
             (substitute* "configure"
               (("\\$SPHINX --version 2>&1")
                "$SPHINX --version 2>&1 \
| sed 's/.sphinx-build-real/sphinx-build/g'"))))

         ;; The build system invokes Cabal and GHC, which do not work with
         ;; GHC_PACKAGE_PATH: <https://github.com/haskell/cabal/issues/3728>.
         ;; Tweak the build system to do roughly what haskell-build-system does.
         (add-before 'configure 'configure-haskell
           (assoc-ref haskell:%standard-phases 'setup-compiler))
         (add-after 'configure 'do-not-use-GHC_PACKAGE_PATH
           (lambda _
             (unsetenv "GHC_PACKAGE_PATH")
             (substitute* "Makefile"
               (("\\$\\(CABAL\\)")
                "$(CABAL) --package-db=../package.conf.d")
               (("\\$\\(GHC\\)")
                "$(GHC) -package-db=../package.conf.d"))))
         (add-after 'configure 'make-ghc-use-shared-libraries
           (lambda _
             (substitute* "Makefile"
               (("HFLAGS =") "HFLAGS = -dynamic -fPIC"))))
         (add-after 'configure 'fix-installation-directories
           (lambda _
             (substitute* "Makefile"
               ;; Do not attempt to create /var during install.
               (("\\$\\(DESTDIR\\)\\$\\{localstatedir\\}")
                "$(DESTDIR)${prefix}${localstatedir}")
               ;; Similarly, do not attempt to install the sample ifup scripts
               ;; to /etc/ganeti.
               (("\\$\\(DESTDIR\\)\\$\\(ifupdir\\)")
                "$(DESTDIR)${prefix}$(ifupdir)"))))
         (add-before 'build 'adjust-tests
           (lambda _
             ;; Disable tests that can not run.  Do it early to prevent
             ;; touching the Makefile later and triggering a needless rebuild.
             (substitute* "Makefile"
               ;; These tests expect the presence of a 'root' user (via
               ;; ganeti/runtime.py), which fails in the build environment.
               (("test/py/ganeti\\.asyncnotifier_unittest\\.py") "")
               (("test/py/ganeti\\.backend_unittest\\.py") "")
               (("test/py/ganeti\\.daemon_unittest\\.py") "")
               (("test/py/ganeti\\.hypervisor\\.hv_kvm_unittest\\.py") "")
               (("test/py/ganeti\\.tools\\.ensure_dirs_unittest\\.py") "")
               (("test/py/ganeti\\.utils\\.io_unittest-runasroot\\.py") "")
               ;; Tracked at: https://github.com/ganeti/ganeti/issues/1752
               (("test/py/ganeti\\.ssh_unittest\\.py") "")
               ;; Disable the bash_completion test, as it requires the full
               ;; bash instead of bash-minimal.
               (("test/py/bash_completion\\.bash")
                "")
               ;; This test requires networking.
               (("test/py/import-export_unittest\\.bash")
                ""))
             (substitute* "test/hs/Test/Ganeti/OpCodes.hs"
               ;; Some serdes failure, tracked at:
               ;; https://github.com/ganeti/ganeti/issues/1753
               ((", 'case_py_compat_types") ""))))
         (add-after 'build 'build-bash-completions
           (lambda _
             (setenv "PYTHONPATH" ".")
             (invoke "./autotools/build-bash-completion")
             (unsetenv "PYTHONPATH")))
         (add-before 'check 'pre-check
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Set TZDIR so that time zones are found.
             (setenv "TZDIR" (search-input-directory inputs "share/zoneinfo"))

             (substitute* "test/py/ganeti.utils.process_unittest.py"
               ;; This test attempts to run an executable with
               ;; RunCmd(..., reset_env=True), which fails because the default
               ;; PATH from Constants.hs does not exist in the build container.
               ((".*def testResetEnv.*" all)
                (string-append "  @unittest.skipIf(True, "
                               "\"cannot reset env in the build container\")\n"
                               all))

               ;; XXX: Somehow this test fails in the build container, but
               ;; works in 'guix environment -C', even without /bin/sh?
               ((".*def testPidFile.*" all)
                (string-append "  @unittest.skipIf(True, "
                               "\"testPidFile fails in the build container\")\n"
                               all)))

             ;; XXX: Why are these links not added automatically.
             (with-directory-excursion "test/hs"
               (for-each (lambda (file)
                           (symlink "../../src/htools" file))
                         '("hspace" "hscan" "hinfo" "hbal" "hroller"
                           "hcheck" "hail" "hsqueeze")))))
         (add-after 'install 'install-bash-completions
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (compdir (string-append out "/etc/bash_completion.d")))
               (mkdir-p compdir)
               (copy-file "doc/examples/bash_completion"
                          (string-append compdir "/ganeti"))
               ;; The one file contains completions for many different
               ;; executables.  Create symlinks for found completions.
               (with-directory-excursion compdir
                 (for-each
                  (lambda (prog) (symlink "ganeti" prog))
                  (call-with-input-file "ganeti"
                    (lambda (port)
                      (let loop ((line (read-line port))
                                 (progs '()))
                        (if (eof-object? line)
                            progs
                            (if (string-prefix? "complete" line)
                                (loop (read-line port)
                                      ;; Extract "prog" from lines of the form:
                                      ;; "complete -F _prog -o filenames prog".
                                      ;; Note that 'burnin' is listed with the
                                      ;; absolute file name, which is why we
                                      ;; run everything through 'basename'.
                                      (match (string-split line #\ )
                                        ((commands ... prog)
                                         (cons (basename prog) progs))))
                                (loop (read-line port) progs)))))))))))
         ;; Wrap all executables with GUIX_PYTHONPATH.  We can't borrow
         ;; the phase from python-build-system because we also need to wrap
         ;; the scripts in $out/lib/ganeti such as "node-daemon-setup".
         (add-after 'install 'wrap
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (sbin (string-append out "/sbin"))
                    (lib (string-append out "/lib"))
                    (PYTHONPATH (string-append (site-packages inputs outputs)
                                               ":" (getenv "GUIX_PYTHONPATH"))))
               (define (shell-script? file)
                 (call-with-ascii-input-file file
                   (lambda (port)
                     (let ((shebang (false-if-exception (read-line port))))
                       (and shebang
                            (string-prefix? "#!" shebang)
                            (or (string-contains shebang "/bin/bash")
                                (string-contains shebang "/bin/sh")))))))

               (define* (wrap? file #:rest _)
                 ;; Do not wrap shell scripts because some are meant to be
                 ;; sourced, which breaks if they are wrapped.  We do wrap
                 ;; the Haskell executables because some call out to Python
                 ;; directly.
                 (and (executable-file? file)
                      (not (symbolic-link? file))
                      (not (shell-script? file))))

               (for-each (lambda (file)
                           (wrap-program file
                             `("GUIX_PYTHONPATH" ":" prefix
                               (,PYTHONPATH))))
                         (append-map (cut find-files <> wrap?)
                                     (list (string-append lib "/ganeti")
                                           sbin)))))))))
    (native-inputs
     `(("haskell" ,ghc)
       ("cabal" ,cabal-install)
       ("m4" ,m4)

       ;; These inputs are necessary to bootstrap the package, because we
       ;; have patched the build system.
       ("autoconf" ,autoconf)
       ("automake" ,automake)

       ;; For the documentation.
       ("python-docutils" ,python-docutils)
       ("sphinx" ,python-sphinx)
       ("pandoc" ,pandoc)
       ("dot" ,graphviz)

       ;; Test dependencies.
       ("fakeroot" ,fakeroot)
       ("ghc-temporary" ,ghc-temporary)
       ("ghc-test-framework" ,ghc-test-framework)
       ("ghc-test-framework-hunit" ,ghc-test-framework-hunit)
       ("ghc-test-framework-quickcheck2" ,ghc-test-framework-quickcheck2)
       ("python-mock" ,python-mock)
       ("python-pyyaml" ,python-pyyaml)
       ("openssh" ,openssh)
       ("procps" ,procps)
       ("shelltestrunner" ,shelltestrunner)
       ("tzdata" ,tzdata-for-tests)))
    (inputs
     (list bash-minimal
           iputils                      ;for 'arping'
           curl
           fping
           iproute
           ndisc6
           socat
           qemu-minimal                 ;for qemu-img
           ghc-attoparsec
           ghc-base64-bytestring
           ghc-cryptonite
           ghc-curl
           ghc-hinotify
           ghc-hslogger
           ghc-json
           ghc-lens
           ghc-lifted-base
           ghc-network
           ghc-old-time
           ghc-psqueue
           ghc-regex-pcre
           ghc-utf8-string
           ghc-zlib
           ;; For the optional metadata daemon.
           ghc-snap-core
           ghc-snap-server
           python
           python-pyopenssl
           python-simplejson
           python-pyparsing
           python-pyinotify
           python-pycurl
           python-bitarray
           python-paramiko
           python-psutil))
    (home-page "https://www.ganeti.org/")
    (synopsis "Cluster-based virtual machine management system")
    (description
     "Ganeti is a virtual machine management tool built on top of existing
virtualization technologies such as Xen or KVM.  Ganeti controls:

@itemize @bullet
@item Disk creation management;
@item Operating system installation for instances (in co-operation with
OS-specific install scripts); and
@item Startup, shutdown, and failover between physical systems.
@end itemize

Ganeti is designed to facilitate cluster management of virtual servers and
to provide fast and simple recovery after physical failures, using
commodity hardware.")
    (license license:bsd-2)))

(define-public ganeti-instance-guix
  (package
    (name "ganeti-instance-guix")
    (version "0.8")
    (home-page "https://github.com/mbakke/ganeti-instance-guix")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page) (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0sw9ks3j3y33apdcghjxxjf09ld592z9skaa7bgn9d2lhplzjihr"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--sysconfdir=/etc" "--localstatedir=/var")))
    (native-inputs
     (list autoconf automake jq))
    (inputs
     (list btrfs-progs
           cryptsetup
           e2fsprogs
           f2fs-tools
           lvm2
           multipath-tools
           util-linux
           parted
           xfsprogs))
    (synopsis "Guix OS integration for Ganeti")
    (description
     "This package provides a guest OS definition for Ganeti that uses
Guix to build virtual machines.")
    (license license:gpl3+)))

(define-public ganeti-instance-debootstrap
  (package
    (name "ganeti-instance-debootstrap")
    ;; We need two commits on top of the latest release for compatibility
    ;; with newer sfdisk, as well as gnt-network integration.
    (version "0.16-2-ge145396")
    (home-page "https://github.com/ganeti/instance-debootstrap")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page) (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0f2isw9d8lawzj21rrq1q9xhq8xfa65rqbhqmrn59z201x9q1336"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--sysconfdir=/etc" "--localstatedir=/var")
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'add-absolute-references
                    (lambda _
                      (substitute* "common.sh.in"
                        (("/sbin/blkid") (which "blkid"))
                        (("kpartx -")
                         (string-append (which "kpartx") " -")))
                      (substitute* "import"
                        (("restore -r")
                         (string-append (which "restore") " -r")))
                      (substitute* "export"
                        (("dump -0")
                         (string-append (which "dump") " -0")))
                      (substitute* "create"
                        (("debootstrap") (which "debootstrap"))
                        (("`which run-parts`") (which "run-parts"))
                        ;; Here we actually need to hard code /bin/passwd
                        ;; because it's called via chroot, which fails if
                        ;; "/bin" is not in PATH.
                        (("passwd") "/bin/passwd"))
                      #t))
                  (add-after 'unpack 'set-dpkg-arch
                    (lambda* (#:key system #:allow-other-keys)
                      ;; The create script passes --arch to debootstrap,
                      ;; and defaults to `dpkg --print-architecture` when
                      ;; ARCH is not set in variant.conf.  Hard code the
                      ;; build-time architecture to avoid the dpkg dependency.
                      (let ((dpkg-arch
                             (cond ((string-prefix? "x86_64" system)
                                    "amd64")
                                   ((string-prefix? "i686" system)
                                    "i386")
                                   ((string-prefix? "aarch64" system)
                                    "arm64")
                                   (else (car (string-split system #\-))))))
                        (substitute* "create"
                          (("`dpkg --print-architecture`")
                           dpkg-arch))
                        #t)))
                  (add-after 'configure 'adjust-Makefile
                    (lambda _
                      ;; Do not attempt to create /etc/ganeti/instance-debootstrap
                      ;; and /etc/default/ganeti-instance-debootstrap during install.
                      ;; They are created by the Ganeti service.
                      (substitute* "Makefile"
                        (("\\$\\(variantsdir\\)")
                         "$(prefix)/etc/ganeti/instance-debootstrap/variants")
                        (("\\$\\(defaultsdir\\)")
                         "$(prefix)/etc/default/ganeti-instance-debootstrap"))
                      #t))
                  (add-after 'install 'make-variants.list-symlink
                    (lambda* (#:key outputs #:allow-other-keys)
                      ;; The Ganeti OS API mandates a variants.list file that
                      ;; describes all supported "variants" of this OS.
                      ;; Guix generates this file, so make the original file
                      ;; a symlink to it.
                      (with-directory-excursion (string-append
                                                 (assoc-ref outputs "out")
                                                 "/share/ganeti/os/debootstrap")
                        (delete-file "variants.list")
                        (symlink "/etc/ganeti/instance-debootstrap/variants/variants.list"
                                 "variants.list"))
                      #t)))))
    (native-inputs
     (list autoconf automake))
    (inputs
     `(("debianutils" ,debianutils)
       ("debootstrap" ,debootstrap)
       ("dump" ,dump)
       ("kpartx" ,multipath-tools)
       ("util-linux" ,util-linux)))
    (synopsis "Debian OS integration for Ganeti")
    (description
     "This package provides a guest OS definition for Ganeti.  It installs
Debian or a derivative using @command{debootstrap}.")
    (license license:gpl2+)))

(define-public rvvm
  (package
    (name "rvvm")
    (version "0.6")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/LekKit/RVVM")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1mbb1c9r33am2z27a17xkjw5gpzvz41wywx1g021s0w34lmsax76"))))
    (build-system cmake-build-system)
    (arguments
     (list
       #:configure-flags
       ;; See src/rvjit/rvjit.h for list of architectures.
       #~(#$@(if (or (target-x86?)
                     (target-arm?))
               #~'()
               #~(list "-DRVVM_USE_JIT=NO")))
       #:modules `((srfi srfi-26)
                  (guix build utils)
                  (guix build cmake-build-system))
       #:phases
       #~(modify-phases %standard-phases
           ;; Install phase inspired by the Makefile.
           (replace 'install
             (lambda _
               (let ((src "../source/src/")
                     (incl (string-append #$output "/include/rvvm/")))
                 (install-file "rvvm" (string-append #$output "/bin"))
                 (for-each
                   (cut install-file <> (string-append #$output "/lib"))
                   (find-files "." "\\.(so|a)$"))
                 (install-file (string-append src "rvvmlib.h") incl)
                 (for-each
                   (cut install-file <> (string-append incl "devices"))
                   (find-files (string-append src "devices") "\\.h$"))))))
       #:tests? #f))    ; no tests
    (home-page "https://github.com/LekKit/RVVM")
    (synopsis "RISC-V virtual machine")
    (description
     "RVVM is a RISC-V CPU and system software implementation written in C.  It
supports the entire RV64GC ISA, and it passes compliance tests for both RV64 and
RV32.  OpenSBI, U-Boot, and custom firmwares boot and execute properly.  It is
capable of running Linux, FreeBSD, OpenBSD, Haiku, and other OSes.  Furthermore,
it emulates a variety of hardware and peripherals.")
    (license (list license:gpl3+ license:mpl2.0))))

(define-public spike
  (package
    (name "spike")
    (version "1.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/riscv-software-src/riscv-isa-sim")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32 "0cik2m0byfp9ppq0hpg3xyrlp5ag1i4dww7a7872mlm36xxqagg0"))))
    (build-system gnu-build-system)
    (arguments
     (list
       #:phases
       #~(modify-phases %standard-phases
           (add-before 'configure 'configure-dtc-path
             (lambda* (#:key inputs #:allow-other-keys)
               ;; Reference dtc by its absolute store path.
               (substitute* "riscv/dts.cc"
                 (("DTC")
                  (string-append "\"" (search-input-file inputs "/bin/dtc") "\""))))))))
    (inputs
     (list bash-minimal dtc))
    (native-inputs
     (list python-wrapper))
    (home-page "https://github.com/riscv-software-src/riscv-isa-sim")
    (synopsis "RISC-V ISA Simulator")
    (description "Spike, the RISC-V ISA Simulator, implements a functional model
of one or more RISC-V harts.")
    (license license:bsd-3)))

(define-public libosinfo
  (package
    (name "libosinfo")
    (version "1.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://releases.pagure.org/libosinfo/libosinfo-"
                           version ".tar.xz"))
       (sha256
        (base32
         "1dn6pzv0gzkxrjvi60cdzdmyxqlcsvinbrbds91xm4v7wbn5g1dd"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list (string-append "-Dwith-usb-ids-path="
                             (search-input-file %build-inputs
                                                "share/hwdata/usb.ids"))
              (string-append "-Dwith-pci-ids-path="
                             (search-input-file %build-inputs
                                                "share/hwdata/pci.ids")))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-osinfo-path
            (lambda* (#:key native-inputs inputs #:allow-other-keys)
              (substitute* "osinfo/osinfo_loader.c"
                (("path = DATA_DIR.*")
                 (format #f "path = ~s;"
                         (search-input-directory (or native-inputs inputs)
                                                 "share/osinfo")))))))))
    (inputs (list libsoup libxml2 libxslt osinfo-db))
    (native-inputs
     (list `(,glib "bin")                ;glib-mkenums, etc.
           gobject-introspection
           gtk-doc/stable
           hwdata
           vala
           intltool
           pkg-config))
    (home-page "https://libosinfo.org/")
    (synopsis "Operating system information database")
    (description "libosinfo is a GObject based library API for managing
information about operating systems, hypervisors and the (virtual) hardware
devices they can support.  It includes a database containing device metadata
and provides APIs to match/identify optimal devices for deploying an operating
system on a hypervisor.  Via GObject Introspection, the API is available in
all common programming languages.  Vala bindings are also provided.")
    ;; The library files are released under LGPLv2.1 or later; the source
    ;; files in the "tools" directory are released under GPLv2+.
    (license (list license:lgpl2.1+ license:gpl2+))))

(define-public lxc
  (package
    (name "lxc")
    (version "6.0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://linuxcontainers.org/downloads/lxc/lxc-"
                    version ".tar.gz"))
              (sha256
               (base32
                "1q3p3zzm338pmc97z6ly8cjginkyljxqbk1c37l2xa46vfy8zcyc"))
              (patches (search-patches "lxc-no-static-bin.patch"))))
    (build-system meson-build-system)
    (native-inputs
     (list pkg-config docbook2x))
    (inputs
     (list apparmor dbus gnutls libcap libseccomp libselinux))
    (arguments
     (list #:configure-flags
           #~(list (string-append "-Ddoc-path=" #$output "/share/doc/"
                                  #$name "-" #$version)
                   "-Ddistrosysconfdir=/etc"
                   "-Dinit-script=sysvinit"
                   "-Dinstall-state-dirs=false"
                   "-Dinstall-init-files=false"
                   "-Dspecfile=false"
                   "-Db_lto=false")))
    (synopsis "Linux container tools")
    (home-page "https://linuxcontainers.org/")
    (description
     "LXC is a userspace interface for the Linux kernel containment features.
Through a powerful API and simple tools, it lets Linux users easily create and
manage system or application containers.")
    (license license:lgpl2.1+)))

(define-public lxcfs
  (package
    (name "lxcfs")
    (version "5.0.4")
    (home-page "https://github.com/lxc/lxcfs")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference (url home-page)
                           (commit (string-append "lxcfs-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "15cc7kvnln4qqlv22hprfzmq89jbkx7yra730hap8wkvamn33sxy"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list "-Dinit-script=sysvinit"))) ; no ‘none’ option
    (native-inputs
     (list help2man pkg-config python python-jinja2))
    (inputs
     (list fuse))
    (synopsis "FUSE-based file system for LXC")
    (description "LXCFS is a small FUSE file system written with the intention
of making Linux containers feel more like a virtual machine.
It started as a side project of LXC but can be used by any run-time.")
    (license license:lgpl2.1+)))

(define-public lxd
  (package
    (name "lxd")
    (version "4.24")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/lxc/lxd/releases/download/"
                    "lxd-" version "/lxd-" version ".tar.gz"))
              (sha256
               (base32
                "0lmjmvm98m6yjxcqlfw690i71nazfzgrm3mzbjj77g1631df3ylp"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/lxc/lxd"
       #:tests? #f ;; tests fail due to missing /var, cgroups, etc.
       #:modules ((guix build go-build-system)
                  (guix build union)
                  (guix build utils)
                  (srfi srfi-1))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'unpack-dist
           (lambda* (#:key import-path #:allow-other-keys)
             (with-directory-excursion (string-append "src/" import-path)
               ;; Move all the dependencies into the src directory.
               (copy-recursively "_dist/src" "../../.."))))
         (replace 'build
           (lambda* (#:key import-path #:allow-other-keys)
             (with-directory-excursion (string-append "src/" import-path)
               (invoke "make" "build" "CC=gcc" "TAG_SQLITE3=libsqlite3"))))
         (replace 'check
           (lambda* (#:key tests? import-path #:allow-other-keys)
             (when tests?
               (with-directory-excursion (string-append "src/" import-path)
                 (invoke "make" "check" "CC=gcc" "TAG_SQLITE3=libsqlite3")))))
         (replace 'install
           (lambda* (#:key inputs outputs import-path #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin-dir
                     (string-append out "/bin/"))
                    (doc-dir
                     (string-append out "/share/doc/lxd-" ,version))
                    (completions-dir
                     (string-append out "/share/bash-completion/completions")))
               (with-directory-excursion (string-append "src/" import-path)
                 ;; Wrap lxd with run-time dependencies.
                 (wrap-program (string-append bin-dir "lxd")
                   `("PATH" ":" prefix
                     ,(fold (lambda (input paths)
                              ;; TODO: Use 'search-input-directory' rather
                              ;; than look up inputs by name.
                              (let* ((in (assoc-ref inputs input))
                                     (bin (string-append in "/bin"))
                                     (sbin (string-append in "/sbin")))
                                (append (filter file-exists?
                                                (list bin sbin)) paths)))
                            '()
                            '("bash-minimal" "acl" "rsync" "tar" "xz" "btrfs-progs"
                              "gzip" "dnsmasq" "squashfs-tools" "iproute2"
                              "criu" "iptables" "attr"))))
                 ;; Remove unwanted binaries.
                 (for-each (lambda (prog)
                             (delete-file (string-append bin-dir prog)))
                           '("deps" "macaroon-identity" "generate"))
                 ;; Install documentation.
                 (for-each (lambda (file)
                             (install-file file doc-dir))
                           (find-files "doc"))
                 ;; Install bash completion.
                 (rename-file "scripts/bash/lxd-client" "scripts/bash/lxd")
                 (install-file "scripts/bash/lxd" completions-dir))))))))
    (native-inputs
     (list ;; Test dependencies:
           ;; ("go-github-com-rogpeppe-godeps" ,go-github-com-rogpeppe-godeps)
           ;; ("go-github-com-tsenart-deadcode" ,go-github-com-tsenart-deadcode)
           ;; ("go-golang-org-x-lint" ,go-golang-org-x-lint)
           pkg-config))
    (inputs
     (list acl
           eudev
           libdqlite
           libraft
           libcap
           lxc
           ;; Run-time dependencies.
           attr
           bash-minimal
           rsync
           tar
           xz
           btrfs-progs
           gzip
           dnsmasq
           squashfs-tools
           iproute
           criu
           iptables))
    (synopsis "Daemon based on liblxc offering a REST API to manage containers")
    (home-page "https://linuxcontainers.org/lxd/")
    (description "LXD is a next generation system container manager.  It
offers a user experience similar to virtual machines but using Linux
containers instead.  It's image based with pre-made images available for a
wide number of Linux distributions and is built around a very powerful, yet
pretty simple, REST API.")
    (license license:asl2.0)))

(define-public libvirt
  (package
    (name "libvirt")
    (version "10.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://libvirt.org/sources/libvirt-"
                           version ".tar.xz"))
       (sha256
        (base32 "15jpfrn3d2zyhbm5ip7bmpjb6ch2bfxm1h6yfgh0l3bw3g9ppgg1"))
       (patches (search-patches "libvirt-add-install-prefix.patch"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:modules '((guix build meson-build-system)
                  (guix build utils)
                  (ice-9 format))
      #:configure-flags
      #~(list "-Ddriver_qemu=enabled"
              "-Dqemu_user=nobody"
              "-Dqemu_group=kvm"
              "-Dstorage_disk=enabled"
              "-Dstorage_dir=enabled"
              "-Dpolkit=enabled"
              ;; XXX The default, but required to make -Dsasl ‘stick’.
              ;; See <https://gitlab.com/libvirt/libvirt/-/issues/185>
              "-Ddriver_remote=enabled"
              "-Dnls=enabled"           ;translations
              (string-append "-Ddocdir=" #$output "/share/doc/"
                             #$(package-name this-package) "-"
                             #$(package-version this-package))
              "-Dbash_completion=enabled"
              (string-append "-Dinstall_prefix=" #$output)
              "--sysconfdir=/etc"
              "--localstatedir=/var")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'skip-directory-confusion
            (lambda _
              ;; Don't try to install an (unused) /var outside of the store.
              (substitute* "scripts/meson-install-dirs.py"
                (("destdir = .*")
                 "destdir = '/tmp'"))))
          (add-after 'unpack 'patch-commands
            (lambda* (#:key inputs #:allow-other-keys)
              (let-syntax
                  ((patch-command-define
                    ;; Patch lines of the form: #define COMMAND "command"
                    (lambda (x)
                      (syntax-case x ()
                        ((_ file command)
                         (with-syntax ((pattern (datum->syntax x 'pattern)))
                           #'(let ((pattern
                                    (format #f "(#define ~:@(~a~) )\"~a\""
                                            command command)))
                               (substitute* file
                                 ((pattern dummy head)
                                  (format
                                   #f "~a~s" head
                                   (search-input-file
                                    inputs (string-append "sbin/"
                                                          command))))))))))))
                (with-directory-excursion "src/util"
                  (patch-command-define "virdnsmasq.c" "dnsmasq")
                  (patch-command-define "virfirewall.h" "ebtables")
                  (patch-command-define "virfirewall.h" "iptables")
                  (patch-command-define "virfirewall.h" "ip6tables")
                  (patch-command-define "virfirewall.h" "nft")))))
          (add-before 'configure 'disable-broken-tests
            (lambda _
              (let ((tests (list "commandtest"         ; hangs idly
                                 "networkxml2conftest" ; fails with absolute dnsmasq
                                 "qemuxml2argvtest"    ; fails
                                 "virnetsockettest"))) ; tries to network
                (substitute* "tests/meson.build"
                  (((format #f ".*'name': '(~a)'.*" (string-join tests "|")))
                   "")))))
          (add-before 'configure 'fix-test-data
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* (find-files "tests/qemuvhostuserdata/usr/libexec")
                (("^cat") (search-input-file inputs "bin/cat"))))))))
    (inputs
     (list acl
           attr
           audit
           curl
           cyrus-sasl
           dbus
           dmidecode
           dnsmasq
           eudev
           fuse-2
           gnutls
           iproute
           iptables
           json-c
           libnl
           libpcap
           libpciaccess
           libssh2                      ;optional
           libtirpc                     ;for <rpc/rpc.h>
           libxml2
           lvm2                         ;for libdevmapper
           nftables
           openssl
           parted
           `(,util-linux "lib")
           readline
           yajl))
    (native-inputs
     (list bash-completion
           gettext-minimal
           libxslt
           perl
           pkg-config
           polkit
           python-wrapper
           python-docutils              ;for rst2html
           rpcsvc-proto))               ;for rpcgen
    (home-page "https://libvirt.org")
    (synopsis "Simple API for virtualization")
    (description "Libvirt is a C toolkit to interact with the virtualization
capabilities of recent versions of Linux.  The library aims at providing long
term stable C API initially for the Xen paravirtualization but should be able
to integrate other virtualization mechanisms if needed.")
    (license license:lgpl2.1+)))

(define-public libvirt-glib
  (package
    (name "libvirt-glib")
    (version "5.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "ftp://libvirt.org/libvirt/glib/"
                                  "libvirt-glib-" version ".tar.xz"))
              (sha256
               (base32
                "12rv5wxa92iigqlb3ncza6m6995j8739nac7bmbs65i4713c7zlv"))))
    (build-system meson-build-system)
    (inputs
     (list openssl cyrus-sasl lvm2 ; for libdevmapper
           yajl))
    (native-inputs
     (list pkg-config intltool
           `(,glib "bin") vala))
    (propagated-inputs
     ;; ‘Required:’ by the installed .pc files.
     (list glib libvirt libxml2 gobject-introspection))
    (home-page "https://libvirt.org")
    (synopsis "GLib wrapper around libvirt")
    (description "libvirt-glib wraps the libvirt library to provide a
high-level object-oriented API better suited for glib-based applications, via
three libraries:

@enumerate
@item libvirt-glib - GLib main loop integration & misc helper APIs
@item libvirt-gconfig - GObjects for manipulating libvirt XML documents
@item libvirt-gobject - GObjects for managing libvirt objects
@end enumerate
")
    (license license:lgpl2.1+)))

(define-public python-libvirt
  (package
    (name "python-libvirt")
    (version "11.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://libvirt.org/sources/python/libvirt-python-"
                           version ".tar.gz"))
       (sha256
        (base32 "1y26104zgylz0v9d6xc0bm6m13is94shn9dlhjycaf347jjjbs6f"))))
    (build-system pyproject-build-system)
    (inputs
     (list libvirt))
    (propagated-inputs
     (list python-lxml))
    (native-inputs
     (list pkg-config python-pytest python-setuptools python-wheel))
    (home-page "https://libvirt.org")
    (synopsis "Python bindings to libvirt")
    (description "This package provides Python bindings to the libvirt
virtualization library.")
    (properties
     '((upstream-name . "libvirt-python")))
    (license license:lgpl2.1+)))

(define-public virt-manager
  (package
    (name "virt-manager")
    (version "5.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://releases.pagure.org/" name
                                  "/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "0jpqhc02ya55rr8bin734znasslwlff42ann9rsvv5y9w13ax2dw"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:modules
      '((ice-9 match)
        (srfi srfi-1)
        (srfi srfi-26)
        (guix build meson-build-system)
        (guix build utils))
      #:glib-or-gtk? #t
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'set-SOURCE_DIR
            (lambda _
              (setenv "SOURCE_DIR" (getcwd))))
          (add-after 'unpack 'fix-default-uri
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "virtManager/createconn.py"
                (("/usr(/bin/qemu-system-\\*)" _ suffix)
                 (string-append #$(this-package-input "qemu") suffix)))))
          (add-before 'glib-or-gtk-wrap 'wrap-more
            (lambda* (#:key inputs #:allow-other-keys)
              (let* ((paths (filter-map
                             (match-lambda
                               ((output . directory)
                                (let* ((girepodir (string-append
                                                   directory
                                                   "/lib/girepository-1.0")))
                                  (and (file-exists? girepodir)
                                       girepodir))))
                             inputs)))
                (for-each (lambda (file)
                            (format #t "wrapping ~a~%" file)
                            (wrap-program file
                              `("GI_TYPELIB_PATH" prefix ,paths)
                              `("GUIX_PYTHONPATH" prefix
                                ;; FIXME: This wraps too much (see: bug#25235).
                                (,(getenv "GUIX_PYTHONPATH")))))
                          (find-files (string-append #$output "/bin"))))))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (with-directory-excursion (getenv "SOURCE_DIR")
                (invoke "pytest" "-vvv"
                        "-k" (string-append
                              ;; The cdrom_url and CLI0062 tests cause
                              ;; some crash, also outside the build container.
                              "not install_cdrom_url and "
                              "not testCLI0062virt_install and "
                              ;; These tests fail only inside the
                              ;; build container, perhaps due to
                              ;; missing /dev nodes.
                              "not install_s390x_cdrom and "
                              "not install_many_devices and "
                              "not test_disk and "
                              "not virt_clone"))))))))
    (native-inputs
     (list cdrtools
           cpio
           gettext-minimal
           `(,glib "bin")               ;glib-compile-schemas
           gobject-introspection
           `(,gtk+ "bin")               ;gtk-update-icon-cache
           pkg-config
           python-docutils              ;rst2ma
           python-pytest))
    (inputs
     (list bash-minimal
           dconf
           gtk+
           gtk-vnc
           gtksourceview-4
           libosinfo
           libvirt
           libvirt-glib
           python-minimal
           python-libxml2
           python-libvirt
           python-pycairo
           python-pygobject
           python-requests
           qemu
           spice-gtk
           vte/gtk+-3))
    (home-page "https://virt-manager.org/")
    (synopsis "Manage virtual machines")
    (description
     "The virt-manager application is a desktop user interface for managing
virtual machines through libvirt.  It primarily targets KVM VMs, but also
manages Xen and LXC (Linux containers).  It presents a summary view of running
domains, their live performance and resource utilization statistics.")
    (license license:gpl2+)))

(define-public vmware-open-vm-tools
  (package
    (name "vmware-open-vm-tools")
    (version "12.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/vmware/open-vm-tools")
             (commit (string-append "stable-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1hbimhady0v1kd45azknl1lgzgldhgdjd7bj540rn3y4cai5cnk1"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list "--with-fuse=fuse3"
              "--without-kernel-modules"
              "--without-x"
              (string-append
               "--with-udev-rules-dir=" #$output "/lib/udev/rules.d"))
      ;; TODO: Add iproute2 dbus which commands wrap.
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'chdir
            (lambda _
              (chdir "open-vm-tools")))
          (replace 'bootstrap
            (lambda _
              (system* "autoreconf" "-if")))
          (add-after 'bootstrap 'patch-paths
            (lambda _
              (substitute* "Makefile.am"
                (("/etc/vmware-tools/")
                 (string-append #$output "/etc/vmware-tools/")))
              (substitute* "scripts/Makefile.am"
                (("/etc/vmware-tools")
                 (string-append #$output "/etc/vmware-tools"))
                (("/usr/bin")
                 (string-append #$output "/bin")))
              (substitute* "services/vmtoolsd/Makefile.am"
                (("/etc/vmware-tools")
                 (string-append #$output "/etc/vmware-tools"))
                (("\\$\\(PAM_PREFIX\\)")
                 (string-append #$output "/$(PAM_PREFIX)")))
              (substitute* "vgauth/service/Makefile.am"
                (("/etc/vmware-tools/vgauth/schemas")
                 (string-append #$output "/etc/vmware-tools/vgauth/schemas"))
                (("etc/vmware-tools/vgauth.conf")
                 (string-append #$output "/etc/vmware-tools/vgauth.conf")))
              (substitute* "vmhgfs-fuse/config.c"
                (("/bin/fusermount3")
                 (string-append
                  #$(this-package-input "fuse") "/bin/fusermount3")))
              ;; XXX: This part might need more testing with shutdown and halt
              ;; commands provided by Shepherd.
              (substitute* "lib/system/systemLinux.c"
                (("/sbin/shutdown")
                 (string-append
                  #$(this-package-input "shepherd") "/sbin/shutdown"))
                (("/bin/reboot")
                 (string-append
                  #$(this-package-input "shepherd") "/sbin/reboot")))
              (substitute* "services/plugins/vix/foundryToolsDaemon.c"
                (("/bin/mount")
                 (string-append
                  #$(this-package-input "util-linux") "/bin/mount"))
                (("/usr/bin/vmhgfs-fuse")
                 (string-append #$output "/bin/vmhgfs-fuse"))))))))
    (native-inputs
     (list `(,glib "bin")               ; for glib-genmarshal
           autoconf
           automake
           libltdl
           libtool
           pkg-config))
    (inputs
     (list eudev
           fuse
           glib
           xmlsec
           libmspack
           ;; libdnet ; Not packed
           libtirpc
           libxcrypt
           libxml2
           linux-pam
           openssl
           procps
           rpcsvc-proto
           shepherd     ;for 'halt' and 'reboot', invoked from VMWare host.
           util-linux
           xmlsec))
    (home-page "https://github.com/vmware/open-vm-tools")
    (synopsis "Tools for VMWare guest VM to enhance host-guest integration")
    (description
     "@code{open-vm-tools} is a set of services and modules that enable several
features in VMware products for better management of, and seamless user
interactions with, guests.

@code{open-vm-tools} enables the following features in VMware products:

@itemize
@item The ability to perform virtual machine power operations gracefully.
@item Execution of VMware provided or user configured scripts in guests during
various power operations.
@item The ability to run programs, commands and file system operation in guests
to enhance guest automation.
@item Authentication for guest operations.
@item Periodic collection of network, disk, and memory usage information from
the guest.
@item Generation of heartbeat from guests to hosts so VMware's HA solution can
determine guests' availability.
@item Clock synchronization between guests and hosts or client desktops.
@item Quiescing guest file systems to allow hosts to capture
file-system-consistent guest snapshots.
@item Execution of pre-freeze and post-thaw scripts while quiescing guest file
systems.
@item The ability to customize guest operating systems immediately after
powering on virtual machines.
@item Enabling shared folders between host and guest file systems on VMware
Workstation and VMware Fusion.
@item Copying and pasting text, graphics, and files between guests and hosts or
client desktops.
@end itemize")
    (license license:gpl2)))

(define-public vmware-open-vm-tools-gtk
  (package/inherit vmware-open-vm-tools
    (name "vmware-open-vm-tools-gtk")
    (inputs
     (modify-inputs (package-inputs vmware-open-vm-tools)
       (prepend gdk-pixbuf-xlib
                gtk+
                gtkmm-3
                libdrm
                libx11
                libxext
                libxi
                libxinerama
                libxrandr
                libxrender
                libxtst)))
    (arguments
     (substitute-keyword-arguments (package-arguments vmware-open-vm-tools)
       ((#:configure-flags flags)
        #~(delete "--without-x" #$flags))))
    (description "This package provides a GTK+ support for @code{open-vm-tools}.")))

(define-public criu
  (package
    (name "criu")
    (version "3.17.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/checkpoint-restore/criu")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ff3xfcf0wfz02fc0qbj56mci1a0xdl8jzaihaw6qyjvgrsiq7fh"))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"
       #:tests? #f                      ; tests require mounting as root
       #:make-flags
       (list (string-append "PREFIX=" (assoc-ref %outputs "out"))
             (string-append "LIBDIR=$(PREFIX)/lib")
             ;; Upstream mistakenly puts binaries in /var.  Now, in practice no
             ;; plugins are built, but the build system still fails otherwise.
             (string-append "PLUGINDIR=$(LIBDIR)/criu")
             (string-append "ASCIIDOC="
                            (search-input-file %build-inputs
                                               "/bin/asciidoc"))
             (string-append "PYTHON=python3")
             (string-append "XMLTO="
                            (search-input-file %build-inputs
                                               "/bin/xmlto")))
       #:modules ((guix build gnu-build-system)
                  (guix build utils)
                  ((guix build python-build-system)
                   #:select (ensure-no-mtimes-pre-1980)))
       #:imported-modules ,(append %default-gnu-imported-modules
                                   %python-build-system-modules)
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)            ; no configure script
         (add-after 'unpack 'hardcode-variables
           (lambda* (#:key inputs outputs #:allow-other-keys)
             ;; Hardcode arm version detection
             (substitute* "Makefile"
               (("ARMV.*:=.*") "ARMV := 7\n"))
             ;; Hard-code the correct PLUGINDIR above.
             (substitute* "criu/include/plugin.h"
               (("/var") (string-append (assoc-ref outputs "out"))))))
         (add-after 'unpack 'ensure-no-mtimes-pre-1980
           ensure-no-mtimes-pre-1980)
         (add-before 'build 'fix-symlink
           (lambda* (#:key inputs #:allow-other-keys)
             ;; The file 'images/google/protobuf/descriptor.proto' points to
             ;; /usr/include/..., which obviously does not exist.
             (let* ((file "google/protobuf/descriptor.proto")
                    (target (string-append "images/" file))
                    (source (search-input-file
                             inputs
                             (string-append "include/" file))))
               (delete-file target)
               (symlink source target))))
         (add-after 'install 'wrap
           (lambda* (#:key inputs outputs #:allow-other-keys)
             ;; Make sure 'crit' runs with the correct PYTHONPATH.
             (let* ((out  (assoc-ref outputs "out"))
                    (site (string-append out "/lib/python"
                                         ,(version-major+minor
                                           (package-version python))
                                         "/site-packages"))
                    (path (getenv "GUIX_PYTHONPATH")))
               (wrap-program (string-append out "/bin/crit")
                 `("GUIX_PYTHONPATH" ":" prefix (,site ,path))))))
         (add-after 'install 'delete-static-libraries
           ;; Not building/installing these at all doesn't seem to be supported.
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (for-each delete-file (find-files out "\\.a$"))))))))
    (inputs
     (list bash-minimal
           protobuf
           python-protobuf
           iproute
           libaio
           libcap
           libnet
           libnl
           libbsd
           nftables))
    (native-inputs
     (list pkg-config
           perl
           asciidoc
           xmlto
           docbook-xml
           docbook-xsl
           python-toolchain))
    (propagated-inputs
     ;; included by 'rpc.pb-c.h'
     (list protobuf-c))
    (home-page "https://criu.org")
    (synopsis "Checkpoint and restore in user space")
    (description "Using this tool, you can freeze a running application (or
part of it) and checkpoint it to a hard drive as a collection of files.  You
can then use the files to restore and run the application from the point it
was frozen at.  The distinctive feature of the CRIU project is that it is
mainly implemented in user space.")
    ;; The project is licensed under GPLv2; files in the lib/ directory are
    ;; LGPLv2.1.
    (license (list license:gpl2 license:lgpl2.1))))

(define-public python-qemu-qmp
  (package
    (name "python-qemu-qmp")
    (version "0.0.0a0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "qemu.qmp" version))
       (sha256
        (base32 "1rpsbiwvngij6fjcc5cx1azcc4dxmm080crr31wc7jrm7i61p7c2"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (if tests?
                  (begin  ; avocado insists on writing stuff to HOME.
                    (setenv "HOME" "/tmp")
                    (invoke "avocado" "--show=all" "run" "tests/protocol.py"))
                  (format #t "test suite not run~%")))))))
    (native-inputs
     (list python-avocado-framework
           python-flake8
           python-isort
           python-pylint
           python-setuptools
           python-setuptools-scm
           python-wheel))
    (propagated-inputs
     (list python-pygments
           python-urwid
           python-urwid-readline))
    (home-page "https://gitlab.com/jsnow/qemu.qmp")
    (synopsis "QEMU Monitor Protocol Python library")
    (description "@code{emu.qmp} is a
@url{https://gitlab.com/qemu-project/qemu/-/blob/master/docs/interop/qmp-intro.txt,
QEMU Monitor Protocol (QMP)} library written in Python.  It is used to send
QMP messages to running QEMU emulators.  It can be used to communicate with
QEMU emulators, the QEMU Guest Agent (QGA), the QEMU Storage Daemon (QSD), or
any other utility or application that speaks QMP.")
    (license license:gpl2+)))

(define-public qmpbackup
  (package
    (name "qmpbackup")
    (version "0.23")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/abbbi/qmpbackup")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0x9v81z0b2qr2y6m46rfnl4kl5jnixsdrl1c790iwl6pq9kzzvzg"))))
    (build-system python-build-system)
    ;; The test suite requires to download a 241 MiB QEMU image; skip it.
    (arguments (list #:tests? #f))
    (inputs (list python-qemu-qmp))
    (home-page "https://github.com/abbbi/qmpbackup")
    (synopsis "Backup and restore QEMU machines")
    (description "@command{qmpbackup} is designed to create and restore full
and incremental backups of running QEMU virtual machines via QMP, the QEMU
Machine Protocol.")
    (license license:gpl3+)))

(define-public looking-glass-client
  (package
    (name "looking-glass-client")
    (version "B6")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://looking-glass.io/artifact/" version
                                  "/source"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "15d7wwbzfw28yqbz451b6n33ixy50vv8acyzi8gig1mq5a8gzdib"))))
    (build-system cmake-build-system)
    (inputs (list bash-minimal
                  font-dejavu
                  fontconfig
                  freetype
                  glu
                  gmp
                  libglvnd
                  libiberty
                  libsamplerate
                  libx11
                  libxcursor
                  libxfixes
                  libxi
                  libxinerama
                  libxkbcommon
                  libxpresent
                  libxrandr
                  libxscrnsaver
                  mesa
                  pipewire
                  pulseaudio
                  spice-protocol
                  wayland
                  wayland-protocols
                  `(,zlib "static")))
    (native-inputs (list nettle pkg-config))
    (arguments
     (list #:tests? #f ;No tests are available.
           ;; Package uses "-march=native" by default. We disable that to build with the
           ;; lowest supported architecture for reproducibility and CPU compatibility.
           #:configure-flags #~'("-DOPTIMIZE_FOR_NATIVE=OFF"
                                 "-DENABLE_BACKTRACE=no")
           #:make-flags #~'("CC=gcc")
           #:phases #~(modify-phases %standard-phases
                        (add-before 'configure 'chdir-to-client
                          (lambda* (#:key outputs #:allow-other-keys)
                            (chdir "client")))
                        (replace 'install
                          (lambda* (#:key outputs #:allow-other-keys)
                            (install-file "looking-glass-client"
                                          (string-append (assoc-ref outputs
                                                                    "out")
                                                         "/bin"))))
                        (add-after 'install 'wrapper
                          (lambda* (#:key inputs outputs #:allow-other-keys)
                            (wrap-program (string-append (assoc-ref outputs
                                                                    "out")
                                           "/bin/looking-glass-client")
                              `("LD_LIBRARY_PATH" ":" prefix
                                ,(map (lambda (name)
                                        (let ((input (assoc-ref inputs name)))
                                          (string-append input "/lib")))
                                      '("gmp" "libxi"
                                        "nettle"
                                        "mesa"
                                        "wayland"
                                        "fontconfig-minimal"
                                        "freetype"
                                        "libx11"
                                        "libxfixes"
                                        "libxscrnsaver"
                                        "libxinerama")))))))))
    (home-page "https://looking-glass.io/")
    (synopsis "KVM Frame Relay (KVMFR) implementation")
    (description
     "Looking Glass allows the use of a KVM (Kernel-based Virtual
Machine) configured for VGA PCI Pass-through without an attached physical
monitor, keyboard or mouse.  It displays the VM's rendered contents on your
main monitor/GPU.")
    ;; This package requires SSE instructions.
    (supported-systems '("i686-linux" "x86_64-linux"))
    (license license:gpl2+)))

(define-public runc
  (package
    (name "runc")
    (version "1.1.14")
    ;; XXX: Source contains "vendor", consider to unbundle and pack missing
    ;; packages.
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/opencontainers/runc/releases/"
                    "download/v" version "/runc.tar.xz"))
              (file-name (string-append name "-" version ".tar.xz"))
              (sha256
               (base32
                "1nypczyb3fp3cnfdg13grxjhg9361i44zialsdkxcfxb0c9g79jf"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/opencontainers/runc"
       #:install-source? #f
       ;; XXX: 20/139 tests fail due to missing /var, cgroups and apparmor in
       ;; the build environment.
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda* (#:key import-path #:allow-other-keys)
             (with-directory-excursion (string-append "src/" import-path)
               (invoke "make" "all" "man"))))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "make" "localunittest"))))
         (replace 'install
           (lambda* (#:key import-path outputs #:allow-other-keys)
             (with-directory-excursion (string-append "src/" import-path)
               (let ((out (assoc-ref outputs "out")))
                 (invoke "make" "install" "install-bash" "install-man"
                         (string-append "PREFIX=" out)))))))))
    (native-inputs
     (list go-github-com-go-md2man pkg-config))
    (inputs
     (list libseccomp))
    (synopsis "Open container initiative runtime")
    (home-page "https://opencontainers.org/")
    (description
     "@command{runc} is a command line client for running applications
packaged according to the
@uref{https://github.com/opencontainers/runtime-spec/blob/master/spec.md, Open
Container Initiative (OCI) format} and is a compliant implementation of the
Open Container Initiative specification.")
    (license license:asl2.0)))

(define-public umoci
  (package
    (name "umoci")
    (version "0.4.7")
    ;; XXX: Source contain vendor, consider to pack all missing dependencies.
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/opencontainers/umoci/releases/download/v"
             version "/umoci.tar.xz"))
       (file-name (string-append "umoci-" version ".tar.xz"))
       (sha256
        (base32 "0fvljj9k4f83wbqzd8nbijz0p1zaq633f8yxyvl5sy3wjf03ffk9"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/opencontainers/umoci"
       #:install-source? #f
       #:test-subdirs '(".")
       #:phases
       (modify-phases %standard-phases
         (replace 'unpack
           (lambda* (#:key source import-path #:allow-other-keys)
             ;; Unpack the tarball into 'umoci' instead of "runc-${version}".
             (let ((dest (string-append "src/" import-path)))
               (mkdir-p dest)
               (invoke "tar" "-C" (string-append "src/" import-path)
                       "--strip-components=1"
                       "-xvf" source))))
         (replace 'build
           (lambda* (#:key import-path #:allow-other-keys)
             (with-directory-excursion (string-append "src/" import-path)
               ;; TODO: build manpages with 'go-md2man'.
               (invoke "make" "SHELL=bash"))))
         (replace 'install
           (lambda* (#:key import-path outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bindir (string-append out "/bin")))
               (install-file (string-append "src/" import-path "/umoci")
                             bindir)
               #t))))))
    (home-page "https://umo.ci/")
    (synopsis "Tool for modifying Open Container images")
    (description
     "@command{umoci} is a tool that allows for high-level modification of an
Open Container Initiative (OCI) image layout and its tagged images.")
    (license license:asl2.0)))

(define-public skopeo
  (package
    (name "skopeo")
    (version "1.18.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/containers/skopeo")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1rql5yw1ppvh2fppan413irn11jplxj8kmna0zrsy9kniiskbkas"))))
    (build-system gnu-build-system)
    (native-inputs
     (list go-1.22
           go-github-com-go-md2man
           pkg-config))
    (inputs
     (list bash-minimal
           btrfs-progs
           eudev
           libassuan
           libselinux
           libostree
           lvm2
           glib
           gpgme))
    (arguments
     (list
      #:make-flags
      #~(list (string-append "CC=" #$(cc-for-target))
              "PREFIX="
              (string-append "DESTDIR=" #$output)
              "GOGCFLAGS=-trimpath"
              (string-append "GOMD2MAN="
                             #$go-github-com-go-md2man "/bin/go-md2man"))
      #:tests? #f                       ; The tests require Docker
      #:test-target "test-unit"
      #:imported-modules
      (source-module-closure `(,@%default-gnu-imported-modules
                               (guix build go-build-system)))
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (add-after 'unpack 'set-env
            (lambda _
              ;; When running go, things fail because HOME=/homeless-shelter.
              (setenv "HOME" "/tmp")
              ;; Required for detecting btrfs in hack/btrfs* due to bug in GNU
              ;; Make <4.4 causing CC not to be propagated into $(shell ...)
              ;; calls.  Can be removed once we update to >4.3.
              (setenv "CC" #$(cc-for-target))))
          (add-after 'install 'wrap-skopeo
            (lambda _
              (wrap-program (string-append #$output "/bin/skopeo")
                `("PATH" suffix
                  ;; We need at least newuidmap, newgidmap and mount.
                  ("/run/privileged/bin"))))))))
    (home-page "https://github.com/containers/skopeo")
    (synopsis "Interact with container images and container image registries")
    (description
     "@command{skopeo} is a command line utility providing various operations
with container images and container image registries.  It can:
@enumerate

@item Copy container images between various containers image stores,
converting them as necessary.

@item Convert a Docker schema 2 or schema 1 container image to an OCI image.

@item Inspect a repository on a container registry without needlessly pulling
the image.

@item Sign and verify container images.

@item Delete container images from a remote container registry.

@end enumerate")
    (license license:asl2.0)))

(define-public ruby-vagrant-spec-helper-basic
  (package
    (name "ruby-vagrant-spec-helper-basic")
    (version "0.2.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "vagrant-spec-helper-basic" version))
              (sha256
               (base32
                "1qhxxc07dhrma1s1x2g9sma7xxgwzs20s6v5pv9jrpz6bl4b527n"))))
    (build-system ruby-build-system)
    (arguments
     (list #:tests? #f))  ;; has not tests
    (synopsis "Helper for vagrant-spec")
    (description "This package is an internal helper for vagrant-spec.  Don't
use it.")
    (home-page "https://github.com/hashicorp/vagrant-spec")
    (license license:mpl2.0)))

(define-public ruby-vagrant-spec
  (package
    (name "ruby-vagrant-spec")
    (version "0.0.6")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "vagrant_spec" version))
              (sha256
               (base32
                "1bkzz3mj7kzsv6k0ii8w31cgkpiqw3wvmvv2c6rknsavqqnagb4g"))))
    (build-system ruby-build-system)
    ;; (native-inputs (list ruby-rubocop ruby-vagrant-spec-helper-basic))
    (propagated-inputs (list ruby-coveralls ruby-serverspec ruby-dep))
    (arguments
     (list
      #:tests? #f  ;; tests require vagrant
      ;; target 'test' includes 'cops' and running some ansible-playbook
      #:test-target "unit"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch
            (lambda _
              (substitute* "Rakefile"
                (("Bundler::GemHelper") "require 'bundler'\nBundler::GemHelper"))))
          (add-before 'check 'prepare-check
            (lambda _
              (setenv "HOME" "/tmp"))))))
    (synopsis "Specification and tests for Vagrant")
    (description "@code{vagrant-spec} is a both a specification of how Vagrant
and its various components should behave as well as a library of testing
helpers that let you write your own unit and acceptance tests for Vagrant.")
    (home-page "https://github.com/hashicorp/vagrant-spec")
    (license license:mpl2.0)))

(define-public python-vagrant
  (package
    (name "python-vagrant")
    (version "0.5.15")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "python-vagrant" version))
        (sha256
         (base32
          "1ikrh6canhcxg5y7pzmkcnnydikppv7s6sm9prfx90nk0ac8m6mg"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f)) ; tests involve running vagrant.
    (home-page "https://github.com/todddeluca/python-vagrant")
    (synopsis "Python bindings for Vagrant")
    (description
     "Python-vagrant is a Python module that provides a thin wrapper around the
@code{vagrant} command line executable, allowing programmatic control of Vagrant
virtual machines.")
    (license license:expat)))

(define-public bubblewrap
  (package
    (name "bubblewrap")
    (version "0.10.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/containers/bubblewrap/")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0paahq4y8fmdnipahgymsshi3klmi60lvcqhhg1020z7n1gni0hx"))
              (patches (search-patches "bubblewrap-fix-locale-in-tests.patch"))))
    (build-system meson-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'fix-test
                 (lambda* (#:key outputs #:allow-other-keys)
                   ;; Tests try to access /var/tmp, which is not possible in our build
                   ;; environment.  Let's give them another directory.
                   ;; /tmp gets overriden in some tests, so we need another directory.
                   ;; the only possibility is the output directory.
                   (let ((tmp-dir (string-append (assoc-ref outputs "out") "/tmp")))
                     (mkdir-p tmp-dir)
                     (substitute* "tests/test-run.sh"
                       (("/var/tmp") tmp-dir)
                       ;; Tests create a temporary python script, so fix its shebang.
                       (("/usr/bin/env python3") (which "python3"))
                       ;; Tests call /usr/bin/env, so fix its path.
                       (("/usr/bin/env") (which "env"))
                       ;; Some tests try to access /usr, but that doesn't exist.
                       ;; Give them /gnu instead.
                       (("/usr") "/gnu")
                       (("--ro-bind /bin /bin") "--ro-bind /gnu /bin")
                       (("--ro-bind /sbin /sbin") "--ro-bind /gnu /sbin")
                       (("--ro-bind /lib /lib") "--ro-bind /gnu /lib")
                       (("  */bin/bash") (which "bash"))
                       (("/bin/sh") (which "sh"))
                       (("findmnt") (which "findmnt")))
                     (substitute* "tests/libtest.sh"
                       (("/var/tmp") tmp-dir)
                       (("/usr") "/gnu")
                       (("--ro-bind /bin /bin") "--ro-bind /gnu /bin")
                       (("--ro-bind /sbin /sbin") "--ro-bind /gnu /sbin")
                       (("--ro-bind /lib /lib") "--ro-bind /gnu /lib")))))
               ;; Remove the directory we gave to tests to have a clean package.
               (add-after 'check 'remove-tmp-dir
                 (lambda* (#:key outputs #:allow-other-keys)
                   (delete-file-recursively (string-append (assoc-ref outputs "out") "/tmp")))))))
    (inputs (list libcap))
    (native-inputs (list python-wrapper util-linux pkg-config))
    (home-page "https://github.com/containers/bubblewrap")
    (synopsis "Unprivileged sandboxing tool")
    (description "Bubblewrap is aimed at running applications in a sandbox,
restricting their access to parts of the operating system or user data such as
the home directory.  Bubblewrap always creates a new mount namespace, and the
user can specify exactly what parts of the file system should be made visible
in the sandbox.  These directories are mounted with the @code{nodev} option
by default and can be made read-only.")
    (license license:lgpl2.0+)))

(define-public bochs
  (package
    (name "bochs")
    (version "2.8")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://sourceforge.net/projects/bochs/files/bochs/"
                           version "/bochs-" version ".tar.gz"))
       (sha256
        (base32 "0n80v8wjd9i3rhc51sq7n7xw2paz7g1scsrmkxx1yhfqyypi6nx8"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f))                    ; no tests exist
    (inputs
     (list libxrandr))
    (home-page "https://bochs.sourceforge.net/")
    (synopsis "Emulator for x86 PC")
    (description
     "Bochs is an emulator which can emulate Intel x86 CPU, common I/O
devices, and a custom BIOS.  It can also be compiled to emulate many different
x86 CPUs, from early 386 to the most recent x86-64 Intel and AMD processors.
Bochs can run most Operating Systems inside the emulation including Linux,
DOS or Microsoft Windows.")
    (license license:lgpl2.0+)))

(define-public xen
  (package
    (name "xen")
    (version "4.19.0")               ; please update the mini-os input as well
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://xenbits.xen.org/git-http/xen.git")
                    (commit (string-append "RELEASE-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1r33ak7j6czcjxf5zxswfkppnv0w1n6hi262x9rk08bqyvcpxb23"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list "--enable-rpath"
              "--disable-qemu-traditional" ; tries to "git clone"
              "--disable-rombios"       ; tries to "git clone" via etherboot
              ;; TODO: Re-enable stubdom (it's "more secure" to use it).
              "--disable-stubdom"    ; tries to "git clone" old patched newlib
              (string-append "--with-initddir=" #$output "/etc/init.d")
              (string-append "--with-system-qemu="
                             (search-input-file %build-inputs
                                                "bin/qemu-system-i386"))
              (string-append "--with-system-seabios="
                             (search-input-file %build-inputs
                                                "share/firmware/bios.bin"))
              (string-append "--with-system-ovmf="
                             (search-input-file %build-inputs
                                                "share/firmware/ovmf_ia32.bin")))
      #:make-flags
      #~(list "XEN_BUILD_DATE=Thu Jan  1 01:00:01 CET 1970"
              "XEN_BUILD_TIME=01:00:01"
              "XEN_BUILD_HOST="
              "ETHERBOOT_NICS="
              "SMBIOS_REL_DATE=01/01/1970"
              "VGABIOS_REL_DATE=01 Jan 1970"
              ;; QEMU_TRADITIONAL_LOC
              ;; QEMU_UPSTREAM_LOC
              "SYSCONFIG_DIR=/tmp/etc/default"
              (string-append "BASH_COMPLETION_DIR=" #$output
                             "/etc/bash_completion.d")
              (string-append "BOOT_DIR=" #$output "/boot")
              (string-append "DEBUG_DIR=" #$output "/lib/debug")
              (string-append "EFI_DIR=" #$output "/lib/efi")
              (string-append "SHLIB_libxenctrl=-Wl,-rpath=" #$output "/lib")
              (string-append "SHLIB_libxenguest=-Wl,-rpath=" #$output "/lib")
              (string-append "SHLIB_libxenstore=-Wl,-rpath=" #$output "/lib")
              "MINIOS_UPSTREAM_URL=")
      #:test-target "test"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'unpack-mini-os
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((mini-os (dirname (search-input-file inputs "minios.mk"))))
                (copy-recursively mini-os "extras/mini-os"))))
          (add-after 'unpack-mini-os 'patch
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "tools/firmware/Rules.mk"
                (("override XEN_TARGET_ARCH = x86_32" match)
                 (string-append match "\noverride CC = "
                                (search-input-file inputs
                                                   "bin/i686-linux-gnu-gcc")))
                (("^CFLAGS =$" match)
                 (string-append match " -I" (assoc-ref inputs "cross-libc")
                                "/include\n")))
              (substitute* "config/x86_32.mk"
                (("(CFLAGS += )-m32 -march=i686" _ match)
                 (string-append match "-march=i686 -I"
                                (assoc-ref inputs "cross-libc") "/include")))
              ;; /var is not in /gnu/store, so don't try to create it.
              (substitute* '("tools/Makefile"
                             "tools/xenstored/Makefile"
                             "tools/xenpaging/Makefile")
                (("\\$\\(INSTALL_DIR\\) .*XEN_(DUMP|LOG|RUN|LIB|PAGING)_DIR.*")
                 "\n")
                (("\\$\\(INSTALL_DIR\\) .*XEN_(RUN|LIB)_STORED.*") "\n"))
              ;; Prevent xen from creating /etc.
              (substitute* "tools/examples/Makefile"
                ((" install-(configs|readmes)") ""))
              ;; Set rpath.
              (substitute* "tools/pygrub/setup.py"
                (("library_dirs =" match)
                 ;; TODO: extra_link_args = ['-Wl,-rpath=/opt/foo'],
                 (string-append "runtime_library_dirs = ['" #$output "/lib'],"
                                "\n" match)))))
          (add-before 'configure 'patch-xen-script-directory
            (lambda _
              (substitute* '("configure"
                             "tools/configure"
                             "docs/configure")
                (("(XEN_SCRIPT_DIR=).*" _ match)
                 (string-append match #$output "/etc/xen/scripts")))))
          (add-before 'configure 'set-environment-up
            (lambda* (#:key make-flags #:allow-other-keys)
              (define (cross? x)
                (string-contains x "cross-i686-linux"))
              (define (filter-environment! filter-predicate
                                           environment-variable-names)
                (for-each
                 (lambda (env-name)
                   (let* ((env-value (getenv env-name))
                          (search-path (search-path-as-string->list env-value))
                          (new-search-path (filter filter-predicate
                                                   search-path))
                          (new-env-value (list->search-path-as-string
                                          new-search-path ":")))
                     (setenv env-name new-env-value)))
                 environment-variable-names))
              (setenv "CROSS_C_INCLUDE_PATH" (getenv "C_INCLUDE_PATH"))
              (setenv "CROSS_CPLUS_INCLUDE_PATH" (getenv "CPLUS_INCLUDE_PATH"))
              (setenv "CROSS_LIBRARY_PATH" (getenv "LIBRARY_PATH"))
              (filter-environment! cross?
                                   '("CROSS_C_INCLUDE_PATH"
                                     "CROSS_CPLUS_INCLUDE_PATH"
                                     "CROSS_LIBRARY_PATH"))
              (filter-environment! (lambda (e) (not (cross? e)))
                                   '("C_INCLUDE_PATH" "CPLUS_INCLUDE_PATH"
                                     "LIBRARY_PATH"))
              ;; Guix tries to be helpful and automatically adds
              ;; mini-os-git-checkout/include to the include path,
              ;; but actually we don't want it to be there (yet).
              (filter-environment! (lambda (e)
                                     (not
                                      (string-contains e
                                                       "mini-os-git-checkout")))
                                   '("C_INCLUDE_PATH" "CPLUS_INCLUDE_PATH"
                                     "LIBRARY_PATH"))
              (setenv "EFI_VENDOR" "guix")))
          (replace 'build
            (lambda* (#:key make-flags parallel-build? #:allow-other-keys)
              (apply invoke "make" "world"
                     "-j" (number->string
                           (if parallel-build? (parallel-job-count) 1))
                     make-flags)))
          (add-after 'install 'remove-cruft
            (lambda _
              (with-directory-excursion #$output
                ;; Delete useless (and irreproducible) build-time left-overs.
                (for-each delete-file
                          (find-files "share/doc" "^\\.deps$"))))))))
    (inputs
     (list acpica                       ; TODO: patch iasl invocation
           bridge-utils                 ; TODO: patch invocations
           glib
           iproute                      ; TODO: patch invocations
           libaio
           libx11
           libxcrypt                    ; required by Python.h
           yajl
           ncurses
           openssl
           ovmf-i686
           pixman
           qemu-minimal
           seabios
           `(,util-linux "lib")         ; uuid
           ;; TODO: ocaml-findlib, ocaml-nox.
           xz                           ; for liblzma
           zlib))
    (native-inputs
     (list dev86
       bison
       cmake-minimal
       figlet
       flex
       gettext-minimal
       libnl
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://xenbits.xen.org/git-http/mini-os.git")
               ;; This corresponds to (string-append "xen-RELEASE-" version))
               ;; at time of packaging, but upstream has unfortunately modified
               ;; existing tags in the past.  Also, not all Xen releases get a
               ;; new tag.  See <https://xenbits.xen.org/gitweb/?p=mini-os.git>.
               (commit "8b038c7411ae7e823eaf6d15d5efbe037a07197a")))
         (sha256
          (base32 "1xgazvvhy5m9nabbmlwslynhk73k9a8wnzrjwjplj52f0cm10fjq"))
         (file-name (string-append name "-" version "-mini-os-git-checkout")))
       perl
       ;; TODO: markdown.
       pkg-config
       python
       wget
       (cross-gcc "i686-linux-gnu"
                  #:xbinutils (cross-binutils "i686-linux-gnu")
                  #:libc (cross-libc "i686-linux-gnu"))
       (cross-libc "i686-linux-gnu") ; header files
       `(,(cross-libc "i686-linux-gnu") "static")))
    (home-page "https://xenproject.org/")
    (synopsis "Hypervisor")
    (description "This package provides the Xen Virtual Machine Monitor
which is a hypervisor.")
    ;; TODO: Some files are licensed differently.  List those.
    (license license:gpl2)
    (supported-systems '("i686-linux" "x86_64-linux" "armhf-linux"))))

(define-public xen-guest-agent
  (package
    (name "xen-guest-agent")
    (version "0.4.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.com/xen-project/xen-guest-agent")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32 "1ab6mgrvnd49m0ay9fbfyd02xaf3qvkwhyyavra4a7wpz0brg54h"))))
    (build-system cargo-build-system)
    (arguments
     (list
       #:install-source? #f
       #:cargo-inputs
       (list rust-futures-0.3
             rust-libc-0.2
             rust-tokio-1
             rust-netlink-packet-core-0.7
             rust-netlink-packet-route-0.18
             rust-netlink-proto-0.11
             rust-rtnetlink-0.14
             rust-async-stream-0.3
             rust-os-info-3
             rust-pnet-datalink-0.35    ; any version
             rust-pnet-base-0.35        ; any version
             rust-ipnetwork-0.20        ; any version
             rust-log-0.4
             rust-env-logger-0.10
             rust-clap-4
             rust-xenstore-rs-0.6
             ;; Unix-specific dependencies
             rust-uname-0.1
             rust-syslog-6
             rust-sysctl-0.5)))
    (native-inputs
     (list pkg-config
           xen ; Pull in Xen for libxenstore
           clang))
    (home-page "https://gitlab.com/xen-project/xen-guest-agent")
    (synopsis "Provides guest VM information to hosting Xen hypervisor")
    (description "The agent gathers some guest information, and writes them to
xenstore so tooling in dom0 can read it.  The default behavior is to be
compatible with the XAPI toolstack as currently used in XCP-ng and Citrix
Hypervisor/Xenserver, and thus roughly follow what @code{xe-guest-utilities}
is doing.")
    (license license:agpl3)))

(define-public xe-guest-utilities
  (package
    (name "xe-guest-utilities")
    (version "8.4.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/xenserver/xe-guest-utilities")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1yqspizhq3ii6cz2w75slaxy8838yyri9pmgc2q1radnm7w735if"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/xenserver/xe-guest-utilities"
      #:install-source? #f
      #:tests? #f ; There are no tests.
      #:phases
      #~(modify-phases %standard-phases
          ;; Despite using go-build-system, this project does not use Go's build
          ;; infrastructure to do anything, instead relying on a Makefile.
          ;; NOTE: This target builds a tarball, but it is only filled with
          ;; 2 binaries, 1 script, and a bunch of text files; it is tiny.
          (add-after 'patch-source-shebangs 'fix-udev-rule
            (lambda* (#:key inputs import-path #:allow-other-keys)
              (substitute* (string-append "src/" import-path "/mk/xen-vcpu-hotplug.rules")
                (("/bin/sh") (search-input-file inputs "/bin/sh")))))
          (replace 'build
            (lambda* (#:key import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                ;; Explicitly state version, removes git as native-input.
                ;; NOTE: The final step of the Makefile's build target is to "cd"
                ;; to the final build directory.
                (invoke "make" (string-append "RELEASE=" #$version) "build"))))
          ;; The default "install" actions produce package-manager-specific
          ;; outputs, .deb, .rpm, and .tgz. We just copy the final build
          ;; products out.
          (replace 'install
            (lambda* (#:key outputs import-path #:allow-other-keys)
              (let* ((stage (string-append "src/" import-path "/build/stage"))
                     (out (assoc-ref outputs "out")))
                ;; Put udev rules in #$output/lib/udev/rules.d/
                (copy-recursively (string-append stage "/etc/udev")
                                  (string-append out "/lib/udev"))
                ;; Copy produced binaries and scripts
                (copy-recursively (string-append stage "/usr") out)))))))
    (native-inputs (list go-golang-org-x-sys))
    (inputs (list bash-minimal))
    (home-page "https://github.com/xenserver/xe-guest-utilities")
    (synopsis "XenServer guest utilities for unix-like operating systems")
    (description "The XenServer guest utilities enable a Xen-based hypervisor,
(Citrix XenServer, XCP-NG, etc.) to work with a Xen-enabled Unix-like guest VMs.
This allows the guest to share information about its state back to the host,
such IP address, memory usage, etc. and allows the host to inform the guest VM
about events that change the virtualized hardware, such as hotplugging.")
    (license license:bsd-2)))

(define-public osinfo-db-tools
  (package
    (name "osinfo-db-tools")
    (version "1.12.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://releases.pagure.org/libosinfo/osinfo-db-tools-"
                                  version ".tar.xz"))

              (sha256
               (base32
                "1wz7fqvrqpw1bvp3f37n03xhxj4g1fr09vd8vqjhyxqqbmkmycgk"))))
    (build-system meson-build-system)
    (inputs
     (list libsoup-minimal-2 libxml2 libxslt json-glib libarchive))
    (native-inputs
     (list perl
           gobject-introspection
           gettext-minimal
           pkg-config
           ;; Tests
           python
           python-pytest
           python-requests))
    (home-page "https://gitlab.com/libosinfo/osinfo-db-tools")
    (synopsis "Tools for managing the osinfo database")
    (description "This package contains a set of tools to assist
administrators and developers in managing the database.")
    (license license:lgpl2.0+)))

(define-public osinfo-db
  (package
    (name "osinfo-db")
    (version "20240701")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://releases.pagure.org/libosinfo/osinfo-db-"
                           version ".tar.xz"))
       (sha256
        (base32 "0pchyq749nh6ivvd3d9bgd7zqdqa07x94jpsprrz8i8c5ykq2wqx"))))
    (build-system trivial-build-system)
    (arguments
     (list
      #:modules '((guix build utils))
      #:builder
      #~(begin
          (use-modules (guix build utils))
          (let ((osinfo (string-append #$output "/share/osinfo"))
                (source (assoc-ref %build-inputs "source"))
                (import-osinfo-db
                 (string-append #$(this-package-native-input "osinfo-db-tools")
                                "/bin/osinfo-db-import")))
            (mkdir-p osinfo)
            (invoke import-osinfo-db "--dir" osinfo source)))))
    (native-inputs
     (list intltool osinfo-db-tools))
    (home-page "https://gitlab.com/libosinfo/osinfo-db")
    (synopsis "Database of information about operating systems")
    (description "Osinfo-db provides the database files for use with the
libosinfo library.  It provides information about guest operating systems for
use with virtualization provisioning tools")
    (license license:lgpl2.0+)))

(define-public python-transient
  (package
    (name "python-transient")
    (version "0.12")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "transient" version))
       (sha256
        (base32 "148yiqrmcscsi6787y0f27i1y9cf0gcw3mqfv5frhpmsmv62mv5z"))))
    (build-system pyproject-build-system)
    (arguments
     `(#:tests? #f ; Requires behave
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'fix-dependencies
                    (lambda _
                      (substitute* "setup.py"
                        (("==")
                         ">=")))))))
    (native-inputs
     (list python-black
           python-mypy
           python-pyhamcrest
           python-setuptools
           python-twine
           python-wheel))
    (propagated-inputs
     (list python-beautifultable
           python-click
           python-importlib-resources
           python-lark-parser
           python-marshmallow
           python-progressbar2
           python-requests
           python-toml))
    (home-page "https://github.com/ALSchwalm/transient")
    (synopsis "QEMU Wrapper written in Python")
    (description
     "@code{transient} is a wrapper for QEMU allowing the creation of virtual
machines with shared folder, ssh, and disk creation support.")
    (license license:expat)))

(define-public riscv-pk
  (package
    (name "riscv-pk")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/riscv-software-src/riscv-pk")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1cc0rz4q3a1zw8756b8yysw8lb5g4xbjajh5lvqbjix41hbdx6xz"))))
    (build-system gnu-build-system)
    (arguments
     (list #:out-of-source? #t
           ;; riscv-pk can only be built for riscv64.
           #:target "riscv64-linux-gnu"
           #:make-flags #~(list (string-append "INSTALLDIR=" #$output))
           ;; Add flags to keep symbols fromhost and tohost. These symbols are
           ;; required for the correct functioning of pk.
           #:strip-flags #~(list "--strip-unneeded"
                                 "--keep-symbol=fromhost"
                                 "--keep-symbol=tohost"
                                 "--enable-deterministic-archives")))
    (home-page "https://github.com/riscv-software-src/riscv-pk")
    (synopsis "RISC-V Proxy Kernel")
    (description "The RISC-V Proxy Kernel, @command{pk}, is a lightweight
application execution environment that can host statically-linked RISC-V ELF
binaries.  It is designed to support tethered RISC-V implementations with
limited I/O capability and thus handles I/O-related system calls by proxying
them to a host computer.

This package also contains the Berkeley Boot Loader, @command{bbl}, which is a
supervisor execution environment for tethered RISC-V systems.  It is designed
to host the RISC-V Linux port.")
    (license license:bsd-3)))

(define-public hivex
  (package
    (name "hivex")
    (version "1.3.24")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://libguestfs.org/download/"
                                  name "/" name "-" version ".tar.gz"))
              (sha256
               (base32
                "0g0rib62qg81fda8lxsaa7a1ykqy4rl5sq185pdqm9y9xifa8bx5"))))
    (build-system gnu-build-system)
    (native-inputs (list automake
                         autoconf
                         gettext-minimal
                         libtool
                         ocaml
                         pkg-config
                         perl-io-stringy
                         python-wrapper
                         ruby
                         ruby-rake
                         ruby-rdoc))
    (inputs
     (list bash-minimal
           libxml2
           perl
           readline))
    (arguments
     (list
      #:configure-flags
      #~(list "--disable-static" "--with-readline" "--disable-rpath"
              (string-append "LDFLAGS=-Wl,-rpath=" #$output "/lib"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-makefiles
            (lambda _
              (let* ((current-system (or #$(%current-target-system)
                                         #$(%current-system)))
                     (ocamllib
                      (string-append #$output "/lib/ocaml/"
                                     #$(package-version
                                        (this-package-native-input "ocaml")) "/site-lib"))
                     (python-installdir
                      (string-append #$output "/lib/python"
                                     #$(version-major+minor
                                        (package-version
                                         (this-package-native-input
                                          "python-wrapper")))
                                     "/site-packages"))
                     (ruby-version
                      #$(package-version
                         (this-package-native-input "ruby")))
                     (ruby-libdir
                      (string-append #$output
                                     "/lib/ruby/site_ruby/"
                                     ruby-version))
                     (ruby-archdir
                      (string-append ruby-libdir "/" current-system)))
                (substitute* "lib/Makefile.am"
                  (((string-append "\\$\\(VERSION_SCRIPT_FLAGS\\)"
                                   "\\$\\(srcdir\\)/hivex\\.syms"))
                   ""))
                (substitute* "python/Makefile.am"
                  (("\\$\\(PYTHON_INSTALLDIR\\)")
                   python-installdir))
                (substitute* "ocaml/Makefile.am"
                  (("\\$\\(DESTDIR\\)\\$\\(OCAMLLIB\\)")
                   ocamllib))
                (substitute* "ruby/Makefile.am"
                  (("\\$\\(DESTDIR\\)\\$\\(RUBY_ARCHDIR\\)")
                   ruby-archdir)
                  (("\\$\\(DESTDIR\\)\\$\\(RUBY_LIBDIR\\)")
                   ruby-libdir))
                ;; The ‘validate-runpath’ phase fails to find libhivex.so.0.
                (substitute* "perl/Makefile.PL.in"
                  (("CCFLAGS => \\$Config\\{ccflags\\} \\. ' @CFLAGS@',")
                   (string-append "CCFLAGS => $Config{ccflags} . ' @CFLAGS@',
    LDDLFLAGS => $Config{lddlflags} . ' -Wl,-rpath," #$output "/lib',")))
                (substitute* "ruby/ext/hivex/extconf.rb"
                  (("create_header")
                   (string-append "
$LDFLAGS += \" -Wl,-rpath=" #$output "/lib \"
create_header"))))))
          (add-after 'install 'wrap-binaries
            (lambda _
              (let ((hivexregedit
                     (string-append #$output "/bin/hivexregedit"))
                    (hivexml
                     (string-append #$output "/bin/hivexml")))
                (wrap-program hivexregedit
                  `("PERL5LIB" ":" prefix
                    (,(string-append #$output "/lib/perl5/site_perl")))
                  `("PATH" ":" prefix
                    (,(string-append #$output "/bin"))))
                (wrap-program hivexml
                  `("PATH" ":" prefix
                    (,(string-append #$output "/bin"))))))))))
    (home-page "https://github.com/libguestfs/hivex")
    (synopsis "Windows registry hive extraction library")
    (description
     "This package provides a self-contained library for reading and writing
Windows Registry \"hive\" binary files.  Unlike many other tools in this area,
it doesn't use the textual @code{.REG} format for output, because parsing that
is as much trouble as parsing the original binary format.  Instead it makes the
file available through a C API, or through a separate program to export the
hive as XML.")
    (license license:lgpl2.1)))

(define-public libguestfs-minimal
  (package
    (name "libguestfs-minimal")
    (version "1.53.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://libguestfs.org/download/"
                                  (version-major+minor version)
                                  "-stable/libguestfs-" version ".tar.gz"))
              (sha256
               (base32
                "0vssarc3n4kv26fyjmkrrcvh55v41fhycba43pij3rc2izl72s2y"))
              (patches
               (search-patches "libguestfs-syms.patch"))))
    (build-system gnu-build-system)
    (arguments
     (list #:configure-flags
           #~(list "--disable-appliance"
                   "--disable-daemon"
                   "--disable-static"
                   "--disable-erlang"
                   "--disable-golang"
                   "--disable-haskell"
                   "--disable-java"
                   "--disable-lua"
                   ;; FIXME: Perl bindings have wrong rpath and break the
                   ;; validate-runpath phase.  Temporarily disable them until
                   ;; a way is found to correctly patch perl/Build.PL.in.
                   "--disable-perl"
                   "--disable-php"
                   "--with-distro=\"Guix System\""
                   "--with-readline"
                   (string-append "LDFLAGS=-Wl,-rpath," %output "/lib"))
           #:make-flags #~`("REALLY_INSTALL=yes")
           #:phases
           #~(let* ((lib (string-append #$output "/lib"))
                    (lib/ocaml (string-append lib "/ocaml")))
               (modify-phases %standard-phases
                 (add-after 'unpack 'patch-makefiles
                   (lambda _
                     (for-each patch-shebang
                               (find-files "."))
                     (substitute* "ocaml/Makefile.in"
                       (("\\$\\(DESTDIR\\)\\$\\(OCAMLLIB\\)")
                        lib/ocaml))
                     ;; FIXME: Perl bindings have broken runpath,
                     ;; this substitution doesn't seem to work.
                     (substitute* "perl/Build.PL.in"
                       (("extra_linker_flags => \\[")
                        (string-append "extra_linker_flags => [
        '-L" #$output "/lib',")))))
                 (replace 'check
                   (lambda* (#:key tests? make-flags #:allow-other-keys)
                     (when tests?
                       (apply invoke `("make" ,@make-flags "check-direct")))))
                 (replace 'install
                   (lambda* (#:key make-flags #:allow-other-keys)
                     (mkdir-p "temp-build-dir")
                     (apply invoke `("make" ,@make-flags "INSTALLDIRS=vendor"
                                     "install"))))
                 (add-after 'install 'wrap-binaries
                   (lambda _
                     (let ((bin (string-append #$output "/bin")))
                       (for-each
                        (lambda (binary)
                          (use-modules (srfi srfi-1))
                          (wrap-program binary
                            `("PERL5LIB" ":" prefix
                              (,(string-append #$output
                                               "/lib/perl5/site_perl")))
                            `("PATH" ":" prefix
                              ,(search-path-as-list
                                '("bin")
                                (map second
                                     '#$(package-inputs this-package))))))
                        (find-files bin)))))
                 (replace 'validate-documentation-location
                   (lambda _
                     (let ((man-dir
                            (string-append #$output "/man"))
                           (info-dir
                            (string-append #$output "/info")))
                       (for-each (lambda (d)
                                   (invoke "rm" "-rf" d))
                                 (list man-dir info-dir)))))))))
    (native-inputs (list augeas
                         bison
                         cpio
                         flex
                         gettext-minimal
                         gperf
                         libtool
                         ocaml
                         ocaml-findlib
                         ncurses
                         perl
                         perl-getopt-long
                         perl-module-build
                         pkg-config
                         po4a
                         xorriso
                         xz
                         zstd))
    (inputs
     (list file
           fuse
           jansson
           hivex
           libtirpc
           pcre2
           readline
           qemu))
    (home-page "https://libguestfs.org/")
    (synopsis "Access and modify virtual machine disk images")
    (description
     "@code{libguestfs} is a set of tools for accessing and modifying virtual
machine (VM) disk images.  You can use this for viewing and editing files inside
guests, scripting changes to VMs, monitoring disk used/free statistics, creating
guests, P2V, V2V, performing backups, cloning VMs, building VMs, formatting
disks, resizing disks, and much more.")
    (license (list license:gpl2+ license:lgpl2.1+))))

(define-public libguestfs
  (package/inherit libguestfs-minimal
    (name "libguestfs")
    (arguments
     (substitute-keyword-arguments (package-arguments libguestfs-minimal)
       ((#:configure-flags flags)
        #~(append
           (filter
            (lambda (flag)
              (not (string-prefix? "LDFLAGS" flag)))
            #$flags)
           (list
            "--enable-vala=yes"
            (string-append "--with-python-installdir="
                           #$output "/lib/python"
                           #$(version-major+minor
                              (package-version python))
                           "/site-packages")
            (string-append "LDFLAGS=-Wl,-rpath," %output "/lib"))))
       ((#:phases phases)
        #~(modify-phases #$phases
            (add-after 'patch-makefiles 'patch-additional-makefiles
              (lambda _
                (let* ((current-system (or #$(%current-target-system)
                                           #$(%current-system)))
                       (lib (string-append #$output "/lib"))
                       (share (string-append #$output "/share"))
                       (completions
                        (string-append share "/bash-completion/completions"))
                       (lib/lua (string-append lib "/lua"))
                       (lib/ocaml (string-append lib "/ocaml"))
                       (ruby-version
                        #$(package-version
                           (this-package-native-input "ruby")))
                       (ruby-libdir
                        (string-append lib
                                       "/ruby/site_ruby/"
                                       ruby-version))
                       (ruby-archdir
                        (string-append ruby-libdir "/" current-system)))
                  (substitute* "m4/guestfs-bash-completion.m4"
                    (("`pkg-config --variable=completionsdir bash-completion`")
                     completions))
                  (substitute* "ocaml/Makefile.am"
                    (("\\$\\(DESTDIR\\)\\$\\(OCAMLLIB\\)")
                     lib/ocaml))
                  (substitute* "lua/Makefile.am"
                    (("\\$\\(DESTDIR\\)\\$\\(lualibdir\\)")
                     lib/lua))
                  (substitute* "ruby/Makefile.am"
                    (("\\$\\(DESTDIR\\)\\$\\(RUBY_ARCHDIR\\)")
                     ruby-archdir)
                    (("\\$\\(DESTDIR\\)\\$\\(RUBY_LIBDIR\\)")
                     ruby-libdir))
                  ;; The ‘validate-runpath’ phase fails to find libguestfs.so.0.
                  (substitute* "ruby/ext/guestfs/extconf.rb.in"
                    (("create_header")
                     (string-append "
$LDFLAGS += \" -Wl,-rpath=" #$output "/lib \"
create_header"))))))))))
    (native-inputs
     (modify-inputs (package-native-inputs libguestfs-minimal)
       (prepend autoconf
                automake
                bash-completion
                cdrtools
                gobject-introspection
                python
                ruby
                util-linux
                vala)))
    (inputs
     (modify-inputs (package-inputs libguestfs-minimal)
       (prepend acl
                bdb
                fuse
                gmp
                libapparmor
                libcap
                libcap-ng
                libconfig
                libvirt
                libxcrypt
                numactl
                yajl)))))
