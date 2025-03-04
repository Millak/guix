;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 Paul van der Walt <paul@denknerd.org>
;;; Copyright © 2015, 2016, 2021 Sou Bunnbu <iyzsong@member.fsf.org>
;;; Copyright © 2015, 2016 Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>
;;; Copyright © 2015, 2018, 2023 David Thompson <dthompson2@worcester.edu>
;;; Copyright © 2016 Manolis Fragkiskos Ragkousis <manolis837@gmail.com>
;;; Copyright © 2016, 2017, 2018, 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017-2024 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2017, 2020, 2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017, 2018, 2019 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2019 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2019 David Wilson <david@daviwil.com>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2020 Christopher Howard <christopher@librehacker.com>
;;; Copyright © 2021 Felipe Balbi <balbi@kernel.org>
;;; Copyright © 2021, 2024 Felix Gruber <felgru@posteo.net>
;;; Copyright © 2021, 2024, 2025 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2021 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2023 c4droid <c4droid@foxmail.com>
;;; Copyright © 2023 Yovan Naumovski <yovan@gorski.stream>
;;; Copyright © 2023 Hendursaga <hendursaga@aol.com>
;;; Copyright © 2023 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2024 Artyom V. Poptsov <poptsov.artyom@gmail.com>
;;; Copyright © 2025 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2025 Andrew Wong <wongandj@icloud.comg>
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

(define-module (gnu packages emulators)
  #:use-module (ice-9 match)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix svn-download)
  #:use-module (guix hg-download)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages autogen)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages cdrom)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages containers)
  #:use-module (gnu packages cross-base)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages digest)
  #:use-module (gnu packages engineering)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages fltk)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages fribidi)
  #:use-module (gnu packages game-development)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages graphics)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages libedit)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages music)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pretty-print)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages upnp)
  #:use-module (gnu packages video)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages web)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system python)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system qt)
  #:use-module (guix build-system trivial))

(define-public vice
  (package
    (name "vice")
    (version "3.8")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/vice-emu/releases/"
                           "vice-" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "010h3aj0y9n8kcg5yvy1m7g4hc7nbm5gym5r3f3jmk5vyb8c8z8x"))))
    (build-system gnu-build-system)
    (arguments
     (list #:configure-flags #~(list "--disable-html-docs"
                                     "--disable-pdf-docs")))
    (native-inputs
     (list bison
           dos2unix
           flex
           `(,glib "bin") ; for glib-genmarshal, etc.
           pkg-config))
    (inputs
     (list alsa-lib
           curl
           glew
           glib
           gtk+
           pulseaudio
           sdl
           sdl-image
           xa))
    (home-page "https://vice-emu.sourceforge.io/")
    (synopsis "The versatile Commodore emulator")
    (description
     "VICE is a program that emulates the C64, the C64DTV, the C128, the
VIC20, practically all PET models, the PLUS4 and the CBM-II (aka
C610/C510).  An extra emulator is provided for C64 expanded with the CMD
SuperCPU.")
    (license license:gpl2+)))

(define-public blastem
  (package
    (name "blastem")
    (version "0.6.2")
    (source (origin
              (method hg-fetch)
              (uri (hg-reference
                    (url "https://www.retrodev.com/repos/blastem")
                    (changeset (string-append "v" version))))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "08ycfisivh9rb9vmijlrpdryaw8spd81ck48960p15cnf8h2535q"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; TODO: Separately package and unbundle nuklear
                  (delete-file-recursively "zlib")))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags (list (string-append "CC=" ,(cc-for-target))
                          "HOST_ZLIB=1"
                          "HAS_PROC=-DHAS_PROC"
                          (string-append "CONFIG_PATH="
                                         %output "/share/blastem")
                          (string-append "DATA_PATH="
                                         %output "/share/blastem"))
       #:tests? #f ; No check target and custom tests don't seem to build
       #:imported-modules
       ((guix build copy-build-system)
        ,@%default-gnu-imported-modules)
       #:modules
       (((guix build copy-build-system)
         #:prefix copy:)
        (guix build gnu-build-system)
        (guix build utils))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-source
           (lambda _
             (substitute* (find-files "." ".*\\.[ch]")
               (("\"zlib/zlib.h\"") "<zlib.h>"))
             (substitute* "Makefile"
               (("CFLAGS:=-std=gnu99" all)
                (string-append all " -fcommon")))))
         (delete 'configure)
         (replace 'install
           (lambda* args
             (apply (assoc-ref copy:%standard-phases 'install)
                    #:install-plan
                    '(("." "bin" #:include ("blastem" "vgmplay"))
                      ("." "share/blastem"
                       #:include ("default.cfg" "rom.db")
                       #:exclude ("android"))
                      ("shaders" "share/blastem/shaders"))
                    args))))))
    (inputs
     (list glew mesa sdl2 zlib))
    (native-inputs
     (list pkg-config))
    (home-page "https://www.retrodev.com/blastem/")
    (synopsis "Genesis/Mega Drive emulator")
    (description "Blastem is an emulator for the Sega Genesis/Mega Drive
console.")
    (license license:gpl3+)))

(define-public desmume
  (package
    (name "desmume")
    (version "0.9.13")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/TASEmulators/desmume")
                    (commit (string-append "release_"
                                           (string-replace-substring version
                                                                     "." "_")))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1ylxv0gjcxwj6dgwly2fjhyr0wrs5yazkim9nvqb8p72mxfwls5y"))))
    (build-system meson-build-system)
    (arguments
     (list #:configure-flags #~(list "-Dfrontend-cli=true"
                                     "-Dfrontend-gtk=true"
                                     "-Dgdb-stub=true"
                                     "-Dopenal=true")
           #:phases #~(modify-phases %standard-phases
                        ;; meson.build is in a subdirectory.
                        (add-after 'unpack 'chdir
                          (lambda _
                            (chdir "desmume/src/frontend/posix"))))))
    (native-inputs (list `(,glib "bin") gettext-minimal intltool pkg-config))
    (inputs (list agg
                  alsa-lib
                  gtk+
                  libpcap
                  openal
                  sdl2
                  soundtouch
                  zlib))
    (home-page "https://desmume.org/")
    (synopsis "Nintendo DS emulator")
    (description
     "DeSmuME is an emulator for the Nintendo DS handheld gaming console.")
    (license license:gpl2)))

;; Building from recent Git because the official 5.0 release no longer builds.
;; Following commits and revision numbers of beta versions listed at
;; https://dolphin-emu.org/download/.
(define-public dolphin-emu
  (let ((commit "f9deb68aee962564b1495ff04c54c015e58d086f")
        (revision "13669"))
    (package
      (name "dolphin-emu")
      (version (git-version "5.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/dolphin-emu/dolphin")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1p8qsxlabgmz3nic0a9ghh9d3lzl5f8i3kmdrrvx6w8kdlp33018"))
         (modules '((guix build utils)
                    (ice-9 regex)))
         (snippet
          '(begin
             ;; Remove external stuff we don't need.
             (for-each (lambda (dir)
                         (delete-file-recursively
                          (string-append "Externals/" dir)))
                       '("LZO" "OpenAL" "Qt" "SFML" "bzip2"
                         ;; XXX: Attempting to use the vulkan-headers package
                         ;; results in "error:
                         ;; ‘VK_PRESENT_MODE_RANGE_SIZE_KHR’ was not declared
                         ;; in this scope".
                         ;;"Vulkan"
                         "cubeb" "curl" "enet"
                         "ffmpeg" "fmt" "gettext"
                         ;; XXX: Attempting to use an unbundled glslang at the
                         ;; exact commit used by Dolphin still results in
                         ;; "error: ‘DefaultTBuiltInResource’ is not a member
                         ;; of ‘glslang’".
                         ;;"glslang"
                         ;; XXX: Googletest cannot currently easily be
                         ;; unbundled, as there are missing linking
                         ;; directives.
                         ;;"gtest"
                         "hidapi" "libpng" "libusb" "mbedtls"
                         "miniupnpc" "minizip" "MoltenVK" "pugixml"
                         "soundtouch"
                         "xxhash" "zlib" "zstd"))
             ;; Clean up the source.
             (for-each delete-file
                       (find-files
                        "."
                        (lambda (file _)
                          (and (string-match "\\.(bin|dsy|exe|jar|rar)$" file)
                               ;; Preserve the important wc24 .bin
                               ;; configuration *data* files.
                               (not (member (basename file)
                                            '("misc.bin"
                                              "nwc24dl.bin"
                                              "nwc24fl.bin"
                                              "nwc24fls.bin")))))))
             ;; Do not attempt to include now-missing directories.
             (substitute* "CMakeLists.txt"
               ((".*add_subdirectory.*Externals/enet.*") "")
               ((".*add_subdirectory.*Externals/soundtouch.*") "")
               ((".*add_subdirectory.*Externals/xxhash.*") ""))))
         (patches (search-patches "dolphin-emu-data.patch"))))
      (build-system cmake-build-system)
      (arguments
       (list
        #:phases
        #~(modify-phases %standard-phases
            (add-before 'configure 'remove-unittests-target-post-build-command
              (lambda _
                ;; To skip a few problematic tests, CTest will be manually
                ;; invoked in the post-check phase.
                (with-directory-excursion "Source/UnitTests"
                  (substitute* "CMakeLists.txt"
                    (("add_custom_command\\(TARGET unittests POST_BUILD.*")
                     "")))))
            (add-before 'configure 'generate-fonts&hardcore-libvulkan-path
              (lambda* (#:key inputs #:allow-other-keys)
                (let ((fontfile
                       (search-input-file inputs
                                          "/share/fonts/truetype/wqy-microhei.ttc"))
                      (libvulkan
                       (search-input-file inputs "/lib/libvulkan.so")))
                  (chdir "docs")
                  (invoke "bash" "-c" "g++ -O2 $(freetype-config \
--cflags --libs) gc-font-tool.cpp -o gc-font-tool")
                  (invoke "./gc-font-tool" "a" fontfile "font_western.bin")
                  (invoke "./gc-font-tool" "s" fontfile "font_japanese.bin")
                  (copy-file "font_japanese.bin" "../Data/Sys/GC/font_japanese.bin")
                  (copy-file "font_western.bin" "../Data/Sys/GC/font_western.bin")
                  (chdir "..")
                  (substitute* "Source/Core/VideoBackends/Vulkan/VulkanLoader.cpp"
                    (("\"vulkan\", 1") (string-append "\"vulkan\""))
                    (("\"vulkan\"") (string-append "\"" libvulkan "\""))
                    (("Common::DynamicLibrary::GetVersionedFilename") "")))))
            (add-after 'check 'post-check
              (lambda* (#:key tests? #:allow-other-keys)
                (when tests?
                  (with-directory-excursion "Source/UnitTests"
                    (invoke "ctest" "-V" "--output-on-failure"
                            ;; These tests fail due to libusb failing to
                            ;; init inside the build container.
                            "-E" (string-join
                                  '("MMIOTest"
                                    "PageFaultTest"
                                    "CoreTimingTest"
                                    "FileSystemTest"
                                    "PowerPCTest"
                                    "VertexLoaderTest")
                                  "|"))))))
            (add-before 'install 'build-codeloader.bin
              (lambda _
                (with-directory-excursion "../source/docs"
                  ;; The following command-line is adapted from the example in
                  ;; codehandler.s.
                  (invoke "powerpc-linux-gnu-gcc" "-mpowerpc" "-mbig"
                          "codehandler.s" "-nostartfiles" "-nodefaultlibs"
                          "-nostdlib" "-T" "codehandler.ld"
                          "-o" "codehandler.bin")
                  (copy-file "codehandler.bin" "../Data/Sys/codehandler.bin"))))
            (add-before 'install 'build-dsp_rom.bin
              (lambda _
                ;; Ensure dsptool is on PATH.
                (setenv "PATH" (string-append (getenv "PATH") ":"
                                              (getcwd) "/Binaries"))
                (with-directory-excursion "../source"
                  (invoke "dsptool" "-o" "Data/Sys/GC/dsp_rom.bin"
                          "docs/DSP/free_dsp_rom/dsp_rom.ds"))))
            (add-before 'install 'build-dsp_coefs.bin
              (lambda _
                (with-directory-excursion "../source"
                  (invoke "python3" "docs/DSP/free_dsp_rom/generate_coefs.py")
                  (rename-file "dsp_coef.bin" "Data/Sys/GC/dsp_coef.bin")))))
        ;; The FindGTK2 cmake script only checks hardcoded directories for
        ;; glib/gtk headers.  Also add some include directories via the CXX
        ;; flags to let GCC find some headers not actively searched by the
        ;; build system.
        #:configure-flags
        #~(list (string-append "-DCMAKE_CXX_FLAGS="
                               "-I" (search-input-directory
                                     %build-inputs "include/soundtouch"))
                "-DDSPTOOL=ON"
                (string-append "-DX11_INCLUDE_DIR="
                               #$(this-package-input "libx11")
                               "/include")
                (string-append "-DX11_LIBRARIES="
                               (search-input-file %build-inputs
                                                  "lib/libX11.so"))
                "-DX11_FOUND=1")
        #:test-target "unittests"))
      (native-inputs
       (list (cross-gcc "powerpc-linux-gnu")
             gettext-minimal
             pkg-config
             python-minimal
             python-numpy))
      (inputs
       (list alsa-lib
             ao
             bluez
             bzip2
             cubeb
             curl
             enet
             eudev
             ffmpeg-4
             fmt-7
             font-wqy-microhei
             freetype
             glew
             glib
             glu
             gtk+-2
             hidapi
             libevdev
             libpng
             libusb
             libx11
             libxi
             libxrandr
             lzo
             mbedtls-lts
             mesa
             miniupnpc
             minizip-ng
             openal
             pugixml
             pulseaudio
             qtbase-5
             sdl2
             sfml
             soil
             soundtouch-1/integer-samples
             vulkan-loader
             xxhash
             zlib
             `(,zstd "lib")))
      (home-page "https://dolphin-emu.org/")
      (synopsis "Nintendo Wii and GameCube emulator")
      (description
       "Dolphin is an emulator for two Nintendo video game consoles: the
GameCube and the Wii.  It provides compatibility with all PC controllers,
turbo speed, networked multiplayer, and graphical enhancements.")
      (supported-systems '("x86_64-linux" "aarch64-linux"))
      ;; dolphin/Data/Sys/GC/font_*.bin: Licensed under ASL2.0.
      (license (list license:gpl2+ license:asl2.0 license:fdl1.2+)))))

(define-public libretro-dolphin-emu
  ;; There are no tag or release; use the latest commit.
  (let ((commit "89a4df725d4eb24537728f7d655cddb1add25c18")
        (revision "0"))
    (package
      (inherit dolphin-emu)
      (name "libretro-dolphin-emu")
      (version (git-version "5.0" revision commit))
      (source (origin
                (inherit (package-source dolphin-emu))
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/libretro/dolphin")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1fvm6hy0ihc0j3sgv88a7ak08c0kyikmmiif827j981fy7zvglvz"))
                (patches (search-patches "libretro-dolphin-emu-data.patch"))))
      (arguments
       (substitute-keyword-arguments (package-arguments dolphin-emu)
         ((#:configure-flags flags ''())
          #~(cons "-DLIBRETRO=ON" #$flags))
         ((#:phases phases '%standard-phases)
          #~(modify-phases #$phases
              (add-after 'unpack 'deregister-bundled-sources
                (lambda _
                  (substitute* "CMakeLists.txt"
                    ((".*add_subdirectory.*Externals/curl.*") "")
                    ((".*add_subdirectory.*Externals/libpng.*") ""))))
              (replace 'install
                (lambda _
                  (install-file "dolphin_libretro.so"
                                (string-append #$output "/lib/libretro"))
                  ;; The system data files are also required for the proper
                  ;; functioning of dolphin; without them, it crashes with
                  ;; segmentation faults and cannot save files to the memory
                  ;; card.
                  (let ((sysdir (string-append
                                 #$output
                                 "/share/libretro/system/dolphin-emu")))
                    (mkdir-p sysdir)
                    (copy-recursively "../source/Data/Sys"
                                      (string-append sysdir "/Sys")))))))))
      (inputs
       ;; Delete large and extraneous inputs.
       (modify-inputs (package-inputs dolphin-emu)
         (delete "ffmpeg"
                 "gtk+"
                 "qtbase")))
      (synopsis "Libretro port of Dolphin, the Nintendo Wii/GameCube emulator"))))

(define-public dosbox
  (package
    (name "dosbox")
    (version "0.74-3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://sourceforge.net/projects/dosbox"
                                  "/files/dosbox/" version "/dosbox-"
                                  version ".tar.gz/download"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "02i648i50dwicv1vaql15rccv4g8h5blf5g6inv67lrfxpbkvlf0"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake))
    (inputs
     (list sdl
           libpng
           zlib
           alsa-lib
           glu
           mesa))
    (home-page "https://www.dosbox.com")
    (synopsis "X86 emulator with CGA/EGA/VGA/etc. graphics and sound")
    (description "DOSBox is a DOS-emulator that uses the SDL library.  DOSBox
also emulates CPU:286/386 realmode/protected mode, Directory
FileSystem/XMS/EMS, Tandy/Hercules/CGA/EGA/VGA/VESA graphics, a
SoundBlaster/Gravis Ultra Sound card for excellent sound compatibility with
older games.")
    (license license:gpl2+)))

(define-public dosbox-staging
  ;; This is not a patch staging area for DOSBox, but an unaffiliated fork.
  (package
    (name "dosbox-staging")
    (version "0.82.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dosbox-staging/dosbox-staging")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1s4c6fiyjm91dnmkval9fvsqszc6yjq5b6pq895xi606dn29b85d"))))
    (build-system meson-build-system)
    (arguments
     ;; XXX: When build with debugoptimized, some assertions and tests will
     ;; fail.
     (list #:build-type "release"))
    (native-inputs
     (list pkg-config))
    (inputs
     (list alsa-lib
           fluidsynth
           googletest
           iir
           libpng
           libslirp
           mesa
           mt32emu
           opusfile
           sdl2
           sdl2-image
           sdl2-net
           speexdsp
           zlib))
    (home-page "https://dosbox-staging.github.io")
    (synopsis "DOS/x86 PC emulator focusing on ease of use")
    (description
     "The DOSBox Staging project attempts to modernize DOSBox.

DOSBox emulates an Intel x86 personal computer running an IBM PC compatible disk
operating system (@dfn{DOS}) in both real and protected modes.  It was primarily
designed to run old DOS games, but aims to be fully compatible with all DOS
programs and replicate the experience as accurately as possible.

This fork fixes some perceived issues with DOSBox and adds new features such as
Wayland support, PowerPC/POWER dynamic recompilation, and FluidSynth MIDI.
Other features may be removed: for example, physical CDs can no longer be
played, only emulated media.

Graphical emulation includes contemporary text mode, Hercules, CGA, EGA, VGA,
VESA, S3@tie{}Trio@tie{}64, and Tandy hardware.

Emulated legacy sound devices range from a rudimentary `PC speaker' buzzer to
the once state-of-the-art Gravis Utrasound sampling sound card.  The default is
a SoundBlaster 16 providing 16-bit stereo sound.  MIDI is forwarded to the host
through an emulated MPU-401.

An emulated hardware modem is also included, letting one host or dial a
@acronym{BBS, Bulletin Board System} across the Internet, network over IPX, and
emulate a serial nullmodem over TCP/IP.")
    (license license:gpl3+)))

(define-public qtmips
  (package
    (name "qtmips")
    (version "0.7.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/cvut/QtMips")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1fal7a8y5g0rqqjrk795jh1l50ihz01ppjnrfjrk9vkjbd59szbp"))))
    (build-system qt-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (invoke "qmake"
                     (string-append "PREFIX=" (assoc-ref outputs "out"))
                     "qtmips.pro")))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (substitute* "tests/test.sh"
               (("qtchooser.*") ""))
             (substitute* '("tests/cpu_trap/test.sh"
                            "tests/registers/test.sh")
               (("sub-qtmips_cli") "qtmips_cli"))
             (if tests?
               (invoke "tests/run-all.sh")
               #t)))
         (replace 'install
           ;; There is no install target.
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (apps (string-append out "/share/applications"))
                    (icons (string-append out "/share/icons/hicolor")))
               (install-file "qtmips_gui/qtmips_gui" bin)
               (install-file "qtmips_cli/qtmips_cli" bin)
               (install-file "data/qtmips.desktop" apps)
               (install-file "data/icons/qtmips_gui.svg"
                             (string-append icons "/scalable/apps"))
               (install-file "data/icons/qtmips_gui.png"
                             (string-append icons "/48x48/apps"))
               #t))))
       #:tests? #f))    ; test suite wants mips toolchain
    (inputs
     (list elfutils qtbase-5 qtwayland-5))
    (home-page "https://github.com/cvut/QtMips")
    (synopsis "MIPS CPU emulator")
    (description "This package contains a MIPS CPU emulator.  The simulator
accepts ELF statically linked executables compiled for 32-bit big-endian
MIPS target, targeting mips-linux-gnu or mips-elf.")
    (license license:gpl2+)))   ; License file says GPL3

(define-public emulation-station
  ;; No release for a long time, new commits fix build issues
  (let ((commit "9cc42adff67946175d2b7e25c6ae69cc374e98a0")
        (revision "1"))
    (package
      (name "emulation-station")
      (version (git-version "2.0.1" revision commit))
      (source (origin
                (method git-fetch) ; no tarball available
                (uri (git-reference
                      (url "https://github.com/Aloshi/EmulationStation")
                      (commit commit))) ; no version tag
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1cva0ns650v17lfn8in095zci6lc43d23f1x3mlzc41qfqa6mbd1"))))
      (build-system cmake-build-system)
      (arguments
       '(#:tests? #f)) ; no tests
      (inputs
       `(("alsa-lib" ,alsa-lib)
         ("boost" ,boost)
         ("curl" ,curl)
         ("eigin" ,eigen)
         ("freeimage" ,freeimage)
         ("freetype" ,freetype)
         ("mesa" ,mesa)
         ("sdl2" ,sdl2)))
      (synopsis "Video game console emulator front-end")
      (description "EmulationStation provides a graphical front-end to a large
number of video game console emulators.  It features an interface that is
usable with any game controller that has at least 4 buttons, theming support,
and a game metadata scraper.")
      (home-page "https://emulationstation.org")
      (license license:expat))))

(define-public higan
  (package
    (name "higan")
    (version "110")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/higan-emu/higan")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "11rvm53c3p2f6zk8xbyv2j51xp8zmqnch7zravhj3fk590qrjrr2"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("bash" ,bash-minimal) ; for wrap-program
       ("ao" ,ao)
       ("eudev" ,eudev)
       ("gtk+" ,gtk+-2)
       ("gtksourceview-2" ,gtksourceview-2)
       ("libxrandr" ,libxrandr)
       ("libxv" ,libxv)
       ("mesa" ,mesa)
       ("openal" ,openal)
       ("pulseaudio" ,pulseaudio)
       ("sdl2" ,sdl2)))
    (arguments
     '(#:phases
       (let ((build-phase (assoc-ref %standard-phases 'build))
             (install-phase (assoc-ref %standard-phases 'install)))
         (modify-phases %standard-phases
           ;; The higan build system has no configure phase.
           (delete 'configure)
           (add-before 'build 'chdir-to-higan
             (lambda _
               (chdir "higan")
               #t))
           (add-before 'install 'create-/share/applications
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 ;; It seems the author forgot to do this in the Makefile.
                 (mkdir-p (string-append out "/share/applications"))
                 #t)))
           (add-after 'install 'chdir-to-icarus
             (lambda _
               (chdir "../icarus")
               #t))
           (add-after 'chdir-to-icarus 'build-icarus build-phase)
           (add-after 'build-icarus 'install-icarus install-phase)
           (add-after 'install-icarus 'wrap-higan-executable
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (bin (string-append out "/bin"))
                      (higan (string-append bin "/higan"))
                      (higan-original (string-append higan "-original"))
                      (bash (search-input-file inputs "/bin/bash"))
                      (coreutils (assoc-ref inputs "coreutils"))
                      (mkdir (string-append coreutils "/bin/mkdir"))
                      (cp (string-append coreutils "/bin/cp"))
                      (cp-r (string-append cp " -r --no-preserve=mode")))
                 ;; First, have the executable make sure ~/.local/share/higan
                 ;; contains up to date files.  Higan insists on looking there
                 ;; for these data files.
                 (rename-file higan higan-original)
                 (with-output-to-file higan
                   (lambda ()
                     (display
                      (string-append
                       "#!" bash "\n"
                       ;; higan doesn't respect $XDG_DATA_HOME
                       mkdir " -p ~/.local/share\n"
                       cp-r " " out "/share/higan ~/.local/share\n"
                       "exec " higan-original))))
                 (chmod higan #o555)
                 ;; Second, make sure higan will find icarus in PATH.
                 (wrap-program higan
                   `("PATH" ":" prefix (,bin)))
                 #t)))))
       #:make-flags
       (list "compiler=g++"
             (string-append "prefix=" (assoc-ref %outputs "out")))
       ;; There is no test suite.
       #:tests? #f))
    (home-page "https://github.com/higan-emu/higan/")
    (synopsis "Multi-system emulator")
    (description
     "higan is a multi-system emulator with an uncompromising focus on
accuracy and code readability.

It currently emulates the following systems: Famicom, Famicom Disk System,
Super Famicom, Super Game Boy, Game Boy, Game Boy Color, Game Boy Advance,
Game Boy Player, SG-1000, SC-3000, Master System, Game Gear, Mega Drive, Mega
CD, PC Engine, SuperGrafx, MSX, MSX2, ColecoVision, Neo Geo Pocket, Neo Geo
Pocket Color, WonderSwan, WonderSwan Color, SwanCrystal, Pocket Challenge
V2.")
    (license license:gpl3+)))

(define-public mednafen
  (package
    (name "mednafen")
    (version "1.32.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://mednafen.github.io/releases/files/"
                           "mednafen-" version ".tar.xz"))
       (sha256
        (base32 "0ciqr3dlf1b3r8jncy9k9cihiclai8v28r9pb1vsw4k2nr5bjzny"))))
    (build-system gnu-build-system)
    (arguments
     (list #:configure-flags
           #~(list
              ;; "--with-external-mpcdec"
              "--with-external-lzo")))
    (native-inputs
     (list pkg-config))
    (inputs
     (list alsa-lib
           flac
           ;; libmpcdec ;FIXME: not recognized.
           libsndfile
           lzo
           sdl2
           zlib))
    (home-page "https://mednafen.github.io/")
    (synopsis "Multi-system emulator utilizing OpenGL and SDL")
    (description
     "Mednafen is a portable, utilizing OpenGL and SDL, argument-driven
multi-system emulator.  Mednafen has the ability to remap hotkey functions and
virtual system inputs to a keyboard, a joystick, or both simultaneously.  Save
states are supported, as is real-time game rewinding.  Screen snapshots may be
taken, in the PNG file format, at the press of a button.  Mednafen can record
audiovisual movies in the QuickTime file format, with several different
lossless codecs supported.

The following systems are supported:

@itemize
@item Apple II/II+
@item Atari Lynx
@item Neo Geo Pocket (Color)
@item WonderSwan
@item GameBoy (Color)
@item GameBoy Advance
@item Nintendo Entertainment System
@item Super Nintendo Entertainment System/Super Famicom
@item Virtual Boy
@item PC Engine/TurboGrafx 16 (CD)
@item SuperGrafx
@item PC-FX
@item Sega Game Gear
@item Sega Genesis/Megadrive
@item Sega Master System
@item Sega Saturn (experimental, x86_64 only)
@item Sony PlayStation
@end itemize")
    ;; Main license is GPL2+.  Some parts are BSD-3.
    (license (list license:gpl2+ license:bsd-3))))

(define-public mgba
  (package
    (name "mgba")
    (version "0.10.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mgba-emu/mgba")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0lfn5jhgqb06f1i1b8w8fvbi4fy4k8dvialblwg8h49qjqmf610q"))
       (modules '((guix build utils)))
       (snippet
        ;; Make sure we don't use the bundled software.
        '(begin
           (for-each
            (lambda (subdir)
              (let ((lib-subdir (string-append "src/third-party/" subdir)))
                (delete-file-recursively lib-subdir)))
            '("libpng" "lzma" "sqlite3" "zlib"))))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f                      ;no "test" target
       #:configure-flags
       (list "-DBUILD_LTO=OFF" ;FIXME: <https://github.com/mgba-emu/mgba/issues/3115>
             "-DUSE_LZMA=OFF"           ;do not use bundled LZMA
             "-DUSE_LIBZIP=OFF")))      ;use "zlib" instead
    (native-inputs (list pkg-config qttools-5))
    (inputs
     (list ffmpeg
           libedit
           libelf
           libepoxy
           libpng
           mesa
           minizip
           ncurses
           qtbase-5
           qtmultimedia-5
           sdl2
           sqlite
           zlib))
    (home-page "https://mgba.io")
    (synopsis "Game Boy Advance emulator")
    (description
     "mGBA is an emulator for running Game Boy Advance games.  It aims to be
faster and more accurate than many existing Game Boy Advance emulators, as
well as adding features that other emulators lack.  It also supports Game Boy
and Game Boy Color games.")
    ;; Code is mainly MPL 2.0. "blip_buf.c" is LGPL 2.1+, "inih.c" is
    ;; BSD-3, and "discord-rpc" is Expat.
    (license (list license:mpl2.0 license:lgpl2.1+ license:bsd-3 license:expat))))

(define-public sameboy
  (package
    (name "sameboy")
    (version "0.16.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/LIJI32/SameBoy")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1jdjg59vzzkbi3c3qaxpsxqx955sb86cd3kcypb0nhjxbnwac1di"))))
    (build-system gnu-build-system)
    (native-inputs
     (list rgbds pkg-config))
    (inputs
     (list sdl2))
    (arguments
     `(#:tests? #f                      ; There are no tests
       #:make-flags `(,(string-append "CC=" ,(cc-for-target))
                      "NATIVE_CC=gcc" "CONF=release"
                      ,(string-append "DATA_DIR="
                                      (assoc-ref %outputs "out")
                                      "/share/sameboy/"))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (data (string-append out "/share/sameboy/")))
               (with-directory-excursion "build/bin/SDL"
                 (install-file "sameboy" bin)
                 (delete-file "sameboy")
                 (copy-recursively "." data))))))))
    (home-page "https://sameboy.github.io/")
    (synopsis "Accurate Game Boy, Game Boy Color and Super Game Boy emulator")
    (description "SameBoy is a user friendly Game Boy, Game Boy Color
and Super Game Boy emulator.  SameBoy is accurate and includes a wide
range of debugging features.  It has all the features one would expect
from an emulator---from save states to scaling filters.")
    (license license:expat)))

(define-public mupen64plus-core
  (package
    (name "mupen64plus-core")
    (version "2.5.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mupen64plus/mupen64plus-core")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1iav8r3f0r44sq9pz4zjqrdzyspk412c117ywxz02qpjkhkf91a3"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config nasm which))
    (inputs
     (list freetype
           glu
           libpng
           mesa
           sdl2
           zlib))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         ;; The mupen64plus build system has no configure phase.
         (replace 'configure
           (lambda _
             (substitute* "projects/unix/Makefile"
               (("\\$\\(CFLAGS\\)")
                "$(CFLAGS) -fcommon"))))
         ;; Makefile is in a subdirectory.
         (add-before 'build 'chdir-to-project-directory
           (lambda _ (chdir "projects/unix"))))
       #:make-flags (let ((out (assoc-ref %outputs "out")))
                      (list "all" (string-append "PREFIX=" out)))
       ;; There are no tests.
       #:tests? #f))
    ;; As per the Makefile (in projects/unix/Makefile):
    (supported-systems '("i686-linux" "x86_64-linux"))
    (home-page "https://www.mupen64plus.org/")
    (synopsis "Nintendo 64 emulator core library")
    (description
     "Mupen64Plus is a cross-platform plugin-based Nintendo 64 (N64) emulator
which is capable of accurately playing many games.  This package contains the
core library.")
    (license license:gpl2+)))

(define-public mupen64plus-audio-sdl
  (package
    (name "mupen64plus-audio-sdl")
    (version "2.5.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mupen64plus/mupen64plus-audio-sdl")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0j78xk78fj7lhi6jk6npr7wm9ix7qyr5cbaaqsmk6pqw6gfx81kz"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config which))
    (inputs
     (list mupen64plus-core sdl2))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         ;; The mupen64plus build system has no configure phase.
         (delete 'configure)
         ;; Makefile is in a subdirectory.
         (add-before
          'build 'cd-to-project-dir
          (lambda _
            (chdir "projects/unix"))))
       #:make-flags
       (let ((out (assoc-ref %outputs "out"))
             (m64p (assoc-ref %build-inputs "mupen64plus-core")))
         (list "all"
               (string-append "PREFIX=" out)
               (string-append "APIDIR=" m64p "/include/mupen64plus")))
       ;; There are no tests.
       #:tests? #f))
    (home-page "https://www.mupen64plus.org/")
    (synopsis "Mupen64Plus SDL audio plugin")
    (description
     "Mupen64Plus is a cross-platform plugin-based Nintendo 64 (N64) emulator
which is capable of accurately playing many games.  This package contains the
SDL audio plugin.")
    (license license:gpl2+)))

(define-public mupen64plus-input-sdl
  (package
    (name "mupen64plus-input-sdl")
    (version "2.5.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mupen64plus/mupen64plus-input-sdl")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1nnniyiy0wpg4m9918va31xxnz8r5qvj0z08vyq2is0b47ld3kq0"))))
    (build-system gnu-build-system)
    (native-inputs
     (list which))
    (inputs
     (list mupen64plus-core sdl2))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         ;; The mupen64plus build system has no configure phase.
         (delete 'configure)
         ;; Makefile is in a subdirectory.
         (add-before
          'build 'cd-to-project-dir
          (lambda _
            (chdir "projects/unix"))))
       #:make-flags
       (let ((out (assoc-ref %outputs "out"))
             (m64p (assoc-ref %build-inputs "mupen64plus-core")))
         (list "all"
               (string-append "PREFIX=" out)
               (string-append "APIDIR=" m64p "/include/mupen64plus")))
       ;; There are no tests.
       #:tests? #f))
    (home-page "https://www.mupen64plus.org/")
    (synopsis "Mupen64Plus SDL input plugin")
    (description
     "Mupen64Plus is a cross-platform plugin-based Nintendo 64 (N64) emulator
which is capable of accurately playing many games.  This package contains the
SDL input plugin.")
    (license license:gpl2+)))

(define-public mupen64plus-rsp-hle
  (package
    (name "mupen64plus-rsp-hle")
    (version "2.5.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mupen64plus/mupen64plus-rsp-hle")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0sblabl3dp1jy9izbwyhx90690xdj96yfmwi47kpka8axzj93naq"))))
    (build-system gnu-build-system)
    (inputs
     (list mupen64plus-core))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         ;; The mupen64plus build system has no configure phase.
         (delete 'configure)
         ;; Makefile is in a subdirectory.
         (add-before
          'build 'cd-to-project-dir
          (lambda _
            (chdir "projects/unix"))))
       #:make-flags
       (let ((out (assoc-ref %outputs "out"))
             (m64p (assoc-ref %build-inputs "mupen64plus-core")))
         (list "all"
               (string-append "PREFIX=" out)
               (string-append "APIDIR=" m64p "/include/mupen64plus")))
       ;; There are no tests.
       #:tests? #f))
    (home-page "https://www.mupen64plus.org/")
    (synopsis "Mupen64Plus RSP high-level emulation (HLE) plugin")
    (description
     "Mupen64Plus is a cross-platform plugin-based Nintendo 64 (N64) emulator
which is capable of accurately playing many games.  This package contains the
high-level emulation (HLE) RSP processor plugin.")
    (license license:gpl2+)))

(define-public mupen64plus-rsp-z64
  (package
    (name "mupen64plus-rsp-z64")
    (version "2.5.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mupen64plus/mupen64plus-rsp-z64")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "02w7c6b7fc6q5rrvawxv48xp64crfs5jbs06f2fqqj4smysyjfcc"))))
    (build-system gnu-build-system)
    (inputs
     (list mupen64plus-core))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         ;; The mupen64plus build system has no configure phase.
         (delete 'configure)
         ;; Makefile is in a subdirectory.
         (add-before
          'build 'cd-to-project-dir
          (lambda _
            (chdir "projects/unix"))))
       #:make-flags
       (let ((out (assoc-ref %outputs "out"))
             (m64p (assoc-ref %build-inputs "mupen64plus-core")))
         (list "all"
               (string-append "PREFIX=" out)
               (string-append "APIDIR=" m64p "/include/mupen64plus")))
       ;; There are no tests.
       #:tests? #f))
    (home-page "https://www.mupen64plus.org/")
    (synopsis "Mupen64Plus RSP Z64 plugin")
    (description
     "Mupen64Plus is a cross-platform plugin-based Nintendo 64 (N64) emulator
which is capable of accurately playing many games.  This package contains the
Z64 RSP processor plugin.")
    (license license:gpl2+)))

(define-public mupen64plus-video-arachnoid
  (package
    (name "mupen64plus-video-arachnoid")
    (version "2.5.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mupen64plus/mupen64plus-video-arachnoid")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1bkzbmg53qiwvza9h45d76rbyn0isbb31cfp5qqza0fzmgjxhv1d"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config which))
    (inputs
     (list mesa mupen64plus-core))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         ;; The mupen64plus build system has no configure phase.
         (delete 'configure)
         ;; Makefile is in a subdirectory.
         (add-before
          'build 'cd-to-project-dir
          (lambda _
            (chdir "projects/unix"))))
       #:make-flags
       (let ((out (assoc-ref %outputs "out"))
             (m64p (assoc-ref %build-inputs "mupen64plus-core")))
         (list "all"
               (string-append "PREFIX=" out)
               (string-append "APIDIR=" m64p "/include/mupen64plus")))
       ;; There are no tests.
       #:tests? #f))
    (home-page "https://www.mupen64plus.org/")
    (synopsis "Mupen64Plus Arachnoid video plugin")
    (description
     "Mupen64Plus is a cross-platform plugin-based Nintendo 64 (N64) emulator
which is capable of accurately playing many games.  This package contains the
Arachnoid video plugin.")
    (license license:gpl2+)))

(define-public mupen64plus-video-glide64
  (package
    (name "mupen64plus-video-glide64")
    (version "2.5.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mupen64plus/mupen64plus-video-glide64")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0jscvr2imm9wj9jsgsp5815pv27f97w8g19ix0n39y9yy851qvrg"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config which))
    (inputs
     (list mesa mupen64plus-core sdl2))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         ;; The mupen64plus build system has no configure phase.
         (delete 'configure)
         ;; Makefile is in a subdirectory.
         (add-before 'build 'cd-to-project-dir
          (lambda _
            (chdir "projects/unix"))))
       #:make-flags
       (let ((out (assoc-ref %outputs "out"))
             (m64p (assoc-ref %build-inputs "mupen64plus-core")))
         (list "all"
               (string-append "PREFIX=" out)
               (string-append "APIDIR=" m64p "/include/mupen64plus")))
       ;; There are no tests.
       #:tests? #f))
    (home-page "https://www.mupen64plus.org/")
    (synopsis "Mupen64Plus Glide64 video plugin")
    (description
     "Mupen64Plus is a cross-platform plugin-based Nintendo 64 (N64) emulator
which is capable of accurately playing many games.  This package contains the
Glide64 video plugin.")
    (license license:gpl2+)))

(define-public mupen64plus-video-glide64mk2
  (package
    (name "mupen64plus-video-glide64mk2")
    (version "2.5.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mupen64plus/mupen64plus-video-glide64mk2")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1hr0mv6y7v72101iff3zf6rd0wpqah936234m3hcb4cgna6zj9xy"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config which))
    (inputs
     (list boost
           libpng
           mesa
           mupen64plus-core
           sdl2
           zlib))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         ;; The mupen64plus build system has no configure phase.
         (delete 'configure)
         ;; Makefile is in a subdirectory.
         (add-before
          'build 'cd-to-project-dir
          (lambda _
            (chdir "projects/unix"))))
       #:make-flags
       (let ((out (assoc-ref %outputs "out"))
             (m64p (assoc-ref %build-inputs "mupen64plus-core")))
         (list "all"
               (string-append "PREFIX=" out)
               (string-append "APIDIR=" m64p "/include/mupen64plus")))
       ;; There are no tests.
       #:tests? #f))
    (home-page "https://www.mupen64plus.org/")
    (synopsis "Mupen64Plus Glide64MK2 video plugin")
    (description
     "Mupen64Plus is a cross-platform plugin-based Nintendo 64 (N64) emulator
which is capable of accurately playing many games.  This package contains the
Glide64MK2 video plugin.")
    (license license:gpl2+)))

(define-public mupen64plus-video-rice
  (package
    (name "mupen64plus-video-rice")
    (version "2.5.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mupen64plus/mupen64plus-video-rice")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1vn24g7ahyv70jd06f5sq0j4bjs4axl2c0kfz4qdkpqsamsgxng8"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config which))
    (inputs
     (list libpng mesa mupen64plus-core sdl2))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         ;; The mupen64plus build system has no configure phase.
         (delete 'configure)
         ;; Makefile is in a subdirectory.
         (add-before
          'build 'cd-to-project-dir
          (lambda _
            (chdir "projects/unix"))))
       #:make-flags
       (let ((out (assoc-ref %outputs "out"))
             (m64p (assoc-ref %build-inputs "mupen64plus-core")))
         (list "all"
               (string-append "PREFIX=" out)
               (string-append "APIDIR=" m64p "/include/mupen64plus")))
       ;; There are no tests.
       #:tests? #f))
    (home-page "https://www.mupen64plus.org/")
    (synopsis "Mupen64Plus Rice video plugin")
    (description
     "Mupen64Plus is a cross-platform plugin-based Nintendo 64 (N64) emulator
which is capable of accurately playing many games.  This package contains the
Rice Video plugin.")
    (license license:gpl2+)))

(define-public mupen64plus-video-z64
  (package
    (name "mupen64plus-video-z64")
    (version "2.5.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mupen64plus/mupen64plus-video-z64")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1i8dxa0lhcsm5ss1bf74dqnzaa2bw5naj6f56ixw2qjvybrnsmk2"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config which))
    (inputs
     (list glew mupen64plus-core sdl2))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         ;; The mupen64plus build system has no configure phase.
         (delete 'configure)
         ;; Makefile is in a subdirectory.
         (add-before
          'build 'cd-to-project-dir
          (lambda _
            (chdir "projects/unix")))
         ;; XXX Should be unnecessary with the next release.
         (add-before
          'build 'use-sdl2
          (lambda _
            (substitute* "Makefile"
              (("SDL_CONFIG = (.*)sdl-config" all prefix)
               (string-append "SDL_CONFIG = " prefix "sdl2-config"))))))
       #:make-flags
       (let ((out (assoc-ref %outputs "out"))
             (m64p (assoc-ref %build-inputs "mupen64plus-core")))
         (list "all"
               (string-append "PREFIX=" out)
               (string-append "APIDIR=" m64p "/include/mupen64plus")))
       ;; There are no tests.
       #:tests? #f))
    (home-page "https://www.mupen64plus.org/")
    (synopsis "Mupen64Plus Z64 video plugin")
    (description
     "Mupen64Plus is a cross-platform plugin-based Nintendo 64 (N64) emulator
which is capable of accurately playing many games.  This package contains the
Z64 video plugin.")
    (license license:gpl2+)))

(define-public mupen64plus-ui-console
  (package
    (name "mupen64plus-ui-console")
    (version "2.5.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mupen64plus/mupen64plus-ui-console")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1dyrqdfs2jkalfd86bqidgd9y1hy03qgrgwk46d3xf3kyfmaa1cq"))
       (patches (search-patches "mupen64plus-ui-console-notice.patch"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config which))
    (inputs
     (list sdl2))
    ;; Mupen64Plus supports a single data directory and a single plugin
    ;; directory in its configuration, yet we need data and plugin files from
    ;; a variety of packages.  The best way to deal with this is to install
    ;; all packages from which data and plugin files are needed into one's
    ;; profile, and point the configuration there.  Hence, propagate the most
    ;; important packages here to save the user from the bother.  The patch
    ;; mupen64plus-ui-console-notice also gives users instructions on what
    ;; they need to do in order to point the configuration to their profile.
    (propagated-inputs
     (list mupen64plus-core
           mupen64plus-audio-sdl
           mupen64plus-input-sdl
           mupen64plus-rsp-hle
           mupen64plus-video-glide64mk2
           mupen64plus-video-rice))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          ;; The mupen64plus build system has no configure phase.
          (delete 'configure)
          ;; Makefile is in a subdirectory.
          (add-before 'build 'cd-to-project-dir
            (lambda _
              (chdir "projects/unix"))))
      #:make-flags
      #~(let ((m64p #$(this-package-input "mupen64plus-core")))
          (list "all"
                (string-append "PREFIX=" #$output)
                (string-append "APIDIR=" m64p "/include/mupen64plus")
                ;; Trailing slash matters here.
                (string-append "COREDIR=" m64p "/lib/")))
      ;; There are no tests.
      #:tests? #f))
    (home-page "https://www.mupen64plus.org/")
    (synopsis "Mupen64Plus command line user interface")
    (description
     "Mupen64Plus is a cross-platform plugin-based Nintendo 64 (N64) emulator
which is capable of accurately playing many games.  This package contains the
command line user interface.  Installing this package is the easiest way
towards a working Mupen64Plus for casual users.")
    (license license:gpl2+)))

(define-public mupen64plus-video-gliden64
  ;; The latest release is 5 years old, doesn't build with GCC 11.
  (let ((commit "b021d8ee437266cfdd7251daf8c23203578b02b6")
        (revision "0"))
    (package
      (name "mupen64plus-video-gliden64")
      (version (git-version "4.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/gonetz/GLideN64")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0kcx5m8fjgrdi2dby8qbmkl78picip3jx7hg0ah1cazk192v2x98"))
         (modules '((guix build utils)))
         (snippet '(begin
                     ;; Delete 20 MiB of Windows-related files.
                     (delete-file-recursively "projects/msvc")
                     ;; Delete bundled library headers.
                     (delete-file-recursively "src/GLideNHQ/inc") ;zlib, libpng
                     (delete-file-recursively "src/inc/freetype")
                     ;; Unbundle xxhash.
                     (delete-file-recursively "src/xxHash")
                     (with-fluids ((%default-port-encoding "ISO-8859-1"))
                       (substitute* (find-files "." "\\.cpp$")
                         (("#include \"xxHash/xxhash.h\"")
                          "#include <xxhash.h>")))))))
      (build-system cmake-build-system)
      (arguments
       (list
        #:tests? #f                     ;no test suite
        #:configure-flags
        #~(list "-DMUPENPLUSAPI=ON"
                "-DUSE_SYSTEM_LIBS=ON"
                ;; Enable some optimizations.
                "-DVEC4_OPT=ON"
                #$(if (target-x86?)
                      ;; FIXME: Disabled for now as it causes a segmentation
                      ;; fault (see:
                      ;; https://github.com/gonetz/GLideN64/issues/2836).
                      "-DX86_OPT=OFF"    ;extra X86 ASM optimizations
                      "-DX86_OPT=OFF")
                #$(if (target-arm?)
                      "-DNEON_OPT=ON"
                      "-DNEON_OPT=OFF")
                #$(if (target-aarch64?)
                      "-DCRC_ARMV8=ON"  ;use ARMv8 hardware to compute CRCs
                      "-DCRC_OPT=ON"))  ;use xxHash to compute CRCs)
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'chdir
              ;; The src/ subdirectory contains the root CMakeLists.txt file.
              (lambda _
                (chdir "src")))
            (add-after 'chdir 'generate-Revision.h
              (lambda _
                (invoke "sh" "getRevision.sh" "--nogit"))))))
      (inputs (list freetype libpng mesa xxhash zlib))
      (home-page "https://github.com/gonetz/GLideN64")
      (synopsis "Mupen64Plus GlideN64 video plugin")
      (description "GLideN64 is a new generation graphics plugin for Nintendo
64 emulators, which offers better performance and compatibility compared to
the original Glide64 plugin.  This version is built for use with the
Mupen64Plus emulator.")
      (license license:gpl2+))))

(define-public nestopia-ue
  (package
    (name "nestopia-ue")
    (version "1.51.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rdanbrook/nestopia")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1g19gz33jav00rwzkpcnynf5ps41vl64a9qx0xjd6lva4bgn8s57"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf autoconf-archive automake pkg-config))
    (inputs
     `(("fltk" ,fltk)
       ("fontconfig" ,fontconfig)
       ("libarchive" ,libarchive)
       ("libepoxy" ,libepoxy)
       ("libxft" ,libxft)
       ("libxrender" ,libxrender)
       ("sdl2" ,sdl2)
       ("zlib" ,zlib)))
    (arguments
     '(;; There are no tests.
       #:tests? #f))
    (home-page "http://0ldsk00l.ca/nestopia/")
    (synopsis "Nintendo Entertainment System (NES/Famicom) emulator")
    (description
     "Nestopia UE (Undead Edition) is a fork of the Nintendo Entertainment
System (NES/Famicom) emulator Nestopia, with enhancements from members of the
emulation community.  It provides highly accurate emulation.")
    (license license:gpl2+)))

(define (make-libretro-beetle-psx name hw)
  (let ((commit "80d3eba272cf6efab6b76e4dc44ea2834c6f910d")
	(revision "0"))
   (package
    (name name)
    ;; Use Mednafen core version as base. Defined in libretro_options.h:10
    (version (git-version "0.9.44.1" revision commit))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/libretro/beetle-psx-libretro")
             (commit commit)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "14kkrlqhv9pqmbqlv8vvcp0ps938dmg8pk47d7zzc8piq51hkawk"))))
    (build-system gnu-build-system)
    (arguments
     (list #:make-flags #~(list (string-append "HAVE_HW=" #$(if hw "1" "0"))
                                (string-append "CC=" #$(cc-for-target))
                                (string-append "GIT_VERSION=" #$commit)
                                (string-append "prefix=" #$output))
           #:tests? #f                  ;no tests
           #:phases #~(modify-phases %standard-phases
                        (delete 'configure)
			(replace 'install ;there is no install target
			  (lambda* (#:key outputs #:allow-other-keys)
			    (let* ((libretro (string-append
					      (assoc-ref outputs "out")
					      "/lib/libretro")))
			      (install-file (string-append "mednafen_psx_"
                                                           #$(if hw "hw_" "")
                                                           "libretro.so")
					    libretro)))))))
    (inputs (list mesa))
    (home-page "https://github.com/libretro/beetle-psx-libretro")
    (synopsis "Standalone port of Mednafen PSX to libretro")
    (description
     "Beetle PSX is a port/fork of Mednafen's PSX module to the libretro
API.  Additional features include PBP/CHD file format support,
high-resolution software rendering, OpenGL and Vulkan renderers, and
PGXP perspective correct texturing.  For those seeking improved visuals
and performance, Beetle PSX HW provides a hardware-accelerated alternative
with its OpenGL and Vulkan renderer.")
    (license license:gpl2+))))

(define-public libretro-beetle-psx
  (make-libretro-beetle-psx "libretro-beetle-psx" #f))

(define-public libretro-beetle-psx-hw
  (make-libretro-beetle-psx "libretro-beetle-psx-hw" #t))

(define-public libretro-lowresnx
  (package
    (name "libretro-lowresnx")
    (version "1.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/timoinutilis/lowres-nx")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0b0vg3iz342dpkffvf7frsnqh8inj8yzi8550bsx8vnbpq5r2ay5"))))
    (build-system gnu-build-system)
    (arguments
     (list #:tests? #f                  ;no test suite
           #:make-flags #~(list "-C" "platform/LibRetro"
                                (string-append "CC=" #$(cc-for-target)))
           #:phases
           #~(modify-phases %standard-phases
               (delete 'configure)      ;no configure script
               (replace 'install
                 (lambda _
                   (install-file "platform/LibRetro/lowresnx_libretro.so"
                                 (string-append #$output "/lib/libretro")))))))
    (home-page "https://lowresnx.inutilis.com/")
    (synopsis "Libretro core for LowRES NX")
    (description "LowRES NX is a simulated retro game console, which can be
programmed in the classic BASIC language.  This package provides a libretro
core allowing the lowRES NX programs to be used with libretro frontends such
as RetroArch.")
    (license license:zlib)))

(define-public libretro-mupen64plus-nx
  ;; There are no proper release; use the latest commit of the master branch
  ;; (their stable branch).
  (let ((commit "9d940bacb95c4d86733f42b67b57fc83046a6d39")
        (revision "0"))
    (package
      (name "libretro-mupen64plus-nx")
      (version (git-version "0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/libretro/mupen64plus-libretro-nx")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0s3l62mfkbzmv8g1y4r40iayfwdz68rq6l6khc0d8kw08qk7ggl9"))))
      (build-system gnu-build-system)
      (arguments
       (list
        #:tests? #f                     ;no test suite
        #:make-flags
        #~(list (string-append "CC=" #$(cc-for-target))
                (string-append "CXX=" #$(cxx-for-target))
                (string-append "GIT_VERSION=" #$version)
                (string-append "PREFIX=" #$output)
                "LLE=1"
                "HAVE_THR_AL=1"         ;for the angrylion video plugin
                "HAVE_PARALLEL_RDP=1"
                "HAVE_PARALLEL_RSP=1"
                "SYSTEM_MINIZIP=1"
                "SYSTEM_LIBPNG=1"
                "SYSTEM_XXHASH=1"
                "SYSTEM_ZLIB=1")
        #:phases
        #~(modify-phases %standard-phases
            (delete 'configure)
            (replace 'install
              (lambda _
                (install-file "mupen64plus_next_libretro.so"
                              (string-append #$output "/lib/libretro/")))))))
      (native-inputs (list nasm pkg-config))
      (inputs (list mesa libpng minizip unzip xxhash zlib))
      (home-page "https://github.com/libretro/mupen64plus-libretro-nx")
      (synopsis "Improved Mupen64Plus libretro core")
      (description "Mupen64Plus-Next is a N64 emulation library for the
libretro API, based on Mupen64Plus.  It incorporates the following projects:
@itemize
@item @url{https://github.com/mupen64plus/mupen64plus-core, mupen64plus}
@item @url{https://github.com/gonetz/GLideN64, GLideN64}
@item @url{https://github.com/cxd4/rsp, cxd4}
@item @url{https://github.com/Themaister/parallel-rsp, parallel-rsp}
@item @url{https://github.com/ata4/angrylion-rdp-plus, angrylion-rdp-plus}
@end itemize")
      (license license:gpl2+))))

(define-public retroarch-assets
  (package
    (name "retroarch-assets")
    (version "1.19.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/libretro/retroarch-assets")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1i496x0lkqard5i9045yf438kivwd6f6za8p9fil8w1rfrhk2knz"))))
    (build-system gnu-build-system)
    (arguments
     (list #:tests? #f                  ;no test suite
           #:make-flags #~(list (string-append "PREFIX=" #$output))
           #:phases #~(modify-phases %standard-phases
                        (delete 'build)))) ;no compilation required
    (home-page "https://www.libretro.com/")
    (synopsis "RetroArch menu assets")
    (description "The RetroArch assets are the user interface elements used to
generate the various User Experience (UX) environments.")
    (license license:cc-by4.0)))

(define-public libretro-core-info
  (package
    (name "libretro-core-info")
    (version "1.20.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/libretro/libretro-core-info")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1rfvp0lkv99jgpfyb9pp6vrh1i1974p3lckh93y1bibdizyxmwjg"))))
    (build-system copy-build-system)
    (arguments
     (list #:install-plan #~'(("." "lib/libretro/"
                               #:include-regexp ("\\.info$")))))
    (home-page "https://github.com/libretro/libretro-core-info")
    (synopsis "Libretro core info files")
    (description "This is a versioned snapshot of the files containing
metadata about each known libretro core.  The snapshot is taken from the
@url{https://github.com/libretro/libretro-super, libretro-super} repository.")
    (license license:expat)))

(define-public retroarch-core-info
  (deprecated-package "retroarch-core-info" libretro-core-info))

(define-public libretro-database
  (package
    (name "libretro-database")
    (version "1.20.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/libretro/libretro-database")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "086a9grpd1irsdns2zx3hlna72bbrmsfra4r498wi4ia9zf8nb0p"))))
    (build-system gnu-build-system)
    (arguments (list #:tests? #f
                     #:make-flags #~(list (string-append "PREFIX=" #$output))))
    (home-page "https://github.com/libretro/libretro-database/")
    (synopsis "Cheat codes and content data files for RetroArch")
    (description "RetroArch incorporates a ROM scanning system to
automatically produce playlists.  Each ROM that is scanned by the playlist
generator is checked against a database of ROMs that are known to be good
copies.  The various directories contain:
@table @code
@item cht
Cheat codes for various games
@item cursors
Methods for querying the playlists
@item dat
Customized DAT files, maintained by the libretro team
@item metadat
Different metadata and third-party DATs available to the system
@item rdb
The compiled RetroArch database files
@item scripts
Various scripts that are used to maintain the database files.
@end table")
    (license license:cc-by-sa4.0)))

(define-public retroarch-joypad-autoconfig
  (package
    (name "retroarch-joypad-autoconfig")
    (version "1.20.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/libretro/retroarch-joypad-autoconfig")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0nlz3j3575dlv9s15250qrhi90xcs6mg5i40g4lhq1hbwd075lsd"))))
    (build-system gnu-build-system)
    (arguments
     (list #:tests? #f                  ;no meaningful test suite
           #:make-flags #~(list (string-append "PREFIX=" #$output))
           #:phases #~(modify-phases %standard-phases
                        (delete 'configure)))) ;no configure script
    (home-page "https://github.com/libretro/retroarch-joypad-autoconfig")
    (synopsis "RetroArch joypad autoconfig files")
    (description "This package provides joypad autoconfig files for RetroArch,
the reference frontend for the libretro API.  The autoconfig files are used to
recognize input devices and automatically setup default mappings between the
physical device and the RetroPad virtual controller.")
    (license license:expat)))

(define-public libretro-slang-shaders
  ;; There are no releases; use the latest commit.

  ;; BEWARE: Any upgrade to this package must have the sources carefully
  ;; audited for newly added items, with the snippet allow-list updated
  ;; accordingly, due to various items lacking license information or being
  ;; non-free (see: https://github.com/libretro/slang-shaders/issues/150).
  (let ((commit "a8e35920c5a53448bf6ce78dfe4575485a20a41f")
        (revision "0"))
    (package
      (name "libretro-slang-shaders")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/libretro/slang-shaders/")
               (commit commit)))
         (file-name (git-file-name name version))
         (modules '((guix build utils)))
         (snippet
          '(begin
             (use-modules (guix build utils)
                          (ice-9 ftw)
                          (srfi srfi-1)
                          (srfi srfi-26))
             (define (delete-all-but . preserve)
               ;; Walk the file tree and delete everything except the paths
               ;; listed in PRESERVE.  Directories listed PRESERVE will cause
               ;; their whole contents to be preserved.
               (let ((preserve (map (compose (cut string-trim-right <> #\/)
                                             (cut string-append "./" <>))
                                    preserve)))
                 (file-system-fold
                  (lambda (path stat result) ;enter
                    (or (any (lambda (x)
                               (or (string-prefix? path x)
                                   (string-prefix? x path)))
                             preserve)
                        (begin
                          (delete-file-recursively path)
                          #f)))
                  (lambda (path stat result) ;leaf (file)
                    (unless (any (cut string-prefix? <> path) preserve)
                      (delete-file path)))
                  (const #t)                 ;down (directory)
                  (const #t)                 ;up (directory)
                  (lambda (path stat result) ;skip
                    (when (file-exists? path)
                      (error "could not enter unreadable directory" path)))
                  (lambda (path stat errno result) ;error
                    (error "error processing" path (strerror errno)))
                  0
                  ".")))

             ;; This is an allow-list of the shaders explicitly licensed as
             ;; free software.
             (delete-all-but
              "anamorphic/shaders/anamorphic.slang" ;expat
              "anamorphic/anamorphic.slangp"
              "annotated_passthru.slang" ;public license
              "anti-aliasing/aa-shader-4.0-level2.slangp"
              "anti-aliasing/aa-shader-4.0.slangp"
              "anti-aliasing/shaders/aa-shader-4.0.slang"  ;gpl2+
              "anti-aliasing/shaders/aa-shader-4.0-level2" ;gpl2+
              "anti-aliasing/shaders/advanced-aa.slang" ;gpl2+
              "anti-aliasing/advanced-aa.slangp"
              "anti-aliasing/shaders/reverse-aa-post3x"    ;expat
              "anti-aliasing/shaders/reverse-aa.slang"     ;bsd-2
              "anti-aliasing/shaders/smaa/"                ;unlicense
              ;; The following presets refder to stock.slang, which license is
              ;; unknown.
              ;; "anti-aliasing/smaa+linear.slangp"
              ;; "anti-aliasing/smaa+sharpen.slangp"
              ;; "anti-aliasing/smaa.slangp"
              "auto-box/"               ;public domain
              "bezel/koko-aio/"         ;gpl3+
              ;; Mega_Bezel makes use of the include/compat_macros.inc file,
              ;; which carries no license.
              ;; "bezel/Mega_Bezel/" ;gpl3+
              "bfi/"                       ;public domain
              "blurs/shaders/dual_filter/" ;cc0
              "blurs/shaders/gauss_4tap/"  ;cc0
              "blurs/gauss_4tap.slangp"
              "blurs/shaders/gaussian_blur_filtering" ;gpl2+
              "blurs/gaussian_blur_2_pass-sharp.slangp"
              "blurs/gaussian_blur-sharp.slangp"
              "blurs/gaussian_blur_2_pass.slangp"
              "blurs/gaussian_blur.slangp"
              "blurs/shaders/gizmo-blur.slang" ;gpl2+
              "blurs/gizmo-composite-blur.slangp"
              "blurs/shaders/sharpsmoother.slang" ;gpl2+
              "blurs/sharpsmoother.slangp"
              "blurs/shaders/smart-blur.slang" ;expat
              "blurs/smart-blur.slangp"        ;expat
              ;; The .slang shaders of royale, itself Expat, all reference
              ;; include/compat_macros.inc, which is not licensed thus not
              ;; included.
              ;;"blurs/shaders/royale"           ;expat
              "border/shaders/bigblur.slang" ;public domain
              "border/shaders/autocrop-koko" ;gpl3+
              "border/autocrop-koko.slangp"
              "border/autocrop-koko.txt"
              "border/textures"                     ;data
              "border/shaders/imgborder-gbp.slang"  ;<15 LOC
              "border/shaders/imgborder.inc"        ;public domain
              "border/shaders/imgborder-sgba.slang" ;<15 LOC
              "border/shaders/imgborder-sgb.slang"  ;<15 LOC
              "border/shaders/imgborder.slang"      ;<15 LOC
              "border/gameboy-player/gameboy-player-crt-geom-1x.slangp"
              ;;"border/gameboy-player/gameboy-player-crt-royale.slangp"
              "border/gameboy-player/gameboy-player-gba-color.slangp"
              "border/gameboy-player/gameboy-player.png" ;data
              "border/gameboy-player/gameboy-player.slangp"
              "border/gameboy-player/gameboy-player-tvout-gba-color+interlacing.slangp"
              "border/gameboy-player/gameboy-player-tvout-gba-color.slangp"
              "border/gameboy-player/gameboy-player-tvout+interlacing.slangp"
              "border/gameboy-player/gameboy-player-tvout.slangp"
              "border/gameboy-player/sample-borders/" ;data
              "border/handheld-nebula/handheld-nebula-gba+crt-consumer.slangp"
              "border/handheld-nebula/handheld-nebula-gba+dot.slangp"
              "border/handheld-nebula/handheld-nebula-gba.png" ;data
              "border/handheld-nebula/handheld-nebula-gba.slangp"
              "border/handheld-nebula/handheld-nebula-gb+crt-consumer.slangp"
              "border/handheld-nebula/handheld-nebula-gb+dot.slangp"
              "border/handheld-nebula/handheld-nebula-gb.png" ;data
              "border/handheld-nebula/handheld-nebula-gb.slangp"
              "border/handheld-nebula/handheld-nebula-gg+crt-consumer.slangp"
              "border/handheld-nebula/handheld-nebula-gg+dot.slangp"
              "border/handheld-nebula/handheld-nebula-gg.png" ;data
              "border/handheld-nebula/handheld-nebula-gg.slangp"
              "border/handheld-nebula/handheld-nebula-template.png" ;data
              "border/imgborder.slangp"
              "cel/shaders/advcartoon.slang" ;gpl (unknown version)
              "cel/advcartoon.slangp"
              "crt/shaders/Advanced_CRT_shader_whkrmrgks0.slang" ;gpl3+
              "crt/advanced_crt_whkrmrgks0.slangp"
              "crt/shaders/cathode-retro" ;expat
              ;;"crt/cathode-retro_no-signal.slangp"    ;uses stock.slang
              "crt/shaders/crt-1tap.slang"     ;cc0
              "crt/shaders/crt-aperture.slang" ;gpl (unknown version)
              "crt/crt-aperture.slangp"
              "crt/shaders/crt-blurPi.slang" ;expat
              "crt/crt-blurPi-sharp.slangp"
              "crt/crt-blurPi-soft.slangp"
              "crt/shaders/crt-caligari.slang" ;gpl2+
              "crt/crt-caligari.slangp"
              "crt/shaders/crt-cgwg-fast.slang" ;gpl2+
              "crt/crt-cgwg-fast.slangp"
              "crt/shaders/crt-consumer.slang" ;gpl2+
              "crt/shaders/crt-consumer"
              "crt/crt-consumer.slangp"
              "crt/shaders/crt-Cyclon.slang" ;gpl2+
              "crt/crt-Cyclon.slangp"
              "crt/shaders/crt-easymode.slang"    ;gpl3+ (latest assumed)
              "crt/shaders/crt-easymode-halation" ;gpl3+ (latest assumed)
              "crt/crt-easymode-halation.slangp"
              "crt/crt-easymode.slangp"
              "crt/shaders/crt-gdv-mini.slang"       ;gpl2+
              "crt/shaders/crt-gdv-mini-ultra.slang" ;gpl2+
              "crt/crt-gdv-mini.slangp"
              "crt/crt-gdv-mini-ultra-trinitron.slangp"
              "crt/shaders/crt-geom-mini.slang" ;gpl2+
              "crt/shaders/crt-geom.slang"      ;gpl2+
              "crt/crt-geom-deluxe.slangp"
              "crt/crt-geom-mini.slangp"
              "crt/crt-geom.slangp"
              "crt/crt-geom-tate.slangp"
              "crt/shaders/crt-interlaced-halation" ;gpl2+
              "crt/shaders/crt-lottes-fast.slang"   ;unlicense
              "crt/crt-lottes-fast.slangp"
              "crt/shaders/crt-lottes-multipass" ;public domain
              "crt/shaders/crt-lottes.slang"
              "crt/ crt-lottes.slangp"
              ;;"crt/shaders/crt-maximus-royale" ;gpl2+
              "crt/shaders/crt-nobody.slang" ;expat
              "crt/crt-nobody.slangp"
              "crt/shaders/crt-pi.slang" ;gpl2+
              "crt/crt-pi.slangp"
              "crt/shaders/crt-pocket.slang" ;gpl2+
              "crt/crt-pocket.slangp"
              "crt/shaders/crt-potato"  ;gpl3+
              "crt/crt-potato-BVM.slangp"
              "crt/crt-potato-cool.slangp"
              "crt/crt-potato-warm.slangp"
              "crt/shaders/crt-resswitch-glitch-koko.slang" ;gpl3+
              "crt/crt-resswitch-glitch-koko.slangp"
              ;; crt-royale relies on royale, which pulls in the non-free
              ;; include/compat_macros.h.
              ;; "crt/shaders/crt-royale" ;gpl2+
              ;; "crt/crt-royale-fake-bloom-intel.slangp"
              ;; "crt/crt-royale-fake-bloom.slangp"
              ;; "crt/crt-royale-fast.slangp" "crt/crt-royale-intel.slangp"
              ;; "crt/crt-royale.slangp"
              "crt/shaders/crtsim"      ;cc0
              "crt/crtsim.slangp"
              "crt/shaders/crt-simple.slang" ;gpl2+
              "crt/crt-simple.slangp"
              "crt/shaders/crt-super-xbr" ;expat
              "crt/crt-super-xbr.slangp"
              "crt/shaders/dotmask.slang"   ;gpl3+ (latest assumed)
              "crt/shaders/geom-deluxe"     ;gpl2+
              "crt/shaders/gizmo-crt.slang" ;gpl2+
              "crt/gizmo-crt.slangp"
              "crt/shaders/gizmo-slotmask-crt.slang" ;gpl2+
              "crt/gizmo-slotmask-crt.slangp"
              "crt/shaders/GritsScanlines" ;public domain
              ;;"crt/GritsScanlines.slangp"    ;uses stock.slang
              "crt/shaders/gtu-v050"    ;gpl3
              "crt/gtu-v050.slangp"
              "crt/shaders/guest"       ;gpl2+
              "crt/crt-guest-advanced-fastest.slangp"
              ;; The following crt-guest-advanced presets require
              ;; 'stock.slang', which license is unknown.
              ;; "crt/crt-guest-advanced-fast.slangp"
              ;; "crt/crt-guest-advanced-hd.slangp"
              ;; "crt/crt-guest-advanced-ntsc.slangp"
              ;; "crt/crt-guest-advanced.slangp"
              "crt/shaders/hyllian"     ;expat
              "crt/crt-hyllian-3d.slangp"
              "crt/crt-hyllian-fast.slangp"
              "crt/crt-hyllian-fast.slangp"
              "crt/shaders/mame_hlsl"   ;bsd-3
              "crt/mame_hlsl.slangp"
              "crt/shaders/moire-resolve.slang" ;public domain
              "crt/shaders/newpixie"            ;mit or public domain
              "crt/newpixie-crt.slangp"
              "crt/shaders/newpixie-mini" ;mit or public domain
              "crt/newpixie-mini.slangp"
              "crt/shaders/phosphorlut/scanlines-interlace-linearize.slang" ;public domain
              "crt/shaders/rt_curvature" ;cc0
              "crt/ray_traced_curvature_append.slangp"
              "crt/shaders/torridgristle/Brighten.slang"       ;public domain
              "crt/shaders/torridgristle/Candy-Bloom.slang"    ;public domain
              "crt/shaders/torridgristle/ScanlineSimple.slang" ;public domain
              "crt/shaders/torridgristle/sunset-gaussian-horiz.slang" ;public domain
              "crt/shaders/torridgristle/sunset-gaussian-vert.slang" ;public domain
              "crt/shaders/tvout-tweaks.slang"                       ;gpl3
              "crt/tvout-tweaks.slangp"
              "crt/shaders/zfast_crt"   ;gpl2+
              "crt/zfast-crt-composite.slangp"
              "crt/zfast-crt-curvature.slangp"
              "crt/zfast-crt-geo.slangp"
              "crt/zfast-crt-hdmask.slangp"
              "crt/zfast-crt.slangp"
              "deblur/shaders/deblur-luma.slang" ;gpl2+
              "deblur/deblur-luma.slangp"
              "deblur/shaders/deblur.slang" ;gpl2+
              "deblur/deblur.slangp"
              "denoisers/shaders/bilateral-horizontal.slang" ;gpl2+
              "denoisers/shaders/bilateral.slang"            ;gpl2+
              "denoisers/bilateral.slangp"
              "denoisers/shaders/bilateral-vertical.slang" ;gpl2+
              "denoisers/bilateral-2p.slangp"
              "denoisers/shaders/fast-bilateral.slang" ;expat
              "denoisers/fast-bilateral.slangp"
              "denoisers/crt-fast-bilateral-super-xbr.slangp"
              "denoisers/shaders/median_3x3.slang" ;bsd-2
              "denoisers/median_3x3.slangp"
              "denoisers/shaders/median_5x5.slang" ;bsd-2
              "denoisers/median_5x5.slangp"
              "dithering/shaders/bayer_4x4.slang" ;gpl2+
              "dithering/bayer_4x4.slangp"
              "dithering/shaders/blue_noise.slang" ;gpl2+
              "dithering/shaders/blue_noise"
              "dithering/blue_noise.slangp"
              "dithering/shaders/blue_noise_dynamic.slang" ;gpl2+
              "dithering/blue_noise_dynamic_4Bit.slangp"
              "dithering/blue_noise_dynamic_monochrome.slangp"
              "dithering/shaders/cbod-v1-pass1.slang" ;bsd-2
              "dithering/shaders/cbod-v1-pass2.slang" ;bsd-2
              "dithering/cbod_v1.slangp"
              "dithering/shaders/checkerboard-dedither-pass1.slang" ;expat
              "dithering/shaders/checkerboard-dedither-pass2.slang" ;expat
              "dithering/shaders/checkerboard-dedither-pass3.slang"
              "dithering/shaders/gendither.slang" ;gpl2+
              "dithering/gendither.slangp"
              "dithering/shaders/g-sharp_resampler.slang" ;gpl2+
              "dithering/g-sharp_resampler.slangp"
              "dithering/shaders/jinc2-dedither.slang" ;gpl2+
              "dithering/jinc2-dedither.slangp"
              "dithering/shaders/sgenpt-mix/sgenpt-mix-pass1.slang" ;expat
              "dithering/shaders/sgenpt-mix/sgenpt-mix-pass2.slang" ;expat
              "dithering/shaders/sgenpt-mix/sgenpt-mix-pass3.slang" ;expat
              "dithering/shaders/sgenpt-mix/sgenpt-mix-pass4.slang" ;expat
              "dithering/shaders/sgenpt-mix/sgenpt-mix-pass5.slang" ;expat
              "dithering/shaders/sgenpt-mix.slang"                  ;expat
              "dithering/sgenpt-mix.slangp"
              "downsample/shaders/drez-g-sharp_resampler.slang" ;gpl2+
              "downsample/drez/"
              "downsample/drez_1x.slangp"
              "downsample/shaders/mixed-res/cheap-sharpen-tweaked.slang" ;expat
              "downsample/shaders/mixed-res/hires-tagger.slang" ;expat
              "edge-smoothing/ddt/shaders/cut.slang"            ;expat
              "edge-smoothing/ddt//cut.slangp"
              "edge-smoothing/ddt/shaders/ddt-extended.slang" ;expat
              "edge-smoothing/ddt/ddt-extended.slangp"
              "edge-smoothing/ddt/shaders/ddt-jinc.slang" ;gpl2+
              "edge-smoothing/ddt/ddt-jinc.slangp"
              "edge-smoothing/ddt/shaders/ddt.slang" ;expat
              "edge-smoothing/ddt/ddt.slangp"
              "edge-smoothing/ddt/shaders/ddt-waterpaint.slang" ;expat
              "edge-smoothing/ddt/shaders/ddt-xbr-lv1.slang"    ;expat
              "edge-smoothing/ddt/ddt-xbr-lv1.slangp"
              "edge-smoothing/fsr/shaders" ;expat & unlicense
              "edge-smoothing/fsr/fsr-easu.slangp"
              "edge-smoothing/fsr/fsr.slangp"
              ;; hqx presets require stock.slang which has unknown license.
              ;; "edge-smoothing/hqx"           ;expat and lgpl2.1+
              "edge-smoothing/hqx/resources" ;data
              "edge-smoothing/hqx/shaders"   ;expat and lgpl2.1+
              "edge-smoothing/nedi/"         ;gpl3+ and expat
              "edge-smoothing/nnedi3/"       ;gpl3+ and gpl2+
              "edge-smoothing/omniscale/"    ;expat
              "edge-smoothing/sabr/"         ;gpl2+
              "edge-smoothing/scalefx/"      ;expat
              "edge-smoothing/scalehq/shaders/4xScaleHQ.slang" ;gpl2+
              "edge-smoothing/scalenx/shaders/mmpx.slang"      ;expat
              "edge-smoothing/scalenx/mmpx.slangp"
              "edge-smoothing/scalenx/shaders/scale2xplus.slang" ;gpl3+ (latest assumed)
              "edge-smoothing/scalenx/scale2xplus.slangp"
              "edge-smoothing/scalenx/shaders/scale2x.slang" ;gpl3+ (latest assumed)
              "edge-smoothing/scalenx/scale2x.slangp"
              "edge-smoothing/scalenx/shaders/scale3x.slang" ;gpl3+ (latest assumed)
              "edge-smoothing/scalenx/scale3x.slangp"
              "edge-smoothing/xbr/shaders/super-xbr/"               ;expat
              "edge-smoothing/xbr/shaders/xbr-lv1-standalone.slang" ;expat
              "edge-smoothing/xbr/shaders/xbr-lv2-hd.slang"         ;expat
              "edge-smoothing/xbr/shaders/xbr-lv2-hd.slang"         ;expat
              "edge-smoothing/xbr/shaders/xbr-lv2-multipass/"       ;expat
              "edge-smoothing/xbr/shaders/xbr-lv2-standalone.slang" ;expat
              "edge-smoothing/xbr/shaders/xbr-lv3-multipass/"       ;expat
              "edge-smoothing/xbr/shaders/xbr-lv3-standalone.slang" ;expat
              "edge-smoothing/xbr/shaders/xbr-lv4-multipass/"       ;expat
              "edge-smoothing/xbr/other presets/shaders/4xbr-hybrid-crt.slang" ;expat
              "edge-smoothing/xbr/other presets/4xbr-hybrid-crt.slangp"
              "edge-smoothing/xbr/other presets/shaders/super-xbr/"  ;expat
              "edge-smoothing/xbr/other presets/shaders/xbr-hydrid/" ;gpl2+
              "edge-smoothing/xbr/other presets/xbr-lv1-standalone.slangp"
              "edge-smoothing/xbr/other presets/xbr-lv2-hd.slangp"
              "edge-smoothing/xbr/other presets/xbr-lv2-standalone.slangp"
              "edge-smoothing/xbr/other presets/xbr-lv3-9x-standalone.slangp"
              "edge-smoothing/xbr/other presets/xbr-lv3-standalone.slangp"
              "edge-smoothing/xbrz/shaders/" ;expat
              "edge-smoothing/xbrz/2xbrz-linear.slangp"
              "edge-smoothing/xbrz/xbrz-freescale.slangp"
              "edge-smoothing/xsal/shaders/" ;gpl2+
              "edge-smoothing/xsal/2xsal-level2-crt.slangp"
              "edge-smoothing/xsal/2xsal.slangp"
              "edge-smoothing/xsal/4xsal-level2-crt.slangp"
              "edge-smoothing/xsoft/shaders/" ;gpl2+
              "film/shaders/film-grain.slang" ;cc-by3.0
              "film/film-grain.slangp"
              "gpu/"                            ;gpl2+
              "handheld/shaders/authentic_gbc/" ;cc0
              "handheld/authentic_gbc.slangp"
              "handheld/shaders/bevel.slang" ;gpl2+
              "handheld/bevel.slangp"
              "handheld/shaders/color/" ;public domain
              "handheld/nds-color.slangp"
              "handheld/NSO-gba-color.slangp"
              "handheld/NSO-gbc-color.slangp"
              "handheld/palm-color.slangp"
              "handheld/psp-color.slangp"
              "handheld/gba-color.slangp"
              "handheld/gbc-color.slangp"
              "handheld/gbc-dev.slangp"
              "handheld/gbc-gambatte-color.slangp"
              "handheld/SP101-color.slangp"
              "handheld/SwitchOLED-color.slangp"
              "handheld/vba-color.slangp"
              "handheld/shaders/dot.slang" ;public domain
              "handheld/dot.slangp"
              "handheld/shaders/ds-hybrid-view.slang" ;public domain
              "handheld/shaders/gameboy/"             ;gpl3+
              "handheld/gameboy-advance-dot-matrix.slangp"
              "handheld/gameboy-color-dot-matrix.slangp"
              "handheld/gameboy-color-dot-matrix-white-bg.slangp"
              "handheld/gameboy-dark-mode.slangp"
              "handheld/gameboy-light-mode.slangp"
              "handheld/gameboy-light.slangp"
              "handheld/gameboy-pocket-high-contrast.slangp"
              "handheld/gameboy-pocket.slangp"
              "handheld/gameboy.slangp"
              "handheld/shaders/gbc_pokemon_modernizer.slang" ;public domain
              "handheld/shaders/lcd1x_nds.slang"              ;gpl2+
              "handheld/lcd1x_nds.slangp"
              "handheld/shaders/lcd1x_psp.slang" ;gpl2+
              "handheld/lcd1x_psp.slangp"
              "handheld/shaders/lcd1x.slang" ;gpl2+
              "handheld/lcd1x.slangp"
              "handheld/shaders/lcd3x.slang" ;public domain
              "handheld/ lcd3x.slangp"
              "handheld/shaders/lcd-shader/" ;gpl3+
              "handheld/lcd-shader.slangp"
              "handheld/shaders/mgba/"  ;mpl2.0
              "handheld/agb001-gba-color-motionblur.slangp"
              "handheld/ags001-gba-color-motionblur.slangp"
              "handheld/ags001.slangp"
              "handheld/shaders/retro-tiles.slang" ;expat
              "handheld/retro-tiles.slangp"
              "handheld/shaders/retro-v2.slang" ;gpl2+
              "handheld/retro-v2-nds-color.slangp"
              "handheld/retro-v2.slangp"
              "handheld/shaders/retro-v3.slang" ;gpl2+
              "handheld/retro-v3-nds-color.slangp"
              "handheld/retro-v3.slangp"
              "handheld/shaders/sameboy-lcd.slang" ;expat
              "handheld/sameboy-lcd-gbc-color-motionblur.slangp"
              "handheld/sameboy-lcd.slangp"
              "handheld/shaders/simpletex_lcd/" ;gpl2+
              "handheld/simpletex_lcd-4k.slangp"
              "handheld/simpletex_lcd_720p+gba-color.slangp"
              "handheld/simpletex_lcd_720p+gbc-color.slangp"
              "handheld/simpletex_lcd_720p.slangp"
              "handheld/simpletex_lcd+gba-color-4k.slangp"
              "handheld/simpletex_lcd+gba-color.slangp"
              "handheld/simpletex_lcd+gbc-color-4k.slangp"
              "handheld/simpletex_lcd+gbc-color.slangp"
              "handheld/simpletex_lcd.slangp"
              "handheld/shaders/zfast_lcd.slang" ;gpl2+
              "handheld/zfast-lcd.slangp"
              "handheld/console-border/shader-files/gb-pass0.slang" ;gpl3+
              "handheld/console-border/shader-files/gb-pass1.slang" ;gpl3+
              "handheld/console-border/shader-files/gb-pass2.slang" ;gpl3+
              "handheld/console-border/shader-files/gb-pass3.slang" ;gpl3+
              "handheld/console-border/resources/" ;non-functional data
              "handheld/console-border/dmg.slangp"
              "hdr/shaders/crt-guest-advanced-ntsc-pass1a.slang" ;gpl2+
              "include/blur-functions.h"                         ;expat
              "include/cleanEdge.inc"                            ;expat
              "include/colorspace-tools.h"                       ;gpl2+
              "include/gamma-management.h"                       ;expat
              "include/img/black_lvl_dogway.h"                   ;<15 LOC
              "include/img/black_lvl.h"                          ;<15 LOC
              "include/img/cgwg_warp.h"                          ;gpl2
              "include/img/channel_mix.h"                        ;<15 LOC
              "include/img/col_tools.h"                          ;<15 LOC
              "include/img/curvature.h"             ;gpl2 and gpl3
              "include/img/int_ar.h"                ;public domain
              "include/img/subpx_masks.h"           ;<15 LOC
              "include/quad-pixel-communication.h"  ;expat
              "include/special-functions.h"         ;expat
              "include/subpixel_masks.h"            ;public domain
              "interpolation/shaders/bicubic.slang" ;gpl2+
              "interpolation/bicubic.slangp"
              "interpolation/shaders/bicubic-x.slang" ;expat
              "interpolation/shaders/bicubic-y.slang" ;expat
              "interpolation/bicubic-fast.slangp"
              "interpolation/shaders/b-spline-4-taps.slang" ;expat
              "interpolation/b-spline-4-taps.slangp"
              "interpolation/shaders/b-spline-x.slang" ;expat
              "interpolation/shaders/b-spline-y.slang" ;expat
              "interpolation/b-spline-fast.slangp"
              "interpolation/shaders/catmull-rom-x.slang" ;expat
              "interpolation/shaders/catmull-rom-y.slang" ;expat
              "interpolation/catmull-rom-fast.slangp"
              "interpolation/shaders/jinc2.slang" ;gpl2+
              "interpolation/jinc2-sharper.slangp"
              "interpolation/jinc2-sharp.slangp"
              "interpolation/jinc2.slangp"
              "interpolation/shaders/lanczos16.slang" ;gpl2+
              "interpolation/lanczos16.slangp"
              "interpolation/shaders/lanczos3-x.slang" ;gpl2+
              "interpolation/shaders/lanczos3-y.slang" ;gpl2+
              "interpolation/lanczos3-fast.slangp"
              "interpolation/shaders/spline16-x.slang" ;gpl2+
              "interpolation/shaders/spline16-y.slang" ;gpl2+
              "interpolation/spline16-fast.slangp"
              "interpolation/shaders/spline36-x.slang" ;gpl2+
              "interpolation/shaders/spline36-y.slang" ;gpl2+
              "interpolation/spline36-fast.slangp"
              "misc/shaders/anti-flicker.slang" ;public domain
              "misc/anti-flicker.slangp"
              "misc/shaders/bead.slang" ;public domain
              "misc/bead.slangp"
              "misc/shaders/bob-deinterlacing.slang" ;public domain
              "misc/bob-deinterlacing.slangp"
              "misc/shaders/chromaticity.slang" ;gpl3+
              "misc/chromaticity.slangp"
              "misc/shaders/coverage/coverage.inc" ;<15 LOC
              "misc/shaders/deband.slang"          ;gpl2+
              "misc/deband.slangp"
              "misc/shaders/deinterlace.slang" ;gpl2
              "misc/deinterlace.slangp"
              "misc/shaders/deposterize" ;gpl2+
              "misc/shaders/geom.slang"  ;gpl2+
              "misc/geom-append.slangp"
              "misc/shaders/glass.slang" ;public domain
              "misc/glass.slangp"
              "misc/shaders/grade-no-LUT.slang" ;gpl2+
              "misc/grade-no-LUT.slangp"
              "misc/shaders/grade.slang" ;gpl2+
              "misc/grade.slangp"
              "misc/shaders/image-adjustment.slang" ;public domain
              "misc/image-adjustment.slangp"
              "misc/shaders/img_mod.slang" ;public domain
              "misc/img_mod.slangp"
              "misc/shaders/input_transform" ;cc0
              "misc/shaders/interlacing.slang"
              "misc/interlacing.slangp"
              "misc/shaders/print-resolution/print-resolution-generate-and-cache.slang" ;gpl3+
              "misc/shaders/relief.slang" ;expat
              "misc/relief.slangp"
              "misc/shaders/ss-gamma-ramp.slang" ;gpl2
              "misc/ss-gamma-ramp.slangp"
              "motionblur/shaders/braid-rewind.slang" ;gpl2+
              "motionblur/braid-rewind.slangp"
              "motionblur/shaders/mix_frames.slang" ;gpl2+
              "motionblur/mix_frames.slangp"
              "motionblur/shaders/mix_framse_smart.slang" ;gpl2+
              "motionblur/mix_frames_smart.slangp"
              "motionblur/shaders/motionblur-simple.slang" ;gpl2+
              "motionblur/motionblur-simple.slangp"
              "motionblur/shaders/response-time.slang" ;gpl2+
              "motionblur/response-time.slangp"
              "nes_raw_palette/shaders/gtu-famicom/" ;gpl3
              "nes_raw_palette/gtu-famicom.slangp"
              "nes_raw_palette/pal-r57shell-raw.slangp"
              "nes_raw_palette/patchy-mesen-raw-palette.slangp"
              "ntsc/shaders/analog_overshoot.slang"          ;lgpl3
              "ntsc/shaders/ntsc-simple/ntsc-simple-1.slang" ;gpl2+
              "ntsc/shaders/ntsc-simple/ntsc-simple-2.slang" ;gpl2+
              "ntsc/shaders/ntsc-xot.slang" ;cc-by-sa version unknown
              "ntsc/shaderspatchy-ntsc/afterglow0-update/afterglow0-update-pass2.slang" ;gpl2+
              "ntsc/shaderspatchy-ntsc/afterglow0-update/afterglow0-update-pass3.slang" ;gpl2+
              "ntsc/shaders/patchy-ntsc/linear-to-srgb.slang"          ;gpl3
              "ntsc/shaders/patchy-ntsc/P22_80s_D65.png"               ;data
              "ntsc/shaders/patchy-ntsc/P22_90s_D65.png"               ;data
              "ntsc/shaders/patchy-ntsc/P22_J_D65.png"                 ;data
              "ntsc/shaders/patchy-ntsc/P22_J_D93.png"                 ;data
              "ntsc/shaders/patchy-ntsc/patchy-color.slang"            ;gpl3
              "ntsc/shaders/patchy-ntsc/patchy-ntsc-combine-y-c.slang" ;gpl3
              "ntsc/shaders/patchy-ntsc/patchy-ntsc-decode-y-rmy-bmy.slang" ;gpl3
              "ntsc/shaders/patchy-ntsc/patchy-ntsc-encode-y-c.slang"   ;gpl3
              "ntsc/shaders/patchy-ntsc/patchy-ntsc-eotf.slang"         ;gpl3
              "ntsc/shaders/patchy-ntsc/patchy-ntsc-inc-filters.inc"    ;gpl3
              "ntsc/shaders/patchy-ntsc/patchy-ntsc-inc-params.inc"     ;gpl3
              "ntsc/shaders/patchy-ntsc/patchy-ntsc-noise.slang"        ;gpl3
              "ntsc/shaders/patchy-ntsc/patchy-ntsc-separate-y-c.slang" ;gpl3
              "ntsc/shaders/patchy-ntsc/trilinearLUT-switchable.slang"  ;gpl2+
              "ntsc/shaders/patchy-ntsc/TrinitronP22_D65.png"           ;data
              "ntsc/shaders/patchy-ntsc/TrinitronP22_D93.png"           ;data
              "ntsc/patchy-blastem.slangp"
              "ntsc/patchy-genplusgx.slangp"
              "ntsc/patchy-snes.slangp"
              "pal/shaders/pal-r57shell.slang" ;public domain
              "pal/pal-r57shell.slangp"
              "pal/shaders/pal-singlepass.slang" ;bsd-2
              "pal/pal-singlepass.slangp"
              "pal/resources/"                       ;data
              "pixel-art-scaling/shaders/aann.slang" ;expat
              "pixel-art-scaling/aann.slangp"
              "pixel-art-scaling/shaders/bandlimit-pixel.slang" ;expat
              "pixel-art-scaling/shaders/box_filter_aa/"        ;cc0
              "pixel-art-scaling/box_filter_aa_xform.slangp"
              "pixel-art-scaling/shaders/pixel_aa" ;cc0
              "pixel-art-scaling/pixel_aa.slangp"
              "pixel-art-scaling/pixel_aa_xform.slangp"
              "pixel-art-scaling/shaders/pixellate.slang" ;isc
              "pixel-art-scaling/pixellate.slangp"
              "pixel-art-scaling/shaders/sharp-bilinear-scanlines.slang" ;public domain
              "pixel-art-scaling/sharp-bilinear-scanlines.slangp"
              "pixel-art-scaling/shaders/sharp-bilinear-simple.slang" ;public domain
              "pixel-art-scaling/sharp-bilinear-simple.slangp"
              "pixel-art-scaling/shaders/sharp-bilinear.slang"
              "pixel-art-scaling/sharp-bilinear.slangp"
              "pixel-art-scaling/shaders/uniform-nearest.slang" ;gpl2+
              "pixel-art-scaling/uniform-nearest.slangp"
              ;; The following include stock.slang, which license is unknown.
              ;; "presets/crt-hyllian-sinc-smartblur-sgenpt.slangp"
              ;; "presets/crt-hyllian-smartblur-sgenpt.slangp"
              ;; The following depend on royale, which pulls in the non-free
              ;; include/compat_macros.h.
              ;; "presets/crt-royale-fast/4k/crt-royale-fast-rgb-aperture.slangp"
              ;; "presets/crt-royale-fast/4k/crt-royale-fast-rgb-slot.slangp"
              ;; "presets/crt-royale-fast/4k/crt-royale-pvm-rgb-blend.slangp"
              ;; "presets/crt-royale-fast/4k/crt-royale-pvm-rgb-shmup.slangp"
              ;; "presets/crt-royale-fast/4k/crt-royale-pvm-rgb.slangp"
              ;; "presets/crt-royale-fast/crt-royale-fast-rgb-aperture.slangp"
              ;; "presets/crt-royale-fast/crt-royale-fast-rgb-slot.slangp"
              ;; "presets/crt-royale-fast/crt-royale-pvm-rgb-blend.slangp"
              ;; "presets/crt-royale-fast/crt-royale-pvm-rgb-shmup.slangp"
              ;; "presets/crt-royale-fast/crt-royale-pvm-rgb.slangp"
              ;; "presets/crt-royale-fast-ntsc-composite.slangp"
              ;; "presets/crt-royale-kurozumi.slangp"
              ;; "presets/crt-royale-ntsc-composite.slangp"
              ;; "presets/crt-royale-ntsc-svideo.slangp"
              ;; "presets/crt-royale-pal-r57shell.slangp"
              ;; "presets/crt-royale-xm29plus.slangp"
              "presets/crtsim-grungy.slangp"
              "presets/gizmo-crt/"      ;slangp data files
              ;; "presets/imgborder-royale-kurozumi.slangp"
              "presets/my_old_tv.slangp"
              "presets/nedi-powervr-sharpen.slangp"
              "presets/retro-v2+gba-color.slangp"
              "presets/retro-v2+gbc-color.slangp"
              "presets/retro-v2+image-adjustment.slangp"
              "presets/retro-v2+nds-color.slangp"
              "presets/retro-v2+psp-color.slangp"
              "presets/retro-v2+vba-color.slangp"
              "presets/tvout/tvout-jinc-sharpen.slangp"
              "presets/tvout/tvout.slangp"
              "presets/tvout+interlacing/tvout+interlacing.slangp"
              "presets/tvout+interlacing/tvout-jinc-sharpen+interlacing.slangp"
              ;; The xbr-xsal presets require support/linearize.slang, whose
              ;; license is unknown.
              ;;"presets/xbr-xsal/"                          ;slangp data files
              "procedural/iq-raymarching-primitives.slang" ;expat
              "quad/shaders/biquad.slang"                  ;gpl2+
              "quad/quad_interp.slang"                     ;public domain
              "reshade/shaders/magicbloom/"                ;mit
              "scanlines/shaders/res-independent-scanlines.slang" ;public domain
              "scanlines/res-independent-scanlines.slangp"
              "scanlines/shaders/scanline-fract.slang" ;public domain
              "scanlines/scanline-fract.slangp"
              "scanlines/shaders/scanlines-rere.slang" ;public domain
              "scanlines/scanlines-rere.slangp"
              "scanlines/shaders/scanlines-sine-abs.slang" ;public domain
              "scanlines/scanlines-sine-abs.slangp"
              "sharpen/shaders/adaptive-sharpen-pass1.slang" ;bsd-2
              "sharpen/shaders/adaptive-sharpen-pass2.slang" ;bsd-2
              "sharpen/adaptive-sharpen-multipass.slangp"
              "sharpen/shaders/adaptive-sharpen.slang" ;bsd-2
              "sharpen/adaptive-sharpen.slangp"
              "sharpen/shaders/anime4k/anime4k-compute-gradient.slang" ;expat
              "sharpen/shaders/anime4k/anime4k-pushgrad-weak.slang"    ;expat
              "sharpen/shaders/anime4k/anime4k-push.slang"             ;expat
              "sharpen/shaders/cheap-sharpen.slang"                    ;expat
              "sharpen/cheap-sharpen.slangp"
              "sharpen/shaders/diff.slang"         ;lgpl3+
              "sharpen/shaders/fast-sharpen.slang" ;gpl2+
              "sharpen/shaders/rcas.slang"         ;expat
              "sharpen/rca_sharpen.slangp"
              "sharpen/shaders/super-res-ex.slang"                     ;lgpl3+
              "stereoscopic-3d/shaders/anaglyph-to-side-by-side.slang" ;public domain
              "stereoscopic-3d/anaglyph-to-side-by-side.slangp"
              "stereoscopic-3d/shaders/sbs-to-interlaced.slang" ;public domain
              "stereoscopic-3d/side-by-side-to-interlaced.slangp"
              "stereoscopic-3d/shaders/sbs-to-shutter.slang" ;public domain
              "stereoscopic-3d/side-by-side-to-shutter.slangp"
              "stereoscopic-3d/shaders/shutter-to-side-by-side.slang" ;public domain
              "stereoscopic-3d/shutter-to-side-by-side.slangp"
              "stereoscopic-3d/shaders/side-by-side-simple.slang" ;public domain
              "stereoscopic-3d/side-by-side-simple.slangp"
              "vhs/shaders/vhs_and_crt_godot.slang" ;cc0
              "vhs/vhs_and_crt_godot.slangp"        ;cc0
              "warp/shaders/dilation.slang"         ;expat
              "warp/shaders/erosion.slang"          ;expat
              "warp/dilation.slangp"

              ;; Build/development supporting files not installed.
              ".git/"
              "configure"
              "Makefile")

             ;; The following are special cases, to be used sparringly.  These
             ;; presets uses stock.slang, whose license is unclear.
             (for-each
              (lambda (x)
                (if (file-exists? x)
                    (delete-file x)
                    (format (current-error-port)
                            "warning: file ~s does not exist~%" x)))
              '("bfi/120hz-smart-BFI.slangp"
                "crt/shaders/cathode-retro/signal_test.slangp"
                "blurs/shaders/dual_filter/naive_resample.slang"
                "edge-smoothing/scalefx/scalefx+rAA.slangp"
                "edge-smoothing/scalefx/scalefx-9x.slangp"
                "edge-smoothing/scalefx/scalefx-hybrid.slangp"
                "edge-smoothing/scalefx/scalefx.slangp"
                "edge-smoothing/scalefx/shaders/old/scalefx-9x.slangp"
                "edge-smoothing/scalefx/shaders/old/scalefx.slangp"))))
         (sha256
          (base32
           "0r45p61nhi44f7ka5dvcabin7q2l25liyhgynm159pwlpwxz83nv"))))
      (build-system gnu-build-system)
      (arguments
       (list #:tests? #f                ;no test suite
             #:make-flags #~(list (string-append "PREFIX=" #$output))))
      (home-page "https://github.com/libretro/slang-shaders")
      (synopsis "Vulkan GLSL shader collections for RetroArch")
      (description "This package provides a collection of Vulkan
GLSL (@file{.slang}) shaders for use with RetroArch.")
      ;; Here's the current low-down on the licenses used in this aggregated
      ;; collection; please keep it up to date!
      (license (list license:expat
                     license:cc0
                     license:cc-by3.0
                     license:public-domain
                     license:gpl2
                     license:gpl2+
                     license:gpl3
                     license:gpl3+
                     license:isc
                     license:lgpl2.1+
                     license:lgpl3
                     license:mpl2.0
                     license:bsd-2
                     license:bsd-3
                     license:unlicense)))))

(define-public retroarch-minimal
  (package
    (name "retroarch-minimal")
    (version "1.20.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/libretro/RetroArch")
             (commit (string-append "v" version))))
       (snippet
        #~(begin
            (use-modules (guix build utils)
                         (ice-9 ftw)
                         (srfi srfi-26))
            ;; XXX: 'delete-all-but' is copied from the turbovnc package.
            (define (delete-all-but directory . preserve)
              (define (directory? x)
                (and=> (stat x #f)
                       (compose (cut eq? 'directory <>) stat:type)))
              (with-directory-excursion directory
                (let* ((pred
                        (negate (cut member <> (append '("." "..") preserve))))
                       (items (scandir "." pred)))
                  (for-each (lambda (item)
                              (if (directory? item)
                                  (delete-file-recursively item)
                                  (delete-file item)))
                            items))))
            ;; Remove as much bundled sources as possible, shaving off about
            ;; 65 MiB.
            (delete-all-but "deps"
                            "feralgamemode" ;used in platform_unix.c
                            "mbedtls"       ;further refined below
                            "yxml")         ;used in rxml.c
            ;; This is an old root certificate used in net_socket_ssl_mbed.c,
            ;; not actually from mbedtls.
            (delete-all-but "deps/mbedtls" "cacert.h")))
       (patches (search-patches "retroarch-improved-search-paths.patch"
                                "retroarch-unbundle-spirv-cross.patch"))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0yc16j3g2g0if64xqd7qr4dza8rw10x0zypwbl92y735825p87qi"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f                       ; no tests
      #:phases
      #~(modify-phases %standard-phases
          (replace 'configure
            (lambda* (#:key inputs #:allow-other-keys)
              ;; Hard-code some store file names.
              (substitute* "gfx/common/vulkan_common.c"
                (("libvulkan.so")
                 (search-input-file inputs "lib/libvulkan.so")))
              (substitute* "gfx/common/wayland/generate_wayland_protos.sh"
                (("/usr/local/share/wayland-protocols")
                 (search-input-directory inputs "share/wayland-protocols")))

              ;; Without HLSL, we can still enable GLSLANG and Vulkan support.
              (substitute* "qb/config.libs.sh"
                (("[$]HAVE_GLSLANG_HLSL") "notcare"))

              ;; The configure script does not yet accept the extra arguments
              ;; (like ‘CONFIG_SHELL=’) passed by the default configure phase.
              (invoke
               "./configure"
               #$@(if (string-prefix? "armhf" (or (%current-target-system)
                                                  (%current-system)))
                      '("--enable-neon" "--enable-floathard")
                      '())
               (string-append "--prefix=" #$output)
               ;; D-Bus support is required for 'suspend screensaver' option
               ;; to work.
               "--enable-dbus"
               ;; Non-free software are available through the core updater,
               ;; disable it.  See <https://issues.guix.gnu.org/38360>.
               "--disable-update_cores"
               "--disable-update_core_info"
               "--disable-online_updater"
               ;; The assets are provided via the `retroarch-assets' package.
               "--disable-update_assets"
               "--disable-builtinmbedtls"
               "--disable-builtinbearssl"
               "--disable-builtinzlib"
               "--disable-builtinflac"
               "--disable-builtinglslang"
               "--disable-builtinspirv_cross"
               ;; These are disabled to avoid requiring the bundled
               ;; dependencies.
               "--disable-7zip"
               "--disable-cheevos"
               "--disable-crtswitchres"
               "--disable-discord"
               "--disable-dr_mp3"
               "--disable-ibxm"
               "--disable-stb_font"
               "--disable-stb_image"
               "--disable-stb_vorbis"
               "--disable-xdelta"))))))
    (native-inputs
     (list pkg-config
           wayland-protocols
           which))
    (inputs
     (list alsa-lib
           dbus
           eudev
           ffmpeg
           flac
           fontconfig
           freetype
           glslang
           libxinerama
           libxkbcommon
           libxml2
           libxrandr
           libxv
           mbedtls-lts
           mesa
           openal
           openssl
           pulseaudio
           python
           qtbase-5
           sdl2
           spirv-cross
           spirv-headers
           spirv-tools
           v4l-utils
           vulkan-loader
           wayland
           zlib))
    (native-search-paths
     (list (search-path-specification
            (variable "LIBRETRO_DIRECTORY")
            (separator #f)              ;single entry
            (files '("lib/libretro")))
           (search-path-specification
            (variable "LIBRETRO_ASSETS_DIRECTORY")
            (separator #f)              ;single entry
            (files '("share/libretro/assets")))
           (search-path-specification
            (variable "LIBRETRO_AUTOCONFIG_DIRECTORY")
            (separator #f)              ;single entry
            (files '("share/libretro/autoconfig")))
           (search-path-specification
            (variable "LIBRETRO_CHEATS_DIRECTORY")
            (separator #f)            ;single entry
            (files '("share/libretro/database/cht")))
           (search-path-specification
            (variable "LIBRETRO_DATABASE_DIRECTORY")
            (separator #f)              ;single entry
            (files '("share/libretro/database/rdb")))
           (search-path-specification
            (variable "LIBRETRO_SYSTEM_DIRECTORY")
            (separator #f)              ;single entry
            (files '("share/libretro/system")))
           (search-path-specification
            (variable "LIBRETRO_VIDEO_FILTER_DIRECTORY")
            (separator #f)              ;single entry
            (files '("share/libretro/filters/video")))
           (search-path-specification
            (variable "LIBRETRO_VIDEO_SHADER_DIRECTORY")
            (separator #f)              ;single entry
            (files '("share/libretro/shaders")))))
    (home-page "https://www.libretro.com/")
    (synopsis "Reference frontend for the libretro API")
    (description
     "Libretro is a simple but powerful development interface that allows for
the easy creation of emulators, games and multimedia applications that can plug
straight into any libretro-compatible frontend.  RetroArch is the official
reference frontend for the libretro API, currently used by most as a modular
multi-system game/emulator system.")
    (license (list license:gpl3+        ;for RetroArch itself
                   license:asl2.0       ;SPIRV-Cross
                   license:expat        ;yxml
                   license:bsd-3)))) ;feragamemode

(define-public retroarch
  (package
    (inherit retroarch-minimal)
    (name "retroarch")
    (source #f)
    (build-system trivial-build-system)
    (arguments (list #:builder #~(mkdir #$output)))
    (propagated-inputs
     (list retroarch-minimal
           ;; We cannot simply hard-code the resource paths, as they'd written
           ;; to ~/.config/retroarch.cfg and never updated (going stale),
           ;; which is problematic.  The environment variables overrides the
           ;; configuration file values.
           retroarch-assets
           libretro-core-info
           libretro-database
           retroarch-joypad-autoconfig))))

(define-public wasm4
  (package
    (name "wasm4")
    (version "2.5.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/aduros/wasm4")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0ycnznwy4i4fw6l507y5xm986rxqvnpl971725q8xinsnq2swpnl"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f ; no check target
      #:configure-flags
      #~(list (string-append "-DCMAKE_C_FLAGS="
                             "-I" #$minifb "/include "
                             "-I" #$wasm3 "/include"))
      #:phases
      '(modify-phases %standard-phases
         ;; WASM4's source is a combination of multiple runtimes.  We want to
         ;; build the native one.
         (add-after 'unpack 'chdir-to-native-runtime
           (lambda _
             (chdir "runtimes/native")))
         ;; WASM4 uses git submodules to bundle several dependencies, which we
         ;; have instead made dedicated packages for.  This phase hacks the
         ;; build system to use our own stuff.
         (add-after 'chdir-to-native-runtime 'unbundle
           (lambda _
             (substitute* "CMakeLists.txt"
               ;; These directories do not exist because we aren't pulling in
               ;; submodules.
               (("add_subdirectory\\(vendor/minifb\\)") "")
               (("add_subdirectory\\(vendor/cubeb\\)") "")
               ;; Add additional libraries needed to successfully link the
               ;; wasm4 executable using the unbundled dependencies.
               (("target_link_libraries\\(wasm4 minifb cubeb\\)")
                "target_link_libraries(wasm4 m GL X11 xkbcommon minifb cubeb m3)")))))))
    (inputs (list cubeb minifb wasm3))
    (synopsis "WebAssembly fantasy console")
    (description "WASM-4 is a low-level fantasy game console for building
small games with WebAssembly.  Game cartridges (ROMs) are small,
self-contained .wasm files that can be built with any programming language
that compiles to WebAssembly.")
    (home-page "https://wasm4.org")
    (license license:isc)))

(define-public scummvm
  (package
    (name "scummvm")
    (version "2.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://downloads.scummvm.org/frs/scummvm/" version
                           "/scummvm-" version ".tar.xz"))
       (sha256
        (base32 "0lllm5fsmb5gdrxnpfryyl85i4sb1dkrqw97j7q4glkhplr3bcym"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f                                   ;require "git"
      #:configure-flags #~(list "--enable-release") ;for optimizations
      #:phases
      #~(modify-phases %standard-phases
          (replace 'configure
            ;; configure does not work followed by both "SHELL=..." and
            ;; "CONFIG_SHELL=..."; set environment variables instead
            (lambda* (#:key inputs configure-flags #:allow-other-keys)
              (let ((bash (search-input-file inputs "/bin/bash"))
                    (flags `(,(string-append "--prefix=" #$output)
                             ,@configure-flags)))
                (setenv "SHELL" bash)
                (setenv "CONFIG_SHELL" bash)
                (apply invoke "./configure" flags)))))))
    (native-inputs
     (list nasm pkg-config))
    (inputs
     (list alsa-lib
           faad2
           fluidsynth
           freetype
           fribidi
           glew
           giflib
           liba52
           flac
           libjpeg-turbo
           libmad
           libmpeg2
           libogg
           libpng
           libtheora
           libvorbis
           (sdl-union (list sdl2 sdl2-net))
           zlib))
    (home-page "https://www.scummvm.org/")
    (synopsis "Engine for several graphical adventure games")
    (description "ScummVM is a program which allows you to run certain
classic graphical point-and-click adventure games, provided you
already have their data files.  The clever part about this: ScummVM
just replaces the executables shipped with the games, allowing you to
play them on systems for which they were never designed!")
    (license license:gpl2+)))

(define-public libticables2
  (package
    (name "libticables2")
    (version "1.3.5")
    (source (origin
              (method url-fetch)
              (uri "https://www.ticalc.org/pub/unix/tilibs.tar.gz")
              (sha256
               (base32
                "07cfwwlidgx4fx88whnlch6y1342x16h15lkvkkdlp2y26sn2yxg"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags (list "--enable-libusb10")
       #:phases
       (modify-phases %standard-phases
         (replace 'unpack
           (lambda* (#:key source #:allow-other-keys)
             (invoke "tar" "xvkf" source)
             (invoke "tar" "xvkf"
                     (string-append "tilibs2/libticables2-"
                                    ,version ".tar.bz2"))
             (chdir (string-append "libticables2-" ,version))
             #t)))))
    (native-inputs
     (list autoconf
           autogen
           automake
           gnu-gettext
           libtool
           pkg-config))
    (inputs
     (list glib libusb))
    (synopsis "Link cable library for TI calculators")
    (description
     "This package contains libticables, a library for operations on
@acronym{TI, Texas Instruments} calculator link cables.

This is a part of the TiLP project.")
    (home-page "http://lpg.ticalc.org/prj_tilp/")
    (license license:gpl2+)))

(define-public libticonv
  (package
    (name "libticonv")
    (version "1.1.5")
    (source (origin
              (method url-fetch)
              (uri "https://www.ticalc.org/pub/unix/tilibs.tar.gz")
              (sha256
               (base32
                "07cfwwlidgx4fx88whnlch6y1342x16h15lkvkkdlp2y26sn2yxg"))))
    (build-system gnu-build-system)
    (arguments
     ;; build fails with out --enable-iconv (...?)
     `(#:configure-flags (list "--enable-iconv")
       #:phases
       (modify-phases %standard-phases
         (replace 'unpack
           (lambda* (#:key source #:allow-other-keys)
             (invoke "tar" "xvkf" source)
             (invoke "tar" "xvkf"
                     (string-append "tilibs2/libticonv-"
                                    ,version ".tar.bz2"))
             (chdir (string-append "libticonv-" ,version))
             #t)))))
    (native-inputs
     (list autoconf automake libtool pkg-config))
    (inputs
     (list glib))
    (synopsis "Character conversion library for TI calculators")
    (description
     "This package contains libticonv, a library to support working with
@acronym{TI, Texas Instruments} calculator charsets.

This is a part of the TiLP project.")
    (home-page "http://lpg.ticalc.org/prj_tilp/")
    (license license:gpl2+)))

(define-public libtifiles2
  (package
    (name "libtifiles2")
    (version "1.1.7")
    (source (origin
              (method url-fetch)
              (uri "https://www.ticalc.org/pub/unix/tilibs.tar.gz")
              (sha256
               (base32
                "07cfwwlidgx4fx88whnlch6y1342x16h15lkvkkdlp2y26sn2yxg"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'unpack
           (lambda* (#:key source #:allow-other-keys)
             (invoke "tar" "xvkf" source)
             (invoke "tar" "xvkf"
                     (string-append "tilibs2/libtifiles2-"
                                    ,version ".tar.bz2"))
             (chdir (string-append "libtifiles2-" ,version))
             #t)))))
    (native-inputs
     (list autoconf automake gnu-gettext libtool pkg-config))
    (inputs
     (list glib libarchive libticonv))
    (synopsis "File functions library for TI calculators")
    (description
     "This package contains libticonv, a library to support working with
@acronym{TI, Texas Instruments} calculator files.

This is a part of the TiLP project.")
    (home-page "http://lpg.ticalc.org/prj_tilp/")
    (license license:gpl2+)))

(define-public libticalcs2
  (package
    (name "libticalcs2")
    (version "1.1.9")
    (source (origin
              (method url-fetch)
              (uri "https://www.ticalc.org/pub/unix/tilibs.tar.gz")
              (sha256
               (base32
                "07cfwwlidgx4fx88whnlch6y1342x16h15lkvkkdlp2y26sn2yxg"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'unpack
           (lambda* (#:key source #:allow-other-keys)
             (invoke "tar" "xvkf" source)
             (invoke "tar" "xvkf"
                     (string-append "tilibs2/libticalcs2-"
                                    ,version ".tar.bz2"))
             (chdir (string-append "libticalcs2-" ,version))
             #t)))))
    (native-inputs
     (list autoconf automake gnu-gettext libtool pkg-config))
    (inputs
     (list glib libarchive libticables2 libticonv libtifiles2))
    (synopsis "Support library for TI calculators")
    (description
     "This project aims to develop a multi-platform linking program for use
with all @acronym{TI, Texas Instruments} graphing calculators (TI73 to
V200PLT).

This is a part of the TiLP project.")
    (home-page "http://lpg.ticalc.org/prj_tilp/")
    (license license:gpl2+)))

(define-public mame
  (package
    (name "mame")
    (version "0.252")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mamedev/mame")
             (commit (apply string-append "mame" (string-split version #\.)))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "07qhcm1v47sy2wj30nx3cbhvcbgki0cl83gabr0miiw60fhgyn6j"))
       (modules '((guix build utils)))
       (snippet
        ;; Remove bundled libraries.
        '(begin
           (with-directory-excursion "3rdparty"
             (for-each delete-file-recursively
                       '("asio" "expat" "glm" "libflac" "libjpeg" "lua"
                         "portaudio" "portmidi" "pugixml" "rapidjson" "SDL2"
                         "SDL2-override" "sqlite3" "utf8proc" "zlib")))))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:make-flags
      #~(cons*
         ;; A 'strict-overflow' error pops up on i686 so disable '-Werror'.
         "NOWERROR=1"
         (string-append "QT_HOME=" #$(this-package-input "qtbase"))
         (string-append "SDL_INI_PATH=" #$output "/share/mame/ini")
         (map (lambda (lib)
                (string-append "USE_SYSTEM_LIB_" (string-upcase lib) "=1"))
              '("asio" "expat" "flac" "glm" "jpeg" "lua" "portaudio" "portmidi"
                "pugixml" "rapidjson" "sqlite3" "utf8proc" "zlib")))
      #:tests? #f                       ;no test in regular release
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (add-after 'build 'build-documentation
            (lambda _ (invoke "make" "-C" "docs" "man" "info")))
          (replace 'install
            ;; Upstream does not provide an installation phase.
            (lambda _
              (let ((share (string-append #$output "/share/mame")))
                ;; Install data.
                (for-each (lambda (dir)
                            (copy-recursively dir (string-append share "/" dir)))
                          '("artwork" "bgfx" "ctrlr" "hash" "ini" "language"
                            "plugins" "samples"))
                (let ((keymaps (string-append share "/keymaps")))
                  (for-each (lambda (file) (install-file file keymaps))
                            (find-files "keymaps" ".*LINUX\\.map")))
                (let ((fonts (string-append share "/fonts")))
                  (install-file "uismall.bdf" fonts))
                (when (file-exists? "mame64")
                  (rename-file "mame64" "mame"))
                (install-file "mame" (string-append #$output "/bin")))))
          (add-after 'install 'install-documentation
            (lambda _
              (let ((man (string-append #$output "/share/man/man1"))
                    (info (string-append #$output "/share/info")))
                (install-file "docs/build/man/MAME.1" man)
                (install-file "docs/build/texinfo/MAME.info" info))))
          (add-after 'install 'install-ini-file
            ;; Generate an ini file so as to set some directories (e.g., roms)
            ;; to a writable location, i.e., "$HOME/.mame/" and "$HOME/mame/".
            ;;
            ;; XXX: We need to insert absolute references to the store.  It can
            ;; be an issue if they leak into user's home directory, e.g., with
            ;; "mame -createconfig" and the package is later GC'ed.
            (lambda _
              (let* ((share (string-append #$output "/share/mame"))
                     (ini (string-append share "/ini")))
                (with-output-to-file (string-append ini "/mame.ini")
                  (lambda _
                    (format #t
                            "inipath              $HOME/.mame;~a/ini~@
                            homepath             $HOME/mame~@
                            rompath              $HOME/mame/roms~@
                            samplepath           $HOME/mame/samples;~a/samples~@
                            cheatpath            $HOME/mame/cheat~@
                            artpath              $HOME/mame/artwork;~a/artwork~@
                            crosshairpath        $HOME/mame/crosshair~@
                            snapshot_directory   $HOME/mame/snapshots~@
                            hashpath             ~a/hash~@
                            fontpath             $HOME/mame/fonts;~a/fonts~@
                            ctrlrpath            $HOME/mame/ctrlr;~a/ctrlr~@
                            bgfx_path            ~a/bgfx~@
                            pluginspath          $HOME/mame/plugins;~a/plugins~@
                            languagepath         ~a/language~@
                            cfg_directory        $HOME/.mame/cfg~@
                            nvram_directory      $HOME/.mame/nvram~@
                            input_directory      $HOME/.mame/inp~@
                            state_directory      $HOME/.mame/sta~@
                            diff_directory       $HOME/.mame/diff~@
                            comment_directory    $HOME/.mame/comments~%"
                            share share share share share share share share
                            share)))
                (with-output-to-file (string-append ini "/ui.ini")
                  (lambda _
                    (format #t
                            "historypath          $HOME/mame/history~@
                            categorypath         $HOME/mame/folders~@
                            cabinets_directory   $HOME/mame/cabinets~@
                            cpanels_directory    $HOME/mame/cpanel~@
                            pcbs_directory       $HOME/mame/pcb~@
                            flyers_directory     $HOME/mame/flyers~@
                            titles_directory     $HOME/mame/titles~@
                            ends_directory       $HOME/mame/ends~@
                            marquees_directory   $HOME/mame/marquees~@
                            artwork_preview_directory $HOME/mame/artpreview~@
                            bosses_directory     $HOME/mame/bosses~@
                            logos_directory      $HOME/mame/logo~@
                            scores_directory     $HOME/mame/scores~@
                            versus_directory     $HOME/mame/versus~@
                            gameover_directory   $HOME/mame/gameover~@
                            howto_directory      $HOME/mame/howto~@
                            select_directory     $HOME/mame/select~@
                            icons_directory      $HOME/mame/icons~@
                            covers_directory     $HOME/mame/covers~@
                            ui_path              $HOME/.mame/ui~%"))))))
          (add-after 'install 'install-desktop-file
            (lambda _
              (let ((desktop (string-append #$output "/share/applications"))
                    (executable (string-append #$output "/bin/mame")))
                (mkdir-p desktop)
                (with-output-to-file (string-append desktop "/mame.desktop")
                  (lambda _
                    (format #t
                            "[Desktop Entry]~@
                           Name=mame~@
                           Comment=Multi-purpose emulation framework~@
                           Exec=~a~@
                           TryExec=~@*~a~@
                           Terminal=false~@
                           Type=Application~@
                           Categories=Game;Emulator;~@
                           Keywords=Game;Emulator;Arcade;~%"
                            executable)))))))))
    (native-inputs
     (list pkg-config
           python-sphinx
           python-sphinxcontrib-svg2pdfconverter
           texinfo))
    (inputs
     (list alsa-lib
           asio
           expat
           flac
           fontconfig
           glm
           libjpeg-turbo
           libxi
           libxinerama
           lua
           portaudio
           portmidi
           pugixml
           pulseaudio
           python-wrapper
           qtbase-5
           rapidjson
           (sdl-union (list sdl2 sdl2-ttf))
           sqlite
           utf8proc
           zlib))
    (home-page "https://www.mamedev.org")
    (synopsis "Multi-purpose emulation framework")
    (description "MAME's purpose is to preserve decades of software
history.  As electronic technology continues to rush forward, MAME
prevents this important @emph{vintage} software from being lost and
forgotten.  This is achieved by documenting the hardware and how it
functions.  The source code to MAME serves as this documentation.")
    ;; The MAME project as a whole is distributed under the terms of GPL2+.
    ;; However, over 90% of the files are under Expat license.  Also, artwork,
    ;; keymaps, languages and samples are under CC0.
    (license (list license:gpl2+ license:expat license:cc0))))

(define-public gnome-arcade
  (package
    (name "gnome-arcade")
    (version "0.240")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/strippato/gnome-arcade")
             (commit (string-append "v." version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "110dpbbcj73s3i2zcnay0kdpsngcpq8mif88279pdc2967ld0a6r"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f                       ; No tests.
      #:configure-flags
      #~(list
         (string-append "-DMAME_BIN=\""
                        #$(this-package-input "mame")
                        "/bin/mame\"")
         (string-append "-DAPP_RES=\"" #$output "/share/gnome-arcade/\""))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'fix-paths
            (lambda _
              (substitute* "../source/src/config.c"
                (("/usr/share") (string-append #$output "/share")))))
          (replace 'install
            (lambda _
              (let ((bin (string-append #$output "/bin"))
                    (rom (string-append #$output
                                        "/share/gnome-arcade/data/rom"))
                    (tile (string-append #$output
                                         "/share/gnome-arcade/data/tile")))
                (mkdir-p bin)
                (install-file "../gnome-arcade" bin)
                (copy-recursively "../source/res"
                                  (string-append #$output
                                                 "/share/gnome-arcade/res"))
                (mkdir-p rom)
                (install-file "../source/data/rom/ROM.TXT" rom)
                (mkdir-p tile)
                (install-file "../source/data/tile/TILE.TXT" tile)))))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list gtk+ libarchive libevdev mame vlc))
    (home-page "https://github.com/strippato/gnome-arcade")
    (synopsis "Minimal MAME frontend")
    (description
     "Gnome Arcade is a minimal GTK+ frontend for MAME, the multi-purpose
arcade and console emulator.")
    (license license:gpl3+)))

(define-public gnusim8085
  (package
    (name "gnusim8085")
    (version "1.4.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/srid/GNUSim8085/releases/download/"
                    version "/gnusim8085-" version ".tar.gz"))
              (sha256
               (base32
                "05x0is0ckagb3r74p6lw9b8nqrrh7q2v4jvc4cnhljchz9x7kw2a"))))
    (native-inputs (list pkg-config))
    (inputs (list gtksourceview-3 adwaita-icon-theme))
    (build-system glib-or-gtk-build-system)
    (home-page "https://gnusim8085.srid.ca")
    (synopsis "Graphical simulator for the Intel 8085 microprocessor")
    (description
     "GNUSim8085 is a graphical simulator,
assembler, and debugger for the Intel 8085 microprocessor.

@itemize
@item A simple editor component with syntax highlighting.
@item A keypad to input assembly language instructions with appropriate arguments.
@item Easy view of register contents.
@item Easy view of flag contents.
@item Hexadecimal/decimal converter.
@item View of stack, memory and I/O contents.
@item Support for breakpoints for program debugging.
@item Stepwise program execution.
@item One click conversion of assembly program to opcode listing.
@item Printing support.
@item UI translated in various languages.
@end itemize")
    (license license:gpl2+)))

(define-public pcsxr
  ;; No release since 2017.
  (let ((commit "6484236cb0281e8040ff6c8078c87899a3407534"))
    (package
      (name "pcsxr")
      ;; Version is tagged here: https://github.com/frealgagu/PCSX-Reloaded
      (version "1.9.95")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/pcsxr/PCSX-Reloaded")
               (commit commit)))
         (sha256
          (base32
           "138mayp7zi9v4l3lm5f6xxkds619w1fgg769zm8s45c84jbz7dza"))
         (file-name (git-file-name name commit))))
      (build-system cmake-build-system)
      (arguments
       `(#:tests? #f                    ;no "test" target
         #:configure-flags
         (list "-DSND_BACKEND=pulse"
               "-DENABLE_CCDDA='ON'"
               "-DUSE_LIBARCHIVE='ON'"
               "-DUSE_LIBCDIO='ON'")
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'cd-subdir
             (lambda _ (chdir "pcsxr") #t))
           (add-before 'configure 'fix-cdio-lookup
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "cmake/FindCdio.cmake"
                 (("/usr/include/cdio")
                  (search-input-directory inputs "/include/cdio")))))
           (add-after 'install 'wrap-program
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (wrap-program (string-append (assoc-ref outputs "out")
                                            "/bin/pcsxr")
                 ;; For GtkFileChooserDialog.
                 `("GSETTINGS_SCHEMA_DIR" =
                   (,(string-append (assoc-ref inputs "gtk+")
                                    "/share/glib-2.0/schemas")))))))))
      (native-inputs
       (list pkg-config intltool
             `(,glib "bin")))
      (inputs
       (list bash-minimal
             libcdio
             sdl2
             gtk+
             ffmpeg-4
             libxv
             libarchive
             pulseaudio))
      (home-page "https://archive.codeplex.com/?p=pcsxr")
      (synopsis "PlayStation emulator")
      (description
       "A PlayStation emulator based on PCSX-df Project with bugfixes and
improvements.")
      (license license:gpl2+))))

(define-public gens-gs
  (package
    (name "gens-gs")
    (version "7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://retrocdn.net/images/6/6d/Gens-gs-r"
                           version ".tar.gz"))
       (sha256
        (base32
         "1ha5s6d3y7s9aq9f4zmn9p88109c3mrj36z2w68jhiw5xrxws833"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     `(#:system "i686-linux"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-CFLAGS
           (lambda* _
             ;; Remove GTK API deprecation flags that cause build errors.
             (substitute* "configure"
               (("GTK_CFLAGS=\"\\$GTK_CFLAGS .*\"") ""))
             #t)))))
    (native-inputs
     (list pkg-config nasm))
    (inputs
     `(("sdl" ,sdl)
       ("gtk" ,gtk+-2)))
    (home-page "https://segaretro.org/Gens/GS")
    (synopsis "Emulator for Sega Genesis/Mega Drive systems")
    (description
     "Gens/GS is an emulator for the Mega Drive (also known as Sega Genesis),
derived from Gens.  Project goals include clean source code, combined features
from various forks of Gens, and improved platform portability.")
    (supported-systems '("i686-linux" "x86_64-linux"))
    (license license:gpl2+)))

(define-public bsnes
  (package
    (name "bsnes")
    (version "115")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/bsnes-emu/bsnes")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0j054x38fwai61vj36sc04r3zkzay5acq2cgd9zqv5hs51s36g5b"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:make-flags #~(list "-C" "bsnes"
                           ;; Remove march=native
                           "local=false"
                           (string-append "prefix=" #$output))
      #:tests? #f                       ;No tests.
      #:phases #~(modify-phases %standard-phases
                   (delete 'configure))))
    (native-inputs (list pkg-config))
    (inputs
     (list alsa-lib
           ao
           cairo
           eudev
           gtksourceview-2
           libxrandr
           libxv
           openal
           pulseaudio
           sdl2))
    (home-page "https://github.com/bsnes-emu/bsnes")
    (synopsis "Emulator for the Super Nintendo / Super Famicom systems")
    (description
     "bsnes is a Super Nintendo / Super Famicom emulator that focuses on
performance, features, and ease of use.")
    (license license:gpl3)))

(define-public bsnes-hd
  (package
    (inherit bsnes)
    (name "bsnes-hd")
    ;; As of 10.6, there only ever was beta releases -- treat these as the
    ;; stable releases for now.
    (version "10.6")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/DerKoun/bsnes-hd")
                    (commit (string-append
                             "beta_"
                             (string-replace-substring version "." "_")))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0f3cd89fd0lqskzj98cc1pzmdbscq0psdjckp86w94rbchx7iw4h"))))
    (build-system gnu-build-system)
    (home-page "https://github.com/DerKoun/bsnes-hd/")
    (synopsis "Fork of bsnes with added HD video features")
    (description "bsnes-hd (called ``HD Mode 7 mod, for bsnes'' in early
betas) is a fork of bsnes (the great SNES emulator by Near) that adds HD video
features, such as:
@table @asis
@item HD Mode 7
Renders the rotated, scaled or pseudo perspective backgrounds at
higher resolutions.  This does not involve new custom imagery or upscaling
algorithms.  It is a higher resolution version of the process the SNES uses.
@item Widescreen
Extends the scenes to the left and right, without distorting them.  It works
for most Mode 7 scenes, but also for some other scenes/games, after some
settings tweaking.
@item True color
Color calculation are done at true color instead of the SNES color depth (3x8
instead of 3x5 bit).  With the optional line color smoothing color ``steps''
turn into actual gradients (without influencing the sharpness of the artwork).
@end table")
    (license license:gpl3+)))

(define-public libretro-bsnes-hd
  (package/inherit bsnes-hd
    (name "libretro-bsnes-hd")
    (arguments
     (substitute-keyword-arguments (package-arguments bsnes-hd)
       ((#:make-flags flags ''())
        #~(cons "target=libretro" #$flags))
       ((#:phases phases '%standard-phases)
        #~(modify-phases #$phases
            (replace 'install           ;no install target
              (lambda _
                (install-file "bsnes/out/bsnes_hd_beta_libretro.so"
                              (string-append #$output "/lib/libretro/"))))))))
    (synopsis "Libretro port of bsnes-hd")))

(define-public jg-api
  (package
    (name "jg-api")
    (version "1.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.com/jgemu/jg")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0117cvfvzhrm9fxnryhbnf9r0f8ij4ahhfqiqp5yv11bz2wcyhqh"))))
    (build-system gnu-build-system)
    (arguments
     (list #:tests? #f                  ;no test suite
           #:make-flags #~(list (string-append "PREFIX=" #$output))
           #:phases #~(modify-phases %standard-phases
                        (delete 'configure)))) ;no configure phase
    (home-page "https://gitlab.com/jgemu/jg")
    (synopsis "Emulators Plugin API")
    (description "This package provides the Jolly Good API C and C++ headers.
The Jolly Good API is a shared object or plugin @acronym{API, Application
Programming Interface} for emulators.")
    (license license:zlib)))

(define-public jgrf
  (package
    (name "jgrf")
    (version "1.2.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.com/jgemu/jgrf")
                    (commit version)))
              (file-name (git-file-name name version))
              (snippet '(begin
                          ;; TODO: Package md5.h and md5.c from
                          ;; http://openwall.info/wiki/people/solar/software
                          ;; /public-domain-source-code/md5,
                          ;; remove these bundled files and set the
                          ;; USE_EXTERNAL_MD5 Make flag to 1.
                          ;; (delete-file "deps/md5.h")
                          ;; (delete-file "deps/md5.c")
                          (use-modules (guix build utils))
                          (delete-file-recursively "deps/miniz")))
              (sha256
               (base32
                "1ivc8jj0majvgi0rj9nn429bmh7wp2nf87hq8xg05fjqwalfy3bl"))))
    (build-system gnu-build-system)
    (arguments
     (list #:tests? #f                ;no test suite
           #:make-flags
           #~(list (string-append "AR=" #$(ar-for-target))
                   (string-append "CC=" #$(cc-for-target))
                   (string-append "CXX=" #$(cxx-for-target))
                   (string-append "PREFIX=" #$output))
           #:phases #~(modify-phases %standard-phases
                        (delete 'configure))))
    (native-inputs (list jg-api pkg-config))
    (inputs
     (list flac
           libepoxy
           libsamplerate
           lzo
           miniz
           sdl2
           soxr
           speexdsp
           zlib
           `(,zstd "lib")))
    (native-search-paths
     (list (search-path-specification
            (variable "JOLLYGOOD_CORE_DIRS")
            (files '("lib/jollygood")))
           (search-path-specification
            (variable "JOLLYGOOD_ASSET_DIRS")
            (files '("share/jollygood")))))
    (home-page "https://gitlab.com/jgemu/jgrf")
    (synopsis "Jolly Good Reference Frontend")
    (description "The Jolly Good Reference Frontend (accessible via the
@command{jollygood} command) aims to be the simplest possible frontend to The
Jolly Good API.  It may be used to run emulators built as shared objects, or
as a \"white-label\" frontend for statically linked standalone emulators.")
    ;; The main license is BSD-3; the bundled source licenses are also listed
    ;; below.
    (license (list license:bsd-3 ;this software, gltext.h, lodepng.c, lodepng.h
                   license:expat ;ezmenu.h source, musl_memmem.c,
                                        ;parson.h, parson.c, tconfig.h, tconfig.c
                   license:public-domain ;md5.h, md5.c, parg.h, parg.c
                   license:cc0))))

(define-public jg-bsnes
  (package
    (name "jg-bsnes")
    (version "2.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.com/jgemu/bsnes")
                    (commit version)))
              (file-name (git-file-name name version))
              ;; XXX: Some source dependencies are bundled and are not easy to
              ;; unbundle due to the build system building an object combining
              ;; their sources directly (see:
              ;; https://gitlab.com/jgemu/bsnes/-/issues/6).
              ;; - byuuML (no build system)
              ;; - gb (the 'Core' sources of SameBoy)
              ;; - libco (no build system)
              ;; - snes_spc (also modified by this project)
              (snippet '(begin
                          (use-modules (guix build utils))
                          (delete-file-recursively "deps/libsamplerate")))
              (sha256
               (base32
                "0z1ka4si8vcb0j6ih087cni18vpgfd3qnaw24awycxz23xc0jkdv"))))
    (build-system gnu-build-system)
    (arguments
     (list #:tests? #f                  ;no test suite
           #:make-flags
           #~(list (string-append "AR=" #$(ar-for-target))
                   (string-append "CC=" #$(cc-for-target))
                   (string-append "CXX=" #$(cxx-for-target))
                   (string-append "PREFIX=" #$output))
           #:phases #~(modify-phases %standard-phases
                        (delete 'configure)))) ;no configure script
    (native-inputs (list jg-api pkg-config))
    (inputs (list libsamplerate))
    (home-page "https://gitlab.com/jgemu/bsnes")
    (synopsis "Jolly Good Fork of bsnes")
    (description "@code{bsnes-jg} is a cycle accurate emulator for the Super
Famicom/Super Nintendo Entertainment System, including support for the Super
Game Boy, BS-X Satellaview, and Sufami Turbo.  @code{bsnes-jg} is a fork of
@code{bsnes} v115, Many changes have been made post-fork:
@itemize
@item Higher quality resampler with settings
@item Improved performance without loss of accuracy
@item Portability improvements
@item Removal of accuracy-reducing hacks and unnecessary code
@item Significant increase in standards compliance
@item Translation to the C++ Standard Library (ISO C++11)
@end itemize

In particular, it uses much less @acronym{CPU, Central Processing Unit}
compared to the original @code{bsnes} (though not as little as @code{zsnes}).

The supported file formats are:
@itemize @file
@item .sfc
@item .smc
@item .bs
@item .st
@item .fig
@item .swc
@end itemize

This is intended to be used with the Jolly Good Reference Frontend
@command{jollygood} command from the @code{jgrf} package.")
    ;; The project license is GPL3+.  The bundled source licenses are also
    ;; listed below.
    (license (list license:gpl3+
                   license:bsd-3        ;byuuML
                   license:expat        ;gb
                   license:isc          ;libco
                   license:lgpl2.1+))))

(define-public libretro-beetle-gba
  ;; There are no releases.  Use the latest commit.
  (let ((commit "6cee80685f735ea6c2373db2622a1f1ee9f39d39")
        (revision "0"))
    (package
      (name "libretro-beetle-gba")
      ;; Use Mednafen core version as base.  Defined in libretro.cpp:73.
      (version (git-version "0.9.36" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/libretro/beetle-gba-libretro")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "14fm2g3hrsvvd57d6m9apzc30ypa4m0m5hk2viq422fm2l9y0xbb"))))
      (build-system gnu-build-system)
      (arguments
       (list #:make-flags #~(list (string-append "CC=" #$(cc-for-target))
                                  (string-append "GIT_VERSION=" #$commit)
                                  (string-append "prefix=" #$output))
             #:tests? #f                ;no tests
             #:phases #~(modify-phases %standard-phases
                          (delete 'configure))))
      (home-page "https://github.com/libretro/beetle-gba-libretro")
      (synopsis "Standalone port of Mednafen GBA to libretro")
      (description
       "A standalone port of Mednafen’s GameBoy Advance emulator called Beetle
GBA to libretro.  Beetle GBA is based on VBA-M, itself a fork of Visual Boy
Advance.")
      (license license:gpl2+))))

(define-public libretro-bsnes-jg
  ;; There aren't any release yet; use the latest commit.
  (let ((commit "0d42dea0cb20aba8bfec05b928e4aed2b295352a")
        (revision "0"))
    (package
      (inherit jg-bsnes)
      (name "libretro-bsnes-jg")
      (version (git-version "0" revision commit))
      (source (origin
                (inherit (package-source jg-bsnes))
                (uri (git-reference
                      (url "https://git.libretro.com/libretro/bsnes-jg")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1dq2ypf4g4karayc9sgqn74bfnnsq2f4b3r615xyczchdaf2mi1n"))))
      (arguments
       (substitute-keyword-arguments (package-arguments jg-bsnes)
         ((#:make-flags flags)
          #~(cons* #$(string-append "GIT_VERSION=" version)
                   (string-append "prefix=" #$output)
                   #$flags))
         ((#:phases phases)
          #~(modify-phases #$phases
              (add-after 'unpack 'chdir
                (lambda _
                  (chdir "libretro")))
              (add-after 'chdir 'unbundle-libsamplerate
                (lambda _
                  (substitute* "Makefile.common"
                    (("LIBS \\+= -lm")
                     "LIBS += -lm -lsamplerate")
                    ((".*\\$\\(CORE_DIR)/deps/libsamplerate/.*")
                     ""))))))))
      (home-page "https://git.libretro.com/libretro/bsnes-jg")
      (synopsis "libretro port of bsnes-jg"))))

(define-public jg-nestopia
  (package
    (name "jg-nestopia")
    (version "1.52.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.com/jgemu/nestopia")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "19qg9hgh25aaym7b81v5g7165v4fyymas6dmzc4z867mzaphbn6s"))))
    (build-system gnu-build-system)
    (arguments
     (list #:tests? #f                  ;no test suite
           #:make-flags
           #~(list (string-append "AR=" #$(ar-for-target))
                   (string-append "CC=" #$(cc-for-target))
                   (string-append "CXX=" #$(cxx-for-target))
                   (string-append "PREFIX=" #$output))
           #:phases #~(modify-phases %standard-phases
                        (delete 'configure)))) ;no configure script
    (native-inputs (list jg-api pkg-config))
    (home-page "https://gitlab.com/jgemu/nestopia")
    (synopsis "Jolly Good Fork of Nestopia")
    (description "Nestopia JG is an emulator for the Nintendo Entertainment
System/Famicom, including support for the Famicom Disk System and VS. System.
Though originally a fork, Nestopia JG has become the de facto upstream branch
of the Nestopia emulator.")
    (license (list license:gpl2+        ;this project
                   license:lgpl2.1+)))) ;nes_ntsc source files

(define-public jg-cega
  (package
    (name "jg-cega")
    (version "0.6.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.com/jgemu/cega")
                    (commit version)))
              (modules '((guix build utils)))
              (snippet '(delete-file-recursively "deps/"))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "10qxfch08850zivxf4s1mhh0clx4h1cfn440acm6d7glb6wbv822"))))
    (build-system gnu-build-system)
    (arguments
     (list #:tests? #f                  ;no test suite
           #:make-flags
           #~(list (string-append "CC=" #$(cc-for-target))
                   (string-append "PREFIX=" #$output))
           #:phases #~(modify-phases %standard-phases
                        (delete 'configure))))
    (native-inputs (list jg-api pkg-config))
    (inputs (list speexdsp))
    (home-page "https://gitlab.com/jgemu/cega")
    (synopsis "Jolly Good SG-1000, SMS, Game Gear, and Mega Drive/Genesis \
emulator")
    (description "Cega is a cycle accurate emulator for the Sega SG-1000,
Master System, and Game Gear written specifically for The Jolly Good API.
Mega Drive emulation is in an experimental state.")
    (license (list license:mpl2.0
                   license:expat        ;src/emu2413, src/m68k
                   license:bsd-3        ;src/ymfm
                   license:zlib))))     ;src/z80.h

(define-public zsnes
  (package
    (name "zsnes")
    (version "2.0.12")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/xyproto/zsnes")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0g9l1ij3p1adkp97wkp0dz44i2xpmsvfpkxvlfkpr7190dibsgsz"))))
    (build-system gnu-build-system)
    (arguments
     (list #:system "i686-linux"        ;requires 32 bit libraries to build
           #:tests? #f                  ;no test suite
           #:make-flags
           #~(list (string-append "CC=" #$(cc-for-target))
                   (string-append "CXX=" #$(cxx-for-target))
                   (string-append "PREFIX=" #$output))
           #:phases #~(modify-phases %standard-phases
                        (delete 'configure)))) ;no configure script
    (native-inputs (list nasm pkg-config))
    (inputs (list glib libpng mesa ncurses sdl zlib))
    (home-page "https://www.zsnes.com")
    (synopsis "Super Nintendo Entertainment System emulator")
    (description "ZSNES is a @acronym{Super Nintendo Entertainment System,
SNES} emulator that can play most games at full speed with sound and special
graphic filters.  Some of its features include:
@itemize
@item Support for smooth and dynamic image scaling
@item Support for rewinding and fast-forwarding in-game
@item JMA compression format
@item Change the appearance of the GUI
@item Take screenshots of currently running games
@item Saving the game at any point by recording the console’s state
@item Record movies of gameplay which can be played back.
@end itemize")
    (license license:gpl2+)
    (supported-systems (list "x86_64-linux"))))

(define-public unicorn
  (package
    (name "unicorn")
    (version "2.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri name version))
       (sha256
        (base32 "18sbrycr62wcs3a68a9q76ihpahfsd4bn3mryvyhimwwn1342kwh"))
       (modules '((guix build utils)))
       ;; cmake files are not in the cmake dir in pypi
       (snippet #~(substitute* "src/CMakeLists.txt"
                    (("include\\(cmake/")
                     "include(")))))
    (build-system pyproject-build-system)
    (native-inputs (list cmake pkg-config python-setuptools python-wheel))
    (home-page "https://www.unicorn-engine.org")
    (synopsis "Generic CPU emulator framework")
    (description
     "Uniforn is a lightweight, multi-platform, multi-architecture CPU
emulator framework based on QEMU.")
    (license license:gpl2+)))

(define-public ppsspp
  (package
    (name "ppsspp")
    (version "1.14.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hrydgard/ppsspp")
             (commit (string-append "v" version))))
       (sha256
        (base32 "1l80zs1khph4a3g3hnh91awafmyy6wdcayb81xnflkzmpv3bwq8i"))
       (file-name (git-file-name name version))
       (patches
        (search-patches "ppsspp-disable-upgrade-and-gold.patch"))
       (modules '((guix build utils)))
       (snippet
        `(begin
           ;; The following is quite a heavy-handed way of unbundling PPSSPP.
           ;; There are still a number of external sources, that we don't
           ;; remove here.  Some may be packaged, others are not.
           ;; First, we patch existing sources to include the right headers.
           (substitute* (append (find-files "Common" ".*\\.(h|cpp)")
                                (find-files "Core" ".*\\.(h|cpp)")
                                (find-files "GPU" ".*\\.(h|cpp)")
                                (find-files "SDL" ".*\\.(h|cpp)")
                                (find-files "UI" ".*\\.(h|cpp)"))
             ;; These headers are all hard-coded in the original source.
             (("ext/cityhash/") "")
             (("ext/glslang/glslang/") "glslang/")
             (("ext/glslang/") "glslang/")
             (("ext/miniupnp/") "")
             (("ext/SPIRV-Cross/") "spirv_cross/")
             (("ext/vulkan/") "vulkan/")
             (("ext/xxhash.h") "xxhash.h")
             ;; These definitions do not actually exist in the Vulkan headers,
             ;; but PPSSPP defines them in ext/vulkan.
             (("VK_FORMAT_BEGIN_RANGE") "VK_FORMAT_UNDEFINED")
             (("VK_FORMAT_END_RANGE") "VK_FORMAT_ASTC_12x12_SRGB_BLOCK"))
           ;; Next, we patch CMakeLists.
           (substitute* "CMakeLists.txt"
             ;; Drop unnecessary includes and targets.
             (("include_directories\\(ext/glslang\\)") "")
             (("target_include_directories\\(.*ext/xxhash\\)") "")
             (("target_include_directories\\(.*ext/cityhash\\)") "")
             (("set_target_properties\\(cityhash .*\\)") "")
             ;; Fix linking to GLEW.
             (("TARGET Ext::GLEW") "true")
             (("target_link_libraries\\(native Ext::GLEW\\)")
              "find_package(GLEW)\ntarget_link_libraries(native GLEW::GLEW)")
             (("Ext::Snappy") "snappy")
             ;; Don't search for cityhash/xxhash, we already have them.
             (("add_library\\((city|xx)hash STATIC") "if()\nendif(")
             (("ext/xxhash\\.[ch]") "")
             (("ext/cityhash/.*\\.(cpp|h)") "")
             (("if\\(USE_MINIUPNPC\\)" all)
              (string-append all "
find_package(miniupnpc)
target_link_libraries(${CoreLibName} miniupnpc ${LDLIBS})
elseif(FALSE)"))
             ;; Link all of spirv-cross.
             (("spirv-cross-glsl" all)
              (string-append all
                             " spirv-cross-core spirv-cross-cpp"
                             " spirv-cross-reflect spirv-cross-util")))
           (substitute* "ext/CMakeLists.txt"
             (("add_subdirectory\\(glew.*") "")
             (("add_subdirectory\\(glslang.*") "")
             (("add_subdirectory\\(snappy.*") "")
             (("add_subdirectory\\(SPIRV-Cross-build.*") "")
             (("add_subdirectory\\(zstd.*") ""))
           ;; Finally, we can delete the bundled sources.
           (for-each delete-file-recursively
                     '("ext/cmake"
                       "ext/glew"
                       "ext/glslang" "ext/glslang-build"
                       "ext/miniupnp" "ext/miniupnp-build"
                       "ext/native"
                       "ext/snappy"
                       "ext/SPIRV-Cross" "ext/SPIRV-Cross-build"
                       "ext/vulkan"
                       "ext/xxhash.c"
                       "ext/xxhash.h"
                       "ext/zlib"
                       "ext/zstd"))
           ;; Since we are not including git as an input, PPSSPP is confused
           ;; about its version.  Let's fix that here.
           (substitute* "git-version.cmake"
             (("unknown") ,version))))))
    (build-system cmake-build-system)
    (native-inputs (list pkg-config python))
    (inputs (list bash
                  cityhash
                  ffmpeg-4
                  glew
                  glslang
                  libpng
                  libzip
                  mesa
                  miniupnpc
                  sdl2
                  snappy
                  spirv-cross
                  vulkan-headers
                  vulkan-loader
                  xxhash
                  zlib
                  `(,zstd "lib")))
    (arguments
     (list
      #:out-of-source? #f
      #:configure-flags #~(list "-DARMIPS_USE_STD_FILESYSTEM=ON" ; from armips
                                "-DUSE_DISCORD=OFF"
                                "-DUSE_SYSTEM_FFMPEG=ON"
                                "-DUSE_SYSTEM_LIBZIP=ON"
                                "-DUSE_SYSTEM_ZSTD=ON"
                                ;; for testing
                                "-DUNITTEST=ON" "-DHEADLESS=ON")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'add-external-sources
            (lambda* (#:key inputs #:allow-other-keys)
              ;; TODO: unbundle armips.
              (copy-recursively #$(package-source armips) "ext/armips")))
          (replace 'install
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (bin/ppsspp (string-append out "/bin/ppsspp"))
                     (share (string-append out "/share/ppsspp")))
                (copy-recursively "icons/hicolor"
                                  (string-append out "/share/icons/hicolor"))
                (install-file "PPSSPPSDL" share)
                (copy-recursively "assets" (string-append share "/assets"))

                (make-desktop-entry-file
                 (string-append out "/share/applications/ppsspp.desktop")
                 #:name "PPSSPP"
                 #:exec (string-append share "/PPSSPPSDL")
                 #:icon "ppsspp")
                (mkdir-p (string-append out "/bin"))
                (with-output-to-file bin/ppsspp
                  (lambda ()
                    (format #t "#!~a~%exec ~a/PPSSPPSDL \"$@\""
                            (search-input-file inputs "/bin/bash") share)))
                (chmod bin/ppsspp #o755)))))))
    (home-page "https://www.ppsspp.org/")
    (synopsis "PSP emulator")
    (description
     "PPSSPP is a ``high-level'' emulator simulating the PSP operating
system.")
    (license license:gpl2+)))

(define-public exomizer
  (package
    (name "exomizer")
    (version "3.1.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://bitbucket.org/magli143/exomizer.git")
                     (commit version)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "04795l75nlbz0g5gp1xx8kiwbrm5pv5pj24ja02cnan6mglj7j0w"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f  ; No target exists
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _
             (delete-file-recursively "exodecrs")
             (delete-file-recursively "rawdecrs")
             (chdir "src")
             ;; Those will be regenerated.
             (delete-file "asm.tab.h")
             (delete-file "asm.tab.c")
             (delete-file "lex.yy.c")
             #t))
         (replace 'configure
           (lambda _
             (setenv "CC" ,(cc-for-target))
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out-bin (string-append (assoc-ref outputs "out") "/bin")))
               (install-file "exomizer" out-bin)
               (install-file "exobasic" out-bin))
             #t)))))
    (native-inputs
     (list flex bison))
    (synopsis "Compressor for use on Commodore home computers")
    (description "This program compresses files in a way that tries to be as
efficient as possible but still allows them to be decompressed in environments
where CPU speed and RAM are limited.  It also generate a self-extractor for use
on a Commodore C64, C128 etc.")
    (home-page "https://bitbucket.org/magli143/exomizer/wiki/Home")
    ;; Some files are LGPL 2.1--but we aren't building from or installing those.
    ;; zlib license with an (non-)advertising clause.
    (license license:zlib)))

(define-public flycast
  (package
    (name "flycast")
    (version "2.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/flyinghead/flycast")
             (commit (string-append "v" version))
             ;; There are many bundled packages here included as git
             ;; submodules. Removing many of them would require patching the
             ;; source code and repository layout.
             (recursive? #t)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ainy75gkrvilcm89hq6wq9md41w0mxgp6l27q5fzrxxykpjh6ym"))
       (modules '((guix build utils)))
       (snippet #~(begin
                    (substitute* "CMakeLists.txt"
                      (("add_subdirectory\\(core/deps/Vulkan-Headers\\)")
                       "find_package(VulkanHeaders)"))
                    (with-directory-excursion "core/deps"
                      (for-each delete-file-recursively
                                '("SDL"
                                  "Spout"
                                  "Syphon"
                                  "Vulkan-Headers"
                                  "breakpad"
                                  "discord-rpc"
                                  "libzip"
                                  "oboe")))))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f ; no test suite
      #:configure-flags
      #~(list "-DUSE_ALSA=ON"
              "-DUSE_BREAKPAD=OFF"
              "-DUSE_DX11=OFF"
              "-DUSE_DX9=OFF"
              ;; The USE_HOST_GLSLANG option is not implemented correctly.
              ;; (see: https://github.com/flyinghead/flycast/issues/1843)
              "-DUSE_HOST_GLSLANG=OFF"
              "-DUSE_HOST_LIBZIP=ON"
              "-DUSE_HOST_SDL=ON"
              "-DUSE_LIBAO=ON"
              "-DUSE_LUA=ON"
              "-DUSE_PULSEAUDIO=ON"
              "-DUSE_VULKAN=ON")))
    (inputs (list alsa-lib
                  ao
                  curl
                  glslang
                  libzip
                  lua
                  miniupnpc
                  pulseaudio
                  sdl2
                  spirv-tools
                  vulkan-headers
                  pkg-config))
    (home-page "https://github.com/flyinghead/flycast")
    (synopsis "Sega Dreamcast, Naomi, Naomi 2, and Atomiswave emulator")
    (description "Flycast is a multi-platform Sega Dreamcast, Naomi, Naomi 2,
and Atomiswave emulator derived from reicast.")
    (license license:gpl2+)))

(define-public freedisksysrom
  ;; There is no release; use the latest commit.
  (let ((commit "0d5f95f109bb3aadf2bb9510bfda13879bbd5266")
        (revision "0"))
    (package
      (name "freedisksysrom")
      (version (git-version "0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/jamesathey/FreeDiskSysROM")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0bmzmh3aq76jr31wn5lxvqvy9lpildxzqrvvqg1xxk5pvfjl8bip"))))
      (build-system gnu-build-system)
      (arguments
       (list
        #:tests? #f                     ;no test suite
        #:phases
        #~(modify-phases %standard-phases
            (delete 'configure)         ;no configure script
            (replace 'build
              (lambda _
                (invoke "asm6f" "freedisksys.asm")))
            (replace 'install
              (lambda _
                (let ((libexec (string-append #$output "/libexec")))
                  (install-file "freedisksys.bin" libexec)
                  (with-directory-excursion libexec
                    (symlink "freedisksys.bin" "disksys.rom"))))))))
      (native-inputs (list asm6f))
      (home-page "https://github.com/jamesathey/FreeDiskSysROM")
      (synopsis "Implementation of the Famicom Disk System BIOS")
      (description "FreeDiskSysROM aims to provide a replacement for the
original @acronym{FDS, Famicom Disk System} BIOS (often referred to as
@file{disksys.rom}) that can be freely redistributed and that is capable of
running all published FDS software.  FreeDiskSysROM is not currently fully
completed and may not be sufficient for some FDS software.  To track its
status, consult
@url{https://github.com/jamesathey/FreeDiskSysROM?tab=readme-ov-file#apis}.")
      (license license:lgpl3+))))

(define-public qtrvsim
  (package
    (name "qtrvsim")
    (version "0.9.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/cvut/qtrvsim")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1zi39q8ajkzl8d47sacj0dk1a2n5jmfgr29x9iby59v792g7p8ac"))
              (modules '((guix build utils)))
              (snippet #~(begin (delete-file-recursively "external/libelf")))))
    (build-system qt-build-system)
    (inputs (list libelf qtbase-5))
    (home-page "https://github.com/cvut/qtrvsim")
    (synopsis "RISC-V CPU simulator for education purposes")
    (description "RISC-V CPU simulator for education purposes with pipeline and
cache visualization.  Developed at FEE CTU for computer architecture classes.")
    (license license:gpl3+)))

(define-public cc65
  (package
    (name "cc65")
    (version "2.19")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/cc65/cc65.git")
                     (commit (string-append "V" version))))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "01a15yvs455qp20hri2pbg2wqvcip0d50kb7dibi9427hqk9cnj4"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f  ; No target exists.
       #:make-flags
       (list "BUILD_ID=V2.18 - Git 55528249"
             (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key source #:allow-other-keys)
             ;; We include $SOURCE/include in C_INCLUDE_PATH.  Remove it.
             (setenv "C_INCLUDE_PATH"
               (string-join
                (filter (lambda (name)
                          (not (string=? name (string-append source "/include"))))
                        (string-split (getenv "C_INCLUDE_PATH") #\:))
                ":"))
             #t)))))
    (synopsis "Development environment for 6502 systems")
    (description "This package provides a development environment for 6502 systems, including macro assembler, C compiler, linker, librarian and several other tools.")
    (home-page "https://cc65.github.io/")
    (license license:zlib)))

(define-public uxn
  (let ((commit "83237c9641490d303a42c81ca247314d11055dea")
        (revision "1"))
    (package
      (name "uxn")
      (version (git-version "0.1.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://git.sr.ht/~rabbits/uxn")
                      (commit commit)))
                (file-name (string-append name "-" version))
                (sha256
                 (base32
                  "159qfz66k1jc43jhyl8by3yiphsr2dyiyclw1x7mkr3zciwc29z3"))))
      (build-system gnu-build-system)
      (arguments
       (list #:tests? #f ;no tests
             #:phases #~(modify-phases %standard-phases
                          (delete 'configure)
                          (replace 'build
                            (lambda _
                              (setenv "CC" #$(cc-for-target))
                              (invoke "./build.sh" "--no-run")))
                          (replace 'install
                            (lambda _
                              (let ((bin (string-append #$output "/bin"))
                                    (share (string-append #$output
                                                          "/share/uxn")))
                                (with-directory-excursion "bin"
                                  (for-each (lambda (x)
                                              (install-file x bin))
                                            '("uxnasm" "uxncli" "uxnemu"))
                                  (for-each (lambda (x)
                                              (install-file x share))
                                            '("asma.rom" "launcher.rom")))))))))
      (inputs (list sdl2))
      (home-page "https://100r.co/site/uxn.html")
      (synopsis "Assembler and emulator for the Uxn stack-machine")
      (description
       "This package provides an assembler and emulator for the Uxn
stack-machine, written in ANSI C.  Graphical output is implemented using SDL2.")
      (license license:expat))))

(define-public python-keystone-engine
  (package
    (name "python-keystone-engine")
    (version "0.9.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "keystone-engine" version))
       (sha256
        (base32 "1xahdr6bh3dw5swrc2r8kqa8ljhqlb7k2kxv5mrw5rhcmcnzcyig"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list cmake
           python-setuptools
           python-wheel))
    (home-page "https://www.keystone-engine.org")
    (synopsis "Lightweight multi-platform, multi-architecture assembler framework")
    (description
     "Keystone is a lightweight multi-platform, multi-architecture
assembler framework.  It supports a wide-range of different architectures
and offers an intuitive architecture-neutral API for interacting with
assembly for these architectures.")
    (license license:gpl2)))

;; can be removed once Guix upgrades to Python 3.11.
(define-public python-backports-strenum
  (package
    (name "python-backports-strenum")
    (version "1.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "backports_strenum" version))
       (sha256
        (base32 "0514yj1391k6pbs2cch6i57hidwb3236wngh2ivlk6186h3j9ibp"))))
    (native-inputs (list python-poetry-core))
    (build-system pyproject-build-system)
    ;; TODO: Running tests requires a new version of poetry in Guix.
    (arguments
     (list
      #:tests? #f))
    (home-page "https://github.com/clbarnes/backports.strenum")
    (synopsis "Backport of additions to the 'strenum' module")
    (description
     "Provides a backport of Python's @code{StrEnum} class which was introduced in
Python 3.11 for Python >=3.8.6.")
    (license license:expat)))

(define-public python-archinfo
  (package
    (name "python-archinfo")
    ;; Must be the same version as python-angr.
    (version "9.2.112")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "archinfo" version))
       (sha256
        (base32 "011n9vrrsbqbnw2i38ls7f0xkd85kxcnn14fm4lhxjpi91p7hshb"))))
    (build-system pyproject-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-after 'unpack 'patch
                    (lambda _
                      (substitute* "setup.cfg"
                        (("backports.strenum")
                         "backports_strenum"))))
                  (replace 'check
                    (lambda* (#:key tests? #:allow-other-keys)
                      (when tests?
                        (with-directory-excursion "tests"
                          (invoke "python" "-m" "unittest"))))))))
    (propagated-inputs
     (list python-backports-strenum
           python-capstone
           python-keystone-engine))
    (native-inputs
     (list python-setuptools python-wheel))
    (home-page "https://github.com/angr/archinfo")
    (synopsis "Extract architecture-specific information from binaries")
    (description
     "Collection of classes that contain architecture-specific information
information.  Useful for cross-architecture tools (such as @code{python-pyvex}).")
    (license license:bsd-2)))

(define-public emu8051
  (let ((commit "5dc681275151c4a5d7b85ec9ff4ceb1b25abd5a8")
        (revision "1"))
    (package
      (name "emu8051")
      (version (git-version "0.1" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/jarikomppa/emu8051")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1xxmkcwvd5fjnhwbricafg4xvxvr8dxhfanyfp4rbksw37dgk2fx"))
                (modules '((guix build utils)))
                (snippet #~(begin
                             ;; Replace LDFLAGS -lcurses to -lncurses
                             (substitute* "Makefile"
                               (("-lcurses") "-lncurses"))))))
      (build-system gnu-build-system)
      (arguments
       (list #:tests? #f ;No test suite
             #:make-flags #~(list (string-append "CC="
                                                 #$(cc-for-target)))
             #:phases
             #~(modify-phases %standard-phases
                 (delete 'configure)    ;No ./configure script
                 (replace 'install
                   ;; No installation procedure
                   (lambda _
                     (install-file "emu"
                                   (string-append #$output "/bin")))))))
      (inputs (list ncurses))
      (home-page "https://github.com/jarikomppa/emu8051")
      (synopsis "8051/8052 emulator with curses-based UI")
      (description "emu8051 is a simulator of the 8051/8052 microcontrollers.")
      (license license:expat))))
