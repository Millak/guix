;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2018, 2019, 2020, 2022 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2019, 2020 Christopher Howard <christopher@librehacker.com>
;;; Copyright © 2019, 2020 Evan Straw <evan.straw99@gmail.com>
;;; Copyright © 2020-2025 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2020 Danny Milosavljevic <dannym@scratchpost.org>
;;; Copyright © 2020 Charlie Ritter <chewzerita@posteo.net>
;;; Copyright © 2020–2022 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2021 João Pedro Simas <jpsimas@gmail.com>
;;; Copyright © 2021 Jack Hill <jackhill@jackhill.us>
;;; Copyright © 2022 Jai Vetrivelan <jaivetrivelan@gmail.com>
;;; Copyright © 2022 Sheng Yang <styang@fastmail.com>
;;; Copyright © 2022 Greg Hogan <code@greghogan.com>
;;; Copyright © 2022 Ryan Tolboom <ryan@using.tech>
;;; Copyright © 2023 Sharlatan Hellseher <sharlatanus@gmail.com>
;;; Copyright © 2023, 2025 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2024 Andy Tai <atai@atai.org>
;;; Copyright © 2024 Noisytoot <ron@noisytoot.org>
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

(define-module (gnu packages radio)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages astronomy)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages avahi)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages engineering)
  #:use-module (gnu packages fltk)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gd)
  #:use-module (gnu packages geo)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages gps)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages image-processing)
  #:use-module (gnu packages javascript)
  #:use-module (gnu packages jemalloc)
  #:use-module (gnu packages libedit)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages logging)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages man)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages openstack)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages popt)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages pretty-print)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages toolkits)
  #:use-module (gnu packages video)
  #:use-module (gnu packages web)
  #:use-module (gnu packages wxwidgets)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (guix build-system qt))

(define-public libfec
  ;; Use commit to get compilation fixes that are not in a release yet.
  (let ((commit "9750ca0a6d0a786b506e44692776b541f90daa91")
        (revision "1"))
    (package
      (name "libfec")
      (version (git-version "1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/quiet/libfec")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0i6jhrdswr1wglyb9h39idpz5v9z13yhidvlbj34vxpyngrkhlvs"))))
      (build-system cmake-build-system)
      (arguments
       `(#:configure-flags '("-DBUILD_SHARED_LIBS=ON")
         #:test-target "test_all"))
      (home-page "https://github.com/quiet/libfec")
      (synopsis "Forward error correction algorithms library")
      (description
       "This package provides a set of functions that implement several popular
@dfn{forward error correction} (FEC) algorithms and several low-level routines
useful in modems implemented with @dfn{digital signal processing} (DSP).")
      (license license:lgpl2.1))))

(define-public libcorrect
  (let ((commit "f5a28c74fba7a99736fe49d3a5243eca29517ae9")
        (revision "1"))
    (package
      (name "libcorrect")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/quiet/libcorrect")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0qc9k8x51k2xfvp6cx8vdiyb3g6fl1y657z4m201aw2m06hs1hzg"))))
      (build-system cmake-build-system)
      (arguments
       (list
        #:test-target "check"
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'build 'build-libfec-compatibility-layer
              (lambda _
                (invoke "make" "shim")))
            (add-after 'install 'delete-static-libraries
              (lambda _
                (delete-file (string-append #$output "/lib/libcorrect.a"))
                (delete-file (string-append #$output "/lib/libfec.a")))))))
      (home-page "https://github.com/quiet/libcorrect")
      (synopsis "Forward error correction library")
      (description
       "This library provides convolutional and Reed-Solomon codes for forward
error correction.  It also includes a compatibility layer so that it can be
used as a drop-in substitute for @code{libfec}.")
      (license license:bsd-3))))

(define-public liquid-dsp
  (package
    (name "liquid-dsp")
    (version "1.6.0")
    (source
     (origin (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/jgaeddert/liquid-dsp")
                   (commit (string-append "v" version))))
             (file-name (git-file-name name version))
             (sha256
              (base32 "1zw3h2d7kiyxz5zcg5wy4d6pkb07q1pqkc6zz4v9wq8s2v180hnx"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake))
    (inputs
     (list fftwf libfec))
    (arguments
     (list
      ;; For reproducibility, disable use of SSE3, SSE4.1, etc.
      #:configure-flags #~(list "--enable-simdoverride")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'delete-static-library
            (lambda _
              (delete-file (string-append #$output "/lib/libliquid.a")))))))
    (home-page "https://liquidsdr.org")
    (synopsis "Signal processing library for software-defined radios")
    (description
     "Liquid DSP is a @dfn{digital signal processing} (DSP) library designed
specifically for software-defined radios on embedded platforms.  The aim is to
provide a lightweight DSP library that does not rely on a myriad of external
dependencies or proprietary and otherwise cumbersome frameworks.  All signal
processing elements are designed to be flexible, scalable, and dynamic,
including filters, filter design, oscillators, modems, synchronizers, complex
mathematical operations, and much more.")
    (license license:expat)))

(define-public rtl-sdr
  (package
    (name "rtl-sdr")
    (version "2.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.osmocom.org/rtl-sdr/")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0z8dn0gdava894fb9fs9gcwvmik31fcj6ldkggylc0mhgw5145pr"))))
    (build-system cmake-build-system)
    (inputs
     (list libusb))
    (native-inputs
     (list pkg-config))
    (arguments
     `(#:configure-flags '("-DDETACH_KERNEL_DRIVER=ON"
                           "-DINSTALL_UDEV_RULES=ON")
       #:tests? #f ; No tests
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-paths
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "CMakeLists.txt"
               (("DESTINATION \"/etc/udev/")
                (string-append "DESTINATION \""
                               (assoc-ref outputs "out")
                               "/lib/udev/")))))
         (add-after 'fix-paths 'fix-udev-rules
           (lambda _
             (substitute* "rtl-sdr.rules"
               ;; The plugdev group does not exist; use dialout as in
               ;; the hackrf package.
               (("GROUP=\"plugdev\"")
                "GROUP=\"dialout\"")))))))
    (home-page "https://osmocom.org/projects/sdr/wiki/rtl-sdr")
    (synopsis "Software defined radio driver for Realtek RTL2832U")
    (description "DVB-T dongles based on the Realtek RTL2832U can be used as a
cheap software defined radio, since the chip allows transferring the raw I/Q
samples to the host.  @code{rtl-sdr} provides drivers for this purpose.

The default Linux driver managing DVB-T dongles as TV devices doesn't work for
SDR purposes and clashes with this package.  Therefore you must prevent the
kernel from loading it automatically by adding the following line to your
system configuration:

@lisp
(kernel-arguments '(\"modprobe.blacklist=dvb_usb_rtl28xxu\"))
@end lisp

To install the rtl-sdr udev rules, you must extend 'udev-service-type' with
this package.  E.g.: @code{(udev-rules-service 'rtl-sdr rtl-sdr)}")
    (license license:gpl2+)))

(define-public airspy
  (let ((commit "6f92f47146aa8a8fce59b60927cf8c53da6851b3")
        (revision "1"))
    (package
      (name "airspy")
      (version (git-version "1.0.10" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/airspy/airspyone_host")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0y2yz8agp4v36z1766hi92msgs35yvy32brfcscijxdkgswdgbkd"))))
      (build-system cmake-build-system)
      (native-inputs
       (list pkg-config))
      (inputs
       (list libusb))
      (arguments
       (list #:configure-flags #~(list "-DINSTALL_UDEV_RULES=ON")
             #:tests? #f ; No tests
             #:phases
             #~(modify-phases %standard-phases
                 (add-after 'unpack 'fix-paths
                   (lambda _
                     (substitute* "airspy-tools/CMakeLists.txt"
                       (("DESTINATION \"/etc/udev/")
                        (string-append "DESTINATION \""
                                       #$output
                                       "/lib/udev/")))))
                 (add-after 'fix-paths 'fix-udev-rules
                   (lambda _
                     (substitute* "airspy-tools/52-airspy.rules"
                       ;; The plugdev group does not exist; use dialout as in
                       ;; the hackrf package.
                       (("GROUP=\"plugdev\"")
                        "GROUP=\"dialout\"")))))))
      (home-page "https://github.com/airspy/airspyone_host")
      (synopsis "Software defined radio driver for Airspy")
      (description
       "This package provides the driver and utilities for controlling the
Airspy Software Defined Radio (SDR) over USB.

To install the airspy udev rules, you must extend @code{udev-service-type}
with this package.  E.g.: @code{(udev-rules-service 'airspy airspy)}")
      (license (list license:bsd-3
                     license:expat
                     license:gpl2+)))))

(define-public airspyhf
  (let ((commit "40836c59d35d989fe00ac12ef774df736a36c6e4")
        (revision "1"))
    (package
      (name "airspyhf")
      (version (git-version "1.6.8" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/airspy/airspyhf")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1s3fm856smvja3cg6fy615igir8wb0dzbp0q25v3vls0qj6pvprb"))))
      (build-system cmake-build-system)
      (native-inputs
       (list pkg-config))
      (inputs
       (list libusb))
      (arguments
       '(#:configure-flags '("-DINSTALL_UDEV_RULES=ON")
         #:tests? #f ; No tests
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-paths
             (lambda* (#:key outputs #:allow-other-keys)
               (substitute* "tools/CMakeLists.txt"
                 (("DESTINATION \"/etc/udev/")
                  (string-append "DESTINATION \""
                                 (assoc-ref outputs "out")
                                 "/lib/udev/")))))
           (add-after 'fix-paths 'fix-udev-rules
             (lambda _
               (substitute* "tools/52-airspyhf.rules"
                 ;; The plugdev group does not exist; use dialout as in
                 ;; the hackrf package.
                 (("GROUP=\"plugdev\"")
                  "GROUP=\"dialout\"")))))))
      (home-page "https://github.com/airspy/airspyhf")
      (synopsis "Software defined radio driver for Airspy HF+")
      (description
       "This package provides the driver and utilities for controlling the
Airspy HF+ Software Defined Radio (SDR) over USB.

To install the airspyhf udev rules, you must extend @code{udev-service-type}
with this package.  E.g.: @code{(udev-rules-service 'airspyhf airspyhf)}")
      (license license:bsd-3))))

(define-public soapysdr
  (package
    (name "soapysdr")
    (version "0.8.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pothosware/SoapySDR")
             (commit (string-append "soapy-sdr-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "19f2x0pkxvf9figa0pl6xqlcz8fblvqb19mcnj632p0l8vk6qdv2"))))
    (build-system cmake-build-system)
    (native-inputs
     (list python swig))
    (native-search-paths
     (list (search-path-specification
            (variable "SOAPY_SDR_PLUGIN_PATH")
            (files (list (string-append "lib/SoapySDR/modules"
                                        (version-major+minor version)))))))
    (home-page "https://github.com/pothosware/SoapySDR/wiki")
    (synopsis "Vendor and platform neutral SDR support library")
    (description
     "SoapySDR is a library designed to support several kinds of software
defined radio hardware devices with a common API.")
    (license license:boost1.0)))

(define-public soapyairspy
  (package
    (name "soapyairspy")
    (version "0.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pothosware/SoapyAirspy")
             (commit (string-append "soapy-airspy-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0g23yybnmq0pg2m8m7dbhif8lw0hdsmnnjym93fdyxfk5iln7fsc"))))
    (build-system cmake-build-system)
    (inputs
     (list airspy soapysdr))
    (arguments
     (list #:tests? #f))  ; No test suite
    (home-page "https://github.com/pothosware/SoapyAirspy/wiki")
    (synopsis "SoapySDR Airspy module")
    (description "This package provides Airspy devices support to the
SoapySDR library.")
    (license license:expat)))

(define-public soapyairspyhf
  (package
    (name "soapyairspyhf")
    (version "0.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pothosware/SoapyAirspyHF")
             (commit (string-append "soapy-airspyhf-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "04krqinglgkjvx7klqik6yn8rb4mlpwzb6zvnmvm7szqci2agggz"))))
    (build-system cmake-build-system)
    (inputs
     (list airspyhf soapysdr))
    (arguments
     `(#:tests? #f))  ; No test suite
    (home-page "https://github.com/pothosware/SoapyAirspyHF/wiki")
    (synopsis "SoapySDR Airspy HF+ module")
    (description "This package provides Airspy HF+ devices support to the
SoapySDR library.")
    (license license:expat)))

(define-public soapyaudio
  ;; Use commit directly because fixes for recent hamlib are not in the latest
  ;; release (0.1.1).
  (let ((commit "79129c9bb98deca3294c05108fdc545579af6418")
        (revision "0"))
    (package
      (name "soapyaudio")
      (version (git-version "0.1.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/pothosware/SoapyAudio")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0mrcnd3k0j599x3k93dkpi5zgr0l7nblz8am9f0s6zs3dikfncvb"))
         (modules '((guix build utils)))
         (snippet
          '(begin
             ;; Delete bundled rtaudio.
             (delete-file-recursively "RtAudio")))))
      (build-system cmake-build-system)
      (native-inputs
       (list pkg-config))
      (inputs
       (list alsa-lib
             hamlib
             jack-1
             libusb
             pulseaudio
             rtaudio
             soapysdr))
      (arguments
       `(#:configure-flags '("-DUSE_HAMLIB=ON")
         #:tests? #f  ; No test suite
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-rtaudio-detection
             ;; CMake only finds rtaudio if it looks for it before looking
             ;; for hamlib, not sure why...
             (lambda _
               (substitute* "CMakeLists.txt"
                 (("option\\(USE_HAMLIB OFF" all)
                  (string-append "find_package(RtAudio)\n" all))))))))
      (home-page "https://github.com/pothosware/SoapyAudio/wiki")
      (synopsis "SoapySDR module for audio devices")
      (description
       "This package provides support for sound card devices to the SoapySDR
library.  It also adds hamlib support, which provides basic gain and frequency
controls for certain tuners which may be paired with an audio device.")
      (license license:expat))))

(define-public soapybladerf
  (let ((commit "85f6dc554ed4c618304d99395b19c4e1523675b0")
        (revision "1"))
    (package
      (name "soapybladerf")
      (version (git-version "0.4.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/pothosware/SoapyBladeRF")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "05c5mv1b55jv7dcr740hv4b3gplfaqryflfvprhlkm7bycr8pp16"))))
      (build-system cmake-build-system)
      (inputs (list bladerf soapysdr))
      (arguments (list #:tests? #f))  ; No test suite
      (home-page "https://github.com/pothosware/SoapyBladeRF/wiki")
      (synopsis "SoapySDR BladeRF module")
      (description "This package provides BladeRF devices support to the
SoapySDR library.")
      (license license:lgpl2.1+))))

(define-public soapyhackrf
  ;; Some fixes are not yet in a tagged release.
  (let ((commit "6c0c33f0aa44c3080674e6bca0273184d3e9eb44")
        (revision "1"))
    (package
      (name "soapyhackrf")
      (version (git-version "0.3.4" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/pothosware/SoapyHackRF")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1khplrp6iphdclj1wrybxs7pw42rf9112fhfhw7x2fvds8yiswm7"))))
      (build-system cmake-build-system)
      (inputs
       (list hackrf soapysdr))
      (arguments
       `(#:tests? #f))  ; No test suite
      (home-page "https://github.com/pothosware/SoapyHackRF/wiki")
      (synopsis "SoapySDR HackRF module")
      (description
       "This package provides HackRF devices support to the SoapySDR library.")
      (license license:expat))))

(define-public soapymultisdr
  (let ((commit "e8bd3298afaec04cb7ce2c8c516cb9cd8bd3bc9d")
        (revision "1"))
    (package
      (name "soapymultisdr")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/pothosware/SoapyMultiSDR")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0f7d39s2zpgfi677i2aqp4zkf5c6cv8mpm7w8s7xj45bfhf94acl"))))
      (build-system cmake-build-system)
      (inputs
       (list soapysdr))
      (home-page "https://github.com/pothosware/SoapyMultiSDR")
      (synopsis "Multi-device support module for SoapySDR")
      (description
       "This is a SoapySDR module to use multiple supported devices under
a single device wrapper.")
      (license license:boost1.0))))

(define-public soapyremote
  (let ((commit "f375555e7380acfd2517acde598e2e553e08df88")
        (revision "1"))
    (package
      (name "soapyremote")
      (version (git-version "0.5.2" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/pothosware/SoapyRemote")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0whn87wck7agsk3af4lh7nyyjn0ncs3xdny4vsd94qbjikfl6x5z"))))
      (build-system cmake-build-system)
      (inputs
       (list avahi soapysdr))
      (arguments
       '(#:tests? #f)) ; No test suite
      (home-page "https://github.com/pothosware/SoapyRemote")
      (synopsis "Remote support for Soapy SDR")
      (description
       "This is a SoapySDR module to use a supported device transparently over
a local network link.")
      (license license:boost1.0))))

(define-public soapyrtlsdr
  (package
    (name "soapyrtlsdr")
    (version "0.3.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pothosware/SoapyRTLSDR")
             (commit (string-append "soapy-rtl-sdr-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0g1d69lpqw9c8hkir0h61nh40rgyz9p9x04xcfdih7iw26n5vai1"))))
    (build-system cmake-build-system)
    (inputs
     (list rtl-sdr soapysdr))
    (arguments
     `(#:tests? #f))  ; No test suite
    (home-page "https://github.com/pothosware/SoapyRTLSDR/wiki")
    (synopsis "SoapySDR RTL-SDR module")
    (description
     "This package provides RTL-SDR devices support to the SoapySDR library.")
    (license license:expat)))

(define-public python-simplesoapy
  (package
    (name "python-simplesoapy")
    (version "1.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "SimpleSoapy" version))
       (sha256
        (base32 "0bh02m5zj82mp7sxpvwr24ylmrbp3p4r9q7psqcfnxl628w3b4hl"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-numpy soapysdr))
    (home-page "https://github.com/xmikos/simplesoapy")
    (synopsis "Python wrapper for SoapySDR")
    (description
     "This package provide a simple pythonic wrapper for the SoapySDR
library.")
    (license license:expat)))

(define-public soapy-power
  (package
    (name "soapy-power")
    (version "1.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "soapy_power" version))
       (sha256
        (base32 "1rajmygcqvv5ph7yk65r4w581lfszrz0f48csvfmma1ami0lirdm"))))
    (build-system python-build-system)
    (inputs
     (list python-numpy
           python-scipy
           python-simplesoapy
           python-simplespectral))
    (home-page "https://github.com/xmikos/soapy_power")
    (synopsis "Obtain power spectrum from SDR devices")
    (description "The @code{soapy_power} obtains the power spectrum from SDR
devices that are supported by the SoapySDR library.")
    (license license:expat)))

(define-public qspectrumanalyzer
  (package
    (name "qspectrumanalyzer")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "QSpectrumAnalyzer" version))
       (sha256
        (base32 "1bhl8zp4z7v3595ailyivx9vb7y5si6kr22aylphb5pf60jxqhn0"))))
    (build-system python-build-system)
    (inputs
     (list bash-minimal
           python-pyqt
           python-pyqtgraph
           python-qt.py
           python-simplespectral
           python-simplesoapy
           soapy-power))
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-after 'install 'wrap-path
                 ;; Add the location of the default backend to PATH.
                 (lambda* (#:key inputs #:allow-other-keys)
                   (wrap-program (string-append #$output
                                                "/bin/qspectrumanalyzer")
                     `("PATH" ":" prefix
                       (,(string-append (assoc-ref inputs "soapy-power")
                                        "/bin")))))))))
    (home-page "https://github.com/xmikos/qspectrumanalyzer")
    (synopsis "Spectrum analyzer for multiple SDR platforms")
    (description
     "This package provides a spectrum analyzer for multiple SDR platforms.
It is a GUI for @code{soapy_power}, @code{hackrf_sweep}, @code{rtl_power},
@code{rx_power} and other backends.")
    (license license:gpl3)))

(define-public aptdec
  ;; No release since 2013, use commit directly.
  (let ((commit "4d4a0c9787a27d1eba26b9299c23ae9c66e56716")
        (revision "3"))
    (package
      (name "aptdec")
      (version (git-version "1.7" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Xerbo/aptdec")
               (commit commit)
               (recursive? #t)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "09dvvwk4bs9d4bf9z74ixvhzghwzmlbqbx1dn20hbhpm0bgxwk8m"))))
      (build-system cmake-build-system)
      (inputs
       (list libpng libsndfile))
      (arguments
       `(#:tests? #f))  ; no tests
      (home-page "https://github.com/Xerbo/aptdec")
      (synopsis "NOAA Automatic Picture Transmission (APT) decoder")
      (description "Aptdec decodes Automatic Picture Transmission (APT) images.
These are medium resolution images of the Earth transmitted by, among other
satellites, the POES NOAA weather satellite series.  These transmissions are
on a frequency of 137 MHz.  They can be received using an inexpensive antenna
and a dedicated receiver.")
      (license license:gpl2+))))

(define-public redsea
  (package
    (name "redsea")
    (version "0.20")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/windytan/redsea")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1bx4l87vz935cj5hapdh1dkjlmlfg73cgsjaf27314n7p4xkv50v"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; The configure.ac file does not explicitly link against libiconv
         ;; except on Mac OS, causing the build to fail. This phase comments
         ;; out the original AC_SUBST macro (located inside a conditional) and
         ;; adds an explicit use of it underneath, so that libiconv is always
         ;; linked against.
         (add-after 'unpack 'patch-libiconv
           (lambda _
             (substitute* "configure.ac"
               (("^ +AC_SUBST")
                "# AC_SUBST")
               (("esac")
                "esac\nAC_SUBST([ICONV], [\"-liconv\"])"))
             #t)))))
    (inputs
     (list libiconv libsndfile liquid-dsp))
    (native-inputs
     (list autoconf automake))
    (home-page "https://github.com/windytan/redsea")
    (synopsis "Lightweight RDS to JSON decoder")
    (description "redsea is a lightweight command-line @dfn{FM Radio Data
System} (FM-RDS) decoder.  Redsea can be used with any RTL-SDR USB radio stick
with the rtl_fm tool, or any other @dfn{software-defined radio} (SDR) via
csdr, for example.  It can also decode raw ASCII bitstream, the hex format
used by RDS Spy, and audio files containing @dfn{multiplex} signals (MPX).")
    (license license:expat)))

(define-public gnuradio
  (package
    (name "gnuradio")
    (version "3.10.11.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gnuradio/gnuradio")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1px44c9clafivjw37zy6h6d94xf70v7i5iyarrdgm6cr7x95grj0"))))
    (build-system cmake-build-system)
    (native-inputs
     (list doxygen
           ghostscript
           js-mathjax
           orc
           pkg-config
           pybind11
           python-cheetah
           python-mako
           python-pyzmq
           python-scipy
           python-sphinx
           (texlive-updmap.cfg (list texlive-newunicodechar))
           xorg-server-for-tests))
    (inputs
     (list alsa-lib
           bash-minimal
           boost
           cairo
           codec2
           cppzmq
           fftwf
           gmp
           gsl
           gsm
           gtk+
           jack-1
           libsndfile
           log4cpp
           pango
           portaudio
           python
           python-click
           python-click-plugins
           python-jsonschema
           python-lxml
           python-matplotlib
           python-numpy
           python-pycairo
           python-pygobject
           python-pyqt
           python-pyqtgraph
           python-pyyaml
           qtbase-5
           qwt
           sdl
           soapysdr
           spdlog
           volk
           zeromq))
    (arguments
     `(#:modules ((guix build cmake-build-system)
                  ((guix build glib-or-gtk-build-system) #:prefix glib-or-gtk:)
                  ((guix build python-build-system) #:prefix python:)
                  (guix build utils)
                  (ice-9 match))
       #:imported-modules (,@%cmake-build-system-modules
                           (guix build glib-or-gtk-build-system)
                           (guix build python-build-system))
       #:configure-flags
       (list "-DENABLE_GRC=ON"
             (string-append "-DMATHJAX2_ROOT="
                            (assoc-ref %build-inputs "js-mathjax")
                            "/share/javascript/mathjax"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'make-source-writable
           (lambda _
             ;; The test_add and test_newmod open(sources, "w") for some reason.
             (for-each make-file-writable
                       (find-files "." ".*"))))
         (add-after 'unpack 'fix-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((qwt (assoc-ref inputs "qwt")))
               (substitute* "cmake/Modules/FindQwt.cmake"
                 (("/usr/include")
                  (string-append qwt "/include"))
                 (("/usr/lib")
                  (string-append qwt "/lib"))
                 (("qwt6-\\$\\{QWT_QT_VERSION\\}")
                  "qwt")))
             (substitute* "cmake/Modules/GrPython.cmake"
               (("dist-packages")
                "site-packages"))
             (substitute* '("gr-vocoder/include/gnuradio/vocoder/codec2.h"
                            "gr-vocoder/include/gnuradio/vocoder/freedv_api.h")
               (("<codec2/")
                "<"))))
         (add-before 'check 'set-test-environment
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "HOME" "/tmp")
             (system "Xvfb :1 &")
             (setenv "DISPLAY" ":1")))
         (replace 'check
           (lambda* (#:key tests? parallel-tests? #:allow-other-keys)
             (invoke "ctest" "-j" (if parallel-tests?
                                      (number->string (parallel-job-count))
                                      "1")
                     "--output-on-failure"
                     ;;disable broken tests
                     "-E" (string-join
                           '(;; https://github.com/gnuradio/gnuradio/issues/3871
                             "qa_header_payload_demux"
                             ;; https://github.com/gnuradio/gnuradio/issues/4348
                             "qa_packet_headerparser_b"
                             ;; qa_rotator_cc sometimes fails, it looks like
                             ;; a floating point number precision issue.
                             "qa_rotator_cc")
                           "|"))))
         (add-after 'install 'wrap-python
           (assoc-ref python:%standard-phases 'wrap))
         (add-after 'wrap-python 'wrap-glib-or-gtk
           (assoc-ref glib-or-gtk:%standard-phases 'glib-or-gtk-wrap))
         (add-after 'wrap-glib-or-gtk 'wrap-with-GI_TYPELIB_PATH
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (paths (map (match-lambda
                                 ((output . directory)
                                  (let ((girepodir (string-append
                                                    directory
                                                    "/lib/girepository-1.0")))
                                    (if (file-exists? girepodir)
                                        girepodir
                                        #f))))
                               inputs)))
               (wrap-program (string-append out "/bin/gnuradio-companion")
                 `("GI_TYPELIB_PATH" ":" prefix ,(filter identity paths)))))))))
    (native-search-paths
     ;; Variables required to find third-party plugins at runtime.
     (list (search-path-specification
            (variable "GRC_BLOCKS_PATH")
            (files '("share/gnuradio/grc/blocks")))
           (search-path-specification
            (variable "GUIX_PYTHONPATH")
            (files (list (string-append "lib/python"
                                        (version-major+minor
                                         (package-version python))
                                        "/site-packages"))))))
    (synopsis "Toolkit for software-defined radios")
    (description
     "GNU Radio is a development toolkit that provides signal processing blocks
to implement software radios.  It can be used with external RF hardware to
create software-defined radios, or without hardware in a simulation-like
environment.")
    (home-page "https://www.gnuradio.org")
    (license license:gpl3+)))

(define-public gr-osmosdr
  (package
    (name "gr-osmosdr")
    (version "0.2.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.osmocom.org/gr-osmosdr")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1qpa908bb7iagvaa7h541k1x092mb6dfrmw5ayy4p51qks45nj3p"))))
    (build-system cmake-build-system)
    (native-inputs
     (list doxygen pkg-config pybind11 python-mako python-six))
    (inputs
     (list airspy
           airspyhf
           bladerf
           boost
           fftwf
           gmp
           gnuradio
           gr-iqbal
           hackrf
           libsndfile
           log4cpp
           python
           python-numpy
           python-pyqt
           rtl-sdr
           soapysdr
           spdlog
           volk))
    (arguments
     (list #:modules '((guix build cmake-build-system)
                       ((guix build python-build-system) #:prefix python:)
                       (guix build utils))
           #:imported-modules `(,@%cmake-build-system-modules
                                (guix build python-build-system))
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'fix-gnuradio-iqbalance-detection
                 (lambda _
                   (substitute* "CMakeLists.txt"
                     (("find_package\\(gnuradio-iqbalance PATHS \\$\\{Gnuradio_DIR\\}\\)")
                      (string-append "find_package(gnuradio-iqbalance PATHS "
                                     #$(this-package-input "gr-iqbal")
                                     "/lib/cmake/gnuradio)")))))
               (add-after 'install 'wrap-python
                 (assoc-ref python:%standard-phases 'wrap)))))
    (synopsis "GNU Radio block for interfacing with various radio hardware")
    (description "This is a block for GNU Radio allowing to use a common API
to access different radio hardware.")
    (home-page "https://osmocom.org/projects/gr-osmosdr/wiki/GrOsmoSDR")
    (license license:gpl3+)))

(define-public libosmo-dsp
  (package
    (name "libosmo-dsp")
    (version "0.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.osmocom.org/libosmo-dsp")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "00plihnpym1gkfpflah8il9463qxzm9kx2f07jyvbkszpj8viq5g"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("bash-minimal" ,bash-minimal)
       ("doxygen" ,doxygen)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)
       ("texlive" ,(texlive-updmap.cfg (list texlive-newunicodechar)))))
    (inputs
     (list fftwf))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "git-version-gen"
               (("/bin/sh")
                (search-input-file inputs "/bin/bash"))))))))
    (synopsis "DSP primitives for SDR")
    (description
     "This a C-language library for common DSP (Digital Signal Processing)
primitives for SDR (Software Defined Radio).")
    (home-page "https://osmocom.org/projects/libosmo-dsp")
    (license license:gpl2+)))

(define-public gr-dsd
  ;; The bundled DSD has been modified to bypass the soundcard.
  (let ((commit "f9b99360b9b15a568befec1b8cc262f7806898e9")
        (revision "0"))
    (package
      (name "gr-dsd")
      (version (git-version "1.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/argilo/gr-dsd")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1axxb8jdbjbf69csp17gpfis8id66ijjrqp2wbyvz1p66m0svldr"))))
      (build-system cmake-build-system)
      (native-inputs
       (list cppunit
             doxygen
             pkg-config
             pybind11
             python-numpy))
      (inputs
       (list boost
             gmp
             gnuradio
             itpp
             libsndfile
             log4cpp
             spdlog
             volk))
      (arguments
       (list ;; Tests fail with:
             ;;   from dsd import dsd_block_ff
             ;;   ModuleNotFoundError: No module named 'dsd'
             #:tests? #f
             #:phases
             #~(modify-phases %standard-phases
                 (add-after 'unpack 'fix-itpp-detection
                   (lambda _
                     (substitute* "dsd/cmake/FindITPP.cmake"
                       (("libitpp\\.dll")
                        "itpp_debug")))))))
      (synopsis "GNU Radio block for Digital Speech Decoder")
      (description
       "This package provides a GNU Radio block interfacing with Digital
Speech Decoder (DSD) to decode several digital voice protocols, like D-STAR,
DMR, NXDN, P25, etc.")
      (home-page "https://github.com/argilo/gr-dsd")
      (license (list license:bsd-3
                     license:gpl2
                     license:gpl3+
                     license:isc)))))

(define-public gr-iqbal
  ;; No tag for version supporting Gnuradio 3.9; use commit.
  (let ((commit "fbee239a6fb36dd2fb564f6e6a0d393c4bc844db")
        (revision "0"))
    (package
      (name "gr-iqbal")
      (version (git-version "0.38.2" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://git.osmocom.org/gr-iqbal")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "12p193ngcs65nd3lynry119nhv40mikamqkw37wdln7lawx3nw7p"))))
      (build-system cmake-build-system)
      (native-inputs
       (list doxygen
             pkg-config
             pybind11
             python
             python-numpy
             python-six))
      (inputs
       (list boost
             fftwf
             gmp
             gnuradio
             libosmo-dsp
             log4cpp
             spdlog
             volk))
      (synopsis "GNU Radio block to correct IQ imbalance")
      (description
     "This is a GNU Radio block to correct IQ imbalance in quadrature
receivers.  It's composed of two main block:
@itemize
@item Fix: Given a phase and amplitude error, it will correct a complex signal.
@item Optimize: Attempts to auto-detect the phase and amplitude error to feed
to the fix block above.
@end itemize")
      (home-page "https://git.osmocom.org/gr-iqbal/")
      (license license:gpl3+))))
(deprecated-package "gnuradio-iqbalance" gr-iqbal)

(define-public gr-satellites
  (package
    (name "gr-satellites")
    (version "4.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/daniestevez/gr-satellites")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0mcrxwb27n2v8v8vmcmmm1pbmy3c02a22mz2wnpdsfb2163qpchw"))))
    (build-system cmake-build-system)
    (native-inputs
     (list pkg-config pybind11 python-six))
    (inputs
     (list boost
           gmp
           gnuradio
           log4cpp
           python
           python-construct
           python-numpy
           python-pyaml
           python-pyzmq
           python-requests
           spdlog-1.13
           volk))
    (arguments
     `(#:modules ((guix build cmake-build-system)
                  ((guix build python-build-system) #:prefix python:)
                  (guix build utils))
       #:imported-modules (,@%cmake-build-system-modules
                           (guix build python-build-system))
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'set-test-environment
           (lambda _
             (setenv "HOME" "/tmp")))
         (add-after 'install 'wrap-python
           (assoc-ref python:%standard-phases 'wrap)))))
    (synopsis "GNU Radio decoders for several Amateur satellites")
    (description
     "@code{gr-satellites} is a GNU Radio out-of-tree module encompassing
a collection of telemetry decoders that supports many different Amateur
satellites.")
    (home-page "https://github.com/daniestevez/gr-satellites")
    (license (list license:asl2.0
                   license:gpl3+
                   license:lgpl2.1))))

(define-public gqrx
  (package
    (name "gqrx")
    (version "2.17.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gqrx-sdr/gqrx")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "17ddhxkh9rcfzahv88knfs895sjihj7j8ag1kwjfzdm80drhlagz"))))
    (build-system qt-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list alsa-lib
           boost
           fftwf
           gmp
           gnuradio
           gr-iqbal
           gr-osmosdr
           jack-1
           libsndfile
           log4cpp
           portaudio
           pulseaudio
           qtbase-5
           qtsvg-5
           spdlog
           volk))
    (arguments
     `(#:tests? #f))                    ; no tests
    (synopsis "Software defined radio receiver")
    (description "Gqrx is a software defined radio (SDR) receiver implemented
using GNU Radio and the Qt GUI toolkit.")
    (home-page "https://gqrx.dk/")
    (license license:gpl3+)))

(define-public gqrx-scanner
  (package
    (name "gqrx-scanner")
    (version "1.0.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/neural75/gqrx-scanner")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0swciyqx5fnqmbb6d55cnjrc2bysr9vamyp0lzsvwgh2g2r29i7w"))))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #f)) ; No test suite
    (synopsis "Frequency scanner for Gqrx")
    (description
     "This package provides a frequency scanner for the Gqrx software-defined
radio receiver.")
    (home-page "https://github.com/neural75/gqrx-scanner")
    (license license:expat)))

(define-public fldigi
  (package
    (name "fldigi")
    (version "4.2.06")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.code.sf.net/p/fldigi/fldigi")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0l70skbs92pma8ypils4h1wygm7pc61zfqjbnf5kn28vvah3n8zn"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf-2.71 automake gettext-minimal pkg-config))
    (inputs
     (list alsa-lib
           fltk
           eudev
           hamlib
           libpng
           libsamplerate
           libusb
           libx11
           libxext
           libxfixes
           libxft
           portaudio
           pulseaudio))
    (synopsis "Software modem for amateur radio use")
    (description
     "Fldigi is a software modem for amateur radio use.  It is a sound card
based program that is used for both transmitting and receiving data by
connecting the microphone and headphone connections of a computer to some radio
hardware.")
    (home-page "http://www.w1hkj.com/")
    (license license:gpl3+)))

(define-public flrig
  (package
    (name "flrig")
    (version "2.0.04")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.code.sf.net/p/fldigi/flrig")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0q8zwg5l9x62qls71sqqw31c79y9df7cmin91w1v5pbigxac4v9v"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake pkg-config))
    (inputs
     (list eudev fltk libx11 libxext libxfixes libxft))
    (synopsis "Radio transceiver control program")
    (description
     "Flrig is a transceiver control program for amateur radio use.
It provides computer aided control of various radios using a serial
or USB connection.")
    (home-page "http://www.w1hkj.com/")
    (license license:gpl3+)))

(define-public flamp
  (package
    (name "flamp")
    (version "2.2.12")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.code.sf.net/p/fldigi/flamp")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0739x3b8a2f74s91mzpb43r5h71h81v0i12qjz0h0gg07ndzj3j7"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake pkg-config))
    (inputs
     (list fltk libx11 libxext libxfixes libxft))
    (synopsis "Tool for AMP file transfer")
    (description
     "FLAMP is a program for transferring files by radio waves using AMP
(Amateur Multicast Protocol).")
    (home-page "http://www.w1hkj.com/")
    (license license:gpl3+)))

(define-public flmsg
  (package
    (name "flmsg")
    (version "4.0.23")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.code.sf.net/p/fldigi/flmsg")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0nn2ai97a2izckg6lcnxa36ipmrz0pa4d8gdvk47fhwxlcr7pdrl"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf-2.71 automake pkg-config))
    (inputs
     (list fltk libx11 libxext libxfixes libxft))
    (synopsis "NBEMS messaging system")
    (description
     "FLMSG is a Narrow Band Emergency Messaging Software (NBEMS).
It can be used to manage, send and receive the forms that are used as basis
for emergency communications data transfers (like ICS213 forms).")
    (home-page "http://www.w1hkj.com/")
    (license license:gpl3+)))

(define-public flwrap
  (package
    (name "flwrap")
    (version "1.3.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.code.sf.net/p/fldigi/flwrap")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0kd8jpgqyff3ck103i2nfvlxv2vqwn9ks02s8yddk7scww8raziy"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake pkg-config))
    (inputs
     (list fltk-1.3 libx11 libxext libxfixes libxft))
    (synopsis "File encapsulation program")
    (description
     "Flwrap is a software utility for amateur radio use.  Its purpose is to
encapsulate both text and binary files in a way that allows them to be
transmitted over any of several digital modes and verified at the receipt end
for correctness.")
    (home-page "http://www.w1hkj.com/")
    (license license:gpl3+)))

(define-public hackrf
  (package
    (name "hackrf")
    (version "2023.01.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/greatscottgadgets/hackrf")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ybgppwy09j9rmfhh84072li698k64w84q5hjrayc73avc495x6f"))))
    (build-system cmake-build-system)
    (arguments
     (list #:configure-flags
           #~(list "-DUDEV_RULES_GROUP=dialout"
                   (string-append "-DUDEV_RULES_PATH="
                                  #$output
                                  "/lib/udev/rules.d"))
           #:phases
           #~(modify-phases %standard-phases
               (add-before 'configure 'enter-source-directory
                 (lambda _
                   (chdir "host")))
               (add-after 'install 'delete-static-library
                 (lambda _
                   (delete-file (string-append #$output "/lib/libhackrf.a"))))
               (add-before 'install-license-files 'leave-source-directory
                 (lambda _
                   (chdir ".."))))
           #:tests? #f)) ; no test suite
    (native-inputs
     (list pkg-config))
    (inputs
     (list fftw fftwf libusb))
    (home-page "https://greatscottgadgets.com/hackrf/")
    (synopsis "User-space library and utilities for HackRF SDR")
    (description
     "Command line utilities and a C library for controlling the HackRF
Software Defined Radio (SDR) over USB.  Installing this package installs the
userspace hackrf utilities and C library.  To install the hackrf udev rules,
you must extend 'udev-service-type' with this package.  E.g.:
@code{(udev-rules-service 'hackrf hackrf #:groups '(\"dialout\"))}.")
    (license license:gpl2)))

(define-public bladerf
  (package
    (name "bladerf")
    (version "2023.02")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Nuand/bladeRF")
             (commit version)
             (recursive? #t)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "038v9qdmrwx9mxsrq4l36bap0bsypyg4i8hs7l7srv4b0c2s7ynp"))))
    (build-system cmake-build-system)
    (native-inputs (list doxygen help2man pkg-config))
    (inputs (list libedit libusb))
    (arguments
     (list #:configure-flags #~(list "-DTAGGED_RELEASE=ON"
                                     (string-append "-DUDEV_RULES_PATH="
                                                    #$output
                                                    "/lib/udev/rules.d")
                                     "-DBLADERF_GROUP=dialout"
                                     "-DBUILD_DOCUMENTATION=ON")
           #:tests? #f)) ; No test suite
    (home-page "https://www.nuand.com/")
    (synopsis "User-space library and utilities for BladeRF SDR")
    (description
     "This package contains a library and command line utilities for
controlling the BladeRF Software Defined Radio (SDR) over USB.  To install the
bladerf udev rules, you must extend 'udev-service-type' with this package.
E.g.: @code{(udev-rules-service 'bladerf bladerf)}.")
    (license (list license:bsd-3
                   license:expat
                   license:gpl2+
                   license:lgpl2.1+))))

(define-public hamlib
  (package
    (name "hamlib")
    (version "4.6.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Hamlib/Hamlib")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1vq5ppn5hipklpp49wvq252br4s35mjc2z9dr23yhrx4qxby6gnr"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf
           automake
           doxygen
           libtool
           pkg-config
           python-wrapper
           swig))
    (inputs
     (list gd
           libusb
           libxml2
           lua
           python
           readline
           tcl))
    (arguments
     `(#:configure-flags
       '("--disable-static"
         "--with-lua-binding"
         "--with-python-binding"
         "--with-tcl-binding"
         "--with-xml-support")))
    (synopsis "Tools and API to control radios")
    (description
     "The Ham Radio Control Library (Hamlib) is a project to provide programs
with a consistent Application Programming Interface (API) for controlling the
myriad of radios and rotators available to amateur radio and communications
users.")
    (home-page "https://hamlib.github.io/")
    (license (list license:gpl2+ license:lgpl2.1+))))

(define wsjtx-hamlib
  ;; Fork of hamlib with custom patches used by wsjtx.
  (package
    (inherit hamlib)
    (name "wsjtx-hamlib")
    (version "2.5.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.code.sf.net/u/bsomervi/hamlib.git")
             (commit (string-append "wsjtx-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1bgf7bz2280739a7ip7lvpns0i7x6svryxfmsp32cff2dr146lz3"))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)
       ("texinfo" ,texinfo)
       ,@(package-native-inputs hamlib)))
    (arguments
     `(#:configure-flags '("--disable-static"
                           "--with-lua-binding"
                           "--with-python-binding"
                           "--with-tcl-binding"
                           "--with-xml-support")))))

(define-public jtdx-hamlib
  ;; Fork of hamlib with custom patches used by jtdx.
  (package
    (inherit hamlib)
    (name "jtdx-hamlib")
    (version "2.2.158")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jtdx-project/jtdxhamlib.git")
             (commit "158")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0m9i5k1n6j0nvmsqcy12x2ngqzjvxxlc3jg29igh93hb7lprlkjv"))))
    (native-inputs
     (modify-inputs (package-native-inputs hamlib)
       (prepend autoconf automake libtool texinfo)))
    (arguments
     `(#:configure-flags '("--disable-shared"
                           "--enable-static"
                           "--without-cxx-binding"
                           "--disable-winradio")))))

(define-public tlf
  (package
    (name "tlf")
    (version "1.4.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tlf/tlf")
             (commit (string-append "tlf-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1xpgs4k27pjd9mianfknknp6mf34365bcp96wrv5xh4dhph573rj"))
       (patches
        (search-patches "tlf-support-hamlib-4.2+.patch"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags
       (list "--enable-fldigi-xmlrpc")))
    (native-inputs
     (list autoconf automake perl pkg-config))
    (inputs
     (list cmocka
           glib
           hamlib
           libusb ;`Requires.private: libusb-1.0` in hamlib pkg-config
           ncurses
           xmlrpc-c))
    (home-page "https://tlf.github.io/")
    (synopsis "Amateur radio contest logging for the terminal")
    (description "TLF is a @acronym{Text User Interface, TUI} amateur radio
contest logging program.  It integrates with radios supported by hamlib and
other ham radio programs like fldigi.  Many contests are supported including:

@itemize
@item CQWW (SO, M/S and M/M)
@item WPX (SO, M/S and M/M)
@item ARRL Sweepstakes (SO, M/S )
@item EU SPRINT
@item EUHFC
@item ARRL-DX (both sides)
@item ARRL-FD
@item ARRL 10m
@item ARRL 160m
@item Region1 field day
@item SP DX contest
@item PACC (both sides)
@item NRAU - scandinavian
@item Wysiwyg mults mode (per band or per contest)
@item WAEDX
@end itemize

It also supports connecting to DX clusters, log synchronization with other TLF
instances over the network, and general QSO and DXpedition logging.")
    (license license:gpl2+)))

(define-public wsjtx
  (package
    (name "wsjtx")
    (version "2.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.code.sf.net/p/wsjt/wsjtx.git")
             (commit (string-append "wsjtx-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1d000ndqxh7chkzaig848xp3any0mr6swzf0vdzprkr1jm6dj0q0"))))
    (build-system qt-build-system)
    (arguments
     (list #:tests? #f   ; No test suite
           #:configure-flags
           (if (this-package-native-input "ruby-asciidoctor")
             #~'()
             #~(list "-DWSJT_GENERATE_DOCS=OFF"))))
    (native-inputs
     (append (list asciidoc
                   gfortran
                   pkg-config
                   qttools-5)
             (if (supported-package? ruby-asciidoctor)
               (list ruby-asciidoctor)
               '())))
    (inputs
     (list boost
           fftw
           fftwf
           hamlib
           libusb
           qtbase-5
           qtmultimedia-5
           qtserialport-5))
    (home-page "https://wsjt.sourceforge.io/wsjtx.html")
    (synopsis "Weak-signal ham radio communication program")
    (description
     "WSJT-X implements communication protocols or modes called FT4, FT8,
JT4, JT9, JT65, QRA64, ISCAT, MSK144, and WSPR, as well as one called Echo for
detecting and measuring your own radio signals reflected from the Moon.  These
modes were all designed for making reliable, confirmed QSOs under extreme
weak-signal conditions.")
    (license license:gpl3)))

(define-public jtdx
  (package
    (name "jtdx")
    (version "2.2.158")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jtdx-project/jtdx")
             (commit "158")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1lw9q7ggh2jlasipl3v5pkbabysjr6baw15lnmg664ah3fwdrvnx"))))
    (build-system qt-build-system)
    (native-inputs
     (append (list asciidoc gfortran pkg-config qttools-5)
             (if (supported-package? ruby-asciidoctor)
                 (list ruby-asciidoctor)
                 '())))
    (inputs
     (list
      boost
      fftw
      fftwf
      jtdx-hamlib
      libusb
      qtbase-5
      qtwebsockets-5
      qtmultimedia-5
      qtserialport-5))
    (arguments
     `(,@(if (this-package-native-input "ruby-asciidoctor")
             '()
             `(#:configure-flags '("-DWSJT_GENERATE_DOCS=OFF")))
       #:tests? #f)) ; No test suite
    (synopsis "Weak-signal ham radio communication program, forked from WSJTX")
    (description
     "JTDX means \"JT,T10 and FT8 and FT4 modes for DXing\", it is being
developed with main focus on the sensitivity and decoding efficiency, both, in
overcrowded and half empty HF band conditions.

It is modified WSJT-X software forked from WSJT-X r6462.  JTDX supports JT9,
JT65, T10, FT8 and FT4 © digital modes for HF amateur radio communication,
focused on DXing and being shaped by community of DXers.JTDX")
    (home-page "https://www.jtdx.tech/en/")
    (license license:gpl3)))

(define-public js8call
  (package
    (name "js8call")
    (version "2.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://files.js8call.com/" version
                           "/js8call-" version ".tgz"))
       (sha256
        (base32 "149sjwc4zg6ckgq26af93p4fxappa4k9dh7rdy67g8ajfjad4cd8"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            ;; Delete bundled boost to use the shared one.
            (delete-file-recursively "boost")))))
    (build-system qt-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-paths
            (lambda _
              ;; XXX: How to get the /tmp/<build-name>.drv-<num> path? Use
              ;; output path for after install check phase instead.
              (substitute* "media/tests/test"
                (("~/js8call-prefix/build/js8")
                 (string-append #$output "/bin/js8"))
                (("/opt/js8call/bin/js8")
                 (string-append #$output "/bin/js8")))
              (substitute* "CMakeLists.txt"
                (("DESTINATION /usr/share")
                 (string-append "DESTINATION " #$output "/share")))))
          (add-after 'unpack 'fix-hamlib
            (lambda _
              (substitute* "CMake/Modules/Findhamlib.cmake"
                (("set \\(ENV\\{PKG_CONFIG_PATH\\}.*\\)")
                 "set (__pc_path $ENV{PKG_CONFIG_PATH})
  list (APPEND __pc_path \"${__hamlib_pc_path}\")
  set (ENV{PKG_CONFIG_PATH} \"${__pc_path}\")"))
              (substitute* "HamlibTransceiver.hpp"
                (("#ifdef JS8_USE_LEGACY_HAMLIB")
                 "#if 1"))))
          (delete 'check)
          (add-after 'install 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (with-directory-excursion "../js8call/media/tests"
                  (invoke "./test"))))))))
    (native-inputs
     (list asciidoc
           gfortran
           pkg-config
           qttools-5))
    (inputs
     (list boost
           fftw
           fftwf
           libusb
           qtbase-5
           qtmultimedia-5
           qtserialport-5
           wsjtx-hamlib))
    (home-page "http://js8call.com/")
    (synopsis "Weak-signal ham radio communication program")
    (description
     "JS8Call is a software using the JS8 digital mode (a derivative of the FT8
mode) providing weak signal keyboard to keyboard messaging to amateur radio
operators.")
    (license license:gpl3)))

(define-public xnec2c
  (package
    (name "xnec2c")
    (version "4.4.16")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.xnec2c.org/releases/xnec2c-v"
                           version ".tar.gz"))
       (sha256
        (base32 "0sdfmaaipcz23807xiaxjkxw0m0px4plgr73lp37hz76jzr649jy"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf
           automake
           gettext-minimal
           `(,glib "bin")
           libtool
           pkg-config))
    (inputs
     (list gtk+ openblas))
    (arguments
     `(#:configure-flags
       ,#~(list (string-append "--with-openblas-incdir="
                               #$(this-package-input "openblas")
                               "/include"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "src/Makefile.am"
               (("\\$\\(GLIB_COMPILE_RESOURCES\\)")
                (search-input-file inputs "bin/glib-compile-resources")))
             (substitute* "src/mathlib.c"
               (("libopenblas\\.so")
                (search-input-file inputs "lib/libopenblas.so"))))))))
    (synopsis "Antenna modeling software")
    (description
     "Xnec2c is a GTK3-based graphical version of nec2c, a translation to the
C language of NEC2, the FORTRAN Numerical Electromagnetics Code commonly used
for antenna simulation and analysis.  It can be used to define the geometry of
an antenna, and then plot the radiation pattern or frequency-related data like
gain and standing wave ratio.")
    (home-page "https://www.xnec2c.org/")
    (license license:gpl3+)))

(define-public dump1090
  (package
    (name "dump1090")
    (version "8.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/flightaware/dump1090")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "16ylywy2fdwf5kqr8kgl9lbzy1zwx4ckj9y122k3h86pfkswljs9"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list bladerf hackrf libusb ncurses rtl-sdr))
    (arguments
     (list
      #:test-target "test"
      #:make-flags
      #~(list (string-append "CC=" #$(cc-for-target)))
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (replace 'install
            (lambda _
              (let ((bin (string-append #$output "/bin/")))
                (install-file "dump1090" bin)
                (install-file "view1090" bin)))))))
    (synopsis "Mode S decoder for rtl-sdr devices")
    (description
     "Dump1090 is a Mode S decoder specifically designed for rtl-sdr devices.
It can be used to decode the ADS-B signals that planes emit to indicate
their position, altitude, speed, etc.")
    (home-page "https://github.com/flightaware/dump1090")
    (license license:gpl2+)))

(define-public libacars
  (package
    (name "libacars")
    (version "2.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/szpajder/libacars")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "08cadcqzhl3i7hpd8jwph33kx52vdwbrj1rlagwrkwb2mfw6szfs"))))
    (build-system cmake-build-system)
    (inputs (list jansson libxml2 zlib))
    (arguments (list #:tests? #f)) ; No test suite
    (synopsis "Decoder for ACARS messages")
    (description "This package provides a library for decoding the contents of
ACARS messages used by planes.")
    (home-page "https://github.com/szpajder/libacars")
    (license (list license:bsd-2
                   license:expat))))

(define-public dumpvdl2
  (package
    (name "dumpvdl2")
    (version "2.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/szpajder/dumpvdl2")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1zxv24fg2ciy7bfiqhx95v0h8b1bnbs3ax06n9ywsssbf4ndas4n"))))
    (build-system cmake-build-system)
    (native-inputs (list pkg-config))
    (inputs
     (list glib
           libacars
           protobuf-c
           rtl-sdr
           soapysdr
           sqlite
           zeromq))
    (arguments (list #:tests? #f)) ; No test suite
    (synopsis "VDL Mode 2 message decoder")
    (description "This package provides a decoder for VDL Mode 2 messages used
by planes.")
    (home-page "https://github.com/szpajder/dumpvdl2")
    (license license:gpl3+)))

(define-public rtl-433
  (package
    (name "rtl-433")
    (version "23.11")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/merbanan/rtl_433")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "11qigwnaa22vgd43jvzk2byiancahdkhxpsh6cp74q2ywb0wy9x8"))))
    (build-system cmake-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list libusb openssl rtl-sdr soapysdr))
    (synopsis "Decoder for radio transmissions in ISM bands")
    (description
     "This is a generic data receiver, mainly for decoding radio transmissions
from devices on the 433 MHz, 868 MHz, 315 MHz, 345 MHz and 915 MHz ISM bands.")
    (home-page "https://github.com/merbanan/rtl_433")
    (license license:gpl2+)))

(define-public multimon-ng
  (package
    (name "multimon-ng")
    (version "1.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/EliasOenal/multimon-ng")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "00v8ix51gn30mc6bhm7fi37cyksaf8vs27xsi1435kpix9askcla"))))
    (build-system cmake-build-system)
    (inputs
     (list libx11 pulseaudio))
    (arguments
     '(#:tests? #f))                    ; no test suite
    (home-page "https://github.com/EliasOenal/multimon-ng")
    (synopsis "Decoder for digital radio transmission modes")
    (description "Multimon-ng can decode several digital radio transmission
modes:
@itemize
@item POCSAG512, POCSAG1200, POCSAG2400
@item FLEX
@item EAS
@item UFSK1200, CLIPFSK, AFSK1200, AFSK2400, AFSK2400_2, AFSK2400_3
@item HAPN4800
@item FSK9600
@item DTMF
@item ZVEI1, ZVEI2, ZVEI3, DZVEI, PZVEI
@item EEA, EIA, CCIR
@item MORSE CW
@item X10
@end itemize")
    (license license:gpl2+)))

(define-public nanovna-saver
  (package
    (name "nanovna-saver")
    (version "0.6.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/NanoVNA-Saver/nanovna-saver")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "07mih8jgpnnq101yqwv82sihbfjqi47dkvni34minbp19676q1bw"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-cython python-setuptools-scm))
    (inputs
     (list python-numpy python-pyqt-6 python-pyserial python-scipy))
    (arguments
     (list #:tests? #f
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'set-version
                 (lambda _
                   (setenv "SETUPTOOLS_SCM_PRETEND_VERSION" #$version))))))
    (home-page "https://github.com/NanoVNA-Saver/nanovna-saver")
    (synopsis "GUI for NanoVNA devices")
    (description
     "NanoVNA-Saver is a tool for reading, displaying and saving data from the
NanoVNA vector network analyzers.")
    (license license:gpl3+)))

(define-public qsstv
  (package
    (name "qsstv")
    (version "9.5.8")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://users.telenet.be/on4qz/qsstv/downloads/"
                           "qsstv_" version ".tar.gz"))
       (sha256
        (base32 "0s3sivc0xan6amibdiwfnknrl3248wzgy98w6gyxikl0qsjpygy0"))))
    (build-system qt-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list alsa-lib
           fftw
           fftwf
           hamlib
           openjpeg
           pulseaudio
           qtbase-5
           v4l-utils))
    (arguments
     `(#:tests? #f  ; No test suite.
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (invoke "qmake"
                     (string-append "PREFIX=" (assoc-ref outputs "out"))))))))
    (home-page "http://users.telenet.be/on4qz/qsstv/")
    (synopsis "Program for receiving and transmitting SSTV and HAMDRM")
    (description
     "QSSTV is a program for receiving and transmitting SSTV and HAMDRM
(sometimes called DSSTV).  It is compatible with most of MMSSTV and EasyPal.")
    (license (list license:gpl2+
                   license:qwt1.0))))

(define-public direwolf
  (package
    (name "direwolf")
    (version "1.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/wb2osz/direwolf")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0zhigg0slb8gmv622ayaj0nba5kxcgdyadgv0lmyqaw2mvlmrg2m"))))
    (build-system cmake-build-system)
    (inputs
     (list alsa-lib hamlib))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-paths
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "conf/CMakeLists.txt"
               (("DESTINATION /etc")
                (string-append "DESTINATION "
                               (assoc-ref outputs "out")
                               "/etc"))))))))
    (home-page "https://github.com/wb2osz/direwolf")
    (synopsis "TNC for Amateur Packet Radio")
    (description
     "Dire Wolf is a Terminal Node Controller (TNC) for Amateur Packet Radio.
It can perform as:
@itemize
@item APRS GPS tracker,
@item Digipeater,
@item Internet gateway (IGate)
@item APRStt gateway
@end itemize\n")
    (license license:gpl2+)))

(define-public aldo
  (package
    (name "aldo")
    (version "0.7.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://savannah/aldo/aldo-" version ".tar.bz2"))
       (sha256
        (base32 "14lzgldqzbbzydsy1cai3wln3hpyj1yhj8ji3wygyzr616fq9f7i"))))
    (build-system gnu-build-system)
    (inputs
     (list ao))
    (home-page "https://www.nongnu.org/aldo/")
    (synopsis "Morse code tutor")
    (description
     "Aldo is a morse code learning tool providing four type of training
methods:

@itemize
@item Classic exercise,
@item Koch method,
@item Read from file,
@item Callsign exercise.
@end itemize\n")
    (license license:gpl3+)))

(define-public unixcw
  (package
    (name "unixcw")
    (version "3.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/unixcw/unixcw-"
                           version ".tar.gz"))
       (sha256
        (base32 "15wriwv91583kmmyijbzam3dpclzmg4qjyfzjv5f75x9b0gqabxm"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list alsa-lib ncurses pulseaudio qtbase-5))
    (arguments
     `(#:configure-flags '("--disable-static")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* '("configure"
                            "src/config.h.in"
                            "src/cwcp/Makefile.am"
                            "src/cwcp/Makefile.in")
               (("-lcurses")
                "-lncurses"))
             (substitute* "src/libcw/libcw_pa.c"
               (("libpulse-simple.so" all)
                (search-input-file inputs "/lib/libpulse-simple.so"))))))))
    (home-page "https://unixcw.sourceforge.net/")
    (synopsis "Morse code library and programs")
    (description
     "@code{unixcw} is a project providing the libcw library and a set of
programs using the library: cw, cwgen, cwcp and xcwcp.  The programs are
intended for people who want to learn receiving and sending morse code.")
    (license license:gpl2+)))

(define-public kochmorse
  (package
    (name "kochmorse")
    (version "3.5.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hmatuschek/kochmorse")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1s1aj223n57rpc95rih98z08xnyhq2zp02byzrc3f7s01fv3nj0l"))))
    (build-system qt-build-system)
    (native-inputs
     (list qttools-5))
    (inputs
     (list qtbase-5 qtmultimedia-5))
    (arguments
     `(#:tests? #f)) ; No test suite
    (home-page "https://dm3mat.darc.de/kochmorse/")
    (synopsis "Morse code tutor")
    (description
     "KochMorse is a simple morse-code tutor using the Koch method.")
    (license license:gpl2+)))

(define-public ggmorse
  (let ((commit "8fb433d6cd6a71940f51b5724663ec0c75bf0b62")
        (revision "1"))
    (package
      (name "ggmorse")
      (version (git-version "0.1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/ggerganov/ggmorse")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1lhsmyhljqa6apzbysqar56wpfcdvs3pq9ia1mshqd6d3hz74s78"))))
      (build-system cmake-build-system)
      (arguments
       (list #:configure-flags #~(list "-DGGMORSE_SUPPORT_SDL2=OFF")
             #:phases #~(modify-phases %standard-phases
                          (add-after 'unpack 'disable-imgui-build
                            (lambda _
                              (substitute* "examples/CMakeLists.txt"
                                (("add_subdirectory\\(third-party\\)")
                                 "")))))))
      (synopsis "Morse code decoder")
      (description "GGMorse is a library that decodes Morse code in real-time
from raw audio.")
      (home-page "https://ggmorse.ggerganov.com/")
      (license license:expat))))

(define-public gnuais
  (package
    (name "gnuais")
    (version "0.3.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rubund/gnuais")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1rik5fdfslszdn3yvj769jzmnv9pirzf76ki33bjjzk7nkabbnlm"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("mariadb-dev" ,mariadb "dev")
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("curl" ,curl)
       ("gtk+" ,gtk+)
       ("libsoup" ,libsoup-minimal)
       ("mariadb-lib" ,mariadb "lib")
       ("osm-gps-map" ,osm-gps-map)
       ("pulseaudio" ,pulseaudio)))
    (arguments
     `(#:configure-flags '("-DCMAKE_C_FLAGS=-fcommon")
       #:tests? #f ; No test suite
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-paths
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "src/cfgfile.c"
               (("/usr/share/")
                (string-append (assoc-ref outputs "out") "/share/"))))))))
    (home-page "https://gnuais.sourceforge.net/")
    (synopsis "AIS message demodulator and decoder")
    (description
     "This program contains algorithms to demodulate and decode AIS (Automatic
Identification System) messages sent by ships and coast stations.")
    (license license:gpl2+)))

(define-public kappanhang
  (package
    (name "kappanhang")
    (version "1.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nonoo/kappanhang")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ycy8avq5s7zspfi0d9klqcwwkpmcaz742cigd7pmcnbbhspcicp"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/nonoo/kappanhang"
       #:install-source? #f))
    (inputs
     (list go-github-com-akosmarton-papipes
           go-github-com-fatih-color
           go-github-com-google-goterm
           go-github-com-mattn-go-isatty
           go-github-com-mesilliac-pulse-simple
           go-github-com-pborman-getopt
           go-go-uber-org-multierr
           go-go-uber-org-zap))
    (home-page "https://github.com/nonoo/kappanhang")
    (synopsis "Client for Icom RS-BA1 server")
    (description
     "Kappanhang remotely opens audio channels and a serial port to an Icom
RS-BA1 server.  The application is mainly developed for connecting to the Icom
IC-705 transceiver, which has built-in WiFi and RS-BA1 server.

Compatible hardware/software:
@itemize
@item Icom RS-BA1 server software,
@item Icom IC-705
@item Icom IC-9700
@end itemize\n")
    (license license:expat)))

(define-public dream
  (package
    (name "dream")
    (version "2.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/drm/dream/" version
                           "/dream_" version ".orig.tar.gz"))
       (sha256
        (base32 "0mpg341b0vnm6ym0cag9zri9w6kw012rv68zdmmi2hlvq7iiw8gp"))))
    (build-system qt-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list faad2
           fftw
           libsndfile
           libpcap
           opus
           pulseaudio
           qtbase-5
           qtsvg-5
           qwt
           speexdsp
           zlib))
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-paths
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (substitute* "dream.pro"
               (("target\\.path = /usr/bin")
                (string-append "target.path = "
                               (assoc-ref outputs "out") "/bin"))
               (("documentation\\.path = /usr/share/man/man1")
                (string-append "documentation.path = "
                               (assoc-ref outputs "out")
                               "/share/man/man1"))
               (("/usr/include/pulse")
                (search-input-directory inputs "/include/pulse"))
               (("/usr/include/sndfile\\.h")
                (search-input-file inputs "/include/sndfile.h"))
               (("/usr/include/opus")
                (search-input-directory inputs "/include/opus"))
               (("/usr/include/speex")
                (search-input-directory inputs "/include/speex"))
               (("/usr/include/qwt")
                (search-input-directory inputs "/include/qwt"))
               (("\\$\\$OUT_PWD/include/neaacdec\\.h")
                (search-input-file inputs "/include/neaacdec.h")))))
         (replace 'configure
           (lambda _
             (invoke "qmake"))))))
    (home-page "https://sourceforge.net/projects/drm/")
    (synopsis "Digital Radio Mondiale receiver")
    (description
     "Dream is a software implementation of a Digital Radio Mondiale (DRM)
receiver.")
    (license license:gpl2+)))

(define-public welle-io
  (package
    (name "welle-io")
    (version "2.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/albrechtl/welle.io")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1c3pcn2apc7sf68scwnij6xr00x25650387gr05z7xw3k36jfadi"))))
    (build-system qt-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list airspy
           alsa-lib
           faad2
           fftwf
           lame
           libusb
           mpg123
           rtl-sdr
           qtdeclarative
           qtcharts
           qt5compat
           qtmultimedia
           soapysdr))
    (arguments
     (list #:qtbase qtbase
           #:configure-flags #~(list "-DAIRSPY=ON"
                                     "-DRTLSDR=ON"
                                     "-DSOAPYSDR=ON")
           #:tests? #f))
    (home-page "https://www.welle.io/")
    (synopsis "DAB and DAB+ software radio")
    (description
     "@code{welle.io} is a Digital Audio Broadcasting (DAB and DAB+) software
defined radio with support for rtl-sdr.")
    (license license:gpl2+)))

(define-public csdr
  (package
    (name "csdr")
    (version "0.18.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jketterl/csdr")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0j5d64na47w1j1sprwj41d9dzvs2x7xwyp0pbl439g686iwp7m9d"))))
    (build-system cmake-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list fftwf libsamplerate))
    (arguments
     (list #:tests? #f)) ; No check phase
    (home-page "https://github.com/jketterl/csdr")
    (synopsis "DSP for software defined radio")
    (description
     "This package includes the @code{libcsdr} library of
@acronym{DSP, Digital Signal Processing} functions for
@acronym{SDRs, Software Defined Radios}, and the @code{csdr} command line
program that can be used to build simple signal processing flow graphs.")
    (license license:gpl3+)))

(define-public convert-samples
  (package
    (name "convert-samples")
    (version "3.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://codeberg.org/glv/convert-samples")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1d9w9m5agi8fiv1wk8nhjrbm2jkm2fks4ymbxkn0xphbwj3gwr7i"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake))
    (inputs
     (list liquid-dsp))
    (synopsis "SDR samples converter")
    (description
     "@code{convert-samples} is a command-line program to convert samples
received from software defined radios from one format to another.

Supported formats:
@itemize
@item s8: signed 8 bit integer
@item u8: unsigned 8 bit integer
@item s16: signed 16 bit integer
@item u16: unsigned 16 bit integer
@item s32: signed 32 bit integer
@item u32: unsigned 32 bit integer
@item f32: 32 bit float
@item cs8: complex made of signed 8 bit integers
@item cu8: complex made of unsigned 8 bit integers
@item cs16: complex made of signed 16 bit integers
@item cu16: complex made of unsigned 16 bit integers
@item cs32: complex made of signed 32 bit integers
@item cu32: complex made of unsigned 32 bit integers
@item cf32: complex made of 32 bit floats
@end itemize")
    (home-page "https://codeberg.org/glv/convert-samples")
    (license license:gpl3+)))

(define-public serialdv
  (package
    (name "serialdv")
    (version "1.1.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/f4exb/serialDV")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0d88h2wjhf79nisiv96bq522hkbknzm88wsv0q9k33mzmrwnrx93"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f))  ; No test suite.
    (home-page "https://github.com/f4exb/serialDV")
    (synopsis "Audio interface for AMBE3000 based devices")
    (description
     "SerialDV is a minimal interface to encode and decode audio with AMBE3000
based devices in packet mode over a serial link.")
    (license license:gpl3+)))

(define-public cm256cc
  (package
    (name "cm256cc")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/f4exb/cm256cc")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1n9v7g6d370263bgqrjv38s9aq5953rzy7jvd8i30xq6aram9djg"))))
    (build-system cmake-build-system)
    (arguments
     ;; Disable some SIMD features for reproducibility.
     `(#:configure-flags '("-DENABLE_DISTRIBUTION=1")
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "./cm256_test")))))))
    (home-page "https://github.com/f4exb/cm256cc")
    (synopsis "Cauchy MDS Block Erasure Codec")
    (description
     "This is a C++ library implementing fast GF(256) Cauchy MDS Block Erasure
Codec.")
    (license license:gpl3+)))

(define-public libdab
  ;; No release since 2017, use commit directly.
  (let ((commit "b578d02eda60f613d35bab5d762ae7c9a27758d8")
        (revision "1"))
    (package
      (name "libdab")
      (version (git-version "0.8" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/JvanKatwijk/dab-cmdline")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0j339kx3n2plgfw7ikpp7b81h5n68wmsgflwljbh2sy8j62faik9"))))
      (build-system cmake-build-system)
      (inputs
       (list faad2 fftwf zlib))
      (arguments
       `(#:tests? #f  ; No test suite.
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'enter-sources-directory
             (lambda _
               (chdir "library"))))))
      (home-page "https://github.com/JvanKatwijk/dab-cmdline")
      (synopsis "DAB decoding library")
      (description "This is a library to decode @acronym{DAB/DAB+, Digital
Audio Broadcasting}.")
      (license license:gpl2+))))

(define-public dsd
  (let ((commit "59423fa46be8b41ef0bd2f3d2b45590600be29f0")
        (revision "1"))
    (package
      (name "dsd")
      (version (git-version "1.7.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/szechyjs/dsd")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "128gvgkanvh4n5bjnzkfk419hf5fdbad94fb8d8lv67h94vfchyd"))))
      (build-system cmake-build-system)
      (native-inputs
       (list pkg-config))
      (inputs
       (list itpp libsndfile mbelib portaudio))
      (arguments
       (list #:phases
             #~(modify-phases %standard-phases
                 (add-after 'unpack 'fix-itpp-detection
                   (lambda _
                     (substitute* "cmake/FindITPP.cmake"
                       (("libitpp\\.dll")
                        "itpp_debug")))))))
      (synopsis "Digital speech decoder")
      (description
       "DSD is able to decode several digital voice formats used in radio
transmissions.")
      (home-page "https://github.com/szechyjs/dsd")
      (license (list license:expat license:gpl2)))))

(define-public dsdcc
  (package
    (name "dsdcc")
    (version "1.9.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/f4exb/dsdcc")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1rb9r1m4rfi9x5x4h5frpl65xmk5p2bqyfisnrv6nbmnsgds9h0c"))))
    (build-system cmake-build-system)
    (inputs
     (list mbelib serialdv))
    (arguments
     `(#:tests? #f  ; No test suite.
       #:configure-flags
       (list "-DUSE_MBELIB=ON"
             (string-append "-DLIBMBE_INCLUDE_DIR="
                            (assoc-ref %build-inputs "mbelib")
                            "/include")
             (string-append "-DLIBMBE_LIBRARY="
                            (assoc-ref %build-inputs "mbelib")
                            "/lib/libmbe.so")
             (string-append "-DLIBSERIALDV_INCLUDE_DIR="
                            (assoc-ref %build-inputs "serialdv")
                            "/include/serialdv")
             (string-append "-DLIBSERIALDV_LIBRARY="
                            (assoc-ref %build-inputs "serialdv")
                            "/lib/libserialdv.so"))))
    (home-page "https://github.com/f4exb/dsdcc")
    (synopsis "Digital speech decoder")
    (description
     "This package provides a library and a program to decode several digital
voice formats.")
    (license license:gpl3+)))

(define-public sdrangel
  (package
    (name "sdrangel")
    (version "7.22.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/f4exb/sdrangel")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "02m9kqkk9alnib51b67zssm8126c6ljsy4zfy1sz3zz5z3w10l0w"))))
    (build-system qt-build-system)
    (native-inputs
     (list doxygen graphviz pkg-config))
    (inputs
     (list airspy
           airspyhf
           alsa-lib
           aptdec
           bladerf
           boost
           cm256cc
           codec2
           dsdcc
           faad2
           ffmpeg
           fftwf
           flac
           ggmorse
           hackrf
           hamlib
           hidapi
           libdab
           libusb
           mbelib
           opencv
           opus
           pulseaudio
           qtbase-5
           qtcharts-5
           qtdeclarative-5
           qtgamepad
           qtgraphicaleffects
           qtlocation-5
           qtmultimedia-5
           qtquickcontrols2-5
           qtserialport-5
           qtspeech-5
           qtsvg-5
           qtwebchannel-5
           qtwebengine-5
           qtwebsockets-5
           rtl-sdr
           serialdv
           soapysdr
           sgp4
           zlib))
    (arguments
     `(#:tests? #f  ; No test suite.
       #:configure-flags
       ,#~(list (string-append "-DAPT_DIR="
                               #$(this-package-input "aptdec"))
                (string-append "-DDAB_DIR="
                               #$(this-package-input "libdab"))
                (string-append "-DDSDCC_DIR="
                               #$(this-package-input "dsdcc"))
                (string-append "-DMBE_DIR="
                               #$(this-package-input "mbelib"))
                (string-append "-DSERIALDV_DIR="
                               #$(this-package-input "serialdv"))
                (string-append "-DSGP4_DIR="
                               #$(this-package-input "sgp4"))
                (string-append "-DSOAPYSDR_DIR="
                               #$(this-package-input "soapysdr")))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-unrecognized-compiler-option
           (lambda _
             (substitute* "cmake/Modules/CompilerOptions.cmake"
               (("-Wno-inconsistent-missing-override")
                "-fpermissive"))))
         (add-after 'unpack 'fix-CPU-extension-detection
           ;; ‘Fix’ in the static sense.  TODO: Make this -tune'able.
           (lambda _
             (substitute* "CMakeLists.txt"
               (("set\\(ARCH_OPT \"native\"")
                "set(ARCH_OPT \"\""))
             (let ((file "cmake/Modules/DetectArchitecture.cmake"))
               ;; Disable all build-time CPU extension detection…
               (substitute* file
                 (("detect_extensions\\(.*") ""))
               (when ,(target-x86-64?)
                 ;; …but force extensions that are guaranteed to be available.
                 (substitute* file
                   ((".*cmake_pop_check_state" eof)
                    (string-append "force_ext_available(SSE2)\n" eof))))))))))
    (home-page "https://github.com/f4exb/sdrangel/wiki")
    (synopsis "Software defined radio")
    (description
     "SDRangel is a Qt software defined radio and signal analyzer frontend for
various hardware.")
    (license license:gpl3+)))

(define-public sdr++
  (let ((commit "b89fdba433cf6aa0dab424a06974a0b45abf6c4a")
        (revision "1"))
    (package
      (name "sdr++")
      (version (git-version "1.1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/AlexandreRouma/SDRPlusPlus")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "11l1ja3dwxa67rp09x4rr5pd6rh6amn48z5vv6dygspq64w63hp2"))))
      (build-system cmake-build-system)
      (native-inputs
       (list pkg-config))
      (inputs
       (list airspy
             airspyhf
             alsa-lib
             bladerf
             codec2
             fftwf
             glew
             glfw
             hackrf
             jack-2
             libusb
             pulseaudio
             rtaudio
             rtl-sdr
             soapysdr
             volk
             (list zstd "lib")))
      (arguments
       (list #:tests? #f ; No test suite.
             #:configure-flags #~(list "-DOPT_BUILD_BLADERF_SOURCE=ON"
                                       "-DOPT_BUILD_PLUTOSDR_SOURCE=OFF"
                                       "-DOPT_BUILD_M17_DECODER=ON")
             #:phases
             #~(modify-phases %standard-phases
                 (add-after 'unpack 'fix-paths
                   (lambda _
                     (substitute* "CMakeLists.txt"
                       (("/usr")
                        #$output)))))))
      (home-page "https://github.com/AlexandreRouma/SDRPlusPlus")
      (synopsis "Software defined radio software")
      (description
       "SDR++ is a software defined radio software for various hardware.")
      (license license:gpl3+))))

(define-public inspectrum
  (package
    (name "inspectrum")
    (version "0.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/miek/inspectrum")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "11v5idwvfi9w60qg4fgqgvm7ahmb0ys4j094qv4c93r92kd9d3f9"))))
    (build-system qt-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list fftwf liquid-dsp qtbase-5))
    (home-page "https://github.com/miek/inspectrum")
    (synopsis "Radio signal analyser")
    (description
     "Inspectrum is a tool for analysing captured signals, primarily from
software-defined radio receivers.")
    (license license:gpl3+)))

(define-public wfview
  (package
    (name "wfview")
    (version "2.03")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/eliggett/wfview")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0b74sbi10plrd6dyqm80k0gggvh7fdnwzlddk18gnj5zzsiq562f"))))
    (build-system qt-build-system)
    (arguments
     (list
      #:tests? #f  ; No test suite.
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-paths
            (lambda _
              (substitute* "wfview.pro"
                (("`hostname`")
                 "guix")
                (("`whoami`")
                 "build")
                (("\\$\\(shell git -C .* HEAD\\)")
                 "")
                (("!win32:LIBS \\+= -L\\./ -lopus")
                 "!win32:LIBS += -L./ -lopus -lqcustomplot")
                (("/sbin/")
                 ""))
              (substitute* '("wfmain.cpp")
                (("/usr/share")
                 (string-append #$output "/share")))))
          (replace 'configure
            (lambda _
              (mkdir-p "build")
              (chdir "build")
              (invoke "qmake"
                      (string-append "PREFIX=" #$output)
                      "../wfview.pro"))))))
    (inputs
     (list eigen
           eudev
           hidapi
           opus
           portaudio
           pulseaudio
           qcustomplot
           ;; TODO: Needs to be renamed to qtgamepad-5 when version 6 is
           ;; packed.
           qtgamepad
           qtbase-5
           qtmultimedia-5
           qtserialport-5
           qtwebsockets-5
           rtaudio))
    (home-page "https://wfview.org/")
    (synopsis "Software to control Icom radios")
    (description
     "@code{wfview} is a program to control modern Icom radios and view the
spectrum waterfall.  It supports at least the following models:

@itemize
@item IC-705
@item IC-905
@item IC-7300
@item IC-7610
@item IC-7850
@item IC-7851
@item IC-9700
@end itemize\n")
    (license (list license:expat
                   license:gpl3))))

(define-public minimodem
  (package
    (name "minimodem")
    (version "0.24")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://www.whence.com/minimodem/minimodem-"
                           version ".tar.gz"))
       (sha256
        (base32 "13ipyh39l7p420j1j9kvwyskv2nqnimls1a3z1klsa1zivds9k7q"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list alsa-lib fftwf libsndfile pulseaudio))
    (home-page "http://www.whence.com/minimodem/")
    (synopsis "Software audio FSK modem")
    (description
     "Minimodem is a command-line program which decodes (or generates) audio
modem tones at any specified baud rate, using various framing protocols.  It
acts a general-purpose software FSK modem, and includes support for various
standard FSK protocols such as Bell103, Bell202, RTTY, TTY/TDD, NOAA SAME, and
Caller-ID.")
    (license license:gpl3+)))

(define-public rfcat
  (package
    (name "rfcat")
    (version "1.9.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/atlas0fd00m/rfcat")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0zmgbgf1025ln2v6lc27dmkmwv8pxjgrmhmpk34rkkixhvnk69pf"))))
    (build-system python-build-system)
    (inputs
     (list python-future
           python-ipython
           python-numpy
           python-pyserial
           python-pyside-2
           python-pyusb))
    (arguments
     (list
      #:tests? #f  ; Tests want to use a serial port
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-permissions
            (lambda _
              (make-file-writable "rflib/rflib_version.py")))
          (add-after 'install 'install-udev-rules
            (lambda* _
              (install-file "etc/udev/rules.d/20-rfcat.rules"
                            (string-append #$output "/lib/udev/rules.d")))))))
    (home-page "https://github.com/atlas0fd00m/rfcat")
    (synopsis "Program to control some radio dongles")
    (description
     "@code{rfcat} is a program to control some radio dongles operating in
ISM bands.

Supported dongles:
@itemize
@item YARD Stick One
@item cc1111emk
@item chronos watch dongle
@item imme (limited support)
@end itemize

To install the rfcat udev rules, you must extend @code{udev-service-type} with
this package.  E.g.: @code{(udev-rules-service 'rfcat rfcat)}")
    (license (list license:bsd-3
                   license:gpl2))))

(define-public rx-tools
  ;; No tagged release since 2016, use commit instead.
  (let ((commit "811b21c4c8a592515279bd19f7460c6e4ff0551c")
        (revision "1"))
    (package
      (name "rx-tools")
      (version (git-version "1.0.3" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/rxseger/rx_tools")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0qr8q00cv6q0ikjrph0qh07mlbvgk4yimccpkn3ir8ib5ma0r9sr"))))
      (build-system cmake-build-system)
      (inputs
       (list soapysdr))
      (arguments
       `(#:tests? #f)) ; No test suite.
      (home-page "https://github.com/rxseger/rx_tools")
      (synopsis "Command line programs for receiving data from SDRs")
      (description
       "This package provides the @code{rx_fm}, @code{rx_power} and
@code{rx_sdr} tools for receiving data from SDRs, based on @code{rtl_fm},
@code{rtl_power} and @code{rtl_sdr} from RTL-SDR, but using the SoapySDR
vendor-neutral SDR support library instead, intended to support a wider range
of devices than RTL-SDR.")
      (license license:gpl2+))))

(define-public urh
  (package
    (name "urh")
    (version "2.9.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jopohl/urh")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0wfqdcfip1kg5b5a8d01bip5nqvjhs2x8bgc9vwhghn6vk8pqxxg"))))
    (build-system python-build-system)
    (native-inputs
     (list python-cython
           python-pytest
           xorg-server-for-tests))
    (inputs
     (list airspy
           bladerf
           gnuradio
           gr-osmosdr
           hackrf
           python-numpy
           python-psutil
           python-pyaudio
           python-pyqt
           rtl-sdr))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'configure-compiler
           (lambda _
             ;; Use gcc as compiler
             (substitute* "src/urh/dev/native/ExtensionHelper.py"
               (("compiler = ccompiler\\.new_compiler\\(\\)\n" all)
                (string-append
                 all "    compiler.set_executables(compiler='gcc',"
                 " compiler_so='gcc', linker_exe='gcc', linker_so='gcc -shared')\n")))))
         (add-after 'unpack 'disable-some-tests
           (lambda _
             ;; FIXME
             (for-each delete-file
                       '("tests/test_continuous_modulator.py"
                         ;; This test causes a segmentation fault
                         "tests/test_send_recv_dialog_gui.py"
                         ;; This test hangs forever
                         "tests/test_spectrogram.py"))))
         (add-after 'build 'build-cythonext
           (lambda _
             (invoke "python" "src/urh/cythonext/build.py")))
         (replace 'check
           (lambda* (#:key inputs tests? #:allow-other-keys)
             (when tests?
               (setenv "HOME" "/tmp")
               (system (string-append (search-input-file inputs "/bin/Xvfb")
                                     " :1 &"))
               (setenv "DISPLAY" ":1")
               (invoke "pytest")))))))
    (home-page "https://github.com/jopohl/urh")
    (synopsis "Wireless protocol investigation program")
    (description
     "The Universal Radio Hacker (URH) is a complete suite for wireless
protocol investigation with native support for many common Software Defined
Radios.")
    (license license:gpl3+)))

(define-public volk-gnsssdr
  (package
    (name "volk-gnsssdr")
    (version "0.0.19")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gnss-sdr/gnss-sdr")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0l1hqfqh8ffgy6nxqdk390vmnmhv66x7m8323mz2izczqc5acy1p"))))
    (build-system cmake-build-system)
    (native-inputs (list python python-mako))
    (inputs (list cpu-features))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'chdir
            (lambda _
              (chdir "src/algorithms/libs/volk_gnsssdr_module/volk_gnsssdr"))))))
    (home-page "https://github.com/gnss-sdr/gnss-sdr/blob/main/src/algorithms/libs/volk_gnsssdr_module/volk_gnsssdr/")
    (synopsis "Extra VOLK kernels for GNSS-SDR")
    (description
     "This library contains VOLK kernels of hand-written SIMD code for
different mathematical operations used by GNSS-SDR, mainly with 8-bit and
16-bit real and complex data types, offering a platform/architecture agnostic
version that will run in all machines, plus other versions for different SIMD
instruction sets.")
    (license license:gpl3+)))

(define-public gnss-sdr
  (package
    (name "gnss-sdr")
    (version "0.0.19")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gnss-sdr/gnss-sdr")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0l1hqfqh8ffgy6nxqdk390vmnmhv66x7m8323mz2izczqc5acy1p"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("googletest-source" ,(package-source googletest))
       ("orc" ,orc)
       ("pkg-config" ,pkg-config)
       ("python" ,python)
       ("python-mako" ,python-mako)))
    (inputs
     (list armadillo
           boost
           cpu-features
           fmt
           gflags
           glog
           gmp
           gnuradio
           gnuplot
           gnutls
           gr-osmosdr
           libpcap
           log4cpp
           matio
           openblas
           openssl
           protobuf
           pugixml
           spdlog
           volk
           volk-gnsssdr))
    (arguments
     `(#:configure-flags
       (list "-DENABLE_GENERIC_ARCH=ON"
             "-DENABLE_OSMOSDR=ON"

             "-DENABLE_UNIT_TESTING=FALSE" ; many tests needing data download
             "-DBLA_VENDOR=OpenBLAS"
             (string-append "-DGFLAGS_ROOT="
                            (assoc-ref %build-inputs "gflags"))
             (string-append "-DGLOG_ROOT="
                            (assoc-ref %build-inputs "glog"))
             (string-append "-DGTEST_DIR="
                            (assoc-ref %build-inputs "googletest-source")))
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'set-home
           (lambda _
             (setenv "HOME" "/tmp"))))))
    (home-page "https://gnss-sdr.org/")
    (synopsis "Global Navigation Satellite Systems software-defined receiver")
    (description
     "This program is a software-defined receiver which is able to process
(that is, to perform detection, synchronization, demodulation and decoding of
the navigation message, computation of observables and, finally, computation of
position fixes) the signals of the BeiDou, Galileo, GLONASS and GPS Global
Navigation Satellite System.")
    (license license:gpl3+)))

(define-public satdump
  (package
    (name "satdump")
    (version "1.2.2")
    (source
     ;; TODO: The sources embed some libraries (in src-core/libs).
     ;; Using regular packaged shared libraries instead will require big
     ;; changes in CMakeList files.
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/SatDump/SatDump")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "13f0r513pvf5wax81i6443z3i0808zqf3yppvln8171hsgwdwagr"))))
    (build-system cmake-build-system)
    (native-inputs (list pkg-config))
    (inputs
     (list airspy
           airspyhf
           armadillo
           bladerf
           curl
           fftwf
           glew
           glfw
           hackrf
           hdf5
           jemalloc
           libpng
           libtiff
           luajit
           nng
           portaudio
           rtl-sdr
           volk
           (list zstd "lib")))
    (arguments
     (list #:tests? #f)) ; No test suite
    (home-page "https://www.satdump.org/")
    (synopsis "Satellite data processing software")
    (description "SatDump is a generic satellite data processing software.
For example, it can decode the telemetry and images sent by some meteorological
satellites.")
    (license license:gpl3)))

(define-public chirp
  (let ((commit "1219bee0d39ca3778acdf5d7f0a92c1e8208bae9")
        (revision "3"))
    (package
      (name "chirp")
      (version (git-version "0.4.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/kk7ds/chirp")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "19z3f05zppg8w4z4sdich8d173sd87501l0p8l1vn1awgky2q0r8"))))
      (build-system python-build-system)
      (native-inputs
       (list python-mock
             python-mox3
             python-pep8
             python-pytest
             python-pytest-mock
             python-pyyaml
             python-tox))
      (inputs
       (list python-future
             python-importlib-resources
             python-lark-parser
             python-pyserial
             python-requests
             python-six
             python-suds
             python-wxpython
             python-yattag))
      (arguments
       (list ;; FIXME: How to run the tests? The default way crashes.
             #:tests? #f
             #:phases
             #~(modify-phases %standard-phases
                 ;; FIXME: Why does sanity-check phase fail to find lark?
                 (delete 'sanity-check)
                 (add-after 'build 'set-home-for-tests
                   (lambda _
                     (setenv "HOME" "/tmp"))))))
      (synopsis "Cross-radio programming tool")
      (description "Chirp is a cross-radio programming tool.  It supports a
growing list of radios across several manufacturers and allows transferring of
memory contents between them.")
      (home-page "https://chirp.danplanet.com")
      (license license:gpl3+))))

(define-public qdmr
  (package
    (name "qdmr")
    (version "0.12.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/hmatuschek/qdmr")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "08g00xwdqchc21nmacw45s65k8hnk8450yavjb1dx8kmd31kds79"))))
    (build-system cmake-build-system)
    (arguments
     (list #:tests? #f ;no tests
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'patch-paths
                 (lambda _
                   (substitute* "lib/CMakeLists.txt"
                     (("(DESTINATION \")/etc/udev/" _ directive)
                      (string-append directive #$output "/lib/udev/"))))))))
    (inputs (list libusb qtbase-5 qtlocation-5 qtserialport-5 yaml-cpp))
    (native-inputs (list qttools-5))
    (home-page "https://dm3mat.darc.de/qdmr/")
    (synopsis "GUI application and command line tool to program DMR radios")
    (description
     "qdmr is a graphical user interface (GUI) application that allows one to
program several types of DMR radios.  It is comparable to the Customer
Programming Software (CPS) bundled with these radios but aims to be a more
universal tool.

To install the qdmr udev rules, you must extend @code{udev-service-type} with this
package.  E.g.: @code{(udev-rules-service 'qdmr qdmr)}")
    (license license:gpl3+)))
