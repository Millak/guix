;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 Petr Hodina <phodina@protonmail.com>
;;; Copyright © 2023 Steve George <steve@futurile.net>
;;; Copyright © 2023 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2023 Jaeme Sifat <jaeme@runbox.com>
;;; Copyright © 2024 Sergio Pastor Pérez <sergio.pastorperez@outlook.es>
;;; Copyright © 2024 Roman Scherer <roman@burningswell.com>
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

(define-module (gnu packages crates-audio)
  #:use-module (guix build-system cargo)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages crates-apple)
  #:use-module (gnu packages crates-check)
  #:use-module (gnu packages crates-crypto)
  #:use-module (gnu packages crates-graphics)
  #:use-module (gnu packages crates-gtk)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages crates-web)
  #:use-module (gnu packages crates-windows)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio))

;;;
;;; Please: Try to add new module packages in alphabetic order.
;;;

(define-public rust-alsa-0.9
  (package
    (name "rust-alsa")
    (version "0.9.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "alsa" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0hvxc447bsynyhzhmznw6w2kwbid83p712dls4h1x8w3pavp4xgd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags
       (list "--"
             ;; These try to use the audio interface
             "--skip=pcm::drop"
             "--skip=pcm::info_from_default"
             "--skip=pcm::playback_to_default"
             "--skip=pcm::record_from_default"
             "--skip=seq::print_seqs"
             "--skip=seq::seq_loopback"
             "--skip=seq::seq_portsubscribeiter"
             "--skip=seq::seq_subscribe"
             "--skip=src/pcm.rs - pcm (line 6)")
       #:cargo-inputs (("rust-alsa-sys" ,rust-alsa-sys-0.3)
                       ("rust-bitflags" ,rust-bitflags-2)
                       ("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-libc" ,rust-libc-0.2))))
    (inputs (list alsa-lib))
    (native-inputs (list pkg-config))
    (home-page "https://github.com/diwic/alsa-rs")
    (synopsis "Thin and safe wrapper around ALSA")
    (description "A thin and safe wrapper around ALSA.  Provides APIs for many
parts of ALSA including audio playback, audio recording, HCtl API, raw MIDI and
MIDI sequencer.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-alsa-0.8
  (package
    (inherit rust-alsa-0.9)
    (name "rust-alsa")
    (version "0.8.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "alsa" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "02pzlq2q8ml28ikvkvm77bwdqmi22d6ak1qvrc0cr6yjb9adwd6f"))))
    (arguments
     `(#:cargo-test-flags
       (list "--release"
             ;; Not the doc tests.
             "--lib" "--bins" "--tests" "--"
             ;; These try to use the audio interface
             "--skip=pcm::drop"
             "--skip=pcm::info_from_default"
             "--skip=pcm::playback_to_default"
             "--skip=pcm::record_from_default"
             "--skip=seq::print_seqs"
             "--skip=seq::seq_loopback"
             "--skip=seq::seq_portsubscribeiter"
             "--skip=seq::seq_subscribe")
       #:cargo-inputs (("rust-alsa-sys" ,rust-alsa-sys-0.3)
                       ("rust-bitflags" ,rust-bitflags-2)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-nix" ,rust-nix-0.26))))))

(define-public rust-alsa-0.7
  (package
    (inherit rust-alsa-0.8)
    (name "rust-alsa")
    (version "0.7.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "alsa" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0iwbdgb6lr81iji9sr4f91mys24pia5avnkgbkv8kxzhvkc2lmp2"))))
    (arguments
     (list #:cargo-test-flags `(list "--release"
                                     ;; Not the doc tests.
                                     "--lib" "--bins" "--tests"
                                     "--"
                                     ;; These try to use the audio interface
                                     "--skip=pcm::drop"
                                     "--skip=pcm::info_from_default"
                                     "--skip=pcm::playback_to_default"
                                     "--skip=pcm::record_from_default"
                                     "--skip=seq::print_seqs"
                                     "--skip=seq::seq_loopback"
                                     "--skip=seq::seq_portsubscribeiter"
                                     "--skip=seq::seq_subscribe")
           #:cargo-inputs `(("rust-alsa-sys" ,rust-alsa-sys-0.3)
                            ("rust-bitflags" ,rust-bitflags-1)
                            ("rust-libc" ,rust-libc-0.2)
                            ("rust-nix" ,rust-nix-0.24))))))

(define-public rust-alsa-0.6
  (package
    (inherit rust-alsa-0.7)
    (name "rust-alsa")
    (version "0.6.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "alsa" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0szx8finhqbffh08fp3bgh4ywz0b572vcdyh4hwyhrfgw8pza5ar"))))
    (arguments
     (list #:cargo-test-flags `(list "--release"
                                     ;; Not the doc tests.
                                     "--lib" "--bins" "--tests"
                                     "--"
                                     ;; These try to use the audio interface
                                     "--skip=pcm::drop"
                                     "--skip=pcm::info_from_default"
                                     "--skip=pcm::playback_to_default"
                                     "--skip=pcm::record_from_default"
                                     "--skip=seq::print_seqs"
                                     "--skip=seq::seq_loopback"
                                     "--skip=seq::seq_portsubscribeiter"
                                     "--skip=seq::seq_subscribe")
           #:cargo-inputs `(("rust-alsa-sys" ,rust-alsa-sys-0.3)
                            ("rust-bitflags" ,rust-bitflags-1)
                            ("rust-libc" ,rust-libc-0.2)
                            ("rust-nix" ,rust-nix-0.23))))))

(define-public rust-alsa-sys-0.3
  (package
    (name "rust-alsa-sys")
    (version "0.3.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "alsa-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "09qmmnpmlcj23zcgx2xsi4phcgm5i02g9xaf801y7i067mkfx3yv"))))
    (build-system cargo-build-system)
    (arguments
     (list #:tests? #f  ; doc tests fail
           #:cargo-inputs `(("rust-libc" ,rust-libc-0.2)
                            ("rust-pkg-config" ,rust-pkg-config-0.3))))
    (native-inputs (list pkg-config alsa-lib))
    (home-page "https://github.com/diwic/alsa-sys")
    (synopsis "FFI bindings for the ALSA sound API")
    (description
     "FFI bindings for the ALSA sound API.  This package contains
the code to interact with the underlying operating system ALSA interface.")
    (license license:expat)))

(define-public rust-asio-sys-0.2
  (package
    (name "rust-asio-sys")
    (version "0.2.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "asio-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "16lbavksj2aasadyxbdnbrll6a1m8cwl4skbxgbvr1ma2wpwv82c"))))
    (build-system cargo-build-system)
    (arguments
     (list #:skip-build? #t
           #:cargo-inputs `(("rust-bindgen" ,rust-bindgen-0.56)
                            ("rust-cc" ,rust-cc-1)
                            ("rust-num-derive" ,rust-num-derive-0.3)
                            ("rust-num-traits" ,rust-num-traits-0.2)
                            ("rust-once-cell" ,rust-once-cell-1)
                            ("rust-walkdir" ,rust-walkdir-2))))
    (home-page "https://github.com/RustAudio/cpal/")
    (synopsis
     "Low-level interface and binding generation for the Steinberg ASIO SDK")
    (description
     "Low-level interface and binding generation for the Steinberg ASIO SDK.")
    (license license:asl2.0)))

(define-public rust-cpal-0.13
  (package
    (name "rust-cpal")
    (version "0.13.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cpal" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32 "05j11vz8rw19gqqvpd48i7wvm6j77v8fwx5lwhlkckqjllv7h4bl"))
        (snippet
         #~(begin (use-modules (guix build utils))
                  ;; Force cpal-0.13.5 to accept any version of jack, so
                  ;; that other packages like librespot-playback can use
                  ;; the one they want.
                  (substitute* "Cargo.toml.orig"
                    (("(jack = \\{ version = \").*(\", optional.*)" _ jack optional)
                     (string-append jack "*" optional))
                    ;; Remove path for asio-sys, use packaged crate.
                    ((", path =.*,") ","))
                  (copy-file "Cargo.toml.orig" "Cargo.toml")))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-alsa" ,rust-alsa-0.6)
                       ("rust-asio-sys" ,rust-asio-sys-0.2)
                       ("rust-core-foundation-sys" ,rust-core-foundation-sys-0.8)
                       ("rust-coreaudio-rs" ,rust-coreaudio-rs-0.10)
                       ("rust-jack" ,rust-jack-0.8)
                       ("rust-jni" ,rust-jni-0.19)
                       ("rust-js-sys" ,rust-js-sys-0.3)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-mach" ,rust-mach-0.3)
                       ("rust-ndk" ,rust-ndk-0.6)
                       ("rust-ndk-glue" ,rust-ndk-glue-0.6)
                       ("rust-nix" ,rust-nix-0.23)
                       ("rust-num-traits" ,rust-num-traits-0.2)
                       ("rust-oboe" ,rust-oboe-0.4)
                       ("rust-parking-lot" ,rust-parking-lot-0.11)
                       ("rust-stdweb" ,rust-stdweb-0.1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
                       ("rust-web-sys" ,rust-web-sys-0.3)
                       ("rust-winapi" ,rust-winapi-0.3))
       #:cargo-development-inputs (("rust-anyhow" ,rust-anyhow-1)
                                   ("rust-clap" ,rust-clap-3)
                                   ("rust-hound" ,rust-hound-3)
                                   ("rust-ringbuf" ,rust-ringbuf-0.2))))
    (native-inputs (list pkg-config))
    (inputs (list alsa-lib))
    (home-page "https://github.com/rustaudio/cpal")
    (synopsis "Low-level cross-platform audio I/O library in pure Rust")
    (description "Low-level cross-platform audio I/O library in pure Rust.
Supports Linux through either JACK or ALSA.")
    (license license:asl2.0)))

(define-public rust-jack-0.10
  (package
    (name "rust-jack")
    (version "0.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "jack" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0djs3j0icxbzbivhj73vgjrvjw6ncpfak2vyxjcbn4wvl9ajcwnf"))))
    (build-system cargo-build-system)
    (arguments
     (list #:tests? #f
           #:cargo-inputs
           `(("rust-bitflags" ,rust-bitflags-1)
             ("rust-jack-sys" ,rust-jack-sys-0.4)
             ("rust-lazy-static" ,rust-lazy-static-1)
             ("rust-libc" ,rust-libc-0.2)
             ("rust-log" ,rust-log-0.4))
           #:cargo-development-inputs
           `(("rust-crossbeam-channel" ,rust-crossbeam-channel-0.5))))
    (native-inputs (list pkg-config))
    (inputs (list jack-2))
    (home-page "https://github.com/RustAudio/rust-jack")
    (synopsis "Real time audio and midi with JACK")
    (description "Real time audio and midi with JACK.")
    (license license:expat)))

(define-public rust-jack-0.8
  (package
    (inherit rust-jack-0.10)
    (name "rust-jack")
    (version "0.8.4")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "jack" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0lz10s0n2gy128m65pf96is9ip00vfgvnkfja0y9ydmv24pw2ajx"))))
    (arguments
     (list #:tests? #f
           #:cargo-inputs `(("rust-bitflags" ,rust-bitflags-1)
                            ("rust-jack-sys" ,rust-jack-sys-0.2)
                            ("rust-lazy-static" ,rust-lazy-static-1)
                            ("rust-libc" ,rust-libc-0.2)
                            ("rust-log" ,rust-log-0.4)
                            ("rust-crossbeam-channel" ,rust-crossbeam-channel-0.5))))))

(define-public rust-jack-sys-0.4
  (package
    (name "rust-jack-sys")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "jack-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "17vaq4i8q5nx39rjqx9sixqn1xraf1vxs3bmrf618v8nzxchbmz9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f      ; cannot find value `library` in this scope
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-1)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-libloading" ,rust-libloading-0.7)
        ("rust-pkg-config" ,rust-pkg-config-0.3))))
    (native-inputs (list pkg-config))
    (inputs (list jack-2))
    (home-page "https://github.com/RustAudio/rust-jack/tree/main/jack-sys")
    (synopsis "Low-level binding to the JACK audio API")
    (description "Low-level binding to the JACK audio API.")
    (license (list license:expat license:asl2.0))))

(define-public rust-jack-sys-0.2
  (package
    (inherit rust-jack-sys-0.4)
    (name "rust-jack-sys")
    (version "0.2.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "jack-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1h9c9za19nyr1prx77gkia18ia93f73lpyjdiyrvmhhbs79g54bv"))))
    (build-system cargo-build-system)
    (arguments
     (list #:cargo-inputs `(("rust-lazy-static" ,rust-lazy-static-1)
                            ("rust-libc" ,rust-libc-0.2)
                            ("rust-libloading" ,rust-libloading-0.6)
                            ("rust-pkg-config" ,rust-pkg-config-0.3))))))

(define-public rust-lewton-0.10
  (package
    (name "rust-lewton")
    (version "0.10.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "lewton" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0c60fn004awg5c3cvx82d6na2pirf0qdz9w3b93mbcdakbglhyvp"))))
    (build-system cargo-build-system)
    (arguments
     (list #:cargo-inputs
           `(("rust-byteorder" ,rust-byteorder-1)
             ("rust-futures" ,rust-futures-0.1)
             ("rust-ogg" ,rust-ogg-0.8)
             ("rust-tinyvec" ,rust-tinyvec-1)
             ("rust-tokio-io" ,rust-tokio-io-0.1))
           #:cargo-development-inputs
           `(("rust-alto" ,rust-alto-3)
             ("rust-ogg" ,rust-ogg-0.8))))
    (home-page "https://github.com/RustAudio/lewton")
    (synopsis "Pure Rust Vorbis decoder")
    (description "A pure Rust Vorbis decoder.  Vorbis is a free and open
source audio format.")
    (license (list license:expat license:asl2.0))))

(define-public rust-libpulse-binding-2
  (package
    (name "rust-libpulse-binding")
    (version "2.28.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "libpulse-binding" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1zza12f22wf1qs6h71lq1i73aj3kmv3036hqc7qci063vyi5fdgd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-libpulse-sys" ,rust-libpulse-sys-1)
        ("rust-num-derive" ,rust-num-derive-0.3)
        ("rust-num-traits" ,rust-num-traits-0.2)
        ("rust-winapi" ,rust-winapi-0.3))
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'set-HOME
           (lambda _ (setenv "HOME" "/tmp"))))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list pulseaudio))
    (home-page "https://github.com/jnqnfe/pulse-binding-rust")
    (synopsis "Binding for the PulseAudio libpulse library")
    (description
     "This package provides a Rust language binding for the PulseAudio libpulse
library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-libpulse-simple-binding-2
  (package
    (name "rust-libpulse-simple-binding")
    (version "2.28.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "libpulse-simple-binding" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                 "139hiksmxrmj8zcdqvswgjnwl1rivh915vg6cl92asizydl6pz85"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-libpulse-binding" ,rust-libpulse-binding-2)
                       ("rust-libpulse-simple-sys" ,rust-libpulse-simple-sys-1)
                       ("rust-libpulse-sys" ,rust-libpulse-sys-1))
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'set-HOME
           (lambda _ (setenv "HOME" "/tmp"))))))
    (native-inputs (list pkg-config))
    (inputs (list pulseaudio))
    (home-page "https://github.com/jnqnfe/pulse-binding-rust")
    (synopsis "Rust language bindings for PulseAudio's libpulse-simple library")
    (description
     "A Rust language binding for the PulseAudio libpulse-simple library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-libpulse-simple-sys-1
  (package
    (name "rust-libpulse-simple-sys")
    (version "1.21.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "libpulse-simple-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32 "0lj13ibdwf69ghy1zlldxq5vsyxi1h13wqpvvh79z2wx36s16rpa"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-libpulse-sys" ,rust-libpulse-sys-1)
                       ("rust-pkg-config" ,rust-pkg-config-0.3))))
    (native-inputs (list pkg-config))
    (inputs (list pulseaudio))
    (home-page "https://github.com/jnqnfe/pulse-binding-rust")
    (synopsis "FFI indings for PulseAudio's libpulse-simple system library")
    (description
     "FFI bindings for the PulseAudio libpulse-simple system library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-libpulse-sys-1
  (package
    (name "rust-libpulse-sys")
    (version "1.21.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "libpulse-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "16vs0qk6xadckb5qxlrhg0f4jn2zakfd7xih1lk1fb7lzc8f26dw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-libc" ,rust-libc-0.2)
        ("rust-num-derive" ,rust-num-derive-0.3)
        ("rust-num-traits" ,rust-num-traits-0.2)
        ("rust-pkg-config" ,rust-pkg-config-0.3)
        ("rust-winapi" ,rust-winapi-0.3))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list pulseaudio))
    (home-page "https://github.com/jnqnfe/pulse-binding-rust")
    (synopsis "FFI bindings for the PulseAudio")
    (description
     "This package provides FFI bindings for the PulseAudio libpulse system
library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-librespot-audio-0.4
  (package
    (name "rust-librespot-audio")
    (version "0.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "librespot-audio" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "178djijj7fkg5ca5rhk10rvy9gs797gikvackh5qxsp1al9s6xn1"))))
    (build-system cargo-build-system)
    (arguments
     (list #:cargo-inputs
           `(("rust-aes-ctr" ,rust-aes-ctr-0.6)
             ("rust-byteorder" ,rust-byteorder-1)
             ("rust-bytes" ,rust-bytes-1)
             ("rust-futures-util" ,rust-futures-util-0.3)
             ("rust-librespot-core" ,rust-librespot-core-0.4)
             ("rust-log" ,rust-log-0.4)
             ("rust-tempfile" ,rust-tempfile-3)
             ("rust-tokio" ,rust-tokio-1))))
    (home-page "https://github.com/librespot-org/librespot")
    (synopsis "The audio fetching logic for Librespot")
    (description
     "Part of Librespot, an open source client library for Spotify.  This
package contains the audio fetching logic.")
    (license license:expat)))

(define-public rust-librespot-connect-0.4
  (package
    (name "rust-librespot-connect")
    (version "0.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "librespot-connect" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1v6k20173hx27g34d24vkb4a67av7dbr3mfmng64b51y8imgpyjg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-form-urlencoded" ,rust-form-urlencoded-1)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-librespot-core" ,rust-librespot-core-0.4)
                       ("rust-librespot-discovery" ,rust-librespot-discovery-0.4)
                       ("rust-librespot-playback" ,rust-librespot-playback-0.4)
                       ("rust-librespot-protocol" ,rust-librespot-protocol-0.4)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-protobuf" ,rust-protobuf-2)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-stream" ,rust-tokio-stream-0.1))))
    (home-page "https://github.com/librespot-org/librespot")
    (synopsis "Discovery and Spotify Connect logic for Librespot")
    (description
     "Librespot is an open source client library for Spotify.  This package
contains the discovery and Spotify Connect logic.")
    (license license:expat)))

(define-public rust-librespot-core-0.4
  (package
    (name "rust-librespot-core")
    (version "0.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "librespot-core" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0vaxnnlnsx8bmphiikm4kb99795jch0xxifr0azl9rl8b3r4jqq4"))))
    (build-system cargo-build-system)
    (arguments
     (list #:cargo-test-flags
           ``("--release" "--"
              "--skip=test_connection"
              "--skip=test_apresolve"
              "--skip=test_apresolve_port_443")
           #:cargo-inputs
           `(("rust-aes" ,rust-aes-0.6)
             ("rust-base64" ,rust-base64-0.13)
             ("rust-byteorder" ,rust-byteorder-1)
             ("rust-bytes" ,rust-bytes-1)
             ("rust-form-urlencoded" ,rust-form-urlencoded-1)
             ("rust-futures-core" ,rust-futures-core-0.3)
             ("rust-futures-util" ,rust-futures-util-0.3)
             ("rust-hmac" ,rust-hmac-0.11)
             ("rust-http" ,rust-http-0.2)
             ("rust-httparse" ,rust-httparse-1)
             ("rust-hyper" ,rust-hyper-0.14)
             ("rust-hyper-proxy" ,rust-hyper-proxy-0.9)
             ("rust-librespot-protocol" ,rust-librespot-protocol-0.4)
             ("rust-log" ,rust-log-0.4)
             ("rust-num-bigint" ,rust-num-bigint-0.4)
             ("rust-num-integer" ,rust-num-integer-0.1)
             ("rust-num-traits" ,rust-num-traits-0.2)
             ("rust-once-cell" ,rust-once-cell-1)
             ("rust-pbkdf2" ,rust-pbkdf2-0.8)
             ("rust-priority-queue" ,rust-priority-queue-1)
             ("rust-protobuf" ,rust-protobuf-2)
             ("rust-rand" ,rust-rand-0.8)
             ("rust-serde" ,rust-serde-1)
             ("rust-serde-json" ,rust-serde-json-1)
             ("rust-sha-1" ,rust-sha-1-0.10)
             ("rust-shannon" ,rust-shannon-0.2)
             ("rust-thiserror" ,rust-thiserror-1)
             ("rust-tokio" ,rust-tokio-1)
             ("rust-tokio-stream" ,rust-tokio-stream-0.1)
             ("rust-tokio-util" ,rust-tokio-util-0.7)
             ("rust-url" ,rust-url-2)
             ("rust-uuid" ,rust-uuid-1)
             ("rust-vergen" ,rust-vergen-3))
           #:cargo-development-inputs
           `(("rust-env-logger" ,rust-env-logger-0.9)
             ("rust-tokio" ,rust-tokio-1))))
    (home-page "https://github.com/librespot-org/librespot")
    (synopsis "The core functionality provided by librespot")
    (description
     "Part of Librespot, an open source client library for
Spotify.  This package contains core functionality, such as authentication,
channel and session.")
    (license license:expat)))

(define-public rust-librespot-discovery-0.4
  (package
    (name "rust-librespot-discovery")
    (version "0.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "librespot-discovery" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "01igbv0xf3vj046jvblbr09cgmv25mlfajyb2903cl31iz8pga1a"))))
    (build-system cargo-build-system)
    (arguments
     (list #:cargo-inputs
           `(("rust-aes-ctr" ,rust-aes-ctr-0.6)
             ("rust-base64" ,rust-base64-0.13)
             ("rust-form-urlencoded" ,rust-form-urlencoded-1)
             ("rust-futures-core" ,rust-futures-core-0.3)
             ("rust-hmac" ,rust-hmac-0.11)
             ("rust-hyper" ,rust-hyper-0.14)
             ("rust-libmdns" ,rust-libmdns-0.7)
             ("rust-librespot-core" ,rust-librespot-core-0.4)
             ("rust-log" ,rust-log-0.4)
             ("rust-rand" ,rust-rand-0.8)
             ("rust-serde-json" ,rust-serde-json-1)
             ("rust-sha-1" ,rust-sha-1-0.9)
             ("rust-thiserror" ,rust-thiserror-1)
             ("rust-tokio" ,rust-tokio-1)
             ("rust-dns-sd" ,rust-dns-sd-0.1))
           #:cargo-development-inputs
           `(("rust-futures" ,rust-futures-0.3)
             ("rust-hex" ,rust-hex-0.4)
             ("rust-simple-logger" ,rust-simple-logger-2)
             ("rust-tokio" ,rust-tokio-1))))
    (home-page "https://github.com/librespot-org/librespot")
    (synopsis "The discovery logic of Librespot")
    (description "Part of Librespot, an open source client library for
Spotify.  This package contains the discovery logic.")
    (license license:expat)))

(define-public rust-librespot-metadata-0.4
  (package
    (name "rust-librespot-metadata")
    (version "0.4.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "librespot-metadata" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "07626b84cghd3jabdvyqhn1v0lax9p1hhz6ldw2r4l6brcgkd03b"))))
    (build-system cargo-build-system)
    (arguments
     (list #:cargo-inputs
           `(("rust-async-trait" ,rust-async-trait-0.1)
             ("rust-byteorder" ,rust-byteorder-1)
             ("rust-librespot-core" ,rust-librespot-core-0.4)
             ("rust-librespot-protocol" ,rust-librespot-protocol-0.4)
             ("rust-log" ,rust-log-0.4)
             ("rust-protobuf" ,rust-protobuf-2))))
    (home-page "https://github.com/librespot-org/librespot")
    (synopsis "The metadata elements of Librespot")
    (description "Part of Librespot, an open source client library for
Spotify.  This package contains the metadata logic.")
    (license license:expat)))

(define-public rust-librespot-playback-0.4
  (package
    (name "rust-librespot-playback")
    (version "0.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "librespot-playback" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1dygnzldvkv1qpagr9nl62hmqh0xfcf4lsva37j0xxy7pjws142i"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-alsa" ,rust-alsa-0.6)
        ("rust-byteorder" ,rust-byteorder-1)
        ("rust-cpal" ,rust-cpal-0.13)
        ("rust-futures-executor" ,rust-futures-executor-0.3)
        ("rust-futures-util" ,rust-futures-util-0.3)
        ("rust-glib" ,rust-glib-0.15)
        ("rust-gstreamer" ,rust-gstreamer-0.18)
        ("rust-gstreamer-app" ,rust-gstreamer-app-0.18)
        ("rust-gstreamer-audio" ,rust-gstreamer-audio-0.18)
        ("rust-jack" ,rust-jack-0.10)
        ("rust-lewton" ,rust-lewton-0.10)
        ("rust-libpulse-binding" ,rust-libpulse-binding-2)
        ("rust-libpulse-simple-binding" ,rust-libpulse-simple-binding-2)
        ("rust-librespot-audio" ,rust-librespot-audio-0.4)
        ("rust-librespot-core" ,rust-librespot-core-0.4)
        ("rust-librespot-metadata" ,rust-librespot-metadata-0.4)
        ("rust-log" ,rust-log-0.4)
        ("rust-ogg" ,rust-ogg-0.8)
        ("rust-parking-lot" ,rust-parking-lot-0.12)
        ("rust-portaudio-rs" ,rust-portaudio-rs-0.3)
        ("rust-rand" ,rust-rand-0.8)
        ("rust-rand-distr" ,rust-rand-distr-0.4)
        ("rust-rodio" ,rust-rodio-0.15)
        ("rust-sdl2" ,rust-sdl2-0.35)
        ("rust-shell-words" ,rust-shell-words-1)
        ("rust-thiserror" ,rust-thiserror-1)
        ("rust-tokio" ,rust-tokio-1)
        ("rust-zerocopy" ,rust-zerocopy-0.6))))
    (home-page "https://github.com/librespot-org/librespot")
    (synopsis "Audio playback for Librespot")
    (description "Audio playback for Librespot, an open source client
library for Spotify.")
    (license license:expat)))

(define-public rust-librespot-protocol-0.4
  (package
    (name "rust-librespot-protocol")
    (version "0.4.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "librespot-protocol" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "17xkvhlxfkjh1z79pvq22nrxi99hcxnzafg0pdkymh3a3733lvax"))))
    (build-system cargo-build-system)
    (arguments
     (list
       #:cargo-inputs
       `(("rust-protobuf" ,rust-protobuf-2)
         ("rust-glob" ,rust-glob-0.3)
         ("rust-protobuf-codegen-pure" ,rust-protobuf-codegen-pure-2))))
    (home-page "https://github.com/librespot-org/librespot")
    (synopsis "The protobuf logic for communicating with Spotify servers")
    (description "Part of Librespot, an open source, Spotify client library.
This package contains the protobuf logic.")
    (license license:expat)))

(define-public rust-lv2-0.6
  (package
    (name "rust-lv2")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "lv2" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1xh4hjfh2w5rhzbk0g9845k25f6fxrv7xqpkr09p0x57b200qc41"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-lv2-atom" ,rust-lv2-atom-2)
                       ("rust-lv2-core" ,rust-lv2-core-3)
                       ("rust-lv2-midi" ,rust-lv2-midi-1)
                       ("rust-lv2-state" ,rust-lv2-state-2)
                       ("rust-lv2-sys" ,rust-lv2-sys-2)
                       ("rust-lv2-time" ,rust-lv2-time-0.1)
                       ("rust-lv2-units" ,rust-lv2-units-0.1)
                       ("rust-lv2-urid" ,rust-lv2-urid-2)
                       ("rust-lv2-worker" ,rust-lv2-worker-0.1)
                       ("rust-urid" ,rust-urid-0.1))))
    (home-page "https://github.com/RustAudio/rust-lv2")
    (synopsis "Safe, fast, and ergonomic framework to create LV2 plugins in Rust")
    (description "This package provides a safe, fast, and ergonomic framework
to create LV2 plugins in Rust.")
    (license (list license:expat license:asl2.0))))

(define-public rust-lv2-atom-2
  (package
    (name "rust-lv2-atom")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "lv2-atom" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0wd9rgsn8sag8wyhjccmnn82gx4w1yyiav52nyvk579l21xlw6wm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-lv2-core" ,rust-lv2-core-3)
                       ("rust-lv2-sys" ,rust-lv2-sys-2)
                       ("rust-lv2-units" ,rust-lv2-units-0.1)
                       ("rust-urid" ,rust-urid-0.1))
       #:cargo-development-inputs (("rust-lv2-urid" ,rust-lv2-urid-2))))
    (home-page "https://github.com/RustAudio/rust-lv2")
    (synopsis "Rust LV2 Atom handling library")
    (description "This package provides a Rust LV2 Atom handling library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-lv2-core-3
  (package
    (name "rust-lv2-core")
    (version "3.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "lv2-core" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1pj9l15zwqwj2h83f3xfpwxsj70vvhkw52gyzkljafvrbx1h00fm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-lv2-core-derive" ,rust-lv2-core-derive-2)
                       ("rust-lv2-sys" ,rust-lv2-sys-2)
                       ("rust-urid" ,rust-urid-0.1))))
    (home-page "https://github.com/RustAudio/rust-lv2")
    (synopsis "Rust LV2 core library")
    (description "This package provides the Rust LV2 core library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-lv2-core-derive-2
  (package
    (name "rust-lv2-core-derive")
    (version "2.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "lv2-core-derive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "12w3l41jzargrcywz13hbmaazfw4ix2sljl3601h6jfbdrw8zybv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/RustAudio/rust-lv2")
    (synopsis "Procedural macros for lv2-core")
    (description "This package provides Procedural macros for lv2-core.")
    (license (list license:expat license:asl2.0))))

(define-public rust-lv2-midi-1
  (package
    (name "rust-lv2-midi")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "lv2-midi" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0x0glbrfri1glgcrmvc6i1jfv6azhpqvp4ibk5cihsq3s2yfc8xd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f      ; use of undeclared crate or module `wmidi`
       #:cargo-inputs (("rust-lv2-atom" ,rust-lv2-atom-2)
                       ("rust-lv2-sys" ,rust-lv2-sys-2)
                       ("rust-urid" ,rust-urid-0.1)
                       ("rust-wmidi" ,rust-wmidi-3))
       #:cargo-development-inputs (("rust-lv2-core" ,rust-lv2-core-3)
                                   ("rust-lv2-units" ,rust-lv2-units-0.1))))
    (home-page "https://github.com/RustAudio/rust-lv2")
    (synopsis "Rust LV2 MIDI processing library")
    (description "This package provides a Rust LV2 MIDI processing library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-lv2-state-2
  (package
    (name "rust-lv2-state")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "lv2-state" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0nm0fc7cb4rkmfsvvr4xbac4qf0j7wl2gws3qrcflx057i2lpsb5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-lv2-atom" ,rust-lv2-atom-2)
                       ("rust-lv2-core" ,rust-lv2-core-3)
                       ("rust-lv2-sys" ,rust-lv2-sys-2)
                       ("rust-urid" ,rust-urid-0.1))
       #:cargo-development-inputs (("rust-lv2-urid" ,rust-lv2-urid-2)
                                   ("rust-mktemp" ,rust-mktemp-0.4))))
    (home-page "https://github.com/RustAudio/rust-lv2")
    (synopsis "Rust LV2 state handling library")
    (description "This package provides a Rust LV2 state handling library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-lv2-sys-2
  (package
    (name "rust-lv2-sys")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "lv2-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0c4f59mrjyy0z0wf033wp648df0sc6zirrcd6kndqj9nvvkzkl4x"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/RustAudio/rust-lv2")
    (synopsis "Rust LV2 C header bindings")
    (description "This package provides Rust LV2 C header bindings.")
    (license (list license:expat license:asl2.0))))

(define-public rust-lv2-time-0.1
  (package
    (name "rust-lv2-time")
    (version "0.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "lv2-time" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0wznk17vvn5dph6r47vjwmf7g98pb6ij2fdhizdk95sf2qvkf82c"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-lv2-sys" ,rust-lv2-sys-2)
                       ("rust-urid" ,rust-urid-0.1))))
    (home-page "https://github.com/RustAudio/rust-lv2")
    (synopsis "Rust LV2 wrapper for LV2 time types")
    (description "This package provides a Rust LV2 wrapper for LV2 time types.")
    (license (list license:expat license:asl2.0))))

(define-public rust-lv2-units-0.1
  (package
    (name "rust-lv2-units")
    (version "0.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "lv2-units" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0fdamp3hxdr36hqi1j6y01rz1x17if1ibzr7rr4nrabidw74gf82"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-lv2-sys" ,rust-lv2-sys-2)
                       ("rust-urid" ,rust-urid-0.1))))
    (home-page "https://github.com/RustAudio/rust-lv2")
    (synopsis "Rust LV2 wrapper of LV2 unit types")
    (description "This package provides a Rust LV2 wrapper of LV2 unit types.")
    (license (list license:expat license:asl2.0))))

(define-public rust-lv2-urid-2
  (package
    (name "rust-lv2-urid")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "lv2-urid" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0s2fcb0nyn54ml6azkbhnnxghy898x1q5vs5qgdznrhy9m20624c"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-lv2-core" ,rust-lv2-core-3)
                       ("rust-lv2-sys" ,rust-lv2-sys-2)
                       ("rust-urid" ,rust-urid-0.1))))
    (home-page "https://github.com/RustAudio/rust-lv2")
    (synopsis "Rust LV2 URID handling library")
    (description "This package provides a LV2 URID handling library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-lv2-worker-0.1
  (package
    (name "rust-lv2-worker")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "lv2-worker" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "14crsrnjyarra9ipma6lhaj4gpfadvippzr134nkn0z3y30ip4fj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f      ; Tests panic
       #:cargo-inputs (("rust-lv2-core" ,rust-lv2-core-3)
                       ("rust-lv2-sys" ,rust-lv2-sys-2)
                       ("rust-urid" ,rust-urid-0.1))))
    (home-page "https://github.com/RustAudio/rust-lv2")
    (synopsis "Rust LV2 work offloading library")
    (description "This package provides a Rust LV2 work offloading library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-minimp3-0.5
  (package
    (name "rust-minimp3")
    (version "0.5.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "minimp3" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0wj3nzj1swnvwsk3a4a3hkfj1d21jsi7babi40wlrxzbbzvkhm4q"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f  ; no method named `next_frame_future` found for struct `Decoder`
       #:cargo-inputs (("rust-minimp3-sys" ,rust-minimp3-sys-0.3)
                       ("rust-slice-deque" ,rust-slice-deque-0.3)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-tokio" ,rust-tokio-1))
       #:cargo-development-inputs (("rust-futures" ,rust-futures-0.3)
                                   ("rust-tokio" ,rust-tokio-1))))
    (home-page "https://github.com/germangb/minimp3-rs")
    (synopsis "Rust bindings for the minimp3 library")
    (description "Rust bindings for the minimp3 library.")
    (license license:expat)))

(define-public rust-minimp3-sys-0.3
  (package
    (name "rust-minimp3-sys")
    (version "0.3.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "minimp3-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "144vmf3s89kad0smjprzigcp2c9r5dm95n4ydilrbp399irp6772"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f      ; Not all files included.
       #:cargo-inputs (("rust-cc" ,rust-cc-1))))
    (native-inputs (list pkg-config))
    (inputs (list minimp3))
    (home-page "https://github.com/germangb/minimp3-rs")
    (synopsis "Rust bindings for the minimp3 library")
    (description "Rust bindings for the minimp3 library.")
    (license license:expat)))

(define-public rust-oboe-0.4
  (package
    (name "rust-oboe")
    (version "0.4.6")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "oboe" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1hd5626s8qkpgrl2alwz73i8rh1rzifbxj6pxz7zp82gicskrxi7"))))
    (build-system cargo-build-system)
    (arguments
     (list #:skip-build? #t ; requires Android libs
           #:cargo-inputs `(("rust-jni" ,rust-jni-0.19)
                            ("rust-ndk" ,rust-ndk-0.6)
                            ("rust-ndk-context" ,rust-ndk-context-0.1)
                            ("rust-num-derive" ,rust-num-derive-0.3)
                            ("rust-num-traits" ,rust-num-traits-0.2)
                            ("rust-oboe-sys" ,rust-oboe-sys-0.4))))
    (home-page "https://github.com/katyo/oboe-rs")
    (synopsis
     "Safe interface for oboe an android library for low latency audio IO")
    (description
     "Safe interface for oboe an android library for low latency audio IO.")
    (license license:asl2.0)))

(define-public rust-oboe-sys-0.4
  (package
    (name "rust-oboe-sys")
    (version "0.4.5")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "oboe-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1gcl494yy880h2gfgsbdd32g2h0s1n94v58j5hil9mrf6yvsnw1k"))))
    (build-system cargo-build-system)
    (arguments
     (list #:skip-build? #t ; requires Android libs
           #:cargo-inputs `(("rust-bindgen" ,rust-bindgen-0.59)
                            ("rust-cc" ,rust-cc-1)
                            ("rust-fetch-unroll" ,rust-fetch-unroll-0.3))))
    (home-page "https://github.com/katyo/oboe-rs")
    (synopsis
     "Unsafe bindings for oboe an android library for low latency audio IO")
    (description
     "Unsafe bindings for oboe an android library for low latency audio IO.")
    (license license:asl2.0)))

(define-public rust-ogg-0.8
  (package
    (name "rust-ogg")
    (version "0.8.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "ogg" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0vjxmqcv9252aj8byy70iy2krqfjknfcxg11lcyikj11pzlb8lb9"))))
    (build-system cargo-build-system)
    (arguments
     (list #:cargo-inputs
           `(("rust-byteorder" ,rust-byteorder-1)
             ("rust-bytes" ,rust-bytes-0.4)
             ("rust-futures" ,rust-futures-0.1)
             ("rust-tokio-io" ,rust-tokio-io-0.1))
           #:cargo-development-inputs
           `(("rust-rand" ,rust-rand-0.3))))
    (home-page "https://github.com/RustAudio/ogg")
    (synopsis "Ogg container decoder and encoder written in pure Rust")
    (description "An Ogg decoder and encoder.  Implements the xiph.org Ogg
spec in pure Rust.")
    (license license:expat)))

(define-public rust-pipewire-0.7
  (package
    (name "rust-pipewire")
    (version "0.7.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pipewire" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1sg9cbvhp0s07a337zwli0xm40f8wkvm06d72nsr1s35vp40kl52"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-anyhow" ,rust-anyhow-1)
                       ("rust-bitflags" ,rust-bitflags-2)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-libspa" ,rust-libspa-0.7)
                       ("rust-libspa-sys" ,rust-libspa-sys-0.7)
                       ("rust-nix" ,rust-nix-0.26)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-pipewire-sys" ,rust-pipewire-sys-0.7)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list pipewire clang))
    (home-page "https://pipewire.org")
    (synopsis "Rust bindings for PipeWire")
    (description "This package provides Rust bindings for @code{PipeWire}.")
    (license license:expat)))

(define-public rust-pipewire-sys-0.7
  (package
    (name "rust-pipewire-sys")
    (version "0.7.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pipewire-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0r4z0farzflycgfp6x7z65h57np4l1qnpj4r8z5lcwkkgd70h349"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bindgen" ,rust-bindgen-0.66)
                       ("rust-libspa-sys" ,rust-libspa-sys-0.7)
                       ("rust-system-deps" ,rust-system-deps-6))))
    (native-inputs
     (list pkg-config clang))
    (inputs
     (list pipewire))
    (home-page "https://pipewire.org")
    (synopsis "Rust FFI bindings for PipeWire")
    (description
     "This package provides Rust FFI bindings for @code{PipeWire}.")
    (license license:expat)))

(define-public rust-portaudio-rs-0.3
  (package
    (name "rust-portaudio-rs")
    (version "0.3.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "portaudio-rs" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0qnmc7amk0fzbcs985ixv0k4955f0fmpkhrl9ps9pk3cz7pvbdnd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bitflags" ,rust-bitflags-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-portaudio-sys" ,rust-portaudio-sys-0.1))))
    (native-inputs (list pkg-config))
    (inputs (list portaudio alsa-lib))
    (home-page "https://github.com/RustAudio/rust-portaudio")
    (synopsis "Rust bindings for PortAudio a cross-platfomr audio library")
    (description "Rusting bindings for PortAudio an open source, cross-platform
audio I/O library.")
    (license license:expat)))

(define-public rust-portaudio-sys-0.1
  (package
    (name "rust-portaudio-sys")
    (version "0.1.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "portaudio-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1xdpywirpr1kqkbak7hnny62gmsc93qgc3ij3j2zskrvjpxa952i"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-libc" ,rust-libc-0.2)
                       ("rust-pkg-config" ,rust-pkg-config-0.3))))
    (native-inputs (list pkg-config))
    (inputs (list portaudio alsa-lib))
    (home-page "https://github.com/RustAudio/rust-portaudio")
    (synopsis "Bindings for PortAudio a cross-platform audio library")
    (description "Bindings for PortAudio an open source, cross-platform audio
I/O library.")
    (license license:expat)))

(define-public rust-rodio-0.15
  (package
    (name "rust-rodio")
    (version "0.15.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rodio" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "07kkrx0hxfcqgkpg0lrh9355bj1rl0k65nwsk3qwdri6yvlkj2gc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-claxon" ,rust-claxon-0.4)
                       ("rust-cpal" ,rust-cpal-0.13)
                       ("rust-hound" ,rust-hound-3)
                       ("rust-lewton" ,rust-lewton-0.10)
                       ("rust-minimp3" ,rust-minimp3-0.5)
                       ("rust-symphonia" ,rust-symphonia-0.4))
       #:cargo-development-inputs (("rust-quickcheck" ,rust-quickcheck-0.9))))
    (native-inputs (list pkg-config))
    (inputs (list alsa-lib))
    (home-page "https://github.com/RustAudio/rodio")
    (synopsis "Pure Rust audio playback library")
    (description "Audio playback library written in pure Rust that supports
many formats including AAC, FLAC, MP3, MP4 and WAV.")
    (license (list license:expat license:asl2.0))))

(define-public rust-symphonia-0.4
  (package
    (name "rust-symphonia")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "symphonia" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1dx4v5libbksi4rd6b9290nci3h8xqyakymhxd72yybyl25g7rd7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-symphonia-bundle-flac" ,rust-symphonia-bundle-flac-0.4)
                       ("rust-symphonia-bundle-mp3" ,rust-symphonia-bundle-mp3-0.4)
                       ("rust-symphonia-codec-aac" ,rust-symphonia-codec-aac-0.4)
                       ("rust-symphonia-codec-pcm" ,rust-symphonia-codec-pcm-0.4)
                       ("rust-symphonia-codec-vorbis" ,rust-symphonia-codec-vorbis-0.4)
                       ("rust-symphonia-core" ,rust-symphonia-core-0.4)
                       ("rust-symphonia-format-isomp4" ,rust-symphonia-format-isomp4-0.4)
                       ("rust-symphonia-format-ogg" ,rust-symphonia-format-ogg-0.4)
                       ("rust-symphonia-format-wav" ,rust-symphonia-format-wav-0.4)
                       ("rust-symphonia-metadata" ,rust-symphonia-metadata-0.4))))
    (home-page "https://github.com/pdeljanov/Symphonia")
    (synopsis
     "Symphonia is a pure Rust media container and audio decoding library")
    (description
     "Symphonia is a pure Rust media container and audio decoding library
that supports a wide range of audio formats.")
    (license license:mpl2.0)))

(define-public rust-symphonia-bundle-flac-0.4
  (package
    (name "rust-symphonia-bundle-flac")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "symphonia-bundle-flac" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "00jxn9izfg1g07srhgglpqgadmzwsr88sqnnxw3mskpvyl958vhi"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-log" ,rust-log-0.4)
                       ("rust-symphonia-core" ,rust-symphonia-core-0.4)
                       ("rust-symphonia-metadata" ,rust-symphonia-metadata-0.4)
                       ("rust-symphonia-utils-xiph" ,rust-symphonia-utils-xiph-0.4))))
    (home-page "https://github.com/pdeljanov/Symphonia")
    (synopsis "FLAC demuxer and decoder for the Symphonia library")
    (description
     "Symphonia is a pure Rust audio decoding and media demuxing library
supporting a wide range of audio formats.  This package contains a FLAC demuxer
and decoder.")
    (license license:mpl2.0)))

(define-public rust-symphonia-bundle-mp3-0.4
  (package
    (name "rust-symphonia-bundle-mp3")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "symphonia-bundle-mp3" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "14074njhgrcgh2p5iryrd68mgdzcxf9v7p8xfm8ldkhylv29fkgc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bitflags" ,rust-bitflags-1)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-symphonia-core" ,rust-symphonia-core-0.4)
                       ("rust-symphonia-metadata" ,rust-symphonia-metadata-0.4))))
    (home-page "https://github.com/pdeljanov/Symphonia")
    (synopsis "MP1, MP2, and MP3 demuxers and decoders written in pure Rust")
    (description
     "Symphonia is a pure Rust audio decoding and media demuxing library
supporting a wide range of aduio formats.  This package contains support for
MP1, MP2 and MP3.")
    (license license:mpl2.0)))

(define-public rust-symphonia-codec-aac-0.4
  (package
    (name "rust-symphonia-codec-aac")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "symphonia-codec-aac" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "13smaxgb1jadl4jyay7hixqgwaiqrjvsvmzdvlbdzdxrgsrplgdx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-symphonia-core" ,rust-symphonia-core-0.4))))
    (home-page "https://github.com/pdeljanov/Symphonia")
    (synopsis "Pure Rust AAC decoder from Symphonia")
    (description
     "Symphonia is a pure Rust audio decoding and media demuxing library
that supports a wide range of audio formats.  This package contains an AAC
decoder.")
    (license license:mpl2.0)))

(define-public rust-symphonia-codec-pcm-0.4
  (package
    (name "rust-symphonia-codec-pcm")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "symphonia-codec-pcm" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1is49qjnfy541zpgzz498hnpz0nsq7i4nfky2133b6aqhxrm87ds"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-log" ,rust-log-0.4)
                       ("rust-symphonia-core" ,rust-symphonia-core-0.4))))
    (home-page "https://github.com/pdeljanov/Symphonia")
    (synopsis "Pure Rust PCM audio decoder, part of Symphonia")
    (description
     "Symphonia is a pure Rust audio decoding and media demuxing library
that supports a wide range of audio formats.  This package contains a
@acronym{PCM, Pulse-Code Modulation} audio decoder.")
    (license license:mpl2.0)))

(define-public rust-symphonia-codec-vorbis-0.4
  (package
    (name "rust-symphonia-codec-vorbis")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "symphonia-codec-vorbis" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1yj1si92fnnzdfkw27cq324h6y1s958s8r2hl0szpvvqh1sdd7m2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-log" ,rust-log-0.4)
                       ("rust-symphonia-core" ,rust-symphonia-core-0.4)
                       ("rust-symphonia-utils-xiph" ,rust-symphonia-utils-xiph-0.4))))
    (home-page "https://github.com/pdeljanov/Symphonia")
    (synopsis "Pure Rust Vorbis decoder, part of the Symphonia project")
    (description
     "Symphonia is a pure Rust audio decoding and media demuxing library
that supports a wide range of audio formats.  This package is a Vorbis
decoder.")
    (license license:mpl2.0)))

(define-public rust-symphonia-core-0.4
  (package
    (name "rust-symphonia-core")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "symphonia-core" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1j84q4a9p9qa23976spxap9s6ns3fm6fzrfz65n6cjhgpsbmw4zs"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-arrayvec" ,rust-arrayvec-0.7)
                       ("rust-bitflags" ,rust-bitflags-1)
                       ("rust-bytemuck" ,rust-bytemuck-1)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-log" ,rust-log-0.4))))
    (home-page "https://github.com/pdeljanov/Symphonia")
    (synopsis "Shared elements for Symphonia a Rust audio library")
    (description
     "Symphonia is a pure Rust audio decoding and media demuxing
library supporting a wide range of audio formats.  This package contains
shared structs, traits, and features.")
    (license license:mpl2.0)))

(define-public rust-symphonia-format-isomp4-0.4
  (package
    (name "rust-symphonia-format-isomp4")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "symphonia-format-isomp4" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1dap5yh286j74sybjsam378v1jxkpdl3hvvm81sipv7725vkmvpy"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-encoding-rs" ,rust-encoding-rs-0.8)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-symphonia-core" ,rust-symphonia-core-0.4)
                       ("rust-symphonia-metadata" ,rust-symphonia-metadata-0.4))))
    (home-page "https://github.com/pdeljanov/Symphonia")
    (synopsis "Rust ISO/MP4 demuxer from the Symphonia project")
    (description
     "Symphonia is a pure Rust audio decoding and media demuxing library
that supports a wide range of audio formats.  This package contains an
ISO/MP4 demuxer.")
    (license license:mpl2.0)))

(define-public rust-symphonia-format-ogg-0.4
  (package
    (name "rust-symphonia-format-ogg")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "symphonia-format-ogg" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "06d5327m4yk8a6yq7zzyiv2sbkwnjq28dz9cagndz6m7i1r3bcnp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-log" ,rust-log-0.4)
                       ("rust-symphonia-core" ,rust-symphonia-core-0.4)
                       ("rust-symphonia-metadata" ,rust-symphonia-metadata-0.4)
                       ("rust-symphonia-utils-xiph" ,rust-symphonia-utils-xiph-0.4))))
    (home-page "https://github.com/pdeljanov/Symphonia")
    (synopsis "Pure Rust OGG demuxer, part of Symphonia")
    (description
     "Symphonia is a pure Rust decoding and media demuxing library that
supports a wide range of audio formats.  This package is an OGG demuxer.")
    (license license:mpl2.0)))

(define-public rust-symphonia-format-wav-0.4
  (package
    (name "rust-symphonia-format-wav")
    (version "0.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "symphonia-format-wav" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1b8x213s44xis4pb1ibnqr1a20hsxf3phm527dvadpi0nkjsb7vd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-log" ,rust-log-0.4)
                       ("rust-symphonia-core" ,rust-symphonia-core-0.4)
                       ("rust-symphonia-metadata" ,rust-symphonia-metadata-0.4))))
    (home-page "https://github.com/pdeljanov/Symphonia")
    (synopsis "Rust WAV demuxer from the Symphonia project")
    (description
     "Symphonia is a pure Rust decoding and media demuxing library that
supports a wide range of audio formats.  This package is a WAV demuxer.")
    (license license:mpl2.0)))

(define-public rust-symphonia-metadata-0.4
  (package
    (name "rust-symphonia-metadata")
    (version "0.4.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "symphonia-metadata" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "06lvwy24kirc84r6d23ncad544525fsb6gna0plqz3d1mffmjq2j"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-encoding-rs" ,rust-encoding-rs-0.8)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-symphonia-core" ,rust-symphonia-core-0.4))))
    (home-page "https://github.com/pdeljanov/Symphonia")
    (synopsis "Multimedia tag and metadata readers for the Symphonia library")
    (description "Symphonia is a pure Rust audio decoding and media demuxing
library supporting a wide range of audio formats.  This package contains
multimedia tag and metadata readers.")
    (license license:mpl2.0)))

(define-public rust-symphonia-utils-xiph-0.4
  (package
    (name "rust-symphonia-utils-xiph")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "symphonia-utils-xiph" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1sg1y7s607rk1akrrzyhdsqimiwwaw440jzr1cp89zs8d5n04dva"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-symphonia-core" ,rust-symphonia-core-0.4)
                       ("rust-symphonia-metadata" ,rust-symphonia-metadata-0.4))))
    (home-page "https://github.com/pdeljanov/Symphonia")
    (synopsis "Xiph codecs and formats for Rust's Symphonia")
    (description
     "Symphonia is a pure Rust audio decoding and media demuxing
library that supports a wide range of audio formats.  This package contains Xiph
codecs and formats.")
    (license license:mpl2.0)))

(define-public rust-urid-0.1
  (package
    (name "rust-urid")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "urid" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "195672gs136vczn1r4hkjg5vfa7vdzr26bzv6lwhk0z7cvbvaa38"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-urid-derive" ,rust-urid-derive-0.1))))
    (home-page "https://github.com/RustAudio/rust-lv2")
    (synopsis "Library for idiomatic URID support")
    (description "This package provides Library for idiomatic URID support.")
    (license (list license:expat license:asl2.0))))

(define-public rust-urid-derive-0.1
  (package
    (name "rust-urid-derive")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "urid-derive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0i1nf0sgq4ai051h17s9msaavl3jfzdmdlsy8455pr88y0pfx7l1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/RustAudio/rust-lv2")
    (synopsis "Procedural macros for urid")
    (description "This package provides procedural macros for urid.")
    (license (list license:expat license:asl2.0))))

(define-public rust-wmidi-3
  (package
    (name "rust-wmidi")
    (version "3.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wmidi" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1kxnbs18nmpzm2hfwaaa5h2s77cmk5w53srzxqmrqlkdpdcrjafa"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/RustAudio/wmidi")
    (synopsis "Midi parsing library")
    (description "This package provides a Midi parsing library.")
    (license license:expat)))

;;;
;;; Avoid adding new packages to the end of this file. To reduce the chances
;;; of a merge conflict, place them above by existing packages with similar
;;; functionality or similar names.
;;;
