;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2022 Maya Tomasek <maya.tomasek@disroot.org>
;;; Copyright © 2023 Ekaitz Zarraga <ekaitz@elenq.tech>
;;; Copyright © 2023 Felix Lechner <felix.lechner@lease-up.com>
;;; Copyright © 2024 Justin Veilleux <terramorpha@cock.li>
;;; Copyright © 2025 Ashvith Shetty <ashvithshetty0010@zohomail.in>
;;; Copyright © 2025 Meredith Oleander <mereditholeander@gmail.com>
;;; Copyright © 2025 Raven Hallsby <karl@hallsby.com>
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

(define-module (gnu packages zig-xyz)
  #:use-module (ice-9 match)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system zig)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages man)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages window-management)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages zig))

(define-public poop
  (package
    (name "poop")
    (version "0.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/andrewrk/poop")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1m296ly3159gkbf33zziqarx2wxh0lmkydky440inbn46kyr3fnf"))))
        (build-system zig-build-system)
    (arguments
     (list #:install-source? #f
           #:tests? #f ; There are no tests.
           #:zig-release-type "safe"))
    (home-page "https://github.com/andrewrk/poop")
    (synopsis "Performance Optimizer Observation Platform")
    (description
     "POOP (Performance Optimizer Observation Platform) is a command line tool
that uses Linux's @code{perf_event_open} functionality to compare the
performance of multiple commands with a colorful terminal user interface.")
    (license license:expat)))

(define-public river
  (package
    (name "river")
    (version "0.3.12")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://codeberg.org/river/river")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1jh374v6c0mfrppj0fgz177qxi3ihcisq158ismqnhagnbdk3z2w"))))
    (build-system zig-build-system)
    (arguments
     (list #:zig zig-0.15
           #:install-source? #f
           #:zig-release-type "safe"
           #:zig-build-flags
           #~(list "-Dpie" "-Dxwayland")
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'fix-path
                 (lambda _
                   (substitute* "build.zig"
                     (("/bin/sh") (which "sh")))))
               (add-after 'unpack 'prepare-build.zig.zon
                 (lambda _
                   (substitute* "build.zig.zon"
                     (("\\.pixman") ".@\"zig-pixman\"")
                     (("\\.wayland") ".@\"zig-wayland\"")
                     (("\\.wlroots") ".@\"zig-wlroots\"")
                     (("\\.xkbcommon") ".@\"zig-xkbcommon\""))))
               (add-before 'build 'revert-build.zig.zon
                 (lambda _
                   (substitute* "build.zig.zon"
                     (("\\.@\"zig-pixman\"") ".pixman")
                     (("\\.@\"zig-wayland\"") ".wayland")
                     (("\\.@\"zig-wlroots\"") ".wlroots")
                     (("\\.@\"zig-xkbcommon\"") ".xkbcommon"))))
               (add-after 'install 'install-wayland-session
                 (lambda _
                   (let ((wayland-sessions
                          (string-append #$output "/share/wayland-sessions")))
                     (mkdir-p wayland-sessions)
                     (install-file "contrib/river.desktop"
                                   wayland-sessions)))))))
    (inputs
     (list libevdev
           zig-wayland
           zig-wlroots
           zig-xkbcommon))
    (native-inputs
     (list pkg-config
           scdoc))
    (home-page "https://isaacfreund.com/software/river/")
    (synopsis "Dynamic tiling Wayland compositor")
    (description
     "River is a dynamic tiling Wayland compositor with flexible
runtime configuration.  It can run nested in an X11/Wayland session or also
directly from a tty using KMS/DRM.")
    (license license:gpl3)))

(define-public tigerbeetle
  ;; Keep in sync with upstream release note.
  (let ((commit "af6cebb66578481b507bf351c0ddb19cfa038cc3")
        (min-release "0.16.4"))
    (package
      (name "tigerbeetle")
      (version "0.16.41")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/tigerbeetledb/tigerbeetle")
               (commit version)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "13c2y89ksdmnh8g666s0rlchwby860i5lvsvdrgj685222jdch2l"))))
      (build-system zig-build-system)
      (arguments
       (list
          #:zig zig-0.13
          #:test-target "test:unit"
          #:install-source? #f
          #:zig-release-type "safe"
          ;; Exact value required in build.zig.
          #:zig-build-target
          (match (or (%current-target-system)
                     (%current-system))
            ((? target-x86-64?)
             "x86_64-linux")
            ((? target-aarch64?)
             "aarch64-linux")
            (_ ""))
          #:zig-build-flags
          #~(list
             (string-append "-Dconfig-release=" #$(package-version this-package))
             (string-append "-Dconfig-release-client-min=" #$min-release)
             (string-append "-Dgit-commit=" #$commit))
          #:zig-test-flags
          #~(list (string-append "-Dgit-commit=" #$commit))
          #:phases
          #~(modify-phases %standard-phases
              (add-before 'check 'prepare-test-suite
                (lambda _
                  (substitute* "src/unit_tests.zig"
                    ;; Expects git repository.
                    ((".*tidy.zig.*") "")))))))
      (synopsis "Distributed financial accounting database")
      (description "TigerBeetle is a financial accounting database designed for
mission-critical safety and performance for financial services.")
      (home-page "https://github.com/tigerbeetledb/tigerbeetle")
      (supported-systems '("aarch64-linux" "x86_64-linux"))
      (license license:asl2.0))))

(define-public waylock
  (package
    (name "waylock")
    (version "1.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://codeberg.org/ifreund/waylock")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1bxs0gbczw8hb42fzl0i51jbzq82gvi3dad7xzhlall6fkl8882d"))))
    (build-system zig-build-system)
    (arguments
     (list
      #:zig zig-0.15
      #:install-source? #f
      ;; No tests.
      #:tests? #f
      #:zig-release-type "safe"
      #:zig-build-flags
      #~(list "-Dpie")))
    (inputs (list linux-pam zig-wayland zig-xkbcommon))
    (native-inputs (list pkg-config scdoc))
    (home-page "https://codeberg.org/ifreund/waylock")
    (synopsis "Wayland screen locker")
    (description
     "Waylock is a small screen locker for Wayland compositors implementing the
@code{ext-session-lock-v1} protocol.")
    (license license:expat)))

(define-public zig-clap
  (package
    (name "zig-clap")
    (version "0.9.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Hejsil/zig-clap")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1xjskvyib3kai3nmp574zfm07yvjsbzsxfysj96ss9339nq07ix6"))))
    (build-system zig-build-system)
    (home-page "https://github.com/Hejsil/zig-clap")
    (synopsis "Command line argument parsing library")
    (description
     "@code{clap} is a simple and easy to use command line argument parser
library for Zig.")
    (license license:expat)))

(define-public zig-lsp-codegen
  (let ((commit "063a98c13a2293d8654086140813bdd1de6501bc")
        (revision "0"))
    (package
      (name "zig-lsp-codegen")
      (version (git-version "0.1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/zigtools/zig-lsp-codegen")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "041lk25x050pz7yc781fn8bfpzci4kdrz8mw69sjs2f5mpyn4d1b"))))
      (build-system zig-build-system)
      (arguments (list #:zig zig-0.14))
      (home-page "https://zigtools.github.io/zig-lsp-codegen/")
      (synopsis "Generate @code{std.json} compatible Zig code")
      (description
       "This package provides a Zig library to generate @code{std.json}
compatible Zig code based on the @acronym{LSP, Language Server Protocol} meta
model.")
      (license license:expat))))

(define-public zig-lsp-kit-for-zig-0.15
  ;; Use the latest commit from the 0.15.x branch.
  (let ((commit "421ae644f53bd788e3699b5fc22b3e07b161e2b6")
        (revision "0"))
    (package
      (name "zig-lsp-kit")
      (version (git-version "0.1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/zigtools/lsp-kit")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0bm011zi7m1vbj3lss8h68v1gzhy45c06ff4r7mdaf34kwqax0ir"))))
      (build-system zig-build-system)
      (arguments (list #:skip-build? #t))
      (home-page "https://zigtools.github.io/lsp-kit/")
      (synopsis "Develop Language Server Protocol implementations in Zig")
      (description
       "Zig @acronym{LSP, Language Server Protocol} Kit provides the necessary
building blocks to develop LSP implementations in Zig.")
      (license license:expat))))

;;; TODO: Remove after 2027-01-16.
(define-deprecated-package zig-lsp-kit-for-zls-0.15
  zig-lsp-kit-for-zig-0.15)

(define-public zig-lsp-kit-for-zig-0.16
  (let ((commit "cf0ff08ac00301859656bbd4a5af6fcb889497e4")
        (revision "0"))
    (package
      (inherit zig-lsp-kit-for-zig-0.15)
      (name "zig-lsp-kit")
      (version (git-version "0.1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/zigtools/lsp-kit")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0nf4yb6sndgv18iqrysqw4iba7srwyicqwfpij6bl2h4xsbnxqnk")))))))

(define-public zig-diffz
  (let ((commit "420fcb22306ffd4c9c3c761863dfbb6bdbb18a73")
        (revision "0"))
    (package
      (name "zig-diffz")
      (version (git-version "0.0.1" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/ziglibs/diffz")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0rbcprl2c1kbd7xfwdqycz8r5grm069fcy6fafi14cnak77i0xyi"))))
      (build-system zig-build-system)
      (arguments (list #:skip-build? #t))
      (synopsis "Implementation of go-diff's diffmatchpatch in Zig")
      (description
       "This package provides a Zig implementation of @code{diffmatchpatch} in
@code{go-github-com-sergi-go-diff}.")
      (home-page "https://github.com/ziglibs/diffz")
      (license license:expat))))

(define-public zig-diffz-for-zig-zls-0.14
  (let ((commit "ef45c00d655e5e40faf35afbbde81a1fa5ed7ffb")
        (revision "1"))
    (package
      (inherit zig-diffz)
      (name "zig-diffz")
      (version (git-version "0.0.1" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/ziglibs/diffz")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0ah1m8mjqjc2szl5lx62zqj69irkbb3y245z14pknikxgg8xdzg7")))))))

(define-public zig-diffz-for-zls-0.15
  (let ((commit "a20dd1f11b10819a6f570f98b42e1c91e3704357")
        (revision "0"))
    (package
      (inherit zig-diffz)
      (name "zig-diffz")
      (version (git-version "0.0.1" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/ziglibs/diffz")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1qz7jqdh4f5dcrzkxipdmsgkfs37k450r9gm7ik7r72dfvjs9c6b")))))))

(define-public zig-diffz-for-zls-0.16
  (let ((commit "b39fe07e7fdbcf56e43ba2890b9f484f16969f90")
        (revision "0"))
    (package
      (inherit zig-diffz)
      (name "zig-diffz")
      (version (git-version "0.0.1" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/ziglibs/diffz")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0ismvmi82m0l9cdp5kgd4n94l0pgba0r4mfpn061d839f4wils4s")))))))

(define-public zig-known-folders
  (let ((commit "1cceeb70e77dec941a4178160ff6c8d05a74de6f")
        (revision "0"))
    (package
      (name "zig-known-folders")
      (version (git-version "0.7.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/ziglibs/known-folders")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1kr58ragd6nk29ps0fwc4r3zxv2javkiq4vny4zwx6wqqid98nld"))))
      (build-system zig-build-system)
      (synopsis "Zig library to access well-known folders")
      (description
       "This package provides a Zig library for accessing well-known folders
across several operating systems.")
      (home-page "https://github.com/ziglibs/known-folders")
      (license license:expat))))

(define-public zig-known-folders-for-zig-0.14
  (let ((commit "aa24df42183ad415d10bc0a33e6238c437fc0f59")
        (revision "1")
        (base zig-known-folders))
    (package
      (inherit base)
      (name "zig-known-folders")
      (version (git-version "0.7.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/ziglibs/known-folders")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1ilq3hqprrwpbz7ckp01g4ksl41jq57rd7zf622w3immy6apc8k2"))))
      (arguments
       (substitute-keyword-arguments arguments
         ((#:zig _ #f) zig-0.14))))))

(define-public zig-known-folders-for-zig-0.15
  (let ((commit "83d39161eac2ed6f37ad3cb4d9dd518696ce90bb")
        (revision "0")
        (base zig-known-folders))
    (package
      (inherit base)
      (name "zig-known-folders")
      (version (git-version "0.7.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/ziglibs/known-folders")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0czhcdrj2scpg14y75mfgg2xdn79is79ny10ys6fx8dl7xhdm9bx"))))
      (arguments
       (substitute-keyword-arguments arguments
         ((#:zig _ #f) zig-0.15))))))

;;; TODO: Remove after 2027-01-16.
(define-deprecated-package zig-known-folders-for-zls-0.15
  zig-known-folders-for-zig-0.15)

(define-public zig-known-folders-for-zig-0.16
  (let ((commit "7deb7aa3ba631db04faaa343cd30553c34b7d01d")
        (revision "0"))
    (package
      (inherit zig-known-folders)
      (name "zig-known-folders")
      (version (git-version "0.16.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/ziglibs/known-folders")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "19g6z3cdgyh1c3kj02c9vlkg812hh7f76pdsxk6x2bwn9x6wxd2l"))))
      (arguments
       (substitute-keyword-arguments arguments
         ((#:zig _ #f) zig-0.16))))))

(define-public zig-pixman
  (package
    (name "zig-pixman")
    (version "0.3.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://codeberg.org/ifreund/zig-pixman")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1mg0fmcnfl5il8nx9nplxm8hg54jkdg9ks7r40w4451rx9m3il7j"))))
    (build-system zig-build-system)
    (arguments (list #:skip-build? #t))
    (propagated-inputs (list pixman))
    (synopsis "Zig bindings for Pixman")
    (description "This package provides Zig bindings for @code{pixman}.")
    (home-page "https://codeberg.org/ifreund/zig-pixman")
    (license license:expat)))

(define-public zig-scripty
  (let ((commit "50dbab8945440089384f26ec165d870c29555247")
        (revision "0"))
    (package
      (name "zig-scripty")
      (version (git-version "0.1.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/kristoff-it/scripty")
                       (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1qkmrisac80h2k996a8s60pxr4fyn3gfjisb9dvpyl29pa4ghwmg"))))
      (build-system zig-build-system)
      (arguments (list #:zig zig-0.15))
      (inputs (list zig-tracy))
      (home-page "https://github.com/kristoff-it/scripty")
      (synopsis "Tiny scripting language meant to be embedded in strings")
      (description "Scripty is a tiny scripting language meant to be embedded
in strings or other similar constructs, usually within a host document.
Scripty supports only the basic syntax necessary to build expressions that can:
@itemize
@item refer to basic literals;
@item access fields starting from a root evaluation context (eg $foo.bar);
@item call functions.
@end itemize")
      (license license:expat))))

(define-public zig-tracy
  (let ((commit "67d2d89e351048c76fc6d161e0ac09d8a831dc60")
        (revision "0"))
    (package
      (name "zig-tracy")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/kristoff-it/tracy")
                       (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0hb6qsx589icsa16ip59jlfr55mgdqlnmzwzwngyzzls3dp3bah4"))))
      (build-system zig-build-system)
      (arguments (list #:zig zig-0.15))
      (home-page "https://github.com/kristoff-it/tracy")
      (synopsis "Zig bindings for Tracy")
      (description "This package provides bindings for Tracy, the frame
profiler (@pxref{https://tracy.nereid.pl}).")
      (license license:expat))))

(define-public zig-wayland
  (package
    (name "zig-wayland")
    (version "0.6.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://codeberg.org/ifreund/zig-wayland")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "10chjkhahkjpl9wn308qdasa955rnipsmlrzp3wryl2rv16chvyy"))))
    (build-system zig-build-system)
    (arguments
     (list #:zig zig-0.16
           #:zig-release-type "safe"
           #:zig-build-flags
           #~(list "-Denable-tests")
           #:zig-test-flags
           #~(list "-Denable-tests")
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'configure 'fix-cross-compilation
                 (lambda _
                   (substitute* "build.zig"
                     (("pkg-config") (getenv "PKG_CONFIG"))))))))
    (propagated-inputs (list wayland wayland-protocols))
    (native-inputs (list pkg-config wayland))
    (synopsis "Zig Wayland bindings and protocol scanner")
    (description
     "This package provides Zig bindings for @code{wayland} and a @code{Scanner}
interface.")
    (home-page "https://codeberg.org/ifreund/zig-wayland")
    (license license:expat)))

(define-public zig-wlroots
  (package
    (name "zig-wlroots")
    (version "0.20.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://codeberg.org/ifreund/zig-wlroots")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "16cy0a65jddfhsvgvcmbj5ln7wx37nia6j6rr062k272dhkwgz3i"))))
    (build-system zig-build-system)
    (arguments
     (list #:zig zig-0.16
           #:zig-release-type "safe"
           #:zig-build-flags
           #~(list "-Denable-tests")
           #:zig-test-flags
           #~(list "-Denable-tests")))
    (propagated-inputs
     (list wlroots-0.20
           zig-pixman
           zig-wayland
           zig-xkbcommon-0.3))
    (native-inputs (list pkg-config))
    (synopsis "Zig bindings for wlroots")
    (description "This package provides Zig bindings for @code{wlroots}.")
    (home-page "https://codeberg.org/ifreund/zig-wlroots")
    (license license:expat)))

(define-public zig-xkbcommon
  (package
    (name "zig-xkbcommon")
    (version "0.4.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://codeberg.org/ifreund/zig-xkbcommon")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "18cjrv4gzyihs6cvr1djkb74lj76yn84p65s71ihp11fywzjc2fd"))))
    (build-system zig-build-system)
    (arguments (list #:skip-build? #t))
    (propagated-inputs (list libxkbcommon))
    (synopsis "Zig bindings for libxkbcommon")
    (description "This package provides Zig bindings for @code{libxkbcommon}.")
    (home-page "https://codeberg.org/ifreund/zig-xkbcommon")
    (license license:expat)))

;; Older version needed for `river-classic' and `zig-wlroots'.
(define-public zig-xkbcommon-0.3
  (package
    (inherit zig-xkbcommon)
    (name "zig-xkbcommon")
    (version "0.3.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://codeberg.org/ifreund/zig-xkbcommon")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "117nw4b5q14mb6j5yhvydlwllbd7gyxp176as4gj9qb5zh8wz5kv"))))))

(define-public zig-zigimg
  ;; No tagged release.
  (let ((commit "52f10dd3e3b1cd4614fe72a8a8f0eddc7700bc0a")
        (revision "0"))
    (package
      (name "zig-zigimg")
      (version (git-version "0.1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/zigimg/zigimg")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "187nh49rdv37arlvf056jiv58n4y87q2pm6qvznn75zrszjlvp0b"))))
      (build-system zig-build-system)
      (home-page "https://github.com/zigimg/zigimg")
      (synopsis "Zig image library")
      (description
       "@code{zigimg} is a Zig library for reading and writing different image
formats.")
      (license license:expat))))

(define-public zig-zls-0.12
  (package
    (name "zig-zls")
    (version "0.12.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/zigtools/zls")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1ini1ifa9b0v2ika3sqsiiv2p7v9npfslss45280yxwn2pjqmn7n"))
       (snippet
        (rename-zig-dependencies
         '(("diffz" . "zig-diffz")
           ("known_folders" . "zig-known-folders"))))))
    (build-system zig-build-system)
    (arguments
     (let ((version-data-path
            #~(string-append
               "-Dversion_data_path="
               #+(package-source (this-package-native-input "zig"))
               "/doc/langref.html.in")))
       (list #:zig (this-package-native-input "zig")
             #:install-source? #f
             #:zig-release-type "safe"
             #:zig-build-flags
             #~(list #$version-data-path "-Dpie")
             #:zig-test-flags
             #~(list #$version-data-path))))
    (inputs (list zig-diffz zig-known-folders))
    (native-inputs (list zig-0.12))
    (synopsis "Zig language server")
    (description
     "Zig Language Server is a language server implementing the @acronym{LSP,
Language Server Protocol} for the Zig programming language.")
    (home-page "https://github.com/zigtools/zls")
    (license license:expat)))

(define-public zig-zls-0.13
  (let ((base zig-zls-0.12))
    (package
      (inherit base)
      (name "zig-zls")
      (version "0.13.0")
      (source (origin
                (inherit (package-source base))
                (uri (git-reference
                      (url "https://github.com/zigtools/zls")
                      (commit version)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1dbg06v136yjcs9grc6xwsmv0cm39c0sdkh5vzn7h1qxxka6ixlp"))))
      (build-system zig-build-system)
      (native-inputs
       (modify-inputs native-inputs
         (replace "zig" zig-0.13))))))

(define-public zig-zls-0.14
  (let ((base zig-zls-0.13))
    (package
      (inherit base)
      (name "zig-zls")
      (version "0.14.0")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/zigtools/zls")
                      (commit version)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1al4ry22y56v1jbph3vb6la2ln8dxc8hb3g7byng6yf8czx2g4q3"))
                (snippet
                 (rename-zig-dependencies
                  '(("diffz" . "zig-diffz")
                    ("known_folders" . "zig-known-folders")
                    ("lsp-codegen" . "zig-lsp-codegen"))))))
      (arguments
       (list #:zig (this-package-native-input "zig")
             #:install-source? #f
             #:zig-release-type "safe"
             #:zig-build-flags ''("-Dpie")))
      (native-inputs
       (modify-inputs native-inputs
         (replace "zig" zig-0.14)))
      (inputs
       (modify-inputs inputs
         (prepend zig-lsp-codegen)
         (replace "zig-diffz" zig-diffz-for-zig-zls-0.14)
         (replace "zig-known-folders" zig-known-folders-for-zig-0.14))))))

(define-public zig-zls-0.15
  (let ((base zig-zls-0.14))
    (package
      (inherit base)
      (name "zig-zls")
      (version "0.15.0")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/zigtools/zls")
                      (commit version)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0ydnaxf29mj3gamig9phf991s1civfk6jkydn2xiqwv394fx4p0q"))
                (snippet
                 (rename-zig-dependencies
                  '(("diffz" . "zig-diffz")
                    ("known_folders" . "zig-known-folders")
                    ("lsp_kit" . "zig-lsp-kit"))))))
      (native-inputs
       (modify-inputs native-inputs
         (replace "zig" zig-0.15)))
      (inputs
       (modify-inputs inputs
         (prepend zig-lsp-kit-for-zig-0.15)
         (delete "zig-lsp-codegen")
         (replace "zig-diffz" zig-diffz-for-zls-0.15)
         (replace "zig-known-folders" zig-known-folders-for-zig-0.15))))))

(define-public zig-zls-0.16
  (package
    (inherit zig-zls-0.15)
    (name "zig-zls")
    (version "0.16.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/zigtools/zls")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0sbxz7nk9xam6whi8z05a469c9sfg50gkjm70ivavx1hpcwmck4k"))
              (snippet
               (rename-zig-dependencies
                '(("diffz" . "zig-diffz")
                  ("known_folders" . "zig-known-folders")
                  ("lsp_kit" . "zig-lsp-kit"))))))
    (native-inputs
     (modify-inputs native-inputs
       (replace "zig" zig-0.16)))
    (inputs
     (modify-inputs inputs
       (replace "zig-diffz" zig-diffz-for-zls-0.16)
       (replace "zig-known-folders" zig-known-folders-for-zig-0.16)
       (replace "zig-lsp-kit" zig-lsp-kit-for-zig-0.16)))))

(define-public zig-zls zig-zls-0.13)
