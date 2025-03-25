;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2022 Maya Tomasek <maya.tomasek@disroot.org>
;;; Copyright © 2023 Ekaitz Zarraga <ekaitz@elenq.tech>
;;; Copyright © 2023 Felix Lechner <felix.lechner@lease-up.com>
;;; Copyright © 2024 Justin Veilleux <terramorpha@cock.li>
;;; Copyright © 2025 Ashvith Shetty <ashvithshetty0010@zohomail.in>
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
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system zig)
  #:use-module (guix gexp)
  #:use-module (gnu packages)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages man)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages zig))

(define-public beanbag
  (package
    (name "beanbag")
    (version "0.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://codeberg.org/bwbuhse/beanbag")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1d2h5bqicqnyawswdq7bg1w9frjk0ra2sva1as2qgc5s7pjclyql"))
       (snippet (rename-zig-dependencies '(("clap" . "zig-clap")
                                           ("zigimg" . "zig-zigimg"))))))
    (build-system zig-build-system)
    (arguments
     (list
      #:install-source? #f
      #:zig-release-type "safe"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-deps
            (lambda* (#:key inputs #:allow-other-keys)
              (delete-file-recursively "protocol")
              (substitute* "build.zig"
                (("protocol/(wlr-layer-shell-unstable-v1.xml)" _ file)
                 (string-append
                  (search-input-file
                   inputs
                   (in-vicinity "share/wlr-protocols/unstable/" file)))))))
          ;; TODO: Support test-target when changing zig-build-system next time.
          (replace 'check
            (lambda* (#:key tests? zig-test-flags target parallel-tests?
                      #:allow-other-keys)
              (when (and tests? (not target))
                (let ((old-destdir (getenv "DESTDIR")))
                  (setenv "DESTDIR" "test-out") ;; Avoid colisions with the build output
                  (let ((call `("zig" "build" "check" "--verbose"
                                ,(string-append
                                  "-j" (if parallel-tests?
                                           (number->string (parallel-job-count))
                                           "1"))
                                ,@zig-test-flags)))
                    (format #t "running: ~s~%" call)
                    (apply invoke call))
                  (if old-destdir
                      (setenv "DESTDIR" old-destdir)
                      (unsetenv "DESTDIR")))))))))
    (inputs (list wlr-protocols zig-clap zig-pixman zig-wayland zig-zigimg))
    (native-inputs (list pkg-config))
    (home-page "https://codeberg.org/bwbuhse/beanbag")
    (synopsis "Wallpaper application for Wayland compositors")
    (description
     "@command{beanbag} is a wallpaper application for Wayland compositors,
heavily inspired by @code{wbg}.")
    (license license:gpl3+)))

(define-public river
  (package
    (name "river")
    (version "0.3.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://codeberg.org/river/river")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1g4p1fa9kf23ldq1p8yplkb7hnkhfynhn93z6qqm67nacss2idbc"))))
    (build-system zig-build-system)
    (arguments
     (list #:install-source? #f
           #:zig-release-type "safe"
           #:zig-build-flags
           #~(list "-Dpie" "-Dxwayland")
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'fix-path
                 (lambda _
                   (substitute* "build.zig"
                     (("/bin/sh") (which "sh")))))
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
  (package
    (name "tigerbeetle")
    (version "0.13.35")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tigerbeetledb/tigerbeetle")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0x8msknvq8s6vnlczq5fxmaiqvig2sbcv60c3x8zbgr28dsqpmll"))))
    (build-system zig-build-system)
    (arguments
     (list
      #:zig zig-0.9
      #:install-source? #f
      #:zig-release-type "safe"))
    (synopsis "Distributed financial accounting database")
    (description "TigerBeetle is a financial accounting database designed for
mission-critical safety and performance for financial services.")
    (home-page "https://github.com/tigerbeetledb/tigerbeetle")
    (license license:asl2.0)))

(define-public waylock
  (package
    (name "waylock")
    (version "1.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://codeberg.org/ifreund/waylock")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0jlq23cb5069sa5ipshhj82a9rn30frflwi9skp2kplqpxm15wwd"))))
    (build-system zig-build-system)
    (arguments
     (list
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

(define-public zig-pixman
  (package
    (name "zig-pixman")
    (version "0.2.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://codeberg.org/ifreund/zig-pixman")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0il6nw51kf08bcxpf45n7h78k1iyfi1zarcvpb7n19g2r48dkiyd"))))
    (build-system zig-build-system)
    (arguments (list #:skip-build? #t))
    (propagated-inputs (list pixman))
    (synopsis "Zig bindings for Pixman")
    (description "This package provides Zig bindings for @code{pixman}.")
    (home-page "https://codeberg.org/ifreund/zig-pixman")
    (license license:expat)))

(define-public zig-wayland
  (package
    (name "zig-wayland")
    (version "0.2.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://codeberg.org/ifreund/zig-wayland")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1cf5085f6c0yly4fcr49jry3mh12bybw98x5lvickl6w5gxsvy3n"))))
    (build-system zig-build-system)
    (arguments
     (list #:phases
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
    (version "0.18.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://codeberg.org/ifreund/zig-wlroots")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1rckbrdqc4b5q6r8ppijkkf0xi01536sfsnfgmy9f3mfj3hvifvy"))))
    (build-system zig-build-system)
    (arguments
     (list #:zig-release-type "safe"
           #:zig-build-flags
           #~(list "-Denable-tests")
           #:zig-test-flags
           #~(list "-Denable-tests")
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'install 'install-tinywl
                 (lambda args
                   (chdir "tinywl")
                   (apply (assoc-ref %standard-phases 'build)
                          `(,@args #:zig-build-flags ()))
                   (apply (assoc-ref %standard-phases 'install)
                          `(,@args #:install-source? #f))
                   (chdir ".."))))))
    (propagated-inputs
     (list wlroots
           zig-pixman
           zig-wayland
           zig-xkbcommon))
    (native-inputs (list pkg-config))
    (synopsis "Zig bindings for wlroots")
    (description "This package provides Zig bindings for @code{wlroots}.")
    (home-page "https://codeberg.org/ifreund/zig-wlroots")
    (license license:expat)))

(define-public zig-xkbcommon
  (package
    (name "zig-xkbcommon")
    (version "0.2.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://codeberg.org/ifreund/zig-xkbcommon")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "16f59n7l2gcpnq8gb4v8skr4jhb2l6ax75rna92nqzj15f4ikqag"))))
    (build-system zig-build-system)
    (arguments (list #:skip-build? #t))
    (propagated-inputs (list libxkbcommon))
    (synopsis "Zig bindings for libxkbcommon")
    (description "This package provides Zig bindings for @code{libxkbcommon}.")
    (home-page "https://codeberg.org/ifreund/zig-xkbcommon")
    (license license:expat)))

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

(define-public zig-zls-0.10
  (package
    (name "zig-zls")
    (version "0.10.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/zigtools/zls")
                    (commit version)
                    (recursive? #t)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1lsks7h3z2m4psyn9mwdylv1d6a9i3z54ssadiz76w0clbh8ch9k"))))
    (build-system zig-build-system)
    (inputs (list zig-0.10 python))
    (arguments
     (list #:zig zig-0.10
           #:install-source? #f
           #:zig-release-type "safe"
           ;; The tests fail with memory leaks.
           #:tests? #f))
    (synopsis "Zig language server")
    (description
     "Zig Language Server is a language server implementing the @acronym{LSP,
Language Server Protocol} for the Zig programming language.")
    (home-page "https://github.com/zigtools/zls")
    (license license:expat)))

(define-public zig-zls-0.12
  (package
    (inherit zig-zls-0.10)
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
    (native-inputs (list zig-0.12))))

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
       (modify-inputs (package-native-inputs base)
         (replace "zig" zig-0.13))))))

(define-public zig-zls zig-zls-0.13)
