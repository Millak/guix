;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 Timmy Douglas <mail@timmydouglas.com>
;;; Copyright © 2022 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2022 Zhu Zihao <all_but_last@163.com>
;;; Copyright © 2022 Michael Rohleder <mike@rohleder.de>
;;; Copyright © 2023 Zongyuan Li <zongyuan.li@c0x0o.me>
;;; Copyright © 2023 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2024 Tomas Volf <~@wolfsden.cz>
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

(define-module (gnu packages containers)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (guix build-system meson)
  #:use-module (guix utils)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages python)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages selinux)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages virtualization)
  #:use-module (gnu packages web)
  #:use-module (gnu packages wget))

(define-public crun
  (package
    (name "crun")
    (version "1.14.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/containers/crun/releases/download/"
             version
             "/crun-" version ".tar.gz"))
       (sha256
        (base32
         "02lplc2asyllb58mvy7l8b9gsk7fxs95g928xk28yzmf592ay33x"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--disable-systemd")
       #:tests? #f ; XXX: needs /sys/fs/cgroup mounted
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-tests
           (lambda _
             (substitute* (find-files "tests" "\\.(c|py)")
               (("/bin/true") (which "true"))
               (("/bin/false") (which "false"))
                                        ; relies on sd_notify which requires systemd?
               (("\"sd-notify\" : test_sd_notify,") "")
               (("\"sd-notify-file\" : test_sd_notify_file,") "")))))))
    (inputs
     (list libcap
           libseccomp
           yajl))
    (native-inputs
     (list automake
           autoconf
           git
           libtool
           pkg-config
           python-3))
    (home-page "https://github.com/containers/crun")
    (synopsis "Open Container Initiative (OCI) Container runtime")
    (description
     "crun is a fast and low-memory footprint Open Container Initiative (OCI)
Container Runtime fully written in C.")
    (license license:gpl2+)))

(define-public conmon
  (package
    (name "conmon")
    (version "2.0.31")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/containers/conmon")
             (commit (string-append "v" version))))
       (sha256
        (base32 "1cxklcihb2i4ywli0fxafkp2gi1x831r37z7spnigaj6pzj1517w"))
       (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (arguments
     (list #:make-flags
           #~(list (string-append "CC=" #$(cc-for-target))
                   (string-append "PREFIX=" #$output))
           ;; XXX: uses `go get` to download 50 packages, runs a ginkgo test suite
           ;; then tries to download busybox and use a systemd logging library
           ;; see also https://github.com/containers/conmon/blob/main/nix/derivation.nix
           #:tests? #f
           #:test-target "test"
           #:phases
           #~(modify-phases %standard-phases
               (delete 'configure)
               (add-after 'unpack 'set-env
                 (lambda _
                   ;; when running go, things fail because
                   ;; HOME=/homeless-shelter.
                   (setenv "HOME" "/tmp"))))))
    (inputs
     (list crun
           glib
           libseccomp))
    (native-inputs
     (list git
           go
           pkg-config))
    (home-page "https://github.com/containers/conmon")
    (synopsis "Monitoring tool for Open Container Initiative (OCI) runtime")
    (description
     "Conmon is a monitoring program and communication tool between a container
manager (like Podman or CRI-O) and an Open Container Initiative (OCI)
runtime (like runc or crun) for a single container.")
    (license license:asl2.0)))

(define-public distrobox
  (package
    (name "distrobox")
    (version "1.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/89luca89/distrobox")
             (commit version)))
       (sha256
        (base32 "1g14q1sm3026h9n85v1gc3m2v9sgrac2mr9yrkh98qg5yahzmpc3"))
       (file-name (git-file-name name version))))
    (build-system copy-build-system)
    (inputs
     (list podman wget))
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-before 'install 'refer-to-inputs
                 (lambda* (#:key inputs #:allow-other-keys)
                   (substitute* (find-files "." "^distrobox[^.]*[^1]$")
                     (("podman") (search-input-file inputs "/bin/podman"))
                     (("wget") (search-input-file inputs "/bin/wget"))
                     (("command -v") "test -x"))))
               (replace 'install
                 (lambda _
                   (invoke "./install" "--prefix" #$output))))))
    (home-page "https://distrobox.privatedns.org/")
    (synopsis "Create and start containers highly integrated with the hosts")
    (description
     "Distrobox is a fancy wrapper around Podman or Docker to create and start
containers highly integrated with the hosts.")
    (license license:gpl3)))

(define-public libslirp
  (package
    (name "libslirp")
    (version "4.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.freedesktop.org/slirp/libslirp")
             (commit (string-append "v" version))))
       (sha256
        (base32 "0dny8187a8qh6akaa37aa9b5pjxx88f02wh6achp4mygff0ipxba"))
       (file-name (git-file-name name version))))
    (build-system meson-build-system)
    (propagated-inputs
     ;; In Requires of slirp.pc.
     (list glib))
    (native-inputs
     (list pkg-config))
    (home-page "https://gitlab.freedesktop.org/slirp/libslirp")
    (synopsis "User-mode networking library")
    (description
     "libslirp is a user-mode networking library used by virtual machines,
containers or various tools.")
    (license license:bsd-3)))

(define-public slirp4netns
  (package
    (name "slirp4netns")
    (version "1.2.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rootless-containers/slirp4netns")
             (commit (string-append "v" version))))
       (sha256
        (base32 "0czvdsdv821fz4jd9rgrlkdhhjna6frawr8klvx3k2cfh444fbii"))
       (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f ; XXX: open("/dev/net/tun"): No such file or directory
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'fix-hardcoded-paths
                    (lambda _
                      (substitute* (find-files "tests" "\\.sh")
                        (("ping") "/run/setuid-programs/ping")))))))
    (inputs
     (list glib
           libcap
           libseccomp
           libslirp))
    (native-inputs
     (list automake
           autoconf
           iproute ; iproute, jq, nmap (ncat) and util-linux are for tests
           jq
           nmap
           pkg-config
           util-linux))
    (home-page "https://github.com/rootless-containers/slirp4netns")
    (synopsis "User-mode networking for unprivileged network namespaces")
    (description
     "slirp4netns provides user-mode networking (\"slirp\") for unprivileged
network namespaces.")
    (license license:gpl2+)))

(define-public passt
  (package
    (name "passt")
    (version "2023_12_30.f091893")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://passt.top/passt/snapshot/passt-" version
                           ".tar.gz"))
       (sha256
        (base32 "1nyd4h93qlxn1r01ffijpsd7r7ny62phki5j58in8gz021jj4f3d"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:make-flags
      #~(list (string-append "CC=" #$(cc-for-target))
              "RLIMIT_STACK_VAL=1024"   ; ¯\_ (ツ)_/¯
              (string-append "VERSION=" #$version)
              (string-append "prefix=" #$output))
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure))))
    (home-page "https://passt.top")
    (synopsis "Plug A Simple Socket Transport")
    (description
     "passt implements a thin layer between guest and host, that only
implements what's strictly needed to pretend processes are running locally.
The TCP adaptation doesn't keep per-connection packet buffers, and reflects
observed sending windows and acknowledgements between the two sides.  This TCP
adaptation is needed as passt runs without the CAP_NET_RAW capability: it
can't create raw IP sockets on the pod, and therefore needs to map packets at
Layer-2 to Layer-4 sockets offered by the host kernel.

Also provides pasta, which similarly to slirp4netns, provides networking to
containers by creating a tap interface available to processes in the
namespace, and mapping network traffic outside the namespace using native
Layer-4 sockets.")
    (license (list license:gpl2+ license:bsd-3))))

(define-public cni-plugins
  (package
    (name "cni-plugins")
    (version "1.4.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/containernetworking/plugins")
             (commit (string-append "v" version))))
       (sha256
        (base32 "0l6f4z762n8blak41wcxdmdhm92gqw2qcxcqd3s4wiql3d7273kj"))
       (file-name (git-file-name name version))))
    (build-system go-build-system)
    (arguments
     `(#:unpack-path "github.com/containernetworking/plugins"
       #:tests? #f ; XXX: see stat /var/run below
       #:phases (modify-phases %standard-phases
                  (replace 'build
                    (lambda _
                      (with-directory-excursion
                          "src/github.com/containernetworking/plugins"
                        (invoke "./build_linux.sh"))))
                  (replace 'check
                    (lambda* (#:key tests? #:allow-other-keys)
                      ; only pkg/ns tests run without root
                      (when tests?
                        (with-directory-excursion
                            "src/github.com/containernetworking/plugins/pkg/ns"
                          (invoke "stat" "/var/run") ; XXX: test tries to stat this directory
                          (invoke "unshare" "-rmn" "go" "test")))))
                  (add-before 'check 'set-test-environment
                    (lambda _
                      (setenv "XDG_RUNTIME_DIR" "/tmp/cni-rootless")))
                  (replace 'install
                    (lambda* (#:key outputs #:allow-other-keys)
                      (copy-recursively
                       "src/github.com/containernetworking/plugins/bin"
                       (string-append (assoc-ref outputs "out") "/bin")))))))
    (native-inputs
     (list util-linux))
    (home-page "https://github.com/containernetworking/plugins")
    (synopsis "Container Network Interface (CNI) network plugins")
    (description
     "This package provides Container Network Interface (CNI) plugins to
configure network interfaces in Linux containers.")
    (license license:asl2.0)))

(define-public gvisor-tap-vsock
  (package
    (name "gvisor-tap-vsock")
    (version "0.7.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/containers/gvisor-tap-vsock")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1q1zism0c63k2aq6yhkjqc3b2zsm4lwn0bk39p2kl79h798wfyp4"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:make-flags `(list ,(string-append "GIT_VERSION=v" version))
      #:test-target "test"
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (add-before 'build 'setenv
            (lambda _
              ;; For golang toolchain.
              (setenv "HOME" "/tmp")))
          (add-before 'check 'prune-tests
            (lambda _
              ;; Requires internet connection to fetch QEMU image.
              (invoke "rm" "-r" "test")))
          (replace 'install
            (lambda _
              (install-file "bin/gvproxy" (string-append #$output "/bin")))))))
    (native-inputs (list go-1.20))
    (home-page "https://github.com/containers/gvisor-tap-vsock")
    (synopsis "Network stack for virtualization based on gVisor")
    (description "This package provides a replacement for @code{libslirp} and
@code{VPNKit}, written in pure Go.  It is based on the network stack of gVisor
and brings a configurable DNS server and dynamic port forwarding.

It can be used with QEMU, Hyperkit, Hyper-V and User-Mode Linux.

The binary is called @command{gvproxy}.")
    (license license:asl2.0)))

;; For podman to work, the user needs to run
;; `sudo mount -t cgroup2 none /sys/fs/cgroup`

(define-public podman
  (package
    (name "podman")
    (version "4.9.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/containers/podman")
             (commit (string-append "v" version))))
       (modules '((guix build utils)))
       ;; FIXME: Btrfs libraries not detected by these scripts.
       (snippet '(substitute* "Makefile"
                   ((".*hack/btrfs.*") "")))
       (patches
        (search-patches
         "podman-program-lookup.patch"))
       (sha256
        (base32 "17g7n09ndxhpjr39s9qwxdcv08wavjj0g5nmnrvrkz2wgdqigl1x"))
       (file-name (git-file-name name version))))

    (build-system gnu-build-system)
    (arguments
     (list
      #:make-flags
      #~(list #$(string-append "CC=" (cc-for-target))
              (string-append "PREFIX=" #$output))
      #:tests? #f                  ; /sys/fs/cgroup not set up in guix sandbox
      #:test-target "test"
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (add-after 'unpack 'set-env
            (lambda* (#:key inputs #:allow-other-keys)
              ;; when running go, things fail because
              ;; HOME=/homeless-shelter.
              (setenv "HOME" "/tmp")))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                ;; (invoke "strace" "-f" "bin/podman" "version")
                (invoke "make" "localsystem")
                (invoke "make" "remotesystem"))))
          (add-after 'unpack 'fix-hardcoded-paths
            (lambda _
              (substitute* "vendor/github.com/containers/common/pkg/config/config.go"
                (("@SLIRP4NETNS_DIR@")
                 (string-append #$slirp4netns "/bin"))
                (("@PASST_DIR@")
                 (string-append #$passt "/bin")))
              (substitute* "hack/install_catatonit.sh"
                (("CATATONIT_PATH=\"[^\"]+\"")
                 (string-append "CATATONIT_PATH=" (which "true"))))
              (substitute* "vendor/github.com/containers/common/pkg/config/config_linux.go"
                (("/usr/local/libexec/podman")
                 (string-append #$output "/libexec/podman"))
                (("/usr/local/lib/podman")
                 (string-append #$output "/bin")))
              (substitute* "vendor/github.com/containers/common/pkg/config/default.go"
                (("/usr/libexec/podman/conmon") (which "conmon"))
                (("/usr/local/libexec/cni")
                 (string-append #$(this-package-input "cni-plugins")
                                "/bin"))
                (("/usr/bin/crun") (which "crun")))))
          (add-after 'install 'install-completions
            (lambda _
              (invoke "make" "install.completions"
                      (string-append "PREFIX=" #$output)))))))
    (inputs
     (list btrfs-progs
           cni-plugins
           conmon
           crun
           gpgme
           go-github-com-go-md2man
           iptables
           libassuan
           libseccomp
           libselinux
           passt
           slirp4netns))
    (native-inputs
     (list bats
           git
           go-1.21
           ; strace ; XXX debug
           pkg-config
           python))
    (home-page "https://podman.io")
    (synopsis "Manage containers, images, pods, and their volumes")
    (description
     "Podman (the POD MANager) is a tool for managing containers and images,
volumes mounted into those containers, and pods made from groups of
containers.

The @code{machine} subcommand is not supported due to gvproxy not being
packaged.")
    (license license:asl2.0)))

(define-public buildah
  (package
    (name "buildah")
    (version "1.29.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/containers/buildah")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1mcqkz68fjccdla1bgxw57w268a586brm6x28fcm6x425ah0w07h"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/containers/buildah/cmd/buildah"
           #:unpack-path "github.com/containers/buildah"

           ;; Some dependencies require go-1.18 to build.
           #:go go-1.18

           #:tests? #f
           #:install-source? #f
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'prepare-install-docs
                 (lambda* (#:key unpack-path #:allow-other-keys)
                   (substitute* (string-append "src/"
                                               unpack-path
                                               "/docs/Makefile")
                     (("../tests/tools/build/go-md2man")
                      (which "go-md2man")))
                   (substitute* (string-append "src/"
                                               unpack-path
                                               "/docs/Makefile")
                     (("/usr/local") (string-append #$output)))))
               (add-after 'build 'build-docs
                 (lambda* (#:key unpack-path #:allow-other-keys)
                   (let ((doc (string-append "src/" unpack-path "/docs")))
                     (invoke "make" "-C" doc))))
               (add-after 'install 'install-docs
                 (lambda* (#:key unpack-path #:allow-other-keys)
                   (let ((doc (string-append "src/" unpack-path "/docs")))
                     (invoke "make" "-C" doc "install")))))))
    (inputs (list btrfs-progs
                  cni-plugins
                  conmon
                  eudev
                  glib
                  gpgme
                  libassuan
                  libseccomp
                  lvm2
                  runc))
    (native-inputs
     (list go-github-com-go-md2man
           gnu-make
           pkg-config))
    (synopsis "Build @acronym{OCI, Open Container Initiative} images")
    (description
     "Buildah is a command-line tool to build @acronym{OCI, Open Container
Initiative} container images.  More generally, it can be used to:

@itemize
@item
create a working container, either from scratch or using an image as a
starting point;
@item
create an image, either from a working container or via the instructions
in a @file{Dockerfile};
@item
mount a working container's root filesystem for manipulation;
@item
use the updated contents of a container's root filesystem as a filesystem
layer to create a new image.
@end itemize")
    (home-page "https://buildah.io")
    (license license:asl2.0)))
