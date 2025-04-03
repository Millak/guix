;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 Timmy Douglas <mail@timmydouglas.com>
;;; Copyright © 2022 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2022 Zhu Zihao <all_but_last@163.com>
;;; Copyright © 2022 Michael Rohleder <mike@rohleder.de>
;;; Copyright © 2023 Zongyuan Li <zongyuan.li@c0x0o.me>
;;; Copyright © 2023 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2024, 2025 Tomas Volf <~@wolfsden.cz>
;;; Copyright © 2024 Foundation Devices, Inc. <hello@foundation.xyz>
;;; Copyright © 2024 Jean-Pierre De Jesus DIAZ <jean@foundation.xyz>
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
  #:use-module (guix modules)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system pyproject)
  #:use-module (guix utils)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages man)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages python)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages selinux)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages virtualization)
  #:use-module (gnu packages web)
  #:use-module (gnu packages wget))

(define-public crun
  (package
    (name "crun")
    (version "1.21")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/containers/crun/releases/download/"
             version
             "/crun-" version ".tar.gz"))
       (sha256
        (base32
         "11n42h4sx02xyrnkaq3d6l2i0fac7xshgkryvsj08j2afq771ysb"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:configure-flags #~(list "--disable-systemd")
      #:tests? #f ; XXX: needs /sys/fs/cgroup mounted
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-tests
            (lambda _
              (substitute* (find-files "tests" "\\.(c|py)")
                (("/bin/true") (which "true"))
                (("/bin/false") (which "false"))
                ;; relies on sd_notify which requires systemd?
                (("\"sd-notify\" : test_sd_notify,") "")
                (("\"sd-notify-file\" : test_sd_notify_file,") "")))))))
    (inputs
     (list libcap
           libseccomp
           yajl))
    (native-inputs
     (list automake
           autoconf
           git-minimal/pinned
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
    (version "2.1.13")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/containers/conmon")
             (commit (string-append "v" version))))
       (sha256
        (base32 "0fiixv9h5dycrixs7s3bq81d06p7qs8491mskxj42wqlkdq5diay"))
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
    (version "1.8.1.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/89luca89/distrobox")
             (commit version)))
       (sha256
        (base32 "168hjzifdvmlr4rplgp2jh4sg1dwjfmbrwj8589zys8r4kqvwfy1"))
       (file-name (git-file-name name version))))
    (build-system copy-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               ;; This script creates desktop files but when the store path for
               ;; distrobox changes it leaves the stale path on the desktop
               ;; file, so remove the path to use the profile's current
               ;; distrobox.
               (add-after 'unpack 'patch-distrobox-generate-entry
                 (lambda _
                   (substitute* "distrobox-generate-entry"
                     (("\\$\\{distrobox_path\\}/distrobox") "distrobox"))))
               ;; Use WRAP-SCRIPT to wrap all of the scripts of distrobox,
               ;; excluding the host side ones.
               (add-after 'install 'wrap-scripts
                 (lambda _
                   (let ((path (search-path-as-list
                                 (list "bin")
                                 (list #$(this-package-input "podman")
                                       #$(this-package-input "wget")))))
                     (for-each (lambda (script)
                                 (wrap-script
                                   (string-append #$output "/bin/distrobox-"
                                                  script)
                                   `("PATH" ":" prefix ,path)))
                               '("assemble"
                                 "create"
                                 "enter"
                                 "ephemeral"
                                 "generate-entry"
                                 "list"
                                 "rm"
                                 "stop"
                                 "upgrade")))))
               ;; These scripts are used in the container side and the
               ;; /gnu/store path is not shared with the containers.
               (add-after 'patch-shebangs 'unpatch-shebangs
                 (lambda _
                   (for-each (lambda (script)
                               (substitute*
                                 (string-append #$output "/bin/distrobox-"
                                                script)
                                 (("#!.*/bin/sh") "#!/bin/sh\n")))
                             '("export" "host-exec" "init"))))
               (replace 'install
                 (lambda _
                   (invoke "./install" "--prefix" #$output))))))
    (inputs
     (list guile-3.0 ; for wrap-script
           podman
           wget))
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
    (version "1.3.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rootless-containers/slirp4netns")
             (commit (string-append "v" version))))
       (sha256
        (base32 "13lw48b89583smblp2d4kxb1j23k2qx3i2j5zi91sxwnnn4ndks2"))
       (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f ; XXX: open("/dev/net/tun"): No such file or directory
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'fix-hardcoded-paths
                    (lambda _
                      (substitute* (find-files "tests" "\\.sh")
                        (("ping") "/run/privileged/bin/ping")))))))
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
    (version "2024_12_11.09478d5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://passt.top/passt/snapshot/passt-" version
                           ".tar.gz"))
       (sha256
        (base32 "1arkir4784chw9x37174rc12cp353501m43p6iwvk5mqrlq02k90"))))
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
    (version "1.6.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/containernetworking/plugins")
             (commit (string-append "v" version))))
       (sha256
        (base32 "164savm1iic5ax2xi4zgy9lm7wk8kjy22n4is463lj9rkbp4s6xn"))
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
          ;; Add -trimpath flag to avoid keeping references to go package
          ;; in the store.
          (add-after 'unpack 'patch-go-reference
            (lambda _
              (substitute* "Makefile"
                (("go build") "go build -trimpath"))))
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

(define-public catatonit
  (package
    (name "catatonit")
    (version "0.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/openSUSE/catatonit/")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "14vh0xpg6lzmh7r52vi9w1qfc14r7cfhfrbca7q5fg62d3hx7kxi"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake libtool))
    (home-page "https://github.com/openSUSE/catatonit")
    (synopsis "Container init")
    (description
     "Catatonit is a simple container init tool developed as a rewrite of
@url{https://github.com/cyphar/initrs, initrs} in C due to the need for static
compilation of Rust binaries with @code{musl}.  Inspired by other container
inits like @url{https://github.com/krallin/tini, tini} and
@url{https://github.com/Yelp/dumb-init, dumb-init}, catatonit focuses on
correct signal handling, utilizing @code{signalfd(2)} for improved stability.
Its main purpose is to support the key usage by @code{docker-init}:
@code{/dev/init} – <your program>, with minimal additional features planned.")
    (license license:gpl2+)))

(define-public podman
  (package
    (name "podman")
    (version "5.4.2")
    (outputs '("out" "docker"))
    (properties
      `((output-synopsis "docker" "docker alias for podman")))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/containers/podman")
             (commit (string-append "v" version))))
       (sha256
        (base32 "1yjkyn1wrpzfcndds23p0r192l95rq4jcw7w9m2y1zdhafjlgr5y"))
       (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:make-flags
      #~(list (string-append "CC=" #$(cc-for-target))
              (string-append "PREFIX=" #$output)
              (string-append "HELPER_BINARIES_DIR=" #$output "/_guix")
              (string-append "GOMD2MAN="
                             #$go-github-com-go-md2man "/bin/go-md2man")
              (string-append "BUILDFLAGS=-trimpath"))
      #:tests? #f                  ; /sys/fs/cgroup not set up in guix sandbox
      #:test-target "test"
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
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "make" "localsystem")
                (invoke "make" "remotesystem"))))
          (add-after 'unpack 'fix-hardcoded-paths
            (lambda _
              (substitute* "vendor/github.com/containers/common/pkg/config/config_linux.go"
                (("/usr/local/libexec/podman")
                 (string-append #$output "/libexec/podman"))
                (("/usr/local/lib/podman")
                 (string-append #$output "/bin")))))
          (add-after 'install 'symlink-helpers
            (lambda _
              (mkdir-p (string-append #$output "/_guix"))
              (for-each
               (lambda (what)
                 (symlink (string-append (car what) "/bin/" (cdr what))
                          (string-append #$output "/_guix/" (cdr what))))
               ;; Only tools that cannot be discovered via $PATH are
               ;; symlinked.  Rest is handled in the 'wrap-podman phase.
               `((#$aardvark-dns     . "aardvark-dns")
                 ;; Required for podman-machine, which is *not* supported out
                 ;; of the box.  But it cannot be discovered via $PATH, so
                 ;; there is no other way for the user to install it.  It
                 ;; costs ~10MB, so let's leave it here.
                 (#$gvisor-tap-vsock . "gvproxy")
                 (#$netavark         . "netavark")))))
          (add-after 'install 'wrap-podman
            (lambda _
              (wrap-program (string-append #$output "/bin/podman")
                `("PATH" suffix
                  (,(string-append #$catatonit      "/bin")
                   ,(string-append #$conmon         "/bin")
                   ,(string-append #$crun           "/bin")
                   ,(string-append #$gcc            "/bin") ; cpp
                   ,(string-append #$iptables       "/sbin")
                   ,(string-append #$passt          "/bin")
                   ,(string-append #$procps         "/bin") ; ps
                   "/run/privileged/bin")))))
          (add-after 'install 'install-docker
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((docker (assoc-ref outputs "docker")))
                ;; So it picks podman of the other output.
                (let ((bin-dir (string-append (assoc-ref outputs "out")
                                              "/bin")))
                  (substitute* "docker/docker.in"
                   (("[$][{]BINDIR[}]") bin-dir)
                   (("[$][{]ETCDIR[}]") "/etc")))
                (invoke "make" "install.docker"
                        (string-append "PREFIX=" (assoc-ref outputs "docker"))
                        (string-append "ETCDIR=" (string-append (assoc-ref outputs "docker")
                                                                "/etc"))))))
          (add-after 'install 'install-completions
            (lambda _
              (invoke "make" "install.completions"
                      (string-append "PREFIX=" #$output)))))))
    (inputs
     (list bash-minimal
           btrfs-progs
           gpgme
           libassuan
           libseccomp
           libselinux))
    (native-inputs
     (list (package/inherit grep
             (inputs (list pcre2)))     ; Drop once grep on master supports -P
           bats
           git-minimal/pinned
           go-1.22
           go-github-com-go-md2man
           gnu-gettext ; for envsubst
           mandoc
           pkg-config
           python))
    (home-page "https://podman.io")
    (synopsis "Manage containers, images, pods, and their volumes")
    (description
     "Podman (the POD MANager) is a tool for managing containers and images,
volumes mounted into those containers, and pods made from groups of
containers.

Not all commands are working out of the box due to requiring additional
binaries to be present in the $PATH.

To get @code{podman compose} working, install either @code{podman-compose} or
@code{docker-compose} packages.

To get @code{podman machine} working, install @code{qemu-minimal}, and
@code{openssh} packages.")
    (license license:asl2.0)))

(define-public podman-compose
  (package
    (name "podman-compose")
    (version "1.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/containers/podman-compose")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "06vm088q1x7j929n93ylq3bav716bqh6l79agj8sgzsqxjsmli73"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags #~(list "pytests")))
    (native-inputs (list python-pytest python-parameterized python-setuptools python-wheel))
    (propagated-inputs (list python-dotenv python-pyyaml))
    (home-page "https://github.com/containers/podman-compose")
    (synopsis "Script to run docker-compose.yml using podman")
    (description
     "This package provides an implementation of
@url{https://compose-spec.io/, Compose Spec} for @code{podman} focused on
being rootless and not requiring any daemon to be running.")
    (license license:gpl2)))

(define-public buildah
  (package
    (name "buildah")
    (version "1.39.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/containers/buildah")
             (commit (string-append "v" version))))
       (sha256
        (base32 "0b16zhsf9g863wxjvqyapb6wa8dr6r1rwrpffy6hf98fnq38jyh8"))
       (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:make-flags
      #~(list (string-append "CC=" #$(cc-for-target))
              (string-append "PREFIX=" #$output)
              (string-append "GOMD2MAN="
                             #$go-github-com-go-md2man "/bin/go-md2man"))
      #:tests? #f                  ; /sys/fs/cgroup not set up in guix sandbox
      #:test-target "test-unit"
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
          ;; Add -trimpath to build flags to avoid keeping references to go
          ;; packages.
          (add-after 'set-env 'patch-buildflags
            (lambda _
              (substitute* "Makefile"
                (("BUILDFLAGS :=") "BUILDFLAGS := -trimpath "))))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "make" "test-unit")
                (invoke "make" "test-conformance")
                (invoke "make" "test-integration"))))
          (add-after 'install 'symlink-helpers
            (lambda _
              (mkdir-p (string-append #$output "/_guix"))
              (for-each
               (lambda (what)
                 (symlink (string-append (car what) "/bin/" (cdr what))
                          (string-append #$output "/_guix/" (cdr what))))
               ;; Only tools that cannot be discovered via $PATH are
               ;; symlinked.  Rest is handled in the 'wrap-buildah phase.
               `((#$aardvark-dns     . "aardvark-dns")
                 (#$netavark         . "netavark")))))
          (add-after 'install 'wrap-buildah
            (lambda _
              (wrap-program (string-append #$output "/bin/buildah")
                `("CONTAINERS_HELPER_BINARY_DIR" =
                  (,(string-append #$output "/_guix")))
                `("PATH" suffix
                  (,(string-append #$crun           "/bin")
                   ,(string-append #$gcc            "/bin") ; cpp
                   ,(string-append #$passt          "/bin")
                   "/run/privileged/bin")))))
          (add-after 'install 'install-completions
            (lambda _
              (invoke "make" "install.completions"
                      (string-append "PREFIX=" #$output)))))))
    (inputs (list bash-minimal
                  btrfs-progs
                  eudev
                  glib
                  gpgme
                  libassuan
                  libseccomp
                  lvm2))
    (native-inputs
     (list bats
           go-1.22
           go-github-com-go-md2man
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
