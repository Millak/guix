;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 Timmy Douglas <mail@timmydouglas.com>
;;; Copyright © 2022 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2022 Zhu Zihao <all_but_last@163.com>
;;; Copyright © 2022 Michael Rohleder <mike@rohleder.de>
;;; Copyright © 2023 Zongyuan Li <zongyuan.li@c0x0o.me>
;;; Copyright © 2023 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2024, 2025, 2026 Tomas Volf <~@wolfsden.cz>
;;; Copyright © 2024 Foundation Devices, Inc. <hello@foundation.xyz>
;;; Copyright © 2024 Jean-Pierre De Jesus DIAZ <jean@foundation.xyz>
;;; Copyright © 2025 Tomas Volf <~@wolfsden.cz>
;;; Copyright © 2025 Foster Hangdaan <foster@hangdaan.email>
;;; Copyright © 2026 Giacomo Leidi <therewasa@fishinthecalculator.me>
;;; Copyright © 2026 Sharlatan Hellseher <sharlatanus@gmail.com>
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
  #:use-module (guix build-system guile)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system pyproject)
  #:use-module ((guix search-paths) #:select ($GUIX_EXTENSIONS_PATH))
  #:use-module (guix utils)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages docker)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-check)
  #:use-module (gnu packages golang-compression)
  #:use-module (gnu packages golang-crypto)
  #:use-module (gnu packages golang-web)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages man)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages python)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages prometheus)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages selinux)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages virtualization)
  #:use-module (gnu packages web)
  #:use-module (gnu packages wget))

;;; Code:

;;;
;;; Libraries:
;;;

(define-public go-github-com-checkpoint-restore-checkpointctl
  (package
    (name "go-github-com-checkpoint-restore-checkpointctl")
    (version "1.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/checkpoint-restore/checkpointctl")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0qvgld9vji5f7h2idk6r3q30909hqws0rkpvgina43i57bsfh2sv"))
       (snippet
        #~(begin
            (use-modules (guix build utils))
            (delete-file-recursively "vendor")))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:import-path "github.com/checkpoint-restore/checkpointctl"))
    (native-inputs
     (list go-github-com-spf13-cobra))
    (propagated-inputs
     (list go-github-com-checkpoint-restore-go-criu-v8
           go-github-com-containers-storage
           go-github-com-opencontainers-runtime-spec
           go-github-com-xlab-treeprint))
    (home-page "https://github.com/checkpoint-restore/checkpointctl")
    (synopsis "Tool for in-depth analysis of container checkpoints")
    (description
     "This package provides a Go library to read and manipulate checkpoint
archives as created by Podman, CRI-O and containerd.")
    (license license:asl2.0)))

(define-public go-github-com-containerd-containerd-v2
  (package
    (name "go-github-com-containerd-containerd-v2")
    (version "2.2.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/containerd/containerd")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0wxy5np689571s6lw77mx63nw75fx85w5svi0jplksmqzmjqp8wd"))
       (snippet
        #~(begin (use-modules (guix build utils))
         (delete-file-recursively "vendor")
            ;; Submodules with their own go.mod files and packaged separately:
            ;;
            ;; - github.com/containerd/containerd/api
            (delete-file-recursively "api")))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:import-path "github.com/containerd/containerd/v2"
      #:test-subdirs
      #~(list "cmd/protoc-gen-go-fieldpath/..."
              "core/containers/..."
              "core/content/..."
              "core/diff"
              "core/diff/apply"
              "core/events/..."
              "core/images/..."
              "core/introspection/..."
              "core/leases/..."
              "core/metrics/..."
              "core/remotes/..."
              "core/sandbox/..."
              "core/streaming/..."
              "core/transfer"
              "core/transfer/archive/..."
              "core/transfer/image/..."
              "core/transfer/local/..."
              "core/transfer/plugins/..."
              "core/transfer/streaming/..."
              "core/unpack/..."
              "internal/cleanup/..."
              "internal/erofsutils/..."
              "internal/eventq/..."
              "internal/failpoint/..."
              "internal/fsverity/..."
              "internal/kmutex/..."
              "internal/lazyregexp/..."
              "internal/pprof/..."
              "internal/randutil/..."
              "internal/registrar/..."
              "internal/tomlext/..."
              "internal/truncindex/..."
              "internal/userns/..."
              "internal/wintls/..."
              "pkg/apparmor/..."
              "pkg/archive/..."
              "pkg/atomicfile/..."
              "pkg/blockio/..."
              "pkg/cap/..."
              "pkg/cio..."
              "pkg/deprecation/..."
              "pkg/dialer/..."
              "pkg/display/..."
              "pkg/epoch/..."
              "pkg/fifosync/..."
              "pkg/filters/..."
              "pkg/gc/..."
              "pkg/httpdbg/..."

              ;; TODO: Check why these submodule fail to build.
              ;; "client/..."
              ;; "cmd/containerd-shim-runc-v2/..."
              ;; "cmd/containerd-stress/..."
              ;; "cmd/containerd/..."
              ;; "cmd/ctr/..."
              ;; "cmd/gen-manpages/..."
              ;; "contrib/..."
              ;; "core/diff/proxy"
              ;; "core/metadata/..."
              ;; "core/mount/..."
              ;; "core/runtime/..."
              ;; "core/snapshots/..."
              ;; "core/transfer/proxy/..."
              ;; "core/transfer/registry/..."
              ;; "integration/..."
              ;; "internal/cri/..."
              ;; "internal/nri/..."
              ;; "pkg/cdi/..."
              #;"plugins/...")
      #:test-flags
      #~(list "-skip" (string-join
                      ;; panic: cannot statfs cgroup root [recovered]
                      (list "TestValidateConfig"
                            ;; io_test.go:40: failed to start binary process:
                            ;; fork/exec /bin/echo: no such file or directory
                            "TestNewBinaryIO"
                            ;; expected success: got executable file not found in $PATH
                            "TestExecutorWithArgs"
                            "TestSetEnv"
                            "TestStdIOPipes"
                            ;; panic: cannot statfs cgroup root
                            "TestContainerCapabilities"
                            "TestContainerSpecTty"
                            "TestContainerSpecReadonlyRootfs"
                            "TestContainerSpecWithExtraMounts"
                            "TestContainerAndSandboxPrivileged"
                            "TestPrivilegedBindMount"
                            "TestCgroupNamespace"
                            "TestPidNamespace/node_namespace_mode"
                            ;; failed to apply b: invalid argument
                            "TestDiffTar/IgnoreSockets"
                            "TestBinDirVerifyImage/max_verifiers_=_-1,_with_timeout"
                            "TestContainerSpecDefaultPath"
                            ;; Error: Not equal:
                            ;;        expected: 1000
                            ;;        actual  : 123
                            "TestSetPositiveOomScoreAdjustment")
                       "|"))))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-dario-cat-mergo
           go-github-com-adalogics-go-fuzz-headers
           go-github-com-checkpoint-restore-checkpointctl
           go-github-com-checkpoint-restore-go-criu-v7
           go-github-com-containerd-btrfs-v2
           go-github-com-containerd-cgroups-v3
           go-github-com-containerd-console
           go-github-com-containerd-containerd-api
           go-github-com-containerd-continuity
           go-github-com-containerd-errdefs
           go-github-com-containerd-errdefs-pkg
           go-github-com-containerd-fifo
           go-github-com-containerd-go-cni
           go-github-com-containerd-go-runc
           go-github-com-containerd-imgcrypt-v2
           go-github-com-containerd-log
           go-github-com-containerd-nri
           go-github-com-containerd-otelttrpc
           go-github-com-containerd-platforms
           go-github-com-containerd-plugin
           go-github-com-containerd-ttrpc
           go-github-com-containerd-typeurl-v2
           go-github-com-containerd-zfs-v2
           go-github-com-containernetworking-cni
           go-github-com-containernetworking-plugins
           go-github-com-coreos-go-systemd-v22
           go-github-com-davecgh-go-spew
           go-github-com-distribution-reference
           go-github-com-docker-go-events
           go-github-com-docker-go-metrics
           go-github-com-docker-go-units
           go-github-com-emicklei-go-restful-v3
           go-github-com-fsnotify-fsnotify
           go-github-com-google-certtostore
           go-github-com-google-go-cmp
           go-github-com-google-uuid
           go-github-com-grpc-ecosystem-go-grpc-middleware-providers-prometheus
           go-github-com-intel-goresctrl
           go-github-com-klauspost-compress
           go-github-com-mdlayher-vsock
           ;;go-github-com-microsoft-go-winio ;Windows only
           ;;go-github-com-microsoft-hcsshim ;Windows only
           go-github-com-moby-locker
           go-github-com-moby-sys-mountinfo
           go-github-com-moby-sys-sequential
           go-github-com-moby-sys-signal
           go-github-com-moby-sys-symlink
           go-github-com-moby-sys-user
           go-github-com-moby-sys-userns
           go-github-com-opencontainers-go-digest
           go-github-com-opencontainers-image-spec
           go-github-com-opencontainers-runtime-spec
           go-github-com-opencontainers-runtime-tools
           go-github-com-opencontainers-selinux
           go-github-com-pelletier-go-toml-v2
           go-github-com-prometheus-client-golang
           go-github-com-sirupsen-logrus
           go-github-com-tchap-go-patricia-v2
           go-github-com-urfave-cli-v2
           go-github-com-vishvananda-netlink
           go-github-com-vishvananda-netns
           go-go-etcd-io-bbolt
           go-go-opentelemetry-io-contrib-instrumentation-google-golang-org-grpc-otelgrpc
           go-go-opentelemetry-io-contrib-instrumentation-net-http-otelhttp
           go-go-opentelemetry-io-otel
           go-go-opentelemetry-io-otel-exporters-otlp-otlptrace
           go-go-opentelemetry-io-otel-exporters-otlp-otlptrace-otlptracegrpc
           go-go-opentelemetry-io-otel-exporters-otlp-otlptrace-otlptracehttp
           go-go-opentelemetry-io-otel-sdk
           go-go-opentelemetry-io-otel-trace
           go-go-uber-org-goleak
           go-golang-org-x-mod
           go-golang-org-x-sync
           go-golang-org-x-sys
           go-golang-org-x-time
           go-google-golang-org-genproto-googleapis-rpc
           go-google-golang-org-grpc
           go-google-golang-org-protobuf
           go-gopkg-in-inf-v0
           go-k8s-io-apimachinery
           go-k8s-io-client-go
           go-k8s-io-cri-api
           go-k8s-io-klog-v2
           go-tags-cncf-io-container-device-interface))
    (home-page "https://containerd.io/")
    (synopsis "Container runtime support daemon")
    (description
     "Containerd is a container runtime with an emphasis on simplicity,
robustness, and portability.  It is available as a daemon, which can manage
the complete container lifecycle of its host system: image transfer and
storage, container execution and supervision, low-level storage and network
attachments, etc.")
    (license license:asl2.0)))

(define-public go-github-com-containers-gvisor-tap-vsock
  (package
    (name "go-github-com-containers-gvisor-tap-vsock")
    (version "0.8.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/containers/gvisor-tap-vsock")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1xz710dmy58gngd0qizjw8g9nkraksqald8vzhwc5h36dqkc8nrf"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            (delete-file-recursively "vendor")
            ;; Submodules with their own go.mod files and packaged separately:
            ;;
            ;; - github.com/containers/gvisor-tap-vsock/tools
            (delete-file-recursively "tools")))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:import-path "github.com/containers/gvisor-tap-vsock"
      #:unpack-path "github.com/containers/gvisor-tap-vsock"
      #:build-flags
      #~(list (string-append "-ldflags="
                             "-X github.com/containers/gvisor-tap-vsock"
                             "/pkg/types.gitVersion=" #$version))
      #:test-flags
      #~(list "-skip"
              (string-join
               ;; Received unexpected error:
               ;; listen unix /tmp/guix-.../test.sock: bind: invalid argument
               (list "TestNotificationSender_Success"
                     ;; Requires network
                     "TestSuite"
                     "TestDNS")
               "|"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'prune-tests
            (lambda* (#:key unpack-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" unpack-path)
                ;; Requires working DNS.
                (substitute* "pkg/services/dns/dns_test.go"
                  (("Should pass DNS requests to default system DNS.*" all)
                   (string-append all "\n" "ginkgo.Skip(\"No network.\");"))
                  (("\"redhat.com\",")
                   "\"localhost\",")
                  (("\"52.200.142.250\"")
                   "\"127.0.0.1\""))))))))
    (native-inputs
     (list go-github-com-stretchr-testify
           go-github-com-foxcpp-go-mockdns))
    (propagated-inputs
     (list go-github-com-apparentlymart-go-cidr
           go-github-com-containers-winquit
           go-github-com-coreos-stream-metadata-go
           go-github-com-dustin-go-humanize
           go-github-com-google-gopacket
           go-github-com-inetaf-tcpproxy
           go-github-com-insomniacslk-dhcp
           ;; go-github-com-linuxkit-virtsock  ;Windows only
           go-github-com-mdlayher-vsock
           ;; go-github-com-microsoft-go-winio ;Windows only
           go-github-com-miekg-dns
           go-github-com-onsi-ginkgo
           go-github-com-onsi-gomega
           go-github-com-opencontainers-go-digest
           go-github-com-sirupsen-logrus
           go-github-com-songgao-packets
           go-github-com-songgao-water
           go-github-com-vishvananda-netlink
           go-golang-org-x-crypto
           go-golang-org-x-mod
           go-golang-org-x-sync
           go-golang-org-x-sys
           go-gopkg-in-yaml-v3
           go-gvisor-dev-gvisor))
    (home-page "https://github.com/containers/gvisor-tap-vsock")
    (synopsis "Network stack for virtualization based on gVisor")
    (description "This package provides a replacement for @code{libslirp} and
@code{VPNKit}, written in pure Go.  It is based on the network stack of gVisor
and brings a configurable DNS server and dynamic port forwarding.

It can be used with QEMU, Hyperkit, Hyper-V and User-Mode Linux.

The binary is called @command{gvproxy}.")
    (license license:asl2.0)))

(define-public go-github-com-rootless-containers-rootlesskit-v3
  (package
    (name "go-github-com-rootless-containers-rootlesskit-v3")
    (version "3.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/rootless-containers/rootlesskit")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0giw1whjpm64h8f1iamgym246rr3wl01w7zgw4lygrj7dqk3clmb"))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:import-path "github.com/rootless-containers/rootlesskit/v3"))
    (propagated-inputs
     (list go-github-com-containernetworking-plugins
           go-github-com-containers-gvisor-tap-vsock
           go-github-com-gofrs-flock
           go-github-com-google-uuid
           go-github-com-gorilla-mux
           go-github-com-insomniacslk-dhcp
           go-github-com-masterminds-semver-v3
           go-github-com-moby-sys-mountinfo
           go-github-com-moby-vpnkit
           go-github-com-sirupsen-logrus
           go-github-com-songgao-water
           go-github-com-urfave-cli-v2
           go-golang-org-x-sync
           go-golang-org-x-sys
           go-gotest-tools-v3))
    (home-page "https://github.com/rootless-containers/rootlesskit")
    (synopsis "Linux-native fakeroot using user namespaces in Golang")
    (description
     "@code{RootlessKit} is a Linux-native implementation of \"fake root\" using
@url{http://man7.org/linux/man-pages/man7/user_namespaces.7.html,(code
user_namespaces(7))}.  It is used to run containers engines as an
unprivileged user, known as \"Rootless mode\".")
    (license license:asl2.0)))



;;;
;;; Executables:
;;;

(define-public checkpointctl
  (package/inherit go-github-com-checkpoint-restore-checkpointctl
    (name "checkpointctl")
    (arguments
     (substitute-keyword-arguments
         (package-arguments go-github-com-checkpoint-restore-checkpointctl)
       ((#:build-flags _) #~(list (string-append "-X main.version="
                                                 #$version)))
       ((#:install-source? _ #t) #f)
       ((#:skip-build? _ #t) #f)
       ((#:tests? _ #t) #f)))
    (native-inputs
     (append
      (package-native-inputs go-github-com-checkpoint-restore-checkpointctl)
      (package-propagated-inputs go-github-com-checkpoint-restore-checkpointctl)))
    (propagated-inputs '())
    (inputs '())
    (description
     "This package provides a tool to read and manipulate checkpoint archives
as created by Podman, CRI-O and containerd.")))

(define-public crun
  (package
    (name "crun")
    (version "1.27.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/containers/crun/releases/download/"
             version
             "/crun-" version ".tar.gz"))
       (sha256
        (base32
         "1w782s95f3xvw3fb66l2ciqkqsg4bk7n9ph31jmvn669ap272ymy"))))
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
    (version "2.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/containers/conmon")
              (commit (string-append "v" version))))
       (sha256
        (base32 "0n9l6030ibhk7pmsq85rarcf9b0kzglxibd1xnb6vzmkz3ywg1il"))
       (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (arguments
     (list #:make-flags
           #~(list (string-append "CC=" #$(cc-for-target))
                   (string-append "PREFIX=" #$output))
           #:test-target "test"
           #:phases
           #~(modify-phases %standard-phases
               (delete 'configure)
               (add-before 'check 'prepare-tests
                 (lambda* (#:key inputs #:allow-other-keys)
                   (setenv "RUNTIME_BINARY"
                           (search-input-file inputs "sbin/runc"))

                   ;; We need to skip all tests requiring journald.
                   (for-each
                    (lambda (test file)
                      (substitute* file
                        (((string-append "@test \"" test "\" \\{\n$") all)
                         (string-append all "skip 'no journald in Guix';"))))
                    '("log driver as journald should pass"
                      "log driver as journald with short cid should fail"
                      "multiple log drivers should pass"
                      "log management: should work with multiple log drivers")
                    '("test/01-basic.bats"
                      "test/01-basic.bats"
                      "test/01-basic.bats"
                      "test/06-log-management.bats")))))))
    (inputs
     (list crun
           glib
           libseccomp))
    (native-inputs
     (list bats
           git
           go-md2man
           pkg-config
           socat
           runc))
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
    (version "1.8.2.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/89luca89/distrobox")
             (commit version)))
       (sha256
        (base32 "07kqgr5diwvkks3fn1r0nnpfqq6gngqyx4x7lxs06ri6g0a4knvf"))
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
    (home-page "https://distrobox.it")
    (synopsis "Create and start containers highly integrated with the hosts")
    (description
     "Distrobox is a fancy wrapper around Podman or Docker to create and start
containers highly integrated with the hosts.")
    (license license:gpl3)))

(define-public dive
  (package
    (name "dive")
    (version "0.12.0") ;newer version needs docker/docker@28+
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/wagoodman/dive")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0p60bq0lc820p7x3nq8kxc8cx646c0z7zxqc7vav77zc4qbm3r8a"))))
    (build-system go-build-system)
    (arguments
     (list
      #:install-source? #f
      #:build-flags
      #~(list (string-append "-ldflags=-X main.version=" #$version))
      #:import-path "github.com/wagoodman/dive"
      #:test-flags #~(list "-vet=off")))
    (native-inputs
     (list go-github-com-awesome-gocui-gocui
           go-github-com-awesome-gocui-keybinding
           go-github-com-cespare-xxhash
           go-github-com-docker-cli
           go-github-com-docker-docker
           go-github-com-dustin-go-humanize
           go-github-com-fatih-color
           go-github-com-google-uuid
           go-github-com-logrusorgru-aurora
           go-github-com-lunixbochs-vtclean
           go-github-com-mitchellh-go-homedir
           go-github-com-phayes-permbits
           go-github-com-sergi-go-diff
           go-github-com-sirupsen-logrus
           go-github-com-spf13-afero
           go-github-com-spf13-cobra
           go-github-com-spf13-viper
           go-golang-org-x-net))
    (home-page "https://github.com/wagoodman/dive")
    (synopsis "Tool for exploring each layer in a docker image")
    (description
     "This package provides a tool for exploring a Docker image, layer
contents, and discovering ways to shrink the size of Docker/OCI image.")
    (license license:expat)))

(define-public guix-compose
  (package
    (name "guix-compose")
    (version "0.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://codeberg.org/fishinthecalculator/guix-compose")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1dy48qkz3ifxagijpvwg7rmq7hz3pikhdcfral6djyc9ppmz8mbm"))))
    (build-system guile-build-system)
    (arguments
     (list
      #:source-directory "src"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'set-load-paths-in-entry-point
            (lambda _
              (define load-path
                (cons (string-append #$output "/share/guile/site/3.0")
                      (parse-path (getenv "GUILE_LOAD_PATH"))))
              (define load-compiled-path
                (cons (string-append #$output "/lib/guile/3.0/site-ccache")
                      (parse-path (getenv "GUILE_LOAD_COMPILED_PATH"))))
              (define search-paths-header
                `(begin
                   (set! %load-path
                         (append (list ,@load-path) %load-path))
                   (set! %load-compiled-path
                         (append (list ,@load-compiled-path)
                                 %load-compiled-path))))

              (substitute* "src/guix/extensions/compose.scm"
                ((";;@load-paths@")
                 (with-output-to-string
                   (lambda () (write search-paths-header)))))))
          (add-after 'build 'add-extension-to-search-path
            (lambda _
              (with-directory-excursion #$output
                (mkdir-p "share/guix/extensions")
                (symlink
                 (string-append
                  #$output "/share/guile/site/3.0/guix/extensions/compose.scm")
                 "share/guix/extensions/compose.scm"))))
          (add-after 'add-extension-to-search-path 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke
                 "guile" "-L" "./modules" "-s" "tests/test-compose.scm")))))))
    (native-inputs (list guile-3.0))
    ;; Avoid setting propagated so that we use the user’s profile.
    (inputs (list guix guile-dotenv guile-yamlpp))
    (native-search-paths
     (list $GUIX_EXTENSIONS_PATH))
    (synopsis "Guix' docker compose compatibility layer")
    (description "A toolkit to run, read and write docker-compose.yml files with
Guix machinery.")
    (home-page "https://codeberg.org/fishinthecalculator/guix-compose")
    (license license:gpl3+)))

(define-public libslirp
  (package
    (name "libslirp")
    (version "4.9.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.freedesktop.org/slirp/libslirp")
             (commit (string-append "v" version))))
       (sha256
        (base32 "1jlqda5k5wm0ql0fymkr3dbf1d71gxcj7896r8mz8s1i264gg8rh"))
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
    (version "1.3.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rootless-containers/slirp4netns")
             (commit (string-append "v" version))))
       (sha256
        (base32 "165z1ccsb8w901965rlzcrbln17l1jdg9k7vsiamlx0q06v24b96"))
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
    (version "1.9.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/containernetworking/plugins")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "12z6w2jk6xgfiwdxys7skpkxldz1cgaa7scgfcr90lsghay59s6w"))
       (snippet
        #~(begin (use-modules (guix build utils))
                 (delete-file-recursively "vendor")))))
    (build-system go-build-system)
    (arguments
     (list
      #:install-source? #f
      ;; XXX: Tests require root access, see test_linux.sh.
      #:tests? #f
      #:import-path "github.com/containernetworking/plugins/plugins/..."
      #:unpack-path "github.com/containernetworking/plugins"))
    (native-inputs
     (list go-github-com-alexflint-go-filemutex
           go-github-com-buger-jsonparser
           go-github-com-containernetworking-cni
           go-github-com-coreos-go-iptables
           go-github-com-coreos-go-systemd-v22
           go-github-com-godbus-dbus-v5
           go-github-com-insomniacslk-dhcp
           go-github-com-mattn-go-shellwords
           ;; go-github-com-microsoft-hcsshim
           go-github-com-networkplumbing-go-nft
           go-github-com-onsi-ginkgo-v2
           go-github-com-onsi-gomega
           go-github-com-opencontainers-selinux
           go-github-com-pkg-errors
           go-github-com-safchain-ethtool
           go-github-com-vishvananda-netlink
           go-github-com-vishvananda-netns
           go-golang-org-x-sys
           go-sigs-k8s-io-knftables
           util-linux))
    (home-page "https://github.com/containernetworking/plugins")
    (synopsis "Container Network Interface (CNI) network plugins")
    (description
     "This package provides Container Network Interface (CNI) plugins to
configure network interfaces in Linux containers.")
    (license license:asl2.0)))

(define-public gvisor-tap-vsock
  (package/inherit go-github-com-containers-gvisor-tap-vsock
    (name "gvisor-tap-vsock")
    (arguments
     (substitute-keyword-arguments arguments
       ((#:install-source? _ #t) #f)
       ((#:skip-build? _ #t) #f)
       ((#:tests? _ #t) #f)
       ((#:phases _ '%standard-phases)
        #~(modify-phases %standard-phases
            ;; Build binary outputs are taken from project's Makefile.
            (replace 'build
              (lambda arguments
                (for-each
                 (lambda (cmd)
                   (apply (assoc-ref %standard-phases 'build)
                          `(,@arguments #:import-path ,cmd)))
                 (list "github.com/containers/gvisor-tap-vsock/cmd/gvproxy"
                       "github.com/containers/gvisor-tap-vsock/cmd/qemu-wrapper"
                       "github.com/containers/gvisor-tap-vsock/cmd/vm"))))
            (add-after 'install 'fix-bin-name
              (lambda _
                (rename-file (string-append #$output "/bin/vm")
                             (string-append #$output "/bin/gvforwarder"))))))))
    (native-inputs
     (package-propagated-inputs go-github-com-containers-gvisor-tap-vsock))
    (propagated-inputs '())
    (inputs '())))

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
    (version "5.8.2")
    (outputs '("out" "docker"))
    (properties
     `((output-synopsis "docker" "docker alias for podman")
       ;; XXX: Addressed already.
       (lint-hidden-cve . ("CVE-2022-2989"))))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/containers/podman")
             (commit (string-append "v" version))))
       (sha256
        (base32 "12bmzbfyjsf0mwnam38cw9ib54wj1znh5b7lxzdyll9cvvkhqisr"))
       (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:make-flags
      #~(list (string-append "CC=" #$(cc-for-target))
              (string-append "PREFIX=" #$output)
              (string-append "HELPER_BINARIES_DIR=" #$output "/_guix")
              (string-append "GOMD2MAN=" #$go-md2man "/bin/go-md2man")
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
              (substitute* "vendor/go.podman.io/common/pkg/config/config_linux.go"
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
                   ,(string-append #$nftables       "/sbin")
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
                        (string-append "ETCDIR="
                                       (string-append (assoc-ref outputs "docker")
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
     (list grep
           bats
           git-minimal/pinned
           go-1.24
           go-md2man
           gettext-minimal ; for envsubst
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
    (version "1.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/containers/podman-compose")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "08pz9axvgfyr0jd8rlndmhh8147s864mz17ng6qs07831g9ylj80"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; Only run tests in `tests/unit`, skipping the ones in
      ;; `tests/integration`. The integration tests need an environment with
      ;; the ability to manage containers and volumes using the `podman`
      ;; command.
      ;;
      ;; tests: 378 tests
      #:test-backend #~'unittest
      #:test-flags #~(list "discover" "tests/unit")))
    (native-inputs
     (list python-parameterized
           python-setuptools))
    (propagated-inputs
     (list python-dotenv
           python-pyyaml))
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
    (version "1.43.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/containers/buildah")
             (commit (string-append "v" version))))
       (sha256
        (base32 "00x4ja5n49jmkmiv54mmkihks9pnjnna7s7py57in6pc9g85xj2y"))
       (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:make-flags
      #~(list (string-append "CC=" #$(cc-for-target))
              (string-append "PREFIX=" #$output)
              (string-append "GOMD2MAN=" #$go-md2man "/bin/go-md2man"))
      #:tests? #f                  ; /sys/fs/cgroup not set up in guix sandbox
      #:test-target "test-unit"
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (add-after 'unpack 'set-env
            (lambda _
              ;; When running go, things fail because HOME=/homeless-shelter.
              (setenv "HOME" "/tmp")))
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
           go-1.24
           go-md2man
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
