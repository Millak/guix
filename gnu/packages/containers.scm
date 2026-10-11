;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 David Thompson <davet@gnu.org>
;;; Copyright © 2018 宋文武 <iyzsong@envs.net>
;;; Copyright © 2018, 2023 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2019 Leo Famulari <leo@famulari.name>
;;; Copyright © 2019, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019, 2020, 2021, 2023 Maxim Cournoyer <maxim@guixotic.coop>
;;; Copyright © 2018, 2019, 2021, 2022, 2024 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020 Jelle Licht <jlicht@fsfe.org>
;;; Copyright © 2020 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2020 Michael Rohleder <mike@rohleder.de>
;;; Copyright © 2020 Katherine Cox-Buday <cox.katherine.e@gmail.com>
;;; Copyright © 2020 Jesse Dowell <jessedowell@gmail.com>
;;; Copyright © 2021 Léo Le Bouter <lle-bout@zaclys.net>
;;; Copyright © 2021 Maxim Cournoyer <maxim@guixotic.coop>
;;; Copyright © 2021 Timmy Douglas <mail@timmydouglas.com>
;;; Copyright © 2021, 2022 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2022 Michael Rohleder <mike@rohleder.de>
;;; Copyright © 2022 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2022 Zhu Zihao <all_but_last@163.com>
;;; Copyright © 2022 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2023 Hilton Chain <hako@ultrarare.space>
;;; Copyright © 2023 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2023 Zongyuan Li <zongyuan.li@c0x0o.me>
;;; Copyright © 2024 Ashish SHUKLA <ashish.is@lostca.se>
;;; Copyright © 2024 Foundation Devices, Inc. <hello@foundation.xyz>
;;; Copyright © 2024 Nicolas Graves <ngraves@ngraves.fr>
;;; Copyright © 2024 Jean-Pierre De Jesus DIAZ <jean@foundation.xyz>
;;; Copyright © 2024-2026 Sharlatan Hellseher <sharlatanus@gmail.com>
;;; Copyright © 2024-2026 Tomas Volf <~@wolfsden.cz>
;;; Copyright © 2025, 2026 Foster Hangdaan <foster@hangdaan.email>
;;; Copyright © 2025 Artyom V. Poptsov <poptsov.artyom@gmail.com>
;;; Copyright © 2025 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2025 John Kehayias <john.kehayias@protonmail.com>
;;; Copyright © 2025 Arthur Rodrigues <arthurhdrodrigues@proton.me>
;;; Copyright © 2026 Giacomo Leidi <therewasa@fishinthecalculator.me>
;;; Copyright © 2026 Konstantin Suntsov <protvin@disroot.org>
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
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
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
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-check)
  #:use-module (gnu packages golang-compression)
  #:use-module (gnu packages golang-crypto)
  #:use-module (gnu packages golang-maths)
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
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages selinux)
  #:use-module (gnu packages version-control)
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

(define-public go-github-com-compose-spec-compose-go-v2
  (package
    (name "go-github-com-compose-spec-compose-go-v2")
    (version "2.9.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/compose-spec/compose-go")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "18z626vjd0cbqs130nsqhx0vfr6hxarvwgrw5acydy7hfv594aiq"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            ;; Build fails when this file is kept: Code in directory
            ;; /tmp/<...>/src/github.com/compose-spec/compose-go/v2 expects
            ;; import "github.com/compose-spec/compose-go"
            (delete-file "package.go")))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:embed-files
      #~(list "applicator"
              "content"
              "core"
              "format"
              "format-annotation"
              "format-assertion"
              "meta-data"
              "schema"
              "unevaluated"
              "validation")
      #:import-path "github.com/compose-spec/compose-go/v2"
      #:test-flags
      #~(list "-skip" (string-join
                       ;; error decoding 'services[test].environment':
                       ;; environment variable DEBUG is declared with a
                       ;; trailing space
                       (list "TestEnvironmentWhitespace"
                             ;; ports_test.go:80: assertion failed: expected
                             ;; error "Invalid ip address: 127.0.1", got
                             ;; "invalid IP address: 127.0.1"
                             "Test_transformPorts/invalid_IP"
                             ;; types_test.go:190: assertion failed: expected
                             ;; error "Invalid containerPort: 9999999", got
                             ;; "invalid containerPort: 9999999"
                             "TestParsePortConfig")
                       "|"))))
    (native-inputs
     (list go-github-com-google-go-cmp
           go-github-com-stretchr-testify
           go-gotest-tools-v3))
    (propagated-inputs
     (list go-github-com-distribution-reference
           go-github-com-docker-go-connections
           go-github-com-docker-go-units
           go-github-com-go-viper-mapstructure-v2
           go-github-com-mattn-go-shellwords
           go-github-com-opencontainers-go-digest
           go-github-com-santhosh-tekuri-jsonschema-v6
           go-github-com-sirupsen-logrus
           go-github-com-xhit-go-str2duration-v2
           go-go-yaml-in-yaml-v3
           go-golang-org-x-sync
           go-golang-org-x-text))
    (home-page "https://compose-spec.io/")
    (synopsis "Reference library for parsing and loading Compose YAML files")
    (description
     "This package provides a Golang reference library for parsing and
loading Compose files as specified by the
@url{https://github.com/compose-spec/compose-spec, Compose specification}.")
    (license license:asl2.0)))

(define-public go-github-com-containerd-accelerated-container-image
  (package
    (name "go-github-com-containerd-accelerated-container-image")
    (version "1.4.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/containerd/accelerated-container-image")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "086ywdk8mnqnjj3a07ggcyf52cqqsr8cbx2iw2qrncd1slb9l0vv"))
       (modules '((guix build utils)))
        (snippet
         #~(begin
            ;; It requires Windows-only packages in check phase
            ;; (go-github-com-microsoft-hcsshim)
            (delete-file-recursively "cmd/ctr")))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:import-path "github.com/containerd/accelerated-container-image"
      #:test-flags
      ;; Requires network connection
      #~(list "-skip" "TestConvertReferrer")))
    (native-inputs
     (list go-github-com-containerd-log
           go-github-com-prometheus-client-golang
           go-github-com-sirupsen-logrus
           go-github-com-spf13-cobra
           go-github-com-urfave-cli-v2))
    (propagated-inputs
     (list go-github-com-containerd-containerd-api
           go-github-com-containerd-containerd-v2
           go-github-com-containerd-continuity
           go-github-com-containerd-errdefs
           go-github-com-containerd-go-cni
           go-github-com-containerd-platforms
           go-github-com-data-accelerator-zdfs
           go-github-com-docker-go-units
           go-github-com-go-sql-driver-mysql
           go-github-com-moby-locker
           go-github-com-moby-sys-mountinfo
           go-github-com-opencontainers-go-digest
           go-github-com-opencontainers-image-spec
           go-github-com-opencontainers-runtime-spec
           go-golang-org-x-sync
           go-golang-org-x-sys
           go-google-golang-org-grpc
           go-oras-land-oras-go-v2))
    (home-page "https://github.com/containerd/accelerated-container-image")
    (synopsis "Remote container image format")
    (description
     "Accelerated Container Image is an implementation of paper
@url{https://www.usenix.org/conference/atc20/presentation/li-huiba, DADI:
Block-Level Image Service for Agile and Elastic Application Deployment. USENIX
ATC'20}.")
    (license license:asl2.0)))

(define-public go-github-com-containerd-accelerated-container-image-pkg-types
  ;; Submodule to break cycle in github.com/data-accelerator/zdfs.
  (hidden-package
   (package
     (name "go-github-com-containerd-accelerated-container-image-pkg-types")
     (version "1.4.3")
     (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/containerd/accelerated-container-image")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "086ywdk8mnqnjj3a07ggcyf52cqqsr8cbx2iw2qrncd1slb9l0vv"))
        (modules '((guix build utils)
                   (ice-9 ftw)
                   (srfi srfi-26)))
        (snippet
         #~(begin
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
             (delete-all-but "." "pkg")
             (delete-all-but "pkg" "types")))))
     (build-system go-build-system)
     (arguments
      (list
       #:skip-build? #t
       #:tests? #f
       #:import-path "github.com/containerd/accelerated-container-image"))
     (home-page "https://github.com/containerd/accelerated-container-image")
     (synopsis "Remote container image format types")
     (description
      "This packages provides types from
@url{https://github.com/containerd/accelerated-container-image}.")
     (license license:asl2.0))))

(define-public go-github-com-containerd-aufs
  (package
    (name "go-github-com-containerd-aufs")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/containerd/aufs")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0jyyyf6sr910m602axmp4h4j1l2n680cpp60z09pvprz55zi4ba0"))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t  ;source only package
      #:tests? #f
      #:import-path "github.com/containerd/aufs"))
    (propagated-inputs
     (list ;; go-github-com-containerd-containerd       ; cycles
           go-github-com-containerd-continuity
           go-github-com-pkg-errors
           go-golang-org-x-sys))
    (home-page "https://github.com/containerd/aufs")
    (synopsis "AUFS snapshotter containerd v1")
    (description
     "This package provides an @acronym{Advanced multi-layered Unification
FilesyStem, AUFS} implementation of the snapshot interface for containerd.")
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

(define-public go-github-com-containerd-stargz-snapshotter
  (package
    (name "go-github-com-containerd-stargz-snapshotter")
    (version "0.18.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/containerd/stargz-snapshotter")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "06nksi5xpbys6bfqsfm7bax9lxnwynl7y3hw6pcxhv5hckxnb8fp"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            ;; Submodules with their own go.mod files and packaged separately:
            (delete-file-recursively "cmd")
            (delete-file-recursively "estargz")
            (delete-file-recursively "ipfs")))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:import-path "github.com/containerd/stargz-snapshotter"
      ;; TODO: Remove when all transitive inputs are packaged.
      #:test-subdirs
      #~(list "fs" "task" "cache" "estargz" "fs/layer" "fs/reader"
              "fs/remote" "util/cacheutil" "metadata/memory"
              "analyzer/recorder" "estargz/errorutil" "estargz/externaltoc"
              "estargz/zstdchunked" "util/decompressutil")))
    (propagated-inputs
     (list go-github-com-containerd-console
           go-github-com-containerd-containerd-v2
           go-github-com-containerd-continuity
           go-github-com-containerd-errdefs
           go-github-com-containerd-log
           go-github-com-containerd-platforms
           go-github-com-containerd-plugin
           go-github-com-distribution-reference
           go-github-com-docker-cli
           go-github-com-docker-go-metrics
           go-github-com-gogo-protobuf
           go-github-com-golang-groupcache
           go-github-com-hanwen-go-fuse-v2
           go-github-com-hashicorp-go-retryablehttp
           go-github-com-klauspost-compress
           go-github-com-moby-sys-mountinfo
           go-github-com-opencontainers-go-digest
           go-github-com-opencontainers-image-spec
           go-github-com-opencontainers-runtime-spec
           go-github-com-prometheus-client-golang
           go-github-com-rs-xid
           go-github-com-sirupsen-logrus
           go-go-etcd-io-bbolt
           go-golang-org-x-sync
           go-golang-org-x-sys
           go-google-golang-org-grpc
           go-k8s-io-api
           go-k8s-io-apimachinery
           go-k8s-io-client-go
           go-k8s-io-cri-api))
    (home-page "https://github.com/containerd/stargz-snapshotter")
    (synopsis "Fast container image distribution plugin with lazy pulling")
    (description
     "This package provides a container image distribution plugin with lazy
pulling for Containerd implemented in @code{eStargz} - Standard-Compatible
Extensions to Tar.gz Layers for Lazy Pulling Container Images.")
    (license license:asl2.0)))

(define-public go-github-com-containers-gvisor-tap-vsock
  (package
    (name "go-github-com-containers-gvisor-tap-vsock")
    (version "0.8.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/containers/gvisor-tap-vsock")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1knivsg39x46i4zhbz7cjrcykdkg4xwqy0qf24zyd022hzjznykr"))
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
           go-gvisor-dev-gvisor-source))
    (home-page "https://github.com/containers/gvisor-tap-vsock")
    (synopsis "Network stack for virtualization based on gVisor")
    (description "This package provides a replacement for @code{libslirp} and
@code{VPNKit}, written in pure Go.  It is based on the network stack of gVisor
and brings a configurable DNS server and dynamic port forwarding.

It can be used with QEMU, Hyperkit, Hyper-V and User-Mode Linux.

The binary is called @command{gvproxy}.")
    (license license:asl2.0)))

(define-public go-github-com-data-accelerator-zdfs
  (package
    (name "go-github-com-data-accelerator-zdfs")
    (version "0.1.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/data-accelerator/zdfs")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0j2xr9li2qciqdi6is82aw3fx1lm673bfddgfg3zhbpx3r95mwsy"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/data-accelerator/zdfs"))
    (native-inputs
     (list go-github-com-stretchr-testify
           go-github-com-containerd-accelerated-container-image-pkg-types))
    (propagated-inputs
     (list go-github-com-containerd-containerd-v2
           go-github-com-containerd-continuity
           go-github-com-distribution-reference
           go-github-com-pkg-errors
           go-github-com-sirupsen-logrus))
    (home-page "https://github.com/data-accelerator/zdfs")
    (synopsis "Extension package of Overlaybd-snapshotter")
    (description
     "This package provides an extension overlaybd-snapshotter.  It constructs
the overlaybd image in OCIv1 tgz format through a tricky method which makes
'overlaybd-snapshotter' adapter for a normal OCIv1 image.")
    (license license:asl2.0)))

(define-public go-github-com-docker-docker
  (package
    (name "go-github-com-docker-docker")
    (version "28.5.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/moby/moby")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1jy92qqpdj78af8m0qy0agykhmc9h0apx0lfxc8x6mcbakbg772g"))
       (snippet
        #~(begin (use-modules (guix build utils))
                 (delete-file-recursively "vendor")))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/docker/docker"
      #:skip-build? #t
      ;; These tests open SCTP sockets, which fails with "protocol not
      ;; supported" on a kernel built without SCTP; the build environment
      ;; cannot rely on it.
      #:test-flags
      #~(list "-skip" (string-join
                       (list "TestSCTP4ProxyNoListener"
                             "TestSCTP6ProxyNoListener")
                       "|"))
      #:test-subdirs
      ;; XXX: Remove when all inputs are packaged.
      ;;
      ;; daemon/graphdriver/btrfs is left out: its tests exercise the driver
      ;; on the file system holding the build directory, so on btrfs they
      ;; activate and fail because the build user cannot chown, and on any
      ;; other file system they merely skip.
      #~(list "oci" "opts" "image" "layer" "quota" "client" "plugin" "errdefs"
              "registry" "testutil" "pkg/pools" "pkg/stack" "plugin/v2"
              "reference" "runconfig" "pkg/system" "pkg/tarsum" "image/cache"
              "pkg/homedir" "pkg/idtools" "pkg/ioutils" "pkg/meminfo"
              "pkg/parsers" "pkg/pidfile" "pkg/process" "pkg/stdcopy"
              "pkg/sysinfo" "daemon/links" "internal/mod" "pkg/longpath"
              "pkg/progress" "pkg/stringid" "pkg/tailfile" "volume/local"
              "daemon/events" "daemon/logger" "dockerversion" "internal/opts"
              "pkg/fileutils" "pkg/useragent" "volume/mounts"
              "api/types/time" "daemon/network" "restartmanager"
              "volume/drivers" "volume/service" "pkg/jsonmessage"
              "cmd/docker-proxy" "cmd/dockerd/trap" "container/stream"
              "internal/ioutils" "libnetwork/types" "api/types/filters"
              "api/types/network" "cmd/dockerd/debug" "distribution/xfer"
              "internal/cleanups" "internal/platform" "libnetwork/bitmap"
              "libnetwork/config" "libnetwork/ipbits" "pkg/authorization"
              "api/types/registry" "api/types/strslice" "api/types/versions"
              "daemon/graphdriver" "integration/plugin"
              "internal/directory" "internal/sliceutil"
              "internal/usergroup" "libnetwork/options"
              "pkg/namesgenerator" "pkg/parsers/kernel"
              "registry/resumable" "api/types/container"
              "daemon/logger/local" "internal/lazyregexp"
              "internal/multierror" "libcontainerd/queue"
              "libnetwork/etchosts" "libnetwork/netlabel"
              "pkg/streamformatter" "api/server/httputils"
              "daemon/logger/splunk" "daemon/logger/syslog"
              "internal/containerfs" "libnetwork/datastore"
              "libnetwork/driverapi" "libnetwork/networkdb"
              "api/server/middleware" "daemon/logger/awslogs"
              "distribution/metadata" "libnetwork/ipams/null"
              "libnetwork/osl/kernel" "pkg/plugins/transport"
              "daemon/logger/journald" "libnetwork/drvregistry"
              "api/server/router/swarm" "daemon/graphdriver/copy"
              "daemon/logger/templates" "libnetwork/drivers/host"
              "libnetwork/drivers/null" "api/server/router/system"
              "api/server/router/volume" "libnetwork/portallocator"
              "daemon/logger/jsonfilelog"
              "daemon/logger/loggerutils" "libnetwork/drivers/ipvlan"
              "pkg/plugins/pluginrpc-gen" "container/stream/bytespipe"
              "libnetwork/drivers/macvlan" "libnetwork/drivers/overlay"
              "libnetwork/internal/caller" "daemon/graphdriver/overlay2"
              "libnetwork/internal/addrset" "pkg/parsers/operatingsystem"
              "daemon/internal/capabilities" "libnetwork/ipams/defaultipam"
              "builder/remotecontext/urlutil" "libnetwork/internal/netiputil"
              "libnetwork/internal/setmatrix"
              "libnetwork/internal/resolvconf"
              "daemon/internal/filedescriptors"
              "daemon/logger/loggerutils/cache"
              "daemon/graphdriver/fuse-overlayfs"
              "daemon/logger/jsonfilelog/jsonlog"
              "libnetwork/drivers/overlay/ovmanager"
              "daemon/logger/journald/internal/export"
              "integration/plugin/logging/cmd/discard"
              "libnetwork/drivers/overlay/overlayutils")))
    (native-inputs
     (list go-github-com-google-go-cmp
           go-github-com-spf13-cobra
           go-github-com-spf13-pflag))
    (propagated-inputs
     (list go-cloud-google-com-go-compute-metadata
           go-cloud-google-com-go-logging
           go-code-cloudfoundry-org-clock
           go-dario-cat-mergo
           go-github-com-adalogics-go-fuzz-headers
           ;; go-github-com-azure-go-ansiterm   ;Windows only
           go-github-com-graylog2-go-gelf
           ;; go-github-com-microsoft-go-winio  ;Windows only
           ;; go-github-com-microsoft-hcsshim   ;Windows only
           go-github-com-racksec-srslog
           go-github-com-aws-aws-sdk-go-v2
           go-github-com-aws-aws-sdk-go-v2-config
           go-github-com-aws-aws-sdk-go-v2-credentials
           go-github-com-aws-aws-sdk-go-v2-feature-ec2-imds
           go-github-com-aws-aws-sdk-go-v2-service-cloudwatchlogs
           go-github-com-aws-smithy-go
           go-github-com-cloudflare-cfssl
           go-github-com-containerd-cgroups-v3
           go-github-com-containerd-containerd-api
           go-github-com-containerd-containerd-v2
           go-github-com-containerd-continuity
           go-github-com-containerd-errdefs
           go-github-com-containerd-errdefs-pkg
           go-github-com-containerd-fifo
           go-github-com-containerd-log
           go-github-com-containerd-platforms
           go-github-com-containerd-typeurl-v2
           go-github-com-coreos-go-systemd-v22
           go-github-com-cpuguy83-tar2go
           go-github-com-creack-pty
           go-github-com-deckarep-golang-set-v2
           go-github-com-distribution-reference
           go-github-com-docker-distribution
           go-github-com-docker-go-connections
           go-github-com-docker-go-events
           go-github-com-docker-go-metrics
           go-github-com-docker-go-units
           go-github-com-fluent-fluent-logger-golang
           go-github-com-godbus-dbus-v5
           go-github-com-gogo-protobuf
           go-github-com-golang-protobuf
           go-github-com-google-uuid
           go-github-com-gorilla-mux
           go-github-com-hashicorp-go-immutable-radix-v2
           go-github-com-hashicorp-go-memdb
           go-github-com-hashicorp-go-multierror
           go-github-com-hashicorp-memberlist
           go-github-com-hashicorp-serf
           go-github-com-ishidawataru-sctp
           go-github-com-miekg-dns
           go-github-com-mistifyio-go-zfs-v3
           go-github-com-mitchellh-copystructure
           go-github-com-moby-docker-image-spec
           go-github-com-moby-go-archive
           go-github-com-moby-ipvs
           go-github-com-moby-locker
           go-github-com-moby-patternmatcher
           go-github-com-moby-profiles-apparmor
           go-github-com-moby-profiles-seccomp
           go-github-com-moby-pubsub
           go-github-com-moby-sys-atomicwriter
           go-github-com-moby-sys-mount
           go-github-com-moby-sys-mountinfo
           go-github-com-moby-sys-reexec
           go-github-com-moby-sys-sequential
           go-github-com-moby-sys-signal
           go-github-com-moby-sys-symlink
           go-github-com-moby-sys-user
           go-github-com-moby-sys-userns
           go-github-com-moby-term
           go-github-com-morikuni-aec
           go-github-com-opencontainers-cgroups
           go-github-com-opencontainers-go-digest
           go-github-com-opencontainers-image-spec
           go-github-com-opencontainers-runtime-spec
           go-github-com-opencontainers-selinux
           go-github-com-pelletier-go-toml
           go-github-com-pkg-errors
           go-github-com-prometheus-client-golang
           go-github-com-sirupsen-logrus
           go-github-com-tonistiigi-go-archvariant
           go-github-com-vbatts-tar-split
           go-github-com-vishvananda-netlink
           go-github-com-vishvananda-netns
           go-go-etcd-io-bbolt
           go-go-opentelemetry-io-contrib-instrumentation-google-golang-org-grpc-otelgrpc
           go-go-opentelemetry-io-contrib-instrumentation-net-http-otelhttp
           go-go-opentelemetry-io-otel
           go-go-opentelemetry-io-otel-exporters-otlp-otlptrace-otlptracehttp
           go-go-opentelemetry-io-otel-sdk
           go-go-opentelemetry-io-otel-trace
           go-golang-org-x-mod
           go-golang-org-x-net
           go-golang-org-x-sync
           go-golang-org-x-sys
           go-golang-org-x-text
           go-golang-org-x-time
           go-google-golang-org-genproto-googleapis-api
           go-google-golang-org-grpc
           go-google-golang-org-protobuf
           go-gotest-tools-v3
           go-resenje-org-singleflight
           go-tags-cncf-io-container-device-interface

           ;; TODO: Complete packaging.
           ;; go-github-com-golang-gddo
           ;; go-github-com-moby-buildkit
           ;; go-github-com-moby-swarmkit-v2
           ;; go-github-com-rootless-containers-rootlesskit-v2
           #;go-go-opentelemetry-io-contrib-processors-baggagecopy))
    (home-page "https://github.com/docker/docker")
    (synopsis "The Moby Project")
    (description
     "Moby is an open-source project created by Docker to enable and accelerate
software containerization.")
    (license license:asl2.0)))

(define-public go-github-com-docker-go-events
  (package
    (name "go-github-com-docker-go-events")
    (version "0.0.0-20250808211157-605354379745")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/docker/go-events")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0q3ylzyl670m3an32nhb0l5bsc3f4d9b963x3pjwsc94brg9l5qp"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/docker/go-events"))
    (propagated-inputs
     (list go-github-com-sirupsen-logrus))
    (home-page "https://github.com/docker/go-events")
    (synopsis "Composable event distribution for Golang")
    (description
     "This package implements a composable event distribution library,
originally created to implement the notifications in
@url{https://github.com/distribution/distribution/blob/v3.0.0/docs/content/about/notifications.md,
Docker Registry 2}.")
    (license license:asl2.0)))

(define-public go-github-com-docker-go-metrics
  (package
    (name "go-github-com-docker-go-metrics")
    (version "0.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/docker/go-metrics")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1b6f1889chmwlsgrqxylnks2jic16j2dqhsdd1dvaklk48ky95ga"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/docker/go-metrics"))
    (propagated-inputs (list go-github-com-prometheus-client-golang))
    (home-page "https://github.com/docker/go-metrics")
    (synopsis "Go library for metrics collection from Docker projects")
    (description
     "This package is a small wrapper around the Prometheus Go client to help
enforce convention and best practices for metrics collection in Docker
projects.")
    (license (list license:asl2.0 license:cc-by-sa4.0))))

(define-public go-github-com-moby-policy-helpers
  (package
    (name "go-github-com-moby-policy-helpers")
    (version "0.0.0-20260507153417-a39d60132186")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/moby/policy-helpers")
              (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "12fa602jianip7dg98f2i9i686s2a9bsclw8sqxw1ac1v19qlqr8"))
       (modules '((guix build utils)))
       (snippet '(delete-file-recursively "vendor"))))
    (build-system go-build-system)
    (arguments
     (list
      #:embed-files #~(list ".*\\.json")
      #:import-path "github.com/moby/policy-helpers"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-containerd-containerd-v2
           go-github-com-containerd-errdefs
           go-github-com-containerd-platforms
           go-github-com-distribution-reference
           go-github-com-gofrs-flock
           go-github-com-in-toto-in-toto-golang
           go-github-com-opencontainers-go-digest
           go-github-com-opencontainers-image-spec
           go-github-com-pkg-errors
           go-github-com-sigstore-protobuf-specs
           go-github-com-sigstore-sigstore
           go-github-com-sigstore-sigstore-go
           go-github-com-theupdateframework-go-tuf-v2
           go-golang-org-x-sync))
    (home-page "https://github.com/moby/policy-helpers")
    (synopsis "Policy helpers for Docker")
    (description "This package provides a policy helpers for Moby (Docker)
and BuildKit.  It is a work in progress.")
    (license license:asl2.0)))

(define-public go-github-com-opencontainers-image-spec-schema
  (package
    (name "go-github-com-opencontainers-image-spec-schema")
    (version "0.0.0-20260514171043-13cff54902ec")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/opencontainers/image-spec")
              (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0jg1wfbr6rva24cz6q6d73wgaridzkh9sclzm2dwxpiwmbkcas38"))
         (modules '((guix build utils)
                    (ice-9 ftw)
                    (srfi srfi-26)))
         (snippet
          #~(begin
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
              (delete-all-but "." "schema")))))
    (build-system go-build-system)
    (arguments
     (list
      #:embed-files
      ;; For go-github-com-santhosh-tekuri-jsonschema-v6:
      #~(list "applicator"
              "content"
              "core"
              "format"
              "format-annotation"
              "format-assertion"
              "meta-data"
              "schema"
              "unevaluated"
              "validation")
      #:import-path "github.com/opencontainers/image-spec/schema"
      #:unpack-path "github.com/opencontainers/image-spec"))
    (propagated-inputs
     (list go-github-com-opencontainers-go-digest
           go-github-com-opencontainers-image-spec
           go-github-com-russross-blackfriday-v2
           go-github-com-santhosh-tekuri-jsonschema-v6))
    (home-page "https://github.com/opencontainers/image-spec")
    (synopsis "OCI Image Format")
    (description
     "Package schema defines the OCI image media types, schema definitions and
validation functions.")
    (license license:asl2.0)))

(define-public go-github-com-opencontainers-image-tools
  (package
    (name "go-github-com-opencontainers-image-tools")
    (version "0.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/opencontainers/image-tools")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0drxjgcxm268cwv547anl23rv1jx7mvdfxp7nxr6fjbzpxav6bd2"))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:import-path "github.com/opencontainers/image-tools"))
    (home-page "https://github.com/opencontainers/image-tools")
    (synopsis "OCI Image Tooling")
    (description
     "@code{oci-image-tool} is a collection of tools for working with the
@url{https://github.com/opencontainers/image-spec, OCI image format
specification}.")
    (license license:asl2.0)))

(define-public go-github-com-opencontainers-umoci
  (package
    (name "go-github-com-opencontainers-umoci")
    (version "0.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/opencontainers/umoci")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "10pxiqk4194nbnvlvfvlbk31wp8k35in3g694y20f9261nn0qx6n"))
       (snippet
        #~(begin (use-modules (guix build utils))
                 (delete-file-recursively "vendor")))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/opencontainers/umoci"
      ;; convert spec to rootless: inspecting mount flags of /etc/resolv.conf:
      ;; no such file or directory
      #:test-flags
      #~(list "-skip" "TestUnpackManifestCustomLayer|TestUnpackStartFromDescriptor")
      #:test-subdirs
      ;; cmd/umoci needs older version of image-spec, excluding it.
      #~(list "." "mutate"
              "oci/cas/dir"
              "oci/casext/blobcompress"
              "oci/config/generate"
              "pkg/funchelpers"
              "pkg/hardening"
              "pkg/idtools"
              "pkg/mtreefilter"
              "pkg/pathtrie"
              "pkg/system"
              "pkg/unpriv")))
    (native-inputs
     (list go-github-com-mohae-deepcopy
           go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-adalogics-go-fuzz-headers
           go-github-com-apex-log
           go-github-com-blang-semver-v4
           go-github-com-cyphar-filepath-securejoin-0.4.1
           go-github-com-docker-go-units
           go-github-com-klauspost-compress
           go-github-com-klauspost-pgzip
           go-github-com-moby-sys-user
           go-github-com-moby-sys-userns
           go-github-com-opencontainers-go-digest
           go-github-com-opencontainers-image-spec
           go-github-com-opencontainers-runtime-spec
           go-github-com-rootless-containers-proto-go-proto
           go-github-com-urfave-cli
           go-github-com-vbatts-go-mtree
           go-golang-org-x-sys
           go-google-golang-org-protobuf))
    (home-page "https://umo.ci/")
    (synopsis "Modifies Open Container images")
    (description
     "umoci modifies Open Container images (pronounced /uːmoˈʨi/) is a
reference implementation of the @url{OCI image specification,
https://github.com/opencontainers/image-spec} and provides users with the
ability to create, manipulate, and otherwise interact with container images.
It is designed to be as small and unopinonated as possible, so as to act as a
foundation for larger systems to be built on top of.")
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

(define-public go-go-podman-io-common
  (package
    (name "go-go-podman-io-common")
    (version "0.68.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/podman-container-tools/container-libs")
              (commit (go-version->git-ref version #:subdir "common"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "040snqsg3il98pz9w8442wdzwm2rm07yqbch87f4jic4p4s0707s"))
       (modules '((guix build utils)
                  (ice-9 ftw)
                  (srfi srfi-26)))
       (snippet
        #~(begin
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
            (delete-all-but "." "common")
            ;; Module name has been changed upstream.
            (substitute* (find-files "." "\\.go$")
              (("github.com/disiqueira/gotree")
               "github.com/d6o/GoTree"))))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:import-path "go.podman.io/common"
      #:unpack-path "go.podman.io"
      #:embed-files
      #~(list "^VERSION$")
      #:test-subdirs
      ;; XXX: Remove when go-go-podman-io-image-v5 and go-go-podman-io-storage
      ;; are updated.
      #~(list "pkg/auth" "pkg/flag" "pkg/chown" "pkg/parse" "pkg/umask"
              "pkg/report" "pkg/sysctl" "pkg/cgroups" "pkg/filters"
              "pkg/formats" "pkg/machine" "pkg/seccomp" "pkg/secrets"
              "pkg/sysinfo" "pkg/timetype" "pkg/manifests" "pkg/configmaps"
              "pkg/hooks/0.1.0" "pkg/strongunits" "pkg/capabilities"
              "pkg/subscriptions" "pkg/secrets/filedriver"
              "pkg/configmaps/filedriver" "pkg/apparmor/internal/supported")))
    (native-inputs
     (list go-github-com-onsi-ginkgo-v2
           go-github-com-davecgh-go-spew
           go-github-com-onsi-gomega
           go-github-com-spf13-cobra
           go-github-com-spf13-pflag
           go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-checkpoint-restore-checkpointctl
           go-github-com-checkpoint-restore-go-criu-v8
           go-github-com-containerd-platforms
           go-github-com-containers-ocicrypt
           go-github-com-coreos-go-systemd-v22
           go-github-com-cyphar-filepath-securejoin-0.4.1
           go-github-com-davecgh-go-spew
           go-github-com-d6o-gotree-v3    ;go-github-com-disiqueira-gotree-v3
           go-github-com-docker-distribution
           go-github-com-docker-go-units
           go-github-com-fsnotify-fsnotify
           go-github-com-godbus-dbus-v5
           go-github-com-hashicorp-go-multierror
           go-github-com-jinzhu-copier
           go-github-com-json-iterator-go
           go-github-com-moby-sys-capability
           go-github-com-moby-sys-devices
           go-github-com-opencontainers-cgroups
           go-github-com-opencontainers-go-digest
           go-github-com-opencontainers-image-spec
           go-github-com-opencontainers-runtime-spec
           go-github-com-opencontainers-runtime-tools
           go-github-com-opencontainers-selinux
           go-github-com-pkg-sftp
           go-github-com-pmezard-go-difflib
           go-github-com-seccomp-libseccomp-golang
           go-github-com-sirupsen-logrus
           go-github-com-skeema-knownhosts
           go-github-com-vishvananda-netlink
           go-go-etcd-io-bbolt
           go-go-podman-io-image-v5
           go-go-podman-io-storage
           go-golang-org-x-crypto
           go-golang-org-x-sync
           go-golang-org-x-sys
           go-golang-org-x-term
           go-sigs-k8s-io-yaml
           go-tags-cncf-io-container-device-interface))
    (home-page "https://go.podman.io")
    (synopsis "Go code and configuration used across containers projects")
    (description
     "This package provides shared common files and common Go code to manage
those files in github.com/containers repos.")
    (license license:asl2.0)))

(define-public go-go-podman-io-image-v5
  (package
    (name "go-go-podman-io-image-v5")
    (version "5.39.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/podman-container-tools/container-libs")
              (commit (go-version->git-ref version #:subdir "image"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "17qjzgc3h89sa0y8qkviz5gry1kvkkm3j79yhn78c7gbmqcx5v0j"))
       (modules '((guix build utils)
                  (ice-9 ftw)
                  (srfi srfi-26)))
       (snippet
        #~(begin
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
            (delete-all-but "." "image")
            ;; This is a workaround to provide a correct import-path.
            (rename-file "image" "tmp")
            (mkdir-p "image/v5")
            (copy-recursively "tmp" "image/v5")
            (delete-file-recursively "tmp")))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:import-path "go.podman.io/image/v5"
      #:unpack-path "go.podman.io"
      #:embed-files
      #~(list "^VERSION$"
              ;; For go-github-com-santhosh-tekuri-jsonschema-v6:
              "applicator"
              "content"
              "core"
              "format"
              "format-annotation"
              "format-assertion"
              "meta-data"
              "schema"
              "unevaluated"
              "validation")
      #:test-flags
      #~(list "-skip" (string-join
                       ;; Tests try to access local directories: /usr/share,
                       ;; /var/tmp; remote source https://quay.io/v2/,
                       ;; https://registry.suse.com/auth.
                       (list "TestComputeBlobInfo"
                             "TestCreateBigFileTemp"
                             "TestGPGSigningMechanismSign"
                             "TestMkDirBigFileTemp"
                             "TestReferenceNewImage"
                             "TestReferenceNewImageSource"
                             "TestReferencePolicyConfigurationNamespaces"
                             "TestSetCredentialsInteroperability"
                             "TestSetupCertificates"
                             "TestSign"
                             "TestSignDockerManifest"
                             "TestSignDockerManifestWithPassphrase"
                             "TestSimpleSignerSignImageManifest"
                             "TestSourcePrepareLayerData")
                       "|"))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'set-HOME
            (lambda _
              (setenv "HOME" "/tmp"))))))
    (native-inputs
     (list btrfs-progs
           eudev
           gnupg
           go-github-com-stretchr-testify
           gpgme
           libassuan))
    (propagated-inputs
     (list go-dario-cat-mergo
           go-github-com-burntsushi-toml
           go-github-com-containers-libtrust
           go-github-com-containers-ocicrypt
           go-github-com-cyberphone-json-canonicalization
           go-github-com-distribution-reference
           go-github-com-docker-cli
           go-github-com-docker-distribution
           go-github-com-docker-docker
           go-github-com-docker-docker-credential-helpers
           go-github-com-docker-go-connections
           go-github-com-hashicorp-go-cleanhttp
           go-github-com-hashicorp-go-retryablehttp
           go-github-com-klauspost-compress
           go-github-com-klauspost-pgzip
           go-github-com-manifoldco-promptui
           go-github-com-mattn-go-sqlite3
           go-github-com-opencontainers-go-digest
           go-github-com-opencontainers-image-spec
           go-github-com-proglottis-gpgme
           go-github-com-santhosh-tekuri-jsonschema-v6
           go-github-com-secure-systems-lab-go-securesystemslib
           go-github-com-sigstore-fulcio
           go-github-com-sigstore-sigstore
           go-github-com-sirupsen-logrus
           go-github-com-sylabs-sif-v2
           go-github-com-ulikunitz-xz
           go-github-com-vbauerster-mpb-v8
           go-go-etcd-io-bbolt
           go-go-podman-io-storage
           go-golang-org-x-crypto
           go-golang-org-x-oauth2
           go-golang-org-x-sync
           go-golang-org-x-term
           go-gopkg-in-yaml-v3))
    (home-page "https://go.podman.io")
    (synopsis "Go library to work in various way with containers' images")
    (description
     "@code{image} is a set of Go libraries aimed at working in various way
with containers' images and container image registries.  The
@code{containers/image} library allows application to pull and push images
from container image registries, like the docker.io and quay.io registries. It
also implements \"simple image signing\".  It's a successor of
@url{https://github.com/containers/image} project.")
    (license license:asl2.0)))

(define-public go-go-podman-io-storage
  (package
    (name "go-go-podman-io-storage")
    (version "1.62.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/podman-container-tools/container-libs")
              (commit (go-version->git-ref version #:subdir "storage"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ywj80wkpyq2yhizsdlfh0n2fxip75042rsk7jm4vyp98bb18rb6"))
       (modules '((guix build utils)
                  (ice-9 ftw)
                  (srfi srfi-26)))
       (snippet
        #~(begin
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
            (delete-all-but "." "storage")))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "go.podman.io/storage"
      #:unpack-path "go.podman.io"
      #:test-flags
      #~(list "-skip" (string-join
                       ;; Root access is required.
                       (list "TestAttachLoopbackDeviceRace"
                             "TestChangesWithChangesGH13590"
                             "TestChroot.*"
                             "TestCopyDir"
                             "TestCopyWithTarInexistentDestWillCreateIt"
                             "TestEnsureRemoveAllWithMount"
                             "TestLookupAdditionalLayerDecodeError"
                             "TestLookupAdditionalLayerSuccess"
                             "TestMkdir.*"
                             "TestReplaceFileTarWrapper"
                             "TestStoreDelete"
                             "TestStoreMultiList"
                             "TestSupportsShifting"
                             "TestTarUntarWithXattr"
                             "TestTarWithBlockCharFifo"
                             "TestTarWithMaliciousSymlinks"
                             "TestUnshareOOMScoreAdj"
                             "TestUntarHardlinkToSymlink"
                             "TestUntarPath"
                             "TestUntarWithMaliciousSymlinks"
                             "TestVfs.*")
                       "|"))))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (inputs
     (list btrfs-progs))
    (propagated-inputs
     (list go-github-com-burntsushi-toml
           go-github-com-containerd-stargz-snapshotter-estargz
           go-github-com-cyphar-filepath-securejoin-0.4.1
           go-github-com-docker-go-units
           go-github-com-google-go-intervals
           go-github-com-json-iterator-go
           go-github-com-klauspost-compress
           go-github-com-klauspost-pgzip
           go-github-com-mattn-go-shellwords
           go-github-com-mistifyio-go-zfs-v3
           go-github-com-moby-sys-capability
           go-github-com-moby-sys-mountinfo
           go-github-com-moby-sys-user
           go-github-com-opencontainers-go-digest
           go-github-com-opencontainers-runtime-spec
           go-github-com-opencontainers-selinux
           go-github-com-sirupsen-logrus
           go-github-com-tchap-go-patricia-v2
           go-github-com-ulikunitz-xz
           go-github-com-vbatts-tar-split
           go-golang-org-x-sync
           go-golang-org-x-sys
           go-gotest-tools-v3))
    (home-page "https://go.podman.io")
    (synopsis "Manage layer/image/container storage")
    (description
     "@code{storage} is a Go library which aims to provide methods for storing
filesystem layers, container images, and containers.")
    (license license:asl2.0)))

(define-public python-docker
  (package
    (name "python-docker")
    (version "7.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/docker/docker-py")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1dd4p0xfv6vja4mgzwn2yfyna7vi7bc1pr5f59jg9yd4nxj96kmj"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; Integration tests need a running Docker daemon.
      #:test-flags #~(list "--ignore" "tests/integration")))
    (native-inputs (list python-hatch-vcs python-hatchling python-pytest))
    (inputs
     (list python-requests python-urllib3))
    (propagated-inputs
     (list python-paramiko ;adds SSH support
           python-websocket-client))
    (home-page "https://github.com/docker/docker-py/")
    (synopsis "Python client for Docker")
    (description "Docker-Py is a Python client for the Docker container
management tool.")
    (license license:asl2.0)))

(define-public python-docker-pycreds
  (package
    (name "python-docker-pycreds")
    (version "0.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "docker-pycreds" version))
        (sha256
         (base32
          "1m44smrggnqghxkqfl7vhapdw89m1p3vdr177r6cq17lr85jgqvc"))))
    (build-system pyproject-build-system)
    (arguments
     (list  ; XXX: These tests require docker credentials to run.
      #:test-flags '(list "--ignore=tests/store_test.py")))
    (native-inputs
     (list python-pytest python-setuptools python-wheel))
    (propagated-inputs
     (list python-six))
    (home-page "https://github.com/shin-/dockerpy-creds")
    (synopsis
     "Python bindings for the Docker credentials store API")
    (description
     "Docker-Pycreds contains the Python bindings for the docker credentials
store API.  It allows programmers to interact with a Docker registry using
Python without keeping their credentials in a Docker configuration file.")
    (license license:asl2.0)))

;; Needed for old v1 of docker-compose; remove once Docker is updated to a
;; more recent version which has the command "docker compose" built-in.
(define-public python-docker-5
  (package
    (inherit python-docker)
    (name "python-docker")
    (version "5.0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/docker/docker-py")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0m5ifgxdhcf7yci0ncgnxjas879sksrf3im0fahs573g268farz9"))))
    (build-system pyproject-build-system)
    ;; Integration tests need a running Docker daemon.
    (arguments (list #:tests? #f))
    (native-inputs (list python-setuptools))
    (inputs (modify-inputs inputs
              (prepend python-six)
              (delete "python-urllib3")))
    (propagated-inputs
     (modify-inputs propagated-inputs
       (prepend python-docker-pycreds python-urllib3-1.26)))))

;; Needed for old v1 of docker-compose; remove once Docker is updated to a
;; more recent version which has the command "docker compose" built-in, see:
;; <https://codeberg.org/guix/guix/milestone/30347>.
(define-public python-dockerpty
  (package
    (name "python-dockerpty")
    (version "0.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "dockerpty" version))
       (sha256
        (base32
         "1kjn64wx23jmr8dcc6g7bwlmrhfmxr77gh6iphqsl39sayfxdab9"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:tests? #f)) ; XXX: Requires outdated python-expects
    (native-inputs
     (list python-setuptools python-six))
    (home-page "https://github.com/d11wtq/dockerpty")
    (synopsis "Python library to use the pseudo-TTY of a Docker container")
    (description "Docker PTY provides the functionality needed to operate the
pseudo-terminal (PTY) allocated to a Docker container using the Python
client.")
    (license license:asl2.0)))

(define-public python-udocker
  (package
    (name "python-udocker")
    (version "1.3.17")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/indigo-dc/udocker")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1nbsj3kwlnkr12ykl1xd0r2hykikhrs0z9wgfb26y2nxpf85z3rz"))))
    (build-system pyproject-build-system)
    ;; Daemon chroot inconsistencies.
    (arguments (list #:test-flags #~(list "-k" "not test_05__get_volume_bindings")))
    (native-inputs (list python-pytest python-setuptools))
    (home-page "https://github.com/indigo-dc/udocker")
    (synopsis "Execute simple docker containers without root privileges")
    (description
     "This package provides a basic user tool to execute simple docker containers in
batch or interactive systems without root privileges.")
    (license license:asl2.0)))

;;;
;;; Executables:
;;;

;; Note - when changing Docker versions it is important to update the versions
;; of several associated packages (docker-libnetwork and go-sctp).
(define %docker-version "20.10.27")

(define-public buildah
  (package
    (name "buildah")
    (version "1.44.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/containers/buildah")
             (commit (string-append "v" version))))
       (sha256
        (base32 "0ivy7i0pqzhpsnd38l9favi32vgm0xcsn5bg4cxxqg6yzgw0fshh"))
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
           go
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

(define-public containerd
  (package
    (name "containerd")
    (version "1.6.22")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/containerd/containerd")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1m31y00sq2m76m1jiq4znws8gxbgkh5adklvqibxiz1b96vvwjk8"))
       (patches
        (search-patches "containerd-create-pid-file.patch"
                        "containerd-fix-includes.patch"))))
    (build-system go-build-system)
    (arguments
     (let ((make-flags #~(list (string-append "VERSION=" #$version)
                               (string-append "DESTDIR=" #$output)
                               "PREFIX="
                               "REVISION=0")))
       (list
        #:import-path "github.com/containerd/containerd"
        ;; XXX: This package contains full vendor, tests fail when run with
        ;; "...", limit to the project's root. Try to unvendor.
        #:test-subdirs #~(list ".")
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'patch-paths
              (lambda* (#:key inputs import-path outputs #:allow-other-keys)
                (with-directory-excursion (string-append "src/" import-path)
                  (substitute* "runtime/v1/linux/runtime.go"
                    (("defaultRuntime[ \t]*=.*")
                     (string-append "defaultRuntime = \""
                                    (search-input-file inputs "/sbin/runc")
                                    "\"\n"))
                    (("defaultShim[ \t]*=.*")
                     (string-append "defaultShim = \""
                                    (assoc-ref outputs "out")
                                    "/bin/containerd-shim\"\n")))
                  (substitute* "pkg/cri/config/config_unix.go"
                    (("DefaultRuntimeName: \"runc\"")
                     (string-append "DefaultRuntimeName: \""
                                    (search-input-file inputs "/sbin/runc")
                                    "\""))
                    ;; ContainerdConfig.Runtimes
                    (("\"runc\":")
                     (string-append "\""
                                    (search-input-file inputs "/sbin/runc")
                                    "\":")))
                  (substitute* "vendor/github.com/containerd/go-runc/runc.go"
                    (("DefaultCommand[ \t]*=.*")
                     (string-append "DefaultCommand = \""
                                    (search-input-file inputs "/sbin/runc")
                                    "\"\n")))
                  (substitute* "vendor/github.com/containerd/continuity/testutil\
/loopback/loopback_linux.go"
                    (("exec\\.Command\\(\"losetup\"")
                     (string-append "exec.Command(\""
                                    (search-input-file inputs "/sbin/losetup")
                                    "\"")))
                  (substitute* "archive/compression/compression.go"
                    (("exec\\.LookPath\\(\"unpigz\"\\)")
                     (string-append "\""
                                    (search-input-file inputs "/bin/unpigz")
                                    "\", error(nil)"))))))
            (replace 'build
              (lambda* (#:key import-path #:allow-other-keys)
                (with-directory-excursion (string-append "src/" import-path)
                  (apply invoke "make" #$make-flags))))
            (replace 'install
              (lambda* (#:key import-path #:allow-other-keys)
                (with-directory-excursion (string-append "src/" import-path)
                  (apply invoke "make" "install" #$make-flags))))))))
    (inputs
     (list btrfs-progs libseccomp pigz runc util-linux))
    (native-inputs
     (list go pkg-config))
    (synopsis "Docker container runtime")
    (description "This package provides the container daemon for Docker.
It includes image transfer and storage, container execution and supervision,
network attachments.")
    (home-page "https://containerd.io/")
    (license license:asl2.0)))

(define-public cqfd
  (package
    (name "cqfd")
    (version "5.6.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/savoirfairelinux/cqfd")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1p0093hx2ryxng2l53cqaca8g7gw888ydq2i328raip62pcdrccp"))))
    (build-system gnu-build-system)
    (arguments
     ;; The test suite requires a docker daemon and connectivity.
     (list
      #:tests? #f
      #:make-flags #~(list (string-append "COMPLETIONSDIR="
                                          #$output "/etc/bash_completion.d")
                           (string-append "PREFIX=" #$output))
      #:phases #~(modify-phases %standard-phases
                   (delete 'configure)
                   (delete 'build))))
    (home-page "https://github.com/savoirfairelinux/cqfd")
    (synopsis "Convenience wrapper for Docker")
    (description "cqfd is a Bash script that provides a quick and convenient
way to run commands in the current directory, but within a Docker container
defined in a per-project configuration file.")
    (license license:gpl3+)))

(define-public crun
  (package
    (name "crun")
    (version "1.30.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/containers/crun")
              (commit version)
              (recursive? #t)))
       (sha256
        (base32
         "0xcplka3n3blj7yycdxc0v9231fbcpbhn2kx9vdysa5p13bv1hvh"))
       (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:configure-flags #~(list "--disable-systemd")
      #:tests? #f ; XXX: needs /sys/fs/cgroup mounted
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'git-version-h
            (lambda _
              (call-with-output-file "git-version.h"
                (lambda (port)
                  (format
                   port
                   "#ifndef GIT_VERSION\n# define GIT_VERSION ~s\n#endif"
                   #$version)))))
          (add-after 'unpack 'fix-tests
            (lambda _
              (substitute* (find-files "tests" "\\.(c|py)")
                (("/bin/true") (which "true"))
                (("/bin/false") (which "false"))
                ;; relies on sd_notify which requires systemd?
                (("\"sd-notify\" : test_sd_notify,") "")
                (("\"sd-notify-file\" : test_sd_notify_file,") "")))))))
    (inputs
     (list json-c
           libcap
           libseccomp
           yajl))
    (native-inputs
     (list automake
           autoconf
           git-minimal/pinned
           libtool
           pkg-config
           python-minimal-wrapper))
    (home-page "https://github.com/containers/crun")
    (synopsis "Open Container Initiative (OCI) Container runtime")
    (description
     "crun is a fast and low-memory footprint Open Container Initiative (OCI)
Container Runtime fully written in C.")
    (license license:gpl2+)))

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

;; TODO: Patch out modprobes for ip_vs, nf_conntrack,
;; brige, nf_conntrack_netlink, aufs.
(define-public docker
  (package
    (name "docker")
    (version %docker-version)
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/moby/moby")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "017frilx35w3m4dz3n6m2f293q4fq4jrk6hl8f7wg5xs3r8hswvq"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:modules
      '((guix build gnu-build-system)
        ((guix build go-build-system) #:prefix go:)
        (guix build union)
        (guix build utils))
      #:imported-modules
      `(,@%default-gnu-imported-modules
        (guix build union)
        (guix build go-build-system))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-paths
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "builder/builder-next/executor_unix.go"
                (("CommandCandidates:.*runc.*")
                 (string-append "CommandCandidates: []string{\""
                                (search-input-file inputs "/sbin/runc")
                                "\"},\n")))
              (substitute* "vendor/github.com/containerd/go-runc/runc.go"
                (("DefaultCommand = .*")
                 (string-append "DefaultCommand = \""
                                (search-input-file inputs "/sbin/runc")
                                "\"\n")))
              (substitute* "vendor/github.com/containerd/containerd/\
runtime/v1/linux/runtime.go"
                (("defaultRuntime[ \t]*=.*")
                 (string-append "defaultRuntime = \""
                                (search-input-file inputs "/sbin/runc")
                                "\"\n"))
                (("defaultShim[ \t]*=.*")
                 (string-append "defaultShim = \""
                                (search-input-file inputs "/bin/containerd-shim")
                                "\"\n")))
              (substitute* "daemon/daemon_unix.go"
                (("DefaultShimBinary = .*")
                 (string-append "DefaultShimBinary = \""
                                (search-input-file inputs "/bin/containerd-shim")
                                "\"\n"))
                (("DefaultRuntimeBinary = .*")
                 (string-append "DefaultRuntimeBinary = \""
                                (search-input-file inputs "/sbin/runc")
                                "\"\n")))
              (substitute* "daemon/runtime_unix.go"
                (("defaultRuntimeName = .*")
                 (string-append "defaultRuntimeName = \""
                                (search-input-file inputs "/sbin/runc")
                                "\"\n")))
              (substitute* "daemon/config/config.go"
                (("StockRuntimeName = .*")
                 (string-append "StockRuntimeName = \""
                                (search-input-file inputs "/sbin/runc")
                                "\"\n"))
                (("DefaultInitBinary = .*")
                 (string-append "DefaultInitBinary = \""
                                (search-input-file inputs "/bin/tini-static")
                                "\"\n")))
              (substitute* "daemon/config/config_common_unix_test.go"
                (("expectedInitPath: \"docker-init\"")
                 (string-append "expectedInitPath: \""
                                (search-input-file inputs "/bin/tini-static")
                                "\"")))
              (substitute* "vendor/github.com/moby/buildkit/executor/\
runcexecutor/executor.go"
                (("var defaultCommandCandidates = .*")
                 (string-append "var defaultCommandCandidates = []string{\""
                                (search-input-file inputs "/sbin/runc") "\"}")))
              (substitute* "vendor/github.com/docker/libnetwork/portmapper/proxy.go"
                (("var userlandProxyCommandName = .*")
                 (string-append "var userlandProxyCommandName = \""
                                (search-input-file inputs "/bin/proxy")
                                "\"\n")))
              (substitute* "pkg/archive/archive.go"
                (("string\\{\"xz")
                 (string-append "string{\"" (search-input-file inputs "/bin/xz"))))

              (let ((source-files (filter (lambda (name)
                                            (not (string-contains name "test")))
                                          (find-files "." "\\.go$"))))
                (let-syntax ((substitute-LookPath*
                              (syntax-rules ()
                                ((_ (source-text path) ...)
                                 (substitute* source-files
                                   (((string-append "\\<exec\\.LookPath\\(\""
                                                    source-text
                                                    "\")"))
                                    (string-append "\""
                                                   (search-input-file inputs path)
                                                   "\", error(nil)")) ...))))
                             (substitute-Command*
                              (syntax-rules ()
                                ((_ (source-text path) ...)
                                 (substitute* source-files
                                   (((string-append "\\<(re)?exec\\.Command\\(\""
                                                    source-text
                                                    "\"") _ re?)
                                    (string-append (if re? re? "")
                                                   "exec.Command(\""
                                                   (search-input-file inputs path)
                                                   "\"")) ...)))))
                  (substitute-LookPath*
                   ("containerd" "/bin/containerd")
                   ("ps" "/bin/ps")
                   ("mkfs.xfs" "/sbin/mkfs.xfs")
                   ("lvmdiskscan" "/sbin/lvmdiskscan")
                   ("pvdisplay" "/sbin/pvdisplay")
                   ("blkid" "/sbin/blkid")
                   ("unpigz" "/bin/unpigz")
                   ("iptables" "/sbin/iptables")
                   ("ip6tables" "/sbin/ip6tables")
                   ("iptables-legacy" "/sbin/iptables")
                   ("ip" "/sbin/ip"))

                  (substitute-Command*
                   ("modprobe" "/bin/modprobe")
                   ("pvcreate" "/sbin/pvcreate")
                   ("vgcreate" "/sbin/vgcreate")
                   ("lvcreate" "/sbin/lvcreate")
                   ("lvconvert" "/sbin/lvconvert")
                   ("lvchange" "/sbin/lvchange")
                   ("mkfs.xfs" "/sbin/mkfs.xfs")
                   ("xfs_growfs" "/sbin/xfs_growfs")
                   ("mkfs.ext4" "/sbin/mkfs.ext4")
                   ("tune2fs" "/sbin/tune2fs")
                   ("blkid" "/sbin/blkid")
                   ("resize2fs" "/sbin/resize2fs")
                   ("ps" "/bin/ps")
                   ("losetup" "/sbin/losetup")
                   ("uname" "/bin/uname")
                   ("dbus-launch" "/bin/dbus-launch")
                   ("git" "/bin/git")))
                ;; docker-mountfrom ??
                ;; docker
                ;; docker-untar ??
                ;; docker-applyLayer ??
                ;; /usr/bin/uname
                ;; grep
                ;; apparmor_parser

                ;; Make compilation fail when, in future versions, Docker
                ;; invokes other programs we don't know about and thus don't
                ;; substitute.
                (substitute* source-files
                  ;; Search for Java in PATH.
                  (("\\<exec\\.Command\\(\"java\"")
                   "xxec.Command(\"java\"")
                  ;; Search for AUFS in PATH (mainline Linux doesn't support it).
                  (("\\<exec\\.Command\\(\"auplink\"")
                   "xxec.Command(\"auplink\"")
                  ;; Fail on other unsubstituted commands.
                  (("\\<exec\\.Command\\(\"([a-zA-Z0-9][a-zA-Z0-9_-]*)\""
                    _ executable)
                   (string-append "exec.Guix_doesnt_want_Command(\""
                                  executable "\""))
                  (("\\<xxec\\.Command")
                   "exec.Command")
                  ;; Search for ZFS in PATH.
                  (("\\<LookPath\\(\"zfs\"\\)") "LooxPath(\"zfs\")")
                 ;; Do not fail when buildkit-qemu-<target> isn't found.
                 ;; FIXME: We might need to package buildkit and docker's
                 ;; buildx plugin, to support qemu-based docker containers.
                  (("\\<LookPath\\(\"buildkit-qemu-\"") "LooxPath(\"buildkit-qemu-\"")
                  ;; Fail on other unsubstituted LookPaths.
                  (("\\<LookPath\\(\"") "Guix_doesnt_want_LookPath\\(\"")
                  (("\\<LooxPath") "LookPath")))))
          (add-after 'patch-paths 'delete-failing-tests
            (lambda _
              ;; Needs internet access.
              (delete-file "builder/remotecontext/git/gitutils_test.go")
              ;; Permission denied.
              (delete-file "daemon/graphdriver/devmapper/devmapper_test.go")
              ;; Operation not permitted (idtools.MkdirAllAndChown).
              (delete-file "daemon/graphdriver/vfs/vfs_test.go")
              ;; Timeouts after 5 min.
              (delete-file "plugin/manager_linux_test.go")
              ;; Operation not permitted.
              (delete-file "daemon/graphdriver/aufs/aufs_test.go")
              (delete-file "daemon/graphdriver/btrfs/btrfs_test.go")
              (delete-file "daemon/graphdriver/overlay/overlay_test.go")
              (delete-file "daemon/graphdriver/overlay2/overlay_test.go")
              (delete-file "pkg/chrootarchive/archive_unix_test.go")
              (delete-file "daemon/container_unix_test.go")
              ;; This file uses cgroups and /proc.
              (delete-file "pkg/sysinfo/sysinfo_linux_test.go")
              ;; This file uses cgroups.
              (delete-file "runconfig/config_test.go")
              ;; This file uses /var.
              (delete-file "daemon/oci_linux_test.go")
              ;; Signal tests fail in bizarre ways
              (delete-file "pkg/signal/signal_linux_test.go")))
          (replace 'configure
            (lambda _
              (setenv "DOCKER_BUILDTAGS" "seccomp")
              (setenv "DOCKER_GITCOMMIT" (string-append "v" #$%docker-version))
              (setenv "VERSION" (string-append #$%docker-version "-ce"))
              ;; Automatically use bundled dependencies.
              ;; TODO: Unbundle - see file "vendor.conf".
              (setenv "AUTO_GOPATH" "1")
              ;; Respectively, strip the symbol table and debug
              ;; information, and the DWARF symbol table.
              (setenv "LDFLAGS" "-s -w")
              ;; Make build faster
              (setenv "GOCACHE" "/tmp")))
          (add-before 'build 'setup-go-environment
            (assoc-ref go:%standard-phases 'setup-go-environment))
          (replace 'build
            (lambda _
              ;; Our LD doesn't like the statically linked relocatable things
              ;; that go produces, so install the dynamic version of
              ;; dockerd instead.
              (setenv "BUILDFLAGS" "-trimpath")
              (invoke "hack/make.sh" "dynbinary")))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                ;; The build process generated a file because the environment
                ;; variable "AUTO_GOPATH" was set.  Use it.
                (setenv "GOPATH" (string-append (getcwd) "/.gopath"))
                ;; ".gopath/src/github.com/docker/docker" is a link to the current
                ;; directory and chdir would canonicalize to that.
                ;; But go needs to have the uncanonicalized directory name, so
                ;; store that.
                (setenv "PWD" (string-append
                               (getcwd) "/.gopath/src/github.com/docker/docker"))
                (with-directory-excursion ".gopath/src/github.com/docker/docker"
                  (invoke "hack/test/unit"))
                (setenv "PWD" #f))))
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (out-bin (string-append out "/bin")))
                (install-file "bundles/dynbinary-daemon/dockerd" out-bin)
                (install-file (string-append "bundles/dynbinary-daemon/dockerd-"
                                             (getenv "VERSION"))
                              out-bin)))))))
    (inputs
     (list btrfs-progs
           containerd       ; for containerd-shim
           coreutils
           dbus
           docker-libnetwork-cmd-proxy
           e2fsprogs
           git
           iproute
           iptables
           kmod
           libseccomp
           pigz
           procps
           runc
           util-linux
           lvm2
           tini
           xfsprogs-5.9
           xz))
    (native-inputs
     (list eudev ; TODO: Should be propagated by lvm2 (.pc -> .pc)
           go-1.22 gotestsum pkg-config))
    (synopsis "Container component library and daemon")
    (description "This package provides a framework to assemble specialized
container systems.  It includes components for orchestration, image
management, secret management, configuration management, networking,
provisioning etc.")
    (home-page "https://mobyproject.org/")
    (license license:asl2.0)))

(define-public docker-cli
  (package
    (name "docker-cli")
    (version %docker-version)
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/docker/cli")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0szwaxiasy77mm90wj2qg747zb9lyiqndg5halg7qbi41ng6ry0h"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/docker/cli"
       ;; TODO: Tests require a running Docker daemon.
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'setup-environment-2
           (lambda _
             ;; Respectively, strip the symbol table and debug
             ;; information, and the DWARF symbol table.
             (setenv "LDFLAGS" "-s -w")

             ;; Make sure "docker -v" prints a usable version string.
             (setenv "VERSION" ,%docker-version)

             ;; Make build reproducible.
             (setenv "BUILDTIME" "1970-01-01 00:00:01.000000000+00:00")
             (symlink "src/github.com/docker/cli/scripts" "./scripts")
             (symlink "src/github.com/docker/cli/docker.Makefile" "./docker.Makefile")))
         (replace 'build
           (lambda _
             (setenv "GO_LINKMODE" "dynamic")
             (invoke "./scripts/build/binary")))
         (replace 'check
           (lambda* (#:key make-flags tests? #:allow-other-keys)
             (setenv "PATH" (string-append (getcwd) "/build:" (getenv "PATH")))
             (when tests?
               ;; Use the newly-built docker client for the tests.
               (with-directory-excursion "src/github.com/docker/cli"
                 ;; TODO: Run test-e2e as well?
                 (apply invoke "make" "-f" "docker.Makefile" "test-unit"
                        (or make-flags '()))))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (out-bin (string-append out "/bin"))
                    (etc (string-append out "/etc")))
               (with-directory-excursion "src/github.com/docker/cli/contrib/completion"
                 (install-file "bash/docker"
                               (string-append etc "/bash_completion.d"))
                 (install-file "fish/docker.fish"
                               (string-append etc "/fish/completions"))
                 (install-file "zsh/_docker"
                               (string-append etc "/zsh/site-functions")))
               (install-file "build/docker" out-bin)))))))
    (native-inputs
     (list go libltdl pkg-config))
    (synopsis "Command line interface to Docker")
    (description "This package provides a command line interface to Docker.")
    (home-page "https://www.docker.com/")
    (license license:asl2.0)))

;;; TODO: This package needs to be updated to its 2.x series, now authored in
;;; Go.
(define-public docker-compose
  (package
    (name "docker-compose")
    (version "1.29.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "docker-compose" version))
       (sha256
        (base32
         "1dq9kfak61xx7chjrzmkvbw9mvj9008k7g8q7mwi4x133p9dk32c"))))
    (build-system pyproject-build-system)
    ;; TODO: Tests require running Docker daemon.
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-pyyaml
            (lambda _
              (substitute* "setup.py"
                ((", < 6")
                 "")))))))
    (native-inputs (list python-setuptools))
    (inputs
     (list python-cached-property
           python-distro
           python-docker-5
           python-dockerpty
           python-docopt
           python-dotenv-0.13.0
           python-jsonschema-3
           python-pyyaml
           python-requests
           python-six
           python-texttable
           python-websocket-client-0.59))
    (home-page "https://www.docker.com/")
    (synopsis "Multi-container orchestration for Docker")
    (description "Docker Compose is a tool for defining and running
multi-container Docker applications.  A Compose file is used to configure an
application’s services.  Then, using a single command, the containers are
created and all the services are started as specified in the configuration.")
    (license license:asl2.0)))

;;; Private package that shouldn't be used directly; its purposes is to be
;;; used as a template for the various packages it contains.  It doesn't build
;;; anyway, as it needs many dependencies that aren't being satisfied.
(define docker-libnetwork
  ;; There are no recent release for libnetwork, so choose the last commit of
  ;; the branch that Docker uses, as can be seen in the 'vendor.conf' Docker
  ;; source file.  NOTE - It is important that this version is kept in sync
  ;; with the version of Docker being used.
  (let ((commit "3797618f9a38372e8107d8c06f6ae199e1133ae8")
        (version (version-major+minor %docker-version))
        (revision "3"))
    (package
      (name "docker-libnetwork")
      (version (git-version version revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      ;; Redirected from github.com/docker/libnetwork.
                      (url "https://github.com/moby/libnetwork")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1km3p6ya9az0ax2zww8wb5vbifr1gj5n9l82i273m9f3z9f2mq2p"))
                ;; Delete bundled ("vendored") free software source code.
                (modules '((guix build utils)))
                (snippet '(delete-file-recursively "vendor"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "github.com/moby/libnetwork/"))
      (home-page "https://github.com/moby/libnetwork/")
      (synopsis "Networking for containers")
      (description "Libnetwork provides a native Go implementation for
connecting containers.  The goal of @code{libnetwork} is to deliver a robust
container network model that provides a consistent programming interface and
the required network abstractions for applications.")
      (license license:asl2.0))))

(define-public docker-libnetwork-cmd-proxy
  (package
    (inherit docker-libnetwork)
    (name "docker-libnetwork-cmd-proxy")
    (arguments
     (list
      ;; The tests are unsupported on all architectures except x86_64-linux.
      #:tests? (and (not (%current-target-system)) (target-x86-64?))
      #:install-source? #f
      #:import-path "github.com/docker/libnetwork/cmd/proxy"
      #:unpack-path "github.com/docker/libnetwork"))
    (native-inputs
     (list go-github-com-sirupsen-logrus ; for tests.
           go-github-com-vishvananda-netlink
           go-github-com-vishvananda-netns
           go-golang-org-x-crypto
           go-golang-org-x-sys
           go-sctp))
    (synopsis "Docker user-space proxy")
    (description
     "This package provides a proxy running in the user space.  It is used by
the built-in registry server of Docker.")
    (license license:asl2.0)))

(define-public docker-policy-helper
  (package/inherit go-github-com-moby-policy-helpers
    (name "docker-policy-helper")
    (arguments
     (substitute-keyword-arguments arguments
       ((#:import-path _) "github.com/moby/policy-helpers/cmd/policy-helper")
       ((#:install-source? _ #t) #f)
       ((#:skip-build? _ #t) #f)
       ((#:tests? _ #t) #f)
       ((#:unpack-path _ "") "github.com/moby/policy-helpers")
       ((#:phases phases '%standard-phases)
        #~(modify-phases #$phases
            (add-after 'install 'fix-bin-name
              (lambda _
                (rename-file (string-append #$output "/bin/policy-helper")
                             (string-append #$output "/bin/docker-policy-helper"))))))))
    (native-inputs (package-propagated-inputs go-github-com-moby-policy-helpers))
    (propagated-inputs '())
    (inputs '())))

(define-public docker-registry
  (package
    (name "docker-registry")
    ;; XXX: The project ships a "vendor" directory containing all
    ;; dependencies, consider to review and package them.  The Golang library
    ;; is packaged in (gnu packges golang-xyz) as
    ;; go-github-com-docker-distribution.
    (version "2.8.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/docker/distribution")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0dbaxmkhg53anhkzngyzlxm2bd4dwv0sv75zip1rkm0874wjbxzb"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/docker/distribution"
      #:test-subdirs #~(list "configuration"
                             "context"
                             "health"
                             "manifest"
                             "notifications/..."
                             "uuid")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'chdir-to-src
            (lambda _ (chdir "src/github.com/docker/distribution")))
          (add-after 'chdir-to-src 'fix-versioning
            (lambda _
              ;; The Makefile use git to compute the version and the
              ;; revision. This requires the .git directory that we don't have
              ;; anymore in the unpacked source.
              (substitute* "Makefile"
                (("^VERSION=\\$\\(.*\\)")
                 (string-append "VERSION=v" #$version))
                ;; The revision originally used the git hash with .m appended
                ;; if there was any local modifications.
                (("^REVISION=\\$\\(.*\\)") "REVISION=0"))))
          (replace 'build
            (lambda _
              (invoke "make" "binaries")))
          (replace 'install
            (lambda _
              (let ((bin (string-append #$output "/bin")))
                (mkdir-p bin)
                (for-each
                 (lambda (file)
                   (install-file (string-append "bin/" file) bin))
                 '("digest"
                   "registry"
                   "registry-api-descriptor-template")))
              (let ((doc (string-append
                          #$output "/share/doc/" #$name "-" #$version)))
                (mkdir-p doc)
                (for-each
                 (lambda (file)
                   (install-file file doc))
                 '("BUILDING.md"
                   "CONTRIBUTING.md"
                   "LICENSE"
                   "MAINTAINERS"
                   "README.md"
                   "ROADMAP.md"))
                (copy-recursively "docs/" (string-append doc "/docs")))
              (let ((examples
                     (string-append
                      #$output "/share/doc/" #$name "-" #$version
                      "/registry-example-configs")))
                (mkdir-p examples)
                (for-each
                 (lambda (file)
                   (install-file (string-append "cmd/registry/" file) examples))
                 '("config-cache.yml"
                   "config-example.yml"
                   "config-dev.yml")))))
          (delete 'install-license-files))))
    (home-page "https://github.com/docker/distribution")
    (synopsis "Docker registry server and associated tools")
    (description "The Docker registry server enable you to host your own
docker registry.  With it, there is also two other utilities:
@itemize
@item The digest utility is a tool that generates checksums compatibles with
various docker manifest files.
@item The registry-api-descriptor-template is a tool for generating API
specifications from the docs/spec/api.md.tmpl file.
@end itemize")
    (license license:asl2.0)))

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
    (synopsis "Guix' docker compose compatibility layer")
    (description "A toolkit to run, read and write docker-compose.yml files with
Guix machinery.")
    (home-page "https://codeberg.org/fishinthecalculator/guix-compose")
    (license license:gpl3+)))

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

(define-public libslirp
  (package
    (name "libslirp")
    (version "4.9.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.freedesktop.org/slirp/libslirp")
             (commit (string-append "v" version))))
       (sha256
        (base32 "19f1p37b4ybqbgk817g3sqhdvr1gl3d1063hj7zg1jqa301s2afw"))
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

(define-public podman
  (package
    (name "podman")
    (version "6.1.3")
    (outputs '("out" "docker"))
    (properties
     `((output-synopsis "docker" "docker alias for podman")))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/podman-container-tools/podman")
             (commit (string-append "v" version))))
       (sha256
        (base32 "17wqxxw0jkqgzf6w68qichxi7hsj709p3hnn7a8yypi6l1vnqnxn"))
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
              (setenv "HOME" "/tmp")))
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
            (lambda _
              ;; So it picks podman of the other output.
              (substitute* "docker/docker.in"
                (("[$][{]BINDIR[}]") (string-append #$output "/bin"))
                (("[$][{]ETCDIR[}]") "/etc"))
              (invoke "make" "install.docker"
                      (string-append "PREFIX=" #$output:docker)
                      (string-append "ETCDIR=" #$output:docker "/etc"))))
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
           go-1.26
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
    (version "1.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/containers/podman-compose")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0lp4s7j1dwrnl8r9k93kd0396jwy0rkyq545ca88d90nyrjwniff"))))
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

(define-public podman-containers-storage
  (package/inherit go-go-podman-io-storage
    (name "podman-containers-storage")
    (arguments
     (substitute-keyword-arguments arguments
       ((#:import-path _ "") "go.podman.io/storage/cmd/...")
       ((#:install-source? #t #t) #f)
       ((#:tests? #t #t) #f)
       ((#:phases phases '%standard-phases)
        #~(modify-phases #$phases
            (add-after 'install 'build-and-install-docs
              (lambda* (#:key unpack-path #:allow-other-keys)
                (with-directory-excursion (string-append "src/" unpack-path
                                                         "/storage")
                  (setenv "PREFIX" #$output)
                  (invoke "make" "-C" "docs" "docs" "install"))))))))
    (native-inputs
     (append
      (modify-inputs native-inputs
        (append go-md2man))
      (package-propagated-inputs go-go-podman-io-storage)))
    (propagated-inputs '())
    (description
     "@code{containers-storage} is a command line tool for manipulating local
layer/image/container stores.")))

(define-public runc
  ;; TODO: Inheerit form go-github-com-opencontainers-runc when it's moved
  ;; here.
  (package
    (name "runc")
    (version "1.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/opencontainers/runc")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0midvxwmj4fvhy5mqv616bhlx39j0gd6y890adx7dnz5in506ym1"))
       (snippet
        #~(begin
            (use-modules (guix build utils))
            (delete-file-recursively "vendor")))))
    (build-system go-build-system)
    (arguments
     (list
      ;; XXX: 20/139 tests fail due to missing /var, cgroups and apparmor in
      ;; the build environment.
      #:tests? #f
      #:install-source? #f
      #:import-path "github.com/opencontainers/runc"
      #:phases
      #~(modify-phases %standard-phases
         (add-after 'unpack 'patch-source
           (lambda* (#:key import-path #:allow-other-keys)
             (substitute*  (string-append "src/" import-path "/Makefile")
               (("/bin/bash") (which "bash")))))
          (replace 'build
            (lambda* (#:key import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (invoke "make" "all" "man"))))
          (replace 'install
            (lambda* (#:key import-path outputs #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (invoke "make" "install" "install-bash" "install-man"
                        (string-append "PREFIX=" #$output))))))))
    (native-inputs
     (list go-github-com-checkpoint-restore-go-criu-v6
           go-github-com-containerd-console
           go-github-com-coreos-go-systemd-v22
           go-github-com-cyphar-filepath-securejoin-0.4.1
           go-github-com-docker-go-units
           go-github-com-godbus-dbus-v5
           go-github-com-moby-sys-capability
           go-github-com-moby-sys-mountinfo
           go-github-com-moby-sys-user
           go-github-com-moby-sys-userns
           go-github-com-mrunalp-fileutils
           go-github-com-opencontainers-cgroups-0.0.1
           go-github-com-opencontainers-runtime-spec-1.2.1
           go-github-com-opencontainers-selinux
           go-github-com-seccomp-libseccomp-golang
           go-github-com-sirupsen-logrus
           go-github-com-urfave-cli
           go-github-com-vishvananda-netlink
           go-golang-org-x-net
           go-golang-org-x-sys
           go-google-golang-org-protobuf
           go-md2man
           pkg-config))
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

(define-public skopeo
  (package
    (name "skopeo")
    (version "1.24.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/podman-container-tools/skopeo")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "05a2xp3ss59nld8i4zf85rqrqlsid1ipj5484vp23a5sdiybl0j4"))))
    (build-system gnu-build-system)
    (native-inputs
     (list go
           go-md2man
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
              (string-append "GOMD2MAN=" #$go-md2man "/bin/go-md2man"))
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
    (home-page "https://github.com/podman-container-tools/skopeo")
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

(define-public tini
  (package
    (name "tini")
    (version "0.19.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/krallin/tini")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1hnnvjydg7gi5gx6nibjjdnfipblh84qcpajc08nvr44rkzswck4"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f                    ;tests require a Docker daemon
       ;; 'tini-static' is a static binary, which leads CMake to fail with
       ;; ‘file RPATH_CHANGE could not write new RPATH: ...’.  Clear
       ;; CMAKE_INSTALL_RPATH to avoid that problem.
       #:configure-flags '("-DCMAKE_INSTALL_RPATH=")))
    (home-page "https://github.com/krallin/tini")
    (synopsis "Tiny but valid init for containers")
    (description "Tini is an init program specifically designed for use with
containers.  It manages a single child process and ensures that any zombie
processes produced from it are reaped and that signals are properly forwarded.
Tini is integrated with Docker.")
    (license license:expat)))

(define-public umoci
  (package
    (name "umoci")
    (version "0.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/opencontainers/umoci")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0m50x2q2h34g6sh786blf8r9wh098yzgwnicdlx0cgsqqwjsn0ia"))
       (snippet
        #~(begin
            (use-modules (guix build utils))
            (delete-file-recursively "vendor")))))
    (build-system go-build-system)
    (arguments
     (list
      #:install-source? #f
      #:import-path "github.com/opencontainers/umoci/cmd/umoci"
      #:unpack-path "github.com/opencontainers/umoci"
      #:test-flags
      ;; Two tests fail with error: unpack config.json: convert spec to
      ;; rootless: inspecting mount flags of /etc/resolv.conf: no such file or
      ;; directory
      #~(list "-skip" (string-append "TestUnpackManifestCustomLayer"
                                     "|TestUnpackStartFromDescriptor"))
      #:test-subdirs #~(list "../../...")       ;test the whole library
      #:build-flags
      #~(list (string-append "-ldflags="
                             "-X github.com/opencontainers/umoci.version="
                             #$version))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'build-and-install-man-pages
            (lambda* (#:key unpack-path #:allow-other-keys)
              (with-directory-excursion
                  (string-append "src/" unpack-path "/doc/man")
                (mkdir-p (string-append #$output "/share/man/man1"))
                (for-each
                 (lambda (file)
                   (let* ((file (string-drop-right file 3))      ;cut .md
                          (in-md (string-append file ".md"))
                          (out-man (string-append #$output
                                                  "/share/man/man1/" file)))
                     (invoke "go-md2man" "-in" in-md "-out" out-man)))
                 (find-files "." "\\.md$"))))))))
    (native-inputs
     (list go-github-com-adalogics-go-fuzz-headers
           go-github-com-apex-log
           go-github-com-blang-semver-v4
           go-github-com-containerd-platforms
           go-github-com-cyphar-filepath-securejoin-0.4.1
           go-github-com-cyphar-go-mtree
           go-github-com-docker-go-units
           go-github-com-klauspost-compress
           go-github-com-klauspost-pgzip
           go-github-com-moby-sys-user
           go-github-com-moby-sys-userns
           go-github-com-mohae-deepcopy
           go-github-com-opencontainers-go-digest
           go-github-com-opencontainers-image-spec
           go-github-com-opencontainers-runtime-spec
           go-github-com-rootless-containers-proto-go-proto
           go-github-com-stretchr-testify
           go-github-com-urfave-cli
           go-golang-org-x-sys
           go-google-golang-org-protobuf
           go-md2man))
    (home-page "https://umo.ci/")
    (synopsis "Tool for modifying Open Container images")
    (description
     "@command{umoci} is a tool that allows for high-level modification of an
Open Container Initiative (OCI) image layout and its tagged images.")
    (license license:asl2.0)))
