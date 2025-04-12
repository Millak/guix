;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 David Thompson <davet@gnu.org>
;;; Copyright © 2018, 2023 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2019, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019, 2020, 2021, 2023 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2020 Michael Rohleder <mike@rohleder.de>
;;; Copyright © 2020 Katherine Cox-Buday <cox.katherine.e@gmail.com>
;;; Copyright © 2020 Jesse Dowell <jessedowell@gmail.com>
;;; Copyright © 2021, 2022 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2022 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2024 Nicolas Graves <ngraves@ngraves.fr>
;;; Copyright © 2025 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

(define-module (gnu packages docker)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (guix build-system python)
  #:use-module (guix build-system pyproject)
  #:use-module (guix utils)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-web)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages virtualization))

;; Note - when changing Docker versions it is important to update the versions
;; of several associated packages (docker-libnetwork and go-sctp).
(define %docker-version "20.10.27")

(define-public python-docker
  (package
    (name "python-docker")
    (version "5.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "docker" version))
       (sha256
        (base32
         "1yr7w8vmdis01myx26pqx7wcyz2cy1mfs421alppq3lpc9ms45nr"))))
    (build-system python-build-system)
    ;; TODO: Tests require a running Docker daemon.
    (arguments '(#:tests? #f))
    (inputs
     (list python-requests python-six python-urllib3))
    (propagated-inputs
     (list python-docker-pycreds python-paramiko ;adds SSH support
           python-websocket-client))
    (home-page "https://github.com/docker/docker-py/")
    (synopsis "Python client for Docker")
    (description "Docker-Py is a Python client for the Docker container
management tool.")
    (license license:asl2.0)))

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
    (build-system python-build-system)
    (native-inputs
     (list python-six))
    (home-page "https://github.com/d11wtq/dockerpty")
    (synopsis "Python library to use the pseudo-TTY of a Docker container")
    (description "Docker PTY provides the functionality needed to operate the
pseudo-terminal (PTY) allocated to a Docker container using the Python
client.")
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
    (build-system python-build-system)
    ;; TODO: Tests require running Docker daemon.
    (arguments '(#:tests? #f))
    (inputs
     (list python-cached-property
           python-distro
           python-docker
           python-dockerpty
           python-docopt
           python-dotenv
           python-jsonschema-3
           python-pyyaml-5
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
        (search-patches "containerd-create-pid-file.patch"))))
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
           go-1.20 gotestsum pkg-config))
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
