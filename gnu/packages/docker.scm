;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 David Thompson <davet@gnu.org>
;;; Copyright © 2018 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (guix build-system python)
  #:use-module (guix utils)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages virtualization))

(define %docker-version "18.09.5")

(define-public python-docker-py
  (package
    (name "python-docker-py")
    (version "3.7.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "docker" version))
       (sha256
        (base32
         "0qmrcvpaz37p85hfddsd4yc8hgqlkzs4cz09q9wmy0pz5pwajqm0"))))
    (build-system python-build-system)
    ;; TODO: Tests require a running Docker daemon.
    (arguments '(#:tests? #f))
    (inputs
     `(("python-requests" ,python-requests-2.20)
       ("python-ipaddress" ,python-ipaddress)
       ("python-six" ,python-six)
       ("python-urllib3" ,python-urllib3-1.24)
       ("python-websocket-client" ,python-websocket-client)))
    (propagated-inputs
     `(("python-docker-pycreds" ,python-docker-pycreds)
       ("python-paramiko" ,python-paramiko))) ; adds SSH support
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
     `(("python-six" ,python-six)))
    (home-page "https://github.com/d11wtq/dockerpty")
    (synopsis "Python library to use the pseudo-TTY of a Docker container")
    (description "Docker PTY provides the functionality needed to operate the
pseudo-terminal (PTY) allocated to a Docker container using the Python
client.")
    (license license:asl2.0)))

;; When updating, check whether python-jsonschema-2.6 can be removed from Guix
;; entirely.
(define-public docker-compose
  (package
    (name "docker-compose")
    (version "1.24.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "docker-compose" version))
       (sha256
        (base32
         "0lx7bx6jvhydbab8vwry0bclhdf0dfj6jrns1m5y45yp9ybqxmd5"))))
    (build-system python-build-system)
    ;; TODO: Tests require running Docker daemon.
    (arguments '(#:tests? #f))
    (inputs
     `(("python-cached-property"
        ,python-cached-property)
       ("python-docker-py" ,python-docker-py)
       ("python-dockerpty" ,python-dockerpty)
       ("python-docopt" ,python-docopt)
       ("python-jsonschema" ,python-jsonschema-2.6)
       ("python-pyyaml" ,python-pyyaml)
       ("python-requests" ,python-requests-2.20)
       ("python-six" ,python-six)
       ("python-texttable" ,python-texttable)
       ("python-websocket-client" ,python-websocket-client)))
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
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-versioning
           (lambda _
             (substitute* "test-requirements.txt"
               (("3.0.2") ,(package-version python-pytest))
               (("2.3.1") ,(package-version python-pytest-cov))
               (("2.4.1") ,(package-version python-flake8)))
             #t)))))
    (native-inputs
     `(("python-flake8" ,python-flake8)
       ("python-pytest" ,python-pytest)
       ("python-pytest-cov" ,python-pytest-cov)))
    (propagated-inputs
     `(("python-six" ,python-six)))
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
    (version "1.2.5")
    (source
     (origin
      (method git-fetch)
      (uri (git-reference
            (url "https://github.com/containerd/containerd.git")
            (commit (string-append "v" version))))
      (file-name (git-file-name name version))
      (sha256
       (base32 "0npbzixf3c0jvzm159vygvkydrr8h36c9sq50yv0mdinrys2bvg0"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/containerd/containerd"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'chdir
           (lambda _
             (chdir "src/github.com/containerd/containerd")
             #t))
         (add-after 'chdir 'patch-paths
           (lambda* (#:key inputs outputs #:allow-other-keys)
             ;; TODO: Patch "socat", "unpigz".
             (substitute* "./runtime/v1/linux/runtime.go"
              (("defaultRuntime[ \t]*=.*")
               (string-append "defaultRuntime = \""
                              (assoc-ref inputs "runc")
                              "/sbin/runc\"\n"))
              (("defaultShim[ \t]*=.*")
               (string-append "defaultShim = \""
                              (assoc-ref outputs "out")
                              "/bin/containerd-shim\"\n")))
            (substitute* "./vendor/github.com/containerd/go-runc/runc.go"
              (("DefaultCommand[ \t]*=.*")
               (string-append "DefaultCommand = \""
                              (assoc-ref inputs "runc")
                              "/sbin/runc\"\n")))
            (substitute* "vendor/github.com/containerd/continuity/testutil/loopback/loopback_linux.go"
             (("exec\\.Command\\(\"losetup\"") ; )
              (string-append "exec.Command(\""
                             (assoc-ref inputs "util-linux")
                             "/sbin/losetup\""))) ;)
             #t))
         (replace 'build
           (lambda* (#:key (make-flags '()) #:allow-other-keys)
             (apply invoke "make" make-flags)))
         (replace 'install
           (lambda* (#:key outputs (make-flags '()) #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out")))
               (apply invoke "make" (string-append "DESTDIR=" out) "install"
                      make-flags)))))))
    (inputs
     `(("btrfs-progs" ,btrfs-progs)
       ("libseccomp" ,libseccomp)
       ("runc" ,runc)
       ("util-linux" ,util-linux)))
    (native-inputs
     `(("go" ,go)
       ("pkg-config" ,pkg-config)))
    (synopsis "Docker container runtime")
    (description "This package provides the container daemon for Docker.
It includes image transfer and storage, container execution and supervision,
network attachments.")
    (home-page "http://containerd.io/")
    (license license:asl2.0)))

;;; Private package that shouldn't be used directly; its purposes is to be
;;; used as a template for the various packages it contains.  It doesn't build
;;; anyway, as it needs many dependencies that aren't being satisfied.
(define docker-libnetwork
  ;; There are no recent release for libnetwork, so choose the last commit of
  ;; the branch that Docker uses, as can be seen in the Docker source file
  ;; 'hack/dockerfile/install/proxy.installer'.
  (let ((commit "4725f2163fb214a6312f3beae5991f838ec36326")
        (version "18.09")
        (revision "1"))
    (package
      (name "docker-libnetwork")
      (version (git-version version "1" commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/docker/libnetwork.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1zpnxki8qfzha6ljahpwd3vkzmjhsvkmf73w6crm4ilxxw5vnpfb"))
                ;; Delete bundled ("vendored") free software source code.
                (modules '((guix build utils)))
                (snippet '(begin
                            (delete-file-recursively "vendor")
                            #t))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "github.com/docker/libnetwork/"))
      (home-page "https://github.com/docker/libnetwork/")
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
     `(#:import-path "github.com/docker/libnetwork/cmd/proxy"
       #:unpack-path "github.com/docker/libnetwork"
       #:install-source? #f))
    (native-inputs
     `(("go-sctp" ,go-sctp)
       ;; For tests.
       ("logrus" ,go-github-com-sirupsen-logrus)
       ("go-netlink" ,go-netlink)
       ("go-netns" ,go-netns)
       ("go-golang-org-x-crypto"
        ,go-golang-org-x-crypto)
       ("go-golang-org-x-sys" ,go-golang-org-x-sys)))
    (synopsis "Docker user-space proxy")
    (description "A proxy running in the user space.  It is used by the
built-in registry server of Docker.")
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
             (url "https://github.com/docker/engine.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0cirpd9l2qazp2jyanwzvrkx2m98nksjdvn43ff38p89w6133ipb"))
       (patches
        (search-patches "docker-engine-test-noinstall.patch"
                        "docker-fix-tests.patch"
                        "docker-use-fewer-modprobes.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:modules
       ((guix build gnu-build-system)
        ((guix build go-build-system) #:prefix go:)
        (guix build union)
        (guix build utils))
       #:imported-modules
       (,@%gnu-build-system-modules
        (guix build union)
        (guix build go-build-system))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "builder/builder-next/executor_unix.go"
               (("CommandCandidates:.*runc.*")
                (string-append "CommandCandidates: []string{\""
                               (assoc-ref inputs "runc")
                               "/sbin/runc\"},\n")))
             (substitute* "vendor/github.com/containerd/go-runc/runc.go"
               (("DefaultCommand = .*")
                (string-append "DefaultCommand = \""
                               (assoc-ref inputs "runc")
                               "/sbin/runc\"\n")))
             (substitute* "vendor/github.com/containerd/containerd/runtime/v1/linux/runtime.go"
               (("defaultRuntime[ \t]*=.*")
                (string-append "defaultRuntime = \""
                               (assoc-ref inputs "runc")
                               "/sbin/runc\"\n"))
               (("defaultShim[ \t]*=.*")
                (string-append "defaultShim = \""
                               (assoc-ref inputs "containerd")
                               "/bin/containerd-shim\"\n")))
             (substitute* "daemon/daemon_unix.go"
               (("DefaultShimBinary = .*")
                (string-append "DefaultShimBinary = \""
                               (assoc-ref inputs "containerd")
                               "/bin/containerd-shim\"\n"))
               (("DefaultRuntimeBinary = .*")
                (string-append "DefaultRuntimeBinary = \""
                               (assoc-ref inputs "runc")
                               "/sbin/runc\"\n"))
               (("DefaultRuntimeName = .*")
                (string-append "DefaultRuntimeName = \""
                               (assoc-ref inputs "runc")
                               "/sbin/runc\"\n")))
             (substitute* "daemon/config/config.go"
               (("StockRuntimeName = .*")
                (string-append "StockRuntimeName = \""
                               (assoc-ref inputs "runc")
                               "/sbin/runc\"\n"))
               (("DefaultInitBinary = .*")
                (string-append "DefaultInitBinary = \""
                               (assoc-ref inputs "tini")
                               "/bin/tini\"\n")))
             (substitute* "daemon/config/config_common_unix_test.go"
               (("expectedInitPath: \"docker-init\"")
                (string-append "expectedInitPath: \""
                               (assoc-ref inputs "tini")
                               "/bin/tini\"")))
             (substitute* "vendor/github.com/moby/buildkit/executor/runcexecutor/executor.go"
               (("var defaultCommandCandidates = .*")
                (string-append "var defaultCommandCandidates = []string{\""
                               (assoc-ref inputs "runc") "/sbin/runc\"}")))
             (substitute* "vendor/github.com/docker/libnetwork/portmapper/proxy.go"
               (("var userlandProxyCommandName = .*")
                (string-append "var userlandProxyCommandName = \""
                               (assoc-ref inputs "docker-proxy")
                               "/bin/proxy\"\n")))
             (substitute* "pkg/archive/archive.go"
               (("string\\{\"xz")
                (string-append "string{\"" (assoc-ref inputs "xz") "/bin/xz")))
             (let ((source-files (filter (lambda (name)
                                           (not (string-contains name "test")))
                                         (find-files "." "\\.go$"))))
               (let-syntax ((substitute-LookPath*
                             (syntax-rules ()
                               ((_ (source-text package relative-path) ...)
                                (substitute* source-files
                                  (((string-append "\\<exec\\.LookPath\\(\""
                                                   source-text
                                                   "\")"))
                                   (string-append "\""
                                                  (assoc-ref inputs package)
                                                  "/" relative-path
                                                  "\", error(nil)")) ...))))
                            (substitute-Command*
                             (syntax-rules ()
                               ((_ (source-text package relative-path) ...)
                                (substitute* source-files
                                  (((string-append "\\<(re)?exec\\.Command\\(\""
                                                   source-text
                                                   "\"") _ re?)
                                   (string-append (if re? re? "")
                                                  "exec.Command(\""
                                                  (assoc-ref inputs package)
                                                  "/" relative-path
                                                  "\"")) ...)))))
                 (substitute-LookPath*
                  ("ps" "procps" "bin/ps")
                  ("mkfs.xfs" "xfsprogs" "bin/mkfs.xfs")
                  ("lvmdiskscan" "lvm2" "sbin/lvmdiskscan")
                  ("pvdisplay" "lvm2" "sbin/pvdisplay")
                  ("blkid" "util-linux" "sbin/blkid")
                  ("unpigz" "pigz" "bin/unpigz")
                  ("iptables" "iptables" "sbin/iptables")
                  ("iptables-legacy" "iptables" "sbin/iptables")
                  ("ip" "iproute2" "sbin/ip"))

                 (substitute-Command*
                  ("modprobe" "kmod" "bin/modprobe")
                  ("pvcreate" "lvm2" "sbin/pvcreate")
                  ("vgcreate" "lvm2" "sbin/vgcreate")
                  ("lvcreate" "lvm2" "sbin/lvcreate")
                  ("lvconvert" "lvm2" "sbin/lvconvert")
                  ("lvchange" "lvm2" "sbin/lvchange")
                  ("mkfs.xfs" "xfsprogs" "sbin/mkfs.xfs")
                  ("xfs_growfs" "xfsprogs" "sbin/xfs_growfs")
                  ("mkfs.ext4" "e2fsprogs" "sbin/mkfs.ext4")
                  ("tune2fs" "e2fsprogs" "sbin/tune2fs")
                  ("blkid" "util-linux" "sbin/blkid")
                  ("resize2fs" "e2fsprogs" "sbin/resize2fs")
                  ("ps" "procps" "bin/ps")
                  ("losetup" "util-linux" "sbin/losetup")
                  ("uname" "coreutils" "bin/uname")
                  ("dbus-launch" "dbus" "bin/dbus-launch")
                  ("git" "git" "bin/git")))
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
                 ;; Fail on other unsubstituted LookPaths.
                 (("\\<LookPath\\(\"") "Guix_doesnt_want_LookPath\\(\"")
                 (("\\<LooxPath") "LookPath")))
             #t))
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
             (delete-file "daemon/graphdriver/btrfs/btrfs_test.go")
             (delete-file "daemon/graphdriver/overlay/overlay_test.go")
             (delete-file "daemon/graphdriver/overlay2/overlay_test.go")
             #t))
         (replace 'configure
           (lambda _
             (setenv "DOCKER_GITCOMMIT" (string-append "v" ,%docker-version))
             ;; Automatically use bundled dependencies.
             ;; TODO: Unbundle - see file "vendor.conf".
             (setenv "AUTO_GOPATH" "1")
             ;; Respectively, strip the symbol table and debug
             ;; information, and the DWARF symbol table.
             (setenv "LDFLAGS" "-s -w")
             ;; Make build faster
             (setenv "GOCACHE" "/tmp")
             #t))
         (add-before 'build 'setup-go-environment
           (assoc-ref go:%standard-phases 'setup-go-environment))
         (replace 'build
           (lambda _
             ;; Our LD doesn't like the statically linked relocatable things
             ;; that go produces, so install the dynamic version of
             ;; dockerd instead.
             (invoke "hack/make.sh" "dynbinary")))
         (replace 'check
           (lambda _
             ;; The build process generated a file because the environment
             ;; variable "AUTO_GOPATH" was set.  Use it.
             (setenv "GOPATH" (string-append (getcwd) "/.gopath"))
             ;; ".gopath/src/github.com/docker/docker" is a link to the current
             ;; directory and chdir would canonicalize to that.
             ;; But go needs to have the uncanonicalized directory name, so
             ;; store that.
             (setenv "PWD" (string-append (getcwd)
                                          "/.gopath/src/github.com/docker/docker"))
             (with-directory-excursion ".gopath/src/github.com/docker/docker"
               (invoke "hack/test/unit"))
             (setenv "PWD" #f)
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (out-bin (string-append out "/bin")))
               (install-file "bundles/dynbinary-daemon/dockerd" out-bin)
               (install-file "bundles/dynbinary-daemon/dockerd-dev" out-bin)
               #t))))))
    (inputs
     `(("btrfs-progs" ,btrfs-progs)
       ("containerd" ,containerd)       ; for containerd-shim
       ("coreutils" ,coreutils)
       ("dbus" ,dbus)
       ("docker-proxy" ,docker-libnetwork-cmd-proxy)
       ("e2fsprogs" ,e2fsprogs)
       ("git" ,git)
       ("iproute2" ,iproute)
       ("iptables" ,iptables)
       ("kmod" ,kmod)
       ("libseccomp" ,libseccomp)
       ("pigz" ,pigz)
       ("procps" ,procps)
       ("runc" ,runc)
       ("util-linux" ,util-linux)
       ("lvm2" ,lvm2)
       ("tini" ,tini)
       ("xfsprogs" ,xfsprogs)
       ("xz" ,xz)))
    (native-inputs
     `(("eudev" ,eudev)      ; TODO: Should be propagated by lvm2 (.pc -> .pc)
       ("go" ,go)
       ("pkg-config" ,pkg-config)))
    (synopsis "Docker container component library, and daemon")
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
            (url "https://github.com/docker/cli.git")
            (commit (string-append "v" version))))
      (file-name (git-file-name name version))
      (sha256
       (base32 "0mxxjzkwdny8p2dmyjich7x1gn7hdlfppzjy2skk2k5bwv7nxpmi"))))
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
             (symlink "src/github.com/docker/cli/docker.Makefile" "./docker.Makefile")
             #t))
         (replace 'build
           (lambda _
             (invoke "./scripts/build/dynbinary")))
         (replace 'check
           (lambda* (#:key make-flags tests? #:allow-other-keys)
             (setenv "PATH" (string-append (getcwd) "/build:" (getenv "PATH")))
             (if tests?
                 ;; Use the newly-built docker client for the tests.
                 (with-directory-excursion "src/github.com/docker/cli"
                   ;; TODO: Run test-e2e as well?
                   (apply invoke "make" "-f" "docker.Makefile" "test-unit"
                          (or make-flags '())))
                 #t)))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (out-bin (string-append out "/bin")))
               (chdir "build")
               (install-file "docker" out-bin)
               #t))))))
    (native-inputs
     `(("go" ,go)
       ("libltdl" ,libltdl)
       ("pkg-config" ,pkg-config)))
    (synopsis "Command line interface to Docker")
    (description "This package provides a command line interface to Docker.")
    (home-page "https://www.docker.com/")
    (license license:asl2.0)))

(define-public cqfd
  (package
    (name "cqfd")
    (version "5.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/savoirfairelinux/cqfd.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1xhydz01f2rrnw7rmnaxh3f3q1ariq7j2ig0i0w1p3wn10l3q0nv"))))
    (build-system gnu-build-system)
    (arguments
     ;; The test suite requires a docker daemon and connectivity.
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               ;; Fix the directory of the bash completion.
               (substitute* "Makefile"
                 (("completionsdir=.*$")
                  (string-append "completionsdir=" out
                                 "/etc/bash_completion.d; \\\n")))
               (invoke "make" "install"
                       (string-append "PREFIX=" out))))))))
    (home-page "https://github.com/savoirfairelinux/cqfd")
    (synopsis "Convenience wrapper for Docker")
    (description "cqfd is a Bash script that provides a quick and convenient
way to run commands in the current directory, but within a Docker container
defined in a per-project configuration file.")
    (license license:gpl3+)))

(define-public tini
  (package
    (name "tini")
    (version "0.18.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/krallin/tini.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1h20i3wwlbd8x4jr2gz68hgklh0lb0jj7y5xk1wvr8y58fip1rdn"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f                    ;tests require a Docker daemon
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'disable-static-build
                    ;; Disable the static build as it fails to install, with
                    ;; the error: "No valid ELF RPATH or RUNPATH entry exists
                    ;; in the file".
                    (lambda _
                      (substitute* "CMakeLists.txt"
                        ((".*tini-static.*") ""))
                      #t)))))
    (home-page "https://github.com/krallin/tini")
    (synopsis "Tiny but valid init for containers")
    (description "Tini is an init program specifically designed for use with
containers.  It manages a single child process and ensures that any zombie
processes produced from it are reaped and that signals are properly forwarded.
Tini is integrated with Docker.")
    (license license:expat)))
