;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2016 Jan Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2016, 2017 Mathieu Lirzin <mthl@gnu.org>
;;; Copyright © 2017, 2020, 2021 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2017, 2019, 2020 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2018 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2022, 2024 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2023 David Pflug <david@pflug.io>
;;; Copyright © 2025 David Thompson <davet@gnu.org>
;;; Copyright © 2025-2026 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu packages ci)
  #:use-module ((guix search-paths) #:select ($SSL_CERT_DIR))
  #:use-module ((guix licenses) #:prefix l:)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-check)
  #:use-module (gnu packages golang-compression)
  #:use-module (gnu packages golang-vcs)
  #:use-module (gnu packages golang-web)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages mail)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-compression)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages uglifyjs)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml))

(define-public cuirass
  (package
    (name "cuirass")
    (version "1.3.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://git.guix.gnu.org/cuirass.git")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0b1rjb5vjv72a25w7awz2gaf7bvqjn8l699anigrrc62sjj4mk80"))))
    (build-system gnu-build-system)
    (arguments
     (list #:modules `((guix build utils)
                       (guix build gnu-build-system)
                       (ice-9 match)
                       (ice-9 rdelim)
                       (ice-9 popen)
                       (srfi srfi-1))
           #:configure-flags #~'("--localstatedir=/var" ;for /var/log/cuirass
                                 "--sysconfdir=/etc") ;for /etc/cuirass/forge-tokens

           ;; XXX: HTTP tests fail on aarch64 due to Fibers errors, disable them
           ;; on that architecture for now.
           #:tests? (let ((s (or (%current-target-system)
                                 (%current-system))))
                      (not (string-prefix? "aarch64" s)))
           #:parallel-tests? #f
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'install 'wrap-program
                 (lambda* (#:key inputs outputs #:allow-other-keys)
                   ;; Wrap the 'cuirass' command to refer to the right modules.
                   ;; Do so by collecting the subset of INPUTS that provides Guile
                   ;; modules.  This includes direct inputs as well as their
                   ;; propagated inputs--e.g., 'guix' propagates 'guile-zstd'.
                   (define (sub-directory suffix)
                     (match-lambda
                       ((label . directory)
                        (let ((directory (string-append directory suffix)))
                          (and (directory-exists? directory)
                               directory)))))

                   (let* ((out (assoc-ref outputs "out"))
                          (effective
                           (read-line
                            (open-pipe* OPEN_READ (which "guile") "-c"
                                        "(display (effective-version))")))
                          (mods (filter-map (sub-directory
                                             (string-append
                                              "/share/guile/site/"
                                              effective))
                                            inputs))
                          (objs (filter-map (sub-directory
                                             (string-append
                                              "/lib/guile/" effective
                                              "/site-ccache"))
                                            inputs)))
                     (wrap-program (string-append out "/bin/cuirass")
                       `("PATH" ":" prefix
                         (,(string-append out "/bin")))
                       `("GUILE_LOAD_PATH" ":" prefix
                         ,mods)
                       `("GUILE_LOAD_COMPILED_PATH" ":" prefix
                         ,objs))))))))
    (inputs
     (list bash-minimal
           guile-3.0-latest
           guile-avahi
           guile-fibers
           guile-gcrypt
           guile-json-4
           guile-simple-zmq
           guile-squee
           guile-git
           guile-zlib
           guile-mastodon
           guile-gnutls
           mailutils
           ;; FIXME: this is propagated by "guile-git", but it needs to be among
           ;; the inputs to add it to GUILE_LOAD_PATH.
           guile-bytestructures

           guix))
    (native-inputs
     (list autoconf-2.71
           automake
           pkg-config
           texinfo
           ephemeralpg
           esbuild))
    (native-search-paths
     ;; For HTTPS access, Cuirass itself honors these variables, with the
     ;; same semantics as Git and OpenSSL (respectively).
     (list (search-path-specification
             (variable "GIT_SSL_CAINFO")
             (file-type 'regular)
             (separator #f)                      ;single entry
             (files '("etc/ssl/certs/ca-certificates.crt")))
           $SSL_CERT_DIR))
    (synopsis "Continuous integration system")
    (description
     "Cuirass is a continuous integration tool using GNU Guix.  It is
intended as a replacement for Hydra.")
    (home-page "https://guix.gnu.org/cuirass/")
    (license l:gpl3+)))

(define-public laminar
  (package
    (name "laminar")
    (version "1.3")
    (source
     (origin (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/ohwgiles/laminar")
                   (commit version)))
             (file-name (git-file-name name version))
             (sha256
              (base32
               "0ya5nzl1qf11if564xd49l8ajxyish7pbn875js4f153z775d3ks"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f                      ; TODO Can't build tests
       #:configure-flags
       (list "-DCMAKE_CXX_STANDARD=17"
             ;; "-DBUILD_TESTS=true" TODO: objcopy: js/stPskyUS: can't add
             ;; section '.note.GNU-stack': file format not recognized
             (string-append "-DLAMINAR_VERSION=" ,version))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-CMakeLists.txt
           (lambda _
             (substitute* "CMakeLists.txt"
               (("file\\(DOWNLOAD.*\n$")
                "# file download removed by Guix --")
               (("install\\(FILES etc/laminar.service DESTINATION \\$\\{SYSTEMD\\_UNITDIR\\}\\)")
                "")
               (("install\\(FILES \\$\\{CMAKE\\_CURRENT\\_BINARY\\_DIR\\}\\/laminar\\.service DESTINATION \\$\\{SYSTEMD\\_UNITDIR\\}\\)")
                "")
               (("install\\(FILES etc/laminar\\.conf DESTINATION \\/etc\\)") "")
               (("\\/usr\\/") ""))))
         (add-after 'configure 'copy-in-javascript-and-css
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (use-modules (ice-9 popen))

             (mkdir-p "../build/js")
             (invoke "tar" "-xf" (assoc-ref inputs "chart.js.tgz")
                     "--strip-components" "2"
                     "package/dist/chart.js")
             (for-each (lambda (file minified-file)
                         (let* ((port
                                 (open-pipe* OPEN_READ "uglifyjs" file))
                                (destination
                                 (string-append
                                  "../build/js/" minified-file)))

                           (call-with-output-file destination
                             (lambda (output-port)
                               (dump-port port output-port)))

                           (let ((exit (close-pipe port)))
                             (unless (zero? exit)
                               (error "uglifyjs failed" exit)))))

                       (list (assoc-ref inputs "vue.js")
                             (assoc-ref inputs "vue-router.js")
                             "chart.js")
                       (list "vue.min.js"
                             "vue-router.min.js"
                             "Chart.min.js"))

             ;; ansi_up.js isn't minified
             (copy-file (assoc-ref inputs "ansi_up.js")
                        "../build/js/ansi_up.js"))))))
    (inputs
     (list capnproto rapidjson sqlite boost zlib))
    (native-inputs
     `(("googletest" ,googletest)
       ("uglifyjs" ,node-uglify-js)

       ("vue.js"
        ,(origin (method url-fetch)
                 (uri (string-append "https://raw.githubusercontent.com/"
                                     "vuejs/vue/v2.6.12/dist/vue.js"))
                 (sha256
                  (base32
                   "1mq2dn6yqbmzar77xf4x2bvvanf9xc9nwfq06sksl5zmr300m7qm"))))
       ("vue-router.js"
        ,(origin (method url-fetch)
                 (uri (string-append "https://raw.githubusercontent.com/"
                                     "vuejs/vue-router/v3.4.8/dist/vue-router.js"))
                 (sha256
                  (base32
                   "1hkrbgzhpnrsb4zdafslqagy1vkac6bkdj7kh49js2lhkp9z4nj5"))))
       ("ansi_up.js"
        ,(origin (method url-fetch)
                 (uri (string-append "https://raw.githubusercontent.com/"
                                     "drudru/ansi_up/v4.0.4/ansi_up.js"))
                 (sha256
                  (base32
                   "1dx8wn38ds8d01kkih26fx1yrisg3kpz61qynjr4zil03ap0hrlr"))))
       ("chart.js.tgz"
        ,(origin (method url-fetch)
                 (uri (string-append "https://github.com/chartjs/Chart.js/"
                                     "releases/download/v3.9.1/chart.js-3.9.1.tgz"))
                 (sha256
                  (base32
                   "1ikjgspaknqlhpjad17563yph4pvrh8dkzjdx58pl23gg58hf7hi"))))))
    (synopsis "Lightweight continuous integration service")
    (description
     "Laminar is a lightweight and modular continuous integration service.  It
doesn't have a configuration web UI instead uses version-controllable
configuration files and scripts.

Laminar encourages the use of existing tools such as bash and cron instead of
reinventing them.")
    (home-page "https://laminar.ohwg.net/")
    (license l:gpl3+)))

(define-public go-code-forgejo-org-forgejo-runner-v12
  (package
    (name "go-code-forgejo-org-forgejo-runner-v12")
    (version "12.13.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://code.forgejo.org/forgejo/runner.git")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0bni952r7bqqa1g8rkwi1gvkxx1shi4cjcy58gr8aqfpqavknbza"))))
    (build-system go-build-system)
    (arguments
     (list
      ;; forgejo-runner binary is in forgejo-runner package, this is a Go
      ;; source library to use in inputs for other packages.
      #:skip-build? #t
      #:import-path "code.forgejo.org/forgejo/runner/v12"
      #:build-flags
      #~(list (string-append "-ldflags=-X code.forgejo.org/"
                             "forgejo/runner/v12/internal/pkg/ver.version="
                             #$version))
      #:embed-files #~(list ".*\\.json" ".*\\.js" ".*\\.sh")
      #:test-flags
      #~(list "-vet=off"
              "-skip" (string-join
                       ;; Newtork access and running Docker are required.
                       (list "TestCancelLongRunningCommand"
                             "TestClone/annotated-tag"
                             "TestClone/branch"
                             "TestClone/sha"
                             "TestClone/short-sha"
                             "TestClone/tag"
                             "TestCloneIfRequired/clone"
                             "TestCloneIfRequired/clone_different_remote"
                             "TestHandler"
                             "TestHandlerAPIFatalErrors/commit"
                             "TestHandlerAPIFatalErrors/find"
                             "TestHandlerAPIFatalErrors/get"
                             "TestHandlerAPIFatalErrors/upload"
                             "TestHandler_gcCache"
                             "TestPollerPoll"
                             "TestPollerPoll/fetchTasks_rate_limited"
                             "TestRunJob_WithConnectionFromCommandOptions"
                             "TestRunner_ReusableWorkflowGitHubInstance"
                             "TestStepDockerMain")
                       "|")
              "-args" "-features" "-"
              )))
    (native-inputs
     (list git-minimal/pinned
           go-github-com-google-go-cmp
           go-github-com-spf13-cobra
           go-github-com-spf13-pflag
           go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-code-forgejo-org-forgejo-actions-proto
           go-connectrpc-com-connect
           go-dario-cat-mergo
           go-github-com-masterminds-semver
           go-github-com-avast-retry-go-v4
           go-github-com-containerd-errdefs
           go-github-com-creack-pty
           go-github-com-distribution-reference
           go-github-com-djherbis-buffer
           go-github-com-djherbis-nio-v3
           go-github-com-docker-cli
           go-github-com-docker-docker
           go-github-com-docker-go-connections
           go-github-com-gdgvda-cron
           go-github-com-gobwas-glob
           go-github-com-google-uuid
           go-github-com-joho-godotenv
           go-github-com-julienschmidt-httprouter
           go-github-com-kballard-go-shellquote
           go-github-com-mattn-go-isatty
           go-github-com-moby-go-archive
           go-github-com-moby-patternmatcher
           go-github-com-opencontainers-image-spec
           go-github-com-opencontainers-selinux
           go-github-com-powerman-fileuri
           go-github-com-rhysd-actionlint
           go-github-com-sirupsen-logrus
           go-github-com-timshannon-bolthold
           go-go-etcd-io-bbolt
           go-go-yaml-in-yaml-v3
           go-golang-org-x-sys
           go-golang-org-x-term
           go-golang-org-x-time
           go-google-golang-org-protobuf
           go-gotest-tools-v3))
    (home-page "https://code.forgejo.org/forgejo/runner")
    (synopsis "Run continuous integration jobs for Forgejo")
    (description
     "This package provides a daemon that connects to a Forgejo instance and
runs jobs for continuous integration.  Forgejo Runner can also run workflows
locally and act as a cache.")
    (license l:gpl3)))

(define-public forgejo-runner
  (package/inherit go-code-forgejo-org-forgejo-runner-v12
    (name "forgejo-runner")
    (arguments
     (substitute-keyword-arguments arguments
       ((#:install-source? _ #t) #f)
       ((#:skip-build? _ #t) #f)
       ((#:tests? _ #t) #f)
       ((#:phases phases '%standard-phases)
        #~(modify-phases #$phases
            (add-after 'install 'rename-binary
              (lambda _
                (with-directory-excursion (string-append #$output "/bin")
                  (rename-file "v12" "forgejo-runner"))))))))
    (native-inputs
     (append
      (package-native-inputs go-code-forgejo-org-forgejo-runner-v12)
      (package-propagated-inputs go-code-forgejo-org-forgejo-runner-v12)))
    (propagated-inputs '())
    (inputs '())))
