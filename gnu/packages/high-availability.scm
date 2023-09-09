;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Brice Waegeneire <brice@waegenei.re>
;;; Copyright © 2020, 2022 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2022 Sharlatan Hellseher <sharlatanus@gmail.com>
;;;
;;; This file is part of GNU Guix.
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This module provides package definitions related for High Availability (HA)
;; software, which come as foundation to create clusterization and load
;; balancing of services.

;;; Code:

(define-module (gnu packages high-availability)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages hardware)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages rsync)
  #:use-module (gnu packages syncthing)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages valgrind)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module ((guix licenses)
                #:prefix license:))

(define-public haproxy
  (package
    (name "haproxy")
    (version "2.7.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.haproxy.org/download/"
                           (version-major+minor version)
                           "/src/haproxy-" version ".tar.gz"))
       (sha256
        (base32 "0kxpvrn6iaxhw2f2hrxblns6pnxmrds3vvs9h6nwbkrzvdykagqk"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f  ; there are only regression tests, using varnishtest
      #:make-flags
      #~(list "LUA_LIB_NAME=lua"
              "TARGET=linux-glibc"
              "USE_LUA=1"
              "USE_OPENSSL=1"
              "USE_PCRE2=1"
              "USE_PCRE2_JIT=1"
              "USE_PROMEX=1"
              "USE_ZLIB=1"
              (string-append "CC=" #$(cc-for-target))
              (string-append "DOCDIR=" #$output "/share/" #$name)
              (string-append "LUA_INC=" #$(this-package-input "lua") "/include")
              (string-append "LUA_LIB=" #$(this-package-input "lua") "/lib")
              (string-append "PREFIX=" #$output))
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure))))
    (inputs
     (list lua openssl pcre2 zlib))
    (home-page "https://www.haproxy.org/")
    (synopsis "Reliable, high performance TCP/HTTP load balancer")
    (description "HAProxy offers @acronym{HA, high availability}, load
balancing, and proxying for TCP and HTTP-based applications.  It is particularly
suited to Web sites crawling under very high loads while needing persistence or
Layer 7 processing.  Supporting tens of thousands of connections is clearly
realistic with today's hardware.")
    (license (list license:gpl2+
                   license:lgpl2.1
                   license:lgpl2.1+))))

(define-public libqb
  (package
    (name "libqb")
    (version "2.0.7")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/ClusterLabs/libqb/releases/download/v"
                    version "/libqb-" version ".tar.xz"))
              (sha256
               (base32
                "0xd51wh7gdindh6fzi62r4xp9lkayggb1rqsprqmjkh1m71gnvin"))))
    (build-system gnu-build-system)
    (native-inputs (list pkg-config))
    (inputs (list libxml2))
    (home-page "https://clusterlabs.github.io/libqb/")
    (synopsis
     "Library providing high-performance logging, tracing, IPC, and polling")
    (description
     "Libqb is a library with the primary purpose of providing
high-performance, reusable features for client-server architecture, such as
logging, tracing, @acronym{IPC, inter-process communication} and polling.  Libqb
is not intended to be an all-encompassing library, but instead provide focused
APIs that are highly tuned for maximum performance for client-server
applications.")
    (license license:lgpl2.1)))

(define-public kronosnet
  (package
    (name "kronosnet")
    (version "1.24")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/kronosnet/kronosnet")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1b8rz7f2h3scrq0xcqz58ckzsvv08g31j5jgy2v4i6w87r9c75lw"))))
    (build-system gnu-build-system)
    (arguments
     ;; XXX: Multiple tests failed. Tests
     ;; require very complex environment and for some of them root privileges to
     ;; set network configuration. It has it's own CI based on Jenkis
     ;; https://ci.kronosnet.org/.
     (list #:tests? #f
           #:configure-flags #~'("--disable-static")
           #:phases #~(modify-phases %standard-phases
                        (add-before 'bootstrap 'fix-version-gen
                          (lambda _
                            (call-with-output-file ".tarball-version"
                              (lambda (port)
                                (display #$version port))))))))
    (native-inputs (list autoconf
                         automake
                         doxygen
                         libtool
                         net-tools
                         pkg-config))
    (inputs (list lksctp-tools
                  libnl
                  libqb
                  libxml2
                  lz4
                  lzo
                  nss
                  nspr
                  openssl
                  xz
                  zlib
                  `(,zstd "lib")))
    (home-page "https://kronosnet.org/")
    (synopsis "Network abstraction layer designed for High Availability")
    (description
     "Kronosnet, often referred to as @code{knet}, is a network
 abstraction layer designed for High Availability use cases, where redundancy,
 security, fault tolerance and fast fail-over are the core requirements of
 your application.

 Kronosnet is the new underlying network protocol for Linux HA components
 (Corosync), that features ability to use multiple links between nodes,
 active/active and active/passive link failover policies, automatic link
 recovery, FIPS compliant encryption (nss and/or openssl), automatic PMTUd and
 in general better performances compared to the old network protocol.")
    (license (list license:gpl2+ license:lgpl2.1+))))

(define-public nsq
  (package
    (name "nsq")
    (version "1.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nsqio/nsq")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ajqjwfn06zsmz21z9mkl4cblarypaf20228pqcd1293zl6y3ry8"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/nsqio/nsq"
      #:install-source? #f
      #:phases
      #~(modify-phases %standard-phases
          (replace 'build
            (lambda* (#:key import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (invoke "make"))))
          (replace 'check
            (lambda* (#:key tests? import-path #:allow-other-keys)
              (when tests?
                (setenv "HOME" "/tmp")
                (with-directory-excursion (string-append "src/" import-path)
                  (invoke #$@(if (target-x86?)
                                 (list "go" "test" "-v" "-race" "./...")
                                 (list "go" "test" "-v" "./...")))))))
          (replace 'install
            (lambda* (#:key import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (invoke "make" (string-append "PREFIX=" #$output)
                        "install")))))))
    (native-inputs
     (list go-github-com-bitly-go-hostpool
           go-github-com-bitly-timer-metrics
           go-github-com-blang-semver
           go-github-com-bmizerany-perks-quantile
           go-github-com-burntsushi-toml
           go-github-com-davecgh-go-spew
           go-github-com-golang-snappy ; Move to (gnu packages golang)
           go-github-com-julienschmidt-httprouter
           go-github-com-mreiferson-go-options
           go-github-com-mreiferson-go-svc
           go-github-com-nsqio-go-diskqueue
           go-github-com-nsqio-go-nsq
           python-wrapper))
    (home-page "https://nsq.io")
    (synopsis "Realtime distributed messaging platform")
    (description
     "NSQ is a realtime distributed messaging platform designed to operate at
scale, handling billions of messages per day.

Key features:
@itemize
@item support distributed topologies without @acronym{SPOF, Single Point of
Failure}
@item scale horizontally (no brokers, seamlessly add more nodes to the
cluster)
@item low-latency push based message delivery (performance)
@item combine load-balanced and multicast style message routing
@item excel at both streaming (high-throughput) and job oriented
(low-throughput) workloads
@item primarily in-memory (beyond a high-water mark messages are transparently
kept on disk)
@item runtime discovery service for consumers to find producers (nsqlookupd)
@item transport layer security (TLS)
@item data format agnostic
@item few dependencies (easy to deploy) and a sane, bounded, default
configuration
@item simple TCP protocol supporting client libraries in any language
@item HTTP interface for stats, admin actions, and producers (no client
library needed to publish)
@item integrate with @acronym{StatsD, Stats aggregation Daemon} for realtime
instrumentation
@item robust cluster administration interface (nsqadmin)
@end itemize")
    (license license:expat)))

(define-public corosync
  (package
    (name "corosync")
    (version "3.1.6")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/corosync/corosync")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "03g3qnm5acmk7jry6kspvkssbiv8k39749bic2f0cj3ckkwy2li4"))))
    (build-system gnu-build-system)
    (arguments
     (list #:configure-flags #~'("--disable-static")
           #:phases #~(modify-phases %standard-phases
                        (add-before 'bootstrap 'fix-version-gen
                          (lambda _
                            (call-with-output-file ".tarball-version"
                              (lambda (port)
                                (display #$version port))))))))
    (native-inputs (list autoconf automake libtool pkg-config))
    (inputs (list kronosnet libqb))
    (home-page "https://corosync.github.io/corosync/")
    (synopsis
     "Group communication system for implementing High Availability in applications")
    (description
     "The Corosync Cluster Engine is a Group Communication System with additional
features for implementing high availability within applications.  The project
provides four C Application Programming Interface features:

@itemize

@item A closed process group communication model with extended virtual synchrony
guarantees for creating replicated state machines.

@item A simple availability manager that restarts the application process when
it has failed.

@item A configuration and statistics in-memory database that provide the ability
to set, retrieve, and receive change notifications of information.

@item A quorum system that notifies applications when quorum is achieved or
lost.

@end itemize")
    (license (list license:bsd-0 license:gpl3+))))

(define-public pacemaker
  (package
    (name "pacemaker")
    (version "2.1.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ClusterLabs/pacemaker")
                    (commit (string-append "Pacemaker-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "04gfd7i3w0zbzv7vi7728lgbyjq7cbqpr7jsp501piwg3z5j4mvb"))))
    (build-system gnu-build-system)
    (arguments
     (list #:configure-flags #~(list "--with-corosync" "--disable-static"
                                     (string-append "--with-initdir="
                                                    #$output "/etc/init.d")
                                     (string-append "--with-ocfdir="
                                                    #$output "/lib"))))
    (native-inputs (list autoconf
                         automake
                         cmocka
                         gettext-minimal
                         libtool
                         pkg-config
                         rsync
                         util-linux
                         valgrind))
    (inputs (list dbus
                  corosync
                  glib
                  gnutls
                  libqb
                  libxml2
                  libxslt
                  python
                  `(,util-linux "lib")))
    (home-page "https://www.clusterlabs.org/pacemaker/")
    (synopsis "Scalable High-Availability cluster resource manager")
    (description
     "Pacemaker is a high-availability cluster resource manager.

It achieves maximum availability for your cluster services (a.k.a. resources) by
detecting and recovering from node- and resource-level failures by making use of
the messaging and membership capabilities provided by Corosync.

It can do this for clusters of practically any size and comes with a powerful
dependency model that allows the administrator to accurately express the
relationships (both ordering and location) between the cluster resources.

Virtually anything that can be scripted can be managed as part of a Pacemaker cluster.")
    (license (list license:cc-by4.0 license:gpl2+ license:lgpl2.1+))))
