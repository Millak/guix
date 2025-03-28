;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2021, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2018 Sou Bunnbu <iyzsong@member.fsf.org>
;;; Copyright © 2017, 2018, 2019, 2020 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2018, 2020, 2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Gábor Boskovits <boskovits@gmail.com>
;;; Copyright © 2018, 2019, 2020 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2020 Alex ter Weele <alex.ter.weele@gmail.com>
;;; Copyright © 2020 Lars-Dominik Braun <ldb@leibniz-psychology.org>
;;; Copyright © 2021, 2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2021 Stefan Reichör <stefan@xsteve.at>
;;; Copyright © 2021 Raphaël Mélotte <raphael.melotte@mind.be>
;;; Copyright © 2022 Paul A. Patience <paul@apatience.com>
;;; Copyright © 2022 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2022 ( <paren@disroot.org>
;;; Copyright © 2022 Mathieu Laparie <mlaparie@disr.it>
;;; Copyright © 2025 Nicolas Graves <ngraves@ngraves.fr>
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

(define-module (gnu packages monitoring)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages django)
  #:use-module (gnu packages gd)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gnome)               ;libnotify
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-web)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages image)
  #:use-module (gnu packages mail)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages prometheus)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages rrdtool)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages web))

(define-public nagios
  (package
    (name "nagios")
    (version "4.4.6")
    ;; XXX: Nagios 4.2.x and later bundle a copy of AngularJS.
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://sourceforge/nagios/nagios-4.x/nagios-"
                    version "/nagios-" version ".tar.gz"))
              (sha256
               (base32
                "1x5hb97zbvkm73q53ydp1gwj8nnznm72q9c4rm6ny7phr995l3db"))
              (modules '((guix build utils)))
              (snippet
               ;; Ensure reproducibility.
               '(begin
                  (substitute* (find-files "cgi" "\\.c$")
                    (("__DATE__") "\"1970-01-01\"")
                    (("__TIME__") "\"00:00:00\""))
                  #t))))
    (build-system gnu-build-system)
    (native-inputs
     (list unzip))
    (inputs
     `(("zlib" ,zlib)
       ("libpng-apng" ,libpng)
       ("gd" ,gd)
       ("perl" ,perl)
       ("mailutils" ,mailutils)))
    (arguments
     '(#:configure-flags (list "--sysconfdir=/etc"

                               ;; 'include/locations.h.in' defines file
                               ;; locations, and many things go directly under
                               ;; LOCALSTATEDIR, hence the extra '/nagios'.
                               "--localstatedir=/var/nagios"

                               (string-append
                                "--with-mail="
                                (assoc-ref %build-inputs "mailutils")
                                "/bin/mail"))
       #:make-flags '("all")
       #:phases (modify-phases %standard-phases
                  (add-before 'build 'do-not-chown-to-nagios
                    (lambda _
                      ;; Makefiles do 'install -o nagios -g nagios', which
                      ;; doesn't work for us.
                      (substitute* (find-files "." "^Makefile$")
                        (("-o nagios -g nagios")
                         ""))
                      #t))
                  (add-before 'build 'do-not-create-sysconfdir
                    (lambda _
                      ;; Don't try to create /var upon 'make install'.
                      (substitute* "Makefile"
                        (("\\$\\(INSTALL\\).*\\$\\(LOGDIR\\).*$" all)
                         (string-append "# " all))
                        (("\\$\\(INSTALL\\).*\\$\\(CHECKRESULTDIR\\).*$" all)
                         (string-append "# " all))
                        (("chmod g\\+s.*" all)
                         (string-append "# " all)))
                      #t))
                  (add-before 'build 'set-html/php-directory
                    (lambda _
                      ;; Install HTML and PHP files under 'share/nagios/html'
                      ;; instead of just 'share/'.
                      (substitute* '("html/Makefile" "Makefile")
                        (("HTMLDIR=.*$")
                         "HTMLDIR = $(datarootdir)/nagios/html\n"))
                      #t)))
       #:tests? #f))                             ;no 'check' target or similar
    (home-page "https://www.nagios.org/")
    (synopsis "Host, service, and network monitoring program")
    (description
     "Nagios is a host, service, and network monitoring program written in C.
CGI programs are included to allow you to view the current status, history,
etc. via a Web interface.  Features include:

@itemize
@item Monitoring of network services (via SMTP, POP3, HTTP, PING, etc).
@item Monitoring of host resources (processor load, disk usage, etc.).
@item A plugin interface to allow for user-developed service monitoring
  methods.
@item Ability to define network host hierarchy using \"parent\" hosts,
  allowing detection of and distinction between hosts that are down
  and those that are unreachable.
@item Notifications when problems occur and get resolved (via email,
  pager, or user-defined method).
@item Ability to define event handlers for proactive problem resolution.
@item Automatic log file rotation/archiving.
@item Optional web interface for viewing current network status,
  notification and problem history, log file, etc.
@end itemize\n")
    (license license:gpl2)))

(define-public zabbix-agentd
  (package
    (name "zabbix-agentd")
    (version "6.0.14")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://cdn.zabbix.com/zabbix/sources/stable/"
             (version-major+minor version) "/zabbix-" version ".tar.gz"))
       (sha256
        (base32 "0n6fqa9vbhh2syxii7ds2x6dnplrgrj98by1zl0ij1wfbnbxa6k3"))
       (modules '((guix build utils)))
       (snippet
        '(substitute* '("src/zabbix_proxy/proxy.c"
                        "src/zabbix_server/server.c")
           ;; 'fping' must be setuid, so look for it in the usual location.
           (("/usr/sbin/fping6?")
            "/run/privileged/bin/fping")))))
    (build-system gnu-build-system)
    (arguments
     (list #:configure-flags
           #~(list "--enable-agent"
                   "--enable-ipv6"
                   "--with-libpcre2"
                   "--with-gnutls"
                   (string-append "--with-gnutls="
                                  (assoc-ref %build-inputs "gnutls")))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list gnutls pcre2))
    (home-page "https://www.zabbix.com/")
    (synopsis "Distributed monitoring solution (client-side agent)")
    (description "This package provides a distributed monitoring
solution (client-side agent)")
    (license license:gpl2+)
    (properties
     '((release-monitoring-url . "https://www.zabbix.com/download_sources")
       (upstream-name . "zabbix")))))

(define-public zabbix-agent2
  (package/inherit zabbix-agentd
    (name "zabbix-agent2")
    (arguments
     (list #:configure-flags
           #~(list "--disable-agent"
                   "--enable-agent2"
                   "--enable-ipv6"
                   "--with-libpcre2"
                   ;; agent2 only supports OpenSSL.
                   (string-append "--with-openssl="
                                  (dirname (dirname
                                            (search-input-file
                                             %build-inputs "lib/libssl.so")))))
           #:make-flags
           #~'("BUILD_TIME=00:00:01" "BUILD_DATE=Jan 1 1970")
           #:phases
           #~(modify-phases %standard-phases
               (add-before 'build 'set-HOME
                 (lambda _
                   (setenv "HOME" "/tmp"))))))
    (native-inputs
     (list go pkg-config))
    (inputs
     (list openssl pcre2 zlib))))

(define-public zabbix-server
  (package
    (inherit zabbix-agentd)
    (name "zabbix-server")
    (outputs '("out" "front-end" "schema"))
    (arguments
     (substitute-keyword-arguments (package-arguments zabbix-agentd)
       ((#:phases phases '%standard-phases)
        #~(modify-phases #$phases
            (add-after 'install 'install-front-end
               (lambda* (#:key outputs #:allow-other-keys)
                 (let* ((php (string-append (assoc-ref outputs "front-end")
                                            "/share/zabbix/php"))
                        (front-end-conf (string-append php "/conf"))
                        (etc (string-append php "/etc")))
                   (mkdir-p php)
                   (copy-recursively "ui" php)
                   ;; Make front-end read config from ‘/etc/zabbix’ directory.
                   (rename-file front-end-conf
                                (string-append front-end-conf "-example"))
                   (symlink "/etc/zabbix" front-end-conf))))
             (add-after 'install 'install-schema
               (lambda* (#:key outputs #:allow-other-keys)
                 (let ((database-directory
                        (string-append (assoc-ref outputs "schema")
                                       "/database")))
                   (for-each delete-file
                             (find-files "database" "Makefile\\.in|\\.am$"))
                   (mkdir-p database-directory)
                   (copy-recursively "database" database-directory))))))
       ((#:configure-flags flags ''())
        #~(append (list "--enable-server"
                        "--with-postgresql"
                        (string-append "--with-libevent="
                                       (assoc-ref %build-inputs "libevent"))
                        "--with-net-snmp"
                        "--with-libcurl"
                        (string-append "--with-zlib="
                                       (assoc-ref %build-inputs "zlib")))
                  (delete "--enable-agent" #$flags)))))
    (inputs
     (modify-inputs (package-inputs zabbix-agentd)
       (prepend curl
                libevent
                net-snmp
                postgresql
                zlib)))
    (synopsis "Distributed monitoring solution (server-side)")
    (description "This package provides a distributed monitoring
solution (server-side)")))

(define-public zabbix-cli
  (package
    (name "zabbix-cli")
    (version "2.3.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/unioslo/zabbix-cli")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1al3gwbnh0wbal5gq2apq27kh61q8dz355k4w8kx073b4y4nz1wb"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'use-absolute-ncurses
                 (lambda* (#:key inputs #:allow-other-keys)
                   (let ((clear (search-input-file inputs "bin/clear")))
                     (substitute* "bin/zabbix-cli"
                       (("'clear'")
                        (string-append "'" clear "'"))))))
               (add-after 'unpack 'patch-setup.py
                 (lambda _
                   ;; Install data_files to $out/share instead of /usr/share.
                   (substitute* "setup.py"
                     (("/usr/") ""))))
               (add-after 'build 'build-docs
                 (lambda _
                   (invoke "make" "-C" "docs" "manual")
                   (invoke "make" "-C" "docs" "singlehtml")))
               (add-after 'install 'install-docs
                 (lambda _
                   (install-file "docs/_build/man/zabbix-cli.1"
                                 (string-append #$output "/share/man/man1"))
                   (copy-recursively "docs/_build/singlehtml"
                                     (string-append #$output "/share/doc/"
                                                    #$name "/html")))))))
    (native-inputs
     (list python-pytest
           python-setuptools
           python-sphinx
           python-wheel))
    (inputs
     (list ncurses python-requests))
    (home-page "https://github.com/unioslo/zabbix-cli")
    (synopsis "Command-line interface to Zabbix")
    (description
     "@command{zabbix-cli} is a command-line client for the Zabbix
monitoring system.  It can configure and display various aspects of Zabbix
through a text-based interface.")
    (license license:gpl3+)))

(define-public python-pyzabbix
  (package
    (name "python-pyzabbix")
    (version "1.2.1")
    (home-page "https://github.com/lukecyca/pyzabbix")
    ;; No tests on PyPI, use the git checkout.
    (source
     (origin
       (method git-fetch)
       (uri (git-reference (url home-page) (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0ad5xac67brmwc3wd0f87pjplly3cqyrz1dp725lzz2hrjgiaqi8"))))
    (build-system python-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (replace 'check
                    (lambda* (#:key tests? #:allow-other-keys)
                      (if tests?
                          (invoke "pytest" "-vv" "tests")
                          (format #t "test suite not run~%")))))))
    (native-inputs
     ;; For tests.
     (list python-requests-mock python-pytest))
    (propagated-inputs
     (list python-packaging python-requests))
    (synopsis "Python interface to the Zabbix API")
    (description
     "@code{pyzabbix} is a Python module for working with the Zabbix API.")
    (license license:lgpl2.1+)))

(define-public darkstat
  (package
    (name "darkstat")
    (version "3.0.719")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://unix4lyfe.org/darkstat/darkstat-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "1mzddlim6dhd7jhr4smh0n2fa511nvyjhlx76b03vx7phnar1bxf"))))
    (build-system gnu-build-system)
    (arguments '(#:tests? #f))          ; no tests
    (inputs
     (list libpcap zlib))
    (home-page "https://unix4lyfe.org/darkstat/")
    (synopsis "Network statistics gatherer")
    (description
     "@command{darkstat} is a packet sniffer that runs as a background process,
gathers all sorts of statistics about network usage, and serves them over
HTTP.  Features:

@itemize
@item Traffic graphs, reports per host, shows ports for each host.
@item Embedded web-server with deflate compression.
@item Asynchronous reverse DNS resolution using a child process.
@item Small.  Portable.  Single-threaded.  Efficient.
@item Supports IPv6.
@end itemize")
    (license license:gpl2)))

(define-public python-whisper
  (package
    (name "python-whisper")
    (version "1.1.8")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "whisper" version))
       (sha256
        (base32
         "1bk29w09zcpsv8hp0g0al7nwrxa07z0ycls3mbh83wfavk83aprl"))))
    (build-system python-build-system)
    (native-inputs (list python-six))
    (home-page "https://graphiteapp.org/")
    (synopsis "Fixed size round-robin style database for Graphite")
    (description "Whisper is one of three components within the Graphite
project.  Whisper is a fixed-size database, similar in design and purpose to
RRD (round-robin-database).  It provides fast, reliable storage of numeric
data over time.  Whisper allows for higher resolution (seconds per point) of
recent data to degrade into lower resolutions for long-term retention of
historical data.")
    (license license:asl2.0)))

(define-public python-carbon
  (package
    (name "python-carbon")
    (version "1.1.10")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "carbon" version))
       (sha256
        (base32
         "0p6yjxif5ly5wkllnaw41w2zy9y0nffgfk91v861fn6c26lmnfy1"))))
    (build-system pyproject-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; Don't install to /opt
         (add-after 'unpack 'do-not-install-to-/opt
           (lambda _ (setenv "GRAPHITE_NO_PREFIX" "1") #t)))))
    (native-inputs (list python-setuptools python-wheel))
    (propagated-inputs
     (list python-cachetools python-twisted python-txamqp python-urllib3))
    (home-page "https://graphiteapp.org/")
    (synopsis "Backend data caching and persistence daemon for Graphite")
    (description "Carbon is a backend data caching and persistence daemon for
Graphite.  Carbon is responsible for receiving metrics over the network,
caching them in memory for \"hot queries\" from the Graphite-Web application,
and persisting them to disk using the Whisper time-series library.")
    (license license:asl2.0)))

(define-public graphite-web
  (let ((commit "49c28e2015d605ad9ec93524f7076dd924a4731a")
        (revision "2"))
    (package
      (name "graphite-web")
      (version (git-version "1.1.10" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/graphite-project/graphite-web")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0bcc6jh7gyp8f54dzy4zza1z46gk3530r952pi86irf834z106sg"))))
      (build-system pyproject-build-system)
      (arguments
       `(#:tests? #f               ;XXX: Requires database, unable to run now
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'relax-requirements
             (lambda _
               (substitute* "setup.py"
                 ;; Allow newer versions of django-tagging.
                 (("django-tagging==") "django-tagging>=")
                 ;; And Django.
                 (("Django>=3\\.2,<4") "Django>=4,<5"))))
           ;; Don't install to /opt
           (add-after 'unpack 'do-not-install-to-/opt
             (lambda _ (setenv "GRAPHITE_NO_PREFIX" "1")))
           (replace 'check
             (lambda* (#:key tests? #:allow-other-keys)
               (when tests?
                 (mkdir-p "storage/log/webapp")
                 (with-directory-excursion "webapp"
                   (invoke "./manage.py" "test" "--pythonpath=." "tests"
                           "-k" (string-join
                                 (list
                                  "not test_dashboard_save_temporary_xss_key"
                                  "test_dashboard_save_temporary_xss_name")
                                 " and not ")))))))))
      (native-inputs
       (list python-carbon
             python-mock
             python-pytest
             python-rrdtool
             python-setuptools
             python-tzdata
             python-wheel
             python-whisper))
      (propagated-inputs
       (list python-cairocffi
             python-django-4.2
             python-django-tagging
             python-pyparsing
             python-pytz
             python-six
             python-urllib3))
      (home-page "https://graphiteapp.org/")
      (synopsis "Scalable realtime graphing system")
      (description "Graphite is a scalable real-time graphing system that does
two things: store numeric time-series data, and render graphs of this data on
demand.")
      (license license:asl2.0))))

(define-public python-prometheus-client
  (package
    (name "python-prometheus-client")
    (version "0.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "prometheus_client" version))
       (sha256
        (base32 "12dvlh4k6in87q47f0zqh8nrnnfs0pwrs2xynbf34yhl1g82jxi8"))))
    (build-system python-build-system)
    (arguments
     '(;; No included tests.
       #:tests? #f))
    (home-page
     "https://github.com/prometheus/client_python")
    (synopsis "Python client for the Prometheus monitoring system")
    (description
     "The @code{prometheus_client} package supports exposing metrics from
software written in Python, so that they can be scraped by a Prometheus
service.

Metrics can be exposed through a standalone web server, or through Twisted,
WSGI and the node exporter textfile collector.")
    (license license:asl2.0)))

(define-public prometheus-node-exporter
  (package
    (name "prometheus-node-exporter")
    (version "1.9.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/prometheus/node_exporter")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0xs57jlmjj1vqac6xlkygg3xb08g356nlnc852ds1ia87911jvls"))))
    (build-system go-build-system)
    (arguments
     (list
      #:install-source? #f
      #:build-flags
      #~(list (string-append
               "-ldflags="
               "-X github.com/prometheus/common/version.Version=" #$version
               " -X github.com/prometheus/common/version.Revision=0"
               " -X github.com/prometheus/common/version.Branch=master"
               " -X github.com/prometheus/common/version.BuildUser=guix"
               " -X github.com/prometheus/common/version.BuildDate=n/a"))
      #:embed-files #~(list "landing_page.css" "landing_page.html")
      #:import-path "github.com/prometheus/node_exporter"
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'pre-check
            (lambda* (#:key tests? import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (invoke "./ttar" "-C" "collector/fixtures"
                        "-x" "-f" "collector/fixtures/sys.ttar")
                (invoke "./ttar" "-C" "collector/fixtures"
                        "-x" "-f" "collector/fixtures/udev.ttar"))))
          (add-after 'check 'post-check
            (lambda* (#:key tests? import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (for-each delete-file-recursively
                          (list "collector/fixtures/sys"
                                "collector/fixtures/sys.ttar"
                                "collector/fixtures/udev"
                                "collector/fixtures/udev.ttar"))))))))
    (propagated-inputs
     (list go-github-com-alecthomas-kingpin-v2
           go-github-com-beevik-ntp
           go-github-com-coreos-go-systemd-v22
           go-github-com-dennwc-btrfs
           go-github-com-ema-qdisc
           go-github-com-go-kit-log
           go-github-com-godbus-dbus-v5
           go-github-com-hashicorp-go-envparse
           go-github-com-hodgesds-perf-utils
           go-github-com-josharian-native
           go-github-com-jsimonetti-rtnetlink
           go-github-com-mattn-go-xmlrpc
           go-github-com-mdlayher-ethtool
           go-github-com-mdlayher-netlink
           go-github-com-mdlayher-wifi
           go-github-com-jsimonetti-rtnetlink-v2
           go-github-com-opencontainers-selinux
           go-github-com-prometheus-client-golang
           go-github-com-prometheus-client-model
           go-github-com-prometheus-common
           go-github-com-prometheus-community-go-runit
           go-github-com-prometheus-exporter-toolkit
           go-github-com-prometheus-procfs
           go-github-com-safchain-ethtool
           go-golang-org-x-exp
           go-golang-org-x-sys
           go-howett-net-plist))
    (home-page "https://github.com/prometheus/node_exporter")
    (synopsis "Prometheus exporter for hardware and OS metrics")
    (description
     "Prometheus exporter for metrics exposed by *NIX kernels,
written in Go with pluggable metric collectors.")
    (license license:asl2.0)))

(define-public go-github-com-prometheus-node-exporter
  (deprecated-package "go-github-com-prometheus-node-exporter"
                      prometheus-node-exporter))

(define-public temper-exporter
  (let ((commit "a87bbab19c05609d62d9e4c7941178700c1ef84d")
        (revision "0"))
    (package
      (name "temper-exporter")
      (version (git-version "0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/yrro/temper-exporter")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0jk3ydi8s14q5kyl9j3gm2zrnwlb1jwjqpg5vqrgkbm9jrldrabc"))))
      (build-system python-build-system)
      (arguments
       '(#:tests? #f                    ; One test failure:
                                        ; test/test_exporter.py:33:
                                        ; AssertionError
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'patch-setup.py
             (lambda _
               (substitute* "setup.py"
                 (("git_ref = .*\n") "git_ref = ''\n"))
               #t))
           (add-after 'install 'install-udev-rules
             (lambda* (#:key outputs #:allow-other-keys)
               (install-file "debian/prometheus-temper-exporter.udev"
                             (string-append (assoc-ref outputs "out")
                                            "/lib/udev/rules.d"))
               #t)))))
      (inputs
       (list python-prometheus-client python-pyudev))
      (native-inputs
       (list python-pytest python-pytest-mock python-pytest-runner))
      (home-page "https://github.com/yrro/temper-exporter")
      (synopsis "Prometheus exporter for PCSensor TEMPer sensor devices")
      (description
       "This package contains a Prometheus exporter for the TEMPer sensor
devices.")
      (license license:expat))))

(define-public fswatch
  (package
    (name "fswatch")
    (version "1.17.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                      (url "https://github.com/emcrisostomo/fswatch")
                      (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "15bk2adv1ycqn3rxvpvapwrlzsxw8fvcahqiglfkibwb9ss7s8dh"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake gettext-minimal libtool))
    (arguments
     (list #:configure-flags
           #~(list "--disable-static")))
    (synopsis "File system monitor")
    (description "This package provides a file system monitor.")
    (home-page "https://github.com/emcrisostomo/fswatch")
    (license license:gpl3+)))

(define-public collectd
  (package
    (name "collectd")
    (version "5.12.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://storage.googleapis.com/collectd-tarballs/collectd-"
                    version
                    ".tar.bz2"))
              (sha256
               (base32
                "1mh97afgq6qgmpvpr84zngh58m0sl1b4wimqgvvk376188q09bjv"))
              (patches (search-patches "collectd-5.11.0-noinstallvar.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags (list "--localstatedir=/var" "--sysconfdir=/etc")
       #:phases (modify-phases %standard-phases
                  (add-before 'configure 'autoreconf
                    (lambda _
                      ;; Required because of patched sources.
                      (invoke "autoreconf" "-vfi"))))))
    (inputs
     (list rrdtool curl yajl))
    (native-inputs
     (list autoconf automake libtool pkg-config))
    (home-page "https://collectd.org/")
    (synopsis "Collect system and application performance metrics periodically")
    (description
     "collectd gathers metrics from various sources such as the operating system,
applications, log files and external devices, and stores this information or
makes it available over the network.  Those statistics can be used to monitor
systems, find performance bottlenecks (i.e., performance analysis) and predict
future system load (i.e., capacity planning).")
    ;; license:expat for the daemon in src/daemon/ and some plugins,
    ;; license:gpl2 for other plugins
    (license (list license:expat license:gpl2))))

(define-public hostscope
  (package
    (name "hostscope")
    (version "8.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://www.maier-komor.de/hostscope/hostscope-V"
                    version ".tgz"))
              (sha256
               (base32
                "0jw6yij8va0f292g4xkf9lp9sxkzfgv67ajw49g3vq42q47ld7cv"))))
    (build-system gnu-build-system)
    (inputs (list ncurses))
    (arguments '(#:tests? #f)) ;; No included tests.
    (home-page "https://www.maier-komor.de/hostscope.html")
    (properties `((release-monitoring-url . ,home-page)))
    (synopsis
     "System monitoring tool for multiple hosts")
    (description
     "HostScope displays key system metrics of Linux hosts, such as detailed
CPU load, speed and temperature, I/O rates of network interfaces, I/O rates of
disks, and user process summary information.  All metrics are multicast on the
LAN, if wanted, and clients can switch between multiple hosts on the network.
Hostscope features a bridge to Influx DB.  So Grafana can be used to visualize
the recorded data over time.")
    (license license:gpl3+)))

(define-public fatrace
  (package
    (name "fatrace")
    (version "0.18.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/martinpitt/fatrace")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0hm4zsxkbsl37677b2hq4v6pnq6mjspvcf285l9d844i2f3syij2"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         ;; tests need root to run as root,
         ;; and there is no make target for them:
         (delete 'check))
       #:make-flags
       (list (string-append "CC=" ,(cc-for-target))
             (string-append "PREFIX=" %output))))
    (synopsis "File access events monitor")
    (description "This package provides a utility to report system wide file
access events from all running processes.  Its main purpose is to find
processes which keep waking up the disk unnecessarily and thus prevent some
power saving.")
    (home-page "https://github.com/martinpitt/fatrace")
    (license license:gpl3+)))

(define-public pw
  (package
    (name "pw")
    (version "2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://www.kylheku.com/git/pw")
             (commit (string-append "pw-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1xn3qnzz48xan78cp83hfrcifrxx9lgnm14134qhyr5wvj7dk246"))))
    (build-system gnu-build-system)
    (arguments
     (list #:tests? #f                  ; There are no tests
           #:make-flags
           #~(list (string-append "CC=" #$(cc-for-target))
                   (string-append "DESTDIR=" #$output))
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'fix-makefile
                 (lambda _
                   (substitute* "Makefile"
                     (("/share/man/man1 \\\\") "/share/man/man1; \\"))))
               (delete 'configure)
               (add-before 'install 'make-install-dirs
                 (lambda _
                   (mkdir-p (string-append #$output "/bin"))
                   (mkdir-p (string-append #$output "/share/man/man1"))
                   (mkdir-p (string-append #$output "/share/man/man5")))))))
    (home-page "https://www.kylheku.com/cgit/pw/")
    (synopsis "Monitor recent lines of output from pipe")
    (description
     "@command{pw} is Pipe Watch, a utility that continuously reads lines of
text from a pipe or pipe-like source, passes them through a FIFO buffer, and
maintains a display based on the occasional sampling of the contents of the
FIFO buffer, with useful features such as triggering and filtering.

With @command{pw} you can:

@itemize
@item Interactively apply and remove filters on-the-fly, without interrupting
the source.

@item Make recurring patterns in the stream appear to ``freeze'' on the
screen, using triggers.

@item Prevent the overwhelming amount of output from a program from flooding
the terminal, while consuming all of that output so that the program isn't
blocked.  @command{pw} can pause its display updates entirely.

@item Juggle multiple shell background jobs that produce output, yet execute
indefinitely without blocking.  When @command{pw} runs as part of a shell
background job, it continues to consume input, process filters and take
snapshots, without displaying anything.  When put into the foreground again,
display resumes.
@end itemize")
    (license license:bsd-2)))

(define-public python-rrdtool
  (package
    (name "python-rrdtool")
    (version "0.1.16")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "rrdtool" version))
       (sha256
        (base32 "0l8lbarzfwbwnq9jm9gv4mmrxgjlb9hbz27sa8b703qa7s5zy2jz"))))
    (build-system pyproject-build-system)
    (arguments (list #:tests? #f)) ; No tests in pypi archive
    (inputs (list rrdtool))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/commx/python-rrdtool")
    (synopsis "Python bindings for rrdtool")
    (description "This package provides Python bindings for rrdtool.")
    (license license:lgpl2.1)))

(define-public python-statsd
  (package
    (name "python-statsd")
    (version "4.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "statsd" version))
       (sha256
        (base32 "0a6ilx68lxbynfm1slpaa87qs6mhrjm12bfjngvdma7y3fl3sxlr"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags #~(list "statsd/tests.py")))
    (native-inputs
     (list python-pytest
           python-setuptools
           python-wheel))
    (home-page "https://github.com/jsocol/pystatsd")
    (synopsis "Simple StatsD client")
    (description "StatsD is a friendly front-end to Graphite.  This package
provides a simple Python client for the StatsD daemon.")
    (license license:expat)))

(define-public batsignal
  (package
    (name "batsignal")
    (version "1.8.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/electrickite/batsignal")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0l6j873l1l0al95zl9ihxzrmy9r11pfm269gydlx8pps4gdisy6a"))))
    (build-system gnu-build-system)
    (arguments
     (list #:make-flags
           #~(list (string-append "PREFIX=" #$output)
                   (string-append "CC=" #$(cc-for-target)))
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'fix-cross-compile
                 (lambda _
                   (substitute* "Makefile"
                     (("pkg-config")
                      #$(pkg-config-for-target)))))
               (delete 'configure)
               (replace 'check
                 (lambda* (#:key tests? #:allow-other-keys)
                   (when tests?
                     (invoke "./batsignal" "-v")))))))
    (inputs (list libnotify))
    (native-inputs (list pkg-config))
    (home-page "https://github.com/electrickite/batsignal")
    (synopsis "Power monitoring tool")
    (description
     "This package provides a daemon that monitors device power levels,
notifying the user and optionally running a command when it reaches
user-configured power thresholds.  This can be used to force powering off a
laptop when the battery gets below critical levels, instead of damaging the
battery.")
    (license license:isc)))
