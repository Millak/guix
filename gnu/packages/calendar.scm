;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 David Thompson <davet@gnu.org>
;;; Copyright © 2015, 2016, 2017 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2016, 2017, 2020, 2022-2024 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Troy Sankey <sankeytms@gmail.com>
;;; Copyright © 2016, 2021 Stefan Reichoer <stefan@xsteve.at>
;;; Copyright © 2018, 2019, 2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020 Marius Bakke <mbakke@fastmail.com
;;; Copyright © 2020 Brendan Tildesley <mail@brendan.scot>
;;; Copyright © 2020 Tanguy Le Carrour <tanguy@bioneland.org>
;;; Copyright © 2020 Peng Mei Yu <pengmeiyu@riseup.net>
;;; Copyright © 2021 Wamm K. D. <jaft.r@outlook.com>
;;; Copyright © 2022 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2025 Denis 'GNUtoo' Carikli <GNUtoo@cyberdimension.org>
;;; Copyright © 2025 Sharlatan Hellseher <sharlatanus@gmail.com>
;;; Copyright © 2025 Ashish SHUKLA <ashish.is@lostca.se>
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

(define-module (gnu packages calendar)
  #:use-module (gnu packages)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system pyproject)
  #:use-module ((guix build-system python) #:select (pypi-uri))
  #:use-module (gnu packages admin)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages check)
  #:use-module (gnu packages dav)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages time)
  #:use-module (gnu packages xml)
  #:use-module (srfi srfi-26))

(define-public adl-submit
  (let ((commit "f38c7ad161fbe6ec72ecc725edbd624f5c627ea9")
        (revision "0"))
    (package
      (name "adl-submit")
      (version (git-version "1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://framagit.org/agenda-libre/adl-submit.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1zi5s8xpbm253mjdlcc1j13qqz5q7s9zavk3h0m2gfgb52xy2avp"))))
      (build-system pyproject-build-system)
      (arguments
       (list #:tests? #f)) ; no tests provided
      (native-inputs
       (list python-setuptools
             python-wheel))
      (inputs
       (list python python-pycurl))
      (home-page "https://www.agendadulibre.org")
      (synopsis "Submit events to the Agenda Du Libre")
      (description
       "adl-submit is a tool that can be used to submit events to any instance
of the Agenda Du Libre (a web calendar originally meant for free software
events).  Users can set fields through the command line or create an XML that
can be submitted with the adl-submit tool.  While the Agenda Du Libre web
application is available in multiple languages, most of the events on
https://www.agendadulibre.org are in French and the adl-submit tool is only
available in French.")
      (license license:gpl2))))

(define-public date
  (package
    (name "date")
    (version "3.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/HowardHinnant/date")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1qk7pgnk0bpinja28104qha6f7r1xwh5dy3gra7vjkqwl0jdwa35"))
       (patches
        ;; Install pkg-config files
        ;; https://github.com/HowardHinnant/date/pull/538
        (search-patches "date-ignore-zonenow.patch"
                        "date-output-pkg-config-files.patch"))))
    (inputs (list tzdata))
    (build-system cmake-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list "-DUSE_SYSTEM_TZ_DB=ON" "-DBUILD_SHARED_LIBS=ON"
              "-DBUILD_TZ_LIB=ON" "-DENABLE_DATE_TESTING=ON")
      #:modules '((guix build cmake-build-system)
                  ((guix build gnu-build-system) #:prefix gnu:)
                  (guix build utils))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-bin-bash
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "compile_fail.sh"
                (("/bin/bash")
                 (search-input-file inputs "bin/bash")))))
          (add-after 'unpack 'patch-zoneinfo-path
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "src/tz.cpp"
                (("/usr/share/zoneinfo")
                 (search-input-directory inputs "share/zoneinfo")))))
          (add-after 'unpack 'skip-failing-tests
            ;; Disable test that requires checking timezone that
            ;; isn't set in the build environment.
            (lambda _
              (for-each delete-file
                        '("test/solar_hijri_test/parse.pass.cpp"
                          "test/tz_test/zoned_time_deduction.pass.cpp"))))
          (replace 'check
            (lambda* (#:rest args)
              (apply (assoc-ref gnu:%standard-phases 'check)
                     #:test-target "testit" args))))))
    (synopsis "Date and time library for C++11 and C++14")
    (description
     "Date is a header only C++ library that extends the chrono date
algorithms library for calendar dates and durations.  It also provides the
<tz.h> library for handling time zones and leap seconds.")
    (home-page "https://howardhinnant.github.io/date/date.html")
    (license license:expat)))

(define-public libical
  (package
    (name "libical")
    (version "3.0.17")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/libical/libical/releases/download/v"
                    version "/libical-" version ".tar.gz"))
              (sha256
               (base32
                "06vqbxg4f3i03087grjncfy9pbvmlhg4v1ajhwr400l7nrnrmnmw"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:parallel-tests? #f
      #:configure-flags #~(list "-DSHARED_ONLY=true"
                                ;; required by evolution-data-server
                                "-DGOBJECT_INTROSPECTION=true"
                                "-DICAL_GLIB_VAPI=true")
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'configure 'patch-paths
            (lambda* (#:key inputs #:allow-other-keys)
              (define zoneinfo (search-input-directory inputs "share/zoneinfo"))
              ;; The timezones test fails if TZDIR is not set, for some
              ;; reason.  If only TZDIR is set, tests checking the timezone
              ;; fallback fail, so also patch the source.
              (setenv "TZDIR" zoneinfo) ;for tests
              (substitute* "src/libical/icaltz-util.c"
                (("\\\"/usr/share/zoneinfo\\\",")
                 (format #f "~s" zoneinfo))
                (("\\\"/usr/lib/zoneinfo\\\",") "")
                (("\\\"/etc/zoneinfo\\\",") "")
                (("\\\"/usr/share/lib/zoneinfo\\\"") "")))))))
    (native-inputs
     (list docbook-xml-4.3
           gobject-introspection
           gtk-doc/stable
           perl
           pkg-config
           vala))
    (inputs
     (list glib libxml2 tzdata))
    (propagated-inputs
     ;; In Requires.private of libical.pc.
     (list icu4c))
    (home-page "https://libical.github.io/libical/")
    (synopsis "iCalendar protocols and data formats implementation")
    (description
     "Libical is an implementation of the iCalendar protocols and protocol
data units.")
    ;; Can be used with either license.  See COPYING.
    (license (list license:lgpl2.1 license:mpl2.0))))

(define-public khal
  (package
    (name "khal")
    ;; TODO: The latest version requires fresh pytz module and fails with
    ;; error: E AttributeError: module 'icalendar' has no attribute 'use_pytz'
    (version "0.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "khal" version))
       (sha256
        (base32 "1gxrhfr4kv5mij75nzjgj69wcssbx4dfbky196w6b4nh3v7nm2pf"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          ;; Building the manpage requires khal to be installed.
          (add-after 'install 'manpage
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (add-installed-pythonpath inputs outputs)
              (invoke "make" "--directory=doc/" "man")
              (install-file "doc/build/man/khal.1"
                            (string-append #$output "/share/man/man1")))))))
    (native-inputs
     (list python-freezegun
           python-importlib-metadata
           python-packaging
           python-pytest
           python-setuptools-next
           python-setuptools-scm-next
           python-sphinx
           python-sphinxcontrib-newsfeed
           python-wheel))
    (inputs
     (list python-aiohttp
           python-atomicwrites
           python-click
           python-click-log
           python-configobj
           python-dateutil
           python-icalendar
           python-pytz
           python-pyxdg
           python-setproctitle
           python-tzlocal
           python-urwid
           vdirsyncer))
    (home-page "https://lostpackets.de/khal/")
    (synopsis "Console calendar program")
    (description
     "Khal is a standards based console calendar program, able to synchronize
with CalDAV servers through vdirsyncer.  It includes both a @acronym{CLI,
command-line interface} and a @acronym{TUI, textual user interface} named
'ikhal'.")
    (license license:expat)))

(define-public remind
  (package
    (name "remind")
    (version "6.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://dianne.skoll.ca/projects/remind/download/"
                           "remind-"
                           (string-join (map (cut string-pad <> 2 #\0)
                                             (string-split version #\.))
                                        ".")
                           ".tar.gz"))
       (sha256
        (base32 "01zhs8lgncpm1229s7b49fhnwwnxyyan845gb47ppkfn03vvc187"))))
    (properties
     `((output-synopsis "tcl" "graphical front-end to Remind calendar program")))
    (build-system gnu-build-system)
    (outputs (list "out"
                   "tcl"))           ; more than doubles the closure by >110 MiB
    (arguments
     '(#:test-target "test"
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'split-:tcl
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (tcl (assoc-ref outputs "tcl")))
               (for-each
                (lambda (file)
                  (let ((from (string-append out "/" file))
                        (to   (string-append tcl "/" file)))
                    (mkdir-p (dirname to))
                    (rename-file from to)))
                (list "bin/tkremind"
                      "share/man/man1/tkremind.1"
                      "share/pixmaps/tkremind.png"
                      "share/applications/tkremind.desktop"))
               (wrap-program (string-append tcl "/bin/tkremind")
                 `("PATH" ":" prefix
                   ,(map (lambda (dir)
                           (string-append dir "/bin"))
                         (append (list out tcl)
                                 (map (lambda (input)
                                        (assoc-ref inputs input))
                                      (list "tcl" "tk" "inetutils")))))
                 `("TCLLIBPATH" " " =
                   (,(getenv "TCLLIBPATH"))))))))))
    (inputs
     (list bash-minimal inetutils tcl tcllib tk))
    (native-inputs (list tzdata-for-tests))       ;required for some tests
    (home-page "https://dianne.skoll.ca/projects/remind/")
    (synopsis "Sophisticated calendar and alarm program")
    (description
     "Remind allows you to remind yourself of upcoming events and appointments.
Each reminder or alarm can consist of a message sent to standard output, or a
program to be executed.  It also features: sophisticated date calculation,
moon phases, sunrise/sunset, Hebrew calendar, alarms, PostScript output and
proper handling of holidays.")
    (license license:gpl2)))

(define-public libhdate
  (package
    (name "libhdate")
    (version "1.6.02")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://sourceforge/libhdate/libhdate/libhdate-"
                            version "/" name "-" version ".tar.bz2"))
        (sha256
         (base32
          "0qkpq412p78znw8gckwcx3l0wcss9s0dgw1pvjb1ih2pxf6hm4rw"))
        (snippet
         #~(begin (use-modules (guix build utils))
                  (substitute* "libhdate.pc.in"
                    (("prefix=/usr") "prefix=@prefix@"))))))
    (build-system gnu-build-system)
    (home-page "http://libhdate.sourceforge.net/")
    (synopsis "Library to use Hebrew dates")
    (description "LibHdate is a small library for the Hebrew calendar and times
of day, written in C, and including bindings for C++, pascal, perl, php, python,
and ruby.  It includes two illustrative command-line programs, @code{hcal} and
@code{hdate}, and some snippets and scripts written in the binding languages.")
    (license license:gpl3+)))

(define-public confclerk
  (package
    (name "confclerk")
    (version "0.6.4")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://www.toastfreeware.priv.at/tarballs/"
                            "confclerk/confclerk-" version ".tar.gz"))
        (sha256
         (base32
          "10rhg44px4nvbkd3p341cmp2ds43jn8r4rvgladda9v8zmsgr2b3"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               ;; Install directory is currently hard-coded.
               (substitute* "src/app/app.pro"
                 (("PREFIX = /usr/bin")
                  (string-append "PREFIX =" out "/bin")))
               (invoke "qmake"))))
         (add-after 'install 'install-docs
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out   (assoc-ref outputs "out"))
                    (share (string-append out "/share")))
               (install-file "data/confclerk.1"
                             (string-append share "/man/man1"))
               (install-file "data/confclerk.desktop"
                             (string-append share "/applications"))
               (install-file "data/confclerk.svg"
                             (string-append share "/icons/hicolor/scalable/apps"))
               #t))))
       #:tests? #f)) ; no tests
    (native-inputs
     (list perl)) ; pod2man
    (inputs
     (list qtbase-5))
    (home-page "https://www.toastfreeware.priv.at/confclerk")
    (synopsis "Offline conference schedule application")
    (description
     "ConfClerk is an application written in Qt, which makes conference schedules
available offline.  It displays the conference schedule from various views,
support searches on various items (speaker, speech topic, location, etc.) and
enables you to select favorite events and create your own schedule.

At the moment ConfClerk is able to import schedules in XML format created by
the PentaBarf conference management system (or frab) used by e.g. FOSDEM,
DebConf, FrOSCon, Grazer LinuxTage, and the CCC congresses.

ConfClerk is targeted at mobile devices but works on any system running Qt.")
    (license (list license:gpl2+
                   license:lgpl3)))) ; or cc-by3.0 for src/icons/*

(define-public ccal
  (package
    (name "ccal")
    (version "2.5.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://ccal.chinesebay.com/ccal/ccal-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "15nza1d1lvk3dp0wcl53wsd32yhbgyzznha092mh5kh5z74vsk1x"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute* "Makefile"
                 (("/usr/local/bin")
                  (string-append out "/bin"))
                 (("/usr/local/man")
                  (string-append out "/share/man"))))
             #t))
         (add-after 'install 'install-manuals
           (lambda _
             (invoke "make" "install-man"))))
       ;; no tests
       #:tests? #f))
    (home-page "http://ccal.chinesebay.com/ccal/ccal.htm")
    (synopsis "Command line program for Chinese calendar")
    (description "@command{ccal} is a command line program which writes a
Gregorian calendar together with Chinese calendar to standard output.  Its
usage is similar to the @command{cal} program.  In addition to console output,
it can also generate Encapsulated Postscript and HTML table outputs for use in
do-it-yourself calendars and web pages.  It supports both simplified and
traditional Chinese characters.")
    ;; Both licenses are in use in various source files.  Note that
    ;; COPYING.LESSER specifies LGPL 3.0, but all source files say
    ;; 'Lesser GPL version 2 or later'.
    (license (list license:gpl2+ license:lgpl2.1+))))

(define-public gsimplecal
  (let ((version "2.4.1"))
    (package
      (name "gsimplecal")
      (version version)
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/dmedvinsky/gsimplecal/")
                      (commit (string-append "v" version))))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0ypnq9q6v2l8jg0ah31d8502jig1rk2bz749ljj97wk0rg1rixpi"))
                (modules '((guix build utils)))))
      (build-system gnu-build-system)
      (inputs (list gtk+))
      (native-inputs (list autoconf automake pkg-config))
      (home-page "https://dmedvinsky.github.io/gsimplecal/")
      (synopsis "Lightweight calendar applet")
      (description
       "@command{gsimplecal} is a lightweight calendar application
written in C++ using GTK.  Launched once, it pops up a small calendar applet,
launched again it closes the running instance.  It can additionally be
configured to show the current time in different timezones.")
      (license license:bsd-3))))

(define-public hebcal
  (package
    (name "hebcal")
    (version "5.8.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hebcal/hebcal")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1a1b9jip1ha6bzv6xg9fx47q167yzgbxjvrp5zngv175nzl9427j"))))
    (build-system go-build-system)
    (arguments
     (list
      #:install-source? #f
      #:import-path "github.com/hebcal/hebcal"
      #:phases
      #~(modify-phases %standard-phases
          ;; taken from Makefile
          (add-after 'unpack 'set-defautl-city
            (lambda* (#:key import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (with-output-to-file "dcity.go"
                  (lambda ()
                    (format #t "package main~%var defaultCity = ~s~%"
                            "Paris")))))))))
    (inputs
     (list go-github-com-hebcal-hebcal-go
           go-github-com-pborman-getopt-v2))
    (home-page "https://github.com/hebcal/hebcal")
    (synopsis "Perpetual Jewish Calendar program")
    (description
     "Hebcal is a program for converting between Hebrew and Gregorian dates,
and generating lists of Jewish holidays for a given year.  Shabbat, holiday
candle lighting, and havdalah times are approximated using your location.

It can also show daily prayer times, the weekly Torah reading, and the daily
leaf of Talmud.  The program can help with counting of the Omer or with
calculation of Hebrew yahrzeits, birthdays, or anniversaries.")
      (license license:gpl2+)))
