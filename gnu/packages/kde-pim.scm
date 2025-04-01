;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2019, 2020 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2020 Marius Bakke <marius@gnu.org>
;;; Copyright © 2021, 2022 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2022 Brendan Tildesley <mail@brendan.scot>
;;; Copyright © 2022 Petr Hodina <phodina@protonmail.com>
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

(define-module (gnu packages kde-pim)
  #:use-module (guix build-system qt)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages aidc)
  #:use-module (gnu packages base)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cyrus-sasl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages kde)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages markup)
  #:use-module (gnu packages openldap)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages search)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml))

(define-public akonadi
  (package
    (name "akonadi")
    (version "24.12.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/release-service/"
                                  version "/src/akonadi-" version ".tar.xz"))
              (sha256
               (base32
                "183158qrd19hk7jphhgd3wcm3vzyd117nqs3kzsqq7mabqzvj0z9"))
              (patches (search-patches "akonadi-paths.patch"
                                       "akonadi-timestamps.patch"
                                       "akonadi-not-relocatable.patch"))))
    (build-system qt-build-system)
    (native-inputs
     (list dbus
           extra-cmake-modules
           qttools
           shared-mime-info
           pkg-config))
    (inputs
     (list boost
           libaccounts-qt6
           kconfig
           kconfigwidgets
           kcoreaddons
           kcrash
           ki18n
           kiconthemes
           kio
           kitemmodels
           kitemviews
           kwidgetsaddons
           kwindowsystem
           kxmlgui
           libxml2
           libxslt
           ;; Do NOT add mysql or postgresql to the inputs. Otherwise the binaries
           ;; and wrapped files will refer to them, even if the user choices none
           ;; of these.  Executables are searched on $PATH then.
           signond))
    (propagated-inputs (list sqlite kaccounts-integration))
    (arguments
     (list #:qtbase qtbase
           #:tests? #f
           #:configure-flags #~'("-DDATABASE_BACKEND=SQLITE") ;lightweight
           #:modules `((ice-9 textual-ports)
                       ,@%qt-build-system-modules)
           #:phases
           #~(modify-phases (@ (guix build qt-build-system) %standard-phases)
               (replace 'check
                 (lambda* (#:key tests? #:allow-other-keys)
                   (when tests?
                     (setenv "PATH"
                             (string-append (getcwd) "/bin" ":"
                                            (getenv "PATH")))
                     (invoke "dbus-launch" "ctest" "-E"
                             "(AkonadiServer-dbconfigtest|mimetypecheckertest|entitytreemodeltest|akonadi-sqlite-testenvironmenttest|akonadi-sqlite-autoincrementtest|akonadi-sqlite-attributefactorytest|akonadi-sqlite-collectionpathresolvertest|akonadi-sqlite-collectionattributetest|akonadi-sqlite-itemfetchtest|akonadi-sqlite-itemappendtest|akonadi-sqlite-itemstoretest|akonadi-sqlite-itemdeletetest|akonadi-sqlite-entitycachetest|akonadi-sqlite-monitortest|akonadi-sqlite-changerecordertest|akonadi-sqlite-resourcetest|akonadi-sqlite-subscriptiontest|akonadi-sqlite-transactiontest|akonadi-sqlite-itemcopytest|akonadi-sqlite-itemmovetest|akonadi-sqlite-invalidatecachejobtest|akonadi-sqlite-collectioncreatetest|akonadi-sqlite-collectioncopytest|akonadi-sqlite-collectionmovetest|akonadi-sqlite-collectionsynctest|akonadi-sqlite-itemsynctest)"))))
               (add-before 'configure 'add-definitions
                 (lambda* (#:key outputs inputs #:allow-other-keys)
                   (with-output-to-file "CMakeLists.txt.new"
                     (lambda _
                       (display (string-append
                                 "add_compile_definitions(\n"
                                 "NIX_OUT=\""
                                 #$output "\"\n" ")\n\n"))
                       (display (call-with-input-file "CMakeLists.txt"
                                  get-string-all))))
                   (rename-file "CMakeLists.txt.new" "CMakeLists.txt"))))))
    (home-page "https://kontact.kde.org/components/akonadi/")
    (synopsis "Extensible cross-desktop storage service for PIM")
    (description
     "Akonadi is an extensible cross-desktop Personal Information
Management (PIM) storage service.  It provides a common framework for
applications to store and access mail, calendars, addressbooks, and other PIM
data.

This package contains the Akonadi PIM storage server and associated
programs.")
    (license license:fdl1.2+)))

(define-public akonadi-calendar
  (package
    (name "akonadi-calendar")
    (version "24.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/akonadi-calendar-" version ".tar.xz"))
       (sha256
        (base32 "1avdawfl46x1vf7sgwllgmywk1m28sxk3n002m68df2c7k8cb2hr"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules))
    (inputs
     (list akonadi-contacts
           akonadi-mime
           boost
           grantleetheme
           kcalutils
           kcodecs
           kcontacts
           kcrash
           kdbusaddons
           kiconthemes
           kio
           kitemmodels
           kmailtransport
           kmime
           kmessagelib
           knotifications
           kpimtextedit
           ksmtp
           ktextwidgets
           kxmlgui
           kwallet))
    (propagated-inputs (list akonadi
                             kcalendarcore
                             ki18n
                             kwidgetsaddons
                             kidentitymanagement))
    (arguments
     (list #:qtbase qtbase
           #:tests? #f))  ;; TODO: 1/1 test fails
    (home-page "https://api.kde.org/kdepim/akonadi/html/index.html")
    (synopsis "Library providing calendar helpers for Akonadi items")
    (description "This library manages calendar specific actions for
collection and item views.")
    (license license:lgpl2.0+)))

(define-public akonadi-contacts
  (package
    (name "akonadi-contacts")
    (version "24.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/akonadi-contacts-" version ".tar.xz"))
       (sha256
        (base32 "1s1yyq9wrrm6670h8xjj5b020nirszckdpq55v4xqpgvcm4547sk"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules))
    (inputs
     (list akonadi
           boost
           grantleetheme
           kauth
           kcmutils
           kcodecs
           kcompletion
           kconfigwidgets
           kcontacts
           kcoreaddons
           kdbusaddons
           kguiaddons
           ki18n
           kiconthemes
           kitemmodels
           kitemviews
           kjobwidgets
           kmime
           kservice
           ktextwidgets
           ktexttemplate
           ktextaddons
           ktexteditor
           kwidgetsaddons
           kxmlgui
           prison
           kio
           solid
           sonnet))
    (arguments
     (list #:qtbase qtbase))
    (home-page "https://api.kde.org/kdepim/akonadi/html/index.html")
    (synopsis "Akonadi contacts access library")
    (description "Akonadi Contacts is a library that effectively bridges the
type-agnostic API of the Akonadi client libraries and the domain-specific
KContacts library.  It provides jobs, models and other helpers to make working
with contacts and addressbooks through Akonadi easier.

The library provides a complex dialog for editing contacts and several models
to list and filter contacts.")
    (license ;; GPL for programs, LGPL for libraries
     (list license:gpl2+ license:lgpl2.0+))))

(define-public akonadi-mime
  (package
    (name "akonadi-mime")
    (version "24.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/akonadi-mime-" version ".tar.xz"))
       (sha256
        (base32 "0zvdszsa6axif4849bs2ic7ywgjn4s1lszqg2idx2amd9r5lfhd5"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules libxslt ;; xslt for generating interface descriptions
           shared-mime-info))
    (inputs
     (list akonadi
           boost
           kcodecs
           kconfig
           kconfigwidgets
           kdbusaddons
           ki18n
           kio
           kitemmodels
           kmime
           kwidgetsaddons
           kxmlgui))
    (home-page "https://api.kde.org/kdepim/akonadi/html/index.html")
    (arguments
     (list
      #:qtbase qtbase
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'copy-desktop-file-early
            (lambda _
              (let ((plugins-dir "/tmp/.local/share/akonadi/plugins/serializer"))
                (mkdir-p plugins-dir)
                (copy-file "serializers/akonadi_serializer_mail.desktop"
                           (string-append plugins-dir "/akonadi_serializer_mail.desktop")))))
          (add-before 'check 'check-setup
            (lambda _
              (setenv "HOME" "/tmp"))))))
    (synopsis "Akonadi MIME handling library")
    (description "Akonadi Mime is a library that effectively bridges the
type-agnostic API of the Akonadi client libraries and the domain-specific
KMime library.  It provides jobs, models and other helpers to make working
with emails through Akonadi easier.")
    (license ;; GPL for programs, LGPL for libraries
     (list license:gpl2+ license:lgpl2.0+))))

(define-public akonadi-notes
  (package
    (name "akonadi-notes")
    (version "24.08.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/akonadi-notes-" version ".tar.xz"))
       (sha256
        (base32 "1ppgdwjg9w4igwqf1b0b1xzbc1c4j3z67ha9381ncklpd227nxvq"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules))
    (inputs
     (list akonadi kcodecs ki18n kmime))
    (arguments (list #:qtbase qtbase))
    (home-page "https://api.kde.org/kdepim/akonadi/html/index.html")
    (synopsis "Akonadi notes access library")
    (description "Akonadi Notes is a library that effectively bridges the
type-agnostic API of the Akonadi client libraries and the domain-specific
KMime library.  It provides a helper class for note attachments and for
wrapping notes into KMime::Message objects.")
    (license ;; GPL for programs, LGPL for libraries
     (list license:gpl2+ license:lgpl2.0+))))

(define-public akonadi-search
  (package
    (name "akonadi-search")
    (version "24.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/akonadi-search-" version ".tar.xz"))
       (sha256
        (base32 "03kjm3s3zy2lb8qyazpsy5vys17dkbz2jj8hqj40b36w3f6fv55y"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules
           ;; For tests.
           dbus))
    (inputs
     (list akonadi
           akonadi-mime
           boost
           kcalendarcore
           kcmutils
           kcontacts
           kcrash
           kdbusaddons
           ktextaddons
           ki18n
           kio
           kitemmodels
           kmime
           kxmlgui
           krunner
           kwindowsystem
           xapian))
    (arguments
     (list
      #:qtbase qtbase
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'disable-failing-test
            (lambda _
              ;; FIXME: This test fails because it fails to establish
              ;; a socket connection, seemingly due to failure during
              ;; DBus communication.  See also 'korganizer'.
              (substitute* "agent/autotests/CMakeLists.txt"
                ((".*schedulertest\\.cpp.*")
                 ""))))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "dbus-launch" "ctest" "-E"
                        "akonadi-sqlite-collectionindexingjobtest")))))))
    (home-page "https://api.kde.org/kdepim/akonadi/html/index.html")
    (synopsis "Akonadi search library")
    (description "This package provides a library used to search in the
Akonadi PIM data server.  It uses Xapian for indexing and querying.")
    (license ;; GPL for programs, LGPL for libraries
     (list license:gpl2+ license:lgpl2.0+))))

(define-public akonadi-import-wizard
  (package
    (name "akonadi-import-wizard")
    (version "24.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/akonadi-import-wizard-" version
                           ".tar.xz"))
       (sha256
        (base32 "1gv2jxy1xc6b70jax320rvk68qmgz1wpnrbx6wlxg1dmrafhby4d"))))
    (build-system qt-build-system)
    (arguments
     (list #:qtbase qtbase
           #:tests? #f))
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list akonadi
           akonadi-contacts
           akonadi-mime
           grantlee
           grantleetheme
           karchive
           kauth
           kconfig
           kconfigwidgets
           kcontacts
           kcrash
           kdbusaddons
           kiconthemes
           kidentitymanagement
           kimap
           kio
           kitemmodels
           kmailcommon
           kmailimporter
           kmailtransport
           kmessagelib
           kmime
           kpimcommon
           kpimtextedit
           ktextaddons
           ktextwidgets
           kwallet
           kxmlgui
           libkdepim
           libkleo
           qtkeychain-qt6
           qtwebengine))
    (home-page "https://invent.kde.org/pim/akonadi-import-wizard")
    (synopsis "Assistant to import external PIM data into Akonadi")
    (description "Akonadi Data import Wizard is an assistant to import external
PIM data into Akonadi for use in KDE PIM applications.")
    (license
     (list license:gpl2+ license:lgpl2.0+))))

(define-public itinerary
  (package
    (name "itinerary")
    (version "24.12.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/release-service/"
                                  version "/src/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "0d2nckmi4j36k9nhp62zdjyb2fckzq3205fy221nxn8cnpi121ni"))))
    (build-system qt-build-system)

    (arguments
     (list #:qtbase qtbase
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'stop-require-qmlmodule
                 (lambda _
                   ;; HACK: ecm_find_qmlmodule cann't find qmlmodule on other
                   ;; prefix, so we remove it require.
                   (substitute* "CMakeLists.txt"
                     (("\\$\\{GEAR_MIN_VERSION\\} REQUIRED")
                      "${GEAR_MIN_VERSION}")))))
           #:tests? #f)) ;Fails 20/27
    (native-inputs (list extra-cmake-modules python-minimal))
    (inputs (list karchive
                  kdbusaddons
                  ki18n
                  kio
                  kirigami
                  kirigami-addons
                  kitinerary
                  kitemmodels
                  kcoreaddons
                  kcontacts
                  kholidays
                  kmime
                  knotifications
                  kpublictransport
                  kcalendarcore
                  khealthcertificate
                  kosmindoormap
                  kopeninghours
                  kpkpass
                  kunitconversion
                  kwindowsystem
                  prison
                  qtdeclarative
                  qtkeychain-qt6
                  qtpositioning
                  qtlocation
                  qtmultimedia
                  qqc2-desktop-style
                  shared-mime-info
                  solid
                  sonnet
                  zlib))
    (home-page "https://invent.kde.org/pim/itinerary")
    (synopsis "Boarding pass and itinerary management")
    (description
     "This package provides a tool for managing itinerary and boarding pass
information.")
    (license ;GPL for programs, LGPL for libraries
     (list license:gpl2+ license:lgpl2.0+))))

(define-public kincidenceeditor
  (package
    (name "kincidenceeditor")
    (version "24.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/incidenceeditor-" version ".tar.xz"))
       (sha256
        (base32 "13knf50ds2shjps93v5z8dvh19cx820gfh2dk5wlpz3sdcmh5hw9"))))
    (properties `((upstream-name . "incidenceeditor")))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules dbus))
    (inputs
     (list akonadi
           akonadi-calendar
           akonadi-contacts
           akonadi-mime
           boost
           grantleetheme
           kcalendarcore
           kcalendarsupport
           kcalutils
           kcodecs
           kcontacts
           kconfigwidgets
           kdbusaddons
           kdiagram
           keventviews
           kguiaddons
           ki18n
           kiconthemes
           kidentitymanagement
           kimap
           kio
           kitemmodels
           kldap
           kmailtransport
           kmime
           kpimcommon
           kpimtextedit
           ktextaddons
           ktextwidgets
           kxmlgui
           kwallet
           libkdepim))
    (arguments
     (list
      #:qtbase qtbase
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "dbus-launch" "ctest" ;; FIXME: tests fails.
                        "-E"
		        "(akonadi-sqlite-incidencedatetimetest|ktimezonecomboboxtest|testindividualmaildialog)")))))))
    (home-page "https://invent.kde.org/pim/incidenceeditor")
    (synopsis "KDE PIM library for editing incidences")
    (description "This library provides an incidence editor for KDE PIM.")
    (license ;; GPL for programs, LGPL for libraries
     (list license:gpl2+ license:lgpl2.0+))))

(define-public kaddressbook
  (package
    (name "kaddressbook")
    (version "24.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kaddressbook-" version ".tar.xz"))
       (sha256
        (base32 "0dy5ir5xb6mamc7xqqm6hi3mdgcs91d98rf29xmmh6yld1gg7dc2"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list akonadi
           akonadi-contacts
           akonadi-mime
           akonadi-search
           boost
           gpgme
           grantleetheme
           kcalendarcore
           kcmutils
           kcompletion
           kcontacts
           kcrash
           kdbusaddons
           ki18n
           kiconthemes
           kimap
           kio
           kitemmodels
           kldap
           kmime
           kontactinterface
           kparts
           kpimcommon
           kpimtextedit
           ktextaddons
           ktextwidgets
           kxmlgui
           libkdepim
           libkleo
           breeze-icons ; default icon set, required for tests
           prison
           qgpgme-qt6))
    (arguments
     (list #:qtbase qtbase
           #:tests? #f))
    (home-page "https://kontact.kde.org/components/kaddressbook/")
    (synopsis "Address Book application to manage your contacts")
    (description "KAddressBook stores all the personal details of your family,
friends and other contacts.  It supports large variety of services, including
NextCloud, Kolab, Google Contacts, Microsoft Exchange (EWS) or any standard
CalDAV server.")
    (license (list license:gpl2+ license:lgpl2.0+ license:fdl1.2+))))

(define-public kaccounts-integration
  (package
    (name "kaccounts-integration")
    (version "24.12.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/release-service/"
                                  version "/src/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "1y19kb87wqf8w5b5yvnkykqkrri7djrb8gd39c734zq7ks5wjd67"))))
    (build-system qt-build-system)
    (native-inputs (list extra-cmake-modules))
    (inputs (list kcmutils
                  ki18n
                  kcoreaddons
                  kdbusaddons
                  kdeclarative
                  kwallet
                  kio
                  libaccounts-qt6
                  qcoro-qt6
                  signond-qt6))
    (arguments
     (list #:qtbase qtbase
           #:tests? #f))
    (home-page "https://invent.kde.org/network/kaccounts-integration")
    (synopsis "Online account management system")
    (description "The Kaccounts Integration library provides online account
management system and its Plasma integration components.")
    (license license:lgpl2.0+)))

(define-public kaccounts-providers
  (package
    (name "kaccounts-providers")
    (version "24.12.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/release-service/"
                                  version "/src/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "1jaigpnclcmwm0az169mc7l2kic652prxcbs1k71c8b04frnw44c"))))
    (build-system qt-build-system)
    (native-inputs (list extra-cmake-modules intltool))
    (inputs (list kaccounts-integration
                  kcoreaddons
                  kdeclarative
                  kpackage
                  ki18n
                  kio
                  libaccounts-qt6
                  qtwebengine
                  qcoro-qt6
                  signond-qt6))
    (arguments
     (list #:qtbase qtbase
           #:tests? #f))
    (home-page "https://invent.kde.org/network/kaccounts-providers")
    (synopsis "Online account providers for the KAccounts system")
    (description "This package provides online account providers for the
KAccounts system.")
    (license license:lgpl2.0+)))

(define-public kcalendarsupport
  (package
    (name "kcalendarsupport")
    (version "24.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/calendarsupport-" version ".tar.xz"))
       (sha256
        (base32 "0xpxyqgnranf89ybkd8aqfrxk1szg0ryr0xdwgrjkgqfn0p2az7z"))))
    (properties `((upstream-name . "calendarsupport")))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules qttools))
    (inputs
     (list akonadi
           akonadi-mime
           akonadi-notes
           boost
           kcalendarcore
           kcalutils
           kcompletion
           kdbusaddons
           kguiaddons
           kholidays
           ki18n
           kiconthemes
           kio
           kitemmodels
           kpimcommon
           kpimtextedit
           ktextwidgets
           kxmlgui))
    (propagated-inputs (list akonadi-calendar kidentitymanagement kmime))
    (arguments (list #:qtbase qtbase))
    (home-page "https://api.kde.org/kdepim/calendarsupport/html/index.html")
    (synopsis "Calendar Support library for KDE PIM")
    (description "The Calendar Support library provides helper utilities for
calendaring applications.")
    (license ;; GPL for programs, LGPL for libraries
     (list license:gpl2+ license:lgpl2.0+))))

(define-public kcalutils
  (package
    (name "kcalutils")
    (version "24.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kcalutils-" version ".tar.xz"))
       (sha256
        (base32 "0iq5df6swl3k3vw7ak502z6v98732rig7k49x4xc63rya8xfw8gk"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules libxml2)) ;; xmllint required for tests
    (inputs
     (list breeze-icons ; default icon set, required for tests
           kcalendarcore
           kcodecs
           kconfig
           kconfigwidgets
           kcoreaddons
           ki18n
           kiconthemes
           kidentitymanagement
           kpimtextedit
           ktextwidgets
           ktexttemplate
           kwidgetsaddons))
    (arguments
     (list #:qtbase qtbase
           #:tests? #f)) ;; TODO: seem to pull in some wrong theme
    (home-page "https://api.kde.org/kdepim/kcalutils/html/index.html")
    (synopsis "Library with utility functions for the handling of calendar
data")
    (description "This library provides a utility and user interface
functions for accessing calendar data using the kcalcore API.")
    (license  license:lgpl2.0+)))

(define-public kdepim-addons
  (package
    (name "kdepim-addons")
    (version "24.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kdepim-addons-" version ".tar.xz"))
       (sha256
        (base32 "1wm1bp41q1asd6wi5q305gjvgfjaa50l401k2nnn7gvdrz3y4fa6"))))
    (build-system qt-build-system)
    (arguments
     (list #:qtbase qtbase
           #:phases
           #~(modify-phases %standard-phases
               ;; TODO: Out of 156 tests, 10 fail and 2 get stuck.
               ;; kdepim-addons-todoedittest and kdepim-addons-eventedittest
               ;; get stuck. Do they require user input?
               ;; eventdatavisitortest and pimeventsplugintest fail only in the
               ;; check phase of guix build, but testing the same normally
               ;; outside the guix build passes these two tests.
               ;; messageviewer-dkimauthenticationverifiedserverdialogtest
               ;; fails due to SEGFAULT.
               (replace 'check
                 (lambda* (#:key tests? #:allow-other-keys)
                   (setenv "HOME" "/tmp")
                   (when tests?
                     (invoke "dbus-launch" "ctest" "-E" "\
(kdepim-addons-todoedittest|kdepim-addons-eventedittest\
|enterpriseheaderstyleplugintest|fancyheaderstyleplugintest\
|grantleeheaderstyleplugintest|messageviewerplugins-rendertest\
|akonadi-sqlite-rendertest-akonadi|akonadi-sqlite-mailsenderjobtest\
|akonadi-sqlite-gravatarupdatewidgettest|eventdatavisitortest\
|pimeventsplugintest\
|messageviewer-dkimauthenticationverifiedserverdialogtest)")))))))
    (native-inputs
     (list dbus extra-cmake-modules libxml2)) ;libxml2 for xmllint
    (inputs
     (list akonadi
           akonadi-calendar
           akonadi-contacts
           akonadi-import-wizard
           akonadi-mime
           akonadi-notes
           discount
           grantlee
           grantleetheme
           kaddressbook
           karchive
           kcalendarcore
           kcalendarsupport
           kcalutils
           kconfig
           kcontacts
           kdbusaddons
           kdeclarative
           keventviews
           kguiaddons
           kholidays
           ki18n
           kiconthemes
           kidentitymanagement
           kimap
           kincidenceeditor
           kio
           kitemmodels
           kitinerary
           kldap
           kmailcommon
           kmailimporter
           kmailtransport
           kmessagelib
           kmime
           kparts
           kpimcommon
           kpimtextedit
           kpkpass
           ksyntaxhighlighting
           ktextaddons
           ktnef
           kwallet
           kxmlgui
           libgravatar
           libkdepim
           libkleo
           libksieve
           prison
           qtwebengine))
    (home-page "https://invent.kde.org/pim/kdepim-addons")
    (synopsis "Add-ons for KDE PIM applications")
    (description "This package contains add-ons for KDE PIM applications such
as KMail, KAddressBook etc.")
    (license
     (list license:gpl2+ license:lgpl2.0+))))

(define-public kdepim-runtime
  (package
    (name "kdepim-runtime")
    (version "24.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kdepim-runtime-" version ".tar.xz"))
       (sha256
        (base32 "0n7wdnx7zl9pykg75naai0g1b0m3ck20g61k0rqj9gx6dmfy468i"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules dbus kdoctools libxslt shared-mime-info))
    (inputs
     (list akonadi
           akonadi-calendar
           akonadi-contacts
           akonadi-mime
           akonadi-notes
           boost
           cyrus-sasl
           grantleetheme
           kcalendarcore
           kcalutils
           kcmutils
           kcodecs
           kconfig
           kconfigwidgets
           kcontacts
           kdav
           kholidays
           kidentitymanagement
           kimap
           kio
           kitemmodels
           kmailtransport
           kldap
           kmbox
           kmime
           kxmlgui
           knotifications
           knotifyconfig
           kpimcommon
           kpimtextedit
           ktextwidgets
           kwallet
           kwindowsystem
           libkdepim
           libkgapi
           ;; TODO: libkolab
           qca-qt6
           qtdeclarative
           qtkeychain-qt6
           qtnetworkauth
           qtspeech
           qtwebchannel
           qtwebengine))
    (arguments
     ;; TODO: 5/45 tests fail for quite different reasons, even with
     ;; "offscreen" and dbus
     (list #:qtbase qtbase
           #:phases #~(modify-phases %standard-phases
                        (add-after 'set-paths 'extend-CPLUS_INCLUDE_PATH
                          (lambda* (#:key inputs #:allow-other-keys)
                            ;; FIXME: <Akonadi/KMime/SpecialMailCollections> is not
                            ;; found during one of the compilation steps without
                            ;; this hack.
                            (setenv "CPLUS_INCLUDE_PATH"
                                    (string-append
                                     (assoc-ref inputs "akonadi-mime") "/include/KF6:"
                                     (or (getenv "CPLUS_INCLUDE_PATH") "")))))
                        (replace 'check
                          (lambda* (#:key tests? #:allow-other-keys)
                            (when tests?
                              ;; FIXME: Atleast some appear to require network.
                              (invoke "dbus-launch" "ctest" "-E" "\
(akonadi-sqlite-icalcategoriestotagsmigrationtest|akonadi-sqlite-synctest\
|akonadi-sqlite-pop3test|storecompacttest|akonadi-sqlite-ewstest\
|ewsmoveitemrequest_ut|ewsdeleteitemrequest_ut\
|ewsgetitemrequest_ut|ewsunsubscriberequest_ut|ewssettings_ut\
|templatemethodstest|akonadi-sqlite-serverbusytest|ewsattachment_ut\
|testmovecollectiontask)")))))))
    (home-page "https://invent.kde.org/pim/kdepim-runtime")
    (synopsis "Runtime components for Akonadi KDE")
    (description "This package contains Akonadi agents written using KDE
Development Platform libraries.  Any package that uses Akonadi should probably
pull this in as a dependency.  The kres-bridges is also parts of this
package.")
    (license ;; Files vary a lot regarding the license. GPL2+ and LGPL2.1+
     ;; have been used in those I checked. But the archive also includes
     ;; license texts for GPL3 and AGPL3.
     (list license:gpl2+ license:lgpl2.0+))))

(define-public keventviews
  (package
    (name "keventviews")
    (version "24.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/eventviews-" version ".tar.xz"))
       (sha256
        (base32 "11f2k7mszik3f05rn6gv7a6ihhdl8vb9g97ynqdbc8qvik2x8frq"))))
    (properties `((upstream-name . "eventviews")))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules qttools))
    (inputs
     (list akonadi-contacts
           boost
           kcalendarsupport
           kcalutils
           kcodecs
           kcompletion
           kconfigwidgets
           kcontacts
           kdbusaddons
           kdiagram
           kguiaddons
           kholidays
           ki18n
           kiconthemes
           kidentitymanagement
           kio
           kitemmodels
           kmime
           kpimtextedit
           kservice
           ktextwidgets
           kxmlgui
           libkdepim))
    (propagated-inputs (list akonadi
                             akonadi-calendar
                             kcalendarcore
                             kcalendarsupport))
    (arguments (list #:qtbase qtbase))
    (home-page "https://invent.kde.org/pim/eventviews")
    (synopsis "KDE PIM library for creating events")
    (description "This library provides an event creator for KDE PIM.")
    (license ;; GPL for programs, LGPL for libraries
     (list license:gpl2+ license:lgpl2.0+))))

(define-public kgpg
  (package
    (name "kgpg")
    (version "24.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kgpg-" version ".tar.xz"))
       (sha256
        (base32 "0rw6qm92jm0afi9p12dgqapzsmwa4jk83rwzxnnsvqfircsl7njb"))))
    (build-system qt-build-system)
    (arguments
     (list #:qtbase qtbase
           #:phases
           #~(modify-phases %standard-phases
               (replace 'check
                 (lambda* (#:key tests? #:allow-other-keys)
                   (when tests?
                     (setenv "HOME" (getcwd))
                     (invoke "ctest")))))
           ;; XXX: Tests could fail randomly with:
           ;;   gpg: can't connect to the agent: IPC connect call failed
           ;;   gpg process did not finish. Cannot generate a new key pair.
           #:tests? #f))
    (native-inputs
     (list extra-cmake-modules
           gnupg ;; TODO: Remove after gpgme uses fixed path
           kdoctools
           pkg-config))
    (inputs
     (list akonadi
           akonadi-contacts
           boost
           breeze-icons ;; default icon set
           gpgme
           grantleetheme
           karchive
           kcodecs
           kcontacts
           kcoreaddons
           kcrash
           kdbusaddons
           ki18n
           kiconthemes
           kio
           kitemmodels
           kjobwidgets
           knotifications
           kservice
           kstatusnotifieritem
           ktextwidgets
           kwidgetsaddons
           kwindowsystem
           kxmlgui))
    (home-page "https://apps.kde.org/kgpg/")
    (synopsis "Graphical front end for GNU Privacy Guard")
    (description "Kgpg manages cryptographic keys for the GNU Privacy Guard,
and can encrypt, decrypt, sign, and verify files.  It features a simple editor
for applying cryptography to short pieces of text, and can also quickly apply
cryptography to the contents of the clipboard.")
    (license license:gpl2+)))

(define-public khealthcertificate
  (package
    (name "khealthcertificate")
    (version "24.12.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/release-service/" version
                                  "/src/khealthcertificate-" version ".tar.xz"))
              (sha256
               (base32
                "1vxlq0gfpg9q58963zm8fb5vsp16aicfqp0xgzdj5ad2pk1zgrkm"))))
    (build-system qt-build-system)
    (arguments
     (list
      #:qtbase qtbase
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key inputs tests? #:allow-other-keys)
              (when tests?
                (setenv "TZDIR"
                        (search-input-directory inputs "share/zoneinfo"))
                (invoke "ctest" "-E"
                        "(icaovdsparsertest|eudgcparsertest)")))))))
    (native-inputs (list extra-cmake-modules pkg-config tzdata-for-tests))
    (inputs (list karchive
                  kcodecs
                  ki18n
                  openssl
                  qtdeclarative
                  zlib))
    (home-page "https://api.kde.org/khealthcertificate/html/index.html")
    (synopsis "Digital vaccination and recovery certificate library")
    (description
     "This package provides a library for arsing of digital vaccination,
test and recovery certificates.")
    (license license:lgpl2.0)))

(define-public kidentitymanagement
  (package
    (name "kidentitymanagement")
    (version "24.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kidentitymanagement-" version ".tar.xz"))
       (sha256
        (base32 "1qyyps5xy8cij4zsk6jhgvjj4xcs10vd7i2si6a4l72vg8rwd3if"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules))
    (inputs
     (list kcodecs
           kcompletion
           kconfig
           kcoreaddons
           kiconthemes
           kio
           kpimtextedit
           ktextwidgets
           ktextaddons
           kxmlgui
           kirigami-addons))
    (arguments
     (list #:qtbase qtbase
           #:phases
           #~(modify-phases %standard-phases
               (add-before 'check 'set-home
                 (lambda _
                   (setenv "HOME" "/tmp/dummy-home")))))) ;; FIXME: what is this?
    (home-page "https://kontact.kde.org/")
    (synopsis "Library for shared identities between mail applications")
    (description "This library provides an API for managing user identities.")
    (license ;; GPL for programs, LGPL for libraries, FDL for documentation
     (list license:gpl2+ license:lgpl2.0+ license:fdl1.2+))))

(define-public kimap
  (package
    (name "kimap")
    (version "24.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kimap-" version ".tar.xz"))
       (sha256
        (base32 "0rqbafm4af70lbl05xmnmr0dcs2fxn7xbck2mpa0yrkky5657gdh"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules))
    (inputs
     (list cyrus-sasl
           kcoreaddons
           kcodecs
           ki18n
           kio
           kmime))
    (arguments (list #:qtbase qtbase))
    (home-page "https://api.kde.org/kdepim/kimap/html/index.html")
    (synopsis "Library for handling IMAP")
    (description "This library provides a job-based API for interacting with
an IMAP4rev1 server.  It manages connections, encryption and parameter quoting
and encoding, but otherwise provides quite a low-level interface to the
protocol.  This library does not implement an IMAP client; it merely makes it
easier to do so.")
    (license ;; GPL for programs, LGPL for libraries
     (list license:gpl2+ license:lgpl2.0+))))

(define-public kitinerary
  (package
    (name "kitinerary")
    (version "24.12.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/release-service/"
                                  version "/src/kitinerary-" version ".tar.xz"))
              (sha256
               (base32
                "1fxrhhffkp5mw5i8pdaxj92ggs5zx7z8vgdarjqr1ml48sl1cv7a"))))
    (build-system qt-build-system)
    (arguments
     (list #:qtbase qtbase
           #:phases #~(modify-phases %standard-phases
                        (replace 'check
                          (lambda* (#:key inputs tests? #:allow-other-keys)
                            (when tests?
                              (setenv "TZDIR"
                                      (search-input-directory inputs "share/zoneinfo"))
                              (invoke "dbus-launch" "ctest" "-E"
                                      "(jsonlddocumenttest|mergeutiltest|locationutiltest|knowledgedbtest|airportdbtest|extractorscriptenginetest|pkpassextractortest|postprocessortest|calendarhandlertest|extractortest)")))))))
    (native-inputs (list dbus extra-cmake-modules tzdata-for-tests))
    (inputs (list kpkpass
                  kcalendarcore
                  karchive
                  ki18n
                  kcoreaddons
                  kcontacts
                  kmime
                  knotifications
                  shared-mime-info
                  openssl
                  poppler
                  qtdeclarative
                  qtkeychain-qt6
                  libxml2
                  zlib
                  zxing-cpp))
    (home-page "https://apps.kde.org/itinerary/")
    (synopsis
     "Data Model and Extraction System for Travel Reservation information")
    (description "This package provides a library containing itinerary data
model and itinerary extraction code.")
    (license license:lgpl2.0)))

(define-public kldap
  (package
    (name "kldap")
    (version "24.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kldap-" version ".tar.xz"))
       (sha256
        (base32 "13yvxiff898r6lx2fv8722w5xm51lapwaqjk5max1znabhmnahvy"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list ki18n kio kwidgetsaddons qtkeychain-qt6))
    (propagated-inputs
     (list cyrus-sasl openldap))
    (arguments (list #:qtbase qtbase))
    (home-page "https://api.kde.org/kdepim/kldap/html/index.html")
    (synopsis "Library for accessing LDAP")
    (description "This is a library for accessing LDAP with a convenient Qt
style C++ API.  LDAP (Lightweight Directory Access Protocol) is an application
protocol for querying and modifying directory services running over TCP/IP.")
    (license license:lgpl2.0+)))

(define-public kleopatra
  (package
    (name "kleopatra")
    (version "24.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kleopatra-" version ".tar.xz"))
       (sha256
        (base32 "02fp20aj45sr0r2hxbc28jnmllpqrpay3sn5gva1makf6r5vyy5q"))))
    (build-system qt-build-system)
    (native-inputs
     (list dbus extra-cmake-modules gnupg ;; TODO: Remove after gpgme uses fixed path
           kdoctools))
    (inputs
     (list boost
           gpgme
           kcmutils
           kcodecs
           kconfig
           kconfigwidgets
           kcoreaddons
           kcrash
           kdbusaddons
           kguiaddons
           ki18n
           kio
           kiconthemes
           kitemmodels
           kmime
           knotifications
           ktextwidgets
           kstatusnotifieritem
           kwidgetsaddons
           kwindowsystem
           kxmlgui
           libassuan
           libkleo
           mimetreeparser
           breeze-icons ;; default icon set
           qgpgme-qt6))
    (arguments
     (list #:qtbase qtbase
           #:phases
           #~(modify-phases %standard-phases
               (replace 'check
                 (lambda* (#:key tests? #:allow-other-keys)
                   (when tests?
                     (invoke "dbus-launch" "ctest")))))))
    (home-page "https://apps.kde.org/kleopatra/")
    (synopsis "Certificate Manager and Unified Crypto GUI")
    (description "Kleopatra is a certificate manager and a universal crypto
GUI.  It supports managing X.509 and OpenPGP certificates in the GpgSM keybox
and retrieving certificates from LDAP servers.")
    (license ;; GPL for programs, FDL for documentation
     (list license:gpl2+ license:fdl1.2+))))

(define-public kmail
  (package
    (name "kmail")
    (version "24.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kmail-" version ".tar.xz"))
       (sha256
        (base32 "1cl6giblw0jwyxr7bk88gd8krqkd3xxryniyz2jbrqnkvbrml23n"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules dbus kdoctools))
    (inputs
     (list akonadi
           akonadi-contacts
           akonadi-mime
           akonadi-search
           boost
           gpgme
           grantleetheme
           kaddressbook
           kbookmarks
           kcalendarcore
           kcalutils
           kcmutils
           kcodecs
           kconfig
           kconfigwidgets
           kcontacts
           kcrash
           kdbusaddons
           kguiaddons
           ki18n
           kiconthemes
           kidentitymanagement
           kimap
           kio
           kitemmodels
           kitemviews
           kjobwidgets
           kldap
           kmail-account-wizard
           kmailcommon
           kmailtransport
           kmessagelib
           kmime
           knotifications
           knotifyconfig
           kontactinterface
           kparts
           kpimcommon
           kpimtextedit
           kservice
           kstatusnotifieritem
           ksyntaxhighlighting
           ktextaddons
           ktextwidgets
           kuserfeedback
           ktnef
           kwallet
           kwidgetsaddons
           kwindowsystem
           kxmlgui
           libgravatar
           libkdepim
           libkleo
           libksieve
           breeze-icons ; default icon set, required for tests
           qgpgme-qt6
           qtdeclarative
           qtkeychain-qt6
           qtwebchannel
           qtwebengine
           sonnet))
    (arguments
     (list
      #:qtbase qtbase
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'wrap-program
            (lambda* (#:key inputs #:allow-other-keys)
              (define (find-program-directory name)
                (dirname (search-input-file
                          inputs (string-append "/bin/" name))))
              (wrap-program (string-append #$output "/bin/kmail")
                `("XDG_DATA_DIRS" ":" prefix
                  (,(getenv "XDG_DATA_DIRS")))
                `("PATH" ":" prefix
                  ,(map find-program-directory
                        (list "kaddressbook"
                              "akonadictl"
                              "accountwizard"))))))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "dbus-launch" "ctest" "-E" ;; FIXME: Many failing tests.
                        "(akonadi-sqlite-kmcomposerwintest|\
akonadi-sqlite-archivemailwidgettest|\
akonadi-sqlite-tagselectdialogtest|\
akonadi-sqlite-kmcommandstest|\
sendlateragent-sendlaterutiltest|\
sendlateragent-sendlaterconfigtest|\
followupreminder-followupreminderconfigtest|\
akonadi-sqlite-unifiedmailboxmanagertest)")))))))
    (home-page "https://kontact.kde.org/components/kmail/")
    (synopsis "Full featured graphical email client")
    (description "KMail supports multiple accounts, mail filtering and email
encryption.  The program let you configure your workflow and it has good
integration into KDE (Plasma Desktop) but is also usable with other Desktop
Environments.

KMail is the email component of Kontact, the integrated personal information
manager from KDE.")
    (license ;; GPL for programs, LGPL for libraries, FDL for documentation
     (list license:gpl2+ license:lgpl2.0+ license:fdl1.2+))))

(define-public kmail-account-wizard
  (package
    (name "kmail-account-wizard")
    (version "24.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kmail-account-wizard-" version ".tar.xz"))
       (sha256
        (base32 "0v0pczbwv6ba5ly4s2x8jw6b300f6wprlikz8hmx0xrk0mnx4zqh"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules shared-mime-info))
    (inputs
     (list akonadi
           akonadi-mime
           kcrash
           kcmutils
           kdbusaddons
           ki18n
           kiconthemes
           kidentitymanagement
           kimap
           kitemmodels
           kldap
           kmailtransport
           kmime
           knewstuff
           knotifications
           knotifyconfig
           kpimcommon
           kpimtextedit
           ktextaddons
           ktexteditor
           kwallet
           libkdepim
           libkleo
           qtkeychain-qt6))
    (arguments (list #:qtbase qtbase
                     ;; TODO: pass test.
                     #:tests? #f))
    (home-page "https://invent.kde.org/pim/kmail-account-wizard")
    (synopsis "Assistant for the configuration of accounts in KMail")
    (description
     "This package provides an assistant for the configuration of accounts in
KMail.")
    (license ;;GPL for programs, LGPL for libraries, FDL for documentation
     (list license:gpl2+ license:lgpl2.0+ license:fdl1.2+))))

(define-public kmailcommon
  (package
    (name "kmailcommon")
    (version "24.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/mailcommon-" version ".tar.xz"))
       (sha256
        (base32 "1jnq02ji9bqfk03idzf54ggzfwnpinivw35d56hjxpq41b0z9lhw"))))
    (properties `((upstream-name . "mailcommon")))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules dbus gnupg qttools))
    (inputs
     (list akonadi-contacts
           boost
           gpgme
           grantleetheme
           karchive
           kcodecs
           kconfig
           kconfigwidgets
           kcontacts
           kdbusaddons
           kguiaddons
           ki18n
           kiconthemes
           kidentitymanagement
           kimap
           kio
           kitemmodels
           kitemviews
           kldap
           kmailimporter
           kmailtransport
           kmime
           kpimtextedit
           ksyntaxhighlighting
           ktextaddons
           ktextwidgets
           kwallet
           kwidgetsaddons
           kwindowsystem
           kxmlgui
           libkleo
           libxslt
           phonon
           qgpgme
           qtwebchannel
           qtwebengine))
    (propagated-inputs (list akonadi
                             akonadi-mime
                             kcompletion
                             kmessagelib
                             kpimcommon
                             libkdepim))
    (arguments
     (list
      #:qtbase qtbase
      #:tests? #f))  ;; TODO: 12/62 tests fail
    (home-page "https://invent.kde.org/pim/mailcommon")
    (synopsis "KDE email utility library")
    (description "The mail common library provides utility functions for
dealing with email.")
    (license ;; GPL for programs, LGPL for libraries
     (list license:gpl2+ license:lgpl2.0+))))

(define-public kmailimporter
  (package
    (name "kmailimporter")
    (version "24.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/mailimporter-" version ".tar.xz"))
       (sha256
        (base32 "0l83ynsxbapcvjjzdlrfspfkg013w2c3hji1wcv97q8qpx4269j8"))))
    (properties `((upstream-name . "mailimporter")))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules))
    (inputs
     (list akonadi
           akonadi-contacts
           akonadi-mime
           grantleetheme
           boost
           kcompletion
           kconfig
           kconfigwidgets
           kcontacts
           kcoreaddons
           kdbusaddons
           ki18n
           kimap
           kio
           kitemmodels
           kmime
           kpimcommon
           kpimtextedit
           ktextaddons
           ktextwidgets
           kxmlgui
           libkdepim))
    (propagated-inputs (list karchive))
    (arguments (list #:qtbase qtbase))
    (home-page "https://invent.kde.org/pim/mailimporter")
    (synopsis "KDE mail importer library")
    (description "This package provides libraries for importing mails other
e-mail client programs into KMail and KDE PIM.")
    (license ;; GPL for programs, LGPL for libraries
     (list license:gpl2+ license:lgpl2.0+))))

(define-public kmailtransport
  (package
    (name "kmailtransport")
    (version "24.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kmailtransport-" version ".tar.xz"))
       (sha256
        (base32 "1bwla6i318armzay0hbnmjl9fy49m5d2bybq07zs509rnmi5af2x"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list akonadi
           akonadi-mime
           boost
           cyrus-sasl
           kcalendarcore
           kcmutils
           kcontacts
           kdbusaddons
           kconfigwidgets
           ki18n
           kitemmodels
           kio
           kmime
           ksmtp
           ktextwidgets
           kwallet
           libkgapi
           qtkeychain-qt6))
    (arguments
     (list
      #:qtbase qtbase
      #:tests? #f)) ;; 1/2 tests fail, require network.
    (home-page "https://api.kde.org/kdepim/kmailtransport/html/index.html")
    (synopsis "Mail transport service library")
    (description "This library provides an API and support code for managing
mail transport.")
    (license license:lgpl2.0+)))

(define-public kmbox
  (package
    (name "kmbox")
    (version "24.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kmbox-" version ".tar.xz"))
       (sha256
        (base32 "1z6rv3chc9y9ppy9jx1lj8nkl3wlcv8511p0zaarz9khsx413jns"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules))
    (inputs
     (list kcodecs kmime))
    (arguments (list #:qtbase qtbase))
    (home-page "https://api.kde.org/kdepim/kmbox/html/index.html")
    (synopsis "Library for handling mbox mailboxes")
    (description "This is a library for handling mailboxes in mbox format,
using a Qt/KMime C++ API.")
    (license license:lgpl2.0+ )))

(define-public kmessagelib
  (package
    (name "kmessagelib")
    (version "24.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/messagelib-" version ".tar.xz"))
       (sha256
        (base32 "0v8yrl6j5325mwnhix43qp4s9jjny2437hns9sr5zq7ry4zfas73"))))
    (properties `((upstream-name . "messagelib")))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules gnupg libxml2))
    (inputs
     (list akonadi-contacts
           akonadi-notes
           akonadi-search
           boost
           gpgme
           grantleetheme
           karchive
           kcalendarcore
           kcodecs
           kcompletion
           kconfig
           kconfigwidgets
           kcontacts
           kdbusaddons
           kguiaddons
           ki18n
           kiconthemes
           kimap
           kio
           kitemmodels
           kitemviews
           kjobwidgets
           kldap
           kmailtransport
           kmbox
           knewstuff
           knotifications
           kservice
           ksyntaxhighlighting
           ktextwidgets
           ktexttemplate
           kwallet
           kwidgetsaddons
           kwindowsystem
           kxmlgui
           openssl
           libgravatar
           qca-qt6
           qgpgme-qt6
           qtdeclarative
           qtwebchannel
           qtwebengine
           sonnet))
    (propagated-inputs
     (list akonadi
           akonadi-mime
           kidentitymanagement
           kmime
           kpimcommon
           kpimtextedit
           ktextaddons
           libkdepim
           libkleo))
    (arguments
     (list #:qtbase qtbase
           #:tests? #f     ;TODO many test fail for quite different reasons
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'add-miss-PrintSupport
                 (lambda _
                   (substitute* "webengineviewer/src/CMakeLists.txt"
                     (("KF6::ConfigCore")
                      "KF6::ConfigCore\n    Qt::PrintSupport")))))))
    (home-page "https://invent.kde.org/pim/messagelib")
    (synopsis "KDE PIM messaging libraries")
    (description "This package provides several libraries for messages,
e.g. a message list, a mime tree parse, a template parser and the
kwebengineviewer.")
    (license ;; GPL for programs, LGPL for libraries
     (list license:gpl2+ license:lgpl2.0+))))

(define-public kmime
  (package
    (name "kmime")
    (version "24.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kmime-" version ".tar.xz"))
       (sha256
        (base32 "1x0ylkpfj75bh0av6k2dvjhrxh6p4701lvvnqmx726pa8cp9ziq6"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules qttools tzdata-for-tests))
    (inputs
     (list kcodecs ki18n))
    (arguments
     (list #:qtbase qtbase
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'fix-test-case
                 (lambda* (#:key inputs tests? #:allow-other-keys)
                   (when tests?
                     (setenv "LC_ALL" "C.utf8") ;for 'testFancyFormat'
                     (setenv "TZDIR" (search-input-directory
                                      inputs "share/zoneinfo"))))))))
    (home-page "https://api.kde.org/stable/kdepimlibs-apidocs/")
    (synopsis "Library for handling MIME data")
    (description "This library provides an API for handling MIME
data.  MIME (Multipurpose Internet Mail Extensions) is an Internet Standard
that extends the format of e-mail to support text in character sets other than
US-ASCII, non-text attachments, multi-part message bodies, and header
information in non-ASCII character sets.")
    (license license:lgpl2.0+)))

(define-public knotes
  (package
    (name "knotes")
    (version "24.05.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/knotes-" version ".tar.xz"))
       (sha256
        (base32 "14nm2s86hqvvg0wyg8q5dd273dpppqw692h3mzya5mfg3j7acvaf"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools libxslt))
    (inputs
     (list akonadi
           akonadi-contacts
           akonadi-mime
           akonadi-notes
           akonadi-search
           boost
           breeze-icons ; default icon set, required for tests
           grantleetheme
           kcalendarcore
           kcalutils
           kcmutils
           kcompletion
           kconfig
           kconfigwidgets
           kcontacts
           kcoreaddons
           kcrash
           kdnssd
           kglobalaccel
           kiconthemes
           kimap
           kitemmodels
           kitemviews
           kmime
           knewstuff
           knotifications
           knotifyconfig
           kontactinterface
           kparts
           kpimcommon
           kpimtextedit
           kstatusnotifieritem
           ktextaddons
           ktextwidgets
           ktexttemplate
           kwidgetsaddons
           kwindowsystem
           kxmlgui
           kxmlgui
           libkdepim))
    (arguments (list #:qtbase qtbase))
    (home-page "https://apps.kde.org/knotes/")
    (synopsis "Note-taking utility")
    (description "KNotes lets you write the computer equivalent of sticky
notes.  The notes are saved automatically when you exit the program, and they
display when you open the program.

Features:
@itemize
@item Write notes in your choice of font and background color
@item Use drag and drop to email your notes
@item Can be dragged into Calendar to book a time-slot
@item Notes can be printed
@end itemize")
    (license (list license:gpl2+ license:lgpl2.0+))))

(define-public kontactinterface
  (package
    (name "kontactinterface")
    (version "24.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kontactinterface-" version ".tar.xz"))
       (sha256
        (base32 "0n4dxnavd0rsa6qa9929sd7bjk9x58r11w5i1i6hsmg2f5zwf7wv"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules))
    (inputs
     (list kcoreaddons
           ki18n
           kiconthemes
           kparts
           kwindowsystem
           kxmlgui
           libxkbcommon))
    (arguments
     (list #:qtbase qtbase
           #:tests? #f))
    (home-page "https://api.kde.org/kdepim/kontactinterface/html/index.html")
    (synopsis "Kontact interface library")
    (description "This library provides the glue necessary for
application \"Parts\" to be embedded as a Kontact component (or plugin).")
    (license license:lgpl2.0+)))

(define-public korganizer
  (package
    (name "korganizer")
    (version "24.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/korganizer-" version ".tar.xz"))
       (sha256
        (base32 "092ad5qw6jsvn7rf5gmkwcq5khlxr04lizdcflw70lnb7j4y7zpk"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules dbus qttools kdoctools tzdata-for-tests))
    (inputs
     (list akonadi
           akonadi-calendar
           akonadi-contacts
           akonadi-mime
           akonadi-notes
           akonadi-search
           boost
           grantleetheme
           kcalendarcore
           kcalendarsupport
           kcalutils
           kcmutils
           kcodecs
           kcompletion
           kconfig
           kconfigwidgets
           kcontacts
           kcoreaddons
           kcrash
           kdbusaddons
           keventviews
           kholidays
           kiconthemes
           kidentitymanagement
           kimap
           kincidenceeditor
           kitemmodels
           kitemviews
           kjobwidgets
           kldap
           kmailtransport
           kmime
           knewstuff
           knotifications
           kontactinterface
           kparts
           kpimcommon
           kpimtextedit
           kservice
           ktextaddons
           kwallet
           kwidgetsaddons
           kwindowsystem
           kxmlgui
           libkdepim
           breeze-icons ; default icon set, required for tests
           phonon))
    (arguments
     (list #:qtbase qtbase
           #:phases
           #~(modify-phases %standard-phases
               (replace 'check
                 (lambda* (#:key inputs tests? #:allow-other-keys)
                   (when tests?
                     (setenv "TZDIR" (search-input-directory
                                      inputs "share/zoneinfo"))
                     (invoke "dbus-launch" "ctest"
                             "-E" "akonadi-sqlite-koeventpopupmenutest")))))))
    (home-page "https://apps.kde.org/korganizer/")
    (synopsis "Organizational assistant, providing calendars and other similar
functionality")
    (description "KOrganizer is the calendar and scheduling component of
Kontact.  It provides management of events and tasks, alarm notification, web
export, network transparent handling of data, group scheduling, import and
export of calendar files and more.  It is able to work together with a wide
variety of calendaring services, including NextCloud, Kolab, Google Calendar
and others.  KOrganizer is fully customizable to your needs and is an integral
part of the Kontact suite, which aims to be a complete solution for organizing
your personal data.  KOrganizer supports the two dominant standards for storing
and exchanging calendar data, vCalendar and iCalendar.")
    (license ;; GPL for programs, LGPL for libraries, FDL for documentation
     (list license:gpl2+ license:lgpl2.0+ license:fdl1.2+))))

(define-public kpeoplevcard
  (let ((commit "d4acaa3038f6b030d0d6390e628c39e9f8d87903")
        (revision "0"))
    ;; use unreleased commit for KF6 supports.
    (package
      (name "kpeoplevcard")
      (version (git-version "0.1" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://invent.kde.org/pim/kpeoplevcard")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1wgpwxjb5zvhgzyq9zd8mp6r5cawjzlhk980drl05nr27a0q32vz"))))
      (build-system qt-build-system)
      (arguments
       (list #:qtbase qtbase
             #:configure-flags #~(list "-DQT_MAJOR_VERSION=6")
             #:phases #~(modify-phases %standard-phases
                          (replace 'check-setup
                            (lambda _
                              (setenv "HOME" "/tmp"))))))
      (native-inputs
       (list extra-cmake-modules))
      (inputs
       (list kcontacts kpeople))
      (home-page "https://invent.kde.org/pim/kpeoplevcard")
      (synopsis "Expose vCard contacts to KPeople")
      (description
       "This plugins adds support for vCard (also known as @acronym{VCF,
Virtual Contact File}) files to the KPeople contact management library.")
      (license license:lgpl2.1+))))

(define-public kpkpass
  (package
    (name "kpkpass")
    (version "24.12.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/release-service/" version
                                  "/src/kpkpass-" version ".tar.xz"))
              (sha256
               (base32
                "1hrqzy8rc51sixkmkfl8zvqpynyv7zb8sfjlbxslnjrib9p9i2vx"))))
    (build-system qt-build-system)
    (native-inputs (list extra-cmake-modules))
    (inputs (list karchive shared-mime-info))
    (arguments (list #:qtbase qtbase))
    (home-page "https://invent.kde.org/pim/kpkpass")
    (synopsis "Apple Wallet Pass reader")
    (description "This package provides library to deal with Apple Wallet
pass files.")
    (license license:lgpl2.0+)))

(define-public kpimcommon
  (package
    (name "kpimcommon")
    (version "24.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/pimcommon-" version ".tar.xz"))
       (sha256
        (base32 "1fwxjdjixbn84flq9g95ig7z4csi08amsblr6rm1fr2lk8nazmg4"))))
    (properties `((upstream-name . "pimcommon")))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules qttools))
    (inputs
     (list karchive
           akonadi
           akonadi-contacts
           akonadi-mime
           akonadi-search
           boost
           grantleetheme
           kaccounts-integration
           kcalendarcore
           kcmutils
           kcodecs
           kconfig
           kconfigwidgets
           kcontacts
           kcoreaddons
           ki18n
           kiconthemes
           kio
           kirigami ;; run-time dependency
           kitemmodels
           kitemviews
           kjobwidgets
           kldap
           kmime
           knewstuff
           kpimtextedit
           ktextwidgets
           ktexttemplate
           kwallet
           kwidgetsaddons
           kwindowsystem
           kxmlgui
           libxslt
           purpose
           qtwebengine))
    (propagated-inputs (list kimap ktextaddons libkdepim))
    (arguments
     (list #:qtbase qtbase))
    (home-page "https://invent.kde.org/pim/pimcommon")
    (synopsis "Common libraries for KDE PIM")
    (description "This package provides common libraries for KDE PIM.")
    (license ;; GPL for programs, LGPL for libraries
     (list license:gpl2+ license:lgpl2.0+))))

(define-public libgravatar
  (package
    (name "libgravatar")
    (version "24.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/libgravatar-" version ".tar.xz"))
       (sha256
        (base32 "1grs2dnxlh1lb1mv47aggymawvc6lb7yjrnw1sqwbiwj8ign2p66"))))
    (build-system qt-build-system)
    (native-inputs (list extra-cmake-modules))
    (inputs (list kconfig
                  ki18n
                  kio
                  kconfigwidgets
                  kpimcommon
                  kpimtextedit
                  ktextaddons
                  ktextwidgets
                  kwidgetsaddons
                  qtbase))
    (arguments
     (list #:qtbase qtbase
           #:tests? #f)) ;; 2/7 tests fail (due to network issues?)
    (home-page "https://invent.kde.org/pim/libgravatar")
    (synopsis "Online avatar lookup library")
    (description "This library retrieves avatar images based on a
hash from a person's email address, as well as local caching to avoid
unnecessary network operations.")
    (license ;; GPL for programs, LGPL for libraries
     (list license:gpl2+ license:lgpl2.0+))))

(define-public kpimtextedit
  (package
    (name "kpimtextedit")
    (version "24.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kpimtextedit-" version ".tar.xz"))
       (sha256
        (base32 "0xrlym88l2xsclcbmdl7jsphnf3z39hxgk0xdb6qjk719mi4w2lb"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules qttools))
    (inputs
     (list kcodecs
           kconfigwidgets
           kcoreaddons
           ktextaddons
           ki18n
           kiconthemes
           kio
           ksyntaxhighlighting
           ktextwidgets
           kwidgetsaddons
           kxmlgui
           qtspeech
           sonnet))
    (arguments
     (list #:qtbase qtbase
           #:tests? #f)) ;; TODO - test suite hangs
    (home-page "https://api.kde.org/kdepim/kpimtextedit/html/index.html")
    (synopsis "Library providing a textedit with PIM-specific features")
    (description "This package provides a textedit with PIM-specific features.
It also provides so-called rich text builders which can convert the formatted
text in the text edit to all kinds of markup, like HTML or BBCODE.")
    (license ;; GPL for programs, LGPL for libraries, FDL for documentation
     (list license:gpl2+ license:lgpl2.0+ license:fdl1.2+))))

(define-public ksmtp
  (package
    (name "ksmtp")
    (version "24.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/ksmtp-" version ".tar.xz"))
       (sha256
        (base32 "0kn4kh71kxjr8pgngdkm298xdzj3jbn44dyllvm582m035hm5nx4"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules))
    (inputs
     (list cyrus-sasl
           kcodecs
           kconfig
           kcoreaddons
           ki18n
           kio))
    (arguments
     (list #:qtbase qtbase
           #:tests? #f)) ;; TODO: does not find sasl mechs
    (home-page "https://invent.kde.org/pim/ksmtp")
    (synopsis "Library for sending email through an SMTP server")
    (description "This library provides an API for handling SMTP
services.  SMTP (Simple Mail Transfer Protocol) is the most prevalent Internet
standard protocols for e-mail transmission.")
    (license license:lgpl2.0+)))

(define-public ktnef
  (package
    (name "ktnef")
    (version "24.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/ktnef-" version ".tar.xz"))
       (sha256
        (base32 "1ylpmrzqgpkfdzmzgq9b1hvsvna6q0fvv1c9p9qxhnbaycyyg92g"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules))
    (inputs
     (list kcalendarcore
           kcalutils
           kcodecs
           kconfig
           kcontacts
           kcoreaddons
           ki18n))
    (arguments (list #:qtbase qtbase))
    (home-page "https://api.kde.org/kdepim/ktnef/html/index.html")
    (synopsis "Library for handling mail attachments using TNEF format")
    (description "Ktnef is a library for handling data in the TNEF
format (Transport Neutral Encapsulation Format, a proprietary format of e-mail
attachment used by Microsoft Outlook and Microsoft Exchange Server).  The API
permits access to the actual attachments, the message properties (TNEF/MAPI),
and allows one to view/extract message formatted text in Rich Text Format.")
    (license license:lgpl2.0+)))

(define-public libkdepim
  (package
    (name "libkdepim")
    (version "24.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/libkdepim-" version ".tar.xz"))
       (sha256
        (base32 "0f3lz2n2683zx0m13kd6k518idw1xfdj6v0371b1d57sz0bvgj3i"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules qttools))
    (inputs
     (list akonadi
           akonadi-contacts
           akonadi-mime
           akonadi-search
           boost
           kcmutils
           kcodecs
           kcalendarcore
           kcompletion
           kconfig
           kconfigwidgets
           kcontacts
           kcoreaddons
           kdbusaddons
           ki18n
           kiconthemes
           kio
           kitemmodels
           kitemviews
           kjobwidgets
           kldap
           kmime
           kwallet
           kwidgetsaddons))
    (arguments (list #:qtbase qtbase))
    (home-page "https://invent.kde.org/pim/libkdepim")
    (synopsis "Libraries for common KDE PIM apps")
    (description "This package provided libraries for common KDE PIM apps.")
    (license ;; GPL for programs, LGPL for libraries
     (list license:gpl2+ license:lgpl2.0+))))

(define-public libkgapi
  (package
    (name "libkgapi")
    (version "24.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/libkgapi-" version ".tar.xz"))
       (sha256
        (base32 "1k508grqzw60xjvjx8679ha8gsfvvvdyi9dn6xvz3zi34rhlqiim"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules qttools))
    (inputs
     (list cyrus-sasl
           ki18n
           kcontacts
           kcalendarcore
           kio
           kwallet
           kwindowsystem
           qtdeclarative
           qtwebchannel
           qtwebengine))
    (arguments
     (list #:qtbase qtbase
           #:tests? #f)) ;; TODO 6/48 tests fail
    (home-page "https://invent.kde.org/pim/libkgapi")
    (synopsis "Library for accessing various Google services via their public
API")
    (description "@code{LibKGAPI} is a C++ library that implements APIs for
various Google services.")
    (license license:lgpl2.0+)))

(define-public libkleo
  (package
    (name "libkleo")
    (version "24.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/libkleo-" version ".tar.xz"))
       (sha256
        (base32 "1jajjq0gnymkfhviixjiz3x3b97pcdrgcs63x92sj1qncvs51yjp"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools qttools))
    (inputs
     (list boost
           gpgme
           kcodecs
           kcompletion
           kconfig
           kconfigwidgets
           kcoreaddons
           kcrash
           ki18n
           kitemmodels
           kwidgetsaddons
           kwindowsystem
           kpimtextedit
           qgpgme-qt6))
    (propagated-inputs
     (list gpgme qgpgme-qt6))
    (arguments
     (list
      #:qtbase qtbase
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests? ;; FIXME: These tests fail.
                (invoke "ctest" "-E"
                        "(expirycheckertest|keyresolvercoretest|\
newkeyapprovaldialogtest|keyparameterstest|keycachetest)")))))))
    (home-page "https://invent.kde.org/pim/libkleo")
    (synopsis "KDE PIM cryptographic library")
    (description "@code{libkleo} is a library for Kleopatra and other parts of
KDE using certificate-based crypto.")
    (license ;; GPL for programs, LGPL for libraries
     (list license:gpl2+ license:lgpl2.0+))))

(define-public libksieve
  (package
    (name "libksieve")
    (version "24.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/libksieve-" version ".tar.xz"))
       (sha256
        (base32
	 "1syshv3i1qfs6mg8cds9jkpzgngkiy1i1l5p0qva2hvz6lp64kmk"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list akonadi
           cyrus-sasl
           grantleetheme
           kconfigwidgets
           karchive
           ki18n
           kiconthemes
           kidentitymanagement
           kimap
           kio
           kmailtransport
           kmime
           knewstuff
           kpimcommon
           kpimtextedit
           ksyntaxhighlighting
           ktextaddons
           ktextwidgets
           kwallet
           kwindowsystem
           libkdepim
           qtdeclarative
           qtwebchannel
           qtwebengine))
    (arguments
     (list #:qtbase qtbase
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'substitute
                 (lambda _
                   ;; Disable a failing test
                   ;; sieveeditorhelphtmlwidgettest fails with `sigtrap`
                   (substitute*
                       "src/ksieveui/editor/webengine/autotests/CMakeLists.txt"
                     (("^\\s*(add_test|ecm_mark_as_test|set_tests_properties)\\W" line)
                      (string-append "# " line))))))))
    (home-page "https://invent.kde.org/pim/libksieve")
    (synopsis "KDE Sieve library")
    (description "Sieve is a language that can be used filter emails.  KSieve
is a Sieve parser and interpreter library for KDE.")
    (license ;; GPL for programs, LGPL for libraries
     (list license:gpl2+ license:lgpl2.0+))))

(define-public merkuro
  (package
    (name "merkuro")
    (version "24.12.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/release-service/"
                                  version "/src/" name "-" version
                                  ".tar.xz"))
              (sha256
               (base32
                "18sdf4nd0dl36d3wv4hl99d1by5871wdblc9d9z72kph4jgyyacm"))))
    (build-system qt-build-system)
    (arguments
     (list #:qtbase qtbase
           #:tests? #f
           #:phases
           #~(modify-phases %standard-phases
               (replace 'check
                 (lambda* (#:key tests? #:allow-other-keys)
                   (when tests?
                     (invoke "dbus-launch" "ctest")))))))
    (native-inputs (list dbus extra-cmake-modules))
    (inputs (list akonadi
                  akonadi-calendar
                  akonadi-contacts
                  akonadi-mime
                  akonadi-mime
                  breeze-icons
                  gpgme
                  grantleetheme
                  kcalendarcore
                  kcalendarsupport
                  kcalutils
                  kconfigwidgets
                  kcontacts
                  kcoreaddons
                  kcrash
                  kdbusaddons
                  keventviews
                  ki18n
                  kiconthemes
                  kidentitymanagement
                  kimap
                  kio
                  kirigami
                  kirigami-addons
                  kitemmodels
                  kmailcommon
                  kmailtransport
                  kmessagelib
                  kmime
                  knotifications
                  kpimcommon
                  kpimtextedit
                  ktextaddons
                  ktextwidgets
                  kwindowsystem
                  kxmlgui
                  libkdepim
                  libkleo
                  mimetreeparser
                  qqc2-desktop-style
                  qtdeclarative
                  qtsvg
                  qtwebengine))
    (home-page "https://apps.kde.org/kalendar/")
    (synopsis "Calendar application")
    (description
     "Merkuro is a calendar application using Akonadi to sync with
external services.

NOTE: plsase add akonadi and kdepim-runtime to system package.")
    (license license:gpl3+)))

(define-public mimetreeparser
  (package
    (name "mimetreeparser")
    (version "24.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/mimetreeparser-" version ".tar.xz"))
       (sha256
        (base32 "0hpn8v2c6v5r2ydqha9xr28ki3ml9k3wf3lpra0i6q81bp4ainfi"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules))
    (inputs
     (list kcalendarcore kcodecs libkleo kwidgetsaddons qtdeclarative))
    (propagated-inputs (list ki18n kmime kmbox))
    (arguments
     (list #:tests? #f ;; FIXME: 7/9 tests fail.
           #:qtbase qtbase))
    (home-page "https://kontact.kde.org")
    (synopsis "Parser for MIME trees")
    (description "This package provides a parser for a MIME tree and is based
on KMime.  The goal is given a MIME tree to extract a list of parts and a list
of attachments, check the validity of the signatures and decrypt any encrypted
part.")
    (license license:lgpl2.0+)))
