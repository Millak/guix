;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2019, 2020 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2020 Marius Bakke <marius@gnu.org>
;;; Copyright © 2020, 2023-2025 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2021, 2022 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2022 Brendan Tildesley <mail@brendan.scot>
;;; Copyright © 2022 Petr Hodina <phodina@protonmail.com>
;;; Copyright © 2023-2025 Sughosha <sughosha@disroot.org>
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
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system cmake)
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
  #:use-module (gnu packages bison)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages calendar)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages cyrus-sasl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages geo)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages kde-multimedia)
  #:use-module (gnu packages kde-plasma)
  #:use-module (gnu packages kde-graphics)
  #:use-module (gnu packages markup)
  #:use-module (gnu packages messaging)
  #:use-module (gnu packages openldap)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages search)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml))

(define-public kopeninghours
  (package
    (name "kopeninghours")
    (version "25.12.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/release-service/"
                                  version "/src/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "0izsnzgwmdy8sxzqndbzxkjqf3mwqnwf5c15g5w1nzxxjl62ir54"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:configure-flags #~(list "-DBUILD_WITH_QT6=ON")
      #:phases #~(modify-phases %standard-phases
                   (replace 'check
                     (lambda* (#:key tests? #:allow-other-keys)
                       (when tests?
                         (setenv "QT_QPA_PLATFORM" "offscreen")
                         (invoke "ctest" "-E"
                                 "(evaluatetest|iterationtest)")))))))
    (native-inputs (list bison extra-cmake-modules flex))
    (inputs (list boost
                  kholidays
                  ki18n
                  osmctools
                  qtbase
                  qtdeclarative))
    (home-page "https://invent.kde.org/libraries/kopeninghours")
    (synopsis "Get opening hours from OpenStreetMap")
    (description
     "This package provides a library for parsing and evaluating OpenStreetMap
opening hours expressions.")
    (license license:lgpl2.0+)))

(define-public kosmindoormap
  (package
    (name "kosmindoormap")
    (version "25.12.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/release-service/"
                                  version "/src/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "1m2f7cgns6grighyv6p943nfmrc0rf5x9wc887zynhp4w8d45frw"))))
    (build-system cmake-build-system)
    (native-inputs (list bison extra-cmake-modules flex python-minimal))
    (inputs (list ki18n
                  kirigami-addons
                  kopeninghours
                  kpublictransport
                  qtbase
                  qtdeclarative
                  libxkbcommon
                  zlib))
    (home-page "https://invent.kde.org/libraries/kosmindoormap")
    (synopsis "Indoor map renderer")
    (description
     "This package provides facilities for rendering OpenStreetMap
multi-floor indoor maps.")
    (license license:lgpl2.0+)))

(define-public grantleetheme
  (package
    (name "grantleetheme")
    (version "25.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/grantleetheme-" version ".tar.xz"))
       (sha256
        (base32 "0iswifrwndyd8vcj179zhq7331y91g3kjpq4f3ip5xyz8vq9xr0z"))))
    (build-system qt-build-system)
    (arguments (list
                #:qtbase qtbase
                #:tests? #f))  ; unexpected error in the test suite.
    (native-inputs
     (list extra-cmake-modules libxml2)) ;; xmllint required for tests
    (inputs
     (list kguiaddons
           ki18n
           kiconthemes
           knewstuff
           kxmlgui))
    (propagated-inputs (list ktexttemplate))
    (home-page "https://invent.kde.org/pim/grantleetheme")
    (synopsis "Library providing Grantlee theme support")
    (description "This library provides Grantlee theme support.")
    (license ;; LGPL for libraries, FDL for documentation
     (list license:lgpl2.1+ license:fdl1.2+))))

(define-public akonadi
  (package
    (name "akonadi")
    (version "25.12.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/release-service/"
                                  version "/src/akonadi-" version ".tar.xz"))
              (sha256
               (base32
                "1p30cv8x75jn5y0xz78wygi8m69c6mghiyvgbmz8arghwv4zzl49"))
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
           signond-qt6))
    (propagated-inputs (list sqlite kaccounts-integration))
    (arguments
     (list #:qtbase qtbase
           #:tests? #f
           #:test-exclude
           (string-append "("
                          (string-join '("AkonadiServer-dbconfigtest"
                                         "mimetypecheckertest"
                                         "entitytreemodeltest"
                                         "akonadi-sqlite-testenvironmenttest"
                                         "akonadi-sqlite-autoincrementtest"
                                         "akonadi-sqlite-attributefactorytest"
                                         "akonadi-sqlite-collectionpath\
resolvertest"
                                         "akonadi-sqlite-collectionattribute\
test"                                    "akonadi-sqlite-itemfetchtest"
                                         "akonadi-sqlite-itemappendtest"
                                         "akonadi-sqlite-itemstoretest"
                                         "akonadi-sqlite-itemdeletetest"
                                         "akonadi-sqlite-entitycachetest"
                                         "akonadi-sqlite-monitortest"
                                         "akonadi-sqlite-changerecordertest"
                                         "akonadi-sqlite-resourcetest"
                                         "akonadi-sqlite-subscriptiontest"
                                         "akonadi-sqlite-transactiontest"
                                         "akonadi-sqlite-itemcopytest"
                                         "akonadi-sqlite-itemmovetest"
                                         "akonadi-sqlite-invalidatecachejob\
test"
                                         "akonadi-sqlite-collectioncreatetest"
                                         "akonadi-sqlite-collectioncopytest"
                                         "akonadi-sqlite-collectionmovetest"
                                         "akonadi-sqlite-collectionsynctest"
                                         "akonadi-sqlite-itemsynctest")
                                       "|")
                          ")")
           #:configure-flags #~'("-DDATABASE_BACKEND=SQLITE") ;lightweight
           #:modules `((ice-9 textual-ports)
                       ,@%qt-build-system-modules)
           #:phases
           #~(modify-phases (@ (guix build qt-build-system) %standard-phases)
               (replace 'check
                 (lambda* (#:key tests? (test-exclude "") #:allow-other-keys)
                   (when tests?
                     (setenv "PATH"
                             (string-append (getcwd) "/bin" ":"
                                            (getenv "PATH")))
                     (invoke "dbus-launch" "ctest" "-E" test-exclude))))
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
    (version "25.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/akonadi-calendar-" version ".tar.xz"))
       (sha256
        (base32 "04p9n0m3iksw31zwhg21jz25ip4agp6jx86xcswf6hjl65jqpydq"))))
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
    (version "25.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/akonadi-contacts-" version ".tar.xz"))
       (sha256
        (base32 "0xala89nypbf3jgi5mkpxsywrfa1b5zkd70xmnfnz2n3xs382ljz"))))
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
    (version "25.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/akonadi-mime-" version ".tar.xz"))
       (sha256
        (base32 "1y7m0jrmm4r9pvhj8lhvj1wdn51gph2alpbyd7j34jsz4m0g5pcd"))))
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

(define-public akonadi-search
  (package
    (name "akonadi-search")
    (version "25.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/akonadi-search-" version ".tar.xz"))
       (sha256
        (base32 "1xmq9jjz8dvxgc8prbdg9pgdrdrv0d6k31qkpggfd220ygvrri4d"))))
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
      ;; FIXME: This test fails because it fails to establish a socket
      ;; connection, seemingly due to failure during DBus communication.  See
      ;; also 'korganizer'.
      #:test-exclude "akonadi-sqlite-collectionindexingjobtest"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'disable-failing-test
            (lambda _

              (substitute* "agent/autotests/CMakeLists.txt"
                ((".*schedulertest\\.cpp.*")
                 ""))))
          (replace 'check
            (lambda* (#:key tests? (test-exclude "") #:allow-other-keys)
              (when tests?
                (invoke "dbus-launch" "ctest" "-E" test-exclude)))))))
    (home-page "https://api.kde.org/kdepim/akonadi/html/index.html")
    (synopsis "Akonadi search library")
    (description "This package provides a library used to search in the
Akonadi PIM data server.  It uses Xapian for indexing and querying.")
    (license ;; GPL for programs, LGPL for libraries
     (list license:gpl2+ license:lgpl2.0+))))

(define-public akonadi-import-wizard
  (package
    (name "akonadi-import-wizard")
    (version "25.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/akonadi-import-wizard-" version
                           ".tar.xz"))
       (sha256
        (base32 "12v22gajm8dnyly8wcf8whs1ddi4rya0p6bjpscq4kyaz23nflzf"))))
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

(define-public akregator
  (package
    (name "akregator")
    (version "25.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/akregator-" version ".tar.xz"))
       (sha256
        (base32 "17aishnjfl5bjn5ahs57v770lwccfvmcdll92gs55l63km9dwlkp"))))
    (build-system qt-build-system)
    (arguments
     (list #:qtbase qtbase))
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list akonadi
           akonadi-contacts
           akonadi-mime
           boost
           breeze-icons
           gpgme
           grantleetheme
           kcmutils
           kcontacts
           kcrash
           kiconthemes
           kimap
           kitemmodels
           kmessagelib
           kmime
           knotifications
           knotifyconfig
           kontactinterface
           kpimcommon
           kpimtextedit
           kquickcharts
           kstatusnotifieritem
           ktextaddons
           ktexteditor
           ktextwidgets
           kuserfeedback
           libkdepim
           libkleo
           qgpgme
           qtdeclarative
           qtwayland
           qtwebchannel
           qtwebengine
           syndication))
    (home-page "https://apps.kde.org/en/akregator")
    (synopsis "KDE Feed Reader")
    (description
     "Akregator is a news feed reader.  It enables you to follow news
sites, blogs and other RSS/Atom-enabled websites without the need to manually
check for updates using a web browser.  Akregator is designed to be both easy to
use and to be powerful enough to read hundreds of news sources conveniently.
It comes with a fast search, advanced archiving functionality and an internal
browser for easy news reading.")
    (license license:gpl2+)))

(define-public itinerary
  (package
    (name "itinerary")
    (version "25.12.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/release-service/"
                                  version "/src/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "1g50w82qj8wwgafs7c41pkyrx6lqljls53dj9r561911nm5n6k7n"))))
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
                  kcalendarcore
                  kcontacts
                  kcoreaddons
                  kcrash
                  kdbusaddons
                  kfilemetadata
                  khealthcertificate
                  kholidays
                  ki18n
                  kio
                  kirigami
                  kirigami-addons
                  kitemmodels
                  kitinerary
                  kmime
                  knotifications
                  kopeninghours
                  kosmindoormap
                  kpkpass
                  kpublictransport
                  kunitconversion
                  kwindowsystem
                  libical
                  libqmatrixclient
                  olm
                  openssl
                  prison
                  qcoro-qt6
                  qqc2-desktop-style
                  qtdeclarative
                  qtkeychain-qt6
                  qtlocation
                  qtmultimedia
                  qtpositioning
                  qtwayland
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
    (version "25.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/incidenceeditor-" version ".tar.xz"))
       (sha256
        (base32 "1kz738byg15nrym3d94wmv783jjwqqjl4zb802pipq9scvj1r8kd"))))
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
      ;; FIXME: These tests fail.
      #:test-exclude
      (string-append "("
                     (string-join '("akonadi-sqlite-incidencedatetimetest"
                                    "ktimezonecomboboxtest"
                                    "testindividualmaildialog")
                                  "|")
                     ")")
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key tests? (test-exclude "") #:allow-other-keys)
              (when tests?
                (invoke "dbus-launch" "ctest" "-E" test-exclude)))))))
    (home-page "https://invent.kde.org/pim/incidenceeditor")
    (synopsis "KDE PIM library for editing incidences")
    (description "This library provides an incidence editor for KDE PIM.")
    (license ;; GPL for programs, LGPL for libraries
     (list license:gpl2+ license:lgpl2.0+))))

(define-public kaddressbook
  (package
    (name "kaddressbook")
    (version "25.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kaddressbook-" version ".tar.xz"))
       (sha256
        (base32 "0pnr2ya7n29zyyd9gm7xd75cmynky8bx5r7qmmcx1q8sghi73jcg"))))
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
           qgpgme
           qtwayland))
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
    (version "25.12.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/release-service/"
                                  version "/src/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "04a7pwg09af2n097v87ga73jqpa6vvq4prm6b6sm2f3ffxbkxinn"))))
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
    (version "25.08.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/release-service/"
                                  version "/src/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "0vqh8mc3rhg6mq12cji3svy2scy7jz6ird1bz9syysjr3gr4wahj"))))
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
    (version "25.08.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/calendarsupport-" version ".tar.xz"))
       (sha256
        (base32 "1bcxryikk45f5pnhppcfja535qhw1vklhfhdsxk19i78h5cp6hw3"))))
    (properties `((upstream-name . "calendarsupport")))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules qttools))
    (inputs
     (list akonadi
           akonadi-mime
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
    (version "25.08.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kcalutils-" version ".tar.xz"))
       (sha256
        (base32 "0bcan45zpx9sya6j8lcjn9q40wpz5fx90fliq3is82sigav3g86f"))))
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
    (version "25.08.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kdepim-addons-" version ".tar.xz"))
       (sha256
        (base32 "1mww8xnmgg2nhcwc6lil0xabzp80nfvpnkd5s5r8kry98wbmra5s"))))
    (build-system qt-build-system)
    (arguments
     (list #:qtbase qtbase
           #:test-exclude
           (string-append "("
                          (string-join '("kdepim-addons-todoedittest"
                                         "kdepim-addons-eventedittest"
                                         "enterpriseheaderstyleplugintest"
                                         "fancyheaderstyleplugintest"
                                         "grantleeheaderstyleplugintest"
                                         "messageviewerplugins-rendertest"
                                         "akonadi-sqlite-rendertest-akonadi"
                                         "akonadi-sqlite-mailsenderjobtest"
                                         "akonadi-sqlite-gravatarupdatewidget\
test"
                                         "eventdatavisitortest" ;FIXME: enable
                                         "pimeventsplugintest" ;FIXME: enable
                                         "messageviewer-dkimauthentication\
verifiedserverdialogtest" ;SEGFAULT
                                         "markdowncreateimagewidgettest")
                                       "|")
                          ")")
           #:imported-modules
           `(,@%qt-build-system-modules
             ,@%cargo-build-system-modules)
           #:modules
           '(((guix build cargo-build-system) #:prefix cargo:)
             (guix build qt-build-system)
             (guix build utils))
           #:phases
           (with-extensions (list (cargo-guile-json))
           #~(modify-phases %standard-phases
               (add-before 'configure 'change-directory-to-adblock
                 (lambda _
                   (chdir "plugins/webengineurlinterceptor/adblock")))
               (add-after 'change-directory-to-adblock 'unpack-rust-crates
                 (assoc-ref cargo:%standard-phases 'unpack-rust-crates))
               (add-after 'unpack-rust-crates 'configure-adblock
                 (assoc-ref cargo:%standard-phases 'configure))
               (add-after 'configure-adblock 'check-for-pregenerated-files
                 (assoc-ref cargo:%standard-phases
                            'check-for-pregenerated-files))
               (add-after 'check-for-pregenerated-files 'patch-cargo-checksums
                 (assoc-ref cargo:%standard-phases 'patch-cargo-checksums))
               (add-after 'patch-cargo-checksums 'build-adblock
                 (assoc-ref cargo:%standard-phases 'build))
               (add-after 'build-adblock 'change-directory-back-to-source
                 (lambda _
                   (chdir "../../..")))
               (replace 'check
                 (lambda* (#:key tests? (test-exclude "") #:allow-other-keys)
                   (setenv "HOME" "/tmp")
                   (when tests?
                     (invoke "dbus-launch" "ctest" "-E" test-exclude))))))))
    (native-inputs
     (list corrosion
           dbus
           extra-cmake-modules
           pkg-config
           rust
           `(,rust "cargo")
           libxml2)) ;libxml2 for xmllint
    (inputs
     (cons* akonadi
            akonadi-calendar
            akonadi-contacts
            akonadi-import-wizard
            akonadi-mime
            discount
            grantlee
            grantleetheme
            kaddressbook
            karchive
            kcalendarcore
            kcalendarsupport
            kcalutils
            kcmutils
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
            plasma-activities
            prison
            qtwebengine
            (cargo-inputs 'kdepim-addons)))
    (home-page "https://invent.kde.org/pim/kdepim-addons")
    (synopsis "Add-ons for KDE PIM applications")
    (description "This package contains add-ons for KDE PIM applications such
as KMail, KAddressBook etc.")
    (license
     (list license:gpl2+ license:lgpl2.0+))))

(define-public kdepim-runtime
  (package
    (name "kdepim-runtime")
    (version "25.08.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kdepim-runtime-" version ".tar.xz"))
       (sha256
        (base32 "11azlr7vbbih8vxv2p1znxchdghkcmki39b9j4yqsrbifbqardwq"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules dbus kdoctools libxslt shared-mime-info))
    (inputs
     (list akonadi
           akonadi-calendar
           akonadi-contacts
           akonadi-mime
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
           ;; FIXME: Atleast some appear to require network.
           #:test-exclude
           (string-append "("
                          (string-join '("akonadi-sqlite-icalcategoriestotags\
migrationtest"
                                         "akonadi-sqlite-synctest"
                                         "akonadi-sqlite-pop3test"
                                         "storecompacttest"
                                         "akonadi-sqlite-ewstest"
                                         "ewsmoveitemrequest_ut"
                                         "ewsdeleteitemrequest_ut"
                                         "ewsgetitemrequest_ut"
                                         "ewsunsubscriberequest_ut"
                                         "ewssettings_ut"
                                         "templatemethodstest"
                                         "akonadi-sqlite-serverbusytest"
                                         "ewsattachment_ut"
                                         "testmovecollectiontask")
                                       "|")
                          ")")
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'set-paths 'extend-CPLUS_INCLUDE_PATH
                 (lambda* (#:key inputs #:allow-other-keys)
                   ;; FIXME: <Akonadi/KMime/SpecialMailCollections> is not
                   ;; found during one of the compilation steps without
                   ;; this hack.
                   (setenv "CPLUS_INCLUDE_PATH"
                           (string-append (assoc-ref inputs "akonadi-mime")
                                          "/include/KF6:"
                                          (or (getenv "CPLUS_INCLUDE_PATH")
                                              "")))))
               (replace 'check
                 (lambda* (#:key tests? (test-exclude "") #:allow-other-keys)
                   (when tests?
                     (invoke "dbus-launch" "ctest" "-E" test-exclude)))))))
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
    (version "25.08.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/eventviews-" version ".tar.xz"))
       (sha256
        (base32 "0giq1v8jsy55yrqcc8nzq8k2z2ryfs9yzfdgkmdlyzcwjccws5pa"))))
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
    (version "25.08.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kgpg-" version ".tar.xz"))
       (sha256
        (base32 "1bzq88c961jhygq29f67mdnzcb6a2rc9w53gv16571k82ip9y7p4"))))
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
           kxmlgui
           qtwayland))
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
    (version "25.08.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/release-service/" version
                                  "/src/khealthcertificate-" version ".tar.xz"))
              (sha256
               (base32
                "1nkd4pxj6cpcqpmiv0ylcvrkqj2z3k3y5kj0kzhw3g6yanhigvbc"))))
    (build-system qt-build-system)
    (arguments
     (list
      #:qtbase qtbase
      #:test-exclude
      (string-append "("
                     (string-join '("icaovdsparsertest"
                                    "eudgcparsertest")
                                  "|")
                     ")")
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key inputs tests? (test-exclude "") #:allow-other-keys)
              (when tests?
                (setenv "TZDIR"
                        (search-input-directory inputs "share/zoneinfo"))
                (invoke "ctest" "-E" test-exclude)))))))
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
    (version "25.08.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kidentitymanagement-" version ".tar.xz"))
       (sha256
        (base32 "1nzp9ww8nc07cf74zlx7b8ssvrqmv5d919n479p0c0nh8kzixcpv"))))
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
    (version "25.08.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kimap-" version ".tar.xz"))
       (sha256
        (base32 "0gdbi4s46lkwha8fgdmp0n4s4lc1sn1pgf45yj6km0xqn9y31f6n"))))
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
    (version "25.08.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/release-service/"
                                  version "/src/kitinerary-" version ".tar.xz"))
              (sha256
               (base32
                "083jyh8qjqgajj0jvx0n7vayd2085mzwp3682yy5y4x5nvyhcx5k"))))
    (build-system qt-build-system)
    (arguments
     (list #:qtbase qtbase
           #:test-exclude
           (string-append "("
                          (string-join '("jsonlddocumenttest"
                                         "mergeutiltest"
                                         "locationutiltest"
                                         "knowledgedbtest"
                                         "airportdbtest"
                                         "extractorscriptenginetest"
                                         "pkpassextractortest"
                                         "postprocessortest"
                                         "calendarhandlertest"
                                         "extractortest")
                                       "|")
                          ")")
           #:phases
           #~(modify-phases %standard-phases
               (replace 'check
                 (lambda* (#:key inputs tests? (test-exclude "")
                           #:allow-other-keys)
                   (when tests?
                     (setenv "TZDIR"
                             (search-input-directory inputs "share/zoneinfo"))
                     (invoke "dbus-launch" "ctest" "-E" test-exclude)))))))
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
    (version "25.08.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kldap-" version ".tar.xz"))
       (sha256
        (base32 "0cn0ywd1nnbrmgjfzkn9h9vq0dsmgmi6ci3dz2g5kf93yx1q05v2"))))
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
    (version "25.08.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kleopatra-" version ".tar.xz"))
       (sha256
        (base32 "0rk7ri8kp06ihp1gqyl91qk46v1a6j9jm0qycdr6zkfagqh6abrw"))))
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
           qgpgme
           qtwayland))
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
    (version "25.08.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kmail-" version ".tar.xz"))
       (sha256
        (base32 "1x4idbflx49yphb3168a5s0bnfsrkshiih4rl2x4vpzsw9yfhi55"))))
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
           qgpgme
           qtdeclarative
           qtkeychain-qt6
           qtwayland
           qtwebchannel
           qtwebengine
           sonnet))
    (arguments
     (list
      #:qtbase qtbase
      ;; FIXME: Many failing tests.
      #:test-exclude
      (string-append "("
                     (string-join '("akonadi-sqlite-kmcomposerwintest"
                                    "akonadi-sqlite-archivemailwidgettest"
                                    "akonadi-sqlite-tagselectdialogtest"
                                    "akonadi-sqlite-kmcommandstest"
                                    "sendlateragent-sendlaterutiltest"
                                    "sendlateragent-sendlaterconfigtest"
                                    "followupreminder-followupreminderconfig\
test"
                                    "akonadi-sqlite-unifiedmailboxmanagertest")
                                  "|")
                     ")")
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
            (lambda* (#:key tests? (test-exclude "") #:allow-other-keys)
              (when tests?
                (invoke "dbus-launch" "ctest" "-E" test-exclude)))))))
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
    (version "25.08.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kmail-account-wizard-" version ".tar.xz"))
       (sha256
        (base32 "0i63k32530czi0k085xf6g199xp313y0iccr2z1jj6i0adhz2r9k"))))
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
           qtkeychain-qt6
           qtwayland))
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
    (version "25.08.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/mailcommon-" version ".tar.xz"))
       (sha256
        (base32 "1yj32yqi7aknibvcc224v7rcgqjkbb6zm4sraavh03llfgw8ciiw"))))
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
           qtmultimedia
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
    (version "25.08.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/mailimporter-" version ".tar.xz"))
       (sha256
        (base32 "1afsdz67n2vwjfqvzmkig7iz0gw12a9m3vbdflr56fjbwamh3jzh"))))
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
    (version "25.08.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kmailtransport-" version ".tar.xz"))
       (sha256
        (base32 "1bip4prvds1nmifpyr4q291r6sh6y409fa8x4znwl9gdckmqbyr1"))))
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
    (version "25.08.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kmbox-" version ".tar.xz"))
       (sha256
        (base32 "132my33ivkpbrxnx0w92lqr0ih1z9ac0n2ifvldmcf3y73x8aq9i"))))
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
    (version "25.08.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/messagelib-" version ".tar.xz"))
       (sha256
        (base32 "1y37ylr03dm6sk5m7v4clav306kfqxiragcbr590fyax1a1pij57"))))
    (properties `((upstream-name . "messagelib")))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules gnupg libxml2))
    (inputs
     (list akonadi-contacts
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
           qgpgme
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
    (version "25.08.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kmime-" version ".tar.xz"))
       (sha256
        (base32 "00bzhfhrljjf2w05cjsmv0x6khfkywyhgn7kki6xi7wrzwxwlkqz"))))
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

(define-public kontactinterface
  (package
    (name "kontactinterface")
    (version "25.08.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kontactinterface-" version ".tar.xz"))
       (sha256
        (base32 "124p0pcb764648fa2xx7wfkqrpkyv1pj4z0fs0p8kgmg0wwd0nbv"))))
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
    (version "25.08.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/korganizer-" version ".tar.xz"))
       (sha256
        (base32 "0xdrn3v90vpgxbsx9jwf0763p25jdg4zr57i5xjxi4jvb38s18b2"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules dbus qttools kdoctools tzdata-for-tests))
    (inputs
     (list akonadi
           akonadi-calendar
           akonadi-contacts
           akonadi-mime
           akonadi-search
           boost
           breeze-icons ; default icon set, required for tests
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
           phonon
           qtwayland))
    (arguments
     (list #:qtbase qtbase
           #:test-exclude "akonadi-sqlite-koeventpopupmenutest"
           #:phases
           #~(modify-phases %standard-phases
               (replace 'check
                 (lambda* (#:key inputs tests? (test-exclude "")
                           #:allow-other-keys)
                   (when tests?
                     (setenv "TZDIR" (search-input-directory
                                      inputs "share/zoneinfo"))
                     (invoke "dbus-launch" "ctest" "-E" test-exclude)))))))
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

(define-public kpkpass
  (package
    (name "kpkpass")
    (version "25.08.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/release-service/" version
                                  "/src/kpkpass-" version ".tar.xz"))
              (sha256
               (base32
                "12vm7vgqwngvi4i3p23d722973j77sv3wfy0fplip46x7vrac0iq"))))
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
    (version "25.08.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/pimcommon-" version ".tar.xz"))
       (sha256
        (base32 "123vhygbyrcbj8rw8ny1kb2hd11lwml2xkw3xp75bdbqifv0d0py"))))
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
           plasma-activities
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

(define-public kpublictransport
  (package
    (name "kpublictransport")
    (version "25.08.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/release-service/"
                                  version "/src/kpublictransport-" version
                                  ".tar.xz"))
              (sha256
               (base32
                "05vcb00s6jq8lzwc8yls9qjn4vf2mvgrk75mf78yp8847pr7v27d"))))
    (build-system qt-build-system)
    (arguments
     (list #:qtbase qtbase
           #:test-exclude
           (string-append "("
                          (string-join '("updatetest"
                                         "motis2parsertest")
                                       "|")
                          ")")
           #:phases #~(modify-phases %standard-phases
                        (add-before 'check 'check-setup
                          (lambda* (#:key inputs #:allow-other-keys)
                            (setenv "QT_QPA_PLATFORM" "offscreen")
                            (setenv "HOME" ".")
                            (setenv "TZ" "Europe/Prague")
                            (setenv "TZDIR"
                                    (search-input-directory inputs
                                                            "share/zoneinfo")))))))
    (native-inputs (list extra-cmake-modules pkg-config tzdata-for-tests))
    ;; TODO: clipper and osmctools are not detected
    (inputs (list clipper
                  kirigami-addons
                  osmctools
                  protobuf
                  qtdeclarative
                  zlib
                  networkmanager-qt
                  ki18n))
    (home-page "https://api.kde.org/kdepim/kpublictransport/html/index.html")
    (synopsis "Library for accessing realtime public transport data")
    (description
     "This package provides a library for accessing realtime public
transport data and for performing public transport journey queries.")
    (license (list license:lgpl2.0+))))

(define-public libgravatar
  (package
    (name "libgravatar")
    (version "25.08.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/libgravatar-" version ".tar.xz"))
       (sha256
        (base32 "1yr3dxgbgbdf7ibfzzpbb809fc4lkmljg8j49n5smbdk08k7giwl"))))
    (build-system cmake-build-system)
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
     (list #:tests? #f)) ;; 2/7 tests fail (due to network issues?)
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
    (version "25.08.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kpimtextedit-" version ".tar.xz"))
       (sha256
        (base32 "0ikdwkzxg7bwpxdcs4725vjc531gh72c20j78akkwaxd2jkgz6s2"))))
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
    (version "25.08.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/ksmtp-" version ".tar.xz"))
       (sha256
        (base32 "1rnc2pja4ciqwmgksli2z1yzvzl5sya67s61dgjcxw93fy0421qm"))))
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
    (version "25.08.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/ktnef-" version ".tar.xz"))
       (sha256
        (base32 "04z20lksck42pvz9dx048rpqgdmi0r1vkrmlv6wxv70ia1wlf5bh"))))
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
    (version "25.08.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/libkdepim-" version ".tar.xz"))
       (sha256
        (base32 "1sdis1614734zldn7dzb8h6d0jvdfvyggwh5jdlv6np33fvmbjis"))))
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
    (version "25.08.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/libkgapi-" version ".tar.xz"))
       (sha256
        (base32 "0pnizcxj8n88ijbs982l73v5gpvwr7bwg87jacxhqjng0kd90ras"))))
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
    (version "25.08.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/libkleo-" version ".tar.xz"))
       (sha256
        (base32 "05mqa2k1r8v7mxz67hfj8dyx1r1ag15076dfbzzzpdaw565b2lq5"))))
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
           qgpgme))
    (propagated-inputs
     (list gpgme qgpgme))
    (arguments
     (list
      #:qtbase qtbase
      #:test-exclude
      (string-append "("
                     (string-join '("expirycheckertest"
                                    "keyresolvercoretest"
                                    "newkeyapprovaldialogtest"
                                    "keyparameterstest"
                                    "keycachetest")
                                  "|")
                          ")")
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key tests? (test-exclude "") #:allow-other-keys)
              (when tests? ;; FIXME: These tests fail.
                (invoke "ctest" "-E" test-exclude)))))))
    (home-page "https://invent.kde.org/pim/libkleo")
    (synopsis "KDE PIM cryptographic library")
    (description "@code{libkleo} is a library for Kleopatra and other parts of
KDE using certificate-based crypto.")
    (license ;; GPL for programs, LGPL for libraries
     (list license:gpl2+ license:lgpl2.0+))))

(define-public libksieve
  (package
    (name "libksieve")
    (version "25.08.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/libksieve-" version ".tar.xz"))
       (sha256
        (base32
	 "16gzbxvp2sc3piw74fxfnxhvncvi6v287asgck2hfh0cgbmn61kf"))))
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
           #:test-exclude "sieveeditorhelphtmlwidgettest"))
    (home-page "https://invent.kde.org/pim/libksieve")
    (synopsis "KDE Sieve library")
    (description "Sieve is a language that can be used filter emails.  KSieve
is a Sieve parser and interpreter library for KDE.")
    (license ;; GPL for programs, LGPL for libraries
     (list license:gpl2+ license:lgpl2.0+))))

(define-public merkuro
  (package
    (name "merkuro")
    (version "25.08.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/release-service/"
                                  version "/src/" name "-" version
                                  ".tar.xz"))
              (sha256
               (base32
                "1b8j2chnnc2ysadsp1g4xgaxsfrbzi0qbzvblmsr6l90b9ywn8d0"))))
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
                  akonadi-search
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
                  kholidays
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
                  qtlocation
                  qtpositioning
                  qtsvg
                  qtwayland
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
    (version "25.08.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/mimetreeparser-" version ".tar.xz"))
       (sha256
        (base32 "0xxr6p44764pi5d58nxmc405sw50xwbbv50lli8aj73yr0zwy5pa"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules))
    (inputs
     (list kcalendarcore
           kcodecs
           kcolorscheme
           libkleo
           kwidgetsaddons
           qtdeclarative))
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

(define-public zanshin
  (package
    (name "zanshin")
    (version "25.08.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/zanshin-" version ".tar.xz"))
       (sha256
        (base32 "10bd545qr0qj6l552rb1bx7fri5fzn75wnrrybz5f4ra5wc103fw"))))
    (build-system qt-build-system)
    (arguments
     (list ;; TODO: Do not exclude this test when
           ;; AkonadiSerializerTest::shouldNotBreakRecurrenceDuringSerialization
           ;; passes.
           #:test-exclude "tests-units-akonadi-akonadiserializertest"
           #:phases
           #~(modify-phases %standard-phases
               (add-before 'check 'check-setup
                 (lambda _
                   (setenv "HOME" (getcwd))))
               (replace 'check
                 (lambda* (#:key tests? (test-exclude "") #:allow-other-keys)
                   (when tests?
                     (invoke "dbus-launch" "ctest" "-E" test-exclude)))))))
    (native-inputs
     (list dbus extra-cmake-modules))
    ;; TODO: Unbundle mockitopp, which is needed for tests.
    (inputs
     (list boost
           akonadi
           akonadi-calendar
           kcalendarcore
           kcrash
           ki18n
           kitemmodels
           kontactinterface
           kparts
           krunner
           kwindowsystem))
    (home-page "https://apps.kde.org/zanshin/")
    (synopsis "TO-do management application")
    (description "Zanshin is a powerful yet simple application for managing
your day to day actions.  It helps you organize and reduce the cognitive
pressure of what one has to do in his job and personal life.")
    (license (list license:gpl3+
                   license:expat))))     ;for mockitopp
