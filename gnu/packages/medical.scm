;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017 Quiliro <quiliro@fsfla.org>
;;; Copyright © 2022 Petr Hodina <phodina@protonmail.com>
;;; Copyright © 2025 Jake Forster <jakecameron.forster@gmail.com>
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

(define-module (gnu packages medical)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (guix build-system qt)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix hg-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash) ; wrap-program
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages image-processing)
  #:use-module (gnu packages kde-frameworks) ; kirigami
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml))

(define-public mygnuhealth
  (package
    (name "mygnuhealth")
    (version "2.2.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "MyGNUHealth" version))
              (sha256
               (base32
                "1jcrriccqzb4jx7zayhiqmpvi3cvfy3bbf9zr3m83878f94yww8j"))))
    (build-system python-build-system)
    (arguments
     (list
      #:imported-modules `(,@%python-build-system-modules
                           ,@%qt-build-system-modules)
      #:modules `(((guix build qt-build-system) #:prefix qt:)
                  (guix build python-build-system)
                  (guix build utils))
      #:phases #~(modify-phases %standard-phases
                   (add-after 'install 'qt-wrap
                     (assoc-ref qt:%standard-phases 'qt-wrap))
                   (add-before 'check 'env-setup
                     (lambda _
                       (mkdir-p "/tmp/mygh/")
                       (setenv "HOME" "/tmp"))))))
    (native-inputs (list python-pyside-2))
    (inputs (list bash-minimal
                  kirigami-5
                  python
                  python-bcrypt
                  python-matplotlib
                  python-requests
                  python-tinydb
                  qtbase-5
                  qtdeclarative-5
                  qtgraphicaleffects
                  qtquickcontrols-5
                  qtquickcontrols2-5
                  qtsvg-5))
    (home-page "https://www.gnuhealth.org")
    (synopsis "The GNU Health Personal Health Record")
    (description
     "This package provides GNUHealth Personal Health Record
application for desktop and mobile devices that integrates with the GNU
Health Federation.")
    (license license:gpl3+)))

(define-public openmolar-1
  (package
    (name "openmolar")
    (version "1.1.6-g81838c85")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://static.openmolar.com/om1/releases/openmolar-" version
             ".tar.gz"))
       (sha256
        (base32 "09vrfqn511vswnj2q9m7srlwdgz066qvqpmja6sg1yl1ibh3cbpr"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-/usr
            (lambda* (#:key outputs #:allow-other-keys)
              (substitute* '("setup.py"
                             "src/openmolar/settings/localsettings.py")
                (("/usr") #$output))))
          (add-after 'unpack 'set-acceptable-version
            (lambda _
              (substitute* "src/openmolar/settings/version.py"
                ((#$version) "1.1.6")))))))
    (native-inputs
     (list python-setuptools python-wheel))
    (inputs (list python-pyqtwebengine
                  python-pyqt+qscintilla
                  python-mysqlclient
                  qscintilla))
    (propagated-inputs (list qtwebengine-5))
    (home-page "https://openmolar.com/om1")
    (synopsis "Dental practice management software")
    (description
     "Openmolar is a dental practice management suite.  Its
functionality includes appointments, patient records, treatment planning,
billing etc.  It is a full featured, reliable and thoroughly tested
application and has been translated into many languages.")
    (license license:gpl3+)))

(define-public orthanc
  (package
    (name "orthanc")
    (version "1.12.7")
    (source
     (origin
       (method hg-fetch)
       (uri (hg-reference (url "https://orthanc.uclouvain.be/hg/orthanc")
                          (changeset "a30cc2fa7250")))
       (file-name (hg-file-name name version))
       (sha256
        (base32 "1n8v0dh17g9ihny8cpq63qd6ai5l95i5h3sz9vwzfnd7q3qh0rb9"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list (string-append "-DDCMTK_DICTIONARY_DIR="
                             #$(this-package-input "dcmtk") "/share/dcmtk-"
                             #$(package-version (this-package-input "dcmtk")))
              ;; Building Connectivity Checks plugin requires the network.
              "-DBUILD_CONNECTIVITY_CHECKS=OFF"
              ;; Disable 4 tests that require the network.
              "-DUNIT_TESTS_WITH_HTTP_CONNEXIONS=OFF")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'chdir
            (lambda _
              ;; The OrthancServer/ subdirectory contains the root
              ;; CMakeLists.txt file.
              (chdir "OrthancServer")))
          (add-after 'unpack 'remove-localtime-requirement
            (lambda _
              ;; /etc/localtime is not present in the build container;
              ;; don't let that stop the build.
              (substitute* "OrthancFramework/Sources/Toolbox.cpp"
                (("if \\(!SystemToolbox::IsExistingFile\\(LOCALTIME\\)\\)")
                 "if (false)"))))
          ;; There is no test target; simply run the binary.
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "./UnitTests")))))))
    (native-inputs (list doxygen
                         glibc-utf8-locales ;for one test
                         googletest
                         python))
    (inputs (list boost
                  civetweb
                  curl
                  dcmtk
                  jsoncpp
                  libjpeg-turbo
                  libpng
                  lua
                  openssl
                  protobuf
                  pugixml
                  `(,util-linux "lib")  ;for uuid.h
                  sqlite
                  unzip))
    (home-page "https://orthanc-server.com")
    (synopsis
     "Standalone @acronym{DICOM, Digital Imaging and Communications in
Medicine} server")
    (description
     "Orthanc is a standalone @acronym{DICOM, Digital Imaging and
Communications in Medicine} server with a RESTful API and plugin
mechanism for extending its functionality.")
    (license license:gpl3+)))

(define-public orthanc-postgresql
  ;; No tags provided.
  (let ((changeset "ddc98844d931")
        (revision "0"))
    (package
      (name "orthanc-postgresql")
      (version (git-version "7.2" revision changeset))
      (source
       (origin
         (method hg-fetch)
         (uri (hg-reference
                (url "https://orthanc.uclouvain.be/hg/orthanc-databases")
                (changeset changeset)))
         (file-name (hg-file-name name version))
         (sha256
          (base32 "1zv2lrj0f3fahmdyiwyj70ayv5ysa3cj33i58fbk7hy2zclrkjzf"))))
      (build-system cmake-build-system)
      (arguments
       (list
        #:configure-flags
        #~(list
           ;; Don't try to download the Orthanc source code.
           "-DORTHANC_FRAMEWORK_SOURCE=path"
           (string-append "-DORTHANC_FRAMEWORK_ROOT="
                          #$(package-source (this-package-input "orthanc"))
                          "/OrthancFramework/Sources"))
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'chdir
              (lambda _
                ;; The PostgreSQL/ subdirectory contains the root
                ;; CMakeLists.txt file.
                (chdir "PostgreSQL")))
            ;; There is no test target; simply run the binary.
            (replace 'check
              (lambda* (#:key tests? #:allow-other-keys)
                (when tests?
                  ;; The tests rely on a PostgreSQL test server
                  ;; being available.
                  (let ((pgdata "/tmp/pgdata")
                        (host "/tmp")
                        (port "5432")
                        ;; This username is hard-coded in tests.
                        (username "postgres")
                        (password "")
                        (database "orthanctest"))
                    (invoke "initdb" pgdata)
                    (invoke "pg_ctl" "-D" pgdata
                            "-o" (string-append "-k " host)
                            "start")
                    (invoke "createdb" "-h" host database)
                    (invoke "createuser" "-h" host username)
                    ;; The tests include managing Large Objects.
                    (invoke "psql" "-h" host "-d" database "-c"
                            (string-append "ALTER ROLE " username
                                           " WITH SUPERUSER;"))
                    (invoke "./UnitTests" host port username password
                            database))))))))
      (native-inputs
       (list googletest
             python))
      (inputs
       (list boost
             jsoncpp
             openssl
             orthanc
             postgresql
             protobuf
             unzip
             (list util-linux "lib") ; <uuid.h>
             zlib))
      (home-page "https://orthanc-server.com")
      (synopsis "PostgreSQL plugins for Orthanc")
      (description
       "This package provides plugins to use PostgreSQL as the database
backend of an Orthanc @acronym{DICOM, Digital Imaging and Communications in
Medicine} server instead of SQLite.")
      (license license:agpl3+))))

(define-public orthanc-mysql
  ;; No tags provided.
  (let ((changeset "a6300ccf6683")
        (revision "0"))
    (package
      (name "orthanc-mysql")
      (version (git-version "5.2" revision changeset))
      (source
       (origin
         (method hg-fetch)
         (uri (hg-reference
                (url "https://orthanc.uclouvain.be/hg/orthanc-databases")
                (changeset changeset)))
         (file-name (hg-file-name name version))
         (sha256
          (base32 "0hi3bawj117lk7lrxpa6h7vq4hx9jwqb3m5pwn3726hnf4kmp4jc"))))
      (build-system cmake-build-system)
      (arguments
       (list
        #:configure-flags
        #~(list
           ;; Don't try to download the Orthanc source code.
           "-DORTHANC_FRAMEWORK_SOURCE=path"
           (string-append "-DORTHANC_FRAMEWORK_ROOT="
                          #$(package-source (this-package-input "orthanc"))
                          "/OrthancFramework/Sources"))
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'chdir
              (lambda _
                ;; The MySQL/ subdirectory contains the root CMakeLists.txt
                ;; file.
                (chdir "MySQL")))
            (add-after 'unpack 'fix-mysql-include-path
              (lambda* (#:key inputs #:allow-other-keys)
                ;; Help it find mysql.h, either from mysql in this package or
                ;; from mariadb:dev in orthanc-mariadb.
                (substitute* "Resources/CMake/MariaDBConfiguration.cmake"
                  (("/usr/include/mysql")
                   (search-input-directory inputs "include/mysql")))))
            ;; There is no test target; simply run the binary.
            (replace 'check
              (lambda* (#:key tests? #:allow-other-keys)
                (when tests?
                  ;; The tests rely on a MySQL or MariaDB test server
                  ;; being available.
                  (let ((datadir "/tmp/mysql")
                        (socket "/tmp/mysql/mysqld.sock")
                        (username "root")
                        (password "")
                        (database "orthanctest"))
                    (invoke "mysqld" "--initialize-insecure"
                            (string-append "--datadir=" datadir))
                    (spawn "mysqld"
                           (list
                            "mysqld"
                            ;; Respect '--datadir'.
                            "--no-defaults"
                            (string-append "--datadir=" datadir)
                            (string-append "--socket=" socket)
                            ;; For one test.
                            "--max_allowed_packet=32m"))
                    (sleep 1)
                    (invoke "mysql"
                            (string-append "--socket=" socket)
                            "-u" username
                            "-e" "CREATE DATABASE orthanctest;")
                    (invoke "./UnitTests" socket username password
                            database))))))))
      (native-inputs
       (list googletest
             python))
      (inputs
       (list boost
             curl
             jsoncpp
             mysql
             openssl
             orthanc
             protobuf
             unzip
             (list util-linux "lib") ; <uuid.h>
             zlib))
      (home-page "https://orthanc-server.com")
      (synopsis "MySQL plugins for Orthanc")
      (description
       "This package provides plugins to use MySQL or MariaDB as the database
backend of an Orthanc @acronym{DICOM, Digital Imaging and Communications in
Medicine} server instead of SQLite.")
      (license license:agpl3+))))

;; Provide this variant without MySQL to save MariaDB users ~340 MiB
;; of disk space.
(define-public orthanc-mariadb
  (package/inherit orthanc-mysql
    (name "orthanc-mariadb")
    (arguments
     (substitute-keyword-arguments (package-arguments orthanc-mysql)
       ((#:phases phases)
        #~(modify-phases #$phases
            (replace 'check
              (lambda* (#:key tests? #:allow-other-keys)
                (when tests?
                  ;; The tests rely on a MySQL or MariaDB test server
                  ;; being available.
                  (let ((datadir "/tmp/mysql")
                        (socket "/tmp/mysql/mysqld.sock")
                        (username "root")
                        (password "")
                        (database "orthanctest"))
                    (invoke "mariadb-install-db"
                            (string-append "--datadir=" datadir)
                            "--auth-root-authentication-method=normal")
                    (spawn "mysqld"
                           (list
                            "mysqld"
                            ;; Respect '--datadir'.
                            "--no-defaults"
                            (string-append "--datadir=" datadir)
                            (string-append "--socket=" socket)
                            ;; For one test.
                            "--max-allowed-packet=32m"))
                    (sleep 1)
                    (invoke "mysql"
                            (string-append "--socket=" socket)
                            "-u" username
                            "-e" "CREATE DATABASE orthanctest;")
                    (invoke "./UnitTests" socket username password
                            database)))))))))
    (native-inputs (modify-inputs (package-native-inputs orthanc-mysql)
                     (prepend mariadb))) ;for tests
    (inputs (modify-inputs (package-inputs orthanc-mysql)
              (delete "mysql")
              (prepend `(,mariadb "dev")
                       `(,mariadb "lib"))))
    (synopsis "MariaDB plugins for Orthanc")))

(define-public xmedcon
  (package
    (name "xmedcon")
    (version "0.25.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "git://git.code.sf.net/p/xmedcon/code")
             (commit (string-append "Version-"
                                    (string-replace-substring version "." "_")))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1rvh1i967s87d8vpm5fgz07c9vnvwbh3cdphwp806jcvw93y90kw"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list "--disable-static"
              (string-append "--with-nifti-prefix="
                             #$(this-package-input "niftilib")))))
    (inputs (list glib gtk+ libpng niftilib))
    (native-inputs (list pkg-config))
    (home-page "https://xmedcon.sourceforge.io")
    (synopsis "Medical image conversion utility")
    (description
     "@acronym{(X)MedCon, Medical Image Conversion} can be used to convert
between different file formats used in medical imaging.  The supported formats
are @acronym{ACR, American College of Radiology}-@acronym{NEMA, National
Electrical Manufacturers Association} 2.0, Analyze (@acronym{SPM, Statistical
Parametric Mapping}), Concorde/μPET, @acronym{DICOM, Digital Imaging and
Communications in Medicine} 3.0, CTI ECAT 6/7, @acronym{NIfTI, Neuroimaging
Informatics Technology Initiative}-1, Interfile 3.3, @acronym{PNG, Portable
Network Graphics}, and @acronym{GIF, Graphics Interchange Format}87a/89a.
This package includes a command-line utility (@code{medcon}) and a GTK+ based
GUI application (@code{xmedcon}).")
    (license license:lgpl2.0+)))
