;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017 Quiliro <quiliro@fsfla.org>
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

(define-module (gnu packages medical)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (guix build-system qt)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (gnu packages bash) ; wrap-program
  #:use-module (gnu packages databases)
  #:use-module (gnu packages kde-frameworks) ; kirigami
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt))

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
    (license gpl3+)))

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
    (license gpl3+)))
