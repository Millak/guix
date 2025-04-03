;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2022, 2024 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2025 Andrew Wong <wongandj@icloud.com>
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

(define-module (gnu packages weather)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system go)
  #:use-module (guix build-system python)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages xml))

(define-public wego
  (package
    (name "wego")
    (version "2.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/schachmat/wego")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "16fx09wf5mll8nbyq5bjd9lbwmq1bqhsvln56jjqpzm28nbjarb0"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/schachmat/wego"))
    (propagated-inputs
     (list go-golang-org-x-sys
           go-github-com-rivo-uniseg
           go-github-com-mattn-go-isatty
           go-github-com-schachmat-ingo
           go-github-com-mattn-go-runewidth
           go-github-com-mattn-go-colorable))
    (home-page "https://github.com/schachmat/wego")
    (synopsis "Weather client for the terminal")
    (description "Wego is a weather client for the terminal.  It shows
forecast for one or seven days.  Displayed information includes temperature
range---felt and measured---, wind speed and direction, viewing distance,
precipitation amount and probability.")
    (license license:isc)))

(define-public meteo-qt
  (package
    (name "meteo-qt")
    (version "4.2")
    (source
     (origin (method git-fetch)
             (uri (git-reference (url "https://github.com/dglent/meteo-qt")
                                 (commit (string-append "v" version))))
             (file-name (git-file-name name version))
             (sha256
              (base32 "1cvmh5rq50dncd2fmp4amjb2hhl2mryb2ywg0zdzhz89dkjq0kdk"))))
    (build-system python-build-system)
    (native-inputs (list python-pyqt-6))
    (propagated-inputs (list python-lxml python-pyqt-6 python-sip))
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-before 'build 'remove-translations
                 (lambda _
                   ;; Translation processing is broken because Guix thinks lprodump
                   ;; is in qtbase, not qttools. So, we'll skip it and exclude
                   ;; qttools from the native input list until it is fixed.
                   (substitute* "setup.py"
                     (("/usr") #$output)
                     (("^.+lrelease-pro-qt6.+$") "")
                     (("^.+meteo_qt/translations.+$") "")))))))
    (home-page "https://github.com/dglent/meteo-qt")
    (synopsis "Weather application for the system tray")
    (description "meteo-qt is an application to display weather information in
desktop panels, desktop notifictions and its own window.  Weather information is
retrieved from OpenWeatherMap.")
    (license license:gpl3)))
