;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2022, 2024 Janneke Nieuwenhuizen <janneke@gnu.org>
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

;; GNU Guix development manifest.  To create development environment, run
;;
;;     guix shell
;;
;; or something like
;;
;;     guix shell --pure -m manifest.scm hello ...

(use-modules (guix packages))

(concatenate-manifests
 (list (package->development-manifest
        (let ((guix (specification->package "guix")))
          (package/inherit guix
            ;; Replace with non-minimal Graphviz for PDF support.
            (native-inputs (modify-inputs (package-native-inputs guix)
                             (replace "graphviz"
                               (specification->package "graphviz")))))))

       ;; Extra packages used by unit tests.
       (specifications->manifest (list "gnupg"))

       ;; Packages needed for 'make dist' and 'make distcheck'.
       (specifications->manifest
        (list "imagemagick"
              "perl"))

       ;; Useful extras for patches submission.
       (specifications->manifest
        (list "b4"
              "git"
              "git:send-email"
              "mumi"
              "nss-certs"
              "openssl"              ;required if using 'smtpEncryption = tls'
              "patman"))))
