;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2020, 2023 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2020 Martin Becze <mjbecze@riseup.net>
;;; Copyright © 2021 Xinglu Chen <public@yoctocell.xyz>
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

(define-module (test-elpa)
  #:use-module (guix import elpa)
  #:use-module (guix upstream)
  #:use-module ((guix download) #:select (url-fetch))
  #:use-module (guix tests)
  #:use-module (guix tests http)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 match)
  #:use-module (web client))

(define elpa-mock-archive
  '(1
    (ace-window .
                [(0 9 0)
                 ((avy
                   (0 2 0)))
                 "Quickly switch windows." single
                 ((:url . "https://github.com/abo-abo/ace-window")
                  (:keywords "window" "location"))])
    (auctex .
            [(11 88 6)
             nil "Integrated environment for *TeX*" tar
             ((:url . "http://www.gnu.org/software/auctex/"))])
    (fake-taxy-magit-section .
		             [(0 12 2)
		              ((emacs
			        (26 3))
		               (magit-section
			        (3 2 1))
		               (taxy
			        (0 10)))
		              "View Taxy structs in a Magit Section buffer" tar
		              ((:url . "https://github.com/alphapapa/taxy.el")
		               (:keywords "lisp"))])))


(test-begin "elpa")

(define (eval-test-with-elpa pkg)
  ;; Set up an HTTP server and use it as a pseudo-proxy so that
  ;; 'elpa->guix-package' talks to it.
  (with-http-server `((200 ,(object->string elpa-mock-archive))
                      (200 "This is the description.")
                      (200 "fake tarball contents"))
    (parameterize ((current-http-proxy (%local-url)))
      (match (elpa->guix-package pkg #:repo 'gnu/http)
        (`(package
            (name "emacs-auctex")
            (version "11.88.6")
            (source
             (origin
               (method url-fetch)
               (uri (string-append
                     "http://elpa.gnu.org/packages/auctex-" version ".tar"))
               (sha256 (base32 ,(? string? hash)))))
            (build-system emacs-build-system)
            (home-page "http://www.gnu.org/software/auctex/")
            (synopsis "Integrated environment for *TeX*")
            (description "This is the description.")
            (license license:gpl3+))
         #t)
        (x
         (pk 'fail x #f))))))

(test-assert "elpa->guix-package test 1"
  (eval-test-with-elpa "auctex"))

(test-equal "package-latest-release"
  (list '("http://elpa.gnu.org/packages/fake-taxy-magit-section-0.12.2.tar")
        '("http://elpa.gnu.org/packages/fake-taxy-magit-section-0.12.2.tar.sig")
        (list (upstream-input
               (name "magit-section")
               (downstream-name "emacs-magit-section")
               (type 'propagated)
               (min-version "3.2.1")
               (max-version min-version))
              (upstream-input
               (name "taxy")
               (downstream-name "emacs-taxy")
               (type 'propagated)
               (min-version "0.10")
               (max-version #f))))
  (with-http-server `((200 ,(object->string elpa-mock-archive)))
    (parameterize ((current-http-proxy (%local-url)))
      (define source
        ;; Note: Use 'http' URLs to the proxy is used.
        (package-latest-release
         (dummy-package "emacs-fake-taxy-magit-section"
                        (version "0.0.0")
                        (source (dummy-origin
                                 (method url-fetch)
                                 (uri "http://elpa.gnu.org/xyz"))))
         (list %elpa-updater)))

      (list (upstream-source-urls source)
            (upstream-source-signature-urls source)
            (upstream-source-inputs source)))))

(test-equal "guix-package->elpa-name: without 'upstream-name' property"
  "auctex"
  (guix-package->elpa-name (dummy-package "emacs-auctex")))

(test-equal "guix-package->elpa-name: with 'upstream-name' property"
  "project"
  (guix-package->elpa-name
   (dummy-package "emacs-fake-name"
     (properties '((upstream-name . "project"))))))

(test-end "elpa")

;; Local Variables:
;; eval: (put 'with-http-server 'scheme-indent-function 1)
;; End:
