;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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

(define-module (tests services telephony)
  #:use-module (gnu build jami-service)
  #:use-module (gnu services telephony)
  #:use-module (srfi srfi-64))

;;; Tests for the (gnu services telephony) and related modules.

(test-begin "jami-service")

(define jami-account->alist
  (@@ (gnu services telephony) jami-account->alist))

(define %dummy-jami-account (jami-account
                             (archive "/tmp/dummy.gz")))

(define %dummy-jami-account-2 (jami-account
                               (archive "/tmp/dummy.gz")
                               (rendezvous-point? #t)
                               (peer-discovery? #f)
                               (bootstrap-hostnames '("bootstrap.me"
                                                      "fallback.another.host"))
                               (name-server-uri "https://my.name.server")))

(test-equal "jami-account->alist, no account detail value set"
  '()
  (jami-account->alist %dummy-jami-account))

(test-equal "jami-account->alist, with account detail values"
  '(("Account.hostname" . "bootstrap.me;fallback.another.host")
    ("Account.peerDiscovery" . "false")
    ("Account.rendezVous" . "true")
    ("RingNS.uri" . "https://my.name.server"))
  (sort (jami-account->alist %dummy-jami-account-2)
        (lambda (x y)
          (string<=? (car x) (car y)))))

(test-end)
