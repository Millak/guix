;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2023 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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

(define-module (tests services vpn)
  #:use-module (gnu packages vpn)
  #:use-module (gnu services vpn)
  #:use-module (guix gexp)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64))

;;; Commentary:
;;;
;;; Unit tests for the (gnu services vpn) module.
;;;
;;; Code:

;;; Access some internals for whitebox testing.
(define ipv4-address? (@@ (gnu services vpn) ipv4-address?))
(define ipv6-address? (@@ (gnu services vpn) ipv6-address?))
(define host-name? (@@ (gnu services vpn) host-name?))
(define endpoint-host-names
  (@@ (gnu services vpn) endpoint-host-names))

(test-begin "vpn-services")

(test-assert "ipv4-address?"
  (every ipv4-address?
         (list "192.95.5.67:1234"
               "10.0.0.1")))

(test-assert "ipv6-address?"
  (every ipv6-address?
         (list "[2001:db8::c05f:543]:2468"
               "2001:db8::c05f:543"
               "2001:db8:855b:0000:0000:0567:5673:23b5"
               "2001:db8:855b::0567:5673:23b5")))

(define %wireguard-peers
  (list (wireguard-peer
         (name "dummy1")
         (public-key "VlesLiEB5BFd//OD2ILKXviolfz+hodG6uZ+XjoalC8=")
         (endpoint "some.dynamic-dns.service:53281")
         (allowed-ips '()))
        (wireguard-peer
         (name "dummy2")
         (public-key "AlesLiEB5BFd//OD2ILKXviolfz+hodG6uZ+XgoalC9=")
         (endpoint "example.org")
         (allowed-ips '()))
        (wireguard-peer
         (name "dummy3")
         (public-key "BlesLiEB5BFd//OD2ILKXviolfz+hodG6uZ+XgoalC7=")
         (endpoint "10.0.0.7:7777")
         (allowed-ips '()))
        (wireguard-peer
         (name "dummy4")
         (public-key "ClesLiEB5BFd//OD2ILKXviolfz+hodG6uZ+XgoalC6=")
         (endpoint "[2345:0425:2CA1::0567:5673:23b5]:44444")
         (allowed-ips '()))))

(test-equal "endpoint-host-names"
  ;; The first element of the pair the public Wireguard key associated to a
  ;; host name.
  '(("VlesLiEB5BFd//OD2ILKXviolfz+hodG6uZ+XjoalC8=" .
     "some.dynamic-dns.service:53281")
    ("AlesLiEB5BFd//OD2ILKXviolfz+hodG6uZ+XgoalC9=" .
     "example.org"))
  (endpoint-host-names %wireguard-peers))

(test-end "vpn-services")
