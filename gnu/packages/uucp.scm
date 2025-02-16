;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2021 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2025 Artyom V. Poptsov <poptsov.artyom@gmail.com>
;;; Copyright © 2025 Sharlatan Hellseher <sharlatanus@gmail.com>
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

(define-module (gnu packages uucp)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-compression)
  #:use-module (gnu packages golang-crypto)
  #:use-module (gnu packages golang-web)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages texinfo)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go))

(define-public uucp
  (package
    (name "uucp")
    (version "1.07")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/uucp/uucp-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0b5nhl9vvif1w3wdipjsk8ckw49jj1w85xw1mmqi3zbcpazia306"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             ;; The old 'configure' script doesn't support the arguments
             ;; that we pass by default.
             (setenv "CONFIG_SHELL" (which "sh"))
             (let ((out (assoc-ref outputs "out")))
               (invoke "./configure"
                       (string-append "--prefix=" out)
                       (string-append "--infodir=" out
                                      "/share/info"))))))))
    (home-page "https://www.gnu.org/software/uucp/uucp.html")
    (synopsis "UUCP protocol implementation")
    (description
     "Taylor UUCP is the GNU implementation of UUCP (Unix-to-Unix Copy), a
set of utilities for remotely transferring files, email and net news
between computers.")
    (license gpl2+)))

(define-public nncp
  (package
    (name "nncp")
    (version "8.11.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://www.nncpgo.org/download/nncp-"
                           version ".tar.xz"))
       (sha256
        (base32
         "1wmg6k1nprk9b7vnnly3m6xxyma2l0xamnrq3xwahjhqv6y18hgc"))
       (modules '((ice-9 ftw)
                  (guix build utils)))
       (snippet
        #~(begin
            ;; TODO: Find out how to obtain sources for go.cypherpunks.ru and
            ;; go.cypherpunks.su.
            (with-directory-excursion "src/vendor"
              (for-each delete-file-recursively
                        (list "github.com"
                              "go.uber.org"
                              "golang.org"
                              "gvisor.dev"
                              "lukechampine.com")))
            ;; The hack to trick go-build-system.
            (rename-file "src" "v8")))))
    (build-system go-build-system)
    (arguments
     (list
      #:install-source? #f
      #:import-path "go.cypherpunks.su/nncp/v8"
      #:unpack-path "go.cypherpunks.su/nncp"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-paths
            (lambda* (#:key import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (substitute* (list "pipe.go" "toss_test.go")
                  (("/bin/sh") (which "sh"))
                  (("; cat") (format #f "; ~a" (which "cat")))))))
          (replace 'build
            (lambda arguments
              (let ((path-prefix "go.cypherpunks.su/nncp/v8/cmd/"))
                (for-each
                 (lambda (cmd)
                   (apply (assoc-ref %standard-phases 'build)
                          `(,@arguments #:import-path
                            ,(string-append path-prefix cmd))))
                 (list "nncp-ack"
                       "nncp-bundle"
                       "nncp-call"
                       "nncp-caller"
                       "nncp-cfgdir"
                       "nncp-cfgenc"
                       "nncp-cfgmin"
                       "nncp-cfgnew"
                       "nncp-check"
                       "nncp-cronexpr"
                       "nncp-daemon"
                       "nncp-exec"
                       "nncp-file"
                       "nncp-freq"
                       "nncp-hash"
                       "nncp-log"
                       "nncp-pkt"
                       "nncp-reass"
                       "nncp-rm"
                       "nncp-stat"
                       "nncp-toss"
                       "nncp-trns"
                       "nncp-xfer"))))))))
    (inputs
     (list go-github-com-arceliar-ironwood
           go-github-com-davecgh-go-xdr
           go-github-com-dustin-go-humanize
           go-github-com-flynn-noise
           go-github-com-fsnotify-fsnotify
           go-github-com-gologme-log
           go-github-com-gorhill-cronexpr
           go-github-com-hjson-hjson-go-v4
           go-github-com-klauspost-compress
           go-github-com-yggdrasil-network-yggdrasil-go
           ;; go-go-cypherpunks-ru-balloon    ; sourcing from vendor
           ;; go-go-cypherpunks-su-recfile-v2 ; sourcing from vendor
           go-golang-org-x-crypto
           go-golang-org-x-net
           go-golang-org-x-sys
           go-golang-org-x-term
           go-gvisor-dev-gvisor
           go-lukechampine-com-blake3))
    (native-inputs
     (list texinfo))
    (home-page "http://www.nncpgo.org/")
    (synopsis "Store and forward utilities")
    (description "NNCP (Node to Node copy) is a collection of utilities
simplifying secure store-and-forward files, mail and command exchanging.
These utilities are intended to help build up small size (dozens of nodes)
ad-hoc friend-to-friend (F2F) statically routed darknet delay-tolerant
networks for fire-and-forget secure reliable files, file requests, Internet
mail and commands transmission.  All packets are integrity checked, end-to-end
encrypted, explicitly authenticated by known participants public keys.  Onion
encryption is applied to relayed packets.  Each node acts both as a client and
server, can use push and poll behaviour model.  Multicasting areas, offline
sneakernet/floppynet, dead drops, sequential and append-only CD-ROM/tape
storages, air-gapped computers and online TCP daemon with full-duplex
resumable data transmission exists are all supported.")
    (license gpl3)))
