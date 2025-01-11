;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2024-2025 Ludovic Courtès <ludo@gnu.org>
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

;; This manifest computes upgrades of key packages using updaters from (guix
;; upstream) and supporting code for the 'with-latest' transformation.

(use-modules (guix memoization)
             (guix monads)
             (guix graph)
             (guix packages)
             (guix profiles)
             (guix store)
             (guix transformations)
             (guix upstream)
             ((guix scripts build) #:select (dependents))
             ((guix scripts graph) #:select (%bag-node-type))
             ((guix import github) #:select (%github-api))
             (guix build-system gnu)
             (guix build-system cmake)
             ((gnu packages)
              #:select (all-packages specification->package))

             (gnu packages backup)
             (gnu packages crypto)
             (gnu packages curl)
             (gnu packages freedesktop)
             (gnu packages gnupg)
             (gnu packages nettle)
             (gnu packages ssh)
             (gnu packages tls)
             (gnu packages web)
             (gnu packages xorg)

             (ice-9 match)
             (srfi srfi-1))

;; Bypass the GitHub updater: we'd need an API token or we would hit the rate
;; limit.
(%github-api "http://example.org")

(define security-packages
  (list xorg-server
        elogind

        openssl
        gnutls
        curl
        curl-ssh

        ;; Web.
        nghttp2
        nginx

        libarchive
        libssh

        ;; Since there are several libgit2 versions, pick the latest one and
        ;; compute the upgrade against that one.
        (specification->package "libgit2")

        ;; GnuPG.
        libassuan
        libgpg-error
        libgcrypt
        libksba
        npth
        gnupg
        gpgme
        pinentry

        ;; Other crypto libraries.
        nettle
        libsodium))

(define latest-version
  (mlambdaq (package)
    (package-with-upstream-version package
                                   ;; Preserve patches and snippets to get
                                   ;; exactly the same as what we'd have with
                                   ;; 'guix refresh -u PACKAGE'.
                                   #:preserve-patches? #t

                                   ;; XXX: Disable source code authentication:
                                   ;; this requires a local keyring, populated
                                   ;; from key servers, but key servers may be
                                   ;; unreliable or may lack the upstream
                                   ;; keys.  Leave it up to packagers to
                                   ;; actually authenticate code and make sure
                                   ;; it matches what this manifest computed.
                                   #:authenticate? #f)))

(define individual-security-upgrades
  ;; Upgrades of individual packages with their direct dependents built
  ;; against that upgrade.
  (manifest
   (with-store store
     (append-map (lambda (package)
                   (let* ((name (package-name package))
                          (newest (latest-version package))
                          (update (package-input-rewriting
                                   `((,package . ,newest)))))
                     (map (lambda (package)
                            (manifest-entry
                              (inherit (package->manifest-entry
                                        (update package)))
                              (name (string-append (package-name package)
                                                   "-with-latest-" name))))
                          (dependents store (list package) 1))))
                 security-packages))))

(define joint-security-upgrades
  ;; All of SECURITY-PACKAGES updated at once, together with their dependents.
  (manifest
   (with-store store
     (let ((update-all (package-input-rewriting
                        (map (lambda (package)
                               `(,package . ,(latest-version package)))
                             security-packages)
                        #:recursive? #t)))
       (map (lambda (package)
              (manifest-entry
                (inherit (package->manifest-entry
                          (update-all package)))
                (name (string-append (package-name package) "-full-upgrade"))))
            (dependents store security-packages 2))))))

;; Install a UTF-8 locale so that file names in Git checkouts are interpreted
;; as UTF-8 (the libgit2 source tree contains non-ASCII file names, for
;; instance).  XXX: This works around the fact that 'cuirass register' and
;; thus 'cuirass evaluate' may not be running with a UTF-8 locale.
(unless (string-suffix? ".UTF-8" (setlocale LC_ALL))
  (or (false-if-exception (setlocale LC_ALL "C.UTF-8"))
      (false-if-exception (setlocale LC_ALL "en_US.UTF-8"))
      (format (current-error-port) "warning: failed to install UTF-8 locale~%")))

(concatenate-manifests
 (list individual-security-upgrades joint-security-upgrades))
