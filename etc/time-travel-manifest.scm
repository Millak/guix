;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2022 Ludovic Courtès <ludo@gnu.org>
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

;;; This file returns a manifest containing entries to build past Guix
;;; releases from the current Guix, as per 'guix time-machine'.

(use-modules (srfi srfi-9) (ice-9 match)
             (guix channels) (guix gexp)
             ((guix store) #:select (%store-monad))
             ((guix monads) #:select (mparameterize return))
             ((guix git) #:select (%repository-cache-directory))
             ((guix build utils) #:select (mkdir-p)))

;; Representation of the latest channels.  This type exists just so we can
;; refer to such records in a gexp.
(define-record-type <guix-instance>
  (guix-instance channels)
  guix-instance?
  (channels guix-instance-channels))

(define-gexp-compiler (guix-instance-compiler (instance <guix-instance>)
                                              system target)
  (match instance
    (($ <guix-instance> channels)
     ;; When this manifest is evaluated by Cuirass, make sure it does not
     ;; fiddle with the cached checkout that Cuirass is also using since
     ;; concurrent accesses are unsafe.
     (mparameterize %store-monad ((%repository-cache-directory
                                   (string-append (%repository-cache-directory)
                                                  "/time-travel/" system)))
       (return (mkdir-p (%repository-cache-directory)))
       (latest-channel-derivation channels)))))

(define (guix-instance->manifest-entry instance)
  "Return a manifest entry for INSTANCE."
  (define (shorten commit)
    (string-take commit 7))

  (manifest-entry
    (name "guix")
    (version (string-join (map (compose shorten channel-commit)
                               (guix-instance-channels instance))
                          "-"))
    (item instance)))

(define (commit->guix-instance commit)
  "Return a Guix instance for COMMIT."
  (guix-instance (list (channel
                        (inherit %default-guix-channel)
                        (commit commit)))))

(define %release-commits
  ;; Release commits: the list of version/commit pairs.
  '(("1.3.0" . "a0178d34f582b50e9bdbb0403943129ae5b560ff")
    ("1.2.0" . "a099685659b4bfa6b3218f84953cbb7ff9e88063")
    ("1.1.0" . "d62c9b2671be55ae0305bebfda17b595f33797f2")
    ("1.0.1" . "d68de958b60426798ed62797ff7c96c327a672ac")
    ("1.0.0" . "6298c3ffd9654d3231a6f25390b056483e8f407c")
    ("0.16.0" . "4a0b87f0ec5b6c2dcf82b372dd20ca7ea6acdd9c")))

(manifest
 (map (match-lambda
        ((version . commit)
         (let ((entry (guix-instance->manifest-entry
                       (commit->guix-instance commit))))
           (manifest-entry
             (inherit entry)
             (version version)))))
      %release-commits))
