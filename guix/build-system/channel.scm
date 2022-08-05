;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019-2021 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix build-system channel)
  #:use-module ((guix store) #:select (%store-monad))
  #:use-module ((guix gexp) #:select (lower-object))
  #:use-module (guix monads)
  #:use-module (guix channels)
  #:use-module (guix build-system)
  #:export (channel-build-system))

;;; Commentary:
;;;
;;; The "channel" build system lets you build Guix instances from channel
;;; specifications, similar to how 'guix time-machine' would do it, as regular
;;; packages.
;;;
;;; Code:

(define channel-build-system
  ;; Build system used to "convert" a channel instance to a package.
  (let* ((build (lambda* (name inputs
                               #:key source commit system
                               #:allow-other-keys)
                  (mlet* %store-monad ((source (if (string? source)
                                                   (return source)
                                                   (lower-object source)))
                                       (instance
                                        -> (checkout->channel-instance
                                            source #:commit commit)))
                    (channel-instances->derivation (list instance)))))
         (lower (lambda* (name #:key system source commit
                               #:allow-other-keys)
                  (bag
                    (name name)
                    (system system)
                    (build build)
                    (arguments `(#:source ,source
                                 #:commit ,commit))))))
    (build-system (name 'channel)
                  (description "Turn a channel instance into a package.")
                  (lower lower))))

