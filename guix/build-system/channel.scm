;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019-2022 Ludovic Courtès <ludo@gnu.org>
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
  #:use-module ((guix store) #:select (%store-monad store-lift))
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

(define latest-channel-instances*
  (store-lift latest-channel-instances))

(define* (build-channels name inputs
                         #:key source system commit
                         (authenticate? #t)
                         #:allow-other-keys)
  (mlet* %store-monad ((instances
                        (cond ((channel-instance? source)
                               (return (list source)))
                              ((channel? source)
                               (latest-channel-instances*
                                (list source)
                                #:authenticate? authenticate?))
                              ((string? source)
                               ;; If SOURCE is a store file name, as is the
                               ;; case when called from (gnu ci), return it as
                               ;; is.
                               (return
                                (list (checkout->channel-instance
                                       source #:commit commit))))
                              (else
                               (mlet %store-monad ((source
                                                    (lower-object source)))
                                 (return
                                  (list (checkout->channel-instance
                                         source #:commit commit))))))))
    (channel-instances->derivation instances)))

(define channel-build-system
  ;; Build system used to "convert" a channel instance to a package.
  (let ((lower (lambda* (name #:key system source commit (authenticate? #t)
                              #:allow-other-keys)
                 (bag
                   (name name)
                   (system system)
                   (build build-channels)
                   (arguments `(#:source ,source
                                #:authenticate? ,authenticate?
                                #:commit ,commit))))))
    (build-system (name 'channel)
                  (description "Turn a channel instance into a package.")
                  (lower lower))))

