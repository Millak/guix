;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2026 Yarl Baudig <yarl-baudig@mailoo.org>

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

(define-module (gnu home services git-annex-assistant)
  #:use-module (guix gexp)
  #:use-module ((guix packages) #:select (package?))
  #:use-module (guix records)
  #:use-module (gnu services configuration)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu packages haskell-apps)
  #:use-module (srfi srfi-1)
  #:export (home-git-annex-assistant-configuration
            home-git-annex-assistant-service-type))

(define-configuration/no-serialization home-git-annex-assistant-configuration
  (package
    (package git-annex)
    "Package that provides @code{git-annex}.")
  (directories
   (list-of-strings '())
   "List of directories."))

(define (home-git-annex-assistant-shepherd-services config)
  (match-record config
      <home-git-annex-assistant-configuration> (package directories)
    (let ((gaa-command
           #~(list (string-append #$package "/bin/git-annex") "assistant")))
      (map
       (lambda (dir)
         (let ((pid-file (in-vicinity dir ".git/annex/daemon.pid")))
           (shepherd-service
	     (documentation
              (string-append "Run git-annex assistant against " dir "."))
	     (provision
	      (list
	       (symbol-append 'home-git-annex-assistant-
                              (string->symbol dir))))
	     (start #~(lambda _
		        (and (zero? (spawn-command #$gaa-command
					           #:directory #$dir))
			     (read-pid-file #$pid-file))))
	     (stop #~(lambda _
		       (unless (zero? (spawn-command
				       (append #$gaa-command '("--stop"))
				       #:directory #$dir))
		         (error "failed to stop git-annex assistant"))
		       #f)))))
       directories))))

(define home-git-annex-assistant-service-type
  (service-type
    (name 'git-annex-assistant)
    (extensions
     (list (service-extension home-shepherd-service-type
			      home-git-annex-assistant-shepherd-services)))
    (compose concatenate)
    (extend (lambda (config directories)
              (home-git-annex-assistant-configuration
               (inherit config)
               (directories
                (append
                 (home-git-annex-assistant-configuration-directories config)
                 directories)))))
    (description
     "Run the git-annex assistant daemon on a list of directories.")))

