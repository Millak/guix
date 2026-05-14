;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2026 Nicolas Graves <ngraves@ngraves.fr>
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

(define-module (gnu services jupyter)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages jupyter)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:export (jupyter-service-type
            jupyter-configuration))

(define list-of-file-likes? (list-of file-like?))

(define-configuration/no-serialization jupyter-configuration
  (python-jupyter-core
   (file-like python-jupyter-core)
   "Package providing the @command{jupyter} binary.")
  (extensions
   (list-of-file-likes (list))
   "List of packages providing server extensions to Jupyter.")
  (kernels
   (list-of-file-likes (list))
   "Kernel packages to add to the profile, e.g. for python @code{python-ipykernel}.")
  (disable-check-xsrf?
   (boolean #f)
   "Whether to disable the XSRF check.")
  (token
   (string "MY_TOKEN")
   "Authentication token for the Jupyter server."))

(define (jupyter-profile-service config)
  (match-record config <jupyter-configuration>
                (python-jupyter-core extensions kernels)
    (append (list python-jupyter-core) extensions kernels)))

(define (jupyter-server-runtime-config-getter-procedure python-jupyter-core
                                                        field)
  (with-extensions (list guile-json-4)
    #~(begin
        (use-modules (json))
        (lambda (process . args)
          (let* ((pid (process-id process))
                 (port
                  ((@ (ice-9 popen) open-input-pipe)
                   (string-append #$(file-append python-jupyter-core
                                                 "/bin/jupyter")
                                  " --runtime-dir")))
                 (dir (read port))
                 (file (format #f "~a/jpserver-~a.json" dir pid)))
            ((@ (ice-9 popen) close-pipe) port)
            (call-with-input-file file
              (lambda (port)
                (let ((url (assoc-ref (json->scm port) #$field)))
                  (format #t "~a~%" url)))))))))

(define (jupyter-shepherd-service config)
  (match-record config <jupyter-configuration>
                (python-jupyter-core disable-check-xsrf? token)
    (list
     (shepherd-service
       (provision '(jupyter))
       (documentation "Run a Jupyter server.")
       (start
        #~(make-forkexec-constructor
           (list #$(file-append python-jupyter-core "/bin/jupyter")
                 "server" "--no-browser"
                 #$(string-append "--IdentityProvider.token=" token)
                 #$@(if disable-check-xsrf?
                        #~("--ServerApp.disable_check_xsrf=True")
                        #~()))
           #:log-file "/var/log/jupyter.log"))
       (stop #~(make-kill-destructor))
       (respawn? #f)
       (actions
        (list
         (shepherd-action
           (name 'get-url)
           (documentation "Print the Jupyter server URL.")
           (procedure
            (jupyter-server-runtime-config-getter-procedure
             python-jupyter-core "url")))))))))

(define jupyter-service-type
  (service-type
    (name 'jupyter)
    (extensions
     (list
      (service-extension profile-service-type
                         jupyter-profile-service)
      (service-extension shepherd-root-service-type
                         jupyter-shepherd-service)))
    (default-value (jupyter-configuration))
    (description
     "Run a Jupyter server.  The runtime file is reliably at
@file{$(jupyter --runtime-dir)/jpserver-@var{pid}.json}.  Provide the
@code{get-url} function to return a valid URL to fetch from.")))
