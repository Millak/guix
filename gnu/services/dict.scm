;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2016, 2017, 2018, 2020, 2022, 2023 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2017 Huang Ying <huang.ying.caritas@gmail.com>
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

(define-module (gnu services dict)
  #:use-module (guix deprecation)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (guix modules)
  #:use-module (guix least-authority)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system shadow)
  #:use-module ((gnu packages admin) #:select (shadow))
  #:use-module (gnu packages dico)
  #:use-module (gnu packages dictionaries)
  #:autoload   (gnu build linux-container) (%namespaces)
  #:autoload   (gnu system file-systems) (file-system-mapping)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:export (dicod-service  ; deprecated
            dicod-service-type
            dicod-configuration
            dicod-handler
            dicod-database
            %dicod-database:gcide))


;;;
;;; GNU Dico.
;;;

(define-record-type* <dicod-configuration>
  dicod-configuration make-dicod-configuration
  dicod-configuration?
  (dico        dicod-configuration-dico       (default dico))
  (interfaces  dicod-configuration-interfaces     ;list of strings
               (default '("localhost")))
  (handlers    dicod-configuration-handlers       ;list of <dicod-handler>
               (default '()))
  (databases   dicod-configuration-databases      ;list of <dicod-database>
               (default (list %dicod-database:gcide)))
  (home-service? dicod-configuration-home-service? ;boolean
                 (default for-home?) (innate)))

(define-record-type* <dicod-handler>
  dicod-handler make-dicod-handler
  dicod-handler?
  (name        dicod-handler-name)
  (module      dicod-handler-module          (default #f))
  (options     dicod-handler-options         (default '())))

(define-record-type* <dicod-database>
  dicod-database make-dicod-database
  dicod-database?
  (name        dicod-database-name)
  (handler     dicod-database-handler)
  (complex?    dicod-database-complex?       (default #f))
  (options     dicod-database-options        (default '())))

(define %dicod-gcide-index
  ;; The GCIDE pre-built index.  The Dico 'gcide' module can build it lazily;
  ;; do it upfront so there's no need for a writable directory at run-time.
  (computed-file "dicod-gcide-index"
                 (with-imported-modules '((guix build utils))
                   #~(begin
                       (use-modules (guix build utils))
                       (mkdir #$output)
                       (invoke #+(file-append dico "/libexec/idxgcide")
                               #+(file-append gcide "/share/gcide")
                               #$output)))))

(define %dicod-database:gcide
  (dicod-database
   (name "gcide")
   (handler "gcide")
   (options (list #~(string-append "dbdir=" #$gcide "/share/gcide")
                  #~(string-append "idxdir=" #$%dicod-gcide-index)))))

(define %dicod-accounts
  (list (user-group
         (name "dicod")
         (system? #t))
        (user-account
         (name "dicod")
         (group "dicod")
         (system? #t)
         (home-directory "/var/empty")
         (shell (file-append shadow "/sbin/nologin")))))

(define (dicod-configuration-file config)
  (define handler->text
    (match-lambda
      (($ <dicod-handler> name #f '())
       `("
load-module " ,name ";"))
      (($ <dicod-handler> name #f options)
       (handler->text (dicod-handler
                       (name name)
                       (module name)
                       (options options))))
      (($ <dicod-handler> name module options)
       `("
load-module " ,name " {
   command \"" ,module (string-join (list ,@options) " " 'prefix) "\";
}\n"))))

  (define database->text
    (match-lambda
      (($ <dicod-database> name handler #f options)
       (append
        (handler->text (dicod-handler
                        (name handler)))
        (database->text (dicod-database
                         (name name)
                         (handler handler)
                         (complex? #t)
                         (options options)))))
      (($ <dicod-database> name handler complex? options)
       `("
database {
   name \"" ,name "\";
   handler \"" ,handler
   (string-join (list ,@options) " " 'prefix) "\";
}\n"))))

  (define configuration->text
    (match-lambda
      (($ <dicod-configuration> dico (interfaces ...) handlers databases)
       (append `("listen ("
                 ,(string-join interfaces ", ") ");\n")
               (append-map handler->text handlers)
               (append-map database->text databases)))))

  (apply mixed-text-file "dicod.conf" (configuration->text config)))

(define (dicod-shepherd-service config)
  (let* ((dicod.conf (dicod-configuration-file config))
         (interfaces (dicod-configuration-interfaces config))
         (home-service? (dicod-configuration-home-service? config))
         (mappings  `(,@(if home-service?
                            '()
                            (list (file-system-mapping
                                   (source "/dev/log")
                                   (target source))))
                      ,(file-system-mapping
                        (source dicod.conf)
                        (target source))))
         (dicod      (least-authority-wrapper
                      (file-append (dicod-configuration-dico config)
                                   "/bin/dicod")
                      #:name "dicod"
                      #:mappings mappings
                      #:namespaces (delq 'net %namespaces))))
    (list (shepherd-service
           (provision '(dicod))
           (requirement (if home-service?
                            '()
                            '(user-processes)))
           (documentation "Run the dicod daemon.")
           (start #~(make-inetd-constructor
                     (list #$dicod "--inetd" "--foreground"
                           (string-append "--config=" #$dicod.conf))
                     (map (lambda (interface)
                            (endpoint
                             (addrinfo:addr
                              (car (getaddrinfo interface "dict")))))
                          '#$interfaces)
                     #:requirements '#$requirement
                     #:user #$(and (not home-service?) "dicod")
                     #:group #$(and (not home-service?) "dicod")
                     #:service-name-stem "dicod"))
           (stop #~(make-inetd-destructor))
           (actions (list (shepherd-configuration-action dicod.conf)))))))

(define dicod-service-type
  (service-type
   (name 'dict)
   (extensions
    (list (service-extension account-service-type
                             (const %dicod-accounts))
          (service-extension shepherd-root-service-type
                             dicod-shepherd-service)))
   (default-value (dicod-configuration))
   (description
    "Run @command{dicod}, the dictionary server of
@uref{https://www.gnu.org/software/dico, GNU Dico}.  @command{dicod}
implements the standard DICT protocol supported by clients such as
@command{dico} and GNOME Dictionary.")))

(define-deprecated (dicod-service #:key (config (dicod-configuration)))
  dicod-service-type
  "Return a service that runs the @command{dicod} daemon, an implementation
of DICT server (@pxref{Dicod,,, dico, GNU Dico Manual}).

The optional @var{config} argument specifies the configuration for
@command{dicod}, which should be a @code{<dicod-configuration>} object, by
default it serves the GNU Collaborative International Dictionary of English.

You can add @command{open localhost} to your @file{~/.dico} file to make
@code{localhost} the default server for @command{dico}
client (@pxref{Initialization File,,, dico, GNU Dico Manual})."
  (service dicod-service-type config))
