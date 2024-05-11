;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014-2016, 2019, 2021-2024 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014 Sree Harsha Totakura <sreeharsha@totakura.in>
;;; Copyright © 2017, 2019, 2021 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (guix svn-download)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (guix monads)
  #:use-module (guix modules)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module ((guix build svn) #:prefix build:)
  #:use-module ((guix build utils) #:select (mkdir-p))
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (rnrs bytevectors)
  #:export (svn-reference
            svn-reference?
            svn-reference-url
            svn-reference-revision
            svn-reference-recursive?
            svn-reference-user-name
            svn-reference-password
            svn-fetch
            download-svn-to-store

            svn-multi-reference
            svn-multi-reference?
            svn-multi-reference-url
            svn-multi-reference-revision
            svn-multi-reference-locations
            svn-multi-reference-recursive?
            svn-multi-reference-user-name
            svn-multi-reference-password
            svn-multi-fetch
            download-multi-svn-to-store))

;;; Commentary:
;;;
;;; An <origin> method that fetches a specific revision from a Subversion
;;; repository.  The repository URL and REVISION are specified with a
;;; <svn-reference> object.  REVISION should be specified as a number.
;;;
;;; Code:

(define-record-type* <svn-reference>
  svn-reference make-svn-reference
  svn-reference?
  (url        svn-reference-url)                    ; string
  (revision   svn-reference-revision)               ; number
  (recursive? svn-reference-recursive? (default #f))
  (user-name  svn-reference-user-name (default #f))
  (password   svn-reference-password (default #f)))

(define (subversion-package)
  "Return the default Subversion package."
  (let ((distro (resolve-interface '(gnu packages version-control))))
    (module-ref distro 'subversion)))

(define (svn-fetch-builder svn hash-algo)
  (define guile-json
    (module-ref (resolve-interface '(gnu packages guile)) 'guile-json-4))

  (define guile-lzlib
    (module-ref (resolve-interface '(gnu packages guile)) 'guile-lzlib))

  (define guile-gnutls
    (module-ref (resolve-interface '(gnu packages tls)) 'guile-gnutls))

  (define tar+gzip                                ;for (guix swh)
    (list (module-ref (resolve-interface '(gnu packages compression))
                      'gzip)
          (module-ref (resolve-interface '(gnu packages base))
                      'tar)))

  (with-imported-modules
      (source-module-closure '((guix build svn)
                               (guix build download)
                               (guix build download-nar)
                               (guix build utils)
                               (guix swh)))
    (with-extensions (list guile-json guile-gnutls   ;for (guix swh)
                           guile-lzlib)
      #~(begin
          (use-modules (guix build svn)
                       ((guix build download)
                        #:select (download-method-enabled?))
                       (guix build download-nar)
                       (guix build utils)
                       (guix swh)
                       (ice-9 match))

          ;; Add tar and gzip to $PATH so
          ;; 'swh-download-directory-by-nar-hash' can invoke them.
          (set-path-environment-variable "PATH" '("bin") '(#+@tar+gzip))

          (or (and (download-method-enabled? 'upstream)
                   (svn-fetch (getenv "svn url")
                              (string->number (getenv "svn revision"))
                              #$output
                              #:svn-command #+(file-append svn "/bin/svn")
                              #:recursive? (match (getenv "svn recursive?")
                                             ("yes" #t)
                                             (_ #f))
                              #:user-name (getenv "svn user name")
                              #:password (getenv "svn password")))
              (and (download-method-enabled? 'nar)
                   (download-nar #$output))
              (and (download-method-enabled? 'swh)
                   (parameterize ((%verify-swh-certificate? #f))
                     (swh-download-directory-by-nar-hash
                      (u8-list->bytevector
                       (map string->number
                            (string-split (getenv "hash") #\,)))
                      '#$hash-algo
                      #$output))))))))

(define* (svn-fetch ref hash-algo hash
                    #:optional name
                    #:key (system (%current-system)) (guile (default-guile))
                    (svn (subversion-package)))
  "Return a fixed-output derivation that fetches REF, a <svn-reference>
object.  The output is expected to have recursive hash HASH of type
HASH-ALGO (a symbol).  Use NAME as the file name, or a generic name if #f."
  (mlet %store-monad ((guile (package->derivation guile system)))
    (gexp->derivation (or name "svn-checkout")
                      ;; Avoid the builder differing for every single use as
                      ;; having less builder is more efficient for computing
                      ;; derivations.
                      ;;
                      ;; Don't pass package specific data in to the following
                      ;; procedure, use #:env-vars below instead.
                      (svn-fetch-builder svn hash-algo)
                      #:script-name "svn-download"
                      #:env-vars
                      `(("svn url" . ,(svn-reference-url ref))
                        ("svn revision"
                         . ,(number->string (svn-reference-revision ref)))
                        ,@(if (svn-reference-recursive? ref)
                              `(("svn recursive?" . "yes"))
                              '())
                        ,@(if (svn-reference-user-name ref)
                              `(("svn user name"
                                 . ,(svn-reference-user-name ref)))
                              '())
                        ,@(if (svn-reference-password ref)
                              `(("svn password"
                                 . ,(svn-reference-password ref)))
                              '())
                        ,@(match (getenv "GUIX_DOWNLOAD_METHODS")
                            (#f '())
                            (value
                             `(("GUIX_DOWNLOAD_METHODS" . ,value))))
                        ;; To avoid pulling in (guix base32) in the builder
                        ;; script, use bytevector->u8-list from (rnrs
                        ;; bytevectors)
                        ("hash" . ,(string-join
                                    (map number->string
                                         (bytevector->u8-list hash))
                                    ",")))

                      #:system system
                      #:hash-algo hash-algo
                      #:hash hash
                      #:recursive? #t
                      #:guile-for-build guile
                      #:local-build? #t)))

(define-record-type* <svn-multi-reference>
  svn-multi-reference make-svn-multi-reference
  svn-multi-reference?
  (url        svn-multi-reference-url)                 ; string
  (revision   svn-multi-reference-revision)            ; number
  (locations  svn-multi-reference-locations)           ; list of strings
  (recursive? svn-multi-reference-recursive? (default #f))
  (user-name  svn-multi-reference-user-name (default #f))
  (password   svn-multi-reference-password (default #f)))

(define (svn-multi-fetch-builder svn hash-algo)
  (define guile-json
    (module-ref (resolve-interface '(gnu packages guile)) 'guile-json-4))

  (define guile-lzlib
    (module-ref (resolve-interface '(gnu packages guile)) 'guile-lzlib))

  (define guile-gnutls
    (module-ref (resolve-interface '(gnu packages tls)) 'guile-gnutls))

  (define tar+gzip                                ;for (guix swh)
    (list (module-ref (resolve-interface '(gnu packages compression))
                      'gzip)
          (module-ref (resolve-interface '(gnu packages base))
                      'tar)))

  (with-imported-modules
      (source-module-closure '((guix build svn)
                               (guix build download)
                               (guix build download-nar)
                               (guix build utils)
                               (guix swh)))
    (with-extensions (list guile-json guile-gnutls   ;for (guix swh)
                           guile-lzlib)
      #~(begin
          (use-modules (guix build svn)
                       (guix build utils)
                       ((guix build download)
                        #:select (download-method-enabled?))
                       (guix build download-nar)
                       (guix swh)
                       (srfi srfi-1)
                       (ice-9 match)
                       (rnrs bytevectors))

          ;; Add tar and gzip to $PATH so
          ;; 'swh-download-directory-by-nar-hash' can invoke them.
          (set-path-environment-variable "PATH" '("bin") '(#+@tar+gzip))

          (or (every
               (lambda (location)
                 ;; The directory must exist if we are to fetch only a
                 ;; single file.
                 (unless (string-suffix? "/" location)
                   (mkdir-p (string-append #$output "/" (dirname location))))
                 (and (download-method-enabled? 'upstream)
                      (svn-fetch (string-append (getenv "svn url") "/" location)
                                 (string->number (getenv "svn revision"))
                                 (if (string-suffix? "/" location)
                                     (string-append #$output "/" location)
                                     (string-append #$output "/" (dirname location)))
                                 #:svn-command #+(file-append svn "/bin/svn")
                                 #:recursive? (match (getenv "svn recursive?")
                                                ("yes" #t)
                                                (_ #f))
                                 #:user-name (getenv "svn user name")
                                 #:password (getenv "svn password"))))
               (call-with-input-string (getenv "svn locations")
                 read))
              (begin
                (when (file-exists? #$output)
                  (delete-file-recursively #$output))
                (or (and (download-method-enabled? 'nar)
                         (download-nar #$output))
                    (and (download-method-enabled? 'swh)
                         ;; SWH keeps HASH as an ExtID for the combination
                         ;; of files/directories, which allows us to
                         ;; retrieve the entire combination at once:
                         ;; <https://gitlab.softwareheritage.org/swh/infra/sysadm-environment/-/issues/5263>.
                         (parameterize ((%verify-swh-certificate? #f))
                           (swh-download-directory-by-nar-hash
                            (u8-list->bytevector
                             (map string->number
                                  (string-split (getenv "hash") #\,)))
                            '#$hash-algo
                            #$output))))))))))

(define* (svn-multi-fetch ref hash-algo hash
                          #:optional name
                          #:key (system (%current-system)) (guile (default-guile))
                          (svn (subversion-package)))
  "Return a fixed-output derivation that fetches REF, a <svn-multi-reference>
object.  The output is expected to have recursive hash HASH of type
HASH-ALGO (a symbol).  Use NAME as the file name, or a generic name if #f."
  (mlet %store-monad ((guile (package->derivation guile system)))
    (gexp->derivation (or name "svn-checkout")
                      ;; Avoid the builder differing for every single use as
                      ;; having less builder is more efficient for computing
                      ;; derivations.
                      ;;
                      ;; Don't pass package specific data in to the following
                      ;; procedure, use #:env-vars below instead.
                      (svn-multi-fetch-builder svn hash-algo)
                      #:script-name "svn-multi-download"
                      #:env-vars
                      `(("svn url" . ,(svn-multi-reference-url ref))
                        ("svn locations"
                         . ,(object->string (svn-multi-reference-locations ref)))
                        ("svn revision"
                         . ,(number->string (svn-multi-reference-revision ref)))
                        ,@(if (svn-multi-reference-recursive? ref)
                              `(("svn recursive?" . "yes"))
                              '())
                        ,@(if (svn-multi-reference-user-name ref)
                              `(("svn user name"
                                 . ,(svn-multi-reference-user-name ref)))
                              '())
                        ,@(if (svn-multi-reference-password ref)
                              `(("svn password"
                                 . ,(svn-multi-reference-password ref)))
                              '())
                        ,@(match (getenv "GUIX_DOWNLOAD_METHODS")
                            (#f '())
                            (value
                             `(("GUIX_DOWNLOAD_METHODS" . ,value))))
                        ;; To avoid pulling in (guix base32) in the builder
                        ;; script, use bytevector->u8-list from (rnrs
                        ;; bytevectors)
                        ("hash" . ,(string-join
                                    (map number->string
                                         (bytevector->u8-list hash))
                                    ",")))

                      #:leaked-env-vars '("http_proxy" "https_proxy"
                                          "LC_ALL" "LC_MESSAGES" "LANG"
                                          "COLUMNS")
                      #:system system
                      #:hash-algo hash-algo
                      #:hash hash
                      #:recursive? #t
                      #:guile-for-build guile
                      #:local-build? #t)))

(define* (download-svn-to-store store ref
                                #:optional (name (basename (svn-reference-url ref)))
                                #:key (log (current-error-port)))
  "Download from REF, a <svn-reference> object to STORE.  Write progress
reports to LOG."
  (call-with-temporary-directory
   (lambda (temp)
     (let ((result
            (parameterize ((current-output-port log))
              (build:svn-fetch (svn-reference-url ref)
                               (svn-reference-revision ref)
                               (string-append temp "/svn")
                               #:user-name (svn-reference-user-name ref)
                               #:password (svn-reference-password ref)))))
       (and result
            (add-to-store store name #t "sha256"
                          (string-append temp "/svn")))))))

(define* (download-multi-svn-to-store store ref
                                      #:optional (name (basename (svn-multi-reference-url ref)))
                                      #:key (log (current-error-port)))
  "Download from REF, a <svn-multi-reference> object to STORE.  Write progress
reports to LOG."
  (call-with-temporary-directory
   (lambda (temp)
     (and (every (lambda (location)
                   (let ((dir (string-append temp "/" (dirname location))))
                     (mkdir-p dir))
                   (parameterize ((current-output-port log))
                     (build:svn-fetch (string-append (svn-multi-reference-url ref)
                                                     "/" location)
                                      (svn-multi-reference-revision ref)
                                      (if (string-suffix? "/" location)
                                          (string-append temp "/" location)
                                          (string-append temp "/" (dirname location)))
                                      #:recursive?
                                      (svn-multi-reference-recursive? ref)
                                      #:user-name (svn-multi-reference-user-name ref)
                                      #:password (svn-multi-reference-password ref))))
                 (svn-multi-reference-locations ref))
          (add-to-store store name #t "sha256" temp)))))

;;; svn-download.scm ends here
