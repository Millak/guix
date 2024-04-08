;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014-2017, 2019-2021, 2023-2024 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2018 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2023 Janneke Nieuwenhuizen <jannek@gnu.org>
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

(define-module (gnu system locale)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (guix modules)
  #:use-module (guix records)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:export (locale-definition
            locale-definition?
            locale-definition-name
            locale-definition-source
            locale-definition-charset

            locale-name->definition
            locale-directory

            %default-locale-libcs
            %default-locale-definitions

            glibc-supported-locales))

;;; Commentary:
;;;
;;; Locale definitions, and compilation thereof.
;;;
;;; Code:

(define-record-type* <locale-definition> locale-definition
  make-locale-definition
  locale-definition?
  (name    locale-definition-name)                ;string--e.g., "fr_FR.utf8"
  (source  locale-definition-source)              ;string--e.g., "fr_FR"
  (charset locale-definition-charset              ;string--e.g., "UTF-8"
           (default "UTF-8")))

(define %not-dot
  (char-set-complement (char-set #\.)))

(define (denormalize-codeset codeset)
  "Attempt to guess the \"real\" name of CODESET, a normalized codeset as
defined in (info \"(libc) Using gettextized software\")."
  (cond ((string=? codeset "utf8")
         "UTF-8")
        ((string-prefix? "iso8859" codeset)
         (string-append "ISO-8859-" (string-drop codeset 7)))
        ((string=? codeset "eucjp")
         "EUC-JP")
        (else                                ;cross fingers, hope for the best
         codeset)))

(define (locale-name->definition name)
  "Return a <locale-definition> corresponding to NAME, guessing the charset,
or #f on failure."
  (match (string-tokenize name %not-dot)
    ((source charset)
     ;; XXX: NAME is supposed to use the "normalized codeset", such as "utf8",
     ;; whereas the actual name used is different.  Add a special case to make
     ;; the right guess for UTF-8.
     (locale-definition (name name)
                        (source source)
                        (charset (denormalize-codeset charset))))
    (_
     #f)))

(define* (single-locale-directory locales
                                  #:key (libc glibc))
  "Return a directory containing all of LOCALES for LIBC compiled.

Because locale data formats are incompatible when switching from one libc to
another, locale data is put in a sub-directory named after the 'version' field
of LIBC."
  (define version
    (version-major+minor (package-version libc)))

  (define build
    (with-imported-modules (source-module-closure
                            '((gnu build locale)))
     #~(begin
         (use-modules (gnu build locale))

         (mkdir #$output)
         (mkdir (string-append #$output "/" #$version))

         ;; 'localedef' executes 'gzip' to access compressed locale sources.
         (setenv "PATH"
                 (string-append #+gzip "/bin:" #+libc "/bin"))

         (setvbuf (current-output-port) 'line)
         (setvbuf (current-error-port) 'line)
         (for-each (lambda (locale codeset name)
                     (build-locale locale
                                   #:codeset codeset
                                   #:name name
                                   #:directory
                                   (string-append #$output "/" #$version)))
                   '#$(map locale-definition-source locales)
                   '#$(map locale-definition-charset locales)
                   '#$(map locale-definition-name locales)))))

  (computed-file (string-append "locale-" version) build))

(define* (locale-directory locales
                           #:key (libcs %default-locale-libcs))
  "Return a locale directory containing all of LOCALES for each libc package
listed in LIBCS.

It is useful to list more than one libc when willing to support
already-installed packages built against a different libc since the locale
data format changes between libc versions."
  (match libcs
    ((libc)
     (single-locale-directory locales #:libc libc))
    ((libcs ..1)
     (let ((dirs (map (lambda (libc)
                        (single-locale-directory locales #:libc libc))
                      libcs)))
       (computed-file "locale-multiple-versions"
                      (with-imported-modules '((guix build union))
                        #~(begin
                            (use-modules (guix build union))
                            (union-build #$output (list #$@dirs))))
                      #:options '(#:local-build? #t
                                  #:substitutable? #f))))))

(define %default-locale-libcs
  ;; The libcs for which we build locales by default.
  (if (system-hurd?)
      (list glibc/hurd)
      (list glibc)))

(define %default-locale-definitions
  ;; Arbitrary set of locales that are built by default.  They come as a
  ;; "bonus" in addition to that specified in the 'locale' field of the
  ;; operating system, for the user's convenience, so they shouldn't take too
  ;; much space.
  (letrec-syntax ((utf8-locale (syntax-rules ()
                                 ((_ name*)
                                  (locale-definition
                                   ;; Note: We choose "utf8", which is the
                                   ;; "normalized codeset".
                                   (name (string-append name* ".utf8"))
                                   (source name*)
                                   (charset "UTF-8")))))
                  (utf8-locales (syntax-rules ()
                                  ((_ name ...)
                                   (list (utf8-locale name) ...)))))
    ;; The six UN official languages plus Portuguese, with at most two
    ;; variants per language.
    (utf8-locales "ar_DZ"
                  "en_GB"
                  "en_US"
                  "es_AR"
                  "es_ES"
                  "fr_FR"
                  "pt_BR"
                  "pt_PT"
                  "ru_RU"
                  "zh_CN")))


;;;
;;; Locales supported by glibc.
;;;

(define* (glibc-supported-locales #:optional (glibc glibc))
  "Return a file-like object that contains a list of locale name/encoding
pairs such as (\"oc_FR.UTF-8\" . \"UTF-8\").  Each pair corresponds to a
locale supported by GLIBC."
  (define build
    (with-imported-modules (source-module-closure
                            '((guix build gnu-build-system)
                              (gnu build locale)))
      #~(begin
          (use-modules (guix build gnu-build-system)
                       (gnu build locale)
                       (ice-9 pretty-print))

          (define unpack
            (assq-ref %standard-phases 'unpack))


          (setenv "PATH"
                  (string-append #+(file-append tar "/bin") ":"
                                 #+(file-append xz "/bin") ":"
                                 #+(file-append gzip "/bin")))
          (unpack #:source #+(package-source glibc))

          (let ((locales (call-with-input-file "localedata/SUPPORTED"
                           read-supported-locales)))
            (call-with-output-file #$output
              (lambda (port)
                (pretty-print locales port)))))))

  (computed-file "glibc-supported-locales.scm" build))

;;; locale.scm ends here
