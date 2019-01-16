;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Mathieu Othacehe <m.othacehe@gmail.com>
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

(define-module (gnu installer newt locale)
  #:use-module (gnu installer locale)
  #:use-module (gnu installer steps)
  #:use-module (gnu installer newt page)
  #:use-module (guix i18n)
  #:use-module (newt)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (ice-9 match)
  #:export (run-locale-page))

(define (run-language-page languages language->text)
  (let ((title (G_ "Locale language")))
    (run-listbox-selection-page
     #:title title
     #:info-text (G_ "Choose the locale's language to be used for the \
installation process. A locale is a regional variant of your language \
encompassing number, date and currency format, among other details.

Based on the language you choose, you will possibly be asked to \
select a locale's territory, codeset and modifier in the next \
steps. The locale will also be used as the default one for the \
installed system.")
     #:info-textbox-width 70
     #:listbox-items languages
     #:listbox-item->text language->text
     #:sort-listbox-items? #f
     #:button-text (G_ "Cancel")
     #:button-callback-procedure
     (lambda _
       (raise
        (condition
         (&installer-step-abort)))))))

(define (run-territory-page territories territory->text)
  (let ((title (G_ "Locale location")))
    (run-listbox-selection-page
     #:title title
     #:info-text (G_ "Choose your locale's location. This is a shortlist of \
locations based on the language you selected.")
     #:listbox-items territories
     #:listbox-item->text territory->text
     #:button-text (G_ "Back")
     #:button-callback-procedure
     (lambda _
       (raise
        (condition
         (&installer-step-abort)))))))

(define (run-codeset-page codesets)
  (let ((title (G_ "Locale codeset")))
    (run-listbox-selection-page
     #:title title
     #:info-text (G_ "Choose your locale's codeset. If UTF-8 is available, \
 it should be preferred.")
     #:listbox-items codesets
     #:listbox-item->text identity
     #:listbox-default-item "UTF-8"
     #:button-text (G_ "Back")
     #:button-callback-procedure
     (lambda _
       (raise
        (condition
         (&installer-step-abort)))))))

(define (run-modifier-page modifiers modifier->text)
  (let ((title (G_ "Locale modifier")))
    (run-listbox-selection-page
     #:title title
     #:info-text (G_ "Choose your locale's modifier. The most frequent \
modifier is euro. It indicates that you want to use Euro as the currency \
symbol.")
     #:listbox-items modifiers
     #:listbox-item->text modifier->text
     #:button-text (G_ "Back")
     #:button-callback-procedure
     (lambda _
       (raise
        (condition
         (&installer-step-abort)))))))

(define* (run-locale-page #:key
                          supported-locales
                          iso639-languages
                          iso3166-territories)
  "Run a page asking the user to select a locale language and possibly
territory, codeset and modifier. Use SUPPORTED-LOCALES as the list of glibc
available locales. ISO639-LANGUAGES is an association list associating a
locale code to a locale name. ISO3166-TERRITORIES is an association list
associating a territory code with a territory name. The formated locale, under
glibc format is returned."

  (define (break-on-locale-found locales)
    "Raise the &installer-step-break condition if LOCALES contains exactly one
element."
    (and (= (length locales) 1)
         (raise
          (condition (&installer-step-break)))))

  (define (filter-locales locales result)
    "Filter the list of locale records LOCALES using the RESULT returned by
the installer-steps defined below."
    (filter
     (lambda (locale)
       (and-map identity
                `(,(string=? (locale-language locale)
                             (result-step result 'language))
                  ,@(if (result-step-done? result 'territory)
                        (list (equal? (locale-territory locale)
                                      (result-step result 'territory)))
                        '())
                  ,@(if (result-step-done? result 'codeset)
                        (list (equal? (locale-codeset locale)
                                      (result-step result 'codeset)))
                        '())
                  ,@(if (result-step-done? result 'modifier)
                        (list (equal? (locale-modifier locale)
                                      (result-step result 'modifier)))
                        '()))))
     locales))

  (define (result->locale-string locales result)
    "Supposing that LOCALES contains exactly one locale record, turn it into a
glibc locale string and return it."
    (match (filter-locales locales result)
      ((locale)
       (locale->locale-string locale))))

  (define (sort-languages languages)
    "Extract some languages from LANGUAGES list and place them ahead."
    (let* ((first-languages '("en"))
           (other-languages (lset-difference equal?
                                             languages
                                             first-languages)))
      `(,@first-languages ,@other-languages)))

  (define locale-steps
    (list
     (installer-step
      (id 'language)
      (compute
       (lambda _
         (run-language-page
          (sort-languages
           (delete-duplicates (map locale-language supported-locales)))
          (cut language-code->language-name iso639-languages <>)))))
     (installer-step
      (id 'territory)
      (compute
       (lambda (result _)
         (let ((locales (filter-locales supported-locales result)))
           ;; Stop the process if the language returned by the previous step
           ;; is matching one and only one supported locale.
           (break-on-locale-found locales)

           ;; Otherwise, ask the user to select a territory among those
           ;; supported by the previously selected language.
           (run-territory-page
            (delete-duplicates (map locale-territory locales))
            (lambda (territory-code)
              (if territory-code
                  (territory-code->territory-name iso3166-territories
                                                  territory-code)
                  (G_ "No location"))))))))
     (installer-step
      (id 'codeset)
      (compute
       (lambda (result _)
         (let ((locales (filter-locales supported-locales result)))
           ;; Same as above but we now have a language and a territory to
           ;; narrow down the search of a locale.
           (break-on-locale-found locales)

           ;; Otherwise, ask for a codeset.
           (run-codeset-page
            (delete-duplicates (map locale-codeset locales)))))))
     (installer-step
      (id 'modifier)
      (compute
       (lambda (result _)
         (let ((locales (filter-locales supported-locales result)))
           ;; Same thing with a language, a territory and a codeset this time.
           (break-on-locale-found locales)

           ;; Otherwise, ask for a modifier.
           (run-modifier-page
            (delete-duplicates (map locale-modifier locales))
            (lambda (modifier)
              (or modifier (G_ "No modifier"))))))))))

  ;; If run-installer-steps returns locally, it means that the user had to go
  ;; through all steps (language, territory, codeset and modifier) to select a
  ;; locale. In that case, like if we exited by raising &installer-step-break
  ;; condition, turn the result into a glibc locale string and return it.
  (result->locale-string
   supported-locales
   (run-installer-steps #:steps locale-steps)))
