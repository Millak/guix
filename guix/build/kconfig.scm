;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Stefan <stefan-guix@vodafonemail.de>
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

(define-module (guix build kconfig)
  #:use-module  (ice-9 rdelim)
  #:use-module  (ice-9 regex)
  #:use-module  (srfi srfi-1)
  #:use-module  (srfi srfi-26)
  #:export (modify-defconfig
            verify-config))

;; Commentary:
;;
;; Builder-side code to modify configurations for the Kconfig build system as
;; used by Linux and U-Boot.
;;
;; Code:

(define (pair->config-string pair)
  "Convert a PAIR back to a config-string."
  (let* ((key (first pair))
         (value (cdr pair)))
    (if (string? key)
        (if (string? value)
            (string-append key "=" value)
            (string-append "# " key " is not set"))
        value)))

(define (config-string->pair config-string)
  "Parse a configuration string like \"CONFIG_EXAMPLE=m\" into a key-value pair.
An error is thrown for invalid configurations.

\"CONFIG_A=y\"            -> '(\"CONFIG_A\" . \"y\")
\"CONFIG_B=\\\"\\\"\"         -> '(\"CONFIG_B\" . \"\\\"\\\"\")
\"CONFIG_C=\"             -> '(\"CONFIG_C\" . \"\")
\"# CONFIG_E is not set\" -> '(\"CONFIG_E\" . #f)
\"CONFIG_D\"              -> '(\"CONFIG_D\" . #f)
\"# Any comment\"         -> '(#f . \"# Any comment\")
\"\"                      -> '(#f . \"\")
\"# CONFIG_E=y\"          -> (error \"Invalid configuration\")
\"CONFIG_E is not set\"   -> (error \"Invalid configuration\")
\"Anything else\"         -> (error \"Invalid configuration\")"
  (define config-regexp
    (make-regexp
     ;; (match:substring (string-match "=(.*)" "=") 1) returns "", but the
     ;; pattern "=(.+)?" makes it return #f instead.  From a "CONFIG_A=" we like
     ;; to get "", which later emits "CONFIG_A=" again.
     (string-append "^ *(#[\\t ]*)?(CONFIG_[a-zA-Z0-9_]+)([\\t ]*="
                    "[\\t ]*(.*)|([\\t ]+is[\\t ]+not[\\t ]+set))?$")))

  (define config-comment-regexp
    (make-regexp "^([\\t ]*(#.*)?)$"))

  (let ((match (regexp-exec config-regexp (string-trim-right config-string))))
    (if match
        (let* ((comment (match:substring match 1))
               (key (match:substring match 2))
               (unset (match:substring match 5))
               (value (and (not comment)
                           (not unset)
                           (match:substring match 4))))
          (if (eq? (not comment) (not unset))
              ;; The key is uncommented and set or commented and unset.
              (cons key value)
              ;; The key is set or unset ambigiously.
              (error (format #f "invalid configuration, did you mean \"~a\"?"
                             (pair->config-string (cons key #f)))
                     config-string)))
        ;; This is not a valid or ambigious config-string, but maybe a
        ;; comment.
        (if (regexp-exec config-comment-regexp config-string)
            (cons #f config-string)     ;keep valid comments
            (error "Invalid configuration" config-string)))))

(define (defconfig->alist defconfig)
  "Convert the content of a DEFCONFIG (or .config) file into an alist."
  (with-input-from-file defconfig
    (lambda ()
      (let loop ((alist '())
                 (line (read-line)))
        (if (eof-object? line)
            ;; Building the alist is done, now check for duplicates.
            ;; Note: the filter invocation is used to remove comments.
            (let loop ((keys (map first (filter first alist)))
                       (duplicates '()))
              (if (null? keys)
                  ;; The search for duplicates is done.
                  ;; Return the alist or throw an error on duplicates.
                  (if (null? duplicates)
                      (reverse alist)
                      (error
                       (format #f "duplicate configurations in ~a" defconfig)
                       (reverse duplicates)))
                  ;; Continue the search for duplicates.
                  (loop (cdr keys)
                        (if (member (first keys) (cdr keys))
                            (cons (first keys) duplicates)
                            duplicates))))
            ;; Build the alist.
            (loop (cons (config-string->pair line) alist)
                  (read-line)))))))

(define (modify-defconfig defconfig configs)
  "This function can modify a given DEFCONFIG (or .config) file by adding,
changing or removing the list of strings in CONFIGS.  This allows customization
of Kconfig based projects like the kernel Linux or the bootloader 'Das U-Boot'.

These are examples for CONFIGS to add, change or remove configurations to/from
DEFCONFIG:

'(\"CONFIG_A=\\\"a\\\"\"
  \"CONFIG_B=0\"
  \"CONFIG_C=y\"
  \"CONFIG_D=m\"
  \"CONFIG_E=\"
  \"# CONFIG_G is not set\"
  ;; For convenience this abbrevation can be used for not set configurations.
  \"CONFIG_F\")

Instead of a list, CONFIGS can be a string with one configuration per line."
  ;; Normalize CONFIGS to a list of configuration pairs.
  (let* ((config-pairs (map config-string->pair
                            (append-map (cut string-split <>  #\newline)
                                        (if (string? configs)
                                            (list configs)
                                            configs))))
         ;; Generate a blocklist from all valid keys in config-pairs.
         (blocklist (delete #f (map first config-pairs)))
         ;; Generate an alist from the defconfig without the keys in blocklist.
         (filtered-defconfig-pairs (remove (lambda (pair)
                                             (member (first pair) blocklist))
                                           (defconfig->alist defconfig))))
    (with-output-to-file defconfig
      (lambda ()
        (for-each (lambda (pair)
                    (display (pair->config-string pair))
                    (newline))
                  (append filtered-defconfig-pairs config-pairs))))))

(define (verify-config config defconfig)
  "Verify that the CONFIG file contains all configurations from the DEFCONFIG
file.  When the verification fails, raise an error with the mismatching keys
and their values."
  (let* ((config-pairs (defconfig->alist config))
         (defconfig-pairs (defconfig->alist defconfig))
         (mismatching-pairs
          (remove (lambda (pair)
                    ;; Remove all configurations, whose values are #f and
                    ;; whose keys are not in config-pairs, as not in
                    ;; config-pairs means unset, ...
                    (and (not (cdr pair))
                         (not (assoc-ref config-pairs (first pair)))))
                  ;; ... from the defconfig-pairs different to config-pairs.
                  (lset-difference equal?
                                   ;; Remove comments by filtering with first.
                                   (filter first defconfig-pairs)
                                   config-pairs))))
    (unless (null? mismatching-pairs)
      (error (format #f "Mismatching configurations in ~a and ~a"
                     config defconfig)
             (map (lambda (mismatching-pair)
                    (let* ((key (first mismatching-pair))
                           (defconfig-value (cdr mismatching-pair))
                           (config-value (assoc-ref config-pairs key)))
                      (cons key (list (list config-value defconfig-value)))))
                  mismatching-pairs)))))
