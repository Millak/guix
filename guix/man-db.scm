;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2018 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2022, 2024 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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

(define-module (guix man-db)
  #:autoload (zlib) (call-with-gzip-input-port)
  #:autoload (zstd) (call-with-zstd-input-port)
  #:use-module ((guix build utils) #:select (find-files))
  #:use-module (gdbm)                             ;gdbm-ffi
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:export (mandb-entry?
            mandb-entry-file-name
            mandb-entry-name
            mandb-entry-section
            mandb-entry-synopsis
            mandb-entry-kind

            mandb-entries
            write-mandb-database))

;;; Comment:
;;;
;;; Scan gzipped man pages and create a man-db database.  The database is
;;; meant to be used by 'man -k KEYWORD'.
;;;
;;; The implementation here aims to be simpler than that of 'man-db', and to
;;; produce deterministic output.  See <https://bugs.gnu.org/29654>.
;;;
;;; Code:

(define-record-type <mandb-entry>
  (mandb-entry file-name name section synopsis kind)
  mandb-entry?
  (file-name mandb-entry-file-name)               ;e.g., "../abiword.1.zst"
  (name      mandb-entry-name)                    ;e.g., "ABIWORD"
  (section   mandb-entry-section)                 ;number
  (synopsis  mandb-entry-synopsis)                ;string
  (kind      mandb-entry-kind))                   ;'ultimate | 'link

(define (mandb-entry<? entry1 entry2)
  (match entry1
    (($ <mandb-entry> file1 name1 section1)
     (match entry2
       (($ <mandb-entry> file2 name2 section2)
        (or (< section1 section2)
            (string<? (basename file1) (basename file2))))))))

(define abbreviate-file-name
  (let ((man-file-rx (make-regexp "(.+)\\.[0-9][a-z]?(\\.(gz|zst))?$")))
    (lambda (file)
      (match (regexp-exec man-file-rx (basename file))
        (#f
         (basename file))
        (matches
         (match:substring matches 1))))))

(define (gzip-compressed? file-name)
  "True if FILE-NAME is suffixed with the '.gz' file extension."
  (string-suffix? ".gz" file-name))

(define (zstd-compressed? file-name)
  "True if FILE-NAME is suffixed with the '.zst' file extension."
  (string-suffix? ".zst" file-name))

(define (entry->string entry)
  "Return the wire format for ENTRY as a string."
  (match entry
    (($ <mandb-entry> file name section synopsis kind)
     ;; See db_store.c:make_content in man-db for the format.
     (string-append (abbreviate-file-name file) "\t"
                    (number->string section) "\t"
                    (number->string section)

                    ;; Timestamp that we always set to the epoch.
                    "\t0\t0"

                    ;; See "db_storage.h" in man-db for the different kinds.
                    "\t"
                    (case kind
                      ((ultimate) "A")     ;ultimate man page
                      ((link)     "B")     ;".so" link to other man page
                      (else       "A"))    ;something that doesn't matter much

                    "\t-\t-\t"

                    (cond
                     ((gzip-compressed? file) "gz")
                     ((zstd-compressed? file) "zst")
                     (else ""))

                    "\t"

                    synopsis "\x00"))))

;; The man-db schema version we're compatible with.
(define %version-key "$version$\x00")
(define %version-value "2.5.0\x00")

(define (write-mandb-database file entries)
  "Write ENTRIES to FILE as a man-db database.  FILE is usually
\".../index.db\", and is a GDBM database."
  (let ((db (gdbm-open file GDBM_WRCREAT)))
    (gdbm-set! db %version-key %version-value)

    ;; Write ENTRIES in sorted order so we get deterministic output.
    (for-each (lambda (entry)
                (gdbm-set! db
                           ;; For the 'whatis' tool to find anything, the key
                           ;; should match the name of the software,
                           ;; e.g. 'cat'.  Derive it from the file name, as
                           ;; the name could technically be #f.
                           (string-append (abbreviate-file-name
                                           (mandb-entry-file-name entry))
                                          "\x00")
                           (entry->string entry)))
              (sort entries mandb-entry<?))
    (gdbm-close db)))

(define (read-synopsis port)
  "Read from PORT a man page synopsis."
  (define (section? line)
    ;; True if LINE starts with ".SH", ".PP", or so.
    (string-prefix? "." (string-trim line)))

  (define (extract-synopsis str)
    (match (string-contains str "\\-")
      (#f "")
      (index
       (string-map (match-lambda
                     (#\newline #\space)
                     (chr chr))
                   (string-trim-both (string-drop str (+ 2 index)))))))

  ;; Synopses look like "Command \- Do something.", possibly spanning several
  ;; lines.
  (let loop ((lines '()))
    (match (read-line port 'concat)
      ((? eof-object?)
       (extract-synopsis (string-concatenate-reverse lines)))
      ((? section?)
       (extract-synopsis (string-concatenate-reverse lines)))
      (line
       (loop (cons line lines))))))

(define (man-macro-tokenize input)
  "Split INPUT string, a man macro invocation, into a list containing the macro's
name followed by its arguments."
  (let loop ((pos 0)
             (tokens '())
             (characters '())
             (in-string? #f))
    (if (>= pos (string-length input))
        ;; End of input
        (reverse (if (null? characters)
                     tokens
                     (cons (list->string (reverse characters)) tokens)))
        (let ((c (string-ref input pos)))
          (cond
           ;; Inside a string
           (in-string?
            (if (char=? c #\")
                (if (and (< (+ pos 1) (string-length input))
                         (char=? (string-ref input (+ pos 1)) #\"))
                    ;; Double quote inside string
                    (loop (+ pos 2) tokens (cons #\" characters) #t)
                    ;; End of string
                    (loop (+ pos 1) (cons (list->string (reverse characters)) tokens) '() #f))
                ;; Regular character in string
                (loop (+ pos 1) tokens (cons c characters) #t)))

           ;; Whitespace outside string
           ((char-whitespace? c)
            (if (null? characters)
                (loop (+ pos 1) tokens '() #f)
                (loop (+ pos 1) (cons (list->string (reverse characters)) tokens) '() #f)))

           ;; Start of string
           ((char=? c #\")
            (if (null? characters)
                (loop (+ pos 1) tokens '() #t)
                (loop pos (cons (list->string (reverse characters)) tokens) '() #f)))

           ;; Symbol character
           (else
            (loop (+ pos 1) tokens (cons c characters) #f)))))))

(define* (man-page->entry file #:optional (resolve identity))
  "Parse FILE, a gzip or zstd compressed man page, and return a <mandb-entry>
for it."
  (define call-with-input-port*
    (cond
     ((gzip-compressed? file) call-with-gzip-input-port)
     ((zstd-compressed? file) call-with-zstd-input-port)
     (else call-with-port)))

  (call-with-input-port* (open-file file "r0")
    (lambda (port)
      (let loop ((name     #f)
                 (section  #f)
                 (synopsis #f)
                 (kind     'ultimate))
        (if (and name section synopsis)
            (mandb-entry file name section synopsis kind)
            (let ((line (read-line port)))
              (if (eof-object? line)
                  (mandb-entry file name (or section 0) (or synopsis "")
                               kind)
                  ;; man 7 groff groff_mdoc groff_man
                  ;; look for metadata in macro invocations (lines starting with .)
                  (match (and (string-prefix? "." line) (man-macro-tokenize line))
                    ((".TH" name (= string->number section) _ ...)
                     (loop name section synopsis kind))
                    ((".SH" (or "NAME" "\"NAME\""))
                     (loop name section (read-synopsis port) kind))
                    ((".so" link)
                     (match (and=> (resolve link)
                                   (cut man-page->entry <> resolve))
                       (#f
                        (loop name section synopsis 'link))
                       (alias
                        (mandb-entry file
                                     (mandb-entry-name alias)
                                     (mandb-entry-section alias)
                                     (mandb-entry-synopsis alias)
                                     'link))))
                    (_
                     (loop name section synopsis kind))))))))))

(define (man-files directory)
  "Return the list of man pages found under DIRECTORY, recursively."
  ;; Filter the list to ensure that broken symlinks are excluded.
  (filter file-exists?
          (find-files directory "\\.[0-9][a-z]?(\\.(gz|zst))?$")))

(define (mandb-entries directory)
  "Return mandb entries for the man pages found under DIRECTORY, recursively."
  (map (lambda (file)
         (man-page->entry file
                          (lambda (link)
                            (let ((file-gz (string-append directory "/" link
                                                          ".gz"))
                                  (file-zst (string-append directory "/" link
                                                           ".zst")))
                              (or (and (file-exists? file-gz) file-gz)
                                  (and (file-exists? file-zst) file-zst))))))
       (man-files directory)))
