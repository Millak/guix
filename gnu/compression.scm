;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2022 Mathieu Othacehe <othacehe@gnu.org>
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

(define-module (gnu compression)
  #:use-module (guix gexp)
  #:use-module (guix ui)
  #:use-module ((gnu packages compression) #:hide (zip))
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 match)
  #:export (compressor
            compressor?
            compressor-name
            compressor-extension
            compressor-command
            %compressors
            lookup-compressor))

;; Type of a compression tool.
(define-record-type <compressor>
  (compressor name extension command)
  compressor?
  (name       compressor-name)      ;string (e.g., "gzip")
  (extension  compressor-extension) ;string (e.g., ".lz")
  (command    compressor-command))  ;gexp (e.g., #~(list "/gnu/store/…/gzip"
                                    ;                    "-9n" ))

(define %compressors
  ;; Available compression tools.
  (list (compressor "gzip"  ".gz"
                    #~(list #+(file-append gzip "/bin/gzip") "-9n"))
        (compressor "lzip"  ".lz"
                    #~(list #+(file-append lzip "/bin/lzip") "-9"))
        (compressor "xz"    ".xz"
                    #~(append (list #+(file-append xz "/bin/xz")
                                    "-e")
                              (%xz-parallel-args)))
        (compressor "bzip2" ".bz2"
                    #~(list #+(file-append bzip2 "/bin/bzip2") "-9"))
        (compressor "zstd" ".zst"
                    ;; The default level 3 compresses better than gzip in a
                    ;; fraction of the time, while the highest level 19
                    ;; (de)compresses more slowly and worse than xz.
                    #~(list #+(file-append zstd "/bin/zstd") "-3"))
        (compressor "none" "" #f)))

(define (lookup-compressor name)
  "Return the compressor object called NAME.  Error out if it could not be
found."
  (or (find (match-lambda
              (($ <compressor> name*)
               (string=? name* name)))
            %compressors)
      (leave (G_ "~a: compressor not found~%") name)))
