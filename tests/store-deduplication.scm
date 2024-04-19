;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018, 2020-2022, 2024 Ludovic Courtès <ludo@gnu.org>
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

(define-module (test-store-deduplication)
  #:use-module (guix tests)
  #:use-module (guix store deduplication)
  #:use-module (gcrypt hash)
  #:use-module ((guix utils) #:select (call-with-temporary-directory))
  #:use-module (guix build utils)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-64))

(define (cartesian-product . lst)
  "Return the Cartesian product of all the given lists."
  (match lst
    ((head)
     (map list head))
    ((head . rest)
     (let ((others (apply cartesian-product rest)))
       (append-map (lambda (init)
                     (map (lambda (lst)
                            (cons init lst))
                          others))
                   head)))
    (()
     '())))


(test-begin "store-deduplication")

(test-equal "deduplicate, below %deduplication-minimum-size"
  (list #t (make-list 5 1))

  (call-with-temporary-directory
   (lambda (store)
     ;; Note: DATA must be longer than %DEDUPLICATION-MINIMUM-SIZE.
     (let ((data      "Hello, world!")
           (identical (map (lambda (n)
                             (string-append store "/" (number->string n)
                                            "/a/b/c"))
                           (iota 5))))
       (for-each (lambda (file)
                   (mkdir-p (dirname file))
                   (call-with-output-file file
                     (lambda (port)
                       (put-bytevector port (string->utf8 data)))))
                 identical)

       (deduplicate store (nar-sha256 store) #:store store)

       ;; (system (string-append "ls -lRia " store))
       (list (= (length (delete-duplicates
                         (map (compose stat:ino stat) identical)))
                (length identical))
             (map (compose stat:nlink stat) identical))))))

(test-equal "deduplicate"
  (cons* #t #f                                    ;inode comparisons
         2 (make-list 5 6))                       ;'nlink' values

  (call-with-temporary-directory
   (lambda (store)
     ;; Note: DATA must be longer than %DEDUPLICATION-MINIMUM-SIZE.
     (let ((data      (string-concatenate (make-list 1000 "Hello, world!")))
           (identical (map (lambda (n)
                             (string-append store "/" (number->string n)
                                            "/a/b/c"))
                           (iota 5)))
           (unique    (string-append store "/unique")))
       (for-each (lambda (file)
                   (mkdir-p (dirname file))
                   (call-with-output-file file
                     (lambda (port)
                       (put-bytevector port (string->utf8 data)))))
                 identical)
       ;; Make the parent of IDENTICAL read-only.  This should not prevent
       ;; deduplication from inserting its hard link.
       (chmod (dirname (second identical)) #o544)

       (call-with-output-file unique
         (lambda (port)
           (put-bytevector port (string->utf8 (string-reverse data)))))

       (deduplicate store (nar-sha256 store) #:store store)

       ;; (system (string-append "ls -lRia " store))
       (cons* (apply = (map (compose stat:ino stat) identical))
              (= (stat:ino (stat unique))
                 (stat:ino (stat (car identical))))
              (stat:nlink (stat unique))
              (map (compose stat:nlink stat) identical))))))

(test-equal "deduplicate, ENOSPC"
  (cons* #f                                       ;inode comparison
         (append (make-list 3 4)
                 (make-list 7 1)))                ;'nlink' values

  ;; In this scenario the first 3 files are properly deduplicated and then we
  ;; simulate a full '.links' directory where link(2) gets ENOSPC, thereby
  ;; preventing deduplication of the subsequent files.
  (call-with-temporary-directory
   (lambda (store)
     (let ((true-link link)
           (links     0)
           (data1     (string->utf8
                       (string-concatenate (make-list 1000 "Hello, world!"))))
           (data2     (string->utf8
                       (string-concatenate (make-list 1000 "Hi, world!"))))
           (identical (map (lambda (n)
                             (string-append store "/" (number->string n)
                                            "/a/b/c"))
                           (iota 10)))
           (populate  (lambda (data)
                        (lambda (file)
                          (mkdir-p (dirname file))
                          (call-with-output-file file
                            (lambda (port)
                              (put-bytevector port data)))))))
       (for-each (populate data1) (take identical 5))
       (for-each (populate data2) (drop identical 5))
       (dynamic-wind
         (lambda ()
           (set! link (lambda (old new)
                        (set! links (+ links 1))
                        (if (<= links 4)
                            (true-link old new)
                            (throw 'system-error "link" "~A" '("Whaaat?!")
                                   (list ENOSPC))))))
         (lambda ()
           (deduplicate store (nar-sha256 store) #:store store))
         (lambda ()
           (set! link true-link)))

       (cons (apply = (map (compose stat:ino stat) identical))
             (map (compose stat:nlink stat) identical))))))

(test-assert "copy-file/deduplicate, below %deduplication-minimum-size"
  (call-with-temporary-directory
   (lambda (store)
     (let ((source (string-append store "/input")))
       (call-with-output-file source
         (lambda (port)
           (display "Hello!\n" port)))
       (copy-file/deduplicate source
                              (string-append store "/a")
                              #:store store)
       (and (not (directory-exists? (string-append store "/.links")))
            (file=? source (string-append store "/a"))
            (not (= (stat:ino (stat (string-append store "/a")))
                    (stat:ino (stat source)))))))))

(test-assert "copy-file/deduplicate"
  (call-with-temporary-directory
   (lambda (store)
     (let ((source (search-path %load-path "gnu/packages/emacs-xyz.scm")))
       (for-each (lambda (target)
                   (copy-file/deduplicate source
                                          (string-append store target)
                                          #:store store))
                 '("/a" "/b" "/c"))
       (and (directory-exists? (string-append store "/.links"))
            (file=? source (string-append store "/a"))
            (apply = (map (compose stat:ino stat
                                   (cut string-append store <>))
                          '("/a" "/b" "/c"))))))))

(for-each (match-lambda
            ((initial-gap middle-gap final-gap)
             (test-assert
                 (format #f "copy-file/deduplicate, sparse files (holes: ~a/~a/~a)"
                         initial-gap middle-gap final-gap)
               (call-with-temporary-directory
                (lambda (store)
                  (let ((source (string-append store "/source")))
                    (call-with-output-file source
                      (lambda (port)
                        (seek port initial-gap SEEK_CUR)
                        (display "hi!" port)
                        (seek port middle-gap SEEK_CUR)
                        (display "bye." port)
                        (when (> final-gap 0)
                          (seek port (- final-gap 1) SEEK_CUR)
                          (put-u8 port 0))))

                    (for-each (lambda (target)
                                (copy-file/deduplicate source
                                                       (string-append store target)
                                                       #:store store))
                              '("/a" "/b" "/c"))
                    (system* "du" "-h" source)
                    (system* "du" "-h" "--apparent-size" source)
                    (system* "du" "-h" (string-append store "/a"))
                    (system* "du" "-h" "--apparent-size" (string-append store "/a"))
                    (and (directory-exists? (string-append store "/.links"))
                         (file=? source (string-append store "/a"))
                         (apply = (map (compose stat:ino stat
                                                (cut string-append store <>))
                                       '("/a" "/b" "/c")))
                         (let ((st (pk 'S (stat (string-append store "/a")))))
                           (<= (* 512 (stat:blocks st))
                               (stat:size st))))))))))
          (cartesian-product '(0 3333 8192)
                             '(8192 9999 16384 22222)
                             '(0 8192)))

(test-end "store-deduplication")
