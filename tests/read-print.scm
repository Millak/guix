;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021-2022 Ludovic Courtès <ludo@gnu.org>
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

(define-module (tests-style)
  #:use-module (guix read-print)
  #:use-module (guix gexp)                        ;for the reader extensions
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 match))

(define-syntax-rule (test-pretty-print str args ...)
  "Test equality after a round-trip where STR is passed to
'read-with-comments' and the resulting sexp is then passed to
'pretty-print-with-comments'."
  (test-equal str
    (call-with-output-string
      (lambda (port)
        (let ((exp (call-with-input-string str
                     read-with-comments)))
         (pretty-print-with-comments port exp args ...))))))

(define-syntax-rule (test-pretty-print/sequence str args ...)
  "Likewise, but read and print entire sequences rather than individual
expressions."
  (test-equal str
    (call-with-output-string
      (lambda (port)
        (let ((lst (call-with-input-string str
                     read-with-comments/sequence)))
         (pretty-print-with-comments/splice port lst args ...))))))


(test-begin "read-print")

(test-assert "read-with-comments: missing closing paren"
  (guard (c ((error? c) #t))
    (call-with-input-string "(what is going on?"
      read-with-comments)))

(test-equal "read-with-comments: dot notation"
  (cons 'a 'b)
  (call-with-input-string "(a . b)"
    read-with-comments))

(test-equal "read-with-comments: list with blank line"
  `(list with ,(vertical-space 1) blank line)
  (call-with-input-string "\
(list with

      blank line)\n"
    read-with-comments))

(test-equal "read-with-comments: list with multiple blank lines"
  `(list with ,(comment ";multiple\n" #t)
         ,(vertical-space 3) blank lines)
  (call-with-input-string "\
(list with ;multiple



      blank lines)\n"
    read-with-comments))

(test-equal "read-with-comments: top-level blank lines"
  (list (vertical-space 2) '(a b c) (vertical-space 2))
  (call-with-input-string "

(a b c)\n\n"
    (lambda (port)
      (list (read-with-comments port)
            (read-with-comments port)
            (read-with-comments port)))))

(test-equal "read-with-comments: top-level page break"
  (list (comment ";; Begin.\n") (vertical-space 1)
        (page-break)
        (comment ";; End.\n"))
  (call-with-input-string "\
;; Begin.


;; End.\n"
    (lambda (port)
      (list (read-with-comments port)
            (read-with-comments port)
            (read-with-comments port)
            (read-with-comments port)))))

(test-pretty-print "(list 1 2 3 4)")
(test-pretty-print "((a . 1) (b . 2))")
(test-pretty-print "(a b c . boom)")
(test-pretty-print "(list 1
                          2
                          3
                          4)"
                   #:long-list 3
                   #:indent 20)
(test-pretty-print "\
(list abc
      def)"
                   #:max-width 11)
(test-pretty-print "\
(#:foo
 #:bar)"
                   #:max-width 10)

(test-pretty-print "\
(#:first 1
 #:second 2
 #:third 3)")

(test-pretty-print "\
((x
  1)
 (y
  2)
 (z
  3))"
                   #:max-width 3)

(test-pretty-print "\
(let ((x 1)
      (y 2)
      (z 3)
      (p 4))
  (+ x y))"
                   #:max-width 11)

(test-pretty-print "\
(lambda (x y)
  ;; This is a procedure.
  (let ((z (+ x y)))
    (* z z)))")

(test-pretty-print "\
(case x
  ((1)
   'one)
  ((2)
   'two))")

(test-pretty-print "\
(cond
  ((zero? x)
   'zero)
  ((odd? x)
   'odd)
  (else #f))")

(test-pretty-print "\
#~(string-append #$coreutils \"/bin/uname\")")

(test-pretty-print "\
(package
  (inherit coreutils)
  (version \"42\"))")

(test-pretty-print "\
(modify-phases %standard-phases
  (add-after 'unpack 'post-unpack
    (lambda _
      #t))
  (add-before 'check 'pre-check
    (lambda* (#:key inputs #:allow-other-keys)
      do things ...)))")

(test-pretty-print "\
(#:phases (modify-phases sdfsdf
            (add-before 'x 'y
              (lambda _
                xyz))))")

(test-pretty-print "\
(string-append \"a\\tb\" \"\\n\")")

(test-pretty-print "\
(description \"abcdefghijkl
mnopqrstuvwxyz.\")"
                   #:max-width 30)

(test-pretty-print "\
(description
 \"abcdefghijkl
mnopqrstuvwxyz.\")"
                   #:max-width 12)

(test-pretty-print "\
(description
 \"abcdefghijklmnopqrstuvwxyz\")"
                   #:max-width 33)

(test-pretty-print "\
(modify-phases %standard-phases
  (replace 'build
    ;; Nicely indented in 'modify-phases' context.
    (lambda _
      #t)))")

(test-pretty-print "\
(modify-inputs inputs
  ;; Regular indentation for 'replace' here.
  (replace \"gmp\" gmp))")

(test-pretty-print "\
(package
  ;; Here 'sha256', 'base32', and 'arguments' must be
  ;; immediately followed by a newline.
  (source (origin
            (method url-fetch)
            (sha256
             (base32
              \"not a real base32 string\"))))
  (arguments
   '(#:phases %standard-phases
     #:tests? #f)))")

;; '#:key value' is kept on the same line.
(test-pretty-print "\
(package
  (name \"keyword-value-same-line\")
  (arguments
   (list #:phases #~(modify-phases %standard-phases
                      (add-before 'x 'y
                        (lambda* (#:key inputs #:allow-other-keys)
                          (foo bar baz))))
         #:make-flags #~'(\"ANSWER=42\")
         #:tests? #f)))")

(test-pretty-print "\
(let ((x 1)
      (y 2)
      (z (let* ((a 3)
                (b 4))
           (+ a b))))
  (list x y z))")

(test-pretty-print "\
(begin
  (chmod \"foo\" #o750)
  (chmod port
         (logand #o644
                 (lognot (umask))))
  (logand #x7f xyz))")

(test-pretty-print "\
(substitute-keyword-arguments (package-arguments x)
  ((#:phases phases)
   `(modify-phases ,phases
      (add-before 'build 'do-things
        (lambda _
          #t))))
  ((#:configure-flags flags)
   `(cons \"--without-any-problem\"
          ,flags)))")

(test-pretty-print "\
(vertical-space one:

                two:


                three:



                end)")

(test-pretty-print "\
(vertical-space one

                ;; Comment after blank line.
                two)")

(test-pretty-print "\
(begin
  break

  ;; page break above
  end)")

(test-pretty-print/sequence "\
;;; This is a top-level comment.


;; Above is a page break.
(this is an sexp
      ;; with a comment
      !!)

;; The end.\n")

(test-pretty-print/sequence "
;;; Hello!
;;; Notice that there are three semicolons here.

(define-module (foo bar)
  #:use-module (guix)
  #:use-module (gnu))


;; And now, the OS.
(operating-system
  (host-name \"komputilo\")
  (locale \"eo_EO.UTF-8\")

  (services
   (cons (service mcron-service-type) %base-services)))\n"
                            #:format-comment canonicalize-comment)

(test-equal "pretty-print-with-comments, canonicalize-comment"
  "\
(list abc
      ;; Not a margin comment.
      ;; Ditto.
      ;;
      ;; There's a blank line above.
      def ;margin comment
      ghi)"
  (let ((sexp (call-with-input-string
                  "\
(list abc
  ;Not a margin comment.
  ;;;  Ditto.
  ;;;;;
  ; There's a blank line above.
  def  ;; margin comment
  ghi)"
                read-with-comments)))
    (call-with-output-string
      (lambda (port)
        (pretty-print-with-comments port sexp
                                    #:format-comment
                                    canonicalize-comment)))))

(test-equal "pretty-print-with-comments, canonicalize-vertical-space"
  "\
(list abc

      def

      ;; last one
      ghi)"
  (let ((sexp (call-with-input-string
                  "\
(list abc



  def


;; last one
  ghi)"
                read-with-comments)))
    (call-with-output-string
      (lambda (port)
        (pretty-print-with-comments port sexp
                                    #:format-vertical-space
                                    canonicalize-vertical-space)))))

(test-equal "pretty-print-with-comments, multi-line comment"
  "\
(list abc
      ;; This comment spans
      ;; two lines.
      def)"
  (call-with-output-string
    (lambda (port)
      (pretty-print-with-comments port
                                  `(list abc ,(comment "\
;; This comment spans\n
;; two lines.\n")
                                         def)))))

(test-end)
