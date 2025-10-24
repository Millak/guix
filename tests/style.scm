;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021-2024 Ludovic Courtès <ludo@gnu.org>
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
  #:use-module ((gcrypt hash) #:select (port-sha256))
  #:use-module (guix packages)
  #:use-module (guix scripts style)
  #:use-module ((guix utils) #:select (call-with-temporary-directory))
  #:use-module ((guix build utils) #:select (substitute*))
  #:use-module (guix gexp)                        ;for the reader extension
  #:use-module (guix diagnostics)
  #:use-module (guix git)
  #:use-module (guix tests)
  #:use-module (guix tests git)
  #:use-module (gnu packages)
  #:use-module (gnu packages acl)
  #:use-module (gnu packages multiprecision)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 vlist))

(define* (call-with-test-package inputs proc #:optional suffix)
  (let ((module-name (if suffix
                         (string-append "my-packages-" suffix)
                         "my-packages"))
        (name (if suffix
                  (string-append "my-coreutils-" suffix)
                  "my-coreutils")))
    (call-with-temporary-directory
     (lambda (directory)
       (call-with-output-file (string-append directory "/" module-name ".scm")
         (lambda (port)
           (pretty-print
            `(begin
               (define-module (,(string->symbol module-name))
                 #:use-module (guix)
                 #:use-module (guix git-download) ; for -S git-source
                 #:use-module ((gnu packages) #:select (search-patches))
                 #:use-module (guix licenses)
                 #:use-module (gnu packages acl)
                 #:use-module (gnu packages base)
                 #:use-module (gnu packages multiprecision)
                 #:use-module (srfi srfi-1))

               (define base
                 (package
                   (inherit coreutils)
                   (inputs '())
                   (native-inputs '())
                   (propagated-inputs '())))

               (define (sdl-union . lst)
                 (package
                   (inherit base)
                   (name "sdl-union")))

               (define-public ,(string->symbol name)
                 (package
                   (inherit base)
                   (name ,name)
                   ,@inputs
                   ;; XXX: The field below was added so that the 'inputs'
                   ;; field doesn't come last; if it did, 'read-package-field'
                   ;; in the tests below would read the three closing parens
                   ;; for each test.
                   (properties '()))))
            port)))

       (proc directory)))))

(define test-directory
  ;; Directory where the package definition lives.
  (make-parameter #f))

(define-syntax-rule (with-test-package fields exp ...)
  (call-with-test-package fields
    (lambda (directory)
      (define file
        (string-append directory "/my-packages.scm"))

      ;; Run as a separate process to make sure FILE is reloaded.
      (system* "guix" "style" "-L" directory "-S" "inputs"
               "my-coreutils")
      (system* "cat" file)

      (load file)
      (parameterize ((test-directory directory))
        exp ...))))

(define* (read-lines port line #:optional (count 1))
  "Read COUNT lines from PORT, starting from LINE."
  (let loop ((lines '())
             (count count))
    (cond ((< (port-line port) (- line 1))
           (read-char port)
           (loop lines count))
          ((zero? count)
           (string-concatenate-reverse lines))
          (else
           (match (read-line port 'concat)
             ((? eof-object?)
              (loop lines 0))
             (line
              (loop (cons line lines) (- count 1))))))))

(define* (read-package-field package field #:optional (count 1))
  (let* ((location (package-field-location package field))
         (file (location-file location))
         (line (location-line location)))
    (call-with-input-file (if (string-prefix? "/" file)
                              file
                              (string-append (test-directory) "/"
                                             file))
      (lambda (port)
        (read-lines port line count)))))


(test-begin "style")

(test-equal "nothing to rewrite"
  '()
  (with-test-package '()
    (package-direct-inputs (@ (my-packages) my-coreutils))))

(test-equal "input labels, mismatch"
  (list `(("foo" ,gmp) ("bar" ,acl))
        "      (inputs `((\"foo\" ,gmp) (\"bar\" ,acl)))\n")
  (with-test-package '((inputs `(("foo" ,gmp) ("bar" ,acl))))
    (list (package-direct-inputs (@ (my-packages) my-coreutils))
          (read-package-field (@ (my-packages) my-coreutils) 'inputs))))

(test-equal "input labels, simple"
  (list `(("gmp" ,gmp) ("acl" ,acl))
        "      (inputs (list gmp acl))\n")
  (with-test-package '((inputs `(("gmp" ,gmp) ("acl" ,acl))))
    (list (package-direct-inputs (@ (my-packages) my-coreutils))
          (read-package-field (@ (my-packages) my-coreutils) 'inputs))))

(test-equal "input labels, long list with one item per line"
  (list (concatenate (make-list 4 `(("gmp" ,gmp) ("acl" ,acl))))
        "\
        (list gmp
              acl
              gmp
              acl
              gmp
              acl
              gmp
              acl))\n")
  (with-test-package '((inputs `(("gmp" ,gmp) ("acl" ,acl)
                                 ("gmp" ,gmp) ("acl" ,acl)
                                 ("gmp" ,gmp) ("acl" ,acl)
                                 ("gmp" ,gmp) ("acl" ,acl))))
    (list (package-direct-inputs (@ (my-packages) my-coreutils))
          (read-package-field (@ (my-packages) my-coreutils) 'inputs 8))))

(test-equal "input labels, sdl-union"
  "\
        (list gmp acl
              (sdl-union 1 2 3 4)))\n"
  (with-test-package '((inputs `(("gmp" ,gmp) ("acl" ,acl)
                                 ("sdl-union" ,(sdl-union 1 2 3 4)))))
    (read-package-field (@ (my-packages) my-coreutils) 'inputs 2)))

(test-equal "input labels, output"
  (list `(("gmp" ,gmp "debug") ("acl" ,acl))
        "      (inputs (list `(,gmp \"debug\") acl))\n")
  (with-test-package '((inputs `(("gmp" ,gmp "debug") ("acl" ,acl))))
    (list (package-direct-inputs (@ (my-packages) my-coreutils))
          (read-package-field (@ (my-packages) my-coreutils) 'inputs))))

(test-equal "input labels, prepend"
  (list `(("gmp" ,gmp) ("acl" ,acl))
        "\
        (modify-inputs (package-propagated-inputs coreutils)
          (prepend gmp acl)))\n")
  (with-test-package '((inputs `(("gmp" ,gmp) ("acl" ,acl)
                                 ,@(package-propagated-inputs coreutils))))
    (list (package-inputs (@ (my-packages) my-coreutils))
          (read-package-field (@ (my-packages) my-coreutils) 'inputs 2))))

(test-equal "input labels, prepend + delete"
  (list `(("gmp" ,gmp) ("acl" ,acl))
        "\
        (modify-inputs (package-propagated-inputs coreutils)
          (delete \"gmp\")
          (prepend gmp acl)))\n")
  (with-test-package '((inputs `(("gmp" ,gmp)
                                 ("acl" ,acl)
                                 ,@(alist-delete "gmp"
                                                 (package-propagated-inputs coreutils)))))
    (list (package-inputs (@ (my-packages) my-coreutils))
          (read-package-field (@ (my-packages) my-coreutils) 'inputs 3))))

(test-equal "input labels, prepend + delete multiple"
  (list `(("gmp" ,gmp) ("acl" ,acl))
        "\
        (modify-inputs (package-propagated-inputs coreutils)
          (delete \"foo\" \"bar\" \"baz\")
          (prepend gmp acl)))\n")
  (with-test-package '((inputs `(("gmp" ,gmp)
                                 ("acl" ,acl)
                                 ,@(fold alist-delete
                                         (package-propagated-inputs coreutils)
                                         '("foo" "bar" "baz")))))
    (list (package-inputs (@ (my-packages) my-coreutils))
          (read-package-field (@ (my-packages) my-coreutils) 'inputs 3))))

(test-equal "input labels, replace"
  (list '()                                 ;there's no "gmp" input to replace
        "\
        (modify-inputs (package-propagated-inputs coreutils)
          (replace \"gmp\" gmp)))\n")
  (with-test-package '((inputs `(("gmp" ,gmp)
                                 ,@(alist-delete "gmp"
                                                 (package-propagated-inputs coreutils)))))
    (list (package-inputs (@ (my-packages) my-coreutils))
          (read-package-field (@ (my-packages) my-coreutils) 'inputs 2))))

(test-equal "input labels, 'safe' policy"
  (list `(("gmp" ,gmp) ("acl" ,acl))
        "\
      (inputs (list gmp acl))\n")
  (call-with-test-package '((inputs `(("GMP" ,gmp) ("ACL" ,acl)))
                            (arguments '()))      ;no build system arguments
    (lambda (directory)
      (define file
        (string-append directory "/my-packages.scm"))

      (system* "guix" "style" "-L" directory "my-coreutils"
               "-S" "inputs"
               "--input-simplification=safe")

      (load file)
      (list (package-inputs (@ (my-packages) my-coreutils))
            (read-package-field (@ (my-packages) my-coreutils) 'inputs)))))

(test-equal "input labels, 'safe' policy, trivial arguments"
  (list `(("gmp" ,gmp) ("mpfr" ,mpfr))
        "\
      (inputs (list gmp mpfr))\n")
  (call-with-test-package '((inputs `(("GMP" ,gmp) ("Mpfr" ,mpfr)))
                            (arguments            ;"trivial" arguments
                             '(#:tests? #f
                               #:test-target "whatever")))
    (lambda (directory)
      (define file
        (string-append directory "/my-packages.scm"))

      (system* "guix" "style" "-L" directory "my-coreutils"
               "-S" "inputs"
               "--input-simplification=safe")

      (load file)
      (list (package-inputs (@ (my-packages) my-coreutils))
            (read-package-field (@ (my-packages) my-coreutils) 'inputs)))))

(test-equal "input labels, 'safe' policy, nothing changed"
  (list `(("GMP" ,gmp) ("ACL" ,acl))
        "\
      (inputs `((\"GMP\" ,gmp) (\"ACL\" ,acl)))\n")
  (call-with-test-package '((inputs `(("GMP" ,gmp) ("ACL" ,acl)))
                            ;; Non-empty argument list, so potentially unsafe
                            ;; input simplification.
                            (arguments
                             '(#:configure-flags
                               (assoc-ref %build-inputs "GMP"))))
    (lambda (directory)
      (define file
        (string-append directory "/my-packages.scm"))

      (system* "guix" "style" "-L" directory "my-coreutils"
               "-S" "inputs"
               "--input-simplification=safe")

      (load file)
      (list (package-inputs (@ (my-packages) my-coreutils))
            (read-package-field (@ (my-packages) my-coreutils) 'inputs)))))

(test-equal "input labels, margin comment"
  (list `(("gmp" ,gmp))
        `(("acl" ,acl))
        "      (inputs (list gmp)) ;margin comment\n"
        "      (native-inputs (list acl)) ;another one\n")
  (call-with-test-package '((inputs `(("gmp" ,gmp)))
                            (native-inputs `(("acl" ,acl))))
    (lambda (directory)
      (define file
        (string-append directory "/my-packages.scm"))

      (substitute* file
        (("\"gmp\"(.*)$" _ rest)
         (string-append "\"gmp\"" (string-trim-right rest)
                        " ;margin comment\n"))
        (("\"acl\"(.*)$" _ rest)
         (string-append "\"acl\"" (string-trim-right rest)
                        " ;another one\n")))
      (system* "cat" file)

      (system* "guix" "style" "-L" directory "-S" "inputs"
               "my-coreutils")

      (load file)
      (list (package-inputs (@ (my-packages) my-coreutils))
            (package-native-inputs (@ (my-packages) my-coreutils))
            (read-package-field (@ (my-packages) my-coreutils) 'inputs)
            (read-package-field (@ (my-packages) my-coreutils) 'native-inputs)))))

(test-equal "input labels, margin comment on long list"
  (list (concatenate (make-list 4 `(("gmp" ,gmp) ("acl" ,acl))))
        "\
        (list gmp ;margin comment
              acl
              gmp ;margin comment
              acl
              gmp ;margin comment
              acl
              gmp ;margin comment
              acl))\n")
  (call-with-test-package '((inputs `(("gmp" ,gmp) ("acl" ,acl)
                                      ("gmp" ,gmp) ("acl" ,acl)
                                      ("gmp" ,gmp) ("acl" ,acl)
                                      ("gmp" ,gmp) ("acl" ,acl))))
    (lambda (directory)
      (define file
        (string-append directory "/my-packages.scm"))

      (substitute* file
        (("\"gmp\"(.*)$" _ rest)
         (string-append "\"gmp\"" (string-trim-right rest)
                        " ;margin comment\n")))
      (system* "cat" file)

      (system* "guix" "style" "-L" directory "-S" "inputs"
               "my-coreutils")

      (load file)
      (list (package-inputs (@ (my-packages) my-coreutils))
            (read-package-field (@ (my-packages) my-coreutils) 'inputs 8)))))

(test-equal "input labels, line comment"
  (list `(("gmp" ,gmp) ("acl" ,acl))
        "\
      (inputs (list gmp
                    ;; line comment!
                    acl))\n")
  (call-with-test-package '((inputs `(("gmp" ,gmp) ("acl" ,acl))))
    (lambda (directory)
      (define file
        (string-append directory "/my-packages.scm"))

      (substitute* file
        ((",gmp\\)(.*)$" _ rest)
         (string-append ",gmp)\n   ;; line comment!\n" rest)))

      (system* "guix" "style" "-L" directory "-S" "inputs"
               "my-coreutils")

      (load file)
      (list (package-inputs (@ (my-packages) my-coreutils))
            (read-package-field (@ (my-packages) my-coreutils) 'inputs 3)))))

(test-equal "input labels, modify-inputs and margin comment"
  (list `(("gmp" ,gmp) ("acl" ,acl) ("mpfr" ,mpfr))
        "\
        (modify-inputs (package-propagated-inputs coreutils)
          (prepend gmp ;margin comment
                   acl ;another one
                   mpfr)))\n")
  (call-with-test-package '((inputs
                             `(("gmp" ,gmp) ("acl" ,acl) ("mpfr" ,mpfr)
                               ,@(package-propagated-inputs coreutils))))
    (lambda (directory)
      (define file
        (string-append directory "/my-packages.scm"))

      (substitute* file
        ((",gmp\\)(.*)$" _ rest)
         (string-append ",gmp) ;margin comment" rest))
        ((",acl\\)(.*)$" _ rest)
         (string-append ",acl) ;another one" rest)))

      (system* "guix" "style" "-L" directory "-S" "inputs"
               "my-coreutils")

      (load file)
      (list (package-inputs (@ (my-packages) my-coreutils))
            (read-package-field (@ (my-packages) my-coreutils) 'inputs 4)))))

(test-assert "gexpify arguments, already gexpified"
  (call-with-test-package '((arguments
                             (list #:configure-flags #~'("--help"))))
    (lambda (directory)
      (define file
        (string-append directory "/my-packages.scm"))
      (define (fingerprint file)
        (let ((stat (stat file)))
          (list (stat:mtime stat) (stat:size stat))))
      (define before
        (fingerprint file))

      (system* "guix" "style" "-L" directory "my-coreutils"
               "-S" "arguments")

      (equal? (fingerprint file) before))))

(test-equal "gexpify arguments, non-gexp arguments, margin comment"
  (list (list #:tests? #f #:test-target "check")
        "\
      (arguments (list #:tests? #f ;no tests
                       #:test-target \"check\"))\n")
  (call-with-test-package '((arguments
                             '(#:tests? #f
                               #:test-target "check")))
    (lambda (directory)
      (define file
        (string-append directory "/my-packages.scm"))

      (substitute* file
        (("#:tests\\? #f" all)
         (string-append all " ;no tests\n")))

      (system* "guix" "style" "-L" directory "my-coreutils"
               "-S" "arguments")

      (load file)
      (list (package-arguments (@ (my-packages) my-coreutils))
            (read-package-field (@ (my-packages) my-coreutils) 'arguments 2)))))

(test-equal "gexpify arguments, phases and flags"
  "\
        (list #:tests? #f
              #:configure-flags #~'(\"--fast\")
              #:make-flags #~(list (string-append \"CC=\"
                                                  #$(cc-for-target)))
              #:phases #~(modify-phases %standard-phases
                           ;; Line comment.
                           whatever)))\n"
  (call-with-test-package '((arguments
                             `(#:tests? #f
                               #:configure-flags '("--fast")
                               #:make-flags
                               (list (string-append "CC=" ,(cc-for-target)))
                               #:phases (modify-phases %standard-phases
                                          whatever))))
    (lambda (directory)
      (define file
        (string-append directory "/my-packages.scm"))

      (substitute* file
        (("whatever")
         "\n;; Line comment.
         whatever"))
      (system* "guix" "style" "-L" directory "my-coreutils"
               "-S" "arguments")

      (load file)
      (read-package-field (@ (my-packages) my-coreutils) 'arguments 7))))

(test-equal "gexpify arguments, append arguments"
  "\
        (append (list #:tests? #f
                      #:configure-flags #~'(\"--fast\"))
                (package-arguments coreutils)))\n"
  (call-with-test-package '((arguments
                             `(#:tests? #f
                               #:configure-flags '("--fast")
                               ,@(package-arguments coreutils))))
    (lambda (directory)
      (define file
        (string-append directory "/my-packages.scm"))

      (system* "guix" "style" "-L" directory "my-coreutils"
               "-S" "arguments")

      (load file)
      (read-package-field (@ (my-packages) my-coreutils) 'arguments 3))))

(test-equal "gexpify arguments, substitute-keyword-arguments"
  "\
        (substitute-keyword-arguments (package-arguments coreutils)
          ((#:tests? _ #f)
           #t)
          ((#:make-flags flags
            #~'())
           #~(cons \"-DXYZ=yes\"
                   #$flags))))\n"
  (call-with-test-package '((arguments
                             (substitute-keyword-arguments
                                 (package-arguments coreutils)
                               ((#:tests? _ #f) #t)
                               ((#:make-flags flags ''())
                                `(cons "-DXYZ=yes" ,flags)))))
    (lambda (directory)
      (define file
        (string-append directory "/my-packages.scm"))

      (system* "guix" "style" "-L" directory "my-coreutils"
               "-S" "arguments")

      (load file)
      (read-package-field (@ (my-packages) my-coreutils) 'arguments 7))))

(test-equal "gexpify arguments, substitute-keyword-arguments + unquote-splicing"
  "\
        (substitute-keyword-arguments (package-arguments coreutils)
          ((#:make-flags flags
            #~'())
           #~(cons \"-DXYZ=yes\"
                   #$@(if #t flags
                          '())))))\n"
  (call-with-test-package '((arguments
                             (substitute-keyword-arguments
                                 (package-arguments coreutils)
                               ((#:make-flags flags ''())
                                `(cons "-DXYZ=yes" ,@(if #t flags '()))))))
    (lambda (directory)
      (define file
        (string-append directory "/my-packages.scm"))

      (system* "guix" "style" "-L" directory "my-coreutils"
               "-S" "arguments")

      (load file)
      (read-package-field (@ (my-packages) my-coreutils) 'arguments 6))))

(test-equal "gexpify arguments, append substitute-keyword-arguments"
  "\
        (append (list #:tests? #f)
                (substitute-keyword-arguments (package-arguments coreutils)
                  ((#:make-flags flags)
                   #~(append `(\"-n\" ,%output)
                             #$flags)))))\n"
  (call-with-test-package '((arguments
                             `(#:tests? #f
                               ,@(substitute-keyword-arguments
                                     (package-arguments coreutils)
                                   ((#:make-flags flags)
                                    `(append `("-n" ,%output) ,flags))))))
    (lambda (directory)
      (define file
        (string-append directory "/my-packages.scm"))

      (system* "guix" "style" "-L" directory "my-coreutils"
               "-S" "arguments")

      (load file)
      (read-package-field (@ (my-packages) my-coreutils) 'arguments 5))))

;;;
;;; url-fetch->git-fetch transformation
;;;

(test-equal "url-fetch->git-fetch, basic transformation"
  `(origin
     (method git-fetch)
     (uri (git-reference (url "https://github.com/foo/bar")
                         (commit version)))
     (file-name (git-file-name name version))
     (sha256
      (base32 "0j8vhvfj1d3jvbrd4kh20m50knmwj19xk0l3s78z1xxayp3c5zkk")))
  (call-with-test-package
      '((home-page "@substitute-me@")
        (version "1.0")
        (source
         (origin
           (method url-fetch)
           (uri (string-append "https://example.com/foo-" version ".tar.gz"))
           (sha256
            (base32 "0000000000000000000000000000000000000000000000000000")))))
    (lambda (directory)
      (define file
        (string-append directory "/my-packages-0.scm"))

      (parameterize ((test-directory directory))
        (with-temporary-git-repository repository
            `((add "README" "Initial commit")
              (commit "First commit")
              (tag "1.0" "Initial release"))
          (mock ((guix import utils) git-repository-url? (const #t))
                (substitute* file
                  (("@substitute-me@")
                   (string-append "file://" repository)))
                ;; XXX: Calling guix-style is necessary to use mock.
                (guix-style "-L" directory "-S" "git-source" "my-coreutils-0")
                (substitute* file
                  (((string-append "file://" repository))
                   "https://github.com/foo/bar"))

                (load file)
                (and=> (false-if-exception
                        (read-package-field (@ (my-packages-0) my-coreutils-0) 'source 8))
                       (cut call-with-input-string <> read))))))
    "0"))

(test-assert "url-fetch->git-fetch, preserved field"
  (call-with-test-package
      '((home-page "@substitute-me@")
        (version "1.0")
        (source
         (origin
           (method url-fetch)
           (uri "https://example.com/foo.tar.gz")
           (sha256
            (base32 "0000000000000000000000000000000000000000000000000000"))
           (patches (search-patches "foo.patch")))))
    (lambda (directory)
      (define file
        (string-append directory "/my-packages-1.scm"))
      (call-with-output-file (string-append directory "/foo.patch")
        (const #t))

      (parameterize ((test-directory directory)
                     (%patch-path (list directory))
                     (%package-module-path (list directory "")))
        (with-temporary-git-repository repository
            `((add "README" "Initial commit")
              (commit "First commit")
              (tag "1.0" "Initial release"))
          (mock ((guix import utils) git-repository-url? (const #t))
                (mock ((gnu packages) specification->package
                       (lambda (spec)
                         (car
                          (vhash-fold* cons '() spec
                                       (fold-packages
                                        (lambda (p r)
                                          (vhash-cons (package-name p) p r))
                                        vlist-null)))))
                      (substitute* file
                        (("@substitute-me@")
                         (string-append "file://" repository)))
                      ;; XXX: Calling guix-style is necessary to use mock.
                      (guix-style "-L" directory "-S" "git-source" "my-coreutils-1")
                      (substitute* file
                        (((string-append "file://" repository))
                         "https://github.com/foo/bar"))
                      (load file)
                      (and=> (read-package-field
                              (@ (my-packages-1) my-coreutils-1) 'source 8)
                             (cut string-contains <> "patches")))))))
    "1"))

(test-assert "url-fetch->git-fetch, non-git home-page unchanged"
  (call-with-test-package
      '((home-page "https://www.example.com")
        (source
         (origin
           (method url-fetch)
           (uri "https://example.com/foo.tar.gz")
           (sha256
            (base32 "0000000000000000000000000000000000000000000000000000")))))
    (lambda (directory)
      (define file
        (string-append directory "/my-packages.scm"))
      (define before-hash
        (call-with-input-file file port-sha256))

      (system* "guix" "style" "-L" directory "-S" "git-source"
               "my-coreutils")

      ;; File should be unchanged
      (equal? (call-with-input-file file port-sha256) before-hash))))

(test-assert "url-fetch->git-fetch, already git-fetch unchanged"
  (call-with-test-package
      '((home-page "https://github.com/foo/bar")
        (source
         (origin
           (method git-fetch)
           (uri (git-reference
                  (url "https://github.com/foo/bar")
                  (commit version)))
           (file-name (git-file-name name version))
           (sha256
            (base32 "0000000000000000000000000000000000000000000000000000")))))
    (lambda (directory)
      (define file
        (string-append directory "/my-packages.scm"))
      (define before-hash
        (call-with-input-file file port-sha256))

      (system* "guix" "style" "-L" directory "-S" "git-source"
               "my-coreutils")

      ;; File should be unchanged
      (equal? (call-with-input-file file port-sha256) before-hash))))

(test-end)

;; Local Variables:
;; eval: (put 'with-test-package 'scheme-indent-function 1)
;; eval: (put 'call-with-test-package 'scheme-indent-function 1)
;; End:
