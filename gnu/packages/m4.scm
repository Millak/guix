;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2015 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2023, 2024 Janneke Nieuwenhuizen <janneke@gnu.org>
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

(define-module (gnu packages m4)
  #:use-module (guix licenses)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix utils))

(define-public m4
  (package
   (name "m4")
   (version "1.4.20")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/m4/m4-"
                                version ".tar.xz"))
            (sha256
             (base32
              "0axgilr6n88br7msm3ls8m4zlwgk4q3vcjqw1cknqpyg3hxfldp2"))))
   (build-system gnu-build-system)
   (arguments
    `(;; Explicitly disable tests when cross-compiling, otherwise 'make check'
      ;; proceeds and fails, unsurprisingly.
      #:tests? ,(not (%current-target-system))
      ,@(if (system-hurd64?)
            (list #:configure-flags
                  `'(,(string-append
                       "CFLAGS=-g -O2"
                       " -Wno-error=implicit-function-declaration")))
            '())
      #:phases
      (modify-phases %standard-phases
        (add-after 'unpack 'disable-test
          (lambda _
            ;; Test 5 raises SIGINT from a child and immediately returns
            ;; code 71, and tests whether the child was killed by a signal.
            ;; Since there is no signal handler for SIGINT in the build
            ;; container, the parent sees the return code, and fails.
            ;; XXX: For some reason adding signal handlers in Guile before
            ;; running tests has no effect.
            (substitute* "tests/test-execute.sh"
              (("4 5 6")
               "4 6"))))
        ,@(if (target-hurd?)
              '((add-after 'unpack 'skip-stack-overflow-tests
                  (lambda _
                    (substitute* '("tests/test-sigsegv-catch-stackoverflow1.c"
                                   "tests/test-sigsegv-catch-stackoverflow2.c")
                      (("(^| )main *\\(.*" all)
                       (string-append all "{\n  exit (77);//"))))))
              '())
        ,@(if (target-hurd64?)
              '((add-after 'unpack 'patch-sigsegv
                  (lambda _
                    ;; Stack overflow recovery does not compile
                    (substitute* "lib/sigsegv.in.h"
                      (("__GNU__") "__XGNU__")))))
              '())
        (add-after 'unpack 'configure-shell
          (lambda* (#:key native-inputs inputs #:allow-other-keys)
            (let ((/bin/sh (search-input-file (or native-inputs inputs)
                                              "/bin/sh")))
              ;; Adjust hard-coded /bin/sh for tests.
              (substitute* "lib/config.hin"
                (("\"/bin/sh\"")
                 (format #f "\"~a\"" /bin/sh)))))))))
   (synopsis "Macro processor")
   (description
    "GNU M4 is an implementation of the M4 macro language, which features
some extensions over other implementations, some of which are required by GNU
Autoconf.  It is used as a macro processor, which means it processes text,
expanding macros as it encounters them.  It also has some built-in functions,
for example to run shell commands or to do arithmetic.")
   (license gpl3+)
   (home-page "https://www.gnu.org/software/m4/")))
