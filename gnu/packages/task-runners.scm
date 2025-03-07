;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 Stefan Reichör <stefan@xsteve.at>
;;; Copyright © 2021 Ivan Gankevich <i.gankevich@spbu.ru>
;;; Copyright © 2024-2025 Sharlatan Hellseher <sharlatanus@gmail.com>
;;; Copyright © 2025 Nicolas Graves <ngraves@ngraves.fr>
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

(define-module (gnu packages task-runners)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages mail)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public run
  (package
    (name "run")
    (version "0.11.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/TekWizely/run")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1p9hyc3zrjh58nqdc2j2qnkcyg7z8av2q7fb49ycs4292awl0zka"))))
    (build-system go-build-system)
    (arguments
     (list
      #:install-source? #f
      #:import-path "github.com/tekwizely/run"))
    (native-inputs
     (list go-github-com-goreleaser-fileglob
           go-github-com-subosito-gotenv
           go-github-com-tekwizely-go-parsing))
    (home-page "https://github.com/TekWizely/run")
    (synopsis "Easily manage and invoke small scripts and wrappers")
    (description
     "Run is a tool to easily manage and invoke small scripts and wrappers by
using a Runfile.")
    (license license:expat)))

(define-public task-spooler
  (package
    (name "task-spooler")
    (version "1.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://viric.name/soft/ts/ts-"
                           version ".tar.gz"))
       (sha256
        (base32 "0a5l8bjq869lvqys3amsil933vmm9b387axp1jv3bi9xah8k70zs"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:make-flags
      #~(let ((c-flags "-g -O2"))
          (list (string-append "PREFIX=" #$output)
                (string-append "CC=" #$(cc-for-target))
                (string-append "CFLAGS=" c-flags)))
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure) ;; no configuration script
          (add-after 'unpack 'rename-and-patch-paths
            (lambda* (#:key inputs #:allow-other-keys)
              ;; Rename "ts" to "tsp" to not interfere with "ts" command
              ;; from moreutils package.
              (rename-file "ts.1" "tsp.1");
              (substitute* '("Makefile" "testbench.sh")
                (("\\bts\\b") "tsp"))
              ;; Patch gzip/sendmail/shell paths.
              (substitute* '("execute.c" "list.c")
                (("execlp\\(\"gzip\"")
                 (format #f "execlp(\"~a/bin/gzip\""
                         #$(this-package-input "gzip")))
                (("execlp\\(\"sh\"")
                 (format #f "execlp(\"~a/bin/sh\""
                         #$(this-package-input "bash-minimal")))
                (("/bin/sh\\b")
                 (format #f "~a/bin/sh"
                         #$(this-package-input "bash-minimal"))))
              (substitute* "mail.c"
                (("/usr/sbin/sendmail")
                 (format #f "~a/sbin/sendmail"
                         #$(this-package-input "sendmail"))))))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (if tests?
                  (begin
                    (setenv "PATH"
                            (string-append (getenv "PATH") ":" (getcwd)))
                    (invoke "./testbench.sh"))
                  (format #t "test suite not run ~%")))))))
    (inputs
     (list bash-minimal gzip sendmail))
    (home-page "https://viric.name/soft/ts/")
    (synopsis "UNIX task queue system")
    (description
     "Task spooler lets users run shell commands asynchronously one after the
other in a separate process.")
    (license license:gpl2+)))
