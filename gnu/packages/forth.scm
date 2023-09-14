;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Nikita <nikita@n0.is>
;;; Copyright © 2016 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2023 B. Wilson <elaexuotee@wilsonb.com>
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

(define-module (gnu packages forth)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages vim))

(define-public gforth
  (package
    (name "gforth")
    (version "0.7.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/gforth/gforth-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1c1bahc9ypmca8rv2dijiqbangm1d9av286904yw48ph7ciz4qig"))))
    (build-system gnu-build-system)
    (arguments
     '(#:parallel-build? #f             ; XXX: parallel build fails
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-gforth.el
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out           (assoc-ref outputs "out"))
                    (emacs-sitedir (string-append
                                    out "/share/emacs/site-lisp")))
               ;; TODO: compile and autoload it.
               (install-file "gforth.el" emacs-sitedir)
               #t))))))
    (native-inputs
     (list m4))
    (synopsis "Forth interpreter")
    (description
     "Gforth is a fast and portable implementation of the ANSI Forth language.
It includes an editing mode for Emacs and an interpreter featuring completion
and history.  A generic virtual machine environment, vmgen, is also
included.")
    (home-page "https://www.gnu.org/software/gforth/")
    (license license:gpl3+)))

(define-public smithforth
  (package
    (name "smithforth")
    (version "220711")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://dacvs.neocities.org/SF/SForth"
                            version "dmp.txt"))
        (sha256
          (base32 "0a39pv7529llsa3f48fmvwvlcp3f9v8qkn5ziw2l6kxf0qvli3lm"))))
    (build-system trivial-build-system)
    (native-inputs (list xxd))
    (arguments
      (list
        #:modules '((guix build utils))
        #:builder
        #~(begin
            (use-modules (guix build utils)
                         (ice-9 textual-ports))
            (let* ((sforth.dmp #$(package-source this-package))
                   (system.fs  #$(origin
                                   (method url-fetch)
                                   (uri (string-append
                                          "https://dacvs.neocities.org/SF/system"
                                          version "fs.txt"))
                                   (sha256
                                    (base32
                                     "17v1pp64s6n8q8w3kg48nd7zdcx2208y4svr5fpfms5lkyzg7z1m"))))
                   (xxd        (string-append (assoc-ref %build-inputs "xxd")
                                              "/bin/xxd"))
                   (bin        (string-append (assoc-ref %outputs "out") "/bin")))
              (copy-file sforth.dmp "sforth.dmp")
              (substitute* "sforth.dmp" (("#.*$") "\n"))
              (with-output-to-file "sforth"
                (lambda _
                  (invoke xxd "-p" "-r" "sforth.dmp")
                  (call-with-input-file system.fs
                    (lambda (port) (display (get-string-all port)))))
                #:binary #t)
              (chmod "sforth" #o755)
              (install-file "sforth" bin)))))
    (home-page "https://dacvs.neocities.org/SF/")
    (synopsis "Forth programming language for x86-64 desktop computers")
    (description "SmithForth is an implementation of the Forth programming
language for x86-64 desktop computers. SmithForth is a text interpreter that
runs in a Linux text console.")
    (supported-systems '("x86_64-linux"))
    (license license:expat-0)))
