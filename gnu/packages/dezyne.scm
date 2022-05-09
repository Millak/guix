;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2022 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

(define-module (gnu packages dezyne)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages pkg-config))

(define-public dezyne
  (package
    (name "dezyne")
    (version "2.15.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://dezyne.org/download/dezyne/"
                           name "-" version ".tar.gz"))
       (sha256
        (base32 "1sh9chg5q10c3bzsmgl1pb7pmdf04j2lqszhw8jk5qlxr9y8ybcq"))))
    (inputs (list bash-minimal
                  guile-3.0-latest
                  guile-json-4
                  guile-readline
                  mcrl2-minimal
                  sed))
    (native-inputs (list guile-3.0-latest pkg-config))
    (build-system gnu-build-system)
    (arguments
     (list
      #:modules `((ice-9 popen)
                  ,@%gnu-build-system-modules)
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'disable-tests
            (lambda _
              ;; The mCRL2 output for these tests is unstable, i.e., varies
              ;; between different builds.
              (substitute* "Makefile.in"
                (("test/all/compliance_blocking_double_release ") " ")
                (("test/all/illegal_external_nonsynchronous ") " ")
                (("test/all/livelock_synchronous_illegal ") " ")
                (("test/all/queuefull_external_sync ") " "))))
          (add-before 'configure 'setenv
            (lambda _
              (setenv "GUILE_AUTO_COMPILE" "0")))
          (add-after 'install 'install-readmes
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (base (string-append #$name "-" #$version))
                     (doc (string-append out "/share/doc/" base)))
                (mkdir-p doc)
                (copy-file "NEWS" (string-append doc "/NEWS")))))
          (add-after 'install 'wrap-binaries
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (bash (assoc-ref %build-inputs "bash-minimal"))
                     (guile (assoc-ref %build-inputs "guile"))
                     (json (assoc-ref %build-inputs "guile-json"))
                     (mcrl2 (assoc-ref %build-inputs "mcrl2-minimal"))
                     (readline (assoc-ref %build-inputs "guile-readline"))
                     (sed (assoc-ref %build-inputs "sed"))
                     (effective (read
                                 (open-pipe* OPEN_READ
                                             "guile" "-c"
                                             "(write (effective-version))")))
                     (path (list (string-append bash "/bin")
                                 (string-append guile "/bin")
                                 (string-append mcrl2 "/bin")
                                 (string-append sed "/bin")))
                     (scm-dir (string-append "/share/guile/site/" effective))
                     (scm-path
                      (list (string-append out scm-dir)
                            (string-append json scm-dir)
                            (string-append readline scm-dir)))
                     (go-dir (string-append "/lib/guile/" effective
                                            "/site-ccache/"))
                     (go-path (list (string-append out go-dir)
                                    (string-append json go-dir)
                                    (string-append readline go-dir))))
                (wrap-program (string-append out "/bin/dzn")
                  `("PATH" ":" prefix ,path)
                  `("GUILE_AUTO_COMPILE" ":" = ("0"))
                  `("GUILE_LOAD_PATH" ":" prefix ,scm-path)
                  `("GUILE_LOAD_COMPILED_PATH" ":" prefix ,go-path))))))))
    (synopsis "Programming language with verifyable formal semantics")
    (description "Dezyne is a programming language and a set of tools to
specify, validate, verify, simulate, document, and implement concurrent
control software for embedded and cyber-physical systems.  The Dezyne language
has formal semantics expressed in @url{https://mcrl2.org,mCRL2}.")
    (home-page "https://dezyne.org")
    (license (list license:agpl3+       ;Dezyne itself
                   license:lgpl3+       ;Dezyne runtime library
                   license:cc0)))) ;Code snippets, images, test data
