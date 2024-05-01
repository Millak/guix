;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2022, 2023, 2024 Janneke Nieuwenhuizen <janneke@gnu.org>
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
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pth))

(define-public dezyne
  (package
    (name "dezyne")
    (version "2.18.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://dezyne.org/download/dezyne/"
                           name "-" version ".tar.gz"))
       (sha256
        (base32 "0cya5p7y546ldiycz5clv0r8xhxf8zp8iim50y20vhsfgxvh96is"))))
    (propagated-inputs (list boost
                             guile-json-4
                             guile-readline
                             scmackerel))
    (inputs (list bash-minimal
                  guile-3.0
                  mcrl2-minimal
                  pth
                  sed))
    (native-inputs (list guile-3.0 pkg-config))
    (build-system gnu-build-system)
    (arguments
     (list
      #:modules `((ice-9 popen)
                  ,@%gnu-build-system-modules)
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'configure 'setenv
            (lambda _
              (setenv "GUILE_AUTO_COMPILE" "0")))
          (add-after 'install 'install-readmes
            (lambda _
              (let* ((base (string-append #$name "-" #$version))
                     (doc (string-append #$output "/share/doc/" base)))
                (mkdir-p doc)
                (copy-file "NEWS" (string-append doc "/NEWS")))))
          (add-after 'install 'wrap-binaries
            (lambda _
              (let* ((bash #$(this-package-input "bash-minimal"))
                     (guile #$(this-package-input "guile"))
                     (json #$(this-package-input "guile-json"))
                     (mcrl2 #$(this-package-input "mcrl2-minimal"))
                     (readline #$(this-package-input "guile-readline"))
                     (scmackerel #$(this-package-input "scmackerel"))
                     (sed #$(this-package-input "sed"))
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
                      (list (string-append #$output scm-dir)
                            (string-append json scm-dir)
                            (string-append readline scm-dir)
                            (string-append scmackerel scm-dir)))
                     (go-dir (string-append "/lib/guile/" effective
                                            "/site-ccache/"))
                     (go-path (list (string-append #$output go-dir)
                                    (string-append json go-dir)
                                    (string-append readline go-dir)
                                    (string-append scmackerel go-dir))))
                (wrap-program (string-append #$output "/bin/dzn")
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

(define-public scmackerel
  (package
    (name "scmackerel")
    (version "0.5.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://dezyne.org/download/scmackerel/"
                           name "-" version ".tar.gz"))
       (sha256
        (base32 "1sgrkw3idsni1ylf0slwgzzwq31b1yx6s0j17yq99c88agk9cvd6"))))
    (inputs (list bash-minimal
                  guile-3.0
                  guile-readline
                  mcrl2-minimal))
    (native-inputs (list guile-3.0 pkg-config))
    (build-system gnu-build-system)
    (arguments
     (list
      #:modules `((ice-9 popen)
                  ,@%gnu-build-system-modules)
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'configure 'setenv
            (lambda _
              (setenv "GUILE_AUTO_COMPILE" "0")))
          (add-after 'install 'install-readmes
            (lambda _
              (let* ((base (string-append #$name "-" #$version))
                     (doc (string-append #$output "/share/doc/" base)))
                (mkdir-p doc)
                (copy-file "NEWS" (string-append doc "/NEWS"))))))))
    (synopsis "AST library in GNU Guile")
    (description "SCMackerel is a library in GNU Guile to create abstract
syntax trees (ASTs).  Initially written for @url{https://mcrl2.org,mCRL2} and
now also supporting other languages, such as C, C++, and C#.  Based on GNU
Guix records.")
    (home-page "https://gitlab.com/janneke/scmackerel")
    (license (list license:gpl3+))))
