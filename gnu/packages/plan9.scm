;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 宋文武 <iyzsong@member.fsf.org>
;;; Copyright © 2023 Antero Mejr <antero@mailbox.org>
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

(define-module (gnu packages plan9)
  #:use-module (guix build-system gnu)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg))

(define-public drawterm
  (let ((revision "1")
        (commit "f11139d4c918802a87730bc14d094670ee4ce572"))
    (package
      (name "drawterm")
      (version (git-version "20240703" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "git://git.9front.org/plan9front/drawterm")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0ggh5g19899iq9bb5r03bvhamndyai4ylr3ajkbd02xkhz65fh5y"))))
      (build-system gnu-build-system)
      (arguments
       `(#:make-flags (list "CONF=unix"
                            (string-append "CC=" ,(cc-for-target)))
         #:tests? #f                    ; no tests
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)          ; no configure script
           (replace 'install            ; no install target
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (bin (string-append out "/bin/"))
                      (man (string-append out "/share/man/man1/")))
                 (install-file "drawterm" bin)
                 (install-file "drawterm.1" man)))))))
      (inputs
       (list libx11 libxt))
      (synopsis "Connect to Plan 9 systems")
      (home-page "https://drawterm.9front.org")
      (description
       "@command{drawterm} is a client for connecting venerable systems to
Plan 9 systems.  It behaves like a Plan 9 kernel and will attempt to
reconstruct a Plan 9 terminal-like experience from a non-Plan 9 system.")
      (license license:expat))))

(define-public drawterm-wayland
  (package
    (inherit drawterm)
    (name "drawterm-wayland")
    (arguments
     (substitute-keyword-arguments (package-arguments drawterm)
       ((#:make-flags _)
        `(list "CONF=linux"
               ,(string-append "CC=" (cc-for-target))))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list libxkbcommon pipewire wayland wayland-protocols wlr-protocols))))

(define-public plan9port
  ;; no releases
  (let ((commit "f8681acb374fa0d5ed1568dbedb00a4abe1ca6f1")
        (revision "1"))
    (package
      (name "plan9port")
      (version (git-version "0.1.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/9fans/plan9port")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "01343jvn8kr63i78h8xlgscn6wihdsr44xzh1cylvhigjbqw8n2x"))
                (modules '((guix build utils)))
                (snippet #~(for-each delete-file-recursively
                                     '("font/luc" ;nonfree
                                       "font/lucm" "font/lucsans" "font/pelm")))))
      (build-system gnu-build-system)
      (arguments
       (list #:tests? #f ;no tests
             #:strip-directories #~'("plan9/bin")
             #:phases
             #~(modify-phases %standard-phases
                 (add-after 'unpack 'setup
                   (lambda _
                     (let ((dest (string-append #$output "/plan9")))
                       (delete-file "src/cmd/mk/mk.pdf")
                       (substitute* "src/cmd/acme/acme.c"
                         (("/lib/font/bit/lucsans/euro.8.font")
                          (string-append dest
                                         "/font/fixed/unicode.5x8.font"))
                         (("/lib/font/bit/lucm/unicode.9.font")
                          (string-append dest
                                         "/font/fixed/unicode.6x9.font")))
                       (substitute* (find-files "src")
                         (("/lib/font/bit")
                          (string-append dest "/font")))
                       (substitute* "bin/9c"
                         (("which")
                          (which "which")))
                       (substitute* "src/cmd/fontsrv/freetyperules.sh"
                         (("'\\$i'/freetype2")
                          (string-append "-I"
                                         #$freetype
                                         "/include/freetype2")))
                       (with-output-to-file "LOCAL.config"
                         (lambda _
                           (format #t "CC9=~a~%" #$(cc-for-target))
                           (format #t "FONTSRV=fontsrv~%")))
                       (setenv "X11" #$libx11)
                       (setenv "PLAN9" (getcwd))
                       (setenv "PLAN9_TARGET" dest))))
                 (delete 'configure)    ;no configure
                 (replace 'build
                   (lambda _
                     (invoke "./INSTALL" "-b")))
                 (replace 'install
                   (lambda _
                     (invoke "./INSTALL" "-c")
                     (let ((dest (getenv "PLAN9_TARGET")))
                       (for-each (lambda (x)
                                   (let ((out (string-append dest "/" x)))
                                     (mkdir-p out)
                                     (copy-recursively x out)))
                                 ;; TODO: use external sky and dict packages
                                 '("bin" "face"
                                   "font"
                                   "include"
                                   "lib"
                                   "lp"
                                   "mail"
                                   "man"
                                   "ndb"
                                   "plumb"
                                   "tmac"
                                   "troff"
                                   "postscript"))
                       (install-file "rcmain" dest)
                       (mkdir-p (string-append #$output "/bin"))
                       (symlink (string-append dest "/bin/9")
                                (string-append #$output "/bin/9")))))
                 ;; Plan9 doesn't compress man pages
                 (delete 'compress-documentation))))
      (native-inputs (list perl which))
      (inputs (list bash-minimal                  ;for 'wrap-program'
                    fontconfig libx11 libxext libxt))
      ;; Propagate gcc-toolchain because some programs, like the 9c compiler,
      ;; are just aliased scripts to gcc equivalents.
      (propagated-inputs (list (module-ref (resolve-interface
                                          '(gnu packages commencement))
                                         'gcc-toolchain)))
      (home-page "https://9fans.github.io/plan9port/")
      (synopsis "Port of many Plan 9 libraries and programs")
      (description
       "Plan 9 from User Space (aka plan9port) is a port of many Plan 9
programs from their native Plan 9 environment to Unix-like operating
systems.")
      (license (list license:expat ;modifications
                     license:lpl1.02 ;original Plan9 code
                     license:zlib))))) ;src/cmd/bzip2
