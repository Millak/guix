;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015, 2016, 2017 Manolis Fragkiskos Ragkousis <manolis837@gmail.com>
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

(define-module (gnu packages hurd)
  #:use-module (guix licenses)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages base)
  #:use-module (guix git-download)
  #:export (hurd-triplet?))

(define (hurd-triplet? triplet)
  (and (string-suffix? "-gnu" triplet)
       (not (string-contains triplet "linux"))))

(define (gnumach-source-url version)
  (string-append "mirror://gnu/gnumach/gnumach-"
                 version ".tar.gz"))

(define (hurd-source-url version)
  (string-append "mirror://gnu/hurd/hurd-"
                 version ".tar.gz"))

(define-public gnumach-headers
  (package
    (name "gnumach-headers")
    (version "1.8")
    (source
     (origin
      (method url-fetch)
      (uri (gnumach-source-url version))
      (sha256
       (base32
        "02hygsfpd2dljl5lg1vjjg9pizi9jyxd4aiiqzjshz6jax62jm9f"))))
    (build-system gnu-build-system)
    (arguments
    `(#:phases (alist-replace
                'install
                (lambda _
                  (zero?
                   (system* "make" "install-data")))
                (alist-delete
                 'build
                 %standard-phases))

      ;; GNU Mach supports only IA32 currently, so cheat so that we can at
      ;; least install its headers.
      ,@(if (%current-target-system)
            '()
            ;; See <http://lists.gnu.org/archive/html/bug-hurd/2015-06/msg00042.html>
            ;; <http://lists.gnu.org/archive/html/guix-devel/2015-06/msg00716.html>
            '(#:configure-flags '("--build=i586-pc-gnu")))

      #:tests? #f))
    (home-page "https://www.gnu.org/software/hurd/microkernel/mach/gnumach.html")
    (synopsis "GNU Mach kernel headers")
    (description
     "Headers of the GNU Mach kernel.")
    (license gpl2+)))

(define-public mig
  (package
    (name "mig")
    (version "1.8")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/mig/mig-"
                          version ".tar.gz"))
      (sha256
       (base32
        "1gyda8sq6b379nx01hkpbd85lz39irdvz2b9wbr63gicicx8i706"))))
    (build-system gnu-build-system)
    ;; Flex is needed both at build and run time.
    (inputs `(("gnumach-headers" ,gnumach-headers)
              ("flex" ,flex)))
    (native-inputs
     `(("flex" ,flex)
       ("bison" ,bison)))
    (arguments `(#:tests? #f))
    (home-page "https://www.gnu.org/software/hurd/microkernel/mach/mig/gnu_mig.html")
    (synopsis "Mach 3.0 interface generator for the Hurd")
    (description
     "GNU MIG is the GNU distribution of the Mach 3.0 interface generator
MIG, as maintained by the GNU Hurd developers for the GNU project.
You need this tool to compile the GNU Mach and GNU Hurd distributions,
and to compile the GNU C library for the Hurd.  Also, you will need it
for other software in the GNU system that uses Mach-based inter-process
communication.")
    (license gpl2+)))

(define-public hurd-headers
  (package
    (name "hurd-headers")
    (version "0.9")
    (source (origin
              (method url-fetch)
              (uri (hurd-source-url version))
              (sha256
               (base32
                "1nw9gly0n7pyv3cpfm4mmxy4yccrx4g0lyrvd3vk2vil26jpbggw"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("mig" ,mig)))
    (arguments
     `(#:phases (alist-replace
                 'install
                 (lambda _
                   (zero? (system* "make" "install-headers" "no_deps=t")))
                 (alist-delete 'build %standard-phases))

       #:configure-flags '(;; Pretend we're on GNU/Hurd; 'configure' wants
                           ;; that.
                           ,@(if (%current-target-system)
                                 '()
                                 '("--host=i586-pc-gnu"))

                           ;; Reduce set of dependencies.
                           "--without-parted"
                           "--disable-ncursesw"
                           "--disable-test"
                           "--without-libbz2"
                           "--without-libz"
                           ;; Skip the clnt_create check because it expects
                           ;; a working glibc causing a circular dependency.
                           "ac_cv_search_clnt_create=no")

       #:tests? #f))
    (home-page "https://www.gnu.org/software/hurd/hurd.html")
    (synopsis "GNU Hurd headers")
    (description
     "This package provides C headers of the GNU Hurd, used to build the GNU C
Library and other user programs.")
    (license gpl2+)))

(define-public hurd-minimal
  (package (inherit hurd-headers)
    (name "hurd-minimal")
    (inputs `(("glibc-hurd-headers" ,glibc/hurd-headers)))
    (native-inputs
     `(("mig" ,mig)))
    (arguments
     (substitute-keyword-arguments (package-arguments hurd-headers)
       ((#:phases _)
        '(alist-replace
          'install
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((out (assoc-ref outputs "out")))
              ;; We need to copy libihash.a to the output directory manually,
              ;; since there is no target for that in the makefile.
              (mkdir-p (string-append out "/include"))
              (copy-file "libihash/ihash.h"
                         (string-append out "/include/ihash.h"))
              (mkdir-p (string-append out "/lib"))
              (copy-file "libihash/libihash.a"
                         (string-append out "/lib/libihash.a"))
              #t))
          (alist-replace
           'build
           (lambda _
             (zero? (system* "make" "-Clibihash" "libihash.a")))
           %standard-phases)))))
    (home-page "https://www.gnu.org/software/hurd/hurd.html")
    (synopsis "GNU Hurd libraries")
    (description
     "This package provides libihash, needed to build the GNU C
Library for GNU/Hurd.")
    (license gpl2+)))

(define-public hurd-core-headers
  (package
    (name "hurd-core-headers")
    (version (package-version hurd-headers))
    (source #f)
    (build-system trivial-build-system)
    (arguments
     '(#:modules ((guix build union))
       #:builder (begin
                   (use-modules (ice-9 match)
                                (guix build union))
                   (match %build-inputs
                     (((names . directories) ...)
                      (union-build (assoc-ref %outputs "out")
                                   directories))))))
    (inputs `(("gnumach-headers" ,gnumach-headers)
              ("hurd-headers" ,hurd-headers)
              ("hurd-minimal" ,hurd-minimal)))
    (synopsis "Union of the Hurd headers and libraries")
    (description
     "This package contains the union of the Mach and Hurd headers and the
Hurd-minimal package which are needed for both glibc and GCC.")
    (home-page (package-home-page hurd-headers))
    (license (package-license hurd-headers))))

(define-public gnumach
  (package
    (name "gnumach")
    (version "1.8")
    (source (origin
              (method url-fetch)
              (uri (gnumach-source-url version))
              (sha256
               (base32
                "02hygsfpd2dljl5lg1vjjg9pizi9jyxd4aiiqzjshz6jax62jm9f"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-after 'install 'produce-image
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let* ((out  (assoc-ref outputs "out"))
                             (boot (string-append out "/boot")))
                        (and (zero? (system* "make" "gnumach.gz"))
                             (begin
                               (install-file "gnumach.gz" boot)
                               #t))))))))
    (native-inputs
     `(("mig" ,mig)
       ("perl" ,perl)))
    (supported-systems (cons "i686-linux" %hurd-systems))
    (home-page
     "https://www.gnu.org/software/hurd/microkernel/mach/gnumach.html")
    (synopsis "Microkernel of the GNU system")
    (description
     "GNU Mach is the microkernel upon which a GNU Hurd system is based.")
    (license gpl2+)))
