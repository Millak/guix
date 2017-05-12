;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2014, 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016 Leo Famulari <leo@famulari.name>
;;; Copyright © 2017 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2017 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu packages pcre)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public pcre
  (package
   (name "pcre")
   (version "8.40")
   (replacement pcre/fixed)
   (source (origin
            (method url-fetch)
            (uri (list
                  (string-append "ftp://ftp.csx.cam.ac.uk"
                                 "/pub/software/programming/pcre/"
                                 "pcre-" version ".tar.bz2")
                  (string-append "mirror://sourceforge/pcre/pcre/"
                                 version "/pcre-" version ".tar.bz2")))
            (sha256
             (base32
              "1x7lpjn7jhk0n3sdvggxrlrhab8kkfjwl7qix0ypw9nlx8lpmqh0"))))
   (build-system gnu-build-system)
   (outputs '("out"           ;library & headers
              "bin"           ;depends on Readline (adds 20MiB to the closure)
              "doc"))         ;1.8 MiB of HTML
   (inputs `(("bzip2" ,bzip2)
             ("readline" ,readline)
             ("zlib" ,zlib)))
   (arguments
    '(#:disallowed-references ("doc")
      #:configure-flags '("--enable-utf"
                          "--enable-pcregrep-libz"
                          "--enable-pcregrep-libbz2"
                          "--enable-pcretest-libreadline"
                          "--enable-unicode-properties"
                          "--enable-pcre16"
                          "--enable-pcre32"
                          "--enable-jit")))
   (synopsis "Perl Compatible Regular Expressions")
   (description
    "The PCRE library is a set of functions that implement regular expression
pattern matching using the same syntax and semantics as Perl 5.  PCRE has its
own native API, as well as a set of wrapper functions that correspond to the
POSIX regular expression API.")
   (license license:bsd-3)
   (home-page "http://www.pcre.org/")))

(define pcre/fixed
  (package
    (inherit pcre)
    (replacement #f)
    (source (origin
              (inherit (package-source pcre))
              (patches (search-patches "pcre-CVE-2017-7186.patch"))))))

(define-public pcre2
  (package
    (name "pcre2")
    (version "10.23")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/pcre/pcre2/"
                                  version "/pcre2-" version ".tar.bz2"))

              (sha256
               (base32
                "0vn5g0mkkp99mmzpissa06hpyj6pk9s4mlwbjqrjvw3ihy8rpiyz"))
              (patches (search-patches "pcre2-CVE-2017-7186.patch"
                                       "pcre2-CVE-2017-8786.patch"))))
   (build-system gnu-build-system)
   (inputs `(("bzip2" ,bzip2)
             ("readline" ,readline)
             ("zlib" ,zlib)))
   (arguments
    `(#:configure-flags '("--enable-unicode"
                          "--enable-pcre2grep-libz"
                          "--enable-pcre2grep-libbz2"
                          "--enable-pcre2test-libreadline"
                          "--enable-pcre2-16"
                          "--enable-pcre2-32"
                          "--enable-jit")
      #:phases
      (modify-phases %standard-phases
        (add-after 'unpack 'patch-paths
          (lambda _
            (substitute* "RunGrepTest"
              (("/bin/echo") (which "echo")))
            #t)))))
   (synopsis "Perl Compatible Regular Expressions")
   (description
    "The PCRE library is a set of functions that implement regular expression
pattern matching using the same syntax and semantics as Perl 5.  PCRE has its
own native API, as well as a set of wrapper functions that correspond to the
POSIX regular expression API.")
   (license license:bsd-3)
   (home-page "http://www.pcre.org/")))
