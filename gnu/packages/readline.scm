;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014, 2020 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016, 2019 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Jan Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2018, 2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019, 2020, 2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2025 Maxim Cournoyer <maxim@guixotic.coop>
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

(define-module (gnu packages readline)
  #:use-module (guix licenses)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages perl)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix utils)
  #:use-module (ice-9 format))

(define (patch-url version seqno)
  (format #f "mirror://gnu/readline/readline-~a-patches/readline~a-~3,'0d"
          version (string-join (string-split version #\.) "") seqno))

(define (readline-patch version seqno sha256-bv)
  "Return the origin of Readline patch SEQNO, with expected hash SHA256-BV"
  (origin
    (method url-fetch)
    (uri (patch-url version seqno))
    (sha256 sha256-bv)))

(define-syntax-rule (patch-series version (seqno hash) ...)
  (list (readline-patch version seqno (base32 hash))
        ...))

(define %patch-series-8.3
  (patch-series
   "8.3"
   (1 "1pl4midx7kc56bw4ansrdpdjpanv1vmp0p6jghrrgrnv0qqs1w11")))

(define %patch-series-7.0
  (patch-series
   "7.0"
   (1 "0xm3sxvwmss7ddyfb11n6pgcqd1aglnpy15g143vzcf75snb7hcs")
   (2 "0n1dxmqsbjgrfxb1hgk5c6lsraw4ncbnzxlsx7m35nym6lncjiw7")
   (3 "1027kmymniizcy0zbdlrczxfx3clxcdln5yq05q9yzlc6y9slhwy")
   (4 "0r3bbaf12iz8m02z6p3fzww2m365fhn71xmzab2p62gj54s6h9gr")
   (5 "0lxpa4f72y2nsgj6fgrhjk2nmmxvccys6aciwfxwchb5f21rq5fa")))

(define-public readline
  (package
    (name "readline")
    (version (string-append "8.3."
                            (number->string (length %patch-series-8.3))))
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/readline/readline-"
                                  (version-major+minor version) ".tar.gz"))
              (sha256
               (base32
                "1k31j1prf9zn86hccay2i49vlzks0cy1v3gfjpa8r0k78hh86lzy"))
              (patches (append %patch-series-8.3
                               (search-patches "readline-link-ncurses.patch")))
              (patch-flags '("-p0"))))
    (build-system gnu-build-system)
    (propagated-inputs (list ncurses))
    (arguments
     (append
      (if (target-loongarch64?)
          (list #:phases
                #~(modify-phases %standard-phases
                    (add-after 'unpack 'update-config-scripts
                      (lambda* (#:key inputs native-inputs #:allow-other-keys)
                        ;; Replace outdated config.guess and config.sub.
                        (for-each (lambda (file)
                                    (install-file
                                     (search-input-file
                                      (or native-inputs inputs)
                                      (string-append "/bin/" file)) "./support"))
                                  '("config.guess" "config.sub"))))))
          '())
      (list #:configure-flags
            #~(list (string-append
                     "LDFLAGS=-Wl,-rpath -Wl,"
                     (dirname (search-input-file %build-inputs
                                                 "lib/libncurses.so")))

                    ;; This test does an 'AC_TRY_RUN', which aborts when
                    ;; cross-compiling, so provide the correct answer.
                    #$@(if (%current-target-system)
                           '("bash_cv_wcwidth_broken=no")
                           '())
                    ;; MinGW: ncurses provides the termcap api.
                    #$@(if (target-mingw?)
                           '("bash_cv_termcap_lib=ncurses")
                           '()))

            #:make-flags
            (if (target-mingw?)
                ;; MinGW: termcap in ncurses
                ;; some SIG_* #defined in _POSIX
                #~'("TERMCAP_LIB=-lncurses"
                    "CPPFLAGS=-D_POSIX -D'chown(f,o,g)=0'")
                #~'()))))
    (native-inputs (if (target-loongarch64?)
                       (list config)
                       '()))
    (synopsis "Edit command lines while typing, with history support")
    (description
     "The GNU readline library allows users to edit command lines as they
are typed in.  It can maintain a searchable history of previously entered
commands, letting you easily recall, edit and re-enter past commands.  It
features both Emacs-like and vi-like keybindings, making its usage
comfortable for anyone.")
    (license gpl3+)
    (home-page "https://savannah.gnu.org/projects/readline/")))

(define-public readline-7
  (package (inherit readline)
    (name "readline")
    (version (string-append "7.0."
                            (number->string (length %patch-series-7.0))))
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/readline/readline-"
                                  (version-major+minor version) ".tar.gz"))
              (sha256
               (base32
                "0d13sg9ksf982rrrmv5mb6a2p4ys9rvg9r71d6il0vr8hmql63bm"))
              (patches (append
                        %patch-series-7.0
                        (search-patches "readline-link-ncurses.patch")))
              (patch-flags '("-p0"))))))

(define-public readline-6.2
  (package (inherit readline)
    (version "6.2")
    (source (origin (inherit (package-source readline))
              (method url-fetch)
              (uri (string-append "mirror://gnu/readline/readline-"
                                  version ".tar.gz"))
              (patches (search-patches "readline-6.2-CVE-2014-2524.patch"))
              (patch-flags '("-p0"))
              (sha256
               (base32
                "10ckm2bd2rkxhvdmj7nmbsylmihw0abwcsnxf8y27305183rd9kr"))))))

(define-public rlwrap
  (package
    (name "rlwrap")
    (version "0.46.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hanslub42/rlwrap")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0mp3rhq8qifj0gr31qi5sh10ixlpvqv3vi4miaf61v1xiphvd6nk"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake perl))
    (inputs
     (list readline))
    (synopsis "Wrapper to allow the editing of keyboard commands")
    (description
     "Rlwrap is a 'readline wrapper', a small utility that uses the GNU
readline library to allow the editing of keyboard input for any command.  You
should consider rlwrap especially when you need user-defined completion (by way
of completion word lists) and persistent history, or if you want to program
`special effects' using the filter mechanism.")
    (home-page "https://github.com/hanslub42/rlwrap")
    (license gpl2+)))
