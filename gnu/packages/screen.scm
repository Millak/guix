;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Cyril Roelandt <tipecaml@gmail.com>
;;; Copyright © 2014 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015, 2017 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2016, 2017, 2019-2022, 2025 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2017 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2017, 2018, 2019, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2023 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2024 Janneke Nieuwenhuizen <janneke@gnu.org>
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

(define-module (gnu packages screen)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages hurd)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages slang)
  #:use-module (gnu packages texinfo))

(define-public screen
  (package
    (name "screen")
    (version "5.0.1")
    (source (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://git.savannah.gnu.org/git/screen.git")
                   (commit (string-append "v." version))))
             (file-name (git-file-name name version))
             (patches (search-patches "screen-hurd-path-max.patch"))
             (sha256
              (base32 "1km6qbczlvzwcaagb8fy65k5z6ywn2whw57wdkfxg9dh9kg39hg7"))
             (snippet
              #~(begin (use-modules (guix build utils))
                       ;; Fixes https://savannah.gnu.org/bugs/?67126.
                       ;; Can be removed with 5.0.2?
                       (with-fluids ((%default-port-encoding #f))
                         (substitute* "src/doc/screen.texinfo"
                           (("alexander_naumov@opensuse.org")
                            "alexander_naumov@@opensuse.org")))))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf-2.71 automake texinfo))
    (inputs
     (list libxcrypt linux-pam ncurses perl))
    (arguments
     (list
       #:configure-flags
       ;; GNU_SOURCE must be defined for mallocmock_reset() to be defined
       #~(list "CFLAGS=-O2 -g -D_GNU_SOURCE=1"

               ;; By default, screen supports 16 colors, but we want 256 when
               ;; ~/.screenrc contains 'term xterm-256color'.
               "--enable-colors256")
       #:phases
       #~(modify-phases %standard-phases
           (add-after 'unpack 'chdir
             (lambda _ (chdir "src"))))))
    (home-page "https://www.gnu.org/software/screen/")
    (synopsis "Full-screen window manager providing multiple terminals")
    (description
     "GNU Screen is a terminal window manager that multiplexes a single
terminal between several processes.  The virtual terminals each provide
features such as a scroll-back buffer and a copy-and-paste mechanism.  Screen
then manages the different virtual terminals, allowing you to easily switch
between them, to detach them from the current session, or even splitting the
view to show two terminals at once.")
    (license gpl2+)))

(define-public dtach
  (package
    (name "dtach")
    (version "0.9")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/" name "/" name "/"
                                  version "/" name "-" version ".tar.gz"))
              (sha256
               (base32
                "1wwj2hlngi8qn2pisvhyfxxs8gyqjlgrrv5lz91w8ly54dlzvs9j"))))
    (build-system gnu-build-system)
    (arguments
     ;; No install target.
     '(#:phases
       (modify-phases %standard-phases
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (install-file "dtach" (string-append out "/bin"))
               (install-file "dtach.1" (string-append out "/share/man/man1"))
               #t))))
       ;; No check target.
       #:tests? #f))
    (home-page "https://dtach.sourceforge.net/")
    (synopsis "Emulates the detach feature of screen")
    (description
     "dtach is a tiny program that emulates the detach feature of screen,
allowing you to run a program in an environment that is protected from the
controlling terminal and attach to it later.")
    (license gpl2+)))

(define-public byobu
  (package
    (name "byobu")
    (version "5.133")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://launchpad.net/byobu/trunk/"
                           version "/+download/byobu_"
                           version ".orig.tar.gz"))
       (sha256
        (base32 "0qvmmdnvwqbgbhn5c8asmrmjhclcl029py2d2zvmd7h5ij7s93jd"))
       (patches (search-patches "byobu-writable-status.patch"))))
    (build-system gnu-build-system)
    (inputs
     `(("bash" ,bash-minimal) ; for wrap-program
       ("python" ,python-wrapper)       ; for config and session GUIs
       ("python-newt" ,newt "python")))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before
          'configure 'provide-locale
          (lambda* (#:key inputs #:allow-other-keys)
            (let ((libc (assoc-ref inputs "libc"))) ; implicit input
              (substitute* "usr/bin/byobu.in"
                (("locale") (string-append libc "/bin/locale")))
              #t)))
         (add-after
          'install 'wrap-python-scripts
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (let* ((python (search-input-file inputs "/bin/python"))
                   (out    (assoc-ref outputs "out"))
                   (config (string-append out "/bin/byobu-config"))
                   (select (string-append out "/bin/byobu-select-session")))
              (wrap-program config
                `("BYOBU_PYTHON" = (,python))
                `("GUIX_PYTHONPATH" ":" prefix (,(getenv "GUIX_PYTHONPATH"))))
              (wrap-program select
                `("BYOBU_PYTHON" = (,python)))
              #t))))))
    (home-page "https://byobu.org/")
    (synopsis "Text-based window manager and terminal multiplexer")
    (description
     "Byobu is a Japanese term for decorative, multi-panel screens that serve
as folding room dividers.  The Byobu software includes an enhanced profile,
configuration utilities, and system status notifications for the GNU Screen
window manager as well as the Tmux terminal multiplexer.")
    (license gpl3+)))

(define-public reptyr
  (package
    (name "reptyr")
    (version "0.10.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nelhage/reptyr")
             (commit (string-append "reptyr-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "03k95bf7b46g89j0hgfvra9d512z0ha7vw9358h9j67h9b5bylwf"))))
    (build-system gnu-build-system)
    (arguments
     (list #:tests? #f                      ; no tests
           #:make-flags
           #~(list (string-append "CC=" #$(cc-for-target))
                   (string-append "PREFIX=" #$output)
                   (string-append "BASHCOMPDIR=" #$output
                                  "/etc/bash_completion.d"))
           #:phases
           #~(modify-phases %standard-phases
               (delete 'configure))))         ; no configure script
    (home-page "https://github.com/nelhage/reptyr")
    (synopsis "Tool for reparenting a running program to a new terminal")
    (description
     "reptyr is a utility for taking an existing running program and attaching
it to a new terminal.  Started a long-running process over @code{ssh}, but have
to leave and don't want to interrupt it?  Just start a @code{screen}, use
reptyr to grab it, and then kill the @code{ssh} session and head on home.")
    ;; Reptyr currently does not support mips.
    (supported-systems (delete "mips64el-linux" %supported-systems))
    (license expat)))
