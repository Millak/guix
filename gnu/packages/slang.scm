;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2018, 2019, 2022 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (gnu packages slang)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages popt)
  #:use-module (gnu packages fribidi)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages image)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages python))

(define-public slang
  (package
    (name "slang")
    (version "2.3.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.jedsoft.org/releases/slang/slang-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "06p379fqn6w38rdpqi98irxi2bf4llb0rja3dlgkqz7nqh7kp7pw"))
              (modules '((guix build utils)))
              (snippet
               #~(begin
                   (substitute* "src/Makefile.in"
                     (("/bin/ln") "ln"))))))
    (build-system gnu-build-system)
    (arguments
     (list #:parallel-tests? #f
           #:parallel-build? #f         ; race to build/use elfobj
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'reduce-array-test-size
                 ;; Fix array.sl/array.slc failure on 32-bit systems ("Unable to
                 ;; to create a multi-dimensional array of the desired size").
                 (lambda _
                   (substitute* "src/test/array.sl"
                     (("10000,10000,10000,10000,10000,10000")
                      "10,10,10,10,10,10"))))
               (add-before 'configure 'fix-configure-script
                 ;; Don't try to link to the long-obsolete (and gone) -ltermcap.
                 (lambda _
                   (substitute* "configure"
                     (("(MISC_TERMINFO_DIRS)=.*" _ variable)
                      (format #f "~a=\"~a/share/terminfo\"\n" variable
                              #$(this-package-input "ncurses")))))))))
    (inputs
     (list readline zlib libpng pcre ncurses))
    (home-page "https://www.jedsoft.org/slang/")
    (synopsis "Library for interactive applications and extensibility")
    (description
     "S-Lang is a multi-platform programmer's library designed to allow a
developer to create robust multi-platform software.  It provides facilities
required by interactive applications such as display/screen management,
keyboard input, keymaps, and so on.  The most exciting feature of the library
is the slang interpreter that may be easily embedded into a program to make it
extensible.  While the emphasis has always been on the embedded nature of the
interpreter, it may also be used in a stand-alone fashion through the use of
slsh, which is part of the S-Lang distribution.")
    (license license:gpl2+)))

(define-public most
  (package
    (name "most")
    (version "5.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://www.jedsoft.org/releases/most/most-"
                           version ".tar.gz"))
       (sha256
        (base32 "008537ns659pw2aag15imwjrxj73j26aqq90h285is6kz8gmv06v"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            (substitute* "src/Makefile.in"
              (("/bin/cp") "cp"))))))
    (build-system gnu-build-system)
    (arguments
     (list #:configure-flags
           #~(list (string-append "--with-slang="
                                  #$(this-package-input "slang")))
           #:tests? #f                  ; no test suite
           #:phases
           #~(modify-phases %standard-phases
               (add-before 'configure 'fix-configure-script
                 ;; Don't try to link to the long-obsolete (and gone) -ltermcap.
                 (lambda _
                   (substitute* "configure"
                     (("(MISC_TERMINFO_DIRS)=.*" _ variable)
                      (format #f "~a=\"~a/share/terminfo\"\n" variable
                              #$(this-package-input "ncurses")))))))))
    (inputs
     (list ncurses slang))
    (home-page "https://www.jedsoft.org/most/")
    (synopsis
     "@dfn{Pager} (terminal text viewer) with multiple windows and filters")
    (description
     "Most is a paging text viewer.  It displays the contents of a file or the
output of a command on the terminal, one screenful at a time, and lets you
scroll up and down to (re)view the entire text.

You can open multiple windows within @command{most} to view different files, or
to inspect different parts of the same file, at the same time.")
    (license license:gpl2+)))

(define-public newt
  (package
    (name "newt")
    (version "0.52.21")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://pagure.io/releases/newt/"
                                  "newt-" version ".tar.gz"))
              (sha256
               (base32
                "0cdvbancr7y4nrj8257y5n45hmhizr8isynagy4fpsnpammv8pi6"))))
    (build-system gnu-build-system)
    (outputs '("out" "python"))
    (inputs
     (list slang popt python fribidi))
    (arguments
     (list
       #:tests? #f    ; no test suite
       #:configure-flags
       ;; Set the correct RUNPATH in binaries.
       #~(list (string-append "LDFLAGS=-Wl,-rpath=" #$output "/lib"))
       #:make-flags
       ;; configure uses a hard-coded search of /usr/include/python* to set
       ;; this variable, and does not allow us to override it from the
       ;; command line.  Fortunately, the Makefile does, so provide it here.
       #~(list
          (string-append "PYTHONVERS=python"
                         #$(version-major+minor (package-version python))))
       #:phases
       #~(modify-phases %standard-phases
           (add-after 'unpack 'patch-/usr/bin/install
             (lambda _
               (substitute* "po/Makefile"
                 (("/usr/bin/install") "install"))))
           (add-before 'build 'add-python-config-to-path
             (lambda* (#:key target #:allow-other-keys)
               ;; When cross-compiling python-config is not present in $PATH.
               ;;
               ;; It is a shell script without dependencies on target binaries
               ;; so it can be run on the host to allow cross-compilation.
               (when target
                 (let ((path (getenv "PATH"))
                       (py (string-append #$python "/bin")))
                   (setenv "PATH" (string-append path ":" py))))))
           (add-after 'install 'move-python
             (lambda* _
               (let ((ver #$(version-major+minor (package-version python))))
                 (mkdir-p (string-append #$output:python "/lib"))
                 (rename-file
                   (string-append #$output "/lib/python" ver)
                   (string-append #$output:python  "/lib/python" ver))))))))
    (home-page "https://pagure.io/newt")
    (synopsis "Not Erik's Windowing Toolkit - text mode windowing with slang")
    (description
     "Newt is a windowing toolkit for text mode built from the slang library.
It allows color text mode applications to easily use stackable windows, push
buttons, check boxes, radio buttons, lists, entry fields, labels, and
displayable text.  Scrollbars are supported, and forms may be nested to
provide extra functionality.")
    (license license:lgpl2.0)))
