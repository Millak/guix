;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2019 Björn Höfling <bjoern.hoefling@bjoernhoefling.de>
;;; Copyright © 2020, 2022 Hartmut Goebel <h.goebel@crazy-compilers.com>
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

(define-module (guix build rebar-build-system)
  #:use-module ((guix build gnu-build-system) #:prefix gnu:)
  #:use-module ((guix build utils) #:hide (delete))
  #:use-module (ice-9 match)
  #:use-module (ice-9 ftw)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (rebar-build
            %standard-phases))

;;
;; Builder-side code of the standard build procedure for Erlang packages using
;; rebar3.
;;
;; TODO: Think about whether bindir ("ebin"), libdir ("priv") and includedir
;; "(include") need to be configurable

(define %erlang-libdir "/lib/erlang/lib")

(define* (erlang-depends #:key inputs #:allow-other-keys)
  (define input-directories
    (match inputs
      (((_ . dir) ...)
       dir)))
  (mkdir-p "_checkouts")

  (for-each
   (lambda (input-dir)
     (let ((elibdir (string-append input-dir %erlang-libdir)))
       (when (directory-exists? elibdir)
         (for-each
          (lambda (dirname)
            (let ((dest (string-append elibdir "/" dirname))
                  (link (string-append "_checkouts/" dirname)))
              (when (not (file-exists? link))
                ;; RETHINK: Maybe better copy and make writable to avoid some
                ;; error messages e.g. when using with rebar3-git-vsn.
                (symlink dest link))))
          (list-directories elibdir)))))
   input-directories))

(define* (unpack #:key source #:allow-other-keys)
  "Unpack SOURCE in the working directory, and change directory within the
source.  When SOURCE is a directory, copy it in a sub-directory of the current
working directory."
  (let ((gnu-unpack (assoc-ref gnu:%standard-phases 'unpack)))
    (gnu-unpack #:source source)
    ;; Packages from hex.pm typically have a contents.tar.gz containing the
    ;; actual source. If this tar file exists, extract it.
    (when (file-exists? "contents.tar.gz")
      (invoke "tar" "xvf" "contents.tar.gz"))))

(define* (build #:key (rebar-flags '()) #:allow-other-keys)
  (apply invoke `("rebar3" "compile" ,@rebar-flags)))

(define* (check #:key target (rebar-flags '()) (tests? (not target))
                (test-target "eunit")
                #:allow-other-keys)
  (if tests?
      (apply invoke `("rebar3" ,test-target ,@rebar-flags))
      (format #t "test suite not run~%")))

(define (erlang-package? name)
  "Check if NAME correspond to the name of an Erlang package."
  (string-prefix? "erlang-" name))

(define (package-name-version->erlang-name name+ver)
  "Convert the Guix package NAME-VER to the corresponding Erlang name-version
format.  Essentially drop the prefix used in Guix and replace dashes by
underscores."
  (let* ((name- (package-name->name+version name+ver)))
    (string-join
     (string-split
      (if (erlang-package? name-)  ; checks for "erlang-" prefix
          (string-drop name- (string-length "erlang-"))
          name-)
      #\-)
     "_")))

(define (list-directories directory)
  "Return file names of the sub-directory of DIRECTORY."
  (scandir directory
           (lambda (file)
             (and (not (member file '("." "..")))
                  (file-is-directory? (string-append directory "/" file))))))

(define* (install #:key name outputs
                  (install-name (package-name-version->erlang-name name))
                  (install-profile "default") ; build profile outputs to install
                  #:allow-other-keys)
  (let* ((out (assoc-ref outputs "out"))
         (pkg-dir (string-append out %erlang-libdir "/" install-name)))
    (let ((bin-dir (string-append "_build/" install-profile "/bin"))
          (lib-dir (string-append "_build/" install-profile "/lib")))
      ;; install _build/PROFILE/bin
      (when (file-exists? bin-dir)
        (copy-recursively bin-dir out #:follow-symlinks? #t))
      ;; install _build/PROFILE/lib/*/{ebin,include,priv}
      (for-each
       (lambda (*)
         (for-each
          (lambda (dirname)
            (let ((src-dir (string-append lib-dir "/" * "/" dirname))
                  (dst-dir (string-append pkg-dir "/" dirname)))
              (when (file-exists? src-dir)
                (copy-recursively src-dir dst-dir #:follow-symlinks? #t))
              (false-if-exception
               (delete-file (string-append dst-dir "/.gitignore")))))
          '("ebin" "include" "priv")))
       (list-directories lib-dir))
      (false-if-exception
       (delete-file (string-append pkg-dir "/priv/Run-eunit-loop.expect"))))))

(define %standard-phases
  (modify-phases gnu:%standard-phases
    (replace 'unpack unpack)
    (delete 'bootstrap)
    (delete 'configure)
    (add-before 'build 'erlang-depends erlang-depends)
    (replace 'build build)
    (replace 'check check)
    (replace 'install install)))

(define* (rebar-build #:key inputs (phases %standard-phases)
                      #:allow-other-keys #:rest args)
  "Build the given Erlang package, applying all of PHASES in order."
  (apply gnu:gnu-build #:inputs inputs #:phases phases args))
