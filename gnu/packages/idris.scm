;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Paul van der Walt <paul@denknerd.org>
;;; Copyright © 2016, 2017 David Craven <david@craven.ch>
;;; Copyright © 2018 Alex ter Weele <alex.ter.weele@gmail.com>
;;; Copyright © 2019, 2021, 2022 Eric Bavier <bavier@posteo.net>
;;; Copyright © 2022 Attila Lendvai <attila@lendvai.name>
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

(define-module (gnu packages idris)
  #:use-module (gnu packages)
  #:use-module (gnu packages haskell-check)
  #:use-module (gnu packages haskell-web)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages perl)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system haskell)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))

(define-public idris
  (package
    (name "idris")
    (version "1.3.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/idris-lang/Idris-dev.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0cd2a92323hb9a6wy8sc0cqwnisf4pv8y9y2rxvxcbyv8cs1q8g2"))
              (patches (search-patches "idris-test-ffi008.patch"))))
    (build-system haskell-build-system)
    (native-inputs                      ;For tests
     (list perl ghc-cheapskate ghc-tasty ghc-tasty-golden
           ghc-tasty-rerun))
    (inputs
     (list gmp
           ncurses
           ghc-aeson
           ghc-annotated-wl-pprint
           ghc-ansi-terminal
           ghc-ansi-wl-pprint
           ghc-async
           ghc-base64-bytestring
           ghc-blaze-html
           ghc-blaze-markup
           ghc-cheapskate
           ghc-code-page
           ghc-fingertree
           ghc-fsnotify
           ghc-ieee754
           ghc-libffi
           ghc-megaparsec
           ghc-network
           ghc-optparse-applicative
           ghc-regex-tdfa
           ghc-safe
           ghc-split
           ghc-terminal-size
           ghc-uniplate
           ghc-unordered-containers
           ghc-utf8-string
           ghc-vector
           ghc-vector-binary-instances
           ghc-zip-archive))
    (arguments
     `(#:configure-flags
       (list (string-append "--datasubdir="
                            (assoc-ref %outputs "out") "/lib/idris")
             "-fFFI" "-fGMP")
       #:phases
       (modify-phases %standard-phases
         ;; This allows us to call the 'idris' binary before installing.
         (add-after 'unpack 'set-ld-library-path
           (lambda _
             (setenv "LD_LIBRARY_PATH" (string-append (getcwd) "/dist/build"))))
         (add-before 'configure 'update-constraints
           (lambda _
             (substitute* "idris.cabal"
               (("(aeson|ansi-terminal|bytestring|haskeline|libffi|megaparsec|network|optparse-applicative)\\s+[<>=0-9. &|]+" all dep)
                dep))))
         (add-before 'configure 'set-cc-command
           (lambda _
             (setenv "CC" ,(cc-for-target))))
         (add-after 'install 'fix-libs-install-location
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (lib (string-append out "/lib/idris"))
                    (modules (string-append lib "/libs")))
               (for-each
                (lambda (module)
                  (symlink (string-append modules "/" module)
                           (string-append lib "/" module)))
                '("prelude" "base" "contrib" "effects" "pruviloj")))))
         (delete 'check)                ;Run check later
         (add-after 'install 'check
           (lambda* (#:key outputs #:allow-other-keys #:rest args)
             (let ((out (assoc-ref outputs "out")))
               (chmod "test/scripts/timeout" #o755) ;must be executable
               (setenv "TASTY_NUM_THREADS" (number->string (parallel-job-count)))
               (setenv "IDRIS_CC" ,(cc-for-target)) ;Needed for creating executables
               (setenv "PATH" (string-append out "/bin:" (getenv "PATH")))
               (apply (assoc-ref %standard-phases 'check) args)))))))
    (native-search-paths
     (list (search-path-specification
            (variable "IDRIS_LIBRARY_PATH")
            (files '("lib/idris")))))
    (home-page "https://www.idris-lang.org")
    (synopsis "General purpose language with full dependent types")
    (description "Idris is a general purpose language with full dependent
types.  It is compiled, with eager evaluation.  Dependent types allow types to
be predicated on values, meaning that some aspects of a program's behaviour
can be specified precisely in the type.  The language is closely related to
Epigram and Agda.")
    (license license:bsd-3)))

;; Idris modules use the gnu-build-system so that the IDRIS_LIBRARY_PATH is set.
(define (idris-default-arguments name)
  `(#:modules ((guix build gnu-build-system)
               (guix build utils)
               (ice-9 ftw)
               (ice-9 match))
    #:phases
    (modify-phases %standard-phases
      (delete 'configure)
      (delete 'build)
      (delete 'check)
      (replace 'install
        (lambda* (#:key inputs outputs #:allow-other-keys)
          (let* ((out (assoc-ref outputs "out"))
                 (idris (assoc-ref inputs "idris"))
                 (idris-bin (string-append idris "/bin/idris"))
                 (idris-libs (string-append idris "/lib/idris/libs"))
                 (module-name (and (string-prefix? "idris-" ,name)
                                   (substring ,name 6)))
                 (ibcsubdir (string-append out "/lib/idris/" module-name))
                 (ipkg (string-append module-name ".ipkg"))
                 (idris-library-path (getenv "IDRIS_LIBRARY_PATH"))
                 (idris-path (string-split idris-library-path #\:))
                 (idris-path-files (apply append
                                          (map (lambda (path)
                                                 (map (lambda (dir)
                                                        (string-append path "/" dir))
                                                      (scandir path))) idris-path)))
                 (idris-path-subdirs (filter (lambda (path)
                                               (and path (match (stat:type (stat path))
                                                           ('directory #t)
                                                           (_ #f))))
                                                    idris-path-files))
                 (install-cmd (cons* idris-bin
                                     "--ibcsubdir" ibcsubdir
                                     "--build" ipkg
                                     ;; only trigger a build, as --ibcsubdir
                                     ;; already installs .ibc files.

                                     (apply append (map (lambda (path)
                                                          (list "--idrispath"
                                                                path))
                                                        idris-path-subdirs)))))
            ;; FIXME: Seems to be a bug in idris that causes a dubious failure.
            (apply system* install-cmd)))))))
