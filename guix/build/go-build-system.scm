;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Petter <petter@mykolab.ch>
;;; Copyright © 2017, 2019 Leo Famulari <leo@famulari.name>
;;; Copyright © 2019 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2020 Jack Hill <jackhill@jackhill.us>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2020, 2021, 2023, 2024 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2021 Sarah Morgensen <iskarian@mgsn.dev>
;;; Copyright © 2024 Ekaitz Zarraga <ekaitz@elenq.tech>
;;; Copyright © 2024 Picnoir <picnoir@alternativebit.fr>
;;; Copyright © 2024 Troy Figiel <troy@troyfigiel.com>
;;; Copyright © 2024 Sharlatan Hellseher <sharlatanus@gmail.com>
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

(define-module (guix build go-build-system)
  #:use-module ((guix build gnu-build-system) #:prefix gnu:)
  #:use-module (guix build union)
  #:use-module (guix build utils)
  #:use-module (ice-9 format)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (rnrs io ports)
  #:use-module (rnrs bytevectors)
  #:export (%standard-phases
            go-build))

;; Commentary:
;;
;; Build procedures for Go packages.  This is the builder-side code.
;;
;; Software written in Go is either a 'package' (i.e. library) or 'command'
;; (i.e. executable).  Both types can be built with either the `go build` or `go
;; install` commands.  However, `go build` discards the result of the build
;; process for Go libraries, so we use `go install`, which preserves the
;; results. [0]

;; Go software is developed and built within a particular file system hierarchy
;; structure called a 'workspace' [1].  This workspace can be found by Go via
;; the GOPATH environment variable.  Typically, all Go source code and compiled
;; objects are kept in a single workspace, but GOPATH may be a list of
;; directories [2].  In this go-build-system we create a file system union of
;; the Go-language dependencies. Previously, we made GOPATH a list of store
;; directories, but stopped because Go programs started keeping references to
;; these directories in Go 1.11:
;; <https://bugs.gnu.org/33620>.
;;
;; Go software, whether a package or a command, is uniquely named using an
;; 'import path'.  The import path is based on the URL of the software's source.
;; Because most source code is provided over the internet, the import path is
;; typically a combination of the remote URL and the source repository's file
;; system structure. For example, the Go port of the common `du` command is
;; hosted on github.com, at <https://github.com/calmh/du>.  Thus, the import
;; path is <github.com/calmh/du>. [3]
;;
;; It may be possible to automatically guess a package's import path based on
;; the source URL, but we don't try that in this revision of the
;; go-build-system.
;;
;; Modules of modular Go libraries are named uniquely with their
;; file system paths.  For example, the supplemental but "standardized"
;; libraries developed by the Go upstream developers are available at
;; <https://golang.org/x/{net,text,crypto, et cetera}>.  The Go IPv4
;; library's import path is <golang.org/x/net/ipv4>.  The source of
;; such modular libraries must be unpacked at the top-level of the
;; file system structure of the library.  So the IPv4 library should be
;; unpacked to <golang.org/x/net>.  This is handled in the
;; go-build-system with the optional #:unpack-path key.
;;
;; In general, Go software is built using a standardized build mechanism
;; that does not require any build scripts like Makefiles.  This means
;; that all modules of modular libraries cannot be built with a single
;; command.  Each module must be built individually.  This complicates
;; certain cases, and these issues are currently resolved by creating a
;; file system union of the required modules of such libraries.  I think
;; this could be improved in future revisions of the go-build-system.
;;
;; TODO:
;; * Avoid copying dependencies into the build environment and / or avoid using
;; a tmpdir when creating the inputs union.
;; * Use Go modules [4]
;; * Re-use compiled packages [5]
;; * Avoid the go-inputs hack
;; * Remove module packages, only offering the full Git repos? This is
;; more idiomatic, I think, because Go downloads Git repos, not modules.
;; What are the trade-offs?
;; * Figurie out how to passthrough --verbosity option to "build" and "check"
;; procedures.
;; * Implement test-backend option, which would be similar to pyproject's
;; one, allowing to provide custom test runner.
;;
;; [0] `go build`:
;; https://golang.org/cmd/go/#hdr-Compile_packages_and_dependencies
;; `go install`:
;; https://golang.org/cmd/go/#hdr-Compile_and_install_packages_and_dependencies
;; [1] Go workspace example, from <https://golang.org/doc/code.html#Workspaces>:
;; bin/
;;     hello                          # command executable
;;     outyet                         # command executable
;; pkg/
;;     linux_amd64/
;;         github.com/golang/example/
;;             stringutil.a           # package object
;; src/
;;     github.com/golang/example/
;;         .git/                      # Git repository metadata
;; 	   hello/
;; 	       hello.go               # command source
;; 	   outyet/
;; 	        main.go               # command source
;; 	        main_test.go          # test source
;; 	   stringutil/
;; 	       reverse.go             # package source
;; 	       reverse_test.go        # test source
;;         golang.org/x/image/
;;             .git/                  # Git repository metadata
;; 	       bmp/
;; 	           reader.go          # package source
;; 	           writer.go          # package source
;;     ... (many more repositories and packages omitted) ...
;;
;; [2] https://golang.org/doc/code.html#GOPATH
;; [3] https://golang.org/doc/code.html#ImportPaths
;; [4] https://golang.org/cmd/go/#hdr-Modules__module_versions__and_more
;; [5] https://bugs.gnu.org/32919
;;
;; Code:

(define* (setup-go-environment #:key inputs outputs goos goarch #:allow-other-keys)
  "Prepare a Go build environment for INPUTS and OUTPUTS.  Build a file system
union of INPUTS.  Export GOPATH, which helps the compiler find the source code
of the package being built and its dependencies, and GOBIN, which determines
where executables (\"commands\") are installed to.  This phase is sometimes used
by packages that use (guix build-system gnu) but have a handful of Go
dependencies, so it should be self-contained."
  (define (search-input-directories dir)
    (filter directory-exists?
            (map (match-lambda
                   ((name . directory)
                    (string-append directory "/" dir)))
                 inputs)))

  ;; Seed the Go build cache with the build caches from input packages.
  (let ((cache (string-append (getcwd) "/go-build")))
    (setenv "GOCACHE" cache)
    (union-build cache
                 (search-input-directories "/var/cache/go/build")
                 ;; Creating all directories isn't that bad, because there are
                 ;; only ever 256 of them.
                 #:create-all-directories? #t
                 #:log-port (%make-void-port "w"))

    ;; Tell Go that the cache was recently trimmed, so it doesn't try to.
    (call-with-output-file (string-append cache "/trim.txt")
      (lambda (port)
        (format port "~a" (current-time)))))

  ;; Using the current working directory as GOPATH makes it easier for packagers
  ;; who need to manipulate the unpacked source code.
  (setenv "GOPATH" (getcwd))
  ;; Go 1.13 uses go modules by default. The go build system does not
  ;; currently support modules, so turn modules off to continue using the old
  ;; GOPATH behavior.
  (setenv "GO111MODULE" "off")
  (setenv "GOBIN" (string-append (assoc-ref outputs "out") "/bin"))

  ;; Make sure we're building for the correct architecture and OS targets
  ;; that Guix targets.
  (setenv "GOARCH" (or goarch
                       (getenv "GOHOSTARCH")))
  (setenv "GOOS" (cond ((and goos
                             (string=? "mingw" goos))
                        "windows")
                       (goos goos)
                       (else (getenv "GOHOSTOS"))))
  (match goarch
    ("arm"
     (setenv "GOARM" "7"))
    ((or "mips" "mipsel")
     (setenv "GOMIPS" "hardfloat"))
    ((or "mips64" "mips64le")
     (setenv "GOMIPS64" "hardfloat"))
    ((or "ppc64" "ppc64le")
     (setenv "GOPPC64" "power8"))
    (_ #t))

  (let ((tmpdir (tmpnam)))
    (match (go-inputs inputs)
      (((names . directories) ...)
       (union-build tmpdir (filter directory-exists? directories)
                    #:create-all-directories? #t
                    #:log-port (%make-void-port "w"))))
    ;; XXX A little dance because (guix build union) doesn't use mkdir-p.
    (copy-recursively tmpdir
                      (string-append (getenv "GOPATH"))
                      #:keep-mtime? #t)
    (delete-file-recursively tmpdir))
  #t)

(define* (fix-embed-files #:key embed-files #:allow-other-keys)
  "Golang cannot determine the valid directory of the module of an embed file
which is symlinked during setup environment phase, but easily resolved after
copying the file from the store to the build directory of the current package.
Take a list of files or regexps matching files from EMBED-FILES parameter,
fail over to 'editions_defaults.binpb' which is a part of
<github.com/golang/protobuf>."
  ;; For the details, consult the Golang source:
  ;;
  ;; - URL: <https://raw.githubusercontent.com/golang/go/>
  ;; - commit: 82c14346d89ec0eeca114f9ca0e88516b2cda454
  ;; - file: src/cmd/go/internal/load/pkg.go
  ;; - line: 2059
  (let ((embed-files (format #f "^(~{~a|~}~a)$"
                             embed-files
                             "editions_defaults.binpb")))
    (for-each (lambda (file)
                (when (eq? (stat:type (lstat file))
                           'symlink)
                  (let ((file-store-path (readlink file)))
                    (delete-file file)
                    (copy-recursively file-store-path file))))
              (find-files "src" embed-files))))

(define* (unpack #:key source import-path unpack-path #:allow-other-keys)
  "Relative to $GOPATH, unpack SOURCE in UNPACK-PATH, or IMPORT-PATH when
UNPACK-PATH is unset.  If the SOURCE archive has a single top level directory,
it is stripped so that the sources appear directly under UNPACK-PATH.  When
SOURCE is a directory, copy its content into UNPACK-PATH instead of
unpacking."
  (define (unpack-maybe-strip source dest)
    (let* ((scratch-dir (string-append (or (getenv "TMPDIR") "/tmp")
                                       "/scratch-dir"))
           (out (mkdir-p scratch-dir)))
      (with-directory-excursion scratch-dir
        (if (string-suffix? ".zip" source)
            (invoke "unzip" source)
            (invoke "tar" "-xvf" source))
        (let ((top-level-files (remove (lambda (x)
                                         (member x '("." "..")))
                                       (scandir "."))))
          (match top-level-files
            ((top-level-file)
             (when (file-is-directory? top-level-file)
               (copy-recursively top-level-file dest #:keep-mtime? #t)))
            (_
             (copy-recursively "." dest #:keep-mtime? #t)))))
      (delete-file-recursively scratch-dir)))

  (when (string-null? import-path)
    (display "WARNING: The Go import path is unset.\n"))
  (let ((dest (string-append (getenv "GOPATH") "/src/"
                             (if (string-null? unpack-path)
                                 import-path
                                 unpack-path))))
    (mkdir-p dest)
    (if (file-is-directory? source)
        (copy-recursively source dest #:keep-mtime? #t)
        (unpack-maybe-strip source dest)))
  #t)

(define (go-package? name)
  (string-prefix? "go-" name))

(define (go-inputs inputs)
  "Return the alist of INPUTS that are Go software."
  ;; XXX This should not check the file name of the store item. Instead we
  ;; should pass, from the host side, the list of inputs that are packages using
  ;; the go-build-system.
  (alist-delete "go" ; Exclude the Go compiler
    (alist-delete "source" ; Exclude the source code of the package being built
      (filter (match-lambda
                ((label . directory)
                 (go-package? ((compose package-name->name+version
                                        strip-store-file-name)
                               directory)))
                (_ #f))
              inputs))))

(define* (build #:key
                build-flags
                skip-build?
                import-path
                (parallel-build? #t)
                (verbosity 1)
                #:allow-other-keys)
  "Build the package named by IMPORT-PATH."
  (let* ((njobs (if parallel-build? (parallel-job-count) 1))
         ;; Utilizing GOFLAGS for flexible build options passthrough, refer
         ;; for more examples to online documentation of Golang
         ;; <https://go.dev/src/cmd/go/testdata/script/goflags.txt>.
         (goflags (string-join
                   (list
                    ;; Print the name of packages (pathes) as they are compiled.
                    "-v"
                    ;; Print each command as it is invoked. When enabled, it
                    ;; generates a lot of noisy logs which makes identifying
                    ;; build failures harder to determine.
                    (if (> verbosity 1) "-x" ""))
                   " ")))
    (setenv "GOFLAGS" goflags)
    (setenv "GOMAXPROCS" (number->string njobs)))

  (with-throw-handler
      #t
    (lambda _
      (if skip-build?
          (begin
            (format #t "Build is skipped, no go files in project's root.~%")
            #t)
          (apply invoke "go" "install"
                 ;; Respectively, strip the symbol table and debug
                 ;; information, and the DWARF symbol table.
                 "-ldflags=-s -w"
                 ;; Remove all file system paths from the resulting
                 ;; executable.  Instead of absolute file system paths, the
                 ;; recorded file names will begin either a module
                 ;; path@version (when using modules), or a plain import path
                 ;; (when using the standard library, or GOPATH).
                 "-trimpath"
                 `(,@build-flags ,import-path))))

    (lambda (key . args)
      (display (string-append "Building '" import-path "' failed.\n"
                              "Here are the results of `go env`:\n"))
      (invoke "go" "env"))))

(define* (check #:key
                tests?
                import-path
                test-flags
                test-subdirs
                (parallel-tests? #t)
                #:allow-other-keys)
  "Run the tests for the package named by IMPORT-PATH."
  (when tests?
    (let* ((njobs (if parallel-tests? (parallel-job-count) 1)))
      (setenv "GOMAXPROCS" (number->string njobs)))
    (apply invoke "go" "test"
           `(,@(map (lambda (dir)
                      (format #f "~a~:[/~;~]~a"
                              import-path (string-null? dir) dir))
                    test-subdirs)
             ,@test-flags)))
  #t)

(define* (install #:key install-source? outputs import-path unpack-path #:allow-other-keys)
  "Install the source code of IMPORT-PATH to the primary output directory.
Compiled executable files (Go \"commands\") should have already been installed
to the store based on $GOBIN in the build phase.
XXX We can't make use of compiled libraries (Go \"packages\")."
  (when install-source?
    (if (string-null? import-path)
        ((display "WARNING: The Go import path is unset.\n")))
    (let* ((out (assoc-ref outputs "out"))
           (source (string-append (getenv "GOPATH") "/src/" import-path))
           (dest (string-append out "/src/" import-path)))
      (mkdir-p dest)
      (copy-recursively source dest #:keep-mtime? #t)))
  #t)

(define* (install-license-files #:key unpack-path
                                import-path
                                #:allow-other-keys
                                #:rest args)
  "Install license files matching LICENSE-FILE-REGEXP to 'share/doc'.  Adjust
the standard install-license-files phase to first enter the correct directory."
  (with-directory-excursion (string-append "src/" (if (string-null? unpack-path)
                                                    import-path
                                                    unpack-path))
    (apply (assoc-ref gnu:%standard-phases 'install-license-files) args)))


(define %standard-phases
  (modify-phases gnu:%standard-phases
    (delete 'bootstrap)
    (delete 'configure)
    (delete 'patch-generated-file-shebangs)
    (add-before 'unpack 'setup-go-environment setup-go-environment)
    (replace 'unpack unpack)
    (add-after 'unpack 'fix-embed-files fix-embed-files)
    (replace 'build build)
    (replace 'check check)
    (replace 'install install)
    (replace 'install-license-files install-license-files)))

(define* (go-build #:key inputs (phases %standard-phases)
                      #:allow-other-keys #:rest args)
  "Build the given Go package, applying all of PHASES in order."
  (apply gnu:gnu-build #:inputs inputs #:phases phases args))
