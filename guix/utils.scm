;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012-2022, 2024-2025 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013, 2014, 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2014 Ian Denhardt <ian@zenhack.net>
;;; Copyright © 2016 Mathieu Lirzin <mthl@gnu.org>
;;; Copyright © 2015 David Thompson <davet@gnu.org>
;;; Copyright © 2017 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2018, 2020 Marius Bakke <marius@gnu.org>
;;; Copyright © 2020, 2021, 2024 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2020, 2021, 2022 Maxim Cournoyer <maxim@guixotic.coop>
;;; Copyright © 2021 Simon Tournier <zimon.toutoune@gmail.com>
;;; Copyright © 2021 Chris Marusich <cmmarusich@gmail.com>
;;; Copyright © 2021 Maxime Devos <maximedevos@telenet.be>
;;; Copyright © 2018 Steve Sprang <scs@stevesprang.com>
;;; Copyright © 2022 Taiju HIGASHI <higashi@taiju.info>
;;; Copyright © 2022 Denis 'GNUtoo' Carikli <GNUtoo@cyberdimension.org>
;;; Copyright © 2022 Antero Mejr <antero@mailbox.org>
;;; Copyright © 2023 Philip McGrath <philip@philipmcgrath.com>
;;; Copyright © 2023, 2024 Janneke Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2023 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2023 Foundation Devices, Inc. <hello@foundationdevices.com>
;;; Copyright © 2024 Herman Rimm <herman@rimm.ee>
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

(define-module (guix utils)
  #:use-module (guix config)
  #:autoload   (guix read-print) (object->string*)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-71)
  #:use-module (rnrs io ports)                    ;need 'port-position' etc.
  #:use-module ((rnrs bytevectors) #:select (bytevector-u8-set!))
  #:use-module (guix memoization)
  #:autoload   (guix build utils) (dump-port
                                   mkdir-p
                                   delete-file-recursively
                                   %xz-parallel-args)
  #:autoload   (guix build syscalls) (mkdtemp! fdatasync)
  #:autoload   (guix combinators) (fold2)
  #:use-module (guix diagnostics)           ;<location>, &error-location, etc.
  #:use-module (ice-9 format)
  #:use-module ((ice-9 iconv) #:prefix iconv:)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 vlist)
  #:autoload   (zlib) (make-zlib-input-port make-zlib-output-port)
  #:use-module (system foreign)
  #:re-export (<location>                         ;for backwards compatibility
               location
               location?
               location-file
               location-line
               location-column
               source-properties->location
               location->source-properties

               &error-location
               error-location?
               error-location

               &fix-hint
               fix-hint?
               condition-fix-hint)
  #:export (strip-keyword-arguments
            default-keyword-arguments
            substitute-keyword-arguments
            ensure-keyword-arguments

            %guix-source-root-directory
            current-source-directory

            nix-system->gnu-triplet
            gnu-triplet->nix-system
            %current-system
            %current-target-system
            package-name->name+version
            downstream-package-name
            target-linux?
            target-hurd?
            system-hurd?
            target-hurd64?
            system-hurd64?
            host-hurd?
            host-hurd64?
            host-x86-64?
            target-mingw?
            target-x86-32?
            target-x86-64?
            target-x86?
            target-x32?
            target-arm32?
            target-aarch64?
            target-arm?
            target-avr?
            target-ppc32?
            target-ppc64le?
            target-powerpc?
            target-riscv64?
            target-loongarch64?
            target-mips64el?
            target-64bit?
            target-little-endian?
            ar-for-target
            as-for-target
            cc-for-target
            cxx-for-target
            ld-for-target
            pkg-config-for-target
            strip-for-target

            version-compare
            version>?
            version>=?
            version-prefix
            version-major+minor+point
            version-major+minor
            version-major
            version-unique-prefix
            guile-version>?
            version-prefix?
            string-replace-substring
            file-extension
            file-sans-extension
            tarball-sans-extension
            compressed-file?
            call-with-temporary-directory
            with-atomic-file-output

            with-environment-variables
            arguments-from-environment-variable

            config-directory
            cache-directory

            readlink*
            go-to-location
            edit-expression
            delete-expression
            insert-expression
            find-definition-location
            find-definition-insertion-location

            filtered-port
            decompressed-port
            call-with-decompressed-port
            compressed-output-port
            call-with-compressed-output-port
            canonical-newline-port

            string-distance
            string-closest

            pretty-print-table))


;;;
;;; Environment variables.
;;;

(define (call-with-environment-variables variables thunk)
  "Call THUNK with the environment VARIABLES set."
  (let ((environment (environ)))
    (dynamic-wind
      (lambda ()
        (for-each (match-lambda
                    ((variable #false)
                     (unsetenv variable))
                    ((variable value)
                     (setenv variable value)))
                  variables))
      thunk
      (lambda ()
        (environ environment)))))

(define-syntax-rule (with-environment-variables variables exp ...)
  "Evaluate EXP with the given environment VARIABLES set."
  (call-with-environment-variables variables
                                   (lambda () exp ...)))

(define (arguments-from-environment-variable variable)
  "Retrieve value of environment variable denoted by string VARIABLE in the
form of a list of strings (`char-set:graphic' tokens) suitable for consumption
by `args-fold', if VARIABLE is defined, otherwise return an empty list."
  (let ((env (getenv variable)))
    (if env
        (string-tokenize env char-set:graphic)
        '())))


;;;
;;; Filtering & pipes.
;;;

(define (filtered-port command input)
  "Return an input port where data drained from INPUT is filtered through
COMMAND (a list).  In addition, return a list of PIDs that the caller must
wait.  When INPUT is a file port, it must be unbuffered; otherwise, any
buffered data is lost."
  (let loop ((input input)
             (pids  '()))
    (if (file-port? input)
        (match (pipe)
          ((in . out)
           (match (primitive-fork)
             (0
              (dynamic-wind
                (const #f)
                (lambda ()
                  (close-port in)
                  (close-port (current-input-port))
                  (dup2 (fileno input) 0)
                  (close-port (current-output-port))
                  (dup2 (fileno out) 1)
                  (catch 'system-error
                    (lambda ()
                      (apply execl (car command) command))
                    (lambda args
                      (format (current-error-port)
                              "filtered-port: failed to execute '~{~a ~}': ~a~%"
                              command (strerror (system-error-errno args))))))
                (lambda ()
                  (primitive-_exit 1))))
             (child
              (close-port out)
              (values in (cons child pids))))))

        ;; INPUT is not a file port, so fork just for the sake of tunneling it
        ;; through a file port.
        (match (pipe)
          ((in . out)
           (match (primitive-fork)
             (0
              (dynamic-wind
                (const #t)
                (lambda ()
                  (close-port in)
                  (dump-port input out))
                (lambda ()
                  (close-port input)
                  (false-if-exception (close out))
                  (primitive-_exit 0))))
             (child
              (close-port input)
              (close-port out)
              (loop in (cons child pids)))))))))

(define (lzip-port proc port . args)
  "Return the lzip port produced by calling PROC (a symbol) on PORT and ARGS.
Raise an error if lzlib support is missing."
  (let ((make-port (module-ref (resolve-interface '(lzlib)) proc)))
    (make-port port)))

(define (zstd-port proc port . args)
  "Return the zstd port produced by calling PROC (a symbol) on PORT and ARGS.
Raise an error if zstd support is missing."
  (let ((make-port (module-ref (resolve-interface '(zstd)) proc)))
    (make-port port)))

(define (decompressed-port compression input)
  "Return an input port where INPUT is decompressed according to COMPRESSION,
a symbol such as 'xz."
  (match compression
    ((or #f 'none) (values input '()))
    ('bzip2        (filtered-port `(,%bzip2 "-dc") input))
    ('xz           (filtered-port `(,%xz "-dc") input))
    ('gzip         (values (make-zlib-input-port input #:format 'gzip)
                           '()))
    ('lzip         (values (lzip-port 'make-lzip-input-port input)
                           '()))
    ('zstd         (values (zstd-port 'make-zstd-input-port input)
                           '()))
    (_             (error "unsupported compression scheme" compression))))

(define (compressed-port compression input)
  "Return an input port where INPUT is compressed according to COMPRESSION,
a symbol such as 'xz."
  (match compression
    ((or #f 'none) (values input '()))
    ('bzip2        (filtered-port `(,%bzip2 "-c") input))
    ('xz           (filtered-port `(,%xz "-c" ,@(%xz-parallel-args)) input))
    ('gzip         (filtered-port `(,%gzip "-c") input))
    ('lzip         (values (lzip-port 'make-lzip-input-port/compressed input)
                           '()))
    (_             (error "unsupported compression scheme" compression))))

(define (call-with-decompressed-port compression port proc)
  "Call PROC with a wrapper around PORT, a file port, that decompresses data
read from PORT according to COMPRESSION, a symbol such as 'xz."
  (let-values (((decompressed pids)
                (decompressed-port compression port)))
    (dynamic-wind
      (const #f)
      (lambda ()
        (proc decompressed))
      (lambda ()
        (close-port decompressed)
        (unless (every (compose zero? cdr waitpid) pids)
          (error "decompressed-port failure" pids))))))

(define (filtered-output-port command output)
  "Return an output port.  Data written to that port is filtered through
COMMAND and written to OUTPUT, an output file port.  In addition, return a
list of PIDs to wait for.  OUTPUT must be unbuffered; otherwise, any buffered
data is lost."
  (match (pipe)
    ((in . out)
     (match (primitive-fork)
       (0
        (dynamic-wind
          (const #f)
          (lambda ()
            (close-port out)
            (close-port (current-input-port))
            (dup2 (fileno in) 0)
            (close-port (current-output-port))
            (dup2 (fileno output) 1)
            (catch 'system-error
              (lambda ()
                (apply execl (car command) command))
              (lambda args
                (format (current-error-port)
                        "filtered-output-port: failed to execute '~{~a ~}': ~a~%"
                        command (strerror (system-error-errno args))))))
          (lambda ()
            (primitive-_exit 1))))
       (child
        (close-port in)
        (values out (list child)))))))

(define* (compressed-output-port compression output
                                 #:key (options '()))
  "Return an output port whose input is compressed according to COMPRESSION,
a symbol such as 'xz, and then written to OUTPUT.  In addition return a list
of PIDs to wait for.  OPTIONS is a list of strings passed to the compression
program--e.g., '(\"--fast\")."
  (match compression
    ((or #f 'none) (values output '()))
    ('bzip2        (filtered-output-port `(,%bzip2 "-c" ,@options) output))
    ('xz           (filtered-output-port `(,%xz "-c" ,@options) output))
    ('gzip         (values (make-zlib-output-port output #:format 'gzip)
                           '()))
    ('lzip         (values (lzip-port 'make-lzip-output-port output)
                           '()))
    ('zstd         (values (zstd-port 'make-zstd-output-port output)
                           '()))
    (_             (error "unsupported compression scheme" compression))))

(define* (call-with-compressed-output-port compression port proc
                                           #:key (options '()))
  "Call PROC with a wrapper around PORT, a file port, that compresses data
that goes to PORT according to COMPRESSION, a symbol such as 'xz.  OPTIONS is
a list of command-line arguments passed to the compression program."
  (let-values (((compressed pids)
                (compressed-output-port compression port
                                        #:options options)))
    (dynamic-wind
      (const #f)
      (lambda ()
        (proc compressed))
      (lambda ()
        (close-port compressed)
        (unless (every (compose zero? cdr waitpid) pids)
          (error "compressed-output-port failure" pids))))))

(define %source-location-map
  ;; Maps inode/device tuples to "source location maps" used by
  ;; 'go-to-location'.
  (make-hash-table))

(define (source-location-key/stamp stat)
  "Return two values: the key for STAT in %SOURCE-LOCATION-MAP, and a stamp
used to invalidate corresponding entries."
  (let ((key   (list (stat:ino stat) (stat:dev stat)))
        (stamp (list (stat:mtime stat) (stat:mtimensec stat)
                     (stat:size stat))))
    (values key stamp)))

(define* (go-to-location port line column)
  "Jump to LINE and COLUMN (both one-indexed) in PORT.  Maintain a source
location map such that this can boil down to seek(2) and a few read(2) calls,
which can drastically speed up repetitive operations on large files."
  (let* ((stat       (stat port))
         (key stamp  (source-location-key/stamp stat))

         ;; Look for an up-to-date source map for KEY.  The map is a vlist
         ;; where each entry gives the byte offset of the beginning of a line:
         ;; element 0 is the offset of the first line, element 1 the offset of
         ;; the second line, etc.  The map is filled lazily.
         (source-map (match (hash-ref %source-location-map key)
                       (#f
                        (vlist-cons 0 vlist-null))
                       ((cache-stamp ... map)
                        (if (equal? cache-stamp stamp) ;invalidate?
                            map
                            (vlist-cons 0 vlist-null)))))
         (last       (vlist-length source-map)))
    ;; Jump to LINE, ideally via SOURCE-MAP.
    (if (<= line last)
        (seek port (vlist-ref source-map (- line 1)) SEEK_SET)
        (let ((target line)
              (offset (vlist-ref source-map (- last 1))))
          (seek port offset SEEK_SET)
          (let loop ((source-map (vlist-reverse source-map))
                     (line last))
            (if (< line target)
                (match (read-char port)
                  (#\newline
                   (loop (vlist-cons (ftell port) source-map)
                         (+ 1 line)))
                  ((? eof-object?)
                   (error "unexpected end of file" port line))
                  (chr (loop source-map line)))
                (hash-set! %source-location-map key
                           `(,@stamp
                             ,(vlist-reverse source-map)))))))

    ;; Read up to COLUMN.
    (let ((target column))
      (let loop ((column 1))
        (when (< column target)
          (match (read-char port)
            (#\newline (error "unexpected end of line" port))
            (#\tab (loop (+ 8 column)))
            (chr (loop (+ 1 column)))))))

    ;; Update PORT's position info.
    (set-port-line! port (- line 1))
    (set-port-column! port (- column 1))))

(define (move-source-location-map! source target line)
  "Move the source location map from SOURCE up to LINE to TARGET.  SOURCE and
TARGET must be stat buffers as returned by 'stat'."
  (let* ((source-key (source-location-key/stamp source))
         (target-key target-stamp (source-location-key/stamp target)))
    (match (hash-ref %source-location-map source-key)
      (#f #t)
      ((_ ... source-map)
       ;; Strip the source map and update the associated stamp.
       (let ((source-map (vlist-take source-map (max line 1))))
         (hash-remove! %source-location-map source-key)
         (hash-set! %source-location-map target-key
                    `(,@target-stamp ,source-map)))))))

(define* (edit-expression source-properties proc #:key (encoding "UTF-8")
                          include-trailing-newline?)
  "Edit the expression specified by SOURCE-PROPERTIES using PROC, which should
be a procedure that takes the original expression in string and returns a new
one.  ENCODING will be used to interpret all port I/O, it defaults to UTF-8.
This procedure returns #t on success.  When INCLUDE-TRAILING-NEWLINE? is true,
the trailing line is included in the edited expression."
  (define file   (assq-ref source-properties 'filename))
  (define line   (assq-ref source-properties 'line))
  (define column (assq-ref source-properties 'column))

  (with-fluids ((%default-port-encoding encoding))
    (call-with-input-file file
      (lambda (in)
        (let* ( ;; The start byte position of the expression.
               (start  (begin (go-to-location
                               in (+ 1 line) (+ 1 column))
                              (ftell in)))
               ;; The end byte position of the expression.
               (end    (begin (read in)
                              (when include-trailing-newline?
                                (read-line in))
                              (ftell in))))
          (seek in 0 SEEK_SET)          ; read from the beginning of the file.
          (let* ((pre-bv  (get-bytevector-n in start))
                 ;; The expression in string form.
                 (str     (iconv:bytevector->string
                           (get-bytevector-n in (- end start))
                           (port-encoding in)))
                 (str*    (proc str)))
            ;; Modify FILE only if there are changes.
            (unless (string=? str* str)
              ;; Verify the edited expression is still a scheme expression.
              (call-with-input-string str* read)

              (let ((post-bv (get-bytevector-all in)))
                ;; Update the file with edited expression.
                (with-atomic-file-output file
                  (lambda (out)
                    (put-bytevector out pre-bv)
                    (display str* out)
                    (unless (eof-object? post-bv)
                      ;; Copy everything that came after STR.
                      (put-bytevector out post-bv))))

                ;; Due to 'with-atomic-file-output', IN and FILE no longer
                ;; share the same inode, but we can reassign the source map up
                ;; to LINE to the new file.
                (move-source-location-map! (stat in) (stat file)
                                           (+ 1 line))))))))))

(define (delete-expression source-properties)
  "Delete the expression specified by SOURCE-PROPERTIES."
  (edit-expression source-properties (const "") #:include-trailing-newline? #t))

(define (insert-expression source-properties expr)
  "Insert EXPR before the top-level expression specified by
SOURCE-PROPERTIES."
  (let* ((expr (object->string* expr 0))
         (insert (lambda (str)
                   (string-append expr "\n\n" str))))
    (edit-expression source-properties insert)))

(define* (find-definition-location file term
                                   #:key (define-prefix 'define-public)
                                   (pred string=))
  "Search in FILE for a top-level definition created using DEFINE-PREFIX, with
the defined term compared to TERM through PRED.  Return the location if PRED
returns #t, or #f otherwise."
  (let ((search-term (symbol->string term))
        (define-prefix? (cut eq? define-prefix <>)))
    (call-with-input-file file
      (lambda (port)
        (do ((syntax (read-syntax port)
                     (read-syntax port)))
          ((match (syntax->datum syntax)
             (((? define-prefix?) current-term _ ...)
              (pred (symbol->string current-term)
                    search-term))
             ((? eof-object?) #t)
             (_ #f))
           (and (not (eof-object? syntax))
                (syntax-source syntax))))))))

(define* (find-definition-insertion-location file term
                                             #:key
                                             (define-prefix 'define-public))
  "Search in FILE for a top-level definition created using DEFINE-PREFIX, with
the defined term alphabetically succeeds TERM.  Return the location if found,
or #f otherwise."
  (find-definition-location
   file term #:define-prefix define-prefix #:pred string>))


;;;
;;; Keyword arguments.
;;;

(define (strip-keyword-arguments keywords args)
  "Remove all of the keyword arguments listed in KEYWORDS from ARGS."
  (let loop ((args   args)
             (result '()))
    (match args
      (()
       (reverse result))
      (((? keyword? kw) arg . rest)
       (loop rest
             (if (memq kw keywords)
                 result
                 (cons* arg kw result))))
      ((head . tail)
       (loop tail (cons head result))))))

(define (default-keyword-arguments args defaults)
  "Return ARGS augmented with any keyword/value from DEFAULTS for
keywords not already present in ARGS."
  (let loop ((defaults defaults)
             (args     args))
    (match defaults
      ((kw value rest ...)
       (loop rest
             (if (memq kw args)
                 args
                 (cons* kw value args))))
      (()
       args))))

(define-syntax collect-default-args
  (syntax-rules ()
    ((_)
     '())
    ((_ (_ _) rest ...)
     (collect-default-args rest ...))
    ((_ (kw _ dflt) rest ...)
     (cons* kw dflt (collect-default-args rest ...)))))

(define-syntax substitute-keyword-arguments
  (syntax-rules ()
    "Return a new list of arguments where the value for keyword arg KW is
replaced by EXP.  EXP is evaluated in a context where VAR is bound to the
previous value of the keyword argument, or DFLT if given."
    ((_ original-args ((kw var dflt ...) exp) ...)
     (let loop ((args (default-keyword-arguments
                        original-args
                        (collect-default-args (kw var dflt ...) ...)))
                (before '()))
       (match args
         ((kw var rest (... ...))
          (loop rest (cons* exp kw before)))
         ...
         ((x rest (... ...))
          (loop rest (cons x before)))
         (()
          (reverse before)))))))

(define (delkw kw lst)
  "Remove KW and its associated value from LST, a keyword/value list such
as '(#:foo 1 #:bar 2)."
  (let loop ((lst    lst)
             (result '()))
    (match lst
      (()
       (reverse result))
      ((kw? value rest ...)
       (if (eq? kw? kw)
           (append (reverse result) rest)
           (loop rest (cons* value kw? result)))))))

(define (ensure-keyword-arguments args kw/values)
  "Force the keywords arguments KW/VALUES in the keyword argument list ARGS.
For instance:

  (ensure-keyword-arguments '(#:foo 2) '(#:foo 2))
  => (#:foo 2)

  (ensure-keyword-arguments '(#:foo 2) '(#:bar 3))
  => (#:foo 2 #:bar 3)

  (ensure-keyword-arguments '(#:foo 2) '(#:bar 3 #:foo 42))
  => (#:foo 42 #:bar 3)
"
  (let loop ((args      args)
             (kw/values kw/values)
             (result    '()))
    (match args
      (()
       (append (reverse result) kw/values))
      ((kw value rest ...)
       (match (memq kw kw/values)
         ((_ value . _)
          (loop rest (delkw kw kw/values) (cons* value kw result)))
         (#f
          (loop rest kw/values (cons* value kw result))))))))


;;;
;;; System strings.
;;;

(define* (nix-system->gnu-triplet
          #:optional (system (%current-system)) (vendor "unknown"))
  "Return a guess of the GNU triplet corresponding to Nix system
identifier SYSTEM."
  (match system
    ("armhf-linux"
     (string-append "arm-" vendor "-linux-gnueabihf"))
    (_
     (let* ((dash (string-index system #\-))
            (arch (substring system 0 dash))
            (os   (substring system (+ 1 dash))))
       (string-append arch
                      "-" vendor "-"
                      (if (string=? os "linux")
                          "linux-gnu"
                          os))))))

(define (gnu-triplet->nix-system triplet)
  "Return the Nix system type corresponding to TRIPLET, a GNU triplet as
returned by `config.guess'."
  (let ((triplet (cond ((string-match "^i[345]86-(.*)$" triplet)
                        =>
                        (lambda (m)
                          (string-append "i686-" (match:substring m 1))))
                       (else triplet))))
    (cond ((string-match "^arm[^-]*-([^-]+-)?linux-gnueabihf" triplet)
           "armhf-linux")
          ;; Otherwise it will show up as x86_64-linux... which isn't wrong.
          ((string-match "x86_64-linux-gnux32" triplet) "x86_64-linux-gnux32")
          ((string-match "^([^-]+)-([^-]+-)?linux-gnu.*" triplet)
           =>
           (lambda (m)
             ;; Nix omits `-gnu' for GNU/Linux.
             (string-append (match:substring m 1) "-linux")))
          ((string-match "^([^-]+)-([^-]+-)?([[:alpha:]]+)([0-9]+\\.?)*$" triplet)
           =>
           (lambda (m)
             ;; Nix strip the version number from names such as `gnu0.3',
             ;; `darwin10.2.0', etc., and always strips the vendor part.
             (string-append (match:substring m 1) "-"
                            (match:substring m 3))))
          (else triplet))))

(define %current-system
  ;; System type as expected by Nix, usually ARCHITECTURE-KERNEL.
  ;; By default, this is equal to (gnu-triplet->nix-system %host-type).
  (make-parameter %system))

(define %current-target-system
  ;; Either #f or a GNU triplet representing the target system we are
  ;; cross-building to.
  (make-parameter #f))

(define* (package-name->name+version spec
                                     #:optional (delimiter #\@))
  "Given SPEC, a package name like \"foo@0.9.1b\", return two values: \"foo\"
and \"0.9.1b\".  When the version part is unavailable, SPEC and #f are
returned.  Both parts must not contain any '@'.  Optionally, DELIMITER can be
a character other than '@'."
  (match (string-rindex spec delimiter)
    (#f  (values spec #f))
    (idx (values (substring spec 0 idx)
                 (substring spec (1+ idx))))))

(define (downstream-package-name prefix name)
  "Return the Guix package name for a given package NAME."
  (string-append prefix (string-map (match-lambda
                                      (#\_ #\-)
                                      (#\. #\-)
                                      (chr (char-downcase chr)))
                                    name)))

(define* (target-linux? #:optional (target (or (%current-target-system)
                                               (%current-system))))
  "Does the operating system of TARGET use the Linux kernel?"
  (->bool (string-contains target "linux")))

(define* (target-hurd? #:optional (target (or (%current-target-system)
                                              (%current-system))))
  "Does TARGET represent the GNU(/Hurd) system?"
  (and (string-suffix? "-gnu" target)
       (not (string-contains target "linux"))))

(define* (system-hurd?)
  "Is the current system the GNU(/Hurd) system?"
  (and=> (%current-system) target-hurd?))

(define* (target-hurd64? #:optional (target (or (%current-target-system)
                                                (%current-system))))
  "Does TARGET represent the 64bit GNU(/Hurd) system?"
  (and (target-hurd?)
       (target-64bit? target)))

(define* (system-hurd64?)
  "Is the current system the 64bit GNU(/Hurd) system?"
  (and (system-hurd?)
       (target-64bit? (%current-system))))

(define (host-hurd?)
  "Are we running on a Hurd system?  This is almost never the right function,
use target-hurd? or system-hurd? instead."
  (equal? (utsname:sysname (uname)) "GNU"))

(define (host-x86-64?)
  "Are we running on a x86_64 system?  This is almost never the right
function, use target-x86-64? or system-x86-64? instead."
  (equal? (utsname:machine (uname)) "x86_64"))

(define (host-hurd64?)
  "Are we running on a 64bit Hurd?  This is almost never the right
function, use target-hurd64? or system-hurd64? instead."
  (and (host-hurd?)
       (host-x86-64?)))

(define* (target-mingw? #:optional (target (%current-target-system)))
  "Is the operating system of TARGET Windows?"
  (and target
       ;; The "-32" doesn't mean TARGET is 32-bit, as "x86_64-w64-mingw32"
       ;; is a valid triplet (see the (gnu ci) module) and 'w64' and 'x86_64'
       ;; are 64-bit.
       (string-suffix? "-mingw32" target)))

(define* (target-x86-32? #:optional (target (or (%current-target-system)
                                                (%current-system))))
  "Is the architecture of TARGET a variant of Intel's 32-bit architecture
(IA32)?"
  ;; Intel also has a 16-bit architecture in the iN86 series, i286
  ;; (see, e.g., https://en.wikipedia.org/wiki/Intel_80286) so this
  ;; procedure is not named target-x86?.
  (or (string-prefix? "i386-" target)
      (string-prefix? "i486-" target)
      (string-prefix? "i586-" target)
      (string-prefix? "i686-" target)))

(define* (target-x86-64? #:optional (target (or (%current-target-system)
                                                 (%current-system))))
  "Is the architecture of TARGET a variant of Intel/AMD's 64-bit
architecture (x86_64)?"
  (string-prefix? "x86_64-" target))

(define* (target-x32? #:optional (target (or (%current-target-system)
                                             (%current-system))))
  "Is the architecture of TARGET a variant of Intel/AMD's 64-bit
architecture (x86_64) using 32-bit data types?"
  (and (target-x86-64? target)
       (string-suffix? "gnux32" target)))

(define* (target-x86? #:optional (target (or (%current-target-system)
                                             (%current-system))))
  (or (target-x86-32? target) (target-x86-64? target)))

(define* (target-arm32? #:optional (target (or (%current-target-system)
                                               (%current-system))))
  (string-prefix? "arm" target))

(define* (target-aarch64? #:optional (target (or (%current-target-system)
                                                 (%current-system))))
  (string-prefix? "aarch64" target))

(define* (target-arm? #:optional (target (or (%current-target-system)
                                             (%current-system))))
  (or (target-arm32? target) (target-aarch64? target)))

(define* (target-avr? #:optional (target (%current-target-system)))
  "Is the architecture of TARGET a variant of Microchip's AVR architecture?"
  (or (string=? target "avr") (string-prefix? "avr-" target)))

(define* (target-loongarch64? #:optional (target (or (%current-target-system)
                                                     (%current-system))))
  (string-prefix? "loongarch64-" target))

(define* (target-ppc32? #:optional (target (or (%current-target-system)
                                               (%current-system))))
  (string-prefix? "powerpc-" target))

(define* (target-ppc64le? #:optional (target (or (%current-target-system)
                                               (%current-system))))
  (string-prefix? "powerpc64le-" target))

(define* (target-powerpc? #:optional (target (or (%current-target-system)
                                                 (%current-system))))
  (string-prefix? "powerpc" target))

(define* (target-riscv64? #:optional (target (or (%current-target-system)
                                                 (%current-system))))
  "Is the architecture of TARGET a 'riscv64' machine?"
  (string-prefix? "riscv64" target))

(define* (target-mips64el? #:optional (target (or (%current-target-system)
                                                  (%current-system))))
  (string-prefix? "mips64el-" target))

(define* (target-64bit? #:optional (system (or (%current-target-system)
                                               (%current-system))))
  (any (cut string-prefix? <> system) '("x86_64" "aarch64" "mips64"
                                        "powerpc64" "riscv64" "loongarch64")))

(define* (target-little-endian? #:optional (target (or (%current-target-system)
                                                       (%current-system))))
  "Is the architecture of TARGET little-endian?"
  ;; At least in Guix.  Aarch64 and 32-bit arm have a big-endian mode as well.
  (not (target-ppc32? target)))

(define* (ar-for-target #:optional (target (%current-target-system)))
  (if target
      (string-append target "-ar")
      "ar"))

(define* (as-for-target #:optional (target (%current-target-system)))
  (if target
      (string-append target "-as")
      "as"))

(define* (cc-for-target #:optional (target (%current-target-system)))
  (if target
      (string-append target "-gcc")
      "gcc"))

(define* (cxx-for-target #:optional (target (%current-target-system)))
  (if target
      (string-append target "-g++")
      "g++"))

(define* (ld-for-target #:optional (target (%current-target-system)))
  (if target
      (string-append target "-ld")
      "ld"))

(define* (pkg-config-for-target #:optional (target (%current-target-system)))
  (if target
      (string-append target "-pkg-config")
      "pkg-config"))

(define* (strip-for-target #:optional (target (%current-target-system)))
  (if target
      (string-append target "-strip")
      "strip"))

(define version-compare
  (let ((strverscmp
         (let ((sym (or (dynamic-func "strverscmp" (dynamic-link))
                        (error "could not find `strverscmp' (from GNU libc)"))))
           (pointer->procedure int sym (list '* '*)))))
    (lambda (a b)
      "Return '> when A denotes a newer version than B,
'< when A denotes a older version than B,
or '= when they denote equal versions."
      (let ((result (strverscmp (string->pointer a) (string->pointer b))))
        (cond ((positive? result) '>)
              ((negative? result) '<)
              (else '=))))))

(define (version-prefix version-string num-parts)
  "Truncate version-string to the first num-parts components of the version.
For example, (version-prefix \"2.1.47.4.23\" 3) returns \"2.1.47\""
  (string-join (take (string-split version-string #\.) num-parts) "."))

(define (version-major+minor+point version-string)
  "Return \"major>.<minor>.<point>\", where major, minor and point are the
major, minor and point version numbers from the version-string.  For example,
(version-major+minor+point \"6.4.5.2\") returns \"6.4.5\" or
(version-major+minor+point \"1.19.2-2581-324ca14c3003\") returns \"1.19.2\"."
  (let* ((3-dot (version-prefix version-string 3))
         (index (string-index 3-dot #\-)))
    (or (false-if-exception (substring 3-dot 0 index))
        3-dot)))

(define (version-major+minor version-string)
  "Return \"<major>.<minor>\", where major and minor are the major and
minor version numbers from version-string."
  (version-prefix version-string 2))

(define (version-major version-string)
  "Return the major version number as string from the version-string."
  (version-prefix version-string 1))

(define (version-unique-prefix version versions)
  "Return the shortest version prefix to unambiguously identify VERSION among
VERSIONS.  For example:

  (version-unique-prefix \"2.0\" '(\"3.0\" \"2.0\"))
  => \"2\"

  (version-unique-prefix \"2.2\" '(\"3.0.5\" \"2.0.9\" \"2.2.7\"))
  => \"2.2\"

  (version-unique-prefix \"27.1\" '(\"27.1\"))
  => \"\"
"
  (define not-dot
    (char-set-complement (char-set #\.)))

  (define other-versions
    (delete version versions))

  (let loop ((prefix     '())
             (components (string-tokenize version not-dot)))
    (define prefix-str
      (string-join prefix "."))

    (if (any (cut string-prefix? prefix-str <>) other-versions)
        (match components
          ((head . tail)
           (loop `(,@prefix ,head) tail))
          (()
           version))
        prefix-str)))

(define (version>? a b)
  "Return #t when A denotes a version strictly newer than B."
  (eq? '> (version-compare a b)))

(define (version>=? a b)
  "Return #t when A denotes a version newer or equal to B."
  (case (version-compare a b)
    ((> =) #t)
    (else #f)))

(define (guile-version>? str)
  "Return #t if the running Guile version is greater than STR."
  ;; Note: Using (version>? (version) "2.0.5") or similar doesn't work,
  ;; because the result of (version) can have a prefix, like "2.0.5-deb1".
  (version>? (string-append (major-version) "."
                            (minor-version) "."
                            (micro-version))
             str))

(define version-prefix?
  (let ((not-dot (char-set-complement (char-set #\.))))
    (lambda (v1 v2)
      "Return true if V1 is a version prefix of V2:

  (version-prefix? \"4.1\" \"4.16.2\") => #f
  (version-prefix? \"4.1\" \"4.1.2\") => #t
"
      (define (list-prefix? lst1 lst2)
        (match lst1
          (() #t)
          ((head1 tail1 ...)
           (match lst2
             (() #f)
             ((head2 tail2 ...)
              (and (equal? head1 head2)
                   (list-prefix? tail1 tail2)))))))

      (list-prefix? (string-tokenize v1 not-dot)
                    (string-tokenize v2 not-dot)))))


;;;
;;; Files.
;;;

(define (file-extension file)
  "Return the extension of FILE or #f if there is none."
  (let ((dot (string-rindex file #\.)))
    (and dot (substring file (+ 1 dot) (string-length file)))))

(define (file-sans-extension file)
  "Return the substring of FILE without its extension, if any."
  (let ((dot (string-rindex file #\.)))
    (if dot
        (substring file 0 dot)
        file)))

(define (tarball-sans-extension tarball)
  "Return TARBALL without its .tar.* or .zip extension."
  (let ((end (or (string-contains tarball ".tar")
                 (string-contains tarball ".tgz")
                 (string-contains tarball ".zip"))))
    (substring tarball 0 end)))

(define (compressed-file? file)
  "Return true if FILE denotes a compressed file."
  (->bool (member (file-extension file)
                  '("gz" "bz2" "xz" "lz" "lzma" "tgz" "tbz2" "tzst"
                    "zip" "zst"))))

(define* (string-replace-substring str substr replacement
                                   #:optional
                                   (start 0)
                                   (end (string-length str)))
  "Replace all occurrences of SUBSTR in the START--END range of STR by
REPLACEMENT."
  (match (string-length substr)
    (0
     (error "string-replace-substring: empty substring"))
    (substr-length
     (let loop ((start  start)
                (pieces (list (substring str 0 start))))
       (match (string-contains str substr start end)
         (#f
          (string-concatenate-reverse
           (cons (substring str start) pieces)))
         (index
          (loop (+ index substr-length)
                (cons* replacement
                       (substring str start index)
                       pieces))))))))

(define (call-with-temporary-directory proc)
  "Call PROC with a name of a temporary directory; close the directory and
delete it when leaving the dynamic extent of this call."
  (let* ((directory (or (getenv "TMPDIR") "/tmp"))
         (template  (string-append directory "/guix-directory.XXXXXX"))
         (tmp-dir   (mkdtemp! template)))
    (dynamic-wind
      (const #t)
      (lambda ()
        (proc tmp-dir))
      (lambda ()
        (false-if-exception (delete-file-recursively tmp-dir))))))

(define* (with-atomic-file-output file proc #:key (sync? #t))
  "Call PROC with an output port for the file that is going to replace FILE.
Upon success, FILE is atomically replaced by what has been written to the
output port, and PROC's result is returned.

When SYNC? is true, call 'fdatasync' on the temporary file before renaming it
to FILE; set it to #false for caches and temporary files to improve
performance."
  (let* ((template (string-append file ".XXXXXX"))
         (out      (mkstemp! template)))
    (with-throw-handler #t
      (lambda ()
        (let ((result (proc out)))
          (when sync?
            (force-output out)
            (fdatasync out))
          (close-port out)
          (rename-file template file)
          result))
      (lambda (key . args)
        (false-if-exception (delete-file template))
        (close-port out)))))

(define* (xdg-directory variable suffix #:key (ensure? #t))
  "Return the name of the XDG directory that matches VARIABLE and SUFFIX,
after making sure that it exists if ENSURE? is true.  VARIABLE is an
environment variable name like \"XDG_CONFIG_HOME\"; SUFFIX is a suffix like
\"/.config\".  Honor the XDG specs,
<http://standards.freedesktop.org/basedir-spec/basedir-spec-latest.html>."
  (let ((dir (and=> (or (getenv variable)
                        (and=> (or (getenv "HOME")
                                   (passwd:dir (getpwuid (getuid))))
                               (cut string-append <> suffix)))
                    (cut string-append <> "/guix"))))
    (when ensure?
      (mkdir-p dir))
    dir))

(define config-directory
  (cut xdg-directory "XDG_CONFIG_HOME" "/.config" <...>))

(define cache-directory
  (cut xdg-directory "XDG_CACHE_HOME" "/.cache" <...>))

(define (readlink* file)
  "Call 'readlink' until the result is not a symlink."
  (define %max-symlink-depth 50)

  (let loop ((file  file)
             (depth 0))
    (define (absolute target)
      (if (absolute-file-name? target)
          target
          (string-append (dirname file) "/" target)))

    (if (>= depth %max-symlink-depth)
        file
        (call-with-values
            (lambda ()
              (catch 'system-error
                (lambda ()
                  (values #t (readlink file)))
                (lambda args
                  (let ((errno (system-error-errno args)))
                    (if (or (= errno EINVAL))
                        (values #f file)
                        (apply throw args))))))
          (lambda (success? target)
            (if success?
                (loop (absolute target) (+ depth 1))
                file))))))

(define (canonical-newline-port port)
  "Return an input port that wraps PORT such that all newlines consist
  of a single linefeed."
  (define (get-position)
    (if (port-has-port-position? port) (port-position port) #f))
  (define (set-position! position)
    (if (port-has-set-port-position!? port)
        (set-port-position! position port)
        #f))
  (define (close) (close-port port))
  (define (read! bv start n)
    (let loop ((count 0)
               (byte (get-u8 port)))
      (cond ((eof-object? byte) count)
            ;; XXX: consume all CRs even if not followed by LF.
            ((eqv? byte (char->integer #\return)) (loop count (get-u8 port)))
            ((= count (- n 1))
             (bytevector-u8-set! bv (+ start count) byte)
             n)
            (else
             (bytevector-u8-set! bv (+ start count) byte)
             (loop (+ count 1) (get-u8 port))))))
  (make-custom-binary-input-port "canonical-newline-port"
                                 read!
                                 get-position
                                 set-position!
                                 close))

;;;
;;; Source location.
;;;

(define (%guix-source-root-directory)
  "Return the source root directory of the Guix found in %load-path."
  (dirname (absolute-dirname "guix/packages.scm")))

(define absolute-dirname
  ;; Memoize to avoid repeated 'stat' storms from 'search-path'.
  (mlambda (file)
    "Return the absolute name of the directory containing FILE, or #f upon
failure."
    (match (search-path %load-path file)
      (#f #f)
      ((? string? file)
       (dirname (canonicalize-path file))))))

(define-syntax current-source-directory
  (lambda (s)
    "Return the absolute name of the current directory, or #f if it could not
be determined."
    (syntax-case s ()
      ((_)
       (match (assq 'filename (or (syntax-source s) '()))
         (('filename . (? string? file-name))
          ;; If %FILE-PORT-NAME-CANONICALIZATION is 'relative, then FILE-NAME
          ;; can be relative.  In that case, we try to find out at run time
          ;; the absolute file name by looking at %LOAD-PATH; doing this at
          ;; run time rather than expansion time is necessary to allow files
          ;; to be moved on the file system.
          (if (string-prefix? "/" file-name)
              (dirname (canonicalize-path file-name))
              #`(absolute-dirname #,file-name)))
         ((or ('filename . #f) #f)
          ;; raising an error would upset Geiser users
          #f))))))


;;;
;;; String comparison.
;;;

(define (string-distance s1 s2)
  "Compute the Levenshtein distance between two strings."
  ;; Naive implementation
  (define loop
    (mlambda (as bt)
      (match as
        (() (length bt))
        ((a s ...)
         (match bt
           (() (length as))
           ((b t ...)
            (if (char=? a b)
                (loop s t)
                (1+ (min
                     (loop as t)
                     (loop s bt)
                     (loop s t))))))))))

  (let ((c1 (string->list s1))
        (c2 (string->list s2)))
    (loop c1 c2)))

(define* (string-closest trial tests #:key (threshold 3))
  "Return the string from TESTS that is the closest from the TRIAL,
according to 'string-distance'.  If the TESTS are too far from TRIAL,
according to THRESHOLD, then #f is returned."
  (identity                              ;discard second return value
    (fold2 (lambda (test closest minimal)
             (let ((dist (string-distance trial test)))
               (if (and  (< dist minimal) (< dist threshold))
                   (values test dist)
                   (values closest minimal))))
           #f +inf.0
           tests)))


;;;
;;; Prettified output.
;;;

(define* (pretty-print-table rows #:key (max-column-width 20) (left-pad 0))
  "Print ROWS in neat columns.  All rows should be lists of strings and each
row should have the same length.  The columns are separated by a tab
character, and aligned using spaces.  The maximum width of each column is
bound by MAX-COLUMN-WIDTH.  Each row is prefixed with LEFT-PAD spaces."
  (let* ((number-of-columns-to-pad (if (null? rows)
                                       0
                                       (1- (length (first rows)))))
         ;; Ignore the last column as it is left aligned and doesn't need
         ;; padding; this prevents printing extraneous trailing spaces.
         (column-widths (fold (lambda (row maximums)
                                (map max (map string-length row) maximums))
                              ;; Initial max width is 0 for each column.
                              (make-list number-of-columns-to-pad 0)
                              (map (cut drop-right <> 1) rows)))
         (column-formats (map (cut format #f "~~~da" <>)
                              (map (cut min <> max-column-width)
                                   column-widths)))
         (fmt (string-append (string-join column-formats "\t") "\t~a")))
    (for-each (cut format #t "~v_~?~%" left-pad fmt <>) rows)))

;;; Local Variables:
;;; eval: (put 'call-with-progress-reporter 'scheme-indent-function 1)
;;; End:
