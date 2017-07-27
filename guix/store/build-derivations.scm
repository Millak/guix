;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Caleb Ristvedt <caleb.ristvedt@cune.org>
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

;;; For building derivations.

(define-module (guix store build-derivations)
  #:use-module (guix store deduplication)
  #:use-module (guix derivations)
  #:use-module (guix store)
  #:use-module (guix store database)
  #:use-module (guix config)
  #:use-module (guix build syscalls)
  #:use-module (ice-9 vlist)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-11)
  #:use-module (guix hash)
  #:use-module (guix serialization)
  #:use-module ((guix build utils) #:select (delete-file-recursively
                                             mkdir-p
                                             copy-recursively))
  #:use-module (gnu system file-systems)
  #:use-module (gnu build linux-container)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 match)
  #:use-module (rnrs io ports)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 q)
  #:use-module (srfi srfi-43)
  #:use-module (rnrs bytevectors)
  #:export (build-derivation))


(define-record-type <build-environment>
  (make-build-environment drv build-dir-inside build-dir env-vars input-paths
                          filesystems user group)
  build-environment?
  (drv        build-environment-derivation) ; <derivation> this is for.
  (build-dir-inside build-directory-inside) ; path of chroot directory.
  (build-dir  build-directory)              ; build dir (outside chroot).
  (env-vars   build-environment-variables)  ; alist of environment variables.
  (input-paths build-input-paths)           ; list of paths or pairs of paths.
  (filesystems build-filesystems)           ; list of <file-system> objects.
  (user        build-environment-user)      ; the user id to build with.
  (group       build-environment-group))    ; the group id to build with.      

;;; The derivation building process:
;;; 1. Build inputs if necessary.
;;; 2. Make a build directory under TMPDIR or /tmp
;;; 3. Gather all the inputs, the inputs of the inputs, the inputs of the
;;; inputs of the inputs, and so on. Copy them to /gnu/store under the build
;;; directory.
;;; 4. Gather all the sources and plop them in the build directory
;;; 5. Make an output directory for the build under /gnu/store in the build
;;; directory.
;;; 6. Set all the environment variables listed in the derivation, some of
;;; which we have to honor ourselves, like "preferLocalBuild",
;;; "allowSubstitutes", "allowedReferences", "disallowedReferences", and
;;; "impureEnvVars".
;;; 7. Run the builder in a chroot where the build directory is the root.

;; Add this to (guix config) later
(define %temp-directory "/tmp")

;; if a derivation builder name is in here, it is a builtin. For normal
;; behavior, make sure everything starts with "builtin:". Also, the procedures
;; stored in here should take a single argument, the derivation.

(define (get-output-specs drv possible-references)
  (map (match-lambda
         ((outid . ($ <derivation-output> output-path))
          (let-values (((references hash nar-size)
                        (scan-for-references output-path
                                             possible-references)))
            (list outid output-path references hash nar-size))))
       (derivation-outputs drv)))

(define (builtin-download drv)
  ((@@ (guix scripts perform-download) perform-download) drv)
  (get-output-specs drv (all-transitive-inputs drv)))

(define builtins
  (let ((builtins-table (make-hash-table 10)))
    (hash-set! builtins-table
               "builtin:download"
               builtin-download)
    builtins-table))

;; We might want to add to this sometime.
(define %default-chroot-dirs
  '())

(define* (build-directory-name drv #:optional
                               (attempt 0)
                               (temp-directory %temp-directory))
  (string-append temp-directory
                 "/guix-build-"
                 (store-path-package-name (derivation-file-name drv))
                 "-"
                 (number->string attempt)))

(define* (make-build-directory drv #:optional (temp-directory %temp-directory))
  (let try-again ((attempt-number 0))
    (catch 'system-error
      (lambda ()
        (let ((build-dir (build-directory-name drv
                                               attempt-number
                                               temp-directory)))
          (mkdir build-dir #o0700)
          build-dir))
      (lambda args
        (if (= (system-error-errno args) EEXIST)
            (try-again (+ attempt-number 1))
            (throw args))))))


(define (build-environment-vars drv in-chroot-build-dir)
  "Returns an alist of environment variable / value pairs for every
environment variable that should be set during the build execution."
  (let ((leaked-vars (and
                      (fixed-output-derivation? drv)
                      (let ((leak-string
                             (assoc-ref (derivation-builder-environment-vars drv)
                                        "impureEnvVars")))
                        (and leak-string
                             (parse-delimited leak-string))))))
    (append `(("PATH"             .  "/path-not-set")
              ("HOME"             .  "/homeless-shelter")
              ("NIX_STORE"        .  ,%store-directory)
              ;; XXX: make this configurable
              ("NIX_BUILD_CORES"  .  "1")
              ("NIX_BUILD_TOP"    .  ,in-chroot-build-dir)
              ;; why yes that is something like /tmp/guix-build-<drv>-0, yes
              ;; indeed it does not make much sense to make that the TMPDIR
              ;; instead of /tmp, and no I do not know why the C++ code does it
              ;; that way.
              ("TMPDIR"           .  ,in-chroot-build-dir)
              ("TEMPDIR"          .  ,in-chroot-build-dir)
              ("TMP"              .  ,in-chroot-build-dir)
              ("TEMP"             .  ,in-chroot-build-dir)
              ("PWD"              .  ,in-chroot-build-dir))
            (if (fixed-output-derivation? drv)
                '(("NIX_OUTPUT_CHECKED" . "1"))
                '())
            (if leaked-vars
                (map (lambda (leaked-var)
                       (cons leaked-var (getenv leaked-var)))
                     leaked-vars)
                '())
            (derivation-builder-environment-vars drv))))

(define (default-files drv)
  "Returns a list of the files to be bind-mounted that aren't store items or
already added by call-with-container."
  `(,@(if (file-exists? "/dev/kvm")
          '("/dev/kvm")
          '())
    ,@(if (fixed-output-derivation? drv)
          '("/etc/resolv.conf"
            "/etc/nsswitch.conf"
            "/etc/services"
            "/etc/hosts")
          '())))

;; yes, there is most likely already something that does this.
(define (format-file file-name . args)
  (call-with-output-file file-name
    (lambda (port)
      (apply simple-format port args))))

(define* (mkdir-p* dir #:optional permissions)
  (mkdir-p dir)
  (when permissions
    (chmod dir permissions)))

(define (add-core-files environment)
  "Creates core files that will not vary when the derivation is constant. That
is, whether these files are present or not is influenced solely by the
derivation itself."
  (mkdir-p* %store-directory #o1775)
  (mkdir-p* "/tmp" #o1777)
  (mkdir-p* "/etc")

  (format-file "/etc/passwd"
               (string-append "nixbld:x:~a:~a:Nix build user:/:/noshell~%"
                              "nobody:x:65534:65534:Nobody:/:/noshell~%")
               (build-environment-user environment)
               (build-environment-group environment))
  (format-file "/etc/group"
               "nixbld:!:~a:~%"
               (build-environment-group environment))
  (unless (fixed-output-derivation?
           (build-environment-derivation environment))
    (format-file "/etc/hosts" "127.0.0.1 localhost~%")))

(define (path-already-assigned? path paths)
  "Determines whether something is already going to be bind-mounted to PATH
based on what is in PATHS, which should be a list of paths or path pairs."
  (find (match-lambda
          ((source . target)
           (string= target path))
          (target
           (string= target path)))
        paths))

(define* (prepare-build-environment drv #:key
                                    build-chroot-dirs 
                                    (extra-chroot-dirs '())
                                    (build-user (getuid))
                                    (build-group (getgid)))
  "Creates a <build-environment> for the derivation DRV. BUILD-CHROOT-DIRS
will override the default chroot directories, EXTRA-CHROOT-DIRS will
not. Those two arguments should be #f or lists of either file names or pairs
of file names of the form (outside . inside). Returns the <build-environment>
and a list of all the files to be added from the store (useful for scanning
for references to them)."
  (let* ((build-dir (make-build-directory drv))
         (build-dir-inside (build-directory-name drv 0 "/tmp"))
         (env-vars (build-environment-vars drv build-dir-inside))
         (inputs-from-store (all-transitive-inputs drv))
         (all-inputs `(,@(or build-chroot-dirs
                             %default-chroot-dirs)
                       ,@extra-chroot-dirs
                       ,@(default-files drv)
                       ,(cons build-dir
                              build-dir-inside)
                       ,@inputs-from-store
                       ,@(derivation-sources drv))))
    ;; 4. Honor "environment variables" passed through the derivation.
    ;;    these include "impureEnvVars", "exportReferencesGraph",
    ;;    "build-chroot-dirs", "build-extra-chroot-dirs", "preferLocalBuild"
    (values
     (make-build-environment drv build-dir-inside build-dir env-vars
                             all-inputs
                             (special-filesystems all-inputs)
                             build-user
                             build-group)
     inputs-from-store)))

(define (all-transitive-inputs drv)
  "Produces a list of all inputs and all of their references."
  (let ((input-paths (inputs-closure drv)))
    (vhash-fold (lambda (key val prev)
                  (cons key prev))
                input-paths
                (fold (lambda (input list-so-far)
                        (file-closure input #:list-so-far list-so-far))
                      vlist-null
                      ;; include the derivation's references as well
                      (cons (derivation-file-name drv)
                            input-paths)))))

;; Sigh... I just HAD to go and ask "what if there are spaces in the mountinfo
;; entries"... I couldn't find the behavior documented anywhere, but
;; experimentally it appears to be octal-escaped.
(define (octal-escaped str)
  "Converts octal escapes of the form \\abc to the corresponding character
code points."
  (define (octal-triplet->char octet1 octet2 octet3) 
    ;; I'm using "octet" here like I would normally use "digit".
    (integer->char (string->number (string octet1 octet2 octet3)
                                   8)))

  (let next-char ((result-list '())
                  (to-convert (string->list str)))
    (match to-convert
      ((#\\ octet1 octet2 octet3 . others)
       (next-char (cons (octal-triplet->char octet1 octet2 octet3)
                        result-list)
                  others))
      ((char . others)
       (next-char (cons char result-list)
                  others))
      (()
       (list->string (reverse! result-list))))))

(define (special-filesystems input-paths)
  "Returns whatever new filesystems need to be created in the container, which
depends on whether they're already set to be bind-mounted. INPUT-PATHS must be
a list of paths or pairs of paths."
  ;; procfs is already taken care of by call-with-container
  `(,@(if (file-exists? "/dev/shm")
          (list (file-system
                  (device "none")
                  (mount-point "/dev/shm")
                  (type "tmpfs")
                  (check? #f)))
          '())
    
    ;; Indicates CONFIG_DEVPTS_MULTIPLE_INSTANCES=y in the kernel.
    ,@(if  (and (file-exists? "/dev/pts/ptmx")
                (not (file-exists? "/dev/ptmx"))
                (not (path-already-assigned? "/dev/pts"
                                             input-paths)))
           (list (file-system
                   (device "none")
                   (mount-point "/dev/pts")
                   (type "devpts")
                   (options "newinstance,mode=0620")
                   (check? #f)))
           '())))

(define (initialize-loopback)
  ;; XXX: Implement this. I couldn't find anything in the manual about ioctl,
  ;; which we need to use, soo...
  ;; (let ((sock (socket PF_INET SOCK_DGRAM IPPROTO_IP)))
  ;;   )
  #f)

(define (enact-build-environment build-environment)
  "Makes the <build-environment> BUILD-ENVIRONMENT current by setting the
environment variables and bind-mounting the listed files. Importantly, this
assumes that it is in a separate namespace at this point."
  ;; warning: the order in which a lot of this happens is significant and
  ;; partially based on guesswork / copying what the c++ does.

  (add-core-files build-environment)
  ;; local communication within the build environment should still be
  ;; possible.
  (initialize-loopback)
  ;; This couldn't really be described by a <file-system> object, so we have
  ;; to do this extra bit ourselves. 
  (when (find (lambda (fs)
                (string=? (file-system-type fs) "devpts"))
              (build-filesystems build-environment))
    (symlink "/dev/pts/ptmx" "/dev/ptmx")
    (chmod "/dev/pts/ptmx" #o0666))
  (environ (map (match-lambda
                  ((key . val)
                   (string-append key "=" val)))
                (build-environment-variables build-environment))))

;; The C++ stuff does this, and in pursuit of a bug I will mindlessly mimic
;; anything.
(define (close-most-files)
  (port-for-each (lambda (port)
                   (when (port-filename port)
                     (let ((port-fd (port->fdes port)))
                       (unless (or
                                (= port-fd (port->fdes (current-input-port)))
                                (= port-fd (port->fdes (current-output-port)))
                                (= port-fd (port->fdes (current-error-port))))
                         (close port-fd)))))))

(define (inputs->mounts inputs)
  (map (match-lambda
         ((source . dest)
          (file-system
            (device source)
            (mount-point dest)
            (type "none")
            (flags '(bind-mount))
            (check? #f)))
         (source
          (file-system
            (device source)
            (mount-point source)
            (type "none")
            (flags '(bind-mount))
            (check? #f))))
       inputs))

(define (run-builder environment)
  "Runs the builder in the environment ENVIRONMENT."
  (let ((drv (build-environment-derivation environment)))
    (call-with-container
        (append (inputs->mounts (build-input-paths environment))
                (build-filesystems environment))
      (lambda ()
        (enact-build-environment environment)
        ;(close-most-files)
        ;; DROP PRIVILEGES HERE
        (chdir (build-directory-inside environment))
        
        (format #t "command line: ~a~%"
                (cons (derivation-builder drv)
                      (derivation-builder-arguments drv)))
        (if (zero? (status:exit-val
                    (apply system*
                           (derivation-builder drv)
                                        ;(basename (derivation-builder drv))
                           (derivation-builder-arguments drv))))
            0
            (throw 'build-failed-but-lets-debug drv)))
      #:namespaces `(mnt pid ipc uts ,@(if (fixed-output-derivation? drv)
                                           '(net)
                                           '()))
      #:host-uids (1+ (build-environment-user environment))
      #:use-output (lambda (root)
                     (for-each (match-lambda
                                 ((outid . ($ <derivation-output> output-path))
                                  (copy-recursively (string-append root
                                                                   output-path)
                                                    output-path)))
                               (derivation-outputs drv))))))

;; I want to be able to test if a derivation's outputs exist without reading
;; it in. The database makes this possible. But we can't figure out WHICH
;; outputs it even has without reading it in. For most of the derivations, we
;; don't need to know which outputs it has, as long as we know the outputs we
;; want. Okay, okay, new plan: build-derivation takes a <derivation>, but
;; ensure-input-outputs-exist takes <derivation-input>
;; objects. build-derivation is only called when we know it needs to be built

(define (inputs-closure drv)
  "Given a <derivation> DRV, finds all store paths needed to build it."
  (fold (lambda (input prev)
          (fold (lambda (output outputs-list)
                  (cons output outputs-list))
                prev
                (derivation-input-output-paths input)))
        '()
        (derivation-prerequisites drv)))

(define (attempt-substitute drv)
  #f)

(define (maybe-use-builtin drv)
  "Uses a builtin builder to build DRV if it exists. Returns #f if there is no
builtin builder for DRV or it failed."
  (let ((builder (hash-ref builtins
                           (derivation-builder drv))))
    (if builder
        (builder drv)
        #f)))



(define-record-type <trie-node>
  (make-trie-node table string-exists?)
  trie-node?
  ;; TODO implement skip values. Probably not as big a speed gain as you think
  ;; it is, since this is I/O-bound.
  ;; (skip-value node-skip-value set-skip-value!)
  (table node-table set-node-table!)
  ;; Technically speaking, it's possible for both CAT and CATTLE to be in a
  ;; trie at once. Of course, for our purposes, this is 
  (string-exists? node-string-exists? set-string-exists?!))

(define* (add-to-trie trie string #:optional (new-tables-size 2))
  "Adds STR to TRIE."
  (let ((str (string->utf8 string)))
    (let next-node ((position 0)
                    (current-node trie))
      (if (= position (bytevector-length str))
          ;; this is it. This is where we need to register that this string is
          ;; present.
          (set-string-exists?! current-node #t)
          (let* ((current-table (node-table current-node))
                 (node (hash-ref current-table
                                 (bytevector-u8-ref str position))))
            (if node
                (next-node (1+ position)
                           node)
                (let ((new-node (make-trie-node (make-hash-table new-tables-size)
                                                #f)))
                  (hash-set! current-table
                             (bytevector-u8-ref str position)
                             new-node)
                  (next-node (1+ position)
                             new-node))))))))

(define (make-search-trie strings)
  ;; TODO: make the first few trie levels non-sparse tables to avoid hashing
  ;; overhead. 
  (let ((root (make-trie-node (make-hash-table) #f)))
    (for-each (cut add-to-trie root <>)
              strings)
    root))


(define (remove-from-trie! trie sequence)
  "Removes SEQUENCE from TRIE. This means that any nodes that are only in the
path of SEQUENCE are removed. It's an error to use this with a sequence not
already in TRIE."
  ;; Hm. Looks like we'll have to recurse all the way down, find where it
  ;; ends, then stop at the first thing on the way back up that has anything
  ;; with the same prefix. Or I could do this the right way with an explicit
  ;; stack. Hm...
  
  (define (node-stack)
    (let next ((nodes '())
               (i 0)
               (current-node trie))
      (if (= (bytevector-length sequence) i)
          (begin
            ;; it's possible that even though this is the last node of this
            ;; sequence it can't be deleted. So mark it as not denoting a
            ;; string.
            (set-string-exists?! current-node #f)
            (cons current-node nodes))
          (let ((next-node (hash-ref (node-table current-node)
                                     (bytevector-u8-ref sequence i))))
            (next (cons current-node nodes)
                  (1+ i)
                  next-node)))))

  (let maybe-delete ((visited-nodes (node-stack))
                     (i (1- (bytevector-length sequence))))
    (match visited-nodes
      ((current parent others ...)
       (when (<= (hash-count (const #t)
                             (node-table current))
                 1)

         (hash-remove! (node-table parent)
                       (bytevector-u8-ref sequence i))
         (maybe-delete (cdr visited-nodes)
                       (1- i))))
      ((current)
       #f))))

(define (scanning-wrapper-port output-port strings)
  "Creates a wrapper port which passes through bytes to OUTPUT-PORT and
returns it as well as a procedure which, when called, returns a list of all
references out of the possibilities enumerated in STRINGS that were
detected."
  ;; Not sure if I should be using custom ports or soft ports...
  (let* ((lookback-size (apply max (map string-length strings)))
         (smallest-length (apply min (map string-length strings)))
         (lookback-buffer (make-bytevector lookback-size))
         (search-trie (make-search-trie strings))
         (buffer-pos 0)
         (references '()))
    
    (values
     (make-custom-binary-output-port
      "scanning-wrapper"
      ;; write
      (lambda (bytes offset count)
        (define (in-lookback? n)
          (< n buffer-pos))
        ;; the "virtual" stuff provides a convenient interface that makes it
        ;; look like we magically remember the end of the previous buffer.
        (define (virtual-ref n)
          (if (in-lookback? n)
              (bytevector-u8-ref lookback-buffer n)
              (bytevector-u8-ref bytes (- (+ offset n)
                                          buffer-pos))))
        

        (let ((total-length (+ buffer-pos count)))
          
          (define (virtual-copy! start end target)
            (let* ((copy-size (- end start))
                   (new-bytevector (make-bytevector copy-size)))
              (let copy-next ((i 0))
                (unless (= i copy-size)
                  (bytevector-u8-set! new-bytevector
                                      i
                                      (virtual-ref (+ start i)))
                  (copy-next (1+ i))))
              new-bytevector))

          ;; the gritty reality of that magic
          (define (remember-end)
            (let* ((copy-amount (min total-length
                                     lookback-size))
                   (start (- total-length copy-amount))
                   (end total-length))
              (virtual-copy! start end lookback-buffer)
              (set! buffer-pos copy-amount)))
          
          (define (attempt-match n trie)
            (let test-position ((i n)
                                (current-node trie))
              (if (node-string-exists? current-node)
                  ;; MATCH
                  (begin
                    (format #t "Start:~a End: ~a~%" n i)
                    (virtual-copy! n i (make-bytevector (- i n))))
                  (if (>= i total-length)
                      #f
                      (let ((next-node (hash-ref (node-table current-node)
                                                 (virtual-ref i))))
                        (if next-node
                            (test-position (1+ i)
                                           next-node)
                            #f))))))
          
          (define (scan)
            (let next-char ((i 0))
              (when (< i (- total-length smallest-length))
                (let ((match-result (attempt-match i search-trie)))
                  (if match-result
                      (begin
                        (set! references
                          (cons (utf8->string match-result)
                                references))
                        ;; We're not interested in multiple references, it'd
                        ;; just slow us down.
                        (remove-from-trie! search-trie match-result)
                        (next-char (+ i (bytevector-length match-result))))
                      (next-char (1+ i)))))))
          (scan)
          (remember-end)
          (put-bytevector output-port bytes offset count)
          count))
      #f ;; get-position
      #f ;; set-position
      (lambda ()
        (close-port output-port)))
     (lambda ()
       references))))


;; There are two main approaches we can use here: we can look for the entire
;; store path of the form "/gnu/store/hashpart-name", which will yield no
;; false positives and likely be faster due to being more quickly able to rule
;; out sequences, and we can look for just hashpart, which will be faster to
;; lookup and may both increase false positives and decrease false negatives
;; as stuff that gets split up will likely still have the hash part all
;; together, but adds a chance that 32 random base-32 characters could cause a
;; false positive, but the chances of that are extremely slim, and an
;; adversary couldn't really use that.
(define (scan-for-references file possibilities)
  "Scans for literal references in FILE as long as they happen to be in
POSSIBILITIES. Returns the list of references found, the sha256 hash of the
nar, and the length of the nar."
  (let*-values (((hash-port get-hash) (open-sha256-port))
                ((scanning-port get-references)
                 (scanning-wrapper-port hash-port possibilities))
                ((counting-port) (counting-wrapper-port scanning-port)))
    (write-file file counting-port)
    (force-output counting-port)
    (let ((size (port-position counting-port)))
      (close-port counting-port)
      (values (get-references)
              (get-hash)
              size))))

;; every method of getting a derivation's outputs in the store needs to
;; provide 3 pieces of metadata: the size of the nar, the references of each
;; output, and the hash of each output. We happen to have ways of getting all
;; of those as long as we know which references to be looking for.
;;~/Programming/guix/test-tmp/store/3zazs4zzhv0iw4xw0bi0im0wi55cl4gv-hello-2.10.drv
(define (do-derivation-build drv)
  (format #t "Starting build of derivation ~a~%~%" drv)
  ;; inputs should all exist as of now
  (let-values (((build-env store-inputs)
                (prepare-build-environment drv #:extra-chroot-dirs '())))
    (if (zero? (run-builder build-env))
        (get-output-specs drv store-inputs)
        #f)))

(define (%build-derivation drv) 
  "Given a <derivation> DRV, builds/substitutes the derivation unconditionally
even if its outputs already exist."
  ;; Inputs need to exist regardless of how we're getting the outputs of this
  ;; derivation.
  (ensure-input-outputs-exist (derivation-inputs drv))
  (let ((output-specs
         (or (attempt-substitute drv)
             (maybe-use-builtin drv)
             (do-derivation-build drv))))
    (if output-specs
        (for-each (match-lambda
                    ((outid output-path references nar-size hash)
                     (register-derivation-output %store-database
                                                 (derivation-file-name drv)
                                                 outid
                                                 output-path
                                                 references
                                                 nar-size
                                                 hash))
                    (assimilate-path output-path))
                  output-specs)
        (throw 'derivation-build-failed drv))))

(define (ensure-input-outputs-exist inputs)
  (for-each
   (lambda (input)
     (let ((input-drv-path (derivation-input-path input)))
       (unless (outputs-exist? input-drv-path
                               (derivation-input-sub-derivations input))
         (%build-derivation (read-derivation-from-file input-drv-path)))))
   inputs))

(define* (build-derivation drv #:optional (outputs (derivation-output-names drv)))
  "Given a <derivation> DRV with desired outputs OUTPUTS, builds DRV if the
outputs don't already exist."
  (unless (outputs-exist? (derivation-file-name drv)
                          outputs)
    (%build-derivation drv)))



