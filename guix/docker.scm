;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017, 2018, 2019, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2018 Chris Marusich <cmmarusich@gmail.com>
;;; Copyright © 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2023 Oleg Pykhalov <go.wigust@gmail.com>
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

(define-module (guix docker)
  #:use-module (gcrypt hash)
  #:use-module (guix base16)
  #:use-module (guix build pack)
  #:use-module ((guix build utils)
                #:select (mkdir-p
                          delete-file-recursively
                          with-directory-excursion
                          invoke))
  #:use-module (gnu build install)
  #:use-module ((guix build store-copy)
                #:select (file-size))
  #:use-module (json)                             ;guile-json
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-71)
  #:use-module ((texinfo string-utils)
                #:select (escape-special-chars))
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:export (%docker-image-max-layers
            build-docker-image))

;; The maximum number of layers allowed in a Docker image is typically around
;; 128, although it may vary depending on the Docker daemon. However, we
;; recommend setting the limit to 100 to ensure sufficient room for future
;; extensions.
(define %docker-image-max-layers
  #f)

;; Generate a 256-bit identifier in hexadecimal encoding for the Docker image.
(define docker-id
  (compose bytevector->base16-string sha256 string->utf8))

(define (layer-diff-id layer)
  "Generate a layer DiffID for the given LAYER archive."
  (string-append "sha256:" (bytevector->base16-string (file-sha256 layer))))

;; This is the semantic version of the JSON metadata schema according to
;; https://github.com/docker/docker/blob/master/image/spec/v1.2.md
;; It is NOT the version of the image specification.
(define schema-version "1.0")

(define (image-description id time)
  "Generate a simple image description."
  `((id . ,id)
    (created . ,time)
    (container_config . #nil)))

(define (canonicalize-repository-name name)
  "\"Repository\" names are restricted to roughly [a-z0-9_.-].
Return a version of TAG that follows these rules."
  ;; Refer to https://docs.docker.com/docker-hub/repos/.
  (define min-length 2)
  (define padding-character #\a)
  (define max-length 255)

  (define ascii-letters
    (string->char-set "abcdefghijklmnopqrstuvwxyz"))

  (define separators
    (string->char-set "_-."))

  (define repo-char-set
    (char-set-union char-set:digit ascii-letters separators))

  (define normalized-name
    (string-map (lambda (chr)
                  (if (char-set-contains? repo-char-set chr)
                      chr
                      #\.))
                (string-trim (string-downcase name) separators)))

  (let ((l (string-length normalized-name)))
    (match l
      ((? (cut > <> max-length))
       (string-take normalized-name max-length))
      ((? (cut < <> min-length))
       (string-append normalized-name
                      (make-string (- min-length l) padding-character)))
      (_ normalized-name))))

(define* (manifest path layers #:optional (tag "guix"))
  "Generate a simple image manifest."
  (let ((tag (canonicalize-repository-name tag)))
    `#(((Config . "config.json")
        (RepoTags . #(,(string-append tag ":latest")))
        (Layers . ,(list->vector layers))))))

;; According to the specifications this is required for backwards
;; compatibility.  It duplicates information provided by the manifest.
(define* (repositories path id #:optional (tag "guix"))
  "Generate a repositories file referencing PATH and the image ID."
  `((,(canonicalize-repository-name tag) . ((latest . ,id)))))

;; See https://github.com/opencontainers/image-spec/blob/master/config.md
(define* (config layers-diff-ids time arch #:key entry-point (environment '()))
  "Generate a minimal image configuration for the given LAYERS files."
  ;; "architecture" must be values matching "platform.arch" in the
  ;; runtime-spec at
  ;; https://github.com/opencontainers/runtime-spec/blob/v1.0.0-rc2/config.md#platform
  `((architecture . ,arch)
    (comment . "Generated by GNU Guix")
    (created . ,time)
    (config . ,`((env . ,(list->vector
                          (map (match-lambda
                                 ((name . value)
                                  (string-append name "=" value)))
                               environment)))
                 ,@(if entry-point
                       `((entrypoint . ,(list->vector entry-point)))
                       '())))
    (container_config . #nil)
    (os . "linux")
    (rootfs . ((type . "layers")
               (diff_ids . ,(list->vector layers-diff-ids))))))

(define directive-file
  ;; Return the file or directory created by a 'evaluate-populate-directive'
  ;; directive.
  (match-lambda
    ((source '-> target)
     (string-trim source #\/))
    (('directory name _ ...)
     (string-trim name #\/))))

(define (size-sorted-store-items items max-layers)
  "Split list of ITEMS at %MAX-LAYERS and sort by disk usage."
  (let* ((items-length (length items))
         (head tail
               (split-at
                (map (match-lambda ((size . item) item))
                     (sort (map (lambda (item)
                                  (cons (file-size item) item))
                                items)
                           (lambda (item1 item2)
                             (< (match item2 ((size . _) size))
                                (match item1 ((size . _) size))))))
                (if (>= items-length max-layers)
                    (- max-layers 2)
                    (1- items-length)))))
    (list head tail)))

(define (create-empty-tar file)
  (invoke "tar" "-cf" file "--files-from" "/dev/null"))

(define* (build-docker-image image paths prefix
                             #:key
                             (repository "guix")
                             (extra-files '())
                             (transformations '())
                             (system (utsname:machine (uname)))
                             database
                             entry-point
                             (environment '())
                             compressor
                             (creation-time (current-time time-utc))
                             max-layers
                             root-system)
  "Write to IMAGE a layerer Docker image archive containing the given PATHS.
PREFIX must be a store path that is a prefix of any store paths in PATHS.
REPOSITORY is a descriptive name that will show up in \"REPOSITORY\" column of
the output of \"docker images\".

When DATABASE is true, copy it to /var/guix/db in the image and create
/var/guix/gcroots and friends.

When ENTRY-POINT is true, it must be a list of strings; it is stored as the
entry point in the Docker image JSON structure.

ENVIRONMENT must be a list of name/value pairs.  It specifies the environment
variables that must be defined in the resulting image.

EXTRA-FILES must be a list of directives for 'evaluate-populate-directive'
describing non-store files that must be created in the image.

TRANSFORMATIONS must be a list of (OLD -> NEW) tuples describing how to
transform the PATHS.  Any path in PATHS that begins with OLD will be rewritten
in the Docker image so that it begins with NEW instead.  If a path is a
non-empty directory, then its contents will be recursively added, as well.

SYSTEM is a GNU triplet (or prefix thereof) of the system the binaries in
PATHS are for; it is used to produce metadata in the image.  Use COMPRESSOR, a
command such as '(\"gzip\" \"-9n\"), to compress IMAGE.  Use CREATION-TIME, a
SRFI-19 time-utc object, as the creation time in metadata.

When MAX-LAYERS is not false build layered image, providing a Docker
image with store paths splitted in their own layers to improve sharing
between images.

ROOT-SYSTEM is a directory with a provisioned root file system, which will be
added to image as a layer."
  (define (sanitize path-fragment)
    (escape-special-chars
     ;; GNU tar strips the leading slash off of absolute paths before applying
     ;; the transformations, so we need to do the same, or else our
     ;; replacements won't match any paths.
     (string-trim path-fragment #\/)
     ;; Escape the basic regexp special characters (see: "(sed) BRE syntax").
     ;; We also need to escape "/" because we use it as a delimiter.
     "/*.^$[]\\"
     #\\))
  (define transformation->replacement
    (match-lambda
      ((old '-> new)
       ;; See "(tar) transform" for details on the expression syntax.
       (string-append "s/^" (sanitize old) "/" (sanitize new) "/"))))
  (define (transformations->expression transformations)
    (let ((replacements (map transformation->replacement transformations)))
      (string-append
       ;; Avoid transforming link targets, since that would break some links
       ;; (e.g., symlinks that point to an absolute store path).
       "flags=rSH;"
       (string-join replacements ";")
       ;; Some paths might still have a leading path delimiter even after tar
       ;; transforms them (e.g., "/a/b" might be transformed into "/b"), so
       ;; strip any leading path delimiters that remain.
       ";s,^//*,,")))
  (define transformation-options
    (if (eq? '() transformations)
        '()
        `("--transform" ,(transformations->expression transformations))))
  (define (seal-layer)
    ;; Add 'layer.tar' to 'image.tar' under the right name.  Return its hash.
    (let* ((file-hash (layer-diff-id "layer.tar"))
           (file-name (string-append file-hash "/layer.tar")))
      (mkdir file-hash)
      (rename-file "layer.tar" file-name)
      (invoke "tar" "-rf" "image.tar" file-name)
      (delete-file file-name)
      file-hash))
  (define layers-hashes
    ;; Generate a tarball that includes container image layers as tarballs,
    ;; along with a manifest.json file describing the layer and config file
    ;; locations.
    (match-lambda
      (((head ...) (tail ...) id)
       (create-empty-tar "image.tar")
       (let* ((head-layers
               (map
                (lambda (file)
                  (invoke "tar" "cf" "layer.tar" file)
                  (seal-layer))
                head))
              (tail-layer
               (begin
                 (create-empty-tar "layer.tar")
                 (for-each (lambda (file)
                             (invoke "tar" "-rf" "layer.tar" file))
                           tail)
                 (let* ((file-hash (layer-diff-id "layer.tar"))
                        (file-name (string-append file-hash "/layer.tar")))
                   (mkdir file-hash)
                   (rename-file "layer.tar" file-name)
                   (invoke "tar" "-rf" "image.tar" file-name)
                   (delete-file file-name)
                   file-hash)))
              (customization-layer
               (let* ((file-id (string-append id "/layer.tar"))
                      (file-hash (layer-diff-id file-id))
                      (file-name (string-append file-hash "/layer.tar")))
                 (mkdir file-hash)
                 (rename-file file-id file-name)
                 (invoke "tar" "-rf" "image.tar" file-name)
                 file-hash))
              (all-layers
               (append head-layers (list tail-layer customization-layer))))
         (with-output-to-file "manifest.json"
           (lambda ()
             (scm->json (manifest prefix
                                  (map (cut string-append <> "/layer.tar")
                                       all-layers)
                                  repository))))
         (invoke "tar" "-rf" "image.tar" "manifest.json")
         all-layers))))
  (let* ((directory "/tmp/docker-image") ;temporary working directory
         (id (docker-id prefix))
         (time (date->string (time-utc->date creation-time) "~4"))
         (arch (let-syntax ((cond* (syntax-rules ()
                                     ((_ (pattern clause) ...)
                                      (cond ((string-prefix? pattern system)
                                             clause)
                                            ...
                                            (else
                                             (error "unsupported system"
                                                    system)))))))
                 (cond* ("x86_64"  "amd64")
                        ("i686"    "386")
                        ("arm"     "arm")
                        ("aarch64" "arm64")
                        ("mips64"  "mips64le")))))
    ;; Make sure we start with a fresh, empty working directory.
    (mkdir directory)
    (with-directory-excursion directory
      (mkdir id)
      (with-directory-excursion id
        (with-output-to-file "VERSION"
          (lambda () (display schema-version)))
        (with-output-to-file "json"
          (lambda () (scm->json (image-description id time))))

        (if root-system
            (let ((directory (getcwd)))
              (with-directory-excursion root-system
                (apply invoke "tar"
                       "-cf" (string-append directory "/layer.tar")
                       `(,@transformation-options
                         ,@(tar-base-options)
                         ,@(scandir "."
                                    (lambda (file)
                                      (not (member file '("." "..")))))))))
            (begin
              ;; Create a directory for the non-store files that need to go
              ;; into the archive.
              (mkdir "extra")

              (with-directory-excursion "extra"
                ;; Create non-store files.
                (for-each (cut evaluate-populate-directive <> "./")
                          extra-files)

                (when database
                  ;; Initialize /var/guix, assuming PREFIX points to a
                  ;; profile.
                  (install-database-and-gc-roots "." database prefix))

                (apply invoke "tar" "-cf" "../layer.tar"
                       `(,@transformation-options
                         ,@(tar-base-options)
                         ,@(if max-layers '() paths)
                         ,@(scandir "."
                                    (lambda (file)
                                      (not (member file '("." ".."))))))))
              (delete-file-recursively "extra")))

        ;; It is possible for "/" to show up in the archive, especially when
        ;; applying transformations.  For example, the transformation
        ;; "s,^/a,," will (perhaps surprisingly) cause GNU tar to transform
        ;; the path "/a" into "/".  The presence of "/" in the archive is
        ;; probably benign, but it is definitely safe to remove it, so let's
        ;; do that.  This fails when "/" is not in the archive, so use system*
        ;; instead of invoke to avoid an exception in that case, and redirect
        ;; stderr to the bit bucket to avoid "Exiting with failure status"
        ;; error messages.
        (with-error-to-port (%make-void-port "w")
          (lambda ()
            (system* "tar" "--delete" "/" "-f" "layer.tar"))))

      (with-output-to-file "config.json"
        (lambda ()
          (scm->json
           (config (if max-layers
                       (layers-hashes
                        (append (size-sorted-store-items paths max-layers)
                                (list id)))
                       (list (layer-diff-id (string-append id "/layer.tar"))))
                   time arch
                   #:environment environment
                   #:entry-point entry-point))))
      (if max-layers
          (begin
            (invoke "tar" "-rf" "image.tar" "config.json")
            (if compressor
                (begin
                  (apply invoke `(,@compressor "image.tar"))
                  (copy-file "image.tar.gz" image))
                (copy-file "image.tar" image)))
          (begin
            (with-output-to-file "manifest.json"
              (lambda ()
                (scm->json (manifest prefix
                                     (list (string-append id "/layer.tar"))
                                     repository))))
            (with-output-to-file "repositories"
              (lambda ()
                (scm->json (repositories prefix id repository))))
            (apply invoke "tar" "-cf" image
                   `(,@(tar-base-options #:compressor compressor)
                     ".")))))
    (delete-file-recursively directory)))
