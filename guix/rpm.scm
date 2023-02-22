;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2023 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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

(define-module (guix rpm)
  #:autoload (gcrypt hash) (hash-algorithm file-hash md5)
  #:use-module (guix build utils)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 textual-ports)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-71)
  #:use-module (srfi srfi-171)
  #:export (generate-lead
            generate-signature
            generate-header
            assemble-rpm-metadata

            ;; XXX: These are internals, but the inline disabling trick
            ;; doesn't work on them.
            make-header-entry
            header-entry?
            header-entry-tag
            header-entry-count
            header-entry-value

            bytevector->hex-string

            fhs-directory?))

;;; Commentary:
;;;
;;; This module provides the building blocks required to construct RPM
;;; archives.  It is intended to be importable on the build side, so shouldn't
;;; depend on (guix diagnostics) or other host-side-only modules.
;;;
;;; Code:

(define (gnu-system-triplet->machine-type triplet)
  "Return the machine component of TRIPLET, a GNU system triplet."
  (first (string-split triplet #\-)))

(define (gnu-machine-type->rpm-arch type)
  "Return the canonical RPM architecture string, given machine TYPE."
  (match type
    ("arm" "armv7hl")
    ("powerpc" "ppc")
    ("powerpc64le" "ppc64le")
    (machine machine)))                 ;unchanged

(define (gnu-machine-type->rpm-number type)
  "Translate machine TYPE to its corresponding RPM integer value."
  ;; Refer to the rpmrc.in file in the RPM source for the complete
  ;; translation tables.
  (match type
    ((or "i486" "i586" "i686" "x86_64")	1)
    ((? (cut string-prefix? "powerpc" <>)) 5)
    ("mips64el"	11)
    ((? (cut string-prefix? "arm" <>)) 12)
    ("aarch64" 19)
    ((? (cut string-prefix? "riscv" <>)) 22)
    (_ (error "no RPM number known for machine type" type))))

(define (u16-number->u8-list number)
  "Return a list of byte values made of NUMBER, a 16 bit unsigned integer."
  (let ((bv (uint-list->bytevector (list number) (endianness big) 2)))
    (bytevector->u8-list bv)))

(define (u32-number->u8-list number)
  "Return a list of byte values made of NUMBER, a 32 bit unsigned integer."
  (let ((bv (uint-list->bytevector (list number) (endianness big) 4)))
    (bytevector->u8-list bv)))

(define (s32-number->u8-list number)
  "Return a list of byte values made of NUMBER, a 32 bit signed integer."
  (let ((bv (sint-list->bytevector (list number) (endianness big) 4)))
    (bytevector->u8-list bv)))

(define (u8-list->u32-number lst)
  "Return the 32 bit unsigned integer corresponding to the 4 bytes in LST."
  (bytevector-u32-ref (u8-list->bytevector lst) 0 (endianness big)))


;;;
;;; Lead section.
;;;

;; Refer to the docs/manual/format.md file of the RPM source for the details
;; regarding the binary format of an RPM archive.
(define* (generate-lead name-version #:key (target %host-type))
  "Generate a RPM lead u8-list that uses NAME-VERSION, the name and version
string of the package, and TARGET, a GNU triplet used to derive the target
machine type."
  (define machine-type (gnu-system-triplet->machine-type target))
  (define magic (list #xed #xab #xee #xdb))
  (define file-format-version (list 3 0)) ;3.0
  (define type (list 0 0))                ;0 for binary packages
  (define arch-number (u16-number->u8-list
                       (gnu-machine-type->rpm-number machine-type)))
  ;; The 66 bytes from 10 to 75 are for the name-version-release string.
  (define name
    (let ((padding-bytes (make-list (- 66 (string-length name-version)) 0)))
      (append (bytevector->u8-list (string->utf8 name-version))
              padding-bytes)))
  ;; There is no OS number corresponding to GNU/Hurd (GNU), only Linux, per
  ;; rpmrc.in.
  (define os-number (list 0 1))

  ;; For RPM format 3.0, the signature type is 5, which means a "Header-style"
  ;; signature.
  (define signature-type (list 0 5))

  (define reserved-bytes (make-list 16 0))

  (append magic file-format-version type arch-number name
          os-number signature-type reserved-bytes))


;;;
;;; Header section.
;;;

(define header-magic (list #x8e #xad #xe8))
(define header-version (list 1))
(define header-reserved (make-list 4 0)) ;4 reserved bytes
;;; Every header starts with 8 bytes made by the header magic number, the
;;; header version and 4 reserved bytes.
(define header-intro (append header-magic header-version header-reserved))

;;; Header entry data types.
(define NULL 0)
(define CHAR 1)
(define INT8 2)
(define INT16 3)                        ;2-bytes aligned
(define INT32 4)                        ;4-bytes aligned
(define INT64 5)                        ;8-bytes aligned
(define STRING 6)
(define BIN 7)
(define STRING_ARRAY 8)
(define I18NSTRIN_TYPE 9)

;;; Header entry tags.
(define-record-type <rpm-tag>
  (make-rpm-tag number type)
  rpm-tag?
  (number rpm-tag-number)
  (type rpm-tag-type))

;;; The following are internal tags used to identify the data sections.
(define RPMTAG_HEADERSIGNATURES (make-rpm-tag 62 BIN)) ;signature header
(define RPMTAG_HEADERIMMUTABLE (make-rpm-tag 63 BIN))  ;main/data header
(define RPMTAG_HEADERI18NTABLE (make-rpm-tag 100 STRING_ARRAY))

;;; Subset of RPM tags from include/rpm/rpmtag.h.
(define RPMTAG_NAME (make-rpm-tag 1000 STRING))
(define RPMTAG_VERSION (make-rpm-tag 1001 STRING))
(define RPMTAG_RELEASE (make-rpm-tag 1002 STRING))
(define RPMTAG_SUMMARY (make-rpm-tag 1004 STRING))
(define RPMTAG_SIZE (make-rpm-tag 1009 INT32))
(define RPMTAG_LICENSE (make-rpm-tag 1014 STRING))
(define RPMTAG_OS (make-rpm-tag 1021 STRING))
(define RPMTAG_ARCH (make-rpm-tag 1022 STRING))
(define RPMTAG_PREIN (make-rpm-tag 1023 STRING))
(define RPMTAG_POSTIN (make-rpm-tag 1024 STRING))
(define RPMTAG_PREUN (make-rpm-tag 1025 STRING))
(define RPMTAG_POSTUN (make-rpm-tag 1026 STRING))
(define RPMTAG_FILESIZES (make-rpm-tag 1028 INT32))
(define RPMTAG_FILEMODES (make-rpm-tag 1030 INT16))
(define RPMTAG_FILEDIGESTS (make-rpm-tag 1035 STRING_ARRAY))
(define RPMTAG_FILELINKTOS (make-rpm-tag 1036 STRING_ARRAY))
(define RPMTAG_FILEUSERNAME (make-rpm-tag 1039 STRING_ARRAY))
(define RPMTAG_GROUPNAME (make-rpm-tag 1040 STRING_ARRAY))
(define RPMTAG_PREFIXES (make-rpm-tag 1098 STRING_ARRAY))
(define RPMTAG_DIRINDEXES (make-rpm-tag 1116 INT32))
(define RPMTAG_BASENAMES (make-rpm-tag 1117 STRING_ARRAY))
(define RPMTAG_DIRNAMES (make-rpm-tag 1118 STRING_ARRAY))
(define RPMTAG_PAYLOADFORMAT (make-rpm-tag 1124 STRING))
(define RPMTAG_PAYLOADCOMPRESSOR (make-rpm-tag 1125 STRING))
(define RPMTAG_LONGFILESIZES (make-rpm-tag 5008 INT64))
(define RPMTAG_LONGSIZE (make-rpm-tag 5009 INT64))
;;; The algorithm used to compute the digest of each file, e.g. RPM_HASH_MD5.
(define RPMTAG_FILEDIGESTALGO (make-rpm-tag 5011 INT32))
;;; RPMTAG_ENCODING specifies the encoding used for strings, e.g. "utf-8".
(define RPMTAG_ENCODING (make-rpm-tag 5062 STRING))
;;; Compressed payload digest.  Its type is a string array, but currently in
;;; practice it is equivalent to STRING, since only the first element is used.
(define RPMTAG_PAYLOADDIGEST (make-rpm-tag 5092 STRING_ARRAY))
;;; The algorithm used to compute the payload digest, e.g. RPM_HASH_SHA256.
(define RPMTAG_PAYLOADDIGESTALGO (make-rpm-tag 5093 INT32))
;;; The following are taken from the rpmHashAlgo_e enum in rpmcrypto.h.
(define RPM_HASH_MD5 1)
(define RPM_HASH_SHA256 8)

;;; Other useful internal definitions.
(define REGION_TAG_COUNT 16)            ;number of bytes
(define INT32_MAX (1- (expt 2 32)))     ;4294967295 bytes (unsigned)

(define (rpm-tag->u8-list tag)
  "Return the u8 list corresponding to RPM-TAG, a <rpm-tag> object."
  (append (u32-number->u8-list (rpm-tag-number tag))
          (u32-number->u8-list (rpm-tag-type tag))))

(define-record-type <header-entry>
  (make-header-entry tag count value)
  header-entry?
  (tag header-entry-tag)                ;<rpm-tag>
  (count header-entry-count)            ;number (u32)
  (value header-entry-value))           ;string|number|list|...

(define (entry-type->alignement type)
  "Return the byte alignment of TYPE, an RPM header entry type."
  (cond ((= INT16 type) 2)
        ((= INT32 type) 4)
        ((= INT64 type) 8)
        (else 1)))

(define (next-aligned-offset offset alignment)
  "Return the next position from OFFSET which satisfies ALIGNMENT."
  (if (= 0 (modulo offset alignment))
      offset
      (next-aligned-offset (1+ offset) alignment)))

(define (header-entry->data entry)
  "Return the data of ENTRY, a <header-entry> object, as a u8 list."
  (let* ((tag (header-entry-tag entry))
         (count (header-entry-count entry))
         (value (header-entry-value entry))
         (number (rpm-tag-number tag))
         (type (rpm-tag-type tag)))
    (cond
     ((= STRING type)
      (unless (string? value)
        (error "expected string value for STRING type, got" value))
      (unless (= 1 count)
        (error "count must be 1 for STRING type"))
      (let ((value (cond ((= (rpm-tag-number RPMTAG_VERSION) number)
                          ;; Hyphens are not allowed in version strings.
                          (string-map (match-lambda
                                        (#\- #\+)
                                        (c c))
                                      value))
                         (else value))))
        (append (bytevector->u8-list (string->utf8 value))
                (list 0))))             ;strings must end with null byte
     ((= STRING_ARRAY type)
      (unless (list? value)
        (error "expected a list of strings for STRING_ARRAY type, got" value))
      (unless (= count (length value))
        (error "expected count to be equal to" (length value) 'got count))
      (append-map (lambda (s)
                    (append (bytevector->u8-list (string->utf8 s))
                            (list 0)))  ;null byte separated
                  value))
     ((member type (list INT8 INT16 INT32))
      (if (= 1 count)
          (unless (number? value)
            (error "expected number value for scalar INT type; got" value))
          (unless (list? value)
            (error "expected list value for array INT type; got" value)))
      (if (list? value)
          (cond ((= INT8 type) value)
                ((= INT16 type) (append-map u16-number->u8-list value))
                ((= INT32 type) (append-map u32-number->u8-list value))
                (else (error "unexpected type" type)))
          (cond ((= INT8 type) (list value))
                ((= INT16 type) (u16-number->u8-list value))
                ((= INT32 type) (u32-number->u8-list value))
                (else (error "unexpected type" type)))))
     ((= BIN type)
      (unless (list? value)
        (error "expected list value for BIN type; got" value))
      value)
     (else (error "unimplemented type" type)))))

(define (make-header-index+data entries)
  "Return the index and data sections as u8 number lists, via multiple values.
An index is composed of four u32 (16 bytes total) quantities, in order: tag,
type, offset and count."
  (match (fold (match-lambda*
                 ((entry (offset . (index . data)))
                  (let* ((tag (header-entry-tag entry))
                         (tag-number (rpm-tag-number tag))
                         (tag-type (rpm-tag-type tag))
                         (count (header-entry-count entry))
                         (data* (header-entry->data entry))
                         (alignment (entry-type->alignement tag-type))
                         (aligned-offset (next-aligned-offset offset alignment))
                         (padding (make-list (- aligned-offset offset) 0)))
                    (cons (+ aligned-offset (length data*))
                          (cons (append index
                                        (u32-number->u8-list tag-number)
                                        (u32-number->u8-list tag-type)
                                        (u32-number->u8-list aligned-offset)
                                        (u32-number->u8-list count))
                                (append data padding data*))))))
               '(0 . (() . ()))
               entries)
    ((offset . (index . data))
     (values index data))))

;; Prevent inlining of the variables/procedures accessed by unit tests.
(set! make-header-index+data make-header-index+data)
(set! RPMTAG_ARCH RPMTAG_ARCH)
(set! RPMTAG_LICENSE RPMTAG_LICENSE)
(set! RPMTAG_NAME RPMTAG_NAME)
(set! RPMTAG_OS RPMTAG_OS)
(set! RPMTAG_RELEASE RPMTAG_RELEASE)
(set! RPMTAG_SUMMARY RPMTAG_SUMMARY)
(set! RPMTAG_VERSION RPMTAG_VERSION)

(define (wrap-in-region-tags header region-tag)
  "Wrap HEADER, a header provided as u8-list with REGION-TAG."
  (let* ((type (rpm-tag-type region-tag))
         (header-intro (take header 16))
         (header-rest (drop header 16))
         ;; Increment the existing index value to account for the added region
         ;; tag index.
         (index-length (1+ (u8-list->u32-number
                            (drop-right (drop header-intro 8) 4)))) ;bytes 8-11
         ;; Increment the data length value to account for the added region
         ;; tag data.
         (data-length (+ REGION_TAG_COUNT
                         (u8-list->u32-number
                          (take-right header-intro 4))))) ;last 4 bytes of intro
    (unless (member region-tag (list RPMTAG_HEADERSIGNATURES
                                     RPMTAG_HEADERIMMUTABLE))
      (error "expected RPMTAG_HEADERSIGNATURES or RPMTAG_HEADERIMMUTABLE, got"
             region-tag))
    (append (drop-right header-intro 8) ;strip existing index and data lengths
            (u32-number->u8-list index-length)
            (u32-number->u8-list data-length)
            ;; Region tag (16 bytes).
            (u32-number->u8-list (rpm-tag-number region-tag))      ;number
            (u32-number->u8-list type)                             ;type
            (u32-number->u8-list (- data-length REGION_TAG_COUNT)) ;offset
            (u32-number->u8-list REGION_TAG_COUNT)                 ;count
            ;; Immutable region.
            header-rest
            ;; Region tag trailer (16 bytes).  Note: the trailer offset value
            ;; is an enforced convention; it has no practical use.
            (u32-number->u8-list (rpm-tag-number region-tag)) ;number
            (u32-number->u8-list type)                        ;type
            (s32-number->u8-list (* -1 index-length 16))      ;negative offset
            (u32-number->u8-list REGION_TAG_COUNT))))         ;count

(define (bytevector->hex-string bv)
  (format #f "~{~2,'0x~}" (bytevector->u8-list bv)))

(define (files->md5-checksums files)
  "Return the MD5 checksums (formatted as hexadecimal strings) for FILES."
  (let ((file-md5 (cut file-hash (hash-algorithm md5) <>)))
    (map (lambda (f)
           (or (and=> (false-if-exception (file-md5 f))
                      bytevector->hex-string)
               ;; Only regular files (e.g., not directories) can have their
               ;; checksum computed.
               ""))
         files)))

(define (strip-leading-dot name)
  "Remove the leading \".\" from NAME, if present.  If a single \".\" is
encountered, translate it to \"/\"."
  (match name
    ("." "/")                           ;special case
    ((? (cut string-prefix? "." <>))
     (string-drop name 1))
    (x name)))

;;; An extensive list of required and optional FHS directories, per its 3.0
;;; revision.
(define %fhs-directories
  (list "/bin" "/boot" "/dev"
        "/etc" "/etc/opt" "/etc/X11" "/etc/sgml" "/etc/xml"
        "/home" "/root" "/lib" "/media" "/mnt"
        "/opt" "/opt/bin" "/opt/doc" "/opt/include"
        "/opt/info" "/opt/lib" "/opt/man"
        "/run" "/sbin" "/srv" "/sys" "/tmp"
        "/usr" "/usr/bin" "/usr/include" "/usr/libexec"
        "/usr/share" "/usr/share/applications"
        "/usr/share/color" "/usr/share/dict" "/usr/share/doc" "/usr/share/games"
        "/usr/share/icons" "/usr/share/icons/hicolor"
        "/usr/share/icons/hicolor/48x48"
        "/usr/share/icons/hicolor/48x48/apps"
        "/usr/share/icons/hicolor/scalable"
        "/usr/share/icons/hicolor/scalable/apps"
        "/usr/share/info" "/usr/share/locale" "/usr/share/man"
        "/usr/share/metainfo" "/usr/share/misc"
        "/usr/share/nls" "/usr/share/ppd" "/usr/share/sgml"
        "/usr/share/terminfo" "/usr/share/tmac" "/usr/share/xml"
        "/usr/share/zoneinfo" "/usr/local" "/usr/local/bin" "/usr/local/etc"
        "/usr/local/games" "/usr/local/include" "/usr/local/lib"
        "/usr/local/man" "/usr/local/sbin" "/usr/local/sbin" "/usr/local/share"
        "/usr/local/src" "/var" "/var/account" "/var/backups"
        "/var/cache" "/var/cache/fonts" "/var/cache/man" "/var/cache/www"
        "/var/crash" "/var/cron" "/var/games" "/var/mail" "/var/msgs"
        "/var/lib" "/var/lib/color" "/var/lib/hwclock" "/var/lib/misc"
        "/var/local" "/var/lock" "/var/log" "/var/opt" "/var/preserve"
        "/var/run" "/var/spool" "/var/spool/lpd" "/var/spool/mqueue"
        "/var/spool/news" "/var/spool/rwho" "/var/spool/uucp"
        "/var/tmp" "/var/yp"))

(define (fhs-directory? file-name)
  "Predicate to check if FILE-NAME is a known File Hierarchy Standard (FHS)
directory."
  (member (strip-leading-dot file-name) %fhs-directories))

(define (directory->file-entries directory)
  "Return the file lists triplet header entries for the files found under
DIRECTORY."
  (with-directory-excursion directory
    ;; Skip the initial "." directory, as its name would get concatenated with
    ;; the "./" dirname and fail to match "." in the payload.
    (let* ((files (cdr (find-files "." #:directories? #t)))
           (file-stats (map lstat files))
           (directories
            (append (list ".")
                    (filter-map (match-lambda
                                  ((index . file)
                                   (let ((st (list-ref file-stats index)))
                                     (and (eq? 'directory (stat:type st))
                                          file))))
                                (list-transduce (tenumerate) rcons files))))
           ;; Omit any FHS directories found in FILES to avoid the RPM package
           ;; from owning them.  This can occur when symlinks directives such
           ;; as "/usr/bin/hello -> bin/hello" are used.
           (package-files package-file-stats
                          (unzip2 (reverse
                                   (fold (lambda (file stat res)
                                           (if (fhs-directory? file)
                                               res
                                               (cons (list file stat) res)))
                                         '() files file-stats))))

           ;; When provided with the index of a file, the directory index must
           ;; return the index of the corresponding directory entry.
           (dirindexes (map (lambda (d)
                              (list-index (cut string=? <> d) directories))
                            (map dirname package-files)))
           ;; The files owned are those appearing in 'basenames'; own them
           ;; all.
           (basenames (map basename package-files))
           ;; The directory names must end with a trailing "/".
           (dirnames (map (compose strip-leading-dot (cut string-append <> "/"))
                          directories))
           ;; Note: All the file-related entries must have the same length as
           ;; the basenames entry.
           (symlink-targets (map (lambda (f)
                                   (if (symbolic-link? f)
                                       (readlink f)
                                       "")) ;unused
                                 package-files))
           (file-modes (map stat:mode package-file-stats))
           (file-sizes (map stat:size package-file-stats))
           (file-md5s (files->md5-checksums package-files)))
      (let ((basenames-length (length basenames))
            (dirindexes-length (length dirindexes)))
        (unless (= basenames-length dirindexes-length)
          (error "length mismatch for dirIndexes; expected/actual"
                 basenames-length dirindexes-length))
        (append
         (if (> (apply max file-sizes) INT32_MAX)
             (list (make-header-entry RPMTAG_LONGFILESIZES (length file-sizes)
                                      file-sizes)
                   (make-header-entry RPMTAG_LONGSIZE 1
                                      (reduce + 0 file-sizes)))
             (list (make-header-entry RPMTAG_FILESIZES (length file-sizes)
                                      file-sizes)
                   (make-header-entry RPMTAG_SIZE 1 (reduce + 0 file-sizes))))
         (list
          (make-header-entry RPMTAG_FILEMODES (length file-modes) file-modes)
          (make-header-entry RPMTAG_FILEDIGESTS (length file-md5s) file-md5s)
          (make-header-entry RPMTAG_FILEDIGESTALGO 1 RPM_HASH_MD5)
          (make-header-entry RPMTAG_FILELINKTOS (length symlink-targets)
                             symlink-targets)
          (make-header-entry RPMTAG_FILEUSERNAME basenames-length
                             (make-list basenames-length "root"))
          (make-header-entry RPMTAG_GROUPNAME basenames-length
                             (make-list basenames-length "root"))
          ;; The dirindexes, basenames and dirnames tags form the so-called RPM
          ;; "path triplet".
          (make-header-entry RPMTAG_DIRINDEXES dirindexes-length dirindexes)
          (make-header-entry RPMTAG_BASENAMES basenames-length basenames)
          (make-header-entry RPMTAG_DIRNAMES (length dirnames) dirnames)))))))

(define (make-header entries)
  "Return the u8 list of a RPM header containing ENTRIES, a list of
<rpm-entry> objects."
  (let* ((entries (sort entries (lambda (x y)
                                  (< (rpm-tag-number (header-entry-tag x))
                                     (rpm-tag-number (header-entry-tag y))))))
         (count (length entries))
         (index data (make-header-index+data entries)))
    (append header-intro                        ;8 bytes
            (u32-number->u8-list count)         ;4 bytes
            (u32-number->u8-list (length data)) ;4 bytes
            ;; Now starts the header index, which can contain up to 32 entries
            ;; of 16 bytes each.
            index data)))

(define* (generate-header name version
                          payload-digest
                          payload-directory
                          payload-compressor
                          #:key
                          relocatable?
                          prein-file postin-file
                          preun-file postun-file
                          (target %host-type)
                          (release "0")
                          (license "N/A")
                          (summary "RPM archive generated by GNU Guix.")
                          (os "Linux")) ;see rpmrc.in
  "Return the u8 list corresponding to the Header section.  PAYLOAD-DIGEST is
the SHA256 checksum string of the compressed payload.  PAYLOAD-DIRECTORY is
the directory containing the payload files.  PAYLOAD-COMPRESSOR is the name of
the compressor used to compress the CPIO payload, such as \"none\", \"gz\",
\"xz\" or \"zstd\"."
  (let* ((rpm-arch (gnu-machine-type->rpm-arch
                    (gnu-system-triplet->machine-type target)))
         (file->string (cut call-with-input-file <> get-string-all))
         (prein-script (and=> prein-file file->string))
         (postin-script (and=> postin-file file->string))
         (preun-script (and=> preun-file file->string))
         (postun-script (and=> postun-file file->string)))
    (wrap-in-region-tags
     (make-header (append
                   (list (make-header-entry RPMTAG_HEADERI18NTABLE 1 (list "C"))
                         (make-header-entry RPMTAG_NAME 1 name)
                         (make-header-entry RPMTAG_VERSION 1 version)
                         (make-header-entry RPMTAG_RELEASE 1 release)
                         (make-header-entry RPMTAG_SUMMARY 1 summary)
                         (make-header-entry RPMTAG_LICENSE 1 license)
                         (make-header-entry RPMTAG_OS 1 os)
                         (make-header-entry RPMTAG_ARCH 1 rpm-arch))
                   (directory->file-entries payload-directory)
                   (if relocatable?
                       ;; Note: RPMTAG_PREFIXES must not have a trailing
                       ;; slash, unless it's '/'.  This allows installing the
                       ;; package via 'rpm -i --prefix=/tmp', for example.
                       (list (make-header-entry RPMTAG_PREFIXES 1 (list "/")))
                       '())
                   (if prein-script
                       (list (make-header-entry RPMTAG_PREIN 1 prein-script))
                       '())
                   (if postin-script
                       (list (make-header-entry RPMTAG_POSTIN 1 postin-script))
                       '())
                   (if preun-script
                       (list (make-header-entry RPMTAG_PREUN 1 preun-script))
                       '())
                   (if postun-script
                       (list (make-header-entry RPMTAG_POSTUN 1 postun-script))
                       '())
                   (if (string=? "none" payload-compressor)
                       '()
                       (list (make-header-entry RPMTAG_PAYLOADCOMPRESSOR 1
                                                payload-compressor)))
                   (list (make-header-entry RPMTAG_ENCODING 1 "utf-8")
                         (make-header-entry RPMTAG_PAYLOADFORMAT 1 "cpio")
                         (make-header-entry RPMTAG_PAYLOADDIGEST 1
                                            (list payload-digest))
                         (make-header-entry RPMTAG_PAYLOADDIGESTALGO 1
                                            RPM_HASH_SHA256))))
     RPMTAG_HEADERIMMUTABLE)))


;;;
;;; Signature section
;;;

;;; Header sha256 checksum.
(define RPMSIGTAG_SHA256 (make-rpm-tag 273 STRING))
;;; Uncompressed payload size.
(define RPMSIGTAG_PAYLOADSIZE (make-rpm-tag 1007 INT32))
;;; Header and compressed payload combined size.
(define RPMSIGTAG_SIZE (make-rpm-tag 1000 INT32))
;;; Uncompressed payload size (when size > max u32).
(define RPMSIGTAG_LONGARCHIVESIZE (make-rpm-tag 271 INT64))
;;; Header and compressed payload combined size (when size > max u32).
(define RPMSIGTAG_LONGSIZE (make-rpm-tag 270 INT64))
;;; Extra space reserved for signatures (typically 32 bytes).
(define RPMSIGTAG_RESERVEDSPACE (make-rpm-tag 1008 BIN))

(define (generate-signature header-sha256
                            header+compressed-payload-size
                            ;; uncompressed-payload-size
                            )
  "Return the u8 list representing a signature header containing the
HEADER-SHA256 (a string) and the PAYLOAD-SIZE, which is the combined size of
the header and compressed payload."
  (define size-tag (if (> header+compressed-payload-size INT32_MAX)
                       RPMSIGTAG_LONGSIZE
                       RPMSIGTAG_SIZE))
  (wrap-in-region-tags
   (make-header (list (make-header-entry RPMSIGTAG_SHA256 1 header-sha256)
                      (make-header-entry size-tag 1
                                         header+compressed-payload-size)
                      ;; (make-header-entry RPMSIGTAG_PAYLOADSIZE 1
                      ;;                    uncompressed-payload-size)
                      ;; Reserve 32 bytes of extra space in case users would
                      ;; like to add signatures, as done in rpmGenerateSignature.
                      (make-header-entry RPMSIGTAG_RESERVEDSPACE 32
                                         (make-list 32 0))))
   RPMTAG_HEADERSIGNATURES))

(define (assemble-rpm-metadata lead signature header)
  "Align and append the various u8 list components together, and return the
result as a bytevector."
  (let* ((offset (+ (length lead) (length signature)))
         (header-offset (next-aligned-offset offset 8))
         (padding (make-list (- header-offset offset) 0)))
    ;; The Header is 8-bytes aligned.
    (u8-list->bytevector (append lead signature padding header))))
