;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012-2025 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix remote-procedures)
  #:use-module (guix serialization)
  #:export (%protocol-version
            %worker-magic-1
            %worker-magic-2
            protocol-major
            protocol-minor
            protocol-version

            visit-remote-procedures
            id:
            returns:
            remote-procedure-id
            stream-argument

            hash-algo
            build-mode
            gc-action))

;;; Comment:
;;;
;;; This module defines the remote procedure interface between the build
;;; daemon and clients (also known as "remote procedure calls" or RPCs).  It
;;; is used to generate client stubs in (guix store) and can also be used to
;;; generate server stubs.
;;;
;;; Code:

(define %protocol-version
  ;; Version of the currently-implemented protocol.
  #x164)

;; Values sent by the client and then the server upon handshake.
(define %worker-magic-1 #x6e697863)               ; "nixc"
(define %worker-magic-2 #x6478696f)               ; "dxio"

(define (protocol-major magic)
  "Extract from MAGIC, an integer, the protocol major version."
  (logand magic #xff00))

(define (protocol-minor magic)
  "Extract from MAGIC, an integer, the protocol minor version."
  (logand magic #x00ff))

(define (protocol-version major minor)
  "Return an integer representing protocol version MAJOR and MINOR."
  (logior major minor))

(define-syntax define-remote-procedures
  (syntax-rules (define)
    ((_ walk definition ...)
     (define-syntax-rule (walk process)
       (process definition ...)))))

;; Syntactic keywords.
(define-syntax id:
  (lambda (s) #`(syntax-error "invalid use of 'id:' keyword" #,s)))
(define-syntax returns:
  (lambda (s) #`(syntax-error "invalid use of 'returns:' keyword" #,s)))

;; What follows is in essence an instance of the interface definition language
;; (IDL) for client/daemon messaging.
(define-remote-procedures visit-remote-procedures

  (define (valid-path? (utf8-string path))
    "Return #t when PATH designates a valid store item and #f otherwise (an
invalid item may exist on disk but still be invalid, for instance because it
is the result of an aborted or failed build.)

An exception is raised if PATH is not prefixed by the store
directory (/gnu/store)."
    id: 1
    returns: boolean)

  (define (has-substitutes? (store-path path))    ;deprecated
    "Return #t if binary substitutes are available for PATH, and #f
otherwise."
    id: 3
    returns: boolean)

  (define (query-path-hash (store-path path))
    "Return the SHA256 hash of the nar serialization of PATH as a bytevector."
    id: 4
    returns: base16)

  (define (query-references (store-path path))
    "Return the list of references of PATH."
    id: 5
    returns: store-path-list)

  (define (query-referrers (store-path path))
    "Return the list of store items that refer to PATH."
    id: 6
    returns: store-path-list)

  (define (add-to-store (utf8-string basename)
                        (boolean obsolete)        ;obsolete, must be #t
                        (boolean recursive?)
                        (utf8-string hash-algo)
                        (file file-name))
    "Add the contents of FILE-NAME under BASENAME to the store.  When
RECURSIVE? is false, FILE-NAME must designate a regular file--not a directory
nor a symlink.  When RECURSIVE? is true and FILE-NAME designates a directory,
the contents of FILE-NAME are added recursively; if FILE-NAME designates a
flat file and RECURSIVE? is true, its contents are added, and its permission
bits are kept.  HASH-ALGO must be a string such as \"sha256\".

When RECURSIVE? is true, call (SELECT?  FILE STAT) for each directory entry,
where FILE is the entry's absolute file name and STAT is the result of
'lstat'; exclude entries for which SELECT? does not return true.

Return the store file name of the newly added file."
    id: 7
    returns: store-path)

  (define (add-text-to-store (utf8-string name) (bytevector text)
                             (string-list references))
    "Add BYTES under file NAME in the store, and return its store path.
REFERENCES is the list of store paths referred to by the resulting store
path."
    id: 8
    returns: store-path)

  (define (build-things (string-list things)
                        (integer mode))
    "Build THINGS, a list of store items which may be either '.drv' files or
outputs, and return when the worker is done building them.  Elements of THINGS
that are not derivations can only be substituted and not built locally.
Alternatively, an element of THING can be a derivation/output name pair, in
which case the daemon will attempt to substitute just the requested output of
the derivation.  Return #t on success."
    id: 9
    returns: boolean)

  (define (ensure-path (store-path path))
    "Ensure that the given path is valid; if it is not valid, it may be made valid
by running a substitute.  Return #t on success and raise an exception on
failure.

As a GC root is not created by the daemon, you may want to call
'add-temp-root' on that store path."
    id: 10
    returns: boolean)

  (define (add-temp-root (store-path path))
    "Make PATH a temporary root for the duration of the current session.
Return #t."
    id: 11
    returns: boolean)

  (define (add-indirect-root (utf8-string file-name))
    "Make the symlink FILE-NAME an indirect root for the garbage collector:
whatever store item FILE-NAME points to will not be collected.  Return #t on
success.

FILE-NAME can be anywhere on the file system, but it must be an absolute file
name--it is the caller's responsibility to ensure that it is an absolute file
name."
    id: 12
    returns: boolean)

  (define (find-roots)
    "Return a list of root/target pairs: for each pair, the first element is the
GC root file name and the second element is its target in the store.

When talking to a local daemon, this operation is equivalent to the 'gc-roots'
procedure in (guix store roots), except that the 'find-roots' excludes
potential roots that do not point to store items."
    id: 14
    returns: string-pairs)

  (define (export-path (store-path item)
                       (boolean sign?)
                       (stream port))
    "Export PATH to PORT; when SIGN? is true, sign it.  Return #t."
    id: 16
    returns: boolean)

  (define (set-options (boolean keep-failed?) (boolean keep-going?)
                       (boolean fallback?) (integer verbosity)
                       (boolean offload?)
                       (integer build-verbosity) (integer log-type)
                       (boolean print-build-trace)
                       (boolean use-substitutes?)
                       (string-pairs settings))
    "Set daemon options for this session; return nothing."
    id: 19
    returns:)                                     ;returns nothing

  (define (collect-garbage (integer action)
                           (store-path-list to-delete)
                           (boolean obsolete1)
                           (long-long min-freed)
                           (integer obsolete2)
                           (integer obsolete3)
                           (integer obsolete4))
    "Invoke the garbage collector to perform ACTION, a 'gc-action' value."
    id: 20
    returns: store-path-list long-long long-long)

  (define (query-derivation-outputs ;avoid name clash with `derivation-outputs'
           (store-path path))
    "Return the list of outputs of PATH, a .drv file."
    id: 22
    returns: store-path-list)

  ;; missing: query-all-valid-paths id: 23

  (define (query-failed-paths)
    "Return the list of store items for which a build failure is cached.

The result is always the empty list unless the daemon was started with
'--cache-failures'."
    id: 24
    returns: store-path-list)

  (define (clear-failed-paths (store-path-list items))
    "Remove ITEMS from the list of cached build failures and return #t.

This makes sense only when the daemon was started with '--cache-failures'."
    id: 25
    returns: boolean)

  (define (query-path-info (store-path path))
    "Return the info (hash, references, etc.) for PATH."
    id: 26
    returns: path-info)

  (define (import-paths (stream port))
    "Import the set of store paths read from PORT into SERVER's store.  An error
is raised if the set of paths read from PORT is not signed (as per
'export-path #:sign? #t'.)  Return the list of store paths imported."
    id: 27
    returns: store-path-list)

  (define (query-path-from-hash-part (utf8-string hash))
    "Return the store path whose hash part is HASH-PART (a nix-base32
string).  Return the empty string if no such path exists."
    ;; This RPC is primarily used by Hydra to reply to HTTP GETs of
    ;; /HASH.narinfo.
    id: 29
    returns: store-path)

  (define (query-substitutable-paths (store-path-list paths))
    "Return the subset of PATHS that is substitutable."
    id: 32
    returns: store-path-list)

  (define (query-valid-derivers (store-path path))
    "Return the list of valid \"derivers\" of PATH---i.e., all the
.drv present in the store that have PATH among their outputs."
    id: 33
    returns: store-path-list)

  (define (query-substitutable-path-infos (store-path-list paths))
    "Return information about the subset of PATHS that is
substitutable.  For each substitutable path, a `substitutable?' object is
returned; thus, the resulting list can be shorter than PATHS.  Furthermore,
that there is no guarantee that the order of the resulting list matches the
order of PATHS."
    id: 30
    returns: substitutable-path-list)

  (define (optimize-store)
    "Optimize the store by hard-linking identical files (\"deduplication\".)
Return #t on success."
    id: 34
    returns: boolean)

  (define (verify-store (boolean check-contents?)
                        (boolean repair?))
    "Verify the store.  Return #t if errors were encountered, false otherwise."
    id: 35
    returns: boolean)

  ;; Guix-specific RPCs.

  (define (built-in-builders)
    "Return the built-in builders."
    id: 80
    returns: string-list)

  (define (substitute-urls)
    "Return the list of substitute URLs."
    id: 81
    returns: string-list))

(define-syntax remote-procedure-id
  (syntax-rules ()
    "Return the numeric identifier of the remote procedure with the given name."
    ((_ name)
     (letrec-syntax ((extract-id
                      (lambda (s)
                        (syntax-case s (define id: returns:)
                          ((_)
                           #`(syntax-error "remote procedure not found"
                                           #,s))
                          ((_ (define (procedure formals (... ...))
                                doc
                                id: id
                                returns: _ (... ...))
                              rest (... ...))
                           (eq? (syntax->datum #'procedure)
                                (syntax->datum #'name))
                           #'id)
                          ((_ (define _ (... ...))
                              rest (... ...))
                           #'(extract-id rest (... ...)))))))
       (visit-remote-procedures extract-id)))))

(define-syntax stream-argument
  (syntax-rules (stream)
    "Extract the stream from the given formal parameter list, or expand to #f
if there is none."
    ((_ (stream arg) rest ...)
     arg)
    ((_ (type arg) rest ...)
     (stream-argument rest ...))
    ((_)
     #f)))

(define-syntax define-enumerate-type
  (syntax-rules ()
    ((_ name->int (name id) ...)
     (define-syntax name->int
       (syntax-rules (name ...)
         ((_ name) id) ...)))))

(define-enumerate-type hash-algo
  ;; hash.hh
  (md5 1)
  (sha1 2)
  (sha256 3))

(define-enumerate-type build-mode
  ;; store-api.hh
  (normal 0)
  (repair 1)
  (check 2))

(define-enumerate-type gc-action
  ;; store-api.hh
  (return-live 0)
  (return-dead 1)
  (delete-dead 2)
  (delete-specific 3))
