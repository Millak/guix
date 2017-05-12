;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014, 2015, 2016, 2017 Ludovic Courtès <ludo@gnu.org>
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

(define-module (test-store)
  #:use-module (guix tests)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (guix monads)
  #:use-module (guix hash)
  #:use-module (guix base32)
  #:use-module (guix packages)
  #:use-module (guix derivations)
  #:use-module (guix serialization)
  #:use-module (guix build utils)
  #:use-module (guix gexp)
  #:use-module (gnu packages)
  #:use-module (gnu packages bootstrap)
  #:use-module (ice-9 match)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:use-module (web uri)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-64))

;; Test the (guix store) module.

(define %store
  (open-connection-for-tests))


(test-begin "store")

(test-assert "open-connection with file:// URI"
  (let ((store (open-connection (string-append "file://"
                                               (%daemon-socket-uri)))))
    (and (add-text-to-store store "foo" "bar")
         (begin
           (close-connection store)
           #t))))

(test-equal "connection handshake error"
  EPROTO
  (let ((port (%make-void-port "rw")))
    (guard (c ((nix-connection-error? c)
               (and (eq? port (nix-connection-error-file c))
                    (nix-connection-error-code c))))
      (open-connection #f #:port port)
      'broken)))

(test-equal "store-path-hash-part"
  "283gqy39v3g9dxjy26rynl0zls82fmcg"
  (store-path-hash-part
   (string-append (%store-prefix)
                  "/283gqy39v3g9dxjy26rynl0zls82fmcg-guile-2.0.7")))

(test-equal "store-path-hash-part #f"
  #f
  (store-path-hash-part
   (string-append (%store-prefix)
                  "/foo/bar/283gqy39v3g9dxjy26rynl0zls82fmcg-guile-2.0.7")))

(test-equal "store-path-package-name"
  "guile-2.0.7"
  (store-path-package-name
   (string-append (%store-prefix)
                  "/283gqy39v3g9dxjy26rynl0zls82fmcg-guile-2.0.7")))

(test-equal "store-path-package-name #f"
  #f
  (store-path-package-name
   "/foo/bar/283gqy39v3g9dxjy26rynl0zls82fmcg-guile-2.0.7"))

(test-assert "direct-store-path?"
  (and (direct-store-path?
        (string-append (%store-prefix)
                       "/283gqy39v3g9dxjy26rynl0zls82fmcg-guile-2.0.7"))
       (not (direct-store-path?
             (string-append
              (%store-prefix)
              "/283gqy39v3g9dxjy26rynl0zls82fmcg-guile-2.0.7/bin/guile")))
       (not (direct-store-path? (%store-prefix)))))

(test-skip (if %store 0 13))

(test-equal "add-data-to-store"
  #vu8(1 2 3 4 5)
  (call-with-input-file (add-data-to-store %store "data" #vu8(1 2 3 4 5))
    get-bytevector-all))

(test-assert "valid-path? live"
  (let ((p (add-text-to-store %store "hello" "hello, world")))
    (valid-path? %store p)))

(test-assert "valid-path? false"
  (not (valid-path? %store
                    (string-append (%store-prefix) "/"
                                   (make-string 32 #\e) "-foobar"))))

(test-assert "valid-path? error"
  (with-store s
    (guard (c ((nix-protocol-error? c) #t))
      (valid-path? s "foo")
      #f)))

(test-assert "valid-path? recovery"
  ;; Prior to Nix commit 51800e0 (18 Mar. 2014), the daemon would immediately
  ;; close the connection after receiving a 'valid-path?' RPC with a non-store
  ;; file name.  See
  ;; <http://article.gmane.org/gmane.linux.distributions.nixos/12411> for
  ;; details.
  (with-store s
    (let-syntax ((true-if-error (syntax-rules ()
                                  ((_ exp)
                                   (guard (c ((nix-protocol-error? c) #t))
                                     exp #f)))))
      (and (true-if-error (valid-path? s "foo"))
           (true-if-error (valid-path? s "bar"))
           (true-if-error (valid-path? s "baz"))
           (true-if-error (valid-path? s "chbouib"))
           (valid-path? s (add-text-to-store s "valid" "yeah"))))))

(test-assert "hash-part->path"
  (let ((p (add-text-to-store %store "hello" "hello, world")))
    (equal? (hash-part->path %store (store-path-hash-part p))
            p)))

(test-assert "dead-paths"
  (let ((p (add-text-to-store %store "random-text" (random-text))))
    (->bool (member p (dead-paths %store)))))

;; FIXME: Find a test for `live-paths'.
;;
;; (test-assert "temporary root is in live-paths"
;;   (let* ((p1 (add-text-to-store %store "random-text"
;;                                 (random-text) '()))
;;          (b  (add-text-to-store %store "link-builder"
;;                                 (format #f "echo ~a > $out" p1)
;;                                 '()))
;;          (d1 (derivation %store "link"
;;                          "/bin/sh" `("-e" ,b)
;;                          #:inputs `((,b) (,p1))))
;;          (p2 (derivation->output-path d1)))
;;     (and (add-temp-root %store p2)
;;          (build-derivations %store (list d1))
;;          (valid-path? %store p1)
;;          (member (pk p2) (live-paths %store)))))

(test-assert "permanent root"
  (let* ((p  (with-store store
               (let ((p (add-text-to-store store "random-text"
                                           (random-text))))
                 (add-permanent-root p)
                 (add-permanent-root p)           ; should not throw
                 p))))
    (and (member p (live-paths %store))
         (begin
           (remove-permanent-root p)
           (->bool (member p (dead-paths %store)))))))

(test-assert "dead path can be explicitly collected"
  (let ((p (add-text-to-store %store "random-text"
                              (random-text) '())))
    (let-values (((paths freed) (delete-paths %store (list p))))
      (and (equal? paths (list p))
           (> freed 0)
           (not (file-exists? p))))))

(test-assert "add-text-to-store vs. delete-paths"
  ;; Before, 'add-text-to-store' would return PATH2 without noticing that it
  ;; is no longer valid.
  (with-store store
    (let* ((text    (random-text))
           (path    (add-text-to-store store "delete-me" text))
           (deleted (delete-paths store (list path)))
           (path2   (add-text-to-store store "delete-me" text)))
      (and (string=? path path2)
           (equal? deleted (list path))
           (valid-path? store path)
           (file-exists? path)))))

(test-assert "add-to-store vs. delete-paths"
  ;; Same as above.
  (with-store store
    (let* ((file    (search-path %load-path "guix.scm"))
           (path    (add-to-store store "delete-me" #t "sha256" file))
           (deleted (delete-paths store (list path)))
           (path2   (add-to-store store "delete-me" #t "sha256" file)))
      (and (string=? path path2)
           (equal? deleted (list path))
           (valid-path? store path)
           (file-exists? path)))))

(test-assert "references"
  (let* ((t1 (add-text-to-store %store "random1"
                                (random-text)))
         (t2 (add-text-to-store %store "random2"
                                (random-text) (list t1))))
    (and (equal? (list t1) (references %store t2))
         (equal? (list t2) (referrers %store t1))
         (null? (references %store t1))
         (null? (referrers %store t2)))))

(test-assert "references/substitutes missing reference info"
  (with-store s
    (set-build-options s #:use-substitutes? #f)
    (guard (c ((nix-protocol-error? c) #t))
      (let* ((b  (add-to-store s "bash" #t "sha256"
                               (search-bootstrap-binary "bash"
                                                        (%current-system))))
             (d  (derivation s "the-thing" b '("--help")
                             #:inputs `((,b)))))
        (references/substitutes s (list (derivation->output-path d) b))
        #f))))

(test-assert "references/substitutes with substitute info"
  (with-store s
    (set-build-options s #:use-substitutes? #t)
    (let* ((t1 (add-text-to-store s "random1" (random-text)))
           (t2 (add-text-to-store s "random2" (random-text)
                                  (list t1)))
           (t3 (add-text-to-store s "build" "echo -n $t2 > $out"))
           (b  (add-to-store s "bash" #t "sha256"
                             (search-bootstrap-binary "bash"
                                                      (%current-system))))
           (d  (derivation s "the-thing" b `("-e" ,t3)
                           #:inputs `((,b) (,t3) (,t2))
                           #:env-vars `(("t2" . ,t2))))
           (o  (derivation->output-path d)))
      (with-derivation-narinfo d
        (sha256 => (sha256 (string->utf8 t2)))
        (references => (list t2))

        (equal? (references/substitutes s (list o t3 t2 t1))
                `((,t2)                           ;refs of O
                  ()                              ;refs of T3
                  (,t1)                           ;refs of T2
                  ()))))))                        ;refs of T1

(test-equal "substitutable-path-info when substitutes are turned off"
  '()
  (with-store s
    (set-build-options s #:use-substitutes? #f)
    (let* ((b  (add-to-store s "bash" #t "sha256"
                             (search-bootstrap-binary "bash"
                                                      (%current-system))))
           (d  (derivation s "the-thing" b '("--version")
                           #:inputs `((,b))))
           (o  (derivation->output-path d)))
      (with-derivation-narinfo d
        (substitutable-path-info s (list o))))))

(test-equal "substitutable-paths when substitutes are turned off"
  '()
  (with-store s
    (set-build-options s #:use-substitutes? #f)
    (let* ((b  (add-to-store s "bash" #t "sha256"
                             (search-bootstrap-binary "bash"
                                                      (%current-system))))
           (d  (derivation s "the-thing" b '("--version")
                           #:inputs `((,b))))
           (o  (derivation->output-path d)))
      (with-derivation-narinfo d
        (substitutable-paths s (list o))))))

(test-assert "requisites"
  (let* ((t1 (add-text-to-store %store "random1"
                                (random-text) '()))
         (t2 (add-text-to-store %store "random2"
                                (random-text) (list t1)))
         (t3 (add-text-to-store %store "random3"
                                (random-text) (list t2)))
         (t4 (add-text-to-store %store "random4"
                                (random-text) (list t1 t3))))
    (define (same? x y)
      (and (= (length x) (length y))
           (lset= equal? x y)))

    (and (same? (requisites %store (list t1)) (list t1))
         (same? (requisites %store (list t2)) (list t1 t2))
         (same? (requisites %store (list t3)) (list t1 t2 t3))
         (same? (requisites %store (list t4)) (list t1 t2 t3 t4))
         (same? (requisites %store (list t1 t2 t3 t4))
                (list t1 t2 t3 t4)))))

(test-assert "derivers"
  (let* ((b (add-text-to-store %store "build" "echo $foo > $out" '()))
         (s (add-to-store %store "bash" #t "sha256"
                          (search-bootstrap-binary "bash"
                                                   (%current-system))))
         (d (derivation %store "the-thing"
                        s `("-e" ,b)
                        #:env-vars `(("foo" . ,(random-text)))
                        #:inputs `((,b) (,s))))
         (o (derivation->output-path d)))
    (and (build-derivations %store (list d))
         (equal? (query-derivation-outputs %store (derivation-file-name d))
                 (list o))
         (equal? (valid-derivers %store o)
                 (list (derivation-file-name d))))))

(test-assert "topologically-sorted, one item"
  (let* ((a (add-text-to-store %store "a" "a"))
         (b (add-text-to-store %store "b" "b" (list a)))
         (c (add-text-to-store %store "c" "c" (list b)))
         (d (add-text-to-store %store "d" "d" (list c)))
         (s (topologically-sorted %store (list d))))
    (equal? s (list a b c d))))

(test-assert "topologically-sorted, several items"
  (let* ((a  (add-text-to-store %store "a" "a"))
         (b  (add-text-to-store %store "b" "b" (list a)))
         (c  (add-text-to-store %store "c" "c" (list b)))
         (d  (add-text-to-store %store "d" "d" (list c)))
         (s1 (topologically-sorted %store (list d a c b)))
         (s2 (topologically-sorted %store (list b d c a b d))))
    (equal? s1 s2 (list a b c d))))

(test-assert "topologically-sorted, more difficult"
  (let* ((a  (add-text-to-store %store "a" "a"))
         (b  (add-text-to-store %store "b" "b" (list a)))
         (c  (add-text-to-store %store "c" "c" (list b)))
         (d  (add-text-to-store %store "d" "d" (list c)))
         (w  (add-text-to-store %store "w" "w"))
         (x  (add-text-to-store %store "x" "x" (list w)))
         (y  (add-text-to-store %store "y" "y" (list x d)))
         (s1 (topologically-sorted %store (list y)))
         (s2 (topologically-sorted %store (list c y)))
         (s3 (topologically-sorted %store (cons y (references %store y)))))
    ;; The order in which 'references' returns the references of Y is
    ;; unspecified, so accommodate.
    (let* ((x-then-d? (equal? (references %store y) (list x d))))
      (and (equal? s1
                   (if x-then-d?
                       (list w x a b c d y)
                       (list a b c d w x y)))
           (equal? s2
                   (if x-then-d?
                       (list a b c w x d y)
                       (list a b c d w x y)))
           (lset= string=? s1 s3)))))

(test-assert "current-build-output-port, UTF-8"
  ;; Are UTF-8 strings in the build log properly interpreted?
  (string-contains
   (with-fluids ((%default-port-encoding "UTF-8")) ;for the string port
     (call-with-output-string
      (lambda (port)
        (parameterize ((current-build-output-port port))
          (let* ((s "Here’s a Greek letter: λ.")
                 (d (build-expression->derivation
                     %store "foo" `(display ,s)
                     #:guile-for-build
                     (package-derivation s %bootstrap-guile (%current-system)))))
            (guard (c ((nix-protocol-error? c) #t))
              (build-derivations %store (list d))))))))
   "Here’s a Greek letter: λ."))

(test-assert "current-build-output-port, UTF-8 + garbage"
  ;; What about a mixture of UTF-8 + garbage?
  (string-contains
   (with-fluids ((%default-port-encoding "UTF-8")) ;for the string port
     (call-with-output-string
      (lambda (port)
        (parameterize ((current-build-output-port port))
          (let ((d (build-expression->derivation
                    %store "foo"
                    `(begin
                       (use-modules (rnrs io ports))
                       (display "garbage: ")
                       (put-bytevector (current-output-port) #vu8(128))
                       (display "lambda: λ\n"))
                     #:guile-for-build
                     (package-derivation %store %bootstrap-guile))))
            (guard (c ((nix-protocol-error? c) #t))
              (build-derivations %store (list d))))))))
   (cond-expand
     (guile-2.2 "garbage: �lambda: λ")
     (else      "garbage: ?lambda: λ"))))

(test-assert "log-file, derivation"
  (let* ((b (add-text-to-store %store "build" "echo $foo > $out" '()))
         (s (add-to-store %store "bash" #t "sha256"
                          (search-bootstrap-binary "bash"
                                                   (%current-system))))
         (d (derivation %store "the-thing"
                        s `("-e" ,b)
                        #:env-vars `(("foo" . ,(random-text)))
                        #:inputs `((,b) (,s)))))
    (and (build-derivations %store (list d))
         (file-exists? (pk (log-file %store (derivation-file-name d)))))))

(test-assert "log-file, output file name"
  (let* ((b (add-text-to-store %store "build" "echo $foo > $out" '()))
         (s (add-to-store %store "bash" #t "sha256"
                          (search-bootstrap-binary "bash"
                                                   (%current-system))))
         (d (derivation %store "the-thing"
                        s `("-e" ,b)
                        #:env-vars `(("foo" . ,(random-text)))
                        #:inputs `((,b) (,s))))
         (o (derivation->output-path d)))
    (and (build-derivations %store (list d))
         (file-exists? (pk (log-file %store o)))
         (string=? (log-file %store (derivation-file-name d))
                   (log-file %store o)))))

(test-assert "no substitutes"
  (with-store s
    (let* ((d1 (package-derivation s %bootstrap-guile (%current-system)))
           (d2 (package-derivation s %bootstrap-glibc (%current-system)))
           (o  (map derivation->output-path (list d1 d2))))
      (set-build-options s #:use-substitutes? #f)
      (and (not (has-substitutes? s (derivation-file-name d1)))
           (not (has-substitutes? s (derivation-file-name d2)))
           (null? (substitutable-paths s o))
           (null? (substitutable-path-info s o))))))

(test-assert "build-things with output path"
  (with-store s
    (let* ((c   (random-text))                    ;contents of the output
           (d   (build-expression->derivation
                 s "substitute-me"
                 `(call-with-output-file %output
                    (lambda (p)
                      (display ,c p)))
                 #:guile-for-build
                 (package-derivation s %bootstrap-guile (%current-system))))
           (o   (derivation->output-path d)))
      (set-build-options s #:use-substitutes? #f)

      ;; Pass 'build-things' the output file name, O.  However, since there
      ;; are no substitutes for O, it will just do nothing.
      (build-things s (list o))
      (not (valid-path? s o)))))

(test-skip (if (getenv "GUIX_BINARY_SUBSTITUTE_URL") 0 1))

(test-assert "substitute query"
  (with-store s
    (let* ((d (package-derivation s %bootstrap-guile (%current-system)))
           (o (derivation->output-path d)))
      ;; Create fake substituter data, to be read by 'guix substitute'.
      (with-derivation-narinfo d
        ;; Remove entry from the local cache.
        (false-if-exception
         (delete-file-recursively (string-append (getenv "XDG_CACHE_HOME")
                                                 "/guix/substitute")))

        ;; Make sure 'guix substitute' correctly communicates the above
        ;; data.
        (set-build-options s #:use-substitutes? #t
                           #:substitute-urls (%test-substitute-urls))
        (and (has-substitutes? s o)
             (equal? (list o) (substitutable-paths s (list o)))
             (match (pk 'spi (substitutable-path-info s (list o)))
               (((? substitutable? s))
                (and (string=? (substitutable-deriver s)
                               (derivation-file-name d))
                     (null? (substitutable-references s))
                     (equal? (substitutable-nar-size s) 1234)))))))))

(test-assert "substitute query, alternating URLs"
  (let* ((d (with-store s
              (package-derivation s %bootstrap-guile (%current-system))))
         (o (derivation->output-path d)))
    (with-derivation-narinfo d
      ;; Remove entry from the local cache.
      (false-if-exception
       (delete-file-recursively (string-append (getenv "XDG_CACHE_HOME")
                                               "/guix/substitute")))

      ;; Note: We reconnect to the daemon to force a new instance of 'guix
      ;; substitute' to be used; otherwise the #:substitute-urls of
      ;; 'set-build-options' would have no effect.

      (and (with-store s                        ;the right substitute URL
             (set-build-options s #:use-substitutes? #t
                                #:substitute-urls (%test-substitute-urls))
             (has-substitutes? s o))
           (with-store s                        ;the wrong one
             (set-build-options s #:use-substitutes? #t
                                #:substitute-urls (list
                                                   "http://does-not-exist"))
             (not (has-substitutes? s o)))
           (with-store s                        ;the right one again
             (set-build-options s #:use-substitutes? #t
                                #:substitute-urls (%test-substitute-urls))
             (has-substitutes? s o))
           (with-store s                        ;empty list of URLs
             (set-build-options s #:use-substitutes? #t
                                #:substitute-urls '())
             (not (has-substitutes? s o)))))))

(test-assert "substitute"
  (with-store s
    (let* ((c   (random-text))                     ; contents of the output
           (d   (build-expression->derivation
                 s "substitute-me"
                 `(call-with-output-file %output
                    (lambda (p)
                      (exit 1)                     ; would actually fail
                      (display ,c p)))
                 #:guile-for-build
                 (package-derivation s %bootstrap-guile (%current-system))))
           (o   (derivation->output-path d)))
      (with-derivation-substitute d c
        (set-build-options s #:use-substitutes? #t
                           #:substitute-urls (%test-substitute-urls))
        (and (has-substitutes? s o)
             (build-derivations s (list d))
             (equal? c (call-with-input-file o get-string-all)))))))

(test-assert "substitute + build-things with output path"
  (with-store s
    (let* ((c   (random-text))                    ;contents of the output
           (d   (build-expression->derivation
                 s "substitute-me"
                 `(call-with-output-file %output
                    (lambda (p)
                      (exit 1)                    ;would actually fail
                      (display ,c p)))
                 #:guile-for-build
                 (package-derivation s %bootstrap-guile (%current-system))))
           (o   (derivation->output-path d)))
      (with-derivation-substitute d c
        (set-build-options s #:use-substitutes? #t
                           #:substitute-urls (%test-substitute-urls))
        (and (has-substitutes? s o)
             (build-things s (list o))            ;give the output path
             (valid-path? s o)
             (equal? c (call-with-input-file o get-string-all)))))))

(test-assert "substitute, corrupt output hash"
  ;; Tweak the substituter into installing a substitute whose hash doesn't
  ;; match the one announced in the narinfo.  The daemon must notice this and
  ;; raise an error.
  (with-store s
    (let* ((c   "hello, world")                    ; contents of the output
           (d   (build-expression->derivation
                 s "corrupt-substitute"
                 `(mkdir %output)
                 #:guile-for-build
                 (package-derivation s %bootstrap-guile (%current-system))))
           (o   (derivation->output-path d)))
      (with-derivation-substitute d c
        (sha256 => (make-bytevector 32 0)) ;select a hash that doesn't match C

        ;; Make sure we use 'guix substitute'.
        (set-build-options s
                           #:use-substitutes? #t
                           #:fallback? #f
                           #:substitute-urls (%test-substitute-urls))
        (and (has-substitutes? s o)
             (guard (c ((nix-protocol-error? c)
                        ;; XXX: the daemon writes "hash mismatch in downloaded
                        ;; path", but the actual error returned to the client
                        ;; doesn't mention that.
                        (pk 'corrupt c)
                        (not (zero? (nix-protocol-error-status c)))))
               (build-derivations s (list d))
               #f))))))

(test-assert "substitute --fallback"
  (with-store s
    (let* ((t   (random-text))                    ; contents of the output
           (d   (build-expression->derivation
                 s "substitute-me-not"
                 `(call-with-output-file %output
                    (lambda (p)
                      (display ,t p)))
                 #:guile-for-build
                 (package-derivation s %bootstrap-guile (%current-system))))
           (o   (derivation->output-path d)))
      ;; Create fake substituter data, to be read by 'guix substitute'.
      (with-derivation-narinfo d
        ;; Make sure we use 'guix substitute'.
        (set-build-options s #:use-substitutes? #t
                           #:substitute-urls (%test-substitute-urls))
        (and (has-substitutes? s o)
             (guard (c ((nix-protocol-error? c)
                        ;; The substituter failed as expected.  Now make
                        ;; sure that #:fallback? #t works correctly.
                        (set-build-options s
                                           #:use-substitutes? #t
                                           #:substitute-urls
                                             (%test-substitute-urls)
                                           #:fallback? #t)
                        (and (build-derivations s (list d))
                             (equal? t (call-with-input-file o
                                         get-string-all)))))
               ;; Should fail.
               (build-derivations s (list d))
               #f))))))

(test-assert "export/import several paths"
  (let* ((texts (unfold (cut >= <> 10)
                        (lambda _ (random-text))
                        1+
                        0))
         (files (map (cut add-text-to-store %store "text" <>) texts))
         (dump  (call-with-bytevector-output-port
                 (cut export-paths %store files <>))))
    (delete-paths %store files)
    (and (every (negate file-exists?) files)
         (let* ((source   (open-bytevector-input-port dump))
                (imported (import-paths %store source)))
           (and (equal? imported files)
                (every file-exists? files)
                (equal? texts
                        (map (lambda (file)
                               (call-with-input-file file
                                 get-string-all))
                             files)))))))

(test-assert "export/import paths, ensure topological order"
  (let* ((file0 (add-text-to-store %store "baz" (random-text)))
         (file1 (add-text-to-store %store "foo" (random-text)
                                   (list file0)))
         (file2 (add-text-to-store %store "bar" (random-text)
                                   (list file1)))
         (files (list file1 file2))
         (dump1 (call-with-bytevector-output-port
                 (cute export-paths %store (list file1 file2) <>)))
         (dump2 (call-with-bytevector-output-port
                 (cute export-paths %store (list file2 file1) <>))))
    (delete-paths %store files)
    (and (every (negate file-exists?) files)
         (bytevector=? dump1 dump2)
         (let* ((source   (open-bytevector-input-port dump1))
                (imported (import-paths %store source)))
           ;; DUMP1 should contain exactly FILE1 and FILE2, not FILE0.
           (and (equal? imported (list file1 file2))
                (every file-exists? files)
                (equal? (list file0) (references %store file1))
                (equal? (list file1) (references %store file2)))))))

(test-assert "export/import incomplete"
  (let* ((file0 (add-text-to-store %store "baz" (random-text)))
         (file1 (add-text-to-store %store "foo" (random-text)
                                   (list file0)))
         (file2 (add-text-to-store %store "bar" (random-text)
                                   (list file1)))
         (dump  (call-with-bytevector-output-port
                 (cute export-paths %store (list file2) <>))))
    (delete-paths %store (list file0 file1 file2))
    (guard (c ((nix-protocol-error? c)
               (and (not (zero? (nix-protocol-error-status c)))
                    (string-contains (nix-protocol-error-message c)
                                     "not valid"))))
      ;; Here we get an exception because DUMP does not include FILE0 and
      ;; FILE1, which are dependencies of FILE2.
      (import-paths %store (open-bytevector-input-port dump)))))

(test-assert "export/import recursive"
  (let* ((file0 (add-text-to-store %store "baz" (random-text)))
         (file1 (add-text-to-store %store "foo" (random-text)
                                   (list file0)))
         (file2 (add-text-to-store %store "bar" (random-text)
                                   (list file1)))
         (dump  (call-with-bytevector-output-port
                 (cute export-paths %store (list file2) <>
                       #:recursive? #t))))
    (delete-paths %store (list file0 file1 file2))
    (let ((imported (import-paths %store (open-bytevector-input-port dump))))
      (and (equal? imported (list file0 file1 file2))
           (every file-exists? (list file0 file1 file2))
           (equal? (list file0) (references %store file1))
           (equal? (list file1) (references %store file2))))))

(test-assert "write-file & export-path yield the same result"
  ;; Here we compare 'write-file' and the daemon's own implementation.
  ;; 'write-file' is the reference because we know it sorts file
  ;; deterministically.  Conversely, the daemon uses 'readdir' and the entries
  ;; currently happen to be sorted as a side-effect of some unrelated
  ;; operation (search for 'unhacked' in archive.cc.)  Make sure we detect any
  ;; changes there.
  (run-with-store %store
    (mlet* %store-monad ((drv1 (package->derivation %bootstrap-guile))
                         (out1 -> (derivation->output-path drv1))
                         (data -> (unfold (cut >= <> 26)
                                          (lambda (i)
                                            (random-bytevector 128))
                                          1+ 0))
                         (build
                          -> #~(begin
                                 (use-modules (rnrs io ports) (srfi srfi-1))
                                 (let ()
                                   (define letters
                                     (map (lambda (i)
                                            (string
                                             (integer->char
                                              (+ i (char->integer #\a)))))
                                          (iota 26)))
                                   (define (touch file data)
                                     (call-with-output-file file
                                       (lambda (port)
                                         (put-bytevector port data))))

                                   (mkdir #$output)
                                   (chdir #$output)

                                   ;; The files must be different so they have
                                   ;; different inode numbers, and the inode
                                   ;; order must differ from the lexicographic
                                   ;; order.
                                   (for-each touch
                                             (append (drop letters 10)
                                                     (take letters 10))
                                             (list #$@data))
                                   #t)))
                         (drv2 (gexp->derivation "bunch" build))
                         (out2 -> (derivation->output-path drv2))
                         (item-info -> (store-lift query-path-info)))
      (mbegin %store-monad
        (built-derivations (list drv1 drv2))
        (foldm %store-monad
               (lambda (item result)
                 (define ref-hash
                   (let-values (((port get) (open-sha256-port)))
                     (write-file item port)
                     (close-port port)
                     (get)))

                 ;; 'query-path-info' returns a hash produced by using the
                 ;; daemon's C++ 'dump' function, which is the implementation
                 ;; under test.
                 (>>= (item-info item)
                      (lambda (info)
                        (return
                         (and result
                              (bytevector=? (path-info-hash info) ref-hash))))))
               #t
               (list out1 out2))))
    #:guile-for-build (%guile-for-build)))

(test-assert "import corrupt path"
  (let* ((text (random-text))
         (file (add-text-to-store %store "text" text))
         (dump (call-with-bytevector-output-port
                (cut export-paths %store (list file) <>))))
    (delete-paths %store (list file))

    ;; Flip a bit in the stream's payload.  INDEX here falls in the middle of
    ;; the file contents in DUMP, regardless of the store prefix.
    (let* ((index #x70)
           (byte  (bytevector-u8-ref dump index)))
      (bytevector-u8-set! dump index (logxor #xff byte)))

    (and (not (file-exists? file))
         (guard (c ((nix-protocol-error? c)
                    (pk 'c c)
                    (and (not (zero? (nix-protocol-error-status c)))
                         (string-contains (nix-protocol-error-message c)
                                          "corrupt"))))
           (let* ((source   (open-bytevector-input-port dump))
                  (imported (import-paths %store source)))
             (pk 'corrupt-imported imported)
             #f)))))

(test-assert "register-path"
  (let ((file (string-append (%store-prefix) "/" (make-string 32 #\f)
                             "-fake")))
    (when (valid-path? %store file)
      (delete-paths %store (list file)))
    (false-if-exception (delete-file file))

    (let ((ref (add-text-to-store %store "ref-of-fake" (random-text)))
          (drv (string-append file ".drv")))
      (call-with-output-file file
        (cut display "This is a fake store item.\n" <>))
      (register-path file
                     #:references (list ref)
                     #:deriver drv)

      (and (valid-path? %store file)
           (equal? (references %store file) (list ref))
           (null? (valid-derivers %store file))
           (null? (referrers %store file))))))

(test-assert "verify-store"
  (let* ((text  (random-text))
         (file1 (add-text-to-store %store "foo" text))
         (file2 (add-text-to-store %store "bar" (random-text)
                                   (list file1))))
    (and (pk 'verify1 (verify-store %store))    ;hopefully OK ;
         (begin
           (delete-file file1)
           (not (pk 'verify2 (verify-store %store)))) ;bad! ;
         (begin
           ;; Using 'add-text-to-store' here wouldn't work: It would succeed ;
           ;; without actually creating the file. ;
           (call-with-output-file file1
             (lambda (port)
               (display text port)))
           (pk 'verify3 (verify-store %store)))))) ;OK again

(test-assert "verify-store + check-contents"
  ;; XXX: This test is I/O intensive.
  (with-store s
    (let* ((text (random-text))
           (drv  (build-expression->derivation
                  s "corrupt"
                  `(let ((out (assoc-ref %outputs "out")))
                     (call-with-output-file out
                       (lambda (port)
                         (display ,text port)))
                     #t)
                  #:guile-for-build
                  (package-derivation s %bootstrap-guile (%current-system))))
           (file (derivation->output-path drv)))
      (with-derivation-substitute drv text
        (and (build-derivations s (list drv))
             (verify-store s #:check-contents? #t) ;should be OK
             (begin
               (chmod file #o644)
               (call-with-output-file file
                 (lambda (port)
                   (display "corrupt!" port)))
               #t)

             ;; Make sure the corruption is detected.  We don't test repairing
             ;; because only "trusted" users are allowed to do it, but we
             ;; don't expose that notion of trusted users that nix-daemon
             ;; supports because it seems dubious and redundant with what the
             ;; OS provides (in Nix "trusted" users have additional
             ;; privileges, such as overriding the set of substitute URLs, but
             ;; we instead want to allow anyone to modify them, provided
             ;; substitutes are signed by a root-approved key.)
             (not (verify-store s #:check-contents? #t))

             ;; Delete the corrupt item to leave the store in a clean state.
             (delete-paths s (list file)))))))

(test-assert "build-things, check mode"
  (with-store store
    (call-with-temporary-output-file
     (lambda (entropy entropy-port)
       (write (random-text) entropy-port)
       (force-output entropy-port)
       (let* ((drv  (build-expression->derivation
                     store "non-deterministic"
                     `(begin
                        (use-modules (rnrs io ports))
                        (let ((out (assoc-ref %outputs "out")))
                          (call-with-output-file out
                            (lambda (port)
                              ;; Rely on the fact that tests do not use the
                              ;; chroot, and thus ENTROPY is readable.
                              (display (call-with-input-file ,entropy
                                         get-string-all)
                                       port)))
                          #t))
                     #:guile-for-build
                     (package-derivation store %bootstrap-guile (%current-system))))
              (file (derivation->output-path drv)))
         (and (build-things store (list (derivation-file-name drv)))
              (begin
                (write (random-text) entropy-port)
                (force-output entropy-port)
                (guard (c ((nix-protocol-error? c)
                           (pk 'determinism-exception c)
                           (and (not (zero? (nix-protocol-error-status c)))
                                (string-contains (nix-protocol-error-message c)
                                                 "deterministic"))))
                  ;; This one will produce a different result.  Since we're in
                  ;; 'check' mode, this must fail.
                  (build-things store (list (derivation-file-name drv))
                                (build-mode check))
                  #f))))))))

(test-assert "build multiple times"
  (with-store store
    ;; Ask to build twice.
    (set-build-options store #:rounds 2 #:use-substitutes? #f)

    (call-with-temporary-output-file
     (lambda (entropy entropy-port)
       (write (random-text) entropy-port)
       (force-output entropy-port)
       (let* ((drv  (build-expression->derivation
                     store "non-deterministic"
                     `(begin
                        (use-modules (rnrs io ports))
                        (let ((out (assoc-ref %outputs "out")))
                          (call-with-output-file out
                            (lambda (port)
                              ;; Rely on the fact that tests do not use the
                              ;; chroot, and thus ENTROPY is accessible.
                              (display (call-with-input-file ,entropy
                                         get-string-all)
                                       port)
                              (call-with-output-file ,entropy
                                (lambda (port)
                                  (write 'foobar port)))))
                          #t))
                     #:guile-for-build
                     (package-derivation store %bootstrap-guile (%current-system))))
              (file (derivation->output-path drv)))
         (guard (c ((nix-protocol-error? c)
                    (pk 'multiple-build c)
                    (and (not (zero? (nix-protocol-error-status c)))
                         (string-contains (nix-protocol-error-message c)
                                          "deterministic"))))
           ;; This one will produce a different result on the second run.
           (current-build-output-port (current-error-port))
           (build-things store (list (derivation-file-name drv)))
           #f))))))

(test-equal "store-lower"
  "Lowered."
  (let* ((add  (store-lower text-file))
         (file (add %store "foo" "Lowered.")))
    (call-with-input-file file get-string-all)))

(test-equal "current-system"
  "bar"
  (parameterize ((%current-system "frob"))
    (run-with-store %store
      (mbegin %store-monad
        (set-current-system "bar")
        (current-system))
      #:system "foo")))

(test-assert "query-path-info"
  (let* ((ref (add-text-to-store %store "ref" "foo"))
         (item (add-text-to-store %store "item" "bar" (list ref)))
         (info (query-path-info %store item)))
    (and (equal? (path-info-references info) (list ref))
         (equal? (path-info-hash info)
                 (sha256
                  (string->utf8
                   (call-with-output-string (cut write-file item <>))))))))

(test-assert "path-info-deriver"
  (let* ((b (add-text-to-store %store "build" "echo $foo > $out" '()))
         (s (add-to-store %store "bash" #t "sha256"
                          (search-bootstrap-binary "bash"
                                                   (%current-system))))
         (d (derivation %store "the-thing"
                        s `("-e" ,b)
                        #:env-vars `(("foo" . ,(random-text)))
                        #:inputs `((,b) (,s))))
         (o (derivation->output-path d)))
    (and (build-derivations %store (list d))
         (not (path-info-deriver (query-path-info %store b)))
         (string=? (derivation-file-name d)
                   (path-info-deriver (query-path-info %store o))))))

(test-equal "build-cores"
  (list 0 42)
  (with-store store
    (let* ((build  (add-text-to-store store "build.sh"
                                      "echo $NIX_BUILD_CORES > $out"))
           (bash   (add-to-store store "bash" #t "sha256"
                                 (search-bootstrap-binary "bash"
                                                          (%current-system))))
           (drv1   (derivation store "the-thing" bash
                               `("-e" ,build)
                               #:inputs `((,bash) (,build))
                               #:env-vars `(("x" . ,(random-text)))))
           (drv2   (derivation store "the-thing" bash
                               `("-e" ,build)
                               #:inputs `((,bash) (,build))
                               #:env-vars `(("x" . ,(random-text))))))
      (and (build-derivations store (list drv1))
           (begin
             (set-build-options store #:build-cores 42)
             (build-derivations store (list drv2)))
           (list (call-with-input-file (derivation->output-path drv1)
                   read)
                 (call-with-input-file (derivation->output-path drv2)
                   read))))))

(test-end "store")
