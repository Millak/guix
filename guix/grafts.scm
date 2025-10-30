;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014-2024 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2024 David Elsing <david.elsing@posteo.net>
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

(define-module (guix grafts)
  #:use-module (guix store)
  #:use-module (guix monads)
  #:use-module (guix records)
  #:use-module (guix combinators)
  #:use-module (guix derivations)
  #:use-module ((guix utils) #:select (%current-system target-hurd?))
  #:use-module (guix sets)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-71)
  #:use-module (ice-9 match)
  #:use-module (ice-9 vlist)
  #:export (graft?
            graft
            graft-origin
            graft-replacement
            graft-origin-output
            graft-replacement-output

            graft-derivation
            graft-derivation/shallow

            %graft-with-utf8-locale?)
  #:re-export (%graft?                            ;for backward compatibility
               without-grafting
               set-grafting
               grafting?))

(define-record-type* <graft> graft make-graft
  graft?
  (origin             graft-origin)               ;derivation | store item
  (origin-output      graft-origin-output         ;string | #f
                      (default "out"))
  (replacement        graft-replacement)    ;derivation | store item | monadic
  (replacement-output graft-replacement-output    ;string | #f
                      (default "out")))

(define (write-graft graft port)
  "Write a concise representation of GRAFT to PORT."
  (define (->string thing output)
    (if (derivation? thing)
        (derivation->output-path thing output)
        thing))

  (match graft
    (($ <graft> origin origin-output replacement replacement-output)
     (format port "#<graft ~a ==> ~a ~a>"
             (->string origin origin-output)
             (->string replacement replacement-output)
             (number->string (object-address graft) 16)))))

(set-record-type-printer! <graft> write-graft)

(define (graft-origin-file-name graft)
  "Return the output file name of the origin of GRAFT."
  (match graft
    (($ <graft> (? derivation? origin) output)
     (derivation->output-path origin output))
    (($ <graft> (? string? item))
     item)))

(define %graft-with-utf8-locale?
  ;; Whether to install a UTF-8 locale for grafting.  This parameter exists
  ;; for the sole purpose of being able to run tests without having to build
  ;; 'glibc-utf8-locales'.
  (make-parameter #t))

(define* (graft-derivation/shallow drv grafts
                                   #:key
                                   (name (derivation-name drv))
                                   (outputs (derivation-output-names drv))
                                   (guile (%guile-for-build))
                                   (system (%current-system)))
  "Return a derivation called NAME, which applies GRAFTS to the specified
OUTPUTS of DRV.  This procedure performs \"shallow\" grafting in that GRAFTS
are not recursively applied to dependencies of DRV."
  (define mapping
    ;; List of store item pairs.
    (map (lambda (graft)
           (gexp
            ((ungexp (with-parameters ((%graft? #f))
                       (graft-origin graft))
                     (graft-origin-output graft))
             . (ungexp (with-parameters ((%graft? #t))
                         (graft-replacement graft))
                       (graft-replacement-output graft)))))
         grafts))

  (define set-utf8-locale
    (and (%graft-with-utf8-locale?)
         ;; Let Guile interpret file names as UTF-8.
         #~(setlocale LC_ALL "C.UTF-8")))


  (define build
    (with-imported-modules '((guix build graft)
                             (guix build utils)
                             (guix build debug-link)
                             (guix elf))
      #~(begin
          (use-modules (guix build graft)
                       (guix build utils)
                       (ice-9 match))

          (define %outputs
            (ungexp (outputs->gexp outputs)))

          #+set-utf8-locale
          (let* ((old-outputs '(ungexp
                                (map (lambda (output)
                                       (gexp ((ungexp output)
                                              . (ungexp drv output))))
                                     outputs)))
                 (mapping (append '(ungexp mapping)
                                  (map (match-lambda
                                         ((name . file)
                                          (cons (assoc-ref old-outputs name)
                                                file)))
                                       %outputs))))
            (graft old-outputs %outputs mapping)))))


  (define properties
    `((type . graft)
      (graft (count . ,(length grafts)))))

  (gexp->derivation name build
                    #:system system
                    #:guile-for-build guile

                    ;; Grafts are computationally cheap so no
                    ;; need to offload or substitute.
                    #:local-build? #t
                    #:substitutable? #f

                    #:properties properties))

(define graft-derivation/shallow*
  (store-lower graft-derivation/shallow))

(define (non-self-references store drv outputs)
  "Return the list of references of the OUTPUTS of DRV, excluding self
references."
  (define (references* items)
    ;; Return the references of ITEMS.
    (guard (c ((store-protocol-error? c)
               ;; ITEMS are not in store so build INPUT first.
               (and (build-derivations store (list drv))
                    (append-map (cut references/cached store <>) items))))
      (append-map (cut references/cached store <>) items)))

  (let* ((self (map (cut derivation->output-path drv <>) outputs))
         (refs (references* self)))
    (remove (cut member <> self) refs)))

(define %graft-cache
  ;; Cache that maps derivation/outputs/grafts tuples to lists of grafts.
  (allocate-store-connection-cache 'grafts))

(define record-cache-lookup!
  (cache-lookup-recorder "derivation-graft-cache"
                         "Derivation graft cache"))

(define-syntax-rule (with-cache key exp ...)
  "Cache the value of monadic expression EXP under KEY."
  (mlet* %state-monad ((cache (current-state))
                       (result -> (vhash-assoc key cache)))
    (record-cache-lookup! result cache)
    (match result
      ((_ . result)                               ;cache hit
       (return result))
      (#f                                         ;cache miss
       (mlet %state-monad ((result (begin exp ...))
                           (cache  (current-state)))
         (mbegin %state-monad
           (set-current-state (vhash-cons key result cache))
           (return result)))))))

(define (reference-origins drv items)
  "Return the derivation/output pairs among DRV and its inputs, recursively,
that produce ITEMS.  Elements of ITEMS not produced by a derivation (i.e.,
it's a content-addressed \"source\"), or not produced by a dependency of DRV,
have no corresponding element in the resulting list."
  (define (lookup-derivers drv result items)
    ;; Return RESULT augmented by all the drv/output pairs producing one of
    ;; ITEMS, and ITEMS stripped of matching items.
    (fold2 (match-lambda*
             (((output . file) result items)
              (if (member file items)
                  (values (alist-cons drv output result)
                          (delete file items))
                  (values result items))))
           result items
           (derivation->output-paths drv)))

  ;; Perform a breadth-first traversal of the dependency graph of DRV in
  ;; search of the derivations that produce ITEMS.
  (let loop ((drv (list drv))
             (items items)
             (result '())
             (visited (setq)))
    (match drv
      (()
       result)
      ((drv . rest)
       (cond ((null? items)
              result)
             ((set-contains? visited drv)
              (loop rest items result visited))
             (else
              (let ((result items (lookup-derivers drv result items)))
                (loop (append rest
                              (map derivation-input-derivation
                                   (derivation-inputs drv)))
                      items result
                      (set-insert drv visited)))))))))

(define* (cumulative-grafts store drv grafts
                            #:key
                            (guile (%guile-for-build))
                            (system (%current-system)))
  "Augment GRAFTS with additional grafts resulting from the application of
GRAFTS to the dependencies of DRV.  Return the resulting list of grafts.

This is a monadic procedure in %STATE-MONAD where the state is a vhash mapping
derivations to the corresponding set of grafts."
  ;;; The derivation returned by this procedure pulls in all the outputs of DRV.
  ;;; This may be wasteful in cases where only one of them is actually used
  ;;; (as noted in <https://issues.guix.gnu.org/24886>), but it ensures that
  ;;; only one grafted variant of DRV ever exists.  That way, it is
  ;;; deterministic and avoids undesirable side effects such as run-time
  ;;; crashes (see <https://bugs.gnu.org/75157>).
  (let ((outputs (derivation-output-names drv)))
    (define (graft-origin? drv output graft)
      ;; Return true if DRV and OUTPUT correspond to the origin of GRAFT.
      (match graft
        (($ <graft> (? derivation? origin) origin-output)
         (and (string=? origin-output output)
              (match (assoc-ref (derivation->output-paths drv) output)
                ((? string? result)
                 (string=? result
                           (derivation->output-path origin output)))
                (_
                 #f))))
        (_
         #f)))

    (define (dependency-grafts items)
      (mapm %store-monad
            (lambda (drv+output)
              (match drv+output
                ((drv . output)
                 ;; If GRAFTS already contains a graft from DRV, do not
                 ;; override it.
                 (if (find (cut graft-origin? drv output <>) grafts)
                     (state-return grafts)
                     (cumulative-grafts store drv grafts
                                        #:guile guile
                                        #:system system)))))
            (reference-origins drv items)))

    (with-cache (list (derivation-file-name drv) outputs grafts)
      (match (non-self-references store drv outputs)
        (()                                         ;no dependencies
         (return grafts))
        (deps                                       ;one or more dependencies
         (mlet %state-monad ((grafts (dependency-grafts deps)))
           (let ((grafts (delete-duplicates (concatenate grafts) equal?)))
             (match (filter (lambda (graft)
                              (member (graft-origin-file-name graft) deps))
                            grafts)
               (()
                (return grafts))
               ((applicable ..1)
                ;; Use APPLICABLE, the subset of GRAFTS that is really
                ;; applicable to DRV, to avoid creating several identical
                ;; grafted variants of DRV.
                (let* ((new    (graft-derivation/shallow* store drv applicable
                                                          #:guile guile
                                                          #:system system))
                       (grafts (append (map (lambda (output)
                                              (graft
                                                (origin drv)
                                                (origin-output output)
                                                (replacement new)
                                                (replacement-output output)))
                                            outputs)
                                       grafts)))
                  (return grafts)))))))))))

(define* (graft-derivation store drv grafts
                           #:key
                           (guile (%guile-for-build))
                           (system (%current-system)))
  "Apply GRAFTS to the OUTPUTS of DRV and all their dependencies, recursively.
That is, if GRAFTS apply only indirectly to DRV, graft the dependencies of
DRV, and graft DRV itself to refer to those grafted dependencies."
  (let ((grafts cache
                (run-with-state
                    (cumulative-grafts store drv grafts
                                       #:guile guile #:system system)
                  (store-connection-cache store %graft-cache))))

    ;; Save CACHE in STORE to benefit from it on the next call.
    ;; XXX: Ideally we'd use %STORE-MONAD and 'mcached' and avoid mutating
    ;; STORE.
    (set-store-connection-cache! store %graft-cache cache)

    (match grafts
      ((first . rest)
       ;; If FIRST is not a graft for DRV, it means that GRAFTS are not
       ;; applicable to DRV and nothing needs to be done.
       (if (equal? drv (graft-origin first))
           (graft-replacement first)
           drv)))))

;; Local Variables:
;; eval: (put 'with-cache 'scheme-indent-function 1)
;; End:

;;; grafts.scm ends here
