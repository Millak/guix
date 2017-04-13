;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Andy Wingo <wingo@pobox.com>
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

(define-module (guix potluck environment))

;;; Commentary:
;;;
;;; This module's public interface forms a safe set of stable bindings
;;; available to Guix potluck package definition files.
;;;
;;; Code:

(define-syntax-rule (define-bindings module-name binding ...)
  (module-use! (module-public-interface (current-module))
               (resolve-interface 'module-name #:select '(binding ...))))

;; Core bindings.
(define-bindings (guile)
  and
  begin
  apply
  call-with-values
  values
  case
  case-lambda
  case-lambda*
  cond
  define
  define*
  define-values
  do
  if
  lambda
  lambda*
  let
  let*
  letrec
  letrec*
  or
  quasiquote
  quote
  ;; Can't allow mutation to globals.
  ;; set!
  unless
  unquote
  unquote-splicing
  when
  while
  λ)

;; Macro bindings.
(define-bindings (guile)
  ;; Although these have "current" in their name, they are lexically
  ;; scoped, not dynamically scoped.
  current-filename
  current-source-location
  ;; A subset of Guile's macro capabilities, for simplicity.
  define-syntax
  define-syntax-parameter
  define-syntax-rule
  identifier-syntax
  let-syntax
  letrec-syntax
  syntax-error
  syntax-rules)

;; Iteration bindings.
(define-bindings (guile)
  compose
  for-each
  identity
  iota
  map
  map-in-order
  const
  noop)

;; Unspecified bindings.
(define-bindings (guile)
  unspecified?
  *unspecified*)

;; Predicate bindings.
(define-bindings (guile)
  ->bool
  and-map
  and=>
  boolean?
  eq?
  equal?
  eqv?
  negate
  not
  or-map)

;; The current ports (current-input-port et al) are dynamically scoped,
;; which is a footgun from a sandboxing perspective.  It's too easy for
;; a procedure that is the result of a sandboxed evaluation to be later
;; invoked in a different context and thereby be implicitly granted
;; capabilities to whatever port is then current.  This is compounded by
;; the fact that most Scheme i/o primitives allow the port to be omitted
;; and thereby default to whatever's current.  For now, sadly, we avoid
;; exposing any i/o primitive to the sandbox.

;; Error bindings.
(define-bindings (guile)
  error
  throw
  with-throw-handler
  catch
  ;; false-if-exception can cause i/o if the #:warning arg is passed.
  ;; false-if-exception
  strerror
  scm-error)

;;  Sort bindings.
(define-bindings (guile)
  sort
  sorted?
  stable-sort
  sort-list)

;; Alist bindings.
(define-bindings (guile)
  acons
  assoc
  assoc-ref
  assq
  assq-ref
  assv
  assv-ref
  sloppy-assoc
  sloppy-assq
  sloppy-assv)

;; Number bindings.
(define-bindings (guile)
  *
  +
  -
  /
  1+
  1-
  <
  <=
  =
  >
  >=
  abs
  acos
  acosh
  angle
  asin
  asinh
  atan
  atanh
  ceiling
  ceiling-quotient
  ceiling-remainder
  ceiling/
  centered-quotient
  centered-remainder
  centered/
  complex?
  cos
  cosh
  denominator
  euclidean-quotient
  euclidean-remainder
  euclidean/
  even?
  exact->inexact
  exact-integer-sqrt
  exact-integer?
  exact?
  exp
  expt
  finite?
  floor
  floor-quotient
  floor-remainder
  floor/
  gcd
  imag-part
  inf
  inf?
  integer-expt
  integer-length
  integer?
  lcm
  log
  log10
  magnitude
  make-polar
  make-rectangular
  max
  min
  modulo
  modulo-expt
  most-negative-fixnum
  most-positive-fixnum
  nan
  nan?
  negative?
  numerator
  odd?
  positive?
  quotient
  rational?
  rationalize
  real-part
  real?
  remainder
  round
  round-quotient
  round-remainder
  round/
  sin
  sinh
  sqrt
  tan
  tanh
  truncate
  truncate-quotient
  truncate-remainder
  truncate/
  zero?
  number?
  number->string
  string->number)

;; Charset bindings.
(define-bindings (guile)
  ->char-set
  char-set
  char-set->list
  char-set->string
  char-set-adjoin
  char-set-any
  char-set-complement
  char-set-contains?
  char-set-copy
  char-set-count
  char-set-cursor
  char-set-cursor-next
  char-set-delete
  char-set-diff+intersection
  char-set-difference
  char-set-every
  char-set-filter
  char-set-fold
  char-set-for-each
  char-set-hash
  char-set-intersection
  char-set-map
  char-set-ref
  char-set-size
  char-set-unfold
  char-set-union
  char-set-xor
  char-set:ascii
  char-set:blank
  char-set:designated
  char-set:digit
  char-set:empty
  char-set:full
  char-set:graphic
  char-set:hex-digit
  char-set:iso-control
  char-set:letter
  char-set:letter+digit
  char-set:lower-case
  char-set:printing
  char-set:punctuation
  char-set:symbol
  char-set:title-case
  char-set:upper-case
  char-set:whitespace
  char-set<=
  char-set=
  char-set?
  end-of-char-set?
  list->char-set
  string->char-set
  ucs-range->char-set)

;; String bindings.
(define-bindings (guile)
  absolute-file-name?
  file-name-separator-string
  file-name-separator?
  in-vicinity
  basename
  dirname

  list->string
  make-string
  reverse-list->string
  string
  string->list
  string-any
  string-any-c-code
  string-append
  string-append/shared
  string-capitalize
  string-ci<
  string-ci<=
  string-ci<=?
  string-ci<>
  string-ci<?
  string-ci=
  string-ci=?
  string-ci>
  string-ci>=
  string-ci>=?
  string-ci>?
  string-compare
  string-compare-ci
  string-concatenate
  string-concatenate-reverse
  string-concatenate-reverse/shared
  string-concatenate/shared
  string-contains
  string-contains-ci
  string-copy
  string-count
  string-delete
  string-downcase
  string-drop
  string-drop-right
  string-every
  string-filter
  string-fold
  string-fold-right
  string-for-each
  string-for-each-index
  string-hash
  string-hash-ci
  string-index
  string-index-right
  string-join
  string-length
  string-map
  string-normalize-nfc
  string-normalize-nfd
  string-normalize-nfkc
  string-normalize-nfkd
  string-null?
  string-pad
  string-pad-right
  string-prefix-ci?
  string-prefix-length
  string-prefix-length-ci
  string-prefix?
  string-ref
  string-replace
  string-reverse
  string-rindex
  string-skip
  string-skip-right
  string-split
  string-suffix-ci?
  string-suffix-length
  string-suffix-length-ci
  string-suffix?
  string-tabulate
  string-take
  string-take-right
  string-titlecase
  string-tokenize
  string-trim
  string-trim-both
  string-trim-right
  string-unfold
  string-unfold-right
  string-upcase
  string-utf8-length
  string<
  string<=
  string<=?
  string<>
  string<?
  string=
  string=?
  string>
  string>=
  string>=?
  string>?
  string?
  substring
  substring/copy
  substring/read-only
  substring/shared
  xsubstring)

;; Symbol bindings.
(define-bindings (guile)
  string->symbol
  string-ci->symbol
  symbol->string
  list->symbol
  make-symbol
  symbol
  symbol-append
  symbol-interned?
  symbol?)

;; Keyword bindings.
(define-bindings (guile)
  keyword?
  keyword->symbol
  symbol->keyword)

;; Bit bindings.
(define-bindings (guile)
  ash
  round-ash
  logand
  logcount
  logior
  lognot
  logtest
  logxor
  logbit?)

;; Char bindings.
(define-bindings (guile)
  char-alphabetic?
  char-ci<=?
  char-ci<?
  char-ci=?
  char-ci>=?
  char-ci>?
  char-downcase
  char-general-category
  char-is-both?
  char-lower-case?
  char-numeric?
  char-titlecase
  char-upcase
  char-upper-case?
  char-whitespace?
  char<=?
  char<?
  char=?
  char>=?
  char>?
  char?
  char->integer
  integer->char)

;; List bindings.
(define-bindings (guile)
  list
  list-cdr-ref
  list-copy
  list-head
  list-index
  list-ref
  list-tail
  list?
  null?
  make-list
  append
  delete
  delq
  delv
  filter
  length
  member
  memq
  memv
  merge
  reverse)

;; Pair bindings.
(define-bindings (guile)
  last-pair
  pair?
  caaaar
  caaadr
  caaar
  caadar
  caaddr
  caadr
  caar
  cadaar
  cadadr
  cadar
  caddar
  cadddr
  caddr
  cadr
  car
  cdaaar
  cdaadr
  cdaar
  cdadar
  cdaddr
  cdadr
  cdar
  cddaar
  cddadr
  cddar
  cdddar
  cddddr
  cdddr
  cddr
  cdr
  cons
  cons*)

;; Promise bindings.
(define-bindings (guile)
  force
  delay
  make-promise
  promise?)

;; Finally, the potluck bindings.
(define-bindings (guix potluck packages)
  potluck-package
  potluck-source)
