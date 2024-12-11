;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2024 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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

;;;
;;; Please: Try to add new module packages in alphabetic order.
;;;

(define-module (gnu packages crates-check)
  #:use-module (guix build-system cargo)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (gnu packages crates-io))

(define-public rust-mark-flaky-tests-1
  (package
    (name "rust-mark-flaky-tests")
    (version "1.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "mark-flaky-tests" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0c29bflpb5aawl5vzcai2rhvphskvh7gdr5v9sq52lx0jmy4lv2q"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-futures" ,rust-futures-0.3)
        ("rust-mark-flaky-tests-macro" ,rust-mark-flaky-tests-macro-1))
       #:cargo-development-inputs (("rust-paste" ,rust-paste-1)
                                   ("rust-tokio" ,rust-tokio-1))))
    (home-page "https://github.com/GoldsteinE/mark-flaky-tests/")
    (synopsis "Mark, debug and auto-retry your flaky tests")
    (description
     "This package provides a way to mark, debug and auto-retry your flaky tests.")
    (license (list license:expat license:asl2.0))))

(define-public rust-mark-flaky-tests-macro-1
  (package
    (name "rust-mark-flaky-tests-macro")
    (version "1.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "mark-flaky-tests-macro" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "115bb0pb4vb8pwm6lblcnc6zxxlk6w654njiphp696dj2vyiz2q7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-proc-macro-crate" ,rust-proc-macro-crate-1)
                       ("rust-proc-macro-error" ,rust-proc-macro-error-1)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/GoldsteinE/mark-flaky-tests/")
    (synopsis "Mark, debug and auto-retry your flaky tests (proc-macro crate)")
    (description
     "This package provides a way to mark, debug and auto-retry your flaky tests
(proc-macro crate).")
    (license (list license:expat license:asl2.0))))

;;;
;;; Avoid adding new packages to the end of this file. To reduce the chances
;;; of a merge conflict, place them above by existing packages with similar
;;; functionality or similar names.
;;;
