;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2026 Cayetano Santos <csantosb@inventati.org>
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

(define-module (gnu packages computer-architecture)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system qt)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages electronics)
  #:use-module (gnu packages python)
  #:use-module (gnu packages qt))

(define-public ripes
  ;; Includes Qt6 support.
  (let ((commit "720066f8d1c9f8d62fad058f44471763a0312084")
        (revision "0"))
    (package
      (name "ripes")
      (version (git-version "2.2.6" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
                (url "https://github.com/mortbopet/Ripes/")
                (commit commit)
                ;; TODO: Unbundle. Ripes recursively requires tones of non
                ;; packaged dependencies in "external" directories.
                (recursive? #t)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1kkdc3y18h985zvgmhbdbms3zynygrg5f64w1lm948hffj58x7gy"))))
      (build-system qt-build-system)
      (arguments
       (list
        #:qtbase qtbase                   ;for Qt6
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'disable-tests
              (lambda _
                (substitute* "test/CMakeLists.txt"
                  ;; Fails with "Compared values are not the same"
                  (("create_qtest\\(tst_riscv\\)") "")
                  ;; Fails with "Internal test error"
                  (("create_qtest\\(tst_reverse\\)") ""))))
            (add-after 'unpack 'patch-bugs
              (lambda* (#:key inputs #:allow-other-keys)
                ;; TODO: Remove when fixed upstream.
                ;; See https://github.com/mortbopet/Ripes/issues/388 and 62
                (substitute*
                    (string-append "external/VSRTL/external/cereal/include"
                                   "/cereal/external/rapidjson/document.h")
                  (("GenericStringRef& operator=.*")
                   (string-append
                    "GenericStringRef& operator=(const GenericStringRef& rhs)"
                    " { s = rhs.s; return *this; }")))
                ;; As suggested by error message itself.
                (substitute* '("external/VSRTL/interface/vsrtl_vcdfile.cpp")
                  (("#include <sstream>")
                   "#include <sstream>\n#include <cstdint>"))
                ;; As suggested by error message itself.
                (substitute* '("external/VSRTL/interface/vsrtl_vcdfile.h")
                  (("#include <map>")
                   "#include <map>\n#include <cstdint>"))))
            (add-before 'configure 'set-envs
              (lambda* (#:key inputs #:allow-other-keys)
                (setenv "VERILATOR_ROOT"
                        (dirname
                         (dirname
                          (search-input-file inputs "/bin/verilator"))))))
            (replace 'check             ;as for .github/workflows/test.yml
              (lambda* (#:key tests? #:allow-other-keys)
                (when tests?
                  (for-each
                   (lambda (f) (invoke f))
                   (filter executable-file? (find-files "test"  "tst_")))))))
        #:configure-flags
        #~(list "-DRIPES_BUILD_VERILATOR_PROCESSORS=ON"
                "-DRIPES_BUILD_TESTS=ON")))
      (native-inputs
       (list python-minimal-wrapper verilator))
      (inputs
       (list qtsvg qtcharts))
      (home-page "https://github.com/mortbopet/Ripes/")
      (synopsis "Visual computer architecture editor for RISCV")
      (description
       "Ripes is a visual computer architecture simulator and assembly code
editor built for the RISCV instruction set architecture.")
      (license license:expat))))
