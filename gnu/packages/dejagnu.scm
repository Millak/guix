;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016, 2017, 2022 Efraim Flashner <efraim@flasher.co.il>
;;; Copyright © 2018 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2022 Maxime Devos <maximedevos@telenet.be>
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

(define-module (gnu packages dejagnu)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix build-system gnu)
  #:use-module (guix licenses)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages tcl))

(define-public dejagnu
  (package
    (name "dejagnu")
    (version "1.6.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnu/dejagnu/dejagnu-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1qx2cv6qkxbiqg87jh217jb62hk3s7dmcs4cz1llm2wmsynfznl7"))))
    (build-system gnu-build-system)
    (inputs (list bash-minimal expect))
    (arguments
     (list
      ;; Do an out-of-source build to work-around a non-deterministic bug in Expect:
      ;; <https://lists.gnu.org/archive/html/bug-dejagnu/2021-06/msg00013.html>.
      #:out-of-source? #true
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-/bin/sh
            (lambda* (#:key inputs #:allow-other-keys)
              ;; Patch embedded /bin/sh references.
              (let ((/bin/sh (search-input-file inputs "/bin/sh")))
                (substitute* "dejagnu"
                  (("exec /bin/sh")
                   (string-append "exec " /bin/sh)))
                (substitute* (find-files "testsuite/report-card.all")
                  (("/bin/sh") /bin/sh)))))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              ;; Note: The test-suite *requires* /dev/pts among the
              ;; `build-chroot-dirs' of the build daemon when
              ;; building in a chroot.  See
              ;; <http://thread.gmane.org/gmane.linux.distributions.nixos/1036>
              ;; for details.
              (when tests?
                (if (and (directory-exists? "/dev/pts")
                         (directory-exists? "/proc"))
                    (begin
                      ;; Provide `runtest' with a log name, otherwise it
                      ;; tries to run `whoami', which fails when in a chroot.
                      (setenv "LOGNAME" "guix-builder")

                      ;; The test-suite needs to have a non-empty stdin:
                      ;; <http://lists.gnu.org/archive/html/bug-dejagnu/2003-06/msg00002.html>.
                      (unless (zero? (system "make check < /dev/zero"))
                        (error "make check failed")))
                    (display "test suite cannot be run, skipping\n")))))
          (add-after 'install 'post-install
            (lambda* (#:key inputs outputs #:allow-other-keys)
              ;; Use the right `expect' binary.
              (let ((expect (search-input-file inputs "/bin/expect")))
                (substitute* (string-append #$output "/bin/runtest")
                  (("^mypath.*$" all)
                   (string-append all
                                  "export PATH="
                                  (dirname expect) ":$PATH\n")))))))))
    (home-page "https://www.gnu.org/software/dejagnu/")
    (synopsis "GNU software testing framework")
    (description
     "DejaGnu is a framework for testing software.  In effect, it serves as
a front-end for all tests written for a program.  Thus, each program can have
multiple test suites, which are then all managed by a single harness.")
    (license gpl3+)))
