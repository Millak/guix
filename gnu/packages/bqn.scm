;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2022 Christopher Rodriguez <yewscion@gmail.com>
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

(define-module (gnu packages bqn)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages base)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages java)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages compression))

(define-public dbqn
  (let ((commit "88f2b43966a75cc2c382421218eb30003bb16f4a")
        (revision "1"))
    (package
      (name "dbqn")
      (version (git-version "0.2.1" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/dzaima/BQN")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "06mzvv7kmandhgwb6jwz3rivsj4ic549sy8afnb5zr6mfn5isyg5"))))
      (build-system gnu-build-system)
      (arguments
       (list
        #:imported-modules `(,@%gnu-build-system-modules
                             (guix build syscalls)
                             (guix build ant-build-system))
        #:modules `((guix build gnu-build-system)
                    ((guix build ant-build-system)
                     #:prefix ant:)
                    (guix build utils))
        #:phases #~(modify-phases %standard-phases
                     (delete 'configure)
                     (replace 'build
                       (lambda* _
                         (invoke "./build")
                         (chmod "./BQN" #o755)))
                     (replace 'check
                       (lambda* (#:key tests? #:allow-other-keys)
                         (when tests?
                           (system "./BQN ./test/test"))))
                     (add-after 'install 'reorder-jar-content
                       (lambda* (#:key outputs #:allow-other-keys)
                         (apply (assoc-ref ant:%standard-phases
                                           'reorder-jar-content)
                                #:outputs (list outputs))))
                     (add-after 'reorder-jar-content 'jar-indices
                       (lambda* (#:key outputs #:allow-other-keys)
                         (apply (assoc-ref ant:%standard-phases
                                           'generate-jar-indices)
                                #:outputs (list outputs))))
                     (add-after 'jar-indices 'fix-jar-timestamps
                       (lambda* (#:key outputs #:allow-other-keys)
                         (apply (assoc-ref ant:%standard-phases
                                           'reorder-jar-content)
                                #:outputs (list outputs))))
                     (replace 'install
                       (lambda* (#:key outputs #:allow-other-keys)
                         (let* ((out (assoc-ref outputs "out"))
                                (dest-bin (string-append out "/bin"))
                                (dest-jar (string-append out "/share/java")))
                           (mkdir-p dest-bin)
                           (mkdir-p dest-jar)
                           (copy-recursively "BQN"
                                             (string-append dest-bin
                                                            "/dbqn"))
                           (install-file "BQN.jar" dest-jar)
                           (substitute* (string-append dest-bin "/dbqn")
                             (("BQN.jar")
                              (string-append dest-jar "/BQN.jar")))))))))
      (native-inputs (list `(,icedtea-8 "jdk") zip))
      (inputs (list icedtea-8 bash-minimal))
      (synopsis "BQN implementation based on dzaima/APL")
      (description
       "dbqn is a Java implementation of the
@uref{https://mlochbaum.github.io/BQN/, BQN programming language} that does
not need to be bootstrapped, based on an earlier Java implementation of APL by
the same author.")
      (home-page "https://github.com/dzaima/BQN")
      (license license:expat))))
