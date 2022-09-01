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

(define bqn-sources
  ;; Aside from dbqn above, the main bqn repository is used by other
  ;; implementations as a "known good" set of sources. CBQN uses dbqn to
  ;; generate an intermediate bytecode for its own compilation.
    (let ((commit "e219af48401473a7bac49bdd8b89d69082cf5dd8"))
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/mlochbaum/BQN")
              (commit commit)))
        (file-name (git-file-name "bqn-sources" commit))
        (sha256
         (base32 "0r6pa9lscl2395g4xlvmg90vpdsjzhin4f1r0s7brymmpvmns2yc")))))

(define cbqn-bootstrap
  (let* ((revision "1")
         (commit "9c1cbdc99863b1da0116df61cd832137b196dc5c"))
    (package
      (name "cbqn-bootstrap")
      (version (git-version "0" "1" commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/dzaima/CBQN")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0w38fhwf20drkyijy6nfnhmc5g5gw0zmzgmy1q605x57znlj85a2"))))
      (build-system gnu-build-system)
      (arguments
       (list #:tests? #f                         ;skipping tests for bootstrap
             #:phases #~(modify-phases %standard-phases
                          (delete 'configure)
                          (add-before 'build 'generate-bytecode
                            (lambda* (#:key inputs #:allow-other-keys)
                              (system (string-append #+dbqn
                                                     "/bin/dbqn ./genRuntime "
                                                     #+bqn-sources))))
                          (replace 'install
                            (lambda* (#:key outputs #:allow-other-keys)
                              (mkdir-p (string-append #$output "/bin"))
                              (chmod "BQN" #o755)
                              (copy-recursively "BQN"
                                                (string-append #$output
                                                               "/bin/bqn")))))))
      (native-inputs (list dbqn clang-toolchain bqn-sources))
      (inputs (list icedtea-8 libffi))
      (synopsis "BQN implementation in C")
      (description "This package provides the reference implementation of
@uref{https://mlochbaum.github.io/BQN/, BQN}, a programming language inspired
by APL.")
      (home-page "https://mlochbaum.github.io/BQN/")
      (license license:gpl3))))

(define singeli-sources
  (let ((commit "fd17b144483549dbd2bcf23e3a37a09219171a99"))
    (origin
      (method git-fetch)
      (uri (git-reference
            (url "https://github.com/mlochbaum/Singeli")
            (commit commit)))
      (file-name (git-file-name "singeli-sources" commit))
      (sha256
       (base32 "1rr4l7ijzcg25n2igi1mzya6qllh5wsrf3m5i429rlgwv1fwvfji")))))

(define-public cbqn
  (package
    (inherit cbqn-bootstrap)
    (name "cbqn")
    (outputs '("out" "lib"))
    (arguments
     (list #:make-flags '(list "shared-o3" "o3n-singeli")
           #:phases #~(modify-phases %standard-phases
                        (delete 'configure)
                        (add-before 'build 'link-singeli
                          (lambda* (#:key inputs #:allow-other-keys)
                            (symlink #+singeli-sources "Singeli")))
                        (add-before 'build 'generate-bytecode
                          (lambda* (#:key inputs #:allow-other-keys)
                            (system (string-append #+dbqn
                                                   "/bin/dbqn ./genRuntime "
                                                   #+bqn-sources))))
                        (replace 'check
                          (lambda* (#:key inputs tests? #:allow-other-keys)
                            (when tests?
                              (system (string-append "./BQN -M 1000 \""
                                                     #+bqn-sources
                                                     "/test/this.bqn\""))
                              (map (lambda (x)
                                     (system (string-append "./BQN ./test/" x
                                                            ".bqn")))
                                   '("cmp" "equal" "copy" "random"))
                              (system "make -C test/ffi"))))
                        (replace 'install
                          (lambda* (#:key outputs #:allow-other-keys)
                            (let* ((bin (string-append (assoc-ref outputs
                                                                  "out")
                                                       "/bin"))
                                   (lib (string-append (assoc-ref outputs
                                                                  "lib")
                                                       "/lib")))
                              (mkdir-p bin)
                              (copy-recursively "BQN"
                                                (string-append bin "/bqn"))
                              (install-file "libcbqn.so" lib)))))))
    (native-inputs (list dbqn
                         bqn-sources
                         singeli-sources
                         libffi
                         clang-toolchain
                         linux-libre-headers))))
