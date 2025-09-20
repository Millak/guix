;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2022 Christopher Rodriguez <yewscion@gmail.com>
;;; Copyright © 2022 Liliana Marie Prikler <liliana.prikler@gmail.com>
;;; Copyright © 2025 Lee Thompson <lee.p.thomp@gmail.com>
;;; © 2025 case_lambda <case_lambda@disroot.org>
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
  #:use-module (guix build-system gnu)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages base)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages java)
  #:use-module (gnu packages compression))

(define-public dbqn
  (package
    (name "dbqn")
    (version "0.2.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/dzaima/BQN")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1zy3y9wbmaw0mrd2sp7d1r912gvs9k0mzw5d3drgmbzkbvpd6iq1"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:imported-modules `(,@%default-gnu-imported-modules
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
                         (rename-file "BQN" "dbqn")
                         (install-file "dbqn" dest-bin)
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
    (license license:expat)))

(define bqn-sources
  ;; Aside from dbqn above, the main bqn repository is used by other
  ;; implementations as a "known good" set of sources. CBQN uses dbqn to
  ;; generate an intermediate bytecode for its own compilation.
    (let ((commit "5880fa153bb3e3172afc59a711af7e471aeedcd3"))
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/mlochbaum/BQN")
              (commit commit)))
        (file-name (git-file-name "bqn-sources" commit))
        (sha256
         (base32 "1cap927i0s8ly4mckppw33ahlc5xnp3l2shk1m79wndf362x3r7c")))))

(define %cbqn-version "0.9.0")

(define cbqn-sources
  (let ((version %cbqn-version))
    (origin
      (method git-fetch)
      (uri (git-reference
            (url "https://github.com/dzaima/CBQN")
            (commit (string-append "v" version))))
      (file-name (git-file-name "cbqn" version))
      (sha256
       (base32 "0433hp9lgv6w6mhdz0k1kx2rmxia76yy9i0z7ps4qdk7snf2yr2q")))))

(define %replxx-commit "13f7b60f4f79c2f14f352a76d94860bad0fc7ce9")

(define replxx-sources
  (let ((commit %replxx-commit))
    (origin
      (method git-fetch)
      (uri
       (git-reference
         (url "https://github.com/dzaima/replxx")
         (commit commit)))
      (file-name (git-file-name "replxx" commit))
      (sha256
       (base32 "0440xjvdkrbpxqjrd6nsrnaxki0mgyinsb0b1dcshjj3h3jr1yy4")))))

(define %singeli-commit "53f42ce4331176d281fa577408ec5a652bdd9127")

(define singeli-sources
  (let ((commit %singeli-commit))
    (origin
      (method git-fetch)
      (uri
       (git-reference
         (url "https://github.com/mlochbaum/Singeli")
         (commit commit)))
      (file-name (git-file-name "singeli" commit))
      (sha256
       (base32 "1dzg4gk74lhy6pwvxzhk4zj1qinc83l7i6x6zpvdajdlz5vqvc1m")))))

(define cbqn-bootstrap
  (package
    (name "cbqn-bootstrap")
    (version %cbqn-version)
    (source cbqn-sources)
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f ;skipping tests for bootstrap
      ;; `make for-bootstrap' implicitly disables REPLXX and Singeli.
      #:make-flags
      #~(list (string-append "CC=" #$(cc-for-target))
              ;; Without specifying a version, the Makefile tries to
              ;; use the current commit hash.
              (string-append "version=" #$version)
              "nogit=1"
              "for-bootstrap")
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (mkdir-p (string-append #$output "/bin"))
              (chmod "BQN" #o755)
              (rename-file "BQN" "bqn")
              (install-file "bqn"
                            (string-append #$output "/bin")))))))
    (inputs (list libffi))
    (synopsis "BQN implementation in C")
    (description
     "This package provides the reference implementation of
@uref{https://mlochbaum.github.io/BQN/, BQN}, a programming language inspired
by APL.")
    (home-page "https://mlochbaum.github.io/BQN/")
    ;; Upstream explains licensing situation
    (license (list license:asl2.0       ;src/utils/ryu*
                   license:boost1.0
                   license:expat        ;src/builtins/sortTemplate.h
                   license:lgpl3+       ;Everything else except the above
                   license:gpl3+
                   license:mpl2.0))))

(define* (cbqn-combined-source name version #:key cbqn-sources replxx-sources singeli-sources)
  (origin
    (method (@@ (guix packages) computed-origin-method))
    (file-name (string-append name "-" version ".tar.gz"))
    (sha256 #f)
    (uri
     (delay
       (with-imported-modules '((guix build utils))
         #~(begin
             (use-modules (guix build utils))
             (let* ((dir (string-append "cbqn" #$version))
                    (replxx-local-dir "build/replxxLocal")
                    (singeli-local-dir "build/singeliLocal"))

               (set-path-environment-variable
                "PATH" '("bin")
                (list #+(canonical-package bash)
                      #+(canonical-package coreutils)
                      #+(canonical-package gzip)
                      #+(canonical-package tar)))
               (copy-recursively #+cbqn-sources dir)

               (with-directory-excursion dir
                 ;; Copy in local replxx and Singeli code
                 (mkdir replxx-local-dir)
                 (mkdir singeli-local-dir)
                 (copy-recursively #+replxx-sources replxx-local-dir)
                 (copy-recursively #+singeli-sources singeli-local-dir))

               (invoke "tar" "cvfa" #$output
                       "--mtime=@0"
                       "--owner=root:0"
                       "--group=root:0"
                       "--sort=name"
                       "--hard-dereference"
                       dir))))))))

(define-public cbqn
  (package
    (inherit cbqn-bootstrap)
    (name "cbqn")
    (outputs '("out" "lib"))
    (source
     (cbqn-combined-source name %cbqn-version
                           #:cbqn-sources cbqn-sources
                           #:replxx-sources replxx-sources
                           #:singeli-sources singeli-sources))
    (arguments
     (substitute-keyword-arguments
         (strip-keyword-arguments (list #:tests?)
                                  (package-arguments cbqn-bootstrap))
       ((#:make-flags flags #~(list))
        #~(cons* "shared-o3" "o3" "for-build" #$flags))
       ((#:phases phases #~%standard-phases)
        #~(modify-phases #$phases
            ;; Build bytecode using bootstrap CBQN
            (add-before 'build 'generate-bytecode
              (lambda* (#:key inputs #:allow-other-keys)
                (let* ((self-hosted-source (dirname (search-input-file
                                                     inputs
                                                     "bqn.bqn"))))
                  (mkdir-p "build/bytecodeLocal/gen")
                  (invoke "bqn" "build/bootstrap.bqn" self-hosted-source))))
            (replace 'check
              (lambda* (#:key inputs tests? #:allow-other-keys)
                (when tests?
                  (system (string-append "./BQN -M 1000 \""
                                         #+bqn-sources "/test/this.bqn\""))
                  (map (lambda (x)
                         (system (string-append "./BQN ./test/" x ".bqn")))
                       '("cmp"
                         "equal"
                         "copy"
                         "bitcpy"
                         "bit"
                         "mut"
                         "hash"
                         "squeezeValid"
                         "squeezeExact"
                         "various"
                         "random"
                         "joinReuse"))
                  (system "make -C test/ffi"))))
            (replace 'install
              (lambda* (#:key outputs #:allow-other-keys)
                (let* ((bin (string-append (assoc-ref outputs "out") "/bin"))
                       (lib (string-append (assoc-ref outputs "lib") "/lib"))
                       (include (string-append (assoc-ref outputs "lib")
                                               "/include")))
                  (mkdir-p bin)
                  (rename-file "BQN" "bqn")
                  (install-file "bqn" bin)
                  (install-file "libcbqn.so" lib)
                  (install-file "include/bqnffi.h" include))))))))
    (native-inputs (list cbqn-bootstrap libffi))
    (inputs (modify-inputs (package-inputs cbqn-bootstrap)
              (prepend bqn-sources)))
    (license (append (package-license cbqn-bootstrap)
                     (list license:isc   ;Singeli module
                           license:bsd-3 ;REPLXX module
                           license:unicode)))
    (properties `((tunable? . #t)))))
