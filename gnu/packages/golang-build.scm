;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Brian Leung <bkleung89@gmail.com>
;;; Copyright © 2019, 2020 Leo Famulari <leo@famulari.name>
;;; Copyright © 2020 Danny Milosavljevic <dannym@scratchpost.org>
;;; Copyright © 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2020 HiPhish <hiphish@posteo.de>
;;; Copyright © 2020 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2020 Ryan Prior <rprior@protonmail.com>
;;; Copyright © 2020 Vagrant Cascadian <vagrant@debian.org>
;;; Copyright © 2021 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2021 Sarah Morgensen <iskarian@mgsn.dev>
;;; Copyright © 2021 hackeryarn <artemchernyak@gmail.com>
;;; Copyright © 2023 Artyom V. Poptsov <poptsov.artyom@gmail.com>
;;; Copyright © 2023 Felix Lechner <felix.lechner@lease-up.com>
;;; Copyright © 2023 Katherine Cox-Buday <cox.katherine.e@gmail.com>
;;; Copyright © 2023 Nicolas Graves <ngraves@ngraves.fr>
;;; Copyright © 2023 Timo Wilken <guix@twilken.net>
;;; Copyright © 2024 Troy Figiel <troy@troyfigiel.com>
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

(define-module (gnu packages golang-build)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system go)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages))

;;; Commentary:
;;;
;;; Modules (libraries) which are part of the Golang project but outside the
;;; main Golang tree, see <https://pkg.go.dev/golang.org/x>
;;;
;;; Since they are bound to be relied on by many, their dependencies should be
;;; kept minimal, and this module should not depend on other modules
;;; containing Golang packages.
;;;
;;; Please: Try to add new module packages in alphabetic order.
;;;
;;; Code:

(define-public go-github-com-yuin-goldmark
  (package
    (name "go-github-com-yuin-goldmark")
    (version "1.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/yuin/goldmark")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "12rsnsf65drcp0jfw2jl9w589vsn3pxdk1zh3v9q908iigngrcmy"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/yuin/goldmark"))
    (home-page "https://github.com/yuin/goldmark/")
    (synopsis "Markdown parser")
    (description "This package provides a markdown parser.")
    (license license:expat)))

(define-public go-golang-org-x-crypto
  (package
    (name "go-golang-org-x-crypto")
    (version "0.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://go.googlesource.com/crypto")
             (commit (string-append "v" version))))
       (file-name (string-append "go.googlesource.com-crypto-"
                                 version "-checkout"))
       (sha256
        (base32 "13i0yz4hvc4qdr438nmzilvl5ns73v3910bakcddny3jbzq72i2m"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "golang.org/x/crypto"
       ;; Source-only package
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         ;; Source-only package
         (delete 'build))))
    (propagated-inputs
     (list go-golang-org-x-sys))
    (home-page "https://go.googlesource.com/crypto/")
    (synopsis "Supplementary cryptographic libraries in Go")
    (description "This package provides supplementary cryptographic libraries
for the Go language.")
    (license license:bsd-3)))

(define-public go-golang-org-x-exp
  (package
    (name "go-golang-org-x-exp")
    (version "0.0.0-20221004215720-b9f4876ce741")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://go.googlesource.com/exp")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "030b929xyg8dpp6f4qbyg63msi6zgzj9sqmvnyphfcrjkqf7nr41"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "golang.org/x/exp"
       ;; Source-only package
       #:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build))))
    (home-page "https://golang.org/x/exp")
    (synopsis "Experimental and deprecated Go packages")
    (description "This subrepository holds experimental and deprecated (in the
@code{old} directory) packages.")
    (license license:bsd-3)))

;; It's for the Kubo update; remove it when it is no longer needed.
(define-public go-golang-org-x-exp-2023
  (package
    (inherit go-golang-org-x-exp)
    (name "go-golang-org-x-exp")
    (version "0.0.0-20230725012225-302865e7556b")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://go.googlesource.com/exp")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1fkmi0zjwv70n9d9q9bmzch2zvf7xm3d65qh63zll6dljq2lh4b8"))))))

(define-public go-golang-org-x-image
  (let ((commit "58c23975cae11f062d4b3b0c143fe248faac195d")
        (revision "1"))
    (package
      (name "go-golang-org-x-image")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://go.googlesource.com/image")
               (commit commit)))
         (file-name (string-append "go.googlesource.com-image-"
                                   version "-checkout"))
         (sha256
          (base32 "0i2p2girc1sfcic6xs6vrq0fp3szfx057xppksb67kliywjjrm5x"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "golang.org/x/image"
         ;; Source-only package
         #:tests? #f
         #:phases
         (modify-phases %standard-phases
           (delete 'build))))
      (home-page "https://go.googlesource.com/image")
      (synopsis "Supplemental Go image libraries")
      (description "This package provides supplemental Go libraries for image
processing.")
      (license license:bsd-3))))

(define-public go-golang-org-x-mod
  (let ((commit "7c05a442b7c1d1a107879b4a090bb5a38d3774a1")
        (revision "0"))
    (package
      (name "go-golang-org-x-mod")
      (version (git-version "0.7.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/golang/mod")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "14r24fq3kn84k2y2jvvg8hwpy52a3q429pimrdwl5zwknbr2awmh"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "golang.org/x/mod/"
         #:tests? #f
         #:phases (modify-phases %standard-phases
                    ;; Source-only package
                    (delete 'build))))
      (home-page "https://golang.org/x/mod")
      (synopsis "Tools to work directly with Go module mechanics")
      (description
       "This repository holds packages for writing tools that work directly
with Go module mechanics.  That is, it is for direct manipulation of Go
modules themselves.

The specific case of loading packages should still be done by invoking the
@command{go} command, which remains the single point of truth for package
loading algorithms.")
      (license license:bsd-3))))

(define-public go-golang-org-x-net
  (let ((commit "8e0e7d8d38f2b6d21d742845570dde2902d06a1d")
        (revision "0"))
    (package
      (name "go-golang-org-x-net")
      (version (git-version "0.5.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://go.googlesource.com/net")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1fidlcn3vcz42v2lc0rpmqh3bz08bcklj6jvnmz2vvgc481ci5hy"))))
      (build-system go-build-system)
      (arguments
       (list
        #:import-path "golang.org/x/net"
        ;; Source-only package
        #:tests? #f
        #:phases
        #~(modify-phases %standard-phases
            ;; Source-only package
            (delete 'build))))
      (home-page "https://go.googlesource.com/net")
      (synopsis "Go supplemental networking libraries")
      (description "This package provides supplemental Go networking libraries.")
      (license license:bsd-3))))

;; XXX: Not in use by any other packages, consider to remove or merge with
;; go-golang-org-x-net.
(define-public go-golang-org-x-net-0.17
  (let ((commit "b225e7ca6dde1ef5a5ae5ce922861bda011cfabd")
        (revision "0"))
    (package
      (inherit go-golang-org-x-net)
      (name "go-golang-org-x-net")
      (version (git-version "0.17.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://go.googlesource.com/net")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "17zhim2m0r8nyy18g2lsawxm4rawix2qbjyn80x9vc6jc8fv05m9")))))))

(define-public go-golang-org-x-net-html
  (package
    (inherit go-golang-org-x-net)
    (name "go-golang-org-x-net-html")
    (arguments
     '(#:import-path "golang.org/x/net/html"
       #:unpack-path "golang.org/x/net"))
    (propagated-inputs (list go-golang-org-x-text))
    (home-page "https://godoc.org/golang.org/x/net/html")
    (synopsis "HTML5-compliant tokenizer and parser")
    (description
     "This package provides an HTML5-compliant tokenizer and parser.")))

(define-public go-golang-org-x-sync
  (let ((commit "8fcdb60fdcc0539c5e357b2308249e4e752147f1")
        (revision "1"))
    (package
      (name "go-golang-org-x-sync")
      (version (git-version "0.1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://go.googlesource.com/sync")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "07qrhni6f5hh5p95k1yk6s4wsj341q663irvx6rllrxfsymj6a0z"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "golang.org/x/sync"
         #:tests? #f
         ;; Source-only package
         #:phases (modify-phases %standard-phases
                    (delete 'build))))
      (home-page "https://go.googlesource.com/sync/")
      (synopsis "Additional Go concurrency primitives")
      (description "This package provides Go concurrency primitives in
addition to the ones provided by the language and “sync” and “sync/atomic”
packages.")
      (license license:bsd-3))))

(define-public go-golang.org-x-sync-errgroup
  (let ((commit "cd5d95a43a6e21273425c7ae415d3df9ea832eeb")
        (revision "0"))
    (package
      (name "go-golang.org-x-sync-errgroup")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://go.googlesource.com/sync")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1nqkyz2y1qvqcma52ijh02s8aiqmkfb95j08f6zcjhbga3ds6hds"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "golang.org/x/sync/errgroup"
         #:unpack-path "golang.org/x/sync"))
      (home-page "https://godoc.org/golang.org/x/sync/errgroup")
      (synopsis "Synchronization, error propagation, and Context cancellation
for groups of goroutines working on subtasks of a common task")
      (description "This package provides synchronization, error
propagation, and Context cancellation for groups of goroutines working on
subtasks of a common task.")
      (license license:bsd-3))))

(define-public go-golang.org-x-sync-semaphore
  (package
    (inherit go-golang.org-x-sync-errgroup)
    (name "go-golang.org-x-sync-semaphore")
    (arguments
     '(#:import-path "golang.org/x/sync/semaphore"
       #:unpack-path "golang.org/x/sync"))
    (home-page "https://godoc.org/golang.org/x/sync/semaphore")
    (synopsis "Weighted semaphore implementation in Go")
    (description "Weighted semaphore implementation in Go.")))

(define-public go-golang-org-x-sys
  (let ((commit "ca59edaa5a761e1d0ea91d6c07b063f85ef24f78")
        (revision "0"))
    (package
      (name "go-golang-org-x-sys")
      (version (git-version "0.8.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://go.googlesource.com/sys")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1p81niiin8dwyrjl2xsc95136w3vdw4kmj0w3mlh0vh5v134s4xq"))))
      (build-system go-build-system)
      (arguments
       (list
        #:import-path "golang.org/x/sys"
        ;; Source-only package
        #:tests? #f
        #:phases
        #~(modify-phases %standard-phases
            ;; Source-only package
            (delete 'build))))
      (home-page "https://go.googlesource.com/sys")
      (synopsis "Go support for low-level system interaction")
      (description "This package provides supplemental libraries offering Go
support for low-level interaction with the operating system.")
      (license license:bsd-3))))

(define-public go-golang-org-x-term
  (package
    (name "go-golang-org-x-term")
    (version "0.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://go.googlesource.com/term")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "16s3d62fgdhiqvcib61s5pwxp08hhrmzx8bdv5zk1w1krjizdarl"))))
    (build-system go-build-system)
    (arguments '(#:import-path "golang.org/x/term"))
    (propagated-inputs
     (list go-golang-org-x-sys))
    (home-page "https://pkg.go.dev/golang.org/x/term")
    (synopsis "Go terminal/console support")
    (description "@code{term} provides support functions for dealing with
terminals, as commonly found on Unix systems.")
    (license license:bsd-3)))

(define-public go-golang-org-x-text
  (package
    (name "go-golang-org-x-text")
    (version "0.3.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://go.googlesource.com/text")
             (commit (string-append "v" version))))
       (file-name (string-append "go.googlesource.com-text-"
                                 version "-checkout"))
       (sha256
        (base32 "0flv9idw0jm5nm8lx25xqanbkqgfiym6619w575p7nrdh0riqwqh"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "golang.org/x/text"
       ;; Source-only package
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'build))))
    (home-page "https://go.googlesource.com/text")
    (synopsis "Supplemental Go text processing libraries")
    (description "This package provides supplemental Go libraries for text
processing.")
    (license license:bsd-3)))

(define-public go-golang-org-x-time
  (let ((commit "9d24e82272b4f38b78bc8cff74fa936d31ccd8ef")
        (revision "2"))
    (package
      (name "go-golang-org-x-time")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://go.googlesource.com/time")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1f5nkr4vys2vbd8wrwyiq2f5wcaahhpxmia85d1gshcbqjqf8dkb"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "golang.org/x/time"
         ;; Source-only package
         #:tests? #f
         #:phases
         (modify-phases %standard-phases
         ;; Source-only package
           (delete 'build))))
      (home-page "https://godoc.org/golang.org/x/time/rate")
      (synopsis "Supplemental Go time libraries")
      (description "This package provides supplemental Go libraries related to
time.")
      (license license:bsd-3))))

(define-public go-golang-org-x-tools
  (package
    (name "go-golang-org-x-tools")
    (version "0.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://go.googlesource.com/tools")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "08kx2nndq3sr6xai7403mbsqvz5shxmp2icylfr2fmwagr59cb2n"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; gopls versions are tagged separately, and it is a
           ;; separate Guix package.
           (delete-file-recursively "gopls")))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "golang.org/x/tools"
       ;; Source-only package
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         ;; Source-only package
         (delete 'build))))
    (propagated-inputs
     (list go-github-com-yuin-goldmark
           go-golang-org-x-mod
           go-golang-org-x-net
           go-golang-org-x-sys))
    (home-page "https://go.googlesource.com/tools/")
    (synopsis "Tools that support the Go programming language")
    (description "This package provides miscellaneous tools that support the
Go programming language.")
    (license license:bsd-3)))

(define-public go-golang-org-x-xerrors
  (let ((commit "5ec99f83aff198f5fbd629d6c8d8eb38a04218ca")
        (revision "0"))
    (package
      (name "go-golang-org-x-xerrors")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://go.googlesource.com/xerrors")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1dbzc3gmf2haazpv7cgmv97rq40g2xzwbglc17vas8dwhgwgwrzb"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "golang.org/x/xerrors"))
      (home-page "https://godoc.org/golang.org/x/xerrors")
      (synopsis "Go 1.13 error values")
      (description "This package holds the transition packages for the new Go
1.13 error values.")
      (license license:bsd-3))))

;;;
;;; Avoid adding new packages to the end of this file. To reduce the chances
;;; of a merge conflict, place them above by existing packages with similar
;;; functionality or similar names.
;;;
