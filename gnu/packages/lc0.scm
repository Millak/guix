;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2023 zamfofex <zamfofex@twdb.moe>
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

(define-module (gnu packages lc0)
  #:use-module (guix build utils)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system trivial)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages c)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages machine-learning)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python))

(define-public lc0
  (package
    (name "lc0")
    (version "0.31.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/LeelaChessZero/lc0")
                    (commit (string-append "v" version))
                    ;; Only a few source files are in one Git submodules
                    ;; (rather than there being bundled projects).  These
                    ;; files are in a different repository just because they
                    ;; are used across multiple repositories of the Leela
                    ;; Chess Zero project.
                    (recursive? #t)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0hhil95vwx3vval8wz5k8cr3djj2lgy5racahmj9xjaj7h6as1pk"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list "-Ddnnl=true"
              (string-append "-Ddnnl_dir="
                             #$(this-package-input "oneapi-dnnl")))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-march-native
            (lambda _
              (substitute* "meson.build"
                (("-march=native") "")))))))
    (native-search-paths
     (list (search-path-specification
            (variable "XDG_DATA_DIRS")
            (files '("share")))))
    (inputs (list eigen oneapi-dnnl zlib))
    (native-inputs (list googletest ispc pkg-config python))
    (synopsis "Chess engine based on neural networks")
    (description
     "Leela Chess Zero is a UCI-compliant chess engine designed to play chess
using neural networks.  This package does not provide a neural network, which
is necessary to use Leela Chess Zero and should be installed separately.")
    (home-page "https://lczero.org")
    (license license:gpl3+)))

(define (make-lc0-net name file-name url hash description)
  (package
    (name (string-append "lc0-" name))
    (version "0")
    (source (origin
              (method url-fetch)
              (uri url)
              (file-name (string-append "lc0-" file-name))
              (sha256
               (base32 hash))))
    (build-system trivial-build-system)
    (arguments
     (list #:builder
           (with-imported-modules '((guix build utils))
             #~(let ((share (string-append #$output "/share/lc0/")))
                 (use-modules (guix build utils))
                 (mkdir-p share)
                 (copy-file (assoc-ref %build-inputs "source")
                            (string-append share #$file-name))))))
    (synopsis "Pre-trained neural network for Leela Chess Zero")
    (description description)
    (home-page "https://lczero.org")
    (license license:gpl3+)))

(define-public (make-lc0-official-net name url-hash hash description)
  (make-lc0-net
   name
   (string-append name ".pb.gz")
   (string-append "https://storage.lczero.org/files/networks/" url-hash)
   hash
   description))

(define-public (make-lc0-contrib-net name file-name hash description)
  (make-lc0-net name file-name
                (string-append
                 "https://storage.lczero.org/files/networks-contrib/"
                 file-name)
                hash description))

(define-public lc0-t2
  (make-lc0-contrib-net "t2" "t2-768x15x24h-swa-5230000.pb.gz"
   "126305hby24k5agxiixadp1lcgmv8drm1dkpb4jf5jzq7k4m855w"
   "T2 is currently one of the best neural networks for Leela Chess Zero,
superseding the neural network T1."))

(define-public lc0-t1
  (make-lc0-contrib-net "t1" "t1-768x15x24h-swa-4000000.pb.gz"
   "0zm9v91gfnm69n4x2fckair24fypjrwiip3j86ylsqncsi1sh5nq"
   "T1 is currently one of the best neural networks for Leela Chess Zero,
however, it was superseded by the neural network T2."))

(define-public lc0-t1-512
  (make-lc0-contrib-net "t1-512" "t1-512x15x8h-distilled-swa-3395000.pb.gz"
   "1s4pq06qkdpaq0a4z6j5qqnf6h7njpmwh7i0v7qh6bmhwlcibnqz"
   "This is a smaller version of the T1 neural network, which is currently one
of the best neural networks for Leela Chess Zero."))

(define-public lc0-t1-256
  (make-lc0-contrib-net "t1-256" "t1-256x10-distilled-swa-2432500.pb.gz"
   "01ilar7y2nf3kfkq3x77n4jxlvqdpgddjsham2wz4dmdx35ac9xw"
   "This is a smaller version of the T1 neural network, which is currently one
of the best neural networks for Leela Chess Zero."))

(define-public lc0-611246
  (make-lc0-official-net "611246"
   "7ca2381cfeac5c280f304e7027ffbea1b7d87474672e5d6fb16d5cd881640e04"
   "0rapfvjqqrhydhp8a6r3skmjci2dc2yxz912bglv9yd9k00l9rww"
   "This is an official neural network of a ``main run'' of the Leela Chess
Zero project that was finished being trained in January of 2022."))

(define-public lc0-791556
  (make-lc0-official-net "791556"
   "f404e156ceb2882470fd8c032b8754af0fa0b71168328912eaef14671a256e34"
   "03b9xl9vkiihdilz5dzcpg6g4inb6n4k5gs911i3gbd8h9sh9ixi"
   "This is an official neural network of the Leela Chess Zero project that
was finished being trained in April of 2022."))

(define-public lc0-815383
  (make-lc0-official-net "815383"
   "8af6098d6fb76e2bef6ef9ee87e61fadcb99f0b789013a72953a95ad10a9e933"
   "09gm8lgaick60rn4x9h9w5sxdqivr4ign73viviadw1gj7wsbnsg"
   "This is an official neural network of a ``main run'' of the Leela Chess
Zero project.  The network was finished being trained in September of 2023."))

(define (make-lc0-maia rating)
  (package
    (name (string-append "lc0-maia-" rating))
    (version "1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/CSSLab/maia-chess")
                    (commit (string-append "v" version))))
              (file-name (git-file-name "maia" version))
              (sha256
               (base32
                "0qjkp56pb5vvkr3j1vdsdzligvy7faza917z7vdfmf168pkvrxsr"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:install-plan
      #~(list
         `(,(string-append "model_files/" #$rating
                           "/final_" #$rating "-40.pb.gz")
           ,(string-append "share/lc0/maia-" #$rating ".pb.gz")))))
    (synopsis "Human-like neural network for Leela Chess Zero")
    (description
     "Maia’s goal is to play the human move, not necessarily the best move.
As a result, Maia has a more human-like style than previous engines, matching
moves played by human players in online games over 50% of the time.")
    (home-page "https://maiachess.com")
    (license license:gpl3)))

(define-public lc0-maia-1100
  (make-lc0-maia "1100"))

(define-public lc0-maia-1200
  (make-lc0-maia "1200"))

(define-public lc0-maia-1300
  (make-lc0-maia "1300"))

(define-public lc0-maia-1400
  (make-lc0-maia "1400"))

(define-public lc0-maia-1500
  (make-lc0-maia "1500"))

(define-public lc0-maia-1600
  (make-lc0-maia "1600"))

(define-public lc0-maia-1700
  (make-lc0-maia "1700"))

(define-public lc0-maia-1800
  (make-lc0-maia "1800"))

(define-public lc0-maia-1900
  (make-lc0-maia "1900"))
