;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Danny Milosavljevic <dannym@scratchpost.org>
;;; Copyright © 2019, 2026 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2019–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2021 Vincent Legoll <vincent.legoll@gmail.com>
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

(define-module (gnu packages efi)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages man)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (ice-9 match))

(define-public gnu-efi
  (package
    (name "gnu-efi")
    (version "4.0.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ncroxon/gnu-efi")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "02hhl7xqiiicp0pra99vb0w3hqnyf9fxpzd5nsx1d9v1bk6dsjbp"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f                       ;no tests
      #:make-flags
      #~(list (string-append "PREFIX=" #$output))
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure))))
    (home-page "https://github.com/ncroxon/gnu-efi")
    (synopsis "EFI build environment with GNU toolchain")
    (description
     "This package provides a development environment for building programs
targeting the @acronym{UEFI, Unified Extensible Firmware Interface}
using the GNU toolchain.")
    ;; Distribution is allowed only when accepting all those licenses.
    (license (list license:bsd-2 license:bsd-3 license:bsd-4 license:expat))))

(define-public gnu-efi-3
  (package/inherit gnu-efi
    (name (package-name gnu-efi))
    (version "3.0.19")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ncroxon/gnu-efi")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "05jip30fcc49hfzjp1q63i73l8xkrskbdz1gjswdk6ypaa18mn66"))))))

(define-public efi-analyzer
  ;; No public releases; this is the master tip since 2026-01-28.
  (let ((commit "8b6527eec8fa6d66d5208983cf6a906aad464c6d")
        (revision "1"))
    (package
      (name "efi-analyzer")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/xypron/efi_analyzer")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0i9gg3fk7cq41pg6jf2xyfw60fp9wsavyvxafbf7x031vbgf1wyk"))))
      (build-system gnu-build-system)
      (arguments
       (list
        #:make-flags
        #~(list (string-append "prefix=" #$output))
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'support-cross-compilation
              (lambda _
                (substitute* "Makefile"
                  (("gcc") #$(cc-for-target)))))
            (delete 'configure))))
      (home-page "https://github.com/xypron/efi_analyzer")
      (synopsis "Analyze EFI binaries")
      (description
       "The EFI Analyzer checks EFI binaries and prints out header and section
information.")
      (license license:bsd-2))))

(define-public sbsigntools
  (package
    (name "sbsigntools")
    (version "0.9.5")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://git.kernel.org/pub/scm/linux/kernel/git/jejb/sbsigntools.git")
         (commit (string-append "v" version))
         (recursive? #t)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "060n6w0dx1mrilhdv482ncckanqz6pdv53piimiki0bm15d2fcp4"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-more-shebangs
            (lambda* (#:key inputs native-inputs #:allow-other-keys)
              (substitute* "lib/ccan.git/tools/create-ccan-tree"
                (("#!/bin/bash")
                 (string-append "#!"
                                (search-input-file (or native-inputs inputs)
                                                   "/bin/bash"))))))
          (add-after 'unpack 'patch
            (lambda _
              (substitute* '("configure.ac"
                             "tests/Makefile.am")
                (("/usr/include/efi")
                 (string-append #$(this-package-input "gnu-efi")
                                "/include/efi"))
                (("/usr/lib/gnuefi")
                 (string-append #$(this-package-input "gnu-efi")
                                "/lib")))))
          (add-after 'unpack 'setenv
            (lambda _
              (setenv "CC" #$(cc-for-target)))))))
    (native-inputs
     (list autoconf
           automake
           bash
           help2man
           pkg-config
           util-linux)) ; getopt
    (inputs
     (list gnu-efi
           `(,util-linux "lib") ; libuuid
           openssl))
    (synopsis "EFI signing tools")
    (description "This package provides tools for signing EFI binaries.")
    (home-page "https://git.kernel.org/pub/scm/linux/kernel/git/jejb/sbsigntools.git/")
    (license license:gpl3+)))

(define-public efitools
  (package
    (name "efitools")
    (version "1.9.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.kernel.org/pub/scm/linux/kernel/git/jejb/efitools.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0jabgl2pxvfl780yvghq131ylpf82k7banjz0ksjhlm66ik8gb1i"))
       (patches (search-patches "efitools-riscv64-support.patch"))))
    (build-system gnu-build-system)
    (arguments
     (list
       #:tests? #f          ; No tests exist.
       #:parallel-build? #f ; Makefile contains a race condition.
       #:make-flags
       #~(list "CC=gcc -g -O2 -Wno-error=implicit-function-declaration")
       #:phases
       #~(modify-phases %standard-phases
           (add-after 'unpack 'patch
             (lambda _
               (let ((gnu-efi (assoc-ref %build-inputs "gnu-efi")))
                 (substitute* "Make.rules"
                   (("/usr/include/efi")
                    (string-append gnu-efi "/include/efi"))
                   (("\\$\\(DESTDIR\\)/usr")
                    #$output)
                   (("/usr/lib/gnuefi")
                    (string-append gnu-efi "/lib"))))))
           (add-after 'unpack 'patch-more-shebangs
             (lambda _
               (substitute* "xxdi.pl"
                 (("/usr/bin/env perl")
                  (search-input-file %build-inputs "/bin/perl")))))
           (delete 'configure))))
    (native-inputs (list help2man perl perl-file-slurp sbsigntools))
    (inputs (list gnu-efi openssl))
    (synopsis "EFI tools (key management, variable management)")
    (description "This package provides EFI tools for EFI key management
and EFI variable management.")
    (home-page
     "https://blog.hansenpartnership.com/efitools-1-4-with-linux-key-manipulation-utilities-released/")
    ;; Programs are under GPL 2.
    ;; Library routines (in lib/) are under LGPL 2.1.
    ;; Compiling/linking/using OpenSSL is permitted.
    (license (list license:gpl2 license:lgpl2.1))))
