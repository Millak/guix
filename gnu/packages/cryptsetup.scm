;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2016 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2019–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2024 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2025 Antoine Côté <antoine.cote@posteo.net>
;;; Copyright © 2024 Janneke Nieuwenhuizen <janneke@gnu.org>
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

(define-module (gnu packages cryptsetup)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages password-utils)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages popt)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages ruby-check)
  #:use-module (gnu packages web))

(define-public cryptsetup-minimal
  (package
   (name "cryptsetup-minimal")
   (version "2.8.0")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://kernel.org/linux/utils/cryptsetup/v"
                                (version-major+minor version)
                                "/cryptsetup-" version ".tar.xz"))
            (sha256
             (base32
              "18j3a8zzcwc0sragq47vf0b0qyr06bajh2sj6zm1r1ssq8vjv7nc"))))
   (build-system gnu-build-system)
   (arguments
    `(#:configure-flags
      (append
        (if (assoc-ref %build-inputs "ruby-asciidoctor")
            '()
            (list "--disable-asciidoc"))
        (list
          ;; Argon2 is always enabled, this just selects the (faster) full version.
          "--enable-libargon2"
          ;; The default is OpenSSL which provides better PBKDF performance.
          "--with-crypto_backend=gcrypt"
          ;; GRUB 2.06 supports LUKS2, but does it reliably support all set-ups…?
          "--with-default-luks-format=LUKS1"
          ;; External tokens would need an env variable to work on Guix, and we
          ;; don't have users for it yet.
          "--disable-external-tokens"
          "--disable-ssh-token"
          ;; libgcrypt is not found otherwise when cross-compiling.
          ;; <https://issues.guix.gnu.org/63864>
          (string-append "--with-libgcrypt-prefix="
                         (assoc-ref %build-inputs "libgcrypt"))))))
   (native-inputs
    (list pkg-config))
   (inputs
    (list argon2
          json-c
          libgcrypt
          lvm2 ; device-mapper
          popt
          `(,util-linux "lib"))) ;libuuid
   (synopsis "Set up transparent encryption of block devices using dm-crypt")
   (description
    "Cryptsetup is a utility used to conveniently set up disk encryption based
on the @code{dm-crypt} Linux kernel module.  It is most often used to manage
LUKS volumes but also supports plain dm-crypt volumes and loop-AES, TrueCrypt
(including VeraCrypt extension), and BitLocker formats.

@acronym{LUKS, Linux Unified Key Setup} is the standard for hard disk encryption
with the kernel Linux.  It provides a standard on-disk-format compatible amongst
distributions as well as secure management of multiple user passwords.  LUKS
stores all necessary setup information in the partition header to facilitate
data transport and migration.

The package also includes the @command{veritysetup} and @command{integritysetup}
utilities to conveniently configure the @code{dm-verity} and @code{dm-integrity}
block integrity kernel modules.")
   (license license:gpl2)
   (home-page "https://gitlab.com/cryptsetup/cryptsetup")))

(define-public cryptsetup
  (package/inherit cryptsetup-minimal
    (name "cryptsetup")
    (native-inputs `(,(if (supported-package? ruby-asciidoctor/minimal)
                          `("ruby-asciidoctor" ,ruby-asciidoctor/minimal)
                          '())
                     ,@(package-native-inputs cryptsetup-minimal)))))

(define-public (libcryptsetup-propagated-inputs)
  (list argon2
        json-c
        libgcrypt
        lvm2
        `(,util-linux "lib")))

(define (static-library library)
  "Return a variant of package LIBRARY that provides static libraries ('.a'
files).  This assumes LIBRARY uses Libtool."
  (package
    (inherit library)
    (name (string-append (package-name library) "-static"))
    (arguments
     (substitute-keyword-arguments (package-arguments library)
       ((#:configure-flags flags #~'())
        #~(append '("--disable-shared" "--enable-static")
                  #$flags))))))

(define-public cryptsetup-static
  ;; Stripped-down statically-linked 'cryptsetup' command for use in initrds.
  (package
    (inherit cryptsetup-minimal)
    (name "cryptsetup-static")
    (arguments
     (substitute-keyword-arguments (package-arguments cryptsetup-minimal)
       ((#:configure-flags flags ''())
        `(cons* "--disable-shared"
                "--enable-static-cryptsetup"

                "--disable-veritysetup"
                "--disable-integritysetup"
                ;; Bypass broken pkg-config paths for the static output of
                ;; util-linux.  Only blkid is located through pkg-config, not
                ;; uuid.
                (format #f "BLKID_CFLAGS=-I~a"
                        (search-input-directory %build-inputs "include/blkid"))
                (format #f "BLKID_LIBS=-L~a -lblkid"
                        (dirname (search-input-file %build-inputs "lib/libblkid.a")))
                ,flags))
       ((#:allowed-references refs '())
        '())
       ((#:modules modules '())
        '((ice-9 ftw)
          (ice-9 match)
          (guix build utils)
          (guix build gnu-build-system)))
       ((#:phases phases #~%standard-phases)
        #~(modify-phases #$phases
            (add-after 'install 'remove-cruft
              (lambda* (#:key outputs #:allow-other-keys)
                ;; Remove everything except the 'cryptsetup' command.
                (let ((out (assoc-ref outputs "out")))
                  (with-directory-excursion out
                    (let ((dirs (scandir "."
                                         (match-lambda
                                           ((or "." "..") #f)
                                           (_ #t)))))
                      (for-each delete-file-recursively
                                (delete "sbin" dirs))
                      (for-each (lambda (file)
                                  (rename-file (string-append file
                                                              ".static")
                                               file)
                                  (remove-store-references file))
                                '("sbin/cryptsetup"))
                      #t)))))))))
    (inputs
     (let ((libgcrypt-static
            (package
              (inherit (static-library libgcrypt))
              (propagated-inputs
               `(("libgpg-error-host" ,(static-library libgpg-error)))))))
       `(("argon2" ,(static-library argon2))
         ("json-c" ,(static-library json-c-0.13))
         ("libgcrypt" ,libgcrypt-static)
         ("lvm2" ,lvm2-static)
         ("util-linux" ,util-linux "static")
         ("util-linux" ,util-linux "lib")
         ("popt" ,(static-library popt)))))
    (synopsis "Hard disk encryption tool (statically linked)")))
