;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 David Thompson <davet@gnu.org>
;;; Copyright © 2015, 2017, 2018, 2019, 2022 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016, 2017, 2018, 2019, 2021 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016 Lukas Gradl <lgradl@openmailbox>
;;; Copyright © 2016–2022 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2016, 2017 Nikita <nikita@n0.is>
;;; Copyright © 2016, 2017, 2019, 2020 Eric Bavier <bavier@posteo.net>
;;; Copyright © 2017 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2018, 2020, 2021, 2023, 2025 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2018 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2018, 2020 Nicolò Balzarotti <nicolo@nixo.xyz>
;;; Copyright © 2018 Tim Gesthuizen <tim.gesthuizen@yahoo.de>
;;; Copyright © 2019 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2019 Tanguy Le Carrour <tanguy@bioneland.org>
;;; Copyright © 2020 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2020 Brice Waegeneire <brice@waegenei.re>
;;; Copyright © 2020 Hendur Saga <hendursaga@yahoo.com>
;;; Copyright © 2020 pukkamustard <pukkamustard@posteo.net>
;;; Copyright © 2021 Ellis Kenyő <me@elken.dev>
;;; Copyright © 2021 Maxime Devos <maximedevos@telenet.be>
;;; Copyright © 2021, 2022 Brendan Tildesley <mail@brendan.scot>
;;; Copyright © 2022 Allan Adair <allan@adair.no>
;;; Copyright © 2022 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2022 Denis 'GNUtoo' Carikli <GNUtoo@cyberdimension.org>
;;; Copyright © 2023 Ivan Vilata-i-Balaguer <ivan@selidor.net>
;;; Copyright © 2023 Foundation Devices, Inc. <hello@foundationdevices.com>
;;; Copyright © 2024, 2025 Janneke Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2025 Ashish SHUKLA <ashish.is@lostca.se>
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

(define-module (gnu packages crypto)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages aidc)
  #:use-module (gnu packages attr)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages crates-crypto)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages cryptsetup)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages image)
  #:use-module (gnu packages kerberos)
  #:use-module (gnu packages libbsd)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages logging)
  #:use-module (gnu packages lsof)
  #:use-module (gnu packages man)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages nettle)
  #:use-module (gnu packages password-utils)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-check)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages search)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages xml)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system python)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26))

(define-public libdecaf
  (package
    (name "libdecaf")
    (version "1.0.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "git://git.code.sf.net/p/ed448goldilocks/code")
                    (commit
                     (string-append "v" version))))
              (file-name
               (git-file-name name version))
              (sha256
               (base32 "1ajgmyvc6a4m1h2hg1g4wz7ibx10x1xys9m6ancnmmf1f2srlfly"))))
    (build-system cmake-build-system)
    (outputs '("out" "python" "doc"))
    (arguments
     `(#:configure-flags '("-DENABLE_STATIC=OFF")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-python-binding
           (lambda _
             (substitute* "python/setup.py"
               (("gmake")
                "make")
               (("'\\.\\.', 'build', 'lib', 'libdecaf\\.so'")
                "'..', '..', 'build', 'src', 'libdecaf.so'"))))
         (add-after 'install 'install-python-binding
           (lambda* (#:key outputs #:allow-other-keys)
             (with-directory-excursion "../source/python"
               (invoke "python" "setup.py" "install"
                       (string-append "--prefix=" (assoc-ref outputs "python"))
                       "--root=/"))))
         (add-after 'install-python-binding 'install-documentation
           (lambda* (#:key outputs #:allow-other-keys)
             (invoke "make" "doc")
             (let* ((doc (assoc-ref outputs "doc"))
                    (dest (string-append doc "/share/doc")))
               (copy-recursively "doc" dest)))))))
    (native-inputs
     `(("dot" ,graphviz)
       ("doxygen" ,doxygen)
       ("python" ,python-wrapper)))
    (synopsis "Decaf Elliptic Curve Library")
    (description "The libdecaf library is an implementation of elliptic curve
cryptography using the Montgomery and Edwards curves Curve25519, Ed25519,
Ed448-Goldilocks and Curve448, using the Decaf encoding.")
    (home-page "https://ed448goldilocks.sourceforge.net/")
    (license (list license:expat        ;library
                   license:bsd-2))))    ;python bindings

(define-public libsodium
  (package
    (name "libsodium")
    (version "1.0.18")
    (source (origin
            (method url-fetch)
            (uri (list (string-append
                        "https://download.libsodium.org/libsodium/"
                        "releases/libsodium-" version ".tar.gz")
                       (string-append
                        "https://download.libsodium.org/libsodium/"
                        "releases/old/libsodium-" version ".tar.gz")))
            (sha256
             (base32
              "1h9ncvj23qbbni958knzsli8dvybcswcjbx0qjjgi922nf848l3g"))))
    (build-system gnu-build-system)
    (synopsis "Portable NaCl-based crypto library")
    (description
     "Sodium is a new easy-to-use high-speed software library for network
communication, encryption, decryption, signatures, etc.")
    (license license:isc)
    (home-page "https://libsodium.org")))

(define-public libmd
  (package
    (name "libmd")
    (version "1.0.4")
    (source (origin
            (method url-fetch)
            (uri
             (list
              (string-append "https://archive.hadrons.org/software/libmd/libmd-"
                             version ".tar.xz")
              (string-append "https://libbsd.freedesktop.org/releases/libmd-"
                             version ".tar.xz")))
            (sha256
             (base32
              "03skgv013v0y9hxh6j143xdwynb5cmbmbdylvvgfsjz38889477m"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list "--disable-static")))
    (synopsis "Message Digest functions from BSD systems")
    (description
     "The currently provided message digest algorithms are:
@itemize
@item MD2
@item MD4
@item MD5
@item RIPEMD-160
@item SHA-1
@item SHA-2 (SHA-256, SHA-384 and SHA-512)
@end itemize")
    (license (list license:bsd-3
                   license:bsd-2
                   license:isc
                   license:public-domain))
    (home-page "https://www.hadrons.org/software/libmd/")))

(define-public signify
  (package
    (name "signify")
    (version "32")
    (home-page "https://github.com/aperezdc/signify")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/aperezdc/signify/releases"
                                  "/download/v" version "/signify-" version ".tar.xz"))
              (sha256
               (base32
                "1maq4wv7934gql95rr55pj419gnbj8jkrgn1f25jcg97v5zvklbd"))
              (modules '((guix build utils)))
              (snippet '(delete-file-recursively "libbsd"))))
    (build-system gnu-build-system)
    ;; TODO Build with libwaive (described in README.md), to implement something
    ;; like OpenBSD's pledge().
    (arguments
     (list
      #:make-flags
      #~(list (string-append "CC=" #$(cc-for-target))
              (string-append "PREFIX=" #$output))
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list libbsd))
    (synopsis "Create and verify cryptographic signatures")
    (description "The signify utility creates and verifies cryptographic
signatures using the elliptic curve Ed25519.  This is a Linux port of the
OpenBSD tool of the same name.")
    ;; This package includes third-party code that was originally released under
    ;; various non-copyleft licenses. See the source files for clarification.
    (license (list license:bsd-3 license:expat license:isc
                   license:public-domain (license:non-copyleft
                                          "file://base64.c"
                                          "See base64.c in the distribution for
                                           the license from IBM.")))))

(define-public rust-minisign
  (package
    (name "rust-minisign")
    (version "0.7.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "minisign" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32 "1lmp83bxdg53c4n35fbwr3rkh6178y75fwsn25hf1kn62f2gbdnj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-getrandom" ,rust-getrandom-0.2)
        ("rust-rpassword" ,rust-rpassword-7)
        ("rust-scrypt" ,rust-scrypt-0.11))))
    (home-page "https://github.com/jedisct1/rust-minisign")
    (synopsis "Crate to sign files and verify signatures")
    (description
     "This package provides a crate to sign files and verify signatures.")
    (license license:expat)))

(define-public encfs
  (package
    (name "encfs")
    (version "1.9.5")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://github.com/vgough/encfs/releases/download/v"
                       version "/encfs-" version ".tar.gz"))
       (sha256
        (base32
         "0qzxavvv20577bxvly8s7d3y7bqasqclc2mllp0ddfncjm9z02a7"))
       (modules '((guix build utils)))
       ;; Remove bundled dependencies in favour of proper inputs.
       (snippet '(begin
                   (for-each delete-file-recursively
                             '("vendor/github.com/leethomason/tinyxml2"
                               "vendor/github.com/google/googletest"))
                   #t))))
    (build-system cmake-build-system)
    (native-inputs
     `(("gettext" ,gettext-minimal)

       ;; Test dependencies.
       ("expect" ,expect)
       ("googletest-source" ,(package-source googletest))
       ("perl" ,perl)))
    (inputs
     (list attr fuse-2 openssl-1.1 tinyxml2))
    (arguments
     `(#:configure-flags (list "-DUSE_INTERNAL_TINYXML=OFF")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'unpack-googletest
           (lambda* (#:key inputs #:allow-other-keys)
             (mkdir-p "vendor/github.com/google/googletest")
             (copy-recursively (assoc-ref inputs "googletest-source")
                               "vendor/github.com/google/googletest")
             #t))
         (add-before 'configure 'patch-CMakeLists.txt
           (lambda _
             ;; Prevent CMake from adding libc on the system include path.
             ;; Otherwise it will interfere with the libc used by GCC and
             ;; ultimately cause #include_next errors.
             (substitute* "CMakeLists.txt"
               (("include_directories \\(SYSTEM \\$\\{Intl_INCLUDE_DIRS\\}\\)")
                ""))
             #t))
         (add-before 'check 'make-unittests
           (lambda _
             (invoke "make" "unittests"))))))
    (home-page "https://vgough.github.io/encfs")
    (synopsis "Encrypted virtual file system")
    (description
     "EncFS creates a virtual encrypted file system in user-space.  Each file
created under an EncFS mount point is stored as a separate encrypted file on
the underlying file system.  Like most encrypted file systems, EncFS is meant
to provide security against off-line attacks, such as a drive falling into
the wrong hands.")
    (license (list license:expat                  ; internal/easylogging++.h
                   license:lgpl3+                 ; encfs library
                   license:gpl3+))))              ; command-line tools

(define-public keyutils
  (package
    (name "keyutils")
    (version "1.6.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (string-append "https://git.kernel.org/pub/scm/linux/kernel/"
                                 "git/dhowells/keyutils.git"))
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1095g1p5038m91wf2dxnagngpvww7ilcj8fhyviid3srvxr675i7"))
       (modules '((guix build utils)))
       ;; Create relative symbolic links instead of absolute ones to /lib/*.
       (snippet '(begin
                   (substitute* "Makefile" (("\\$\\(LNS\\) \\$\\(LIBDIR\\)/")
                                            "$(LNS) "))))))
    (build-system gnu-build-system)
    (arguments
     (list #:make-flags
           #~(list (string-append "CC=" #$(cc-for-target))
                   ;; "NO_ARLIB=1" would cleanly disable static libraries.
                   "RPATH=-Wl,-rpath,$(DESTDIR)$(LIBDIR)"
                   (string-append "DESTDIR=" #$output)
                   "INCLUDEDIR=/include"
                   "LIBDIR=/lib"
                   "MANDIR=/share/man"
                   "SHAREDIR=/share/keyutils")
           #:phases
           #~(modify-phases %standard-phases
               (delete 'configure)      ; no configure script
               (add-after 'unpack 'avoid-embedding-timestamp
                 ;; Do not embed build timestamp
                 (lambda _
                   (substitute* "Makefile"
                     (("shell date") "shell true"))))
               (add-after 'install 'install:static
                 (lambda _
                   (with-directory-excursion #$output
                     (for-each (lambda (file)
                                 (let ((target (string-append #$output:static
                                                              "/" file)))
                                   (format #t "~a -> ~a\n" file target)
                                   (mkdir-p (dirname target))
                                   (rename-file file target)))
                               (find-files "lib" "\\.a$"))))))
           #:test-target "test"))
    (inputs
     (list mit-krb5))
    (home-page "https://people.redhat.com/dhowells/keyutils/")
    (synopsis "Linux key management utilities")
    (description
     "Keyutils is a set of utilities for managing the key retention facility in
the Linux kernel, which can be used by file systems, block devices, and more to
gain and retain the authorization and encryption keys required to perform
secure operations.")
    (license (list license:lgpl2.1+             ; the files keyutils.*
                   license:gpl2+))))            ; the rest

(define-public ssss
  (package
    (name "ssss")
    (version "0.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://point-at-infinity.org/ssss/ssss-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "15grn2fp1x8p92kxkwbmsx8rz16g93y9grl3hfqbh1jn21ama5jx"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; No test suite
       #:make-flags (list (string-append "PREFIX=" (assoc-ref %outputs "out"))
                          ,(string-append "CC=" (cc-for-target)))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure) ; no configuration to be done
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((outdir (assoc-ref outputs "out"))
                    (bindir (string-append outdir "/bin"))
                    (docdir (string-append outdir
                                           "/share/doc/ssss-"
                                           ,version)))
               (install-file "ssss-combine" bindir)
               (install-file "ssss-split" bindir)
               (install-file "ssss.1" docdir)
               (install-file "ssss.1.html" docdir)
               #t))))))
    (inputs
     (list gmp))
    (native-inputs
     (list xmltoman))
    (home-page "http://point-at-infinity.org/ssss/")
    (synopsis "Shamir's secret sharing scheme implementation")
    (description "@command{ssss-split} and @command{ssss-combine} are utilities that split
and combine secrets securely using Shamir's secret sharing scheme.  This implementation
allows for a threshold scheme where the minimum number of shares can be less than the
total number of shares generated.")
    (license license:gpl2+)))

(define-public tomb
  (package
    (name "tomb")
    (version "2.11")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://files.dyne.org/tomb/releases/"
                           "Tomb-" version ".tar.gz"))
       (sha256
        (base32 "05f34yx91bn9fj7rkabgpzvkw4pa6bg2c1r8cnp72wwnx6bzj97m"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:make-flags #~(list (string-append "PREFIX=" #$output))
      ;; TODO: Build and install gtk and qt trays
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)            ;no configuration to be done
          (add-after 'install 'i18n
            (lambda* (#:key make-flags #:allow-other-keys)
              (apply invoke "make" "-C" "extras/translations"
                     "install" make-flags)))
          (add-after 'install 'cloak
            (lambda* (#:key make-flags #:allow-other-keys)
              (apply invoke "make" "-C" "extras/cloak" "install" make-flags)
              (copy-recursively
               "extras/cloak/ciphers"
               (string-append #$output "/share/tomb/extras/cloak/ciphers"))))
          (add-after 'install 'gtomb
            (lambda _
              (install-file "extras/gtomb/gtomb"
                            (string-append #$output "/bin"))))
          (add-after 'install 'wrap
            (lambda* (#:key inputs #:allow-other-keys)
              (wrap-program (string-append #$output "/bin/tomb")
                `("PATH" ":" prefix
                  (,(string-append (assoc-ref inputs "mlocate") "/bin")
                   ,@(map (lambda (program)
                            (or (and=> (which program) dirname)
                                (error "program not found:" program)))
                          ;; The size of this package explodes with:
                          ;; "unoconv" "recoll"
                          '("argon2" "seq" "mkfs.ext4" "pinentry"
                            "gpg" "cryptsetup" "gettext" "lsof"
                            "qrencode" "steghide" "findmnt" "getent")))))))
          (delete 'check)
          (add-after 'wrap 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                ;; Running the full tests requires sudo/root access for
                ;; cryptsetup, which is not available in the build
                ;; environment.  But we can run `tomb dig` without root, so
                ;; make sure that works.
                ;;
                ;; TODO: It Would Be Nice to check the expected "index",
                ;; "search", "bury", and "exhume" features are available by
                ;; querying `tomb -h`.
                (let ((tomb (string-append #$output "/bin/tomb")))
                  (invoke tomb "dig" "-s" "10" "secrets.tomb"))))))))
    (native-inputs
     (list which))
    (inputs
     (list argon2
           bash-minimal
           cryptsetup-minimal
           e2fsprogs        ; for mkfs.ext4
           gettext-minimal  ; used at runtime
           gnupg
           lsof
           mlocate
           pinentry
           qrencode
           ;; recoll        ; size explodes
           steghide
           ;; unoconv       ; size explodes
           util-linux
           zsh))
    (home-page "https://dyne.org/tomb/")
    (synopsis "File encryption for secret data")
    (description
     "Tomb is an application to manage the creation and access of encrypted
storage files: it can be operated from commandline and it can integrate with a
user's graphical desktop.")
    (license license:gpl3+)))

(define-public scrypt
  (package
    (name "scrypt")
    (version "1.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://www.tarsnap.com/scrypt/scrypt-"
                            version ".tgz"))
        (sha256
         (base32
          "1hnl0r6pmyxiy4dmafmqk1db7wpc0x9rqpzqcwr9d2cmghcj6byz"))))
    (build-system gnu-build-system)
    (arguments
     `(#:license-file-regexp "COPYRIGHT"
       #:phases (modify-phases %standard-phases
        (add-after 'unpack 'patch-$PATH-assumptions
          (lambda _
            (substitute* "configure"
              (("\\{POSIX_PATH\\}")
               "{PATH}"))
            (substitute* "Makefile.in"
              (("command -p") ""))
            #t))
        (add-after 'install 'install-docs
          (lambda* (#:key outputs #:allow-other-keys)
            (let* ((out (assoc-ref %outputs "out"))
                   (doc (string-append out "/share/doc/" ,name "-" ,version)))
              (install-file "FORMAT" doc)
              #t))))))
    (inputs
     (list openssl))
    (home-page "https://www.tarsnap.com/scrypt.html")
    (synopsis "Memory-hard encryption tool based on scrypt")
    (description "This package provides a simple password-based encryption
utility as a demonstration of the @code{scrypt} key derivation function.
@code{Scrypt} is designed to be far more resistant against hardware brute-force
attacks than alternative functions such as @code{PBKDF2} or @code{bcrypt}.")
    (license license:bsd-2)))

(define-public libscrypt
  (package
    (name "libscrypt")
    (version "1.22")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/technion/libscrypt")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "10dinz1zx8zfm81ra16s20izpm7f7j414n4i3fkdf40vbl5slra1"))))
    (build-system gnu-build-system)
    (outputs (list "out" "static"))
    (arguments
     (list #:make-flags
           #~(list (string-append "PREFIX=" #$output)
                   (string-append "CC=" #$(cc-for-target)))
           #:phases
           #~(modify-phases %standard-phases
               (delete 'configure)      ; no configure script
               (add-after 'install 'install:static
                 (lambda _
                   (install-file "libscrypt.a"
                                 (string-append #$output:static "/lib")))))))
    (home-page "https://lolware.net/libscrypt.html")
    (synopsis "Password hashing library")
    (description "@code{libscrypt} implements @code{scrypt} key derivation
function.  It is designed to be far more secure against hardware brute-force
attacks than alternative functions such as @code{PBKDF2} or @code{bcrypt}.")
    (license license:bsd-3)))

(define-public perl-math-random-isaac-xs
  (package
    (name "perl-math-random-isaac-xs")
    (version "1.004")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/J/JA/JAWNSY/"
                           "Math-Random-ISAAC-XS-" version ".tar.gz"))
       (sha256
        (base32
         "0yxqqcqvj51fn7b7j5xqhz65v74arzgainn66c6k7inijbmr1xws"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-module-build perl-test-nowarnings))
    (home-page "https://metacpan.org/release/Math-Random-ISAAC-XS")
    (synopsis "C implementation of the ISAAC PRNG algorithm")
    (description "ISAAC (Indirection, Shift, Accumulate, Add, and Count) is a
fast pseudo-random number generator.  It is suitable for applications where a
significant amount of random data needs to be produced quickly, such as
solving using the Monte Carlo method or for games.  The results are uniformly
distributed, unbiased, and unpredictable unless you know the seed.

This package implements the same interface as @code{Math::Random::ISAAC}.")
    (license license:public-domain)))

(define-public perl-math-random-isaac
  (package
    (name "perl-math-random-isaac")
    (version "1.004")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/J/JA/JAWNSY/"
                           "Math-Random-ISAAC-" version ".tar.gz"))
       (sha256
        (base32
         "0z1b3xbb3xz71h25fg6jgsccra7migq7s0vawx2rfzi0pwpz0wr7"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-test-nowarnings))
    (propagated-inputs
     (list perl-math-random-isaac-xs))
    (home-page "https://metacpan.org/release/Math-Random-ISAAC")
    (synopsis "Perl interface to the ISAAC PRNG algorithm")
    (description "ISAAC (Indirection, Shift, Accumulate, Add, and Count) is a
fast pseudo-random number generator.  It is suitable for applications where a
significant amount of random data needs to be produced quickly, such as
solving using the Monte Carlo method or for games.  The results are uniformly
distributed, unbiased, and unpredictable unless you know the seed.

This package provides a Perl interface to the ISAAC pseudo random number
generator.")
    (license license:public-domain)))

(define-public perl-crypt-random-source
  (package
    (name "perl-crypt-random-source")
    (version "0.14")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/E/ET/ETHER/"
                           "Crypt-Random-Source-" version ".tar.gz"))
       (sha256
        (base32 "1rpdds3sy5l1fhngnkrsgwsmwd54wpicx3i9ds69blcskwkcwkpc"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-module-build-tiny perl-test-fatal))
    (propagated-inputs
     (list perl-capture-tiny
           perl-module-find
           perl-module-runtime
           perl-moo
           perl-namespace-clean
           perl-sub-exporter
           perl-type-tiny))
    (home-page "https://metacpan.org/release/Crypt-Random-Source")
    (synopsis "Get weak or strong random data from pluggable sources")
    (description "This module provides implementations for a number of
byte-oriented sources of random data.")
    (license license:perl-license)))

(define-public perl-math-random-secure
  (package
    (name "perl-math-random-secure")
    (version "0.080001")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/F/FR/FREW/"
                           "Math-Random-Secure-" version ".tar.gz"))
       (sha256
        (base32
         "0dgbf4ncll4kmgkyb9fsaxn0vf2smc9dmwqzgh3259zc2zla995z"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-list-moreutils perl-test-leaktrace perl-test-sharedfork
           perl-test-warn))
    (inputs
     (list perl-crypt-random-source perl-math-random-isaac
           perl-math-random-isaac-xs perl-moo))
    (home-page "https://metacpan.org/release/Math-Random-Secure")
    (synopsis "Cryptographically secure replacement for rand()")
    (description "This module is intended to provide a
cryptographically-secure replacement for Perl's built-in @code{rand} function.
\"Cryptographically secure\", in this case, means:

@enumerate
@item No matter how many numbers you see generated by the random number
generator, you cannot guess the future numbers, and you cannot guess the seed.
@item There are so many possible seeds that it would take decades, centuries,
or millennia for an attacker to try them all.
@item The seed comes from a source that generates relatively strong random
data on your platform, so the seed itself will be as random as possible.
@end enumerate\n")
    (license license:artistic2.0)))

(define-public crypto++
  (package
    (name "crypto++")
    (version "8.8.0")
    (source (origin
              (method git-fetch)
              (uri
               (git-reference
                (url "https://github.com/weidai11/cryptopp")
                (commit
                 (string-append "CRYPTOPP_"
                                (string-replace-substring version "." "_")))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "11gfnsqbb531zwgzpm0x9hsgshzcj1j049vg0zqsaqf8lvky03l6"))))
    (build-system gnu-build-system)
    (arguments
     (list #:make-flags
           #~(list (string-append "PREFIX=" #$output)
                   (string-append "CC=" #$(cc-for-target))
                   (string-append "CXX=" #$(cxx-for-target))
                   (string-append "AR=" #$(ar-for-target))
                   ;; Override "/sbin/ldconfig" with simply "echo" since
                   ;; we don't need ldconfig(8).
                   "LDCONF=echo")
           #:phases
           #~(modify-phases %standard-phases
               (delete 'configure)
               (replace 'build
                 ;; By default, only the static library is built.
                 (lambda* (#:key make-flags parallel-build?
                           #:allow-other-keys)
                   (let* ((job-count (if parallel-build?
                                         (number->string (parallel-job-count))
                                         1))
                          (jobs (string-append "-j" job-count))
                          (target #$(if (target-mingw?)
                                        "static"
                                        "shared")))
                     (apply invoke "make" target jobs make-flags)
                     (apply invoke "make" "libcryptopp.pc" jobs
                            make-flags)))))))
    (properties '((tunable? . #t)))
    (native-inputs
     (list unzip))
    (home-page "https://cryptopp.com/")
    (synopsis "C++ class library of cryptographic schemes")
    (description "Crypto++ is a C++ class library of cryptographic schemes.")
    ;; The compilation is distributed under the Boost license; the individual
    ;; files in the compilation are in the public domain.
    (license (list license:boost1.0 license:public-domain))))

(define-public libb2
  (package
    (name "libb2")
    (version "0.98.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/BLAKE2/libb2/releases/download/v"
                    version "/libb2-" version ".tar.gz"))
              (sha256
               (base32
                "0bn7yrzdixdvzm46shbhpkqbr6zyqyxiqn7a7x54ag3mrvfnyqjk"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list
        ,@(if (target-x86?)
              ;; fat only checks for Intel optimisations
              '("--enable-fat")
              '())
        "--disable-native")))           ;don't optimise at build time
    (home-page "https://blake2.net/")
    (synopsis "Library implementing the BLAKE2 family of hash functions")
    (description
     "libb2 is a portable implementation of the BLAKE2 family of cryptographic
hash functions.  It includes optimised implementations for IA-32 and AMD64
processors, and an interface layer that automatically selects the best
implementation for the processor it is run on.

@dfn{BLAKE2} (RFC 7693) is a family of high-speed cryptographic hash functions
that are faster than MD5, SHA-1, SHA-2, and SHA-3, yet are at least as secure
as the latest standard, SHA-3.  It is an improved version of the SHA-3 finalist
BLAKE.")
    (license license:public-domain)))

(define-public b2sum
  ;; Upstream doesn't seem to use a versioned release workflow, so build from
  ;; a recent commit.
  (let ((commit "54f4faa4c16ea34bcd59d16e8da46a64b259fc07")
        (revision "0"))
    (package
      (name "b2sum")
      (version (git-version "20190724" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/BLAKE2/BLAKE2")
                       (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32 "04z631v0vzl52g73v390ask5fnzi5wg83lcjkjhpmmymaz0jn152"))))
      ;; "This code requires at least SSE2".
      (supported-systems '("x86_64-linux"))
      (build-system gnu-build-system)
      (arguments
       `(#:make-flags (list (string-append "CC=" ,(cc-for-target))
                            (string-append "PREFIX=" (assoc-ref %outputs "out")))
         #:tests? #f ; No test suite
         #:phases
         (modify-phases %standard-phases
           (add-before 'build 'change-directory
                       (lambda _
                         (chdir "b2sum")))
           ;; Produce generic binaries
           (add-after 'change-directory 'de-tune
                       (lambda _
                         (substitute* "makefile"
                           ((" -march=native") ""))))
           (delete 'configure)))) ; No ./configure script
      (home-page "https://www.blake2.net/")
      (synopsis "BLAKE2 checksum tool")
      (description "BLAKE2 is a cryptographic hash function faster than MD5,
SHA-1, SHA-2, and SHA-3, yet is at least as secure as SHA-3.")
      ;; You may also choose to redistribute this program as Apache 2.0 or the
      ;; OpenSSL license. See 'b2sum/b2sum.c' in the source distribution.
      (license license:cc0)
      ;; There is a significant speedup when the compiler generates
      ;; instructions tuned to the CPU of the running machine:
      ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=51198#22
      (properties '((tunable? . #true))))))

(define-public rhash
  (package
    (name "rhash")
    (version "1.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/rhash/rhash/" version
                           "/rhash-" version "-src.tar.gz"))
       (file-name (string-append "rhash-" version ".tar.gz"))
       (sha256
        (base32
         "0glaghjvwh9ziiqf599v0fdr6jrgc7lcnriq0h7r41k3jrkglh0y"))))
    (build-system gnu-build-system)
    (arguments
     (list #:configure-flags
           #~(list (string-append "--prefix=" #$output)
                   #$@(let ((target (%current-target-system)))
                        (if target
                            #~((string-append "--target=" #$target)
                               (string-append "--cc="
                                              (assoc-ref %build-inputs "cross-gcc")
                                              "/bin/" #$target "-gcc"))
                            #~())))
           #:make-flags
           ;; The binaries in /bin need some help finding librhash.so.0.
           #~(list (string-append "LDFLAGS=-Wl,-rpath=" #$output "/lib"))
           #:test-target "test"         ; ‘make check’ just checks the sources
           #:phases
           #~(modify-phases %standard-phases
              (delete 'configure)
              (add-before 'build 'configure
                ;; ./configure is not GNU autotools' and doesn't gracefully handle
                ;; unrecognized options, so we must call it manually.
                (lambda* (#:key configure-flags #:allow-other-keys)
                  (apply invoke "./configure" configure-flags)))
              (add-before 'check 'patch-/bin/sh
                (lambda _
                  (substitute* "Makefile"
                    (("/bin/sh") (which "sh")))))
              (add-after 'install 'install-library-extras
                (lambda* (#:key make-flags #:allow-other-keys)
                  (apply invoke
                         "make" "-C" "librhash"
                         "install-lib-headers" "install-so-link"
                         make-flags))))))
    (home-page "https://sourceforge.net/projects/rhash/")
    (synopsis "Utility for computing hash sums")
    (description "RHash is a console utility for calculation and verification
of magnet links and a wide range of hash sums like CRC32, MD4, MD5, SHA1,
SHA256, SHA512, SHA3, AICH, ED2K, Tiger, DC++ TTH, BitTorrent BTIH, GOST R
34.11-94, RIPEMD-160, HAS-160, EDON-R, Whirlpool and Snefru.")
    (license (license:non-copyleft "file://COPYING"))))

(define-public botan
  (package
    (name "botan")
    (version "2.19.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://botan.randombit.net/releases/"
                                  "Botan-" version ".tar.xz"))
              (sha256
               (base32
                "0m9dh00zibx13pbjij8lbncf86pix3cxklxmgl47z965k7rlgq6s"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (replace 'configure
            (lambda* (#:key inputs outputs #:allow-other-keys)
              ;; Upstream tests and benchmarks with -O3.
              (setenv "CXXFLAGS" "-O3")
              (invoke "python" "./configure.py"
                      (string-append "--prefix=" #$output)
                      "--disable-static"

                      ;; Otherwise, the `botan` executable cannot find
                      ;; libbotan.
                      (string-append "--ldflags=-Wl,-rpath=" #$output "/lib")

                      "--with-os-feature=getentropy"
                      "--with-rst2man"

                      ;; Recommended by upstream
                      "--with-zlib" "--with-bzip2" "--with-sqlite3")))
          (add-before 'check 'library-path-for-tests
            (lambda _ (setenv "LD_LIBRARY_PATH" (getcwd))))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "./botan-test")))))))
    (native-inputs (list python-wrapper python-docutils))
    (inputs (list sqlite bzip2 zlib))
    (synopsis "Cryptographic library in C++11")
    (description "Botan is a cryptography library, written in C++11, offering
the tools necessary to implement a range of practical systems, such as TLS/DTLS,
PKIX certificate handling, PKCS#11 and TPM hardware support, password hashing,
and post-quantum crypto schemes.  In addition to the C++, botan has a C89 API
specifically designed to be easy to call from other languages.  A Python binding
using ctypes is included, and several other language bindings are available.")
    (home-page "https://botan.randombit.net")
    (license license:bsd-2)))

(define-public ccrypt
  (package
    (name "ccrypt")
    (version "1.11")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/ccrypt/"
                                  version "/ccrypt-" version ".tar.gz"))
              (sha256
               (base32
                "0kx4a5mhmp73ljknl2lcccmw9z3f5y8lqw0ghaymzvln1984g75i"))))
    (build-system gnu-build-system)
    (home-page "https://ccrypt.sourceforge.net")
    (synopsis "Command-line utility for encrypting and decrypting files and streams")
    (description "@command{ccrypt} is a utility for encrypting and decrypting
files and streams.  It was designed as a replacement for the standard unix
@command{crypt} utility, which is notorious for using a very weak encryption
algorithm.  @command{ccrypt} is based on the Rijndael block cipher, a version of
which is also used in the Advanced Encryption Standard (AES, see
@url{http://www.nist.gov/aes}).  This cipher is believed to provide very strong
security.")
    (license license:gpl2)))

(define-public asignify
  (let ((commit "08af003e1f4833713db28b871759d94f9b2b1469")
        (revision "1"))
    (package
      (name "asignify")
      (version (git-version "1.1" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/vstakhov/asignify")
                       (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1zacpqa8b5lg270z1g06r5ik9vnb91crb4ivyy20381dny82xvr1"))))
      (build-system gnu-build-system)
      (arguments
       `(#:configure-flags
         (list "--enable-openssl"
               (string-append "--with-openssl="
                              (assoc-ref %build-inputs "openssl")))))
      (native-inputs
       (list autoconf automake libtool))
      (inputs
       (list openssl))
      (home-page "https://github.com/vstakhov/asignify")
      (synopsis "Cryptographic authentication and encryption tool and library")
      (description "Asignify offers public cryptographic signatures and
encryption with a library or a command-line tool.  The tool is heavily inspired
by signify as used in OpenBSD.  The main goal of this project is to define a
high level API for signing files, validating signatures and encrypting using
public-key cryptography.  Asignify is designed to be portable and self-contained
with zero external dependencies.  Asignify can verify OpenBSD signatures, but it
cannot sign messages in OpenBSD format yet.")
      (license license:bsd-2))))

(define-public enchive
  (package
    (name "enchive")
    (version "3.5")
    (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/skeeto/enchive")
                      (commit version)))
                (sha256
                 (base32
                  "0fdrfc5l42lj2bvmv9dmkmhmm7qiszwk7cmdvnqad3fs7652g0qa"))
                (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no check target         '
       #:make-flags (list ,(string-append "CC=" (cc-for-target))
                          "PREFIX=$(out)")
       #:phases (modify-phases %standard-phases
                  (delete 'configure)
                  (add-after 'install 'post-install
                    (lambda _
                      (let* ((out (assoc-ref %outputs "out"))
                             (lisp (string-append out "/share/emacs/site-lisp")))
                        (install-file "enchive-mode.el" lisp)
                        #t))))))
    (synopsis "Encrypted personal archives")
    (description
     "Enchive is a tool to encrypt files to yourself for long-term
archival.  It's a focused, simple alternative to more complex solutions such as
GnuPG or encrypted filesystems.  Enchive has no external dependencies and is
trivial to build for local use.  Portability is emphasized over performance.")
    (home-page "https://github.com/skeeto/enchive")
    (license license:unlicense)))

(define-public libsecp256k1
  (package
    (name "libsecp256k1")
    (version "0.6.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/bitcoin-core/secp256k1")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0v9xa9vbr18c4kxflp718h5cilkgpwrfyrl65pjxzj6g598qs954"))
              (modules '((guix build utils)))
              (snippet
               ;; These files are pre-generated, the build system is able to
               ;; re-generate those.
               #~(for-each delete-file '("src/precomputed_ecmult.c"
                                         "src/precomputed_ecmult_gen.c")))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--enable-module-recovery"
                           "--enable-experimental"
                           "--enable-shared"
                           "--disable-static"
                           "--disable-benchmark")))
    (native-inputs
     (list autoconf automake libtool))
    (synopsis "C library for EC operations on curve secp256k1")
    (description
     "Optimized C library for EC operations on curve secp256k1.

Features:

@itemize
@item secp256k1 ECDSA signing/verification and key generation.
@item Additive and multiplicative tweaking of secret/public keys.
@item Serialization/parsing of private keys, public keys, signatures.
@item Constant time, constant memory access signing and public key generation.
@item Derandomized ECDSA (via RFC6979 or with a caller provided function.)
@item Very efficient implementation.
@item Suitable for embedded systems.
@item No runtime dependencies.
@item Optional module for public key recovery.
@item Optional module for ECDH key exchange.
@item Optional module for Schnorr signatures according to BIP-340.
@end itemize\n")
    (home-page "https://github.com/bitcoin-core/secp256k1")
    (license license:expat)))

(define-public libsecp256k1-bitcoin-cash
  (package
    (name "libsecp256k1-bitcoin-cash")
    (version "0.22.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Bitcoin-ABC/secp256k1")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1rnif3iny6pz1r3g69bagzr342mm3x0v66b60csnmm1rg44bd5v1"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake libtool))
    (arguments
     '(#:configure-flags '("--enable-module-recovery"
                           "--enable-experimental"
                           "--enable-module-ecdh"
                           "--disable-jni"
                           "--with-bignum=no"
                           "--enable-module-schnorr"
                           "--disable-static"
                           "--enable-shared")))
    (synopsis "Optimized C library for EC operations on curve secp256k1")
    (description
     "Optimized C library for cryptographic operations on curve secp256k1.

This library is used for consensus critical cryptographic operations on the
Bitcoin Cash network.

Features:

@itemize
@item secp256k1 ECDSA signing/verification and key generation.
@item secp256k1 Schnorr signing/verification (Bitcoin Cash Schnorr variant).
@item Additive and multiplicative tweaking of secret/public keys.
@item Serialization/parsing of secret keys, public keys, signatures.
@item Constant time, constant memory access signing and pubkey generation.
@item Derandomized ECDSA (via RFC6979 or with a caller provided function).
@item Very efficient implementation.
@item Suitable for embedded systems.
@item Optional module for public key recovery.
@item Optional module for ECDH key exchange (experimental).
@item Optional module for multiset hash (experimental).
@end itemize\n")
    (home-page "https://github.com/Bitcoin-ABC/secp256k1")
    (license license:expat)))

(define-public stoken
  (package
    (name "stoken")
    (version "0.92")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/stoken/"
                                  "stoken-" version ".tar.gz"))
              (sha256
               (base32
                "0npgr6y85gzwksy8jkwa4yzvqwjprwnplx3yiw3ayk4f0ldlhaxa"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list nettle libxml2))
    (home-page "https://stoken.sf.net")
    (synopsis "Software Token for cryptographic authentication")
    (description
     "@code{stoken} is a token code generator compatible with RSA SecurID
128-bit (AES) tokens.  This package contains a standalone command-line program
that allows for importing token seeds, generating token codes, and various
utility/testing functions.")
    (license license:lgpl2.1+)))

(define-public hpenc
  (package
    (name "hpenc")
    (version "3.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/vstakhov/hpenc")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1fb5yi3d2k8kd4zm7liiqagpz610y168xrr1cvn7cbq314jm2my1"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; No test suite
       #:make-flags
       (list (string-append "PREFIX=" (assoc-ref %outputs "out"))
             ;; Build the program and the docs.
             "SUBDIRS=src doc")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure) ; No ./configure script
         (add-after 'unpack 'patch-path
           (lambda _
             (substitute* '("src/Makefile" "doc/Makefile")
               (("/usr/bin/install")
                "install"))))
         (add-before 'install 'make-output-directories
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (man1 (string-append out "/share/man/man1")))
               (mkdir-p bin)
               (mkdir-p man1)
               #t))))))
    (inputs
     (list libsodium openssl))
    (synopsis "High-performance command-line tool for stream encryption")
    (description "Hpenc is a command-line tool for performing authenticated
encryption (AES-GCM and ChaCha20-Poly1305) of streaming data.  It does not
perform an asymmetric key exchange, instead requiring the user to distribute
pre-shared keys out of band.  It is designed to handle large amounts of data
quickly by using all your CPU cores and hardware acceleration.")
    (home-page "https://github.com/vstakhov/hpenc")
    (license license:bsd-3)))

(define-public minisign
  (package
    (name "minisign")
    (version "0.11")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jedisct1/minisign")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1vv2bhhsyhlpnjclfa7gkqgd9xi3jfcdrss7abbdxvvflyrwdk5i"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f))                    ; no test suite
    (native-inputs
     (list pkg-config))
    (inputs
     (list libsodium))
    (home-page "https://jedisct1.github.io/minisign")
    (synopsis "Tool to sign files and verify signatures")
    (description
     "Minisign is a dead simple tool to sign files and verify signatures.  It is
portable, lightweight, and uses the highly secure Ed25519 public-key signature
system.  Signature written by minisign can be verified using OpenBSD's
signify tool: public key files and signature files are compatible.  However,
minisign uses a slightly different format to store secret keys.  Minisign
signatures include trusted comments in addition to untrusted comments.
Trusted comments are signed, thus verified, before being displayed.")
    (license license:isc)))

(define-public olm
  (package
    (name "olm")
    (version "3.2.14")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.matrix.org/matrix-org/olm")
                    (commit version)))
              (sha256
               (base32
                "0pj7gs32ixhlls792wah7xf49j5pra0avp7dpvy9cndkdkg6biq5"))
              (file-name (git-file-name name version))
              ;; Delete the bundled blob.  It's free, but unauditable,
              ;; and apparently only required for android.
              (snippet '(delete-file
                         "android/gradle/wrapper/gradle-wrapper.jar"))))
    (build-system cmake-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (with-directory-excursion "tests"
                 (invoke "ctest" "."))))))))
    (synopsis "Implementation of the Olm and Megolm cryptographic ratchets")
    (description "The Olm library implements the Double Ratchet
cryptographic ratchet.  It is written in C and C++11, and exposed as a C
API.")
    (home-page "https://matrix.org/docs/projects/other/olm/")
    (license license:asl2.0)))

(define-public libolm
  (deprecated-package "libolm" olm))

(define-public python-olm
  (package
    ;; python-olm is part of libolm and must be updated at the same time.
    (inherit libolm)
    (name "python-olm")
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _
             (chdir "python")))
         (add-before 'build 'set-preprocessor
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "CPP" "gcc -E")))
         (replace 'check
           (lambda* (#:key tests? inputs outputs #:allow-other-keys)
             (when tests?
               (add-installed-pythonpath inputs outputs)
               (invoke "pytest")))))))
    (inputs (list libolm))
    (propagated-inputs
     (list python-cffi python-future))
    (native-inputs
     (list python-pytest python-pytest-benchmark python-aspectlib))
    (synopsis "Python bindings for Olm")
    (description "The Olm library implements the Double Ratchet
cryptographic ratchet.  This package contains its Python bindings.")))

(define-public hash-extender
  (let ((commit "cb8aaee49f93e9c0d2f03eb3cafb429c9eed723d")
        (revision "2"))
    (package
      (name "hash-extender")
      (version (git-version "0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/iagox86/hash_extender")
                      (commit commit)))
                (sha256
                 (base32
                  "1fj118566hr1wv03az2w0iqknazsqqkak0mvlcvwpgr6midjqi9b"))
                (file-name (git-file-name name version))))
      (build-system gnu-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (replace 'check
             (lambda _
               (invoke "./hash_extender_test")))
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((outdir (assoc-ref outputs "out"))
                      (bindir (string-append outdir "/bin"))
                      (docdir (string-append outdir
                                             "/share/doc/hash-extender-"
                                             ,version)))
                 (install-file "hash_extender" bindir)
                 (install-file "README.md" docdir)
                 #t))))))
      (inputs
       (list openssl))
      (synopsis "Tool for hash length extension attacks")
      (description "@command{hash_extender} is a utility for performing hash
length extension attacks supporting MD4, MD5, RIPEMD-160, SHA-0, SHA-1,
SHA-256, SHA-512, and WHIRLPOOL hashes.")
      (home-page "https://github.com/iagox86/hash_extender")
      (license license:bsd-3))))

(define-public mkp224o
  (package
    (name "mkp224o")
    (version "1.7.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/cathugger/mkp224o")
                    (commit (string-append "v" version))))
              (sha256
               (base32
                "1d0mrp936if1zi8ss0mmywglsmrl0jx42x2sgnm56js8ij3g3g9q"))
              (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no test suite
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'set-cflags
           (lambda _
             (setenv "CFLAGS" "-O3")))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((outdir (assoc-ref outputs "out"))
                    (bindir (string-append outdir "/bin")))
               (install-file "mkp224o" bindir)
               #t))))))
    (native-inputs
     (list autoconf))
    (inputs
     (list libsodium))
    (synopsis "Tor hidden service v3 name generator")
    (description "@code{mkp224o} generates valid ed25519 (hidden service
version 3) onion addresses.  It allows one to produce customized vanity .onion
addresses using a brute-force method.")
    (home-page "https://github.com/cathugger/mkp224o")
    (license license:cc0)))

(define-public transcrypt
  (package
    (name "transcrypt")
    (version "2.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/elasticdog/transcrypt")
             (commit (string-append "v" version))))
       (sha256
        (base32 "0bpz1hazbhfb6pqi68x55kq6a31bgh6vwij836slmi4jqiwvnh5a"))
       (file-name (git-file-name name version))))
    (inputs
     (list git openssl))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan
       '(("transcrypt" "bin/transcrypt")
         ("man/transcrypt.1" "share/man/man1/transcrypt.1")
         ("contrib/bash/transcrypt"
          "share/bash-completion/completions/transcrypt")
         ("contrib/zsh/_transcrypt"
          "share/zsh/site-functions/_transcrypt"))))
    (home-page "https://github.com/elasticdog/transcrypt")
    (synopsis "Transparently encrypt files within a git repository")
    (description
     "Transcrypt is a script to configure transparent encryption of sensitive
files stored in a Git repository.  Files that you choose will be automatically
encrypted when you commit them, and automatically decrypted when you check
them out.  The process will degrade gracefully, so even people without your
encryption password can safely commit changes to the repository's
non-encrypted files.")
    (license license:expat)))

(define-public cryfs
  (package
    (name "cryfs")
    (version "0.11.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/cryfs/cryfs/releases/download/"
             version "/cryfs-" version ".tar.xz"))
       (sha256
        (base32 "0a48qijfrd02ianp19x3kz24w1pgigmlxdr5nks0gag7z5b2s7m7"))))
    (build-system cmake-build-system)
    (arguments
     '(#:modules ((guix build cmake-build-system)
                  (guix build utils)
                  (srfi srfi-1))
       #:configure-flags
        ;; Note: This also disables checking for security issues.
       `("-DCRYFS_UPDATE_CHECKS=OFF"
         ;; This helps us use some dependencies from Guix instead of conan.
         ;; crypto++ is still bundled: https://github.com/cryfs/cryfs/issues/369
         ;; Googletest is also since I wasn't sure how to unbundle that.
         ,(string-append "-DDEPENDENCY_CONFIG=" (getcwd)
                         "/cmake-utils/DependenciesFromLocalSystem.cmake"))
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'fix-up
           (lambda* (#:key tests? #:allow-other-keys)
             ;; Remove junk directory that breaks the build
             (chdir "..") (delete-file-recursively ".circleci")
             ;; Install documentation with Guix defaults.
             (substitute* "doc/CMakeLists.txt"
               (("CONFIGURATIONS Release")
                "CONFIGURATIONS Release RelWithDebInfo"))
             (when tests?
               (substitute* "CMakeLists.txt"
                 (("option.BUILD_TESTING .build test cases. OFF.")
                  "option(BUILD_TESTING \"build test cases\" ON)")))))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (let ((tests (find-files "." "-test$")))
                 ;; XXX: Disable failing tests. Unfortunately there are a
                 ;; few. Some only fail in the build environment due to
                 ;; FUSE not being available.
                 (for-each invoke
                           (lset-difference string-contains
                                            tests
                                            '("cpp-utils-test"
                                              "cryfs-cli-test"
                                              "blobstore-test"
                                              "fspp-test"))))))))))
    (native-inputs
     (list pkg-config python-wrapper))
    (inputs
     (list boost curl fuse-2 range-v3 spdlog))
    (home-page "https://www.cryfs.org/")
    (synopsis "Encrypted FUSE filesystem for the cloud")
    (description "CryFS encrypts your files, so you can safely store them anywhere.
It works well together with cloud services like Dropbox, iCloud, OneDrive and
others.  CryFS creates an encrypted userspace filesystem that can be mounted
via FUSE without root permissions.  It is similar to EncFS, but provides
additional security and privacy measures such as hiding file sizes and directory
structure.  However CryFS is not considered stable yet by the developers.")
    (license license:lgpl3+)))

(define-public rust-blake3-0.3
  (package
    (name "rust-blake3")
    (version "0.3.8")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "blake3" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1cr5l5szgxm632px41kavl6cgils8h6yhdfkm6jsc5jgiivqai5n"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build? #t
        #:cargo-inputs
        (("rust-arrayref" ,rust-arrayref-0.3)
         ("rust-arrayvec" ,rust-arrayvec-0.5)
         ("rust-cc" ,rust-cc-1)
         ("rust-cfg-if" ,rust-cfg-if-0.1)
         ("rust-constant-time-eq" ,rust-constant-time-eq-0.1)
         ("rust-crypto-mac" ,rust-crypto-mac-0.8)
         ("rust-digest" ,rust-digest-0.9)
         ("rust-rayon" ,rust-rayon-1))))
    (home-page "https://github.com/BLAKE3-team/BLAKE3")
    (synopsis "BLAKE3 hash function Rust implementation")
    (description "This crate provides the official Rust implementation of the
BLAKE3 cryptographic hash function.  BLAKE3 is faster than MD5, SHA-1, SHA-2,
SHA-3, and BLAKE2.")
    ;; Users may choose between these two licenses when redistributing the
    ;; program provided by this package.
    (license (list license:cc0 license:asl2.0))))

(define-public rust-blake3-1
  (package
    (name "rust-blake3")
    (version "1.5.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "blake3" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "07k07q7f2m0hr6z944gf0wn1s15f3gwsydhpz2ssbpn44hc0rvmq"))))
    (build-system cargo-build-system)
    (arguments
     (list
      #:tests? #f       ; use of undeclared crate or module `reference_impl`
      #:cargo-inputs
      `(("rust-arrayref" ,rust-arrayref-0.3)
        ("rust-arrayvec" ,rust-arrayvec-0.7)
        ("rust-cc" ,rust-cc-1)
        ("rust-cfg-if" ,rust-cfg-if-1)
        ("rust-constant-time-eq" ,rust-constant-time-eq-0.3)
        ("rust-digest" ,rust-digest-0.10)
        ("rust-memmap2" ,rust-memmap2-0.9)
        ("rust-rayon-core" ,rust-rayon-core-1)
        ("rust-serde" ,rust-serde-1)
        ("rust-zeroize" ,rust-zeroize-1))
      #:cargo-development-inputs
      `(("rust-ciborium" ,rust-ciborium-0.2)
        ("rust-hex" ,rust-hex-0.4)
        ("rust-hmac" ,rust-hmac-0.12)
        ("rust-page-size" ,rust-page-size-0.6)
        ("rust-rand" ,rust-rand-0.8)
        ("rust-rand-chacha" ,rust-rand-chacha-0.3)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-tempfile" ,rust-tempfile-3))))
    (home-page "https://github.com/BLAKE3-team/BLAKE3")
    (synopsis "BLAKE3 hash function Rust implementation")
    (description "This crate provides the official Rust implementation of the
BLAKE3 cryptographic hash function.  BLAKE3 is faster than MD5, SHA-1, SHA-2,
SHA-3, and BLAKE2.")
    ;; Users may choose between these two licenses when redistributing the
    ;; program provided by this package.
    (license (list license:cc0 license:asl2.0))))

(define-public b3sum
  (package
    (name "b3sum")
    (version "1.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "b3sum" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32 "05k0vn7gpbvjr925vjc5yzvhiyrmkw9pqmch5fr4ir7s8wiaq2fm"))))
    (build-system cargo-build-system)
    (arguments
      `(;; Install the source so that Cargo.toml is installed, because that is
        ;; the only reference to the license information.
        #:install-source? #t
        #:phases
        (modify-phases %standard-phases
          (add-before 'check 'patch-tests
            (lambda _
              (substitute* "tests/cli_tests.rs"
                (("/bin/sh") (which "sh")))))
          (add-after 'install 'install-doc
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (doc (string-append out "/share/doc/" ,name "-"
                                         ,(package-version this-package))))
                (install-file "README.md" doc)))))
        #:cargo-inputs
        (("rust-anyhow" ,rust-anyhow-1)
         ("rust-blake3" ,rust-blake3-1)
         ("rust-clap" ,rust-clap-4)
         ("rust-hex" ,rust-hex-0.4)
         ("rust-memmap2" ,rust-memmap2-0.7)
         ("rust-rayon" ,rust-rayon-1)
         ("rust-wild" ,rust-wild-2))
        #:cargo-development-inputs
        (("rust-duct" ,rust-duct-0.13)
         ("rust-tempfile" ,rust-tempfile-3))))
    (home-page "https://github.com/BLAKE3-team/BLAKE3")
    (synopsis "Command line BLAKE3 checksum tool")
    (description "This package provides @code{b3sum}, a command line
checksum tool based on the BLAKE3 cryptographic hash function.")
    ;; Users may choose between these two licenses when redistributing the
    ;; program provided by this package.
    (license (list license:cc0 license:asl2.0))))

(define-public libxcrypt
  (package
    (name "libxcrypt")
    (version "4.4.38")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://github.com/besser82/libxcrypt/releases/download/v" version
         "/libxcrypt-" version ".tar.xz"))
       (sha256
        (base32 "1mkd42s12iagnpqk29hqi5vk2a6vkdaagn81gwr9k9vf62f4nc40"))))
    (build-system gnu-build-system)
    (native-inputs
     (list perl))
    (arguments
     (cond
       ((target-ppc32?)
        (list #:tests? #f))     ; TODO: Investigate test failures.
       ((target-mingw?)
        (list #:configure-flags
              #~(list #$(string-append "CFLAGS=-g -O2"
                                       " -Wno-error=pedantic"
                                       " -Wno-error=conversion")
                      "ac_cv_ld_no_undefined=-no-undefined")))
       (else '())))
    (synopsis
     "Extended crypt library for descrypt, md5crypt, bcrypt, and others")
    (description
     "libxcrypt is a modern library for one-way hashing of passwords.  It
supports a wide variety of both modern and historical hashing methods:
yescrypt, gost-yescrypt, scrypt, bcrypt, sha512crypt, sha256crypt, md5crypt,
SunMD5, sha1crypt, NT, bsdicrypt, bigcrypt, and descrypt.")
    (home-page "https://github.com/besser82/libxcrypt")
    (license license:lgpl2.1)))

(define-public keychain
  (package
    (name "keychain")
    (version "2.9.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/funtoo/keychain")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1i698n0mp2wxk1yd8lhwq7i1dj5v01li1g9qi047aqc34r4079lq"))))
    (build-system gnu-build-system)
    (propagated-inputs (list procps))
    (native-inputs (list perl))
    (arguments
     `(#:tests? #f ; No test suite
       #:phases (modify-phases %standard-phases
                  (delete 'configure)
                  (replace 'install
                    (lambda _
                      (install-file "keychain"
                                    (string-append %output "/bin/"))
                      (install-file "keychain.1"
                                    (string-append %output "/share/man/man1"))
                      #t)))))
    (synopsis
     "SSH or GPG agent frontend that can share a single agent on the same
system")
    (description
     "Keychain is usually run from shell profiles like ~/.bash_profile, but
it is also possible to use it with non-interactive shells.  It works
with various operating systems (including GNU/Linux and HURD) and
shells (like bourne-compatible, csh-compatible and fish shells).  By
default Keychain will only start ssh-agent, but it can also be
configured to start gpg-agent.")
    (home-page "https://www.funtoo.org/Keychain")
    (license license:gpl2)))
