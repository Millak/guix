;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Steve Sprang <scs@stevesprang.com>
;;; Copyright © 2015-2024 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2015 Aljosha Papsch <misc@rpapsch.de>
;;; Copyright © 2016 Christine Lemmer-Webber <cwebber@dustycloud.org>
;;; Copyright © 2016 Jessica Tallon <tsyesika@tsyesika.se>
;;; Copyright © 2016 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2016 Lukas Gradl <lgradl@openmailbox.org>
;;; Copyright © 2016, 2019, 2020 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2017 Leo Famulari <leo@famulari.name>
;;; Copyright © 2017, 2018 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2017–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Jelle Licht <jlicht@fsfe.org>
;;; Copyright © 2017, 2019 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2017, 2020-2022, 2024 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2017 Manolis Fragkiskos Ragkousis <manolis837@gmail.com>
;;; Copyright © 2017 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2018, 2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2018 Konrad Hinsen <konrad.hinsen@fastmail.net>
;;; Copyright © 2018 Thomas Sigurdsen <tonton@riseup.net>
;;; Copyright © 2018 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2018 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2018 Amirouche Boubekki <amirouche@hypermove.net>
;;; Copyright © 2018, 2019, 2020 Tim Gesthuizen <tim.gesthuizen@yahoo.de>
;;; Copyright © 2019 Jens Mølgaard <jens@zete.tk>
;;; Copyright © 2019,2022 Tanguy Le Carrour <tanguy@bioneland.org>
;;; Copyright © 2020, 2024 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2020 Brice Waegeneire <brice@waegenei.re>
;;; Copyright © 2020 Jean-Baptiste Note <jean-baptiste.note@m4x.org>
;;; Copyright © 2020 Michael Rohleder <mike@rohleder.de>
;;; Copyright © 2020 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2020 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2021 Stefan Reichör <stefan@xsteve.at>
;;; Copyright © 2021 Xinglu Chen <public@yoctocell.xyz>
;;; Copyright © 2020 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2021 David Dashyan <mail@davie.li>
;;; Copyright © 2022 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2022 Maxime Devos <maximedevos@telenet.be>
;;; Copyright © 2022 ( <paren@disroot.org>
;;; Copyright © 2022 Nicolas Graves <ngraves@ngraves.fr>
;;; Copyright © 2022 Petr Hodina <phodina@protonmail.com>
;;; Copyright © 2023 Christian Miller <christian.miller@dadoes.de>
;;; Copyright © 2024 John Kehayias <john.kehayias@protonmail.com>
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

(define-module (gnu packages password-utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (guix build-system python)
  #:use-module (guix build-system qt)
  #:use-module (guix build-system trivial)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages aidc)
  #:use-module (gnu packages authentication)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages cryptsetup)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages digest)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages file)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-crypto)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages golang-web)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages kerberos)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages man)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages opencl)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages rdesktop)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages security-token)
  #:use-module (gnu packages suckless)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages web)
  #:use-module (gnu packages wxwidgets)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xml))

(define-public aws-vault
  (package
    (name "aws-vault")
    (version "7.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/99designs/aws-vault")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1dqg6d2k8r80ww70afghf823z0pijha1i0a0c0c6918yb322zkj2"))))
    (build-system go-build-system)
    (arguments
     (list
      #:install-source? #f
      #:import-path "github.com/99designs/aws-vault"
      #:build-flags
      #~(list (string-append "-ldflags=-X main.Version=" #$version))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'build 'contrib
            (lambda* (#:key import-path #:allow-other-keys)
              (let* ((zsh-site-dir
                      (string-append #$output "/share/zsh/site-functions"))
                     (bash-completion-dir
                      (string-append #$output "/share/bash-completion/completions"))
                     (fish-completion-dir
                      (string-append #$output "/share/fish/completions")))
                (for-each mkdir-p (list bash-completion-dir
                                        fish-completion-dir
                                        zsh-site-dir))
                (with-directory-excursion
                    (string-append "src/" import-path "/contrib/completions")
                  (copy-file "zsh/aws-vault.zsh"
                             (string-append zsh-site-dir "/_aws-vault"))
                  (copy-file "bash/aws-vault.bash"
                             (string-append bash-completion-dir "/aws-vault"))
                  (copy-file "fish/aws-vault.fish"
                             (string-append fish-completion-dir "/aws-vault.fish"))))))
          ;; aws-vault: error: add: mkdir /homeless-shelter: permission
          ;; denied.
          (add-before 'check 'set-home
            (lambda _
              (setenv "HOME" "/tmp"))))))
    (native-inputs
     (list go-github-com-99designs-keyring
           go-github-com-alecthomas-kingpin-v2
           go-github-com-aws-aws-sdk-go-v2
           go-github-com-aws-aws-sdk-go-v2-config
           go-github-com-aws-aws-sdk-go-v2-credentials
           go-github-com-aws-aws-sdk-go-v2-service-iam
           go-github-com-aws-aws-sdk-go-v2-service-sso
           go-github-com-aws-aws-sdk-go-v2-service-ssooidc
           go-github-com-aws-aws-sdk-go-v2-service-sts
           go-github-com-google-go-cmp
           go-github-com-mattn-go-isatty
           go-github-com-mattn-go-tty
           go-github-com-skratchdot-open-golang
           go-golang-org-x-term
           go-gopkg-in-ini-v1))
    (home-page "https://github.com/99designs/aws-vault")
    (synopsis "Vault for securely storing and accessing AWS credentials")
    (description
     "AWS Vault is a tool to securely store and access @acronym{Amazon Web
Services,AWS} credentials.

AWS Vault stores IAM credentials in your operating system's secure keystore and
then generates temporary credentials from those to expose to your shell and
applications.  It's designed to be complementary to the AWS CLI tools, and is
aware of your profiles and configuration in ~/.aws/config.")
    (license license:expat)))

(define-public pwgen
  (package
    (name "pwgen")
    (version "2.08")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/pwgen/pwgen/" version
                           "/pwgen-" version ".tar.gz"))
       (sha256
        (base32 "0yy90pqrr2pszzhb5hxjishq9qc7dqd290amiibqx9fm1b9kvc6s"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f)) ; no test suite
    (home-page "http://pwgen.sourceforge.net/")
    (synopsis "Password generator")
    (description "Pwgen generates passwords which can be easily memorized by a
human.")
    (license license:gpl2)))

(define-public keepassxc
  (package
    (name "keepassxc")
    (version "2.7.10")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/keepassxreboot/keepassxc"
                           "/releases/download/" version "/keepassxc-"
                           version "-src.tar.xz"))
       (sha256
        (base32 "1ylxh72bpf4pzsj13j8zlxpidp6aygaikw454n228v4q81j6vrsw"))))
    (build-system qt-build-system)
    (arguments
     (list
      #:configure-flags
      #~(append
         (list "-DWITH_XC_ALL=YES"
               "-DWITH_XC_UPDATECHECK=NO")
         #$(if (member (%current-system)
                       (package-transitive-supported-systems ruby-asciidoctor))
               #~'()
               #~(list "-DWITH_XC_DOCS=NO")))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'record-clipboard-programs
            (lambda* (#:key inputs #:allow-other-keys)
              ;; Record the file names of clipboard programs invoked by
              ;; 'keepassxc-cli clip' and similar.
              ;;
              ;; Note: Use 'QString::fromUtf8' rather than 'QStringLiteral' so
              ;; that the store reference is stored as ASCII instead of
              ;; UTF-16, which would be invisible to the GC's scanner.
              (substitute* "src/cli/Utils.cpp"
                (("QStringLiteral\\(\"xclip\"\\)")
                 (string-append
                  "QString::fromUtf8(\""
                  (search-input-file inputs "bin/xclip")
                  "\")"))
                (("QStringLiteral\\(\"wl-copy\"\\)")
                 (string-append
                  "QString::fromUtf8(\""
                  (search-input-file inputs "bin/wl-copy")
                  "\")")))
              (substitute* "src/gui/Clipboard.cpp"
                (("\"wl-copy\"")
                 (format #f "~s" (search-input-file inputs "bin/wl-copy"))))))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                ;; Entries have recently become sorted per the locale, and
                ;; causes testentrymodel to fail when using the C locale (see:
                ;; https://github.com/keepassxreboot/keepassxc/issues/11813).
                ;; testClip runs 'xclip', which hangs in the container,
                ;; perhaps because it lacks a TTY.
                (invoke "ctest" "--exclude-regex"
                        "testentrymodel|testcli")))))))
    (native-inputs
     (append
      (list qttools-5)
      (if (member (%current-system)
                  (package-transitive-supported-systems ruby-asciidoctor))
          (list ruby-asciidoctor)
          '())))
    (inputs
     (list argon2
           botan
           libgcrypt
           libsodium                    ; XC_BROWSER
           libusb
           libyubikey                   ; XC_YUBIKEY
           libxi
           libxtst
           minizip
           pcsc-lite
           qrencode
           qtsvg-5
           qtwayland-5
           qtx11extras
           readline
           wl-clipboard                 ;for 'wl-copy'
           xclip                        ;for 'xclip'
           yubikey-personalization      ; XC_YUBIKEY
           zlib))
    (home-page "https://keepassxc.org")
    (synopsis "Password manager")
    (description "KeePassXC is a password manager or safe which helps you to
manage your passwords in a secure way.  You can put all your passwords in one
database, which is locked with one master key or a key-file which can be stored
on an external storage device.  The databases are encrypted using the
algorithms AES or Twofish.")
    (properties
     '((release-monitoring-url . "https://github.com/keepassxreboot/keepassxc/releases")))
    ;; While various parts of the software are licensed under different licenses,
    ;; the combined work falls under the GPLv3.
    (license license:gpl3)))

(define-public pwsafe
  (package
    (name "pwsafe")
    (version "3.60.0")
    (home-page "https://www.pwsafe.org/")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pwsafe/pwsafe")
             (commit version)))
       (sha256
        (base32 "064y78sqr8h9mq922spi4r13ga0a1j09mfh4kc4pn7j697nl6b5y"))
       (file-name (git-file-name name version))))
    (build-system cmake-build-system)
    (native-inputs
     (list gettext-minimal googletest perl zip))
    (inputs (list curl
                  file
                  `(,util-linux "lib")
                  libxt
                  libxtst
                  openssl
                  qrencode
                  wxwidgets
                  xerces-c))
    (arguments '(#:configure-flags (list "-DNO_GTEST=YES")
                 #:phases (modify-phases %standard-phases
                            (add-after 'unpack 'add-gtest
                              (lambda* (#:key inputs #:allow-other-keys)
                                (chmod "CMakeLists.txt" #o644)
                                (let ((cmake-port (open-file "CMakeLists.txt"
                                                             "a")))
                                  (display "find_package(GTest)
add_subdirectory(src/test)\n" cmake-port)
                                  (close cmake-port)
                                  #t))))))
    (synopsis "Password safe with automatic input and key generation")
    (description "pwsafe is a password manager originally designed by Bruce
Schneier.  It offers a simple UI to manage passwords for different services.
There are other programs that support the file format on different
platforms.")
    (license license:artistic2.0)))

(define-public pwsafe-cli
  (let ((commit "c49a0541b66647ad04d19ddb351d264054c67759")
        (revision "0"))
    (package
      (name "pwsafe-cli")
      (version (git-version "0.2.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/nsd20463/pwsafe")
               (commit commit)))
         (sha256
          (base32
           "0ak09r1l7k57m6pdx468hhzvz0szmaq42vyr575fvsjc8rbrp8qq"))
         (file-name (git-file-name name version))))
      (build-system gnu-build-system)
      (arguments
       ;; FIXME: skip failing test suite (requires write access to /tmp),
       ;; patching path does not help somehow.
       `(#:tests? #f
         #:phases
         (modify-phases %standard-phases
           (replace 'bootstrap
             (lambda _
               (invoke "aclocal")
               (invoke "autoheader")
               (invoke "automake" "--add-missing")
               (invoke "autoconf")
               #t)))))
      (native-inputs
       (list autoconf automake))
      (inputs
       (list libx11 libxmu libxt openssl))
      (home-page "https://github.com/nsd20463/pwsafe")
      (synopsis "CLI password manager")
      (description
       "@command{pwsafe} is a command line tool compatible with
Counterpane's Passwordsafe.")
      (license license:gpl2+))))

(define-public otpclient
  (package
    (name "otpclient")
    (version "4.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/paolostivanin/OTPClient")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "061idzh9sz556nm7ahjrvcbnbmgvgfwmph1lfiy7bcvj1g3rf8cm"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:modules `(((guix build glib-or-gtk-build-system) #:prefix glib-or-gtk:)
                  (guix build cmake-build-system)
                  (guix build utils))
      #:imported-modules `((guix build glib-or-gtk-build-system)
                           ,@%cmake-build-system-modules)
      #:tests? #f                        ; No tests
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'generate-gdk-pixbuf-loaders-cache-file
            (assoc-ref glib-or-gtk:%standard-phases
                       'generate-gdk-pixbuf-loaders-cache-file))
          (add-after 'install 'glib-or-gtk-compile-schemas
            (assoc-ref glib-or-gtk:%standard-phases
                       'glib-or-gtk-compile-schemas))
          (add-after 'glib-or-gtk-compile-schemas 'glib-or-gtk-wrap
            (assoc-ref glib-or-gtk:%standard-phases 'glib-or-gtk-wrap)))))
    (inputs (list adwaita-icon-theme
                  libcotp
                  libgcrypt
                  libsecret
                  libzip
                  hicolor-icon-theme
                  gtk+
                  jansson
                  protobuf
                  protobuf-c
                  qrencode
                  zbar))
    (native-inputs (list pkg-config protobuf))
    (home-page "https://github.com/paolostivanin/OTPClient")
    (synopsis "Two-factor authentication client")
    (description "OTPClient is a GTK+-based @acronym{OTP, One Time Password}
client, supporting @acronym{TOTP, Time-based one time passwords} and
@acronym{HOTP,HMAC-based one time passwords}.")
    (license license:gpl3)))

(define-public shroud
  (package
    (name "shroud")
    (version "0.1.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://files.dthompson.us/shroud/shroud-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1l2shrhvcwfzkar9qiwb75nhcqmx25iz55lzmz0c187nbjhqzi9p"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config))
    (arguments
     `(#:modules ((guix build gnu-build-system)
                    (guix build utils)
                    (ice-9 popen)
                    (ice-9 rdelim))
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'wrap-shroud
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out       (assoc-ref outputs "out"))
                    (guile (assoc-ref inputs "guile"))
                    (effective (read-line
                                (open-pipe* OPEN_READ
                                            (string-append guile "/bin/guile")
                                            "-c" "(display (effective-version))")))
                    (ccachedir (string-append out
                                             "/lib/guile/" effective "/site-ccache"))
                    (prog      (string-append out "/bin/shroud")))
               (wrap-program prog
                 `("GUILE_LOAD_COMPILED_PATH" ":" prefix (,ccachedir)))))))))
    (inputs (list bash-minimal guile-2.2 gnupg xclip))
    (synopsis "GnuPG-based secret manager")
    (description "Shroud is a simple secret manager with a command line
interface.  The password database is stored as a Scheme s-expression and
encrypted with a GnuPG key.  Secrets consist of an arbitrary number of
key/value pairs, making Shroud suitable for more than just password storage.
For copying and pasting secrets into web browsers and other graphical
applications, there is xclip integration." )
    (home-page "https://dthompson.us/projects/shroud.html")
    (license license:gpl3+)))

(define-public ssh-to-age
  (package
    (name "ssh-to-age")
    (version "1.1.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Mic92/ssh-to-age")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "134gpbalyll238wvj9ci0rascgm4csayz863ci99cy5qq8266wrl"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/Mic92/ssh-to-age/cmd/ssh-to-age"
       #:unpack-path "github.com/Mic92/ssh-to-age"))
    (inputs (list go-golang-org-x-crypto
                  go-filippo-io-edwards25519
                  go-filippo-io-age))
    (home-page "https://github.com/Mic92/ssh-to-age")
    (synopsis "Convert SSH @code{ed25519} keys to @code{age} keys")
    (description "This package provides a simple command-line tool to
convert SSH @code{ed25519} keys to @code{age} keys.")
    (license license:expat)))

(define-public yapet
  (package
    (name "yapet")
    (version "2.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://yapet.guengel.ch/downloads/yapet-"
                           version ".tar.xz"))
       (sha256
        (base32 "00k38n5vlxl73m11pp1v50fqf702lv86hzwgj0qca6qxqz4i3jjl"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list (string-append "--docdir=" (assoc-ref %outputs "out")
                            "/share/doc/" ,name "-" ,version))))
    (inputs
     (list argon2 ncurses openssl))
    (native-inputs
     (list cppunit pkg-config))
    (synopsis "Yet Another Password Encryption Tool")
    (description "YAPET is a text based password manager using the Blowfish
encryption algorithm.  Because of its small footprint and very few library
dependencies, it is suited for installing on desktop and server systems alike.
The text based user interface allows you to run YAPET easily in a Secure Shell
session.  Two companion utilities enable users to convert CSV files to YAPET
and vice versa.")
    (home-page "https://yapet.guengel.ch/")
    (license license:gpl3+)))

(define-public cracklib
  (package
    (name "cracklib")
    (version "2.9.8")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/cracklib/cracklib/"
                           "releases/download/v" version "/"
                           "cracklib-" version ".tar.bz2"))
       (sha256
        (base32 "11p3f0yqg9d32g3n1qik7jfyl2l14pf8i8vzq3bpram3bqw3978z"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-dict
           (lambda* (#:key make-flags #:allow-other-keys)
             (begin
               (chmod (string-append "util/cracklib-format") #o755)
               (apply invoke "make" "dict" make-flags)
               #t))))))
    (synopsis "Password checking library")
    (home-page "https://github.com/cracklib/cracklib")
    (description
     "CrackLib is a library containing a C function which may be used in a
@command{passwd}-like program.  The idea is simple: try to prevent users from
choosing passwords that could easily be guessed (or \"cracked\") by filtering
them out, at the source.")
    (license license:lgpl2.1)))

(define-public libpwquality
  (package
    (name "libpwquality")
    (version "1.4.4")
    (source (origin
              (method url-fetch)
              (uri (list
                    (string-append "https://github.com/libpwquality/libpwquality"
                                   "/releases/download/libpwquality-" version
                                   "/libpwquality-" version ".tar.bz2")
                    (string-append "https://launchpad.net/libpwquality/trunk/"
                                   version "/+download/"
                                   "libpwquality-" version ".tar.bz2")))
              (sha256
               (base32
                "0id5a8bi8xnjg11g9vzrl2xbpx65mfxclxcvis7zx1v8vhisyfyl"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'set-LDFLAGS
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (setenv "LDFLAGS"
                     (string-append
                      "-Wl,-rpath="
                      (assoc-ref outputs "out") "/lib"))
             #t)))))
    (native-inputs
     `(("python" ,python-wrapper)))
    (inputs
     (list cracklib))
    (synopsis "Password quality checker")
    (home-page "https://github.com/libpwquality/libpwquality")
    (description
     "Libpwquality is a library for password quality checking and generation of
random passwords that pass the checks.")
    (license license:gpl2+)))

(define-public passwdqc
  (package
    (name "passwdqc")
    (version "2.0.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.openwall.com/passwdqc/passwdqc"
                                  "-" version ".tar.gz"))
              (sha256
               (base32
                "1x4c92b3i5wmxh2111lynyndalpkryvan4wybqchd7rn96yg9c2k"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f                       ; no tests provided
      #:make-flags
      #~(list (string-append "CC=" #$(cc-for-target))
              (string-append "DESTDIR=" #$output) ; no $prefix support
              "BINDIR=/bin"
              "DEVEL_LIBDIR=/lib"
              "SHARED_LIBDIR_REL=."
              "INCLUDEDIR=/include"
              "MANDIR=/share/man"
              (string-append "LDFLAGS=-Wl,-rpath=" #$output "/lib"))
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure))))        ;no configure script
    (inputs (list linux-pam libxcrypt))
    (home-page "https://www.openwall.com/passwdqc/")
    (synopsis
     "Password/passphrase strength checking and policy enforcement toolset")
    (description
     "Passwdqc is a password/passphrase strength checking and policy
enforcement toolset, including an optional PAM module, @code{pam_passwdqc},
command-line programs (@command{pwqcheck}, @command{pwqfilter}, and
@command{pwqgen}), and a library, @code{libpasswdqc}.")
    (license (list license:bsd-3        ;manual pages
                   license:bsd-1))))    ;code

(define-public assword
  (package
    (name "assword")
    (version "0.11")
    (source (origin
              (method url-fetch)
              (uri (list
                    (string-append
                     "http://http.debian.net/debian/pool/main/a/assword/"
                     "assword_" version ".orig.tar.gz")))
              (sha256
               (base32
                "03gkb6kvsghznbcw5l7nmrc6mn3ixkjd5jcs96ni4zs9l47jf7yp"))))
    (arguments
     `(;; irritatingly, tests do run but not there are two problems:
       ;;  - "import gtk" fails for unknown reasons here despite it the
       ;;    program working (indeed, I've found I have to do a logout and log
       ;;    back in in after an install order for some mumbo jumbo environment
       ;;    variable mess to work with pygtk and assword... what's up with
       ;;    that?)
       ;;  - even when the tests fail, they don't return a nonzero status,
       ;;    so I'm not sure how to programmatically get that information
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'wrap-assword
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((prog            (string-append
                                     (assoc-ref outputs "out")
                                     "/bin/assword"))
                   (gi-typelib-path (getenv "GI_TYPELIB_PATH")))
               (wrap-program prog
                 `("GI_TYPELIB_PATH" ":" prefix (,gi-typelib-path))))))
         (add-after 'install 'manpage
           (lambda* (#:key outputs #:allow-other-keys)
             (invoke "make" "assword.1")
             (install-file
              "assword.1"
              (string-append (assoc-ref outputs "out") "/share/man/man1")))))))
    (build-system python-build-system)
    (native-inputs
     (list txt2man))
    (inputs
     (list bash-minimal gtk+ python-xdo python-gpg python-pygobject))
    (propagated-inputs
     (list xclip))
    (home-page "https://finestructure.net/assword/")
    (synopsis "Password manager")
    (description "assword is a simple password manager using GPG-wrapped
JSON files.  It has a command line interface as well as a very simple
graphical interface, which can even \"type\" your passwords into
any X11 window.")
    (license license:gpl3+)))

(define-public password-store
  (package
    (name "password-store")
    (version "1.7.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "git://git.zx2c4.com/password-store")
                    (commit version)))
              (sha256
               (base32
                "17zp9pnb3i9sd2zn9qanngmsywrb7y495ngcqs6313pv3gb83v53"))
              (patches (search-patches "password-store-tree-compat.patch"))
              (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (add-before 'install 'patch-system-extension-dir
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (extension-dir (string-append out "/lib/password-store/extensions")))
               (substitute* "src/password-store.sh"
                 (("^SYSTEM_EXTENSION_DIR=.*$")
                  ;; lead with whitespace to prevent 'make install' from
                  ;; overwriting it again
                  (string-append " SYSTEM_EXTENSION_DIR=\""
                                 "${PASSWORD_STORE_SYSTEM_EXTENSION_DIR:-"
                                 extension-dir
                                 "}\"\n"))))))
         (add-before 'install 'patch-program-name
           ;; Use pass not .pass-real in tmpdir and cmd_usage
           (lambda _
             (substitute* "src/password-store.sh"
               (("^PROGRAM=.*$")
                "PROGRAM=\"pass\"\n"))))
         (add-before 'install 'patch-passmenu-path
           ;; FIXME Wayland support requires ydotool and dmenu-wl packages
           ;; We are ignoring part of the script that gets executed if
           ;; WAYLAND_DISPLAY env variable is set, leaving dmenu-wl and ydotool
           ;; commands as is.
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "contrib/dmenu/passmenu"
               (("dmenu=dmenu\n")
                (string-append "dmenu="
                               (search-input-file inputs "/bin/dmenu")
                               "\n"))
               (("xdotool=\"xdotool")
                (string-append "xdotool=\""
                               (search-input-file inputs "/bin/xdotool"))))))
         (add-after 'install 'install-passmenu
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (install-file "contrib/dmenu/passmenu" bin))))
         (add-after 'install 'wrap-path
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (requisites '("getopt" "git" "gpg" "qrencode" "sed"
                                  "tail" "tree" "which" "wl-copy" "xclip"))
                    (path (map (lambda (pkg)
                                 (dirname (search-input-file
                                           inputs (string-append "/bin/" pkg))))
                               requisites)))
               (wrap-program (string-append out "/bin/pass")
                 `("PATH" ":" prefix (,(string-join path ":"))))))))
       #:make-flags (list "CC=gcc" (string-append "PREFIX=" %output)
                          "WITH_ALLCOMP=yes"
                          (string-append "BASHCOMPDIR="
                                         %output "/etc/bash_completion.d"))
       ;; Parallel tests may cause a race condition leading to a
       ;; timeout in some circumstances.
       #:parallel-tests? #f
       #:test-target "test"))
    (native-search-paths
     (list (search-path-specification
            (variable "PASSWORD_STORE_SYSTEM_EXTENSION_DIR")
            (separator #f)             ;single entry
            (files '("lib/password-store/extensions")))))
    (inputs
     (list bash-minimal
           coreutils
           dmenu
           util-linux
           git
           gnupg
           qrencode
           sed
           tree
           which
           wl-clipboard
           xclip
           xdotool))
    (home-page "https://www.passwordstore.org/")
    (synopsis "Encrypted password manager")
    (description "Password-store is a password manager which uses GnuPG to
store and retrieve passwords.  The tool stores each password in its own
GnuPG-encrypted file, allowing the program to be simple yet secure.
Synchronization is possible using the integrated git support, which commits
changes to your password database to a git repository that can be managed
through the pass command.")
    (license license:gpl2+)))

(define-public pass-age
  (package
    (inherit password-store)
    (name "pass-age")
    (version "1.7.4a2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/FiloSottile/passage")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ap2i08zjvacd2rllrsx9bw3zz5i99bk0i5yxrssvn6w60bwjqdl"))))
    (build-system copy-build-system)
    (arguments
     '(#:modules
       ((guix build copy-build-system)
        (guix build utils)
        (srfi srfi-26))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'rename-script
           (lambda _
             (rename-file "src/password-store.sh"
                          "src/passage")))
         (add-after 'install 'wrap-script
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (script (string-append out "/bin/passage")))
               (substitute* script
                 ;; Avoid ugly ‘.passage-real’ in --help output and elsewhere.
                 (("^(PROGRAM=).*" _ program=)
                  (string-append program= (basename script) "\n")))
               (wrap-program script
                 `("PATH" ":" prefix
                   ,(map dirname
                         (map (cut search-input-file inputs <>)
                              (list "bin/age"
                                    "bin/age-keygen"
                                    "bin/cat"
                                    "bin/getopt"
                                    "bin/git"
                                    "bin/pkill"
                                    "bin/qrencode"
                                    "bin/sed"
                                    "bin/tree")))))))))
       #:install-plan
       '(("src/passage" "/bin/")
         ("src/completion/pass.bash-completion"
          "/etc/bash-completion.d/passage")
         ("src/completion/pass.fish-completion"
          "/share/fish/vendor_completions.d/passage")
         ("src/completion/pass.zsh-completion"
          "/share/zsh/site-functions/_passage"))))
    (inputs
     (list age coreutils-minimal git-minimal
           procps qrencode sed tree util-linux))
    (home-page "https://github.com/FiloSottile/passage")
    (synopsis "Encrypted password manager")
    (description "This package provides an encrypted password manager, forked
from the @code{password-store} package.  Files are encrypted with the
@command{age} encryption package with small explicit keys.")
    (license license:gpl2+)))

(define-public pass-otp
  (package
    (name "pass-otp")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://github.com/tadfisher/pass-otp/releases/"
                       "download/v" version "/pass-otp-" version ".tar.gz"))
       (sha256
        (base32
         "0rrs3iazq80dn0wbl20xkh270428jd8l99m5gd7hl93s4r4sc82p"))))
    (build-system gnu-build-system)
    (arguments
     '(#:make-flags
       (let* ((out      (assoc-ref %outputs "out"))
              (bashcomp (string-append out "/etc/bash_completion.d")))
         (list (string-append "PREFIX=" %output)
               (string-append "BASHCOMPDIR=" bashcomp)))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after 'build 'patch-oath-path
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "otp.bash"
               (("^OATH=.*$")
                (string-append
                 "OATH="
                 (assoc-ref inputs "oath-toolkit")
                 "/bin/oathtool\n")))
             #t)))
       #:test-target "test"))
    (inputs
     (list oath-toolkit))
    (native-inputs
     (list password-store expect git gnupg which))
    (home-page "https://github.com/tadfisher/pass-otp")
    (synopsis "Pass extension for managing one-time-password (OTP) tokens")
    (description
     "Pass OTP is an extension for password-store that allows adding
one-time-password (OTP) secrets, generating OTP codes, and displaying secret
key URIs using the standard otpauth:// scheme.")
    (license license:gpl3+)))

(define-public qtpass
  (package
    (name "qtpass")
    (version "1.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/IJHack/QtPass")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "10ixahm4ap0l1rrz4cyswblm22ns9z1baf5lv3dn23wprfdcp8m0"))))
    (build-system qt-build-system)
    (arguments
     (list
      #:test-target "check"
      #:phases
      #~(modify-phases %standard-phases
          (replace 'configure
            (lambda _
              (invoke "qmake"
                      "QMAKE_LRELEASE=lrelease"
                      "QMAKE_LUPDATE=lupdate"
                      (string-append "PREFIX=" #$output))))
          (add-before 'check 'pre-check
            ;; Fontconfig needs a writable cache.
            (lambda _ (setenv "HOME" "/tmp")))
          (add-after 'install 'install-auxilliary
            ;; Install man-page, icon and .desktop file.
            (lambda _
              (let ((applications (string-append #$output "/share/applications"))
                    (icons (string-append #$output "/share/icons/hicolor/scalable/apps"))
                    (man (string-append #$output "/share/man/man1")))
                (install-file "qtpass.desktop" applications)
                (install-file "artwork/icon.svg" icons)
                (rename-file (string-append icons "/icon.svg")
                             (string-append icons "/qtpass-icon.svg"))
                (install-file "qtpass.1" man)))))))
    (native-inputs
     (list qttools-5))
    (inputs
     (list qtsvg-5))
    (home-page "https://qtpass.org")
    (synopsis "GUI for password manager password-store")
    (description
     "Qt-based graphical user interface for the password manager
password-store also known as pass.  Can use either pass or gpg to interact
with password-store files.  Features configurable password generation,
templates, clipboard handling, and per folder settings for multi-recipient
encryption.")
    (license license:gpl3+)))

(define-public rofi-pass
  ;; No release in over 5 years with recent commits adding features like
  ;; Wayland support.
  (let ((commit "8aa6b9293a8f0af267425326fa966966ca42085e")
        (revision "0"))
    (package
      (name "rofi-pass")
      (version (git-version "2.0.2" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/carnager/rofi-pass")
               (commit commit)))
         (sha256
          (base32
           "0axz4ijp6fay6f2yn1cg6223l89jkg8wnxslbk1g5jpli0njxw43"))
         (file-name (git-file-name name version))))
      (build-system gnu-build-system)
      (arguments
       (list #:tests? #f ; no tests
             #:make-flags #~(list (string-append "PREFIX=" #$output))
             #:phases
             #~(modify-phases %standard-phases
                 (add-after 'unpack 'fix-etc-path
                   (lambda _
                     (substitute* "Makefile"
                       (("\\$\\(DESTDIR\\)/etc")
                        (string-append #$output "/etc")))
                     (substitute* "rofi-pass"
                       (("/etc")
                        (string-append #$output "/etc")))))
                 (delete 'configure) ; no configure
                 (delete 'build)     ; no build
                 (add-after 'install 'wrap-path
                   (lambda* (#:key inputs #:allow-other-keys)
                     (let ((bin (string-append #$output "/bin")))
                       (for-each
                        (lambda (script)
                          (wrap-program (string-append bin "/" script)
                            (list "PATH" 'prefix
                                  (map
                                   (lambda (binary)
                                     (dirname (search-input-file
                                               inputs
                                               (string-append "bin/" binary))))
                                   '("pass" "pwgen" "rofi"
                                     "xclip" "xdotool" "xset")))))
                        (list "addpass" "rofi-pass"))))))))
      (inputs (list bash-minimal ;for wrap-program
                    password-store
                    pwgen
                    rofi
                    xclip
                    xdotool
                    xset))
      (home-page "https://github.com/carnager/rofi-pass")
      (synopsis "Rofi frontend for password-store")
      (description "Rofi-pass provides a way to manipulate information stored
using password-store through rofi interface:
@enumerate
@item open URLs of entries with hotkey;
@item type any field from entry;
@item auto-typing of user and/or password fields;
@item auto-typing username based on path;
@item auto-typing of more than one field, using the autotype entry;
@item bookmarks mode (open stored URLs in browser, default: Alt+x).
@end enumerate")
      (license license:gpl3))))

(define-public rofi-pass-wayland
  (package
    (inherit rofi-pass)
    (name "rofi-pass-wayland")
    (arguments
     (substitute-keyword-arguments (package-arguments rofi-pass)
       ((#:phases phases)
        #~(modify-phases #$phases
            ;; Set the clipboard and backend tools to ones for Wayland in the
            ;; default configuration file.
            (add-after 'fix-etc-path 'set-wayland-defaults
                   (lambda _
                     (substitute* "config.example"
                       ;; Note the typo in current configuration.
                       (("#clibpoard_backend=xclip")
                        "clipboard_backend=wl-clipboard")
                       (("#backend=xdotool")
                        "backend=wtype"))))
            ;; Use Wayland related tools instead.
            (replace 'wrap-path
              (lambda* (#:key inputs #:allow-other-keys)
                (let ((bin (string-append #$output "/bin")))
                  (for-each
                   (lambda (script)
                     (wrap-program (string-append bin "/" script)
                       (list "PATH" 'prefix
                             (map
                              (lambda (binary)
                                (dirname (search-input-file
                                          inputs
                                          (string-append "bin/" binary))))
                              ;; wl-copy for wl-clipboard.
                              '("pass" "pwgen" "rofi" "wl-copy" "wtype")))))
                   (list "addpass" "rofi-pass")))))))))
    (inputs
     (modify-inputs (package-inputs rofi-pass)
       (replace "rofi" rofi-wayland)
       (replace "xclip" wl-clipboard)
       (replace "xdotool" wtype)
       (delete "xset")))
    (description (string-append
                  (package-description rofi-pass)
                  "\nThis package provides Wayland support by default."))))

(define-public tessen
  (package
    (name "tessen")
    (version "2.2.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ayushnix/tessen/")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0pxx3x50k1zi82vjvib94rar6sy5bz3s2amq4zyba6s1a8isqlcr"))))
    (build-system gnu-build-system)
    (arguments
     (list #:tests?
           #f ;no tests
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'patch-wtype-path
                 (lambda* (#:key inputs #:allow-other-keys)
                   (substitute* "tessen"
                     (("notify-send") (search-input-file inputs
                                                         "/bin/notify-send"))
                     (("wl-copy") (search-input-file inputs "/bin/wl-copy"))
                     (("wtype") (search-input-file inputs "/bin/wtype"))
                     (("xdg-open") (search-input-file inputs "/bin/xdg-open")))))
               (delete 'configure)) ;no configure script
           #:make-flags
           #~(list (string-append "DESTDIR=" #$output)
                   "PREFIX=''")))
    (native-inputs (list scdoc))
    (inputs (list libnotify wl-clipboard wtype xdg-utils))
    (home-page "https://github.com/ayushnix/tessen")
    (synopsis "Frontend for password-store and gopass")
    (description "Tessen is a bash script that can autotype and copy data
from password-store and gopass files.")
    (license license:gpl2)))

(define-public browserpass-native
  (package
    (name "browserpass-native")
    (version "3.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/browserpass/browserpass-native")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1if72k526sqqxnw250qwxvzwvh1w0k8ag4p4xq3442b22hywx72i"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/browserpass/browserpass-native"
           #:install-source? #f
           #:phases
           #~(modify-phases %standard-phases
               (add-before 'build 'patch-makefile
                 (lambda _
                   ;; This doesn't go in #:make-flags because the Makefile
                   ;; itself gets installed.
                   (substitute* "src/github.com/browserpass/browserpass-native/Makefile"
                     (("PREFIX \\?= /usr")
                      (string-append "PREFIX ?= " #$output)))))
               (add-before 'build 'configure
                 (lambda _
                   (with-directory-excursion
                       "src/github.com/browserpass/browserpass-native"
                     (invoke "make" "configure"))))
               (replace 'build
                 (lambda _
                   (with-directory-excursion
                       "src/github.com/browserpass/browserpass-native"
                     (invoke "make"))))
               (replace 'install
                 (lambda _
                   (with-directory-excursion
                       "src/github.com/browserpass/browserpass-native"
                     (invoke "make" "install"))))
               (add-after 'install 'wrap-executable
                 (lambda _
                   (wrap-program (string-append #$output "/bin/browserpass")
                     `("PATH" ":" prefix
                       (,(string-append #$(this-package-input "gnupg") "/bin")))))))))
    (native-inputs
     (list which))
    (inputs
     (list bash-minimal gnupg go-github-com-mattn-go-zglob
           go-github-com-rifflock-lfshook go-github-com-sirupsen-logrus
           go-golang-org-x-sys))
    (home-page "https://github.com/browserpass/browserpass-native")
    (synopsis "Browserpass native messaging host")
    (description "Browserpass is a browser extension for pass, a
UNIX-based password store manager.  It allows you to auto-fill or copy to
clipboard credentials for the current domain, protecting you from phishing
attacks.

This package only contains the Browserpass native messaging host.  You must
also install the browser extension for GNU IceCat or ungoogled-chromium
separately.")
    (license license:isc)))

(define-public cpass
  (package
    (name "cpass")
    (version "0.9.4")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "cpass" version))
              (sha256
               (base32
                "1zp3a8mgqxn916fzj1v2yhgnl7v6s0vnd0qcghqs3qq648qmlwr5"))))
    (build-system python-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'patch-pass-refs
                 (lambda* (#:key inputs #:allow-other-keys)
                   (substitute* "cpass.py"
                     (("'pass'")
                      (string-append "'"
                                     (search-input-file inputs
                                                        "bin/pass")
                                     "'"))
                     (("\\.password_store")
                      ".password-store")))))))
    (inputs (list password-store))
    (propagated-inputs (list python-urwid))
    (home-page "https://github.com/OliverLew/cpass")
    (synopsis "Textual interface for @command{pass}")
    (description
     "@command{cpass} is a terminal user interface for @command{pass}.
It supports both vim-like keybindings and the mouse.")
    (license license:expat)))

(define-public argon2
  (package
    (name "argon2")
    (version "20190702")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/P-H-C/phc-winner-argon2")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "01rwanr4wmr9vm6c712x411wig543q195z2icn388z892a93lc7p"))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"
       #:make-flags (list (string-append "CC=" ,(cc-for-target))
                          (string-append "PREFIX=" (assoc-ref %outputs "out"))
                          "LIBRARY_REL=lib"
                          (string-append "ARGON2_VERSION=" ,version)
                          "OPTTEST=1")  ; disable CPU optimization
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))         ; No configure script.
    (home-page "https://www.argon2.com/")
    (synopsis "Password hashing library")
    (description "Argon2 provides a key derivation function that was declared
winner of the 2015 Password Hashing Competition.")
    ;; Argon2 is dual licensed under CC0 and ASL 2.0.  Some of the source
    ;; files are CC0 only; see README.md and LICENSE for details.
    (license (list license:cc0 license:asl2.0))))

(define-public pass-git-helper
  (package
    (name "pass-git-helper")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/languitar/pass-git-helper")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "18nvwlp0w4aqj268wly60rnjzqw2d8jl0hbs6bkwp3hpzzz5g6yd"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'patch-pass-path
           (lambda* (#:key inputs #:allow-other-keys)
             (let* ((password-store (assoc-ref inputs "password-store"))
                    (pass (string-append password-store "/bin/pass")))
               (substitute* '("passgithelper.py"
                              "test_passgithelper.py")
                 (("'pass'") (string-append "'" pass "'")))
               #t)))
         (replace 'check
           (lambda _
             (setenv "HOME" (getcwd))
             (invoke "pytest"))))))
    (inputs
     (list python-pyxdg password-store))
    (native-inputs
     (list python-pytest python-pytest-mock))
    (home-page "https://github.com/languitar/pass-git-helper")
    (synopsis "Git credential helper interfacing with pass")
    (description "pass-git-helper is a git credential helper which
uses pass, the standard unix password manager, as the credential backend for
your git repositories.  This is achieved by explicitly defining mappings
between hosts and entries in the password store.")
    (license license:lgpl3+)))

(define-public john-the-ripper-jumbo
  (let ((official-version "1.9.0")
        (jumbo-version "1"))
    (package
      (name "john-the-ripper-jumbo")
      (version (string-append official-version "-" jumbo-version))
      (source
       (origin
         (method url-fetch)
         (uri (string-append "http://www.openwall.com/john/j/john-"
                             official-version "-jumbo-" jumbo-version ".tar.xz"))
         (sha256
          (base32 "0fvz3v41hnaiv1ggpxanfykyfjq79cwp9qcqqn63vic357w27lgm"))
         (patches (search-patches "john-the-ripper-jumbo-with-gcc-11.patch"))))
      (build-system gnu-build-system)
      (native-inputs
       (list perl))
      (inputs
       (list gmp
             libpcap
             nss
             openssl
             python-wrapper
             ruby                       ; For genincstats.rb
             zlib))
      (arguments
       `(#:configure-flags
         (list "--with-systemwide"
               ;; Do not test for instruction set in configure, and do not
               ;; pass '-march=native' to gcc:
               "--disable-native-tests"
               "--disable-native-march"
               ,(string-append
                 "--enable-simd="
                 (let ((system (or (%current-target-system)
                                   (%current-system))))
                   (cond
                    ((or (string-prefix? "x86_64" system)
                         (string-prefix? "i686" system)) "sse2")
                    ((string-prefix? "aarch" system) "neon")
                    (else "no")))))
         #:phases
         (modify-phases %standard-phases
           (add-before 'configure 'chdir-src
             (lambda _ (chdir "src") #t))
           (replace 'install
             (lambda _
               (let ((bindir (string-append %output "/bin"))
                     (docdir (string-append %output "/share/doc/john"))
                     (execdir (string-append %output "/libexec/john"))
                     (datadir (string-append %output "/share/john"))
                     (install-file-to (lambda (dir)
                                        (lambda (f) (install-file f dir))))
                     (symlink? (lambda (_ s) (eq? (stat:type s) 'symlink))))
                 (with-directory-excursion "../run"
                   (for-each (install-file-to bindir)
                             (cons*
                              "john" "makechr" "cprepair" "SIPdump" "tgtsnarf"
                              "genmkvpwd" "mkvcalcproba" "calc_stat" "raw2dyna"
                              (find-files "." "(to|2)?john(-[^.]*)?$")))
                   (for-each (lambda (f) ; Install symlinked aliases
                               (let ((tgt (string-append bindir "/" (basename f))))
                                 ;; The use of install-file above dereferences
                                 ;; symlinks.  We'd rather have the symlinks
                                 ;; for clarity, so remove tgt before linking.
                                 (when (file-exists? tgt) (delete-file tgt))
                                 (symlink "john" tgt)))
                             (find-files "." symlink?))
                   (for-each (install-file-to execdir)
                             (cons* "mailer" "benchmark-unify" "relbench"
                                    (find-files "." ".*\\.js")))
                   (for-each (lambda (f)
                               (let* ((base (basename f))
                                      (name (substring base 0 (string-index base #\.)))
                                      (link (string-append bindir "/" name)))
                                 (install-file f execdir)
                                 (when (and (executable-file? f)
                                            (not (file-exists? link)))
                                   (symlink (string-append execdir "/" base) link))))
                             (find-files "." ".*\\.(pl|py|rb|lua)"))
                   (for-each (install-file-to datadir)
                             (append (find-files "." "(stats|dictionary.*)")
                                     (find-files "." "(.*\\.chr|.*\\.lst)")
                                     (find-files "." ".*\\.conf")))
                   (copy-recursively "rules" (string-append datadir "/rules")))
                 (copy-recursively "../doc" docdir))))
           (delete 'check) ; Tests need installed .conf files; move after install
           (add-after 'install 'check
             (lambda args
               (setenv "HOME" "/tmp")   ; Some tests need to write to ~/.john
               (setenv "OMP_NUM_THREADS" (number->string (parallel-job-count)))
               (apply (assoc-ref %standard-phases 'check) args))))))
      (home-page "http://www.openwall.com/john/")
      (synopsis "Password cracker")
      (description "John the Ripper is a fast password cracker.  Its primary
purpose is to detect weak Unix passwords.  Besides several @code{crypt}
password hash types most commonly found on various Unix systems, supported out
of the box are Windows LM hashes, plus lots of other hashes and ciphers.  This
is the community-enhanced, \"jumbo\" version of John the Ripper.")
      (license license:gpl2+))))

(define-public fpm2
  (package
    (name "fpm2")
    (version "0.79")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://als.regnet.cz/fpm2/download/fpm2-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "19sdy1lygfhkg5nxi2w9a4d9kwvw24nxp0ix0p0lz91qpvk9qpnm"))))
    (build-system gnu-build-system)
    (inputs `(("gtk2" ,gtk+-2)
              ("gnupg" ,gnupg)
              ("libxml2" ,libxml2)))
    (native-inputs (list pkg-config intltool))
    (arguments
     `(#:configure-flags '("CFLAGS=-O2 -g -fcommon")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'pre-configure
           ;; The file po/POTFILES.in ends up missing for some reason in
           ;; both nix and guix builds. Adding the file with contents
           ;; found during troubleshooting.
           (lambda _
             (call-with-output-file "po/POTFILES.in"
               (lambda (port)
                 (format port "data/fpm2.desktop.in
data/fpm2.desktop.in.in
fpm2.glade
src/callbacks.c
src/fpm.c
src/fpm_file.c
src/interface.c
src/support.c
fpm2.glade
")))
             #t)))))
    (synopsis "Manage, generate and store passwords encrypted")
    (description "FPM2 is GTK2 port from Figaro's Password Manager
originally developed by John Conneely, with some new enhancements.

Upstream development seems to have stopped.  It is therefore recommended
to use a different password manager.")
    (home-page "https://als.regnet.cz/fpm2/")
    (license license:gpl2+)))

(define-public pass-rotate
  (package
    (name "pass-rotate")
    (version "0.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ddevault/pass-rotate")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1m067vvdlc85csbpkp8aw4s3ags7q8s3jszrr32kmj9qhk5c254f"))))
    (build-system python-build-system)
    (inputs
     (list python-beautifulsoup4 python-docopt python-html5lib
           python-requests))
    (home-page "https://github.com/ddevault/pass-rotate")
    (synopsis "Rotate password on online services")
    (description "pass-rotate is a command line utility and python library for
rotating passwords on various web services.  It makes it easier to rotate your
passwords, one at a time or in bulk, when security events or routine upkeep of
your online accounts makes it necessary.")
    (license license:expat)))

(define-public hashcat
  (package
    (name "hashcat")
    (version "6.2.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hashcat.net/files/hashcat-" version
                                  ".tar.gz"))
              (sha256
               (base32
                "0akv1cgbmwyw8h8zbw5w5ixh92y95sdadh8qiz60hjgkpivi0pmj"))
              (modules '((guix build utils)))
              ;; Delete bundled libraries.
              (snippet
               ;; TODO: Unbundle LZMA-SDK as well
               #~(for-each delete-file-recursively
                           '("deps/zlib" "deps/xxHash" "deps/OpenCL-Headers")))))
    (inputs (list minizip opencl-headers xxhash zlib))
    (build-system gnu-build-system)
    (arguments
     (list #:tests? #f ;no tests
           #:make-flags #~(list (string-append "PREFIX=" #$output)
                                (string-append "AR=" #$(ar-for-target))
                                (string-append "CC=" #$(cc-for-target))
                                (string-append "USE_SYSTEM_ZLIB=1")
                                (string-append "USE_SYSTEM_OPENCL=1")
                                (string-append "USE_SYSTEM_XXHASH=1"))
           #:phases #~(modify-phases %standard-phases
                        (add-after 'unpack 'fix-reproducibility
                          (lambda _
                            (substitute* "src/Makefile"
                              (("\\$\\(shell date \\+%s\\)")
                               "0"))))
                        (delete 'configure))))
    (home-page "https://hashcat.net/hashcat/")
    (synopsis "Advanced password recovery utility")
    (description
     "Hashcat is an password recovery utility, supporting five
unique modes of attack for over 200 highly-optimized hashing algorithms.
Hashcat currently supports CPUs, GPUs, and other hardware accelerators on
Linux, Windows, and macOS, and has facilities to help enable distributed
password cracking.")
    (license license:expat)))

(define-public hashcat-utils
  (package
    (name "hashcat-utils")
    (version "1.9")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/hashcat/hashcat-utils/releases/"
                           "download/v" version "/"
                           "hashcat-utils-" version ".7z"))
       (sha256
        (base32 "0kq555kb338691qd7zjmi8vhq4km3apnsl2w63zh0igwzcjx6lx1"))))
    (native-inputs
     (list p7zip))
    (inputs
     (list perl))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ;no tests
       #:make-flags (list "CC=gcc"
                          ;; Upstream bug(?): "make all" seems to remove the
                          ;; Perl scripts from the source.
                          "native")
       #:phases
       (modify-phases %standard-phases
         (replace 'unpack
           (lambda* (#:key source #:allow-other-keys)
             (invoke "7z" "x" source)
             (chdir (string-append "hashcat-utils-" ,version "/src"))
             #t))
         (delete 'configure)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (string-append (assoc-ref outputs "out") "/bin")))
               (mkdir-p out)
               (for-each
                (lambda (file)
                  (copy-file file (string-append out "/"
                                                 (basename file ".bin"))))
                (find-files "." "\\.bin$"))
               (for-each
                (lambda (file)
                  (copy-file file (string-append out "/"
                                                 (basename file ".pl"))))
                (find-files "../bin" "\\.pl$"))
               #t))))))
    (home-page "https://github.com/hashcat/hashcat-utils/")
    (synopsis "Small utilities that are useful in advanced password cracking")
    (description "Hashcat-utils are a set of small utilities that are useful
in advanced password cracking.  They all are packed into multiple stand-alone
binaries.  All of these utils are designed to execute only one specific
function.  Since they all work with @code{STDIN} and @code{STDOUT} you can
group them into chains.")
    (license license:expat)))

(define-public zxcvbn-c
  (package
    (name "zxcvbn-c")
    (version "2.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/tsyrogit/zxcvbn-c")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1clm9sxz6q360xk6vq5hi3b0mq374nl958mjcfxk94l692zrpaj4"))))
    (build-system gnu-build-system)
    (arguments
     (list #:make-flags
           #~(list (string-append "CC=" #$(cc-for-target))
                   (string-append "CXX=" #$(cxx-for-target)))
           #:test-target "test"
           #:phases
           #~(modify-phases %standard-phases
               (delete 'configure)
               (replace 'install
                 (lambda _
                   (install-file "zxcvbn.h" (string-append #$output "/include"))
                   (for-each (lambda (file)
                               (install-file file
                                             (string-append #$output "/lib")))
                             (find-files "." "libzxcvbn\\.so.*")))))))
    (home-page "https://github.com/tsyrogit/zxcvbn-c")
    (synopsis "C/C++ implementation of the zxcvbn password strength estimation")
    (description "This library is a C/C++ implementation of the zxcvbn password
strength estimator.  It provides functions to rate password strength, by
comparing the password to several word lists, including English first and last
names.")
    (license license:expat)))

(define-public hydra
  (package
    (name "hydra")
    (version "9.5")
    (home-page "https://github.com/vanhauser-thc/thc-hydra")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0pg4alaznygngdzn4k6p540g059w7mpiakchsp0526f1b9s33lw1"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ;no test suite
       #:make-flags (list (string-append "CC="
                                         ,(cc-for-target)))))
    (native-inputs (list pkg-config))
    (inputs (list freerdp gtk+ openssl zlib))
    (synopsis "Gain access to a remote system by trying logins and passwords")
    (description
     "This package provides a tool to demonstrate how easy it is to gain
unauthorized access to a system by automatically attempting logins and
passwords.  It supports a wide range of protocols including SSH, SMTP and
HTTP.")
    (license license:agpl3+)))

(define-public bruteforce-luks
  (package
    (name "bruteforce-luks")
    (version "1.4.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/glv2/bruteforce-luks")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1fhvm7ykqv2anny6zavd4iwh6gq5rp1r27p3zhn78sd3y34xhkmp"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake))
    (inputs
     (list cryptsetup))
    (synopsis "LUKS encrypted volume cracker")
    (description
     "This is a cracker for LUKS encrypted volumes.  It can be used either in
exhaustive mode to try every password given a charset or in dictionary mode to
try every password contained in a file.")
    (home-page "https://github.com/glv2/bruteforce-luks")
    (license license:gpl3+)))

(define-public bruteforce-salted-openssl
  (package
    (name "bruteforce-salted-openssl")
    (version "1.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/glv2/bruteforce-salted-openssl")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "00w1szj04jb63rh7sq1spc50013jgmz2nwm8k552i9ir8h4phw45"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake))
    (inputs
     (list openssl))
    (synopsis "Bruteforce cracker for openssl encrypted files")
    (description
     "This is a cracker for openssl encrypted files.  It can be used either in
exhaustive mode to try every password given a charset or in dictionary mode to
try every password contained in a file.")
    (home-page "https://github.com/glv2/bruteforce-salted-openssl")
    (license license:gpl3+)))

(define-public makepasswd
  (let ((commit "3545d57d3a589a392d7eb0df36a5286785345c9e")
        (revision "1"))
    (package
      (name "makepasswd")
      (version (git-version "0.5.4" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/khorben/makepasswd")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0lspqyyxbk6h28yxnp7pd5aib161vrkzgasam5jpzn35n1jacx2j"))))
      (build-system gnu-build-system)
      (native-inputs
       (list pkg-config libxslt libxml2 docbook-xsl docbook-xml))
      (inputs
       (list libxcrypt openssl))
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (delete 'configure))
         #:make-flags (list "CC=gcc"
                            (string-append "PREFIX=" (assoc-ref %outputs "out")))
         #:tests? #f))  ;no tests
      (synopsis "Generate (pseudo-)random passwords and hashes")
      (description
       "Makepasswd is a program that generates pseudo-random passwords of a
desired length.  It can also generate their corresponding hashes for a given
encryption algorithm if so desired.")
      (home-page "https://github.com/khorben/makepasswd")
      (license license:gpl3))))

(define-public pass-tomb
  (package
    (name "pass-tomb")
    (version "1.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/roddhjav/pass-tomb")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1sjkbdm2i3v77nbnap8sypbfdqwxckc8h66g3ixjnyr6cqgcrdli"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (let ((out (assoc-ref %outputs "out")))
         (list (string-append "PREFIX=" out)
               (string-append "BASHCOMPDIR=" out "/etc/bash_completion.d")))
       #:test-target "tests"
       ;; tests are very dependent on system state (swap partition) and require
       ;; access to /tmp/zsh which is not in the build container.
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'set-tomb-path
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((tomb (assoc-ref inputs "tomb")))
               (substitute* "tomb.bash"
                 ((":-tomb")
                  (string-append ":-" tomb "/bin/tomb"))))))
         (delete 'configure))))
    (inputs
     (list tomb))
    (home-page "https://github.com/roddhjav/pass-tomb")
    (synopsis "Pass extension keeping the tree of passwords encrypted")
    (description "Pass-tomb provides a convenient solution to put your
password store in a Tomb and then keep your password tree encrypted when you
are not using it.  It uses the same GPG key to encrypt passwords and tomb,
therefore you don't need to manage more key or secret.  Moreover, you can ask
pass-tomb to automatically close your store after a given time.")
    (license license:gpl3+)))

(define-public pass-coffin
  (package
    (name "pass-coffin")
    (version "1.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ayushnix/pass-coffin")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1486ikwsdjsj74qf949vk47r8mfp2mbbdc3scs8786nnnkhzc89n"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f ;No tests
      #:make-flags #~(list (string-append "PREFIX="
                                          #$output)
                           (string-append "BASHCOMPDIR="
                                          #$output "/etc/bash_completion.d"))
      #:phases #~(modify-phases %standard-phases
                   (delete 'configure))))
    (inputs (list password-store tar))
    (home-page "https://github.com/ayushnix/pass-coffin")
    (synopsis "Pass extension to keep the tree of passwords encrypted")
    (description
     "Pass-coffin is a pass extension that hides the password store
data inside a GPG encrypted file, which we'll call a coffin.")
    (license license:gpl3)))

(define-public xkcdpass
  (package
    (name "xkcdpass")
    (version "1.19.4")
    (home-page "https://github.com/redacted/XKCD-password-generator")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "xkcdpass-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1din4fqxgcj74vcjrsmn19sv81raards39x8pd75hmfxqqgggnd6"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-manpage
           (lambda* (#:key outputs #:allow-other-keys)
             (install-file
              "xkcdpass.1"
              (string-append (assoc-ref outputs "out") "/share/man/man1")))))))
    (synopsis
     "Generate secure multiword passwords/passphrases, inspired by XKCD")
    (description
     "This package provides a flexible and scriptable password generator which
generates strong passphrases, inspired by
@url{https://xkcd.com/936/,XKCD 936}.")
    (license (list license:bsd-3 ;code
                   license:cc0 ;spanish, eff_large_de, french word lists
                   license:cc-by-sa3.0 ;finnish, italian word list
                   license:cc-by-sa4.0 ;norwegian word list
                   license:eupl1.1 ;finnish word list
                   license:gpl2 ;portuguese word list
                   license:gpl3 ;ger-anix word list
                   license:lgpl2.0 ;finnish word list
                   license:lgpl2.1 ;portuguese word list
                   license:mpl1.1)))) ;portuguese word list
