;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Cyril Roelandt <tipecaml@gmail.com>
;;; Copyright © 2014 Kevin Lemonnier <lemonnierk@ulrar.net>
;;; Copyright © 2015, 2017 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015-2023 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Nikita <nikita@n0.is>
;;; Copyright © 2017 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2017–2023 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2020, 2021, 2022 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2020 Brett Gilio <brettg@gnu.org>
;;; Copyright © 2021 WinterHound <winterhound@yandex.com>
;;; Copyright © 2022 Jai Vetrivelan <jaivetrivelan@gmail.com>
;;; Copyright © 2022 jgart <jgart@dismail.de>
;;; Copyright © 2023, 2025 Janneke Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2024, 2025 Ashish SHUKLA <ashish.is@lostca.se>
;;; Copyright © 2024 Christian Miller <christian.miller@dadoes.de>
;;; Copyright © 2024 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (gnu packages irc)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (guix build-system guile)
  #:use-module (guix build-system haskell)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system python)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system qt)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages aspell)
  #:use-module (gnu packages autogen)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages code)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages cyrus-sasl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages enchant)
  #:use-module (gnu packages file)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages geo)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-check)
  #:use-module (gnu packages golang-web)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages haskell-check)
  #:use-module (gnu packages haskell-crypto)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages javascript)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages lxqt)
  #:use-module (gnu packages man)
  #:use-module (gnu packages messaging)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages openldap)
  #:use-module (gnu packages kde)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages password-utils)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages prometheus)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages regex)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages web)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26))

(define-public glirc
  (package
    (name "glirc")
    (version "2.39.0.1")               ; inherited by glirc-* extensions below
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "glirc" version))
       (sha256
        (base32 "0jaywb43jfv6kzyz540k02mxdgw1shc6hn7kia21alssszkilh4r"))))
    (build-system haskell-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'configure 'update-constraints
           (lambda _
             (substitute* "glirc.cabal"
               (("vty\\s+>=5.35\\s+&&\\s+<5.36") "vty"))))
          (add-after 'install 'install-extra-documentation
            (lambda _
              (install-file "glirc.1"
                            (string-append #$output "/share/man/man1"))
              ;; The man page is very terse and punts to the GitHub wiki for real
              ;; information.  Some of that is also in the README, so install it.
              (install-file "README.md"
                            (string-append #$output "/share/doc/"
                                           #$name "-" #$version)))))))
    (native-inputs
     (list ghc-hunit))
    (inputs
     (list ghc-async
           ghc-attoparsec
           ghc-base64-bytestring
           ghc-config-schema
           ghc-config-value
           ghc-curve25519
           ghc-free
           ghc-githash
           ghc-hashable
           ghc-hookup
           ghc-hsopenssl
           ghc-irc-core
           ghc-kan-extensions
           ghc-lens
           ghc-network
           ghc-psqueues
           ghc-random
           ghc-regex-tdfa
           ghc-split
           ghc-unordered-containers
           ghc-vector
           ghc-vty))
    (home-page "https://github.com/glguy/irc-core")
    (synopsis "Console IRC client")
    (description
     "Glirc is a console IRC client that focuses on providing both high-detail
and concise views of an IRC connection.  All views and transformation are
dynamic and don't change the underlying model.  It also provides advanced
line-editing features including syntax-highlighting, multi-line buffering,
and argument placeholders.")
    (license license:isc)))

(define-public glirc-lua
  (package
    (name "glirc-lua")
    (version (package-version glirc))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/glguy/irc-core")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1hadxsahl30jhgk8vvcg7lwndzc282iybcjam87xx5c0lh0mfzan"))))
    (build-system meson-build-system)
    (arguments
       (list
        #:modules
        '((guix build meson-build-system)
          (guix build utils)
          (ice-9 match))
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'enter-subdirectory
              (lambda _
                (chdir "lua-extension")))
            (replace 'install
              (lambda _
                (install-file "glirc-lua.so" (string-append #$output "/lib"))))
            (add-after 'install 'set-lua-paths
              (lambda _
                (let ((x.y       #$(version-major+minor
                                    (package-version
                                     (this-package-native-input "lua"))))
                      (libraries (filter (match-lambda
                                           ((label . _)
                                            (string-prefix? "lua-" label)))
                                         '#$(package-native-inputs
                                             this-package))))
                  (setenv "LUA_PATH"
                          (string-join
                           (map (match-lambda
                                  ((_ dir)
                                   (string-append
                                    dir "/share/lua/" x.y "/?.lua;"
                                    dir "/share/lua/" x.y "/?/?.lua")))
                                libraries)
                           ";"))
                  (setenv "LUA_CPATH"
                          (string-join
                           (map (match-lambda
                                  ((_ dir)
                                   (string-append
                                    dir "/lib/lua/" x.y "/?.so;"
                                    dir "/lib/lua/" x.y "/?/?.so")))
                                libraries)
                           ";")))))
            (add-after 'set-lua-paths 'document
              (lambda _
                (with-directory-excursion "../lua-extension/doc"
                  ;; Guix's ldoc command is a shell script without a shebang.
                  (invoke "sh" "ldoc" ".")
                  (let ((doc (string-append #$output "/share/doc/" #$name)))
                    (mkdir-p doc)
                    (copy-recursively "api" doc)))))
            (add-after 'document 'leave-subdirectory
              ;; Let default phases like 'install-license-files do their thing.
              (lambda _
                (chdir ".."))))))
    (native-inputs
     (list pkg-config
           ;; For building the API documentation.
           lua lua-filesystem lua-ldoc lua-penlight))
    (inputs
     (list lua))
    (home-page (package-home-page glirc))
    (synopsis "Lua scripting extension to the glirc IRC client")
    (description
     "This extension lets you script the glirc IRC client using Lua.
To use it, you must tell @command{glirc} exactly where to find
@file{glirc-lua.so} by adding something like this to your
@file{~/.config/glirc/config}:

@example
extensions:
  * path: \"../../.guix-profile/lib/glirc-lua.so\"
    args: [\"example.lua\", @dots{}]
@end example

Also ensure that @file{example.lua} finds any Lua libraries it needs, e.g., by
setting @env{LUA_PATH} and @env{LUA_CPATH} in glirc's run-time environment.")
    (license (package-license glirc))))

(define-public quassel
  (package
    (name "quassel")
    (version "0.14.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://quassel-irc.org/pub/quassel-"
                            version ".tar.xz"))
        (sha256
         (base32
          "042fzssydvv35jjknziph8iyyjsyrsb2hp3d0ix0bqbagbrpf1q9"))
        (modules '((guix build utils)))
        ;; We don't want to install the bundled inxi script.
        (snippet
         '(begin
            (delete-file "data/scripts/inxi")))))
    (build-system qt-build-system)
    (arguments
      ;; The three binaries are not mutually exlusive, and are all built
      ;; by default.
     '(#:configure-flags '("-DBUILD_TESTING=ON"
                           ;;"-DWANT_QTCLIENT=OFF"
                           ;;"-DWANT_CORE=OFF"
                           ;;"-DWANT_MONO=OFF"
                           "-DWITH_KDE=OFF"
                           "-DWITH_BUNDLED_ICONS=ON"
                           "-DWITH_OXYGEN_ICONS=ON"
                           ;; This disables link previews.
                           "-DWITH_WEBENGINE=OFF")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-inxi-reference
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((inxi (search-input-file inputs "/bin/inxi")))
               (symlink inxi "data/scripts/inxi")))))))
    (native-inputs
     (list extra-cmake-modules pkg-config qttools-5))
    (inputs
     (list boost
           inxi-minimal
           libdbusmenu-qt
           perl
           qca
           qtbase-5
           qtmultimedia-5
           qtscript
           qtsvg-5
           snorenotify
           sonnet
           zlib))
    (home-page "https://quassel-irc.org/")
    (synopsis "Distributed IRC client")
    (description "Quassel is a distributed IRC client, meaning that one or more
clients can attach to and detach from the central core.  It resembles the
popular combination of screen and a text-based IRC client such as WeeChat or
irssi, but graphical.")
    (license (list license:gpl2 license:gpl3)))) ;; dual licensed

(define-public irssi
  (package
    (name "irssi")
    (version "1.4.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/irssi/irssi/"
                                  "releases/download/" version "/irssi-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "0qp05z2qfqhp5wawxqz7qwv2fh9sb801z032i5d7h8nn1b5m3abj"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (setenv "CONFIG_SHELL" (which "bash"))
               (invoke "./configure"
                       (string-append "--prefix=" out)
                       (string-append "--enable-true-color")
                       (string-append "--with-proxy")
                       (string-append "--with-socks")))))
         (add-before 'check 'set-home
           (lambda _
             (setenv "HOME" (getcwd)))))))
    (inputs
     (list glib ncurses openssl perl utf8proc))
    (native-inputs
     (list pkg-config))
    (home-page "https://irssi.org/")
    (synopsis "Extensible terminal-based IRC client")
    (description
     "Irssi is a text terminal-based @acronym{IRC, Internet relay chat} client.
It is completely themable and extensible through Perl scripts, of which many
have already been written by the community.

Plug-ins add support for other protocols like @acronym{SILC, Secure Internet Live
Conferencing} and @acronym{ICB, Internet Citizen's Band}.")
    (license license:gpl2+)))

(define-public weechat
  (package
    (name "weechat")
    (version "4.6.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/weechat/weechat")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1qiz87ribpn69kn1zbdjc216wlc5cvfjbim3d6b22n0zvsqdrqqj"))))
    (build-system cmake-build-system)
    (outputs '("out" "doc"))
    (native-inputs
     (append (list gettext-minimal pkg-config)
             (if (target-x86?)
                 (list ruby-asciidoctor)
                 '())))
    (inputs
     (list aspell
           curl
           gnutls
           libgcrypt
           ncurses
           zlib
           (list zstd "lib")
           ;; Scripting language plug-ins.
           guile-3.0
           lua-5.1
           perl
           python
           ruby
           tcl
           cjson))
    (arguments
     (list #:configure-flags
           #~(list "-DENABLE_PHP=OFF"
                   #$@(if (target-x86?)
                          #~("-DENABLE_MAN=ON"
                             "-DENABLE_DOC=ON"
                             "-DENABLE_DOC_INCOMPLETE=ON")
                          #~()))
           #:phases
           #~(modify-phases %standard-phases
               #$@(if (target-x86?)
                      #~((add-after 'install 'move-doc
                           (lambda* (#:key outputs #:allow-other-keys)
                             (let* ((out (assoc-ref outputs "out"))
                                    (doc (assoc-ref outputs "doc"))
                                    (from (string-append out "/share/doc/weechat"))
                                    (to (string-append doc "/share/doc/weechat")))
                               (mkdir-p (string-append doc "/share/doc"))
                               (rename-file from to)))))
                      #~()))))
    (synopsis "Extensible chat client")
    (description "WeeChat (Wee Enhanced Environment for Chat) is an
@dfn{Internet Relay Chat} (IRC) client, which is designed to be light and fast.
The client uses a curses frontend, and there are remote interfaces for Web,
Qt, Android, and Emacs.

Everything in WeeChat can be done with the keyboard, though it also supports
using a mouse.  It is customizable and extensible with plugins and scripts.")
    (home-page "https://weechat.org/")
    (license license:gpl3)))

(define-public srain
  (package
    (name "srain")
    (version "1.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/SrainApp/srain")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1qswvhx1s90jbsdx5znbc478v2ix3g0p6qm97cj7zzl0kx5kd780"))))
    (build-system meson-build-system)
    (arguments
     `(#:tests? #f ;there are no tests
       #:glib-or-gtk? #t))
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin")
       ("pkg-config" ,pkg-config)
       ("python" ,python-wrapper)
       ("python-sphinx" ,python-sphinx)))
    (inputs
     (list glib-networking
           gsettings-desktop-schemas
           gtk+
           libconfig
           libsecret
           libsoup-minimal-2
           openssl))
    (home-page "https://srain.im")
    (synopsis "Modern IRC client written in GTK")
    (description
     "Srain is an IRC client written in GTK.  It aims to be modern and easy to
use while still remaining useful to power users.  It also has partial support
for the IRCv3 protocol.")
    (license license:gpl3+)))

(define-public ircii
  (package
    (name "ircii")
    (version "20210314")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://ircii.warped.com/"
                                 name "-" version ".tar.gz"))
             (sha256
              (base32
               "04jczayv1vdn21fcf5zkfaa98sy7d6ydrv2sns2i67gvya2z28j3"))))
    (build-system gnu-build-system)
    ;; TODO: We should package a small socks4/5 library/server to configure
    ;; ircii with socks client. `ghc-socks' pulls in lots of haskell, which
    ;; is too big.
    (arguments
     `(#:tests? #f
       #:configure-flags (list
                          "--enable-ipv6"
                          "--with-emacs-meta-keys"
                          (string-append "--with-openssl="
                                         (assoc-ref %build-inputs "openssl")))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-bsdinstall-absolute-path-bins
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "bsdinstall"
               (("/bin/strip") "strip")
               (("/bin/cp") "cp")
               (("/bin/chmod") "chmod")
               (("/etc/chown") "chown")
               (("/bin/chgrp") "chgrp")
               (("/bin/mkdir") "mkdir")
               (("/bin/rm") "rm")
               (("/bin/mv") "mv")))))))
    (inputs
     (list ncurses openssl))
    (native-inputs
     (list pkg-config perl))
    (home-page "http://www.eterna.com.au/ircii/")
    (synopsis "Terminal-based IRC and ICB client")
    (description
     "ircII is a terminal based IRC and ICB client for UNIX systems.")
    (license license:bsd-3)))

(define-public catgirl
  (package
    (name "catgirl")
    (version "2.2a")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.causal.agency/catgirl")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0fvjx4a523bf2m522ya8r94ikhs8d864hrd85jn6bm414sga877p"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f                       ; no tests
      #:make-flags
      #~(list (string-append "prefix=" #$output)
              (string-append "CC=" #$(cc-for-target)))))
    (native-inputs
     (list universal-ctags pkg-config))
    (inputs
     (list libressl ncurses))
    (home-page "https://git.causal.agency/catgirl")
    (synopsis "TLS-only terminal IRC client")
    (description
     "@command{catgirl} is a TLS-only terminal IRC client.

Notable features include:
@itemize
@item Tab complete: most recently seen or mentioned nicks are completed first.
  Commas are inserted between multiple nicks.
@item Prompt: the prompt clearly shows whether input will be interpreted as a
command or sent as a message.
@item Split scroll: keeps the latest messages in view while scrolling.
@item URL detection: recent URLs from a particular user or matching a
substring can be opened or copied.
@item Nick coloring: color generation based on usernames remains stable across
nick changes.  Mentions of users in messages are colored.
@item Topic diffing: the modified portion of a channel topic change is
highlighted.
@end itemize")
    (license license:gpl3+)))

(define-public ii
  (package
    (name "ii")
    (version "2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://dl.suckless.org/tools/"
                                  name "-" version ".tar.gz"))
              (sha256
               (base32
                "0ns2wpzkk7qzhv7addgr0w5as0m7jwag5nxai2dr61wc436syrsg"))))
    (build-system gnu-build-system)
    (arguments
     (list #:tests? #f                  ; no tests
           #:make-flags
           #~(list (string-append "PREFIX=" #$output)
                   (string-append "CC=" #$(cc-for-target)))
           #:phases
           #~(modify-phases %standard-phases
               (delete 'configure))))   ; no configure script
    (home-page "https://tools.suckless.org/ii/")
    (synopsis "FIFO and file system based IRC client")
    (description
     "ii (Irc it) is a minimalist FIFO and file system based IRC client.")
    (license license:expat)))

(define-public sic
  (package
    (name "sic")
    (version "1.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://dl.suckless.org/tools/"
                                  name "-" version ".tar.gz"))
              (sha256
               (base32
                "1lk57mrrqgky37bjsnp72s8libywzsrbbjq8bpmz4xdw7smqyirh"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no tests
       #:make-flags (list ,(string-append "CC=" (cc-for-target))
                          (string-append "PREFIX=" %output))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)))) ; no configure
    (home-page "https://tools.suckless.org/sic/")
    (synopsis "Simple IRC client")
    (description
     "sic is a simple IRC client, even more minimalistic than ii.")
    (license license:expat)))

(define-public kirc
  (package
    (name "kirc")
    (version "0.3.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/mcpcpc/kirc")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32 "1ighpinss3k6xyqk05wrs76wvp2ahhh0jkkg8h7bhg66b14fsws9"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no tests
       #:make-flags
       (list (string-append "CC=" ,(cc-for-target))
             (string-append "PREFIX=" %output))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))         ; No ./configure script
    (home-page "https://mcpcpc.github.io/kirc/")
    (synopsis "IRC client written in POSIX C99")
    (description "Kirc is an Internet Relay Chat (IRC) client.  It includes
support for Simple Authentication and Security Layer (SASL), the
client-to-client (CTCP) protocol, simple chat history logging, synchronous
message handling, multi-channel joining at server connection, full support for
all RFC 2812 commands, and customized color scheme definitions.")
    (license license:expat)))

(define-public kvirc
  (package
    (name "kvirc")
    (version "5.2.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/kvirc/KVIrc")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0i3gkjv8l7w3smz6dv1734ja91y281bmfr5sajyzcclyc7yq7w24"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f)) ;no tests
    (native-inputs
     (list doxygen
           graphviz
           pkg-config))
    (inputs
     (list enchant
           gettext-minimal
           openssl
           perl
           python
           qtbase-5
           qtmultimedia-5
           qtsvg-5
           qtwebengine-5
           qtx11extras
           zlib))
    (home-page "https://www.kvirc.net/")
    (synopsis "IRC client based on QT GUI toolkit")
    (description
     "KVIrc is a IRC client based on the Qt GUI toolkit.")
    ;; doc/LICENSE-OPENSSL
    ;; doc/LICENSE-AMIP
    ;; GPL2+ mentioned in README
    (license (list license:gpl2+ license:openssl))))

(define-public limnoria
  (package
    (name "limnoria")
    (version "2019.11.22")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "limnoria" version))
       (sha256
        (base32 "0853xk1ps3v6lkmfx50wv56vynnzpl84v66hxnhl8i34zl36kk3c"))))
    (build-system python-build-system)
    (inputs
     (list python-pytz
           python-chardet
           python-dateutil
           python-gnupg
           python-feedparser
           python-sqlalchemy
           python-socksipy-branch
           python-ecdsa))
    (native-inputs
     (list python-mock))
    ;; Despite the existence of a test folder there is no test phase.
    ;; We need to package https://github.com/ProgVal/irctest and write
    ;; our own testphase.
    (arguments
     `(#:tests? #f))
    (home-page "https://github.com/ProgVal/Limnoria")
    (synopsis "Modified version of Supybot (an IRC bot and framework)")
    (description
     "Modified version of Supybot with Python 3 and IRCv3 support,
embedded web server, translations (fr, fi, it, hu, de), and many
other enhancements and bug fixes.")
    (license license:bsd-3)))

(define-public epic5
  (package
    (name "epic5")
    (version "2.0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://ftp.epicsol.org/pub/"
                                  "epic/EPIC5-PRODUCTION/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1ap73d5f4vccxjaaq249zh981z85106vvqmxfm4plvy76b40y9jm"))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-perl
           (lambda _
             (substitute* "regress/crash-irc"
               (("perl5") (which "perl")))
             #t))
         (add-after 'unpack 'patch-bsdinstall
           ;; If we just remove /bin/ some part of the bsdinstall breaks.
           ;; Furthermore bsdinstalls has a reference to /etc/chmod here, which
           ;; means if we leave /etc/ in, install fails.
           (lambda _
             (substitute* "bsdinstall"
               (("/bin/strip") "strip")
               (("/bin/cp") "cp")
               (("/bin/chmod") "chmod")
               (("/bin/chgrp") "chgrp")
               (("/bin/mkdir") "mkdir")
               (("/bin/rm") "rm")
               (("/bin/mv") "mv")
               (("/etc/") ""))
             #t))
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             ;; The tarball uses a very old version of autconf. It does not
             ;; understand extra flags like `--enable-fast-install', so
             ;; we need to invoke it with just what it understands.
             (let ((out (assoc-ref outputs "out")))
               ;; 'configure' doesn't understand '--host'.
               ,@(if (%current-target-system)
                     `((setenv "CHOST" ,(%current-target-system)))
                     '())
               (setenv "CONFIG_SHELL" (which "bash"))
               (setenv "SHELL" (which "bash"))
               (invoke "./configure"
                       (string-append "--prefix=" out)
                       "--with-ipv6" "--with-libarchive"
                       ;; We use libressl because openssl does not come
                       ;; with the lib/libssl.a which is needed for epic5.
                       ;; XXX: No matter which implementation is chosen,
                       ;; epic5 fails to connect to tls ports of roundrobin
                       ;; irc networks. This however is believed to be an
                       ;; protocol issue at epic5 related to ircd.
                       (string-append "--with-ssl="
                                      (assoc-ref %build-inputs "libressl"))
                       (string-append "--with-tcl="
                                      (assoc-ref %build-inputs "tcl")
                                      "/lib/tclConfig.sh"))))))))
    (inputs
     (list libressl
           ncurses
           libarchive ; CHANGELOG: "Support for loading zip files"
           perl
           tcl
           ruby))
    (native-inputs
     (list pkg-config))
    (home-page "http://epicsol.org")
    (synopsis "IRC Client")
    (description
     "EPIC is a IRC client that has been under active development for
over 20 years.  It is stable and mature, and offers an excellent ircII
interface for those who are accustomed to the ircII way of doing things.")
    (license (list license:bsd-3
                   license:isc
                   license:bsd-4
                   ;; The epic license is equal to the standard three-clause
                   ;; BSD license except that you are not permitted to remove the
                   ;; "Redistribution is permitted" clause of the license if you
                   ;; distribute binaries.
                   (license:non-copyleft "http://epicsol.org/copyright")))))

(define-public python-girc
  (package
    (name "python-girc")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "girc" version))
       (sha256
        (base32 "0gbx64j8782m1x2w9dkiynvshj43m0y4i0xnsiz0gsmyfl0jk8jl"))
       (snippet
        #~(begin (use-modules (guix build utils))
                 (substitute* '("girc/utils.py"
                                "girc/imapping.py")
                   (("collections.MutableSequence")
                    "collections.abc.MutableSequence")
                   (("collections.MutableMapping")
                    "collections.abc.MutableMapping")
                   (("collections.Mapping")
                    "collections.abc.Mapping"))))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      '(modify-phases %standard-phases
         (add-after 'unpack 'delete-some-tests
           ;; This file depends on python-irc-parser-tests, which depends on
           ;; this package.
           (lambda _ (delete-file "tests/test_parse.py"))))))
    (propagated-inputs (list python-docopt))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/DanielOaks/girc")
    (synopsis "IRC library for Python")
    (description
     "This package provides an IRC library for Python, based on asyncio.")
    (license license:isc)))

(define-public python-ircmatch
  (package
    (name "python-ircmatch")
    (version "1.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ircmatch" version))
       (sha256
        (base32 "1bn92bnk958c097jhwkas24i4a07h905hifix7bg111npc48536l"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://pypi.org/project/ircmatch/")
    (synopsis "Library for matching IRC masks based on atheme")
    (description "This is a python extension which provides string comparison
and matching functions from Atheme.  They are hand-optimized for high
performance when matching IRC hostmasks.")
    (license license:isc)))

(define-public python-irc-parser-tests
  (package
    (name "python-irc-parser-tests")
    (version "0.0.4")
    (source
     (origin
       (method git-fetch) ; PyPI has a broken tests and data locations
       (uri (git-reference
             (url "https://github.com/ircdocs/parser-tests")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0x0psq31f43d88b8jhaqwd9f1ykiqm4j13i8nxgcgkgp992cw002"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-pyyaml))
    (native-inputs
     (list python-girc python-ircmatch
           python-setuptools python-wheel))
    (home-page "https://github.com/ircdocs/parser-tests")
    (synopsis "Tests for various IRC protocol parsers")
    (description
     "This package provides a library of tests for various IRC protocol
parsers")
    (license (list license:cc0
                   license:public-domain))))

(define-public go-gopkg-in-irc-v3
  (package
    (name "go-gopkg-in-irc-v3")
    (version "3.1.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gopkg.in/irc.v3")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0f2vv947yf9ygy8ylwqkd9yshybfdsbsp9pffjyvm7l7rnq5da60"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "gopkg.in/irc.v3"
      #:phases
      #~(modify-phases %standard-phases
          ;; Testscases is a git submodule to
          ;; <https://github.com/go-irc/irc-parser-tests> which is an
          ;; unmaintained clone of <https://github.com/ircdocs/parser-tests>
          ;; which is packed in Guix as python-irc-parser-tests.  Tests data
          ;; (YAML files) are distributed as Python package and located in
          ;; <lib/python3.11/site-packages/parser_tests/data/>.
          (add-before 'check 'install-testcases-data
            (lambda* (#:key import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (mkdir-p "./testcases/tests")
                (for-each
                 (lambda (file)
                   (install-file file "./testcases/tests"))
                 (find-files
                  #$(this-package-native-input "python-irc-parser-tests") "\\.yaml$"))))))))
    (native-inputs
     (list go-github-com-stretchr-testify python-irc-parser-tests))
    (propagated-inputs
     (list go-gopkg-in-yaml-v2))
    (home-page "https://gopkg.in/irc.v3")
    (synopsis "Low-level IRC library for Go")
    (description "Package irc provides a simple IRC library meant as a
building block for other projects.")
    (license license:expat)))

(define-public go-gopkg-in-irc-v4
  (package
    (inherit go-gopkg-in-irc-v3)
    (name "go-gopkg-in-irc-v4")
    (version "4.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gopkg.in/irc.v4")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1yr7m1vz7fj0jbmk8njg54nyc9hx4kv24k13sjc4zj5fyqljj0p2"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "gopkg.in/irc.v4"
      #:phases
      #~(modify-phases %standard-phases
          ;; testcases is renamed to _testcases in v4 for some reason.
          (add-before 'check 'adjust-testcases-data
            (lambda* (#:key import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (mkdir-p "./_testcases/tests")
                (for-each
                 (lambda (file)
                   (install-file file "./_testcases/tests"))
                 (find-files
                  #$(this-package-native-input "python-irc-parser-tests") "\\.yaml$"))))))))
    (propagated-inputs
     (modify-inputs (package-propagated-inputs go-gopkg-in-irc-v3)
       (append go-golang-org-x-time)))))

(define-public chathistorysync
  (package
    (name "chathistorysync")
    (version "0.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.sr.ht/~emersion/chathistorysync")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "03dxr178wnicggx0k95wvyzgyk4s4g0adbi2z0md517a5qd1lh23"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "git.sr.ht/~emersion/chathistorysync"
           #:install-source? #f ; chathistorysync is an end-user application.
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'build 'doc
                 (lambda _
                   (with-directory-excursion
                       "src/git.sr.ht/~emersion/chathistorysync"
                     (invoke "sh" "-c"
                             "scdoc <chathistorysync.1.scd >chathistorysync.1"))))
               (add-after 'install 'install-doc
                 (lambda* (#:key outputs #:allow-other-keys)
                   (let* ((out (assoc-ref outputs "out")))
                     (with-directory-excursion
                         "src/git.sr.ht/~emersion/chathistorysync"
                       (install-file
                        "chathistorysync.1"
                        (string-append out "/share/man/man1")))))))))
    (inputs
     (list go-golang-org-x-sys
           go-golang-org-x-term
           go-golang-org-x-crypto
           go-gopkg-in-irc-v3))
    (native-inputs (list scdoc))
    (home-page "https://git.sr.ht/~emersion/chathistorysync")
    (synopsis "Synchronization tool for IRC chat history")
    (description
     "This package provides a synchronization tool for IRC chat history.")
    (license license:agpl3)))

(define-public litterbox
  (package
    (name "litterbox")
    (version "1.9")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://git.causal.agency/litterbox/snapshot/litterbox-"
                           version ".tar.gz"))
       (sha256
        (base32 "1ag5x7h71pxjaaf4b561rwdqr05zzywkc0p3jf2yhg3lbjkjrc7z"))))
    (build-system gnu-build-system)
    (arguments
     (list #:tests? #f ; There are no tests.
           #:make-flags
           #~(list
              (string-append "CC=" #$(cc-for-target))
              (string-append "PREFIX=" #$output))))
    (native-inputs
      (list pkg-config universal-ctags))
    (inputs
      (list libressl sqlite))
    (home-page "https://code.causal.agency/june/litterbox")
    (synopsis "TLS-only IRC logger")
    (description
"@command{litterbox} is a TLS-only IRC logger.  It logs
events from IRC in a SQLite database, indexing messages for full-text
search.  It is intended for use with the IRC bouncer @command{pounce},
but can also be used independently as a logging bot.")
    (license license:gpl3+)))

(define-public inspircd
  (package
    (name "inspircd")
    (version "3.12.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/inspircd/inspircd")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0xlfs269iaw7dfryzl6vjzqsn2g4nqh6kpf5xfgk3zbjhqaczknx"))))
    (build-system gnu-build-system)
    (arguments
     (list #:configure-flags
           #~(map (lambda (module)
                    (string-append "--enable-extras=" module))
                  '("m_argon2.cpp"
                    "m_geo_maxmind.cpp"
                    "m_ldap.cpp"
                    "m_mysql.cpp"
                    "m_pgsql.cpp"
                    "m_regex_pcre.cpp"
                    "m_regex_posix.cpp"
                    "m_regex_stdlib.cpp"
                    "m_regex_re2.cpp"
                    "m_regex_tre.cpp"
                    "m_sqlite3.cpp"
                    "m_ssl_gnutls.cpp"
                    "m_ssl_openssl.cpp"
                    "m_ssl_mbedtls.cpp"
                    "m_sslrehashsignal.cpp"))
           #:tests? #f                  ; XXX figure out later
           #:phases
           #~(modify-phases %standard-phases
               (add-before 'configure 'module-configure
                 (lambda* (#:key configure-flags #:allow-other-keys)
                   (apply invoke "./configure"
                          configure-flags)))
               (replace 'configure
                 (lambda _
                   (let ((lib (string-append #$output "/lib/"))
                         (bin (string-append #$output "/bin/"))
                         (etc (string-append #$output "/etc/"))
                         (name "inspircd"))
                     (invoke "./configure"
                             (string-append "--prefix=" lib name)
                             (string-append "--binary-dir=" bin)
                             (string-append "--module-dir=" lib name "/modules/")
                             (string-append "--config-dir=" etc name))))))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list argon2
           gnutls
           libmaxminddb
           mbedtls-lts
           (list mariadb "dev")
           openldap
           openssl
           `(,pcre "bin")
           perl
           postgresql
           re2
           sqlite
           tre))
    (synopsis "Modular IRC server written in C++")
    (description "InspIRCd is a modular Internet Relay Chat
server written in C++ for Unix-like operating systems.")
    (home-page "https://www.inspircd.org/")
    (license license:gpl2)))

(define-public snuik
  (package
    (name "snuik")
    (version "0.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://dezyne.org/download/snuik/"
                           name "-" version ".tar.gz"))
       (sha256
        (base32 "1hqhni5xgm7jg8md305clix1r3dbxkq6fw93kxzar1nv7wvy7z38"))))
    (native-inputs (list guile-3.0
                         ngircd))       ;for live test
    (inputs
     (list bash-minimal guile-3.0 guile-fibers guile-gnutls guile-goblins))
    (build-system guile-build-system)
    (arguments
     (list
      #:not-compiled-file-regexp "(guix|guix/.*)[.]scm$"
      #:modules '((srfi srfi-1)
                  (ice-9 popen)
                  (guix build guile-build-system)
                  (guix build utils))
      #:phases
      #~(modify-phases %standard-phases
          #$@(if (%current-target-system)
                 #~()
                 #~((add-after 'build 'check
                      (lambda _
                        (let* ((tests (find-files "test" "[.]scm$"))
                               (guile #$(this-package-input "guile"))
                               (guile (string-append guile "/bin/guile")))
                          (fold (lambda (test result)
                                  (and
                                   result
                                   (invoke guile "--no-auto-compile" test)))
                                #t
                                tests))))))
          (add-after 'build 'install-script
            (lambda _
              (let* ((bash #$(this-package-input "bash-minimal"))
                     (bash (string-append bash "/bin/bash"))
                     (guile #$(this-package-input "guile"))
                     (guile (string-append guile "/bin/guile"))
                     (build-guile #$(this-package-native-input "guile"))
                     (build-guile (string-append build-guile "/bin/guile"))
                     (guile-fibers #$(this-package-input "guile-fibers"))
                     (guile-gnutls #$(this-package-input "guile-gnutls"))
                     (guile-goblins #$(this-package-input "guile-goblins"))
                     (out #$output)
                     (bin (string-append out "/bin"))
                     (effective (read
                                 (open-pipe* OPEN_READ
                                             build-guile "-c"
                                             "(write (effective-version))")))
                     (path (list (string-append guile "/bin")))
                     (scm-dir (string-append "/share/guile/site/" effective))
                     (scm-path (list (string-append out scm-dir)
                                     (string-append guile-fibers scm-dir)
                                     (string-append guile-gnutls scm-dir)
                                     (string-append guile-goblins scm-dir)))
                     (go-dir (string-append "/lib/guile/" effective
                                            "/site-ccache/"))
                     (go-path (list (string-append out go-dir)
                                    (string-append guile-fibers go-dir)
                                    (string-append guile-gnutls go-dir)
                                    (string-append guile-goblins go-dir))))
                (mkdir-p "bin")
                (with-output-to-file "bin/snuik"
                  (lambda _
                    (display "\
#!@GUILE@ --no-auto-compile
!#
(set! %load-path (append '(\"@guilemoduledir@\") %load-path))
(set! %load-compiled-path (append '(\"@guileobjectdir@\") %load-compiled-path))
((@ (snuik) main) (command-line)) ")))
                (chmod "bin/snuik" #o755)
                (substitute* "bin/snuik"
                  (("@GUILE@") guile)
                  (("@guilemoduledir@") (string-append #$output "/" scm-dir))
                  (("@guileobjectdir@")  (string-append #$output "/" go-dir)))
                (chmod "snuik" #o755)
                (install-file "bin/snuik" bin)
                (wrap-program (string-append #$output "/bin/snuik")
                  `("PATH" ":" prefix ,path)
                  `("GUILE_AUTO_COMPILE" ":" = ("0"))
                  `("GUILE_LOAD_PATH" ":" prefix ,scm-path)
                  `("GUILE_LOAD_COMPILED_PATH" ":" prefix ,go-path))))))))
    (home-page "https://gitlab.com/janneke/snuik")
    (synopsis "IRC bot using Guile-goblins")
    (description "@code{Snuik} is an IRC bot in Guile Goblins.  It has
some basic functionality only, such as greeting, feed, identify, info,
seen, tell, and what.")
    (license license:gpl3+)))

(define-public soju
  (package
    (name "soju")
    (version "0.8.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://codeberg.org/emersion/soju")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1dk9w87ksjvbhnchyyl4yhdlhjnc9s9hpzhykfiyh935g75zv66c"))))
    (build-system go-build-system)
    (arguments
     (list
      #:install-source? #f
      #:import-path "codeberg.org/emersion/soju"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'adjust-makefile
            (lambda* (#:key import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (substitute* "Makefile"
                  ;; Do not set default config path.
                  ((".*config_path.*:.*") "")
                  (("-X.*=.*config_path.*' ") "")
                  ((".*cp -f.*config_path.*") "")
                  ;; Prevent creating /var/lib/soju.
                  ((".*sharedstatedir.*") "")))))
          (replace 'build
            (lambda* (#:key import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (setenv "GOFLAGS" "-v -x -trimpath -tags=pam")
                (setenv "SYSCONFDIR" (string-append #$output "/etc"))
                (invoke "make"))))
          (replace 'install
            (lambda* (#:key import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (setenv "PREFIX" #$output)
                (invoke "make" "install")))))))
    (native-inputs
     (list go-git-sr-ht-emersion-go-scfg
           go-git-sr-ht-emersion-go-sqlite3-fts5
           go-git-sr-ht-sircmpwn-go-bare
           go-github-com-coder-websocket
           go-github-com-emersion-go-sasl
           go-github-com-lib-pq
           go-github-com-mattn-go-sqlite3
           go-github-com-msteinert-pam-v2
           go-github-com-pires-go-proxyproto
           go-github-com-prometheus-client-golang
           go-github-com-sherclockholmes-webpush-go
           go-golang-org-x-crypto
           go-golang-org-x-time
           go-google-golang-org-protobuf
           go-gopkg-in-irc-v4
           scdoc))
    (home-page "https://soju.im/")
    (synopsis "User-friendly IRC bouncer")
    (description
     "Connects to upstream IRC servers on behalf of the user to provide
extra functionality. soju supports many features
such as multiple users, numerous @@url{https://ircv3.net/,IRCv3} extensions,
chat history playback and detached channels.  It is well-suited for both small
and large deployments.")
    (license license:agpl3)))

