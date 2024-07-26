;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Siniša Biđin <sinisa@bidin.eu>
;;; Copyright © 2015, 2017, 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016, 2017, 2018 Nikita <nikita@n0.is>
;;; Copyright © 2017 Danny Milosavljevic <dannym@scratchpost.org>
;;; Copyright © 2017, 2018 Alex Vong <alexvong1995@gmail.com>
;;; Copyright © 2017–2019, 2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Timothy Sample <samplet@ngyro.com>
;;; Copyright © 2018 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2016, 2017 Leo Famulari <leo@famulari.name>
;;; Copyright © 2015 Paul van der Walt <paul@denknerd.org>
;;; Copyright © 2019, 2020 Kyle Meyer <kyle@kyleam.com>
;;; Copyright © 2015 John Soo <jsoo1@asu.edu>
;;; Copyright © 2019, 2020, 2022, 2023 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2019, 2020 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2020 Alexandru-Sergiu Marton <brown121407@member.fsf.org>
;;; Copyright © 2020 Brian Leung <bkleung89@gmail.com>
;;; Copyright © 2021 EuAndreh <eu@euandre.org>
;;; Copyright © 2021 Stefan Reichör <stefan@xsteve.at>
;;; Copyright © 2021 Morgan Smith <Morgan.J.Smith@outlook.com>
;;; Copyright © 2022 David Thompson <dthompson2@worcester.edu>
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

(define-module (gnu packages haskell-apps)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system haskell)
  #:use-module (gnu packages)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages haskell)
  #:use-module (gnu packages haskell-check)
  #:use-module (gnu packages haskell-crypto)
  #:use-module (gnu packages haskell-web)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages lsof)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages rsync)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages xorg))

(define-public apply-refact
  (package
    (name "apply-refact")
    (version "0.10.0.0")
    (source (origin
              (method url-fetch)
              (uri (hackage-uri "apply-refact" version))
              (sha256
               (base32
                "129bf8n66kpwh5420rxprngg43bqr2agyd8q8d7l49k2rxsjl1fb"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "apply-refact")))
    (inputs (list ghc-refact
                  ghc-exactprint
                  ghc-paths
                  ghc-extra
                  ghc-syb
                  ghc-filemanip
                  ghc-uniplate
                  ghc-unix-compat
                  ghc-optparse-applicative))
    (native-inputs (list ghc-tasty ghc-tasty-golden ghc-tasty-expected-failure
                         ghc-silently))
    (home-page "https://github.com/mpickering/apply-refact")
    (synopsis "Perform refactorings specified by the refact library")
    (description
     "This package lets you perform refactorings specified by the refact
library.  It is primarily used with HLint's @code{--refactor} flag.")
    (license license:bsd-3)))

;; cabal-install depends on Cabal, which is part of GHC. You can only
;; update this packages after updating GHC.
(define-public cabal-install
 (package
   (name "cabal-install")
   (version "3.6.2.0")
   (source (origin
             (method url-fetch)
             (uri (hackage-uri "cabal-install" version))
             (sha256
              (base32
               "0dihpm4h3xh13vnpvwflnb7v614qdvljycc6ffg5cvhwbwfrxyfw"))))
   (build-system haskell-build-system)
   (properties '((upstream-name . "cabal-install")))
   (inputs (list ghc-async
                 ghc-base16-bytestring
                 ghc-cryptohash-sha256
                 ghc-echo
                 ghc-edit-distance
                 ghc-hashable
                 ghc-http
                 ghc-network-uri
                 ghc-random
                 ghc-tar
                 ghc-zlib
                 ghc-hackage-security
                 ghc-regex-base
                 ghc-regex-posix
                 ghc-resolv
                 ghc-lukko))
   (arguments
    `(#:cabal-revision ("2"
                        "1kpgyfl5njxp4c8ax5ziag1bhqvph3h0pn660v3vpxalz8d1j6xv")))
   (home-page "https://www.haskell.org/cabal/")
   (synopsis "Command-line interface for Cabal and Hackage")
   (description
    "The cabal command-line program simplifies the process of managing
Haskell software by automating the fetching, configuration, compilation and
installation of Haskell libraries and programs.")
   (license license:bsd-3)))

(define-public cpphs
  (package
    (name "cpphs")
    (version "1.20.9.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "cpphs" version))
       (sha256
        (base32
         "17wi7fma2qaqdm1hwgaam3fd140v9bpa8ky0wg708h1pqc5v2nbz"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "cpphs")))
    (inputs
     (list ghc-polyparse ghc-old-locale ghc-old-time))
    (arguments
     `(#:cabal-revision ("1"
                         "1f8jzs8zdh4wwbcq8fy6qqxkv75ypnvsm4yzw49wpr3b9vpnzlha")))
    (home-page "https://projects.haskell.org/cpphs/")
    (synopsis "Liberalised re-implementation of cpp, the C pre-processor")
    (description "Cpphs is a re-implementation of the C pre-processor that is
both more compatible with Haskell, and itself written in Haskell so that it
can be distributed with compilers.  This version of the C pre-processor is
pretty-much feature-complete and compatible with traditional (K&R)
pre-processors.  Additional features include: a plain-text mode; an option to
unlit literate code files; and an option to turn off macro-expansion.")
    (license (list license:lgpl2.1+ license:gpl3+))))

;; Darcs has no https support:
;; http://darcs.net/manual/Configuring_darcs.html#SECTION00440070000000000000
;; and results of search engines will show that if the protocol is http, https
;; is never mentioned.
(define-public darcs
  (package
    (name "darcs")
    (version "2.16.5")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "darcs" version))
       (sha256
        (base32
         "0ar4markr71l9hzrbgcz4q37cf2rf3936i6qi8p827p36v96qg6n"))
       (modules '((guix build utils)))
       ;; Remove time-dependent code for reproducibility.
       (snippet
        '(begin
           (substitute* "darcs/darcs.hs"
             (("__DATE__") "\"1970-01-01\"")
             (("__TIME__") "\"00:00:00\""))
           #t))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "darcs")))
    (arguments
     `(#:tests? #f ; TODO: Needs QuickCheck ==2.13.*, and more…
       #:configure-flags '("-fpkgconfig" "-fcurl" "-flibiconv" "-fthreaded"
                           "-fnetwork-uri" "-fhttp" "--flag=executable"
                           "--flag=library")
       #:haddock? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'patch-source-shebangs 'patch-sh
           (lambda _
             (substitute* "tests/issue538.sh"
               (("/bin/sh") (which "sh")))
             #t))
         (add-before 'configure 'update-constraints
           (lambda _
             (substitute* "darcs.cabal"
               (("(attoparsec|base|bytestring|constraints|cryptonite|hashable|memory|regex-tdfa|time)\\s+[^,]+" all dep)
                dep))))
         (add-after 'register 'remove-libraries
           (lambda* (#:key outputs #:allow-other-keys)
             (delete-file-recursively (string-append (assoc-ref outputs "out") "/lib")))))))
    (inputs (list ghc-regex-base
                  ghc-regex-tdfa
                  ghc-regex-applicative
                  ghc-fgl
                  ghc-html
                  ghc-memory
                  ghc-cryptonite
                  ghc-base16-bytestring
                  ghc-utf8-string
                  ghc-vector
                  ghc-tar
                  ghc-data-ordlist
                  ghc-attoparsec
                  ghc-zip-archive
                  ghc-async
                  ghc-constraints
                  ghc-unix-compat
                  ghc-old-time
                  ghc-temporary
                  ghc-hashable
                  ghc-mmap
                  ghc-zlib
                  ghc-network-uri
                  ghc-network
                  ghc-conduit
                  ghc-http-conduit
                  ghc-http-types
                  curl))
    (native-inputs (list ghc-cmdargs
                         ghc-findbin
                         ghc-quickcheck
                         ghc-leancheck
                         ghc-hunit
                         ghc-test-framework
                         ghc-test-framework-hunit
                         ghc-test-framework-quickcheck2
                         ghc-test-framework-leancheck
                         ghc-monad-control
                         ghc-system-filepath
                         ghc-system-fileio
                         ghc-transformers-base
                         pkg-config))
    (home-page "http://darcs.net")
    (synopsis "Distributed Revision Control System")
    (description
     "Darcs is a revision control system.  It is:

@enumerate
@item Distributed: Every user has access to the full command set, removing boundaries
between server and client or committer and non-committers.
@item Interactive: Darcs is easy to learn and efficient to use because it asks you
questions in response to simple commands, giving you choices in your work flow.
You can choose to record one change in a file, while ignoring another.  As you update
from upstream, you can review each patch name, even the full diff for interesting
patches.
@item Smart: Originally developed by physicist David Roundy, darcs is based on a
unique algebra of patches called @url{http://darcs.net/Theory,Patchtheory}.
@end enumerate")
    (license license:gpl2)))

(define-public ghcid
  (package
    (name "ghcid")
    (version "0.8.7")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "ghcid" version))
       (sha256
        (base32 "0yqc1pkfajnr56gnh43sbj50r7c3r41b2jfz07ivgl6phi4frjbq"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "ghcid")))
    (inputs
     (list ghc-extra ghc-ansi-terminal ghc-cmdargs ghc-fsnotify
           ghc-terminal-size))
    (native-inputs
     (list ghc-tasty ghc-tasty-hunit))
    (home-page "https://github.com/ndmitchell/ghcid#readme")
    (synopsis "GHCi based bare bones IDE")
    (description
     "Either \"GHCi as a daemon\" or \"GHC + a bit of an IDE\".  A very simple Haskell
development tool which shows you the errors in your project and updates them whenever
you save.  Run @code{ghcid --topmost --command=ghci}, where @code{--topmost} makes the
window on top of all others (Windows only) and @code{--command} is the command to start
GHCi on your project (defaults to @code{ghci} if you have a @file{.ghci} file, or else
to @code{cabal repl}).")
    (license license:bsd-3)))

(define-public git-annex
  (package
    (name "git-annex")
    (version "10.20240227")
    (source
     (origin
       ;; hackage release doesn't include everything needed for extra bits.
       (method git-fetch)
       (uri (git-reference
              (url "https://git.joeyh.name/git/git-annex.git")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "023gk1v01ks367h4zz67ksn2xaw6pgcfhmniay6ipkncfqv8rsra"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "git-annex")))
    (arguments
     `(#:configure-flags
       '("--flags=-Android -Webapp")
       #:haddock? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'patch-shell-for-tests
           (lambda _
             ;; Shell.hs defines "/bin/sh" that is used in Git hooks.  We
             ;; shouldn't patch hooks with Guix's current bash because the
             ;; hooks can exist after that bash is garbage collected, but
             ;; let's temporarily patch it so that we can run the tests.
             (copy-file "Utility/Shell.hs" "/tmp/Shell.hs")
             (substitute* "Utility/Shell.hs"
               (("/bin/sh") (which "sh")))))
         (add-before 'configure 'patch-webapp
           (lambda _
             ;; Replace loose references to xdg-open so that 'git annex
             ;; webapp' runs without making the user also install xdg-utils.
             (substitute* '("Assistant/WebApp/DashBoard.hs"
                            "Utility/WebApp.hs")
               (("xdg-open") (which "xdg-open")))
             ;; Also replace loose references to lsof.
             (substitute* "Assistant/Threads/Watcher.hs"
               (("\"lsof\"")
                (string-append "\"" (which "lsof") "\"")))))
         (add-before 'configure 'factor-setup
           (lambda _
             ;; Factor out necessary build logic from the provided
             ;; `Setup.hs' script.  The script as-is does not work because
             ;; it cannot find its dependencies, and there is no obvious way
             ;; to tell it where to look.
             (call-with-output-file "PreConf.hs"
               (lambda (out)
                 (format out "import qualified Build.Configure as Configure~%")
                 (format out "main = Configure.run Configure.tests~%")))
             (call-with-output-file "Setup.hs"
               (lambda (out)
                 (format out "import Distribution.Simple~%")
                 (format out "main = defaultMain~%")))))
         (add-before 'configure 'pre-configure
           (lambda _
             (invoke "runhaskell" "PreConf.hs")))
         (add-after 'build 'build-manpages
           (lambda _
             (invoke "make" "mans")))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             ;; We need to set the path so that Git recognizes
             ;; `git annex' as a custom command.
             (setenv "PATH" (string-append (getenv "PATH") ":"
                                           (getcwd) "/dist/build/git-annex"))
             (when tests?
               (with-directory-excursion "dist/build/git-annex"
                 (symlink "git-annex" "git-annex-shell"))
               (invoke "git-annex" "test"))))
         (add-after 'check 'unpatch-shell-and-rebuild
           (lambda args
             ;; Undo `patch-shell-for-tests'.
             (copy-file "/tmp/Shell.hs" "Utility/Shell.hs")
             (apply (assoc-ref %standard-phases 'build) args)))
         (add-after 'install 'install-more
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bash (string-append out "/etc/bash_completions.d"))
                    (fish (string-append out "/share/fish/vendor_completions.d"))
                    (zsh (string-append out "/share/zsh/site-functions")))
             (setenv "PREFIX" out)
             (invoke "make" "install-mans")
             (mkdir-p bash)
             (copy-file "bash-completion.bash"
                        (string-append bash "/git-annex"))
             (mkdir-p fish)
             (with-output-to-file (string-append fish "/git-annex.fish")
               (lambda _
                 (invoke (string-append out "/bin/git-annex")
                         "--fish-completion-script" "git-annex")))
             (mkdir-p zsh)
             (with-output-to-file (string-append zsh "/_git-annex")
               (lambda _
                 (invoke (string-append out "/bin/git-annex")
                         "--zsh-completion-script" "git-annex"))))))
         (add-after 'install 'install-symlinks
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (symlink (string-append bin "/git-annex")
                        (string-append bin "/git-annex-shell"))
               (symlink (string-append bin "/git-annex")
                        (string-append bin "/git-remote-tor-annex"))))))))
    (inputs
     (list curl
           ghc-aeson
           ghc-ansi-terminal
           ghc-async
           ghc-attoparsec
           ghc-aws
           ghc-bloomfilter
           ghc-byteable
           ghc-case-insensitive
           ghc-clientsession
           ghc-concurrent-output
           ghc-conduit
           ghc-connection
           ghc-crypto-api
           ghc-cryptonite
           ghc-data-default
           ghc-dav
           ghc-dbus
           ghc-disk-free-space
           ghc-dlist
           ghc-edit-distance
           ghc-exceptions
           ghc-fdo-notify
           ghc-feed
           ghc-filepath-bytestring
           ghc-free
           ghc-git-lfs
           ghc-hinotify
           ghc-http-client
           ghc-http-client-tls
           ghc-http-client-restricted
           ghc-http-conduit
           ghc-http-types
           ghc-ifelse
           ghc-magic
           ghc-memory
           ghc-microlens
           ghc-monad-control
           ghc-monad-logger
           ghc-mountpoints
           ghc-network
           ghc-network-bsd
           ghc-network-info
           ghc-network-multicast
           ghc-network-uri
           ghc-old-locale
           ghc-optparse-applicative
           ghc-persistent
           ghc-persistent-sqlite
           ghc-persistent-template
           ghc-quickcheck
           ghc-random
           ghc-regex-tdfa
           ghc-resourcet
           ghc-safesemaphore
           ghc-sandi
           ghc-securemem
           ghc-socks
           ghc-split
           ghc-stm-chans
           ghc-tagsoup
           ghc-torrent
           ghc-transformers
           ghc-unix-compat
           ghc-unliftio-core
           ghc-unordered-containers
           ghc-utf8-string
           ghc-uuid
           ghc-vector
           ghc-wai
           ghc-wai-extra
           ghc-warp
           ghc-warp-tls
           ghc-yesod
           ghc-yesod-core
           ghc-yesod-form
           ghc-yesod-static
           lsof
           rsync
           xdg-utils))
    (propagated-inputs
     (list git))
    (native-inputs
     (list ghc-tasty ghc-tasty-hunit ghc-tasty-quickcheck ghc-tasty-rerun
           perl))
    (home-page "https://git-annex.branchable.com/")
    (synopsis "Manage files with Git, without checking in their contents")
    (description "This package allows managing files with Git, without
checking the file contents into Git.  It can store files in many places,
such as local hard drives and cloud storage services.  It can also be
used to keep a folder in sync between computers.")
    ;; The main author has released all his changes under AGPLv3+ as of March
    ;; 2019 (7.20190219-187-g40ecf58d4).  These are also licensed under the
    ;; original GPLv3+ license, but going forward new changes will be under
    ;; only AGPLv3+.  The other licenses below cover code written by others.
    ;; See git-annex's COPYRIGHT file for details on each file.
    (license (list license:agpl3+
                   license:gpl3+
                   license:bsd-2
                   license:expat
                   license:gpl2))))

(define-public hlint
  (package
    (name "hlint")
    (version "3.4.1")
    (source (origin
              (method url-fetch)
              (uri (hackage-uri "hlint" version))
              (sha256
               (base32
                "0bkk03c9hacvfd73dk89g4r81b50g7pjgw5pavldali4qwss34cz"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "hlint")))
    (inputs (list ghc-unordered-containers
                  ghc-vector
                  ghc-file-embed
                  ghc-utf8-string
                  ghc-data-default
                  cpphs
                  ghc-cmdargs
                  ghc-uniplate
                  ghc-ansi-terminal
                  ghc-extra
                  ghc-refact
                  ghc-aeson
                  ghc-deriving-aeson
                  ghc-filepattern
                  ghc-lib-parser-ex
                  hscolour
                  ghc-yaml))
    (arguments
     `(#:cabal-revision ("1"
                         "1rdaffg5n179yfcn5zjwjb0bki09qy13gz2ijky455y9pbaz8yz9")))
    (home-page "https://github.com/ndmitchell/hlint#readme")
    (synopsis "Suggest improvements for Haskell source code")
    (description
     "HLint reads Haskell programs and suggests changes that
hopefully make them easier to read.  HLint also makes it easy to disable
unwanted suggestions, and to add your own custom suggestions.")
    (license license:bsd-3)))

(define-public hoogle
  (package
    (name "hoogle")
    (version "5.0.18.3")
    (source (origin
              (method url-fetch)
              (uri (hackage-uri "hoogle" version))
              (sha256
               (base32
                "0v6k75w0an9pqgb7a6cicnpf9rz77xd2lmxfbafc5l4f99jg83bn"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "hoogle")))
    (inputs (list ghc-quickcheck
                  ghc-aeson
                  ghc-blaze-html
                  ghc-blaze-markup
                  ghc-cmdargs
                  ghc-conduit
                  ghc-conduit-extra
                  ghc-connection
                  ghc-extra
                  ghc-foundation
                  ghc-old-locale
                  ghc-hashable
                  ghc-haskell-src-exts
                  ghc-http-conduit
                  ghc-http-types
                  ghc-js-flot
                  ghc-js-jquery
                  ghc-mmap
                  ghc-process-extras
                  ghc-resourcet
                  ghc-storable-tuple
                  ghc-tar
                  ghc-uniplate
                  ghc-utf8-string
                  ghc-vector
                  ghc-wai
                  ghc-wai-logger
                  ghc-warp
                  ghc-warp-tls
                  ghc-zlib
                  ghc-semigroups))
    (home-page "https://hoogle.haskell.org/")
    (synopsis "Haskell API Search")
    (description
     "Hoogle is a Haskell API search engine, which allows
you to search many standard Haskell libraries by either function name,
or by approximate type signature.")
    (license license:bsd-3)))

(define-public hscolour
  (package
    (name "hscolour")
    (version "1.24.4")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "hscolour" version))
       (sha256
        (base32
         "079jwph4bwllfp03yfr26s5zc6m6kw3nhb1cggrifh99haq34cr4"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "hscolour")))
    (home-page "https://hackage.haskell.org/package/hscolour")
    (synopsis "Script to colourise Haskell code")
    (description "HSColour is a small Haskell script to colourise Haskell
code.  It currently has six output formats: ANSI terminal codes (optionally
XTerm-256colour codes), HTML 3.2 with font tags, HTML 4.01 with CSS, HTML 4.01
with CSS and mouseover annotations, XHTML 1.0 with inline CSS styling, LaTeX,
and mIRC chat codes.")
    (license license:bsd-3)))

(define-public kmonad
  ;; Project is active, but no new releases exist. Pick current master
  ;; HEAD as of 2023-01-08.
  (let ((commit "31c591b647d277fe34cb06fc70b0d053dd15f867")
        (revision "0"))
    (package
      (name "kmonad")
      (version (git-version "0.4.2" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/david-janssen/kmonad")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0k1dfyy1q86l5yivv1jrclgvp9k48qm8pzk1j9wavq92li77y7r5"))))
      (build-system haskell-build-system)
      (arguments
       `(#:haddock? #f ; Haddock fails to generate docs
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'patch-git-path
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "src/KMonad/Args/TH.hs"
                 (("\"git\"")
                  (string-append "\"" (search-input-file inputs "/bin/git") "\"")))))
           (add-after 'install 'install-udev-rules
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (rules (string-append out "/lib/udev/rules.d")))
                 (mkdir-p rules)
                 (call-with-output-file (string-append rules "/70-kmonad.rules")
                   (lambda (port)
                     (display
                      (string-append
                       "KERNEL==\"uinput\", MODE=\"0660\", "
                       "GROUP=\"input\", OPTIONS+=\"static_node=uinput\"\n")
                      port)))
                 #t)))
           (add-after 'install-udev-rules 'install-documentation
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (doc (string-append out "/share/doc/kmonad-" ,version)))
                 (install-file "README.md" doc)
                 (copy-recursively "doc" doc)
                 (copy-recursively "keymap" (string-append doc "/keymap"))
                 #t))))))
      (inputs
       (list ghc-cereal
             ghc-exceptions
             ghc-lens
             ghc-megaparsec
             ghc-optparse-applicative
             ghc-resourcet
             ghc-rio
             ghc-unliftio
             ghc-unordered-containers
             ghc-template-haskell))
      (native-inputs (list ghc-hspec hspec-discover git))
      (home-page "https://github.com/david-janssen/kmonad")
      (synopsis "Advanced keyboard manager")
      (description "KMonad is a keyboard remapping utility that supports
advanced functionality, such as custom keymap layers and modifiers, macros,
and conditional mappings that send a different keycode when tapped or held.
By operating at a lower level than most similar tools, it supports X11,
Wayland, and Linux console environments alike.")
      (license license:expat))))

(define-public matterhorn
  (package
    (name "matterhorn")
    (version "90000.0.0")
    (source
     (origin
       ;; use git repo instead of hackage URL because the hackage tarball
       ;; doesn't contain the sample config file
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/matterhorn-chat/matterhorn")
             (commit version)))
       (sha256
        (base32 "08ng5axranilvfl9j3v0mjgpg76kzacrqj4c8x6pblpc3yxx02i5"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "matterhorn")))
    (inputs (list ghc-aeson
                  ghc-aspell-pipe
                  ghc-async
                  ghc-base-compat
                  ghc-bimap
                  ghc-brick
                  ghc-brick-skylighting
                  ghc-commonmark
                  ghc-commonmark-extensions
                  ghc-config-ini
                  ghc-crypton-connection
                  ghc-data-clist
                  ghc-gitrev
                  ghc-hashable
                  ghc-hclip
                  ghc-mattermost-api
                  ghc-microlens-platform
                  ghc-network-uri
                  ghc-random
                  ghc-semigroups
                  ghc-skylighting-core
                  ghc-split
                  ghc-stm-delay
                  ghc-strict
                  ghc-temporary
                  ghc-text-zipper
                  ghc-timezone-olson
                  ghc-timezone-series
                  ghc-unix-compat-7
                  ghc-unordered-containers
                  ghc-utf8-string
                  ghc-uuid
                  ghc-vector
                  ghc-vty-6
                  ghc-vty-crossplatform
                  ghc-word-wrap
                  ghc-xdg-basedir))
    (native-inputs (list ghc-checkers
                         ghc-mattermost-api-qc
                         ghc-tasty
                         ghc-tasty-hunit
                         ghc-tasty-quickcheck
                         ghc-unique))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-requirements
            (lambda _
              (for-each (lambda (dep)
                          (substitute* "matterhorn.cabal"
                            (((string-append "(,\\s" dep
                                             "\\s*>=\\s[0-9].[0-9]).*")
                              all pat)
                             pat)))
                        (list "random"
                              "data-clist"
                              "semigroups"
                              "word-wrap"
                              "unix-compat"
                              "skylighting-core"
                              "checkers"
                              "vty"
                              "vty-crossplatform"
                              "brick"))))
          (add-after 'install 'install-config-file
            (lambda _
              (install-file "./docs/sample-config.ini"
                            (string-append #$output "/share/doc/"
                                           #$name "-" #$version "/etc/")))))))
    (home-page "https://hackage.haskell.org/package/matterhorn")
    (synopsis "Terminal client for the Mattermost chat system")
    (description
     "This is a terminal client for the Mattermost chat system.")
    (license license:bsd-3)))

(define-public nixfmt
  (package
    (name "nixfmt")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "nixfmt" version))
       (sha256
        (base32 "0rxi8zrd2xr72w673nvgnhb0g3r7rssc1ahlhz8rmdpc6c1a82wl"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "nixfmt")))
    (inputs
     (list ghc-megaparsec ghc-parser-combinators ghc-cmdargs
           ghc-safe-exceptions))
    (home-page "https://github.com/serokell/nixfmt")
    (synopsis "Opinionated formatter for Nix")
    (description
     "Nixfmt is a formatter for Nix that ensures consistent and clear
formatting by forgetting all existing formatting during parsing.")
    (license license:mpl2.0)))

(define-public greenclip
  (package
    (name "greenclip")
    (version "4.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/erebe/greenclip")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "10r485q055ci29fmpsjy55n1yqfil53cvdxldlzw2n6mpynmckyv"))))
    (build-system haskell-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list libx11
           libxext
           libxscrnsaver
           ghc-x11
           ghc-exceptions
           ghc-hashable
           ghc-microlens
           ghc-microlens-mtl
           ghc-protolude
           ghc-tomland
           ghc-vector
           ghc-wordexp))
    (home-page "https://github.com/erebe/greenclip")
    (synopsis "Simple Clipboard manager")
    (description "@code{greenclip} is a clipboard manager written in
Haskell.")
    (license license:bsd-3)))

(define-public raincat
  (package
    (name "raincat")
    (version "1.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "Raincat" version))
       (sha256
        (base32
         "10y9zi22m6hf13c9h8zd9vg7mljpwbw0r3djb6r80bna701fdf6c"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "Raincat")))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'wrap-executable
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (wrap-program (string-append out "/bin/raincat")
                 `("LD_LIBRARY_PATH" ":" =
                   (,(string-append (assoc-ref inputs "freeglut")
                                    "/lib"))))
               #t))))))
    (inputs
     (list ghc-extensible-exceptions
           ghc-random
           ghc-glut
           freeglut
           ghc-opengl
           ghc-sdl2
           ghc-sdl2-image
           ghc-sdl2-mixer))
    (home-page "https://www.gamecreation.org/games/raincat")
    (synopsis "Puzzle game with a cat in lead role")
    (description "Project Raincat is a game developed by Carnegie Mellon
students through GCS during the Fall 2008 semester.  Raincat features game
play inspired from classics Lemmings and The Incredible Machine.  The project
proved to be an excellent learning experience for the programmers.  Everything
is programmed in Haskell.")
    (license license:bsd-3)))

(define-public scroll
  (package
    (name "scroll")
    (version "1.20180421")
    (source
      (origin
        (method url-fetch)
        (uri (hackage-uri "scroll" version))
        (sha256
         (base32
          "0apzrvf99rskj4dbmn57jjxrsf19j436s8a09m950df5aws3a0wj"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "scroll")))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'touch-static-output
           (lambda* (#:key outputs #:allow-other-keys)
             ;; The Haskell build system adds a "static" output by
             ;; default, and there is no way to override this until
             ;; <https://issues.guix.gnu.org/41569> is fixed.  Without
             ;; this phase, the daemon complains because we do not
             ;; create the "static" output.
             (with-output-to-file (assoc-ref outputs "static")
               (lambda ()
                 (display "static output not used\n")))
             #t)))))
    (inputs
     (list ghc-case-insensitive
           ghc-data-default
           ghc-ifelse
           ghc-monad-loops
           ghc-ncurses
           ghc-optparse-applicative
           ghc-random
           ghc-vector))
    (home-page "https://joeyh.name/code/scroll/")
    (synopsis "scroll(6), a roguelike game")
    (description
     "You're a bookworm that's stuck on a scroll.  You have to dodge between
words and use spells to make your way down the page as the scroll is read.  Go
too slow and you'll get wound up in the scroll and crushed.")
    (license license:gpl2)))

(define-public shellcheck
  (package
    (name "shellcheck")
    (version "0.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "ShellCheck" version))
       (sha256
        (base32 "071k2gc8rzpg9lwq9g10c9xx0zm1wcgsf8v4n1csj9fm56vy7gmb"))
       (file-name (string-append name "-" version ".tar.gz"))))
    (build-system haskell-build-system)
    (arguments
     '(#:haddock? #f ; TODO: Fails to build.
       #:phases
       (modify-phases %standard-phases
         (add-after 'build 'build-man-page
           (lambda _
             (invoke "./manpage")))
         (add-after 'install 'install-man-page
           (lambda* (#:key outputs #:allow-other-keys)
             (install-file "shellcheck.1"
                           (string-append (assoc-ref outputs "out")
                                          "/share/man/man1/"))))
         (add-after 'register 'remove-libraries
           (lambda* (#:key outputs #:allow-other-keys)
             (delete-file-recursively (string-append (assoc-ref outputs "out") "/lib")))))))
    (native-inputs
     (list pandoc))
    (inputs
     (list ghc-aeson ghc-diff ghc-fgl ghc-quickcheck ghc-regex-tdfa))
    (home-page "https://www.shellcheck.net/")
    (synopsis "Static analysis for shell scripts")
    (description "@code{shellcheck} provides static analysis for
@command{bash} and @command{sh} shell scripts.
It gives warnings and suggestions in order to:

@enumerate
@item Point out and clarify typical beginner's syntax issues that cause
a shell to give cryptic error messages.
@item Point out and clarify typical intermediate level semantic problems
that cause a shell to behave strangely and counter-intuitively.
@item Point out subtle caveats, corner cases and pitfalls that may cause an
advanced user's otherwise working script to fail under future circumstances.
@end enumerate")
    ;; CVE-2021-28794 is for a completely different, unofficial add-on.
    (properties `((lint-hidden-cve . ("CVE-2021-28794")) (upstream-name . "ShellCheck")))
    (license license:gpl3+)))

(define-public shelltestrunner
  (package
    (name "shelltestrunner")
    (version "1.9")
    (source (origin
              (method url-fetch)
              (uri (hackage-uri "shelltestrunner" version))
              (sha256
               (base32
                "1a5kzqbwg6990249ypw0cx6cqj6663as1kbj8nzblcky8j6kbi6b"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "shelltestrunner")))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (delete 'check)
         (add-after 'install 'check
           (lambda* (#:key outputs tests? parallel-tests? #:allow-other-keys)
             ;; This test is inspired by the Makefile in the upstream
             ;; repository, which is missing in the Hackage release tarball
             ;; along with some of the tests.  The Makefile would not work
             ;; anyway as it ties into the 'stack' build tool.
             (let* ((out (assoc-ref outputs "out"))
                    (shelltest (string-append out "/bin/shelltest"))
                    (numjobs (if parallel-tests?
                                 (number->string (parallel-job-count))
                                 "1")))
               (if tests?
                   (invoke shelltest (string-append "-j" numjobs)
                           "tests/examples")
                   (format #t "test suite not run~%"))
               #t))))))
    (inputs
     (list ghc-diff
           ghc-cmdargs
           ghc-filemanip
           ghc-hunit
           ghc-pretty-show
           ghc-regex-tdfa
           ghc-safe
           ghc-utf8-string
           ghc-test-framework
           ghc-test-framework-hunit))
    (home-page "https://github.com/simonmichael/shelltestrunner")
    (synopsis "Test CLI programs")
    (description
     "shelltestrunner (executable: @command{shelltest}) is a command-line tool
for testing command-line programs, or general shell commands.  It reads simple
test specifications defining a command to run, some input, and the expected
output, stderr, and exit status.")
    (license license:gpl3+)))

(define-public stylish-haskell
  (package
    (name "stylish-haskell")
    (version "0.14.3.0")
    (source (origin
              (method url-fetch)
              (uri (hackage-uri "stylish-haskell" version))
              (sha256
               (base32
                "17w92v0qnwj7m6yqdq5cxbr04xiz0yfnnyx5q54218wdl7n5lf6d"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "stylish-haskell")))
    (inputs (list ghc-aeson
                  ghc-file-embed
                  ghc-regex-tdfa
                  ghc-syb
                  ghc-hsyaml-aeson
                  ghc-hsyaml
                  ghc-semigroups
                  ghc-lib-parser-ex
                  ghc-strict
                  ghc-optparse-applicative))
    (native-inputs (list ghc-hunit ghc-random ghc-test-framework
                         ghc-test-framework-hunit))
    (home-page "https://github.com/haskell/stylish-haskell")
    (synopsis "Haskell code prettifier")
    (description
     "Stylish-haskell is a Haskell code prettifier.  The goal is
not to format all of the code in a file, to avoid \"getting in the way\".
However, this tool can e.g. clean up import statements and help doing various
tasks that get tedious very quickly.  It can
@itemize
@item
Align and sort @code{import} statements
@item
Group and wrap @code{{-# LANGUAGE #-}} pragmas, remove (some) redundant
pragmas
@item
Remove trailing whitespaces
@item
Align branches in @code{case} and fields in records
@item
Convert line endings (customisable)
@item
Replace tabs by four spaces (turned off by default)
@item
Replace some ASCII sequences by their Unicode equivalent (turned off by
default)
@end itemize")
    (license license:bsd-3)))
