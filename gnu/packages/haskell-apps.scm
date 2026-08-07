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
;;; Copyright © 2019, 2020, 2022-2024 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2019, 2020 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2020 Alexandru-Sergiu Marton <brown121407@member.fsf.org>
;;; Copyright © 2020 Brian Leung <bkleung89@gmail.com>
;;; Copyright © 2021 EuAndreh <eu@euandre.org>
;;; Copyright © 2021 Stefan Reichör <stefan@xsteve.at>
;;; Copyright © 2021 Morgan Smith <Morgan.J.Smith@outlook.com>
;;; Copyright © 2022 David Thompson <dthompson2@worcester.edu>
;;; Copyright © 2024 jgart <jgart@dismail.de>
;;; Copyright © 2026 Konstantin Suntsov <protvin@disroot.org>
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
  #:use-module (gnu packages bash)
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
    (version "0.15.0.0")
    (source (origin
              (method url-fetch)
              (uri (hackage-uri "apply-refact" version))
              (sha256
               (base32
                "0iny8ynxx46afbzjp14rvpd4sqk8c1p5883mbln53gmlxcm8zq53"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "apply-refact")))
    (inputs (list ghc-refact
                  ghc-data-default
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
   (version "3.12.1.0")
   (source
    (origin
      (method url-fetch)
      (uri (hackage-uri "cabal-install" version))
      (sha256
       (base32 "1cmifq189i4x0r0yha3dl8nrzzfh92bnd2saak7dqvvjkkysqj38"))))
   (build-system haskell-build-system)
   (properties '((upstream-name . "cabal-install")))
   (inputs (list ghc-async
                 ghc-base16-bytestring
                 ghc-cabal-install-solver
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
                 ghc-open-browser
                 ghc-regex-base
                 ghc-regex-posix
                 ghc-safe-exceptions
                 ghc-semaphore-compat
                 ghc-resolv
                 ghc-lukko))
   (native-inputs (list ghc-tasty
                        ghc-tasty-golden
                        ghc-tasty-quickcheck
                        ghc-tasty-expected-failure
                        ghc-tasty-hunit
                        ghc-tree-diff
                        ghc-quickcheck
                        ghc-tasty
                        ghc-tasty-hunit
                        ghc-tasty
                        ghc-tasty-hunit
                        ghc-tagged
                        ghc-tagged
                        ghc-tasty
                        ghc-tasty-expected-failure
                        ghc-tasty-hunit
                        ghc-tasty-quickcheck
                        ghc-quickcheck
                        ghc-pretty-show))
   (arguments
    `(#:cabal-revision ("2"
                        "0fdzqdkg2vbyg0lnbk9bdskr2d23hwjpmnc7jnvpzkcmxmcvl99n")
      ;; The tests depend on Cabal-{QuickCheck,described,tests,tree-diff},
      ;; which are not distributed on hackage.
      #:tests? #f))
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
    (inputs (list ghc-polyparse))
    (arguments
     `(#:cabal-revision ("2"
                         "0vxav36p0kplp4dpd17i4cfzrsl3r437d840xwv83lf1bqp7mrxc")))
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
    (version "0.8.9")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "ghcid" version))
       (sha256
        (base32 "1dq8lc0dwzib8y21279q4j54cmm7lvx64b3hw2yiym1kzi9rrhj4"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "ghcid")))
    (inputs (list ghc-extra ghc-ansi-terminal ghc-cmdargs ghc-fsnotify
                  ghc-terminal-size))
    (native-inputs (list ghc-tasty ghc-tasty-hunit))
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
    (version "10.20250721")
    (source
     (origin
       ;; hackage release doesn't include everything needed for extra bits.
       (method git-fetch)
       (uri (git-reference
              (url "https://git.joeyh.name/git/git-annex.git")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1a2g4z9fl90kyl7k6clmc61ma2njnljr4na6jbf04m43qnnr9s5a"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "git-annex")))
    (arguments
     `(#:configure-flags
       '("--flags=-Android -Webapp Servant")
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
                 (symlink "git-annex" "git-annex-shell")
                 (symlink "git-annex" "git-remote-annex")
                 (symlink "git-annex" "git-remote-tor-annex"))
               (invoke "git-annex" "test"))))
         (add-after 'check 'unpatch-shell-and-rebuild
           (lambda args
             ;; Undo `patch-shell-for-tests'.
             (copy-file "/tmp/Shell.hs" "Utility/Shell.hs")
             (apply (assoc-ref %standard-phases 'build) args)))
         (add-after 'install 'install-more
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bash (string-append out "/etc/bash_completion.d"))
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
                        (string-append bin "/git-remote-annex"))
               (symlink (string-append bin "/git-annex")
                        (string-append bin "/git-remote-tor-annex"))))))))
    (inputs (list curl
                  ghc-network-uri
                  ghc-optparse-applicative
                  ghc-uuid
                  ghc-data-default
                  ghc-case-insensitive
                  ghc-random
                  ghc-dlist
                  ghc-unix-compat
                  ghc-safesemaphore
                  ghc-async
                  ghc-disk-free-space
                  ghc-ifelse
                  ghc-monad-logger
                  ghc-free
                  ghc-utf8-string
                  ghc-sandi
                  ghc-monad-control
                  ghc-bloomfilter
                  ghc-edit-distance
                  ghc-resourcet
                  ghc-http-client
                  ghc-http-client-tls
                  ghc-http-types
                  ghc-http-conduit
                  ghc-http-client-restricted
                  ghc-conduit
                  ghc-old-locale
                  ghc-persistent-sqlite
                  ghc-persistent
                  ghc-persistent-template
                  ghc-unliftio-core
                  ghc-microlens
                  ghc-aeson
                  ghc-vector
                  ghc-tagsoup
                  ghc-unordered-containers
                  ghc-feed
                  ghc-regex-tdfa
                  ghc-socks
                  ghc-byteable
                  ghc-stm-chans
                  ghc-securemem
                  ghc-crypto-api
                  ghc-memory
                  ghc-split
                  ghc-attoparsec
                  ghc-concurrent-output
                  ghc-unbounded-delays
                  ghc-quickcheck
                  ghc-tasty
                  ghc-tasty-hunit
                  ghc-tasty-quickcheck
                  ghc-tasty-rerun
                  ghc-ansi-terminal
                  ghc-aws
                  ghc-dav
                  ghc-network
                  ghc-network-bsd
                  ghc-git-lfs
                  ghc-clock
                  ghc-crypton
                  ghc-servant
                  ghc-servant-server
                  ghc-servant-client
                  ghc-servant-client-core
                  ghc-warp
                  ghc-warp-tls
                  ghc-os-string
                  ghc-file-io
                  ghc-mountpoints
                  ghc-yesod
                  ghc-yesod-static
                  ghc-yesod-form
                  ghc-yesod-core
                  ghc-path-pieces
                  ghc-wai
                  ghc-wai-extra
                  ghc-blaze-builder
                  ghc-clientsession
                  ghc-shakespeare
                  ghc-hinotify
                  ghc-dbus
                  ghc-fdo-notify
                  ghc-network-multicast
                  ghc-network-info
                  ghc-torrent
                  ghc-magic
                  ghc-criterion
                  lsof
                  rsync
                  xdg-utils))
    (propagated-inputs
     (list git))
    (native-inputs (list perl ghc-filepath-bytestring))
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
    (version "3.10")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "hlint" version))
       (sha256
        (base32 "0dzy7spc45v88yplczhd3la464bhcbaihi619a45bd06ghrp55nr"))))
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
                  ghc-lib-parser
                  ghc-lib-parser-ex
                  hscolour
                  ghc-yaml))
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
    (version "5.0.18.4")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "hoogle" version))
       (sha256
        (base32 "08z32d87vqzhapb2vw21h25jb2g74csxlpvd8f54xl91k3ijs3wx"))
       (patches (search-patches "hoogle-crypton-connection-0.4.X.patch"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "hoogle")))
    (inputs (list ghc-quickcheck
                  ghc-aeson
                  ghc-blaze-html
                  ghc-blaze-markup
                  ghc-cmdargs
                  ghc-conduit
                  ghc-conduit-extra
                  ghc-crypton-connection
                  ghc-data-default-class
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
                  ghc-safe
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
    (version "1.25")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "hscolour" version))
       (sha256
        (base32 "0z679khnmb6as1zcdb44n9qjk7in32jpm4ldscpqg7jrapd31kjl"))))
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

(define-public lhs2tex
  (package
    (name "lhs2tex")
    (version "1.25")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "lhs2tex" version))
       (sha256
        (base32 "0cf66z6mgadgqd1xs5b6gw8l9rkwgbfsc5czwdiapn7ichi26qyj"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "lhs2tex")))
    (inputs (list ghc-regex-compat))
    (home-page "https://github.com/kosmikus/lhs2tex")
    (synopsis "Preprocessor for typesetting Haskell sources with LaTeX")
    (description
     "This tool is primarily intended for people who want to write articles or
books using LaTeX that contain some Haskell code.  It works on literate Haskell
documents where the non-Haskell parts form essentially a valid LaTeX document,
then processes the Haskell code in the document and replaces it with formatted
LaTeX output.")
    (license license:gpl2+)))

(define-public kmonad
  (package
    (name "kmonad")
    (version "0.4.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/kmonad/kmonad")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ng07i2zb98gx7giz7cjxjx908p1v14wn913k810n550k2gfbvp9"))))
    (build-system haskell-build-system)
    (arguments
     (list
      #:haddock? #f ;Haddock fails to generate docs
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-git-path
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "src/KMonad/Args/TH.hs"
                (("\"git\"")
                 (string-append "\""
                                (search-input-file inputs "/bin/git") "\"")))))
          (add-after 'install 'install-udev-rules
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((rules (string-append #$output "/lib/udev/rules.d")))
                (mkdir-p rules)
                (call-with-output-file (string-append rules "/70-kmonad.rules")
                  (lambda (port)
                    (display (string-append
                              "KERNEL==\"uinput\", MODE=\"0660\", "
                              "GROUP=\"input\", OPTIONS+=\"static_node=uinput\"")
                             port))) #t)))
          (add-after 'install-udev-rules 'install-documentation
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((doc (string-append #$output "/share/doc/"
                                         #$name "-"
                                         #$version)))
                (copy-recursively "doc" doc)
                (copy-recursively "keymap"
                                  (string-append doc "/keymap")) #t))))))
    (native-inputs (list ghc-hspec
                         hspec-discover
                         git))
    (inputs (list ghc-cereal
                  ghc-exceptions
                  ghc-hinotify
                  ghc-lens
                  ghc-megaparsec
                  ghc-optparse-applicative
                  ghc-resourcet
                  ghc-rio
                  ghc-unliftio
                  ghc-unordered-containers
                  ghc-template-haskell))
    (home-page "https://github.com/kmonad/kmonad")
    (synopsis "Advanced keyboard manager")
    (description
     "KMonad is a keyboard remapping utility that supports
advanced functionality, such as custom keymap layers and modifiers, macros,
and conditional mappings that send a different keycode when tapped or held.
By operating at a lower level than most similar tools, it supports X11,
Wayland, and Linux console environments alike.")
    (license license:expat)))

(define-public matterhorn
  (package
    (name "matterhorn")
    (version "90000.1.2")
    (source
     (origin
       ;; use git repo instead of hackage URL because the hackage tarball
       ;; doesn't contain the sample config file
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/matterhorn-chat/matterhorn")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0i3q7896ss4dnmqs83klia4i9psv0w9ahy3k5yrm20m6qwmair5b"))
       (snippet
        #~(substitute* "matterhorn.cabal"
            (("base-compat          >= 0\\.9     && < 0\\.13")
             "base-compat          >= 0.9     && < 0.15")
            (("bytestring           >= 0\\.10    && < 0\\.12")
             "bytestring           >= 0.10    && < 0.13")
            (("containers           >= 0\\.5\\.7   && < 0\\.7")
             "containers           >= 0.5.7   && < 0.8")
            (("crypton-connection   >= 0\\.3\\.1   && < 0\\.4")
             "crypton-connection   >= 0.3.1   && < 0.5")
            (("filepath             >= 1\\.4     && < 1\\.5")
             "filepath             >= 1.4     && < 1.6")
            (("hashable             >= 1\\.2     && < 1\\.5")
             "hashable             >= 1.2     && < 1.6")
            (("tasty                >= 0\\.11    && < 1\\.5")
             "tasty                >= 0.11    && < 1.6")
            (("text                 >= 1\\.2\\.5\\.0 && < 2\\.1")
             "text                 >= 1.2.5.0 && < 2.2")))
       (modules '((guix build utils)))))
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
                  ghc-tasty
                  ghc-temporary
                  ghc-text-zipper
                  ghc-timezone-olson
                  ghc-timezone-series
                  ghc-unix-compat
                  ghc-unordered-containers
                  ghc-utf8-string
                  ghc-uuid
                  ghc-vector
                  ghc-vty
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
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
          (url "https://github.com/NixOS/nixfmt")
          (commit (string-append "v" version))))
       (sha256
        (base32 "19sydkdw1579qmvzx0zq06s23bm6m6l9wp1kvsfhxawk8pkz2pc2"))
       (snippet
        #~(substitute* "nixfmt.cabal"
            (("(megaparsec|filepath)  *[0-9<>=^&|. ]*" _ package)
             package)))
       (modules '((guix build utils)))))
    (build-system haskell-build-system)
    (inputs (list ghc-megaparsec
                  ghc-parser-combinators
                  ghc-scientific
                  ghc-pretty-simple
                  ghc-cmdargs
                  ghc-file-embed
                  ghc-safe-exceptions))
    (home-page "https://github.com/NixOS/nixfmt")
    (synopsis "Official formatter for Nix code")
    (description
     "Nixfmt is a formatter for Nix that ensures consistent and clear
formatting by forgetting all existing formatting during parsing.")
    (license license:mpl2.0)))

(define-public ormolu
  (package
    (name "ormolu")
    (version "0.8.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "ormolu" version))
       (sha256
        (base32 "1fh2m4sy8vzxvm1qm9413apzblqf4sla3454mx64ngkwx0pgi2ad"))
       (patches (search-patches "ormolu-support-cabal-syntax-3.12.patch"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "ormolu")))
    (inputs (list ghc-cabal-syntax
                  ghc-diff
                  ghc-lib-parser
                  ghc-memotrie
                  ghc-ansi-terminal
                  ghc-choice
                  ghc-file-embed
                  ghc-lib-parser
                  ghc-megaparsec
                  ghc-syb
                  ghc-optparse-applicative
                  ghc-th-env
                  ghc-unliftio))
    (native-inputs (list ghc-quickcheck
                         ghc-hspec
                         ghc-hspec-megaparsec
                         ghc-path
                         ghc-path-io
                         ghc-temporary
                         hspec-discover))
    (home-page "https://github.com/tweag/ormolu")
    (synopsis "Formatter for Haskell source code")
    (description "This package provides a formatter for Haskell source code.")
    (license license:bsd-3)))

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
     (list bash-minimal
           ghc-extensible-exceptions
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
    (version "1.20250228.2")
    (source
      (origin
        (method url-fetch)
        (uri (hackage-uri "scroll" version))
        (sha256
         (base32
          "1p1741zqsxg017d08ym1clzqcdlai487wb6q12m1q7dr6i8c0gfj"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "scroll")
                  (updater-extra-inputs "ghc-base-compat")))
    (inputs
     (list ghc-base-compat
           ghc-case-insensitive
           ghc-data-default
           ghc-ifelse
           ghc-monad-loops
           ghc-ncurses
           ghc-optparse-applicative
           ghc-random
           ghc-vector))
    (home-page "https://joeyh.name/code/scroll/")
    (synopsis "Roguelike game")
    (description
     "You're a bookworm that's stuck on a scroll.  You have to dodge between
words and use spells to make your way down the page as the scroll is read.  Go
too slow and you'll get wound up in the scroll and crushed.")
    (license license:gpl2)))

(define-public shellcheck
  (package
    (name "shellcheck")
    (version "0.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "ShellCheck" version))
       (sha256
        (base32 "08bdjcdl457xz2vh8y2w29bcwh1k7sfzyvszln3498vm5m1xn22d"))
       (file-name (string-append name "-" version ".tar.gz"))))
    (build-system haskell-build-system)
    (arguments
     (list #:haddock? #f ; TODO: Fails to build.
           #:cabal-revision '("1" "1935jrzy1r3g9cc74b330fmxnz2i1j8hsdk9jnl557qgk6xjqzs7")
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'build 'build-man-page
                 (lambda _
                   (chmod "manpage" #o555)
                   (invoke "./manpage")))
               (add-after 'install 'install-man-page
                 (lambda* (#:key outputs #:allow-other-keys)
                   (install-file "shellcheck.1"
                                 (string-append #$output
                                                "/share/man/man1/"))))
               (add-after 'register 'remove-libraries
                 (lambda* (#:key outputs #:allow-other-keys)
                   (delete-file-recursively (string-append #$output "/lib")))))))
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
    (version "1.10")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "shelltestrunner" version))
       (sha256
        (base32 "1c6bjyxqa4mgnh3w4pqp6sbr5cf160n7jf9i1b4b9sdxzdjk7g87"))))
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
    (inputs (list ghc-diff
                  ghc-filemanip
                  ghc-hunit
                  ghc-cmdargs
                  ghc-pretty-show
                  ghc-regex-tdfa
                  ghc-safe
                  ghc-test-framework
                  ghc-test-framework-hunit
                  ghc-utf8-string
                  ghc-hspec
                  ghc-hspec-core
                  ghc-hspec-contrib))
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
    (version "0.15.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "stylish-haskell" version))
       (sha256
        (base32 "06y6f7bv5j5k7q81194v9jqcbmmqcv7h8ii3lq1783bpfnyd6h19"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "stylish-haskell")))
    (inputs (list ghc-aeson
                  ghc-file-embed
                  ghc-regex-tdfa
                  ghc-syb
                  ghc-hsyaml-aeson
                  ghc-hsyaml
                  ghc-semigroups
                  ghc-lib-parser
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
