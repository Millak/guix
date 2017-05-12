;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Cyril Roelandt <tipecaml@gmail.com>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016, 2017 ng0 <ng0@no-reply.pragmatique.xyz>
;;; Copyright © 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017 Marius Bakke <mbakke@fastmail.com>
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

(define-module (gnu packages vim)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages acl)
  #:use-module (gnu packages admin) ; For GNU hostname
  #:use-module (gnu packages attr)
  #:use-module (gnu packages base)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages groff)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages jemalloc)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg))

(define-public vim
  (package
    (name "vim")
    (version "8.0.0566")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://github.com/vim/vim/archive/v"
                                 version ".tar.gz"))
             (file-name (string-append name "-" version ".tar.gz"))
             (sha256
              (base32
               "0qq9pj8391sikzaahlqi289l5wdkbvsdhz8qb6np268yqizpg4p2"))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"
       #:parallel-tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'make-bit-reproducable
           (lambda _
             (substitute* "src/version.c"
               ((" VIM_VERSION_LONG_DATE") " VIM_VERSION_LONG")
               ((" __DATE__") "")
               ((" __TIME__") ""))
             #t))
         (add-after 'configure 'patch-config-files
           (lambda _
             (substitute* "runtime/tools/mve.awk"
               (("/usr/bin/nawk") (which "gawk")))
             (substitute* '("src/testdir/Makefile"
                            "src/testdir/test_normal.vim")
               (("/bin/sh") (which "sh")))
             #t)))))
    (inputs
     `(("gawk" ,gawk)
       ("inetutils" ,inetutils)
       ("ncurses" ,ncurses)
       ("perl" ,perl)
       ("tcsh" ,tcsh))) ; For runtime/tools/vim32
    (home-page "http://www.vim.org/")
    (synopsis "Text editor based on vi")
    (description
     "Vim is a highly configurable text editor built to enable efficient text
editing.  It is an improved version of the vi editor distributed with most UNIX
systems.

Vim is often called a \"programmer's editor,\" and so useful for programming
that many consider it an entire IDE.  It's not just for programmers, though.
Vim is perfect for all kinds of text editing, from composing email to editing
configuration files.")
    (license license:vim)))

(define-public vim-full
  (package
    (inherit vim)
    (name "vim-full")
    (arguments
     `(#:configure-flags
       (list (string-append "--with-lua-prefix="
                            (assoc-ref %build-inputs "lua"))
             "--with-features=huge"
             "--enable-python3interp=yes"
             "--enable-perlinterp=yes"
             "--enable-rubyinterp=yes"
             "--enable-tclinterp=yes"
             "--enable-luainterp=yes"
             "--enable-cscope"
             "--enable-sniff"
             "--enable-multibyte"
             "--enable-xim"
             "--disable-selinux"
             "--enable-gui")
       ,@(substitute-keyword-arguments (package-arguments vim)
           ((#:phases phases)
            `(modify-phases ,phases
               (add-after 'build 'drop-failing-tests
                 (lambda _
                   ;; These tests fail mysteriously with GUI enabled.
                   ;; https://github.com/vim/vim/issues/1460
                   (substitute* "src/testdir/test_cmdline.vim"
                     (("call assert_equal\\(.+getcmd.+\\(\\)\\)") ""))
                   #t))
               (add-before 'check 'start-xserver
                 (lambda* (#:key inputs #:allow-other-keys)
                   ;; Some tests require an X server, but does not start one.
                   (let ((xorg-server (assoc-ref inputs "xorg-server"))
                         (display ":1"))
                     (setenv "DISPLAY" display)
                     (zero? (system (string-append xorg-server "/bin/Xvfb "
                                                    display " &")))))))))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("xorg-server" ,xorg-server)))
    (inputs
     `(("acl" ,acl)
       ("atk" ,atk)
       ("attr" ,attr)
       ("cairo" ,cairo)
       ("fontconfig" ,fontconfig)
       ("freetype" ,freetype)
       ("gdk-pixbuf" ,gdk-pixbuf)
       ("gettext" ,gettext-minimal)
       ("glib" ,glib)
       ("gpm" ,gpm)
       ("gtk" ,gtk+-2)
       ("harfbuzz" ,harfbuzz)
       ("libice" ,libice)
       ("libpng" ,libpng)
       ("libsm" ,libsm)
       ("libx11" ,libx11)
       ("libxdmcp" ,libxdmcp)
       ("libxt" ,libxt)
       ("libxpm" ,libxpm)
       ("lua" ,lua)
       ("pango" ,pango)
       ("pixman" ,pixman)
       ("python-3" ,python)
       ("ruby" ,ruby)
       ("tcl" ,tcl)
       ,@(package-inputs vim)))))

(define-public vim-neocomplete
  (package
    (name "vim-neocomplete")
    (version "2.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/Shougo/neocomplete.vim/"
                           "archive/ver." version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1307gbrdwam2akq9w2lpijc41740i4layk2qkd9sjkqxfch5lni2"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (vimfiles (string-append out "/share/vim/vimfiles"))
                    (autoload (string-append vimfiles "/autoload"))
                    (doc (string-append vimfiles "/doc"))
                    (plugin (string-append vimfiles "/plugin")))
               (copy-recursively "autoload" autoload)
               (copy-recursively "doc" doc)
               (copy-recursively "plugin" plugin)
               #t))))))
    (synopsis "Next generation completion framework for Vim")
    (description
     "@code{neocomplete}, an abbreviation of 'neo-completion with cache',
is a plugin for Vim.
It provides keyword completion system by maintaining a cache of keywords in
the current buffer.  Neocomplete can be customized easily and has many more
features than Vim's built-in completion.")
    (home-page "https://github.com/Shougo/neocomplete.vim/")
    (license license:expat)))

;; There are no release tarballs.
(define-public vim-neosnippet-snippets
  (let ((commit "8e2b1c0cab9ed9a832b3743dbb65e9966a64331a")
        (revision "1"))
    (package
      (name "vim-neosnippet-snippets")
      (version (string-append "0.0.0-" revision "." (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Shougo/neosnippet-snippets")
               (commit commit)))
         (file-name (string-append name "-" version "-checkout"))
         (sha256
          (base32
           "151wpvbj6jb9jdkbhj3b77f5sq7y328spvwfbqyj1y32rg4ifmc6"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (delete 'build)
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (vimfiles (string-append out "/share/vim/vimfiles")))
                 (copy-recursively "neosnippets"
                                   (string-append vimfiles "/neosnippets"))
               #t))))))
    (synopsis "Snippets for neosnippet")
    (description
     "@code{neosnippet-snippets} provides standard snippets for the Vim plugin
@code{neosnippet}.  Snippets are small templates for commonly used code that
you can fill in on the fly.")
    (home-page "https://github.com/Shougo/neosnippet-snippets")
    (license license:expat))))

;; The released tarball is too old for our Vim.
(define-public vim-neosnippet
  (let ((commit "1bd7e23c79b73da16eb0c9469b25c376d3594583")
        (revision "1"))
  (package
    (name "vim-neosnippet")
    (version (string-append "4.2-" revision "." (string-take commit 7)))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Shougo/neosnippet.vim/")
             (commit commit)))
       (file-name (string-append name "-" version "-checkout"))
       (sha256
        (base32
         "0k80syscmpnj38ks1fq02ds59g0r4jlg9ll7z4qc048mgi35alw5"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (vimfiles (string-append out "/share/vim/vimfiles"))
                    (autoload (string-append vimfiles "/autoload"))
                    (doc (string-append vimfiles "/doc"))
                    (ftdetect (string-append vimfiles "/ftdetect"))
                    (ftplugin (string-append vimfiles "/ftplugin"))
                    (indent (string-append vimfiles "/indent"))
                    (plugin (string-append vimfiles "/plugin"))
                    (rplugin (string-append vimfiles "/rplugin"))
                    (syntax (string-append vimfiles "/syntax")))
               (copy-recursively "autoload" autoload)
               (copy-recursively "doc" doc)
               (copy-recursively "ftdetect" ftdetect)
               (copy-recursively "ftplugin" ftplugin)
               (copy-recursively "indent" indent)
               (copy-recursively "plugin" plugin)
               (copy-recursively "rplugin" rplugin)
               (copy-recursively "syntax" syntax)
               #t))))))
    (synopsis "Snippet support for Vim")
    (description
     "@code{neosnippet}, is a plugin for Vim which adds snippet support to Vim.
Snippets are small templates for commonly used code that you can fill in on
the fly.  To use snippets can increase your productivity in Vim a lot.
The functionality of this plug-in is quite similar to plug-ins like
@code{snipMate.vim} or @code{snippetsEmu.vim}.  But since you can choose
snippets with the neocomplcache / neocomplete interface, you might have less
trouble using them, because you do not have to remember each snippet name.")
    (home-page "https://github.com/Shougo/neosnippet.vim/")
    (license license:expat))))

(define-public vim-scheme
  (let ((commit "93827987c10f2d5dc519166a761f219204926d5f")
        (revision "1"))
    (package
      (name "vim-scheme")
      (version (string-append "0.0.0-" revision "." (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "http://git.foldling.org/vim-scheme.git")
               (commit commit)))
         (file-name (string-append name "-" version "-checkout"))
         (sha256
          (base32
           "1ynjr1109dxgj0lz261gmzz3wf5ap1m6j6hnvl3lcyv66a4y8pjv"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (delete 'build)
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (vimfiles (string-append out "/share/vim/vimfiles"))
                      (after (string-append vimfiles "/after"))
                      (syntax (string-append vimfiles "/syntax"))
                      (ftplugin (string-append vimfiles "/ftplugin")))
                 (copy-recursively "after" after)
                 (copy-recursively "ftplugin" ftplugin)
                 (copy-recursively "syntax" syntax)
                 #t))))))
      (synopsis "Scheme syntax for Vim")
      (description
       "@code{vim-scheme} provides Scheme support for Vim (R7RS and CHICKEN).")
      (home-page "http://foldling.org/git/vim-scheme.git/")
      (license license:public-domain))))

(define-public vim-luna
  (let ((commit "633619953dcf8577168e255230f96b05f28d6371")
        (revision "1"))
    (package
      (name "vim-luna")
      (version (string-append "0.0.0-" revision "." (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/notpratheek/vim-luna")
               (commit commit)))
         (file-name (string-append name "-" version "-checkout"))
         (sha256
          (base32
           "0ka3qbhsh8lix1vyj4678j7dnchkd8khhirrnn3aylxxf8fpqyg8"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (delete 'build)
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (vimfiles (string-append out "/share/vim/vimfiles"))
                      (colors (string-append vimfiles "/colors")))
                 (copy-recursively "colors" colors)
                 #t))))))
      (synopsis "Dark color theme for Vim")
      (description
       "@code{vim-luna} is a dark color theme for Vim.")
      (home-page "https://github.com/notpratheek/vim-luna")
      (license license:expat))))

;; There are no tarball releases.
(define-public vim-context-filetype
  (let ((commit "5e85f8cae26806f391aefe2661791a6de53bcea2")
        (revision "1"))
    (package
      (name "vim-context-filetype")
      (version (string-append "0.0.0-" revision "." (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Shougo/context_filetype.vim")
               (commit commit)))
         (file-name (string-append name "-" version "-checkout"))
         (sha256
          (base32
           "0alvrfhmd91zkd9h83s8wvgyq4iakcf6rybsyjd369qbgpcqky89"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (delete 'build)
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (vimfiles (string-append out "/share/vim/vimfiles"))
                      (doc (string-append vimfiles "/doc"))
                      (autoload (string-append vimfiles "/autoload")))
                 (copy-recursively "doc" doc)
                 (copy-recursively "autoload" autoload)
                 #t))))))
      (synopsis "Context filetype library for Vim")
      (description
       "@code{vim-context-filetype} is context filetype library for Vim script.")
      (home-page "https://github.com/Shougo/context_filetype.vim")
      (license license:expat)))) ; ??? check again

(define-public vim-airline
  (package
    (name "vim-airline")
    (version "0.8")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/vim-airline/vim-airline/"
                           "archive/v" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "053sfq3jmgdc5y7zbg6jrk7r2hp0raj3y3mxa2h1c1bnkb6wvcaz"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (vimfiles (string-append out "/share/vim/vimfiles"))
                    (autoload (string-append vimfiles "/autoload"))
                    (doc (string-append vimfiles "/doc"))
                    (t (string-append vimfiles "/t"))
                    (plugin (string-append vimfiles "/plugin")))
               (copy-recursively "autoload" autoload)
               (copy-recursively "doc" doc)
               (copy-recursively "plugin" plugin)
               (copy-recursively "t" t)
               #t))))))
    (synopsis "Statusline for Vim")
    (description
     "@code{vim-airline} is an extensible statusline for Vim.
It can be extended and costumized with themes, works with unicode fonts
and powerline symbols, etc.")
    (home-page "https://github.com/vim-airline/vim-airline")
    (license license:expat)))

;; There are no tarball releases.
(define-public vim-airline-themes
  (let ((commit "6026eb78bf362cb3aa875aff8487f65728d0f7d8")
        (revision "1"))
    (package
      (name "vim-airline-themes")
      (version (string-append "0.0.0-" revision "." (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/vim-airline/vim-airline-themes")
               (commit commit)))
         (file-name (string-append name "-" version "-checkout"))
         (sha256
          (base32
           "13ijkavh1r0935cn2rjsfbdd1q3ka8bi26kw0bdkrqlrqxwvpss8"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (delete 'build)
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (vimfiles (string-append out "/share/vim/vimfiles"))
                      (doc (string-append vimfiles "/doc"))
                      (plugin (string-append vimfiles "/plugin"))
                      (autoload (string-append vimfiles "/autoload")))
                 (copy-recursively "doc" doc)
                 (copy-recursively "autoload" autoload)
                 (copy-recursively "plugin" plugin)
                 #t))))))
      (synopsis "Collection of themes for Vim-airline")
      (description
       "@code{vim-airline-themes} is a collection of themes for @code{vim-airline}.")
      (home-page "https://github.com/vim-airline/vim-airline-themes")
      (license license:expat))))

(define-public vim-syntastic
  (package
    (name "vim-syntastic")
    (version "3.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/vim-syntastic/syntastic/archive/"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0wsnd9bsp5x6yiw96h1bnd1vyxdkh130hb82kyyxydgsplx92ima"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (vimfiles (string-append out "/share/vim/vimfiles"))
                    (doc (string-append vimfiles "/doc"))
                    (plugin (string-append vimfiles "/plugin"))
                    (autoload (string-append vimfiles "/autoload"))
                    (syntax-checkers (string-append vimfiles "/syntax_checkers")))
               (copy-recursively "doc" doc)
               (copy-recursively "autoload" autoload)
               (copy-recursively "plugin" plugin)
               (copy-recursively "syntax_checkers" syntax-checkers)
               #t))))))
    (synopsis "Syntax checking plugin for Vim")
    (description
     "Vim-syntastic is a syntax checking plugin for Vim.  It runs files through
external syntax checkers and displays any resulting errors to the user.  This
can be done on demand, or automatically as files are saved.  If syntax errors
are detected, the user is notified.")
    (home-page "https://github.com/vim-syntastic/syntastic")
    (license license:wtfpl2)))

(define-public neovim-syntastic
  (package
    (inherit vim-syntastic)
    (name "neovim-syntastic")
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (vimfiles (string-append out "/share/nvim/site"))
                    (doc (string-append vimfiles "/doc"))
                    (plugin (string-append vimfiles "/plugin"))
                    (autoload (string-append vimfiles "/autoload"))
                    (syntax-checkers (string-append vimfiles "/syntax_checkers")))
               (copy-recursively "doc" doc)
               (copy-recursively "autoload" autoload)
               (copy-recursively "plugin" plugin)
               (copy-recursively "syntax_checkers" syntax-checkers)
               #t))))))
    (synopsis "Syntax checking plugin for Neovim")
    (description
     "Vim-syntastic is a syntax checking plugin for Neovim.  It runs files through
external syntax checkers and displays any resulting errors to the user.  This
can be done on demand, or automatically as files are saved.  If syntax errors
are detected, the user is notified.")))

(define-public neovim
  (package
    (name "neovim")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/neovim/neovim/"
                           "archive/v" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1db27zm6cldm1aw0570vii1bxc16a34x8lissl1h9rizsbwn7qkj"))))
    (build-system cmake-build-system)
    (arguments
     `(#:modules ((srfi srfi-26)
                  (guix build cmake-build-system)
                  (guix build utils))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'set-lua-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (let* ((lua-version "5.2")
                    (lua-cpath-spec
                     (lambda (prefix)
                       (let ((path (string-append prefix "/lib/lua/" lua-version)))
                         (string-append path "/?.so;" path "/?/?.so"))))
                    (lua-path-spec
                     (lambda (prefix)
                       (let ((path (string-append prefix "/share/lua/" lua-version)))
                         (string-append path "/?.lua;" path "/?/?.lua"))))
                    (lua-inputs (map (cute assoc-ref %build-inputs <>)
                                     '("lua"
                                       "lua-lpeg"
                                       "lua-bitop"
                                       "lua-libmpack"))))
               (setenv "LUA_PATH"
                       (string-join (map lua-path-spec lua-inputs) ";"))
               (setenv "LUA_CPATH"
                       (string-join (map lua-cpath-spec lua-inputs) ";"))
               #t))))))
    (inputs
     `(("libuv" ,libuv)
       ("msgpack" ,msgpack)
       ("libtermkey" ,libtermkey)
       ("libvterm" ,libvterm)
       ("unibilium" ,unibilium)
       ("jemalloc" ,jemalloc)
       ("libiconv" ,libiconv)
       ("lua" ,lua-5.2)
       ("lua-lpeg" ,lua5.2-lpeg)
       ("lua-bitop" ,lua5.2-bitop)
       ("lua-libmpack" ,lua5.2-libmpack)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("gettext" ,gettext-minimal)
       ("gperf" ,gperf)))
    (home-page "http://neovim.io")
    (synopsis "Fork of vim focused on extensibility and agility")
    (description "Neovim is a project that seeks to aggressively
refactor Vim in order to:

@itemize
@item Simplify maintenance and encourage contributions
@item Split the work between multiple developers
@item Enable advanced external UIs without modifications to the core
@item Improve extensibility with a new plugin architecture
@end itemize\n")
    ;; Neovim is licensed under the terms of the Apache 2.0 license,
    ;; except for parts that were contributed under the Vim license.
    (license (list license:asl2.0 license:vim))))

(define-public vifm
  (package
    (name "vifm")
    (version "0.8.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://sourceforge/vifm/vifm/vifm-"
                            version ".tar.bz2"))
        (sha256
         (base32
          "07r15kq7kjl3a41sd11ncpsii866xxps4f90zh3lv8jqcrv6silb"))))
    (build-system gnu-build-system)
    (arguments
    '(#:phases
      (modify-phases %standard-phases
        (add-after 'patch-source-shebangs 'patch-test-shebangs
          (lambda _
            (substitute* (find-files "tests" "\\.c$")
              (("/bin/sh") (which "sh")))
            #t)))))
    (native-inputs
     `(("groff" ,groff) ; for the documentation
       ("perl" ,perl)))
    (inputs
     `(("libx11" ,libx11)
       ("ncurses" ,ncurses)))
    (home-page "http://vifm.info/")
    (synopsis "Flexible vi-like file manager using ncurses")
    (description "Vifm is a file manager providing a @command{vi}-like usage
experience.  It has similar keybindings and modes (e.g. normal, command line,
visual).  The interface uses ncurses, thus vifm can be used in text-only
environments.  It supports a wide range of features, some of which are known
from the @command{vi}-editor:
@enumerate
@item utf8 support
@item user mappings (almost like in @code{vi})
@item ranges in command
@item line commands
@item user defined commands (with support for ranges)
@item registers
@item operation undoing/redoing
@item fuse file systems support
@item trash
@item multiple files renaming
@item support of filename modifiers
@item colorschemes support
@item file name color according to file type
@item path specific colorscheme customization
@item bookmarks
@item operation backgrounding
@item customizable file viewers
@item handy @code{less}-like preview mode
@item filtering out and searching for files using regular expressions
@item one or two panes view
@end enumerate
With the package comes a plugin to use vifm as a vim file selector.")
    (license license:gpl2+)))
