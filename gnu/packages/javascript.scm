;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2017, 2019, 2020, 2022, 2023 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017, 2018, 2020, 2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017-2020, 2022-2024 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2021 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2021 Maxim Cournoyer <maxim@guixotic.coop>
;;; Copyright © 2022 Frank Pursel <frank.pursel@gmail.com>
;;; Copyright © 2023 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2025 Ashvith Shetty <ashvithshetty0010@zohomail.in>
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

(define-module (gnu packages javascript)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages c)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages java)
  #:use-module (gnu packages node)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages uglifyjs)
  #:use-module (gnu packages web)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system ant)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system minify)
  #:use-module (guix build-system trivial)
  #:use-module (guix utils))

(define-public cjson
  (package
    (name "cjson")
    (version "1.7.18")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/DaveGamble/cJSON")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32 "08p37q4i3za3dgz7wynma1fh8y4rq7pyzyjzcda710nxrmsm1pyv"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags '("-DENABLE_CJSON_UTILS=On")))
    (home-page "https://github.com/DaveGamble/cJSON")
    (synopsis "JSON parser written in ANSI C")
    (description "This library provides a portable embeddable JSON parser.")
    (license license:expat)))

(define-public js-context-menu
  (package
    (name "js-context-menu")
    (version "0.6.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/zorkow/context-menu")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1q063l6477z285j6h5wvccp6iswvlp0jmb96sgk32sh0lf7nhknh"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (chdir (assoc-ref %build-inputs "source"))
         (let ((target (string-append %output "/share/javascript/context-menu")))
           (apply invoke (search-input-file %build-inputs "/bin/esbuild")
                  "--bundle"
                  "--tsconfig=tsconfig.json"
                  (string-append "--outdir=" target)
                  (find-files "ts" "\\.ts$"))))))
    (native-inputs
     (list esbuild))
    (home-page "https://github.com/zorkow/context-menu")
    (synopsis "Generic context menu")
    (description "This package provides a reimplementation of the MathJax
context menu in TypeScript.")
    (license license:asl2.0)))

(define-public font-mathjax
  (package
    (name "font-mathjax")
    (version "2.7.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/mathjax/MathJax")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "127j12g7v2hx6k7r00b8cp49s7nkrwhxy6l8p03pw34xpxbgbimm"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils)
                      (ice-9 match))
         (let ((install-directory (string-append %output "/share/fonts/mathjax")))
           (mkdir-p install-directory)
           (copy-recursively (string-append (assoc-ref %build-inputs "source")
                                            "/fonts")
                             install-directory)))))
    (home-page "https://www.mathjax.org/")
    (synopsis "Fonts for MathJax")
    (description "This package contains the fonts required for MathJax.")
    (license license:asl2.0)))

(define-public js-mathjax
  (package
    (inherit font-mathjax)
    (name "js-mathjax")
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils)
                      (ice-9 match)
                      (ice-9 popen)
                      (ice-9 regex))
         (set-path-environment-variable
          "PATH" '("bin") (map (match-lambda
                                 ((_ . input)
                                  input))
                               %build-inputs))
         (set-path-environment-variable
          "GUIX_LOCPATH" '("lib/locale")
          (list (assoc-ref %build-inputs "glibc-utf8-locales")))
         (setenv "LANG" "en_US.UTF-8")
         (let ((install-directory (string-append %output "/share/javascript/mathjax")))
           (copy-recursively (string-append (assoc-ref %build-inputs "source") "/unpacked")
                             "MathJax-unpacked")
           (mkdir-p install-directory)
           (symlink (string-append (assoc-ref %build-inputs "font-mathjax")
                                   "/share/fonts/mathjax")
                    (string-append install-directory "/fonts"))

           (for-each
            (lambda (file)
              (let ((installed (string-append install-directory
                                              ;; remove prefix "./MathJax-unpacked"
                                              (string-drop file 18))))
                (format #t "~a -> ~a~%" file installed)
                (cond
                 ((string-match "\\.js$" file)
                  (mkdir-p (dirname installed))
                  (let ((minified (open-pipe* OPEN_READ "uglifyjs" file)))
                    (call-with-output-file installed
                      (lambda (port)
                        (dump-port minified port)))

                    (let ((exit (close-pipe minified)))
                      (unless (zero? exit)
                        (error "dear, uglifyjs failed" exit)))))
                 (else
                  (install-file file (dirname installed))))))
            (find-files "."))

           #t))))
    (native-inputs
     `(("font-mathjax" ,font-mathjax)
       ("glibc-utf8-locales" ,(libc-utf8-locales-for-target))
       ("uglifyjs" ,node-uglify-js)
       ,@(package-native-inputs font-mathjax)))
    (synopsis "JavaScript display engine for LaTeX, MathML, and AsciiMath")
    (description "MathJax is a JavaScript display engine for LaTeX, MathML,
and AsciiMath notation that works in all modern browsers.  It requires no
plugins or software to be installed on the browser.  So the page author can
write web documents that include mathematics and be confident that readers will
be able to view it naturally and easily.")))

(define-public js-mathjax-3
  (package
    (name "js-mathjax")
    (version "3.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/mathjax/MathJax-src")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "05lm6nw7rzpcc5yz7xsjxi4id9369vvnrksx82nglxrqrpws97wx"))
       (patches (search-patches "mathjax-disable-webpack.patch"
                                "mathjax-no-a11y.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'prepare-sources
           (lambda* (#:key inputs #:allow-other-keys)
             ;; All a11y components depend on speech-rule-engine, which cannot be
             ;; built from source. Since this only affects accessibility, remove them.
             (delete-file-recursively "ts/a11y")
             (delete-file-recursively "components/src/a11y")
             (delete-file-recursively "components/src/sre")
             (delete-file-recursively "components/src/node-main")

             ;; Copy sources of dependencies, so we can create symlinks.
             (mkdir-p "node_modules")
             (with-directory-excursion "node_modules"
               (for-each
                (lambda (p)
                 (copy-recursively (assoc-ref inputs (string-append "node-" p)) p))
                '("mj-context-menu" "mhchemparser")))

             ;; Make sure esbuild can find imports. This way we don’t have to rewrite files.
             (symlink "ts" "js")
             (symlink "ts" "node_modules/mj-context-menu/js")))
         (delete 'configure)
         (replace 'build
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((esbuild (string-append (assoc-ref inputs "esbuild")
                                           "/bin/esbuild"))
                   (node (string-append (assoc-ref inputs "node")
                                        "/bin/node"))
                   (target (string-append (assoc-ref outputs "out")
                                          "/share/javascript/mathjax/es5")))
               ;; Preprocess files and generate lib/ subdirs.
               (invoke node "components/bin/makeAll")
               ;; Build components.
               (apply
                invoke
                esbuild
                "--bundle"
                "--minify"
                ;; esbuild cannot transpile some features to ES5, so use ES6 instead.
                "--target=es6"
                (string-append "--outdir=" target)
                "--sourcemap"
                "--outbase=components/src"
                "--define:__dirname=\"/\""
                ;; In the browser the global object is window, see
                ;; https://developer.mozilla.org/en-US/docs/Glossary/Global_object
                "--define:global=window"
                ;; Find all component entry points, which have the same name as their
                ;; parent directory.
                (filter
                 (lambda (f)
                     (string=?
                       (basename (dirname f))
                       (string-drop-right (basename f) 3)))
                 (find-files "components/src" "\\.js$")))
               ;; Move all .js files into their parent directory, where MathJax
               ;; expects them.
               (for-each
                 (lambda (f)
                     (rename-file f (string-append (dirname (dirname f)) "/" (basename f))))
                 (find-files target "\\.js(\\.map)?$"))
               ;; Copy font files.
               (copy-recursively
                 "ts/output/chtml/fonts/tex-woff-v2"
                 (string-append target "/output/chtml/fonts/woff-v2")))))
         (delete 'check)
         (delete 'install))))
    (native-inputs
     `(("esbuild" ,esbuild)
       ("node" ,node-lts)
       ("node-mj-context-menu"
        ,(let ((name "context-menu")
               (version "0.6.1"))
           (origin
             (method git-fetch)
             (uri (git-reference
                    (url "https://github.com/zorkow/context-menu.git")
                    (commit (string-append "v" version))))
             (file-name (git-file-name name version))
             (sha256
              (base32
               "1q063l6477z285j6h5wvccp6iswvlp0jmb96sgk32sh0lf7nhknh")))))
       ("node-mhchemparser"
        ,(let ((name "mhchemparser")
               ;; Version 4.1.1. There are no tags.
               (version "b1bd0670df7e9bbd5a724ac642aa2664d6e500b3"))
           (origin
             (method git-fetch)
             (uri (git-reference
                    (url "https://github.com/mhchem/mhchemParser.git")
                    (commit version)))
             (file-name (git-file-name name version))
             (sha256
              (base32
               "1g72kbxzf9f2igidpma3jwk28ln4bp3qhwspmhnm79baf3701dgv")))))))
    (home-page "https://www.mathjax.org/")
    (synopsis (package-synopsis js-mathjax))
    (description (package-description js-mathjax))
    (license license:asl2.0)))

(define-public js-mathjax-for-r-mathjaxr
  (package
    (inherit js-mathjax-3)
    (name "js-mathjax")
    (version "3.1.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/mathjax/MathJax-src")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0kqcb6pl0zfs4hf8zqb4l50kkfq7isv35vpy05m0lg0yr9w0w4ai"))
       (patches (search-patches "mathjax-disable-webpack.patch"
                                "mathjax-3.1.2-no-a11y.patch"))))
    (arguments
     (substitute-keyword-arguments (package-arguments js-mathjax-3)
       ((#:phases phases '%standard-phases)
        `(modify-phases ,phases
           (replace 'prepare-sources
             (lambda* (#:key inputs #:allow-other-keys)
               ;; All a11y components depend on speech-rule-engine, which cannot be
               ;; built from source. Since this only affects accessibility, remove them.
               (delete-file-recursively "ts/a11y")
               (delete-file-recursively "components/src/a11y")
               (delete-file-recursively "components/src/sre")
               (delete-file-recursively "components/src/node-main")

               ;; Copy sources of dependencies, so we can create symlinks.
               (mkdir-p "node_modules")
               (with-directory-excursion "node_modules"
                 (for-each
                  (lambda (p)
                    (copy-recursively (assoc-ref inputs (string-append "node-" p)) p))
                  '("mj-context-menu")))

               ;; Make sure esbuild can find imports. This way we don’t have to rewrite files.
               (symlink "ts" "js")
               (symlink "ts" "node_modules/mj-context-menu/js")))))))
    (native-inputs
     `(("esbuild" ,esbuild)
       ("node" ,node-lts)
       ("node-mj-context-menu"
        ,(let ((name "context-menu")
               (version "0.6.1"))
           (origin
             (method git-fetch)
             (uri (git-reference
                    (url "https://github.com/zorkow/context-menu.git")
                    (commit (string-append "v" version))))
             (file-name (git-file-name name version))
             (sha256
              (base32
               "1q063l6477z285j6h5wvccp6iswvlp0jmb96sgk32sh0lf7nhknh")))))))))

(define-public js-commander
  (package
    (name "js-commander")
    (version "6.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/tj/commander.js")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "126m25s6mxpxmdj4aw5awz06b47r8r798lcf1c5bnmmh39cik5i1"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (chdir (assoc-ref %build-inputs "source"))
         (let ((esbuild (search-input-file %build-inputs "/bin/esbuild"))
               (target (string-append %output "/share/javascript/commander")))
           (invoke esbuild
                   "--bundle"
                   "--minify"
                   "--tsconfig=tsconfig.json"
                   "--platform=node"
                   (string-append "--outfile=" target "/index.min.js")
                   "index.js")))))
    (native-inputs
     (list esbuild))
    (home-page "https://github.com/tj/commander.js")
    (synopsis "Library for node.js command-line interfaces")
    (description "Commander.js aims to be the complete solution for node.js
command-line interfaces.")
    (license license:expat)))

(define-public js-xmldom-sre
  ;; This commit corresponds to the untagged release 0.1.32
  (let ((commit "3c79325bc2c9e5d27e3ba44b680fa8c5dd6a381d")
        (revision "1"))
    (package
      (name "js-xmldom-sre")
      (version "0.1.32")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/zorkow/xmldom/")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0a88v0id3mjflpvjqhv8a28br0xvaaszxbf7alg6pxfbnkb69yyq"))))
      (build-system trivial-build-system)
      (arguments
       `(#:modules ((guix build utils))
         #:builder
         (begin
           (use-modules (guix build utils))
           (chdir (assoc-ref %build-inputs "source"))
           (let ((esbuild (search-input-file %build-inputs "/bin/esbuild"))
                 (target (string-append %output "/share/javascript/xmldom-sre")))
             (invoke esbuild
                     "--bundle"
                     "--minify"
                     "--platform=node"
                     (string-append "--outfile=" target "/dom-parser.min.js")
                     "dom-parser.js")))))
      (native-inputs
       (list esbuild))
      (home-page "https://github.com/zorkow/xmldom/")
      (synopsis "DOM parser and XML serializer")
      (description "This is a fork of the xmldom library.  It allows the use
of wicked-good-xpath together with xmldom.")
      ;; One of these licenses may be selected.
      (license (list license:expat
                     license:lgpl2.0)))))

(define-public js-respond
  (package
    (name "js-respond")
    (version "1.4.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/scottjehl/Respond")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "00xid731rirc7sdy1gc8qal3v9g0agr2qx15hm4x97l1lcbylyn2"))))
    (build-system minify-build-system)
    (arguments
     `(#:javascript-files '("src/matchmedia.addListener.js"
                            "src/matchmedia.polyfill.js"
                            "src/respond.js")))
    (home-page "https://github.com/scottjehl/Respond")
    (synopsis "Polyfill for min/max-width CSS3 Media Queries")
    (description "The goal of this script is to provide a fast and lightweight
script to enable responsive web designs in browsers that don't support CSS3
Media Queries.")
    (license license:expat)))

(define-public js-html5shiv
  (package
    (name "js-html5shiv")
    (version "3.7.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/aFarkas/html5shiv")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0y1c5nyq0brl9fjdihhax33vks4s1ij9iv113879sg3zflmgqpd0"))))
    (build-system minify-build-system)
    (home-page "https://github.com/aFarkas/html5shiv")
    (synopsis "Enable HTML5 sectioning elements in legacy browsers")
    (description "The HTML5 Shiv enables use of HTML5 sectioning elements in
legacy Internet Explorer and provides basic HTML5 styling for Internet
Explorer 6-9, Safari 4.x (and iPhone 3.x), and Firefox 3.x.")
    ;; From the file "MIT and GPL2 licenses.md":
    ;;
    ;;   This software is licensed under a dual license system (MIT or GPL
    ;;   version 2). This means you are free to choose with which of both
    ;;   licenses (MIT or GPL version 2) you want to use this library.
    (license (list license:expat license:gpl2))))

(define-public js-json2
  (let ((commit "031b1d9e6971bd4c433ca85e216cc853f5a867bd")
        (revision "1"))
    (package
      (name "js-json2")
      (version (string-append "2016-10-28." revision "-" (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/douglascrockford/JSON-js")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "1fvb6b2y5sd3sqdgcj683sdxcbxdii34q0ysc9wg0dq1sy81l11v"))))
      (build-system minify-build-system)
      (arguments
       `(#:javascript-files '("json2.js"
                              "json_parse.js"
                              "json_parse_state.js"
                              "cycle.js")))
      (home-page "https://github.com/douglascrockford/JSON-js")
      (synopsis "JSON encoders and decoders")
      (description "The files in this collection implement JSON
encoders/decoders in JavaScript.

@code{json2.js}: This file creates a JSON property in the global object, if
there isn't already one, setting its value to an object containing a stringify
method and a parse method.  The @code{parse} method uses the @code{eval}
method to do the parsing, guarding it with several regular expressions to
defend against accidental code execution hazards.  On current browsers, this
file does nothing, preferring the built-in JSON object.

@code{json_parse.js}: This file contains an alternative JSON @code{parse}
function that uses recursive descent instead of @code{eval}.

@code{json_parse_state.js}: This files contains an alternative JSON
@code{parse} function that uses a state machine instead of @code{eval}.

@code{cycle.js}: This file contains two functions, @code{JSON.decycle} and
@code{JSON.retrocycle}, which make it possible to encode cyclical structures
and DAGs in JSON, and to then recover them.  This is a capability that is not
provided by ES5.  @code{JSONPath} is used to represent the links.")
      (license license:public-domain))))

(define (make-js-lunr-lang lang-name abbrev)
  (package
    (name (string-append "js-lunr-" abbrev))
    (version "1.13.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/MihaiValentin/lunr-languages")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "03q1awcg5plxdzxg89cd4x8lvnjfba541kbx59q2c6ly7dh4pyv6"))))
    (build-system minify-build-system)
    (arguments
     (list
      #:javascript-files
      #~(list #$(string-append "lunr." abbrev ".js"))
      #:phases
      #~(modify-phases %standard-phases
          (delete 'install-license-files))))
    (home-page "https://github.com/MihaiValentin/lunr-languages")
    (synopsis (string-append lang-name "stemmers and stopwords"))
    (description
     (format #f "This package provides ~a stemmers and stopwords for the Lunr \
Javascript library."
             lang-name))
    (license license:expat)))

(define-public js-lunr-ar (make-js-lunr-lang "Arabic" "ar"))
(define-public js-lunr-da (make-js-lunr-lang "Danish" "da"))
(define-public js-lunr-de (make-js-lunr-lang "German" "de"))
(define-public js-lunr-el (make-js-lunr-lang "Greek" "el"))
(define-public js-lunr-es (make-js-lunr-lang "Spanish" "es"))
(define-public js-lunr-fi (make-js-lunr-lang "Finnish" "fi"))
(define-public js-lunr-fr (make-js-lunr-lang "French" "fr"))
(define-public js-lunr-he (make-js-lunr-lang "Hebrew" "he"))
(define-public js-lunr-hi (make-js-lunr-lang "Hindi" "hi"))
(define-public js-lunr-hu (make-js-lunr-lang "Hungarian" "hu"))
(define-public js-lunr-hy (make-js-lunr-lang "Armenian" "hy"))
(define-public js-lunr-it (make-js-lunr-lang "Italian" "it"))
(define-public js-lunr-ja (make-js-lunr-lang "Japanese" "ja"))
(define-public js-lunr-kn (make-js-lunr-lang "Kannada" "kn"))
(define-public js-lunr-ko (make-js-lunr-lang "Korean" "ko"))
(define-public js-lunr-nl (make-js-lunr-lang "Dutch" "nl"))
(define-public js-lunr-no (make-js-lunr-lang "Norwegian" "no"))
(define-public js-lunr-pt (make-js-lunr-lang "Portuguese" "pt"))
(define-public js-lunr-ro (make-js-lunr-lang "Romanian" "ro"))
(define-public js-lunr-ru (make-js-lunr-lang "Russian" "ru"))
(define-public js-lunr-sa (make-js-lunr-lang "Sanskrit" "sa"))
(define-public js-lunr-sv (make-js-lunr-lang "Swedish" "sv"))
(define-public js-lunr-ta (make-js-lunr-lang "Tamil" "ta"))
(define-public js-lunr-te (make-js-lunr-lang "Telugu" "te"))
(define-public js-lunr-th (make-js-lunr-lang "Thai" "th"))
(define-public js-lunr-tr (make-js-lunr-lang "Turkish" "tr"))
(define-public js-lunr-vi (make-js-lunr-lang "Vietnamese" "vi"))
(define-public js-lunr-zh (make-js-lunr-lang "Chinese" "zh"))

(define-public js-lunr-multi (make-js-lunr-lang "Multilanguages" "multi"))
(define-public js-lunr-stemmer-support
  (let ((pkg (make-js-lunr-lang "" "stemmer.support")))
    (package/inherit pkg
      (name "js-lunr-stemmer-support")
      (synopsis "Stemmer support for Lunr")
      (description
       "This package provides stemmer support for the Lunr Javascript
library."))))

(define-public js-strftime
  (package
    (name "js-strftime")
    (version "0.10.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url"https://github.com/samsonjs/strftime")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "131nmlivazwxyba25kh9lda99749fq4xsyin6lzfalaaydviby4p"))))
    (build-system minify-build-system)
    (arguments
     `(#:javascript-files '("strftime.js")))
    (home-page "https://github.com/samsonjs/strftime")
    (synopsis "Implementation of strftime to JavaScript")
    (description "This is an implementation of the @code{strftime} procedure
for JavaScript.  It works in (at least) node.js and browsers.  It supports
localization and timezones.  Most standard specifiers from C are supported as
well as some other extensions from Ruby.")
    (license license:expat)))

(define-public js-highlight
  (package
    (name "js-highlight")
    (version "9.12.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/isagalaev/highlight.js")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "12qz22qjpd6svj58pwgcwg2x2rzhihfdrxg6lgj39nfpaln6dris"))))
    (build-system minify-build-system)
    (arguments
     `(#:javascript-files '("src/highlight.js")))
    (home-page "https://github.com/isagalaev/highlight.js")
    (synopsis "Syntax highlighting for JavaScript")
    (description "Highlight.js is a syntax highlighter written in JavaScript.
It works in the browser as well as on the server.  It works with pretty much
any markup, doesn’t depend on any framework and has automatic language
detection.")
    (license license:bsd-3)))

(define-public js-datatables
  (package
    (name "js-datatables")
    (version "1.10.19")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://datatables.net/releases/DataTables-"
                                  version ".zip"))
              (sha256
               (base32
                "0cff8a1g7pjwbjdqq0yzqd963ar7pfi4splmm6rwdzganr77rkhb"))))
    (build-system minify-build-system)
    (arguments
     `(#:javascript-files '("media/js/dataTables.bootstrap.js"
                            "media/js/jquery.dataTables.js")))
    (native-inputs
     (list unzip))
    (home-page "https://datatables.net")
    (synopsis "DataTables plug-in for jQuery")
    (description "DataTables is a table enhancing plug-in for the jQuery
Javascript library, adding sorting, paging and filtering abilities to plain
HTML tables with minimal effort.")
    (license license:expat)))

(define-public js-datatables-1.9
  (package
    (inherit js-datatables)
    (name "js-datatables")
    (version "1.9.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://datatables.net/releases/DataTables-"
                                  version ".zip"))
              (sha256
               (base32
                "0yd6548cbpb4hlpwybjp93b9m084n5rba6v1x5n83y0dvlxcd06g"))))
    (arguments
     (list #:javascript-files '(list "media/js/jquery.dataTables.js")))))

(define-public js-requirejs
  (package
    (name "js-requirejs")
    (version "2.3.6")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/requirejs/requirejs")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0cvd5y2mb3h6yil3niqn3gjqrzixdsxcz4rvc2f0hg4kzp5y0w86"))))
    (build-system minify-build-system)
    (arguments `(#:javascript-files '("require.js")))
    (home-page "https://github.com/requirejs/requirejs/")
    (synopsis "File and module loader for JavaScript")
    (description "RequireJS loads plain JavaScript files as well as more
defined modules.  It is optimized for in-browser use, including in a Web
Worker, but it can be used in other JavaScript environments.")
    (license license:expat)))

(define-public js-selectize
  (package
    (name "js-selectize")
    (version "0.12.6")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/selectize/selectize.js")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "15gichl8wi6yxag2ps723nxrgyan15976dzsnvw9h9py8sbyyzjn"))))
    (build-system minify-build-system)
    ;; We use the standalone file instead of src/selectize.js because the
    ;; former includes the source code for MicroEvent and other modules that
    ;; Selectize refers to.
    (arguments `(#:javascript-files '("dist/js/standalone/selectize.js")))
    (home-page "https://selectize.github.io/selectize.js/")
    (synopsis "Hybrid widget between a textbox and <select> box")
    (description "Selectize is the hybrid of a textbox and @code{<select>}
box.  It's jQuery based and it has autocomplete and native-feeling keyboard
navigation; it is useful for tagging, contact lists, etc.")
    (license license:asl2.0)))

(define-public js-es5-shim
  (package
    (name "js-es5-shim")
    (version "4.5.13")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/es-shims/es5-shim")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "142w384fbyllq4yggv173g82lw3wix4jqcg6hkhx1ymq89vvnpmh"))))
    (build-system minify-build-system)
    (arguments `(#:javascript-files
                 '("es5-sham.js"
                   "es5-shim.js")))
    (home-page "https://github.com/es-shims/es5-shim")
    (synopsis "ECMAScript 5 compatibility shims for legacy JavaScript engines")
    (description "@code{es5-shim.js} patches a JavaScript context to contain
all ECMAScript 5 methods that can be faithfully emulated with a legacy
JavaScript engine.  @code{es5-sham.js} patches other ES5 methods as closely as
possible.  Many of these shams are intended only to allow code to be written
to ES5 without causing run-time errors in older engines.  In many cases, this
means that these shams cause many ES5 methods to silently fail.")
    (license license:expat)))

(define-public js-filesaver
  (package
    (name "js-filesaver")
    (version "1.3.8")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/eligrey/FileSaver.js")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0gvqk0hnr8fig0n4da7vj7q6z31bcyv52916xz3rbmdj3pgpiv1d"))))
    (build-system minify-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-uglification
           ;; Remove "export" keyword. That keyword is not present in
           ;; the minified version of the library some projects are using,
           ;; e.g.,
           ;; <https://github.com/jmoenig/Snap--Build-Your-Own-Blocks/blob/master/FileSaver.min.js>
           (lambda _
             (substitute* "src/FileSaver.js"
               (("export ") ""))
             #t))
         (add-after 'install 'install-unminified-version
           (lambda* (#:key outputs #:allow-other-keys)
             (install-file "src/FileSaver.js"
                           (string-append (assoc-ref outputs "out")
                                          "/share/javascript"))
             #t)))))
    (home-page
     "https://eligrey.com/blog/saving-generated-files-on-the-client-side/")
    (synopsis "HTML5 saveAs() FileSaver implementation")
    (description "@file{FileSaver.js} implements the @code{saveAs()}
FileSaver interface in browsers that do not natively support it.

@file{FileSaver.js} is the solution to saving files on the
client-side, and is perfect for webapps that need to generate files,
or for saving sensitive information that shouldn't be sent to an
external server.")
    (license license:expat)))

(define-public js-scianimator
  (package
    (name "js-scianimator")
    (version "1.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/brentertz/scianimator.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0b4r6z85gnsjagdchvf1pvrhylfiaidh701hna8jrm0l4kbb735x"))))
    (build-system minify-build-system)
    (arguments
     '(#:javascript-files
       (list "assets/js/jquery.scianimator.js")))
    (home-page "https://github.com/brentertz/scianimator")
    (synopsis "Scientific image animator plugin for jQuery")
    (description "SciAnimator provides a simple yet powerful interface for
animating a series of images.")
    (license license:expat)))

(define-public mujs
  (package
    (name "mujs")
    (version "1.3.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ccxvii/mujs")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1p90cb830li6y38m748s4kz2pkimxarbcaym3bwrxnk3jaqcf69q"))
       (snippet
        #~(begin
            (use-modules (guix build utils))
            (for-each delete-file
                      (list "astnames.h"
                            "opnames.h"
                            "one.c"))))))
    (build-system gnu-build-system)
    (arguments
     (list
       #:phases
       #~(modify-phases %standard-phases
           (delete 'configure)          ; no configure script
           (replace 'install
             (lambda* (#:key (make-flags '()) #:allow-other-keys)
               (apply invoke "make" "install-shared" make-flags))))
       #:make-flags
       #~(list (string-append "VERSION=" #$version)
               (string-append "CC=" #$(cc-for-target))
               (string-append "prefix=" #$output))
       #:tests? #f))                    ; no tests
    (inputs
     (list readline))
    (home-page "https://mujs.com/")
    (synopsis "JavaScript interpreter written in C")
    (description "MuJS is a lightweight Javascript interpreter designed for
embedding in other software to extend them with scripting capabilities.  MuJS
was designed with a focus on small size, correctness, and simplicity.  It is
written in portable C and implements ECMAScript as specified by ECMA-262.  The
interface for binding with native code is designed to be as simple as possible
to use, and is very similar to Lua.  There is no need to interact with byzantine
C++ template mechanisms, or worry about marking and unmarking garbage collection
roots, or wrestle with obscure build systems.")
    (license license:isc)))

(define-public quickjs
  (package
    (name "quickjs")
    (version "2025-04-26")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://bellard.org/quickjs/quickjs-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "1gw39jazggpbrjmkkmkhsj2wn0mmaw6wb0gkh7vzcvhn4m60f81g"))
              (snippet
               #~(begin (use-modules (guix build utils))
                        (for-each delete-file
                                  '("doc/quickjs.pdf"
                                    "doc/quickjs.html"))))))
    (build-system gnu-build-system)
    (arguments
     (list #:make-flags
           #~(list (string-append "PREFIX=" #$output)
                   #$@(if (or (target-riscv64?)
                              (target-ppc32?))
                          '("LDFLAGS=-latomic")
                          '()))
           #:phases #~(modify-phases %standard-phases
                        (delete 'configure)
                        (replace 'check
                          (lambda* (#:key tests? #:allow-other-keys)
                            (when tests?
                              (invoke "make" "microbench")))))))
    (home-page "https://bellard.org/quickjs/")
    (synopsis "Small embeddable Javascript engine")
    (description "QuickJS supports the ES2023 specification including modules,
asynchronous generators, proxies, BigInt, BigDecimal, BigFloat and operator
overloading.  It can compile Javascript sources to executables with no external
dependency.  It includes a command line interpreter with contextual colorization
implemented in Javascript and a small built-in standard library with C library
wrappers.")
    (license license:expat)))

(define-public test262-source
  (origin
    (method git-fetch)
    (uri (git-reference (url "https://github.com/tc39/test262")
                        (commit "e7a84b0a090ca86cc05f2e90116115c8c2a7c059")))
    (file-name "test262")
    (sha256 (base32 "0y1w6ypjpam6dal75cwvwwmrrhh7glx9pp812yxvy8rc8w9sfwyw"))
    (modules '((guix build utils)))
    (snippet #~(begin
                 (for-each delete-file
                           (list "ECMA TR-104.pdf" "docs/coverage.html"
                                 "docs/ES3_ES5_Changes.xlsx"))))))

(define-public quickjs-ng
  (package
    (name "quickjs-ng")
    (version "0.9.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/quickjs-ng/quickjs")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0lbjayp6x5784dxa7ll67isjjiqajg4hx3ls6fyzzfk6hd44jkzw"))))
    (arguments
     (list
      ;; Data model is ILP32 in 32bit, LP64 in 64bit
      ;; https://docs.oracle.com/cd/E19620-01/805-3024/lp64-1/index.html
      #:tests? (and (not (%current-target-system))
                    (target-64bit?))
      #:configure-flags
      #~(list "-DBUILD_SHARED_LIBS:BOOL=TRUE"
              "-DQJS_BUILD_EXAMPLES:BOOL=FALSE"
              "-DQJS_BUILD_CLI_WITH_MIMALLOC:BOOL=TRUE"
              "-DQJS_ENABLE_ASAN:BOOL=FALSE"
              "-DQJS_ENABLE_UBSAN:BOOL=FALSE"
              "-DQJS_ENABLE_MSAN:BOOL=FALSE"
              "-DQJS_ENABLE_TSAN:BOOL=FALSE"
              ;; Only works with static build
              "-DQJS_BUILD_LIBC:BOOL=FALSE"
              #$@(if (or (target-riscv64?)
                         (target-ppc32?))
                     #~("-DCMAKE_EXE_LINKER_FLAGS=-latomic")
                     #~()))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'configure 'prepare-tests
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* '("test262.conf" "test262-fast.conf"
                             "test262_errors.txt")
                (("test262/")
                 (string-append (assoc-ref inputs "test262") "/")))
              (substitute* "tests/test_std.js"
                (("/bin/sh")
                 (which "sh")))
              (substitute* "Makefile"
                ((" \\$\\(QJS\\)")
                 "")
                (("BUILD_DIR=build")
                 "BUILD_DIR=../build"))))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (with-directory-excursion "../source"
                  (invoke "make" "test")
                  (invoke "make" "test262-fast")
                  (invoke "make" "microbench")
                  (invoke "../build/qjs" "-c" "examples/hello.js" "-o" "hello")
                  (invoke "./hello")
                  (invoke "../build/api-test"))))))))
    (build-system cmake-build-system)
    (native-inputs (list test262-source))
    (inputs (list mimalloc))
    (home-page "https://quickjs-ng.github.io/quickjs/")
    (synopsis "Small embeddable Javascript engine")
    (description
     "@code{quickjs-ng} supports the ES2023 specification including modules,
asynchronous generators, proxies, BigInt, BigDecimal, BigFloat and operator
overloading.  It can compile Javascript sources to executables with no external
dependency.  It includes a command line interpreter with contextual colorization
implemented in Javascript and a small built-in standard library with C library
wrappers.")
    ;; 3-clause BSD license for test262
    (license (list license:expat license:bsd-3))))

(define-public duktape
  (package
    (name "duktape")
    (version "2.6.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://duktape.org/duktape-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "19szwxzvl2g65fw95ggvb8h0ma5bd9vvnnccn59hwnc4dida1x4n"))))
    (build-system gnu-build-system)
    (arguments
     (list #:tests? #f                  ; No tests.
           #:make-flags
           #~(list "-f" "Makefile.sharedlibrary"
                   (string-append "INSTALL_PREFIX="
                                  ;; XXX Replace with #$output on core-updates.
                                  #$(if (%current-target-system)
                                        #~#$output
                                        #~%output))
                   ;; XXX Unconditionally set to CC-FOR-TARGET on core-updates.
                   #$@(if (%current-target-system)
                          #~((string-append "CC=" #$(cc-for-target)))
                          #~()))
           #:phases
           #~(modify-phases %standard-phases
               (delete 'configure)
               ;; At least another major GNU/Linux distribution carries their own
               ;; .pc file with this package.
               (add-after 'install 'install-pkg-config
                 (lambda* (#:key outputs #:allow-other-keys)
                   (let* ((out (assoc-ref outputs "out"))
                          (pkg-config-dir (string-append out "/lib/pkgconfig")))
                     (mkdir-p pkg-config-dir)
                     (with-output-to-file (string-append pkg-config-dir "/duktape.pc")
                       (lambda _
                         (format #t "prefix=~@*~a~@
                               libdir=${prefix}/lib~@
                               includedir=${prefix}/include~@

                               Name: duktape~@
                               Description: Embeddable Javascript engine~@
                               Version: ~a~@
                               Libs: -L${libdir} -lduktape~@
                               Cflags: -I${includedir}~%"
                                 out #$version)))))))))
    (home-page "https://duktape.org/")
    (synopsis "Small embeddable Javascript engine")
    (description "Duktape is an embeddable Javascript engine, with a focus on
portability and compact footprint.  Duktape is easy to integrate into a C/C++
project: add @file{duktape.c}, @file{duktape.h}, and @file{duk_config.h} to
your build, and use the Duktape API to call ECMAScript functions from C code
and vice versa.")
    (license license:expat)))

(define-public rhino
    (package
      (name "rhino")
      (version "1.7.7.2")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/mozilla/rhino.git")
                      (commit "935942527ff434b205e797df4185518e5369466e")))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "09i4yr98hs6855fs7fhgmrpiwpr90lhxdv2bvfj97nn4rv1d7wl8"))
                (modules '((guix build utils)))
                (snippet '(begin
                            ;; Remove benchmark testing
                            (delete-file-recursively "testsrc/benchmarks")
                            (delete-file-recursively
                             "testsrc/org/mozilla/javascript/benchmarks")
                            ;; Identify bundled jars
                            (format #t "~%~a~%" "Sourced jars")
                            (for-each (lambda (f)
                                        (format #t "~/Deleting: ~a~%" f)
                                        (delete-file f))
                                      (find-files "." "\\.jar$"))))))
      (build-system ant-build-system)
      (inputs (list bash-minimal))
      (native-inputs (list java-junit java-hamcrest-core java-snakeyaml))
      (arguments
       `(#:phases
         (modify-phases
             %standard-phases
           (add-after 'unpack 'remove-build-dates
             ;; Avoid embedding build date for reproducible builds
             (lambda _
               (substitute*
                   "build.properties"
                 (("..implementation.date.") ""))))
           (replace 'check
             (lambda* (#:key tests? inputs native-inputs
                       #:allow-other-keys)
               (when tests?
                 (setenv "ANT_OPTS" "-Doffline=true")
                 (let ((junit-lib
                        (assoc-ref inputs "java-junit"))
                       (hamcrest-lib
                        (assoc-ref inputs "java-hamcrest-core"))
                       (snakeyaml-lib
                        (assoc-ref inputs "java-snakeyaml")))
                   (with-directory-excursion "testsrc"
                     (substitute* "build.xml"
                       (("<pathelement location=\"\\$\\{xbean.jar\\}\" */>" all)
                        (string-append "<!-- " all " -->"))
                       (("<pathelement location=\"\\$\\{jsr173.jar\\}\" */>" all)
                        (string-append "<!-- " all " -->"))
                       (("<pathelement path=\"\\$\\{coverage.classes.dir\\}\" */>"
                         all)
                        (string-append "<!-- " all " -->"))
                       (("<pathelement path=\"lib/emma.jar\"/>" all)
                        (string-append "<!-- " all " -->"))
                       (("<pathelement path=\"lib/junit.jar\" ?/>")
                        (string-append
                         "<fileset dir=\"" junit-lib "\" includes=\"**/*.jar\"/>"))
                       (("<pathelement path=\"lib/hamcrest.jar\" ?/>")
                        (string-append "<fileset dir=\"" hamcrest-lib
                                       "\" includes=\"**/*.jar\"/>"))
                       (("<pathelement path=\"lib/snakeyaml.jar\" ?/>")
                        (string-append "<fileset dir=\"" snakeyaml-lib
                         "\" includes=\"**/*.jar\"/>"))
                       ;; Disabling instrumentation.
                       (("(<target name=\"junit\" depends=\"junit-compile),.*"
                         all pre)
                        (string-append pre "\">"))))
                   (invoke "ant" "junit")))))
           (replace 'install
                      (lambda* (#:key inputs outputs #:allow-other-keys)
                        (let* ((out (assoc-ref outputs "out"))
                               (pkg+ver (string-append ,name ,version))
                               (bin (string-append out "/bin"))
                               (rhino (string-append bin "/rhino"))
                               (man (string-append out "/share/man/man1")))
                          (mkdir-p bin)
                          (install-file "man/rhino.1" man)
                          (install-file (string-append "build/" pkg+ver
                                                       "/js.jar")
                                        (string-append out "/share/java"))
                          (with-output-to-file rhino
                            (lambda _
                              (format #t "#!~a~%~a -jar ~a $@~%"
                                      (search-input-file inputs "/bin/bash")
                                      (search-input-file inputs "/bin/java")
                                      (string-append out "/share/java/js.jar"))))
                          (chmod rhino #o755)))))))
      (home-page "https://mozilla.github.io/rhino")
      (synopsis "Javascript implemented in Java")
      (description
       "Rhino implements ECMAScript, also known as JavaScript, in Java as
specified in the fifth edition of ECMA-262.")
      (license license:mpl2.0)))

