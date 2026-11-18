;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 José Miguel Sánchez García <jmi2k@openmailbox.org>
;;; Copyright © 2017, 2022 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018, 2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020 Tanguy Le Carrour <tanguy@bioneland.org>
;;; Copyright © 2022 (unmatched parenthesis <paren@disroot.org>
;;; Copyright © 2022 Trevor Richards <trev@trevdev.ca>
;;; Copyright © 2023 Gruruya <greytest@disroot.org>
;;; Copyright © 2025, 2026 Ashish SHUKLA <ashish.is@lostca.se>
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

(define-module (gnu packages nim)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (gnu packages bdw-gc)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tls))

(define atlas
  (origin
    (method git-fetch)
    (file-name "atlas-checkout")
    (uri
     (git-reference
      (url "https://github.com/nim-lang/atlas.git")
      ;; referenced in koch.nim
      (commit "fa19ace5c5d176d0e19f9b6ad08239ce2ceea676")))
    (sha256 (base32 "0b7cgv1yg9hpixv6w4a7xyhcf23qjl91rd9wrwfhs0vvk3izsj6c"))))

(define sat
  (origin
    (method git-fetch)
    (file-name "sat-checkout")
    (uri
     (git-reference
      (url "https://github.com/nim-lang/sat.git")
      ;; referenced in koch.nim
      (commit "9d52513b3c68bfb929dbd687d4fb2836cfee6936")))
    (sha256 (base32 "1f944w3w7imrnan88l5z4ff8s5ks00g4l7sg42img6bbh66hbnfb"))))

(define-public nim
  (package
    (name "nim")
    (version "2.4.2")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://nim-lang.org/download/"
                          name "-" version ".tar.xz"))
      (sha256
       (base32 "04c25npx42wv4xpxcq0vvgl8vsfmnf0wk1b6s6lxninf3zdg92xj"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f          ; TODO: Investigate tests failures.
       #:modules
       ((ice-9 rdelim)
        (ice-9 regex)
        (ice-9 match)
        ,@%default-gnu-modules)
       #:make-flags (list (string-append "CC=" ,(cc-for-target)))
       #:phases
       ,#~(modify-phases %standard-phases
             (delete 'configure)          ; no configure script
             (add-after 'unpack 'copy-deps:www
               (lambda _
                 (copy-recursively #$(this-package-input "atlas-checkout") "dist/atlas"
                                   #:keep-permissions? #f)
                 (copy-recursively #$(this-package-input "sat-checkout") "dist/atlas/dist/sat"
                                   #:keep-permissions? #f)))
             (add-after 'unpack 'patch-installer
               (lambda* (#:key outputs #:allow-other-keys)
                 (let ((out (assoc-ref outputs "out")))
                   (substitute* "install.sh"
                    (("/usr/local") out)
                    (("/lib/nim") "/")
                    (("/opt/nimble") (string-append out "/share/nimble"))
                    (("configdir=\"/etc/nim\"")
                     (string-append "configdir=\"" out "/etc/nim\""))))))
             (add-after 'unpack 'patch-dynamic-libraries
               (lambda* (#:key inputs native-inputs #:allow-other-keys)
                 (substitute* "lib/system.nim"
                   (("libgc\\.so")
                    (search-input-file (or native-inputs inputs)
                                       "/lib/libgc.so")))
                 (substitute* "lib/wrappers/openssl.nim"
                   (("libssl\\.so")
                    (search-input-file (or native-inputs inputs)
                                       "/lib/libssl.so"))
                   (("libcrypto\\.so")
                    (search-input-file (or native-inputs inputs)
                                       "/lib/libcrypto.so")))
                 (substitute* "lib/wrappers/pcre.nim"
                   (("libpcre\\.so")
                    (search-input-file (or native-inputs inputs)
                                       "/lib/libpcre.so")))))
             (add-after 'patch-source-shebangs 'patch-more-shebangs
               (lambda _

                 (define sh (which "sh"))
                 (define sh-len (string-length sh))

                 (define rx1 (make-regexp "^(.*NIM_CHAR data\\[)(8|15)(\\];.*)$" regexp/extended))

                 ;; } TM__HZdw8BhppcTQo8DIK46LSg_5 = {(7 | NIM_STRLIT_FLAG), "/bin/sh"};

                 (define rx2 (make-regexp
                              (string-append "^(\\} )"
                                             "([^[:space:]]+)"
                                             "( = \\{[(])"
                                             "(7|14)"
                                             "( [|] NIM_STRLIT_FLAG[)], )"
                                             "\"(/system)?/bin/sh\""
                                             "(.*)$")
                              regexp/extended))

                 (define (fixup-1 matches out)
                   (format out "~a~a~a\n"
                           ;;   NIM_CHAR data[(string-length (which "sh"))];
                           (match:substring matches 1) ;; NIM_CHAR data[
                           (+ 1 sh-len) ;; (string-length (which "sh")
                           (match:substring matches 3)) ;; ];
                   #f)

                 (define (fixup-2 matches out)
                   (format out "~a~a~a~a~a~s~a\n"
                           ;; } TM__HZdw8BhppcTQo8DIK46LSg_5 = { (string-length (which "sh")) | NIM_STRLIT_FLAG, (which "sh") };
                           (match:substring matches 1) ;; }
                           (match:substring matches 2) ;; TM__HZdw8BhppcTQo8DIK46LSg_5
                           (match:substring matches 3) ;; = {
                           sh-len ;; (string-length (which "sh"))
                           (match:substring matches 5) ;; | NIM_STRLIT_FLAG,
                           sh                          ;; (which "sh")
                           (match:substring matches 7)) ;; };
                   ;; return a reference to patch
                   (match:substring matches 2)) ;; TM__HZdw8BhppcTQo8DIK46LSg_5

                 (define fixups
                   (list (list rx1 fixup-1)
                         (list rx2 fixup-2)))

                 (define (rx-match rx-list line in out)
                   (if (null? rx-list)
                       (begin
                         (format out "~a\n" line)
                         #f)

                       (match rx-list
                         (((rx fixup) . rest)
                          (let ((matches (regexp-exec rx line)))
                            (if (regexp-match? matches)
                                (fixup matches out)
                                (rx-match rest line in out)))))))

                 (define (fixup-bin-sh-references in out)
                   (let loop ((line (read-line in))
                              (flagged #f))
                     (unless (eof-object? line)
                       (let* ((pat (and (string? flagged)
                                        (format #f "~a(7|14), ~a~a~a"
                                                (regexp-quote "{")
                                                (regexp-quote "((NimStrPayload*) (&")
                                                flagged
                                                (regexp-quote "))};"))))
                              (res (and (string? flagged)
                                        (string-match pat line))))
                         (if (regexp-match? res)
                             (begin
                               (format out
                                       "~a{~a, ((NimStrPayload*) (&~a))};\n"
                                       (substring line 0
                                                  (- (string-length line)
                                                     (string-length (match:substring res 0))))
                                       sh-len
                                       flagged)
                               (loop (read-line in) flagged))

                             (loop (read-line in)
                                   (rx-match fixups line in out)))))))

                 (substitute* '("tests/stdlib/tosprocterminate.nim"
                                "tests/stdlib/tstrscans.nim"
                                "lib/pure/osproc.nim"
                                "lib/pure/strscans.nim")
                   (("/bin/sh") sh))

                 (for-each (lambda (f)
                             (with-atomic-file-replacement f fixup-bin-sh-references))
                           (find-files "c_code" "@posproc\\.nim\\.c"))))
             (replace 'build
              (lambda args
                (apply (assoc-ref %standard-phases 'build) args)
                (setenv "XDG_CACHE_HOME" "./cache-home")
                (mkdir-p "./cache-home")
                (invoke "./bin/nim" "c" "-d:release" "koch")
                (invoke "./koch" "boot" "-d:release")
                (invoke "./koch" "tools")))
             (replace 'check
               (lambda* (#:key tests? #:allow-other-keys)
                 (when tests?
                   (invoke "./koch" "tests"))))
             (replace 'install
               (lambda* (#:key outputs #:allow-other-keys)
                 (let ((bin (string-append (assoc-ref outputs "out") "/bin")))
                   (mkdir-p bin)
                   (invoke "./install.sh" bin)
                   (for-each (lambda (file)
                               (install-file file bin))
                             (delete "testament" (find-files "bin"))))))
             (add-after 'install 'install-completions
               (lambda* (#:key outputs #:allow-other-keys)
                 (let* ((share (string-append (assoc-ref outputs "out") "/share"))
                        (bash  (string-append share "/bash-completion/completions"))
                        (zsh   (string-append share "/zsh/vendor_completions")))
                   (mkdir-p bash)
                   (mkdir-p zsh)
                   (copy-file "tools/nim.bash-completion"
                              (string-append bash "/nim"))
                   (copy-file "dist/nimble/nimble.bash-completion"
                              (string-append bash "/nimble"))
                   (copy-file "tools/nim.zsh-completion"
                              (string-append zsh "/_nim"))
                   (copy-file "dist/nimble/nimble.bash-completion"
                              (string-append zsh "/_nimble"))))))))
    (inputs (list atlas libgc openssl pcre sat sqlite))
    (native-inputs (list nss-certs))
    (home-page "https://nim-lang.org")
    (synopsis "Statically-typed, imperative programming language")
    (description "Nim (formerly known as Nimrod) is a statically-typed,
imperative programming language that tries to give the programmer ultimate power
without compromises on runtime efficiency.  This means it focuses on compile-time
mechanisms in all their various forms.")
    (license license:expat)))
