;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 José Miguel Sánchez García <jmi2k@openmailbox.org>
;;; Copyright © 2017, 2022 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018, 2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020 Tanguy Le Carrour <tanguy@bioneland.org>
;;; Copyright © 2022 (unmatched parenthesis <paren@disroot.org>
;;; Copyright © 2022 Trevor Richards <trev@trevdev.ca>
;;; Copyright © 2023 Gruruya <greytest@disroot.org>
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

(define-module (gnu packages nim)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (gnu packages bdw-gc)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages parallel)
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
      (commit "5faec3e9a33afe99a7d22377dd1b45a5391f5504")))
    (sha256 (base32 "1yd3pcg46blkxfkpcvymfln44wgryq9v1mcxblqjhgfi0rvdjm0v"))))

(define sat
  (origin
    (method git-fetch)
    (file-name "sat-checkout")
    (uri
     (git-reference
      (url "https://github.com/nim-lang/sat.git")
      ;; referenced in koch.nim
      (commit "faf1617f44d7632ee9601ebc13887644925dcc01")))
    (sha256 (base32 "1dxbc41wbvkpdp6q3qz1r38lpn32447qkkgyh2s12ym6bx4ynni4"))))

(define-public nim
  (package
    (name "nim")
    (version "2.2.2")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://nim-lang.org/download/"
                          name "-" version ".tar.xz"))
      (sha256
       (base32 "07wazlsj3yby4vb008b2rairdkl0hhnnxhpxi6jaa2wwmj3rpk3z"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f          ; TODO: Investigate tests failures.
       #:modules
       ((ice-9 rdelim)
        (ice-9 regex)
        (ice-9 match)
        ,@%default-gnu-modules)
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

                 (define rx1 (make-regexp "^(.*NIM_CHAR data\\[)7(\\+1\\];.*)$" regexp/extended))
                 ;; } TM__HZdw8BhppcTQo8DIK46LSg_5 = { 7 | NIM_STRLIT_FLAG, "/bin/sh" };
                 (define rx2 (make-regexp
                               (string-append "^(\\} )"
                                               "([^[:space:]]+)"
                                               "( = \\{ )"
                                               "7"
                                               "( [|] NIM_STRLIT_FLAG, )"
                                               "\"/bin/sh\""
                                               "(.*)$")
                               regexp/extended))

                 (define (fixup-1 matches out)
                   (format out "~a~a~a\n"
                           ;;   NI cap; NIM_CHAR data[(string-length (which "sh"))+1];
                           (match:substring matches 1)  ;; NI cap; NIM_CHAR data[
                           sh-len                       ;; (string-length (which "sh")
                           (match:substring matches 2)) ;; +1];
                   #f)

                 (define (fixup-2 matches out)
                   (format out "~a~a~a~a~a~s~a\n"
                           ;; } TM__HZdw8BhppcTQo8DIK46LSg_5 = { (string-length (which "sh")) | NIM_STRLIT_FLAG, (which "sh") };
                           (match:substring matches 1)  ;; }
                           (match:substring matches 2)  ;; TM__HZdw8BhppcTQo8DIK46LSg_5
                           (match:substring matches 3)  ;; = {
                           sh-len                       ;; (string-length (which "sh"))
                           (match:substring matches 4)  ;; | NIM_STRLIT_FLAG,
                           sh                           ;; (which "sh")
                           (match:substring matches 5)) ;; };
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
                     (let* ((pat (and (string? flagged)
                                      (format #f "{7, (NimStrPayload*)&~a};" flagged)))
                            (pat-len (if (string? pat) (string-length pat) 0)))
                       (unless (eof-object? line)
                         (if (and (string? pat) (string-suffix? pat line))
                             (begin
                               (format out
                                       "~a{~a, (NimStrPayload*)&~a};\n"
                                       (substring line 0
                                                  (- (string-length line) pat-len))
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
                           (find-files "c_code" "@m\\.\\.@slib@spure@sosproc\\.nim\\.c"))))
             (replace 'build
               (lambda* (#:key (parallel-build? #t) #:allow-other-keys)
                 (setenv "XDG_CACHE_HOME" "./cache-home")
                 (setenv "HOME" "./cache-home")
                 (setenv "SHELL" (which "sh"))
                 (mkdir-p "./cache-home")
                 (invoke "sh" "build.sh"
                         "--parallel"
                         (if parallel-build?
                           (number->string (parallel-job-count))
                           "1"))
                 (sleep 5)        ; Wait for the parallel builds to finish.
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
    (native-inputs (list nss-certs parallel))
    (home-page "https://nim-lang.org")
    (synopsis "Statically-typed, imperative programming language")
    (description "Nim (formerly known as Nimrod) is a statically-typed,
imperative programming language that tries to give the programmer ultimate power
without compromises on runtime efficiency.  This means it focuses on compile-time
mechanisms in all their various forms.")
    (license license:expat)))
