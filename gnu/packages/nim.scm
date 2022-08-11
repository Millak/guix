;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 José Miguel Sánchez García <jmi2k@openmailbox.org>
;;; Copyright © 2017, 2022 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018, 2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020 Tanguy Le Carrour <tanguy@bioneland.org>
;;; Copyright © 2022 (unmatched parenthesis <paren@disroot.org>
;;; Copyright © 2022 Trevor Richards <trev@trevdev.ca>
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
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (gnu packages bdw-gc)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages parallel)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tls))

(define-public nim
  (package
    (name "nim")
    (version "1.6.6")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://nim-lang.org/download/"
                          name "-" version ".tar.xz"))
      (sha256
       (base32 "0lm4450ig8k4l3rzxv6kcqji5l1lzicsw76ckwxm0q9qdz713cb7"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f          ; TODO: Investigate tests failures.
       #:phases
         (modify-phases %standard-phases
           (delete 'configure)          ; no configure script
           (add-after 'unpack 'patch-installer
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 (substitute* "install.sh"
                  (("/usr/local") out)
                  (("/opt/nimble") (string-append out "/share/nimble"))
                  (("configdir=/etc/nim")
                   (string-append "configdir=" out "/etc/nim"))))))
           (add-after 'unpack 'patch-dynamic-libraries
             (lambda* (#:key inputs native-inputs #:allow-other-keys)
               ;(substitute* "compiler/nodejs.nim"
               ;  (("nodejs")
               ;   (search-input-file (or native-inputs inputs)
               ;                      "/bin/nodejs"))
               ;  (("node")
               ;   (search-input-file (or native-inputs inputs)
               ;                      "/bin/node")))
               (substitute* "lib/system.nim"
                 (("libgc\\.so")
                  (search-input-file (or native-inputs inputs)
                                     "/lib/libgc.so")))
               ;(substitute* "lib/wrappers/mysql.nim"
               ;  (("\(libmysqlclient|libmariadbclient\)\\.so")
               ;   (search-input-file (or native-inputs inputs)
               ;                      "/lib/libmariadbclient.so")))
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
                                     "/lib/libpcre.so")))
               ;(substitute* "lib/wrappers/postgres.nim"
               ;  (("libpg\\.so")
               ;   (search-input-file (or native-inputs inputs)
               ;                      "/lib/libpg.so")))
               (substitute* "lib/wrappers/sqlite3.nim"
                 (("libsqlite3\\.so")
                  (search-input-file (or native-inputs inputs)
                                     "/lib/libsqlite3.so")))))
           (add-after 'patch-source-shebangs 'patch-more-shebangs
             (lambda _
               (let ((sh (which "sh")))
                 (substitute* '("tests/stdlib/tosprocterminate.nim"
                                "tests/stdlib/tstrscans.nim"
                                "lib/pure/osproc.nim"
                                "lib/pure/strscans.nim")
                   (("/bin/sh") sh))
                 (substitute* (find-files "c_code" "stdlib_osproc\\.nim\\.c")
                   (("\"/bin/sh\", 7") (format #f "~s, ~s" sh (string-length sh)))))))
           (replace 'build
             (lambda* (#:key (parallel-build? #t) #:allow-other-keys)
               (setenv "XDG_CACHE_HOME" "./cache-home")
               (setenv "HOME" "./cache-home")
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
    (inputs (list libgc openssl pcre sqlite))
    (native-inputs (list nss-certs parallel))
    (home-page "https://nim-lang.org")
    (synopsis "Statically-typed, imperative programming language")
    (description "Nim (formerly known as Nimrod) is a statically-typed,
imperative programming language that tries to give the programmer ultimate power
without compromises on runtime efficiency.  This means it focuses on compile-time
mechanisms in all their various forms.")
    (license license:expat)))
