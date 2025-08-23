;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Andy Patterson <ajpatter@uwaterloo.ca>
;;; Copyright © 2017, 2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2019, 2020 Brett Gilio <brettg@gnu.org>
;;; Copyright © 2021, 2022 Foo Chuan Wei <chuanwei.foo@hotmail.com>
;;; Copyright © 2025 Tomás Ortín Fernández <quanrong@mailbox.org>
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

(define-module (gnu packages sml)
  #:use-module (gnu packages lesstif)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages xorg)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public polyml
  (package
    (name "polyml")
    (version "5.9.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/polyml/polyml")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1kvkpighzz6dkfh9gr1franvjfjhr4lcwyb0cmngzvb2nf6g8f6v"))))
    (build-system gnu-build-system)
    (inputs
     (list gmp libffi libx11 libxt motif))
    (arguments
     '(#:configure-flags
       (list "--with-gmp"
             "--with-x")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-config-h
           (lambda _
             ;; courtesy: https://github.com/NixOS/nixpkgs/pull/372200
             (call-with-port (open-file "config.h.in" "a")
               (lambda (out)
                 (display "\n#define _Static_assert static_assert\n" out)))))
         (add-after 'build 'build-compiler
           (lambda* (#:key make-flags parallel-build? #:allow-other-keys)
             (define flags
               (if parallel-build?
                   (cons (format #f "-j~d" (parallel-job-count))
                         make-flags)
                   make-flags))
             (apply system* "make" (append flags (list "compiler"))))))))
    (home-page "https://www.polyml.org/")
    (synopsis "Standard ML implementation")
    (description "Poly/ML is a Standard ML implementation.  It is fully
compatible with the ML97 standard.  It includes a thread library, a foreign
function interface, and a symbolic debugger.")
    ;; Some source files specify 'or any later version'; some don't
    (license
     (list license:lgpl2.1
           license:lgpl2.1+))))

(define (smlnj-file version filename hash)
  (origin
    (method url-fetch)
    (uri (string-append "http://smlnj.cs.uchicago.edu/dist/working/"
                        version "/" filename))
    (sha256 (base32 hash))))

(define-public smlnj
  (package
    (name "smlnj")
    (version "110.99.7.1")
    (source #f)  ; Sources are passed as native-inputs.
    (supported-systems '("x86_64-linux" "i686-linux"))
    (build-system gnu-build-system)
    (outputs '("out" "doc"))
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'unpack
           (lambda* (#:key inputs #:allow-other-keys)
             (for-each
               (lambda (file)
                 (invoke "tar" "xvf" (assoc-ref inputs file)))
               (list (if (string=? "i686-linux" ,(%current-system))
                       "boot.x86-unix"
                       "boot.amd64-unix")
                     "config"
                     "cm"
                     "compiler"
                     "runtime"
                     "system"
                     "MLRISC"
                     "smlnj-lib"
                     "old-basis"
                     "ckit"
                     "nlffi"
                     "cml"
                     "eXene"
                     "ml-lpt"
                     "ml-lex"
                     "ml-yacc"
                     "ml-burg"
                     "pgraph"
                     "trace-debug-profile"
                     "heap2asm"
                     "smlnj-c"
                     "doc"
                     "asdl"))
             ;; Same directory structure as what the config/unpack script
             ;; would produce.
             (mkdir "base")
             (rename-file "runtime" "base/runtime")
             (rename-file "compiler" "base/compiler")
             (rename-file "cm" "base/cm")
             (rename-file "old-basis" "base/old-basis")
             (rename-file "system" "base/system")
             #t))
         (delete 'configure)
         (replace 'patch-source-shebangs
           (lambda _
             ;; Fix paths to /bin/sh.
             (substitute* (list "config/install.sh"
                                (if (string=? "i686-linux" ,(%current-system))
                                  "base/runtime/objs/mk.x86-linux"
                                  "base/runtime/objs/mk.amd64-linux")
                                "asdl/configure"
                                "asdl/src/asdlgen/Makefile.in")
               (("^SHELL[[:space:]]*=[[:space:]]*/bin/sh")
                (string-append "SHELL=" (which "sh"))))
             (substitute* "asdl/configure"
               (("^SHELL=\\$\\{CONFIG_SHELL-/bin/sh\\}")
                (string-append "SHELL=" (which "sh"))))
             (substitute* (list "asdl/src/gen/fragments/mkfrags_sh.in"
                                "asdl/src/gen/fragments/mkmk_sh.in")
               (("^#!/bin/sh")
                (string-append "#!" (which "sh"))))
             #t))
         (replace 'build
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (substitute* "config/chk-global-names.sh"
               (("^CC=.*")
                (string-append "CC=" ,(cc-for-target))))

             ;; /bin and /usr/bin do not exist in the build environment.
             (substitute* "config/_arch-n-opsys"
               (("^export PATH") "")
               (("^PATH=\"/bin:/usr/bin\"") "")
               (("uname") (which "uname")))
             (substitute* "base/runtime/config/gen-posix-names.sh"
               (("^PATH=/bin:/usr/bin") ""))

             ;; The build process uses an SML Basis Library function
             ;; `OS.Process.system`, which uses "/bin/sh" (this is hardcoded).
             ;; However, /bin/sh does not exist in the Guix build environment.
             ;; Solution: binary patch — replace "/bin/sh" with "/tmp/sh".
             (symlink (which "sh") "/tmp/sh")
             (invoke "sed" "-i" "s,/bin/sh,/tmp/sh,"
                     (if (string=? "i686-linux" ,(%current-system))
                       "sml.boot.x86-unix/SMLNJ-BASIS/.cm/x86-unix/basis-common.cm"
                       "sml.boot.amd64-unix/SMLNJ-BASIS/.cm/amd64-unix/basis-common.cm"))

             ;; Build.
             ;; The `sml` executable built by this package somehow inherits the
             ;; signal dispositions of the shell where it was built. If SIGINT
             ;; is ignored in the shell, the resulting `sml` will also ignore
             ;; SIGINT. This will break the use of Ctrl-c for interrupting
             ;; execution in the SML/NJ REPL.
             ;; Here, we use Guile's `system` procedure instead of Guix's
             ;; `invoke` because `invoke` uses Guile's `system*`, which causes
             ;; SIGINT and SIGQUIT to be ignored.
             (let ((exit-code
                     (system (string-append "./config/install.sh -default "
                                            (if (string=? "i686-linux"
                                                          ,(%current-system))
                                              "32"
                                              "64")))))
               (unless (zero? exit-code)
                 (error (format #f "Exit code: ~a" exit-code))))

             ;; Undo the binary patch.
             (for-each
               (lambda (file)
                 (invoke "sed" "-i" "s,/tmp/sh,/bin/sh," file))
               (if (string=? "i686-linux" ,(%current-system))
                 '("bin/.heap/sml.x86-linux"
                   "lib/SMLNJ-BASIS/.cm/x86-unix/basis-common.cm")
                 '("bin/.heap/sml.amd64-linux"
                   "lib/SMLNJ-BASIS/.cm/amd64-unix/basis-common.cm")))

             ;; Set SMLNJ_HOME in the bin/ files, so that `sml` is able to find
             ;; the SML/NJ Library.
             (let ((out (assoc-ref outputs "out")))
               (for-each
                 (lambda (file)
                   (invoke "sed" "-i"
                           (string-append "2iSMLNJ_HOME=${SMLNJ_HOME:-" out "}")
                           file))
                 '("bin/.link-sml"
                   "bin/.run-sml"
                   "bin/ml-build"
                   "bin/ml-makedepend")))))
        (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (out-bin (string-append out "/bin/"))
                    (out-lib (string-append out "/lib/"))
                    (out-man (string-append out "/share/man/")))
               (copy-recursively "bin" out-bin)
               (copy-recursively "lib" out-lib)
               (copy-recursively "doc/man" out-man))
             #t))
        (add-after 'install 'install-doc
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((doc (string-append (assoc-ref outputs "doc")
                                       "/share/doc/smlnj")))
               (mkdir-p doc)
               (copy-recursively "doc" doc))
             #t)))))
    (native-inputs
     `(,(if (string=? "i686-linux" (%current-system))
          `("boot.x86-unix"
            ,(smlnj-file version
                         "boot.x86-unix.tgz"
                         "0nl71scc4q1jlwikfph9r01fs5nhqaiikbxq732y0199mwwlqhjm"))
          `("boot.amd64-unix"
            ,(smlnj-file version
                         "boot.amd64-unix.tgz"
                         "0m6p8zlcxd11c0zswma4bbh0wxi9f8926b21ddqhypnkrbq061xa")))
       ("config"
        ,(smlnj-file version
                     "config.tgz"
                     "108j8hk6zk9nf25hw53b3ig64gysj518v5997b0c06syjfnnpc3y"))
       ("cm"
        ,(smlnj-file version
                     "cm.tgz"
                     "0sh69rswnaarycm2i5v9piv9725ld62pxs4q4q4xw7nggmqfw4pq"))
       ("compiler"
        ,(smlnj-file version
                     "compiler.tgz"
                     "0qr1wghnld085bf55ygd0iwdqsyi39x7wjl2m36d8saymagc1lzm"))
       ("runtime"
        ,(smlnj-file version
                     "runtime.tgz"
                     "11f05pd5rgkn4k3apnb9zbimkqg6z2plmd2550zph3n8sq883fax"))
       ("system"
        ,(smlnj-file version
                     "system.tgz"
                     "0aj6vrbmpziqf50x7gggzkvijhny9xrwqmjjrqxq2rkywy41k1i2"))
       ("MLRISC"
        ,(smlnj-file version
                     "MLRISC.tgz"
                     "0svwsnpzcb1hiwfsjmh53wa2qsry3ycpk907jj1ggdz52vwj1p96"))
       ("smlnj-lib"
        ,(smlnj-file version
                     "smlnj-lib.tgz"
                     "04vhn7rvji05x4ms887xzwrm9bkd3164k9hhc885a6pdlrp7632f"))
       ("old-basis"
        ,(smlnj-file version
                     "old-basis.tgz"
                     "04ihh23vgrlqq4smxnnnh4k1xclqfmzz9430hc9d0w87ccy13hbm"))
       ("ckit"
        ,(smlnj-file version
                     "ckit.tgz"
                     "1ym96jg6a4x9nvbd2jsn6dy47nvdi92h5nlbdkxyzqf58r772ab7"))
       ("nlffi"
        ,(smlnj-file version
                     "nlffi.tgz"
                     "1q60lnhggxyhzdh7d9119mg5avfz1ax89lciwpd20alrjwjvkwv7"))
       ("cml"
        ,(smlnj-file version
                     "cml.tgz"
                     "1z5qc52i5k4a7144fmska0kawibrr5xsbvb0ppl98sqyzj34wa3r"))
       ("eXene"
        ,(smlnj-file version
                     "eXene.tgz"
                     "05fwzpks5whwgvfa9wd70rwrym1r6z8i79vbvipnncqz794nywr5"))
       ("ml-lpt"
        ,(smlnj-file version
                     "ml-lpt.tgz"
                     "1dlwl3b1kz1968yj2razsqvyv6pmz3ripg84rv8nvlj02sknw8p7"))
       ("ml-lex"
        ,(smlnj-file version
                     "ml-lex.tgz"
                     "1jjbs7zvzvajiz8zb1bz9k2i1w3przhk6q76964hg4y2zghi74kq"))
       ("ml-yacc"
        ,(smlnj-file version
                     "ml-yacc.tgz"
                     "0s53cgjs8zk16qywb18l08ycanfzycrl3bbm0177a1y0y2y1q5zx"))
       ("ml-burg"
        ,(smlnj-file version
                     "ml-burg.tgz"
                     "085i565maijp5mmyrcg7cg3ff75iw6c2wmjldgq1psbsjlymv7ni"))
       ("pgraph"
        ,(smlnj-file version
                     "pgraph.tgz"
                     "1vlb6hlicxf806z216x33qmx0mi3f8kgddp6jwqxxwbxlzpwa2h6"))
       ("trace-debug-profile"
        ,(smlnj-file version
                     "trace-debug-profile.tgz"
                     "08p9j4n3x2xnfksl8xkfr6ajynpfbcgsrcv5w98ri0akvny9dc6p"))
       ("heap2asm"
        ,(smlnj-file version
                     "heap2asm.tgz"
                     "0qzqnagpyfipgzkds8jxix9riaps591dnwa4nw7wb3b2pm7xpqm8"))
       ("smlnj-c"
        ,(smlnj-file version
                     "smlnj-c.tgz"
                     "194vzb5ig373np2lnldsq66pdwzf3wsjrmhhc1bpyfw6h5z1g82p"))
       ("doc"
        ,(smlnj-file version
                     "doc.tgz"
                     "0b9z1dhicg14dmb04zrla6jrmcv2pb08i5bvgw9pns0gvqzc5iyb"))
       ("asdl"
        ,(smlnj-file version
                     "asdl.tgz"
                     "1dzhc56m23gylj3jwgxg7qlbx4sh8wklraj1j4mic6dn4jqyqcvh"))))
    (home-page "https://www.smlnj.org")
    (synopsis "Standard ML of New Jersey interactive compiler")
    (description
      "SML/NJ is an implementation of the Standard ML programming language.
Standard ML has many features, including type safety, polymorphism, algebraic
data types with pattern matching, higher-order functions, and a sophisticated
module system.  It is especially well-suited for writing compilers and other
language processors.")
    (license (license:fsf-free
               "https://www.smlnj.org/license.html"
               "https://www.gnu.org/licenses/license-list#StandardMLofNJ"))))
