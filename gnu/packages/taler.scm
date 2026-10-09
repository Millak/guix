;;; GNU Guix --- GNU Taler components and extension
;;; Copyright © 2026 Maxim Cournoyer <maxim@guixotic.coop>
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

(define-module (gnu packages taler)
  #:use-module (gnu packages c)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages python)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages rust-sources)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system gnu))

(define-public depolymerization
  ;; The last v0.0.1 release does not current build with a recent rust-taler;
  ;; use the latest commit available.
  (let ((commit "db9f7c37411027149abfbb45d071771a9026f059")
        (revision "0"))
    (package
      (name "depolymerization")
      (version (git-version "0.0.1" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "git://git.taler.net/depolymerization.git")
                       (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1i4dprlgl35pwifiix29fcn8gdizv215r24zxa5c5c2f4z5d3z1b"))))
      (build-system cargo-build-system)
      (arguments
       (list
        #:install-source? #f
        #:cargo-package-crates
        '(list "uri-pack"
               "depolymerizer-common"
               "depolymerizer-bitcoin")
        #:cargo-install-paths '(list "depolymerizer-bitcoin")
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'adjust-version-info
              (lambda _
                (call-with-output-file ".version"
                  (lambda (port)
                    ;; Must match the format of `git describe --tags --always
                    ;; --abbrev=8' per the README.md.
                    (format port "v~a~%" #$version)))
                (substitute* "depolymerizer-common/Cargo.toml"
                  (("^version.workspace = true")
                   (format #f "version = ~s" #$version))
                  (("uri-pack = \\{ path")
                   "uri-pack = { version = \"*\", path"))
                (substitute* "Cargo.toml"
                  (("^version =.*")
                   (format #f "version = ~s~%" #$version))
                  (("path = \"depolymerizer-common\"" all)
                   (format #f "~a, version = ~s" all #$version)))))
            (add-after 'unpack 'use-guix-vendored-dependencies
              (lambda _
                (substitute* "Cargo.toml"
                  (("git = \"[[:graph:]]+\"")
                   ;; Cargo is picky when using pre-release version strings;
                   ;; provide the exact one used.
                   (format #f "version = ~s"
                           #$(package-version
                              (this-package-input "rust-taler")))))))
            (delete 'check)             ;moved after install-extra-files
            (add-after 'install 'install-data-files
              (lambda _
                (substitute* "makefile"
                  (("doc/prebuilt/man")
                   (string-append #$(this-package-native-input "taler-docs")
                                  "/share/man"))
                  (("man/depolymerizer-bitcoin[[:graph:]]*" all)
                   (string-append all ".zst")))
                (invoke "make" "install-files"
                        (string-append "prefix=" #$output))))
            (add-after 'install 'add-to-PATH
              (lambda* (#:key outputs #:allow-other-keys)
                ;; Ensure the binary is on PATH so that the
                ;; etc/depolymerizer-bitcoin.conf configuration file can be
                ;; found relative to it, which is needed for the test suite.
                (setenv "PATH"
                        (string-append (dirname
                                        (search-input-file
                                         outputs "bin/depolymerizer-bitcoin"))
                                       ":" (getenv "PATH")))))
            (add-after 'install-data-files 'check
              (assoc-ref %standard-phases 'check))
            (add-before 'check 'start-postgresql
              (lambda* (#:key tests? #:allow-other-keys)
                (when tests?
                  (let ((dbdir (string-append (getcwd) "/../pgdir")))
                    (invoke "initdb" "-D" dbdir)
                    (invoke "pg_ctl" (string-append "--options=-k " dbdir)
                            "-D" dbdir "-l" (string-append dbdir "/db.log")
                            "start")
                    (invoke "createdb" "-h" dbdir
                            "taler_rust_check"))))))))
      (native-inputs (list postgresql taler-docs))
      (inputs (cons* libsecp256k1 mimalloc (cargo-inputs 'depolymerization)))
      (home-page "https://www.taler.net/")
      (synopsis "Blockchain adapters for the Taler ecosystem")
      (description "Depolymerization is a GNU Taler project to create blockchain
adapters (depolymerizer) for the Taler ecosystem.")
      (license license:agpl3+))))

(define-public robocop
  (package
    (name "robocop")
    (version "1.6.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "git://git.taler.net/robocop.git")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "09javwf0izasfa2480wp5x1rq64n9aqacnxrd7d2izd4rchb6hl6"))))
    (build-system cargo-build-system)
    (arguments
     (list
      #:install-source? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-commands
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "robocop-json-postprocess"
                (("exec jq")
                 (string-append "exec " (search-input-file inputs "bin/jq"))))))
          (add-after 'install 'install-doc
            (lambda* (#:key native-inputs inputs #:allow-other-keys)
              (install-file (search-input-file (or native-inputs inputs)
                                               "share/man/robocop.1.zst")
                            (string-append #$output "/share/man"))))
          (add-after 'install 'install-extra-scripts
            (lambda _
              (for-each (lambda (x)
                          (install-file x (string-append #$output "/bin")))
                        (list "robocop-ch-to-json"
                              "robocop-eu-to-json"
                              "robocop-un-to-json"
                              "robocop-ofac-to-json"
                              "robocop-uk-to-json"
                              "robocop-json-postprocess"))))
          (add-after 'install-extra-scripts 'wrap-python-path
            (lambda* (#:key outputs #:allow-other-keys)
              (for-each (lambda (x)
                          (wrap-script (search-input-file
                                        outputs (string-append "bin/" x))
                            `("GUIX_PYTHONPATH" prefix
                              (,(getenv "GUIX_PYTHONPATH")))))
                        ;; All these are Python scripts that depend on the
                        ;; lxml library.
                        (list "robocop-ch-to-json"
                              "robocop-eu-to-json"
                              "robocop-un-to-json"
                              "robocop-ofac-to-json"
                              "robocop-uk-to-json")))))))
    (native-inputs (list taler-docs))
    (inputs (cons* jq
                   guile-3.0            ;for wrap-script
                   python
                   python-lxml
                   (cargo-inputs 'robocop)))
    (home-page "https://www.taler.net/")
    (synopsis "@acronym{CTF, Counter Terrorist Financing} sanction \
processing tool")
    (description "Robocop is a @acronym{CTF, Counter Terrorist Financing}
sanction processing tool.  It can be used for compliance processes in software
such as @url{https://taler.net, GNU Taler}.")
    (license license:gpl3+)))

(define-public taler-docs
  ;; No tag or commit; use the latest commit.
  (let ((commit "7944885e71fbf970d21e0d9f4fe88dcc08146457")
        (revision "1"))
    (package
      (name "taler-docs")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "git://git.taler.net/taler-docs.git")
                       (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1bn9594qh8p88llglgvr71qwl27i8c5sjx0b49wsjfz93d9d0bsa"))
                (modules '((guix build utils)))
                (snippet
                 #~(begin
                     ;; Delete 3rd party bundled Sphinx extensions.
                     (delete-all-but "_exts"
                                     ;; These are custom extensions.
                                     "ebicsdomain.py"
                                     "typescriptdomain.py")
                     (substitute* "conf.py"
                       (("httpdomain.httpdomain")
                        "sphinxcontrib.httpdomain"))))))
      (build-system gnu-build-system)
      (arguments
       (list
        #:tests? #f                     ;no tests
        #:make-flags #~(list "info" "man")
        #:phases
        #~(modify-phases %standard-phases
            (delete 'configure)         ;no configure phase
            (replace 'install
              (lambda _
                ;; Take care to install the man pages before re-running make
                ;; below again; it appears to remove some of the manpages like
                ;; 'depolymerizer-bitcoin.1' otherwise.
                (let ((mandir (string-append #$output "/share/man")))
                  (mkdir-p mandir)
                  (copy-recursively "_build/man" mandir))
                (invoke "make" "-C" "_build/texinfo"
                        "install-info" (string-append "infodir=" #$output
                                                      "/share/info")))))))
      (native-inputs
       (list python-minimal
             python-myst-parser
             python-sphinx
             python-sphinx-design
             python-sphinx-multitoc-numbering
             python-sphinx-plantuml
             python-sphinxcontrib-httpdomain
             python-sphinxcontrib-mermaid
             texinfo))
      (home-page "https://www.taler.net/")
      (synopsis "Documentation of the GNU Taler project")
      (description "This package includes the complete documentation of all
the GNU Taler components, as Texinfo manuals and man pages.")
      ;; The source headers say AGPL 2.1+, which is odd since there isn't an
      ;; AGPL 2.1 version (see: <https://bugs.gnunet.org/view.php?id=11573>).
      (license license:agpl3+))))
