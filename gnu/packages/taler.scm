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
  #:use-module (gnu packages python)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages texinfo)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu))

(define-public taler-docs
  ;; No tag or commit; use the latest commit.
  (let ((commit "1557e7a3547cfa04b111c40ce0feef9581121635")
        (revision "0"))
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
                  "1lhcvqlazsqbj391d7dlp6rzblld4jij8z2vym2cbgz4mzb2vqj1"))
                (modules '((guix build utils)
                           (ice-9 ftw)
                           (srfi srfi-26)))
                (snippet
                 #~(begin
                     ;; XXX: 'delete-all-but' is copied from the turbovnc package.
                     (define (delete-all-but directory . preserve)
                       (with-directory-excursion directory
                         (let* ((pred (negate (cut member <>
                                                   (cons* "." ".." preserve))))
                                (items (scandir "." pred)))
                           (for-each (cut delete-file-recursively <>) items))))
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
                (invoke "make" "-C" "_build/texinfo"
                        "install-info" (string-append "infodir=" #$output
                                                      "/share/info"))
                (let ((mandir (string-append #$output "/share/man")))
                  (mkdir-p mandir)
                  (copy-recursively "_build/man" mandir)))))))
      (native-inputs
       (list python-minimal
             python-myst-parser
             python-sphinx
             python-sphinx-design
             python-sphinx-multitoc-numbering
             python-sphinx-plantuml
             python-sphinxcontrib-httpdomain
             texinfo))
      (home-page "https://www.taler.net/")
      (synopsis "Documentation of the GNU Taler project")
      (description "This package includes the complete documentation of all
the GNU Taler components, as Texinfo manuals and man pages.")
      ;; The source headers say AGPL 2.1+, which is odd since there isn't an
      ;; AGPL 2.1 version (see: <https://bugs.gnunet.org/view.php?id=11573>).
      (license license:agpl3+))))
