;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2022 pukkamustard <pukkamustard@posteo.net>
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

(define-module (gnu packages pikchr)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:))

(define-public pikchr
  (let ((commit "221988914eff6efe")
        (revision "0"))                       ;increment for each new snapshot
    (package
      (name "pikchr")
      ;; To update, use the last check-in in https://pikchr.org/home/timeline?r=trunk
      (version (string-append revision "-" (string-take commit 5)))
      (source (origin
                (method url-fetch)
                (uri (string-append "https://pikchr.org/home/tarball/" commit
                                    "/pikchr.tar.gz"))
                (file-name (string-append "pikchr-" version ".tar.gz"))
                (sha256
                 (base32
                  "0yclkincsgfni4scjzp5avdsij8vmyxjn0q2qkwjhn3p43y8nxzd"))))
      (build-system gnu-build-system)
      (arguments
       '(#:tests? #f                              ;no tests
         #:phases
         (modify-phases %standard-phases
           (delete 'bootstrap)
           (delete 'configure)
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (bin (string-append out "/bin")))
                 (install-file "pikchr" bin)))))))
      (home-page "https://pikchr.org")
      (synopsis "Markup language for diagrams in technical documentation")
      (description
       "Pikchr (pronounced @emph{picture}) is a PIC-like markup language for
diagrams in technical documentation.  Pikchr is designed to be embedded in
fenced code blocks of Markdown or similar mechanisms of other documentation
markup languages.")
      (license license:bsd-0))))
