;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Pierre-Antoine Rouby <pierre-antoine.rouby@inria.fr>
;;; Copyright © 2020 Martin Becze <mjbecze@riseup.net>
;;; Copyright © 2020 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2021 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2021 Sarah Morgensen <iskarian@mgsn.dev>
;;; Copyright © 2022 Giacomo Leidi <goodoldpaul@autistici.org>
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

(define-module (gnu packages golang-web)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix build-system go)
  #:use-module (guix git-download)
  #:use-module (gnu packages)
  #:use-module (gnu packages golang))

;;; Commentary:
;;;
;;; Golang modules (libraries) related to HTML, CSS, SCSS, JavaScript, JSON,
;;; Web-framework, REST-API or similar functionality.
;;;
;;; Please: Try to add new module packages in alphabetic order.
;;;
;;; Code:

(define-public go-github-com-andybalholm-cascadia
  (package
    (name "go-github-com-andybalholm-cascadia")
    (version "1.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/andybalholm/cascadia")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0zgc9fjkn7d66cnmgnmalr9lrq4ii1spap95pf2x1hln4pflib5s"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/andybalholm/cascadia"))
    (native-inputs
     (list go-golang-org-x-net))
    (home-page "https://github.com/andybalholm/cascadia/")
    (synopsis "CSS selectors for HTML")
    (description "The Cascadia package implements CSS selectors for use with
the parse trees produced by the html package.")
    (license license:bsd-2)))

(define-public go-github-com-aymerick-douceur
  (package
    (name "go-github-com-aymerick-douceur")
    (version "0.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/aymerick/douceur/")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1hfysznib0fqbp8vqxpk0xiggpp0ayk2bsddi36vbg6f8zq5f81n"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/aymerick/douceur"))
    (native-inputs
     (list go-github-com-andybalholm-cascadia
           go-github-com-gorilla-css
           go-github-com-puerkitobio-goquery
           go-golang-org-x-net))
    (home-page "https://github.com/aymerick/douceur/")
    (synopsis "CSS parser and inliner")
    (description "This package provides a CSS parser and inliner.")
    (license license:expat)))

(define-public go-github-com-gorilla-css
  (package
    (name "go-github-com-gorilla-css")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gorilla/css")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "116fhy3n7bsq3psyn4pa0i4x9zy916kh1zxslmbbp0p9l4i7ysrj"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/gorilla/css/scanner"
       #:unpack-path "github.com/gorilla/css"))
    (home-page "https://github.com/gorilla/css/")
    (synopsis "CSS3 tokenizer")
    (description "This package provides a CSS3 tokenizer.")
    (license license:bsd-3)))

(define-public go-github-com-gorilla-mux
  (package
    (name "go-github-com-gorilla-mux")
    (version "1.8.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gorilla/mux")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "18f0q9qxgq1yh4ji07mqhiydfcwvi56z9d775v7dc7yckj33kpdk"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/gorilla/mux"))
    (home-page "https://github.com/gorilla/mux")
    (synopsis "URL router and dispatcher for Go")
    (description
     "Gorilla/Mux implements a request router and dispatcher for matching
incoming requests with their respective handler.")
    (license license:bsd-3)))

(define-public go-github-com-puerkitobio-goquery
  (package
    (name "go-github-com-puerkitobio-goquery")
    (version "1.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/PuerkitoBio/goquery")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0gh1d99l5xc9hvwa4j40pfq3y9vfyq52mnrz6bf1kw2r2zr2gbcc"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/PuerkitoBio/goquery"))
    (propagated-inputs
     (list go-github-com-andybalholm-cascadia go-golang-org-x-net))
    (home-page "https://github.com/PuerkitoBio/goquery")
    (synopsis "Features similar to jQuery to the Go language")
    (description "@code{goquery} brings a syntax and a set of features similar
to jQuery to the Go language.")
    (license license:bsd-3)))

;;;
;;; Avoid adding new packages to the end of this file. To reduce the chances
;;; of a merge conflict, place them above by existing packages with similar
;;; functionality or similar names.
;;;
