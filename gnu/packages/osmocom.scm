;;; GNU Guix
;;; TODO
(define-module (gnu packages osmocom)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
	#:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages embedded)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages samba)
  #:use-module (gnu packages security-token))

(define-public libosmocore
  (let ((commit "69b61fe510dd9357040ad0a9f3a1449d913067a7"))
    (package
      (name "libosmocore")
      (version (git-version "0.10.2" "1" commit))
      (source (origin 
                (method git-fetch)
                (uri (git-reference
                       (url "https://git.osmocom.org/libosmocore")
                       (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                  (base32 "0bb79qr7505p77pz85vj9qsib7c40h7xaq3zph87gmz4wfngv3cd"))))
      (build-system gnu-build-system)
      (native-inputs
        `(("autoconf" ,autoconf)
          ("automake" ,automake)
          ("libtool" ,libtool)
          ("pcsc-lite" ,pcsc-lite)
          ("pkg-config" ,pkg-config)
          ("python2" ,python-2)
          ("talloc" ,talloc)))
      (arguments
        `(#:phases (modify-phases %standard-phases
                     (add-after 'unpack 'autoreconf
                       ;; needs autoreconf
                       (lambda _
                         (let ((sh (which "sh")))
                           (substitute* "git-version-gen" (("/bin/sh") sh))
                           (zero? (system* "autoreconf" "-i"))))))))
      (home-page "https://osmocom.org/projects/libosmocore")
      (synopsis "bla")
      (description "blubb")
      (license license:gpl2))))

(define-public osmocom-bb
  (let ((commit "f02c04f4441ae0d05f46f5fdfd1aa8181ede57a8"))
    (package
      (name "osmocom-bb")
      (version (git-version "0.0.0" "1" commit))
      (source (origin 
                (method git-fetch)
                (uri (git-reference
                       (url "https://git.osmocom.org/osmocom-bb")
                       (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                  (base32 "1zkcd4pa7ahk4jchpg6wsn1hin3k47bkh4rpm93s6zc3x4piy7l1"))))
      (build-system gnu-build-system)
      (native-inputs
        `(("autoconf" ,autoconf)
          ("automake" ,automake)
          ("bash" ,bash)
          ("gcc-arm-none-eabi-4.9" ,gcc-arm-none-eabi-4.9)
          ("libosmocore" ,libosmocore)
          ("libtool" ,libtool)
          ("pkg-config" ,pkg-config)
          ("python2" ,python-2)
          ("talloc" ,talloc)))
      (arguments
        `(#:phases (modify-phases %standard-phases
          (delete 'configure)
          (add-before 'build 'enter-dir
            (lambda _
             (let ((sh (which "sh")))
              (chdir "src")
              (setenv "CONFIG_SHELL" sh)))))))
      (home-page "https://osmocom.org/projects/libosmocore")
      (synopsis "bla")
      (description "blubb")
      (license license:gpl2))))
