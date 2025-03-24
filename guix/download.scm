;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012-2021, 2024-2025 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013, 2014, 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2015 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2016 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2016 David Craven <david@craven.ch>
;;; Copyright © 2016, 2017, 2020, 2022 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Guy Fleury Iteriteka <hoonandon@gmail.com>
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

(define-module (guix download)
  #:use-module (ice-9 match)
  #:use-module (guix derivations)
  #:use-module (guix packages)
  #:use-module (guix store)
  #:autoload   (guix build download) (url-fetch)
  #:use-module (guix monads)
  #:use-module (guix gexp)
  #:autoload   (guix build utils) (call-with-temporary-output-file)
  #:autoload   (web uri) (string->uri uri-scheme uri-path)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (%download-methods
            %mirrors
            %disarchive-mirrors
            (url-fetch* . url-fetch)
            url-fetch/executable
            url-fetch/tarbomb
            url-fetch/zipbomb
            download-to-store))

;;; Commentary:
;;;
;;; Produce fixed-output derivations with data fetched over HTTP or FTP.
;;;
;;; Code:

(define %mirrors
  ;; Mirror lists used when `mirror://' URLs are passed.  The first mirror
  ;; entry of each set should ideally be the most authoritative one, as that's
  ;; what the generic HTML updater will pick to look for updates, with
  ;; possible exceptions when the authoritative mirror is too slow.
  (let* ((gnu-mirrors
          '(;; This one redirects to a (supposedly) nearby and (supposedly)
            ;; up-to-date mirror.
            "https://ftpmirror.gnu.org/gnu/"

            "ftp://ftp.cs.tu-berlin.de/pub/gnu/"
            "ftp://ftp.funet.fi/pub/mirrors/ftp.gnu.org/gnu/"

            ;; This one is the master repository, and thus it's always
            ;; up-to-date.
            "http://ftp.gnu.org/pub/gnu/")))
    `((gnu ,@gnu-mirrors)
      (gcc
       "ftp://ftp.nluug.nl/mirror/languages/gcc/"
       "ftp://ftp.fu-berlin.de/unix/languages/gcc/"
       "ftp://ftp.irisa.fr/pub/mirrors/gcc.gnu.org/gcc/"
       "ftp://gcc.gnu.org/pub/gcc/"
       ,@(map (cut string-append <> "/gcc") gnu-mirrors))
      (gnupg
       "https://gnupg.org/ftp/gcrypt/"
       "ftp://mirrors.dotsrc.org/gcrypt/"
       "ftp://ftp.heanet.ie/mirrors/ftp.gnupg.org/gcrypt/"
       "ftp://ftp.mirrorservice.org/sites/ftp.gnupg.org/gcrypt/"
       "ftp://ftp.ring.gr.jp/pub/net/gnupg/")
      (gnome
       "https://download.gnome.org/"
       "http://ftp.gnome.org/pub/GNOME/")
      (hackage
       "http://hackage.haskell.org/")
      (savannah           ; http://download0.savannah.gnu.org/mirmon/savannah/
       "https://download.savannah.gnu.org/releases/"
       "https://ftp.cc.uoc.gr/mirrors/nongnu.org/"
       "http://ftp.twaren.net/Unix/NonGNU/" ; https appears unsupported
       "https://mirror.csclub.uwaterloo.ca/nongnu/"
       "https://nongnu.askapache.com/"
       "https://savannah.c3sl.ufpr.br/"
       "https://download-mirror.savannah.gnu.org/releases/"
       "ftp://ftp.twaren.net/Unix/NonGNU/"
       "ftp://mirror.csclub.uwaterloo.ca/nongnu/"
       "ftp://mirror.publicns.net/pub/nongnu/"
       "ftp://savannah.c3sl.ufpr.br/")
      (sourceforge ; https://sourceforge.net/p/forge/documentation/Mirrors/
       "http://downloads.sourceforge.net/project/"
       "http://ufpr.dl.sourceforge.net/project/"
       "http://freefr.dl.sourceforge.net/project/"
       "http://internode.dl.sourceforge.net/project/"
       "http://jaist.dl.sourceforge.net/project/"
       "http://liquidtelecom.dl.sourceforge.net/project/"
       "http://nchc.dl.sourceforge.net/project/"
       "http://netcologne.dl.sourceforge.net/project/"
       "http://netix.dl.sourceforge.net/project/"
       "http://tenet.dl.sourceforge.net/project/")
      (netfilter.org ; https://www.netfilter.org/mirrors.html
       "http://ftp.netfilter.org/pub/"
       "ftp://ftp.es.netfilter.org/mirrors/netfilter/"
       "ftp://ftp.hu.netfilter.org/"
       "ftp://www.lt.netfilter.org/pub/")
      (kernel.org
       "https://cdn.kernel.org/pub/"
       "http://ftp.be.debian.org/pub/"
       "https://mirrors.edge.kernel.org/pub/"
       "ftp://ftp.funet.fi/pub/mirrors/ftp.kernel.org/pub/")
      (apache
       "https://dlcdn.apache.org/"
       "https://downloads.apache.org/"
       "https://mirrors.sonic.net/apache/"
       "https://apache.osuosl.org/"
       "https://mirrors.ircam.fr/pub/apache/"
       "https://apache-mirror.rbc.ru/pub/apache/"
       "https://mirrors.ibiblio.org/apache/"

       ;; No HTTPS.
       "http://apache.mirror.iweb.ca/"
       "http://apache.mirrors.ovh.net/ftp.apache.org/dist/"

       ;; As a last resort, try the archive.
       "https://archive.apache.org/dist/")
      (xorg               ; from http://www.x.org/wiki/Releases/Download
       "http://www.x.org/releases/" ; main mirrors
       "http://mirror.csclub.uwaterloo.ca/x.org/" ; North America
       "http://xorg.mirrors.pair.com/"
       "ftp://mirror.csclub.uwaterloo.ca/x.org/"
       "ftp://xorg.mirrors.pair.com/"
       "ftp://artfiles.org/x.org/" ; Europe
       "ftp://ftp.chg.ru/pub/X11/x.org/"
       "ftp://ftp.fu-berlin.de/unix/X11/FTP.X.ORG/"
       "ftp://ftp.gwdg.de/pub/x11/x.org/"
       "ftp://ftp.mirrorservice.org/sites/ftp.x.org/"
       "ftp://ftp.ntua.gr/pub/X11/"
       "ftp://ftp.piotrkosoft.net/pub/mirrors/ftp.x.org/"
       "ftp://ftp.portal-to-web.de/pub/mirrors/x.org/"
       "ftp://ftp.solnet.ch/mirror/x.org/"
       "ftp://mi.mirror.garr.it/mirrors/x.org/"
       "ftp://mirror.cict.fr/x.org/"
       "ftp://mirror.switch.ch/mirror/X11/"
       "ftp://mirrors.ircam.fr/pub/x.org/"
       "ftp://x.mirrors.skynet.be/pub/ftp.x.org/"
       "ftp://ftp.cs.cuhk.edu.hk/pub/X11" ; East Asia
       "ftp://ftp.u-aizu.ac.jp/pub/x11/x.org/"
       "ftp://ftp.yz.yamagata-u.ac.jp/pub/X11/x.org/"
       "ftp://ftp.kaist.ac.kr/x.org/"
       "ftp://mirrors.go-part.com/xorg/"
       "ftp://ftp.is.co.za/pub/x.org")            ; South Africa
      (cpan
       "http://www.cpan.org/"
       "http://cpan.metacpan.org/"
       ;; A selection of HTTP mirrors from http://www.cpan.org/SITES.html.
       ;; Europe.
       "http://ftp.belnet.be/mirror/ftp.cpan.org/"
       "http://mirrors.nic.cz/CPAN/"
       "http://mirror.ibcp.fr/pub/CPAN/"
       "http://ftp.ntua.gr/pub/lang/perl/"
       "http://mirror.as43289.net/pub/CPAN/"
       "http://cpan.cs.uu.nl/"
       "http://cpan.uib.no/"
       "http://cpan-mirror.rbc.ru/pub/CPAN/"
       "http://mirror.sbb.rs/CPAN/"
       "http://cpan.lnx.sk/"
       "http://ftp.rediris.es/mirror/CPAN/"
       "http://mirror.ox.ac.uk/sites/www.cpan.org/"
       ;; Africa.
       "http://mirror.liquidtelecom.com/CPAN/"
       "http://cpan.mirror.ac.za/"
       "http://mirror.is.co.za/pub/cpan/"
       "http://cpan.saix.net/"
       "http://mirror.ucu.ac.ug/cpan/"
       ;; North America.
       "http://mirrors.gossamer-threads.com/CPAN/"
       "http://mirror.csclub.uwaterloo.ca/CPAN/"
       "http://mirrors.ucr.ac.cr/CPAN/"
       "http://www.msg.com.mx/CPAN/"
       "http://mirrors.namecheap.com/CPAN/"
       "http://mirror.uic.edu/CPAN/"
       "http://mirror.datapipe.net/CPAN/"
       "http://mirror.cc.columbia.edu/pub/software/cpan/"
       "http://mirror.uta.edu/CPAN/"
       ;; South America.
       "http://cpan.mmgdesigns.com.ar/"
       "http://mirror.nbtelecom.com.br/CPAN/"
       "http://linorg.usp.br/CPAN/"
       "http://cpan.dcc.uchile.cl/"
       "http://mirror.cedia.org.ec/CPAN/"
       ;; Oceania.
       "http://cpan.mirror.serversaustralia.com.au/"
       "http://mirror.waia.asn.au/pub/cpan/"
       "http://mirror.as24220.net/pub/cpan/"
       "http://cpan.lagoon.nc/pub/CPAN/"
       "http://cpan.inspire.net.nz/"
       ;; Asia.
       "http://mirror.dhakacom.com/CPAN/"
       "http://mirrors.ustc.edu.cn/CPAN/"
       "http://ftp.cuhk.edu.hk/pub/packages/perl/CPAN/"
       "http://kambing.ui.ac.id/cpan/"
       "http://cpan.hostiran.ir/"
       "http://ftp.nara.wide.ad.jp/pub/CPAN/"
       "http://mirror.neolabs.kz/CPAN/"
       "http://cpan.nctu.edu.tw/"
       "http://cpan.ulak.net.tr/"
       "http://mirrors.vinahost.vn/CPAN/")
      (cran
       ;; Arbitrary mirrors from http://cran.r-project.org/mirrors.html
       ;; This one automatically redirects to servers worldwide
       "http://cran.r-project.org/"
       "http://cran.rstudio.com/"
       "http://cran.univ-lyon1.fr/"
       "http://cran.ism.ac.jp/"
       "http://cran.stat.auckland.ac.nz/"
       "http://cran.mirror.ac.za/"
       "http://cran.csie.ntu.edu.tw/")
      (ctan
       ;; This is the CTAN mirror multiplexor service, which automatically
       ;; redirect to a mirror in or close to the country of the requester
       ;; (see: https://ctan.org/mirrors/).
       "https://mirror.ctan.org/")
      (imagemagick
       ;; from http://www.imagemagick.org/script/download.php
       ;; (without mirrors that are unavailable or not up to date)
       "https://sunsite.icm.edu.pl/packages/ImageMagick/releases"
       "http://mirror.checkdomain.de/imagemagick/releases"
       "ftp://ftp.u-aizu.ac.jp/pub/graphics/image/ImageMagick/imagemagick.org/releases"
       "ftp://ftp.nluug.nl/pub/ImageMagick/"
       "http://www.imagemagick.org/download/releases/"
       ;; Try this if all else fails (normally contains just the latest version).
       "http://www.imagemagick.org/download/")
      (debian
       "http://ftp.de.debian.org/debian/"
       "http://ftp.fr.debian.org/debian/"
       "http://ftp.debian.org/debian/"
       "http://archive.debian.org/debian/")
      (kde
       "https://download.kde.org/"
       "https://download.kde.org/Attic/"    ; for when it gets archived.
       ;; I could not find the classic static mirror list anymore.  Instead,
       ;; add ‘.mirrorlist’ to the end of a recent download.kde.org tarball URL.
       ;; Europe
       "https://mirrors.xtom.de/kde/"
       "https://mirror.lyrahosting.com/pub/kde/"
       "https://mirrors.xtom.nl/kde/"
       "https://mirror.hs-esslingen.de/Mirrors/ftp.kde.org/pub/kde/"
       "https://mirror.kumi.systems/kde/ftp/"
       "https://mirrors.ircam.fr/pub/KDE/"
       "https://ftp.gwdg.de/pub/linux/kde/"
       "https://fr2.rpmfind.net/linux/KDE/"
       "https://mirror.faigner.de/kde/ftp/"
       "https://www.mirrorservice.org/sites/download.kde.org/"
       "https://mirrors.ukfast.co.uk/sites/kde.org/ftp/"
       "https://mirrors.dotsrc.org/kde/"
       "http://kde.mirror.anlx.net/"
       "https://mirror.karneval.cz/pub/kde/"
       "https://ftp.fi.muni.cz/pub/kde/"
       "https://www-ftp.lip6.fr/pub/X11/kde/"
       "https://ftp.icm.edu.pl/pub/unix/kde/"
       "https://kde.mirror.garr.it/kde/ftp/"
       "https://ftp.acc.umu.se/mirror/kde.org/ftp/"
       "https://mirrors.up.pt/pub/kde/"
       "https://mirrors.nav.ro/kde/"
       "https://mirrors.xtom.ee/kde/"
       "https://ftp.funet.fi/pub/mirrors/ftp.kde.org/pub/kde/"
       "https://mirrors.netix.net/kde/"
       "https://ftp.cc.uoc.gr/mirrors/kde/"
       ;; North America
       "https://mirror.its.dal.ca/kde/"
       "https://nnenix.mm.fcix.net/kdeftp/"
       "https://mirrors.mit.edu/kde/"
       "https://mirror.csclub.uwaterloo.ca/kde/"
       "https://mirror.fcix.net/kdeftp/"
       "https://mirrors.ocf.berkeley.edu/kde/"
       "https://mirrors.xtom.com/kde/"
       ;; South America
       "https://kde.c3sl.ufpr.br/"
       ;; Asia
       "https://mirrors.bfsu.edu.cn/kde/"
       "https://ftp-srv2.kddi-research.jp/pub/X11/kde/"
       "https://mirrors.xtom.jp/kde/"
       "https://mirrors.xtom.hk/kde/"
       ;; Africa
       "https://mirror.dimensiondata.com/mirror/ftp.kde.org/"
       ;; Oceania
       "https://mirrors.xtom.au/kde/")
      (openbsd
       "https://ftp.openbsd.org/pub/OpenBSD/"
       ;; Anycast CDN redirecting to your friendly local mirror.
       "https://mirrors.evowise.com/pub/OpenBSD/"
       ;; Other HTTPS mirrors from https://www.openbsd.org/ftp.html
       "https://mirror.aarnet.edu.au/pub/OpenBSD/"
       "https://ftp2.eu.openbsd.org/pub/OpenBSD/"
       "https://openbsd.c3sl.ufpr.br/pub/OpenBSD/"
       "https://openbsd.ipacct.com/pub/OpenBSD/"
       "https://ftp.OpenBSD.org/pub/OpenBSD/"
       "https://openbsd.cs.toronto.edu/pub/OpenBSD/"
       "https://openbsd.delfic.org/pub/OpenBSD/"
       "https://openbsd.mirror.netelligent.ca/pub/OpenBSD/"
       "https://mirrors.ucr.ac.cr/pub/OpenBSD/"
       "https://mirrors.dotsrc.org/pub/OpenBSD/"
       "https://mirror.one.com/pub/OpenBSD/"
       "https://ftp.fr.openbsd.org/pub/OpenBSD/"
       "https://ftp2.fr.openbsd.org/pub/OpenBSD/"
       "https://mirrors.ircam.fr/pub/OpenBSD/"
       "https://ftp.spline.de/pub/OpenBSD/"
       "https://mirror.hs-esslingen.de/pub/OpenBSD/"
       "https://ftp.halifax.rwth-aachen.de/openbsd/"
       "https://ftp.hostserver.de/pub/OpenBSD/"
       "https://ftp.fau.de/pub/OpenBSD/"
       "https://ftp.cc.uoc.gr/pub/OpenBSD/"
       "https://openbsd.hk/pub/OpenBSD/"
       "https://ftp.heanet.ie/pub/OpenBSD/"
       "https://openbsd.mirror.garr.it/pub/OpenBSD/"
       "https://mirror.litnet.lt/pub/OpenBSD/"
       "https://mirror.meerval.net/pub/OpenBSD/"
       "https://ftp.nluug.nl/pub/OpenBSD/"
       "https://ftp.bit.nl/pub/OpenBSD/"
       "https://mirrors.dalenys.com/pub/OpenBSD/"
       "https://ftp.icm.edu.pl/pub/OpenBSD/"
       "https://ftp.rnl.tecnico.ulisboa.pt/pub/OpenBSD/"
       "https://mirrors.pidginhost.com/pub/OpenBSD/"
       "https://mirror.yandex.ru/pub/OpenBSD/"
       "https://ftp.eu.openbsd.org/pub/OpenBSD/"
       "https://ftp.yzu.edu.tw/pub/OpenBSD/"
       "https://www.mirrorservice.org/pub/OpenBSD/"
       "https://anorien.csc.warwick.ac.uk/pub/OpenBSD/"
       "https://mirror.bytemark.co.uk/pub/OpenBSD/"
       "https://mirrors.sonic.net/pub/OpenBSD/"
       "https://ftp3.usa.openbsd.org/pub/OpenBSD/"
       "https://mirrors.syringanetworks.net/pub/OpenBSD/"
       "https://openbsd.mirror.constant.com/pub/OpenBSD/"
       "https://ftp4.usa.openbsd.org/pub/OpenBSD/"
       "https://ftp5.usa.openbsd.org/pub/OpenBSD/"
       "https://mirror.esc7.net/pub/OpenBSD/")
      (mate
       "https://pub.mate-desktop.org/releases/"
       "http://pub.mate-desktop.org/releases/")
      (qt
       "https://mirrors.ocf.berkeley.edu/qt/official_releases/"
       "https://ftp.jaist.ac.jp/pub/qtproject/official_releases/"
       "https://ftp.nluug.nl/languages/qt/official_releases/"
       "https://mirrors.cloud.tencent.com/qt/official_releases/"
       "https://mirrors.sjtug.sjtu.edu.cn/qt/official_releases/"
       "https://qtproject.mirror.liquidtelecom.com/official_releases/"
       "https://download.qt.io/official_releases/")))) ;slow

(define %mirror-file
  ;; Copy of the list of mirrors to a file.  This allows us to keep a single
  ;; copy in the store, and computing it here avoids repeated calls to
  ;; 'object->string'.
  (plain-file "mirrors" (object->string %mirrors)))

(define %content-addressed-mirrors
  ;; List of content-addressed mirrors.  Each mirror is represented as a
  ;; procedure that takes a file name, an algorithm (symbol) and a hash
  ;; (bytevector), and returns a URL or #f.
  '(begin
     (use-modules (guix base16) (guix base32))

     (define (guix-publish host)
       (lambda (file algo hash)
         ;; Files served by 'guix publish' are accessible under a single
         ;; hash algorithm.
         (string-append "https://" host "/file/"
                        file "/" (symbol->string algo) "/"
                        (bytevector->nix-base32-string hash))))

     (list (guix-publish
            ;; bordeaux.guix.gnu.org uses the nar-herder rather than guix
            ;; publish, but it supports the same style of requests
            "bordeaux.guix.gnu.org")
           (guix-publish "ci.guix.gnu.org")
           (lambda (file algo hash)
             ;; 'tarballs.nixos.org' supports several algorithms.
             (string-append "https://tarballs.nixos.org/"
                            (symbol->string algo) "/"
                            (bytevector->nix-base32-string hash)))
           (lambda (file algo hash)
             ;; Software Heritage usually archives VCS history rather than
             ;; tarballs, but tarballs are sometimes available (and can be
             ;; explicitly stored there.)  For example, see
             ;; <https://archive.softwareheritage.org/api/1/content/sha256:92d0fa1c311cacefa89853bdb53c62f4110cdfda3820346b59cbd098f40f955e/>.
             (string-append "https://archive.softwareheritage.org/api/1/content/"
                            (symbol->string algo) ":"
                            (bytevector->base16-string hash) "/raw/")))))

(define %content-addressed-mirror-file
  ;; Content-addressed mirrors stored in a file.
  (plain-file "content-addressed-mirrors"
              (object->string %content-addressed-mirrors)))

(define %no-mirrors-file
  ;; File specifying an empty list of mirrors, for fallback tests.
  (plain-file "no-content-addressed-mirrors" (object->string ''())))

(define %disarchive-mirrors
  ;; TODO: Eventually turn into a procedure that takes a hash algorithm
  ;; (symbol) and hash (bytevector).
  '("https://disarchive.guix.gnu.org/"
    "https://disarchive.ngyro.com/"))

(define %disarchive-mirror-file
  (plain-file "disarchive-mirrors" (object->string %disarchive-mirrors)))

(define %no-disarchive-mirrors-file
  ;; File specifying an empty list of Disarchive mirrors, for fallback tests.
  (plain-file "no-disarchive-mirrors" (object->string '())))

(define built-in-builders*
  (store-lift built-in-builders))

(define %download-methods
  ;; Either #f (the default) or a list of symbols denoting the sequence of
  ;; download methods to be used--e.g., '(swh nar upstream).
  (make-parameter
   (and=> (getenv "GUIX_DOWNLOAD_METHODS")
          (lambda (str)
            (map string->symbol (string-tokenize str))))))

(define* (built-in-download file-name url
                            #:key system hash-algo hash
                            mirrors content-addressed-mirrors
                            disarchive-mirrors
                            (download-methods (%download-methods))
                            executable?
                            (guile 'unused))
  "Download FILE-NAME from URL using the built-in 'download' builder.  When
EXECUTABLE? is true, make the downloaded file executable.

This is an \"out-of-band\" download in that the returned derivation does not
explicitly depend on Guile, GnuTLS, etc.  Instead, the daemon performs the
download by itself using its own dependencies."
  (mlet %store-monad ((mirrors (lower-object mirrors))
                      (content-addressed-mirrors
                       (lower-object content-addressed-mirrors))
                      (disarchive-mirrors (lower-object disarchive-mirrors)))
    (raw-derivation file-name "builtin:download" '()
                    #:system system
                    #:hash-algo hash-algo
                    #:hash hash
                    #:recursive? executable?
                    #:sources (list mirrors
                                    content-addressed-mirrors
                                    disarchive-mirrors)

                    ;; Honor the user's proxy and locale settings.
                    #:leaked-env-vars '("http_proxy" "https_proxy"
                                        "LC_ALL" "LC_MESSAGES" "LANG"
                                        "COLUMNS")

                    #:env-vars `(("url" . ,(object->string url))
                                 ("mirrors" . ,mirrors)
                                 ("content-addressed-mirrors"
                                  . ,content-addressed-mirrors)
                                 ("disarchive-mirrors" . ,disarchive-mirrors)
                                 ,@(if executable?
                                       '(("executable" . "1"))
                                       '())
                                 ,@(if download-methods
                                       `(("download-methods"
                                          . ,(object->string
                                              download-methods)))
                                       '()))

                    ;; Do not offload this derivation because we cannot be
                    ;; sure that the remote daemon supports the 'download'
                    ;; built-in.  We may remove this limitation when support
                    ;; for that built-in is widespread.
                    #:local-build? #t)))

(define* (url-fetch* url hash-algo hash
                     #:optional name
                     #:key (system (%current-system))
                     (guile (default-guile))
                     executable?)
  "Return a fixed-output derivation that fetches data from URL (a string, or a
list of strings denoting alternate URLs), which is expected to have hash HASH
of type HASH-ALGO (a symbol).  By default, the file name is the base name of
URL; optionally, NAME can specify a different file name.  When EXECUTABLE? is
true, make the downloaded file executable.

When one of the URL starts with mirror://, then its host part is
interpreted as the name of a mirror scheme, taken from %MIRROR-FILE.

Alternatively, when URL starts with file://, return the corresponding file
name in the store."
  (define file-name
    (match url
      ((head _ ...)
       (basename head))
      (_
       (basename url))))

  (let ((uri (and (string? url) (string->uri url))))
    (if (or (and (string? url) (not uri))
            (and uri (memq (uri-scheme uri) '(#f file))))
        (interned-file (if uri (uri-path uri) url)
                       (or name file-name))
        (mlet %store-monad ((builtins (built-in-builders*)))
          ;; The "download" built-in builder was added in guix-daemon in
          ;; Nov. 2016 and made it in the 0.12.0 release of Dec. 2016.  We now
          ;; require it.
          (unless (member "download" builtins)
            (error "'guix-daemon' is too old, please upgrade" builtins))

          (built-in-download (or name file-name) url
                             #:guile guile
                             #:system system
                             #:hash-algo hash-algo
                             #:hash hash
                             #:executable? executable?
                             #:mirrors %mirror-file
                             #:content-addressed-mirrors
                             %content-addressed-mirror-file
                             #:disarchive-mirrors
                             %disarchive-mirror-file)))))

(define* (url-fetch/executable url hash-algo hash
                               #:optional name
                               #:key (system (%current-system))
                               (guile (default-guile)))
  "Like 'url-fetch', but make the downloaded file executable."
  (url-fetch* url hash-algo hash name
              #:system system
              #:guile guile
              #:executable? #t))

(define* (url-fetch/tarbomb url hash-algo hash
                            #:optional name
                            #:key (system (%current-system))
                            (guile (default-guile)))
  "Similar to 'url-fetch' but unpack the file from URL in a directory of its
own.  This helper makes it easier to deal with \"tar bombs\"."
  (define file-name
    (match url
      ((head _ ...)
       (basename head))
      (_
       (basename url))))
  (define gzip
    (module-ref (resolve-interface '(gnu packages compression)) 'gzip))
  (define tar
    (module-ref (resolve-interface '(gnu packages base)) 'tar))

  (mlet %store-monad ((drv (url-fetch* url hash-algo hash
                                       (string-append "tarbomb-"
                                                      (or name file-name))
                                       #:system system
                                       #:guile guile))
                      (guile (package->derivation guile system
                                                  #:graft? #f)))
    ;; Take the tar bomb, and simply unpack it as a directory.
    ;; Use ungrafted tar/gzip so that the resulting tarball doesn't depend on
    ;; whether grafts are enabled.
    (gexp->derivation (or name file-name)
                      (with-imported-modules '((guix build utils))
                        #~(begin
                            (use-modules (guix build utils))
                            (mkdir #$output)
                            (setenv "PATH" (string-append #+gzip "/bin"))
                            (chdir #$output)
                            (invoke (string-append #+tar "/bin/tar")
                                    "xf" #$drv)))
                      #:system system
                      #:guile-for-build guile
                      #:graft? #f
                      #:local-build? #t)))

(define* (url-fetch/zipbomb url hash-algo hash
                            #:optional name
                            #:key (system (%current-system))
                            (guile (default-guile)))
  "Similar to 'url-fetch' but unpack the zip file at URL in a directory of its
own.  This helper makes it easier to deal with \"zip bombs\"."
  (define file-name
    (match url
      ((head _ ...)
       (basename head))
      (_
       (basename url))))
  (define unzip
    (module-ref (resolve-interface '(gnu packages compression)) 'unzip))

  (mlet %store-monad ((drv (url-fetch* url hash-algo hash
                                       (string-append "zipbomb-"
                                                      (or name file-name))
                                       #:system system
                                       #:guile guile))
                      (guile (package->derivation guile system
                                                  #:graft? #f)))
    ;; Take the zip bomb, and simply unpack it as a directory.
    ;; Use ungrafted unzip so that the resulting tarball doesn't depend on
    ;; whether grafts are enabled.
    (gexp->derivation (or name file-name)
                      (with-imported-modules '((guix build utils))
                        #~(begin
                            (use-modules (guix build utils))
                            (mkdir #$output)
                            (chdir #$output)
                            (invoke (string-append #+unzip "/bin/unzip")
                                    #$drv)))
                      #:system system
                      #:guile-for-build guile
                      #:graft? #f
                      #:local-build? #t)))

(define* (download-to-store store url #:optional (name (basename url))
                            #:key (log (current-error-port)) recursive?
                            (verify-certificate? #t))
  "Download from URL to STORE, either under NAME or URL's basename if
omitted.  Write progress reports to LOG.  RECURSIVE? has the same effect as
the same-named parameter of 'add-to-store'.  VERIFY-CERTIFICATE? determines
whether or not to validate HTTPS server certificates."
  (define uri
    (string->uri url))

  (if (or (not uri) (memq (uri-scheme uri) '(file #f)))
      (add-to-store store name recursive? "sha256"
                    (if uri (uri-path uri) url))
      (call-with-temporary-output-file
       (lambda (temp port)
         (let ((result
                (parameterize ((current-output-port log))
                  (url-fetch url temp
                             #:mirrors %mirrors
                             #:verify-certificate? verify-certificate?))))
           (close port)
           (and result
                (add-to-store store name recursive? "sha256" temp)))))))

;;; download.scm ends here
