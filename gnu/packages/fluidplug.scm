;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025 Giacomo Leidi <goodoldpaul@autistici.org>
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

(define-module (gnu packages fluidplug)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (guix utils)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages pkg-config)
  #:use-module (srfi srfi-1)
  #:export (fluidplug-plugin
            fluidplug-plugin?
            fluidplug-plugin-name
            fluidplug-plugin-path
            fluidplug-plugin-hash
            fluidplug-plugin-license

            fluidplug-lv2

            fluidplug-plugin->package
            fluidplug-airfont320-lv2
            fluidplug-avl-drumkits-perc-lv2
            fluidplug-black-pearl-4a-lv2
            fluidplug-black-pearl-4b-lv2
            fluidplug-black-pearl-5-lv2
            fluidplug-red-zeppelin-4-lv2
            fluidplug-red-zeppelin-5-lv2
            fluidplug-fluidgm-lv2
            fluidplug-fluidbass-lv2
            fluidplug-fluidbrass-lv2
            fluidplug-fluidchromperc-lv2
            fluidplug-fluiddrums-lv2
            fluidplug-fluidensemble-lv2
            fluidplug-fluidethnic-lv2
            fluidplug-fluidguitars-lv2
            fluidplug-fluidorgans-lv2
            fluidplug-fluidpercussion-lv2
            fluidplug-fluidpianos-lv2
            fluidplug-fluidpipes-lv2
            fluidplug-fluidreeds-lv2
            fluidplug-fluidsoundfx-lv2
            fluidplug-fluidstrings-lv2
            fluidplug-fluidsynthfx-lv2
            fluidplug-fluidsynthleads-lv2
            fluidplug-fluidsynthpads-lv2))

(define-record-type* <fluidplug-plugin>
  fluidplug-plugin make-fluidplug-plugin
  fluidplug-plugin?
  (name            fluidplug-plugin-name)         ;string
  (hash            fluidplug-plugin-hash)         ;string
  (path            fluidplug-plugin-path)         ;string
  (license         fluidplug-plugin-license       ;license
                   (default license:cc-by-sa4.0)))

(define (fluidplug-plugin->package-name record)
  (string-append "fluidplug-"
                 (string-downcase
                  (string-replace-substring
                   (fluidplug-plugin-name record) "_" "-"))
                 "-lv2"))

(define (fluidplug-plugin->origin record)
  (origin
    (method url-fetch)
    (uri
     (string-append "https://download.linuxaudio.org/"
                    "musical-instrument-libraries/sf2/"
                    (fluidplug-plugin-path record)))
    (sha256
     (base32 (fluidplug-plugin-hash record)))))

(define (fluidplug-plugin->local-path record)
  (string-append "./" (fluidplug-plugin-name record) ".lv2/"
                 (basename (fluidplug-plugin-path record))))

(define airfont320
  (fluidplug-plugin
   (name "AirFont320")
   (path "airfont_a340u.tar.7z")
   (hash "1x4xzm4khq823i4p18ydbkjza2nv3hzwyxp7vf3gzqs374jdcqbw")
   (license license:gpl2)))

(define avl-drumkits-perc
  (fluidplug-plugin
   (name "AVL_Drumkits_Perc")
   (path "AVL-Drumkits-1.1-SF2-splitted/AVL_Drumkits_Perc_1.1.tar.7z")
   (hash "0l1zvzw9dg922wf9llcl9zlw5ybjmayg7yqacp253xcr2jz1hn7m")
   (license license:cc-by-sa3.0)))

(define black-pearl-4a
  (fluidplug-plugin
   (name "Black_Pearl_4A")
   (path "AVL-Drumkits-1.1-SF2-splitted/Black_Pearl_4A-1.1.tar.7z")
   (hash "0flzlg5m2r3df8dchydzg3xllcv7ignr7hx6qxzy51s6gzrlbzli")
   (license license:cc-by-sa3.0)))

(define black-pearl-4b
  (fluidplug-plugin
   (name "Black_Pearl_4B")
   (path "AVL-Drumkits-1.1-SF2-splitted/Black_Pearl_4B-1.1.tar.7z")
   (hash "1z5q8z4k81xnbcv0gpc2xgrzqm9fg4n2yly1kaw82q7sm29vi0z9")
   (license license:cc-by-sa3.0)))

(define black-pearl-5
  (fluidplug-plugin
   (name "Black_Pearl_5")
   (path "AVL-Drumkits-1.1-SF2-splitted/Black_Pearl_5-1.1.tar.7z")
   (hash "0mg41wc912sfj2mydn86ychwk21l3ngbn7k3qsm6jgf1ipmsip9y")
   (license license:cc-by-sa3.0)))

(define red-zeppelin-4
  (fluidplug-plugin
   (name "Red_Zeppelin_4")
   (path "AVL-Drumkits-1.1-SF2-splitted/Red_Zeppelin_4-1.1.tar.7z")
   (hash "0pcswg4hlyn8j6nghbwvxa71ibp6r0wxcbxjzxs4iim43vpxc659")
   (license license:cc-by-sa3.0)))

(define red-zeppelin-5
  (fluidplug-plugin
   (name "Red_Zeppelin_5")
   (path "AVL-Drumkits-1.1-SF2-splitted/Red_Zeppelin_5-1.1.tar.7z")
   (hash "18hhmi1d1i7gr2rp4wn28pnfwl825rmhmjnvxvvcwdmis6nad4gm")
   (license license:cc-by-sa3.0)))

(define fluidgm
  (fluidplug-plugin
   (name "FluidGM")
   (path "fluidr3.tar.7z")
   (hash "00ka1b2pxn0g2g0hm7kdg5w785pfx8f9585238d183wjdkc3a8m6")
   (license license:expat)))

(define fluidbass
  (fluidplug-plugin
   (name "FluidBass")
   (path "fluidr3-splitted/fluidr3gm_bass.sf2.tar.7z")
   (hash "1zhixrxkwvmn6xkpy9f8zkrwxmaqwdcx0jz7zg8lc2amqkx9wqan")))

(define fluidbrass
  (fluidplug-plugin
   (name "FluidBrass")
   (path "fluidr3-splitted/fluidr3gm_brass.sf2.tar.7z")
   (hash "027l3q4q9011xzbzsrsp5nj3h05w9dj7d8b943xzck7q4sp9alkp")))

(define fluidchromperc
  (fluidplug-plugin
   (name "FluidChromPerc")
   (path "fluidr3-splitted/fluidr3gm_chromaticpercussion.sf2.tar.7z")
   (hash "1233fh36wlifrawh57v8vycxc0g4pbvy3qr9f596g9n7mm4h51f7")))

(define fluiddrums
  (fluidplug-plugin
   (name "FluidDrums")
   (path "fluidr3-splitted/fluidr3gm_drums.sf2.tar.7z")
   (hash "01pv3aj52pz8xy29acprwkb69jxhrn64kx7w29ch5bl091lccqgr")))

(define fluidensemble
  (fluidplug-plugin
   (name "FluidEnsemble")
   (path "fluidr3-splitted/fluidr3gm_ensemble.sf2.tar.7z")
   (hash "0rswcgzapcjmwddh2hgicym27hqz3p43r2s8j7hy1s40hzk3nbax")))

(define fluidethnic
  (fluidplug-plugin
   (name "FluidEthnic")
   (path "fluidr3-splitted/fluidr3gm_ethnic.sf2.tar.7z")
   (hash "0z1z37wn6lw1n919bkpfirachg5wwddi7f0g4l9vimazv34ds1b1")))

(define fluidguitars
  (fluidplug-plugin
   (name "FluidGuitars")
   (path "fluidr3-splitted/fluidr3gm_guitar.sf2.tar.7z")
   (hash "1d5jxx4sgbyh9sqmfksggcsri9hal7mw83a6h1vqb9xf59gnmx1i")))

(define fluidorgans
  (fluidplug-plugin
   (name "FluidOrgans")
   (path "fluidr3-splitted/fluidr3gm_organ.sf2.tar.7z")
   (hash "12zlq28cmr2hsgn1319hidzcm8yifx6vsqsqk544z3qkhc8cg4fr")))

(define fluidpercussion
  (fluidplug-plugin
   (name "FluidPercussion")
   (path "fluidr3-splitted/fluidr3gm_percussive.sf2.tar.7z")
   (hash "1gql0g1zk09n6r90mav47khdc406jhmg5iql6i2zjmg9l7yg65n3")))

(define fluidpianos
  (fluidplug-plugin
   (name "FluidPianos")
   (path "fluidr3-splitted/fluidr3gm_piano.sf2.tar.7z")
   (hash "06a817xvx7qj8plr1vhpanbdr97cbmzky0pp6xhff916s5k0jg2r")))

(define fluidpipes
  (fluidplug-plugin
   (name "FluidPipes")
   (path "fluidr3-splitted/fluidr3gm_pipe.sf2.tar.7z")
   (hash "09wzvsabcif27687g71g4m75zz08zpwpm2dzwh8a7xsfi5qdynf2")))

(define fluidreeds
  (fluidplug-plugin
   (name "FluidReeds")
   (path "fluidr3-splitted/fluidr3gm_reed.sf2.tar.7z")
   (hash "1yb0whi67wz7ag6hcfdns69m1ic3fq6firvbb893ilnx8h17yjak")))

(define fluidsoundfx
  (fluidplug-plugin
   (name "FluidSoundFX")
   (path "fluidr3-splitted/fluidr3gm_soundeffects.sf2.tar.7z")
   (hash "0sg8gca9735gy0pna63vgsfnabk54dl30n1lnhcf976n7y0npzfx")))

(define fluidstrings
  (fluidplug-plugin
   (name "FluidStrings")
   (path "fluidr3-splitted/fluidr3gm_strings.sf2.tar.7z")
   (hash "1nnxr62i3p3hhdpwlly62c4cvf7f3dzq818fmlnlxp25215h17bs")))

(define fluidsynthfx
  (fluidplug-plugin
   (name "FluidSynthFX")
   (path "fluidr3-splitted/fluidr3gm_syntheffects.sf2.tar.7z")
   (hash "0rlq58v9m6wjk45kxmza54y8fkj1b8y1zg00r80nwyqz6rylgjh4")))

(define fluidsynthleads
  (fluidplug-plugin
   (name "FluidSynthLeads")
   (path "fluidr3-splitted/fluidr3gm_synthlead.sf2.tar.7z")
   (hash "0axi5dyqf8zh1gn82qq0jpzr0cgbsjn4xm0yrlqld4k6h9ggj475")))

(define fluidsynthpads
  (fluidplug-plugin
   (name "FluidSynthPads")
   (path "fluidr3-splitted/fluidr3gm_synthpad.sf2.tar.7z")
   (hash "1q4pd6ymb08yc6rz68wlidjp5kp7pygc2aw45djw9wr8id6pnadi")))

(define fluidplug-plugins
  (list airfont320
        avl-drumkits-perc
        black-pearl-4a
        black-pearl-4b
        black-pearl-5
        red-zeppelin-4
        red-zeppelin-5
        fluidgm
        fluidbass
        fluidbrass
        fluidchromperc
        fluiddrums
        fluidensemble
        fluidethnic
        fluidguitars
        fluidorgans
        fluidpercussion
        fluidpianos
        fluidpipes
        fluidreeds
        fluidsoundfx
        fluidstrings
        fluidsynthfx
        fluidsynthleads
        fluidsynthpads))

(define fluidplug-lv2
  (let ((version "0.0.5")
        (revision "0")
        ;; Latest commit from the default brach.
        (commit "1cf494cb3a5b1f7f2a95f872768a1eef141c3e0a"))
    (package
      (name "fluidplug-lv2")
      (version (git-version version revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/falkTX/FluidPlug")
                       (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0rqy4xzswkvmqa4mwyy5nz6r8w4bxjfg2rafbid4lb0izjnhma8q"))))
      (build-system gnu-build-system)
      (arguments
       (list
        #:tests? #f ;there are no tests.
        #:make-flags #~(list (string-append "CC=" #$(cc-for-target))
                             (string-append "DESTDIR=" #$output)
                             "PREFIX=")
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'conditionally-enable-amd64-optimizations
              (lambda _
                (let ((system #$(or (%current-target-system)
                                    (%current-system))))
                  (unless (string-prefix? "x86_64" system)
                    (substitute* "Makefile.mk"
                      (("-msse -msse2 -mfpmath=sse") ""))))))
            (add-after 'conditionally-enable-amd64-optimizations
                       'disable-plugin-download
              (lambda _
                ;; SoundFonts can't be downloaded at build time
                ;; in Guix, so it is disabled.
                (substitute* "Makefile"
                  (("wget") "# wget"))))
            (add-after 'disable-plugin-download 'unpack-plugins
              (lambda _
                (use-modules (srfi srfi-1))
                (for-each
                 (lambda (p)
                   (define source (first p))
                   (define target (second p))
                   (symlink source target))
                 '(#$@(map
                       (lambda (p)
                         (list (fluidplug-plugin->origin p)
                               (fluidplug-plugin->local-path p)))
                       fluidplug-plugins)))))
            (delete 'configure))))
      (native-inputs
       (list gnu-make p7zip pkg-config))
      (inputs (list fluidsynth lv2))
      (native-search-paths
       (list (search-path-specification
               (variable "LV2_PATH")
               (files '("lib/lv2")))))
      (synopsis
       "SoundFonts as LV2 plugins via FluidSynth")
      (description
       "@code{FluidPlug} provides SoundFonts as LV2 plugins via FluidSynth.")
      (home-page "https://github.com/falkTX/FluidPlug")
      (license
       (delete-duplicates
        (map fluidplug-plugin-license fluidplug-plugins))))))

(define (fluidplug-plugin->package record)
  (package
    (inherit fluidplug-lv2)
    (name (fluidplug-plugin->package-name record))
    (arguments
     (substitute-keyword-arguments (package-arguments fluidplug-lv2)
       ((#:make-flags make-flags)
        #~(list (string-append "CC=" #$(cc-for-target))
                (string-append "DESTDIR=" #$output)
                "PREFIX="))
       ((#:phases phases)
        #~(modify-phases #$phases
            (replace 'unpack-plugins
              (lambda _
                (symlink #$(fluidplug-plugin->origin record)
                         #$(fluidplug-plugin->local-path record))))
            (replace 'build
              (lambda* (#:key make-flags #:allow-other-keys)
                (apply invoke `("make" ,@make-flags
                                #$(fluidplug-plugin-name record)))))
            (replace 'install
              (lambda _
                (let* ((plugin-directory
                        (string-append #$(fluidplug-plugin-name record)
                                       ".lv2"))
                       (lib (string-append #$output "/lib/lv2"))
                       (share/doc (string-append #$output "/share/doc"))
                       (plugin-lib (string-append lib "/" plugin-directory)))

                  ;; Install plugin
                  (for-each
                   (lambda (f)
                     (install-file f plugin-lib))
                   (find-files plugin-directory
                               "^.*\\.(sf2|so|ttl)$"))

                  ;; Install license
                  (for-each
                   (lambda (f)
                     (install-file f share/doc))
                   (find-files plugin-directory "(README|License\\.pdf)"))

                  ;; Install UI
                  (copy-recursively (string-append plugin-directory "/modgui")
                                    (string-append plugin-lib "/modgui")))))))))
    (description
     (string-append (package-description fluidplug-lv2) "

This package provides the @code{" (fluidplug-plugin-name record)
                    "} LV2 plugin."))
    (license (fluidplug-plugin-license record))))

(define fluidplug-airfont320-lv2
  (fluidplug-plugin->package airfont320))

(define fluidplug-avl-drumkits-perc-lv2
  (fluidplug-plugin->package avl-drumkits-perc))

(define fluidplug-black-pearl-4a-lv2
  (fluidplug-plugin->package black-pearl-4a))

(define fluidplug-black-pearl-4b-lv2
  (fluidplug-plugin->package black-pearl-4b))

(define fluidplug-black-pearl-5-lv2
  (fluidplug-plugin->package black-pearl-5))

(define fluidplug-red-zeppelin-4-lv2
  (fluidplug-plugin->package red-zeppelin-4))

(define fluidplug-red-zeppelin-5-lv2
  (fluidplug-plugin->package red-zeppelin-5))

(define fluidplug-fluidgm-lv2
  (fluidplug-plugin->package fluidgm))

(define fluidplug-fluidbass-lv2
  (fluidplug-plugin->package fluidbass))

(define fluidplug-fluidbrass-lv2
  (fluidplug-plugin->package fluidbrass))

(define fluidplug-fluidchromperc-lv2
  (fluidplug-plugin->package fluidchromperc))

(define fluidplug-fluiddrums-lv2
  (fluidplug-plugin->package fluiddrums))

(define fluidplug-fluidensemble-lv2
  (fluidplug-plugin->package fluidensemble))

(define fluidplug-fluidethnic-lv2
  (fluidplug-plugin->package fluidethnic))

(define fluidplug-fluidguitars-lv2
  (fluidplug-plugin->package fluidguitars))

(define fluidplug-fluidorgans-lv2
  (fluidplug-plugin->package fluidorgans))

(define fluidplug-fluidpercussion-lv2
  (fluidplug-plugin->package fluidpercussion))

(define fluidplug-fluidpianos-lv2
  (fluidplug-plugin->package fluidpianos))

(define fluidplug-fluidpipes-lv2
  (fluidplug-plugin->package fluidpipes))

(define fluidplug-fluidreeds-lv2
  (fluidplug-plugin->package fluidreeds))

(define fluidplug-fluidsoundfx-lv2
  (fluidplug-plugin->package fluidsoundfx))

(define fluidplug-fluidstrings-lv2
  (fluidplug-plugin->package fluidstrings))

(define fluidplug-fluidsynthfx-lv2
  (fluidplug-plugin->package fluidsynthfx))

(define fluidplug-fluidsynthleads-lv2
  (fluidplug-plugin->package fluidsynthleads))

(define fluidplug-fluidsynthpads-lv2
  (fluidplug-plugin->package fluidsynthpads))
