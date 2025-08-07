;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015, 2016 David Thompson <dthompson2@worcester.edu>
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2018 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2015, 2019 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016, 2017, 2018, 2019 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2019 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2019–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2021 Trevor Hass <thass@okstate.edu>
;;; Copyright © 2020, 2021, 2022 Liliana Marie Prikler <liliana.prikler@gmail.com>
;;; Copyright © 2021 Maxime Devos <maximedevos@telenet.be>
;;; Copyright © 2024 Jan Wielkiewicz <tona_kosmicznego_smiecia@interia.pl>
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

(define-module (gnu packages minetest)
  #:use-module (guix packages)
  #:use-module (gnu packages luanti))

(define-public minetest
  (deprecated-package "minetest" luanti))

(define-public minetest-server
  (deprecated-package "minetest-server" luanti-server))

(define-public minetest-moreores
  (deprecated-package "minetest-moreores" luanti-moreores))

(define-public minetest-sound-api-core
  (deprecated-package "minetest-sound-api-core" luanti-sound-api-core))

(define-public minetest-basic-materials
  (deprecated-package "minetest-basic-materials" luanti-basic-materials))

(define-public minetest-coloredwood
  (deprecated-package "minetest-coloredwood" luanti-coloredwood))

(define-public minetest-ethereal
  (deprecated-package "minetest-ethereal" luanti-ethereal))

(define-public minetest-homedecor-modpack
  (deprecated-package "minetest-homedecor-modpack" luanti-homedecor-modpack))

(define-public minetest-mesecons
  (deprecated-package "minetest-mesecons" luanti-mesecons))

(define-public minetest-mineclone
  (deprecated-package "minetest-mineclone" luanti-voxelibre))

(define-public minetest-mobs
  (deprecated-package "minetest-mobs" luanti-mobs))

(define-public minetest-mobs-animal
  (deprecated-package "minetest-mobs-animal" luanti-mobs-animal))

(define-public minetest-mobs-monster
  (deprecated-package "minetest-mobs-monster" luanti-mobs-monster))

(define-public minetest-pipeworks
  (deprecated-package "minetest-pipeworks" luanti-pipeworks))

(define-public minetest-technic
  (deprecated-package "minetest-technic" luanti-technic))

(define-public minetest-throwing
  (deprecated-package "minetest-throwing" luanti-throwing))

(define-public minetest-throwing-arrows
  (deprecated-package "minetest-throwing-arrows" luanti-throwing-arrows))

(define-public minetest-worldedit
  (deprecated-package "minetest-worldedit" luanti-worldedit))

(define-public minetest-unifieddyes
  (deprecated-package "minetest-unifieddyes" luanti-unifieddyes))

(define-public minetest-unified-inventory
  (deprecated-package "minetest-unified-inventory" luanti-unified-inventory))

(define-public minetest-advtrains
  (deprecated-package "minetest-advtrains" luanti-advtrains))

(define-public minetest-basic-trains
  (deprecated-package "minetest-basic-trains" luanti-basic-trains))

(define-public minetest-oneblock
  (deprecated-package "minetest-oneblock" luanti-oneblock))

(define-public minetest-wielded-light
  (deprecated-package "minetest-wielded-light" luanti-wielded-light))
