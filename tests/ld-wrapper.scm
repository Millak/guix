;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025 Maxim Cournoyer <maxim@guixotic.coop>
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

(use-modules (guix tests)
             (srfi srfi-26)
             (srfi srfi-64))

(define %project-root
  (dirname (dirname (canonicalize-path (or (current-filename))))))

;;; Load the ld-wrapper module and expose some internals, for white-box
;;; testing.
(load (string-append %project-root "/gnu/packages/ld-wrapper.in"))
(define ld-wrapper-module (resolve-module '(gnu build-support ld-wrapper)))
(define library-files-linked (module-ref ld-wrapper-module 'library-files-linked))

(define %dummy-library-prefix "/gnu/store/...-dummy-0.0.0/lib")

(test-begin "ld-wrapper")

(define lugaru-link-arguments
  '("-Wall" "-Wextra" "-Wno-parentheses" "-pedantic" "--std=gnu++11" "-O2" "-g"
    "-DNDEBUG" "-rdynamic" "-Wl,--dependency-file=CMakeFiles/lugaru.dir/link.d"
    "CMakeFiles/lugaru.dir/Source/main.cpp.o"
    "CMakeFiles/lugaru.dir/Source/Animation/Animation.cpp.o"
    "-o" "lugaru" "-lopenal" "-lpng" "-ljpeg" "-lz" "-lSDL2"
    "-lGL" "-lGLU" "-lvorbisfile" "-logg"))

(define lugaru-link-libraries
  (map (cut string-append "lib" <> ".so")
       '("openal" "png" "jpeg" "z" "SDL2" "GL" "GLU" "vorbisfile" "ogg")))

(test-equal "library files linked"
  (map (cut string-append %dummy-library-prefix "/" <>)
       lugaru-link-libraries)
  (mock ((guile) search-path
         (lambda (_ library)
           (string-append %dummy-library-prefix "/" library)))
        (library-files-linked lugaru-link-arguments "dummy:library:path")))

(test-end)
