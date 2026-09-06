;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2024, 2026 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (gnu packages llvm-meta)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1)
  #:export (clang-compiler-cpu-architectures))

(define (clang-compiler-cpu-architectures version)
  "Return package properties for Clang VERSION."
  `((compiler-cpu-architectures
      ;; These lists were obtained by running:
      ;;
      ;; guix shell clang -- llc -march=x86-64 -mattr=help
      ;;
      ;; and then filtering against clang/test/Misc/target-invalid-cpu-note.c
      ("powerpc64le"
       ,@(cond
           ((version>=? version "19.0")
            '("power8" "power9" "power10" "power11" "powerpc64le"))
           ((version>=? version "11.0")
            '("power8" "power9" "power10" "powerpc64le"))
           (else '())))
      ("x86_64"
       ,@(let* ((clang-23-cpu-architectures
                  '("nocona" "core2" "penryn" "bonnell" "atom" "silvermont"
                    "slm" "goldmont" "goldmont-plus" "tremont" "nehalem"
                    "corei7" "westmere" "sandybridge" "corei7-avx" "ivybridge"
                    "core-avx-i" "haswell" "core-avx2" "broadwell" "skylake"
                    "skylake-avx512" "skx" "cascadelake" "cooperlake"
                    "cannonlake" "icelake-client" "rocketlake" "icelake-server"
                    "tigerlake" "sapphirerapids" "alderlake" "raptorlake"
                    "arrowlake" "arrowlake-s" "lunarlake" "gracemont"
                    "pantherlake" "meteorlake" "wildcatlake" "novalake"
                    "sierraforest" "grandridge" "graniterapids"
                    "graniterapids-d" "emeraldrapids" "clearwaterforest"
                    "diamondrapids" "knl" "knm"

                    "k8" "athlon64" "athlon-fx" "opteron" "k8-sse3"
                    "athlon64-sse3" "opteron-sse3" "amdfam10" "barcelona"
                    "btver1" "btver2" "bdver1" "bdver2" "bdver3" "bdver4"
                    "znver1" "znver2" "znver3" "znver4" "znver5" "znver6"

                    "c86-4g-m4" "c86-4g-m6" "c86-4g-m7" "c86-4g-m8"

                    "x86-64" "x86-64-v2" "x86-64-v3" "x86-64-v4"))
                (clang-22-cpu-architectures
                  (fold delete clang-23-cpu-architectures
                        '("znver6" "c86-4g-m4" "c86-4g-m6" "c86-4g-m7"
                          "c86-4g-m8")))
                (clang-20-cpu-architectures
                  (fold delete clang-22-cpu-architectures
                        '("wildcatlake" "novalake")))
                (clang-19-cpu-architectures
                  (delete "diamondrapids" clang-20-cpu-architectures))
                (clang-18-cpu-architectures
                  (delete "znver5" clang-19-cpu-architectures))
                (clang-17-cpu-architectures
                  (fold delete clang-18-cpu-architectures
                        '("arrowlake" "arrowlake-s" "lunarlake" "gracemont"
                          "pantherlake" "clearwaterforest")))
                (clang-16-cpu-architectures
                  (delete "graniterapids-d" clang-17-cpu-architectures))
                (clang-13-cpu-architectures
                  (fold delete clang-16-cpu-architectures
                        '("raptorlake" "meteorlake" "sierraforest" "grandridge"
                          "graniterapids" "emeraldrapids" "znver4")))
                (clang-9-cpu-architectures
                  (fold delete clang-13-cpu-architectures
                        '("rocketlake" "tigerlake" "sapphirerapids" "alderlake"
                          "znver3" "x86-64-v2" "x86-64-v3" "x86-64-v4"))))
           (cond
             ((version>=? version "23.0")
              clang-23-cpu-architectures)
             ((version>=? version "22.0")
              clang-22-cpu-architectures)
             ((version>=? version "20.0")
              clang-20-cpu-architectures)
             ((version>=? version "19.0")
              clang-19-cpu-architectures)
             ((version>=? version "18.0")
              clang-18-cpu-architectures)
             ((version>=? version "17.0")
              clang-17-cpu-architectures)
             ((version>=? version "16.0")
              clang-16-cpu-architectures)
             ((version>=? version "13.0")
              clang-13-cpu-architectures)
             ((version>=? version "9.0")
              clang-9-cpu-architectures)
             (else '())))))))
