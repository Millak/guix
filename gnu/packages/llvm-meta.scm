;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2024 Efraim Flashner <efraim@flashner.co.il>
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
       ,@(if (version>=? version "11.0")
           '("power8" "power9" "power10" "powerpc64le")))
      ("x86_64"
       ,@(cond
           ((version>=? version "18.0")
            '("nocona" "core2" "penryn" "bonnell" "atom" "silvermont" "slm"
              "goldmont" "goldmont-plus" "tremont" "nehalem" "corei7" "westmere"
              "sandybridge" "corei7-avx" "ivybridge" "core-avx-i" "haswell"
              "core-avx2" "broadwell" "skylake" "skylake-avx512" "skx"
              "cascadelake" "cooperlake" "cannonlake" "icelake-client"
              "rocketlake" "icelake-server" "tigerlake" "sapphirerapids"
              "alderlake" "raptorlake" "arrowlake" "arrowlake-s" "lunarlake"
              "gracemont" "pantherlake" "meteorlake" "sierraforest" "grandridge"
              "graniterapids" "graniterapids-d" "emeraldrapids"
              "clearwaterforest" "knl" "knm" "k8" "athlon64" "athlon-fx"
              "opteron" "k8-sse3" "athlon64-sse3" "opteron-sse3" "amdfam10"
              "barcelona" "btver1" "btver2" "bdver1" "bdver2" "bdver3" "bdver4"
              "znver1" "znver2" "znver3" "znver4" "x86-64" "x86-64-v2"
              "x86-64-v3" "x86-64-v4"))
           ((version>=? version "17.0")
            '("nocona" "core2" "penryn" "bonnell" "atom" "silvermont" "slm"
              "goldmont" "goldmont-plus" "tremont" "nehalem" "corei7" "westmere"
              "sandybridge" "corei7-avx" "ivybridge" "core-avx-i" "haswell"
              "core-avx2" "broadwell" "skylake" "skylake-avx512" "skx"
              "cascadelake" "cooperlake" "cannonlake" "icelake-client"
              "rocketlake" "icelake-server" "tigerlake" "sapphirerapids"
              "alderlake" "raptorlake" "meteorlake" "sierraforest" "grandridge"
              "graniterapids" "graniterapids-d" "emeraldrapids" "knl" "knm" "k8"
              "athlon64" "athlon-fx" "opteron" "k8-sse3" "athlon64-sse3"
              "opteron-sse3" "amdfam10" "barcelona" "btver1" "btver2" "bdver1"
              "bdver2" "bdver3" "bdver4" "znver1" "znver2" "znver3" "znver4"
              "x86-64" "x86-64-v2" "x86-64-v3" "x86-64-v4"))
           ((version>=? version "16.0")
            '("nocona" "core2" "penryn" "bonnell" "atom" "silvermont" "slm"
              "goldmont" "goldmont-plus" "tremont" "nehalem" "corei7" "westmere"
              "sandybridge" "corei7-avx" "ivybridge" "core-avx-i" "haswell"
              "core-avx2" "broadwell" "skylake" "skylake-avx512" "skx"
              "cascadelake" "cooperlake" "cannonlake" "icelake-client"
              "rocketlake" "icelake-server" "tigerlake" "sapphirerapids"
              "alderlake" "raptorlake" "meteorlake" "sierraforest" "grandridge"
              "graniterapids" "emeraldrapids" "knl" "knm" "k8" "athlon64"
              "athlon-fx" "opteron" "k8-sse3" "athlon64-sse3" "opteron-sse3"
              "amdfam10" "barcelona" "btver1" "btver2" "bdver1" "bdver2"
              "bdver3" "bdver4" "znver1" "znver2" "znver3" "znver4" "x86-64"
              "x86-64-v2" "x86-64-v3" "x86-64-v4"))
           ((version>=? version "13.0")
            '("nocona" "core2" "penryn" "bonnell" "atom" "silvermont" "slm"
              "goldmont" "goldmont-plus" "tremont" "nehalem" "corei7" "westmere"
              "sandybridge" "corei7-avx" "ivybridge" "core-avx-i" "haswell"
              "core-avx2" "broadwell" "skylake" "skylake-avx512" "skx"
              "cascadelake" "cooperlake" "cannonlake" "icelake-client"
              "rocketlake" "icelake-server" "tigerlake" "sapphirerapids"
              "alderlake" "knl" "knm" "k8" "athlon64" "athlon-fx" "opteron"
              "k8-sse3" "athlon64-sse3" "opteron-sse3" "amdfam10" "barcelona"
              "btver1" "btver2" "bdver1" "bdver2" "bdver3" "bdver4" "znver1"
              "znver2" "znver3" "x86-64" "x86-64-v2" "x86-64-v3" "x86-64-v4"))
           ((version>=? version "9.0")
            '("atom" "silvermont" "slm" "goldmont" "goldmont-plus" "tremont"
              "nehalem" "corei7" "westmere" "sandybridge" "corei7-avx"
              "ivybridge" "core-avx-i" "haswell" "core-avx2" "broadwell"
              "skylake" "skylake-avx512" "skx" "cascadelake" "cooperlake"
              "cannonlake" "icelake-client" "icelake-server" "knl" "knm" "k8"
              "athlon64" "athlon-fx" "opteron" "k8-sse3" "athlon64-sse3"
              "opteron-sse3" "amdfam10" "barcelona" "btver1" "btver2" "bdver1"
              "bdver2" "bdver3" "bdver4" "znver1" "znver2" "x86-64"))
           (else '()))))))
