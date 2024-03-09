;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2022-2024 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (guix cpu)
  #:use-module (guix sets)
  #:use-module (guix memoization)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:export (current-cpu
            cpu?
            cpu-architecture
            cpu-vendor
            cpu-family
            cpu-model
            cpu-flags

            cpu->gcc-architecture
            gcc-architecture->micro-architecture-level))

;;; Commentary:
;;;
;;; This module provides tools to determine the micro-architecture supported
;;; by the CPU and to map it to a name known to GCC's '-march'.
;;;
;;; Code:

;; CPU description.
(define-record-type <cpu>
  (cpu architecture vendor family model flags)
  cpu?
  (architecture cpu-architecture)                 ;string, from 'uname'
  (vendor       cpu-vendor)                       ;string
  (family       cpu-family)                       ;integer
  (model        cpu-model)                        ;integer
  (flags        cpu-flags))                       ;set of strings

(define current-cpu
  (mlambda ()
    "Return a <cpu> record representing the host CPU."
    (define (prefix? prefix)
      (lambda (str)
        (string-prefix? prefix str)))

    (call-with-input-file "/proc/cpuinfo"
      (lambda (port)
        (let loop ((vendor #f)
                   (family #f)
                   (model #f)
                   (flags (set)))
          (match (read-line port)
            ((? eof-object?)
             (cpu (utsname:machine (uname))
                  vendor family model flags))
            ;; vendor for x86_64 and i686
            ((? (prefix? "vendor_id") str)
             (match (string-tokenize str)
               (("vendor_id" ":" vendor)
                (loop vendor family model flags))))
            ;; vendor for aarch64 and armhf
            ((? (prefix? "CPU implementer") str)
             (match (string-tokenize str)
               (("CPU" "implementer" ":" vendor)
                (loop vendor family model flags))))
            ;; family for x86_64 and i686
            ((? (prefix? "cpu family") str)
             (match (string-tokenize str)
               (("cpu" "family" ":" family)
                (loop vendor (string->number family) model flags))))
            ;; model for x86_64 and i686
            ((? (prefix? "model") str)
             (match (string-tokenize str)
               (("model" ":" model)
                (loop vendor family (string->number model) flags))
               (_
                (loop vendor family model flags))))
            ;; model for aarch64 and armhf
            ((? (prefix? "CPU part") str)
             (match (string-tokenize str)
               (("CPU" "part" ":" model)
                (loop vendor family (string->number (string-drop model 2) 16) flags))))
            ;; flags for x86_64 and i686
            ((? (prefix? "flags") str)
             (match (string-tokenize str)
               (("flags" ":" flags ...)
                (loop vendor family model (list->set flags)))))
            ;; flags for aarch64 and armhf
            ((? (prefix? "Features") str)
             (match (string-tokenize str)
               (("Features" ":" flags ...)
                (loop vendor family model (list->set flags)))))
            (_
             (loop vendor family model flags))))))))

(define (cpu->gcc-architecture cpu)
  "Return the architecture name, suitable for GCC's '-march' flag, that
corresponds to CPU, a record as returned by 'current-cpu'."
  (match (cpu-architecture cpu)
    ((or "x86_64" "i686")
     ;; Transcribed from GCC's 'host_detect_local_cpu' in driver-i386.cc.
     (letrec-syntax ((if-flags (syntax-rules (=>)
                                 ((_)
                                  #f)
                                 ((_ (flags ... => name) rest ...)
                                  (if (every (lambda (flag)
                                               (set-contains? (cpu-flags cpu)
                                                              flag))
                                             '(flags ...))
                                    name
                                    (if-flags rest ...))))))

       (or (and (equal? "GenuineIntel" (cpu-vendor cpu))
                (= 6 (cpu-family cpu))              ;the "Pentium Pro" family
                (if-flags ("avx512f" "amx_complex" => "graniterapids-d")
                          ("avx512f" "amx_fp16" => "graniterapids")
                          ("avx512f" "avx512vp2intersect" => "tigerlake")
                          ("avx512f" "tsxldtrk" => "sapphirerapids")
                          ("avx512f" "avx512bf16" => "cooperlake")
                          ("avx512f" "wbnoinvd" => "icelake-server")
                          ("avx512f" "avx512bitalg" => "icelake-client")
                          ("avx512f" "avx512vbmi" => "cannonlake")
                          ("avx512f" "avx5124vnniw" => "knm")
                          ("avx512f" "avx512er" => "knl")
                          ("avx512f" => "skylake-avx512")
                          ("avx" "prefetchi" => "pantherlake")
                          ("avx" "user_msr" => "clearwaterforest")
                          ("avx" "sm3" => "arrowlake-s")
                          ("avx" "avxvnniint8" => "sierraforest")
                          ("avx" "serialize" => "alderlake")
                          ("avx" "clflushopt" => "skylake")
                          ("avx" "adx" => "broadwell")
                          ("avx" "avx2" => "haswell")
                          ("avx" => "sandybridge")
                          ("sse4_2" "gfni" => "tremont")
                          ("sse4_2" "sgx" => "goldmont-plus")
                          ("sse4_2" "xsave" => "goldmont")
                          ("sse4_2" "movbe" => "silvermont")
                          ("sse4_2" => "nehalem")
                          ("ssse3" "movbe" => "bonnell")
                          ("ssse3" => "core2")
                          ("longmode" => "x86-64")
                          ("lm" => "x86-64")))

           (and (equal? "AuthenticAMD" (cpu-vendor cpu))
                (or (and (= 22 (cpu-family cpu))
                         (if-flags ("movbe" => "btver2")))
                    (and (= 6 (cpu-family cpu))
                         (if-flags ("3dnowp" => "athalon")
                                   ("longmode" "sse3" => "k8-sse3")
                                   ("lm" "sse3" => "k8-sse3")
                                   ("longmode" => "k8")
                                   ("lm" => "k8")))
                    (if-flags ("avx512f" => "znver4")
                              ("vaes" => "znver3")
                              ("clwb" => "znver2")
                              ("clzero" => "znver1")
                              ("avx2" => "bdver4")
                              ("xsaveopt" => "bdver3")
                              ("bmi" => "bdver2")
                              ("xop" => "bdver1")
                              ("sse4a" "has_ssse3" => "btver1")
                              ("sse4a" => "amdfam10")
                              ("sse2" "sse3" => "k8-sse3")
                              ("longmode" "sse3" => "k8-sse3")
                              ("lm" "sse3" => "k8-sse3")
                              ("sse2" => "k8")
                              ("longmode" => "k8")
                              ("lm" => "k8")
                              ("mmx" "3dnow" => "k6-3")
                              ("mmx" => "k6")
                              (_ => "pentium"))))

           ;; Fallback case for non-Intel processors or for processors not
           ;; recognized above.
           (if (and (= 7 (cpu-family cpu))
                    (= #x3b (cpu-model cpu)))
             "lujiazui"
             (cpu->micro-architecture-level cpu))
           (if (and (= 7 (cpu-family cpu))
                    (>= #x5b (cpu-model cpu)))
             "yongfeng"
             (cpu->micro-architecture-level cpu))

         ;; TODO: Recognize CENTAUR/CYRIX/NSC?

         (match (cpu-architecture cpu)
           ("x86_64" "x86-64")
           (_ "generic")))))
    ("aarch64"
     ;; Transcribed from GCC's list of aarch64 processors in aarch64-cores.def
     ;; What to do with big.LITTLE cores?
     (match (cpu-vendor cpu)
       ("0x41"
        (match (cpu-model cpu)
          ((or #xd02 #xd04 #xd03 #xd07 #xd08 #xd09)
           "armv8-a")
          ((or #xd05 #xd0a #xd0b #xd0e #xd0d #xd41 #xd42 #xd4b #xd06 #xd43 #xd44
               #xd4c #xd0c #xd4a)
           "armv8.2-a")
          (#xd40
           "armv8.4-a")
          (#xd15
           "armv8-r")
          ((or #xd46 #xd47 #xd4d #xd48 #xd4e #xd49 #xd4f)
           "armv9-a")
          ((or #xd80 #xd81)
           "armv9.2-a")))
       ("0x42"
        "armv8.1-a")
       ("0x43"
        (match (cpu-model cpu)
          ((or #x0a0 #x0a1 #x0a2 #x0a3)
           "armv8-a")
          (#x0af
           "armv8.1-a")
          ((or #x0b0 #x0b1 #x0b2 #x0b3 #x0b4 #x0b5)
           "armv8.2-a")
          (#x0b8
           "armv8.3-a")))
       ("0x46"
        "armv8.2-a")
       ("0x48"
        "armv8.2-a")
       ("0x50"
        "armv8-a")
       ("0x51"
        (match (cpu-model cpu)
          (#xC00
           "armv8-a")
          (#x516
           "armv8.1-a")
          (#xC01
           "armv8.4-a")))
       ("0x53"
        "armv8-a")
       ("0x68"
        "armv8-a")
       ("0x6d"
        "armv9-a")
       ("0xC0"
        (match (cpu-model cpu)
          ((or #xac3 #xac4)
           "armv8.6-a")
          (#xac5
           "armv8.7-a")))
       ("0xC00"
        "armv8-a")
       (_
        "armv8-a"))
     "armv8-a")
    (architecture
     ;; TODO: More architectures
     architecture)))

(define (cpu->micro-architecture-level cpu)
  "Return a micro-architecture name, suitable for generalized optimizations that
correspond roughly to CPU, a record as returned by 'current-cpu'."
  (match (cpu-architecture cpu)
    ("x86_64"
     (or (letrec-syntax ((if-flags (syntax-rules (=>)
                                     ((_)
                                      #f)
                                     ((_ (flags ... => name) rest ...)
                                      (if (every (lambda (flag)
                                                   (set-contains? (cpu-flags cpu)
                                                                  flag))
                                                 '(flags ...))
                                        name
                                        (if-flags rest ...))))))

           (if-flags
             ;; https://gitlab.com/x86-psABIs/x86-64-ABI/-/blob/master/x86-64-ABI/low-level-sys-info.tex
             ;; v4: AVX512F, AVX512BW, AVX512CD, AVX512DQ, AVX512VL
             ;; v3: AVX, AVX2, BMI1, BMI2, F16C, FMA, LZCNT, MOVBE, OSXSAVE
             ;; v2: CMPXCHG16B, LAHF, SAHF, POPCNT, SSE3, SSE4.1, SSE4.2, SSSE3
             ("avx512f" "avx512bw" "abx512cd" "abx512dq" "avx512vl"
              "avx" "avx2" "bmi1" "bmi2" "f16c" "fma" "movbe"
              "popcnt" "sse3" "sse4_1" "sse4_2" "ssse3" => "x86-64-v4")
             ("avx" "avx2" "bmi1" "bmi2" "f16c" "fma" "movbe"
              "popcnt" "sse3" "sse4_1" "sse4_2" "ssse3" => "x86-64-v3")
             ("popcnt" "sse3" "sse4_1" "sse4_2" "ssse3" => "x86-64-v2")
             (_ => "x86-64")))
         "x86-64"))
    (architecture
     ;; TODO: More architectures
     architecture)))

(define (gcc-architecture->micro-architecture-level gcc-architecture)
  "Return a matching psABI micro-architecture, allowing optimizations for x86_64
CPUs for compilers which don't allow for more focused optimizing."
  ;; Matching gcc-architectures isn't an easy task, with the rule-of-thumb being
  ;; AVX512F+ for x86-64-v4, AVX+ for x86-64-v3.
  ;; https://gitlab.com/x86-psABIs/x86-64-ABI/-/blob/master/x86-64-ABI/low-level-sys-info.tex
  (match gcc-architecture
    ((or "graniterapids-d" "graniterapids" "tigerlake" "sapphirerapids"
         "cooperlake" "icelake-server" "icelake-client" "cannonlake" "knm"
         "knl" "skylake-avx512"
         "znver4")
     "x86-64-v4")
    ((or "pantherlake" "clearwaterforest" "arrowlake-s" "sierraforest"
         "alderlake" "skylake" "broadwell" "haswell"
         "znver3" "znver2" "znver1" "bdver4")
     "x86-64-v3")
    ((or "sandybridge" "tremont" "goldmont-plus" "goldmont" "silvermont"
         "nehalem" "bonnell" "core2"
         "btver2" "athalon" "k8-sse3" "k8" "bdver3" "bdver2" "bdver1" "btver1"
         "amdfam10"
         "lujiazui" "yongfeng" "x86-64")
     "x86-64")
    (_ gcc-architecture)))
