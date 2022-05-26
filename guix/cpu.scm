;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2022 Efraim Flashner <efraim@flashner.co.il>
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

            cpu->gcc-architecture))

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
    ("x86_64"
     ;; Transcribed from GCC's 'host_detect_local_cpu' in driver-i386.c.
     (or (and (equal? "GenuineIntel" (cpu-vendor cpu))
              (= 6 (cpu-family cpu))              ;the "Pentium Pro" family
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

                (if-flags ("avx" "avx512vp2intersect" "tsxldtrk" => "sapphirerapids")
                          ("avx" "avx512vp2intersect" => "tigerlake")
                          ("avx" "avx512bf16" => "cooperlake")
                          ("avx" "wbnoinvd" => "icelake-server")
                          ("avx" "avx512bitalg" => "icelake-client")
                          ("avx" "avx512vbmi" => "cannonlake")
                          ("avx" "avx5124vnniw" => "knm")
                          ("avx" "avx512er" => "knl")
                          ("avx" "avx512f" => "skylake-avx512")
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
                          ("longmode" => "x86-64"))))

         (and (equal? "AuthenticAMD" (cpu-vendor cpu))
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

                (or (and (= 22 (cpu-family cpu))
                         (if-flags ("movbe" => "btver2")))
                    (and (= 6 (cpu-family cpu))
                         (if-flags ("3dnowp" => "athalon")))
                    (if-flags ("vaes" => "znver3")
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
                              ("sse2" => "k8")
                              ("longmode" => "k8")
                              ("mmx" "3dnow" => "k6-3")
                              ("mmx" => "k6")
                              (_ => "pentium")))))

         ;; Fallback case for non-Intel processors or for Intel processors not
         ;; recognized above.
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
           (if-flags ("avx512" => "knl")
                     ("adx" => "broadwell")
                     ("avx2" => "haswell")
                     ;; TODO: tigerlake, cooperlake, etc.
                     ("avx" => "sandybridge")
                     ("sse4_2" "gfni" => "tremont")
                     ("sse4_2" "sgx" => "goldmont-plus")
                     ("sse4_2" "xsave" => "goldmont")
                     ("sse4_2" "movbe" => "silvermont")
                     ("sse4_2" => "nehalem")
                     ("ssse3" "movbe" => "bonnell")
                     ("ssse3" => "core2")))

         ;; TODO: Recognize CENTAUR/CYRIX/NSC?

         "x86_64"))
    ("aarch64"
     ;; Transcribed from GCC's list of aarch64 processors in aarch64-cores.def
     ;; What to do with big.LITTLE cores?
     (match (cpu-vendor cpu)
       ("0x41"
        (match (cpu-model cpu)
          ((or #xd02 #xd04 #xd03 #xd07 #xd08 #xd09)
           "armv8-a")
          ((or #xd05 #xd0a #xd0b #xd0e #xd0d #xd41 #xd42 #xd4b #xd46 #xd43 #xd44 #xd41 #xd0c #xd4a)
           "armv8.2-a")
          (#xd40
           "armv8.4-a")
          (#xd15
           "armv8-r")
          ((or #xd46 #xd47 #xd48 #xd49 #xd4f)
           "armv9-a")))
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
       ("0xC0"
        "armv8.6-a")
       (_
        "armv8-a"))
     "armv8-a")
    (architecture
     ;; TODO: More architectures
     architecture)))
