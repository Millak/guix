;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025 Hilton Chain <hako@ultrarare.space>
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

(define-module (gnu packages rust-crates)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cargo)
  #:use-module (gnu packages rust-sources)
  #:export (lookup-cargo-inputs))

;;;
;;; This file is managed by ‘guix import’.  Do NOT add definitions manually.
;;;

;;;
;;; Rust libraries fetched from crates.io and non-workspace development
;;; snapshots.
;;;

(define qqqq-separator 'begin-of-crates)

(define rust-bumpalo-3.17.0
  (crate-source "bumpalo" "3.17.0"
                "1gxxsn2fsjmv03g8p3m749mczv2k4m8xspifs5l7bcx0vx3gna0n"))

(define rust-cc-1.2.18
  (crate-source "cc" "1.2.18"
                "0p6d2pfyrjgqpf2w399wzj4hmyffj6g0gyzg3pdy6xl3gmhlcl2j"))

(define rust-cfg-if-1.0.0
  (crate-source "cfg-if" "1.0.0"
                "1za0vb97n4brpzpv8lsbnzmq5r8f2b0cpqqr0sy8h5bn751xxwds"))

(define rust-getrandom-0.2.15
  (crate-source "getrandom" "0.2.15"
                "1mzlnrb3dgyd1fb84gvw10pyr8wdqdl4ry4sr64i1s8an66pqmn4"))

(define rust-js-sys-0.3.77
  (crate-source "js-sys" "0.3.77"
                "13x2qcky5l22z4xgivi59xhjjx4kxir1zg7gcj0f1ijzd4yg7yhw"))

(define rust-libc-0.2.171
  (crate-source "libc" "0.2.171"
                "1mipla3dy3l59pfa9xy4iw2vdgn8n30dzf4vdnasjflxdqhkg6f1"))

(define rust-log-0.4.27
  (crate-source "log" "0.4.27"
                "150x589dqil307rv0rwj0jsgz5bjbwvl83gyl61jf873a7rjvp0k"))

(define rust-minicov-0.3.7
  (crate-source "minicov" "0.3.7"
                "0jsvi62lklfyvdmsiizipkqcfpsc7h4c4illgxlf28iwrkqyjzzj"))

(define rust-once-cell-1.21.3
  (crate-source "once_cell" "1.21.3"
                "0b9x77lb9f1j6nqgf5aka4s2qj0nly176bpbrv6f9iakk5ff3xa2"))

(define rust-proc-macro2-1.0.94
  (crate-source "proc-macro2" "1.0.94"
                "114wxb56gdj9vy44q0ll3l2x9niqzcbyqikydmlb5f3h5rsp26d3"))

(define rust-quote-1.0.40
  (crate-source "quote" "1.0.40"
                "1394cxjg6nwld82pzp2d4fp6pmaz32gai1zh9z5hvh0dawww118q"))

(define rust-same-file-1.0.6
  (crate-source "same-file" "1.0.6"
                "00h5j1w87dmhnvbv9l8bic3y7xxsnjmssvifw2ayvgx9mb1ivz4k"))

(define rust-shlex-1.3.0
  (crate-source "shlex" "1.3.0"
                "0r1y6bv26c1scpxvhg2cabimrmwgbp4p3wy6syj9n0c4s3q2znhg"))

(define rust-spin-0.9.8
  (crate-source "spin" "0.9.8"
                "0rvam5r0p3a6qhc18scqpvpgb3ckzyqxpgdfyjnghh8ja7byi039"))

(define rust-syn-2.0.100
  (crate-source "syn" "2.0.100"
                "18623wdkns03blpv65xsjn8fipl9p9hj98vlrnhin7nqran496mh"))

(define rust-unicode-ident-1.0.18
  (crate-source "unicode-ident" "1.0.18"
                "04k5r6sijkafzljykdq26mhjpmhdx4jwzvn1lh90g9ax9903jpss"))

(define rust-untrusted-0.9.0
  (crate-source "untrusted" "0.9.0"
                "1ha7ib98vkc538x0z60gfn0fc5whqdd85mb87dvisdcaifi6vjwf"
                #:snippet '(delete-file-recursively "mk")))

(define rust-walkdir-2.5.0
  (crate-source "walkdir" "2.5.0"
                "0jsy7a710qv8gld5957ybrnc07gavppp963gs32xk4ag8130jy99"
                #:snippet '(for-each delete-file-recursively '("compare" "src/tests"))))

(define rust-wasi-0.11.0+wasi-snapshot-preview1
  (crate-source "wasi" "0.11.0+wasi-snapshot-preview1"
                "08z4hxwkpdpalxjps1ai9y7ihin26y9f476i53dv98v45gkqg3cw"))

(define rust-wasm-bindgen-0.2.100
  (crate-source "wasm-bindgen" "0.2.100"
                "1x8ymcm6yi3i1rwj78myl1agqv2m86i648myy3lc97s9swlqkp0y"))

(define rust-wasm-bindgen-backend-0.2.100
  (crate-source "wasm-bindgen-backend" "0.2.100"
                "1ihbf1hq3y81c4md9lyh6lcwbx6a5j0fw4fygd423g62lm8hc2ig"))

(define rust-wasm-bindgen-futures-0.4.50
  (crate-source "wasm-bindgen-futures" "0.4.50"
                "0q8ymi6i9r3vxly551dhxcyai7nc491mspj0j1wbafxwq074fpam"))

(define rust-wasm-bindgen-macro-0.2.100
  (crate-source "wasm-bindgen-macro" "0.2.100"
                "01xls2dvzh38yj17jgrbiib1d3nyad7k2yw9s0mpklwys333zrkz"))

(define rust-wasm-bindgen-macro-support-0.2.100
  (crate-source "wasm-bindgen-macro-support" "0.2.100"
                "1plm8dh20jg2id0320pbmrlsv6cazfv6b6907z19ys4z1jj7xs4a"))

(define rust-wasm-bindgen-shared-0.2.100
  (crate-source "wasm-bindgen-shared" "0.2.100"
                "0gffxvqgbh9r9xl36gprkfnh3w9gl8wgia6xrin7v11sjcxxf18s"))

(define rust-wasm-bindgen-test-0.3.50
  (crate-source "wasm-bindgen-test" "0.3.50"
                "1hsjc60wynlhgw02p32pgb93303pqmsdfxj67gxdkdm37kixbj36"))

(define rust-wasm-bindgen-test-macro-0.3.50
  (crate-source "wasm-bindgen-test-macro" "0.3.50"
                "16znd6wz79v2i3b2sf5n4ld2kcci8br3wcx7z5c9c07sqln09m8p"))

(define rust-web-sys-0.3.77
  (crate-source "web-sys" "0.3.77"
                "1lnmc1ffbq34qw91nndklqqm75rasaffj2g4f8h1yvqqz4pdvdik"))

(define rust-winapi-util-0.1.9
  (crate-source "winapi-util" "0.1.9"
                "1fqhkcl9scd230cnfj8apfficpf5c9vhwnk4yy9xfc1sw69iq8ng"))

(define rust-windows-aarch64-gnullvm-0.52.6
  (crate-source "windows_aarch64_gnullvm" "0.52.6"
                "1lrcq38cr2arvmz19v32qaggvj8bh1640mdm9c2fr877h0hn591j"
                #:snippet '(delete-file-recursively "lib")))

(define rust-windows-aarch64-msvc-0.52.6
  (crate-source "windows_aarch64_msvc" "0.52.6"
                "0sfl0nysnz32yyfh773hpi49b1q700ah6y7sacmjbqjjn5xjmv09"
                #:snippet '(delete-file-recursively "lib")))

(define rust-windows-i686-gnu-0.52.6
  (crate-source "windows_i686_gnu" "0.52.6"
                "02zspglbykh1jh9pi7gn8g1f97jh1rrccni9ivmrfbl0mgamm6wf"
                #:snippet '(delete-file-recursively "lib")))

(define rust-windows-i686-gnullvm-0.52.6
  (crate-source "windows_i686_gnullvm" "0.52.6"
                "0rpdx1537mw6slcpqa0rm3qixmsb79nbhqy5fsm3q2q9ik9m5vhf"
                #:snippet '(delete-file-recursively "lib")))

(define rust-windows-i686-msvc-0.52.6
  (crate-source "windows_i686_msvc" "0.52.6"
                "0rkcqmp4zzmfvrrrx01260q3xkpzi6fzi2x2pgdcdry50ny4h294"
                #:snippet '(delete-file-recursively "lib")))

(define rust-windows-sys-0.52.0
  (crate-source "windows-sys" "0.52.0"
                "0gd3v4ji88490zgb6b5mq5zgbvwv7zx1ibn8v3x83rwcdbryaar8"))

(define rust-windows-sys-0.59.0
  (crate-source "windows-sys" "0.59.0"
                "0fw5672ziw8b3zpmnbp9pdv1famk74f1l9fcbc3zsrzdg56vqf0y"))

(define rust-windows-targets-0.52.6
  (crate-source "windows-targets" "0.52.6"
                "0wwrx625nwlfp7k93r2rra568gad1mwd888h1jwnl0vfg5r4ywlv"))

(define rust-windows-x86-64-gnu-0.52.6
  (crate-source "windows_x86_64_gnu" "0.52.6"
                "0y0sifqcb56a56mvn7xjgs8g43p33mfqkd8wj1yhrgxzma05qyhl"
                #:snippet '(delete-file-recursively "lib")))

(define rust-windows-x86-64-gnullvm-0.52.6
  (crate-source "windows_x86_64_gnullvm" "0.52.6"
                "03gda7zjx1qh8k9nnlgb7m3w3s1xkysg55hkd1wjch8pqhyv5m94"
                #:snippet '(delete-file-recursively "lib")))

(define rust-windows-x86-64-msvc-0.52.6
  (crate-source "windows_x86_64_msvc" "0.52.6"
                "1v7rb5cibyzx8vak29pdrk8nx9hycsjs4w0jgms08qk49jl6v7sq"
                #:snippet '(delete-file-recursively "lib")))

(define ssss-separator 'end-of-crates)


;;;
;;; Cargo inputs.
;;;

(define-cargo-inputs lookup-cargo-inputs
                     (rust-ring-0.17 =>
                                     (list rust-bumpalo-3.17.0
                                      rust-cc-1.2.18
                                      rust-cfg-if-1.0.0
                                      rust-getrandom-0.2.15
                                      rust-js-sys-0.3.77
                                      rust-libc-0.2.171
                                      rust-log-0.4.27
                                      rust-minicov-0.3.7
                                      rust-once-cell-1.21.3
                                      rust-proc-macro2-1.0.94
                                      rust-quote-1.0.40
                                      rust-same-file-1.0.6
                                      rust-shlex-1.3.0
                                      rust-spin-0.9.8
                                      rust-syn-2.0.100
                                      rust-unicode-ident-1.0.18
                                      rust-untrusted-0.9.0
                                      rust-walkdir-2.5.0
                                      rust-wasi-0.11.0+wasi-snapshot-preview1
                                      rust-wasm-bindgen-0.2.100
                                      rust-wasm-bindgen-backend-0.2.100
                                      rust-wasm-bindgen-futures-0.4.50
                                      rust-wasm-bindgen-macro-0.2.100
                                      rust-wasm-bindgen-macro-support-0.2.100
                                      rust-wasm-bindgen-shared-0.2.100
                                      rust-wasm-bindgen-test-0.3.50
                                      rust-wasm-bindgen-test-macro-0.3.50
                                      rust-web-sys-0.3.77
                                      rust-winapi-util-0.1.9
                                      rust-windows-sys-0.52.0
                                      rust-windows-sys-0.59.0
                                      rust-windows-targets-0.52.6
                                      rust-windows-aarch64-gnullvm-0.52.6
                                      rust-windows-aarch64-msvc-0.52.6
                                      rust-windows-i686-gnu-0.52.6
                                      rust-windows-i686-gnullvm-0.52.6
                                      rust-windows-i686-msvc-0.52.6
                                      rust-windows-x86-64-gnu-0.52.6
                                      rust-windows-x86-64-gnullvm-0.52.6
                                      rust-windows-x86-64-msvc-0.52.6)))
