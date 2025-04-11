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

(define rust-ab-glyph-rasterizer-0.1.8
  (crate-source "ab_glyph_rasterizer" "0.1.8"
                "0ikhgzig59q8b1a1iw83sxfnvylg5gx6w2y8ynbnf231xs9if6y7"))

(define rust-abscissa-core-0.8.2
  (crate-source "abscissa_core" "0.8.2"
                "0fvpm79dnwh1lj5kpw1bs3sfs8drcwbvbic6vrmjsh34v1x1i0rh"))

(define rust-abscissa-derive-0.8.2
  (crate-source "abscissa_derive" "0.8.2"
                "1qkzz20v71y26id2sfcdfc3jhgzf4gihf6g07x1xmx1f3mi19n88"))

(define rust-addr2line-0.21.0
  (crate-source "addr2line" "0.21.0"
                "1jx0k3iwyqr8klqbzk6kjvr496yd94aspis10vwsj5wy7gib4c4a"))

(define rust-addr2line-0.24.2
  (crate-source "addr2line" "0.24.2"
                "1hd1i57zxgz08j6h5qrhsnm2fi0bcqvsh389fw400xm3arz2ggnz"))

(define rust-adler-1.0.2
  (crate-source "adler" "1.0.2"
                "1zim79cvzd5yrkzl3nyfx0avijwgk9fqv3yrscdy1cc79ih02qpj"))

(define rust-adler2-2.0.0
  (crate-source "adler2" "2.0.0"
                "09r6drylvgy8vv8k20lnbvwq8gp09h7smfn6h1rxsy15pgh629si"))

(define rust-adler32-1.2.0
  (crate-source "adler32" "1.2.0"
                "0d7jq7jsjyhsgbhnfq5fvrlh9j0i9g1fqrl2735ibv5f75yjgqda"))

(define rust-aead-0.5.2
  (crate-source "aead" "0.5.2"
                "1c32aviraqag7926xcb9sybdm36v5vh9gnxpn4pxdwjc50zl28ni"))

(define rust-aes-0.8.4
  (crate-source "aes" "0.8.4"
                "1853796anlwp4kqim0s6wm1srl4ib621nm0cl2h3c8klsjkgfsdi"
                #:snippet '(delete-file-recursively "tests")))

(define rust-aes-gcm-0.10.3
  (crate-source "aes-gcm" "0.10.3"
                "1lgaqgg1gh9crg435509lqdhajg1m2vgma6f7fdj1qa2yyh10443"
                #:snippet '(delete-file-recursively "tests")))

(define rust-ahash-0.8.11
  (crate-source "ahash" "0.8.11"
                "04chdfkls5xmhp1d48gnjsmglbqibizs3bpbj6rsj604m10si7g8"))

(define rust-aho-corasick-1.1.3
  (crate-source "aho-corasick" "1.1.3"
                "05mrpkvdgp5d20y2p989f187ry9diliijgwrs254fs9s1m1x6q4f"))

(define rust-alga-0.9.3
  (crate-source "alga" "0.9.3"
                "1wl4z8ini9269x04g8wwdz1nn3hmmvaaysq4jwhymikyg81kv0jg"))

(define rust-aliasable-0.1.3
  (crate-source "aliasable" "0.1.3"
                "1z8548zdjlm4ps1k0d7x68lfdyji02crwcc9rw3q3bb106f643r5"))

(define rust-aligned-vec-0.5.0
  (crate-source "aligned-vec" "0.5.0"
                "1lb8qjqfap028ylf8zap6rkwrnrqimc3v6h3cixycjrdx1y0vaaa"))

(define rust-alloc-no-stdlib-2.0.4
  (crate-source "alloc-no-stdlib" "2.0.4"
                "1cy6r2sfv5y5cigv86vms7n5nlwhx1rbyxwcraqnmm1rxiib2yyc"))

(define rust-alloc-stdlib-0.2.2
  (crate-source "alloc-stdlib" "0.2.2"
                "1kkfbld20ab4165p29v172h8g0wvq8i06z8vnng14whw0isq5ywl"))

(define rust-alloca-0.4.0
  (crate-source "alloca" "0.4.0"
                "1x6p4387rz6j7h342kp3b7bgvqzyl9mibf959pkfk9xflrgd19z5"))

(define rust-allocator-api2-0.2.21
  (crate-source "allocator-api2" "0.2.21"
                "08zrzs022xwndihvzdn78yqarv2b9696y67i6h78nla3ww87jgb8"))

(define rust-alphanumeric-sort-1.5.3
  (crate-source "alphanumeric-sort" "1.5.3"
                "13vyx3cqpylvc0md4563rd42b7dvk3fv4wny0kpcc48gy72n0z6n"))

(define rust-android-activity-0.6.0
  (crate-source "android-activity" "0.6.0"
                "0inh88x8x2fh62jg739s9hwyvdh8i920qf0qw7bhr802j9c7hsgg"))

(define rust-android-properties-0.2.2
  (crate-source "android-properties" "0.2.2"
                "016slvg269c0y120p9qd8vdfqa2jbw4j0g18gfw6p3ain44v4zpw"))

(define rust-android-system-properties-0.1.5
  (crate-source "android_system_properties" "0.1.5"
                "04b3wrz12837j7mdczqd95b732gw5q7q66cv4yn4646lvccp57l1"))

(define rust-android-tzdata-0.1.1
  (crate-source "android-tzdata" "0.1.1"
                "1w7ynjxrfs97xg3qlcdns4kgfpwcdv824g611fq32cag4cdr96g9"))

(define rust-anes-0.1.6
  (crate-source "anes" "0.1.6"
                "16bj1ww1xkwzbckk32j2pnbn5vk6wgsl3q4p3j9551xbcarwnijb"))

(define rust-annotate-snippets-0.11.5
  (crate-source "annotate-snippets" "0.11.5"
                "1i1bmr5vy957l8fvivj9x1xs24np0k56rdgwj0bxqk45b2p8w3ki"))

(define rust-annotate-snippets-0.9.2
  (crate-source "annotate-snippets" "0.9.2"
                "07p8r6jzb7nqydq0kr5pllckqcdxlyld2g275v425axnzffpxbyc"))

(define rust-ansi-str-0.8.0
  (crate-source "ansi-str" "0.8.0"
                "07ddhqynv05xjyhw295w29qy77fi84sh5p2mm46ap0d94s4mgx0w"))

(define rust-ansi-term-0.12.1
  (crate-source "ansi_term" "0.12.1"
                "1ljmkbilxgmhavxvxqa7qvm6f3fjggi7q2l3a72q9x0cxjvrnanm"))

(define rust-ansitok-0.2.0
  (crate-source "ansitok" "0.2.0"
                "10vc2d1325qsbvbnqnj48zg55wv7jz929drx9vpdscdvl7k48012"))

(define rust-anstream-0.6.18
  (crate-source "anstream" "0.6.18"
                "16sjk4x3ns2c3ya1x28a44kh6p47c7vhk27251i015hik1lm7k4a"))

(define rust-anstyle-1.0.10
  (crate-source "anstyle" "1.0.10"
                "1yai2vppmd7zlvlrp9grwll60knrmscalf8l2qpfz8b7y5lkpk2m"))

(define rust-anstyle-parse-0.2.6
  (crate-source "anstyle-parse" "0.2.6"
                "1acqayy22fwzsrvr6n0lz6a4zvjjcvgr5sm941m7m0b2fr81cb9v"))

(define rust-anstyle-query-1.1.2
  (crate-source "anstyle-query" "1.1.2"
                "036nm3lkyk43xbps1yql3583fp4hg3b1600is7mcyxs1gzrpm53r"))

(define rust-anstyle-wincon-3.0.7
  (crate-source "anstyle-wincon" "3.0.7"
                "0kmf0fq4c8yribdpdpylzz1zccpy84hizmcsac3wrac1f7kk8dfa"))

(define rust-anyhow-1.0.97
  (crate-source "anyhow" "1.0.97"
                "0kvspbiwncmmkdgrwjrimsmbmhzxc641p5ql99l2rjq6smmdbznw"))

(define rust-aom-sys-0.3.3
  (crate-source "aom-sys" "0.3.3"
                "0bc1dzl3c95s44q7c1i0vnj7fhiqf44in8w22nw5vmp1vgbpadk2"))

(define rust-aperture-0.3.2
  (crate-source "aperture" "0.3.2"
                "02bjzskxp91br91yvf5f32wakp1i9948sxbsy9hdrxs52w38hr61"))

(define rust-appendlist-1.4.0
  (crate-source "appendlist" "1.4.0"
                "1lnbl7mc7capcqj1z1ylxvm4h492sb9sr8pzww3q6lrhrmrxqjg1"))

(define rust-approx-0.3.2
  (crate-source "approx" "0.3.2"
                "1hx580xjdxl3766js9b49rnbnmr8gw8c060809l43k9f0xshprph"))

(define rust-approx-0.4.0
  (crate-source "approx" "0.4.0"
                "0y52dg58lapl4pp1kqlznfw1blbki0nx6b0aw8kja2yi3gyhaaiz"))

(define rust-approx-0.5.1
  (crate-source "approx" "0.5.1"
                "1ilpv3dgd58rasslss0labarq7jawxmivk17wsh8wmkdm3q15cfa"))

(define rust-arbitrary-1.4.1
  (crate-source "arbitrary" "1.4.1"
                "08zj2yanll5s5gsbmvgwvbq39iqzy3nia3yx3db3zwba08yhpqnx"))

(define rust-arboard-3.5.0
  (crate-source "arboard" "3.5.0"
                "0w1yqcx51153hy5w3y0702xjc9nmlhncw9f5l0rdwbl62pvj3py1"))

(define rust-arc-swap-1.7.1
  (crate-source "arc-swap" "1.7.1"
                "0mrl9a9r9p9bln74q6aszvf22q1ijiw089jkrmabfqkbj31zixv9"))

(define rust-archery-1.2.1
  (crate-source "archery" "1.2.1"
                "0sdqlmybcvd0rzv22ac3k3xxm5anr1gpm03sf02iy0jmrlhyvqpa"))

(define rust-arcstr-1.2.0
  (crate-source "arcstr" "1.2.0"
                "0vbyslhqr5fh84w5dd2hqck5y5r154p771wqddfah0bpplyqr483"))

(define rust-arg-enum-proc-macro-0.3.4
  (crate-source "arg_enum_proc_macro" "0.3.4"
                "1sjdfd5a8j6r99cf0bpqrd6b160x9vz97y5rysycsjda358jms8a"))

(define rust-array-macro-1.0.5
  (crate-source "array-macro" "1.0.5"
                "19mdx2xlppnqwl6rhsbzylx61a0kkp2ql8q16195b7iga977ps86"))

(define rust-arraydeque-0.5.1
  (crate-source "arraydeque" "0.5.1"
                "0dn2xdfg3rkiqsh8a6achnmvf5nf11xk33xgjzpksliab4yjx43x"))

(define rust-arrayref-0.3.9
  (crate-source "arrayref" "0.3.9"
                "1jzyp0nvp10dmahaq9a2rnxqdd5wxgbvp8xaibps3zai8c9fi8kn"))

(define rust-arrayvec-0.5.2
  (crate-source "arrayvec" "0.5.2"
                "12q6hn01x5435bprwlb7w9m7817dyfq55yrl4psygr78bp32zdi3"))

(define rust-arrayvec-0.7.6
  (crate-source "arrayvec" "0.7.6"
                "0l1fz4ccgv6pm609rif37sl5nv5k6lbzi7kkppgzqzh1vwix20kw"))

(define rust-as-raw-xcb-connection-1.0.1
  (crate-source "as-raw-xcb-connection" "1.0.1"
                "0sqgpz2ymv5yx76r5j2npjq2x5qvvqnw0vrs35cyv30p3pfp2m8p"))

(define rust-ash-0.38.0+1.3.281
  (crate-source "ash" "0.38.0+1.3.281"
                "0vx4yf689v1rc680jvy8bnysx5sgd8f33wnp2vqaizh0v0v4kd0b"))

(define rust-ashpd-0.6.8
  (crate-source "ashpd" "0.6.8"
                "109d7w6v0rnpy9lv4kmhwgh0sff0440s2vybj1k0ik4ib3d2xhja"))

(define rust-asn1-0.20.0
  (crate-source "asn1" "0.20.0"
                "0ckg83ingvagwjvmxadjjmkgna5kvlvrfx9arlfvzqhxxas892rd"))

(define rust-asn1-derive-0.20.0
  (crate-source "asn1_derive" "0.20.0"
                "1b88xsqmxpxjq4p2mrn1icj7c3k2s041v7wqp8yhnqiq06fq0052"))

(define rust-assert-cmd-2.0.16
  (crate-source "assert_cmd" "2.0.16"
                "0gdj0710k3lnvyjmpv8a4dgwrk9ib85l2wfw4n2xwy3qyavka66w"))

(define rust-assert-fs-1.1.2
  (crate-source "assert_fs" "1.1.2"
                "0x3nj817l5kbpmr42habqv5i49rpxdpncmr86ix840knnkyv3zby"))

(define rust-assert-json-diff-2.0.2
  (crate-source "assert-json-diff" "2.0.2"
                "04mg3w0rh3schpla51l18362hsirl23q93aisws2irrj32wg5r27"))

(define rust-astral-tokio-tar-0.5.2
  (crate-source "astral-tokio-tar" "0.5.2"
                "16wip9bzzbjkpf1rgs8cjbfmnhxflrdi35xpb53yrncrl7xjpfqs"
                #:snippet '(delete-file-recursively "tests")))

(define rust-async-broadcast-0.5.1
  (crate-source "async-broadcast" "0.5.1"
                "0avdqbci1qdlfc4glc3wqrb0wi5ffc7bqv2q1wg14syayvdwqj3w"))

(define rust-async-broadcast-0.7.2
  (crate-source "async-broadcast" "0.7.2"
                "0ckmqcwyqwbl2cijk1y4r0vy60i89gqc86ijrxzz5f2m4yjqfnj3"))

(define rust-async-channel-2.3.1
  (crate-source "async-channel" "2.3.1"
                "0skvwxj6ysfc6d7bhczz9a2550260g62bm5gl0nmjxxyn007id49"))

(define rust-async-compression-0.4.19
  (crate-source "async-compression" "0.4.19"
                "0g006fvpri6drd0k8ds144gg9yxzm9xi14hwcr90yn3kjrm5wmq6"
                #:snippet '(delete-file-recursively "tests")))

(define rust-async-compression-0.4.22
  (crate-source "async-compression" "0.4.22"
                "0r6shv717rl3qzccjc9qlxmnaj3l22rr9197jsahkn33v7wr98ar"
                #:snippet '(delete-file-recursively "tests")))

(define rust-async-executor-1.13.1
  (crate-source "async-executor" "1.13.1"
                "1v6w1dbvsmw6cs4dk4lxj5dvrikc6xi479wikwaab2qy3h09mjih"))

(define rust-async-fs-2.1.2
  (crate-source "async-fs" "2.1.2"
                "0jp0p7lg9zqy2djgdmivbzx0yqmfn9sm2s9dkhaws3zlharhkkgb"))

(define rust-async-http-range-reader-0.9.1
  (crate-source "async_http_range_reader" "0.9.1"
                "15s16da73xw2vl8z3iyh1y01na6dijzwmmzm0qz98gwy4q07qlrb"))

(define rust-async-io-1.13.0
  (crate-source "async-io" "1.13.0"
                "1byj7lpw0ahk6k63sbc9859v68f28hpaab41dxsjj1ggjdfv9i8g"))

(define rust-async-io-2.4.0
  (crate-source "async-io" "2.4.0"
                "0n8h0vy53n4vdkq529scqnkzm9vcl3r73za9nj81s2nfrhiv78j3"))

(define rust-async-lock-2.8.0
  (crate-source "async-lock" "2.8.0"
                "0asq5xdzgp3d5m82y5rg7a0k9q0g95jy6mgc7ivl334x7qlp4wi8"))

(define rust-async-lock-3.4.0
  (crate-source "async-lock" "3.4.0"
                "060vh45i809wcqyxzs5g69nqiqah7ydz0hpkcjys9258vqn4fvpz"))

(define rust-async-process-1.8.1
  (crate-source "async-process" "1.8.1"
                "126s968lvhg9rlwsnxp7wfzkfn7rl87p0dlvqqlibn081ax3hr7a"))

(define rust-async-process-2.3.0
  (crate-source "async-process" "2.3.0"
                "1fr6cpqdw7hrmzns1983lgx86cg8vyz7nlrn0h0125iqq8fmy9b3"))

(define rust-async-recursion-1.1.1
  (crate-source "async-recursion" "1.1.1"
                "04ac4zh8qz2xjc79lmfi4jlqj5f92xjvfaqvbzwkizyqd4pl4hrv"))

(define rust-async-signal-0.2.10
  (crate-source "async-signal" "0.2.10"
                "1wxrq3871l00mil43nmh0akvwjjjnv0bn7n2pzwbvh00k0s00zk3"))

(define rust-async-task-4.7.1
  (crate-source "async-task" "4.7.1"
                "1pp3avr4ri2nbh7s6y9ws0397nkx1zymmcr14sq761ljarh3axcb"))

(define rust-async-trait-0.1.88
  (crate-source "async-trait" "0.1.88"
                "1dgxvz7g75cmz6vqqz0mri4xazc6a8xfj1db6r9fxz29lzyd6fg5"))

(define rust-async-zip-0.0.17.c909fda
  (origin
    (method git-fetch)
    (uri (git-reference (url "https://github.com/charliermarsh/rs-async-zip")
                        (commit "c909fda63fcafe4af496a07bfda28a5aae97e58d")))
    (file-name (git-file-name "rust-async-zip" "0.0.17.c909fda"))
    (sha256 (base32 "1nk16a56fjjjd12rpg92d2az8y0cyvbsw6c4l42nf5rrjvcj1x2m"))
    (modules '((guix build utils)))
    (snippet '(for-each delete-file-recursively '("src/tests" "tests")))))

(define rust-atoi-2.0.0
  (crate-source "atoi" "2.0.0"
                "0a05h42fggmy7h0ajjv6m7z72l924i7igbx13hk9d8pyign9k3gj"))

(define rust-atomic-0.6.0
  (crate-source "atomic" "0.6.0"
                "15193mfhmrq3p6vi1a10hw3n6kvzf5h32zikhby3mdj0ww1q10cd"))

(define rust-atomic-polyfill-1.0.3
  (crate-source "atomic-polyfill" "1.0.3"
                "1x00ndablb89zvbr8m03cgjzgajg86fqn8pgz85yy2gy1pivrwlc"))

(define rust-atomic-refcell-0.1.13
  (crate-source "atomic_refcell" "0.1.13"
                "0z04ng59y22mwf315wamx78ybhjag0x6k7isc36hdgcv63c7rrj1"))

(define rust-atomic-waker-1.1.2
  (crate-source "atomic-waker" "1.1.2"
                "1h5av1lw56m0jf0fd3bchxq8a30xv0b4wv8s4zkp4s0i7mfvs18m"))

(define rust-atty-0.2.14
  (crate-source "atty" "0.2.14"
                "1s7yslcs6a28c5vz7jwj63lkfgyx8mx99fdirlhi9lbhhzhrpcyr"))

(define rust-auditable-extract-0.3.5
  (crate-source "auditable-extract" "0.3.5"
                "0yiy1ph2620qlwj4anijmzi6w8gaci6gximn8afa9pjrjygiwds4"))

(define rust-auditable-info-0.8.0
  (crate-source "auditable-info" "0.8.0"
                "1kbhc89fg4ykywwp2z810if1i48pqng6va2rnf79qwbxcq2ffscq"))

(define rust-auditable-serde-0.7.0
  (crate-source "auditable-serde" "0.7.0"
                "0qqnraspp6zvncmda8i6mx8lsj78gxqx9qwk50nmpibnmh660zpc"))

(define rust-autocfg-1.4.0
  (crate-source "autocfg" "1.4.0"
                "09lz3by90d2hphbq56znag9v87gfpd9gb8nr82hll8z6x2nhprdc"))

(define rust-autocompress-0.6.0
  (crate-source "autocompress" "0.6.0"
                "1rjpqvf5if087n1cijcxf5shw9cjv7nyhkxigr0zg9446p8hg2bi"))

(define rust-automod-1.0.15
  (crate-source "automod" "1.0.15"
                "0w76lzg8vbrvx4cl8x63kdaxkwpbp17c4cg1bcgwmqmj3lqbvd7b"))

(define rust-av-metrics-0.9.1
  (crate-source "av-metrics" "0.9.1"
                "0c1m5rrrx88y1hm4i17qh0fd2rqd3jwck86lj5dkw85hpmdyjv4r"))

(define rust-av1-grain-0.2.3
  (crate-source "av1-grain" "0.2.3"
                "1gvqdh21bm1cfqiwyiinbqi0mg7x2lg2fwgmphma8ijxijfr0y36"))

(define rust-avif-serialize-0.8.3
  (crate-source "avif-serialize" "0.8.3"
                "13k0sy5qd6pyvfqzbd06zadz5cavq36fxn391j10ijzv9im2v4lq"))

(define rust-axoasset-1.2.0
  (crate-source "axoasset" "1.2.0"
                "0hql04vlw1z5dnrdij1mkfi82gnvpq3ywrmps9rhjzx1za60k8av"
                #:snippet '(delete-file-recursively "tests")))

(define rust-axoprocess-0.2.0
  (crate-source "axoprocess" "0.2.0"
                "0zfs9fkn04jj8r30i470mqv28m6dp8q636cphxjrbvwab0h6kr2d"))

(define rust-axotag-0.2.0
  (crate-source "axotag" "0.2.0"
                "0m3c38gbz2dk42s5qlspqdayjnmg4mdgqhx76vgwnr1ynz0gm26q"))

(define rust-axoupdater-0.9.0
  (crate-source "axoupdater" "0.9.0"
                "1r6cmppqv57059vg22z43wa25ahnhx7s2gxy517vrpd8c3wll6dw"))

(define rust-backon-1.4.1
  (crate-source "backon" "1.4.1"
                "1drv0gvhjs3g0q88f1mknqjdyhh6qg8pvb9nkfasba011ibr23cp"))

(define rust-backtrace-0.3.71
  (crate-source "backtrace" "0.3.71"
                "17bgd7pbjb9gc8q47qwsg2lmy9i62x3bsjmmnjrwh5z8s805ic16"))

(define rust-backtrace-0.3.74
  (crate-source "backtrace" "0.3.74"
                "06pfif7nwx66qf2zaanc2fcq7m64i91ki9imw9xd3bnz5hrwp0ld"))

(define rust-backtrace-ext-0.2.1
  (crate-source "backtrace-ext" "0.2.1"
                "0l4xacjnx4jrn9k14xbs2swks018mviq03sp7c1gn62apviywysk"))

(define rust-base16ct-0.2.0
  (crate-source "base16ct" "0.2.0"
                "1kylrjhdzk7qpknrvlphw8ywdnvvg39dizw9622w3wk5xba04zsc"))

(define rust-base64-0.21.7
  (crate-source "base64" "0.21.7"
                "0rw52yvsk75kar9wgqfwgb414kvil1gn7mqkrhn9zf1537mpsacx"))

(define rust-base64-0.22.1
  (crate-source "base64" "0.22.1"
                "1imqzgh7bxcikp5vx3shqvw9j09g9ly0xr0jma0q66i52r7jbcvj"))

(define rust-base64ct-1.7.3
  (crate-source "base64ct" "1.7.3"
                "18scrpjl145msdb64q4nbb0plm4wbmp5lml134nz0c5rvxm5pql9"))

(define rust-bgzip-0.3.1
  (crate-source "bgzip" "0.3.1"
                "16zr2nclis3kgz0jxi7ayyk510ar5dvyfpf03fazajmn1ycdhkxn"))

(define rust-bincode-1.3.3
  (crate-source "bincode" "1.3.3"
                "1bfw3mnwzx5g1465kiqllp5n4r10qrqy88kdlp3jfwnq2ya5xx5i"))

(define rust-bindgen-0.65.1
  (crate-source "bindgen" "0.65.1"
                "1i9wci1h3xnk8hi7cf06capgifnmpk9dd59zqznh6jcsdx37ppyg"))

(define rust-bindgen-0.69.5
  (crate-source "bindgen" "0.69.5"
                "1240snlcfj663k04bjsg629g4wx6f83flgbjh5rzpgyagk3864r7"))

(define rust-bindgen-0.70.1
  (crate-source "bindgen" "0.70.1"
                "0vyf0jp6apcy9kjyz4s8vldj0xqycnbzb6zv3skkwiqdi3nqz7gl"))

(define rust-bindgen-0.71.1
  (crate-source "bindgen" "0.71.1"
                "1cynz43s9xwjbd1y03rx9h37xs0isyl8bi6g6yngp35nglyvyn2z"))

(define rust-binfarce-0.2.1
  (crate-source "binfarce" "0.2.1"
                "18hnqqir3gk5sx1mlndzgpxs0l4rviv7dk3h1piyspayp35lqihq"))

(define rust-bio-0.33.0
  (crate-source "bio" "0.33.0"
                "1zaghvazh243x2pg866hjx1zwwpp9qzwkf45iz2fs9af5y661gz5"))

(define rust-bio-0.39.2
  (crate-source "bio" "0.39.2"
                "00k0zv4yyiipzg2arnkrplfi0digwnv89bczfzhyva68yfv8scvb"))

(define rust-bio-1.6.0
  (crate-source "bio" "1.6.0"
                "1hpnfwjyqg23dlk75frydf9kgilccyn3haaw6xdwh25zpa9wnwks"))

(define rust-bio-types-0.13.0
  (crate-source "bio-types" "0.13.0"
                "102cn2kpp4ivj0ixcnc98m0nnbp6ff6xagfwk2jmywr81bs91afz"))

(define rust-bio-types-1.0.4
  (crate-source "bio-types" "1.0.4"
                "0zmdcvj44a088larkahcic5z61cwn2x80iym0w14albzid7zbp7l"))

(define rust-biquad-0.4.2
  (crate-source "biquad" "0.4.2"
                "0gpc13lag439nmq077wfwz055qbjaxbpk7znvnbddbg3wgsj81c2"))

(define rust-bisection-0.1.0
  (crate-source "bisection" "0.1.0"
                "1hx80j7lmj3mg093psrnf5llmyksqg078jsbrzkcq3mb3fd0f7h2"))

(define rust-bit-field-0.10.2
  (crate-source "bit_field" "0.10.2"
                "0qav5rpm4hqc33vmf4vc4r0mh51yjx5vmd9zhih26n9yjs3730nw"))

(define rust-bit-set-0.5.3
  (crate-source "bit-set" "0.5.3"
                "1wcm9vxi00ma4rcxkl3pzzjli6ihrpn9cfdi0c5b4cvga2mxs007"))

(define rust-bit-set-0.8.0
  (crate-source "bit-set" "0.8.0"
                "18riaa10s6n59n39vix0cr7l2dgwdhcpbcm97x1xbyfp1q47x008"))

(define rust-bit-vec-0.6.3
  (crate-source "bit-vec" "0.6.3"
                "1ywqjnv60cdh1slhz67psnp422md6jdliji6alq0gmly2xm9p7rl"))

(define rust-bit-vec-0.8.0
  (crate-source "bit-vec" "0.8.0"
                "1xxa1s2cj291r7k1whbxq840jxvmdsq9xgh7bvrxl46m80fllxjy"))

(define rust-bitfield-0.14.0
  (crate-source "bitfield" "0.14.0"
                "1b26k9acwss4qvrl60lf9n83l17d4hj47n5rmpd3iigf9j9n0zid"))

(define rust-bitflags-1.3.2
  (crate-source "bitflags" "1.3.2"
                "12ki6w8gn1ldq7yz9y680llwk5gmrhrzszaa17g1sbrw2r2qvwxy"))

(define rust-bitflags-2.9.0
  (crate-source "bitflags" "2.9.0"
                "1gb5w7pxnmx8l2bjz1i6rkbwbm2167k294rhy6cl1y3vbc8i90jw"))

(define rust-bitmaps-2.1.0
  (crate-source "bitmaps" "2.1.0"
                "18k4mcwxl96yvii5kcljkpb8pg5j4jj1zbsdn26nsx4r83846403"))

(define rust-bitstream-io-2.6.0
  (crate-source "bitstream-io" "2.6.0"
                "1cli390l1dhp9skygyjjnqvczp36b7f31mkx9ry3dg26330cv6b0"))

(define rust-bitvec-1.0.1
  (crate-source "bitvec" "1.0.1"
                "173ydyj2q5vwj88k6xgjnfsshs4x9wbvjjv7sm0h36r34hn87hhv"))

(define rust-blake2b-simd-1.0.3
  (crate-source "blake2b_simd" "1.0.3"
                "16cxmm4pr5jx9xc0msj1qn018ram3vz9k17cj57r97hm1fi07s86"))

(define rust-blake3-1.8.1
  (crate-source "blake3" "1.8.1"
                "1czffygg8dhdsjyzydsrf50harfrralrkm10ckhkja1i6jdhk6iq"))

(define rust-block-0.1.6
  (crate-source "block" "0.1.6"
                "16k9jgll25pzsq14f244q22cdv0zb4bqacldg3kx6h89d7piz30d"))

(define rust-block-buffer-0.10.4
  (crate-source "block-buffer" "0.10.4"
                "0w9sa2ypmrsqqvc20nhwr75wbb5cjr4kkyhpjm1z1lv2kdicfy1h"))

(define rust-block-padding-0.3.3
  (crate-source "block-padding" "0.3.3"
                "14wdad0r1qk5gmszxqd8cky6vx8qg7c153jv981mixzrpzmlz2d8"))

(define rust-block2-0.5.1
  (crate-source "block2" "0.5.1"
                "0pyiha5his2grzqr3mynmq244laql2j20992i59asp0gy7mjw4rc"))

(define rust-blocking-1.6.1
  (crate-source "blocking" "1.6.1"
                "1si99l8zp7c4zq87y35ayjgc5c9b60jb8h0k14zfcs679z2l2gvh"))

(define rust-borsh-1.5.7
  (crate-source "borsh" "1.5.7"
                "1kikljm5yr3l9qsw5xvdccragxj4445s4s3fqsgy6hmmipwld1md"))

(define rust-boxcar-0.2.11
  (crate-source "boxcar" "0.2.11"
                "1gip12s87j97dxh94q4jjdcykbi6714wf52jqdbzlq33zkicch37"))

(define rust-bracoxide-0.1.5
  (crate-source "bracoxide" "0.1.5"
                "0alnn0lkfyfk5i1dd0ykqpw4zli6az7bdkslh98xga8s90f9jlkz"))

(define rust-brotli-7.0.0
  (crate-source "brotli" "7.0.0"
                "1g99xay61mds9d23fnfj5gfbd6g11gihfgs3y1abljwldzqvi5yc"
                #:snippet '(delete-file-recursively "testdata")))

(define rust-brotli-decompressor-4.0.2
  (crate-source "brotli-decompressor" "4.0.2"
                "0dqb0vbhrc77c09qf6qmbylgkfnbjaq8629qp0z42gc0gnnhbykl"))

(define rust-bstr-1.11.3
  (crate-source "bstr" "1.11.3"
                "1q3g2wmrvclgx7lk2p6mpzhqxzx41hyg962gkmlyxql1liar26jk"))

(define rust-bstr-1.12.0
  (crate-source "bstr" "1.12.0"
                "195i0gd7r7jg7a8spkmw08492n7rmiabcvz880xn2z8dkp8i6h93"))

(define rust-buffer-redux-1.0.2
  (crate-source "buffer-redux" "1.0.2"
                "1waq39blrj7j6qp1sp2fvplwmq10yhks7fgbsdy8kxdrqn3wz2jf"))

(define rust-built-0.7.7
  (crate-source "built" "0.7.7"
                "0ywn0m11xm80pg6zrzq3sdj3vmzg3qs6baqnvfmkd377ly8n3van"))

(define rust-bumpalo-3.17.0
  (crate-source "bumpalo" "3.17.0"
                "1gxxsn2fsjmv03g8p3m749mczv2k4m8xspifs5l7bcx0vx3gna0n"))

(define rust-bv-0.11.1
  (crate-source "bv" "0.11.1"
                "0h5kbl54fsccznfixw83xndbripw39y2qkqjwf709p75iqfvnd48"))

(define rust-bytecheck-0.8.1
  (crate-source "bytecheck" "0.8.1"
                "18yisf4zvvhhpv987ic5js6arwj69h44c9rpa0szxf8g6yrhysah"))

(define rust-bytecheck-derive-0.8.1
  (crate-source "bytecheck_derive" "0.8.1"
                "0wfbjc1vqkmszx99y6hghlcnk8xgxlhyfsgc5mf3b05i1ip89dzg"))

(define rust-bytecount-0.6.8
  (crate-source "bytecount" "0.6.8"
                "1klqfjwn41fwmcqw4z03v6i4imgrf7lmf3b5s9v74hxir8hrps2w"))

(define rust-bytemuck-1.22.0
  (crate-source "bytemuck" "1.22.0"
                "0h6m8wh7iw98cn69k53plbyqff78c2yrs32l0fy4wqdcvc8grcdn"))

(define rust-bytemuck-derive-1.9.3
  (crate-source "bytemuck_derive" "1.9.3"
                "18g1r1zgwiz5px2kf1n55ibjb2aqm86nkw28ss1mn85k94xjgk3y"))

(define rust-byteorder-1.5.0
  (crate-source "byteorder" "1.5.0"
                "0jzncxyf404mwqdbspihyzpkndfgda450l0893pz5xj685cg5l0z"))

(define rust-byteorder-lite-0.1.0
  (crate-source "byteorder-lite" "0.1.0"
                "15alafmz4b9az56z6x7glcbcb6a8bfgyd109qc3bvx07zx4fj7wg"))

(define rust-bytes-1.10.1
  (crate-source "bytes" "1.10.1"
                "0smd4wi2yrhp5pmq571yiaqx84bjqlm1ixqhnvfwzzc6pqkn26yp"))

(define rust-bytesize-1.3.3
  (crate-source "bytesize" "1.3.3"
                "0nb645ma48nwsv1piylzcza0avjp435sl8krhyws3q18kv5ap4rf"))

(define rust-bzip2-0.4.4
  (crate-source "bzip2" "0.4.4"
                "1y27wgqkx3k2jmh4k26vra2kqjq1qc1asww8hac3cv1zxyk1dcdx"
                #:snippet '(delete-file-recursively "tests")))

(define rust-bzip2-0.5.2
  (crate-source "bzip2" "0.5.2"
                "0iya6nbj0p2y8jss0z05yncc5hadry164fw3zva01y06v4igpv29"
                #:snippet '(delete-file-recursively "tests")))

(define rust-bzip2-rs-0.1.2
  (crate-source "bzip2-rs" "0.1.2"
                "0dgp83kixqrqj6q6574qr5zsfpbsiiwhqs3krhvsn4f8wkkmksxy"
                #:snippet '(delete-file-recursively "tests")))

(define rust-bzip2-sys-0.1.13+1.0.8
  (crate-source "bzip2-sys" "0.1.13+1.0.8"
                "056c39pgjh4272bdslv445f5ry64xvb0f7nph3z7860ln8rzynr2"
                #:snippet
                '(begin
                   (delete-file-recursively "bzip2-1.0.8")
                   (delete-file "build.rs")
                   ;; Inspired by Debian's patch.
                   (with-output-to-file "build.rs"
                     (lambda _
                       (format #t "fn main() {~@
                        println!(\"cargo:rustc-link-lib=bz2\");~@
                        }~%"))))))

(define rust-c2rust-ast-builder-0.20.0
  (crate-source "c2rust-ast-builder" "0.20.0"
                "1kk7vm9zmsnavslmblcp3pgl4rc9kbsynm6yykvhjfjcd0fgd5vs"))

(define rust-c2rust-ast-exporter-0.20.0
  (crate-source "c2rust-ast-exporter" "0.20.0"
                "0np9n0k2fw10l07w23yzi58vv15npqcr01rayf5n4rlmhafm6s48"))

(define rust-c2rust-ast-printer-0.20.0
  (crate-source "c2rust-ast-printer" "0.20.0"
                "13n6krmvrrmkrk7sq65iwndbqvxsjri7ipk6hdzc3gv151w6rmv4"))

(define rust-c2rust-bitfields-0.20.0
  (crate-source "c2rust-bitfields" "0.20.0"
                "1pqp22mr43m1v3vcjakc8j0q51b66jbnkp5jgvab7l50zwmpvp26"))

(define rust-c2rust-bitfields-derive-0.20.0
  (crate-source "c2rust-bitfields-derive" "0.20.0"
                "1y1kad9kim7f4w1khx9g4m585lafx2hizqrl00lcwdsrz9x13qgb"))

(define rust-c2rust-build-paths-0.20.0
  (crate-source "c2rust-build-paths" "0.20.0"
                "1y7dljrgrfjn5mgydlqh5f0m1576brc133pjqzw2c19vly1rba16"))

(define rust-c2rust-transpile-0.20.0
  (crate-source "c2rust-transpile" "0.20.0"
                "0801i05pmsp8nvw0r4nx8pnircsdsjb9764bc5gv7sh6wmaavdyz"))

(define rust-cairo-rs-0.18.5
  (crate-source "cairo-rs" "0.18.5"
                "1qjfkcq3mrh3p01nnn71dy3kn99g21xx3j8xcdvzn8ll2pq6x8lc"))

(define rust-cairo-rs-0.19.4
  (crate-source "cairo-rs" "0.19.4"
                "0qp5rixgipdj9d8yd5458hzfxam1rgpzcxi90vq6q0v91r6jmb5j"))

(define rust-cairo-rs-0.20.7
  (crate-source "cairo-rs" "0.20.7"
                "1xy02qa4mn9bwnhsbmkry4yjz230r66nvrkh4fn9dkw61m8val5f"))

(define rust-cairo-sys-rs-0.18.2
  (crate-source "cairo-sys-rs" "0.18.2"
                "0lfsxl7ylw3phbnwmz3k58j1gnqi6kc2hdc7g3bb7f4hwnl9yp38"))

(define rust-cairo-sys-rs-0.19.2
  (crate-source "cairo-sys-rs" "0.19.2"
                "0r0yp0lph77lm4blrn6fvdmz2i3r8ibkkjg6nmwbvvv4jq8v6fzx"))

(define rust-cairo-sys-rs-0.20.7
  (crate-source "cairo-sys-rs" "0.20.7"
                "1pwh4b4mdsipjl9lrg5p5bygbdk11kz6m5y7mbrb0ziwwjw6p2zi"))

(define rust-calamine-0.26.1
  (crate-source "calamine" "0.26.1"
                "1cbfjwb37c28gkb42wsgpp93fxsrzhxgjjza0hc7yp9cmywld1hk"))

(define rust-calloop-0.13.0
  (crate-source "calloop" "0.13.0"
                "1v5zgidnhsyml403rzr7vm99f8q6r5bxq5gxyiqkr8lcapwa57dr"))

(define rust-calloop-0.14.2
  (crate-source "calloop" "0.14.2"
                "1jzx8rmgndj1br4gnd4iaxayqi79g897lz6qdy2l670xcqj9g4hh"))

(define rust-calloop-0.9.3
  (crate-source "calloop" "0.9.3"
                "10mbcsd7fj3cg0a463h3003wycv955cnj4pm2gla2sp5xxhyqbmz"))

(define rust-calloop-wayland-source-0.3.0
  (crate-source "calloop-wayland-source" "0.3.0"
                "086x5mq16prrcwd9k6bw9an0sp8bj9l5daz4ziz5z4snf2c6m9lm"))

(define rust-calloop-wayland-source-0.4.0
  (crate-source "calloop-wayland-source" "0.4.0"
                "1bsxx4dz4k4icza63w108n8s1agm7890nl3syigaa9p0pcfplsl7"))

(define rust-camino-1.1.9
  (crate-source "camino" "1.1.9"
                "1lqszl12l1146jf8g01rvjmapif82mhzih870ln3x0dmcr4yr5lb"))

(define rust-canonical-path-2.0.2
  (crate-source "canonical-path" "2.0.2"
                "0vvsjda6ka5nz8zvx6r08zqi0j59sjccgcbjxj96xj764w9y1sg6"))

(define rust-cargo-0.85.0
  (crate-source "cargo" "0.85.0"
                "05n42kxzxhkfj4s2jg2qcw759h2b3piai6p1fm90kx17jhlg9vxv"
                #:snippet '(for-each delete-file-recursively '("benches" "tests"))))

(define rust-cargo-config2-0.1.32
  (crate-source "cargo-config2" "0.1.32"
                "0qf4kkbh3m47n6s3scaqjr41ysn3n153wyhy3yckqhp06sd79hvd"))

(define rust-cargo-credential-0.4.8
  (crate-source "cargo-credential" "0.4.8"
                "0anzvfk11fc1l72h2cm2q9b0i680qk98864h1qcxpqfx184ga7mc"))

(define rust-cargo-credential-libsecret-0.4.12
  (crate-source "cargo-credential-libsecret" "0.4.12"
                "1f2ijp1k4rr7yjk9rppwgga6f2ppsmm6yx9yh0qb85g8dk1dpn78"))

(define rust-cargo-credential-macos-keychain-0.4.12
  (crate-source "cargo-credential-macos-keychain" "0.4.12"
                "0a5ggaba8wb9fj785fq5v2y67r95fw7ips2z1c22rqrvwvdcp2dl"))

(define rust-cargo-credential-wincred-0.4.12
  (crate-source "cargo-credential-wincred" "0.4.12"
                "0jrg7d3pnx6rrj2brp53jwfjcb2svcvc8yq7316pl9m5alicpij9"))

(define rust-cargo-lock-10.1.0
  (crate-source "cargo-lock" "10.1.0"
                "0m74y8w9wn7rl5mpzr0436r6fshf3qhm7d3wl02s4ys0f57wnsn0"))

(define rust-cargo-metadata-0.14.2
  (crate-source "cargo_metadata" "0.14.2"
                "1yl1y40vby9cas4dlfc44szrbl4m4z3pahv3p6ckdqp8ksfv1jsa"))

(define rust-cargo-metadata-0.18.1
  (crate-source "cargo_metadata" "0.18.1"
                "0drh0zndl4qgndy6kg6783cydbvhxgv0hcg7d9hhqx0zwi3nb21d"))

(define rust-cargo-metadata-0.19.2
  (crate-source "cargo_metadata" "0.19.2"
                "1fkr8jp6vhva4kc3rhx13yrnl8g3zch463j20vbwa9scxlabcpnx"))

(define rust-cargo-options-0.7.5
  (crate-source "cargo-options" "0.7.5"
                "0wc1qy1plwp6i0g5p74bnvy545xk7hccvv68lmmg4739g0ay923l"
                #:snippet '(delete-file-recursively "tests")))

(define rust-cargo-platform-0.1.9
  (crate-source "cargo-platform" "0.1.9"
                "1sinpmqjdk3q9llbmxr0h0nyvqrif1r5qs34l000z73b024z2np3"))

(define rust-cargo-util-0.2.19
  (crate-source "cargo-util" "0.2.19"
                "02m2xjvq02iiz9dp9v6m792w55jrka3vwll0ca82wjc09qm6wzsj"))

(define rust-cargo-util-schemas-0.7.2
  (crate-source "cargo-util-schemas" "0.7.2"
                "014qha8zp604n7yh2scbn2jz0q2ki6jmi61db61832nbz1l5z44z"))

(define rust-cassowary-0.3.0
  (crate-source "cassowary" "0.3.0"
                "0lvanj0gsk6pc1chqrh4k5k0vi1rfbgzmsk46dwy3nmrqyw711nz"))

(define rust-cast-0.3.0
  (crate-source "cast" "0.3.0"
                "1dbyngbyz2qkk0jn2sxil8vrz3rnpcj142y184p9l4nbl9radcip"))

(define rust-castaway-0.2.3
  (crate-source "castaway" "0.2.3"
                "1mf0wypwnkpa1hi0058vp8g7bjh2qraip2qv7dmak7mg1azfkfha"))

(define rust-cbc-0.1.2
  (crate-source "cbc" "0.1.2"
                "19l9y9ccv1ffg6876hshd123f2f8v7zbkc4nkckqycxf8fajmd96"
                #:snippet '(delete-file-recursively "tests")))

(define rust-cbindgen-0.25.0
  (crate-source "cbindgen" "0.25.0"
                "1jl82k816rxwi2ks9smnzlqv02fbq0sqy6r1g5d9f9x7wn9sdsps"))

(define rust-cbindgen-0.27.0
  (crate-source "cbindgen" "0.27.0"
                "1sqm3axr678d72yihgmpr9d17mj99ccibxfqhw53mgzwzkbqvkiz"))

(define rust-cbindgen-0.28.0
  (crate-source "cbindgen" "0.28.0"
                "1zyiaifg6mcd4wwhhbxk8adzhph6qz4wxzgagvg3ijp95j58dpga"))

(define rust-cc-1.2.18
  (crate-source "cc" "1.2.18"
                "0p6d2pfyrjgqpf2w399wzj4hmyffj6g0gyzg3pdy6xl3gmhlcl2j"))

(define rust-cesu8-1.1.0
  (crate-source "cesu8" "1.1.0"
                "0g6q58wa7khxrxcxgnqyi9s1z2cjywwwd3hzr5c55wskhx6s0hvd"))

(define rust-cexpr-0.6.0
  (crate-source "cexpr" "0.6.0"
                "0rl77bwhs5p979ih4r0202cn5jrfsrbgrksp40lkfz5vk1x3ib3g"))

(define rust-cfg-aliases-0.1.1
  (crate-source "cfg_aliases" "0.1.1"
                "17p821nc6jm830vzl2lmwz60g3a30hcm33nk6l257i1rjdqw85px"))

(define rust-cfg-aliases-0.2.1
  (crate-source "cfg_aliases" "0.2.1"
                "092pxdc1dbgjb6qvh83gk56rkic2n2ybm4yvy76cgynmzi3zwfk1"))

(define rust-cfg-expr-0.15.8
  (crate-source "cfg-expr" "0.15.8"
                "00lgf717pmf5qd2qsxxzs815v6baqg38d6m5i6wlh235p14asryh"))

(define rust-cfg-expr-0.17.2
  (crate-source "cfg-expr" "0.17.2"
                "12a7zr6ff4i6mfwcv711dll0w5pr3dw1lvkaf4c4a66i1gjacjwd"))

(define rust-cfg-if-1.0.0
  (crate-source "cfg-if" "1.0.0"
                "1za0vb97n4brpzpv8lsbnzmq5r8f2b0cpqqr0sy8h5bn751xxwds"))

(define rust-cgmath-0.18.0
  (crate-source "cgmath" "0.18.0"
                "05sk7c1c1jg5ygqvc3y77kxddp177gwazfibhd864ag3800x760s"))

(define rust-chardetng-0.1.17
  (crate-source "chardetng" "0.1.17"
                "1spikjcnblwa5n1nnk46fxkwn86yfiqxgs47h4yaw23vbfvg1f0l"))

(define rust-charset-0.1.5
  (crate-source "charset" "0.1.5"
                "0zkwcw525qwcqsdf74l9d2r6m69yxfxb4kgywp3q9fklgjq2gygi"))

(define rust-checked-int-cast-1.0.0
  (crate-source "checked_int_cast" "1.0.0"
                "06brva5agm6g12q15f8fidz17akb85q211496p1k2qxhb9mmxk0p"))

(define rust-chrono-0.4.40
  (crate-source "chrono" "0.4.40"
                "0z334kqnvq5zx6xsq1k6zk8g9z14fgk2w3vkn4n13pvi3mhn8y8s"))

(define rust-chrono-humanize-0.2.3
  (crate-source "chrono-humanize" "0.2.3"
                "0fq25fcdqd7s39dx81hq123210q4lpcbjdz82jl2fy6jnkk2g5kr"))

(define rust-chrono-tz-0.10.3
  (crate-source "chrono-tz" "0.10.3"
                "1wxl6jm6x5z2m6bh10dhi9s8hyqb6kmgd3naf6h37wbhqd4y3p7g"))

(define rust-chrono-tz-build-0.4.1
  (crate-source "chrono-tz-build" "0.4.1"
                "00llaqz0mhzhayswlxxx9d3qxd50akdzr1gq1w9gqc8f6k4zh44g"))

(define rust-chumsky-0.9.3
  (crate-source "chumsky" "0.9.3"
                "1jcnafc8rjfs1al08gqzyn0kpbaizgdwrd0ajqafspd18ikxdswf"))

(define rust-ciborium-0.2.2
  (crate-source "ciborium" "0.2.2"
                "03hgfw4674im1pdqblcp77m7rc8x2v828si5570ga5q9dzyrzrj2"))

(define rust-ciborium-io-0.2.2
  (crate-source "ciborium-io" "0.2.2"
                "0my7s5g24hvp1rs1zd1cxapz94inrvqpdf1rslrvxj8618gfmbq5"))

(define rust-ciborium-ll-0.2.2
  (crate-source "ciborium-ll" "0.2.2"
                "1n8g4j5rwkfs3rzfi6g1p7ngmz6m5yxsksryzf5k72ll7mjknrjp"))

(define rust-cipher-0.4.4
  (crate-source "cipher" "0.4.4"
                "1b9x9agg67xq5nq879z66ni4l08m6m3hqcshk37d4is4ysd3ngvp"))

(define rust-clang-sys-1.8.1
  (crate-source "clang-sys" "1.8.1"
                "1x1r9yqss76z8xwpdanw313ss6fniwc1r7dzb5ycjn0ph53kj0hb"))

(define rust-clap-2.34.0
  (crate-source "clap" "2.34.0"
                "071q5d8jfwbazi6zhik9xwpacx5i6kb2vkzy060vhf0c3120aqd0"))

(define rust-clap-3.2.25
  (crate-source "clap" "3.2.25"
                "08vi402vfqmfj9f07c4gl6082qxgf4c9x98pbndcnwbgaszq38af"))

(define rust-clap-4.5.35
  (crate-source "clap" "4.5.35"
                "0i1rnz7mwbhs5qf10r6vmrkplkzm3477khkwz189rha49f9qdanq"))

(define rust-clap-builder-4.5.35
  (crate-source "clap_builder" "4.5.35"
                "1nczcw6cc49ap99nn3v3n0vrv7j74zin34palq6ji586vnrdn514"))

(define rust-clap-complete-4.5.47
  (crate-source "clap_complete" "4.5.47"
                "1dkzjgmi0c4jgq4cwvmzbaki9mxanll6d0mw5gwd8ji6x9w56vy0"))

(define rust-clap-complete-command-0.6.1
  (crate-source "clap_complete_command" "0.6.1"
                "0qhv99j7msqyw7j17hswqwpqbdvqawy8l7ip6rnnh5930n61k3ns"))

(define rust-clap-complete-fig-4.5.2
  (crate-source "clap_complete_fig" "4.5.2"
                "0sy88ybw33ba7qj02caxr9jv03wq1f8rdbrbqw81i5gkiwn1156l"))

(define rust-clap-complete-nushell-4.5.5
  (crate-source "clap_complete_nushell" "4.5.5"
                "12miqxh9g7q37w11bgv55b32s0hdf6avf0lhagzc5psp6icv3a66"))

(define rust-clap-derive-3.2.25
  (crate-source "clap_derive" "3.2.25"
                "025hh66cyjk5xhhq8s1qw5wkxvrm8hnv5xwwksax7dy8pnw72qxf"))

(define rust-clap-derive-4.5.32
  (crate-source "clap_derive" "4.5.32"
                "1mqcag8qapb5yhygg2hi153kzmbf7w5hqp3nl3fvl5cn4yp6l5q9"))

(define rust-clap-lex-0.2.4
  (crate-source "clap_lex" "0.2.4"
                "1ib1a9v55ybnaws11l63az0jgz5xiy24jkdgsmyl7grcm3sz4l18"))

(define rust-clap-lex-0.7.4
  (crate-source "clap_lex" "0.7.4"
                "19nwfls5db269js5n822vkc8dw0wjq2h1wf0hgr06ld2g52d2spl"))

(define rust-clap-mangen-0.2.26
  (crate-source "clap_mangen" "0.2.26"
                "0fn1svjqm3znajji679nc2kfwm9lkyl73lzknf4rnkqlkgx44j3j"))

(define rust-clipboard-win-5.4.0
  (crate-source "clipboard-win" "5.4.0"
                "14n87fc0vzbd0wdhqzvcs1lqgafsncplzcanhpik93xhhalfgvqm"))

(define rust-clru-0.6.2
  (crate-source "clru" "0.6.2"
                "0ngyycxpxif84wpjjn0ixywylk95h5iv8fqycg2zsr3f0rpggl6b"))

(define rust-cmake-0.1.54
  (crate-source "cmake" "0.1.54"
                "1w41ma28yzad9x757s9sfq1wigjs9j902hbzc0nbxpc9vvws7jp7"))

(define rust-codepage-0.1.2
  (crate-source "codepage" "0.1.2"
                "1d0qr4wqc4yrab7halsa3r6akb2i2bk2cqr04vl8m0n23c38vxj8"))

(define rust-codspeed-2.10.0
  (crate-source "codspeed" "2.10.0"
                "1x9anwlfzlfby794d5fcvp214bj8bp29br8pkksxwb7834djja6j"))

(define rust-codspeed-criterion-compat-2.10.0
  (crate-source "codspeed-criterion-compat" "2.10.0"
                "1jj7n8q4f80sgyp66fax09p76bhbwvlgsndd21l0ahw1xdckmdji"))

(define rust-codspeed-criterion-compat-walltime-2.10.0
  (crate-source "codspeed-criterion-compat-walltime" "2.10.0"
                "0jpl1hwyqcay8cffqd66swaw0mllq8avbpxxk09cgrp8cgw1z649"))

(define rust-coitrees-0.2.1
  (crate-source "coitrees" "0.2.1"
                "1l2ybr8n02vm08wq9mrix7r07bgwm85i6fyachlm8d626w9w9d3f"))

(define rust-color-eyre-0.6.3
  (crate-source "color-eyre" "0.6.3"
                "1m9shifr9sdw0drszzyhvaq5jysrsiki44bl7m1gfdzj8rg6y52m"
                #:snippet '(for-each delete-file-recursively '("pictures" "scripts"))))

(define rust-color-print-0.3.7
  (crate-source "color-print" "0.3.7"
                "1x5nrpwwl3n8qawdyywiawv4j6yrd6mxjiz04db7sy8334bm9a9s"))

(define rust-color-print-proc-macro-0.3.7
  (crate-source "color-print-proc-macro" "0.3.7"
                "08la26krj5n9rl2c69hk2j711d4yrrza9bjrbbj0fh75xfsqc8b9"))

(define rust-color-quant-1.1.0
  (crate-source "color_quant" "1.1.0"
                "12q1n427h2bbmmm1mnglr57jaz2dj9apk0plcxw7nwqiai7qjyrx"))

(define rust-colorchoice-1.0.3
  (crate-source "colorchoice" "1.0.3"
                "1439m3r3jy3xqck8aa13q658visn71ki76qa93cy55wkmalwlqsv"))

(define rust-colored-1.9.4
  (crate-source "colored" "1.9.4"
                "0mc302pm2x0vpmc3ni35w0666858pmqlqzbipyz42cw2j4f78pss"))

(define rust-colored-2.2.0
  (crate-source "colored" "2.2.0"
                "0g6s7j2qayjd7i3sivmwiawfdg8c8ldy0g2kl4vwk1yk16hjaxqi"))

(define rust-colorz-1.1.4
  (crate-source "colorz" "1.1.4"
                "0yq6wvrajh73b9hwjr03brc2znhr1x1nym6bd5ry68c8g72kgsvc"))

(define rust-combine-4.6.7
  (crate-source "combine" "4.6.7"
                "1z8rh8wp59gf8k23ar010phgs0wgf5i8cx4fg01gwcnzfn5k0nms"))

(define rust-compact-str-0.8.1
  (crate-source "compact_str" "0.8.1"
                "0cmgp61hw4fwaakhilwznfgncw2p4wkbvz6dw3i7ibbckh3c8y9v"))

(define rust-concurrent-queue-2.5.0
  (crate-source "concurrent-queue" "2.5.0"
                "0wrr3mzq2ijdkxwndhf79k952cp4zkz35ray8hvsxl96xrx1k82c"))

(define rust-config-file-0.2.3
  (crate-source "config-file" "0.2.3"
                "1yys2088y6lnc959k1k78y0amjkp6a00pjybsk3x50872lnfflfz"))

(define rust-configparser-3.1.0
  (crate-source "configparser" "3.1.0"
                "16v47b7lknb35ragwhj9gzgwfpxs34vn2b97hhaky30ry1r34zp5"))

(define rust-console-0.15.11
  (crate-source "console" "0.15.11"
                "1n5gmsjk6isbnw6qss043377kln20lfwlmdk3vswpwpr21dwnk05"))

(define rust-const-format-0.2.34
  (crate-source "const_format" "0.2.34"
                "1pb3vx4k0bl3cy45fmba36hzds1jhkr8y9k3j5nnvm4abjb9fvqj"))

(define rust-const-format-proc-macros-0.2.34
  (crate-source "const_format_proc_macros" "0.2.34"
                "0i3pxxcl4xvwq4mlfg3csb4j0n6v0mhj07p6yk0vlvdirznc4mqx"))

(define rust-const-oid-0.9.6
  (crate-source "const-oid" "0.9.6"
                "1y0jnqaq7p2wvspnx7qj76m7hjcqpz73qzvr9l2p9n2s51vr6if2"))

(define rust-constant-time-eq-0.3.1
  (crate-source "constant_time_eq" "0.3.1"
                "19nwwczii762pwlsm7bpizgjg8hkg1kqi32b2g4rglijklsbhx3w"))

(define rust-container-of-0.5.1
  (crate-source "container_of" "0.5.1"
                "0as7g6gspvdbp4vl1a1834pzh481x9jp4clfgyl6c7vnhvmvpxc9"))

(define rust-content-inspector-0.2.4
  (crate-source "content_inspector" "0.2.4"
                "0f1gwv4axxw9wck4a4jxlkm7xjjakb3616isll2k0s4chmpadgdp"
                #:snippet '(delete-file-recursively "testdata")))

(define rust-convert-case-0.6.0
  (crate-source "convert_case" "0.6.0"
                "1jn1pq6fp3rri88zyw6jlhwwgf6qiyc08d6gjv0qypgkl862n67c"))

(define rust-cookie-factory-0.3.3
  (crate-source "cookie-factory" "0.3.3"
                "18mka6fk3843qq3jw1fdfvzyv05kx7kcmirfbs2vg2kbw9qzm1cq"))

(define rust-core-foundation-0.10.0
  (crate-source "core-foundation" "0.10.0"
                "0qscay14s2rwkg8nd8ljhiaf149hj8sfy95d70zssy64r3jp2lmm"))

(define rust-core-foundation-0.9.4
  (crate-source "core-foundation" "0.9.4"
                "13zvbbj07yk3b61b8fhwfzhy35535a583irf23vlcg59j7h9bqci"))

(define rust-core-foundation-sys-0.8.7
  (crate-source "core-foundation-sys" "0.8.7"
                "12w8j73lazxmr1z0h98hf3z623kl8ms7g07jch7n4p8f9nwlhdkp"))

(define rust-core-graphics-0.23.2
  (crate-source "core-graphics" "0.23.2"
                "10dhv3gk4kmbzl14xxkrhhky4fdp8h6nzff6h0019qgr6nz84xy0"))

(define rust-core-graphics-0.24.0
  (crate-source "core-graphics" "0.24.0"
                "1w8n8gqqm8swkanaibilqya8ryldp9fvf80byjxsaprn493a75gs"))

(define rust-core-graphics-types-0.1.3
  (crate-source "core-graphics-types" "0.1.3"
                "1bxg8nxc8fk4kxnqyanhf36wq0zrjr552c58qy6733zn2ihhwfa5"))

(define rust-core-graphics-types-0.2.0
  (crate-source "core-graphics-types" "0.2.0"
                "1sqka1rz84lr3p69i1s6lggnpnznmrw4ngc5q76w9xhky80s2i1x"))

(define rust-cpufeatures-0.2.17
  (crate-source "cpufeatures" "0.2.17"
                "10023dnnaghhdl70xcds12fsx2b966sxbxjq5sxs49mvxqw5ivar"))

(define rust-crates-io-0.40.9
  (crate-source "crates-io" "0.40.9"
                "0553wkxfgki90cb7qkzyb5f2z4lvqzwc5za8fzw5qj5wh7xbjrv9"))

(define rust-crc32fast-1.4.2
  (crate-source "crc32fast" "1.4.2"
                "1czp7vif73b8xslr3c9yxysmh9ws2r8824qda7j47ffs9pcnjxx9"))

(define rust-criterion-0.5.1
  (crate-source "criterion" "0.5.1"
                "0bv9ipygam3z8kk6k771gh9zi0j0lb9ir0xi1pc075ljg80jvcgj"))

(define rust-criterion-plot-0.5.0
  (crate-source "criterion-plot" "0.5.0"
                "1c866xkjqqhzg4cjvg01f8w6xc1j3j7s58rdksl52skq89iq4l3b"))

(define rust-critical-section-1.2.0
  (crate-source "critical-section" "1.2.0"
                "02ylhcykxjc40xrfhk1lwc21jqgz4dbwv3jr49ymw733c51yl3kr"))

(define rust-crossbeam-0.8.4
  (crate-source "crossbeam" "0.8.4"
                "1a5c7yacnk723x0hfycdbl91ks2nxhwbwy46b8y5vyy0gxzcsdqi"))

(define rust-crossbeam-channel-0.5.14
  (crate-source "crossbeam-channel" "0.5.14"
                "0wa41qybq5w8s70anb472myh4fid4aw6v65vws6wn528w9l6vfh6"))

(define rust-crossbeam-channel-0.5.15
  (crate-source "crossbeam-channel" "0.5.15"
                "1cicd9ins0fkpfgvz9vhz3m9rpkh6n8d3437c3wnfsdkd3wgif42"))

(define rust-crossbeam-deque-0.8.6
  (crate-source "crossbeam-deque" "0.8.6"
                "0l9f1saqp1gn5qy0rxvkmz4m6n7fc0b3dbm6q1r5pmgpnyvi3lcx"))

(define rust-crossbeam-epoch-0.9.18
  (crate-source "crossbeam-epoch" "0.9.18"
                "03j2np8llwf376m3fxqx859mgp9f83hj1w34153c7a9c7i5ar0jv"))

(define rust-crossbeam-queue-0.3.12
  (crate-source "crossbeam-queue" "0.3.12"
                "059igaxckccj6ndmg45d5yf7cm4ps46c18m21afq3pwiiz1bnn0g"))

(define rust-crossbeam-utils-0.8.21
  (crate-source "crossbeam-utils" "0.8.21"
                "0a3aa2bmc8q35fb67432w16wvi54sfmb69rk9h5bhd18vw0c99fh"))

(define rust-crossterm-0.28.1
  (crate-source "crossterm" "0.28.1"
                "1im9vs6fvkql0sr378dfr4wdm1rrkrvr22v4i8byz05k1dd9b7c2"
                #:snippet '(delete-file-recursively "docs")))

(define rust-crossterm-winapi-0.9.1
  (crate-source "crossterm_winapi" "0.9.1"
                "0axbfb2ykbwbpf1hmxwpawwfs8wvmkcka5m561l7yp36ldi7rpdc"))

(define rust-crunchy-0.2.3
  (crate-source "crunchy" "0.2.3"
                "0aa9k4izp962qlsn5ndgw2zq62mizcpnkns8bxscgz3gqr35knj3"))

(define rust-crypto-bigint-0.5.5
  (crate-source "crypto-bigint" "0.5.5"
                "0xmbdff3g6ii5sbxjxc31xfkv9lrmyril4arh3dzckd4gjsjzj8d"))

(define rust-crypto-common-0.1.6
  (crate-source "crypto-common" "0.1.6"
                "1cvby95a6xg7kxdz5ln3rl9xh66nz66w46mm3g56ri1z5x815yqv"))

(define rust-csscolorparser-0.7.0
  (crate-source "csscolorparser" "0.7.0"
                "12423a53ikbzacavi157kf3xz9p93sn8gqvwsifvjzwahima3ya6"))

(define rust-cssparser-0.31.2
  (crate-source "cssparser" "0.31.2"
                "1gnmn2wjvhvkj98ygcd5jdwi0wxsigvd7j0yq0zfgfsz7vwz8gav"))

(define rust-cssparser-macros-0.6.1
  (crate-source "cssparser-macros" "0.6.1"
                "0cfkzj60avrnskdmaf7f8zw6pp3di4ylplk455zrzaf19ax8id8k"))

(define rust-cstr-0.2.12
  (crate-source "cstr" "0.2.12"
                "0dj6ll9ry27kn4k0vvhlvbhn9dyyr9haxnd06bxaqnmfr01kjlk8"))

(define rust-csv-1.3.1
  (crate-source "csv" "1.3.1"
                "1bzxgbbhy27flcyafxbj7f1hbn7b8wac04ijfgj34ry9m61lip5c"))

(define rust-csv-core-0.1.12
  (crate-source "csv-core" "0.1.12"
                "0gfrjjlfagarhyclxrqv6b14iaxgvgc8kmwwdvw08racvaqg60kx"))

(define rust-ct-codecs-1.1.3
  (crate-source "ct-codecs" "1.1.3"
                "191f2id5zqv5hjm0nsblbwq1n276ba55w0bgi6b2c674x66bl5mr"))

(define rust-ctr-0.9.2
  (crate-source "ctr" "0.9.2"
                "0d88b73waamgpfjdml78icxz45d95q7vi2aqa604b0visqdfws83"
                #:snippet '(delete-file-recursively "tests")))

(define rust-ctrlc-3.4.6
  (crate-source "ctrlc" "3.4.6"
                "0735llsx3zwqvqbwfikz3j4aa03c075ii04f8zi5mza8yccm8yv9"))

(define rust-curl-0.4.47
  (crate-source "curl" "0.4.47"
                "0rcjdrl35xs8l5v3wv6q5z37hjw3r5bvmbb09pqmhaxyl49lvyyr"))

(define rust-curl-sys-0.4.80+curl-8.12.1
  (crate-source "curl-sys" "0.4.80+curl-8.12.1"
                "0d7ppx4kq77hc5nyff6jydmfabpgd0i3ppjvn8x0q833mhpdzxsm"
                #:snippet '(delete-file-recursively "curl")))

(define rust-cursor-icon-1.1.0
  (crate-source "cursor-icon" "1.1.0"
                "14brf4vd6az9hnszwzqj7xyfaymqx9806d4i7xmwlaja3wjsr9ln"))

(define rust-custom-derive-0.1.7
  (crate-source "custom_derive" "0.1.7"
                "1f81bavw1wnykwh21hh4yyzigs6zl6f6pkk9p3car8kq95yfb2pg"))

(define rust-cvss-2.0.0
  (crate-source "cvss" "2.0.0"
                "03q1nh4jy0cvgckji1jr1kz3j7gf2zg74240j8qi1qxhk7vs5iky"))

(define rust-darling-0.20.11
  (crate-source "darling" "0.20.11"
                "1vmlphlrlw4f50z16p4bc9p5qwdni1ba95qmxfrrmzs6dh8lczzw"))

(define rust-darling-core-0.20.11
  (crate-source "darling_core" "0.20.11"
                "0bj1af6xl4ablnqbgn827m43b8fiicgv180749f5cphqdmcvj00d"))

(define rust-darling-macro-0.20.11
  (crate-source "darling_macro" "0.20.11"
                "1bbfbc2px6sj1pqqq97bgqn6c8xdnb2fmz66f7f40nrqrcybjd7w"))

(define rust-dashmap-5.5.3
  (crate-source "dashmap" "5.5.3"
                "0miqnlxi501vfbv6mw5jbmzgnj0wjrch3p4abvpd59s9v30lg1wp"))

(define rust-dashmap-6.1.0
  (crate-source "dashmap" "6.1.0"
                "1kvnw859xvrqyd1lk89na6797yvl5bri4wi9j0viz2a4j54wqhah"))

(define rust-data-encoding-2.8.0
  (crate-source "data-encoding" "2.8.0"
                "0470yf5ly1ibzmwygyjqg9ii9njbsha3xr5qj5dxyf2psbgpapsp"))

(define rust-data-url-0.3.1
  (crate-source "data-url" "0.3.1"
                "0ahclz72myi350cs1xcsxdh1v0iljpfj4ghcy2fy46mpfhf7laaw"))

(define rust-dbus-0.9.7
  (crate-source "dbus" "0.9.7"
                "06vdv4aarjs4w6byg9nqajr67c8qvlhk3153ic2i65pvp63ikchv"))

(define rust-deadpool-0.10.0
  (crate-source "deadpool" "0.10.0"
                "145lq79dlc4jn7jvlcf4lb105bs3z3jy6g7d15zv7iy1g04i117v"))

(define rust-deadpool-runtime-0.1.4
  (crate-source "deadpool-runtime" "0.1.4"
                "0arbchl5j887hcfvjy4gq38d32055s5cf7pkpmwn0lfw3ss6ca89"))

(define rust-delharc-0.6.1
  (crate-source "delharc" "0.6.1"
                "18g5haj6bj92azif4jifhdy9vrv6blg3wyvpmxslh2gm2wkbm4qw"))

(define rust-der-0.7.9
  (crate-source "der" "0.7.9"
                "1h4vzjfa1lczxdf8avfj9qlwh1qianqlxdy1g5rn762qnvkzhnzm"
                #:snippet '(delete-file-recursively "tests")))

(define rust-deranged-0.4.0
  (crate-source "deranged" "0.4.0"
                "13h6skwk411wzhf1l9l7d3yz5y6vg9d7s3dwhhb4a942r88nm7lw"))

(define rust-deranged-0.4.1
  (crate-source "deranged" "0.4.1"
                "0n7hswnz5jz1rjy6zr8sc9awbszkmv1345hphccawj40w1larkr8"))

(define rust-derivative-2.2.0
  (crate-source "derivative" "2.2.0"
                "02vpb81wisk2zh1d5f44szzxamzinqgq2k8ydrfjj2wwkrgdvhzw"))

(define rust-derive-arbitrary-1.4.1
  (crate-source "derive_arbitrary" "1.4.1"
                "000839h4mbgs65x1f8540kbjk2ifw68c4d8r5b9f7q0jv4d2qm1h"))

(define rust-derive-more-0.99.19
  (crate-source "derive_more" "0.99.19"
                "17y6g78dg31fsv7z4p455bzxs670spg476ww2ibg3mj3vww9m8ix"))

(define rust-derive-new-0.5.9
  (crate-source "derive-new" "0.5.9"
                "0d9m5kcj1rdmdjqfgj7rxxhdzx0as7p4rp1mjx5j6w5dl2f3461l"))

(define rust-derive-new-0.6.0
  (crate-source "derive-new" "0.6.0"
                "1b8jv6jx0b8jgkz9kmz0ciqmnf74xkk0mmvkb5z1c87932kdwl6i"))

(define rust-devicons-0.6.12
  (crate-source "devicons" "0.6.12"
                "0jwh0g72rfkpbsm16rxb47y3ylmr47wwx3cmbbflzkrhygi4f3l3"))

(define rust-dialoguer-0.11.0
  (crate-source "dialoguer" "0.11.0"
                "1pl0744wwr97kp8qnaybzgrfwk66qakzq0i1qrxl03vpbn0cx2v5"))

(define rust-diesel-2.2.9
  (crate-source "diesel" "2.2.9"
                "0lsid456rv1bj7g1aai4sh33082yfxzb8qk12886jfmsj039blrl"))

(define rust-diesel-derives-2.2.4
  (crate-source "diesel_derives" "2.2.4"
                "0mddq3ha8v8yz6vp2w0pinfrsnc0078p7zw784xsdgkh9cjmhfd9"))

(define rust-diesel-migrations-2.2.0
  (crate-source "diesel_migrations" "2.2.0"
                "1xn12ny9m1ci74iqpvhcfyhapr6wj56k3wxz07q32hmd9dqcwwwa"))

(define rust-diesel-table-macro-syntax-0.2.0
  (crate-source "diesel_table_macro_syntax" "0.2.0"
                "09gvkyljhchbxfkxlkkrdcqcmcxwsim9sfljqilbq4x485b77710"))

(define rust-diff-0.1.13
  (crate-source "diff" "0.1.13"
                "1j0nzjxci2zqx63hdcihkp0a4dkdmzxd7my4m7zk6cjyfy34j9an"))

(define rust-difflib-0.4.0
  (crate-source "difflib" "0.4.0"
                "1s7byq4d7jgf2hcp2lcqxi2piqwl8xqlharfbi8kf90n8csy7131"))

(define rust-digest-0.10.7
  (crate-source "digest" "0.10.7"
                "14p2n6ih29x81akj097lvz7wi9b6b9hvls0lwrv7b6xwyy0s5ncy"))

(define rust-directories-4.0.1
  (crate-source "directories" "4.0.1"
                "045jbj5y2f1fmjs9rfcw95y0vjydb2rqqhz1sdnqhdmxv96ms77m"))

(define rust-directories-5.0.1
  (crate-source "directories" "5.0.1"
                "0dba6xzk79s1clqzxh2qlgzk3lmvvks1lzzjhhi3hd70hhxifjcs"))

(define rust-directories-6.0.0
  (crate-source "directories" "6.0.0"
                "0zgy2w088v8w865c11dmc3dih899fgrhvrfp7g83h6v6ai60kx8n"))

(define rust-dirs-5.0.1
  (crate-source "dirs" "5.0.1"
                "0992xk5vx75b2x91nw9ssb51mpl8x73j9rxmpi96cryn0ffmmi24"))

(define rust-dirs-next-2.0.0
  (crate-source "dirs-next" "2.0.0"
                "1q9kr151h9681wwp6is18750ssghz6j9j7qm7qi1ngcwy7mzi35r"))

(define rust-dirs-sys-0.3.7
  (crate-source "dirs-sys" "0.3.7"
                "19md1cnkazham8a6kh22v12d8hh3raqahfk6yb043vrjr68is78v"))

(define rust-dirs-sys-0.4.1
  (crate-source "dirs-sys" "0.4.1"
                "071jy0pvaad9lsa6mzawxrh7cmr7hsmsdxwzm7jzldfkrfjha3sj"))

(define rust-dirs-sys-0.5.0
  (crate-source "dirs-sys" "0.5.0"
                "1aqzpgq6ampza6v012gm2dppx9k35cdycbj54808ksbys9k366p0"))

(define rust-dirs-sys-next-0.1.2
  (crate-source "dirs-sys-next" "0.1.2"
                "0kavhavdxv4phzj4l0psvh55hszwnr0rcz8sxbvx20pyqi2a3gaf"))

(define rust-dispatch-0.2.0
  (crate-source "dispatch" "0.2.0"
                "0fwjr9b7582ic5689zxj8lf7zl94iklhlns3yivrnv8c9fxr635x"))

(define rust-display-error-chain-0.2.2
  (crate-source "display-error-chain" "0.2.2"
                "1xbcilzyfc8n60vjkmsf8v53kw855xw68jh69hpza6dwhrp19hhb"))

(define rust-displaydoc-0.2.5
  (crate-source "displaydoc" "0.2.5"
                "1q0alair462j21iiqwrr21iabkfnb13d6x5w95lkdg21q2xrqdlp"))

(define rust-dissimilar-1.0.10
  (crate-source "dissimilar" "1.0.10"
                "08b94x25x3ba6vg79i53wspxyagqr43crg9dw2zn2dpgl3dgyxc9"))

(define rust-dlib-0.5.2
  (crate-source "dlib" "0.5.2"
                "04m4zzybx804394dnqs1blz241xcy480bdwf3w9p4k6c3l46031k"))

(define rust-dns-lookup-2.0.4
  (crate-source "dns-lookup" "2.0.4"
                "1z74n2zij2gahycabm0gkmkyx574h76gwk7sz93yqpr3qa3n0xp5"))

(define rust-doc-comment-0.3.3
  (crate-source "doc-comment" "0.3.3"
                "043sprsf3wl926zmck1bm7gw0jq50mb76lkpk49vasfr6ax1p97y"))

(define rust-docopt-1.1.1
  (crate-source "docopt" "1.1.1"
                "07x5g52n6fzilxhk5220caznkvdnzzvahlzrzkmgj8y88sc12gvz"))

(define rust-doctest-file-1.0.0
  (crate-source "doctest-file" "1.0.0"
                "0qkmnrsx2kszm58wxyry63bs35msj9chdb6jlh54a8cdwaiizj5a"))

(define rust-dotenvy-0.15.7
  (crate-source "dotenvy" "0.15.7"
                "16s3n973n5aqym02692i1npb079n5mb0fwql42ikmwn8wnrrbbqs"))

(define rust-downcast-rs-1.2.1
  (crate-source "downcast-rs" "1.2.1"
                "1lmrq383d1yszp7mg5i7i56b17x2lnn3kb91jwsq0zykvg2jbcvm"))

(define rust-dpi-0.1.1
  (crate-source "dpi" "0.1.1"
                "0lzz48gpgbwdrw0s8vib0589ij9jizv1vzsphm4xd9kw58lhwp7j"))

(define rust-drm-0.14.1
  (crate-source "drm" "0.14.1"
                "0vvmj9n0wslrbw3rinpzlfyhwwgr02gqspy1al5gfh99dif8rg40"
                #:snippet '(delete-file-recursively "examples")))

(define rust-drm-ffi-0.9.0
  (crate-source "drm-ffi" "0.9.0"
                "12vff80hdpp81gj5lqw25xnkppwsxc4wklpn8nc556wsv5ci9r6q"))

(define rust-drm-fourcc-2.2.0
  (crate-source "drm-fourcc" "2.2.0"
                "1x76v9a0pkgym4n6cah4barnai9gsssm7gjzxskw2agwibdvrbqa"))

(define rust-drm-sys-0.8.0
  (crate-source "drm-sys" "0.8.0"
                "1345z72hd2rna4qxd2zcpbzvw0z7ywfndk6g2ngdci69vg46dyxs"))

(define rust-dsl-auto-type-0.1.3
  (crate-source "dsl_auto_type" "0.1.3"
                "0nzzqmqyymlnffhms1kism5xhgsk73mq6r6pdpr8azsjlynfk6hk"))

(define rust-dtoa-1.0.10
  (crate-source "dtoa" "1.0.10"
                "016gid01rarcdv57h049d7nr9daxc2hc2gqzx0mji57krywd7bfn"))

(define rust-dtoa-short-0.3.5
  (crate-source "dtoa-short" "0.3.5"
                "11rwnkgql5jilsmwxpx6hjzkgyrbdmx1d71s0jyrjqm5nski25fd"))

(define rust-dtparse-2.0.1
  (crate-source "dtparse" "2.0.1"
                "1mqz4164mc4xyq73c22wf900v8cn4sy63nalrkr5mlr614y41yr3"))

(define rust-duct-0.13.7
  (crate-source "duct" "0.13.7"
                "174jk13rlvfgypha4f3l27mzzyc0ci7zginh5hjn6jr2s4c5gaz4"))

(define rust-dunce-1.0.5
  (crate-source "dunce" "1.0.5"
                "04y8wwv3vvcqaqmqzssi6k0ii9gs6fpz96j5w9nky2ccsl23axwj"))

(define rust-dyn-clone-1.0.19
  (crate-source "dyn-clone" "1.0.19"
                "01ahm5abl20480v48nxy4ffyx80cs6263q9zf0gnrxpvm6w8yyhw"))

(define rust-ecdsa-0.16.9
  (crate-source "ecdsa" "0.16.9"
                "1jhb0bcbkaz4001sdmfyv8ajrv8a1cg7z7aa5myrd4jjbhmz69zf"))

(define rust-ed25519-compact-2.1.1
  (crate-source "ed25519-compact" "2.1.1"
                "1431kxw67xkk5y5kamfdjxnqbzqy5y4p032syi3wva5y8h7ldcz9"))

(define rust-editdistancek-1.0.2
  (crate-source "editdistancek" "1.0.2"
                "04r6lfq9sfz3wqhqm6fzfcqbj8w16y8bh0x6kzkgkimislixy0iy"))

(define rust-either-1.15.0
  (crate-source "either" "1.15.0"
                "069p1fknsmzn9llaizh77kip0pqmcwpdsykv2x30xpjyija5gis8"))

(define rust-elliptic-curve-0.13.8
  (crate-source "elliptic-curve" "0.13.8"
                "0ixx4brgnzi61z29r3g1606nh2za88hzyz8c5r3p6ydzhqq09rmm"
                #:snippet '(delete-file-recursively "tests")))

(define rust-embed-resource-2.5.1
  (crate-source "embed-resource" "2.5.1"
                "0yb3kbw3xpghiwf69769jpng725kwa2cxm27qj5s7dm0cfgnz2xn"))

(define rust-encode-unicode-0.3.6
  (crate-source "encode_unicode" "0.3.6"
                "07w3vzrhxh9lpjgsg2y5bwzfar2aq35mdznvcp3zjl0ssj7d4mx3"))

(define rust-encode-unicode-1.0.0
  (crate-source "encode_unicode" "1.0.0"
                "1h5j7j7byi289by63s3w4a8b3g6l5ccdrws7a67nn07vdxj77ail"))

(define rust-encoding-rs-0.8.35
  (crate-source "encoding_rs" "0.8.35"
                "1wv64xdrr9v37rqqdjsyb8l8wzlcbab80ryxhrszvnj59wy0y0vm"))

(define rust-encoding-rs-io-0.1.7
  (crate-source "encoding_rs_io" "0.1.7"
                "10ra4l688cdadd8h1lsbahld1zbywnnqv68366mbhamn3xjwbhqw"))

(define rust-endi-1.1.0
  (crate-source "endi" "1.1.0"
                "1gxp388g2zzbncp3rdn60wxkr49xbhhx94nl9p4a6c41w4ma7n53"))

(define rust-endian-type-0.1.2
  (crate-source "endian-type" "0.1.2"
                "0bbh88zaig1jfqrm7w3gx0pz81kw2jakk3055vbgapw3dmk08ky3"))

(define rust-enquote-1.1.0
  (crate-source "enquote" "1.1.0"
                "0clrjghlfkkb7sndabs5wch0fz2nif6nj4b117s8kqxx3nqnrhq6"))

(define rust-enum-map-0.6.6
  (crate-source "enum-map" "0.6.6"
                "166yh44izfz871f4avl10cp0l161shhsawfcl1q6slv3siamqn93"))

(define rust-enum-map-1.1.1
  (crate-source "enum-map" "1.1.1"
                "1n99fg6hwxjfb9fzil2zrvc7radj9yqnyjn8vrc110hnc6xag4z8"))

(define rust-enum-map-2.7.3
  (crate-source "enum-map" "2.7.3"
                "1sgjgl4mmz93jdkfdsmapc3dmaq8gddagw9s0fd501w2vyzz6rk8"))

(define rust-enum-map-derive-0.17.0
  (crate-source "enum-map-derive" "0.17.0"
                "1sv4mb343rsz4lc3rh7cyn0pdhf7fk18k1dgq8kfn5i5x7gwz0pj"))

(define rust-enum-map-derive-0.4.6
  (crate-source "enum-map-derive" "0.4.6"
                "0mg43p1x90cz604zddk9qzss077v2id04qmmbpa1i7jc637m1i75"))

(define rust-enum-map-derive-0.6.0
  (crate-source "enum-map-derive" "0.6.0"
                "1ah7b71llknvwj031zsrqpxam5566hbc2i6vq7v4zqzn1ap8w9w4"))

(define rust-enum-ordinalize-4.3.0
  (crate-source "enum-ordinalize" "4.3.0"
                "1max64z9giii61qcwl56rndd7pakaylkaij5zqbbbvjl9vxdr87y"))

(define rust-enum-ordinalize-derive-4.3.1
  (crate-source "enum-ordinalize-derive" "4.3.1"
                "1zy53fabazimwv5cl0366k834ybixzl84lxj9mfavbnlfn532a0d"))

(define rust-enum-primitive-derive-0.2.2
  (crate-source "enum-primitive-derive" "0.2.2"
                "03ibjjx8dc4akpq8ck24qda5ix4jybz9jagfxykd0s6vxb2vjxf3"))

(define rust-enumflags2-0.7.11
  (crate-source "enumflags2" "0.7.11"
                "0iwi60d54lgby0f29b5isikxraf0wvnqdmlddx68a62kbx34nbxs"))

(define rust-enumflags2-derive-0.7.11
  (crate-source "enumflags2_derive" "0.7.11"
                "0yfdjyrf9b4mi1r589azkyirjhzmdw29nqq0mdjnsyldlmjayk7w"))

(define rust-env-filter-0.1.3
  (crate-source "env_filter" "0.1.3"
                "1l4p6f845cylripc3zkxa0lklk8rn2q86fqm522p6l2cknjhavhq"))

(define rust-env-home-0.1.0
  (crate-source "env_home" "0.1.0"
                "1zn08mk95rjh97831rky1n944k024qrwjhbcgb0xv9zhrh94xy67"))

(define rust-env-logger-0.10.2
  (crate-source "env_logger" "0.10.2"
                "1005v71kay9kbz1d5907l0y7vh9qn2fqsp2yfgb8bjvin6m0bm2c"))

(define rust-env-logger-0.11.8
  (crate-source "env_logger" "0.11.8"
                "17q6zbjam4wq75fa3m4gvvmv3rj3ch25abwbm84b28a0j3q67j0k"))

(define rust-env-logger-0.8.4
  (crate-source "env_logger" "0.8.4"
                "1qzw8g11dbdfi7ixm44ldykwcqsxqkh8vx5cgpd88zmclgz8g4d1"))

(define rust-env-logger-0.9.3
  (crate-source "env_logger" "0.9.3"
                "1rq0kqpa8my6i1qcyhfqrn1g9xr5fbkwwbd42nqvlzn9qibncbm1"))

(define rust-equivalent-1.0.2
  (crate-source "equivalent" "1.0.2"
                "03swzqznragy8n0x31lqc78g2af054jwivp7lkrbrc0khz74lyl7"))

(define rust-erased-serde-0.4.6
  (crate-source "erased-serde" "0.4.6"
                "1dx5hj16hsl143czwl2g7ymdi1y84lsjyyii2zprzjqzyn3xh170"))

(define rust-errno-0.2.8
  (crate-source "errno" "0.2.8"
                "18cnqgk8r6lq1n5cfy3bryiyz9zkqr10dxj49sa3fkzfamih8fgn"))

(define rust-errno-0.3.11
  (crate-source "errno" "0.3.11"
                "0kjrrcaa5nvickysw7z3vm5p0l7l457idf1ff3z6ang8qwnx8vcp"))

(define rust-errno-dragonfly-0.1.2
  (crate-source "errno-dragonfly" "0.1.2"
                "1grrmcm6q8512hkq5yzch3yv8wafflc2apbmsaabiyk44yqz2s5a"))

(define rust-error-chain-0.12.4
  (crate-source "error-chain" "0.12.4"
                "1z6y5isg0il93jp287sv7pn10i4wrkik2cpyk376wl61rawhcbrd"))

(define rust-error-code-3.3.1
  (crate-source "error-code" "3.3.1"
                "0bx9hw3pahzqym8jvb0ln4qsabnysgn98mikyh2afhk9rif31nd5"))

(define rust-etcetera-0.10.0
  (crate-source "etcetera" "0.10.0"
                "1rka6bskn93pdhx32xaagr147q95z5bnz7ym5xr85jw00wyv3ir6"))

(define rust-etcetera-0.8.0
  (crate-source "etcetera" "0.8.0"
                "0hxrsn75dirbjhwgkdkh0pnpqrnq17ypyhjpjaypgax1hd91nv8k"))

(define rust-evdev-0.12.2
  (crate-source "evdev" "0.12.2"
                "19qh6r1z4v8ja6qqigjbg9vckbhlycc6wkqgzfz9fcln7almaq5b"))

(define rust-event-listener-2.5.3
  (crate-source "event-listener" "2.5.3"
                "1q4w3pndc518crld6zsqvvpy9lkzwahp2zgza9kbzmmqh9gif1h2"))

(define rust-event-listener-3.1.0
  (crate-source "event-listener" "3.1.0"
                "1hihkg6ihvb6p9yi7nq11di8mhd5y0iqv81ij6h0rf0fvsy7ff6r"))

(define rust-event-listener-5.4.0
  (crate-source "event-listener" "5.4.0"
                "1bii2gn3vaa33s0gr2zph7cagiq0ppcfxcxabs24ri9z9kgar4il"))

(define rust-event-listener-strategy-0.5.4
  (crate-source "event-listener-strategy" "0.5.4"
                "14rv18av8s7n8yixg38bxp5vg2qs394rl1w052by5npzmbgz7scb"))

(define rust-expect-test-1.5.1
  (crate-source "expect-test" "1.5.1"
                "1c5c081ykm4k5rlsam9jw56w4wgs2h7r4aj78zxlis1i8kzl7bv3"))

(define rust-exr-1.73.0
  (crate-source "exr" "1.73.0"
                "1q47yq78q9k210r6jy1wwrilxwwxqavik9l3l426rd17k7srfcgq"))

(define rust-eyre-0.6.12
  (crate-source "eyre" "0.6.12"
                "1v1a3vb9gs5zkwp4jzkcfnpg0gvyp4ifydzx37f4qy14kzcibnbw"))

(define rust-failure-0.1.8
  (crate-source "failure" "0.1.8"
                "11jg1wmbkijrs6bk9fqnbrm9zf0850whnqpgnxyswbn0dk8rnbnk"))

(define rust-failure-derive-0.1.8
  (crate-source "failure_derive" "0.1.8"
                "1936adqqk080439kx2bjf1bds7h89sg6wcif4jw0syndcv3s6kda"))

(define rust-fallible-iterator-0.3.0
  (crate-source "fallible-iterator" "0.3.0"
                "0ja6l56yka5vn4y4pk6hn88z0bpny7a8k1919aqjzp0j1yhy9k1a"))

(define rust-fallible-streaming-iterator-0.1.9
  (crate-source "fallible-streaming-iterator" "0.1.9"
                "0nj6j26p71bjy8h42x6jahx1hn0ng6mc2miwpgwnp8vnwqf4jq3k"))

(define rust-fancy-regex-0.14.0
  (crate-source "fancy-regex" "0.14.0"
                "162j2qx2ikgl79grq12mawyflwkirnjzrvxh11a1xbmwjidcn93f"))

(define rust-faster-hex-0.9.0
  (crate-source "faster-hex" "0.9.0"
                "10wi4vqbdpkamw4qvra1ijp4as2j7j1zc66g4rdr6h0xv8gb38m2"))

(define rust-fastrand-1.9.0
  (crate-source "fastrand" "1.9.0"
                "1gh12m56265ihdbzh46bhh0jf74i197wm51jg1cw75q7ggi96475"))

(define rust-fastrand-2.3.0
  (crate-source "fastrand" "2.3.0"
                "1ghiahsw1jd68df895cy5h3gzwk30hndidn3b682zmshpgmrx41p"))

(define rust-fat-macho-0.4.9
  (crate-source "fat-macho" "0.4.9"
                "0idkn366wipv2l757yqfgzgibqc6jvm89gdk9kpgmvf6lv54b72c"
                #:snippet '(delete-file-recursively "tests")))

(define rust-fd-lock-4.0.4
  (crate-source "fd-lock" "4.0.4"
                "0y5a22zaqns06slndm64gjdx983i6b4l4ks895rxznnn4bv2zs8c"))

(define rust-fdeflate-0.3.7
  (crate-source "fdeflate" "0.3.7"
                "130ga18vyxbb5idbgi07njymdaavvk6j08yh1dfarm294ssm6s0y"
                #:snippet '(delete-file-recursively "tests")))

(define rust-feature-probe-0.1.1
  (crate-source "feature-probe" "0.1.1"
                "1nhif9zpr2f17gagf0qb0v914wc3jr9sfjzvnpi7b7pcs73ksnl3"))

(define rust-fern-0.6.2
  (crate-source "fern" "0.6.2"
                "1vpinainw32498p0zydmxc24yd3r6479pmhdfb429mfbji3c3w6r"
                #:snippet '(delete-file-recursively "examples")))

(define rust-ff-0.13.1
  (crate-source "ff" "0.13.1"
                "14v3bc6q24gbcjnxjfbq2dddgf4as2z2gd4mj35gjlrncpxhpdf0"))

(define rust-fiat-crypto-0.2.9
  (crate-source "fiat-crypto" "0.2.9"
                "07c1vknddv3ak7w89n85ik0g34nzzpms6yb845vrjnv9m4csbpi8"))

(define rust-field-offset-0.3.6
  (crate-source "field-offset" "0.3.6"
                "0zq5sssaa2ckmcmxxbly8qgz3sxpb8g1lwv90sdh1z74qif2gqiq"))

(define rust-file-id-0.2.2
  (crate-source "file-id" "0.2.2"
                "0dmylm34z6g8cg3b60sc6bk9v5wv9930vyx9wgcdpjpgpfwh9jbb"))

(define rust-filesize-0.2.0
  (crate-source "filesize" "0.2.0"
                "0hvx4dfnara3a2dnhb9ci5bmm1m8s44h9l61s5djwkjx87i43mqj"))

(define rust-filetime-0.2.25
  (crate-source "filetime" "0.2.25"
                "11l5zr86n5sr6g6k6sqldswk0jzklm0q95rzikxcns0yk0p55h1m"))

(define rust-fixedbitset-0.2.0
  (crate-source "fixedbitset" "0.2.0"
                "0kg03p777wc0dajd9pvlcnsyrwa8dhqwf0sd9r4dw0p82rs39arp"))

(define rust-fixedbitset-0.4.2
  (crate-source "fixedbitset" "0.4.2"
                "101v41amgv5n9h4hcghvrbfk5vrncx1jwm35rn5szv4rk55i7rqc"))

(define rust-fixedbitset-0.5.7
  (crate-source "fixedbitset" "0.5.7"
                "16fd3v9d2cms2vddf9xhlm56sz4j0zgrk3d2h6v1l7hx760lwrqx"))

(define rust-flate2-1.1.1
  (crate-source "flate2" "1.1.1"
                "1kpycx57dqpkr3vp53b4nq75p9mflh0smxy8hkys4v4ndvkr5vbw"
                #:snippet '(for-each delete-file-recursively '("examples" "tests"))))

(define rust-float-cmp-0.10.0
  (crate-source "float-cmp" "0.10.0"
                "1n760i3nxd2x0zc7fkxkg3vhvdyfbvzngna006cl9s9jacaz775h"))

(define rust-float-cmp-0.9.0
  (crate-source "float-cmp" "0.9.0"
                "1i799ksbq7fj9rm9m82g1yqgm6xi3jnrmylddmqknmksajylpplq"))

(define rust-fluent-uri-0.1.4
  (crate-source "fluent-uri" "0.1.4"
                "03ah2qajw5l1zbc81kh1n8g7n24mfxbg6vqyv9ixipg1vglh9iqp"))

(define rust-fnv-1.0.7
  (crate-source "fnv" "1.0.7"
                "1hc2mcqha06aibcaza94vbi81j6pr9a1bbxrxjfhc91zin8yr7iz"))

(define rust-foldhash-0.1.5
  (crate-source "foldhash" "0.1.5"
                "1wisr1xlc2bj7hk4rgkcjkz3j2x4dhd1h9lwk7mj8p71qpdgbi6r"))

(define rust-foreign-types-0.3.2
  (crate-source "foreign-types" "0.3.2"
                "1cgk0vyd7r45cj769jym4a6s7vwshvd0z4bqrb92q1fwibmkkwzn"))

(define rust-foreign-types-0.5.0
  (crate-source "foreign-types" "0.5.0"
                "0rfr2zfxnx9rz3292z5nyk8qs2iirznn5ff3rd4vgdwza6mdjdyp"))

(define rust-foreign-types-macros-0.2.3
  (crate-source "foreign-types-macros" "0.2.3"
                "0hjpii8ny6l7h7jpns2cp9589016l8mlrpaigcnayjn9bdc6qp0s"))

(define rust-foreign-types-shared-0.1.1
  (crate-source "foreign-types-shared" "0.1.1"
                "0jxgzd04ra4imjv8jgkmdq59kj8fsz6w4zxsbmlai34h26225c00"))

(define rust-foreign-types-shared-0.3.1
  (crate-source "foreign-types-shared" "0.3.1"
                "0nykdvv41a3d4py61bylmlwjhhvdm0b3bcj9vxhqgxaxnp5ik6ma"))

(define rust-form-urlencoded-1.2.1
  (crate-source "form_urlencoded" "1.2.1"
                "0milh8x7nl4f450s3ddhg57a3flcv6yq8hlkyk6fyr3mcb128dp1"))

(define rust-fps-ticker-1.0.0
  (crate-source "fps_ticker" "1.0.0"
                "06cj5c5rk5grm2ajh4sabcppxr1h57gxfqacvi5psxb9zw2lj5py"))

(define rust-fs-err-2.11.0
  (crate-source "fs-err" "2.11.0"
                "0hdajzh5sjvvdjg0n15j91mv8ydvb7ff6m909frvdmg1bw81z948"))

(define rust-fs-err-3.1.0
  (crate-source "fs-err" "3.1.0"
                "1al2sj8src02wwww70vv2gypsrs6wyzx6zlpk82h84m2qajbv28z"))

(define rust-fs-extra-1.3.0
  (crate-source "fs_extra" "1.3.0"
                "075i25z70j2mz9r7i9p9r521y8xdj81q7skslyb7zhqnnw33fw22"))

(define rust-fs-utils-1.1.4
  (crate-source "fs-utils" "1.1.4"
                "14r5wl14mz227v0lpy89lvjzfnxgdxigvrrmm6c4r52w03fakivg"))

(define rust-fs2-0.4.3
  (crate-source "fs2" "0.4.3"
                "04v2hwk7035c088f19mfl5b1lz84gnvv2hv6m935n0hmirszqr4m"))

(define rust-fs4-0.12.0
  (crate-source "fs4" "0.12.0"
                "08gm0p6x133cav6yrcc3qhcr2qr1917yhj0bdx3psm0q8il31762"))

(define rust-fsevent-sys-4.1.0
  (crate-source "fsevent-sys" "4.1.0"
                "1liz67v8b0gcs8r31vxkvm2jzgl9p14i78yfqx81c8sdv817mvkn"))

(define rust-funty-2.0.0
  (crate-source "funty" "2.0.0"
                "177w048bm0046qlzvp33ag3ghqkqw4ncpzcm5lq36gxf2lla7mg6"))

(define rust-futf-0.1.5
  (crate-source "futf" "0.1.5"
                "0hvqk2r7v4fnc34hvc3vkri89gn52d5m9ihygmwn75l1hhp0whnz"))

(define rust-futures-0.3.31
  (crate-source "futures" "0.3.31"
                "0xh8ddbkm9jy8kc5gbvjp9a4b6rqqxvc8471yb2qaz5wm2qhgg35"))

(define rust-futures-channel-0.3.31
  (crate-source "futures-channel" "0.3.31"
                "040vpqpqlbk099razq8lyn74m0f161zd0rp36hciqrwcg2zibzrd"))

(define rust-futures-core-0.3.31
  (crate-source "futures-core" "0.3.31"
                "0gk6yrxgi5ihfanm2y431jadrll00n5ifhnpx090c2f2q1cr1wh5"))

(define rust-futures-executor-0.3.31
  (crate-source "futures-executor" "0.3.31"
                "17vcci6mdfzx4gbk0wx64chr2f13wwwpvyf3xd5fb1gmjzcx2a0y"))

(define rust-futures-io-0.3.31
  (crate-source "futures-io" "0.3.31"
                "1ikmw1yfbgvsychmsihdkwa8a1knank2d9a8dk01mbjar9w1np4y"))

(define rust-futures-lite-1.13.0
  (crate-source "futures-lite" "1.13.0"
                "1kkbqhaib68nzmys2dc8j9fl2bwzf2s91jfk13lb2q3nwhfdbaa9"))

(define rust-futures-lite-2.6.0
  (crate-source "futures-lite" "2.6.0"
                "0cmmgszlmkwsac9pyw5rfjakmshgx4wmzmlyn6mmjs0jav4axvgm"))

(define rust-futures-macro-0.3.31
  (crate-source "futures-macro" "0.3.31"
                "0l1n7kqzwwmgiznn0ywdc5i24z72zvh9q1dwps54mimppi7f6bhn"))

(define rust-futures-sink-0.3.31
  (crate-source "futures-sink" "0.3.31"
                "1xyly6naq6aqm52d5rh236snm08kw8zadydwqz8bip70s6vzlxg5"))

(define rust-futures-task-0.3.31
  (crate-source "futures-task" "0.3.31"
                "124rv4n90f5xwfsm9qw6y99755y021cmi5dhzh253s920z77s3zr"))

(define rust-futures-timer-3.0.3
  (crate-source "futures-timer" "3.0.3"
                "094vw8k37djpbwv74bwf2qb7n6v6ghif4myss6smd6hgyajb127j"))

(define rust-futures-util-0.3.31
  (crate-source "futures-util" "0.3.31"
                "10aa1ar8bgkgbr4wzxlidkqkcxf77gffyj8j7768h831pcaq784z"))

(define rust-fuzzy-matcher-0.3.7
  (crate-source "fuzzy-matcher" "0.3.7"
                "153csv8rsk2vxagb68kpmiknvdd3bzqj03x805khckck28rllqal"))

(define rust-fxhash-0.2.1
  (crate-source "fxhash" "0.2.1"
                "037mb9ichariqi45xm6mz0b11pa92gj38ba0409z3iz239sns6y3"))

(define rust-gbm-0.18.0
  (crate-source "gbm" "0.18.0"
                "0skyaj51xlazaa24jdkxxi2g6pnw834k3yqlf2ly999wincjx1ff"))

(define rust-gbm-sys-0.4.0
  (crate-source "gbm-sys" "0.4.0"
                "0vzp28ip4w74p05ygs4p9m7sspggn2zvcykbpyv8ypbqrhm5yfn1"))

(define rust-gdk-pixbuf-0.18.5
  (crate-source "gdk-pixbuf" "0.18.5"
                "1v7svvl0g7zybndmis5inaqqgi1mvcc6s1n8rkb31f5zn3qzbqah"))

(define rust-gdk-pixbuf-0.19.8
  (crate-source "gdk-pixbuf" "0.19.8"
                "16c6kznkh3vi82843ays8awdm37fwjd1fblv6g3h64824shsnkk2"))

(define rust-gdk-pixbuf-0.20.9
  (crate-source "gdk-pixbuf" "0.20.9"
                "1l0llkzf7v634h5a8dz6935xkf3ma3fqm9vhpggiw8hazzbayqvm"))

(define rust-gdk-pixbuf-sys-0.18.0
  (crate-source "gdk-pixbuf-sys" "0.18.0"
                "1xya543c4ffd2n7aiwwrdxsyc9casdbasafi6ixcknafckm3k61z"))

(define rust-gdk-pixbuf-sys-0.19.8
  (crate-source "gdk-pixbuf-sys" "0.19.8"
                "0y93g24mdgskvyhva46xv3qyb1cvj5xpi0yqnh7cb31wz2j0byjf"))

(define rust-gdk-pixbuf-sys-0.20.7
  (crate-source "gdk-pixbuf-sys" "0.20.7"
                "0p0b3lrzamsz580dyrr5i99i32ppsjp6mfmvfrs9kgq2j9y5iwk7"))

(define rust-gdk4-0.7.3
  (crate-source "gdk4" "0.7.3"
                "1xiacc63p73apr033gjrb9dsk0y4yxnsljwfxbwfry41snd03nvy"))

(define rust-gdk4-0.9.6
  (crate-source "gdk4" "0.9.6"
                "0q1dld01fgj7qxj644by0fc242mcn36w3bagn4z1mkdfq7cwjl28"))

(define rust-gdk4-sys-0.7.2
  (crate-source "gdk4-sys" "0.7.2"
                "1w7yvir565sjrrw828lss07749hfpfsr19jdjzwivkx36brl7ayv"))

(define rust-gdk4-sys-0.9.6
  (crate-source "gdk4-sys" "0.9.6"
                "0fj722lp86fpa1b1i3s2anavdmcpybd0b47mkhknzd72k1bvjvkg"))

(define rust-gdk4-wayland-0.7.2
  (crate-source "gdk4-wayland" "0.7.2"
                "04zkspjs1r6l4gj241p9xm2zmp91phm1khakw5jvsm8yy4pi1f8d"))

(define rust-gdk4-wayland-sys-0.7.2
  (crate-source "gdk4-wayland-sys" "0.7.2"
                "092nbn4gk82kbdvji2qnqy181l4pf5i8961bb8nj3q3a4nz5k0fl"))

(define rust-gdk4-win32-0.7.2
  (crate-source "gdk4-win32" "0.7.2"
                "0mv04mipl57v1lj94j2rkrk9qm75jvdlnp7qm6nl2kpn8466arpy"))

(define rust-gdk4-win32-sys-0.7.2
  (crate-source "gdk4-win32-sys" "0.7.2"
                "0v9rkv1i1sbzckigdr5zjy0xzz6qf9iccikvg3qxjfnd8rsihp2b"))

(define rust-gdk4-x11-0.7.2
  (crate-source "gdk4-x11" "0.7.2"
                "0l54c1m0gsdm07drvy171a0i97ic2kygmzf3fjg4da0yxbwbpj98"))

(define rust-gdk4-x11-sys-0.7.2
  (crate-source "gdk4-x11-sys" "0.7.2"
                "09gill32x6qy4s55xjckqvqrfxw4jfjrlcpmd4iijn076w4igpm3"))

(define rust-generator-0.8.4
  (crate-source "generator" "0.8.4"
                "1p9qqk9nzarjdcl5fr4iylvsv446g0svlpk63lxis4ysrqad2syc"))

(define rust-generic-array-0.13.3
  (crate-source "generic-array" "0.13.3"
                "02g3zhqc086zmsb6kcmjs2fiprz8gq12g0xbm9g23215ydxfd5zp"))

(define rust-generic-array-0.14.7
  (crate-source "generic-array" "0.14.7"
                "16lyyrzrljfq424c3n8kfwkqihlimmsg5nhshbbp48np3yjrqr45"))

(define rust-gethostname-0.4.3
  (crate-source "gethostname" "0.4.3"
                "063qqhznyckwx9n4z4xrmdv10s0fi6kbr17r6bi1yjifki2y0xh1"))

(define rust-getopts-0.2.21
  (crate-source "getopts" "0.2.21"
                "1mgb3qvivi26gs6ihqqhh8iyhp3vgxri6vwyrwg28w0xqzavznql"))

(define rust-getrandom-0.1.16
  (crate-source "getrandom" "0.1.16"
                "1kjzmz60qx9mn615ks1akjbf36n3lkv27zfwbcam0fzmj56wphwg"))

(define rust-getrandom-0.2.15
  (crate-source "getrandom" "0.2.15"
                "1mzlnrb3dgyd1fb84gvw10pyr8wdqdl4ry4sr64i1s8an66pqmn4"))

(define rust-getrandom-0.3.2
  (crate-source "getrandom" "0.3.2"
                "1w2mlixa1989v7czr68iji7h67yra2pbg3s480wsqjza1r2sizkk"))

(define rust-getset-0.0.9
  (crate-source "getset" "0.0.9"
                "0aaldwfs2690rjqg2ygan27l2qa614w2p6zj7k99n36pv2vzbcsv"))

(define rust-getset-0.1.5
  (crate-source "getset" "0.1.5"
                "1zpap947rb3rw9xlp7v37hs4zsykwdrdlcxp9qh8gpric4jnyn7k"))

(define rust-gettext-rs-0.7.2
  (crate-source "gettext-rs" "0.7.2"
                "12ikrzvx35aybip55ib9zmnjf8is4mhy2pfmgv50lhq8vkvr4km4"))

(define rust-gettext-sys-0.22.5
  (crate-source "gettext-sys" "0.22.5"
                "0qgnkr3gaf6pndai26ysmhyr8kwn8psmhifhxhmg2ic9bczpfidv"
                #:snippet '(delete-file "gettext-0.22.5.tar.xz")))

(define rust-ghash-0.5.1
  (crate-source "ghash" "0.5.1"
                "1wbg4vdgzwhkpkclz1g6bs4r5x984w5gnlsj4q5wnafb5hva9n7h"))

(define rust-gif-0.13.1
  (crate-source "gif" "0.13.1"
                "1whrkvdg26gp1r7f95c6800y6ijqw5y0z8rgj6xihpi136dxdciz"))

(define rust-gimli-0.28.1
  (crate-source "gimli" "0.28.1"
                "0lv23wc8rxvmjia3mcxc6hj9vkqnv1bqq0h8nzjcgf71mrxx6wa2"))

(define rust-gimli-0.31.1
  (crate-source "gimli" "0.31.1"
                "0gvqc0ramx8szv76jhfd4dms0zyamvlg4whhiz11j34hh3dqxqh7"))

(define rust-gio-0.18.4
  (crate-source "gio" "0.18.4"
                "0wsc6mnx057s4ailacg99dwgna38dbqli5x7a6y9rdw75x9qzz6l"))

(define rust-gio-0.19.8
  (crate-source "gio" "0.19.8"
                "1znz5ngfvv3gbndf6lzz3hs27hlb8ysls4axlfccrzvkscbz2jac"))

(define rust-gio-0.20.9
  (crate-source "gio" "0.20.9"
                "11vl4zkb3zvrklr7zhdlcyb35rbrm8d0xpbjfpm89782z1q0rw54"))

(define rust-gio-sys-0.18.1
 (crate-source "gio-sys" "0.18.1"
                "1lip8z35iy9d184x2qwjxlbxi64q9cpayy7v1p5y9xdsa3w6smip"))

(define rust-gio-sys-0.19.8
  (crate-source "gio-sys" "0.19.8"
                "1vylsskpipfwl7mvffp1s0227d0k5amyhd32dfnp3mhl8yx47mrc"))

(define rust-gio-sys-0.20.9
  (crate-source "gio-sys" "0.20.9"
                "17izngigdvv3gfda2qk059vcgihmxbaa7rjl3cz8r69618jva3hn"))

(define rust-git-version-0.3.9
  (crate-source "git-version" "0.3.9"
                "06ddi3px6l2ip0srn8512bsh8wrx4rzi65piya0vrz5h7nm6im8s"))

(define rust-git-version-macro-0.3.9
  (crate-source "git-version-macro" "0.3.9"
                "1h1s08fgh9bkwnc2hmjxcldv69hlxpq7a09cqdxsd5hb235hq0ak"))

(define rust-git2-0.19.0
  (crate-source "git2" "0.19.0"
                "091pv7866z1qjq800ys0wjv8n73wrv7fqdrddxcnq36w8lzbf0xr"))

(define rust-git2-0.20.1
  (crate-source "git2" "0.20.1"
                "1fgf67h78yrw2gm1n8ghgr0jwsbkvmjfhnbng9zrm2n68jxbh82j"))

(define rust-git2-curl-0.20.0
  (crate-source "git2-curl" "0.20.0"
                "17q7p4xdmvzn8jy75cdpl6bncy70z1v864wv0ch2690wg9919zv8"))

(define rust-gix-0.67.0
  (crate-source "gix" "0.67.0"
                "0zziyg31w7knv6cdizhm3fgxi8xg5ay64a5wpzix6s63va6ygly7"))

(define rust-gix-0.70.0
  (crate-source "gix" "0.70.0"
                "0s3b5407lqx9nf81xfrmka6l269551kkwm9blmpabwq5cxii8vvk"))

(define rust-gix-actor-0.33.2
  (crate-source "gix-actor" "0.33.2"
                "1cp47vxcd7f7nf225spdhncqqsrcjcf5qc68zkqnbq1jccd8l090"))

(define rust-gix-attributes-0.23.1
  (crate-source "gix-attributes" "0.23.1"
                "1p6a6ai3pk8c7xn48vbw7gvjh7rc5m13cbcsd7zfvh4l462vzyfx"))

(define rust-gix-attributes-0.24.0
  (crate-source "gix-attributes" "0.24.0"
                "0f6vdp77d5z98bv3w6i71zlaqcgf8bch4qfa3rj5zvv2yq5h0lgi"))

(define rust-gix-bitmap-0.2.14
  (crate-source "gix-bitmap" "0.2.14"
                "0h3msc00gi2vr2k4q41ddb68qprbvkih824glq6na0lmqrjrgnxi"))

(define rust-gix-chunk-0.4.11
  (crate-source "gix-chunk" "0.4.11"
                "0vxxq4q5pn5cz2xhghcjpp8z83r8xxy74gsffvf9k1lmcj3is7qb"))

(define rust-gix-command-0.3.11
  (crate-source "gix-command" "0.3.11"
                "0lzyg587s4rcrlvi42ml744ardqy6l5vh7hrx3bkyib47a7nnzbd"))

(define rust-gix-command-0.4.1
  (crate-source "gix-command" "0.4.1"
                "1wcdm6f8v28y2rv5lmz7kh4lnkdzplc92nh2c9gb8papss20nhfb"))

(define rust-gix-commitgraph-0.25.1
  (crate-source "gix-commitgraph" "0.25.1"
                "11cdlkbkv80imbdkiy8k09gb1c48k6qadpmxvavb53w6ly8nbnm8"))

(define rust-gix-commitgraph-0.26.0
  (crate-source "gix-commitgraph" "0.26.0"
                "0xs85svhri8b40paa3zjjxfqzl6g3ganxnxg1nhjcq51v318wfp2"))

(define rust-gix-config-0.41.0
  (crate-source "gix-config" "0.41.0"
                "0pj4mijnx46s2lq1sw78w82nq0brvvhfh1vjspllp6bv3jzx3v8b"))

(define rust-gix-config-0.43.0
  (crate-source "gix-config" "0.43.0"
                "1sfry54k4f35ar6y0d7n52ccwyq9r192kkdkw1lx9m8l43yiwz1p"))

(define rust-gix-config-value-0.14.12
  (crate-source "gix-config-value" "0.14.12"
                "1dj4g52s18ab01pnw55rd0qdf7frdxryzawccy21h56gqi2cihld"))

(define rust-gix-credentials-0.25.1
  (crate-source "gix-credentials" "0.25.1"
                "0wdfnq6y3za7h1xqj32af84zdzwg0r2irxrf0gkydiszd2w7ps1b"))

(define rust-gix-credentials-0.27.0
  (crate-source "gix-credentials" "0.27.0"
                "0icymf6v01y2r07bmwaw3vb1mx59m2x54lcb732bj2v9w6g0z5fg"))

(define rust-gix-date-0.9.4
  (crate-source "gix-date" "0.9.4"
                "1r0pc9ra4r7qxwsyd0jvxh3vsnm3jvkgkr19qbxi2dbxxic018ys"))

(define rust-gix-diff-0.47.0
  (crate-source "gix-diff" "0.47.0"
                "03i6v91k0bwyzzyjl2jp9rsx780v149hs4wydzdi7wasq780z1f9"))

(define rust-gix-diff-0.50.0
  (crate-source "gix-diff" "0.50.0"
                "0kbwn5js7qwnqxxva52hrhxrkwhvxfr6a86rvblz9k8arbsbgbv2"))

(define rust-gix-dir-0.9.0
  (crate-source "gix-dir" "0.9.0"
                "18rlpbjy14ljv1sq839skfn2x8f121gaspwjsjb3kbvvy6dw5xmv"))

(define rust-gix-discover-0.36.0
  (crate-source "gix-discover" "0.36.0"
                "1xkijvasm2c9a1pwjjc05xq8ydy5fc4f255hvw4syl4g8lgy68n5"))

(define rust-gix-discover-0.38.0
  (crate-source "gix-discover" "0.38.0"
                "1n35pfcr4didkxswigy4lvwkqrhyvbgjk82sb87lw1h4vx5l3hnh"))

(define rust-gix-features-0.39.1
  (crate-source "gix-features" "0.39.1"
                "07yqby9y0icx2l7kwbvxfg6z8b7gfznknwd4vd0a68p0y9rxd1bx"))

(define rust-gix-features-0.40.0
  (crate-source "gix-features" "0.40.0"
                "0m6mf6f341shzs5b1iy79klkw00x84kba34z5i4bshldia1x9zcb"))

(define rust-gix-filter-0.14.0
  (crate-source "gix-filter" "0.14.0"
                "1sk50qqkhvbql3slagm6y9sgc6zdbiqsx4w9xmq5fj54b4izhdvb"))

(define rust-gix-filter-0.17.0
  (crate-source "gix-filter" "0.17.0"
                "1frbjkmwrafbp7psbnh9rp9szlakcn44b1jmqc7fsqxwgp6kdk5x"))

(define rust-gix-fs-0.12.1
  (crate-source "gix-fs" "0.12.1"
                "1f8xifs0wkq7lhy3c8091kq2lx15qkynjb6fwnbiyqjsa2n4yg9v"))

(define rust-gix-fs-0.13.0
  (crate-source "gix-fs" "0.13.0"
                "0g86cb2i18c7jnj8i9691a3h07nz7hvinig7ryvzyi6zpykpybhq"))

(define rust-gix-glob-0.17.1
  (crate-source "gix-glob" "0.17.1"
                "0d9lrxas6zjia91j3m4z8rnazz1s02j9kgw4fib82d8aximrmxma"))

(define rust-gix-glob-0.18.0
  (crate-source "gix-glob" "0.18.0"
                "0kii7bpz1vcdykb0x1k9zmhn22hynwyk4n5acfrzjy0az94p572f"))

(define rust-gix-hash-0.15.1
  (crate-source "gix-hash" "0.15.1"
                "1kp4yjlkp8g4qg0r2zs0jmz19r076f2y91cjsikhxvclf70wqphb"))

(define rust-gix-hash-0.16.0
  (crate-source "gix-hash" "0.16.0"
                "1y79zcwja9b1bqlr27awndla5wcmzd7a8rnh7qdq5ca9hv25w778"))

(define rust-gix-hashtable-0.6.0
  (crate-source "gix-hashtable" "0.6.0"
                "1zhqgncv6jh3x7a7a2w3qbayghmiwv230mdw6gvqw1ricqjmpxhf"))

(define rust-gix-hashtable-0.7.0
  (crate-source "gix-hashtable" "0.7.0"
                "1l8jq85fkfw4inmpd6w2pk1dq67krsqmmp100lpd1k1a6yy3148q"))

(define rust-gix-ignore-0.12.1
  (crate-source "gix-ignore" "0.12.1"
                "12mv0lgq8aviy6fc4mdxr7r0ra0l1kb729wf8fkhmbx4s8jgpcdn"))

(define rust-gix-ignore-0.13.0
  (crate-source "gix-ignore" "0.13.0"
                "0vyz5jfqd72b4pygwqrssr96jvfzi32hm7y4lz05b65zh35rsljg"))

(define rust-gix-index-0.36.0
  (crate-source "gix-index" "0.36.0"
                "0agycrg9hywdn89sj8hxbhx1c2aszbsp64h4hpc3z8qyr84r0q97"))

(define rust-gix-index-0.38.0
  (crate-source "gix-index" "0.38.0"
                "1n45vkbmkpc4m570rdanyqz62a68mihsrqpz1wqnk4w74qv2xldc"))

(define rust-gix-lock-15.0.1
  (crate-source "gix-lock" "15.0.1"
                "0h6r088yv5fk0d14zihssfh1zfhdyc8cpnpbygcn7nsjlilaplqw"))

(define rust-gix-lock-16.0.0
  (crate-source "gix-lock" "16.0.0"
                "0hn696w506zwqfl9pjhijaqkshzr5lb4v0j1hjb40sgzf1982fcp"))

(define rust-gix-negotiate-0.16.0
  (crate-source "gix-negotiate" "0.16.0"
                "1gfhnzjv0q2gj27xwgdx576q8kw5zx0diiirm6g39hrq30lhcj21"))

(define rust-gix-negotiate-0.18.0
  (crate-source "gix-negotiate" "0.18.0"
                "107gh0yn4z1lnzljlr538gg5bs9k9mzjncam1g9h7qxvywgaza56"))

(define rust-gix-object-0.45.0
  (crate-source "gix-object" "0.45.0"
                "06pwqvxwr3appcw3k63hj6jfg0a4j921g2xfv59qaa9xfpkvcxra"))

(define rust-gix-object-0.47.0
  (crate-source "gix-object" "0.47.0"
                "0s7xwm1nmx2zp10qnrlxh8vmw5nakjkvfzrl4bzg0i220jhb7i6x"))

(define rust-gix-odb-0.64.0
  (crate-source "gix-odb" "0.64.0"
                "0q8gwv4mdm8jqmfr73q0z009fvvh151wjkqvc20gkcpiyynnmf0b"))

(define rust-gix-odb-0.67.0
  (crate-source "gix-odb" "0.67.0"
                "06ww8mc10iydvqxdin0miny89g9z8i7zmsccc1rrbl4wyrylb4ry"))

(define rust-gix-pack-0.54.0
  (crate-source "gix-pack" "0.54.0"
                "0sq240glmpvp0x1bpsngrlk82iz2d3dkk0a0f8v29fjmm1cnwgin"))

(define rust-gix-pack-0.57.0
  (crate-source "gix-pack" "0.57.0"
                "05d57xpzk35i2cclnb9iclvm1gvrc20mzcvz04bmcwyvndss84zw"))

(define rust-gix-packetline-0.18.4
  (crate-source "gix-packetline" "0.18.4"
                "011sdpf03fp066v9q8zcjjz63vwavaqbl1nw84j3bmgl1jkl8f0j"))

(define rust-gix-packetline-blocking-0.18.3
  (crate-source "gix-packetline-blocking" "0.18.3"
                "1crknbdkdnh5aanpyda3dhm2c0a3hacl1h5shxay9iq5w6i3xkqy"))

(define rust-gix-path-0.10.15
  (crate-source "gix-path" "0.10.15"
                "11xylymhw8maxv5z81w5hrxry10sibw4vw516pzmaakb5y76c47r"))

(define rust-gix-pathspec-0.8.1
  (crate-source "gix-pathspec" "0.8.1"
                "07mqfl6232285yzsmym2vr7vndwh3ivx9p7xgv7nzsd4wkxjsisc"))

(define rust-gix-pathspec-0.9.0
  (crate-source "gix-pathspec" "0.9.0"
                "0v7q0b55fn0raaj52cg75bi5yc8pijkzl1lq05crv3n0hskd6c34"))

(define rust-gix-prompt-0.8.9
  (crate-source "gix-prompt" "0.8.9"
                "1505js24g8dziljc7jl5frmk0af1847v106fqsxmz75wqjpj4y3s"))

(define rust-gix-prompt-0.9.1
  (crate-source "gix-prompt" "0.9.1"
                "0v9v9icnryhcx2z256kmm8aa1p31ipghjx80kac2nlg1b1ciiwkr"))

(define rust-gix-protocol-0.46.1
  (crate-source "gix-protocol" "0.46.1"
                "1jmq10azisdp4k1i18hif4cdxchrm4ppwacc8k9k39fyl18pwzks"))

(define rust-gix-protocol-0.48.0
  (crate-source "gix-protocol" "0.48.0"
                "145sln6g810vab9jhwiz3r1bwr61jh1i1qj168hpvdn6mxhvsqbc"))

(define rust-gix-quote-0.4.15
  (crate-source "gix-quote" "0.4.15"
                "1ik6l3s0hjb2p4dlgdarb59v7n9jvgvak4ij786mrj5hrpy5g4z4"))

(define rust-gix-ref-0.48.0
  (crate-source "gix-ref" "0.48.0"
                "18mfzrnp1308g5c454xwa85dz3c0913fyhp66n6dmnd23zkqawx4"))

(define rust-gix-ref-0.50.0
  (crate-source "gix-ref" "0.50.0"
                "03723r9s3m3grmjzcasxp7jcz0z5xs90spg9aj2ryhikz72z9ba7"))

(define rust-gix-refspec-0.26.0
  (crate-source "gix-refspec" "0.26.0"
                "0hn4mbnvcammpwrqcawpysbqv1h2np5yzs1vfyzrl3fq165068h0"))

(define rust-gix-refspec-0.28.0
  (crate-source "gix-refspec" "0.28.0"
                "140aif2nciz9j9a0h9lqsg8cb1pkzhbza9bsgy7gc4pnv0l04rar"))

(define rust-gix-revision-0.30.0
  (crate-source "gix-revision" "0.30.0"
                "1wam9d627191a4qdfjjj8lryk44z0qg7apaamxi3bkpyi10fps2f"))

(define rust-gix-revision-0.32.0
  (crate-source "gix-revision" "0.32.0"
                "0lvb7rrjjdr9h21ign5g0za2jg00nimzqvkcdvbacpd5rjy8pqiz"))

(define rust-gix-revwalk-0.16.0
  (crate-source "gix-revwalk" "0.16.0"
                "1cirkpxgz52mvib9lw1vb0jp9a09pxv8afh637zkd3d9dm4skjg6"))

(define rust-gix-revwalk-0.18.0
  (crate-source "gix-revwalk" "0.18.0"
                "0iv2c479w9lkjwbngdwyial6km8dllgah8wvp7r9w7jv4c6biv6l"))

(define rust-gix-sec-0.10.12
  (crate-source "gix-sec" "0.10.12"
                "122qvp6sll7hkrpjiwf9ga1dni0gwf6j3zzm6cq2zvz97pqv1bj7"))

(define rust-gix-shallow-0.2.0
  (crate-source "gix-shallow" "0.2.0"
                "0rjhwcjjixfy4fbzciyz5mikkvq38rwfyny86ckya0z324q58wmb"))

(define rust-gix-submodule-0.15.0
  (crate-source "gix-submodule" "0.15.0"
                "0yj9y2b7425a3bc2wp2sy7z50zialdv230pwh32kdkbk31i9kl1y"))

(define rust-gix-submodule-0.17.0
  (crate-source "gix-submodule" "0.17.0"
                "1b532y2c7qg8axqc2nkw2mdiq8mg9hxq87mfj2aa1j3askl2z5vl"))

(define rust-gix-tempfile-15.0.0
  (crate-source "gix-tempfile" "15.0.0"
                "10nvk82g7fhljg5y63dxpd8p7296wrfzxyssk957misc17pqdsrg"))

(define rust-gix-tempfile-16.0.0
  (crate-source "gix-tempfile" "16.0.0"
                "00c5czgzzi3c8yxv24vh1rmkgf06vgb1ypf5521lmwjyjhiz8n15"))

(define rust-gix-trace-0.1.12
  (crate-source "gix-trace" "0.1.12"
                "1xv54v5y91vxjx351wl3yk66fwk7ybkna2knbxlnj34j6qh6lfbw"))

(define rust-gix-transport-0.43.1
  (crate-source "gix-transport" "0.43.1"
                "02r0fwai9pq6f2n1nn588pjc71rxh9zi9169w01nq8xpaw9s989r"))

(define rust-gix-transport-0.45.0
  (crate-source "gix-transport" "0.45.0"
                "1nb4p7jwy80g51afzc64ya1faxxcpgnimbk2p2sv2xwl90c7860i"))

(define rust-gix-traverse-0.42.0
  (crate-source "gix-traverse" "0.42.0"
                "1pqqx02bab9101iqry4f8nsbwd3azg1a0sjfna9bm9jgrh9in3zj"))

(define rust-gix-traverse-0.44.0
  (crate-source "gix-traverse" "0.44.0"
                "1d311l7wlgpv41hvp1ni3r9hhwxn4x27xyiy5brnwn4n73jp1v1b"))

(define rust-gix-url-0.28.2
  (crate-source "gix-url" "0.28.2"
                "1ncj6k4lk3qb0i27ida7ngi9z06qpmrbva6v0da3zgd67drzp5nh"))

(define rust-gix-url-0.29.0
  (crate-source "gix-url" "0.29.0"
                "04qb2p68886axrbx5gdjlhqwg55j0pn7zn25c08qzpakidv8q899"))

(define rust-gix-utils-0.1.14
  (crate-source "gix-utils" "0.1.14"
                "0pykxyp0cm2x8lj4ryj1pflksf9k7iyrshf8g321d2dc0d7g427z"))

(define rust-gix-validate-0.9.4
  (crate-source "gix-validate" "0.9.4"
                "11204daz5qlk9kqnmiq4syv0n21phkiy3xkwxmwnrnh964jz3d9l"))

(define rust-gix-worktree-0.37.0
  (crate-source "gix-worktree" "0.37.0"
                "177j311n46ysiyb52x68rwf02lp7gnavy4p9l17zwl1ma9dmwd0d"))

(define rust-gix-worktree-0.39.0
  (crate-source "gix-worktree" "0.39.0"
                "0n49fywzh1f4gmv7gwd4d5jnq7ahiabsdv6wda3scmxagqpm2wv6"))

(define rust-gix-worktree-state-0.17.0
  (crate-source "gix-worktree-state" "0.17.0"
                "1w2vaz776v13hrnzhnsihmcbhb6883b33gc3cq475yasmncy3xc6"))

(define rust-gl-generator-0.14.0
  (crate-source "gl_generator" "0.14.0"
                "0k8j1hmfnff312gy7x1aqjzcm8zxid7ij7dlb8prljib7b1dz58s"))

(define rust-glam-0.30.1
  (crate-source "glam" "0.30.1"
                "1yfm49g6xqwfdg9dw0q6px12a5wgc87qakzz4r0j6awdj46sffmz"))

(define rust-glib-0.18.5
  (crate-source "glib" "0.18.5"
                "1r8fw0627nmn19bgk3xpmcfngx3wkn7mcpq5a8ma3risx3valg93"))

(define rust-glib-0.19.9
  (crate-source "glib" "0.19.9"
                "0i2ak1scmzfmfxbm2dr146jl4y9mafxf1ald05jr8iimy5wh4r9r"))

(define rust-glib-0.20.9
  (crate-source "glib" "0.20.9"
                "11knyc1lgd0bkw42ysl4v2x3v9c7glqz5s9db8wyb7h5z2d82yvh"))

(define rust-glib-macros-0.18.5
  (crate-source "glib-macros" "0.18.5"
                "1p5cla53fcp195zp0hkqpmnn7iwmkdswhy7xh34002bw8y7j5c0b"))

(define rust-glib-macros-0.19.9
  (crate-source "glib-macros" "0.19.9"
                "1mzsh8jkg8vldvgvr9gsaidvn2myn5cbdn8a6m8rgbhlg8kv0aa4"))

(define rust-glib-macros-0.20.7
  (crate-source "glib-macros" "0.20.7"
                "0s6yik6pgqg5wydcz5v0x8m1jz57m5bsd50zkkpvlw9fy3w02mki"))

(define rust-glib-sys-0.18.1
  (crate-source "glib-sys" "0.18.1"
                "164qhsfmlzd5mhyxs8123jzbdfldwxbikfpq5cysj3lddbmy4g06"))

(define rust-glib-sys-0.19.8
  (crate-source "glib-sys" "0.19.8"
                "19f4q8x77vd7c1d9ikw492yskq5kpd7k04qb8xnh1c427a6w2baw"))

(define rust-glib-sys-0.20.9
  (crate-source "glib-sys" "0.20.9"
                "1yxfqf6wllka0am0brqwwj18yb7q9xp79mhprgyd3zaclilqi4m8"))

(define rust-glob-0.3.2
  (crate-source "glob" "0.3.2"
                "1cm2w34b5w45fxr522h5b0fv1bxchfswcj560m3pnjbia7asvld8"))

(define rust-glob-match-0.2.1
  (crate-source "glob-match" "0.2.1"
                "178bjn684dd50px9n8lwa72fn94566d9wmcp86m9h8a17d8ck1cr"))

(define rust-globset-0.4.16
  (crate-source "globset" "0.4.16"
                "1xa9ivqs74imf1q288spxh49g6iw2mn3x9snibdgapazzj6h58al"))

(define rust-globwalk-0.9.1
  (crate-source "globwalk" "0.9.1"
                "0mz7bsa66p2rrgnz3l94ac4kbklh7mq8j30iizyxjy4qyvmn1xqb"))

(define rust-gloo-timers-0.3.0
  (crate-source "gloo-timers" "0.3.0"
                "1519157n7xppkk6pdw5w52vy1llzn5iljkqd7q1h5609jv7l7cdv"))

(define rust-glow-0.16.0
  (crate-source "glow" "0.16.0"
                "022z12nlyfpy36fvp2szq792xix1xbgkznpmicf1c404sxhfmrf5"))

(define rust-gobject-sys-0.18.0
  (crate-source "gobject-sys" "0.18.0"
                "0i6fhp3m6vs3wkzyc22rk2cqj68qvgddxmpaai34l72da5xi4l08"))

(define rust-gobject-sys-0.19.8
  (crate-source "gobject-sys" "0.19.8"
                "17lb7dfbpcg8zchwlfbc08kckwf0a7d9n5ly3pyic13f5ljpws9f"))

(define rust-gobject-sys-0.20.9
  (crate-source "gobject-sys" "0.20.9"
                "1qz7jrpfk0z8mhnz7fxxx208kc5ljryif7f84sfas6d4735s6wy7"))

(define rust-goblin-0.7.1
  (crate-source "goblin" "0.7.1"
                "0d11fk9bdxzf228xpr8v6d6a01dib00khjg5bldk9kf2d51inz7j"))

(define rust-goblin-0.9.3
  (crate-source "goblin" "0.9.3"
                "0ifpcsp0hpp7lx10yqln9ybmfkky7gig9idlhc2j7sx7456sd86s"))

(define rust-graphene-rs-0.18.1
  (crate-source "graphene-rs" "0.18.1"
                "00f4q1ra4haap5i7lazwhkdgnb49fs8adk2nm6ki6mjhl76jh8iv"))

(define rust-graphene-rs-0.20.9
  (crate-source "graphene-rs" "0.20.9"
                "06fy773j9r0v7xzwv3sl2pvw55i68q653jcjzbf6hbdkpw8mkg1w"))

(define rust-graphene-sys-0.18.1
  (crate-source "graphene-sys" "0.18.1"
                "0n8zlg7z26lwpnvlqp1hjlgrs671skqwagdpm7r8i1zwx3748hfc"))

(define rust-graphene-sys-0.20.7
  (crate-source "graphene-sys" "0.20.7"
                "0fnjh55lnrd8mgladbapfxak44swlbafqb5pg7l41wsva4wqv9hi"))

(define rust-greetd-ipc-0.10.3
  (crate-source "greetd_ipc" "0.10.3"
                "0y1095qwmiwpm94z3vgbklbnlp088pi6vd1isp8l584izidjw3bh"))

(define rust-group-0.13.0
  (crate-source "group" "0.13.0"
                "0qqs2p5vqnv3zvq9mfjkmw3qlvgqb0c3cm6p33srkh7pc9sfzygh"))

(define rust-gsk4-0.7.3
  (crate-source "gsk4" "0.7.3"
                "0zhzs2dkgiinhgc11akpn2harq3x5n1iq21dnc4h689g3lsqx58d"))

(define rust-gsk4-0.9.6
  (crate-source "gsk4" "0.9.6"
                "0mgqq5m6cm4q7ajjgw92z13z2ikpvh6zx2gwzdjrz30wjcpygxb1"))

(define rust-gsk4-sys-0.7.3
  (crate-source "gsk4-sys" "0.7.3"
                "0mbdlm9qi1hql48rr29vsj9vlqwc7gxg67wg1q19z67azwz9xg8j"))

(define rust-gsk4-sys-0.9.6
  (crate-source "gsk4-sys" "0.9.6"
                "1p1n4jhhxyvj7hb0cqhzvazrck0qw81sz36ydfj8avzsapg5jl3m"))

(define rust-gst-plugin-gtk4-0.11.4
  (crate-source "gst-plugin-gtk4" "0.11.4"
                "1hmky9p16hnhrwm5i63ynlwfl1bpc9fp3as5ibrni1qlfq0vhwdj"))

(define rust-gst-plugin-version-helper-0.8.2
  (crate-source "gst-plugin-version-helper" "0.8.2"
                "0alv0v7jfg7ryybb3qnbdwx3nqzkdl305il1xk92y9b02r7qfpjf"))

(define rust-gstreamer-0.21.3
  (crate-source "gstreamer" "0.21.3"
                "0mchpvvll5i4ck8zr7aarrz6p975n0dcyy92wksg8ycf9hzp15fy"))

(define rust-gstreamer-audio-0.21.3
  (crate-source "gstreamer-audio" "0.21.3"
                "0b91wjhhq0harwl7kyfv5l0kwp6w1vklpnpynakv92f8x6jci5vs"))

(define rust-gstreamer-audio-sys-0.21.1
  (crate-source "gstreamer-audio-sys" "0.21.1"
                "1lamp4s9cl0hhpbfcwdprn36fll6qq4xihrqbf2pfwqpifp99gbq"))

(define rust-gstreamer-base-0.21.2
  (crate-source "gstreamer-base" "0.21.2"
                "0zqnld0w2jqkz1m5xna3a3nnrpvrchpcrrzdgwim54540ilhn5fb"))

(define rust-gstreamer-base-sys-0.21.1
  (crate-source "gstreamer-base-sys" "0.21.1"
                "0r2bb4rmkpxs1l2jy61rn2srqzsp1f8q0k5j55di3zkqj0gp1jpl"))

(define rust-gstreamer-gl-0.21.2
  (crate-source "gstreamer-gl" "0.21.2"
                "140vnlxnkq12c8qqgc5i2y9wdz8c8dga25d99021cg16508vkkry"))

(define rust-gstreamer-gl-egl-0.21.2
  (crate-source "gstreamer-gl-egl" "0.21.2"
                "10nwlmyw1z4jccyrbqijx6iny2c64164jaz05dgnvi5378ianwx1"))

(define rust-gstreamer-gl-egl-sys-0.21.2
  (crate-source "gstreamer-gl-egl-sys" "0.21.2"
                "0m79m0lbk2s89cm4vc6ckwkgs9khmh2ri7x3gfgmz2hwy2v8hg7f"))

(define rust-gstreamer-gl-sys-0.21.2
  (crate-source "gstreamer-gl-sys" "0.21.2"
                "1kgi8rrlw2qx1p6q9ybk52wxpjn5wscx84lqfg4ng9lr7hdrg06m"))

(define rust-gstreamer-gl-wayland-0.21.1
  (crate-source "gstreamer-gl-wayland" "0.21.1"
                "1zz7as2qlf021dvpy1qs4rbahf94p6jb1msmfsgx08nhyai7dhpy"))

(define rust-gstreamer-gl-wayland-sys-0.21.1
  (crate-source "gstreamer-gl-wayland-sys" "0.21.1"
                "188j8i47zn93gph6ngmpjqbvm44jy0wzybr5052s6lxadzqqcywi"))

(define rust-gstreamer-gl-x11-0.21.1
  (crate-source "gstreamer-gl-x11" "0.21.1"
                "0zgn5aih3bcz3ci3xkdpc2jzxrxiz1hdpkwq121w5ln96ag1n0np"))

(define rust-gstreamer-gl-x11-sys-0.21.1
  (crate-source "gstreamer-gl-x11-sys" "0.21.1"
                "1p5wdrn3h55jx15963z1wnk7whwplpjfymy5yjsmkqdrqw1yz6n4"))

(define rust-gstreamer-pbutils-0.21.3
  (crate-source "gstreamer-pbutils" "0.21.3"
                "0idr354x9j77q8qrb99r6m6hrpa0z8j97jncqim5m08vhgbij9sb"))

(define rust-gstreamer-pbutils-sys-0.21.0
  (crate-source "gstreamer-pbutils-sys" "0.21.0"
                "0scx3w02wkyvmq76ia2jr6zhkf24zivn9vyphrcwmj2b8piydakg"))

(define rust-gstreamer-sys-0.21.2
  (crate-source "gstreamer-sys" "0.21.2"
                "1i1vrqs9ys5y0ljl4nxh1x25dnwlcyh9hiybh4dysviy5dwdlk2n"))

(define rust-gstreamer-video-0.21.2
  (crate-source "gstreamer-video" "0.21.2"
                "1r8mhzb1bq4dnj08f4szgarxd2fvqbakwv400fp9hyiv3m6jlnz8"))

(define rust-gstreamer-video-sys-0.21.2
  (crate-source "gstreamer-video-sys" "0.21.2"
                "1vhn7fiibwc2q5h8cjlg44imh8i0xss6nns83r859c76k26k20h3"))

(define rust-gtk4-0.7.3
  (crate-source "gtk4" "0.7.3"
                "0hh8nzglmz94v1m1h6vy8z12m6fr7ia467ry0md5fa4p7sm53sss"))

(define rust-gtk4-0.9.6
  (crate-source "gtk4" "0.9.6"
                "078911sc8wvnihlz3kq80chl0miz9z2g7rnds17rjc7ha484j75g"))

(define rust-gtk4-macros-0.7.2
  (crate-source "gtk4-macros" "0.7.2"
                "0bw3cchiycf7dw1bw4p8946gv38azxy05a5w0ndgcmxnz6fc8znm"))

(define rust-gtk4-macros-0.9.5
  (crate-source "gtk4-macros" "0.9.5"
                "169rqfxfczivcpz7019slsrpkx8crqjka43ymxmikp838xn7il8f"))

(define rust-gtk4-sys-0.7.3
  (crate-source "gtk4-sys" "0.7.3"
                "1f2ylskyqkjdik9fij2m46pra4jagnif5xyalbxfk3334fmc9n2l"))

(define rust-gtk4-sys-0.9.6
  (crate-source "gtk4-sys" "0.9.6"
                "1mh3xjkjb99y97z234cvyar08vcr7zblg1nrw48c6xsdwl0kpq21"))

(define rust-h2-0.3.26
  (crate-source "h2" "0.3.26"
                "1s7msnfv7xprzs6xzfj5sg6p8bjcdpcqcmjjbkd345cyi1x55zl1"))

(define rust-h2-0.4.8
  (crate-source "h2" "0.4.8"
                "1hp3lijg1br982kzgglb5ks2ibg68a76z3rl052r8c5vyi7jj5sh"))

(define rust-half-1.8.3
  (crate-source "half" "1.8.3"
                "00q4ki8ycdswapw6xn1q89vr7rzi1c8m99igps0lx1i1gzhyshqv"))

(define rust-half-2.5.0
  (crate-source "half" "2.5.0"
                "1ldv2i761fjqxl4rn033nasjrdnw5ysnc1xalsfkfl5skc9zzckx"))

(define rust-half-2.6.0
  (crate-source "half" "2.6.0"
                "1j83v0xaqvrw50ppn0g33zig0zsbdi7xiqbzgn7sd5al57nrd4a5"))

(define rust-handlebars-4.5.0
  (crate-source "handlebars" "4.5.0"
                "09dj4rk6r9ngy1ki34mppcqq4pcnlhjd02yhnf724qpkkympp9ps"))

(define rust-hash32-0.2.1
  (crate-source "hash32" "0.2.1"
                "0rrbv5pc5b1vax6j6hk7zvlrpw0h6aybshxy9vbpgsrgfrc5zhxh"))

(define rust-hashbrown-0.12.3
  (crate-source "hashbrown" "0.12.3"
                "1268ka4750pyg2pbgsr43f0289l5zah4arir2k4igx5a8c6fg7la"))

(define rust-hashbrown-0.14.5
  (crate-source "hashbrown" "0.14.5"
                "1wa1vy1xs3mp11bn3z9dv0jricgr6a2j0zkf1g19yz3vw4il89z5"))

(define rust-hashbrown-0.15.2
  (crate-source "hashbrown" "0.15.2"
                "12dj0yfn59p3kh3679ac0w1fagvzf4z2zp87a13gbbqbzw0185dz"))

(define rust-hashlink-0.10.0
  (crate-source "hashlink" "0.10.0"
                "1h8lzvnl9qxi3zyagivzz2p1hp6shgddfmccyf6jv7s1cdicz0kk"))

(define rust-hashlink-0.9.1
  (crate-source "hashlink" "0.9.1"
                "1byq4nyrflm5s6wdx5qwp96l1qbp2d0nljvrr5yqrsfy51qzz93b"))

(define rust-heapless-0.7.17
  (crate-source "heapless" "0.7.17"
                "0kwn2wzk9fnsqnwp6rqjqhvh6hfq4rh225xwqjm72b5n1ry4bind"))

(define rust-heck-0.3.3
  (crate-source "heck" "0.3.3"
                "0b0kkr790p66lvzn9nsmfjvydrbmh9z5gb664jchwgw64vxiwqkd"))

(define rust-heck-0.4.1
  (crate-source "heck" "0.4.1"
                "1a7mqsnycv5z4z5vnv1k34548jzmc0ajic7c1j8jsaspnhw5ql4m"))

(define rust-heck-0.5.0
  (crate-source "heck" "0.5.0"
                "1sjmpsdl8czyh9ywl3qcsfsq9a307dg4ni2vnlwgnzzqhc4y0113"))

(define rust-hermit-abi-0.1.19
  (crate-source "hermit-abi" "0.1.19"
                "0cxcm8093nf5fyn114w8vxbrbcyvv91d4015rdnlgfll7cs6gd32"))

(define rust-hermit-abi-0.3.9
  (crate-source "hermit-abi" "0.3.9"
                "092hxjbjnq5fmz66grd9plxd0sh6ssg5fhgwwwqbrzgzkjwdycfj"))

(define rust-hermit-abi-0.4.0
  (crate-source "hermit-abi" "0.4.0"
                "1k1zwllx6nfq417hy38x4akw1ivlv68ymvnzyxs76ffgsqcskxpv"))

(define rust-hermit-abi-0.5.0
  (crate-source "hermit-abi" "0.5.0"
                "0zp3khi7bl6x8gazm9i9dgjf4h47mj10v4j18i3823y3bkz81mzv"))

(define rust-hex-0.4.3
  (crate-source "hex" "0.4.3"
                "0w1a4davm1lgzpamwnba907aysmlrnygbqmfis2mqjx5m552a93z"))

(define rust-hex-literal-0.4.1
  (crate-source "hex-literal" "0.4.1"
                "0iny5inkixsdr41pm2vkqh3fl66752z5j5c0cdxw16yl9ryjdqkg"))

(define rust-hkdf-0.12.4
  (crate-source "hkdf" "0.12.4"
                "1xxxzcarz151p1b858yn5skmhyrvn8fs4ivx5km3i1kjmnr8wpvv"
                #:snippet '(delete-file-recursively "tests")))

(define rust-hmac-0.12.1
  (crate-source "hmac" "0.12.1"
                "0pmbr069sfg76z7wsssfk5ddcqd9ncp79fyz6zcm6yn115yc6jbc"
                #:snippet '(delete-file-recursively "tests")))

(define rust-home-0.5.11
  (crate-source "home" "0.5.11"
                "1kxb4k87a9sayr8jipr7nq9wpgmjk4hk4047hmf9kc24692k75aq"))

(define rust-homedir-0.3.4
  (crate-source "homedir" "0.3.4"
                "18kb7myfvzzixv02k066477k11zzbaj2yddarjbrcx65r1dvvnsv"))

(define rust-html-escape-0.2.13
  (crate-source "html-escape" "0.2.13"
                "0xml3hswv0205fbm5iq7dqiwjkr6d245xkfppwi7wqjdfr4x86kd"))

(define rust-hts-sys-2.2.0
  (crate-source "hts-sys" "2.2.0"
                "1cmvdwssd6xjk6w1iigaj5rl9ibx4zaaskfb2ji2mlhw28f7z3g3"
                #:snippet
                '(begin
                   (for-each delete-file-recursively
                             '("htslib/htscodecs/tests" "htslib/test"))
                   (substitute* "Cargo.toml"
                     ;; Do not use zlib-ng; just use zlib.
                     (("\"zlib-ng\",") "")
                     (("\"static\",") "")
                     ;; No static libraries please in curl-sys.
                     (("\"static-curl\",") "")
                     (("\"static-ssl\",") "\"ssl\",")
                     ;; No static lzma.
                     (("\\[\"static\"\\]") "[]")))))

(define rust-http-0.2.12
  (crate-source "http" "0.2.12"
                "1w81s4bcbmcj9bjp7mllm8jlz6b31wzvirz8bgpzbqkpwmbvn730"))

(define rust-http-1.3.1
  (crate-source "http" "1.3.1"
                "0r95i5h7dr1xadp1ac9453w0s62s27hzkam356nyx2d9mqqmva7l"))

(define rust-http-auth-0.1.10
  (crate-source "http-auth" "0.1.10"
                "08l8z75cpda5y25cnd5fzgsahb35xn29qlgl9j12dy9f8sls83qm"))

(define rust-http-body-0.4.6
  (crate-source "http-body" "0.4.6"
                "1lmyjfk6bqk6k9gkn1dxq770sb78pqbqshga241hr5p995bb5skw"))

(define rust-http-body-1.0.1
  (crate-source "http-body" "1.0.1"
                "111ir5k2b9ihz5nr9cz7cwm7fnydca7dx4hc7vr16scfzghxrzhy"))

(define rust-http-body-util-0.1.3
  (crate-source "http-body-util" "0.1.3"
                "0jm6jv4gxsnlsi1kzdyffjrj8cfr3zninnxpw73mvkxy4qzdj8dh"))

(define rust-http-content-range-0.2.1
  (crate-source "http-content-range" "0.2.1"
                "1jmzikxrqmjhkyw69b894sapr1bkyc7y7ca3vl5xg5hlkw58xaml"))

(define rust-httparse-1.10.1
  (crate-source "httparse" "1.10.1"
                "11ycd554bw2dkgw0q61xsa7a4jn1wb1xbfacmf3dbwsikvkkvgvd"))

(define rust-httpdate-1.0.3
  (crate-source "httpdate" "1.0.3"
                "1aa9rd2sac0zhjqh24c9xvir96g188zldkx0hr6dnnlx5904cfyz"))

(define rust-human-date-parser-0.2.0
  (crate-source "human-date-parser" "0.2.0"
                "0qqwf20wz13ww06i1xqr9hfbg4c598f34n442q90qxzyxd6wy5hi"))

(define rust-humantime-2.2.0
  (crate-source "humantime" "2.2.0"
                "17rz8jhh1mcv4b03wnknhv1shwq2v9vhkhlfg884pprsig62l4cv"))

(define rust-humantime-serde-1.1.1
  (crate-source "humantime-serde" "1.1.1"
                "0310ri4zb33qbwqv0n51xysfjpnwc6rgxscl5i09jgcjlmgdp8sp"))

(define rust-hyper-0.14.32
  (crate-source "hyper" "0.14.32"
                "1rvcb0smz8q1i0y6p7rwxr02x5sclfg2hhxf3g0774zczn0cgps1"))

(define rust-hyper-1.6.0
  (crate-source "hyper" "1.6.0"
                "103ggny2k31z0iq2gzwk2vbx601wx6xkpjpxn40hr3p3b0b5fayc"))

(define rust-hyper-rustls-0.27.5
  (crate-source "hyper-rustls" "0.27.5"
                "1cjr3yf3x5mr3194llsfibacl6j7n2dknii2dwjha4ysyf1ia69d"))

(define rust-hyper-tls-0.5.0
  (crate-source "hyper-tls" "0.5.0"
                "01crgy13102iagakf6q4mb75dprzr7ps1gj0l5hxm1cvm7gks66n"))

(define rust-hyper-tls-0.6.0
  (crate-source "hyper-tls" "0.6.0"
                "1q36x2yps6hhvxq5r7mc8ph9zz6xlb573gx0x3yskb0fi736y83h"))

(define rust-hyper-util-0.1.11
  (crate-source "hyper-util" "0.1.11"
                "1wj3svb1r6yv6kgk5fsz6wwajmngc4zxcw4wxpwlmpbgl8rvqys9"))

(define rust-iana-time-zone-0.1.63
  (crate-source "iana-time-zone" "0.1.63"
                "1n171f5lbc7bryzmp1h30zw86zbvl5480aq02z92lcdwvvjikjdh"))

(define rust-iana-time-zone-haiku-0.1.2
  (crate-source "iana-time-zone-haiku" "0.1.2"
                "17r6jmj31chn7xs9698r122mapq85mfnv98bb4pg6spm0si2f67k"))

(define rust-icu-collections-1.5.0
  (crate-source "icu_collections" "1.5.0"
                "09j5kskirl59mvqc8kabhy7005yyy7dp88jw9f6f3gkf419a8byv"))

(define rust-icu-locid-1.5.0
  (crate-source "icu_locid" "1.5.0"
                "0dznvd1c5b02iilqm044q4hvar0sqibq1z46prqwjzwif61vpb0k"))

(define rust-icu-locid-transform-1.5.0
  (crate-source "icu_locid_transform" "1.5.0"
                "0kmmi1kmj9yph6mdgkc7v3wz6995v7ly3n80vbg0zr78bp1iml81"))

(define rust-icu-locid-transform-data-1.5.1
  (crate-source "icu_locid_transform_data" "1.5.1"
                "07gignya9gzynnyds88bmra4blq9jxzgrcss43vzk2q9h7byc5bm"))

(define rust-icu-normalizer-1.5.0
  (crate-source "icu_normalizer" "1.5.0"
                "0kx8qryp8ma8fw1vijbgbnf7zz9f2j4d14rw36fmjs7cl86kxkhr"))

(define rust-icu-normalizer-data-1.5.1
  (crate-source "icu_normalizer_data" "1.5.1"
                "1dqcm86spcqcs4jnra81yqq3g5bpw6bvf5iz621spj5x52137s65"))

(define rust-icu-properties-1.5.1
  (crate-source "icu_properties" "1.5.1"
                "1xgf584rx10xc1p7zjr78k0n4zn3g23rrg6v2ln31ingcq3h5mlk"))

(define rust-icu-properties-data-1.5.1
  (crate-source "icu_properties_data" "1.5.1"
                "1qm5vf17nyiwb87s3g9x9fsj32gkv4a7q7d2sblawx9vfncqgyw5"))

(define rust-icu-provider-1.5.0
  (crate-source "icu_provider" "1.5.0"
                "1nb8vvgw8dv2inqklvk05fs0qxzkw8xrg2n9vgid6y7gm3423m3f"))

(define rust-icu-provider-macros-1.5.0
  (crate-source "icu_provider_macros" "1.5.0"
                "1mjs0w7fcm2lcqmbakhninzrjwqs485lkps4hz0cv3k36y9rxj0y"))

(define rust-ident-case-1.0.1
  (crate-source "ident_case" "1.0.1"
                "0fac21q6pwns8gh1hz3nbq15j8fi441ncl6w4vlnd1cmc55kiq5r"))

(define rust-idna-1.0.3
  (crate-source "idna" "1.0.3"
                "0zlajvm2k3wy0ay8plr07w22hxkkmrxkffa6ah57ac6nci984vv8"))

(define rust-idna-adapter-1.2.0
  (crate-source "idna_adapter" "1.2.0"
                "0wggnkiivaj5lw0g0384ql2d7zk4ppkn3b1ry4n0ncjpr7qivjns"))

(define rust-ieee754-0.2.6
  (crate-source "ieee754" "0.2.6"
                "1771d2kvw1wga65yrg9m7maky0fzsaq9hvhkv91n6gmxmjfdl1wh"))

(define rust-ignore-0.4.23
  (crate-source "ignore" "0.4.23"
                "0jysggjfmlxbg60vhhiz4pb8jfb7cnq5swdsvxknbs7x18wgv2bd"))

(define rust-im-rc-15.1.0
  (crate-source "im-rc" "15.1.0"
                "1zp5vdjj4b4lg8jnrz0wmdln2cdd9gn24a4psdvwd050bykma6dg"))

(define rust-image-0.24.9
  (crate-source "image" "0.24.9"
                "17gnr6ifnpzvhjf6dwbl9hki8x6bji5mwcqp0048x1jm5yfi742n"
                #:snippet '(for-each delete-file-recursively '("examples" "tests"))))

(define rust-image-0.25.6
  (crate-source "image" "0.25.6"
                "06i522bq4qlwylwnlmcn0sgqg72swwan544aldbhi0drwr66cdfv"))

(define rust-image-webp-0.2.1
  (crate-source "image-webp" "0.2.1"
                "0zwg4gpnp69dpn8pdhgjy14mawwi3md02mp1162al6s64bl02zdp"))

(define rust-imgref-1.11.0
  (crate-source "imgref" "1.11.0"
                "0254wzkakm31fdix6diqng0fkggknibh0b1iv570ap0djwykl9nh"))

(define rust-implib-0.3.5
  (crate-source "implib" "0.3.5"
                "0qjyhapssradwljryq8v31kbyg6s6fqdg50cyyq3by0mc84zg2sr"))

(define rust-indenter-0.3.3
  (crate-source "indenter" "0.3.3"
                "10y6i6y4ls7xsfsc1r3p5j2hhbxhaqnk5zzk8aj52b14v05ba8yf"))

(define rust-indexmap-1.9.3
  (crate-source "indexmap" "1.9.3"
                "16dxmy7yvk51wvnih3a3im6fp5lmx0wx76i03n06wyak6cwhw1xx"))

(define rust-indexmap-2.9.0
  (crate-source "indexmap" "2.9.0"
                "07m15a571yywmvqyb7ms717q9n42b46badbpsmx215jrg7dhv9yf"))

(define rust-indicatif-0.15.0
  (crate-source "indicatif" "0.15.0"
                "1r4n50mclyi4c7b9c9mlma1rhchjamw71r3z8vgqcmp24mhvbakv"))

(define rust-indicatif-0.16.2
  (crate-source "indicatif" "0.16.2"
                "06xyjs0kzqiqkjn60n1miwm2l87sa9p2lmzz0ymq18y72z37s81d"))

(define rust-indicatif-0.17.11
  (crate-source "indicatif" "0.17.11"
                "0db2b2r79r9x8x4lysq1ci9xm13c0xg0sqn3z960yh2bk2430fqq"))

(define rust-indoc-1.0.9
  (crate-source "indoc" "1.0.9"
                "01l3b4ami6sck57yrn8n2z44jifph2m3jiivkws7w2njbvfrk9xz"))

(define rust-indoc-2.0.6
  (crate-source "indoc" "2.0.6"
                "1gbn2pkx5sgbd9lp05d2bkqpbfgazi0z3nvharh5ajah11d29izl"))

(define rust-inflate-0.4.5
  (crate-source "inflate" "0.4.5"
                "1zxjdn8iwa0ssxrnjmywm3r1v284wryvzrf8vkc7nyf5ijbjknqw"
                #:snippet '(delete-file-recursively "tests")))

(define rust-inotify-0.10.2
  (crate-source "inotify" "0.10.2"
                "1k2m6a95827yspax1icmwiz4szr7c01w3dnn2b2bil4hfvcnilgx"))

(define rust-inotify-0.9.6
  (crate-source "inotify" "0.9.6"
                "1zxb04c4qccp8wnr3v04l503qpxzxzzzph61amlqbsslq4z9s1pq"))

(define rust-inotify-sys-0.1.5
  (crate-source "inotify-sys" "0.1.5"
                "1syhjgvkram88my04kv03s0zwa66mdwa5v7ddja3pzwvx2sh4p70"))

(define rust-inout-0.1.4
  (crate-source "inout" "0.1.4"
                "008xfl1jn9rxsq19phnhbimccf4p64880jmnpg59wqi07kk117w7"))

(define rust-input-0.9.1
  (crate-source "input" "0.9.1"
                "1abmv1djhynihipjppgsmw6nbp6pcgzk8rzi4v6wmyci9990kp7v"))

(define rust-input-sys-1.18.0
  (crate-source "input-sys" "1.18.0"
                "1c4y24wf0jixi52js4f7cjspbgi0bzzaqfhn8m91qcq03i6mnkxx"))

(define rust-insta-1.42.2
  (crate-source "insta" "1.42.2"
                "111hrdc3bxwp146kz2ffwdq0qypdjk8a2yzwr8mivlb7maxrl9ah"))

(define rust-instability-0.3.7
  (crate-source "instability" "0.3.7"
                "07f7k0cs1l8cdwxm46vy457bk880hgg6p83nfi777yqwv7bgxy8b"))

(define rust-instant-0.1.13
  (crate-source "instant" "0.1.13"
                "08h27kzvb5jw74mh0ajv0nv9ggwvgqm8ynjsn2sa9jsks4cjh970"))

(define rust-interception-sys-0.1.3
  (crate-source "interception-sys" "0.1.3"
                "1lgwbml7gzq5a5rriy708w68gx6yiw9cdg7xy2c5vsrrck7pbs5b"
                #:snippet '(for-each delete-file (find-files "." "\\.(dll|lib)$"))))

(define rust-interpolate-name-0.2.4
  (crate-source "interpolate_name" "0.2.4"
                "0q7s5mrfkx4p56dl8q9zq71y1ysdj4shh6f28qf9gly35l21jj63"))

(define rust-interprocess-2.2.3
  (crate-source "interprocess" "2.2.3"
                "0bgcvxjgqqm9m1iwfppc4id98d4imkk8x6l5hww9j8i3pl2v8hfr"))

(define rust-inventory-0.3.20
  (crate-source "inventory" "0.3.20"
                "10ybwdx175d7xpvzpz0g2cczn0yvqykkwf75974z55sq5k6xf25b"))

(define rust-io-close-0.3.7
  (crate-source "io-close" "0.3.7"
                "1g4hldfn436rkrx3jlm4az1y5gdmkcixdlhkwy64yx06gx2czbcw"))

(define rust-io-lifetimes-1.0.11
  (crate-source "io-lifetimes" "1.0.11"
                "1hph5lz4wd3drnn6saakwxr497liznpfnv70via6s0v8x6pbkrza"))

(define rust-ipnet-2.11.0
  (crate-source "ipnet" "2.11.0"
                "0c5i9sfi2asai28m8xp48k5gvwkqrg5ffpi767py6mzsrswv17s6"))

(define rust-is-ci-1.2.0
  (crate-source "is_ci" "1.2.0"
                "0ifwvxmrsj4r29agfzr71bjq6y1bihkx38fbzafq5vl0jn1wjmbn"))

(define rust-is-debug-1.1.0
  (crate-source "is_debug" "1.1.0"
                "01yl28nv69wsqiyyhfbgx52yskpjyw5z4xq137c33ja3wb96dqhz"))

(define rust-is-docker-0.2.0
  (crate-source "is-docker" "0.2.0"
                "1cyibrv6817cqcpf391m327ss40xlbik8wxcv5h9pj9byhksx2wj"))

(define rust-is-executable-1.0.4
  (crate-source "is_executable" "1.0.4"
                "1qlafm7f0zq0kzvbd4fhcfci4g9gxp6g3yqxjqsjj1zrssxbb8fl"
                #:snippet '(delete-file-recursively "tests")))

(define rust-is-terminal-0.4.16
  (crate-source "is-terminal" "0.4.16"
                "1acm63whnpwiw1padm9bpqz04sz8msymrmyxc55mvlq8hqqpykg0"))

(define rust-is-terminal-polyfill-1.70.1
  (crate-source "is_terminal_polyfill" "1.70.1"
                "1kwfgglh91z33kl0w5i338mfpa3zs0hidq5j4ny4rmjwrikchhvr"))

(define rust-is-wsl-0.4.0
  (crate-source "is-wsl" "0.4.0"
                "19bs5pq221d4bknnwiqqkqrnsx2in0fsk8fylxm1747iim4hjdhp"))

(define rust-itertools-0.10.5
  (crate-source "itertools" "0.10.5"
                "0ww45h7nxx5kj6z2y6chlskxd1igvs4j507anr6dzg99x1h25zdh"))

(define rust-itertools-0.11.0
  (crate-source "itertools" "0.11.0"
                "0mzyqcc59azx9g5cg6fs8k529gvh4463smmka6jvzs3cd2jp7hdi"))

(define rust-itertools-0.12.1
  (crate-source "itertools" "0.12.1"
                "0s95jbb3ndj1lvfxyq5wanc0fm0r6hg6q4ngb92qlfdxvci10ads"))

(define rust-itertools-0.13.0
  (crate-source "itertools" "0.13.0"
                "11hiy3qzl643zcigknclh446qb9zlg4dpdzfkjaa9q9fqpgyfgj1"))

(define rust-itertools-0.14.0
  (crate-source "itertools" "0.14.0"
                "118j6l1vs2mx65dqhwyssbrxpawa90886m3mzafdvyip41w2q69b"))

(define rust-itertools-0.9.0
  (crate-source "itertools" "0.9.0"
                "0jyml7ygr7kijkcjdl3fk5f34y5h5jsavclim7l13zjiavw1hkr8"))

(define rust-itertools-num-0.1.3
  (crate-source "itertools-num" "0.1.3"
                "1rr7ig9nkpampcas23s91x7yac6qdnwssq3nap522xbgkqps4wm8"))

(define rust-itoa-0.4.8
  (crate-source "itoa" "0.4.8"
                "1m1dairwyx8kfxi7ab3b5jc71z1vigh9w4shnhiajji9avzr26dp"))

(define rust-itoa-1.0.15
  (crate-source "itoa" "1.0.15"
                "0b4fj9kz54dr3wam0vprjwgygvycyw8r0qwg7vp19ly8b2w16psa"))

(define rust-ivf-0.1.3
  (crate-source "ivf" "0.1.3"
                "1jjy911flpfpflnxw5fqsx6a3ghaq5wi2q18nx9cawpf81qnabsm"))

(define rust-jiff-0.2.5
  (crate-source "jiff" "0.2.5"
                "0q3jpq3scznmviiajldyf5xby38zgyvkxbrmgb9hf78r6416f0n1"))

(define rust-jiff-static-0.2.5
  (crate-source "jiff-static" "0.2.5"
                "0k1v30mhbgh4zj2r9d7lfqlh5b20b5573cx0a4gip7rlkldf7pac"))

(define rust-jiff-tzdb-0.1.4
  (crate-source "jiff-tzdb" "0.1.4"
                "09350bna4vxdn2fv7gd08ay41llkflmfyvpx5d6l088axc2kfa61"
                #:snippet '(delete-file "concatenated-zoneinfo.dat")))

(define rust-jiff-tzdb-platform-0.1.3
  (crate-source "jiff-tzdb-platform" "0.1.3"
                "1s1ja692wyhbv7f60mc0x90h7kn1pv65xkqi2y4imarbmilmlnl7"))

(define rust-jni-0.21.1
  (crate-source "jni" "0.21.1"
                "15wczfkr2r45slsljby12ymf2hij8wi5b104ghck9byjnwmsm1qs"))

(define rust-jni-sys-0.3.0
  (crate-source "jni-sys" "0.3.0"
                "0c01zb9ygvwg9wdx2fii2d39myzprnpqqhy7yizxvjqp5p04pbwf"))

(define rust-jobserver-0.1.33
  (crate-source "jobserver" "0.1.33"
                "12jkn3cxvfs7jsb6knmh9y2b41lwmrk3vdqywkmssx61jzq65wiq"))

(define rust-jpeg-decoder-0.3.1
  (crate-source "jpeg-decoder" "0.3.1"
                "1c1k53svpdyfhibkmm0ir5w0v3qmcmca8xr8vnnmizwf6pdagm7m"
                #:snippet '(delete-file-recursively "benches")))

(define rust-js-sys-0.3.77
  (crate-source "js-sys" "0.3.77"
                "13x2qcky5l22z4xgivi59xhjjx4kxir1zg7gcj0f1ijzd4yg7yhw"))

(define rust-junction-1.2.0
  (crate-source "junction" "1.2.0"
                "05mxqwzxgb2aqgbq9b5lgbqq0r0nds6yx68zzhyxlhx26zbxzfvj"))

(define rust-kanata-interception-0.3.0
  (crate-source "kanata-interception" "0.3.0"
                "01mn1dskhm124x0nxfcw5cyb07h0i256x9bfj23aq6adjsdpprg2"))

(define rust-kanata-keyberon-0.180.2
  (crate-source "kanata-keyberon" "0.180.2"
                "1b0axjmzq79pi5xbj82c38mvvwwylar42jwiwzz3n8v0bjln6czj"
                #:snippet '(delete-file-recursively "images")))

(define rust-kanata-keyberon-macros-0.2.0
  (crate-source "kanata-keyberon-macros" "0.2.0"
                "0lj7ldiazmszh0k01h7mjzhjg59bdakvx2pnpc9mq2ir0czzixkk"))

(define rust-kanata-parser-0.180.2
  (crate-source "kanata-parser" "0.180.2"
                "0dfbjc2gy5jc3wqy0fnn6c9wpqxcwwwkkv6lf4lgnp6sfkvqsi18"))

(define rust-kanata-tcp-protocol-0.180.2
  (crate-source "kanata-tcp-protocol" "0.180.2"
                "1x74ncvffz3cani6l84jl8rqr26d445hz3h88h75k7aa59jc8fax"))

(define rust-karabiner-driverkit-0.1.5
  (crate-source "karabiner-driverkit" "0.1.5"
                "0pqnh9n3a8wxqzdj7d30f99g322da8zpnixsq5gfs9n1klccj380"
                #:snippet '(delete-file-recursively "c_src")))

(define rust-keyframe-1.1.1
  (crate-source "keyframe" "1.1.1"
                "1afr5ffns3k79xaqnw6rw3qn8sngwly6gxfnjn8d060mk3vqnw30"))

(define rust-keyring-2.3.3
  (crate-source "keyring" "2.3.3"
                "184mshdrgghlvmlz0n7w1167yx0sa3zb82n31jk4lwcx07q8fcrn"))

(define rust-khronos-api-3.1.0
  (crate-source "khronos_api" "3.1.0"
                "1p0xj5mlbagqyvvnv8wmv3cr7l9y1m153888pxqwg3vk3mg5inz2"))

(define rust-knuffel-3.2.0
  (crate-source "knuffel" "3.2.0"
                "04vl2xmdn280rcigv96v06a00v7gbxqggr0w9cqi2407qvfydgh4"
                #:snippet '(delete-file-recursively "images")))

(define rust-knuffel-derive-3.2.0
  (crate-source "knuffel-derive" "3.2.0"
                "0g98909l5wb1d1hcz61q53kvsmjadry2w3l47lg9dywwqib7z5wi"))

(define rust-kqueue-1.0.8
  (crate-source "kqueue" "1.0.8"
                "033x2knkbv8d3jy6i9r32jcgsq6zm3g97zh5la43amkv3g5g2ivl"))

(define rust-kqueue-sys-1.0.4
  (crate-source "kqueue-sys" "1.0.4"
                "12w3wi90y4kwis4k9g6fp0kqjdmc6l00j16g8mgbhac7vbzjb5pd"))

(define rust-kstring-2.0.2
  (crate-source "kstring" "2.0.2"
                "1lfvqlqkg2x23nglznb7ah6fk3vv3y5i759h5l2151ami98gk2sm"))

(define rust-lab-0.11.0
  (crate-source "lab" "0.11.0"
                "13ymsn5cwl5i9pmp5mfmbap7q688dcp9a17q82crkvb784yifdmz"))

(define rust-language-tags-0.3.2
  (crate-source "language-tags" "0.3.2"
                "124k6w9nx33q4xs8rpa9f7klshrsa0x4f7qngdwq890lpdj5jd6l"))

(define rust-lazy-static-1.5.0
  (crate-source "lazy_static" "1.5.0"
                "1zk6dqqni0193xg6iijh7i3i44sryglwgvx20spdvwk3r6sbrlmv"))

(define rust-lazycell-1.3.0
  (crate-source "lazycell" "1.3.0"
                "0m8gw7dn30i0zjjpjdyf6pc16c34nl71lpv461mix50x3p70h3c3"))

(define rust-lddtree-0.3.7
  (crate-source "lddtree" "0.3.7"
                "13mh4y1bwlqpa8jpsgj3zb0mxi4syzdqh8x13xwapdnp9g4rlxz0"
                #:snippet '(delete-file-recursively "tests")))

(define rust-leb128-0.2.5
  (crate-source "leb128" "0.2.5"
                "0rxxjdn76sjbrb08s4bi7m4x47zg68f71jzgx8ww7j0cnivjckl8"))

(define rust-lebe-0.5.2
  (crate-source "lebe" "0.5.2"
                "1j2l6chx19qpa5gqcw434j83gyskq3g2cnffrbl3842ymlmpq203"))

(define rust-libadwaita-0.5.3
  (crate-source "libadwaita" "0.5.3"
                "174pzn9dwsk8ikvrhx13vkh0zrpvb3rhg9yd2q5d2zjh0q6fgrrg"))

(define rust-libadwaita-0.7.2
  (crate-source "libadwaita" "0.7.2"
                "14c1qy6mq5l9wlwsr2x9ijbvis283msfglxgp9kvzahnkk93a0ah"))

(define rust-libadwaita-sys-0.5.3
  (crate-source "libadwaita-sys" "0.5.3"
                "16n6xsy6jhbj0jbpz8yvql6c9b89a99v9vhdz5s37mg1inisl42y"))

(define rust-libadwaita-sys-0.7.2
  (crate-source "libadwaita-sys" "0.7.2"
                "1nqjr514hhdc4aldlsc4y3vkpnkq9q73g2jl7ypqnmf2b209i036"))

(define rust-libc-0.2.171
  (crate-source "libc" "0.2.171"
                "1mipla3dy3l59pfa9xy4iw2vdgn8n30dzf4vdnasjflxdqhkg6f1"))

(define rust-libdav1d-sys-0.6.0
  (crate-source "libdav1d-sys" "0.6.0"
                "0wh5jgdm33ld6djxsc7cmwd1ifqys145zlbsf8516n625lscrj8j"
                #:snippet '(delete-file-recursively "vendor")))

(define rust-libdbus-sys-0.2.5
  (crate-source "libdbus-sys" "0.2.5"
                "0wjw93q6ckrn8qdrxzdi02f0ma9g7nnlpgkrkcll1mjhnw95a206"
                #:snippet '(delete-file-recursively "vendor")))

(define rust-libdisplay-info-0.2.2
  (crate-source "libdisplay-info" "0.2.2"
                "0avs90mwzbfkwc09xlvvdy0szrbi670y61c1w0l75hqd7blwy422"))

(define rust-libdisplay-info-derive-0.1.0
  (crate-source "libdisplay-info-derive" "0.1.0"
                "1vpwss66rmhdd0f85c3nwjshddmiarf4iya5v13aacmp6q8d677a"))

(define rust-libdisplay-info-sys-0.2.2
  (crate-source "libdisplay-info-sys" "0.2.2"
                "0v34vjczpj8hzxnx1nj5cxwf326m91gn7bi3l3zkfg72xij94kvz"))

(define rust-libfuzzer-sys-0.4.9
  (crate-source "libfuzzer-sys" "0.4.9"
                "0xfwg8shqvysl2bma2lyfcswbbdljajphflp795diwhc80nzay6g"
                #:snippet '(delete-file-recursively "libfuzzer")))

(define rust-libgit2-sys-0.17.0+1.8.1
  (crate-source "libgit2-sys" "0.17.0+1.8.1"
                "093jxfl2i9vxdlgf7vk9d040sjwy0nq4fid640y7qix6m0k26iqh"
                #:snippet '(delete-file-recursively "libgit2")))

(define rust-libgit2-sys-0.18.1+1.9.0
  (crate-source "libgit2-sys" "0.18.1+1.9.0"
                "03i98nb84aa99bn7sxja11pllq6fghsaw4d3qwjxikgzhh7v5p71"
                #:snippet '(delete-file-recursively "libgit2")))

(define rust-libloading-0.7.4
  (crate-source "libloading" "0.7.4"
                "17wbccnjvhjd9ibh019xcd8kjvqws8lqgq86lqkpbgig7gyq0wxn"
                #:snippet '(delete-file-recursively "tests")))

(define rust-libloading-0.8.6
  (crate-source "libloading" "0.8.6"
                "0d2ccr88f8kv3x7va2ccjxalcjnhrci4j2kwxp7lfmbkpjs4wbzw"
                #:snippet '(delete-file-recursively "tests")))

(define rust-libm-0.1.4
  (crate-source "libm" "0.1.4"
                "16pc0gx4gkg0q2s1ssq8268brn14j8344623vwhadmivc4lsmivz"))

(define rust-libm-0.2.11
  (crate-source "libm" "0.2.11"
                "1yjgk18rk71rjbqcw9l1zaqna89p9s603k7n327nqs8dn88vwmc3"))

(define rust-libmimalloc-sys-0.1.41
  (crate-source "libmimalloc-sys" "0.1.41"
                "0yykxwnppvcwk8yfvniij65a945lqw7ykiakqyylvhaa7b5dl83b"
                #:snippet
                '(begin
                   (delete-file-recursively "c_src")
                   (delete-file "build.rs")
                   (with-output-to-file "build.rs"
                     (lambda _
                       (format #t "fn main() {~@
                        println!(\"cargo:rustc-link-lib=mimalloc\");~@
                        }~%"))))))

(define rust-libnghttp2-sys-0.1.11+1.64.0
  (crate-source "libnghttp2-sys" "0.1.11+1.64.0"
                "1i0klzhn5s5y2v0am948qrk2wj7sfzakknhrf7xcyrviibj28v0v"
                #:snippet
                '(begin
                   (delete-file-recursively "nghttp2")
                   (delete-file "build.rs")
                   (with-output-to-file "build.rs"
                     (lambda _
                       (format #t "fn main() {~@
                        println!(\"cargo:rustc-link-lib=nghttp2\");~@
                        }~%"))))))

(define rust-libproc-0.14.10
  (crate-source "libproc" "0.14.10"
                "1yzbhywjzvhi4353pni6k8jbpildd1qp66d1banvrbg5dfshk2p7"))

(define rust-libradicl-0.5.1
  (crate-source "libradicl" "0.5.1"
                "0hgh395sys55z3xayg7pwibz0ymf83zj3bm111fvrq1jjh77zij4"))

(define rust-libredox-0.1.3
  (crate-source "libredox" "0.1.3"
                "139602gzgs0k91zb7dvgj1qh4ynb8g1lbxsswdim18hcb6ykgzy0"))

(define rust-libseat-0.2.3
  (crate-source "libseat" "0.2.3"
                "0350b89h2xk5rdqqla162a2pak7yzbpfckqwg68cd42ppmdj8fn2"))

(define rust-libseat-sys-0.1.9
  (crate-source "libseat-sys" "0.1.9"
                "0997n2s413ggzi4amy0jbnfl8jvgpjnkxzycjs56ks2p0pjj2ihk"))

(define rust-libspa-0.8.0.fd3d8f7 rust-pipewire-for-niri)

(define rust-libspa-sys-0.8.0.fd3d8f7 rust-pipewire-for-niri)

(define rust-libsqlite3-sys-0.28.0
  (crate-source "libsqlite3-sys" "0.28.0"
                "0gzwfw0n2wqgaihcgj65wzd3lclfxyy62gixq8sv6z04fi15h40c"
                #:snippet
                '(for-each delete-file
                           (append
                            (find-files "sqlcipher" "\\.(c|h)$")
                            (find-files "sqlite3" "\\.(c|h)$")))))

(define rust-libsqlite3-sys-0.30.1
  (crate-source "libsqlite3-sys" "0.30.1"
                "0jcikvgbj84xc7ikdmpc8m4y5lyqgrb9aqblphwk67kv95xgp69f"
                #:snippet
                '(for-each delete-file
                           (append
                            (find-files "sqlcipher" "\\.(c|h)$")
                            (find-files "sqlite3" "\\.(c|h)$")))))

(define rust-libsqlite3-sys-0.32.0
  (crate-source "libsqlite3-sys" "0.32.0"
                "1rvgxy0hy7p4jib8hw1l5bcg2806v190rwlndrndf2q6nh5jgf7v"
                #:snippet
                '(for-each delete-file
                           (append
                            (find-files "sqlcipher" "\\.(c|h)$")
                            (find-files "sqlite3" "\\.(c|h)$")))))

(define rust-libssh2-sys-0.3.1
  (crate-source "libssh2-sys" "0.3.1"
                "1f8i31h3666rl6sq7v64ajdq03hmylkh6c1vaf9828aaml2ly3i2"
                #:snippet '(delete-file-recursively "libssh2")))

(define rust-libudev-sys-0.1.4
  (crate-source "libudev-sys" "0.1.4"
                "09236fdzlx9l0dlrsc6xx21v5x8flpfm3d5rjq9jr5ivlas6k11w"))

(define rust-libz-rs-sys-0.5.0
  (crate-source "libz-rs-sys" "0.5.0"
                "0nlc7cdcrh8mxqb08yw5i7ghgpcs1ixq4kk4sx19dzk0sydwm2b4"))

(define rust-libz-sys-1.1.22
  (crate-source "libz-sys" "1.1.22"
                "07b5wxh0ska996kc0g2hanjhmb4di7ksm6ndljhr4pi0vykyfw4b"
                #:snippet '(for-each delete-file-recursively '("src/zlib" "src/zlib-ng"))))

(define rust-linear-map-1.2.0
  (crate-source "linear-map" "1.2.0"
                "1vh3sczl4xb5asdlpafdf3y4g9bp63fgs8y2a2sjgmcsn7v21bmz"))

(define rust-linecount-0.1.0
  (crate-source "linecount" "0.1.0"
                "1n2733qcmvxl3fra3924yj8sf21vr6f6g6dg6wnhbkxr8fia9mfm"))

(define rust-linked-hash-map-0.5.6
  (crate-source "linked-hash-map" "0.5.6"
                "03vpgw7x507g524nx5i1jf5dl8k3kv0fzg8v3ip6qqwbpkqww5q7"))

(define rust-linux-keyutils-0.2.4
  (crate-source "linux-keyutils" "0.2.4"
                "13nipvk2mzk76y7yfsqwnwsqk21x6xy8fkmqz5is99fqbzn4j7kn"))

(define rust-linux-raw-sys-0.3.8
  (crate-source "linux-raw-sys" "0.3.8"
                "068mbigb3frrxvbi5g61lx25kksy98f2qgkvc4xg8zxznwp98lzg"))

(define rust-linux-raw-sys-0.4.15
  (crate-source "linux-raw-sys" "0.4.15"
                "1aq7r2g7786hyxhv40spzf2nhag5xbw2axxc1k8z5k1dsgdm4v6j"))

(define rust-linux-raw-sys-0.6.5
  (crate-source "linux-raw-sys" "0.6.5"
                "1mv3c1zz51ydcj768zavm8g06gz5jb1p7yigmmif7hz5whdmnf1a"))

(define rust-linux-raw-sys-0.9.3
  (crate-source "linux-raw-sys" "0.9.3"
                "04zl7j4k1kgbn7rrl3i7yszaglgxp0c8dbwx8f1cabnjjwhb2zgy"))

(define rust-linux-raw-sys-0.9.4
  (crate-source "linux-raw-sys" "0.9.4"
                "04kyjdrq79lz9ibrf7czk6cv9d3jl597pb9738vzbsbzy1j5i56d"))

(define rust-litemap-0.7.5
  (crate-source "litemap" "0.7.5"
                "0mi8ykav0s974ps79p438x04snh0cdb7lc864b42jws5375i9yr3"))

(define rust-locale-config-0.3.0
  (crate-source "locale_config" "0.3.0"
                "0d399alr1i7h7yji4vydbdbzd8hp0xaykr7h4rn3yj7l2rdw7lh8"))

(define rust-lock-api-0.4.12
  (crate-source "lock_api" "0.4.12"
                "05qvxa6g27yyva25a5ghsg85apdxkvr77yhkyhapj6r8vnf8pbq7"))

(define rust-lockfree-object-pool-0.1.6
  (crate-source "lockfree-object-pool" "0.1.6"
                "0bjm2g1g1avab86r02jb65iyd7hdi35khn1y81z4nba0511fyx4k"))

(define rust-log-0.4.27
  (crate-source "log" "0.4.27"
                "150x589dqil307rv0rwj0jsgz5bjbwvl83gyl61jf873a7rjvp0k"))

(define rust-log-reroute-0.1.8
  (crate-source "log-reroute" "0.1.8"
                "00mw91qd2ibaawl7x1pxc1kryki0ixyirnlx64qx78d9g6k3n6kl"))

(define rust-loom-0.7.2
  (crate-source "loom" "0.7.2"
                "1jpszf9qxv8ydpsm2h9vcyvxvyxcfkhmmfbylzd4gfbc0k40v7j1"))

(define rust-loop9-0.1.5
  (crate-source "loop9" "0.1.5"
                "0qphc1c0cbbx43pwm6isnwzwbg6nsxjh7jah04n1sg5h4p0qgbhg"))

(define rust-lopdf-0.32.0
  (crate-source "lopdf" "0.32.0"
                "0aw7diz39z3mk22k0mp7jk7qiaaagfvggzly1baqg2jf4vpf8xg7"
                #:snippet '(delete-file-recursively "assets")))

(define rust-lru-0.12.5
  (crate-source "lru" "0.12.5"
                "0f1a7cgqxbyhrmgaqqa11m3azwhcc36w0v5r4izgbhadl3sg8k13"))

(define rust-lscolors-0.13.0
  (crate-source "lscolors" "0.13.0"
                "1wnxs5d004fx71apvh9124xqky0qjjmpibag24km7bvvss2xrpn2"))

(define rust-lscolors-0.17.0
  (crate-source "lscolors" "0.17.0"
                "08z5jslgigvnpc1gj2i8r9pi8yn4m0pf8dzf3rk9grdidbzlyc2k"))

(define rust-lsp-server-0.7.8
  (crate-source "lsp-server" "0.7.8"
                "1yanavncgsx0i0rj65q12ddfcwpvzx5x8wgiq4g9fzz1fgfc8qll"))

(define rust-lsp-textdocument-0.4.2
  (crate-source "lsp-textdocument" "0.4.2"
                "0b7dxa7x3v5z58nj5rsscv1kpifasbdp5x0v0wycsgafbxclsmid"))

(define rust-lsp-types-0.97.0
  (crate-source "lsp-types" "0.97.0"
                "0wb0yr2cdhlndjkcfyabr17ib0nvqa4v3zl5qm3aq13wl583adak"))

(define rust-lv2-0.6.0
  (crate-source "lv2" "0.6.0"
                "1xh4hjfh2w5rhzbk0g9845k25f6fxrv7xqpkr09p0x57b200qc41"))

(define rust-lv2-atom-2.0.0
  (crate-source "lv2-atom" "2.0.0"
                "0wd9rgsn8sag8wyhjccmnn82gx4w1yyiav52nyvk579l21xlw6wm"))

(define rust-lv2-core-3.0.0
  (crate-source "lv2-core" "3.0.0"
                "1pj9l15zwqwj2h83f3xfpwxsj70vvhkw52gyzkljafvrbx1h00fm"))

(define rust-lv2-core-derive-2.1.1
  (crate-source "lv2-core-derive" "2.1.1"
                "12w3l41jzargrcywz13hbmaazfw4ix2sljl3601h6jfbdrw8zybv"))

(define rust-lv2-midi-1.2.0
  (crate-source "lv2-midi" "1.2.0"
                "0x0glbrfri1glgcrmvc6i1jfv6azhpqvp4ibk5cihsq3s2yfc8xd"))

(define rust-lv2-sys-2.0.0
  (crate-source "lv2-sys" "2.0.0"
                "0c4f59mrjyy0z0wf033wp648df0sc6zirrcd6kndqj9nvvkzkl4x"))

(define rust-lv2-units-0.1.3
  (crate-source "lv2-units" "0.1.3"
                "0fdamp3hxdr36hqi1j6y01rz1x17if1ibzr7rr4nrabidw74gf82"))

(define rust-lv2-urid-2.1.0
  (crate-source "lv2-urid" "2.1.0"
                "0s2fcb0nyn54ml6azkbhnnxghy898x1q5vs5qgdznrhy9m20624c"))

(define rust-lz4-flex-0.10.0
  (crate-source "lz4_flex" "0.10.0"
                "10sgbj93sagbl0ngzqvnlkldzbfz5vnzr7fry8sgssy299cp534b"))

(define rust-lzma-sys-0.1.20
  (crate-source "lzma-sys" "0.1.20"
                "09sxp20waxyglgn3cjz8qjkspb3ryz2fwx4rigkwvrk46ymh9njz"
                #:snippet
                '(begin
                   (delete-file-recursively "xz-5.2")
                   (substitute* "build.rs"
                     (("(want_static = ).*" _ prefix)
                      (string-append prefix "false;\n"))))))

(define rust-mac-0.1.1
  (crate-source "mac" "0.1.1"
                "194vc7vrshqff72rl56f9xgb0cazyl4jda7qsv31m5l6xx7hq7n4"))

(define rust-mach2-0.4.2
  (crate-source "mach2" "0.4.2"
                "02gpyq89rcrqdbz4hgp5bpjas21dllxfc70jgw8vj0iaxg6mbf8r"))

(define rust-mailparse-0.15.0
  (crate-source "mailparse" "0.15.0"
                "0zkwbrzgr7pp1wyywjgvlxayr1p3nnkn2yxgi97746j1h1ckv81x"))

(define rust-mailparse-0.16.1
  (crate-source "mailparse" "0.16.1"
                "0bzdd3yliadzjnirb4g0jwz7j3k2yds02fzb2ib1m0ybvnbrm0b0"
                #:snippet '(delete-file-recursively "tests")))

(define rust-malloc-buf-0.0.6
  (crate-source "malloc_buf" "0.0.6"
                "1jqr77j89pwszv51fmnknzvd53i1nkmcr8rjrvcxhm4dx1zr1fv2"))

(define rust-markup5ever-0.11.0
  (crate-source "markup5ever" "0.11.0"
                "05mhzsp6lfxla1fgd0ac283b405s6kyj27wj5r6d7wq42jxjj9ks"))

(define rust-matchers-0.1.0
  (crate-source "matchers" "0.1.0"
                "0n2mbk7lg2vf962c8xwzdq96yrc9i0p8dbmm4wa1nnkcp1dhfqw2"))

(define rust-matches-0.1.10
  (crate-source "matches" "0.1.10"
                "1994402fq4viys7pjhzisj4wcw894l53g798kkm2y74laxk0jci5"))

(define rust-matrixmultiply-0.2.4
  (crate-source "matrixmultiply" "0.2.4"
                "1hc4vp19x823xgkm374wsxnzmqbjhmyaj5nr0lhm9k9i02x0cs4i"))

(define rust-matrixmultiply-0.3.9
  (crate-source "matrixmultiply" "0.3.9"
                "06msav241ybxvsqfwm4hfmb1pbws71v0inhmyk0i0vg9wc8vk04k"))

(define rust-maybe-async-0.2.10
  (crate-source "maybe-async" "0.2.10"
                "04fvg2ywb2p9dzf7i35xqfibxc05k1pirv36jswxcqg3qw82ryaw"))

(define rust-maybe-rayon-0.1.1
  (crate-source "maybe-rayon" "0.1.1"
                "06cmvhj4n36459g327ng5dnj8d58qs472pv5ahlhm7ynxl6g78cf"))

(define rust-md-5-0.10.6
  (crate-source "md-5" "0.10.6"
                "1kvq5rnpm4fzwmyv5nmnxygdhhb2369888a06gdc9pxyrzh7x7nq"
                #:snippet '(delete-file-recursively "tests")))

(define rust-md5-0.7.0
  (crate-source "md5" "0.7.0"
                "0wcps37hrhz59fkhf8di1ppdnqld6l1w5sdy7jp7p51z0i4c8329"))

(define rust-memchr-2.7.4
  (crate-source "memchr" "2.7.4"
                "18z32bhxrax0fnjikv475z7ii718hq457qwmaryixfxsl2qrmjkq"
                #:snippet '(delete-file-recursively "src/tests")))

(define rust-memmap2-0.3.1
  (crate-source "memmap2" "0.3.1"
                "0mz3fdcq443m3a1phrhp2yvd1h9vrvbhinzmi23ik031zzmw5dh0"))

(define rust-memmap2-0.8.0
  (crate-source "memmap2" "0.8.0"
                "1vf3djv9s917fbvw5vclllpl22g12iph6cz11gn57ndhxwya19a3"))

(define rust-memmap2-0.9.5
  (crate-source "memmap2" "0.9.5"
                "0krpvvkpg4i3l05cv3q2xk24a1vj5c86gbrli2wzhj1qkpnpwgzx"))

(define rust-memoffset-0.6.5
  (crate-source "memoffset" "0.6.5"
                "1kkrzll58a3ayn5zdyy9i1f1v3mx0xgl29x0chq614zazba638ss"))

(define rust-memoffset-0.7.1
  (crate-source "memoffset" "0.7.1"
                "1x2zv8hv9c9bvgmhsjvr9bymqwyxvgbca12cm8xkhpyy5k1r7s2x"))

(define rust-memoffset-0.8.0
  (crate-source "memoffset" "0.8.0"
                "1qcdic88dhgw76pafgndpz04pig8il4advq978mxdxdwrydp276n"))

(define rust-memoffset-0.9.1
  (crate-source "memoffset" "0.9.1"
                "12i17wh9a9plx869g7j4whf62xw68k5zd4k0k5nh6ys5mszid028"))

(define rust-miette-5.10.0
  (crate-source "miette" "5.10.0"
                "0vl5qvl3bgha6nnkdl7kiha6v4ypd6d51wyc4q1bvdpamr75ifsr"))

(define rust-miette-7.5.0
  (crate-source "miette" "7.5.0"
                "114lv0nx46lxc5pncz6iyrzcfhn5g9a5janzc8cgsdvvz1jm358s"))

(define rust-miette-derive-5.10.0
  (crate-source "miette-derive" "5.10.0"
                "0p33msrngkxlp5ajm8nijamii9vcwwpy8gfh4m53qnmrc0avrrs9"))

(define rust-miette-derive-7.5.0
  (crate-source "miette-derive" "7.5.0"
                "0irig3c79184h54zasn06yiz25znqrpvx8r72byr5gj9md2byidz"))

(define rust-migrations-internals-2.2.0
  (crate-source "migrations_internals" "2.2.0"
                "1zw0lf2lw3wlmyb0kv68cnr3ya2n80svpavf0jcqfbz8a6c060gx"))

(define rust-migrations-macros-2.2.0
  (crate-source "migrations_macros" "2.2.0"
                "1z9p2ag0fnnh0m7z8qfncwyjc0pgschca7vzlixb6v0pfb663cgz"))

(define rust-mimalloc-0.1.45
  (crate-source "mimalloc" "0.1.45"
                "0pwjnz8s4qmbz4qxncxqqx3i64zczwvd758ir9hh1r9z1641zjq3"))

(define rust-mime-0.3.17
  (crate-source "mime" "0.3.17"
                "16hkibgvb9klh0w0jk5crr5xv90l3wlf77ggymzjmvl1818vnxv8"))

(define rust-mime-guess-2.0.5
  (crate-source "mime_guess" "2.0.5"
                "03jmg3yx6j39mg0kayf7w4a886dl3j15y8zs119zw01ccy74zi7p"))

(define rust-minicov-0.3.7
  (crate-source "minicov" "0.3.7"
                "0jsvi62lklfyvdmsiizipkqcfpsc7h4c4illgxlf28iwrkqyjzzj"))

(define rust-minijinja-2.9.0
  (crate-source "minijinja" "2.9.0"
                "0m77dg3fp7xdbfpjl8rjpqgrbam40z6pfyrhk9vj44d9zinjlr4q"))

(define rust-minimal-lexical-0.2.1
  (crate-source "minimal-lexical" "0.2.1"
                "16ppc5g84aijpri4jzv14rvcnslvlpphbszc7zzp6vfkddf4qdb8"))

(define rust-miniz-oxide-0.6.2
  (crate-source "miniz_oxide" "0.6.2"
                "1yp8z6yll5ypz1ldmgnv7zi0r78kbvmqmn2mii77jzmk5069axdj"))

(define rust-miniz-oxide-0.7.4
  (crate-source "miniz_oxide" "0.7.4"
                "024wv14aa75cvik7005s5y2nfc8zfidddbd7g55g7sjgnzfl18mq"))

(define rust-miniz-oxide-0.8.7
  (crate-source "miniz_oxide" "0.8.7"
                "0c4lj692adnzw0h9j8l24d7imds3icpgdkk3b03zlhxf90zcww7z"))

(define rust-miniz-oxide-0.8.8
  (crate-source "miniz_oxide" "0.8.8"
                "0al9iy33flfgxawj789w2c8xxwg1n2r5vv6m6p5hl2fvd2vlgriv"))

(define rust-mio-0.8.11
  (crate-source "mio" "0.8.11"
                "034byyl0ardml5yliy1hmvx8arkmn9rv479pid794sm07ia519m4"))

(define rust-mio-1.0.3
  (crate-source "mio" "1.0.3"
                "1gah0h4ia3avxbwym0b6bi6lr6rpysmj9zvw6zis5yq0z0xq91i8"))

(define rust-miow-0.6.0
  (crate-source "miow" "0.6.0"
                "0i307jyhxnhgzj148cdb9zq59rhlhr1b65g142g9z9r01d1pd7rm"))

(define rust-muldiv-0.2.1
  (crate-source "muldiv" "0.2.1"
                "014jlry2l2ph56mp8knw65637hh49q7fmrraim2bx9vz0a638684"))

(define rust-muldiv-1.0.1
  (crate-source "muldiv" "1.0.1"
                "1c6ljsp41n8ijsx7zicwfm135drgyhcms12668ivvsbm1r98frwm"))

(define rust-multimap-0.8.3
  (crate-source "multimap" "0.8.3"
                "0sicyz4n500vdhgcxn4g8jz97cp1ijir1rnbgph3pmx9ckz4dkp5"))

(define rust-multimap-0.9.1
  (crate-source "multimap" "0.9.1"
                "04fmnxhpawndrr5x730s3gb4h77ldbrnlww86a8vsb9mkf5x79g1"))

(define rust-multipart-0.18.0
  (crate-source "multipart" "0.18.0"
                "10libwfbazqcyxcpgpcdf1a66jnzghwlmxlxnffg4rrqhqrwdph0"))

(define rust-multipart-rs-0.1.13
  (crate-source "multipart-rs" "0.1.13"
                "1wj5jgbrq7svcqdahxi17j8vws8nsz6a5y9f6ir51ajjgq7f1jk4"))

(define rust-munge-0.4.3
  (crate-source "munge" "0.4.3"
                "1myrsmqpwk3vlbvbs6jjni1iqsybri8dz7r7djj6vw4cr41142d0"))

(define rust-munge-macro-0.4.3
  (crate-source "munge_macro" "0.4.3"
                "0n94vxw2fygwqsqh4kvxwa8xsh09hli1mikhz6r215s7j77rjivk"))

(define rust-nalgebra-0.19.0
  (crate-source "nalgebra" "0.19.0"
                "0i87k57nav221lnr9z7ljlwxh8073qsx33bajdm146y00q805fqa"
                #:snippet '(delete-file-recursively "tests")))

(define rust-nalgebra-0.27.1
  (crate-source "nalgebra" "0.27.1"
                "0811vksy1ls1m41gqkh1i5cy3h3g3b615kwxd8gjwkrg03jgybs6"
                #:snippet '(delete-file-recursively "tests")))

(define rust-nalgebra-0.29.0
  (crate-source "nalgebra" "0.29.0"
                "1zri11vkrbk3ayvy8xqcdvvyjvfcbl5a18x8z82racnn11zfn1nm"))

(define rust-nalgebra-0.32.6
  (crate-source "nalgebra" "0.32.6"
                "1r033ciacblmkif5njlhprfp0k59spjv54cqsyggb1is0bg1fp3v"))

(define rust-nalgebra-macros-0.1.0
  (crate-source "nalgebra-macros" "0.1.0"
                "063jvvvlwmzzxfr4wyiil2cn1yqj3arvghwsr2nk4ilv2jwc1z01"))

(define rust-nalgebra-macros-0.2.2
  (crate-source "nalgebra-macros" "0.2.2"
                "1z6v9phhr1hwzyyblf792128lxfv1hy1sxl4cvikihcgmxr56ji5"))

(define rust-nanoid-0.4.0
  (crate-source "nanoid" "0.4.0"
                "1n5l5xig5zcinh41bb4p0rzam8gxic02qpngnylb3d8pq3g01yiz"))

(define rust-nasm-rs-0.2.5
  (crate-source "nasm-rs" "0.2.5"
                "0lfs2xfbpl1j7zq6qfg2wmi4djbl36qsygjb2spisjsz0v89hkgy"))

(define rust-native-tls-0.2.14
  (crate-source "native-tls" "0.2.14"
                "03hga800x8bzkp8h7frnm7yp545dwwawgmaq673vx7byk1139pl7"))

(define rust-native-windows-gui-1.0.13
  (crate-source "native-windows-gui" "1.0.13"
                "0m44lbslzvs04i4rgcphld23qlwf9zzlzmspgimyp3gnd6k06w2g"
                #:snippet '(delete-file-recursively "test_rc")))

(define rust-ndarray-0.14.0
  (crate-source "ndarray" "0.14.0"
                "011wqzmrd9gpfcfvy1xfbskqfiahn96pmi2d0r9x34d682amq3bc"))

(define rust-ndarray-0.15.6
  (crate-source "ndarray" "0.15.6"
                "0cpsm28hyk8qfjs4g9649dprv3hm53z12qqwyyjqbi3yjr72vcdd"))

(define rust-ndk-0.9.0
  (crate-source "ndk" "0.9.0"
                "1m32zpmi5w1pf3j47k6k5fw395dc7aj8d0mdpsv53lqkprxjxx63"))

(define rust-ndk-context-0.1.1
  (crate-source "ndk-context" "0.1.1"
                "12sai3dqsblsvfd1l1zab0z6xsnlha3xsfl7kagdnmj3an3jvc17"))

(define rust-ndk-sys-0.6.0+11769913
  (crate-source "ndk-sys" "0.6.0+11769913"
                "0wx8r6pji20if4xs04g73gxl98nmjrfc73z0v6w1ypv6a4qdlv7f"))

(define rust-new-debug-unreachable-1.0.6
  (crate-source "new_debug_unreachable" "1.0.6"
                "11phpf1mjxq6khk91yzcbd3ympm78m3ivl7xg6lg2c0lf66fy3k5"))

(define rust-newtype-derive-0.1.6
  (crate-source "newtype_derive" "0.1.6"
                "1v3170xscs65gjx5vl1zjnqp86wngbzw3n2q74ibfnqqkx6x535c"))

(define rust-nibble-vec-0.1.0
  (crate-source "nibble_vec" "0.1.0"
                "0hsdp3s724s30hkqz74ky6sqnadhp2xwcj1n1hzy4vzkz4yxi9bp"))

(define rust-nix-0.22.3
  (crate-source "nix" "0.22.3"
                "1bsgc8vjq07a1wg9vz819bva3dvn58an4r87h80dxrfqkqanz4g4"
                #:snippet '(delete-file-recursively "test")))

(define rust-nix-0.23.2
  (crate-source "nix" "0.23.2"
                "0p5kxhm5d8lry0szqbsllpcb5i3z7lg1dkglw0ni2l011b090dwg"
                #:snippet '(delete-file-recursively "test")))

(define rust-nix-0.24.3
  (crate-source "nix" "0.24.3"
                "0sc0yzdl51b49bqd9l9cmimp1sw1hxb8iyv4d35ww6d7m5rfjlps"
                #:snippet '(delete-file-recursively "test")))

(define rust-nix-0.25.1
  (crate-source "nix" "0.25.1"
                "1r4vyp5g1lxzpig31bkrhxdf2bggb4nvk405x5gngzfvwxqgyipk"
                #:snippet '(delete-file-recursively "test")))

(define rust-nix-0.26.4
  (crate-source "nix" "0.26.4"
                "06xgl4ybb8pvjrbmc3xggbgk3kbs1j0c4c0nzdfrmpbgrkrym2sr"
                #:snippet '(delete-file-recursively "test")))

(define rust-nix-0.27.1
  (crate-source "nix" "0.27.1"
                "0ly0kkmij5f0sqz35lx9czlbk6zpihb7yh1bsy4irzwfd2f4xc1f"
                #:snippet '(delete-file-recursively "test")))

(define rust-nix-0.28.0
  (crate-source "nix" "0.28.0"
                "1r0rylax4ycx3iqakwjvaa178jrrwiiwghcw95ndzy72zk25c8db"
                #:snippet '(delete-file-recursively "test")))

(define rust-nix-0.29.0
  (crate-source "nix" "0.29.0"
                "0ikvn7s9r2lrfdm3mx1h7nbfjvcc6s9vxdzw7j5xfkd2qdnp9qki"
                #:snippet '(delete-file-recursively "test")))

(define rust-nom-7.1.3
  (crate-source "nom" "7.1.3"
                "0jha9901wxam390jcf5pfa0qqfrgh8li787jx2ip0yk5b8y9hwyj"))

(define rust-noop-proc-macro-0.3.0
  (crate-source "noop_proc_macro" "0.3.0"
                "1j2v1c6ric4w9v12h34jghzmngcwmn0hll1ywly4h6lcm4rbnxh6"))

(define rust-normalize-line-endings-0.3.0
  (crate-source "normalize-line-endings" "0.3.0"
                "1gp52dfn2glz26a352zra8h04351icf0fkqzw1shkwrgh1vpz031"))

(define rust-normpath-1.3.0
  (crate-source "normpath" "1.3.0"
                "1vfplrj3miplk0qc7b6psvf6vrmhr2whvqvlvk09lm5iqibik4f8"))

(define rust-notify-6.1.1
  (crate-source "notify" "6.1.1"
                "0bad98r0ilkhhq2jg3zs11zcqasgbvxia8224wpasm74n65vs1b2"))

(define rust-notify-debouncer-full-0.3.2
  (crate-source "notify-debouncer-full" "0.3.2"
                "0kpkz1y4v2kwh5rgkhx4h2fxnwqka30lsrcy2vzwk2cpfdkd2zzv"))

(define rust-ntapi-0.3.7
  (crate-source "ntapi" "0.3.7"
                "03wqq2x5jv5xrsirbxmm460gcfmpakjpq8yqmc5lzfrgznkp91y2"))

(define rust-ntapi-0.4.1
  (crate-source "ntapi" "0.4.1"
                "1r38zhbwdvkis2mzs6671cm1p6djgsl49i7bwxzrvhwicdf8k8z8"))

(define rust-nu-ansi-term-0.46.0
  (crate-source "nu-ansi-term" "0.46.0"
                "115sywxh53p190lyw97alm14nc004qj5jm5lvdj608z84rbida3p"))

(define rust-nu-ansi-term-0.50.1
  (crate-source "nu-ansi-term" "0.50.1"
                "16a3isvbxx8pa3lk71h3cq2fsx2d17zzq42j4mhpxy81gl2qx8nl"))

(define rust-nu-cli-0.103.0
  (crate-source "nu-cli" "0.103.0"
                "0w7fqf138sz6rk5krsi3w5qm6z05ii2c8khh9gayiqfp01vv1ggz"))

(define rust-nu-cmd-base-0.103.0
  (crate-source "nu-cmd-base" "0.103.0"
                "1ln1hr86kzy0d4r16r5acg1yhrn126yz0variivq6cxzwzgzmqxm"))

(define rust-nu-cmd-extra-0.103.0
  (crate-source "nu-cmd-extra" "0.103.0"
                "1sdlyagcsdgs832l3d6xrlhnfhl6j24vzi2zh9i3lip45ssacsz2"))

(define rust-nu-cmd-lang-0.103.0
  (crate-source "nu-cmd-lang" "0.103.0"
                "0382aqmzyr3rx8qhvfzrkcvj6yz5v277q7928si49jc9y7ir7slc"))

(define rust-nu-cmd-plugin-0.103.0
  (crate-source "nu-cmd-plugin" "0.103.0"
                "1jhjqnz7yvm8idhl4hj3yb923rvs3rn2587k59ky9sb5xyl9nq31"))

(define rust-nu-color-config-0.103.0
  (crate-source "nu-color-config" "0.103.0"
                "08k3vjg8wylv0bhznmagswshqgwfbba0iqxw78wwfyy33s8khdmx"))

(define rust-nu-command-0.103.0
  (crate-source "nu-command" "0.103.0"
                "1v6ph836j86v2vibhrg2mjq4nfqm4mbxrvgg6sghn4zs63y18mcy"))

(define rust-nu-derive-value-0.103.0
  (crate-source "nu-derive-value" "0.103.0"
                "098dr378j106pyvcyp654w43lm1igsi62yl2k8m5b4k86sc527wg"))

(define rust-nu-engine-0.103.0
  (crate-source "nu-engine" "0.103.0"
                "0mma3n2abhqylfxa1qhi6a0ck43i9zq11dy546fjbr0q9jxibdqc"))

(define rust-nu-explore-0.103.0
  (crate-source "nu-explore" "0.103.0"
                "03hw24njj4h9zmc63d7wbg6bvnrc5g4lasssvq5kpa79p2av60wx"))

(define rust-nu-glob-0.103.0
  (crate-source "nu-glob" "0.103.0"
                "016p18zd8fd9169j7j7i0m55zrdv4hlgcqf5xqwm9mryb5vaakwh"))

(define rust-nu-json-0.103.0
  (crate-source "nu-json" "0.103.0"
                "1affhp2fxa51kyb6v9kyvp7sijhl40dfk8ynlvkp6fbbnj2li8qd"))

(define rust-nu-lsp-0.103.0
  (crate-source "nu-lsp" "0.103.0"
                "0b42ihc2mcpfv4hmkh0hghr089awx50b5zzwm1a2c7ikykilz9c0"))

(define rust-nu-parser-0.103.0
  (crate-source "nu-parser" "0.103.0"
                "148zq4wm7a28sqp362af0g7m92m7qijx0zn6rd5m7li3q5v6vb6s"))

(define rust-nu-path-0.103.0
  (crate-source "nu-path" "0.103.0"
                "1ymj4n4fg46alnyymi8k3zvf3pn7zjj1zx5lzfc1ybg44rgsbqz6"))

(define rust-nu-plugin-core-0.103.0
  (crate-source "nu-plugin-core" "0.103.0"
                "1zj52rnazk49w0nwd9ip28xgmf1hynh6b3kfq999ijpnqz8z39jv"))

(define rust-nu-plugin-engine-0.103.0
  (crate-source "nu-plugin-engine" "0.103.0"
                "17vwr08s2n4ssdv1bjs57b7ljrlgc1jp1gcp7hak8i2ycvzrrizd"))

(define rust-nu-plugin-protocol-0.103.0
  (crate-source "nu-plugin-protocol" "0.103.0"
                "11qznn3c0ia5gd99jv3ds3wfiykaw6kyvcwz6hdmc2crlmjz3gl8"))

(define rust-nu-pretty-hex-0.103.0
  (crate-source "nu-pretty-hex" "0.103.0"
                "1mp9q6j2gx7d3r19rkgh5vmni21kwcqvb0xkskxvz0dqyi40a2m1"))

(define rust-nu-protocol-0.103.0
  (crate-source "nu-protocol" "0.103.0"
                "1dvabyfqdqlpi2ixir85nahbg7y95zv768yljh4qw7hp1n3badfa"))

(define rust-nu-std-0.103.0
  (crate-source "nu-std" "0.103.0"
                "1h12bca1s7srswazfi6i82zzfphbp4ihnkrk4bdzqjl2jfn3amj6"))

(define rust-nu-system-0.103.0
  (crate-source "nu-system" "0.103.0"
                "09rvsapdiyv61h3sk2a6ybi3w6j90cg9x1xl6syp9lmcb4f9pfvh"))

(define rust-nu-table-0.103.0
  (crate-source "nu-table" "0.103.0"
                "18v1ax4ynm1jl6xk95agrdqs0i9jxx5qvdwhfnbgpx5wn5w5mcy9"))

(define rust-nu-term-grid-0.103.0
  (crate-source "nu-term-grid" "0.103.0"
                "133fnxqj7icwvqiblkyf46548wjb3014jq72wmdschwkjmgffszi"))

(define rust-nu-test-support-0.103.0
  (crate-source "nu-test-support" "0.103.0"
                "1hdl24c37qxk7b1563g9w05bpxriwnjbk9fym8pcr2gvf9g3xgcm"))

(define rust-nu-utils-0.103.0
  (crate-source "nu-utils" "0.103.0"
                "14s74lc376hw38f97chlnxn306vfadn2hl020abm7xwl7id3809g"))

(define rust-nucleo-matcher-0.3.1
  (crate-source "nucleo-matcher" "0.3.1"
                "11dc5kfin1n561qdcg0x9aflvw876a8vldmqjhs5l6ixfcwgacxz"))

(define rust-num-0.4.3
  (crate-source "num" "0.4.3"
                "08yb2fc1psig7pkzaplm495yp7c30m4pykpkwmi5bxrgid705g9m"))

(define rust-num-bigint-0.4.6
  (crate-source "num-bigint" "0.4.6"
                "1f903zd33i6hkjpsgwhqwi2wffnvkxbn6rv4mkgcjcqi7xr4zr55"))

(define rust-num-bigint-dig-0.8.4
  (crate-source "num-bigint-dig" "0.8.4"
                "0lb12df24wgxxbspz4gw1sf1kdqwvpdcpwq4fdlwg4gj41c1k16w"))

(define rust-num-complex-0.2.4
  (crate-source "num-complex" "0.2.4"
                "15dwaksw729r3v14sgzc9723s3fnfixiir8jzwx7b7kim48r9cdn"))

(define rust-num-complex-0.3.1
  (crate-source "num-complex" "0.3.1"
                "1igjwm5kk2df9mxmpb260q6p40xfnkrq4smymgdqg2sm1hn66zbl"))

(define rust-num-complex-0.4.6
  (crate-source "num-complex" "0.4.6"
                "15cla16mnw12xzf5g041nxbjjm9m85hdgadd5dl5d0b30w9qmy3k"))

(define rust-num-conv-0.1.0
  (crate-source "num-conv" "0.1.0"
                "1ndiyg82q73783jq18isi71a7mjh56wxrk52rlvyx0mi5z9ibmai"))

(define rust-num-cpus-1.16.0
  (crate-source "num_cpus" "1.16.0"
                "0hra6ihpnh06dvfvz9ipscys0xfqa9ca9hzp384d5m02ssvgqqa1"))

(define rust-num-derive-0.3.3
  (crate-source "num-derive" "0.3.3"
                "0gbl94ckzqjdzy4j8b1p55mz01g6n1l9bckllqvaj0wfz7zm6sl7"))

(define rust-num-derive-0.4.2
  (crate-source "num-derive" "0.4.2"
                "00p2am9ma8jgd2v6xpsz621wc7wbn1yqi71g15gc3h67m7qmafgd"))

(define rust-num-enum-0.6.1
  (crate-source "num_enum" "0.6.1"
                "18bna04g6zq978z2b4ygz0f8pbva37id4xnpgwh8l41w1m1mn0bs"))

(define rust-num-enum-0.7.3
  (crate-source "num_enum" "0.7.3"
                "0yai0vafhy85mvhknzfqd7lm04hzaln7i5c599rhy8mj831kyqaf"))

(define rust-num-enum-derive-0.6.1
  (crate-source "num_enum_derive" "0.6.1"
                "19k57c0wg56vzzj2w77jsi8nls1b8xh8pvpzjnrgf8d9cnvpsrln"))

(define rust-num-enum-derive-0.7.3
  (crate-source "num_enum_derive" "0.7.3"
                "0mksna1jj87ydh146gn6jcqkvvs920c3dgh0p4f3xk184kpl865g"))

(define rust-num-format-0.4.4
  (crate-source "num-format" "0.4.4"
                "1hvjmib117jspyixfr76f900mhz5zfn71dnyqg9iywb339vxjlm6"))

(define rust-num-integer-0.1.46
  (crate-source "num-integer" "0.1.46"
                "13w5g54a9184cqlbsq80rnxw4jj4s0d8wv75jsq5r2lms8gncsbr"))

(define rust-num-iter-0.1.45
  (crate-source "num-iter" "0.1.45"
                "1gzm7vc5g9qsjjl3bqk9rz1h6raxhygbrcpbfl04swlh0i506a8l"))

(define rust-num-rational-0.2.4
  (crate-source "num-rational" "0.2.4"
                "1vsaz96chxcgpqd5a0dq8hb3b4sj6dnlhwmpbkf4mx6vnls0202w"))

(define rust-num-rational-0.4.2
  (crate-source "num-rational" "0.4.2"
                "093qndy02817vpgcqjnj139im3jl7vkq4h68kykdqqh577d18ggq"))

(define rust-num-threads-0.1.7
  (crate-source "num_threads" "0.1.7"
                "1ngajbmhrgyhzrlc4d5ga9ych1vrfcvfsiqz6zv0h2dpr2wrhwsw"))

(define rust-num-traits-0.2.19
  (crate-source "num-traits" "0.2.19"
                "0h984rhdkkqd4ny9cif7y2azl3xdfb7768hb9irhpsch4q3gq787"))

(define rust-number-prefix-0.3.0
  (crate-source "number_prefix" "0.3.0"
                "0slm4mqmpgs6hvz22ycny9lvyvl9ivs80a1lncslp7lszz02zc0p"))

(define rust-number-prefix-0.4.0
  (crate-source "number_prefix" "0.4.0"
                "1wvh13wvlajqxkb1filsfzbrnq0vrmrw298v2j3sy82z1rm282w3"))

(define rust-nuon-0.103.0
  (crate-source "nuon" "0.103.0"
                "05nd4acyfc7nbl2bnvlcra60h8n28b8bqaa24j645rk9hz6ckb0r"))

(define rust-objc-0.2.7
  (crate-source "objc" "0.2.7"
                "1cbpf6kz8a244nn1qzl3xyhmp05gsg4n313c9m3567625d3innwi"))

(define rust-objc-foundation-0.1.1
  (crate-source "objc-foundation" "0.1.1"
                "1y9bwb3m5fdq7w7i4bnds067dhm4qxv4m1mbg9y61j9nkrjipp8s"))

(define rust-objc-id-0.1.1
  (crate-source "objc_id" "0.1.1"
                "0fq71hnp2sdblaighjc82yrac3adfmqzhpr11irhvdfp9gdlsbf9"))

(define rust-objc-sys-0.3.5
  (crate-source "objc-sys" "0.3.5"
                "0423gry7s3rmz8s3pzzm1zy5mdjif75g6dbzc2lf2z0c77fipffd"))

(define rust-objc2-0.5.2
  (crate-source "objc2" "0.5.2"
                "015qa2d3vh7c1j2736h5wjrznri7x5ic35vl916c22gzxva8b9s6"))

(define rust-objc2-0.6.0
  (crate-source "objc2" "0.6.0"
                "0nbf78zvqz3wnp23iianpwbds563fiz8b6bsnxizikyrj18zcc9m"))

(define rust-objc2-app-kit-0.2.2
  (crate-source "objc2-app-kit" "0.2.2"
                "1zqyi5l1bm26j1bgmac9783ah36m5kcrxlqp5carglnpwgcrms74"))

(define rust-objc2-app-kit-0.3.0
  (crate-source "objc2-app-kit" "0.3.0"
                "1yyh2j44kjhj2n55y2vlj82lzsbfpngvivv9w1x2z3hpawrgj1jr"))

(define rust-objc2-cloud-kit-0.2.2
  (crate-source "objc2-cloud-kit" "0.2.2"
                "02dhjvmcq8c2bwj31jx423jygif1scs9f0lmlab0ayhw75b3ppbl"))

(define rust-objc2-contacts-0.2.2
  (crate-source "objc2-contacts" "0.2.2"
                "12a8m927xrrxa54xhqhqnkkl1a6l07pyrpnqfk9jz09kkh755zx5"))

(define rust-objc2-core-data-0.2.2
  (crate-source "objc2-core-data" "0.2.2"
                "1vvk8zjylfjjj04dzawydmqqz5ajvdkhf22cnb07ihbiw14vyzv1"))

(define rust-objc2-core-foundation-0.3.0
  (crate-source "objc2-core-foundation" "0.3.0"
                "09frj2bc6w6dnpjfix1skq8g91kx7w788bqwiaa2c7a74l7zdsns"))

(define rust-objc2-core-graphics-0.3.0
  (crate-source "objc2-core-graphics" "0.3.0"
                "00nynwlbppcdp7q51cmq4x04sns0lqhhla8kcmmkarcbc81adp7q"))

(define rust-objc2-core-image-0.2.2
  (crate-source "objc2-core-image" "0.2.2"
                "102csfb82zi2sbzliwsfd589ckz0gysf7y6434c9zj97lmihj9jm"))

(define rust-objc2-core-location-0.2.2
  (crate-source "objc2-core-location" "0.2.2"
                "10apgsrigqryvi4rcc0f6yfjflvrl83f4bi5hkr48ck89vizw300"))

(define rust-objc2-encode-4.1.0
  (crate-source "objc2-encode" "4.1.0"
                "0cqckp4cpf68mxyc2zgnazj8klv0z395nsgbafa61cjgsyyan9gg"))

(define rust-objc2-foundation-0.2.2
  (crate-source "objc2-foundation" "0.2.2"
                "1a6mi77jsig7950vmx9ydvsxaighzdiglk5d229k569pvajkirhf"))

(define rust-objc2-foundation-0.3.0
  (crate-source "objc2-foundation" "0.3.0"
                "166rz3jh3b2nrwvldllp3ihq4lb4j5pkjnyv2naw70jb074wc89s"))

(define rust-objc2-io-surface-0.3.0
  (crate-source "objc2-io-surface" "0.3.0"
                "068wb02jcnna2qgir250nfvrsic4kz1rx7ks39p0h416wf3qn6hn"))

(define rust-objc2-link-presentation-0.2.2
  (crate-source "objc2-link-presentation" "0.2.2"
                "160k4qh00yrx57dabn3hzas4r98kmk9bc0qsy1jvwday3irax8d1"))

(define rust-objc2-metal-0.2.2
  (crate-source "objc2-metal" "0.2.2"
                "1mmdga66qpxrcfq3gxxhysfx3zg1hpx4z886liv3j0pnfq9bl36x"))

(define rust-objc2-quartz-core-0.2.2
  (crate-source "objc2-quartz-core" "0.2.2"
                "0ynw8819c36l11rim8n0yzk0fskbzrgaqayscyqi8swhzxxywaz4"))

(define rust-objc2-symbols-0.2.2
  (crate-source "objc2-symbols" "0.2.2"
                "1p04hjkxan18g2b7h9n2n8xxsvazapv2h6mfmmdk06zc7pz4ws0a"))

(define rust-objc2-ui-kit-0.2.2
  (crate-source "objc2-ui-kit" "0.2.2"
                "0vrb5r8z658l8c19bx78qks8c5hg956544yirf8npk90idwldfxq"))

(define rust-objc2-uniform-type-identifiers-0.2.2
  (crate-source "objc2-uniform-type-identifiers" "0.2.2"
                "1ziv4wkbxcaw015ypg0q49ycl7m14l3x56mpq2k1rznv92bmzyj4"))

(define rust-objc2-user-notifications-0.2.2
  (crate-source "objc2-user-notifications" "0.2.2"
                "1cscv2w3vxzaslz101ddv0z9ycrrs4ayikk4my4qd3im8bvcpkvn"))

(define rust-object-0.32.2
  (crate-source "object" "0.32.2"
                "0hc4cjwyngiy6k51hlzrlsxgv5z25vv7c2cp0ky1lckfic0259m6"))

(define rust-object-0.36.7
  (crate-source "object" "0.36.7"
                "11vv97djn9nc5n6w1gc6bd96d2qk2c8cg1kw5km9bsi3v4a8x532"))

(define rust-oem-cp-2.0.0
  (crate-source "oem_cp" "2.0.0"
                "1r0fn4bnmc78pkqhvzjjbm1zidgzvryspdz6hsdb1nml5a83h09k"))

(define rust-omnipath-0.1.6
  (crate-source "omnipath" "0.1.6"
                "0xd5a4xwsfmhzk59v6wz65f59rk16d7gvkg90w1qhb0jg08b7bc0"))

(define rust-once-cell-1.21.3
  (crate-source "once_cell" "1.21.3"
                "0b9x77lb9f1j6nqgf5aka4s2qj0nly176bpbrv6f9iakk5ff3xa2"))

(define rust-onenote-parser-0.3.1.29c0853
  (origin
    (method git-fetch)
    (uri (git-reference (url "https://github.com/Cisco-Talos/onenote.rs.git")
                        (commit "29c08532252b917543ff268284f926f30876bb79")))
    (file-name (git-file-name "rust-onenote-parser" "0.3.1.29c0853"))
    (sha256 (base32 "1sfx3jgj1vgwh00kckl5hfbmdpp4wmvszsxwzi1k10nihryvkdy5"))
    (modules '((guix build utils)))
    (snippet '(delete-file-recursively "tests"))))

(define rust-oo7-0.2.2
  (crate-source "oo7" "0.2.2"
                "13cpaq7f51gqcspd4097vjr7r2cjpxpn6c02x67dsdizk0xaiv5c"))

(define rust-oorandom-11.1.5
  (crate-source "oorandom" "11.1.5"
                "07mlf13z453fq01qff38big1lh83j8l6aaglf63ksqzzqxc0yyfn"))

(define rust-opaque-debug-0.3.1
  (crate-source "opaque-debug" "0.3.1"
                "10b3w0kydz5jf1ydyli5nv10gdfp97xh79bgz327d273bs46b3f0"))

(define rust-open-5.3.2
  (crate-source "open" "5.3.2"
                "15ggfx1p8rl7w4rr1n5qj1wxy1kk7757lsjpyc947a9fwri3aj72"))

(define rust-opener-0.7.2
  (crate-source "opener" "0.7.2"
                "10bn0m6pfv9mvv9lky0l48fb6vflx9pkg8sir1aa73gh9mg2x0fh"))

(define rust-openssl-0.10.57
  (crate-source "openssl" "0.10.57"
                "0z0f8g84y0lvnbc60586ibjpf8r1q1dv672vfqan5d5bk7imxhms"
                #:snippet '(delete-file-recursively "test")))

(define rust-openssl-0.10.72
  (crate-source "openssl" "0.10.72"
                "1np54pm6hw512rmfjv3kc54h8yvf51mdlm8a8cc33xx1b1yympzy"
                #:snippet '(delete-file-recursively "test")))

(define rust-openssl-macros-0.1.1
  (crate-source "openssl-macros" "0.1.1"
                "173xxvfc63rr5ybwqwylsir0vq6xsj4kxiv4hmg4c3vscdmncj59"))

(define rust-openssl-probe-0.1.6
  (crate-source "openssl-probe" "0.1.6"
                "0bl52x55laalqb707k009h8kfawliwp992rlsvkzy49n47p2fpnh"))

(define rust-openssl-src-300.4.2+3.4.1 #f)

(define rust-openssl-src-300.5.0+3.5.0 #f)

(define rust-openssl-sys-0.9.107
  (crate-source "openssl-sys" "0.9.107"
                "01yydv8yaagdnapvair8b6rggf225lwb854h99s9qx44rnd9g242"
                #:snippet
                #~(begin
                    ;; Remove dependency on boringssl and vendor openssl source.
                    (substitute* "Cargo.toml.orig"
                      (("vendored = .*") "vendored = []\n")
                      ((".*bssl.*") "")
                      ((".*openssl-src.*") "")
                      ;; Allow any version of bindgen.
                      (("(bindgen = \\{ version =) \".*\"," _ bindgen)
                       (string-append bindgen "\"*\",")))
                    (copy-file "Cargo.toml.orig" "Cargo.toml"))))

(define rust-option-ext-0.2.0
  (crate-source "option-ext" "0.2.0"
                "0zbf7cx8ib99frnlanpyikm1bx8qn8x602sw1n7bg6p9x94lyx04"))

(define rust-option-operations-0.5.0
  (crate-source "option-operations" "0.5.0"
                "1l13n9487gk6063zzjdwzqbig78n8mh6dxsbiq9nbaxfn5xx49kw"))

(define rust-orbclient-0.3.48
  (crate-source "orbclient" "0.3.48"
                "0hzxjsvvsl5i9d3aqzc6kdcsch1i6flij5dkignhhkz2qb72c2xs"
                #:snippet '(delete-file-recursively "res")))

(define rust-ordered-float-1.1.1
  (crate-source "ordered-float" "1.1.1"
                "1drp3v9jsm1mn0wqp4p5y19j3bmz3w5rw4yxzi39zlld4wssy19k"))

(define rust-ordered-float-2.10.1
  (crate-source "ordered-float" "2.10.1"
                "075i108hr95pr7hy4fgxivib5pky3b6b22rywya5qyd2wmkrvwb8"))

(define rust-ordered-float-3.9.2
  (crate-source "ordered-float" "3.9.2"
                "1p3jkxlz89ndm4lmwr2n5kdnckhm5pcmqqkihkag259dff8c7qgi"))

(define rust-ordered-float-5.0.0
  (crate-source "ordered-float" "5.0.0"
                "009z1k7w729ls2sfg4zknn9v63sk1zghnq54p2lwcjjkdvszkhg2"))

(define rust-ordered-stream-0.2.0
  (crate-source "ordered-stream" "0.2.0"
                "0l0xxp697q7wiix1gnfn66xsss7fdhfivl2k7bvpjs4i3lgb18ls"))

(define rust-orion-0.17.9
  (crate-source "orion" "0.17.9"
                "02sk0qv7r8nffqv56im11x4balxjb7zrxw867zab6pvwk9s0nbmz"))

(define rust-os-display-0.1.4
  (crate-source "os_display" "0.1.4"
                "13x07viih4f7l5cicbqw9xv378h0a096vphdclcbjvq2g4dxfpxd"))

(define rust-os-info-3.10.0
  (crate-source "os_info" "3.10.0"
                "1dd6hpdrrmir059dxcbnci96209snb4f597bc1l2hqa7q99lwq1a"))

(define rust-os-pipe-1.2.1
  (crate-source "os_pipe" "1.2.1"
                "10nrh0i507560rsiy4c79fajdmqgbr6dha2pbl9mncrlaq52pzaz"))

(define rust-os-str-bytes-6.6.1
  (crate-source "os_str_bytes" "6.6.1"
                "1885z1x4sm86v5p41ggrl49m58rbzzhd1kj72x46yy53p62msdg2"))

(define rust-ouroboros-0.18.5
  (crate-source "ouroboros" "0.18.5"
                "0ndy8hvp24gs7yxw9wj81hs5rb36wxmpw4i38ylrfjy4p46ha3qy"))

(define rust-ouroboros-macro-0.18.5
  (crate-source "ouroboros_macro" "0.18.5"
                "1l343ss6hlh0abbwjk6zah6mdlyhh1v1imflv3v86c6lsfyjhw1w"))

(define rust-overload-0.1.1
  (crate-source "overload" "0.1.1"
                "0fdgbaqwknillagy1xq7xfgv60qdbk010diwl7s1p0qx7hb16n5i"
                #:snippet '(delete-file "logo.png")))

(define rust-owned-ttf-parser-0.15.2
  (crate-source "owned_ttf_parser" "0.15.2"
                "1frgpf2a5j17ylk1lmrj2lpyhf6izq7x8b1xlbv6ybb3n7zazrh5"
                #:snippet '(delete-file-recursively "fonts")))

(define rust-owo-colors-3.5.0
  (crate-source "owo-colors" "3.5.0"
                "0vyvry6ba1xmpd45hpi6savd8mbx09jpmvnnwkf6z62pk6s4zc61"))

(define rust-owo-colors-4.2.0
  (crate-source "owo-colors" "4.2.0"
                "0r7mxiyxg8zbyjqm8y2n2amykl2i51y6agvjrw036ba2p5dqcdhh"))

(define rust-p384-0.13.1
  (crate-source "p384" "0.13.1"
                "1dnnp133mbpp72mfss3fhm8wx3yp3p3abdhlix27v92j19kz2hpy"
                #:snippet '(delete-file-recursively "src/test_vectors")))

(define rust-pam-sys-0.5.6
  (crate-source "pam-sys" "0.5.6"
                "0d14501d5vybjnzxfjf96321xa5wa36x1xvf02h02zq938qmhj6d"))

(define rust-pango-0.18.3
  (crate-source "pango" "0.18.3"
                "1r5ygq7036sv7w32kp8yxr6vgggd54iaavh3yckanmq4xg0px8kw"))

(define rust-pango-0.19.8
  (crate-source "pango" "0.19.8"
                "1kffxkk7730csly86fkgja50k1184zj9lz49sv7qb0059233439z"))

(define rust-pango-0.20.9
  (crate-source "pango" "0.20.9"
                "1v7h4m7sz0x38il14jzsw7qphbpsw17a0hq8zj5w16ygp30ms7vb"))

(define rust-pango-sys-0.18.0
  (crate-source "pango-sys" "0.18.0"
                "1iaxalcaaj59cl9n10svh4g50v8jrc1a36kd7n9yahx8j7ikfrs3"))

(define rust-pango-sys-0.19.8
  (crate-source "pango-sys" "0.19.8"
                "182bcd6255v5yvnskbhxnb6kwak240z7sn54si2b5h46l17xl0zz"))

(define rust-pango-sys-0.20.9
  (crate-source "pango-sys" "0.20.9"
                "1dds8ljd3ar05c9744s3xlcyg8bkg5a211mpkvj8zgbk2rsrpfqd"))

(define rust-pangocairo-0.19.8
  (crate-source "pangocairo" "0.19.8"
                "1n8wrqy260zpfiifb2n10mbsv3kbrvxm1z7pv8b4w77c08yb9j74"))

(define rust-pangocairo-0.20.7
  (crate-source "pangocairo" "0.20.7"
                "1dlp76pkknxfl6pi41zfcm9kdhchyzjs72pgl196aapa5yd51426"))

(define rust-pangocairo-sys-0.19.8
  (crate-source "pangocairo-sys" "0.19.8"
                "1myq3p8qrd63nlacd4sba66c17lfqgvzv8mpyn2rg1rqhi4h86ar"))

(define rust-pangocairo-sys-0.20.7
  (crate-source "pangocairo-sys" "0.20.7"
                "0rv7mnp2cnrvaq73c3dwf1szc0ngi312z4l3cyjac4br2hjarrjv"))

(define rust-papergrid-0.13.0
  (crate-source "papergrid" "0.13.0"
                "1ggfj9xvqqazcc2avmi9va40crdmlxjxlgjzi4yf25ziy7ggic6j"))

(define rust-parking-2.2.1
  (crate-source "parking" "2.2.1"
                "1fnfgmzkfpjd69v4j9x737b1k8pnn054bvzcn5dm3pkgq595d3gk"))

(define rust-parking-lot-0.11.2
  (crate-source "parking_lot" "0.11.2"
                "16gzf41bxmm10x82bla8d6wfppy9ym3fxsmdjyvn61m66s0bf5vx"))

(define rust-parking-lot-0.12.3
  (crate-source "parking_lot" "0.12.3"
                "09ws9g6245iiq8z975h8ycf818a66q3c6zv4b5h8skpm7hc1igzi"))

(define rust-parking-lot-core-0.8.6
  (crate-source "parking_lot_core" "0.8.6"
                "1p2nfcbr0b9lm9rglgm28k6mwyjwgm4knipsmqbgqaxdy3kcz8k0"))

(define rust-parking-lot-core-0.9.10
  (crate-source "parking_lot_core" "0.9.10"
                "1y3cf9ld9ijf7i4igwzffcn0xl16dxyn4c5bwgjck1dkgabiyh0y"))

(define rust-parse-datetime-0.6.0
  (crate-source "parse_datetime" "0.6.0"
                "1l4n9gsflf1fksfrvyhqyf8bdwxrk0s705l7x86g4jnxwds08wm8"
                #:snippet '(delete-file-recursively "tests")))

(define rust-parse-zoneinfo-0.3.1
  (crate-source "parse-zoneinfo" "0.3.1"
                "093cs8slbd6kyfi6h12isz0mnaayf5ha8szri1xrbqj4inqhaahz"))

(define rust-partial-ref-0.3.3
  (crate-source "partial_ref" "0.3.3"
                "034i78nmzp2bdapvpz8fgh14932aj0s70l5s1kj5d5j7n74qnwhg"))

(define rust-partial-ref-derive-0.3.3
  (crate-source "partial_ref_derive" "0.3.3"
                "10bm0pxwjph40z9pqwdzkkkbyz9n1h6lx69f6jjvb65qnln1s3ih"))

(define rust-pasetors-0.7.2
  (crate-source "pasetors" "0.7.2"
                "0adpf4fd6bgkznrb4fzmbiahvh0c6s6i2pring2wkrx64px48jf5"))

(define rust-paste-1.0.15
  (crate-source "paste" "1.0.15"
                "02pxffpdqkapy292harq6asfjvadgp1s005fip9ljfsn9fvxgh2p"))

(define rust-path-slash-0.2.1
  (crate-source "path-slash" "0.2.1"
                "0hjgljv4vy97qqw9gxnwzqhhpysjss2yhdphfccy3c388afhk48y"))

(define rust-pathdiff-0.2.3
  (crate-source "pathdiff" "0.2.3"
                "1lrqp4ip05df8dzldq6gb2c1sq2gs54gly8lcnv3rhav1qhwx56z"))

(define rust-patricia-tree-0.8.0
  (crate-source "patricia_tree" "0.8.0"
                "0s5fya6rvgg2gxxp5mbv0xdq8jqikps1sc6snk23zrgzkd9z9wii"))

(define rust-pbkdf2-0.12.2
  (crate-source "pbkdf2" "0.12.2"
                "1wms79jh4flpy1zi8xdp4h8ccxv4d85adc6zjagknvppc5vnmvgq"))

(define rust-pcre2-0.2.9.85b7afb rust-pcre2-utf32-0.2)

(define rust-pcre2-sys-0.2.9.85b7afb rust-pcre2-utf32-0.2)

(define rust-peeking-take-while-0.1.2
  (crate-source "peeking_take_while" "0.1.2"
                "16bhqr6rdyrp12zv381cxaaqqd0pwysvm1q8h2ygihvypvfprc8r"))

(define rust-pem-3.0.5
  (crate-source "pem" "3.0.5"
                "1wwfk8sbyi9l18fvvn6z9p2gy7v7q7wimbhvrvixxj8a8zl3ibrq"))

(define rust-pem-rfc7468-0.7.0
  (crate-source "pem-rfc7468" "0.7.0"
                "04l4852scl4zdva31c1z6jafbak0ni5pi0j38ml108zwzjdrrcw8"
                #:snippet '(delete-file-recursively "tests")))

(define rust-pep440-rs-0.6.6
  (crate-source "pep440_rs" "0.6.6"
                "11g35bi0gagfchjcdvpaj1i82gxkrc391fcpm2f0cblw2yissvj6"))

(define rust-pep508-rs-0.6.1
  (crate-source "pep508_rs" "0.6.1"
                "0y99l63mi09vkl7b7qzdw5gn8kpq6kj26c9k280cik4rk947g21z"))

(define rust-percent-encoding-2.3.1
  (crate-source "percent-encoding" "2.3.1"
                "0gi8wgx0dcy8rnv1kywdv98lwcx67hz0a0zwpib5v2i08r88y573"))

(define rust-pest-2.8.0
  (crate-source "pest" "2.8.0"
                "1dp741bxqiracvvwl66mfvlr29byvvph28n4c6ip136m652vg38r"))

(define rust-pest-derive-2.8.0
  (crate-source "pest_derive" "2.8.0"
                "1icp5i01mgpbgwbkrcy4d0ykbxmns4wyz8j1jg6dr1wysz7xj9fp"))

(define rust-pest-generator-2.8.0
  (crate-source "pest_generator" "2.8.0"
                "0hgqngsxfr8y5p47bgjvd038j55ix1x4dpzr6amndaz8ddr02zfv"))

(define rust-pest-meta-2.8.0
  (crate-source "pest_meta" "2.8.0"
                "182w5fyiqm7zbn0p8313xc5wc73rnn59ycm5zk8hcja9f0j877vz"))

(define rust-petgraph-0.5.1
  (crate-source "petgraph" "0.5.1"
                "1dzxda6z17sfxly11m8ja3iargh73pw0s1sdgjyp0qp5dm51cza6"))

(define rust-petgraph-0.6.5
  (crate-source "petgraph" "0.6.5"
                "1ns7mbxidnn2pqahbbjccxkrqkrll2i5rbxx43ns6rh6fn3cridl"
                #:snippet '(for-each delete-file-recursively '("assets"))))

(define rust-petgraph-0.7.1
  (crate-source "petgraph" "0.7.1"
                "0wkpppwrfv1h197asz1p4yfb4li5b1kw0nqllil67n6vj1qb6win"
                #:snippet '(delete-file-recursively "assets")))

(define rust-phf-0.10.1
  (crate-source "phf" "0.10.1"
                "0naj8n5nasv5hj5ldlva3cl6y3sv7zp3kfgqylhbrg55v3mg3fzs"))

(define rust-phf-0.11.3
  (crate-source "phf" "0.11.3"
                "0y6hxp1d48rx2434wgi5g8j1pr8s5jja29ha2b65435fh057imhz"))

(define rust-phf-codegen-0.10.0
  (crate-source "phf_codegen" "0.10.0"
                "1k8kdad9wk2d5972k6jmjki2xpdy2ky4zd19rv7ybm2dpjlc7cag"))

(define rust-phf-codegen-0.11.3
  (crate-source "phf_codegen" "0.11.3"
                "0si1n6zr93kzjs3wah04ikw8z6npsr39jw4dam8yi9czg2609y5f"))

(define rust-phf-generator-0.10.0
  (crate-source "phf_generator" "0.10.0"
                "1mlq6hlajsvlsx6rhw49g9ricsm017lrxmgmmbk85sxm7f4qaljx"))

(define rust-phf-generator-0.11.3
  (crate-source "phf_generator" "0.11.3"
                "0gc4np7s91ynrgw73s2i7iakhb4lzdv1gcyx7yhlc0n214a2701w"))

(define rust-phf-macros-0.11.3
  (crate-source "phf_macros" "0.11.3"
                "05kjfbyb439344rhmlzzw0f9bwk9fp95mmw56zs7yfn1552c0jpq"))

(define rust-phf-shared-0.10.0
  (crate-source "phf_shared" "0.10.0"
                "15n02nc8yqpd8hbxngblar2g53p3nllc93d8s8ih3p5cf7bnlydn"))

(define rust-phf-shared-0.11.3
  (crate-source "phf_shared" "0.11.3"
                "1rallyvh28jqd9i916gk5gk2igdmzlgvv5q0l3xbf3m6y8pbrsk7"))

(define rust-pin-project-1.1.10
  (crate-source "pin-project" "1.1.10"
                "12kadbnfm1f43cyadw9gsbyln1cy7vj764wz5c8wxaiza3filzv7"))

(define rust-pin-project-internal-1.1.10
  (crate-source "pin-project-internal" "1.1.10"
                "0qgqzfl0f4lzaz7yl5llhbg97g68r15kljzihaw9wm64z17qx4bf"))

(define rust-pin-project-lite-0.2.16
  (crate-source "pin-project-lite" "0.2.16"
                "16wzc7z7dfkf9bmjin22f5282783f6mdksnr0nv0j5ym5f9gyg1v"))

(define rust-pin-utils-0.1.0
  (crate-source "pin-utils" "0.1.0"
                "117ir7vslsl2z1a7qzhws4pd01cg2d3338c47swjyvqv2n60v1wb"))

(define rust-piper-0.2.4
  (crate-source "piper" "0.2.4"
                "0rn0mjjm0cwagdkay77wgmz3sqf8fqmv9d9czm79mvr2yj8c9j4n"))

(define rust-pipewire-0.8.0.fd3d8f7 rust-pipewire-for-niri)

(define rust-pipewire-sys-0.8.0.fd3d8f7 rust-pipewire-for-niri)

(define rust-pixman-0.2.1
  (crate-source "pixman" "0.2.1"
                "1pqybqb7rmd58yr9xvmd8iix30znw5w71cq2wnlc16n1jva1g8nf"))

(define rust-pixman-sys-0.1.0
  (crate-source "pixman-sys" "0.1.0"
                "1nja8kc7zs1w4lhllvsgssa0b07n4cgwb0zyvqapj7g8i4z4i851"))

(define rust-pkcs8-0.10.2
  (crate-source "pkcs8" "0.10.2"
                "1dx7w21gvn07azszgqd3ryjhyphsrjrmq5mmz1fbxkj5g0vv4l7r"
                #:snippet '(delete-file-recursively "tests")))

(define rust-pkg-config-0.3.32
  (crate-source "pkg-config" "0.3.32"
                "0k4h3gnzs94sjb2ix6jyksacs52cf1fanpwsmlhjnwrdnp8dppby"))

(define rust-plain-0.2.3
  (crate-source "plain" "0.2.3"
                "19n1xbxb4wa7w891268bzf6cbwq4qvdb86bik1z129qb0xnnnndl"))

(define rust-platform-info-2.0.5
  (crate-source "platform-info" "2.0.5"
                "06j5v6hg914lbdr732jfx5syx703l5qwy1qk6dm4zjyqznrswfbm"))

(define rust-platforms-3.5.0
  (crate-source "platforms" "3.5.0"
                "009pi8n0vca83az15gj0fzpjq0dwy1y96jw6kr7ml21p08q6fd6l"))

(define rust-plotters-0.3.7
  (crate-source "plotters" "0.3.7"
                "0ixpy9svpmr2rkzkxvvdpysjjky4gw104d73n7pi2jbs7m06zsss"))

(define rust-plotters-backend-0.3.7
  (crate-source "plotters-backend" "0.3.7"
                "0ahpliim4hrrf7d4ispc2hwr7rzkn6d6nf7lyyrid2lm28yf2hnz"))

(define rust-plotters-svg-0.3.7
  (crate-source "plotters-svg" "0.3.7"
                "0w56sxaa2crpasa1zj0bhxzihlapqfkncggavyngg0w86anf5fji"))

(define rust-png-0.17.16
  (crate-source "png" "0.17.16"
                "09kmkms9fmkbkarw0lnf0scqvjwwg3r7riddag0i3q39r0pil5c2"))

(define rust-polling-2.8.0
  (crate-source "polling" "2.8.0"
                "1kixxfq1af1k7gkmmk9yv4j2krpp4fji2r8j4cz6p6d7ihz34bab"))

(define rust-polling-3.7.4
  (crate-source "polling" "3.7.4"
                "0bs4nhwfwsvlzlhah2gbhj3aa9ynvchv2g350wapswh26a65c156"))

(define rust-polyval-0.6.2
  (crate-source "polyval" "0.6.2"
                "09gs56vm36ls6pyxgh06gw2875z2x77r8b2km8q28fql0q6yc7wx"))

(define rust-pori-0.0.0
  (crate-source "pori" "0.0.0"
                "01p9g4fn3kasnmwj8i4plzk6nnnk7ak2qsfcv9b9y4zcilrkv9m4"))

(define rust-portable-atomic-1.11.0
  (crate-source "portable-atomic" "1.11.0"
                "0glb2wngflvfmg789qbf6dbnwcf6ai212fs7n0lf1c66rd49n3im"))

(define rust-portable-atomic-util-0.2.4
  (crate-source "portable-atomic-util" "0.2.4"
                "01rmx1li07ixsx3sqg2bxqrkzk7b5n8pibwwf2589ms0s3cg18nq"))

(define rust-powerfmt-0.2.0
  (crate-source "powerfmt" "0.2.0"
                "14ckj2xdpkhv3h6l5sdmb9f1d57z8hbfpdldjc2vl5givq2y77j3"))

(define rust-ppv-lite86-0.2.21
  (crate-source "ppv-lite86" "0.2.21"
                "1abxx6qz5qnd43br1dd9b2savpihzjza8gb4fbzdql1gxp2f7sl5"))

(define rust-precomputed-hash-0.1.1
  (crate-source "precomputed-hash" "0.1.1"
                "075k9bfy39jhs53cb2fpb9klfakx2glxnf28zdw08ws6lgpq6lwj"))

(define rust-predicates-3.1.3
  (crate-source "predicates" "3.1.3"
                "0wrm57acvagx0xmh5xffx5xspsr2kbggm698x0vks132fpjrxld5"))

(define rust-predicates-core-1.0.9
  (crate-source "predicates-core" "1.0.9"
                "1yjz144yn3imq2r4mh7k9h0r8wv4yyjjj57bs0zwkscz24mlczkj"))

(define rust-predicates-tree-1.0.12
  (crate-source "predicates-tree" "1.0.12"
                "0p223d9y02ywwxs3yl68kziswz4da4vabz67jfhp7yqx71njvpbj"))

(define rust-pretty-assertions-1.4.1
  (crate-source "pretty_assertions" "1.4.1"
                "0v8iq35ca4rw3rza5is3wjxwsf88303ivys07anc5yviybi31q9s"
                #:snippet '(delete-file-recursively "examples")))

(define rust-pretty-env-logger-0.5.0
  (crate-source "pretty_env_logger" "0.5.0"
                "076w9dnvcpx6d3mdbkqad8nwnsynb7c8haxmscyrz7g3vga28mw6"))

(define rust-pretty-hex-0.4.1
  (crate-source "pretty-hex" "0.4.1"
                "0m0j8pqmh6gq1mq7yzp12z0ix159fw0di5lhiwv2y1j0m3j3xj5v"
                #:snippet '(delete-file-recursively "tests")))

(define rust-prettyplease-0.1.25
  (crate-source "prettyplease" "0.1.25"
                "11lskniv8pf8y8bn4dc3nmjapfhnibxbm5gamp2ad9qna3lld1kc"))

(define rust-prettyplease-0.2.32
  (crate-source "prettyplease" "0.2.32"
                "1xmdmwhsvqc8l5ns029vzjida4k3lp5ynin0xra43qsiki0wakk6"))

(define rust-primal-check-0.3.4
  (crate-source "primal-check" "0.3.4"
                "025xnak4rhkwa4h970bjb3cvp2k853wviyr84n8gjfhy65dqj3fw"))

(define rust-primeorder-0.13.6
  (crate-source "primeorder" "0.13.6"
                "1rp16710mxksagcjnxqjjq9r9wf5vf72fs8wxffnvhb6i6hiqgim"))

(define rust-print-bytes-1.2.0
  (crate-source "print_bytes" "1.2.0"
                "0a1pc0bs0f3mgy86mazdy81k6iaj8s23ly49mcka4in2pj27lzxl"))

(define rust-print-positions-0.6.1
  (crate-source "print-positions" "0.6.1"
                "026jzdf63b37bb9ix3mpczln2pqylsiwkkxhikj05x9y1r3r7x8x"))

(define rust-priority-queue-2.3.1
  (crate-source "priority-queue" "2.3.1"
                "13ff7y3s9x6m9q0dazdnjz6v0b3j2iyxfjljm9cim6jql5gp027g"))

(define rust-proc-macro-crate-1.3.1
  (crate-source "proc-macro-crate" "1.3.1"
                "069r1k56bvgk0f58dm5swlssfcp79im230affwk6d9ck20g04k3z"))

(define rust-proc-macro-crate-2.0.0
  (crate-source "proc-macro-crate" "2.0.0"
                "1s23imns07vmacn2xjd5hv2h6rr94iqq3fd2frwa6i4h2nk6d0vy"))

(define rust-proc-macro-crate-3.3.0
  (crate-source "proc-macro-crate" "3.3.0"
                "0d9xlymplfi9yv3f5g4bp0d6qh70apnihvqcjllampx4f5lmikpd"))

(define rust-proc-macro-error-1.0.4
  (crate-source "proc-macro-error" "1.0.4"
                "1373bhxaf0pagd8zkyd03kkx6bchzf6g0dkwrwzsnal9z47lj9fs"))

(define rust-proc-macro-error-attr-1.0.4
  (crate-source "proc-macro-error-attr" "1.0.4"
                "0sgq6m5jfmasmwwy8x4mjygx5l7kp8s4j60bv25ckv2j1qc41gm1"))

(define rust-proc-macro-error-attr2-2.0.0
  (crate-source "proc-macro-error-attr2" "2.0.0"
                "1ifzi763l7swl258d8ar4wbpxj4c9c2im7zy89avm6xv6vgl5pln"))

(define rust-proc-macro-error2-2.0.1
  (crate-source "proc-macro-error2" "2.0.1"
                "00lq21vgh7mvyx51nwxwf822w2fpww1x0z8z0q47p8705g2hbv0i"))

(define rust-proc-macro2-1.0.94
  (crate-source "proc-macro2" "1.0.94"
                "114wxb56gdj9vy44q0ll3l2x9niqzcbyqikydmlb5f3h5rsp26d3"))

(define rust-proc-macro2-diagnostics-0.10.1
  (crate-source "proc-macro2-diagnostics" "0.10.1"
                "1j48ipc80pykvhx6yhndfa774s58ax1h6sm6mlhf09ls76f6l1mg"))

(define rust-procfs-0.17.0
  (crate-source "procfs" "0.17.0"
                "17swyjqinpb745f07dpdi7c8q37hxvhx9xmmsi2dhxaj2kc74nyc"))

(define rust-procfs-core-0.17.0
  (crate-source "procfs-core" "0.17.0"
                "1v0jdbyc1rq1x22m0wn7n4iq4h86gdls38wqfg06zc29hcnz1793"))

(define rust-prodash-29.0.1
  (crate-source "prodash" "29.0.1"
                "12xm50jzkqzdqdcidmzy4d6rj9r8x6mf8sidgrh7dfc0r4jcxrwy"))

(define rust-profiling-1.0.16
  (crate-source "profiling" "1.0.16"
                "0kcz2xzg4qx01r5az8cf9ffjasi2srj56sna32igddh0vi7cggdg"))

(define rust-profiling-procmacros-1.0.16
  (crate-source "profiling-procmacros" "1.0.16"
                "0c7y2k4mz5dp2ksj1h4zbxsxq4plmjzccscdaml3h1pizdh2wpx6"))

(define rust-proptest-1.6.0
  (crate-source "proptest" "1.6.0"
                "0l4y4bb8hffv7cys7d59qwqdmvmqjfzz0x9vblc08209clqfkjhl"))

(define rust-proptest-derive-0.5.1
  (crate-source "proptest-derive" "0.5.1"
                "0jay6jwfvrwzz5bqpi4hxx3ax6kax06p0h29vgkxb0vl42nckqaf"))

(define rust-prost-0.12.6
  (crate-source "prost" "0.12.6"
                "0a8z87ir8yqjgl1kxbdj30a7pzsjs9ka85szll6i6xlb31f47cfy"))

(define rust-prost-derive-0.12.6
  (crate-source "prost-derive" "0.12.6"
                "1waaq9d2f114bvvpw957s7vsx268licnfawr20b51ydb43dxrgc1"))

(define rust-psm-0.1.25
  (crate-source "psm" "0.1.25"
                "125y7h40mkwb64j4v2v7s6f69ilk745kg60w1s2cq62cw8im93pm"
                #:snippet '(delete-file "src/arch/wasm32.o")))

(define rust-ptr-meta-0.3.0
  (crate-source "ptr_meta" "0.3.0"
                "147a6z4qz35gipj9k0d2yh4wygmibhaqsna59vs0d5izdpv7d7py"))

(define rust-ptr-meta-derive-0.3.0
  (crate-source "ptr_meta_derive" "0.3.0"
                "1l9jznaz85cchixyp07v6sxcvjadsyq6lmhjbh98sk0v2pdlwhfa"))

(define rust-pubgrub-0.3.0-alpha.1.b70cf70 rust-pubgrub-for-uv)

(define rust-puffin-0.16.0
  (crate-source "puffin" "0.16.0"
                "08ass1hfdcq86y7dywa1jylzq57la95rgpcmd6yx82hs9symlhkn"))

(define rust-puffin-0.19.1
  (crate-source "puffin" "0.19.1"
                "07vlkf4i88475a80fhckayzxr9v4pkc21kwvpjkc2bn00mxsx7gs"))

(define rust-puffin-http-0.13.0
  (crate-source "puffin_http" "0.13.0"
                "14w1ihjlv48mpbh114yvgixdqdnzzipnmsg158l3v49m1ihgrgqk"))

(define rust-pulldown-cmark-0.9.6
  (crate-source "pulldown-cmark" "0.9.6"
                "0av876a31qvqhy7gzdg134zn4s10smlyi744mz9vrllkf906n82p"))

(define rust-pure-rust-locales-0.8.1
  (crate-source "pure-rust-locales" "0.8.1"
                "0fkkwggiq2053rmiah2h06dz6w3yhy9pa82g30vy3sbcmqcgv40i"))

(define rust-pwd-1.4.0
  (crate-source "pwd" "1.4.0"
                "18p4j95sqqcxn3fbm6gbi7klxp8n40xmcjqy9vz1ww5rg461rivj"))

(define rust-pyo3-0.19.2
  (crate-source "pyo3" "0.19.2"
                "0f1aqx947mzsrk05zdm9w2702lj5k9s97y9w9lxwkp2avk7sd0g6"))

(define rust-pyo3-0.23.5
  (crate-source "pyo3" "0.23.5"
                "0wm8z6jgg18z2cgr99wc34mbkffhcnb50igmq5d1ff6ghpyvyy3p"
                #:snippet '(delete-file-recursively "branding")))

(define rust-pyo3-build-config-0.19.2
  (crate-source "pyo3-build-config" "0.19.2"
                "19bb7aqyvr4kmh8b2lnrmcv9251j8yxw7l7xyr77m3s3pk876v07"))

(define rust-pyo3-build-config-0.23.5
  (crate-source "pyo3-build-config" "0.23.5"
                "1yqhw1k466k65rqvy2d4xz2shl0hzkry1xlxinciigzkdvlcpxll"))

(define rust-pyo3-ffi-0.19.2
  (crate-source "pyo3-ffi" "0.19.2"
                "1nc4696k03ydyiy1f69l3ywknavjzxzag2mscrh2bgkywx1fwg75"))

(define rust-pyo3-ffi-0.23.5
  (crate-source "pyo3-ffi" "0.23.5"
                "13fxvxijl59vilv39akdzwqd1l7fb6c70f53n27irfy0672b9wg9"))

(define rust-pyo3-macros-0.19.2
  (crate-source "pyo3-macros" "0.19.2"
                "1lggr5pnpmdj0cznlhw7ykm1qka3wlymwzfxqql6a4vyb6clrsyz"))

(define rust-pyo3-macros-0.23.5
  (crate-source "pyo3-macros" "0.23.5"
                "1nm9i19aff7zn245v35qb0lbr3cxr19zdgcayq84fg7n509j1hpv"))

(define rust-pyo3-macros-backend-0.19.2
  (crate-source "pyo3-macros-backend" "0.19.2"
                "0dlm4pg29hjmlqx15gcy9cmnabvc8ycy60hcvjg8hm62flhw2zcl"))

(define rust-pyo3-macros-backend-0.23.5
  (crate-source "pyo3-macros-backend" "0.23.5"
                "0a10yxj41kvjhh9vywzd2zj3h6iwm4bg3mlkw2frrnpks1m759pw"))

(define rust-pyproject-toml-0.11.0
  (crate-source "pyproject-toml" "0.11.0"
                "0sm3ncm57hgcyladl55w59ycl39vq4crigjb9bya0n6b7c162w7g"))

(define rust-python-pkginfo-0.6.5
  (crate-source "python-pkginfo" "0.6.5"
                "006n1myk8lx98g9w4d250lmnmahs0ns9lqrgkg992pn41y45h7y2"
                #:snippet '(delete-file-recursively "tests")))

(define rust-qoi-0.4.1
  (crate-source "qoi" "0.4.1"
                "00c0wkb112annn2wl72ixyd78mf56p4lxkhlmsggx65l3v3n8vbz"
                #:snippet '(delete-file-recursively "doc")))

(define rust-qrencode-0.14.0
  (crate-source "qrencode" "0.14.0"
                "1hgp0lchnp3zx79j3799nm445rvqg7x62x2x7926ky22lqhv23d6"
                #:snippet '(for-each delete-file (find-files "src" "^test_annex_i_.*"))))

(define rust-quick-error-1.2.3
  (crate-source "quick-error" "1.2.3"
                "1q6za3v78hsspisc197bg3g7rpc989qycy8ypr8ap8igv10ikl51"))

(define rust-quick-error-2.0.1
  (crate-source "quick-error" "2.0.1"
                "18z6r2rcjvvf8cn92xjhm2qc3jpd1ljvcbf12zv0k9p565gmb4x9"))

(define rust-quick-xml-0.30.0
  (crate-source "quick-xml" "0.30.0"
                "0mp9cqy06blsaka3r1n2p40ddmzhsf7bx37x22r5faw6hq753xpg"))

(define rust-quick-xml-0.31.0
  (crate-source "quick-xml" "0.31.0"
                "0cravqanylzh5cq2v6hzlfqgxcid5nrp2snnb3pf4m0and2a610h"))

(define rust-quick-xml-0.37.4
  (crate-source "quick-xml" "0.37.4"
                "0s8krrf4ci10kcxfzdja7h7dz5kcp1mgndhgf0wghkrjvs48rkm4"))

(define rust-quickcheck-1.0.3
  (crate-source "quickcheck" "1.0.3"
                "1mjhkfqwrb8mdyxdqr4zzbj1rm5dfx25n9zcc25lb6fxwiw673sq"))

(define rust-quinn-0.11.7
  (crate-source "quinn" "0.11.7"
                "04ihd2jibw0carrx081pwdkh8n0l03n9zjvxi21yyylnyak1bgf3"))

(define rust-quinn-proto-0.11.10
  (crate-source "quinn-proto" "0.11.10"
                "1k12m8y3k8dszv9ysb3hxp92cnhva6f670w176img6ywni77885q"))

(define rust-quinn-udp-0.5.11
  (crate-source "quinn-udp" "0.5.11"
                "1daa7c1hws33395zzw62i879dxr168fp8llaff87lx7cqrbhy7al"))

(define rust-quitters-0.1.0
  (crate-source "quitters" "0.1.0"
                "055hdmm9b78jpdirsbfl7bi2zi1zjwiskjxma2r1a8adv3kwv378"))

(define rust-quote-1.0.40
  (crate-source "quote" "1.0.40"
                "1394cxjg6nwld82pzp2d4fp6pmaz32gai1zh9z5hvh0dawww118q"))

(define rust-quoted-printable-0.5.1
  (crate-source "quoted_printable" "0.5.1"
                "0wvwq6w6rdsx1yxzr7ckspff0qk0q9252dzmxrd4c0kv97c9n334"))

(define rust-r-efi-5.2.0
  (crate-source "r-efi" "5.2.0"
                "1ig93jvpqyi87nc5kb6dri49p56q7r7qxrn8kfizmqkfj5nmyxkl"))

(define rust-r2d2-0.8.10
  (crate-source "r2d2" "0.8.10"
                "14qw32y4m564xb1f5ya8ii7dwqyknvk8bsx2r0lljlmn7zxqbpji"))

(define rust-radium-0.7.0
  (crate-source "radium" "0.7.0"
                "02cxfi3ky3c4yhyqx9axqwhyaca804ws46nn4gc1imbk94nzycyw"))

(define rust-radix-trie-0.2.1
  (crate-source "radix_trie" "0.2.1"
                "1zaq3im5ss03w91ij11cj97vvzc5y1f3064d9pi2ysnwziww2sf0"))

(define rust-rancor-0.1.0
  (crate-source "rancor" "0.1.0"
                "0iyr19x1aryadcyc2zwjbwmskkkjqfbvrjp4l37d3f9434bggxfa"))

(define rust-rand-0.7.3
  (crate-source "rand" "0.7.3"
                "00sdaimkbz491qgi6qxkv582yivl32m2jd401kzbn94vsiwicsva"))

(define rust-rand-0.8.5
  (crate-source "rand" "0.8.5"
                "013l6931nn7gkc23jz5mm3qdhf93jjf0fg64nz2lp4i51qd8vbrl"))

(define rust-rand-0.9.0
  (crate-source "rand" "0.9.0"
                "156dyvsfa6fjnv6nx5vzczay1scy5183dvjchd7bvs47xd5bjy9p"))

(define rust-rand-chacha-0.2.2
  (crate-source "rand_chacha" "0.2.2"
                "00il36fkdbsmpr99p9ksmmp6dn1md7rmnwmz0rr77jbrca2yvj7l"))

(define rust-rand-chacha-0.3.1
  (crate-source "rand_chacha" "0.3.1"
                "123x2adin558xbhvqb8w4f6syjsdkmqff8cxwhmjacpsl1ihmhg6"))

(define rust-rand-chacha-0.9.0
  (crate-source "rand_chacha" "0.9.0"
                "1jr5ygix7r60pz0s1cv3ms1f6pd1i9pcdmnxzzhjc3zn3mgjn0nk"))

(define rust-rand-core-0.5.1
  (crate-source "rand_core" "0.5.1"
                "06bdvx08v3rkz451cm7z59xwwqn1rkfh6v9ay77b14f8dwlybgch"))

(define rust-rand-core-0.6.4
  (crate-source "rand_core" "0.6.4"
                "0b4j2v4cb5krak1pv6kakv4sz6xcwbrmy2zckc32hsigbrwy82zc"))

(define rust-rand-core-0.9.3
  (crate-source "rand_core" "0.9.3"
                "0f3xhf16yks5ic6kmgxcpv1ngdhp48mmfy4ag82i1wnwh8ws3ncr"))

(define rust-rand-distr-0.2.2
  (crate-source "rand_distr" "0.2.2"
                "1cpz577qid09lirjjhhn98yqdwsv0c01jf973pxpcr9svp5pm5wn"))

(define rust-rand-distr-0.4.3
  (crate-source "rand_distr" "0.4.3"
                "0cgfwg3z0pkqhrl0x90c77kx70r6g9z4m6fxq9v0h2ibr2dhpjrj"))

(define rust-rand-hc-0.2.0
  (crate-source "rand_hc" "0.2.0"
                "0g31sqwpmsirdlwr0svnacr4dbqyz339im4ssl9738cjgfpjjcfa"))

(define rust-rand-xorshift-0.3.0
  (crate-source "rand_xorshift" "0.3.0"
                "13vcag7gmqspzyabfl1gr9ykvxd2142q2agrj8dkyjmfqmgg4nyj"))

(define rust-rand-xoshiro-0.6.0
  (crate-source "rand_xoshiro" "0.6.0"
                "1ajsic84rzwz5qr0mzlay8vi17swqi684bqvwqyiim3flfrcv5vg"))

(define rust-ratatui-0.29.0
  (crate-source "ratatui" "0.29.0"
                "0yqiccg1wmqqxpb2sz3q2v3nifmhsrfdsjgwhc2w40bqyg199gga"))

(define rust-rav1e-0.7.1
  (crate-source "rav1e" "0.7.1"
                "1sawva6nmj2fvynydbcirr3nb7wjyg0id2hz2771qnv6ly0cx1yd"))

(define rust-ravif-0.11.11
  (crate-source "ravif" "0.11.11"
                "1ij51acd3pkl3rr2ha3r3nc7pvg649m49bvyngpcv98fpnbgs4r4"))

(define rust-raw-window-handle-0.6.2
  (crate-source "raw-window-handle" "0.6.2"
                "0ff5c648hncwx7hm2a8fqgqlbvbl4xawb6v3xxv9wkpjyrr5arr0"))

(define rust-rawpointer-0.2.1
  (crate-source "rawpointer" "0.2.1"
                "1qy1qvj17yh957vhffnq6agq0brvylw27xgks171qrah75wmg8v0"))

(define rust-rayon-1.10.0
  (crate-source "rayon" "1.10.0"
                "1ylgnzwgllajalr4v00y4kj22klq2jbwllm70aha232iah0sc65l"))

(define rust-rayon-core-1.12.1
  (crate-source "rayon-core" "1.12.1"
                "1qpwim68ai5h0j7axa8ai8z0payaawv3id0lrgkqmapx7lx8fr8l"))

(define rust-rctree-0.6.0
  (crate-source "rctree" "0.6.0"
                "1sd6vsa5p3j27v6f1v0l0afl3hn4an1jr3psky3024gcmdk7hgp0"))

(define rust-recvmsg-1.0.0
  (crate-source "recvmsg" "1.0.0"
                "0xa173gbg1cx8q7wyzi6c4kmcsz5rka68r4jb6kg14icskax9vfk"))

(define rust-redox-syscall-0.2.16
  (crate-source "redox_syscall" "0.2.16"
                "16jicm96kjyzm802cxdd1k9jmcph0db1a4lhslcnhjsvhp0mhnpv"))

(define rust-redox-syscall-0.4.1
  (crate-source "redox_syscall" "0.4.1"
                "1aiifyz5dnybfvkk4cdab9p2kmphag1yad6iknc7aszlxxldf8j7"))

(define rust-redox-syscall-0.5.10
  (crate-source "redox_syscall" "0.5.10"
                "1l9b638qx72312yzh8ykvda9b3lqd9gf6yqn66b23a331ck0r30b"))

(define rust-redox-syscall-0.5.11
  (crate-source "redox_syscall" "0.5.11"
                "18qijn18r10haiglv4261wb0yh1agqqlvs0nxfy8yjbpsb307wfj"))

(define rust-redox-users-0.4.6
  (crate-source "redox_users" "0.4.6"
                "0hya2cxx6hxmjfxzv9n8rjl5igpychav7zfi1f81pz6i4krry05s"))

(define rust-redox-users-0.5.0
  (crate-source "redox_users" "0.5.0"
                "0awxx66izdw6kz97r3zxrl5ms5f6dqi5l0f58mlsvlmx8wyrsvyx"))

(define rust-reedline-0.39.0
  (crate-source "reedline" "0.39.0"
                "0jn6vq2x0najy4yvfa1dgd3g1cv4mb8p0i769qv3manjf7p2hiyx"))

(define rust-ref-cast-1.0.24
  (crate-source "ref-cast" "1.0.24"
                "1kx57g118vs9sqi6d2dcxy6vp8jbx8n5hilmv1sacip9vc8y82ja"))

(define rust-ref-cast-impl-1.0.24
  (crate-source "ref-cast-impl" "1.0.24"
                "1ir7dm7hpqqdgg60hlspsc1ck6wli7wa3xcqrsxz7wdz45f24r8i"))

(define rust-reflink-copy-0.1.26
  (crate-source "reflink-copy" "0.1.26"
                "15g89wsra90s4wcp2fvcj1brwfcx04pzklh0rhrl2lic1801vj3q"))

(define rust-regex-1.11.1
  (crate-source "regex" "1.11.1"
                "148i41mzbx8bmq32hsj1q4karkzzx5m60qza6gdw4pdc9qdyyi5m"
                #:snippet '(delete-file-recursively "tests")))

(define rust-regex-automata-0.1.10
  (crate-source "regex-automata" "0.1.10"
                "0ci1hvbzhrfby5fdpf4ganhf7kla58acad9i1ff1p34dzdrhs8vc"))

(define rust-regex-automata-0.4.9
  (crate-source "regex-automata" "0.4.9"
                "02092l8zfh3vkmk47yjc8d631zhhcd49ck2zr133prvd3z38v7l0"
                #:snippet '(delete-file-recursively "tests")))

(define rust-regex-syntax-0.6.29
  (crate-source "regex-syntax" "0.6.29"
                "1qgj49vm6y3zn1hi09x91jvgkl2b1fiaq402skj83280ggfwcqpi"))

(define rust-regex-syntax-0.8.5
  (crate-source "regex-syntax" "0.8.5"
                "0p41p3hj9ww7blnbwbj9h7rwxzxg0c1hvrdycgys8rxyhqqw859b"))

(define rust-relative-path-1.9.3
  (crate-source "relative-path" "1.9.3"
                "1limlh8fzwi21g0473fqzd6fln9iqkwvzp3816bxi31pkilz6fds"))

(define rust-rend-0.5.2
  (crate-source "rend" "0.5.2"
                "05gjxzzsajl61sgif4h0lvagmbry5rm2xak6782j3lccy9mqlpm3"))

(define rust-renderdoc-0.11.0
  (crate-source "renderdoc" "0.11.0"
                "04hycbzwqmzw25qnk0lwps70jgxi43cgmkjdvwbyzc183vnajb97"))

(define rust-renderdoc-sys-1.1.0
  (crate-source "renderdoc-sys" "1.1.0"
                "0cj8zjs7k0gvchcx3jhpg8r9bbqy8b1hsgbz0flcq2ydn12hmcqr"))

(define rust-reqwest-0.11.27
  (crate-source "reqwest" "0.11.27"
                "0qjary4hpplpgdi62d2m0xvbn6lnzckwffm0rgkm2x51023m6ryx"))

(define rust-reqwest-0.12.15
  (crate-source "reqwest" "0.12.15"
                "1fvvrl3jmsnlm99ldl0ariklrlsmrky06qabp7dc92ylznk4d76i"
                #:snippet '(delete-file-recursively "tests")))

(define rust-reqwest-middleware-0.4.1
  (crate-source "reqwest-middleware" "0.4.1"
                "1i64p3wqqgj8yrfr97afn5fy161lkgkk046hm91pm6mx2dargs34"))

(define rust-reqwest-retry-0.7.0
  (crate-source "reqwest-retry" "0.7.0"
                "0y0ihmfs4pailrcdwmbcjx30dfbs82rxk45pfjqvrgx6jm0kxir9"))

(define rust-retry-policies-0.4.0
  (crate-source "retry-policies" "0.4.0"
                "070alim5nhp3bvf58cyc729kq4dmgmr8rjqf2ny72a5bdhg4fxaq"))

(define rust-rfc2047-decoder-1.0.6
  (crate-source "rfc2047-decoder" "1.0.6"
                "0afyg0k2hagnirvv5rk2ysr3khz8ab5ifdap3dsnli9121fm8dmw"
                #:snippet '(delete-file "rustfmt.toml")))

(define rust-rfc6979-0.4.0
  (crate-source "rfc6979" "0.4.0"
                "1chw95jgcfrysyzsq6a10b1j5qb7bagkx8h0wda4lv25in02mpgq"))

(define rust-rgb-0.8.50
  (crate-source "rgb" "0.8.50"
                "02ii3nsciska0sj23ggxaz8gj64ksw8nbpfjcwxlh037chb7sfap"))

(define rust-ring-0.17.14 rust-ring-0.17)

(define rust-rinja-0.3.5
  (crate-source "rinja" "0.3.5"
                "198ppf5bnm6q53dhn4nijl9vbrdm49i1z86msyrk0m2r006r9i1x"))

(define rust-rinja-derive-0.3.5
  (crate-source "rinja_derive" "0.3.5"
                "12x1dfrjxhzfai2izmrqpbplj1aifkq1a58vby1f5xmf8q0yvn88"))

(define rust-rinja-parser-0.3.5
  (crate-source "rinja_parser" "0.3.5"
                "046602hy9x1q3np3qm64xpkw1xx64kiyjikyn8gpl2p0w9kaiyck"
                #:snippet '(delete-file-recursively "tests")))

(define rust-rkyv-0.8.10
  (crate-source "rkyv" "0.8.10"
                "0rbvkcm1ia5rafajf9hlqcw882slm123jj6vzkif2lsmqxqp650y"))

(define rust-rkyv-derive-0.8.10
  (crate-source "rkyv_derive" "0.8.10"
                "0ymv3al6d3qza3lpqdhp7v2lclkdxzl05f14s5swdxls32n40sr4"))

(define rust-rmp-0.8.14
  (crate-source "rmp" "0.8.14"
                "1i1l6dhv7vws5vp0ikakj44fk597xi59g3j6ng1q55x3dz0xg3i2"))

(define rust-rmp-serde-1.3.0
  (crate-source "rmp-serde" "1.3.0"
                "1nylmh7w2vpa1bwrnx1jfp2l4yz6i5qrmpic5zll166gfyj9kraj"))

(define rust-roff-0.2.2
  (crate-source "roff" "0.2.2"
                "1wyqz6m0pm4p6wzhwhahvcidfm7nwb38zl4q7ha940pn3w66dy48"))

(define rust-ron-0.9.0-alpha.1
  (crate-source "ron" "0.9.0-alpha.1"
                "0dh8fd4l54a36881b51275z3hbbjrmrj6rglr28sjzzz76js4i3n"
                #:snippet '(delete-file-recursively "tests")))

(define rust-roxmltree-0.18.1
  (crate-source "roxmltree" "0.18.1"
                "00mkd2xyrxm8ap39sxpkhzdzfn2m98q3zicf6wd2f6yfa7il08w6"))

(define rust-roxmltree-0.20.0
  (crate-source "roxmltree" "0.20.0"
                "15vw91ps91wkmmgy62khf9zb63bdinvm80957dascbsw7dwvc83c"))

(define rust-rpassword-5.0.1
  (crate-source "rpassword" "5.0.1"
                "1yry1kmgjcb4qd5wak91203493x42ak3dz9hy1c0r9kyib7kdjgz"))

(define rust-rpassword-7.3.1
  (crate-source "rpassword" "7.3.1"
                "0gvy3lcpph9vv1rl0cjfn72ylvmgbw2vklmj6w0iv4cpr3ijniw0"))

(define rust-rpds-1.1.0
  (crate-source "rpds" "1.1.0"
                "194hjbsicmgqi3dyllqrz09mmhh597m2j9l49lr16cyfscambqd0"))

(define rust-rsconf-0.2.2
  (crate-source "rsconf" "0.2.2"
                "1p5w6qiskg43f5qhsh6slpc3klmhhyf76xvmzkkh215gy5czhamx"))

(define rust-rstest-0.22.0
  (crate-source "rstest" "0.22.0"
                "0dlrn6y4z5xgsvf6ky3lrjwsxpvi13sizlkwnqs1gmmxc873yhkv"))

(define rust-rstest-0.23.0
  (crate-source "rstest" "0.23.0"
                "0d90hr3i2yajzgpzvsh6p2yjzmcb3nm8884xdbb5sswvwmdmhb0a"))

(define rust-rstest-macros-0.22.0
  (crate-source "rstest_macros" "0.22.0"
                "0hiba8l3d20ajkifd3kz5rzzpxsy311ca4c4ll94pxqlglg73qf5"))

(define rust-rstest-macros-0.23.0
  (crate-source "rstest_macros" "0.23.0"
                "0nmdm7a4ysihnh0zz6w6gqrmw205zfp7xqkb2id3858vg20afpl2"))

(define rust-rstest-reuse-0.7.0
  (crate-source "rstest_reuse" "0.7.0"
                "057y4v1rh9br58n2m3xqvm8xyx8k96jpgibgls3sah78f93gpa5k"))

(define rust-rtoolbox-0.0.2
  (crate-source "rtoolbox" "0.0.2"
                "03n9z8x353kylxhr9im8zawcisnmid3jiqrs8rbdn313cd7d4iy2"))

(define rust-rusqlite-0.31.0
  (crate-source "rusqlite" "0.31.0"
                "1bic69apqidimqf8gm80b98a832qzl9x6ns8myzah4yjg2ifnf5q"))

(define rust-rusqlite-0.32.1
  (crate-source "rusqlite" "0.32.1"
                "0vlx040bppl414pbjgbp7qr4jdxwszi9krx0m63zzf2f2whvflvp"))

(define rust-rusqlite-0.34.0
  (crate-source "rusqlite" "0.34.0"
                "0hy15qj9fzi4xhpslq32ajl6x07x2lzkrqn0qx8y1n48va349qrp"))

(define rust-rust-argon2-2.1.0
  (crate-source "rust-argon2" "2.1.0"
                "1s66kgbvnv5vaq4vlglx587bq93c662whrniz6ycpjb03m9li64x"))

(define rust-rust-decimal-1.37.1
  (crate-source "rust_decimal" "1.37.1"
                "0l3d6p96vqzand26al7r24aawljag37bxff6j2yr3hkallmxx9zs"))

(define rust-rust-embed-8.6.0
  (crate-source "rust-embed" "8.6.0"
                "0vxz02gqj9d65ym7ygbnwwrbgzh88zjqs2b1zk4vabb20i8vlfhb"
                #:snippet '(delete-file-recursively "examples")))

(define rust-rust-embed-8.7.0
  (crate-source "rust-embed" "8.7.0"
                "17f4pribh9nd97szi8zzc2a5xd5myxfjwi5vrvvrmfgwa3pc1yz5"
                #:snippet '(delete-file-recursively "examples")))

(define rust-rust-embed-impl-8.6.0
  (crate-source "rust-embed-impl" "8.6.0"
                "1bms1vmb9z8rcwrjdk1rzmb3wi08mmh7jbz1m1d2r384prrqq68z"))

(define rust-rust-embed-impl-8.7.0
  (crate-source "rust-embed-impl" "8.7.0"
                "0bkh66kzmqv1i478d24nsv4nf89crhs732lblcy6dxp3lb4iix3b"))

(define rust-rust-embed-utils-8.6.0
  (crate-source "rust-embed-utils" "8.6.0"
                "16mxa8asv0aq04fnbz1748rrzl8sc7frmjj2529c538cyk4wsbss"))

(define rust-rust-embed-utils-8.7.0
  (crate-source "rust-embed-utils" "8.7.0"
                "08cfp8x1nw1p272128hfwr9fvnlbg7dmafbbs1ji5q3z2jampm88"))

(define rust-rust-htslib-0.39.5
  (crate-source "rust-htslib" "0.39.5"
                "0lwf22acf6zn9znqd9irc9ny7msya8macvvsdgasqndz9lrzg7i3"))

(define rust-rust-htslib-0.40.2
  (crate-source "rust-htslib" "0.40.2"
                "1m08935ijvxaq6pjmzhkimc6isb3qm7h1sza1012vwk99vzdwwqb"))

(define rust-rust-netrc-0.1.2
  (crate-source "rust-netrc" "0.1.2"
                "1sb3bl0aqisg7q6hxrz29s5c33m6q1qizdazz7dr57vnc9zhk63y"))

(define rust-rustc-demangle-0.1.24
  (crate-source "rustc-demangle" "0.1.24"
                "07zysaafgrkzy2rjgwqdj2a8qdpsm6zv6f5pgpk9x0lm40z9b6vi"))

(define rust-rustc-hash-1.1.0
  (crate-source "rustc-hash" "1.1.0"
                "1qkc5khrmv5pqi5l5ca9p5nl5hs742cagrndhbrlk3dhlrx3zm08"))

(define rust-rustc-hash-2.1.1
  (crate-source "rustc-hash" "2.1.1"
                "03gz5lvd9ghcwsal022cgkq67dmimcgdjghfb5yb5d352ga06xrm"))

(define rust-rustc-stable-hash-0.1.2
  (crate-source "rustc-stable-hash" "0.1.2"
                "026drx2ly2b8b1pp1c2v3p3ws6k0jaa5bbc5f4xwkibhj7r4453q"))

(define rust-rustc-version-0.1.7
  (crate-source "rustc_version" "0.1.7"
                "1160jjsqhqr25cvhr48hmpp8v61bjvjcnxzb0cyf4373lmp3gxf5"))

(define rust-rustc-version-0.4.1
  (crate-source "rustc_version" "0.4.1"
                "14lvdsmr5si5qbqzrajgb6vfn69k0sfygrvfvr2mps26xwi3mjyg"))

(define rust-rustdct-0.7.1
  (crate-source "rustdct" "0.7.1"
                "0lcm1191xx8wizima5j3n25fs90x58v3q1kwg6cbzafn0m8maqcb"))

(define rust-rustfft-6.2.0
  (crate-source "rustfft" "6.2.0"
                "11hx83yr2h2jszkba9qhq2d08q9i5rsashq62rfhqvahpihnb023"))

(define rust-rustfix-0.9.0
  (crate-source "rustfix" "0.9.0"
                "1a79gyag6w459qani0a1m6asadz6vxvgvmrw4l94zzvifiniarkz"))

(define rust-rustix-0.37.28
  (crate-source "rustix" "0.37.28"
                "1dn131z1vj1ani37acirby3rwh6ksm3m1qdv8k554xxrg39nb4ai"
                #:snippet
                '(begin
                   (for-each delete-file (find-files "." "\\.a$"))
                   (delete-file "Cargo.toml")
                   (substitute* "Cargo.toml.orig"
                     ;; Depend unconditionally on the cc crate
                     (("(cc = .*), optional = true.*" _ cc)
                      (string-append cc " }\n"))
                     ;; Disable using the linux_raw backend
                     (("not\\(rustic_use_libc\\)") "miri"))
                   (substitute* "build.rs"
                     ;; Always use the 'feature = "cc"' path
                     (("not\\(feature = \"cc\"\\)") "feature = \"foobar\"")
                     (("#\\[cfg\\(feature = \"cc\"\\)\\]" all)
                      (string-append "//" all)))
                   (copy-file "Cargo.toml.orig" "Cargo.toml"))))

(define rust-rustix-0.38.44
  (crate-source "rustix" "0.38.44"
                "0m61v0h15lf5rrnbjhcb9306bgqrhskrqv7i1n0939dsw8dbrdgx"))

(define rust-rustix-1.0.5
  (crate-source "rustix" "1.0.5"
                "1gsqrw9cp762ps9dl1d13n8mk5r0b6r2s002l1njxfylilwify6r"))

(define rust-rustls-0.23.25
  (crate-source "rustls" "0.23.25"
                "0g5idwxm04i71k3n66ml30zyfbgv6p85a7jky2i09v64i8cfjbl2"))

(define rust-rustls-native-certs-0.8.1
  (crate-source "rustls-native-certs" "0.8.1"
                "1ls7laa3748mkn23fmi3g4mlwk131lx6chq2lyc8v2mmabfz5kvz"))

(define rust-rustls-pemfile-1.0.4
  (crate-source "rustls-pemfile" "1.0.4"
                "1324n5bcns0rnw6vywr5agff3rwfvzphi7rmbyzwnv6glkhclx0w"
                #:snippet '(delete-file-recursively "tests")))

(define rust-rustls-pemfile-2.2.0
  (crate-source "rustls-pemfile" "2.2.0"
                "0l3f3mrfkgdjrava7ibwzgwc4h3dljw3pdkbsi9rkwz3zvji9qyw"
                #:snippet '(delete-file-recursively "tests")))

(define rust-rustls-pki-types-1.11.0
  (crate-source "rustls-pki-types" "1.11.0"
                "0755isc0x5iymm3wsn59s0ad1pm9zidw7p34qfqlsjsac9jf4z4i"
                #:snippet '(delete-file-recursively "tests")))

(define rust-rustls-webpki-0.103.1
  (crate-source "rustls-webpki" "0.103.1"
                "00rcdz0rb9ia2ivrq7412ry9qkvbh78pra2phl4p7kxck9vbiy7y"
                #:snippet '(delete-file-recursively "tests")))

(define rust-rustsec-0.30.2
  (crate-source "rustsec" "0.30.2"
                "1j9fl7wx4zz7rq6i1p4jsvn7z704l3d082bizzg6pwdmpmfmr8vc"))

(define rust-rusttype-0.9.3
  (crate-source "rusttype" "0.9.3"
                "0rx9z8pdg9rxall8nlk9dwizgis1vk9kmdlmg55jad21l153gy1z"))

(define rust-rustversion-1.0.20
  (crate-source "rustversion" "1.0.20"
                "1lhwjb16dsm8brd18bn2bh0ryzc7qi29bi2jjsc6ny2zbwn3ivgd"))

(define rust-rusty-fork-0.3.0
  (crate-source "rusty-fork" "0.3.0"
                "0kxwq5c480gg6q0j3bg4zzyfh2kwmc3v2ba94jw8ncjc8mpcqgfb"))

(define rust-ryu-1.0.20
  (crate-source "ryu" "1.0.20"
                "07s855l8sb333h6bpn24pka5sp7hjk2w667xy6a0khkf6sqv5lr8"))

(define rust-safe-arch-0.7.4
  (crate-source "safe_arch" "0.7.4"
                "08sk47n1kcm5w2di6bpgi2hsw8r2caz2230pwqvbdqfv5pl2vc4n"))

(define rust-salsa20-0.10.2
  (crate-source "salsa20" "0.10.2"
                "04w211x17xzny53f83p8f7cj7k2hi8zck282q5aajwqzydd2z8lp"))

(define rust-same-file-1.0.6
  (crate-source "same-file" "1.0.6"
                "00h5j1w87dmhnvbv9l8bic3y7xxsnjmssvifw2ayvgx9mb1ivz4k"))

(define rust-scan-fmt-0.2.6
  (crate-source "scan_fmt" "0.2.6"
                "0j0jb1dsa8zjpnc875wy72190zlyngvl62mfv8pqwal8vfjv0lqb"))

(define rust-scc-2.3.3
  (crate-source "scc" "2.3.3"
                "1cc5ccsjhs0b31yfcdjw0dbkgm1fd7pg811zk4wam595min1y2ga"))

(define rust-schannel-0.1.27
  (crate-source "schannel" "0.1.27"
                "0gbbhy28v72kd5iina0z2vcdl3vz63mk5idvkzn5r52z6jmfna8z"
                #:snippet '(delete-file-recursively "test")))

(define rust-scheduled-thread-pool-0.2.7
  (crate-source "scheduled-thread-pool" "0.2.7"
                "068s77f9xcpvzl70nsxk8750dzzc6f9pixajhd979815cj0ndg1w"))

(define rust-schemars-0.8.22
  (crate-source "schemars" "0.8.22"
                "05an9nbi18ynyxv1rjmwbg6j08j0496hd64mjggh53mwp3hjmgrz"))

(define rust-schemars-derive-0.8.22
  (crate-source "schemars_derive" "0.8.22"
                "0kakyzrp5801s4i043l4ilv96lzimnlh01pap958h66n99w6bqij"))

(define rust-scoped-threadpool-0.1.9
  (crate-source "scoped_threadpool" "0.1.9"
                "1a26d3lk40s9mrf4imhbik7caahmw2jryhhb6vqv6fplbbgzal8x"))

(define rust-scoped-tls-1.0.1
  (crate-source "scoped-tls" "1.0.1"
                "15524h04mafihcvfpgxd8f4bgc3k95aclz8grjkg9a0rxcvn9kz1"))

(define rust-scopeguard-1.2.0
  (crate-source "scopeguard" "1.2.0"
                "0jcz9sd47zlsgcnm1hdw0664krxwb5gczlif4qngj2aif8vky54l"))

(define rust-scroll-0.11.0
  (crate-source "scroll" "0.11.0"
                "1nhrhpzf95pxbcjjy222blwf8rl3adws6vsqax0yzyxsa6snbi84"))

(define rust-scroll-0.12.0
  (crate-source "scroll" "0.12.0"
                "19mix9vm4k23jkknpgbi0ylmhpf2hnlpzzrfj9wqcj88lj55kf3a"))

(define rust-scroll-derive-0.11.1
  (crate-source "scroll_derive" "0.11.1"
                "1bi5ljnzksvqhic6j7i2a2ap41s78xr0gifkgjxdxlj63pw4kc8x"))

(define rust-scroll-derive-0.12.0
  (crate-source "scroll_derive" "0.12.0"
                "0cmr3hxk318s2ivv37cik2l1r0d8r0qhahnin5lpxbr5w3yw50bz"))

(define rust-scroll-derive-0.12.1
  (crate-source "scroll_derive" "0.12.1"
                "0gb89b1yr8a6jwp4rcm00xqry6ajvmfywsm7bf5f42a686yfm0qp"))

(define rust-scrypt-0.11.0
  (crate-source "scrypt" "0.11.0"
                "07zxfaqpns9jn0mnxm7wj3ksqsinyfpirkav1f7kc2bchs2s65h5"))

(define rust-sd-notify-0.4.5
  (crate-source "sd-notify" "0.4.5"
                "1x1bmz30x2i35j771rqyyan40473aqk0xjrh2dk9xdnqf7gylhxr"))

(define rust-sdd-3.0.8
  (crate-source "sdd" "3.0.8"
                "08gfrs3cz2ncbqz7v1xm5687hggl166pic1frfk7s0f7244hfkjq"))

(define rust-seahash-4.1.0
  (crate-source "seahash" "4.1.0"
                "0sxsb64np6bvnppjz5hg4rqpnkczhsl8w8kf2a5lr1c08xppn40w"
                #:snippet '(delete-file "logo.png")))

(define rust-search-provider-0.6.0
  (crate-source "search-provider" "0.6.0"
                "01jby7xq0dd9rafw5kgskpbxgppn4imzm71w1sdj8iy9ci4ghh9z"))

(define rust-sec1-0.7.3
  (crate-source "sec1" "0.7.3"
                "1p273j8c87pid6a1iyyc7vxbvifrw55wbxgr0dh3l8vnbxb7msfk"
                #:snippet '(delete-file-recursively "tests")))

(define rust-secrecy-0.10.3
  (crate-source "secrecy" "0.10.3"
                "0nmfsf9qm8921v2jliz08bj8zrryqar4gj3d6irqfc3kaj2az4g8"))

(define rust-security-framework-2.11.1
  (crate-source "security-framework" "2.11.1"
                "00ldclwx78dm61v7wkach9lcx76awlrv0fdgjdwch4dmy12j4yw9"))

(define rust-security-framework-3.2.0
  (crate-source "security-framework" "3.2.0"
                "05mkrddi9i18h9p098d0iimqv1xxz0wd8mbgpbvh9jj67x0205r7"))

(define rust-security-framework-sys-2.14.0
  (crate-source "security-framework-sys" "2.14.0"
                "0chwn01qrnvs59i5220bymd38iddy4krbnmfnhf4k451aqfj7ns9"))

(define rust-selectors-0.25.0
  (crate-source "selectors" "0.25.0"
                "01kvl1r7plzlb665r64p11djabhsrd88si2zh7vci3v3ydshbcsf"))

(define rust-self-cell-1.1.0
  (crate-source "self_cell" "1.1.0"
                "1gmxk5bvnnimcif7v1jk8ai2azfvh9djki545nd86vsnphjgrzf2"))

(define rust-self-replace-1.5.0
  (crate-source "self-replace" "1.5.0"
                "1drganasvf5b0x6c9g60jkfhzjc9in3r6cznjfw0lhmbbrdq3v03"))

(define rust-semver-0.1.20
  (crate-source "semver" "0.1.20"
                "1b10m0hxrr947gp41lj9vnmgl5ddwx3d41vnblsg06ppvkz11x6l"))

(define rust-semver-1.0.26
  (crate-source "semver" "1.0.26"
                "1l5q2vb8fjkby657kdyfpvv40x2i2xqq9bg57pxqakfj92fgmrjn"))

(define rust-seq-io-0.3.4
  (crate-source "seq_io" "0.3.4"
                "1pkasxcf25p1cf2w99a4flhjjaicg4rs14w5g8fkrs0fafg5a0qk"))

(define rust-serde-1.0.219
  (crate-source "serde" "1.0.219"
                "1dl6nyxnsi82a197sd752128a4avm6mxnscywas1jq30srp2q3jz"))

(define rust-serde-bencode-0.2.4
  (crate-source "serde_bencode" "0.2.4"
                "0gj17p1w5hyi69fngv55dai4nb4fmdij76gqwyb9if9qfixzq3d7"))

(define rust-serde-big-array-0.5.1
  (crate-source "serde-big-array" "0.5.1"
                "0zsb9s9rcca3408kg20c6xpx917c9vbbnap5gvrf0wvdqz17rz0i"))

(define rust-serde-bytes-0.11.17
  (crate-source "serde_bytes" "0.11.17"
                "15kds0mw19lvm8ydd1qbharh5rz96zir06yn2silqbfy3cigsdw4"))

(define rust-serde-cbor-0.11.2
  (crate-source "serde_cbor" "0.11.2"
                "1xf1bq7ixha30914pd5jl3yw9v1x6car7xgrpimvfvs5vszjxvrb"
                #:snippet '(delete-file-recursively "tests")))

(define rust-serde-derive-1.0.219
  (crate-source "serde_derive" "1.0.219"
                "001azhjmj7ya52pmfiw4ppxm16nd44y15j2pf5gkcwrcgz7pc0jv"))

(define rust-serde-derive-internals-0.29.1
  (crate-source "serde_derive_internals" "0.29.1"
                "04g7macx819vbnxhi52cx0nhxi56xlhrybgwybyy7fb9m4h6mlhq"))

(define rust-serde-ignored-0.1.11
  (crate-source "serde_ignored" "0.1.11"
                "0xhdf1qd3c8blczag31b8f0v231nwpq1ywxk52bh0bp9h1yscvan"))

(define rust-serde-json-1.0.140
  (crate-source "serde_json" "1.0.140"
                "0wwkp4vc20r87081ihj3vpyz5qf7wqkqipq17v99nv6wjrp8n1i0"))

(define rust-serde-repr-0.1.20
  (crate-source "serde_repr" "0.1.20"
                "1755gss3f6lwvv23pk7fhnjdkjw7609rcgjlr8vjg6791blf6php"))

(define rust-serde-spanned-0.6.8
  (crate-source "serde_spanned" "0.6.8"
                "1q89g70azwi4ybilz5jb8prfpa575165lmrffd49vmcf76qpqq47"))

(define rust-serde-untagged-0.1.7
  (crate-source "serde-untagged" "0.1.7"
                "0vp6gvl4a40rzsxdn5js5bn5sqac7rqdbp8an55dnrnlswcrr799"))

(define rust-serde-urlencoded-0.7.1
  (crate-source "serde_urlencoded" "0.7.1"
                "1zgklbdaysj3230xivihs30qi5vkhigg323a9m62k8jwf4a1qjfk"))

(define rust-serde-value-0.7.0
  (crate-source "serde-value" "0.7.0"
                "0b18ngk7n4f9zmwsfdkhgsp31192smzyl5z143qmx1qi28sa78gk"))

(define rust-serde-yaml-0.9.34+deprecated
  (crate-source "serde_yaml" "0.9.34+deprecated"
                "0isba1fjyg3l6rxk156k600ilzr8fp7crv82rhal0rxz5qd1m2va"))

(define rust-serial-test-0.5.1
  (crate-source "serial_test" "0.5.1"
                "0pchc7imdi9wv8xxnwkb9lzs6cg06ghs0gaajjb834y8837wpg70"))

(define rust-serial-test-1.0.0
  (crate-source "serial_test" "1.0.0"
                "04864v5ain4nan2k5l32sr3bxpg0sfxxs2iki3xxcq78g9s3132k"))

(define rust-serial-test-2.0.0
  (crate-source "serial_test" "2.0.0"
                "0b9v0csv9wxl1gcjq99plwimxbmhgr6kzbwqyb457qh3d22xsmhf"))

(define rust-serial-test-3.2.0
  (crate-source "serial_test" "3.2.0"
                "1a8zg87gi28952hzj363ykwd8p1ssrakl1gi3f4xdqa4y84q298v"))

(define rust-serial-test-derive-0.5.1
  (crate-source "serial_test_derive" "0.5.1"
                "1m8sd97xr8dn6p9by0xwfqm0rz8cbn1ghs5l1fv1xd6xzvgddb5j"))

(define rust-serial-test-derive-1.0.0
  (crate-source "serial_test_derive" "1.0.0"
                "0scscldvlz3an9v0spcizaqp5wa2y4w15bk4ink8jpgq2pgq76h7"))

(define rust-serial-test-derive-2.0.0
  (crate-source "serial_test_derive" "2.0.0"
                "13zvd5ds76hhjn3z0axc05n15lzpxpz77jcykic8q5knhlbjklci"))

(define rust-serial-test-derive-3.2.0
  (crate-source "serial_test_derive" "3.2.0"
                "1vwyz2k5kiy5jmba0fvp6ph8ia707801bz918n2ff7bm11d2csax"))

(define rust-servo-arc-0.3.0
  (crate-source "servo_arc" "0.3.0"
                "0i0s9786np106yl6w29bfzwnj29rqak912skcdxcf04yjlddfdnh"))

(define rust-sha1-0.10.6
  (crate-source "sha1" "0.10.6"
                "1fnnxlfg08xhkmwf2ahv634as30l1i3xhlhkvxflmasi5nd85gz3"
                #:snippet '(delete-file-recursively "tests")))

(define rust-sha1-smol-1.0.1
  (crate-source "sha1_smol" "1.0.1"
                "0pbh2xjfnzgblws3hims0ib5bphv7r5rfdpizyh51vnzvnribymv"))

(define rust-sha2-0.10.8
  (crate-source "sha2" "0.10.8"
                "1j1x78zk9il95w9iv46dh9wm73r6xrgj32y6lzzw7bxws9dbfgbr"
                #:snippet '(delete-file-recursively "tests")))

(define rust-shadow-rs-0.38.1
  (crate-source "shadow-rs" "0.38.1"
                "12gd7bq9lmpc1qgmr1q3paglwkf0az39jhkc9bvlp7y2k33lrhbf"))

(define rust-sharded-slab-0.1.7
  (crate-source "sharded-slab" "0.1.7"
                "1xipjr4nqsgw34k7a2cgj9zaasl2ds6jwn89886kww93d32a637l"))

(define rust-shared-child-1.0.1
  (crate-source "shared_child" "1.0.1"
                "035679h89ppqcfkjzgz9bb2hdlkw5wjv598l310xz8frmqw97yh9"))

(define rust-shell-escape-0.1.5
  (crate-source "shell-escape" "0.1.5"
                "0kqq83dk0r1fqj4cfzddpxrni2hpz5i1y607g366c4m9iyhngfs5"))

(define rust-shell-words-1.1.0
  (crate-source "shell-words" "1.1.0"
                "1plgwx8r0h5ismbbp6cp03740wmzgzhip85k5hxqrrkaddkql614"))

(define rust-shellexpand-3.1.0
  (crate-source "shellexpand" "3.1.0"
                "0jz1i14ziz8gbyj71212s7dqrw6q96f25i48zkmy66fcjhxzl0ys"))

(define rust-shlex-1.3.0
  (crate-source "shlex" "1.3.0"
                "0r1y6bv26c1scpxvhg2cabimrmwgbp4p3wy6syj9n0c4s3q2znhg"))

(define rust-signal-hook-0.3.17
  (crate-source "signal-hook" "0.3.17"
                "0098nsah04spqf3n8niirmfym4wsdgjl57c78kmzijlq8xymh8c6"))

(define rust-signal-hook-mio-0.2.4
  (crate-source "signal-hook-mio" "0.2.4"
                "1k8pl9aafiadr4czsg8zal9b4jdk6kq5985p90i19jc5sh31mnrl"))

(define rust-signal-hook-registry-1.4.2
  (crate-source "signal-hook-registry" "1.4.2"
                "1cb5akgq8ajnd5spyn587srvs4n26ryq0p78nswffwhv46sf1sd9"))

(define rust-signature-2.2.0
  (crate-source "signature" "2.2.0"
                "1pi9hd5vqfr3q3k49k37z06p7gs5si0in32qia4mmr1dancr6m3p"))

(define rust-simba-0.5.1
  (crate-source "simba" "0.5.1"
                "0p1x1ndajy4j3dr9zbh79cz5k0hbj4p9bagd7cj00gc5aws0d0lf"))

(define rust-simba-0.6.0
  (crate-source "simba" "0.6.0"
                "0px0nncs3ki86pjcldz40mhvraywh7y9jypfcqqdcihs287q9dzh"))

(define rust-simba-0.8.1
  (crate-source "simba" "0.8.1"
                "1bnf7ainywmaz2z67ss1q0bjwccf80c50c50r6hlpay69z4hf586"))

(define rust-simd-adler32-0.3.7
  (crate-source "simd-adler32" "0.3.7"
                "1zkq40c3iajcnr5936gjp9jjh1lpzhy44p3dq3fiw75iwr1w2vfn"))

(define rust-simd-helpers-0.1.0
  (crate-source "simd_helpers" "0.1.0"
                "19idqicn9k4vhd04ifh2ff41wvna79zphdf2c81rlmpc7f3hz2cm"))

(define rust-simdutf8-0.1.5
  (crate-source "simdutf8" "0.1.5"
                "0vmpf7xaa0dnaikib5jlx6y4dxd3hxqz6l830qb079g7wcsgxag3"))

(define rust-similar-2.7.0
  (crate-source "similar" "2.7.0"
                "1aidids7ymfr96s70232s6962v5g9l4zwhkvcjp4c5hlb6b5vfxv"))

(define rust-simplelog-0.12.2
  (crate-source "simplelog" "0.12.2"
                "1h59cp84gwdmbxiljq6qmqq1x3lv9ikc1gb32f5ya7pgzbdpl98n"))

(define rust-siphasher-0.3.11
  (crate-source "siphasher" "0.3.11"
                "03axamhmwsrmh0psdw3gf7c0zc4fyl5yjxfifz9qfka6yhkqid9q"))

(define rust-siphasher-1.0.1
  (crate-source "siphasher" "1.0.1"
                "17f35782ma3fn6sh21c027kjmd227xyrx06ffi8gw4xzv9yry6an"))

(define rust-sized-chunks-0.6.5
  (crate-source "sized-chunks" "0.6.5"
                "07ix5fsdnpf2xsb0k5rbiwlmsicm2237fcx7blirp9p7pljr5mhn"))

(define rust-skeptic-0.13.7
  (crate-source "skeptic" "0.13.7"
                "1a205720pnss0alxvbx0fcn3883cg3fbz5y1047hmjbnaq0kplhn"))

(define rust-slab-0.4.9
  (crate-source "slab" "0.4.9"
                "0rxvsgir0qw5lkycrqgb1cxsvxzjv9bmx73bk5y42svnzfba94lg"))

(define rust-slog-2.7.0
  (crate-source "slog" "2.7.0"
                "01ldk4yarx7x4y4rgsf4kmrcy3wrpcxdd53v2lkk355x9rnh8iw3"))

(define rust-slog-async-2.8.0
  (crate-source "slog-async" "2.8.0"
                "113b17aw7jx7mr68vwfq2yiv6mb4702hz6a0g587jb4ai67h7j3j"))

(define rust-slog-term-2.9.1
  (crate-source "slog-term" "2.9.1"
                "1s0h8qhqnvy5a7m7gmnca2a2d5m5a4sz1hc26xfgxawqp7825q5n"))

(define rust-slotmap-1.0.7
  (crate-source "slotmap" "1.0.7"
                "0amqb2fn9lcy1ri0risblkcp88dl0rnfmynw7lx0nqwza77lmzyv"))

(define rust-smallvec-1.15.0
  (crate-source "smallvec" "1.15.0"
                "1sgfw8z729nlxk8k13dhs0a762wnaxmlx70a7xlf3wz989bjh5w9"))

(define rust-smawk-0.3.2
  (crate-source "smawk" "0.3.2"
                "0344z1la39incggwn6nl45k8cbw2x10mr5j0qz85cdz9np0qihxp"))

(define rust-smithay-0.4.0.0cd3345 rust-smithay-for-niri)

(define rust-smithay-client-toolkit-0.15.4
  (crate-source "smithay-client-toolkit" "0.15.4"
                "18wxla80y6m4l3dwawi7bl1d9m9dfcg4sxxjcgjqq3psjxmg2a4a"))

(define rust-smithay-client-toolkit-0.19.2
  (crate-source "smithay-client-toolkit" "0.19.2"
                "05h05hg4dn3v6br5jbdbs5nalk076a64s7fn6i01nqzby2hxwmrl"))

(define rust-smithay-drm-extras-0.1.0.0cd3345 rust-smithay-for-niri)

(define rust-smol-str-0.2.2
  (crate-source "smol_str" "0.2.2"
                "1bfylqf2vnqaglw58930vpxm2rfzji5gjp15a2c0kh8aj6v8ylyx"))

(define rust-smol-str-0.3.2
  (crate-source "smol_str" "0.3.2"
                "039mj6lc1vkljj17ndlzzkak8kvlmw8ppi6yjdxsh433snfbhxln"))

(define rust-snap-1.1.1
  (crate-source "snap" "1.1.1"
                "0fxw80m831l76a5zxcwmz2aq7mcwc1pp345pnljl4cv1kbxnfsqv"))

(define rust-snapbox-0.6.21
  (crate-source "snapbox" "0.6.21"
                "0ss3nd9ky0fkq7idj7jzr22kvkhxz3ylrq9fmiq5sdg3h52zrp4n"))

(define rust-snapbox-macros-0.3.10
  (crate-source "snapbox-macros" "0.3.10"
                "1bv4lq1kw1vrd9lk7yk79a0z8q8nma2502ifysv1p913r99rymhn"))

(define rust-socket2-0.4.10
  (crate-source "socket2" "0.4.10"
                "03ack54dxhgfifzsj14k7qa3r5c9wqy3v6mqhlim99cc03y1cycz"))

(define rust-socket2-0.5.9
  (crate-source "socket2" "0.5.9"
                "1vzds1wwwi0a51fn10r98j7cx3ir4shvhykpbk7md2h5h1ydapsg"))

(define rust-socks-0.3.4
  (crate-source "socks" "0.3.4"
                "12ymihhib0zybm6n4mrvh39hj1dm0ya8mqnqdly63079kayxphzh"))

(define rust-spdx-0.10.8
  (crate-source "spdx" "0.10.8"
                "14r1bl3gmx7cj91l5r1qr5wildjacmzflw9cahgzrqk7v9b97djq"))

(define rust-spin-0.9.8
  (crate-source "spin" "0.9.8"
                "0rvam5r0p3a6qhc18scqpvpgb3ckzyqxpgdfyjnghh8ja7byi039"))

(define rust-spki-0.7.3
  (crate-source "spki" "0.7.3"
                "17fj8k5fmx4w9mp27l970clrh5qa7r5sjdvbsln987xhb34dc7nr"
                #:snippet '(delete-file-recursively "tests")))

(define rust-stable-deref-trait-1.2.0
  (crate-source "stable_deref_trait" "1.2.0"
                "1lxjr8q2n534b2lhkxd6l6wcddzjvnksi58zv11f9y0jjmr15wd8"))

(define rust-stacker-0.1.20
  (crate-source "stacker" "0.1.20"
                "1ab039lan55s3vrb5vcldfdfzfajk52vyy344q09rc5rzq0r47v0"))

(define rust-static-assertions-1.1.0
  (crate-source "static_assertions" "1.1.0"
                "0gsl6xmw10gvn3zs1rv99laj5ig7ylffnh71f9l34js4nr4r7sx2"))

(define rust-statrs-0.13.0
  (crate-source "statrs" "0.13.0"
                "0r00b60zlsn6srb6m6bzbw3w5cyihcy4w2rfjav64x4viy5bad0y"))

(define rust-statrs-0.15.0
  (crate-source "statrs" "0.15.0"
                "01bggaq9n09ch20r4yq9s2c4y54367nd71asg22nl8bq9s7bpg85"))

(define rust-statrs-0.16.1
  (crate-source "statrs" "0.16.1"
                "08bp7n3rwk41r11ynwl5x7xdc9cv85zw4r7ww117mhfsp8nhcnmk"))

(define rust-stfu8-0.2.7
  (crate-source "stfu8" "0.2.7"
                "0y0rzzphh2mzfhjz0sxymnjn0s4ap21c74f469s9xycky24iw7z5"))

(define rust-strength-reduce-0.2.4
  (crate-source "strength_reduce" "0.2.4"
                "10jdq9dijjdkb20wg1dmwg447rnj37jbq0mwvbadvqi2gys5x2gy"))

(define rust-string-cache-0.8.9
  (crate-source "string_cache" "0.8.9"
                "03z7km2kzlwiv2r2qifq5riv4g8phazwng9wnvs3py3lzainnxxz"))

(define rust-string-cache-codegen-0.5.4
  (crate-source "string_cache_codegen" "0.5.4"
                "181ir4d6y053s1kka2idpjx5g9d9jgll6fy517jhzzpi2n3r44f7"))

(define rust-strip-ansi-escapes-0.2.1
  (crate-source "strip-ansi-escapes" "0.2.1"
                "0980min1s9f5g47rwlq8l9njks952a0jlz0v7yxrm5p7www813ra"))

(define rust-strsim-0.10.0
  (crate-source "strsim" "0.10.0"
                "08s69r4rcrahwnickvi0kq49z524ci50capybln83mg6b473qivk"))

(define rust-strsim-0.11.1
  (crate-source "strsim" "0.11.1"
                "0kzvqlw8hxqb7y598w1s0hxlnmi84sg5vsipp3yg5na5d1rvba3x"))

(define rust-strsim-0.8.0
  (crate-source "strsim" "0.8.0"
                "0sjsm7hrvjdifz661pjxq5w4hf190hx53fra8dfvamacvff139cf"))

(define rust-strum-0.20.0
  (crate-source "strum" "0.20.0"
                "0p5cslmdnz261kiwmm4h7qsmv9bh83r0f9lq6f2z2mxsnl4wa63k"))

(define rust-strum-0.23.0
  (crate-source "strum" "0.23.0"
                "1fvhkg7di4psfw289v2flv19i28rcflq1g1z3n2rl76iqy8lpqfa"))

(define rust-strum-0.24.1
  (crate-source "strum" "0.24.1"
                "0gz6cjhlps5idwasznklxdh2zsas6mxf99vr0n27j876q12n0gh6"))

(define rust-strum-0.25.0
  (crate-source "strum" "0.25.0"
                "09g1q55ms8vax1z0mxlbva3vm8n2r1179kfvbccnkjcidzm58399"))

(define rust-strum-0.26.3
  (crate-source "strum" "0.26.3"
                "01lgl6jvrf4j28v5kmx9bp480ygf1nhvac8b4p7rcj9hxw50zv4g"))

(define rust-strum-macros-0.20.1
  (crate-source "strum_macros" "0.20.1"
                "0j9ikvxlqzr667ghc045qkpwprjgcfmzgagln7maw4jigawcd2zf"))

(define rust-strum-macros-0.23.1
  (crate-source "strum_macros" "0.23.1"
                "0f2sr3nnhbfpg92ralzqka0wb90nz8ks3sfdk5hylp61x5zdrc2v"))

(define rust-strum-macros-0.24.3
  (crate-source "strum_macros" "0.24.3"
                "0naxz2y38kwq5wgirmia64vvf6qhwy8j367rw966n62gsbh5nf0y"))

(define rust-strum-macros-0.25.3
  (crate-source "strum_macros" "0.25.3"
                "184y62g474zqb2f7n16x3ghvlyjbh50viw32p9w9l5lwmjlizp13"))

(define rust-strum-macros-0.26.4
  (crate-source "strum_macros" "0.26.4"
                "1gl1wmq24b8md527cpyd5bw9rkbqldd7k1h38kf5ajd2ln2ywssc"))

(define rust-subtle-2.6.1
  (crate-source "subtle" "2.6.1"
                "14ijxaymghbl1p0wql9cib5zlwiina7kall6w7g89csprkgbvhhk"))

(define rust-supports-color-2.1.0
  (crate-source "supports-color" "2.1.0"
                "12csf7chawxinaapm9rh718nha9hggk6ra86fdaw9hxdagg8qffn"))

(define rust-supports-color-3.0.2
  (crate-source "supports-color" "3.0.2"
                "1mk7r2j6l7zmqk3pg7av0l6viq413lmk1vz4bjnf9lnq5liwfky6"))

(define rust-supports-hyperlinks-2.1.0
  (crate-source "supports-hyperlinks" "2.1.0"
                "0g93nh1db3f9lyd0ry35bqjrxkg6sbysn36x9hgd9m5h5rlk2hpq"))

(define rust-supports-hyperlinks-3.1.0
  (crate-source "supports-hyperlinks" "3.1.0"
                "12r8d8ckdx78rhdsavh08gg4210i3bmcn2prm7k2s5b37knl8kw0"))

(define rust-supports-unicode-2.1.0
  (crate-source "supports-unicode" "2.1.0"
                "0yp703pvpzpmaw9mpncvwf0iqis4xmhs569ii1g20jhqvngc2l7q"))

(define rust-supports-unicode-3.0.0
  (crate-source "supports-unicode" "3.0.0"
                "1qpc344453x3ai4k9iygxnbk6lr2nw5jflj8ns5q3dbcmwq1lh5p"))

(define rust-svg-0.17.0
  (crate-source "svg" "0.17.0"
                "17kp090hniz0axnv75ripfr5d2xhcbnyhiml30yc4ngmyd0gn3kh"))

(define rust-svg-metadata-0.4.4
  (crate-source "svg_metadata" "0.4.4"
                "002j0na1kfz4pgi43hdcz5baygzk6irnjd5lrmbqqfjldwn3sbx4"
                #:snippet '(for-each delete-file-recursively '("fixtures" "tests"))))

(define rust-syn-1.0.109
  (crate-source "syn" "1.0.109"
                "0ds2if4600bd59wsv7jjgfkayfzy3hnazs394kz6zdkmna8l3dkj"))

(define rust-syn-2.0.100
  (crate-source "syn" "2.0.100"
                "18623wdkns03blpv65xsjn8fipl9p9hj98vlrnhin7nqran496mh"))

(define rust-sync-wrapper-0.1.2
  (crate-source "sync_wrapper" "0.1.2"
                "0q01lyj0gr9a93n10nxsn8lwbzq97jqd6b768x17c8f7v7gccir0"))

(define rust-sync-wrapper-1.0.2
  (crate-source "sync_wrapper" "1.0.2"
                "0qvjyasd6w18mjg5xlaq5jgy84jsjfsvmnn12c13gypxbv75dwhb"))

(define rust-synstructure-0.12.6
  (crate-source "synstructure" "0.12.6"
                "03r1lydbf3japnlpc4wka7y90pmz1i0danaj3f9a7b431akdlszk"))

(define rust-synstructure-0.13.1
  (crate-source "synstructure" "0.13.1"
                "0wc9f002ia2zqcbj0q2id5x6n7g1zjqba7qkg2mr0qvvmdk7dby8"))

(define rust-sys-info-0.9.1
  (crate-source "sys-info" "0.9.1"
                "0b759814ng0cj5a1iiqqjgrzfg9vqlpkbp6z3l76mycbp850sfhb"))

(define rust-sys-locale-0.3.2
  (crate-source "sys-locale" "0.3.2"
                "1i16hq9mkwpzqvixjfy1ph4i2q5klgagjg4hibz6k894l2crmawf"))

(define rust-sysinfo-0.21.2
  (crate-source "sysinfo" "0.21.2"
                "16j4wfiihlq261dh2k939br6q9zzaiwh719m7lb8idj16dpxd24g"))

(define rust-sysinfo-0.27.8
  (crate-source "sysinfo" "0.27.8"
                "0cqy39g76298pqfr8jv30j6cxl9bpnd7c2smfxl5s2na1w2yj0m9"))

(define rust-sysinfo-0.33.1
  (crate-source "sysinfo" "0.33.1"
                "00bcbj9rk39n07ylclj9klggkshxyianv2lfkpqnc6x0iqj5ij2g"))

(define rust-system-configuration-0.5.1
  (crate-source "system-configuration" "0.5.1"
                "1rz0r30xn7fiyqay2dvzfy56cvaa3km74hnbz2d72p97bkf3lfms"))

(define rust-system-configuration-0.6.1
  (crate-source "system-configuration" "0.6.1"
                "0sxslml567zm0v8g732314vd2gk9sd3k4xj22xk6p64xir29v1rw"))

(define rust-system-configuration-sys-0.5.0
  (crate-source "system-configuration-sys" "0.5.0"
                "1jckxvdr37bay3i9v52izgy52dg690x5xfg3hd394sv2xf4b2px7"))

(define rust-system-configuration-sys-0.6.0
  (crate-source "system-configuration-sys" "0.6.0"
                "1i5sqrmgy58l4704hibjbl36hclddglh73fb3wx95jnmrq81n7cf"))

(define rust-system-deps-6.2.2
  (crate-source "system-deps" "6.2.2"
                "0j93ryw031n3h8b0nfpj5xwh3ify636xmv8kxianvlyyipmkbrd3"
                #:snippet '(delete-file-recursively "src/tests")))

(define rust-system-deps-7.0.3
  (crate-source "system-deps" "7.0.3"
                "01d0fllzpkfybzadyaq1vlx70imzj56dxs4rk9w2f4ikkypkmlk6"
                #:snippet '(delete-file-recursively "src/tests")))

(define rust-tabled-0.17.0
  (crate-source "tabled" "0.17.0"
                "02ji1rhr6kig7mhh3zlwbflqghadav6mj1g5gi13fccpyci94w66"))

(define rust-take-mut-0.2.2
  (crate-source "take_mut" "0.2.2"
                "0q2d7w6nd5bl7bay5csq065sjg8fw0jcx6hl1983cpzf25fh0r7p"))

(define rust-tame-index-0.18.1
  (crate-source "tame-index" "0.18.1"
                "0365pyq3qp7415z1xql03763krh63779gqdgxwc8l22dq5hrxkpz"))

(define rust-tango-bench-0.6.0
  (crate-source "tango-bench" "0.6.0"
                "0gj2jgfdmwhrdggqh3yp8h33n1jrz6f3drmzg3nny83gihsj4y15"))

(define rust-tap-1.0.1
  (crate-source "tap" "1.0.1"
                "0sc3gl4nldqpvyhqi3bbd0l9k7fngrcl4zs47n314nqqk4bpx4sm"))

(define rust-tar-0.4.44
  (crate-source "tar" "0.4.44"
                "0yk69a8j9xv51mdcy0853jai5zh1pd9yn456q4cpmj0js9w3i1hx"))

(define rust-target-lexicon-0.12.16
  (crate-source "target-lexicon" "0.12.16"
                "1cg3bnx1gdkdr5hac1hzxy64fhw4g7dqkd0n3dxy5lfngpr1mi31"))

(define rust-target-lexicon-0.13.2
  (crate-source "target-lexicon" "0.13.2"
                "16m6smfz533im9dyxfhnzmpi4af75g2iii36ylc4gfmqvf6gf0p5"))

(define rust-temp-dir-0.1.14
  (crate-source "temp-dir" "0.1.14"
                "0wiwpkkxln7ykj44029bmc86cariqq2mjwllrdjzf4jgygpfc7mw"))

(define rust-temp-env-0.3.6
  (crate-source "temp-env" "0.3.6"
                "0l7hpkd0nhiy4w70j9xbygl1vjr9ipcfxii164n40iwg0ralhdwn"))

(define rust-tempfile-3.19.1
  (crate-source "tempfile" "3.19.1"
                "1grmcj8y6rcavndw2dm18ndzdimsq5f8lcrwyg627cdrcdvsqdvl"))

(define rust-tendril-0.4.3
  (crate-source "tendril" "0.4.3"
                "1c3vip59sqwxn148i714nmkrvjzbk7105vj0h92s6r64bw614jnj"))

(define rust-term-0.7.0
  (crate-source "term" "0.7.0"
                "07xzxmg7dbhlirpyfq09v7cfb9gxn0077sqqvszgjvyrjnngi7f5"
                #:snippet '(delete-file-recursively "tests")))

(define rust-termcolor-1.4.1
  (crate-source "termcolor" "1.4.1"
                "0mappjh3fj3p2nmrg4y7qv94rchwi9mzmgmfflr8p2awdj7lyy86"))

(define rust-terminal-size-0.1.17
  (crate-source "terminal_size" "0.1.17"
                "1pq60ng1a7fjp597ifk1cqlz8fv9raz9xihddld1m1pfdia1lg33"))

(define rust-terminal-size-0.2.6
  (crate-source "terminal_size" "0.2.6"
                "0drj7gb77kay5r1cv53ysq3g9g4f8n0jkhld0kadi3lzkvqzcswf"))

(define rust-terminal-size-0.4.2
  (crate-source "terminal_size" "0.4.2"
                "1vdm5xhzn7sqcsr762vmnavkhid3hs8w8qjyh9iwrr1990f4iij5"))

(define rust-terminfo-0.9.0
  (crate-source "terminfo" "0.9.0"
                "0qp6rrzkxcg08vjzsim2bw7mid3vi29mizrg70dzbycj0q7q3snl"
                #:snippet '(delete-file-recursively "tests")))

(define rust-termtree-0.5.1
  (crate-source "termtree" "0.5.1"
                "10s610ax6nb70yi7xfmwcb6d3wi9sj5isd0m63gy2pizr2zgwl4g"))

(define rust-test-case-3.3.1
  (crate-source "test-case" "3.3.1"
                "1a380yzm6787737cw7s09jqmkn9035hghahradl2ikdg2gfm09gb"))

(define rust-test-case-core-3.3.1
  (crate-source "test-case-core" "3.3.1"
                "0krqi0gbi1yyycigyjlak63r8h1n0vms7mg3kckqwlfd87c7zjxd"))

(define rust-test-case-macros-3.3.1
  (crate-source "test-case-macros" "3.3.1"
                "1yvgky3qax73bic6m368q04xc955p4a91mddd6b5fk7d04mfg2aw"))

(define rust-test-log-0.2.17
  (crate-source "test-log" "0.2.17"
                "03ydax0mamyhn5n6x0l8d2kz76fly7jv3xmnwmk12611sa1n1x77"))

(define rust-test-log-macros-0.2.17
  (crate-source "test-log-macros" "0.2.17"
                "0gsmmdsi8aqgxbr16n78sf1m6x5s29gfvlk0n7d0yg5mdly0r3c8"))

(define rust-textwrap-0.11.0
  (crate-source "textwrap" "0.11.0"
                "0q5hky03ik3y50s9sz25r438bc4nwhqc6dqwynv4wylc807n29nk"))

(define rust-textwrap-0.15.2
  (crate-source "textwrap" "0.15.2"
                "0galmidi6gpn308b1kv3r4qbb48j2926lcj0idwhdhlylhjybcxp"))

(define rust-textwrap-0.16.2
  (crate-source "textwrap" "0.16.2"
                "0mrhd8q0dnh5hwbwhiv89c6i41yzmhw4clwa592rrp24b9hlfdf1"))

(define rust-thiserror-1.0.69
  (crate-source "thiserror" "1.0.69"
                "0lizjay08agcr5hs9yfzzj6axs53a2rgx070a1dsi3jpkcrzbamn"))

(define rust-thiserror-2.0.12
  (crate-source "thiserror" "2.0.12"
                "024791nsc0np63g2pq30cjf9acj38z3jwx9apvvi8qsqmqnqlysn"))

(define rust-thiserror-impl-1.0.69
  (crate-source "thiserror-impl" "1.0.69"
                "1h84fmn2nai41cxbhk6pqf46bxqq1b344v8yz089w1chzi76rvjg"))

(define rust-thiserror-impl-2.0.12
  (crate-source "thiserror-impl" "2.0.12"
                "07bsn7shydaidvyyrm7jz29vp78vrxr9cr9044rfmn078lmz8z3z"))

(define rust-thousands-0.2.0
  (crate-source "thousands" "0.2.0"
                "0848gnkn7ah51lrx15z9zmn701ipn6gc4xbk4kfdlfahkypkpxiv"))

(define rust-thread-local-1.1.8
  (crate-source "thread_local" "1.1.8"
                "173i5lyjh011gsimk21np9jn8al18rxsrkjli20a7b8ks2xgk7lb"))

(define rust-tiff-0.9.1
  (crate-source "tiff" "0.9.1"
                "0ghyxlz566dzc3scvgmzys11dhq2ri77kb8sznjakijlxby104xs"
                #:snippet '(delete-file-recursively "tests")))

(define rust-tikv-jemalloc-sys-0.6.0+5.3.0-1-ge13ca993e8ccb9ba9847cc330696e02839f328f7
  (crate-source "tikv-jemalloc-sys"
                "0.6.0+5.3.0-1-ge13ca993e8ccb9ba9847cc330696e02839f328f7"
                "0baf5vjpg9ipa388md4yxim77rdblnk8r95mnp1akbqjcj860g6d"
                #:snippet '(delete-file-recursively "jemalloc")))

(define rust-tikv-jemallocator-0.6.0
  (crate-source "tikv-jemallocator" "0.6.0"
                "0r985npb7d9hrbs3mb0bkfbv0nvzjpgvzsbpyj21bn0qhpqmzv2c"
                #:snippet '(delete-file ".Cargo.toml.swp")))

(define rust-time-0.3.41
  (crate-source "time" "0.3.41"
                "0h0cpiyya8cjlrh00d2r72bmgg4lsdcncs76qpwy0rn2kghijxla"))

(define rust-time-core-0.1.4
  (crate-source "time-core" "0.1.4"
                "0z5h9fknvdvbs2k2s1chpi3ab3jvgkfhdnqwrvixjngm263s7sf9"))

(define rust-time-macros-0.2.22
  (crate-source "time-macros" "0.2.22"
                "0jcaxpw220han2bzbrdlpqhy1s5k9i8ri3lw6n5zv4zcja9p69im"))

(define rust-tinystr-0.7.6
  (crate-source "tinystr" "0.7.6"
                "0bxqaw7z8r2kzngxlzlgvld1r6jbnwyylyvyjbv1q71rvgaga5wi"))

(define rust-tinytemplate-1.2.1
  (crate-source "tinytemplate" "1.2.1"
                "1g5n77cqkdh9hy75zdb01adxn45mkh9y40wdr7l68xpz35gnnkdy"))

(define rust-tinyvec-1.9.0
  (crate-source "tinyvec" "1.9.0"
                "0w9w8qcifns9lzvlbfwa01y0skhr542anwa3rpn28rg82wgndcq9"))

(define rust-tinyvec-macros-0.1.1
  (crate-source "tinyvec_macros" "0.1.1"
                "081gag86208sc3y6sdkshgw3vysm5d34p431dzw0bshz66ncng0z"))

(define rust-titlecase-3.5.0
  (crate-source "titlecase" "3.5.0"
                "0dcl4rg82qrkzh7paivvc519cgf8w84m1svd5n9lawjz89yx1ym1"))

(define rust-tl-0.7.8.6e25b2e
  (origin
    (method git-fetch)
    (uri (git-reference (url "https://github.com/astral-sh/tl.git")
                        (commit "6e25b2ee2513d75385101a8ff9f591ef51f314ec")))
    (file-name (git-file-name "rust-tl" "0.7.8.6e25b2e"))
    (sha256 (base32 "0r6wwvw1apsfzdhzvw2vinjb3nwbyly2ycx09yfqc0wrwiav6khp"))))

(define rust-tokio-1.44.1
  (crate-source "tokio" "1.44.1"
                "06n90q5hh1yd844s6nf4j3fwbrkm2bnq533kp3a488l4bdhxm0pk"))

(define rust-tokio-1.44.2
  (crate-source "tokio" "1.44.2"
                "0j4w3qvlcqzgbxlnap0czvspqj6x461vyk1sbqcf97g4rci8if76"))

(define rust-tokio-macros-2.5.0
  (crate-source "tokio-macros" "2.5.0"
                "1f6az2xbvqp7am417b78d1za8axbvjvxnmkakz9vr8s52czx81kf"))

(define rust-tokio-native-tls-0.3.1
  (crate-source "tokio-native-tls" "0.3.1"
                "1wkfg6zn85zckmv4im7mv20ca6b1vmlib5xwz9p7g19wjfmpdbmv"
                #:snippet '(for-each delete-file-recursively '("examples" "tests"))))

(define rust-tokio-rustls-0.26.2
  (crate-source "tokio-rustls" "0.26.2"
                "16wf007q3584j46wc4s0zc4szj6280g23hka6x6bgs50l4v7nwlf"))

(define rust-tokio-socks-0.5.2
  (crate-source "tokio-socks" "0.5.2"
                "0gq40sgggz21wfpshiq8pryh062vp7m36rrz3c8c2wj60aw70iqd"))

(define rust-tokio-stream-0.1.17
  (crate-source "tokio-stream" "0.1.17"
                "0ix0770hfp4x5rh5bl7vsnr3d4iz4ms43i522xw70xaap9xqv9gc"))

(define rust-tokio-util-0.7.14
  (crate-source "tokio-util" "0.7.14"
                "0d7hm1jrwpzryvni72fy5dg9blqs776wq5w38lwigk3g7swr15bb"
                #:snippet '(delete-file "src/sync/tests/mod.rs")))

(define rust-toml-0.5.11
  (crate-source "toml" "0.5.11"
                "0d2266nx8b3n22c7k24x4428z6di8n83a9n466jm7a2hipfz1xzl"))

(define rust-toml-0.8.20
  (crate-source "toml" "0.8.20"
                "0j012b37iz1mihksr6a928s6dzszxvblzg3l5wxp7azzsv6sb1yd"))

(define rust-toml-datetime-0.6.8
  (crate-source "toml_datetime" "0.6.8"
                "0hgv7v9g35d7y9r2afic58jvlwnf73vgd1mz2k8gihlgrf73bmqd"))

(define rust-toml-edit-0.19.15
  (crate-source "toml_edit" "0.19.15"
                "08bl7rp5g6jwmfpad9s8jpw8wjrciadpnbaswgywpr9hv9qbfnqv"
                #:snippet '(delete-file-recursively "tests")))

(define rust-toml-edit-0.20.7
  (crate-source "toml_edit" "0.20.7"
                "10bdyrl1yj5jxkiqfa2fyx9inlzlm7s8nf1jnysp4k6qwky2gx3h"
                #:snippet '(delete-file-recursively "tests")))

(define rust-toml-edit-0.22.24
  (crate-source "toml_edit" "0.22.24"
                "0x0lgp70x5cl9nla03xqs5vwwwlrwmd0djkdrp3h3lpdymgpkd0p"
                #:snippet '(delete-file-recursively "tests")))

(define rust-toml-span-0.4.1
  (crate-source "toml-span" "0.4.1"
                "1ifdm3lyqssp1zsfixy3pvm2857b1dx2ssgvv5ga5cz7j3s3czvm"))

(define rust-topological-sort-0.2.2
  (crate-source "topological-sort" "0.2.2"
                "0gcxahg24c058izagz642vs0kfb2zja48my3qrd0kkaf2d730s7a"))

(define rust-tower-0.5.2
  (crate-source "tower" "0.5.2"
                "1ybmd59nm4abl9bsvy6rx31m4zvzp5rja2slzpn712y9b68ssffh"))

(define rust-tower-layer-0.3.3
  (crate-source "tower-layer" "0.3.3"
                "03kq92fdzxin51w8iqix06dcfgydyvx7yr6izjq0p626v9n2l70j"))

(define rust-tower-service-0.3.3
  (crate-source "tower-service" "0.3.3"
                "1hzfkvkci33ra94xjx64vv3pp0sq346w06fpkcdwjcid7zhvdycd"))

(define rust-tracing-0.1.41
  (crate-source "tracing" "0.1.41"
                "1l5xrzyjfyayrwhvhldfnwdyligi1mpqm8mzbi2m1d6y6p2hlkkq"))

(define rust-tracing-attributes-0.1.28
  (crate-source "tracing-attributes" "0.1.28"
                "0v92l9cxs42rdm4m5hsa8z7ln1xsiw1zc2iil8c6k7lzq0jf2nir"))

(define rust-tracing-chrome-0.7.2
  (crate-source "tracing-chrome" "0.7.2"
                "0977zy46gpawva2laffigxr2pph8v0xa51kfp6ghlifnsn7762mz"))

(define rust-tracing-core-0.1.33
  (crate-source "tracing-core" "0.1.33"
                "170gc7cxyjx824r9kr17zc9gvzx89ypqfdzq259pr56gg5bwjwp6"))

(define rust-tracing-durations-export-0.3.0
  (crate-source "tracing-durations-export" "0.3.0"
                "03ssam7j7isyis7khpyfmdg6zznpz5mgab6x8din9nz0z1g04biq"
                #:snippet '(delete-file-recursively "examples")))

(define rust-tracing-log-0.2.0
  (crate-source "tracing-log" "0.2.0"
                "1hs77z026k730ij1a9dhahzrl0s073gfa2hm5p0fbl0b80gmz1gf"))

(define rust-tracing-serde-0.2.0
  (crate-source "tracing-serde" "0.2.0"
                "1wbgzi364vzfswfkvy48a3p0z5xmv98sx342r57sil70ggmiljvh"))

(define rust-tracing-subscriber-0.3.19
  (crate-source "tracing-subscriber" "0.3.19"
                "0220rignck8072i89jjsh140vmh14ydwpdwnifyaf3xcnpn9s678"))

(define rust-tracing-test-0.2.5
  (crate-source "tracing-test" "0.2.5"
                "0s0x076wpga7k1a3cl8da76rrgvs45zzq9rl6q75w3gy6qa8jysm"))

(define rust-tracing-test-macro-0.2.5
  (crate-source "tracing-test-macro" "0.2.5"
                "0s3m7a3pycn8r4xyql5gv5b85sdrqp4w24k1aqy26zf80vdrsr84"))

(define rust-tracing-tree-0.4.0
  (crate-source "tracing-tree" "0.4.0"
                "175lqyfp6zq7jbj8m026xdp8p765pzgfdzfxahfggmdhy5wwlngl"))

(define rust-tracy-client-0.17.6
  (crate-source "tracy-client" "0.17.6"
                "0zkwz9aq97znyal3hz9wmxya97pj01ddpv92ha7l39a6fdw2s83k"))

;; TODO: Unbundle tracy, environment variables TRACY_CLIENT_LIB and
;; TRACY_CLIENT_LIB_PATH might be helpful.
(define rust-tracy-client-0.18.0
  (crate-source "tracy-client" "0.18.0"
                "1nrn739vanildbbzfdcsh8y1fzp2p848db49vmpvf0jv600jq2nr"))

(define rust-tracy-client-sys-0.24.3
  (crate-source "tracy-client-sys" "0.24.3"
                "0ps3iwb7q1fzs9pir6b0nqi8n7i67lci4jp6z4xrq8s8lmyz7zv9"))

(define rust-transpose-0.2.3
  (crate-source "transpose" "0.2.3"
                "0zp74v7jrjg4jr654dncdj6hqvacicsywyhc62jawgxwhvnimmhs"))

(define rust-trash-5.2.2
  (crate-source "trash" "5.2.2"
                "0wdq8ax8z47cabs7j9hkghli3pyzaxq8z18diw5dd1bd1imnqx12"))

(define rust-tree-magic-mini-3.1.6
  (crate-source "tree_magic_mini" "3.1.6"
                "0qwx2b0xfr00vdskl951cvh3m040zj5n8vm7ln4k6p143ybyiida"))

(define rust-triomphe-0.1.14
  (crate-source "triomphe" "0.1.14"
                "11fciha522hrz6pkafy3xlq20w405w9dqvy9ln7ba1s8v8k7g3zg"))

(define rust-triple-accel-0.3.4
  (crate-source "triple_accel" "0.3.4"
                "0v795l496crk3h6yff9zh1cjyrh5s9v23fbgccc4dpz25z70jav2"))

(define rust-triple-accel-0.4.0
  (crate-source "triple_accel" "0.4.0"
                "0qqyhl1pdvmfbx9fgw5jc15j42d0j1i7b6pzn42zsbzvbp4qn112"))

(define rust-try-lock-0.2.5
  (crate-source "try-lock" "0.2.5"
                "0jqijrrvm1pyq34zn1jmy2vihd4jcrjlvsh4alkjahhssjnsn8g4"))

(define rust-trycmd-0.15.9
  (crate-source "trycmd" "0.15.9"
                "1r5a5r22j7gi69y0zdbwhb6d2hp8r34plnfncp0alql870lwzdd8"))

(define rust-ttf-parser-0.15.2
  (crate-source "ttf-parser" "0.15.2"
                "1pfqn06vjlr6pvlljjmkmcb2kb8ind09q5f78nvxc3nqp74hcgkv"
                #:snippet '(for-each delete-file-recursively '("examples" "tests"))))

(define rust-twox-hash-2.1.0
  (crate-source "twox-hash" "2.1.0"
                "022rwrv24rl6g32nqv1mywf6vdnkn7vq34fg793vll1hgccpzcg7"))

(define rust-typeid-1.0.3
  (crate-source "typeid" "1.0.3"
                "0727ypay2p6mlw72gz3yxkqayzdmjckw46sxqpaj08v0b0r64zdw"))

(define rust-typenum-1.18.0
  (crate-source "typenum" "1.18.0"
                "0gwgz8n91pv40gabrr1lzji0b0hsmg0817njpy397bq7rvizzk0x"))

(define rust-typetag-0.2.20
  (crate-source "typetag" "0.2.20"
                "0vyyqg2ard4jz8jxdcim0j4j2r1nh4h71y8c4ca8rzkvvm02pwkk"))

(define rust-typetag-impl-0.2.20
  (crate-source "typetag-impl" "0.2.20"
                "0lnrad6qrzgw77p8ra9cm8ba0xbihxmzkgglwi6iphzz144kix9m"))

(define rust-ucd-trie-0.1.7
  (crate-source "ucd-trie" "0.1.7"
                "0wc9p07sqwz320848i52nvyjvpsxkx3kv5bfbmm6s35809fdk5i8"))

(define rust-udev-0.7.0
  (crate-source "udev" "0.7.0"
                "06hr927z0fdn7ay0p817b9x19i5fagmpmvz95yhl4d1pf3bbpgaf"))

(define rust-udev-0.9.3
  (crate-source "udev" "0.9.3"
                "17vy1yc6ipb5m2kc2d4lx2qpj45yr7grsjzm3y2gq0a4xblkfkmg"))

(define rust-uds-windows-1.1.0
  (crate-source "uds_windows" "1.1.0"
                "1fb4y65pw0rsp0gyfyinjazlzxz1f6zv7j4zmb20l5pxwv1ypnl9"))

(define rust-uluru-3.1.0
  (crate-source "uluru" "3.1.0"
                "1njp6vvy1mm8idnsp6ljyxx5znfsk3xkmk9cr2am0vkfwmlj92kw"))

(define rust-umask-2.1.0
  (crate-source "umask" "2.1.0"
                "071xszsd6znk0ik11pxl7mwhf07clsiq3qpzw1ac0dcyak14d6pc"))

(define rust-unarray-0.1.4
  (crate-source "unarray" "0.1.4"
                "154smf048k84prsdgh09nkm2n0w0336v84jd4zikyn6v6jrqbspa"))

(define rust-unicase-2.8.1
  (crate-source "unicase" "2.8.1"
                "0fd5ddbhpva7wrln2iah054ar2pc1drqjcll0f493vj3fv8l9f3m"))

(define rust-unicode-bom-2.0.3
  (crate-source "unicode-bom" "2.0.3"
                "05s2sqyjanqrbds3fxam35f92npp5ci2wz9zg7v690r0448mvv3y"))

(define rust-unicode-ident-1.0.18
  (crate-source "unicode-ident" "1.0.18"
                "04k5r6sijkafzljykdq26mhjpmhdx4jwzvn1lh90g9ax9903jpss"
                #:snippet '(delete-file-recursively "tests")))

(define rust-unicode-linebreak-0.1.5
  (crate-source "unicode-linebreak" "0.1.5"
                "07spj2hh3daajg335m4wdav6nfkl0f6c0q72lc37blr97hych29v"))

(define rust-unicode-normalization-0.1.24
  (crate-source "unicode-normalization" "0.1.24"
                "0mnrk809z3ix1wspcqy97ld5wxdb31f3xz6nsvg5qcv289ycjcsh"))

(define rust-unicode-segmentation-1.12.0
  (crate-source "unicode-segmentation" "1.12.0"
                "14qla2jfx74yyb9ds3d2mpwpa4l4lzb9z57c6d2ba511458z5k7n"))

(define rust-unicode-truncate-1.1.0
  (crate-source "unicode-truncate" "1.1.0"
                "1gr7arjjhrhy8dww7hj8qqlws97xf9d276svr4hs6pxgllklcr5k"))

(define rust-unicode-width-0.1.14
  (crate-source "unicode-width" "0.1.14"
                "1bzn2zv0gp8xxbxbhifw778a7fc93pa6a1kj24jgg9msj07f7mkx"))

(define rust-unicode-width-0.2.0
  (crate-source "unicode-width" "0.2.0"
                "1zd0r5vs52ifxn25rs06gxrgz8cmh4xpra922k0xlmrchib1kj0z"))

(define rust-unicode-xid-0.2.6
  (crate-source "unicode-xid" "0.2.6"
                "0lzqaky89fq0bcrh6jj6bhlz37scfd8c7dsj5dq7y32if56c1hgb"))

(define rust-unindent-0.1.11
  (crate-source "unindent" "0.1.11"
                "171may3v15wzc10z64i8sahdz49d031v7424mjsifa205ml6sxp1"))

(define rust-unindent-0.2.4
  (crate-source "unindent" "0.2.4"
                "1wvfh815i6wm6whpdz1viig7ib14cwfymyr1kn3sxk2kyl3y2r3j"))

(define rust-universal-hash-0.5.1
  (crate-source "universal-hash" "0.5.1"
                "1sh79x677zkncasa95wz05b36134822w6qxmi1ck05fwi33f47gw"))

(define rust-unsafe-libyaml-0.2.11
  (crate-source "unsafe-libyaml" "0.2.11"
                "0qdq69ffl3v5pzx9kzxbghzn0fzn266i1xn70y88maybz9csqfk7"))

(define rust-unscanny-0.1.0
  (crate-source "unscanny" "0.1.0"
                "0ivbipc1rnq15fhzgna41p1h01ncq4shycii72f3x5d7czq2mpz9"))

(define rust-untrusted-0.9.0
  (crate-source "untrusted" "0.9.0"
                "1ha7ib98vkc538x0z60gfn0fc5whqdd85mb87dvisdcaifi6vjwf"
                #:snippet '(delete-file-recursively "mk")))

(define rust-update-informer-1.2.0
  (crate-source "update-informer" "1.2.0"
                "19w2s4wqpjrw70b8v5b6h0lrxk11jpllik7qjh3l7n7hspskp0ak"))

(define rust-ureq-2.12.1
  (crate-source "ureq" "2.12.1"
                "07f0qdn6459k4rmdnkivkz0y7j28vxh5c8q8sr0gcxgdfxiadl82"))

(define rust-urid-0.1.0
  (crate-source "urid" "0.1.0"
                "195672gs136vczn1r4hkjg5vfa7vdzr26bzv6lwhk0z7cvbvaa38"))

(define rust-urid-derive-0.1.1
  (crate-source "urid-derive" "0.1.1"
                "0i1nf0sgq4ai051h17s9msaavl3jfzdmdlsy8455pr88y0pfx7l1"))

(define rust-url-2.5.4
  (crate-source "url" "2.5.4"
                "0q6sgznyy2n4l5lm16zahkisvc9nip9aa5q1pps7656xra3bdy1j"))

(define rust-urlencoding-2.1.3
  (crate-source "urlencoding" "2.1.3"
                "1nj99jp37k47n0hvaz5fvz7z6jd0sb4ppvfy3nphr1zbnyixpy6s"))

(define rust-utf-8-0.7.6
  (crate-source "utf-8" "0.7.6"
                "1a9ns3fvgird0snjkd3wbdhwd3zdpc2h5gpyybrfr6ra5pkqxk09"))

(define rust-utf16-iter-1.0.5
  (crate-source "utf16_iter" "1.0.5"
                "0ik2krdr73hfgsdzw0218fn35fa09dg2hvbi1xp3bmdfrp9js8y8"))

(define rust-utf8-iter-1.0.4
  (crate-source "utf8_iter" "1.0.4"
                "1gmna9flnj8dbyd8ba17zigrp9c4c3zclngf5lnb5yvz1ri41hdn"))

(define rust-utf8-width-0.1.7
  (crate-source "utf8-width" "0.1.7"
                "1qwj8c0fg8cpn8hq7c9xzz26kdz6ci32bf0madz57a2xi578vgc6"))

(define rust-utf8parse-0.2.2
  (crate-source "utf8parse" "0.2.2"
                "088807qwjq46azicqwbhlmzwrbkz7l4hpw43sdkdyyk524vdxaq6"))

(define rust-uu-cp-0.0.29
  (crate-source "uu_cp" "0.0.29"
                "1a2s7z41fr5lv74fys8m7j8cwpc4n6sjr6chvg5rkxygxzw3jm13"))

(define rust-uu-mkdir-0.0.29
  (crate-source "uu_mkdir" "0.0.29"
                "0lk96l3lh0lajc0dwfnnbzq8f7xnxdizxq92nlbfm9w81mi302pa"))

(define rust-uu-mktemp-0.0.29
  (crate-source "uu_mktemp" "0.0.29"
                "1k3k0i3shdkpvs1vfw52xx6f60rvvr39bvz6l3ldyk06zx47dnz1"))

(define rust-uu-mv-0.0.29
  (crate-source "uu_mv" "0.0.29"
                "0wb0fxzb7zdc7wl60hlfyh629yvwh8cl74kn06maslvimhbqrawj"))

(define rust-uu-touch-0.0.29
  (crate-source "uu_touch" "0.0.29"
                "0rsbh2mixpnh4ybz9g2zrn67p3qb5szz55gbzv92q9v1g567gp1s"))

(define rust-uu-uname-0.0.29
  (crate-source "uu_uname" "0.0.29"
                "1f5gzlh5msmgrip0fn1njg8wxkfxz400xcbjv7sd80zcd9i45acm"))

(define rust-uu-whoami-0.0.29
  (crate-source "uu_whoami" "0.0.29"
                "066hqqcq0r4cnamksqgcqcd2qqrcvx461dhkxlamqdy972lgf4m8"))

(define rust-uucore-0.0.29
  (crate-source "uucore" "0.0.29"
                "0pivc0vzsai7hdk7905csi5qgjppjblxk96f4j7s0nfrk0axrq2h"))

(define rust-uucore-procs-0.0.29
  (crate-source "uucore_procs" "0.0.29"
                "0pqwka50344s00f39dla2dnwaihxxcm4l26agi1w0mibmcrxxlr7"))

(define rust-uuhelp-parser-0.0.29
  (crate-source "uuhelp_parser" "0.0.29"
                "1g2cj9lhr1cmrfmdw73i70ps8shs180ahv0djiwxrddz3arwix0c"))

(define rust-uuid-1.16.0
  (crate-source "uuid" "1.16.0"
                "1a9dkv6jm4lz7ip9l9i1mcx7sh389xjsr03l6jgwqjpmkdvpm3s5"))

(define rust-v-frame-0.3.8
  (crate-source "v_frame" "0.3.8"
                "0az9nd6qi1gyikh9yb3lhm453kf7d5isd6xai3j13kds4jm2mwyn"))

(define rust-v-htmlescape-0.15.8
  (crate-source "v_htmlescape" "0.15.8"
                "135inp4x7cc32k0hzrymlz1baf0rj0ah5h82nrpa9w0hqpxmg0jf"))

(define rust-valuable-0.1.1
  (crate-source "valuable" "0.1.1"
                "0r9srp55v7g27s5bg7a2m095fzckrcdca5maih6dy9bay6fflwxs"))

(define rust-varisat-0.2.2
  (crate-source "varisat" "0.2.2"
                "1bvwh2bk80a5nci3sd3p205200c6pmbgd5f299krd48y3n2hkrpb"))

(define rust-varisat-checker-0.2.2
  (crate-source "varisat-checker" "0.2.2"
                "0fvndkgd2ypgr9rygnj0glxk0492696qw6xqysc6xv8kb5y9fp0k"))

(define rust-varisat-dimacs-0.2.2
  (crate-source "varisat-dimacs" "0.2.2"
                "0di68140imf7nfhhipkqllrp5m6f1iqsxxrrm70087xy457fw79x"))

(define rust-varisat-formula-0.2.2
  (crate-source "varisat-formula" "0.2.2"
                "08256rfjdmvfxjjw162r6l5ipfd46in9vx1sdmnhgndzp51map1r"))

(define rust-varisat-internal-macros-0.2.2
  (crate-source "varisat-internal-macros" "0.2.2"
                "13a0297kq3qhk6wa59sd44zjlhn0qs358ia8g2m6dl236mvwwbk0"))

(define rust-varisat-internal-proof-0.2.2
  (crate-source "varisat-internal-proof" "0.2.2"
                "01yj4zalzp6x6wa0yr3xl8v1q51xh1vgjr3dnxvz12h1r5xvnqv1"))

(define rust-vcpkg-0.2.15
  (crate-source "vcpkg" "0.2.15"
                "09i4nf5y8lig6xgj3f7fyrvzd3nlaw4znrihw8psidvv5yk4xkdc"
                #:snippet '(delete-file-recursively "test-data")))

(define rust-vec-map-0.8.2
  (crate-source "vec_map" "0.8.2"
                "1481w9g1dw9rxp3l6snkdqihzyrd2f8vispzqmwjwsdyhw8xzggi"))

(define rust-vec-mut-scan-0.3.0
  (crate-source "vec_mut_scan" "0.3.0"
                "1lkz66l8z13lvjll69s23vrca12inpyyh00kwg0djqsyil563vb8"))

(define rust-version-check-0.9.5
  (crate-source "version_check" "0.9.5"
                "0nhhi4i5x89gm911azqbn7avs9mdacw2i3vcz3cnmz3mv4rqz4hb"))

(define rust-version-compare-0.2.0
  (crate-source "version-compare" "0.2.0"
                "12y9262fhjm1wp0aj3mwhads7kv0jz8h168nn5fb8b43nwf9abl5"))

(define rust-version-ranges-0.1.1.b70cf70 rust-pubgrub-for-uv)

(define rust-vswhom-0.1.0
  (crate-source "vswhom" "0.1.0"
                "12v0fjjzxdc3y5c0lcwycfhphz7zf2s06hl5krwhawah0xzrp5xy"
                #:snippet '(delete-file "gh_rsa.enc")))

(define rust-vswhom-sys-0.1.3
  (crate-source "vswhom-sys" "0.1.3"
                "0l0i4fijapsybmfckfqh53yqxsg0bm5ikja6vz8ngw0zpm67w1pv"
                #:snippet '(delete-file "gh_rsa.enc")))

(define rust-vte-0.10.1
  (crate-source "vte" "0.10.1"
                "10srmy9ssircrwsb5lpx3fbhx71460j77kvz0krz38jcmf9fdg3c"
                #:snippet '(delete-file-recursively "tests")))

(define rust-vte-0.14.1
  (crate-source "vte" "0.14.1"
                "0xy01fgkzb2080prh2ncd8949hm2248fc5wf1lryhdrhxzbxq7r3"
                #:snippet '(delete-file-recursively "tests")))

(define rust-vte-generate-state-changes-0.1.2
  (crate-source "vte_generate_state_changes" "0.1.2"
                "0biwgpcji3w4llz7h4bi8c2rwqchm9gmyr7dnjki1m853gp9ndif"))

(define rust-wait-timeout-0.2.1
  (crate-source "wait-timeout" "0.2.1"
                "04azqv9mnfxgvnc8j2wp362xraybakh2dy1nj22gj51rdl93pb09"))

(define rust-waker-fn-1.2.0
  (crate-source "waker-fn" "1.2.0"
                "1dvk0qsv88kiq22x8w0qz0k9nyrxxm5a9a9czdwdvvhcvjh12wii"))

(define rust-walkdir-2.5.0
  (crate-source "walkdir" "2.5.0"
                "0jsy7a710qv8gld5957ybrnc07gavppp963gs32xk4ag8130jy99"
                #:snippet '(for-each delete-file-recursively '("compare" "src/tests"))))

(define rust-want-0.3.1
  (crate-source "want" "0.3.1"
                "03hbfrnvqqdchb5kgxyavb9jabwza0dmh2vw5kg0dq8rxl57d9xz"))

(define rust-wasi-0.11.0+wasi-snapshot-preview1
  (crate-source "wasi" "0.11.0+wasi-snapshot-preview1"
                "08z4hxwkpdpalxjps1ai9y7ihin26y9f476i53dv98v45gkqg3cw"))

(define rust-wasi-0.14.2+wasi-0.2.4
  (crate-source "wasi" "0.14.2+wasi-0.2.4"
                "1cwcqjr3dgdq8j325awgk8a715h0hg0f7jqzsb077n4qm6jzk0wn"))

(define rust-wasi-0.9.0+wasi-snapshot-preview1
  (crate-source "wasi" "0.9.0+wasi-snapshot-preview1"
                "06g5v3vrdapfzvfq662cij7v8a1flwr2my45nnncdv2galrdzkfc"))

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

(define rust-wasm-streams-0.4.2
  (crate-source "wasm-streams" "0.4.2"
                "0rddn007hp6k2cm91mm9y33n79b0jxv0c3znzszcvv67hn6ks18m"))

(define rust-wasm-timer-0.2.5
  (crate-source "wasm-timer" "0.2.5"
                "0zsyijv3wgj9p4q47a4awla8j4kw33jd7da2fsd1wml0nh6wn3my"))

(define rust-wasmparser-0.207.0
  (crate-source "wasmparser" "0.207.0"
                "0b694q3frf4xvavj0rw7xk3j852gqljdp2pghajnsq87mgwbk6z1"))

(define rust-wax-0.6.0
  (crate-source "wax" "0.6.0"
                "0mqk9qxsjlbwnjnj0gkaa29qm3mmgbgrc6pd4qpjvcmsl25af4ld"))

(define rust-wayland-backend-0.3.8
  (crate-source "wayland-backend" "0.3.8"
                "1gs7dw6s3lp9g6g0rhk4bh66wl41jnbkd27c6ynhv1x3xac8j85p"))

(define rust-wayland-client-0.29.5
  (crate-source "wayland-client" "0.29.5"
                "05b7qikqj22rjy17kqw5ar7j2chpy18dr0gqapvwjfd00n60cfrz"))

(define rust-wayland-client-0.31.8
  (crate-source "wayland-client" "0.31.8"
                "0gzpr9gdd8yk1crflxngg5iwa1szyyzp4i4zbgpslf1nsgihs4n2"))

(define rust-wayland-commons-0.29.5
  (crate-source "wayland-commons" "0.29.5"
                "00m90bnxqy0d6lzqlyazc1jh18jgbjwigmyr0rk3m8w4slsg34c6"))

(define rust-wayland-csd-frame-0.3.0
  (crate-source "wayland-csd-frame" "0.3.0"
                "0zjcmcqprfzx57hlm741n89ssp4sha5yh5cnmbk2agflvclm0p32"))

(define rust-wayland-cursor-0.29.5
  (crate-source "wayland-cursor" "0.29.5"
                "0qbn6wqmjibkx3lb3ggbp07iabzgx2zhrm0wxxxjbmhkdyvccrb8"))

(define rust-wayland-cursor-0.31.8
  (crate-source "wayland-cursor" "0.31.8"
                "0pccjqiln1izjbdzprqiijhad4k00wmr5r003a44h1v5nv5jjc59"))

(define rust-wayland-egl-0.32.5
  (crate-source "wayland-egl" "0.32.5"
                "00lr5nlc7xa050p7rzlrndlc82iy0g29lhpxizs73qhh38j3hj2h"))

;; TODO: Use our packaged protocols.
(define rust-wayland-protocols-0.29.5
  (crate-source "wayland-protocols" "0.29.5"
                "1ihbjyd0w460gd7w22g9qabbwd4v8x74f8vsh7p25csljcgn4l5r"))

(define rust-wayland-protocols-0.32.6
  (crate-source "wayland-protocols" "0.32.6"
                "1z0yahh48x8qzdbcallmxn5am5897hkk5d7p51ly6dwvhr3cz087"))

(define rust-wayland-protocols-misc-0.3.6
  (crate-source "wayland-protocols-misc" "0.3.6"
                "0m2spzy9diwc3sx0bqq7qx8qdip5lw1ns227bnqinv8220cfxdzy"))

(define rust-wayland-protocols-plasma-0.3.6
  (crate-source "wayland-protocols-plasma" "0.3.6"
                "1cs6gpgxybjvr60j6j824blsvz4hnmjwaah2cdkzvzh3cz3srjkw"))

(define rust-wayland-protocols-wlr-0.3.6
  (crate-source "wayland-protocols-wlr" "0.3.6"
                "1cpqb0d4ryf87x2wgca5n71wilhvc0jjva0zasbdgalmypk052i4"))

(define rust-wayland-scanner-0.29.5
  (crate-source "wayland-scanner" "0.29.5"
                "0lxx3i2kxnmsk421qx87lqqc9kd2y1ksjxcyg0pqbar2zbc06hwg"))

(define rust-wayland-scanner-0.31.6
  (crate-source "wayland-scanner" "0.31.6"
                "110ldnyfxjqvjssir1jf3ndlci7xy9lpv4aqg775y518bpyxlvw9"))

(define rust-wayland-server-0.31.7
  (crate-source "wayland-server" "0.31.7"
                "1jx410qa59vry55xm40dqgqa7d0cx7xs3a5qaxv8xzwcsrzbvylp"))

(define rust-wayland-sys-0.29.5
  (crate-source "wayland-sys" "0.29.5"
                "1m79qqmr1hx7jlyrvnrxjma5s6dk5js9fjsr4nx7vv1r7hdcw4my"))

(define rust-wayland-sys-0.31.6
  (crate-source "wayland-sys" "0.31.6"
                "05b6i4lg2qrrz7l4h2b5fd7blkkvxq34i1yvlngsmmbpkhwvpknv"))

(define rust-web-sys-0.3.77
  (crate-source "web-sys" "0.3.77"
                "1lnmc1ffbq34qw91nndklqqm75rasaffj2g4f8h1yvqqz4pdvdik"))

(define rust-web-time-1.1.0
  (crate-source "web-time" "1.1.0"
                "1fx05yqx83dhx628wb70fyy10yjfq1jpl20qfqhdkymi13rq0ras"))

(define rust-webpki-roots-0.26.8
  (crate-source "webpki-roots" "0.26.8"
                "1jf54brni9lk4ak5pkma2pn18hli22gr7i7wp9zn2lzayy8v4412"
                #:snippet '(delete-file-recursively "tests")))

(define rust-weezl-0.1.8
  (crate-source "weezl" "0.1.8"
                "10lhndjgs6y5djpg3b420xngcr6jkmv70q8rb1qcicbily35pa2k"))

(define rust-which-4.4.2
  (crate-source "which" "4.4.2"
                "1ixzmx3svsv5hbdvd8vdhd3qwvf6ns8jdpif1wmwsy10k90j9fl7"))

(define rust-which-6.0.3
  (crate-source "which" "6.0.3"
                "07yg74dsq644hq5a35546c9mja6rsjdsg92rykr9hkflxf7r5vml"))

(define rust-which-7.0.2
  (crate-source "which" "7.0.2"
                "10qjdz8cld2ljd62kk3yaqhn7b96dj4bmy02vjmb6wphw5hwhx17"))

(define rust-which-7.0.3
  (crate-source "which" "7.0.3"
                "0qj7lx7v98hcs0rfw4xqw1ssn47v6h7hhak0ai4bbrfk7z747mi4"))

(define rust-wide-0.7.32
  (crate-source "wide" "0.7.32"
                "08mb6iqdscqiqrbfkjrnfr876ah4cc0cx5pjilz3yqw1k9mmgda1"))

(define rust-widestring-1.2.0
  (crate-source "widestring" "1.2.0"
                "0zg04qvpk2xysbbwhdgyilgn4p9igvyj9fhzl7pckam1khvz6z6x"))

(define rust-wild-2.2.1
  (crate-source "wild" "2.2.1"
                "1q8hnhmv3fvgx0j7bv8qig00599a15mfsdhgx3hq2ljpiky1l4x3"))

(define rust-winapi-0.3.9
  (crate-source "winapi" "0.3.9"
                "06gl025x418lchw1wxj64ycr7gha83m44cjr5sarhynd9xkrm0sw"))

(define rust-winapi-build-0.1.1
  (crate-source "winapi-build" "0.1.1"
                "1g4rqsgjky0a7530qajn2bbfcrl2v0zb39idgdws9b1l7gp5wc9d"))

(define rust-winapi-i686-pc-windows-gnu-0.4.0
  (crate-source "winapi-i686-pc-windows-gnu" "0.4.0"
                "1dmpa6mvcvzz16zg6d5vrfy4bxgg541wxrcip7cnshi06v38ffxc"
                #:snippet '(delete-file-recursively "lib")))

(define rust-winapi-util-0.1.9
  (crate-source "winapi-util" "0.1.9"
                "1fqhkcl9scd230cnfj8apfficpf5c9vhwnk4yy9xfc1sw69iq8ng"))

(define rust-winapi-x86-64-pc-windows-gnu-0.4.0
  (crate-source "winapi-x86_64-pc-windows-gnu" "0.4.0"
                "0gqq64czqb64kskjryj8isp62m2sgvx25yyj3kpc2myh85w24bki"
                #:snippet '(delete-file-recursively "lib")))

(define rust-windows-0.56.0
  (crate-source "windows" "0.56.0"
                "0cp10nzrqgrlk91dpwxjcpzyy6imr5vxr5f898pss7nz3gq9vrhx"))

(define rust-windows-0.57.0
  (crate-source "windows" "0.57.0"
                "0hqid10bqvxa3pbpgvrh2cilf950lxsd9zqfv3rldc73v2s2qd0j"))

(define rust-windows-0.58.0
  (crate-source "windows" "0.58.0"
                "1dkjj94b0gn91nn1n22cvm4afsj98f5qrhcl3112v6f4jcfx816x"))

(define rust-windows-0.61.1
  (crate-source "windows" "0.61.1"
                "06d4ahj0lns53cgza2w73r82fqwabyxqp1npp81cnf2p08yqzvn5"))

(define rust-windows-aarch64-gnullvm-0.42.2
  (crate-source "windows_aarch64_gnullvm" "0.42.2"
                "1y4q0qmvl0lvp7syxvfykafvmwal5hrjb4fmv04bqs0bawc52yjr"
                #:snippet '(delete-file-recursively "lib")))

(define rust-windows-aarch64-gnullvm-0.48.5
  (crate-source "windows_aarch64_gnullvm" "0.48.5"
                "1n05v7qblg1ci3i567inc7xrkmywczxrs1z3lj3rkkxw18py6f1b"
                #:snippet '(delete-file-recursively "lib")))

(define rust-windows-aarch64-gnullvm-0.52.6
  (crate-source "windows_aarch64_gnullvm" "0.52.6"
                "1lrcq38cr2arvmz19v32qaggvj8bh1640mdm9c2fr877h0hn591j"
                #:snippet '(delete-file-recursively "lib")))

(define rust-windows-aarch64-gnullvm-0.53.0
  (crate-source "windows_aarch64_gnullvm" "0.53.0"
                "0r77pbpbcf8bq4yfwpz2hpq3vns8m0yacpvs2i5cn6fx1pwxbf46"
                #:snippet '(delete-file-recursively "lib")))

(define rust-windows-aarch64-msvc-0.42.2
  (crate-source "windows_aarch64_msvc" "0.42.2"
                "0hsdikjl5sa1fva5qskpwlxzpc5q9l909fpl1w6yy1hglrj8i3p0"
                #:snippet '(delete-file-recursively "lib")))

(define rust-windows-aarch64-msvc-0.48.5
  (crate-source "windows_aarch64_msvc" "0.48.5"
                "1g5l4ry968p73g6bg6jgyvy9lb8fyhcs54067yzxpcpkf44k2dfw"
                #:snippet '(delete-file-recursively "lib")))

(define rust-windows-aarch64-msvc-0.52.6
  (crate-source "windows_aarch64_msvc" "0.52.6"
                "0sfl0nysnz32yyfh773hpi49b1q700ah6y7sacmjbqjjn5xjmv09"
                #:snippet '(delete-file-recursively "lib")))

(define rust-windows-aarch64-msvc-0.53.0
  (crate-source "windows_aarch64_msvc" "0.53.0"
                "0v766yqw51pzxxwp203yqy39ijgjamp54hhdbsyqq6x1c8gilrf7"
                #:snippet '(delete-file-recursively "lib")))

(define rust-windows-collections-0.2.0
  (crate-source "windows-collections" "0.2.0"
                "1s65anr609qvsjga7w971p6iq964h87670dkfqfypnfgwnswxviv"))

(define rust-windows-core-0.56.0
  (crate-source "windows-core" "0.56.0"
                "19pj57bm0rzhlk0ghrccd3i5zvh0ghm52f8cmdc8d3yhs8pfb626"))

(define rust-windows-core-0.57.0
  (crate-source "windows-core" "0.57.0"
                "0bc3jxw2jw76xkk3ddvnp5b2m76qmbzv1qncgvb6qrlhl8wj9vfj"))

(define rust-windows-core-0.58.0
  (crate-source "windows-core" "0.58.0"
                "16czypy425jzmiys4yb3pwsh7cm6grxn9kjp889iqnf2r17d99kb"))

(define rust-windows-core-0.61.0
  (crate-source "windows-core" "0.61.0"
                "104915nsby2cgp322pqqkmj2r82v5sg4hil0hxddg1hc67gc2qs7"))

(define rust-windows-future-0.2.0
  (crate-source "windows-future" "0.2.0"
                "0cms355fawdyz32r1573lwccyvyscp4ip3l2376hmdmpzjz6n7bs"))

(define rust-windows-i686-gnu-0.42.2
  (crate-source "windows_i686_gnu" "0.42.2"
                "0kx866dfrby88lqs9v1vgmrkk1z6af9lhaghh5maj7d4imyr47f6"
                #:snippet '(delete-file-recursively "lib")))

(define rust-windows-i686-gnu-0.48.5
  (crate-source "windows_i686_gnu" "0.48.5"
                "0gklnglwd9ilqx7ac3cn8hbhkraqisd0n83jxzf9837nvvkiand7"
                #:snippet '(delete-file-recursively "lib")))

(define rust-windows-i686-gnu-0.52.6
  (crate-source "windows_i686_gnu" "0.52.6"
                "02zspglbykh1jh9pi7gn8g1f97jh1rrccni9ivmrfbl0mgamm6wf"
                #:snippet '(delete-file-recursively "lib")))

(define rust-windows-i686-gnu-0.53.0
  (crate-source "windows_i686_gnu" "0.53.0"
                "1hvjc8nv95sx5vdd79fivn8bpm7i517dqyf4yvsqgwrmkmjngp61"
                #:snippet '(delete-file-recursively "lib")))

(define rust-windows-i686-gnullvm-0.52.6
  (crate-source "windows_i686_gnullvm" "0.52.6"
                "0rpdx1537mw6slcpqa0rm3qixmsb79nbhqy5fsm3q2q9ik9m5vhf"
                #:snippet '(delete-file-recursively "lib")))

(define rust-windows-i686-gnullvm-0.53.0
  (crate-source "windows_i686_gnullvm" "0.53.0"
                "04df1in2k91qyf1wzizvh560bvyzq20yf68k8xa66vdzxnywrrlw"
                #:snippet '(delete-file-recursively "lib")))

(define rust-windows-i686-msvc-0.42.2
  (crate-source "windows_i686_msvc" "0.42.2"
                "0q0h9m2aq1pygc199pa5jgc952qhcnf0zn688454i7v4xjv41n24"
                #:snippet '(delete-file-recursively "lib")))

(define rust-windows-i686-msvc-0.48.5
  (crate-source "windows_i686_msvc" "0.48.5"
                "01m4rik437dl9rdf0ndnm2syh10hizvq0dajdkv2fjqcywrw4mcg"
                #:snippet '(delete-file-recursively "lib")))

(define rust-windows-i686-msvc-0.52.6
  (crate-source "windows_i686_msvc" "0.52.6"
                "0rkcqmp4zzmfvrrrx01260q3xkpzi6fzi2x2pgdcdry50ny4h294"
                #:snippet '(delete-file-recursively "lib")))

(define rust-windows-i686-msvc-0.53.0
  (crate-source "windows_i686_msvc" "0.53.0"
                "0pcvb25fkvqnp91z25qr5x61wyya12lx8p7nsa137cbb82ayw7sq"
                #:snippet '(delete-file-recursively "lib")))

(define rust-windows-implement-0.56.0
  (crate-source "windows-implement" "0.56.0"
                "16rgkvlx4syqmajfdwmkcvn6nvh126wjj8sg3jvsk5fdivskbz7n"))

(define rust-windows-implement-0.57.0
  (crate-source "windows-implement" "0.57.0"
                "1mqs7qypclnmx5r8yq5jy3g2d8i27vzag9yzzzxzpdnmb70ds1wi"))

(define rust-windows-implement-0.58.0
  (crate-source "windows-implement" "0.58.0"
                "16spr5z65z21qyv379rv2mb1s5q2i9ibd1p2pkn0dr9qr535pg9b"))

(define rust-windows-implement-0.60.0
  (crate-source "windows-implement" "0.60.0"
                "0dm88k3hlaax85xkls4gf597ar4z8m5vzjjagzk910ph7b8xszx4"))

(define rust-windows-interface-0.56.0
  (crate-source "windows-interface" "0.56.0"
                "1k2prfxna0mw47f8gi8qhw9jfpw66bh2cqzs67sgipjfpx30b688"))

(define rust-windows-interface-0.57.0
  (crate-source "windows-interface" "0.57.0"
                "19zwlzr0q1z9s692681yb5w2lhvwcyx4v95s25hfdkd3isry9gi9"))

(define rust-windows-interface-0.58.0
  (crate-source "windows-interface" "0.58.0"
                "059mxmfvx3x88q74ms0qlxmj2pnidmr5mzn60hakn7f95m34qg05"))

(define rust-windows-interface-0.59.1
  (crate-source "windows-interface" "0.59.1"
                "1a4zr8740gyzzhq02xgl6vx8l669jwfby57xgf0zmkcdkyv134mx"))

(define rust-windows-link-0.1.1
  (crate-source "windows-link" "0.1.1"
                "0f2cq7imbrppsmmnz8899hfhg07cp5gq6rh0bjhb1qb6nwshk13n"))

(define rust-windows-numerics-0.2.0
  (crate-source "windows-numerics" "0.2.0"
                "1cf2j8nbqf0hqqa7chnyid91wxsl2m131kn0vl3mqk3c0rlayl4i"))

(define rust-windows-registry-0.4.0
  (crate-source "windows-registry" "0.4.0"
                "18wbgr6z6765qdnasi8mmvxhvp82xd1zlvd6s7pp2l5lvn8av1j2"))

(define rust-windows-registry-0.5.1
  (crate-source "windows-registry" "0.5.1"
                "0bl461w9qj942sg27c181sxhkzrbw8r779ixvzgm6xnw6vja67dd"))

(define rust-windows-result-0.1.2
  (crate-source "windows-result" "0.1.2"
                "1y274q1v0vy21lhkgslpxpq1m08hvr1mcs2l88h1b1gcx0136f2y"))

(define rust-windows-result-0.2.0
  (crate-source "windows-result" "0.2.0"
                "03mf2z1xcy2slhhsm15z24p76qxgm2m74xdjp8bihyag47c4640x"))

(define rust-windows-result-0.3.2
  (crate-source "windows-result" "0.3.2"
                "0li2f76anf0rg7i966d9qs5iprsg555g9rgyzj7gcpfr9wdd2ky6"))

(define rust-windows-strings-0.1.0
  (crate-source "windows-strings" "0.1.0"
                "042dxvi3133f7dyi2pgcvknwkikk47k8bddwxbq5s0l6qhjv3nac"))

(define rust-windows-strings-0.3.1
  (crate-source "windows-strings" "0.3.1"
                "06bkhkyclbfchcsv5bnhz77r290k20m15glj2xq60ra0bp64iyl7"))

(define rust-windows-strings-0.4.0
  (crate-source "windows-strings" "0.4.0"
                "15rg6a0ha1d231wwps2qlgyqrgkyj1r8v9vsb8nlbvih4ijajavs"))

(define rust-windows-sys-0.45.0
  (crate-source "windows-sys" "0.45.0"
                "1l36bcqm4g89pknfp8r9rl1w4bn017q6a8qlx8viv0xjxzjkna3m"))

(define rust-windows-sys-0.48.0
  (crate-source "windows-sys" "0.48.0"
                "1aan23v5gs7gya1lc46hqn9mdh8yph3fhxmhxlw36pn6pqc28zb7"))

(define rust-windows-sys-0.52.0
  (crate-source "windows-sys" "0.52.0"
                "0gd3v4ji88490zgb6b5mq5zgbvwv7zx1ibn8v3x83rwcdbryaar8"))

(define rust-windows-sys-0.59.0
  (crate-source "windows-sys" "0.59.0"
                "0fw5672ziw8b3zpmnbp9pdv1famk74f1l9fcbc3zsrzdg56vqf0y"))

(define rust-windows-targets-0.42.2
  (crate-source "windows-targets" "0.42.2"
                "0wfhnib2fisxlx8c507dbmh97kgij4r6kcxdi0f9nk6l1k080lcf"))

(define rust-windows-targets-0.48.5
  (crate-source "windows-targets" "0.48.5"
                "034ljxqshifs1lan89xwpcy1hp0lhdh4b5n0d2z4fwjx2piacbws"))

(define rust-windows-targets-0.52.6
  (crate-source "windows-targets" "0.52.6"
                "0wwrx625nwlfp7k93r2rra568gad1mwd888h1jwnl0vfg5r4ywlv"))

(define rust-windows-targets-0.53.0
  (crate-source "windows-targets" "0.53.0"
                "12yakpjizhfpppz1i3zgcwxlbar8axrp9j87fmywpydarvlcgr5i"))

(define rust-windows-x86-64-gnu-0.42.2
  (crate-source "windows_x86_64_gnu" "0.42.2"
                "0dnbf2xnp3xrvy8v9mgs3var4zq9v9yh9kv79035rdgyp2w15scd"
                #:snippet '(delete-file-recursively "lib")))

(define rust-windows-x86-64-gnu-0.48.5
  (crate-source "windows_x86_64_gnu" "0.48.5"
                "13kiqqcvz2vnyxzydjh73hwgigsdr2z1xpzx313kxll34nyhmm2k"
                #:snippet '(delete-file-recursively "lib")))

(define rust-windows-x86-64-gnu-0.52.6
  (crate-source "windows_x86_64_gnu" "0.52.6"
                "0y0sifqcb56a56mvn7xjgs8g43p33mfqkd8wj1yhrgxzma05qyhl"
                #:snippet '(delete-file-recursively "lib")))

(define rust-windows-x86-64-gnu-0.53.0
  (crate-source "windows_x86_64_gnu" "0.53.0"
                "1flh84xkssn1n6m1riddipydcksp2pdl45vdf70jygx3ksnbam9f"
                #:snippet '(delete-file-recursively "lib")))

(define rust-windows-x86-64-gnullvm-0.42.2
  (crate-source "windows_x86_64_gnullvm" "0.42.2"
                "18wl9r8qbsl475j39zvawlidp1bsbinliwfymr43fibdld31pm16"
                #:snippet '(delete-file-recursively "lib")))

(define rust-windows-x86-64-gnullvm-0.48.5
  (crate-source "windows_x86_64_gnullvm" "0.48.5"
                "1k24810wfbgz8k48c2yknqjmiigmql6kk3knmddkv8k8g1v54yqb"
                #:snippet '(delete-file-recursively "lib")))

(define rust-windows-x86-64-gnullvm-0.52.6
  (crate-source "windows_x86_64_gnullvm" "0.52.6"
                "03gda7zjx1qh8k9nnlgb7m3w3s1xkysg55hkd1wjch8pqhyv5m94"
                #:snippet '(delete-file-recursively "lib")))

(define rust-windows-x86-64-gnullvm-0.53.0
  (crate-source "windows_x86_64_gnullvm" "0.53.0"
                "0mvc8119xpbi3q2m6mrjcdzl6afx4wffacp13v76g4jrs1fh6vha"
                #:snippet '(delete-file-recursively "lib")))

(define rust-windows-x86-64-msvc-0.42.2
  (crate-source "windows_x86_64_msvc" "0.42.2"
                "1w5r0q0yzx827d10dpjza2ww0j8iajqhmb54s735hhaj66imvv4s"
                #:snippet '(delete-file-recursively "lib")))

(define rust-windows-x86-64-msvc-0.48.5
  (crate-source "windows_x86_64_msvc" "0.48.5"
                "0f4mdp895kkjh9zv8dxvn4pc10xr7839lf5pa9l0193i2pkgr57d"
                #:snippet '(delete-file-recursively "lib")))

(define rust-windows-x86-64-msvc-0.52.6
  (crate-source "windows_x86_64_msvc" "0.52.6"
                "1v7rb5cibyzx8vak29pdrk8nx9hycsjs4w0jgms08qk49jl6v7sq"
                #:snippet '(delete-file-recursively "lib")))

(define rust-windows-x86-64-msvc-0.53.0
  (crate-source "windows_x86_64_msvc" "0.53.0"
                "11h4i28hq0zlnjcaqi2xdxr7ibnpa8djfggch9rki1zzb8qi8517"
                #:snippet '(delete-file-recursively "lib")))

(define rust-winit-0.30.9
  (crate-source "winit" "0.30.9"
                "1h1wmvhfcq2lg6c6715d7pgnv85508zm94ahcfvaiv68337yl2d8"
                #:snippet '(delete-file-recursively "examples")))

(define rust-winnow-0.5.40
  (crate-source "winnow" "0.5.40"
                "0xk8maai7gyxda673mmw3pj1hdizy5fpi7287vaywykkk19sk4zm"))

(define rust-winnow-0.6.26
  (crate-source "winnow" "0.6.26"
                "0a4sjbbrkhbd0ba6dy0011hln1q3ry4iv6srqjjpi8hsmk9fv40y"))

(define rust-winnow-0.7.4
  (crate-source "winnow" "0.7.4"
                "0dmbsz6zfddcgsqzzqxw1h8f7zy19x407g7zl3hyp6vf2m2bb5qf"))

(define rust-winnow-0.7.6
  (crate-source "winnow" "0.7.6"
                "047abhm7qqgc32pf9a2arini5wsrx7p9wsbx3s106jx4pgczrlv3"))

(define rust-winreg-0.50.0
  (crate-source "winreg" "0.50.0"
                "1cddmp929k882mdh6i9f2as848f13qqna6czwsqzkh1pqnr5fkjj"))

(define rust-winreg-0.52.0
  (crate-source "winreg" "0.52.0"
                "19gh9vp7mp1ab84kc3ag48nm9y7xgjhh3xa4vxss1gylk1rsaxx2"))

(define rust-winresource-0.1.20
  (crate-source "winresource" "0.1.20"
                "1xwkhnqdbr8xpkmm05ma9mj7jvicw73yn6xl3h62qy75iv3nfjms"
                #:snippet '(for-each delete-file '("test.ico" "winresource_embed_properties.png"))))

(define rust-winsafe-0.0.19
  (crate-source "winsafe" "0.0.19"
                "0169xy9mjma8dys4m8v4x0xhw2gkbhv2v1wsbvcjl9bhnxxd2dfi"))

(define rust-winsafe-0.0.23
  (crate-source "winsafe" "0.0.23"
                "1f94wsw4w13nkgb00q3iynkj8r40ak1s07207khh3imjiiigr5m0"))

(define rust-wio-0.2.2
  (crate-source "wio" "0.2.2"
                "199p404fp96w1f1c93bf1jrvaqwypxf3hmmldhww4jk4yhr9j4jx"))

(define rust-wiremock-0.6.3
  (crate-source "wiremock" "0.6.3"
                "00g3524s3nz6fbm56c6d7wqxsd1kx1j5mkvvx2cmhyyq9jvq25hh"))

(define rust-wit-bindgen-rt-0.39.0
  (crate-source "wit-bindgen-rt" "0.39.0"
                "1hd65pa5hp0nl664m94bg554h4zlhrzmkjsf6lsgsb7yc4734hkg"
                #:snippet '(for-each delete-file (find-files "." "\\.(a|o)$"))))

(define rust-wl-clipboard-rs-0.9.2
  (crate-source "wl-clipboard-rs" "0.9.2"
                "1sxsaspzix3xiq6wi1l1g55acgi04sv6r7gxz94zar80wv8ghpwf"))

(define rust-wlcs-0.1.0
  (crate-source "wlcs" "0.1.0"
                "17k0nwn3f2z71rncb8glb4x15m5zmcbklnk71hpv739nrq2w769d"))

(define rust-wmidi-3.1.0
  (crate-source "wmidi" "3.1.0"
                "1kxnbs18nmpzm2hfwaaa5h2s77cmk5w53srzxqmrqlkdpdcrjafa"))

(define rust-write16-1.0.0
  (crate-source "write16" "1.0.0"
                "0dnryvrrbrnl7vvf5vb1zkmwldhjkf2n5znliviam7bm4900z2fi"))

(define rust-writeable-0.5.5
  (crate-source "writeable" "0.5.5"
                "0lawr6y0bwqfyayf3z8zmqlhpnzhdx0ahs54isacbhyjwa7g778y"))

(define rust-wyz-0.5.1
  (crate-source "wyz" "0.5.1"
                "1vdrfy7i2bznnzjdl9vvrzljvs4s3qm8bnlgqwln6a941gy61wq5"))

(define rust-x11-dl-2.21.0
  (crate-source "x11-dl" "2.21.0"
                "0vsiq62xpcfm0kn9zjw5c9iycvccxl22jya8wnk18lyxzqj5jwrq"))

(define rust-x11rb-0.13.1
  (crate-source "x11rb" "0.13.1"
                "04jyfm0xmc538v09pzsyr2w801yadsgvyl2p0p76hzzffg5gz4ax"))

(define rust-x11rb-protocol-0.13.1
  (crate-source "x11rb-protocol" "0.13.1"
                "0gfbxf2k7kbk577j3rjhfx7hm70kmwln6da7xyc4l2za0d2pq47c"))

(define rust-xattr-1.5.0
  (crate-source "xattr" "1.5.0"
                "17nq2c23zcjciz8sxwhisqjkv4l7zcylx3yl2915c59cy7rcnr8d"))

(define rust-xcursor-0.3.8
  (crate-source "xcursor" "0.3.8"
                "0qazsl7h8nrbbzx84qrv39w8m2qc27g0mvrszgdls2v6n6k3vwqf"))

(define rust-xdg-home-1.3.0
  (crate-source "xdg-home" "1.3.0"
                "1xm122zz0wjc8p8cmchij0j9nw34hwncb39jc7dc0mgvb2rdl77c"))

(define rust-xkbcommon-0.7.0
  (crate-source "xkbcommon" "0.7.0"
                "07n9shhcls66wjvmk5pzqql46ipfdv7b8hbc384wgv9hk4jpv1hk"))

(define rust-xkbcommon-0.8.0
  (crate-source "xkbcommon" "0.8.0"
                "1j8s1sfwc6bw9phsca65rw3q3b5l2651v1s0pk5yxm6baa9wlrld"))

(define rust-xkbcommon-dl-0.4.2
  (crate-source "xkbcommon-dl" "0.4.2"
                "1iai0r3b5skd9vbr8z5b0qixiz8jblzfm778ddm8ba596a0dwffh"))

(define rust-xkeysym-0.2.1
  (crate-source "xkeysym" "0.2.1"
                "0mksx670cszyd7jln6s7dhkw11hdfv7blwwr3isq98k22ljh1k5r"))

(define rust-xml-rs-0.8.25
  (crate-source "xml-rs" "0.8.25"
                "1i73ajf6scni5bi1a51r19xykgrambdx5fkks0fyg5jqqbml1ff5"))

(define rust-xml5ever-0.17.0
  (crate-source "xml5ever" "0.17.0"
                "0l76v0c228c92sskiflpsy19c0bgc8q7flhlfanm32zrbb8f2d20"))

(define rust-xmlparser-0.13.6
  (crate-source "xmlparser" "0.13.6"
                "1r796g21c70p983ax0j6rmhzmalg4rhx61mvd4farxdhfyvy1zk6"))

(define rust-xshell-0.2.7
  (crate-source "xshell" "0.2.7"
                "0g9pd9bfp0f35rzichic55k7p1mn8mqp607y5rimhiq14g390wly"))

(define rust-xshell-macros-0.2.7
  (crate-source "xshell-macros" "0.2.7"
                "0irm50jxdc92r0kd6yvl5p28jsfzha59brxk7z9w3jcf7z6h1b1j"))

(define rust-xz2-0.1.7
  (crate-source "xz2" "0.1.7"
                "1qk7nzpblizvayyq4xzi4b0zacmmbqr6vb9fc0v1avyp17f4931q"))

(define rust-y4m-0.8.0
  (crate-source "y4m" "0.8.0"
                "0j24y2zf60lpxwd7kyg737hqfyqx16y32s0fjyi6fax6w4hlnnks"
                #:snippet '(delete-file-recursively "scripts")))

(define rust-yaml-rust-0.4.5
  (crate-source "yaml-rust" "0.4.5"
                "118wbqrr4n6wgk5rjjnlrdlahawlxc1bdsx146mwk8f79in97han"))

(define rust-yansi-1.0.1
  (crate-source "yansi" "1.0.1"
                "0jdh55jyv0dpd38ij4qh60zglbw9aa8wafqai6m0wa7xaxk3mrfg"
                #:snippet '(delete-file-recursively ".github")))

(define rust-yansi-term-0.1.2
  (crate-source "yansi-term" "0.1.2"
                "1w8vjlvxba6yvidqdvxddx3crl6z66h39qxj8xi6aqayw2nk0p7y"))

(define rust-yeslogic-fontconfig-sys-5.0.0
  (crate-source "yeslogic-fontconfig-sys" "5.0.0"
                "0yiwnf2gapqaprp3icvv6b1jjh5d356vpis7pybskcd8k4wv5dpz"))

(define rust-yoke-0.7.5
  (crate-source "yoke" "0.7.5"
                "0h3znzrdmll0a7sglzf9ji0p5iqml11wrj1dypaf6ad6kbpnl3hj"))

(define rust-yoke-derive-0.7.5
  (crate-source "yoke-derive" "0.7.5"
                "0m4i4a7gy826bfvnqa9wy6sp90qf0as3wps3wb0smjaamn68g013"))

(define rust-zbar-rust-0.0.24
  (crate-source "zbar-rust" "0.0.24"
                "09j65i3ic19j5mcjvwdzxqqylhx2gg4aaxhscfdblzxk2k6bmxzq"))

(define rust-zbus-3.15.2
  (crate-source "zbus" "3.15.2"
                "1ri5gklhh3kl9gywym95679xs7n3sw2j3ky80jcd8siacc5ifpb7"))

(define rust-zbus-5.5.0
  (crate-source "zbus" "5.5.0"
                "0dmjaih7gi2d0fa37zzylvbmxqn80x4d7haxr5xn86za93v37hsr"))

(define rust-zbus-macros-3.15.2
  (crate-source "zbus_macros" "3.15.2"
                "19g0d7d4b8l8ycw498sz8pwkplv300j31i9hnihq0zl81xxljcbi"))

(define rust-zbus-macros-5.5.0
  (crate-source "zbus_macros" "5.5.0"
                "1h4zf0wh647fvv97bnsr3ah64cgcnz1r8d10c2q3w2hdxc8as9gk"))

(define rust-zbus-names-2.6.1
  (crate-source "zbus_names" "2.6.1"
                "13achs6jbrp4l0jy5m6nn7v89clfgb63qhldkg5ddgjh6y6p6za3"))

(define rust-zbus-names-4.2.0
  (crate-source "zbus_names" "4.2.0"
                "15ybdd6zic7simi9wjg0ywrxfq4sxg3z0wiyysadps3cpxj8xrkv"))

(define rust-zerocopy-0.7.35
  (crate-source "zerocopy" "0.7.35"
                "1w36q7b9il2flg0qskapgi9ymgg7p985vniqd09vi0mwib8lz6qv"))

(define rust-zerocopy-0.8.24
  (crate-source "zerocopy" "0.8.24"
                "0yb8hyzfnwzr2wg4p7cnqmjps8fsw8xqnprafgpmfs8qisigx1i5"))

(define rust-zerocopy-derive-0.7.35
  (crate-source "zerocopy-derive" "0.7.35"
                "0gnf2ap2y92nwdalzz3x7142f2b83sni66l39vxp2ijd6j080kzs"))

(define rust-zerocopy-derive-0.8.24
  (crate-source "zerocopy-derive" "0.8.24"
                "1gk9047pbq1yjj2jyiv0s37nqc53maqbmhcsjp6lhi2w7kvai5m9"))

(define rust-zerofrom-0.1.6
  (crate-source "zerofrom" "0.1.6"
                "19dyky67zkjichsb7ykhv0aqws3q0jfvzww76l66c19y6gh45k2h"))

(define rust-zerofrom-derive-0.1.6
  (crate-source "zerofrom-derive" "0.1.6"
                "00l5niw7c1b0lf1vhvajpjmcnbdp2vn96jg4nmkhq2db0rp5s7np"))

(define rust-zeroize-1.8.1
  (crate-source "zeroize" "1.8.1"
                "1pjdrmjwmszpxfd7r860jx54cyk94qk59x13sc307cvr5256glyf"))

(define rust-zeroize-derive-1.4.2
  (crate-source "zeroize_derive" "1.4.2"
                "0sczjlqjdmrp3wn62g7mw6p438c9j4jgp2f9zamd56991mdycdnf"))

(define rust-zerovec-0.10.4
  (crate-source "zerovec" "0.10.4"
                "0yghix7n3fjfdppwghknzvx9v8cf826h2qal5nqvy8yzg4yqjaxa"))

(define rust-zerovec-derive-0.10.3
  (crate-source "zerovec-derive" "0.10.3"
                "1ik322dys6wnap5d3gcsn09azmssq466xryn5czfm13mn7gsdbvf"))

(define rust-zip-2.5.0
  (crate-source "zip" "2.5.0"
                "120zjj8rg5fzmvrb1lmznljmkxlcvi7lnmrpdwzy4r2g8qbkih17"))

(define rust-zip-2.6.1
  (crate-source "zip" "2.6.1"
                "0i276d0kracqv27f5r42p3ha2345f77isv5rp54sw9i52p829jqx"))

(define rust-zlib-rs-0.5.0
  (crate-source "zlib-rs" "0.5.0"
                "1a1vssif5m2hwsy574l1gb668q4k04ggqv88yvr9mq29g66r52w6"
                #:snippet '(delete-file-recursively "src/deflate/test-data")))

(define rust-zopfli-0.8.1
  (crate-source "zopfli" "0.8.1"
                "0ip9azz9ldk19m0m1hdppz3n5zcz0cywbg1vx59g4p5c3cwry0g5"))

(define rust-zstd-0.13.3
  (crate-source "zstd" "0.13.3"
                "12n0h4w9l526li7jl972rxpyf012jw3nwmji2qbjghv9ll8y67p9"))

(define rust-zstd-safe-7.2.4
  (crate-source "zstd-safe" "7.2.4"
                "179vxmkzhpz6cq6mfzvgwc99bpgllkr6lwxq7ylh5dmby3aw8jcg"))

(define rust-zstd-sys-2.0.15+zstd.1.5.7
  (crate-source "zstd-sys" "2.0.15+zstd.1.5.7"
                "0dx2l7dyw1p7x7g3p1pfd25ip36hr22hvmgixm6cgl4pvlyii0gb"
                #:snippet '(delete-file-recursively "zstd")))

(define rust-zune-core-0.4.12
  (crate-source "zune-core" "0.4.12"
                "0jj1ra86klzlcj9aha9als9d1dzs7pqv3azs1j3n96822wn3lhiz"))

(define rust-zune-inflate-0.2.54
  (crate-source "zune-inflate" "0.2.54"
                "00kg24jh3zqa3i6rg6yksnb71bch9yi1casqydl00s7nw8pk7avk"))

(define rust-zune-jpeg-0.4.14
  (crate-source "zune-jpeg" "0.4.14"
                "0a70sbnxxkgfm777i1xjkhyn8mx07swg5cabbi083pyysywbm9cr"))

(define rust-zvariant-3.15.2
  (crate-source "zvariant" "3.15.2"
                "1nxj9x187jl32fd32zvq8hfn6lyq3kjadb2q7f6kb6x0igl2pvsf"))

(define rust-zvariant-5.4.0
  (crate-source "zvariant" "5.4.0"
                "1b53qpb3q7j233512s2684iy7wyydra31pi5vkxwygw98kh9xpxj"))

(define rust-zvariant-derive-3.15.2
  (crate-source "zvariant_derive" "3.15.2"
                "1nbydrkawjwxan12vy79qsrn7gwc483mpfzqs685ybyppv04vhip"))

(define rust-zvariant-derive-5.4.0
  (crate-source "zvariant_derive" "5.4.0"
                "0bsbz68dsvkynnnpxpfchmhyl5bsgjjmcbazjg24rf5qhnm0q5vl"))

(define rust-zvariant-utils-1.0.1
  (crate-source "zvariant_utils" "1.0.1"
                "00625h3240rixvfhq6yhws1d4bwf3vrf74v8s69b97aq27cg0d3j"))

(define rust-zvariant-utils-3.2.0
  (crate-source "zvariant_utils" "3.2.0"
                "0d7wllndiv7vwgapmji3q9sxmzbaqfdxjwkqnx9vbmz58gpdyvp1"))

(define ssss-separator 'end-of-crates)


;;;
;;; Cargo inputs.
;;;

;; TODO: Maintain this automatically too.
(define-public mesa-cargo-inputs
  `(("paste" ,rust-paste-1.0.15)
    ("proc-macro2" ,rust-proc-macro2-1.0.94)
    ("quote" ,rust-quote-1.0.40)
    ("syn" ,rust-syn-2.0.100)
    ("unicode-ident" ,rust-unicode-ident-1.0.18)))

(define-cargo-inputs lookup-cargo-inputs
                     (b3sum =>
                            (list rust-anstream-0.6.18
                                  rust-anstyle-1.0.10
                                  rust-anstyle-parse-0.2.6
                                  rust-anstyle-query-1.1.2
                                  rust-anstyle-wincon-3.0.7
                                  rust-anyhow-1.0.97
                                  rust-arrayref-0.3.9
                                  rust-arrayvec-0.7.6
                                  rust-bitflags-2.9.0
                                  rust-blake3-1.8.1
                                  rust-cc-1.2.18
                                  rust-cfg-if-1.0.0
                                  rust-clap-4.5.35
                                  rust-clap-builder-4.5.35
                                  rust-clap-derive-4.5.32
                                  rust-clap-lex-0.7.4
                                  rust-colorchoice-1.0.3
                                  rust-constant-time-eq-0.3.1
                                  rust-crossbeam-deque-0.8.6
                                  rust-crossbeam-epoch-0.9.18
                                  rust-crossbeam-utils-0.8.21
                                  rust-duct-0.13.7
                                  rust-errno-0.3.11
                                  rust-fastrand-2.3.0
                                  rust-getrandom-0.3.2
                                  rust-glob-0.3.2
                                  rust-heck-0.5.0
                                  rust-hex-0.4.3
                                  rust-is-terminal-polyfill-1.70.1
                                  rust-libc-0.2.171
                                  rust-linux-raw-sys-0.9.3
                                  rust-memmap2-0.9.5
                                  rust-once-cell-1.21.3
                                  rust-os-pipe-1.2.1
                                  rust-proc-macro2-1.0.94
                                  rust-quote-1.0.40
                                  rust-r-efi-5.2.0
                                  rust-rayon-core-1.12.1
                                  rust-rustix-1.0.5
                                  rust-shared-child-1.0.1
                                  rust-shlex-1.3.0
                                  rust-strsim-0.11.1
                                  rust-syn-2.0.100
                                  rust-tempfile-3.19.1
                                  rust-terminal-size-0.4.2
                                  rust-unicode-ident-1.0.18
                                  rust-utf8parse-0.2.2
                                  rust-wasi-0.14.2+wasi-0.2.4
                                  rust-wild-2.2.1
                                  rust-windows-sys-0.59.0
                                  rust-windows-targets-0.52.6
                                  rust-windows-aarch64-gnullvm-0.52.6
                                  rust-windows-aarch64-msvc-0.52.6
                                  rust-windows-i686-gnu-0.52.6
                                  rust-windows-i686-gnullvm-0.52.6
                                  rust-windows-i686-msvc-0.52.6
                                  rust-windows-x86-64-gnu-0.52.6
                                  rust-windows-x86-64-gnullvm-0.52.6
                                  rust-windows-x86-64-msvc-0.52.6
                                  rust-wit-bindgen-rt-0.39.0))
                     (bankstown-lv2 =>
                                    (list rust-biquad-0.4.2
                                          rust-libm-0.1.4
                                          rust-lv2-0.6.0
                                          rust-lv2-atom-2.0.0
                                          rust-lv2-core-3.0.0
                                          rust-lv2-core-derive-2.1.1
                                          rust-lv2-midi-1.2.0
                                          rust-lv2-sys-2.0.0
                                          rust-lv2-units-0.1.3
                                          rust-lv2-urid-2.1.0
                                          rust-proc-macro2-1.0.94
                                          rust-quote-1.0.40
                                          rust-syn-1.0.109
                                          rust-unicode-ident-1.0.18
                                          rust-urid-0.1.0
                                          rust-urid-derive-0.1.1
                                          rust-wmidi-3.1.0))
                     (bcachefs-tools =>
                                     (list rust-aho-corasick-1.1.3
                                      rust-anstream-0.6.18
                                      rust-anstyle-1.0.10
                                      rust-anstyle-parse-0.2.6
                                      rust-anstyle-query-1.1.2
                                      rust-anstyle-wincon-3.0.7
                                      rust-anyhow-1.0.97
                                      rust-autocfg-1.4.0
                                      rust-bindgen-0.69.5
                                      rust-bitfield-0.14.0
                                      rust-bitflags-1.3.2
                                      rust-bitflags-2.9.0
                                      rust-byteorder-1.5.0
                                      rust-cc-1.2.18
                                      rust-cexpr-0.6.0
                                      rust-cfg-if-1.0.0
                                      rust-clang-sys-1.8.1
                                      rust-clap-4.5.35
                                      rust-clap-builder-4.5.35
                                      rust-clap-complete-4.5.47
                                      rust-clap-derive-4.5.32
                                      rust-clap-lex-0.7.4
                                      rust-colorchoice-1.0.3
                                      rust-either-1.15.0
                                      rust-env-logger-0.10.2
                                      rust-errno-0.2.8
                                      rust-errno-0.3.11
                                      rust-errno-dragonfly-0.1.2
                                      rust-glob-0.3.2
                                      rust-heck-0.5.0
                                      rust-home-0.5.11
                                      rust-is-terminal-polyfill-1.70.1
                                      rust-itertools-0.12.1
                                      rust-lazy-static-1.5.0
                                      rust-lazycell-1.3.0
                                      rust-libc-0.2.171
                                      rust-libloading-0.8.6
                                      rust-libudev-sys-0.1.4
                                      rust-linux-raw-sys-0.4.15
                                      rust-linux-raw-sys-0.9.3
                                      rust-log-0.4.27
                                      rust-memchr-2.7.4
                                      rust-memoffset-0.8.0
                                      rust-minimal-lexical-0.2.1
                                      rust-nom-7.1.3
                                      rust-once-cell-1.21.3
                                      rust-owo-colors-4.2.0
                                      rust-paste-1.0.15
                                      rust-pkg-config-0.3.32
                                      rust-prettyplease-0.2.32
                                      rust-proc-macro2-1.0.94
                                      rust-quote-1.0.40
                                      rust-regex-1.11.1
                                      rust-regex-automata-0.4.9
                                      rust-regex-syntax-0.8.5
                                      rust-rustc-hash-1.1.0
                                      rust-rustix-0.38.44
                                      rust-rustix-1.0.5
                                      rust-rustversion-1.0.20
                                      rust-shlex-1.3.0
                                      rust-strsim-0.11.1
                                      rust-strum-0.26.3
                                      rust-strum-macros-0.26.4
                                      rust-syn-2.0.100
                                      rust-terminal-size-0.4.2
                                      rust-udev-0.7.0
                                      rust-unicode-ident-1.0.18
                                      rust-utf8parse-0.2.2
                                      rust-uuid-1.16.0
                                      rust-which-4.4.2
                                      rust-winapi-0.3.9
                                      rust-winapi-i686-pc-windows-gnu-0.4.0
                                      rust-winapi-x86-64-pc-windows-gnu-0.4.0
                                      rust-windows-sys-0.59.0
                                      rust-windows-targets-0.52.6
                                      rust-windows-aarch64-gnullvm-0.52.6
                                      rust-windows-aarch64-msvc-0.52.6
                                      rust-windows-i686-gnu-0.52.6
                                      rust-windows-i686-gnullvm-0.52.6
                                      rust-windows-i686-msvc-0.52.6
                                      rust-windows-x86-64-gnu-0.52.6
                                      rust-windows-x86-64-gnullvm-0.52.6
                                      rust-windows-x86-64-msvc-0.52.6
                                      rust-zeroize-1.8.1
                                      rust-zeroize-derive-1.4.2))
                     (c2rust =>
                             (list rust-addr2line-0.24.2
                                   rust-adler2-2.0.0
                                   rust-aho-corasick-1.1.3
                                   rust-anyhow-1.0.97
                                   rust-arc-swap-1.7.1
                                   rust-atty-0.2.14
                                   rust-autocfg-1.4.0
                                   rust-backtrace-0.3.74
                                   rust-bindgen-0.65.1
                                   rust-bitflags-1.3.2
                                   rust-bitflags-2.9.0
                                   rust-block-buffer-0.10.4
                                   rust-c2rust-ast-builder-0.20.0
                                   rust-c2rust-ast-exporter-0.20.0
                                   rust-c2rust-ast-printer-0.20.0
                                   rust-c2rust-bitfields-0.20.0
                                   rust-c2rust-bitfields-derive-0.20.0
                                   rust-c2rust-build-paths-0.20.0
                                   rust-c2rust-transpile-0.20.0
                                   rust-cc-1.2.18
                                   rust-cexpr-0.6.0
                                   rust-cfg-if-1.0.0
                                   rust-clang-sys-1.8.1
                                   rust-clap-3.2.25
                                   rust-clap-derive-3.2.25
                                   rust-clap-lex-0.2.4
                                   rust-cmake-0.1.54
                                   rust-colored-1.9.4
                                   rust-colored-2.2.0
                                   rust-cpufeatures-0.2.17
                                   rust-crypto-common-0.1.6
                                   rust-digest-0.10.7
                                   rust-dtoa-1.0.10
                                   rust-either-1.15.0
                                   rust-env-logger-0.10.2
                                   rust-errno-0.3.11
                                   rust-failure-0.1.8
                                   rust-failure-derive-0.1.8
                                   rust-fern-0.6.2
                                   rust-generic-array-0.14.7
                                   rust-gimli-0.31.1
                                   rust-glob-0.3.2
                                   rust-half-1.8.3
                                   rust-handlebars-4.5.0
                                   rust-hashbrown-0.12.3
                                   rust-heck-0.4.1
                                   rust-hermit-abi-0.1.19
                                   rust-hermit-abi-0.5.0
                                   rust-home-0.5.11
                                   rust-humantime-2.2.0
                                   rust-indexmap-1.9.3
                                   rust-is-terminal-0.4.16
                                   rust-is-executable-1.0.4
                                   rust-itertools-0.10.5
                                   rust-itoa-1.0.15
                                   rust-lazy-static-1.5.0
                                   rust-lazycell-1.3.0
                                   rust-libc-0.2.171
                                   rust-libloading-0.8.6
                                   rust-linked-hash-map-0.5.6
                                   rust-linux-raw-sys-0.4.15
                                   rust-log-0.4.27
                                   rust-log-reroute-0.1.8
                                   rust-memchr-2.7.4
                                   rust-minimal-lexical-0.2.1
                                   rust-miniz-oxide-0.8.7
                                   rust-nom-7.1.3
                                   rust-object-0.36.7
                                   rust-once-cell-1.21.3
                                   rust-os-str-bytes-6.6.1
                                   rust-pathdiff-0.2.3
                                   rust-peeking-take-while-0.1.2
                                   rust-pest-2.8.0
                                   rust-pest-derive-2.8.0
                                   rust-pest-generator-2.8.0
                                   rust-pest-meta-2.8.0
                                   rust-prettyplease-0.1.25
                                   rust-prettyplease-0.2.32
                                   rust-print-bytes-1.2.0
                                   rust-proc-macro-error-1.0.4
                                   rust-proc-macro-error-attr-1.0.4
                                   rust-proc-macro2-1.0.94
                                   rust-quote-1.0.40
                                   rust-regex-1.11.1
                                   rust-regex-automata-0.4.9
                                   rust-regex-syntax-0.8.5
                                   rust-rustc-demangle-0.1.24
                                   rust-rustc-hash-1.1.0
                                   rust-rustix-0.38.44
                                   rust-rustversion-1.0.20
                                   rust-ryu-1.0.20
                                   rust-serde-1.0.219
                                   rust-serde-bencode-0.2.4
                                   rust-serde-bytes-0.11.17
                                   rust-serde-cbor-0.11.2
                                   rust-serde-derive-1.0.219
                                   rust-serde-json-1.0.140
                                   rust-sha2-0.10.8
                                   rust-shlex-1.3.0
                                   rust-smallvec-1.15.0
                                   rust-strsim-0.10.0
                                   rust-strum-0.24.1
                                   rust-strum-macros-0.24.3
                                   rust-syn-1.0.109
                                   rust-syn-2.0.100
                                   rust-synstructure-0.12.6
                                   rust-termcolor-1.4.1
                                   rust-textwrap-0.16.2
                                   rust-thiserror-1.0.69
                                   rust-thiserror-2.0.12
                                   rust-thiserror-impl-1.0.69
                                   rust-thiserror-impl-2.0.12
                                   rust-typenum-1.18.0
                                   rust-ucd-trie-0.1.7
                                   rust-unicode-ident-1.0.18
                                   rust-unicode-xid-0.2.6
                                   rust-version-check-0.9.5
                                   rust-which-4.4.2
                                   rust-winapi-0.3.9
                                   rust-winapi-i686-pc-windows-gnu-0.4.0
                                   rust-winapi-util-0.1.9
                                   rust-winapi-x86-64-pc-windows-gnu-0.4.0
                                   rust-windows-sys-0.48.0
                                   rust-windows-sys-0.59.0
                                   rust-windows-targets-0.48.5
                                   rust-windows-targets-0.52.6
                                   rust-windows-aarch64-gnullvm-0.48.5
                                   rust-windows-aarch64-gnullvm-0.52.6
                                   rust-windows-aarch64-msvc-0.48.5
                                   rust-windows-aarch64-msvc-0.52.6
                                   rust-windows-i686-gnu-0.48.5
                                   rust-windows-i686-gnu-0.52.6
                                   rust-windows-i686-gnullvm-0.52.6
                                   rust-windows-i686-msvc-0.48.5
                                   rust-windows-i686-msvc-0.52.6
                                   rust-windows-x86-64-gnu-0.48.5
                                   rust-windows-x86-64-gnu-0.52.6
                                   rust-windows-x86-64-gnullvm-0.48.5
                                   rust-windows-x86-64-gnullvm-0.52.6
                                   rust-windows-x86-64-msvc-0.48.5
                                   rust-windows-x86-64-msvc-0.52.6
                                   rust-yaml-rust-0.4.5))
                     (cargo-audit =>
                                  (list rust-abscissa-core-0.8.2
                                   rust-abscissa-derive-0.8.2
                                   rust-addr2line-0.21.0
                                   rust-adler-1.0.2
                                   rust-adler2-2.0.0
                                   rust-ahash-0.8.11
                                   rust-aho-corasick-1.1.3
                                   rust-allocator-api2-0.2.21
                                   rust-anstream-0.6.18
                                   rust-anstyle-1.0.10
                                   rust-anstyle-parse-0.2.6
                                   rust-anstyle-query-1.1.2
                                   rust-anstyle-wincon-3.0.7
                                   rust-arc-swap-1.7.1
                                   rust-arrayvec-0.7.6
                                   rust-async-compression-0.4.22
                                   rust-atomic-waker-1.1.2
                                   rust-auditable-extract-0.3.5
                                   rust-auditable-info-0.8.0
                                   rust-auditable-serde-0.7.0
                                   rust-autocfg-1.4.0
                                   rust-backtrace-0.3.71
                                   rust-base64-0.22.1
                                   rust-binfarce-0.2.1
                                   rust-bitflags-2.9.0
                                   rust-borsh-1.5.7
                                   rust-bstr-1.11.3
                                   rust-bumpalo-3.17.0
                                   rust-bytes-1.10.1
                                   rust-camino-1.1.9
                                   rust-canonical-path-2.0.2
                                   rust-cargo-lock-10.1.0
                                   rust-cc-1.2.18
                                   rust-cfg-if-1.0.0
                                   rust-cfg-aliases-0.2.1
                                   rust-clap-4.5.35
                                   rust-clap-builder-4.5.35
                                   rust-clap-derive-4.5.32
                                   rust-clap-lex-0.7.4
                                   rust-clru-0.6.2
                                   rust-color-eyre-0.6.3
                                   rust-colorchoice-1.0.3
                                   rust-core-foundation-0.9.4
                                   rust-core-foundation-0.10.0
                                   rust-core-foundation-sys-0.8.7
                                   rust-crc32fast-1.4.2
                                   rust-crossbeam-channel-0.5.14
                                   rust-crossbeam-deque-0.8.6
                                   rust-crossbeam-epoch-0.9.18
                                   rust-crossbeam-utils-0.8.21
                                   rust-cvss-2.0.0
                                   rust-deranged-0.4.1
                                   rust-display-error-chain-0.2.2
                                   rust-displaydoc-0.2.5
                                   rust-dunce-1.0.5
                                   rust-either-1.15.0
                                   rust-encoding-rs-0.8.35
                                   rust-equivalent-1.0.2
                                   rust-errno-0.3.11
                                   rust-eyre-0.6.12
                                   rust-faster-hex-0.9.0
                                   rust-fastrand-2.3.0
                                   rust-filetime-0.2.25
                                   rust-fixedbitset-0.4.2
                                   rust-flate2-1.1.1
                                   rust-fnv-1.0.7
                                   rust-form-urlencoded-1.2.1
                                   rust-fs-err-2.11.0
                                   rust-futures-channel-0.3.31
                                   rust-futures-core-0.3.31
                                   rust-futures-io-0.3.31
                                   rust-futures-sink-0.3.31
                                   rust-futures-task-0.3.31
                                   rust-futures-util-0.3.31
                                   rust-getrandom-0.2.15
                                   rust-getrandom-0.3.2
                                   rust-gimli-0.28.1
                                   rust-gix-0.70.0
                                   rust-gix-actor-0.33.2
                                   rust-gix-attributes-0.24.0
                                   rust-gix-bitmap-0.2.14
                                   rust-gix-chunk-0.4.11
                                   rust-gix-command-0.4.1
                                   rust-gix-commitgraph-0.26.0
                                   rust-gix-config-0.43.0
                                   rust-gix-config-value-0.14.12
                                   rust-gix-credentials-0.27.0
                                   rust-gix-date-0.9.4
                                   rust-gix-diff-0.50.0
                                   rust-gix-discover-0.38.0
                                   rust-gix-features-0.40.0
                                   rust-gix-filter-0.17.0
                                   rust-gix-fs-0.13.0
                                   rust-gix-glob-0.18.0
                                   rust-gix-hash-0.16.0
                                   rust-gix-hashtable-0.7.0
                                   rust-gix-ignore-0.13.0
                                   rust-gix-index-0.38.0
                                   rust-gix-lock-16.0.0
                                   rust-gix-negotiate-0.18.0
                                   rust-gix-object-0.47.0
                                   rust-gix-odb-0.67.0
                                   rust-gix-pack-0.57.0
                                   rust-gix-packetline-0.18.4
                                   rust-gix-packetline-blocking-0.18.3
                                   rust-gix-path-0.10.15
                                   rust-gix-pathspec-0.9.0
                                   rust-gix-prompt-0.9.1
                                   rust-gix-protocol-0.48.0
                                   rust-gix-quote-0.4.15
                                   rust-gix-ref-0.50.0
                                   rust-gix-refspec-0.28.0
                                   rust-gix-revision-0.32.0
                                   rust-gix-revwalk-0.18.0
                                   rust-gix-sec-0.10.12
                                   rust-gix-shallow-0.2.0
                                   rust-gix-submodule-0.17.0
                                   rust-gix-tempfile-16.0.0
                                   rust-gix-trace-0.1.12
                                   rust-gix-transport-0.45.0
                                   rust-gix-traverse-0.44.0
                                   rust-gix-url-0.29.0
                                   rust-gix-utils-0.1.14
                                   rust-gix-validate-0.9.4
                                   rust-gix-worktree-0.39.0
                                   rust-gix-worktree-state-0.17.0
                                   rust-h2-0.4.8
                                   rust-hashbrown-0.14.5
                                   rust-hashbrown-0.15.2
                                   rust-heck-0.5.0
                                   rust-home-0.5.11
                                   rust-http-1.3.1
                                   rust-http-body-1.0.1
                                   rust-http-body-util-0.1.3
                                   rust-httparse-1.10.1
                                   rust-hyper-1.6.0
                                   rust-hyper-rustls-0.27.5
                                   rust-hyper-util-0.1.11
                                   rust-icu-collections-1.5.0
                                   rust-icu-locid-1.5.0
                                   rust-icu-locid-transform-1.5.0
                                   rust-icu-locid-transform-data-1.5.1
                                   rust-icu-normalizer-1.5.0
                                   rust-icu-normalizer-data-1.5.1
                                   rust-icu-properties-1.5.1
                                   rust-icu-properties-data-1.5.1
                                   rust-icu-provider-1.5.0
                                   rust-icu-provider-macros-1.5.0
                                   rust-ident-case-1.0.1
                                   rust-idna-1.0.3
                                   rust-idna-adapter-1.2.0
                                   rust-indenter-0.3.3
                                   rust-indexmap-2.9.0
                                   rust-io-close-0.3.7
                                   rust-ipnet-2.11.0
                                   rust-is-terminal-polyfill-1.70.1
                                   rust-itoa-1.0.15
                                   rust-jiff-0.2.5
                                   rust-jiff-static-0.2.5
                                   rust-jiff-tzdb-0.1.4
                                   rust-jiff-tzdb-platform-0.1.3
                                   rust-js-sys-0.3.77
                                   rust-kstring-2.0.2
                                   rust-lazy-static-1.5.0
                                   rust-libc-0.2.171
                                   rust-libredox-0.1.3
                                   rust-linux-raw-sys-0.4.15
                                   rust-linux-raw-sys-0.9.3
                                   rust-litemap-0.7.5
                                   rust-lock-api-0.4.12
                                   rust-log-0.4.27
                                   rust-matchers-0.1.0
                                   rust-maybe-async-0.2.10
                                   rust-memchr-2.7.4
                                   rust-memmap2-0.9.5
                                   rust-mime-0.3.17
                                   rust-miniz-oxide-0.6.2
                                   rust-miniz-oxide-0.7.4
                                   rust-miniz-oxide-0.8.7
                                   rust-mio-1.0.3
                                   rust-nu-ansi-term-0.46.0
                                   rust-num-conv-0.1.0
                                   rust-object-0.32.2
                                   rust-once-cell-1.21.3
                                   rust-openssl-probe-0.1.6
                                   rust-overload-0.1.1
                                   rust-owo-colors-3.5.0
                                   rust-parking-lot-0.12.3
                                   rust-parking-lot-core-0.9.10
                                   rust-percent-encoding-2.3.1
                                   rust-petgraph-0.6.5
                                   rust-pin-project-lite-0.2.16
                                   rust-pin-utils-0.1.0
                                   rust-platforms-3.5.0
                                   rust-portable-atomic-1.11.0
                                   rust-portable-atomic-util-0.2.4
                                   rust-powerfmt-0.2.0
                                   rust-ppv-lite86-0.2.21
                                   rust-proc-macro2-1.0.94
                                   rust-prodash-29.0.1
                                   rust-quinn-0.11.7
                                   rust-quinn-proto-0.11.10
                                   rust-quinn-udp-0.5.11
                                   rust-quitters-0.1.0
                                   rust-quote-1.0.40
                                   rust-r-efi-5.2.0
                                   rust-rand-0.9.0
                                   rust-rand-chacha-0.9.0
                                   rust-rand-core-0.9.3
                                   rust-rayon-1.10.0
                                   rust-rayon-core-1.12.1
                                   rust-redox-syscall-0.5.10
                                   rust-regex-1.11.1
                                   rust-regex-automata-0.1.10
                                   rust-regex-automata-0.4.9
                                   rust-regex-syntax-0.6.29
                                   rust-regex-syntax-0.8.5
                                   rust-reqwest-0.12.15
                                   rust-ring-0.17.14
                                   rust-rustc-demangle-0.1.24
                                   rust-rustc-hash-2.1.1
                                   rust-rustc-stable-hash-0.1.2
                                   rust-rustix-0.38.44
                                   rust-rustix-1.0.5
                                   rust-rustls-0.23.25
                                   rust-rustls-native-certs-0.8.1
                                   rust-rustls-pemfile-2.2.0
                                   rust-rustls-pki-types-1.11.0
                                   rust-rustls-webpki-0.103.1
                                   rust-rustsec-0.30.2
                                   rust-rustversion-1.0.20
                                   rust-ryu-1.0.20
                                   rust-same-file-1.0.6
                                   rust-schannel-0.1.27
                                   rust-scopeguard-1.2.0
                                   rust-secrecy-0.10.3
                                   rust-security-framework-3.2.0
                                   rust-security-framework-sys-2.14.0
                                   rust-semver-1.0.26
                                   rust-serde-1.0.219
                                   rust-serde-derive-1.0.219
                                   rust-serde-json-1.0.140
                                   rust-serde-spanned-0.6.8
                                   rust-serde-urlencoded-0.7.1
                                   rust-sha1-smol-1.0.1
                                   rust-sharded-slab-0.1.7
                                   rust-shell-words-1.1.0
                                   rust-shlex-1.3.0
                                   rust-slab-0.4.9
                                   rust-smallvec-1.15.0
                                   rust-smol-str-0.3.2
                                   rust-socket2-0.5.9
                                   rust-stable-deref-trait-1.2.0
                                   rust-static-assertions-1.1.0
                                   rust-strsim-0.11.1
                                   rust-subtle-2.6.1
                                   rust-syn-1.0.109
                                   rust-syn-2.0.100
                                   rust-sync-wrapper-1.0.2
                                   rust-synstructure-0.12.6
                                   rust-synstructure-0.13.1
                                   rust-system-configuration-0.6.1
                                   rust-system-configuration-sys-0.6.0
                                   rust-tame-index-0.18.1
                                   rust-tempfile-3.19.1
                                   rust-termcolor-1.4.1
                                   rust-thiserror-1.0.69
                                   rust-thiserror-2.0.12
                                   rust-thiserror-impl-1.0.69
                                   rust-thiserror-impl-2.0.12
                                   rust-thread-local-1.1.8
                                   rust-time-0.3.41
                                   rust-time-core-0.1.4
                                   rust-time-macros-0.2.22
                                   rust-tinystr-0.7.6
                                   rust-tinyvec-1.9.0
                                   rust-tinyvec-macros-0.1.1
                                   rust-tokio-1.44.1
                                   rust-tokio-rustls-0.26.2
                                   rust-tokio-util-0.7.14
                                   rust-toml-0.8.20
                                   rust-toml-span-0.4.1
                                   rust-toml-datetime-0.6.8
                                   rust-toml-edit-0.22.24
                                   rust-topological-sort-0.2.2
                                   rust-tower-0.5.2
                                   rust-tower-layer-0.3.3
                                   rust-tower-service-0.3.3
                                   rust-tracing-0.1.41
                                   rust-tracing-attributes-0.1.28
                                   rust-tracing-core-0.1.33
                                   rust-tracing-log-0.2.0
                                   rust-tracing-subscriber-0.3.19
                                   rust-try-lock-0.2.5
                                   rust-twox-hash-2.1.0
                                   rust-uluru-3.1.0
                                   rust-unicode-bom-2.0.3
                                   rust-unicode-ident-1.0.18
                                   rust-unicode-normalization-0.1.24
                                   rust-unicode-xid-0.2.6
                                   rust-untrusted-0.9.0
                                   rust-url-2.5.4
                                   rust-utf16-iter-1.0.5
                                   rust-utf8-iter-1.0.4
                                   rust-utf8parse-0.2.2
                                   rust-valuable-0.1.1
                                   rust-version-check-0.9.5
                                   rust-wait-timeout-0.2.1
                                   rust-walkdir-2.5.0
                                   rust-want-0.3.1
                                   rust-wasi-0.11.0+wasi-snapshot-preview1
                                   rust-wasi-0.14.2+wasi-0.2.4
                                   rust-wasm-bindgen-0.2.100
                                   rust-wasm-bindgen-backend-0.2.100
                                   rust-wasm-bindgen-futures-0.4.50
                                   rust-wasm-bindgen-macro-0.2.100
                                   rust-wasm-bindgen-macro-support-0.2.100
                                   rust-wasm-bindgen-shared-0.2.100
                                   rust-wasmparser-0.207.0
                                   rust-web-sys-0.3.77
                                   rust-web-time-1.1.0
                                   rust-winapi-0.3.9
                                   rust-winapi-i686-pc-windows-gnu-0.4.0
                                   rust-winapi-util-0.1.9
                                   rust-winapi-x86-64-pc-windows-gnu-0.4.0
                                   rust-windows-link-0.1.1
                                   rust-windows-registry-0.4.0
                                   rust-windows-result-0.3.2
                                   rust-windows-strings-0.3.1
                                   rust-windows-sys-0.52.0
                                   rust-windows-sys-0.59.0
                                   rust-windows-targets-0.52.6
                                   rust-windows-targets-0.53.0
                                   rust-windows-aarch64-gnullvm-0.52.6
                                   rust-windows-aarch64-gnullvm-0.53.0
                                   rust-windows-aarch64-msvc-0.52.6
                                   rust-windows-aarch64-msvc-0.53.0
                                   rust-windows-i686-gnu-0.52.6
                                   rust-windows-i686-gnu-0.53.0
                                   rust-windows-i686-gnullvm-0.52.6
                                   rust-windows-i686-gnullvm-0.53.0
                                   rust-windows-i686-msvc-0.52.6
                                   rust-windows-i686-msvc-0.53.0
                                   rust-windows-x86-64-gnu-0.52.6
                                   rust-windows-x86-64-gnu-0.53.0
                                   rust-windows-x86-64-gnullvm-0.52.6
                                   rust-windows-x86-64-gnullvm-0.53.0
                                   rust-windows-x86-64-msvc-0.52.6
                                   rust-windows-x86-64-msvc-0.53.0
                                   rust-winnow-0.6.26
                                   rust-winnow-0.7.4
                                   rust-wit-bindgen-rt-0.39.0
                                   rust-write16-1.0.0
                                   rust-writeable-0.5.5
                                   rust-yoke-0.7.5
                                   rust-yoke-derive-0.7.5
                                   rust-zerocopy-0.7.35
                                   rust-zerocopy-0.8.24
                                   rust-zerocopy-derive-0.7.35
                                   rust-zerocopy-derive-0.8.24
                                   rust-zerofrom-0.1.6
                                   rust-zerofrom-derive-0.1.6
                                   rust-zeroize-1.8.1
                                   rust-zerovec-0.10.4
                                   rust-zerovec-derive-0.10.3))
                     (cargo-license =>
                                    (list rust-ansi-term-0.12.1
                                     rust-anstream-0.6.18
                                     rust-anstyle-1.0.10
                                     rust-anstyle-parse-0.2.6
                                     rust-anstyle-query-1.1.2
                                     rust-anstyle-wincon-3.0.7
                                     rust-anyhow-1.0.97
                                     rust-camino-1.1.9
                                     rust-cargo-platform-0.1.9
                                     rust-cargo-metadata-0.18.1
                                     rust-clap-4.5.35
                                     rust-clap-builder-4.5.35
                                     rust-clap-derive-4.5.32
                                     rust-clap-lex-0.7.4
                                     rust-colorchoice-1.0.3
                                     rust-csv-1.3.1
                                     rust-csv-core-0.1.12
                                     rust-either-1.15.0
                                     rust-equivalent-1.0.2
                                     rust-getopts-0.2.21
                                     rust-hashbrown-0.15.2
                                     rust-heck-0.5.0
                                     rust-indexmap-2.9.0
                                     rust-is-terminal-polyfill-1.70.1
                                     rust-itertools-0.12.1
                                     rust-itoa-1.0.15
                                     rust-memchr-2.7.4
                                     rust-once-cell-1.21.3
                                     rust-proc-macro2-1.0.94
                                     rust-quote-1.0.40
                                     rust-ryu-1.0.20
                                     rust-semver-1.0.26
                                     rust-serde-1.0.219
                                     rust-serde-derive-1.0.219
                                     rust-serde-json-1.0.140
                                     rust-serde-spanned-0.6.8
                                     rust-smallvec-1.15.0
                                     rust-spdx-0.10.8
                                     rust-strsim-0.11.1
                                     rust-syn-2.0.100
                                     rust-thiserror-1.0.69
                                     rust-thiserror-impl-1.0.69
                                     rust-toml-0.8.20
                                     rust-toml-datetime-0.6.8
                                     rust-toml-edit-0.22.24
                                     rust-unicode-ident-1.0.18
                                     rust-unicode-width-0.1.14
                                     rust-utf8parse-0.2.2
                                     rust-winapi-0.3.9
                                     rust-winapi-i686-pc-windows-gnu-0.4.0
                                     rust-winapi-x86-64-pc-windows-gnu-0.4.0
                                     rust-windows-sys-0.59.0
                                     rust-windows-targets-0.52.6
                                     rust-windows-aarch64-gnullvm-0.52.6
                                     rust-windows-aarch64-msvc-0.52.6
                                     rust-windows-i686-gnu-0.52.6
                                     rust-windows-i686-gnullvm-0.52.6
                                     rust-windows-i686-msvc-0.52.6
                                     rust-windows-x86-64-gnu-0.52.6
                                     rust-windows-x86-64-gnullvm-0.52.6
                                     rust-windows-x86-64-msvc-0.52.6
                                     rust-winnow-0.7.4))
                     (circtools =>
                                (list rust-adler2-2.0.0
                                      rust-aho-corasick-1.1.3
                                      rust-alga-0.9.3
                                      rust-android-tzdata-0.1.1
                                      rust-android-system-properties-0.1.5
                                      rust-anyhow-1.0.97
                                      rust-approx-0.3.2
                                      rust-array-macro-1.0.5
                                      rust-autocfg-1.4.0
                                      rust-bio-0.33.0
                                      rust-bio-types-1.0.4
                                      rust-bit-set-0.5.3
                                      rust-bit-vec-0.6.3
                                      rust-buffer-redux-1.0.2
                                      rust-bumpalo-3.17.0
                                      rust-bv-0.11.1
                                      rust-bytecount-0.6.8
                                      rust-byteorder-1.5.0
                                      rust-cc-1.2.18
                                      rust-cfg-if-1.0.0
                                      rust-chrono-0.4.40
                                      rust-cmake-0.1.54
                                      rust-console-0.15.11
                                      rust-core-foundation-sys-0.8.7
                                      rust-crc32fast-1.4.2
                                      rust-crossbeam-utils-0.8.21
                                      rust-csv-1.3.1
                                      rust-csv-core-0.1.12
                                      rust-custom-derive-0.1.7
                                      rust-derive-new-0.6.0
                                      rust-docopt-1.1.1
                                      rust-either-1.15.0
                                      rust-encode-unicode-1.0.0
                                      rust-enum-map-0.6.6
                                      rust-enum-map-derive-0.4.6
                                      rust-feature-probe-0.1.1
                                      rust-fixedbitset-0.2.0
                                      rust-flate2-1.1.1
                                      rust-fnv-1.0.7
                                      rust-fxhash-0.2.1
                                      rust-generic-array-0.13.3
                                      rust-getrandom-0.1.16
                                      rust-getset-0.0.9
                                      rust-hashbrown-0.12.3
                                      rust-heck-0.3.3
                                      rust-heck-0.5.0
                                      rust-iana-time-zone-0.1.63
                                      rust-iana-time-zone-haiku-0.1.2
                                      rust-indexmap-1.9.3
                                      rust-indicatif-0.15.0
                                      rust-itertools-0.9.0
                                      rust-itertools-num-0.1.3
                                      rust-itoa-1.0.15
                                      rust-js-sys-0.3.77
                                      rust-lazy-static-1.5.0
                                      rust-libc-0.2.171
                                      rust-libm-0.2.11
                                      rust-log-0.4.27
                                      rust-matrixmultiply-0.2.4
                                      rust-memchr-2.7.4
                                      rust-miniz-oxide-0.8.7
                                      rust-multimap-0.8.3
                                      rust-nalgebra-0.19.0
                                      rust-ndarray-0.14.0
                                      rust-newtype-derive-0.1.6
                                      rust-num-complex-0.2.4
                                      rust-num-complex-0.3.1
                                      rust-num-integer-0.1.46
                                      rust-num-rational-0.2.4
                                      rust-num-traits-0.2.19
                                      rust-number-prefix-0.3.0
                                      rust-once-cell-1.21.3
                                      rust-ordered-float-1.1.1
                                      rust-petgraph-0.5.1
                                      rust-ppv-lite86-0.2.21
                                      rust-proc-macro2-1.0.94
                                      rust-quote-1.0.40
                                      rust-rand-0.7.3
                                      rust-rand-chacha-0.2.2
                                      rust-rand-core-0.5.1
                                      rust-rand-distr-0.2.2
                                      rust-rand-hc-0.2.0
                                      rust-rawpointer-0.2.1
                                      rust-regex-1.11.1
                                      rust-regex-automata-0.4.9
                                      rust-regex-syntax-0.8.5
                                      rust-rustc-version-0.1.7
                                      rust-rustversion-1.0.20
                                      rust-ryu-1.0.20
                                      rust-scoped-threadpool-0.1.9
                                      rust-semver-0.1.20
                                      rust-seq-io-0.3.4
                                      rust-serde-1.0.219
                                      rust-serde-derive-1.0.219
                                      rust-shlex-1.3.0
                                      rust-statrs-0.13.0
                                      rust-strsim-0.10.0
                                      rust-strum-0.20.0
                                      rust-strum-macros-0.20.1
                                      rust-strum-macros-0.26.4
                                      rust-syn-1.0.109
                                      rust-syn-2.0.100
                                      rust-thiserror-1.0.69
                                      rust-thiserror-impl-1.0.69
                                      rust-triple-accel-0.3.4
                                      rust-typenum-1.18.0
                                      rust-unicode-ident-1.0.18
                                      rust-unicode-segmentation-1.12.0
                                      rust-unicode-width-0.2.0
                                      rust-vec-map-0.8.2
                                      rust-wasi-0.9.0+wasi-snapshot-preview1
                                      rust-wasm-bindgen-0.2.100
                                      rust-wasm-bindgen-backend-0.2.100
                                      rust-wasm-bindgen-macro-0.2.100
                                      rust-wasm-bindgen-macro-support-0.2.100
                                      rust-wasm-bindgen-shared-0.2.100
                                      rust-windows-core-0.61.0
                                      rust-windows-implement-0.60.0
                                      rust-windows-interface-0.59.1
                                      rust-windows-link-0.1.1
                                      rust-windows-result-0.3.2
                                      rust-windows-strings-0.4.0
                                      rust-windows-sys-0.59.0
                                      rust-windows-targets-0.52.6
                                      rust-windows-aarch64-gnullvm-0.52.6
                                      rust-windows-aarch64-msvc-0.52.6
                                      rust-windows-i686-gnu-0.52.6
                                      rust-windows-i686-gnullvm-0.52.6
                                      rust-windows-i686-msvc-0.52.6
                                      rust-windows-x86-64-gnu-0.52.6
                                      rust-windows-x86-64-gnullvm-0.52.6
                                      rust-windows-x86-64-msvc-0.52.6
                                      rust-zerocopy-0.8.24
                                      rust-zerocopy-derive-0.8.24))
                     (clamav =>
                             (list rust-adler2-2.0.0
                                   rust-adler32-1.2.0
                                   rust-aho-corasick-1.1.3
                                   rust-android-tzdata-0.1.1
                                   rust-android-system-properties-0.1.5
                                   rust-autocfg-1.4.0
                                   rust-base64-0.21.7
                                   rust-bindgen-0.65.1
                                   rust-bit-field-0.10.2
                                   rust-bitflags-1.3.2
                                   rust-bitflags-2.9.0
                                   rust-block-buffer-0.10.4
                                   rust-bumpalo-3.17.0
                                   rust-bytemuck-1.22.0
                                   rust-byteorder-1.5.0
                                   rust-bytes-1.10.1
                                   rust-bzip2-rs-0.1.2
                                   rust-cbindgen-0.25.0
                                   rust-cc-1.2.18
                                   rust-cexpr-0.6.0
                                   rust-cfg-if-1.0.0
                                   rust-chrono-0.4.40
                                   rust-clang-sys-1.8.1
                                   rust-color-quant-1.1.0
                                   rust-core-foundation-sys-0.8.7
                                   rust-cpufeatures-0.2.17
                                   rust-crc32fast-1.4.2
                                   rust-crossbeam-deque-0.8.6
                                   rust-crossbeam-epoch-0.9.18
                                   rust-crossbeam-utils-0.8.21
                                   rust-crunchy-0.2.3
                                   rust-crypto-common-0.1.6
                                   rust-delharc-0.6.1
                                   rust-digest-0.10.7
                                   rust-either-1.15.0
                                   rust-encoding-rs-0.8.35
                                   rust-enum-primitive-derive-0.2.2
                                   rust-errno-0.3.11
                                   rust-exr-1.73.0
                                   rust-fastrand-2.3.0
                                   rust-fdeflate-0.3.7
                                   rust-flate2-1.1.1
                                   rust-generic-array-0.14.7
                                   rust-getrandom-0.3.2
                                   rust-gif-0.13.1
                                   rust-glob-0.3.2
                                   rust-half-2.5.0
                                   rust-hashbrown-0.12.3
                                   rust-heck-0.4.1
                                   rust-hex-0.4.3
                                   rust-hex-literal-0.4.1
                                   rust-home-0.5.11
                                   rust-iana-time-zone-0.1.63
                                   rust-iana-time-zone-haiku-0.1.2
                                   rust-image-0.24.9
                                   rust-indexmap-1.9.3
                                   rust-inflate-0.4.5
                                   rust-itertools-0.10.5
                                   rust-itoa-1.0.15
                                   rust-jpeg-decoder-0.3.1
                                   rust-js-sys-0.3.77
                                   rust-lazy-static-1.5.0
                                   rust-lazycell-1.3.0
                                   rust-lebe-0.5.2
                                   rust-libc-0.2.171
                                   rust-libloading-0.8.6
                                   rust-linux-raw-sys-0.4.15
                                   rust-linux-raw-sys-0.9.3
                                   rust-log-0.4.27
                                   rust-memchr-2.7.4
                                   rust-minimal-lexical-0.2.1
                                   rust-miniz-oxide-0.8.7
                                   rust-nom-7.1.3
                                   rust-num-complex-0.4.6
                                   rust-num-integer-0.1.46
                                   rust-num-traits-0.2.19
                                   rust-once-cell-1.21.3
                                   rust-onenote-parser-0.3.1.29c0853
                                   rust-paste-1.0.15
                                   rust-peeking-take-while-0.1.2
                                   rust-png-0.17.16
                                   rust-prettyplease-0.2.32
                                   rust-primal-check-0.3.4
                                   rust-proc-macro2-1.0.94
                                   rust-qoi-0.4.1
                                   rust-quote-1.0.40
                                   rust-r-efi-5.2.0
                                   rust-rayon-1.10.0
                                   rust-rayon-core-1.12.1
                                   rust-regex-1.11.1
                                   rust-regex-automata-0.4.9
                                   rust-regex-syntax-0.8.5
                                   rust-rustc-hash-1.1.0
                                   rust-rustdct-0.7.1
                                   rust-rustfft-6.2.0
                                   rust-rustix-0.38.44
                                   rust-rustix-1.0.5
                                   rust-rustversion-1.0.20
                                   rust-ryu-1.0.20
                                   rust-serde-1.0.219
                                   rust-serde-derive-1.0.219
                                   rust-serde-json-1.0.140
                                   rust-sha1-0.10.6
                                   rust-sha2-0.10.8
                                   rust-shlex-1.3.0
                                   rust-simd-adler32-0.3.7
                                   rust-smallvec-1.15.0
                                   rust-strength-reduce-0.2.4
                                   rust-syn-1.0.109
                                   rust-syn-2.0.100
                                   rust-tempfile-3.19.1
                                   rust-thiserror-1.0.69
                                   rust-thiserror-impl-1.0.69
                                   rust-tiff-0.9.1
                                   rust-tinyvec-1.9.0
                                   rust-toml-0.5.11
                                   rust-transpose-0.2.3
                                   rust-typenum-1.18.0
                                   rust-unicode-ident-1.0.18
                                   rust-unicode-segmentation-1.12.0
                                   rust-uuid-1.16.0
                                   rust-version-check-0.9.5
                                   rust-wasi-0.14.2+wasi-0.2.4
                                   rust-wasm-bindgen-0.2.100
                                   rust-wasm-bindgen-backend-0.2.100
                                   rust-wasm-bindgen-macro-0.2.100
                                   rust-wasm-bindgen-macro-support-0.2.100
                                   rust-wasm-bindgen-shared-0.2.100
                                   rust-weezl-0.1.8
                                   rust-which-4.4.2
                                   rust-widestring-1.2.0
                                   rust-windows-core-0.61.0
                                   rust-windows-implement-0.60.0
                                   rust-windows-interface-0.59.1
                                   rust-windows-link-0.1.1
                                   rust-windows-result-0.3.2
                                   rust-windows-strings-0.4.0
                                   rust-windows-sys-0.59.0
                                   rust-windows-targets-0.52.6
                                   rust-windows-aarch64-gnullvm-0.52.6
                                   rust-windows-aarch64-msvc-0.52.6
                                   rust-windows-i686-gnu-0.52.6
                                   rust-windows-i686-gnullvm-0.52.6
                                   rust-windows-i686-msvc-0.52.6
                                   rust-windows-x86-64-gnu-0.52.6
                                   rust-windows-x86-64-gnullvm-0.52.6
                                   rust-windows-x86-64-msvc-0.52.6
                                   rust-wit-bindgen-rt-0.39.0
                                   rust-zune-inflate-0.2.54))
                     (du-dust =>
                              (list rust-aho-corasick-1.1.3
                                    rust-android-tzdata-0.1.1
                                    rust-android-system-properties-0.1.5
                                    rust-ansi-term-0.12.1
                                    rust-anstream-0.6.18
                                    rust-anstyle-1.0.10
                                    rust-anstyle-parse-0.2.6
                                    rust-anstyle-query-1.1.2
                                    rust-anstyle-wincon-3.0.7
                                    rust-assert-cmd-2.0.16
                                    rust-autocfg-1.4.0
                                    rust-bitflags-1.3.2
                                    rust-bitflags-2.9.0
                                    rust-bstr-1.11.3
                                    rust-bumpalo-3.17.0
                                    rust-cc-1.2.18
                                    rust-cfg-if-1.0.0
                                    rust-cfg-aliases-0.2.1
                                    rust-chrono-0.4.40
                                    rust-clap-4.5.35
                                    rust-clap-builder-4.5.35
                                    rust-clap-complete-4.5.47
                                    rust-clap-lex-0.7.4
                                    rust-clap-mangen-0.2.26
                                    rust-colorchoice-1.0.3
                                    rust-config-file-0.2.3
                                    rust-core-foundation-sys-0.8.7
                                    rust-crossbeam-deque-0.8.6
                                    rust-crossbeam-epoch-0.9.18
                                    rust-crossbeam-utils-0.8.21
                                    rust-ctrlc-3.4.6
                                    rust-difflib-0.4.0
                                    rust-directories-4.0.1
                                    rust-dirs-sys-0.3.7
                                    rust-doc-comment-0.3.3
                                    rust-either-1.15.0
                                    rust-errno-0.3.11
                                    rust-fastrand-2.3.0
                                    rust-filesize-0.2.0
                                    rust-getrandom-0.2.15
                                    rust-getrandom-0.3.2
                                    rust-hermit-abi-0.3.9
                                    rust-iana-time-zone-0.1.63
                                    rust-iana-time-zone-haiku-0.1.2
                                    rust-io-lifetimes-1.0.11
                                    rust-is-terminal-polyfill-1.70.1
                                    rust-itoa-1.0.15
                                    rust-js-sys-0.3.77
                                    rust-libc-0.2.171
                                    rust-libredox-0.1.3
                                    rust-linux-raw-sys-0.3.8
                                    rust-linux-raw-sys-0.9.3
                                    rust-log-0.4.27
                                    rust-lscolors-0.13.0
                                    rust-memchr-2.7.4
                                    rust-nix-0.29.0
                                    rust-ntapi-0.4.1
                                    rust-nu-ansi-term-0.46.0
                                    rust-num-traits-0.2.19
                                    rust-once-cell-1.21.3
                                    rust-overload-0.1.1
                                    rust-portable-atomic-1.11.0
                                    rust-predicates-3.1.3
                                    rust-predicates-core-1.0.9
                                    rust-predicates-tree-1.0.12
                                    rust-proc-macro2-1.0.94
                                    rust-quote-1.0.40
                                    rust-r-efi-5.2.0
                                    rust-rayon-1.10.0
                                    rust-rayon-core-1.12.1
                                    rust-redox-users-0.4.6
                                    rust-regex-1.11.1
                                    rust-regex-automata-0.4.9
                                    rust-regex-syntax-0.8.5
                                    rust-roff-0.2.2
                                    rust-rustix-0.37.28
                                    rust-rustix-1.0.5
                                    rust-rustversion-1.0.20
                                    rust-ryu-1.0.20
                                    rust-serde-1.0.219
                                    rust-serde-derive-1.0.219
                                    rust-serde-json-1.0.140
                                    rust-shlex-1.3.0
                                    rust-stfu8-0.2.7
                                    rust-strsim-0.11.1
                                    rust-syn-2.0.100
                                    rust-sysinfo-0.27.8
                                    rust-tempfile-3.19.1
                                    rust-terminal-size-0.2.6
                                    rust-termtree-0.5.1
                                    rust-thiserror-1.0.69
                                    rust-thiserror-impl-1.0.69
                                    rust-thousands-0.2.0
                                    rust-toml-0.5.11
                                    rust-unicode-ident-1.0.18
                                    rust-unicode-width-0.1.14
                                    rust-utf8parse-0.2.2
                                    rust-wait-timeout-0.2.1
                                    rust-wasi-0.11.0+wasi-snapshot-preview1
                                    rust-wasi-0.14.2+wasi-0.2.4
                                    rust-wasm-bindgen-0.2.100
                                    rust-wasm-bindgen-backend-0.2.100
                                    rust-wasm-bindgen-macro-0.2.100
                                    rust-wasm-bindgen-macro-support-0.2.100
                                    rust-wasm-bindgen-shared-0.2.100
                                    rust-winapi-0.3.9
                                    rust-winapi-i686-pc-windows-gnu-0.4.0
                                    rust-winapi-util-0.1.9
                                    rust-winapi-x86-64-pc-windows-gnu-0.4.0
                                    rust-windows-core-0.61.0
                                    rust-windows-implement-0.60.0
                                    rust-windows-interface-0.59.1
                                    rust-windows-link-0.1.1
                                    rust-windows-result-0.3.2
                                    rust-windows-strings-0.4.0
                                    rust-windows-sys-0.48.0
                                    rust-windows-sys-0.59.0
                                    rust-windows-targets-0.48.5
                                    rust-windows-targets-0.52.6
                                    rust-windows-aarch64-gnullvm-0.48.5
                                    rust-windows-aarch64-gnullvm-0.52.6
                                    rust-windows-aarch64-msvc-0.48.5
                                    rust-windows-aarch64-msvc-0.52.6
                                    rust-windows-i686-gnu-0.48.5
                                    rust-windows-i686-gnu-0.52.6
                                    rust-windows-i686-gnullvm-0.52.6
                                    rust-windows-i686-msvc-0.48.5
                                    rust-windows-i686-msvc-0.52.6
                                    rust-windows-x86-64-gnu-0.48.5
                                    rust-windows-x86-64-gnu-0.52.6
                                    rust-windows-x86-64-gnullvm-0.48.5
                                    rust-windows-x86-64-gnullvm-0.52.6
                                    rust-windows-x86-64-msvc-0.48.5
                                    rust-windows-x86-64-msvc-0.52.6
                                    rust-wit-bindgen-rt-0.39.0))
                     (fish =>
                           (list rust-allocator-api2-0.2.21
                                 rust-autocfg-1.4.0
                                 rust-bitflags-2.9.0
                                 rust-block-buffer-0.10.4
                                 rust-cc-1.2.18
                                 rust-cfg-if-1.0.0
                                 rust-cfg-aliases-0.2.1
                                 rust-cpufeatures-0.2.17
                                 rust-crypto-common-0.1.6
                                 rust-dashmap-5.5.3
                                 rust-digest-0.10.7
                                 rust-equivalent-1.0.2
                                 rust-errno-0.3.11
                                 rust-fnv-1.0.7
                                 rust-foldhash-0.1.5
                                 rust-generic-array-0.14.7
                                 rust-getrandom-0.3.2
                                 rust-hashbrown-0.14.5
                                 rust-hashbrown-0.15.2
                                 rust-jobserver-0.1.33
                                 rust-lazy-static-1.5.0
                                 rust-libc-0.2.171
                                 rust-lock-api-0.4.12
                                 rust-log-0.4.27
                                 rust-lru-0.12.5
                                 rust-memchr-2.7.4
                                 rust-minimal-lexical-0.2.1
                                 rust-nix-0.29.0
                                 rust-nom-7.1.3
                                 rust-num-traits-0.2.19
                                 rust-once-cell-1.21.3
                                 rust-parking-lot-0.12.3
                                 rust-parking-lot-core-0.9.10
                                 rust-pcre2-0.2.9.85b7afb
                                 rust-pcre2-sys-0.2.9.85b7afb
                                 rust-phf-0.11.3
                                 rust-phf-codegen-0.11.3
                                 rust-phf-generator-0.11.3
                                 rust-phf-shared-0.11.3
                                 rust-pkg-config-0.3.32
                                 rust-portable-atomic-1.11.0
                                 rust-proc-macro2-1.0.94
                                 rust-quote-1.0.40
                                 rust-r-efi-5.2.0
                                 rust-rand-0.8.5
                                 rust-rand-core-0.6.4
                                 rust-redox-syscall-0.5.11
                                 rust-rsconf-0.2.2
                                 rust-rust-embed-8.6.0
                                 rust-rust-embed-impl-8.6.0
                                 rust-rust-embed-utils-8.6.0
                                 rust-same-file-1.0.6
                                 rust-scopeguard-1.2.0
                                 rust-serial-test-1.0.0
                                 rust-serial-test-derive-1.0.0
                                 rust-sha2-0.10.8
                                 rust-shlex-1.3.0
                                 rust-siphasher-1.0.1
                                 rust-smallvec-1.15.0
                                 rust-syn-1.0.109
                                 rust-syn-2.0.100
                                 rust-terminfo-0.9.0
                                 rust-typenum-1.18.0
                                 rust-unicode-ident-1.0.18
                                 rust-version-check-0.9.5
                                 rust-walkdir-2.5.0
                                 rust-wasi-0.14.2+wasi-0.2.4
                                 rust-widestring-1.2.0
                                 rust-winapi-util-0.1.9
                                 rust-windows-sys-0.59.0
                                 rust-windows-targets-0.52.6
                                 rust-windows-aarch64-gnullvm-0.52.6
                                 rust-windows-aarch64-msvc-0.52.6
                                 rust-windows-i686-gnu-0.52.6
                                 rust-windows-i686-gnullvm-0.52.6
                                 rust-windows-i686-msvc-0.52.6
                                 rust-windows-x86-64-gnu-0.52.6
                                 rust-windows-x86-64-gnullvm-0.52.6
                                 rust-windows-x86-64-msvc-0.52.6
                                 rust-wit-bindgen-rt-0.39.0))
                     (gnome-authenticator =>
                                          (list rust-addr2line-0.24.2
                                           rust-adler2-2.0.0
                                           rust-aead-0.5.2
                                           rust-aes-0.8.4
                                           rust-aes-gcm-0.10.3
                                           rust-aho-corasick-1.1.3
                                           rust-android-tzdata-0.1.1
                                           rust-android-system-properties-0.1.5
                                           rust-anyhow-1.0.97
                                           rust-aperture-0.3.2
                                           rust-arrayref-0.3.9
                                           rust-arrayvec-0.7.6
                                           rust-ashpd-0.6.8
                                           rust-async-broadcast-0.5.1
                                           rust-async-channel-2.3.1
                                           rust-async-io-1.13.0
                                           rust-async-io-2.4.0
                                           rust-async-lock-2.8.0
                                           rust-async-lock-3.4.0
                                           rust-async-process-1.8.1
                                           rust-async-recursion-1.1.1
                                           rust-async-signal-0.2.10
                                           rust-async-task-4.7.1
                                           rust-async-trait-0.1.88
                                           rust-atomic-waker-1.1.2
                                           rust-atomic-refcell-0.1.13
                                           rust-autocfg-1.4.0
                                           rust-backtrace-0.3.74
                                           rust-base64-0.21.7
                                           rust-bitflags-1.3.2
                                           rust-bitflags-2.9.0
                                           rust-blake2b-simd-1.0.3
                                           rust-block-0.1.6
                                           rust-block-buffer-0.10.4
                                           rust-block-padding-0.3.3
                                           rust-blocking-1.6.1
                                           rust-bumpalo-3.17.0
                                           rust-bytecount-0.6.8
                                           rust-bytemuck-1.22.0
                                           rust-byteorder-1.5.0
                                           rust-bytes-1.10.1
                                           rust-cairo-rs-0.18.5
                                           rust-cairo-sys-rs-0.18.2
                                           rust-camino-1.1.9
                                           rust-cargo-platform-0.1.9
                                           rust-cargo-metadata-0.14.2
                                           rust-cbc-0.1.2
                                           rust-cc-1.2.18
                                           rust-cfg-expr-0.15.8
                                           rust-cfg-if-1.0.0
                                           rust-checked-int-cast-1.0.0
                                           rust-chrono-0.4.40
                                           rust-cipher-0.4.4
                                           rust-color-quant-1.1.0
                                           rust-concurrent-queue-2.5.0
                                           rust-constant-time-eq-0.3.1
                                           rust-core-foundation-0.9.4
                                           rust-core-foundation-sys-0.8.7
                                           rust-cpufeatures-0.2.17
                                           rust-crc32fast-1.4.2
                                           rust-crossbeam-utils-0.8.21
                                           rust-crypto-common-0.1.6
                                           rust-ctr-0.9.2
                                           rust-darling-0.20.11
                                           rust-darling-core-0.20.11
                                           rust-darling-macro-0.20.11
                                           rust-data-encoding-2.8.0
                                           rust-deranged-0.4.0
                                           rust-derivative-2.2.0
                                           rust-diesel-2.2.9
                                           rust-diesel-derives-2.2.4
                                           rust-diesel-migrations-2.2.0
                                           rust-diesel-table-macro-syntax-0.2.0
                                           rust-digest-0.10.7
                                           rust-displaydoc-0.2.5
                                           rust-doc-comment-0.3.3
                                           rust-dsl-auto-type-0.1.3
                                           rust-either-1.15.0
                                           rust-encoding-rs-0.8.35
                                           rust-enum-ordinalize-4.3.0
                                           rust-enum-ordinalize-derive-4.3.1
                                           rust-enumflags2-0.7.11
                                           rust-enumflags2-derive-0.7.11
                                           rust-equivalent-1.0.2
                                           rust-errno-0.3.11
                                           rust-error-chain-0.12.4
                                           rust-event-listener-2.5.3
                                           rust-event-listener-3.1.0
                                           rust-event-listener-5.4.0
                                           rust-event-listener-strategy-0.5.4
                                           rust-fastrand-1.9.0
                                           rust-fastrand-2.3.0
                                           rust-fdeflate-0.3.7
                                           rust-field-offset-0.3.6
                                           rust-flate2-1.1.1
                                           rust-fnv-1.0.7
                                           rust-foreign-types-0.3.2
                                           rust-foreign-types-shared-0.1.1
                                           rust-form-urlencoded-1.2.1
                                           rust-futures-channel-0.3.31
                                           rust-futures-core-0.3.31
                                           rust-futures-executor-0.3.31
                                           rust-futures-io-0.3.31
                                           rust-futures-lite-1.13.0
                                           rust-futures-lite-2.6.0
                                           rust-futures-macro-0.3.31
                                           rust-futures-sink-0.3.31
                                           rust-futures-task-0.3.31
                                           rust-futures-util-0.3.31
                                           rust-gdk-pixbuf-0.18.5
                                           rust-gdk-pixbuf-sys-0.18.0
                                           rust-gdk4-0.7.3
                                           rust-gdk4-sys-0.7.2
                                           rust-gdk4-wayland-0.7.2
                                           rust-gdk4-wayland-sys-0.7.2
                                           rust-gdk4-win32-0.7.2
                                           rust-gdk4-win32-sys-0.7.2
                                           rust-gdk4-x11-0.7.2
                                           rust-gdk4-x11-sys-0.7.2
                                           rust-generic-array-0.14.7
                                           rust-getrandom-0.2.15
                                           rust-getrandom-0.3.2
                                           rust-gettext-rs-0.7.2
                                           rust-gettext-sys-0.22.5
                                           rust-ghash-0.5.1
                                           rust-gimli-0.31.1
                                           rust-gio-0.18.4
                                           rust-gio-sys-0.18.1
                                           rust-glib-0.18.5
                                           rust-glib-macros-0.18.5
                                           rust-glib-sys-0.18.1
                                           rust-glob-0.3.2
                                           rust-gobject-sys-0.18.0
                                           rust-graphene-rs-0.18.1
                                           rust-graphene-sys-0.18.1
                                           rust-gsk4-0.7.3
                                           rust-gsk4-sys-0.7.3
                                           rust-gst-plugin-gtk4-0.11.4
                                           rust-gst-plugin-version-helper-0.8.2
                                           rust-gstreamer-0.21.3
                                           rust-gstreamer-audio-0.21.3
                                           rust-gstreamer-audio-sys-0.21.1
                                           rust-gstreamer-base-0.21.2
                                           rust-gstreamer-base-sys-0.21.1
                                           rust-gstreamer-gl-0.21.2
                                           rust-gstreamer-gl-egl-0.21.2
                                           rust-gstreamer-gl-egl-sys-0.21.2
                                           rust-gstreamer-gl-sys-0.21.2
                                           rust-gstreamer-gl-wayland-0.21.1
                                           rust-gstreamer-gl-wayland-sys-0.21.1
                                           rust-gstreamer-gl-x11-0.21.1
                                           rust-gstreamer-gl-x11-sys-0.21.1
                                           rust-gstreamer-pbutils-0.21.3
                                           rust-gstreamer-pbutils-sys-0.21.0
                                           rust-gstreamer-sys-0.21.2
                                           rust-gstreamer-video-0.21.2
                                           rust-gstreamer-video-sys-0.21.2
                                           rust-gtk4-0.7.3
                                           rust-gtk4-macros-0.7.2
                                           rust-gtk4-sys-0.7.3
                                           rust-h2-0.3.26
                                           rust-hashbrown-0.15.2
                                           rust-heck-0.4.1
                                           rust-heck-0.5.0
                                           rust-hermit-abi-0.3.9
                                           rust-hermit-abi-0.4.0
                                           rust-hex-0.4.3
                                           rust-hkdf-0.12.4
                                           rust-hmac-0.12.1
                                           rust-http-0.2.12
                                           rust-http-body-0.4.6
                                           rust-httparse-1.10.1
                                           rust-httpdate-1.0.3
                                           rust-hyper-0.14.32
                                           rust-hyper-tls-0.5.0
                                           rust-iana-time-zone-0.1.63
                                           rust-iana-time-zone-haiku-0.1.2
                                           rust-icu-collections-1.5.0
                                           rust-icu-locid-1.5.0
                                           rust-icu-locid-transform-1.5.0
                                           rust-icu-locid-transform-data-1.5.1
                                           rust-icu-normalizer-1.5.0
                                           rust-icu-normalizer-data-1.5.1
                                           rust-icu-properties-1.5.1
                                           rust-icu-properties-data-1.5.1
                                           rust-icu-provider-1.5.0
                                           rust-icu-provider-macros-1.5.0
                                           rust-ident-case-1.0.1
                                           rust-idna-1.0.3
                                           rust-idna-adapter-1.2.0
                                           rust-image-0.24.9
                                           rust-indexmap-2.9.0
                                           rust-inout-0.1.4
                                           rust-instant-0.1.13
                                           rust-io-lifetimes-1.0.11
                                           rust-ipnet-2.11.0
                                           rust-itertools-0.12.1
                                           rust-itoa-1.0.15
                                           rust-js-sys-0.3.77
                                           rust-lazy-static-1.5.0
                                           rust-libadwaita-0.5.3
                                           rust-libadwaita-sys-0.5.3
                                           rust-libc-0.2.171
                                           rust-libm-0.2.11
                                           rust-libsqlite3-sys-0.32.0
                                           rust-linux-raw-sys-0.3.8
                                           rust-linux-raw-sys-0.4.15
                                           rust-linux-raw-sys-0.9.3
                                           rust-litemap-0.7.5
                                           rust-locale-config-0.3.0
                                           rust-lock-api-0.4.12
                                           rust-log-0.4.27
                                           rust-malloc-buf-0.0.6
                                           rust-memchr-2.7.4
                                           rust-memoffset-0.7.1
                                           rust-memoffset-0.9.1
                                           rust-migrations-internals-2.2.0
                                           rust-migrations-macros-2.2.0
                                           rust-mime-0.3.17
                                           rust-miniz-oxide-0.8.7
                                           rust-mio-1.0.3
                                           rust-muldiv-1.0.1
                                           rust-native-tls-0.2.14
                                           rust-nix-0.26.4
                                           rust-num-0.4.3
                                           rust-num-bigint-0.4.6
                                           rust-num-bigint-dig-0.8.4
                                           rust-num-complex-0.4.6
                                           rust-num-conv-0.1.0
                                           rust-num-integer-0.1.46
                                           rust-num-iter-0.1.45
                                           rust-num-rational-0.4.2
                                           rust-num-traits-0.2.19
                                           rust-objc-0.2.7
                                           rust-objc-foundation-0.1.1
                                           rust-objc-id-0.1.1
                                           rust-object-0.36.7
                                           rust-once-cell-1.21.3
                                           rust-oo7-0.2.2
                                           rust-opaque-debug-0.3.1
                                           rust-openssl-0.10.72
                                           rust-openssl-macros-0.1.1
                                           rust-openssl-probe-0.1.6
                                           rust-openssl-sys-0.9.107
                                           rust-option-operations-0.5.0
                                           rust-ordered-stream-0.2.0
                                           rust-pango-0.18.3
                                           rust-pango-sys-0.18.0
                                           rust-parking-2.2.1
                                           rust-parking-lot-0.12.3
                                           rust-parking-lot-core-0.9.10
                                           rust-paste-1.0.15
                                           rust-pbkdf2-0.12.2
                                           rust-percent-encoding-2.3.1
                                           rust-pin-project-lite-0.2.16
                                           rust-pin-utils-0.1.0
                                           rust-piper-0.2.4
                                           rust-pkg-config-0.3.32
                                           rust-png-0.17.16
                                           rust-polling-2.8.0
                                           rust-polling-3.7.4
                                           rust-polyval-0.6.2
                                           rust-powerfmt-0.2.0
                                           rust-ppv-lite86-0.2.21
                                           rust-pretty-hex-0.4.1
                                           rust-proc-macro-crate-1.3.1
                                           rust-proc-macro-crate-2.0.0
                                           rust-proc-macro-error-1.0.4
                                           rust-proc-macro-error-attr-1.0.4
                                           rust-proc-macro2-1.0.94
                                           rust-prost-0.12.6
                                           rust-prost-derive-0.12.6
                                           rust-pulldown-cmark-0.9.6
                                           rust-qrencode-0.14.0
                                           rust-quick-xml-0.30.0
                                           rust-quote-1.0.40
                                           rust-r-efi-5.2.0
                                           rust-r2d2-0.8.10
                                           rust-rand-0.8.5
                                           rust-rand-chacha-0.3.1
                                           rust-rand-core-0.6.4
                                           rust-redox-syscall-0.5.11
                                           rust-regex-1.11.1
                                           rust-regex-automata-0.4.9
                                           rust-regex-syntax-0.8.5
                                           rust-reqwest-0.11.27
                                           rust-ring-0.17.14
                                           rust-roxmltree-0.18.1
                                           rust-rust-argon2-2.1.0
                                           rust-rustc-demangle-0.1.24
                                           rust-rustc-version-0.4.1
                                           rust-rustix-0.37.28
                                           rust-rustix-0.38.44
                                           rust-rustix-1.0.5
                                           rust-rustls-pemfile-1.0.4
                                           rust-rustversion-1.0.20
                                           rust-ryu-1.0.20
                                           rust-salsa20-0.10.2
                                           rust-same-file-1.0.6
                                           rust-schannel-0.1.27
                                           rust-scheduled-thread-pool-0.2.7
                                           rust-scopeguard-1.2.0
                                           rust-scrypt-0.11.0
                                           rust-search-provider-0.6.0
                                           rust-security-framework-2.11.1
                                           rust-security-framework-sys-2.14.0
                                           rust-semver-1.0.26
                                           rust-serde-1.0.219
                                           rust-serde-derive-1.0.219
                                           rust-serde-json-1.0.140
                                           rust-serde-repr-0.1.20
                                           rust-serde-spanned-0.6.8
                                           rust-serde-urlencoded-0.7.1
                                           rust-sha1-0.10.6
                                           rust-sha2-0.10.8
                                           rust-sharded-slab-0.1.7
                                           rust-shlex-1.3.0
                                           rust-signal-hook-registry-1.4.2
                                           rust-simd-adler32-0.3.7
                                           rust-skeptic-0.13.7
                                           rust-slab-0.4.9
                                           rust-smallvec-1.15.0
                                           rust-socket2-0.4.10
                                           rust-socket2-0.5.9
                                           rust-spin-0.9.8
                                           rust-stable-deref-trait-1.2.0
                                           rust-static-assertions-1.1.0
                                           rust-strsim-0.11.1
                                           rust-subtle-2.6.1
                                           rust-svg-metadata-0.4.4
                                           rust-syn-1.0.109
                                           rust-syn-2.0.100
                                           rust-sync-wrapper-0.1.2
                                           rust-synstructure-0.13.1
                                           rust-system-configuration-0.5.1
                                           rust-system-configuration-sys-0.5.0
                                           rust-system-deps-6.2.2
                                           rust-target-lexicon-0.12.16
                                           rust-temp-dir-0.1.14
                                           rust-tempfile-3.19.1
                                           rust-thiserror-1.0.69
                                           rust-thiserror-impl-1.0.69
                                           rust-thread-local-1.1.8
                                           rust-time-0.3.41
                                           rust-time-core-0.1.4
                                           rust-time-macros-0.2.22
                                           rust-tinystr-0.7.6
                                           rust-tokio-1.44.2
                                           rust-tokio-macros-2.5.0
                                           rust-tokio-native-tls-0.3.1
                                           rust-tokio-util-0.7.14
                                           rust-toml-0.8.20
                                           rust-toml-datetime-0.6.8
                                           rust-toml-edit-0.19.15
                                           rust-toml-edit-0.20.7
                                           rust-toml-edit-0.22.24
                                           rust-tower-service-0.3.3
                                           rust-tracing-0.1.41
                                           rust-tracing-attributes-0.1.28
                                           rust-tracing-core-0.1.33
                                           rust-tracing-subscriber-0.3.19
                                           rust-try-lock-0.2.5
                                           rust-typenum-1.18.0
                                           rust-uds-windows-1.1.0
                                           rust-unicase-2.8.1
                                           rust-unicode-ident-1.0.18
                                           rust-universal-hash-0.5.1
                                           rust-untrusted-0.9.0
                                           rust-url-2.5.4
                                           rust-utf16-iter-1.0.5
                                           rust-utf8-iter-1.0.4
                                           rust-uuid-1.16.0
                                           rust-vcpkg-0.2.15
                                           rust-version-compare-0.2.0
                                           rust-version-check-0.9.5
                                           rust-waker-fn-1.2.0
                                           rust-walkdir-2.5.0
                                           rust-want-0.3.1
                                           rust-wasi-0.11.0+wasi-snapshot-preview1
                                           rust-wasi-0.14.2+wasi-0.2.4
                                           rust-wasm-bindgen-0.2.100
                                           rust-wasm-bindgen-backend-0.2.100
                                           rust-wasm-bindgen-futures-0.4.50
                                           rust-wasm-bindgen-macro-0.2.100
                                           rust-wasm-bindgen-macro-support-0.2.100
                                           rust-wasm-bindgen-shared-0.2.100
                                           rust-web-sys-0.3.77
                                           rust-winapi-0.3.9
                                           rust-winapi-i686-pc-windows-gnu-0.4.0
                                           rust-winapi-util-0.1.9
                                           rust-winapi-x86-64-pc-windows-gnu-0.4.0
                                           rust-windows-core-0.61.0
                                           rust-windows-implement-0.60.0
                                           rust-windows-interface-0.59.1
                                           rust-windows-link-0.1.1
                                           rust-windows-result-0.3.2
                                           rust-windows-strings-0.4.0
                                           rust-windows-sys-0.48.0
                                           rust-windows-sys-0.52.0
                                           rust-windows-sys-0.59.0
                                           rust-windows-targets-0.48.5
                                           rust-windows-targets-0.52.6
                                           rust-windows-aarch64-gnullvm-0.48.5
                                           rust-windows-aarch64-gnullvm-0.52.6
                                           rust-windows-aarch64-msvc-0.48.5
                                           rust-windows-aarch64-msvc-0.52.6
                                           rust-windows-i686-gnu-0.48.5
                                           rust-windows-i686-gnu-0.52.6
                                           rust-windows-i686-gnullvm-0.52.6
                                           rust-windows-i686-msvc-0.48.5
                                           rust-windows-i686-msvc-0.52.6
                                           rust-windows-x86-64-gnu-0.48.5
                                           rust-windows-x86-64-gnu-0.52.6
                                           rust-windows-x86-64-gnullvm-0.48.5
                                           rust-windows-x86-64-gnullvm-0.52.6
                                           rust-windows-x86-64-msvc-0.48.5
                                           rust-windows-x86-64-msvc-0.52.6
                                           rust-winnow-0.5.40
                                           rust-winnow-0.7.4
                                           rust-winreg-0.50.0
                                           rust-wit-bindgen-rt-0.39.0
                                           rust-write16-1.0.0
                                           rust-writeable-0.5.5
                                           rust-xdg-home-1.3.0
                                           rust-xmlparser-0.13.6
                                           rust-yoke-0.7.5
                                           rust-yoke-derive-0.7.5
                                           rust-zbar-rust-0.0.24
                                           rust-zbus-3.15.2
                                           rust-zbus-macros-3.15.2
                                           rust-zbus-names-2.6.1
                                           rust-zerocopy-0.8.24
                                           rust-zerocopy-derive-0.8.24
                                           rust-zerofrom-0.1.6
                                           rust-zerofrom-derive-0.1.6
                                           rust-zeroize-1.8.1
                                           rust-zeroize-derive-1.4.2
                                           rust-zerovec-0.10.4
                                           rust-zerovec-derive-0.10.3
                                           rust-zvariant-3.15.2
                                           rust-zvariant-derive-3.15.2
                                           rust-zvariant-utils-1.0.1))
                     (greetd =>
                             (list rust-addr2line-0.24.2
                                   rust-adler2-2.0.0
                                   rust-async-trait-0.1.88
                                   rust-backtrace-0.3.74
                                   rust-bitflags-2.9.0
                                   rust-bytes-1.10.1
                                   rust-cfg-if-1.0.0
                                   rust-enquote-1.1.0
                                   rust-getopts-0.2.21
                                   rust-gimli-0.31.1
                                   rust-itoa-1.0.15
                                   rust-libc-0.2.171
                                   rust-memchr-2.7.4
                                   rust-miniz-oxide-0.8.7
                                   rust-mio-1.0.3
                                   rust-nix-0.27.1
                                   rust-object-0.36.7
                                   rust-pam-sys-0.5.6
                                   rust-pin-project-lite-0.2.16
                                   rust-proc-macro2-1.0.94
                                   rust-quote-1.0.40
                                   rust-rpassword-5.0.1
                                   rust-rustc-demangle-0.1.24
                                   rust-ryu-1.0.20
                                   rust-serde-1.0.219
                                   rust-serde-derive-1.0.219
                                   rust-serde-json-1.0.140
                                   rust-signal-hook-registry-1.4.2
                                   rust-socket2-0.5.9
                                   rust-syn-2.0.100
                                   rust-thiserror-1.0.69
                                   rust-thiserror-impl-1.0.69
                                   rust-tokio-1.44.2
                                   rust-tokio-macros-2.5.0
                                   rust-unicode-ident-1.0.18
                                   rust-unicode-width-0.1.14
                                   rust-wasi-0.11.0+wasi-snapshot-preview1
                                   rust-winapi-0.3.9
                                   rust-winapi-i686-pc-windows-gnu-0.4.0
                                   rust-winapi-x86-64-pc-windows-gnu-0.4.0
                                   rust-windows-sys-0.52.0
                                   rust-windows-targets-0.52.6
                                   rust-windows-aarch64-gnullvm-0.52.6
                                   rust-windows-aarch64-msvc-0.52.6
                                   rust-windows-i686-gnu-0.52.6
                                   rust-windows-i686-gnullvm-0.52.6
                                   rust-windows-i686-msvc-0.52.6
                                   rust-windows-x86-64-gnu-0.52.6
                                   rust-windows-x86-64-gnullvm-0.52.6
                                   rust-windows-x86-64-msvc-0.52.6))
                     (kanata =>
                             (list rust-addr2line-0.24.2
                                   rust-adler2-2.0.0
                                   rust-aho-corasick-1.1.3
                                   rust-anstyle-1.0.10
                                   rust-anyhow-1.0.97
                                   rust-arboard-3.5.0
                                   rust-arraydeque-0.5.1
                                   rust-atomic-polyfill-1.0.3
                                   rust-autocfg-1.4.0
                                   rust-backtrace-0.3.74
                                   rust-backtrace-ext-0.2.1
                                   rust-bitflags-1.3.2
                                   rust-bitflags-2.9.0
                                   rust-bitvec-1.0.1
                                   rust-bumpalo-3.17.0
                                   rust-bytemuck-1.22.0
                                   rust-byteorder-1.5.0
                                   rust-byteorder-lite-0.1.0
                                   rust-cc-1.2.18
                                   rust-cfg-if-1.0.0
                                   rust-clap-4.5.35
                                   rust-clap-builder-4.5.35
                                   rust-clap-derive-4.5.32
                                   rust-clap-lex-0.7.4
                                   rust-clipboard-win-5.4.0
                                   rust-core-foundation-0.10.0
                                   rust-core-foundation-sys-0.8.7
                                   rust-core-graphics-0.24.0
                                   rust-core-graphics-types-0.2.0
                                   rust-crc32fast-1.4.2
                                   rust-critical-section-1.2.0
                                   rust-deranged-0.4.0
                                   rust-dirs-5.0.1
                                   rust-dirs-sys-0.4.1
                                   rust-dunce-1.0.5
                                   rust-either-1.15.0
                                   rust-embed-resource-2.5.1
                                   rust-encode-unicode-0.3.6
                                   rust-endian-type-0.1.2
                                   rust-equivalent-1.0.2
                                   rust-errno-0.3.11
                                   rust-error-code-3.3.1
                                   rust-evdev-0.12.2
                                   rust-fdeflate-0.3.7
                                   rust-flate2-1.1.1
                                   rust-foreign-types-0.5.0
                                   rust-foreign-types-macros-0.2.3
                                   rust-foreign-types-shared-0.3.1
                                   rust-funty-2.0.0
                                   rust-gethostname-0.4.3
                                   rust-getrandom-0.2.15
                                   rust-getrandom-0.3.2
                                   rust-gimli-0.31.1
                                   rust-hash32-0.2.1
                                   rust-hashbrown-0.15.2
                                   rust-heapless-0.7.17
                                   rust-heck-0.5.0
                                   rust-hermit-abi-0.5.0
                                   rust-image-0.25.6
                                   rust-indexmap-2.9.0
                                   rust-indoc-2.0.6
                                   rust-inotify-0.10.2
                                   rust-inotify-sys-0.1.5
                                   rust-instant-0.1.13
                                   rust-interception-sys-0.1.3
                                   rust-is-docker-0.2.0
                                   rust-is-terminal-0.4.16
                                   rust-is-wsl-0.4.0
                                   rust-is-ci-1.2.0
                                   rust-itertools-0.12.1
                                   rust-itoa-1.0.15
                                   rust-jobserver-0.1.33
                                   rust-jpeg-decoder-0.3.1
                                   rust-js-sys-0.3.77
                                   rust-kanata-interception-0.3.0
                                   rust-kanata-keyberon-0.180.2
                                   rust-kanata-keyberon-macros-0.2.0
                                   rust-kanata-parser-0.180.2
                                   rust-kanata-tcp-protocol-0.180.2
                                   rust-karabiner-driverkit-0.1.5
                                   rust-lazy-static-1.5.0
                                   rust-libc-0.2.171
                                   rust-libredox-0.1.3
                                   rust-linux-raw-sys-0.4.15
                                   rust-lock-api-0.4.12
                                   rust-log-0.4.27
                                   rust-malloc-buf-0.0.6
                                   rust-memchr-2.7.4
                                   rust-memoffset-0.6.5
                                   rust-memoffset-0.7.1
                                   rust-miette-5.10.0
                                   rust-miette-derive-5.10.0
                                   rust-miniz-oxide-0.8.8
                                   rust-mio-0.8.11
                                   rust-muldiv-0.2.1
                                   rust-muldiv-1.0.1
                                   rust-native-windows-gui-1.0.13
                                   rust-nibble-vec-0.1.0
                                   rust-nix-0.23.2
                                   rust-nix-0.26.4
                                   rust-num-conv-0.1.0
                                   rust-num-traits-0.2.19
                                   rust-num-enum-0.6.1
                                   rust-num-enum-derive-0.6.1
                                   rust-num-threads-0.1.7
                                   rust-objc-0.2.7
                                   rust-objc2-0.6.0
                                   rust-objc2-app-kit-0.3.0
                                   rust-objc2-core-foundation-0.3.0
                                   rust-objc2-core-graphics-0.3.0
                                   rust-objc2-encode-4.1.0
                                   rust-objc2-foundation-0.3.0
                                   rust-objc2-io-surface-0.3.0
                                   rust-object-0.36.7
                                   rust-once-cell-1.21.3
                                   rust-open-5.3.2
                                   rust-option-ext-0.2.0
                                   rust-os-info-3.10.0
                                   rust-os-pipe-1.2.1
                                   rust-owo-colors-3.5.0
                                   rust-parking-lot-0.12.3
                                   rust-parking-lot-core-0.9.10
                                   rust-pathdiff-0.2.3
                                   rust-patricia-tree-0.8.0
                                   rust-percent-encoding-2.3.1
                                   rust-pin-utils-0.1.0
                                   rust-png-0.17.16
                                   rust-powerfmt-0.2.0
                                   rust-proc-macro-crate-1.3.1
                                   rust-proc-macro2-1.0.94
                                   rust-quote-1.0.40
                                   rust-r-efi-5.2.0
                                   rust-radium-0.7.0
                                   rust-radix-trie-0.2.1
                                   rust-redox-syscall-0.5.11
                                   rust-redox-users-0.4.6
                                   rust-regex-1.11.1
                                   rust-regex-automata-0.4.9
                                   rust-regex-syntax-0.8.5
                                   rust-rustc-demangle-0.1.24
                                   rust-rustc-hash-1.1.0
                                   rust-rustc-version-0.4.1
                                   rust-rustix-0.38.44
                                   rust-rustversion-1.0.20
                                   rust-ryu-1.0.20
                                   rust-scopeguard-1.2.0
                                   rust-sd-notify-0.4.5
                                   rust-semver-1.0.26
                                   rust-serde-1.0.219
                                   rust-serde-derive-1.0.219
                                   rust-serde-json-1.0.140
                                   rust-serde-spanned-0.6.8
                                   rust-shlex-1.3.0
                                   rust-signal-hook-0.3.17
                                   rust-signal-hook-registry-1.4.2
                                   rust-simd-adler32-0.3.7
                                   rust-simplelog-0.12.2
                                   rust-smallvec-1.15.0
                                   rust-smawk-0.3.2
                                   rust-spin-0.9.8
                                   rust-stable-deref-trait-1.2.0
                                   rust-strip-ansi-escapes-0.2.1
                                   rust-strsim-0.11.1
                                   rust-supports-color-2.1.0
                                   rust-supports-hyperlinks-2.1.0
                                   rust-supports-unicode-2.1.0
                                   rust-syn-2.0.100
                                   rust-tap-1.0.1
                                   rust-termcolor-1.4.1
                                   rust-terminal-size-0.1.17
                                   rust-textwrap-0.15.2
                                   rust-thiserror-1.0.69
                                   rust-thiserror-impl-1.0.69
                                   rust-tiff-0.9.1
                                   rust-time-0.3.41
                                   rust-time-core-0.1.4
                                   rust-time-macros-0.2.22
                                   rust-toml-0.8.20
                                   rust-toml-datetime-0.6.8
                                   rust-toml-edit-0.19.15
                                   rust-toml-edit-0.22.24
                                   rust-unicode-ident-1.0.18
                                   rust-unicode-linebreak-0.1.5
                                   rust-unicode-width-0.1.14
                                   rust-vswhom-0.1.0
                                   rust-vswhom-sys-0.1.3
                                   rust-vte-0.14.1
                                   rust-wasi-0.11.0+wasi-snapshot-preview1
                                   rust-wasi-0.14.2+wasi-0.2.4
                                   rust-wasm-bindgen-0.2.100
                                   rust-wasm-bindgen-backend-0.2.100
                                   rust-wasm-bindgen-macro-0.2.100
                                   rust-wasm-bindgen-macro-support-0.2.100
                                   rust-wasm-bindgen-shared-0.2.100
                                   rust-web-sys-0.3.77
                                   rust-weezl-0.1.8
                                   rust-winapi-0.3.9
                                   rust-winapi-build-0.1.1
                                   rust-winapi-i686-pc-windows-gnu-0.4.0
                                   rust-winapi-util-0.1.9
                                   rust-winapi-x86-64-pc-windows-gnu-0.4.0
                                   rust-windows-sys-0.48.0
                                   rust-windows-sys-0.52.0
                                   rust-windows-sys-0.59.0
                                   rust-windows-targets-0.48.5
                                   rust-windows-targets-0.52.6
                                   rust-windows-aarch64-gnullvm-0.48.5
                                   rust-windows-aarch64-gnullvm-0.52.6
                                   rust-windows-aarch64-msvc-0.48.5
                                   rust-windows-aarch64-msvc-0.52.6
                                   rust-windows-i686-gnu-0.48.5
                                   rust-windows-i686-gnu-0.52.6
                                   rust-windows-i686-gnullvm-0.52.6
                                   rust-windows-i686-msvc-0.48.5
                                   rust-windows-i686-msvc-0.52.6
                                   rust-windows-x86-64-gnu-0.48.5
                                   rust-windows-x86-64-gnu-0.52.6
                                   rust-windows-x86-64-gnullvm-0.48.5
                                   rust-windows-x86-64-gnullvm-0.52.6
                                   rust-windows-x86-64-msvc-0.48.5
                                   rust-windows-x86-64-msvc-0.52.6
                                   rust-winnow-0.5.40
                                   rust-winnow-0.7.6
                                   rust-winreg-0.52.0
                                   rust-wit-bindgen-rt-0.39.0
                                   rust-wyz-0.5.1
                                   rust-x11rb-0.13.1
                                   rust-x11rb-protocol-0.13.1))
                     (libchewing =>
                                 (list rust-anstream-0.6.18
                                  rust-anstyle-1.0.10
                                  rust-anstyle-parse-0.2.6
                                  rust-anstyle-query-1.1.2
                                  rust-anstyle-wincon-3.0.7
                                  rust-anyhow-1.0.97
                                  rust-bitflags-2.9.0
                                  rust-cfg-if-1.0.0
                                  rust-clap-4.5.35
                                  rust-clap-builder-4.5.35
                                  rust-clap-derive-4.5.32
                                  rust-clap-lex-0.7.4
                                  rust-clap-mangen-0.2.26
                                  rust-colorchoice-1.0.3
                                  rust-der-0.7.9
                                  rust-directories-5.0.1
                                  rust-dirs-sys-0.4.1
                                  rust-env-filter-0.1.3
                                  rust-env-logger-0.11.8
                                  rust-errno-0.3.11
                                  rust-fallible-iterator-0.3.0
                                  rust-fallible-streaming-iterator-0.1.9
                                  rust-fastrand-2.3.0
                                  rust-foldhash-0.1.5
                                  rust-getrandom-0.2.15
                                  rust-getrandom-0.3.2
                                  rust-hashbrown-0.15.2
                                  rust-hashlink-0.10.0
                                  rust-heck-0.5.0
                                  rust-is-terminal-polyfill-1.70.1
                                  rust-libc-0.2.171
                                  rust-libredox-0.1.3
                                  rust-libsqlite3-sys-0.32.0
                                  rust-linux-raw-sys-0.9.4
                                  rust-log-0.4.27
                                  rust-once-cell-1.21.3
                                  rust-option-ext-0.2.0
                                  rust-pkg-config-0.3.32
                                  rust-proc-macro2-1.0.94
                                  rust-quote-1.0.40
                                  rust-r-efi-5.2.0
                                  rust-redox-users-0.4.6
                                  rust-roff-0.2.2
                                  rust-rusqlite-0.34.0
                                  rust-rustix-1.0.5
                                  rust-smallvec-1.15.0
                                  rust-strsim-0.11.1
                                  rust-syn-2.0.100
                                  rust-tempfile-3.19.1
                                  rust-thiserror-1.0.69
                                  rust-thiserror-impl-1.0.69
                                  rust-unicode-ident-1.0.18
                                  rust-utf8parse-0.2.2
                                  rust-vcpkg-0.2.15
                                  rust-wasi-0.11.0+wasi-snapshot-preview1
                                  rust-wasi-0.14.2+wasi-0.2.4
                                  rust-windows-sys-0.48.0
                                  rust-windows-sys-0.59.0
                                  rust-windows-targets-0.48.5
                                  rust-windows-targets-0.52.6
                                  rust-windows-aarch64-gnullvm-0.48.5
                                  rust-windows-aarch64-gnullvm-0.52.6
                                  rust-windows-aarch64-msvc-0.48.5
                                  rust-windows-aarch64-msvc-0.52.6
                                  rust-windows-i686-gnu-0.48.5
                                  rust-windows-i686-gnu-0.52.6
                                  rust-windows-i686-gnullvm-0.52.6
                                  rust-windows-i686-msvc-0.48.5
                                  rust-windows-i686-msvc-0.52.6
                                  rust-windows-x86-64-gnu-0.48.5
                                  rust-windows-x86-64-gnu-0.52.6
                                  rust-windows-x86-64-gnullvm-0.48.5
                                  rust-windows-x86-64-gnullvm-0.52.6
                                  rust-windows-x86-64-msvc-0.48.5
                                  rust-windows-x86-64-msvc-0.52.6
                                  rust-wit-bindgen-rt-0.39.0
                                  rust-zeroize-1.8.1))
                     (librsvg =>
                              (list rust-adler2-2.0.0
                                    rust-aho-corasick-1.1.3
                                    rust-android-tzdata-0.1.1
                                    rust-android-system-properties-0.1.5
                                    rust-anes-0.1.6
                                    rust-anstream-0.6.18
                                    rust-anstyle-1.0.10
                                    rust-anstyle-parse-0.2.6
                                    rust-anstyle-query-1.1.2
                                    rust-anstyle-wincon-3.0.7
                                    rust-anyhow-1.0.97
                                    rust-approx-0.5.1
                                    rust-assert-cmd-2.0.16
                                    rust-autocfg-1.4.0
                                    rust-bit-set-0.8.0
                                    rust-bit-vec-0.8.0
                                    rust-bit-field-0.10.2
                                    rust-bitflags-1.3.2
                                    rust-bitflags-2.9.0
                                    rust-block-0.1.6
                                    rust-bstr-1.11.3
                                    rust-bumpalo-3.17.0
                                    rust-bytemuck-1.22.0
                                    rust-byteorder-1.5.0
                                    rust-cairo-rs-0.19.4
                                    rust-cairo-sys-rs-0.19.2
                                    rust-cast-0.3.0
                                    rust-cc-1.2.18
                                    rust-cfg-expr-0.15.8
                                    rust-cfg-if-1.0.0
                                    rust-chrono-0.4.40
                                    rust-ciborium-0.2.2
                                    rust-ciborium-io-0.2.2
                                    rust-ciborium-ll-0.2.2
                                    rust-clap-4.5.35
                                    rust-clap-builder-4.5.35
                                    rust-clap-complete-4.5.47
                                    rust-clap-derive-4.5.32
                                    rust-clap-lex-0.7.4
                                    rust-color-quant-1.1.0
                                    rust-colorchoice-1.0.3
                                    rust-core-foundation-sys-0.8.7
                                    rust-crc32fast-1.4.2
                                    rust-criterion-0.5.1
                                    rust-criterion-plot-0.5.0
                                    rust-crossbeam-deque-0.8.6
                                    rust-crossbeam-epoch-0.9.18
                                    rust-crossbeam-utils-0.8.21
                                    rust-crunchy-0.2.3
                                    rust-cssparser-0.31.2
                                    rust-cssparser-macros-0.6.1
                                    rust-cstr-0.2.12
                                    rust-data-url-0.3.1
                                    rust-deranged-0.4.1
                                    rust-derive-more-0.99.19
                                    rust-difflib-0.4.0
                                    rust-displaydoc-0.2.5
                                    rust-dlib-0.5.2
                                    rust-doc-comment-0.3.3
                                    rust-dtoa-1.0.10
                                    rust-dtoa-short-0.3.5
                                    rust-either-1.15.0
                                    rust-encoding-rs-0.8.35
                                    rust-equivalent-1.0.2
                                    rust-errno-0.3.11
                                    rust-exr-1.73.0
                                    rust-fastrand-2.3.0
                                    rust-fdeflate-0.3.7
                                    rust-flate2-1.1.1
                                    rust-float-cmp-0.9.0
                                    rust-float-cmp-0.10.0
                                    rust-fnv-1.0.7
                                    rust-form-urlencoded-1.2.1
                                    rust-futf-0.1.5
                                    rust-futures-channel-0.3.31
                                    rust-futures-core-0.3.31
                                    rust-futures-executor-0.3.31
                                    rust-futures-io-0.3.31
                                    rust-futures-macro-0.3.31
                                    rust-futures-task-0.3.31
                                    rust-futures-util-0.3.31
                                    rust-fxhash-0.2.1
                                    rust-gdk-pixbuf-0.19.8
                                    rust-gdk-pixbuf-sys-0.19.8
                                    rust-getrandom-0.2.15
                                    rust-getrandom-0.3.2
                                    rust-gif-0.13.1
                                    rust-gio-0.19.8
                                    rust-gio-sys-0.19.8
                                    rust-glib-0.19.9
                                    rust-glib-macros-0.19.9
                                    rust-glib-sys-0.19.8
                                    rust-gobject-sys-0.19.8
                                    rust-half-2.5.0
                                    rust-hashbrown-0.15.2
                                    rust-heck-0.5.0
                                    rust-hermit-abi-0.5.0
                                    rust-iana-time-zone-0.1.63
                                    rust-iana-time-zone-haiku-0.1.2
                                    rust-icu-collections-1.5.0
                                    rust-icu-locid-1.5.0
                                    rust-icu-locid-transform-1.5.0
                                    rust-icu-locid-transform-data-1.5.1
                                    rust-icu-normalizer-1.5.0
                                    rust-icu-normalizer-data-1.5.1
                                    rust-icu-properties-1.5.1
                                    rust-icu-properties-data-1.5.1
                                    rust-icu-provider-1.5.0
                                    rust-icu-provider-macros-1.5.0
                                    rust-idna-1.0.3
                                    rust-idna-adapter-1.2.0
                                    rust-image-0.24.9
                                    rust-indexmap-2.9.0
                                    rust-is-terminal-0.4.16
                                    rust-is-terminal-polyfill-1.70.1
                                    rust-itertools-0.10.5
                                    rust-itertools-0.12.1
                                    rust-itoa-1.0.15
                                    rust-jpeg-decoder-0.3.1
                                    rust-js-sys-0.3.77
                                    rust-language-tags-0.3.2
                                    rust-lazy-static-1.5.0
                                    rust-lebe-0.5.2
                                    rust-libc-0.2.171
                                    rust-libloading-0.8.6
                                    rust-linked-hash-map-0.5.6
                                    rust-linux-raw-sys-0.9.3
                                    rust-litemap-0.7.5
                                    rust-locale-config-0.3.0
                                    rust-lock-api-0.4.12
                                    rust-log-0.4.27
                                    rust-lopdf-0.32.0
                                    rust-mac-0.1.1
                                    rust-malloc-buf-0.0.6
                                    rust-markup5ever-0.11.0
                                    rust-matches-0.1.10
                                    rust-matrixmultiply-0.3.9
                                    rust-md5-0.7.0
                                    rust-memchr-2.7.4
                                    rust-minimal-lexical-0.2.1
                                    rust-miniz-oxide-0.8.7
                                    rust-nalgebra-0.32.6
                                    rust-nalgebra-macros-0.2.2
                                    rust-new-debug-unreachable-1.0.6
                                    rust-nom-7.1.3
                                    rust-normalize-line-endings-0.3.0
                                    rust-num-complex-0.4.6
                                    rust-num-conv-0.1.0
                                    rust-num-integer-0.1.46
                                    rust-num-rational-0.4.2
                                    rust-num-traits-0.2.19
                                    rust-objc-0.2.7
                                    rust-objc-foundation-0.1.1
                                    rust-objc-id-0.1.1
                                    rust-once-cell-1.21.3
                                    rust-oorandom-11.1.5
                                    rust-pango-0.19.8
                                    rust-pango-sys-0.19.8
                                    rust-pangocairo-0.19.8
                                    rust-pangocairo-sys-0.19.8
                                    rust-parking-lot-0.12.3
                                    rust-parking-lot-core-0.9.10
                                    rust-paste-1.0.15
                                    rust-percent-encoding-2.3.1
                                    rust-phf-0.10.1
                                    rust-phf-0.11.3
                                    rust-phf-codegen-0.10.0
                                    rust-phf-generator-0.10.0
                                    rust-phf-generator-0.11.3
                                    rust-phf-macros-0.11.3
                                    rust-phf-shared-0.10.0
                                    rust-phf-shared-0.11.3
                                    rust-pin-project-lite-0.2.16
                                    rust-pin-utils-0.1.0
                                    rust-pkg-config-0.3.32
                                    rust-plotters-0.3.7
                                    rust-plotters-backend-0.3.7
                                    rust-plotters-svg-0.3.7
                                    rust-png-0.17.16
                                    rust-powerfmt-0.2.0
                                    rust-ppv-lite86-0.2.21
                                    rust-precomputed-hash-0.1.1
                                    rust-predicates-3.1.3
                                    rust-predicates-core-1.0.9
                                    rust-predicates-tree-1.0.12
                                    rust-proc-macro-crate-3.3.0
                                    rust-proc-macro2-1.0.94
                                    rust-proptest-1.6.0
                                    rust-qoi-0.4.1
                                    rust-quick-error-1.2.3
                                    rust-quick-error-2.0.1
                                    rust-quote-1.0.40
                                    rust-r-efi-5.2.0
                                    rust-rand-0.8.5
                                    rust-rand-chacha-0.3.1
                                    rust-rand-core-0.6.4
                                    rust-rand-xorshift-0.3.0
                                    rust-rawpointer-0.2.1
                                    rust-rayon-1.10.0
                                    rust-rayon-core-1.12.1
                                    rust-rctree-0.6.0
                                    rust-redox-syscall-0.5.10
                                    rust-regex-1.11.1
                                    rust-regex-automata-0.4.9
                                    rust-regex-syntax-0.8.5
                                    rust-rgb-0.8.50
                                    rust-rustix-1.0.5
                                    rust-rustversion-1.0.20
                                    rust-rusty-fork-0.3.0
                                    rust-ryu-1.0.20
                                    rust-safe-arch-0.7.4
                                    rust-same-file-1.0.6
                                    rust-scopeguard-1.2.0
                                    rust-selectors-0.25.0
                                    rust-serde-1.0.219
                                    rust-serde-derive-1.0.219
                                    rust-serde-json-1.0.140
                                    rust-serde-spanned-0.6.8
                                    rust-servo-arc-0.3.0
                                    rust-shlex-1.3.0
                                    rust-simba-0.8.1
                                    rust-simd-adler32-0.3.7
                                    rust-siphasher-0.3.11
                                    rust-siphasher-1.0.1
                                    rust-slab-0.4.9
                                    rust-smallvec-1.15.0
                                    rust-stable-deref-trait-1.2.0
                                    rust-string-cache-0.8.9
                                    rust-string-cache-codegen-0.5.4
                                    rust-strsim-0.11.1
                                    rust-syn-2.0.100
                                    rust-synstructure-0.13.1
                                    rust-system-deps-6.2.2
                                    rust-target-lexicon-0.12.16
                                    rust-tempfile-3.19.1
                                    rust-tendril-0.4.3
                                    rust-termtree-0.5.1
                                    rust-thiserror-1.0.69
                                    rust-thiserror-impl-1.0.69
                                    rust-tiff-0.9.1
                                    rust-time-0.3.41
                                    rust-time-core-0.1.4
                                    rust-time-macros-0.2.22
                                    rust-tinystr-0.7.6
                                    rust-tinytemplate-1.2.1
                                    rust-tinyvec-1.9.0
                                    rust-tinyvec-macros-0.1.1
                                    rust-toml-0.8.20
                                    rust-toml-datetime-0.6.8
                                    rust-toml-edit-0.22.24
                                    rust-typenum-1.18.0
                                    rust-unarray-0.1.4
                                    rust-unicode-ident-1.0.18
                                    rust-url-2.5.4
                                    rust-utf-8-0.7.6
                                    rust-utf16-iter-1.0.5
                                    rust-utf8-iter-1.0.4
                                    rust-utf8parse-0.2.2
                                    rust-version-compare-0.2.0
                                    rust-wait-timeout-0.2.1
                                    rust-walkdir-2.5.0
                                    rust-wasi-0.11.0+wasi-snapshot-preview1
                                    rust-wasi-0.14.2+wasi-0.2.4
                                    rust-wasm-bindgen-0.2.100
                                    rust-wasm-bindgen-backend-0.2.100
                                    rust-wasm-bindgen-macro-0.2.100
                                    rust-wasm-bindgen-macro-support-0.2.100
                                    rust-wasm-bindgen-shared-0.2.100
                                    rust-web-sys-0.3.77
                                    rust-weezl-0.1.8
                                    rust-wide-0.7.32
                                    rust-winapi-0.3.9
                                    rust-winapi-i686-pc-windows-gnu-0.4.0
                                    rust-winapi-util-0.1.9
                                    rust-winapi-x86-64-pc-windows-gnu-0.4.0
                                    rust-windows-core-0.61.0
                                    rust-windows-implement-0.60.0
                                    rust-windows-interface-0.59.1
                                    rust-windows-link-0.1.1
                                    rust-windows-result-0.3.2
                                    rust-windows-strings-0.4.0
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
                                    rust-windows-x86-64-msvc-0.52.6
                                    rust-winnow-0.7.4
                                    rust-wit-bindgen-rt-0.39.0
                                    rust-write16-1.0.0
                                    rust-writeable-0.5.5
                                    rust-xml5ever-0.17.0
                                    rust-yeslogic-fontconfig-sys-5.0.0
                                    rust-yoke-0.7.5
                                    rust-yoke-derive-0.7.5
                                    rust-zerocopy-0.8.24
                                    rust-zerocopy-derive-0.8.24
                                    rust-zerofrom-0.1.6
                                    rust-zerofrom-derive-0.1.6
                                    rust-zerovec-0.10.4
                                    rust-zerovec-derive-0.10.3
                                    rust-zune-inflate-0.2.54))
                     (maturin =>
                              (list rust-adler2-2.0.0
                                    rust-ahash-0.8.11
                                    rust-aho-corasick-1.1.3
                                    rust-allocator-api2-0.2.21
                                    rust-anstream-0.6.18
                                    rust-anstyle-1.0.10
                                    rust-anstyle-parse-0.2.6
                                    rust-anstyle-query-1.1.2
                                    rust-anstyle-wincon-3.0.7
                                    rust-anyhow-1.0.97
                                    rust-arbitrary-1.4.1
                                    rust-autocfg-1.4.0
                                    rust-automod-1.0.15
                                    rust-base64-0.21.7
                                    rust-base64-0.22.1
                                    rust-bitflags-2.9.0
                                    rust-block-buffer-0.10.4
                                    rust-bstr-1.11.3
                                    rust-bumpalo-3.17.0
                                    rust-byteorder-1.5.0
                                    rust-bytesize-1.3.3
                                    rust-bzip2-0.5.2
                                    rust-bzip2-sys-0.1.13+1.0.8
                                    rust-camino-1.1.9
                                    rust-cargo-config2-0.1.32
                                    rust-cargo-options-0.7.5
                                    rust-cargo-platform-0.1.9
                                    rust-cargo-metadata-0.19.2
                                    rust-cbindgen-0.27.0
                                    rust-cc-1.2.18
                                    rust-cfg-if-1.0.0
                                    rust-charset-0.1.5
                                    rust-chumsky-0.9.3
                                    rust-clap-4.5.35
                                    rust-clap-builder-4.5.35
                                    rust-clap-complete-4.5.47
                                    rust-clap-complete-command-0.6.1
                                    rust-clap-complete-nushell-4.5.5
                                    rust-clap-derive-4.5.32
                                    rust-clap-lex-0.7.4
                                    rust-colorchoice-1.0.3
                                    rust-configparser-3.1.0
                                    rust-console-0.15.11
                                    rust-content-inspector-0.2.4
                                    rust-core-foundation-0.9.4
                                    rust-core-foundation-sys-0.8.7
                                    rust-cpufeatures-0.2.17
                                    rust-crc32fast-1.4.2
                                    rust-crossbeam-deque-0.8.6
                                    rust-crossbeam-epoch-0.9.18
                                    rust-crossbeam-utils-0.8.21
                                    rust-crypto-common-0.1.6
                                    rust-data-encoding-2.8.0
                                    rust-deranged-0.4.1
                                    rust-derivative-2.2.0
                                    rust-derive-arbitrary-1.4.1
                                    rust-dialoguer-0.11.0
                                    rust-diff-0.1.13
                                    rust-digest-0.10.7
                                    rust-dirs-5.0.1
                                    rust-dirs-sys-0.4.1
                                    rust-displaydoc-0.2.5
                                    rust-dissimilar-1.0.10
                                    rust-dunce-1.0.5
                                    rust-dyn-clone-1.0.19
                                    rust-either-1.15.0
                                    rust-encode-unicode-1.0.0
                                    rust-encoding-rs-0.8.35
                                    rust-env-home-0.1.0
                                    rust-equivalent-1.0.2
                                    rust-errno-0.3.11
                                    rust-expect-test-1.5.1
                                    rust-fastrand-2.3.0
                                    rust-fat-macho-0.4.9
                                    rust-filetime-0.2.25
                                    rust-flate2-1.1.1
                                    rust-foreign-types-0.3.2
                                    rust-foreign-types-shared-0.1.1
                                    rust-form-urlencoded-1.2.1
                                    rust-fs-err-3.1.0
                                    rust-fs4-0.12.0
                                    rust-futures-0.3.31
                                    rust-futures-channel-0.3.31
                                    rust-futures-core-0.3.31
                                    rust-futures-executor-0.3.31
                                    rust-futures-io-0.3.31
                                    rust-futures-macro-0.3.31
                                    rust-futures-sink-0.3.31
                                    rust-futures-task-0.3.31
                                    rust-futures-timer-3.0.3
                                    rust-futures-util-0.3.31
                                    rust-generic-array-0.14.7
                                    rust-getrandom-0.2.15
                                    rust-getrandom-0.3.2
                                    rust-glob-0.3.2
                                    rust-globset-0.4.16
                                    rust-goblin-0.9.3
                                    rust-hashbrown-0.14.5
                                    rust-hashbrown-0.15.2
                                    rust-heck-0.4.1
                                    rust-heck-0.5.0
                                    rust-humantime-2.2.0
                                    rust-humantime-serde-1.1.1
                                    rust-icu-collections-1.5.0
                                    rust-icu-locid-1.5.0
                                    rust-icu-locid-transform-1.5.0
                                    rust-icu-locid-transform-data-1.5.1
                                    rust-icu-normalizer-1.5.0
                                    rust-icu-normalizer-data-1.5.1
                                    rust-icu-properties-1.5.1
                                    rust-icu-properties-data-1.5.1
                                    rust-icu-provider-1.5.0
                                    rust-icu-provider-macros-1.5.0
                                    rust-idna-1.0.3
                                    rust-idna-adapter-1.2.0
                                    rust-ignore-0.4.23
                                    rust-indexmap-2.9.0
                                    rust-indoc-2.0.6
                                    rust-is-terminal-polyfill-1.70.1
                                    rust-itertools-0.12.1
                                    rust-itoa-1.0.15
                                    rust-keyring-2.3.3
                                    rust-lazy-static-1.5.0
                                    rust-lddtree-0.3.7
                                    rust-libc-0.2.171
                                    rust-libredox-0.1.3
                                    rust-linux-keyutils-0.2.4
                                    rust-linux-raw-sys-0.4.15
                                    rust-linux-raw-sys-0.9.3
                                    rust-litemap-0.7.5
                                    rust-lockfree-object-pool-0.1.6
                                    rust-log-0.4.27
                                    rust-mailparse-0.15.0
                                    rust-matchers-0.1.0
                                    rust-memchr-2.7.4
                                    rust-mime-0.3.17
                                    rust-mime-guess-2.0.5
                                    rust-minijinja-2.9.0
                                    rust-miniz-oxide-0.8.7
                                    rust-multipart-0.18.0
                                    rust-native-tls-0.2.14
                                    rust-normalize-line-endings-0.3.0
                                    rust-normpath-1.3.0
                                    rust-nu-ansi-term-0.46.0
                                    rust-num-conv-0.1.0
                                    rust-once-cell-1.21.3
                                    rust-openssl-0.10.72
                                    rust-openssl-macros-0.1.1
                                    rust-openssl-probe-0.1.6
                                    rust-openssl-sys-0.9.107
                                    rust-option-ext-0.2.0
                                    rust-os-pipe-1.2.1
                                    rust-overload-0.1.1
                                    rust-path-slash-0.2.1
                                    rust-pep440-rs-0.6.6
                                    rust-pep508-rs-0.6.1
                                    rust-percent-encoding-2.3.1
                                    rust-pin-project-lite-0.2.16
                                    rust-pin-utils-0.1.0
                                    rust-pkg-config-0.3.32
                                    rust-plain-0.2.3
                                    rust-platform-info-2.0.5
                                    rust-powerfmt-0.2.0
                                    rust-ppv-lite86-0.2.21
                                    rust-pretty-assertions-1.4.1
                                    rust-proc-macro-crate-3.3.0
                                    rust-proc-macro2-1.0.94
                                    rust-psm-0.1.25
                                    rust-pyproject-toml-0.11.0
                                    rust-python-pkginfo-0.6.5
                                    rust-quote-1.0.40
                                    rust-quoted-printable-0.5.1
                                    rust-r-efi-5.2.0
                                    rust-rand-0.8.5
                                    rust-rand-chacha-0.3.1
                                    rust-rand-core-0.6.4
                                    rust-rayon-1.10.0
                                    rust-rayon-core-1.12.1
                                    rust-redox-syscall-0.5.10
                                    rust-redox-users-0.4.6
                                    rust-regex-1.11.1
                                    rust-regex-automata-0.1.10
                                    rust-regex-automata-0.4.9
                                    rust-regex-syntax-0.6.29
                                    rust-regex-syntax-0.8.5
                                    rust-relative-path-1.9.3
                                    rust-rfc2047-decoder-1.0.6
                                    rust-ring-0.17.14
                                    rust-rstest-0.22.0
                                    rust-rstest-macros-0.22.0
                                    rust-rustc-version-0.4.1
                                    rust-rustix-0.38.44
                                    rust-rustix-1.0.5
                                    rust-rustls-0.23.25
                                    rust-rustls-pemfile-2.2.0
                                    rust-rustls-pki-types-1.11.0
                                    rust-rustls-webpki-0.103.1
                                    rust-rustversion-1.0.20
                                    rust-ryu-1.0.20
                                    rust-same-file-1.0.6
                                    rust-schannel-0.1.27
                                    rust-schemars-0.8.22
                                    rust-schemars-derive-0.8.22
                                    rust-scroll-0.12.0
                                    rust-scroll-derive-0.12.0
                                    rust-security-framework-2.11.1
                                    rust-security-framework-sys-2.14.0
                                    rust-semver-1.0.26
                                    rust-serde-1.0.219
                                    rust-serde-derive-1.0.219
                                    rust-serde-derive-internals-0.29.1
                                    rust-serde-json-1.0.140
                                    rust-serde-spanned-0.6.8
                                    rust-sha2-0.10.8
                                    rust-sharded-slab-0.1.7
                                    rust-shell-words-1.1.0
                                    rust-shlex-1.3.0
                                    rust-simd-adler32-0.3.7
                                    rust-similar-2.7.0
                                    rust-slab-0.4.9
                                    rust-smallvec-1.15.0
                                    rust-smawk-0.3.2
                                    rust-snapbox-0.6.21
                                    rust-snapbox-macros-0.3.10
                                    rust-socks-0.3.4
                                    rust-stable-deref-trait-1.2.0
                                    rust-stacker-0.1.20
                                    rust-strsim-0.11.1
                                    rust-subtle-2.6.1
                                    rust-syn-1.0.109
                                    rust-syn-2.0.100
                                    rust-synstructure-0.13.1
                                    rust-tar-0.4.44
                                    rust-target-lexicon-0.13.2
                                    rust-tempfile-3.19.1
                                    rust-terminal-size-0.4.2
                                    rust-textwrap-0.16.2
                                    rust-thiserror-1.0.69
                                    rust-thiserror-2.0.12
                                    rust-thiserror-impl-1.0.69
                                    rust-thiserror-impl-2.0.12
                                    rust-thread-local-1.1.8
                                    rust-time-0.3.41
                                    rust-time-core-0.1.4
                                    rust-time-macros-0.2.22
                                    rust-tinystr-0.7.6
                                    rust-toml-0.8.20
                                    rust-toml-datetime-0.6.8
                                    rust-toml-edit-0.22.24
                                    rust-tracing-0.1.41
                                    rust-tracing-attributes-0.1.28
                                    rust-tracing-core-0.1.33
                                    rust-tracing-log-0.2.0
                                    rust-tracing-subscriber-0.3.19
                                    rust-trycmd-0.15.9
                                    rust-typenum-1.18.0
                                    rust-unicase-2.8.1
                                    rust-unicode-ident-1.0.18
                                    rust-unicode-linebreak-0.1.5
                                    rust-unicode-width-0.1.14
                                    rust-unicode-width-0.2.0
                                    rust-unicode-xid-0.2.6
                                    rust-unscanny-0.1.0
                                    rust-untrusted-0.9.0
                                    rust-ureq-2.12.1
                                    rust-url-2.5.4
                                    rust-urlencoding-2.1.3
                                    rust-utf16-iter-1.0.5
                                    rust-utf8-iter-1.0.4
                                    rust-utf8parse-0.2.2
                                    rust-valuable-0.1.1
                                    rust-vcpkg-0.2.15
                                    rust-version-check-0.9.5
                                    rust-wait-timeout-0.2.1
                                    rust-walkdir-2.5.0
                                    rust-wasi-0.11.0+wasi-snapshot-preview1
                                    rust-wasi-0.14.2+wasi-0.2.4
                                    rust-webpki-roots-0.26.8
                                    rust-which-7.0.2
                                    rust-wild-2.2.1
                                    rust-winapi-0.3.9
                                    rust-winapi-i686-pc-windows-gnu-0.4.0
                                    rust-winapi-util-0.1.9
                                    rust-winapi-x86-64-pc-windows-gnu-0.4.0
                                    rust-windows-sys-0.48.0
                                    rust-windows-sys-0.52.0
                                    rust-windows-sys-0.59.0
                                    rust-windows-targets-0.48.5
                                    rust-windows-targets-0.52.6
                                    rust-windows-aarch64-gnullvm-0.48.5
                                    rust-windows-aarch64-gnullvm-0.52.6
                                    rust-windows-aarch64-msvc-0.48.5
                                    rust-windows-aarch64-msvc-0.52.6
                                    rust-windows-i686-gnu-0.48.5
                                    rust-windows-i686-gnu-0.52.6
                                    rust-windows-i686-gnullvm-0.52.6
                                    rust-windows-i686-msvc-0.48.5
                                    rust-windows-i686-msvc-0.52.6
                                    rust-windows-x86-64-gnu-0.48.5
                                    rust-windows-x86-64-gnu-0.52.6
                                    rust-windows-x86-64-gnullvm-0.48.5
                                    rust-windows-x86-64-gnullvm-0.52.6
                                    rust-windows-x86-64-msvc-0.48.5
                                    rust-windows-x86-64-msvc-0.52.6
                                    rust-winnow-0.7.4
                                    rust-winsafe-0.0.19
                                    rust-wit-bindgen-rt-0.39.0
                                    rust-write16-1.0.0
                                    rust-writeable-0.5.5
                                    rust-xattr-1.5.0
                                    rust-yansi-1.0.1
                                    rust-yoke-0.7.5
                                    rust-yoke-derive-0.7.5
                                    rust-zerocopy-0.7.35
                                    rust-zerocopy-0.8.24
                                    rust-zerocopy-derive-0.7.35
                                    rust-zerocopy-derive-0.8.24
                                    rust-zerofrom-0.1.6
                                    rust-zerofrom-derive-0.1.6
                                    rust-zeroize-1.8.1
                                    rust-zerovec-0.10.4
                                    rust-zerovec-derive-0.10.3
                                    rust-zip-2.6.1
                                    rust-zopfli-0.8.1))
                     (mudskipper =>
                                 (list rust-ahash-0.8.11
                                  rust-aho-corasick-1.1.3
                                  rust-ansi-term-0.12.1
                                  rust-anyhow-1.0.97
                                  rust-approx-0.5.1
                                  rust-atty-0.2.14
                                  rust-autocfg-1.4.0
                                  rust-bio-0.39.2
                                  rust-bio-types-0.13.0
                                  rust-bio-types-1.0.4
                                  rust-bit-set-0.5.3
                                  rust-bit-vec-0.6.3
                                  rust-bitflags-1.3.2
                                  rust-bitflags-2.9.0
                                  rust-bv-0.11.1
                                  rust-bytecount-0.6.8
                                  rust-byteorder-1.5.0
                                  rust-bzip2-sys-0.1.13+1.0.8
                                  rust-cc-1.2.18
                                  rust-cfg-if-1.0.0
                                  rust-clap-2.34.0
                                  rust-cmake-0.1.54
                                  rust-coitrees-0.2.1
                                  rust-console-0.15.11
                                  rust-core-foundation-sys-0.8.7
                                  rust-crossbeam-deque-0.8.6
                                  rust-crossbeam-epoch-0.9.18
                                  rust-crossbeam-utils-0.8.21
                                  rust-csv-1.3.1
                                  rust-csv-core-0.1.12
                                  rust-custom-derive-0.1.7
                                  rust-dashmap-5.5.3
                                  rust-derive-new-0.5.9
                                  rust-derive-new-0.6.0
                                  rust-displaydoc-0.2.5
                                  rust-either-1.15.0
                                  rust-encode-unicode-1.0.0
                                  rust-enum-map-1.1.1
                                  rust-enum-map-derive-0.6.0
                                  rust-env-logger-0.9.3
                                  rust-equivalent-1.0.2
                                  rust-errno-0.3.11
                                  rust-fastrand-2.3.0
                                  rust-feature-probe-0.1.1
                                  rust-fixedbitset-0.4.2
                                  rust-fnv-1.0.7
                                  rust-form-urlencoded-1.2.1
                                  rust-fs-utils-1.1.4
                                  rust-fxhash-0.2.1
                                  rust-getrandom-0.2.15
                                  rust-getrandom-0.3.2
                                  rust-getset-0.1.5
                                  rust-glob-0.3.2
                                  rust-hashbrown-0.14.5
                                  rust-hashbrown-0.15.2
                                  rust-heck-0.3.3
                                  rust-heck-0.4.1
                                  rust-heck-0.5.0
                                  rust-hermit-abi-0.1.19
                                  rust-hermit-abi-0.3.9
                                  rust-hts-sys-2.2.0
                                  rust-humantime-2.2.0
                                  rust-icu-collections-1.5.0
                                  rust-icu-locid-1.5.0
                                  rust-icu-locid-transform-1.5.0
                                  rust-icu-locid-transform-data-1.5.1
                                  rust-icu-normalizer-1.5.0
                                  rust-icu-normalizer-data-1.5.1
                                  rust-icu-properties-1.5.1
                                  rust-icu-properties-data-1.5.1
                                  rust-icu-provider-1.5.0
                                  rust-icu-provider-macros-1.5.0
                                  rust-idna-1.0.3
                                  rust-idna-adapter-1.2.0
                                  rust-ieee754-0.2.6
                                  rust-indexmap-2.9.0
                                  rust-indicatif-0.16.2
                                  rust-itertools-0.10.5
                                  rust-itertools-num-0.1.3
                                  rust-itoa-1.0.15
                                  rust-jobserver-0.1.33
                                  rust-lazy-static-1.5.0
                                  rust-libc-0.2.171
                                  rust-libm-0.2.11
                                  rust-libradicl-0.5.1
                                  rust-libz-sys-1.1.22
                                  rust-linear-map-1.2.0
                                  rust-linecount-0.1.0
                                  rust-linux-raw-sys-0.9.4
                                  rust-litemap-0.7.5
                                  rust-lock-api-0.4.12
                                  rust-log-0.4.27
                                  rust-lzma-sys-0.1.20
                                  rust-matrixmultiply-0.3.9
                                  rust-memchr-2.7.4
                                  rust-multimap-0.8.3
                                  rust-nalgebra-0.27.1
                                  rust-nalgebra-macros-0.1.0
                                  rust-ndarray-0.15.6
                                  rust-newtype-derive-0.1.6
                                  rust-ntapi-0.3.7
                                  rust-num-0.4.3
                                  rust-num-bigint-0.4.6
                                  rust-num-complex-0.4.6
                                  rust-num-integer-0.1.46
                                  rust-num-iter-0.1.45
                                  rust-num-rational-0.4.2
                                  rust-num-traits-0.2.19
                                  rust-num-cpus-1.16.0
                                  rust-number-prefix-0.4.0
                                  rust-once-cell-1.21.3
                                  rust-ordered-float-1.1.1
                                  rust-parking-lot-core-0.9.10
                                  rust-paste-1.0.15
                                  rust-percent-encoding-2.3.1
                                  rust-petgraph-0.6.5
                                  rust-pkg-config-0.3.32
                                  rust-ppv-lite86-0.2.21
                                  rust-proc-macro-error-attr2-2.0.0
                                  rust-proc-macro-error2-2.0.1
                                  rust-proc-macro2-1.0.94
                                  rust-quick-error-1.2.3
                                  rust-quote-1.0.40
                                  rust-r-efi-5.2.0
                                  rust-rand-0.8.5
                                  rust-rand-chacha-0.3.1
                                  rust-rand-core-0.6.4
                                  rust-rand-distr-0.4.3
                                  rust-rawpointer-0.2.1
                                  rust-rayon-1.10.0
                                  rust-rayon-core-1.12.1
                                  rust-redox-syscall-0.5.11
                                  rust-regex-1.11.1
                                  rust-regex-automata-0.4.9
                                  rust-regex-syntax-0.8.5
                                  rust-rust-htslib-0.39.5
                                  rust-rust-htslib-0.40.2
                                  rust-rustc-version-0.1.7
                                  rust-rustix-1.0.5
                                  rust-rustversion-1.0.20
                                  rust-ryu-1.0.20
                                  rust-scopeguard-1.2.0
                                  rust-scroll-0.11.0
                                  rust-semver-0.1.20
                                  rust-serde-1.0.219
                                  rust-serde-bytes-0.11.17
                                  rust-serde-derive-1.0.219
                                  rust-shlex-1.3.0
                                  rust-simba-0.5.1
                                  rust-smallvec-1.15.0
                                  rust-snap-1.1.1
                                  rust-stable-deref-trait-1.2.0
                                  rust-statrs-0.15.0
                                  rust-strsim-0.8.0
                                  rust-strum-0.23.0
                                  rust-strum-macros-0.23.1
                                  rust-strum-macros-0.24.3
                                  rust-strum-macros-0.26.4
                                  rust-syn-1.0.109
                                  rust-syn-2.0.100
                                  rust-synstructure-0.13.1
                                  rust-sysinfo-0.21.2
                                  rust-tempfile-3.19.1
                                  rust-termcolor-1.4.1
                                  rust-textwrap-0.11.0
                                  rust-thiserror-1.0.69
                                  rust-thiserror-impl-1.0.69
                                  rust-tinystr-0.7.6
                                  rust-triple-accel-0.4.0
                                  rust-typenum-1.18.0
                                  rust-unicode-ident-1.0.18
                                  rust-unicode-segmentation-1.12.0
                                  rust-unicode-width-0.1.14
                                  rust-url-2.5.4
                                  rust-utf16-iter-1.0.5
                                  rust-utf8-iter-1.0.4
                                  rust-vcpkg-0.2.15
                                  rust-vec-map-0.8.2
                                  rust-version-check-0.9.5
                                  rust-wasi-0.11.0+wasi-snapshot-preview1
                                  rust-wasi-0.14.2+wasi-0.2.4
                                  rust-winapi-0.3.9
                                  rust-winapi-i686-pc-windows-gnu-0.4.0
                                  rust-winapi-util-0.1.9
                                  rust-winapi-x86-64-pc-windows-gnu-0.4.0
                                  rust-windows-sys-0.59.0
                                  rust-windows-targets-0.52.6
                                  rust-windows-aarch64-gnullvm-0.52.6
                                  rust-windows-aarch64-msvc-0.52.6
                                  rust-windows-i686-gnu-0.52.6
                                  rust-windows-i686-gnullvm-0.52.6
                                  rust-windows-i686-msvc-0.52.6
                                  rust-windows-x86-64-gnu-0.52.6
                                  rust-windows-x86-64-gnullvm-0.52.6
                                  rust-windows-x86-64-msvc-0.52.6
                                  rust-wit-bindgen-rt-0.39.0
                                  rust-write16-1.0.0
                                  rust-writeable-0.5.5
                                  rust-yoke-0.7.5
                                  rust-yoke-derive-0.7.5
                                  rust-zerocopy-0.7.35
                                  rust-zerocopy-0.8.24
                                  rust-zerocopy-derive-0.7.35
                                  rust-zerocopy-derive-0.8.24
                                  rust-zerofrom-0.1.6
                                  rust-zerofrom-derive-0.1.6
                                  rust-zerovec-0.10.4
                                  rust-zerovec-derive-0.10.3))
                     (niri =>
                           (list rust-adler2-2.0.0
                                 rust-ahash-0.8.11
                                 rust-aho-corasick-1.1.3
                                 rust-allocator-api2-0.2.21
                                 rust-android-activity-0.6.0
                                 rust-android-properties-0.2.2
                                 rust-annotate-snippets-0.9.2
                                 rust-anstream-0.6.18
                                 rust-anstyle-1.0.10
                                 rust-anstyle-parse-0.2.6
                                 rust-anstyle-query-1.1.2
                                 rust-anstyle-wincon-3.0.7
                                 rust-anyhow-1.0.97
                                 rust-appendlist-1.4.0
                                 rust-approx-0.4.0
                                 rust-approx-0.5.1
                                 rust-arrayvec-0.7.6
                                 rust-as-raw-xcb-connection-1.0.1
                                 rust-async-broadcast-0.7.2
                                 rust-async-channel-2.3.1
                                 rust-async-executor-1.13.1
                                 rust-async-fs-2.1.2
                                 rust-async-io-2.4.0
                                 rust-async-lock-3.4.0
                                 rust-async-process-2.3.0
                                 rust-async-recursion-1.1.1
                                 rust-async-signal-0.2.10
                                 rust-async-task-4.7.1
                                 rust-async-trait-0.1.88
                                 rust-atomic-0.6.0
                                 rust-atomic-waker-1.1.2
                                 rust-autocfg-1.4.0
                                 rust-base64-0.21.7
                                 rust-bindgen-0.69.5
                                 rust-bit-set-0.8.0
                                 rust-bit-vec-0.8.0
                                 rust-bitflags-1.3.2
                                 rust-bitflags-2.9.0
                                 rust-block2-0.5.1
                                 rust-blocking-1.6.1
                                 rust-bumpalo-3.17.0
                                 rust-bytemuck-1.22.0
                                 rust-bytemuck-derive-1.9.3
                                 rust-bytes-1.10.1
                                 rust-cairo-rs-0.20.7
                                 rust-cairo-sys-rs-0.20.7
                                 rust-calloop-0.13.0
                                 rust-calloop-0.14.2
                                 rust-calloop-wayland-source-0.3.0
                                 rust-calloop-wayland-source-0.4.0
                                 rust-cc-1.2.18
                                 rust-cesu8-1.1.0
                                 rust-cexpr-0.6.0
                                 rust-cfg-expr-0.15.8
                                 rust-cfg-expr-0.17.2
                                 rust-cfg-if-1.0.0
                                 rust-cfg-aliases-0.2.1
                                 rust-cgmath-0.18.0
                                 rust-chumsky-0.9.3
                                 rust-clang-sys-1.8.1
                                 rust-clap-4.5.35
                                 rust-clap-builder-4.5.35
                                 rust-clap-derive-4.5.32
                                 rust-clap-lex-0.7.4
                                 rust-colorchoice-1.0.3
                                 rust-combine-4.6.7
                                 rust-concurrent-queue-2.5.0
                                 rust-console-0.15.11
                                 rust-convert-case-0.6.0
                                 rust-cookie-factory-0.3.3
                                 rust-core-foundation-0.9.4
                                 rust-core-foundation-sys-0.8.7
                                 rust-core-graphics-0.23.2
                                 rust-core-graphics-types-0.1.3
                                 rust-crc32fast-1.4.2
                                 rust-crossbeam-deque-0.8.6
                                 rust-crossbeam-epoch-0.9.18
                                 rust-crossbeam-utils-0.8.21
                                 rust-csscolorparser-0.7.0
                                 rust-cursor-icon-1.1.0
                                 rust-diff-0.1.13
                                 rust-directories-6.0.0
                                 rust-dirs-sys-0.5.0
                                 rust-dispatch-0.2.0
                                 rust-displaydoc-0.2.5
                                 rust-dlib-0.5.2
                                 rust-downcast-rs-1.2.1
                                 rust-dpi-0.1.1
                                 rust-drm-0.14.1
                                 rust-drm-ffi-0.9.0
                                 rust-drm-fourcc-2.2.0
                                 rust-drm-sys-0.8.0
                                 rust-dyn-clone-1.0.19
                                 rust-either-1.15.0
                                 rust-encode-unicode-1.0.0
                                 rust-endi-1.1.0
                                 rust-enumflags2-0.7.11
                                 rust-enumflags2-derive-0.7.11
                                 rust-equivalent-1.0.2
                                 rust-errno-0.3.11
                                 rust-event-listener-5.4.0
                                 rust-event-listener-strategy-0.5.4
                                 rust-fastrand-2.3.0
                                 rust-fdeflate-0.3.7
                                 rust-field-offset-0.3.6
                                 rust-flate2-1.1.1
                                 rust-fnv-1.0.7
                                 rust-foreign-types-0.5.0
                                 rust-foreign-types-macros-0.2.3
                                 rust-foreign-types-shared-0.3.1
                                 rust-form-urlencoded-1.2.1
                                 rust-futures-0.3.31
                                 rust-futures-channel-0.3.31
                                 rust-futures-core-0.3.31
                                 rust-futures-executor-0.3.31
                                 rust-futures-io-0.3.31
                                 rust-futures-lite-2.6.0
                                 rust-futures-macro-0.3.31
                                 rust-futures-sink-0.3.31
                                 rust-futures-task-0.3.31
                                 rust-futures-util-0.3.31
                                 rust-gbm-0.18.0
                                 rust-gbm-sys-0.4.0
                                 rust-gdk-pixbuf-0.20.9
                                 rust-gdk-pixbuf-sys-0.20.7
                                 rust-gdk4-0.9.6
                                 rust-gdk4-sys-0.9.6
                                 rust-generator-0.8.4
                                 rust-gethostname-0.4.3
                                 rust-getrandom-0.2.15
                                 rust-getrandom-0.3.2
                                 rust-gio-0.20.9
                                 rust-gio-sys-0.20.9
                                 rust-git-version-0.3.9
                                 rust-git-version-macro-0.3.9
                                 rust-gl-generator-0.14.0
                                 rust-glam-0.30.1
                                 rust-glib-0.20.9
                                 rust-glib-macros-0.20.7
                                 rust-glib-sys-0.20.9
                                 rust-glob-0.3.2
                                 rust-gobject-sys-0.20.9
                                 rust-graphene-rs-0.20.9
                                 rust-graphene-sys-0.20.7
                                 rust-gsk4-0.9.6
                                 rust-gsk4-sys-0.9.6
                                 rust-gtk4-0.9.6
                                 rust-gtk4-macros-0.9.5
                                 rust-gtk4-sys-0.9.6
                                 rust-hashbrown-0.14.5
                                 rust-hashbrown-0.15.2
                                 rust-heck-0.4.1
                                 rust-heck-0.5.0
                                 rust-hermit-abi-0.3.9
                                 rust-hermit-abi-0.4.0
                                 rust-hermit-abi-0.5.0
                                 rust-hex-0.4.3
                                 rust-icu-collections-1.5.0
                                 rust-icu-locid-1.5.0
                                 rust-icu-locid-transform-1.5.0
                                 rust-icu-locid-transform-data-1.5.1
                                 rust-icu-normalizer-1.5.0
                                 rust-icu-normalizer-data-1.5.1
                                 rust-icu-properties-1.5.1
                                 rust-icu-properties-data-1.5.1
                                 rust-icu-provider-1.5.0
                                 rust-icu-provider-macros-1.5.0
                                 rust-idna-1.0.3
                                 rust-idna-adapter-1.2.0
                                 rust-indexmap-2.9.0
                                 rust-input-0.9.1
                                 rust-input-sys-1.18.0
                                 rust-insta-1.42.2
                                 rust-io-lifetimes-1.0.11
                                 rust-is-terminal-0.4.16
                                 rust-is-ci-1.2.0
                                 rust-is-terminal-polyfill-1.70.1
                                 rust-itertools-0.12.1
                                 rust-itoa-1.0.15
                                 rust-jni-0.21.1
                                 rust-jni-sys-0.3.0
                                 rust-jobserver-0.1.33
                                 rust-js-sys-0.3.77
                                 rust-keyframe-1.1.1
                                 rust-khronos-api-3.1.0
                                 rust-knuffel-3.2.0
                                 rust-knuffel-derive-3.2.0
                                 rust-lazy-static-1.5.0
                                 rust-lazycell-1.3.0
                                 rust-libadwaita-0.7.2
                                 rust-libadwaita-sys-0.7.2
                                 rust-libc-0.2.171
                                 rust-libdisplay-info-0.2.2
                                 rust-libdisplay-info-derive-0.1.0
                                 rust-libdisplay-info-sys-0.2.2
                                 rust-libloading-0.8.6
                                 rust-libm-0.2.11
                                 rust-libredox-0.1.3
                                 rust-libseat-0.2.3
                                 rust-libseat-sys-0.1.9
                                 rust-libspa-0.8.0.fd3d8f7
                                 rust-libspa-sys-0.8.0.fd3d8f7
                                 rust-libudev-sys-0.1.4
                                 rust-linked-hash-map-0.5.6
                                 rust-linux-raw-sys-0.4.15
                                 rust-linux-raw-sys-0.6.5
                                 rust-linux-raw-sys-0.9.3
                                 rust-litemap-0.7.5
                                 rust-log-0.4.27
                                 rust-loom-0.7.2
                                 rust-matchers-0.1.0
                                 rust-memchr-2.7.4
                                 rust-memmap2-0.9.5
                                 rust-memoffset-0.9.1
                                 rust-miette-5.10.0
                                 rust-miette-derive-5.10.0
                                 rust-minimal-lexical-0.2.1
                                 rust-miniz-oxide-0.8.7
                                 rust-ndk-0.9.0
                                 rust-ndk-context-0.1.1
                                 rust-ndk-sys-0.6.0+11769913
                                 rust-nix-0.29.0
                                 rust-nom-7.1.3
                                 rust-nu-ansi-term-0.46.0
                                 rust-num-traits-0.2.19
                                 rust-num-enum-0.7.3
                                 rust-num-enum-derive-0.7.3
                                 rust-objc-sys-0.3.5
                                 rust-objc2-0.5.2
                                 rust-objc2-app-kit-0.2.2
                                 rust-objc2-cloud-kit-0.2.2
                                 rust-objc2-contacts-0.2.2
                                 rust-objc2-core-data-0.2.2
                                 rust-objc2-core-image-0.2.2
                                 rust-objc2-core-location-0.2.2
                                 rust-objc2-encode-4.1.0
                                 rust-objc2-foundation-0.2.2
                                 rust-objc2-link-presentation-0.2.2
                                 rust-objc2-metal-0.2.2
                                 rust-objc2-quartz-core-0.2.2
                                 rust-objc2-symbols-0.2.2
                                 rust-objc2-ui-kit-0.2.2
                                 rust-objc2-uniform-type-identifiers-0.2.2
                                 rust-objc2-user-notifications-0.2.2
                                 rust-once-cell-1.21.3
                                 rust-option-ext-0.2.0
                                 rust-orbclient-0.3.48
                                 rust-ordered-float-5.0.0
                                 rust-ordered-stream-0.2.0
                                 rust-overload-0.1.1
                                 rust-owo-colors-3.5.0
                                 rust-pango-0.20.9
                                 rust-pango-sys-0.20.9
                                 rust-pangocairo-0.20.7
                                 rust-pangocairo-sys-0.20.7
                                 rust-parking-2.2.1
                                 rust-paste-1.0.15
                                 rust-percent-encoding-2.3.1
                                 rust-phf-0.11.3
                                 rust-phf-generator-0.11.3
                                 rust-phf-macros-0.11.3
                                 rust-phf-shared-0.11.3
                                 rust-pin-project-1.1.10
                                 rust-pin-project-internal-1.1.10
                                 rust-pin-project-lite-0.2.16
                                 rust-pin-utils-0.1.0
                                 rust-piper-0.2.4
                                 rust-pipewire-0.8.0.fd3d8f7
                                 rust-pipewire-sys-0.8.0.fd3d8f7
                                 rust-pixman-0.2.1
                                 rust-pixman-sys-0.1.0
                                 rust-pkg-config-0.3.32
                                 rust-png-0.17.16
                                 rust-polling-3.7.4
                                 rust-portable-atomic-1.11.0
                                 rust-ppv-lite86-0.2.21
                                 rust-pretty-assertions-1.4.1
                                 rust-proc-macro-crate-3.3.0
                                 rust-proc-macro-error-1.0.4
                                 rust-proc-macro-error-attr-1.0.4
                                 rust-proc-macro2-1.0.94
                                 rust-profiling-1.0.16
                                 rust-profiling-procmacros-1.0.16
                                 rust-proptest-1.6.0
                                 rust-proptest-derive-0.5.1
                                 rust-quick-error-1.2.3
                                 rust-quick-xml-0.37.4
                                 rust-quote-1.0.40
                                 rust-r-efi-5.2.0
                                 rust-rand-0.8.5
                                 rust-rand-chacha-0.3.1
                                 rust-rand-core-0.6.4
                                 rust-rand-xorshift-0.3.0
                                 rust-raw-window-handle-0.6.2
                                 rust-rayon-1.10.0
                                 rust-rayon-core-1.12.1
                                 rust-redox-syscall-0.4.1
                                 rust-redox-syscall-0.5.11
                                 rust-redox-users-0.5.0
                                 rust-regex-1.11.1
                                 rust-regex-automata-0.1.10
                                 rust-regex-automata-0.4.9
                                 rust-regex-syntax-0.6.29
                                 rust-regex-syntax-0.8.5
                                 rust-rustc-hash-1.1.0
                                 rust-rustc-version-0.4.1
                                 rust-rustix-0.38.44
                                 rust-rustix-1.0.5
                                 rust-rustversion-1.0.20
                                 rust-rusty-fork-0.3.0
                                 rust-ryu-1.0.20
                                 rust-same-file-1.0.6
                                 rust-schemars-0.8.22
                                 rust-schemars-derive-0.8.22
                                 rust-scoped-tls-1.0.1
                                 rust-sd-notify-0.4.5
                                 rust-semver-1.0.26
                                 rust-serde-1.0.219
                                 rust-serde-derive-1.0.219
                                 rust-serde-derive-internals-0.29.1
                                 rust-serde-json-1.0.140
                                 rust-serde-repr-0.1.20
                                 rust-serde-spanned-0.6.8
                                 rust-sharded-slab-0.1.7
                                 rust-shlex-1.3.0
                                 rust-signal-hook-registry-1.4.2
                                 rust-simd-adler32-0.3.7
                                 rust-similar-2.7.0
                                 rust-siphasher-1.0.1
                                 rust-slab-0.4.9
                                 rust-smallvec-1.15.0
                                 rust-smawk-0.3.2
                                 rust-smithay-0.4.0.0cd3345
                                 rust-smithay-client-toolkit-0.19.2
                                 rust-smithay-drm-extras-0.1.0.0cd3345
                                 rust-smol-str-0.2.2
                                 rust-stable-deref-trait-1.2.0
                                 rust-static-assertions-1.1.0
                                 rust-strsim-0.11.1
                                 rust-supports-color-2.1.0
                                 rust-supports-hyperlinks-2.1.0
                                 rust-supports-unicode-2.1.0
                                 rust-syn-1.0.109
                                 rust-syn-2.0.100
                                 rust-synstructure-0.13.1
                                 rust-system-deps-6.2.2
                                 rust-system-deps-7.0.3
                                 rust-target-lexicon-0.12.16
                                 rust-tempfile-3.19.1
                                 rust-terminal-size-0.1.17
                                 rust-textwrap-0.15.2
                                 rust-thiserror-1.0.69
                                 rust-thiserror-2.0.12
                                 rust-thiserror-impl-1.0.69
                                 rust-thiserror-impl-2.0.12
                                 rust-thread-local-1.1.8
                                 rust-tinystr-0.7.6
                                 rust-toml-0.8.20
                                 rust-toml-datetime-0.6.8
                                 rust-toml-edit-0.22.24
                                 rust-tracing-0.1.41
                                 rust-tracing-attributes-0.1.28
                                 rust-tracing-core-0.1.33
                                 rust-tracing-log-0.2.0
                                 rust-tracing-subscriber-0.3.19
                                 rust-tracy-client-0.17.6
                                 rust-tracy-client-0.18.0
                                 rust-tracy-client-sys-0.24.3
                                 rust-udev-0.9.3
                                 rust-uds-windows-1.1.0
                                 rust-unarray-0.1.4
                                 rust-unicode-ident-1.0.18
                                 rust-unicode-linebreak-0.1.5
                                 rust-unicode-segmentation-1.12.0
                                 rust-unicode-width-0.1.14
                                 rust-url-2.5.4
                                 rust-utf16-iter-1.0.5
                                 rust-utf8-iter-1.0.4
                                 rust-utf8parse-0.2.2
                                 rust-valuable-0.1.1
                                 rust-version-compare-0.2.0
                                 rust-version-check-0.9.5
                                 rust-wait-timeout-0.2.1
                                 rust-walkdir-2.5.0
                                 rust-wasi-0.11.0+wasi-snapshot-preview1
                                 rust-wasi-0.14.2+wasi-0.2.4
                                 rust-wasm-bindgen-0.2.100
                                 rust-wasm-bindgen-backend-0.2.100
                                 rust-wasm-bindgen-futures-0.4.50
                                 rust-wasm-bindgen-macro-0.2.100
                                 rust-wasm-bindgen-macro-support-0.2.100
                                 rust-wasm-bindgen-shared-0.2.100
                                 rust-wayland-backend-0.3.8
                                 rust-wayland-client-0.31.8
                                 rust-wayland-csd-frame-0.3.0
                                 rust-wayland-cursor-0.31.8
                                 rust-wayland-egl-0.32.5
                                 rust-wayland-protocols-0.32.6
                                 rust-wayland-protocols-misc-0.3.6
                                 rust-wayland-protocols-plasma-0.3.6
                                 rust-wayland-protocols-wlr-0.3.6
                                 rust-wayland-scanner-0.31.6
                                 rust-wayland-server-0.31.7
                                 rust-wayland-sys-0.31.6
                                 rust-web-sys-0.3.77
                                 rust-web-time-1.1.0
                                 rust-winapi-0.3.9
                                 rust-winapi-i686-pc-windows-gnu-0.4.0
                                 rust-winapi-util-0.1.9
                                 rust-winapi-x86-64-pc-windows-gnu-0.4.0
                                 rust-windows-0.58.0
                                 rust-windows-core-0.58.0
                                 rust-windows-implement-0.58.0
                                 rust-windows-interface-0.58.0
                                 rust-windows-result-0.2.0
                                 rust-windows-strings-0.1.0
                                 rust-windows-sys-0.45.0
                                 rust-windows-sys-0.48.0
                                 rust-windows-sys-0.52.0
                                 rust-windows-sys-0.59.0
                                 rust-windows-targets-0.42.2
                                 rust-windows-targets-0.48.5
                                 rust-windows-targets-0.52.6
                                 rust-windows-aarch64-gnullvm-0.42.2
                                 rust-windows-aarch64-gnullvm-0.48.5
                                 rust-windows-aarch64-gnullvm-0.52.6
                                 rust-windows-aarch64-msvc-0.42.2
                                 rust-windows-aarch64-msvc-0.48.5
                                 rust-windows-aarch64-msvc-0.52.6
                                 rust-windows-i686-gnu-0.42.2
                                 rust-windows-i686-gnu-0.48.5
                                 rust-windows-i686-gnu-0.52.6
                                 rust-windows-i686-gnullvm-0.52.6
                                 rust-windows-i686-msvc-0.42.2
                                 rust-windows-i686-msvc-0.48.5
                                 rust-windows-i686-msvc-0.52.6
                                 rust-windows-x86-64-gnu-0.42.2
                                 rust-windows-x86-64-gnu-0.48.5
                                 rust-windows-x86-64-gnu-0.52.6
                                 rust-windows-x86-64-gnullvm-0.42.2
                                 rust-windows-x86-64-gnullvm-0.48.5
                                 rust-windows-x86-64-gnullvm-0.52.6
                                 rust-windows-x86-64-msvc-0.42.2
                                 rust-windows-x86-64-msvc-0.48.5
                                 rust-windows-x86-64-msvc-0.52.6
                                 rust-winit-0.30.9
                                 rust-winnow-0.7.4
                                 rust-wit-bindgen-rt-0.39.0
                                 rust-write16-1.0.0
                                 rust-writeable-0.5.5
                                 rust-x11-dl-2.21.0
                                 rust-x11rb-0.13.1
                                 rust-x11rb-protocol-0.13.1
                                 rust-xcursor-0.3.8
                                 rust-xdg-home-1.3.0
                                 rust-xkbcommon-0.8.0
                                 rust-xkbcommon-dl-0.4.2
                                 rust-xkeysym-0.2.1
                                 rust-xml-rs-0.8.25
                                 rust-xshell-0.2.7
                                 rust-xshell-macros-0.2.7
                                 rust-yansi-1.0.1
                                 rust-yansi-term-0.1.2
                                 rust-yoke-0.7.5
                                 rust-yoke-derive-0.7.5
                                 rust-zbus-5.5.0
                                 rust-zbus-macros-5.5.0
                                 rust-zbus-names-4.2.0
                                 rust-zerocopy-0.7.35
                                 rust-zerocopy-0.8.24
                                 rust-zerocopy-derive-0.7.35
                                 rust-zerocopy-derive-0.8.24
                                 rust-zerofrom-0.1.6
                                 rust-zerofrom-derive-0.1.6
                                 rust-zerovec-0.10.4
                                 rust-zerovec-derive-0.10.3
                                 rust-zvariant-5.4.0
                                 rust-zvariant-derive-5.4.0
                                 rust-zvariant-utils-3.2.0))
                     (nsncd =>
                            (list rust-aho-corasick-1.1.3
                                  rust-anes-0.1.6
                                  rust-anstyle-1.0.10
                                  rust-anyhow-1.0.97
                                  rust-atoi-2.0.0
                                  rust-autocfg-1.4.0
                                  rust-bitflags-2.9.0
                                  rust-bumpalo-3.17.0
                                  rust-cast-0.3.0
                                  rust-cfg-if-1.0.0
                                  rust-cfg-aliases-0.1.1
                                  rust-ciborium-0.2.2
                                  rust-ciborium-io-0.2.2
                                  rust-ciborium-ll-0.2.2
                                  rust-clap-4.5.35
                                  rust-clap-builder-4.5.35
                                  rust-clap-lex-0.7.4
                                  rust-criterion-0.5.1
                                  rust-criterion-plot-0.5.0
                                  rust-crossbeam-channel-0.5.15
                                  rust-crossbeam-deque-0.8.6
                                  rust-crossbeam-epoch-0.9.18
                                  rust-crossbeam-utils-0.8.21
                                  rust-crunchy-0.2.3
                                  rust-deranged-0.4.0
                                  rust-dirs-next-2.0.0
                                  rust-dirs-sys-next-0.1.2
                                  rust-dns-lookup-2.0.4
                                  rust-either-1.15.0
                                  rust-getrandom-0.2.15
                                  rust-half-2.6.0
                                  rust-hermit-abi-0.5.0
                                  rust-is-terminal-0.4.16
                                  rust-itertools-0.10.5
                                  rust-itoa-1.0.15
                                  rust-js-sys-0.3.77
                                  rust-libc-0.2.171
                                  rust-libredox-0.1.3
                                  rust-lock-api-0.4.12
                                  rust-log-0.4.27
                                  rust-memchr-2.7.4
                                  rust-memoffset-0.9.1
                                  rust-nix-0.28.0
                                  rust-num-conv-0.1.0
                                  rust-num-derive-0.3.3
                                  rust-num-traits-0.2.19
                                  rust-once-cell-1.21.3
                                  rust-oorandom-11.1.5
                                  rust-parking-lot-0.12.3
                                  rust-parking-lot-core-0.9.10
                                  rust-plotters-0.3.7
                                  rust-plotters-backend-0.3.7
                                  rust-plotters-svg-0.3.7
                                  rust-powerfmt-0.2.0
                                  rust-proc-macro2-1.0.94
                                  rust-quote-1.0.40
                                  rust-rayon-1.10.0
                                  rust-rayon-core-1.12.1
                                  rust-redox-syscall-0.5.11
                                  rust-redox-users-0.4.6
                                  rust-regex-1.11.1
                                  rust-regex-automata-0.4.9
                                  rust-regex-syntax-0.8.5
                                  rust-rustversion-1.0.20
                                  rust-ryu-1.0.20
                                  rust-same-file-1.0.6
                                  rust-scopeguard-1.2.0
                                  rust-sd-notify-0.4.5
                                  rust-serde-1.0.219
                                  rust-serde-derive-1.0.219
                                  rust-serde-json-1.0.140
                                  rust-slog-2.7.0
                                  rust-slog-async-2.8.0
                                  rust-slog-term-2.9.1
                                  rust-smallvec-1.15.0
                                  rust-socket2-0.5.9
                                  rust-static-assertions-1.1.0
                                  rust-syn-1.0.109
                                  rust-syn-2.0.100
                                  rust-take-mut-0.2.2
                                  rust-temp-env-0.3.6
                                  rust-term-0.7.0
                                  rust-thiserror-1.0.69
                                  rust-thiserror-impl-1.0.69
                                  rust-thread-local-1.1.8
                                  rust-time-0.3.41
                                  rust-time-core-0.1.4
                                  rust-time-macros-0.2.22
                                  rust-tinytemplate-1.2.1
                                  rust-unicode-ident-1.0.18
                                  rust-walkdir-2.5.0
                                  rust-wasi-0.11.0+wasi-snapshot-preview1
                                  rust-wasm-bindgen-0.2.100
                                  rust-wasm-bindgen-backend-0.2.100
                                  rust-wasm-bindgen-macro-0.2.100
                                  rust-wasm-bindgen-macro-support-0.2.100
                                  rust-wasm-bindgen-shared-0.2.100
                                  rust-web-sys-0.3.77
                                  rust-winapi-0.3.9
                                  rust-winapi-i686-pc-windows-gnu-0.4.0
                                  rust-winapi-util-0.1.9
                                  rust-winapi-x86-64-pc-windows-gnu-0.4.0
                                  rust-windows-sys-0.48.0
                                  rust-windows-sys-0.52.0
                                  rust-windows-sys-0.59.0
                                  rust-windows-targets-0.48.5
                                  rust-windows-targets-0.52.6
                                  rust-windows-aarch64-gnullvm-0.48.5
                                  rust-windows-aarch64-gnullvm-0.52.6
                                  rust-windows-aarch64-msvc-0.48.5
                                  rust-windows-aarch64-msvc-0.52.6
                                  rust-windows-i686-gnu-0.48.5
                                  rust-windows-i686-gnu-0.52.6
                                  rust-windows-i686-gnullvm-0.52.6
                                  rust-windows-i686-msvc-0.48.5
                                  rust-windows-i686-msvc-0.52.6
                                  rust-windows-x86-64-gnu-0.48.5
                                  rust-windows-x86-64-gnu-0.52.6
                                  rust-windows-x86-64-gnullvm-0.48.5
                                  rust-windows-x86-64-gnullvm-0.52.6
                                  rust-windows-x86-64-msvc-0.48.5
                                  rust-windows-x86-64-msvc-0.52.6))
                     (nushell =>
                              (list rust-addr2line-0.24.2
                                    rust-adler2-2.0.0
                                    rust-ahash-0.8.11
                                    rust-aho-corasick-1.1.3
                                    rust-alloc-no-stdlib-2.0.4
                                    rust-alloc-stdlib-0.2.2
                                    rust-alloca-0.4.0
                                    rust-allocator-api2-0.2.21
                                    rust-alphanumeric-sort-1.5.3
                                    rust-android-tzdata-0.1.1
                                    rust-android-system-properties-0.1.5
                                    rust-ansi-str-0.8.0
                                    rust-ansitok-0.2.0
                                    rust-anstream-0.6.18
                                    rust-anstyle-1.0.10
                                    rust-anstyle-parse-0.2.6
                                    rust-anstyle-query-1.1.2
                                    rust-anstyle-wincon-3.0.7
                                    rust-anyhow-1.0.97
                                    rust-arbitrary-1.4.1
                                    rust-arboard-3.5.0
                                    rust-arrayvec-0.5.2
                                    rust-arrayvec-0.7.6
                                    rust-assert-cmd-2.0.16
                                    rust-atomic-0.6.0
                                    rust-autocfg-1.4.0
                                    rust-backtrace-0.3.74
                                    rust-backtrace-ext-0.2.1
                                    rust-base64-0.22.1
                                    rust-bindgen-0.70.1
                                    rust-bit-set-0.8.0
                                    rust-bit-vec-0.8.0
                                    rust-bitflags-1.3.2
                                    rust-bitflags-2.9.0
                                    rust-block-buffer-0.10.4
                                    rust-block2-0.5.1
                                    rust-bracoxide-0.1.5
                                    rust-brotli-7.0.0
                                    rust-brotli-decompressor-4.0.2
                                    rust-bstr-1.12.0
                                    rust-bumpalo-3.17.0
                                    rust-bytecount-0.6.8
                                    rust-bytemuck-1.22.0
                                    rust-byteorder-1.5.0
                                    rust-bytes-1.10.1
                                    rust-bytesize-1.3.3
                                    rust-calamine-0.26.1
                                    rust-cassowary-0.3.0
                                    rust-castaway-0.2.3
                                    rust-cc-1.2.18
                                    rust-cexpr-0.6.0
                                    rust-cfg-if-1.0.0
                                    rust-cfg-aliases-0.2.1
                                    rust-chardetng-0.1.17
                                    rust-chrono-0.4.40
                                    rust-chrono-humanize-0.2.3
                                    rust-chrono-tz-0.10.3
                                    rust-chrono-tz-build-0.4.1
                                    rust-clang-sys-1.8.1
                                    rust-clap-4.5.35
                                    rust-clap-builder-4.5.35
                                    rust-clap-derive-4.5.32
                                    rust-clap-lex-0.7.4
                                    rust-clipboard-win-5.4.0
                                    rust-codepage-0.1.2
                                    rust-colorchoice-1.0.3
                                    rust-colorz-1.1.4
                                    rust-compact-str-0.8.1
                                    rust-console-0.15.11
                                    rust-const-format-0.2.34
                                    rust-const-format-proc-macros-0.2.34
                                    rust-core-foundation-0.9.4
                                    rust-core-foundation-sys-0.8.7
                                    rust-cpufeatures-0.2.17
                                    rust-crc32fast-1.4.2
                                    rust-crossbeam-channel-0.5.15
                                    rust-crossbeam-deque-0.8.6
                                    rust-crossbeam-epoch-0.9.18
                                    rust-crossbeam-utils-0.8.21
                                    rust-crossterm-0.28.1
                                    rust-crossterm-winapi-0.9.1
                                    rust-crypto-common-0.1.6
                                    rust-csv-1.3.1
                                    rust-csv-core-0.1.12
                                    rust-ctrlc-3.4.6
                                    rust-darling-0.20.11
                                    rust-darling-core-0.20.11
                                    rust-darling-macro-0.20.11
                                    rust-data-encoding-2.8.0
                                    rust-deranged-0.4.0
                                    rust-derive-arbitrary-1.4.1
                                    rust-devicons-0.6.12
                                    rust-dialoguer-0.11.0
                                    rust-diff-0.1.13
                                    rust-difflib-0.4.0
                                    rust-digest-0.10.7
                                    rust-dirs-5.0.1
                                    rust-dirs-sys-0.4.1
                                    rust-displaydoc-0.2.5
                                    rust-doc-comment-0.3.3
                                    rust-doctest-file-1.0.0
                                    rust-downcast-rs-1.2.1
                                    rust-dtparse-2.0.1
                                    rust-dunce-1.0.5
                                    rust-either-1.15.0
                                    rust-encode-unicode-1.0.0
                                    rust-encoding-rs-0.8.35
                                    rust-env-home-0.1.0
                                    rust-equivalent-1.0.2
                                    rust-erased-serde-0.4.6
                                    rust-errno-0.3.11
                                    rust-error-code-3.3.1
                                    rust-etcetera-0.8.0
                                    rust-fallible-iterator-0.3.0
                                    rust-fallible-streaming-iterator-0.1.9
                                    rust-fancy-regex-0.14.0
                                    rust-fastrand-2.3.0
                                    rust-fd-lock-4.0.4
                                    rust-file-id-0.2.2
                                    rust-filesize-0.2.0
                                    rust-filetime-0.2.25
                                    rust-fixedbitset-0.4.2
                                    rust-flate2-1.1.1
                                    rust-fluent-uri-0.1.4
                                    rust-fnv-1.0.7
                                    rust-foldhash-0.1.5
                                    rust-foreign-types-0.3.2
                                    rust-foreign-types-shared-0.1.1
                                    rust-form-urlencoded-1.2.1
                                    rust-fs-extra-1.3.0
                                    rust-fsevent-sys-4.1.0
                                    rust-futures-0.3.31
                                    rust-futures-channel-0.3.31
                                    rust-futures-core-0.3.31
                                    rust-futures-executor-0.3.31
                                    rust-futures-io-0.3.31
                                    rust-futures-macro-0.3.31
                                    rust-futures-sink-0.3.31
                                    rust-futures-task-0.3.31
                                    rust-futures-util-0.3.31
                                    rust-fuzzy-matcher-0.3.7
                                    rust-generic-array-0.14.7
                                    rust-gethostname-0.4.3
                                    rust-getrandom-0.2.15
                                    rust-getrandom-0.3.2
                                    rust-gimli-0.31.1
                                    rust-glob-0.3.2
                                    rust-glob-match-0.2.1
                                    rust-goblin-0.7.1
                                    rust-hashbrown-0.14.5
                                    rust-hashbrown-0.15.2
                                    rust-hashlink-0.9.1
                                    rust-heck-0.5.0
                                    rust-hex-0.4.3
                                    rust-home-0.5.11
                                    rust-http-1.3.1
                                    rust-http-body-1.0.1
                                    rust-http-body-util-0.1.3
                                    rust-httparse-1.10.1
                                    rust-human-date-parser-0.2.0
                                    rust-hyper-1.6.0
                                    rust-hyper-tls-0.6.0
                                    rust-hyper-util-0.1.11
                                    rust-iana-time-zone-0.1.63
                                    rust-iana-time-zone-haiku-0.1.2
                                    rust-icu-collections-1.5.0
                                    rust-icu-locid-1.5.0
                                    rust-icu-locid-transform-1.5.0
                                    rust-icu-locid-transform-data-1.5.1
                                    rust-icu-normalizer-1.5.0
                                    rust-icu-normalizer-data-1.5.1
                                    rust-icu-properties-1.5.1
                                    rust-icu-properties-data-1.5.1
                                    rust-icu-provider-1.5.0
                                    rust-icu-provider-macros-1.5.0
                                    rust-ident-case-1.0.1
                                    rust-idna-1.0.3
                                    rust-idna-adapter-1.2.0
                                    rust-indexmap-2.9.0
                                    rust-indicatif-0.17.11
                                    rust-indoc-2.0.6
                                    rust-inotify-0.9.6
                                    rust-inotify-sys-0.1.5
                                    rust-instability-0.3.7
                                    rust-interprocess-2.2.3
                                    rust-inventory-0.3.20
                                    rust-ipnet-2.11.0
                                    rust-is-docker-0.2.0
                                    rust-is-wsl-0.4.0
                                    rust-is-ci-1.2.0
                                    rust-is-debug-1.1.0
                                    rust-is-executable-1.0.4
                                    rust-is-terminal-polyfill-1.70.1
                                    rust-itertools-0.11.0
                                    rust-itertools-0.13.0
                                    rust-itoa-1.0.15
                                    rust-js-sys-0.3.77
                                    rust-kqueue-1.0.8
                                    rust-kqueue-sys-1.0.4
                                    rust-lazy-static-1.5.0
                                    rust-libc-0.2.171
                                    rust-libloading-0.8.6
                                    rust-libproc-0.14.10
                                    rust-libredox-0.1.3
                                    rust-libsqlite3-sys-0.28.0
                                    rust-linked-hash-map-0.5.6
                                    rust-linux-raw-sys-0.4.15
                                    rust-linux-raw-sys-0.9.4
                                    rust-litemap-0.7.5
                                    rust-lock-api-0.4.12
                                    rust-lockfree-object-pool-0.1.6
                                    rust-log-0.4.27
                                    rust-lru-0.12.5
                                    rust-lscolors-0.17.0
                                    rust-lsp-server-0.7.8
                                    rust-lsp-textdocument-0.4.2
                                    rust-lsp-types-0.97.0
                                    rust-mach2-0.4.2
                                    rust-md-5-0.10.6
                                    rust-memchr-2.7.4
                                    rust-miette-7.5.0
                                    rust-miette-derive-7.5.0
                                    rust-mime-0.3.17
                                    rust-mime-guess-2.0.5
                                    rust-minimal-lexical-0.2.1
                                    rust-miniz-oxide-0.8.8
                                    rust-mio-0.8.11
                                    rust-mio-1.0.3
                                    rust-multipart-rs-0.1.13
                                    rust-native-tls-0.2.14
                                    rust-nix-0.29.0
                                    rust-nom-7.1.3
                                    rust-notify-6.1.1
                                    rust-notify-debouncer-full-0.3.2
                                    rust-ntapi-0.4.1
                                    rust-nu-ansi-term-0.50.1
                                    rust-nu-cli-0.103.0
                                    rust-nu-cmd-base-0.103.0
                                    rust-nu-cmd-extra-0.103.0
                                    rust-nu-cmd-lang-0.103.0
                                    rust-nu-cmd-plugin-0.103.0
                                    rust-nu-color-config-0.103.0
                                    rust-nu-command-0.103.0
                                    rust-nu-derive-value-0.103.0
                                    rust-nu-engine-0.103.0
                                    rust-nu-explore-0.103.0
                                    rust-nu-glob-0.103.0
                                    rust-nu-json-0.103.0
                                    rust-nu-lsp-0.103.0
                                    rust-nu-parser-0.103.0
                                    rust-nu-path-0.103.0
                                    rust-nu-plugin-core-0.103.0
                                    rust-nu-plugin-engine-0.103.0
                                    rust-nu-plugin-protocol-0.103.0
                                    rust-nu-pretty-hex-0.103.0
                                    rust-nu-protocol-0.103.0
                                    rust-nu-std-0.103.0
                                    rust-nu-system-0.103.0
                                    rust-nu-table-0.103.0
                                    rust-nu-term-grid-0.103.0
                                    rust-nu-test-support-0.103.0
                                    rust-nu-utils-0.103.0
                                    rust-nucleo-matcher-0.3.1
                                    rust-num-conv-0.1.0
                                    rust-num-format-0.4.4
                                    rust-num-traits-0.2.19
                                    rust-num-threads-0.1.7
                                    rust-number-prefix-0.4.0
                                    rust-nuon-0.103.0
                                    rust-objc-sys-0.3.5
                                    rust-objc2-0.5.2
                                    rust-objc2-0.6.0
                                    rust-objc2-app-kit-0.3.0
                                    rust-objc2-core-foundation-0.3.0
                                    rust-objc2-core-graphics-0.3.0
                                    rust-objc2-encode-4.1.0
                                    rust-objc2-foundation-0.2.2
                                    rust-objc2-foundation-0.3.0
                                    rust-objc2-io-surface-0.3.0
                                    rust-object-0.36.7
                                    rust-oem-cp-2.0.0
                                    rust-omnipath-0.1.6
                                    rust-once-cell-1.21.3
                                    rust-open-5.3.2
                                    rust-openssl-0.10.72
                                    rust-openssl-macros-0.1.1
                                    rust-openssl-probe-0.1.6
                                    rust-openssl-src-300.5.0+3.5.0
                                    rust-openssl-sys-0.9.107
                                    rust-option-ext-0.2.0
                                    rust-os-display-0.1.4
                                    rust-os-pipe-1.2.1
                                    rust-owo-colors-4.2.0
                                    rust-papergrid-0.13.0
                                    rust-parking-lot-0.12.3
                                    rust-parking-lot-core-0.9.10
                                    rust-parse-zoneinfo-0.3.1
                                    rust-parse-datetime-0.6.0
                                    rust-paste-1.0.15
                                    rust-pathdiff-0.2.3
                                    rust-percent-encoding-2.3.1
                                    rust-pest-2.8.0
                                    rust-pest-derive-2.8.0
                                    rust-pest-generator-2.8.0
                                    rust-pest-meta-2.8.0
                                    rust-petgraph-0.6.5
                                    rust-phf-0.11.3
                                    rust-phf-codegen-0.11.3
                                    rust-phf-generator-0.11.3
                                    rust-phf-shared-0.11.3
                                    rust-pin-project-lite-0.2.16
                                    rust-pin-utils-0.1.0
                                    rust-pkg-config-0.3.32
                                    rust-plain-0.2.3
                                    rust-platform-info-2.0.5
                                    rust-pori-0.0.0
                                    rust-portable-atomic-1.11.0
                                    rust-powerfmt-0.2.0
                                    rust-ppv-lite86-0.2.21
                                    rust-predicates-3.1.3
                                    rust-predicates-core-1.0.9
                                    rust-predicates-tree-1.0.12
                                    rust-pretty-assertions-1.4.1
                                    rust-print-positions-0.6.1
                                    rust-proc-macro-error-attr2-2.0.0
                                    rust-proc-macro-error2-2.0.1
                                    rust-proc-macro2-1.0.94
                                    rust-procfs-0.17.0
                                    rust-procfs-core-0.17.0
                                    rust-pure-rust-locales-0.8.1
                                    rust-pwd-1.4.0
                                    rust-quick-error-2.0.1
                                    rust-quick-xml-0.31.0
                                    rust-quick-xml-0.37.4
                                    rust-quote-1.0.40
                                    rust-r-efi-5.2.0
                                    rust-rand-0.8.5
                                    rust-rand-chacha-0.3.1
                                    rust-rand-core-0.6.4
                                    rust-ratatui-0.29.0
                                    rust-rayon-1.10.0
                                    rust-rayon-core-1.12.1
                                    rust-recvmsg-1.0.0
                                    rust-redox-syscall-0.5.11
                                    rust-redox-users-0.4.6
                                    rust-reedline-0.39.0
                                    rust-ref-cast-1.0.24
                                    rust-ref-cast-impl-1.0.24
                                    rust-regex-1.11.1
                                    rust-regex-automata-0.4.9
                                    rust-regex-syntax-0.8.5
                                    rust-relative-path-1.9.3
                                    rust-reqwest-0.12.15
                                    rust-rmp-0.8.14
                                    rust-rmp-serde-1.3.0
                                    rust-roxmltree-0.20.0
                                    rust-rstest-0.23.0
                                    rust-rstest-macros-0.23.0
                                    rust-rusqlite-0.31.0
                                    rust-rust-embed-8.7.0
                                    rust-rust-embed-impl-8.7.0
                                    rust-rust-embed-utils-8.7.0
                                    rust-rust-decimal-1.37.1
                                    rust-rustc-demangle-0.1.24
                                    rust-rustc-hash-1.1.0
                                    rust-rustc-version-0.4.1
                                    rust-rustix-0.38.44
                                    rust-rustix-1.0.5
                                    rust-rustls-pemfile-2.2.0
                                    rust-rustls-pki-types-1.11.0
                                    rust-rustversion-1.0.20
                                    rust-ryu-1.0.20
                                    rust-same-file-1.0.6
                                    rust-scc-2.3.3
                                    rust-schannel-0.1.27
                                    rust-scopeguard-1.2.0
                                    rust-scroll-0.11.0
                                    rust-scroll-derive-0.11.1
                                    rust-sdd-3.0.8
                                    rust-security-framework-2.11.1
                                    rust-security-framework-sys-2.14.0
                                    rust-semver-1.0.26
                                    rust-serde-1.0.219
                                    rust-serde-derive-1.0.219
                                    rust-serde-json-1.0.140
                                    rust-serde-repr-0.1.20
                                    rust-serde-spanned-0.6.8
                                    rust-serde-urlencoded-0.7.1
                                    rust-serde-yaml-0.9.34+deprecated
                                    rust-serial-test-3.2.0
                                    rust-serial-test-derive-3.2.0
                                    rust-sha1-smol-1.0.1
                                    rust-sha2-0.10.8
                                    rust-shadow-rs-0.38.1
                                    rust-shell-words-1.1.0
                                    rust-shlex-1.3.0
                                    rust-signal-hook-0.3.17
                                    rust-signal-hook-mio-0.2.4
                                    rust-signal-hook-registry-1.4.2
                                    rust-simd-adler32-0.3.7
                                    rust-simplelog-0.12.2
                                    rust-siphasher-1.0.1
                                    rust-slab-0.4.9
                                    rust-smallvec-1.15.0
                                    rust-socket2-0.5.9
                                    rust-stable-deref-trait-1.2.0
                                    rust-static-assertions-1.1.0
                                    rust-strip-ansi-escapes-0.2.1
                                    rust-strsim-0.11.1
                                    rust-strum-0.26.3
                                    rust-strum-macros-0.26.4
                                    rust-supports-color-3.0.2
                                    rust-supports-hyperlinks-3.1.0
                                    rust-supports-unicode-3.0.0
                                    rust-syn-2.0.100
                                    rust-sync-wrapper-1.0.2
                                    rust-synstructure-0.13.1
                                    rust-sys-locale-0.3.2
                                    rust-sysinfo-0.33.1
                                    rust-tabled-0.17.0
                                    rust-tango-bench-0.6.0
                                    rust-tempfile-3.19.1
                                    rust-termcolor-1.4.1
                                    rust-terminal-size-0.4.2
                                    rust-termtree-0.5.1
                                    rust-textwrap-0.16.2
                                    rust-thiserror-1.0.69
                                    rust-thiserror-2.0.12
                                    rust-thiserror-impl-1.0.69
                                    rust-thiserror-impl-2.0.12
                                    rust-thread-local-1.1.8
                                    rust-time-0.3.41
                                    rust-time-core-0.1.4
                                    rust-time-macros-0.2.22
                                    rust-tinystr-0.7.6
                                    rust-titlecase-3.5.0
                                    rust-tokio-1.44.2
                                    rust-tokio-native-tls-0.3.1
                                    rust-toml-0.8.20
                                    rust-toml-datetime-0.6.8
                                    rust-toml-edit-0.22.24
                                    rust-tower-0.5.2
                                    rust-tower-layer-0.3.3
                                    rust-tower-service-0.3.3
                                    rust-tracing-0.1.41
                                    rust-tracing-core-0.1.33
                                    rust-trash-5.2.2
                                    rust-tree-magic-mini-3.1.6
                                    rust-try-lock-0.2.5
                                    rust-typeid-1.0.3
                                    rust-typenum-1.18.0
                                    rust-typetag-0.2.20
                                    rust-typetag-impl-0.2.20
                                    rust-ucd-trie-0.1.7
                                    rust-umask-2.1.0
                                    rust-unicase-2.8.1
                                    rust-unicode-ident-1.0.18
                                    rust-unicode-linebreak-0.1.5
                                    rust-unicode-segmentation-1.12.0
                                    rust-unicode-truncate-1.1.0
                                    rust-unicode-width-0.1.14
                                    rust-unicode-width-0.2.0
                                    rust-unicode-xid-0.2.6
                                    rust-unsafe-libyaml-0.2.11
                                    rust-update-informer-1.2.0
                                    rust-ureq-2.12.1
                                    rust-url-2.5.4
                                    rust-urlencoding-2.1.3
                                    rust-utf16-iter-1.0.5
                                    rust-utf8-iter-1.0.4
                                    rust-utf8parse-0.2.2
                                    rust-uu-cp-0.0.29
                                    rust-uu-mkdir-0.0.29
                                    rust-uu-mktemp-0.0.29
                                    rust-uu-mv-0.0.29
                                    rust-uu-touch-0.0.29
                                    rust-uu-uname-0.0.29
                                    rust-uu-whoami-0.0.29
                                    rust-uucore-0.0.29
                                    rust-uucore-procs-0.0.29
                                    rust-uuhelp-parser-0.0.29
                                    rust-uuid-1.16.0
                                    rust-v-htmlescape-0.15.8
                                    rust-vcpkg-0.2.15
                                    rust-version-check-0.9.5
                                    rust-vte-0.10.1
                                    rust-vte-0.14.1
                                    rust-vte-generate-state-changes-0.1.2
                                    rust-wait-timeout-0.2.1
                                    rust-walkdir-2.5.0
                                    rust-want-0.3.1
                                    rust-wasi-0.11.0+wasi-snapshot-preview1
                                    rust-wasi-0.14.2+wasi-0.2.4
                                    rust-wasm-bindgen-0.2.100
                                    rust-wasm-bindgen-backend-0.2.100
                                    rust-wasm-bindgen-futures-0.4.50
                                    rust-wasm-bindgen-macro-0.2.100
                                    rust-wasm-bindgen-macro-support-0.2.100
                                    rust-wasm-bindgen-shared-0.2.100
                                    rust-wax-0.6.0
                                    rust-wayland-backend-0.3.8
                                    rust-wayland-client-0.31.8
                                    rust-wayland-protocols-0.32.6
                                    rust-wayland-protocols-wlr-0.3.6
                                    rust-wayland-scanner-0.31.6
                                    rust-wayland-sys-0.31.6
                                    rust-web-sys-0.3.77
                                    rust-web-time-1.1.0
                                    rust-which-7.0.3
                                    rust-widestring-1.2.0
                                    rust-wild-2.2.1
                                    rust-winapi-0.3.9
                                    rust-winapi-i686-pc-windows-gnu-0.4.0
                                    rust-winapi-util-0.1.9
                                    rust-winapi-x86-64-pc-windows-gnu-0.4.0
                                    rust-windows-0.56.0
                                    rust-windows-0.57.0
                                    rust-windows-core-0.56.0
                                    rust-windows-core-0.57.0
                                    rust-windows-core-0.61.0
                                    rust-windows-implement-0.56.0
                                    rust-windows-implement-0.57.0
                                    rust-windows-implement-0.60.0
                                    rust-windows-interface-0.56.0
                                    rust-windows-interface-0.57.0
                                    rust-windows-interface-0.59.1
                                    rust-windows-link-0.1.1
                                    rust-windows-registry-0.4.0
                                    rust-windows-result-0.1.2
                                    rust-windows-result-0.3.2
                                    rust-windows-strings-0.3.1
                                    rust-windows-strings-0.4.0
                                    rust-windows-sys-0.48.0
                                    rust-windows-sys-0.52.0
                                    rust-windows-sys-0.59.0
                                    rust-windows-targets-0.48.5
                                    rust-windows-targets-0.52.6
                                    rust-windows-targets-0.53.0
                                    rust-windows-aarch64-gnullvm-0.48.5
                                    rust-windows-aarch64-gnullvm-0.52.6
                                    rust-windows-aarch64-gnullvm-0.53.0
                                    rust-windows-aarch64-msvc-0.48.5
                                    rust-windows-aarch64-msvc-0.52.6
                                    rust-windows-aarch64-msvc-0.53.0
                                    rust-windows-i686-gnu-0.48.5
                                    rust-windows-i686-gnu-0.52.6
                                    rust-windows-i686-gnu-0.53.0
                                    rust-windows-i686-gnullvm-0.52.6
                                    rust-windows-i686-gnullvm-0.53.0
                                    rust-windows-i686-msvc-0.48.5
                                    rust-windows-i686-msvc-0.52.6
                                    rust-windows-i686-msvc-0.53.0
                                    rust-windows-x86-64-gnu-0.48.5
                                    rust-windows-x86-64-gnu-0.52.6
                                    rust-windows-x86-64-gnu-0.53.0
                                    rust-windows-x86-64-gnullvm-0.48.5
                                    rust-windows-x86-64-gnullvm-0.52.6
                                    rust-windows-x86-64-gnullvm-0.53.0
                                    rust-windows-x86-64-msvc-0.48.5
                                    rust-windows-x86-64-msvc-0.52.6
                                    rust-windows-x86-64-msvc-0.53.0
                                    rust-winnow-0.7.6
                                    rust-winreg-0.52.0
                                    rust-winresource-0.1.20
                                    rust-winsafe-0.0.19
                                    rust-wit-bindgen-rt-0.39.0
                                    rust-wl-clipboard-rs-0.9.2
                                    rust-write16-1.0.0
                                    rust-writeable-0.5.5
                                    rust-x11rb-0.13.1
                                    rust-x11rb-protocol-0.13.1
                                    rust-xattr-1.5.0
                                    rust-yansi-1.0.1
                                    rust-yoke-0.7.5
                                    rust-yoke-derive-0.7.5
                                    rust-zerocopy-0.7.35
                                    rust-zerocopy-0.8.24
                                    rust-zerocopy-derive-0.7.35
                                    rust-zerocopy-derive-0.8.24
                                    rust-zerofrom-0.1.6
                                    rust-zerofrom-derive-0.1.6
                                    rust-zerovec-0.10.4
                                    rust-zerovec-derive-0.10.3
                                    rust-zip-2.5.0
                                    rust-zopfli-0.8.1))
                     (python-cryptography =>
                                          (list rust-asn1-0.20.0
                                           rust-asn1-derive-0.20.0
                                           rust-autocfg-1.4.0
                                           rust-base64-0.22.1
                                           rust-bitflags-2.9.0
                                           rust-cc-1.2.18
                                           rust-cfg-if-1.0.0
                                           rust-foreign-types-0.3.2
                                           rust-foreign-types-shared-0.1.1
                                           rust-heck-0.5.0
                                           rust-indoc-2.0.6
                                           rust-itoa-1.0.15
                                           rust-libc-0.2.171
                                           rust-memoffset-0.9.1
                                           rust-once-cell-1.21.3
                                           rust-openssl-0.10.72
                                           rust-openssl-macros-0.1.1
                                           rust-openssl-sys-0.9.107
                                           rust-pem-3.0.5
                                           rust-pkg-config-0.3.32
                                           rust-portable-atomic-1.11.0
                                           rust-proc-macro2-1.0.94
                                           rust-pyo3-0.23.5
                                           rust-pyo3-build-config-0.23.5
                                           rust-pyo3-ffi-0.23.5
                                           rust-pyo3-macros-0.23.5
                                           rust-pyo3-macros-backend-0.23.5
                                           rust-quote-1.0.40
                                           rust-self-cell-1.1.0
                                           rust-shlex-1.3.0
                                           rust-syn-2.0.100
                                           rust-target-lexicon-0.12.16
                                           rust-unicode-ident-1.0.18
                                           rust-unindent-0.2.4
                                           rust-vcpkg-0.2.15))
                     (python-rpds-py =>
                                     (list rust-archery-1.2.1
                                      rust-autocfg-1.4.0
                                      rust-bitflags-2.9.0
                                      rust-cfg-if-1.0.0
                                      rust-indoc-1.0.9
                                      rust-libc-0.2.171
                                      rust-lock-api-0.4.12
                                      rust-memoffset-0.9.1
                                      rust-once-cell-1.21.3
                                      rust-parking-lot-0.12.3
                                      rust-parking-lot-core-0.9.10
                                      rust-proc-macro2-1.0.94
                                      rust-pyo3-0.19.2
                                      rust-pyo3-build-config-0.19.2
                                      rust-pyo3-ffi-0.19.2
                                      rust-pyo3-macros-0.19.2
                                      rust-pyo3-macros-backend-0.19.2
                                      rust-quote-1.0.40
                                      rust-redox-syscall-0.5.10
                                      rust-rpds-1.1.0
                                      rust-scopeguard-1.2.0
                                      rust-smallvec-1.15.0
                                      rust-syn-1.0.109
                                      rust-target-lexicon-0.12.16
                                      rust-triomphe-0.1.14
                                      rust-unicode-ident-1.0.18
                                      rust-unindent-0.1.11
                                      rust-windows-targets-0.52.6
                                      rust-windows-aarch64-gnullvm-0.52.6
                                      rust-windows-aarch64-msvc-0.52.6
                                      rust-windows-i686-gnu-0.52.6
                                      rust-windows-i686-gnullvm-0.52.6
                                      rust-windows-i686-msvc-0.52.6
                                      rust-windows-x86-64-gnu-0.52.6
                                      rust-windows-x86-64-gnullvm-0.52.6
                                      rust-windows-x86-64-msvc-0.52.6))
                     (rav1e =>
                            (list rust-addr2line-0.24.2
                                  rust-adler2-2.0.0
                                  rust-aho-corasick-1.1.3
                                  rust-aligned-vec-0.5.0
                                  rust-anes-0.1.6
                                  rust-anstream-0.6.18
                                  rust-anstyle-1.0.10
                                  rust-anstyle-parse-0.2.6
                                  rust-anstyle-query-1.1.2
                                  rust-anstyle-wincon-3.0.7
                                  rust-anyhow-1.0.97
                                  rust-aom-sys-0.3.3
                                  rust-arbitrary-1.4.1
                                  rust-arg-enum-proc-macro-0.3.4
                                  rust-arrayvec-0.7.6
                                  rust-assert-cmd-2.0.16
                                  rust-autocfg-1.4.0
                                  rust-av-metrics-0.9.1
                                  rust-av1-grain-0.2.3
                                  rust-backtrace-0.3.74
                                  rust-bindgen-0.69.5
                                  rust-bitflags-1.3.2
                                  rust-bitflags-2.9.0
                                  rust-bitstream-io-2.6.0
                                  rust-bstr-1.11.3
                                  rust-built-0.7.7
                                  rust-bumpalo-3.17.0
                                  rust-bytemuck-1.22.0
                                  rust-byteorder-1.5.0
                                  rust-cast-0.3.0
                                  rust-cc-1.2.18
                                  rust-cexpr-0.6.0
                                  rust-cfg-expr-0.15.8
                                  rust-cfg-if-1.0.0
                                  rust-ciborium-0.2.2
                                  rust-ciborium-io-0.2.2
                                  rust-ciborium-ll-0.2.2
                                  rust-clang-sys-1.8.1
                                  rust-clap-4.5.35
                                  rust-clap-builder-4.5.35
                                  rust-clap-complete-4.5.47
                                  rust-clap-derive-4.5.32
                                  rust-clap-lex-0.7.4
                                  rust-color-quant-1.1.0
                                  rust-colorchoice-1.0.3
                                  rust-console-0.15.11
                                  rust-crc32fast-1.4.2
                                  rust-criterion-0.5.1
                                  rust-criterion-plot-0.5.0
                                  rust-crossbeam-0.8.4
                                  rust-crossbeam-channel-0.5.14
                                  rust-crossbeam-deque-0.8.6
                                  rust-crossbeam-epoch-0.9.18
                                  rust-crossbeam-queue-0.3.12
                                  rust-crossbeam-utils-0.8.21
                                  rust-crunchy-0.2.3
                                  rust-diff-0.1.13
                                  rust-difflib-0.4.0
                                  rust-displaydoc-0.2.5
                                  rust-doc-comment-0.3.3
                                  rust-either-1.15.0
                                  rust-encode-unicode-1.0.0
                                  rust-env-logger-0.8.4
                                  rust-equivalent-1.0.2
                                  rust-errno-0.3.11
                                  rust-fdeflate-0.3.7
                                  rust-fern-0.6.2
                                  rust-flate2-1.1.1
                                  rust-form-urlencoded-1.2.1
                                  rust-getrandom-0.2.15
                                  rust-getrandom-0.3.2
                                  rust-gimli-0.31.1
                                  rust-git2-0.20.1
                                  rust-glob-0.3.2
                                  rust-half-2.5.0
                                  rust-hashbrown-0.15.2
                                  rust-heck-0.5.0
                                  rust-hermit-abi-0.5.0
                                  rust-home-0.5.11
                                  rust-icu-collections-1.5.0
                                  rust-icu-locid-1.5.0
                                  rust-icu-locid-transform-1.5.0
                                  rust-icu-locid-transform-data-1.5.1
                                  rust-icu-normalizer-1.5.0
                                  rust-icu-normalizer-data-1.5.1
                                  rust-icu-properties-1.5.1
                                  rust-icu-properties-data-1.5.1
                                  rust-icu-provider-1.5.0
                                  rust-icu-provider-macros-1.5.0
                                  rust-idna-1.0.3
                                  rust-idna-adapter-1.2.0
                                  rust-image-0.24.9
                                  rust-indexmap-2.9.0
                                  rust-interpolate-name-0.2.4
                                  rust-is-terminal-0.4.16
                                  rust-is-terminal-polyfill-1.70.1
                                  rust-itertools-0.10.5
                                  rust-itertools-0.12.1
                                  rust-itoa-1.0.15
                                  rust-ivf-0.1.3
                                  rust-jobserver-0.1.33
                                  rust-js-sys-0.3.77
                                  rust-lab-0.11.0
                                  rust-lazy-static-1.5.0
                                  rust-lazycell-1.3.0
                                  rust-libc-0.2.171
                                  rust-libdav1d-sys-0.6.0
                                  rust-libfuzzer-sys-0.4.9
                                  rust-libgit2-sys-0.18.1+1.9.0
                                  rust-libloading-0.8.6
                                  rust-libz-sys-1.1.22
                                  rust-linux-raw-sys-0.4.15
                                  rust-linux-raw-sys-0.9.3
                                  rust-litemap-0.7.5
                                  rust-log-0.4.27
                                  rust-maybe-rayon-0.1.1
                                  rust-memchr-2.7.4
                                  rust-minimal-lexical-0.2.1
                                  rust-miniz-oxide-0.8.7
                                  rust-nasm-rs-0.2.5
                                  rust-new-debug-unreachable-1.0.6
                                  rust-nom-7.1.3
                                  rust-noop-proc-macro-0.3.0
                                  rust-nu-ansi-term-0.46.0
                                  rust-num-bigint-0.4.6
                                  rust-num-derive-0.4.2
                                  rust-num-integer-0.1.46
                                  rust-num-rational-0.4.2
                                  rust-num-traits-0.2.19
                                  rust-object-0.36.7
                                  rust-once-cell-1.21.3
                                  rust-oorandom-11.1.5
                                  rust-overload-0.1.1
                                  rust-paste-1.0.15
                                  rust-percent-encoding-2.3.1
                                  rust-pin-project-lite-0.2.16
                                  rust-pkg-config-0.3.32
                                  rust-plotters-0.3.7
                                  rust-plotters-backend-0.3.7
                                  rust-plotters-svg-0.3.7
                                  rust-png-0.17.16
                                  rust-ppv-lite86-0.2.21
                                  rust-predicates-3.1.3
                                  rust-predicates-core-1.0.9
                                  rust-predicates-tree-1.0.12
                                  rust-pretty-assertions-1.4.1
                                  rust-prettyplease-0.2.32
                                  rust-proc-macro2-1.0.94
                                  rust-profiling-1.0.16
                                  rust-profiling-procmacros-1.0.16
                                  rust-quickcheck-1.0.3
                                  rust-quote-1.0.40
                                  rust-r-efi-5.2.0
                                  rust-rand-0.8.5
                                  rust-rand-chacha-0.3.1
                                  rust-rand-core-0.6.4
                                  rust-rayon-1.10.0
                                  rust-rayon-core-1.12.1
                                  rust-regex-1.11.1
                                  rust-regex-automata-0.4.9
                                  rust-regex-syntax-0.8.5
                                  rust-rustc-demangle-0.1.24
                                  rust-rustc-hash-1.1.0
                                  rust-rustix-0.38.44
                                  rust-rustix-1.0.5
                                  rust-rustversion-1.0.20
                                  rust-ryu-1.0.20
                                  rust-same-file-1.0.6
                                  rust-scan-fmt-0.2.6
                                  rust-semver-1.0.26
                                  rust-serde-1.0.219
                                  rust-serde-big-array-0.5.1
                                  rust-serde-derive-1.0.219
                                  rust-serde-json-1.0.140
                                  rust-serde-spanned-0.6.8
                                  rust-sharded-slab-0.1.7
                                  rust-shlex-1.3.0
                                  rust-signal-hook-0.3.17
                                  rust-signal-hook-registry-1.4.2
                                  rust-simd-adler32-0.3.7
                                  rust-simd-helpers-0.1.0
                                  rust-smallvec-1.15.0
                                  rust-stable-deref-trait-1.2.0
                                  rust-syn-2.0.100
                                  rust-synstructure-0.13.1
                                  rust-system-deps-6.2.2
                                  rust-target-lexicon-0.12.16
                                  rust-terminal-size-0.4.2
                                  rust-termtree-0.5.1
                                  rust-thiserror-1.0.69
                                  rust-thiserror-impl-1.0.69
                                  rust-thread-local-1.1.8
                                  rust-tinystr-0.7.6
                                  rust-tinytemplate-1.2.1
                                  rust-toml-0.8.20
                                  rust-toml-datetime-0.6.8
                                  rust-toml-edit-0.22.24
                                  rust-tracing-0.1.41
                                  rust-tracing-attributes-0.1.28
                                  rust-tracing-chrome-0.7.2
                                  rust-tracing-core-0.1.33
                                  rust-tracing-log-0.2.0
                                  rust-tracing-subscriber-0.3.19
                                  rust-unicode-ident-1.0.18
                                  rust-unicode-width-0.2.0
                                  rust-url-2.5.4
                                  rust-utf16-iter-1.0.5
                                  rust-utf8-iter-1.0.4
                                  rust-utf8parse-0.2.2
                                  rust-v-frame-0.3.8
                                  rust-valuable-0.1.1
                                  rust-vcpkg-0.2.15
                                  rust-version-compare-0.2.0
                                  rust-wait-timeout-0.2.1
                                  rust-walkdir-2.5.0
                                  rust-wasi-0.11.0+wasi-snapshot-preview1
                                  rust-wasi-0.14.2+wasi-0.2.4
                                  rust-wasm-bindgen-0.2.100
                                  rust-wasm-bindgen-backend-0.2.100
                                  rust-wasm-bindgen-macro-0.2.100
                                  rust-wasm-bindgen-macro-support-0.2.100
                                  rust-wasm-bindgen-shared-0.2.100
                                  rust-web-sys-0.3.77
                                  rust-which-4.4.2
                                  rust-winapi-0.3.9
                                  rust-winapi-i686-pc-windows-gnu-0.4.0
                                  rust-winapi-util-0.1.9
                                  rust-winapi-x86-64-pc-windows-gnu-0.4.0
                                  rust-windows-sys-0.59.0
                                  rust-windows-targets-0.52.6
                                  rust-windows-aarch64-gnullvm-0.52.6
                                  rust-windows-aarch64-msvc-0.52.6
                                  rust-windows-i686-gnu-0.52.6
                                  rust-windows-i686-gnullvm-0.52.6
                                  rust-windows-i686-msvc-0.52.6
                                  rust-windows-x86-64-gnu-0.52.6
                                  rust-windows-x86-64-gnullvm-0.52.6
                                  rust-windows-x86-64-msvc-0.52.6
                                  rust-winnow-0.7.4
                                  rust-wit-bindgen-rt-0.39.0
                                  rust-write16-1.0.0
                                  rust-writeable-0.5.5
                                  rust-y4m-0.8.0
                                  rust-yansi-1.0.1
                                  rust-yoke-0.7.5
                                  rust-yoke-derive-0.7.5
                                  rust-zerocopy-0.8.24
                                  rust-zerocopy-derive-0.8.24
                                  rust-zerofrom-0.1.6
                                  rust-zerofrom-derive-0.1.6
                                  rust-zerovec-0.10.4
                                  rust-zerovec-derive-0.10.3))
                     (rust-bindgen-cli =>
                                       (list rust-aho-corasick-1.1.3
                                        rust-annotate-snippets-0.11.5
                                        rust-anstream-0.6.18
                                        rust-anstyle-1.0.10
                                        rust-anstyle-parse-0.2.6
                                        rust-anstyle-query-1.1.2
                                        rust-anstyle-wincon-3.0.7
                                        rust-bindgen-0.71.1
                                        rust-bitflags-2.9.0
                                        rust-cexpr-0.6.0
                                        rust-cfg-if-1.0.0
                                        rust-clang-sys-1.8.1
                                        rust-clap-4.5.35
                                        rust-clap-builder-4.5.35
                                        rust-clap-complete-4.5.47
                                        rust-clap-derive-4.5.32
                                        rust-clap-lex-0.7.4
                                        rust-colorchoice-1.0.3
                                        rust-either-1.15.0
                                        rust-env-logger-0.10.2
                                        rust-glob-0.3.2
                                        rust-heck-0.5.0
                                        rust-hermit-abi-0.5.0
                                        rust-humantime-2.2.0
                                        rust-is-terminal-0.4.16
                                        rust-is-terminal-polyfill-1.70.1
                                        rust-itertools-0.13.0
                                        rust-libc-0.2.171
                                        rust-libloading-0.8.6
                                        rust-log-0.4.27
                                        rust-memchr-2.7.4
                                        rust-minimal-lexical-0.2.1
                                        rust-nom-7.1.3
                                        rust-once-cell-1.21.3
                                        rust-prettyplease-0.2.32
                                        rust-proc-macro2-1.0.94
                                        rust-quote-1.0.40
                                        rust-regex-1.11.1
                                        rust-regex-automata-0.4.9
                                        rust-regex-syntax-0.8.5
                                        rust-rustc-hash-2.1.1
                                        rust-shlex-1.3.0
                                        rust-strsim-0.11.1
                                        rust-syn-2.0.100
                                        rust-termcolor-1.4.1
                                        rust-unicode-ident-1.0.18
                                        rust-unicode-width-0.2.0
                                        rust-utf8parse-0.2.2
                                        rust-winapi-util-0.1.9
                                        rust-windows-sys-0.59.0
                                        rust-windows-targets-0.52.6
                                        rust-windows-aarch64-gnullvm-0.52.6
                                        rust-windows-aarch64-msvc-0.52.6
                                        rust-windows-i686-gnu-0.52.6
                                        rust-windows-i686-gnullvm-0.52.6
                                        rust-windows-i686-msvc-0.52.6
                                        rust-windows-x86-64-gnu-0.52.6
                                        rust-windows-x86-64-gnullvm-0.52.6
                                        rust-windows-x86-64-msvc-0.52.6))
                     (rust-cargo-c =>
                                   (list rust-adler2-2.0.0
                                    rust-ahash-0.8.11
                                    rust-aho-corasick-1.1.3
                                    rust-allocator-api2-0.2.21
                                    rust-annotate-snippets-0.11.5
                                    rust-anstream-0.6.18
                                    rust-anstyle-1.0.10
                                    rust-anstyle-parse-0.2.6
                                    rust-anstyle-query-1.1.2
                                    rust-anstyle-wincon-3.0.7
                                    rust-anyhow-1.0.97
                                    rust-arc-swap-1.7.1
                                    rust-arrayref-0.3.9
                                    rust-arrayvec-0.7.6
                                    rust-autocfg-1.4.0
                                    rust-base16ct-0.2.0
                                    rust-base64-0.22.1
                                    rust-base64ct-1.7.3
                                    rust-bitflags-2.9.0
                                    rust-bitmaps-2.1.0
                                    rust-blake3-1.8.1
                                    rust-block-buffer-0.10.4
                                    rust-bstr-1.11.3
                                    rust-bumpalo-3.17.0
                                    rust-bytes-1.10.1
                                    rust-bytesize-1.3.3
                                    rust-cargo-0.85.0
                                    rust-cargo-credential-0.4.8
                                    rust-cargo-credential-libsecret-0.4.12
                                    rust-cargo-credential-macos-keychain-0.4.12
                                    rust-cargo-credential-wincred-0.4.12
                                    rust-cargo-platform-0.1.9
                                    rust-cargo-util-0.2.19
                                    rust-cargo-util-schemas-0.7.2
                                    rust-cbindgen-0.28.0
                                    rust-cc-1.2.18
                                    rust-cfg-if-1.0.0
                                    rust-clap-4.5.35
                                    rust-clap-builder-4.5.35
                                    rust-clap-complete-4.5.47
                                    rust-clap-derive-4.5.32
                                    rust-clap-lex-0.7.4
                                    rust-clru-0.6.2
                                    rust-color-print-0.3.7
                                    rust-color-print-proc-macro-0.3.7
                                    rust-colorchoice-1.0.3
                                    rust-const-oid-0.9.6
                                    rust-constant-time-eq-0.3.1
                                    rust-core-foundation-0.10.0
                                    rust-core-foundation-sys-0.8.7
                                    rust-cpufeatures-0.2.17
                                    rust-crates-io-0.40.9
                                    rust-crc32fast-1.4.2
                                    rust-crossbeam-channel-0.5.14
                                    rust-crossbeam-deque-0.8.6
                                    rust-crossbeam-epoch-0.9.18
                                    rust-crossbeam-utils-0.8.21
                                    rust-crypto-bigint-0.5.5
                                    rust-crypto-common-0.1.6
                                    rust-ct-codecs-1.1.3
                                    rust-curl-0.4.47
                                    rust-curl-sys-0.4.80+curl-8.12.1
                                    rust-dbus-0.9.7
                                    rust-der-0.7.9
                                    rust-deranged-0.4.0
                                    rust-digest-0.10.7
                                    rust-displaydoc-0.2.5
                                    rust-dunce-1.0.5
                                    rust-ecdsa-0.16.9
                                    rust-ed25519-compact-2.1.1
                                    rust-either-1.15.0
                                    rust-elliptic-curve-0.13.8
                                    rust-encoding-rs-0.8.35
                                    rust-equivalent-1.0.2
                                    rust-erased-serde-0.4.6
                                    rust-errno-0.3.11
                                    rust-fallible-iterator-0.3.0
                                    rust-fallible-streaming-iterator-0.1.9
                                    rust-faster-hex-0.9.0
                                    rust-fastrand-2.3.0
                                    rust-ff-0.13.1
                                    rust-fiat-crypto-0.2.9
                                    rust-filetime-0.2.25
                                    rust-flate2-1.1.1
                                    rust-fnv-1.0.7
                                    rust-foldhash-0.1.5
                                    rust-foreign-types-0.3.2
                                    rust-foreign-types-shared-0.1.1
                                    rust-form-urlencoded-1.2.1
                                    rust-generic-array-0.14.7
                                    rust-getrandom-0.2.15
                                    rust-getrandom-0.3.2
                                    rust-git2-0.19.0
                                    rust-git2-curl-0.20.0
                                    rust-gix-0.67.0
                                    rust-gix-actor-0.33.2
                                    rust-gix-attributes-0.23.1
                                    rust-gix-bitmap-0.2.14
                                    rust-gix-chunk-0.4.11
                                    rust-gix-command-0.3.11
                                    rust-gix-commitgraph-0.25.1
                                    rust-gix-config-0.41.0
                                    rust-gix-config-value-0.14.12
                                    rust-gix-credentials-0.25.1
                                    rust-gix-date-0.9.4
                                    rust-gix-diff-0.47.0
                                    rust-gix-dir-0.9.0
                                    rust-gix-discover-0.36.0
                                    rust-gix-features-0.39.1
                                    rust-gix-filter-0.14.0
                                    rust-gix-fs-0.12.1
                                    rust-gix-glob-0.17.1
                                    rust-gix-hash-0.15.1
                                    rust-gix-hashtable-0.6.0
                                    rust-gix-ignore-0.12.1
                                    rust-gix-index-0.36.0
                                    rust-gix-lock-15.0.1
                                    rust-gix-negotiate-0.16.0
                                    rust-gix-object-0.45.0
                                    rust-gix-odb-0.64.0
                                    rust-gix-pack-0.54.0
                                    rust-gix-packetline-0.18.4
                                    rust-gix-packetline-blocking-0.18.3
                                    rust-gix-path-0.10.15
                                    rust-gix-pathspec-0.8.1
                                    rust-gix-prompt-0.8.9
                                    rust-gix-protocol-0.46.1
                                    rust-gix-quote-0.4.15
                                    rust-gix-ref-0.48.0
                                    rust-gix-refspec-0.26.0
                                    rust-gix-revision-0.30.0
                                    rust-gix-revwalk-0.16.0
                                    rust-gix-sec-0.10.12
                                    rust-gix-submodule-0.15.0
                                    rust-gix-tempfile-15.0.0
                                    rust-gix-trace-0.1.12
                                    rust-gix-transport-0.43.1
                                    rust-gix-traverse-0.42.0
                                    rust-gix-url-0.28.2
                                    rust-gix-utils-0.1.14
                                    rust-gix-validate-0.9.4
                                    rust-gix-worktree-0.37.0
                                    rust-glob-0.3.2
                                    rust-globset-0.4.16
                                    rust-group-0.13.0
                                    rust-hashbrown-0.14.5
                                    rust-hashbrown-0.15.2
                                    rust-hashlink-0.9.1
                                    rust-heck-0.4.1
                                    rust-heck-0.5.0
                                    rust-hex-0.4.3
                                    rust-hkdf-0.12.4
                                    rust-hmac-0.12.1
                                    rust-home-0.5.11
                                    rust-http-auth-0.1.10
                                    rust-humantime-2.2.0
                                    rust-icu-collections-1.5.0
                                    rust-icu-locid-1.5.0
                                    rust-icu-locid-transform-1.5.0
                                    rust-icu-locid-transform-data-1.5.1
                                    rust-icu-normalizer-1.5.0
                                    rust-icu-normalizer-data-1.5.1
                                    rust-icu-properties-1.5.1
                                    rust-icu-properties-data-1.5.1
                                    rust-icu-provider-1.5.0
                                    rust-icu-provider-macros-1.5.0
                                    rust-idna-1.0.3
                                    rust-idna-adapter-1.2.0
                                    rust-ignore-0.4.23
                                    rust-im-rc-15.1.0
                                    rust-implib-0.3.5
                                    rust-indexmap-2.9.0
                                    rust-is-executable-1.0.4
                                    rust-is-terminal-polyfill-1.70.1
                                    rust-itertools-0.13.0
                                    rust-itertools-0.14.0
                                    rust-itoa-1.0.15
                                    rust-jiff-0.2.5
                                    rust-jiff-static-0.2.5
                                    rust-jiff-tzdb-0.1.4
                                    rust-jiff-tzdb-platform-0.1.3
                                    rust-jobserver-0.1.33
                                    rust-js-sys-0.3.77
                                    rust-kstring-2.0.2
                                    rust-lazy-static-1.5.0
                                    rust-lazycell-1.3.0
                                    rust-libc-0.2.171
                                    rust-libdbus-sys-0.2.5
                                    rust-libgit2-sys-0.17.0+1.8.1
                                    rust-libloading-0.8.6
                                    rust-libnghttp2-sys-0.1.11+1.64.0
                                    rust-libredox-0.1.3
                                    rust-libsqlite3-sys-0.30.1
                                    rust-libssh2-sys-0.3.1
                                    rust-libz-sys-1.1.22
                                    rust-linux-raw-sys-0.4.15
                                    rust-linux-raw-sys-0.9.3
                                    rust-litemap-0.7.5
                                    rust-lock-api-0.4.12
                                    rust-log-0.4.27
                                    rust-matchers-0.1.0
                                    rust-maybe-async-0.2.10
                                    rust-memchr-2.7.4
                                    rust-memmap2-0.9.5
                                    rust-memoffset-0.9.1
                                    rust-minimal-lexical-0.2.1
                                    rust-miniz-oxide-0.8.7
                                    rust-miow-0.6.0
                                    rust-nom-7.1.3
                                    rust-normpath-1.3.0
                                    rust-nu-ansi-term-0.46.0
                                    rust-num-conv-0.1.0
                                    rust-num-traits-0.2.19
                                    rust-object-0.36.7
                                    rust-once-cell-1.21.3
                                    rust-opener-0.7.2
                                    rust-openssl-0.10.57
                                    rust-openssl-macros-0.1.1
                                    rust-openssl-probe-0.1.6
                                    rust-openssl-src-300.4.2+3.4.1
                                    rust-openssl-sys-0.9.107
                                    rust-ordered-float-2.10.1
                                    rust-orion-0.17.9
                                    rust-os-info-3.10.0
                                    rust-overload-0.1.1
                                    rust-p384-0.13.1
                                    rust-parking-lot-0.12.3
                                    rust-parking-lot-core-0.9.10
                                    rust-pasetors-0.7.2
                                    rust-pathdiff-0.2.3
                                    rust-pem-rfc7468-0.7.0
                                    rust-percent-encoding-2.3.1
                                    rust-pin-project-lite-0.2.16
                                    rust-pkcs8-0.10.2
                                    rust-pkg-config-0.3.32
                                    rust-portable-atomic-1.11.0
                                    rust-portable-atomic-util-0.2.4
                                    rust-powerfmt-0.2.0
                                    rust-ppv-lite86-0.2.21
                                    rust-primeorder-0.13.6
                                    rust-proc-macro2-1.0.94
                                    rust-prodash-29.0.1
                                    rust-quote-1.0.40
                                    rust-r-efi-5.2.0
                                    rust-rand-0.8.5
                                    rust-rand-chacha-0.3.1
                                    rust-rand-core-0.6.4
                                    rust-rand-xoshiro-0.6.0
                                    rust-redox-syscall-0.5.11
                                    rust-regex-1.11.1
                                    rust-regex-automata-0.1.10
                                    rust-regex-automata-0.4.9
                                    rust-regex-syntax-0.6.29
                                    rust-regex-syntax-0.8.5
                                    rust-rfc6979-0.4.0
                                    rust-rusqlite-0.32.1
                                    rust-rustc-hash-2.1.1
                                    rust-rustfix-0.9.0
                                    rust-rustix-0.38.44
                                    rust-rustix-1.0.5
                                    rust-ryu-1.0.20
                                    rust-same-file-1.0.6
                                    rust-schannel-0.1.27
                                    rust-scopeguard-1.2.0
                                    rust-sec1-0.7.3
                                    rust-security-framework-3.2.0
                                    rust-security-framework-sys-2.14.0
                                    rust-semver-1.0.26
                                    rust-serde-1.0.219
                                    rust-serde-untagged-0.1.7
                                    rust-serde-value-0.7.0
                                    rust-serde-derive-1.0.219
                                    rust-serde-ignored-0.1.11
                                    rust-serde-json-1.0.140
                                    rust-serde-spanned-0.6.8
                                    rust-sha1-0.10.6
                                    rust-sha1-smol-1.0.1
                                    rust-sha2-0.10.8
                                    rust-sharded-slab-0.1.7
                                    rust-shell-escape-0.1.5
                                    rust-shell-words-1.1.0
                                    rust-shlex-1.3.0
                                    rust-signature-2.2.0
                                    rust-sized-chunks-0.6.5
                                    rust-smallvec-1.15.0
                                    rust-socket2-0.5.9
                                    rust-spki-0.7.3
                                    rust-stable-deref-trait-1.2.0
                                    rust-static-assertions-1.1.0
                                    rust-strsim-0.11.1
                                    rust-subtle-2.6.1
                                    rust-supports-hyperlinks-3.1.0
                                    rust-supports-unicode-3.0.0
                                    rust-syn-2.0.100
                                    rust-synstructure-0.13.1
                                    rust-tar-0.4.44
                                    rust-tempfile-3.19.1
                                    rust-terminal-size-0.4.2
                                    rust-thiserror-1.0.69
                                    rust-thiserror-2.0.12
                                    rust-thiserror-impl-1.0.69
                                    rust-thiserror-impl-2.0.12
                                    rust-thread-local-1.1.8
                                    rust-time-0.3.41
                                    rust-time-core-0.1.4
                                    rust-time-macros-0.2.22
                                    rust-tinystr-0.7.6
                                    rust-tinyvec-1.9.0
                                    rust-tinyvec-macros-0.1.1
                                    rust-toml-0.8.20
                                    rust-toml-datetime-0.6.8
                                    rust-toml-edit-0.22.24
                                    rust-tracing-0.1.41
                                    rust-tracing-attributes-0.1.28
                                    rust-tracing-chrome-0.7.2
                                    rust-tracing-core-0.1.33
                                    rust-tracing-log-0.2.0
                                    rust-tracing-subscriber-0.3.19
                                    rust-typeid-1.0.3
                                    rust-typenum-1.18.0
                                    rust-unicase-2.8.1
                                    rust-unicode-bom-2.0.3
                                    rust-unicode-ident-1.0.18
                                    rust-unicode-normalization-0.1.24
                                    rust-unicode-width-0.2.0
                                    rust-unicode-xid-0.2.6
                                    rust-url-2.5.4
                                    rust-utf16-iter-1.0.5
                                    rust-utf8-iter-1.0.4
                                    rust-utf8parse-0.2.2
                                    rust-valuable-0.1.1
                                    rust-vcpkg-0.2.15
                                    rust-version-check-0.9.5
                                    rust-walkdir-2.5.0
                                    rust-wasi-0.11.0+wasi-snapshot-preview1
                                    rust-wasi-0.14.2+wasi-0.2.4
                                    rust-wasm-bindgen-0.2.100
                                    rust-wasm-bindgen-backend-0.2.100
                                    rust-wasm-bindgen-macro-0.2.100
                                    rust-wasm-bindgen-macro-support-0.2.100
                                    rust-wasm-bindgen-shared-0.2.100
                                    rust-winapi-0.3.9
                                    rust-winapi-i686-pc-windows-gnu-0.4.0
                                    rust-winapi-util-0.1.9
                                    rust-winapi-x86-64-pc-windows-gnu-0.4.0
                                    rust-windows-sys-0.48.0
                                    rust-windows-sys-0.52.0
                                    rust-windows-sys-0.59.0
                                    rust-windows-targets-0.48.5
                                    rust-windows-targets-0.52.6
                                    rust-windows-aarch64-gnullvm-0.48.5
                                    rust-windows-aarch64-gnullvm-0.52.6
                                    rust-windows-aarch64-msvc-0.48.5
                                    rust-windows-aarch64-msvc-0.52.6
                                    rust-windows-i686-gnu-0.48.5
                                    rust-windows-i686-gnu-0.52.6
                                    rust-windows-i686-gnullvm-0.52.6
                                    rust-windows-i686-msvc-0.48.5
                                    rust-windows-i686-msvc-0.52.6
                                    rust-windows-x86-64-gnu-0.48.5
                                    rust-windows-x86-64-gnu-0.52.6
                                    rust-windows-x86-64-gnullvm-0.48.5
                                    rust-windows-x86-64-gnullvm-0.52.6
                                    rust-windows-x86-64-msvc-0.48.5
                                    rust-windows-x86-64-msvc-0.52.6
                                    rust-winnow-0.6.26
                                    rust-winnow-0.7.4
                                    rust-wit-bindgen-rt-0.39.0
                                    rust-write16-1.0.0
                                    rust-writeable-0.5.5
                                    rust-yoke-0.7.5
                                    rust-yoke-derive-0.7.5
                                    rust-zerocopy-0.7.35
                                    rust-zerocopy-0.8.24
                                    rust-zerocopy-derive-0.7.35
                                    rust-zerocopy-derive-0.8.24
                                    rust-zerofrom-0.1.6
                                    rust-zerofrom-derive-0.1.6
                                    rust-zeroize-1.8.1
                                    rust-zerovec-0.10.4
                                    rust-zerovec-derive-0.10.3))
                     (rust-cbindgen-0.26 =>
                                         (list rust-atty-0.2.14
                                          rust-autocfg-1.4.0
                                          rust-bitflags-1.3.2
                                          rust-bitflags-2.9.0
                                          rust-cfg-if-1.0.0
                                          rust-clap-3.2.25
                                          rust-clap-lex-0.2.4
                                          rust-errno-0.3.11
                                          rust-fastrand-2.3.0
                                          rust-getrandom-0.3.2
                                          rust-hashbrown-0.12.3
                                          rust-heck-0.4.1
                                          rust-hermit-abi-0.1.19
                                          rust-indexmap-1.9.3
                                          rust-instant-0.1.13
                                          rust-itoa-1.0.15
                                          rust-lazy-static-1.5.0
                                          rust-libc-0.2.171
                                          rust-linux-raw-sys-0.9.3
                                          rust-lock-api-0.4.12
                                          rust-log-0.4.27
                                          rust-memchr-2.7.4
                                          rust-once-cell-1.21.3
                                          rust-os-str-bytes-6.6.1
                                          rust-parking-lot-0.11.2
                                          rust-parking-lot-core-0.8.6
                                          rust-proc-macro2-1.0.94
                                          rust-quote-1.0.40
                                          rust-r-efi-5.2.0
                                          rust-redox-syscall-0.2.16
                                          rust-rustix-1.0.5
                                          rust-ryu-1.0.20
                                          rust-scopeguard-1.2.0
                                          rust-serde-1.0.219
                                          rust-serde-derive-1.0.219
                                          rust-serde-json-1.0.140
                                          rust-serial-test-0.5.1
                                          rust-serial-test-derive-0.5.1
                                          rust-smallvec-1.15.0
                                          rust-strsim-0.10.0
                                          rust-syn-1.0.109
                                          rust-syn-2.0.100
                                          rust-tempfile-3.19.1
                                          rust-termcolor-1.4.1
                                          rust-textwrap-0.16.2
                                          rust-toml-0.5.11
                                          rust-unicode-ident-1.0.18
                                          rust-wasi-0.14.2+wasi-0.2.4
                                          rust-winapi-0.3.9
                                          rust-winapi-i686-pc-windows-gnu-0.4.0
                                          rust-winapi-util-0.1.9
                                          rust-winapi-x86-64-pc-windows-gnu-0.4.0
                                          rust-windows-sys-0.59.0
                                          rust-windows-targets-0.52.6
                                          rust-windows-aarch64-gnullvm-0.52.6
                                          rust-windows-aarch64-msvc-0.52.6
                                          rust-windows-i686-gnu-0.52.6
                                          rust-windows-i686-gnullvm-0.52.6
                                          rust-windows-i686-msvc-0.52.6
                                          rust-windows-x86-64-gnu-0.52.6
                                          rust-windows-x86-64-gnullvm-0.52.6
                                          rust-windows-x86-64-msvc-0.52.6
                                          rust-wit-bindgen-rt-0.39.0))
                     (rust-cbindgen-0.28 =>
                                         (list rust-anstream-0.6.18
                                          rust-anstyle-1.0.10
                                          rust-anstyle-parse-0.2.6
                                          rust-anstyle-query-1.1.2
                                          rust-anstyle-wincon-3.0.7
                                          rust-autocfg-1.4.0
                                          rust-bitflags-2.9.0
                                          rust-cfg-if-1.0.0
                                          rust-clap-4.5.35
                                          rust-clap-builder-4.5.35
                                          rust-clap-lex-0.7.4
                                          rust-colorchoice-1.0.3
                                          rust-dashmap-5.5.3
                                          rust-diff-0.1.13
                                          rust-equivalent-1.0.2
                                          rust-errno-0.3.11
                                          rust-fastrand-2.3.0
                                          rust-getrandom-0.3.2
                                          rust-hashbrown-0.14.5
                                          rust-hashbrown-0.15.2
                                          rust-heck-0.4.1
                                          rust-indexmap-2.9.0
                                          rust-is-terminal-polyfill-1.70.1
                                          rust-itoa-1.0.15
                                          rust-lazy-static-1.5.0
                                          rust-libc-0.2.171
                                          rust-linux-raw-sys-0.9.3
                                          rust-lock-api-0.4.12
                                          rust-log-0.4.27
                                          rust-memchr-2.7.4
                                          rust-once-cell-1.21.3
                                          rust-parking-lot-0.12.3
                                          rust-parking-lot-core-0.9.10
                                          rust-pretty-assertions-1.4.1
                                          rust-proc-macro2-1.0.94
                                          rust-quote-1.0.40
                                          rust-r-efi-5.2.0
                                          rust-redox-syscall-0.5.10
                                          rust-rustix-1.0.5
                                          rust-ryu-1.0.20
                                          rust-scopeguard-1.2.0
                                          rust-serde-1.0.219
                                          rust-serde-derive-1.0.219
                                          rust-serde-json-1.0.140
                                          rust-serde-spanned-0.6.8
                                          rust-serial-test-2.0.0
                                          rust-serial-test-derive-2.0.0
                                          rust-smallvec-1.15.0
                                          rust-strsim-0.11.1
                                          rust-syn-2.0.100
                                          rust-tempfile-3.19.1
                                          rust-toml-0.8.20
                                          rust-toml-datetime-0.6.8
                                          rust-toml-edit-0.22.24
                                          rust-unicode-ident-1.0.18
                                          rust-utf8parse-0.2.2
                                          rust-wasi-0.14.2+wasi-0.2.4
                                          rust-windows-sys-0.59.0
                                          rust-windows-targets-0.52.6
                                          rust-windows-aarch64-gnullvm-0.52.6
                                          rust-windows-aarch64-msvc-0.52.6
                                          rust-windows-i686-gnu-0.52.6
                                          rust-windows-i686-gnullvm-0.52.6
                                          rust-windows-i686-msvc-0.52.6
                                          rust-windows-x86-64-gnu-0.52.6
                                          rust-windows-x86-64-gnullvm-0.52.6
                                          rust-windows-x86-64-msvc-0.52.6
                                          rust-winnow-0.7.4
                                          rust-wit-bindgen-rt-0.39.0
                                          rust-yansi-1.0.1))
                     (rust-minisign =>
                                    (list rust-block-buffer-0.10.4
                                     rust-bumpalo-3.17.0
                                     rust-cfg-if-1.0.0
                                     rust-cipher-0.4.4
                                     rust-cpufeatures-0.2.17
                                     rust-crypto-common-0.1.6
                                     rust-ct-codecs-1.1.3
                                     rust-digest-0.10.7
                                     rust-generic-array-0.14.7
                                     rust-getrandom-0.2.15
                                     rust-hmac-0.12.1
                                     rust-inout-0.1.4
                                     rust-js-sys-0.3.77
                                     rust-libc-0.2.171
                                     rust-log-0.4.27
                                     rust-once-cell-1.21.3
                                     rust-pbkdf2-0.12.2
                                     rust-proc-macro2-1.0.94
                                     rust-quote-1.0.40
                                     rust-rpassword-7.3.1
                                     rust-rtoolbox-0.0.2
                                     rust-salsa20-0.10.2
                                     rust-scrypt-0.11.0
                                     rust-sha2-0.10.8
                                     rust-subtle-2.6.1
                                     rust-syn-2.0.100
                                     rust-typenum-1.18.0
                                     rust-unicode-ident-1.0.18
                                     rust-version-check-0.9.5
                                     rust-wasi-0.11.0+wasi-snapshot-preview1
                                     rust-wasm-bindgen-0.2.100
                                     rust-wasm-bindgen-backend-0.2.100
                                     rust-wasm-bindgen-macro-0.2.100
                                     rust-wasm-bindgen-macro-support-0.2.100
                                     rust-wasm-bindgen-shared-0.2.100
                                     rust-windows-sys-0.48.0
                                     rust-windows-targets-0.48.5
                                     rust-windows-aarch64-gnullvm-0.48.5
                                     rust-windows-aarch64-msvc-0.48.5
                                     rust-windows-i686-gnu-0.48.5
                                     rust-windows-i686-msvc-0.48.5
                                     rust-windows-x86-64-gnu-0.48.5
                                     rust-windows-x86-64-gnullvm-0.48.5
                                     rust-windows-x86-64-msvc-0.48.5))
                     (rust-pcre2-utf32-0.2 =>
                                           (list rust-bitflags-2.9.0
                                                 rust-cc-1.2.18
                                                 rust-cfg-if-1.0.0
                                                 rust-getrandom-0.3.2
                                                 rust-jobserver-0.1.33
                                                 rust-libc-0.2.171
                                                 rust-log-0.4.27
                                                 rust-pkg-config-0.3.32
                                                 rust-r-efi-5.2.0
                                                 rust-shlex-1.3.0
                                                 rust-wasi-0.14.2+wasi-0.2.4
                                                 rust-wit-bindgen-rt-0.39.0))
                     (rust-pipewire-for-niri =>
                                             (list rust-aho-corasick-1.1.3
                                              rust-annotate-snippets-0.9.2
                                              rust-anstream-0.6.18
                                              rust-anstyle-1.0.10
                                              rust-anstyle-parse-0.2.6
                                              rust-anstyle-query-1.1.2
                                              rust-anstyle-wincon-3.0.7
                                              rust-anyhow-1.0.97
                                              rust-autocfg-1.4.0
                                              rust-bindgen-0.69.5
                                              rust-bitflags-2.9.0
                                              rust-cc-1.2.18
                                              rust-cexpr-0.6.0
                                              rust-cfg-expr-0.15.8
                                              rust-cfg-if-1.0.0
                                              rust-cfg-aliases-0.2.1
                                              rust-clang-sys-1.8.1
                                              rust-clap-4.5.35
                                              rust-clap-builder-4.5.35
                                              rust-clap-derive-4.5.32
                                              rust-clap-lex-0.7.4
                                              rust-colorchoice-1.0.3
                                              rust-convert-case-0.6.0
                                              rust-cookie-factory-0.3.3
                                              rust-either-1.15.0
                                              rust-equivalent-1.0.2
                                              rust-futures-0.3.31
                                              rust-futures-channel-0.3.31
                                              rust-futures-core-0.3.31
                                              rust-futures-executor-0.3.31
                                              rust-futures-io-0.3.31
                                              rust-futures-macro-0.3.31
                                              rust-futures-sink-0.3.31
                                              rust-futures-task-0.3.31
                                              rust-futures-util-0.3.31
                                              rust-glob-0.3.2
                                              rust-hashbrown-0.15.2
                                              rust-heck-0.5.0
                                              rust-indexmap-2.9.0
                                              rust-is-terminal-polyfill-1.70.1
                                              rust-itertools-0.12.1
                                              rust-lazy-static-1.5.0
                                              rust-lazycell-1.3.0
                                              rust-libc-0.2.171
                                              rust-libloading-0.8.6
                                              rust-memchr-2.7.4
                                              rust-minimal-lexical-0.2.1
                                              rust-nix-0.29.0
                                              rust-nom-7.1.3
                                              rust-once-cell-1.21.3
                                              rust-pin-project-lite-0.2.16
                                              rust-pin-utils-0.1.0
                                              rust-pkg-config-0.3.32
                                              rust-proc-macro2-1.0.94
                                              rust-quote-1.0.40
                                              rust-regex-1.11.1
                                              rust-regex-automata-0.4.9
                                              rust-regex-syntax-0.8.5
                                              rust-rustc-hash-1.1.0
                                              rust-serde-1.0.219
                                              rust-serde-derive-1.0.219
                                              rust-serde-spanned-0.6.8
                                              rust-shlex-1.3.0
                                              rust-slab-0.4.9
                                              rust-smallvec-1.15.0
                                              rust-strsim-0.11.1
                                              rust-syn-2.0.100
                                              rust-system-deps-6.2.2
                                              rust-target-lexicon-0.12.16
                                              rust-thiserror-1.0.69
                                              rust-thiserror-impl-1.0.69
                                              rust-toml-0.8.20
                                              rust-toml-datetime-0.6.8
                                              rust-toml-edit-0.22.24
                                              rust-unicode-ident-1.0.18
                                              rust-unicode-segmentation-1.12.0
                                              rust-unicode-width-0.1.14
                                              rust-utf8parse-0.2.2
                                              rust-version-compare-0.2.0
                                              rust-winapi-0.3.9
                                              rust-winapi-i686-pc-windows-gnu-0.4.0
                                              rust-winapi-x86-64-pc-windows-gnu-0.4.0
                                              rust-windows-sys-0.59.0
                                              rust-windows-targets-0.52.6
                                              rust-windows-aarch64-gnullvm-0.52.6
                                              rust-windows-aarch64-msvc-0.52.6
                                              rust-windows-i686-gnu-0.52.6
                                              rust-windows-i686-gnullvm-0.52.6
                                              rust-windows-i686-msvc-0.52.6
                                              rust-windows-x86-64-gnu-0.52.6
                                              rust-windows-x86-64-gnullvm-0.52.6
                                              rust-windows-x86-64-msvc-0.52.6
                                              rust-winnow-0.7.4
                                              rust-yansi-term-0.1.2))
                     (rust-pubgrub-for-uv =>
                                          (list rust-aho-corasick-1.1.3
                                           rust-anes-0.1.6
                                           rust-anstream-0.6.18
                                           rust-anstyle-1.0.10
                                           rust-anstyle-parse-0.2.6
                                           rust-anstyle-query-1.1.2
                                           rust-anstyle-wincon-3.0.7
                                           rust-anyhow-1.0.97
                                           rust-autocfg-1.4.0
                                           rust-base64-0.22.1
                                           rust-bit-set-0.8.0
                                           rust-bit-vec-0.8.0
                                           rust-bitflags-2.9.0
                                           rust-bumpalo-3.17.0
                                           rust-cast-0.3.0
                                           rust-cfg-if-1.0.0
                                           rust-ciborium-0.2.2
                                           rust-ciborium-io-0.2.2
                                           rust-ciborium-ll-0.2.2
                                           rust-clap-4.5.35
                                           rust-clap-builder-4.5.35
                                           rust-clap-lex-0.7.4
                                           rust-codspeed-2.10.0
                                           rust-codspeed-criterion-compat-2.10.0
                                           rust-codspeed-criterion-compat-walltime-2.10.0
                                           rust-colorchoice-1.0.3
                                           rust-colored-2.2.0
                                           rust-criterion-plot-0.5.0
                                           rust-crossbeam-deque-0.8.6
                                           rust-crossbeam-epoch-0.9.18
                                           rust-crossbeam-utils-0.8.21
                                           rust-crunchy-0.2.3
                                           rust-either-1.15.0
                                           rust-env-filter-0.1.3
                                           rust-env-logger-0.11.8
                                           rust-equivalent-1.0.2
                                           rust-errno-0.3.11
                                           rust-fastrand-2.3.0
                                           rust-fnv-1.0.7
                                           rust-getrandom-0.2.15
                                           rust-getrandom-0.3.2
                                           rust-half-2.5.0
                                           rust-hashbrown-0.15.2
                                           rust-hermit-abi-0.5.0
                                           rust-indexmap-2.9.0
                                           rust-is-terminal-0.4.16
                                           rust-is-terminal-polyfill-1.70.1
                                           rust-itertools-0.10.5
                                           rust-itoa-0.4.8
                                           rust-itoa-1.0.15
                                           rust-jiff-0.2.5
                                           rust-jiff-static-0.2.5
                                           rust-js-sys-0.3.77
                                           rust-lazy-static-1.5.0
                                           rust-leb128-0.2.5
                                           rust-libc-0.2.171
                                           rust-linux-raw-sys-0.9.3
                                           rust-log-0.4.27
                                           rust-memchr-2.7.4
                                           rust-num-traits-0.2.19
                                           rust-once-cell-1.21.3
                                           rust-oorandom-11.1.5
                                           rust-ordered-float-2.10.1
                                           rust-partial-ref-0.3.3
                                           rust-partial-ref-derive-0.3.3
                                           rust-plotters-0.3.7
                                           rust-plotters-backend-0.3.7
                                           rust-plotters-svg-0.3.7
                                           rust-portable-atomic-1.11.0
                                           rust-portable-atomic-util-0.2.4
                                           rust-ppv-lite86-0.2.21
                                           rust-priority-queue-2.3.1
                                           rust-proc-macro2-1.0.94
                                           rust-proptest-1.6.0
                                           rust-quick-error-1.2.3
                                           rust-quote-1.0.40
                                           rust-r-efi-5.2.0
                                           rust-rand-0.8.5
                                           rust-rand-chacha-0.3.1
                                           rust-rand-core-0.6.4
                                           rust-rand-xorshift-0.3.0
                                           rust-rayon-1.10.0
                                           rust-rayon-core-1.12.1
                                           rust-regex-1.11.1
                                           rust-regex-automata-0.4.9
                                           rust-regex-syntax-0.8.5
                                           rust-ron-0.9.0-alpha.1
                                           rust-rustc-hash-1.1.0
                                           rust-rustc-hash-2.1.1
                                           rust-rustix-1.0.5
                                           rust-rustversion-1.0.20
                                           rust-rusty-fork-0.3.0
                                           rust-ryu-1.0.20
                                           rust-same-file-1.0.6
                                           rust-serde-1.0.219
                                           rust-serde-derive-1.0.219
                                           rust-serde-json-1.0.140
                                           rust-smallvec-1.15.0
                                           rust-syn-1.0.109
                                           rust-syn-2.0.100
                                           rust-synstructure-0.12.6
                                           rust-tempfile-3.19.1
                                           rust-thiserror-1.0.69
                                           rust-thiserror-2.0.12
                                           rust-thiserror-impl-1.0.69
                                           rust-thiserror-impl-2.0.12
                                           rust-tinytemplate-1.2.1
                                           rust-unarray-0.1.4
                                           rust-unicode-ident-1.0.18
                                           rust-unicode-xid-0.2.6
                                           rust-utf8parse-0.2.2
                                           rust-uuid-1.16.0
                                           rust-varisat-0.2.2
                                           rust-varisat-checker-0.2.2
                                           rust-varisat-dimacs-0.2.2
                                           rust-varisat-formula-0.2.2
                                           rust-varisat-internal-macros-0.2.2
                                           rust-varisat-internal-proof-0.2.2
                                           rust-vec-mut-scan-0.3.0
                                           rust-wait-timeout-0.2.1
                                           rust-walkdir-2.5.0
                                           rust-wasi-0.11.0+wasi-snapshot-preview1
                                           rust-wasi-0.14.2+wasi-0.2.4
                                           rust-wasm-bindgen-0.2.100
                                           rust-wasm-bindgen-backend-0.2.100
                                           rust-wasm-bindgen-macro-0.2.100
                                           rust-wasm-bindgen-macro-support-0.2.100
                                           rust-wasm-bindgen-shared-0.2.100
                                           rust-web-sys-0.3.77
                                           rust-winapi-util-0.1.9
                                           rust-windows-sys-0.59.0
                                           rust-windows-targets-0.52.6
                                           rust-windows-aarch64-gnullvm-0.52.6
                                           rust-windows-aarch64-msvc-0.52.6
                                           rust-windows-i686-gnu-0.52.6
                                           rust-windows-i686-gnullvm-0.52.6
                                           rust-windows-i686-msvc-0.52.6
                                           rust-windows-x86-64-gnu-0.52.6
                                           rust-windows-x86-64-gnullvm-0.52.6
                                           rust-windows-x86-64-msvc-0.52.6
                                           rust-wit-bindgen-rt-0.39.0
                                           rust-zerocopy-0.8.24
                                           rust-zerocopy-derive-0.8.24))
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
                                      rust-windows-x86-64-msvc-0.52.6))
                     (rust-smithay-for-niri =>
                                            (list rust-adler2-2.0.0
                                             rust-ahash-0.8.11
                                             rust-aho-corasick-1.1.3
                                             rust-aligned-vec-0.5.0
                                             rust-android-activity-0.6.0
                                             rust-android-properties-0.2.2
                                             rust-anes-0.1.6
                                             rust-anstream-0.6.18
                                             rust-anstyle-1.0.10
                                             rust-anstyle-parse-0.2.6
                                             rust-anstyle-query-1.1.2
                                             rust-anstyle-wincon-3.0.7
                                             rust-anyhow-1.0.97
                                             rust-appendlist-1.4.0
                                             rust-approx-0.4.0
                                             rust-arbitrary-1.4.1
                                             rust-arg-enum-proc-macro-0.3.4
                                             rust-arrayvec-0.7.6
                                             rust-as-raw-xcb-connection-1.0.1
                                             rust-ash-0.38.0+1.3.281
                                             rust-atomic-waker-1.1.2
                                             rust-autocfg-1.4.0
                                             rust-av1-grain-0.2.3
                                             rust-avif-serialize-0.8.3
                                             rust-bincode-1.3.3
                                             rust-bindgen-0.69.5
                                             rust-bit-field-0.10.2
                                             rust-bitflags-1.3.2
                                             rust-bitflags-2.9.0
                                             rust-bitstream-io-2.6.0
                                             rust-block2-0.5.1
                                             rust-built-0.7.7
                                             rust-bumpalo-3.17.0
                                             rust-bytemuck-1.22.0
                                             rust-bytemuck-derive-1.9.3
                                             rust-byteorder-1.5.0
                                             rust-byteorder-lite-0.1.0
                                             rust-bytes-1.10.1
                                             rust-calloop-0.13.0
                                             rust-calloop-0.14.2
                                             rust-calloop-wayland-source-0.3.0
                                             rust-cast-0.3.0
                                             rust-cc-1.2.18
                                             rust-cesu8-1.1.0
                                             rust-cexpr-0.6.0
                                             rust-cfg-expr-0.15.8
                                             rust-cfg-expr-0.17.2
                                             rust-cfg-if-1.0.0
                                             rust-cfg-aliases-0.2.1
                                             rust-cgmath-0.18.0
                                             rust-ciborium-0.2.2
                                             rust-ciborium-io-0.2.2
                                             rust-ciborium-ll-0.2.2
                                             rust-clang-sys-1.8.1
                                             rust-clap-4.5.35
                                             rust-clap-builder-4.5.35
                                             rust-clap-derive-4.5.32
                                             rust-clap-lex-0.7.4
                                             rust-color-quant-1.1.0
                                             rust-colorchoice-1.0.3
                                             rust-combine-4.6.7
                                             rust-concurrent-queue-2.5.0
                                             rust-container-of-0.5.1
                                             rust-core-foundation-0.9.4
                                             rust-core-foundation-sys-0.8.7
                                             rust-core-graphics-0.23.2
                                             rust-core-graphics-types-0.1.3
                                             rust-crc32fast-1.4.2
                                             rust-criterion-0.5.1
                                             rust-criterion-plot-0.5.0
                                             rust-crossbeam-channel-0.5.14
                                             rust-crossbeam-deque-0.8.6
                                             rust-crossbeam-epoch-0.9.18
                                             rust-crossbeam-utils-0.8.21
                                             rust-crunchy-0.2.3
                                             rust-cursor-icon-1.1.0
                                             rust-dispatch-0.2.0
                                             rust-dlib-0.5.2
                                             rust-downcast-rs-1.2.1
                                             rust-dpi-0.1.1
                                             rust-drm-0.14.1
                                             rust-drm-ffi-0.9.0
                                             rust-drm-fourcc-2.2.0
                                             rust-drm-sys-0.8.0
                                             rust-either-1.15.0
                                             rust-encoding-rs-0.8.35
                                             rust-equivalent-1.0.2
                                             rust-errno-0.3.11
                                             rust-exr-1.73.0
                                             rust-fastrand-2.3.0
                                             rust-fdeflate-0.3.7
                                             rust-flate2-1.1.1
                                             rust-float-cmp-0.9.0
                                             rust-foreign-types-0.5.0
                                             rust-foreign-types-macros-0.2.3
                                             rust-foreign-types-shared-0.3.1
                                             rust-fps-ticker-1.0.0
                                             rust-gbm-0.18.0
                                             rust-gbm-sys-0.4.0
                                             rust-generator-0.8.4
                                             rust-gethostname-0.4.3
                                             rust-getrandom-0.2.15
                                             rust-getrandom-0.3.2
                                             rust-gif-0.13.1
                                             rust-gl-generator-0.14.0
                                             rust-glob-0.3.2
                                             rust-glow-0.16.0
                                             rust-half-2.5.0
                                             rust-hashbrown-0.15.2
                                             rust-heck-0.5.0
                                             rust-hermit-abi-0.3.9
                                             rust-hermit-abi-0.4.0
                                             rust-hermit-abi-0.5.0
                                             rust-home-0.5.11
                                             rust-image-0.25.6
                                             rust-image-webp-0.2.1
                                             rust-imgref-1.11.0
                                             rust-indexmap-2.9.0
                                             rust-input-0.9.1
                                             rust-input-sys-1.18.0
                                             rust-instant-0.1.13
                                             rust-interpolate-name-0.2.4
                                             rust-io-lifetimes-1.0.11
                                             rust-is-terminal-0.4.16
                                             rust-is-terminal-polyfill-1.70.1
                                             rust-itertools-0.10.5
                                             rust-itertools-0.12.1
                                             rust-itoa-1.0.15
                                             rust-jni-0.21.1
                                             rust-jni-sys-0.3.0
                                             rust-jobserver-0.1.33
                                             rust-jpeg-decoder-0.3.1
                                             rust-js-sys-0.3.77
                                             rust-khronos-api-3.1.0
                                             rust-lazy-static-1.5.0
                                             rust-lazycell-1.3.0
                                             rust-lebe-0.5.2
                                             rust-libc-0.2.171
                                             rust-libdisplay-info-0.2.2
                                             rust-libdisplay-info-derive-0.1.0
                                             rust-libdisplay-info-sys-0.2.2
                                             rust-libfuzzer-sys-0.4.9
                                             rust-libloading-0.7.4
                                             rust-libloading-0.8.6
                                             rust-libredox-0.1.3
                                             rust-libseat-0.2.3
                                             rust-libseat-sys-0.1.9
                                             rust-libudev-sys-0.1.4
                                             rust-linux-raw-sys-0.4.15
                                             rust-linux-raw-sys-0.6.5
                                             rust-linux-raw-sys-0.9.3
                                             rust-lock-api-0.4.12
                                             rust-log-0.4.27
                                             rust-loom-0.7.2
                                             rust-loop9-0.1.5
                                             rust-lz4-flex-0.10.0
                                             rust-matchers-0.1.0
                                             rust-maybe-rayon-0.1.1
                                             rust-memchr-2.7.4
                                             rust-memmap2-0.8.0
                                             rust-memmap2-0.9.5
                                             rust-memoffset-0.6.5
                                             rust-memoffset-0.9.1
                                             rust-minimal-lexical-0.2.1
                                             rust-miniz-oxide-0.8.7
                                             rust-ndk-0.9.0
                                             rust-ndk-context-0.1.1
                                             rust-ndk-sys-0.6.0+11769913
                                             rust-new-debug-unreachable-1.0.6
                                             rust-nix-0.27.1
                                             rust-nom-7.1.3
                                             rust-noop-proc-macro-0.3.0
                                             rust-nu-ansi-term-0.46.0
                                             rust-num-bigint-0.4.6
                                             rust-num-derive-0.4.2
                                             rust-num-integer-0.1.46
                                             rust-num-rational-0.4.2
                                             rust-num-traits-0.2.19
                                             rust-num-enum-0.7.3
                                             rust-num-enum-derive-0.7.3
                                             rust-objc-sys-0.3.5
                                             rust-objc2-0.5.2
                                             rust-objc2-app-kit-0.2.2
                                             rust-objc2-cloud-kit-0.2.2
                                             rust-objc2-contacts-0.2.2
                                             rust-objc2-core-data-0.2.2
                                             rust-objc2-core-image-0.2.2
                                             rust-objc2-core-location-0.2.2
                                             rust-objc2-encode-4.1.0
                                             rust-objc2-foundation-0.2.2
                                             rust-objc2-link-presentation-0.2.2
                                             rust-objc2-metal-0.2.2
                                             rust-objc2-quartz-core-0.2.2
                                             rust-objc2-symbols-0.2.2
                                             rust-objc2-ui-kit-0.2.2
                                             rust-objc2-uniform-type-identifiers-0.2.2
                                             rust-objc2-user-notifications-0.2.2
                                             rust-once-cell-1.21.3
                                             rust-oorandom-11.1.5
                                             rust-orbclient-0.3.48
                                             rust-overload-0.1.1
                                             rust-parking-lot-0.12.3
                                             rust-parking-lot-core-0.9.10
                                             rust-paste-1.0.15
                                             rust-percent-encoding-2.3.1
                                             rust-pin-project-1.1.10
                                             rust-pin-project-internal-1.1.10
                                             rust-pin-project-lite-0.2.16
                                             rust-pixman-0.2.1
                                             rust-pixman-sys-0.1.0
                                             rust-pkg-config-0.3.32
                                             rust-plotters-0.3.7
                                             rust-plotters-backend-0.3.7
                                             rust-plotters-svg-0.3.7
                                             rust-png-0.17.16
                                             rust-polling-3.7.4
                                             rust-ppv-lite86-0.2.21
                                             rust-prettyplease-0.2.32
                                             rust-proc-macro-crate-3.3.0
                                             rust-proc-macro2-1.0.94
                                             rust-profiling-1.0.16
                                             rust-profiling-procmacros-1.0.16
                                             rust-puffin-0.16.0
                                             rust-puffin-0.19.1
                                             rust-puffin-http-0.13.0
                                             rust-qoi-0.4.1
                                             rust-quick-error-2.0.1
                                             rust-quick-xml-0.37.4
                                             rust-quote-1.0.40
                                             rust-r-efi-5.2.0
                                             rust-rand-0.8.5
                                             rust-rand-chacha-0.3.1
                                             rust-rand-core-0.6.4
                                             rust-rav1e-0.7.1
                                             rust-ravif-0.11.11
                                             rust-raw-window-handle-0.6.2
                                             rust-rayon-1.10.0
                                             rust-rayon-core-1.12.1
                                             rust-redox-syscall-0.4.1
                                             rust-redox-syscall-0.5.11
                                             rust-regex-1.11.1
                                             rust-regex-automata-0.1.10
                                             rust-regex-automata-0.4.9
                                             rust-regex-syntax-0.6.29
                                             rust-regex-syntax-0.8.5
                                             rust-renderdoc-0.11.0
                                             rust-renderdoc-sys-1.1.0
                                             rust-rgb-0.8.50
                                             rust-rustc-hash-1.1.0
                                             rust-rustix-0.38.44
                                             rust-rustix-1.0.5
                                             rust-rustversion-1.0.20
                                             rust-ryu-1.0.20
                                             rust-same-file-1.0.6
                                             rust-scoped-tls-1.0.1
                                             rust-scopeguard-1.2.0
                                             rust-semver-1.0.26
                                             rust-serde-1.0.219
                                             rust-serde-derive-1.0.219
                                             rust-serde-json-1.0.140
                                             rust-serde-spanned-0.6.8
                                             rust-sharded-slab-0.1.7
                                             rust-shlex-1.3.0
                                             rust-simd-adler32-0.3.7
                                             rust-simd-helpers-0.1.0
                                             rust-slab-0.4.9
                                             rust-slotmap-1.0.7
                                             rust-smallvec-1.15.0
                                             rust-smithay-client-toolkit-0.19.2
                                             rust-smol-str-0.2.2
                                             rust-strsim-0.11.1
                                             rust-syn-2.0.100
                                             rust-system-deps-6.2.2
                                             rust-system-deps-7.0.3
                                             rust-target-lexicon-0.12.16
                                             rust-tempfile-3.19.1
                                             rust-thiserror-1.0.69
                                             rust-thiserror-2.0.12
                                             rust-thiserror-impl-1.0.69
                                             rust-thiserror-impl-2.0.12
                                             rust-thread-local-1.1.8
                                             rust-tiff-0.9.1
                                             rust-tinytemplate-1.2.1
                                             rust-toml-0.8.20
                                             rust-toml-datetime-0.6.8
                                             rust-toml-edit-0.22.24
                                             rust-tracing-0.1.41
                                             rust-tracing-attributes-0.1.28
                                             rust-tracing-core-0.1.33
                                             rust-tracing-log-0.2.0
                                             rust-tracing-subscriber-0.3.19
                                             rust-tracy-client-0.17.6
                                             rust-tracy-client-sys-0.24.3
                                             rust-udev-0.9.3
                                             rust-unicode-ident-1.0.18
                                             rust-unicode-segmentation-1.12.0
                                             rust-utf8parse-0.2.2
                                             rust-v-frame-0.3.8
                                             rust-valuable-0.1.1
                                             rust-version-compare-0.2.0
                                             rust-version-check-0.9.5
                                             rust-walkdir-2.5.0
                                             rust-wasi-0.11.0+wasi-snapshot-preview1
                                             rust-wasi-0.14.2+wasi-0.2.4
                                             rust-wasm-bindgen-0.2.100
                                             rust-wasm-bindgen-backend-0.2.100
                                             rust-wasm-bindgen-futures-0.4.50
                                             rust-wasm-bindgen-macro-0.2.100
                                             rust-wasm-bindgen-macro-support-0.2.100
                                             rust-wasm-bindgen-shared-0.2.100
                                             rust-wayland-backend-0.3.8
                                             rust-wayland-client-0.31.8
                                             rust-wayland-csd-frame-0.3.0
                                             rust-wayland-cursor-0.31.8
                                             rust-wayland-egl-0.32.5
                                             rust-wayland-protocols-0.32.6
                                             rust-wayland-protocols-misc-0.3.6
                                             rust-wayland-protocols-plasma-0.3.6
                                             rust-wayland-protocols-wlr-0.3.6
                                             rust-wayland-scanner-0.31.6
                                             rust-wayland-server-0.31.7
                                             rust-wayland-sys-0.31.6
                                             rust-web-sys-0.3.77
                                             rust-web-time-1.1.0
                                             rust-weezl-0.1.8
                                             rust-which-4.4.2
                                             rust-winapi-0.3.9
                                             rust-winapi-i686-pc-windows-gnu-0.4.0
                                             rust-winapi-util-0.1.9
                                             rust-winapi-x86-64-pc-windows-gnu-0.4.0
                                             rust-windows-0.58.0
                                             rust-windows-core-0.58.0
                                             rust-windows-implement-0.58.0
                                             rust-windows-interface-0.58.0
                                             rust-windows-result-0.2.0
                                             rust-windows-strings-0.1.0
                                             rust-windows-sys-0.45.0
                                             rust-windows-sys-0.48.0
                                             rust-windows-sys-0.52.0
                                             rust-windows-sys-0.59.0
                                             rust-windows-targets-0.42.2
                                             rust-windows-targets-0.48.5
                                             rust-windows-targets-0.52.6
                                             rust-windows-aarch64-gnullvm-0.42.2
                                             rust-windows-aarch64-gnullvm-0.48.5
                                             rust-windows-aarch64-gnullvm-0.52.6
                                             rust-windows-aarch64-msvc-0.42.2
                                             rust-windows-aarch64-msvc-0.48.5
                                             rust-windows-aarch64-msvc-0.52.6
                                             rust-windows-i686-gnu-0.42.2
                                             rust-windows-i686-gnu-0.48.5
                                             rust-windows-i686-gnu-0.52.6
                                             rust-windows-i686-gnullvm-0.52.6
                                             rust-windows-i686-msvc-0.42.2
                                             rust-windows-i686-msvc-0.48.5
                                             rust-windows-i686-msvc-0.52.6
                                             rust-windows-x86-64-gnu-0.42.2
                                             rust-windows-x86-64-gnu-0.48.5
                                             rust-windows-x86-64-gnu-0.52.6
                                             rust-windows-x86-64-gnullvm-0.42.2
                                             rust-windows-x86-64-gnullvm-0.48.5
                                             rust-windows-x86-64-gnullvm-0.52.6
                                             rust-windows-x86-64-msvc-0.42.2
                                             rust-windows-x86-64-msvc-0.48.5
                                             rust-windows-x86-64-msvc-0.52.6
                                             rust-winit-0.30.9
                                             rust-winnow-0.7.4
                                             rust-wio-0.2.2
                                             rust-wit-bindgen-rt-0.39.0
                                             rust-wlcs-0.1.0
                                             rust-x11-dl-2.21.0
                                             rust-x11rb-0.13.1
                                             rust-x11rb-protocol-0.13.1
                                             rust-xcursor-0.3.8
                                             rust-xkbcommon-0.7.0
                                             rust-xkbcommon-0.8.0
                                             rust-xkbcommon-dl-0.4.2
                                             rust-xkeysym-0.2.1
                                             rust-xml-rs-0.8.25
                                             rust-zerocopy-0.7.35
                                             rust-zerocopy-0.8.24
                                             rust-zerocopy-derive-0.7.35
                                             rust-zerocopy-derive-0.8.24
                                             rust-zune-core-0.4.12
                                             rust-zune-inflate-0.2.54
                                             rust-zune-jpeg-0.4.14))
                     (transanno =>
                                (list rust-addr2line-0.24.2
                                      rust-adler2-2.0.0
                                      rust-aho-corasick-1.1.3
                                      rust-anstream-0.6.18
                                      rust-anstyle-1.0.10
                                      rust-anstyle-parse-0.2.6
                                      rust-anstyle-query-1.1.2
                                      rust-anstyle-wincon-3.0.7
                                      rust-anyhow-1.0.97
                                      rust-approx-0.5.1
                                      rust-autocfg-1.4.0
                                      rust-autocompress-0.6.0
                                      rust-backtrace-0.3.74
                                      rust-bgzip-0.3.1
                                      rust-bio-1.6.0
                                      rust-bio-types-1.0.4
                                      rust-bit-set-0.5.3
                                      rust-bit-vec-0.6.3
                                      rust-bitflags-2.9.0
                                      rust-bv-0.11.1
                                      rust-bytecount-0.6.8
                                      rust-bytemuck-1.22.0
                                      rust-byteorder-1.5.0
                                      rust-bytes-1.10.1
                                      rust-bzip2-0.4.4
                                      rust-bzip2-sys-0.1.13+1.0.8
                                      rust-cc-1.2.18
                                      rust-cfg-if-1.0.0
                                      rust-clap-4.5.35
                                      rust-clap-builder-4.5.35
                                      rust-clap-derive-4.5.32
                                      rust-clap-lex-0.7.4
                                      rust-colorchoice-1.0.3
                                      rust-crc32fast-1.4.2
                                      rust-crossbeam-deque-0.8.6
                                      rust-crossbeam-epoch-0.9.18
                                      rust-crossbeam-utils-0.8.21
                                      rust-csv-1.3.1
                                      rust-csv-core-0.1.12
                                      rust-custom-derive-0.1.7
                                      rust-derive-new-0.6.0
                                      rust-editdistancek-1.0.2
                                      rust-either-1.15.0
                                      rust-enum-map-2.7.3
                                      rust-enum-map-derive-0.17.0
                                      rust-env-logger-0.10.2
                                      rust-equivalent-1.0.2
                                      rust-feature-probe-0.1.1
                                      rust-fixedbitset-0.4.2
                                      rust-flate2-1.1.1
                                      rust-fxhash-0.2.1
                                      rust-getrandom-0.2.15
                                      rust-getrandom-0.3.2
                                      rust-gimli-0.31.1
                                      rust-hashbrown-0.15.2
                                      rust-heck-0.4.1
                                      rust-heck-0.5.0
                                      rust-hermit-abi-0.5.0
                                      rust-humantime-2.2.0
                                      rust-indexmap-2.9.0
                                      rust-is-terminal-0.4.16
                                      rust-is-terminal-polyfill-1.70.1
                                      rust-itertools-0.11.0
                                      rust-itertools-num-0.1.3
                                      rust-itoa-1.0.15
                                      rust-jobserver-0.1.33
                                      rust-lazy-static-1.5.0
                                      rust-libc-0.2.171
                                      rust-libm-0.2.11
                                      rust-log-0.4.27
                                      rust-lzma-sys-0.1.20
                                      rust-matrixmultiply-0.3.9
                                      rust-memchr-2.7.4
                                      rust-minimal-lexical-0.2.1
                                      rust-miniz-oxide-0.8.8
                                      rust-multimap-0.9.1
                                      rust-nalgebra-0.29.0
                                      rust-nalgebra-macros-0.1.0
                                      rust-ndarray-0.15.6
                                      rust-newtype-derive-0.1.6
                                      rust-nom-7.1.3
                                      rust-num-complex-0.4.6
                                      rust-num-integer-0.1.46
                                      rust-num-rational-0.4.2
                                      rust-num-traits-0.2.19
                                      rust-object-0.36.7
                                      rust-once-cell-1.21.3
                                      rust-ordered-float-3.9.2
                                      rust-paste-1.0.15
                                      rust-petgraph-0.6.5
                                      rust-pin-project-lite-0.2.16
                                      rust-pkg-config-0.3.32
                                      rust-ppv-lite86-0.2.21
                                      rust-pretty-env-logger-0.5.0
                                      rust-proc-macro2-1.0.94
                                      rust-quote-1.0.40
                                      rust-r-efi-5.2.0
                                      rust-rand-0.8.5
                                      rust-rand-chacha-0.3.1
                                      rust-rand-core-0.6.4
                                      rust-rand-distr-0.4.3
                                      rust-rawpointer-0.2.1
                                      rust-rayon-1.10.0
                                      rust-rayon-core-1.12.1
                                      rust-regex-1.11.1
                                      rust-regex-automata-0.4.9
                                      rust-regex-syntax-0.8.5
                                      rust-rustc-demangle-0.1.24
                                      rust-rustc-version-0.1.7
                                      rust-rustversion-1.0.20
                                      rust-ryu-1.0.20
                                      rust-safe-arch-0.7.4
                                      rust-semver-0.1.20
                                      rust-serde-1.0.219
                                      rust-serde-derive-1.0.219
                                      rust-serde-json-1.0.140
                                      rust-shlex-1.3.0
                                      rust-simba-0.6.0
                                      rust-statrs-0.16.1
                                      rust-strsim-0.11.1
                                      rust-strum-0.25.0
                                      rust-strum-macros-0.25.3
                                      rust-strum-macros-0.26.4
                                      rust-syn-1.0.109
                                      rust-syn-2.0.100
                                      rust-termcolor-1.4.1
                                      rust-thiserror-1.0.69
                                      rust-thiserror-impl-1.0.69
                                      rust-tokio-1.44.2
                                      rust-triple-accel-0.4.0
                                      rust-typenum-1.18.0
                                      rust-unicode-ident-1.0.18
                                      rust-utf8parse-0.2.2
                                      rust-vec-map-0.8.2
                                      rust-wasi-0.11.0+wasi-snapshot-preview1
                                      rust-wasi-0.14.2+wasi-0.2.4
                                      rust-wide-0.7.32
                                      rust-winapi-util-0.1.9
                                      rust-windows-sys-0.59.0
                                      rust-windows-targets-0.52.6
                                      rust-windows-aarch64-gnullvm-0.52.6
                                      rust-windows-aarch64-msvc-0.52.6
                                      rust-windows-i686-gnu-0.52.6
                                      rust-windows-i686-gnullvm-0.52.6
                                      rust-windows-i686-msvc-0.52.6
                                      rust-windows-x86-64-gnu-0.52.6
                                      rust-windows-x86-64-gnullvm-0.52.6
                                      rust-windows-x86-64-msvc-0.52.6
                                      rust-wit-bindgen-rt-0.39.0
                                      rust-xz2-0.1.7
                                      rust-zerocopy-0.8.24
                                      rust-zerocopy-derive-0.8.24
                                      rust-zstd-0.13.3
                                      rust-zstd-safe-7.2.4
                                      rust-zstd-sys-2.0.15+zstd.1.5.7))
                     (uv =>
                         (list rust-addr2line-0.24.2
                          rust-adler2-2.0.0
                          rust-aho-corasick-1.1.3
                          rust-allocator-api2-0.2.21
                          rust-anstream-0.6.18
                          rust-anstyle-1.0.10
                          rust-anstyle-parse-0.2.6
                          rust-anstyle-query-1.1.2
                          rust-anstyle-wincon-3.0.7
                          rust-anyhow-1.0.97
                          rust-arbitrary-1.4.1
                          rust-arcstr-1.2.0
                          rust-assert-json-diff-2.0.2
                          rust-assert-cmd-2.0.16
                          rust-assert-fs-1.1.2
                          rust-astral-tokio-tar-0.5.2
                          rust-async-channel-2.3.1
                          rust-async-compression-0.4.19
                          rust-async-trait-0.1.88
                          rust-async-http-range-reader-0.9.1
                          rust-async-zip-0.0.17.c909fda
                          rust-atomic-waker-1.1.2
                          rust-autocfg-1.4.0
                          rust-axoasset-1.2.0
                          rust-axoprocess-0.2.0
                          rust-axotag-0.2.0
                          rust-axoupdater-0.9.0
                          rust-backon-1.4.1
                          rust-backtrace-0.3.74
                          rust-base64-0.22.1
                          rust-bisection-0.1.0
                          rust-bitflags-1.3.2
                          rust-bitflags-2.9.0
                          rust-block-buffer-0.10.4
                          rust-boxcar-0.2.11
                          rust-bstr-1.11.3
                          rust-bumpalo-3.17.0
                          rust-bytecheck-0.8.1
                          rust-bytecheck-derive-0.8.1
                          rust-bytemuck-1.22.0
                          rust-byteorder-1.5.0
                          rust-byteorder-lite-0.1.0
                          rust-bytes-1.10.1
                          rust-bzip2-0.5.2
                          rust-bzip2-sys-0.1.13+1.0.8
                          rust-camino-1.1.9
                          rust-cargo-util-0.2.19
                          rust-cc-1.2.18
                          rust-cfg-if-1.0.0
                          rust-cfg-aliases-0.2.1
                          rust-charset-0.1.5
                          rust-clap-4.5.35
                          rust-clap-builder-4.5.35
                          rust-clap-complete-4.5.47
                          rust-clap-complete-command-0.6.1
                          rust-clap-complete-nushell-4.5.5
                          rust-clap-derive-4.5.32
                          rust-clap-lex-0.7.4
                          rust-colorchoice-1.0.3
                          rust-concurrent-queue-2.5.0
                          rust-configparser-3.1.0
                          rust-console-0.15.11
                          rust-core-foundation-0.10.0
                          rust-core-foundation-sys-0.8.7
                          rust-cpufeatures-0.2.17
                          rust-crc32fast-1.4.2
                          rust-crossbeam-deque-0.8.6
                          rust-crossbeam-epoch-0.9.18
                          rust-crossbeam-utils-0.8.21
                          rust-crypto-common-0.1.6
                          rust-csv-1.3.1
                          rust-csv-core-0.1.12
                          rust-ctrlc-3.4.6
                          rust-dashmap-6.1.0
                          rust-data-encoding-2.8.0
                          rust-deadpool-0.10.0
                          rust-deadpool-runtime-0.1.4
                          rust-derive-arbitrary-1.4.1
                          rust-difflib-0.4.0
                          rust-digest-0.10.7
                          rust-dirs-5.0.1
                          rust-dirs-sys-0.4.1
                          rust-displaydoc-0.2.5
                          rust-doc-comment-0.3.3
                          rust-dotenvy-0.15.7
                          rust-dunce-1.0.5
                          rust-dyn-clone-1.0.19
                          rust-either-1.15.0
                          rust-encode-unicode-1.0.0
                          rust-encoding-rs-0.8.35
                          rust-encoding-rs-io-0.1.7
                          rust-env-home-0.1.0
                          rust-equivalent-1.0.2
                          rust-erased-serde-0.4.6
                          rust-errno-0.3.11
                          rust-etcetera-0.10.0
                          rust-event-listener-5.4.0
                          rust-event-listener-strategy-0.5.4
                          rust-fastrand-2.3.0
                          rust-filetime-0.2.25
                          rust-fixedbitset-0.5.7
                          rust-flate2-1.1.1
                          rust-float-cmp-0.10.0
                          rust-fnv-1.0.7
                          rust-foldhash-0.1.5
                          rust-form-urlencoded-1.2.1
                          rust-fs-err-2.11.0
                          rust-fs-err-3.1.0
                          rust-fs2-0.4.3
                          rust-futures-0.3.31
                          rust-futures-channel-0.3.31
                          rust-futures-core-0.3.31
                          rust-futures-executor-0.3.31
                          rust-futures-io-0.3.31
                          rust-futures-lite-2.6.0
                          rust-futures-macro-0.3.31
                          rust-futures-sink-0.3.31
                          rust-futures-task-0.3.31
                          rust-futures-util-0.3.31
                          rust-generator-0.8.4
                          rust-generic-array-0.14.7
                          rust-getrandom-0.2.15
                          rust-getrandom-0.3.2
                          rust-gimli-0.31.1
                          rust-glob-0.3.2
                          rust-globset-0.4.16
                          rust-globwalk-0.9.1
                          rust-gloo-timers-0.3.0
                          rust-goblin-0.9.3
                          rust-h2-0.4.8
                          rust-hashbrown-0.14.5
                          rust-hashbrown-0.15.2
                          rust-heck-0.5.0
                          rust-hermit-abi-0.3.9
                          rust-hex-0.4.3
                          rust-home-0.5.11
                          rust-homedir-0.3.4
                          rust-html-escape-0.2.13
                          rust-http-1.3.1
                          rust-http-body-1.0.1
                          rust-http-body-util-0.1.3
                          rust-http-content-range-0.2.1
                          rust-httparse-1.10.1
                          rust-httpdate-1.0.3
                          rust-hyper-1.6.0
                          rust-hyper-rustls-0.27.5
                          rust-hyper-util-0.1.11
                          rust-icu-collections-1.5.0
                          rust-icu-locid-1.5.0
                          rust-icu-locid-transform-1.5.0
                          rust-icu-locid-transform-data-1.5.1
                          rust-icu-normalizer-1.5.0
                          rust-icu-normalizer-data-1.5.1
                          rust-icu-properties-1.5.1
                          rust-icu-properties-data-1.5.1
                          rust-icu-provider-1.5.0
                          rust-icu-provider-macros-1.5.0
                          rust-idna-1.0.3
                          rust-idna-adapter-1.2.0
                          rust-ignore-0.4.23
                          rust-image-0.25.6
                          rust-indexmap-2.9.0
                          rust-indicatif-0.17.11
                          rust-indoc-2.0.6
                          rust-insta-1.42.2
                          rust-instant-0.1.13
                          rust-ipnet-2.11.0
                          rust-is-ci-1.2.0
                          rust-is-terminal-polyfill-1.70.1
                          rust-itertools-0.13.0
                          rust-itertools-0.14.0
                          rust-itoa-1.0.15
                          rust-jiff-0.2.5
                          rust-jiff-static-0.2.5
                          rust-jiff-tzdb-0.1.4
                          rust-jiff-tzdb-platform-0.1.3
                          rust-jobserver-0.1.33
                          rust-js-sys-0.3.77
                          rust-junction-1.2.0
                          rust-lazy-static-1.5.0
                          rust-libc-0.2.171
                          rust-libmimalloc-sys-0.1.41
                          rust-libredox-0.1.3
                          rust-libz-rs-sys-0.5.0
                          rust-linked-hash-map-0.5.6
                          rust-linux-raw-sys-0.4.15
                          rust-linux-raw-sys-0.9.3
                          rust-litemap-0.7.5
                          rust-lock-api-0.4.12
                          rust-lockfree-object-pool-0.1.6
                          rust-log-0.4.27
                          rust-loom-0.7.2
                          rust-lzma-sys-0.1.20
                          rust-mailparse-0.16.1
                          rust-matchers-0.1.0
                          rust-md-5-0.10.6
                          rust-memchr-2.7.4
                          rust-memmap2-0.9.5
                          rust-miette-7.5.0
                          rust-miette-derive-7.5.0
                          rust-mimalloc-0.1.45
                          rust-mime-0.3.17
                          rust-mime-guess-2.0.5
                          rust-miniz-oxide-0.8.7
                          rust-mio-1.0.3
                          rust-miow-0.6.0
                          rust-munge-0.4.3
                          rust-munge-macro-0.4.3
                          rust-nanoid-0.4.0
                          rust-nix-0.29.0
                          rust-normalize-line-endings-0.3.0
                          rust-nu-ansi-term-0.46.0
                          rust-nu-ansi-term-0.50.1
                          rust-num-traits-0.2.19
                          rust-num-cpus-1.16.0
                          rust-number-prefix-0.4.0
                          rust-object-0.36.7
                          rust-once-cell-1.21.3
                          rust-openssl-probe-0.1.6
                          rust-option-ext-0.2.0
                          rust-os-str-bytes-6.6.1
                          rust-overload-0.1.1
                          rust-owo-colors-4.2.0
                          rust-parking-2.2.1
                          rust-parking-lot-0.11.2
                          rust-parking-lot-0.12.3
                          rust-parking-lot-core-0.8.6
                          rust-parking-lot-core-0.9.10
                          rust-paste-1.0.15
                          rust-path-slash-0.2.1
                          rust-pathdiff-0.2.3
                          rust-percent-encoding-2.3.1
                          rust-pest-2.8.0
                          rust-pest-derive-2.8.0
                          rust-pest-generator-2.8.0
                          rust-pest-meta-2.8.0
                          rust-petgraph-0.7.1
                          rust-pin-project-1.1.10
                          rust-pin-project-internal-1.1.10
                          rust-pin-project-lite-0.2.16
                          rust-pin-utils-0.1.0
                          rust-pkg-config-0.3.32
                          rust-plain-0.2.3
                          rust-portable-atomic-1.11.0
                          rust-portable-atomic-util-0.2.4
                          rust-ppv-lite86-0.2.21
                          rust-predicates-3.1.3
                          rust-predicates-core-1.0.9
                          rust-predicates-tree-1.0.12
                          rust-priority-queue-2.3.1
                          rust-proc-macro2-1.0.94
                          rust-procfs-0.17.0
                          rust-procfs-core-0.17.0
                          rust-ptr-meta-0.3.0
                          rust-ptr-meta-derive-0.3.0
                          rust-pubgrub-0.3.0-alpha.1.b70cf70
                          rust-quinn-0.11.7
                          rust-quinn-proto-0.11.10
                          rust-quinn-udp-0.5.11
                          rust-quote-1.0.40
                          rust-quoted-printable-0.5.1
                          rust-r-efi-5.2.0
                          rust-rancor-0.1.0
                          rust-rand-0.8.5
                          rust-rand-0.9.0
                          rust-rand-chacha-0.3.1
                          rust-rand-chacha-0.9.0
                          rust-rand-core-0.6.4
                          rust-rand-core-0.9.3
                          rust-rayon-1.10.0
                          rust-rayon-core-1.12.1
                          rust-redox-syscall-0.2.16
                          rust-redox-syscall-0.5.11
                          rust-redox-users-0.4.6
                          rust-reflink-copy-0.1.26
                          rust-regex-1.11.1
                          rust-regex-automata-0.1.10
                          rust-regex-automata-0.4.9
                          rust-regex-syntax-0.6.29
                          rust-regex-syntax-0.8.5
                          rust-rend-0.5.2
                          rust-reqwest-0.12.15
                          rust-reqwest-middleware-0.4.1
                          rust-reqwest-retry-0.7.0
                          rust-retry-policies-0.4.0
                          rust-ring-0.17.14
                          rust-rkyv-0.8.10
                          rust-rkyv-derive-0.8.10
                          rust-rmp-0.8.14
                          rust-rmp-serde-1.3.0
                          rust-rust-netrc-0.1.2
                          rust-rustc-demangle-0.1.24
                          rust-rustc-hash-2.1.1
                          rust-rustix-0.38.44
                          rust-rustix-1.0.5
                          rust-rustls-0.23.25
                          rust-rustls-native-certs-0.8.1
                          rust-rustls-pemfile-2.2.0
                          rust-rustls-pki-types-1.11.0
                          rust-rustls-webpki-0.103.1
                          rust-rustversion-1.0.20
                          rust-ryu-1.0.20
                          rust-same-file-1.0.6
                          rust-schannel-0.1.27
                          rust-schemars-0.8.22
                          rust-schemars-derive-0.8.22
                          rust-scoped-tls-1.0.1
                          rust-scopeguard-1.2.0
                          rust-scroll-0.12.0
                          rust-scroll-derive-0.12.1
                          rust-seahash-4.1.0
                          rust-security-framework-3.2.0
                          rust-security-framework-sys-2.14.0
                          rust-self-replace-1.5.0
                          rust-semver-1.0.26
                          rust-serde-1.0.219
                          rust-serde-untagged-0.1.7
                          rust-serde-derive-1.0.219
                          rust-serde-derive-internals-0.29.1
                          rust-serde-json-1.0.140
                          rust-serde-spanned-0.6.8
                          rust-serde-urlencoded-0.7.1
                          rust-sha2-0.10.8
                          rust-sharded-slab-0.1.7
                          rust-shell-escape-0.1.5
                          rust-shellexpand-3.1.0
                          rust-shlex-1.3.0
                          rust-signal-hook-registry-1.4.2
                          rust-simd-adler32-0.3.7
                          rust-simdutf8-0.1.5
                          rust-similar-2.7.0
                          rust-slab-0.4.9
                          rust-smallvec-1.15.0
                          rust-smawk-0.3.2
                          rust-socket2-0.5.9
                          rust-spdx-0.10.8
                          rust-stable-deref-trait-1.2.0
                          rust-strsim-0.11.1
                          rust-subtle-2.6.1
                          rust-supports-color-3.0.2
                          rust-supports-hyperlinks-3.1.0
                          rust-supports-unicode-3.0.0
                          rust-svg-0.17.0
                          rust-syn-2.0.100
                          rust-sync-wrapper-1.0.2
                          rust-synstructure-0.13.1
                          rust-sys-info-0.9.1
                          rust-tar-0.4.44
                          rust-target-lexicon-0.13.2
                          rust-temp-env-0.3.6
                          rust-tempfile-3.19.1
                          rust-terminal-size-0.4.2
                          rust-termtree-0.5.1
                          rust-test-case-3.3.1
                          rust-test-case-core-3.3.1
                          rust-test-case-macros-3.3.1
                          rust-test-log-0.2.17
                          rust-test-log-macros-0.2.17
                          rust-textwrap-0.16.2
                          rust-thiserror-1.0.69
                          rust-thiserror-2.0.12
                          rust-thiserror-impl-1.0.69
                          rust-thiserror-impl-2.0.12
                          rust-thread-local-1.1.8
                          rust-tikv-jemalloc-sys-0.6.0+5.3.0-1-ge13ca993e8ccb9ba9847cc330696e02839f328f7
                          rust-tikv-jemallocator-0.6.0
                          rust-tinystr-0.7.6
                          rust-tinyvec-1.9.0
                          rust-tinyvec-macros-0.1.1
                          rust-tl-0.7.8.6e25b2e
                          rust-tokio-1.44.2
                          rust-tokio-macros-2.5.0
                          rust-tokio-rustls-0.26.2
                          rust-tokio-socks-0.5.2
                          rust-tokio-stream-0.1.17
                          rust-tokio-util-0.7.14
                          rust-toml-0.8.20
                          rust-toml-datetime-0.6.8
                          rust-toml-edit-0.22.24
                          rust-tower-0.5.2
                          rust-tower-layer-0.3.3
                          rust-tower-service-0.3.3
                          rust-tracing-0.1.41
                          rust-tracing-attributes-0.1.28
                          rust-tracing-core-0.1.33
                          rust-tracing-durations-export-0.3.0
                          rust-tracing-log-0.2.0
                          rust-tracing-serde-0.2.0
                          rust-tracing-subscriber-0.3.19
                          rust-tracing-test-0.2.5
                          rust-tracing-test-macro-0.2.5
                          rust-tracing-tree-0.4.0
                          rust-try-lock-0.2.5
                          rust-typeid-1.0.3
                          rust-typenum-1.18.0
                          rust-ucd-trie-0.1.7
                          rust-unicase-2.8.1
                          rust-unicode-ident-1.0.18
                          rust-unicode-linebreak-0.1.5
                          rust-unicode-width-0.1.14
                          rust-unicode-width-0.2.0
                          rust-unscanny-0.1.0
                          rust-untrusted-0.9.0
                          rust-url-2.5.4
                          rust-utf16-iter-1.0.5
                          rust-utf8-width-0.1.7
                          rust-utf8-iter-1.0.4
                          rust-utf8parse-0.2.2
                          rust-uuid-1.16.0
                          rust-valuable-0.1.1
                          rust-version-ranges-0.1.1.b70cf70
                          rust-version-check-0.9.5
                          rust-wait-timeout-0.2.1
                          rust-walkdir-2.5.0
                          rust-want-0.3.1
                          rust-wasi-0.11.0+wasi-snapshot-preview1
                          rust-wasi-0.14.2+wasi-0.2.4
                          rust-wasm-bindgen-0.2.100
                          rust-wasm-bindgen-backend-0.2.100
                          rust-wasm-bindgen-futures-0.4.50
                          rust-wasm-bindgen-macro-0.2.100
                          rust-wasm-bindgen-macro-support-0.2.100
                          rust-wasm-bindgen-shared-0.2.100
                          rust-wasm-streams-0.4.2
                          rust-wasm-timer-0.2.5
                          rust-web-sys-0.3.77
                          rust-web-time-1.1.0
                          rust-webpki-roots-0.26.8
                          rust-which-7.0.2
                          rust-widestring-1.2.0
                          rust-winapi-0.3.9
                          rust-winapi-i686-pc-windows-gnu-0.4.0
                          rust-winapi-util-0.1.9
                          rust-winapi-x86-64-pc-windows-gnu-0.4.0
                          rust-windows-0.57.0
                          rust-windows-0.58.0
                          rust-windows-0.61.1
                          rust-windows-collections-0.2.0
                          rust-windows-core-0.57.0
                          rust-windows-core-0.58.0
                          rust-windows-core-0.61.0
                          rust-windows-future-0.2.0
                          rust-windows-implement-0.57.0
                          rust-windows-implement-0.58.0
                          rust-windows-implement-0.60.0
                          rust-windows-interface-0.57.0
                          rust-windows-interface-0.58.0
                          rust-windows-interface-0.59.1
                          rust-windows-link-0.1.1
                          rust-windows-numerics-0.2.0
                          rust-windows-registry-0.4.0
                          rust-windows-registry-0.5.1
                          rust-windows-result-0.1.2
                          rust-windows-result-0.2.0
                          rust-windows-result-0.3.2
                          rust-windows-strings-0.1.0
                          rust-windows-strings-0.3.1
                          rust-windows-strings-0.4.0
                          rust-windows-sys-0.48.0
                          rust-windows-sys-0.52.0
                          rust-windows-sys-0.59.0
                          rust-windows-targets-0.48.5
                          rust-windows-targets-0.52.6
                          rust-windows-targets-0.53.0
                          rust-windows-aarch64-gnullvm-0.48.5
                          rust-windows-aarch64-gnullvm-0.52.6
                          rust-windows-aarch64-gnullvm-0.53.0
                          rust-windows-aarch64-msvc-0.48.5
                          rust-windows-aarch64-msvc-0.52.6
                          rust-windows-aarch64-msvc-0.53.0
                          rust-windows-i686-gnu-0.48.5
                          rust-windows-i686-gnu-0.52.6
                          rust-windows-i686-gnu-0.53.0
                          rust-windows-i686-gnullvm-0.52.6
                          rust-windows-i686-gnullvm-0.53.0
                          rust-windows-i686-msvc-0.48.5
                          rust-windows-i686-msvc-0.52.6
                          rust-windows-i686-msvc-0.53.0
                          rust-windows-x86-64-gnu-0.48.5
                          rust-windows-x86-64-gnu-0.52.6
                          rust-windows-x86-64-gnu-0.53.0
                          rust-windows-x86-64-gnullvm-0.48.5
                          rust-windows-x86-64-gnullvm-0.52.6
                          rust-windows-x86-64-gnullvm-0.53.0
                          rust-windows-x86-64-msvc-0.48.5
                          rust-windows-x86-64-msvc-0.52.6
                          rust-windows-x86-64-msvc-0.53.0
                          rust-winnow-0.7.4
                          rust-winsafe-0.0.19
                          rust-winsafe-0.0.23
                          rust-wiremock-0.6.3
                          rust-wit-bindgen-rt-0.39.0
                          rust-write16-1.0.0
                          rust-writeable-0.5.5
                          rust-xattr-1.5.0
                          rust-xz2-0.1.7
                          rust-yoke-0.7.5
                          rust-yoke-derive-0.7.5
                          rust-zerocopy-0.8.24
                          rust-zerocopy-derive-0.8.24
                          rust-zerofrom-0.1.6
                          rust-zerofrom-derive-0.1.6
                          rust-zeroize-1.8.1
                          rust-zerovec-0.10.4
                          rust-zerovec-derive-0.10.3
                          rust-zip-2.6.1
                          rust-zlib-rs-0.5.0
                          rust-zopfli-0.8.1
                          rust-zstd-0.13.3
                          rust-zstd-safe-7.2.4
                          rust-zstd-sys-2.0.15+zstd.1.5.7))
                     (wlgreet =>
                              (list rust-ab-glyph-rasterizer-0.1.8
                                    rust-android-tzdata-0.1.1
                                    rust-android-system-properties-0.1.5
                                    rust-autocfg-1.4.0
                                    rust-bitflags-1.3.2
                                    rust-bumpalo-3.17.0
                                    rust-calloop-0.9.3
                                    rust-cc-1.2.18
                                    rust-cfg-if-1.0.0
                                    rust-chrono-0.4.40
                                    rust-core-foundation-sys-0.8.7
                                    rust-dlib-0.5.2
                                    rust-downcast-rs-1.2.1
                                    rust-getopts-0.2.21
                                    rust-greetd-ipc-0.10.3
                                    rust-iana-time-zone-0.1.63
                                    rust-iana-time-zone-haiku-0.1.2
                                    rust-itoa-1.0.15
                                    rust-js-sys-0.3.77
                                    rust-lazy-static-1.5.0
                                    rust-libc-0.2.171
                                    rust-libloading-0.8.6
                                    rust-log-0.4.27
                                    rust-memchr-2.7.4
                                    rust-memmap2-0.3.1
                                    rust-memoffset-0.6.5
                                    rust-nix-0.22.3
                                    rust-nix-0.24.3
                                    rust-nix-0.25.1
                                    rust-num-traits-0.2.19
                                    rust-once-cell-1.21.3
                                    rust-os-pipe-1.2.1
                                    rust-owned-ttf-parser-0.15.2
                                    rust-pin-utils-0.1.0
                                    rust-pkg-config-0.3.32
                                    rust-proc-macro2-1.0.94
                                    rust-quote-1.0.40
                                    rust-rusttype-0.9.3
                                    rust-rustversion-1.0.20
                                    rust-ryu-1.0.20
                                    rust-scoped-tls-1.0.1
                                    rust-serde-1.0.219
                                    rust-serde-derive-1.0.219
                                    rust-serde-json-1.0.140
                                    rust-shlex-1.3.0
                                    rust-smallvec-1.15.0
                                    rust-smithay-client-toolkit-0.15.4
                                    rust-syn-2.0.100
                                    rust-thiserror-1.0.69
                                    rust-thiserror-impl-1.0.69
                                    rust-toml-0.5.11
                                    rust-ttf-parser-0.15.2
                                    rust-unicode-ident-1.0.18
                                    rust-unicode-width-0.1.14
                                    rust-wasm-bindgen-0.2.100
                                    rust-wasm-bindgen-backend-0.2.100
                                    rust-wasm-bindgen-macro-0.2.100
                                    rust-wasm-bindgen-macro-support-0.2.100
                                    rust-wasm-bindgen-shared-0.2.100
                                    rust-wayland-client-0.29.5
                                    rust-wayland-commons-0.29.5
                                    rust-wayland-cursor-0.29.5
                                    rust-wayland-protocols-0.29.5
                                    rust-wayland-scanner-0.29.5
                                    rust-wayland-sys-0.29.5
                                    rust-windows-core-0.61.0
                                    rust-windows-implement-0.60.0
                                    rust-windows-interface-0.59.1
                                    rust-windows-link-0.1.1
                                    rust-windows-result-0.3.2
                                    rust-windows-strings-0.4.0
                                    rust-windows-sys-0.59.0
                                    rust-windows-targets-0.52.6
                                    rust-windows-aarch64-gnullvm-0.52.6
                                    rust-windows-aarch64-msvc-0.52.6
                                    rust-windows-i686-gnu-0.52.6
                                    rust-windows-i686-gnullvm-0.52.6
                                    rust-windows-i686-msvc-0.52.6
                                    rust-windows-x86-64-gnu-0.52.6
                                    rust-windows-x86-64-gnullvm-0.52.6
                                    rust-windows-x86-64-msvc-0.52.6
                                    rust-xcursor-0.3.8
                                    rust-xml-rs-0.8.25))
                     (zoxide =>
                             (list rust-aho-corasick-1.1.3
                                   rust-aliasable-0.1.3
                                   rust-anstream-0.6.18
                                   rust-anstyle-1.0.10
                                   rust-anstyle-parse-0.2.6
                                   rust-anstyle-query-1.1.2
                                   rust-anstyle-wincon-3.0.7
                                   rust-anyhow-1.0.97
                                   rust-assert-cmd-2.0.16
                                   rust-bincode-1.3.3
                                   rust-bitflags-2.9.0
                                   rust-bstr-1.11.3
                                   rust-cfg-if-1.0.0
                                   rust-cfg-aliases-0.2.1
                                   rust-clap-4.5.35
                                   rust-clap-builder-4.5.35
                                   rust-clap-complete-4.5.47
                                   rust-clap-complete-fig-4.5.2
                                   rust-clap-derive-4.5.32
                                   rust-clap-lex-0.7.4
                                   rust-color-print-0.3.7
                                   rust-color-print-proc-macro-0.3.7
                                   rust-colorchoice-1.0.3
                                   rust-difflib-0.4.0
                                   rust-dirs-5.0.1
                                   rust-dirs-sys-0.4.1
                                   rust-doc-comment-0.3.3
                                   rust-dunce-1.0.5
                                   rust-either-1.15.0
                                   rust-errno-0.3.11
                                   rust-fastrand-2.3.0
                                   rust-getrandom-0.2.15
                                   rust-getrandom-0.3.2
                                   rust-glob-0.3.2
                                   rust-heck-0.4.1
                                   rust-heck-0.5.0
                                   rust-home-0.5.11
                                   rust-is-terminal-polyfill-1.70.1
                                   rust-itoa-1.0.15
                                   rust-libc-0.2.171
                                   rust-libredox-0.1.3
                                   rust-linux-raw-sys-0.4.15
                                   rust-linux-raw-sys-0.9.3
                                   rust-memchr-2.7.4
                                   rust-mime-0.3.17
                                   rust-mime-guess-2.0.5
                                   rust-minimal-lexical-0.2.1
                                   rust-nix-0.29.0
                                   rust-nom-7.1.3
                                   rust-once-cell-1.21.3
                                   rust-option-ext-0.2.0
                                   rust-ouroboros-0.18.5
                                   rust-ouroboros-macro-0.18.5
                                   rust-ppv-lite86-0.2.21
                                   rust-predicates-3.1.3
                                   rust-predicates-core-1.0.9
                                   rust-predicates-tree-1.0.12
                                   rust-proc-macro2-1.0.94
                                   rust-proc-macro2-diagnostics-0.10.1
                                   rust-quote-1.0.40
                                   rust-r-efi-5.2.0
                                   rust-rand-0.8.5
                                   rust-rand-chacha-0.3.1
                                   rust-rand-core-0.6.4
                                   rust-redox-users-0.4.6
                                   rust-regex-1.11.1
                                   rust-regex-automata-0.4.9
                                   rust-regex-syntax-0.8.5
                                   rust-relative-path-1.9.3
                                   rust-rinja-0.3.5
                                   rust-rinja-derive-0.3.5
                                   rust-rinja-parser-0.3.5
                                   rust-rstest-0.23.0
                                   rust-rstest-macros-0.23.0
                                   rust-rstest-reuse-0.7.0
                                   rust-rustc-hash-2.1.1
                                   rust-rustc-version-0.4.1
                                   rust-rustix-0.38.44
                                   rust-rustix-1.0.5
                                   rust-semver-1.0.26
                                   rust-serde-1.0.219
                                   rust-serde-derive-1.0.219
                                   rust-static-assertions-1.1.0
                                   rust-strsim-0.11.1
                                   rust-syn-2.0.100
                                   rust-tempfile-3.19.1
                                   rust-termtree-0.5.1
                                   rust-thiserror-1.0.69
                                   rust-thiserror-impl-1.0.69
                                   rust-unicase-2.8.1
                                   rust-unicode-ident-1.0.18
                                   rust-utf8parse-0.2.2
                                   rust-version-check-0.9.5
                                   rust-wait-timeout-0.2.1
                                   rust-wasi-0.11.0+wasi-snapshot-preview1
                                   rust-wasi-0.14.2+wasi-0.2.4
                                   rust-which-6.0.3
                                   rust-windows-sys-0.48.0
                                   rust-windows-sys-0.59.0
                                   rust-windows-targets-0.48.5
                                   rust-windows-targets-0.52.6
                                   rust-windows-aarch64-gnullvm-0.48.5
                                   rust-windows-aarch64-gnullvm-0.52.6
                                   rust-windows-aarch64-msvc-0.48.5
                                   rust-windows-aarch64-msvc-0.52.6
                                   rust-windows-i686-gnu-0.48.5
                                   rust-windows-i686-gnu-0.52.6
                                   rust-windows-i686-gnullvm-0.52.6
                                   rust-windows-i686-msvc-0.48.5
                                   rust-windows-i686-msvc-0.52.6
                                   rust-windows-x86-64-gnu-0.48.5
                                   rust-windows-x86-64-gnu-0.52.6
                                   rust-windows-x86-64-gnullvm-0.48.5
                                   rust-windows-x86-64-gnullvm-0.52.6
                                   rust-windows-x86-64-msvc-0.48.5
                                   rust-windows-x86-64-msvc-0.52.6
                                   rust-winsafe-0.0.19
                                   rust-wit-bindgen-rt-0.39.0
                                   rust-yansi-1.0.1
                                   rust-zerocopy-0.8.24
                                   rust-zerocopy-derive-0.8.24)))
