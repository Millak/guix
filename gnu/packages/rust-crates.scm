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

(define rust-ahash-0.8.11
  (crate-source "ahash" "0.8.11"
                "04chdfkls5xmhp1d48gnjsmglbqibizs3bpbj6rsj604m10si7g8"))

(define rust-aho-corasick-1.1.3
  (crate-source "aho-corasick" "1.1.3"
                "05mrpkvdgp5d20y2p989f187ry9diliijgwrs254fs9s1m1x6q4f"))

(define rust-aliasable-0.1.3
  (crate-source "aliasable" "0.1.3"
                "1z8548zdjlm4ps1k0d7x68lfdyji02crwcc9rw3q3bb106f643r5"))

(define rust-aligned-vec-0.5.0
  (crate-source "aligned-vec" "0.5.0"
                "1lb8qjqfap028ylf8zap6rkwrnrqimc3v6h3cixycjrdx1y0vaaa"))

(define rust-allocator-api2-0.2.21
  (crate-source "allocator-api2" "0.2.21"
                "08zrzs022xwndihvzdn78yqarv2b9696y67i6h78nla3ww87jgb8"))

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

(define rust-ansi-term-0.12.1
  (crate-source "ansi_term" "0.12.1"
                "1ljmkbilxgmhavxvxqa7qvm6f3fjggi7q2l3a72q9x0cxjvrnanm"))

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

(define rust-approx-0.5.1
  (crate-source "approx" "0.5.1"
                "1ilpv3dgd58rasslss0labarq7jawxmivk17wsh8wmkdm3q15cfa"))

(define rust-arbitrary-1.4.1
  (crate-source "arbitrary" "1.4.1"
                "08zj2yanll5s5gsbmvgwvbq39iqzy3nia3yx3db3zwba08yhpqnx"))

(define rust-arc-swap-1.7.1
  (crate-source "arc-swap" "1.7.1"
                "0mrl9a9r9p9bln74q6aszvf22q1ijiw089jkrmabfqkbj31zixv9"))

(define rust-archery-1.2.1
  (crate-source "archery" "1.2.1"
                "0sdqlmybcvd0rzv22ac3k3xxm5anr1gpm03sf02iy0jmrlhyvqpa"))

(define rust-arg-enum-proc-macro-0.3.4
  (crate-source "arg_enum_proc_macro" "0.3.4"
                "1sjdfd5a8j6r99cf0bpqrd6b160x9vz97y5rysycsjda358jms8a"))

(define rust-arrayref-0.3.9
  (crate-source "arrayref" "0.3.9"
                "1jzyp0nvp10dmahaq9a2rnxqdd5wxgbvp8xaibps3zai8c9fi8kn"))

(define rust-arrayvec-0.7.6
  (crate-source "arrayvec" "0.7.6"
                "0l1fz4ccgv6pm609rif37sl5nv5k6lbzi7kkppgzqzh1vwix20kw"))

(define rust-asn1-0.20.0
  (crate-source "asn1" "0.20.0"
                "0ckg83ingvagwjvmxadjjmkgna5kvlvrfx9arlfvzqhxxas892rd"))

(define rust-asn1-derive-0.20.0
  (crate-source "asn1_derive" "0.20.0"
                "1b88xsqmxpxjq4p2mrn1icj7c3k2s041v7wqp8yhnqiq06fq0052"))

(define rust-assert-cmd-2.0.16
  (crate-source "assert_cmd" "2.0.16"
                "0gdj0710k3lnvyjmpv8a4dgwrk9ib85l2wfw4n2xwy3qyavka66w"))

(define rust-async-compression-0.4.22
  (crate-source "async-compression" "0.4.22"
                "0r6shv717rl3qzccjc9qlxmnaj3l22rr9197jsahkn33v7wr98ar"
                #:snippet '(delete-file-recursively "tests")))

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

(define rust-automod-1.0.15
  (crate-source "automod" "1.0.15"
                "0w76lzg8vbrvx4cl8x63kdaxkwpbp17c4cg1bcgwmqmj3lqbvd7b"))

(define rust-av-metrics-0.9.1
  (crate-source "av-metrics" "0.9.1"
                "0c1m5rrrx88y1hm4i17qh0fd2rqd3jwck86lj5dkw85hpmdyjv4r"))

(define rust-av1-grain-0.2.3
  (crate-source "av1-grain" "0.2.3"
                "1gvqdh21bm1cfqiwyiinbqi0mg7x2lg2fwgmphma8ijxijfr0y36"))

(define rust-backtrace-0.3.71
  (crate-source "backtrace" "0.3.71"
                "17bgd7pbjb9gc8q47qwsg2lmy9i62x3bsjmmnjrwh5z8s805ic16"))

(define rust-backtrace-0.3.74
  (crate-source "backtrace" "0.3.74"
                "06pfif7nwx66qf2zaanc2fcq7m64i91ki9imw9xd3bnz5hrwp0ld"))

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

(define rust-bincode-1.3.3
  (crate-source "bincode" "1.3.3"
                "1bfw3mnwzx5g1465kiqllp5n4r10qrqy88kdlp3jfwnq2ya5xx5i"))

(define rust-bindgen-0.69.5
  (crate-source "bindgen" "0.69.5"
                "1240snlcfj663k04bjsg629g4wx6f83flgbjh5rzpgyagk3864r7"))

(define rust-bindgen-0.71.1
  (crate-source "bindgen" "0.71.1"
                "1cynz43s9xwjbd1y03rx9h37xs0isyl8bi6g6yngp35nglyvyn2z"))

(define rust-binfarce-0.2.1
  (crate-source "binfarce" "0.2.1"
                "18hnqqir3gk5sx1mlndzgpxs0l4rviv7dk3h1piyspayp35lqihq"))

(define rust-bit-field-0.10.2
  (crate-source "bit_field" "0.10.2"
                "0qav5rpm4hqc33vmf4vc4r0mh51yjx5vmd9zhih26n9yjs3730nw"))

(define rust-bit-set-0.8.0
  (crate-source "bit-set" "0.8.0"
                "18riaa10s6n59n39vix0cr7l2dgwdhcpbcm97x1xbyfp1q47x008"))

(define rust-bit-vec-0.8.0
  (crate-source "bit-vec" "0.8.0"
                "1xxa1s2cj291r7k1whbxq840jxvmdsq9xgh7bvrxl46m80fllxjy"))

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

(define rust-blake3-1.8.1
  (crate-source "blake3" "1.8.1"
                "1czffygg8dhdsjyzydsrf50harfrralrkm10ckhkja1i6jdhk6iq"))

(define rust-block-0.1.6
  (crate-source "block" "0.1.6"
                "16k9jgll25pzsq14f244q22cdv0zb4bqacldg3kx6h89d7piz30d"))

(define rust-block-buffer-0.10.4
  (crate-source "block-buffer" "0.10.4"
                "0w9sa2ypmrsqqvc20nhwr75wbb5cjr4kkyhpjm1z1lv2kdicfy1h"))

(define rust-borsh-1.5.7
  (crate-source "borsh" "1.5.7"
                "1kikljm5yr3l9qsw5xvdccragxj4445s4s3fqsgy6hmmipwld1md"))

(define rust-bstr-1.11.3
  (crate-source "bstr" "1.11.3"
                "1q3g2wmrvclgx7lk2p6mpzhqxzx41hyg962gkmlyxql1liar26jk"))

(define rust-built-0.7.7
  (crate-source "built" "0.7.7"
                "0ywn0m11xm80pg6zrzq3sdj3vmzg3qs6baqnvfmkd377ly8n3van"))

(define rust-bumpalo-3.17.0
  (crate-source "bumpalo" "3.17.0"
                "1gxxsn2fsjmv03g8p3m749mczv2k4m8xspifs5l7bcx0vx3gna0n"))

(define rust-bytemuck-1.22.0
  (crate-source "bytemuck" "1.22.0"
                "0h6m8wh7iw98cn69k53plbyqff78c2yrs32l0fy4wqdcvc8grcdn"))

(define rust-byteorder-1.5.0
  (crate-source "byteorder" "1.5.0"
                "0jzncxyf404mwqdbspihyzpkndfgda450l0893pz5xj685cg5l0z"))

(define rust-bytes-1.10.1
  (crate-source "bytes" "1.10.1"
                "0smd4wi2yrhp5pmq571yiaqx84bjqlm1ixqhnvfwzzc6pqkn26yp"))

(define rust-bytesize-1.3.3
  (crate-source "bytesize" "1.3.3"
                "0nb645ma48nwsv1piylzcza0avjp435sl8krhyws3q18kv5ap4rf"))

(define rust-bzip2-0.5.2
  (crate-source "bzip2" "0.5.2"
                "0iya6nbj0p2y8jss0z05yncc5hadry164fw3zva01y06v4igpv29"
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

(define rust-cairo-rs-0.19.4
  (crate-source "cairo-rs" "0.19.4"
                "0qp5rixgipdj9d8yd5458hzfxam1rgpzcxi90vq6q0v91r6jmb5j"))

(define rust-cairo-sys-rs-0.19.2
  (crate-source "cairo-sys-rs" "0.19.2"
                "0r0yp0lph77lm4blrn6fvdmz2i3r8ibkkjg6nmwbvvv4jq8v6fzx"))

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

(define rust-cast-0.3.0
  (crate-source "cast" "0.3.0"
                "1dbyngbyz2qkk0jn2sxil8vrz3rnpcj142y184p9l4nbl9radcip"))

(define rust-cbindgen-0.27.0
  (crate-source "cbindgen" "0.27.0"
                "1sqm3axr678d72yihgmpr9d17mj99ccibxfqhw53mgzwzkbqvkiz"))

(define rust-cbindgen-0.28.0
  (crate-source "cbindgen" "0.28.0"
                "1zyiaifg6mcd4wwhhbxk8adzhph6qz4wxzgagvg3ijp95j58dpga"))

(define rust-cc-1.2.18
  (crate-source "cc" "1.2.18"
                "0p6d2pfyrjgqpf2w399wzj4hmyffj6g0gyzg3pdy6xl3gmhlcl2j"))

(define rust-cexpr-0.6.0
  (crate-source "cexpr" "0.6.0"
                "0rl77bwhs5p979ih4r0202cn5jrfsrbgrksp40lkfz5vk1x3ib3g"))

(define rust-cfg-aliases-0.2.1
  (crate-source "cfg_aliases" "0.2.1"
                "092pxdc1dbgjb6qvh83gk56rkic2n2ybm4yvy76cgynmzi3zwfk1"))

(define rust-cfg-expr-0.15.8
  (crate-source "cfg-expr" "0.15.8"
                "00lgf717pmf5qd2qsxxzs815v6baqg38d6m5i6wlh235p14asryh"))

(define rust-cfg-if-1.0.0
  (crate-source "cfg-if" "1.0.0"
                "1za0vb97n4brpzpv8lsbnzmq5r8f2b0cpqqr0sy8h5bn751xxwds"))

(define rust-charset-0.1.5
  (crate-source "charset" "0.1.5"
                "0zkwcw525qwcqsdf74l9d2r6m69yxfxb4kgywp3q9fklgjq2gygi"))

(define rust-chrono-0.4.40
  (crate-source "chrono" "0.4.40"
                "0z334kqnvq5zx6xsq1k6zk8g9z14fgk2w3vkn4n13pvi3mhn8y8s"))

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

(define rust-clang-sys-1.8.1
  (crate-source "clang-sys" "1.8.1"
                "1x1r9yqss76z8xwpdanw313ss6fniwc1r7dzb5ycjn0ph53kj0hb"))

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

(define rust-clap-derive-4.5.32
  (crate-source "clap_derive" "4.5.32"
                "1mqcag8qapb5yhygg2hi153kzmbf7w5hqp3nl3fvl5cn4yp6l5q9"))

(define rust-clap-lex-0.2.4
  (crate-source "clap_lex" "0.2.4"
                "1ib1a9v55ybnaws11l63az0jgz5xiy24jkdgsmyl7grcm3sz4l18"))

(define rust-clap-lex-0.7.4
  (crate-source "clap_lex" "0.7.4"
                "19nwfls5db269js5n822vkc8dw0wjq2h1wf0hgr06ld2g52d2spl"))

(define rust-clru-0.6.2
  (crate-source "clru" "0.6.2"
                "0ngyycxpxif84wpjjn0ixywylk95h5iv8fqycg2zsr3f0rpggl6b"))

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

(define rust-configparser-3.1.0
  (crate-source "configparser" "3.1.0"
                "16v47b7lknb35ragwhj9gzgwfpxs34vn2b97hhaky30ry1r34zp5"))

(define rust-console-0.15.11
  (crate-source "console" "0.15.11"
                "1n5gmsjk6isbnw6qss043377kln20lfwlmdk3vswpwpr21dwnk05"))

(define rust-const-oid-0.9.6
  (crate-source "const-oid" "0.9.6"
                "1y0jnqaq7p2wvspnx7qj76m7hjcqpz73qzvr9l2p9n2s51vr6if2"))

(define rust-constant-time-eq-0.3.1
  (crate-source "constant_time_eq" "0.3.1"
                "19nwwczii762pwlsm7bpizgjg8hkg1kqi32b2g4rglijklsbhx3w"))

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

(define rust-crossbeam-0.8.4
  (crate-source "crossbeam" "0.8.4"
                "1a5c7yacnk723x0hfycdbl91ks2nxhwbwy46b8y5vyy0gxzcsdqi"))

(define rust-crossbeam-channel-0.5.14
  (crate-source "crossbeam-channel" "0.5.14"
                "0wa41qybq5w8s70anb472myh4fid4aw6v65vws6wn528w9l6vfh6"))

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

(define rust-crunchy-0.2.3
  (crate-source "crunchy" "0.2.3"
                "0aa9k4izp962qlsn5ndgw2zq62mizcpnkns8bxscgz3gqr35knj3"))

(define rust-crypto-bigint-0.5.5
  (crate-source "crypto-bigint" "0.5.5"
                "0xmbdff3g6ii5sbxjxc31xfkv9lrmyril4arh3dzckd4gjsjzj8d"))

(define rust-crypto-common-0.1.6
  (crate-source "crypto-common" "0.1.6"
                "1cvby95a6xg7kxdz5ln3rl9xh66nz66w46mm3g56ri1z5x815yqv"))

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

(define rust-curl-0.4.47
  (crate-source "curl" "0.4.47"
                "0rcjdrl35xs8l5v3wv6q5z37hjw3r5bvmbb09pqmhaxyl49lvyyr"))

(define rust-curl-sys-0.4.80+curl-8.12.1
  (crate-source "curl-sys" "0.4.80+curl-8.12.1"
                "0d7ppx4kq77hc5nyff6jydmfabpgd0i3ppjvn8x0q833mhpdzxsm"
                #:snippet '(delete-file-recursively "curl")))

(define rust-cvss-2.0.0
  (crate-source "cvss" "2.0.0"
                "03q1nh4jy0cvgckji1jr1kz3j7gf2zg74240j8qi1qxhk7vs5iky"))

(define rust-dashmap-5.5.3
  (crate-source "dashmap" "5.5.3"
                "0miqnlxi501vfbv6mw5jbmzgnj0wjrch3p4abvpd59s9v30lg1wp"))

(define rust-data-encoding-2.8.0
  (crate-source "data-encoding" "2.8.0"
                "0470yf5ly1ibzmwygyjqg9ii9njbsha3xr5qj5dxyf2psbgpapsp"))

(define rust-data-url-0.3.1
  (crate-source "data-url" "0.3.1"
                "0ahclz72myi350cs1xcsxdh1v0iljpfj4ghcy2fy46mpfhf7laaw"))

(define rust-dbus-0.9.7
  (crate-source "dbus" "0.9.7"
                "06vdv4aarjs4w6byg9nqajr67c8qvlhk3153ic2i65pvp63ikchv"))

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

(define rust-dialoguer-0.11.0
  (crate-source "dialoguer" "0.11.0"
                "1pl0744wwr97kp8qnaybzgrfwk66qakzq0i1qrxl03vpbn0cx2v5"))

(define rust-diff-0.1.13
  (crate-source "diff" "0.1.13"
                "1j0nzjxci2zqx63hdcihkp0a4dkdmzxd7my4m7zk6cjyfy34j9an"))

(define rust-difflib-0.4.0
  (crate-source "difflib" "0.4.0"
                "1s7byq4d7jgf2hcp2lcqxi2piqwl8xqlharfbi8kf90n8csy7131"))

(define rust-digest-0.10.7
  (crate-source "digest" "0.10.7"
                "14p2n6ih29x81akj097lvz7wi9b6b9hvls0lwrv7b6xwyy0s5ncy"))

(define rust-dirs-5.0.1
  (crate-source "dirs" "5.0.1"
                "0992xk5vx75b2x91nw9ssb51mpl8x73j9rxmpi96cryn0ffmmi24"))

(define rust-dirs-sys-0.4.1
  (crate-source "dirs-sys" "0.4.1"
                "071jy0pvaad9lsa6mzawxrh7cmr7hsmsdxwzm7jzldfkrfjha3sj"))

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

(define rust-doc-comment-0.3.3
  (crate-source "doc-comment" "0.3.3"
                "043sprsf3wl926zmck1bm7gw0jq50mb76lkpk49vasfr6ax1p97y"))

(define rust-dtoa-1.0.10
  (crate-source "dtoa" "1.0.10"
                "016gid01rarcdv57h049d7nr9daxc2hc2gqzx0mji57krywd7bfn"))

(define rust-dtoa-short-0.3.5
  (crate-source "dtoa-short" "0.3.5"
                "11rwnkgql5jilsmwxpx6hjzkgyrbdmx1d71s0jyrjqm5nski25fd"))

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

(define rust-either-1.15.0
  (crate-source "either" "1.15.0"
                "069p1fknsmzn9llaizh77kip0pqmcwpdsykv2x30xpjyija5gis8"))

(define rust-elliptic-curve-0.13.8
  (crate-source "elliptic-curve" "0.13.8"
                "0ixx4brgnzi61z29r3g1606nh2za88hzyz8c5r3p6ydzhqq09rmm"
                #:snippet '(delete-file-recursively "tests")))

(define rust-encode-unicode-1.0.0
  (crate-source "encode_unicode" "1.0.0"
                "1h5j7j7byi289by63s3w4a8b3g6l5ccdrws7a67nn07vdxj77ail"))

(define rust-encoding-rs-0.8.35
  (crate-source "encoding_rs" "0.8.35"
                "1wv64xdrr9v37rqqdjsyb8l8wzlcbab80ryxhrszvnj59wy0y0vm"))

(define rust-env-home-0.1.0
  (crate-source "env_home" "0.1.0"
                "1zn08mk95rjh97831rky1n944k024qrwjhbcgb0xv9zhrh94xy67"))

(define rust-env-logger-0.10.2
  (crate-source "env_logger" "0.10.2"
                "1005v71kay9kbz1d5907l0y7vh9qn2fqsp2yfgb8bjvin6m0bm2c"))

(define rust-env-logger-0.8.4
  (crate-source "env_logger" "0.8.4"
                "1qzw8g11dbdfi7ixm44ldykwcqsxqkh8vx5cgpd88zmclgz8g4d1"))

(define rust-equivalent-1.0.2
  (crate-source "equivalent" "1.0.2"
                "03swzqznragy8n0x31lqc78g2af054jwivp7lkrbrc0khz74lyl7"))

(define rust-erased-serde-0.4.6
  (crate-source "erased-serde" "0.4.6"
                "1dx5hj16hsl143czwl2g7ymdi1y84lsjyyii2zprzjqzyn3xh170"))

(define rust-errno-0.3.11
  (crate-source "errno" "0.3.11"
                "0kjrrcaa5nvickysw7z3vm5p0l7l457idf1ff3z6ang8qwnx8vcp"))

(define rust-expect-test-1.5.1
  (crate-source "expect-test" "1.5.1"
                "1c5c081ykm4k5rlsam9jw56w4wgs2h7r4aj78zxlis1i8kzl7bv3"))

(define rust-exr-1.73.0
  (crate-source "exr" "1.73.0"
                "1q47yq78q9k210r6jy1wwrilxwwxqavik9l3l426rd17k7srfcgq"))

(define rust-eyre-0.6.12
  (crate-source "eyre" "0.6.12"
                "1v1a3vb9gs5zkwp4jzkcfnpg0gvyp4ifydzx37f4qy14kzcibnbw"))

(define rust-fallible-iterator-0.3.0
  (crate-source "fallible-iterator" "0.3.0"
                "0ja6l56yka5vn4y4pk6hn88z0bpny7a8k1919aqjzp0j1yhy9k1a"))

(define rust-fallible-streaming-iterator-0.1.9
  (crate-source "fallible-streaming-iterator" "0.1.9"
                "0nj6j26p71bjy8h42x6jahx1hn0ng6mc2miwpgwnp8vnwqf4jq3k"))

(define rust-faster-hex-0.9.0
  (crate-source "faster-hex" "0.9.0"
                "10wi4vqbdpkamw4qvra1ijp4as2j7j1zc66g4rdr6h0xv8gb38m2"))

(define rust-fastrand-2.3.0
  (crate-source "fastrand" "2.3.0"
                "1ghiahsw1jd68df895cy5h3gzwk30hndidn3b682zmshpgmrx41p"))

(define rust-fat-macho-0.4.9
  (crate-source "fat-macho" "0.4.9"
                "0idkn366wipv2l757yqfgzgibqc6jvm89gdk9kpgmvf6lv54b72c"
                #:snippet '(delete-file-recursively "tests")))

(define rust-fdeflate-0.3.7
  (crate-source "fdeflate" "0.3.7"
                "130ga18vyxbb5idbgi07njymdaavvk6j08yh1dfarm294ssm6s0y"
                #:snippet '(delete-file-recursively "tests")))

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

(define rust-filetime-0.2.25
  (crate-source "filetime" "0.2.25"
                "11l5zr86n5sr6g6k6sqldswk0jzklm0q95rzikxcns0yk0p55h1m"))

(define rust-fixedbitset-0.4.2
  (crate-source "fixedbitset" "0.4.2"
                "101v41amgv5n9h4hcghvrbfk5vrncx1jwm35rn5szv4rk55i7rqc"))

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

(define rust-fnv-1.0.7
  (crate-source "fnv" "1.0.7"
                "1hc2mcqha06aibcaza94vbi81j6pr9a1bbxrxjfhc91zin8yr7iz"))

(define rust-foldhash-0.1.5
  (crate-source "foldhash" "0.1.5"
                "1wisr1xlc2bj7hk4rgkcjkz3j2x4dhd1h9lwk7mj8p71qpdgbi6r"))

(define rust-foreign-types-0.3.2
  (crate-source "foreign-types" "0.3.2"
                "1cgk0vyd7r45cj769jym4a6s7vwshvd0z4bqrb92q1fwibmkkwzn"))

(define rust-foreign-types-shared-0.1.1
  (crate-source "foreign-types-shared" "0.1.1"
                "0jxgzd04ra4imjv8jgkmdq59kj8fsz6w4zxsbmlai34h26225c00"))

(define rust-form-urlencoded-1.2.1
  (crate-source "form_urlencoded" "1.2.1"
                "0milh8x7nl4f450s3ddhg57a3flcv6yq8hlkyk6fyr3mcb128dp1"))

(define rust-fs-err-2.11.0
  (crate-source "fs-err" "2.11.0"
                "0hdajzh5sjvvdjg0n15j91mv8ydvb7ff6m909frvdmg1bw81z948"))

(define rust-fs-err-3.1.0
  (crate-source "fs-err" "3.1.0"
                "1al2sj8src02wwww70vv2gypsrs6wyzx6zlpk82h84m2qajbv28z"))

(define rust-fs4-0.12.0
  (crate-source "fs4" "0.12.0"
                "08gm0p6x133cav6yrcc3qhcr2qr1917yhj0bdx3psm0q8il31762"))

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

(define rust-fxhash-0.2.1
  (crate-source "fxhash" "0.2.1"
                "037mb9ichariqi45xm6mz0b11pa92gj38ba0409z3iz239sns6y3"))

(define rust-gdk-pixbuf-0.19.8
  (crate-source "gdk-pixbuf" "0.19.8"
                "16c6kznkh3vi82843ays8awdm37fwjd1fblv6g3h64824shsnkk2"))

(define rust-gdk-pixbuf-sys-0.19.8
  (crate-source "gdk-pixbuf-sys" "0.19.8"
                "0y93g24mdgskvyhva46xv3qyb1cvj5xpi0yqnh7cb31wz2j0byjf"))

(define rust-generic-array-0.14.7
  (crate-source "generic-array" "0.14.7"
                "16lyyrzrljfq424c3n8kfwkqihlimmsg5nhshbbp48np3yjrqr45"))

(define rust-getopts-0.2.21
  (crate-source "getopts" "0.2.21"
                "1mgb3qvivi26gs6ihqqhh8iyhp3vgxri6vwyrwg28w0xqzavznql"))

(define rust-getrandom-0.2.15
  (crate-source "getrandom" "0.2.15"
                "1mzlnrb3dgyd1fb84gvw10pyr8wdqdl4ry4sr64i1s8an66pqmn4"))

(define rust-getrandom-0.3.2
  (crate-source "getrandom" "0.3.2"
                "1w2mlixa1989v7czr68iji7h67yra2pbg3s480wsqjza1r2sizkk"))

(define rust-gif-0.13.1
  (crate-source "gif" "0.13.1"
                "1whrkvdg26gp1r7f95c6800y6ijqw5y0z8rgj6xihpi136dxdciz"))

(define rust-gimli-0.28.1
  (crate-source "gimli" "0.28.1"
                "0lv23wc8rxvmjia3mcxc6hj9vkqnv1bqq0h8nzjcgf71mrxx6wa2"))

(define rust-gimli-0.31.1
  (crate-source "gimli" "0.31.1"
                "0gvqc0ramx8szv76jhfd4dms0zyamvlg4whhiz11j34hh3dqxqh7"))

(define rust-gio-0.19.8
  (crate-source "gio" "0.19.8"
                "1znz5ngfvv3gbndf6lzz3hs27hlb8ysls4axlfccrzvkscbz2jac"))

(define rust-gio-sys-0.19.8
  (crate-source "gio-sys" "0.19.8"
                "1vylsskpipfwl7mvffp1s0227d0k5amyhd32dfnp3mhl8yx47mrc"))

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

(define rust-glib-0.19.9
  (crate-source "glib" "0.19.9"
                "0i2ak1scmzfmfxbm2dr146jl4y9mafxf1ald05jr8iimy5wh4r9r"))

(define rust-glib-macros-0.19.9
  (crate-source "glib-macros" "0.19.9"
                "1mzsh8jkg8vldvgvr9gsaidvn2myn5cbdn8a6m8rgbhlg8kv0aa4"))

(define rust-glib-sys-0.19.8
  (crate-source "glib-sys" "0.19.8"
                "19f4q8x77vd7c1d9ikw492yskq5kpd7k04qb8xnh1c427a6w2baw"))

(define rust-glob-0.3.2
  (crate-source "glob" "0.3.2"
                "1cm2w34b5w45fxr522h5b0fv1bxchfswcj560m3pnjbia7asvld8"))

(define rust-globset-0.4.16
  (crate-source "globset" "0.4.16"
                "1xa9ivqs74imf1q288spxh49g6iw2mn3x9snibdgapazzj6h58al"))

(define rust-gobject-sys-0.19.8
  (crate-source "gobject-sys" "0.19.8"
                "17lb7dfbpcg8zchwlfbc08kckwf0a7d9n5ly3pyic13f5ljpws9f"))

(define rust-goblin-0.9.3
  (crate-source "goblin" "0.9.3"
                "0ifpcsp0hpp7lx10yqln9ybmfkky7gig9idlhc2j7sx7456sd86s"))

(define rust-group-0.13.0
  (crate-source "group" "0.13.0"
                "0qqs2p5vqnv3zvq9mfjkmw3qlvgqb0c3cm6p33srkh7pc9sfzygh"))

(define rust-h2-0.4.8
  (crate-source "h2" "0.4.8"
                "1hp3lijg1br982kzgglb5ks2ibg68a76z3rl052r8c5vyi7jj5sh"))

(define rust-half-2.5.0
  (crate-source "half" "2.5.0"
                "1ldv2i761fjqxl4rn033nasjrdnw5ysnc1xalsfkfl5skc9zzckx"))

(define rust-hashbrown-0.12.3
  (crate-source "hashbrown" "0.12.3"
                "1268ka4750pyg2pbgsr43f0289l5zah4arir2k4igx5a8c6fg7la"))

(define rust-hashbrown-0.14.5
  (crate-source "hashbrown" "0.14.5"
                "1wa1vy1xs3mp11bn3z9dv0jricgr6a2j0zkf1g19yz3vw4il89z5"))

(define rust-hashbrown-0.15.2
  (crate-source "hashbrown" "0.15.2"
                "12dj0yfn59p3kh3679ac0w1fagvzf4z2zp87a13gbbqbzw0185dz"))

(define rust-hashlink-0.9.1
  (crate-source "hashlink" "0.9.1"
                "1byq4nyrflm5s6wdx5qwp96l1qbp2d0nljvrr5yqrsfy51qzz93b"))

(define rust-heck-0.4.1
  (crate-source "heck" "0.4.1"
                "1a7mqsnycv5z4z5vnv1k34548jzmc0ajic7c1j8jsaspnhw5ql4m"))

(define rust-heck-0.5.0
  (crate-source "heck" "0.5.0"
                "1sjmpsdl8czyh9ywl3qcsfsq9a307dg4ni2vnlwgnzzqhc4y0113"))

(define rust-hermit-abi-0.1.19
  (crate-source "hermit-abi" "0.1.19"
                "0cxcm8093nf5fyn114w8vxbrbcyvv91d4015rdnlgfll7cs6gd32"))

(define rust-hermit-abi-0.5.0
  (crate-source "hermit-abi" "0.5.0"
                "0zp3khi7bl6x8gazm9i9dgjf4h47mj10v4j18i3823y3bkz81mzv"))

(define rust-hex-0.4.3
  (crate-source "hex" "0.4.3"
                "0w1a4davm1lgzpamwnba907aysmlrnygbqmfis2mqjx5m552a93z"))

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

(define rust-http-1.3.1
  (crate-source "http" "1.3.1"
                "0r95i5h7dr1xadp1ac9453w0s62s27hzkam356nyx2d9mqqmva7l"))

(define rust-http-auth-0.1.10
  (crate-source "http-auth" "0.1.10"
                "08l8z75cpda5y25cnd5fzgsahb35xn29qlgl9j12dy9f8sls83qm"))

(define rust-http-body-1.0.1
  (crate-source "http-body" "1.0.1"
                "111ir5k2b9ihz5nr9cz7cwm7fnydca7dx4hc7vr16scfzghxrzhy"))

(define rust-http-body-util-0.1.3
  (crate-source "http-body-util" "0.1.3"
                "0jm6jv4gxsnlsi1kzdyffjrj8cfr3zninnxpw73mvkxy4qzdj8dh"))

(define rust-httparse-1.10.1
  (crate-source "httparse" "1.10.1"
                "11ycd554bw2dkgw0q61xsa7a4jn1wb1xbfacmf3dbwsikvkkvgvd"))

(define rust-humantime-2.2.0
  (crate-source "humantime" "2.2.0"
                "17rz8jhh1mcv4b03wnknhv1shwq2v9vhkhlfg884pprsig62l4cv"))

(define rust-humantime-serde-1.1.1
  (crate-source "humantime-serde" "1.1.1"
                "0310ri4zb33qbwqv0n51xysfjpnwc6rgxscl5i09jgcjlmgdp8sp"))

(define rust-hyper-1.6.0
  (crate-source "hyper" "1.6.0"
                "103ggny2k31z0iq2gzwk2vbx601wx6xkpjpxn40hr3p3b0b5fayc"))

(define rust-hyper-rustls-0.27.5
  (crate-source "hyper-rustls" "0.27.5"
                "1cjr3yf3x5mr3194llsfibacl6j7n2dknii2dwjha4ysyf1ia69d"))

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

(define rust-indoc-1.0.9
  (crate-source "indoc" "1.0.9"
                "01l3b4ami6sck57yrn8n2z44jifph2m3jiivkws7w2njbvfrk9xz"))

(define rust-indoc-2.0.6
  (crate-source "indoc" "2.0.6"
                "1gbn2pkx5sgbd9lp05d2bkqpbfgazi0z3nvharh5ajah11d29izl"))

(define rust-instant-0.1.13
  (crate-source "instant" "0.1.13"
                "08h27kzvb5jw74mh0ajv0nv9ggwvgqm8ynjsn2sa9jsks4cjh970"))

(define rust-interpolate-name-0.2.4
  (crate-source "interpolate_name" "0.2.4"
                "0q7s5mrfkx4p56dl8q9zq71y1ysdj4shh6f28qf9gly35l21jj63"))

(define rust-io-close-0.3.7
  (crate-source "io-close" "0.3.7"
                "1g4hldfn436rkrx3jlm4az1y5gdmkcixdlhkwy64yx06gx2czbcw"))

(define rust-ipnet-2.11.0
  (crate-source "ipnet" "2.11.0"
                "0c5i9sfi2asai28m8xp48k5gvwkqrg5ffpi767py6mzsrswv17s6"))

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

(define rust-itertools-0.10.5
  (crate-source "itertools" "0.10.5"
                "0ww45h7nxx5kj6z2y6chlskxd1igvs4j507anr6dzg99x1h25zdh"))

(define rust-itertools-0.12.1
  (crate-source "itertools" "0.12.1"
                "0s95jbb3ndj1lvfxyq5wanc0fm0r6hg6q4ngb92qlfdxvci10ads"))

(define rust-itertools-0.13.0
  (crate-source "itertools" "0.13.0"
                "11hiy3qzl643zcigknclh446qb9zlg4dpdzfkjaa9q9fqpgyfgj1"))

(define rust-itertools-0.14.0
  (crate-source "itertools" "0.14.0"
                "118j6l1vs2mx65dqhwyssbrxpawa90886m3mzafdvyip41w2q69b"))

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

(define rust-keyring-2.3.3
  (crate-source "keyring" "2.3.3"
                "184mshdrgghlvmlz0n7w1167yx0sa3zb82n31jk4lwcx07q8fcrn"))

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

(define rust-lebe-0.5.2
  (crate-source "lebe" "0.5.2"
                "1j2l6chx19qpa5gqcw434j83gyskq3g2cnffrbl3842ymlmpq203"))

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

(define rust-libloading-0.8.6
  (crate-source "libloading" "0.8.6"
                "0d2ccr88f8kv3x7va2ccjxalcjnhrci4j2kwxp7lfmbkpjs4wbzw"
                #:snippet '(delete-file-recursively "tests")))

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

(define rust-libredox-0.1.3
  (crate-source "libredox" "0.1.3"
                "139602gzgs0k91zb7dvgj1qh4ynb8g1lbxsswdim18hcb6ykgzy0"))

(define rust-libspa-0.8.0.fd3d8f7 rust-pipewire-for-niri)

(define rust-libspa-sys-0.8.0.fd3d8f7 rust-pipewire-for-niri)

(define rust-libsqlite3-sys-0.30.1
  (crate-source "libsqlite3-sys" "0.30.1"
                "0jcikvgbj84xc7ikdmpc8m4y5lyqgrb9aqblphwk67kv95xgp69f"
                #:snippet
                '(for-each delete-file
                           (append
                            (find-files "sqlcipher" "\\.(c|h)$")
                            (find-files "sqlite3" "\\.(c|h)$")))))

(define rust-libssh2-sys-0.3.1
  (crate-source "libssh2-sys" "0.3.1"
                "1f8i31h3666rl6sq7v64ajdq03hmylkh6c1vaf9828aaml2ly3i2"
                #:snippet '(delete-file-recursively "libssh2")))

(define rust-libz-sys-1.1.22
  (crate-source "libz-sys" "1.1.22"
                "07b5wxh0ska996kc0g2hanjhmb4di7ksm6ndljhr4pi0vykyfw4b"
                #:snippet '(for-each delete-file-recursively '("src/zlib" "src/zlib-ng"))))

(define rust-linked-hash-map-0.5.6
  (crate-source "linked-hash-map" "0.5.6"
                "03vpgw7x507g524nx5i1jf5dl8k3kv0fzg8v3ip6qqwbpkqww5q7"))

(define rust-linux-keyutils-0.2.4
  (crate-source "linux-keyutils" "0.2.4"
                "13nipvk2mzk76y7yfsqwnwsqk21x6xy8fkmqz5is99fqbzn4j7kn"))

(define rust-linux-raw-sys-0.4.15
  (crate-source "linux-raw-sys" "0.4.15"
                "1aq7r2g7786hyxhv40spzf2nhag5xbw2axxc1k8z5k1dsgdm4v6j"))

(define rust-linux-raw-sys-0.9.3
  (crate-source "linux-raw-sys" "0.9.3"
                "04zl7j4k1kgbn7rrl3i7yszaglgxp0c8dbwx8f1cabnjjwhb2zgy"))

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

(define rust-lopdf-0.32.0
  (crate-source "lopdf" "0.32.0"
                "0aw7diz39z3mk22k0mp7jk7qiaaagfvggzly1baqg2jf4vpf8xg7"
                #:snippet '(delete-file-recursively "assets")))

(define rust-mac-0.1.1
  (crate-source "mac" "0.1.1"
                "194vc7vrshqff72rl56f9xgb0cazyl4jda7qsv31m5l6xx7hq7n4"))

(define rust-mailparse-0.15.0
  (crate-source "mailparse" "0.15.0"
                "0zkwbrzgr7pp1wyywjgvlxayr1p3nnkn2yxgi97746j1h1ckv81x"))

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

(define rust-matrixmultiply-0.3.9
  (crate-source "matrixmultiply" "0.3.9"
                "06msav241ybxvsqfwm4hfmb1pbws71v0inhmyk0i0vg9wc8vk04k"))

(define rust-maybe-async-0.2.10
  (crate-source "maybe-async" "0.2.10"
                "04fvg2ywb2p9dzf7i35xqfibxc05k1pirv36jswxcqg3qw82ryaw"))

(define rust-maybe-rayon-0.1.1
  (crate-source "maybe-rayon" "0.1.1"
                "06cmvhj4n36459g327ng5dnj8d58qs472pv5ahlhm7ynxl6g78cf"))

(define rust-md5-0.7.0
  (crate-source "md5" "0.7.0"
                "0wcps37hrhz59fkhf8di1ppdnqld6l1w5sdy7jp7p51z0i4c8329"))

(define rust-memchr-2.7.4
  (crate-source "memchr" "2.7.4"
                "18z32bhxrax0fnjikv475z7ii718hq457qwmaryixfxsl2qrmjkq"
                #:snippet '(delete-file-recursively "src/tests")))

(define rust-memmap2-0.9.5
  (crate-source "memmap2" "0.9.5"
                "0krpvvkpg4i3l05cv3q2xk24a1vj5c86gbrli2wzhj1qkpnpwgzx"))

(define rust-memoffset-0.9.1
  (crate-source "memoffset" "0.9.1"
                "12i17wh9a9plx869g7j4whf62xw68k5zd4k0k5nh6ys5mszid028"))

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

(define rust-mio-1.0.3
  (crate-source "mio" "1.0.3"
                "1gah0h4ia3avxbwym0b6bi6lr6rpysmj9zvw6zis5yq0z0xq91i8"))

(define rust-miow-0.6.0
  (crate-source "miow" "0.6.0"
                "0i307jyhxnhgzj148cdb9zq59rhlhr1b65g142g9z9r01d1pd7rm"))

(define rust-multipart-0.18.0
  (crate-source "multipart" "0.18.0"
                "10libwfbazqcyxcpgpcdf1a66jnzghwlmxlxnffg4rrqhqrwdph0"))

(define rust-nalgebra-0.32.6
  (crate-source "nalgebra" "0.32.6"
                "1r033ciacblmkif5njlhprfp0k59spjv54cqsyggb1is0bg1fp3v"))

(define rust-nalgebra-macros-0.2.2
  (crate-source "nalgebra-macros" "0.2.2"
                "1z6v9phhr1hwzyyblf792128lxfv1hy1sxl4cvikihcgmxr56ji5"))

(define rust-nasm-rs-0.2.5
  (crate-source "nasm-rs" "0.2.5"
                "0lfs2xfbpl1j7zq6qfg2wmi4djbl36qsygjb2spisjsz0v89hkgy"))

(define rust-native-tls-0.2.14
  (crate-source "native-tls" "0.2.14"
                "03hga800x8bzkp8h7frnm7yp545dwwawgmaq673vx7byk1139pl7"))

(define rust-new-debug-unreachable-1.0.6
  (crate-source "new_debug_unreachable" "1.0.6"
                "11phpf1mjxq6khk91yzcbd3ympm78m3ivl7xg6lg2c0lf66fy3k5"))

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

(define rust-nu-ansi-term-0.46.0
  (crate-source "nu-ansi-term" "0.46.0"
                "115sywxh53p190lyw97alm14nc004qj5jm5lvdj608z84rbida3p"))

(define rust-num-bigint-0.4.6
  (crate-source "num-bigint" "0.4.6"
                "1f903zd33i6hkjpsgwhqwi2wffnvkxbn6rv4mkgcjcqi7xr4zr55"))

(define rust-num-complex-0.4.6
  (crate-source "num-complex" "0.4.6"
                "15cla16mnw12xzf5g041nxbjjm9m85hdgadd5dl5d0b30w9qmy3k"))

(define rust-num-conv-0.1.0
  (crate-source "num-conv" "0.1.0"
                "1ndiyg82q73783jq18isi71a7mjh56wxrk52rlvyx0mi5z9ibmai"))

(define rust-num-derive-0.4.2
  (crate-source "num-derive" "0.4.2"
                "00p2am9ma8jgd2v6xpsz621wc7wbn1yqi71g15gc3h67m7qmafgd"))

(define rust-num-integer-0.1.46
  (crate-source "num-integer" "0.1.46"
                "13w5g54a9184cqlbsq80rnxw4jj4s0d8wv75jsq5r2lms8gncsbr"))

(define rust-num-rational-0.4.2
  (crate-source "num-rational" "0.4.2"
                "093qndy02817vpgcqjnj139im3jl7vkq4h68kykdqqh577d18ggq"))

(define rust-num-traits-0.2.19
  (crate-source "num-traits" "0.2.19"
                "0h984rhdkkqd4ny9cif7y2azl3xdfb7768hb9irhpsch4q3gq787"))

(define rust-objc-0.2.7
  (crate-source "objc" "0.2.7"
                "1cbpf6kz8a244nn1qzl3xyhmp05gsg4n313c9m3567625d3innwi"))

(define rust-objc-foundation-0.1.1
  (crate-source "objc-foundation" "0.1.1"
                "1y9bwb3m5fdq7w7i4bnds067dhm4qxv4m1mbg9y61j9nkrjipp8s"))

(define rust-objc-id-0.1.1
  (crate-source "objc_id" "0.1.1"
                "0fq71hnp2sdblaighjc82yrac3adfmqzhpr11irhvdfp9gdlsbf9"))

(define rust-object-0.32.2
  (crate-source "object" "0.32.2"
                "0hc4cjwyngiy6k51hlzrlsxgv5z25vv7c2cp0ky1lckfic0259m6"))

(define rust-object-0.36.7
  (crate-source "object" "0.36.7"
                "11vv97djn9nc5n6w1gc6bd96d2qk2c8cg1kw5km9bsi3v4a8x532"))

(define rust-once-cell-1.21.3
  (crate-source "once_cell" "1.21.3"
                "0b9x77lb9f1j6nqgf5aka4s2qj0nly176bpbrv6f9iakk5ff3xa2"))

(define rust-oorandom-11.1.5
  (crate-source "oorandom" "11.1.5"
                "07mlf13z453fq01qff38big1lh83j8l6aaglf63ksqzzqxc0yyfn"))

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

(define rust-ordered-float-2.10.1
  (crate-source "ordered-float" "2.10.1"
                "075i108hr95pr7hy4fgxivib5pky3b6b22rywya5qyd2wmkrvwb8"))

(define rust-orion-0.17.9
  (crate-source "orion" "0.17.9"
                "02sk0qv7r8nffqv56im11x4balxjb7zrxw867zab6pvwk9s0nbmz"))

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

(define rust-owo-colors-3.5.0
  (crate-source "owo-colors" "3.5.0"
                "0vyvry6ba1xmpd45hpi6savd8mbx09jpmvnnwkf6z62pk6s4zc61"))

(define rust-p384-0.13.1
  (crate-source "p384" "0.13.1"
                "1dnnp133mbpp72mfss3fhm8wx3yp3p3abdhlix27v92j19kz2hpy"
                #:snippet '(delete-file-recursively "src/test_vectors")))

(define rust-pango-0.19.8
  (crate-source "pango" "0.19.8"
                "1kffxkk7730csly86fkgja50k1184zj9lz49sv7qb0059233439z"))

(define rust-pango-sys-0.19.8
  (crate-source "pango-sys" "0.19.8"
                "182bcd6255v5yvnskbhxnb6kwak240z7sn54si2b5h46l17xl0zz"))

(define rust-pangocairo-0.19.8
  (crate-source "pangocairo" "0.19.8"
                "1n8wrqy260zpfiifb2n10mbsv3kbrvxm1z7pv8b4w77c08yb9j74"))

(define rust-pangocairo-sys-0.19.8
  (crate-source "pangocairo-sys" "0.19.8"
                "1myq3p8qrd63nlacd4sba66c17lfqgvzv8mpyn2rg1rqhi4h86ar"))

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

(define rust-petgraph-0.6.5
  (crate-source "petgraph" "0.6.5"
                "1ns7mbxidnn2pqahbbjccxkrqkrll2i5rbxx43ns6rh6fn3cridl"
                #:snippet '(for-each delete-file-recursively '("assets"))))

(define rust-phf-0.10.1
  (crate-source "phf" "0.10.1"
                "0naj8n5nasv5hj5ldlva3cl6y3sv7zp3kfgqylhbrg55v3mg3fzs"))

(define rust-phf-0.11.3
  (crate-source "phf" "0.11.3"
                "0y6hxp1d48rx2434wgi5g8j1pr8s5jja29ha2b65435fh057imhz"))

(define rust-phf-codegen-0.10.0
  (crate-source "phf_codegen" "0.10.0"
                "1k8kdad9wk2d5972k6jmjki2xpdy2ky4zd19rv7ybm2dpjlc7cag"))

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

(define rust-pin-project-lite-0.2.16
  (crate-source "pin-project-lite" "0.2.16"
                "16wzc7z7dfkf9bmjin22f5282783f6mdksnr0nv0j5ym5f9gyg1v"))

(define rust-pin-utils-0.1.0
  (crate-source "pin-utils" "0.1.0"
                "117ir7vslsl2z1a7qzhws4pd01cg2d3338c47swjyvqv2n60v1wb"))

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

(define rust-prettyplease-0.2.32
  (crate-source "prettyplease" "0.2.32"
                "1xmdmwhsvqc8l5ns029vzjida4k3lp5ynin0xra43qsiki0wakk6"))

(define rust-primeorder-0.13.6
  (crate-source "primeorder" "0.13.6"
                "1rp16710mxksagcjnxqjjq9r9wf5vf72fs8wxffnvhb6i6hiqgim"))

(define rust-proc-macro-crate-3.3.0
  (crate-source "proc-macro-crate" "3.3.0"
                "0d9xlymplfi9yv3f5g4bp0d6qh70apnihvqcjllampx4f5lmikpd"))

(define rust-proc-macro2-1.0.94
  (crate-source "proc-macro2" "1.0.94"
                "114wxb56gdj9vy44q0ll3l2x9niqzcbyqikydmlb5f3h5rsp26d3"))

(define rust-proc-macro2-diagnostics-0.10.1
  (crate-source "proc-macro2-diagnostics" "0.10.1"
                "1j48ipc80pykvhx6yhndfa774s58ax1h6sm6mlhf09ls76f6l1mg"))

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

(define rust-psm-0.1.25
  (crate-source "psm" "0.1.25"
                "125y7h40mkwb64j4v2v7s6f69ilk745kg60w1s2cq62cw8im93pm"
                #:snippet '(delete-file "src/arch/wasm32.o")))

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
                "00c0wkb112annn2wl72ixyd78mf56p4lxkhlmsggx65l3v3n8vbz"))

(define rust-quick-error-1.2.3
  (crate-source "quick-error" "1.2.3"
                "1q6za3v78hsspisc197bg3g7rpc989qycy8ypr8ap8igv10ikl51"))

(define rust-quick-error-2.0.1
  (crate-source "quick-error" "2.0.1"
                "18z6r2rcjvvf8cn92xjhm2qc3jpd1ljvcbf12zv0k9p565gmb4x9"))

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

(define rust-rand-0.8.5
  (crate-source "rand" "0.8.5"
                "013l6931nn7gkc23jz5mm3qdhf93jjf0fg64nz2lp4i51qd8vbrl"))

(define rust-rand-0.9.0
  (crate-source "rand" "0.9.0"
                "156dyvsfa6fjnv6nx5vzczay1scy5183dvjchd7bvs47xd5bjy9p"))

(define rust-rand-chacha-0.3.1
  (crate-source "rand_chacha" "0.3.1"
                "123x2adin558xbhvqb8w4f6syjsdkmqff8cxwhmjacpsl1ihmhg6"))

(define rust-rand-chacha-0.9.0
  (crate-source "rand_chacha" "0.9.0"
                "1jr5ygix7r60pz0s1cv3ms1f6pd1i9pcdmnxzzhjc3zn3mgjn0nk"))

(define rust-rand-core-0.6.4
  (crate-source "rand_core" "0.6.4"
                "0b4j2v4cb5krak1pv6kakv4sz6xcwbrmy2zckc32hsigbrwy82zc"))

(define rust-rand-core-0.9.3
  (crate-source "rand_core" "0.9.3"
                "0f3xhf16yks5ic6kmgxcpv1ngdhp48mmfy4ag82i1wnwh8ws3ncr"))

(define rust-rand-xorshift-0.3.0
  (crate-source "rand_xorshift" "0.3.0"
                "13vcag7gmqspzyabfl1gr9ykvxd2142q2agrj8dkyjmfqmgg4nyj"))

(define rust-rand-xoshiro-0.6.0
  (crate-source "rand_xoshiro" "0.6.0"
                "1ajsic84rzwz5qr0mzlay8vi17swqi684bqvwqyiim3flfrcv5vg"))

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

(define rust-redox-syscall-0.2.16
  (crate-source "redox_syscall" "0.2.16"
                "16jicm96kjyzm802cxdd1k9jmcph0db1a4lhslcnhjsvhp0mhnpv"))

(define rust-redox-syscall-0.5.10
  (crate-source "redox_syscall" "0.5.10"
                "1l9b638qx72312yzh8ykvda9b3lqd9gf6yqn66b23a331ck0r30b"))

(define rust-redox-syscall-0.5.11
  (crate-source "redox_syscall" "0.5.11"
                "18qijn18r10haiglv4261wb0yh1agqqlvs0nxfy8yjbpsb307wfj"))

(define rust-redox-users-0.4.6
  (crate-source "redox_users" "0.4.6"
                "0hya2cxx6hxmjfxzv9n8rjl5igpychav7zfi1f81pz6i4krry05s"))

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

(define rust-reqwest-0.12.15
  (crate-source "reqwest" "0.12.15"
                "1fvvrl3jmsnlm99ldl0ariklrlsmrky06qabp7dc92ylznk4d76i"))

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

(define rust-rpds-1.1.0
  (crate-source "rpds" "1.1.0"
                "194hjbsicmgqi3dyllqrz09mmhh597m2j9l49lr16cyfscambqd0"))

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

(define rust-rusqlite-0.32.1
  (crate-source "rusqlite" "0.32.1"
                "0vlx040bppl414pbjgbp7qr4jdxwszi9krx0m63zzf2f2whvflvp"))

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

(define rust-rustc-version-0.4.1
  (crate-source "rustc_version" "0.4.1"
                "14lvdsmr5si5qbqzrajgb6vfn69k0sfygrvfvr2mps26xwi3mjyg"))

(define rust-rustfix-0.9.0
  (crate-source "rustfix" "0.9.0"
                "1a79gyag6w459qani0a1m6asadz6vxvgvmrw4l94zzvifiniarkz"))

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

(define rust-same-file-1.0.6
  (crate-source "same-file" "1.0.6"
                "00h5j1w87dmhnvbv9l8bic3y7xxsnjmssvifw2ayvgx9mb1ivz4k"))

(define rust-scan-fmt-0.2.6
  (crate-source "scan_fmt" "0.2.6"
                "0j0jb1dsa8zjpnc875wy72190zlyngvl62mfv8pqwal8vfjv0lqb"))

(define rust-schannel-0.1.27
  (crate-source "schannel" "0.1.27"
                "0gbbhy28v72kd5iina0z2vcdl3vz63mk5idvkzn5r52z6jmfna8z"
                #:snippet '(delete-file-recursively "test")))

(define rust-schemars-0.8.22
  (crate-source "schemars" "0.8.22"
                "05an9nbi18ynyxv1rjmwbg6j08j0496hd64mjggh53mwp3hjmgrz"))

(define rust-schemars-derive-0.8.22
  (crate-source "schemars_derive" "0.8.22"
                "0kakyzrp5801s4i043l4ilv96lzimnlh01pap958h66n99w6bqij"))

(define rust-scopeguard-1.2.0
  (crate-source "scopeguard" "1.2.0"
                "0jcz9sd47zlsgcnm1hdw0664krxwb5gczlif4qngj2aif8vky54l"))

(define rust-scroll-0.12.0
  (crate-source "scroll" "0.12.0"
                "19mix9vm4k23jkknpgbi0ylmhpf2hnlpzzrfj9wqcj88lj55kf3a"))

(define rust-scroll-derive-0.12.0
  (crate-source "scroll_derive" "0.12.0"
                "0cmr3hxk318s2ivv37cik2l1r0d8r0qhahnin5lpxbr5w3yw50bz"))

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

(define rust-semver-1.0.26
  (crate-source "semver" "1.0.26"
                "1l5q2vb8fjkby657kdyfpvv40x2i2xqq9bg57pxqakfj92fgmrjn"))

(define rust-serde-1.0.219
  (crate-source "serde" "1.0.219"
                "1dl6nyxnsi82a197sd752128a4avm6mxnscywas1jq30srp2q3jz"))

(define rust-serde-big-array-0.5.1
  (crate-source "serde-big-array" "0.5.1"
                "0zsb9s9rcca3408kg20c6xpx917c9vbbnap5gvrf0wvdqz17rz0i"))

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

(define rust-serial-test-0.5.1
  (crate-source "serial_test" "0.5.1"
                "0pchc7imdi9wv8xxnwkb9lzs6cg06ghs0gaajjb834y8837wpg70"))

(define rust-serial-test-2.0.0
  (crate-source "serial_test" "2.0.0"
                "0b9v0csv9wxl1gcjq99plwimxbmhgr6kzbwqyb457qh3d22xsmhf"))

(define rust-serial-test-derive-0.5.1
  (crate-source "serial_test_derive" "0.5.1"
                "1m8sd97xr8dn6p9by0xwfqm0rz8cbn1ghs5l1fv1xd6xzvgddb5j"))

(define rust-serial-test-derive-2.0.0
  (crate-source "serial_test_derive" "2.0.0"
                "13zvd5ds76hhjn3z0axc05n15lzpxpz77jcykic8q5knhlbjklci"))

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

(define rust-sharded-slab-0.1.7
  (crate-source "sharded-slab" "0.1.7"
                "1xipjr4nqsgw34k7a2cgj9zaasl2ds6jwn89886kww93d32a637l"))

(define rust-shell-escape-0.1.5
  (crate-source "shell-escape" "0.1.5"
                "0kqq83dk0r1fqj4cfzddpxrni2hpz5i1y607g366c4m9iyhngfs5"))

(define rust-shell-words-1.1.0
  (crate-source "shell-words" "1.1.0"
                "1plgwx8r0h5ismbbp6cp03740wmzgzhip85k5hxqrrkaddkql614"))

(define rust-shlex-1.3.0
  (crate-source "shlex" "1.3.0"
                "0r1y6bv26c1scpxvhg2cabimrmwgbp4p3wy6syj9n0c4s3q2znhg"))

(define rust-signal-hook-0.3.17
  (crate-source "signal-hook" "0.3.17"
                "0098nsah04spqf3n8niirmfym4wsdgjl57c78kmzijlq8xymh8c6"))

(define rust-signal-hook-registry-1.4.2
  (crate-source "signal-hook-registry" "1.4.2"
                "1cb5akgq8ajnd5spyn587srvs4n26ryq0p78nswffwhv46sf1sd9"))

(define rust-signature-2.2.0
  (crate-source "signature" "2.2.0"
                "1pi9hd5vqfr3q3k49k37z06p7gs5si0in32qia4mmr1dancr6m3p"))

(define rust-simba-0.8.1
  (crate-source "simba" "0.8.1"
                "1bnf7ainywmaz2z67ss1q0bjwccf80c50c50r6hlpay69z4hf586"))

(define rust-simd-adler32-0.3.7
  (crate-source "simd-adler32" "0.3.7"
                "1zkq40c3iajcnr5936gjp9jjh1lpzhy44p3dq3fiw75iwr1w2vfn"))

(define rust-simd-helpers-0.1.0
  (crate-source "simd_helpers" "0.1.0"
                "19idqicn9k4vhd04ifh2ff41wvna79zphdf2c81rlmpc7f3hz2cm"))

(define rust-similar-2.7.0
  (crate-source "similar" "2.7.0"
                "1aidids7ymfr96s70232s6962v5g9l4zwhkvcjp4c5hlb6b5vfxv"))

(define rust-siphasher-0.3.11
  (crate-source "siphasher" "0.3.11"
                "03axamhmwsrmh0psdw3gf7c0zc4fyl5yjxfifz9qfka6yhkqid9q"))

(define rust-siphasher-1.0.1
  (crate-source "siphasher" "1.0.1"
                "17f35782ma3fn6sh21c027kjmd227xyrx06ffi8gw4xzv9yry6an"))

(define rust-sized-chunks-0.6.5
  (crate-source "sized-chunks" "0.6.5"
                "07ix5fsdnpf2xsb0k5rbiwlmsicm2237fcx7blirp9p7pljr5mhn"))

(define rust-slab-0.4.9
  (crate-source "slab" "0.4.9"
                "0rxvsgir0qw5lkycrqgb1cxsvxzjv9bmx73bk5y42svnzfba94lg"))

(define rust-smallvec-1.15.0
  (crate-source "smallvec" "1.15.0"
                "1sgfw8z729nlxk8k13dhs0a762wnaxmlx70a7xlf3wz989bjh5w9"))

(define rust-smawk-0.3.2
  (crate-source "smawk" "0.3.2"
                "0344z1la39incggwn6nl45k8cbw2x10mr5j0qz85cdz9np0qihxp"))

(define rust-smol-str-0.3.2
  (crate-source "smol_str" "0.3.2"
                "039mj6lc1vkljj17ndlzzkak8kvlmw8ppi6yjdxsh433snfbhxln"))

(define rust-snapbox-0.6.21
  (crate-source "snapbox" "0.6.21"
                "0ss3nd9ky0fkq7idj7jzr22kvkhxz3ylrq9fmiq5sdg3h52zrp4n"))

(define rust-snapbox-macros-0.3.10
  (crate-source "snapbox-macros" "0.3.10"
                "1bv4lq1kw1vrd9lk7yk79a0z8q8nma2502ifysv1p913r99rymhn"))

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

(define rust-string-cache-0.8.9
  (crate-source "string_cache" "0.8.9"
                "03z7km2kzlwiv2r2qifq5riv4g8phazwng9wnvs3py3lzainnxxz"))

(define rust-string-cache-codegen-0.5.4
  (crate-source "string_cache_codegen" "0.5.4"
                "181ir4d6y053s1kka2idpjx5g9d9jgll6fy517jhzzpi2n3r44f7"))

(define rust-strsim-0.10.0
  (crate-source "strsim" "0.10.0"
                "08s69r4rcrahwnickvi0kq49z524ci50capybln83mg6b473qivk"))

(define rust-strsim-0.11.1
  (crate-source "strsim" "0.11.1"
                "0kzvqlw8hxqb7y598w1s0hxlnmi84sg5vsipp3yg5na5d1rvba3x"))

(define rust-subtle-2.6.1
  (crate-source "subtle" "2.6.1"
                "14ijxaymghbl1p0wql9cib5zlwiina7kall6w7g89csprkgbvhhk"))

(define rust-supports-hyperlinks-3.1.0
  (crate-source "supports-hyperlinks" "3.1.0"
                "12r8d8ckdx78rhdsavh08gg4210i3bmcn2prm7k2s5b37knl8kw0"))

(define rust-supports-unicode-3.0.0
  (crate-source "supports-unicode" "3.0.0"
                "1qpc344453x3ai4k9iygxnbk6lr2nw5jflj8ns5q3dbcmwq1lh5p"))

(define rust-syn-1.0.109
  (crate-source "syn" "1.0.109"
                "0ds2if4600bd59wsv7jjgfkayfzy3hnazs394kz6zdkmna8l3dkj"))

(define rust-syn-2.0.100
  (crate-source "syn" "2.0.100"
                "18623wdkns03blpv65xsjn8fipl9p9hj98vlrnhin7nqran496mh"))

(define rust-sync-wrapper-1.0.2
  (crate-source "sync_wrapper" "1.0.2"
                "0qvjyasd6w18mjg5xlaq5jgy84jsjfsvmnn12c13gypxbv75dwhb"))

(define rust-synstructure-0.12.6
  (crate-source "synstructure" "0.12.6"
                "03r1lydbf3japnlpc4wka7y90pmz1i0danaj3f9a7b431akdlszk"))

(define rust-synstructure-0.13.1
  (crate-source "synstructure" "0.13.1"
                "0wc9f002ia2zqcbj0q2id5x6n7g1zjqba7qkg2mr0qvvmdk7dby8"))

(define rust-system-configuration-0.6.1
  (crate-source "system-configuration" "0.6.1"
                "0sxslml567zm0v8g732314vd2gk9sd3k4xj22xk6p64xir29v1rw"))

(define rust-system-configuration-sys-0.6.0
  (crate-source "system-configuration-sys" "0.6.0"
                "1i5sqrmgy58l4704hibjbl36hclddglh73fb3wx95jnmrq81n7cf"))

(define rust-system-deps-6.2.2
  (crate-source "system-deps" "6.2.2"
                "0j93ryw031n3h8b0nfpj5xwh3ify636xmv8kxianvlyyipmkbrd3"
                #:snippet '(delete-file-recursively "src/tests")))

(define rust-tame-index-0.18.1
  (crate-source "tame-index" "0.18.1"
                "0365pyq3qp7415z1xql03763krh63779gqdgxwc8l22dq5hrxkpz"))

(define rust-tar-0.4.44
  (crate-source "tar" "0.4.44"
                "0yk69a8j9xv51mdcy0853jai5zh1pd9yn456q4cpmj0js9w3i1hx"))

(define rust-target-lexicon-0.12.16
  (crate-source "target-lexicon" "0.12.16"
                "1cg3bnx1gdkdr5hac1hzxy64fhw4g7dqkd0n3dxy5lfngpr1mi31"))

(define rust-target-lexicon-0.13.2
  (crate-source "target-lexicon" "0.13.2"
                "16m6smfz533im9dyxfhnzmpi4af75g2iii36ylc4gfmqvf6gf0p5"))

(define rust-tempfile-3.19.1
  (crate-source "tempfile" "3.19.1"
                "1grmcj8y6rcavndw2dm18ndzdimsq5f8lcrwyg627cdrcdvsqdvl"))

(define rust-tendril-0.4.3
  (crate-source "tendril" "0.4.3"
                "1c3vip59sqwxn148i714nmkrvjzbk7105vj0h92s6r64bw614jnj"))

(define rust-termcolor-1.4.1
  (crate-source "termcolor" "1.4.1"
                "0mappjh3fj3p2nmrg4y7qv94rchwi9mzmgmfflr8p2awdj7lyy86"))

(define rust-terminal-size-0.4.2
  (crate-source "terminal_size" "0.4.2"
                "1vdm5xhzn7sqcsr762vmnavkhid3hs8w8qjyh9iwrr1990f4iij5"))

(define rust-termtree-0.5.1
  (crate-source "termtree" "0.5.1"
                "10s610ax6nb70yi7xfmwcb6d3wi9sj5isd0m63gy2pizr2zgwl4g"))

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

(define rust-thread-local-1.1.8
  (crate-source "thread_local" "1.1.8"
                "173i5lyjh011gsimk21np9jn8al18rxsrkjli20a7b8ks2xgk7lb"))

(define rust-tiff-0.9.1
  (crate-source "tiff" "0.9.1"
                "0ghyxlz566dzc3scvgmzys11dhq2ri77kb8sznjakijlxby104xs"
                #:snippet '(delete-file-recursively "tests")))

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

(define rust-tokio-1.44.1
  (crate-source "tokio" "1.44.1"
                "06n90q5hh1yd844s6nf4j3fwbrkm2bnq533kp3a488l4bdhxm0pk"))

(define rust-tokio-rustls-0.26.2
  (crate-source "tokio-rustls" "0.26.2"
                "16wf007q3584j46wc4s0zc4szj6280g23hka6x6bgs50l4v7nwlf"))

(define rust-tokio-util-0.7.14
  (crate-source "tokio-util" "0.7.14"
                "0d7hm1jrwpzryvni72fy5dg9blqs776wq5w38lwigk3g7swr15bb"))

(define rust-toml-0.5.11
  (crate-source "toml" "0.5.11"
                "0d2266nx8b3n22c7k24x4428z6di8n83a9n466jm7a2hipfz1xzl"))

(define rust-toml-0.8.20
  (crate-source "toml" "0.8.20"
                "0j012b37iz1mihksr6a928s6dzszxvblzg3l5wxp7azzsv6sb1yd"))

(define rust-toml-datetime-0.6.8
  (crate-source "toml_datetime" "0.6.8"
                "0hgv7v9g35d7y9r2afic58jvlwnf73vgd1mz2k8gihlgrf73bmqd"))

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

(define rust-tracing-log-0.2.0
  (crate-source "tracing-log" "0.2.0"
                "1hs77z026k730ij1a9dhahzrl0s073gfa2hm5p0fbl0b80gmz1gf"))

(define rust-tracing-subscriber-0.3.19
  (crate-source "tracing-subscriber" "0.3.19"
                "0220rignck8072i89jjsh140vmh14ydwpdwnifyaf3xcnpn9s678"))

(define rust-triomphe-0.1.14
  (crate-source "triomphe" "0.1.14"
                "11fciha522hrz6pkafy3xlq20w405w9dqvy9ln7ba1s8v8k7g3zg"))

(define rust-try-lock-0.2.5
  (crate-source "try-lock" "0.2.5"
                "0jqijrrvm1pyq34zn1jmy2vihd4jcrjlvsh4alkjahhssjnsn8g4"))

(define rust-trycmd-0.15.9
  (crate-source "trycmd" "0.15.9"
                "1r5a5r22j7gi69y0zdbwhb6d2hp8r34plnfncp0alql870lwzdd8"))

(define rust-twox-hash-2.1.0
  (crate-source "twox-hash" "2.1.0"
                "022rwrv24rl6g32nqv1mywf6vdnkn7vq34fg793vll1hgccpzcg7"))

(define rust-typeid-1.0.3
  (crate-source "typeid" "1.0.3"
                "0727ypay2p6mlw72gz3yxkqayzdmjckw46sxqpaj08v0b0r64zdw"))

(define rust-typenum-1.18.0
  (crate-source "typenum" "1.18.0"
                "0gwgz8n91pv40gabrr1lzji0b0hsmg0817njpy397bq7rvizzk0x"))

(define rust-uluru-3.1.0
  (crate-source "uluru" "3.1.0"
                "1njp6vvy1mm8idnsp6ljyxx5znfsk3xkmk9cr2am0vkfwmlj92kw"))

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

(define rust-unscanny-0.1.0
  (crate-source "unscanny" "0.1.0"
                "0ivbipc1rnq15fhzgna41p1h01ncq4shycii72f3x5d7czq2mpz9"))

(define rust-untrusted-0.9.0
  (crate-source "untrusted" "0.9.0"
                "1ha7ib98vkc538x0z60gfn0fc5whqdd85mb87dvisdcaifi6vjwf"
                #:snippet '(delete-file-recursively "mk")))

(define rust-ureq-2.12.1
  (crate-source "ureq" "2.12.1"
                "07f0qdn6459k4rmdnkivkz0y7j28vxh5c8q8sr0gcxgdfxiadl82"))

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

(define rust-utf8parse-0.2.2
  (crate-source "utf8parse" "0.2.2"
                "088807qwjq46azicqwbhlmzwrbkz7l4hpw43sdkdyyk524vdxaq6"))

(define rust-v-frame-0.3.8
  (crate-source "v_frame" "0.3.8"
                "0az9nd6qi1gyikh9yb3lhm453kf7d5isd6xai3j13kds4jm2mwyn"))

(define rust-valuable-0.1.1
  (crate-source "valuable" "0.1.1"
                "0r9srp55v7g27s5bg7a2m095fzckrcdca5maih6dy9bay6fflwxs"))

(define rust-vcpkg-0.2.15
  (crate-source "vcpkg" "0.2.15"
                "09i4nf5y8lig6xgj3f7fyrvzd3nlaw4znrihw8psidvv5yk4xkdc"
                #:snippet '(delete-file-recursively "test-data")))

(define rust-version-check-0.9.5
  (crate-source "version_check" "0.9.5"
                "0nhhi4i5x89gm911azqbn7avs9mdacw2i3vcz3cnmz3mv4rqz4hb"))

(define rust-version-compare-0.2.0
  (crate-source "version-compare" "0.2.0"
                "12y9262fhjm1wp0aj3mwhads7kv0jz8h168nn5fb8b43nwf9abl5"))

(define rust-wait-timeout-0.2.1
  (crate-source "wait-timeout" "0.2.1"
                "04azqv9mnfxgvnc8j2wp362xraybakh2dy1nj22gj51rdl93pb09"))

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

(define rust-wasmparser-0.207.0
  (crate-source "wasmparser" "0.207.0"
                "0b694q3frf4xvavj0rw7xk3j852gqljdp2pghajnsq87mgwbk6z1"))

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

(define rust-wide-0.7.32
  (crate-source "wide" "0.7.32"
                "08mb6iqdscqiqrbfkjrnfr876ah4cc0cx5pjilz3yqw1k9mmgda1"))

(define rust-wild-2.2.1
  (crate-source "wild" "2.2.1"
                "1q8hnhmv3fvgx0j7bv8qig00599a15mfsdhgx3hq2ljpiky1l4x3"))

(define rust-winapi-0.3.9
  (crate-source "winapi" "0.3.9"
                "06gl025x418lchw1wxj64ycr7gha83m44cjr5sarhynd9xkrm0sw"))

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

(define rust-windows-core-0.61.0
  (crate-source "windows-core" "0.61.0"
                "104915nsby2cgp322pqqkmj2r82v5sg4hil0hxddg1hc67gc2qs7"))

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

(define rust-windows-implement-0.60.0
  (crate-source "windows-implement" "0.60.0"
                "0dm88k3hlaax85xkls4gf597ar4z8m5vzjjagzk910ph7b8xszx4"))

(define rust-windows-interface-0.59.1
  (crate-source "windows-interface" "0.59.1"
                "1a4zr8740gyzzhq02xgl6vx8l669jwfby57xgf0zmkcdkyv134mx"))

(define rust-windows-link-0.1.1
  (crate-source "windows-link" "0.1.1"
                "0f2cq7imbrppsmmnz8899hfhg07cp5gq6rh0bjhb1qb6nwshk13n"))

(define rust-windows-registry-0.4.0
  (crate-source "windows-registry" "0.4.0"
                "18wbgr6z6765qdnasi8mmvxhvp82xd1zlvd6s7pp2l5lvn8av1j2"))

(define rust-windows-result-0.3.2
  (crate-source "windows-result" "0.3.2"
                "0li2f76anf0rg7i966d9qs5iprsg555g9rgyzj7gcpfr9wdd2ky6"))

(define rust-windows-strings-0.3.1
  (crate-source "windows-strings" "0.3.1"
                "06bkhkyclbfchcsv5bnhz77r290k20m15glj2xq60ra0bp64iyl7"))

(define rust-windows-strings-0.4.0
  (crate-source "windows-strings" "0.4.0"
                "15rg6a0ha1d231wwps2qlgyqrgkyj1r8v9vsb8nlbvih4ijajavs"))

(define rust-windows-sys-0.48.0
  (crate-source "windows-sys" "0.48.0"
                "1aan23v5gs7gya1lc46hqn9mdh8yph3fhxmhxlw36pn6pqc28zb7"))

(define rust-windows-sys-0.52.0
  (crate-source "windows-sys" "0.52.0"
                "0gd3v4ji88490zgb6b5mq5zgbvwv7zx1ibn8v3x83rwcdbryaar8"))

(define rust-windows-sys-0.59.0
  (crate-source "windows-sys" "0.59.0"
                "0fw5672ziw8b3zpmnbp9pdv1famk74f1l9fcbc3zsrzdg56vqf0y"))

(define rust-windows-targets-0.48.5
  (crate-source "windows-targets" "0.48.5"
                "034ljxqshifs1lan89xwpcy1hp0lhdh4b5n0d2z4fwjx2piacbws"))

(define rust-windows-targets-0.52.6
  (crate-source "windows-targets" "0.52.6"
                "0wwrx625nwlfp7k93r2rra568gad1mwd888h1jwnl0vfg5r4ywlv"))

(define rust-windows-targets-0.53.0
  (crate-source "windows-targets" "0.53.0"
                "12yakpjizhfpppz1i3zgcwxlbar8axrp9j87fmywpydarvlcgr5i"))

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

(define rust-winnow-0.6.26
  (crate-source "winnow" "0.6.26"
                "0a4sjbbrkhbd0ba6dy0011hln1q3ry4iv6srqjjpi8hsmk9fv40y"))

(define rust-winnow-0.7.4
  (crate-source "winnow" "0.7.4"
                "0dmbsz6zfddcgsqzzqxw1h8f7zy19x407g7zl3hyp6vf2m2bb5qf"))

(define rust-winsafe-0.0.19
  (crate-source "winsafe" "0.0.19"
                "0169xy9mjma8dys4m8v4x0xhw2gkbhv2v1wsbvcjl9bhnxxd2dfi"))

(define rust-wit-bindgen-rt-0.39.0
  (crate-source "wit-bindgen-rt" "0.39.0"
                "1hd65pa5hp0nl664m94bg554h4zlhrzmkjsf6lsgsb7yc4734hkg"
                #:snippet '(for-each delete-file (find-files "." "\\.(a|o)$"))))

(define rust-write16-1.0.0
  (crate-source "write16" "1.0.0"
                "0dnryvrrbrnl7vvf5vb1zkmwldhjkf2n5znliviam7bm4900z2fi"))

(define rust-writeable-0.5.5
  (crate-source "writeable" "0.5.5"
                "0lawr6y0bwqfyayf3z8zmqlhpnzhdx0ahs54isacbhyjwa7g778y"))

(define rust-xattr-1.5.0
  (crate-source "xattr" "1.5.0"
                "17nq2c23zcjciz8sxwhisqjkv4l7zcylx3yl2915c59cy7rcnr8d"))

(define rust-xml5ever-0.17.0
  (crate-source "xml5ever" "0.17.0"
                "0l76v0c228c92sskiflpsy19c0bgc8q7flhlfanm32zrbb8f2d20"))

(define rust-y4m-0.8.0
  (crate-source "y4m" "0.8.0"
                "0j24y2zf60lpxwd7kyg737hqfyqx16y32s0fjyi6fax6w4hlnnks"
                #:snippet '(delete-file-recursively "scripts")))

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

(define rust-zerovec-0.10.4
  (crate-source "zerovec" "0.10.4"
                "0yghix7n3fjfdppwghknzvx9v8cf826h2qal5nqvy8yzg4yqjaxa"))

(define rust-zerovec-derive-0.10.3
  (crate-source "zerovec-derive" "0.10.3"
                "1ik322dys6wnap5d3gcsn09azmssq466xryn5czfm13mn7gsdbvf"))

(define rust-zip-2.6.1
  (crate-source "zip" "2.6.1"
                "0i276d0kracqv27f5r42p3ha2345f77isv5rp54sw9i52p829jqx"))

(define rust-zopfli-0.8.1
  (crate-source "zopfli" "0.8.1"
                "0ip9azz9ldk19m0m1hdppz3n5zcz0cywbg1vx59g4p5c3cwry0g5"))

(define rust-zune-inflate-0.2.54
  (crate-source "zune-inflate" "0.2.54"
                "00kg24jh3zqa3i6rg6yksnb71bch9yi1casqydl00s7nw8pk7avk"))

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
