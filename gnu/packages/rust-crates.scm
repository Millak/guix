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

(define rust-approx-0.5.1
  (crate-source "approx" "0.5.1"
                "1ilpv3dgd58rasslss0labarq7jawxmivk17wsh8wmkdm3q15cfa"))

(define rust-arc-swap-1.7.1
  (crate-source "arc-swap" "1.7.1"
                "0mrl9a9r9p9bln74q6aszvf22q1ijiw089jkrmabfqkbj31zixv9"))

(define rust-arrayvec-0.7.6
  (crate-source "arrayvec" "0.7.6"
                "0l1fz4ccgv6pm609rif37sl5nv5k6lbzi7kkppgzqzh1vwix20kw"))

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

(define rust-backtrace-0.3.71
  (crate-source "backtrace" "0.3.71"
                "17bgd7pbjb9gc8q47qwsg2lmy9i62x3bsjmmnjrwh5z8s805ic16"))

(define rust-base64-0.22.1
  (crate-source "base64" "0.22.1"
                "1imqzgh7bxcikp5vx3shqvw9j09g9ly0xr0jma0q66i52r7jbcvj"))

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

(define rust-block-0.1.6
  (crate-source "block" "0.1.6"
                "16k9jgll25pzsq14f244q22cdv0zb4bqacldg3kx6h89d7piz30d"))

(define rust-borsh-1.5.7
  (crate-source "borsh" "1.5.7"
                "1kikljm5yr3l9qsw5xvdccragxj4445s4s3fqsgy6hmmipwld1md"))

(define rust-bstr-1.11.3
  (crate-source "bstr" "1.11.3"
                "1q3g2wmrvclgx7lk2p6mpzhqxzx41hyg962gkmlyxql1liar26jk"))

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

(define rust-cargo-lock-10.1.0
  (crate-source "cargo-lock" "10.1.0"
                "0m74y8w9wn7rl5mpzr0436r6fshf3qhm7d3wl02s4ys0f57wnsn0"))

(define rust-cargo-metadata-0.18.1
  (crate-source "cargo_metadata" "0.18.1"
                "0drh0zndl4qgndy6kg6783cydbvhxgv0hcg7d9hhqx0zwi3nb21d"))

(define rust-cargo-platform-0.1.9
  (crate-source "cargo-platform" "0.1.9"
                "1sinpmqjdk3q9llbmxr0h0nyvqrif1r5qs34l000z73b024z2np3"))

(define rust-cast-0.3.0
  (crate-source "cast" "0.3.0"
                "1dbyngbyz2qkk0jn2sxil8vrz3rnpcj142y184p9l4nbl9radcip"))

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

(define rust-chrono-0.4.40
  (crate-source "chrono" "0.4.40"
                "0z334kqnvq5zx6xsq1k6zk8g9z14fgk2w3vkn4n13pvi3mhn8y8s"))

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

(define rust-color-quant-1.1.0
  (crate-source "color_quant" "1.1.0"
                "12q1n427h2bbmmm1mnglr57jaz2dj9apk0plcxw7nwqiai7qjyrx"))

(define rust-colorchoice-1.0.3
  (crate-source "colorchoice" "1.0.3"
                "1439m3r3jy3xqck8aa13q658visn71ki76qa93cy55wkmalwlqsv"))

(define rust-core-foundation-0.10.0
  (crate-source "core-foundation" "0.10.0"
                "0qscay14s2rwkg8nd8ljhiaf149hj8sfy95d70zssy64r3jp2lmm"))

(define rust-core-foundation-0.9.4
  (crate-source "core-foundation" "0.9.4"
                "13zvbbj07yk3b61b8fhwfzhy35535a583irf23vlcg59j7h9bqci"))

(define rust-core-foundation-sys-0.8.7
  (crate-source "core-foundation-sys" "0.8.7"
                "12w8j73lazxmr1z0h98hf3z623kl8ms7g07jch7n4p8f9nwlhdkp"))

(define rust-crc32fast-1.4.2
  (crate-source "crc32fast" "1.4.2"
                "1czp7vif73b8xslr3c9yxysmh9ws2r8824qda7j47ffs9pcnjxx9"))

(define rust-criterion-0.5.1
  (crate-source "criterion" "0.5.1"
                "0bv9ipygam3z8kk6k771gh9zi0j0lb9ir0xi1pc075ljg80jvcgj"))

(define rust-criterion-plot-0.5.0
  (crate-source "criterion-plot" "0.5.0"
                "1c866xkjqqhzg4cjvg01f8w6xc1j3j7s58rdksl52skq89iq4l3b"))

(define rust-crossbeam-channel-0.5.14
  (crate-source "crossbeam-channel" "0.5.14"
                "0wa41qybq5w8s70anb472myh4fid4aw6v65vws6wn528w9l6vfh6"))

(define rust-crossbeam-deque-0.8.6
  (crate-source "crossbeam-deque" "0.8.6"
                "0l9f1saqp1gn5qy0rxvkmz4m6n7fc0b3dbm6q1r5pmgpnyvi3lcx"))

(define rust-crossbeam-epoch-0.9.18
  (crate-source "crossbeam-epoch" "0.9.18"
                "03j2np8llwf376m3fxqx859mgp9f83hj1w34153c7a9c7i5ar0jv"))

(define rust-crossbeam-utils-0.8.21
  (crate-source "crossbeam-utils" "0.8.21"
                "0a3aa2bmc8q35fb67432w16wvi54sfmb69rk9h5bhd18vw0c99fh"))

(define rust-crunchy-0.2.3
  (crate-source "crunchy" "0.2.3"
                "0aa9k4izp962qlsn5ndgw2zq62mizcpnkns8bxscgz3gqr35knj3"))

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

(define rust-cvss-2.0.0
  (crate-source "cvss" "2.0.0"
                "03q1nh4jy0cvgckji1jr1kz3j7gf2zg74240j8qi1qxhk7vs5iky"))

(define rust-dashmap-5.5.3
  (crate-source "dashmap" "5.5.3"
                "0miqnlxi501vfbv6mw5jbmzgnj0wjrch3p4abvpd59s9v30lg1wp"))

(define rust-data-url-0.3.1
  (crate-source "data-url" "0.3.1"
                "0ahclz72myi350cs1xcsxdh1v0iljpfj4ghcy2fy46mpfhf7laaw"))

(define rust-deranged-0.4.1
  (crate-source "deranged" "0.4.1"
                "0n7hswnz5jz1rjy6zr8sc9awbszkmv1345hphccawj40w1larkr8"))

(define rust-derive-more-0.99.19
  (crate-source "derive_more" "0.99.19"
                "17y6g78dg31fsv7z4p455bzxs670spg476ww2ibg3mj3vww9m8ix"))

(define rust-diff-0.1.13
  (crate-source "diff" "0.1.13"
                "1j0nzjxci2zqx63hdcihkp0a4dkdmzxd7my4m7zk6cjyfy34j9an"))

(define rust-difflib-0.4.0
  (crate-source "difflib" "0.4.0"
                "1s7byq4d7jgf2hcp2lcqxi2piqwl8xqlharfbi8kf90n8csy7131"))

(define rust-display-error-chain-0.2.2
  (crate-source "display-error-chain" "0.2.2"
                "1xbcilzyfc8n60vjkmsf8v53kw855xw68jh69hpza6dwhrp19hhb"))

(define rust-displaydoc-0.2.5
  (crate-source "displaydoc" "0.2.5"
                "1q0alair462j21iiqwrr21iabkfnb13d6x5w95lkdg21q2xrqdlp"))

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

(define rust-either-1.15.0
  (crate-source "either" "1.15.0"
                "069p1fknsmzn9llaizh77kip0pqmcwpdsykv2x30xpjyija5gis8"))

(define rust-encoding-rs-0.8.35
  (crate-source "encoding_rs" "0.8.35"
                "1wv64xdrr9v37rqqdjsyb8l8wzlcbab80ryxhrszvnj59wy0y0vm"))

(define rust-env-logger-0.10.2
  (crate-source "env_logger" "0.10.2"
                "1005v71kay9kbz1d5907l0y7vh9qn2fqsp2yfgb8bjvin6m0bm2c"))

(define rust-equivalent-1.0.2
  (crate-source "equivalent" "1.0.2"
                "03swzqznragy8n0x31lqc78g2af054jwivp7lkrbrc0khz74lyl7"))

(define rust-errno-0.3.11
  (crate-source "errno" "0.3.11"
                "0kjrrcaa5nvickysw7z3vm5p0l7l457idf1ff3z6ang8qwnx8vcp"))

(define rust-exr-1.73.0
  (crate-source "exr" "1.73.0"
                "1q47yq78q9k210r6jy1wwrilxwwxqavik9l3l426rd17k7srfcgq"))

(define rust-eyre-0.6.12
  (crate-source "eyre" "0.6.12"
                "1v1a3vb9gs5zkwp4jzkcfnpg0gvyp4ifydzx37f4qy14kzcibnbw"))

(define rust-faster-hex-0.9.0
  (crate-source "faster-hex" "0.9.0"
                "10wi4vqbdpkamw4qvra1ijp4as2j7j1zc66g4rdr6h0xv8gb38m2"))

(define rust-fastrand-2.3.0
  (crate-source "fastrand" "2.3.0"
                "1ghiahsw1jd68df895cy5h3gzwk30hndidn3b682zmshpgmrx41p"))

(define rust-fdeflate-0.3.7
  (crate-source "fdeflate" "0.3.7"
                "130ga18vyxbb5idbgi07njymdaavvk6j08yh1dfarm294ssm6s0y"
                #:snippet '(delete-file-recursively "tests")))

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

(define rust-form-urlencoded-1.2.1
  (crate-source "form_urlencoded" "1.2.1"
                "0milh8x7nl4f450s3ddhg57a3flcv6yq8hlkyk6fyr3mcb128dp1"))

(define rust-fs-err-2.11.0
  (crate-source "fs-err" "2.11.0"
                "0hdajzh5sjvvdjg0n15j91mv8ydvb7ff6m909frvdmg1bw81z948"))

(define rust-futf-0.1.5
  (crate-source "futf" "0.1.5"
                "0hvqk2r7v4fnc34hvc3vkri89gn52d5m9ihygmwn75l1hhp0whnz"))

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

(define rust-gio-0.19.8
  (crate-source "gio" "0.19.8"
                "1znz5ngfvv3gbndf6lzz3hs27hlb8ysls4axlfccrzvkscbz2jac"))

(define rust-gio-sys-0.19.8
  (crate-source "gio-sys" "0.19.8"
                "1vylsskpipfwl7mvffp1s0227d0k5amyhd32dfnp3mhl8yx47mrc"))

(define rust-gix-0.70.0
  (crate-source "gix" "0.70.0"
                "0s3b5407lqx9nf81xfrmka6l269551kkwm9blmpabwq5cxii8vvk"))

(define rust-gix-actor-0.33.2
  (crate-source "gix-actor" "0.33.2"
                "1cp47vxcd7f7nf225spdhncqqsrcjcf5qc68zkqnbq1jccd8l090"))

(define rust-gix-attributes-0.24.0
  (crate-source "gix-attributes" "0.24.0"
                "0f6vdp77d5z98bv3w6i71zlaqcgf8bch4qfa3rj5zvv2yq5h0lgi"))

(define rust-gix-bitmap-0.2.14
  (crate-source "gix-bitmap" "0.2.14"
                "0h3msc00gi2vr2k4q41ddb68qprbvkih824glq6na0lmqrjrgnxi"))

(define rust-gix-chunk-0.4.11
  (crate-source "gix-chunk" "0.4.11"
                "0vxxq4q5pn5cz2xhghcjpp8z83r8xxy74gsffvf9k1lmcj3is7qb"))

(define rust-gix-command-0.4.1
  (crate-source "gix-command" "0.4.1"
                "1wcdm6f8v28y2rv5lmz7kh4lnkdzplc92nh2c9gb8papss20nhfb"))

(define rust-gix-commitgraph-0.26.0
  (crate-source "gix-commitgraph" "0.26.0"
                "0xs85svhri8b40paa3zjjxfqzl6g3ganxnxg1nhjcq51v318wfp2"))

(define rust-gix-config-0.43.0
  (crate-source "gix-config" "0.43.0"
                "1sfry54k4f35ar6y0d7n52ccwyq9r192kkdkw1lx9m8l43yiwz1p"))

(define rust-gix-config-value-0.14.12
  (crate-source "gix-config-value" "0.14.12"
                "1dj4g52s18ab01pnw55rd0qdf7frdxryzawccy21h56gqi2cihld"))

(define rust-gix-credentials-0.27.0
  (crate-source "gix-credentials" "0.27.0"
                "0icymf6v01y2r07bmwaw3vb1mx59m2x54lcb732bj2v9w6g0z5fg"))

(define rust-gix-date-0.9.4
  (crate-source "gix-date" "0.9.4"
                "1r0pc9ra4r7qxwsyd0jvxh3vsnm3jvkgkr19qbxi2dbxxic018ys"))

(define rust-gix-diff-0.50.0
  (crate-source "gix-diff" "0.50.0"
                "0kbwn5js7qwnqxxva52hrhxrkwhvxfr6a86rvblz9k8arbsbgbv2"))

(define rust-gix-discover-0.38.0
  (crate-source "gix-discover" "0.38.0"
                "1n35pfcr4didkxswigy4lvwkqrhyvbgjk82sb87lw1h4vx5l3hnh"))

(define rust-gix-features-0.40.0
  (crate-source "gix-features" "0.40.0"
                "0m6mf6f341shzs5b1iy79klkw00x84kba34z5i4bshldia1x9zcb"))

(define rust-gix-filter-0.17.0
  (crate-source "gix-filter" "0.17.0"
                "1frbjkmwrafbp7psbnh9rp9szlakcn44b1jmqc7fsqxwgp6kdk5x"))

(define rust-gix-fs-0.13.0
  (crate-source "gix-fs" "0.13.0"
                "0g86cb2i18c7jnj8i9691a3h07nz7hvinig7ryvzyi6zpykpybhq"))

(define rust-gix-glob-0.18.0
  (crate-source "gix-glob" "0.18.0"
                "0kii7bpz1vcdykb0x1k9zmhn22hynwyk4n5acfrzjy0az94p572f"))

(define rust-gix-hash-0.16.0
  (crate-source "gix-hash" "0.16.0"
                "1y79zcwja9b1bqlr27awndla5wcmzd7a8rnh7qdq5ca9hv25w778"))

(define rust-gix-hashtable-0.7.0
  (crate-source "gix-hashtable" "0.7.0"
                "1l8jq85fkfw4inmpd6w2pk1dq67krsqmmp100lpd1k1a6yy3148q"))

(define rust-gix-ignore-0.13.0
  (crate-source "gix-ignore" "0.13.0"
                "0vyz5jfqd72b4pygwqrssr96jvfzi32hm7y4lz05b65zh35rsljg"))

(define rust-gix-index-0.38.0
  (crate-source "gix-index" "0.38.0"
                "1n45vkbmkpc4m570rdanyqz62a68mihsrqpz1wqnk4w74qv2xldc"))

(define rust-gix-lock-16.0.0
  (crate-source "gix-lock" "16.0.0"
                "0hn696w506zwqfl9pjhijaqkshzr5lb4v0j1hjb40sgzf1982fcp"))

(define rust-gix-negotiate-0.18.0
  (crate-source "gix-negotiate" "0.18.0"
                "107gh0yn4z1lnzljlr538gg5bs9k9mzjncam1g9h7qxvywgaza56"))

(define rust-gix-object-0.47.0
  (crate-source "gix-object" "0.47.0"
                "0s7xwm1nmx2zp10qnrlxh8vmw5nakjkvfzrl4bzg0i220jhb7i6x"))

(define rust-gix-odb-0.67.0
  (crate-source "gix-odb" "0.67.0"
                "06ww8mc10iydvqxdin0miny89g9z8i7zmsccc1rrbl4wyrylb4ry"))

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

(define rust-gix-pathspec-0.9.0
  (crate-source "gix-pathspec" "0.9.0"
                "0v7q0b55fn0raaj52cg75bi5yc8pijkzl1lq05crv3n0hskd6c34"))

(define rust-gix-prompt-0.9.1
  (crate-source "gix-prompt" "0.9.1"
                "0v9v9icnryhcx2z256kmm8aa1p31ipghjx80kac2nlg1b1ciiwkr"))

(define rust-gix-protocol-0.48.0
  (crate-source "gix-protocol" "0.48.0"
                "145sln6g810vab9jhwiz3r1bwr61jh1i1qj168hpvdn6mxhvsqbc"))

(define rust-gix-quote-0.4.15
  (crate-source "gix-quote" "0.4.15"
                "1ik6l3s0hjb2p4dlgdarb59v7n9jvgvak4ij786mrj5hrpy5g4z4"))

(define rust-gix-ref-0.50.0
  (crate-source "gix-ref" "0.50.0"
                "03723r9s3m3grmjzcasxp7jcz0z5xs90spg9aj2ryhikz72z9ba7"))

(define rust-gix-refspec-0.28.0
  (crate-source "gix-refspec" "0.28.0"
                "140aif2nciz9j9a0h9lqsg8cb1pkzhbza9bsgy7gc4pnv0l04rar"))

(define rust-gix-revision-0.32.0
  (crate-source "gix-revision" "0.32.0"
                "0lvb7rrjjdr9h21ign5g0za2jg00nimzqvkcdvbacpd5rjy8pqiz"))

(define rust-gix-revwalk-0.18.0
  (crate-source "gix-revwalk" "0.18.0"
                "0iv2c479w9lkjwbngdwyial6km8dllgah8wvp7r9w7jv4c6biv6l"))

(define rust-gix-sec-0.10.12
  (crate-source "gix-sec" "0.10.12"
                "122qvp6sll7hkrpjiwf9ga1dni0gwf6j3zzm6cq2zvz97pqv1bj7"))

(define rust-gix-shallow-0.2.0
  (crate-source "gix-shallow" "0.2.0"
                "0rjhwcjjixfy4fbzciyz5mikkvq38rwfyny86ckya0z324q58wmb"))

(define rust-gix-submodule-0.17.0
  (crate-source "gix-submodule" "0.17.0"
                "1b532y2c7qg8axqc2nkw2mdiq8mg9hxq87mfj2aa1j3askl2z5vl"))

(define rust-gix-tempfile-16.0.0
  (crate-source "gix-tempfile" "16.0.0"
                "00c5czgzzi3c8yxv24vh1rmkgf06vgb1ypf5521lmwjyjhiz8n15"))

(define rust-gix-trace-0.1.12
  (crate-source "gix-trace" "0.1.12"
                "1xv54v5y91vxjx351wl3yk66fwk7ybkna2knbxlnj34j6qh6lfbw"))

(define rust-gix-transport-0.45.0
  (crate-source "gix-transport" "0.45.0"
                "1nb4p7jwy80g51afzc64ya1faxxcpgnimbk2p2sv2xwl90c7860i"))

(define rust-gix-traverse-0.44.0
  (crate-source "gix-traverse" "0.44.0"
                "1d311l7wlgpv41hvp1ni3r9hhwxn4x27xyiy5brnwn4n73jp1v1b"))

(define rust-gix-url-0.29.0
  (crate-source "gix-url" "0.29.0"
                "04qb2p68886axrbx5gdjlhqwg55j0pn7zn25c08qzpakidv8q899"))

(define rust-gix-utils-0.1.14
  (crate-source "gix-utils" "0.1.14"
                "0pykxyp0cm2x8lj4ryj1pflksf9k7iyrshf8g321d2dc0d7g427z"))

(define rust-gix-validate-0.9.4
  (crate-source "gix-validate" "0.9.4"
                "11204daz5qlk9kqnmiq4syv0n21phkiy3xkwxmwnrnh964jz3d9l"))

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

(define rust-gobject-sys-0.19.8
  (crate-source "gobject-sys" "0.19.8"
                "17lb7dfbpcg8zchwlfbc08kckwf0a7d9n5ly3pyic13f5ljpws9f"))

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

(define rust-home-0.5.11
  (crate-source "home" "0.5.11"
                "1kxb4k87a9sayr8jipr7nq9wpgmjk4hk4047hmf9kc24692k75aq"))

(define rust-http-1.3.1
  (crate-source "http" "1.3.1"
                "0r95i5h7dr1xadp1ac9453w0s62s27hzkam356nyx2d9mqqmva7l"))

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

(define rust-image-0.24.9
  (crate-source "image" "0.24.9"
                "17gnr6ifnpzvhjf6dwbl9hki8x6bji5mwcqp0048x1jm5yfi742n"
                #:snippet '(for-each delete-file-recursively '("examples" "tests"))))

(define rust-indenter-0.3.3
  (crate-source "indenter" "0.3.3"
                "10y6i6y4ls7xsfsc1r3p5j2hhbxhaqnk5zzk8aj52b14v05ba8yf"))

(define rust-indexmap-1.9.3
  (crate-source "indexmap" "1.9.3"
                "16dxmy7yvk51wvnih3a3im6fp5lmx0wx76i03n06wyak6cwhw1xx"))

(define rust-indexmap-2.9.0
  (crate-source "indexmap" "2.9.0"
                "07m15a571yywmvqyb7ms717q9n42b46badbpsmx215jrg7dhv9yf"))

(define rust-instant-0.1.13
  (crate-source "instant" "0.1.13"
                "08h27kzvb5jw74mh0ajv0nv9ggwvgqm8ynjsn2sa9jsks4cjh970"))

(define rust-io-close-0.3.7
  (crate-source "io-close" "0.3.7"
                "1g4hldfn436rkrx3jlm4az1y5gdmkcixdlhkwy64yx06gx2czbcw"))

(define rust-ipnet-2.11.0
  (crate-source "ipnet" "2.11.0"
                "0c5i9sfi2asai28m8xp48k5gvwkqrg5ffpi767py6mzsrswv17s6"))

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

(define rust-itoa-1.0.15
  (crate-source "itoa" "1.0.15"
                "0b4fj9kz54dr3wam0vprjwgygvycyw8r0qwg7vp19ly8b2w16psa"))

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

(define rust-jpeg-decoder-0.3.1
  (crate-source "jpeg-decoder" "0.3.1"
                "1c1k53svpdyfhibkmm0ir5w0v3qmcmca8xr8vnnmizwf6pdagm7m"
                #:snippet '(delete-file-recursively "benches")))

(define rust-js-sys-0.3.77
  (crate-source "js-sys" "0.3.77"
                "13x2qcky5l22z4xgivi59xhjjx4kxir1zg7gcj0f1ijzd4yg7yhw"))

(define rust-kstring-2.0.2
  (crate-source "kstring" "2.0.2"
                "1lfvqlqkg2x23nglznb7ah6fk3vv3y5i759h5l2151ami98gk2sm"))

(define rust-language-tags-0.3.2
  (crate-source "language-tags" "0.3.2"
                "124k6w9nx33q4xs8rpa9f7klshrsa0x4f7qngdwq890lpdj5jd6l"))

(define rust-lazy-static-1.5.0
  (crate-source "lazy_static" "1.5.0"
                "1zk6dqqni0193xg6iijh7i3i44sryglwgvx20spdvwk3r6sbrlmv"))

(define rust-lebe-0.5.2
  (crate-source "lebe" "0.5.2"
                "1j2l6chx19qpa5gqcw434j83gyskq3g2cnffrbl3842ymlmpq203"))

(define rust-libc-0.2.171
  (crate-source "libc" "0.2.171"
                "1mipla3dy3l59pfa9xy4iw2vdgn8n30dzf4vdnasjflxdqhkg6f1"))

(define rust-libloading-0.8.6
  (crate-source "libloading" "0.8.6"
                "0d2ccr88f8kv3x7va2ccjxalcjnhrci4j2kwxp7lfmbkpjs4wbzw"
                #:snippet '(delete-file-recursively "tests")))

(define rust-libredox-0.1.3
  (crate-source "libredox" "0.1.3"
                "139602gzgs0k91zb7dvgj1qh4ynb8g1lbxsswdim18hcb6ykgzy0"))

(define rust-linked-hash-map-0.5.6
  (crate-source "linked-hash-map" "0.5.6"
                "03vpgw7x507g524nx5i1jf5dl8k3kv0fzg8v3ip6qqwbpkqww5q7"))

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

(define rust-mime-0.3.17
  (crate-source "mime" "0.3.17"
                "16hkibgvb9klh0w0jk5crr5xv90l3wlf77ggymzjmvl1818vnxv8"))

(define rust-minicov-0.3.7
  (crate-source "minicov" "0.3.7"
                "0jsvi62lklfyvdmsiizipkqcfpsc7h4c4illgxlf28iwrkqyjzzj"))

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

(define rust-nalgebra-0.32.6
  (crate-source "nalgebra" "0.32.6"
                "1r033ciacblmkif5njlhprfp0k59spjv54cqsyggb1is0bg1fp3v"))

(define rust-nalgebra-macros-0.2.2
  (crate-source "nalgebra-macros" "0.2.2"
                "1z6v9phhr1hwzyyblf792128lxfv1hy1sxl4cvikihcgmxr56ji5"))

(define rust-new-debug-unreachable-1.0.6
  (crate-source "new_debug_unreachable" "1.0.6"
                "11phpf1mjxq6khk91yzcbd3ympm78m3ivl7xg6lg2c0lf66fy3k5"))

(define rust-nom-7.1.3
  (crate-source "nom" "7.1.3"
                "0jha9901wxam390jcf5pfa0qqfrgh8li787jx2ip0yk5b8y9hwyj"))

(define rust-normalize-line-endings-0.3.0
  (crate-source "normalize-line-endings" "0.3.0"
                "1gp52dfn2glz26a352zra8h04351icf0fkqzw1shkwrgh1vpz031"))

(define rust-nu-ansi-term-0.46.0
  (crate-source "nu-ansi-term" "0.46.0"
                "115sywxh53p190lyw97alm14nc004qj5jm5lvdj608z84rbida3p"))

(define rust-num-complex-0.4.6
  (crate-source "num-complex" "0.4.6"
                "15cla16mnw12xzf5g041nxbjjm9m85hdgadd5dl5d0b30w9qmy3k"))

(define rust-num-conv-0.1.0
  (crate-source "num-conv" "0.1.0"
                "1ndiyg82q73783jq18isi71a7mjh56wxrk52rlvyx0mi5z9ibmai"))

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

(define rust-once-cell-1.21.3
  (crate-source "once_cell" "1.21.3"
                "0b9x77lb9f1j6nqgf5aka4s2qj0nly176bpbrv6f9iakk5ff3xa2"))

(define rust-oorandom-11.1.5
  (crate-source "oorandom" "11.1.5"
                "07mlf13z453fq01qff38big1lh83j8l6aaglf63ksqzzqxc0yyfn"))

(define rust-openssl-probe-0.1.6
  (crate-source "openssl-probe" "0.1.6"
                "0bl52x55laalqb707k009h8kfawliwp992rlsvkzy49n47p2fpnh"))

(define rust-os-str-bytes-6.6.1
  (crate-source "os_str_bytes" "6.6.1"
                "1885z1x4sm86v5p41ggrl49m58rbzzhd1kj72x46yy53p62msdg2"))

(define rust-overload-0.1.1
  (crate-source "overload" "0.1.1"
                "0fdgbaqwknillagy1xq7xfgv60qdbk010diwl7s1p0qx7hb16n5i"))

(define rust-owo-colors-3.5.0
  (crate-source "owo-colors" "3.5.0"
                "0vyvry6ba1xmpd45hpi6savd8mbx09jpmvnnwkf6z62pk6s4zc61"))

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

(define rust-paste-1.0.15
  (crate-source "paste" "1.0.15"
                "02pxffpdqkapy292harq6asfjvadgp1s005fip9ljfsn9fvxgh2p"))

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

(define rust-pkg-config-0.3.32
  (crate-source "pkg-config" "0.3.32"
                "0k4h3gnzs94sjb2ix6jyksacs52cf1fanpwsmlhjnwrdnp8dppby"))

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

(define rust-proc-macro-crate-3.3.0
  (crate-source "proc-macro-crate" "3.3.0"
                "0d9xlymplfi9yv3f5g4bp0d6qh70apnihvqcjllampx4f5lmikpd"))

(define rust-proc-macro2-1.0.94
  (crate-source "proc-macro2" "1.0.94"
                "114wxb56gdj9vy44q0ll3l2x9niqzcbyqikydmlb5f3h5rsp26d3"))

(define rust-prodash-29.0.1
  (crate-source "prodash" "29.0.1"
                "12xm50jzkqzdqdcidmzy4d6rj9r8x6mf8sidgrh7dfc0r4jcxrwy"))

(define rust-proptest-1.6.0
  (crate-source "proptest" "1.6.0"
                "0l4y4bb8hffv7cys7d59qwqdmvmqjfzz0x9vblc08209clqfkjhl"))

(define rust-qoi-0.4.1
  (crate-source "qoi" "0.4.1"
                "00c0wkb112annn2wl72ixyd78mf56p4lxkhlmsggx65l3v3n8vbz"))

(define rust-quick-error-1.2.3
  (crate-source "quick-error" "1.2.3"
                "1q6za3v78hsspisc197bg3g7rpc989qycy8ypr8ap8igv10ikl51"))

(define rust-quick-error-2.0.1
  (crate-source "quick-error" "2.0.1"
                "18z6r2rcjvvf8cn92xjhm2qc3jpd1ljvcbf12zv0k9p565gmb4x9"))

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

(define rust-reqwest-0.12.15
  (crate-source "reqwest" "0.12.15"
                "1fvvrl3jmsnlm99ldl0ariklrlsmrky06qabp7dc92ylznk4d76i"))

(define rust-rgb-0.8.50
  (crate-source "rgb" "0.8.50"
                "02ii3nsciska0sj23ggxaz8gj64ksw8nbpfjcwxlh037chb7sfap"))

(define rust-ring-0.17.14 rust-ring-0.17)

(define rust-rustc-demangle-0.1.24
  (crate-source "rustc-demangle" "0.1.24"
                "07zysaafgrkzy2rjgwqdj2a8qdpsm6zv6f5pgpk9x0lm40z9b6vi"))

(define rust-rustc-hash-2.1.1
  (crate-source "rustc-hash" "2.1.1"
                "03gz5lvd9ghcwsal022cgkq67dmimcgdjghfb5yb5d352ga06xrm"))

(define rust-rustc-stable-hash-0.1.2
  (crate-source "rustc-stable-hash" "0.1.2"
                "026drx2ly2b8b1pp1c2v3p3ws6k0jaa5bbc5f4xwkibhj7r4453q"))

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
                "0755isc0x5iymm3wsn59s0ad1pm9zidw7p34qfqlsjsac9jf4z4i"))

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

(define rust-schannel-0.1.27
  (crate-source "schannel" "0.1.27"
                "0gbbhy28v72kd5iina0z2vcdl3vz63mk5idvkzn5r52z6jmfna8z"
                #:snippet '(delete-file-recursively "test")))

(define rust-scopeguard-1.2.0
  (crate-source "scopeguard" "1.2.0"
                "0jcz9sd47zlsgcnm1hdw0664krxwb5gczlif4qngj2aif8vky54l"))

(define rust-secrecy-0.10.3
  (crate-source "secrecy" "0.10.3"
                "0nmfsf9qm8921v2jliz08bj8zrryqar4gj3d6irqfc3kaj2az4g8"))

(define rust-security-framework-3.2.0
  (crate-source "security-framework" "3.2.0"
                "05mkrddi9i18h9p098d0iimqv1xxz0wd8mbgpbvh9jj67x0205r7"))

(define rust-security-framework-sys-2.14.0
  (crate-source "security-framework-sys" "2.14.0"
                "0chwn01qrnvs59i5220bymd38iddy4krbnmfnhf4k451aqfj7ns9"))

(define rust-selectors-0.25.0
  (crate-source "selectors" "0.25.0"
                "01kvl1r7plzlb665r64p11djabhsrd88si2zh7vci3v3ydshbcsf"))

(define rust-semver-1.0.26
  (crate-source "semver" "1.0.26"
                "1l5q2vb8fjkby657kdyfpvv40x2i2xqq9bg57pxqakfj92fgmrjn"))

(define rust-serde-1.0.219
  (crate-source "serde" "1.0.219"
                "1dl6nyxnsi82a197sd752128a4avm6mxnscywas1jq30srp2q3jz"))

(define rust-serde-derive-1.0.219
  (crate-source "serde_derive" "1.0.219"
                "001azhjmj7ya52pmfiw4ppxm16nd44y15j2pf5gkcwrcgz7pc0jv"))

(define rust-serde-json-1.0.140
  (crate-source "serde_json" "1.0.140"
                "0wwkp4vc20r87081ihj3vpyz5qf7wqkqipq17v99nv6wjrp8n1i0"))

(define rust-serde-spanned-0.6.8
  (crate-source "serde_spanned" "0.6.8"
                "1q89g70azwi4ybilz5jb8prfpa575165lmrffd49vmcf76qpqq47"))

(define rust-serde-urlencoded-0.7.1
  (crate-source "serde_urlencoded" "0.7.1"
                "1zgklbdaysj3230xivihs30qi5vkhigg323a9m62k8jwf4a1qjfk"))

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

(define rust-sha1-smol-1.0.1
  (crate-source "sha1_smol" "1.0.1"
                "0pbh2xjfnzgblws3hims0ib5bphv7r5rfdpizyh51vnzvnribymv"))

(define rust-sharded-slab-0.1.7
  (crate-source "sharded-slab" "0.1.7"
                "1xipjr4nqsgw34k7a2cgj9zaasl2ds6jwn89886kww93d32a637l"))

(define rust-shell-words-1.1.0
  (crate-source "shell-words" "1.1.0"
                "1plgwx8r0h5ismbbp6cp03740wmzgzhip85k5hxqrrkaddkql614"))

(define rust-shlex-1.3.0
  (crate-source "shlex" "1.3.0"
                "0r1y6bv26c1scpxvhg2cabimrmwgbp4p3wy6syj9n0c4s3q2znhg"))

(define rust-simba-0.8.1
  (crate-source "simba" "0.8.1"
                "1bnf7ainywmaz2z67ss1q0bjwccf80c50c50r6hlpay69z4hf586"))

(define rust-simd-adler32-0.3.7
  (crate-source "simd-adler32" "0.3.7"
                "1zkq40c3iajcnr5936gjp9jjh1lpzhy44p3dq3fiw75iwr1w2vfn"))

(define rust-siphasher-0.3.11
  (crate-source "siphasher" "0.3.11"
                "03axamhmwsrmh0psdw3gf7c0zc4fyl5yjxfifz9qfka6yhkqid9q"))

(define rust-siphasher-1.0.1
  (crate-source "siphasher" "1.0.1"
                "17f35782ma3fn6sh21c027kjmd227xyrx06ffi8gw4xzv9yry6an"))

(define rust-slab-0.4.9
  (crate-source "slab" "0.4.9"
                "0rxvsgir0qw5lkycrqgb1cxsvxzjv9bmx73bk5y42svnzfba94lg"))

(define rust-smallvec-1.15.0
  (crate-source "smallvec" "1.15.0"
                "1sgfw8z729nlxk8k13dhs0a762wnaxmlx70a7xlf3wz989bjh5w9"))

(define rust-smol-str-0.3.2
  (crate-source "smol_str" "0.3.2"
                "039mj6lc1vkljj17ndlzzkak8kvlmw8ppi6yjdxsh433snfbhxln"))

(define rust-socket2-0.5.9
  (crate-source "socket2" "0.5.9"
                "1vzds1wwwi0a51fn10r98j7cx3ir4shvhykpbk7md2h5h1ydapsg"))

(define rust-spdx-0.10.8
  (crate-source "spdx" "0.10.8"
                "14r1bl3gmx7cj91l5r1qr5wildjacmzflw9cahgzrqk7v9b97djq"))

(define rust-spin-0.9.8
  (crate-source "spin" "0.9.8"
                "0rvam5r0p3a6qhc18scqpvpgb3ckzyqxpgdfyjnghh8ja7byi039"))

(define rust-stable-deref-trait-1.2.0
  (crate-source "stable_deref_trait" "1.2.0"
                "1lxjr8q2n534b2lhkxd6l6wcddzjvnksi58zv11f9y0jjmr15wd8"))

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

(define rust-target-lexicon-0.12.16
  (crate-source "target-lexicon" "0.12.16"
                "1cg3bnx1gdkdr5hac1hzxy64fhw4g7dqkd0n3dxy5lfngpr1mi31"))

(define rust-tempfile-3.19.1
  (crate-source "tempfile" "3.19.1"
                "1grmcj8y6rcavndw2dm18ndzdimsq5f8lcrwyg627cdrcdvsqdvl"))

(define rust-tendril-0.4.3
  (crate-source "tendril" "0.4.3"
                "1c3vip59sqwxn148i714nmkrvjzbk7105vj0h92s6r64bw614jnj"))

(define rust-termcolor-1.4.1
  (crate-source "termcolor" "1.4.1"
                "0mappjh3fj3p2nmrg4y7qv94rchwi9mzmgmfflr8p2awdj7lyy86"))

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

(define rust-tracing-core-0.1.33
  (crate-source "tracing-core" "0.1.33"
                "170gc7cxyjx824r9kr17zc9gvzx89ypqfdzq259pr56gg5bwjwp6"))

(define rust-tracing-log-0.2.0
  (crate-source "tracing-log" "0.2.0"
                "1hs77z026k730ij1a9dhahzrl0s073gfa2hm5p0fbl0b80gmz1gf"))

(define rust-tracing-subscriber-0.3.19
  (crate-source "tracing-subscriber" "0.3.19"
                "0220rignck8072i89jjsh140vmh14ydwpdwnifyaf3xcnpn9s678"))

(define rust-try-lock-0.2.5
  (crate-source "try-lock" "0.2.5"
                "0jqijrrvm1pyq34zn1jmy2vihd4jcrjlvsh4alkjahhssjnsn8g4"))

(define rust-twox-hash-2.1.0
  (crate-source "twox-hash" "2.1.0"
                "022rwrv24rl6g32nqv1mywf6vdnkn7vq34fg793vll1hgccpzcg7"))

(define rust-typenum-1.18.0
  (crate-source "typenum" "1.18.0"
                "0gwgz8n91pv40gabrr1lzji0b0hsmg0817njpy397bq7rvizzk0x"))

(define rust-uluru-3.1.0
  (crate-source "uluru" "3.1.0"
                "1njp6vvy1mm8idnsp6ljyxx5znfsk3xkmk9cr2am0vkfwmlj92kw"))

(define rust-unarray-0.1.4
  (crate-source "unarray" "0.1.4"
                "154smf048k84prsdgh09nkm2n0w0336v84jd4zikyn6v6jrqbspa"))

(define rust-unicode-bom-2.0.3
  (crate-source "unicode-bom" "2.0.3"
                "05s2sqyjanqrbds3fxam35f92npp5ci2wz9zg7v690r0448mvv3y"))

(define rust-unicode-ident-1.0.18
  (crate-source "unicode-ident" "1.0.18"
                "04k5r6sijkafzljykdq26mhjpmhdx4jwzvn1lh90g9ax9903jpss"
                #:snippet '(delete-file-recursively "tests")))

(define rust-unicode-normalization-0.1.24
  (crate-source "unicode-normalization" "0.1.24"
                "0mnrk809z3ix1wspcqy97ld5wxdb31f3xz6nsvg5qcv289ycjcsh"))

(define rust-unicode-width-0.1.14
  (crate-source "unicode-width" "0.1.14"
                "1bzn2zv0gp8xxbxbhifw778a7fc93pa6a1kj24jgg9msj07f7mkx"))

(define rust-unicode-width-0.2.0
  (crate-source "unicode-width" "0.2.0"
                "1zd0r5vs52ifxn25rs06gxrgz8cmh4xpra922k0xlmrchib1kj0z"))

(define rust-unicode-xid-0.2.6
  (crate-source "unicode-xid" "0.2.6"
                "0lzqaky89fq0bcrh6jj6bhlz37scfd8c7dsj5dq7y32if56c1hgb"))

(define rust-untrusted-0.9.0
  (crate-source "untrusted" "0.9.0"
                "1ha7ib98vkc538x0z60gfn0fc5whqdd85mb87dvisdcaifi6vjwf"
                #:snippet '(delete-file-recursively "mk")))

(define rust-url-2.5.4
  (crate-source "url" "2.5.4"
                "0q6sgznyy2n4l5lm16zahkisvc9nip9aa5q1pps7656xra3bdy1j"))

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

(define rust-valuable-0.1.1
  (crate-source "valuable" "0.1.1"
                "0r9srp55v7g27s5bg7a2m095fzckrcdca5maih6dy9bay6fflwxs"))

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

(define rust-weezl-0.1.8
  (crate-source "weezl" "0.1.8"
                "10lhndjgs6y5djpg3b420xngcr6jkmv70q8rb1qcicbily35pa2k"))

(define rust-wide-0.7.32
  (crate-source "wide" "0.7.32"
                "08mb6iqdscqiqrbfkjrnfr876ah4cc0cx5pjilz3yqw1k9mmgda1"))

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

(define rust-windows-aarch64-gnullvm-0.52.6
  (crate-source "windows_aarch64_gnullvm" "0.52.6"
                "1lrcq38cr2arvmz19v32qaggvj8bh1640mdm9c2fr877h0hn591j"
                #:snippet '(delete-file-recursively "lib")))

(define rust-windows-aarch64-gnullvm-0.53.0
  (crate-source "windows_aarch64_gnullvm" "0.53.0"
                "0r77pbpbcf8bq4yfwpz2hpq3vns8m0yacpvs2i5cn6fx1pwxbf46"
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

(define rust-windows-sys-0.52.0
  (crate-source "windows-sys" "0.52.0"
                "0gd3v4ji88490zgb6b5mq5zgbvwv7zx1ibn8v3x83rwcdbryaar8"))

(define rust-windows-sys-0.59.0
  (crate-source "windows-sys" "0.59.0"
                "0fw5672ziw8b3zpmnbp9pdv1famk74f1l9fcbc3zsrzdg56vqf0y"))

(define rust-windows-targets-0.52.6
  (crate-source "windows-targets" "0.52.6"
                "0wwrx625nwlfp7k93r2rra568gad1mwd888h1jwnl0vfg5r4ywlv"))

(define rust-windows-targets-0.53.0
  (crate-source "windows-targets" "0.53.0"
                "12yakpjizhfpppz1i3zgcwxlbar8axrp9j87fmywpydarvlcgr5i"))

(define rust-windows-x86-64-gnu-0.52.6
  (crate-source "windows_x86_64_gnu" "0.52.6"
                "0y0sifqcb56a56mvn7xjgs8g43p33mfqkd8wj1yhrgxzma05qyhl"
                #:snippet '(delete-file-recursively "lib")))

(define rust-windows-x86-64-gnu-0.53.0
  (crate-source "windows_x86_64_gnu" "0.53.0"
                "1flh84xkssn1n6m1riddipydcksp2pdl45vdf70jygx3ksnbam9f"
                #:snippet '(delete-file-recursively "lib")))

(define rust-windows-x86-64-gnullvm-0.52.6
  (crate-source "windows_x86_64_gnullvm" "0.52.6"
                "03gda7zjx1qh8k9nnlgb7m3w3s1xkysg55hkd1wjch8pqhyv5m94"
                #:snippet '(delete-file-recursively "lib")))

(define rust-windows-x86-64-gnullvm-0.53.0
  (crate-source "windows_x86_64_gnullvm" "0.53.0"
                "0mvc8119xpbi3q2m6mrjcdzl6afx4wffacp13v76g4jrs1fh6vha"
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

(define rust-xml5ever-0.17.0
  (crate-source "xml5ever" "0.17.0"
                "0l76v0c228c92sskiflpsy19c0bgc8q7flhlfanm32zrbb8f2d20"))

(define rust-yansi-1.0.1
  (crate-source "yansi" "1.0.1"
                "0jdh55jyv0dpd38ij4qh60zglbw9aa8wafqai6m0wa7xaxk3mrfg"
                #:snippet '(delete-file-recursively ".github")))

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

(define rust-zune-inflate-0.2.54
  (crate-source "zune-inflate" "0.2.54"
                "00kg24jh3zqa3i6rg6yksnb71bch9yi1casqydl00s7nw8pk7avk"))

(define ssss-separator 'end-of-crates)


;;;
;;; Cargo inputs.
;;;

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
