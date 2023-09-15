;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015-2023 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016, 2020-2023 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016, 2017, 2020 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2016 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2018, 2019, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2018 Mark Meyer <mark@ofosos.org>
;;; Copyright © 2018 Ben Woodcroft <donttrustben@gmail.com>
;;; Copyright © 2018 Fis Trivial <ybbs.daans@hotmail.com>
;;; Copyright © 2018 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2018 Björn Höfling <bjoern.hoefling@bjoernhoefling.de>
;;; Copyright © 2019 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2019, 2020 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2019 Brett Gilio <brettg@gnu.org>
;;; Copyright © 2020 Konrad Hinsen <konrad.hinsen@fastmail.net>
;;; Copyright © 2020 Edouard Klein <edk@beaver-labs.com>
;;; Copyright © 2020, 2021, 2022, 2023 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2020, 2021, 2022, 2023 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2022, 2023 Nicolas Graves <ngraves@ngraves.fr>
;;; Copyright © 2023 zamfofex <zamfofex@twdb.moe>
;;; Copyright © 2023 Navid Afkhami <navid.afkhami@mdc-berlin.de>
;;; Copyright © 2023 Zheng Junjie <873216071@qq.com>
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

(define-module (gnu packages machine-learning)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix svn-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system ocaml)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (guix build-system r)
  #:use-module (guix build-system trivial)
  #:use-module (guix git-download)
  #:use-module (gnu packages)
  #:use-module (gnu packages adns)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages bdw-gc)
  #:use-module (gnu packages c)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages cran)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages dejagnu)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages image)
  #:use-module (gnu packages image-processing)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages jupyter)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages ninja)
  #:use-module (gnu packages ocaml)
  #:use-module (gnu packages onc-rpc)
  #:use-module (gnu packages opencl)
  #:use-module (gnu packages parallel)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages regex)
  #:use-module (gnu packages rpc)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages video)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (ice-9 match))

(define-public fasttext
  (package
    (name "fasttext")
    (version "0.9.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/facebookresearch/fastText")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "07cz2ghfq6amcljaxpdr5chbd64ph513y8zqmibfx2xwfp74xkhn"))))
    (build-system cmake-build-system)
    ;; Tests require downloading of test data.
    (arguments (list #:tests? #false))
    (home-page "https://github.com/facebookresearch/fastText")
    (synopsis "Library for fast text representation and classification")
    (description "fastText is a library for efficient learning of word
representations and sentence classification.")
    (license license:expat)))

(define-public python-fasttext
  (package
    (inherit fasttext)
    (name "python-fasttext")
    (build-system pyproject-build-system)
    (propagated-inputs (list python-numpy python-scipy))
    (inputs (list fasttext))
    (native-inputs (list pybind11))))

(define-public fann
  ;; The last release is >100 commits behind, so we package from git.
  (let ((commit "d71d54788bee56ba4cf7522801270152da5209d7"))
    (package
      (name "fann")
      (version (string-append "2.2.0-1." (string-take commit 8)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/libfann/fann")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "0ibwpfrjs6q2lijs8slxjgzb2llcl6rk3v2ski4r6215g5jjhg3x"))))
      (build-system cmake-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (replace 'check
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out")))
                 (with-directory-excursion (string-append (getcwd) "/tests")
                   (invoke "./fann_tests"))))))))
      (home-page "http://leenissen.dk/fann/wp/")
      (synopsis "Fast Artificial Neural Network")
      (description
       "FANN is a neural network library, which implements multilayer
artificial neural networks in C with support for both fully connected and
sparsely connected networks.")
      (license license:lgpl2.1))))

(define-public libsvm
  (package
    (name "libsvm")
    (version "3.23")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.csie.ntu.edu.tw/~cjlin/libsvm/"
                           name "-" version ".tar.gz"))
       (sha256
        (base32 "0jpaq0rr92x38p4nk3gjan79ip67m6p80anb28z1d8601miysyi5"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no "check" target
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after 'build 'build-lib
           (lambda _
             (invoke "make" "lib")))
         (replace 'install              ; no ‘install’ target
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin/"))
                    (lib (string-append out "/lib/"))
                    (inc (string-append out "/include/libsvm")))
               (mkdir-p bin)
               (for-each (lambda (file)
                           (copy-file file (string-append bin file)))
                         '("svm-train"
                           "svm-predict"
                           "svm-scale"))
               (mkdir-p lib)
               (install-file "libsvm.so.2" lib)
               (mkdir-p inc)
               (install-file "svm.h" inc)))))))
    (home-page "https://www.csie.ntu.edu.tw/~cjlin/libsvm/")
    (synopsis "Library for Support Vector Machines")
    (description
     "LIBSVM is a machine learning library for support vector
classification, (C-SVC, nu-SVC), regression (epsilon-SVR, nu-SVR) and
distribution estimation (one-class SVM).  It supports multi-class
classification.")
    (license license:bsd-3)))

(define-public python-libsvm
  (package (inherit libsvm)
    (name "python-libsvm")
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no "check" target
       #:make-flags '("-C" "python")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace
          'install                      ; no ‘install’ target
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (let ((site (string-append (assoc-ref outputs "out")
                                       "/lib/python"
                                       (string-take
                                        (string-take-right
                                         (assoc-ref inputs "python") 5) 3)
                                       "/site-packages/")))
              (substitute* "python/svm.py"
                (("../libsvm.so.2") "libsvm.so.2"))
              (mkdir-p site)
              (for-each (lambda (file)
                          (copy-file file (string-append site (basename file))))
                        (find-files "python" "\\.py"))
              (copy-file "libsvm.so.2"
                         (string-append site "libsvm.so.2")))
            #t)))))
    (inputs
     (list python))
    (synopsis "Python bindings of libSVM")))

(define-public ghmm
  ;; The latest release candidate is several years and a couple of fixes have
  ;; been published since.  This is why we download the sources from the SVN
  ;; repository.
  (let ((svn-revision 2341))
    (package
      (name "ghmm")
      (version (string-append "0.9-rc3-0." (number->string svn-revision)))
      (source (origin
                (method svn-fetch)
                (uri (svn-reference
                      (url "http://svn.code.sf.net/p/ghmm/code/trunk")
                      (revision svn-revision)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "07kdsngvr4n1qxpqzv1nlay7g41d6jzjppa8vzmrg220s8ing87z"))))
      (build-system gnu-build-system)
      (arguments
       `(#:imported-modules (,@%gnu-build-system-modules
                             (guix build python-build-system))
         #:modules          ((guix build python-build-system)
                             ,@%gnu-build-system-modules)
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'enter-dir
             (lambda _ (chdir "ghmm")))
           (add-after 'enter-dir 'fix-runpath
             (lambda* (#:key outputs #:allow-other-keys)
               (substitute* "ghmmwrapper/setup.py"
                 (("^(.*)extra_compile_args = \\[" line indent)
                  (string-append indent
                                 "extra_link_args = [\"-Wl,-rpath="
                                 (assoc-ref outputs "out") "/lib\"],\n"
                                 line
                                 "\"-Wl,-rpath="
                                 (assoc-ref outputs "out")
                                 "/lib\", ")))))
           (add-after 'enter-dir 'disable-broken-tests
             (lambda _
               (substitute* "tests/Makefile.am"
                 ;; GHMM_SILENT_TESTS is assumed to be a command.
                 (("TESTS_ENVIRONMENT.*") "")
                 ;; Do not build broken tests.
                 (("chmm .*") "")
                 (("read_fa .*") "")
                 (("mcmc .*") "")
                 (("label_higher_order_test.*$")
                  "label_higher_order_test\n"))

               ;; These Python unittests are broken as there is no gato.
               ;; See https://sourceforge.net/p/ghmm/support-requests/3/
               (substitute* "ghmmwrapper/ghmmunittests.py"
                 (("^(.*)def (testNewXML|testMultipleTransitionClasses|testNewXML)"
                   line indent)
                  (string-append indent
                                 "@unittest.skip(\"Disabled by Guix\")\n"
                                 line))))))))
      (inputs
       `(("python" ,python-2) ; only Python 2 is supported
         ("libxml2" ,libxml2)))
      (native-inputs
       (list pkg-config
             dejagnu
             swig
             autoconf
             automake
             libtool))
      (home-page "http://ghmm.org")
      (synopsis "Hidden Markov Model library")
      (description
       "The General Hidden Markov Model library (GHMM) is a C library with
additional Python bindings implementing a wide range of types of @dfn{Hidden
Markov Models} (HMM) and algorithms: discrete, continuous emissions, basic
training, HMM clustering, HMM mixtures.")
      (license license:lgpl2.0+))))

(define-public guile-aiscm
  (package
    (name "guile-aiscm")
    (version "0.25.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/wedesoft/aiscm")
                    (commit "v0.25.2")))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1sagpxwrqxkn5b9zqzd07c9r7swmw45q672pa8fy6s71iw6a0x77"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:make-flags
      #~(list (string-append "GUILE_CACHE=" #$output "/lib/guile/3.0/site-ccache")
              (string-append "GUILE_EXT=" #$output "/lib/guile/3.0/extensions")
              (string-append "GUILE_SITE=" #$output "/share/guile/site/3.0"))
      #:phases
      '(modify-phases %standard-phases
         (add-after 'unpack 'build-reproducibly
           (lambda _
             (substitute* "doc/Makefile.am"
               (("\\$\\(DATE\\)") "1970-01-01"))))
         (add-after 'unpack 'find-clearsilver
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "configure.ac"
               (("/usr/local/include/ClearSilver")
                (string-append (assoc-ref inputs "clearsilver")
                               "/include/ClearSilver")))
             (substitute* "aiscm/Makefile.am"
               (("-lneo_utl" m)
                (string-append m " -lstreamhtmlparser")))
             (setenv "C_INCLUDE_PATH"
                     (string-append (assoc-ref inputs "clearsilver")
                                    "/include/ClearSilver:"
                                    (or (getenv "C_INCLUDE_PATH") "")))))
         (add-after 'unpack 'use-llvm-config
           (lambda _
             (substitute* "m4/ax_llvmc.m4"
               (("llvm-config-11") "llvm-config")
               ;; For some reason this library is not on the link list.
               (("(LLVM_LIBS=\"\\$\\(\\$ac_llvm_config_path --libs \\$1\\))\"" _ m)
                (string-append m " -lLLVMMCJIT\"")))

             ;; Because of this message:
             ;; symbol lookup error: ./.libs/libguile-aiscm-core.so: undefined symbol: LLVMInitializeX86TargetInfo
             ;; This probably needs to differ when building on architectures
             ;; other than x86_64.
             (substitute* "aiscm/Makefile.am"
               (("LLVM_LIBS\\)") "LLVM_LIBS) \
-lLLVMX86AsmParser -lLLVMX86CodeGen -lLLVMX86Desc -lLLVMX86Info"))))
         ;; This test fails because our version of tensorflow is too old
         ;; to provide tf-string-length.
         (add-after 'unpack 'disable-broken-test
           (lambda _
             (substitute* "tests/test_tensorflow.scm"
               (("\\(test-eqv \"determine string length" m)
                (string-append "#;" m)))))
         ;; Use Clang instead of GCC.
         (add-before 'configure 'prepare-build-environment
           (lambda _
             (setenv "AR" "llvm-ar")
             (setenv "NM" "llvm-nm")
             (setenv "CC" "clang")
             (setenv "CXX" "clang++"))))))
    (inputs
     (list clearsilver
           ffmpeg-4
           freeglut
           guile-3.0
           imagemagick
           libgc
           libjpeg-turbo
           libomp
           libxi
           libxmu
           libxpm
           libxt
           libxv
           mesa
           mjpegtools
           pandoc
           pulseaudio
           tensorflow))
    (native-inputs
     (list clang-13
           llvm-13
           pkg-config
           protobuf-c-for-aiscm
           autoconf
           automake
           gettext-minimal
           libtool
           which))
    (home-page "https://wedesoft.github.io/aiscm/")
    (synopsis "Guile extension for numerical arrays and tensors")
    (description "AIscm is a Guile extension for numerical arrays and tensors.
Performance is achieved by using the LLVM JIT compiler.")
    (license license:gpl3+)))

(define-public guile-aiscm-next
  (deprecated-package "guile-aiscm-next" guile-aiscm))

(define-public llama-cpp
  (let ((commit "f31b5397143009d682db90fd2a6cde83f1ef00eb")
        (revision "0"))
    (package
      (name "llama-cpp")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/ggerganov/llama.cpp")
               (commit (string-append "master-" (string-take commit 7)))))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0ys6n53n032zq1ll9f3vgxk8sw0qq7x3fi7awsyy13adzp3hn08p"))))
      (build-system cmake-build-system)
      (arguments
       (list
        #:modules '((ice-9 textual-ports)
                    (guix build utils)
                    ((guix build python-build-system) #:prefix python:)
                    (guix build cmake-build-system))
        #:imported-modules `(,@%cmake-build-system-modules
                             (guix build python-build-system))
        #:phases
        #~(modify-phases %standard-phases
            (add-before 'install 'install-python-scripts
              (lambda _
                (let ((bin (string-append #$output "/bin/")))
                  (define (make-script script)
                    (let ((suffix (if (string-suffix? ".py" script) "" ".py")))
                      (call-with-input-file
                          (string-append "../source/" script suffix)
                        (lambda (input)
                          (call-with-output-file (string-append bin script)
                            (lambda (output)
                              (format output "#!~a/bin/python3\n~a"
                                      #$(this-package-input "python")
                                      (get-string-all input))))))
                      (chmod (string-append bin script) #o555)))
                  (mkdir-p bin)
                  (make-script "convert-pth-to-ggml")
                  (make-script "convert-lora-to-ggml")
                  (make-script "convert"))))
            (add-after 'install-python-scripts 'wrap-python-scripts
              (assoc-ref python:%standard-phases 'wrap))
            (replace 'install
              (lambda _
                (copy-file "bin/main" (string-append #$output "/bin/llama")))))))
      (inputs (list python))
      (propagated-inputs
       (list python-numpy python-pytorch python-sentencepiece))
      (home-page "https://github.com/ggerganov/llama.cpp")
      (synopsis "Port of Facebook's LLaMA model in C/C++")
      (description "This package provides a port to Facebook's LLaMA collection
of foundation language models.  It requires models parameters to be downloaded
independently to be able to run a LLaMA model.")
      (license license:expat))))

(define-public mcl
  (package
    (name "mcl")
    (version "14.137")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://micans.org/mcl/src/mcl-"
                    (string-replace-substring version "." "-")
                    ".tar.gz"))
              (sha256
               (base32
                "15xlax3z31lsn62vlg94hkm75nm40q4679amnfg13jm8m2bnhy5m"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags (list "--enable-blast"
                               "CFLAGS=-fcommon")))
    (inputs
     (list perl))
    (home-page "https://micans.org/mcl/")
    (synopsis "Clustering algorithm for graphs")
    (description
     "The MCL algorithm is short for the @dfn{Markov Cluster Algorithm}, a
fast and scalable unsupervised cluster algorithm for graphs (also known as
networks) based on simulation of (stochastic) flow in graphs.")
    ;; In the LICENCE file and web page it says "The software is licensed
    ;; under the GNU General Public License, version 3.", but in several of
    ;; the source code files it suggests GPL3 or later.
    ;; http://listserver.ebi.ac.uk/pipermail/mcl-users/2016/000376.html
    (license license:gpl3)))

(define-public ocaml-mcl
  (package
    (name "ocaml-mcl")
    (version "12-068oasis4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/fhcrc/mcl")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0009dc3h2jp3qg5val452wngpqnbfyhbcxylghq0mrjqxx0jdq5p"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'patch-paths
           (lambda _
             (substitute* "setup.ml"
               (("LDFLAGS=-fPIC")
                (string-append "LDFLAGS=-fPIC\"; \"SHELL=" (which "sh")))
               (("-std=c89") "-std=gnu99 -fcommon")

               ;; This is a mutable string, which is no longer supported.  Use
               ;; a byte buffer instead.
               (("String.make \\(String.length s\\)")
                "Bytes.make (String.length s)")

               ;; These two belong together.
               (("OASISString.replace_chars")
                "Bytes.to_string (OASISString.replace_chars")
               ((" s;")
                " s);"))
             (substitute* "myocamlbuild.ml"
               (("std=c89") "std=gnu99 -fcommon"))
             ;; Since we build with a more recent OCaml, we have to use C99 or
             ;; later.  This causes problems with the old C code.
             (substitute* "src/impala/matrix.c"
               (("restrict") "restrict_"))
             #t)))))
    (native-inputs
     (list ocamlbuild))
    (home-page "https://github.com/fhcrc/mcl")
    (synopsis "OCaml wrappers around MCL")
    (description
     "This package provides OCaml bindings for the MCL graph clustering
algorithm.")
    (license license:gpl3)))

(define-public randomjungle
  (package
    (name "randomjungle")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://www.imbs.uni-luebeck.de/fileadmin/files/Software"
             "/randomjungle/randomjungle-" version ".tar_.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (patches (search-patches "randomjungle-disable-static-build.patch"))
       (sha256
        (base32
         "12c8rf30cla71swx2mf4ww9mfd8jbdw5lnxd7dxhyw1ygrvg6y4w"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list "--disable-static"
             (string-append "--with-boost="
                            (assoc-ref %build-inputs "boost")))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-compatibility-errors
           (lambda _
             (substitute* "src/library/IAM2WayImportance.h"
               (("= std::make_pair.*")
                "= std::minmax(varID1, varID2);"))
             (substitute* "src/library/DataFrame.h"
               (("isFirst\\?.*")
                "if (isFirst) { isFirst = false; } else { os << par.delimiter; }\n"))))
         (add-before 'configure 'set-CXXFLAGS
           (lambda _ (setenv "CXXFLAGS" "-fpermissive "))))))
    (inputs
     (list boost gsl libxml2 zlib))
    (native-inputs
     (list gfortran-7 (list gfortran-7 "lib")))
    ;; Non-portable assembly instructions are used so building fails on
    ;; platforms other than x86_64 or i686.
    (supported-systems '("x86_64-linux" "i686-linux"))
    (home-page "https://www.imbs.uni-luebeck.de/forschung/software/details.html#c224")
    (synopsis "Implementation of the Random Forests machine learning method")
    (description
     "Random Jungle is an implementation of Random Forests.  It is supposed to
analyse high dimensional data.  In genetics, it can be used for analysing big
Genome Wide Association (GWA) data.  Random Forests is a powerful machine
learning method.  Most interesting features are variable selection, missing
value imputation, classifier creation, generalization error estimation and
sample proximities between pairs of cases.")
    (license license:gpl3+)))

(define-public r-rcppml/devel
  (let ((commit "e685b3bd7909d3ae74c98f85f81bc0bb679bce23")
        (revision "1"))
    (package
      (name "r-rcppml-devel")
      (version (git-version "0.5.6" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/zdebruine/RcppML")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "18ykh9s9h3x79az7qm3pg48iqm0nmkh2wkppc9wx0lq7kjfqm67a"))))
      (properties `((upstream-name . "RcppML")))
      (build-system r-build-system)
      (propagated-inputs (list r-matrix r-rcpp))
      (native-inputs (list r-knitr))
      (home-page "https://github.com/zdebruine/RcppML")
      (synopsis "Rcpp machine learning Library")
      (description
       "This package provides fast machine learning algorithms including
matrix factorization and divisive clustering for large sparse and dense
matrices.")
      (license license:gpl3+))))

(define-public openfst
  (package
    (name "openfst")
    (version "1.8.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.openfst.org/twiki/pub/FST/"
                                  "FstDownload/openfst-" version ".tar.gz"))
              (sha256
               (base32
                "0hlbdmjjf1jgsvi3d2hwni5lz3l9a5bzj6ijpbawa8a7cbrpp66y"))))
    (build-system gnu-build-system)
    (arguments '(#:configure-flags '("--enable-ngram-fsts")))
    (home-page "https://www.openfst.org")
    (synopsis "Library for weighted finite-state transducers")
    (description "OpenFst is a library for constructing, combining,
optimizing, and searching weighted finite-state transducers (FSTs).")
    (license license:asl2.0)))

;; This is a temporary addition to bypass upstream issues with the kaldi
;; package.
(define-public openfst-1.7.3
  (package (inherit openfst)
    (version "1.7.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.openfst.org/twiki/pub/FST/"
                                  "FstDownload/openfst-" version ".tar.gz"))
              (sha256
               (base32
                "038a60w7y8qnbxmcrsim9rafz9mihsny8xv50jpzlr7rl166pp5q"))))
    (arguments '(#:configure-flags '("--enable-ngram-fsts" "CXXFLAGS=-std=c++14")
                 #:make-flags '("CXXFLAGS=-std=c++14")))))

(define openfst-for-vosk
  (package
    (inherit openfst)
    (version "1.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://www.openfst.org/twiki/pub/FST/"
                           "FstDownload/openfst-" version ".tar.gz"))
       (sha256
        (base32 "0h2lfhhihg63b804hrcljnkggijbjmp84i5g8q735wb09y9z2c4p"))))
    (arguments
     '(#:configure-flags
       '("--enable-shared" "--enable-far" "--enable-ngram-fsts"
         "--enable-lookahead-fsts" "--with-pic" "--disable-bin")))))

(define-public sentencepiece
  (package
    (name "sentencepiece")
    (version "0.1.97")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/google/sentencepiece")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1kzfkp2pk0vabyw3wmkh16h11chzq63mzc20ddhsag5fp6s91ajg"))))
    (build-system cmake-build-system)
    (arguments (list #:tests? #f))      ;no tests
    (native-inputs (list gperftools))
    (home-page "https://github.com/google/sentencepiece")
    (synopsis "Unsupervised tokenizer for Neural Network-based text generation")
    (description
     "SentencePiece is an unsupervised text tokenizer and detokenizer mainly
for Neural Network-based text generation systems where the vocabulary size is
predetermined prior to the neural model training.  SentencePiece implements
subword units---e.g., byte-pair-encoding (BPE) and unigram language
model---with the extension of direct training from raw sentences.
SentencePiece allows us to make a purely end-to-end system that does not
depend on language-specific pre- or post-processing.")
    (license license:asl2.0)))

(define-public python-sacrebleu
  (package
    (name "python-sacrebleu")
    (version "2.3.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/mjpost/sacrebleu")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1al4qf9wsq5l453qqb6clims62ns0s07wb9rfbf4hbpr1f2iv7zv"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; These all need internet access.
      '(list "-k" "not test_api_get_source \
and not test_api_get_reference \
and not test_maybe_download \
and not test_process_to_text \
and not test_get_files_and_fieldnames \
and not test_source_and_references \
and not test_wmt22_references")
      #:phases
      '(modify-phases %standard-phases
         ;; Needed for tests.
         (add-before 'check 'set-HOME
           (lambda _ (setenv "HOME" "/tmp"))))))
    (propagated-inputs (list python-colorama
                             python-lxml
                             python-numpy
                             python-portalocker
                             python-regex
                             python-tabulate))
    (native-inputs (list python-pytest))
    (home-page "https://github.com/mjpost/sacrebleu")
    (synopsis
     "Compute shareable, comparable, and reproducible BLEU, chrF, and TER scores")
    (description
     "This is a package for hassle-free computation of shareable, comparable,
and reproducible BLEU, chrF, and TER scores for natural language processing.")
    (license license:asl2.0)))

(define-public python-sentencepiece
  (package
    (name "python-sentencepiece")
    (version "0.1.97")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sentencepiece" version))
       (sha256
        (base32 "0v0z9ryl66432zajp099bcbnwkkldzlpjvgnjv9bq2vi19g300f9"))))
    (build-system python-build-system)
    (native-inputs (list pkg-config))
    (propagated-inputs (list sentencepiece))
    (home-page "https://github.com/google/sentencepiece")
    (synopsis "SentencePiece python wrapper")
    (description "This package provides a Python wrapper for the SentencePiece
unsupervised text tokenizer.")
    (license license:asl2.0)))

(define-public python-spacy-legacy
  (package
    (name "python-spacy-legacy")
    (version "3.0.12")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "spacy-legacy" version))
              (sha256
               (base32
                "0x57aw1qgjrzgapsv1cwymqlck2anqm1bisvryhpq7bfkc66wzdk"))))
    (build-system pyproject-build-system)
    ;; This package depends on spacy, which depends on this package.
    (arguments (list #:tests? #false))
    (native-inputs (list python-pytest))
    (home-page "https://spacy.io")
    (synopsis "Legacy registered functions for spaCy backwards compatibility")
    (description
     "This package contains legacy registered functions for spaCy backwards
compatibility.")
    (license license:expat)))

(define-public python-spacy-loggers
  (package
    (name "python-spacy-loggers")
    (version "1.0.4")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "spacy-loggers" version))
              (sha256
               (base32
                "0a5zjfxd0mci7pwda24ihg55whalpmjby4bvpgar2013f6zq7yg6"))))
    (build-system pyproject-build-system)
    ;; This package depends on spacy, which depends on this package.
    (arguments (list #:tests? #false))
    (native-inputs (list python-pytest))
    (home-page "https://github.com/explosion/spacy-loggers")
    (synopsis "Logging utilities for SpaCy")
    (description "This package provides logging utilities for the SpaCy
natural language processing framework.")
    (license license:expat)))

(define-public python-spacy
  (package
    (name "python-spacy")
    (version "3.5.3")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "spacy" version))
              (sha256
               (base32
                "13141hc966d8nxbnlwj01vhndgq0rq4nmii3qkb3hrap45kiv5rm"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      '(list "-k"
             (string-append
              ;; We don't do that around here.
              "not test_download_compatibility"
              ;; This needs to download a model.
              " and not test_validate_compatibility_table"
              ;; This tries to run the application with typer, which fails
              ;; with an unspecified error, possibly because the build
              ;; container doesn't have /bin/sh.
              " and not test_project_assets"))
      #:phases
      '(modify-phases %standard-phases
         (add-after 'build 'build-ext
           (lambda _
             (invoke "python" "setup.py" "build_ext" "--inplace"
                     "-j" (number->string (parallel-job-count))))))))
    (propagated-inputs (list python-catalogue
                             python-cymem
                             python-jinja2
                             python-langcodes
                             python-murmurhash
                             python-numpy
                             python-packaging
                             python-pathy
                             python-preshed
                             python-pydantic
                             python-requests
                             python-setuptools
                             python-smart-open
                             python-spacy-legacy
                             python-spacy-loggers
                             python-srsly
                             python-thinc
                             python-tqdm
                             python-typer
                             python-typing-extensions
                             python-wasabi))
    (native-inputs
     (list python-cython python-pytest python-mock))
    (home-page "https://spacy.io")
    (synopsis "Natural Language Processing (NLP) in Python")
    (description
     "SpaCy is a library for advanced Natural Language Processing in Python
and Cython.  It comes with pretrained pipelines and currently supports
tokenization and training for 70+ languages. It features state-of-the-art
speed and neural network models for tagging, parsing, named entity
recognition, text classification and more, multi-task learning with pretrained
transformers like BERT, as well as a production-ready training system and easy
model packaging, deployment and workflow management.")
    (license license:expat)))

(define-public shogun
  (package
    (name "shogun")
    (version "6.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "ftp://shogun-toolbox.org/shogun/releases/"
             (version-major+minor version)
             "/sources/shogun-" version ".tar.bz2"))
       (sha256
        (base32
         "1rn9skm3nw6hr7mr3lgp2gfqhi7ii0lyxck7qmqnf8avq349s5jp"))
       (modules '((guix build utils)
                  (ice-9 rdelim)))
       (snippet
        '(begin
           ;; Remove non-free sources and files referencing them
           (for-each delete-file
                     (find-files "src/shogun/classifier/svm/"
                                 "SVMLight\\.(cpp|h)"))
           (for-each delete-file
                     (find-files "examples/undocumented/libshogun/"
                                 (string-append
                                  "(classifier_.*svmlight.*|"
                                  "evaluation_cross_validation_locked_comparison).cpp")))
           ;; Remove non-free functions.
           (define (delete-ifdefs file)
             (with-atomic-file-replacement file
               (lambda (in out)
                 (let loop ((line (read-line in 'concat))
                            (skipping? #f))
                   (if (eof-object? line)
                       #t
                       (let ((skip-next?
                              (or (and skipping?
                                       (not (string-prefix?
                                             "#endif //USE_SVMLIGHT" line)))
                                  (string-prefix?
                                   "#ifdef USE_SVMLIGHT" line))))
                         (when (or (not skipping?)
                                   (and skipping? (not skip-next?)))
                           (display line out))
                         (loop (read-line in 'concat) skip-next?)))))))
           (for-each delete-ifdefs
                     (append
                      (find-files "src/shogun/classifier/mkl"
                                  "^MKLClassification\\.cpp")
                      (find-files "src/shogun/classifier/svm"
                                  "^SVMLightOneClass\\.(cpp|h)")
                      (find-files "src/shogun/multiclass"
                                  "^ScatterSVM\\.(cpp|h)")
                      (find-files "src/shogun/kernel/"
                                  "^(Kernel|CombinedKernel|ProductKernel)\\.(cpp|h)")
                      (find-files "src/shogun/regression/svr"
                                  "^(MKLRegression|SVRLight)\\.(cpp|h)")
                      (find-files "src/shogun/transfer/domain_adaptation"
                                  "^DomainAdaptationSVM\\.(cpp|h)")))
           #t))))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #f ;no check target
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'delete-broken-symlinks
           (lambda _
             (for-each delete-file '("applications/arts/data"
                                     "applications/asp/data"
                                     "applications/easysvm/data"
                                     "applications/msplicer/data"
                                     "applications/ocr/data"
                                     "examples/meta/data"
                                     "examples/undocumented/data"))
             #t))
         (add-after 'unpack 'change-R-target-path
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* '("src/interfaces/r/CMakeLists.txt"
                            "examples/meta/r/CMakeLists.txt")
               (("\\$\\{R_COMPONENT_LIB_PATH\\}")
                (string-append (assoc-ref outputs "out")
                               "/lib/R/library/")))
             #t))
         (add-after 'unpack 'fix-octave-modules
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "src/interfaces/octave/CMakeLists.txt"
               (("^include_directories\\(\\$\\{OCTAVE_INCLUDE_DIRS\\}")
                "include_directories(${OCTAVE_INCLUDE_DIRS} ${OCTAVE_INCLUDE_DIRS}/octave")
               ;; change target directory
               (("\\$\\{OCTAVE_OCT_LOCAL_API_FILE_DIR\\}")
                (string-append (assoc-ref outputs "out")
                               "/share/octave/packages")))
             (substitute* '("src/interfaces/octave/swig_typemaps.i"
                            "src/interfaces/octave/sg_print_functions.cpp")
               ;; "octave/config.h" and "octave/oct-obj.h" deprecated in Octave.
               (("octave/config\\.h") "octave/octave-config.h")
               (("octave/oct-obj.h") "octave/ovl.h"))
             #t))
         (add-after 'unpack 'move-rxcpp
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((rxcpp-dir "shogun/third-party/rxcpp"))
               (mkdir-p rxcpp-dir)
               (install-file (assoc-ref inputs "rxcpp") rxcpp-dir)
               #t)))
         (add-before 'build 'set-HOME
           ;; $HOME needs to be set at some point during the build phase
           (lambda _ (setenv "HOME" "/tmp") #t)))
       #:configure-flags
       (list "-DCMAKE_BUILD_WITH_INSTALL_RPATH=TRUE"
             "-DUSE_SVMLIGHT=OFF" ;disable proprietary SVMLIGHT
             "-DBUILD_META_EXAMPLES=OFF" ;requires unpackaged ctags
             ;;"-DINTERFACE_JAVA=ON" ;requires unpackaged jblas
             ;;"-DINTERFACE_RUBY=ON" ;requires unpackaged ruby-narray
             ;;"-DINTERFACE_PERL=ON" ;"FindPerlLibs" does not exist
             ;;"-DINTERFACE_LUA=ON"  ;fails because lua doesn't build pkgconfig file
             "-DINTERFACE_OCTAVE=ON"
             "-DINTERFACE_PYTHON=ON"
             "-DINTERFACE_R=ON")))
    (inputs
     `(("python" ,python)
       ("numpy" ,python-numpy)
       ("r-minimal" ,r-minimal)
       ("octave" ,octave-cli)
       ("swig" ,swig)
       ("eigen" ,eigen)
       ("hdf5" ,hdf5)
       ("atlas" ,atlas)
       ("arpack" ,arpack-ng)
       ("lapack" ,lapack)
       ("glpk" ,glpk)
       ("libxml2" ,libxml2)
       ("lzo" ,lzo)
       ("zlib" ,zlib)))
    (native-inputs
     (list pkg-config rxcpp))
    ;; Non-portable SSE instructions are used so building fails on platforms
    ;; other than x86_64.
    (supported-systems '("x86_64-linux"))
    (home-page "https://shogun-toolbox.org/")
    (synopsis "Machine learning toolbox")
    (description
     "The Shogun Machine learning toolbox provides a wide range of unified and
efficient Machine Learning (ML) methods.  The toolbox seamlessly
combines multiple data representations, algorithm classes, and general purpose
tools.  This enables both rapid prototyping of data pipelines and extensibility
in terms of new algorithms.")
    (license license:gpl3+)))

(define-public onnx
  (package
    (name "onnx")
    (version "1.12.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/onnx/onnx")
                    (commit (string-append "v" version))))
              (sha256
               (base32
                "1g9f1hviksbn7gi6fnd0dsm7nf0w3yia0mjj33d9mggklrl0db6x"))
              (file-name (git-file-name name version))
              (patches (search-patches "onnx-use-system-googletest.patch"
                                       "onnx-shared-libraries.patch"
                                       "onnx-skip-model-downloads.patch"))
              (modules '((guix build utils)))
              (snippet '(delete-file-recursively "third_party"))))
    (build-system python-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (add-before 'build 'pass-cmake-arguments
                    (lambda* (#:key outputs #:allow-other-keys)
                      ;; Pass options to the CMake-based build process.
                      (define out
                        (assoc-ref outputs "out"))

                      (define args
                        ;; Copy arguments from 'cmake-build-system', plus ask
                        ;; for shared libraries.
                        (list "-DCMAKE_BUILD_TYPE=RelWithDebInfo"
                              (string-append "-DCMAKE_INSTALL_PREFIX=" out)
                              "-DCMAKE_INSTALL_LIBDIR=lib"
                              "-DCMAKE_INSTALL_RPATH_USE_LINK_PATH=TRUE"
                              (string-append "-DCMAKE_INSTALL_RPATH=" out
                                             "/lib")
                              "-DCMAKE_VERBOSE_MAKEFILE=ON"

                              "-DBUILD_SHARED_LIBS=ON"))

                      ;; This environment variable is honored by 'setup.py',
                      ;; which passes it down to 'cmake'.
                      (setenv "CMAKE_ARGS" (string-join args))

                      ;; This one is honored by 'setup.py' and passed to 'make
                      ;; -j'.
                      (setenv "MAX_JOBS"
                              (number->string (parallel-job-count)))))
                  (add-before 'check 'make-test-directory-writable
                    (lambda _
                      ;; Make things writable for tests.
                      (setenv "HOME" (getcwd))
                      (for-each make-file-writable
                                (find-files "onnx/examples" "."
                                            #:directories? #t))))
                  (add-after 'install 'install-from-cmake
                    (lambda _
                      ;; Run "make install" in the build tree 'setup.py'
                      ;; created for CMake so that libonnx.so,
                      ;; libonnx_proto.so, etc. are installed.
                      (invoke "make" "install"
                              "-C" ".setuptools-cmake-build"))))))
    (native-inputs
     (list cmake
           googletest
           pybind11
           python-coverage
           python-nbval
           python-pytest
           python-pytest-runner))
    (inputs
     (list protobuf))
    (propagated-inputs
     (list python-numpy python-protobuf python-six python-tabulate
           python-typing-extensions))
    (home-page "https://onnx.ai/")
    (synopsis "Open Neural Network Exchange")
    (description
     "@acronym{ONNX, Open Neural Network Exchange} is a format for AI models,
both deep learning and traditional @acronym{ML, machine learning}.  It defines
an extensible computation graph model, as well as definitions of built-in
operators and standard data types.")
    (license license:expat)))

(define-public python-onnx
  ;; This used to be called "python-onnx" because it provided nothing but
  ;; Python bindings.  The package now provides shared libraries and C++
  ;; headers, hence the name change.
  (deprecated-package "python-onnx" onnx))

(define-public onnx-optimizer
  (package
    (name "onnx-optimizer")
    ;; Note: 0.2.x is *more* recent than 1.5.0.
    (version "0.2.6")
    (home-page "https://github.com/onnx/optimizer")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (sha256
               (base32
                "1wkqqdxcxpfbf8zpbdfdd3zz5jkw775g31gyykj11z4y6pp659l6"))
              (file-name (git-file-name name version))
              (patches (search-patches "onnx-optimizer-system-library.patch"))
              (modules '((guix build utils)))
              (snippet '(delete-file-recursively "third_party"))))
    (build-system python-build-system)
    (arguments (package-arguments onnx))          ;reuse build system tweaks
    (native-inputs
     (list cmake python-pytest python-pytest-runner python-nbval
           python-coverage))
    (inputs
     (list onnx protobuf pybind11))
    (propagated-inputs
     (list python-numpy))
    (synopsis "Library to optimize ONNX models")
    (description
     "This package provides a C++ and Python library for performing arbitrary
optimizations on ONNX models, as well as a growing list of prepackaged
optimization passes.

Not all possible optimizations can be directly implemented on ONNX graphs---
some will need additional backend-specific information---but many can, and the
aim is to provide all such passes along with ONNX so that they can be re-used
with a single function call.")
    (license license:expat)))

(define-public rxcpp
  (package
    (name "rxcpp")
    (version "4.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ReactiveX/RxCpp")
             (commit (string-append "v" version))))
       (sha256
        (base32 "1blyjjw6szd74pckdc15ham9i48xf0vwwz5nhl9vyjfq8z7w3piy"))
       (file-name (git-file-name name version))))
    (build-system cmake-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-werror
           (lambda _
             (substitute* (find-files ".")
               (("-Werror") ""))
             #t))
         (replace 'check
           (lambda _
             (invoke "ctest"))))))
    (native-inputs
     (list catch-framework))
    (home-page "https://reactivex.io/")
    (synopsis "Reactive Extensions for C++")
    (description
     "The Reactive Extensions for C++ (RxCpp) is a library of algorithms for
values-distributed-in-time.  ReactiveX is a library for composing asynchronous
and event-based programs by using observable sequences.

It extends the observer pattern to support sequences of data and/or events and
adds operators that allow you to compose sequences together declaratively while
abstracting away concerns about things like low-level threading,
synchronization, thread-safety, concurrent data structures, and non-blocking
I/O.")
    (license license:asl2.0)))


(define-public gemmlowp
  (let ((commit "08e4bb339e34017a0835269d4a37c4ea04d15a69")
        (version "0.1")
        (revision "1"))
    (package
      (name "gemmlowp")
      (version (git-version version revision commit))
      (home-page "https://github.com/google/gemmlowp")
      (source (origin
                (method git-fetch)
                (uri (git-reference (url home-page) (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1q8f3w5slxd8fbn31hpm00y6wyp7gm71rzr27cmcff4b3px4ca6k"))))
      (arguments
       `(#:configure-flags
         (list ,@(match (%current-system)
                   ((or "x86_64-linux" "i686-linux")
                    '("-DCMAKE_CXX_FLAGS=-msse2"))
                   (_ '()))
               "-DBUILD_SHARED_LIBS=ON")
         #:phases
         (modify-phases %standard-phases
           ;; This directory contains the CMakeLists.txt.
           (add-after 'unpack 'chdir
             (lambda _ (chdir "contrib") #t))
           ;; There is no install target
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (lib (string-append out "/lib/"))
                      (inc (string-append out "/include/")))
                 (install-file "../build/libeight_bit_int_gemm.so" lib)
                 (for-each (lambda (dir)
                             (let ((target
                                    (string-append inc "/gemmlowp/" dir)))
                               (for-each (lambda (h)
                                           (install-file h target))
                                         (find-files (string-append "../" dir)
                                                     "\\.h$"))))
                           '("meta" "profiling" "public" "fixedpoint"
                             "eight_bit_int_gemm" "internal"))))))))
      (build-system cmake-build-system)
      (synopsis "Small self-contained low-precision GEMM library")
      (description
       "This is a small self-contained low-precision @dfn{general matrix
multiplication} (GEMM) library.  It is not a full linear algebra library.
Low-precision means that the input and output matrix entries are integers on
at most 8 bits.  To avoid overflow, results are internally accumulated on more
than 8 bits, and at the end only some significant 8 bits are kept.")
      (license license:asl2.0))))

(define-public gemmlowp-for-tensorflow
  ;; The commit hash is taken from "tensorflow/workspace.bzl".
  (let ((commit "38ebac7b059e84692f53e5938f97a9943c120d98")
        (revision "2"))
    (package
      (inherit gemmlowp)
      (version (git-version "0" revision commit))
      (source (origin
                (method url-fetch)
                (uri (string-append "https://mirror.bazel.build/"
                                    "github.com/google/gemmlowp/archive/"
                                    commit ".zip"))
                (file-name (string-append "gemmlowp-" version ".zip"))
                (sha256
                 (base32
                  "0n56s2g8hrssm4w8qj1v58gfm56a04n9v992ixkmvk6zjiralzxq"))))
      (arguments
       (substitute-keyword-arguments (package-arguments gemmlowp)
         ((#:phases phases)
          `(modify-phases ,phases
             (replace 'install
               (lambda* (#:key outputs #:allow-other-keys)
                 (let* ((out (assoc-ref outputs "out"))
                        (lib (string-append out "/lib/"))
                        (inc (string-append out "/include/")))
                   (install-file "../build/libeight_bit_int_gemm.so" lib)
                   (for-each (lambda (dir)
                               ;; Note: Install headers straight into
                               ;; $includedir instead of $includedir/gemmlowp.
                               (let ((target (string-append inc "/" dir)))
                                 (for-each (lambda (h)
                                             (install-file h target))
                                           (find-files (string-append "../" dir)
                                                       "\\.h$"))))
                             '("meta" "profiling" "public" "fixedpoint"
                               "eight_bit_int_gemm" "internal")))))))))
      (native-inputs
       (list unzip))
      (properties '((hidden? . #t))))))

(define-public dlib
  (package
    (name "dlib")
    (version "19.20")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://dlib.net/files/dlib-" version ".tar.bz2"))
              (sha256
               (base32
                "139jyi19qz37wwmmy48gil9d1kkh2r3w3bwdzabha6ayxmba96nz"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Delete ~13MB of bundled dependencies.
                  (delete-file-recursively "dlib/external")
                  (delete-file-recursively "docs/dlib/external")
                  #t))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags '("-DBUILD_SHARED_LIBS=ON")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'disable-asserts
           (lambda _
             ;; config.h recommends explicitly enabling or disabling asserts
             ;; when building as a shared library. By default neither is set.
             (substitute* "dlib/config.h"
               (("^//#define DLIB_DISABLE_ASSERTS") "#define DLIB_DISABLE_ASSERTS"))
             #t))
         (add-after 'disable-asserts 'disable-failing-tests
           (lambda _
             ;; One test times out on MIPS, so we need to disable it.
             ;; Others are flaky on some platforms.
             (let* ((system ,(or (%current-target-system)
                                 (%current-system)))
                    (disabled-tests (cond
                                     ((string-prefix? "mips64" system)
                                      '("object_detector" ; timeout
                                        "data_io"))
                                     ((string-prefix? "armhf" system)
                                      '("learning_to_track"))
                                     ((string-prefix? "i686" system)
                                      '("optimization"))
                                     (else '()))))
               (for-each
                (lambda (test)
                  (substitute* "dlib/test/makefile"
                    (((string-append "SRC \\+= " test "\\.cpp")) "")))
                disabled-tests)
               #t)))
         (replace 'check
           (lambda _
             ;; No test target, so we build and run the unit tests here.
             (let ((test-dir (string-append "../dlib-" ,version "/dlib/test")))
               (with-directory-excursion test-dir
                 (invoke "make" "-j" (number->string (parallel-job-count)))
                 (invoke "./dtest" "--runall"))
               #t))))))
    (native-inputs
     (list pkg-config
           ;; For tests.
           libnsl))
    (inputs
     `(("giflib" ,giflib)
       ("lapack" ,lapack)
       ("libjpeg" ,libjpeg-turbo)
       ("libpng" ,libpng)
       ("libx11" ,libx11)
       ("openblas" ,openblas)
       ("zlib" ,zlib)))
    (synopsis
     "Toolkit for making machine learning and data analysis applications in C++")
    (description
     "Dlib is a modern C++ toolkit containing machine learning algorithms and
tools.  It is used in both industry and academia in a wide range of domains
including robotics, embedded devices, mobile phones, and large high performance
computing environments.")
    (home-page "http://dlib.net")
    (license license:boost1.0)))

(define-public python-scikit-learn
  (package
    (name "python-scikit-learn")
    (version "1.2.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/scikit-learn/scikit-learn")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0x7gfzvcdadf8jnvpz8m373bi37bc6sndfbjh9lzmn3p39pwm2hl"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'configure
           (lambda _
             (setenv "SKLEARN_BUILD_PARALLEL"
                     (number->string (parallel-job-count)))))
         (add-after 'build 'build-ext
           (lambda _ (invoke "python" "setup.py" "build_ext" "--inplace"
                             "-j" (number->string (parallel-job-count)))))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               ;; Restrict OpenBLAS threads to prevent segfaults while testing!
               (setenv "OPENBLAS_NUM_THREADS" "1")

               ;; Some tests require write access to $HOME.
               (setenv "HOME" "/tmp")

               ;; Step out of the source directory to avoid interference;
               ;; we want to run the installed code with extensions etc.
               (with-directory-excursion "/tmp"
                 (invoke "pytest" "-vv" "--pyargs" "sklearn"
                         "-m" "not network"
                         "-n" (number->string (parallel-job-count))
                         ;; This test tries to access the internet.
                         "-k" "not test_load_boston_alternative"))))))))
    (inputs (list openblas))
    (native-inputs
     (list python-cython
           python-pandas
           python-pytest
           python-pytest-xdist))
    (propagated-inputs
     (list python-numpy python-threadpoolctl python-scipy python-joblib))
    (home-page "https://scikit-learn.org/")
    (synopsis "Machine Learning in Python")
    (description
     "Scikit-learn provides simple and efficient tools for data mining and
data analysis.")
    (license license:bsd-3)))

(define-public python-thinc
  (package
    (name "python-thinc")
    (version "8.1.10")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "thinc" version))
              (sha256
               (base32
                "14drmwa2sh8fqszv1fm2jl4lky1j5yrbkjv89bl49q07vbblhjkc"))))
    (build-system pyproject-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'build 'build-ext
           (lambda _
             (invoke "python" "setup.py" "build_ext" "--inplace"
                     "-j" (number->string (parallel-job-count))))))))
    (propagated-inputs (list python-blis-for-thinc
                             python-catalogue
                             python-confection
                             python-contextvars
                             python-cymem
                             python-dataclasses
                             python-murmurhash
                             python-numpy
                             python-packaging
                             python-preshed
                             python-pydantic
                             python-srsly
                             python-typing-extensions
                             python-wasabi))
    (native-inputs (list python-cython python-mock python-pytest))
    (home-page "https://github.com/explosion/thinc")
    (synopsis "Functional take on deep learning")
    (description
     "This package provides a functional take on deep learning, compatible
with your favorite libraries.")
    (license license:expat)))

(define-public python-threadpoolctl
  (package
    (name "python-threadpoolctl")
    (version "3.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "threadpoolctl" version))
        (sha256
         (base32
          "100k76nmajf408lbn5ipis1gilklcs6sbqyqy3hhlh54zanbldd3"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-flit-core python-pytest))
    (home-page "https://github.com/joblib/threadpoolctl")
    (synopsis "Python helpers for common threading libraries")
    (description "Thread-pool Controls provides Python helpers to limit the
number of threads used in the threadpool-backed of common native libraries used
for scientific computing and data science (e.g. BLAS and OpenMP).")
    (license license:bsd-3)))

(define-public python-tslearn
  (package
    (name "python-tslearn")
    (version "0.6.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/tslearn-team/tslearn")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1fhs8c28hdqsyj8kdhzrmrxrh4w92x6nf3gm026xapp9divvljd6"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      '(list "-k"
             (string-append
              ;; This one fails because of a difference in accuracy.
              "not test_all_estimators[LearningShapelets-LearningShapelets]"
              ;; XXX: It's embarrassing to disable these two, but the truth is
              ;; that there's only so much we can do to force this package to
              ;; work with Tensorflow 1.9.  It's still worth having this
              ;; package, because it can be used without the Tensorflow
              ;; backend.
              ;; TypeError: cannot pickle '_thread.RLock' object
              " and not test_shapelets"
              ;; TypeError: Expected binary or unicode string, got 2
              " and not test_serialize_shapelets"))
      #:phases
      '(modify-phases %standard-phases
         (add-after 'unpack 'compatibility
           (lambda _
             (substitute* "tslearn/tests/sklearn_patches.py"
               (("_pairwise_estimator_convert_X")
                "_enforce_estimator_tags_X")
               (("pairwise_estimator_convert_X\\(([^,]+), ([^,\\)]+)" _ a b)
                (string-append "pairwise_estimator_convert_X(" b ", " a)))
             (substitute* "tslearn/tests/test_shapelets.py"
               (("tf.optimizers.Adam")
                "tf.keras.optimizers.Adam"))
             (substitute* "tslearn/shapelets/shapelets.py"
               (("tf.keras.utils.set_random_seed")
                "tf.set_random_seed")
               (("def __call__\\(self, shape, dtype=None\\):")
                "def __call__(self, shape, dtype=None, partition_info=None):")
               (("tf.math.is_finite")
                "tf.is_finite")))))))
    (propagated-inputs (list python-cesium
                             python-h5py
                             python-joblib
                             python-numba
                             python-numpy
                             python-pandas
                             python-scipy
                             python-scikit-learn
                             tensorflow
                             python-wheel))
    (native-inputs (list python-pytest))
    (home-page "https://github.com/tslearn-team/tslearn")
    (synopsis "Machine learning toolkit for time series data")
    (description "This is a Python library for time series data mining.
It provides tools for time series classification, clustering
and forecasting.")
    (license license:bsd-2)))

(define-public python-imbalanced-learn
  (package
    (name "python-imbalanced-learn")
    (version "0.9.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "imbalanced-learn" version))
              (sha256
               (base32
                "0qnrmysnqpc8ii1w5n8mci20gcjhmjr7khvk7f2apdbqc2pgf52f"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      '(modify-phases %standard-phases
         (add-after 'unpack 'unbreak-tests
           (lambda _
             ;; The doctests require tensorflow
             (substitute* "setup.cfg"
               (("--doctest-modules") ""))
             ;; Some tests require a home directory
             (setenv "HOME" (getcwd))
             ;; We don't have keras
             (delete-file "imblearn/keras/tests/test_generator.py")
             ;; We don't have tensorflow
             (delete-file "imblearn/tensorflow/tests/test_generator.py"))))))
    (propagated-inputs
     (list python-joblib
           python-numpy
           python-scikit-learn
           python-scipy
           python-threadpoolctl))
    (native-inputs
     (list python-black
           python-flake8
           python-mypy
           python-pandas
           python-pytest
           python-pytest-cov))
    (home-page "https://github.com/scikit-learn-contrib/imbalanced-learn")
    (synopsis "Toolbox for imbalanced dataset in machine learning")
    (description "This is a Python package offering a number of re-sampling
techniques commonly used in datasets showing strong between-class imbalance.
It is compatible with @code{scikit-learn}.")
    (license license:expat)))

(define-public python-pynndescent
  (package
    (name "python-pynndescent")
    (version "0.5.10")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pynndescent" version))
       (sha256
        (base32 "1bc8aa6jfw28y6sb0nvfdrfgh66a42bqb4znvpimzx9yq21wcpax"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key inputs outputs tests? #:allow-other-keys)
             (when tests?
               (invoke "python" "-m" "pytest" "--pyargs" "pynndescent"
                       ;; wminkowski no longer exists in scipy 1.8.0 (see:
                       ;; https://github.com/lmcinnes/pynndescent/issues/177)
                       "-k" "not test_weighted_minkowski")))))))
    (native-inputs (list python-pytest))
    (propagated-inputs
     (list python-joblib
           python-llvmlite
           python-numba
           python-scikit-learn
           python-scipy))
    (home-page "https://github.com/lmcinnes/pynndescent")
    (synopsis "Nearest neighbor descent for approximate nearest neighbors")
    (description
     "PyNNDescent provides a Python implementation of Nearest Neighbor Descent
for k-neighbor-graph construction and approximate nearest neighbor search.")
    (license license:bsd-2)))

(define-public python-opentsne
  (package
    (name "python-opentsne")
    (version "0.7.1")
    (source
     (origin
       (method git-fetch) ; no tests in PyPI release
       (uri (git-reference
             (url "https://github.com/pavlin-policar/openTSNE")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "12wp98mh67v6v683yi7wbv8zhpafrfz21z349bww4wgi2q7bl3il"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; Benchmarks require the 'macosko2015' data files.
         (add-after 'unpack 'delete-benchmark
           (lambda _ (delete-file-recursively "benchmarks")))
         (add-after 'unpack 'skip-test
           (lambda _ ;; TODO: figure out why this test fails.
             (substitute* "tests/test_correctness.py"
               (("def test_iris\\(self\\)") "def _test_iris(self)"))))
         ;; Numba needs a writable dir to cache functions.
         (add-before 'check 'set-numba-cache-dir
           (lambda _ (setenv "NUMBA_CACHE_DIR" "/tmp"))))))
    (native-inputs (list python-cython))
    (inputs (list fftw))
    (propagated-inputs
     (list python-numpy python-pynndescent python-scikit-learn
           python-scipy))
    (home-page "https://github.com/pavlin-policar/openTSNE")
    (synopsis "Extensible, parallel implementations of t-SNE")
    (description
     "This is a modular Python implementation of t-Distributed Stochastic
Neighbor Embedding (t-SNE), a popular dimensionality-reduction algorithm for
visualizing high-dimensional data sets.")
    (license license:bsd-3)))

(define-public python-scikit-rebate
  (package
    (name "python-scikit-rebate")
    (version "0.62")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "skrebate" version))
              (sha256
               (base32
                "0n55ghvnv7rxqa5agq6a4892ad0ghha165b0g4ghwr9gqm6ss3dj"))))
    (build-system python-build-system)
    (arguments '(#:tests? #f))          ;no tests on PyPI and no tags in repo
    (propagated-inputs
     (list python-numpy python-scipy python-scikit-learn python-joblib))
    (home-page "https://epistasislab.github.io/scikit-rebate/")
    (synopsis "Relief-based feature selection algorithms for Python")
    (description "Scikit-rebate is a scikit-learn-compatible Python
implementation of ReBATE, a suite of Relief-based feature selection algorithms
for Machine Learning.  These algorithms excel at identifying features that are
predictive of the outcome in supervised learning problems, and are especially
good at identifying feature interactions that are normally overlooked by
standard feature selection algorithms.")
    (license license:expat)))

(define-public python-cleanlab
  (package
    (name "python-cleanlab")
    (version "2.2.0")
    ;; The version on pypi does not come with tests.
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/cleanlab/cleanlab")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "00dqhxpwg781skknw943ynll2s44g4j125dx8aapk1d5d71sbzqy"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      '(modify-phases %standard-phases
         (add-after 'unpack 'disable-bad-tests
           (lambda _
             ;; XXX This requires pytest lazy_fixture
             (delete-file "tests/test_multilabel_classification.py")
             ;; Requires tensorflow
             (delete-file "tests/test_frameworks.py")
             ;; Tries to download datasets from the internet at runtime.
             (delete-file "tests/test_dataset.py"))))))
    (propagated-inputs
     (list python-numpy
           python-pandas
           python-scikit-learn
           python-termcolor
           python-tqdm))
    (native-inputs
     (list python-pytest
           python-pytorch
           python-torchvision))
    (home-page "https://cleanlab.ai")
    (synopsis "Automatically find and fix dataset issues")
    (description
     "cleanlab automatically finds and fixes errors in any ML dataset. This
data-centric AI package facilitates machine learning with messy, real-world
data by providing clean labels during training.")
    (license license:agpl3+)))

(define-public python-cleanlab-1
  (package
    (inherit python-cleanlab)
    (name "python-cleanlab")
    (version "1.0.1")
    ;; The version on pypi does not come with tests.
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/cleanlab/cleanlab")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "03kw2agnhadmrq9zvrlvvlc2c37dpflga5nhmsaag8scw223gqyp"))))
    (build-system pyproject-build-system)
    (arguments (list))
    (propagated-inputs
     (list python-numpy
           python-scikit-learn
           python-scipy
           python-tqdm))
    (native-inputs
     (list python-pytest))))

(define-public python-cma
  (package
    (name "python-cma")
    (version "3.3.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "cma" version))
              (sha256
               (base32
                "1v31b2vnnr4v6ack7zfmw7zb47vbzjr9nyvx2lbfhyjf7zhbhj5p"))))
    (build-system python-build-system)
    (arguments
     (list #:phases #~(modify-phases %standard-phases
                        (replace 'check
                          (lambda* (#:key tests? #:allow-other-keys)
                            (when tests?
                              (invoke "python" "-m" "cma.test")))))))
    (propagated-inputs (list python-numpy))
    (home-page "https://github.com/CMA-ES/pycma")
    (synopsis "Python implementation of CMA-ES")
    (description "This package provides a Python implementation of the
@acronym{CMA-ES, Covariance Matrix Adaptation Evolution Strategy} algorithm
and a few related numerical optimization tools.")
    (license license:bsd-3)))

(define-public python-cmaes
  (package
    (name "python-cmaes")
    (version "0.9.1")
    (source
     (origin
       (method git-fetch) ;no tests in PyPI
       (uri (git-reference
             (url "https://github.com/CyberAgentAILab/cmaes")
             (commit (string-append "v" version))))
       (sha256
        (base32 "1f3143w8ii6i93bdh65iazrq1lryccd805ndnqww5l8h7qnnzpkm"))
       (file-name (git-file-name name version))))
    (build-system pyproject-build-system)
    (native-inputs (list python-hypothesis))
    (propagated-inputs (list python-numpy))
    (home-page "https://github.com/CyberAgentAILab/cmaes")
    (synopsis "CMA-ES implementation for Python")
    (description "This package provides provides an implementation of the
Covariance Matrix Adaptation Evolution Strategy (CMA-ES) for Python.")
    (license license:expat)))

(define-public python-autograd
  (let* ((commit "442205dfefe407beffb33550846434baa90c4de7")
         (revision "0")
         (version (git-version "0.0.0" revision commit)))
    (package
      (name "python-autograd")
      (home-page "https://github.com/HIPS/autograd")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url home-page)
                      (commit commit)))
                (sha256
                 (base32
                  "189sv2xb0mwnjawa9z7mrgdglc1miaq93pnck26r28fi1jdwg0z4"))
                (file-name (git-file-name name version))))
      (version version)
      (build-system python-build-system)
      (native-inputs
       (list python-nose python-pytest))
      (propagated-inputs
       (list python-future python-numpy))
      (arguments
       `(#:phases (modify-phases %standard-phases
                    (replace 'check
                      (lambda _
                        (invoke "py.test" "-v"))))))
      (synopsis "Efficiently computes derivatives of NumPy code")
      (description "Autograd can automatically differentiate native Python and
NumPy code.  It can handle a large subset of Python's features, including loops,
ifs, recursion and closures, and it can even take derivatives of derivatives
of derivatives.  It supports reverse-mode differentiation
(a.k.a. backpropagation), which means it can efficiently take gradients of
scalar-valued functions with respect to array-valued arguments, as well as
forward-mode differentiation, and the two can be composed arbitrarily.  The
main intended application of Autograd is gradient-based optimization.")
      (license license:expat))))

(define-public lightgbm
  (package
    (name "lightgbm")
    (version "2.0.12")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/Microsoft/LightGBM")
                     (commit (string-append "v" version))))
              (sha256
               (base32
                "0jlvyn7k81dzrh9ij3zw576wbgiwmmr26rzpdxjn1dbpc3njpvzi"))
              (file-name (git-file-name name version))))
    (native-inputs
     (list python-pytest python-nose))
    (inputs
     (list openmpi))
    (propagated-inputs
     (list python-numpy python-scipy))
    (arguments
     `(#:configure-flags
       '("-DUSE_MPI=ON")
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (with-directory-excursion "../source"
               (invoke "pytest" "tests/c_api_test/test_.py")))))))
    (build-system cmake-build-system)
    (home-page "https://github.com/Microsoft/LightGBM")
    (synopsis "Gradient boosting framework based on decision tree algorithms")
    (description "LightGBM is a gradient boosting framework that uses tree
based learning algorithms.  It is designed to be distributed and efficient with
the following advantages:

@itemize
@item Faster training speed and higher efficiency
@item Lower memory usage
@item Better accuracy
@item Parallel and GPU learning supported (not enabled in this package)
@item Capable of handling large-scale data
@end itemize\n")
    (license license:expat)))

(define-public vowpal-wabbit
  ;; Language bindings not included.
  (package
    (name "vowpal-wabbit")
    (version "8.5.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/JohnLangford/vowpal_wabbit")
                     (commit version)))
              (sha256
               (base32
                "04bwzk6ifgnz3fmzid8b7avxf9n5pnx9xcjm61nkjng1vv0bpj8x"))
              (file-name (git-file-name name version))))
    (inputs
     (list boost zlib))
    (arguments
     `(#:configure-flags
       (list (string-append "--with-boost="
                            (assoc-ref %build-inputs "boost")))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'make-files-writable
           (lambda _
             (for-each make-file-writable (find-files "." ".*")) #t))
         (add-after 'install 'install-more-headers
           (lambda* (#:key outputs #:allow-other-keys)
             (for-each
               (lambda (file)
                 (install-file file (string-append
                                      (assoc-ref outputs "out")
                                      "/include/vowpalwabbit")))
               (find-files "vowpalwabbit" "\\.h$"))
             #t)))))
    (build-system gnu-build-system)
    (home-page "https://github.com/JohnLangford/vowpal_wabbit")
    (synopsis "Fast machine learning library for online learning")
    (description "Vowpal Wabbit is a machine learning system with techniques
such as online, hashing, allreduce, reductions, learning2search, active, and
interactive learning.")
    (license license:bsd-3)))

(define-public python-hyperopt
  (package
    (name "python-hyperopt")
    (version "0.2.7")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "hyperopt" version))
       (sha256
        (base32 "0jd1ghmm423kbhjvd6pxq92y5vkz25390687fcnd7fshh3jrmy0v"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      '(modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "python" "-m" "pytest"
                       ;; Needs python-pyspark.
                       "--ignore" "hyperopt/tests/integration/test_spark.py"
                       ;; Needs both python-scikit-learn and python-lightgbm.
                       "--ignore" "hyperopt/tests/unit/test_atpe_basic.py"
                       ;; The tests below need python-lightgbm.
                       "-k"
                       (string-append "not test_branin"
                                      " and not test_distractor"
                                      " and not test_q1lognormal"
                                      " and not test_quadratic1"
                                      " and not test_twoarms"
                                      ;; XXX Type error with this version of scipy
                                      " and not test_distribution_rvs"))))))))
    (propagated-inputs
     (list python-cloudpickle
           python-future
           python-py4j
           python-networkx
           python-numpy
           python-scipy
           python-six
           python-tqdm))
    (native-inputs
     (list python-black
           python-nose
           python-pymongo
           python-pytest))
    (home-page "https://hyperopt.github.io/hyperopt/")
    (synopsis "Library for hyperparameter optimization")
    (description "Hyperopt is a Python library for serial and parallel
optimization over awkward search spaces, which may include real-valued,
discrete, and conditional dimensions.")
    (license license:bsd-3)))

(define-public python-deepxde
  (package
    (name "python-deepxde")
    (version "1.9.3")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "DeepXDE" version))
              (sha256
               (base32
                "1zw2gqssc0s3maf4gdjckxmzx1d3036hbp1iir26kd08hxj93vzs"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:tests? #f                  ; there are no tests
           #:phases #~(modify-phases %standard-phases
                        (add-before 'sanity-check 'writable-home
                          ;; sanity-check writes ~/.deepxde/config.json to set
                          ;; the default backend.
                          (lambda _
                            (setenv "HOME" "/tmp"))))))
    ;; DeepXDE supported backends are TensorFlow (v1 and v2), PyTorch, JAX and
    ;; PaddlePaddle.  We test with PyTorch because we have it up to date.
    (native-inputs (list python-pytorch python-setuptools-scm))
    (propagated-inputs (list python-matplotlib python-numpy
                             python-scikit-learn python-scikit-optimize
                             python-scipy))
    (home-page "https://deepxde.readthedocs.io/en/latest/")
    (synopsis "Library for scientific machine learning")
    (description "DeepXDE is a library for scientific machine learning and
physics-informed learning.  It includes implementations for the PINN
(physics-informed neural networks), DeepONet (deep operator network) and
MFNN (multifidelity neural network) algorithms.")
    (license license:lgpl2.1+)))

;; There have been no proper releases yet.
(define-public kaldi
  (let ((commit "be22248e3a166d9ec52c78dac945f471e7c3a8aa")
        (revision "1")
        (openfst openfst-1.7.3)) ;; Temporary bypass for upstream issues
    (package
      (name "kaldi")
      (version (git-version "0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/kaldi-asr/kaldi")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1wkxz3p0h68mxbg41i1wygir2r4rraxbb4672xkkvvs85r6c8r8i"))))
      (build-system gnu-build-system)
      (arguments
       `(#:test-target "test"
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'chdir
             (lambda _ (chdir "src")))
           (replace 'configure
             (lambda* (#:key build system inputs outputs #:allow-other-keys)
               (when (not (or (string-prefix? "x86_64" system)
                              (string-prefix? "i686" system)))
                 (substitute* "makefiles/linux_openblas.mk"
                   (("-msse -msse2") "")))
               (substitute* "makefiles/default_rules.mk"
                 (("/bin/bash") (which "bash")))
               (substitute* "Makefile"
                 (("ext_depend: check_portaudio")
                  "ext_depend:"))
               (substitute* '("online/Makefile"
                              "onlinebin/Makefile"
                              "gst-plugin/Makefile")
                 (("../../tools/portaudio/install")
                  (assoc-ref inputs "portaudio")))
               (substitute* "matrix/Makefile"     ;temporary test bypass
                 (("matrix-lib-test sparse-matrix-test") ""))

               ;; This `configure' script doesn't support variables passed as
               ;; arguments, nor does it support "prefix".
               (let ((out (assoc-ref outputs "out"))
                     (openblas (assoc-ref inputs "openblas"))
                     (openfst (assoc-ref inputs "openfst")))
                 (substitute* "configure"
                   (("check_for_slow_expf;") "")
                   ;; This affects the RPATH and also serves as the installation
                   ;; directory.
                   (("KALDILIBDIR=`pwd`/lib")
                    (string-append "KALDILIBDIR=" out "/lib")))
                 (mkdir-p out) ; must exist
                 (setenv "CONFIG_SHELL" (which "bash"))
                 (setenv "OPENFST_VER" ,(package-version openfst))
                 (invoke "./configure"
                         "--use-cuda=no"
                         "--shared"
                         (string-append "--openblas-root=" openblas)
                         (string-append "--fst-root=" openfst)))))
           (add-after 'build 'build-ext-and-gstreamer-plugin
             (lambda _
               (invoke "make" "-C" "online" "depend")
               (invoke "make" "-C" "online")
               (invoke "make" "-C" "onlinebin" "depend")
               (invoke "make" "-C" "onlinebin")
               (invoke "make" "-C" "gst-plugin" "depend")
               (invoke "make" "-C" "gst-plugin")))
           ;; TODO: also install the executables.
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (inc (string-append out "/include"))
                      (lib (string-append out "/lib")))
                 (mkdir-p lib)
                 ;; The build phase installed symlinks to the actual
                 ;; libraries.  Install the actual targets.
                 (for-each (lambda (file)
                             (let ((target (readlink file)))
                               (delete-file file)
                               (install-file target lib)))
                           (find-files lib "\\.so"))
                 ;; Install headers
                 (for-each (lambda (file)
                             (let ((target-dir (string-append inc "/" (dirname file))))
                               (install-file file target-dir)))
                           (find-files "." "\\.h"))
                 (install-file "gst-plugin/libgstonlinegmmdecodefaster.so"
                               (string-append lib "/gstreamer-1.0"))))))))
      (inputs
       (list alsa-lib
             `(,gfortran "lib")
             glib
             gstreamer
             jack-1
             openblas
             openfst
             portaudio
             python))
      (native-inputs
       (list `(,glib "bin") ; glib-genmarshal
             grep
             sed
             pkg-config
             which))
      (home-page "https://kaldi-asr.org/")
      (synopsis "Speech recognition toolkit")
      (description "Kaldi is an extensible toolkit for speech recognition
written in C++.")
      (license license:asl2.0))))

(define kaldi-for-vosk
  (let* ((commit "a25f216f5ce4eec5e45a6ab7651e20c9840a05cd")
         (revision "0")
         (openfst openfst-for-vosk))
    (package
      (inherit kaldi)
      (name "kaldi")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/alphacep/kaldi")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "16w90za8narkfi590cxj4p7vc1f5sdxc927g5hk6kh4l3mf6iisl"))))
      (inputs
       (list alsa-lib
             lapack ;compared to base kaldi, replacing `(,gfortran "lib")
             glib
             gstreamer
             jack-2
             openblas
             openfst
             portaudio
             python))
      (arguments
       (list
        #:test-target "test"
        #:make-flags '(list "online2" "lm" "rnnlm")
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'chdir
              (lambda _ (chdir "src")))
            (replace 'configure
              (lambda _
                (let ((portaudio #$(this-package-input "portaudio"))
                      (lapack    #$(this-package-input "lapack"))
                      (openfst   #$(this-package-input "openfst"))
                      (openblas  #$(this-package-input "openblas")))
                  #$@(if (target-x86?)
                         '()
                         #~((substitute* "makefiles/linux_openblas.mk"
                              (("-msse -msse2") ""))))
                  (substitute* "makefiles/default_rules.mk"
                    (("/bin/bash") (which "bash")))
                  (substitute* "Makefile"
                    (("ext_depend: check_portaudio")
                     "ext_depend:"))
                  (substitute* '("online/Makefile"
                                 "onlinebin/Makefile"
                                 "gst-plugin/Makefile")
                    (("../../tools/portaudio/install")
                     portaudio))
                  (substitute* "matrix/Makefile"     ;temporary test bypass
                    (("matrix-lib-test sparse-matrix-test") ""))
                  (substitute* "cudamatrix/Makefile"
                    ((" cu-array-test") ""))

                  ;; This `configure' script doesn't support variables passed as
                  ;; arguments, nor does it support "prefix".
                  (substitute* "configure"
                    (("check_for_slow_expf;") "")
                    ;; This affects the RPATH and also serves as the installation
                    ;; directory.
                    (("KALDILIBDIR=`pwd`/lib")
                     (string-append "KALDILIBDIR=" #$output "/lib"))
                    (("OPENBLASROOT=\\\"\\$\\(rel2abs ..\\/tools\\/OpenBLAS\\/install\\)\\\"")
                     (string-append "OPENBLASROOT=\"" openblas "\""))
                    (("-L\\$OPENBLASLIBDIR -l:libopenblas.a -l:libblas.a -l:liblapack.a -l:libf2c.a")
                     (string-append "-L$OPENBLASLIBDIR -lopenblas "
                                    "-L" lapack "/lib -lblas -llapack")))
                  (mkdir-p #$output) ; must exist
                  (setenv "CONFIG_SHELL" (which "bash"))
                  (setenv "OPENFST_VER" #$(package-version openfst))
                  (invoke "./configure"
                          "--use-cuda=no"
                          "--mathlib=OPENBLAS_CLAPACK"
                          "--shared"
                          (string-append "--fst-root=" openfst)))))
            (add-after 'configure 'optimize-build
              (lambda _ (substitute* "kaldi.mk" ((" -O1") " -O3"))))
            (replace 'install
              (lambda _
                (let ((inc (string-append #$output "/include"))
                      (lib (string-append #$output "/lib")))
                  ;; The build phase installed symlinks to the actual
                  ;; libraries.  Install the actual targets.
                  (for-each (lambda (file)
                              (let ((target (readlink file)))
                                (delete-file file)
                                (install-file target lib)))
                            (find-files lib "\\.so"))
                  ;; Install headers
                  (for-each (lambda (file)
                              (let ((target-dir (string-append inc "/" (dirname file))))
                                (install-file file target-dir)))
                            (find-files "." "\\.h")))))))))))

(define-public gst-kaldi-nnet2-online
  (let ((commit "7888ae562a65bd7e406783ce2c33535bc66a30ef")
        (revision "3"))
    (package
      (name "gst-kaldi-nnet2-online")
      (version (git-version "0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/alumae/gst-kaldi-nnet2-online")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0xp59a6lmx1y24i8bkmxcm27lhm5x5m6y41670yjzhamcbnx8jcr"))))
      (build-system gnu-build-system)
      (arguments
       (list
        #:tests? #f                    ; there are none
        #:make-flags
        '(list (string-append "SHELL="
                              (assoc-ref %build-inputs "bash") "/bin/bash")
               (string-append "KALDI_ROOT="
                              (assoc-ref %build-inputs "kaldi-src"))
               (string-append "KALDILIBDIR="
                              (assoc-ref %build-inputs "kaldi") "/lib")
               "KALDI_FLAVOR=dynamic")
         #:phases
         '(modify-phases %standard-phases
            (add-after 'unpack 'chdir
              (lambda _ (chdir "src")))
            (replace 'configure
              (lambda* (#:key inputs #:allow-other-keys)
                (let ((glib (assoc-ref inputs "glib")))
                  (setenv "CXXFLAGS" "-fPIC")
                  (setenv "CPLUS_INCLUDE_PATH"
                          (string-append glib "/include/glib-2.0:"
                                         glib "/lib/glib-2.0/include:"
                                         (assoc-ref inputs "gstreamer")
                                         "/include/gstreamer-1.0:"
                                         (getenv "CPLUS_INCLUDE_PATH"))))
                (substitute* "Makefile"
                  (("include \\$\\(KALDI_ROOT\\)/src/kaldi.mk") "")
                  (("\\$\\(error Cannot find") "#"))))
            (add-before 'build 'build-depend
              (lambda* (#:key make-flags #:allow-other-keys)
                (apply invoke "make" "depend" make-flags)))
            (replace 'install
              (lambda* (#:key outputs #:allow-other-keys)
                (let* ((out (assoc-ref outputs "out"))
                       (lib (string-append out "/lib/gstreamer-1.0")))
                  (install-file "libgstkaldinnet2onlinedecoder.so" lib)))))))
      (inputs
       (list glib gstreamer jansson openfst-1.7.3 kaldi))
      (native-inputs
       `(("bash" ,bash)
         ("glib:bin" ,glib "bin")       ; glib-genmarshal
         ("kaldi-src" ,(package-source kaldi))
         ("pkg-config" ,pkg-config)))
      (home-page "https://kaldi-asr.org/")
      (synopsis "Gstreamer plugin for decoding speech")
      (description "This package provides a GStreamer plugin that wraps
Kaldi's @code{SingleUtteranceNnet2Decoder}.  It requires iVector-adapted DNN
acoustic models.  The iVectors are adapted to the current audio stream
automatically.")
      (license license:asl2.0))))

(define-public kaldi-gstreamer-server
  ;; This is the tip of the py3 branch
  (let ((commit "f79e204d751a5964918001822e4520fa2acfd246")
        (revision "3"))
    (package
      (name "kaldi-gstreamer-server")
      (version (git-version "0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/alumae/kaldi-gstreamer-server")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1iijq8jmgdxr7961inal1ggs496ymxradm51m4sqx8vl983x14y8"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f ; there are no tests that can be run automatically
         #:modules ((guix build utils)
                    (guix build gnu-build-system)
                    (srfi srfi-26))
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (replace 'build
             (lambda* (#:key outputs #:allow-other-keys)
               ;; Disable hash randomization to ensure the generated .pycs
               ;; are reproducible.
               (setenv "PYTHONHASHSEED" "0")
               (with-directory-excursion "kaldigstserver"
                 ;; See https://github.com/alumae/kaldi-gstreamer-server/issues/232
                 (substitute* "master_server.py"
                   (("\\.replace\\('\\\\.*") ")"))

                 ;; This is a Python 2 file
                 (delete-file "decoder_test.py")
                 (delete-file "test-buffer.py")

                 (for-each (lambda (file)
                             (apply invoke
                                    `("python"
                                      "-m" "compileall"
                                      "-f" ; force rebuild
                                      ,file)))
                           (find-files "." "\\.py$")))))
           (replace 'install
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (bin (string-append out "/bin"))
                      (share (string-append out "/share/kaldi-gstreamer-server/")))
                 ;; Install Python files
                 (with-directory-excursion "kaldigstserver"
                   (for-each (cut install-file <> share)
                             (find-files "." ".*")))

                 ;; Install sample configuration files
                 (for-each (cut install-file <> share)
                           (find-files "." "\\.yaml"))

                 ;; Install executables
                 (mkdir-p bin)
                 (let* ((server (string-append bin "/kaldi-gst-server"))
                        (client (string-append bin "/kaldi-gst-client"))
                        (worker (string-append bin "/kaldi-gst-worker"))
                        (PYTHONPATH (getenv "GUIX_PYTHONPATH"))
                        (GST_PLUGIN_PATH (string-append
                                          (assoc-ref inputs "gst-kaldi-nnet2-online")
                                          "/lib/gstreamer-1.0:${GST_PLUGIN_PATH}"))
                        (wrap (lambda (wrapper what)
                                (with-output-to-file wrapper
                                  (lambda _
                                    (format #t
                                            "#!~a
export GUIX_PYTHONPATH=~a
export GST_PLUGIN_PATH=~a
exec ~a ~a/~a \"$@\"~%"
                                            (which "bash") PYTHONPATH GST_PLUGIN_PATH
                                            (which "python") share what)))
                                (chmod wrapper #o555))))
                   (for-each wrap
                             (list server client worker)
                             (list "master_server.py"
                                   "client.py"
                                   "worker.py")))))))))
      (inputs
       (list gst-kaldi-nnet2-online
             python-wrapper
             python-pygobject
             python-pyyaml
             python-tornado-6))
      (home-page "https://github.com/alumae/kaldi-gstreamer-server")
      (synopsis "Real-time full-duplex speech recognition server")
      (description "This is a real-time full-duplex speech recognition server,
based on the Kaldi toolkit and the GStreamer framework and implemented in
Python.")
      (license license:bsd-2))))

;; Note that Tensorflow includes a "third_party" directory, which seems to not
;; only contain modified subsets of upstream library source code, but also
;; adapter headers provided by Google (such as the fft.h header, which is not
;; part of the upstream project code).  The Tensorflow code includes headers
;; from the "third_party" directory.  It does not look like we can replace
;; these headers with unmodified upstream files, so we keep them.
(define-public tensorflow
  (package
    (name "tensorflow")
    (version "1.9.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tensorflow/tensorflow")
             (commit (string-append "v" version))))
       (file-name (string-append "tensorflow-" version "-checkout"))
       (sha256
        (base32
         "0a9kwha395g3wgxfwln5j8vn9nkspmd75xldrlqdq540w996g8xa"))
       (patches
        (search-patches "tensorflow-c-api-fix.patch"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f                      ; no "check" target
       #:build-type "Release"
       #:configure-flags
       (let ((protobuf (assoc-ref %build-inputs "protobuf"))
             (protobuf:native (assoc-ref %build-inputs "protobuf:native"))
             (jsoncpp (assoc-ref %build-inputs "jsoncpp"))
             (snappy (assoc-ref %build-inputs "snappy"))
             (sqlite (assoc-ref %build-inputs "sqlite")))
         (list
          ;; Use protobuf from Guix
          (string-append "-Dprotobuf_STATIC_LIBRARIES="
                         protobuf "/lib/libprotobuf.so")
          (string-append "-DPROTOBUF_PROTOC_EXECUTABLE="
                         protobuf:native "/bin/protoc")

          ;; Use snappy from Guix
          (string-append "-Dsnappy_STATIC_LIBRARIES="
                         snappy "/lib/libsnappy.so")
          ;; Yes, this is not actually the include directory but a prefix...
          (string-append "-Dsnappy_INCLUDE_DIR=" snappy)

          ;; Use jsoncpp from Guix
          (string-append "-Djsoncpp_STATIC_LIBRARIES="
                         jsoncpp "/lib/libjsoncpp.so")
          ;; Yes, this is not actually the include directory but a prefix...
          (string-append "-Djsoncpp_INCLUDE_DIR=" jsoncpp)

          ;; Use sqlite from Guix
          (string-append "-Dsqlite_STATIC_LIBRARIES="
                         sqlite "/lib/libsqlite.a")

          ;; Use system libraries wherever possible.  Currently, this
          ;; only affects zlib.
          "-Dsystemlib_ALL=ON"
          "-Dtensorflow_ENABLE_POSITION_INDEPENDENT_CODE=ON"
          "-Dtensorflow_BUILD_SHARED_LIB=ON"
          "-Dtensorflow_OPTIMIZE_FOR_NATIVE_ARCH=OFF"
          "-Dtensorflow_ENABLE_SSL_SUPPORT=OFF"
          "-Dtensorflow_BUILD_CONTRIB_KERNELS=OFF"))
       #:make-flags
       (list "CC=gcc")
       #:modules ((ice-9 ftw)
                  (guix build utils)
                  (guix build cmake-build-system)
                  ((guix build python-build-system)
                   #:select (python-version)))
       #:imported-modules (,@%cmake-build-system-modules
                           (guix build python-build-system))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'set-source-file-times-to-1980
           ;; At the end of the tf_python_build_pip_package target, a ZIP
           ;; archive should be generated via bdist_wheel, but it fails with
           ;; "ZIP does not support timestamps before 1980".  Luckily,
           ;; SOURCE_DATE_EPOCH is respected, which we set to some time in
           ;; 1980.
           (lambda _ (setenv "SOURCE_DATE_EPOCH" "315532800")))
         (add-after 'unpack 'python3.10-compatibility
           (lambda _
             ;; See https://github.com/tensorflow/tensorflow/issues/20517#issuecomment-406373913
             (substitute* '("tensorflow/python/eager/pywrap_tfe_src.cc"
                            "tensorflow/python/lib/core/ndarray_tensor.cc"
                            "tensorflow/python/lib/core/py_func.cc")
               (("PyUnicode_AsUTF8") "(char *)PyUnicode_AsUTF8"))
             (substitute* "tensorflow/c/eager/c_api.h"
               (("unsigned char async")
                "unsigned char is_async"))

             ;; Remove dependency on tensorboard, a complicated but probably
             ;; optional package.
             (substitute* "tensorflow/tools/pip_package/setup.py"
               ((".*'tensorboard >.*") ""))

             ;; Fix the build with python-3.8, taken from rejected upstream patch:
             ;; https://github.com/tensorflow/tensorflow/issues/34197
             (substitute* (find-files "tensorflow/python" ".*\\.cc$")
               (("(nullptr,)(\\ +/. tp_print)" _ _ tp_print)
                (string-append "NULL,   " tp_print)))

             ;; Many collections classes have been moved to collections.abc
             (substitute* '("tensorflow/python/framework/ops.py"
                            "tensorflow/python/ops/clip_ops.py"
                            "tensorflow/python/ops/data_flow_ops.py"
                            "tensorflow/python/ops/gradients_impl.py"
                            "tensorflow/python/training/input.py"
                            "tensorflow/python/training/checkpointable/data_structures.py"
                            "tensorflow/python/util/nest.py"
                            "tensorflow/python/util/protobuf/compare.py")
               (("collections.Mapping") "collections.abc.Mapping")
               (("collections.Sequence") "collections.abc.Sequence"))
             (substitute* "tensorflow/python/feature_column/feature_column.py"
               (("collections.Iterator") "collections.abc.Iterator"))
             (substitute* "tensorflow/python/ops/sparse_ops.py"
               (("collections.Iterable") "collections.abc.Iterable"))
             (substitute* "tensorflow/python/keras/callbacks.py"
               (("from collections import Iterable")
                "from collections.abc import Iterable"))
             (substitute* "tensorflow/python/ops/variable_scope.py"
               (("collections_lib.Sequence") "collections_lib.abc.Sequence"))

             ;; XXX: it is not clear if this is a good idea, but the build
             ;; system tries to overwrite the __or__ and __ror__ methods of
             ;; the Tensor class.
             (substitute* "tensorflow/python/framework/ops.py"
               (("if not isinstance\\(existing, type\\(object.__lt__\\)\\)" m)
                (string-append m
                               " and not isinstance(existing, type(object.__or__))")))

             ;; Fix the build with numpy >= 1.19.
             ;; Suggested in https://github.com/tensorflow/tensorflow/issues/41086#issuecomment-656833081
             (substitute* "tensorflow/python/lib/core/bfloat16.cc"
               (("void BinaryUFunc\\(char\\*\\* args, npy_intp\\* dimensions, npy_intp\\* steps,")
                "void BinaryUFunc(char** args, npy_intp const* dimensions, npy_intp const* steps,")
               (("void CompareUFunc\\(char\\*\\* args, npy_intp\\* dimensions, npy_intp\\* steps,")
                "void CompareUFunc(char** args, npy_intp const* dimensions, npy_intp const* steps,"))

             ;; ...and for numpy >= 1.23
             (substitute* "tensorflow/python/framework/tensor_util.py"
               (("np.asscalar\\(x\\[0\\]\\)") "x[0].item()")
               (("np.asscalar\\(x\\)") "x.item()")
               (("np.asscalar\\(v\\)") "np.ndarray.item(v)")
               (("return np.asscalar") "return np.ndarray.item"))
             (substitute* "tensorflow/python/kernel_tests/cwise_ops_test.py"
               (("np.asscalar\\(np.random.rand\\(1\\) \\* 100.\\)")
                "(np.random.rand(1) * 100.).item()"))
             (substitute* '("tensorflow/python/framework/fast_tensor_util.pyx"
                            "tensorflow/python/estimator/canned/linear_testing_utils.py")
               (("np.asscalar") "np.ndarray.item"))))
         (add-after 'python3.10-compatibility 'chdir
           (lambda _ (chdir "tensorflow/contrib/cmake")))
         (add-after 'chdir 'disable-downloads
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* (find-files "external" "\\.cmake$")
               (("GIT_REPOSITORY.*") "")
               (("GIT_TAG.*") "")
               (("PREFIX ")
                "DOWNLOAD_COMMAND \"\"\nPREFIX "))

             ;; Use packages from Guix
             (let ((grpc (assoc-ref inputs "grpc")))
               (substitute* "CMakeLists.txt"
                 ;; Sqlite
                 (("include\\(sqlite\\)") "")
                 (("\\$\\{sqlite_STATIC_LIBRARIES\\}")
                  (search-input-file inputs "/lib/libsqlite3.so"))
                 (("sqlite_copy_headers_to_destination") "")

                 ;; PNG
                 (("include\\(png\\)") "")
                 (("\\$\\{png_STATIC_LIBRARIES\\}")
                  (search-input-file inputs "/lib/libpng16.so"))
                 (("png_copy_headers_to_destination") "")

                 ;; JPEG
                 (("include\\(jpeg\\)") "")
                 (("\\$\\{jpeg_STATIC_LIBRARIES\\}")
                  (search-input-file inputs "/lib/libjpeg.so"))
                 (("jpeg_copy_headers_to_destination") "")

                 ;; GIF
                 (("include\\(gif\\)") "")
                 (("\\$\\{gif_STATIC_LIBRARIES\\}")
                  (search-input-file inputs "/lib/libgif.so"))
                 (("gif_copy_headers_to_destination") "")

                 ;; lmdb
                 (("include\\(lmdb\\)") "")
                 (("\\$\\{lmdb_STATIC_LIBRARIES\\}")
                  (search-input-file inputs "/lib/liblmdb.so"))
                 (("lmdb_copy_headers_to_destination") "")

                 ;; Protobuf
                 (("include\\(protobuf\\)") "")
                 (("protobuf_copy_headers_to_destination") "")
                 (("^ +protobuf") "")

                 ;; gRPC
                 (("include\\(grpc\\)")
                  "find_package(grpc REQUIRED NAMES gRPC)")
                 (("list\\(APPEND tensorflow_EXTERNAL_DEPENDENCIES grpc\\)") "")

                 ;; Eigen
                 (("include\\(eigen\\)")
                  (string-append "find_package(eigen REQUIRED NAMES Eigen3)
set(eigen_INCLUDE_DIRS ${CMAKE_CURRENT_BINARY_DIR}/external/eigen_archive "
                                 (assoc-ref inputs "eigen") "/include/eigen3)"))
                 (("^ +eigen") "")

                 ;; snappy
                 (("include\\(snappy\\)")
                  "add_definitions(-DTF_USE_SNAPPY)")
                 (("list\\(APPEND tensorflow_EXTERNAL_DEPENDENCIES snappy\\)") "")

                 ;; jsoncpp
                 (("include\\(jsoncpp\\)") "")
                 (("^ +jsoncpp") ""))

               (substitute* "tf_core_framework.cmake"
                 ((" grpc") "")
                 (("\\$\\{GRPC_BUILD\\}/grpc_cpp_plugin")
                  (which "grpc_cpp_plugin"))
                 ;; Link with gRPC libraries
                 (("add_library\\(tf_protos_cc.*" m)
                  (string-append m
                                 (format #f "\ntarget_link_libraries(tf_protos_cc PRIVATE \
~a/lib/libgrpc++_unsecure.a \
~a/lib/libgrpc_unsecure.a \
~a/lib/libaddress_sorting.a \
~a/lib/libgpr.a \
~a//lib/libcares.so
)\n"
                                         grpc grpc grpc grpc
                                         (assoc-ref inputs "c-ares"))))))
             (substitute* "tf_tools.cmake"
               (("add_dependencies\\(\\$\\{proto_text.*") ""))
             ;; Remove dependency on bundled grpc
             (substitute* "tf_core_distributed_runtime.cmake"
               (("tf_core_cpu grpc") "tf_core_cpu"))

             ;; This directory is a dependency of many targets.
             (mkdir-p "protobuf")))
         (add-after 'configure 'unpack-third-party-sources
           (lambda* (#:key inputs outputs #:allow-other-keys)
             ;; This is needed to configure bundled packages properly.
             (setenv "CONFIG_SHELL" (which "bash"))
             (for-each
              (lambda (name)
                (let* ((what  (assoc-ref inputs (string-append name "-src")))
                       (name* (string-map (lambda (c)
                                            (if (char=? c #\-)
                                                #\_ c)) name))
                       (where (string-append "../build/" name* "/src/" name*)))
                  (cond
                   ((string-suffix? ".zip" what)
                    (mkdir-p where)
                    (with-directory-excursion where
                      (invoke "unzip" what)))
                   ((string-suffix? ".tar.gz" what)
                    (mkdir-p where)
                    (invoke "tar" "xf" what
                            "-C" where "--strip-components=1"))
                   (else
                    (let ((parent (dirname where)))
                      (mkdir-p parent)
                      (with-directory-excursion parent
                        (when (file-exists? name*)
                          (delete-file-recursively name*))
                        (copy-recursively what name*)
                        (map make-file-writable
                             (find-files name* ".*"))))))))
              (list "boringssl"
                    "cub"
                    "double-conversion"
                    "farmhash"
                    "fft2d"
                    "highwayhash"
                    "nsync"
                    "re2"))

             ;; https://github.com/google/farmhash/issues/24
             (substitute* "../build/farmhash/src/farmhash/src/farmhash.cc"
               (("using namespace std;") "")
               (("make_pair") "std::make_pair")
               (("pair<") "std::pair<"))

             (rename-file "../build/cub/src/cub/cub-1.8.0/"
                          "../build/cub/src/cub/cub/")

             (setenv "LDFLAGS"
                     (string-append "-Wl,-rpath="
                                    (assoc-ref outputs "out") "/lib"))))
         (add-after 'unpack 'fix-python-build
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (mkdir-p "protobuf-src")
             (invoke "tar" "xf" (assoc-ref inputs "protobuf:src")
                     "-C" "protobuf-src" "--strip-components=1")
             (mkdir-p "eigen-src")
             (copy-recursively (assoc-ref inputs "eigen:src") "eigen-src")

             ;; distutils.sysconfig is deprecated and prints a deprecation
             ;; warning that breaks the generated CXX_INCLUDES line.
             (substitute* "tensorflow/contrib/cmake/tf_python.cmake"
               (("import distutils.sysconfig; print\\(distutils.sysconfig.get_python_inc\\(\\)\\)")
                "import sysconfig; print(sysconfig.get_path('include'))"))

             (substitute* "tensorflow/contrib/cmake/tf_python.cmake"
               ;; Take protobuf source files from our source package.
               (("\\$\\{CMAKE_CURRENT_BINARY_DIR\\}/protobuf/src/protobuf/src/google")
                (string-append (getcwd) "/protobuf-src/src/google")))

             (substitute* '("tensorflow/contrib/cmake/tf_shared_lib.cmake"
                            "tensorflow/contrib/cmake/tf_python.cmake")
               ;; Take Eigen source files from our source package.
               (("\\$\\{CMAKE_CURRENT_BINARY_DIR\\}/eigen/src/eigen/")
                (string-append (getcwd) "/eigen-src/"))
               ;; Take Eigen headers from our own package.
               (("\\$\\{CMAKE_CURRENT_BINARY_DIR\\}/external/eigen_archive")
                (search-input-directory inputs "/include/eigen3")))

             ;; Correct the RUNPATH of ops libraries generated for Python.
             ;; TODO: this doesn't work :(
             ;; /gnu/store/...-tensorflow-1.9.0/lib/python3.7/site-packages/tensorflow/contrib/seq2seq/python/ops/lib_beam_search_ops.so:
             ;; warning: RUNPATH contains bogus entries: ("/tmp/guix-build-tensorflow-1.9.0.drv-0/source/tensorflow/contrib/build")
             ;; /gnu/store/...-tensorflow-1.9.0/lib/python3.7/site-packages/tensorflow/contrib/seq2seq/python/ops/lib_beam_search_ops.so:
             ;; error: depends on 'libpywrap_tensorflow_internal.so', which
             ;; cannot be found in RUNPATH ...
             (substitute* "tensorflow/contrib/cmake/tf_cc_ops.cmake"
               (("set_target_properties.*")
                (string-append "set_target_properties(${_AT_TARGET} PROPERTIES \
COMPILE_FLAGS ${target_compile_flags} \
INSTALL_RPATH_USE_LINK_PATH TRUE \
INSTALL_RPATH " (assoc-ref outputs "out") "/lib)\n")))))
         (add-after 'unpack 'patch-cmake-file-to-install-c-headers
           (lambda _
             (substitute* "tensorflow/contrib/cmake/tf_c.cmake"
               (("if\\(tensorflow_BUILD_PYTHON_BINDINGS" m)
                (string-append
                 "install(DIRECTORY ${tensorflow_source_dir}/tensorflow/c/ \
DESTINATION include/tensorflow/c FILES_MATCHING PATTERN \"*.h\")\n" m)))))
         (add-after 'build 'build-c-bindings
           (lambda* (#:key outputs parallel-build? #:allow-other-keys)
             (invoke "make" "-j" (if parallel-build?
                                     (number->string (parallel-job-count))
                                     "1")
                     "tf_c")))
         (add-after 'install 'build-pip-package
           (lambda* (#:key outputs parallel-build? #:allow-other-keys)
             (invoke "make" "-j" (if parallel-build?
                                     (number->string (parallel-job-count))
                                     "1")
                     "tf_python_build_pip_package")))
         (add-after 'build-pip-package 'install-python
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (wheel (car (find-files "../build/tf_python/dist/" "\\.whl$")))
                   (python-version (python-version
                                    (assoc-ref inputs "python"))))
               (invoke "python" "-m" "pip" "install" wheel
                       (string-append "--prefix=" out))

               ;; XXX: broken RUNPATH, see fix-python-build phase.
               (delete-file
                (string-append
                 out "/lib/python" python-version
                 "/site-packages/tensorflow/contrib/"
                 "seq2seq/python/ops/lib_beam_search_ops.so"))))))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("protobuf:native" ,protobuf-3.6) ; protoc
       ("protobuf:src" ,(package-source protobuf-3.6))
       ("eigen:src" ,(package-source eigen-for-tensorflow))
       ;; install_pip_packages.sh wants setuptools 39.1.0 specifically.
       ("python-setuptools" ,python-setuptools-for-tensorflow)

       ;; The commit hashes and URLs for third-party source code are taken
       ;; from "tensorflow/workspace.bzl".
       ("boringssl-src"
        ,(let ((commit "ee7aa02")
               (revision "1"))
           (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://boringssl.googlesource.com/boringssl")
                   (commit commit)))
             (file-name (string-append "boringssl-0-" revision
                                       (string-take commit 7)
                                       "-checkout"))
             (sha256
              (base32
               "1jf693q0nw0adsic6cgmbdx6g7wr4rj4vxa8j1hpn792fqhd8wgw")))))
       ("cub-src"
        ,(let ((version "1.8.0"))
           (origin
             (method url-fetch)
             (uri (string-append "https://mirror.bazel.build/github.com/NVlabs/"
                                 "cub/archive/" version ".zip"))
             (file-name (string-append "cub-" version ".zip"))
             (sha256
              (base32
               "1hsqikqridb90dkxkjr2918dcry6pfh46ccnwrzawl56aamhdykb")))))
       ("double-conversion-src"
        ,(let ((commit "5664746")
               (revision "1"))
           (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/google/double-conversion")
                   (commit commit)))
             (file-name
              (git-file-name "double-conversion"
                             (string-append "0-" revision "."
                                            (string-take commit 7))))
             (sha256
              (base32
               "1h5lppqqxcvdg5jq42i5msgwx20ryij3apvmndflngrgdpc04gn1")))))
       ("farmhash-src"
        ,(let ((commit "816a4ae622e964763ca0862d9dbd19324a1eaf45"))
           (origin
             (method url-fetch)
             (uri (string-append
                   "https://mirror.bazel.build/github.com/google/farmhash/archive/"
                   commit ".tar.gz"))
             (file-name (string-append "farmhash-0-" (string-take commit 7)
                                       ".tar.gz"))
             (sha256
              (base32
               "185b2xdxl4d4cnsnv6abg8s22gxvx8673jq2yaq85bz4cdy58q35")))))
       ;; The license notice on the home page at
       ;; http://www.kurims.kyoto-u.ac.jp/~ooura/fft.html says:
       ;;   Copyright Takuya OOURA, 1996-2001
       ;;
       ;;   You may use, copy, modify and distribute this code for any purpose
       ;;   (include commercial use) and without fee. Please refer to this
       ;;   package when you modify this code.
       ;;
       ;; We take the identical tarball from the Bazel mirror, because the URL
       ;; at the home page is not versioned and might change.
       ("fft2d-src"
        ,(origin
           (method url-fetch)
           (uri "https://mirror.bazel.build/www.kurims.kyoto-u.ac.jp/~ooura/fft.tgz")
           (file-name "fft2d.tar.gz")
           (sha256
            (base32
             "15jjkfvhqvl2c0753d2di8hz0pyzn598g74wqy79awdrf1y67fsj"))))
       ("highwayhash-src"
        ,(let ((commit "be5edafc2e1a455768e260ccd68ae7317b6690ee")
               (revision "1"))
           (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/google/highwayhash")
                   (commit commit)))
             (file-name (string-append "highwayhash-0-" revision
                                       (string-take commit 7)
                                       "-checkout"))
             (sha256
              (base32
               "154jwf98cyy54hldr94pgjn85zynly3abpnc1avmb8a18lzwjyb6")))))
       ("nsync-src"
        ,(let ((version "0559ce013feac8db639ee1bf776aca0325d28777")
               (revision "1"))
           (origin
             (method url-fetch)
             (uri (string-append "https://mirror.bazel.build/"
                                 "github.com/google/nsync/archive/"
                                 version ".tar.gz"))
             (file-name (string-append "nsync-0." revision
                                       "-" (string-take version 7)
                                       ".tar.gz"))
             (sha256
              (base32
               "0qdkyqym34x739mmzv97ah5r7ph462v5xkxqxvidmcfqbi64b132")))))
       ("re2-src"
        ,(let ((commit "e7efc48")
               (revision "1"))
           (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/google/re2")
                   (commit commit)))
             (file-name (string-append "re2-0-" revision
                                       (string-take commit 7)
                                       "-checkout"))
             (sha256
              (base32
               "161g9841rjfsy5pn52fcis0s9hdr7rxvb06pad38j5rppfihvign")))))
       ("googletest" ,googletest)
       ("swig" ,swig)
       ("unzip" ,unzip)))
    (propagated-inputs
     (list python-absl-py
           python-astor
           python-gast
           python-grpcio
           python-numpy
           python-protobuf-3.6
           python-six
           python-termcolor
           python-wheel))
    (inputs
     `(("c-ares" ,c-ares)
       ("eigen" ,eigen-for-tensorflow)
       ("gemmlowp" ,gemmlowp-for-tensorflow)
       ("lmdb" ,lmdb)
       ("libjpeg" ,libjpeg-turbo)
       ("libpng" ,libpng)
       ("giflib" ,giflib)
       ("grpc" ,grpc-1.16.1 "static")
       ("grpc:bin" ,grpc-1.16.1)
       ("jsoncpp" ,jsoncpp-for-tensorflow)
       ("snappy" ,snappy)
       ("sqlite" ,sqlite)
       ("protobuf" ,protobuf-3.6)
       ("python" ,python-wrapper)
       ("zlib" ,zlib)))
    (home-page "https://tensorflow.org")
    (synopsis "Machine learning framework")
    (description
     "TensorFlow is a flexible platform for building and training machine
learning models.  It provides a library for high performance numerical
computation and includes high level Python APIs, including both a sequential
API for beginners that allows users to build models quickly by plugging
together building blocks and a subclassing API with an imperative style for
advanced research.")
    (license license:asl2.0)))

(define-public tensorflow-lite
  (package
    (name "tensorflow-lite")
    (version "2.12.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tensorflow/tensorflow")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0jkgljdagdqllnxygl35r5bh3f9qmbczymfj357gm9krh59g2kmd"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #false                   ;tests are not building now
      #:build-type "Release"
      #:modules '((ice-9 match)
                  (guix build utils)
                  (guix build cmake-build-system))
      #:configure-flags
      #~(list
         ;; "-DTFLITE_KERNEL_TEST=ON"  ; TODO: build tests
         ;; so cmake can be used to find this from other packages
         "-DTFLITE_ENABLE_INSTALL=ON"

         ;; Use Guix's own packages as dependencies.
         "-DCMAKE_FIND_PACKAGE_PREFER_CONFIG=ON"

         "-DTFLITE_ENABLE_GPU=ON"
         "-DTFLITE_ENABLE_RUY=ON"

         ;; TODO: turn on Farmhash
         ;;"-DSYSTEM_FARMHASH=ON"
         (string-append "-Dabsl_DIR=" #$(this-package-input "abseil-cpp")
                        "/lib/cmake/absl")
         (string-append "-DEigen3_DIR=" #$(this-package-input "eigen")
                        "/share/eigen3/cmake")
         (string-append "-DFlatBuffers_DIR="
                        #$(this-package-input "flatbuffers-shared")
                        "/lib/cmake/flatbuffers")
         (string-append "-DNEON_2_SSE_DIR=" #$(this-package-input "neon2sse")
                        "/lib/cmake/NEON_2_SSE")
         (string-append "-Dcpuinfo_DIR=" #$(this-package-input "cpuinfo")
                        "/share/cpuinfo")
         (string-append "-Druy_DIR=" #$(this-package-input "ruy")
                        "/lib/cmake/ruy")

         ;; TODO: The build system attempts to build xnnpack from source.  We
         ;; would like to use our xnnpack package here, but this requires more
         ;; work.
         "-DTFLITE_ENABLE_XNNPACK=OFF"

         ;; Don't fetch the sources.  We have these already
         "-Degl_headers_POPULATED=TRUE"
         "-Dfp16_headers_POPULATED=TRUE"
         "-Dopencl_headers_POPULATED=TRUE"
         "-Dopengl_headers_POPULATED=TRUE"
         "-Dvulkan_headers_POPULATED=TRUE"
         "-Dgoogletest_POPULATED=TRUE"
         "-Dgoogle_benchmark_POPULATED=TRUE"
         "-Dnsync_POPULATED=TRUE"
         "-Dre2_POPULATED=TRUE"

         "-DFFT2D_SOURCE_DIR=/tmp/fft2d"
         "-DFARMHASH_SOURCE_DIR=/tmp/farmhash"
         "-Dgemmlowp_SOURCE_DIR=/tmp/gemmlowp")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'chdir
            (lambda _ (chdir "tensorflow/lite")))
          (add-after 'chdir 'copy-sources
            (lambda* (#:key inputs #:allow-other-keys)
              ;; TODO: properly use Guix's pthreaqdpool.  We are not using
              ;; pthreadpool because we are not enabling xnnpack
              (substitute* "CMakeLists.txt"
                (("if\\(NOT DEFINED PTHREADPOOL_SOURCE_DIR\\)")
                 "if(false)"))
              (substitute* "CMakeLists.txt"
                (("if\\(NOT TARGET pthreadpool\\)")
                 "if(false)"))

              ;; Don't fetch source code; we already have everything we need.
              (substitute* '("tools/cmake/modules/fft2d.cmake"
                             "tools/cmake/modules/farmhash.cmake"
                             "tools/cmake/modules/gemmlowp.cmake")
                (("OverridableFetchContent_Populate.*") ""))

              (mkdir-p "/tmp/farmhash")
              (with-directory-excursion "/tmp/farmhash"
                (invoke "tar" "--strip-components=1"
                        "-xf" (assoc-ref inputs "farmhash-src")))

              (mkdir-p "/tmp/fft2d")
              (with-directory-excursion "/tmp/fft2d"
                (invoke "tar" "--strip-components=1"
                        "-xf" (assoc-ref inputs "fft2d-src")))

              (copy-recursively (assoc-ref inputs "gemmlowp-src")
                                "/tmp/gemmlowp/")))

          (add-after 'build 'build-shared-library
            (lambda* (#:key configure-flags #:allow-other-keys)
              (mkdir-p "c")
              (with-directory-excursion "c"
                (apply invoke "cmake" (append configure-flags (list "../../lite/c")))
                (invoke "cmake" "--build" "." "-j" (number->string
                                                    (parallel-job-count))))))
          (add-after 'build-shared-library 'build-benchmark-model
            (lambda _
              (invoke "cmake" "--build" "." "--target" "benchmark_model"
                      "-j" (number->string (parallel-job-count)))))

          (add-after 'install 'install-extra
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (lib (string-append out "/lib"))
                     (bin (string-append out "/bin")))
                (install-file "../build/c/libtensorflowlite_c.so" lib)
                (install-file "../build/tools/benchmark/benchmark_model" bin))))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "ctest" "-L" "plain")))))))
    (inputs
     `(("abseil-cpp" ,abseil-cpp-20200923.3)
       ("cpuinfo" ,cpuinfo)
       ("eigen" ,eigen)
       ("fp16" ,fp16)
       ("flatbuffers-shared" ,flatbuffers-next-shared)
       ;;("gemmlowp" ,gemmlowp)  ; TODO
       ("mesa-headers" ,mesa-headers)
       ("neon2sse" ,neon2sse)
       ("nsync" ,nsync)
       ("opencl-clhpp" ,opencl-clhpp)
       ("opencl-headers" ,opencl-headers)
       ("opencl-icd-loader" ,opencl-icd-loader)
       ("pthreadpool" ,pthreadpool)
       ("python" ,python)
       ("ruy" ,ruy)
       ("re2" ,re2)
       ;;("xnnpack" ,xnnpack)     ; TODO: use Guix's copy of xnnpack
       ("vulkan-headers" ,vulkan-headers)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("googletest" ,googletest)
       ("gemmlowp-src"
        ;; The commit hash is taken from
        ;; "tensorflow/lite/tools/cmake/modules/gemmlowp.cmake".
        ,(let ((commit "fda83bdc38b118cc6b56753bd540caa49e570745"))
           (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/google/gemmlowp")
                   (commit commit)))
             (file-name (git-file-name "gemmlowp" (string-take commit 8)))
             (sha256
              (base32
               "1sbp8kmr2azwlvfbzryy1frxi99jhsh1nc93bdbxdf8zdgpv0kxl")))))
       ("farmhash-src"
        ,(let ((commit "816a4ae622e964763ca0862d9dbd19324a1eaf45"))
           (origin
             (method url-fetch)
             (uri (string-append
                   "https://mirror.bazel.build/github.com/google/farmhash/archive/"
                   commit ".tar.gz"))
             (file-name (git-file-name "farmhash" (string-take commit 8)))
             (sha256
              (base32
               "185b2xdxl4d4cnsnv6abg8s22gxvx8673jq2yaq85bz4cdy58q35")))))
       ("fft2d-src"
        ,(origin
           (method url-fetch)
           (uri (string-append "https://storage.googleapis.com/"
                               "mirror.tensorflow.org/github.com/petewarden/"
                               "OouraFFT/archive/v1.0.tar.gz"))
           (file-name "fft2d.tar.gz")
           (sha256
            (base32
             "1jfflzi74fag9z4qmgwvp90aif4dpbr1657izmxlgvf4hy8fk9xd"))))))
    (home-page "https://tensorflow.org")
    (synopsis "Machine learning framework")
    (description
     "TensorFlow is a flexible platform for building and training machine
learning models.  This package provides the \"lite\" variant for mobile
devices.")
    (license license:asl2.0)))

(define-public dmlc-core
  (package
    (name "dmlc-core")
    (version "0.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dmlc/dmlc-core")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1x4ad1jhn84fywlk031fmv1kxyiscclmrqn9hhj8gz0mh7z9vcrh"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       (list "-DGOOGLE_TEST=ON")))
    (native-inputs
     `(("googletest" ,googletest)
       ("python" ,python-wrapper)))
    (home-page "https://github.com/dmlc/dmlc-core")
    (synopsis "Common bricks library for machine learning")
    (description
     "DMLC-Core is the backbone library to support all DMLC projects,
offers the bricks to build efficient and scalable distributed machine
learning libraries.")
    (license license:asl2.0)))

(define-public xgboost
  (package
    (name "xgboost")
    (version "1.5.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dmlc/xgboost")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (patches (search-patches "xgboost-use-system-dmlc-core.patch"))
       (sha256
        (base32 "0qx04y7cz8z7qv6bk9q7d7ba9b7xzj53l83l2x9ykdwhzacc3dn0"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags (list "-DGOOGLE_TEST=ON")))
    (native-inputs
     `(("googletest" ,googletest)
       ("python" ,python-wrapper)))
    (inputs
     (list dmlc-core))
    (home-page "https://xgboost.ai/")
    (synopsis "Gradient boosting (GBDT, GBRT or GBM) library")
    (description
     "XGBoost is an optimized distributed gradient boosting library designed
to be highly efficient, flexible and portable.  It implements machine learning
algorithms under the Gradient Boosting framework.  XGBoost provides a parallel
tree boosting (also known as GBDT, GBM) that solve many data science problems
in a fast and accurate way.")
    (license license:asl2.0)))

(define-public python-xgboost
  (package
    (inherit xgboost)
    (name "python-xgboost")
    (source (package-source xgboost))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'preparations
           (lambda _
             ;; Move python-package content to parent directory to silence
             ;; some warnings about files not being found if we chdir.
             (rename-file "python-package/xgboost" "xgboost")
             (rename-file "python-package/README.rst" "README.rst")
             (rename-file "python-package/setup.cfg" "setup.cfg")
             (rename-file "python-package/setup.py" "setup.py")
             ;; Skip rebuilding libxgboost.so.
             (substitute* "setup.py"
               (("ext_modules=\\[CMakeExtension\\('libxgboost'\\)\\],") "")
               (("'install_lib': InstallLib,") ""))))
         (add-after 'install 'install-version-and-libxgboost
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (pylib (string-append out "/lib/python"
                                          ,(version-major+minor
                                            (package-version python))
                                          "/site-packages"))
                    (xgbdir (string-append pylib "/xgboost"))
                    (version-file (string-append xgbdir "/VERSION"))
                    (libxgboost (string-append (assoc-ref inputs "xgboost")
                                               "/lib/libxgboost.so")))
               (with-output-to-file version-file
                 (lambda ()
                   (display ,(package-version xgboost))))
               (mkdir-p (string-append xgbdir "/lib"))
               (symlink libxgboost (string-append xgbdir "/lib"
                                                  "/libxgboost.so")))))
         (replace 'check
           ;; Python-specific tests are located in tests/python.
           (lambda* (#:key inputs outputs tests? #:allow-other-keys)
             (when tests?
               (add-installed-pythonpath inputs outputs)
               (invoke "pytest" "tests/python"
                       ;; FIXME: CLI tests fail with PermissionError.
                       "--ignore" "tests/python/test_cli.py" "-k"
                       (string-append
                        "not test_cli_regression_demo"
                        ;; The tests below open a network connection.
                        " and not test_model_compatibility"
                        " and not test_get_group"
                        " and not test_cv_no_shuffle"
                        " and not test_cv"
                        " and not test_training"
                        ;; "'['./runexp.sh']' returned non-zero exit status 1"
                        " and not test_cli_binary_classification"))))))))
    (native-inputs
     (list python-pandas python-pytest python-scikit-learn))
    (inputs
     (list xgboost))
    (propagated-inputs
     (list python-numpy python-scipy))
    (synopsis "Python interface for the XGBoost library")))

(define-public python-iml
  (package
    (name "python-iml")
    (version "0.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "iml" version))
       (sha256
        (base32
         "1k8szlpm19rcwcxdny9qdm3gmaqq8akb4xlvrzyz8c2d679aak6l"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-ipython python-numpy python-pandas python-scipy))
    (native-inputs
     (list python-nose))
    (home-page "https://github.com/interpretable-ml/iml")
    (synopsis "Interpretable Machine Learning (iML) package")
    (description "Interpretable ML (iML) is a set of data type objects,
visualizations, and interfaces that can be used by any method designed to
explain the predictions of machine learning models (or really the output of
any function).  It currently contains the interface and IO code from the Shap
project, and it will potentially also do the same for the Lime project.")
    (license license:expat)))

(define-public python-keras-applications
  (package
    (name "python-keras-applications")
    (version "1.0.8")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "Keras_Applications" version))
       (sha256
        (base32
         "1rcz31ca4axa6kzhjx4lwqxbg4wvlljkj8qj9a7p9sfd5fhzjyam"))))
    (build-system python-build-system)
    ;; The tests require Keras, but this package is needed to build Keras.
    (arguments '(#:tests? #f))
    (propagated-inputs
     (list python-h5py python-numpy))
    (native-inputs
     (list python-pytest python-pytest-cov python-pytest-pep8
           python-pytest-xdist))
    (home-page "https://github.com/keras-team/keras-applications")
    (synopsis "Reference implementations of popular deep learning models")
    (description
     "This package provides reference implementations of popular deep learning
models for use with the Keras deep learning framework.")
    (license license:expat)))

(define-public python-keras-preprocessing
  (package
    (name "python-keras-preprocessing")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "Keras_Preprocessing" version))
       (sha256
        (base32
         "1r98nm4k1svsqjyaqkfk23i31bl1kcfcyp7094yyj3c43phfp3as"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-numpy python-six))
    (native-inputs
     (list python-pandas
           python-pillow
           python-pytest
           python-pytest-cov
           python-pytest-xdist
           tensorflow))
    (home-page "https://github.com/keras-team/keras-preprocessing/")
    (synopsis "Data preprocessing and augmentation for deep learning models")
    (description
     "Keras Preprocessing is the data preprocessing and data augmentation
module of the Keras deep learning library.  It provides utilities for working
with image data, text data, and sequence data.")
    (license license:expat)))

(define-public python-keras
  (package
    (name "python-keras")
    (version "2.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "Keras" version))
       (sha256
        (base32
         "1k68xd8n2y9ldijggjc8nn4d6d1axw0p98gfb0fmm8h641vl679j"))
       (modules '((guix build utils)))
       (snippet
        '(substitute* '("keras/callbacks/callbacks.py"
                        "keras/engine/training_utils.py"
                        "keras/engine/training.py"
                        "keras/engine/training_generator.py"
                        "keras/utils/generic_utils.py")
           (("from collections import Iterable")
            "from collections.abc import Iterable")
           (("collections.Container")
            "collections.abc.Container")
           (("collections.Mapping")
            "collections.abc.Mapping")
           (("collections.Sequence")
            "collections.abc.Sequence")))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'tf-compatibility
           (lambda _
             (substitute* "keras/backend/tensorflow_backend.py"
               (("^get_graph = .*")
                "get_graph = tf.get_default_graph")
               (("tf.compat.v1.nn.fused_batch_norm")
                "tf.nn.fused_batch_norm")
               ;; categorical_crossentropy does not support axis
               (("from_logits=from_logits, axis=axis")
                "from_logits=from_logits")
               ;; dropout accepts a level number, not a named rate argument.
               (("dropout\\(x, rate=level,")
                "dropout(x, level,")
               (("return x.shape.rank")
                "return len(x.shape)"))))
         (add-after 'unpack 'hdf5-compatibility
           (lambda _
             ;; The truth value of an array with more than one element is ambiguous.
             (substitute* "tests/keras/utils/io_utils_test.py"
               ((" *assert .* == \\[b'(asd|efg).*") ""))
             (substitute* "tests/test_model_saving.py"
               (("h5py.File\\('does not matter',")
                "h5py.File('does not matter', 'w',"))
             (substitute* "keras/utils/io_utils.py"
               (("h5py.File\\('in-memory-h5py', driver='core', backing_store=False\\)")
                "h5py.File('in-memory-h5py', 'w', driver='core', backing_store=False)")
               (("h5file.fid.get_file_image")
                "h5file.id.get_file_image"))
             (substitute* "keras/engine/saving.py"
               (("\\.decode\\('utf-?8'\\)") ""))))
         (add-after 'unpack 'delete-unavailable-backends
           (lambda _
             (delete-file "keras/backend/theano_backend.py")
             (delete-file "keras/backend/cntk_backend.py")))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               ;; These tests attempt to download data files from the internet.
               (delete-file "tests/integration_tests/test_datasets.py")
               (delete-file "tests/integration_tests/imagenet_utils_test.py")
               (invoke "python" "-m" "pytest" "tests"
                       "-p" "no:pep8"
                       ;; FIXME: python-build-system lacks PARALLEL-TESTS?
                       "-n" (number->string (parallel-job-count))
                       ;; This one uses the theano backend that we don't have.
                       "--ignore=tests/test_api.py"
                       "--ignore=tests/keras/backend/backend_test.py"
                       ;; Our Tensorflow version does not have the coder ops library.
                       "--ignore=tests/keras/callbacks/callbacks_test.py"
                       ;; ...nor do we have tensorboard
                       "--ignore=tests/keras/callbacks/tensorboard_test.py"
                       "-k"
                       (string-append
                        ;; See https://github.com/keras-team/keras/pull/7033
                        "not test_TimeDistributed_learning_phase "
                        ;; XXX fails because no closure is provided
                        "and not test_func_dump_and_load_backwards_compat "
                        ;; XXX real bug?  These are all tests that fail due to
                        ;; shape mismatch, e.g. "got logits shape [12,3] and
                        ;; labels shape [9]"
                        "and not test_model_with_crossentropy_losses_channels_first "
                        "and not test_masking_correctness_output_size_not_equal_to_first_state_size "
                        "and not test_convolutional_recurrent "
                        "and not test_axis "

                        ;; XXX fails because of 3/15 values have unexpected differences.
                        "and not test_masking_correctness_output_not_equal_to_first_state "
                        ;; XXX fails because of a difference of about 0.1
                        "and not test_sample_weighted "
                        ;; XXX fails because of a difference of about 0.3
                        "and not test_scalar_weighted "
                        ;; XXX fails because of a difference of about 0.2
                        "and not test_unweighted "

                        ;; XXX I cannot reproduce this in an interactive
                        ;; Python session, because l2_norm works just fine.
                        "and not test_weighted " ;TestCosineSimilarity
                        "and not test_config "   ;TestCosineSimilarity

                        ;; The following test fails only in the build
                        ;; container; skip it.
                        "and not test_selu "
                        ;; The following test was found flaky and removed in
                        ;; recent versions.
                        "and not test_stateful_metrics"))))))))
    (propagated-inputs
     (list python-h5py
           python-keras-applications
           python-keras-preprocessing
           python-numpy
           python-pydot
           python-pyyaml
           python-scipy
           python-six
           tensorflow
           graphviz))
    (native-inputs
     (list python-flaky
           python-markdown
           python-pandas
           python-pytest
           python-pytest-cov
           python-pytest-pep8
           python-pytest-timeout
           python-pytest-xdist
           python-pyux
           python-sphinx
           python-requests))
    (home-page "https://github.com/keras-team/keras")
    (synopsis "High-level deep learning framework")
    (description "Keras is a high-level neural networks API, written in Python
and capable of running on top of TensorFlow.  It was developed with a focus on
enabling fast experimentation.  Use Keras if you need a deep learning library
that:
@itemize
@item Allows for easy and fast prototyping (through user friendliness,
  modularity, and extensibility).
@item Supports both convolutional networks and recurrent networks, as well as
  combinations of the two.
@item Runs seamlessly on CPU and GPU.
@end itemize\n")
    (license license:expat)))

(define-public gloo
  (let ((version "0.0.0")                         ; no proper version tag
        (commit "c22a5cfba94edf8ea4f53a174d38aa0c629d070f")
        (revision "1"))
    (package
      (name "gloo")
      (version (git-version version revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/facebookincubator/gloo")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1crmqgybzkgkpbmcx16912gsl5qsj49swa0ikx6mhqgph0chrh11"))))
      (build-system cmake-build-system)
      (native-inputs
       (list googletest))
      (inputs
       (append (list openssl-1.1)
               (if (supported-package? rdma-core)
                   (list rdma-core)
                   '())))
      (arguments
       (list #:configure-flags #~'("-DBUILD_SHARED_LIBS=ON"
                                   "-DBUILD_TEST=1"
                                   #$@(if (this-package-input "rdma-core")
                                          #~("-DUSE_IBVERBS=ON")
                                          #~()))
             #:phases
             #~(modify-phases %standard-phases
                 (replace 'check
                   (lambda* (#:key tests? #:allow-other-keys)
                     (when tests?
                       (invoke "make" "gloo_test")))))))
      (synopsis "Collective communications library")
      (description
       "Gloo is a collective communications library.  It comes with a
number of collective algorithms useful for machine learning applications.
These include a barrier, broadcast, and allreduce.")
      (home-page "https://github.com/facebookincubator/gloo")
      (license license:bsd-3))))

(define-public python-tensorly
  (package
    (name "python-tensorly")
    (version "0.8.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/tensorly/tensorly")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "184mvs1gwycsh2f8jgdyc3jyhiylbn4xw2srpjd264dz2l9ms2l7"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      '(list
        ;; These tests fail due to missing example, documentation, or tutorial files.
        "--ignore=doc/sphinx_ext/sphinx_gallery/tests/test_gen_rst.py"
        ;; XXX There is no "get_marker" method.
        "--ignore=doc/sphinx_ext/sphinx_gallery/tests/test_gen_gallery.py"
        "-k"
        (string-append
         ;; tutorials/plot_parse.py is not included
         "not test_jupyter_notebook"
         ;; nor is examples/plot_quantum.py
         " and not test_file_is_generated"))))
    (propagated-inputs (list python-jsmin python-numpy python-scipy))
    (native-inputs (list python-pytest python-pytest-cov python-sphinx))
    (home-page "https://github.com/tensorly/tensorly")
    (synopsis "Tensor learning in Python")
    (description
     "This is a Python library that aims at making tensor learning simple and
accessible.  It allows performing tensor decomposition, tensor learning and
tensor algebra easily.  Its backend system allows seamlessly perform
computation with NumPy, PyTorch, JAX, MXNet, TensorFlow or CuPy and run
methodxs at scale on CPU or GPU.")
    (license license:bsd-3)))

(define-public python-umap-learn
  (package
    (name "python-umap-learn")
    (version "0.5.3")
    (source
     (origin
       (method git-fetch)               ;no tests in pypi release
       (uri (git-reference
             (url "https://github.com/lmcinnes/umap")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1315jkb0h1b579y9m59632f0nnpksilm01nxx46in0rq8zna8vsb"))))
    (build-system python-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'numpy-compatibility
            (lambda _
              (substitute* "umap/tests/test_umap_metrics.py"
                ;; See commit a714b59bd9e2ca2e63312bc3491b2b037a42f2f2
                (("sparse_binary_data.todense\\(\\),")
                 "np.asarray(sparse_binary_data.todense()),")
                ;; See commit c7d05683325589ad432a55e109cacb9d631cfaa9
                (("sparse_spatial_data.todense\\(\\),")
                 "np.asarray(sparse_spatial_data.todense()),"))
              ;; See commit 949abd082524fce8c45dfb147bcd8e8ef49eade3
              (substitute* "umap/tests/test_umap_ops.py"
                (("np.random,") "None,"))))
          ;; Numba needs a writable dir to cache functions.
          (add-before 'check 'set-numba-cache-dir
            (lambda _
              (setenv "NUMBA_CACHE_DIR" "/tmp")))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (setenv "HOME" "/tmp")
                (invoke "pytest" "-vv" "umap"
                        ;; This test can fail because trust may only be
                        ;; 0.9679405204460967 >= 0.97
                        "-k" "not test_densmap_trustworthiness_on_iris_supervised")))))))
    (native-inputs (list python-pytest))
    (propagated-inputs
     (list python-numba
           python-numpy
           python-pynndescent
           python-scikit-learn
           python-scipy
           python-tqdm))
    (home-page "https://github.com/lmcinnes/umap")
    (synopsis "Uniform Manifold Approximation and Projection")
    (description "Uniform Manifold Approximation and Projection is a dimension
reduction technique that can be used for visualization similarly to t-SNE, but
also for general non-linear dimension reduction.")
    (license license:bsd-3)))

(define-public nnpack
  (let ((version "0.0")
        (commit "c07e3a0400713d546e0dea2d5466dd22ea389c73")
        (revision "1"))
    (package
      (name "nnpack")
      (version (git-version version revision commit))
      (home-page "https://github.com/Maratyszcza/NNPACK")
      (source (origin
                (method git-fetch)
                (uri (git-reference (url home-page) (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0s0kk3a35w3yzf0q447p72350sbsh4qhg6vm3y2djbj4xpg7jc8v"))
                (patches (search-patches "nnpack-system-libraries.patch"))))
      (build-system cmake-build-system)
      ;; XXX: The test suite runs but it's very expensive, and on x86_64 CPUs
      ;; that lack the right ISA extensions, tests fail with:
      ;;
      ;; Expected equality of these values:
      ;;   nnp_status_success
      ;;     Which is: 0
      ;;   status
      ;;     Which is: 51
      ;;
      ;; where 51 is 'nnp_status_unsupported_hardware'.
      (arguments '(#:tests? #f))
      (synopsis "Acceleration package for neural network computations")
      (description
       "NNPACK is an acceleration package for neural network computations.
NNPACK aims to provide high-performance implementations of convnet layers for
multi-core CPUs.

NNPACK is not intended to be directly used by machine learning researchers;
instead it provides low-level performance primitives leveraged in leading deep
learning frameworks, such as PyTorch, Caffe2, MXNet, tiny-dnn, Caffe, Torch,
and Darknet.")
      (inputs
       (list cpuinfo
             fp16
             fxdiv
             psimd
             pthreadpool
             googletest))
      (native-inputs
       (list python python-peachpy python-six))
      (license license:bsd-2))))

(define-public xnnpack
  ;; There's currently no tag on this repo.
  (let ((version "0.0")
        (commit "ae108ef49aa5623b896fc93d4298c49d1750d9ba")
        (revision "2"))
    (package
      (name "xnnpack")
      (version (git-version version revision commit))
      (home-page "https://github.com/google/XNNPACK") ;fork of QNNPACK
      (source (origin
                (method git-fetch)
                (uri (git-reference (url home-page) (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0q68q2jxiiiblx45q4337k13ppgh5vqjwrwznchcnpb8hawjj3zl"))
                (patches (search-patches "xnnpack-system-libraries.patch"))))
      (build-system cmake-build-system)
      (arguments
       '(#:configure-flags '("-DXNNPACK_USE_SYSTEM_LIBS=YES"
                             "-DBUILD_SHARED_LIBS=ON"
                             "-DXNNPACK_LIBRARY_TYPE=shared"
                             "-DXNNPACK_BUILD_TESTS=FALSE" ;FIXME: see below
                             "-DXNNPACK_BUILD_BENCHMARKS=FALSE")

         ;; FIXME: Building tests leads to a CMake error:
         ;;
         ;;   ADD_LIBRARY cannot create target "all_microkernels" because
         ;;   another target with the same name already exists.
         #:tests? #f))
      (inputs
       (list cpuinfo
             pthreadpool
             googletest
             googlebenchmark
             fxdiv
             fp16
             psimd))
      (synopsis "Optimized floating-point neural network inference operators")
      (description
       "XNNPACK is a highly optimized library of floating-point neural network
inference operators for ARM, WebAssembly, and x86 platforms.  XNNPACK is not
intended for direct use by deep learning practitioners and researchers;
instead it provides low-level performance primitives for accelerating
high-level machine learning frameworks, such as TensorFlow Lite,
TensorFlow.js, PyTorch, and MediaPipe.")
      (license license:bsd-3))))

;; Please also update python-torchvision when updating this package.
(define-public python-pytorch
  (package
    (name "python-pytorch")
    (version "1.13.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/pytorch/pytorch")
                    (commit (string-append "v" version))
                    (recursive? #t)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "17yxjzwp4zp75fz7czgz9acijzw7dpyqcza50v8y1x7hfg2gw369"))
              (patches (search-patches "python-pytorch-system-libraries.patch"
                                       "python-pytorch-runpath.patch"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; XXX: Let's be clear: this package is a bundling fest.  We
                  ;; delete as much as we can, but there's still a lot left.
                  (for-each (lambda (directory)
                              (delete-file-recursively
                               (string-append "third_party/" directory)))
                            '("benchmark" "cpuinfo" "eigen"

                              ;; FIXME: QNNPACK (of which XNNPACK is a fork)
                              ;; needs these.
                              ;; "FP16" "FXdiv" "gemmlowp" "psimd"

                              "gloo" "googletest" "ios-cmake" "NNPACK"
                              "onnx" "protobuf" "pthreadpool"
                              "pybind11" "python-enum" "python-peachpy"
                              "python-six" "tbb" "XNNPACK" "zstd"))
                  (substitute* "functorch/CMakeLists.txt"
                    (("\\$\\{_rpath_portable_origin\\}/../torch/lib")
                     "$ORIGIN/../torch/lib"))))))
    (build-system python-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (add-before 'build 'use-system-libraries
                    (lambda* (#:key outputs #:allow-other-keys)
                      ;; Tell 'setup.py' to let 'CMakeLists.txt' know that we
                      ;; want to use "system libraries" instead of the bundled
                      ;; ones.
                      (setenv "USE_SYSTEM_LIBS" "1")

                      (substitute* "cmake/Dependencies.cmake"
                        (("if\\(USE_SYSTEM_BIND11\\)")
                         "if(TRUE)"))

                      ;; XXX: Disable that for simplicity for now.
                      (setenv "USE_FBGEMM" "0")))
                  (add-before 'build 'make-things-writable
                    (lambda _
                      ;; The 'build_caffe2' function in
                      ;; 'tools/build_pytorch_libs.py', called from the
                      ;; top-level 'setup.py', needs write access to this
                      ;; directory.
                      (for-each make-file-writable
                                (find-files "caffe2/proto" "."
                                            #:directories? #t))))
                  (replace 'check
                    (lambda* (#:key inputs outputs tests? #:allow-other-keys)
                      ;; Run the test suite following the instructions in
                      ;; 'CONTRIBUTING.md'.  XXX: Unfortunately this doesn't
                      ;; work, unless you set GUIX_PYTHONPATH presumably.
                      (when tests?
                        (add-installed-pythonpath inputs outputs)
                        (invoke "python" "test/run_test.py"))))
                  (add-after 'install 'remove-test-executables
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      ;; Remove test executables, but keep other executables
                      ;; such as 'torch_shm_manager' and and .so files such as
                      ;; 'libtorch_global_deps.so'.
                      (let ((python-site (site-packages inputs outputs)))
                        (for-each delete-file
                                  (find-files python-site
                                              "(^test_cpp_rpc|_test)$")))))
                  (add-after 'install 'remove-caffe2-onnx-scripts
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (bin (string-append out "/bin")))
                        ;; Remove 'convert-caffe2-to-onnx' and
                        ;; 'convert-onnx-to-caffe2': they seem to be
                        ;; deprecated and they cause a failure of the
                        ;; 'sanity-check' phase:
                        ;;
                        ;; ImportError: cannot import name 'metanet_pb2' from partially initialized module 'caffe2.proto' (most likely due to a circular import)
                        (for-each delete-file
                                  (find-files bin "^convert-.*caffe2"))

                        (substitute* (find-files out "^entry_points\\.txt$")
                          (("^convert-.*" all)
                           (string-append "# " all "\n")))))))

       ;; XXX: Tests attempt to download data such as
       ;; <https://raw.githubusercontent.com/pytorch/test-infra/master/stats/slow-tests.json>.
       ;; We're also missing some Python modules, such as expecttest.
       #:tests? #f))
    (native-inputs
     (list cmake ninja))
    (inputs
     (list eigen
           ;; ("fmt" ,fmt)
           fp16
           gemmlowp
           googletest
           googlebenchmark
           gloo
           nnpack
           openblas
           openmpi
           pthreadpool
           protobuf
           pybind11
           sleef
           xnnpack
           zstd))
    (propagated-inputs
     (list python-astunparse
           python-click
           python-numpy
           python-pyyaml
           python-cffi
           python-typing-extensions
           python-future
           python-six
           python-requests
           onnx                             ;propagated for its Python modules
           onnx-optimizer
           cpuinfo))
    (home-page "https://pytorch.org/")
    (synopsis "Python library for tensor computation and deep neural networks")
    (description
     "PyTorch is a Python package that provides two high-level features:

@itemize
@item tensor computation (like NumPy) with strong GPU acceleration;
@item deep neural networks (DNNs) built on a tape-based autograd system.
@end itemize

You can reuse Python packages such as NumPy, SciPy, and Cython to extend
PyTorch when needed.

Note: currently this package does not provide GPU support.")
    (license license:bsd-3)))

(define-public python-pytorch-for-r-torch python-pytorch)

(define-public python-lightning-cloud
  (package
    (name "python-lightning-cloud")
    (version "0.5.34")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "lightning_cloud" version))
              (sha256
               (base32
                "0mqrhq3s23mn8n4i0q791pshn3dgplp0h9ny0pmmp798q0798dzs"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-click
                             python-fastapi-for-pytorch-lightning
                             python-multipart
                             python-pyjwt
                             python-requests
                             python-rich
                             python-six
                             python-urllib3
                             python-uvicorn
                             python-websocket-client))
    (home-page "https://lightning.ai")
    (synopsis "Lightning Cloud command line client")
    (description "This package provides a command line interface for Lightning
AI services.")
    (license license:asl2.0)))

(define-public python-lightning-utilities
  (package
    (name "python-lightning-utilities")
    (version "0.8.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "lightning-utilities" version))
              (sha256
               (base32
                "084pn8fizxrcn1699jb8x2jsg4wcx01l65bwxpgnq0kzqp3rapcf"))))
    (build-system python-build-system)
    (propagated-inputs (list python-importlib-metadata python-packaging
                             python-typing-extensions))
    (native-inputs (list python-coverage))
    (home-page "https://github.com/Lightning-AI/utilities")
    (synopsis "PyTorch Lightning sample project")
    (description "This package provides common Python utilities and GitHub
Actions for the Lightning suite of libraries.")
    (license license:asl2.0)))

(define-public python-captum
  (package
    (name "python-captum")
    (version "0.6.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/pytorch/captum")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1h4n91ivhjxm6wj0vgqpfss2dmq4sjcp0appd08cd5naisabjyb5"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      '(list "-k"
             ;; These two tests (out of more than 1000 tests) fail because of
             ;; accuracy problems.
             "not test_softmax_classification_batch_multi_target\
 and not test_softmax_classification_batch_zero_baseline")))
    (propagated-inputs (list python-matplotlib python-numpy python-pytorch))
    (native-inputs (list jupyter
                         python-annoy
                         python-black
                         python-flake8
                         python-flask
                         python-flask-compress
                         python-ipython
                         python-ipywidgets
                         python-mypy
                         python-parameterized
                         python-pytest
                         python-pytest-cov
                         python-scikit-learn))
    (home-page "https://captum.ai")
    (synopsis "Model interpretability for PyTorch")
    (description "Captum is a model interpretability and understanding library
for PyTorch.  Captum contains general purpose implementations of integrated
gradients, saliency maps, smoothgrad, vargrad and others for PyTorch models.
It has quick integration for models built with domain-specific libraries such
as torchvision, torchtext, and others.")
    (license license:bsd-3)))

(define-public python-readchar
  (package
    (name "python-readchar")
    (version "4.0.5")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "readchar" version))
              (sha256
               (base32
                "09n8vl2jjbnbnrzfvkynijrnwrqvc91bb2267zg8r261sz15d908"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      '(modify-phases %standard-phases
         ;; This one file requires the msvcrt module, which we don't have.
         (add-after 'unpack 'delete-windows-file
           (lambda _
             (delete-file "readchar/_win_read.py"))))))
    (propagated-inputs (list python-setuptools))
    (home-page "https://github.com/magmax/python-readchar")
    (synopsis "Library to easily read single chars and key strokes")
    (description "This package provides a Python library to easily read single
characters and key strokes.")
    (license license:expat)))

(define-public python-inquirer
  (package
    (name "python-inquirer")
    (version "3.1.3")
    ;; Pypi has no tests.
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/magmax/python-inquirer")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0kp6a0535n9ra5sk8bmb5qvhrv0fbn1zawydi0fkb7104jqcfrzc"))))
    (build-system pyproject-build-system)
    (propagated-inputs
     (list python-blessed python-editor python-readchar))
    (native-inputs
     (list python-poetry-core python-pexpect python-pytest))
    (home-page "https://github.com/magmax/python-inquirer")
    (synopsis "Collection of common interactive command line user interfaces")
    (description
     "Inquirer should ease the process of asking end user questions, parsing,
validating answers, managing hierarchical prompts and providing error
feedback.")
    (license license:expat)))

(define-public python-pytorch-lightning
  (package
    (name "python-pytorch-lightning")
    (version "2.0.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Lightning-AI/lightning")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1w4lajiql4y5nnhqf6i5wii1mrwnhp5f4bzbwdzb5zz0d0lysb1i"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      '(list "-m" "not cloud and not tpu" "tests/tests_pytorch"
             ;; we don't have onnxruntime
             "--ignore=tests/tests_pytorch/models/test_onnx.py"

             ;; We don't have tensorboard, so we skip all those tests that
             ;; require it for logging.
             "--ignore=tests/tests_pytorch/checkpointing/test_model_checkpoint.py"
             "--ignore=tests/tests_pytorch/loggers/test_all.py"
             "--ignore=tests/tests_pytorch/loggers/test_logger.py"
             "--ignore=tests/tests_pytorch/loggers/test_tensorboard.py"
             "--ignore=tests/tests_pytorch/models/test_cpu.py"
             "--ignore=tests/tests_pytorch/models/test_hparams.py"
             "--ignore=tests/tests_pytorch/models/test_restore.py"
             "--ignore=tests/tests_pytorch/profilers/test_profiler.py"
             "--ignore=tests/tests_pytorch/trainer/flags/test_fast_dev_run.py"
             "--ignore=tests/tests_pytorch/trainer/logging_/test_eval_loop_logging.py"
             "--ignore=tests/tests_pytorch/trainer/logging_/test_train_loop_logging.py"
             "--ignore=tests/tests_pytorch/trainer/properties/test_loggers.py"
             "--ignore=tests/tests_pytorch/trainer/properties/test_log_dir.py"
             "--ignore=tests/tests_pytorch/trainer/test_trainer.py"

             ;; This needs internet access
             "--ignore=tests/tests_pytorch/helpers/test_models.py"
             "--ignore=tests/tests_pytorch/helpers/test_datasets.py"
             "--ignore=tests/tests_pytorch/helpers/datasets.py"

             ;; We have no legacy checkpoints
             "--ignore=tests/tests_pytorch/checkpointing/test_legacy_checkpoints.py"

             ;; TypeError: _FlakyPlugin._make_test_flaky() got an unexpected keyword argument 'reruns'
             "--ignore=tests/tests_pytorch/models/test_amp.py"
             "--ignore=tests/tests_pytorch/profilers/test_profiler.py"

             "--ignore=tests/tests_pytorch/graveyard/test_legacy_import_unpickler.py"

             "-k"
             (string-append
              ;; We don't have tensorboard
              "not test_property_logger"
              " and not test_cli_logger_shorthand"
              ;; Something wrong with Flaky
              " and not test_servable_module_validator_with_trainer"))
      #:phases
      '(modify-phases %standard-phases
         (add-after 'unpack 'patch-version-detection
           (lambda _
             ;; We do have pytorch 1.13.1, but the version comparison fails.
             (substitute* "src/lightning/fabric/utilities/imports.py"
               (("_TORCH_GREATER_EQUAL_1_13 =.*")
                "_TORCH_GREATER_EQUAL_1_13 = True\n"))))
         (add-before 'build 'pre-build
           (lambda _ (setenv "PACKAGE_NAME" "lightning")))
         (add-after 'install 'pre-build-pytorch
           (lambda _
             ;; pyproject-build-system only tolerates unicycles.
             (for-each delete-file (find-files "dist" "\\.whl"))
             (setenv "PACKAGE_NAME" "pytorch")))
         (add-after 'pre-build-pytorch 'build-pytorch
           (assoc-ref %standard-phases 'build))
         (add-after 'build-pytorch 'install-pytorch
           (assoc-ref %standard-phases 'install))
         (add-before 'check 'pre-check
           (lambda _
             ;; We don't have Tensorboard
             (substitute* "tests/tests_pytorch/test_cli.py"
               ((" TensorBoardLogger\\(\".\"\\)") "")))))))
    (propagated-inputs
     (list python-arrow
           python-beautifulsoup4
           python-croniter
           python-dateutils
           python-deepdiff
           python-fastapi-for-pytorch-lightning
           python-fsspec
           python-inquirer
           python-jsonargparse
           python-lightning-cloud
           python-lightning-utilities
           python-numpy
           python-packaging
           python-pytorch
           python-pyyaml
           python-starsessions-for-pytorch-lightning
           python-torchmetrics
           python-torchvision
           python-tqdm
           python-traitlets
           python-typing-extensions))
    (native-inputs
     (list python-aiohttp
           python-cloudpickle
           python-coverage
           python-flaky
           python-pympler
           python-pytest
           python-psutil
           python-requests-mock
           python-scikit-learn))
    (home-page "https://lightning.ai/")
    (synopsis "Deep learning framework to train, deploy, and ship AI products")
    (description
     "PyTorch Lightning is just organized PyTorch; Lightning disentangles
PyTorch code to decouple the science from the engineering.")
    (license license:asl2.0)))

(define-public python-torchmetrics
  (package
    (name "python-torchmetrics")
    (version "0.11.4")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "torchmetrics" version))
              (sha256
               (base32
                "150lcy6c20n42rwxl4d3m1b8s4js9ddds5wh3685vmjdnha5mr0z"))))
    (build-system pyproject-build-system)
    (propagated-inputs
     (list python-numpy python-packaging python-pytorch
           python-typing-extensions))
    (native-inputs
     (list python-cloudpickle
           python-coverage
           python-fire
           python-mir-eval
           python-mypy
           python-pandas
           python-psutil
           python-pytest
           python-pytest-cov
           python-pytest-doctestplus
           python-pytest-rerunfailures
           python-pytest-timeout
           python-requests
           python-scikit-image
           python-scikit-learn
           python-scipy
           python-types-protobuf
           python-types-setuptools))
    (home-page "https://github.com/Lightning-AI/metrics")
    (synopsis "Machine learning metrics for PyTorch applications")
    (description "TorchMetrics is a collection of 100+ PyTorch metrics
implementations and an easy-to-use API to create custom metrics.  It offers:

@itemize
@item A standardized interface to increase reproducibility
@item Reduces boilerplate
@item Automatic accumulation over batches
@item Metrics optimized for distributed-training
@item Automatic synchronization between multiple devices
@end itemize
")
    (license license:asl2.0)))

;; Keep this in sync with python-pytorch
(define-public python-torchvision
  (package
    (name "python-torchvision")
    (version "0.15.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/pytorch/vision")
                    (commit (string-append "v" version))
                    (recursive? #t)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1cq2s13vkgg9rljjbrm4g33yxq7q5zqp7f4xm5cq624gvs0wxmi8"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:tests? #false)) ;the test suite is expensive and there is no easy
                             ;way to subset it.
    (inputs
     (list libpng
           libjpeg-turbo))
    (propagated-inputs
     (list python-numpy
           python-typing-extensions
           python-requests
           python-pillow
           python-pillow-simd
           python-pytorch))
    (native-inputs
     (list which python-pytest))
    (home-page "https://pytorch.org/vision/stable/index.html")
    (synopsis "Datasets, transforms and models specific to computer vision")
    (description
     "The torchvision package consists of popular datasets, model architectures,
and common image transformations for computer vision.")
    (license license:bsd-3)))

(define-public python-torchfile
  (package
    (name "python-torchfile")
    (version "0.1.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "torchfile" version))
              (sha256
               (base32
                "0vhklj6krl9r0kdynb4kcpwp8y1ihl2zw96byallay3k9c9zwgd5"))))
    (build-system python-build-system)
    (arguments '(#:tests? #false)) ;there are no tests
    (propagated-inputs
     (list python-numpy))
    (home-page "https://github.com/bshillingford/python-torchfile")
    (synopsis "Torch7 binary serialized file parser")
    (description "This package enables you to deserialize Lua torch-serialized objects from
Python.")
    (license license:bsd-3)))

(define-public python-hmmlearn
  (package
    (name "python-hmmlearn")
    (version "0.2.8")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "hmmlearn" version))
       (sha256
        (base32
         "1yd5l9ra37mks41mn5bigav7xpb161a9yqlcnz4ir076vkik2sb9"))))
    (build-system python-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (with-directory-excursion (string-append #$output "/lib")
                  (invoke "python" "-m" "pytest"))))))))
    (propagated-inputs
     (list pybind11
           python-numpy
           python-scikit-learn
           python-scipy
           python-setuptools-scm))
    (native-inputs
     (list python-pytest))
    (home-page "https://github.com/hmmlearn/hmmlearn")
    (synopsis "Hidden Markov Models with scikit-learn like API")
    (description
     "Hmmlearn is a set of algorithms for unsupervised learning and inference
of Hidden Markov Models.")
    (license license:bsd-3)))

;; Keep this in sync with the r-torch package.
(define-public liblantern
  (package
    (name "liblantern")
    (version "0.10.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mlverse/torch")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "12480fac9xq7rgw0q5f2cnvmakhakjsnq1gvh2ncjfwxz34n8fl7"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #false                   ;no test target
      #:phases
      (let ((python-version (version-major+minor (package-version python))))
        #~(modify-phases %standard-phases
            (add-after 'unpack 'chdir
              (lambda _ (chdir "src/lantern")))
            (add-after 'chdir 'do-not-download-binaries
              (lambda* (#:key inputs #:allow-other-keys)
                (substitute* "CMakeLists.txt"
                  (("find_package\\(Torch.*") "set(TORCH_CXX_FLAGS \"-ltorch\")\n")
                  (("retrieve_lib\\(.*") ""))
                (let ((site-packages (string-append "/lib/python"
                                                    #$python-version
                                                    "/site-packages")))
                  (setenv "LIBRARY_PATH"
                          (string-append
                           (search-input-directory
                            inputs (string-append site-packages "/torch/lib"))
                           ":" (or (getenv "LIBRARY_PATH") "")))
                  (setenv "CPLUS_INCLUDE_PATH"
                          (string-append
                           (search-input-directory
                            inputs (string-append
                                    site-packages "/torch/include/torch/csrc/api/include/"))
                           ":"
                           (search-input-directory
                            inputs (string-append site-packages "/torch/include/"))
                           ":"
                           (or (getenv "CPLUS_INCLUDE_PATH") "")))
                  (setenv "C_INCLUDE_PATH"
                          (string-append
                           (search-input-directory
                            inputs (string-append site-packages "/torch/include/"))
                           ":"
                           (or (getenv "C_INCLUDE_PATH") ""))))))
            (replace 'install
              (lambda _
                (install-file
                 "../build/liblantern.so"
                 (string-append #$output "/lib"))
                (copy-recursively
                 "../lantern/include"
                 (string-append #$output "/include"))))))))
    (inputs (list python-pytorch-for-r-torch))
    (home-page "https://github.com/mlverse/torch/")
    (synopsis "C API to libtorch")
    (description
     "Lantern provides a C API to the libtorch machine learning library.")
    (license license:expat)))

(define-public python-lap
  (package
    (name "python-lap")
    (version "0.4.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "lap" version))
              (sha256
               (base32
                "0fqfxpq4jg9h4wxjw540gjmvfg1ccc1nssk7i9njg7qfdybxknn4"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda* (#:key inputs #:allow-other-keys)
             (invoke "python" "setup.py" "build"
                     "--cpu-baseline=sse2")))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               ;; The tests must be run from elsewhere.
               (mkdir-p "/tmp/test")
               (copy-recursively "lap/tests" "/tmp/test")
               (with-directory-excursion "/tmp/test"
                 (invoke "pytest" "-vv"))))))))
    (propagated-inputs
     (list python-numpy
           python-scipy))
    (native-inputs
     (list python-cython python-pytest))
    (home-page "https://github.com/gatagat/lap")
    (synopsis "Linear Assignment Problem solver (LAPJV/LAPMOD)")
    (description "Lap is a linear assignment problem solver using Jonker-Volgenant
algorithm for dense (LAPJV) or sparse (LAPMOD) matrices.")
    (license license:bsd-2)))

(define-public python-visdom
  (package
    (name "python-visdom")
    (version "0.1.8.9")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "visdom" version))
              (sha256
               (base32
                "09kiczx2i5asqsv214fz7sx8wlyldgbqvxwrd0alhjn24cvx4fn7"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-jsonpatch
           python-numpy
           python-pillow
           python-pyzmq
           python-requests
           python-scipy
           python-six
           python-torchfile
           python-tornado
           python-websocket-client))
    (home-page "https://github.com/fossasia/visdom")
    (synopsis "Visualizations of live, rich data for Torch and Numpy")
    (description
     "This package provides a tool for visualizing live, rich data for Torch
and Numpy.")
    (license license:asl2.0)))

(define-public python-pyro-api
  (package
    (name "python-pyro-api")
    (version "0.1.2")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "pyro-api" version))
              (sha256
               (base32
                "086r2h6x9i5d9ayl1x65lx6p84rlydzsn8xingxc588ab3ch1fd1"))))
    (build-system python-build-system)
    (arguments '(#:tests? #false)) ;requires pyro
    (native-inputs
     (list python-flake8
           python-ipython
           python-pytest
           python-sphinx
           python-sphinx-rtd-theme))
    (home-page "https://github.com/pyro-ppl/pyro-api")
    (synopsis "Generic API for dispatch to Pyro backends")
    (description "This package provides a generic API for dispatch to Pyro backends.")
    (license license:asl2.0)))

(define-public python-pyro-ppl
  (package
    (name "python-pyro-ppl")
    (version "1.8.6")
    ;; The sources on pypi don't include tests.
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pyro-ppl/pyro")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0n1vsih99pvswcaygdxkc6kq6r48ny130z6ca8pp3281396r2ykw"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             ;; This tests features that are only implemented when non-free
             ;; software is available (Intel MKL or CUDA).
             (for-each delete-file
                       (list "tests/distributions/test_spanning_tree.py"
                             "tests/infer/mcmc/test_mcmc_api.py"))

             ;; Four test_gamma_elbo tests fail with bad values for unknown
             ;; reasons.
             (delete-file "tests/distributions/test_rejector.py")
             ;; This test fails sometimes.
             (delete-file "tests/optim/test_optim.py")
             (invoke "pytest" "-vv" "--stage=unit"))))))
    (propagated-inputs
     (list python-numpy
           python-opt-einsum
           python-pyro-api
           python-pytorch
           python-tqdm))
    (native-inputs
     (list ninja
           jupyter
           python-black
           python-flake8
           python-graphviz
           python-isort
           python-lap
           python-matplotlib
           python-mypy
           python-nbformat
           python-nbsphinx
           python-nbstripout
           python-nbval
           python-pandas
           python-pillow
           python-pypandoc
           python-pytest
           python-pytest-cov
           python-pytest-xdist
           python-scikit-learn
           python-scipy
           python-seaborn
           python-sphinx
           python-sphinx-rtd-theme
           python-torchvision
           python-visdom
           python-wget
           python-yapf))
    (home-page "https://pyro.ai")
    (synopsis "Python library for probabilistic modeling and inference")
    (description
     "This package provides a Python library for probabilistic modeling and
inference.")
    (license license:asl2.0)))

(define-public vosk-api
  (let* ((openfst openfst-for-vosk)
         (kaldi kaldi-for-vosk))
    (package
      (name "vosk-api")
      (version "0.3.43")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/alphacep/vosk-api")
               (commit (string-append "v" version))))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0xmp8i140c2hd3rj9dap8a2rnsvzb1k9hnqm12xzbaxrw73rkc29"))))
      (build-system gnu-build-system)
      (arguments
       (list
        #:tests? #f
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'chdir
              (lambda _ (chdir "src")))
            (replace 'configure
              (lambda _
                (let* ((lapack   #$(this-package-input "lapack"))
                       (openfst  #$(this-package-input "openfst"))
                       (openblas #$(this-package-input "openblas"))
                       (kaldi    #$(this-package-input "kaldi")))
                  (substitute* "./Makefile"
                    (("USE_SHARED\\?=0")
                     "USE_SHARED?=1")
                    (("-DFST_NO_DYNAMIC_LINKING")
                     "")
                    (("-lopenblas -llapack -lblas -lf2c")
                     (string-append
                      "-L" openblas "/lib " "-lopenblas "
                      "-L" lapack "/lib " "-llapack -lblas "))
                    (("-lfst -lfstngram")
                     (string-append
                      "-L" openfst "/lib " "-lfst -lfstngram "))
                    (("\\$\\(HOME\\)\\/travis\\/kaldi")
                     (string-append kaldi "/include"))
                    (("\\$\\(KALDI_ROOT\\)\\/tools\\/openfst")
                     openfst)
                    (("\\$\\(KALDI_ROOT\\)\\/tools\\/OpenBLAS\\/install")
                     openblas)
                    (("\\$\\(KALDI_ROOT\\)\\/libs")
                     (string-append kaldi "/lib"))))))
            (replace 'install
              (lambda _
                (let* ((lib (string-append #$output "/lib"))
                       (src (string-append #$output "/src")))
                  (mkdir-p lib)
                  (mkdir-p src)
                  (install-file "libvosk.so" lib)
                  (for-each
                   (lambda (x) (install-file x src))
                   (find-files "." "\\.h$"))))))))
      (inputs (list kaldi openfst lapack openblas))
      (home-page "https://alphacephei.com/vosk")
      (synopsis "Speech recognition toolkit based on @code{kaldi}")
      (description "\
This package provides a speech recognition toolkit based on @code{kaldi}.  It
supports more than 20 languages and dialects - English, Indian English,
German, French, Spanish, Portuguese, Chinese, Russian, Turkish, Vietnamese,
Italian, Dutch, Catalan, Arabic, Greek, Farsi, Filipino, Ukrainian, Kazakh,
Swedish, Japanese, Esperanto, Hindi, Czech, Polish. The program works offline,
even on lightweight devices.  Portable per-language models are about 50Mb each,
and there are much bigger and precise models available.

Vosk API provides a streaming API allowing to use it `on-the-fly' and bindings
for different programming languages.  It allows quick reconfiguration of
vocabulary for better accuracy, and supports speaker identification beside
simple speech recognition.")
      (license license:asl2.0))))

(define-public python-vosk
  (package
    (inherit vosk-api)
    (name "python-vosk")
    (build-system python-build-system)
    (propagated-inputs
     (list python-cffi python-requests python-tqdm python-srt python-websockets))
    (inputs (list vosk-api))
    (arguments
     (list
      #:tests? #f  ;; TODO There are tests but not run through Makefile.
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'config
            (lambda _
              (chdir "python")
              (setenv "VOSK_SOURCE" #$vosk-api)))
          (add-before 'build 'from-abi-to-api
            (lambda _
              (substitute* "vosk_builder.py"
                (("ffibuilder\\.set_source\\(\"vosk.vosk_cffi\", None\\)")
                 (string-append
                  "ffibuilder.set_source(\"vosk.vosk_cffi\", "
                  "r\"\"\"\n#include<vosk_api.h>\n#include<Python.h>\"\"\",\n\t"
                  "library_dirs=["
                  "'" #$vosk-api "/lib'"
                  "],\n\t"
                  "libraries=['vosk', 'python3.10'],\n\t"
                  "include_dirs=["
                  "'" #$vosk-api "/src'" "])")))
              (substitute* "vosk/__init__.py"
                (("_c = open_dll\\(\\)")
                 "")
                (("_ffi")
                 "ffi")
                (("from \\.vosk_cffi import ffi as ffi")
                 "from .vosk_cffi import ffi, lib")
                (("_c\\.")
                 "lib.")))))))))

(define-public nerd-dictation
  (let* ((commit "0eb44b7fd0927d69c92de5566e5807ed2c2e20b7")
         (revision "1"))
    (package
      (name "nerd-dictation")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/ideasman42/nerd-dictation")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0frdpswv6w3cwj3c7wd5w8gj3s1hvpdwd48qhfhfxf7imahz9bqf"))))
      (build-system python-build-system)
      (arguments
       '(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'chdir
             (lambda _ (chdir "package/python"))))))
      (propagated-inputs (list python-vosk))
      (home-page "https://github.com/ideasman42/nerd-dictation")
      (synopsis "Offline speech-to-text for desktop Linux")
      (description "\
This package provides simple access speech to text for using in
Linux without being tied to a desktop environment, using the @code{vosk-api}.
The user configuration lets you manipulate text using Python string
operations.  It has zero overhead, as this relies on manual activation and
there are no background processes.  Dictation is accessed manually with
@code{nerd-dictation begin} and @code{nerd-dictation end} commands.")
      (license license:gpl2+))))

(define (nerd-dictation-gexp input-name output-name bash nerd-dictation)
  (with-imported-modules '((guix build utils))
    #~(begin
        (use-modules (guix build utils))

        (let* ((exe (string-append #$output "/bin/nerd-dictation"))
               (nerd-dictation-exe
                #$(file-append nerd-dictation "/bin/nerd-dictation")))
          (mkdir-p (dirname exe))
          (call-with-output-file exe
            (lambda (port)
              (format port "#!~a
if [ \"$1\" = begin ]
  then
    exec ~a $@ --input=~a --simulate-input-tool=~a
  else
    exec ~a $@
fi"
                      #$(file-append bash "/bin/bash")
                      nerd-dictation-exe
                      #$input-name
                      #$output-name
                      nerd-dictation-exe)))
          (chmod exe #o555)))))

(define-public nerd-dictation/xdotool
  (package
    (inherit nerd-dictation)
    (name "nerd-dictation-xdotool")
    (build-system trivial-build-system)
    (arguments
     (list #:builder
           (nerd-dictation-gexp "PAREC" "XDOTOOL"
                                (this-package-input "bash-minimal")
                                (this-package-input "nerd-dictation"))))
    (inputs (list bash-minimal nerd-dictation))
    (propagated-inputs (list pulseaudio xdotool))))

(define-public nerd-dictation/sox-xdotool
  (package
    (inherit nerd-dictation/xdotool)
    (name "nerd-dictation-sox-xdotool")
    (arguments
     (list #:builder
           (nerd-dictation-gexp "SOX" "XDOTOOL"
                                (this-package-input "bash-minimal")
                                (this-package-input "nerd-dictation"))))
    (propagated-inputs (list sox xdotool))))

(define-public nerd-dictation/sox-ydotool
  (package
    (inherit nerd-dictation/xdotool)
    (name "nerd-dictation-sox-ydotool")
    (arguments
     (list #:builder
           (nerd-dictation-gexp "SOX" "YDOTOOL"
                                (this-package-input "bash-minimal")
                                (this-package-input "nerd-dictation"))))
    (propagated-inputs (list sox ydotool))))

(define-public nerd-dictation/sox-wtype
  (package
    (inherit nerd-dictation/xdotool)
    (name "nerd-dictation-sox-wtype")
    (arguments
     (list #:builder
           (nerd-dictation-gexp "SOX" "WTYPE"
                                (this-package-input "bash-minimal")
                                (this-package-input "nerd-dictation"))))
    (propagated-inputs (list sox wtype))))

(define-public python-brian2
  (package
    (name "python-brian2")
    (version "2.5.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "Brian2" version))
              (sha256
               (base32
                "1g48hzn3cdsvfjgz64s3kvh5d5287ggjxdyacb7wh2n5nd5iqlf7"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (setenv "HOME" "/tmp")
               ;; Must be run in a different directory, otherwise compiled
               ;; modules are not found.
               (with-directory-excursion "/tmp"
                 ;; Invoking brian2.test() is preferred to running pytest.
                 (invoke "python" "-c"
                  "import brian2, sys; sys.exit(0 if brian2.test() else 1)"))))))))
    (propagated-inputs (list python-cython ; Required by codegen.
                             python-jinja2
                             python-numpy
                             python-py-cpuinfo
                             python-pyparsing
                             ;; Required by codegen.
                             python-setuptools
                             python-sympy))
    (native-inputs (list python-pytest python-pytest-xdist))
    (home-page "https://briansimulator.org/")
    (synopsis "Clock-driven simulator for spiking neural networks")
    (description
     "Brian is a simulator for spiking neural networks written in Python.  It
is therefore designed to be easy to learn and use, highly flexible and
easily extensible.")
    (license license:cecill)))

(define-public python-brian2tools
  (package
    (name "python-brian2tools")
    (version "0.3")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "brian2tools" version))
              (sha256
               (base32
                "0fn028mfy3qlzjkadd0wr5d7rcplijd5jphln414xifvvsb9jcc2"))))
    (build-system python-build-system)
    ;; Both pypi tarball and git repo lack test files.
    (arguments (list #:tests? #f))
    (propagated-inputs (list python-brian2
                             python-libneuroml
                             python-markdown-strings
                             python-matplotlib
                             python-pylems
                             python-setuptools
                             python-setuptools-scm))
    (native-inputs (list python-pytest))
    (home-page "https://github.com/brian-team/brian2tools")
    (synopsis "Tools for the Brian 2 simulator")
    (description "Visualization and NeuroML import/export tools for the
Brian 2 simulator.")
    (license license:cecill)))

(define-public oneapi-dnnl
  (package
    (name "oneapi-dnnl")
    (version "3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/oneapi-src/oneDNN")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1jgmb5kl0bf4a2zfn94zlb117672r9lvvkkmwl86ihlyr1mpr3d0"))))
    (build-system cmake-build-system)
    (arguments (if (target-riscv64?)
                   (list #:configure-flags #~'("-DDNNL_CPU_RUNTIME=SEQ"))
                   '()))
    (home-page "https://github.com/oneapi-src/oneDNN")
    (synopsis "Deep Neural Network Library")
    (description
     "OneAPI Deep Neural Network Library (oneDNN) is a cross-platform
performance library of basic building blocks for deep learning applications.")
    (license license:asl2.0)))
