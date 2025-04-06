;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015-2025 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016, 2020-2024 Efraim Flashner <efraim@flashner.co.il>
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
;;; Copyright © 2020, 2021, 2022, 2023, 2024 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2020, 2021, 2022, 2023 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2022, 2023, 2024 Nicolas Graves <ngraves@ngraves.fr>
;;; Copyright © 2022 Kiran Shila <me@kiranshila.com>
;;; Copyright © 2022 Wiktor Zelazny <wzelazny@vurv.cz>
;;; Copyright © 2023 zamfofex <zamfofex@twdb.moe>
;;; Copyright © 2023 Navid Afkhami <navid.afkhami@mdc-berlin.de>
;;; Copyright © 2023 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2023 Troy Figiel <troy@troyfigiel.com>
;;; Copyright © 2024-2025 Sharlatan Hellseher <sharlatanus@gmail.com>
;;; Copyright © 2024 David Pflug <david@pflug.io>
;;; Copyright © 2024 Timothee Mathieu <timothee.mathieu@inria.fr>
;;; Copyright © 2024 Spencer King <spencer.king@geneoscopy.com>
;;; Copyright © 2024, 2025 David Elsing <david.elsing@posteo.net>
;;; Copyright © 2024 Andy Tai <atai@atai.org>
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
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
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
  #:use-module (gnu packages crates-check)
  #:use-module (gnu packages crates-crypto)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages crates-tls)
  #:use-module (gnu packages crates-web)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages dejagnu)
  #:use-module (gnu packages documentation)
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
  #:use-module (gnu packages libedit)
  #:use-module (gnu packages libevent)
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
  #:use-module (gnu packages pretty-print)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages rdf)
  #:use-module (gnu packages regex)
  #:use-module (gnu packages rocm)
  #:use-module (gnu packages rpc)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages valgrind)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages video)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (ice-9 match))

(define-public dlpack
  (package
    (name "dlpack")
    (version "1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dmlc/dlpack")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "169slm88jin4ddhdwk1qhqzzkhkwk1jrz35i7abhcqkry9wjib4f"))))
    (build-system cmake-build-system)
    (arguments (list #:tests? #f))      ;No tests.
    (home-page "https://github.com/dmlc/dlpack")
    (synopsis "In memory tensor structure")
    (description
     "DLPack is an in-memory tensor structure for sharing tensors among
frameworks.")
    (license license:asl2.0)))

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

(define-public python-autograd-gamma
  (package
    (name "python-autograd-gamma")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "autograd-gamma" version))
       (sha256
        (base32 "1i699a9m5ndnj8cwzjjf2agb77aawhzrzxfbmn5zrkxridxvnypj"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-autograd
           python-scipy))
    (home-page "https://github.com/CamDavidsonPilon/autograd-gamma")
    (synopsis
     "Autograd-compatible approximations to the gamma family of functions")
    (description
     "This package provides Autograd-compatible approximations to the gamma
family of functions.")
    (license license:expat)))

(define-public python-fasttext
  (package
    (inherit fasttext)
    (name "python-fasttext")
    (build-system pyproject-build-system)
    (propagated-inputs (list python-numpy python-scipy))
    (inputs (list fasttext))
    (native-inputs (list pybind11 python-setuptools python-wheel))))

(define-public python-funsor
  (package
    (name "python-funsor")
    (version "0.4.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "funsor" version))
       (sha256
        (base32 "0cgysij0dix0fikyz2x4f8jvaskm5s5a04s07chzaz2dw1fpxdq8"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      '(list "-k"
             (string-append
              ;; Disable flaky test
              "not test_integrate_variable[x23-i]"
              ;; XXX This test fails because the length of arguments
              ;; is longer than the length of inputs.
              " and not test_function_of_numeric_array"))))
    (propagated-inputs (list python-makefun python-multipledispatch
                             python-numpy python-opt-einsum
                             python-typing-extensions))
    (native-inputs (list python-black
                         python-flake8
                         python-isort
                         python-nbsphinx
                         python-pandas
                         python-pillow
                         python-pyro-api
                         python-pytest
                         python-pytest-xdist
                         python-requests
                         python-scipy
                         python-setuptools
                         python-sphinx
                         python-sphinx-gallery
                         python-sphinx-rtd-theme
                         python-torchvision
                         python-wheel))
    (home-page "https://github.com/pyro-ppl/funsor")
    (synopsis "Tensor-like library for functions and distributions")
    (description
     "This package provides a tensor-like library for functions and
distributions.")
    (license license:asl2.0)))

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

(define-public python-gpy
  (package
    (name "python-gpy")
    (version "1.13.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "GPy" version))
       (sha256
        (base32 "083rl8nr4nmmr1pzn0g8gsc4wi9dnhj3jjhkwsssadm5vns5d0m3"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'compatibility
            (lambda _
              ;; This file uses Python 2 statements
              (delete-file "GPy/testing/mpi_test__.py")
              (substitute* "setup.py"
                (("scipy>=1.3.0,<1.12.0")
                 "scipy>=1.3.0,<=1.13.0"))
              ;; Use numpy.exp because scipy.ext no longer exists
              (substitute* "GPy/kern/src/sde_standard_periodic.py"
                (("sp\\.exp") "np.exp"))
              (substitute* "GPy/kern/src/sde_stationary.py"
                (("sp\\.poly1d") "np.poly1d")
                (("sp\\.roots") "np.roots")))))))
    (native-inputs
     (list python-cython
           python-matplotlib
           python-pods
           python-pytest
           python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-numpy
           python-paramz
           python-scipy
           python-six))
    (home-page "https://sheffieldml.github.io/GPy/")
    (synopsis "The Gaussian Process Toolbox")
    (description
     "@command{GPy} is a Gaussian Process (GP) framework written in
Python, from the Sheffield machine learning group.  GPy implements a range of
machine learning algorithms based on GPs.")
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

(define-public python-ml-collections
  (package
    (name "python-ml-collections")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/google/ml_collections")
             (commit (string-append "v" version))))
       (sha256
        (base32 "1f3rwbgnnvgh2jgnkwxfjdw18yly41hlx9fy56h0x36zyy8p0j21"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags '(list "--pyargs" "ml_collections/config_dict/tests")))
    (propagated-inputs
     (list python-absl-py python-pyyaml))
    (native-inputs (list python-pylint
                         python-pytest
                         python-pytest-xdist
                         python-flit-core))
    (home-page "https://github.com/google/ml_collections")
    (synopsis "Python collections designed for Machine Learning usecases")
    (description
     "ML Collections is a library of Python collections designed for Machine
Learning usecases.")
    (license license:asl2.0)))

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
       `(#:imported-modules (,@%default-gnu-imported-modules
                             (guix build python-build-system))
         #:modules          ((guix build python-build-system)
                             ,@%default-gnu-modules)
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
  (let ((tag "b4549"))
    (package
      (name "llama-cpp")
      (version (string-append "0.0.0-" tag))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/ggerganov/llama.cpp")
               (commit tag)))
         (file-name (git-file-name name tag))
         (sha256
          (base32 "1xf2579q0r8nv06kj8padi6w9cv30w58vdys65nq8yzm3dy452a1"))
         (patches
          (search-patches "llama-cpp-vulkan-optional.patch"))))
      (build-system cmake-build-system)
      (arguments
       (list
        #:configure-flags
        #~(list "-DBUILD_SHARED_LIBS=ON"
                "-DGGML_VULKAN=ON"
                "-DGGML_BLAS=ON"
                "-DGGML_BLAS_VENDOR=OpenBLAS"
                (string-append "-DBLAS_INCLUDE_DIRS="
                               #$(this-package-input "openblas")
                               "/include")
                (string-append "-DBLAS_LIBRARIES="
                               #$(this-package-input "openblas")
                               "/lib/libopenblas.so")

                "-DGGML_NATIVE=OFF" ;no '-march=native'
                "-DGGML_FMA=OFF"    ;and no '-mfma', etc.
                "-DGGML_AVX2=OFF"
                "-DGGML_AVX512=OFF"
                "-DGGML_AVX512_VBMI=OFF"
                "-DGGML_AVX512_VNNI=OFF")

        #:modules '((ice-9 textual-ports)
                    (guix build utils)
                    ((guix build python-build-system) #:prefix python:)
                    (guix build cmake-build-system))
        #:imported-modules `(,@%cmake-build-system-modules
                             (guix build python-build-system))
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'patch-paths
              (lambda* (#:key inputs #:allow-other-keys)
                (substitute* "ggml/src/ggml-vulkan/vulkan-shaders/vulkan-shaders-gen.cpp"
                 (("\"/bin/sh\"")
                  (string-append "\"" (search-input-file inputs "/bin/sh") "\"")))))
            (add-after 'unpack 'disable-unrunable-tests
              (lambda _
                ;; test-eval-callback downloads ML model from network, cannot
                ;; run in Guix build environment
                (substitute* '("examples/eval-callback/CMakeLists.txt")
                  (("COMMAND llama-eval-callback")
                   "COMMAND true llama-eval-callback"))))
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
                  (make-script "convert_hf_to_gguf")
                  (make-script "convert_llama_ggml_to_gguf")
                  (make-script "convert_hf_to_gguf_update.py"))))
            (add-after 'install-python-scripts 'wrap-python-scripts
              (assoc-ref python:%standard-phases 'wrap))
            (add-after 'install 'remove-tests
              (lambda* (#:key outputs #:allow-other-keys)
                (for-each delete-file (find-files
                                       (string-append (assoc-ref outputs "out")
                                                      "/bin")
                                       "^test-")))))))
      (inputs (list python vulkan-headers vulkan-loader))
      (native-inputs (list pkg-config shaderc bash))
      (propagated-inputs
       (list python-numpy python-pytorch python-sentencepiece openblas))
      (properties '((tunable? . #true))) ;use AVX512, FMA, etc. when available
      (home-page "https://github.com/ggerganov/llama.cpp")
      (synopsis "Port of Facebook's LLaMA model in C/C++")
      (description "This package provides a port to Facebook's LLaMA collection
of foundation language models.  It requires models parameters to be downloaded
independently to be able to run a LLaMA model.")
      (license license:expat))))

(define-public whisper-cpp
  (package
    (name "whisper-cpp")
    (version "1.7.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ggml-org/whisper.cpp")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0fs15rizz4psd3flfjpdivzvc9w19i3706flisn136ax0k8r7w5n"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list "-DWHISPER_STANDALONE=TRUE"
              "-DWHISPER_SDL2=TRUE"
              "-DWHISPER_BUILD_TESTS=TRUE"
                                        ; "-DWHISPER_FFMPEG=TRUE"  ; TODO
              "-DBUILD_SHARED_LIBS=ON"
              "-DGGML_BLAS=ON"
              "-DGGML_BLAS_VENDOR=OpenBLAS"
              (string-append "-DBLAS_INCLUDE_DIRS="
                             #$(this-package-input "openblas")
                             "/include")
              (string-append "-DBLAS_LIBRARIES="
                             #$(this-package-input "openblas")
                             "/lib/libopenblas.so")

              "-DGGML_NATIVE=OFF" ;no '-march=native'
              "-DGGML_FMA=OFF"    ;and no '-mfma', etc.
              "-DGGML_AVX2=OFF"
              "-DGGML_AVX512=OFF"
              "-DGGML_AVX512_VBMI=OFF"
              "-DGGML_AVX512_VNNI=OFF")
      #:phases
      #~(modify-phases %standard-phases
          #$@(if (not (target-64bit?))
                 '((add-after 'unpack 'skip-failing-tests
                     (lambda _
                       ;; 32-bit system
                       ;; large model does not fit in RAM in 32-bit system,
                       ;; disable large model test
                       (substitute* "tests/CMakeLists.txt"
                         (("LABELS \"large\"")
                          "DISABLED true")))))
                 '()))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list openblas sdl2 git))
    (synopsis "OpenAI's Whisper model in C/C++")
    (description
     "This package is a high-performance inference of OpenAI's
Whisper automatic speech recognition (ASR) model, implemented in plain C/C++
without dependencies, with
@itemize
@item AVX intrinsics support for x86 architectures
@item VSX intrinsics support for POWER architectures
@item Mixed F16 / F32 precision
@item 4-bit and 5-bit integer quantization support
@item Zero memory allocations at runtime
@item Support for CPU-only inference
@item Efficient GPU support for NVIDIA
@item OpenVINO Support
@item C-style API
@end itemize")
    (properties '((tunable? . #true))) ;use AVX512, FMA, etc. when available
    (home-page "https://github.com/ggerganov/whisper.cpp")
    (license license:expat)))

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

(define-public openmm
  (package
    (name "openmm")
    (version "8.1.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/openmm/openmm")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "064vv6zaci30pj38z5lwfqscxssm67jqxkz30hcya9vm4ng831d5"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:configure-flags
      '(list "-DOPENMM_BUILD_SHARED_LIB=TRUE"
             "-DOPENMM_BUILD_C_AND_FORTRAN_WRAPPERS=TRUE"
             "-DOPENMM_BUILD_PYTHON_WRAPPERS=TRUE"
             "-DOPENMM_BUILD_CUDA_LIB=FALSE")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-python-build-system
            (lambda _
              (substitute* "wrappers/python/CMakeLists.txt"
                (("install --root=\\\\\\$ENV\\{DESTDIR\\}/")
                 (string-append "install --prefix=" #$output
                                " --root=/ --single-version-externally-managed")))))
          (add-after 'install 'install-python
            (lambda _
              (invoke "make" "PythonInstall"))))))
    (inputs
     (list python-wrapper))
    (propagated-inputs
     (list python-numpy))
    (native-inputs
     (list doxygen gfortran opencl-headers python-cython swig))
    (home-page "https://github.com/openmm/openmm/")
    (synopsis "Toolkit for molecular simulation")
    (description
     "OpenMM is a toolkit for molecular simulation.  It can be used either as
a stand-alone application for running simulations, or as a library you call
from your own code.")
    ;; See https://github.com/openmm/openmm/issues/4278#issuecomment-1772982471
    (license license:expat)))

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
  (let ((commit "5449a5b479908f40f56cf911f11e0a7e156d207f")
        (revision "2"))
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
                  "0sfn6cm8qqsv1g80wc6pimldr0q11vfqik0hcynp8dfrkmvzhj8n"))))
      (properties `((upstream-name . "RcppML")))
      (build-system r-build-system)
      (propagated-inputs (list r-matrix r-rcpp))
      (native-inputs (list r-knitr r-testthat))
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

(define-public python-hopcroftkarp
  ;; This commit fixes a broken import, but has not been released to PyPI.
  (let ((commit "2846e1dd3265d95d2bddb0cf4190b830cbb4efe6")
        (revision "1"))
    (package
      (name "python-hopcroftkarp")
      (version (git-version "1.2.5" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/sofiatolaosebikan/hopcroftkarp")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "018ilrp41fcclmb5lsml3aijwbmhbq3m7wy65hr1fryj0avic8fr"))))
      (build-system pyproject-build-system)
      (native-inputs (list python-setuptools python-wheel))
      (home-page "https://github.com/sofiatolaosebikan/hopcroftkarp")
      (synopsis "Implementation of the Hopcroft-Karp algorithm")
      (description
       "This package implements the Hopcroft-Karp algorithm, producing a maximum
cardinality matching from a bipartite graph.")
      (license license:gpl3))))

(define-public python-persim
  (package
    (name "python-persim")
    (version "0.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "persim" version))
       (sha256
        (base32 "0q8wfakx8q4h3ryvw8cba0v6z7xn9139qkrzs3mi1ggyzacnx9d7"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-deprecated
                             python-hopcroftkarp
                             python-joblib
                             python-matplotlib
                             python-numpy
                             python-scikit-learn
                             python-scipy))
    (native-inputs (list python-pytest python-pytest-cov python-setuptools
                         python-wheel))
    (home-page "https://persim.scikit-tda.org")
    (synopsis "Tools for analyzing persistence diagrams in Python")
    (description
     "This package includes a variety of tools used to analyze persistence diagrams.
It currently houses implementations of
@itemize
@item Persistence images
@item Persistence landscapes
@item Bottleneck distance
@item Modified Gromov–Hausdorff distance
@item Sliced Wasserstein kernel
@item Heat kernel
@item Diagram plotting
@end itemize
")
    (license license:expat))) ; MIT License

(define-public python-pot
  (package
    (name "python-pot")
    (version "0.9.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pot" version))
       (sha256
        (base32 "0hk0dmjgnpwka0a7gyzrcq155wzlvzcrsav3qaizyg0wymzywi4n"))
       (snippet '(delete-file "ot/lp/emd_wrap.cpp"))))
    (build-system pyproject-build-system)
    (propagated-inputs
     (list python-autograd
           python-numpy
           python-pytorch
           python-pytorch-geometric
           python-pymanopt
           python-scikit-learn
           python-scipy))
    (native-inputs (list python-cython
                         python-setuptools
                         python-wheel))
    (home-page "https://github.com/PythonOT/POT")
    (synopsis "Python Optimal Transport Library")
    (description "This Python library provides several solvers for
optimization problems related to Optimal Transport for signal, image
processing and machine learning.")
    (license license:expat)))

(define-public python-pymanopt
  (package
    (name "python-pymanopt")
    (version "2.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pymanopt" version))
       (sha256
        (base32 "1nm1yz5hbj1valqq23r8c1g9rhfdndfswlqv6xrnvc3f8fd95167"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; Tests require jax and tensorflow, which are optional.
      #:tests? #false
      #:phases
      '(modify-phases %standard-phases
         ;; This is probably a bad idea.  We don't have scipy 1.13 just yet.
         (add-after 'unpack 'relax-requirements
           (lambda _
             (substitute* "pyproject.toml"
               ((",!=1.12\\.\\*") "")))))))
    (propagated-inputs (list python-numpy python-scipy))
    (native-inputs (list python-autograd
                         python-flake8
                         python-flake8-bugbear
                         python-isort
                         python-matplotlib
                         python-pytest
                         python-pytest-cov
                         python-setuptools
                         python-setuptools-scm
                         python-wheel))
    (home-page "https://pymanopt.org/")
    (synopsis "Toolbox for optimization on Riemannian manifolds")
    (description
     "This package is a toolbox for optimization on Riemannian manifolds with
support for automatic differentiation.")
    (license license:bsd-3)))

(define-public python-ripser
  (package
    (name "python-ripser")
    (version "0.6.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ripser" version))
       (sha256
        (base32 "1575nwsn6b29z7w1mjk23ri83bxq2b4ld979hpgm174642a3x6vs"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-numpy python-persim python-scikit-learn
                             python-scipy))
    (native-inputs
     (list python-cython
           python-pytest
           python-setuptools
           python-wheel))
    (home-page "https://ripser.scikit-tda.org")
    (synopsis "Persistent homology library for Python")
    (description
     "This package implements a variety of persistent homology algorithms.  It
provides an interface for
@itemize
@item computing persistence cohomology of sparse and dense data sets
@item visualizing persistence diagrams
@item computing lowerstar filtrations on images
@item computing representative cochains
@end itemize
")
    (license license:expat))) ; MIT License

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
    (native-inputs (list python-pytest python-setuptools python-wheel))
    (home-page "https://github.com/mjpost/sacrebleu")
    (synopsis
     "Compute shareable, comparable, and reproducible BLEU, chrF, and TER scores")
    (description
     "This is a package for hassle-free computation of shareable, comparable,
and reproducible BLEU, chrF, and TER scores for natural language processing.")
    (license license:asl2.0)))

(define-public rust-safetensors
  (package
    (name "rust-safetensors")
    (version "0.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "safetensors" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1fbx56wikqcvqb4y0ym0cys68lj0v3cpanhsy5i13fkz5jr7dvcc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-serde" ,rust-serde-1)
        ("rust-serde-json" ,rust-serde-json-1))
       #:cargo-development-inputs
       (("rust-criterion" ,rust-criterion-0.5)
        ("rust-memmap2" ,rust-memmap2-0.9)
        ("rust-proptest" ,rust-proptest-1))))
    (home-page "https://github.com/huggingface/safetensors")
    (synopsis "Simple and safe way to store and distribute tensors")
    (description
     "This package provides a fast (zero-copy) and safe (dedicated) format for
storing tensors safely, named safetensors.  They aim to be safer than their
@code{PyTorch} counterparts.")
    (license license:asl2.0)))

(define-public python-safetensors
  (package
    (name "python-safetensors")
    (version "0.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "safetensors" version))
       (sha256
        (base32 "1hhiwy67jarm70l0k26fs1cjhzkgzrh79q14bklj2yp0qi8gr19g"))
       (modules '((guix build utils)
                  (ice-9 ftw)))
       (snippet
        #~(begin                        ;Only keep bindings.
            (for-each
             (lambda (file)
               (unless (member file '("." ".." "bindings" "PKG-INFO"))
                 (delete-file-recursively file)))
             (scandir "."))
            (for-each
             (lambda (file)
               (unless (member file '("." ".."))
                 (rename-file (string-append "bindings/python/" file)
                              file)))
             (scandir "bindings/python"))))))
    (build-system cargo-build-system)
    (arguments
     (list
      #:modules '((guix build cargo-build-system)
                  (guix build utils)
                  (ice-9 regex)
                  (ice-9 textual-ports)
                  (srfi srfi-26))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack-rust-crates 'inject-safetensors
            (lambda _
              (substitute* "Cargo.toml"
                (("\\[dependencies\\]")
                 (format #f "[dependencies]~%safetensors = ~s"
                         #$(package-version rust-safetensors))))
              (call-with-input-file "Cargo.toml"
                (lambda (port)
                  (let* ((content (get-string-all port))
                         (top-match (string-match
                                     "\\[dependencies.safetensors"
                                     content)))
                    (call-with-output-file "Cargo.toml"
                      (cut display (match:prefix top-match) <>)))))))
          (add-before 'check 'install-rust-library
            (lambda _
              (copy-file "target/release/libsafetensors_rust.so"
                         "py_src/safetensors/_safetensors_rust.so")))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (setenv "PYTHONPATH" (string-append (getcwd) "/py_src"))
                (invoke "python3"
                        "-m" "pytest"
                        "-n" "auto"
                        "--dist=loadfile"
                        "-s" "-v" "./tests/"
                        ;; Missing jax and tensorflow dependency
                        "--ignore=./tests/test_flax_comparison.py"
                        "--ignore=./tests/test_tf_comparison.py"))))
          (add-after 'install 'install-python
            (lambda _
              (let* ((pversion #$(version-major+minor
                                  (package-version python)))
                     (lib (string-append #$output "/lib/python" pversion
                                         "/site-packages/"))
                     (info (string-append lib "safetensors-"
                                          #$(package-version this-package)
                                          ".dist-info")))
                (mkdir-p info)
                (copy-file "PKG-INFO" (string-append info "/METADATA"))
                (copy-recursively
                 "py_src/safetensors"
                 (string-append lib "safetensors"))))))
      #:cargo-inputs
      `(("rust-pyo3" ,rust-pyo3-0.21)
        ("rust-memmap2" ,rust-memmap2-0.9)
        ("rust-safetensors" ,rust-safetensors)
        ("rust-serde-json" ,rust-serde-json-1))))
    (inputs
     (list rust-safetensors))
    (native-inputs
     (list python-h5py
           python-minimal
           python-numpy
           python-pytest
           python-pytest-xdist
           python-pytorch))
    (home-page "https://huggingface.co/docs/safetensors")
    (synopsis "Simple and safe way to store and distribute tensors")
    (description "This package provides a fast (zero-copy) and safe
(dedicated) format for storing tensors safely.  This package builds upon
@code{rust-safetensors} and provides Python bindings.")
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

(define-public python-sentence-transformers
  (package
    (name "python-sentence-transformers")
    (version "3.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sentence_transformers" version))
       (sha256
        (base32 "1xmzbyrlp6wa7adf42n67c544db17nz95b10ri603lf4gi9jqgca"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      '(list
        ;; Missing fixture / train or test data.
        ;; Requires internet access.
        "--ignore=tests/test_sentence_transformer.py"
        "--ignore=tests/test_train_stsb.py"
        "--ignore=tests/test_compute_embeddings.py"
        "--ignore=tests/test_cross_encoder.py"
        "--ignore=tests/test_model_card_data.py"
        "--ignore=tests/test_multi_process.py"
        "--ignore=tests/test_pretrained_stsb.py"
        "-k" (string-append
              "not test_LabelAccuracyEvaluator"
              " and not test_ParaphraseMiningEvaluator"
              " and not test_cmnrl_same_grad"
              " and not test_paraphrase_mining"
              " and not test_simple_encode"))))
    (propagated-inputs (list python-huggingface-hub
                             python-numpy
                             python-pillow
                             python-pytorch
                             python-scikit-learn
                             python-scipy
                             python-tqdm
                             python-transformers))
    (native-inputs (list python-pytest python-setuptools python-wheel))
    (home-page "https://www.SBERT.net")
    (synopsis "Multilingual text embeddings")
    (description "This framework provides an easy method to compute dense
vector representations for sentences, paragraphs, and images.  The models are
based on transformer networks like BERT / RoBERTa / XLM-RoBERTa and achieve
state-of-the-art performance in various tasks.  Text is embedded in vector
space such that similar text are closer and can efficiently be found using
cosine similarity.

This package provides easy access to pretrained models for more than 100
languages, fine-tuned for various use-cases.

Further, this framework allows an easy fine-tuning of custom embeddings
models, to achieve maximal performance on your specific task.")
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
    (native-inputs (list python-pytest python-setuptools python-wheel))
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
    (native-inputs (list python-pytest python-setuptools python-wheel))
    (home-page "https://github.com/explosion/spacy-loggers")
    (synopsis "Logging utilities for SpaCy")
    (description "This package provides logging utilities for the SpaCy
natural language processing framework.")
    (license license:expat)))

(define-public python-spacy
  (package
    (name "python-spacy")
    (version "3.6.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "spacy" version))
              (sha256
               (base32
                "0ri1cz62kswawsa4hflh0ah8f63mnnqah0sbd5hmabdf0s3sj8v3"))))
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
                             python-pydantic-2
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
     (list python-cython python-pytest python-mock python-wheel))
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
       ("openblas" ,openblas)
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
    (version "1.17.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/onnx/onnx")
                    (commit (string-append "v" version))))
              (sha256
               (base32
                "1i6bh4z2xzz1maykr0xmrwfybm6i3g38vnx7hsls8hr58rdr30zn"))
              (file-name (git-file-name name version))
              (patches (search-patches
                        "onnx-shared-libraries.patch"
                        "onnx-skip-model-downloads.patch"))
              (modules '((guix build utils)))
              (snippet '(delete-file-recursively "third_party"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; python-nbval depends transitively on Rust.
      #:tests?
      (->bool (member (or (%current-target-system)
                          (%current-system))
                      (package-transitive-supported-systems python-nbval)))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'pass-cmake-arguments
            (lambda* (#:key outputs tests? #:allow-other-keys)
              ;; For derived package use
              (substitute* "CMakeLists.txt"
                (("set\\(ONNX_ROOT.*") "")
                (("\\$\\{ROOT_DIR\\}(/tools.*)" _ rest)
                 (string-append "${PROJECT_SOURCE_DIR}" rest)))
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
                      (string-append "-DONNX_BUILD_TESTS="
                                     (if tests? "ON" "OFF"))
                      "-DBUILD_SHARED_LIBS=ON"
                      "-DONNX_USE_PROTOBUF_SHARED_LIBS=ON"
                      (string-append
                       "-DONNX_ROOT=" #$(package-source this-package))))

              ;; This environment variable is honored by 'setup.py',
              ;; which passes it down to 'cmake'.
              (setenv "CMAKE_ARGS" (string-join args))

              ;; This one is honored by 'setup.py' and passed to 'make -j'.
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
     (append
      (list cmake-minimal
            googletest
            pybind11
            python-coverage
            python-fb-re2
            python-parameterized
            python-pytest
            python-pytest-runner
            python-setuptools
            python-wheel)
      (filter
       (lambda (pkg)
         (member (or (%current-target-system)
                     (%current-system))
                 (package-transitive-supported-systems pkg)))
       (list python-nbval))))
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
    ;; Note: 0.3.x is *more* recent than 1.5.0.
    (version "0.3.19")
    (home-page "https://github.com/onnx/optimizer")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (sha256
               (base32
                "1mx3hsl42na6fr05nh2x3j9kxm56cpfmwk6lwl2cfq9zs3gv929w"))
              (file-name (git-file-name name version))
              (patches (search-patches "onnx-optimizer-system-library.patch"))
              (modules '((guix build utils)))
              (snippet '(delete-file-recursively "third_party"))))
    (build-system python-build-system)
    (arguments
     ;; reuse build system tweaks
     (substitute-keyword-arguments (package-arguments onnx)
       ((#:phases phases)
        #~(modify-phases #$phases
            (add-after 'pass-cmake-arguments
                'pass-onnx-optimizer-cmake-arguments
              (lambda _
                (setenv
                 "CMAKE_ARGS"
                 (string-append
                  (getenv "CMAKE_ARGS")
                  " -DONNX_OPT_USE_SYSTEM_PROTOBUF=ON"
                  " -DCMAKE_CXX_FLAGS=\"-DONNX_ML=1 -DONNX_NAMESPACE=onnx\""))))
            (replace 'check
              (lambda* (#:key tests? #:allow-other-keys)
                (if tests?
                    (invoke "pytest" "-vv" "-k"
                            ;; These tests fail with upstream ONNX:
                            ;; https://github.com/onnx/optimizer/issues/138
                            (string-append
                             "not test_fuse_matmul"
                             " and not test_fuse_consecutive"
                             " and not test_fuse_transpose")))))))))
    (native-inputs
     (append
      (list cmake-minimal python-pytest python-pytest-runner
            python-coverage)
      (filter
       (lambda (pkg)
         (member (or (%current-target-system)
                     (%current-system))
                 (package-transitive-supported-systems pkg)))
       (list python-nbval))))
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
    (version "19.24.6")
    (source
     (origin
      (method git-fetch)
      (uri (git-reference
            (url "https://github.com/davisking/dlib.git")
            (commit (string-append "v" version))))
      (file-name (git-file-name name version))
      (sha256
       (base32 "11j86nxkj78v8xdx2s80mfplq4j0rs0y0iidqgma12b2pdk3p486"))
      (modules '((guix build utils)))
      (snippet
       '(begin
          (delete-file-recursively "dlib/external")))))
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
             ;; XXX: This causes a rebuild--however, trying to run the tests
             ;; without rebuilding causes a lot of errors.
             ;; Also, the official way is to rebuild.
             (mkdir "../source/dlib/test/build")
             (with-directory-excursion "../source/dlib/test/build"
               (invoke "cmake" "-DBUILD_SHARED_LIBS=ON" "..")
               (invoke "cmake" "--build" "." "--config" "Release")
               (invoke "./dtest" "--runall")))))))
    (native-inputs
     (list pkg-config
           ;; For tests.
           libnsl))
    (inputs
     `(("ffmpeg" ,ffmpeg)
       ("giflib" ,giflib)
       ("libjpeg" ,libjpeg-turbo)
       ("libjxl" ,libjxl-0.10)
       ("libpng" ,libpng)
       ("libwebp" ,libwebp)
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
    (version "1.4.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/scikit-learn/scikit-learn")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0pdd508c9540x9qimq83b8kspb6mb98w7w7i7lnb1jqj7rijal6f"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      '(list "-m" "not network"
             "-k" (string-append
                   ;; This test tries to access the internet.
                   "not test_load_boston_alternative"
                   ;; DID NOT RAISE <class 'ValueError'>
                   " and not test_check_pandas_sparse_invalid"
                   ))
      #:phases
      '(modify-phases %standard-phases
         (add-before 'build 'configure
           (lambda _
             (setenv "SKLEARN_BUILD_PARALLEL"
                     (number->string (parallel-job-count)))))
         (add-after 'build 'build-ext
           (lambda _ (invoke "python" "setup.py" "build_ext" "--inplace"
                        "-j" (number->string (parallel-job-count)))))
         (replace 'check
           (lambda* (#:key tests? test-flags #:allow-other-keys)
             (when tests?
               ;; Restrict OpenBLAS threads to prevent segfaults while testing!
               (setenv "OPENBLAS_NUM_THREADS" "1")

               ;; Some tests require write access to $HOME.
               (setenv "HOME" "/tmp")

               ;; Step out of the source directory to avoid interference;
               ;; we want to run the installed code with extensions etc.
               (with-directory-excursion "/tmp"
                 (apply invoke "pytest" "--pyargs" "sklearn"
                        test-flags))))))))
    (inputs (list openblas))
    (native-inputs
     (list python-cython-3
           python-pandas
           python-pytest
           python-pytest-xdist
           python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-numpy python-threadpoolctl python-scipy python-joblib))
    (home-page "https://scikit-learn.org/")
    (synopsis "Machine Learning in Python")
    (description
     "Scikit-learn provides simple and efficient tools for data mining and
data analysis.")
    (license license:bsd-3)))

(define-public python-scikit-learn-extra
  ;; This commit fixes an incompatibility with newer versions of scikit-learn
  (let ((commit "0f95d8dda4c69f9de4fb002366041adcb1302f3b")
        (revision "1"))
    (package
      (name "python-scikit-learn-extra")
      (version (git-version "0.3.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/scikit-learn-contrib/scikit-learn-extra")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0a248sff1psfwzmngj465gzyisq20d83nzpwpq2cspxhih51m6j9"))))
      (build-system pyproject-build-system)
      (arguments
       (list
        #:test-flags
        ;; ignore tests that require network
        '(list "--pyargs" "sklearn_extra"
               "-k" (string-append "not test_build"
                                   ;; The error message format has changed,
                                   ;; but the behavior itself is still the
                                   ;; same.
                                   " and not test_parameter_validation"
                                   ;; exceptions must be derived from Warning,
                                   ;; not <class 'NoneType'>
                                   " and not test_seuclidean"))
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'build 'build-ext
              (lambda _
                (invoke "python" "setup.py" "build_ext"
                        "--inplace")))
            (replace 'check
              (lambda* (#:key tests? test-flags #:allow-other-keys)
                (when tests?
                  ;; Restrict OpenBLAS threads to prevent segfaults while testing!
                  (setenv "OPENBLAS_NUM_THREADS" "1")

                  ;; Some tests require write access to $HOME.
                  (setenv "HOME" "/tmp")

                  ;; Step out of the source directory to avoid interference;
                  ;; we want to run the installed code with extensions etc.
                  (with-directory-excursion "/tmp"
                    (apply invoke "pytest" "-vv" test-flags))))))))
      (propagated-inputs
       (list python-numpy
             python-scikit-learn
             python-scipy
             python-packaging))
      (native-inputs (list python-pytest python-pytest-cov python-cython
                           python-setuptools python-wheel))
      (home-page "https://github.com/scikit-learn-contrib/scikit-learn-extra")
      (synopsis "Set of tools for scikit-learn")
      (description
       "This package provides a Python module for machine learning that extends
scikit-learn.  It includes algorithms that are useful but do not satisfy the
scikit-learn inclusion criteria, for instance due to their novelty or lower
citation number.")
      (license license:bsd-3))))

(define-public python-mord
  (package
    (name "python-mord")
    (version "0.7")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "mord" version))
       (sha256
        (base32 "1cvv9b9w69v0inq0zgcw0vmkiq3zn9q9r6clkynpzjik9rrh405n"))))
    (build-system pyproject-build-system)
    ;; v0.7 does not provide any test cases
    ;; v0.6 relies on deprecated scikit-learn functionality
    (arguments `(#:tests? #f))
    (inputs (list python-numpy python-scipy python-scikit-learn))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://pypi.org/project/mord/")
    (synopsis "Ordinal regression models for scikit-learn")
    (description
     "This package provides a collection of ordinal regression models for
machine learning in Python.  They are intended to be used with scikit-learn
and are compatible with its API.")
    (license license:bsd-3)))

(define-public python-thinc
  (package
    (name "python-thinc")
    (version "8.1.12")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "thinc" version))
              (sha256
               (base32
                "0lx37vl84y2jcsfn9sphdzbjny2jjyfb85llrrvz0xmig5f2rlcx"))))
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
                             python-cymem
                             python-murmurhash
                             python-numpy
                             python-packaging
                             python-preshed
                             python-pydantic-2
                             python-setuptools
                             python-srsly
                             python-wasabi))
    (native-inputs (list python-cython python-mock python-pytest
                         python-setuptools python-wheel))
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

(define-public python-imbalanced-learn
  (package
    (name "python-imbalanced-learn")
    (version "0.12.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "imbalanced-learn" version))
       (sha256
        (base32 "1hgncab4g4xry7yl6wwsj1wmfnxbsajx6qmycvr28wdhvk75c358"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags '(list "-k"
                     ;; Although we cannot satify the Tensorflow and Keras requirements
                     ;; (python-keras >= 2.4.3 and tensorflow >= 2.4.3), all tests
                     ;; besides these pass.
                     "not balanced_batch_generator and not BalancedBatchGenerator")
      #:phases '(modify-phases %standard-phases
                  (add-after 'unpack 'unbreak-tests
                    (lambda _
                      ;; Some tests require a home directory
                      (setenv "HOME"
                              (getcwd)))))))
    (propagated-inputs (list python-joblib python-numpy python-scikit-learn
                             python-scipy python-threadpoolctl))
    (native-inputs (list python-black
                         python-flake8
                         python-keras
                         python-mypy
                         python-pandas
                         python-pytest
                         python-pytest-cov
                         python-setuptools
                         python-wheel
                         tensorflow))
    (home-page "https://github.com/scikit-learn-contrib/imbalanced-learn")
    (synopsis "Toolbox for imbalanced dataset in machine learning")
    (description
     "This is a Python package offering a number of re-sampling
techniques commonly used in datasets showing strong between-class imbalance.
It is compatible with @code{scikit-learn}.")
    (license license:expat)))

(define-public python-hdbscan
  (package
    (name "python-hdbscan")
    (version "0.8.33")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "hdbscan" version))
       (sha256
        (base32 "03gr70ys1zrnp15pxzhichvrdj5bj88p6p5k0wj8vx251rgvryjp"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'build-extensions
            (lambda _
              (invoke "python" "setup.py" "build_ext" "--inplace"))))))
    (propagated-inputs (list python-joblib
                             python-numpy
                             python-scikit-learn
                             python-scipy))
    (native-inputs (list python-cython
                         python-nose
                         python-pytest
                         python-pandas
                         python-networkx
                         python-setuptools
                         python-wheel))
    (home-page "https://github.com/scikit-learn-contrib/hdbscan")
    (synopsis "High performance implementation of HDBSCAN clustering")
    (description "HDBSCAN - Hierarchical Density-Based Spatial Clustering of
Applications with Noise.  Performs DBSCAN over varying epsilon values and
integrates the result to find a clustering that gives the best stability over
epsilon.  This allows HDBSCAN to find clusters of varying densities (unlike
DBSCAN), and be more robust to parameter selection.  HDBSCAN is ideal for
exploratory data analysis; it's a fast and robust algorithm that you can trust
to return meaningful clusters (if there are any).")
    (license license:bsd-3)))

(define-public python-pynndescent
  (package
    (name "python-pynndescent")
    (version "0.5.11")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pynndescent" version))
       (sha256
        (base32 "0l5dpdsk5vg7rpay81bncp04119hnl5z7zxjv63jrnm9spcwwi3g"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-pytest python-setuptools python-wheel))
    (propagated-inputs
     (list python-importlib-metadata
           python-joblib
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
    (version "1.0.1")
    (source
     (origin
       (method git-fetch) ; no tests in PyPI release
       (uri (git-reference
             (url "https://github.com/pavlin-policar/openTSNE")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0xjp0l4rxk1s685skbx50m3m9hwlj78w74qwgswnkmkk6f7c8dsi"))))
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
    (version "2.7.1")
    ;; The version on pypi does not come with tests.
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/cleanlab/cleanlab")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "073w45azq496x4bhrh8mdywcrg3gk33n13w1pqh1kiykw826ld9b"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; This test fails because the newer version of scikit learn returns one
      ;; more classification result than expected.  This should be harmless.
      '(list "-k" "not test_aux_inputs"
             ;; Requires Tensorflow
             "--ignore=tests/test_frameworks.py"
             ;; These need datasets, which needs jax, so it could only live in
             ;; the guix-science channel.
             "--ignore-glob=tests/datalab/**"
             ;; Tries to download datasets from the internet at runtime.
             "--ignore=tests/test_dataset.py"
             ;; Test requiring not packaged dataset.
             "--ignore=tests/spurious_correlation/test_correlation_visualizer.py"
             "--ignore=tests/spurious_correlation/test_spurious_correlation.py")
      #:phases
      '(modify-phases %standard-phases
         (add-after 'unpack 'remove-datasets
           (lambda _
             (delete-file "tests/datalab/conftest.py"))))))
    (propagated-inputs
     (list python-numpy
           python-pandas
           python-scikit-learn
           python-termcolor
           python-tqdm))
    (native-inputs
     (list ;; python-dataset ; https://github.com/huggingface/datasets
           python-pytest
           python-pytorch
           python-setuptools
           python-torchvision
           python-typing-extensions
           python-wheel))
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
     (list python-pytest python-setuptools python-wheel))))

(define-public python-cma
  (package
    (name "python-cma")
    (version "4.0.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "cma" version))
              (sha256
               (base32
                "198kdy2lxslml0g0s1sgc2zq1fsf2dh9s60lwshgrwivk1bcwa7x"))))
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
    (version "0.10.0")
    (source
     (origin
       (method git-fetch) ;no tests in PyPI
       (uri (git-reference
             (url "https://github.com/CyberAgentAILab/cmaes")
             (commit (string-append "v" version))))
       (sha256
        (base32 "17bk60mhkglz6s7wz6xcyhw1h4mvghc1iid0805dra7jdyafwrfn"))
       (file-name (git-file-name name version))))
    (build-system pyproject-build-system)
    (native-inputs (list python-hypothesis python-setuptools python-wheel))
    (propagated-inputs (list python-numpy))
    (home-page "https://github.com/CyberAgentAILab/cmaes")
    (synopsis "CMA-ES implementation for Python")
    (description "This package provides provides an implementation of the
Covariance Matrix Adaptation Evolution Strategy (CMA-ES) for Python.")
    (license license:expat)))

(define-public python-autograd
  (package
    (name "python-autograd")
    (version "1.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/HIPS/autograd")
             (commit (string-append "v" version))))
       (sha256
        (base32 "1fpnmm3mzw355iq7w751j4mjfcr0yh324cxidba1l22652gg8r8m"))
       (file-name (git-file-name name version))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-hatchling
           python-pytest))
    (propagated-inputs
     (list python-future
           python-numpy))
    (home-page "https://github.com/HIPS/autograd")
    (synopsis "Efficiently computes derivatives of NumPy code")
    (description
     "Autograd can automatically differentiate native Python and NumPy code.
It can handle a large subset of Python's features, including loops, ifs,
recursion and closures, and it can even take derivatives of derivatives of
derivatives.  It supports reverse-mode differentiation
(a.k.a. backpropagation), which means it can efficiently take gradients of
scalar-valued functions with respect to array-valued arguments, as well as
forward-mode differentiation, and the two can be composed arbitrarily.  The
main intended application of Autograd is gradient-based optimization.")
    (license license:expat)))

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
           python-setuptools ; For pkg_resources.
           python-six
           python-tqdm))
    (native-inputs
     (list python-black
           python-nose
           python-pymongo
           python-pytest
           python-wheel))
    (home-page "https://hyperopt.github.io/hyperopt/")
    (synopsis "Library for hyperparameter optimization")
    (description "Hyperopt is a Python library for serial and parallel
optimization over awkward search spaces, which may include real-valued,
discrete, and conditional dimensions.")
    (license license:bsd-3)))

(define-public python-deepxde
  (package
    (name "python-deepxde")
    (version "1.13.2")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "deepxde" version))
              (sha256
               (base32
                "11fm9c717pf2j5kb7skvy1nshdqz76m783ndxsd3q3lr5lkqg087"))))
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
    (native-inputs (list python-pytorch python-setuptools
                         python-setuptools-scm python-wheel))
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

(define-public python-jaxtyping
  (package
    (name "python-jaxtyping")
    (version "0.2.21")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "jaxtyping" version))
              (sha256
               (base32
                "19qmsnbn4wv2jl99lpn622qs49mrfxmx8s9pr5y8izzgdjq1fvii"))))
    (build-system pyproject-build-system)
    ;; Tests require JAX, but JAX can't be packaged because it uses the Bazel
    ;; build system.
    (arguments (list #:tests? #f))
    (native-inputs (list python-hatchling))
    (propagated-inputs (list python-numpy python-typeguard
                             python-typing-extensions))
    (home-page "https://github.com/google/jaxtyping")
    (synopsis
     "Type annotations and runtime checking for JAX arrays and others")
    (description "@code{jaxtyping} provides type annotations and runtime
checking for shape and dtype of JAX arrays, PyTorch, NumPy, TensorFlow, and
PyTrees.")
    (license license:expat)))

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
                                    "-L" openblas "/lib -lopenblas")))
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
         (add-after 'unpack 'python3.11-compatibility
           (lambda _
             ;; Py_TYPE was changed to an inline static function in Python
             ;; 3.11, so it cannot be used on the left-hand side.
             (substitute* "tensorflow/python/lib/core/bfloat16.cc"
               (("Py_TYPE\\(&NPyBfloat16_Descr\\) = &PyArrayDescr_Type;")
                "Py_SET_TYPE(&NPyBfloat16_Descr, &PyArrayDescr_Type);"))

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

             ;; ArgSpec has been replaced with FullArgSpec.
             (substitute* "tensorflow/python/util/tf_inspect.py"
               (("ArgSpec = _inspect.ArgSpec") "\
ArgSpec = namedtuple('ArgSpec', [ 'args', 'varargs', 'keywords', 'defaults' ])
def makeargspec(s):
  return ArgSpec(args=s.args, varargs=s.varargs, keywords=s.varkw, defaults=s.defaults)
")
               (("_inspect.getargspec\\((.*)\\)" m target)
                (string-append "makeargspec(_inspect.getfullargspec(" target "))")))

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
         (add-after 'python3.11-compatibility 'numpy-compatibility
           (lambda _
             (substitute* (cons* "tensorflow/compiler/xla/python/xla_client.py"
                                 "tensorflow/contrib/layers/python/ops/sparse_ops_test.py"
                                 (find-files "tensorflow/python/" "\\.py$"))
               (("np.object") "object"))
             (substitute* (append
                           '("tensorflow/compiler/tests/unary_ops_test.py"
                             "tensorflow/compiler/xla/python/xla_client.py"
                             "tensorflow/compiler/xla/python/xla_client_test.py")
                           (find-files "tensorflow/python/" "\\.py$")
                           (find-files "tensorflow/contrib/" "\\.py$"))
               (("np.bool,") "bool,")
               (("np.bool\\)") "bool)")
               (("np.bool:") "bool:"))))
         (add-after 'numpy-compatibility 'chdir
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
    (version "2.13.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tensorflow/tensorflow")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "09mfskmpvpbq919wibnw3bnhi1y3hkx3qrzm72gdr0gsivn1yb3w"))
       (patches (search-patches "tensorflow-lite-unbundle.patch"))))
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
                        #$(this-package-input "flatbuffers")
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
         "-Dgemmlowp_POPULATED=TRUE"
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
         (string-append "-Dgemmlowp_ROOT=" #$(this-package-input "gemmlowp")))
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
                        "-xf" (assoc-ref inputs "fft2d-src")))))
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
       ("flatbuffers" ,flatbuffers-23.1)
       ("gemmlowp" ,gemmlowp)
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
    (home-page "https://www.tensorflow.org")
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

(define-public dmlc-core-next
  ;; Apache Tvm needs the latest code from git commits in May 2024 or later.
  (let ((commit "13341857549852a9a86b1894b5ba84c6276ab381")
        (revision "1"))
    (package
      (inherit dmlc-core)
      (name "dmlc-core")
      (version (git-version "0.5" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/dmlc/dmlc-core")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1hj6h103lal3cm5mnry9lrm3d7aij89rxv46yv6y49vzd5hbnfbd")))))))

(define-public xgboost
  (package
    (name "xgboost")
    (version "1.7.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dmlc/xgboost")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (patches (search-patches "xgboost-use-system-dmlc-core.patch"))
       (sha256
        (base32 "16fbm5y3hn6ccflmbdlmn7krrdq7c0az3mxd8j1d23s9ky8niw05"))))
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
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f ; all tests require network access
      #:phases
      #~(modify-phases %standard-phases
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
                (("'install_lib': InstallLib,") ""))
              ;; Remove bad dataset.  This has been removed in scipy.
              (substitute* "tests/python/testing.py"
                (("TestDataset\\('boston', get_boston, 'reg:squarederror', 'rmse'\\),")
                 "")
                (("datasets.load_boston")
                 "datasets.load_digits"))))
          (add-after 'install 'install-version-and-libxgboost
            (lambda* (#:key inputs #:allow-other-keys)
              (let* ((pylib (string-append #$output "/lib/python"
                                           #$(version-major+minor
                                              (package-version python))
                                           "/site-packages"))
                     (xgbdir (string-append pylib "/xgboost"))
                     (version-file (string-append xgbdir "/VERSION"))
                     (libxgboost (string-append (assoc-ref inputs "xgboost")
                                                "/lib/libxgboost.so")))
                (with-output-to-file version-file
                  (lambda ()
                    (display #$(package-version xgboost))))
                (mkdir-p (string-append xgbdir "/lib"))
                (symlink libxgboost (string-append xgbdir "/lib"
                                                   "/libxgboost.so"))))))))
    (native-inputs
     (list python-pandas python-pytest python-scikit-learn python-setuptools
           python-wheel))
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
     (list python-pytest python-pytest-cov
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
           python-pytest-timeout
           python-pytest-xdist
           python-pyux
           python-sphinx
           python-requests))
    (home-page "https://keras.io/")
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
        (commit "81925d1c674c34f0dc34dd9a0f2151c1b6f701eb")
        (revision "2"))
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
           "16zs8ndbiv9nppn8bv6lfanzyyssz7g5pawxiqcnafwq3nvxpj9m"))))
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
                                   "-DCMAKE_CXX_STANDARD=17"
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
      (supported-systems %64bit-supported-systems)
      (license license:bsd-3))))

(define-public python-tensorly
  (package
    (name "python-tensorly")
    (version "0.9.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/tensorly/tensorly")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "01xdkhzwq7s18pp6433d4dhyblmlhjs87acagxh73vfsqyknb9h3"))))
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
    (native-inputs (list python-pytest python-pytest-cov python-sphinx
                         python-setuptools python-wheel))
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
    (version "0.5.6")
    (source
     (origin
       (method git-fetch)               ;no tests in pypi release
       (uri (git-reference
             (url "https://github.com/lmcinnes/umap")
             (commit (string-append "release-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0rb0nx0zwi5gddiqil20ssqwb45a8w9fk65bnam001kp7vqjb9ky"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; XXX: this one fails with unusually large deviation from the expected
      ;; result.
      '(list "-k" "not test_umap_update_large")
      #:phases
      #~(modify-phases %standard-phases
          ;; Numba needs a writable dir to cache functions.
          (add-before 'check 'set-numba-cache-dir
            (lambda _
              (setenv "NUMBA_CACHE_DIR" "/tmp"))))))
    (native-inputs (list python-pytest python-setuptools python-wheel))
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

;; Requires AVX2 on x86_64-linux.
(define-public nnpack
  (let ((version "0.0")
        (commit "70a77f485e8b934224f3a79efd8edcd84cd377b8")
        (revision "2"))
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
                  "0c4pw926279s3rlx7mg4l4vhnfy6dh374n6w7zqhcn0bxpym1hv1"))
                (patches (search-patches "nnpack-system-libraries.patch"))))
      (build-system cmake-build-system)
      ;; XXX: The test suite runs but it's very expensive. On x86_64-linux, it
      ;; requires AVX2 instructions.
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
       `(,python
         ,@(if (target-x86-64?) (list python-peachpy) '())
         ,python-six))
      ;; Supported for Linux.
      (supported-systems '("x86_64-linux" "armhf-linux" "aarch64-linux"))
      (license license:bsd-2))))

(define-public qnnpack
  (let ((commit "7d2a4e9931a82adc3814275b6219a03e24e36b4c")
        (revision "0"))
    (package
      (name "qnnpack")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/pytorch/qnnpack")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1dgzriiaz9arsrfwhx42y4l74wbzn6xvdmllfb66v4pmvi5gpxc5"))
         (modules '((guix build utils)))
         (snippet
          '(delete-file-recursively "deps"))))
      (build-system cmake-build-system)
      (arguments
       (list
        ;; The shared library build fails with linker errors, so we build the
        ;; static library with -fPIC as in the bundled PyTorch version.
        #:configure-flags
        ''("-DQNNPACK_LIBRARY_TYPE=static"
           "-DCMAKE_POSITION_INDEPENDENT_CODE=ON")
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'patch-cmake
              (lambda _
                (substitute* "CMakeLists.txt"
                  (("IF.*SOURCE_DIR.*")
                   "IF(FALSE)\n")
                  (("IF\\(NOT TARGET.*")
                   "IF(FALSE)\n")
                  (("TARGET_LINK_LIBRARIES.*(fxdiv|psimd|fp16)\\).*")
                   "")
                  (("(TARGET_LINK_LIBRARIES.*) fp16 (.*)" _ before after)
                   (string-append before " " after))))))))
      (inputs (list clog cpuinfo fp16 fxdiv psimd pthreadpool))
      (native-inputs (list googletest googlebenchmark))
      (home-page "https://github.com/pytorch/qnnpack")
      (synopsis "Quantized Neural Network PACKage")
      (description "QNNPACK is a library for low-precision neural network
inference.  It contains the implementation of common neural network operators
on quantized 8-bit tensors.")
      (supported-systems
       '("armhf-linux" "aarch64-linux" "i686-linux" "x86_64-linux"))
      (license license:bsd-3))))

(define-public xnnpack
  ;; There's currently no tag on this repo.
  (let ((version "0.0")
        (commit "08f1489fc815e8f121d4d2676c4863d2b51bfe73")
        (revision "3"))
    (package
      (name "xnnpack")
      (version (git-version version revision commit))
      (home-page "https://github.com/google/XNNPACK") ;fork of QNNPACK
      (source
       (origin
         (method git-fetch)
         (uri (git-reference (url home-page) (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "00jjhz0nfggbdnqqvcznba03pcyy7gssd24yhhzjhincnz9qh8jr"))
         (modules '((guix build utils)
                    (ice-9 ftw)
                    (ice-9 textual-ports)
                    (srfi srfi-26)))
         (snippet
          '(begin
             ;; Remove autogenerated files, which contain the string
             ;; "Auto-generated file"
             (for-each
              (lambda (dir)
                (for-each
                 (lambda (name)
                   (let ((path (string-append dir "/" name)))
                     (when (call-with-input-file path
                             (lambda (port)
                               (string-contains
                                (get-string-all port)
                                "Auto-generated file")))
                       (delete-file path))))
                 (scandir dir (negate (cut member <> '("." ".." "simd"))))))
              (cons*
               "test" "bench" "eval" "models" "src/enums" "src/xnnpack"
               "gen" "cmake/gen"
               (filter
                identity
                (map
                 (lambda (dir)
                   (let ((path
                          (string-append "src/" dir "/gen")))
                     (and (file-exists? path) path)))
                 (scandir "src" (negate (cut member <> '("." ".."))))))))))))
      (build-system cmake-build-system)
      (arguments
       (list
        #:build-type "Release" ;; Debugging symbols require a lot of disk space
        #:configure-flags ''("-DXNNPACK_USE_SYSTEM_LIBS=YES"
                             "-DBUILD_SHARED_LIBS=ON"
                             "-DCMAKE_POSITION_INDEPENDENT_CODE=ON"
                             "-DXNNPACK_LIBRARY_TYPE=shared"
                             "-DXNNPACK_BUILD_BENCHMARKS=FALSE"
                             ;; Tests fail to build with -DXNNPACK_LIBRARY_TYPE=shared:
                             ;; https://github.com/google/XNNPACK/issues/6285
                             "-DXNNPACK_BUILD_TESTS=OFF")
        #:tests? #f
        #:modules '((ice-9 ftw)
                    (guix build cmake-build-system)
                    (guix build utils))
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'fix-cmake
              (lambda _
                (substitute* "CMakeLists.txt"
                  (("TARGET_INCLUDE_DIRECTORIES\\((pthreadpool|cpuinfo).*") "")
                  (("AMD64") "x86_64"))))
            (add-after 'patch-source-shebangs 'generate-files
              (lambda _
                ;; This script just calls two other scripts.
                (delete-file "scripts/generate-tests-and-benchmarks.sh")
                ;; The bash scripts run all jobs at once and then wait, so we
                ;; convert them to Makefiles.
                (for-each
                 (lambda (name)
                   (define counter 0) ; For the targets
                   (define target-deps "")
                   (when (and (string-prefix? "generate" name)
                              (string-suffix? ".sh" name))
                     (let ((file (string-append "scripts/" name)))
                       (substitute* file
                         ;; Turn the commands into targets and remove trailing
                         ;; '&' characters
                         (("(.*(\\.sh|\\.py|-o |--output)[^&]*)&?[[:space:]]*$" _ command)
                          (begin
                            (set! counter (+ counter 1))
                            (string-append "target" (number->string counter)
                                           ":" target-deps
                                           "\n\t" command "\n")))
                         (("[[:space:]]*wait[[:space:]]*") "")
                         ;; The commands after this line depend on the
                         ;; previous commands in the file.
                         (("JIT requires assembly files to be generated first.*" all)
                          (begin
                            (set! target-deps
                                  (string-append
                                   target-deps " target"
                                   (string-join
                                    (map number->string (iota counter 1)) " target")))
                            all)))
                       (display (string-append "Running " name "\n"))
                       (apply invoke "make" "-s" "-f" file "-j"
                              (number->string (parallel-job-count))
                              (map
                               (lambda (i)
                                 (string-append "target" (number->string i)))
                               (iota counter 1))))))
                 (scandir "scripts"))
                ;; These need to run after the above scripts
                (display "Remaining files\n")
                (invoke "python3" "tools/update-microkernels.py")
                (invoke "python3" "tools/update-microkernels.py" "-a")
                (invoke "python3" "tools/generate-lut-norm-test.py"
                        "--spec" "test/u8-lut32norm.yaml"
                        "--output" "test/u8-lut32norm.cc")
                (invoke "python3" "tools/generate-gemm-test.py"
                        "--spec" "test/qd8-f16-qb4w-gemm-minmax.yaml"
                        "--output-test" "test/qd8-f16-qb4w-gemm-minmax.cc")
                (invoke "python3" "tools/generate-gemm-test.py"
                        "--spec" "test/qd8-f32-qb4w-gemm-minmax.yaml"
                        "--output-test" "test/qd8-f32-qb4w-gemm-minmax.cc"))))))
      (inputs
       (list clog
             cpuinfo
             pthreadpool
             googletest
             googlebenchmark
             fxdiv
             fp16
             psimd))
      (native-inputs
       (append (if (target-riscv64?)
                   ;; Required by "#include <riscv_vector.h>"
                   (list gcc-14)
                   (list))
               (list python-pyyaml python-wrapper)))
      (synopsis "Optimized floating-point neural network inference operators")
      (description
       "XNNPACK is a highly optimized library of floating-point neural network
inference operators for ARM, WebAssembly, and x86 platforms.  XNNPACK is not
intended for direct use by deep learning practitioners and researchers;
instead it provides low-level performance primitives for accelerating
high-level machine learning frameworks, such as TensorFlow Lite,
TensorFlow.js, PyTorch, and MediaPipe.")
      (supported-systems
       '("armhf-linux" "aarch64-linux" "riscv64-linux"
         "i686-linux" "x86_64-linux"))
      (license license:bsd-3))))

(define-public xnnpack-for-r-torch
  (let ((version "0.0")
        (commit "51a987591a6fc9f0fc0707077f53d763ac132cbf")
        (revision "2"))
    (package
      (inherit xnnpack)
      (version (git-version version revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference (url (package-home-page xnnpack)) (commit commit)))
         (file-name (git-file-name (package-name xnnpack) version))
         (sha256
          (base32
           "1rzby82xq8d0rl1d148yz88jh9cpsw5c8b2yw7yg39mi7qmr55rm"))
         (modules '((guix build utils)
                    (ice-9 ftw)
                    (ice-9 textual-ports)
                    (srfi srfi-26)))
         (snippet
          '(begin
             ;; Remove autogenerated files
             (for-each
              (lambda (dir)
                (let ((gendir (string-append "src/" dir "/gen")))
                  (when (file-exists? gendir)
                    (delete-file-recursively gendir)
                    ;; Needed for the scripts generating the files
                    (mkdir gendir))))
              (scandir "src" (negate (cut member <> '("." "..")))))
             (delete-file-recursively "google3")
             (delete-file "cmake/microkernels.cmake")
             ;; Additional autogenerated files which contain the string
             ;; "Auto-generated file"
             (for-each
              (lambda (dir)
                (for-each
                 (lambda (name)
                   (let ((path (string-append dir "/" name)))
                     (when (call-with-input-file path
                             (lambda (port)
                               (string-contains
                                (get-string-all port)
                                "Auto-generated file")))
                       (delete-file path))))
                 (scandir dir (negate (cut member <> '("." ".."))))))
              '("test" "bench" "eval" "models" "src/enums" "src/xnnpack"))))))
      (arguments
       (substitute-keyword-arguments (package-arguments xnnpack)
         ((#:phases phases)
          #~(modify-phases #$phases
              (replace 'generate-files
                (lambda _
                  (for-each
                   (lambda (name)
                     (when (and (string-prefix? "generate" name)
                                (string-suffix? ".sh" name)
                                (not (equal? "generate-amalgamation.sh" name)))
                       (display (string-append name "\n"))
                       (invoke "bash" (string-append "scripts/" name))))
                   (scandir "scripts"))
                  ;; These need to run after the above scripts
                  (display "Remaining files\n")
                  (invoke "python3" "tools/update-microkernels.py")
                  (substitute* "tools/amalgamate-microkernels.py"
                    (("BUILD") "BUILD.bazel"))
                  (invoke "bash" "scripts/generate-amalgamation.sh"))))))))))

;; Warning: This package requires AVX2 or AVX-512 instructions.
(define-public fbgemm
  (package
    (name "fbgemm")
    (version "1.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/pytorch/fbgemm")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1a5g5f32377fad99xsfggqkwvl7vh5gc1wj77swa06x06lc1qwyw"))
              (patches (search-patches "fbgemm-use-system-libraries.patch"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:cmake cmake-3.30
      #:configure-flags
      ''("-DFBGEMM_LIBRARY_TYPE=shared")
      ;; Tests require AVX2 or AVX-512 instructions
      #:tests? #f))
    (inputs (list asmjit cpuinfo))
    (native-inputs (list pkg-config python googletest))
    (home-page "https://github.com/pytorch/fbgemm")
    (synopsis "Facebook GEneral Matrix Multiplication")
    (description "Low-precision, high-performance matrix-matrix
multiplications and convolution library for server-side inference.")
    (supported-systems '("x86_64-linux"))
    (license license:bsd-3)))

(define-public tensorpipe
  (let ((commit "bb1473a4b38b18268e8693044afdb8635bc8351b")
        (revision "0"))
    (package
      (name "tensorpipe")
      (version (git-version "0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/pytorch/tensorpipe")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0sbpkd69rzybw2j89sjkf4s0j8vkk96d51bsps28894989a75j6v"))
                (modules '((guix build utils)))
                (snippet
                 '(delete-file-recursively "third_party"))))
      (build-system cmake-build-system)
      (arguments
       (list
        #:configure-flags
        ''("-DBUILD_SHARED_LIBS=ON")
        ;; There are no tests
        #:tests? #f))
      (inputs (list libuv))
      (native-inputs (list googletest pkg-config pybind11 libnop))
      (home-page "https://github.com/pytorch/tensorpipe")
      (synopsis "Tensor-aware point-to-point communication primitive for
machine learning")
      (description "TensorPipe provides a tensor-aware channel to transfer
rich objects from one process to another while using the fastest transport for
the tensors contained therein.")
      (license license:bsd-3))))

(define-public tvm
  ;; Include a bug fix post 0.19 release.
  (let ((commit "d3a2ed68a42f8b51d8ab1533b62e837b9ea74b8e")
        (revision "1"))
    (package
      (name "tvm")
      (version (git-version "0.20.dev0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/apache/tvm")
               (commit commit)))
         (patches (search-patches "tvm_fix_cpptest_build.patch"))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0ng52i0aydljnlrkyg069838c8b311fynhy5976w74smma2m7v72"))))
      (build-system cmake-build-system)
      (arguments
       (list
        #:test-target "cpptest"
        #:configure-flags
        #~(list "-DUSE_OPENCL=ON"
                "-DUSE_VULKAN=ON"
                "-DUSE_OPENCL_ENABLE_HOST_PTR=ON"
                "-DINSTALL_DEV=ON"
                "-DUSE_GTEST=ON"
                "-DHIDE_PRIVATE_SYMBOLS=ON" ;per upstream build instructions
                "-DUSE_LLVM=llvm-config\\ --ignore-libllvm\\ --link-static"
                ;; per upstream build instructions
                (string-append "-DDLPACK_PATH="
                               (assoc-ref %build-inputs "dlpack") "/include")
                (string-append "-DDMLC_PATH="
                               (assoc-ref %build-inputs "dmlc-core")
                               "/include")
                (string-append "-DRANG_PATH="
                               (assoc-ref %build-inputs "rang") "/include"))
        #:phases
        #~(modify-phases %standard-phases
            (replace 'check
              (lambda* (#:key source test-target tests? #:allow-other-keys)
                (when tests?
                  (begin
                    (invoke "make" "-j"
                            (number->string (parallel-job-count)) test-target)
                    ;; Disable below the actual run of the tests because
                    ;; several fail due to platform variations (for example,
                    ;; fp16 tests fail because not supported on CPUs).
                    ;;
                    ;; (chdir "..")
                    ;; (invoke (which "bash")
                    ;;         (string-append source
                    ;;                        "/tests/scripts/task_cpp_unittest.sh"))
                    )))))))
      (inputs (list dmlc-core-next
                    dlpack
                    libedit
                    libxml2
                    llvm-19
                    opencl-clhpp
                    opencl-headers
                    rang
                    mesa
                    mesa-opencl
                    spirv-headers
                    spirv-tools
                    vulkan-headers ;TODO; now not building due to missing vta-hw
                    vulkan-loader
                    zlib
                    zstd))
      (native-inputs (list bash-minimal
                           gcc-14
                           googletest
                           (module-ref (resolve-interface '(gnu packages debug))
                                       'libbacktrace)
                           pkg-config
                           python
                           python-cython))
      (home-page "https://tvm.apache.org/")
      (synopsis
       "Machine learning compiler framework for CPUs, GPUs and accelerators")
      (description
       "Apache TVM is a compiler stack for deep learning systems.  It is
designed to close the gap between the productivity-focused deep learning
frameworks, and the performance- and efficiency-focused hardware backends.
TVM works with deep learning frameworks to provide end to end compilation to
different backends")
      (license license:asl2.0))))

(define-public foxi
  (let
      ((commit "c278588e34e535f0bb8f00df3880d26928038cad")
       (revision "0"))
    (package
      (name "foxi")
      (version (git-version "1.4.1" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/houseroad/foxi")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0q3ssm5hmmvwfwx87mnnajbavzgpahybw6rpn8ysr9r095dwgq5a"))
                (patches (search-patches "foxi-fix-build.patch"))))
      (build-system cmake-build-system)
      (arguments
       (list
        ;; No tests
        #:tests? #f))
      (home-page "https://github.com/houseroad/foxi")
      (synopsis "ONNXIFI with Facebook Extension")
      (description "ONNX Interface for Framework Integration is a cross-platform
API for loading and executing ONNX graphs on optimized backends.  This package
contains facebook extensions and is used by PyTorch.")
      (license license:expat))))

(define-public ideep-pytorch
  (package
    (name "ideep-pytorch")
    (version "3.3.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/intel/ideep")
             (commit (string-append "pytorch-rls-v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0y6r938qryi3bnf15rp0fbilsfimdcgmvsa0ygwrn3zifw6386rb"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:install-plan
      ''(("include" "include"))))
    (home-page "https://github.com/intel/ideep")
    (synopsis "Ideep headers for internal use by PyTorch")
    (description "This library is used internally as header-only library by
PyTorch.")
    (license license:expat)))

(define-public ideep-pytorch-for-r-torch
  (package
    (inherit ideep-pytorch)
    (version "2.7.3-1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/intel/ideep")
             (commit (string-append "pytorch-rls-v" version))))
       (file-name (git-file-name (package-name ideep-pytorch) version))
       (sha256
        (base32
         "0hdpkhcjry22fjx2zg2r48v7f4ljrclzj0li2pgk76kvyblfbyvm"))))))

(define %python-pytorch-version "2.5.1")

(define %python-pytorch-src
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/pytorch/pytorch")
          (commit (string-append "v" %python-pytorch-version))))
    (file-name (git-file-name "python-pytorch" %python-pytorch-version))
    (sha256
     (base32
      "052cvagpmm9y7jspjpcyysx8yc5fhxnjl8rcz6nndis06v8dcj8s"))
    (patches (search-patches "python-pytorch-system-libraries.patch"
                             "python-pytorch-runpath.patch"
                             "python-pytorch-without-kineto.patch"
                             ;; Some autogeneration scripts depend on the
                             ;; compile PyTorch library. Therefore, we create
                             ;; dummy versions which are regenerated later.
                             "python-pytorch-fix-codegen.patch"))
    (modules '((guix build utils)))
    (snippet
     '(begin
        ;; Bundled or unused code
        (for-each
         (lambda (dir)
           (when (file-exists? dir)
             (delete-file-recursively dir)))
         '("android"
           "aten/src/ATen/native/cuda/cutlass_extensions"
           "aten/src/ATen/native/quantized/cpu/qnnpack"
           "caffe2/mobile/contrib/libopencl-stub"
           "caffe2/mobile/contrib/libvulkan-stub"
           "third_party"))

        ;; Autogenerated files
        (for-each
         delete-file
         '("aten/src/ATen/nnapi/nnapi_wrapper.cpp"
           "aten/src/ATen/nnapi/nnapi_wrapper.h"
           ;; These files contain just lists of floating point values and
           ;; might be as well hand-written.
           ;; "test/cpp/api/init_baseline.h"
           ;; "test/cpp/api/optim_baseline.h"
           "test/mobile/test_upgrader_bytecode_table_example.cpp"
           "torch/csrc/jit/mobile/upgrader_mobile.cpp"
           "torch/csrc/jit/runtime/decomposition_registry_util.cpp"
           "torch/csrc/jit/runtime/serialized_shape_function_registry.cpp"
           "torch/csrc/jit/tensorexpr/external_functions_codegen.cpp"
           "torch/csrc/jit/serialization/mobile_bytecode_generated.h"))
        (delete-file-recursively ".github")
        (for-each
         (lambda (dir)
           (for-each
            delete-file
            (find-files dir "\\.cu$")))
         '("aten/src/ATen/native/transformers/cuda/flash_attn/kernels"
           "aten/src/ATen/native/transformers/cuda/mem_eff_attention/kernels"))))))

(define-public qnnpack-pytorch
  (package
    (inherit qnnpack)
    (name "qnnpack-pytorch")
    (version (string-append "pytorch-" %python-pytorch-version))
    (source
     (origin
       (inherit %python-pytorch-src)
       (patches '())
       (modules '((guix build utils)
                  (srfi srfi-26)
                  (ice-9 ftw)))
       (snippet
        '(begin
           (rename-file "aten/src/ATen/native/quantized/cpu/qnnpack"
                        "../qnnpack")
           (let ((outdir (getcwd)))
             (chdir "..")
             (rename-file outdir "dummy")
             (rename-file "qnnpack" outdir)
             (chdir outdir)
             (delete-file-recursively "deps"))))))
    (arguments
     (substitute-keyword-arguments (package-arguments qnnpack)
       ((#:phases phases #~%standard-phases)
        #~(modify-phases %standard-phases
            (add-after 'unpack 'patch-cmake
              (lambda _
                (substitute* "CMakeLists.txt"
                  (("project\\(.*" orig)
                   (apply
                    string-append
                    orig "\n"
                    (map (lambda (name)
                           (string-append
                            "option(" name " \"\" ON)\n"))
                         '("USE_SYSTEM_CPUINFO" "USE_SYSTEM_FP16" "USE_SYSTEM_FXDIV"
                           "USE_SYSTEM_PSIMD" "USE_SYSTEM_PTHREADPOOL"))))
                  (("if.*SOURCE_DIR.*")
                   "if(FALSE)\n")
                  (("if\\(NOT TARGET (clog|gtest|benchmark).*")
                   "if(FALSE)\n")
                  (("target_link_libraries.*(fxdiv|psimd|fp16)\\).*")
                   "")
                  (("(target_link_libraries.*) fp16 (.*)" _ before after)
                   (string-append before " " after)))))
            (add-after 'unpack 'fix-cstring-include
              (lambda _
                (substitute* "include/pack_block_sparse.h"
                  (("#include.*<vector>.*" orig)
                   (string-append orig "\n#include <cstring>\n")))))
            (add-after 'install 'install-missing-headers
              (lambda _
                (for-each
                 (lambda (name)
                   (install-file (string-append "../source/include/" name)
                                 (string-append #$output "/include")))
                 '("pack_block_sparse.h"
                   "pytorch_qnnpack.h"
                   "qnnpack_func.h"))
                (copy-recursively
                 "../source/src/qnnpack"
                 (string-append #$output "/include/qnnpack"))))))
       ;; Some tests occasionally fail on i686 due to floating point rounding.
       ((#:tests? _ #t)
        (not (string-prefix? "i686" (or (%current-target-system)
                                        (%current-system)))))))))

;; Please also update python-torchvision when updating this package.
(define-public python-pytorch
  (package
    (name "python-pytorch")
    (version %python-pytorch-version)
    (source %python-pytorch-src)
    (build-system python-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'cmake-patches
            (lambda _
              (substitute* "cmake/Dependencies.cmake"
                (("#POCKETFFT_INCLUDE_DIR")
                 (string-append
                  #$(this-package-native-input "pocketfft-cpp") "/include"))
                (("#FP16_INCLUDE_DIR")
                 (string-append
                  #$(this-package-input "fp16") "/include"))
                ;; Disable opentelemetry
                ((".*(add_library|target_include_directories).*opentelemetry.*")
                 ""))
              (substitute* "torch/CMakeLists.txt"
                ((".*opentelemetry.*") ""))
              ;; Fix Python install directory
              (substitute* "caffe2/CMakeLists.txt"
                (("\\$\\{Python_SITELIB\\}")
                 (string-append #$output "/lib/python"
                                #$(version-major+minor (package-version python))
                                "/site-packages")))))
          ;; This entry point is broken, because it refers to a module that is
          ;; (intentionally) not installed
          ;; (https://github.com/pytorch/pytorch/pull/134729), which causes
          ;; the 'sanity-check phase to fail.
          (add-after 'unpack 'remove-fr-trace-script
            (lambda _
              (substitute* "setup.py"
                (("entry_points\\[\"console_scripts\"\\]\\.append\\(") "("))))
          (add-before 'build 'use-system-libraries
            (lambda _
              (substitute* '("caffe2/serialize/crc.cc"
                             "caffe2/serialize/inline_container.cc")
                (("\"miniz\\.h\"") "<miniz/miniz.h>"))
              (substitute* "aten/src/ATen/native/vulkan/api/Allocator.h"
                (("<include/vk_mem_alloc.h>")
                 "<vk_mem_alloc.h>"))
              ;; For Vulkan
              (substitute* "CMakeLists.txt"
                (("append_cxx_flag.*-Werror=(return-type|range-loop-construct).*") ""))
              (substitute*
                  (cons*
                   "torch/csrc/Module.cpp"
                   (map
                    (lambda (name)
                      (string-append
                       "torch/utils/benchmark/utils/valgrind_wrapper/"
                       name))
                    '("compat_bindings.cpp" "timer_callgrind_template.cpp")))
                (("<callgrind.h>") "<valgrind/callgrind.h>"))
              (setenv "USE_VULKAN" "1")
              ;; Tell 'setup.py' to let 'CMakeLists.txt' know that we
              ;; want to use "system libraries" instead of the bundled
              ;; ones.
              (setenv "USE_SYSTEM_LIBS" "1")
              ;; For oneDNN
              (setenv "USE_MKLDNN" "1")
              ;; Only works with CUPTI
              (setenv "USE_KINETO" "0")
              ;; Prevent CMake error by disabling explicitely
              (setenv "USE_ITT" "0")
              ;; Disable on unsupported systems
              (if #$(not (member
                          (or (%current-target-system)
                              (%current-system))
                          (package-transitive-supported-systems qnnpack)))
                  (setenv "USE_QNNPACK" "0"))
              (substitute* '("requirements.txt" "setup.py")
                (("sympy==1\\.13\\.1")
                 "sympy>=1.13.1"))))
          ;; PyTorch is still built with AVX2 and AVX-512 support selected at
          ;; runtime, but these dependencies require it (nnpack only for
          ;; x86_64).
          (add-before 'build 'disable-avx-dependencies
            (lambda _
              (setenv "USE_FBGEMM" "0")
              (if #$(not
                     (member (or (%current-target-system)
                                 (%current-system))
                             '("armhf-linux" "aarch64-linux")))
                  (setenv "USE_NNPACK" "0"))))
          (add-after 'use-system-libraries 'set-max-jobs
            (lambda _
              (setenv "MAX_JOBS" (number->string (parallel-job-count)))))
          (add-after 'set-max-jobs 'codegen1
            (lambda _
              (with-directory-excursion "torch/csrc/jit/tensorexpr"
                (setenv "PYTHONPATH" "../../../..")
                (invoke "python3" "codegen_external.py")
                (setenv "PYTHONPATH" #f))

              (invoke "python3" "aten/src/ATen/nnapi/codegen.py")

              (invoke "bash" "tools/gen_flatbuffers.sh")

              ;; Generate dummy files as the generation depends on the compiled
              ;; library. They are regenerated later.
              (setenv "PYTHONPATH" ".")
              (invoke "python3"
                      "torchgen/operator_versions/gen_mobile_upgraders.py"
                      "dummy")
              (setenv "PYTHONPATH" #f)

              (invoke "python3"
                      "torchgen/shape_functions/gen_jit_shape_functions.py"
                      "dummy")

              (invoke "python3"
                      "torchgen/decompositions/gen_jit_decompositions.py"
                      "dummy")))
          ;; Properly generate autogenerated files ...
          (add-after 'install 'codegen2
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (add-installed-pythonpath inputs outputs)
              (invoke "python3"
                      "torchgen/operator_versions/gen_mobile_upgraders.py")
              (invoke "python3"
                      "torchgen/shape_functions/gen_jit_shape_functions.py")
              (invoke "python3"
                      "torchgen/decompositions/gen_jit_decompositions.py")))
          ;; ... rebuild their dependencies ...
          (add-after 'codegen2 'build2
            (lambda _
              (invoke "python3" "setup.py" "build")))
          ;; ... and install again.
          (add-after 'build2 'install2
            (lambda _
              (invoke "python3" "setup.py" "install" (string-append "--prefix=" #$output)
                      "--no-compile" "--single-version-externally-managed" "--root=/")
              (invoke "python" "-m" "compileall"
                      "--invalidation-mode=unchecked-hash" #$output)))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              ;; Run the test suite following the instructions in
              ;; 'CONTRIBUTING.md'. Unfortunately this doesn't work, unless
              ;; you set PYTHONPATH or GUIX_PYTHONPATH, but this is done in
              ;; the codegen2 phase already.
              (when tests?
                (invoke "python3" "test/run_test.py" "--core"))))
          (add-after 'install2 'remove-test-executables
            (lambda* (#:key inputs outputs #:allow-other-keys)
              ;; Remove test executables, but keep other executables
              ;; such as 'torch_shm_manager' and and .so files such as
              ;; 'libtorch_global_deps.so'.
              (let ((python-site (site-packages inputs outputs)))
                (for-each delete-file
                          (find-files python-site
                                      "(^test_cpp_rpc|_test)$")))))
          (add-after 'install2 'remove-caffe2-onnx-scripts
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (bin (string-append out "/bin")))
                ;; Remove 'convert-caffe2-to-onnx' and
                ;; 'convert-onnx-to-caffe2': they seem to be
                ;; deprecated and they cause a failure of the
                ;; 'sanity-check' phase:
                ;;
                ;; ImportError: cannot import name 'metanet_pb2' from
                ;; partially initialized module 'caffe2.proto' (most likely
                ;; due to a circular import)
                (for-each delete-file
                          (find-files bin "^convert-.*caffe2"))

                (substitute* (find-files out "^entry_points\\.txt$")
                  (("^convert-.*" all)
                   (string-append "# " all "\n")))))))

      ;; Even only the core tests take a very long time to run.
      #:tests? #f))
    (native-inputs
     (list cmake-minimal
           doxygen
           ideep-pytorch
           ninja
           nlohmann-json
           pocketfft-cpp
           python-expecttest
           python-pytest-flakefinder
           python-pytest-rerunfailures-13
           python-pytest-shard
           python-pytest-xdist
           python-hypothesis
           python-types-dataclasses
           shaderc
           valgrind))
    (inputs
     (append
      (list asmjit
            brotli ; for cpp-httplib
            clog
            cpp-httplib
            eigen
            flatbuffers-next
            fmt
            fp16
            fxdiv
            gemmlowp
            gloo
            googletest
            googlebenchmark
            libuv
            miniz-for-pytorch
            oneapi-dnnl
            openblas
            openmpi
            openssl ; for cpp-httplib
            pthreadpool
            protobuf
            pybind11
            ;; qnnpack
            qnnpack-pytorch
            sleef
            tensorpipe
            vulkan-headers
            vulkan-loader
            vulkan-memory-allocator
            xnnpack
            zlib ; for cpp-httplib
            zstd)
      ;; nnpack requires AVX2 for x86_64-linux
      (if (equal? (or (%current-target-system)
                      (%current-system))
                  '("aarch64-linux"))
          (list nnpack)
          '())))
    (propagated-inputs
     (append
      (list cpuinfo
            onnx ;propagated for its Python modules
            onnx-optimizer
            python-astunparse
            python-click
            python-filelock
            python-fsspec
            python-future
            python-jinja2
            python-networkx
            python-numpy
            python-opt-einsum
            python-optree
            python-packaging
            python-psutil
            python-pyyaml
            python-requests
            python-sympy
            python-typing-extensions)))
    (home-page "https://pytorch.org/")
    (synopsis "Python library for tensor computation and deep neural networks")
    ;; TODO: Support other 64-bit systems.
    (supported-systems '("x86_64-linux" "aarch64-linux"))
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

;; This package variant includes the dependencies requiring at least AVX2 or
;; AVX-512.
(define-public python-pytorch-avx
  (package/inherit python-pytorch
    (name "python-pytorch-avx")
    (inputs
     (modify-inputs (package-inputs python-pytorch)
       (append fbgemm nnpack)))
    (arguments
     (substitute-keyword-arguments (package-arguments python-pytorch)
      ((#:phases phases)
       #~(modify-phases #$phases
           (delete 'disable-avx-dependencies)))))
    (supported-systems '("x86_64-linux"))))

(define %python-pytorch-for-r-torch-version "2.0.1")

(define %python-pytorch-for-r-torch-src
  (origin
    (inherit %python-pytorch-src)
    (uri (git-reference
          (url "https://github.com/pytorch/pytorch")
          (commit (string-append "v" %python-pytorch-for-r-torch-version))))
    (file-name (git-file-name "python-pytorch"
                              %python-pytorch-for-r-torch-version))
    (sha256
     (base32
      "0iirrn687i7sfv0p0i7dn89x3rf13a7l8y1y5h190h51yjxpxqxa"))
    (patches (search-patches
              "python-pytorch-for-r-torch-system-libraries.patch"
              "python-pytorch-runpath.patch"
              "python-pytorch-without-kineto.patch"
              ;; Some autogeneration scripts depend on the
              ;; compile PyTorch library. Therefore, we create
              ;; dummy versions which are regenerated later.
              "python-pytorch-for-r-torch-fix-codegen.patch"))))

(define-public qnnpack-pytorch-for-r-torch
  (package
    (inherit qnnpack-pytorch)
    (version (string-append "pytorch-" %python-pytorch-for-r-torch-version))
    (source
     (origin
       (inherit %python-pytorch-for-r-torch-src)
       (patches '())
       (modules '((guix build utils)
                  (srfi srfi-26)
                  (ice-9 ftw)))
       (snippet
        (origin-snippet (package-source qnnpack-pytorch)))))))

;; Keep in sync with r-torch
(define-public python-pytorch-for-r-torch
  (package
    (inherit python-pytorch)
    (name "python-pytorch")
    (version %python-pytorch-for-r-torch-version)
    (source %python-pytorch-for-r-torch-src)
    (arguments
     (substitute-keyword-arguments (package-arguments python-pytorch)
       ((#:phases phases)
        #~(modify-phases #$phases
            ;; See https://github.com/pytorch/pytorch/issues/61244
            (add-after 'unpack 'fix-aten-vec
              (lambda _
                (substitute*
                    '("aten/src/ATen/cpu/vec/vec512/vec512_bfloat16.h"
                      "aten/src/ATen/cpu/vec/vec256/vec256_bfloat16.h")
                  (("map\\(const __") "map(__"))))))))
    (native-inputs
     (modify-inputs (package-native-inputs python-pytorch)
       (replace "ideep-pytorch" ideep-pytorch-for-r-torch)))
    (inputs
     (modify-inputs (package-inputs python-pytorch)
       (prepend foxi)
       (prepend qnnpack)
       (replace "qnnpack-pytorch" qnnpack-pytorch-for-r-torch)
       (replace "oneapi-dnnl" oneapi-dnnl-for-r-torch)
       (replace "xnnpack" xnnpack-for-r-torch)))
    (propagated-inputs
     (modify-inputs (package-propagated-inputs python-pytorch)
       (append python-filelock
               python-jinja2
               python-networkx
               python-opt-einsum
               python-sympy)))))

(define-public python-pytorch-geometric
  (package
    (name "python-pytorch-geometric")
    (version "2.6.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/pyg-team/pytorch_geometric/")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0dbxz9d22vzm7fr9kgg66hj3sf8ag2ly8qky58cxvn1hyjl5h3v7"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; Hangs with AttributeError: 'NoneType' object has no attribute 'rpc_async'
      '(list "--ignore=test/distributed/test_rpc.py"
             ;; A message passing jinja template is missing
             "--ignore=test/nn/conv/test_message_passing.py"
             "--ignore=test/nn/test_sequential.py"
             "--ignore=test/nn/models/test_basic_gnn.py"
             ;; These all fail with a size mismatch error such as
             ;; RuntimeError: shape '[-1, 2, 1, 1]' is invalid for input of size 3
             "--ignore=test/explain/algorithm/test_captum_explainer.py"
             ;; Requires the nonfree MKL on CPU.
             "--ignore=test/nn/models/test_graph_unet.py"
             "-k" (string-append
                   ;; Permissions error
                   "not test_packaging"
                   ;; Unknown multiprocessing failure
                   " and not test_data_share_memory"
                   ;; This can fail due to accuracy problems
                   " and not test_gdc"
                   ;; These refuse to be run on CPU and really want a GPU
                   " and not test_add_random_walk_pe"
                   " and not test_asap"
                   " and not test_two_hop"
                   ;; Failed when switched to python@3.11
                   ;; typing module internals
                   " and not test_type_repr"))
      #:phases
      '(modify-phases %standard-phases
         (add-after 'unpack 'delete-top-level-directories
           (lambda _
             ;; The presence of these directories confuses the pyproject build
             ;; system.
             (for-each delete-file-recursively
                       '("conda" "docker" "graphgym")))))))
    (propagated-inputs
     (list onnx
           python-captum
           python-graphviz
           python-h5py
           python-jinja2
           python-matplotlib
           python-networkx
           python-numba
           python-numpy
           python-opt-einsum
           python-pandas
           python-protobuf
           python-psutil
           python-pyparsing
           python-pytorch-lightning
           python-rdflib
           python-requests
           python-scikit-image
           python-scikit-learn
           python-scipy
           python-statsmodels
           python-sympy
           python-tabulate
           python-torchmetrics
           python-tqdm))
    (native-inputs
     (list python-flit-core
           python-pytest
           python-pytest-cov))
    (home-page "https://pyg.org")
    (synopsis "Graph Neural Network library for PyTorch")
    (description
     "PyG is a library built upon PyTorch to easily write and train Graph
Neural Networks for a wide range of applications related to structured data.")
    (license license:expat)))

(define-public python-lightning-cloud
  (package
    (name "python-lightning-cloud")
    (version "0.5.70")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "lightning_cloud" version))
              (sha256
               (base32
                "11xx7w7ypyf6bzwz7pbdakap68a1lnsv3icis8wm8magkfglash2"))))
    (arguments (list #:tests? #f))      ; no tests in PyPI archive.
    (build-system pyproject-build-system)
    (propagated-inputs (list python-boto3
                             python-click
                             python-fastapi
                             python-multipart
                             python-protobuf
                             python-pyjwt
                             python-requests
                             python-rich
                             python-six
                             python-urllib3
                             python-uvicorn
                             python-websocket-client))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://lightning.ai")
    (synopsis "Lightning Cloud command line client")
    (description "This package provides a command line interface for Lightning
AI services.")
    (license license:asl2.0)))

(define-public python-lightning-utilities
  (package
    (name "python-lightning-utilities")
    (version "0.11.6")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "lightning_utilities" version))
              (sha256
               (base32
                "016zikn39apig3y6xyipw34w0w02c73z483radddbf68ivpjgz3r"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-coverage
           python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-importlib-metadata
           python-packaging
           python-typing-extensions))
    (home-page "https://github.com/Lightning-AI/utilities")
    (synopsis "PyTorch Lightning sample project")
    (description "This package provides common Python utilities and GitHub
Actions for the Lightning suite of libraries.")
    (license license:asl2.0)))

(define-public python-captum
  (package
    (name "python-captum")
    (version "0.7.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/pytorch/captum")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0bgfwnlsi50hbmknn7qljiy93fi6ggwz3k7yk9kj7s37mhzaylym"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      '(list "-k"
             ;; These two tests (out of more than 1000 tests) fail because of
             ;; accuracy problems.
             "not test_softmax_classification_batch_multi_target\
 and not test_softmax_classification_batch_zero_baseline")))
    (propagated-inputs
     (list python-matplotlib python-numpy python-pytorch python-tqdm))
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
                         python-scikit-learn
                         python-setuptools))
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
    (native-inputs (list python-wheel))
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
  (let ((commit "9177ec09caadcf88859e1f1e3e10a18e8832069a")
        (revision "0"))
    (package
      (name "python-pytorch-lightning")
      (version (git-version "2.5.0.post0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/Lightning-AI/pytorch-lightning")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0n1dqqaxga0s02l04vy9jfphx7g20m4as17jaxl5bgwzrjfh6k2f"))))
      (build-system pyproject-build-system)
      (arguments
       (list
        #:test-flags
        ;; The tests train a model.  They are much too expensive for our
        ;; purposes, so we only run the core tests.
        '(list "-m" "not cloud and not tpu"
               "tests/tests_pytorch/core"
               "-k"
               (string-append
                ;; Some multiprocessing complaint.
                "not test_result_reduce_ddp"
                ;; FutureWarning is raised.
                " and not test_result_collection_restoration"
                ;; These need tensorboard
                " and not test_property_logger"
                " and not test_property_loggers"))
        #:phases
        '(modify-phases %standard-phases
           (add-after 'unpack 'patch-version-detection
             (lambda _
               ;; We do have pytorch 2.4.0, but the version comparison fails.
               (substitute* "src/lightning/fabric/utilities/imports.py"
                 (("_TORCH_GREATER_EQUAL_2_4 =.*")
                  "_TORCH_GREATER_EQUAL_2_4 = True\n"))))
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
             python-fastapi
             python-fsspec
             python-inquirer
             python-jsonargparse
             python-lightning-cloud
             python-lightning-utilities
             python-numpy
             python-packaging
             python-pytorch
             python-pyyaml
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
      (license license:asl2.0))))

(define-public python-torchmetrics
  (package
    (name "python-torchmetrics")
    (version "1.4.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Lightning-AI/torchmetrics")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0371kx2fpp46rlhzkafa7397kp1lirgykpzk9g12kxsqypb67v1l"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; Tests require many additional dependencies
      #:tests? #f))
    (propagated-inputs
     (list python-numpy python-packaging python-pytorch
           python-typing-extensions
           python-lightning-utilities))
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
    (home-page "https://github.com/Lightning-AI/torchmetrics")
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
    (version "0.20.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/pytorch/vision")
                    (commit (string-append "v" version))
                    (recursive? #t)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1hxcpg44bjnfzqwihzbnfgd0gpkhfgqrcg116mnvdn0fpbhf4yq5"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (delete-file-recursively "android")
                  (delete-file-recursively "ios")))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; The test suite is expensive and there is no easy way to subset it.
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'setenv
            (lambda _
              (let ((jpegdir #$(this-package-input "libjpeg-turbo")))
                (setenv "TORCHVISION_INCLUDE"
                        (string-append jpegdir "/include/"))
                (setenv "TORCHVISION_LIBRARY"
                        (string-append jpegdir "/lib/"))))))))
    (inputs
     (list ffmpeg
           libpng
           libjpeg-turbo))
    (propagated-inputs
     (list python-numpy
           python-typing-extensions
           python-requests
           python-pillow
           python-pillow-simd
           python-pytorch))
    (native-inputs
     (list pybind11
           python-pytest
           python-setuptools
           python-wheel
           which))
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

(define-public python-geomloss
  (package
    (name "python-geomloss")
    (version "0.2.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "geomloss" version))
       (sha256
        (base32 "1szsjpcwjlvqiiws120fwn581a6hs8gm9si8c75v40ahbh44f729"))))
    (build-system pyproject-build-system)
    ;; There are no automated tests.
    (arguments (list #:tests? #false))
    (propagated-inputs (list python-numpy python-pytorch))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://www.kernel-operations.io/geomloss/")
    (synopsis
     "Geometric loss functions between point clouds, images and volumes")
    (description
     "The GeomLoss library provides efficient GPU implementations for:

@itemize
@item Kernel norms (also known as Maximum Mean Discrepancies).
@item Hausdorff divergences, which are positive definite generalizations of
the Chamfer-ICP loss and are analogous to log-likelihoods of Gaussian Mixture
Models.
@item Debiased Sinkhorn divergences, which are affordable yet positive and
definite approximations of Optimal Transport (Wasserstein) distances.
@end itemize")
    (license license:expat)))

(define-public rust-esaxx-rs-0.1
  (package
    (name "rust-esaxx-rs")
    (version "0.1.10")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "esaxx-rs" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1rm6vm5yr7s3n5ly7k9x9j6ra5p2l2ld151gnaya8x03qcwf05yq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-cc" ,rust-cc-1))))
    (home-page "https://github.com/Narsil/esaxx-rs")
    (synopsis "Wrapper for sentencepiece's esaxxx library")
    (description
     "This package provides a wrapper around sentencepiece's esaxxx library.")
    (license license:asl2.0)))

(define-public rust-spm-precompiled-0.1
  (package
    (name "rust-spm-precompiled")
    (version "0.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "spm_precompiled" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "09pkdk2abr8xf4pb9kq3rk80dgziq6vzfk7aywv3diik82f6jlaq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-base64" ,rust-base64-0.13)
        ("rust-nom" ,rust-nom-7)
        ("rust-serde" ,rust-serde-1)
        ("rust-unicode-segmentation" ,rust-unicode-segmentation-1))))
    (home-page "https://github.com/huggingface/spm_precompiled")
    (synopsis "Emulate sentencepiece's DoubleArray")
    (description
     "This crate aims to emulate
@url{https://github.com/google/sentencepiece,sentencepiece}
Dart::@code{DoubleArray} struct and it's Normalizer.  This crate is highly
specialized and not intended for general use.")
    (license license:asl2.0)))

(define-public rust-hf-hub-0.3
  (package
    (name "rust-hf-hub")
    (version "0.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "hf-hub" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0cnpivy9fn62lm1fw85kmg3ryvrx8drq63c96vq94gabawshcy1b"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f  ; require network connection
       #:cargo-inputs
       (("rust-dirs" ,rust-dirs-5)
        ("rust-futures" ,rust-futures-0.3)
        ("rust-indicatif" ,rust-indicatif-0.17)
        ("rust-log" ,rust-log-0.4)
        ("rust-native-tls" ,rust-native-tls-0.2)
        ("rust-num-cpus" ,rust-num-cpus-1)
        ("rust-rand" ,rust-rand-0.8)
        ("rust-reqwest" ,rust-reqwest-0.11)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-thiserror" ,rust-thiserror-1)
        ("rust-tokio" ,rust-tokio-1)
        ("rust-ureq" ,rust-ureq-2))
       #:cargo-development-inputs
       (("rust-hex-literal" ,rust-hex-literal-0.4)
        ("rust-sha2" ,rust-sha2-0.10)
        ("rust-tokio-test" ,rust-tokio-test-0.4))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list openssl))
    (home-page "https://github.com/huggingface/hf-hub")
    (synopsis "Interact with HuggingFace in Rust")
    (description
     "This crates aims ease the interaction with
@url{https://huggingface.co/,huggingface}.  It aims to be compatible with
@url{https://github.com/huggingface/huggingface_hub/,huggingface_hub}
python package, but only implements a smaller subset of functions.")
    (license license:asl2.0)))

(define-public rust-tokenizers
  (package
    (name "rust-tokenizers")
    (version "0.19.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tokenizers" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1zg6ffpllygijb5bh227m9p4lrhf0pjkysky68kddwrsvp8zl075"))
       (modules '((guix build utils)))
       (snippet
        #~(substitute* "Cargo.toml"
            (("0.1.12") ; rust-monostate requires a rust-syn-2 update
             "0.1.11")
            (("version = \"6.4\"")  ; rust-onig
             "version = \"6.1.1\"")))))
    (build-system cargo-build-system)
    (arguments
     (list
      #:tests? #f  ; tests are relying on missing data.
      #:cargo-inputs
      `(("rust-aho-corasick" ,rust-aho-corasick-1)
        ("rust-derive-builder" ,rust-derive-builder-0.20)
        ("rust-esaxx-rs" ,rust-esaxx-rs-0.1)
        ("rust-fancy-regex" ,rust-fancy-regex-0.13)
        ("rust-getrandom" ,rust-getrandom-0.2)
        ("rust-hf-hub" ,rust-hf-hub-0.3)
        ("rust-indicatif" ,rust-indicatif-0.17)
        ("rust-itertools" ,rust-itertools-0.12)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-log" ,rust-log-0.4)
        ("rust-macro-rules-attribute" ,rust-macro-rules-attribute-0.2)
        ("rust-monostate" ,rust-monostate-0.1)
        ("rust-onig" ,rust-onig-6)
        ("rust-paste" ,rust-paste-1)
        ("rust-rand" ,rust-rand-0.8)
        ("rust-rayon" ,rust-rayon-1)
        ("rust-rayon-cond" ,rust-rayon-cond-0.3)
        ("rust-regex" ,rust-regex-1)
        ("rust-regex-syntax" ,rust-regex-syntax-0.8)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-spm-precompiled" ,rust-spm-precompiled-0.1)
        ("rust-thiserror" ,rust-thiserror-1)
        ("rust-unicode-normalization-alignments" ,rust-unicode-normalization-alignments-0.1)
        ("rust-unicode-segmentation" ,rust-unicode-segmentation-1)
        ("rust-unicode-categories" ,rust-unicode-categories-0.1))
      #:cargo-development-inputs
      `(("rust-assert-approx-eq" ,rust-assert-approx-eq-1)
        ("rust-criterion" ,rust-criterion-0.5)
        ("rust-tempfile" ,rust-tempfile-3))))
    (home-page "https://github.com/huggingface/tokenizers")
    (synopsis "Implementation of various popular tokenizers")
    (description
     "This package provides a Rust implementation of today's most used
tokenizers, with a focus on performances and versatility.")
    (license license:asl2.0)))

(define-public python-tokenizers
  (package
    (name "python-tokenizers")
    (version "0.19.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "tokenizers" version))
       (sha256
        (base32 "1qw8mjp0q9w7j1raq1rvcbfw38000kbqpwscf9mvxzfh1rlfcngf"))
       (modules '((guix build utils)
                  (ice-9 ftw)))
       (snippet
        #~(begin  ;; Only keeping bindings.
            (for-each (lambda (file)
                        (unless (member file '("." ".." "bindings" "PKG-INFO"))
                          (delete-file-recursively file)))
                      (scandir "."))
            (for-each (lambda (file)
                        (unless (member file '("." ".."))
                          (rename-file (string-append "bindings/python/" file) file)))
                      (scandir "bindings/python"))
            (delete-file-recursively ".cargo")))))
    (build-system cargo-build-system)
    (arguments
     (list
      #:cargo-test-flags ''("--no-default-features")
      #:imported-modules `(,@%cargo-build-system-modules
                           ,@%pyproject-build-system-modules)
      #:modules '((guix build cargo-build-system)
                  ((guix build pyproject-build-system) #:prefix py:)
                  (guix build utils)
                  (ice-9 regex)
                  (ice-9 textual-ports))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack-rust-crates 'inject-tokenizers
            (lambda _
              (substitute* "Cargo.toml"
                (("\\[dependencies\\]")
                 (format #f "
[dev-dependencies]
tempfile = ~s
pyo3 = { version = ~s, features = [\"auto-initialize\"] }

[dependencies]
tokenizers = ~s"
                         #$(package-version rust-tempfile-3)
                         #$(package-version rust-pyo3-0.21)
                         #$(package-version rust-tokenizers))))
              (let ((file-path "Cargo.toml"))
                (call-with-input-file file-path
                  (lambda (port)
                    (let* ((content (get-string-all port))
                           (top-match (string-match
                                       "\\[dependencies.tokenizers" content)))
                      (call-with-output-file file-path
                        (lambda (out)
                          (format out "~a" (match:prefix top-match))))))))))
          (add-after 'patch-cargo-checksums 'loosen-requirements
            (lambda _
              (substitute* "Cargo.toml"
                (("version = \"6.4\"")
                 (format #f "version = ~s"
                         #$(package-version rust-onig-6))))))
          (add-after 'check 'python-check
            (lambda _
              (copy-file "target/release/libtokenizers.so"
                         "py_src/tokenizers/tokenizers.so")
              (invoke "python3"
                      "-c" (format #f
                                   "import sys; sys.path.append(\"~a/py_src\")"
                                   (getcwd))
                      "-m" "pytest"
                      "-s" "-v" "./tests/")))
          (add-after 'install 'install-python
            (lambda _
              (let* ((pversion #$(version-major+minor (package-version python)))
                     (lib (string-append #$output "/lib/python" pversion
                                         "/site-packages/"))
                     (info (string-append lib "tokenizers-"
                                        #$(package-version this-package)
                                        ".dist-info")))
                (mkdir-p info)
                (copy-file "PKG-INFO" (string-append info "/METADATA"))
                (copy-recursively
                 "py_src/tokenizers"
                 (string-append lib "tokenizers"))))))
      #:cargo-inputs
      `(("rust-rayon" ,rust-rayon-1)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-env-logger" ,rust-env-logger-0.11)
        ("rust-pyo3" ,rust-pyo3-0.21)
        ("rust-numpy" ,rust-numpy-0.21)
        ("rust-ndarray" ,rust-ndarray-0.15)
        ("rust-onig" ,rust-onig-6)
        ("rust-itertools" ,rust-itertools-0.12)
        ("rust-tokenizers" ,rust-tokenizers))
      #:cargo-development-inputs
      `(("rust-tempfile" ,rust-tempfile-3))))
    (native-inputs
     (list python-minimal python-pytest))
    (home-page "https://huggingface.co/docs/tokenizers")
    (synopsis "Implementation of various popular tokenizers")
    (description
     "This package provides bindings to a Rust implementation of the most used
tokenizers, @code{rust-tokenizers}.")
    (license license:asl2.0)))

(define-public python-transformers
  (package
    (name "python-transformers")
    (version "4.44.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "transformers" version))
       (sha256
        (base32 "09h84wqqk2bgi4vr9d1m3dsliard99l53n96wic405gfjb61gain"))))
    (build-system pyproject-build-system)
    (arguments
     ;; Missing inputs.
     (list #:test-flags
           '(list "--ignore=tests/test_modeling_tf_common.py"
                  "--ignore=tests/test_configuration_common.py"
                  "--ignore=tests/test_pipeline_mixin.py"
                  "--ignore=tests/test_sequence_feature_extraction_common.py")))
    ;; The imported package contains ~60 more inputs, but they don't seem
    ;; necessary to build a minimal version of the package.
    (propagated-inputs
     (list python-filelock
           python-huggingface-hub
           python-numpy
           python-pytorch
           python-pyyaml
           python-regex
           python-requests
           python-safetensors
           python-tokenizers
           python-tqdm))
    (native-inputs
     (list python-parameterized-next
           python-pytest python-setuptools python-wheel))
    (home-page "https://github.com/huggingface/transformers")
    (synopsis "Machine Learning for PyTorch and TensorFlow")
    (description
     "This package provides easy download of thousands of pretrained models to
perform tasks on different modalities such as text, vision, and audio.

These models can be applied on:
@itemize
@item Text, for tasks like text classification, information extraction,
question answering, summarization, translation, and text generation, in over
100 languages.
@item Images, for tasks like image classification, object detection, and
segmentation.
@item Audio, for tasks like speech recognition and audio classification.
@end itemize

Transformer models can also perform tasks on several modalities combined, such
as table question answering, optical character recognition, information
extraction from scanned documents, video classification, and visual question
answering.

This package provides APIs to quickly download and use those pretrained models
on a given text, fine-tune them on your own datasets and then share them with
the community.  At the same time, each Python module defining an architecture
is fully standalone and can be modified to enable quick research experiments.

Transformers is backed by the three most popular deep learning libraries —
Jax, PyTorch and TensorFlow — with a seamless integration between them.")
    (license license:asl2.0)))

(define-public python-hmmlearn
  (package
    (name "python-hmmlearn")
    (version "0.3.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "hmmlearn" version))
       (sha256
        (base32
         "1v24rkqjjf67w2rys25qxa3vk30bf23m7zn1ilihqzi5qp25sg0x"))))
    (properties
     '((updater-extra-native-inputs . ("pybind11" "python-setuptools-scm"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      '(modify-phases %standard-phases
         (add-after 'unpack 'set-core-count
           (lambda _
             ;; "Could not find the number of physical cores", so we tell it
             ;; how many cores to use.
             (setenv "LOKY_MAX_CPU_COUNT" "1")))
         (add-before 'check 'build-extensions
           (lambda _
             (invoke "python" "setup.py" "build_ext" "--inplace"))))))
    (propagated-inputs
     (list python-numpy python-scikit-learn python-scipy))
    (native-inputs
     (list pybind11
           python-pytest
           python-setuptools
           python-setuptools-scm
           python-wheel
           util-linux)) ;for lscpu
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
    (version "0.13.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mlverse/torch")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1yy5xpn9mi5qm7k4w7040d6frpixm9ifs46v1cn9b6bpc1qs1a02"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #false                 ;no test target
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
    (version "0.5.12")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "lap" version))
              (sha256
               (base32
                "1za4mf5nd7vzwd24sy2mfxrk8mnwq7d8rv6h96yh8v5flx7422sp"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags #~(list "-v" #$output)
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'check 'check-cleanup
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (for-each
                 delete-file-recursively
                 (find-files #$output
                             (lambda (file stat)
                               (or (member (basename file)
                                           '("tests" ".pytest_cache"))))
                             #:directories? #t))))))))
    (propagated-inputs
     (list python-numpy))
    (native-inputs
     (list python-cython python-pytest python-setuptools python-wheel))
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
    (version "1.9.1")
    ;; The sources on pypi don't include tests.
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pyro-ppl/pyro")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0q87d0frgzn1ljnpbyxmj582yfn6zy3m960m3ab826h4rpzybxhf"))))
    (build-system pyproject-build-system)
    (arguments
     ;; Tests take too long.
     ;; XXX: Maybe select the most important test modules.
     (list #:tests? #f))
    (propagated-inputs
     (list python-numpy
           python-opt-einsum
           python-pyro-api
           python-pytorch
           python-tqdm))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://pyro.ai")
    (synopsis "Python library for probabilistic modeling and inference")
    (description
     "This package provides a Python library for probabilistic modeling and
inference.")
    (license license:asl2.0)))

(define-public python-linear-operator
  (package
    (name "python-linear-operator")
    (version "0.6")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "linear_operator" version))
              (sha256
               (base32
                "0m56f3zrm8xh1bpwh4g7jfc79rf4j94g6zmz663b58pig4w6dqm9"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-jaxtyping
                             python-mpmath
                             python-pytorch
                             python-scipy))
    (native-inputs (list python-pytest
                         python-setuptools
                         python-setuptools-scm
                         python-typeguard
                         python-wheel))
    (home-page "https://github.com/cornellius-gp/linear_operator/")
    (synopsis "Linear operator implementation")
    (description "LinearOperator is a PyTorch package for abstracting away the
linear algebra routines needed for structured matrices (or operators).")
    (license license:expat)))

(define-public python-gpytorch
  (package
    (name "python-gpytorch")
    (version "1.14")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "gpytorch" version))
              (sha256
               (base32
                "13cs6dx8qa5j4ygji9w5xbmaqc68ihqyzz33fyyf9qa6d8gc2b03"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:test-flags
           #~(list "-k" (string-append
                         ;; test_deprecated_methods fails with an AssertionError.
                         "not test_deprecated_methods"
                         ;; This test is flaky: Expects gradients of 0 exactly,
                         ;; can get negligible ones (e-10 to e-16).
                         " and not test_optimization_optimal_error")
                   ;; Ignore lenghty tests of little relevance.
                   "--ignore=test/examples/")))
    (propagated-inputs (list python-jaxtyping
                             python-linear-operator
                             python-mpmath
                             python-scikit-learn
                             python-scipy))
    (native-inputs (list python-nbval
                         python-pytest
                         python-setuptools
                         python-setuptools-scm
                         python-wheel))
    (home-page "https://gpytorch.ai")
    (synopsis "Implementation of Gaussian Processes in PyTorch")
    (description
     "GPyTorch is a Gaussian process library implemented using PyTorch.")
    (license license:expat)))

(define-public python-botorch
  (package
    (name "python-botorch")
    (version "0.13.0")
    (source (origin
              (method git-fetch) ;no tests in PyPI
              (uri (git-reference
                    (url "https://github.com/pytorch/botorch")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1sxgxdq892vg5xj30kb86003b9rwsipc95c7p1zdv865y4f38a8y"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:test-flags #~(list "-k" "not test_all_cases_covered")
           #:phases
           #~(modify-phases %standard-phases
               (add-before 'build 'pretend-version
                 ;; The version string is usually derived via setuptools-scm,
                 ;; but without the git metadata available, the version string
                 ;; is set to '0.0.0'.
                 (lambda _
                   (setenv "SETUPTOOLS_SCM_PRETEND_VERSION"
                           #$(package-version this-package)))))))
    (propagated-inputs (list python-gpytorch
                             python-linear-operator
                             python-multipledispatch
                             python-pyro-ppl
                             python-pytorch
                             python-scipy))
    (native-inputs (list python-pyre-extensions
                         python-pytest
                         python-pytest-cov
                         python-setuptools
                         python-setuptools-scm
                         python-wheel))
    (home-page "https://botorch.org")
    (synopsis "Bayesian Optimization in PyTorch")
    (description
     "BoTorch is a library for Bayesian Optimization built on PyTorch.")
    (license license:expat)))

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
                (let* ((openfst  #$(this-package-input "openfst"))
                       (openblas #$(this-package-input "openblas"))
                       (kaldi    #$(this-package-input "kaldi")))
                  (substitute* "./Makefile"
                    (("USE_SHARED\\?=0")
                     "USE_SHARED?=1")
                    (("-DFST_NO_DYNAMIC_LINKING")
                     "")
                    (("-lopenblas -llapack -lblas -lf2c")
                     (string-append
                      "-L" openblas "/lib " "-lopenblas "))
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
      (inputs (list kaldi openfst openblas))
      (home-page "https://alphacephei.com/vosk")
      (synopsis "Speech recognition toolkit based on @code{kaldi}")
      (description "This package provides a speech recognition toolkit based
on @code{kaldi}.  It supports more than 20 languages and dialects - English,
Indian English, German, French, Spanish, Portuguese, Chinese, Russian,
Turkish, Vietnamese, Italian, Dutch, Catalan, Arabic, Greek, Farsi, Filipino,
Ukrainian, Kazakh, Swedish, Japanese, Esperanto, Hindi, Czech, Polish.  The
program works offline, even on lightweight devices.  Portable per-language
models are about 50Mb each, and there are much bigger and precise models
available.

Vosk API provides a streaming API allowing to use it @emph{on-the-fly} and
bindings for different programming languages.  It allows quick reconfiguration
of vocabulary for better accuracy, and supports speaker identification beside
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
                  "libraries=['vosk', 'python3.11'],\n\t"
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
    (native-inputs (list python-pytest python-pytest-xdist python-wheel))
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
    (version "3.5.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/oneapi-src/oneDNN")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1m2d7qlbfk86rmvmpvx2k3rc2k0l9hf9qpa54jl44670ls9n8i7w"))
       (modules '((guix build utils)
                  (ice-9 rdelim)))
       ;; Copyright date used by code generation script
       (snippet
        '(for-each
          (lambda (file)
            (with-atomic-file-replacement
             file
             (lambda (in out)
               (let loop ((line (read-line in 'concat)))
                 (if (string-contains line "Copyright")
                     (display line out)
                     (loop (read-line in 'concat)))))))
          '("include/oneapi/dnnl/dnnl_debug.h"
            "src/common/dnnl_debug_autogenerated.cpp"
            "tests/benchdnn/dnnl_debug_autogenerated.cpp"
            "tests/benchdnn/dnnl_debug.hpp")))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:configure-flags
      `(list
        ,@(if (target-riscv64?)
              (list #:configure-flags '("-DDNNL_CPU_RUNTIME=SEQ"))
              '())
        ;; Used in PyTorch
        "-DDNNL_EXPERIMENTAL_UKERNEL=ON")
      #:phases
      '(modify-phases %standard-phases
         (add-after 'configure 'codegen
           (lambda _
             (with-directory-excursion "../source"
               (invoke "castxml" "--castxml-cc-gnu-c" "clang"
                       "--castxml-output=1" "-DDNNL_EXPERIMENTAL_SPARSE"
                       "-Iinclude" "-I../build/include"
                       "include/oneapi/dnnl/dnnl_types.h" "-o" "types.xml")
               (invoke "python3" "scripts/generate_dnnl_debug.py" "types.xml")
               ;; Modifies include/oneapi/dnnl/dnnl.hpp
               (invoke "python3" "scripts/generate_format_tags.py")))))))
    (native-inputs (list castxml clang-17 python))
    (home-page "https://github.com/oneapi-src/oneDNN")
    (synopsis "Deep Neural Network Library")
    (description
     "OneAPI Deep Neural Network Library (oneDNN) is a cross-platform
performance library of basic building blocks for deep learning applications.")
    (supported-systems %64bit-supported-systems)
    (license license:asl2.0)))

(define-public oneapi-dnnl-for-r-torch
  (package
    (inherit oneapi-dnnl)
    (version "2.7.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/oneapi-src/oneDNN")
             (commit (string-append "v" version))))
       (file-name (git-file-name (package-name oneapi-dnnl) version))
       (sha256
        (base32 "1zyw5rd8x346bb7gac9a7x3saviw3zvp6aqz2z1l9sv163vmjfz6"))))))

(define-public python-gguf
  (package
    (name "python-gguf")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "gguf" version))
       (sha256
        (base32 "0rbyc2h3kpqnrvbyjvv8a69l577jv55a31l12jnw21m1lamjxqmj"))))
    (build-system pyproject-build-system)
    (arguments
      (list #:tests? #false))
    (inputs (list poetry python-pytest))
    (propagated-inputs (list python-numpy))
    (home-page "https://ggml.ai")
    (synopsis "Read and write ML models in GGUF for GGML")
    (description "A Python library for reading and writing GGUF & GGML format ML models.")
    (license license:expat)))

(define-public python-gymnasium
  (package
    (name "python-gymnasium")
    (version "0.29.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "gymnasium" version))
       (sha256
        (base32 "1cab4wsnlsxn2z90qmymv8ppmsq8yq2amiqwid3r0xfbxx92flqs"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'create-tests-module
            (lambda _
              (with-output-to-file "tests/__init__.py"
                (lambda _ (display ""))))))))
    (propagated-inputs (list python-cloudpickle python-farama-notifications
                             python-importlib-metadata python-numpy
                             python-typing-extensions))
    (native-inputs
     (list python-pytest python-scipy python-setuptools python-wheel))
    (home-page "https://gymnasium.farama.org/")
    (synopsis
     "Standard API for reinforcement learning and a set of reference environments")
    (description
     "This package provides a standard API for reinforcement learning and a
diverse set of reference environments (formerly Gym).")
    (license license:expat)))

;; This will build dlib in the process of building python-dlib--and that
;; seems to be intended by upstream.  Well, at least it probably optimizes
;; better that way.
(define-public python-dlib
  (package
   (inherit dlib)
   (name "python-dlib")
   (build-system pyproject-build-system)
   (arguments
    `(#:phases
      (modify-phases %standard-phases
                     (add-after 'unpack 'subst
                                (lambda _
                                  (substitute* "tools/python/CMakeLists.txt"
                                               (("add_subdirectory[(][.][.]/[.][.]/dlib/external/pybind11 pybind11_build[)]")
                                                "find_package(pybind11 CONFIG)")))))))
   (native-inputs (list python-setuptools python-wheel cmake-minimal perl pkg-config pybind11))))
