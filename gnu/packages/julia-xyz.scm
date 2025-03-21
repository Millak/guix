;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020, 2021 Nicolò Balzarotti <nicolo@nixo.xyz>
;;; Copyright © 2021, 2022 Simon Tournier <zimon.toutoune@gmail.com>
;;; Copyright © 2021-2024 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2021 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2021 jgart <jgart@dismail.de>
;;; Copyright © 2023 Sharlatan Hellseher <sharlatanus@gmail.com>
;;; Copyright © 2024, 2025 Spencer King <spencer.king@geneoscopy.com>
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

(define-module (gnu packages julia-xyz)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system julia)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages julia-jll)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages version-control))

(define-public julia-abstractffts
  (package
    (name "julia-abstractffts")
    (version "1.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaMath/AbstractFFTS.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0083pwdyxjb04i330ir9pc8kmp4bwk59lx1jgc9qi05y8j7xzbp0"))))
    (build-system julia-build-system)
    (inputs                             ;required for tests
     (list julia-unitful))
    (home-page "https://github.com/JuliaGPU/Adapt.jl")
    (synopsis "General framework for fast Fourier transforms (FFTs)")
    (description "This package allows multiple FFT packages to co-exist with
the same underlying @code{fft(x)} and @code{plan_fft(x)} interface.  It is
mainly not intended to be used directly.  Instead, developers of packages that
implement FFTs (such as @code{FFTW.jl} or @code{FastTransforms.jl}) extend the
types/functions defined in AbstractFFTs.")
    (license license:expat)))

(define-public julia-abstracttrees
  (package
    (name "julia-abstracttrees")
    (version "0.4.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaCollections/AbstractTrees.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "04g3b6j4nvxs36rcjm743gwhy0vv0d6pvgx771agjljx109bciyr"))))
    (build-system julia-build-system)
    (home-page "https://juliacollections.github.io/AbstractTrees.jl/stable/")
    (synopsis "Abstract Julia interfaces for working with trees")
    (description "This Julia package provides several utilities for working
with tree-like data structures.  Most importantly, it defines the
@code{children} method that any package that contains such a data structure
may import and extend in order to take advantage of any generic tree algorithm
in this package.")
    (license license:expat)))

(define-public julia-adapt
  (package
    (name "julia-adapt")
    (version "3.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaGPU/Adapt.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "009fj59fzhvfsyw35kakllsh36k3xlwyzq8qa5f5k598i3pq14i7"))))
    (build-system julia-build-system)
    (home-page "https://github.com/JuliaGPU/Adapt.jl")
    (synopsis "Package providing the @code{adapt} function, similar to @code{convert}")
    (description "This Julia package provides the @code{adapt(T, x)} function
acts like @code{convert(T, x)}, but without the restriction of returning a
@code{T}.  This allows you to \"convert\" wrapper types like @code{Adjoint} to
be GPU compatible without throwing away the wrapper.")
    (license license:expat)))

(define-public julia-ansicoloredprinters
  (package
    (name "julia-ansicoloredprinters")
    (version "0.0.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaDocs/ANSIColoredPrinters.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0dp5agljr0g50s5gn0pr70wrz01ggck6pb40ay3l4szhswq7mqzf"))))
    (build-system julia-build-system)
    (home-page "https://github.com/JuliaDocs/ANSIColoredPrinters.jl")
    (synopsis "ANSI escape code translator")
    (description "@code{ANSIColoredPrinters.jl} converts a text qualified by
ANSI escape codes to another format.")
    (license license:expat)))

(define-public julia-aqua
  (package
    (name "julia-aqua")
    (version "0.5.5")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaTesting/Aqua.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "12hng8czkgynsn1pshavma2wijypl6k05hhgivc3rqiyclfpi89z"))))
    (build-system julia-build-system)
    (arguments
     (list #:parallel-tests? #f))
    (home-page "https://github.com/JuliaTesting/Aqua.jl")
    (synopsis "Automated quality assurance for Julia packages")
    (description "@acronym{Aqua.jl, Auto QUality Assurance for Julia packages},
provides functions to run a few automatable checks for Julia packages.")
    (license license:expat)))

(define-public julia-argcheck
  (package
    (name "julia-argcheck")
    (version "2.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jw3126/ArgCheck.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "13mkcq4ln1vm9hxk3rxs6gcrddbs01bapvp3wb39xqvpb6q3fv6h"))))
    (build-system julia-build-system)
    (native-inputs (list julia-benchmarktools))
    (home-page "https://github.com/jw3126/ArgCheck.jl")
    (synopsis "Package for checking function arguments")
    (description "This package provides a method to ensure that arguments
to a function conform to a specification.")
    (license license:expat)))

(define-public julia-arnoldimethod
  (package
    (name "julia-arnoldimethod")
    (version "0.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaLinearAlgebra/ArnoldiMethod.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1gs7pikgdg436srxxfywpnp12ay1mf45f7z80wym92rfrjzakwh2"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-genericschur julia-staticarrays))
    (home-page "https://github.com/JuliaLinearAlgebra/ArnoldiMethod.jl")
    (synopsis "Implicitly Restarted Arnoldi Method, natively in Julia")
    (description
     "@code{ArnoldiMethod.jl} provides an iterative method to find a few
approximate solutions to the eigenvalue problem in standard form with main
goals:

@itemize
@item Having a native Julia implementation of the @code{eigs} function that
performs as well as ARPACK.  With native we mean that its implementation should
be generic and support any number type.  Currently the partialschur function
does not depend on LAPACK, and removing the last remnants of direct calls to
BLAS is in the pipeline.

@item Removing the dependency of the Julia language on ARPACK.  This goal was
already achieved before the package was stable enough, since ARPACK moved to a
separate repository @code{Arpack.jl}.
@end itemize")
    (license license:expat)))

(define-public julia-arrayinterface
  (package
    (name "julia-arrayinterface")
    (version "5.0.8")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaArrays/ArrayInterface.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0b0h4ihc8sykd96rn16vpk5kfk0p1si5iim61cixk9x12ma8ia3h"))))
    (build-system julia-build-system)
    (arguments
     ;; XXXX: Unexpected failures for i686, e.g.,
     ;; Expression: @inferred(ArrayInterface.size(Rnr)) === (StaticInt(4),)
     ;; Evaluated: (static(2),) === (static(4),)
     ;; Disable as stopgap.
     (list #:tests? (not (or (%current-target-system)
                             (target-x86-32?)))))
    (propagated-inputs
     (list julia-compat
           julia-ifelse
           julia-requires
           julia-static-0.6))
    (native-inputs
     (list julia-aqua
           julia-bandedmatrices
           julia-blockbandedmatrices
           julia-ifelse
           julia-offsetarrays
           julia-static-0.6
           julia-staticarrays))
    (home-page "https://github.com/JuliaArrays/ArrayInterface.jl")
    (synopsis "Base array interface primitives")
    (description "The purpose of this library is to solidify extensions to the
current @code{AbstractArray} interface, which are put to use in package
ecosystems like @code{DifferentialEquations.jl}.  Since these libraries are
live, this package will serve as a staging ground for ideas before they are
merged into Base Julia.  For this reason, no functionality is exported so that
if such functions are added and exported in a future Base Julia, there will be
no issues with the upgrade.")
    (license license:expat)))

(define-public julia-arraylayouts
  (package
    (name "julia-arraylayouts")
    (version "0.8.18")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaMatrices/ArrayLayouts.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "11h0w1bqw2md5gh4dfmm1aazifcs2ydrc47hqzvav1xrx25b57z5"))))
    (build-system julia-build-system)
    (arguments
     (if (not (target-x86-64?))
         ;; This test is only broken when using openblas, not openblas-ilp64.
         (list
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'adjust-tests
                 (lambda _
                   (substitute* "test/test_layoutarray.jl"
                     (("test all\\(B") "test_broken all(B"))))))
         '()))
    (propagated-inputs
     (list julia-fillarrays))
    (native-inputs
     (list julia-stablerngs))
    (home-page "https://github.com/JuliaMatrices/ArrayLayouts.jl")
    (synopsis "Array layouts and general fast linear algebra")
    (description "This package implements a trait-based framework for describing
array layouts such as column major, row major, etc. that can be dispatched to
appropriate BLAS or optimised Julia linear algebra routines.  This supports a
much wider class of matrix types than Julia's in-built @code{StridedArray}.")
    (license license:expat)))

(define-public julia-astroangles
  (package
    (name "julia-astroangles")
    (version "0.1.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaAstro/AstroAngles.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1hd03kv1dppdzz7i9jv11f4rby5c21sdyhbcf3yvba9vnkkd1gz5"))))
    (build-system julia-build-system)
    (native-inputs
     (list julia-formatting
           julia-stablerngs))
    (home-page "https://github.com/JuliaAstro/AstroAngles.jl")
    (synopsis "Lightweight string parsing and representation of angles")
    (description
     "This package provides a lightweight string parsing and representation of
angles.")
    (license license:expat)))

(define-public julia-astrolib
  (package
    (name "julia-astrolib")
    (version "0.4.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaAstro/AstroLib.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1zbivs79cw7qazrl9c8rz2n2kifxw3adwjf22nn24dp5i34fkw5d"))))
    (build-system julia-build-system)
    (native-inputs (list julia-staticarrays))
    (home-page "https://github.com/JuliaAstro/AstroLib.jl")
    (synopsis "Bundle of small astronomical and astrophysical routines")
    (description "The aim of this package is to provide users with a set of small
generic routines useful above all in astronomical and astrophysical context,
written in Julia.")
    (license license:expat)))

(define-public julia-astrotime
  (package
    (name "julia-astrotime")
    (version "0.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaAstro/AstroTime.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "032hlanan49ypqh4lwlf91pg7052c8h5sgbxqc1771b8j9srbyd2"))))
    (build-system julia-build-system)
    (native-inputs
     (list julia-measurements))
    (propagated-inputs
     (list julia-erfa
           julia-earthorientation
           julia-itemgraphs
           julia-macrotools
           julia-muladdmacro
           julia-reexport))
    (home-page "https://github.com/JuliaAstro/AstroTime.jl")
    (synopsis "Astronomical time keeping in Julia")
    (description "@code{AstroTime.jl} provides a high-precision, time-scale
aware, @code{DateTime}-like data type which supports all commonly used
astronomical time scales.")
    (license license:expat)))

(define-public julia-automa
  (package
    (name "julia-automa")
    (version "0.8.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/BioJulia/Automa.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0hmwvk3qw54p7f63a2dnzlmvkynfs62x9n8x952bcmczp35csgq0"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-scanbyte
           julia-transcodingstreams))
    (home-page "https://github.com/BioJulia/Automa.jl")
    (synopsis "Validation, parsing, and tokenizing based on state machine compiler")
    (description "This package compiles regular expressions into Julia code,
which is then compiled into low-level machine code by the Julia compiler.  The
package is designed to generate very efficient code to scan large text data,
which is often much faster than handcrafted code.  @code{Automa.jl} can insert
arbitrary Julia code that will be executed in state transitions.  This makes
it possible, for example, to extract substrings that match a part of a regular
expression.")
    (license license:expat)))

(define-public julia-axisalgorithms
  (package
    (name "julia-axisalgorithms")
    (version "1.0.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/timholy/AxisAlgorithms.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "00x85lnfln7xkfnirpplzyi8r6q92nfqwya8il156bf7b1pa20gk"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-woodburymatrices))
    (home-page "https://github.com/timholy/AxisAlgorithms.jl")
    (synopsis "Filtering and linear algebra routines for multidimensional arrays")
    (description "@code{AxisAlgorithms} is a collection of filtering and linear
algebra algorithms for multidimensional arrays.  For algorithms that would
typically apply along the columns of a matrix, you can instead pick an arbitrary
axis (dimension).")
    (license license:expat)))

(define-public julia-axisarrays
  (package
    (name "julia-axisarrays")
    (version "0.4.6")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaArrays/AxisArrays.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1bsd6y866ldfb4072hfm8fvc2k0vy72z2blcwfy2mpj8dlyskx3n"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-rangearrays
           julia-intervalsets
           julia-itertools))
    (native-inputs
     (list julia-offsetarrays
           julia-unitful))
    (home-page "http://juliaarrays.github.io/AxisArrays.jl/latest/")
    (synopsis "Arrays where each dimension can have a named axis with values")
    (description "This package for the Julia language provides an array type
(the AxisArray) that knows about its dimension names and axis values.  This
allows for indexing by name without incurring any runtime overhead.  This
permits one to implement algorithms that are oblivious to the storage order of
the underlying arrays.  AxisArrays can also be indexed by the values along their
axes, allowing column names or interval selections.")
    (license license:expat)))

(define-public julia-bandedmatrices
  (package
    (name "julia-bandedmatrices")
    (version "0.17.9")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaMatrices/BandedMatrices.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0nrcasjdpwf15z7l2lzyhxjqxlnqk5if78s15sh4gdgxf9kzj3a6"))
        (snippet
         #~(begin
             (use-modules (guix build utils))
             ;; From upstream commit 8bbf901bb7fb417fe90be26e0cd9a141cfdfe19c,
             ;; included in 0.17.34.
             (substitute* "src/BandedMatrices.jl"
               (("const libblas = Base\\.libblas_name")
                "const libblas = LinearAlgebra.BLAS.libblas")
               (("const liblapack = Base\\.liblapack_name")
                "const liblapack = LinearAlgebra.BLAS.liblapack"))))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-aqua
           julia-arraylayouts
           julia-fillarrays))
    (native-inputs
     (list julia-aqua
           julia-genericlinearalgebra))
    (home-page "https://github.com/JuliaMatrices/BandedMatrices.jl")
    (synopsis "Julia package for representing banded matrices")
    (description "This package supports representing banded matrices by only
the entries on the bands.")
    (license license:expat)))

(define-public julia-benchmarktools
  (package
    (name "julia-benchmarktools")
    (version "1.3.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaCI/BenchmarkTools.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "02n2pi71jvhsnc25a888i6imimd2c1phg6iyr73b12595lrz175d"))))
    (build-system julia-build-system)
    (arguments
     (list
      #:phases
      (if (target-x86-32?)
          #~(modify-phases %standard-phases
              (add-after 'unpack 'remove-failing-tests-i686
                (lambda _
                  (substitute* "test/GroupsTests.jl"
                    (("@test sprint\\(show, g1\\)")
                     "@test_broken sprint(show, g1)")
                    (("@test sprint\\(show, g1; context = :boundto => 1\\)")
                     "@test_broken sprint(show, g1; context = :boundto => 1)")
                    (("@test sprint\\(show, g1; context = :limit => false\\)")
                     "@test_broken sprint(show, g1; context = :limit => false)")
                    (("@test @test_deprecated") "@test_broken"))
                  (substitute* "test/ExecutionTests.jl"
                    ;; Evaluated: 12 == 8
                    (("@test @ballocated\\(Ref\\(1\\)\\)")
                     "@test_broken @ballocated(Ref(1))")))))
          #~%standard-phases)))
    (propagated-inputs
     (list julia-json))
    (home-page "https://github.com/JuliaCI/BenchmarkTools.jl")
    (synopsis "Benchmarking framework for the Julia language")
    (description "@code{BenchmarkTools.jl} makes performance tracking of Julia
code easy by supplying a framework for writing and running groups of
benchmarks as well as comparing benchmark results.")
    (license license:expat)))

(define-public julia-bfloat16s
  (package
    (name "julia-bfloat16s")
    (version "0.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaMath/BFloat16s.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "12d5dv5jy8vphczlbnks0qa6wmlz0czxq7gc48bcb94f9qvq0r1n"))))
    (build-system julia-build-system)
    (home-page "https://github.com/JuliaMath/BFloat16s.jl")
    (synopsis "Define BFloat16 data type")
    (description "This package defines the @code{BFloat16} data type.  The
only currently available hardware implementation of this datatype are Google's
Cloud TPUs.  As such, this package is suitable to evaluate whether using TPUs
would cause precision problems for any particular algorithm, even without
access to TPU hardware.  Note that this package is designed for functionality,
not performance, so this package should be used for precision experiments
only, not performance experiments.")
    (license license:expat)))

(define-public julia-bijections
  (package
    (name "julia-bijections")
    (version "0.1.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/scheinerman/Bijections.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1f58cvjvrjh6xzi6zzbakdicdhpkyzwdp15fg2y12vslgmab2k9h"))))
    (build-system julia-build-system)
    (home-page "https://github.com/scheinerman/Bijections.jl")
    (synopsis "Bijection data type for Julia")
    (description
     "This package defines the @code{Bijection} data type.
A @code{Bijection} data structure behaves similar to a @code{Dict},
however it prevents assigning the same value to two different keys.")
    (license license:expat)))

(define-public julia-bioalignments
  (package
    (name "julia-bioalignments")
    (version "2.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/BioJulia/BioAlignments.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1wf6qgsada59r2fykxfj9hcr635wl8maqxbd3w8qpa01k9glxa0k"))))
    (build-system julia-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'skip-test
            (lambda _
              ;; Test fails because an unexpected type representation from
              ;; BioSequences.  The aligned value is correct though.
              (substitute* "test/runtests.jl"
                (("@test sprint\\(show, aln\\)")
                 "@test_broken sprint(show, aln)")))))))
    (propagated-inputs
     (list julia-biogenerics
           julia-biosequences
           julia-biosymbols
           julia-intervaltrees))
    (home-page "https://github.com/BioJulia/BioAlignments.jl")
    (synopsis "Sequence alignment algorithm and data structures")
    (description "This package provides alignment algorithms and data
structures for sequence of DNA, RNA, and amino acid sequences.")
    (license license:expat)))

(define-public julia-biogenerics
  ;; No upstream release
  (let ((commit "a75abaf459250e2b5e22b4d9adf25fd36d2acab6")
        (revision "1"))
    (package
      (name "julia-biogenerics")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/BioJulia/BioGenerics.jl")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "17d222vi9nssjwr5l349fss7jnglnjimp1z62kmfmxa4fsn8lk8l"))))
      (build-system julia-build-system)
      (inputs
       (list julia-transcodingstreams))
      (home-page "https://github.com/BioJulia/BioGenerics.jl")
      (synopsis "Generic methods used by BioJulia packages")
      (description "This package provides generic methods and modules used in
many of the other BioJulia packages.  This package defines IO, exceptions, and
other types or methods used by other BioJulia packages.")
      (license license:expat))))

(define-public julia-biosequences
  (package
    (name "julia-biosequences")
    (version "2.0.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/BioJulia/BioSequences.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ns6zk0zvnsf4hlsys9ck2xrn20qck0b0aghh484vc6n458zq2gw"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-biogenerics
           julia-biosymbols
           julia-combinatorics
           julia-indexablebitvectors
           julia-stablerngs
           julia-twiddle))
    (native-inputs
     (list julia-statsbase
           julia-yaml))
    (home-page "https://biojulia.net/BioSequences.jl/stable/")
    (synopsis "Data types and methods for common operations with biological sequences")
    (description "This package provides Data types and methods for common
operations with biological sequences, including DNA, RNA, and amino acid
sequences.")
    (license license:expat)))

(define-public julia-biosymbols
  (package
    (name "julia-biosymbols")
    ;; Older release for compatibility with julia-biosequences.
    (version "4.0.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/BioJulia/BioSymbols.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1222rwdndi777lai8a6dwrh35i5rgmj75kcrhn8si72sxgz0syjm"))))
    (build-system julia-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
        (add-after 'unpack 'adjust-tests
          (lambda _
            (substitute* "test/runtests.jl"
              (("\\@testset \\\"Range.*" all)
               (string-append all " return\n"))))))))
    (propagated-inputs
     (list julia-automa))
    (home-page "https://github.com/BioJulia/BioSymbols.jl")
    (synopsis "Primitive types for nucleic acids and amino acids")
    (description "This package defines the primitive types for nucleic acids
and amino acids that are used ny otherBioJulia packages.")
    (license license:expat)))

(define-public julia-bittwiddlingconveniencefunctions
  (package
    (name "julia-bittwiddlingconveniencefunctions")
    (version "0.1.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url
              "https://github.com/JuliaSIMD/BitTwiddlingConvenienceFunctions.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0qn36wpj7bk4ysizhx0a13ajajjzlc6cxsqhhx9w99d5wciplx4a"))))
    (build-system julia-build-system)
    (propagated-inputs (list julia-static))
    (home-page
     "https://github.com/JuliaSIMD/BitTwiddlingConvenienceFunctions.jl")
    (synopsis "Bit twiddling convenience functions")
    (description
     "This package provides bit twiddling convenience functions in Julia.
These are useful for going to the next or previous mask size or for
calculating corresponding shifts.")
    (license license:expat)))

(define-public julia-blockarrays
  (package
    (name "julia-blockarrays")
    (version "0.16.23")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaArrays/BlockArrays.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "14gby25ixbp9ha0y2aj4gnjkzha4c7v4y3sicicgbkysnq921qd0"))))
    (build-system julia-build-system)
    (arguments
     (list
      #:phases
      (if (target-64bit?)
          #~%standard-phases
          #~(modify-phases %standard-phases
              (add-after 'unpack 'fix-tests-int32-i686
                (lambda _
                  (substitute* "test/test_blockarrays.jl"
                    (("Int64") "Int32"))))))))
    (propagated-inputs
     (list julia-arraylayouts
           julia-fillarrays))
    (native-inputs
     (list julia-aqua
           julia-offsetarrays
           julia-staticarrays))
    (home-page "https://github.com/JuliaArrays/BlockArrays.jl")
    (synopsis "BlockArrays for Julia")
    (description "A block array is a partition of an array into blocks or
subarrays.  This package has two purposes.  Firstly, it defines an interface for
an @code{AbstractBlockArray} block arrays that can be shared among types
representing different types of block arrays.  The advantage to this is that it
provides a consistent API for block arrays.
Secondly, it also implements two different type of block arrays that follow the
@code{AbstractBlockArray} interface.  The type @code{BlockArray} stores each
block contiguously while the type @code{PseudoBlockArray} stores the full matrix
contiguously.  This means that @code{BlockArray} supports fast non copying
extraction and insertion of blocks while @code{PseudoBlockArray} supports fast
access to the full matrix to use in in for example a linear solver.")
    (license license:expat)))

(define-public julia-blockbandedmatrices
  (package
    (name "julia-blockbandedmatrices")
    (version "0.11.9")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaMatrices/BlockBandedMatrices.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1qag5awl8cmsyhpajv6llhpqbzxfii1bacppbjvmb1fqs9s0lifd"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-arraylayouts
           julia-bandedmatrices
           julia-blockarrays
           julia-fillarrays
           julia-matrixfactorizations))
    (home-page "https://github.com/JuliaMatrices/BlockBandedMatrices.jl")
    (synopsis "Block-banded matrices and banded-block-banded matrices")
    (description "This package supports representing block-banded and
banded-block-banded matrices by only storing the entries in the non-zero bands.
A @code{BlockBandedMatrix} is a subtype of @code{BlockMatrix} of
@code{BlockArrays.jl} whose layout of non-zero blocks is banded.")
    (license license:expat)))

(define-public julia-bson
  (package
    (name "julia-bson")
    (version "0.3.6")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaIO/BSON.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1accra3casg66fhn5r07hz3rgs7qf9ld9ajnz8f80aid85zyp891"))))
    (build-system julia-build-system)
    (native-inputs
     (list julia-dataframes))
    (home-page "https://github.com/JuliaIO/BSON.jl")
    (synopsis "Binary JSON serialisation format")
    (description "@code{BSON.jl} is a Julia package for working with the Binary
JSON serialisation format.  It can be used as a general store for Julia data
structures.")
    (license license:expat)))

(define-public julia-bufferedstreams
  (package
    (name "julia-bufferedstreams")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/BioJulia/BufferedStreams.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0sf4sxbq55mg2pwxyxf0c839z1lk0yxg8nmb7617bfbvw31cp88z"))))
    (build-system julia-build-system)
    ;; The package is old and tests are using undefined functions.  They also
    ;; freeze, see
    ;; https://travis-ci.org/BioJulia/BufferedStreams.jl/jobs/491050182
    (arguments
     (list
      #:tests? #f
      #:julia-package-name "BufferedStreams"
      #:julia-package-uuid "e1450e63-4bb3-523b-b2a4-4ffa8c0fd77d"))
    (propagated-inputs
     (list julia-compat))
    (home-page "https://github.com/BioJulia/BufferedStreams.jl")
    (synopsis "Fast composable IO streams")
    (description "@code{BufferedStreams.jl} provides buffering for IO
operations.  It can wrap any @code{IO} type automatically making incremental
reading and writing faster.")
    (license license:expat)))

(define-public julia-calculus
  (package
    (name "julia-calculus")
    (version "0.5.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaMath/Calculus.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0xh0ak2ycsjw2h86ja24ch3kn2d18zx3frrds78aimwdnqb1gdc2"))))
    (build-system julia-build-system)
    (home-page "https://github.com/JuliaMath/Calculus.jl")
    (synopsis "Common utilities for automatic differentiation")
    (description "This package provides tools for working with the basic
calculus operations of differentiation and integration.  The @code{Calculus}
package produces approximate derivatives by several forms of finite
differencing or produces exact derivative using symbolic differentiation.  It
can also be used to compute definite integrals by different numerical
methods.")
    (license license:expat)))

(define-public julia-categoricalarrays
  (package
    (name "julia-categoricalarrays")
    (version "0.10.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaData/CategoricalArrays.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1yd0xky84n858di7ggs4vffil33pf9knw01z64n5v961i4qs8saw"))))
    (build-system julia-build-system)
    (arguments
     (list
       #:phases
       #~(modify-phases %standard-phases
           (add-after 'unpack 'adjust-tests
             (lambda _
               ;; Plots.jl isn't packaged yet.
               (substitute* "test/runtests.jl"
                 ((".*13_arraycommon\\.jl.*") "")))))))
    (native-inputs
     (list julia-json
           julia-json3
           ;julia-plots
           julia-pooledarrays
           julia-recipesbase
           julia-sentinelarrays
           julia-structtypes))
    (propagated-inputs
     (list julia-dataapi
           julia-json
           julia-json3
           julia-missings
           julia-recipesbase
           julia-requires
           julia-sentinelarrays
           julia-structtypes))
    (home-page "https://github.com/JuliaData/CategoricalArrays.jl")
    (synopsis "Arrays for working with categorical data")
    (description "This package provides tools for working with categorical
variables, both with unordered (nominal variables) and ordered categories
(ordinal variables), optionally with missing values.")
    (license license:expat)))

(define-public julia-cenum
  (package
    (name "julia-cenum")
    (version "0.4.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaInterop/CEnum.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0h079mggvv420bw87j8s3hsgk7vavjgm4j1cvk0pnzrrh8ib1381"))))
    (build-system julia-build-system)
    (home-page "https://github.com/JuliaInterop/CEnum.jl")
    (synopsis "C-compatible enum for Julia")
    (description "This package provides a C-compatible enum for Julia.")
    (license license:expat)))

(define-public julia-cfitsio
  (package
    (name "julia-cfitsio")
    (version "1.4.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaAstro/CFITSIO.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "05bxzzjcc021p3hi092h06r2q7qnvql0xz1alggi83i0pp1mxp6d"))))
    (build-system julia-build-system)
    (native-inputs (list julia-aqua))
    (propagated-inputs (list julia-cfitsio-jll))
    (home-page "https://github.com/JuliaAstro/CFITSIO.jl")
    (synopsis "C-style interface to the libcfitsio library")
    (description "This package provides Julia implementation of C-style
interface to CFITSIO functions with following features:
@itemize
@item Function names closely mirror the C interface (e.g.,
@code{fits_open_file()}).
@item Functions operate on @code{FITSFile}, a thin wrapper for fitsfile C
struct (@code{FITSFile} has concept of \"current HDU\", as in CFITSIO).
@item Wrapper functions do check the return status from CFITSIO and throw an
error with the appropriate message.
@end itemize")
    (license license:expat)))

(define-public julia-chainrules
  (package
    (name "julia-chainrules")
    (version "1.35.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaDiff/ChainRules.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "17irgz3gamyrmzsjhq4s1n1sblvhkj10yg5y5y53yr631cl2fr6a"))))
    (build-system julia-build-system)
    (arguments
     (list #:tests? #f))        ; JuliaInterpreter.jl not packaged yet.
    ;(inputs                             ;required for test
    ; (list julia-chainrulestestutils
    ;       julia-finitedifferences
    ;       julia-juliainterpreter))
    (propagated-inputs
     (list julia-chainrulescore
           julia-compat
           julia-irrationalconstants
           julia-realdot))
    (home-page "https://github.com/JuliaDiff/ChainRules.jl")
    (synopsis "Common utilities for automatic differentiation")
    (description "The is package provides a variety of common utilities that
can be used by downstream automatic differentiation (AD) tools to define and
execute forward-, reverse-, and mixed-mode primitives.")
    (license license:expat)))

(define-public julia-chainrulescore
  (package
    (name "julia-chainrulescore")
    (version "1.12.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaDiff/ChainRulesCore.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0lgfcsb7f6c7knhiz5dbqh8x47d370pn71y9ys2y6763g0b4pm61"))))
    (build-system julia-build-system)
    (arguments
     (list
       #:phases
       #~(modify-phases %standard-phases
           (add-after 'unpack 'adjust-tests
             (lambda _
               (substitute* "test/tangent_types/tangent.jl"
                 ;; This test is disabled after the release.
                 (("@test haskey.*Float.*") "")
                 (("@test (.*construct)" _ test)
                  (string-append "@test_broken " test))))))))
    (inputs                             ;required for tests
     (list julia-benchmarktools
           julia-staticarrays))
    (propagated-inputs
     (list julia-compat))
    (home-page "https://github.com/JuliaDiff/ChainRulesCore.jl")
    (synopsis "Common utilities used by downstream automatic differentiation tools")
    (description "The package provides a light-weight dependency for defining
sensitivities for functions without the need to depend on ChainRules itself.")
    (license license:expat)))

(define-public julia-chainrulestestutils
  (package
    (name "julia-chainrulestestutils")
    (version "1.5.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaDiff/ChainRulesTestUtils.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0vk7cpp049pjj7g5zqxr7djp5v0swhvhq3wvkxyw8m8xvqlnfncc"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-chainrulescore
           julia-compat
           julia-finitedifferences))
    (home-page "https://github.com/JuliaDiff/ChainRulesTestUtils.jl")
    (synopsis "Common utilities used by downstream automatic differentiation tools")
    (description "This package is designed to help in testing
@code{ChainRulesCore.frule} and @code{ChainRulesCore.rrule} methods.  The main
entry points are @code{ChainRulesTestUtils.frule_test},
@code{ChainRulesTestUtils.rrule_test}, and
@code{ChainRulesTestUtils.test_scalar}. Currently this is done via testing the
rules against numerical differentiation (using @code{FiniteDifferences.jl}).

@code{ChainRulesTestUtils.jl} is separated from @code{ChainRulesCore.jl} so that it
can be a test-only dependency, allowing it to have potentially heavy
dependencies, while keeping @code{ChainRulesCore.jl} as light-weight as possible.")
    (license license:expat)))

(define-public julia-changesofvariables
  (package
    (name "julia-changesofvariables")
    (version "0.1.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaMath/ChangesOfVariables.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0szkmz7r41hi4zbxli5m9g5r3gyg87y0vc0rd1s1rp28p34a3grw"))))
    (build-system julia-build-system)
    (arguments
     ;; Pulls in ForwardDiff, would have a cyclical
     ;; dependency with LogExpFunctions.
     (list
      #:tests? #f
      #:julia-package-name "ChangesOfVariables"
      #:julia-package-uuid "9e997f8a-9a97-42d5-a9f1-ce6bfc15e2c0"
      #:julia-package-dependencies
      #~(list '("LinearAlgebra" . "37e2e46d-f89d-539d-b4ee-838fcccc9c8e")
              '("Test" . "8dfed614-e22c-5e08-85e1-65c5234f0b40"))))
    (propagated-inputs (list julia-chainrulescore julia-inversefunctions))
    (home-page "https://github.com/JuliaMath/ChangesOfVariables.jl")
    (synopsis "Interface for transformation functions in Julia")
    (description
     "This package defines functionality to calculate volume element
changes for functions that perform a change of variables (like coordinate
transformations).")
    (license license:expat)))

(define-public julia-clausenfunctions
  (package
    (name "julia-clausenfunctions")
    (version "1.16.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Expander/ClausenFunctions.jl")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0vwzbx3nykm6zv4hkpwhfhc0x58zafn8s9xlw6xlj4glaf0v11sk"))))
    (build-system julia-build-system)
    (propagated-inputs (list julia-polylog))
    (home-page "https://github.com/Expander/ClausenFunctions.jl")
    (synopsis "Clausen functions in Julia")
    (description
     "This package provides Julia implementations of the
Standard Clausen functions and Glaisher-Clausen functions of integer
order for real or complex arguments.")
    (license license:expat)))

(define-public julia-codeczlib
  (package
    (name "julia-codeczlib")
    (version "0.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaIO/CodecZlib.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0xm603nylkwk4bzx66zv1g3syzrvn3jh9spdx7kvcvgszzyrrgh4"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-transcodingstreams
           julia-zlib-jll))
    (home-page "https://github.com/JuliaIO/CodecZlib.jl")
    (synopsis "Zlib codecs for @code{TranscodingStreams.jl}")
    (description "This package provides zlib codecs for
@code{TranscodingStreams.jl}.")
    (license license:expat)))

(define-public julia-colors
  (package
    (name "julia-colors")
    (version "0.12.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaGraphics/Colors.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1g0fvvz09pfk6jxqrdplwkw1yywcqvwjd3ga24hblq71mah367n6"))))
    (build-system julia-build-system)
    (arguments
     (list
       #:phases
       #~(modify-phases %standard-phases
           (add-after 'unpack 'adjust-tests
             (lambda _
               (substitute* "test/runtests.jl"
                 ((".*detect_ambiguities.*") "")))))))
    (propagated-inputs
     (list julia-colortypes
           julia-fixedpointnumbers
           julia-reexport))
    (native-inputs
     (list julia-abstracttrees))
    (home-page "https://github.com/JuliaGraphics/Colors.jl")
    (synopsis "Tools for dealing with color")
    (description "This package provides a wide array of functions for dealing
with color.  This includes conversion between colorspaces, measuring distance
between colors, simulating color blindness, parsing colors, and generating
color scales for graphics.")
    (license license:expat)))

(define-public julia-colorschemes
  (package
    (name "julia-colorschemes")
    (version "3.15.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaGraphics/ColorSchemes.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0kpjhmqd5cj8dh8bmh9b5g6bscyp7h23hzpr2s93pnrp57q1wvhq"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-colors
           julia-colortypes
           julia-fixedpointnumbers
           julia-staticarrays))
    (home-page "https://github.com/JuliaGraphics/ColorSchemes.jl")
    (synopsis "Colorschemes, colormaps, gradients, and palettes")
    (description "This package provides a collection of colorschemes.")
    (license license:expat)))

(define-public julia-colortypes
  (package
    (name "julia-colortypes")
    (version "0.11.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaGraphics/ColorTypes.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0cp5wbi2bhnxp4h7wpzkx341d47744f4c9a8n0w0kn016qa16m86"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-fixedpointnumbers))
    (native-inputs
     (list julia-documenter))
    (home-page "https://github.com/JuliaGraphics/ColorTypes.jl")
    (synopsis "Basic color types and constructor")
    (description "This minimalistic package serves as the foundation for
working with colors in Julia.  It defines basic color types and their
constructors, and sets up traits and show methods to make them easier to work
with.")
    (license license:expat)))

(define-public julia-colorvectorspace
  (package
    (name "julia-colorvectorspace")
    (version "0.9.9")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaGraphics/ColorVectorSpace.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "07scws2bn2z3f2crhnx1zxk3zk3vzfv9iz6lv3i9785nplmsgdx9"))))
    (build-system julia-build-system)
    (arguments
     (list
       #:tests? #f))    ; TODO: Reenable the test suite.
    (propagated-inputs
     (list julia-colortypes
           julia-fixedpointnumbers
           julia-specialfunctions
           julia-tensorcore))
    (native-inputs
     (list julia-colors))
    (home-page "https://github.com/JuliaGraphics/ColorVectorSpace.jl")
    (synopsis "Treat colors as n-vectors for the purposes of arithmetic")
    (description "This package is an add-on to @code{ColorTypes.jl} and provides
fast mathematical operations for objects with types such as RGB and Gray.
Specifically, with this package both grayscale and RGB colors are treated as if
they are points in a normed vector space.")
    (license license:expat)))

(define-public julia-combinatorics
  (package
    (name "julia-combinatorics")
    (version "1.0.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaMath/Combinatorics.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0gafqkqi874zfm9h99akw9q95lk3ih5gip2h8p12fj9h7rvyf4j5"))))
    (build-system julia-build-system)
    (home-page "https://github.com/JuliaMath/Combinatorics.jl")
    (synopsis "Combinatorics library for Julia")
    (description "This package provides a combinatorics library for Julia,
focusing mostly (as of now) on enumerative combinatorics and permutations.")
    (license license:expat)))

(define-public julia-commonsolve
  (package
    (name "julia-commonsolve")
    (version "0.2.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/SciML/CommonSolve.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1w05fp02g5cmqzqp96hcpriwjpqx61sl481rj92gf4y4xpinmdf5"))))
    (build-system julia-build-system)
    (home-page "https://docs.sciml.ai/CommonSolve/stable")
    (synopsis "Common solve function for scientific machine learning")
    (description
     "CommonSolve.jl provides @code{solve}, @code{init}, @code{solve!}, and
@code{step!} commands.  By using the same definition, solver libraries from
other completely different ecosystems can extend the functions and thus not
clash with SciML if both ecosystems export the solve command.")
    (license license:expat)))

(define-public julia-commonsubexpressions
  (package
    (name "julia-commonsubexpressions")
    (version "0.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rdeits/CommonSubexpressions.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0mgy90kk8ksv3l720kkk04gnhn4aqhh2dj4sp3x8yy3limngfjay"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-macrotools))
    (home-page "https://github.com/rdeits/CommonSubexpressions.jl")
    (synopsis "@code{@@cse} macro for Julia")
    (description "This package provides the @code{@@cse} macro, which performs
common subexpression elimination.")
    (license license:expat)))

(define-public julia-compat
  (package
    (name "julia-compat")
    (version "3.39.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaLang/Compat.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0qzvaqi5gqgc747fnajbvvf5vqbh6cwykwky00c7glvmvdsgk3z0"))))
    (build-system julia-build-system)
    (arguments
     (list
       #:phases
       #~(modify-phases %standard-phases
           (add-after 'unpack 'patch-shell-invocation
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "test/runtests.jl"
                (("shcmd = `sh`") (string-append "shcmd = `" (which "sh") "`"))))))))
    (home-page "https://github.com/JuliaLang/Compat.jl")
    (synopsis "Compatibility across Julia versions")
    (description "The Compat package is designed to ease interoperability
between older and newer versions of the Julia language.  The Compat package
provides a macro that lets you use the latest syntax in a backwards-compatible
way.")
    (license license:expat)))

(define-public julia-compositionsbases
  (package
    (name "julia-compositionsbase")
    (version "0.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaFunctional/CompositionsBase.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "11cfmc0rv0i8j6l7v59k4b367xx006nsxy9lkmqlzikc679zzzwa"))))
    (build-system julia-build-system)
    (native-inputs (list julia-documenter))
    (propagated-inputs (list julia-inversefunctions))
    (home-page "https://github.com/JuliaFunctional/CompositionsBase.jl")
    (synopsis "Defines an operator for composition of morphisms")
    (description "This package defines a new operator for composition
of morphisms.")
    (license license:expat)))

(define-public julia-configurations
  (package
    (name "julia-configurations")
    (version "0.16.4")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/Roger-luo/Configurations.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1b23p0zk8dx2sf01cnw177mqci7qd81b9s32ixz9clsh0r0icl1b"))))
    (build-system julia-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'link-depot 'fix-tests
            (lambda _
              (substitute* "test/runtests.jl"
                (("option.toml") "test/option.toml"))))
          (add-after 'link-depot 'dont-use-exproniconlite
            (lambda _
              (substitute* '("Project.toml"
                             "src/Configurations.jl"
                             "test/runtests.jl")
                (("ExproniconLite") "Expronicon"))
              (substitute* "Project.toml"
                (("55351af7-c7e9-48d6-89ff-24e801d99491")
                 "6b7a57c9-7cc1-4fdf-b7f5-e857abae3636"))))
          #$@(if (target-64bit?)
                 '()
                 '((add-after 'unpack 'fix-tests-int32-i686
                     (lambda _
                       (substitute* "test/runtests.jl"
                         (("Int64") "Int32")))))))))
    (propagated-inputs
     (list julia-crayons
           julia-expronicon
           julia-orderedcollections))
    (home-page "https://configurations.rogerluo.dev/stable/")
    (synopsis "Tools for options and configurations in Julia")
    (description "@code{Configurations.jl} provides a macro @code{@@option} to
let you define @code{structs} to represent options/configurations, and serialize
between different option/configuration file formats such as @code{TOML}.")
    (license license:expat)))

(define-public julia-constructionbase
  (package
    (name "julia-constructionbase")
    (version "1.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaObjects/ConstructionBase.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1jk3h446vkv4yaavgm1hf1az7cwhppvhklvr08s49hhg02cm750q"))))
    (build-system julia-build-system)
    (home-page "https://juliaobjects.github.io/ConstructionBase.jl/dev/")
    (synopsis "Primitive functions for construction of objects")
    (description "This very lightweight package provides primitive functions
for construction of objects.")
    (license license:expat)))

(define-public julia-coordinatetransformations
  ;; Test suite fixed after the last release.
  (let ((commit "78f5a5cc8cf77f21407b4f175673fa4f6bf86633")
        (revision "1"))
    (package
      (name "julia-coordinatetransformations")
      (version (git-version "0.6.2" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/JuliaGeometry/CoordinateTransformations.jl")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32 "026g3b2m2z509jdlqvd46yhnhg8y6m00plr3k7cjlbzrfi2yjjn8"))))
      (build-system julia-build-system)
      (propagated-inputs
       (list julia-staticarrays))
      (native-inputs
      (list julia-documenter
            julia-forwarddiff
            julia-unitful))
      (home-page "https://github.com/JuliaGeometry/CoordinateTransformations.jl")
      (synopsis "Coordinate transformations in Julia")
      (description "@code{CoordinateTransformations} is a Julia package to manage
simple or complex networks of coordinate system transformations.
Transformations can be easily applied, inverted, composed, and differentiated
(both with respect to the input coordinates and with respect to transformation
parameters such as rotation angle).  Transformations are designed to be
light-weight and efficient enough for, e.g., real-time graphical applications,
while support for both explicit and automatic differentiation makes it easy to
perform optimization and therefore ideal for computer vision applications such
as SLAM (simultaneous localization and mapping).")
      (license license:expat))))

(define-public julia-cpuid
  (package
    (name "julia-cpuid")
    (version "0.3.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/m-j-w/CpuId.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0x2csy8cvd1rm49qpqpkb3zs7swj4r91zdsyjafqr7hvjp9h3hjz"))))
    (build-system julia-build-system)
    (propagated-inputs (list julia-precompiletools))
    (home-page "https://github.com/m-j-w/CpuId.jl")
    (synopsis "Ask the CPU for its features and specifications")
    (description
     "This package allows you to query the availability of specific
CPU features with low run-time cost.")
    (license license:expat)))

(define-public julia-cpusummary
  (package
    (name "julia-cpusummary")
    (version "0.2.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaSIMD/CPUSummary.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "06kmmnhhcdgm538ax6lblklrih91p2ligg328kljq144j8s6cixm"))))
    (build-system julia-build-system)
    (propagated-inputs (list julia-cpuid julia-ifelse julia-precompiletools
                             julia-static))
    (home-page "https://github.com/JuliaSIMD/CPUSummary.jl")
    (synopsis "Provides a summary of CPU features")
    (description
     "This package provides a summary of available CPU features in Julia.")
    (license license:expat)))

(define-public julia-crayons
  (package
    (name "julia-crayons")
    (version "4.1.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/KristofferC/Crayons.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0vfbb02pclwlbpcl7rhr98a495kga5wydf5wz1gp1xn1wxgpgxpd"))))
    (build-system julia-build-system)
    (home-page "https://github.com/KristofferC/Crayons.jl")
    (synopsis "Colored and styled strings for terminals")
    (description "Crayons is a package that makes it simple to write strings in
different colors and styles to terminals.  It supports the 16 system colors,
both the 256 color and 24 bit true color extensions, and the different text
styles available to terminals.")
    (license license:expat)))

(define-public julia-cstparser
  (package
    (name "julia-cstparser")
    (version "3.3.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/julia-vscode/CSTParser.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "162jpcdph26ybg7rajbvfcbpnngygybpzk5bry4c4ppda3m1dl1i"))))
    (build-system julia-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-or-ignore-tests
            (lambda _
              (substitute* "test/iterate.jl"
                (("parser.jl")
                 (string-append #$output "/share/julia/loadpath/CSTParser/test/parser.jl"))
                (("../src")
                 (string-append #$output "/share/julia/loadpath/CSTParser/src")))
              (substitute* "test/check_base.jl"
                (("testset.*" all)
                 (string-append all "return\n"))))))))
    (inputs (list julia-tokenize))
    (home-page "https://github.com/julia-vscode/CSTParser.jl")
    (synopsis "Parser for Julia")
    (description "This package provides a parser for Julia code.")
    (license license:expat)))

(define-public julia-csv
  (package
    (name "julia-csv")
    (version "0.10.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaData/CSV.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "15kjh5wa6ravb10n9n9bsh7ggmarqmw8s57p35l4b3dqk9d8qafh"))))
    (build-system julia-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'fix-reference-to-cat
                 (lambda _
                   (substitute* "test/basics.jl"
                     ;; XXXX: Test fail to read using CVS.Chunk; raising:
                     ;; ArgumentError: unable to iterate chunks from input file source
                     ;; Disable and the two tests using it.
                     (("chunks = CSV.Chunks") "# chunks = CSV.Chunks")
                     (("@test sum\\(length, chunks\\) == 10000")
                      "# @test sum(length, chunks) == 10000")
                     (("@test Tables.partitions\\(chunks\\) === chunks")
                      "# @test Tables.partitions(chunks) === chunks")))))))
    (propagated-inputs
     (list julia-codeczlib
           julia-filepathsbase
           julia-inlinestrings
           julia-parsers
           julia-pooledarrays
           julia-sentinelarrays
           julia-tables
           julia-weakrefstrings))
    (home-page "https://github.com/JuliaData/CSV.jl")
    (synopsis "Fast and flexible delimited-file reader/writer")
    (description "This package provides reader/writer for delimited text data,
as comma-delimited (csv), tab-delimited (tsv), or otherwise.")
    (license license:expat)))

(define-public julia-dataapi
  (package
    (name "julia-dataapi")
    (version "1.16.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaData/DataAPI.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1n3i5ajm7a955nggba8k15m9i0ybiq42v6drn5dqb57lj6sylbbz"))))
    (build-system julia-build-system)
    (home-page "https://github.com/JuliaData/DataAPI.jl")
    (synopsis "Data-focused namespace for packages to share functions")
    (description "This package provides a namespace for data-related generic
function definitions to solve the optional dependency problem; packages wishing
to share and/or extend functions can avoid depending directly on each other by
moving the function definition to DataAPI.jl and each package taking a
dependency on it.")
    (license license:expat)))

(define-public julia-dataframes
  (package
    (name "julia-dataframes")
    (version "1.3.6")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaData/DataFrames.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "01ybc1ckn5wi7kwp29g5ms4m3g650856z4xv71racbdr8475pmg5"))))
    (build-system julia-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'link-depot 'skip-failing-test
            (lambda _
              ;; Tests with non-standard colors.
              (substitute* "test/show.jl"
                (("test (sprint\\(show, df, context=:color=>true)" _ test)
                 (string-append "test_nowarn " test)))
              (substitute* "test/io.jl"
                (("testset \\\"improved.*" all)
                 (string-append all "return\n")))
              (substitute* "test/join.jl"
                (("test (levels\\(outerjoin\\(B)" _ test)
                 (string-append "test_nowarn " test)))
              ;; Compat with julia-1.8, remove with next package update.
              (substitute* "test/indexing_offset.jl"
                (("@test_throws ErrorException")
                 "@test_throws Base.CanonicalIndexError")))))))
    (propagated-inputs
     (list julia-categoricalarrays
           julia-compat
           julia-dataapi
           julia-invertedindices
           julia-iteratorinterfaceextensions
           julia-missings
           julia-pooledarrays
           julia-prettytables
           julia-reexport
           julia-shiftedarrays
           julia-sortingalgorithms
           julia-tables
           julia-tabletraits
           julia-unitful))
    (native-inputs
     (list julia-categoricalarrays
           julia-combinatorics
           julia-datastructures
           julia-datavalues
           julia-offsetarrays
           julia-shiftedarrays
           julia-unitful))
    (home-page "https://dataframes.juliadata.org/stable/")
    (synopsis "In-memory tabular data")
    (description "This package provides a set of tools for working with tabular
data in Julia.  Its design and functionality are similar to those of Pandas from
Python or @code{data.frame}, @code{data.table} and @code{dplyr} from R, making
it a great general purpose data science tool, especially for those coming to
Julia from R or Python.")
    (license license:expat)))

(define-public julia-datastructures
  (package
    (name "julia-datastructures")
    (version "0.18.13")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaCollections/DataStructures.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ikrgc4d39980nrr77yzcnr1v74wrjh9xvyi2ajfzbcim58vrcqg"))))
    (propagated-inputs
     (list julia-compat
           julia-orderedcollections))
    (build-system julia-build-system)
    (arguments
     (list
      #:phases
      (if (target-x86-32?)
          #~(modify-phases %standard-phases
              (add-after 'unpack 'remove-failing-test-i686
                (lambda _
                  ;; The evaluation returns the correct value,
                  ;; Evaluated: "Accumulator(1 => 3, 3 => 4)"
                  ;; but, for some reasons, is considered as failed.
                  (substitute* "test/test_accumulator.jl"
                    (("@test sprint\\(show,Accumulator\\(1 => 3, 3 => 4\\)\\)")
                     "@test_broken sprint(show, Accumulator(1 => 3, 3 => 4))")))))
          #~%standard-phases)))
    (home-page "https://github.com/JuliaCollections/DataStructures.jl")
    (synopsis "Julia module providing different data structures")
    (description "This package implements a variety of data structures,
including, @code{CircularBuffer}, @code{Queue}, @code{Stack},
@code{Accumulators}, @code{LinkedLists}, @code{SortedDicts} and many others.")
    (license license:expat)))

(define-public julia-datavalueinterfaces
  (package
    (name "julia-datavalueinterfaces")
    (version "1.0.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/queryverse/DataValueInterfaces.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0g2wj6q7jj956nx6g7dk8x7w1c4l2xcmnr1kq5x8s8fild9kslg8"))))
    (build-system julia-build-system)
    (home-page "https://github.com/queryverse/DataValueInterfaces.jl")
    (synopsis "Interface for DataValues.jl")
    (description "This package allows a few \"forward\" definitions for the
@code{DataValues.jl} package that other packages can utilize for integration
without having to take direct dependencies.")
    (license license:expat)))

(define-public julia-datavalues
  (package
    (name "julia-datavalues")
    (version "0.4.13")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/queryverse/DataValues.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "15j3hrqq6nazn533bfsvg32xznacbzsl303j1qs48av59ppnvhhv"))))
    (build-system julia-build-system)
    (arguments
     (list
      #:tests? #f      ; Tests need upgrading with newer Julia version.
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'link-depot 'skip-known-failing-tests
            (lambda _
              ;; See upstream report:
              ;; https://github.com/queryverse/DataValues.jl/issues/83
              (substitute* "test/array/test_reduce.jl"
                ((".*DataValue\\(mapreduce.*") "")
                ((".*DataValue\\(method\\(f.*") "")))))))
    (propagated-inputs
     (list julia-datavalueinterfaces))
    (home-page "https://github.com/queryverse/DataValues.jl")
    (synopsis "Missing values for Julia")
    (description "This package provides the type @code{DataValue} that is used
to represent missing data.")
    (license license:expat)))

(define-public julia-deepdiffs
  (package
    (name "julia-deepdiffs")
    (version "1.2.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ssfrr/DeepDiffs.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1gsbxb1d67g05h5bvzz3swdfih6404jrydy724a8dvbdgqvm3sds"))))
    (build-system julia-build-system)
    ;; This package seems unmaintained but still has dependant packages.
    (arguments (list #:tests? #f))
    (home-page "https://github.com/ssfrr/DeepDiffs.jl")
    (synopsis "Compute and pretty-print diffs for data structures")
    (description "@code{DeepDiffs.jl} provides the @code{deepdiff} function,
which finds and displays differences (diffs) between Julia data structures.  It
supports @code{Vectors}, @code{Dicts}, and @code{String}s.  When diffing
dictionaries where values associated with a particular key may change,
@code{deepdiff} will recurse into value to provide a more detailed diff.")
    (license license:expat)))

(define-public julia-dictionaries
  (package
    (name "julia-dictionaries")
    (version "0.3.10")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/andyferris/Dictionaries.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1mm43hm8hd6sgmkkpqhbqhvap7mpkjwzmz5algxi6manp580gkr5"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-indexing))
    (home-page "https://github.com/andyferris/Dictionaries.jl")
    (synopsis "Alternative interface for dictionaries in Julia")
    (description "This package provides an alternative interface for
dictionaries in Julia, for improved productivity and performance.")
    (license license:expat)))

(define-public julia-distances
  (package
    (name "julia-distances")
    (version "0.10.7")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaStats/Distances.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0sgrh3bzhmqqz0m28lmk66xhnl62i5r2miaiqml8nhbkaapbwc06"))))
    (build-system julia-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'link-depot 'skip-flakey-tests
            (lambda _
              ;; Some combination of these tests fail nondeterministically
              ;; each of the times this package is built.
              (substitute* "test/test_dists.jl"
                (("test dyz ≥") "test_nowarn dyz ≥")
                (("test dist\\(y, x") "test_nowarn dist(y, x")
                (("test dist\\(z, x") "test_nowarn dist(z, x")
                (("test dist\\(z, y") "test_nowarn dist(z, y"))
              #$@(if (not (target-64bit?))
                   ;; A little too much precision
                   ;; Evaluated: 1.8839055991209719 === 1.8839055991209717
                   `((substitute* "test/test_dists.jl"
                       (("@test whamming\\(a, b, w\\) === sum")
                        "@test_skip whamming(a, b, w) === sum")))
                   '()))))))
    (propagated-inputs
     (list julia-statsapi))
    (native-inputs
     (list julia-offsetarrays
           julia-unitful))
    (home-page "https://github.com/JuliaStats/Distances.jl")
    (synopsis "Julia package for evaluating distances (metrics) between vectors")
    (description "A Julia package for evaluating distances(metrics) between
vectors.  This package also provides optimized functions to compute column-wise
and pairwise distances, which are often substantially faster than a
straightforward loop implementation.")
    (license license:expat)))

(define-public julia-docstringextensions
  (package
    (name "julia-docstringextensions")
    (version "0.8.5")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaDocs/DocStringExtensions.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0fy4kfnfacyfmlly6nqxn77dk2gqw80b69zb4m1i0i39zv3cpqfb"))))
    (build-system julia-build-system)
    (arguments
     (list #:tests? #f))        ; Tests try to read SSL certificates.
    (home-page "https://juliadocs.github.io/DocStringExtensions.jl/latest/")
    (synopsis "Extensions for Julia's docsystem")
    (description "This package provides a collection of useful extensions for
Julia's built-in docsystem.  These are features that are not yet mature enough
to be considered for inclusion in Base, or that have sufficiently niche use
cases that including them with the default Julia installation is not seen as
valuable enough at this time.")
    (license license:expat)))

;; By removing all the javascript and css downloads any HTML documentation
;; produced by this package will not be very useful.
(define-public julia-documenter
  (package
    (name "julia-documenter")
    (version "0.27.7")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaDocs/Documenter.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "00ai3c24i3fkn5plmavampcxm0ijhwk0v5cn9xwm7rvbjnnvaaam"))))
    (build-system julia-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'link-depot 'patch-source
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "src/Deps.jl"
                (("pip install")
                 (string-append (search-input-file inputs "bin/pip")
                                " install")))))
          (add-after 'link-depot 'fix-test-git-submodule
            ;; Git v2.38.1 fixes security issues and changes the default
            ;; behaviour of `git submodule`.  This substitution is a backport
            ;; of the upstream patch, not yet released, fixing the test suite.
            ;; https://github.com/JuliaDocs/Documenter.jl/commit/b5a5c65d02d136743e7c18ffebf8baba900484fc
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "test/utilities.jl"
                (("submodule add")
                 "-c protocol.file.allow=always submodule add"))))
          (add-after 'link-depot 'remove-javascript-downloads
            (lambda _
              (substitute* "src/Writers/HTMLWriter.jl"
                (("cdnjs.cloudflare.com") "example.com"))
              ;; Removing the javascript downloads causes these tests fail.
              (substitute* "test/examples/tests.jl"
                ((".*Main\\.examples_html_doc.*") "")
                ((".*Main\\.examples_html_mathjax3_doc.*") "")))))))
    (propagated-inputs
     (list julia-ansicoloredprinters
           julia-docstringextensions
           julia-iocapture
           julia-json))
    (inputs
     (list python-wrapper))
    (native-inputs
     (list git-minimal/pinned                  ;needed for the "Utilities" test
           julia-documentermarkdown
           julia-documentertools))
    (home-page "https://juliadocs.github.io/Documenter.jl")
    (synopsis "Documentation generator for Julia")
    (description "This package provides a documentation generator for Julia.")
    (license license:expat)))

(define julia-documenter-bootstrap
  (package
    (inherit julia-documenter)
    (name "julia-documenter-bootstrap")
    (arguments
     (substitute-keyword-arguments (package-arguments julia-documenter)
       ((#:phases phases)
        `(modify-phases ,phases
           (delete 'patch-source)))
       ;; Not all dependencies available in bootstrap version.
       ((#:tests? _ #f) #f)))
    (inputs `())
    (native-inputs `())))

(define-public julia-documentermarkdown
  (package
    (name "julia-documentermarkdown")
    (version "0.2.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaDocs/DocumenterMarkdown.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0sx89hi5p2f8zi2rp5qrv06m270d90pxj5d2y5cxls1spax7wqx8"))))
    (build-system julia-build-system)
    (inputs
     ;; We don't want to propagate the bootstrap version.
     ;; Cycle with Documenter.jl in later versions.
     (list julia-documenter-bootstrap))
    (home-page "https://github.com/JuliaDocs/DocumenterMarkdown.jl")
    (synopsis "Documenter's Markdown")
    (description "This package enables the Markdown / MkDocs backend of
@code{Documenter.jl}.")
    (license license:expat)))

(define-public julia-documentertools
  (package
    (name "julia-documentertools")
    (version "0.1.13")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaDocs/DocumenterTools.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "05p57p8xlkn42m1lv9gq4hl96vp7hpj19d51p828ai1rbpcpi3a6"))))
    (build-system julia-build-system)
    (arguments
     (list #:tests? #f))    ; Tests require network.
    (inputs
     ;; We don't want to propagate the bootstrap version.
     ;; Cycle with Documenter.jl in later versions.
     (list julia-documenter-bootstrap))
    (propagated-inputs
     (list julia-docstringextensions
           julia-gumbo
           julia-sass))
    (native-inputs
     (list julia-example))
    (home-page "https://github.com/JuliaDocs/DocumenterTools.jl")
    (synopsis "Extra tools for setting up Documenter.jl")
    (description "This package contains utilities for setting up documentation
generation with @code{Documenter.jl}.")
    (license license:expat)))

(define-public julia-diffresults
  (package
    (name "julia-diffresults")
    (version "1.0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaDiff/DiffResults.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1w6p3yxajvclax5b9g7cr2jmbc7lvr5nk4gq0aljxdycdq1d2y3v"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-staticarrays))
    (home-page "https://github.com/JuliaDiff/DiffResults.jl")
    (synopsis "In-place differentiation methods of primal values at multi-order")
    (description "This package provides the @code{DiffResult} type, which can
be passed to in-place differentiation methods instead of an output buffer.")
    (license license:expat)))

(define-public julia-diffrules
  (package
    (name "julia-diffrules")
    (version "1.15.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaDiff/DiffRules.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0gbsi9bl3nk9v0wd0d1prwwxpg57632nwdcj6n6qyv21y2vqzajq"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-irrationalconstants
           julia-logexpfunctions
           julia-nanmath
           julia-specialfunctions))
    (native-inputs
     (list julia-finitedifferences))
    (home-page "https://github.com/JuliaDiff/DiffRules.jl")
    (synopsis "Primitive differentiation rules")
    (description "This package provides primitive differentiation rules that
can be composed via various formulations of the chain rule.  Using
@code{DiffRules}, new differentiation rules can defined, query whether or not
a given rule exists, and symbolically apply rules to simple Julia expressions.")
    (license license:expat)))

(define-public julia-difftests
  (package
    (name "julia-difftests")
    (version "0.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaDiff/DiffTests.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1rxpnd5zi3pxgdd38l5jm2sxc3q6p7g57fqgll2dsiin07y3my57"))))
    (build-system julia-build-system)
    (home-page "https://github.com/JuliaDiff/DiffTests.jl")
    (synopsis "Common test functions for differentiation tools")
    (description "This package contains a common suite of test functions for
stressing the robustness of differentiation tools.")
    (license license:expat)))

(define-public julia-dualnumbers
  (package
    (name "julia-dualnumbers")
    (version "0.6.5")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaDiff/DualNumbers.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "05vr5wbzqpchnb96b3pmn67x196mbfnkv7r9bdlz3gm56if4awk5"))))
    (build-system julia-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'link-depot 'adjust-test-suite
            (lambda _
              (substitute* "test/runtests.jl"
                ;; Seems to not play nicely with SpecialFunctions
                ((".*isempty.*") "")))))))
    (propagated-inputs
     (list julia-calculus
           julia-nanmath
           julia-specialfunctions))
    (home-page "https://github.com/JuliaDiff/DualNumbers.jl")
    (synopsis "Represent dual numbers and for perform dual algebra")
    (description "The @code{DualNumbers} Julia package defines the @code{Dual}
type to represent dual numbers, and supports standard mathematical operations on
them.  Conversions and promotions are defined to allow performing operations on
combinations of dual numbers with predefined Julia numeric types.")
    (license license:expat)))

(define-public julia-dynamicpolynomials
  (package
    (name "julia-dynamicpolynomials")
    (version "0.6.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaAlgebra/DynamicPolynomials.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "07fis5d7paz9s0zf5riw63wmqy8xb95h501p7plajd3w15zar5gh"))))
    (build-system julia-build-system)
    (arguments
     (list
      #:julia-package-name "DynamicPolynomials"
      #:julia-package-uuid "7c1d4256-1411-5781-91ec-d7bc3513ac07"
      #:julia-package-dependencies #~(list '("Future" . "9fa8497b-333b-5362-9e8d-4d0656e87820")
                                           '("LinearAlgebra" . "37e2e46d-f89d-539d-b4ee-838fcccc9c8e")
                                           '("Test" . "8dfed614-e22c-5e08-85e1-65c5234f0b40"))))
    (native-inputs (list julia-chainrulescore julia-combinatorics))
    (propagated-inputs (list julia-multivariatepolynomials
                             julia-mutablearithmetics julia-reexport))
    (home-page "https://github.com/JuliaAlgebra/DynamicPolynomials.jl")
    (synopsis "Multivariate polynomials implementation in Julia")
    (description
     "This package provides a multivariate polynomials implementation of commutative and
 non-commutative variables.")
    (license license:expat)))

(define-public julia-earthorientation
  (package
    (name "julia-earthorientation")
    (version "0.7.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaAstro/EarthOrientation.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1fschx4qmfd83q0ymgbzqi1dl0drbh45cd7hlcbqnm9lfmw2d847"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-leapseconds
           julia-optionaldata
           julia-remotefiles))
    (home-page "https://github.com/JuliaAstro/EarthOrientation.jl")
    (synopsis "Calculate Earth orientation parameters from IERS tables in Julia")
    (description
     "This package provides a functionality to calculate Earth orientation parameters
with data retrieved from @acronym{IERS, International Earth Rotation Service}.")
    (license license:expat)))

(define-public julia-ellipsisnotation
  (package
    (name "julia-ellipsisnotation")
    (version "1.6.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ChrisRackauckas/EllipsisNotation.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0l4fc180chhxlq9d67122c0lgq2hfsxsmcgml2bfl2rnh13gya2b"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-arrayinterface))
    (home-page "https://github.com/ChrisRackauckas/EllipsisNotation.jl")
    (synopsis "Ellipsis notation implementation")
    (description "This implements the notation @code{..} for indexing arrays.
It's similar to the Python @code{...} in that it means \"all of the columns
before (or after)\".")
    (license license:expat)))

(define-public julia-erfa
  (package
    (name "julia-erfa")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaAstro/ERFA.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1f63kyqpsx9n4dh54hzy1bvm3fpl4vf8wi1279vfiza3vhh2ggx5"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-erfa-jll julia-staticarrays))
    (home-page "https://github.com/JuliaAstro/ERFA.jl")
    (synopsis "Julia wrapper for liberfa")
    (description "This package provides a Julia wrapper for astronomical library ERFA.")
    (license license:expat)))

(define-public julia-example
  (let ((commit "f968c69dea24f851d0c7e686db23fa55826b5388"))
    (package
      (name "julia-example")
      (version "0.5.4")                   ;tag not created upstream
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/JuliaLang/Example.jl")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1v3z0d6gh6wfbypffy9m9rhh36px6fm5wjzq0y6rbmc95r0qpqlx"))))
      (build-system julia-build-system)
      (home-page "https://github.com/JuliaLang/Example.jl")
      (synopsis "Module providing examples")
      (description "This package provides various examples.")
      (license license:expat))))

;; ExproniconLite.jl is autogenerated from this package.
(define-public julia-expronicon
  (package
    (name "julia-expronicon")
    (version "0.6.10")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/Roger-luo/Expronicon.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0h8aaynqlxrkn8575k5vqmhzil4vvxchhf0bcxa6zwawp558gj2y"))))
    (build-system julia-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'link-depot 'skip-network-tests
            (lambda _
              (substitute* "test/runtests.jl"
                ;; This test tries to access the Julia package registry.
                ((".*expand\\.jl.*") "")))))))
    (propagated-inputs
     (list julia-mlstyle))
    (native-inputs
     (list julia-documenter))
    (home-page "https://expronicon.rogerluo.dev/dev/")
    (synopsis "Collective tools for metaprogramming on Julia Expr")
    (description "This package provides a collection of tools for
metaprogramming on Julia Expr, the meta programming standard library for
@code{MLStyle}.")
    (license license:expat)))

(define-public julia-exprtools
  (package
    (name "julia-exprtools")
    (version "0.1.8")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/invenia/ExprTools.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0sxrhc5dz1v53zs8sym4csfy28ki00b7x7aihm2zmkrx48if63gb"))))
    (build-system julia-build-system)
    (home-page "https://github.com/invenia/ExprTools.jl")
    (synopsis "Light-weight expression manipulation tools")
    (description "@code{ExprTools} provides tooling for working with Julia
expressions during metaprogramming.  This package aims to provide light-weight
performant tooling without requiring additional package dependencies.")
    (license license:expat)))

(define-public julia-extents
  (package
    (name "julia-extents")
    (version "0.1.4")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/rafaqz/Extents.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0l3f7fv13npd71rhjwb49j2f2aq3az6lyls85bbxxfh3pw51yr78"))))
    (build-system julia-build-system)
    (home-page "https://github.com/rafaqz/Extents.jl")
    (synopsis "Shared Extent object for Julia spatial data")
    (description
     "Extents.jl is a small package that defines an @code{Extent} object that
can be used by the different Julia spatial data packages.  @code{Extent} is a
wrapper for a NamedTuple of tuples holding the lower and upper bounds for each
dimension of a object.")
    (license license:expat)))

(define-public julia-ffmpeg
  (package
    (name "julia-ffmpeg")
    (version "0.4.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaIO/FFMPEG.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1ldxbp0kq3ip67x7sp82dz56aq4p5i0chspbgx2zgskr6jcbjj1b"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-ffmpeg-jll
           julia-x264-jll))
    (home-page "https://github.com/JuliaIO/FFMPEG.jl")
    (synopsis "Julia Package for ffmpeg")
    (description "This package is made to be included into packages that just
need the ffmpeg binaries + executables, and don't want the overhead of
@code{VideoIO.jl}.")
    (license license:expat)))

(define-public julia-fileio
  (package
    (name "julia-fileio")
    (version "1.9.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaIO/FileIO.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1b18x43i737g5q41n9818xbnc2pgd98q1m6yw3h29yri0clg4gfx"))))
    (build-system julia-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (delete 'reset-gzip-timestamps)
          (add-after 'link-depot 'skip-network-tests
            (lambda _
              ;; These tests try to download audio/video files.
              (substitute* "test/query.jl"
                (("testset.*(MP4|OGG|MATROSKA).*" all)
                 (string-append all "return\n")))
              (substitute* "test/loadsave.jl"
                (("testset.*CSVFiles.*" all)
                 (string-append all "return\n")))
              ;; This test tries to download a Julia package.
              (substitute* "test/error_handling.jl"
                (("testset.*Not installed.*" all)
                 (string-append all "return\n")))
              ;; This test tries to write to the store.
              ;; (Error says can't find User 0)
              (substitute* "test/runtests.jl"
                ((".*test_mimesave.*") "")))))))
    (propagated-inputs
     (list julia-requires))
    (native-inputs
     (list julia-colortypes
           julia-filepathsbase
           julia-http))
    (home-page "https://github.com/JuliaIO/FileIO.jl")
    (synopsis "Main Package for IO, loading all different kind of files")
    (description "@code{FileIO} aims to provide a common framework for detecting
file formats and dispatching to appropriate readers/writers.  The two core
functions in this package are called @code{load} and @code{save}, and offer
high-level support for formatted files (in contrast with Julia's low-level
@code{read} and @code{write}).")
    (license license:expat)))

(define-public julia-filepathsbase
  (package
    (name "julia-filepathsbase")
    (version "0.9.10")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/rofinn/FilePathsBase.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "136wm4ik6isrdanmpi4gdr1qw0qhr15i925qzjxbawk5hnyzwng9"))))
    (build-system julia-build-system)
    (arguments
     (list #:tests? #f))    ; Cycle with JLSO.jl
    (home-page "https://github.com/rofinn/FilePathsBase.jl")
    (synopsis "Filesystem path types in Julia")
    (description "@code{FilePathsBase.jl} provides a type based approach to
working with filesystem paths in Julia.")
    (license license:expat)))

(define-public julia-fillarrays
  (package
    (name "julia-fillarrays")
    (version "0.13.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaArrays/FillArrays.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1c4i8awmw9qq8dqfhxwjh76mc1nlmzrl5j754fpnbajv8p49gdv5"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-aqua))
    (inputs                             ;required by tests
     (list julia-staticarrays))
    (home-page "https://github.com/JuliaArrays/FillArrays.jl")
    (synopsis "Lazy matrix representation")
    (description "This package lazily represents matrices filled with
a single entry, as well as identity matrices.  This package exports the
following types: @code{Eye}, @code{Fill}, @code{Ones}, @code{Zeros},
@code{Trues} and @code{Falses}.")
    (license license:expat)))

(define-public julia-finitediff
  (package
    (name "julia-finitediff")
    (version "2.8.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaDiff/FiniteDiff.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "105f6r0hq97n9mxf1nacmz94dpca66vzqj5p3zh4h0brshmggqnq"))))
    (build-system julia-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'pre-check
            (lambda _
              ;; We don't want to run all the tests; the Downstream tests
              ;; try to download the package registry.
              (setenv "GROUP" "Core"))))))
    (propagated-inputs
     (list julia-arrayinterface
           julia-requires
           julia-staticarrays))
    (native-inputs
     (list julia-bandedmatrices
           julia-blockbandedmatrices
           julia-safetestsets))
    (home-page "https://github.com/JuliaDiff/FiniteDiff.jl")
    (synopsis "Calculations of gradients, Jacobians, and Hessians")
    (description "This package is for calculating derivatives, gradients,
Jacobians, Hessians, etc. numerically.  This library is for maximizing speed
while giving a usable interface to end users in a way that specializes on array
types and sparsity.")
    (license license:expat)))

(define-public julia-finitedifferences
  (package
    (name "julia-finitedifferences")
    (version "0.12.17")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaDiff/FiniteDifferences.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "09nsf9cgk49yrvprflnhd9h5rrgs280rgj8sad3csghxdx6jqk5c"))))
    (build-system julia-build-system)
    (arguments
     (list
      #:phases
      (if (target-x86-32?)
          #~(modify-phases %standard-phases
              (add-after 'unpack 'remove-failing-test-i686
                (lambda _
                  ;; Machine Precision incorrectly handled
                  (substitute* "test/methods.jl"
                    (("@test central_fdm\\(15, 5, adapt=2\\)\\(exp, 1.0\\)")
                     "@test_broken central_fdm(15, 5, adapt=2)(exp, 1.0)")))))
          #~%standard-phases)))
    (inputs
     (list julia-benchmarktools))
    (propagated-inputs
     (list julia-chainrulescore
           julia-richardson
           julia-staticarrays))
    (home-page "https://github.com/JuliaDiff/FiniteDifferences.jl")
    (synopsis "Estimates derivatives with finite differences")
    (description "This package calculates approximate derivatives numerically
using finite difference.")
    (license license:expat)))

(define-public julia-fitsio
  (package
    (name "julia-fitsio")
    (version "0.17.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaAstro/FITSIO.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1mvrr13qvzdpqdp01hzqqyx84cxagyvjwkbizsp3xaabckg48pnz"))))
    (build-system julia-build-system)
    (native-inputs
     (list julia-aqua julia-orderedcollections))
    (propagated-inputs
     (list julia-cfitsio julia-reexport julia-tables))
    (home-page "https://github.com/JuliaAstro/CFITSIO.jl")
    (synopsis "Astronomical FITS file support for Julia")
    (description "This package provides Julia implementation for reading and
writing @acronym{FITS, Flexible Image Transport System} files, based on the
@code{cfitsio} library.")
    (license license:expat)))

(define-public julia-fixedpointnumbers
  (package
    (name "julia-fixedpointnumbers")
    (version "0.8.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaMath/FixedPointNumbers.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1cixh2acxscrgxglgbj2mlp2bk2yvqil1kjfvnp1xi0zp6px60f6"))))
    (build-system julia-build-system)
    (arguments
     (list #:tests? #f))      ; Cycle with julia-documenter
    (propagated-inputs
     (list julia-compat))
    (home-page "https://github.com/JuliaMath/FixedPointNumbers.jl")
    (synopsis "Fixed point types for Julia")
    (description "@code{FixedPointNumbers.jl} implements fixed-point number
types for Julia.  A fixed-point number represents a fractional, or
non-integral, number.  In contrast with the more widely known floating-point
numbers, with fixed-point numbers the decimal point doesn't \"float\":
fixed-point numbers are effectively integers that are interpreted as being
scaled by a constant factor.  Consequently, they have a fixed number of
digits (bits) after the decimal (radix) point.")
    (license license:expat)))

(define-public julia-formatting
  (package
    (name "julia-formatting")
    (version "0.4.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaIO/Formatting.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0ma3q9my51rr38bb5712xkc4h3rq0wsfjb4ac6mdh9ywn8rqvrmh"))))
    (build-system julia-build-system)
    (home-page "https://github.com/JuliaIO/Formatting.jl")
    (synopsis "Julia package to provide Python-like formatting support")
    (description "This package offers Python-style general formatting and
c-style numerical formatting.")
    (license license:expat)))

(define-public julia-forwarddiff
  (package
    (name "julia-forwarddiff")
    (version "0.10.36")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaDiff/ForwardDiff.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0mg9b5p3farc05wdxzciykrlx9hy7ivm0dq50hwp0dgd600hdjxy"))))
    (build-system julia-build-system)
    (arguments
     ;; XXXX: Unexpected and non-deterministic failures for i686, e.g.,
     ;; Expression: dual_isapprox(FDNUM ^ PRIMAL, exp(PRIMAL * log(FDNUM)))
     ;; ERROR: LoadError: LoadError: There was an error during testing
     ;; Disable as stopgap.
     (list #:tests? (not (or (%current-target-system)
                             (target-x86-32?)))))
    (inputs                             ;required for tests
     (list julia-calculus
           julia-difftests))
    (propagated-inputs
     (list julia-calculus
           julia-commonsubexpressions
           julia-diffresults
           julia-diffrules
           julia-difftests
           julia-logexpfunctions
           julia-nanmath
           julia-specialfunctions
           julia-staticarrays))
    (home-page "https://github.com/JuliaDiff/ForwardDiff.jl")
    (synopsis "Methods to take multidimensional derivatives")
    (description "This package implements methods to take derivatives,
gradients, Jacobians, Hessians, and higher-order derivatives of native Julia
functions (or any callable object, really) using forward mode automatic
differentiation (AD).")
    (license license:expat)))

(define-public julia-functionwrappers
  (package
    (name "julia-functionwrappers")
    (version "1.1.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/yuyichao/FunctionWrappers.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "02jilpjr7px6138dx2w7ixricvfgsxqdk84d9dgviranibhnjcxa"))))
    (build-system julia-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'link-depot 'adjust-tests
            (lambda _
              (substitute* "test/runtests.jl"
                (("testset \\\"Abstract.*" all)
                 (string-append all "return\n"))))))))
    (home-page "https://github.com/yuyichao/FunctionWrappers.jl")
    (synopsis "Type stable and efficient wrapper of arbitrary functions")
    (description "This package provides a type stable and efficient wrapper of
arbitrary functions.")
    (license license:expat)))

(define-public julia-functors
  (package
    (name "julia-functors")
    (version "0.4.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/FluxML/Functors.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "00rzbj2rs2lq91wz8qyxq14bg7p9i49dq7y44fvxn6jaikha2ymw"))))
    (build-system julia-build-system)
    (native-inputs
     (list julia-documenter
           julia-staticarrays
           julia-zygote))
    (home-page "https://fluxml.ai/Functors.jl/stable/")
    (synopsis "Design pattern for structures as in machine learning")
    (description "This package provides tools to express a design pattern for
dealing with large/ nested structures, as in machine learning and
optimisation.  For large machine learning models it can be cumbersome or
inefficient to work with parameters as one big, flat vector, and structs help
in managing complexity; but it is also desirable to easily operate over all
parameters at once, e.g. for changing precision or applying an optimiser
update step.")
    (license license:expat)))

(define-public julia-fuzzycompletions
  (package
    (name "julia-fuzzycompletions")
    (version "0.4.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JunoLab/FuzzyCompletions.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "07sv88c472n6w4x7diy952igbcfm1s104ysnnvprld83312siw06"))))
    (build-system julia-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'link-depot 'skip-failing-test
            (lambda _
              (substitute* "test/runtests.jl"
                ((".*RPLE.*") "")))))))
    (home-page "https://github.com/JunoLab/FuzzyCompletions.jl")
    (synopsis "Fuzzy completion provider for Julia")
    (description
     "FuzzyCompletions provides fuzzy completions for a Julia runtime session.")
    (license license:expat)))

(define-public julia-genericlinearalgebra
  (package
    (name "julia-genericlinearalgebra")
    (version "0.3.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaLinearAlgebra/GenericLinearAlgebra.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "16k1r02w5qivvr99n5a9impbnnzygpj705irf5ypy208np91xyyd"))))
    (build-system julia-build-system)
    (arguments
     (list
      #:phases
      (if (target-aarch64?)
        #~(modify-phases %standard-phases
            (add-after 'unpack 'skip-some-tests
              (lambda _
                (substitute* "test/lapack.jl"
                  (("@testset.*stedc.*" all)
                   (string-append all "return\n"))
                  (("@testset.*stemr.*" all)
                   (string-append all "return\n"))))))
        #~%standard-phases)))
    (native-inputs
     (list julia-quaternions))
    (home-page "https://github.com/JuliaLinearAlgebra/GenericLinearAlgebra.jl")
    (synopsis "Generic numerical linear algebra")
    (description "The purpose of this package is partly to extend linear algebra
functionality in base to cover generic element types, e.g. @code{BigFloat} and
@code{Quaternion}, and partly to be a place to experiment with fast linear
algebra routines written in Julia (except for optimized BLAS).")
    (license license:expat)))

(define-public julia-genericschur
  (package
    (name "julia-genericschur")
    (version "0.5.3")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/RalphAS/GenericSchur.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "02f2azi6036ca8nlgyvvfagwbks8jxfz4k0d8a709ixr1n0ylwap"))))
    (build-system julia-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'link-depot 'adjust-test-suite
            (lambda _
              (substitute* "test/complex.jl"
                ;; expected Vector{Int32,1}, got a value of type Vector{Int64,1}
                (("A = _example") "#A = _example")
                (("schurtest\\(A,20\\)") ""))
              (substitute* "test/runtests.jl"
                ;; Test errors relating to liblapack.so
                ((".*complex\\.jl.*") "")
                ((".*real\\.jl.*") "")
                ;; GenericSVD is deprecated upstream
                ((".*gordschur\\.jl.*") "")))))))
    (home-page "https://github.com/RalphAS/GenericSchur.jl")
    (synopsis "Schur decomposition of matrices with generic element types")
    (description "The Schur decomposition is the workhorse for eigensystem
analysis of dense matrices.  The diagonal eigen-decomposition of normal
(especially Hermitian) matrices is an important special case, but for non-normal
matrices the Schur form is often more useful.")
    (license license:expat)))

(define-public julia-geometrybasics
  (package
    (name "julia-geometrybasics")
    (version "0.4.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaGeometry/GeometryBasics.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0kxn7gzv4sm3017qbng70iqb4wzy1k2fj5w6lkz1kn7lx7z7m33x"))))
    (build-system julia-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'link-depot 'remove-earcut
            (lambda _
              (substitute* '("Project.toml"
                             "src/GeometryBasics.jl")
                ((".*EarCut.*") ""))))
          (add-after 'link-depot 'skip-incompatible-test
            (lambda _
              (substitute* "test/runtests.jl"
                (("@testset.*MetaT and heterogeneous data.*" all)
                 (string-append all "return\n")))))
          #$@(if (target-64bit?)
                 '()
                 '((add-after 'unpack 'fix-tests-int32-i686
                     (lambda _
                       (substitute* "test/runtests.jl"
                         (("Int64") "Int32")))))))))
    (propagated-inputs
     (list julia-itertools
           julia-staticarrays
           julia-structarrays
           julia-tables))
    (native-inputs
     (list julia-offsetarrays))
    (home-page "https://github.com/JuliaGeometry/GeometryBasics.jl")
    (synopsis "Basic Geometry Types")
    (description "This package aims to offer a standard set of Geometry types,
which easily work with metadata, query frameworks on geometries and different
memory layouts.  The aim is to create a solid basis for Graphics/Plotting,
finite elements analysis, Geo applications, and general geometry manipulations
- while offering a Julian API, that still allows performant C-interop.")
    (license license:expat)))

(define-public julia-gpuarrays
  (package
    (name "julia-gpuarrays")
    (version "8.1.3")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaGPU/GPUArrays.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "129q8m94b2xq3vij28pkb7dry3r7qbiqrz72a26ma9kilcr35gk4"))))
    (build-system julia-build-system)
    (inputs
     (list julia-adapt))
    (home-page "https://github.com/JuliaGPU/GPUArrays.jl")
    (synopsis "Reusable GPU array functionality for various GPU backends")
    (description "This package is the counterpart of AbstractArray interface,
but for GPU array types.  It provides functionality and tooling to speed-up
development of new GPU array types.  This package is not intended for end
users; instead, you should use one of the packages that builds on
@code{GPUArrays.jl}, such as @code{CUDA.jl}, @code{oneAPI.jl} or
@code{AMDGPU.jl}.")
    (license license:expat)))

(define-public julia-gr
  (package
    (name "julia-gr")
    (version "0.69.5")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/jheinen/GR.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0i4vb5y1m47x1ispr52h5a5gs544205vpiz4cypd4pr242f96dcb"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-gr-jll))
    (home-page "https://github.com/jheinen/GR.jl")
    (synopsis "Plotting for Julia based on GR")
    (description "This module provides a Julia interface to GR, a framework for
visualisation applications.")
    (license license:expat)))

(define-public julia-graphics
  (package
    (name "julia-graphics")
    (version "1.1.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaGraphics/Graphics.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "083fppcbmchgnqp4xqdsd4asavq51jq31w8ak35ns701534hr82p"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-colors
           julia-nanmath))
    (home-page "https://github.com/JuliaGraphics/Graphics.jl")
    (synopsis "Base graphics in Julia")
    (description "@code{Graphics.jl} is an abstraction layer for graphical
operations in Julia.")
    (license license:expat)))

(define-public julia-gumbo
  (package
    (name "julia-gumbo")
    (version "0.8.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaWeb/Gumbo.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1g22dv3v7caakspv3pdahnqn937fzzsg9y87rj72hid9g8lxl1gm"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-abstracttrees
           julia-gumbo-jll))
    (home-page "https://github.com/JuliaWeb/Gumbo.jl")
    (synopsis "Julia wrapper around Google's gumbo C library for parsing HTML")
    (description "@code{Gumbo.jl} is a Julia wrapper around Google's gumbo
library for parsing HTML.")
    (license license:expat)))

(define-public julia-hostcpufeatures
  (package
    (name "julia-hostcpufeatures")
    (version "0.1.17")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaSIMD/HostCPUFeatures.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1p0phms2zbmlv8bn20lnwn1jh0xjvz7vq266zf296adha534q9pq"))))
    (build-system julia-build-system)
    (propagated-inputs (list julia-bittwiddlingconveniencefunctions
                             julia-ifelse julia-static))
    (home-page "https://github.com/JuliaSIMD/HostCPUFeatures.jl")
    (synopsis "Provides information about the CPU's features")
    (description "This package provides information about the features of
the host CPU in Julia.")
    (license license:expat)))

(define-public julia-http
  (package
    (name "julia-http")
    (version "0.9.17")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaWeb/HTTP.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ynzcl30sf5r42l75l5x1a8z0643hlck2kysyhag9795gzafxzv3"))))
    (build-system julia-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'install 'disable-network-tests
            (lambda _
              (substitute* "test/runtests.jl"
                (("\"async.jl") "# \"async.jl")
                (("\"client.jl") "# \"client.jl"))
              (substitute* "test/aws4.jl"
                (("@testset.*HTTP.request with AWS authentication.*" all)
                 (string-append all "return\n")))
              (substitute* "test/insert_layers.jl"
                (("@testset.*Inserted final layer runs handler.*" all)
                 (string-append all "return\n")))
              (substitute* "test/multipart.jl"
                (("@testset \"Setting of Content-Type.*" all)
                 (string-append all "return\n"))
                (("@testset \"Deprecation of .*" all)
                 (string-append all "return\n")))
              (substitute* "test/websockets.jl"
                (("@testset.*External Host.*" all)
                 (string-append all "return\n")))
              (substitute* "test/messages.jl"
                (("@testset.*Read methods.*" all)
                 (string-append all "return\n"))
                (("@testset.*Body - .*" all)
                 (string-append all "return\n"))
                (("@testset.*Write to file.*" all)
                 (string-append all "return\n")))
              (substitute* "test/cookies.jl"
                (("@testset.*Set-Cookie casing.*" all)
                 (string-append all "return\n"))))))))
    (propagated-inputs
     (list julia-inifile
           julia-mbedtls
           julia-uris))
    ;; required for tests
    (inputs
     (list julia-json
           julia-bufferedstreams))
    (home-page "https://juliaweb.github.io/HTTP.jl/")
    (synopsis "HTTP support for Julia")
    (description "@code{HTTP.jl} is a Julia library for HTTP Messages,
implementing both a client and a server.")
    (license license:expat)))

(define-public julia-ifelse
  (package
    (name "julia-ifelse")
    (version "0.1.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/sciml/ifelse.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1wrw842r8708fryf2ihp9mkmdrg27saa9nix2c31vs995k2fgr9w"))))
    (build-system julia-build-system)
    (home-page "https://github.com/sciml/ifelse.jl")
    (synopsis "Function form of the if-else conditional statement")
    (description "This package provides a convenient function form of the
conditional ifelse.  It is similar to @code{Core.ifelse} but it is extendable.")
    (license license:expat)))

(define-public julia-imageaxes
  (package
    (name "julia-imageaxes")
    (version "0.6.10")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaImages/ImageAxes.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "15f3y46vcr88fplr7rlibrm3k852p8rzwid5dgmbhc03a8xqd50s"))))
    (build-system julia-build-system)
    (arguments
     (list
       #:phases
       #~(modify-phases %standard-phases
           (add-after 'unpack 'adjust-tests
             (lambda _
               (substitute* "test/runtests.jl"
                 ;; Skip the constantly failing grayscale test.
                 (("@test summary") "@test_broken summary")))))))
    (propagated-inputs
     (list julia-axisarrays
           julia-imagebase
           julia-imagecore
           julia-reexport
           julia-simpletraits))
    (native-inputs
     (list julia-aqua
           julia-documenter
           julia-unitful))
    (home-page "https://github.com/JuliaImages/ImageAxes.jl")
    (synopsis "Julia package for giving \"meaning\" to the axes of an image")
    (description "This small package supports the representation of images as
@code{AxisArrays} to endow the axes with \"meaning,\" and makes programming with
such arrays easy via traits.")
    (license license:expat)))

(define-public julia-imagebase
  (package
    (name "julia-imagebase")
    (version "0.1.5")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaImages/ImageBase.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "00gi268jsyhlkadkkbyiffph6c8yb7zw34px76n6hs7dkfzp6jm3"))))
    (build-system julia-build-system)
    (arguments
     (list #:tests? #f))    ; Cycle with ImageMagick.jl.
    (propagated-inputs
     (list julia-imagecore
           julia-reexport))
    ;(native-inputs
    ; (list julia-aqua
    ;       julia-documenter
    ;       julia-imagefiltering
    ;       julia-imageio
    ;       julia-imagemagick
    ;       julia-offsetarrays
    ;       jula-statistics
    ;       julia-testimages))
    (home-page "https://github.com/JuliaImages/ImageBase.jl")
    (synopsis "Wrapper package around ImageCore")
    (description "This is a twin package to @code{ImageCore} with functions that
are used among many of the packages in JuliaImages.  The main purpose of this
package is to reduce unnecessary compilation overhead from external
dependencies.")
    (license license:expat)))

(define-public julia-imagecore
  (package
    (name "julia-imagecore")
    (version "0.9.4")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaImages/ImageCore.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0a6m3cszgh2bfsgs08i64f1h1pwh6by4267rvwyvdk470z0ayc8q"))))
    (build-system julia-build-system)
    (arguments
     (list #:tests? #f))    ; Cycle with ImageMagick.jl.
    (propagated-inputs
     (list julia-abstractffts
           julia-colors
           julia-colorvectorspace
           julia-fixedpointnumbers
           julia-graphics
           julia-mappedarrays
           julia-mosaicviews
           julia-offsetarrays
           julia-paddedviews
           julia-reexport))
    ;(native-inputs
    ; `(("julia-aqua" ,julia-aqua)
    ;   ("julia-blockarrays" ,julia-blockarrays)
    ;   ("julia-documenter" ,julia-documenter)
    ;   ("julia-fftw" ,julia-fftw)
    ;   ("julia-imageinterminal" ,julia-imageinterminal)
    ;   ("julia-imagemagick" ,julia-imagemagick)
    ;   ("julia-referencetests" ,julia-referencetests)
    ;   ("julia-statistics" ,julia-statistics)))
    (home-page "https://github.com/JuliaImages/ImageCore.jl")
    (synopsis "Julia types for representing images")
    (description "@code{ImageCore} is the lowest-level component of the system
of packages designed to support image processing and computer vision.")
    (license license:expat)))

(define-public julia-imageinterminal
  (package
    (name "julia-imageinterminal")
    (version "0.4.7")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaImages/ImageInTerminal.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0bbpzi7bv8jdiggq1wmcn67vnf96qagvwg0fk95s125wy5980xsl"))))
    (build-system julia-build-system)
    (arguments
     (list #:tests? #f))    ; Cycle with ReferenceTests.jl.
    (propagated-inputs
     (list julia-crayons
           julia-imagebase
           julia-imagecore
           julia-requires))
    ;(native-inputs
    ; `(("julia-coordinatetransformations" ,julia-coordinatetransformations)
    ;   ("julia-imagemagick" ,julia-imagemagick)
    ;   ("julia-imagetransformations" ,julia-imagetransformations)
    ;   ("julia-offsetarrays" ,julia-offsetarrays)
    ;   ("julia-referencetests" ,julia-referencetests)
    ;   ("julia-rotations" ,julia-rotations)
    ;   ("julia-sparsearrays" ,julia-sparsearrays)
    ;   ("julia-testimages" ,julia-testimages)))
    (home-page "https://github.com/JuliaImages/ImageInTerminal.jl")
    (synopsis "Julia package for displaying images in the terminal")
    (description "@code{ImageInTerminal.jl} is a drop-in package that once
imported changes a how a single @code{Colorant} and whole @code{Colorant} arrays
(i.e. Images) are displayed in the interactive REPL.  The displayed images will
be downscaled to fit into the size of your active terminal session.")
    (license license:expat)))

(define-public julia-imagemagick
  (package
    (name "julia-imagemagick")
    (version "1.2.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaIO/ImageMagick.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "05vzv4jsj3l9pv6yrix28hlw7wnag0mqdfjwv8shn4x71hcfxl1p"))))
    (build-system julia-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'link-depot 'skip-failing-test
            (lambda _
              ;; These tests try to download from the imagemagick.org
              (substitute* "test/runtests.jl"
                ((".*readremote\\.jl.*") ""))
              ;; Tests with the color gray are hard.
              (substitute* "test/constructed_images.jl"
                (("test (b == aa)" _ test) (string-append "test_nowarn " test))
                (("test (B == map)" _ test) (string-append "test_nowarn " test))))))))
    (propagated-inputs
     (list julia-fileio
           julia-imagecore
           julia-imagemagick-jll))
    (native-inputs
     (list julia-colors
           julia-colorvectorspace
           julia-imagemetadata
           julia-imageshow
           julia-imagetransformations
           julia-indirectarrays
           julia-offsetarrays
           julia-zipfile))
    (home-page "https://github.com/JuliaIO/ImageMagick.jl")
    (synopsis "Thin wrapper for ImageMagick")
    (description "This package provides a wrapper around ImageMagick version 6.
It was split off from @code{Images.jl} to make image I/O more modular.")
    (license license:expat)))

(define-public julia-imagemetadata
  (package
    (name "julia-imagemetadata")
    (version "0.9.8")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaImages/ImageMetadata.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0rdzvya5szlkg5ds3fw7lpk47hn16655i6265czwf8fxs3hb1gvf"))))
    (build-system julia-build-system)
    (arguments
     (list
       #:phases
       #~(modify-phases %standard-phases
           (add-after 'unpack 'adjust-tests
             (lambda _
               (substitute* "test/operations.jl"
                 ;; Skip the constantly failing greyscale test.
                 (("\\@testset \\\"operations.*" all)
                  (string-append all " return\n"))))))))
    (propagated-inputs
     (list julia-axisarrays
           julia-imageaxes
           julia-imagebase
           julia-imagecore))
    (native-inputs
     (list julia-indirectarrays
           julia-offsetarrays
           julia-simpletraits
           julia-unitful))
    (home-page "https://github.com/JuliaImages/ImageMetadata.jl")
    (synopsis "Julia package for images having metadata")
    (description "@code{ImageMetadata} is a simple package providing utilities
for working with images that have metadata attached.  For example, you might
want to associate an image with the date on which the picture was taken, or an
MRI scan with patient data, or an astronomical image with sky coordinates and
information about the detector used to acquire the image.")
    (license license:expat)))

(define-public julia-imageshow
  (package
    (name "julia-imageshow")
    (version "0.3.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaImages/ImageShow.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "00wq3ab8y6nyhxwc5lpz9dnslsmcr1vg3cjdkh7wb7k6a8bw98mh"))))
    (build-system julia-build-system)
    (arguments
     (list #:tests? #f))    ; cycle with ImageMagick.jl.
    (propagated-inputs
     (list julia-fileio
           julia-imagebase
           julia-imagecore
           julia-offsetarrays
           julia-stackviews))
    ;(native-inputs
    ; `(("julia-imagedistances" ,julia-imagedistances)
    ;   ("julia-imagemagick" ,julia-imagemagick)
    ;   ("julia-suppressor" ,julia-suppressor)
    ;   ("julia-testimages" ,julia-testimages)))
    (home-page "https://github.com/JuliaImages/ImageShow.jl")
    (synopsis
     "Inline graphical display of images in Julia graphical environments")
    (description "This package implements image @code{show} methods suitable
for graphical platforms such as IJulia.  It is intended to provide convenient
inline presentation of greyscale or color images.")
    (license license:expat)))

(define-public julia-imagetransformations
  (package
    (name "julia-imagetransformations")
    (version "0.9.5")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaImages/ImageTransformations.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1wavfs5chq8s9ma0k8fxfaqam4560w4l2j3lhbd9aqsjlgvi3swc"))))
    (build-system julia-build-system)
    (arguments
     (list #:tests? #f))    ; Cycle with ImageMagick.jl.
    (propagated-inputs
     (list julia-axisalgorithms
           julia-colorvectorspace
           julia-coordinatetransformations
           julia-imagebase
           julia-imagecore
           julia-interpolations
           julia-offsetarrays
           julia-rotations
           julia-staticarrays))
    ;(native-inputs
    ; (list julia-endpointranges
    ;       julia-imageio
    ;       julia-imagemagick
    ;       julia-referencetests
    ;       julia-tau
    ;       julia-testimages))
    (home-page "https://github.com/JuliaImages/ImageTransformations.jl")
    (synopsis "Geometric transformations on images for Julia")
    (description "This package provides support for image resizing, image
rotation, and other spatial transformations of arrays.")
    (license license:expat)))

(define-public julia-indexablebitvectors
  (package
    (name "julia-indexablebitvectors")
    (version "1.0.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/BioJulia/IndexableBitVectors.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1khaycydwa31sxwvrrvvlylpzdb77kkxfmb8cax3i22ix0c2nmlc"))))
    (build-system julia-build-system)
    ;; Package without Project.toml
    (arguments
     (list
      #:julia-package-name "IndexableBitVectors"
      #:julia-package-uuid "1cb3b9ac-1ffd-5777-9e6b-a3d42300664d"))
    (home-page "https://github.com/BioJulia/IndexableBitVectors.jl")
    (synopsis "Bit vectors operations with extremely fast speed")
    (description "This package exports following operations over bit vectors
with extremely fast speed while keeping extra memory usage small:
@itemize
@item @code{getindex(bv::IndexableBitVectors, i::Integer)}: @code{i}-th
element of @code{bv}
@item @code{rank(b::Bool, bv::AbstractIndexableBitVector, i::Integer)}: the
number of occurrences of bit @code{b} in @code{bv[1:i]}
@item @code{select(b::Bool, bv::AbstractIndexableBitVector, i::Integer)}: the
index of i-th occurrence of @code{b} in @code{bv}.
@end itemize
and other shortcuts or types.")
    ;; There are plenty of places in the code which rely on the
    ;; length of an Integer in a 64-bit system.
    (supported-systems %64bit-supported-systems)
    (license license:expat)))

(define-public julia-indexing
  (package
    (name "julia-indexing")
    (version "1.1.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/andyferris/Indexing.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1s7bz5aaj9sx753pcaixq83jgbk33adxgybpinjgzb9lzdv1ddgx"))))
    (build-system julia-build-system)
    (home-page "https://github.com/andyferris/Indexing.jl")
    (synopsis "Generalized indexing for Julia")
    (description "This package defines functions for getting multiple indices
out of dictionaries, tuples, etc, extending this ability beyond
@code{AbstractArray}.")
    (license license:expat)))

(define-public julia-indirectarrays
  (package
    (name "julia-indirectarrays")
    (version "0.5.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaArrays/IndirectArrays.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0l0jq0jnr9z3k431ni82xycq7mqapgxrbrx4yyk6lycvi41ipm4s"))))
    (build-system julia-build-system)
    (native-inputs
     (list julia-colors
           julia-fixedpointnumbers
           julia-mappedarrays))
    (home-page "https://github.com/JuliaArrays/IndirectArrays.jl")
    (synopsis "Julia implementation of indexed arrays")
    (description "An @code{IndirectArray} is one that encodes data using a
combination of an @code{index} and a @code{value} table.  Each element is
assigned its own index, which is used to retrieve the value from the
@code{value} table.  Among other uses, @code{IndirectArrays} can represent
indexed images, sometimes called \"colormap images\" or \"paletted images.\"")
    (license license:expat)))

(define-public julia-inflate
  (package
    (name "julia-inflate")
    (version "0.1.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/GunnarFarneback/Inflate.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "16nbl40b819yzmfqs860xbcbx8nnxm0pkvzj49qmxibv5jnsj47q"))))
    (build-system julia-build-system)
    (arguments
     ;; FIXME: Tests fail hard with a lot of errors.
     '(#:tests? #f))
    (propagated-inputs
     (list julia-codeczlib))
    (home-page "https://github.com/GunnarFarneback/Inflate.jl")
    (synopsis "Julia implementation of zlib decompression")
    (description "Inflate provides a pure Julia implementation of zlib decompression
functionality, with both in- memory and streaming interfaces.  This covers
decompression of the Deflate algorithm and the Zlib and Gzip wrapper formats, as
specified in RFC 1950, RFC 1951, and RFC 1952.")
    (license license:expat)))

(define-public julia-infinity
  (package
    (name "julia-infinity")
    (version "0.2.4")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/cjdoris/Infinity.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1941lwvrdjnrynigzixxin3chpg1ba6xplvcwc89x0f6z658hwmm"))))
    (build-system julia-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'link-depot 'remove-timezones.jl
            (lambda _
              (substitute* "test/runtests.jl"
                (("using TimeZones.*") "")
                ((".*infextendedtime.*") "")))))))
    (propagated-inputs
     (list julia-requires))
    (native-inputs
     (list julia-compat))
    (home-page "https://docs.juliahub.com/Infinity/")
    (synopsis "Representation of infinity in Julia")
    (description "This package provides representations for infinity and
negative infinity in Julia.")
    (license license:expat)))

(define-public julia-inifile
  (package
    (name "julia-inifile")
    (version "0.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaIO/IniFile.jl")
             (commit "8ba59958495fa276d6489d2c3903e765d75e0bc0")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "11h6f99jpbg729lplw841m68jprka7q3n8yw390bndlmcdsjabpd"))))
    (build-system julia-build-system)
    (home-page "https://github.com/JuliaIO/IniFile.jl")
    (synopsis "Reading Windows-style INI files")
    (description "This is a Julia package that defines an IniFile type that
interfaces with @file{.ini} files.")
    (license license:expat)))

(define-public julia-initialvalues
  (package
    (name "julia-initialvalues")
    (version "0.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaFolds/InitialValues.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "13gihn824c3vxrpm6vg06vr6zmmy43j1qyya48jig8rdkjp820n6"))))
    (build-system julia-build-system)
    (arguments
     (list #:tests? #f)) ; Cycle with BangBang.jl
    (home-page "https://github.com/JuliaFolds/InitialValues.jl")
    (synopsis
     "Canonical default initial values and identity elements for Julia")
    (description
     "This package provides a canonical set of default initial values
and identity elements for Julia.")
    (license license:expat)))

(define-public julia-inlinestrings
  (package
    (name "julia-inlinestrings")
    (version "1.1.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaStrings/InlineStrings.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1dcdpxlphjliqlnkcri7mhg9bqqzpsdj80h9gkw8xhzr3ls473zr"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-parsers))
    (home-page "https://github.com/JuliaStrings/InlineStrings.jl")
    (synopsis "Fixed-width string types")
    (description "This package provides a set of custom string types of
various fixed sizes.  Each inline string is a custom primitive type and can
benefit from being stack friendly by avoiding allocations/heap tracking in the
GC.  When used in an array, the elements are able to be stored inline since
each one has a fixed size.  Currently support inline strings from 1 byte up to
255 bytes.")
    (license license:expat)))

(define-public julia-interfaces
  (package
    (name "julia-interfaces")
    (version "0.3.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rafaqz/Interfaces.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1g765wzqc16pxrr3kqcbq0b2n1w51mwaxffrv4yjn1mvr607y6xf"))))
    (build-system julia-build-system)
    (native-inputs
     (list julia-aqua))
    (propagated-inputs
     (list julia-documenter))
    (home-page "https://github.com/rafaqz/Interfaces.jl")
    (synopsis "Macros to define and implement interfaces")
    (description
     "This package provides macros for defining the required behaviours of
Julia interfaces, and stating that an object implements them.")
    (license license:expat)))

(define-public julia-interpolations
  (package
    (name "julia-interpolations")
    (version "0.13.6")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaMath/Interpolations.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1skzvgd63rhj1zpn45gi3974rbrir9p2y17zyfmkz6c6nird7bkj"))))
    (build-system julia-build-system)
    (arguments
     (list
      #:parallel-tests? #f
      ;; XXXX: Unexpected failures for i686, e.g.,
      ;; Got exception outside of a @test
      ;; OverflowError: 96908232 * 106943408 overflowed for type Int32
      ;; Disable as stopgap.
      #:tests? (not (or (%current-target-system)
                        (target-x86-32?)))))
    (propagated-inputs
     (list julia-axisalgorithms
           julia-chainrulescore
           julia-offsetarrays
           julia-ratios
           julia-requires
           julia-staticarrays
           julia-woodburymatrices))
    (native-inputs
     (list julia-colorvectorspace
           julia-dualnumbers
           julia-forwarddiff
           julia-offsetarrays
           julia-unitful
           julia-zygote))
    (home-page "https://github.com/JuliaMath/Interpolations.jl")
    (synopsis "Continuous interpolation of discrete datasets")
    (description "This package implements a variety of interpolation schemes for
the Julia language.  It has the goals of ease-of-use, broad algorithmic support,
and exceptional performance.")
    (license license:expat)))

(define-public julia-intervalsets
  (package
    (name "julia-intervalsets")
    (version "0.5.3")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaMath/IntervalSets.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0gsz89cd3iygbl5qr389k9vwpg7w1nk0s90g25nsmk34y9hifxag"))))
    (build-system julia-build-system)
    (arguments
     (list
      #:phases
      (if (target-x86-32?)
          #~(modify-phases %standard-phases
              (add-after 'unpack 'remove-failing-test-i686
                (lambda _
                  (substitute* "test/runtests.jl"
                    ;; For some reason, the output is correct but the test
                    ;; is considered as failed:
                    ;; Expression: duration(ClosedInterval(A, B)) ≡ 60
                    ;; Evaluated: 60 ≡ 60
                    (("@test duration\\(ClosedInterval")
                     "@test_broken duration(ClosedInterval")))))
          #~%standard-phases)))
    (propagated-inputs
     (list julia-ellipsisnotation))
    (native-inputs
     (list julia-offsetarrays))
    (home-page "https://github.com/JuliaMath/IntervalSets.jl")
    (synopsis "Interval Sets for Julia")
    (description "This package is intended to implement a \"minimal\" foundation
for intervals upon which other packages might build.  In particular, we
encourage type-piracy for the reason that only one interval package can
unambiguously define the @code{..} and @code{±} operators.")
    (license license:expat)))

(define-public julia-intervaltrees
  ;; Last upstream release on May 2020 and this last release does not contain
  ;; the file Project.toml.
  (let ((commit "e37edab61568d08141a3e9c25ec55caac21e5aa5")
        (revision "1"))
    (package
      (name "julia-intervaltrees")
      (version (git-version "1.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/BioJulia/IntervalTrees.jl")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "01x48a5zrx0833s1kjhf0ml4x9xz8xja4ymran770akmf6968yl9"))))
      (build-system julia-build-system)
      (home-page "https://github.com/BioJulia/IntervalTrees.jl")
      (synopsis "Interval Trees for Julia")
      (description "This package provides an implementation of an associative
container mapping @code{(K,V)} pairs via the type @code{IntervalTree{K, V}}.
The type @code{K} may be any ordered type.")
      (license license:expat))))

(define-public julia-inversefunctions
  (package
    (name "julia-inversefunctions")
    (version "0.1.8")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaMath/InverseFunctions.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "05g9f6i735x7syfr56l4yf4fy71kgdisjc6cfxi4jkf46iq86a69"))))
    (build-system julia-build-system)
    (native-inputs
     (list julia-documenter))
    (home-page "https://github.com/JuliaMath/InverseFunctions.jl")
    (synopsis "Interface for function inversion")
    (description "This package provides an interface to invert functions.")
    (license license:expat)))

(define-public julia-invertedindices
  (package
    (name "julia-invertedindices")
    (version "1.1.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/mbauman/InvertedIndices.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "15ym4dzyi4fkz0dznni032w3c84zmfa6mrzj2ljqvlqx1i6agqis"))))
    (build-system julia-build-system)
    (native-inputs
     (list julia-offsetarrays))
    (home-page "https://github.com/mbauman/InvertedIndices.jl")
    (synopsis "Index type that allows for inverted selections")
    (description "This package just exports one type: the @code{InvertedIndex},
or @code{Not} for short.  It can wrap any supported index type and may be used
as an index into any @code{AbstractArray} subtype, including OffsetArrays.")
    (license license:expat)))

(define-public julia-iocapture
  (package
    (name "julia-iocapture")
    (version "0.2.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaDocs/IOCapture.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0v76wbvg80g9nx0rjbcna82zk61krly1y9yhyfrjv2pf7mcr4idb"))))
    (build-system julia-build-system)
    (home-page "https://github.com/JuliaDocs/IOCapture.jl")
    (synopsis "Capture standard output and error streams")
    (description "This package provides the @code{IOCapture.capture(f)}
function, which captures the standard output and standard error, and returns it
as a string together with the return value.")
    (license license:expat)))

(define-public julia-irrationalconstants
  (package
    (name "julia-irrationalconstants")
    (version "0.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaMath/IrrationalConstants.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1a007iyh26x67a1bj6fcz7pfxa43kn2v7jpmnz727jkk3xgppg2s"))))
    (build-system julia-build-system)
    (home-page "https://github.com/JuliaMath/IrrationalConstants.jl")
    (synopsis "Additional irrationals for Julia")
    (description "This package provides these irrational constants:
@itemize
@item
@item twoπ       = 2π
@item fourπ      = 4π
@item halfπ      = π / 2
@item quartπ     = π / 4
@item invπ       = 1 / π
@item twoinvπ    = 2 / π
@item fourinvπ   = 4 / π
@item inv2π      = 1 / (2π)
@item inv4π      = 1 / (4π)
@item sqrt2      = √2
@item sqrt3      = √3
@item sqrtπ      = √π
@item sqrt2π     = √2π
@item sqrt4π     = √4π
@item sqrthalfπ  = √(π / 2)
@item invsqrt2   = 1 / √2
@item invsqrtπ   = 1 / √π
@item invsqrt2π  = 1 / √2π
@item loghalf    = log(1 / 2)
@item logtwo     = log(2)
@item logten     = log(10)
@item logπ       = log(π)
@item log2π      = log(2π)
@item log4π      = log(4π)
@end itemize")
    (license license:expat)))

(define-public julia-irtools
  (package
    (name "julia-irtools")
    (version "0.4.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/FluxML/IRTools.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1faddim4gp9pgyadgxi7zdqpdn6qkh7acqpdy29ixpbnb0wgla5r"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-macrotools))
    (native-inputs
     (list julia-documenter))
    (home-page "https://github.com/FluxML/IRTools.jl")
    (synopsis "Simple and flexible IR format")
    (description "This package provides a simple and flexible IR format,
expressive enough to work with both lowered and typed Julia code, as well as
external IRs.  It can be used with Julia metaprogramming tools such as
Cassette.")
    (license license:expat)))

(define-public julia-itemgraphs
  (package
    (name "julia-itemgraphs")
    (version "0.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/helgee/ItemGraphs.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "16w30y7s922dzp7i64wxdrafv4gy13v3rl4k1z5jkvnmnw68kygg"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-lightgraphs))
    (home-page "https://github.com/helgee/ItemGraphs.jl")
    (synopsis "Shortest paths between items")
    (description
     "ItemGraphs is a simple wrapper around LightGraphs that enables most
common use case for graph-like data structures: with collection of items that
are in relations between each other providing the shortest path between two
items.")
    (license license:expat)))

(define-public julia-iteratorinterfaceextensions
  (package
    (name "julia-iteratorinterfaceextensions")
    (version "1.0.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/queryverse/IteratorInterfaceExtensions.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1slpay1dhja8f9gy6z7b3psgvgcknn963dvfqqakvg1grk9ppa09"))))
    (build-system julia-build-system)
    (home-page "https://github.com/queryverse/IteratorInterfaceExtensions.jl")
    (synopsis "Traits for Julia iterators")
    (description "IteratorInterfaceExtensions defines a small number of
extensions to the iterator interface.")
    (license license:expat)))

(define-public julia-itertools
  (package
    (name "julia-itertools")
    (version "1.3.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaCollections/IterTools.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0haf974kcqj6arv4if97ahs4w3dmvslh6ab3hl57r9s41ic36xdq"))))
    (build-system julia-build-system)
    (home-page "https://github.com/JuliaCollections/IterTools.jl")
    (synopsis "Common functional iterator patterns")
    (description
     "Common functional iterator patterns (formerly @code{Iterators.jl}).")
    (license license:expat)))

(define-public julia-jive
  (package
    (name "julia-jive")
    (version "0.2.27")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/wookay/Jive.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "010dxs9p5ab97h80kw12bx5mkraf0584wi0ggk8wnhg10jf3lpam"))))
    (build-system julia-build-system)
    (home-page "https://github.com/wookay/Jive.jl")
    (synopsis "Julia package to help with writing tests")
    (description "@code{Jive.jl} is a Julia package to help with writing tests.")
    (license license:expat)))

(define-public julia-json
  (package
    (name "julia-json")
    (version "0.21.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaIO/JSON.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1l2p852sxq6h5fif3dqshvbw17gb06jmq2nkr88spvp7s0n0nslz"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-datastructures
           julia-fixedpointnumbers
           julia-parsers
           julia-offsetarrays))
    (home-page "https://github.com/JuliaIO/JSON.jl")
    (synopsis "JSON parsing and printing library for Julia")
    (description "@code{JSON.jl} is a pure Julia module which supports parsing
and printing JSON documents.")
    (license license:expat)))

(define-public julia-json3
  (package
    (name "julia-json3")
    (version "1.9.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/quinnj/JSON3.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "11z5maz7v50wd99id8z7838higza0cllh2amkdkrlskbri3v2f17"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-parsers
           julia-structtypes))
    (home-page "https://github.com/quinnj/JSON3.jl")
    (synopsis "JSON package for Julia")
    (description "This package provides another JSON package for Julia, with a
focus on speed and slick struct mapping.")
    (license license:expat)))

(define-public julia-juno
  (package
    (name "julia-juno")
    (version "0.8.4")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JunoLab/Juno.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "02ryj5blnrmck3jisrpwn1x563i7rsg65d7zms52njsvv499gygk"))))
    (build-system julia-build-system)
    (inputs
     (list julia-media))
    (home-page "https://github.com/JunoLab/Juno.jl")
    (synopsis "Integrated Development Environment (IDE) for Julia")
    (description "This package provides tools to help you develop code.  Juno
is built on the Atom text editor.  Juno consists of both Julia and Atom
packages in order to add Julia-specific enhancements, such as syntax
highlighting, a plot pane, integration with Julia's debugger, a console for
running code, and much more.

Consider that the package is “maintenance-only mode” and only receives bug
fixes.  The Julia IDE effort is pointed to extension for VSCode.")
    (license license:expat)))

(define-public julia-latexstrings
  (package
    (name "julia-latexstrings")
    (version "1.3.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/stevengj/LaTeXStrings.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0iijp96ca9mqg5skr6ps7q0lvqaa374lr2zkbbia5q6qgpq0j5ww"))))
    (build-system julia-build-system)
    (native-inputs
     (list julia-documenter))
    (home-page "https://github.com/stevengj/LaTeXStrings.jl")
    (synopsis "Input and display of LaTeX equation strings")
    (description "This is a small package to make it easier to type LaTeX
equations in string literals in the Julia language.")
    (license license:expat)))

(define-public julia-lazyarrays
  (package
    (name "julia-lazyarrays")
    (version "0.22.16")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaArrays/LazyArrays.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "127yld4f26lchw5jwp30g2jkjbm7narfsxwcbggy7dfp43s531c5"))))
    (build-system julia-build-system)
    (arguments
     (list
      #:phases
      (if (target-64bit?)
          #~%standard-phases
          #~(modify-phases %standard-phases
              (add-after 'unpack 'fix-tests-int32-i686
                (lambda _
                  (substitute* "test/multests.jl"
                    (("Int64") "Int32"))))))))
    (propagated-inputs
     (list julia-aqua
           julia-arraylayouts
           julia-fillarrays
           julia-macrotools
           julia-matrixfactorizations
           julia-staticarrays))
    (native-inputs
     (list julia-aqua
           julia-tracker))
    (home-page "https://github.com/JuliaArrays/LazyArrays.jl")
    (synopsis "Lazy arrays and linear algebra")
    (description "This package supports lazy analogues of array operations like
@code{vcat}, @code{hcat}, and multiplication.  This helps with the
implementation of matrix-free methods for iterative solvers.")
    (license license:expat)))

(define-public julia-leapseconds
  (package
    (name "julia-leapseconds")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaTime/LeapSeconds.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "13xa49dx11n7ii77rw6300h1rfl4qlq05ypsprvfmvyww81angkp"))))
    (build-system julia-build-system)
    (native-inputs
     (list julia-erfa))
    (home-page "https://github.com/JuliaTime/LeapSeconds.jl")
    (synopsis "Leap seconds in Julia")
    (description
     "@code{LeapSeconds} provides a functionality to return the difference
between @acronym{TAI, International Atomic Time} and @acronym{UTC, Coordinated
Universal Time} or vice versa for a given date.  For dates after 1972-01-01, this
is the number of leap seconds.")
    (license license:expat)))

(define-public julia-lightgraphs
  (package
    (name "julia-lightgraphs")
    (version "1.3.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/sbromberger/LightGraphs.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ygnbzx32b9ciwgg0rn5i0m33dvrb6dh3an6bnmzac1w67sy2vxq"))))
    (build-system julia-build-system)
    (arguments
     (list
       #:phases
       #~(modify-phases %standard-phases
           ;; FIXME: 8x tests fails adjusting for now.
           ;; ERROR: LoadError: Some tests did not pass: 29548 passed, 0 failed,
           ;; 8 errored, 0 broken.
           (add-after 'unpack 'adjust-tests
             (lambda _
               (substitute* "test/runtests.jl"
                 ;; Got exception outside of a @test BoundsError: attempt to
                 ;; access 1-element Vector{SubString{String}} at index [2]
                 ((".*degeneracy.*") "")
                 ;; Got exception outside of a @test type DataType has no field
                 ;; mutable
                 ((".*shortestpaths.*") ""))
               (substitute* "test/experimental/experimental.jl"
                 ;; Got exception outside of a @test type DataType has no field mutable
                 (("\"shortestpaths\",") ""))
               (substitute* "test/linalg/runtests.jl"
                 ;; ArgumentError: Illegal buffers for SparseMatrixCSC
                 ;; construction 5 [1, 3, 5, 7, 9, 10] [1, 2, 1, 3, 2, 4, 3, 5,
                 ;; 4] [1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
                 ;;
                 ;;  ArgumentError: Illegal buffers for SparseMatrixCSC
                 ;;  construction 5 UInt16[0x0001, 0x0003, 0x0005, 0x0007,
                 ;;  0x0009, 0x000a] UInt16[0x0001, 0x0002, 0x0001, 0x0003,
                 ;;  0x0002, 0x0004, 0x0003, 0x0005, 0x0004] [1, 1, 1, 1, 1, 1,
                 ;;  1, 1, 1, 1]
                 ;;
                 ;;  ArgumentError: Illegal buffers for SparseMatrixCSC
                 ;;  construction 5 Int32[1, 3, 5, 7, 9, 10] Int32[1, 2, 1, 3,
                 ;;  2, 4, 3, 5, 4] [1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
                 ((".*spectral.*") ""))
               (substitute* "test/parallel/runtests.jl"
                 ;; Got exception outside of a @test type DataType has no field
                 ;; mutable
                 ((".*shortestpaths/johnson.*") "")
                 ;; Got exception outside of a @test TaskFailedException nested
                 ;; task error: On worker 2: UndefVarError: nv not defined
                 ((".*utils.*") "")))))))
    (propagated-inputs
     (list julia-arnoldimethod
           julia-datastructures
           julia-inflate
           julia-simpletraits))
    (home-page "https://github.com/sbromberger/LightGraphs.jl")
    (synopsis "Optimized graphs package for Julia")
    (description
     "LightGraphs offers both (a) a set of simple, concrete graph implementations --
Graph (for undirected graphs) and DiGraph (for directed graphs), and (b) an API
for the development of more sophisticated graph implementations under the
AbstractGraph type.")
    (license license:bsd-2)))

(define-public julia-linesearches
  (package
    (name "julia-linesearches")
    (version "7.1.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaNLSolvers/LineSearches.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1qc4la07w6s1xhcyd0hvbnpr31zc1a2ssgyybc8biv5m00g0dnr0"))))
    (build-system julia-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'link-depot 'skip-optim-tests
            (lambda _
              (substitute* "test/examples.jl"
                ;; Prevent a cycle with Optim.jl.
                (("^    SKIPFILE.*") "")
                (("^    #SKIPFILE") "    SKIPFILE"))))
          (add-after 'link-depot 'skip-doublefloats-tests
            (lambda _
              (substitute* "test/runtests.jl"
                (("using DoubleFloats.*") "")
                ((".*arbitrary_precision\\.jl.*") "")))))))
    (propagated-inputs
     (list julia-nlsolversbase
           julia-nanmath
           julia-parameters))
    (native-inputs
     ;; DoubleFloats.jl transitively depends on TimeZones.jl, which is currently
     ;; unpackageable due to its oversized Artifacts.toml.
     (list ;julia-doublefloats
           julia-optimtestproblems))
    (home-page "https://github.com/JuliaNLSolvers/LineSearches.jl")
    (synopsis "Line search methods for optimization and root-finding")
    (description "This package provides an interface to line search algorithms
implemented in Julia.")
    (license license:expat)))

(define-public julia-logexpfunctions
  (package
    (name "julia-logexpfunctions")
    (version "0.3.17")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaStats/LogExpFunctions.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0272c1256r42y6g4wsjmgpwcl5s7z98b8sfmyycckqf0zp5dzxg4"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-chainrulescore
           julia-changesofvariables
           julia-docstringextensions
           julia-inversefunctions
           julia-irrationalconstants))
    (native-inputs
     (list julia-chainrulestestutils
           julia-offsetarrays))
    (home-page "https://github.com/JuliaStats/LogExpFunctions.jl")
    (synopsis "Special functions based on @code{log} and @code{exp}")
    (description "Various special functions based on log and exp moved from
@code{StatsFuns.jl} into a separate package, to minimize dependencies.  These
functions only use native Julia code, so there is no need to depend on
@code{librmath} or similar libraries.")
    (license license:expat)))

(define-public julia-macrotools
  (package
    (name "julia-macrotools")
    (version "0.5.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/FluxML/MacroTools.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0k4z2hyasd9cwxf4l61zk3w4ajs44k69wx6z1ghdn8f5p8xy217f"))))
    (build-system julia-build-system)
    (home-page "https://fluxml.ai/MacroTools.jl")
    (synopsis "Tools for working with Julia code and expressions")
    (description "This library provides tools for working with Julia code and
expressions.  This includes a template-matching system and code-walking tools
that let you do deep transformations of code.")
    (license license:expat)))

(define-public julia-manualmemory
  (package
    (name "julia-manualmemory")
    (version "0.1.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaSIMD/ManualMemory.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ajd92q65cffyb9x6mb1x1aprr2afn8p52bfkbmf303dy5ac63lj"))))
    (build-system julia-build-system)
    ;; Tests have a dependency cycle with VectorizationBase
    ;; VectorizationBase -> LayoutPointers -> ManualMemory -> VectorizationBase
    (arguments
     (list
      #:tests? #f))
    (home-page "https://github.com/JuliaSIMD/ManualMemory.jl")
    (synopsis "Manual memory management utilities in Julia")
    (description "This package provides manually managed memory buffers backed
by @code{NTuples} in Julia.")
    (license license:expat)))

(define-public julia-mappedarrays
  (package
    (name "julia-mappedarrays")
    (version "0.4.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaArrays/MappedArrays.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "08kb28dv1zzqbbxblhyllgs4sjxyp76dgjqhdizcq4zg4i1kls6p"))
        (snippet
         #~(begin
             (use-modules (guix build utils))
             ;; Fix deprecation warning
             ;; https://github.com/JuliaArrays/MappedArrays.jl/pull/51
             (substitute* "src/MappedArrays.jl"
               (("Vararg\\{<:AbstractArray") "Vararg{AbstractArray"))
             ;; Fix test failures
             ;; https://github.com/JuliaArrays/MappedArrays.jl/pull/50
             (substitute* "test/runtests.jl"
               (("_zero\\(x\\) = x > 0 \\? x : 0")
                "_zero(x) = ismissing(x) ? x : (x > 0 ? x : 0)"))))))
    (build-system julia-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'adjust-tests
            (lambda _
              (substitute* "test/runtests.jl"
                ((".*@test_throws ErrorException b.*") ""))

              (when #$(not (target-64bit?))
                (substitute* "test/runtests.jl"
                  (("Int64") "Int32"))))))))
    (propagated-inputs
     (list julia-fixedpointnumbers))
    (native-inputs
     (list julia-colors
           julia-fixedpointnumbers
           julia-offsetarrays))
    (home-page "https://github.com/JuliaArrays/MappedArrays.jl")
    (synopsis "Lazy in-place transformations of arrays")
    (description "This package implements \"lazy\" in-place elementwise
transformations of arrays for the Julia programming language.  Explicitly, it
provides a \"view\" M of an array A so that @code{M[i] = f(A[i])} for a
specified (but arbitrary) function f, without ever having to compute M
explicitly (in the sense of allocating storage for M).  The name of the package
comes from the fact that @code{M == map(f, A)}.")
    (license license:expat)))

(define-public julia-matrixfactorizations
  (package
    (name "julia-matrixfactorizations")
    (version "0.9.3")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaMatrices/MatrixFactorizations.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0sqmig01irmvh617h2rsw44hl39qwv2913nlqjsdz9si5vli2hsl"))))
    (build-system julia-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'link-depot 'skip-failing-test
            (lambda _
              (substitute* "test/test_ul.jl"
                ;; Don't warn on the REPL test.
                (("test String") "test_nowarn String")))))))
    (propagated-inputs
     (list julia-arraylayouts))
    (home-page "https://github.com/JuliaMatrices/MatrixFactorizations.jl")
    (synopsis "Julia package to contain non-standard matrix factorizations")
    (description "A Julia package to contain non-standard matrix factorizations.
At the moment it implements the QL, RQ, and UL factorizations, a combined
Cholesky factorization with inverse, and polar decompositions.  In the future it
may include other factorizations such as the LQ factorization.")
    (license license:expat)))

(define-public julia-mbedtls
  (package
    (name "julia-mbedtls")
    (version "1.0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaLang/MbedTLS.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0zjzf2r57l24n3k0gcqkvx3izwn5827iv9ak0lqix0aa5967wvfb"))))
    (build-system julia-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'install 'disable-network-tests
            ;; Tries to connect to httpbin.org
            (lambda _
              (substitute* "test/runtests.jl"
                (("testhost =") "return #")))))))
    (propagated-inputs
     (list julia-mbedtls-jll))
    (home-page "https://github.com/JuliaLang/MbedTLS.jl")
    (synopsis "Apache's mbed TLS library wrapper")
    (description "@code{MbedTLS.jl} provides a wrapper around the @code{mbed
TLS} and cryptography C library for Julia.")
    (license license:expat)))

(define-public julia-measurements
  (package
    (name "julia-measurements")
    (version "2.8.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaPhysics/Measurements.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1rn7qaf2s3l7awm8q5fjxlp1503g9mjgmsnvrbhjjvwyyn1k705r"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-calculus
           julia-recipesbase
           julia-requires))
    (native-inputs
     (list julia-aqua
           julia-quadgk
           julia-specialfunctions
           julia-unitful))
    (home-page "https://juliaphysics.github.io/Measurements.jl/stable/")
    (synopsis "Error propagation calculator and library")
    (description "@code{Measurements.jl} is an error propagation calculator and
library for physical measurements.  It supports real and complex numbers with
uncertainty, arbitrary precision calculations, operations with arrays, and
numerical integration.  The linear error propagation theory is employed to
propagate the errors.")
    (license license:expat)))

(define-public julia-measures
  (package
    (name "julia-measures")
    (version "0.3.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaGraphics/Measures.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0j34psrdijnqqn9zv0r2sknr1p9q0mmbjvjhmjra37bb5fh2gk8l"))))
    (build-system julia-build-system)
    (home-page "https://github.com/JuliaGraphics/Measures.jl")
    (synopsis "Unified measure and coordinates types")
    (description "This library generalizes and unifies the notion of measures
used in Compose, Compose3D, and Escher.  It allows building up and representing
expressions involving differing types of units that are then evaluated,
resolving them into absolute units.")
    (license license:expat)))

(define-public julia-media
  (package
    (name "julia-media")
    (version "0.5.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JunoLab/Media.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "05jq9j3vs8yfj2lwz3sh1vk5rha06xdcikp9s2q3dn316vryy6di"))))
    (build-system julia-build-system)
    ;; Package without Project.toml
    (arguments
     (list
      #:julia-package-name "Media"
      #:julia-package-uuid "e89f7d12-3494-54d1-8411-f7d8b9ae1f27"
      #:julia-package-dependencies
      #~(list '("MacroTools" . "1914dd2f-81c6-5fcd-8719-6d5c9610ff09"))))
    (propagated-inputs
     (list julia-macrotools))
    (home-page "https://github.com/JunoLab/Media.jl")
    (synopsis "Unified measure and coordinates types")
    (description "This package provides a display system which enables the
user handle multiple input/output devices and decide what media types get
displayed where.")
    (license license:expat)))

(define-public julia-millboard
  (package
    (name "julia-millboard")
    (version "0.2.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/wookay/Millboard.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0k9jqgp285qhckldvvsmfk6s69dcr8s74m2fijgm2vxjj2gqjs1n"))))
    (build-system julia-build-system)
    (native-inputs
     (list julia-jive))
    (home-page "https://github.com/wookay/Millboard.jl")
    (synopsis "Displaying data in tables for Julia")
    (description
     "@code{Millboard.jl} provides a library for getting data in a tablized
format to arrange into rows and columns of cells.")
    (license license:expat)))

(define-public julia-missings
  (package
    (name "julia-missings")
    (version "1.0.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaData/Missings.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1k481rm5lahmjyh34j177d4n10svgr0wm7ps5m3ar3xx6nr26ad5"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-dataapi))
    (home-page "https://github.com/JuliaData/Missings.jl")
    (synopsis "Additional missing value support for Julia")
    (description "This package provides additional functionality for working
with @code{missing} values in Julia.")
    (license license:expat)))

(define-public julia-mlstyle
  (package
    (name "julia-mlstyle")
    (version "0.4.10")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/thautwarm/MLStyle.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0h1cd7cr4c4cnpqyj3180113gdbvcc047lqphp8a8gq5smp3c059"))))
    (build-system julia-build-system)
    (native-inputs
     (list julia-datastructures))
    (home-page "https://thautwarm.github.io/MLStyle.jl/latest/")
    (synopsis "Julia functional programming infrastructures")
    (description "This package provides consistent and extensible functional
programming infrastructures, and metaprogramming facilities.")
    (license license:expat)))

(define-public julia-mocking
  (package
    (name "julia-mocking")
    (version "0.7.3")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/invenia/Mocking.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1cg2is83bjmrchmmxcgx57k8c9b9vlamrw38v4fdhbb6d4six5cg"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-compat
           julia-exprtools))
    (home-page "https://github.com/invenia/Mocking.jl")
    (synopsis "Overload Julia function calls")
    (description "The purpose of this package is to allow Julia function calls
to be temporarily overloaded for the purpose of testing.")
    (license license:expat)))

(define-public julia-mosaicviews
  (package
    (name "julia-mosaicviews")
    (version "0.3.3")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaArrays/MosaicViews.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "04fgxghyb7n2ji76xkb1r1fjhzsdbgmp5wsfyyn3yjcsdqbyp8pz"))))
    (build-system julia-build-system)
    (arguments
     (list #:tests? #f))    ; Cycle with ImageCore.jl
    (propagated-inputs
     (list julia-mappedarrays
           julia-paddedviews
           julia-stackviews))
    ;(native-inputs
    ; `(("julia-colorvectorspace" ,julia-colorvectorspace)
    ;   ("julia-imagecore" ,julia-imagecore)))
    (home-page "https://github.com/JuliaArrays/MosaicViews.jl")
    (synopsis
     "Lazily view a 3D or 4D array as an expanded 2D array as a matrix of slices")
    (description "When visualizing images, it is not uncommon to provide a 2D
view of different image sources.  For example, comparing multiple images of
different sizes, getting a preview of machine learning dataset.  This package
aims to provide easy-to-use tools for such tasks.")
    (license license:expat)))

(define-public julia-msgpack
  (package
    (name "julia-msgpack")
    (version "1.1.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaIO/MsgPack.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1layiqjf9si38pfdcszppgcy4zbfqgld7jlw8x645sm9b17b19fg"))))
    (build-system julia-build-system)
    (home-page "https://github.com/JuliaIO/MsgPack.jl")
    (synopsis "Julia MsgPack implementation")
    (description "@code{MsgPack.jl} is a MessagePack implementation in pure
Julia, with type-driven, overloadable packing/unpacking functionality.")
    (license license:expat)))

(define-public julia-muladdmacro
  (package
    (name "julia-muladdmacro")
    (version "0.2.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/SciML/MuladdMacro.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0pvmfw7f3igpwx0w8c8i40pls0wfm248b1i662wnlrkqiw56j0yq"))))
    (build-system julia-build-system)
    (home-page "https://github.com/SciML/MuladdMacro.jl")
    (synopsis "Julia macro to convert expressions to use muladd calls and FMA operations")
    (description
     "This package provides the @code{@@muladd} macro.  It automatically converts
expressions with multiplications and additions or subtractions to calls with
muladd which then fuse via FMA when it would increase the performance of the
code.  The @code{@@muladd} macro can be placed on code blocks and it will automatically
find the appropriate expressions and nest muladd expressions when necessary.  In
mixed expressions summands without multiplication will be grouped together and
evaluated first but otherwise the order of evaluation of multiplications and
additions is not changed.")
    (license license:expat)))

(define-public julia-multivariatepolynomials
  (package
    (name "julia-multivariatepolynomials")
    (version "0.5.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaAlgebra/MultivariatePolynomials.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1brz4s1if813840crr0bl6wl7lw983vg43cm8vqhx6xjby1v7788"))))
    (build-system julia-build-system)
    (arguments
     (list
      #:tests? #f ; circular dependency with DynamicPolynomials
      #:julia-package-name "MultivariatePolynomials"
      #:julia-package-uuid "102ac46a-7ee4-5c85-9060-abc95bfdeaa3"
      #:julia-package-dependencies
      #~(list '("LinearAlgebra" . "37e2e46d-f89d-539d-b4ee-838fcccc9c8e"))))
    (propagated-inputs
     (list julia-chainrulescore
           julia-datastructures
           julia-mutablearithmetics))
    (home-page "https://github.com/JuliaAlgebra/MultivariatePolynomials.jl")
    (synopsis "Package providing an interface for multivariate polynomials")
    (description
     "This package provides an interface for manipulating multivariate polynomials.
Implementing algorithms on polynomials using this interface will allow the algorithm
to work for all polynomials implementing this interface.  The interface contains
functions for accessing the coefficients, monomials, defining arithmetic operations
on them, rational functions, division with remainder, calculus and differentiation,
and evaluation and substitution.")
    (license license:expat)))

(define-public julia-mutablearithmetics
  (package
    (name "julia-mutablearithmetics")
    (version "1.6.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/jump-dev/MutableArithmetics.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1mwa20f09iw9kaacj34fqa1z4gyg7afnxdv6w0rw35cck33i485k"))))
    (build-system julia-build-system)
    (arguments
     (list
      #:julia-package-name "MutableArithmetics"
      #:julia-package-uuid "d8a4904e-b15c-11e9-3269-09a3773c0cb0"
      #:julia-package-dependencies
      #~(list '("LinearAlgebra" . "37e2e46d-f89d-539d-b4ee-838fcccc9c8e")
              '("SparseArrays" . "2f01184e-e22b-5df5-ae63-d93ebab69eaf")
              '("Test" . "8dfed614-e22c-5e08-85e1-65c5234f0b40"))
      #:phases
      (if (target-x86-32?)
          #~(modify-phases %standard-phases
              (add-after 'unpack 'remove-failing-test-i686
                (lambda _
                  (substitute* "test/utilities.jl"
                    ;; Non-deterministic returned value, e.g.,
                    ;;    Expression: n == @allocated(f())
                    ;;    Evaluated: 240 == 120
                    ;; and for some other values:
                    ;;    Got correct result, please change to @test
                    ;; so @test_broken is not enough.
                    (("@test n == @allocated f\\(\\)")
                     " ")))))
          #~%standard-phases)))
    (propagated-inputs
     (list julia-offsetarrays))
    (home-page "https://github.com/jump-dev/MutableArithmetics.jl")
    (synopsis "Interface for arithmetics on mutable types in Julia")
    (description "MutableArithmetics is a Julia package which allows:
@itemize
@item mutable types to implement mutable arithmetics
@item algorithms that could exploit mutable arithmetics to exploit them while
still being completely generic
@end itemize")
    (license license:mpl2.0)))

(define-public julia-nanmath
  (package
    (name "julia-nanmath")
    (version "0.3.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mlubin/NaNMath.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1fwqa2fzl84a86ppjb2xaqh93b5gg42zyrclbjfdm5l0044hwii6"))))
    (build-system julia-build-system)
    (home-page "https://github.com/mlubin/NaNMath.jl")
    (synopsis "Implementations of basic math functions")
    (description "Implementations of basic math functions which return
@code{NaN} instead of throwing a @code{DomainError}.")
    (license license:expat)))

(define-public julia-nlsolversbase
  (package
    (name "julia-nlsolversbase")
    (version "7.8.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaNLSolvers/NLSolversBase.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0n8qh5a2ghjx1j70zxn0hmh8gzpa46kmjg8di879y9974bfk0f98"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-diffresults
           julia-finitediff
           julia-forwarddiff))
    (native-inputs
     (list julia-optimtestproblems
           julia-recursivearraytools))
    (home-page "https://github.com/JuliaNLSolvers/NLSolversBase.jl")
    (synopsis "Optimization and equation solver software in JuliaNLSolvers")
    (description "This package aims at establishing common ground for Optim.jl,
LineSearches.jl, and NLsolve.jl.  The common ground is mainly the types used to
hold objective related callables, information about the objectives, and an
interface to interact with these types.")
    (license license:expat)))

(define-public julia-nnlib
  (package
    (name "julia-nnlib")
    (version "0.7.34")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/FluxML/NNlib.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1xzlh7pj6aqmbkrskqgwvifprg9a6xkkdh00ls6f6xnzqfrnhwna"))))
    (build-system julia-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'link-depot 'skip-some-tests
            (lambda _
              (substitute* "test/runtests.jl"
                ;; Skip the CUDA tests
                (("using CUDA") "")
                (("&& CUDA\\.functional\\(\\)") "")

                ;; UnicodePlots is only used for the doctests
                (("if VERSION <.*") "if true\n"))
              (setenv "NNLIB_TEST_CUDA" "false"))))))
    (propagated-inputs
     (list julia-adapt
           julia-chainrulescore
           julia-compat
           julia-requires))
    (native-inputs
     (list julia-chainrulestestutils
           julia-stablerngs
           julia-zygote))
    (home-page "https://github.com/FluxML/NNlib.jl")
    (synopsis "Neural Network primitives with multiple backends")
    (description "This package will provide a library of functions useful for
machine learning, such as softmax, sigmoid, convolutions and pooling.  It
doesn't provide any other \"high-level\" functionality like layers or AD.")
    (license license:expat)))

(define-public julia-optim
  (package
    (name "julia-optim")
    (version "1.7.4")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaNLSolvers/Optim.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0pdwa2xm08c3g979qgsmcr343j4kkh4l6x5rdj1blhqh5gw8172b"))))
    (build-system julia-build-system)
    (arguments
     (list
       #:phases
       #~(modify-phases %standard-phases
           (add-after 'unpack 'adjust-tests
             (lambda _
               (substitute* "test/runtests.jl"
                 ;; Distributions.jl isn't packaged yet.
                 ((".*newton_trust_region.*") ""))
               (substitute*
                 "test/multivariate/solvers/constrained/ipnewton/constraints.jl"
                 ;; TODO: Figure out why this test fails.
                 (("@test Optim\\.converged") "@test_skip Optim.converged")
                 (("@test Optim\\.minimum") "@test_skip Optim.minimum")))))))
    (propagated-inputs
     (list julia-compat
           julia-fillarrays
           julia-forwarddiff
           julia-linesearches
           julia-nanmath
           julia-nlsolversbase
           julia-parameters
           julia-positivefactorizations
           julia-statsbase))
    (native-inputs
     (list julia-linesearches
           julia-measurements
           julia-nlsolversbase
           julia-optimtestproblems
           julia-positivefactorizations
           julia-recursivearraytools
           julia-stablerngs))
    (home-page "https://github.com/JuliaNLSolvers/Optim.jl")
    (synopsis "Optimization functions for Julia")
    (description "@code{Optim.jl} is a package for univariate and multivariate
optimization of functions.")
    (license license:expat)))

(define-public julia-optimisers
  (package
    (name "julia-optimisers")
    (version "0.2.13")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/FluxML/Optimisers.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1xs51r365l6r56rpm08kba00nfcl5jlglwy8494w06vbi22ysbq7"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-chainrulescore
           julia-functors
           julia-zygote))
    (native-inputs
     (list julia-staticarrays
           julia-zygote))
    (home-page "https://github.com/FluxML/Optimisers.jl")
    (synopsis "Optimisers and utilities for learning loops")
    (description "@code{Optimisers.jl} defines many standard gradient-based
optimisation rules, and tools for applying them to deeply nested models.")
    (license license:expat)))

(define-public julia-optimtestproblems
  (package
    (name "julia-optimtestproblems")
    (version "2.0.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaNLSolvers/OptimTestProblems.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "10h47x5ws42pkqjccimaz0yxfvz41w0yazq6inamfk4lg5g2g3d9"))))
    (build-system julia-build-system)
    (arguments
     (list
      #:julia-package-name "OptimTestProblems"
      #:julia-package-uuid "cec144fc-5a64-5bc6-99fb-dde8f63e154c"
      #:julia-package-dependencies
      #~(list '("LinearAlgebra" . "37e2e46d-f89d-539d-b4ee-838fcccc9c8e")
              '("SparseArrays" . "2f01184e-e22b-5df5-ae63-d93ebab69eaf")
              '("Test" . "8dfed614-e22c-5e08-85e1-65c5234f0b40"))))
    (home-page "https://github.com/JuliaNLSolvers/OptimTestProblems.jl")
    (synopsis "Collection of optimization test problems")
    (description "The purpose of this package is to provide test problems for
JuliaNLSolvers packages.")
    (license license:expat)))

(define-public julia-optionaldata
  (package
    (name "julia-optionaldata")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/helgee/OptionalData.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "11js258j7cz6362ijsi24nih3lx90aalf8k1n3fv6v7iqas8bz5s"))))
    (build-system julia-build-system)
    (home-page "https://github.com/helgee/OptionalData.jl")
    (synopsis "Work with global data that might not be available")
    (description
     "This package provides the @code{@@OptionalData} macro and the corresponding
OptData type which is a thin wrapper around a nullable value (of type @code{Union{T,
Nothing} where T)}.  It allows you to load and access globally available data at
runtime in a type-stable way.")
    (license license:expat)))

(define-public julia-orderedcollections
  (package
    (name "julia-orderedcollections")
    (version "1.4.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaCollections/OrderedCollections.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0jaxcmvkp8zpqrz101yikdigz90s70i7in5wn8kybwzf0na3lhwf"))))
    (build-system julia-build-system)
    (home-page "https://github.com/JuliaCollections/OrderedCollections.jl")
    (synopsis "Associative containers that preserve insertion order")
    (description "This package implements @code{OrderedDicts} and
@code{OrderedSets}, which are similar to containers in base Julia.  However,
during iteration the @code{Ordered*} containers return items in the order in
which they were added to the collection.")
    (license license:expat)))

(define-public julia-offsetarrays
  (package
    (name "julia-offsetarrays")
    (version "1.12.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaArrays/OffsetArrays.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "09cidr42q0xwp6wwyaw09hl580vqi85wb5f78pxrxcfm75yg3xki"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-adapt))
    ;; CatIndices depends on OffsetArrays, introducing a recursive dependency
    (arguments (list #:tests? #f))
    (home-page "https://juliaarrays.github.io/OffsetArrays.jl/stable/")
    (synopsis "Fortran-like arrays with arbitrary, zero or negative indices")
    (description "@code{OffsetArrays.jl} provides Julia users with arrays that
have arbitrary indices, similar to those found in some other programming
languages like Fortran.")
    (license license:expat)))

(define-public julia-paddedviews
  (package
    (name "julia-paddedviews")
    (version "0.5.11")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaArrays/PaddedViews.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1835q06g6ymqh1k7625ssahwm46j08370v2inb61y1lw8vd99f3x"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-offsetarrays))
    (native-inputs
     (list julia-documenter))
    (home-page "https://github.com/JuliaArrays/PaddedViews.jl")
    (synopsis "Add virtual padding to the edges of an array")
    (description "@code{PaddedViews} provides a simple wrapper type,
@code{PaddedView}, to add \"virtual\" padding to any array without copying data.
Edge values not specified by the array are assigned a @code{fillvalue}.
Multiple arrays may be \"promoted\" to have common indices using the
@code{paddedviews} function.")
    (license license:expat)))

(define-public julia-parameters
  (package
    (name "julia-parameters")
    (version "0.12.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/mauro3/Parameters.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0b8lawi7kcws4axfsdf023gyxca15irl648ciyi1kw3wghz3pfi2"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-orderedcollections
           julia-unpack))
    (home-page "https://github.com/mauro3/Parameters.jl")
    (synopsis "Numerical-model parameter helpers")
    (description "This package contains types with default field values, keyword
constructors and (un-)pack macros.  Keyword functions can be slow in Julia,
however, the normal positional constructor is also provided and could be used in
performance critical code.")
    (license license:expat)))

(define-public julia-parsers
  (package
    (name "julia-parsers")
    (version "2.2.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaData/Parsers.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "09v2x9yd1wdp74hzsf6218dpamlf2hb5nkmixqb4bc53ll8hpw4i"))))
    (build-system julia-build-system)
    (home-page "https://github.com/JuliaData/Parsers.jl")
    (synopsis "Fast parsing machinery for basic types in Julia")
    (description "@code{Parsers.jl} is a collection of type parsers and
utilities for Julia.")
    (license license:expat)))

(define-public julia-pdmats
  (package
    (name "julia-pdmats")
    (version "0.11.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaStats/PDMats.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0bc2gmpd30rkclvxyfnssjllp0pk63h0vvgr8862phm5ia83r8j0"))))
    (build-system julia-build-system)
    (home-page "https://github.com/JuliaStats/PDMats.jl")
    (synopsis
     "Uniform Interface for positive definite matrices of various structures")
    (description "PDMats.jl supports efficient computation on positive definite
matrices of various structures.  In particular, it provides uniform interfaces
to use positive definite matrices of various structures for writing generic
algorithms, while ensuring that the most efficient implementation is used in
actual computation.")
    (license license:expat)))

(define-public julia-performancetesttools
  (package
    (name "julia-performancetesttools")
    (version "0.1.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaTesting/PerformanceTestTools.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0v5b8vnnhavra18h0136gahiyl7nc6r3rm3hm359ic3da8yzrnhn"))))
    (build-system julia-build-system)
    (home-page "https://github.com/JuliaTesting/PerformanceTestTools.jl")
    (synopsis "Enables efficient code generation in test cases")
    (description
     "This package enables the Julia compiler to generate
efficient code when running test cases.  Test cases are typically run with
flags that prevent efficient code generation.  This package detects those flags
and instead spawns a separate Julia process without the flags in which to run
the test cases.")
    (license license:expat)))


(define-public julia-plotthemes
  (package
    (name "julia-plotthemes")
    (version "2.0.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaPlots/PlotThemes.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1fd27w9z1vhz0d1bzrs5vcavpb5r5jviyh27d9c4ka37phz4xvmh"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-plotutils
           julia-requires))
    (home-page "https://github.com/JuliaPlots/PlotThemes.jl")
    (synopsis "Themes for the Julia plotting package Plots.jl")
    (description
     "PlotThemes is a package to spice up the plots made with @code{Plots.jl}.")
    (license license:expat)))

(define-public julia-plotutils
  (package
    (name "julia-plotutils")
    (version "1.2.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaPlots/PlotUtils.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1yml9ayaniqnzx5r8sfjckifcm99ck7qhc19cd8fs0bwzkh7nza7"))))
    (build-system julia-build-system)
    (arguments
     (list
       #:phases
       #~(modify-phases %standard-phases
           (add-after 'unpack 'adjust-test-suite
             (lambda _
               (substitute* "test/runtests.jl"
                 (("(@test_throws) ErrorException (.*notacolor)" _ @test notacolor)
                  (string-append @test " ArgumentError " notacolor))))))))
    (propagated-inputs
     (list julia-colors
           julia-colorschemes
           julia-reexport))
    (native-inputs
     (list julia-stablerngs))
    (home-page "https://github.com/JuliaPlots/PlotUtils.jl")
    (synopsis "Helper algorithms for building plotting components")
    (description "This package contains generic helper algorithms for building
plotting components.")
    (license license:expat)))

(define-public julia-polylog
  (package
    (name "julia-polylog")
    (version "2.5.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Expander/PolyLog.jl")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1rfmlh5rvgh84qvvachqgvy0ra5yym28vj09xlw9cq19bkyids64"))))
    (build-system julia-build-system)
    (home-page "https://github.com/Expander/PolyLog.jl")
    (synopsis "Implementation of polylogarithms in Julia")
    (description
     "This package implements real and complex polylogarithms,
including the real and complex dilogarithm and trilogarithm in Julia.")
    (license license:expat)))

(define-public julia-pooledarrays
  (package
    (name "julia-pooledarrays")
    (version "1.4.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaData/PooledArrays.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0g30d46n8cc8vr9icjhfkqz2il2185ijh7xhfy9vhcnmllzpd0yg"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-dataapi))
    (native-inputs
     (list julia-offsetarrays))
    (home-page "https://github.com/JuliaData/PooledArrays.jl")
    (synopsis "Pooled representation of arrays in Julia")
    (description "This package provides a pooled representation of arrays for
purposes of compression when there are few unique elements.")
    (license license:expat)))

(define-public julia-positivefactorizations
  (package
    (name "julia-positivefactorizations")
    (version "0.2.4")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/timholy/PositiveFactorizations.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1wxy6ak7f3hvibcgc8q88cgkf9zvi649mmjy1zlkx1qk80hgvz23"))))
    (build-system julia-build-system)
    (native-inputs
     (list julia-forwarddiff
           julia-reversediff))
    (home-page "https://github.com/timholy/PositiveFactorizations.jl")
    (synopsis "Positive-definite \"approximations\" to matrices")
    (description "@code{PositiveFactorizations} is a package for computing a
positive definite matrix decomposition (factorization) from an arbitrary
symmetric input.  The motivating application is optimization (Newton or
quasi-Newton methods), in which the canonical search direction -H/g (H being the
Hessian and g the gradient) may not be a descent direction if H is not positive
definite.")
    (license license:expat)))

(define-public julia-precompiletools
  (package
    (name "julia-precompiletools")
    (version "1.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaLang/PrecompileTools.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "07b69gmgs3zxs86l9g9dymv3sfgncm8sl86sp0ck6xf5ly10phiy"))))
    (build-system julia-build-system)
    ;; Tests try to download other repositories
    ;; Tests try to install "fake" packages
    (arguments
     (list
      #:tests? #f))
    (propagated-inputs (list julia-preferences))
    (home-page "https://github.com/JuliaLang/PrecompileTools.jl")
    (synopsis "Reduce time-to-first-execution of Julia code")
    (description
     "This package allows you to reduce the latency of the first
execution of Julia code.  It is applicable to both package developers and
end users in their personal workflows.")
    (license license:expat)))

(define-public julia-preferences
  (package
    (name "julia-preferences")
    (version "1.4.3")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaPackaging/Preferences.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "09q5d223ad37qp6vqjm46la9vf8skj30z1ri3qmadq6vdycav7xm"))))
    (build-system julia-build-system)
    (arguments
     (list #:tests? #f))        ; Tests try to mkdir /.julia
    (home-page "https://github.com/JuliaPackaging/Preferences.jl")
    (synopsis "Store configuration switches to TOML files")
    (description "The @code{Preferences} package provides an integrated way for
packages to store configuration switches to persistent TOML files, and use those
pieces of information at both run time and compile time in Julia.  This enables
the user to modify the behavior of a package, and have that choice reflected in
everything from run time algorithm choice to code generation at compile time.")
    (license license:expat)))

(define-public julia-prettytables
  (package
    (name "julia-prettytables")
    (version "2.1.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ronisbr/PrettyTables.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "029niwxgql9rcyx0rxcyhmwkzxciccji4hb59g6752ixam65wxkh"))))
    (build-system julia-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'link-depot 'skip-tests-manipulating-terminal-display
            (lambda _
              (substitute* "test/text_backend.jl"
                ((".*colors\\.jl.*") "")
                ((".*custom_cells\\.jl.*") ""))
              (substitute* "test/general.jl"
                ((".*string\\.jl.*") ""))
              (substitute* "test/text_backend/issues.jl"
                (("testset.*161.*begin" all)
                 (string-append all " return"))))))))
    (propagated-inputs
     (list julia-crayons
           julia-formatting
           julia-offsetarrays
           julia-reexport
           julia-stringmanipulation
           julia-tables))
    (home-page "https://github.com/ronisbr/PrettyTables.jl")
    (synopsis "Print data in formatted tables")
    (description "This package has the purpose to print data in matrices in a
human-readable format.")
    (license license:expat)))

(define-public julia-progressbars
  (package
    (name "julia-progressbars")
    (version "1.5.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cloud-oak/ProgressBars.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0y4i2kdir0wxr3amafidr3585w8lj4s0cqfcbl690a8jvw6rs3jw"))))
    (build-system julia-build-system)
    (home-page "https://github.com/cloud-oak/ProgressBars.jl")
    (synopsis "Progress bar for Julia")
    (description
     "This package provides a fast, extensible progress bar for Julia.
This can help users track the progress of long-running tasks.")
    (license license:mpl2.0)))

(define-public julia-pycall
  (package
    (name "julia-pycall")
    (version "1.92.5")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaPy/PyCall.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1fj5d1ihnhnm0pl4hbx6hcd2bpdyhm8jiaqah2axsbd069j70saf"))))
    (build-system julia-build-system)
    (arguments
     (list
      #:imported-modules `((guix build python-build-system)
                           ,@%julia-build-system-modules)
      #:modules '((guix build julia-build-system)
                  (guix build utils)
                  ((guix build python-build-system) #:prefix python:))
      #:phases
      #~(modify-phases %standard-phases
        (add-after 'link-depot 'remove-conda
          (lambda _
            (substitute* "Project.toml"
              ((".*Conda.*") ""))
            (substitute* (list "src/PyCall.jl"
                               "test/runtests.jl")
              (("import Conda") ""))
            (substitute* "deps/depsutils.jl"
              (("Conda.PYTHONDIR") "\"/\""))))
        (add-after 'link-depot 'set-python
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (let ((python (assoc-ref inputs "python")))
              (setenv "PYCALL_JL_RUNTIME_PYTHON"
                      (string-append python "/bin/python3"))
              (with-output-to-file "deps/deps.jl"
                (lambda _
                  (format #t
                          "const python = \"~a/bin/python3\"~@
                           const pyprogramname = \"~a/bin/python3\"~@
                           const libpython = \"~a/lib/libpython~a.so.1.0\"~@
                           const PYTHONHOME = \"~a\"~@
                           const pyversion_build = v\"~a\"~@
                           const conda = false~%"
                          python
                          python
                          python
                          (python:python-version python)
                          python
                          #$(package-version python)))))))
        (add-before 'check 'pre-check
          (lambda _
            (setenv "CI" "true")
            (setenv "JULIA_PKGEVAL" "true"))))))
    (propagated-inputs
     (list julia-macrotools
           julia-versionparsing))
    (inputs
     (list python))
    (native-inputs
     (list python-numpy))
    (home-page "https://github.com/JuliaPy/PyCall.jl")
    (synopsis "Call Python functions from the Julia language")
    (description "This package provides the ability to directly call and fully
interoperate with Python from the Julia language.  You can import arbitrary
Python modules from Julia, call Python functions (with automatic conversion of
types between Julia and Python), define Python classes from Julia methods, and
share large data structures between Julia and Python without copying them.")
    (license license:expat)))

(define-public julia-pyplot
  (package
    (name "julia-pyplot")
    (version "2.10.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaPy/PyPlot.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "007zs0imfgs69f30pp2a3rc93kl0qiq7qjx6ig35z4wzkmps4skd"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-colors
           julia-latexstrings
           julia-pycall
           julia-versionparsing
           ;; python-matplotlib is expected to be available at runtime.
           python-matplotlib))
    (home-page "https://github.com/JuliaPy/PyPlot.jl")
    (synopsis "Plotting for Julia based on matplotlib.pyplot")
    (description "This package provides a Julia interface to the Matplotlib
plotting library from Python, and specifically to the @code{matplotlib.pyplot}
module.  PyPlot uses the Julia PyCall package to call Matplotlib directly from
Julia with little or no overhead (arrays are passed without making a copy).")
    (license license:expat)))

(define-public julia-quadgk
  (package
    (name "julia-quadgk")
    (version "2.5.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaMath/QuadGK.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0f14dhn0f7ln2j96qvmnsyy9ffzqsngd16ikc136snlxv4k4whiv"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-datastructures))
    (home-page "https://github.com/JuliaMath/QuadGK.jl")
    (synopsis "Adaptive 1d numerical Gauss–Kronrod integration")
    (description "This package provides support for one-dimensional numerical
integration in Julia using adaptive Gauss-Kronrod quadrature.  The code was
originally part of Base Julia.  It supports integration of arbitrary numeric
types, including arbitrary precision (@code{BigFloat}), and even integration of
arbitrary normed vector spaces (e.g. matrix-valued integrands).")
    (license license:expat)))

(define-public julia-quadmath
  (package
    (name "julia-quadmath")
    (version "0.5.5")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaMath/Quadmath.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "051biw4b9zni7cmh2f1yzifp1v8wazlfxrdz4p44lyd1wba6379w"))))
    (build-system julia-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'link-depot 'hardcode-libmath-location
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((gcclib (assoc-ref inputs "gcc:lib")))
                (substitute* "src/Quadmath.jl"
                  (("libgcc_s.so.1" lib) (string-append gcclib "/lib/" lib))
                  (("libquadmath.so.0" lib) (string-append gcclib "/lib/" lib)))))))))
    (propagated-inputs
     (list julia-requires))
    (inputs
     `(("gcc:lib" ,gcc "lib")))
    (native-inputs
     (list julia-specialfunctions))
    (home-page "https://github.com/JuliaMath/Quadmath.jl")
    (synopsis "Float128 and libquadmath for the Julia language")
    (description "This is a Julia interface to @code{libquadmath}, providing a
@code{Float128} type corresponding to the IEEE754 binary128 floating point
format.")
    (license license:expat)))

(define-public julia-quaternions
  (package
    (name "julia-quaternions")
    (version "0.4.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaGeometry/Quaternions.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1zhynyvchc50hywws2jznpkwydr3njh8cv84d2ylyabhcwwmil9s"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-dualnumbers))
    (home-page "https://github.com/JuliaGeometry/Quaternions.jl")
    (synopsis "Quaternion and dual-quaternion functionality")
    (description "Quaternions are best known for their suitability as
representations of 3D rotational orientation.  They can also be viewed as an
extension of complex numbers.")
    (license license:expat)))

(define-public julia-queryoperators
  (package
    (name "julia-queryoperators")
    (version "0.9.3")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/queryverse/QueryOperators.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "06zm4cbn3x49lbpgshhdfvvmgz066qkc8q0d57igm5p8bcp6js22"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-datastructures
           julia-iteratorinterfaceextensions
           julia-tableshowutils))
    (home-page "https://github.com/queryverse/QueryOperators.jl")
    (synopsis "Query operators for Julia")
    (description "This package contains the underlying query operators that are
exposed to users in @code{Query.jl}.")
    (license license:expat)))

(define-public julia-rangearrays
  (package
    (name "julia-rangearrays")
    (version "0.3.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaArrays/RangeArrays.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1ihzfyfq1xihkjcvn7xmzfbn6igzidb4fkzdcxwfr5qkvi52gnmg"))))
    (build-system julia-build-system)
    (home-page "https://github.com/JuliaArrays/RangeArrays.jl")
    (synopsis "Array data structures with autogenerated columns")
    (description "The goal of RangeArrays is to provide efficient and convenient
array data structures where the columns of the arrays are generated (on the fly)
by Ranges.")
    (license license:expat)))

(define-public julia-ratios
  (package
    (name "julia-ratios")
    (version "0.4.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/timholy/Ratios.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1a4fd4jq4qjply29rkwg3m1clfndjsbckj1b1dab1bc35h2c6yxh"))))
    (build-system julia-build-system)
    (home-page "https://github.com/timholy/Ratios.jl")
    (synopsis "Faster Rational-like types for Julia")
    (description "This package provides types similar to Julia's @code{Rational}
type, which make some sacrifices but have better computational performance.")
    (license license:expat)))

(define-public julia-realdot
  (package
    (name "julia-realdot")
    (version "0.1.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaMath/RealDot.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1jr8dq110j8axjfz936b1lqqcnqg3979rfg11w76rq1iz7zgi691"))))
    (build-system julia-build-system)
    (home-page "https://github.com/JuliaMath/RealDot.jl")
    (synopsis "Compute realdot efficiently")
    (description "This package only contains and exports a single function
@code{realdot(x, y)}.  It computes @code{real(LinearAlgebra.dot(x, y))} while
avoiding computing the imaginary part of @code{LinearAlgebra.dot(x, y)} if
possible.  The real dot product is useful when one treats complex numbers as
embedded in a real vector space.")
    (license license:expat)))

(define-public julia-recipesbase
  (package
    (name "julia-recipesbase")
    (version "1.2.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaPlots/RecipesBase.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0icyn56b17bqlxqkc3h44ndn0f1g2g9wy2kjvl8b6pfqni4ybazm"))))
    (build-system julia-build-system)
    (home-page "https://github.com/JuliaPlots/RecipesBase.jl")
    (synopsis "Define transformation recipes on user types")
    (description "This package implements handy macros @code{@@recipe} and
@code{@@series} which will define a custom transformation and attach attributes
for user types.  Its design is an attempt to simplify and generalize the summary
and display of types and data from external packages.  With this package it is
possible to describe visualization routines that can be used as components in
more complex visualizations.")
    (license license:expat)))

(define-public julia-recipespipeline
  (package
    (name "julia-recipespipeline")
    (version "0.6.6")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaPlots/RecipesPipeline.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1lsjnlkmhcxngrcszfwmzb7hqg8bczi00mn7kbmwp5ffqb7fh0vv"))))
    (build-system julia-build-system)
    (arguments
     (list #:tests? #f))    ; Cycle with Plots.jl.
    (propagated-inputs
     (list julia-nanmath
           julia-plotutils
           julia-recipesbase))
    (home-page "https://juliaplots.org/RecipesPipeline.jl/dev/")
    (synopsis "Utilities for processing recipes")
    (description "This package was factored out of @code{Plots.jl} to allow any
other plotting package to use the recipe pipeline.  In short, the extremely
lightweight @code{RecipesBase.jl} package can be depended on by any package to
define \"recipes\": plot specifications of user-defined types, as well as custom
plot types.  @code{RecipePipeline.jl} contains the machinery to translate these
recipes to full specifications for a plot.")
    (license license:expat)))

(define-public julia-recursivearraytools
  (package
    (name "julia-recursivearraytools")
    (version "2.16.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/SciML/RecursiveArrayTools.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0vx8ndxrii53na7jsc2lki47wfpi77rn3y2r6xhiysx1qwr14msf"))))
    (build-system julia-build-system)
    (arguments
     (list #:tests? #f))    ; Cycle with OrdinaryDiffEq.jl.
    (propagated-inputs
     (list julia-arrayinterface
           julia-chainrulescore
           julia-docstringextensions
           julia-recipesbase
           julia-requires
           julia-staticarrays
           julia-zygoterules))
    ;(native-inputs
    ; `(("julia-forwarddiff" ,julia-forwarddiff)
    ;   ("julia-nlsolve" ,julia-nlsolve)
    ;   ("julia-ordinarydiffeq" ,julia-ordinarydiffeq)
    ;   ("julia-structarrays" ,julia-structarrays)
    ;   ("julia-zygote" ,julia-zygote)))
    (home-page "https://github.com/SciML/RecursiveArrayTools.jl")
    (synopsis "Tools for handling objects like arrays of arrays and deeper nestings")
    (description "@code{RecursiveArrayTools.jl} is a set of tools for dealing with
recursive arrays like arrays of arrays.")
    (license license:expat)))

(define-public julia-reexport
  (package
    (name "julia-reexport")
    (version "1.2.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/simonster/Reexport.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0inf5q6f01ncd5c5wm8mwzv2hs627ng6xlh8dhrlflp321hbamwf"))))
    (build-system julia-build-system)
    (home-page "https://github.com/simonster/Reexport.jl")
    (synopsis "Re-export modules and symbols")
    (description "This package provides tools to re-export modules and symbols.")
    (license license:expat)))

(define-public julia-remotefiles
  (package
    (name "julia-remotefiles")
    (version "0.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/helgee/RemoteFiles.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1zpklzpd4ckp7s4wbf93qmq3dyyrx4pzl41x5i9zbiskadhniqnh"))))
    (build-system julia-build-system)
    (arguments
     '(#:tests? #f)) ; Tests try to download from Internet.
    (propagated-inputs
     (list julia-fileio julia-http))
    (home-page "https://github.com/helgee/RemoteFiles.jl")
    (synopsis "Download files from the Internet and keep them up-to-date")
    (description
     "This package provides a functionality of files download with cURL, wget or
@code{HTTP.jl} backends.")
    (license license:expat)))

(define-public julia-referenceables
  (package
    (name "julia-referenceables")
    (version "0.1.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaFolds2/Referenceables.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1qjssgqrpyc726zm31r4fpllsyvgc21lw9kqz96whgg779y0yr80"))))
    (build-system julia-build-system)
    (propagated-inputs (list julia-adapt))
    (home-page "https://github.com/JuliaFolds2/Referenceables.jl")
    (synopsis "Provides an interface for referencing elements")
    (description "This package provides an interface for readable and writable
references to an element of an array or dictionary in Julia.")
    (license license:expat)))

(define-public julia-referencetests
  (package
    (name "julia-referencetests")
    (version "0.9.7")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaTesting/ReferenceTests.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0mm6bjhs8a21pippww6b08b5frmnb9m6k8xrszrwq9zhc879zpc9"))))
    (build-system julia-build-system)
    (arguments
     (list #:tests? #f))    ; Cycle with ImageCore.jl through ImageMagick.jl.
    (propagated-inputs
     (list julia-deepdiffs
           julia-distances
           julia-fileio
           julia-imagecore
           julia-imageinterminal))
    ;(native-inputs
    ; `(("julia-csvfiles" ,julia-csvfiles)
    ;   ("julia-dataframes" ,julia-dataframes)
    ;   ("julia-gr" ,julia-gr)
    ;   ("julia-imagemagick" ,julia-imagemagick)
    ;   ("julia-imagetransformations" ,julia-imagetransformations)
    ;   ("julia-plots" ,julia-plots)
    ;   ("julia-testimages" ,julia-testimages)))
    (home-page "https://juliatesting.github.io/ReferenceTests.jl/latest/")
    (synopsis "Utility package for comparing data against reference files")
    (description "@code{ReferenceTests.jl} is a Julia package that adds a couple
of additional macros to your testing toolbox.  In particular, it focuses on
functionality for testing values against reference files, which in turn the
package can help create and update if need be.")
    (license license:expat)))

(define-public julia-requires
  (package
    (name "julia-requires")
    (version "1.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaPackaging/Requires.jl/")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0gmqs7f17aq500lbdff4ibws00f8m0pnzskvf4b3ig520xv3n3nm"))))
    (build-system julia-build-system)
    (arguments
     (list #:parallel-tests? #f))       ; Test suite has race conditions.
    (native-inputs
     (list julia-colors
           julia-example))
    (home-page "https://github.com/JuliaPackaging/Requires.jl/")
    (synopsis "Faster package loader")
    (description "This package make loading packages faster, maybe.  It
supports specifying glue code in packages which will load automatically when
another package is loaded, so that explicit dependencies (and long load times)
can be avoided.")
    (license license:expat)))

(define-public julia-reversediff
  (package
    (name "julia-reversediff")
    (version "1.14.4")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaDiff/ReverseDiff.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0hwsjmr4wiscqa5kaj4mw0i4agyyzdcmq4r1gp2i563nc1ziaylg"))))
    (build-system julia-build-system)
    (arguments
     ;; XXXX: Test suite failing for i686, e.g.,
     ;; Expression: hash(tr_float, hash(1)) === hash(v_float, hash(1))
     ;; MethodError: no method matching decompose(::ReverseDiff.TrackedReal{Float64, Float64, Nothing})
     ;; Disable as stopgap.
     (list #:tests? (not (or (%current-target-system)
                             (target-x86-32?)))))
    (propagated-inputs
     (list julia-chainrulescore
           julia-diffresults
           julia-diffrules
           julia-difftests
           julia-forwarddiff
           julia-functionwrappers
           julia-logexpfunctions
           julia-macrotools
           julia-nanmath
           julia-specialfunctions
           julia-staticarrays))
    (native-inputs
     (list julia-difftests
           julia-fillarrays))
    (home-page "https://github.com/JuliaDiff/ReverseDiff.jl")
    (synopsis "Reverse Mode Automatic Differentiation for Julia")
    (description "@code{ReverseDiff.jl} is a fast and compile-able tape-based
reverse mode @acronym{AD, automatic differentiation}, that implements methods to
take gradients, Jacobians, Hessians, and higher-order derivatives of native
Julia functions (or any callable object, really).")
    (license license:expat)))

(define-public julia-richardson
  (package
    (name "julia-richardson")
    (version "1.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaMath/Richardson.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "06v9ii3d7hh41fsrfklaa8ap55z5s017f888mrd1c18y4fx9i4nx"))))
    (build-system julia-build-system)
    (home-page "https://juliapackages.com/p/richardson")
    (synopsis "Extrapolate function using Richardson method")
    (description "This package provides a function extrapolate that
extrapolates a given function @code{f(x)} to @code{f(x0)}, evaluating @code{f}
only at a geometric sequence of points @code{> x0} (or optionally @code{<
x0}).  The key algorithm is Richardson extrapolation using a Neville–Aitken
tableau, which adaptively increases the degree of an extrapolation polynomial
until convergence is achieved to a desired tolerance (or convergence stalls
due to e.g. floating-point errors).  This allows one to obtain @code{f(x0)} to
high-order accuracy, assuming that @code{f(x0+h)} has a Taylor series or some
other power series in @code{h}.")
    (license license:expat)))

(define-public julia-rotations
  ;; This is the first commit with support for julia-1.6.
  (let ((commit "b599102535bc3534252c76f3fd4cf521f4741788")
        (revision "1"))
    (package
      (name "julia-rotations")
      (version (git-version "1.0.2" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/JuliaGeometry/Rotations.jl")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32 "01wwqbdmj61yiz7rkmams4xg7gb9hbmg325173846ky4b9x1wb19"))))
      (build-system julia-build-system)
      (propagated-inputs
       (list julia-staticarrays))
      (native-inputs
       (list julia-benchmarktools
             julia-forwarddiff
             julia-unitful))
      (home-page "https://github.com/JuliaGeometry/Rotations.jl")
      (synopsis "Julia implementations for different rotation parameterisations")
      (description "This package implements various 3D rotation parameterizations
and defines conversions between them.  At their heart, each rotation
parameterization is a 3×3 unitary (orthogonal) matrix (based on the
@code{StaticArrays.jl} package), and acts to rotate a 3-vector about the origin
through matrix-vector multiplication.")
      (license license:expat))))

(define-public julia-safetestsets
  ;; The only release tag is the first commit in the repository.
  (let ((commit "e553edc4c753344d38349304b9ff5483c3b8ff21")
        (revision "1"))
    (package
      (name "julia-safetestsets")
      (version (git-version "0.0.1" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/YingboMa/SafeTestsets.jl")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32 "1fb1dfdmiw2ggx60hf70954xlps0r48fcb3k3dvxynlz7ylphp96"))))
      (build-system julia-build-system)
      (arguments
       (list
        #:julia-package-name "SafeTestsets"
        #:julia-package-uuid "1bc83da4-3b8d-516f-aca4-4fe02f6d838f"
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'link-depot 'fix-package-toml
              (lambda _
                (substitute* "Project.toml"
                  (("version = .*") "version = \"0.0.1\"\n")))))))
      (native-inputs
       (list julia-staticarrays))
      (home-page "https://github.com/YingboMa/SafeTestsets.jl")
      (synopsis "Julia's testset in a module")
      (description "This package contains the testset from Julia, packaged into
a loadable module.")
      (license license:expat))))

(define-public julia-sass
  (package
    (name "julia-sass")
    (version "0.2.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/piever/Sass.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0y7kkkj717h5cj659ssry89i5r64symr6pvhr6vv4qmaxrnjxj92"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-libsass-jll))
    (home-page "https://github.com/piever/Sass.jl")
    (synopsis "Compile scss and sass file to css")
    (description "This package provides a simple Julian API to use the
@code{libsass} library to compile scss and sass files to css.")
    (license license:expat)))

(define-public julia-scanbyte
  (package
    (name "julia-scanbyte")
    (version "0.4.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/jakobnissen/ScanByte.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1ww7bbh02s4l917dwkzg9pq71xk0db2rba247vz1xfm24msi8lwj"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-simd))
    (home-page "https://github.com/jakobnissen/ScanByte.jl")
    (synopsis "Find the first occurrence of set of bytes in a chunk of memory")
    (description "This package finds the first occurrence of a byte or set of
bytes in a chunk of memory.  Think of it like a much faster version of
@code{findfirst} that only iterates over bytes in memory.")
    ;; https://github.com/jakobnissen/ScanByte.jl/issues/2
    (supported-systems '("x86_64-linux"))
    (license license:expat)))

(define-public julia-scientifictypesbase
  (package
    (name "julia-scientifictypesbase")
    (version "3.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaAI/ScientificTypesBase.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1gkkyvighbwplbv3mcxdrall19zjsb9cwp0w2mr26sl6xbjxr8xc"))))
    (build-system julia-build-system)
    (native-inputs (list julia-tables))
    (home-page "https://github.com/JuliaAI/ScientificTypesBase.jl")
    (synopsis
     "Base interface for dispatching on the 'scientific' type of data")
    (description
     "This package provides a Julia interface defining a collection
of types (without instances) for implementing conventions about the scientific
interpretation of data.  This package makes a distinction between the machine
type and the scientific type of a Julia object.  A machine type refers to the
Julia type being used to represent the object, for instance @code{Float64}.  The
scientific type refers to how the object should be interpreted, for instance
@code{Continuous} or @code{Multiclass{3}}.")
    (license license:expat)))

(define-public julia-scratch
  (package
    (name "julia-scratch")
    (version "1.1.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaPackaging/Scratch.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "09xni9rrrax17fxjz04j1b48mk9ffww5rcbagh66jklr89mrkqhx"))))
    (build-system julia-build-system)
    (arguments
     (list #:tests? #f)) ; Test suite tries to access the Julia package registry.
    (home-page "https://github.com/JuliaPackaging/Scratch.jl")
    (synopsis "Scratch spaces for all your persistent mutable data needs")
    (description "This repository implements the scratch spaces API for
package-specific mutable containers of data.  These spaces can contain datasets,
text, binaries, or any other kind of data that would be convenient to store in
a location specific to your package.  As compared to Artifacts, these containers
of data are mutable.  Because the scratch space location on disk is not very
user-friendly, scratch spaces should, in general, not be used for a storing
files that the user must interact with through a file browser.")
    (license license:expat)))

(define-public julia-sentinelarrays
  (package
    (name "julia-sentinelarrays")
    (version "1.3.13")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaData/SentinelArrays.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1h3vpz7xskbf2a60imdg0irwh9bybkahjpnb6b3wyk0j9a97nqkr"))))
    (build-system julia-build-system)
    (home-page "https://github.com/JuliaData/SentinelArrays.jl")
    (synopsis "Array types using sentinel values")
    (description "This package provides @code{SentinelArray{T}} that wraps an
@code{AbstractArray} of type @code{T}, and accepts a sentinel and value
argument.")
    (license license:expat)))

(define-public julia-shiftedarrays
  (package
    (name "julia-shiftedarrays")
    (version "1.0.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaArrays/ShiftedArrays.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0wy7k29qx2lvj587kiz31fzdc60808mjsgpp41h6b682ypz8rw0c"))))
    (build-system julia-build-system)
    (home-page "https://github.com/JuliaArrays/ShiftedArrays.jl")
    (synopsis "Lazy shifted arrays for data analysis in Julia")
    (description
     "This package provides an implementation of shifted arrays for Julia.")
    (license license:expat)))

(define-public julia-showoff
  (package
    (name "julia-showoff")
    (version "1.0.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaGraphics/Showoff.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1g4hqvjjpwbrs7fnllyl5w66yj6qlvpvzpygym2nvf01m1ps6m53"))))
    (build-system julia-build-system)
    (home-page "https://github.com/JuliaGraphics/Showoff.jl")
    (synopsis "Nicely format an array of n things for tables and plots")
    (description "@code{Showoff} provides an interface for consistently
formatting an array of n things, e.g. numbers, dates, unitful values.  It's used
in @code{Gadfly}, @code{Plots} and @code{Makie} to label axes and keys.")
    (license license:expat)))

(define-public julia-simd
  (package
    (name "julia-simd")
    (version "3.4.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/eschnett/SIMD.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "02pbrg2qa20pqnckbnbg5jyic2ahydql09f3xhzd1xnxicp77lw5"))))
    (build-system julia-build-system)
    (home-page "https://github.com/eschnett/SIMD.jl")
    (synopsis "Explicit SIMD vectorization")
    (description "This package allows programmers to explicitly SIMD-vectorize
their Julia code.  By exposing SIMD vector types and corresponding operations,
the programmer can explicitly vectorize their code.  While this does not
guarantee that the generated machine code is efficient, it relieves the
compiler from determining whether it is legal to vectorize the code, deciding
whether it is beneficial to do so, and rearranging the code to synthesize
vector instructions.")
    (license license:expat)))

(define-public julia-simdtypes
  (package
    (name "julia-simdtypes")
    (version "0.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaSIMD/SIMDTypes.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0qkg0rwfq0q746j2k5wg3dvrcmxm3lfxw0mxqrqdxccnjnmgcbkr"))))
    (build-system julia-build-system)
    (home-page "https://github.com/JuliaSIMD/SIMDTypes.jl")
    (synopsis "SIMD type declarations")
    (description "This minimalistic package serves as the foundation for
other SIMD packages in Julia.")
    (license license:expat)))

(define-public julia-simpletraits
  (package
    (name "julia-simpletraits")
    (version "0.9.4")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/mauro3/SimpleTraits.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1qvmkqcvhc2nilvkk36szccxdlcv9ls2i0ksxgl2yfjr3b3qlr05"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-macrotools))
    (home-page "https://github.com/mauro3/SimpleTraits.jl")
    (synopsis "Simple Traits for Julia")
    (description "This package provides a macro-based implementation of traits.
The main idea behind traits is to group types outside the type-hierarchy and to
make dispatch work with that grouping.  The difference to Union-types is that
types can be added to a trait after the creation of the trait, whereas Union
types are fixed after creation.")
    (license license:expat)))

(define-public julia-simpletropical
  (package
    (name "julia-simpletropical")
    (version "0.3.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/scheinerman/SimpleTropical.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1rdjg7bdayksdklv3h17cbdjj9gs5krbvmn8ri6jw10rgx5ca961"))))
    (build-system julia-build-system)
    (home-page "https://github.com/scheinerman/SimpleTropical.jl")
    (synopsis "Julia implementation of tropical arithmetic")
    (description "This package is an implementation of tropical (min-plus)
arithmetic in Julia.")
    (license license:expat)))

(define-public julia-softglobalscope
  (package
    (name "julia-softglobalscope")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/stevengj/SoftGlobalScope.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1n3l0al1vw5jpb4k9a29a71666cdb617nmiqg34wcmyfzrxpvv39"))))
    (build-system julia-build-system)
    (home-page "https://github.com/stevengj/SoftGlobalScope.jl")
    (synopsis "Utilities for soft global scope in interactive Julia environments")
    (description
     "SoftGlobalScope is a package for the Julia language that simplifies the
variable scoping rules for code in global scope.  It is intended for interactive
shells to make it easier to work interactively with Julia, especially for
beginners.")
    (license license:expat)))

(define-public julia-sortingalgorithms
  (package
    (name "julia-sortingalgorithms")
    (version "1.0.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaCollections/SortingAlgorithms.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "173x77a80xnh99viqa3r7rgdaksvxaw8xyfqw09gwvp4p2zrxivb"))))
    (build-system julia-build-system)
    (arguments
     (list #:tests? #f))    ; cycle with StatsBase.jl
    (propagated-inputs
     (list julia-datastructures))
    ;(native-inputs
    ; `(("julia-statsbase" ,julia-statsbase)))
    (home-page "https://github.com/JuliaCollections/SortingAlgorithms.jl")
    (synopsis "Extra sorting algorithms extending Julia's sorting API")
    (description "The SortingAlgorithms package provides three sorting
algorithms that can be used with Julia's standard sorting API: heapsort,
timsort and radixsort.")
    (license license:expat)))

(define-public julia-specialfunctions
  (package
    (name "julia-specialfunctions")
    (version "1.8.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaMath/SpecialFunctions.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0shlgx9lkbjb1awdf5lrbkq06bmkyahc92qay2a049b4lvqrhj7a"))))
    (build-system julia-build-system)
    (inputs
     (list julia-chainrulestestutils))
    (propagated-inputs
     (list julia-chainrulescore
           julia-irrationalconstants
           julia-logexpfunctions
           julia-openspecfun-jll))
    (home-page "https://github.com/JuliaMath/SpecialFunctions.jl")
    (synopsis "Special mathematical functions")
    (description "This package provides special mathematical functions,
including Bessel, Hankel, Airy, error, Dawson, exponential (or sine and
cosine) integrals, eta, zeta, digamma, inverse digamma, trigamma, and
polygamma functions.")
    (license license:expat)))

(define-public julia-splitapplycombine
  (package
    (name "julia-splitapplycombine")
    (version "1.1.4")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaData/SplitApplyCombine.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1qzaqvk57b0s5krzn8bxkzmr5kz6hi9dm3jbf2sl7z4vznsgbn9x"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-dictionaries
           julia-indexing))
    (home-page "https://github.com/JuliaData/SplitApplyCombine.jl")
    (synopsis "Split-apply-combine strategies for Julia")
    (description "@code{SplitApplyCombine.jl} provides high-level, generic tools
for manipulating data - particularly focussing on data in nested containers.  An
emphasis is placed on ensuring split-apply-combine strategies are easy to apply,
and work reliably for arbitrary iterables and in an optimized way with the data
structures included in Julia's standard library.")
    (license license:expat)))

(define-public julia-stablerngs
  (package
    (name "julia-stablerngs")
    (version "1.0.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaRandom/StableRNGs.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1cw4wc38qbgmrrx0jjwjhynnarrzjkh0yyz242zj272brbci7p1r"))))
    (build-system julia-build-system)
    (home-page "https://github.com/JuliaRandom/StableRNGs.jl")
    (synopsis "Julia RNG with stable streams")
    (description "This package intends to provide a simple RNG with stable
streams, suitable for tests in packages which need reproducible streams of
random numbers across Julia versions.  Indeed, the Julia RNGs provided by
default are documented to have non-stable streams (which for example enables
some performance improvements).")
    (license license:expat)))

(define-public julia-stackviews
  (package
    (name "julia-stackviews")
    (version "0.1.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaArrays/StackViews.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1fwiaxdpx1z9dli3jr8kyraych0jbdiny3qklynf0r13px25r6i7"))))
    (build-system julia-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'link-depot 'skip-doctest
            (lambda _
              (substitute* "test/runtests.jl"
                ((".*doctest.*") "")))))))
    (propagated-inputs
     (list julia-offsetarrays))
    (native-inputs
    (list julia-aqua
          julia-documenter))
    (home-page "https://github.com/JuliaArrays/StackViews.jl")
    (synopsis "No more catcat")
    (description "StackViews provides only one array type: @code{StackView}.
There are multiple ways to understand @code{StackView}:
@itemize
@item inverse of @code{eachslice}
@item @code{cat} variant
@item view object
@item lazy version of @code{repeat} special case
@end itemize")
    (license license:expat)))

(define-public julia-static
  (package
    (name "julia-static")
    (version "0.8.3")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/SciML/Static.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1ilmging187w37vjff8ilnz1f0qygyhbwl6nhq91z3b5vxyf13zr"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-ifelse))
    (native-inputs
     (list julia-aqua))
    (home-page "https://github.com/SciML/Static.jl")
    (synopsis "Static types useful for dispatch and generated functions")
    (description "Static.jl defines a limited set of statically parameterized
types and a common interface that is shared between them.")
    (license license:expat)))

(define-public julia-static-0.6
  (package
    (inherit julia-static)
    (name "julia-static")
    (version "0.6.6")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/SciML/Static.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "03ri8nl046cz7l433p0nlr84yywxvjykyymqparm8lxxwkv0rxqd"))))))

(define-public julia-staticarrays
  (package
    (name "julia-staticarrays")
    (version "1.2.13")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaArrays/StaticArrays.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "041mijzgzm8r61a3gwspr2wcxjikqksvj2rbnl4gmcy41alqmx79"))))
    (build-system julia-build-system)
    (inputs
     (list julia-benchmarktools))
    (home-page "https://github.com/JuliaArrays/StaticArrays.jl")
    (synopsis "Statically sized arrays")
    (description "This package provides a framework for implementing
statically sized arrays in Julia, using the abstract type
@code{StaticArray{Size,T,N} <: AbstractArray{T,N}}.  Subtypes of
@code{StaticArray} will provide fast implementations of common array and
linear algebra operations.")
    (license license:expat)))

(define-public julia-staticarrayscore
  (package
    (name "julia-staticarrayscore")
    (version "1.4.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaArrays/StaticArraysCore.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0dvi9c4abjzvdn6lyr6adpc8qf4432rg3p5z96a3rc3nlsvfns9y"))))
    (build-system julia-build-system)
    (home-page "https://github.com/JuliaArrays/StaticArraysCore.jl")
    (synopsis "Common types and functions for static arrays")
    (description
     "This package provides definitions for most of the
primary types and functions in @code{StaticArrays.jl}.  This enables
downstream packages to implement new methods on these types without
depending on the entirety of @code{StaticArrays.jl}.")
    (license license:expat)))

(define-public julia-staticnumbers
  (package
    (name "julia-staticnumbers")
    (version "0.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/perrutquist/StaticNumbers.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1m6drdyxgizc6d5qak9l6c2dv8nb6x5kj7sgfxkgwnfxf6ran257"))))
    (build-system julia-build-system)
    (native-inputs (list julia-simd julia-staticarrays))
    (home-page "https://github.com/perrutquist/StaticNumbers.jl")
    (synopsis "Static numbers in Julia")
    (description
     "This package provides number datatypes which store their values in
 type parameters, making them runtime constants.")
    (license license:expat)))

(define-public julia-statisticaltraits
  (package
    (name "julia-statisticaltraits")
    (version "3.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaAI/StatisticalTraits.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1im2j3h8xlja8c4pz22xn4lgb2r7zx50284iwbl72sclhrmjzfkz"))))
    (build-system julia-build-system)
    (arguments
     (list
      #:julia-package-name "StatisticalTraits"
      #:julia-package-uuid "64bff920-2084-43da-a3e6-9bb72801c0c9"
      #:julia-package-dependencies
      #~(list '("SparseArrays" . "2f01184e-e22b-5df5-ae63-d93ebab69eaf")
              '("Test" . "8dfed614-e22c-5e08-85e1-65c5234f0b40"))))
    (propagated-inputs (list julia-scientifictypesbase))
    (home-page "https://github.com/JuliaAI/StatisticalTraits.jl")
    (synopsis "Implementations of traits possessed by statistical objects")
    (description
     "This package provides fall-back implementations for a collection
of traits possessed by statistical objects.  A @code{trait} is a function with a single
arguments that is a Julia type, which might encode type metadata for inspection or
for use in function dispatch.")
    (license license:expat)))

(define-public julia-statsapi
  (package
    (name "julia-statsapi")
    (version "1.0.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaStats/StatsAPI.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1k1c3s7i5wzkz4r9fyy4gd7wb97p0qgbc7bmaajm16zqipfmy2bv"))))
    (build-system julia-build-system)
    (home-page "https://juliastats.org/")
    (synopsis "Statistics-focused namespace for packages to share functions")
    (description "This package provides a namespace for data-related generic
function definitions to solve the optional dependency problem; packages wishing
to share and/or extend functions can avoid depending directly on each other by
moving the function definition to @code{StatsAPI.jl} and each package taking a
dependency on it.")
    (license license:expat)))

(define-public julia-statsbase
  (package
    (name "julia-statsbase")
    (version "0.33.10")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaStats/StatsBase.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0pjsn531zdz3s34pa418pvyqvrx8nbcc8j0fgwfnadssihqah6g7"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-dataapi
           julia-datastructures
           julia-missings
           julia-sortingalgorithms
           julia-statsapi))
    (native-inputs
     (list julia-stablerngs))
    (home-page "https://github.com/JuliaStats/StatsBase.jl")
    (synopsis "Basic statistics for Julia")
    (description "StatsBase.jl is a Julia package that provides basic support
for statistics.  Particularly, it implements a variety of statistics-related
functions, such as scalar statistics, high-order moment computation, counting,
ranking, covariances, sampling, and empirical density estimation.")
    (license license:expat)))

(define-public julia-stringdistances
  (package
    (name "julia-stringdistances")
    (version "0.10.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/matthieugomez/StringDistances.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0n5707wplbibzhhv1xmshvc025d7xk6mnikclc3hvq5cdc0gy9f7"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-distances))
    (home-page "https://github.com/matthieugomez/StringDistances.jl")
    (synopsis "String Distances in Julia")
    (description "This package provides string distances in Julia.  Distances
are defined for @code{AbstractStrings}, and any iterator that define
@code{length()}.  The package also defines Distance \"modifiers\" that can be
applied to any distance.")
    (license license:expat)))

(define-public julia-stringencodings
  (package
    (name "julia-stringencodings")
    (version "0.3.5")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaStrings/StringEncodings.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1qwc5ll68ng80b5921ww6fvifxbsmiylakfgsbsjbzg7lzyb5i67"))))
    (build-system julia-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'skip-failing-test
            (lambda _
              ;; https://github.com/JuliaStrings/StringEncodings.jl/issues/49
              (substitute* "test/runtests.jl"
                (("\"SHIFT_JIS\", \"SHIFT_JISX0213\"")
                 " ")))))))
    (propagated-inputs
     (list julia-libiconv-jll))
    (home-page "https://github.com/JuliaStrings/StringEncodings.jl")
    (synopsis "Support for decoding and encoding texts")
    (description "This package provides support for decoding and encoding
texts between multiple character encodings.  It is currently based on the
@code{iconv} interface, and supports all major platforms using GNU libiconv.")
    (license license:expat)))

(define-public julia-stringmanipulation
  (package
    (name "julia-stringmanipulation")
    (version "0.3.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ronisbr/StringManipulation.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "15ss8hkjyjs2x66j1krrrxaa1hdpwz0ygs3cg3bdpm336k7621q8"))))
    (build-system julia-build-system)
    (home-page "https://github.com/ronisbr/StringManipulation.jl")
    (synopsis "Functions to manipulate strings with ANSI escape sequences")
    (description "This package provides several functions to manipulate strings
with ANSI escape sequences.")
    (license license:expat)))

(define-public julia-structarrays
  (package
    (name "julia-structarrays")
    (version "0.6.3")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaArrays/StructArrays.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0rjcpyjwzg70n87q5r9c5i1qzigavncslxssm3rk5a3y549py56v"))))
    (build-system julia-build-system)
    (arguments
     (list
      #:phases
      (if (target-64bit?)
          #~%standard-phases
          #~(modify-phases %standard-phases
              (add-after 'unpack 'fix-tests-int32-i686
                (lambda _
                  (substitute* '("src/utils.jl"
                                 "test/runtests.jl")
                    (("Int64") "Int32"))))))))
    (propagated-inputs
     (list julia-adapt
           julia-dataapi
           julia-staticarrays
           julia-tables))
    (native-inputs
     (list julia-documenter
           julia-offsetarrays
           julia-pooledarrays
           julia-typedtables
           julia-weakrefstrings))
    (home-page "https://github.com/JuliaArrays/StructArrays.jl")
    (synopsis "Efficient implementation of struct arrays in Julia")
    (description "This package introduces the type @code{StructArray} which is
an @code{AbstractArray} whose elements are @code{struct} (for example
@code{NamedTuples}, or @code{ComplexF64}, or a custom user defined
@code{struct}).  While a @code{StructArray} iterates @code{structs}, the layout
is column based (meaning each field of the @code{struct} is stored in a separate
@code{Array}).")
    (license license:expat)))

(define-public julia-structtypes
  (package
    (name "julia-structtypes")
    (version "1.7.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaData/StructTypes.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "02mn4kkhn3927dk7945c9bjwlldihydxgn5ilmqqvs8dknvbw8p1"))))
    (build-system julia-build-system)
    (home-page "https://juliadata.github.io/StructTypes.jl/stable/")
    (synopsis "Abstract definitions and convenience methods for Julia objects")
    (description "This package provides the @code{StructTypes.StructType} trait
for Julia types to declare the kind of \"struct\" they are, providing
serialization/deserialization packages patterns and strategies to automatically
construct objects.")
    (license license:expat)))

(define-public julia-suppressor
  (package
    (name "julia-suppressor")
    (version "0.2.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaIO/Suppressor.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0v6pxvf8lzrqjc676snvlszh14ridl442g2h6syfjiy75pk7mdyc"))))
    (build-system julia-build-system)
    (home-page "https://github.com/JuliaIO/Suppressor.jl")
    (synopsis "Capture stdout and sterr")
    (description "Julia macros for suppressing and/or capturing output (stdout),
warnings (stderr) or both streams at the same time.")
    (license license:expat)))

(define-public julia-tableiointerface
  (package
    (name "julia-tableiointerface")
    (version "0.1.6")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/lungben/TableIOInterface.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0p2fi9jbyfg2j6rysv4if7dx8qw2mssb04i75j1zq607j8707kvn"))))
    (build-system julia-build-system)
    (home-page "https://github.com/lungben/TableIOInterface.jl")
    (synopsis "File formats based on file extensions")
    (description "This package determines tabular file formats based on file
extensions.  It is intended to be the base both for @code{TableIO.jl} and for
the @code{Pluto.jl} tabular data import functionality.")
    (license license:expat)))

(define-public julia-tables
  (package
    (name "julia-tables")
    (version "1.4.4")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaData/Tables.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1sqqagzqvav8b1rv5ywwbgy9ixvlmipq95fkwfwn0m8769i8jwzb"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-dataapi
           julia-datavalueinterfaces
           julia-iteratorinterfaceextensions
           julia-tabletraits))
    (native-inputs
     (list julia-datavalues
           julia-queryoperators))
    (home-page "https://github.com/JuliaData/Tables.jl")
    (synopsis "Interface for tables in Julia")
    (description "The @code{Tables.jl} package provides simple, yet powerful
interface functions for working with all kinds tabular data.")
    (license license:expat)))

(define-public julia-tableshowutils
  ;; The 0.2.5 release is not fully compatible with newer versions of Julia.
  (let ((commit "c4e02d8b9bbb31fc81ed6618955e9b1c7cb04460")
        (revision "1"))
    (package
      (name "julia-tableshowutils")
      (version "0.2.5")
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/queryverse/TableShowUtils.jl")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32 "0gp3hpj3jvzfhkp9r345vfic2j2n2s60729wv38hwn75csp74cg5"))))
      (build-system julia-build-system)
      (propagated-inputs
       (list julia-datavalues
             julia-json))
      (home-page "https://github.com/queryverse/TableShowUtils.jl")
      (synopsis "Implement show for TableTraits.jl types")
      (description "This package provides some common helper functions that make
it easier to implement various @code{Base.show} functions for types that
participate in the @code{TableTraits.jl} ecosystem.")
      (license license:expat))))

(define-public julia-tabletraits
  (package
    (name "julia-tabletraits")
    (version "1.0.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/queryverse/TableTraits.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "08ssb2630wm6j8f2qa985mn2vfibfm5kjcn4ayl2qkhfcyp8daw4"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-iteratorinterfaceextensions))
    (home-page "https://github.com/queryverse/TableTraits.jl")
    (synopsis "Traits for Julia tables")
    (description "TableTraits defines a generic interface for tabular data.")
    (license license:expat)))

(define-public julia-tensorcore
  (package
    (name "julia-tensorcore")
    (version "0.1.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaMath/TensorCore.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1sy3in4a1rl3l2vk0cm9mzg2nkva7syhr7i35si0kbzhkdwpbqjy"))))
    (build-system julia-build-system)
    (home-page "https://github.com/JuliaMath/TensorCore.jl")
    (synopsis "Tensor-algebra definitions")
    (description "This package is intended as a lightweight foundation for
tensor operations across the Julia ecosystem.  Currently it exports three
operations: @acronym{hadamard, elementwise multiplication}, @acronym{tensor,
product preserves all dimensions}, and @acronym{boxdot, contracts neighboring
dimensions}.")
    (license license:expat)))

(define-public julia-terminterface
  (package
    (name "julia-terminterface")
    (version "2.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaSymbolics/TermInterface.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ish9can05b3pv0kyf035yk1mf1pnkg9l66xb2a8xg9rvcrv5rkb"))))
    (build-system julia-build-system)
    (home-page "https://github.com/JuliaSymbolics/TermInterface.jl")
    (synopsis "Common interface for symbolic terms in Julia")
    (description
     "This package provides definitions for common functions
that are useful for symbolic expression manipulation in Julia.  Its purpose
is to provide a shared interface between various symbolic programming
packages, for example @code{SymbolicUtils.jl}, @code{Symbolics.jl},
and @code{Metatheory.jl}.")
    (license license:expat)))

(define-public julia-testimages
  (package
    (name "julia-testimages")
    (version "1.5.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaImages/TestImages.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1lnfsmx33qspyvxw0cykwh7il8xykjpcw1080sisn95ngz2qhdmy"))))
    (build-system julia-build-system)
    (arguments
     (list #:tests? #f))    ; cycle with ImageMagick.jl
    (propagated-inputs
     (list julia-axisarrays
           julia-colortypes
           julia-fileio
           julia-offsetarrays
           julia-stringdistances))
    ;(native-inputs
    ; `(("julia-colors" ,julia-colors)
    ;   ("julia-fixedpointnumbers" ,julia-fixedpointnumbers)
    ;   ("julia-imagecontrastadjustment" ,julia-imagecontrastadjustment)
    ;   ("julia-imagemagick" ,julia-imagemagick)
    ;   ("julia-ometiff" ,julia-ometiff)
    ;   ("julia-referencetests" ,julia-referencetests)))
    (home-page "https://testimages.juliaimages.org/")
    (synopsis "Standard test images for Julia")
    (description "This package provides a convenient Julia interface for loading
standard named test images and example images for the internal usage in
@code{JuliaImages}.  This can be used in conjunction with the @code{Images}
package.")
    (license license:expat)))

(define-public julia-timeroutputs
  (package
    (name "julia-timeroutputs")
    (version "0.5.26")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/KristofferC/TimerOutputs.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0mz5kpjz2mcmaywxjp1y87wx02lmvi2b0z012rnz70p1y39s5y0v"))))
    (build-system julia-build-system)
    (propagated-inputs (list julia-exprtools))
    (home-page "https://github.com/KristofferC/TimerOutputs.jl")
    (synopsis "Formatted output of timed sections in Julia")
    (description "This package generates formatted output from timings made
in different sections of a program.")
    (license license:expat)))

(define-public julia-tokenize
  (package
    (name "julia-tokenize")
    (version "0.5.24")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaLang/Tokenize.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1l3dy6nad0viavzy26lfnhzpd3gcxgaq7yvm7h1ja280xsh60p3i"))))
    (build-system julia-build-system)
    (home-page "https://github.com/JuliaGPU/Tokenize.jl")
    (synopsis "Tokenize a string or buffer containing Julia code")
    (description "This package takes a string or buffer containing Julia code,
performs lexical analysis and returns a stream of tokens.")
    (license license:expat)))

(define-public julia-tracker
  (package
    (name "julia-tracker")
    (version "0.2.22")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/FluxML/Tracker.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0sxncn999dc5j15y0h3cw28x41pv5qjaw64drhy1y4rn3na48504"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-adapt
           julia-diffrules
           julia-forwarddiff
           julia-functors
           julia-logexpfunctions
           julia-macrotools
           julia-nanmath
           julia-nnlib
           julia-optimisers
           julia-requires
           julia-specialfunctions))
    (native-inputs
     (list julia-pdmats))
    (home-page "https://github.com/FluxML/Tracker.jl")
    (synopsis "Operator overloading reverse-mode automatic differentiator")
    (description "@code{Tracker.jl} previously provided @code{Flux.jl} with
automatic differentiation for its machine learning platform.")
    (license license:expat)))

(define-public julia-transcodingstreams
  (package
    (name "julia-transcodingstreams")
    (version "0.9.6")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaIO/TranscodingStreams.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1w3klii293caqiclfh28jggv7f53xclm9fr6xmw38brwrn1hjb48"))))
    (build-system julia-build-system)
    (arguments
     (list #:tests? #f))                ; Circular dependency with various codecs.
    (home-page "https://github.com/JuliaIO/TranscodingStreams.jl")
    (synopsis "Fast I/O transcoding data streams")
    (description "This package provides tools for transcoding data streams
which are:
@itemize
@item fast: small overhead and specialized methods
@item consistent: basic I/O operations work as expected
@item generic: support any I/O objects like files, buffers, pipes, etc.
@item extensible: easy definition for new codec to transcode data
@end itemize")
    (license license:expat)))

(define-public julia-twiddle
  (package
    (name "julia-twiddle")
    (version "1.1.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/SabrinaJaye/Twiddle.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1c2gdv7sy4n1d8687w2q0yzwmwmanf4p6mvzvkz5gm4baxyzmbh2"))))
    (build-system julia-build-system)
    (home-page "https://ben-ward.science/Twiddle.jl/stable")
    (synopsis "Ready to use bit-twiddling tricks")
    (description "This package provides a collection of useful bit-twiddling
tricks, ready to use as functions, with detailed documentation and example
real-world use cases.")
    (license license:expat)))

(define-public julia-typedtables
  (package
    (name "julia-typedtables")
    (version "1.4.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaData/TypedTables.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0nk6zhqvl2r8yhjdhb59kxq0srd3vy4ysg4d8rszj9a43dnn3w3i"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-adapt
           julia-splitapplycombine
           julia-tables))
    (home-page "https://github.com/JuliaData/TypedTables.jl")
    (synopsis "Column-based storage for data analysis in Julia")
    (description "@code{TypedTables.jl} provides two column-based storage
containers: @code{Table} and @code{FlexTable}, both of which represent an array
of @code{NamedTuples}.  This package is designed to be lightweight, easy-to-use
and fast, and presents a very minimal new interface to learn.")
    (license license:expat)))

(define-public julia-unpack
  (package
    (name "julia-unpack")
    (version "1.0.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/mauro3/UnPack.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "066v1px72zidnvhl0rczhh07rcfwvli0jx5nprrgyi1dvj3mps2a"))))
    (build-system julia-build-system)
    (home-page "https://github.com/mauro3/UnPack.jl")
    (synopsis "Pack and Unpack macros for Julia")
    (description "The @code{@@unpack} and @code{@@pack!} macros work to unpack
types, modules, and dictionaries.")
    (license license:expat)))

(define-public julia-uris
  (package
    (name "julia-uris")
    (version "1.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaWeb/URIs.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0kp4hg3kknkm2smlcizqfd33l9x4vkahc2714gnbjp39fj285b92"))))
    (build-system julia-build-system)
    (arguments
     (list
      #:julia-package-name "URIs"       ;required to run tests
      #:julia-package-uuid "5c2747f8-b7ea-4ff2-ba2e-563bfd36b1d4"
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'change-dir
            ;; Tests must be run from the testdir
            (lambda* (#:key source outputs #:allow-other-keys)
              (let ((out (assoc-ref outputs "out")))
                (chdir
                 (string-append out "/share/julia/loadpath/URIs/test"))))))))
    ;; required for tests
    (inputs (list julia-json))
    (home-page "https://github.com/JuliaWeb/URIs.jl")
    (synopsis "URI parsing in Julia")
    (description "@code{URIs.jl} is a Julia package that allows parsing and
working with @acronym{URIs,Uniform Resource Identifiers}, as defined in RFC
3986.")
    (license license:expat)))

(define-public julia-unitful
  (package
    (name "julia-unitful")
    (version "1.12.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/PainterQubits/Unitful.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1b8w6wqc7azqzg2f8zc3bmc72fb01sx0rqh6dv3k54wj01ph15p7"))))
    (build-system julia-build-system)
    (arguments
     (list #:parallel-tests? #f))
    (propagated-inputs
     (list julia-constructionbase))
    (home-page "https://painterqubits.github.io/Unitful.jl/stable/")
    (synopsis "Physical units in Julia")
    (description "This package supports SI units and also many other unit
system.")
    (license license:expat)))

(define-public julia-versionparsing
  (package
    (name "julia-versionparsing")
    (version "1.2.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaInterop/VersionParsing.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "060s72dsnpavgilf7f7315lw2sn4npk8lkndmj6bg7i23hppiwva"))))
    (build-system julia-build-system)
    (home-page "https://github.com/JuliaInterop/VersionParsing.jl")
    (synopsis "Flexible VersionNumber parsing in Julia")
    (description "The @code{VersionParsing} package implements flexible parsing
of version-number strings into Julia's built-in @code{VersionNumber} type, via
the @code{vparse(string)} function.  Unlike the @code{VersionNumber(string)}
constructor, @code{vparse(string)} can handle version-number strings in a much
wider range of formats than are encompassed by the semver standard.  This is
useful in order to support @code{VersionNumber} comparisons applied to
\"foreign\" version numbers from external packages.")
    (license license:expat)))

(define-public julia-wcs
  (package
    (name "julia-wcs")
    (version "0.6.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaAstro/WCS.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ala8j4mh51gh14k3npcxmnlj2f00l0pij74qz453iqadb2283mi"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-constructionbase julia-wcs-jll))
    (home-page "https://github.com/JuliaAstro/WCS.jl")
    (synopsis "Astronomical WCS library for Julia")
    (description "Astronomical @url{World Coordinate System,
https://www.atnf.csiro.au/people/mcalabre/WCS/} library for Julia.  This package
wraps the WCSLIB C library.")
    (license license:expat)))

(define-public julia-weakrefstrings
  (package
    (name "julia-weakrefstrings")
    (version "1.4.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaData/WeakRefStrings.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1ca94bpsjqrap2y9wlixspnisfkcms7aax0kpv7yn0v2vs9481wk"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-dataapi
           julia-inlinestrings
           julia-parsers))
    (home-page "https://github.com/JuliaData/WeakRefStrings.jl")
    (synopsis "Efficient string representation and transfer in Julia")
    (description "This package provides a minimal String type for Julia that
allows for efficient string representation and transfer")
    (license license:expat)))

(define-public julia-woodburymatrices
  (package
    (name "julia-woodburymatrices")
    (version "0.5.5")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/timholy/WoodburyMatrices.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1vwy8nlhvjh0ndia4ni40iq4pf2nhwy5iy3rmf4i2jff13vc6aqn"))))
    (build-system julia-build-system)
    (home-page "https://github.com/timholy/WoodburyMatrices.jl")
    (synopsis "Support for the Woodbury matrix identity for Julia")
    (description "This package provides support for the Woodbury matrix identity
for the Julia programming language.  This is a generalization of the
Sherman-Morrison formula.  Note that the Woodbury matrix identity is notorious
for floating-point roundoff errors, so be prepared for a certain amount of
inaccuracy in the result.")
    (license license:expat)))

(define-public julia-yaml
  (package
    (name "julia-yaml")
    (version "0.4.7")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaData/YAML.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "00k8456ffldbf75k2q5yxim7cgz3p0pbshsvmpm1331g8qy6liin"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-stringencodings))
    (native-inputs
     (list julia-datastructures
           julia-orderedcollections))
    (home-page "https://github.com/JuliaData/YAML.jl")
    (synopsis "Parses YAML documents into native Julia types")
    (description "This package parses YAML documents into native Julia types
and dumps them back into YAML documents.")
    (license license:expat)))

(define-public julia-zipfile
  (package
    (name "julia-zipfile")
    (version "0.9.3")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/fhs/ZipFile.jl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "15bm3ki5mb9nvqs2byznrryq0bilnjcvsfy3k05hxhk9vapilw7k"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-zlib-jll))
    (home-page "https://github.com/fhs/ZipFile.jl")
    (synopsis "Read/Write ZIP archives in Julia")
    (description "This module provides support for reading and writing ZIP
archives in Julia.")
    (license license:expat)))

(define-public julia-zygoterules
  (package
    (name "julia-zygoterules")
    (version "0.2.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/FluxML/ZygoteRules.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0h9m9ibxfcw9cqa7p0aylpvibvlxsn5nlfzkz1pk68jy58vkzhca"))))
    (build-system julia-build-system)
    (propagated-inputs
     (list julia-macrotools))
    (home-page "https://github.com/FluxML/ZygoteRules.jl")
    (synopsis "Add minimal custom gradients to Zygote")
    (description "Minimal package which enables to add custom gradients to
Zygote, without depending on Zygote itself.")
    (license license:expat)))

(define-public julia-zygote
  (package
    (name "julia-zygote")
    (version "0.6.41")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/FluxML/Zygote.jl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "02bgj6m1j25sm3pa5sgmds706qpxk1qsbm0s2j3rjlrz9xn7glgk"))))
    (build-system julia-build-system)
    (arguments
     (list #:tests? #f))                    ;require CUDA, not packaged yet
    (propagated-inputs
     (list julia-abstractffts
           julia-chainrules
           julia-chainrulescore
           julia-diffrules
           julia-fillarrays
           julia-forwarddiff
           julia-irtools
           julia-logexpfunctions
           julia-macrotools
           julia-nanmath
           julia-requires
           julia-specialfunctions
           julia-zygoterules))
    (home-page "https://fluxml.ai/Zygote.jl")
    (synopsis "Automatic differentiation in Julia")
    (description "Zygote provides source-to-source automatic
differentiation (AD) in Julia, and is the next-generation AD system for the
Flux differentiable programming framework.")
    (license license:expat)))

;;;
;;; Avoid adding new packages to the end of this file. To reduce the chances
;;; of a merge conflict, place them above by existing packages with similar
;;; functionality or similar names.
;;;
