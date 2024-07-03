;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015-2024 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016, 2017, 2018, 2020, 2021 Roel Janssen <roel@gnu.org>
;;; Copyright © 2016 Pjotr Prins <pjotr.guix@thebird.nl>
;;; Copyright © 2016 Ben Woodcroft <donttrustben@gmail.com>
;;; Copyright © 2017, 2022, 2024 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017, 2018, 2019, 2020, 2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019, 2020, 2021, 2022, 2023 Simon Tournier <zimon.toutoune@gmail.com>
;;; Copyright © 2020 Peter Lo <peterloleungyau@gmail.com>
;;; Copyright © 2020-2023 Mădălin Ionel Patrașcu <madalinionel.patrascu@mdc-berlin.de>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2021 Hong Li <hli@mdc-berlin.de>
;;; Copyright © 2021 Tim Howes <timhowes@lavabit.com>
;;; Copyright © 2021 Nicolas Vallet <nls.vallet@gmail.com>
;;; Copyright © 2023 Navid Afkhami <Navid.Afkhami@mdc-berlin.de>
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

(define-module (gnu packages bioconductor)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix git-download)
  #:use-module (guix build-system r)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages chemistry)
  #:use-module (gnu packages cran)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages docker)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages graph)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages image)
  #:use-module (gnu packages java)
  #:use-module (gnu packages javascript)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages netpbm)
  #:use-module (gnu packages python)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module ((srfi srfi-1) #:hide (zip)))


;;; Annotations

(define-public r-bsgenome-hsapiens-ucsc-hg38-masked
  (package
    (name "r-bsgenome-hsapiens-ucsc-hg38-masked")
    (version "1.4.5")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BSgenome.Hsapiens.UCSC.hg38.masked" version
                              'annotation))
       (sha256
        (base32 "0j71hdxqvvc0s8mc6jp6zk502mrf095qazj95yzzb4rm6sjvd20m"))))
    (properties `((upstream-name . "BSgenome.Hsapiens.UCSC.hg38.masked")))
    (build-system r-build-system)
    (propagated-inputs (list r-bsgenome r-bsgenome-hsapiens-ucsc-hg38
                             r-genomeinfodb))
    (home-page
     "https://bioconductor.org/packages/BSgenome.Hsapiens.UCSC.hg38.masked")
    (synopsis
     "Full masked genomic sequences for Homo sapiens (UCSC version hg38)")
    (description
     "This package provides the complete genome sequences for Homo sapiens as
provided by UCSC (genome hg38, based on assembly GRCh38.p14 since 2023/01/31).
The sequences are the same as in BSgenome.Hsapiens.UCSC.hg38, except that each
of them has the 4 following masks on top:

@enumerate
@item the mask of assembly gaps (AGAPS mask);
@item the mask of intra-contig ambiguities (AMB mask);
@item the mask of repeats from @code{RepeatMasker} (RM mask);
@item the mask of repeats from Tandem Repeats Finder (TRF mask).
@end enumerate

Only the AGAPS and AMB masks are \"active\" by default.  The sequences are stored
in @code{MaskedDNAString} objects.")
    (license license:artistic2.0)))

(define-public r-ensdb-hsapiens-v79
  (package
    (name "r-ensdb-hsapiens-v79")
    (version "2.99.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "EnsDb.Hsapiens.v79" version
                              'annotation))
       (sha256
        (base32 "0k94iml0417m3k086d0bzd83fndyb2kn7pimsfzcdmafgy6sxwgg"))))
    (properties `((upstream-name . "EnsDb.Hsapiens.v79")))
    (build-system r-build-system)
    (propagated-inputs (list r-ensembldb))
    (home-page "https://bioconductor.org/packages/EnsDb.Hsapiens.v79")
    (synopsis "Ensembl based annotation package")
    (description "This package exposes an annotation database generated from
Ensembl.")
    (license license:artistic2.0)))

(define-public r-hpo-db
  (package
    (name "r-hpo-db")
    (version "0.99.2")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "HPO.db" version
                              'annotation))
       (sha256
        (base32 "1brzrnafvyh76h8a663gk5lprhixxpi9xi65vwgxwf7jh6yw0was"))))
    (properties `((upstream-name . "HPO.db")))
    (build-system r-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'avoid-internet-access
            (lambda* (#:key inputs #:allow-other-keys)
              (let* ((cache (string-append #$output "/share/HPO.db/cache"))
                     (file (string-append cache "/118333")))
                (mkdir-p cache)
                (copy-file #$(this-package-native-input "HPO.sqlite") file)
                (substitute* "R/zzz.R"
                  (("ah <- suppressMessages\\(AnnotationHub\\(\\)\\)" m)
                   (string-append
                    "if (Sys.getenv(\"NIX_BUILD_TOP\") == \"\") { " m " };"))
                  (("dbfile <- ah.*" m)
                   (string-append
                    "if (Sys.getenv(\"NIX_BUILD_TOP\") != \"\") { dbfile <- \""
                    file "\";} else { " m " }\n")))))))))
    (propagated-inputs
     (list r-annotationdbi r-annotationhub r-biocfilecache r-dbi))
    (native-inputs
     `(("r-knitr" ,r-knitr)
       ("HPO.sqlite"
        ,(origin
           (method url-fetch)
           (uri "https://annotationhub.bioconductor.org/fetch/118333")
           (file-name "HPO.sqlite")
           (sha256
            (base32 "1wwdwf27iil0p41183qgygh2ifphhmlljjkgjm2h8sr25qycf0md"))))))
    (home-page "https://bioconductor.org/packages/HPO.db")
    (synopsis
     "Annotation maps describing the entire Human Phenotype Ontology")
    (description
     "Human Phenotype Ontology (HPO) was developed to create a consistent
description of gene products with disease perspectives, and is essential for
supporting functional genomics in disease context.  Accurate disease
descriptions can discover new relationships between genes and disease, and new
functions for previous uncharacteried genes and alleles.")
    (license license:artistic2.0)))

(define-public r-jaspar2020
  (package
    (name "r-jaspar2020")
    (version "0.99.10")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "JASPAR2020" version 'annotation))
       (sha256
        (base32 "0nrp63z7q2ivv5h87f7inpp2qll2dfgj4227l4rbnzii38a2vfdr"))))
    (properties `((upstream-name . "JASPAR2020")))
    (build-system r-build-system)
    (native-inputs (list r-knitr))
    (home-page "https://jaspar.elixir.no/")
    (synopsis "Data package for JASPAR database (version 2020)")
    (description "Data package for JASPAR2020.  To explore these databases,
utilize the TFBSTools package (version 1.23.1 or higher).")
    (license license:gpl2)))

(define-public r-mafh5-gnomad-v3-1-2-grch38
  (package
    (name "r-mafh5-gnomad-v3-1-2-grch38")
    (version "3.15.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "MafH5.gnomAD.v3.1.2.GRCh38" version
                              'annotation))
       (sha256
        (base32 "1q9hlva814sjfz8vm9bzw7xzppbcfy5qq7nnz9w742yr59cjb6mp"))))
    (properties `((upstream-name . "MafH5.gnomAD.v3.1.2.GRCh38")))
    (build-system r-build-system)
    (propagated-inputs (list r-bsgenome
                             r-genomeinfodb
                             r-genomicranges
                             r-genomicscores
                             r-hdf5array
                             r-iranges
                             r-rhdf5
                             r-s4vectors))
    (home-page "https://bioconductor.org/packages/MafH5.gnomAD.v3.1.2.GRCh38")
    (synopsis
     "Minor allele frequency data from gnomAD version 3.1.2 for GRCh38")
    (description
     "This package is designed to store minor allele frequency data.
It retrieves this data from the Genome Aggregation Database
(@code{gnomAD} version 3.1.2) for the human genome version GRCh38.")
    (license license:artistic2.0)))

(define-public r-mpo-db
  (package
    (name "r-mpo-db")
    (version "0.99.7")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "MPO.db" version
                              'annotation))
       (sha256
        (base32 "0x1rcikg189akbd71yh0p02482km9hry6i69s2srdf5mlgqficvl"))))
    (properties `((upstream-name . "MPO.db")))
    (build-system r-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'avoid-internet-access
            (lambda* (#:key inputs #:allow-other-keys)
              (let* ((cache (string-append #$output "/share/MPO.db/cache"))
                     (file (string-append cache "/118299")))
                (mkdir-p cache)
                (copy-file #$(this-package-native-input "MPO.sqlite") file)
                (substitute* "R/zzz.R"
                  (("ah <- suppressMessages\\(AnnotationHub\\(\\)\\)" m)
                   (string-append
                    "if (Sys.getenv(\"NIX_BUILD_TOP\") == \"\") { " m " };"))
                  (("dbfile <- ah.*" m)
                   (string-append
                    "if (Sys.getenv(\"NIX_BUILD_TOP\") != \"\") { dbfile <- \""
                    file "\";} else { " m " }\n")))))))))
    (propagated-inputs
     (list r-annotationdbi r-annotationhub r-biocfilecache r-dbi))
    (native-inputs
     `(("r-knitr" ,r-knitr)
       ("MPO.sqlite"
        ,(origin
           (method url-fetch)
           (uri "https://annotationhub.bioconductor.org/fetch/118299")
           (file-name "MPO.sqlite")
           (sha256
            (base32 "12rf5dpnjrpw55bgnbn68dni2g0p87nvs9c7mamqk0ayafs61zl0"))))))
    (home-page "https://github.com/YuLab-SMU/MPO.db")
    (synopsis "set of annotation maps describing the Mouse Phenotype Ontology")
    (description
     "This is the human disease ontology R package HDO.db, which provides the
semantic relationship between human diseases.  Relying on the DOSE and
GO@code{SemSim} packages, this package can carry out disease enrichment and
semantic similarity analyses.  Many biological studies are achieved through
mouse models, and a large number of data indicate the association between
genotypes and phenotypes or diseases.  The study of model organisms can be
transformed into useful knowledge about normal human biology and disease to
facilitate treatment and early screening for diseases.  Organism-specific
genotype-phenotypic associations can be applied to cross-species phenotypic
studies to clarify previously unknown phenotypic connections in other species.
Using the same principle to diseases can identify genetic associations and
even help to identify disease associations that are not obvious.")
    (license license:artistic2.0)))

(define-public r-org-eck12-eg-db
  (package
    (name "r-org-eck12-eg-db")
    (version "3.19.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "org.EcK12.eg.db" version 'annotation))
       (sha256
        (base32 "1af2yrbpn58z34jq39qmmwprzsffgxbrs84dsxny4rksa8k6j70s"))))
    (properties
     `((upstream-name . "org.EcK12.eg.db")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotationdbi))
    (home-page "https://bioconductor.org/packages/org.EcK12.eg.db")
    (synopsis "Genome wide annotation for E coli strain K12")
    (description
     "This package provides genome wide annotation for E coli strain K12,
primarily based on mapping using Entrez Gene identifiers.  Entrez Gene is
National Center for Biotechnology Information (NCBI)’s database for
gene-specific information.  Entrez Gene maintains records from genomes which
have been completely sequenced, which have an active research community to
submit gene-specific information, or which are scheduled for intense sequence
analysis.")
    (license license:artistic2.0)))

(define-public r-org-bt-eg-db
  (package
    (name "r-org-bt-eg-db")
    (version "3.19.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri
             "org.Bt.eg.db"
             version
             'annotation))
       (sha256
        (base32
         "0rbbjv47fadc6jgpqrimsd91f35fw6sw8d805vf61nx5xhbl3lpc"))))
    (properties `((upstream-name . "org.Bt.eg.db")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotationdbi))
    (home-page "https://bioconductor.org/packages/org.Bt.eg.db")
    (synopsis "Genome wide annotation for Bovine")
    (description
     "This package provides genome wide annotations for Bovine, primarily
based on mapping using Entrez Gene identifiers.")
    (license license:artistic2.0)))

(define-public r-org-sc-sgd-db
  (package
    (name "r-org-sc-sgd-db")
    (version "3.19.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "org.Sc.sgd.db" version
                              'annotation))
       (sha256
        (base32 "1sbjwyf0ibivdhhxsaljgqa4lqp3hcf8gcrlcm2vd78bfm5cm7bg"))))
    (properties `((upstream-name . "org.Sc.sgd.db")))
    (build-system r-build-system)
    (propagated-inputs (list r-annotationdbi))
    (home-page "https://bioconductor.org/packages/org.Sc.sgd.db")
    (synopsis "Genome wide annotation for Yeast")
    (description
     "This package provides genome wide annotation for Yeast, primarily based
on mapping using ORF identifiers from @acronym{SGD, Saccharomyces Genome
Database}.")
    (license license:artistic2.0)))

(define-public r-pd-mapping50k-xba240
  (package
    (name "r-pd-mapping50k-xba240")
    (version "3.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "pd.mapping50k.xba240" version
                              'annotation))
       (sha256
        (base32 "1a1f3lh5ywhyjawdbj2fzban85c8jz70lfcv3pagd5piincjwxq8"))))
    (properties `((upstream-name . "pd.mapping50k.xba240")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biostrings
           r-dbi
           r-iranges
           r-oligo
           r-oligoclasses
           r-rsqlite
           r-s4vectors))
    (home-page "https://bioconductor.org/packages/pd.mapping50k.xba240")
    (synopsis "Platform design info for Affymetrix Mapping50K_Xba240")
    (description "This package provides platform design info for Affymetrix
Mapping50K_Xba240 (pd.mapping50k.xba240).")
    (license license:artistic2.0)))

(define-public r-reactome-db
  (package
    (name "r-reactome-db")
    (version "1.88.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "reactome.db" version 'annotation))
       (sha256
        (base32
         "1fwbf53vklhvqa6n6pijb8xqyv2pap1hskkirnm5c42wmd6g4ql7"))))
    (properties `((upstream-name . "reactome.db")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotationdbi))
    (home-page "https://bioconductor.org/packages/reactome.db/")
    (synopsis "Annotation maps for reactome")
    (description
     "This package provides a set of annotation maps for the REACTOME
database, assembled using data from REACTOME.")
    (license license:cc-by4.0)))

(define-public r-bsgenome-btaurus-ucsc-bostau8
  (package
    (name "r-bsgenome-btaurus-ucsc-bostau8")
    (version "1.4.2")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "BSgenome.Btaurus.UCSC.bosTau8"
                                     version 'annotation))
              (sha256
               (base32
                "16wjy1aw9nvx03r7w8yh5w7sw3pn8i9nczd0n0728l6nnyqxlsz6"))))
    (properties
     `((upstream-name . "BSgenome.Btaurus.UCSC.bosTau8")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-bsgenome))
    (home-page
     "https://www.bioconductor.org/packages/BSgenome.Btaurus.UCSC.bosTau8/")
    (synopsis "Full genome sequences for Bos taurus (UCSC version bosTau8)")
    (description "This package provides the full genome sequences for Bos
taurus (UCSC version bosTau8).")
    (license license:artistic2.0)))

(define-public r-bsgenome-celegans-ucsc-ce6
  (package
    (name "r-bsgenome-celegans-ucsc-ce6")
    (version "1.4.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "BSgenome.Celegans.UCSC.ce6"
                                     version 'annotation))
              (sha256
               (base32
                "0mqzb353xv2c3m3vkb315dkmnxkgczp7ndnknyhpgjlybyf715v9"))))
    (properties
     `((upstream-name . "BSgenome.Celegans.UCSC.ce6")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-bsgenome))
    (home-page
     "https://www.bioconductor.org/packages/BSgenome.Celegans.UCSC.ce6/")
    (synopsis "Full genome sequences for Worm")
    (description
     "This package provides full genome sequences for Caenorhabditis
elegans (Worm) as provided by UCSC (ce6, May 2008) and stored in Biostrings
objects.")
    (license license:artistic2.0)))

(define-public r-bsgenome-celegans-ucsc-ce10
  (package
    (name "r-bsgenome-celegans-ucsc-ce10")
    (version "1.4.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "BSgenome.Celegans.UCSC.ce10"
                                     version 'annotation))
              (sha256
               (base32
                "1zaym97jk4npxk14ifvwz2rvhm4zx9xgs33r9vvx9rlynp0gydrk"))))
    (properties
     `((upstream-name . "BSgenome.Celegans.UCSC.ce10")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-bsgenome))
    (home-page
     "https://www.bioconductor.org/packages/BSgenome.Celegans.UCSC.ce10/")
    (synopsis "Full genome sequences for Worm")
    (description
     "This package provides full genome sequences for Caenorhabditis
elegans (Worm) as provided by UCSC (ce10, Oct 2010) and stored in Biostrings
objects.")
    (license license:artistic2.0)))

(define-public r-bsgenome-dmelanogaster-ucsc-dm6
  (package
    (name "r-bsgenome-dmelanogaster-ucsc-dm6")
    (version "1.4.1")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "BSgenome.Dmelanogaster.UCSC.dm6"
                                     version 'annotation))
              (sha256
               (base32
                "1bhj0rdgf7lspw4xby9y9mf7v7jxxz8001bc8vw8kf04rjsx6060"))))
    (properties
     `((upstream-name . "BSgenome.Dmelanogaster.UCSC.dm6")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-bsgenome))
    (home-page
     "https://www.bioconductor.org/packages/BSgenome.Dmelanogaster.UCSC.dm6/")
    (synopsis "Full genome sequences for Fly")
    (description
     "This package provides full genome sequences for Drosophila
melanogaster (Fly) as provided by UCSC (dm6) and stored in Biostrings
objects.")
    (license license:artistic2.0)))

(define-public r-bsgenome-dmelanogaster-ucsc-dm3
  (package
    (name "r-bsgenome-dmelanogaster-ucsc-dm3")
    (version "1.4.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "BSgenome.Dmelanogaster.UCSC.dm3"
                                     version 'annotation))
              (sha256
               (base32
                "19bm3lkhhkag3gnwp419211fh0cnr0x6fa0r1lr0ycwrikxdxsv8"))))
    (properties
     `((upstream-name . "BSgenome.Dmelanogaster.UCSC.dm3")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-bsgenome))
    (home-page
     "https://www.bioconductor.org/packages/BSgenome.Dmelanogaster.UCSC.dm3/")
    (synopsis "Full genome sequences for Fly")
    (description
     "This package provides full genome sequences for Drosophila
melanogaster (Fly) as provided by UCSC (dm3, April 2006) and stored in
Biostrings objects.")
    (license license:artistic2.0)))

(define-public r-bsgenome-dmelanogaster-ucsc-dm3-masked
  (package
    (name "r-bsgenome-dmelanogaster-ucsc-dm3-masked")
    (version "1.3.99")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "BSgenome.Dmelanogaster.UCSC.dm3.masked"
                                     version 'annotation))
              (sha256
               (base32
                "1756csb09f1br9rj1l3f08qyh4hlymdbd0cfn8x3fq39dn45m5ap"))))
    (properties
     `((upstream-name . "BSgenome.Dmelanogaster.UCSC.dm3.masked")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-bsgenome r-bsgenome-dmelanogaster-ucsc-dm3))
    (home-page "https://www.bioconductor.org/packages/BSgenome.Dmelanogaster.UCSC.dm3.masked/")
    (synopsis "Full masked genome sequences for Fly")
    (description
     "This package provides full masked genome sequences for Drosophila
melanogaster (Fly) as provided by UCSC (dm3, April 2006) and stored in
Biostrings objects.  The sequences are the same as in
BSgenome.Dmelanogaster.UCSC.dm3, except that each of them has the 4 following
masks on top: (1) the mask of assembly gaps (AGAPS mask), (2) the mask of
intra-contig ambiguities (AMB mask), (3) the mask of repeats from
RepeatMasker (RM mask), and (4) the mask of repeats from Tandem Repeats
Finder (TRF mask).  Only the AGAPS and AMB masks are \"active\" by default.")
    (license license:artistic2.0)))

(define-public r-bsgenome-drerio-ucsc-danrer11
  (package
    (name "r-bsgenome-drerio-ucsc-danrer11")
    (version "1.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BSgenome.Drerio.UCSC.danRer11"
                              version 'annotation))
       (sha256
        (base32 "08a928mqzv2jxngjcs4yr6ni1b9z9al6jdngwi438j8hm41cwk4v"))))
    (properties `((upstream-name . "BSgenome.Drerio.UCSC.danRer11")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-bsgenome))
    (home-page "https://bioconductor.org/packages/BSgenome.Drerio.UCSC.danRer11")
    (synopsis "Full genome sequences for Danio rerio (UCSC version danRer11)")
    (description
     "This package provides full genome sequences for Danio rerio (Zebrafish)
as provided by UCSC (danRer11, May 2017) and stored in Biostrings objects.")
    (license license:artistic2.0)))

(define-public r-bsgenome-ecoli-ncbi-20080805
  (package
    (name "r-bsgenome-ecoli-ncbi-20080805")
    (version "1.3.1000")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BSgenome.Ecoli.NCBI.20080805" version
                              'annotation))
       (sha256
        (base32 "1l7mjyys1kaq4mbia9jamyw6sd0ij1wypwxvwy8aksan3gcfnh27"))))
    (properties `((upstream-name . "BSgenome.Ecoli.NCBI.20080805")))
    (build-system r-build-system)
    (propagated-inputs (list r-bsgenome))
    (home-page
     "https://bioconductor.org/packages/BSgenome.Ecoli.NCBI.20080805")
    (synopsis "Escherichia coli full genomes")
    (description
     "This package provides Escherichia coli full genomes for several strains
as provided by NCBI on 2008/08/05 and stored in Biostrings objects.")
    (license license:artistic2.0)))

(define-public r-bsgenome-hsapiens-1000genomes-hs37d5
  (package
    (name "r-bsgenome-hsapiens-1000genomes-hs37d5")
    (version "0.99.1")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "BSgenome.Hsapiens.1000genomes.hs37d5"
                                     version 'annotation))
              (sha256
               (base32
                "1cg0g5fqmsvwyw2p9hp2yy4ilk21jkbbrnpgqvb5c36ihjwvc7sr"))))
    (properties
     `((upstream-name . "BSgenome.Hsapiens.1000genomes.hs37d5")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-bsgenome))
    (home-page
     "https://www.bioconductor.org/packages/BSgenome.Hsapiens.1000genomes.hs37d5/")
    (synopsis "Full genome sequences for Homo sapiens")
    (description
     "This package provides full genome sequences for Homo sapiens from
1000genomes phase2 reference genome sequence (hs37d5), based on NCBI GRCh37.")
    (license license:artistic2.0)))

(define-public r-bsgenome-hsapiens-ncbi-grch38
  (package
    (name "r-bsgenome-hsapiens-ncbi-grch38")
    (version "1.3.1000")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BSgenome.Hsapiens.NCBI.GRCh38"
                              version 'annotation))
       (sha256
        (base32
         "0y75qdq578fh6420vbvsbwmdw8jvr3g06qli2h3vj3pxmjykh9c1"))))
    (properties `((upstream-name . "BSgenome.Hsapiens.NCBI.GRCh38")))
    (build-system r-build-system)
    (propagated-inputs (list r-bsgenome))
    (home-page
     "https://bioconductor.org/packages/release/data/annotation/html/\
BSgenome.Hsapiens.NCBI.GRCh38.html")
    (synopsis "Full genome sequences for Homo sapiens (GRCh38)")
    (description
     "This package provides full genome sequences for Homo sapiens (Human) as
provided by NCBI (GRCh38, 2013-12-17) and stored in Biostrings objects.")
    (license license:artistic2.0)))

(define-public r-bsgenome-hsapiens-ucsc-hg19-masked
  (package
    (name "r-bsgenome-hsapiens-ucsc-hg19-masked")
    (version "1.3.993")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BSgenome.Hsapiens.UCSC.hg19.masked"
                              version 'annotation))
       (sha256
        (base32 "19533ihgad67bzhavycv6z708012ylz9cw1qdfmk2b7ikf3kiaz9"))))
    (properties
     `((upstream-name . "BSgenome.Hsapiens.UCSC.hg19.masked")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-bsgenome r-bsgenome-hsapiens-ucsc-hg19))
    (home-page "https://bioconductor.org/packages/BSgenome.Hsapiens.UCSC.hg19.masked/")
    (synopsis "Full masked genome sequences for Homo sapiens")
    (description
     "This package provides full genome sequences for Homo sapiens (Human) as
provided by UCSC (hg19, Feb. 2009) and stored in Biostrings objects.  The
sequences are the same as in BSgenome.Hsapiens.UCSC.hg19, except that each of
them has the 4 following masks on top: (1) the mask of assembly gaps (AGAPS
mask), (2) the mask of intra-contig ambiguities (AMB mask), (3) the mask of
repeats from RepeatMasker (RM mask), and (4) the mask of repeats from Tandem
Repeats Finder (TRF mask).  Only the AGAPS and AMB masks are \"active\" by
default.")
    (license license:artistic2.0)))

(define-public r-bsgenome-mmusculus-ucsc-mm9
  (package
    (name "r-bsgenome-mmusculus-ucsc-mm9")
    (version "1.4.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "BSgenome.Mmusculus.UCSC.mm9"
                                     version 'annotation))
              (sha256
               (base32
                "1birqw30g2azimxpnjfzmkphan7x131yy8b9h85lfz5fjdg7841i"))))
    (properties
     `((upstream-name . "BSgenome.Mmusculus.UCSC.mm9")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-bsgenome))
    (home-page
     "https://www.bioconductor.org/packages/BSgenome.Mmusculus.UCSC.mm9/")
    (synopsis "Full genome sequences for Mouse")
    (description
     "This package provides full genome sequences for Mus musculus (Mouse) as
provided by UCSC (mm9, July 2007) and stored in Biostrings objects.")
    (license license:artistic2.0)))

(define-public r-bsgenome-mmusculus-ucsc-mm9-masked
  (package
    (name "r-bsgenome-mmusculus-ucsc-mm9-masked")
    (version "1.3.99")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "BSgenome.Mmusculus.UCSC.mm9.masked"
                                     version 'annotation))
              (sha256
               (base32
                "00bpbm3havqcxr4g63zhllsbpd9q6svgihks7qp7x73nm4gvq7fn"))))
    (properties
     `((upstream-name . "BSgenome.Mmusculus.UCSC.mm9.masked")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-bsgenome r-bsgenome-mmusculus-ucsc-mm9))
    (home-page "https://bioconductor.org/packages/BSgenome.Mmusculus.UCSC.mm9.masked/")
    (synopsis "Full masked genome sequences for Mouse")
    (description
     "This package provides full genome sequences for Mus musculus (Mouse) as
provided by UCSC (mm9, Jul. 2007) and stored in Biostrings objects.  The
sequences are the same as in BSgenome.Mmusculus.UCSC.mm9, except that each of
them has the 4 following masks on top: (1) the mask of assembly gaps (AGAPS
mask), (2) the mask of intra-contig ambiguities (AMB mask), (3) the mask of
repeats from RepeatMasker (RM mask), and (4) the mask of repeats from Tandem
Repeats Finder (TRF mask).  Only the AGAPS and AMB masks are \"active\" by
default."  )
    (license license:artistic2.0)))

(define-public r-bsgenome-mmusculus-ucsc-mm10
  (package
    (name "r-bsgenome-mmusculus-ucsc-mm10")
    (version "1.4.3")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "BSgenome.Mmusculus.UCSC.mm10"
                                     version 'annotation))
              (sha256
               (base32
                "1vprkywr72nqf847vzmgymylcdb45g4hy30fwx6fzwddkyzh5xnw"))))
    (properties
     `((upstream-name . "BSgenome.Mmusculus.UCSC.mm10")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-bsgenome))
    (home-page
     "https://www.bioconductor.org/packages/BSgenome.Mmusculus.UCSC.mm10/")
    (synopsis "Full genome sequences for Mouse")
    (description
     "This package provides full genome sequences for Mus
musculus (Mouse) as provided by UCSC (mm10, December 2011) and stored
in Biostrings objects.")
    (license license:artistic2.0)))

(define-public r-genomeinfodbdata
  (package
    (name "r-genomeinfodbdata")
    (version "1.2.12")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "GenomeInfoDbData" version 'annotation))
              (sha256
               (base32
                "0md3jl5bz4d2wkvf4yra7rydf5dgykrijrdqwwxc7f77l30nsmgp"))))
    (properties
     `((upstream-name . "GenomeInfoDbData")))
    (build-system r-build-system)
    (home-page "https://bioconductor.org/packages/GenomeInfoDbData")
    (synopsis "Species and taxonomy ID look up tables for GenomeInfoDb")
    (description "This package contains data for mapping between NCBI taxonomy
ID and species.  It is used by functions in the GenomeInfoDb package.")
    (license license:artistic2.0)))

(define-public r-go-db
  (package
    (name "r-go-db")
    (version "3.19.1")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "GO.db" version 'annotation))
              (sha256
               (base32
                "199vk4y069yik7fn25pp73nkhs1vaw9km9rcmm1cirncwm2n0vhg"))))
    (properties
     `((upstream-name . "GO.db")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotationdbi))
    (home-page "https://bioconductor.org/packages/GO.db")
    (synopsis "Annotation maps describing the entire Gene Ontology")
    (description
     "The purpose of this GO.db annotation package is to provide detailed
information about the latest version of the Gene Ontologies.")
    (license license:artistic2.0)))

(define-public r-hdo-db
  (package
    (name "r-hdo-db")
    (version "0.99.1")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "HDO.db" version 'annotation))
              (sha256
               (base32
                "14ngyxailmxrbxqqi9m7mchqcvchmbg7zm34i8a927b20s6z4z61"))))
    (properties `((upstream-name . "HDO.db")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotationdbi))
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/HDO.db")
    (synopsis "Annotation maps describing the entire Human Disease Ontology")
    (description
     "This package provides a set of annotation maps describing the entire
Human Disease Ontology.  The annotation data comes from
@url{Humam Disease Ontology repository,
https://github.com/DiseaseOntology/HumanDiseaseOntology/tree/main/src/ontology}.")
    (license license:artistic2.0)))

(define-public r-homo-sapiens
  (package
    (name "r-homo-sapiens")
    (version "1.3.1")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "Homo.sapiens" version 'annotation))
              (sha256
               (base32
                "151vj7h5p1c8yd5swrchk46z469p135wk50hvkl0nhgndvy0jj01"))))
    (properties
     `((upstream-name . "Homo.sapiens")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-genomicfeatures
           r-go-db
           r-org-hs-eg-db
           r-txdb-hsapiens-ucsc-hg19-knowngene
           r-organismdbi
           r-annotationdbi))
    (home-page "https://bioconductor.org/packages/Homo.sapiens/")
    (synopsis "Annotation package for the Homo.sapiens object")
    (description
     "This package contains the Homo.sapiens object to access data from
several related annotation packages.")
    (license license:artistic2.0)))

(define-public r-mus-musculus
  (package
    (name "r-mus-musculus")
    (version "1.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Mus.musculus" version 'annotation))
       (sha256
        (base32
         "143zdf83gbfqhy8jm9df7gzhw5q3a64jrjrxrzjf0zd76j8s8j6y"))))
    (properties `((upstream-name . "Mus.musculus")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotationdbi
           r-genomicfeatures
           r-go-db
           r-org-mm-eg-db
           r-organismdbi
           r-txdb-mmusculus-ucsc-mm10-knowngene))
    (home-page "https://bioconductor.org/packages/Mus.musculus")
    (synopsis "Annotation package for the Mus.musculus object")
    (description
     "This package contains the @code{Mus.musculus} object to access data
from several related annotation packages.")
    (license license:artistic2.0)))

(define-public r-illuminahumanmethylation450kanno-ilmn12-hg19
  (package
    (name "r-illuminahumanmethylation450kanno-ilmn12-hg19")
    (version "0.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri
             "IlluminaHumanMethylation450kanno.ilmn12.hg19"
             version 'annotation))
       (sha256
        (base32
         "1272n72fvj4agszd9cv8l9h9qr2pzmd0rbvdz83x7x03cdddf9rn"))))
    (properties
     `((upstream-name
        . "IlluminaHumanMethylation450kanno.ilmn12.hg19")))
    (build-system r-build-system)
    (propagated-inputs (list r-minfi))
    (home-page
     "https://bioconductor.org/packages/IlluminaHumanMethylation450kanno.ilmn12.hg19/")
    (synopsis "Annotation for Illumina's 450k methylation arrays")
    (description
     "This package provides manifests and annotation for Illumina's 450k array
data.")
    (license license:artistic2.0)))

(define-public r-illuminahumanmethylation450kmanifest
  (package
    (name "r-illuminahumanmethylation450kmanifest")
    (version "0.4.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri
                    "IlluminaHumanMethylation450kmanifest"
                    version 'annotation))
              (sha256
               (base32
                "0qx75xwifrbkqmbkd8dhf44c34ibmbivqh7y8rvgrsizmi5ybcj1"))))
    (properties `((upstream-name . "IlluminaHumanMethylation450kmanifest")))
    (build-system r-build-system)
    (propagated-inputs (list r-minfi))
    (home-page
     "https://bioconductor.org/packages/IlluminaHumanMethylation450kmanifest")
    (synopsis "Annotation for Illumina's 450k methylation arrays")
    (description "This package provides a manifest for Illumina's 450k array
data.")
    (license license:artistic2.0)))

(define-public r-illuminahumanmethylationepicanno-ilm10b4-hg19
  (package
    (name "r-illuminahumanmethylationepicanno-ilm10b4-hg19")
    (version "0.6.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri
                    "IlluminaHumanMethylationEPICanno.ilm10b4.hg19"
                    version 'annotation))
              (sha256
               (base32
                "0687b4k8hwfc18qgdd9ypv1skp37jd204fszba0gmrv3dc92i09c"))))
    (properties `((upstream-name . "IlluminaHumanMethylationEPICanno.ilm10b4.hg19")))
    (build-system r-build-system)
    (propagated-inputs (list r-minfi))
    (home-page
     "https://doi.org/doi:10.18129/B9.bioc.IlluminaHumanMethylationEPICanno.ilm10b4.hg19")
    (synopsis "Annotation for Illumina's EPIC methylation arrays")
    (description
     "This is an annotation package for Illumina's EPIC methylation arrays.")
    (license license:artistic2.0)))

(define-public r-org-ce-eg-db
  (package
    (name "r-org-ce-eg-db")
    (version "3.19.1")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "org.Ce.eg.db" version 'annotation))
              (sha256
               (base32
                "0vg6nw4qzj8janmkv6k2z471m3l4xnd1h232h0w5i53b20c7p6ys"))))
    (properties
     `((upstream-name . "org.Ce.eg.db")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotationdbi))
    (home-page "https://www.bioconductor.org/packages/org.Ce.eg.db/")
    (synopsis "Genome wide annotation for Worm")
    (description
     "This package provides mappings from Entrez gene identifiers to various
annotations for the genome of the model worm Caenorhabditis elegans.")
    (license license:artistic2.0)))

(define-public r-org-dm-eg-db
  (package
    (name "r-org-dm-eg-db")
    (version "3.19.1")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "org.Dm.eg.db" version 'annotation))
              (sha256
               (base32
                "1sqw38z73sgq66x8qrb5xw0jxyimy7l1hs2iwm7xq9ylzrjw3yj0"))))
    (properties
     `((upstream-name . "org.Dm.eg.db")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotationdbi))
    (home-page "https://www.bioconductor.org/packages/org.Dm.eg.db/")
    (synopsis "Genome wide annotation for Fly")
    (description
     "This package provides mappings from Entrez gene identifiers to various
annotations for the genome of the model fruit fly Drosophila melanogaster.")
    (license license:artistic2.0)))

(define-public r-org-dr-eg-db
  (package
    (name "r-org-dr-eg-db")
    (version "3.19.1")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "org.Dr.eg.db" version 'annotation))
              (sha256
               (base32
                "041hp3xhkiwh8vb4zq5kwvrsmp8paqlri6gaj1qy0a5zjlkw0fl6"))))
    (properties
     `((upstream-name . "org.Dr.eg.db")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotationdbi))
    (home-page "https://www.bioconductor.org/packages/org.Dr.eg.db/")
    (synopsis "Annotation for Zebrafish")
    (description
     "This package provides genome wide annotations for Zebrafish, primarily
based on mapping using Entrez Gene identifiers.")
    (license license:artistic2.0)))

(define-public r-org-hs-eg-db
  (package
    (name "r-org-hs-eg-db")
    (version "3.19.1")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "org.Hs.eg.db" version 'annotation))
              (sha256
               (base32
                "1xrbf0zmik1j70i6bg5wqvfwz4vcf3vgypan36r1lv6lspsfychr"))))
    (properties
     `((upstream-name . "org.Hs.eg.db")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotationdbi))
    (home-page "https://www.bioconductor.org/packages/org.Hs.eg.db/")
    (synopsis "Genome wide annotation for Human")
    (description
     "This package contains genome-wide annotations for Human, primarily based
on mapping using Entrez Gene identifiers.")
    (license license:artistic2.0)))

(define-public r-org-mm-eg-db
  (package
    (name "r-org-mm-eg-db")
    (version "3.19.1")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "org.Mm.eg.db" version 'annotation))
              (sha256
               (base32
                "12d8lrf6bxvqzfxzdbxqmjcywy2myhwhi99kxdwpijsczxxfikj7"))))
    (properties
     `((upstream-name . "org.Mm.eg.db")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotationdbi))
    (home-page "https://www.bioconductor.org/packages/org.Mm.eg.db/")
    (synopsis "Genome wide annotation for Mouse")
    (description
     "This package provides mappings from Entrez gene identifiers to various
annotations for the genome of the model mouse Mus musculus.")
    (license license:artistic2.0)))

(define-public r-bsgenome-hsapiens-ucsc-hg19
  (package
    (name "r-bsgenome-hsapiens-ucsc-hg19")
    (version "1.4.3")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "BSgenome.Hsapiens.UCSC.hg19"
                                     version 'annotation))
              (sha256
               (base32
                "06lx7q7i52lg3vkjkqy492z9ianzgi4nhs9m1jrxjjb4hgbnbyjv"))))
    (properties
     `((upstream-name . "BSgenome.Hsapiens.UCSC.hg19")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-bsgenome))
    (home-page
     "https://www.bioconductor.org/packages/BSgenome.Hsapiens.UCSC.hg19/")
    (synopsis "Full genome sequences for Homo sapiens")
    (description
     "This package provides full genome sequences for Homo sapiens as provided
by UCSC (hg19, February 2009) and stored in Biostrings objects.")
    (license license:artistic2.0)))

(define-public r-bsgenome-hsapiens-ucsc-hg38
  (package
    (name "r-bsgenome-hsapiens-ucsc-hg38")
    (version "1.4.5")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "BSgenome.Hsapiens.UCSC.hg38"
                                     version 'annotation))
              (sha256
               (base32 "0lp94r9dsx8sl1ifysavgjf4aamhjc4n71zif5jyfnwmzpj7g4ml"))))
    (properties
     `((upstream-name . "BSgenome.Hsapiens.UCSC.hg38")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-bsgenome r-genomeinfodb))
    (home-page
     "https://www.bioconductor.org/packages/BSgenome.Hsapiens.UCSC.hg38/")
    (synopsis "Full genome sequences for Homo sapiens")
    (description
     "This package provides full genome sequences for Homo sapiens (Human)
as provided by UCSC (hg38, Dec. 2013) and stored in Biostrings objects.")
    (license license:artistic2.0)))

(define-public r-ensdb-hsapiens-v75
  (package
    (name "r-ensdb-hsapiens-v75")
    (version "2.99.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "EnsDb.Hsapiens.v75" version 'annotation))
       (sha256
        (base32
         "0jx6rf6v0j8yr07q3c1h7s121901dc400nm6xaiv4i7kb5czjn9c"))))
    (properties
     `((upstream-name . "EnsDb.Hsapiens.v75")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-ensembldb))
    (home-page "https://bioconductor.org/packages/EnsDb.Hsapiens.v75")
    (synopsis "Ensembl based annotation package")
    (description
     "This package exposes an annotation database generated from Ensembl.")
    (license license:artistic2.0)))

(define-public r-ensdb-hsapiens-v86
  (package
    (name "r-ensdb-hsapiens-v86")
    (version "2.99.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "EnsDb.Hsapiens.v86" version 'annotation))
       (sha256
        (base32 "1gp7xrzddpvmh2vrcp571wyy00skxgxfl39ksj4h0hm1qay0fb2m"))))
    (properties `((upstream-name . "EnsDb.Hsapiens.v86")))
    (build-system r-build-system)
    (propagated-inputs (list r-ensembldb))
    (home-page "https://bioconductor.org/packages/EnsDb.Hsapiens.v86")
    (synopsis "Ensembl based annotation package")
    (description "This package exposes an annotation database generated from
Ensembl.")
    (license license:artistic2.0)))

(define-public r-ensdb-mmusculus-v79
  (package
    (name "r-ensdb-mmusculus-v79")
    (version "2.99.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "EnsDb.Mmusculus.v79" version 'annotation))
       (sha256
        (base32 "1zpmq7v55if6q9r0h883q9k8l70ym20b01m9hxf121wb256rl9f7"))))
    (properties `((upstream-name . "EnsDb.Mmusculus.v79")))
    (build-system r-build-system)
    (propagated-inputs (list r-ensembldb))
    (home-page "https://bioconductor.org/packages/EnsDb.Mmusculus.v79")
    (synopsis "Ensembl based annotation package")
    (description "This package exposes an annotation database generated from
Ensembl.")
    (license license:artistic2.0)))

(define-public r-escape
  (package
    (name "r-escape")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "escape" version))
       (sha256
        (base32 "03wbrxd7kjyl3d4s418ql5fv7bmwdfyfp10jd8knr0xis7vxmz2x"))))
    (properties `((upstream-name . "escape")))
    (build-system r-build-system)
    (propagated-inputs (list r-aucell
                             r-biocparallel
                             r-dplyr
                             r-ggdist
                             r-ggplot2
                             r-ggpointdensity
                             r-ggridges
                             r-gseabase
                             r-gsva
                             r-matrixgenerics
                             r-msigdbr
                             r-patchwork
                             r-reshape2
                             r-seuratobject
                             r-singlecellexperiment
                             r-stringr
                             r-summarizedexperiment
                             r-ucell))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/escape")
    (synopsis "Single cell analysis platform for enrichment")
    (description
     "R-escape streamlines gene set enrichment analysis for single-cell RNA
sequencing.  Using raw count information, Seurat objects, or
@code{SingleCellExperiment} format, users can perform and visualize GSEA
across individual cells.")
    (license license:gpl2)))

(define-public r-snplocs-hsapiens-dbsnp144-grch37
  (package
    (name "r-snplocs-hsapiens-dbsnp144-grch37")
    (version "0.99.20")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "SNPlocs.Hsapiens.dbSNP144.GRCh37"
                                     version 'annotation))
              (sha256
               (base32
                "1z8kx43ki1jvj7ms7pcybakcdimfwr6zpjvspkjmma97bdz093iz"))))
    (build-system r-build-system)
    ;; As this package provides little more than a very large data file it
    ;; doesn't make sense to build substitutes.
    (arguments `(#:substitutable? #f))
    (propagated-inputs
     (list r-biocgenerics
           r-s4vectors
           r-iranges
           r-genomeinfodb
           r-genomicranges
           r-bsgenome
           r-biostrings))
    (home-page
     "https://bioconductor.org/packages/SNPlocs.Hsapiens.dbSNP144.GRCh37/")
    (synopsis "SNP locations for Homo sapiens (dbSNP Build 144)")
    (description "This package provides SNP locations and alleles for Homo
sapiens extracted from NCBI dbSNP Build 144.  The source data files used for
this package were created by NCBI on May 29-30, 2015, and contain SNPs mapped
to reference genome GRCh37.p13.  Note that the GRCh37.p13 genome is a
patched version of GRCh37.  However the patch doesn't alter chromosomes 1-22,
X, Y, MT.  GRCh37 itself is the same as the hg19 genome from UCSC *except* for
the mitochondrion chromosome.  Therefore, the SNPs in this package can be
injected in @code{BSgenome.Hsapiens.UCSC.hg19} and they will land at the
correct position but this injection will exclude chrM (i.e. nothing will be
injected in that sequence).")
    (license license:artistic2.0)))

(define-public r-txdb-dmelanogaster-ucsc-dm6-ensgene
  (package
    (name "r-txdb-dmelanogaster-ucsc-dm6-ensgene")
    (version "3.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "TxDb.Dmelanogaster.UCSC.dm6.ensGene"
                              version 'annotation))
       (sha256
        (base32
         "0yij7zyqkmmr13389rs2gfa5anvvw648nnl1kjbsgvyxkggif8q4"))))
    (properties
     `((upstream-name . "TxDb.Dmelanogaster.UCSC.dm6.ensGene")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotationdbi r-genomicfeatures))
    (home-page
     "https://bioconductor.org/packages/TxDb.Dmelanogaster.UCSC.dm6.ensGene")
    (synopsis "Annotation package for TxDb object(s)")
    (description
     "This package exposes an annotation databases generated from UCSC by
exposing these as TxDb objects.")
    (license license:artistic2.0)))

(define-public r-txdb-hsapiens-ucsc-hg19-knowngene
  (package
    (name "r-txdb-hsapiens-ucsc-hg19-knowngene")
    (version "3.2.2")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "TxDb.Hsapiens.UCSC.hg19.knownGene"
                                     version 'annotation))
              (sha256
               (base32
                "1sajhcqqwazgz2lqbik7rd935i7kpnh08zxbp2ra10j72yqy4g86"))))
    (properties
     `((upstream-name . "TxDb.Hsapiens.UCSC.hg19.knownGene")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-genomicfeatures))
    (home-page
     "https://bioconductor.org/packages/TxDb.Hsapiens.UCSC.hg19.knownGene/")
    (synopsis "Annotation package for human genome in TxDb format")
    (description
     "This package provides an annotation database of Homo sapiens genome
data.  It is derived from the UCSC hg19 genome and based on the \"knownGene\"
track.  The database is exposed as a @code{TxDb} object.")
    (license license:artistic2.0)))

(define-public r-txdb-hsapiens-ucsc-hg38-knowngene
  (package
    (name "r-txdb-hsapiens-ucsc-hg38-knowngene")
    (version "3.18.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "TxDb.Hsapiens.UCSC.hg38.knownGene"
                                     version 'annotation))
              (sha256
               (base32 "0ak6acjll2djamakpn60ks47lfs2y0r8xwhjazj6fvch34xkc2d5"))))
    (properties
     `((upstream-name . "TxDb.Hsapiens.UCSC.hg38.knownGene")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotationdbi r-genomicfeatures))
    (home-page
     "https://bioconductor.org/packages/TxDb.Hsapiens.UCSC.hg38.knownGene/")
    (synopsis "Annotation package for human genome in TxDb format")
    (description
     "This package provides an annotation database of Homo sapiens genome
data.  It is derived from the UCSC hg38 genome and based on the \"knownGene\"
track.  The database is exposed as a @code{TxDb} object.")
    (license license:artistic2.0)))

(define-public r-txdb-mmusculus-ucsc-mm9-knowngene
  (package
    (name "r-txdb-mmusculus-ucsc-mm9-knowngene")
    (version "3.2.2")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "TxDb.Mmusculus.UCSC.mm9.knownGene"
                                     version 'annotation))
              (sha256
               (base32
                "16bjxy00363hf91ik2mqlqls86i07gia72qh92xc3l1ncch61mx2"))))
    (properties
     `((upstream-name . "TxDb.Mmusculus.UCSC.mm9.knownGene")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-genomicfeatures r-annotationdbi))
    (home-page
     "https://bioconductor.org/packages/TxDb.Mmusculus.UCSC.mm9.knownGene/")
    (synopsis "Annotation package for mouse genome in TxDb format")
    (description
     "This package provides an annotation database of Mouse genome data.  It
is derived from the UCSC mm9 genome and based on the \"knownGene\" track.  The
database is exposed as a @code{TxDb} object.")
    (license license:artistic2.0)))

(define-public r-txdb-mmusculus-ucsc-mm10-knowngene
  (package
    (name "r-txdb-mmusculus-ucsc-mm10-knowngene")
    (version "3.10.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "TxDb.Mmusculus.UCSC.mm10.knownGene"
                                     version 'annotation))
              (sha256
               (base32
                "0xs9npnhbwll7p62hibs02y4ac23jchdcr25i6a7qwq1kms82qk9"))))
    (properties
     `((upstream-name . "TxDb.Mmusculus.UCSC.mm10.knownGene")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-bsgenome r-genomicfeatures r-annotationdbi))
    (home-page
     "https://bioconductor.org/packages/TxDb.Mmusculus.UCSC.mm10.knownGene/")
    (synopsis "Annotation package for TxDb knownGene object(s) for Mouse")
    (description
     "This package loads a TxDb object, which is an R interface to
prefabricated databases contained in this package.  This package provides
the TxDb object of Mouse data as provided by UCSC (mm10, December 2011)
based on the knownGene track.")
    (license license:artistic2.0)))

(define-public r-txdb-celegans-ucsc-ce6-ensgene
  (package
    (name "r-txdb-celegans-ucsc-ce6-ensgene")
    (version "3.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "TxDb.Celegans.UCSC.ce6.ensGene"
                              version 'annotation))
       (sha256
        (base32
         "1sgppva33cdy4isj2is8mfalj5gmmkpbkq9w1d83a4agcq31mi90"))))
    (properties
     `((upstream-name . "TxDb.Celegans.UCSC.ce6.ensGene")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotationdbi r-genomicfeatures))
    (home-page "https://bioconductor.org/packages/TxDb.Celegans.UCSC.ce6.ensGene/")
    (synopsis "Annotation package for C elegans TxDb objects")
    (description
     "This package exposes a C elegans annotation database generated from UCSC
by exposing these as TxDb objects.")
    (license license:artistic2.0)))

(define-public r-fdb-infiniummethylation-hg19
  (package
    (name "r-fdb-infiniummethylation-hg19")
    (version "2.2.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "FDb.InfiniumMethylation.hg19"
                                     version 'annotation))
              (sha256
               (base32
                "0gq90fvph6kgrpjb89nvzq6hl1k24swn19rgjh5g98l86mja6nk0"))))
    (properties
     `((upstream-name . "FDb.InfiniumMethylation.hg19")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biostrings r-genomicfeatures r-annotationdbi r-org-hs-eg-db
           r-txdb-hsapiens-ucsc-hg19-knowngene))
    (home-page "https://bioconductor.org/packages/FDb.InfiniumMethylation.hg19/")
    (synopsis "Compiled HumanMethylation27 and HumanMethylation450 annotations")
    (description
     "This is an annotation package for Illumina Infinium DNA methylation
probes.  It contains the compiled HumanMethylation27 and HumanMethylation450
annotations.")
    (license license:artistic2.0)))

(define-public r-illuminahumanmethylationepicmanifest
  (package
    (name "r-illuminahumanmethylationepicmanifest")
    (version "0.3.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "IlluminaHumanMethylationEPICmanifest"
                                     version 'annotation))
              (sha256
               (base32
                "0alhjda5g186z8b1nsmnpfswrlj7prdz8mkwx60wkkl6hkcnk6p3"))))
    (properties
     `((upstream-name . "IlluminaHumanMethylationEPICmanifest")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-minfi))
    (home-page "https://bioconductor.org/packages/IlluminaHumanMethylationEPICmanifest/")
    (synopsis "Manifest for Illumina's EPIC methylation arrays")
    (description
     "This is a manifest package for Illumina's EPIC methylation arrays.")
    (license license:artistic2.0)))

(define-public r-do-db
  (package
    (name "r-do-db")
    (version "2.9")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "DO.db" version 'annotation))
              (sha256
               (base32
                "10bqqa124l61ivzy4mdd3z3ar9a6537qbxw23pc4y9w8a6dwnavn"))))
    (properties
     `((upstream-name . "DO.db")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotationdbi))
    (home-page "https://www.bioconductor.org/packages/DO.db/")
    (synopsis "Annotation maps describing the entire Disease Ontology")
    (description
     "This package provides a set of annotation maps describing the entire
Disease Ontology.")
    (license license:artistic2.0)))

(define-public r-hgu133plus2-db
  (package
    (name "r-hgu133plus2-db")
    (version "3.13.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "hgu133plus2.db" version 'annotation))
       (sha256
        (base32 "0i6cfk7ahql4fcgrq0dai9gkjbsahyzd9iv4lqv1ad58fzkmipnx"))))
    (properties `((upstream-name . "hgu133plus2.db")))
    (build-system r-build-system)
    (propagated-inputs (list r-annotationdbi r-org-hs-eg-db))
    (home-page "https://bioconductor.org/packages/hgu133plus2.db")
    (synopsis "Affymetrix Affymetrix HG-U133_Plus_2 Array annotation data")
    (description
     "This package provides Affymetrix HG-U133_Plus_2 array annotation
data (chip hgu133plus2) assembled using data from public repositories.")
    (license license:artistic2.0)))

(define-public r-pfam-db
  (package
    (name "r-pfam-db")
    (version "3.19.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "PFAM.db" version 'annotation))
       (sha256
        (base32 "0jjba22p8x4cfnihsa3gah3amdr4yrqpfm0zfr990jcjkvf94wvk"))))
    (properties `((upstream-name . "PFAM.db")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotationdbi))
    (home-page "https://bioconductor.org/packages/PFAM.db")
    (synopsis "Set of protein ID mappings for PFAM")
    (description
     "This package provides a set of protein ID mappings for PFAM, assembled
using data from public repositories.")
    (license license:artistic2.0)))

(define-public r-phastcons100way-ucsc-hg19
  (package
    (name "r-phastcons100way-ucsc-hg19")
    (version "3.7.2")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "phastCons100way.UCSC.hg19"
                              version 'annotation))
       (sha256
        (base32
         "1jmc4k4zgkx5vr2plnidnd9bidlwlb0kr7mjg60cqjw7dq7jl1fa"))))
    (properties
     `((upstream-name . "phastCons100way.UCSC.hg19")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-bsgenome
           r-genomeinfodb
           r-genomicranges
           r-genomicscores
           r-iranges
           r-s4vectors))
    (home-page "https://bioconductor.org/packages/phastCons100way.UCSC.hg19")
    (synopsis "UCSC phastCons conservation scores for hg19")
    (description
     "This package provides UCSC phastCons conservation scores for the human
genome (hg19) calculated from multiple alignments with other 99 vertebrate
species.")
    (license license:artistic2.0)))


;;; Experiment data

(define-public r-abadata
  (package
    (name "r-abadata")
    (version "1.12.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "ABAData" version 'experiment))
              (sha256
               (base32
                "1bmj341xcymlrk02gss5vvrqc4ddas0rdw39lnpsj98hq6n11p5z"))))
    (properties
     `((upstream-name . "ABAData")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotationdbi))
    (home-page "https://www.bioconductor.org/packages/ABAData/")
    (synopsis "Gene expression in human brain regions from Allen Brain Atlas")
    (description
     "This package provides the data for the gene expression enrichment
analysis conducted in the package ABAEnrichment.  The package includes three
datasets which are derived from the Allen Brain Atlas:

@enumerate
@item Gene expression data from Human Brain (adults) averaged across donors,
@item Gene expression data from the Developing Human Brain pooled into five
  age categories and averaged across donors, and
@item a developmental effect score based on the Developing Human Brain
  expression data.
@end enumerate

All datasets are restricted to protein coding genes.")
    (license license:gpl2+)))

(define-public r-adductdata
  (package
    (name "r-adductdata")
    (version "1.20.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "adductData" version 'experiment))
              (sha256
               (base32 "1ydd7zxag0cmw7mj38srqs4w6lbp1ic5i8s1ay6pifv14arcncrz"))))
    (properties `((upstream-name . "adductData")))
    (build-system r-build-system)
    (propagated-inputs (list r-annotationhub r-experimenthub))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/adductData")
    (synopsis "Data from untargeted mass spectrometry of modifications to Cys34")
    (description
     "This package contains data from untargeted @dfn{mass spectrometry} (MS)
of modifications to @dfn{oxidized cysteine} (Cys) 34 in @dfn{human serum
albumin} (HSA).")
    (license license:artistic2.0)))

(define-public r-aneufinderdata
  (package
   (name "r-aneufinderdata")
   (version "1.32.0")
   (source (origin
            (method url-fetch)
            (uri (bioconductor-uri "AneuFinderData" version 'experiment))
            (sha256
             (base32 "1qysi874hsa4a7xz0na0cxv0sx02d3b74h77s30mfww1wsg8jy69"))))
   (build-system r-build-system)
   (home-page "https://bioconductor.org/packages/AneuFinderData/")
   (synopsis "Data package for @code{AneuFinder}")
   (description "This package contains whole-genome single cell sequencing data for
demonstration purposes in the @code{AneuFinder} package.")
   (license license:artistic2.0)))

(define-public r-arrmdata
  (package
    (name "r-arrmdata")
    (version "1.40.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "ARRmData" version 'experiment))
              (sha256
               (base32 "02y44d0dappc3srl2d7lgipm6a62gvd8vivzaq4594dnmy8rvkw3"))))
    (properties
     `((upstream-name . "ARRmData")))
    (build-system r-build-system)
    (home-page "https://www.bioconductor.org/packages/ARRmData/")
    (synopsis "Example dataset for normalization of Illumina 450k methylation data")
    (description
     "This package provides raw beta values from 36 samples across 3 groups
from Illumina 450k methylation arrays.")
    (license license:artistic2.0)))

(define-public r-bcellviper
  (package
    (name "r-bcellviper")
    (version "1.40.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "bcellViper" version
                                     'experiment))
              (sha256
               (base32
                "0mx92gs9065lz0c01mzng9snalvmdb3xbsm1r3f6fnhg478da4yq"))))
    (properties `((upstream-name . "bcellViper")))
    (build-system r-build-system)
    (propagated-inputs (list r-biobase))
    (home-page "https://bioconductor.org/packages/bcellViper")
    (synopsis
     "Transcriptional interactome and normal human B-cell expression data")
    (description
     "This is a tool for human B-cell context-specific transcriptional
regulatory network.  In addition, this package provides a human normal B-cells
dataset for the examples in package viper.")
    (license license:gpl2+)))

(define-public r-bladderbatch
  (package
    (name "r-bladderbatch")
    (version "1.42.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "bladderbatch" version
                                     'experiment))
              (sha256
               (base32
                "00cgkpa6mb32q3xl5clkpdh5rkwx7c81rxmyh2qr0jpwpmd3gjwc"))))
    (properties `((upstream-name . "bladderbatch")))
    (build-system r-build-system)
    (propagated-inputs (list r-biobase))
    (home-page "https://bioconductor.org/packages/bladderbatch")
    (synopsis "Bladder gene expression data illustrating batch effects")
    (description
     "This package contains microarray gene expression data on 57 bladder samples from
5 batches.  The data are used as an illustrative example for the sva package.")
    (license license:artistic2.0)))

(define-public r-bodymaprat
  (package
    (name "r-bodymaprat")
    (version "1.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "bodymapRat" version
                              'experiment))
       (sha256
        (base32 "15kjgqq7lq94pjqg380swqrkv684kgbpkaikfxxdx4cpgx8sjzn7"))))
    (properties `((upstream-name . "bodymapRat")))
    (build-system r-build-system)
    (arguments
     (list
      #:phases
      '(modify-phases %standard-phases
         (add-after 'unpack 'set-HOME
           (lambda _
             (setenv "HOME" "/tmp")))
         (add-after 'unpack 'avoid-internet-access
           (lambda _
             (setenv "GUIX_BUILD" "yes")
             (substitute* "R/zzz.R"
               (("createHubAccessors.*" m)
                (string-append
                 "if (Sys.getenv(\"GUIX_BUILD\") == \"\") {" m "}"))))))))
    (propagated-inputs (list r-experimenthub r-summarizedexperiment))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/bodymapRat")
    (synopsis "Experimental dataset from the rat BodyMap project")
    (description
     "This package contains a @code{SummarizedExperiment} from the Yu et
al. (2013) paper that performed the rat @code{BodyMap} across 11 organs and 4
developmental stages.  Raw FASTQ files were downloaded and mapped using
STAR. Data is available on @code{ExperimentHub} as a data package.")
    (license license:cc-by4.0)))

(define-public r-biscuiteerdata
  (package
    (name "r-biscuiteerdata")
    (version "1.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "biscuiteerData" version 'experiment))
       (sha256
        (base32 "1qv2ajy312diacb3mihc3q7hrrlq48k5vd9v0g6w38cqz6gnz0as"))))
    (properties
     `((upstream-name . "biscuiteerData")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotationhub r-curl r-experimenthub r-genomicranges))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/biscuiteerData")
    (synopsis "Data package for Biscuiteer")
    (description
     "This package contains default datasets used by the Bioconductor package
biscuiteer.")
    (license license:gpl3)))

(define-public r-breakpointrdata
  (package
    (name "r-breakpointrdata")
    (version "1.22.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "breakpointRdata" version 'experiment))
              (sha256
               (base32
                "1hm190ji3s451x2hchmpyzx6v4n8p1n6fdqg7cbzmj19rzsb4wha"))))
    (properties `((upstream-name . "breakpointRdata")))
    (build-system r-build-system)
    (native-inputs (list r-knitr))
    (home-page "https://github.com/daewoooo/breakpointRdata")
    (synopsis "Strand-seq data for demonstration purposes")
    (description
     "This package is a collection of Strand-seq data.  The main purpose is to
demonstrate functionalities of the @code{breakpointR} package.")
    (license license:expat)))

(define-public r-breastcancervdx
  (package
    (name "r-breastcancervdx")
    (version "1.42.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "breastCancerVDX" version
                              'experiment))
       (sha256
        (base32 "0mvm2a77mss85vi8x3i35d54z70advyshsmadhl6aysrzfiw5ifd"))))
    (properties `((upstream-name . "breastCancerVDX")))
    (build-system r-build-system)
    (home-page "https://bioconductor.org/packages/breastCancerVDX")
    (synopsis "Gene expression datasets")
    (description
     "This package is a collection of gene expression data from a breast
cancer study published in Wang et al. 2005 and Minn et al 2007.")
    (license license:artistic2.0)))

(define-public r-celldex
  (package
    (name "r-celldex")
    (version "1.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "celldex" version 'experiment))
       (sha256
        (base32 "155hnh62ra06b52xk78f111b10gm34d1djvl5w5mabvrk4vknwj6"))))
    (properties `((upstream-name . "celldex")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-alabaster-base
           r-alabaster-matrix
           r-alabaster-se
           r-annotationdbi
           r-annotationhub
           r-dbi
           r-delayedarray
           r-delayedmatrixstats
           r-experimenthub
           r-gypsum
           r-jsonlite
           r-matrix
           r-rsqlite
           r-s4vectors
           r-summarizedexperiment))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/LTLA/celldex")
    (synopsis "Reference index for cell types")
    (description
     "This package provides a collection of reference expression datasets with
curated cell type labels, for use in procedures like automated annotation of
single-cell data or deconvolution of bulk RNA-seq.")
    (license license:gpl3)))

(define-public r-champdata
  (package
    (name "r-champdata")
    (version "2.36.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "ChAMPdata" version 'experiment))
              (sha256
               (base32
                "0x006ilgqrf15135ivxv8qrcyhssl1w6pm1gpnna96za58l2f2rr"))))
    (properties `((upstream-name . "ChAMPdata")))
    (build-system r-build-system)
    (propagated-inputs (list r-biocgenerics r-genomicranges))
    (home-page "https://bioconductor.org/packages/ChAMPdata")
    (synopsis "Data packages for ChAMP package")
    (description
     "This package provides datasets needed for ChAMP including a test dataset
and blood controls for CNA analysis.")
    (license license:gpl3)))

(define-public r-chromstardata
  (package
    (name "r-chromstardata")
    (version "1.30.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "chromstaRData" version 'experiment))
       (sha256
        (base32 "0v28zg8z5b72hci7rqbvjvcgp979zxnq6xgck4baypmkj9ac5wn3"))))
    (properties `((upstream-name . "chromstaRData")))
    (build-system r-build-system)
    (home-page "https://bioconductor.org/packages/chromstaRData/")
    (synopsis "ChIP-seq data for demonstration purposes")
    (description
     "This package provides ChIP-seq data for demonstration purposes in the
chromstaR package.")
    (license license:gpl3)))

(define-public r-copyhelper
  (package
    (name "r-copyhelper")
    (version "1.36.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "CopyhelpeR" version 'experiment))
       (sha256
        (base32 "1ijlba601wknkj6ybbnarcdw1z54f76gkdgynpry1v8jpbpvcadr"))))
    (properties `((upstream-name . "CopyhelpeR")))
    (build-system r-build-system)
    (home-page "https://bioconductor.org/packages/CopyhelpeR/")
    (synopsis "Helper files for CopywriteR")
    (description
     "This package contains the helper files that are required to run the
Bioconductor package CopywriteR.  It contains pre-assembled 1kb bin GC-content
and mappability files for the reference genomes hg18, hg19, hg38, mm9 and
mm10.  In addition, it contains a blacklist filter to remove regions that
display copy number variation.  Files are stored as GRanges objects from the
GenomicRanges Bioconductor package.")
    (license license:gpl2)))

(define-public r-flowsorted-blood-450k
  (package
    (name "r-flowsorted-blood-450k")
    (version "1.42.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "FlowSorted.Blood.450k"
                                     version 'experiment))
              (sha256
               (base32
                "0lc45dg5jq0kqycpsggrhi5fa4xj1ciqrj6iw548ccjgdyy2w1xh"))))
    (properties `((upstream-name . "FlowSorted.Blood.450k")))
    (build-system r-build-system)
    (propagated-inputs (list r-minfi))
    (home-page "https://bioconductor.org/packages/FlowSorted.Blood.450k")
    (synopsis
     "Illumina HumanMethylation data on sorted blood cell populations")
    (description
     "This package provides raw data objects for the Illumina 450k DNA
methylation microarrays, and an object depicting which CpGs on the array are
associated with cell type.")
    (license license:artistic2.0)))

(define-public r-flowsorted-blood-epic
  (package
    (name "r-flowsorted-blood-epic")
    (version "2.8.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "FlowSorted.Blood.EPIC" version
                                     'experiment))
              (sha256
               (base32
                "1b0azyslk2g08i6v1vv2niw32f0zqasw85b9klw83zdy2f5dcdvh"))))
    (properties `((upstream-name . "FlowSorted.Blood.EPIC")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotationhub
           r-experimenthub
           r-genefilter
           r-minfi
           r-nlme
           r-quadprog
           r-s4vectors
           r-summarizedexperiment))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/immunomethylomics/FlowSorted.Blood.EPIC")
    (synopsis
     "Illumina EPIC data on immunomagnetic sorted peripheral adult blood cells")
    (description
     "This package provides raw data objects to be used for blood cell
proportion estimation in minfi and similar packages.  The
@code{FlowSorted.Blood.EPIC} object is based in samples assayed by Brock
Christensen and colleagues; for details see Salas et al. 2018.
https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE110554.")
    (license license:gpl3)))

(define-public r-genelendatabase
  (package
    (name "r-genelendatabase")
    (version "1.40.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "geneLenDataBase" version 'experiment))
       (sha256
        (base32 "0c8f0sqgrg7rc2vs9id9xzbwwv3zhy6ylqiisji20rw9ipzgv7k8"))))
    (properties
     `((upstream-name . "geneLenDataBase")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-genomicfeatures r-rtracklayer r-txdbmaker))
    (home-page "https://bioconductor.org/packages/geneLenDataBase/")
    (synopsis "Lengths of mRNA transcripts for a number of genomes")
    (description
     "This package provides the lengths of mRNA transcripts for a number of
genomes and gene ID formats, largely based on the UCSC table browser.")
    (license license:lgpl2.0+)))

(define-public r-genomationdata
  (package
    (name "r-genomationdata")
    (version "1.36.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "genomationData" version 'experiment))
       (sha256
        (base32 "16mghhik2xzin14d7mqlwfy2dah3ccnga947z9id44vyz906wws7"))))
    (properties
     `((upstream-name . "genomationData")))
    (build-system r-build-system)
    ;; As this package provides little more than large data files, it doesn't
    ;; make sense to build substitutes.
    (arguments `(#:substitutable? #f))
    (native-inputs
     (list r-knitr))
    (home-page "https://bioinformatics.mdc-berlin.de/genomation/")
    (synopsis "Experimental data for use with the genomation package")
    (description
     "This package contains experimental genetic data for use with the
genomation package.  Included are Chip Seq, Methylation and Cage data,
downloaded from Encode.")
    (license license:gpl3+)))

(define-public r-hdcytodata
  (package
    (name "r-hdcytodata")
    (version "1.24.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "HDCytoData" version 'experiment))
              (sha256
               (base32
                "0pmzgp7x1zam3b7agi8mdq14qmx8nln9lnhiglax2pvg2a5n4ii4"))))
    (properties `((upstream-name . "HDCytoData")))
    (build-system r-build-system)
    (arguments
     (list
      #:phases
      '(modify-phases %standard-phases
         (add-after 'unpack 'set-HOME
           (lambda _
             (setenv "HOME" "/tmp")))
         (add-after 'unpack 'avoid-internet-access
           (lambda _
             (setenv "GUIX_BUILD" "yes")
             (substitute* "R/zzz.R"
               (("createHubAccessors.*" m)
                (string-append
                 "if (Sys.getenv(\"GUIX_BUILD\") == \"\") {" m "}"))))))))
    (propagated-inputs
     (list r-experimenthub r-flowcore r-summarizedexperiment))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/lmweber/HDCytoData")
    (synopsis
     "Set of high-dimensional flow cytometry and mass cytometry benchmark datasets")
    (description
     "HDCytoData contains a set of high-dimensional cytometry benchmark
datasets.  These datasets are formatted into SummarizedExperiment and flowSet
Bioconductor object formats, including all required metadata.  Row metadata
includes sample IDs, group IDs, patient IDs, reference cell population or
cluster labels and labels identifying spiked in cells.  Column metadata
includes channel names, protein marker names, and protein marker classes.")
    (license license:expat)))

(define-public r-illumina450probevariants-db
  (package
    (name "r-illumina450probevariants-db")
    (version "1.40.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "Illumina450ProbeVariants.db"
                                     version 'experiment))
              (sha256
               (base32
                "1jpknhp624753rhalf81kvl02k4wj19xpncagzf85c878953vph0"))))
    (properties `((upstream-name . "Illumina450ProbeVariants.db")))
    (build-system r-build-system)
    (home-page "https://bioconductor.org/packages/Illumina450ProbeVariants.db")
    (synopsis
     "Variant data from 1000 Genomes Project for Illumina HumanMethylation450 Bead Chip probes")
    (description
     "This package includes details on variants for each probe on the 450k
bead chip for each of the four populations (Asian, American, African and
European).")
    (license license:gpl3)))

(define-public r-italicsdata
  (package
    (name "r-italicsdata")
    (version "2.42.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ITALICSData" version 'experiment))
       (sha256
        (base32 "1d0awis142kgx9hi0dggl6gqg17m0awqzn8ncpqh2lwaka372dq8"))))
    (properties `((upstream-name . "ITALICSData")))
    (build-system r-build-system)
    (home-page "http://bioinfo.curie.fr")
    (synopsis "ITALICS data")
    (description "This package provides data needed to use the ITALICS
package.")
    ;; Expanded from GPL
    (license (list license:gpl2+ license:gpl3+))))

(define-public r-jaspar2016
  (package
    (name "r-jaspar2016")
    (version "1.32.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "JASPAR2016" version 'experiment))
       (sha256
        (base32 "09q5m71drbp64n26plr0a9bz71akcs4g7pqqyjmsmjrzi40kx7hr"))))
    (properties `((upstream-name . "JASPAR2016")))
    (build-system r-build-system)
    (home-page "https://jaspar.elixir.no/")
    (synopsis "Data package for JASPAR 2016")
    (description
     "This is a data package for JASPAR 2016.  To search this databases,
please use the package TFBSTools.")
    (license license:gpl2)))

(define-public r-macrophage
  (package
    (name "r-macrophage")
    (version "1.20.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "macrophage" version
                                     'experiment))
              (sha256
               (base32
                "11ydl4srfxbcqnb42kwk783xd74231g6vgp1f0qs3hl8slc1hk1w"))))
    (properties `((upstream-name . "macrophage")))
    (build-system r-build-system)
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/macrophage")
    (synopsis "Human macrophage immune response data")
    (description
     "This package provides the output of running @code{Salmon} on a set of 24
RNA-seq samples from Alasoo, et al. \"Shared genetic effects on chromatin and
gene expression indicate a role for enhancer priming in immune response\", published
in Nature Genetics, January 2018.")
    (license license:gpl2+)))

;; This is available only in the devel branch of Bioconductor.
(define-public r-memes
  (let ((commit "55f0df1fe65c8ead5252542fdc0da0ff2339049e")
        (revision "1"))
    (package
      (name "r-memes")
      (version (git-version "1.11.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/snystrom/memes")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0mp3a610v293prmyid3w1w8fkjkppjnim60h7p9vz2884d0wdvk4"))))
      (properties `((upstream-name . "memes")))
      (build-system r-build-system)
      (propagated-inputs (list r-biostrings
                               r-cmdfun
                               r-dplyr
                               r-genomicranges
                               r-ggplot2
                               r-ggseqlogo
                               r-magrittr
                               r-matrixstats
                               r-patchwork
                               r-processx
                               r-purrr
                               r-readr
                               r-rlang
                               r-tibble
                               r-tidyr
                               r-universalmotif
                               r-usethis
                               r-xml2))
      (native-inputs (list r-knitr))
      (home-page "https://github.com/snystrom/memes")
      (synopsis
       "Motif matching, comparison, and de novo discovery using the MEME Suite")
      (description
       "This package facilitates motif analysis using MEME Suite tools.
Memes provides data aware utilities for using GRanges objects as
entrypoints to motifanalysis, data structures for examining & editing
motif lists, and novel data visualizations.  Memes functions and data
structures are amenable to both base R and tidyverse workflows.")
      (license license:expat))))

(define-public r-methylclockdata
  (package
    (name "r-methylclockdata")
    (version "1.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "methylclockData" version
                              'experiment))
       (sha256
        (base32 "1bwrgib7k71i4i6iq4l71n0z9adq3bg9k8j10bpi79ygn9169np5"))))
    (properties `((upstream-name . "methylclockData")))
    (build-system r-build-system)
    (propagated-inputs (list r-experimenthub r-experimenthubdata))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/isglobal-brge/methylclockData")
    (synopsis "Data for methylclock package")
    (description
     "This package contains a collection of 9 datasets, andrews and bakulski
cord blood, blood gse35069, blood gse35069 chen, blood gse35069 complete,
combined cord blood, cord bloo d gse68456, gervin and lyle cord blood,
guintivano dlpfc and saliva gse48472.  The data are used to estimate cell
counts using Extrinsic epigenetic age acceleration (EEAA) method.  It also
contains a collection of 12 datasets to use with @code{MethylClock} package to
estimate chronological and gestational DNA methylation with estimators to use
with different methylation clocks.")
    (license license:expat)))

(define-public r-mousegastrulationdata
  (package
    (name "r-mousegastrulationdata")
    (version "1.18.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "MouseGastrulationData" version
                                     'experiment))
              (sha256
               (base32
                "0adw98vym1jir1jrzaws7zrlfvls6rfl8bvkpq5sjy4crb33lxy2"))))
    (properties `((upstream-name . "MouseGastrulationData")))
    (build-system r-build-system)
    (propagated-inputs (list r-biocgenerics
                             r-bumpymatrix
                             r-experimenthub
                             r-s4vectors
                             r-singlecellexperiment
                             r-spatialexperiment
                             r-summarizedexperiment))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/MarioniLab/MouseGastrulationData")
    (synopsis
     "Single-Cell omics data across mouse gastrulation and early organogenesis")
    (description
     "This package provides processed and raw count data for single-cell RNA
sequencing.  In addition, this package offers single-cell ATAC-seq, and
@code{seqFISH} (spatial transcriptomic) experiments performed along a
timecourse of mouse gastrulation and early organogenesis.")
    (license license:gpl3)))

(define-public r-minfidata
  (package
    (name "r-minfidata")
    (version "0.50.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "minfiData" version 'experiment))
              (sha256
               (base32
                "1myxv7cqh7d1rjav9qhwqsmw3s1xh3zm34vbxwprqaqg87dg4bw0"))))
    (properties `((upstream-name . "minfiData")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-illuminahumanmethylation450kanno-ilmn12-hg19
           r-illuminahumanmethylation450kmanifest
           r-minfi))
    (home-page "https://bioconductor.org/packages/minfiData")
    (synopsis "Example data for the Illumina Methylation 450k array")
    (description
     "This package provides data from 6 samples across 2 groups from 450k
methylation arrays.")
    (license license:artistic2.0)))

(define-public r-msdata
  (package
    (name "r-msdata")
    (version "0.44.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "msdata" version 'experiment))
       (sha256
        (base32 "0g7mlgks3nfnfipnybs7pvsnmrvmq8888g39badca8pi73q1cm25"))))
    (properties `((upstream-name . "msdata")))
    (build-system r-build-system)
    (home-page "https://bioconductor.org/packages/msdata")
    (synopsis "Various Mass Spectrometry raw data example files")
    (description
     "This package provides Ion Trap positive ionization mode data in mzML file
format.  It includes a subset from 500-850 m/z and 1190-1310 seconds,
including MS2 and MS3, intensity threshold 100.000; extracts from FTICR Apex
III, m/z 400-450; a subset of UPLC - Bruker micrOTOFq data, both mzML and mz5;
LC-MSMS and MRM files from proteomics experiments; and PSI mzIdentML example
files for various search engines.")
    (license license:gpl2+)))

(define-public r-msexperiment
  (package
    (name "r-msexperiment")
    (version "1.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "MsExperiment" version))
       (sha256
        (base32 "0dxv3jw5aj1i73vqrp3c031pj26i7rp0q2zs4nq3x1vni349506d"))))
    (properties `((upstream-name . "MsExperiment")))
    (build-system r-build-system)
    (propagated-inputs (list r-biocgenerics
                             r-dbi
                             r-iranges
                             r-protgenerics
                             r-qfeatures
                             r-s4vectors
                             r-spectra
                             r-summarizedexperiment))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/RforMassSpectrometry/MsExperiment")
    (synopsis "Infrastructure for Mass Spectrometry experiments")
    (description
     "This package provides infrastructure to store and manage all aspects
related to a complete proteomics or metabolomics mass spectrometry (MS)
experiment.  The @code{MsExperiment} package provides light-weight and
flexible containers for MS experiments building on the new MS infrastructure
provided by the Spectra, QFeatures and related packages.  Along with raw data
representations, links to original data files and sample annotations,
additional metadata or annotations can also be stored within the
@code{MsExperiment} container.  To guarantee maximum flexibility only minimal
constraints are put on the type and content of the data within the
containers.")
    (license license:artistic2.0)))

(define-public r-msigdb
  (package
    (name "r-msigdb")
    (version "1.12.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "msigdb" version
                                     'experiment))
              (sha256
               (base32
                "080wm9sjwqhaxqx1r3kly1bi1gvkssqpsadnvabx11w493356qr0"))))
    (properties `((upstream-name . "msigdb")))
    (build-system r-build-system)
    (propagated-inputs (list r-annotationdbi
                             r-annotationhub
                             r-experimenthub
                             r-gseabase
                             r-org-hs-eg-db
                             r-org-mm-eg-db))
    (native-inputs (list r-knitr))
    (home-page "https://doi.org/doi:10.18129/B9.bioc.msigdb")
    (synopsis "ExperimentHub package for the molecular signatures database")
    (description
     "R-msigdb provides the Molecular Signatures Database in a R accessible
objects.  Signatures are stored in @code{GeneSet} class objects form the
GSEABase package and the entire database is stored in a
@code{GeneSetCollection} object.  These data are then hosted on the
@code{ExperimentHub}.  Data used in this package was obtained from the
@code{MSigDB} of the Broad Institute.  Metadata for each gene set is stored
along with the gene set in the @code{GeneSet} class object.")
    (license license:cc-by4.0)))

(define-public r-pasilla
  (package
    (name "r-pasilla")
    (version "1.32.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "pasilla" version 'experiment))
       (sha256
        (base32 "0r5i64125qq5z4c8zliazsi68035d1bsz6ingzqdh37m8prdw1cr"))))
    (build-system r-build-system)
    (propagated-inputs (list r-dexseq))
    (native-inputs (list r-knitr))
    (home-page "https://www.bioconductor.org/packages/pasilla/")
    (synopsis "Data package with per-exon and per-gene read counts")
    (description "This package provides per-exon and per-gene read counts
computed for selected genes from RNA-seq data that were presented in the
article 'Conservation of an RNA regulatory map between Drosophila and mammals'
by Brooks et al., Genome Research 2011.")
    (license license:lgpl2.1+)))

(define-public r-hsmmsinglecell
  (package
    (name "r-hsmmsinglecell")
    (version "1.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "HSMMSingleCell" version 'experiment))
       (sha256
        (base32 "18p8hk2dq26pp4fvcf1m8wsvj1mn66i8nr900knl2n9vs09j70wy"))))
    (properties
     `((upstream-name . "HSMMSingleCell")))
    (build-system r-build-system)
    (home-page "https://www.bioconductor.org/packages/HSMMSingleCell/")
    (synopsis "Single-cell RNA-Seq for differentiating human skeletal muscle myoblasts (HSMM)")
    (description
     "Skeletal myoblasts undergo a well-characterized sequence of
morphological and transcriptional changes during differentiation.  In this
experiment, primary @dfn{human skeletal muscle myoblasts} (HSMM) were expanded
under high mitogen conditions (GM) and then differentiated by switching to
low-mitogen media (DM).  RNA-Seq libraries were sequenced from each of several
hundred cells taken over a time-course of serum-induced differentiation.
Between 49 and 77 cells were captured at each of four time points (0, 24, 48,
72 hours) following serum switch using the Fluidigm C1 microfluidic system.
RNA from each cell was isolated and used to construct mRNA-Seq libraries,
which were then sequenced to a depth of ~4 million reads per library,
resulting in a complete gene expression profile for each cell.")
    (license license:artistic2.0)))

(define-public r-all
  (package
    (name "r-all")
    (version "1.46.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ALL" version 'experiment))
       (sha256
        (base32 "1yl8b8q8i19kx4viwinhyq9xn9d4axlfgjvm7cpf7pys4krhnrha"))))
    (properties `((upstream-name . "ALL")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biobase))
    (home-page "https://bioconductor.org/packages/ALL")
    (synopsis "Acute Lymphoblastic Leukemia data from the Ritz laboratory")
    (description
     "The data consist of microarrays from 128 different individuals with
@dfn{acute lymphoblastic leukemia} (ALL).  A number of additional covariates
are available.  The data have been normalized (using rma) and it is the
jointly normalized data that are available here.  The data are presented in
the form of an @code{exprSet} object.")
    (license license:artistic2.0)))

(define-public r-affydata
  (package
    (name "r-affydata")
    (version "1.52.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "affydata" version 'experiment))
       (sha256
        (base32 "0srmrb2pf5dhjfm1l1jd06jgnah16qxillxi5zjr8vq7pgvw8x4b"))))
    (properties `((upstream-name . "affydata")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-affy))
    (home-page "https://bioconductor.org/packages/affydata/")
    (synopsis "Affymetrix data for demonstration purposes")
    (description
     "This package provides example datasets that represent 'real world
examples' of Affymetrix data, unlike the artificial examples included in the
package @code{affy}.")
    (license license:gpl2+)))

(define-public r-gagedata
  (package
    (name "r-gagedata")
    (version "2.42.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "gageData" version 'experiment))
       (sha256
        (base32 "1kbs0fz1jnx3grd9mpclbahwbp3749ipwfs8ggl9299bhjrgh9i6"))))
    (properties `((upstream-name . "gageData")))
    (build-system r-build-system)
    (home-page "https://bioconductor.org/packages/gageData")
    (synopsis "Auxiliary data for the gage package")
    (description
     "This is a supportive data package for the software package @code{gage}.
However, the data supplied here are also useful for gene set or pathway
analysis or microarray data analysis in general.  In this package, we provide
two demo microarray dataset: GSE16873 (a breast cancer dataset from GEO) and
BMP6 (originally published as an demo dataset for GAGE, also registered as
GSE13604 in GEO).  This package also includes commonly used gene set data based
on KEGG pathways and GO terms for major research species, including human,
mouse, rat and budding yeast.  Mapping data between common gene IDs for budding
yeast are also included.")
    (license license:gpl2+)))

(define-public r-curatedtcgadata
  (package
    (name "r-curatedtcgadata")
    (version "1.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "curatedTCGAData" version 'experiment))
       (sha256
        (base32 "1xdygjwqgy6kjjxzvsy25bckc0f8j5m42wrj1vrnwy5fp0q7y287"))))
    (properties
     `((upstream-name . "curatedTCGAData")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotationhub
           r-experimenthub
           r-hdf5array
           r-multiassayexperiment
           r-s4vectors
           r-summarizedexperiment))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/curatedTCGAData/")
    (synopsis "Curated data from The Cancer Genome Atlas")
    (description
     "This package provides publicly available data from The Cancer Genome
Atlas (TCGA) as @code{MultiAssayExperiment} objects.
@code{MultiAssayExperiment} integrates multiple assays (e.g., RNA-seq, copy
number, mutation, microRNA, protein, and others) with clinical / pathological
data.  It also links assay barcodes with patient identifiers, enabling
harmonized subsetting of rows (features) and columns (patients / samples)
across the entire multi-'omics experiment.")
    (license license:artistic2.0)))

(define-public r-parathyroidse
  (package
    (name "r-parathyroidse")
    (version "1.42.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "parathyroidSE" version
                                     'experiment))
              (sha256
               (base32
                "183cyn1i0i2p8npajwmbwzr2ncrrzwgpvh8adhjxzrdqkn3qdnr5"))))
    (properties `((upstream-name . "parathyroidSE")))
    (build-system r-build-system)
    (propagated-inputs (list r-summarizedexperiment))
    (home-page "https://bioconductor.org/packages/parathyroidSE")
    (synopsis "RangedSummarizedExperiment for RNA-Seq of parathyroid tumors")
    (description
     "This package provides @code{RangedSummarizedExperiment} objects of read
counts in genes and exonic parts for paired-end RNA-Seq data from experiments on
primary cultures of parathyroid tumors.  The sequencing was performed on tumor
cultures from 4 patients at 2 time points over 3 conditions (DPN, OHT and control).")
    ;; The author(s) mentions only LGPL without any specific version.
    (license license:lgpl2.1+)))

(define-public r-sesamedata
  (package
    (name "r-sesamedata")
    (version "1.22.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "sesameData" version
                                     'experiment))
              (sha256
               (base32
                "05k67isysszzfkkbi811dkab0cmxnwz81p2ldcqz3gsq5qin3p1g"))))
    (properties `((upstream-name . "sesameData")))
    (build-system r-build-system)
    (propagated-inputs (list r-annotationhub
                             r-experimenthub
                             r-genomeinfodb
                             r-genomicranges
                             r-iranges
                             r-readr
                             r-s4vectors
                             r-stringr))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/sesameData")
    (synopsis "Supporting Data for SeSAMe Package")
    (description
     "This package provides supporting annotation and test data for
@code{SeSAMe} package.  This includes chip tango addresses, mapping
information, performance annotation, and trained predictor for Infinium array
data.  This package provides user access to essential annotation data for
working with many generations of the Infinium DNA methylation array.  It
currently supports human array (HM27, HM450, EPIC), mouse array (MM285) and
the @code{HorvathMethylChip40} (Mammal40) array.")
    (license license:artistic2.0)))

(define-public r-tcgabiolinksgui-data
  (package
    (name "r-tcgabiolinksgui-data")
    (version "1.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "TCGAbiolinksGUI.data" version 'experiment))
       (sha256
        (base32 "1r36f7nvvjjgkgmhha81n2iyr9k4nzy0qkkblm483rgzxav4gqa6"))))
    (properties `((upstream-name . "TCGAbiolinksGUI.data")))
    (build-system r-build-system)
    (native-inputs (list r-knitr))
    (home-page "https://github.com/BioinformaticsFMRP/TCGAbiolinksGUI.data")
    (synopsis "Data for the TCGAbiolinksGUI package")
    (description "This package provides supporting data for the
TCGAbiolinksGUI package.")
    (license license:gpl3)))

(define-public r-tximportdata
  (package
    (name "r-tximportdata")
    (version "1.32.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "tximportData" version
                                     'experiment))
              (sha256
               (base32
                "15wy0dhs9sbz27ii64b4i0zxlsm6v75m2f32gisdy9f6gnbgwkjg"))))
    (properties `((upstream-name . "tximportData")))
    (build-system r-build-system)
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/tximportData")
    (synopsis "Data for the tximport package")
    (description
     "This package provides the output of running various transcript abundance
quantifiers on a set of 6 RNA-seq samples from the GEUVADIS project.  The
quantifiers were @code{Cufflinks}, @code{RSEM}, @code{kallisto}, @code{Salmon}
and @code{Sailfish}.  Alevin example output is also included.")
    (license license:gpl2+)))



;;; Packages

(define-public r-abarray
  (package
    (name "r-abarray")
    (version "1.72.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "ABarray" version))
              (sha256
               (base32
                "1kfqq4df55pxl7gnwf24aaryxijym4gzqfjyif28arlbjci9hlv6"))))
    (properties `((upstream-name . "ABarray")))
    (build-system r-build-system)
    (propagated-inputs (list r-biobase r-multtest))
    (home-page "https://bioconductor.org/packages/ABarray")
    (synopsis
     "Gene expression analysis for Applied Biosystems Genome Survey Microarray")
    (description
     "The package @code{ABarray} is designed to work with Applied Biosystems
whole genome microarray platform, as well as any other platform whose data can
be transformed into expression data matrix.  Functions include data
preprocessing, filtering, control probe analysis, statistical analysis in one
single function.  A @dfn{graphical user interface} (GUI) is also provided.  The
raw data, processed data, graphics output and statistical results are organized
into folders according to the analysis settings used.")
    (license license:gpl2+)))

(define-public r-absseq
  (package
    (name "r-absseq")
    (version "1.58.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "ABSSeq" version))
              (sha256
               (base32
                "0zqb2w055x4whgl3mljarsnps6lkz90fwlx2lnirbnrgmlnalwjz"))))
    (properties `((upstream-name . "ABSSeq")))
    (build-system r-build-system)
    (propagated-inputs (list r-limma r-locfit))
    (home-page "https://bioconductor.org/packages/ABSSeq")
    (synopsis
     "RNA-Seq analysis based on modelling absolute expression differences")
    (description
     "This package implements a new RNA-Seq analysis method and integrates two
modules: a basic model for pairwise comparison and a linear model for complex
design.  RNA-Seq quantifies gene expression with reads count, which usually
consists of conditions (or treatments) and several replicates for each
condition.  This software infers differential expression directly by the
counts difference between conditions.  It assumes that the sum counts
difference between conditions follow a negative binomial distribution.  In
addition, @code{ABSSeq} moderates the fold-changes by two steps: the
expression level and gene-specific dispersion, that might facilitate the gene
ranking by fold-change and visualization.")
    (license license:gpl3+)))

(define-public r-adacgh2
  (package
    (name "r-adacgh2")
    (version "2.44.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ADaCGH2" version))
       (sha256
        (base32 "1j9dn0xw8mdfvi2ccm1d5j9q740wb89wqi6715n6jy3dljlc8irz"))))
    (properties `((upstream-name . "ADaCGH2")))
    (build-system r-build-system)
    (arguments
     (list
      #:phases
      '(modify-phases %standard-phases
         (add-after 'unpack 'python3-compatibility
           (lambda _
             (substitute* "inst/imagemap-example/toMap.py"
               (("print nameMap") "print(nameMap)")))))))
    (inputs (list python-wrapper))
    (propagated-inputs
     (list r-acgh
           r-bit
           r-cluster
           r-dnacopy
           r-ff
           r-glad
           r-tilingarray
           r-waveslim))
    (home-page "https://github.com/rdiaz02/adacgh2")
    (synopsis "Big data analysis from aCGH experiments")
    (description
     "This package analyzes and creates plots of array @acronym{CGH,
comparative genomic hybridization} data.  Also, it allows usage of
@acronym{CBS, Circular Binary Segementation}, wavelet-based smoothing, HMM,
BioHMM, GLAD, CGHseg.  Most computations are parallelized (either via forking
or with clusters, including MPI and sockets clusters) and use @code{ff} for
storing data.")
    (license license:gpl3+)))

(define-public r-adam
  (package
    (name "r-adam")
    (version "1.20.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "ADAM" version))
              (sha256
               (base32
                "0z0rmpwfhd8p6c4zscviwsningsm740r3pb95cl8brasfdg1kl3v"))))
    (properties `((upstream-name . "ADAM")))
    (build-system r-build-system)
    (propagated-inputs (list r-dplyr
                             r-dt
                             r-go-db
                             r-keggrest
                             r-knitr
                             r-pbapply
                             r-rcpp
                             r-stringr
                             r-summarizedexperiment))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/ADAM")
    (synopsis "Gene activity and diversity analysis module")
    (description
     "This software @code{ADAM} is a @dfn{Gene set enrichment analysis} (GSEA)
package created to group a set of genes from comparative samples (control
versus experiment) belonging to different species according to their respective
functions.  The corresponding roles are extracted from the default collections
like Gene ontology and @dfn{Kyoto encyclopedia of genes and genomes} (KEGG).
@code{ADAM} show their significance by calculating the p-values referring to
gene diversity and activity.  Each group of genes is called @dfn{Group of
functionally associated genes} (GFAG).")
    (license license:gpl2+)))

(define-public r-adamgui
  (package
    (name "r-adamgui")
    (version "1.20.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "ADAMgui" version))
              (sha256
               (base32
                "08kxkb6hgbw7l9dv1wn3jfdkc0sizqdgmjkrzpq73q7rf0p4248g"))))
    (properties `((upstream-name . "ADAMgui")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-adam
           r-colorramps
           r-data-table
           r-dplyr
           r-dt
           r-ggplot2
           r-ggpubr
           r-ggrepel
           r-ggsignif
           r-go-db
           r-gridextra
           r-knitr
           r-rcolorbrewer
           r-reshape2
           r-shiny
           r-shinyjs
           r-stringi
           r-stringr
           r-testthat
           r-varhandle))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/ADAMgui/")
    (synopsis "GUI for gene activity and diversity analysis")
    (description
     "This package @code{ADAMgui} is a @dfn{graphical user interface} (GUI)
for the @code{ADAM} package.  The @code{ADAMgui} package provides two
shiny-based applications that allows the user to study the output of the
@code{ADAM} package files through different plots.  It's possible, for
example, to choose a specific @dfn{group of functionally associated
genes} (GFAG) and observe the gene expression behavior with the plots created
with the @code{GFAGtargetUi} function.  Features such as differential
expression and fold change can be easily seen with aid of the plots made with
the @code{GFAGpathUi} function.")
    (license license:gpl2+)))

(define-public r-adimpute
  (package
    (name "r-adimpute")
    (version "1.14.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "ADImpute" version))
              (sha256
               (base32
                "0kihg0mwm2ysgnx3jg6xln7ibvgmky0x2hjbcmdqlg65znczh4b6"))))
    (properties `((upstream-name . "ADImpute")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocparallel
           r-checkmate
           r-data-table
           r-drimpute
           r-kernlab
           r-mass
           r-matrix
           r-rsvd
           r-s4vectors
           r-saver
           r-singlecellexperiment
           r-summarizedexperiment))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/ADImpute")
    (synopsis "Adaptive computational prediction for dropout imputations")
    (description
     "@dfn{Single-cell RNA sequencing} (scRNA-seq) methods are typically
unable to quantify the expression levels of all genes in a cell, creating a
need for the computational prediction of missing values (dropout imputation).
Most existing dropout imputation methods are limited in the sense that they
exclusively use the scRNA-seq dataset at hand and do not exploit external
gene-gene relationship information.  The @code{ADImpute} package proposes two
methods to address this issue:

@enumerate
@item a gene regulatory network-based approach using gene-gene relationships
  learnt from external data;
@item a baseline approach corresponding to a sample-wide average.
@end enumerate

@code{ADImpute} implements these novel methods and also combines them with
existing imputation methods like @code{DrImpute} and @code{SAVER}.
@code{ADImpute} can learn the best performing method per gene and combine the
results from different methods into an ensemble.")
    (license license:gpl3+)))

(define-public r-adsplit
  (package
    (name "r-adsplit")
    (version "1.74.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "adSplit" version))
              (sha256
               (base32
                "13rrkb0vxkknvqhb8pgx8d13sxzy3w0wbkhhj5lrlc1kb1yqcfbh"))))
    (properties `((upstream-name . "adSplit")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotationdbi
           r-biobase
           r-cluster
           r-go-db
           r-keggrest
           r-multtest))
    (home-page "https://compdiag.molgen.mpg.de/software/adSplit.shtml")
    (synopsis "Annotation-driven splits in microarray data")
    (description
     "This package implements clustering of microarray gene expression
profiles according to functional annotations.  For each term genes are
annotated to, splits into two subclasses are computed and a significance of
the supporting gene set is determined.")
    (license license:gpl2+)))

(define-public r-affixcan
  (package
    (name "r-affixcan")
    (version "1.22.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "AffiXcan" version))
              (sha256
               (base32
                "1241vpqkv4p3yarr6fsz1a7awvc4ff7q75ia3ra7vgkvg7gx9p93"))))
    (properties `((upstream-name . "AffiXcan")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocparallel
           r-crayon
           r-multiassayexperiment
           r-summarizedexperiment))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/AffiXcan")
    (synopsis "Functional approach to impute genetically regulated expression")
    (description
     "The @code{AffiXcan} package imputes a @dfn{genetically regulated
expression} (GReX) for a set of genes in a sample of individuals, using a
method based on the @dfn{total binding affinity} (TBA).  Statistical models to
impute GReX can be trained with a training dataset where the real total
expression values are known.")
    (license license:gpl3)))

(define-public r-affyilm
  (package
    (name "r-affyilm")
    (version "1.56.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "affyILM" version))
       (sha256
        (base32 "185kdrnvcp2jw0k71q0v4yk7583gwgi31h0n281x25hirixf6rm3"))))
    (properties `((upstream-name . "affyILM")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-affxparser
           r-affy
           r-biobase
           r-gcrma))
    (home-page "https://bioconductor.org/packages/affyILM")
    (synopsis
     "Linear model of background subtraction and the Langmuir isotherm")
    (description
     "The affyILM package is a preprocessing tool which estimates gene
expression levels for Affymetrix Gene Chips.  Input from physical chemistry is
employed to first background subtract intensities before calculating
concentrations on behal of the Langmuir model.")
    (license license:gpl3)))

(define-public r-affylmgui
  (package
    (name "r-affylmgui")
    (version "1.78.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "affylmGUI" version))
       (sha256
        (base32 "1qilkr3dcl6driah24kgz7lrzamq08qp2z03inm2m4r5yx8811wc"))))
    (properties `((upstream-name . "affylmGUI")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-affy
           r-affyio
           r-affyplm
           r-annotationdbi
           r-biocgenerics
           r-biocmanager
           r-gcrma
           r-limma
           r-r2html
           r-tkrplot
           r-xtable))
    (home-page "https://bioinf.wehi.edu.au/affylmGUI/")
    (synopsis "GUI for limma package with Affymetrix microarrays")
    (description
     "This package provides a @acronym{GUI, Graphical User Interface} for
analysis of Affymetrix microarray gene expression data using the affy and
limma packages.")
    (license license:gpl2+)))

(define-public r-affyplm
  (package
    (name "r-affyplm")
    (version "1.80.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "affyPLM" version))
       (sha256
        (base32 "1khknl7k45amyw58mcgw7kl511llyddw31qzwqc2l5c4j3bwnpz5"))))
    (properties `((upstream-name . "affyPLM")))
    (build-system r-build-system)
    (inputs (list zlib))
    (propagated-inputs
     (list r-affy
           r-biobase
           r-biocgenerics
           r-gcrma
           r-preprocesscore
           r-zlibbioc))
    (home-page "https://github.com/bmbolstad/affyPLM")
    (synopsis "Methods for fitting probe-level models")
    (description
     "The affyPLM provides a package that extends and improves the
functionality of the base affy package.  For speeding up the runs, it includes
routines that make heavy use of compiled code.  The central focus is on
implementation of methods for fitting probe-level models and tools using these
models.  @acronym{PLM, probe-level models} based quality assessment tools are
also provided.")
    (license license:gpl2+)))

(define-public r-affyrnadegradation
  (package
    (name "r-affyrnadegradation")
    (version "1.50.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "AffyRNADegradation" version))
              (sha256
               (base32
                "19lcs76jsdy5dz57rkdb8qqankpasrw2r5z4cy98q94az6582ix4"))))
    (properties `((upstream-name . "AffyRNADegradation")))
    (build-system r-build-system)
    (propagated-inputs (list r-affy))
    (home-page "https://bioconductor.org/packages/AffyRNADegradation")
    (synopsis
     "Analyze and correct probe positional bias in data due to RNA degradation")
    (description
     "The @code{AffyRNADegradation} package helps with the assessment and
correction of RNA degradation effects in Affymetrix 3 expression arrays.  The
parameter @code{d} gives a robust and accurate measure of RNA integrity.  The
correction removes the probe positional bias, and thus improves comparability
of samples that are affected by RNA degradation.")
    ;; the R file header specifies GPL2 or later
    (license license:gpl2+)))

(define-public r-agdex
  (package
    (name "r-agdex")
    (version "1.52.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "AGDEX" version))
              (sha256
               (base32
                "088r2ahqjv61wnzp7by4c5v9aivahmyfkzr7prnkk3ff8zh6pznl"))))
    (properties `((upstream-name . "AGDEX")))
    (build-system r-build-system)
    (propagated-inputs (list r-biobase r-gseabase))
    (home-page "https://bioconductor.org/packages/AGDEX")
    (synopsis
     "Evaluate agreement of differential expression for cross-species genomics")
    (description
     "The objective of @code{AGDEX} is to evaluate whether the results of a
pair of two-group differential expression analysis comparisons show a level of
agreement that is greater than expected if the group labels for each two-group
comparison are randomly assigned.  The agreement is evaluated for the entire
transcriptome and (optionally) for a collection of pre-defined gene-sets.
Additionally, the procedure performs permutation-based differential expression
and meta analysis at both gene and gene-set levels of the data from each
experiment.")
    (license license:gpl2+)))

(define-public r-aggregatebiovar
  (package
    (name "r-aggregatebiovar")
    (version "1.14.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "aggregateBioVar" version))
              (sha256
               (base32
                "0qag99chj7n7l0wbb8ar0f58xs6883rgddkgj7jnrrlj3acrm12v"))))
    (properties `((upstream-name . "aggregateBioVar")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-matrix
           r-rlang
           r-s4vectors
           r-singlecellexperiment
           r-summarizedexperiment
           r-tibble))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/jasonratcliff/aggregateBioVar")
    (synopsis "Differential gene expression analysis for multi-subject scRNA-seq")
    (description
     "This package @code{aggregateBioVar} contains tools to summarize single
cell gene expression profiles at the level of subject for single cell RNA-seq
data collected from more than one subject (e.g. biological sample or technical
replicates).  A @code{SingleCellExperiment} object is taken as input and
converted to a list of @code{SummarizedExperiment} objects, where each list
element corresponds to an assigned cell type.  The @code{SummarizedExperiment}
objects contain aggregate gene-by-subject count matrices and inter-subject
column metadata for individual subjects that can be processed using downstream
bulk RNA-seq tools.")
    (license license:gpl3)))

(define-public r-agilp
  (package
    (name "r-agilp")
    (version "3.36.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "agilp" version))
              (sha256
               (base32
                "1mfi4xvx6ywkr7b7ma9ip6q2nsjvkri8qx48qyr98yyik7gg28d5"))))
    (properties `((upstream-name . "agilp")))
    (build-system r-build-system)
    (home-page "https://bioconductor.org/packages/agilp")
    (synopsis "Processing of Agilent expression array")
    (description
     "This package aims to provide a pipeline for the low-level analysis of
gene expression microarray data, primarily focused on the Agilent platform,
but which also provides utilities which may be useful for other platforms.")
    ;; Some files are under GPLv2+ but the combined work is released under the
    ;; GPLv3.
    (license license:gpl3)))

(define-public r-adductomicsr
  (package
    (name "r-adductomicsr")
    (version "1.20.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "adductomicsR" version))
              (sha256
               (base32
                "0800jbfm488i6ccz48k49gpxr9sf9104cxcghgy9c4lzm5d3wfbm"))))
    (properties `((upstream-name . "adductomicsR")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-adductdata
           r-ade4
           r-annotationhub
           r-bootstrap
           r-data-table
           r-dosnow
           r-dplyr
           r-dt
           r-experimenthub
           r-fastcluster
           r-foreach
           r-fpc
           r-mzr
           r-orgmassspecr
           r-pastecs
           r-pracma
           r-rcppeigen
           r-reshape2
           r-rvest
           r-smoother
           r-zoo))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/adductomicsR")
    (synopsis "Processing of adductomic mass spectral datasets")
    (description
     "This package @code{adductomicsR} processes data generated by the
@dfn{second stage of mass spectrometry} (MS2) to identify potentially adducted
peptides from spectra that has been corrected for mass drift and retention
time drift and quantifies level mass spectral peaks from @dfn{first stage of
mass spectrometry} (MS1) data.")
    (license license:artistic2.0)))

(define-public r-agimicrorna
  (package
    (name "r-agimicrorna")
    (version "2.54.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "AgiMicroRna" version))
              (sha256
               (base32
                "1bm8rxfdgd0fx48fi70by08qr855jv6r4xcr7al2v9akv6vphws2"))))
    (properties `((upstream-name . "AgiMicroRna")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-affy
           r-affycoretools
           r-biobase
           r-limma
           r-preprocesscore))
    (home-page "https://git.bioconductor.org/packages/AgiMicroRna")
    (synopsis
     "Processing and differential expression analysis of Agilent microRNA chips")
    (description
     "@code{AgiMicroRna} provides useful functionality for the processing,
quality assessment and differential expression analysis of Agilent microRNA
array data.  The package uses a limma-like structure to generate the processed
data in order to make statistical inferences about differential expression
using the linear model features implemented in limma.  Standard Bioconductor
objects are used so that other packages could be used as well.")
    (license license:gpl3)))

(define-public r-aims
  (package
    (name "r-aims")
    (version "1.36.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "AIMS" version))
              (sha256
               (base32
                "0ljdv1gww01yi0h9j9br8kp67k5wizr1dy0091kngjcbn0dxzmla"))))
    (properties `((upstream-name . "AIMS")))
    (build-system r-build-system)
    (propagated-inputs (list r-biobase r-e1071))
    (home-page "https://git.bioconductor.org/packages/AIMS")
    (synopsis
     "Absolute assignment of breast cancer intrinsic molecular subtype")
    (description
     "This package contains an implementation of @code{AIMS} -- Absolute
Intrinsic Molecular Subtyping.  It contains necessary functions to assign the
five intrinsic molecular subtypes (Luminal A, Luminal B, Her2-enriched,
Basal-like, Normal-like).  Assignments could be done on individual samples as
well as on dataset of gene expression data.")
    (license license:artistic2.0)))

(define-public r-airpart
  (package
    (name "r-airpart")
    (version "1.12.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "airpart" version))
              (sha256
               (base32
                "05l5rgjfjln3kip72wj4ll4cgak9yz60xg6dgxxj690pxs1pw7bg"))))
    (properties `((upstream-name . "airpart")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-apeglm
           r-clue
           r-complexheatmap
           r-dplyr
           r-dynamictreecut
           r-emdbook
           r-forestplot
           r-ggplot2
           r-lpsolve
           r-matrixstats
           r-mclust
           r-pbapply
           r-plyr
           r-rcolorbrewer
           r-rlang
           r-s4vectors
           r-scater
           r-singlecellexperiment
           r-smurf
           r-summarizedexperiment))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/airpart")
    (synopsis "Differential cell-type-specific allelic imbalance")
    (description
     "The airpart package identifies sets of genes displaying differential
cell-type-specific allelic imbalance across cell types or states, utilizing
single-cell allelic counts.  It makes use of a generalized fused lasso with
binomial observations of allelic counts to partition cell types by their
allelic imbalance.  Alternatively, a nonparametric method for partitioning
cell types is offered.  The package includes a number of visualizations and
quality control functions for examining single cell allelic imbalance
datasets.")
    (license license:gpl2)))

(define-public r-alabaster-base
  (package
    (name "r-alabaster-base")
    (version "1.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "alabaster.base" version))
       (sha256
        (base32 "1yf4j6dwva1d16fb8ss5hrpmj71azwjk9aqpad9wpnfapaldjzhb"))))
    (properties `((upstream-name . "alabaster.base")))
    (build-system r-build-system)
    (inputs (list zlib))
    (propagated-inputs (list r-alabaster-schemas
                             r-jsonlite
                             r-jsonvalidate
                             r-rcpp
                             r-rhdf5
                             r-rhdf5lib
                             r-s4vectors))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/alabaster.base")
    (synopsis "Save Bioconductor objects to file")
    (description
     "This is a package for saving Bioconductor data structures into file
artifacts, and loading them back into memory.  This is a more robust and
portable alternative to serialization of such objects into RDS files.  Each
artifact is associated with metadata for further interpretation; downstream
applications can enrich this metadata with context-specific properties.")
    (license license:expat)))

(define-public r-alabaster-matrix
  (package
    (name "r-alabaster-matrix")
    (version "1.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "alabaster.matrix" version))
       (sha256
        (base32 "1b32qs2baq9s492a332vh1hpyxq62z16xj2w17m8wh8a4mcabry8"))))
    (properties `((upstream-name . "alabaster.matrix")))
    (build-system r-build-system)
    (propagated-inputs (list r-alabaster-base
                             r-biocgenerics
                             r-delayedarray
                             r-hdf5array
                             r-matrix
                             r-rcpp
                             r-rhdf5
                             r-s4arrays
                             r-s4vectors
                             r-sparsearray))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/alabaster.matrix")
    (synopsis "Load and save artifacts from file")
    (description
     "This is a package for saving matrices, arrays and similar objects into
file artifacts, and loading them back into memory.  This is a more portable
alternative to serialization of such objects into RDS files.  Each artifact is
associated with metadata for further interpretation; downstream applications
can enrich this metadata with context-specific properties.")
    (license license:expat)))

(define-public r-alabaster-ranges
  (package
    (name "r-alabaster-ranges")
    (version "1.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "alabaster.ranges" version))
       (sha256
        (base32 "16r5wdlw2g9qnbvzpx5c5c04xa5dkwrmqlyc81iixkx6j98gsz2w"))))
    (properties `((upstream-name . "alabaster.ranges")))
    (build-system r-build-system)
    (propagated-inputs (list r-alabaster-base
                             r-biocgenerics
                             r-genomeinfodb
                             r-genomicranges
                             r-iranges
                             r-rhdf5
                             r-s4vectors))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/alabaster.ranges")
    (synopsis "Load and save Ranges-related artifacts from file")
    (description
     "This is a package for saving @code{GenomicRanges}, @code{IRanges} and
related data structures into file artifacts, and loading them back into
memory.  This is a more portable alternative to serialization of such objects
into RDS files.  Each artifact is associated with metadata for further
interpretation; downstream applications can enrich this metadata with
context-specific properties.")
    (license license:expat)))

(define-public r-alabaster-sce
  (package
    (name "r-alabaster-sce")
    (version "1.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "alabaster.sce" version))
       (sha256
        (base32 "1i9nmivsfrd7fbb8njmwihfx4xpj4p3h5bnlj9fvk5308bbj3qcg"))))
    (properties `((upstream-name . "alabaster.sce")))
    (build-system r-build-system)
    (propagated-inputs (list r-alabaster-base r-alabaster-se r-jsonlite
                             r-singlecellexperiment))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/alabaster.sce")
    (synopsis "Load and save SingleCellExperiment from file")
    (description
     "This is a package for saving @code{SingleCellExperiment} into file
artifacts, and loading them back into memory.  This is a more portable
alternative to serialization of such objects into RDS files.  Each artifact is
associated with metadata for further interpretation; downstream applications
can enrich this metadata with context-specific properties.")
    (license license:expat)))

(define-public r-alabaster-schemas
  (package
    (name "r-alabaster-schemas")
    (version "1.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "alabaster.schemas" version))
       (sha256
        (base32 "1cizlslxw0nhrfjsz3x90waa9w4n2v72a0cycbzvxqbbvkh3cp1l"))))
    (properties `((upstream-name . "alabaster.schemas")))
    (build-system r-build-system)
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/alabaster.schemas")
    (synopsis "Schemas for the Alabaster framework")
    (description
     "This package stores all schemas required by various @code{alabaster.*}
packages.  No computation should be performed by this package, as that is
handled by @code{alabaster.base}.")
    (license license:expat)))

(define-public r-alabaster-se
  (package
    (name "r-alabaster-se")
    (version "1.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "alabaster.se" version))
       (sha256
        (base32 "134r4pwaa4nsmdgqxs16w3adn482zl70lgyd4s96pmx7hqm9i1vx"))))
    (properties `((upstream-name . "alabaster.se")))
    (build-system r-build-system)
    (propagated-inputs (list r-alabaster-base
                             r-alabaster-matrix
                             r-alabaster-ranges
                             r-biocgenerics
                             r-genomicranges
                             r-iranges
                             r-jsonlite
                             r-s4vectors
                             r-summarizedexperiment))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/alabaster.se")
    (synopsis "Load and save SummarizedExperiments from file")
    (description
     "This is a package for saving @code{SummarizedExperiments} into file
artifacts, and loading them back into memory.  This is a more portable
alternative to serialization of such objects into RDS files.  Each artifact is
associated with metadata for further interpretation; downstream applications
can enrich this metadata with context-specific properties.")
    (license license:expat)))

(define-public r-amountain
  (package
    (name "r-amountain")
    (version "1.30.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "AMOUNTAIN" version))
              (sha256
               (base32
                "1y2k1vg31siic6j98700m54sswbj6wr4dvbw2mz3hlm0nmsmpxc1"))))
    (properties `((upstream-name . "AMOUNTAIN")))
    (build-system r-build-system)
    (inputs (list gsl))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/AMOUNTAIN")
    (synopsis "Modules for multilayer weighted gene co-expression networks")
    (description
     "This package provides a pure data-driven gene network, @dfn{WGCN}(weighted
gene co-expression network) could be constructed only from expression profile.
Different layers in such networks may represent different time points, multiple
conditions or various species.  @code{AMOUNTAIN} aims to search active modules
in multi-layer WGCN using a continuous optimization approach.")
    (license license:gpl2+)))

(define-public r-amplican
  (package
    (name "r-amplican")
    (version "1.26.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "amplican" version))
              (sha256
               (base32
                "1nbyivd5020kqkmk5ngpaixhdxgmqpifaxnfgq6yc2njqizgbhxy"))))
    (properties `((upstream-name . "amplican")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocgenerics
           r-biocparallel
           r-biostrings
           r-cluster
           r-data-table
           r-dplyr
           r-genomeinfodb
           r-genomicranges
           r-ggplot2
           r-ggthemes
           r-gridextra
           r-gtable
           r-iranges
           r-knitr
           r-matrix
           r-matrixstats
           r-pwalign
           r-rcpp
           r-rmarkdown
           r-s4vectors
           r-shortread
           r-stringr
           r-waffle))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/valenlab/amplican")
    (synopsis "Automated analysis of CRISPR experiments")
    (description
     "The package performs alignment of the amplicon reads, normalizes
gathered data, calculates multiple statistics (e.g. cut rates, frameshifts)
and presents the results in the form of aggregated reports.  Data and
statistics can be broken down by experiments, barcodes, user defined groups,
guides and amplicons allowing for quick identification of potential
problems.")
    (license license:gpl3)))

(define-public r-amaretto
  (package
    (name "r-amaretto")
    (version "1.20.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "AMARETTO" version))
              (sha256
               (base32
                "0n3w00x1qajkg43ip3l63k2qs0d1x2zv8wv4xj0iqwl6r6cqgxnf"))))
    (properties `((upstream-name . "AMARETTO")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocfilecache
           r-callr
           r-circlize
           r-complexheatmap
           r-curatedtcgadata
           r-doparallel
           r-dplyr
           r-dt
           r-foreach
           r-ggplot2
           r-glmnet
           r-gridextra
           r-httr
           r-impute
           r-knitr
           r-limma
           r-matrix
           r-matrixstats
           r-multiassayexperiment
           r-rcpp
           r-readr
           r-reshape2
           r-rmarkdown
           r-tibble))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/AMARETTO")
    (synopsis "Regulatory network inference and driver gene evaluation")
    (description
     "This package @code{AMARETTO} represents an algorithm that integrates copy
number, DNA methylation and gene expression data to identify a set of driver
genes by analyzing cancer samples and connects them to clusters of co-expressed
genes, which we define as modules.  @code{AMARETTO} can be applied in a pancancer
setting to identify cancer driver genes and their modules on multiple cancer
sites.  @code{AMARETTO} captures modules enriched in angiogenesis, cell cycle
and EMT, and modules that accurately predict survival and molecular subtypes.
This allows @code{AMARETTO} to identify novel cancer driver genes directing
canonical cancer pathways.")
    (license license:asl2.0)))

(define-public r-anaquin
  (package
    (name "r-anaquin")
    (version "2.28.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "Anaquin" version))
              (sha256
               (base32
                "0hfvwzkfy6bm2ffghax5b228k2l6wf8zq82q13h6bxfps4wz0jgb"))))
    (properties `((upstream-name . "Anaquin")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-deseq2
           r-ggplot2
           r-knitr
           r-locfit
           r-plyr
           r-qvalue
           r-rocr))
    (native-inputs (list r-knitr))
    (home-page "https://www.sequinstandards.com/")
    (synopsis "Statistical analysis of sequins")
    (description
     "The project is intended to support the use of @dfn{sequins}(synthetic
sequencing spike-in controls) owned and made available by the Garvan Institute
of Medical Research.  The goal is to provide a standard library for quantitative
analysis, modelling, and visualization of spike-in controls.")
    (license license:bsd-3)))

(define-public r-ancombc
  (package
    (name "r-ancombc")
    (version "2.6.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "ANCOMBC" version))
              (sha256
               (base32
                "14jdfcv3hghl1z2f7zj2mdqdyblz5sn4l1iwh8bhvih86cpigb58"))))
    (properties `((upstream-name . "ANCOMBC")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-cvxr
           r-desctools
           r-doparallel
           r-dorng
           r-energy
           r-foreach
           r-gtools
           r-hmisc
           r-lme4
           r-lmertest
           r-mass
           r-matrix
           r-mia
           r-multcomp
           r-nloptr
           r-rdpack
           r-s4vectors
           r-singlecellexperiment
           r-summarizedexperiment
           r-treesummarizedexperiment))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/FrederickHuangLin/ANCOMBC")
    (synopsis "Analysis of compositions of microbiomes with bias correction")
    (description
     "@code{ANCOMBC} is a package containing @dfn{differential abundance} (DA)
and correlation analyses for microbiome data.  Specifically, the package
includes @dfn{Analysis of Compositions of Microbiomes with Bias
Correction}(ANCOM-BC) and @dfn{Analysis of Composition of Microbiomes} (ANCOM)
for DA analysis, and @dfn{Sparse Estimation of Correlations among
Microbiomes} (SECOM) for correlation analysis.  Microbiome data are typically
subject to two sources of biases: unequal sampling fractions (sample-specific
biases) and differential sequencing efficiencies (taxon-specific biases).
Methodologies included in the @code{ANCOMBC} package were designed to correct
these biases and construct statistically consistent estimators.")
    (license license:artistic2.0)))

(define-public r-animalcules
  (package
    (name "r-animalcules")
    (version "1.20.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "animalcules" version))
              (sha256
               (base32
                "1g073n8nmckd388hy9gfiddil53wv692k69ywjc85jyp5jy2n265"))))
    (properties `((upstream-name . "animalcules")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-ape
           r-assertthat
           r-caret
           r-covr
           r-deseq2
           r-dplyr
           r-dt
           r-forcats
           r-ggforce
           r-ggplot2
           r-gunifrac
           r-lattice
           r-limma
           r-magrittr
           r-matrix
           r-multiassayexperiment
           r-plotly
           r-rentrez
           r-reshape2
           r-rocit
           r-s4vectors
           r-scales
           r-shiny
           r-shinyjs
           r-summarizedexperiment
           r-tibble
           r-tidyr
           r-tsne
           r-umap
           r-vegan
           r-xml))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/compbiomed/animalcules")
    (synopsis "Interactive microbiome analysis toolkit")
    (description
     "Animalcules is an R package for utilizing up-to-date data analytics,
visualization methods, and machine learning models to provide users an
easy-to-use interactive microbiome analysis framework.  It can be used as a
standalone software package or users can explore their data with the
accompanying interactive R Shiny application.  Traditional microbiome analysis
such as alpha/beta diversity and differential abundance analysis are enhanced,
while new methods like biomarker identification are introduced by animalcules.
Powerful interactive and dynamic figures generated by animalcules enable users
to understand their data better and discover new insights.")
    (license license:artistic2.0)))

(define-public r-annotationhubdata
  (package
    (name "r-annotationhubdata")
    (version "1.34.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "AnnotationHubData" version))
       (sha256
        (base32 "0wcbghjvgr8sx1qg9kyjxb7znkxhihy7jvzycrv4pyq746154xbi"))))
    (properties `((upstream-name . "AnnotationHubData")))
    (build-system r-build-system)
    (arguments
     (list
      #:phases
      '(modify-phases %standard-phases
         (add-before 'install 'set-home
           (lambda _ (setenv "HOME" "/tmp"))))))
    (propagated-inputs
     (list r-annotationdbi
           r-annotationforge
           r-annotationhub
           r-biobase
           r-bioccheck
           r-biocgenerics
           r-biocmanager
           r-biocviews
           r-biostrings
           r-dbi
           r-futile-logger
           r-genomeinfodb
           r-genomicfeatures
           r-genomicranges
           r-graph
           r-iranges
           r-jsonlite
           r-organismdbi
           r-rcurl
           r-rsamtools
           r-rsqlite
           r-rtracklayer
           r-s4vectors
           r-xml))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/AnnotationHubData")
    (synopsis "Transform public data resources into Bioconductor data structures")
    (description
     "This package provides tools to acquire, annotate, convert and store data
for use in Bioconductor’s AnnotationHub.")
    (license license:artistic2.0)))

(define-public r-anvil
  (package
    (name "r-anvil")
    (version "1.16.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "AnVIL" version))
              (sha256
               (base32
                "0859gsgvljl2adjz4md52z4k1bcqh24i5xdfqddflw4gaq1zlm69"))))
    (properties `((upstream-name . "AnVIL")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocbaseutils
           r-biocmanager
           r-dplyr
           r-dt
           r-futile-logger
           r-htmltools
           r-httr
           r-jsonlite
           r-miniui
           r-rapiclient
           r-rlang
           r-shiny
           r-tibble
           r-tidyr
           r-tidyselect
           r-yaml))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/AnVIL")
    (synopsis "Provides access to AnVIL, Terra, Leonardo and other projects")
    (description
     "The AnVIL is a cloud computing resource developed in part by the
National Human Genome Research Institute.  The AnVIL package provides end-user
and developer functionality.  AnVIL provides fast binary package installation,
utilities for working with Terra/AnVIL table and data resources, and
convenient functions for file movement to and from Google cloud storage.  For
developers, AnVIL provides programatic access to the Terra, Leonardo, Rawls,
Dockstore, and Gen3 RESTful programming interface, including helper functions
to transform JSON responses to formats more amenable to manipulation in R.")
    (license license:artistic2.0)))

(define-public r-aldex2
  (package
    (name "r-aldex2")
    (version "1.36.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "ALDEx2" version))
              (sha256
               (base32
                "1bqpk93bbyajw2dm54d68f3dkc0jgcny109asfm6kcz7dg7szi3y"))))
    (properties `((upstream-name . "ALDEx2")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocparallel
           r-directlabels
           r-genomicranges
           r-iranges
           r-lattice
           r-latticeextra
           r-multtest
           r-rfast
           r-s4vectors
           r-summarizedexperiment
           r-zcompositions))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/ggloor/ALDEx_bioc")
    (synopsis "Analysis of differential abundance taking sample variation into account")
    (description
     "This package provides a differential abundance analysis for the
comparison of two or more conditions.  Useful for analyzing data from standard
RNA-seq or meta-RNA-seq assays as well as selected and unselected values from
in-vitro sequence selections.  Uses a Dirichlet-multinomial model to infer
abundance from counts, optimized for three or more experimental replicates.
The method infers biological and sampling variation to calculate the expected
false discovery rate, given the variation, based on a Wilcoxon Rank Sum test
and Welch's t-test, a Kruskal-Wallis test, a generalized linear model, or a
correlation test.  All tests report p-values and Benjamini-Hochberg corrected
p-values.  ALDEx2 also calculates expected standardized effect sizes for
paired or unpaired study designs.")
    ;; The code for the function "rdirichlet" is from the R package
    ;; "mc2d_0.1-14.tar.gz", which is denoted as GPL>=2, and where the
    ;; package's LICENSE is specified as GPL-3.
    (license (list license:agpl3+ license:gpl2+ license:gpl3))))

(define-public r-alevinqc
  (package
    (name "r-alevinqc")
    (version "1.20.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "alevinQC" version))
              (sha256
               (base32
                "1pq42lvgggysg28ngxipqb1l7jfj498nxpg40h02siz6nb6inbdw"))))
    (properties `((upstream-name . "alevinQC")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-cowplot
           r-dplyr
           r-dt
           r-ggally
           r-ggplot2
           r-rcpp
           r-rjson
           r-rlang
           r-rmarkdown
           r-shiny
           r-shinydashboard
           r-tximport))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/csoneson/alevinQC")
    (synopsis "Quality control reports for @code{Alevin} output")
    (description
     "The package @code{r-alevinqc} generates quality control reports
summarizing the output from an @code{alevin} run.  The reports can be
generated as HTML or PDF files, or as Shiny applications.")
    (license license:expat)))

(define-public r-alphabeta
  (package
    (name "r-alphabeta")
    (version "1.18.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "AlphaBeta" version))
              (sha256
               (base32
                "192jyfw2676x8pgcp9h4d78283p7n6gnsdw7wzphfjn12v266fb5"))))
    (properties `((upstream-name . "AlphaBeta")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocparallel
           r-data-table
           r-dplyr
           r-expm
           r-ggplot2
           r-gtools
           r-igraph
           r-optimx
           r-plotly
           r-stringr))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/AlphaBeta")
    (synopsis "Estimate epimutation rates and spectra from DNA methylations in plants")
    (description
     "The package @code{AlphaBeta} is a computational method for estimating
epimutation rates and spectra from high-throughput DNA methylation data in
plants.  The method has been specifically designed to:

@itemize
@item analyze @emph{germline} epimutations in the context of
  multi-generational mutation accumulation lines;
@item analyze @emph{somatic} epimutations in the context of plant development
  and aging.
@end itemize")
    (license license:gpl3)))

(define-public r-alpine
  (package
    (name "r-alpine")
    (version "1.26.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "alpine" version))
              (sha256
               (base32
                "1md4m9ln1mpxf7d2h7jnsjyi4zrviiqn9fzk1gkz2n6qj7jwpqbb"))))
    (properties `((upstream-name . "alpine")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biostrings
           r-genomeinfodb
           r-genomicalignments
           r-genomicfeatures
           r-genomicranges
           r-graph
           r-iranges
           r-rbgl
           r-rsamtools
           r-s4vectors
           r-speedglm
           r-stringr
           r-summarizedexperiment))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/alpine")
    (synopsis "Modeling and correcting fragment sequence bias")
    (description
     "The package @code{alpine} helps to model bias parameters and then using
those parameters to estimate RNA-seq transcript abundance.  @code{Alpine} is a
package for estimating and visualizing many forms of sample-specific biases that
can arise in RNA-seq, including fragment length distribution, positional bias on
the transcript, read start bias (random hexamer priming), and fragment GC-content
(amplification).  It also offers bias-corrected estimates of transcript
abundance in @dfn{FPKM}(Fragments Per Kilobase of transcript per Million
mapped reads).  It is currently designed for un-stranded paired-end RNA-seq
data.")
    (license license:gpl2+)))

(define-public r-alpsnmr
  (package
    (name "r-alpsnmr")
    (version "4.6.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "AlpsNMR" version))
              (sha256
               (base32
                "1c8ynw9424ygc4wj4drbm9gdkdqwgb5z7p72zk3x5zh3zqai0d1d"))))
    (properties `((upstream-name . "AlpsNMR")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-baseline
           r-biocparallel
           r-cli
           r-dplyr
           r-fs
           r-future
           r-generics
           r-ggplot2
           r-glue
           r-htmltools
           r-magrittr
           r-matrixstats
           r-mixomics
           r-pcapp
           r-purrr
           r-readxl
           r-reshape2
           r-rlang
           r-rmarkdown
           r-scales
           r-signal
           r-speaq
           r-stringr
           r-tibble
           r-tidyr
           r-tidyselect
           r-vctrs))
    (native-inputs (list r-knitr))
    (home-page "https://sipss.github.io/AlpsNMR/")
    (synopsis "Automated spectral processing system for NMR")
    (description
     "This package reads Bruker @acronym{NMR, Nuclear Magnetic Resonance} data
directories both zipped and unzipped.  It provides automated and efficient
signal processing for untargeted NMR metabolomics.  It is able to interpolate
the samples, detect outliers, exclude regions, normalize, detect peaks, align
the spectra, integrate peaks, manage metadata and visualize the spectra.
After spectra processing, it can apply multivariate analysis on extracted
data.  Efficient plotting with 1-D data is also available.  Basic reading of
1D ACD/Labs exported JDX samples is also available.")
    (license license:expat)))

(define-public r-altcdfenvs
  (package
    (name "r-altcdfenvs")
    (version "2.66.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "altcdfenvs" version))
              (sha256
               (base32
                "08k3j3dc93vg5c9ccw7mc4fri9xbdj7rdjyj0h6ig0s5866fqvlb"))))
    (properties `((upstream-name . "altcdfenvs")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-affy
           r-biobase
           r-biocgenerics
           r-biostrings
           r-hypergraph
           r-makecdfenv
           r-s4vectors))
    (home-page "https://bioconductor.org/packages/altcdfenvs")
    (synopsis
     "Convenience data structures and functions to handle CDF environments")
    (description
     "The package is usable with Affymetrix GeneChip short oligonucleotide
arrays, and it can be adapted or extended to other platforms.  It is able to
modify or replace the grouping of probes in the probe sets.  Also, the package
contains simple functions to read R connections in the FASTA format and it can
create an alternative mapping from sequences.")
    (license license:gpl2+)))

(define-public r-aneufinder
  (package
    (name "r-aneufinder")
    (version "1.32.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "AneuFinder" version))
              (sha256
               (base32
                "0dgmg0slyrh025wgz299dzzmpirlmdhmabxspci1qfarakw5yicf"))))
    (build-system r-build-system)
    (native-inputs
     (list r-knitr))
    (propagated-inputs
     (list r-aneufinderdata
           r-bamsignals
           r-biocgenerics
           r-biostrings
           r-cowplot
           r-dnacopy
           r-doparallel
           r-ecp
           r-foreach
           r-genomeinfodb
           r-genomicalignments
           r-genomicranges
           r-ggdendro
           r-ggplot2
           r-ggrepel
           r-iranges
           r-mclust
           r-reshape2
           r-rsamtools
           r-s4vectors))
    (home-page "https://bioconductor.org/packages/AneuFinder/")
    (synopsis "Copy number variation analysis in single-cell-sequencing data")
    (description "This package implements functions for copy number variant
calling, plotting, export and analysis from whole-genome single cell
sequencing data.")
    (license license:artistic2.0)))

(define-public r-anf
  (package
    (name "r-anf")
    (version "1.26.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "ANF" version))
              (sha256
               (base32
                "00s2rlkh5snqmvlz5a21ilmm0ngh4g362y7iayr6f78nh8gwp617"))))
    (properties `((upstream-name . "ANF")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biobase
           r-igraph
           r-mass
           r-rcolorbrewer
           r-survival))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/ANF")
    (synopsis "Affinity network fusion for complex patient clustering")
    (description
     "The package @dfn{ANF}(Affinity Network Fusion) provides methods for affinity
matrix construction and fusion as well as spectral clustering.  This package is
used for complex patient clustering by integrating multi-omic data through affinity
network fusion.")
    (license license:gpl3)))

(define-public r-annmap
  (package
    (name "r-annmap")
    (version "1.46.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "annmap" version))
              (sha256
               (base32
                "1xijcm0c19wj2yknrlsz2ifm097z32rc58k608482686srj0bg2i"))))
    (properties `((upstream-name . "annmap")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biobase
           r-biocgenerics
           r-dbi
           r-digest
           r-genefilter
           r-genomicranges
           r-iranges
           r-lattice
           r-rmysql
           r-rsamtools))
    (home-page "https://github.com/cruk-mi/annmap")
    (synopsis
     "Genome annotation and visualisation for Affymetrix arrays and NGS analysis")
    (description
     "This package @code{annmap} provides annotation mappings for Affymetrix exon
arrays and coordinate based queries to support deep sequencing data analysis.
Database access is hidden behind the API which provides a set of functions such
as @code{genesInRange()}, @code{geneToExon()}, @code{exonDetails()}, etc.
Functions to plot gene architecture and BAM file data are also provided.")
    (license license:gpl2)))

(define-public r-antiprofiles
  (package
    (name "r-antiprofiles")
    (version "1.44.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "antiProfiles" version))
              (sha256
               (base32
                "0p2fhxzgn877mic2yv1sgcidgm74z8hxa3hdaawgvhhvi2j6v8wa"))))
    (properties `((upstream-name . "antiProfiles")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-locfit
           r-matrixstats))
    (home-page "https://github.com/HCBravoLab/antiProfiles")
    (synopsis "Implementation of gene expression anti-profiles")
    (description
     "This package implements the gene expression anti-profiles method.
Anti-profiles are a new approach for developing cancer genomic signatures that
specifically take advantage of gene expression heterogeneity.  They explicitly
model increased gene expression variability in cancer to define robust and
reproducible gene expression signatures capable of accurately distinguishing
tumor samples from healthy controls.")
    (license license:artistic2.0)))

(define-public r-arrayexpress
  (package
    (name "r-arrayexpress")
    (version "1.64.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "ArrayExpress" version))
              (sha256
               (base32
                "1ad6h0pi04r9m3nwxrn7sha837m95kwii8z6s7j4zbka9ijy2pr4"))))
    (properties `((upstream-name . "ArrayExpress")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biobase
           r-httr
           r-jsonlite
           r-limma
           r-oligo
           r-rlang))
    (home-page "https://bioconductor.org/packages/ArrayExpress")
    (synopsis "Building R objects from ArrayExpress datasets")
    (description
     "This package offers the possibility to access the ArrayExpress repository
at @dfn{EBI} (European Bioinformatics Institute) and build Bioconductor data
structures: @code{ExpressionSet}, @code{AffyBatch}, @code{NChannelSet}.")
    (license license:artistic2.0)))

(define-public r-arrayqualitymetrics
  (package
    (name "r-arrayqualitymetrics")
    (version "3.60.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "arrayQualityMetrics" version))
              (sha256
               (base32
                "04y1vbpxqhqn1vsyqmscikaxn6wshsk4krcqzf2baf9x4azbdjk1"))))
    (properties `((upstream-name . "arrayQualityMetrics")))
    (build-system r-build-system)
    (propagated-inputs (list r-affy
                             r-affyplm
                             r-beadarray
                             r-biobase
                             r-genefilter
                             r-gridsvg
                             r-hmisc
                             r-hwriter
                             r-lattice
                             r-latticeextra
                             r-limma
                             r-rcolorbrewer
                             r-setrng
                             r-svglite
                             r-vsn
                             r-xml))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/arrayQualityMetrics")
    (synopsis "Quality metrics report for microarray data sets")
    (description
     "This package generates microarray quality metrics reports for data in
Bioconductor microarray data containers @code{(ExpressionSet},
@code{NChannelSet}, @code{AffyBatch}).  One and two color array platforms are
supported.")
    (license license:lgpl2.0+)))

(define-public r-arraymvout
  (package
    (name "r-arraymvout")
    (version "1.62.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "arrayMvout" version))
       (sha256
        (base32 "11v3s972nyahdswqdcab0fj9jb73vd5dnd5kfb09572y4z0hn5xw"))))
    (properties `((upstream-name . "arrayMvout")))
    (build-system r-build-system)
    (arguments
     (list
      #:phases
      '(modify-phases %standard-phases
         (add-before 'install 'change-home-dir
           (lambda _
             ;; Change from /homeless-shelter to /tmp for write permission.
             (setenv "HOME" "/tmp"))))))
    (propagated-inputs
     (list r-affy
           r-affycontam
           r-biobase
           r-lumi
           r-mdqc
           r-parody))
    (home-page "https://bioconductor.org/packages/arrayMvout")
    (synopsis "Multivariate outlier detection for expression array QA")
    (description
     "This package supports the application of diverse quality metrics to
AffyBatch instances, summarizing these metrics via PCA, and then performing
parametric outlier detection on the PCs to identify aberrant arrays with a
fixed Type I error rate.")
    (license license:artistic2.0)))

(define-public r-arrayquality
  (package
    (name "r-arrayquality")
    (version "1.82.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "arrayQuality" version))
       (sha256
        (base32 "0wmczy2wdxki9pam9q6ly5clvqbl018v1ar4zkdm8algjddyc2cp"))))
    (properties `((upstream-name . "arrayQuality")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-gridbase
           r-hexbin
           r-limma
           r-marray
           r-rcolorbrewer))
    (home-page "http://arrays.ucsf.edu/")
    (synopsis "Assessing array quality on spotted arrays")
    (description
     "This package provides functions for performing print-run and array level
quality assessment.")
    (license license:lgpl2.0+)))

(define-public r-asafe
  (package
    (name "r-asafe")
    (version "1.30.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "ASAFE" version))
              (sha256
               (base32
                "13qf4jvf0m6i46bwqxdhd4zllac370ckwnd9xh6p56r1lrgcl4sa"))))
    (properties `((upstream-name . "ASAFE")))
    (build-system r-build-system)
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/ASAFE")
    (synopsis "Ancestry Specific Allele Frequency Estimation")
    (description
     "The @code{ASAFE} package contains a collection of functions that can be
used to carry out an @dfn{EM} (Expectation–maximization) algorithm to estimate
ancestry-specific allele frequencies for a bi-allelic genetic marker, e.g. an
@dfn{SNP} (single nucleotide polymorphism) from genotypes and ancestry
pairs.")
    (license license:artistic2.0)))

(define-public r-aseb
  (package
    (name "r-aseb")
    (version "1.48.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "ASEB" version))
              (sha256
               (base32
                "1h5rgy2n7rxd72ypzwnfclw8h5hvdnac48fgbfll669z2k97dsmm"))))
    (properties `((upstream-name . "ASEB")))
    (build-system r-build-system)
    (home-page "https://bioconductor.org/packages/ASEB")
    (synopsis "Predict acetylated lysine sites")
    (description
     "ASEB is an R package to predict lysine sites that can be acetylated by a
specific @dfn{KAT} (K-acetyl-transferases) family.  Lysine acetylation is a
well-studied posttranslational modification on kinds of proteins.  About four
thousand lysine acetylation sites and over 20 lysine KATs have been
identified.  However, which KAT is responsible for a given protein or lysine
site acetylation is mostly unknown.  In this package, we use a
@dfn{GSEA}-like (Gene Set Enrichment Analysis) method to make predictions.
GSEA method was developed and successfully used to detect coordinated
expression changes and find the putative functions of the long non-coding
RNAs.")
    (license license:gpl3+)))

(define-public r-asgsca
  (package
    (name "r-asgsca")
    (version "1.38.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "ASGSCA" version))
              (sha256
               (base32
                "0gvpb1pspm7zlllg77fbi3g6csa3cpp2lbljna2m0xfi0c5zdqvw"))))
    (properties `((upstream-name . "ASGSCA")))
    (build-system r-build-system)
    (propagated-inputs (list r-mass r-matrix))
    (home-page "https://bioconductor.org/packages/ASGSCA")
    (synopsis "Analysis of associations between multiple genotypes and traits")
    (description
     "The package @dfn{ASGSCA} (Association Study using Generalized Structured
Component Analysis) provides tools to model and test the association between
multiple genotypes and multiple traits, taking into account the prior
biological knowledge.  Genes, and clinical pathways are incorporated in the
model as latent variables.")
    (license license:gpl3)))

(define-public r-asics
  (package
    (name "r-asics")
    (version "2.20.1")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "ASICS" version))
              (sha256
               (base32
                "0halznbl3bflnn38rh9h5a5f0dsc88n6x6kwdffbpnin9i192q1k"))))
    (properties `((upstream-name . "ASICS")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocparallel
           r-ggplot2
           r-glmnet
           r-gridextra
           r-matrix
           r-mvtnorm
           r-pepsnmr
           r-plyr
           r-quadprog
           r-ropls
           r-summarizedexperiment
           r-zoo))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/ASICS")
    (synopsis "Automatic statistical identification in complex spectra")
    (description
     "ASICS quantifies concentration of metabolites in a complex spectrum.
The identification of metabolites is performed by fitting a mixture model to
the spectra of the library with a sparse penalty.")
    (license license:gpl2+)))

(define-public r-aspli
  (package
    (name "r-aspli")
    (version "2.14.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "ASpli" version))
              (sha256
               (base32
                "1br4apbc3zn06ilg8fjb8xcxaimz4h7rnx9i60ak4wnpvgzgbyh2"))))
    (properties `((upstream-name . "ASpli")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotationdbi
           r-biocgenerics
           r-biocstyle
           r-data-table
           r-dt
           r-edger
           r-genomicalignments
           r-genomicfeatures
           r-genomicranges
           r-gviz
           r-htmltools
           r-igraph
           r-iranges
           r-limma
           r-mass
           r-pbmcapply
           r-rsamtools
           r-s4vectors
           r-tidyr
           r-txdbmaker
           r-upsetr))
    (home-page "https://bioconductor.org/packages/ASpli")
    (synopsis "Analysis of alternative splicing using RNA-Seq")
    (description
     "@dfn{AS} (alternative splicing) is a common mechanism of
post-transcriptional gene regulation in eukaryotic organisms that expands the
functional and regulatory diversity of a single gene by generating multiple
mRNA isoforms that encode structurally and functionally distinct proteins.
ASpli is an integrative pipeline and user-friendly R package that facilitates
the analysis of changes in both annotated and novel AS events.  ASpli
integrates several independent signals in order to deal with the complexity
that might arise in splicing patterns.")
    ;; The authors didn't specify any GPL version in description or in the
    ;; sources.
    (license (list license:gpl2+ license:gpl3+))))

(define-public r-assessorf
  (package
    (name "r-assessorf")
    (version "1.22.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "AssessORF" version))
              (sha256
               (base32
                "1gzaafsfqzy2x35i251gvkd1syif80qb08kkvgnf1i6c8d2czmps"))))
    (properties `((upstream-name . "AssessORF")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biostrings
           r-decipher
           r-genomicranges
           r-iranges))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/AssessORF")
    (synopsis "Assess gene predictions using proteomics and evolutionary conservation")
    (description
     "In order to assess the quality of a set of predicted genes for a genome,
evidence must first be mapped to that genome.  Next, each gene must be
categorized based on how strong the evidence is for or against that gene.  The
AssessORF package provides the functions and class structures necessary for
accomplishing those tasks, using proteomics hits and evolutionarily conserved
start codons as the forms of evidence.")
    (license license:gpl3)))

(define-public r-asset
  (package
    (name "r-asset")
    (version "2.22.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "ASSET" version))
              (sha256
               (base32
                "1wqsnp7w3ml7azrx72fafcfbmjg4324jaxcijpcrlrrccxl5rwp4"))))
    (properties `((upstream-name . "ASSET")))
    (build-system r-build-system)
    (propagated-inputs (list r-mass r-msm r-rmeta))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/ASSET")
    (synopsis
     "Subset-based association analysis of heterogeneous traits and subtypes")
    (description
     "This package is an R program for the subset-based analysis of
heterogeneous traits and disease subtypes.  ASSET allows the user to search
through all possible subsets of z-scores to identify the subset of traits
giving the best meta-analyzed z-score.  Further, it returns a p-value
adjusting for the multiple-testing involved in the search.  It also allows for
searching for the best combination of disease subtypes associated with each
variant.")
    (license license:gpl2)))

(define-public r-atena
  (package
    (name "r-atena")
    (version "1.10.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "atena" version))
              (sha256
               (base32
                "1y2v8mi9awgfbcawpcir01qglxn0bdgj6v2dsdqp6if25cizl3sr"))))
    (properties `((upstream-name . "atena")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotationhub
           r-biocgenerics
           r-biocparallel
           r-cli
           r-genomeinfodb
           r-genomicalignments
           r-genomicfeatures
           r-genomicranges
           r-iranges
           r-matrix
           r-matrixgenerics
           r-matrixstats
           r-rsamtools
           r-s4vectors
           r-sparsematrixstats
           r-squarem
           r-summarizedexperiment))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/functionalgenomics/atena")
    (synopsis "Analysis of transposable elements")
    (description
     "The atena package quantifies expression of @dfn{TEs} (transposable
elements) from RNA-seq data through different methods, including ERVmap,
TEtranscripts and Telescope.  A common interface is provided to use each of
these methods, which consists of building a parameter object, calling the
quantification function with this object and getting a
@code{SummarizedExperiment} object as an output container of the quantified
expression profiles.  The implementation allows quantifing TEs and gene
transcripts in an integrated manner.")
    (license license:artistic2.0)))

(define-public r-atsnp
  (package
    (name "r-atsnp")
    (version "1.20.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "atSNP" version))
              (sha256
               (base32
                "08pcbys8iy8wzqzzvlnwc3qw7m7cswkqa3x2x2x8m4ni1s4isd6z"))))
    (properties `((upstream-name . "atSNP")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocfilecache
           r-biocparallel
           r-bsgenome
           r-data-table
           r-ggplot2
           r-lifecycle
           r-motifstack
           r-rappdirs
           r-rcpp
           r-testthat))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/sunyoungshin/atSNP")
    (synopsis
     "Affinity test for identifying regulatory single nucleotide polymorphisms")
    (description
     "The atSNP package performs affinity tests of motif matches with the
@dfn{SNP} (single nucleotide polymorphism) or the reference genomes and
SNP-led changes in motif matches.")
    (license license:gpl2)))

(define-public r-attract
  (package
    (name "r-attract")
    (version "1.56.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "attract" version))
              (sha256
               (base32
                "08l634sk6y06y2m78vsd0ld4w3h6qvx4hk6wkqflkl9jjyqpx8jy"))))
    (properties `((upstream-name . "attract")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotationdbi
           r-biobase
           r-cluster
           r-gostats
           r-keggrest
           r-limma
           r-org-hs-eg-db
           r-reactome-db))
    (home-page "https://bioconductor.org/packages/attract")
    (synopsis "Finding drivers of Kauffman's attractor landscape")
    (description
     "This package contains the functions to find the gene expression modules
that represent the drivers of Kauffman's attractor landscape.  The modules are
the core attractor pathways that discriminate between different cell types of
groups of interest.  Each pathway has a set of synexpression groups, which show
transcriptionally-coordinated changes in gene expression.")
    (license license:lgpl2.0+)))

(define-public r-awfisher
  (package
    (name "r-awfisher")
    (version "1.18.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "AWFisher" version))
              (sha256
               (base32
                "0z5qs337zqqri2g3m69m85xg4x5sxnqlkbwz83vhnqfzmy9nqym0"))))
    (properties `((upstream-name . "AWFisher")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-edger
           r-limma))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/AWFisher")
    (synopsis  "Fast computing for adaptively weighted fisher's method")
    (description
     "This package is an implementation of the Adaptively Weighted Fisher's
method, including fast p-value computing, variability index, and
meta-pattern.")
    (license license:gpl3)))

(define-public r-awst
  (package
    (name "r-awst")
    (version "1.12.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "awst" version))
              (sha256
               (base32
                "1iaakdjxrz4szl4lnjcszy47q5dsh2d7i3wrr3wwdhw6wjcd62hj"))))
    (properties `((upstream-name . "awst")))
    (build-system r-build-system)
    (propagated-inputs (list r-summarizedexperiment))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/drisso/awst")
    (synopsis "Asymmetric within-sample transformation")
    (description
     "This package @dfn{awst} (Asymmetric Within-Sample Transformation) that
regularizes RNA-seq read counts and reduces the effect of noise on the
classification of samples.  AWST comprises two main steps: standardization and
smoothing.  These steps transform gene expression data to reduce the noise of
the lowly expressed features, which suffer from background effects and low
signal-to-noise ratio, and the influence of the highly expressed features,
which may be the result of amplification bias and other experimental
artifacts.")
    (license license:expat)))

(define-public r-baalchip
  (package
    (name "r-baalchip")
    (version "1.30.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "BaalChIP" version))
              (sha256
               (base32
                "1xm6czgbfz2rjrz5996d0n7xmlh9pd1mw9i8ac66pmn5fi44pbz2"))))
    (properties `((upstream-name . "BaalChIP")))
    (build-system r-build-system)
    (inputs (list)) ; extra/get.overlaps.v2_chrXY.perl
    (propagated-inputs
     (list r-coda
           r-doby
           r-doparallel
           r-foreach
           r-genomeinfodb
           r-genomicalignments
           r-genomicranges
           r-ggplot2
           r-iranges
           r-reshape2
           r-rsamtools
           r-scales))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/BaalChIP")
    (synopsis
     "Analysis of allele-specific transcription factor binding in cancer genomes")
    (description
     "This package offers functions to process multiple @code{ChIP-seq BAM}
files and detect allele-specific events.  It computes allele counts at
individual variants (SNPs/SNVs), implements extensive @dfn{QC} (quality
control) steps to remove problematic variants, and utilizes a Bayesian
framework to identify statistically significant allele-specific events.
BaalChIP is able to account for copy number differences between the two
alleles, a known phenotypical feature of cancer samples.")
    (license license:artistic2.0)))

(define-public r-bags
  (package
    (name "r-bags")
    (version "2.44.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BAGS" version))
       (sha256
        (base32 "1xc7gp5fljqz26lj8sjcilm01g636p4wlzy4pzy5wgqxgfcw3zps"))))
    (properties `((upstream-name . "BAGS")))
    (build-system r-build-system)
    (propagated-inputs (list r-biobase r-breastcancervdx))
    (home-page "https://bioconductor.org/packages/BAGS")
    (synopsis "Bayesian approach for geneset selection")
    (description
     "This R package is providing functions to perform geneset significance
analysis over simple cross-sectional data between 2 and 5 phenotypes of
interest.")
    (license license:artistic2.0)))

(define-public r-basespacer
  (package
    (name "r-basespacer")
    (version "1.48.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "BaseSpaceR" version))
              (sha256
               (base32
                "02f2l42p08xags155qydz97ipg5vya0prmrlw4kvahgbq5p0xm44"))))
    (properties `((upstream-name . "BaseSpaceR")))
    (build-system r-build-system)
    (propagated-inputs (list r-rcurl r-rjsonio))
    (home-page "https://bioconductor.org/packages/BaseSpaceR")
    (synopsis "R SDK for BaseSpace RESTful API")
    (description
     "This package provides an R interface to Illumina's BaseSpace cloud
computing environment, enabling the fast development of data analysis and
visualization tools.  Besides providing an easy to use set of tools for
manipulating the data from BaseSpace, it also facilitates the access to R's
rich environment of statistical and data analysis tools.")
    (license license:asl2.0)))

(define-public r-bac
  (package
    (name "r-bac")
    (version "1.58.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "BAC" version))
              (sha256
               (base32
                "00dkhns9n1x4wmlxjcw75h7iwwk37zlv1c2fi0g1mmsw1xvdjzp6"))))
    (properties `((upstream-name . "BAC")))
    (build-system r-build-system)
    (home-page "https://bioconductor.org/packages/BAC")
    (synopsis "Bayesian analysis of Chip-chip experiment")
    (description
     "This package uses a Bayesian hierarchical model to detect enriched
regions from ChIP-chip experiments.  The common goal in analyzing this
ChIP-chip data is to detect DNA-protein interactions from ChIP-chip
experiments.  The BAC package has mainly been tested with Affymetrix tiling
array data.  However, we expect it to work with other platforms (e.g. Agilent,
Nimblegen, cDNA, etc.).  Note that BAC does not deal with normalization, so
you will have to normalize your data beforehand.")
    (license license:artistic2.0)))

(define-public r-bader
  (package
    (name "r-bader")
    (version "1.42.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "BADER" version))
              (sha256
               (base32
                "15lsvfmp4qsrv12bcrz5q8yz8xyg9rckrcflr3k6si03z2vw812p"))))
    (properties `((upstream-name . "BADER")))
    (build-system r-build-system)
    (home-page "https://bioconductor.org/packages/BADER")
    (synopsis
     "Bayesian analysis of differential expression in RNA sequencing data")
    (description
     "The BADER package is intended for the analysis of RNA sequencing data.
The algorithm fits a Bayesian hierarchical model for RNA sequencing count
data.  BADER returns the posterior probability of differential expression for
each gene between two groups A and B.  The joint posterior distribution of the
variables in the model can be returned in the form of posterior samples, which
can be used for further down-stream analyses such as gene set enrichment.")
    (license license:gpl2)))

(define-public r-badregionfinder
  (package
    (name "r-badregionfinder")
    (version "1.32.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "BadRegionFinder" version))
              (sha256
               (base32
                "1hs1mv8gj4w0wk6sw6r08j237vw3lhjrv9zip5nk6l1g1r0pg597"))))
    (properties `((upstream-name . "BadRegionFinder")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biomart
           r-genomicranges
           r-rsamtools
           r-s4vectors
           r-variantannotation))
    (home-page "https://bioconductor.org/packages/BadRegionFinder")
    (synopsis "Identifying regions with bad coverage in sequence alignment data")
    (description
     "BadRegionFinder is a package for identifying regions with a bad,
acceptable and good coverage in sequence alignment data available as bam
files.  The whole genome may be considered as well as a set of target regions.
Various visual and textual types of output are available.")
    (license license:lgpl3)))

(define-public r-bambu
  (package
    (name "r-bambu")
    (version "3.6.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "bambu" version))
              (sha256
               (base32
                "1pn6a74nvcpn0xzfb8167hbc6pdma0ybhh7kjpqdl4hgjp446w5w"))))
    (properties `((upstream-name . "bambu")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocgenerics
           r-biocparallel
           r-bsgenome
           r-data-table
           r-dplyr
           r-genomeinfodb
           r-genomicalignments
           r-genomicfeatures
           r-genomicranges
           r-iranges
           r-rcpp
           r-rcpparmadillo
           r-rsamtools
           r-s4vectors
           r-summarizedexperiment
           r-tidyr
           r-xgboost))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/GoekeLab/bambu")
    (synopsis
     "Isoform reconstruction and quantification for long read RNA-Seq data")
    (description
     "This R package is for multi-sample transcript discovery and
quantification using long read RNA-Seq data.  You can use bambu after read
alignment to obtain expression estimates for known and novel transcripts and
genes.  The output from bambu can directly be used for visualisation and
downstream analysis, such as differential gene expression or transcript
usage.")
    (license license:gpl3)))

(define-public r-bandits
  (package
    (name "r-bandits")
    (version "1.20.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "BANDITS" version))
              (sha256
               (base32
                "1iizs0bvs7rbkwv3vs6xk26jy222kql80fp6cki0qs9h9igpzk5h"))))
    (properties `((upstream-name . "BANDITS")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocparallel
           r-data-table
           r-doparallel
           r-dorng
           r-drimseq
           r-foreach
           r-ggplot2
           r-mass
           r-r-utils
           r-rcpp
           r-rcpparmadillo))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/SimoneTiberi/BANDITS")
    (synopsis "Bayesian analysis of differential splicing")
    (description
     "BANDITS is a Bayesian hierarchical model for detecting differential
splicing of genes and transcripts, via @dfn{DTU} (differential transcript
usage), between two or more conditions.  The method uses a Bayesian
hierarchical framework, which allows for sample specific proportions in a
Dirichlet-Multinomial model, and samples the allocation of fragments to the
transcripts.  Parameters are inferred via @dfn{MCMC} (Markov chain Monte
Carlo) techniques and a DTU test is performed via a multivariate Wald test on
the posterior densities for the average relative abundance of transcripts.")
    (license license:gpl3+)))

(define-public r-banocc
  (package
    (name "r-banocc")
    (version "1.28.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "banocc" version))
              (sha256
               (base32
                "12fysw9m3rrmyfq1j97rdklaln6vgjdmz7hj85vrh10haz9j23s2"))))
    (properties `((upstream-name . "banocc")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-coda
           r-mvtnorm
           r-rstan
           r-stringr))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/banocc")
    (synopsis "Bayesian analysis of compositional covariance")
    (description
     "BAnOCC is a package designed for compositional data, where each sample
sums to one.  It infers the approximate covariance of the unconstrained data
using a Bayesian model coded with @code{rstan}.  It provides as output the
@code{stanfit} object as well as posterior median and credible interval
estimates for each correlation element.")
    (license license:expat)))

(define-public r-barcodetrackr
  (package
    (name "r-barcodetrackr")
    (version "1.12.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "barcodetrackR" version))
              (sha256
               (base32
                "0x0fpa44lyww2l1rdc35pdlgczz4y1vxjbn0s1frlpaacmqcxrly"))))
    (properties `((upstream-name . "barcodetrackR")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-circlize
           r-cowplot
           r-dplyr
           r-ggdendro
           r-ggplot2
           r-ggridges
           r-magrittr
           r-plyr
           r-proxy
           r-rcolorbrewer
           r-rlang
           r-s4vectors
           r-scales
           r-shiny
           r-summarizedexperiment
           r-tibble
           r-tidyr
           r-vegan
           r-viridis))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/dunbarlabNIH/barcodetrackR")
    (synopsis "Functions for analyzing cellular barcoding data")
    (description
     "This package is developed for the analysis and visualization of clonal
tracking data.  The required data is formed by samples and tag abundances in
matrix form, usually from cellular barcoding experiments, integration site
retrieval analyses, or similar technologies.")
    (license license:cc0)))

(define-public r-biocversion
  (package
    (name "r-biocversion")
    (version "3.19.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BiocVersion" version))
       (sha256
        (base32
         "1zhm28313gsbscfhfdbqizvb5v67n4l0dapjmb1xq010ikly95mh"))))
    (properties `((upstream-name . "BiocVersion")))
    (build-system r-build-system)
    (home-page "https://bioconductor.org/packages/BiocVersion/")
    (synopsis "Set the appropriate version of Bioconductor packages")
    (description
     "This package provides repository information for the appropriate version
of Bioconductor.")
    (license license:artistic2.0)))

(define-public r-biocgenerics
  (package
    (name "r-biocgenerics")
    (version "0.50.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "BiocGenerics" version))
              (sha256
               (base32
                "0500ya3df53xx1dfdmk50bh30ja28y6qkdwds1c1m4c7q0h2xmqi"))))
    (properties
     `((upstream-name . "BiocGenerics")))
    (build-system r-build-system)
    (home-page "https://bioconductor.org/packages/BiocGenerics")
    (synopsis "S4 generic functions for Bioconductor")
    (description
     "This package provides S4 generic functions needed by many Bioconductor
packages.")
    (license license:artistic2.0)))

(define-public r-breakpointr
  (package
    (name "r-breakpointr")
    (version "1.22.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "breakpointR" version))
              (sha256
               (base32
                "0imbsqrslclvfyvc00sv6ngvl5hqlr6ian07v521m63ffpx6ll78"))))
    (properties `((upstream-name . "breakpointR")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocgenerics
           r-breakpointrdata
           r-cowplot
           r-doparallel
           r-foreach
           r-genomeinfodb
           r-genomicalignments
           r-genomicranges
           r-ggplot2
           r-gtools
           r-iranges
           r-rsamtools
           r-s4vectors))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/daewoooo/BreakPointR")
    (synopsis "Find breakpoints in Strand-seq data")
    (description
     "This package implements functions for finding breakpoints, plotting and
export of Strand-seq data.")
    (license license:expat)))

(define-public r-cardelino
  (package
    (name "r-cardelino")
    (version "1.6.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "cardelino" version))
              (sha256
               (base32
                "16mczwh41gf7ikbzxvggai7a8d64fn2k7q9w1yxf0sn6cwg3l0di"))))
    (properties `((upstream-name . "cardelino")))
    (build-system r-build-system)
    (propagated-inputs (list r-combinat
                             r-genomeinfodb
                             r-genomicranges
                             r-ggplot2
                             r-ggtree
                             r-matrix
                             r-matrixstats
                             r-pheatmap
                             r-s4vectors
                             r-snpstats
                             r-variantannotation
                             r-vcfr))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/single-cell-genetics/cardelino")
    (synopsis "Clone identification from single cell data")
    (description
     "This package provides methods to infer clonal tree configuration for a
population of cells using single-cell RNA-seq data (scRNA-seq), and possibly
other data modalities.  Methods are also provided to assign cells to inferred
clones and explore differences in gene expression between clones.  These
methods can flexibly integrate information from imperfect clonal trees
inferred based on bulk exome-seq data, and sparse variant alleles expressed in
scRNA-seq data.  A flexible beta-binomial error model that accounts for
stochastic dropout events as well as systematic allelic imbalance is used.")
    (license license:gpl3)))

(define-public r-cellid
  (package
    (name "r-cellid")
    (version "1.12.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "CelliD" version))
              (sha256
               (base32
                "0c7hls5qasjd5ahrqxp2mpd402jc12pi0pgy7spcbp1fkqm8qvbl"))))
    (properties `((upstream-name . "CelliD")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocparallel
           r-data-table
           r-fastmatch
           r-fgsea
           r-ggplot2
           r-glue
           r-irlba
           r-matrix
           r-matrixstats
           r-pbapply
           r-rcpp
           r-rcpparmadillo
           r-reticulate
           r-rtsne
           r-scater
           r-seurat
           r-singlecellexperiment
           r-stringr
           r-summarizedexperiment
           r-tictoc
           r-umap))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/CelliD")
    (synopsis
     "Single cell gene signature extraction using multiple correspondence analysis")
    (description
     "CelliD is a clustering-free method for extracting per-cell gene
signatures from scRNA-seq.  CelliD allows unbiased cell identity recognition
across different donors, tissues-of-origin, model organisms and single-cell
omics protocols.  The package can also be used to explore functional pathways
enrichment in single cell data.")
    (license license:gpl3)))

(define-public r-coregx
  (package
    (name "r-coregx")
    (version "2.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "CoreGx" version))
       (sha256
        (base32 "05fyncpqiq08a169rjwyc117wkyckh714z0is2r0g79920i36jas"))))
    (properties `((upstream-name . "CoreGx")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-bench
           r-biobase
           r-biocgenerics
           r-biocparallel
           r-bumpymatrix
           r-checkmate
           r-crayon
           r-data-table
           r-glue
           r-lsa
           r-matrixgenerics
           r-multiassayexperiment
           r-piano
           r-rlang
           r-s4vectors
           r-summarizedexperiment))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/CoreGx")
    (synopsis
     "Classes and functions to serve as the basis for other Gx packages")
    (description
     "This package provides a collection of functions and classes which serve
as the foundation for packages such as PharmacoGx and RadioGx.  It was created
to abstract shared functionality to increase ease of maintainability and
reduce code repetition in current and future Gx suite programs.  Major
features include a @code{CoreSet} class, from which RadioSet and PharmacoSet
are derived, along with get and set methods for each respective slot.
Additional functions related to fitting and plotting dose response curves,
quantifying statistical correlation and calculating @acronym{AUC, area under
the curve} or @acronym{SF, survival fraction} are included.")
    (license license:gpl3+)))

(define-public r-coverageview
  (package
    (name "r-coverageview")
    (version "1.42.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "CoverageView" version))
              (sha256
               (base32
                "1wgkn2adcxl6js3cvawhvzpxm3nmnnc16wbnzkrnsnd9l12aasih"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-genomicalignments
           r-genomicranges
           r-iranges
           r-rsamtools
           r-rtracklayer
           r-s4vectors))
    (home-page "https://bioconductor.org/packages/CoverageView/")
    (synopsis "Coverage visualization package for R")
    (description "This package provides a framework for the visualization of
genome coverage profiles.  It can be used for ChIP-seq experiments, but it can
be also used for genome-wide nucleosome positioning experiments or other
experiment types where it is important to have a framework in order to inspect
how the coverage distributed across the genome.")
    (license license:artistic2.0)))

(define-public r-cummerbund
  (package
   (name "r-cummerbund")
   (version "2.46.0")
   (source (origin
             (method url-fetch)
             (uri (bioconductor-uri "cummeRbund" version))
             (sha256
              (base32
               "0hdm2214waz02ybr264xis48kj2anpad80jk640p49v9r17iia9l"))))
   (build-system r-build-system)
   (propagated-inputs
    (list r-biobase
          r-biocgenerics
          r-fastcluster
          r-ggplot2
          r-gviz
          r-plyr
          r-reshape2
          r-rsqlite
          r-rtracklayer
          r-s4vectors))
   (home-page "https://bioconductor.org/packages/cummeRbund/")
   (synopsis "Analyze Cufflinks high-throughput sequencing data")
   (description "This package allows for persistent storage, access,
exploration, and manipulation of Cufflinks high-throughput sequencing
data.  In addition, provides numerous plotting functions for commonly
used visualizations.")
   (license license:artistic2.0)))

(define-public r-dama
  (package
    (name "r-dama")
    (version "1.76.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "daMA" version))
              (sha256
               (base32
                "1hv43r6rymp09fl7vgr8jf6naixkrgl282ggf6nva0jxf1jnfmb3"))))
    (properties `((upstream-name . "daMA")))
    (build-system r-build-system)
    (propagated-inputs (list r-mass))
    (home-page "https://bioconductor.org/packages/release/bioc/html/daMA.html")
    (synopsis
     "Efficient design and analysis of factorial two-colour microarray data")
    (description
     "This package contains functions for the efficient design of factorial
two-colour microarray experiments and for the statistical analysis of
factorial microarray data.")
    (license license:gpl2+)))

(define-public r-damefinder
  (package
    (name "r-damefinder")
    (version "1.16.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "DAMEfinder" version))
              (sha256
               (base32
                "1ffqa7a3a0f3m58j6al2hd7aa8bamv4z170y4yn1vr1f8jrb7kq5"))))
    (properties `((upstream-name . "DAMEfinder")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocgenerics
           r-biostrings
           r-bumphunter
           r-cowplot
           r-genomeinfodb
           r-genomicalignments
           r-genomicranges
           r-ggplot2
           r-iranges
           r-limma
           r-plyr
           r-readr
           r-reshape2
           r-rsamtools
           r-s4vectors
           r-stringr
           r-summarizedexperiment
           r-variantannotation))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/DAMEfinder")
    (synopsis "Differential allelicly methylated regions")
    (description
     "This package offers functionality for taking methtuple or Bismark
outputs to calculate @acronym{ASM, Allele-Specific Methylation} scores and
compute @acronym{DAMEs, Differential Allelicly MEthylated} regions.  It also
offers nice visualization of methyl-circle plots.")
    (license license:expat)))

(define-public r-dearseq
  (package
    (name "r-dearseq")
    (version "1.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "dearseq" version))
       (sha256
        (base32
         "0hy8cnqyvp2mrhj5wi1shh1cba79c5z8f17mfw764s2d6ivsfzz6"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-compquadform
           r-dplyr
           r-ggplot2
           r-kernsmooth
           r-magrittr
           r-matrixstats
           r-patchwork
           r-pbapply
           r-reshape2
           r-rlang
           r-scattermore
           r-statmod
           r-survey
           r-tibble
           r-viridislite))
    (native-inputs
     (list r-knitr))
    (home-page "https://github.com/borishejblum/dearseq")
    (synopsis "DEA for RNA-seq data through a robust variance component test")
    (description
     "This is a package for Differential Expression Analysis of RNA-seq data.
It features a variance component score test accounting for data
heteroscedasticity through precision weights.  Perform both gene-wise and gene
set analyses, and can deal with repeated or longitudinal data.")
    (license license:gpl2)))

(define-public r-debcam
  (package
    (name "r-debcam")
    (version "1.22.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "debCAM" version))
              (sha256
               (base32
                "1lbgwv7009fpwqw6kl94gmm44xypn60r8kcm3fjzgbjbwijriwgq"))
              (snippet
               '(for-each delete-file
                          '("inst/java/CornerDetect.jar"
                            "inst/java/lib/pj20150107.jar")))))
    (properties `((upstream-name . "debCAM")))
    (build-system r-build-system)
    (arguments
     (list
      #:configure-flags '(list "--fake")
      #:modules
      '((guix build r-build-system)
        ((guix build ant-build-system) #:prefix ant:)
        (guix build utils))
      #:imported-modules
      `((guix build ant-build-system)
        ,@%r-build-system-modules)
      #:phases
      '(modify-phases %standard-phases
         (add-after 'unpack 'build-jar
           (lambda* (#:key inputs #:allow-other-keys)
             (install-file
              (search-input-file inputs "/share/java/pj20150107.jar")
              "inst/java/lib")
             (with-directory-excursion "java"
               (mkdir "build")
               (invoke "javac" "-d" "./build"
                       "-cp" "../inst/java/lib/pj20150107.jar"
                       "CornerDetectTopN.java"
                       "FixSizedPriorityQueue.java")
               (with-directory-excursion "build"
                 (apply invoke "jar" "cvf" "../../inst/java/CornerDetect.jar"
                        (find-files "."))))))
         (add-after 'install 'strip-jar-timestamps
           (assoc-ref ant:%standard-phases 'strip-jar-timestamps)))))
    (inputs
     (list (list openjdk11 "jdk")
           java-pj))
    (propagated-inputs
     (list r-apcluster
           r-biobase
           r-biocparallel
           r-corpcor
           r-dmwr2
           r-geometry
           r-nmf
           r-nnls
           r-pcapp
           r-rjava
           r-summarizedexperiment))
    (native-inputs
     (list r-knitr zip)) ;zip is needed for repacking the jar
    (home-page "https://bioconductor.org/packages/debCAM")
    (synopsis "Deconvolution by convex analysis of mixtures")
    (description
     "This package is an R implementation for fully unsupervised deconvolution
of complex tissues.  DebCAM provides basic functions to perform unsupervised
deconvolution on mixture expression profiles by @acronym{CAM, Convex Analysis
of Mixtures} and some auxiliary functions to help understand the
subpopulation- specific results.  It also implements functions to perform
supervised deconvolution based on prior knowledge of molecular markers, S
matrix or A matrix.  Combining molecular markers from CAM and from prior
knowledge can achieve semi-supervised deconvolution of mixtures.")
    (license license:gpl2)))

(define-public r-decipher
  (package
    (name "r-decipher")
    (version "3.0.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "DECIPHER" version))
              (sha256
               (base32
                "1bgzl3sy4l5xfgxrkw8z1dvygkn2dgcl2fl9ah13ggs8hc9h9nlm"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biostrings r-dbi r-iranges r-s4vectors r-xvector))
    (home-page "https://www.bioconductor.org/packages/DECIPHER/")
    (synopsis "Tools for deciphering and managing biological sequences")
    (description "This package provides a toolset for deciphering and managing
biological sequences.")
    (license license:gpl3)))

(define-public r-deco
  (package
    (name "r-deco")
    (version "1.13.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "deco" version))
              (sha256
               (base32
                "0d4abif3v62cbas6hl7pfw8q8jihh7nsra76k9cm6kz54qw4fbnw"))))
    (properties `((upstream-name . "deco")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-ade4
           r-annotationdbi
           r-biobase
           r-biocparallel
           r-biocstyle
           r-cluster
           r-foreign
           r-gdata
           r-ggplot2
           r-gplots
           r-gridextra
           r-limma
           r-locfit
           r-made4
           r-rcolorbrewer
           r-reshape2
           r-scatterplot3d
           r-sfsmisc
           r-summarizedexperiment))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/fjcamlab/deco")
    (synopsis "Decomposing heterogeneous cohorts using omic data profiling")
    (description
     "This package discovers differential features in hetero- and homogeneous
omic data by a two-step method including subsampling LIMMA and NSCA.  DECO
reveals feature associations to hidden subclasses not exclusively related to
higher deregulation levels.")
    (license license:gpl3+)))

(define-public r-decomplexdisease
  (package
    (name "r-decomplexdisease")
    (version "1.18.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "DEComplexDisease" version))
              (sha256
               (base32
                "12gw9b0gdwyih51j2gzay6vxhycgc52n8svd0slv6wsbw5rc19lh"))))
    (properties `((upstream-name . "DEComplexDisease")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocparallel
           r-complexheatmap
           r-deseq2
           r-edger
           r-rcpp
           r-summarizedexperiment))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/DEComplexDisease")
    (synopsis "Investigations of complex diseases by bi-clustering analysis")
    (description
     "DEComplexDisease is designed to find the @acronym{DEGs, Differential
Expressed Genes} for complex disease, which is characterized by the
heterogeneous genomic expression profiles.  Different from the established DEG
analysis tools, it does not assume the patients of complex diseases to share
the common DEGs.  By applying a bi-clustering algorithm, DEComplexDisease
finds the DEGs shared by as many patients.  Applying the DEComplexDisease
analysis results, users are possible to find the patients affected by the same
mechanism based on the shared signatures.")
    (license license:gpl3)))

(define-public r-decomptumor2sig
  (package
    (name "r-decomptumor2sig")
    (version "2.20.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "decompTumor2Sig" version))
              (sha256
               (base32
                "1pnxnxz9zsxqs7cbxgkjapr57k2m1d57w5hak0dwcy5zcxjzmai8"))))
    (properties `((upstream-name . "decompTumor2Sig")))
    (build-system r-build-system)
    (inputs (list perl))                ;script/extractSpecColumns.pl
    (propagated-inputs
     (list r-biocgenerics
           r-biostrings
           r-bsgenome-hsapiens-ucsc-hg19
           r-data-table
           r-genomeinfodb
           r-genomicfeatures
           r-genomicranges
           r-ggplot2
           r-ggseqlogo
           r-gridextra
           r-matrix
           r-plyr
           r-quadprog
           r-readxl
           r-s4vectors
           r-summarizedexperiment
           r-txdb-hsapiens-ucsc-hg19-knowngene
           r-variantannotation))
    (native-inputs (list r-knitr))
    (home-page "https://rmpiro.net/decompTumor2Sig/")
    (synopsis "Decomposition of individual tumors into mutational signatures")
    (description
     "The package uses quadratic programming for signature refitting, i.e., to
decompose the mutation catalog from an individual tumor sample into a set of
given mutational signatures (either Alexandrov-model signatures or
Shiraishi-model signatures), computing weights that reflect the contributions
of the signatures to the mutation load of the tumor.")
    (license license:gpl2)))

(define-public r-deconrnaseq
  (package
    (name "r-deconrnaseq")
    (version "1.46.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "DeconRNASeq" version))
              (sha256
               (base32
                "0hwvfsc48dakh3nvz60i92yciz2m5my9w57kzpyj4jyk92cs181g"))))
    (properties `((upstream-name . "DeconRNASeq")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-ggplot2
           r-limsolve
           r-pcamethods))
    (home-page "https://bioconductor.org/packages/DeconRNASeq")
    (synopsis
     "Deconvolution of heterogeneous tissue samples for mRNA-Seq data")
    (description
     "DeconSeq is an R package for deconvolution of heterogeneous tissues
based on mRNA-Seq data.  It models the expression levels from heterogeneous
cell populations in mRNA-Seq as the weighted average of expression from
different constituting cell types and predicted cell type proportions of
single expression profiles.")
    (license license:gpl2)))

(define-public r-decontam
  (package
    (name "r-decontam")
    (version "1.24.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "decontam" version))
              (sha256
               (base32
                "1xjd9jzxlj6gfd6fnygzm21q1wy5x5pl031sq1q0svq3jlnzvgb7"))))
    (properties `((upstream-name . "decontam")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-ggplot2 r-reshape2))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/benjjneb/decontam")
    (synopsis
     "Identification of contaminants in marker-gene and metagenomics data")
    (description
     "This package offers simple statistical identification of contaminating
sequence features in marker-gene or metagenomics data.  It works on any kind
of feature derived from environmental sequencing data (e.g. ASVs, OTUs,
taxonomic groups, MAGs, etc).  Requires DNA quantitation data or sequenced
negative control samples.")
    (license license:artistic2.0)))

(define-public r-deconvr
  (package
    (name "r-deconvr")
    (version "1.10.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "deconvR" version))
              (sha256
               (base32
                "181dxjj8bzy1b9xq9hd3m3ak1698w7j0sf8h9g0bhf3blzlj4pmi"))))
    (properties `((upstream-name . "deconvR")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-assertthat
           r-biocgenerics
           r-data-table
           r-dplyr
           r-e1071
           r-foreach
           r-genomicranges
           r-iranges
           r-magrittr
           r-mass
           r-matrixstats
           r-methylkit
           r-minfi
           r-nnls
           r-quadprog
           r-rsq
           r-s4vectors
           r-tidyr))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/BIMSBbioinfo/deconvR")
    (synopsis "Simulation and deconvolution of omic profiles")
    (description
     "This package provides a collection of functions designed for analyzing
deconvolution of the bulk sample(s) using an atlas of reference omic signature
profiles and a user-selected model.  Users are given the option to create or
extend a reference atlas and,also simulate the desired size of the bulk
signature profile of the reference cell types.  The package includes the
cell-type-specific methylation atlas and, Illumina Epic B5 probe ids that can
be used in deconvolution.  Additionally, we included @code{BSmeth2Probe}, to
make mapping WGBS data to their probe IDs easier.")
    (license license:artistic2.0)))

(define-public r-decoupler
  (package
    (name "r-decoupler")
    (version "2.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "decoupleR" version))
       (sha256
        (base32 "060ww5hlzk0dvh7a52i1nrz1s2xq5mmlw70rxpcs6d4j08k1cmic"))))
    (properties `((upstream-name . "decoupleR")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocparallel
           r-broom
           r-dplyr
           r-magrittr
           r-matrix
           r-parallelly
           r-purrr
           r-rlang
           r-stringr
           r-tibble
           r-tidyr
           r-tidyselect
           r-withr))
    (native-inputs (list r-knitr))
    (home-page "https://saezlab.github.io/decoupleR/")
    (synopsis "Computational methods to infer biological activities from omics data")
    (description
     "Many methods allow us to extract biological activities from omics data using
information from prior knowledge resources, reducing the dimensionality for
increased statistical power and better interpretability.  decoupleR is a
Bioconductor package containing different statistical methods to extract these
signatures within a unified framework.  decoupleR allows the user to flexibly
test any method with any resource.  It incorporates methods that take into
account the sign and weight of network interactions.  decoupleR can be used
with any omic, as long as its features can be linked to a biological process
based on prior knowledge.  For example, in transcriptomics gene sets regulated
by a transcription factor, or in phospho-proteomics phosphosites that are
targeted by a kinase.")
    (license license:gpl3)))

(define-public r-deepsnv
  (package
    (name "r-deepsnv")
    (version "1.50.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "deepSNV" version))
              (sha256
               (base32
                "1rcq2bg4z6z7czqi662w5gsdc1vd4p8vb353hqayhciv31dz51fr"))))
    (properties `((upstream-name . "deepSNV")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biostrings
           r-genomicranges
           r-iranges
           r-rhtslib
           r-summarizedexperiment
           r-variantannotation
           r-vgam))
    (native-inputs
     (list r-knitr))
    (home-page "https://github.com/gerstung-lab/deepSNV/")
    (synopsis "Detection of subclonal SNVs in deep sequencing data")
    (description
     "This package provides quantitative variant callers for detecting
subclonal mutations in ultra-deep (>=100x coverage) sequencing experiments.
The deepSNV algorithm is used for a comparative setup with a control experiment
of the same loci and uses a beta-binomial model and a likelihood ratio test to
discriminate sequencing errors and subclonal SNVs.  The shearwater algorithm
computes a Bayes classifier based on a beta-binomial model for variant calling
with multiple samples for precisely estimating model parameters - such as local
error rates and dispersion - and prior knowledge, e.g.  from variation data
bases such as COSMIC.")
    (license license:gpl3)))

(define-public r-degreport
  (package
    (name "r-degreport")
    (version "1.40.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "DEGreport" version))
              (sha256
               (base32
                "087c7x3vz8z3yz4jdwbcskdqmv3kgz4c4vi3r9gvvay5z36yhpl5"))
              (snippet
               '(delete-file "docs/jquery.sticky-kit.min.js"))))
    (properties `((upstream-name . "DEGreport")))
    (build-system r-build-system)
    (arguments
     (list
      #:phases
      '(modify-phases %standard-phases
         (add-after 'unpack 'process-javascript
           (lambda* (#:key inputs #:allow-other-keys)
             (invoke "esbuild"
                     (assoc-ref inputs "js-jquery-sticky-kit")
                     "--minify"
                     "--outfile=docs/jquery.sticky-kit.min.js"))))))
    (propagated-inputs (list r-biobase
                             r-biocgenerics
                             r-broom
                             r-circlize
                             r-cluster
                             r-complexheatmap
                             r-consensusclusterplus
                             r-cowplot
                             r-dendextend
                             r-deseq2
                             r-dplyr
                             r-edger
                             r-ggdendro
                             r-ggplot2
                             r-ggrepel
                             r-knitr
                             r-logging
                             r-magrittr
                             r-psych
                             r-rcolorbrewer
                             r-reshape
                             r-rlang
                             r-s4vectors
                             r-scales
                             r-stringi
                             r-stringr
                             r-summarizedexperiment
                             r-tibble
                             r-tidyr))
    (native-inputs
     `(("esbuild" ,esbuild)
       ("r-knitr" ,r-knitr)
       ("js-jquery-sticky-kit"
        ,(origin
           (method url-fetch)
           (uri "https://raw.githubusercontent.com/leafo/sticky-kit/\
v1.1.2/jquery.sticky-kit.js")
           (sha256
            (base32
             "17c3a1hqc3ybwj7hpw8prazajp2x98aq7nyfn71h6lzjvblq297g"))))))
    (home-page "https://lpantano.github.io/DEGreport/")
    (synopsis "Report of DEG analysis")
    (description
     "This is a package for creating na HTML report of differential expression
analyses of count data.  It integrates some of the code mentioned in DESeq2
and @code{edgeR} vignettes, and report a ranked list of genes according to the
fold changes mean and variability for each selected gene.")
    (license license:expat)))

(define-public r-delayedarray
  (package
    (name "r-delayedarray")
    (version "0.30.1")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "DelayedArray" version))
              (sha256
               (base32
                "0n6pparz2fg8wgrgcwciibqz3iyrnlmk5hdvnzjppcraxanz8sm6"))))
    (properties
     `((upstream-name . "DelayedArray")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocgenerics
           r-iranges
           r-matrix
           r-matrixgenerics
           r-s4arrays
           r-s4vectors
           r-sparsearray))
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/DelayedArray")
    (synopsis "Delayed operations on array-like objects")
    (description
     "Wrapping an array-like object (typically an on-disk object) in a
@code{DelayedArray} object allows one to perform common array operations on it
without loading the object in memory.  In order to reduce memory usage and
optimize performance, operations on the object are either delayed or executed
using a block processing mechanism.  Note that this also works on in-memory
array-like objects like @code{DataFrame} objects (typically with Rle columns),
@code{Matrix} objects, and ordinary arrays and data frames.")
    (license license:artistic2.0)))

(define-public r-densvis
  (package
    (name "r-densvis")
    (version "1.14.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "densvis" version))
              (sha256
               (base32
                "1xsafzcg6psb4ld7nk4wgdaa3xhy8i2v86asasf8h3mi1ms7lcv0"))))
    (properties `((upstream-name . "densvis")))
    (build-system r-build-system)
    (propagated-inputs (list r-assertthat
                             r-basilisk
                             r-irlba
                             r-rcpp
                             r-reticulate
                             r-rtsne))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/densvis")
    (synopsis
     "Density-preserving data visualization via non-linear dimensionality reduction")
    (description
     "This package implements the density-preserving modification to t-SNE and
UMAP described by Narayan et al. (2020) <doi:10.1101/2020.05.12.077776>.
den-SNE and densMAP aim to enable more accurate visual interpretation of
high-dimensional datasets by producing lower-dimensional embeddings that
accurately represent the heterogeneity of the original high-dimensional space,
enabling the identification of homogeneous and heterogeneous cell states.
This accuracy is accomplished by including in the optimisation process a term
which considers the local density of points in the original high-dimensional
space.  This can help to create visualisations that are more representative of
heterogeneity in the original high-dimensional space.")
    (license license:expat)))

(define-public r-derfinder
  (package
    (name "r-derfinder")
    (version "1.38.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "derfinder" version))
              (sha256
               (base32
                "1i4m20wb08zncksz415jl8bd5r4gdygk7324cp6xhijrc1pfzl1i"))))
    (properties `((upstream-name . "derfinder")))
    (build-system r-build-system)
    (propagated-inputs (list r-annotationdbi
                             r-biocgenerics
                             r-biocparallel
                             r-bumphunter
                             r-derfinderhelper
                             r-genomeinfodb
                             r-genomicalignments
                             r-genomicfeatures
                             r-genomicfiles
                             r-genomicranges
                             r-hmisc
                             r-iranges
                             r-qvalue
                             r-rsamtools
                             r-rtracklayer
                             r-s4vectors))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/lcolladotor/derfinder")
    (synopsis
     "Annotation-agnostic differential expression analysis of RNA-seq data")
    (description
     "This package provides functions for annotation-agnostic differential
expression analysis of RNA-seq data.  Two implementations of the DER Finder
approach are included in this package:

@enumerate
@item single base-level F-statistics and
@item DER identification at the expressed regions-level.
@end enumerate

The DER Finder approach can also be used to identify differentially bounded
@code{ChIP-seq} peaks.")
    (license license:artistic2.0)))

(define-public r-derfinderhelper
  (package
    (name "r-derfinderhelper")
    (version "1.38.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "derfinderHelper" version))
       (sha256
        (base32 "0186wjrszf6c5amibmdblabx3cfvq7p7k6kza38gahw0p7ibwv21"))))
    (properties `((upstream-name . "derfinderHelper")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-iranges r-matrix r-s4vectors))
    (native-inputs
     (list r-knitr))
    (home-page "https://github.com/leekgroup/derfinderHelper")
    (synopsis "Helper for derfinder")
    (description
     "This package speeds up the derfinder package when using multiple cores.
It is particularly useful when using BiocParallel and it helps reduce the time
spent loading the full derfinder package when running the F-statistics
calculation in parallel.")
    (license license:artistic2.0)))

(define-public r-dmrcate
  (package
    (name "r-dmrcate")
    (version "3.0.3")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "DMRcate" version))
              (sha256
               (base32
                "0xgsbsr3g04yiyjlc161424wcvlbik208sz0rzqssw4fj255i88q"))))
    (properties `((upstream-name . "DMRcate")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotationhub
           r-biomart
           r-bsseq
           r-edger
           r-experimenthub
           r-genomeinfodb
           r-genomicranges
           r-gviz
           r-iranges
           r-limma
           r-minfi
           r-missmethyl
           r-plyr
           r-s4vectors
           r-summarizedexperiment))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/DMRcate")
    (synopsis "Methylation array and sequencing spatial analysis methods")
    (description
     "This is a package for de novo identification and extraction of
@dfn{differentially methylated regions} (DMRs) from the human genome using
@dfn{Whole Genome Bisulfite Sequencing} (WGBS) and Illumina Infinium
Array (450K and EPIC) data.  It provides functionality for filtering probes
possibly confounded by SNPs and cross-hybridisation.  It includes
@code{GRanges} generation and plotting functions.")
    ;; GPLv3 with additional liability disclaimer.
    (license license:gpl3)))

(define-public r-drimseq
  (package
    (name "r-drimseq")
    (version "1.32.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "DRIMSeq" version))
       (sha256
        (base32 "10skx3n3r9vp1p1499ddccgabi7q3cy1dpi5pbp4ap0gff5ika5x"))))
    (properties `((upstream-name . "DRIMSeq")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocgenerics
           r-biocparallel
           r-edger
           r-genomicranges
           r-ggplot2
           r-iranges
           r-limma
           r-mass
           r-reshape2
           r-s4vectors))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/DRIMSeq")
    (synopsis "Differential transcript usage and tuQTL analyses with Dirichlet-multinomial model in RNA-seq")
    (description
     "The package provides two frameworks.  One for the differential
transcript usage analysis between different conditions and one for the tuQTL
analysis.  Both are based on modeling the counts of genomic features (i.e.,
transcripts) with the Dirichlet-multinomial distribution.  The package also
makes available functions for visualization and exploration of the data and
results.")
    (license license:gpl3+)))

(define-public r-dropletutils
  (package
    (name "r-dropletutils")
    (version "1.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "DropletUtils" version))
       (sha256
        (base32 "01y88cnii02sn1jgyvvmbx8j60qbzqyznqfxzv06yi4y0xj37klg"))))
    (properties `((upstream-name . "DropletUtils")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-beachmat
           r-bh
           r-biocgenerics
           r-biocparallel
           r-delayedarray
           r-delayedmatrixstats
           r-dqrng
           r-edger
           r-genomicranges
           r-hdf5array
           r-iranges
           r-matrix
           r-r-utils
           r-rcpp
           r-rhdf5
           r-rhdf5lib
           r-s4vectors
           r-scuttle
           r-singlecellexperiment
           r-summarizedexperiment))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/DropletUtils")
    (synopsis "Utilities for handling single-cell droplet data")
    (description
     "This package provides a number of utility functions for handling
single-cell RNA-seq data from droplet technologies such as 10X Genomics.  This
includes data loading from count matrices or molecule information files,
identification of cells from empty droplets, removal of barcode-swapped
pseudo-cells, and downsampling of the count matrix.")
    (license license:gpl3)))

;; This is a CRAN package, but it depends on r-limma from Bioconductor.
(define-public r-dsb
  (package
    (name "r-dsb")
    (version "1.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "dsb" version))
       (sha256
        (base32 "16y0l3nmg88ihsr03f5n151jirypf7pxy6q0cgx2vpmdnqynlh61"))))
    (properties `((upstream-name . "dsb")))
    (build-system r-build-system)
    (propagated-inputs (list r-limma r-magrittr r-mclust))
    (native-inputs (list r-knitr r-rmarkdown))
    (home-page "https://github.com/niaid/dsb")
    (synopsis
     "Normalize & denoise droplet single cell protein data (CITE-Seq)")
    (description
     "R-dsb improves protein expression analysis in droplet-based single-cell
studies.  The package specifically addresses noise in raw protein UMI counts
from methods like CITE-seq.  It identifies and removes two main sources of
noise—protein-specific noise from unbound antibodies and droplet/cell-specific
noise.  The package is applicable to various methods, including CITE-seq,
REAP-seq, ASAP-seq, TEA-seq, and Mission Bioplatform data.  Check the vignette
for tutorials on integrating dsb with Seurat and Bioconductor, and using dsb
in Python.")
    (license license:cc0)))

(define-public r-dss
  (package
    (name "r-dss")
    (version "2.52.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "DSS" version))
              (sha256
               (base32
                "1y9xg6ic3a03xswyfvyxdhnd7ngkljc96bm5fry4ni6kpa77bkv8"))))
    (properties `((upstream-name . "DSS")))
    (build-system r-build-system)
    (propagated-inputs (list r-biobase r-biocparallel r-bsseq))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/DSS")
    (synopsis "Dispersion shrinkage for sequencing data")
    (description
     "DSS is an R library performing differential analysis for count-based
sequencing data.  It detects @dfn{differentially expressed genes} (DEGs) from
RNA-seq, and differentially methylated loci or regions (DML/DMRs) from
@dfn{bisulfite sequencing} (BS-seq).  The core of DSS is a dispersion
shrinkage method for estimating the dispersion parameter from Gamma-Poisson or
Beta-Binomial distributions.")
    ;; Any version of the GPL
    (license (list license:gpl2+ license:gpl3+))))

(define-public r-dyndoc
  (package
    (name "r-dyndoc")
    (version "1.82.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "DynDoc" version))
              (sha256
               (base32
                "174jfdyq7g7mvn7wvmdkf36n0n969aqsg9bwd1nk6a5nw1khm7js"))))
    (properties `((upstream-name . "DynDoc")))
    (build-system r-build-system)
    (home-page "https://bioconductor.org/packages/DynDoc")
    (synopsis "Dynamic document tools")
    (description
     "This package provides a set of functions to create and interact with
dynamic documents and vignettes.")
    (license license:artistic2.0)))

(define-public r-bluster
  (package
   (name "r-bluster")
   (version "1.14.0")
   (source (origin
            (method url-fetch)
            (uri (bioconductor-uri "bluster" version))
            (sha256
             (base32
              "1bj7wx2dnwbrv2gxcmqmq815vawfiqj2xy3624fjli11r8c1hywa"))))
   (properties `((upstream-name . "bluster")))
   (build-system r-build-system)
   (propagated-inputs
    (list r-biocneighbors
          r-biocparallel
          r-cluster
          r-igraph
          r-matrix
          r-rcpp
          r-s4vectors))
   (native-inputs
    (list r-knitr))
   (home-page "https://bioconductor.org/packages/bluster")
   (synopsis "Clustering algorithms for Bioconductor")
   (description"This package wraps common clustering algorithms in an easily
extended S4 framework.  Backends are implemented for hierarchical, k-means
and graph-based clustering.  Several utilities are also provided to compare
and evaluate clustering results.")
   (license license:gpl3)))

(define-public r-ideoviz
  (package
    (name "r-ideoviz")
    (version "1.40.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "IdeoViz" version))
              (sha256
               (base32
                "0mq8qdhi2ccfa66rygn83hpm0fsmwiahpmrh8q522yl618mkrjcs"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biobase
           r-genomeinfodb
           r-genomicranges
           r-iranges
           r-rcolorbrewer
           r-rtracklayer))
    (home-page "https://bioconductor.org/packages/IdeoViz/")
    (synopsis "Plots data along a chromosomal ideogram")
    (description "This package provides functions to plot data associated with
arbitrary genomic intervals along chromosomal ideogram.")
    (license license:gpl2)))

(define-public r-infercnv
  (package
    (name "r-infercnv")
    (version "1.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "infercnv" version))
       (sha256
        (base32
         "1fi89fqzzb9n2c9jchkkgy3yq2fvisjcdavh9r455vmy0xqsyyyd"))))
    (properties `((upstream-name . "infercnv")))
    (build-system r-build-system)
    (inputs (list python))
    (propagated-inputs
     (list r-ape
           r-argparse
           r-biocgenerics
           r-catools
           r-coda
           r-coin
           r-digest
           r-doparallel
           r-dplyr
           r-edger
           r-fastcluster
           r-fitdistrplus
           r-foreach
           r-futile-logger
           r-future
           r-ggplot2
           r-gplots
           r-gridextra
           r-hiddenmarkov
           r-igraph
           r-matrix
           r-paralleldist
           r-phyclust
           r-rann
           r-rcolorbrewer
           r-reshape2
           r-rjags
           r-seurat
           r-singlecellexperiment
           r-summarizedexperiment
           r-tidyr))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/broadinstitute/inferCNV/wiki")
    (synopsis "Infer copy number variation from single-cell RNA-Seq data")
    (description
     "@code{InferCNV} is used to explore tumor single cell RNA-Seq data to identify
evidence for somatic large-scale chromosomal copy number alterations, such as gains
or deletions of entire chromosomes or large segments of chromosomes.  This is done
by exploring expression intensity of genes across positions of a tumor genome in
comparison to a set of reference \"normal\" cells.  A heatmap is generated
illustrating the relative expression intensities across each chromosome, and it
often becomes readily apparent as to which regions of the tumor genome are
over-abundant or less-abundant as compared to that of normal cells.")
    (license license:bsd-3)))

(define-public r-iranges
  (package
    (name "r-iranges")
    (version "2.38.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "IRanges" version))
              (sha256
               (base32
                "047688hrxyg2hxd1wbywxqnvxk1622xlldzana9g5hhxg9lldrk2"))))
    (properties
     `((upstream-name . "IRanges")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocgenerics r-s4vectors))
    (home-page "https://bioconductor.org/packages/IRanges")
    (synopsis "Infrastructure for manipulating intervals on sequences")
    (description
     "This package provides efficient low-level and highly reusable S4 classes
for storing ranges of integers, RLE vectors (Run-Length Encoding), and, more
generally, data that can be organized sequentially (formally defined as
@code{Vector} objects), as well as views on these @code{Vector} objects.
Efficient list-like classes are also provided for storing big collections of
instances of the basic classes.  All classes in the package use consistent
naming and share the same rich and consistent \"Vector API\" as much as
possible.")
    (license license:artistic2.0)))

(define-public r-isoformswitchanalyzer
  (package
    (name "r-isoformswitchanalyzer")
    (version "2.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "IsoformSwitchAnalyzeR" version))
       (sha256
        (base32 "0mmxyq1r4kq1igj108pr0r9215hwmyph5svx0djd460knak24y6z"))))
    (properties `((upstream-name . "IsoformSwitchAnalyzeR")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biobase
           r-biocgenerics
           r-biocparallel
           r-biostrings
           r-bsgenome
           r-dbi
           r-dexseq
           r-dplyr
           r-edger
           r-futile-logger
           r-genomeinfodb
           r-genomicranges
           r-ggplot2
           r-gridextra
           r-iranges
           r-limma
           r-magrittr
           r-pfamanalyzer
           r-plyr
           r-pwalign
           r-rcolorbrewer
           r-rcurl
           r-readr
           r-reshape2
           r-rtracklayer
           r-s4vectors
           r-saturn
           r-stringr
           r-summarizedexperiment
           r-sva
           r-tibble
           r-tidyr
           r-tximeta
           r-tximport
           r-venndiagram
           r-xvector))
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/IsoformSwitchAnalyzeR/")
    (synopsis "Analyze alternative splicing in RNA-seq data")
    (description
     "This is a package for the analysis of alternative splicing and isoform
switches with predicted functional consequences (e.g. gain/loss of protein
domains etc.) from quantification of all types of RNASeq by tools such as
Kallisto, Salmon, StringTie, Cufflinks/Cuffdiff etc.")
    (license license:gpl2+)))

;; This is a CRAN package, but it depends on qvalue from Bioconductor.
(define-public r-isva
  (package
    (name "r-isva")
    (version "1.9")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "isva" version))
              (sha256
               (base32
                "05qx9q0kg4ma23v4abhihw0vz017nq6hv2jzsiqx4d20ngh1dl4z"))))
    (properties `((upstream-name . "isva")))
    (build-system r-build-system)
    (propagated-inputs (list r-fastica r-jade r-qvalue))
    (home-page "https://cran.r-project.org/package=isva")
    (synopsis "Independent surrogate variable analysis")
    (description
     "Independent Surrogate Variable Analysis is an algorithm for feature
selection in the presence of potential confounding factors (see Teschendorff
AE et al 2011, <doi: 10.1093/bioinformatics/btr171>).")
    (license license:gpl2)))

(define-public r-italics
  (package
    (name "r-italics")
    (version "2.64.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ITALICS" version))
       (sha256
        (base32 "06h6iqrqzlcj4hyhj1ikl9isr3ls1pph49v5n7cp87czinr0nd3s"))))
    (properties `((upstream-name . "ITALICS")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-affxparser
           r-dbi
           r-glad
           r-italicsdata
           r-oligo
           r-oligoclasses
           r-pd-mapping50k-xba240))
    (home-page "http://bioinfo.curie.fr")
    (synopsis "Normalizing of the Affymetrix GeneChip human mapping")
    (description
     "This package provides tools for normalizing and analyzing of GeneChip
Mapping 100K and 500K Set.  Affymetrix GeneChip Human Mapping 100K and 500K
Set allows the DNA copy number mea- surement of respectively 2× 50K and 2×
250K SNPs along the genome.  Their high density allows a precise localization
of genomic alterations and makes them a powerful tool for cancer and copy
number polymorphism study.")
    (license license:gpl2)))

;; This is a CRAN package, but it depends on r-biobase and r-limma from Bioconductor.
(define-public r-absfiltergsea
  (package
    (name "r-absfiltergsea")
    (version "1.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "AbsFilterGSEA" version))
       (sha256
        (base32 "15srxkxsvn38kd5frdrwfdf0ad8gskrd0h01wmdf9hglq8fjrp7w"))))
    (properties `((upstream-name . "AbsFilterGSEA")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biobase r-deseq r-limma r-rcpp r-rcpparmadillo))
    (home-page "https://cran.r-project.org/web/packages/AbsFilterGSEA/")
    (synopsis "Improved false positive control of gene-permuting with absolute filtering")
    (description
     "This package provides a function that performs gene-permuting of a gene-set
enrichment analysis (GSEA) calculation with or without the absolute filtering.
  Without filtering, users can perform (original) two-tailed or one-tailed
absolute GSEA.")
    (license license:gpl2)))

;; This is a CRAN package, but it depends on r-biobase from Bioconductor.
(define-public r-bisquerna
  (package
   (name "r-bisquerna")
   (version "1.0.5")
   (source (origin
            (method url-fetch)
            (uri (cran-uri "BisqueRNA" version))
            (sha256
             (base32
              "0p3p5lp69gri7vs6qfpm7br4ksbs4l7clm4nj8ki99wpqiqni23n"))))
   (properties `((upstream-name . "BisqueRNA")))
   (build-system r-build-system)
   (propagated-inputs
    (list r-biobase r-limsolve))
   (native-inputs
     (list r-knitr))
   (home-page "https://www.biorxiv.org/content/10.1101/669911v1")
   (synopsis "Decomposition of bulk expression with single-cell sequencing")
   (description "This package provides tools to accurately estimate cell type
abundances from heterogeneous bulk expression.  A reference-based method
utilizes single-cell information to generate a signature matrix and
transformation of bulk expression for accurate regression based estimates.
A marker-based method utilizes known cell-specific marker genes to measure
relative abundances across samples.")
   (license license:gpl3)))

;; This is a CRAN package, but it depends on r-bsgenome-hsapiens-ucsc-hg19
;; from Bioconductor.
(define-public r-deconstructsigs
  (package
    (name "r-deconstructsigs")
    (version "1.8.0")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "deconstructSigs" version))
              (sha256
               (base32
                "014x0nb23jb98666kaav2phkvmkr38pi38jv0dqd4jv7zp0gdf1a"))))
    (properties
     `((upstream-name . "deconstructSigs")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-bsgenome r-bsgenome-hsapiens-ucsc-hg19 r-genomeinfodb
           r-reshape2))
    (home-page "https://github.com/raerose01/deconstructSigs")
    (synopsis "Identifies signatures present in a tumor sample")
    (description "This package takes sample information in the form of the
fraction of mutations in each of 96 trinucleotide contexts and identifies
the weighted combination of published signatures that, when summed, most
closely reconstructs the mutational profile.")
    (license license:gpl2+)))

;; This is a CRAN package, but it depends on Bioconductor packages.
(define-public r-jetset
  (package
    (name "r-jetset")
    (version "3.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "jetset" version))
       (sha256
        (base32 "0c99h5npsv2gf5d59s4qhkaqmjhbwa3prcykk24wzhnpfq6y6xhp"))))
    (properties `((upstream-name . "jetset")))
    (build-system r-build-system)
    (propagated-inputs (list r-annotationdbi r-org-hs-eg-db))
    (home-page "http://www.cbs.dtu.dk/biotools/jetset/")
    (synopsis "One-to-one gene-probeset mapping for Affymetrix human microarrays")
    (description
     "This package provides a one-to-one mapping from gene to \"best\" probe
set for four Affymetrix human gene expression microarrays: hgu95av2, hgu133a,
hgu133plus2, and u133x3p.  On Affymetrix gene expression microarrays, a single
gene may be measured by multiple probe sets.  This can present a mild
conundrum when attempting to evaluate a gene \"signature\" that is defined by
gene names rather than by specific probe sets.  This package also includes the
pre-calculated probe set quality scores that were used to define the
mapping.")
    (license license:artistic2.0)))

(define-public r-nebulosa
  (package
    (name "r-nebulosa")
    (version "1.14.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "Nebulosa" version))
              (sha256
               (base32
                "0m5z6rribsrk9mzcksjh0nmpf4d0x0hn8jd80h0hj0hj8sj1ifgv"))))
    (properties `((upstream-name . "Nebulosa")))
    (build-system r-build-system)
    (propagated-inputs (list r-ggplot2
                             r-ggrastr
                             r-ks
                             r-matrix
                             r-patchwork
                             r-seuratobject
                             r-singlecellexperiment
                             r-summarizedexperiment))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/powellgenomicslab/Nebulosa")
    (synopsis
     "Single-cell data visualisation using kernel gene-weighted density estimation")
    (description
     "This package provides a enhanced visualization of single-cell data based
on gene-weighted density estimation.  Nebulosa recovers the signal from
dropped-out features and allows the inspection of the joint expression from
multiple features (e.g. genes).  @code{Seurat} and @code{SingleCellExperiment}
objects can be used within Nebulosa.")
    (license license:gpl3)))

;; This is a CRAN package but it requires r-rcy3, a Bioconductor package.
(define-public r-netgsa
  (package
    (name "r-netgsa")
    (version "4.0.5")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "netgsa" version))
       (sha256
        (base32 "1m9myxsbvbljr038azxzakpbh20a21qhiy20d0ipvjc5asq3kfla"))))
    (properties `((upstream-name . "netgsa")))
    (build-system r-build-system)
    (propagated-inputs (list r-annotationdbi
                             r-corpcor
                             r-data-table
                             r-dplyr
                             r-genefilter
                             r-glassofast
                             r-glmnet
                             r-graph
                             r-graphite
                             r-httr
                             r-igraph
                             r-magrittr
                             r-matrix
                             r-msigdbr
                             r-org-hs-eg-db
                             r-quadprog
                             r-rcpp
                             r-rcppeigen
                             r-rcy3
                             r-reshape2
                             r-rlang))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/mikehellstern/netgsa")
    (synopsis "Network-Based gene set analysis")
    (description
     "This package lets you carry out network-based gene set analysis by
incorporating external information about interactions among genes, as well as
novel interactions learned from data.  It implements methods described in
Shojaie A, Michailidis G (2010) <doi:10.1093/biomet/asq038>, Shojaie A,
Michailidis G (2009) <doi:10.1089/cmb.2008.0081>, and Ma J, Shojaie A,
Michailidis G (2016) <doi:10.1093/bioinformatics/btw410>.")
    (license license:gpl3+)))

;; This is a CRAN package, but it depends on Bioconductor packages.
(define-public r-nmf
  (package
    (name "r-nmf")
    (version "0.27")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "NMF" version))
       (sha256
        (base32
         "1y9y7xpfd9y8j5b8s2x5g61455ilpgqpdhrirpz58xjarbph4hxg"))))
    (properties `((upstream-name . "NMF")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biobase
           r-biocmanager
           r-bigmemory ;suggested
           r-cluster
           r-codetools
           r-colorspace
           r-digest
           r-doparallel
           r-foreach
           r-ggplot2
           r-gridbase
           r-rcolorbrewer
           r-registry
           r-reshape2
           r-rngtools
           r-stringr
           r-synchronicity)) ;suggested
    (native-inputs
     (list r-knitr))
    (home-page "https://renozao.github.io/NMF")
    (synopsis "Algorithms and framework for nonnegative matrix factorization")
    (description
     "This package provides a framework to perform Non-negative Matrix
Factorization (NMF).  The package implements a set of already published
algorithms and seeding methods, and provides a framework to test, develop and
plug new or custom algorithms.  Most of the built-in algorithms have been
optimized in C++, and the main interface function provides an easy way of
performing parallel computations on multicore machines.")
    (license license:gpl2+)))

(define-public r-affy
  (package
    (name "r-affy")
    (version "1.82.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "affy" version))
       (sha256
        (base32
         "19z22c1yd3rkwk7dp8q8xv16n0qv92iz00vzid3pniy7zr80cxxy"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-affyio
           r-biobase
           r-biocgenerics
           r-biocmanager
           r-preprocesscore
           r-zlibbioc))
    (inputs
     (list zlib))
    (home-page "https://bioconductor.org/packages/affy")
    (synopsis "Methods for affymetrix oligonucleotide arrays")
    (description
     "This package contains functions for exploratory oligonucleotide array
analysis.")
    (license license:lgpl2.0+)))

(define-public r-affycomp
  (package
    (name "r-affycomp")
    (version "1.80.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "affycomp" version))
       (sha256
        (base32
         "02n2v1iwkhmilv1bjfza2yrqj7rsr8v46bghajcb8wdyyaj759ib"))))
    (properties `((upstream-name . "affycomp")))
    (build-system r-build-system)
    (propagated-inputs (list r-biobase))
    (home-page "https://bioconductor.org/packages/affycomp/")
    (synopsis "Graphics toolbox for assessment of Affymetrix expression measures")
    (description
     "The package contains functions that can be used to compare expression
measures for Affymetrix Oligonucleotide Arrays.")
    (license license:gpl2+)))

(define-public r-affycompatible
  (package
    (name "r-affycompatible")
    (version "1.58.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "AffyCompatible" version))
       (sha256
        (base32
         "1bg7iqasvfsgd9x3ykgpblqnz1q06g3ifmzj4jf2kn8kxj63wfbl"))))
    (properties
     `((upstream-name . "AffyCompatible")))
    (build-system r-build-system)
    (arguments
     (list
      #:phases
      `(modify-phases %standard-phases
         (add-after 'unpack 'make-reproducible
           (lambda _
             ;; Order DTD elements before generating R code from them.
             (substitute* "R/methods-AffyCompatible.R"
               (("dtd <- .*" m)
                (string-append m "
elements <- dtd$elements
ordered <- elements[order(names(elements))]\n"))
               (("elt in dtd\\$elements")
                "elt in ordered"))
             ;; Use a predictable directory name for code generation.
             (mkdir-p "/tmp/NetAffxResourcePrototype")
             (substitute* "R/DataClasses.R"
               (("directory=tempdir\\(\\)")
                "directory=\"/tmp/NetAffxResourcePrototype\"")))))))
    (propagated-inputs
     (list r-biostrings r-rcurl r-xml))
    (home-page "https://bioconductor.org/packages/AffyCompatible/")
    (synopsis "Work with Affymetrix GeneChip files")
    (description
     "This package provides an interface to Affymetrix chip annotation and
sample attribute files.  The package allows an easy way for users to download
and manage local data bases of Affynmetrix NetAffx annotation files.  It also
provides access to @dfn{GeneChip Operating System} (GCOS) and @dfn{GeneChip
Command Console} (AGCC)-compatible sample annotation files.")
    (license license:artistic2.0)))

(define-public r-affycontam
  (package
    (name "r-affycontam")
    (version "1.62.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "affyContam" version))
       (sha256
        (base32
         "0r4pqda9pv60b867c6yi328687kyxjglk5jmcal6xnlr75nf4w0y"))))
    (properties `((upstream-name . "affyContam")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-affy r-affydata r-biobase))
    (home-page "https://bioconductor.org/packages/affyContam/")
    (synopsis "Structured corruption of Affymetrix CEL file data")
    (description
     "Microarray quality assessment is a major concern of microarray analysts.
This package provides some simple approaches to in silico creation of quality
problems in CEL-level data to help evaluate performance of quality metrics.")
    (license license:artistic2.0)))

(define-public r-affycoretools
  (package
    (name "r-affycoretools")
    (version "1.76.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "affycoretools" version))
       (sha256
        (base32
         "0w138n3f68ndr0innk981vf8p5wb729m31p0d3f9qp08hmv58jfw"))))
    (properties `((upstream-name . "affycoretools")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-affy
           r-annotationdbi
           r-biobase
           r-biocgenerics
           r-dbi
           r-edger
           r-gcrma
           r-ggplot2
           r-glimma
           r-gostats
           r-gplots
           r-hwriter
           r-lattice
           r-limma
           r-oligoclasses
           r-reportingtools
           r-rsqlite
           r-s4vectors
           r-xtable))
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/affycoretools/")
    (synopsis "Functions for analyses with Affymetrix GeneChips")
    (description
     "This package provides various wrapper functions that have been written
to streamline the more common analyses that a Biostatistician might see.")
    (license license:artistic2.0)))

(define-public r-affyio
  (package
    (name "r-affyio")
    (version "1.74.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "affyio" version))
       (sha256
        (base32
         "156sijj9zy01p038m7yw58h1bw1ii0kd5q9kv3krh6bs67c3dv8h"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-zlibbioc))
    (inputs
     (list zlib))
    (home-page "https://github.com/bmbolstad/affyio")
    (synopsis "Tools for parsing Affymetrix data files")
    (description
     "This package provides routines for parsing Affymetrix data files based
upon file format information.  The primary focus is on accessing the CEL and
CDF file formats.")
    (license license:lgpl2.0+)))

(define-public r-affxparser
  (package
    (name "r-affxparser")
    (version "1.76.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "affxparser" version))
       (sha256
        (base32
         "0qqc2sq331g2ml390nhjkrnldp6bi7r6xyz7rw77hnr3qpcrhqk2"))))
    (properties `((upstream-name . "affxparser")))
    (build-system r-build-system)
    (home-page "https://github.com/HenrikBengtsson/affxparser")
    (synopsis "Affymetrix File Parsing SDK")
    (description
     "This is a package for parsing Affymetrix files (CDF, CEL, CHP, BPMAP,
BAR).  It provides methods for fast and memory efficient parsing of Affymetrix
files using the Affymetrix' Fusion SDK.  Both ASCII- and binary-based files
are supported.  Currently, there are methods for reading @dfn{chip definition
file} (CDF) and a @dfn{cell intensity file} (CEL).  These files can be read
either in full or in part.  For example, probe signals from a few probesets
can be extracted very quickly from a set of CEL files into a convenient list
structure.")
    ;; The Fusion SDK contains files under GPLv2 and LGPLv2.1.  The R code is
    ;; under LGPLv2+.
    (license (list license:lgpl2.0+ license:lgpl2.1 license:gpl2))))

(define-public r-annotate
  (package
    (name "r-annotate")
    (version "1.82.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "annotate" version))
       (sha256
        (base32
         "1v4sq22lwrhl50whag0jil788wwvi4rpdaij03iwak93q998ls0f"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotationdbi
           r-biobase
           r-biocgenerics
           r-dbi
           r-httr
           r-xml
           r-xtable))
    (home-page
     "https://bioconductor.org/packages/annotate")
    (synopsis "Annotation for microarrays")
    (description "This package provides R environments for the annotation of
microarrays.")
    (license license:artistic2.0)))

(define-public r-annotationdbi
  (package
    (name "r-annotationdbi")
    (version "1.66.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "AnnotationDbi" version))
              (sha256
               (base32
                "1g2yzbx7djkk8kccbclxv953iijmjvsjsalfv1ihmmcai33979rs"))))
    (properties
     `((upstream-name . "AnnotationDbi")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biobase
           r-biocgenerics
           r-dbi
           r-iranges
           r-keggrest
           r-rsqlite
           r-s4vectors))
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/AnnotationDbi")
    (synopsis "Annotation database interface")
    (description
     "This package provides user interface and database connection code for
annotation data packages using SQLite data storage.")
    (license license:artistic2.0)))

(define-public r-annotationfilter
  (package
    (name "r-annotationfilter")
    (version "1.28.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "AnnotationFilter" version))
              (sha256
               (base32
                "1sdm85sl4d9mqbjgjh3yizcmrzgz0g4x17s25gss2f6cjwy5jp40"))))
    (properties
     `((upstream-name . "AnnotationFilter")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-genomicranges r-lazyeval))
    (native-inputs
     (list r-knitr))
    (home-page "https://github.com/Bioconductor/AnnotationFilter")
    (synopsis "Facilities for filtering Bioconductor annotation resources")
    (description
     "This package provides classes and other infrastructure to implement
filters for manipulating Bioconductor annotation resources.  The filters are
used by @code{ensembldb}, @code{Organism.dplyr}, and other packages.")
    (license license:artistic2.0)))

(define-public r-annotationforge
  (package
    (name "r-annotationforge")
    (version "1.46.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "AnnotationForge" version))
       (sha256
        (base32
         "1ikri9hb1s0jzw1wyfpwsl02hg8dw2xs703sp085sbsq3cwk9cn1"))))
    (properties
     `((upstream-name . "AnnotationForge")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotationdbi
           r-biobase
           r-biocgenerics
           r-dbi
           r-rcurl
           r-rsqlite
           r-s4vectors
           r-xml))
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/AnnotationForge")
    (synopsis "Code for building annotation database packages")
    (description
     "This package provides code for generating Annotation packages and their
databases.  Packages produced are intended to be used with AnnotationDbi.")
    (license license:artistic2.0)))

(define-public r-annotationhub
  (package
    (name "r-annotationhub")
    (version "3.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "AnnotationHub" version))
       (sha256
        (base32
         "1da2n13j2xysnd175zi7s6zm8yllnp0k59vygkx9id85jcldqf51"))))
    (properties `((upstream-name . "AnnotationHub")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotationdbi
           r-biocfilecache
           r-biocgenerics
           r-biocmanager
           r-biocversion
           r-curl
           r-dplyr
           r-httr
           r-rappdirs
           r-rsqlite
           r-s4vectors
           r-yaml))
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/AnnotationHub")
    (synopsis "Client to access AnnotationHub resources")
    (description
     "This package provides a client for the Bioconductor AnnotationHub web
resource.  The AnnotationHub web resource provides a central location where
genomic files (e.g. VCF, bed, wig) and other resources from standard
locations (e.g. UCSC, Ensembl) can be discovered.  The resource includes
metadata about each resource, e.g., a textual description, tags, and date of
modification.  The client creates and manages a local cache of files retrieved
by the user, helping with quick and reproducible access.")
    (license license:artistic2.0)))

(define-public r-aroma-light
  (package
    (name "r-aroma-light")
    (version "3.34.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "aroma.light" version))
       (sha256
        (base32
         "1v24b43knsjpw44zkrzv8pvxjiw12rcz9wqp4ahd88dg23j7pvwz"))))
    (properties `((upstream-name . "aroma.light")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-matrixstats r-r-methodss3 r-r-oo r-r-utils))
    (home-page "https://github.com/HenrikBengtsson/aroma.light")
    (synopsis "Methods for normalization and visualization of microarray data")
    (description
     "This package provides methods for microarray analysis that take basic
data types such as matrices and lists of vectors.  These methods can be used
standalone, be utilized in other packages, or be wrapped up in higher-level
classes.")
    (license license:gpl2+)))

(define-public r-bamsignals
  (package
    (name "r-bamsignals")
    (version "1.36.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "bamsignals" version))
       (sha256
        (base32
         "1jq993m6b0cv3ibgjrbzyka7svb653hvi6bvqhf8ir3lgjgs23dp"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocgenerics
           r-genomicranges
           r-iranges
           r-rcpp
           r-rhtslib
           r-zlibbioc))
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/bamsignals")
    (synopsis "Extract read count signals from bam files")
    (description
     "This package efficiently obtains count vectors from indexed bam
files.  It counts the number of nucleotide sequence reads in given genomic
ranges and it computes reads profiles and coverage profiles.  It also handles
paired-end data.")
    (license license:gpl2+)))

(define-public r-biobase
  (package
    (name "r-biobase")
    (version "2.64.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "Biobase" version))
              (sha256
               (base32
                "09xnl0qq4qsd981i9srw07228lgashcvcavi8k0izdzfyk6ka7m2"))))
    (properties
     `((upstream-name . "Biobase")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocgenerics))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/Biobase")
    (synopsis "Base functions for Bioconductor")
    (description
     "This package provides functions that are needed by many other packages
on Bioconductor or which replace R functions.")
    (license license:artistic2.0)))

(define-public r-biomart
  (package
    (name "r-biomart")
    (version "2.60.1")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "biomaRt" version))
              (sha256
               (base32
                "0mzdxpagxwiwyznhd7h93cgy01a2j85rgb5dpnkzgp4sxw01n7rb"))))
    (properties
     `((upstream-name . "biomaRt")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotationdbi
           r-biocfilecache
           r-digest
           r-httr2
           r-progress
           r-rappdirs
           r-stringr
           r-xml2))
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/biomaRt")
    (synopsis "Interface to BioMart databases")
    (description
     "biomaRt provides an interface to a growing collection of databases
implementing the @url{BioMart software suite, http://www.biomart.org}.  The
package enables retrieval of large amounts of data in a uniform way without
the need to know the underlying database schemas or write complex SQL queries.
Examples of BioMart databases are Ensembl, COSMIC, Uniprot, HGNC, Gramene,
Wormbase and dbSNP mapped to Ensembl.  These major databases give biomaRt
users direct access to a diverse set of data and enable a wide range of
powerful online queries from gene annotation to database mining.")
    (license license:artistic2.0)))

;; This is a CRAN package, but it depends on a Bioconductor package:
;; r-biomart
(define-public r-biomartr
  (package
    (name "r-biomartr")
    (version "1.0.7")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "biomartr" version))
              (sha256
               (base32
                "0ic6qbk2xmgrcc0xhxyhjafp1xbf2c5dpbqqrbkprrhynr8mq7cx"))))
    (properties `((upstream-name . "biomartr")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biomart
           r-biostrings
           r-curl
           r-data-table
           r-downloader
           r-dplyr
           r-fs
           r-httr
           r-jsonlite
           r-philentropy
           r-purrr
           r-r-utils
           r-rcurl
           r-readr
           r-stringr
           r-tibble
           r-withr
           r-xml))
    (native-inputs (list r-knitr))
    (home-page "https://docs.ropensci.org/biomartr/")
    (synopsis "Genomic data retrieval")
    (description
     "Perform large scale genomic data retrieval and functional annotation
retrieval.  This package aims to provide users with a standardized way to
automate genome, proteome, RNA, coding sequence (CDS), GFF, and metagenome
retrieval from NCBI RefSeq, NCBI Genbank, ENSEMBL, and UniProt databases.
Furthermore, an interface to the BioMart database allows users to retrieve
functional annotation for genomic loci.  In addition, users can download
entire databases such as NCBI RefSeq, NCBI nr, NCBI nt, NCBI Genbank, etc with
only one command.")
    (license license:gpl2)))

(define-public r-biocparallel
  (package
    (name "r-biocparallel")
    (version "1.38.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "BiocParallel" version))
              (sha256
               (base32
                "1miw26r2a8wvgnfr9sapfv6rqk219rmsyjh6v04annafxdxy15ky"))))
    (properties
     `((upstream-name . "BiocParallel")))
    (build-system r-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'make-reproducible
           (lambda _
             ;; Remove generated documentation.
             (for-each delete-file
                       '("inst/doc/BiocParallel_BatchtoolsParam.pdf"
                         "inst/doc/Errors_Logs_And_Debugging.pdf"
                         "inst/doc/BiocParallel_BatchtoolsParam.R"
                         "inst/doc/Introduction_To_BiocParallel.R"
                         "inst/doc/Errors_Logs_And_Debugging.R"
                         "inst/doc/Random_Numbers.R"))

             ;; Remove time-dependent macro
             (substitute* '("inst/doc/BiocParallel_BatchtoolsParam.Rnw"
                            "inst/doc/Errors_Logs_And_Debugging.Rnw"
                            "vignettes/BiocParallel_BatchtoolsParam.Rnw"
                            "vignettes/Errors_Logs_And_Debugging.Rnw")
               (("\\today") "later"))

             ;; Initialize the random number generator seed when building.
             (substitute* "R/rng.R"
               (("\"L'Ecuyer-CMRG\"\\)" m)
                (string-append
                 m "; if (!is.na(Sys.getenv(\"SOURCE_DATE_EPOCH\"))) {set.seed(100)}\n"))))))))
    (propagated-inputs
     (list r-bh r-codetools r-cpp11 r-futile-logger r-snow))
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/BiocParallel")
    (synopsis "Bioconductor facilities for parallel evaluation")
    (description
     "This package provides modified versions and novel implementation of
functions for parallel evaluation, tailored to use with Bioconductor
objects.")
    (license (list license:gpl2+ license:gpl3+))))

(define-public r-biostrings
  (package
    (name "r-biostrings")
    (version "2.72.1")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "Biostrings" version))
              (sha256
               (base32
                "067vy2v7spbdsknvi0fcpdi085kn8v20pyrhkc7wy357v21h7hgs"))))
    (properties
     `((upstream-name . "Biostrings")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocgenerics
           r-crayon
           r-genomeinfodb
           r-iranges
           r-s4vectors
           r-xvector))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/Biostrings")
    (synopsis "String objects and algorithms for biological sequences")
    (description
     "This package provides memory efficient string containers, string
matching algorithms, and other utilities, for fast manipulation of large
biological sequences or sets of sequences.")
    (license license:artistic2.0)))

(define-public r-biovizbase
  (package
    (name "r-biovizbase")
    (version "1.52.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "biovizBase" version))
       (sha256
        (base32
         "1jbcnjjwdjy7zfy0ybgfy1y5ym7rygzkailr5bm4hnh9bn2837p7"))))
    (properties `((upstream-name . "biovizBase")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotationdbi
           r-annotationfilter
           r-biocgenerics
           r-biostrings
           r-dichromat
           r-ensembldb
           r-genomeinfodb
           r-genomicalignments
           r-genomicfeatures
           r-genomicranges
           r-hmisc
           r-iranges
           r-rcolorbrewer
           r-rlang
           r-rsamtools
           r-s4vectors
           r-scales
           r-summarizedexperiment
           r-variantannotation))
    (home-page "https://bioconductor.org/packages/biovizBase")
    (synopsis "Basic graphic utilities for visualization of genomic data")
    (description
     "The biovizBase package is designed to provide a set of utilities, color
schemes and conventions for genomic data.  It serves as the base for various
high-level packages for biological data visualization.  This saves development
effort and encourages consistency.")
    (license license:artistic2.0)))

(define-public r-bsgenome
  (package
    (name "r-bsgenome")
    (version "1.72.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "BSgenome" version))
              (sha256
               (base32
                "1pjbkakrycvaifl7xrmri2h94zc1ap8a9d1q4c83qji3xa21cm4q"))))
    (properties
     `((upstream-name . "BSgenome")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocgenerics
           r-biocio
           r-biostrings
           r-genomeinfodb
           r-genomicranges
           r-iranges
           r-matrixstats
           r-rsamtools
           r-rtracklayer
           r-s4vectors
           r-xvector))
    (home-page "https://bioconductor.org/packages/BSgenome")
    (synopsis "Infrastructure for Biostrings-based genome data packages")
    (description
     "This package provides infrastructure shared by all Biostrings-based
genome data packages and support for efficient SNP representation.")
    (license license:artistic2.0)))

(define-public r-category
  (package
    (name "r-category")
    (version "2.70.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Category" version))
       (sha256
        (base32
         "0paz6h8rsbqvlxxj4di8cl76aybgjr842z5gdv8xi6c2d7zwbclw"))))
    (properties `((upstream-name . "Category")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotate
           r-annotationdbi
           r-biobase
           r-biocgenerics
           r-dbi
           r-genefilter
           r-graph
           r-gseabase
           r-matrix
           r-rbgl))
    (home-page "https://bioconductor.org/packages/Category")
    (synopsis "Category analysis")
    (description
     "This package provides a collection of tools for performing category
analysis.")
    (license license:artistic2.0)))

(define-public r-champ
  (package
    (name "r-champ")
    (version "2.34.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "ChAMP" version))
              (sha256
               (base32
                "06q539yhbmbp7qr71cwjai2vdw5nnhhjdv2ajlqmlyjpg04arc7v"))))
    (properties `((upstream-name . "ChAMP")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-bumphunter
           r-champdata
           r-combinat
           r-dendextend
           r-dmrcate
           r-dnacopy
           r-doparallel
           r-dt
           r-genomicranges
           r-ggplot2
           r-globaltest
           r-goseq
           r-hmisc
           r-illumina450probevariants-db
           r-illuminahumanmethylation450kmanifest
           r-illuminahumanmethylationepicanno-ilm10b4-hg19
           r-illuminahumanmethylationepicmanifest
           r-illuminaio
           r-impute
           r-isva
           r-kpmt
           r-limma
           r-marray
           r-matrixstats
           r-minfi
           r-missmethyl
           r-plotly
           r-plyr
           r-preprocesscore
           r-prettydoc
           r-quadprog
           r-qvalue
           r-rcolorbrewer
           r-rmarkdown
           r-rpmm
           r-shiny
           r-shinythemes
           r-sva
           r-watermelon))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/ChAMP")
    (synopsis
     "Chip analysis methylation pipeline for Illumina HumanMethylation450 and EPIC")
    (description
     "The package includes quality control metrics, a selection of
normalization methods and novel methods to identify differentially methylated
regions and to highlight copy number alterations.")
    (license license:gpl3)))

(define-public r-chipseeker
  (package
    (name "r-chipseeker")
    (version "1.40.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "ChIPseeker" version))
              (sha256
               (base32
                "00iknjk7vgj48vj1pp7l98g7nvc3x495z3jcq5nbvdiff02faj1f"))))
    (build-system r-build-system)
    (native-inputs
     (list r-knitr))
    (propagated-inputs
     (list r-annotationdbi
           r-aplot
           r-biocgenerics
           r-boot
           r-dplyr
           r-enrichplot
           r-genomeinfodb
           r-genomicfeatures
           r-genomicranges
           r-ggplot2
           r-gplots
           r-gtools
           r-iranges
           r-magrittr
           r-plotrix
           r-rtracklayer
           r-s4vectors
           r-tibble
           r-txdb-hsapiens-ucsc-hg19-knowngene
           r-yulab-utils))
    (home-page "https://www.bioconductor.org/packages/ChIPseeker/")
    (synopsis "ChIPseeker for ChIP peak annotation, comparison, and visualization")
    (description "This package implements functions to retrieve the nearest
genes around the peak, annotate genomic region of the peak, statstical methods
for estimate the significance of overlap among ChIP peak data sets, and
incorporate GEO database for user to compare the own dataset with those
deposited in database.  The comparison can be used to infer cooperative
regulation and thus can be used to generate hypotheses.  Several visualization
functions are implemented to summarize the coverage of the peak experiment,
average profile and heatmap of peaks binding to TSS regions, genomic
annotation, distance to TSS, and overlap of peaks or genes.")
    (license license:artistic2.0)))

(define-public r-chipseq
  (package
    (name "r-chipseq")
    (version "1.54.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "chipseq" version))
       (sha256
        (base32
         "1rd56knaf0ar8d0nh0mcnrrg5jy0csixig1d4abn1b64vbzrvzpz"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocgenerics
           r-genomicranges
           r-iranges
           r-lattice
           r-s4vectors
           r-shortread))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/chipseq")
    (synopsis "Package for analyzing ChIPseq data")
    (description
     "This package provides tools for processing short read data from ChIPseq
experiments.")
    (license license:artistic2.0)))

(define-public r-complexheatmap
  (package
    (name "r-complexheatmap")
    (version "2.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ComplexHeatmap" version))
       (sha256
        (base32
         "1ppzx850nvvdhz2xiggr43c79w0nr0vmsmmnszasd4flf3jm7v8v"))))
    (properties
     `((upstream-name . "ComplexHeatmap")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-circlize
           r-clue
           r-codetools
           r-colorspace
           r-digest
           r-doparallel
           r-foreach
           r-getoptlong
           r-globaloptions
           r-iranges
           r-matrixstats
           r-png
           r-rcolorbrewer))
    (native-inputs
     (list r-knitr))
    (home-page
     "https://github.com/jokergoo/ComplexHeatmap")
    (synopsis "Making Complex Heatmaps")
    (description
     "Complex heatmaps are efficient to visualize associations between
different sources of data sets and reveal potential structures.  This package
provides a highly flexible way to arrange multiple heatmaps and supports
self-defined annotation graphics.")
    (license license:gpl2+)))

;; This is a CRAN package, but it depends on r-complexheatmap from
;; Bioconductor.
(define-public r-conos
  (package
    (name "r-conos")
    (version "1.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "conos" version))
       (sha256
        (base32 "0p66gb308w2zfsppm0k1vsvgr33kzzhql0vh7l39vlsgayi00skk"))))
    (properties `((upstream-name . "conos")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-abind
           r-complexheatmap
           r-cowplot
           r-dendextend
           r-dplyr
           r-ggplot2
           r-ggrepel
           r-gridextra
           r-igraph
           r-irlba
           r-leidenalg
           r-magrittr
           r-matrix
           r-n2r
           r-r6
           r-rcpp
           r-rcpparmadillo
           r-rcppeigen
           r-rcppprogress
           r-reshape2
           r-rlang
           r-rtsne
           r-sccore))
    (home-page "https://github.com/kharchenkolab/conos")
    (synopsis "Clustering on network of samples")
    (description
     "This package wires together large collections of single-cell RNA-seq
datasets, which allows for both the identification of recurrent cell clusters
and the propagation of information between datasets in multi-sample or
atlas-scale collections.  Conos focuses on the uniform mapping of homologous
cell types across heterogeneous sample collections.  For instance, users could
investigate a collection of dozens of peripheral blood samples from cancer
patients combined with dozens of controls, which perhaps includes samples of a
related tissue such as lymph nodes.")
    (license license:gpl3)))

(define-public r-copywriter
  (package
    (name "r-copywriter")
    (version "2.29.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "CopywriteR" version))
       (sha256
        (base32
         "1h4cyrjwdazgk49yzi9lvya8bfz9r4cpq19hyzikvc81ia8zdxs6"))))
    (properties `((upstream-name . "CopywriteR")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocparallel
           r-chipseq
           r-copyhelper
           r-data-table
           r-dnacopy
           r-futile-logger
           r-genomeinfodb
           r-genomicalignments
           r-genomicranges
           r-gtools
           r-iranges
           r-matrixstats
           r-rsamtools
           r-s4vectors))
    (home-page "https://github.com/PeeperLab/CopywriteR")
    (synopsis "Copy number information from targeted sequencing")
    (description
     "CopywriteR extracts DNA copy number information from targeted sequencing
by utilizing off-target reads.  It allows for extracting uniformly distributed
copy number information, can be used without reference, and can be applied to
sequencing data obtained from various techniques including chromatin
immunoprecipitation and target enrichment on small gene panels.  Thereby,
CopywriteR constitutes a widely applicable alternative to available copy
number detection tools.")
    (license license:gpl2)))

(define-public r-deseq
  (package
    (name "r-deseq")
    (version "1.39.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "DESeq" version))
       (sha256
        (base32
         "047hph5aqmjnz1aqprziw0smdn5lf96hmwpnvqrxv1j2yfvcf3h1"))))
    (properties `((upstream-name . "DESeq")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biobase
           r-biocgenerics
           r-genefilter
           r-geneplotter
           r-lattice
           r-locfit
           r-mass
           r-rcolorbrewer))
    (home-page "https://www-huber.embl.de/users/anders/DESeq/")
    (synopsis "Differential gene expression analysis")
    (description
     "This package provides tools for estimating variance-mean dependence in
count data from high-throughput genetic sequencing assays and for testing for
differential expression based on a model using the negative binomial
distribution.")
    (license license:gpl3+)))

(define-public r-deseq2
  (package
    (name "r-deseq2")
    (version "1.44.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "DESeq2" version))
       (sha256
        (base32
         "1j7ivkxbc75hzxhl9d97w8vamrmxf6527n7841i1j9n6s7hz1cdn"))))
    (properties `((upstream-name . "DESeq2")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biobase
           r-biocgenerics
           r-biocparallel
           r-genomicranges
           r-ggplot2
           r-iranges
           r-locfit
           r-matrixgenerics
           r-matrixstats
           r-rcpp
           r-rcpparmadillo
           r-s4vectors
           r-summarizedexperiment))
    (native-inputs
     (list r-knitr r-rmarkdown))
    (home-page "https://bioconductor.org/packages/DESeq2")
    (synopsis "Differential gene expression analysis")
    (description
     "This package provides functions to estimate variance-mean dependence in
count data from high-throughput nucleotide sequencing assays and test for
differential expression based on a model using the negative binomial
distribution.")
    (license license:lgpl3+)))

(define-public r-dexseq
  (package
    (name "r-dexseq")
    (version "1.50.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "DEXSeq" version))
       (sha256
        (base32
         "13y3fhgvf7438yjv6qm28n8f42w4wgkklc0q229ypns1gfh54524"))))
    (properties `((upstream-name . "DEXSeq")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotationdbi
           r-biobase
           r-biocgenerics
           r-biocparallel
           r-biomart
           r-deseq2
           r-genefilter
           r-geneplotter
           r-genomicranges
           r-hwriter
           r-iranges
           r-rcolorbrewer
           r-rsamtools
           r-s4vectors
           r-statmod
           r-stringr
           r-summarizedexperiment))
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/DEXSeq")
    (synopsis "Inference of differential exon usage in RNA-Seq")
    (description
     "This package is focused on finding differential exon usage using RNA-seq
exon counts between samples with different experimental designs.  It provides
functions that allows the user to make the necessary statistical tests based
on a model that uses the negative binomial distribution to estimate the
variance between biological replicates and generalized linear models for
testing.  The package also provides functions for the visualization and
exploration of the results.")
    (license license:gpl3+)))

(define-public r-diffcyt
  (package
    (name "r-diffcyt")
    (version "1.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "diffcyt" version))
       (sha256
        (base32 "1iyqvang5j55s19skd44c1zsxkphn3qqbl4sjhsw1y820ylws5r1"))))
    (properties `((upstream-name . "diffcyt")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-circlize
           r-complexheatmap
           r-dplyr
           r-edger
           r-flowcore
           r-flowsom
           r-limma
           r-lme4
           r-magrittr
           r-multcomp
           r-reshape2
           r-s4vectors
           r-summarizedexperiment
           r-tidyr))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/lmweber/diffcyt")
    (synopsis "Differential discovery in high-dimensional cytometry")
    (description
     "This package provides statistical methods for differential discovery
analyses in high-dimensional cytometry data (including flow cytometry, mass
cytometry or CyTOF, and oligonucleotide-tagged cytometry), based on a
combination of high-resolution clustering and empirical Bayes moderated tests
adapted from transcriptomics.")
    (license license:expat)))

(define-public r-dirichletmultinomial
  (package
    (name "r-dirichletmultinomial")
    (version "1.46.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "DirichletMultinomial" version))
       (sha256
        (base32
         "0djd84vnf4skjr6m25ykqrvwa37lgs5r5aw9md81ykaqmi40szqb"))))
    (properties
     `((upstream-name . "DirichletMultinomial")))
    (build-system r-build-system)
    (inputs
     (list gsl))
    (propagated-inputs
     (list r-biocgenerics r-iranges r-s4vectors))
    (home-page "https://bioconductor.org/packages/DirichletMultinomial")
    (synopsis "Dirichlet-Multinomial mixture models for microbiome data")
    (description
     "Dirichlet-multinomial mixture models can be used to describe variability
in microbial metagenomic data.  This package is an interface to code
originally made available by Holmes, Harris, and Quince, 2012, PLoS ONE 7(2):
1-15.")
    (license license:lgpl3)))

(define-public r-dittoseq
  (package
    (name "r-dittoseq")
    (version "1.16.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "dittoSeq" version))
              (sha256
               (base32
                "15p01f3yaan9x7y6ld1kx40jnbcn6vjjm5n6ir7cicdvv4w5ln49"))))
    (properties `((upstream-name . "dittoSeq")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-colorspace
           r-cowplot
           r-ggplot2
           r-ggrepel
           r-ggridges
           r-gridextra
           r-pheatmap
           r-reshape2
           r-s4vectors
           r-singlecellexperiment
           r-summarizedexperiment))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/dittoSeq")
    (synopsis "Single-cell and bulk RNA sequencing visualization")
    (description
     "This package provides a universal, user friendly, single-cell and bulk RNA
sequencing visualization toolkit that allows highly customizable creation of
color blindness friendly, publication-quality figures.  dittoSeq accepts both
SingleCellExperiment (SCE) and Seurat objects, as well as the import and
usage, via conversion to an SCE, of SummarizedExperiment or DGEList bulk data.
Visualizations include dimensionality reduction plots, heatmaps, scatterplots,
percent composition or expression across groups, and more.  Customizations
range from size and title adjustments to automatic generation of annotations
for heatmaps, overlay of trajectory analysis onto any dimensionality reduciton
plot, hidden data overlay upon cursor hovering via ggplotly conversion, and
many more.  All with simple, discrete inputs.  Color blindness friendliness is
powered by legend adjustments (enlarged keys), and by allowing the use of
shapes or letter-overlay in addition to the carefully selected
code{dittoColors()}.")
    (license license:expat)))

(define-public r-edaseq
  (package
    (name "r-edaseq")
    (version "2.38.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "EDASeq" version))
       (sha256
        (base32
         "18h9zlah21lghy1sjpnzy6lddmff8vh3raxnqi15v00za9azk8yq"))))
    (properties `((upstream-name . "EDASeq")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotationdbi
           r-aroma-light
           r-biobase
           r-biocgenerics
           r-biocmanager
           r-biomart
           r-biostrings
           r-genomicfeatures
           r-genomicranges
           r-iranges
           r-rsamtools
           r-shortread))
    (native-inputs
     (list r-knitr))
    (home-page "https://github.com/drisso/EDASeq")
    (synopsis "Exploratory data analysis and normalization for RNA-Seq")
    (description
     "This package provides support for numerical and graphical summaries of
RNA-Seq genomic read data.  Provided within-lane normalization procedures to
adjust for GC-content effect (or other gene-level effects) on read counts:
loess robust local regression, global-scaling, and full-quantile
normalization.  Between-lane normalization procedures to adjust for
distributional differences between lanes (e.g., sequencing depth):
global-scaling and full-quantile normalization.")
    (license license:artistic2.0)))

(define-public r-edger
  (package
    (name "r-edger")
    (version "4.2.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "edgeR" version))
              (sha256
               (base32
                "1w3dp3h30jqylbkql33j5ibp9i4vp7n1x6wdd2v0kdnv82il6hqg"))))
    (properties `((upstream-name . "edgeR")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-limma r-locfit r-rcpp))
    (native-inputs (list r-knitr))
    (home-page "https://bioinf.wehi.edu.au/edgeR")
    (synopsis "EdgeR does empirical analysis of digital gene expression data")
    (description "This package can do differential expression analysis of
RNA-seq expression profiles with biological replication.  It implements a range
of statistical methodology based on the negative binomial distributions,
including empirical Bayes estimation, exact tests, generalized linear models
and quasi-likelihood tests.  It be applied to differential signal analysis of
other types of genomic data that produce counts, including ChIP-seq, SAGE and
CAGE.")
    (license license:gpl2+)))

(define-public r-enhancedvolcano
  (package
    (name "r-enhancedvolcano")
    (version "1.22.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "EnhancedVolcano" version))
              (sha256
               (base32
                "0ry5r74d6m5x15b4azw93mjrvq4fapj4rxpi9f6macz4l5l6j1j0"))))
    (properties `((upstream-name . "EnhancedVolcano")))
    (build-system r-build-system)
    (propagated-inputs (list r-ggplot2 r-ggrepel))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/kevinblighe/EnhancedVolcano")
    (synopsis
     "Publication-ready volcano plots with enhanced coloring and labeling")
    (description
     "Volcano plots represent a useful way to visualise the results of
differential expression analyses.  This package provides a highly-configurable
function that produces publication-ready volcano plots.  EnhancedVolcano will
attempt to fit as many point labels in the plot window as possible, thus
avoiding clogging up the plot with labels that could not otherwise have been
read.  Other functionality allows the user to identify up to 4 different types
of attributes in the same plot space via color, shape, size, and shade
parameter configurations.")
    (license license:gpl3)))

(define-public r-enmix
  (package
    (name "r-enmix")
    (version "1.40.2")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "ENmix" version))
              (sha256
               (base32
                "1x8rzdzymibypb65axx3sgxsgrj5d1fldj9j70acvg46lqhmq8f2"))))
    (properties `((upstream-name . "ENmix")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotationhub
           r-biobase
           r-doparallel
           r-dynamictreecut
           r-experimenthub
           r-foreach
           r-genefilter
           r-geneplotter
           r-gplots
           r-gtools
           r-illuminaio
           r-impute
           r-iranges
           r-irlba
           r-matrixstats
           r-minfi
           r-quadprog
           r-rpmm
           r-s4vectors
           r-summarizedexperiment))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/release/bioc/html/ENmix.html")
    (synopsis
     "Quality control and analysis tools for Illumina DNA methylation BeadChip")
    (description
     "This package provides tools for quality control, analysis and
visualization of Illumina DNA methylation array data.")
    (license license:artistic2.0)))

(define-public r-ensembldb
  (package
    (name "r-ensembldb")
    (version "2.28.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ensembldb" version))
       (sha256
        (base32
         "10h3ngwq99y0d3ksf0303qjw263ard1zsawpq1pw5n9r5g7xll47"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotationdbi
           r-annotationfilter
           r-biobase
           r-biocgenerics
           r-biostrings
           r-curl
           r-dbi
           r-genomeinfodb
           r-genomicfeatures
           r-genomicranges
           r-iranges
           r-protgenerics
           r-rsamtools
           r-rsqlite
           r-rtracklayer
           r-s4vectors))
    (native-inputs
     (list r-knitr))
    (home-page "https://github.com/jotsetung/ensembldb")
    (synopsis "Utilities to create and use Ensembl-based annotation databases")
    (description
     "The package provides functions to create and use transcript-centric
annotation databases/packages.  The annotation for the databases are directly
fetched from Ensembl using their Perl API.  The functionality and data is
similar to that of the TxDb packages from the @code{GenomicFeatures} package,
but, in addition to retrieve all gene/transcript models and annotations from
the database, the @code{ensembldb} package also provides a filter framework
allowing to retrieve annotations for specific entries like genes encoded on a
chromosome region or transcript models of lincRNA genes.")
    ;; No version specified
    (license license:lgpl3+)))

(define-public r-epidish
  (package
    (name "r-epidish")
    (version "2.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "EpiDISH" version))
       (sha256
        (base32 "17qkm5inavfdfrim9baqxdhs3w64c6y94qadw4y5p9xsck318frq"))))
    (properties `((upstream-name . "EpiDISH")))
    (build-system r-build-system)
    (propagated-inputs (list r-e1071
                             r-locfdr
                             r-mass
                             r-matrix
                             r-matrixstats
                             r-quadprog
                             r-stringr))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/sjczheng/EpiDISH")
    (synopsis "Epigenetic dissection of intra-sample-heterogeneity")
    (description
     "@code{EpiDISH} is a R package to infer the proportions of a priori known
cell-types present in a sample representing a mixture of such cell-types.
Right now, the package can be used on DNAm data of whole blood, generic
epithelial tissue and breast tissue.  Besides, the package provides a function
that allows the identification of differentially methylated cell-types and
their directionality of change in Epigenome-Wide Association Studies.")
    (license license:gpl2)))

(define-public r-fastseg
  (package
    (name "r-fastseg")
    (version "1.50.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "fastseg" version))
       (sha256
        (base32
         "03gl7z7q90jy47q57gfbj5fkv5n8hi43jbbrl53bdyp71f26ry2d"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biobase r-biocgenerics r-genomicranges r-iranges
           r-s4vectors))
    (native-inputs (list r-knitr))
    (home-page "https://www.bioinf.jku.at/software/fastseg/index.html")
    (synopsis "Fast segmentation algorithm for genetic sequencing data")
    (description
     "Fastseg implements a very fast and efficient segmentation algorithm.
It can segment data from DNA microarrays and data from next generation
sequencing for example to detect copy number segments.  Further it can segment
data from RNA microarrays like tiling arrays to identify transcripts.  Most
generally, it can segment data given as a matrix or as a vector.  Various data
formats can be used as input to fastseg like expression set objects for
microarrays or GRanges for sequencing data.")
    (license license:lgpl2.0+)))

(define-public r-gage
  (package
    (name "r-gage")
    (version "2.54.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "gage" version))
       (sha256
        (base32
         "0iwdv9y5x0na38arjqacz00s7plk7aqc1h2ynhlpbyl3nz0a0kyq"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotationdbi r-go-db r-graph r-keggrest))
    (home-page (string-append "https://bmcbioinformatics.biomedcentral.com/"
                              "articles/10.1186/1471-2105-10-161"))
    (synopsis "Generally applicable gene-set enrichment for pathway analysis")
    (description
     "GAGE is a published method for gene set (enrichment or GSEA) or pathway
analysis.  GAGE is generally applicable independent of microarray or RNA-Seq
data attributes including sample sizes, experimental designs, assay platforms,
and other types of heterogeneity.  The gage package provides functions for
basic GAGE analysis, result processing and presentation.  In addition, it
provides demo microarray data and commonly used gene set data based on KEGG
pathways and GO terms.  These functions and data are also useful for gene set
analysis using other methods.")
    (license license:gpl2+)))

(define-public r-genefilter
  (package
    (name "r-genefilter")
    (version "1.86.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "genefilter" version))
       (sha256
        (base32
         "0jp7jvgzb1bs0p76j9k8lhgliipjnif6bng7hdn5wi744jb5fmpr"))))
    (build-system r-build-system)
    (native-inputs
     (list gfortran r-knitr))
    (propagated-inputs
     (list r-annotate r-annotationdbi r-biobase r-matrixgenerics
           r-survival))
    (home-page "https://bioconductor.org/packages/genefilter")
    (synopsis "Filter genes from high-throughput experiments")
    (description
     "This package provides basic functions for filtering genes from
high-throughput sequencing experiments.")
    (license license:artistic2.0)))

(define-public r-geneoverlap
  (package
    (name "r-geneoverlap")
    (version "1.40.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "GeneOverlap" version))
              (sha256
               (base32
                "0bfd4322m6g7dvas214i17k8sjsf62lza9darmhiwb4r8h30qmb2"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-gplots r-rcolorbrewer))
    (home-page "https://www.bioconductor.org/packages/GeneOverlap/")
    (synopsis "Test and visualize gene overlaps")
    (description "This package can be used to test two sets of gene lists
and visualize the results.")
    (license license:gpl3)))

(define-public r-genomation
  (package
    (name "r-genomation")
    (version "1.36.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "genomation" version))
              (sha256
               (base32
                "1bfy46kka77q93s42hsbxpiynfkin5mh7cz3w592smw2dzkh01j4"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biostrings
           r-bsgenome
           r-data-table
           r-genomeinfodb
           r-genomicalignments
           r-genomicranges
           r-ggplot2
           r-gridbase
           r-impute
           r-iranges
           r-matrixstats
           r-plotrix
           r-plyr
           r-rcpp
           r-readr
           r-reshape2
           r-rsamtools
           r-rtracklayer
           r-s4vectors
           r-seqpattern))
    (native-inputs
     (list r-knitr))
    (home-page "https://bioinformatics.mdc-berlin.de/genomation/")
    (synopsis "Summary, annotation and visualization of genomic data")
    (description
     "This package provides a package for summary and annotation of genomic
intervals.  Users can visualize and quantify genomic intervals over
pre-defined functional regions, such as promoters, exons, introns, etc.  The
genomic intervals represent regions with a defined chromosome position, which
may be associated with a score, such as aligned reads from HT-seq experiments,
TF binding sites, methylation scores, etc.  The package can use any tabular
genomic feature data as long as it has minimal information on the locations of
genomic intervals.  In addition, it can use BAM or BigWig files as input.")
    (license license:artistic2.0)))

(define-public r-genomeinfodb
  (package
    (name "r-genomeinfodb")
    (version "1.40.1")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "GenomeInfoDb" version))
              (sha256
               (base32
                "155wj17pbdbixyg6mimv06wg3ylll8ysqh7rffjp65fw662jwhw0"))))
    (properties
     `((upstream-name . "GenomeInfoDb")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocgenerics r-genomeinfodbdata r-iranges r-s4vectors
           r-ucsc-utils))
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/GenomeInfoDb")
    (synopsis "Utilities for manipulating chromosome identifiers")
    (description
     "This package contains data and functions that define and allow
translation between different chromosome sequence naming conventions (e.g.,
\"chr1\" versus \"1\"), including a function that attempts to place sequence
names in their natural, rather than lexicographic, order.")
    (license license:artistic2.0)))

(define-public r-genomicalignments
  (package
    (name "r-genomicalignments")
    (version "1.40.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "GenomicAlignments" version))
              (sha256
               (base32
                "15jzdjna0r9n6ybvyw52v1d9f05vdsdxff0g8q0pa584a1sd2s7s"))))
    (properties
     `((upstream-name . "GenomicAlignments")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocgenerics
           r-biocparallel
           r-biostrings
           r-genomeinfodb
           r-genomicranges
           r-iranges
           r-rsamtools
           r-s4vectors
           r-summarizedexperiment))
    (home-page "https://bioconductor.org/packages/GenomicAlignments")
    (synopsis "Representation and manipulation of short genomic alignments")
    (description
     "This package provides efficient containers for storing and manipulating
short genomic alignments (typically obtained by aligning short reads to a
reference genome).  This includes read counting, computing the coverage,
junction detection, and working with the nucleotide content of the
alignments.")
    (license license:artistic2.0)))

(define-public r-genomicdatacommons
  (package
    (name "r-genomicdatacommons")
    (version "1.28.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "GenomicDataCommons" version))
              (sha256
               (base32
                "0jg4sa1lqn028y2ympb3x4s8yr7qa8ajrwqj2hhcl8zss3sx8gr2"))))
    (properties `((upstream-name . "GenomicDataCommons")))
    (build-system r-build-system)
    (propagated-inputs (list r-dplyr
                             r-genomicranges
                             r-httr
                             r-iranges
                             r-jsonlite
                             r-magrittr
                             r-rappdirs
                             r-readr
                             r-rlang
                             r-tibble
                             r-tidyr
                             r-xml2))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/GenomicDataCommons")
    (synopsis "NIH/NCI genomic data commons access")
    (description
     "This package lets you programmatically access the NIH/NCI Genomic Data
Commons RESTful service.")
    (license license:artistic2.0)))

(define-public r-genomicfeatures
  (package
    (name "r-genomicfeatures")
    (version "1.56.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "GenomicFeatures" version))
              (sha256
               (base32
                "1r1d9g5vq5y946v2z2xgmfb31k0576nj8i0yg6xfj9y2a0a6akc5"))))
    (properties
     `((upstream-name . "GenomicFeatures")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotationdbi
           r-biocgenerics
           r-biostrings
           r-dbi
           r-genomeinfodb
           r-genomicranges
           r-iranges
           r-rtracklayer
           r-s4vectors
           r-xvector))
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/GenomicFeatures")
    (synopsis "Tools for working with transcript centric annotations")
    (description
     "This package provides a set of tools and methods for making and
manipulating transcript centric annotations.  With these tools the user can
easily download the genomic locations of the transcripts, exons and cds of a
given organism, from either the UCSC Genome Browser or a BioMart
database (more sources will be supported in the future).  This information is
then stored in a local database that keeps track of the relationship between
transcripts, exons, cds and genes.  Flexible methods are provided for
extracting the desired features in a convenient format.")
    (license license:artistic2.0)))

(define-public r-genomicfiles
  (package
    (name "r-genomicfiles")
    (version "1.40.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "GenomicFiles" version))
       (sha256
        (base32
         "1b0n5aqvg6jb6rpikjzxndmcjxp31h6vhr1magzalqg7jmlvcv6i"))))
    (properties `((upstream-name . "GenomicFiles")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocgenerics
           r-biocparallel
           r-genomeinfodb
           r-genomicalignments
           r-genomicranges
           r-iranges
           r-matrixgenerics
           r-rsamtools
           r-rtracklayer
           r-s4vectors
           r-summarizedexperiment
           r-variantannotation))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/GenomicFiles")
    (synopsis "Distributed computing by file or by range")
    (description
     "This package provides infrastructure for parallel computations
distributed by file or by range.  User defined mapper and reducer functions
provide added flexibility for data combination and manipulation.")
    (license license:artistic2.0)))

(define-public r-genomicranges
  (package
    (name "r-genomicranges")
    (version "1.56.1")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "GenomicRanges" version))
              (sha256
               (base32
                "0g0ld9i58pdxw2pwxi9nzj82mkryag457pvh6cvbvhb28pfamkma"))))
    (properties
     `((upstream-name . "GenomicRanges")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocgenerics r-genomeinfodb r-iranges r-s4vectors r-xvector))
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/GenomicRanges")
    (synopsis "Representation and manipulation of genomic intervals")
    (description
     "This package provides tools to efficiently represent and manipulate
genomic annotations and alignments is playing a central role when it comes to
analyzing high-throughput sequencing data (a.k.a. NGS data).  The
GenomicRanges package defines general purpose containers for storing and
manipulating genomic intervals and variables defined along a genome.")
    (license license:artistic2.0)))

(define-public r-glad
  (package
    (name "r-glad")
    (version "2.68.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "GLAD" version))
              (sha256
               (base32
                "01qn4v7b7b633dz01njzmg48lga861pfqnys9f0fbk9damxc6rr1"))))
    (properties `((upstream-name . "GLAD")))
    (build-system r-build-system)
    (inputs (list gsl))
    (propagated-inputs (list r-aws))
    (native-inputs (list pkg-config))
    (home-page "http://bioinfo.curie.fr")
    (synopsis "Gain and loss analysis of DNA")
    (description
     "This package helps with the analysis of array @acronym{CGH, comparative
genomic hybridization} data by detecting of the breakpoints in the genomic
profiles and assignment of a status (gain, normal or loss) to each chromosomal
regions identified.")
    (license license:gpl2)))

(define-public r-globalancova
  (package
    (name "r-globalancova")
    (version "4.22.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "GlobalAncova" version))
              (sha256
               (base32
                "0z71a5h4y740k0hx84aavfxl1kpshm4gqs7mh6zf895i2r761bqh"))))
    (properties `((upstream-name . "GlobalAncova")))
    (build-system r-build-system)
    (propagated-inputs (list r-annotate
                             r-annotationdbi
                             r-biobase
                             r-corpcor
                             r-dendextend
                             r-globaltest
                             r-gseabase
                             r-vgam))
    (home-page "https://bioconductor.org/packages/GlobalAncova")
    (synopsis "Global test for groups of variables via model comparisons")
    (description
     "This package supports the computation of an F-test for the association
between expression values and clinical entities.  In many cases a two way
layout with gene and a dichotomous group as factors will be considered.
However, adjustment for other covariates and the analysis of arbitrary
clinical variables, interactions, gene co-expression, time series data and so
on is also possible.  The test is carried out by comparison of corresponding
linear models via the extra sum of squares principle.")
    (license license:gpl2+)))

(define-public r-globaltest
  (package
    (name "r-globaltest")
    (version "5.58.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "globaltest" version))
              (sha256
               (base32
                "1cpvs1jcp9ibis06pkbqvhhksy6cis0zynx774d0hqdbb9ljl945"))))
    (properties `((upstream-name . "globaltest")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotate r-annotationdbi r-biobase r-survival))
    (home-page "https://bioconductor.org/packages/globaltest")
    (synopsis
     "Test groups of covariates for association with a response variable")
    (description
     "The global test tests groups of covariates (or features) for association
with a response variable.  This package implements the test with diagnostic
plots and multiple testing utilities, along with several functions to
facilitate the use of this test for gene set testing of GO and KEGG terms.")
    (license license:gpl2+)))

(define-public r-gostats
  (package
    (name "r-gostats")
    (version "2.70.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "GOstats" version))
       (sha256
        (base32
         "1dygsr8nlqksfpk00lk67dhj4fcpjg1x4dbl2r4sfanlk4hq76cs"))))
    (properties `((upstream-name . "GOstats")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotate
           r-annotationdbi
           r-annotationforge
           r-biobase
           r-category
           r-go-db
           r-graph
           r-rbgl
           r-rgraphviz))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/GOstats")
    (synopsis "Tools for manipulating GO and microarrays")
    (description
     "This package provides a set of tools for interacting with GO and
microarray data.  A variety of basic manipulation tools for graphs, hypothesis
testing and other simple calculations.")
    (license license:artistic2.0)))

(define-public r-gseabase
  (package
    (name "r-gseabase")
    (version "1.66.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "GSEABase" version))
       (sha256
        (base32
         "1pnd8rmpk7gm89cvrqa8qwk40dszkfkhyydxp3gl13zng9c5bq40"))))
    (properties `((upstream-name . "GSEABase")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotate
           r-annotationdbi
           r-biobase
           r-biocgenerics
           r-graph
           r-xml))
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/GSEABase")
    (synopsis "Gene set enrichment data structures and methods")
    (description
     "This package provides classes and methods to support @dfn{Gene Set
Enrichment Analysis} (GSEA).")
    (license license:artistic2.0)))

(define-public r-gsva
  (package
    (name "r-gsva")
    (version "1.52.3")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "GSVA" version))
              (sha256
               (base32
                "08nxg3q350n5gf975r6b0zq4cr7lchv2i811crc4xbb7q39kw8k4"))))
    (properties `((upstream-name . "GSVA")))
    (build-system r-build-system)
    (propagated-inputs (list r-biobase
                             r-biocparallel
                             r-biocsingular
                             r-delayedarray
                             r-delayedmatrixstats
                             r-gseabase
                             r-hdf5array
                             r-iranges
                             r-matrix
                             r-s4vectors
                             r-singlecellexperiment
                             r-sparsematrixstats
                             r-spatialexperiment
                             r-summarizedexperiment))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/rcastelo/GSVA")
    (synopsis "Gene Set Variation Analysis for microarray and RNA-seq data")
    (description
     "Gene Set Variation Analysis (GSVA) is a non-parametric, unsupervised
method for estimating variation of gene set enrichment through the samples of
a expression data set.  GSVA performs a change in coordinate systems,
transforming the data from a gene by sample matrix to a gene-set by sample
matrix, thereby allowing the evaluation of pathway enrichment for each sample.
This new matrix of GSVA enrichment scores facilitates applying standard
analytical methods like functional enrichment, survival analysis, clustering,
CNV-pathway analysis or cross-tissue pathway analysis, in a pathway-centric
manner.")
    (license license:gpl2+)))

(define-public r-gypsum
  (package
    (name "r-gypsum")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "gypsum" version))
       (sha256
        (base32 "1zrmxmlnbiifc2p5mn10y3vpmyzqjimd2hvc3s9c0j0iv6ryxjv0"))))
    (properties `((upstream-name . "gypsum")))
    (build-system r-build-system)
    (propagated-inputs (list r-filelock r-httr2 r-jsonlite))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/ArtifactDB/gypsum-R")
    (synopsis "Interface to the gypsum REST API")
    (description
     "This package provides a client for the gypsum REST
API (https://gypsum.artifactdb.com), a cloud-based file store in the
@code{ArtifactDB} ecosystem.  This package provides functions for uploads,
downloads, and various adminstrative and management tasks.  Check out the
documentation at @url{https://github.com/ArtifactDB/gypsum-worker} for more
details.")
    (license license:expat)))

(define-public r-harshlight
  (package
    (name "r-harshlight")
    (version "1.76.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "Harshlight" version))
              (sha256
               (base32
                "1nqi2ad9hnm6j8hn8f6xp9zav6rsa5wlzb2z5fjnm9q4xbjblnjl"))))
    (properties `((upstream-name . "Harshlight")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-affy
           r-altcdfenvs
           r-biobase))
    (home-page "http://asterion.rockefeller.edu/Harshlight/")
    (synopsis "Corrective make-up program for microarray chips")
    (description
     "The package detects extended diffuse and compact blemishes on microarray
chips.  Harshlight marks the areas in a collection of chips (affybatch
objects).  A corrected @code{AffyBatch} object will result.  The package
replaces the defected areas with @code{N/A}s or the median of the values of
the same probe.  The new version handles the substitute value as a whole
matrix to solve the memory problem.")
    (license license:gpl2+)))

(define-public r-hpar
  (package
    (name "r-hpar")
    (version "1.46.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "hpar" version))
       (sha256
        (base32
         "1qqa0p8k3h8pbbgfz9d1k08zml5i5r9iwi2xqqj1hg84n2saqkzp"))))
    (build-system r-build-system)
    (arguments
     (list
      #:phases
      '(modify-phases %standard-phases
         (add-after 'unpack 'set-HOME
           (lambda _
             (setenv "HOME" "/tmp")))
         (add-after 'unpack 'avoid-internet-access
           (lambda _
             (setenv "GUIX_BUILD" "yes")
             (substitute* "R/zzz.R"
               (("ExperimentHub::createHubAccessors.*" m)
                (string-append
                 "if (Sys.getenv(\"GUIX_BUILD\") == \"\") {" m "}"))))))))
    (propagated-inputs (list r-experimenthub))
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/hpar/")
    (synopsis "Human Protein Atlas in R")
    (description "This package provides a simple interface to and data from
the Human Protein Atlas project.")
    (license license:artistic2.0)))

(define-public r-r3cseq
  (package
    (name "r-r3cseq")
    (version "1.50.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "r3Cseq" version))
       (sha256
        (base32 "02dc6milwymwj90x24fip8av84ik9dp69sg9g7l300kwyk0hcrb7"))))
    (properties `((upstream-name . "r3Cseq")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biostrings
           r-data-table
           r-genomeinfodb
           r-genomicranges
           r-iranges
           r-qvalue
           r-rcolorbrewer
           r-rsamtools
           r-rtracklayer
           r-sqldf
           r-vgam))
    (home-page "http://r3cseq.genereg.net/Site/index.html")
    (synopsis
     "Analysis of Chromosome conformation capture and Next-generation sequencing")
    (description
     "This package is used for the analysis of long-range chromatin
interactions from 3C-seq assay.")
    (license license:gpl3)))

(define-public r-r4rna
  (package
    (name "r-r4rna")
    (version "1.32.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "R4RNA" version))
       (sha256
        (base32 "0cpfngd11zlws756q8cnhy5451kj8hg5ckilvmhndfapkz4w93x1"))))
    (properties `((upstream-name . "R4RNA")))
    (build-system r-build-system)
    (propagated-inputs (list r-biostrings))
    (home-page "https://www.e-rna.org/r-chie/")
    (synopsis "RNA visualization and analysis")
    (description
     "This package provides a package for RNA basepair analysis, including the
visualization of basepairs as arc diagrams for easy comparison and annotation of
sequence and structure.  Arc diagrams can additionally be projected onto
multiple sequence alignments to assess basepair conservation and covariation,
with numerical methods for computing statistics for each.")
    (license license:gpl3)))

(define-public r-radiogx
  (package
    (name "r-radiogx")
    (version "2.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "RadioGx" version))
       (sha256
        (base32 "0ykw5hm48fh6xqxdfgmjjbqdd5hp4dmhnykg3zkmb9wrcfp4zx7f"))))
    (properties `((upstream-name . "RadioGx")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-assertthat
           r-biobase
           r-biocgenerics
           r-biocparallel
           r-catools
           r-coregx
           r-data-table
           r-downloader
           r-magicaxis
           r-matrixstats
           r-rcolorbrewer
           r-reshape2
           r-s4vectors
           r-scales
           r-summarizedexperiment))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/RadioGx")
    (synopsis "Analysis of large-scale radio-genomic data")
    (description
     "This package is a computational tool box for radio-genomic analysis
which integrates radio-response data, radio-biological modelling and
comprehensive cell line annotations for hundreds of cancer cell lines.  The
@code{RadioSet} class enables creation and manipulation of standardized
datasets including information about cancer cells lines, radio-response assays
and dose-response indicators.  Included methods allow fitting and plotting
dose-response data using established radio-biological models along with
quality control to validate results.  Additional functions related to fitting
and plotting dose response curves, quantifying statistical correlation and
calculating @acronym{AUC, area under the curve} or @acronym{SF, survival
fraction} are included.")
    (license license:gpl3)))

(define-public r-raggedexperiment
  (package
    (name "r-raggedexperiment")
    (version "1.28.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "RaggedExperiment" version))
              (sha256
               (base32
                "1mzjc52naf6brffv6d2bak16y3ygz9lsk8yqgcfa44180bslz3xx"))))
    (properties `((upstream-name . "RaggedExperiment")))
    (build-system r-build-system)
    (propagated-inputs (list r-biocbaseutils
                             r-biocgenerics
                             r-genomeinfodb
                             r-genomicranges
                             r-iranges
                             r-matrix
                             r-matrixgenerics
                             r-s4vectors
                             r-summarizedexperiment))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/RaggedExperiment")
    (synopsis "Representation of sparse experiments and assays across samples")
    (description
     "This package provides a flexible representation of copy number,
mutation, and other data that fit into the ragged array schema for genomic
location data.  The basic representation of such data provides a rectangular
flat table interface to the user with range information in the rows and
samples/specimen in the columns.  The @code{RaggedExperiment} class derives
from a @code{GRangesList} representation and provides a semblance of a
rectangular dataset.")
    (license license:artistic2.0)))

(define-public r-rdisop
  (package
    (name "r-rdisop")
    (version "1.64.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "Rdisop" version))
              (sha256
               (base32
                "1gcg441mp6bdnzjkh4gg6zgjn4hnw5i8mdcvb1hfim4hsc7sdsxv"))))
    (properties `((upstream-name . "Rdisop")))
    (build-system r-build-system)
    (propagated-inputs (list r-rcpp))
    (home-page "https://github.com/sneumann/Rdisop")
    (synopsis "Decomposition of isotopic patterns")
    (description
     "This is a package for identification of metabolites using high precision
mass spectrometry.  MS peaks are used to derive a ranked list of sum formulae,
alternatively for a given sum formula the theoretical isotope distribution can
be calculated to search in MS peak lists.")
    (license license:gpl2)))

(define-public r-rhtslib
  (package
    (name "r-rhtslib")
    (version "3.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Rhtslib" version))
       (sha256
        (base32
         "0497b43wxcfqyr4k30f0xq7yillb4fz0gqiqxxp66gvnqv4wiv6v"))))
    (properties `((upstream-name . "Rhtslib")))
    (build-system r-build-system)
    ;; Without this a temporary directory ends up in the Rhtslib.so binary,
    ;; which makes R abort the build.
    (arguments '(#:configure-flags '("--no-staged-install")))
    (propagated-inputs
     (list curl zlib ; packages using rhtslib need to link with zlib
           r-zlibbioc))
    (native-inputs
     (list pkg-config r-knitr))
    (home-page "https://github.com/nhayden/Rhtslib")
    (synopsis "High-throughput sequencing library as an R package")
    (description
     "This package provides the HTSlib C library for high-throughput
nucleotide sequence analysis.  The package is primarily useful to developers
of other R packages who wish to make use of HTSlib.")
    (license license:lgpl2.0+)))

(define-public r-rnbeads
  (package
    (name "r-rnbeads")
    (version "2.22.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "RnBeads" version))
       (sha256
        (base32 "0v7dzmbyskq0vmja09dncg1g50dl3iycg883swb7lfvi4xzr42vj"))))
    (properties `((upstream-name . "RnBeads")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocgenerics
           r-cluster
           r-ff
           r-fields
           r-genomicranges
           r-ggplot2
           r-gplots
           r-gridextra
           r-illuminaio
           r-iranges
           r-limma
           r-mass
           r-matrixstats
           r-methylumi
           r-plyr
           r-s4vectors))
    (home-page "https://bioconductor.org/packages/RnBeads")
    (synopsis "RnBeads")
    (description
     "@code{RnBeads} facilitates comprehensive analysis of various types of DNA
methylation data at the genome scale.")
    (license license:gpl3)))

(define-public r-impute
  (package
    (name "r-impute")
    (version "1.78.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "impute" version))
              (sha256
               (base32
                "1g0m9298srkhjnq3mc5rd7krr9y3cy74yvg9k0j7ypc9nmgcqabr"))))
    (native-inputs
     (list gfortran))
    (build-system r-build-system)
    (home-page "https://bioconductor.org/packages/impute")
    (synopsis "Imputation for microarray data")
    (description
     "This package provides a function to impute missing gene expression
microarray data, using nearest neighbor averaging.")
    (license license:gpl2+)))

(define-public r-interactivedisplay
  (package
    (name "r-interactivedisplay")
    (version "1.42.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "interactiveDisplay" version))
              (sha256
               (base32
                "14k1j8bmpbrldx61fbvn20f7l2lc50bj9iy01a5jf0ddcbi27x5g"))
              (snippet
               '(for-each delete-file
                          '("inst/www/js/d3.v2.js"
                            "inst/www/js/jquery.js"
                            "inst/www/js/jquery.min.js"
                            "inst/www/js/jquery.dataTables.min.js"
                            "inst/www/js/jquery.dataTables.nightly.js")))))
    (properties `((upstream-name . "interactiveDisplay")))
    (build-system r-build-system)
    (arguments
     (list
      #:modules
      '((guix build r-build-system)
        (guix build minify-build-system)
        (guix build utils)
        (ice-9 match))
      #:imported-modules
      `(,@%r-build-system-modules
        (guix build minify-build-system))
      #:phases
      #~(modify-phases (@ (guix build r-build-system) %standard-phases)
          (add-after 'unpack 'process-javascript
            (lambda* (#:key inputs #:allow-other-keys)
              (with-directory-excursion "inst/"
                (for-each (match-lambda
                            ((source . target)
                             (minify source #:target target)))
                          `((,(assoc-ref inputs "js-jquery-1.8.2")
                             . "www/js/jquery.js")
                            (,(assoc-ref inputs "js-jquery-1.9.1")
                             . "www/js/jquery.min.js")
                            (,(search-input-file inputs
                                                 "/share/javascript/jquery.dataTables.min.js")
                             . "www/js/jquery.dataTables.min.js")
                            (,(string-append (assoc-ref inputs "js-datatables-1.9")
                                             "/share/javascript/jquery.dataTables.min.js")
                             . "www/js/jquery.dataTables.min.js")
                            (,(string-append (assoc-ref inputs "js-datatables-1.10")
                                             "/share/javascript/jquery.dataTables.min.js")
                             . "www/js/jquery.dataTables.nightly.js")
                            (,(assoc-ref inputs "js-d3-v2")
                             . "www/js/d3.v2.js")))))))))
    (propagated-inputs
     (list r-annotationdbi
           r-biocgenerics
           r-biocmanager ;this is not listed in DESCRIPTION
           r-category
           r-ggplot2
           r-gridsvg
           r-interactivedisplaybase
           r-plyr
           r-rcolorbrewer
           r-reshape2
           r-shiny
           r-xml))
    (native-inputs
     `(("esbuild" ,esbuild)
       ("r-knitr" ,r-knitr)
       ("js-d3-v2"
        ,(origin
           (method url-fetch)
           (uri "https://web.archive.org/web/20230428092426id_/https://d3js.org/d3.v2.js")
           (sha256
            (base32
             "1m57mxhcynfaz6gz3v0aph5i6hx5jf455jdygyl8yzs9r2dpp5vr"))))
       ("js-datatables-1.9" ,js-datatables-1.9)
       ("js-datatables-1.10" ,js-datatables)
       ("js-jquery-1.8.2"
        ,(origin
           (method url-fetch)
           (uri "https://code.jquery.com/jquery-1.8.2.js")
           (sha256
            (base32
             "0nikk2clbnyi02k0brvhbd8m43lfh4l1zrya35jya9sy6wb9b9ng"))))
       ("js-jquery-1.9.1"
        ,(origin
           (method url-fetch)
           (uri "https://code.jquery.com/jquery-1.9.1.js")
           (sha256
            (base32
             "0h4dk67yc9d0kadqxb6b33585f3x3559p6qmp70l00qwq030vn3v"))))))
    (home-page "https://bioconductor.org/packages/interactiveDisplay")
    (synopsis "Package for Shiny web displays of Bioconductor objects")
    (description
     "This package offers interactive Shiny displays for Bioconductor
objects.  In addition, this package empowers users to develop engaging
visualizations and interfaces for working with Bioconductor data.")
    (license license:artistic2.0)))

(define-public r-interactivedisplaybase
  (package
    (name "r-interactivedisplaybase")
    (version "1.42.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "interactiveDisplayBase" version))
       (sha256
        (base32
         "0ih9b6n81gr84vhmwcq49knrbmkwbh50gy5hrap654nicb3j3jzb"))))
    (properties
     `((upstream-name . "interactiveDisplayBase")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocgenerics r-dt r-shiny))
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/interactiveDisplayBase")
    (synopsis "Base package for web displays of Bioconductor objects")
    (description
     "This package contains the basic methods needed to generate interactive
Shiny-based display methods for Bioconductor objects.")
    (license license:artistic2.0)))

(define-public r-keggrest
  (package
    (name "r-keggrest")
    (version "1.44.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "KEGGREST" version))
       (sha256
        (base32
         "09qdsdbz5ha3b2fafwv80n6pdajjgsc5w52n1dy6ds41vxgf0jis"))))
    (properties `((upstream-name . "KEGGREST")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biostrings r-httr r-png))
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/KEGGREST")
    (synopsis "Client-side REST access to KEGG")
    (description
     "This package provides a package that provides a client interface to the
@dfn{Kyoto Encyclopedia of Genes and Genomes} (KEGG) REST server.")
    (license license:artistic2.0)))

(define-public r-lea
  (package
    (name "r-lea")
    (version "3.16.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "LEA" version))
              (sha256
               (base32
                "0r6nj6ck77fhbmqifbx4srj1f3yr7haiinphcm0p4g8b7zn4a6fm"))))
    (properties `((upstream-name . "LEA")))
    (build-system r-build-system)
    (native-inputs (list r-knitr))
    (home-page "http://membres-timc.imag.fr/Olivier.Francois/LEA/index.htm")
    (synopsis
     "R package for landscape and ecological association studies")
    (description
     "LEA is an R package dedicated to population genomics, landscape genomics
and genotype-environment association tests.  LEA can run analyses of
population structure and genome-wide tests for local adaptation, and also
performs imputation of missing genotypes.  The package includes statistical
methods for estimating ancestry coefficients from large genotypic matrices and
for evaluating the number of ancestral populations (snmf).  It performs
statistical tests using latent factor mixed models for identifying genetic
polymorphisms that exhibit association with environmental gradients or
phenotypic traits (lfmm2).  In addition, LEA computes values of genetic offset
statistics based on new or predicted environments (@code{genetic.gap},
@code{genetic.offset}).  LEA is mainly based on optimized programs that can
scale with the dimensions of large data sets.")
    (license license:gpl3)))

(define-public r-lfa
  (package
    (name "r-lfa")
    (version "2.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "lfa" version))
       (sha256
        (base32 "1sbgr6h2kpfpzw3fkqd1075wrmbblb97pdywmvl8xv52vg0wqxz3"))))
    (properties `((upstream-name . "lfa")))
    (build-system r-build-system)
    (propagated-inputs (list r-corpcor r-rspectra))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/StoreyLab/lfa")
    (synopsis "Logistic Factor Analysis for categorical data")
    (description
     "@dfn{Logistic Factor Analysis} (LFA) is a method for a PCA analogue on
Binomial data via estimation of latent structure in the natural parameter.")
    (license license:gpl3)))

(define-public r-limma
  (package
    (name "r-limma")
    (version "3.60.3")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "limma" version))
              (sha256
               (base32
                "0fy3lmjrhy7301v9far5n6c3m4n3nad463rgy2fka3bk4jls6ipg"))))
    (build-system r-build-system)
    (native-inputs (list r-knitr))
    (propagated-inputs (list r-statmod))
    (home-page "https://bioinf.wehi.edu.au/limma")
    (synopsis "Package for linear models for microarray and RNA-seq data")
    (description "This package can be used for the analysis of gene expression
studies, especially the use of linear models for analysing designed experiments
and the assessment of differential expression.  The analysis methods apply to
different technologies, including microarrays, RNA-seq, and quantitative PCR.")
    (license license:gpl2+)))

(define-public r-maaslin2
  (package
    (name "r-maaslin2")
    (version "1.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Maaslin2" version))
       (sha256
        (base32 "0dl6s4lcd40nd7v0d3k3zn0mkm5rlzb8kf2cjxz298mgdif63xqh"))))
    (properties `((upstream-name . "Maaslin2")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biglm
           r-car
           r-chemometrics
           r-cplm
           r-data-table
           r-dplyr
           r-edger
           r-ggplot2
           r-glmmtmb
           r-hash
           r-lme4
           r-lmertest
           r-logging
           r-mass
           r-metagenomeseq
           r-optparse
           r-pbapply
           r-pcapp
           r-pheatmap
           r-pscl
           r-robustbase
           r-tibble
           r-vegan))
    (native-inputs (list r-knitr))
    (home-page "http://huttenhower.sph.harvard.edu/maaslin2")
    (synopsis
     "Multivariable association discovery in population-scale meta-omics studies")
    (description
     "MaAsLin2 is comprehensive R package for efficiently determining multivariable
association between clinical metadata and microbial meta'omic features.  This
package relies on general linear models to accommodate most modern epidemiological
study designs, including cross-sectional and longitudinal, and offers a variety
of data exploration, normalization, and transformation methods.")
    (license license:expat)))

(define-public r-made4
  (package
    (name "r-made4")
    (version "1.78.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "made4" version))
              (sha256
               (base32
                "0x96lhkz0fwp3qis8k2lllmi520b887bz26jcq3zlk1kjhbg91a8"))))
    (properties `((upstream-name . "made4")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-ade4
           r-biobase
           r-gplots
           r-rcolorbrewer
           r-scatterplot3d
           r-summarizedexperiment))
    (native-inputs (list r-knitr))
    (home-page "http://www.hsph.harvard.edu/aedin-culhane/")
    (synopsis "Multivariate analysis of microarray data using ADE4")
    (description
     "This is a package for multivariate data analysis and graphical display
of microarray data.  Functions are included for supervised dimension
reduction (between group analysis) and joint dimension reduction of two
datasets (coinertia analysis).")
    (license license:artistic2.0)))

(define-public r-makecdfenv
  (package
    (name "r-makecdfenv")
    (version "1.80.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "makecdfenv" version))
              (sha256
               (base32
                "0c1ks5f2rc0mwbfji6iicvypbmm46mq8yckmdpwiyp22l7l05aqz"))))
    (properties `((upstream-name . "makecdfenv")))
    (build-system r-build-system)
    (inputs (list zlib))
    (propagated-inputs
     (list r-affy
           r-affyio
           r-biobase
           r-zlibbioc))
    (home-page "https://bioconductor.org/packages/makecdfenv")
    (synopsis "Chip description file environment maker")
    (description
     "This package implements two functions.  One of them reads an Affymetrix
@acronym{CDF, chip description file} and creates a hash table environment
containing the location/probe set membership mapping.  The other one creates a
package that automatically loads that environment.")
    (license license:gpl2+)))

(define-public r-manor
  (package
    (name "r-manor")
    (version "1.76.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "MANOR" version))
       (sha256
        (base32 "0mlhv4gy4bxvdm0drwwrm4x48wy0mghzcqhvaw2rhw0h65rmy5ps"))))
    (properties `((upstream-name . "MANOR")))
    (build-system r-build-system)
    (propagated-inputs (list r-glad))
    (native-inputs (list r-knitr))
    (home-page "http://bioinfo.curie.fr/projects/manor/index.html")
    (synopsis "CGH micro-array normalization")
    (description
     "This package ofers functions for importation, normalization,
visualization, and quality control to correct identified sources of
variability in array of @acronym{CGH, comparative genomic hybridization}
experiments.")
    (license license:gpl2)))

(define-public r-maser
  (package
    (name "r-maser")
    (version "1.22.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "maser" version))
              (sha256
               (base32
                "0w62dl65ysgcwixgmmb70qp6f85vhfnqjsww5snkjd48ypwx65ca"))))
    (properties `((upstream-name . "maser")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocgenerics
           r-data-table
           r-dplyr
           r-dt
           r-genomeinfodb
           r-genomicranges
           r-ggplot2
           r-gviz
           r-iranges
           r-reshape2
           r-rtracklayer))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/DiogoVeiga/maser")
    (synopsis "Mapping alternative splicing events to proteins")
    (description
     "This package provides functionalities for downstream analysis, annotation
and visualizaton of alternative splicing events generated by rMATS.")
    (license license:expat)))

(define-public r-mdqc
  (package
    (name "r-mdqc")
    (version "1.66.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "mdqc" version))
       (sha256
        (base32 "1frn6s43j6sbwnb5h2hykccvi7cwbasp8l37rc3nkxpc6szvsby6"))))
    (properties `((upstream-name . "mdqc")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-cluster
           r-mass))
    (home-page "https://bioconductor.org/packages/mdqc")
    (synopsis "Mahalanobis distance quality control for microarrays")
    (description
     "MDQC is a multivariate quality assessment method for microarrays based
on quality control (QC) reports.  The Mahalanobis distance of an array's
quality attributes is used to measure the similarity of the quality of that
array against the quality of the other arrays.  Then, arrays with unusually
high distances can be flagged as potentially low-quality.")
    (license license:lgpl2.0+)))

(define-public r-metabocoreutils
  (package
    (name "r-metabocoreutils")
    (version "1.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "MetaboCoreUtils" version))
       (sha256
        (base32 "1y5lq1x60pcvxsl8dim5plzd3y4bqjkcam5hv9614xw0sr4h0r04"))))
    (properties `((upstream-name . "MetaboCoreUtils")))
    (build-system r-build-system)
    (propagated-inputs (list r-biocparallel r-mscoreutils))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/RforMassSpectrometry/MetaboCoreUtils")
    (synopsis "Core utils for Metabolomics data")
    (description
     "@code{MetaboCoreUtils} defines metabolomics-related core functionality
provided as low-level functions to allow a data structure-independent usage
across various R packages.  This includes functions to calculate between
ion (adduct) and compound mass-to-charge ratios and masses or functions to
work with chemical formulas.  The package provides also a set of adduct
definitions and information on some commercially available internal standard
mixes commonly used in MS experiments.")
    (license license:artistic2.0)))

(define-public r-metagenomeseq
  (package
    (name "r-metagenomeseq")
    (version "1.46.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "metagenomeSeq" version))
       (sha256
        (base32 "0bjbiwbgs0lv0j6gjv5rhqhmqk0p7f80lfig4n24i4bqwkd2j26k"))))
    (properties `((upstream-name . "metagenomeSeq")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biobase
           r-foreach
           r-glmnet
           r-gplots
           r-limma
           r-matrix
           r-matrixstats
           r-rcolorbrewer
           r-wrench))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/HCBravoLab/metagenomeSeq")
    (synopsis "Statistical analysis for sparse high-throughput sequencing")
    (description
     "MetagenomeSeq is designed to determine features (be it @acronym{OTU,
Operational Taxanomic Unit}, species, etc.) that are differentially abundant
between two or more groups of multiple samples.  This package is designed to
address the effects of both normalization and under-sampling of microbial
communities on disease association detection and the testing of feature
correlations.")
    (license license:artistic2.0)))

(define-public r-metaneighbor
  (package
    (name "r-metaneighbor")
    (version "1.24.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "MetaNeighbor" version))
              (sha256
               (base32
                "0sz15njb0y9vcmbv89k3x2sw4px8nmy3qhqkvyl69kxlcqdicgxi"))))
    (properties `((upstream-name . "MetaNeighbor")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-beanplot
           r-dplyr
           r-ggplot2
           r-gplots
           r-igraph
           r-matrix
           r-matrixstats
           r-rcolorbrewer
           r-singlecellexperiment
           r-summarizedexperiment
           r-tibble
           r-tidyr))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/MetaNeighbor")
    (synopsis "Single cell replicability analysis")
    (description
     "This package implements a method to rapidly assess cell type identity using
both functional and random gene sets and it allows users to quantify cell type
replicability across datasets using neighbor voting.  @code{MetaNeighbor} works
on the basis that cells of the same type should have more similar gene expression
profiles than cells of different types.")
    (license license:expat)))

(define-public r-methylaid
  (package
    (name "r-methylaid")
    (version "1.38.0")
    (source
    (origin
      (method url-fetch)
      (uri (bioconductor-uri "MethylAid" version))
      (sha256
        (base32 "084mnllaj6qp5ai5bmj6wgc90bs270qbzl60sch1f863x8fv9lzr"))))
    (properties `((upstream-name . "MethylAid")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biobase
           r-biocgenerics
           r-biocparallel
           r-ggplot2
           r-gridbase
           r-hexbin
           r-matrixstats
           r-minfi
           r-rcolorbrewer
           r-shiny
           r-summarizedexperiment))
    (native-inputs (list r-knitr))
    (home-page "https://git.bioconductor.org/packages/MethylAid")
    (synopsis
     "Quality control of large Illumina DNA Methylation array data sets")
    (description
     "This package provides a visual and interactive web application using
RStudio's shiny package.  Bad quality samples are detected using sample-dependent
and sample-independent controls present on the array and user adjustable
thresholds.  In depth exploration of bad quality samples can be performed using
several interactive diagnostic plots of the quality control probes present on
the array.  Furthermore, the impact of any batch effect provided by the user can
be explored.")
    (license license:gpl2+)))

(define-public r-methylkit
  (package
    (name "r-methylkit")
    (version "1.30.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "methylKit" version))
              (sha256
               (base32
                "000dvi63cjikjbq3q83hlspd7y62rpswzg6hs5z1z1pj6a8rifdj"))))
    (properties `((upstream-name . "methylKit")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-data-table
           r-emdbook
           r-fastseg
           r-genomeinfodb
           r-genomicranges
           r-gtools
           r-iranges
           r-kernsmooth
           r-limma
           r-mclust
           r-mgcv
           r-qvalue
           r-r-utils
           r-rcpp
           r-rhtslib
           r-rsamtools
           r-rtracklayer
           r-s4vectors
           r-zlibbioc))
    (native-inputs
     (list r-knitr)) ; for vignettes
    (home-page "https://github.com/al2na/methylKit")
    (synopsis
     "DNA methylation analysis from high-throughput bisulfite sequencing results")
    (description
     "MethylKit is an R package for DNA methylation analysis and annotation
from high-throughput bisulfite sequencing.  The package is designed to deal
with sequencing data from @dfn{Reduced representation bisulfite
sequencing} (RRBS) and its variants, but also target-capture methods and whole
genome bisulfite sequencing.  It also has functions to analyze base-pair
resolution 5hmC data from experimental protocols such as oxBS-Seq and
TAB-Seq.")
    (license license:artistic2.0)))

(define-public r-mfuzz
  (package
    (name "r-mfuzz")
    (version "2.64.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "Mfuzz" version))
              (sha256
               (base32
                "00sx63bg5qw22vw0yw3syhyl5ffjrqvf81b9ksgw2p0z3yiiss5h"))))
    (properties `((upstream-name . "Mfuzz")))
    (build-system r-build-system)
    (propagated-inputs (list r-biobase r-e1071 r-tkwidgets))
    (home-page "http://mfuzz.sysbiolab.eu/")
    (synopsis "Soft clustering of time series gene expression data")
    (description
     "This is a package for noise-robust soft clustering of gene expression
time-series data (including a graphical user interface).")
    (license license:gpl2)))

(define-public r-mmuphin
  (package
    (name "r-mmuphin")
    (version "1.18.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "MMUPHin" version))
       (sha256
        (base32 "0wh998bjbf8zkfrzmrnm8dmm7960w5dnaq1yyc1hl105al0j33aj"))
       ;; Delete generated files.
       (snippet
        '(for-each delete-file
                   '("inst/doc/MMUPHin.R"
                     "inst/doc/MMUPHin.html")))))
    (properties `((upstream-name . "MMUPHin")))
    (build-system r-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-includes
            (lambda _
              (substitute* "inst/doc/MMUPHin.Rmd"
                (("\\.\\./man/figures")
                 (string-append (getcwd) "/man/figures"))
                (("bibliography: references.bib")
                 (string-append "bibliography: "
                                (getcwd) "/vignettes/references.bib")))))
          ;; Maaslin2 generates log files with timestamps.  We don't need to
          ;; keep them.  The generated PDF files also contain timestamps, so
          ;; we replace them with arbitrary fixed timestamps.
          (add-after 'check 'make-reproducible
            (lambda _
              (for-each delete-file
                        (find-files #$output "maaslin2.log"))
              (with-fluids ((%default-port-encoding "ISO-8859-1"))
                (substitute* (find-files #$output "\\.pdf$")
                  (("/CreationDate \\(D:.*\\)")
                   "/CreationDate (D:20230301143558)")
                  (("/ModDate \\(D:.*\\)")
                   "/ModDate (D:20230301143558)"))))))))
    ;; The DESCRIPTION file says that glpk is needed, but this package does
    ;; not seem to reference the library directly.
    (propagated-inputs
     (list r-cowplot
           r-dplyr
           r-fpc
           r-ggplot2
           r-igraph
           r-maaslin2
           r-metafor
           r-stringr
           r-tidyr))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/MMUPHin")
    (synopsis "Meta-analysis with uniform pipeline for heterogeneity in microbiome")
    (description
     "MMUPHin is an R package for meta-analysis tasks of microbiome cohorts.
It has function interfaces for:
@itemize
@item covariate-controlled batch- and cohort effect adjustment;
@item meta-analysis differential abundance testing;
@item meta-analysis unsupervised discrete structure (clustering) discovery;
@item meta-analysis unsupervised continuous structure discovery.
@end itemize")
    (license license:expat)))

(define-public r-modstrings
  (package
    (name "r-modstrings")
    (version "1.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Modstrings" version))
       (sha256
        (base32 "0yc8gsgxlww2g9z1m6jl02aqmama10fkr2mvibvdi01s5pqwdbky"))))
    (properties `((upstream-name . "Modstrings")))
    (build-system r-build-system)
    (propagated-inputs (list r-biocgenerics
                             r-biostrings
                             r-crayon
                             r-genomicranges
                             r-iranges
                             r-s4vectors
                             r-stringi
                             r-stringr
                             r-xvector))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/Modstrings")
    (synopsis "Working with modified nucleotide sequences")
    (description
     "Representing nucleotide modifications in a nucleotide sequence is
usually done via special characters from a number of sources.  This represents
a challenge to work with in R and the Biostrings package.  The Modstrings
package implements this functionallity for RNA and DNA sequences containing
modified nucleotides by translating the character internally in order to work
with the infrastructure of the Biostrings package.  For this the
@code{ModRNAString} and @code{ModDNAString} classes and derivates and
functions to construct and modify these objects despite the encoding issues
are implemenented.  In addition the conversion from sequences to list like
location information (and the reverse operation) is implemented as well.")
    (license license:artistic2.0)))

(define-public r-motifrg
  (package
    (name "r-motifrg")
    (version "1.31.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "motifRG" version))
       (sha256
        (base32
         "1ml6zyzlk8yjbnfhga2qnw8nl43rankvka0kc1yljxr2b66aqbhn"))))
    (properties `((upstream-name . "motifRG")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biostrings
           r-bsgenome
           r-bsgenome-hsapiens-ucsc-hg19
           r-iranges
           r-seqlogo
           r-xvector))
    (home-page "https://bioconductor.org/packages/motifRG")
    (synopsis "Discover motifs in high throughput sequencing data")
    (description
     "This package provides tools for discriminative motif discovery in high
throughput genetic sequencing data sets using regression methods.")
    (license license:artistic2.0)))

(define-public r-muscat
  (package
    (name "r-muscat")
    (version "1.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "muscat" version))
       (sha256
        (base32
         "1gafcq3p693f28gz3p8hk8mlxd9zja95h8953sr56n7krfffy1gi"))))
    (properties `((upstream-name . "muscat")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocparallel
           r-blme
           r-complexheatmap
           r-data-table
           r-deseq2
           r-dplyr
           r-edger
           r-ggplot2
           r-glmmtmb
           r-limma
           r-lme4
           r-lmertest
           r-matrix
           r-matrixstats
           r-progress
           r-purrr
           r-s4vectors
           r-scales
           r-scater
           r-sctransform
           r-scuttle
           r-singlecellexperiment
           r-summarizedexperiment
           r-variancepartition
           r-viridis))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/HelenaLC/muscat")
    (synopsis "Multi-sample multi-group scRNA-seq data analysis tools")
    (description
     "This package @code{muscat} provides various methods and visualization tools
for @dfn{DS}(differential splicing) analysis in multi-sample, multi-group,
multi-(cell-)subpopulation scRNA-seq data, including cell-level mixed models and
methods based on aggregated \"pseudobulk\" data, as well as a flexible simulation
platform that mimics both single and multi-sample scRNA-seq data.")
    (license license:gpl3)))

(define-public r-mutationalpatterns
  (package
    (name "r-mutationalpatterns")
    (version "3.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "MutationalPatterns" version))
       (sha256
        (base32
         "0smf4l023ckwsm5ckrqqw7kjyi1awp378mzhab7v8nk9n5smsj61"))))
    (build-system r-build-system)
    (native-inputs
     (list r-knitr))
    (propagated-inputs
     (list r-biocgenerics
           r-biostrings
           r-bsgenome
           ;; These two packages are suggested packages
           r-bsgenome-hsapiens-1000genomes-hs37d5
           r-bsgenome-hsapiens-ucsc-hg19
           r-cowplot
           r-dplyr
           r-genomeinfodb
           r-genomicranges
           r-ggalluvial
           r-ggdendro
           r-ggplot2
           r-iranges
           r-magrittr
           r-nmf
           r-pracma
           r-purrr
           r-rcolorbrewer
           r-s4vectors
           r-stringr
           r-tibble
           r-tidyr
           r-variantannotation))
    (home-page "https://bioconductor.org/packages/MutationalPatterns/")
    (synopsis "Extract and visualize mutational patterns in genomic data")
    (description "This package provides an extensive toolset for the
characterization and visualization of a wide range of mutational patterns
in SNV base substitution data.")
    (license license:expat)))

(define-public r-msa
  (package
    (name "r-msa")
    (version "1.36.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "msa" version))
              (sha256
               (base32
                "05i6pp10nn4icl51rx3b48s861hnd1yx72d933fphm6a5082zxms"))))
    (properties `((upstream-name . "msa")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocgenerics
           r-biostrings
           r-iranges
           r-rcpp
           r-s4vectors))
    (native-inputs (list r-knitr))
    (home-page "http://www.bioinf.jku.at/software/msa/")
    (synopsis "Multiple sequence alignment")
    (description
     "The msa package provides a unified R/Bioconductor interface to the
multiple sequence alignment algorithms ClustalW, ClustalOmega, and Muscle.
All three algorithms are integrated in the package, therefore, they do not
depend on any external software tools and are available for all major
platforms.  The multiple sequence alignment algorithms are complemented by a
function for pretty-printing multiple sequence alignments using the LaTeX
package TeXshade.")
    (license license:gpl2+)))

(define-public r-msnbase
  (package
    (name "r-msnbase")
    (version "2.30.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "MSnbase" version))
       (sha256
        (base32
         "0cdvi2mjvhj0h8x2rh3kkzrdnmzhr7d9a41m21r6b843a0q7hp6f"))))
    (properties `((upstream-name . "MSnbase")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-affy
           r-biobase
           r-biocgenerics
           r-biocparallel
           r-digest
           r-ggplot2
           r-impute
           r-iranges
           r-lattice
           r-maldiquant
           r-mass
           r-mscoreutils
           r-mzid
           r-mzr
           r-pcamethods
           r-plyr
           r-protgenerics
           r-psmatch
           r-rcpp
           r-s4vectors
           r-scales
           r-vsn))
    (native-inputs
     (list r-knitr))
    (home-page "https://github.com/lgatto/MSnbase")
    (synopsis "Base functions and classes for MS-based proteomics")
    (description
     "This package provides basic plotting, data manipulation and processing
of mass spectrometry based proteomics data.")
    (license license:artistic2.0)))

(define-public r-msnid
  (package
    (name "r-msnid")
    (version "1.38.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "MSnID" version))
       (sha256
        (base32
         "0misrm4x4z5h8dfwcrwi24nhcjslqvbyk5n7qz6kf2zcgcwq8q44"))))
    (properties `((upstream-name . "MSnID")))
    (build-system r-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'set-HOME
           (lambda _ (setenv "HOME" "/tmp"))))))
    (propagated-inputs
     (list r-annotationdbi
           r-annotationhub
           r-biobase
           r-biocgenerics
           r-biocstyle
           r-biostrings
           r-data-table
           r-doparallel
           r-dplyr
           r-foreach
           r-ggplot2
           r-iterators
           r-msmstests
           r-msnbase
           r-mzid
           r-mzr
           r-protgenerics
           r-purrr
           r-r-cache
           r-rcpp
           r-reshape2
           r-rlang
           r-runit
           r-stringr
           r-tibble
           r-xtable))
    (home-page "https://bioconductor.org/packages/MSnID")
    (synopsis "Utilities for LC-MSn proteomics identifications")
    (description
     "This package extracts @dfn{tandem mass spectrometry} (MS/MS) ID data
from mzIdentML (leveraging the mzID package) or text files.  After collating
the search results from multiple datasets it assesses their identification
quality and optimize filtering criteria to achieve the maximum number of
identifications while not exceeding a specified false discovery rate.  It also
contains a number of utilities to explore the MS/MS results and assess missed
and irregular enzymatic cleavages, mass measurement accuracy, etc.")
    (license license:artistic2.0)))

(define-public r-mzid
  (package
    (name "r-mzid")
    (version "1.42.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "mzID" version))
       (sha256
        (base32
         "0kh9swzh3hvi6qi64cgr13f22s1mgvz2rid5psb1gy4l3biwadj5"))))
    (properties `((upstream-name . "mzID")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-doparallel
           r-foreach
           r-iterators
           r-plyr
           r-protgenerics
           r-xml))
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/mzID")
    (synopsis "Parser for mzIdentML files")
    (description
     "This package provides a parser for mzIdentML files implemented using the
XML package.  The parser tries to be general and able to handle all types of
mzIdentML files with the drawback of having less pretty output than a vendor
specific parser.")
    (license license:gpl2+)))

(define-public r-mzr
  (package
    (name "r-mzr")
    (version "2.38.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "mzR" version))
       (sha256
        (base32
         "02dz23xns9ryz98nip1w68h9q5ckxpyl1zvzklsp4j7p9z2400dk"))
       (modules '((guix build utils)))
       (snippet
        '(delete-file-recursively "src/boost"))))
    (properties
     `((upstream-name . "mzR")
       (updater-extra-inputs . ("boost"))))
    (build-system r-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'use-system-boost
           (lambda _
             (substitute* "src/Makevars"
               (("\\./boost/libs.*") "")
               (("PKG_LIBS=") "PKG_LIBS=$(BOOST_LIBS) ")
               (("\\ARCH_OBJS=" line)
                (string-append line
                               "\nBOOST_LIBS=-lboost_system -lboost_regex \
-lboost_iostreams -lboost_thread -lboost_filesystem -lboost_chrono\n"))))))))
    (inputs
     (list boost ; use this instead of the bundled boost sources
           zlib))
    (propagated-inputs
     (list r-biobase
           r-biocgenerics
           r-ncdf4
           r-protgenerics
           r-rcpp
           r-rhdf5lib))
    (native-inputs
     (list r-knitr))
    (home-page "https://github.com/sneumann/mzR/")
    (synopsis "Parser for mass spectrometry data files")
    (description
     "The mzR package provides a unified API to the common file formats and
parsers available for mass spectrometry data.  It comes with a wrapper for the
ISB random access parser for mass spectrometry mzXML, mzData and mzML files.
The package contains the original code written by the ISB, and a subset of the
proteowizard library for mzML and mzIdentML.  The netCDF reading code has
previously been used in XCMS.")
    (license license:artistic2.0)))

;; This is a CRAN package, but it depends on a Bioconductor package.
(define-public r-numbat
  (package
    (name "r-numbat")
    (version "1.3.2-1")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "numbat" version))
              (sha256
               (base32
                "1b9bykgw3z7a8bky5yv2g402gdapv8kcla2kbbyqvs77x4wba4q4"))))
    (properties `((upstream-name . "numbat")))
    (build-system r-build-system)
    (propagated-inputs (list r-ape
                             r-catools
                             r-data-table
                             r-dendextend
                             r-dplyr
                             r-genomicranges
                             r-ggplot2
                             r-ggraph
                             r-ggtree
                             r-glue
                             r-igraph
                             r-iranges
                             r-logger
                             r-magrittr
                             r-matrix
                             r-optparse
                             r-paralleldist
                             r-patchwork
                             r-pryr
                             r-purrr
                             r-r-utils
                             r-rcpp
                             r-rcpparmadillo
                             r-rhpcblasctl
                             r-roptim
                             r-scales
                             r-scistreer
                             r-stringr
                             r-tibble
                             r-tidygraph
                             r-tidyr
                             r-vcfr
                             r-zoo))
    (home-page "https://github.com/kharchenkolab/numbat")
    (synopsis "Haplotype-aware CNV analysis from scRNA-Seq")
    (description
     "This package provides a computational method that infers copy number
variations (CNV) in cancer scRNA-seq data and reconstructs the tumor
phylogeny.  It integrates signals from gene expression, allelic ratio, and
population haplotype structures to accurately infer allele-specific CNVs in
single cells and reconstruct their lineage relationship.  It does not require
tumor/normal-paired DNA or genotype data, but operates solely on the donor
scRNA-data data (for example, 10x Cell Ranger output).  It can be used to:

@enumerate
@item detect allele-specific copy number variations from single-cells
@item differentiate tumor versus normal cells in the tumor microenvironment
@item infer the clonal architecture and evolutionary history of profiled tumors
@end enumerate

For details on the method see @url{https://doi.org/10.1038/s41587-022-01468-y,
Gao et al in Nature Biotechnology 2022}.")
    (license license:expat)))

(define-public r-organism-dplyr
  (package
    (name "r-organism-dplyr")
    (version "1.32.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Organism.dplyr" version))
       (sha256
        (base32
         "03z5pyqbaxwyrq5m858c8k69fwvclq98ksr84dc4m7r8lzz2hfxx"))))
    (properties `((upstream-name . "Organism.dplyr")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotationdbi
           r-annotationfilter
           r-biocfilecache
           r-dbi
           r-dbplyr
           r-dplyr
           r-genomeinfodb
           r-genomicfeatures
           r-genomicranges
           r-iranges
           r-rlang
           r-rsqlite
           r-s4vectors
           r-tibble))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/Organism.dplyr")
    (synopsis "Dplyr-based access to Bioconductor annotation resources")
    (description
     "This package provides an alternative interface to Bioconductor @code{
annotation} resources, in particular the gene identifier mapping functionality
of the @code{org} packages (e.g., @code{org.Hs.eg.db}) and the genome coordinate
functionality of the @code{TxDb} packages (e.g.,
@code{TxDb.Hsapiens.UCSC.hg38.knownGene}).")
    (license license:artistic2.0)))

(define-public r-organismdbi
  (package
    (name "r-organismdbi")
    (version "1.46.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "OrganismDbi" version))
       (sha256
        (base32
         "1cfh632rpmklmyd5cibwiy0i2pk2kk2m39g8mq5sxfwd2am4r0jk"))))
    (properties `((upstream-name . "OrganismDbi")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotationdbi
           r-biobase
           r-biocgenerics
           r-biocmanager
           r-dbi
           r-genomicfeatures
           r-genomicranges
           r-graph
           r-iranges
           r-rbgl
           r-s4vectors
           r-txdbmaker))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/OrganismDbi")
    (synopsis "Software to enable the smooth interfacing of database packages")
    (description "The package enables a simple unified interface to several
annotation packages each of which has its own schema by taking advantage of
the fact that each of these packages implements a select methods.")
    (license license:artistic2.0)))

(define-public r-oscope
  (package
    (name "r-oscope")
    (version "1.34.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "Oscope" version))
              (sha256
               (base32
                "16d2ngakdg73rdx0bf1pz3z1n8npjzl3b7dn94g9zbayw4vp3x5z"))))
    (properties `((upstream-name . "Oscope")))
    (build-system r-build-system)
    (propagated-inputs (list r-biocparallel r-cluster r-ebseq r-testthat))
    (home-page "https://bioconductor.org/packages/Oscope")
    (synopsis
     "Oscillatory genes identifier in unsynchronized single cell RNA-seq")
    (description
     "Oscope is a oscillatory genes identifier in unsynchronized single cell
RNA-seq.  This statistical pipeline has been developed to identify and recover
the base cycle profiles of oscillating genes in an unsynchronized single cell
RNA-seq experiment.  The Oscope pipeline includes three modules: a sine model
module to search for candidate oscillator pairs; a K-medoids clustering module
to cluster candidate oscillators into groups; and an extended nearest
insertion module to recover the base cycle order for each oscillator group.")
    (license license:asl2.0)))

(define-public r-pcaexplorer
  (package
    (name "r-pcaexplorer")
    (version "2.30.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "pcaExplorer" version))
       (sha256
        (base32
         "1szl2gyhw55ad4phcb7qn14amjb6xg65l3p4hnjzp6rda2cy9437"))))
    (properties `((upstream-name . "pcaExplorer")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotationdbi
           r-base64enc
           r-biomart
           r-deseq2
           r-dt
           r-genefilter
           r-genomicranges
           r-ggplot2
           r-ggrepel
           r-go-db
           r-gostats
           r-heatmaply
           r-iranges
           r-knitr
           r-limma
           r-nmf
           r-pheatmap
           r-plotly
           r-plyr
           r-rmarkdown
           r-s4vectors
           r-scales
           r-shiny
           r-shinyace
           r-shinybs
           r-shinydashboard
           r-summarizedexperiment
           r-threejs
           r-tidyr
           r-topgo))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/federicomarini/pcaExplorer")
    (synopsis
     "Interactive Visualization of RNA-seq Data Using a Principal Components Approach")
    (description
     "This package provides functionality for interactive visualization of RNA-seq
datasets based on Principal Components Analysis.  The methods provided allow for
quick information extraction and effective data exploration.  A Shiny
application encapsulates the whole analysis.")
    (license license:expat)))

(define-public r-pcamethods
  (package
    (name "r-pcamethods")
    (version "1.96.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "pcaMethods" version))
       (sha256
        (base32
         "1hrq0xmpr1l95hyi6ls9r86ahwf2rcbvr8m7cpg6ra0gsfyn4mpi"))))
    (properties `((upstream-name . "pcaMethods")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biobase r-biocgenerics r-mass r-rcpp))
    (home-page "https://github.com/hredestig/pcamethods")
    (synopsis "Collection of PCA methods")
    (description
     "This package provides Bayesian PCA, Probabilistic PCA, Nipals PCA,
Inverse Non-Linear PCA and the conventional SVD PCA.  A cluster based method
for missing value estimation is included for comparison.  BPCA, PPCA and
NipalsPCA may be used to perform PCA on incomplete data as well as for
accurate missing value estimation.  A set of methods for printing and plotting
the results is also provided.  All PCA methods make use of the same data
structure (pcaRes) to provide a common interface to the PCA results.")
    (license license:gpl3+)))

(define-public r-pfamanalyzer
  (package
    (name "r-pfamanalyzer")
    (version "1.4.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "pfamAnalyzeR" version))
              (sha256
               (base32
                "06bp5nghgrj8qyzqp1v869vck1j10093r9dp67w0kav4x4srkyyl"))))
    (properties `((upstream-name . "pfamAnalyzeR")))
    (build-system r-build-system)
    (propagated-inputs (list r-dplyr r-magrittr r-readr r-stringr r-tibble))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/pfamAnalyzeR")
    (synopsis "Identification of domain isotypes in pfam data")
    (description
     "This R package enables the user to read pfam predictions into R.  Most
human protein domains exist as multiple distinct variants termed domain
isotypes. This R package enables the identification and classification of such
domain isotypes from pfam data.")
    (license license:expat)))

(define-public r-piano
  (package
    (name "r-piano")
    (version "2.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "piano" version))
       (sha256
        (base32 "0v168wkwxzh70h2c08ad4dzlgklbgvx1fzwrzw2mqq0fyczj0diw"))))
    (properties `((upstream-name . "piano")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biobase
           r-biocgenerics
           r-dt
           r-fgsea
           r-gplots
           r-htmlwidgets
           r-igraph
           r-marray
           r-relations
           r-scales
           r-shiny
           r-shinydashboard
           r-shinyjs
           r-visnetwork))
    (native-inputs (list r-knitr))
    (home-page "https://varemo.github.io/piano/")
    (synopsis "Platform for integrative analysis of omics data")
    (description
     "Piano performs gene set analysis using various statistical methods, from
different gene level statistics and a wide range of gene-set collections.  The
package contains functions for combining the results of multiple runs of gene
set analyses.")
    (license license:gpl2+)))

(define-public r-polyester
  (package
    (name "r-polyester")
    (version "1.39.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "polyester" version))
       (sha256
        (base32 "0fw0qhmbygjq4ryspyqikb8rxfn33ks000j7sshgfg8hf2rmqg3x"))))
    (properties `((upstream-name . "polyester")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biostrings
           r-iranges
           r-limma
           r-logspline
           r-s4vectors
           r-zlibbioc))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/polyester")
    (synopsis "Simulate RNA-seq reads")
    (description
     "The polyester package simulates RNA-seq reads from differential expression
experiments with replicates.  The reads can then be aligned and used to perform
comparisons of methods for differential expression.")
    (license license:artistic2.0)))

(define-public r-powertcr
  (package
    (name "r-powertcr")
    (version "1.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "powerTCR" version))
       (sha256
        (base32 "04w6jhpc1vkqrm3dr5igp1iwkammmi53mqafjz694n379hamxrhg"))))
    (properties `((upstream-name . "powerTCR")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-cubature
           r-doparallel
           r-evmix
           r-foreach
           r-magrittr
           r-purrr
           r-truncdist
           r-vegan
           r-vgam))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/powerTCR")
    (synopsis "Model-based comparative analysis of the TCR repertoire")
    (description
     "This package provides a model for the clone size distribution of the
@acronym{TCR, T-cell receptor} repertoire.  Further, it permits comparative
analysis of TCR repertoire libraries based on theoretical model fits.")
    (license license:artistic2.0)))

;; This is a CRAN package, but it depends on a Bioconductor package:
;; r-aroma-light, r-dnacopy..
(define-public r-pscbs
  (package
    (name "r-pscbs")
    (version "0.67.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "PSCBS" version))
       (sha256
        (base32 "1gnd2nkh587dqpnbk5fsai7hx261vm405550rhlxffvs366x3596"))))
    (properties `((upstream-name . "PSCBS")))
    (build-system r-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'install 'change-home-dir
           (lambda _
             ;; Change from /homeless-shelter to /tmp for write permission.
             (setenv "HOME" "/tmp"))))))
    (propagated-inputs
     (list r-aroma-light
           r-dnacopy
           r-future
           r-listenv
           r-matrixstats
           r-r-cache
           r-r-methodss3
           r-r-oo
           r-r-utils))
    (native-inputs
     (list r-r-rsp                      ;used to build vignettes
           r-r-devices))
    (home-page "https://github.com/HenrikBengtsson/PSCBS")
    (synopsis "Analysis of parent-specific DNA copy numbers")
    (description
     "This is a package for segmentation of allele-specific DNA copy number
data and detection of regions with abnormal copy number within each parental
chromosome.  Both tumor-normal paired and tumor-only analyses are supported.")
    (license license:gpl2+)))

(define-public r-psmatch
  (package
    (name "r-psmatch")
    (version "1.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "PSMatch" version))
       (sha256
        (base32 "13w145is3d95392qqdq6qh3mrlcixbj0jzz4r590xas1qjp5d03h"))))
    (properties `((upstream-name . "PSMatch")))
    (build-system r-build-system)
    (propagated-inputs (list r-biocgenerics
                             r-biocparallel
                             r-igraph
                             r-matrix
                             r-mscoreutils
                             r-protgenerics
                             r-qfeatures
                             r-s4vectors))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/RforMassSpectrometry/PSM")
    (synopsis "Handling and managing peptide spectrum matches")
    (description
     "The PSMatch package helps proteomics practitioners to load, handle and
manage peptide spectrum matches.  It provides functions to model
peptide-protein relations as adjacency matrices and connected components,
visualise these as graphs and make informed decision about shared peptide
filtering.  The package also provides functions to calculate and visualise MS2
fragment ions.")
    (license license:artistic2.0)))

(define-public r-protgear
  (package
    (name "r-protgear")
    (version "1.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "protGear" version))
       (sha256
        (base32 "1wbdp7f55lbn0gcxgnp9xk0y5npn7jmk9vp7yq9v76j7f396vj2f"))))
    (properties `((upstream-name . "protGear")))
    (build-system r-build-system)
    (arguments
     (list
      #:phases
      '(modify-phases %standard-phases
         ;; Needed by styler for writing to caches.
         (add-after 'unpack 'set-HOME
           (lambda _ (setenv "HOME" "/tmp"))))))
    (propagated-inputs (list r-biobase
                             r-data-table
                             r-dplyr
                             r-factoextra
                             r-factominer
                             r-flexdashboard
                             r-genefilter
                             r-ggally
                             r-ggplot2
                             r-ggpubr
                             r-gtools
                             r-htmltools
                             r-kendall
                             r-knitr
                             r-limma
                             r-magrittr
                             r-mass
                             r-pheatmap
                             r-plotly
                             r-plyr
                             r-purrr
                             r-readr
                             r-remotes
                             r-rlang
                             r-rmarkdown
                             r-shiny
                             r-shinydashboard
                             r-styler
                             r-tibble
                             r-tidyr
                             r-vsn))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/Keniajin/protGear")
    (synopsis
     "Protein micro array data management and interactive visualization")
    (description
     "This package provides a generic three-step pre-processing package for
protein microarray data.  This package contains different data pre-processing
procedures to allow comparison of their performance.  These steps are
background correction, the coefficient of variation (CV) based filtering,
batch correction and normalization.")
    (license license:gpl3)))

(define-public r-protgenerics
  (package
    (name "r-protgenerics")
    (version "1.36.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ProtGenerics" version))
       (sha256
        (base32
         "0h0i4dgsgvyhma5pfvycv6wyxv27vp0b2i8b6l8hj55z8cvmvfln"))))
    (properties `((upstream-name . "ProtGenerics")))
    (build-system r-build-system)
    (home-page "https://github.com/lgatto/ProtGenerics")
    (synopsis "S4 generic functions for proteomics infrastructure")
    (description
     "This package provides S4 generic functions needed by Bioconductor
proteomics packages.")
    (license license:artistic2.0)))

(define-public r-pwalign
  (package
    (name "r-pwalign")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "pwalign" version))
       (sha256
        (base32 "1m0j1m5jif5spd9fkpzz3z3c9s8vwiy8xvpx8rz8igxj89rwrwb7"))))
    (properties `((upstream-name . "pwalign")))
    (build-system r-build-system)
    (propagated-inputs (list r-biocgenerics r-biostrings r-iranges r-s4vectors
                             r-xvector))
    (home-page "https://bioconductor.org/packages/pwalign")
    (synopsis "Perform pairwise sequence alignments")
    (description
     "The two main functions in the package are @code{pairwiseAlignment} and
@code{stringDist}.  The former solves (Needleman-Wunsch) global alignment,
(Smith-Waterman) local alignment, and (ends-free) overlap alignment problems.
The latter computes the Levenshtein edit distance or pairwise alignment score
matrix for a set of strings.")
    (license license:artistic2.0)))

(define-public r-rbgl
  (package
    (name "r-rbgl")
    (version "1.80.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "RBGL" version))
       (sha256
        (base32
         "1n6rfrcvlgi6mzz36v9sniqr05xfjzvfdpv4xwsn1xq41326wdv3"))))
    (properties `((upstream-name . "RBGL")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-bh r-graph))
    (native-inputs (list r-knitr))
    (home-page "https://www.bioconductor.org/packages/RBGL")
    (synopsis "Interface to the Boost graph library")
    (description
     "This package provides a fairly extensive and comprehensive interface to
the graph algorithms contained in the Boost library.")
    (license license:artistic2.0)))

(define-public r-rcas
  (package
    (name "r-rcas")
    (version "1.30.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "RCAS" version))
              (sha256
               (base32
                "0ll0sj2zyazw0nxbpybq9frmxvmcjkgk3hwp3flnysjxg056dhxg"))))
    (properties `((upstream-name . "RCAS")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocgenerics
           r-biostrings
           r-bsgenome
           r-bsgenome-hsapiens-ucsc-hg19
           r-cowplot
           r-data-table
           r-dt
           r-genomation
           r-genomeinfodb
           r-genomicfeatures
           r-genomicranges
           r-ggplot2
           r-gprofiler2
           r-iranges
           r-knitr
           r-pbapply
           r-pheatmap
           r-plotly
           r-plotrix
           r-proxy
           r-ranger
           r-rmarkdown
           r-rsqlite
           r-rtracklayer
           r-s4vectors
           r-seqlogo
           r-txdbmaker))
    (native-inputs
     (list r-knitr))
    (synopsis "RNA-centric annotation system")
    (description
     "RCAS aims to be a standalone RNA-centric annotation system that provides
intuitive reports and publication-ready graphics.  This package provides the R
library implementing most of the pipeline's features.")
    (home-page "https://github.com/BIMSBbioinfo/RCAS")
    (license license:artistic2.0)))

(define-public r-rcy3
  (package
    (name "r-rcy3")
    (version "2.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "RCy3" version))
       (sha256
        (base32 "03zmskcb8n87x6m5jlhsh2b0b2qjk91y16fdcj3kz5c1j7zfs18i"))))
    (properties `((upstream-name . "RCy3")))
    (build-system r-build-system)
    (propagated-inputs (list r-base64enc
                             r-base64url
                             r-biocgenerics
                             r-fs
                             r-glue
                             r-gplots
                             r-graph
                             r-httr
                             r-irdisplay
                             r-irkernel
                             r-rcolorbrewer
                             r-rcurl
                             r-rjsonio
                             r-stringi
                             r-uuid
                             r-xml))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/cytoscape/RCy3")
    (synopsis "Functions to access and control Cytoscape")
    (description
     "Vizualize, analyze and explore networks using Cytoscape via R.  Anything
you can do using the graphical user interface of Cytoscape, you can now do
with a single RCy3 function.")
    (license license:expat)))

(define-public r-regioner
  (package
    (name "r-regioner")
    (version "1.36.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "regioneR" version))
       (sha256
        (base32
         "1ygdlqvhf165mqd247783pi1yzidqap8b913444vrs881n62xvrb"))))
    (properties `((upstream-name . "regioneR")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biostrings
           r-bsgenome
           r-genomeinfodb
           r-genomicranges
           r-iranges
           r-memoise
           r-rtracklayer
           r-s4vectors))
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/regioneR/")
    (synopsis "Association analysis of genomic regions")
    (description "This package offers a statistical framework based on
customizable permutation tests to assess the association between genomic
region sets and other genomic features.")
    (license license:artistic2.0)))

(define-public r-reportingtools
  (package
    (name "r-reportingtools")
    (version "2.44.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ReportingTools" version))
       (sha256
        (base32
         "0vq46dj2v3imjmf6cw8mab6k9v4rrc5r2vh100jjlsm3z15gvl17"))
       (snippet
        '(delete-file "inst/extdata/jslib/jquery-1.8.0.min.js"))))
    (properties
     `((upstream-name . "ReportingTools")))
    (build-system r-build-system)
    (arguments
     (list
      #:modules '((guix build utils)
                  (guix build r-build-system)
                  (guix build minify-build-system)
                  (ice-9 match))
      #:imported-modules
      `(,@%r-build-system-modules
        (guix build minify-build-system))
      #:phases
      '(modify-phases (@ (guix build r-build-system) %standard-phases)
         (add-after 'unpack 'process-javascript
           (lambda* (#:key inputs #:allow-other-keys)
             (for-each (match-lambda
                         ((source . target)
                          (minify source #:target target)))
                       `((,(assoc-ref inputs "_")
                          . "inst/extdata/jslib/jquery-1.8.0.min.js"))))))))
    (propagated-inputs
     (list r-annotate
           r-annotationdbi
           r-biobase
           r-biocgenerics
           r-category
           r-deseq2
           r-edger
           r-ggbio
           r-ggplot2
           r-gostats
           r-gseabase
           r-hwriter
           r-iranges
           r-knitr
           r-lattice
           r-limma
           r-pfam-db
           r-r-utils
           r-xml))
    (native-inputs
     (list esbuild r-rmarkdown
           (origin
             (method url-fetch)
             (uri "https://code.jquery.com/jquery-1.8.0.js")
             (sha256
              (base32
               "02vnwfxrrfsqm6qbmxyv9rdg32qyzs81d1snk62fy08gv7r62hfk")))))
    (home-page "https://bioconductor.org/packages/ReportingTools/")
    (synopsis "Tools for making reports in various formats")
    (description
     "The ReportingTools package enables users to easily display reports of
analysis results generated from sources such as microarray and sequencing
data.  The package allows users to create HTML pages that may be viewed on a
web browser, or in other formats.  Users can generate tables with sortable and
filterable columns, make and display plots, and link table entries to other
data sources such as NCBI or larger plots within the HTML page.  Using the
package, users can also produce a table of contents page to link various
reports together for a particular project that can be viewed in a web
browser.")
    (license license:artistic2.0)))

(define-public r-rhdf5
  (package
    (name "r-rhdf5")
    (version "2.48.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "rhdf5" version))
              (sha256
               (base32
                "086bnwzmlwq3z9a4kb1rnzb2wnchhi4gvzpijhladgd83nqyb0wn"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-rhdf5filters r-rhdf5lib))
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/rhdf5")
    (synopsis "HDF5 interface to R")
    (description
     "This R/Bioconductor package provides an interface between HDF5 and R.
HDF5's main features are the ability to store and access very large and/or
complex datasets and a wide variety of metadata on mass storage (disk) through
a completely portable file format.  The rhdf5 package is thus suited for the
exchange of large and/or complex datasets between R and other software
package, and for letting R applications work on datasets that are larger than
the available RAM.")
    (license license:artistic2.0)))

(define-public r-rhdf5filters
  (package
    (name "r-rhdf5filters")
    (version "1.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "rhdf5filters" version))
       (sha256
        (base32
         "0fsfp34pfb5k030j7nfx9b9vsil02y85sv1dd1y0bfdxchi4bw8x"))))
    (properties `((upstream-name . "rhdf5filters")))
    (build-system r-build-system)
    (arguments
     (list
      #:configure-flags
      '(list "--without-bundled-libs")))
    (propagated-inputs
     (list r-rhdf5lib))
    (inputs
     (list bzip2 c-blosc zlib (list zstd "lib")))
    (native-inputs
     (list r-knitr))
    (home-page "https://github.com/grimbough/rhdf5filters")
    (synopsis "HDF5 compression filters")
    (description
     "This package provides a collection of compression filters for use with
HDF5 datasets.")
    (license license:bsd-2)))

(define-public r-rsamtools
  (package
    (name "r-rsamtools")
    (version "2.20.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "Rsamtools" version))
              (sha256
               (base32
                "04lz0dzwmgi8j9wyspbmjln6yb5fv4f538nqyrfd31d0g16937b8"))))
    (properties
     `((upstream-name . "Rsamtools")))
    (build-system r-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'use-system-zlib
           (lambda _
             (substitute* "DESCRIPTION"
               (("zlibbioc, ") ""))
             (substitute* "NAMESPACE"
               (("import\\(zlibbioc\\)") "")))))))
    (propagated-inputs
     (list r-biocgenerics
           r-biocparallel
           r-biostrings
           r-bitops
           r-genomeinfodb
           r-genomicranges
           r-iranges
           r-rhtslib
           r-s4vectors
           r-xvector
           r-zlibbioc))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/release/bioc/html/Rsamtools.html")
    (synopsis "Interface to samtools, bcftools, and tabix")
    (description
     "This package provides an interface to the @code{samtools},
@code{bcftools}, and @code{tabix} utilities for manipulating SAM (Sequence
Alignment / Map), FASTA, binary variant call (BCF) and compressed indexed
tab-delimited (tabix) files.")
    (license license:expat)))

;; This is a CRAN package, but it depends on a Bioconductor package:
;; s4vectors.
(define-public r-restfulr
  (package
    (name "r-restfulr")
    (version "0.0.15")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "restfulr" version))
       (sha256
        (base32
         "14p6h0gjknqy5z2fprxw7waf4p0cd2qmp18s7qig4ylqn8gqzzs0"))))
    (properties `((upstream-name . "restfulr")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-rcurl r-rjson r-s4vectors r-xml r-yaml))
    (home-page "https://cran.r-project.org/package=restfulr")
    (synopsis "R interface to RESTful web services")
    (description
     "This package models a RESTful service as if it were a nested R list.")
    (license license:artistic2.0)))

(define-public r-rtcga
  (package
    (name "r-rtcga")
    (version "1.34.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "RTCGA" version))
              (sha256
               (base32
                "1a967id4anjzpdlbdd8a85fvb1z7mcr759pr91aashv12bzzhnk0"))))
    (properties `((upstream-name . "RTCGA")))
    (build-system r-build-system)
    (propagated-inputs (list r-assertthat
                             r-data-table
                             r-dplyr
                             r-ggplot2
                             r-ggthemes
                             r-htmltools
                             r-knitr
                             r-purrr
                             r-rcurl
                             r-rmarkdown
                             r-rvest
                             r-scales
                             r-stringi
                             r-survival
                             r-survminer
                             r-viridis
                             r-xml
                             r-xml2))
    (native-inputs (list r-knitr))
    (home-page "https://rtcga.github.io/RTCGA/")
    (synopsis "The Cancer Genome Atlas data integration")
    (description
     "The Cancer Genome Atlas (TCGA) Data Portal provides a platform for
researchers to search, download, and analyze data sets generated by TCGA.  It
contains clinical information, genomic characterization data, and high level
sequence analysis of the tumor genomes.  The key is to understand genomics to
improve cancer care.  RTCGA package offers download and integration of the
variety and volume of TCGA data using patient barcode key, what enables easier
data possession.  This may have an benefcial infuence on impact on development
of science and improvement of patients treatment.  Furthermore, RTCGA package
transforms TCGA data to tidy form which is convenient to use.")
    (license license:gpl2)))

(define-public r-rtcgatoolbox
  (package
    (name "r-rtcgatoolbox")
    (version "2.34.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "RTCGAToolbox" version))
              (sha256
               (base32
                "0l4idk6gsrnm7laj1xkm6jwmyy5l90kphm21zk7y33cpnp2ryjwr"))))
    (properties `((upstream-name . "RTCGAToolbox")))
    (build-system r-build-system)
    (propagated-inputs (list r-biocgenerics
                             r-data-table
                             r-delayedarray
                             r-genomeinfodb
                             r-genomicranges
                             r-httr
                             r-raggedexperiment
                             r-rcurl
                             r-rjsonio
                             r-rvest
                             r-s4vectors
                             r-stringr
                             r-summarizedexperiment
                             r-tcgautils))
    (native-inputs (list r-knitr))
    (home-page "http://mksamur.github.io/RTCGAToolbox/")
    (synopsis "Export TCGA Firehose data")
    (description
     "Managing data from large scale projects such as The Cancer Genome
Atlas (TCGA) for further analysis is an important and time consuming step for
research projects.  Several efforts, such as Firehose project, make TCGA
pre-processed data publicly available via web services and data portals but it
requires managing, downloading and preparing the data for following steps.
This package provides an extensible R based data client for Firehose
pre-processed data.")
    (license license:gpl2)))

(define-public r-rtracklayer
  (package
    (name "r-rtracklayer")
    (version "1.64.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "rtracklayer" version))
              (sha256
               (base32
                "0h2cqr1jqqqxvkmqpngfl8ppm1p65vvlb1wgfwl9r4f99b8vz33g"))))
    (build-system r-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'use-system-zlib
           (lambda _
             (substitute* "DESCRIPTION"
               ((" zlibbioc,") ""))
             (substitute* "NAMESPACE"
               (("import\\(zlibbioc\\)") "")))))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list curl openssl zlib))
    (propagated-inputs
     (list r-biocgenerics
           r-biocio
           r-biostrings
           r-curl
           r-genomeinfodb
           r-genomicalignments
           r-genomicranges
           r-httr
           r-iranges
           r-restfulr
           r-rsamtools
           r-s4vectors
           r-xml
           r-xvector
           r-zlibbioc))
    (home-page "https://bioconductor.org/packages/rtracklayer")
    (synopsis "R interface to genome browsers and their annotation tracks")
    (description
     "rtracklayer is an extensible framework for interacting with multiple
genome browsers (currently UCSC built-in) and manipulating annotation tracks
in various formats (currently GFF, BED, bedGraph, BED15, WIG, BigWig and 2bit
built-in).  The user may export/import tracks to/from the supported browsers,
as well as query and modify the browser state, such as the current viewport.")
    (license license:artistic2.0)))

;; This is a CRAN package, but it depends on a Bioconductor package.
(define-public r-samr
  (package
    (name "r-samr")
    (version "3.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "samr" version))
       (sha256
        (base32
         "01km0f7qgm73x19vbvsxl083hs1dq4dj8qm5h64cxbf20b08my15"))))
    (properties `((upstream-name . "samr")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-gsa
           r-impute
           r-matrixstats
           r-openxlsx
           r-shiny
           r-shinyfiles))
    (native-inputs (list gfortran))
    (home-page "https://statweb.stanford.edu/~tibs/SAM/")
    (synopsis "Significance analysis of Microarrays")
    (description
     "This is a package for significance analysis of Microarrays for
differential expression analysis, RNAseq data and related problems.")
    ;; Any version of the LGPL
    (license license:lgpl3+)))

(define-public r-saturn
  (package
    (name "r-saturn")
    (version "1.12.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "satuRn" version))
              (sha256
               (base32
                "03mlb4j219w8760zm5hnryjwxns3qz9y3wl6zaqdfnl94bwyl9r8"))))
    (properties `((upstream-name . "satuRn")))
    (build-system r-build-system)
    (propagated-inputs (list r-biocparallel
                             r-boot
                             r-ggplot2
                             r-limma
                             r-locfdr
                             r-matrix
                             r-pbapply
                             r-summarizedexperiment))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/statOmics/satuRn")
    (synopsis
     "Analysis of differential transcript usage for scRNA-seq applications")
    (description
     "satuRn provides a framework for performing differential transcript usage
analyses.  The package consists of three main functions.  The first function,
@code{fitDTU}, fits quasi-binomial generalized linear models that model
transcript usage in different groups of interest.  The second function,
@code{testDTU}, tests for differential usage of transcripts between groups of
interest.  Finally, @code{plotDTU} visualizes the usage profiles of
transcripts in groups of interest.")
    (license license:artistic2.0)))

(define-public r-scannotatr
  (package
    (name "r-scannotatr")
    (version "1.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "scAnnotatR" version))
       (sha256
        (base32 "1gzssp7im1d9pp2mbar27k854fcc3rqyza5d77flmhshq88xrskv"))))
    (properties `((upstream-name . "scAnnotatR")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotationhub
           r-ape
           r-caret
           r-data-tree
           r-dplyr
           r-e1071
           r-ggplot2
           r-kernlab
           r-proc
           r-rocr
           r-seurat
           r-singlecellexperiment
           r-summarizedexperiment))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/grisslab/scAnnotatR")
    (synopsis "Pretrained models for prediction on single cell RNA-sequencing data")
    (description
     "This package comprises a set of pretrained machine learning models to
predict basic immune cell types.  This enables to quickly get a first
annotation of the cell types present in the dataset without requiring prior
knowledge.  The package also lets you train using own models to predict new
cell types based on specific research needs.")
    (license license:expat)))

(define-public r-scdblfinder
  (package
    (name "r-scdblfinder")
    (version "1.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "scDblFinder" version))
       (sha256
        (base32 "1np3arhjxxgif68i0wi0nqqbrwavy14dh9vy2g9mw4408k7789w0"))))
    (properties `((upstream-name . "scDblFinder")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocgenerics
           r-biocneighbors
           r-biocparallel
           r-biocsingular
           r-bluster
           r-delayedarray
           r-genomeinfodb
           r-genomicranges
           r-igraph
           r-iranges
           r-mass
           r-matrix
           r-rsamtools
           r-rtracklayer
           r-s4vectors
           r-scater
           r-scran
           r-scuttle
           r-singlecellexperiment
           r-summarizedexperiment
           r-xgboost))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/plger/scDblFinder")
    (synopsis "Detect multiplets in single-cell RNA sequencing data")
    (description
     "The scDblFinder package gathers various methods for the detection and
handling of doublets/multiplets in single-cell RNA sequencing data (i.e.
multiple cells captured within the same droplet or reaction volume).  It
includes methods formerly found in the scran package, and the new fast and
comprehensive scDblFinder method.")
    (license license:gpl3)))

;; This is a CRAN package, but it depends on packages from Bioconductor.
(define-public r-scgate
  (package
    (name "r-scgate")
    (version "1.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "scGate" version))
       (sha256
        (base32 "0zlbrvr6lxcihspfqgpvv5kyrzqqpy3451j0kpnqc0n93bbs4cqk"))))
    (properties `((upstream-name . "scGate")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocparallel
           r-dplyr
           r-ggplot2
           r-ggridges
           r-patchwork
           r-reshape2
           r-seurat
           r-ucell))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/carmonalab/scGate")
    (synopsis
     "Marker-based cell type purification for single-cell sequencing data")
    (description
     "This package provides a method to purify a cell type or cell population
of interest from heterogeneous datasets.  scGate package automatizes
marker-based purification of specific cell populations, without requiring
training data or reference gene expression profiles.  scGate takes as input a
gene expression matrix stored in a Seurat object and a @acronym{GM, gating
model}, consisting of a set of marker genes that define the cell population of
interest.  It evaluates the strength of signature marker expression in each
cell using the rank-based method UCell, and then performs @acronym{kNN,
k-nearest neighbor} smoothing by calculating the mean UCell score across
neighboring cells.  kNN-smoothing aims at compensating for the large degree of
sparsity in scRNAseq data.  Finally, a universal threshold over kNN-smoothed
signature scores is applied in binary decision trees generated from the
user-provided gating model, to annotate cells as either “pure” or “impure”,
with respect to the cell population of interest.")
    (license license:gpl3)))

;; This is a CRAN package, but it depends on packages from Bioconductor.
(define-public r-scistreer
  (package
    (name "r-scistreer")
    (version "1.2.0")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "scistreer" version))
              (sha256
               (base32
                "03nd4p7ik66v09yv18c0z1bvdnkr5m0axk78yapd2ri80ihmyi3c"))))
    (properties `((upstream-name . "scistreer")))
    (build-system r-build-system)
    (propagated-inputs (list r-ape
                             r-dplyr
                             r-ggplot2
                             r-ggtree
                             r-igraph
                             r-paralleldist
                             r-patchwork
                             r-phangorn
                             r-rcpp
                             r-rcpparmadillo
                             r-rcppparallel
                             r-reshape2
                             r-rhpcblasctl
                             r-stringr
                             r-tidygraph))
    (home-page "https://github.com/kharchenkolab/scistreer")
    (synopsis "Maximum-likelihood perfect phylogeny Inference at scale")
    (description
     "This package provides fast maximum-likelihood phylogeny inference from
noisy single-cell data using the ScisTree algorithm proposed by
@code{doi.org/10.1093/bioinformatics/btz676, Yufeng Wu (2019)}.  It makes the
method applicable to massive single-cell datasets (>10,000 cells).")
    (license license:gpl3)))

(define-public r-scmap
  (package
    (name "r-scmap")
    (version "1.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "scmap" version))
       (sha256
        (base32 "0acy3sg4j36znsfxpkb190wbawbngx15cr5x2lhgrjivvw3637x1"))))
    (properties `((upstream-name . "scmap")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biobase
           r-biocgenerics
           r-dplyr
           r-e1071
           r-ggplot2
           r-googlevis
           r-matrixstats
           r-proxy
           r-randomforest
           r-rcpp
           r-rcpparmadillo
           r-reshape2
           r-s4vectors
           r-singlecellexperiment
           r-summarizedexperiment))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/hemberg-lab/scmap")
    (synopsis "Tool for unsupervised projection of single cell RNA-seq data")
    (description
     "@dfn{Single-cell RNA-seq} (scRNA-seq) is widely used to investigate the
composition of complex tissues since the technology allows researchers to
define cell-types using unsupervised clustering of the transcriptome.
However, due to differences in experimental methods and computational
analyses, it is often challenging to directly compare the cells identified in
two different experiments.  @code{scmap} is a method for projecting cells from
a scRNA-seq experiment onto the cell-types or individual cells identified in a
different experiment.")
    (license license:gpl3)))

(define-public r-screpertoire
  (package
    (name "r-screpertoire")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "scRepertoire" version))
       (sha256
        (base32 "1ifnyfzswisacvxxf041l1snfv1b8x30pjg2pwampg804c56f1wj"))))
    (properties `((upstream-name . "scRepertoire")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-cubature
           r-dplyr
           r-evmix
           r-ggalluvial
           r-ggdendro
           r-ggplot2
           r-ggraph
           r-hash
           r-igraph
           r-inext
           r-plyr
           r-quantreg
           r-rcpp
           r-reshape2
           r-rjson
           r-rlang
           r-s4vectors
           r-seuratobject
           r-singlecellexperiment
           r-stringdist
           r-stringr
           r-summarizedexperiment
           r-tidygraph
           r-truncdist
           r-vgam))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/scRepertoire")
    (synopsis "Toolkit for single-cell immune receptor profiling")
    (description
     "The scRepertoire package was built to process data derived from the 10x
Genomics Chromium Immune Profiling for both @acronym{TCR, T-cell receptor} and
@acronym{Ig, immunoglobulin} enrichment workflows and subsequently interacts with
the popular Seurat and SingleCellExperiment R packages.  It also allows for
general analysis of single-cell clonotype information without the use of
expression information.  The package functions as a wrapper for Startrac and
powerTCR R packages.")
    (license license:gpl2)))

(define-public r-scrnaseq
  (package
    (name "r-scrnaseq")
    (version "2.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "scRNAseq" version
                              'experiment))
       (sha256
        (base32 "0jsq037ba4xsb3bwwf021jw2hqv30vb5nrjag97xadx75z5fzqlr"))))
    (properties `((upstream-name . "scRNAseq")))
    (build-system r-build-system)
    (propagated-inputs (list r-alabaster-base
                             r-alabaster-matrix
                             r-alabaster-sce
                             r-annotationdbi
                             r-annotationhub
                             r-biocgenerics
                             r-dbi
                             r-delayedarray
                             r-ensembldb
                             r-experimenthub
                             r-genomicfeatures
                             r-genomicranges
                             r-gypsum
                             r-jsonlite
                             r-matrix
                             r-rsqlite
                             r-s4vectors
                             r-singlecellexperiment
                             r-sparsearray
                             r-summarizedexperiment))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/scRNAseq")
    (synopsis "Collection of public single-cell RNA-seq datasets")
    (description
     "This package contains gene-level counts for a collection of public
@code{scRNA-seq} datasets, provided as @code{SingleCellExperiment} objects
with cell- and gene-level metadata.")
    (license license:cc0)))

(define-public r-scry
  (package
    (name "r-scry")
    (version "1.16.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "scry" version))
              (sha256
               (base32
                "16489ff1nc798nfcmqvqaczm7hqffkhpad1dfgr7dxlahd8sbnw9"))))
    (properties `((upstream-name . "scry")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocsingular
           r-delayedarray
           r-glmpca
           r-matrix
           r-singlecellexperiment
           r-summarizedexperiment))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/scry.html")
    (synopsis "Small-count analysis methods for high-dimensional data")
    (description
     "Many modern biological datasets consist of small counts that are not
well fit by standard linear-Gaussian methods such as principal component
analysis.  This package provides implementations of count-based feature
selection and dimension reduction algorithms.  These methods can be used to
facilitate unsupervised analysis of any high-dimensional data such as
single-cell RNA-seq.")
    (license license:artistic2.0)))

(define-public r-seqarray
  (package
    (name "r-seqarray")
    (version "1.44.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "SeqArray" version))
              (sha256
               (base32
                "1g8867y66f8ihr0w2shc17wxnavnkwn9y25f850nwvnn9nsbk88k"))))
    (properties `((upstream-name . "SeqArray")))
    (build-system r-build-system)
    (propagated-inputs (list r-biostrings
                             r-gdsfmt
                             r-genomeinfodb
                             r-genomicranges
                             r-iranges
                             r-s4vectors))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/zhengxwen/SeqArray")
    (synopsis
     "Data management of large-scale whole-genome sequence variant calls")
    (description
     "This package supports data management of large-scale whole-genome
sequencing variant calls with thousands of individuals: genotypic data (e.g.,
SNVs, indels and structural variation calls) and annotations in SeqArray GDS
files are stored in an array-oriented and compressed manner, with efficient
data access using the R programming language.")
    (license license:gpl3)))

(define-public r-seqlogo
  (package
    (name "r-seqlogo")
    (version "1.70.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "seqLogo" version))
       (sha256
        (base32
         "154c3g4i6snsi8vkwjw4d8lyr3372d6vyw237nslw1y5n837yzir"))))
    (properties `((upstream-name . "seqLogo")))
    (build-system r-build-system)
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/seqLogo")
    (synopsis "Sequence logos for DNA sequence alignments")
    (description
     "seqLogo takes the position weight matrix of a DNA sequence motif and
plots the corresponding sequence logo as introduced by Schneider and
Stephens (1990).")
    (license license:lgpl2.0+)))

(define-public r-seqpattern
  (package
    (name "r-seqpattern")
    (version "1.36.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "seqPattern" version))
              (sha256
               (base32
                "0iv0dhs1z8i0jglh1iw6ypzh6qslnn5xly2kjfxaxk9cggnjb5h6"))))
    (properties
     `((upstream-name . "seqPattern")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biostrings r-genomicranges r-iranges r-kernsmooth r-plotrix))
    (home-page "https://bioconductor.org/packages/seqPattern")
    (synopsis "Visualising oligonucleotide patterns and motif occurrences")
    (description
     "This package provides tools to visualize oligonucleotide patterns and
sequence motif occurrences across a large set of sequences centred at a common
reference point and sorted by a user defined feature.")
    (license license:gpl3+)))

(define-public r-sesame
  (package
    (name "r-sesame")
    (version "1.22.2")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "sesame" version))
              (sha256
               (base32
                "1d2vjglpbhmr4fjp3zwipf0nwj5pms4l9q1xngyxrsbr26x7jp1d"))))
    (properties `((upstream-name . "sesame")))
    (build-system r-build-system)
    (propagated-inputs (list r-biocfilecache
                             r-biocparallel
                             r-dplyr
                             r-genomeinfodb
                             r-genomicranges
                             r-ggplot2
                             r-iranges
                             r-mass
                             r-preprocesscore
                             r-readr
                             r-reshape2
                             r-s4vectors
                             r-sesamedata
                             r-stringr
                             r-summarizedexperiment
                             r-tibble
                             r-wheatmap))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/zwdzwd/sesame")
    (synopsis "Step-wise analysis of DNA Methylation BeadChips")
    (description
     "This package provides tools For analyzing Illumina Infinium DNA
methylation arrays.  @code{SeSAMe} provides utilities to support analyses of
multiple generations of Infinium DNA methylation @code{BeadChips}, including
preprocessing, quality control, visualization and inference.  @code{SeSAMe}
features accurate detection calling, intelligent inference of ethnicity, sex
and advanced quality control routines.")
    (license license:expat)))

(define-public r-shinymethyl
  (package
    (name "r-shinymethyl")
    (version "1.40.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "shinyMethyl" version))
              (sha256
               (base32
                "15r9x1fqladjviyisbhr8zff0a6g9kvbvx72xcp3bfadp3wpygds"))))
    (properties `((upstream-name . "shinyMethyl")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biobase
           r-biocgenerics
           r-htmltools
           r-matrixgenerics
           r-minfi
           r-rcolorbrewer
           r-shiny))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/shinyMethyl")
    (synopsis "Interactive visualization for Illumina methylation arrays")
    (description
     "This package provides an interactive tool for visualizing Illumina
methylation array data.  Both the 450k and EPIC array are supported.")
    (license license:artistic2.0)))

(define-public r-shortread
  (package
    (name "r-shortread")
    (version "1.62.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ShortRead" version))
       (sha256
        (base32
         "0x5iqx2k2xs5rwjch0bpcb8pgc0nhb571991yahdr8hsxwbas3jm"))))
    (properties `((upstream-name . "ShortRead")))
    (build-system r-build-system)
    (inputs
     (list zlib))
    (propagated-inputs
     (list r-biobase
           r-biocgenerics
           r-biocparallel
           r-biostrings
           r-genomeinfodb
           r-genomicalignments
           r-genomicranges
           r-hwriter
           r-iranges
           r-lattice
           r-latticeextra
           r-pwalign
           r-rhtslib
           r-rsamtools
           r-s4vectors
           r-xvector
           r-zlibbioc))
    (home-page "https://bioconductor.org/packages/ShortRead")
    (synopsis "FASTQ input and manipulation tools")
    (description
     "This package implements sampling, iteration, and input of FASTQ files.
It includes functions for filtering and trimming reads, and for generating a
quality assessment report.  Data are represented as
@code{DNAStringSet}-derived objects, and easily manipulated for a diversity of
purposes.  The package also contains legacy support for early single-end,
ungapped alignment formats.")
    (license license:artistic2.0)))

(define-public r-sictools
  (package
    (name "r-sictools")
    (version "1.34.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "SICtools" version))
       (sha256
        (base32 "0ymk18aj4r0526xqqkrvd75p4jrgrq2wshx7mqh2wj7nizjfn8fv"))))
    (properties `((upstream-name . "SICtools")))
    (build-system r-build-system)
    (arguments
     (list
      #:phases
      '(modify-phases %standard-phases
         (add-after 'unpack 'patch-curses
           (lambda _
             (substitute* "src/Makefile"
               (("^LIBCURSES.*")
                "LIBCURSES=-lncurses\n")))))))
    (propagated-inputs (list r-biostrings
                             r-doparallel
                             r-genomicranges
                             r-iranges
                             r-matrixstats
                             r-plyr
                             r-rsamtools
                             r-stringr))
    (inputs (list ncurses))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/SICtools")
    (synopsis
     "Find SNV/Indel differences between two bam files with near relationship")
    (description
     "This package is to find SNV/Indel differences between two @file{bam}
files with near relationship in a way of pairwise comparison through each base
position across the genome region of interest.  The difference is inferred by
Fisher test and euclidean distance, the input of which is the base
count (A,T,G,C) in a given position and read counts for indels that span no
less than 2bp on both sides of indel region.")
    (license license:gpl2+)))

(define-public r-simplifyenrichment
  (package
    (name "r-simplifyenrichment")
    (version "1.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "simplifyEnrichment" version))
       (sha256
        (base32
         "1031rkl2gcgx4k1biz4bc38hfq7r6diqk48769jhii0r06ablrhi"))))
    (properties
     `((upstream-name . "simplifyEnrichment")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotationdbi
           r-biocgenerics
           r-circlize
           r-clue
           r-cluster
           r-colorspace
           r-complexheatmap
           r-digest
           r-getoptlong
           r-globaloptions
           r-go-db
           r-gosemsim
           r-matrix
           r-org-hs-eg-db
           r-proxyc
           r-slam
           r-tm))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/jokergoo/simplifyEnrichment")
    (synopsis "Simplify functional enrichment results")
    (description "This package provides a new clustering algorithm, binary
cut, for clustering similarity matrices of functional terms is implemented in
this package.  It also provides functionalities for visualizing, summarizing
and comparing the clusterings.")
    (license license:expat)))

(define-public r-singscore
  (package
    (name "r-singscore")
    (version "1.24.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "singscore" version))
              (sha256
               (base32
                "12avzrc16mxz7v8jp4rxz5ia6008rjcja1h3hd0n60z644w9cng2"))))
    (properties `((upstream-name . "singscore")))
    (build-system r-build-system)
    (propagated-inputs (list r-biobase
                             r-biocparallel
                             r-edger
                             r-ggplot2
                             r-ggrepel
                             r-gseabase
                             r-magrittr
                             r-matrixstats
                             r-plotly
                             r-plyr
                             r-rcolorbrewer
                             r-reshape
                             r-reshape2
                             r-s4vectors
                             r-summarizedexperiment
                             r-tidyr))
    (native-inputs (list r-knitr))
    (home-page "https://davislaboratory.github.io/singscore/")
    (synopsis "Rank-based single-sample gene set scoring method")
    (description
     "This package provides a simple single-sample gene signature scoring
method that uses rank-based statistics to analyze the sample's gene expression
profile.  It scores the expression activities of gene sets at a single-sample
level.")
    (license license:gpl3)))

(define-public r-tcgautils
  (package
    (name "r-tcgautils")
    (version "1.24.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "TCGAutils" version))
              (sha256
               (base32
                "0fmz1lfpn8ws584q4xlicg897qs36wgr78crw0d9415ci6qqb4r8"))))
    (properties `((upstream-name . "TCGAutils")))
    (build-system r-build-system)
    (propagated-inputs (list r-annotationdbi
                             r-biocbaseutils
                             r-biocgenerics
                             r-genomeinfodb
                             r-genomicdatacommons
                             r-genomicfeatures
                             r-genomicranges
                             r-iranges
                             r-multiassayexperiment
                             r-raggedexperiment
                             r-rvest
                             r-s4vectors
                             r-stringr
                             r-summarizedexperiment
                             r-xml2))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/TCGAutils")
    (synopsis "TCGA utility functions for data management")
    (description
     "This package provides a suite of helper functions for checking and
manipulating TCGA data including data obtained from the @code{curatedTCGAData}
experiment package.  These functions aim to simplify and make working with
TCGA data more manageable.  Exported functions include those that import data
from flat files into Bioconductor objects, convert row annotations, and
identifier translation via the GDC API.")
    (license license:artistic2.0)))

(define-public r-tkwidgets
  (package
    (name "r-tkwidgets")
    (version "1.82.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "tkWidgets" version))
              (sha256
               (base32
                "0k1d5r306mbry0h7yqp246i13adxlmxl2vhwdsfgh29v8qkg1szl"))))
    (properties `((upstream-name . "tkWidgets")))
    (build-system r-build-system)
    (propagated-inputs (list r-dyndoc r-widgettools))
    (home-page "https://bioconductor.org/packages/tkWidgets")
    (synopsis "R based tk widgets")
    (description
     "This package implements widgets to provide user interfaces.")
    (license license:artistic2.0)))

(define-public r-toast
  (package
    (name "r-toast")
    (version "1.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "TOAST" version))
       (sha256
        (base32 "0srjx9z59a65nhrxbzxzxnkyn28zskc6z4p8drihq1lhw6mvdp4r"))))
    (properties `((upstream-name . "TOAST")))
    (build-system r-build-system)
    (propagated-inputs (list r-corpcor
                             r-doparallel
                             r-epidish
                             r-ggally
                             r-ggplot2
                             r-limma
                             r-nnls
                             r-quadprog
                             r-summarizedexperiment
                             r-tidyr))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/TOAST")
    (synopsis "Tools for the analysis of heterogeneous tissues")
    (description
     "This package is devoted to analyzing high-throughput data (e.g. gene
expression microarray, DNA methylation microarray, RNA-seq) from complex
tissues.  Current functionalities include

@enumerate
@item detect cell-type specific or cross-cell type differential signals
@item tree-based differential analysis
@item improve variable selection in reference-free deconvolution
@item partial reference-free deconvolution with prior knowledge.
@end enumerate")
    (license license:gpl2)))

(define-public r-trackviewer
  (package
    (name "r-trackviewer")
    (version "1.40.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "trackViewer" version))
              (sha256
               (base32
                "01m0jwig3l6dfhd3w5aq4sx2krsipjg5jjxa78kalfrzrm6k05s4"))
              (snippet
               '(delete-file "inst/htmlwidgets/lib/d3/d3.v4.min.js"))))
    (properties `((upstream-name . "trackViewer")))
    (build-system r-build-system)
    (arguments
     (list
      #:phases
      '(modify-phases %standard-phases
         (add-after 'unpack 'process-javascript
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "inst/htmlwidgets/lib/d3"
               (let ((source (assoc-ref inputs "_"))
                     (target "d3.v4.min.js"))
                 (format #true "Processing ~a --> ~a~%"
                         source target)
                 (invoke "esbuild" source "--minify"
                         (string-append "--outfile=" target)))))))))
    (propagated-inputs (list r-annotationdbi
                             r-biocgenerics
                             r-genomeinfodb
                             r-genomicalignments
                             r-genomicfeatures
                             r-genomicranges
                             r-grimport
                             r-gviz
                             r-htmlwidgets
                             r-igraph
                             r-interactionset
                             r-iranges
                             r-plotrix
                             r-rhdf5
                             r-rsamtools
                             r-rtracklayer
                             r-s4vectors
                             r-scales
                             r-strawr
                             r-txdbmaker))
    (native-inputs
     (list esbuild
           r-knitr
           (origin
             (method url-fetch)
             (uri "https://web.archive.org/web/20230428092426id_/\
https://d3js.org/d3.v4.js")
             (sha256
              (base32
               "0y7byf6kcinfz9ac59jxc4v6kppdazmnyqfav0dm4h550fzfqqlg")))))
    (home-page "https://bioconductor.org/packages/trackViewer")
    (synopsis "Web interface for interactive multi-omics data analysis")
    (description
     "TrackViewer offers multi-omics analysis with web based tracks and
lollipops.  Visualize mapped reads along with annotation as track layers for
NGS datasets such as ChIP-seq, RNA-seq, miRNA-seq, DNA-seq, SNPs and
methylation data.")
    (license license:gpl2+)))

(define-public r-transcriptr
  (package
    (name "r-transcriptr")
    (version "1.32.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "transcriptR" version))
       (sha256
        (base32 "1lx11dyr8q4yd57l7rj0788488amhiiahfml2m69554hk94cd46i"))))
    (properties `((upstream-name . "transcriptR")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocgenerics
           r-caret
           r-chipseq
           r-e1071
           r-genomeinfodb
           r-genomicalignments
           r-genomicfeatures
           r-genomicranges
           r-ggplot2
           r-iranges
           r-proc
           r-reshape2
           r-rsamtools
           r-rtracklayer
           r-s4vectors))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/transcriptR")
    (synopsis "Primary transcripts detection and quantification")
    (description
     "The differences in the RNA types being sequenced have an impact on the
resulting sequencing profiles.  mRNA-seq data is enriched with reads derived
from exons, while GRO-, nucRNA- and chrRNA-seq demonstrate a substantial
broader coverage of both exonic and intronic regions.  The presence of
intronic reads in GRO-seq type of data makes it possible to use it to
computationally identify and quantify all de novo continuous regions of
transcription distributed across the genome.  This type of data, however, is
more challenging to interpret and less common practice compared to mRNA-seq.
One of the challenges for primary transcript detection concerns the
simultaneous transcription of closely spaced genes, which needs to be properly
divided into individually transcribed units.  The R package transcriptR
combines RNA-seq data with ChIP-seq data of histone modifications that mark
active Transcription Start Sites (TSSs), such as, H3K4me3 or H3K9/14Ac to
overcome this challenge.  The advantage of this approach over the use of, for
example, gene annotations is that this approach is data driven and therefore
able to deal also with novel and case specific events.")
    (license license:gpl3)))

(define-public r-trajectoryutils
  (package
    (name "r-trajectoryutils")
    (version "1.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "TrajectoryUtils" version))
       (sha256
        (base32
         "15q1mq48nclf2lqc2v769q79frrbm2l65d54vcqg06b861q2wq9h"))))
    (properties
     `((upstream-name . "TrajectoryUtils")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-igraph r-matrix r-s4vectors r-singlecellexperiment
           r-summarizedexperiment))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/TrajectoryUtils")
    (synopsis "Single-cell trajectory analysis utilities")
    (description
     "This package implements low-level utilities for single-cell trajectory
analysis, primarily intended for re-use inside higher-level packages.  It
includes a function to create a cluster-level minimum spanning tree and data
structures to hold pseudotime inference results.")
    (license license:gpl3)))

(define-public r-trna
  (package
    (name "r-trna")
    (version "1.22.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "tRNA" version))
       (sha256
        (base32
         "0dg4s5kxsdh2j8smqi8vm7yncy9kb79sj8xlvf4wgf3aljarms21"))))
    (properties
     `((upstream-name . "tRNA")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocgenerics
           r-biostrings
           r-genomicranges
           r-ggplot2
           r-iranges
           r-modstrings
           r-s4vectors
           r-scales
           r-stringr
           r-structstrings
           r-xvector))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/tRNA")
    (synopsis "Analyzing tRNA sequences and structures")
    (description
     "The tRNA package allows tRNA sequences and structures to be accessed and
used for subsetting.  In addition, it provides visualization tools to compare
feature parameters of multiple tRNA sets and correlate them to additional
data.  The tRNA package uses GRanges objects as inputs requiring only few
additional column data sets.")
    (license license:gpl3)))

(define-public r-trnadbimport
  (package
    (name "r-trnadbimport")
    (version "1.22.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "tRNAdbImport" version))
       (sha256
        (base32 "0yvgxxf5d5mh91cmbba66krj6jz5gl1qkgjck9310k4mk7pjfww3"))))
    (properties `((upstream-name . "tRNAdbImport")))
    (build-system r-build-system)
    (propagated-inputs (list r-biostrings
                             r-genomicranges
                             r-httr2
                             r-iranges
                             r-modstrings
                             r-s4vectors
                             r-stringr
                             r-structstrings
                             r-trna
                             r-xml2))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/tRNAdbImport")
    (synopsis "Importing from tRNAdb and mitotRNAdb as GRanges objects")
    (description
     "@code{tRNAdbImport} imports the entries of the @code{tRNAdb} and
@code{mtRNAdb} as GRanges object.")
    (license license:gpl3)))

(define-public r-scds
  (package
    (name "r-scds")
    (version "1.20.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "scds" version))
              (sha256
               (base32
                "19xma4kmx8s6m6dvqyw461ahqyfqb7z3n5dz4ppab4xcpmqvx0vh"))))
    (properties `((upstream-name . "scds")))
    (build-system r-build-system)
    (propagated-inputs (list r-dplyr
                             r-matrix
                             r-proc
                             r-s4vectors
                             r-singlecellexperiment
                             r-summarizedexperiment
                             r-xgboost))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/scds")
    (synopsis "In-silico doublet annotation for single cell RNA sequencing data")
    (description
     "This is an R package for doublet annotation of single cell RNA
sequencing data.  @code{scds} provides methods to annotate doublets in
scRNA-seq data computationally.")
    (license license:expat)))

(define-public r-slingshot
  (package
   (name "r-slingshot")
   (version "2.12.0")
   (source (origin
            (method url-fetch)
            (uri (bioconductor-uri "slingshot" version))
            (sha256
             (base32
              "055dylajbn94khi0829qhxx0lbn1pvh30xw09gjn1jvxsgzj49nc"))))
   (build-system r-build-system)
   (propagated-inputs
    (list r-igraph
          r-matrixstats
          r-princurve
          r-s4vectors
          r-singlecellexperiment
          r-summarizedexperiment
          r-trajectoryutils))
   (native-inputs
    (list r-knitr))
   (home-page "https://bioconductor.org/packages/slingshot")
   (synopsis "Tools for ordering single-cell sequencing")
   (description "This package provides functions for inferring continuous,
branching lineage structures in low-dimensional data.  Slingshot was designed
to model developmental trajectories in single-cell RNA sequencing data and
serve as a component in an analysis pipeline after dimensionality reduction
and clustering.  It is flexible enough to handle arbitrarily many branching
events and allows for the incorporation of prior knowledge through supervised
graph construction.")
   (license license:artistic2.0)))

;; This is a CRAN package but it depends on a bioconductor package.
(define-public r-speaq
  (package
    (name "r-speaq")
    (version "2.7.0")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "speaq" version))
              (sha256
               (base32
                "0z9a3nbfazphp090c6hg892vjq7jp4g4cij3s5wbs1q567inbmlk"))))
    (properties `((upstream-name . "speaq")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-cluster
           r-data-table
           r-dosnow
           r-foreach
           r-ggplot2
           r-gridextra
           r-impute
           r-massspecwavelet
           r-missforest
           r-reshape2
           r-rfast
           r-rvest
           r-xml2))
    (native-inputs (list r-knitr))
    (home-page "https://cran.r-project.org/package=speaq")
    (synopsis "Tools for nuclear magnetic resonance spectra alignment")
    (description
     "This package aims to make @acronym{NMR, Nuclear Magnetic Resonance}
spectroscopy data analysis as easy as possible.  It only requires a small set
of functions to perform an entire analysis.  Speaq offers the possibility of
raw spectra alignment and quantitation but also an analysis based on features
whereby the spectra are converted to peaks which are then grouped and turned
into features.  These features can be processed with any number of statistical
tools either included in speaq or available elsewhere on CRAN.")
    (license license:asl2.0)))

(define-public r-spectra
  (package
    (name "r-spectra")
    (version "1.14.1")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "Spectra" version))
              (sha256
               (base32
                "0mvfndd7n7varrzmv9cd3ygwbdn97x5qcgmzcxc6hr76lnkwp8al"))))
    (properties `((upstream-name . "Spectra")))
    (build-system r-build-system)
    (propagated-inputs (list r-biocgenerics
                             r-biocparallel
                             r-fs
                             r-iranges
                             r-metabocoreutils
                             r-mscoreutils
                             r-protgenerics
                             r-s4vectors))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/RforMassSpectrometry/Spectra")
    (synopsis "Spectra infrastructure for mass spectrometry data")
    (description
     "The Spectra package defines an efficient infrastructure for storing and
handling mass spectrometry spectra and functionality to subset, process,
visualize and compare spectra data.  It provides different
implementations (backends) to store mass spectrometry data.  These comprise
backends tuned for fast data access and processing and backends for very large
data sets ensuring a small memory footprint.")
    (license license:artistic2.0)))

(define-public r-stager
  (package
    (name "r-stager")
    (version "1.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "stageR" version))
       (sha256
        (base32 "0ayjwv2i9q8dzq5pzhyzkxzkz9ycka2v6ccvq5y9cg7x0mw4qvbh"))))
    (properties `((upstream-name . "stageR")))
    (build-system r-build-system)
    (propagated-inputs (list r-summarizedexperiment))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/stageR")
    (synopsis "Stage-wise analysis of high throughput gene expression data")
    (description
     "The stageR package allows automated stage-wise analysis of
high-throughput gene expression data.  The method is published in Genome
Biology at
@url{https://genomebiology.biomedcentral.com/articles/10.1186/s13059-017-1277-0}.")
    (license license:gpl3)))

(define-public r-stringdb
  (package
    (name "r-stringdb")
    (version "2.16.4")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "STRINGdb" version))
       (sha256
        (base32 "02fim09w9s38hvgp1g3fxqabd5r2fjysljlfrya2nr17drqkzqyx"))))
    (properties `((upstream-name . "STRINGdb")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-gplots
           r-hash
           r-httr
           r-igraph
           r-plotrix
           r-plyr
           r-png
           r-rcolorbrewer
           r-sqldf))
    (home-page "https://git.bioconductor.org/packages/STRINGdb")
    (synopsis "Search tool for the retrieval of interacting proteins database")
    (description
     "The @code{STRINGdb} package provides an R interface to the STRING
protein-protein interactions database.  @url{https://www.string-db.org,
STRING} is a database of known and predicted protein-protein interactions.
The interactions include direct (physical) and indirect (functional)
associations.  Each interaction is associated with a combined confidence score
that integrates the various evidences.")
    (license license:gpl2)))

(define-public r-structstrings
  (package
    (name "r-structstrings")
    (version "1.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Structstrings" version))
       (sha256
        (base32 "1ka9kixhgc9vfar18fzd8vd5ydmdkqa2s6xsmhlh927zrijs3vz9"))))
    (properties `((upstream-name . "Structstrings")))
    (build-system r-build-system)
    (propagated-inputs (list r-biocgenerics
                             r-biostrings
                             r-crayon
                             r-iranges
                             r-s4vectors
                             r-stringi
                             r-stringr
                             r-xvector))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/FelixErnst/Structstrings")
    (synopsis "Implementation of the dot bracket annotations with Biostrings")
    (description
     "The Structstrings package implements the widely used dot bracket
annotation for storing base pairing information in structured
RNA. Structstrings uses the infrastructure provided by the Biostrings package
and derives the @code{DotBracketString} and related classes from the BString
class.  From these, base pair tables can be produced for in depth analysis.
In addition, the loop indices of the base pairs can be retrieved as well.  For
better efficiency, information conversion is implemented in C, inspired to a
large extend by the @code{ViennaRNA} package.")
    (license license:artistic2.0)))

(define-public r-structuralvariantannotation
  (package
    (name "r-structuralvariantannotation")
    (version "1.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "StructuralVariantAnnotation" version))
       (sha256
        (base32 "0hnpa6ykfasp2hshqdny29dyw6y9dib4rsm379fm7n8iami1rjp1"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-assertthat
           r-biocgenerics
           r-biostrings
           r-dplyr
           r-genomeinfodb
           r-genomicfeatures
           r-genomicranges
           r-iranges
           r-pwalign
           r-rlang
           r-rtracklayer
           r-s4vectors
           r-stringr
           r-summarizedexperiment
           r-variantannotation))
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/StructuralVariantAnnotation/")
    (synopsis "R package designed to simplify structural variant analysis")
    (description
     "This package contains useful helper functions for dealing with structural
variants in VCF format.  The packages contains functions for parsing VCFs from
a number of popular callers as well as functions for dealing with breakpoints
involving two separate genomic loci encoded as GRanges objects.")
    (license license:gpl3)))

(define-public r-summarizedexperiment
  (package
    (name "r-summarizedexperiment")
    (version "1.34.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "SummarizedExperiment" version))
              (sha256
               (base32
                "02cvgq2vgg0gz1gxkc4vnr9jg4wkj32bl5a1qyv77wnw2b2v5hp0"))))
    (properties
     `((upstream-name . "SummarizedExperiment")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biobase
           r-biocgenerics
           r-delayedarray
           r-genomeinfodb
           r-genomicranges
           r-iranges
           r-matrix
           r-matrixgenerics
           r-s4arrays
           r-s4vectors))
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/SummarizedExperiment")
    (synopsis "Container for representing genomic ranges by sample")
    (description
     "The SummarizedExperiment container contains one or more assays, each
represented by a matrix-like object of numeric or other mode.  The rows
typically represent genomic ranges of interest and the columns represent
samples.")
    (license license:artistic2.0)))

(define-public r-survcomp
  (package
    (name "r-survcomp")
    (version "1.54.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "survcomp" version))
              (sha256
               (base32
                "090dl580cwicn3px1khkhcix25snig0k7phb7z3nzmrbxmp82ygc"))))
    (properties `((upstream-name . "survcomp")))
    (build-system r-build-system)
    (propagated-inputs (list r-bootstrap
                             r-ipred
                             r-kernsmooth
                             r-prodlim
                             r-rmeta
                             r-suppdists
                             r-survival
                             r-survivalroc))
    (home-page "https://www.pmgenomics.ca/bhklab/")
    (synopsis "Performance assessment and comparison for survival analysis")
    (description
     "This is a package for the assessment and comparison of the performance
of risk prediction (survival) models.")
    (license license:artistic2.0)))

(define-public r-sva
  (package
    (name "r-sva")
    (version "3.52.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "sva" version))
       (sha256
        (base32
         "07iq894mf5d66cxjl283pcvj1frj56pb15z984d4svk6f5cacg3z"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocparallel
           r-edger
           r-genefilter
           r-limma
           r-matrixstats
           r-mgcv))
    (home-page "https://bioconductor.org/packages/sva")
    (synopsis "Surrogate variable analysis")
    (description
     "This package contains functions for removing batch effects and other
unwanted variation in high-throughput experiment.  It also contains functions
for identifying and building surrogate variables for high-dimensional data
sets.  Surrogate variables are covariates constructed directly from
high-dimensional data like gene expression/RNA sequencing/methylation/brain
imaging data that can be used in subsequent analyses to adjust for unknown,
unmodeled, or latent sources of noise.")
    (license license:artistic2.0)))

(define-public r-systempiper
  (package
    (name "r-systempiper")
    (version "2.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "systemPipeR" version))
       (sha256
        (base32
         "13xxcqy5hhy45zglv6gc86ndmisfkjq4dg55x2j1kw3jqha1brnx"))))
    (properties `((upstream-name . "systemPipeR")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocgenerics
           r-biostrings
           r-crayon
           r-genomicranges
           r-ggplot2
           r-htmlwidgets
           r-magrittr
           r-rsamtools
           r-s4vectors
           r-shortread
           r-stringr
           r-summarizedexperiment
           r-yaml))
    (native-inputs
     (list r-knitr))
    (home-page "https://github.com/tgirke/systemPipeR")
    (synopsis "Next generation sequencing workflow and reporting environment")
    (description
     "This R package provides tools for building and running automated
end-to-end analysis workflows for a wide range of @dfn{next generation
sequence} (NGS) applications such as RNA-Seq, ChIP-Seq, VAR-Seq and Ribo-Seq.
Important features include a uniform workflow interface across different NGS
applications, automated report generation, and support for running both R and
command-line software, such as NGS aligners or peak/variant callers, on local
computers or compute clusters.  Efficient handling of complex sample sets and
experimental designs is facilitated by a consistently implemented sample
annotation infrastructure.")
    (license license:artistic2.0)))

(define-public r-topgo
  (package
    (name "r-topgo")
    (version "2.56.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "topGO" version))
              (sha256
               (base32
                "0ly4rai3na88dnfizsi60r6py0qmf0pw95b0hprfkvzpjyd72p75"))))
    (properties
     `((upstream-name . "topGO")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotationdbi
           r-biobase
           r-biocgenerics
           r-dbi
           r-go-db
           r-graph
           r-lattice
           r-matrixstats
           r-sparsem))
    (home-page "https://bioconductor.org/packages/topGO")
    (synopsis "Enrichment analysis for gene ontology")
    (description
     "The topGO package provides tools for testing @dfn{gene ontology} (GO)
terms while accounting for the topology of the GO graph.  Different test
statistics and different methods for eliminating local similarities and
dependencies between GO terms can be implemented and applied.")
    ;; Any version of the LGPL applies.
    (license license:lgpl2.1+)))

(define-public r-txdbmaker
  (package
    (name "r-txdbmaker")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "txdbmaker" version))
       (sha256
        (base32 "1nl7lr0x0kicd44gvwrdslcnnlmqaf57rgp1gb22yvz3lfzjziqi"))))
    (properties `((upstream-name . "txdbmaker")))
    (build-system r-build-system)
    (propagated-inputs (list r-annotationdbi
                             r-biobase
                             r-biocgenerics
                             r-biocio
                             r-biomart
                             r-dbi
                             r-genomeinfodb
                             r-genomicfeatures
                             r-genomicranges
                             r-httr
                             r-iranges
                             r-rjson
                             r-rsqlite
                             r-rtracklayer
                             r-s4vectors
                             r-ucsc-utils))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/txdbmaker")
    (synopsis "Tools for making TxDb objects from genomic annotations")
    (description
     "This package provides a set of tools for making @code{TxDb} objects from
genomic annotations from various sources (e.g. UCSC, Ensembl, and GFF files).
These tools allow the user to download the genomic locations of transcripts,
exons, and CDS, for a given assembly, and to import them in a @code{TxDb}
object.  @code{TxDb} objects are implemented in the @code{GenomicFeatures}
package, together with flexible methods for extracting the desired features in
convenient formats.")
    (license license:artistic2.0)))

(define-public r-tximport
  (package
    (name "r-tximport")
    (version "1.32.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "tximport" version))
              (sha256
               (base32
                "0wayib05k976wivf9xgbfz5mfvi5hmxj3wm6zw031bj4w8dkc1v6"))))
    (build-system r-build-system)
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/tximport")
    (synopsis "Import and summarize transcript-level estimates for gene-level analysis")
    (description
     "This package provides tools to import transcript-level abundance,
estimated counts and transcript lengths, and to summarize them into matrices
for use with downstream gene-level analysis packages.  Average transcript
length, weighted by sample-specific transcript abundance estimates, is
provided as a matrix which can be used as an offset for different expression
of gene-level counts.")
    (license license:gpl2+)))

;; This is a CRAN package, but it depends on a Bioconductor package.
(define-public r-valr
  (package
    (name "r-valr")
    (version "0.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "valr" version))
       (sha256
        (base32
         "1pp8naglh26yi8c2p15arx8jkj3jyslzw7j3n5bnzngdpfprk3as"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-broom
           r-cli
           r-dplyr
           r-ggplot2
           r-lifecycle
           r-rcpp
           r-readr
           r-rlang
           r-rtracklayer
           r-stringr
           r-tibble))
    (native-inputs
     (list r-knitr))
    (home-page "https://github.com/rnabioco/valr")
    (synopsis "Genome interval arithmetic in R")
    (description
     "This package enables you to read and manipulate genome intervals and
signals.  It provides functionality similar to command-line tool suites within
R, enabling interactive analysis and visualization of genome-scale data.")
    (license license:expat)))

(define-public r-variantannotation
  (package
    (name "r-variantannotation")
    (version "1.50.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "VariantAnnotation" version))
              (sha256
               (base32
                "0slvlhm65hkjspzlzs3gi04a6mlfwvda314z1nmzpdi3y5111fg7"))))
    (properties
     `((upstream-name . "VariantAnnotation")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotationdbi
           r-biobase
           r-biocgenerics
           r-biostrings
           r-bsgenome
           r-dbi
           r-genomeinfodb
           r-genomicfeatures
           r-genomicranges
           r-iranges
           r-matrixgenerics
           r-rhtslib
           r-rsamtools
           r-rtracklayer
           r-s4vectors
           r-summarizedexperiment
           r-xvector
           r-zlibbioc))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/VariantAnnotation")
    (synopsis "Package for annotation of genetic variants")
    (description "This R package can annotate variants, compute amino acid
coding changes and predict coding outcomes.")
    (license license:artistic2.0)))

(define-public r-vsn
  (package
    (name "r-vsn")
    (version "3.72.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "vsn" version))
       (sha256
        (base32
         "1r6cd92d4xlx4zdyy1sfaxicjf4q3arflmswpdkckkc4h9c6zf8m"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-affy r-biobase r-ggplot2 r-lattice r-limma))
    (native-inputs
     (list r-knitr)) ; for vignettes
    (home-page "https://bioconductor.org/packages/release/bioc/html/vsn.html")
    (synopsis "Variance stabilization and calibration for microarray data")
    (description
     "The package implements a method for normalising microarray intensities,
and works for single- and multiple-color arrays.  It can also be used for data
from other technologies, as long as they have similar format.  The method uses
a robust variant of the maximum-likelihood estimator for an
additive-multiplicative error model and affine calibration.  The model
incorporates data calibration step (a.k.a.  normalization), a model for the
dependence of the variance on the mean intensity and a variance stabilizing
data transformation.  Differences between transformed intensities are
analogous to \"normalized log-ratios\".  However, in contrast to the latter,
their variance is independent of the mean, and they are usually more sensitive
and specific in detecting differential transcription.")
    (license license:artistic2.0)))

;; There is no source tarball, so we fetch the code from the Bioconductor git
;; repository.
(define-public r-xcir
  (let ((commit "3b59d456f2ad7f70285915b036b1dc4279687277")
        (revision "1"))
    (package
      (name "r-xcir")
      (version (git-version "1.8.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://git.bioconductor.org/packages/XCIR")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1xxw5ady5j2p7z7zjxgx7lhm85x7fxbljiv49lc2ghsvh9wm937p"))))
      (properties `((upstream-name . "XCIR")))
      (build-system r-build-system)
      (propagated-inputs (list r-biomart
                               r-biostrings
                               r-data-table
                               r-ggplot2
                               r-iranges
                               r-readxl
                               r-s4vectors
                               r-seqminer
                               r-variantannotation))
      (native-inputs (list r-knitr))
      (home-page "https://github.com/SRenan/XCIR")
      (synopsis "Analysis of X chromosome inactivation")
      (description
       "This package is an R package that offers models and tools for subject
level analysis of @dfn{X chromosome inactivation} (XCI) and XCI-escape
inference.")
      (license license:gpl2))))

(define-public r-xina
  (package
    (name "r-xina")
    (version "1.22.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "XINA" version))
       (sha256
        (base32 "177yzqmkf3f2xgfrz1zpcw554yzm4njd2chhmvxi2vvijvnrgdf3"))))
    (properties `((upstream-name . "XINA")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-alluvial
           r-ggplot2
           r-gridextra
           r-igraph
           r-mclust
           r-plyr
           r-stringdb))
    (native-inputs (list r-knitr))
    (home-page "https://git.bioconductor.org/packages/XINA")
    (synopsis "Identifying proteins that exhibit similar patterns")
    (description
     "The aim of @code{XINA} is to determine which proteins exhibit similar
patterns within and across experimental conditions, since proteins with
co-abundance patterns may have common molecular functions.  @code{XINA} imports
multiple datasets, tags dataset in silico, and combines the data for subsequent
subgrouping into multiple clusters.  The result is a single output depicting
the variation across all conditions.  @code{XINA} not only extracts
coabundance profiles within and across experiments, but also incorporates
protein-protein interaction databases and integrative resources such as
@dfn{Kyoto encyclopedia of genes and genomes} (KEGG) to infer interactors and
molecular functions, respectively, and produces intuitive graphical outputs.")
    (license license:gpl3)))

(define-public r-xmapbridge
  (package
    (name "r-xmapbridge")
    (version "1.62.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "xmapbridge" version))
       (sha256
        (base32 "0laahd3v5m313q1iq259aikgqf7zlmpxyxhws2pr7vg97g37bbhs"))))
    (properties `((upstream-name . "xmapbridge")))
    (build-system r-build-system)
    (home-page "https://git.bioconductor.org/packages/xmapbridge")
    (synopsis "Display numeric data in the web based genome browser X:MAP")
    (description
     "The package @code{xmapbridge} can plot graphs in the X:Map genome
browser.  X:Map uses the Google Maps API to provide a scrollable view of the
genome.  It supports a number of species, and can be accessed at
@url{http://xmap.picr.man.ac.uk}.  This package exports plotting files in a
suitable format.  Graph plotting in R is done using calls to the functions
@code{xmap.plot} and @code{xmap.points}, which have parameters that aim to be
similar to those used by the standard plot methods in R.  These result in data
being written to a set of files (in a specific directory structure) that
contain the data to be displayed, as well as some additional meta-data
describing each of the graphs.")
    (license license:lgpl3)))

(define-public r-xvector
  (package
    (name "r-xvector")
    (version "0.44.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "XVector" version))
              (sha256
               (base32
                "0fxv97m79w0ph4wbgp9drz6ahvnnzpv08grb11lr6a7laqq5x8s2"))))
    (properties
     `((upstream-name . "XVector")))
    (build-system r-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'use-system-zlib
           (lambda _
             (substitute* "DESCRIPTION"
               (("zlibbioc, ") ""))
             (substitute* "NAMESPACE"
               (("import\\(zlibbioc\\)") ""))
             #t)))))
    (inputs
     (list zlib))
    (propagated-inputs
     (list r-biocgenerics r-iranges r-s4vectors r-zlibbioc))
    (home-page "https://bioconductor.org/packages/XVector")
    (synopsis "Representation and manpulation of external sequences")
    (description
     "This package provides memory efficient S4 classes for storing sequences
\"externally\" (behind an R external pointer, or on disk).")
    (license license:artistic2.0)))

(define-public r-zlibbioc
  (package
    (name "r-zlibbioc")
    (version "1.50.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "zlibbioc" version))
              (sha256
               (base32
                "155q6983xjdjpq445lpg5n9gzf8gr18nll6dmjk7jwdq7fdxb602"))))
    (properties
     `((upstream-name . "zlibbioc")))
    (build-system r-build-system)
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/zlibbioc")
    (synopsis "Provider for zlib-1.2.5 to R packages")
    (description "This package uses the source code of zlib-1.2.5 to create
libraries for systems that do not have these available via other means.")
    (license license:artistic2.0)))

(define-public r-zellkonverter
  (package
    (name "r-zellkonverter")
    (version "1.14.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "zellkonverter" version))
       (sha256
        (base32 "1fyy3v1papg46cclk5fg8iqmg0671ccr82alvv41hk5d85ck04aq"))))
    (properties `((upstream-name . "zellkonverter")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-basilisk
           r-cli
           r-delayedarray
           r-matrix
           r-reticulate
           r-s4vectors
           r-singlecellexperiment
           r-summarizedexperiment))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/theislab/zellkonverter")
    (synopsis "Conversion between AnnData and single-cell experiments objects")
    (description
     "This package provides methods to convert between Python AnnData objects
and SingleCellExperiment objects.  These are primarily intended for use by
downstream Bioconductor packages that wrap Python methods for single-cell data
analysis.  It also includes functions to read and write H5AD files used for
saving AnnData objects to disk.")
    (license license:expat)))

(define-public r-geneplotter
  (package
    (name "r-geneplotter")
    (version "1.82.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "geneplotter" version))
       (sha256
        (base32
         "19znr3vwp20x84jiwy0im7s6vd8a71yng8w2ji88ffzq0xdkyyjm"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotate
           r-annotationdbi
           r-biobase
           r-biocgenerics
           r-lattice
           r-rcolorbrewer))
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/geneplotter")
    (synopsis "Graphics functions for genomic data")
    (description
     "This package provides functions for plotting genomic data.")
    (license license:artistic2.0)))

(define-public r-oligoclasses
  (package
    (name "r-oligoclasses")
    (version "1.66.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "oligoClasses" version))
       (sha256
        (base32
         "15icfcmpwrnjpyahc4v168fpcbsml6x7h4drmsyh9l8d1284yb4n"))))
    (properties `((upstream-name . "oligoClasses")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-affyio
           r-biobase
           r-biocgenerics
           r-biocmanager
           r-biostrings
           r-dbi
           r-ff
           r-foreach
           r-genomicranges
           r-iranges
           r-rsqlite
           r-s4vectors
           r-summarizedexperiment))
    (home-page "https://bioconductor.org/packages/oligoClasses/")
    (synopsis "Classes for high-throughput arrays")
    (description
     "This package contains class definitions, validity checks, and
initialization methods for classes used by the @code{oligo} and @code{crlmm}
packages.")
    (license license:gpl2+)))

(define-public r-oligo
  (package
    (name "r-oligo")
    (version "1.68.2")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "oligo" version))
       (sha256
        (base32
         "0ch6b4rg5v6fj0fgniqiigx1gnnqgwji90bxqngllman388bx867"))))
    (properties `((upstream-name . "oligo")))
    (build-system r-build-system)
    (inputs (list zlib))
    (propagated-inputs
     (list r-affxparser
           r-affyio
           r-biobase
           r-biocgenerics
           r-biostrings
           r-dbi
           r-ff
           r-oligoclasses
           r-preprocesscore
           r-rsqlite
           r-zlibbioc))
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/oligo/")
    (synopsis "Preprocessing tools for oligonucleotide arrays")
    (description
     "This package provides a package to analyze oligonucleotide
arrays (expression/SNP/tiling/exon) at probe-level.  It currently supports
Affymetrix (CEL files) and NimbleGen arrays (XYS files).")
    (license license:lgpl2.0+)))

(define-public r-qfeatures
  (package
    (name "r-qfeatures")
    (version "1.14.1")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "QFeatures" version))
              (sha256
               (base32
                "1sgvabfad3zibrg449y7hw8l59yb2d8q01x7lp841m45b1g0q9d8"))))
    (properties `((upstream-name . "QFeatures")))
    (build-system r-build-system)
    (propagated-inputs (list r-annotationfilter
                             r-biobase
                             r-biocgenerics
                             r-igraph
                             r-iranges
                             r-lazyeval
                             r-mscoreutils
                             r-multiassayexperiment
                             r-plotly
                             r-protgenerics
                             r-reshape2
                             r-s4vectors
                             r-summarizedexperiment
                             r-tidyr
                             r-tidyselect))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/RforMassSpectrometry/QFeatures")
    (synopsis "Quantitative features for mass spectrometry data")
    (description
     "The QFeatures infrastructure enables the management and processing of
quantitative features for high-throughput mass spectrometry assays.  It
provides a familiar Bioconductor user experience to manages quantitative data
across different assay levels (such as peptide spectrum matches, peptides and
proteins) in a coherent and tractable format.")
    (license license:artistic2.0)))

(define-public r-quantsmooth
  (package
    (name "r-quantsmooth")
    (version "1.70.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "quantsmooth" version))
       (sha256
        (base32 "1mgdrhq0j4b29l3y49h00ccwkz6vhs6hjb1236blmlzdf590q7gc"))))
    (properties `((upstream-name . "quantsmooth")))
    (build-system r-build-system)
    (propagated-inputs (list r-quantreg))
    (home-page "https://bioconductor.org/packages/quantsmooth")
    (synopsis "Quantile smoothing and genomic visualization of array data")
    (description
     "This package implements quantile smoothing.  It contains a dataset used
to produce human chromosomal ideograms for plotting purposes and a collection
of arrays that contains data of chromosome 14 of 3 colorectal tumors.  The
package provides functions for painting chromosomal icons, chromosome or
chromosomal idiogram and other types of plots.  Quantsmooth offers options
like converting chromosomal ids to their numeric form, retrieving the human
chromosomal length from NCBI data, retrieving regions of interest in a vector
of intensities using quantile smoothing, determining cytoband position based
on the location of the probe, and other useful tools.")
    (license license:gpl2)))

(define-public r-qvalue
  (package
    (name "r-qvalue")
    (version "2.36.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "qvalue" version))
       (sha256
        (base32
         "1hgkrvbs1rf8db105ik9ar54zdx49v25hn71jgw7j5r43zngfg2a"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-ggplot2 r-reshape2))
    (native-inputs
     (list r-knitr))
    (home-page "https://github.com/StoreyLab/qvalue")
    (synopsis "Q-value estimation for false discovery rate control")
    (description
     "This package takes a list of p-values resulting from the simultaneous
testing of many hypotheses and estimates their q-values and local @dfn{false
discovery rate} (FDR) values.  The q-value of a test measures the proportion
of false positives incurred when that particular test is called significant.
The local FDR measures the posterior probability the null hypothesis is true
given the test's p-value.  Various plots are automatically generated, allowing
one to make sensible significance cut-offs.  The software can be applied to
problems in genomics, brain imaging, astrophysics, and data mining.")
    ;; Any version of the LGPL.
    (license license:lgpl3+)))

(define r-rcppnumerical
  (package
   (name "r-rcppnumerical")
   (version "0.4-0")
   (source (origin
            (method url-fetch)
            (uri (cran-uri "RcppNumerical" version))
            (sha256
             (base32
              "1a92fql6mijhnr1kxkcxwivf95pk9lhgmhzkshs51h0ybfv5krik"))))
   (properties `((upstream-name . "RcppNumerical")))
   (build-system r-build-system)
   (propagated-inputs
    `(("r-rcpp" ,r-rcpp)
      ("r-rcppeigen" ,r-rcppeigen)))
   (native-inputs
    `(("r-knitr" ,r-knitr)))
   (home-page "https://github.com/yixuan/RcppNumerical")
   (synopsis "Rcpp integration for numerical computing libraries")
   (description
    "This package provides a collection of libraries for numerical computing
(numerical integration, optimization, etc.) and their integration with
@code{Rcpp}.")
   (license license:gpl2+)))

(define-public r-apcomplex
  (package
    (name "r-apcomplex")
    (version "2.70.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "apComplex" version))
       (sha256
        (base32 "1fsqrds5r8bhdf7fbnw6rf3vqapl6xk1a5l6jjb98raxmjsrfyqs"))))
    (properties `((upstream-name . "apComplex")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-graph
           r-org-sc-sgd-db
           r-rbgl
           r-rgraphviz))
    (home-page "https://bioconductor.org/packages/apComplex")
    (synopsis "Estimate protein complex membership using AP-MS protein data")
    (description
     "This package provides functions to estimate a bipartite graph of protein
complex membership using @acronym{AP-MS, affinity purification–mass
spectrometry} data.")
    (license license:lgpl2.0+)))

(define-public r-apeglm
  (package
    (name "r-apeglm")
    (version "1.26.1")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "apeglm" version))
              (sha256
               (base32
                "039my876v0k2mr50w90w3phj4cq55awf5ks07kr2fnm7w211gxvs"))))
    (properties `((upstream-name . "apeglm")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-emdbook
           r-genomicranges
           r-rcpp
           r-rcppeigen
           r-rcppnumerical
           r-summarizedexperiment))
    (native-inputs (list r-knitr r-rmarkdown))
    (home-page "https://bioconductor.org/packages/apeglm")
    (synopsis "Approximate posterior estimation for GLM coefficients")
    (description "This package provides Bayesian shrinkage estimators for
effect sizes for a variety of GLM models, using approximation of the
posterior for individual coefficients.")
    (license license:gpl2)))

(define-public r-greylistchip
  (package
   (name "r-greylistchip")
   (version "1.36.0")
   (source (origin
            (method url-fetch)
            (uri (bioconductor-uri "GreyListChIP" version))
            (sha256
             (base32
              "0dj3pcycnjll8z1r61l6gnjvq74w2l0glrx6hqwcqvk5j7ixcdz7"))))
   (properties `((upstream-name . "GreyListChIP")))
   (build-system r-build-system)
   (propagated-inputs
    (list r-bsgenome
          r-genomeinfodb
          r-genomicalignments
          r-genomicranges
          r-mass
          r-rsamtools
          r-rtracklayer
          r-summarizedexperiment))
   (home-page "https://bioconductor.org/packages/GreyListChIP")
   (synopsis "Greylist artefact regions based on ChIP inputs")
   (description "This package identifies regions of ChIP experiments with high
signal in the input, that lead to spurious peaks during peak calling.")
   (license license:artistic2.0)))

(define-public r-diffbind
  (package
    (name "r-diffbind")
    (version "3.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "DiffBind" version))
       (sha256
        (base32
         "1sr0zy5pbjmjzkwkpjmr26k4dcf3f3kxsl7z2nqw0p0a14rh57pm"))))
    (properties `((upstream-name . "DiffBind")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-amap
           r-apeglm
           r-ashr
           r-biocparallel
           r-deseq2
           r-dplyr
           r-genomicalignments
           r-genomicranges
           r-ggplot2
           r-ggrepel
           r-gplots
           r-greylistchip
           r-iranges
           r-lattice
           r-limma
           r-locfit
           r-rcolorbrewer
           r-rcpp
           r-rhtslib
           r-rsamtools
           r-s4vectors
           r-summarizedexperiment
           r-systempiper))
    (home-page "https://bioconductor.org/packages/DiffBind")
    (synopsis "Differential binding analysis of ChIP-Seq peak data")
    (description
     "This package computes differentially bound sites from multiple
ChIP-seq experiments using affinity (quantitative) data.  Also enables
occupancy (overlap) analysis and plotting functions.")
    (license license:artistic2.0)))

(define-public r-ripseeker
  (package
    (name "r-ripseeker")
    (version "1.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "RIPSeeker" version))
       (sha256
        (base32
         "1wyv9mfrbxzklysfjcnwb8yils71janyyxa982jn0zxx4p9cl3vs"))))
    (properties `((upstream-name . "RIPSeeker")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-s4vectors
           r-iranges
           r-genomicranges
           r-summarizedexperiment
           r-rsamtools
           r-genomicalignments
           r-rtracklayer))
    (home-page "https://bioconductor.org/packages/RIPSeeker")
    (synopsis
     "Identifying protein-associated transcripts from RIP-seq experiments")
    (description
     "This package infers and discriminates RIP peaks from RIP-seq alignments
using two-state HMM with negative binomial emission probability.  While
RIPSeeker is specifically tailored for RIP-seq data analysis, it also provides
a suite of bioinformatics tools integrated within this self-contained software
package comprehensively addressing issues ranging from post-alignments
processing to visualization and annotation.")
    (license license:gpl2)))

(define-public r-mbecs
  (package
    (name "r-mbecs")
    (version "1.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "MBECS" version))
       (sha256
        (base32 "15nv5rbw158d071gscms6p42wzf6ibnabiqcq256rcz6j0r69b63"))))
    (properties `((upstream-name . "MBECS")))
    (build-system r-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'set-HOME
            ;; Fontconfig needs a writable cache
            (lambda _ (setenv "HOME" "/tmp"))))))
    (propagated-inputs
     (list r-cluster
           r-dplyr
           r-ggplot2
           r-gridextra
           r-limma
           r-lme4
           r-lmertest
           r-magrittr
           r-matrix
           r-pheatmap
           r-phyloseq
           r-rmarkdown
           r-ruv
           r-sva
           r-tibble
           r-tidyr
           r-vegan))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/rmolbrich/MBECS")
    (synopsis
     "Evaluation and correction of batch effects in microbiome data-sets")
    (description
     "The @acronym{MBECS, Microbiome Batch Effect Correction Suite} provides a
set of functions to evaluate and mitigate unwated noise due to processing in
batches.  To that end it incorporates a host of batch correcting algorithms
(BECA) from various packages.  In addition it offers a correction and reporting
pipeline that provides a preliminary look at the characteristics of a data-set
before and after correcting for batch effects.")
    (license license:artistic2.0)))

(define-public r-mbkmeans
  (package
    (name "r-mbkmeans")
    (version "1.20.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "mbkmeans" version))
              (sha256
               (base32
                "0fnf6ymbcfchhlcbwpwlcq1jc3lfzh9f8mf76xfy7vrpwngds1fs"))))
    (build-system r-build-system)
    (native-inputs
     (list r-knitr))
    (propagated-inputs
     (list r-beachmat
           r-benchmarkme
           r-biocparallel
           r-clusterr
           r-delayedarray
           r-matrix
           r-rcpp
           r-rcpparmadillo
           r-rhdf5lib
           r-s4vectors
           r-singlecellexperiment
           r-summarizedexperiment))
    (home-page "https://bioconductor.org/packages/mbkmeans")
    (synopsis "Mini-batch k-means clustering for single-cell RNA-seq")
    (description "This package implements the mini-batch k-means algorithm for
large datasets, including support for on-disk data representation.")
    (license license:expat)))

(define-public r-multibac
  (package
    (name "r-multibac")
    (version "1.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "MultiBaC" version))
       (sha256
        (base32 "0m08620sii9181y0vlcrw9nbcnp4vnjjxvp96iqlgg5fsb23wbl7"))))
    (properties `((upstream-name . "MultiBaC")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-ggplot2
           r-matrix
           r-multiassayexperiment
           r-pcamethods
           r-plotrix
           r-ropls))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/MultiBaC")
    (synopsis "Multiomic batch effect correction")
    (description
     "MultiBaC is a strategy to correct batch effects from multiomic datasets
distributed across different labs or data acquisition events.  MultiBaC is
able to remove batch effects across different omics generated within separate
batches provided that at least one common omic data type is included in all
the batches considered.")
    (license license:gpl3)))

(define-public r-multtest
  (package
    (name "r-multtest")
    (version "2.60.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "multtest" version))
       (sha256
        (base32
         "04dv5rcqwpv0im7j3hvcj8hjybgs4a384zgnjyqbcipcicd2rjz4"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biobase r-biocgenerics r-mass r-survival))
    (home-page "https://bioconductor.org/packages/multtest")
    (synopsis "Resampling-based multiple hypothesis testing")
    (description
     "This package can do non-parametric bootstrap and permutation
resampling-based multiple testing procedures (including empirical Bayes
methods) for controlling the family-wise error rate (FWER), generalized
family-wise error rate (gFWER), tail probability of the proportion of
false positives (TPPFP), and false discovery rate (FDR).  Several choices
of bootstrap-based null distribution are implemented (centered, centered
and scaled, quantile-transformed).  Single-step and step-wise methods are
available.  Tests based on a variety of T- and F-statistics (including
T-statistics based on regression parameters from linear and survival models
as well as those based on correlation parameters) are included.  When probing
hypotheses with T-statistics, users may also select a potentially faster null
distribution which is multivariate normal with mean zero and variance
covariance matrix derived from the vector influence function.  Results are
reported in terms of adjusted P-values, confidence regions and test statistic
cutoffs.  The procedures are directly applicable to identifying differentially
expressed genes in DNA microarray experiments.")
    (license license:lgpl3)))

(define-public r-graph
  (package
    (name "r-graph")
    (version "1.82.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "graph" version))
              (sha256
               (base32
                "1wa10s9a8p6lajim1lbw4rxyc4fz7y6fpymrh0c0ip85971cd9hv"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocgenerics))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/graph")
    (synopsis "Handle graph data structures in R")
    (description
     "This package implements some simple graph handling capabilities for R.")
    (license license:artistic2.0)))

;; This is a CRAN package, but it depends on a Bioconductor package.
(define-public r-ggm
  (package
    (name "r-ggm")
    (version "2.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ggm" version))
       (sha256
        (base32
         "1bp00m93mrx33gpd36qnrxx60y17aimlh6bd36cqyh6vpf0vpfvg"))))
    (properties `((upstream-name . "ggm")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocmanager r-graph r-igraph))
    (home-page "https://cran.r-project.org/package=ggm")
    (synopsis "Functions for graphical Markov models")
    (description
     "This package provides functions and datasets for maximum likelihood
fitting of some classes of graphical Markov models.")
    (license license:gpl2+)))

(define-public r-ggtreeextra
  (package
    (name "r-ggtreeextra")
    (version "1.14.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "ggtreeExtra" version))
              (sha256
               (base32
                "1wy229rnp356hi144flm9fvhv414jxmxc967f7pyqwl4abiwlmzm"))))
    (properties `((upstream-name . "ggtreeExtra")))
    (build-system r-build-system)
    (propagated-inputs (list r-cli
                             r-ggnewscale
                             r-ggplot2
                             r-ggtree
                             r-magrittr
                             r-rlang
                             r-tidytree))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/YuLab-SMU/ggtreeExtra/")
    (synopsis
     "Add geometric layers On circular or other layout tree of ggtree")
    (description
     "ggtreeExtra extends the method for mapping and visualizing associated
data on phylogenetic tree using ggtree.  These associated data can be
presented on the external panels to circular layout, fan layout, or other
rectangular layout tree built by ggtree with the grammar of ggplot2.")
    (license license:gpl3+)))

;; This is a CRAN package, but it depends on a bunch of Bioconductor packages.
(define-public r-ggpicrust2
  (package
    (name "r-ggpicrust2")
    (version "1.7.3")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "ggpicrust2" version))
              (sha256
               (base32
                "0zjlsvzl2f74fvqw4ijnai23qwhlcpgd5p8z9dclnwnsgdbm6hcq"))))
    (properties `((upstream-name . "ggpicrust2")))
    (build-system r-build-system)
    (propagated-inputs (list r-aldex2
                             r-aplot
                             r-circlize
                             r-deseq2
                             r-dplyr
                             r-edger
                             r-ggally
                             r-ggh4x
                             r-ggplot2
                             r-ggprism
                             r-lefser
                             r-limma
                             r-maaslin2
                             r-metagenomeseq
                             r-microbiomestat
                             r-patchwork
                             r-readr
                             r-summarizedexperiment
                             r-tibble
                             r-tidyr))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/cafferychen777/ggpicrust2")
    (synopsis "Make PICRUSt2 output analysis and visualization easier")
    (description
     "This package provides a convenient way to analyze and visualize PICRUSt2
output with pre-defined plots and functions.  It allows for generating
statistical plots about microbiome functional predictions and offers
customization options.  It features a one-click option for creating
publication-level plots, saving time and effort in producing
professional-grade figures.  It streamlines the PICRUSt2 analysis and
visualization process.")
    (license license:expat)))

;; This is a CRAN package, but it depends on a Bioconductor package, r-graph.
(define-public r-perfmeas
  (package
    (name "r-perfmeas")
    (version "1.2.5")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "PerfMeas" version))
       (sha256
        (base32
         "13yjk0kwpbsqwl056hzf0zj2br1mk4faqcn1whdfxmq348c14hjb"))))
    (properties `((upstream-name . "PerfMeas")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-graph r-limma r-rbgl))
    (home-page "https://cran.r-project.org/web/packages/PerfMeas/")
    (synopsis "Performance measures for ranking and classification tasks")
    (description
     "This package implements different performance measures for
classification and ranking tasks.  @dfn{Area under curve} (AUC), precision at
a given recall, F-score for single and multiple classes are available.")
    (license license:gpl2+)))

(define-public r-pepsnmr
  (package
    (name "r-pepsnmr")
    (version "1.22.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "PepsNMR" version))
              (sha256
               (base32
                "0z3kw8j972zmrcyikag5js20zpgc3l5bgkn5q31dzi1q7ynpkwhv"))))
    (properties `((upstream-name . "PepsNMR")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-ggplot2
           r-gridextra
           r-matrix
           r-matrixstats
           r-ptw
           r-reshape2))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/ManonMartin/PepsNMR")
    (synopsis "Pre-process 1H-NMR FID signals")
    (description
     "This package provides R functions for common pre-processing steps that
are applied on @acronym{1H-NMR, proton nuclear magnetic resonance} data.  It
also provides a function to read the @acronym{FID, free induction decay}
signals directly in the Bruker format.")
    (license license:gpl2)))

;; This is a CRAN package, but it depends on a Bioconductor package.
(define-public r-codedepends
  (package
    (name "r-codedepends")
    (version "0.6.6")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "CodeDepends" version))
       (sha256
        (base32
         "0msq5ksjqp2pjzl6f1vizw53gdf5lz520bpdr5dbjwz5mc1mp0bz"))))
    (properties `((upstream-name . "CodeDepends")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-codetools r-graph r-xml))
    (native-inputs (list r-knitr))
    (home-page "https://cran.r-project.org/web/packages/CodeDepends")
    (synopsis "Analysis of R code for reproducible research and code comprehension")
    (description
     "This package provides tools for analyzing R expressions or blocks of
code and determining the dependencies between them.  It focuses on R scripts,
but can be used on the bodies of functions.  There are many facilities
including the ability to summarize or get a high-level view of code,
determining dependencies between variables, code improvement suggestions.")
    ;; Any version of the GPL
    (license (list license:gpl2+ license:gpl3+))))

(define-public r-chippeakanno
  (package
    (name "r-chippeakanno")
    (version "3.38.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ChIPpeakAnno" version))
       (sha256
        (base32
         "1g7p53g91xx3x10kad2f190bmg10ryp928m0q5wxz9n8lycb5rgl"))))
    (properties `((upstream-name . "ChIPpeakAnno")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotationdbi
           r-biocgenerics
           r-biomart
           r-biostrings
           r-data-table
           r-dbi
           r-dplyr
           r-ensembldb
           r-genomeinfodb
           r-genomicalignments
           r-genomicfeatures
           r-genomicranges
           r-ggplot2
           r-graph
           r-interactionset
           r-iranges
           r-keggrest
           r-matrixstats
           r-multtest
           r-pwalign
           r-rbgl
           r-regioner
           r-rsamtools
           r-rtracklayer
           r-s4vectors
           r-scales
           r-stringr
           r-summarizedexperiment
           r-tibble
           r-tidyr
           r-universalmotif
           r-venndiagram))
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/ChIPpeakAnno")
    (synopsis "Peaks annotation from ChIP-seq and ChIP-chip experiments")
    (description
     "The package includes functions to retrieve the sequences around the peak,
obtain enriched Gene Ontology (GO) terms, find the nearest gene, exon, miRNA or
custom features such as most conserved elements and other transcription factor
binding sites supplied by users.  Starting 2.0.5, new functions have been added
for finding the peaks with bi-directional promoters with summary statistics
(peaksNearBDP), for summarizing the occurrence of motifs in peaks
(summarizePatternInPeaks) and for adding other IDs to annotated peaks or
enrichedGO (addGeneIDs).")
    (license license:gpl2+)))

(define-public r-matrixgenerics
  (package
   (name "r-matrixgenerics")
   (version "1.16.0")
   (source (origin
            (method url-fetch)
            (uri (bioconductor-uri "MatrixGenerics" version))
            (sha256
             (base32
              "1r7n5p89460h47mnfh190184dbcmrbj0j5d7dswkfwh6ixan3zyc"))))
   (properties
    `((upstream-name . "MatrixGenerics")))
   (build-system r-build-system)
   (propagated-inputs
    (list r-matrixstats))
   (home-page "https://bioconductor.org/packages/MatrixGenerics")
   (synopsis "S4 generic summary statistic functions for matrix-like objects")
   (description
    "This package provides S4 generic functions modeled after the
@code{matrixStats} API for alternative matrix implementations.  Packages with
alternative matrix implementation can depend on this package and implement the
generic functions that are defined here for a useful set of row and column
summary statistics.  Other package developers can import this package and
handle a different matrix implementations without worrying about
incompatibilities.")
   (license license:artistic2.0)))

(define-public r-marray
  (package
    (name "r-marray")
    (version "1.82.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "marray" version))
              (sha256
               (base32 "1h96aq92bad69ycrmrvxwdz8dg9rk44925gj83gxzqypw88j2x0y"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-limma))
    (home-page "https://bioconductor.org/packages/marray")
    (synopsis "Exploratory analysis for two-color spotted microarray data")
    (description "This package contains class definitions for two-color spotted
microarray data.  It also includes functions for data input, diagnostic plots,
normalization and quality checking.")
    (license license:lgpl2.0+)))

(define-public r-cghbase
  (package
   (name "r-cghbase")
   (version "1.64.0")
   (source (origin
            (method url-fetch)
            (uri (bioconductor-uri "CGHbase" version))
            (sha256
             (base32 "1fz39fdkfvfhzm69pfvliqvp48wsldwm3kpgbsaacrdh46jg5vzk"))))
   (properties `((upstream-name . "CGHbase")))
   (build-system r-build-system)
   (propagated-inputs
    (list r-biobase r-marray))
   (home-page "https://bioconductor.org/packages/CGHbase")
   (synopsis "Base functions and classes for arrayCGH data analysis")
   (description "This package contains functions and classes that are needed by
the @code{arrayCGH} packages.")
   (license license:gpl2+)))

(define-public r-cghcall
  (package
   (name "r-cghcall")
   (version "2.66.0")
   (source (origin
            (method url-fetch)
            (uri (bioconductor-uri "CGHcall" version))
            (sha256
             (base32 "1gjd3ggiw93cwcacjb1472ia98aaapj9gln04v5pbwmp2r4xfcs7"))))
   (properties `((upstream-name . "CGHcall")))
   (build-system r-build-system)
   (propagated-inputs
    (list r-biobase r-cghbase r-dnacopy r-impute r-snowfall))
   (home-page "https://bioconductor.org/packages/CGHcall")
   (synopsis "Base functions and classes for arrayCGH data analysis")
   (description "This package contains functions and classes that are needed by
@code{arrayCGH} packages.")
   (license license:gpl2+)))

(define-public r-qdnaseq
  (package
    (name "r-qdnaseq")
    (version "1.40.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "QDNAseq" version))
              (sha256
               (base32 "1myk6qgl45ya7qw6sx6xc98d8m5ffl2ghm3zvqmyh9r0sp6x184f"))))
    (properties `((upstream-name . "QDNAseq")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biobase
           r-cghbase
           r-cghcall
           r-dnacopy
           r-future-apply
           r-genomicranges
           r-iranges
           r-matrixstats
           r-r-utils
           r-rsamtools))
    (home-page "https://bioconductor.org/packages/QDNAseq")
    (synopsis "Quantitative DNA sequencing for chromosomal aberrations")
    (description "The genome is divided into non-overlapping fixed-sized bins,
number of sequence reads in each counted, adjusted with a simultaneous
two-dimensional loess correction for sequence mappability and GC content, and
filtered to remove spurious regions in the genome.  Downstream steps of
segmentation and calling are also implemented via packages DNAcopy and CGHcall,
respectively.")
    (license license:gpl2+)))

(define-public r-bayseq
  (package
    (name "r-bayseq")
    (version "2.38.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "baySeq" version))
       (sha256
        (base32
         "0nlkynzijxl6f8983vdpmcssvrpjf6s42zx3lxfwxwlhrvz1yg9l"))))
    (properties `((upstream-name . "baySeq")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-abind r-edger r-genomicranges))
    (home-page "https://bioconductor.org/packages/baySeq/")
    (synopsis "Bayesian analysis of differential expression patterns in count data")
    (description
     "This package identifies differential expression in high-throughput count
data, such as that derived from next-generation sequencing machines,
calculating estimated posterior likelihoods of differential expression (or
more complex hypotheses) via empirical Bayesian methods.")
    (license license:gpl3)))

(define-public r-chipcomp
  (package
    (name "r-chipcomp")
    (version "1.34.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ChIPComp" version))
       (sha256
        (base32
         "013lmgdkj596gqzm0nlxjqpv0w94hap9vndp5znqpk4inq88gj8m"))))
    (properties `((upstream-name . "ChIPComp")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocgenerics
           r-bsgenome-hsapiens-ucsc-hg19
           r-bsgenome-mmusculus-ucsc-mm9
           r-genomeinfodb
           r-genomicranges
           r-iranges
           r-limma
           r-rsamtools
           r-rtracklayer
           r-s4vectors))
    (home-page "https://bioconductor.org/packages/ChIPComp")
    (synopsis "Quantitative comparison of multiple ChIP-seq datasets")
    (description
     "ChIPComp implements a statistical method for quantitative comparison of
multiple ChIP-seq datasets.  It detects differentially bound sharp binding
sites across multiple conditions considering matching control in ChIP-seq
datasets.")
    ;; Any version of the GPL.
    (license license:gpl3+)))

(define-public r-riboprofiling
  (package
    (name "r-riboprofiling")
    (version "1.34.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "RiboProfiling" version))
       (sha256
        (base32
         "0h0sxmg4knsarh7w7dnp539cs9sfxjw4413q1yyyz3gbi23bzww4"))))
    (properties `((upstream-name . "RiboProfiling")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocgenerics
           r-biostrings
           r-data-table
           r-genomeinfodb
           r-genomicalignments
           r-genomicfeatures
           r-genomicranges
           r-ggbio
           r-ggplot2
           r-iranges
           r-plyr
           r-reshape2
           r-rsamtools
           r-rtracklayer
           r-s4vectors
           r-sqldf))
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/RiboProfiling/")
    (synopsis "Ribosome profiling data analysis")
    (description "Starting with a BAM file, this package provides the
necessary functions for quality assessment, read start position recalibration,
the counting of genomic sequence reads on CDS, 3'UTR, and 5'UTR, and plotting
of count data: pairs, log fold-change, codon frequency and coverage
assessment, principal component analysis on codon coverage.")
    (license license:gpl3)))

(define-public r-riboseqr
  (package
    (name "r-riboseqr")
    (version "1.38.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "riboSeqR" version))
       (sha256
        (base32
         "0p0q28n8hlai80pxxns9pzzg552pbwlxlph8zjf78bwbjagrfbz5"))))
    (properties `((upstream-name . "riboSeqR")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-abind
           r-bayseq
           r-genomeinfodb
           r-genomicranges
           r-iranges
           r-rsamtools
           r-s4vectors
           r-seqlogo))
    (home-page "https://bioconductor.org/packages/riboSeqR/")
    (synopsis "Analysis of sequencing data from ribosome profiling experiments")
    (description
     "This package provides plotting functions, frameshift detection and
parsing of genetic sequencing data from ribosome profiling experiments.")
    (license license:gpl3)))

(define-public r-interactionset
  (package
    (name "r-interactionset")
    (version "1.32.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "InteractionSet" version))
       (sha256
        (base32
         "0ikkmhz6grmhksmyypj9hqmblls4cgww0dx4kzg4cy5bpavsq1hs"))))
    (properties
     `((upstream-name . "InteractionSet")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocgenerics
           r-genomeinfodb
           r-genomicranges
           r-iranges
           r-matrix
           r-rcpp
           r-s4vectors
           r-summarizedexperiment))
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/InteractionSet")
    (synopsis "Base classes for storing genomic interaction data")
    (description
     "This package provides the @code{GInteractions},
@code{InteractionSet} and @code{ContactMatrix} objects and associated methods
for storing and manipulating genomic interaction data from Hi-C and ChIA-PET
experiments.")
    (license license:gpl3)))

(define-public r-genomicinteractions
  (package
    (name "r-genomicinteractions")
    (version "1.38.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "GenomicInteractions" version))
       (sha256
        (base32
         "16aaaa4nr1y5ng1hzw87q950clid28iqf4gkypcpihg3cz69axxg"))))
    (properties
     `((upstream-name . "GenomicInteractions")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biobase
           r-biocgenerics
           r-data-table
           r-dplyr
           r-genomeinfodb
           r-genomicranges
           r-ggplot2
           r-gridextra
           r-gviz
           r-igraph
           r-interactionset
           r-iranges
           r-rsamtools
           r-rtracklayer
           r-s4vectors
           r-stringr))
    (native-inputs
     (list r-knitr))
    (home-page "https://github.com/ComputationalRegulatoryGenomicsICL/GenomicInteractions/")
    (synopsis "R package for handling genomic interaction data")
    (description
     "This R package provides tools for handling genomic interaction data,
such as ChIA-PET/Hi-C, annotating genomic features with interaction
information and producing various plots and statistics.")
    (license license:gpl3)))

(define-public r-ctc
  (package
    (name "r-ctc")
    (version "1.78.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ctc" version))
       (sha256
        (base32
         "09whaam2r30dq8y9r6vvc0r4sisz78c921bgx7wj6cmn2nvpgf9q"))))
    (build-system r-build-system)
    (propagated-inputs (list r-amap))
    (home-page "https://bioconductor.org/packages/ctc/")
    (synopsis "Cluster and tree conversion")
    (description
     "This package provides tools for exporting and importing classification
trees and clusters to other programs.")
    (license license:gpl2)))

(define-public r-goseq
  (package
    (name "r-goseq")
    (version "1.56.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "goseq" version))
       (sha256
        (base32
         "0bc7dcpz0agbc42f8agf05amyhpavz55fdp622if5af5dn1ww6pr"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotationdbi
           r-biasedurn
           r-biocgenerics
           r-genelendatabase
           r-go-db
           r-mgcv))
    (home-page "https://bioconductor.org/packages/goseq/")
    (synopsis "Gene Ontology analyser for RNA-seq and other length biased data")
    (description
     "This package provides tools to detect Gene Ontology and/or other user
defined categories which are over/under represented in RNA-seq data.")
    (license license:lgpl2.0+)))

(define-public r-glimma
  (package
    (name "r-glimma")
    (version "2.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Glimma" version))
       (sha256
        (base32
         "1d7rq3irp3n0fabfgjs2x2c9lscz70a38qf3wbxvi417qn7fpyla"))
       (modules '((guix build utils)))
       (snippet
        '(with-directory-excursion "inst/htmlwidgets/lib/"
           (for-each delete-file
                     ;; XXX: we keep inst/v1/js/glimma.min.js because
                     ;; it's not clear how to build it.
                     (cons "vega/vega.min.js"
                           (find-files "datatables"
                                       "\\.min\\.js$")))))))
    (properties `((upstream-name . "Glimma")))
    (build-system r-build-system)
    (arguments
     (list
      #:modules '((guix build utils)
                  (guix build r-build-system)
                  (srfi srfi-1))
      #:phases
      '(modify-phases %standard-phases
         (add-after 'unpack 'process-javascript
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "inst/htmlwidgets/lib/"
               (let ((files (list "datatables/Buttons-1.6.1/js/buttons.bootstrap.js"
                                  "datatables/Buttons-1.6.1/js/buttons.bootstrap4.js"
                                  "datatables/Buttons-1.6.1/js/buttons.colVis.js"
                                  "datatables/Buttons-1.6.1/js/buttons.flash.js"
                                  "datatables/Buttons-1.6.1/js/buttons.foundation.js"
                                  "datatables/Buttons-1.6.1/js/buttons.html5.js"
                                  "datatables/Buttons-1.6.1/js/buttons.jqueryui.js"
                                  "datatables/Buttons-1.6.1/js/buttons.print.js"
                                  "datatables/Buttons-1.6.1/js/buttons.semanticui.js"
                                  "datatables/Buttons-1.6.1/js/dataTables.buttons.js"
                                  "datatables/DataTables-1.10.20/js/dataTables.bootstrap.js"
                                  "datatables/DataTables-1.10.20/js/dataTables.bootstrap4.js"
                                  "datatables/DataTables-1.10.20/js/dataTables.foundation.js"
                                  "datatables/DataTables-1.10.20/js/dataTables.jqueryui.js"
                                  "datatables/DataTables-1.10.20/js/dataTables.semanticui.js"
                                  "datatables/DataTables-1.10.20/js/jquery.dataTables.js"
                                  "datatables/JSZip-2.5.0/jszip.js"
                                  "datatables/Scroller-2.0.1/js/dataTables.scroller.js"
                                  "datatables/Scroller-2.0.1/js/scroller.bootstrap.js"
                                  "datatables/Scroller-2.0.1/js/scroller.bootstrap4.js"
                                  "datatables/Scroller-2.0.1/js/scroller.foundation.js"
                                  "datatables/Scroller-2.0.1/js/scroller.jqueryui.js"
                                  "datatables/Scroller-2.0.1/js/scroller.semanticui.js"
                                  "datatables/datatables.js"
                                  "datatables/jQuery-1.12.4/jquery-1.12.4.js"
                                  "vega/vega.js")))
                 (for-each (lambda (source)
                             (let ((target (string-append (basename source ".js") ".min.js")))
                               (format #true "Processing ~a --> ~a~%"
                                       source target)
                               (invoke "esbuild" source "--minify"
                                       (string-append "--outfile=" target))))
                           files))))))))
    (propagated-inputs
     (list r-deseq2
           r-edger
           r-htmlwidgets
           r-jsonlite
           r-limma
           r-s4vectors
           r-summarizedexperiment))
    (native-inputs
     (list esbuild r-knitr))
    (home-page "https://github.com/Shians/Glimma")
    (synopsis "Interactive HTML graphics")
    (description
     "This package generates interactive visualisations for analysis of
RNA-sequencing data using output from limma, edgeR or DESeq2 packages in an
HTML page.  The interactions are built on top of the popular static
representations of analysis results in order to provide additional
information.")
    (license license:lgpl3)))

(define-public r-glmgampoi
  (package
    (name "r-glmgampoi")
    (version "1.16.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "glmGamPoi" version))
              (sha256
               (base32
                "1my8v0zx5xsxjf8dj2yw92b8zp725hxhb2vrl5ccgz2w9xkn0dkj"))))
    (properties `((upstream-name . "glmGamPoi")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-beachmat
           r-biocgenerics
           r-delayedarray
           r-delayedmatrixstats
           r-hdf5array
           r-matrixgenerics
           r-matrixstats
           r-rcpp
           r-rcpparmadillo
           r-rlang
           r-singlecellexperiment
           r-summarizedexperiment
           r-vctrs))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/const-ae/glmGamPoi")
    (synopsis "Fit a Gamma-Poisson Generalized Linear Model")
    (description
     "Fit linear models to overdispersed count data.  The package can estimate
the overdispersion and fit repeated models for matrix input.  It is designed
to handle large input datasets as they typically occur in single cell RNA-seq
experiments.")
    (license license:gpl3)))

(define-public r-rots
  (package
    (name "r-rots")
    (version "1.32.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ROTS" version))
       (sha256
        (base32
         "1s9hr64gpz08j638wlglcmk6c8yyj1hzrgddiphklkapc1asr320"))))
    (properties `((upstream-name . "ROTS")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biobase r-rcpp))
    (home-page "https://bioconductor.org/packages/ROTS/")
    (synopsis "Reproducibility-Optimized Test Statistic")
    (description
     "This package provides tools for calculating the
@dfn{Reproducibility-Optimized Test Statistic} (ROTS) for differential testing
in omics data.")
    (license license:gpl2+)))

(define-public r-plgem
  (package
    (name "r-plgem")
    (version "1.76.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "plgem" version))
       (sha256
        (base32
         "0my4jpxw3xy9n40cqvdaz6ay7gpwshn2iqxr43knrk0ckpyxx2cr"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biobase r-mass))
    (home-page "https://www.genopolis.it")
    (synopsis "Detect differential expression in microarray and proteomics datasets")
    (description
     "The Power Law Global Error Model (PLGEM) has been shown to faithfully
model the variance-versus-mean dependence that exists in a variety of
genome-wide datasets, including microarray and proteomics data.  The use of
PLGEM has been shown to improve the detection of differentially expressed
genes or proteins in these datasets.")
    (license license:gpl2)))

(define-public r-plyranges
  (package
    (name "r-plyranges")
    (version "1.24.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "plyranges" version))
              (sha256
               (base32
                "0rh5l6bd80nhffqpnnc2c00mvp8p32ykld05h94c3y8c9mvmsl58"))))
    (properties `((upstream-name . "plyranges")))
    (build-system r-build-system)
    (propagated-inputs (list r-biocgenerics
                             r-dplyr
                             r-genomeinfodb
                             r-genomicalignments
                             r-genomicranges
                             r-iranges
                             r-magrittr
                             r-rlang
                             r-rsamtools
                             r-rtracklayer
                             r-s4vectors
                             r-tidyselect))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/plyranges")
    (synopsis "Fluent interface for manipulating GenomicRanges")
    (description
     "This package provides a dplyr-like interface for interacting with the
common Bioconductor classes @code{Ranges} and @code{GenomicRanges}.  By
providing a grammatical and consistent way of manipulating these classes their
accessiblity for new Bioconductor users is hopefully increased.")
    (license license:artistic2.0)))

(define-public r-inspect
  (package
    (name "r-inspect")
    (version "1.34.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "INSPEcT" version))
       (sha256
        (base32
         "0147ss218d3dnqbdgfwsg24an63qln8l5kkv0y4hm56bncdrm1i0"))))
    (properties `((upstream-name . "INSPEcT")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biobase
           r-biocgenerics
           r-biocparallel
           r-deseq2
           r-desolve
           r-genomeinfodb
           r-genomicalignments
           r-genomicfeatures
           r-genomicranges
           r-iranges
           r-kernsmooth
           r-plgem
           r-proc
           r-readxl
           r-rootsolve
           r-rsamtools
           r-rtracklayer
           r-s4vectors
           r-shiny
           r-summarizedexperiment
           r-txdb-mmusculus-ucsc-mm9-knowngene))
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/INSPEcT")
    (synopsis "Analysis of 4sU-seq and RNA-seq time-course data")
    (description
     "INSPEcT (INference of Synthesis, Processing and dEgradation rates in
Time-Course experiments) analyses 4sU-seq and RNA-seq time-course data in
order to evaluate synthesis, processing and degradation rates and assess via
modeling the rates that determines changes in mature mRNA levels.")
    (license license:gpl2)))

(define-public r-dnabarcodes
  (package
    (name "r-dnabarcodes")
    (version "1.34.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "DNABarcodes" version))
       (sha256
        (base32
         "0vxxg6fkgck640p9plk4vx3d05794s3xgxksv819d2wicvyby4hk"))))
    (properties `((upstream-name . "DNABarcodes")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-bh r-matrix r-rcpp))
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/DNABarcodes")
    (synopsis "Create and analyze DNA barcodes")
    (description
     "This package offers tools to create DNA barcode sets capable of
correcting insertion, deletion, and substitution errors.  Existing barcodes
can be analyzed regarding their minimal, maximal and average distances between
barcodes.  Finally, reads that start with a (possibly mutated) barcode can be
demultiplexed, i.e. assigned to their original reference barcode.")
    (license license:gpl2)))

(define-public r-ruvseq
  (package
    (name "r-ruvseq")
    (version "1.38.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "RUVSeq" version))
       (sha256
        (base32
         "0i91kjwransy6l217rmxfzfchb43h1sjk8ygf0y8a3rv6rp7ylfd"))))
    (properties `((upstream-name . "RUVSeq")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biobase r-edaseq r-edger r-mass))
    (native-inputs
     (list r-knitr))
    (home-page "https://github.com/drisso/RUVSeq")
    (synopsis "Remove unwanted variation from RNA-Seq data")
    (description
     "This package implements methods to @dfn{remove unwanted variation} (RUV)
of Risso et al. (2014) for the normalization of RNA-Seq read counts between
samples.")
    (license license:artistic2.0)))

(define-public r-biocneighbors
  (package
    (name "r-biocneighbors")
    (version "1.22.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BiocNeighbors" version))
       (sha256
        (base32
         "1krg4cs406ymk27qcj3j1jsl8vl4j9k5zvwf4vzxgbw3x6lpywid"))))
    (properties `((upstream-name . "BiocNeighbors")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocparallel r-matrix r-rcpp r-rcpphnsw r-s4vectors))
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/BiocNeighbors")
    (synopsis "Nearest Neighbor Detection for Bioconductor packages")
    (description
     "This package implements exact and approximate methods for nearest
neighbor detection, in a framework that allows them to be easily switched
within Bioconductor packages or workflows.  The exact algorithm is implemented
using pre-clustering with the k-means algorithm.  Functions are also provided
to search for all neighbors within a given distance.  Parallelization is
achieved for all methods using the BiocParallel framework.")
    (license license:gpl3)))

(define-public r-scaledmatrix
  (package
    (name "r-scaledmatrix")
    (version "1.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ScaledMatrix" version))
       (sha256
        (base32
         "0c6sp5nps31ygrr7lxxc2gbg19qsk107v23x8f6acqk1xwlsfppi"))))
    (properties `((upstream-name . "ScaledMatrix")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-delayedarray r-matrix r-s4vectors))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/LTLA/ScaledMatrix")
    (synopsis "Create a DelayedMatrix of scaled and centered values")
    (description
     "This package provides delayed computation of a matrix of scaled and
centered values.  The result is equivalent to using the @code{scale} function
but avoids explicit realization of a dense matrix during block processing.
This permits greater efficiency in common operations, most notably matrix
multiplication.")
    (license license:gpl3)))

(define-public r-treeio
  (package
    (name "r-treeio")
    (version "1.28.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "treeio" version))
       (sha256
        (base32
         "0xfjq13yirfz7dzzlf2m60lk8xb3qh6k5hr66ygvjnm716x3vyh1"))))
    (properties `((upstream-name . "treeio")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-ape
           r-dplyr
           r-jsonlite
           r-magrittr
           r-rlang
           r-tibble
           r-tidytree
           r-yulab-utils))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/YuLab-SMU/treeio")
    (synopsis "Base classes and functions for Phylogenetic tree input and output")
    (description
     "This is an R package to make it easier to import and store phylogenetic
trees with associated data; and to link external data from different sources
to phylogeny.  It also supports exporting phylogenetic trees with
heterogeneous associated data to a single tree file and can be served as a
platform for merging tree with associated data and converting file formats.")
    (license license:artistic2.0)))

(define-public r-treesummarizedexperiment
  (package
    (name "r-treesummarizedexperiment")
    (version "2.12.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "TreeSummarizedExperiment" version))
              (sha256
               (base32
                "0ygzw9nba5pxgrbib8sm7qwhzzm8m038banb0z7n42vkb1cb3d5i"))))
    (properties `((upstream-name . "TreeSummarizedExperiment")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-ape
           r-biocgenerics
           r-biocparallel
           r-biostrings
           r-dplyr
           r-iranges
           r-rlang
           r-s4vectors
           r-singlecellexperiment
           r-summarizedexperiment
           r-treeio))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/TreeSummarizedExperiment")
    (synopsis "S4 class for data with tree structures")
    (description
     "@code{TreeSummarizedExperiment} extends @code{SingleCellExperiment} to
include hierarchical information on the rows or columns of the rectangular
data.")
    (license license:gpl2+)))

(define-public r-ggtree
  (package
    (name "r-ggtree")
    (version "3.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ggtree" version))
       (sha256
        (base32
         "1bm7c46k8qbajjvl8475b7fcd0dvg9s1bir8313qgm466qm6immh"))))
    (properties `((upstream-name . "ggtree")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-ape
           r-aplot
           r-cli
           r-dplyr
           r-ggfun
           r-ggplot2
           r-magrittr
           r-purrr
           r-rlang
           r-scales
           r-tidyr
           r-tidytree
           r-treeio
           r-yulab-utils))
    (native-inputs (list r-knitr))
    (home-page "https://yulab-smu.top/treedata-book/")
    (synopsis "R package for visualization of trees and annotation data")
    (description
     "This package extends the ggplot2 plotting system which implements a
grammar of graphics.  ggtree is designed for visualization and annotation of
phylogenetic trees and other tree-like structures with their annotation
data.")
    (license license:artistic2.0)))

(define-public r-metapod
  (package
    (name "r-metapod")
    (version "1.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "metapod" version))
       (sha256
        (base32
         "100wsg2y7069r7xk5bsk5n4n8dqvpbk297w5ishnijrvxhbs9f8n"))))
    (properties `((upstream-name . "metapod")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-rcpp))
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/metapod")
    (synopsis "Meta-analyses on p-values of differential analyses")
    (description
     "This package implements a variety of methods for combining p-values in
differential analyses of genome-scale datasets.  Functions can combine
p-values across different tests in the same analysis (e.g., genomic windows in
ChIP-seq, exons in RNA-seq) or for corresponding tests across separate
analyses (e.g., replicated comparisons, effect of different treatment
conditions).  Support is provided for handling log-transformed input p-values,
missing values and weighting where appropriate.")
    (license license:gpl3)))

(define-public r-biocsingular
  (package
    (name "r-biocsingular")
    (version "1.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BiocSingular" version))
       (sha256
        (base32
         "1xl9wn3slj3ywjglb8zq2p0ifx4xwxbqsm99fjlf6hanfhmnxyay"))))
    (properties `((upstream-name . "BiocSingular")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-beachmat
           r-biocgenerics
           r-biocparallel
           r-delayedarray
           r-irlba
           r-matrix
           r-rcpp
           r-rsvd
           r-s4vectors
           r-scaledmatrix))
    (native-inputs
     (list r-knitr))
    (home-page "https://github.com/LTLA/BiocSingular")
    (synopsis "Singular value decomposition for Bioconductor packages")
    (description
     "This package implements exact and approximate methods for singular value
decomposition and principal components analysis, in a framework that allows
them to be easily switched within Bioconductor packages or workflows.  Where
possible, parallelization is achieved using the BiocParallel framework.")
    (license license:gpl3)))

(define-public r-destiny
  (package
    (name "r-destiny")
    (version "3.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "destiny" version))
       (sha256
        (base32
         "0w7j6xh0gzhahk1r8w2wgnylq5v476jzz0g14yjmszfkzm10va3h"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biobase
           r-biocgenerics
           r-ggplot-multistats
           r-ggplot2
           r-ggthemes
           r-irlba
           r-knn-covertree
           r-matrix
           r-pcamethods
           r-proxy
           r-rcpp
           r-rcppeigen
           r-rcpphnsw
           r-rlang
           r-rspectra
           r-scales
           r-scatterplot3d
           r-singlecellexperiment
           r-smoother
           r-summarizedexperiment
           r-tidyr
           r-tidyselect
           r-vim))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/destiny/")
    (synopsis "Create and plot diffusion maps")
    (description "This package provides tools to create and plot diffusion
maps.")
    ;; Any version of the GPL
    (license license:gpl3+)))

(define-public r-savr
  (package
    (name "r-savr")
    (version "1.37.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "savR" version))
       (sha256
        (base32
         "13wiw7isdmpwhynxjrhimy9yglcz1108k7nwa98vq9czq49zpada"))))
    (properties `((upstream-name . "savR")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-ggplot2 r-gridextra r-reshape2 r-scales r-xml))
    (home-page "https://github.com/bcalder/savR")
    (synopsis "Parse and analyze Illumina SAV files")
    (description
     "This package provides tools to parse Illumina Sequence Analysis
Viewer (SAV) files, access data, and generate QC plots.")
    (license license:agpl3+)))

(define-public r-chipexoqual
  (package
    (name "r-chipexoqual")
    (version "1.28.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ChIPexoQual" version))
       (sha256
        (base32
         "0y39fpbnykqf5ycpi9q3rw73dkn2ksll7n6nafimxvxsh463d8ky"))))
    (properties `((upstream-name . "ChIPexoQual")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocparallel
           r-biovizbase
           r-broom
           r-data-table
           r-dplyr
           r-genomeinfodb
           r-genomicalignments
           r-genomicranges
           r-ggplot2
           r-hexbin
           r-iranges
           r-rcolorbrewer
           r-rmarkdown
           r-rsamtools
           r-s4vectors
           r-scales
           r-viridis))
    (native-inputs
     (list r-knitr))
    (home-page "https://github.com/keleslab/ChIPexoQual")
    (synopsis "Quality control pipeline for ChIP-exo/nexus data")
    (description
     "This package provides a quality control pipeline for ChIP-exo/nexus
sequencing data.")
    (license license:gpl2+)))

(define-public r-copynumber
  (package
    (name "r-copynumber")
    (version "1.38.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "copynumber" version))
              (sha256
               (base32
                "1a664bllaq9pbb5cpd01j919qirylvnm8qd49lwlz89jvqjdri19"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-s4vectors r-iranges r-genomicranges r-biocgenerics))
    (home-page "https://bioconductor.org/packages/copynumber")
    (synopsis "Segmentation of single- and multi-track copy number data")
    (description
     "This package segments single- and multi-track copy number data by a
penalized least squares regression method.")
    (license license:artistic2.0)))

(define-public r-dnacopy
  (package
    (name "r-dnacopy")
    (version "1.78.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "DNAcopy" version))
       (sha256
        (base32
         "1q4s6rb6iv9cmfkjymgdyb4h3z0x0kzzd2bvfdjrzzzza6vhjpxl"))))
    (properties `((upstream-name . "DNAcopy")))
    (build-system r-build-system)
    (native-inputs (list gfortran))
    (home-page "https://bioconductor.org/packages/DNAcopy")
    (synopsis "DNA copy number data analysis")
    (description
     "This package implements the @dfn{circular binary segmentation} (CBS)
algorithm to segment DNA copy number data and identify genomic regions with
abnormal copy number.")
    (license license:gpl2+)))

;; This is a CRAN package, but it uncharacteristically depends on a
;; Bioconductor package.
(define-public r-htscluster
  (package
    (name "r-htscluster")
    (version "2.0.11")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "HTSCluster" version))
       (sha256
        (base32
         "0x9shhyla9bldkkh367gfdmf0k72l1ppixb8gzsa6nf8jx8qdpbp"))))
    (properties `((upstream-name . "HTSCluster")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-capushe r-edger r-plotrix))
    (home-page "https://cran.r-project.org/web/packages/HTSCluster")
    (synopsis "Clustering high-throughput transcriptome sequencing (HTS) data")
    (description
     "This package provides a Poisson mixture model is implemented to cluster
genes from high-throughput transcriptome sequencing (RNA-seq) data.  Parameter
estimation is performed using either the EM or CEM algorithm, and the slope
heuristics are used for model selection (i.e., to choose the number of
clusters).")
    (license license:gpl3+)))

(define-public r-deds
  (package
    (name "r-deds")
    (version "1.60.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "DEDS" version))
       (sha256
        (base32
         "0vzsmah2lhxf8k6n4d0i4j609sbvygmb6ii2ridg9z3nskwkrhp8"))))
    (properties `((upstream-name . "DEDS")))
    (build-system r-build-system)
    (home-page "https://bioconductor.org/packages/DEDS/")
    (synopsis "Differential expression via distance summary for microarray data")
    (description
     "This library contains functions that calculate various statistics of
differential expression for microarray data, including t statistics, fold
change, F statistics, SAM, moderated t and F statistics and B statistics.  It
also implements a new methodology called DEDS (Differential Expression via
Distance Summary), which selects differentially expressed genes by integrating
and summarizing a set of statistics using a weighted distance approach.")
    ;; Any version of the LGPL.
    (license license:lgpl3+)))

;; This is a CRAN package, but since it depends on a Bioconductor package we
;; put it here.
(define-public r-nbpseq
  (package
    (name "r-nbpseq")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "NBPSeq" version))
       (sha256
        (base32
         "07mnnk4n0cyksp1mw36y6369is62kxsfg3wb8d3dwswycdmj8m14"))))
    (properties `((upstream-name . "NBPSeq")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-qvalue))
    (home-page "https://cran.r-project.org/web/packages/NBPSeq")
    (synopsis "Negative binomial models for RNA-Seq data")
    (description
     "This package provides negative binomial models for two-group comparisons
and regression inferences from RNA-sequencing data.")
    (license license:gpl2)))

(define-public r-ebseq
  (package
    (name "r-ebseq")
    (version "2.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "EBSeq" version))
       (sha256
        (base32
         "1lbvrw6fcjhrm6v71lmnphv4hbdh03czgx3822ckdk6xrvyh54cf"))))
    (properties `((upstream-name . "EBSeq")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-bh
           r-blockmodeling
           r-gplots
           r-rcpp
           r-rcppeigen
           r-testthat))
    (home-page "https://bioconductor.org/packages/EBSeq")
    (synopsis "Differential expression analysis of RNA-seq data")
    (description
     "This package provides tools for differential expression analysis at both
gene and isoform level using RNA-seq data")
    (license license:artistic2.0)))

(define-public r-karyoploter
  (package
    (name "r-karyoploter")
    (version "1.30.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "karyoploteR" version))
              (sha256
               (base32
                "1g4pms5dcr4cdb47gr9kw3mc06v890b38zrsapd4x1cqhgfbgjyg"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotationdbi
           r-bamsignals
           r-bezier
           r-biovizbase
           r-digest
           r-genomeinfodb
           r-genomicfeatures
           r-genomicranges
           r-iranges
           r-memoise
           r-regioner
           r-rsamtools
           r-rtracklayer
           r-s4vectors
           r-variantannotation))
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/karyoploteR/")
    (synopsis "Plot customizable linear genomes displaying arbitrary data")
    (description "This package creates karyotype plots of arbitrary genomes and
offers a complete set of functions to plot arbitrary data on them.  It mimics
many R base graphics functions coupling them with a coordinate change function
automatically mapping the chromosome and data coordinates into the plot
coordinates.")
    (license license:artistic2.0)))

(define-public r-lpsymphony
  (package
    (name "r-lpsymphony")
    (version "1.32.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "lpsymphony" version))
       (sha256
        (base32
         "1wcjmw655k389ph61v3gxjv7gk2q70h25zmdzvd4ggbxg8vh2mp1"))))
    (build-system r-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'make-build-order-reproducible
            (lambda _
              (substitute* '("src/SYMPHONY/Cgl/configure.ac"
                             "src/SYMPHONY/Cgl/configure")
                (("for file in `ls \\*/Makefile.in`")
                 "for file in `ls */Makefile.in | sort`"))))
          #$@(if (or (target-aarch64?)
                     (target-riscv64?))
                 #~((add-after 'unpack 'update-config-files
                      (lambda* (#:key native-inputs inputs #:allow-other-keys)
                        (for-each
                          (lambda (location)
                            (for-each (lambda (file)
                                        (install-file
                                          (search-input-file
                                            (or native-inputs inputs)
                                            (string-append "/bin/" file))
                                          (dirname location)))
                                      '("config.guess" "config.sub")))
                          (find-files "." "config\\.guess")))))
                 #~()))))
    (inputs
     (list zlib))
    (native-inputs
     (append
       (list pkg-config r-knitr)
       (if (or (target-aarch64?)
               (target-riscv64?))
           (list config)
           '())))
    (home-page "https://r-forge.r-project.org/projects/rsymphony")
    (synopsis "Symphony integer linear programming solver in R")
    (description
     "This package was derived from Rsymphony.  The package provides an R
interface to SYMPHONY, a linear programming solver written in C++.  The main
difference between this package and Rsymphony is that it includes the solver
source code, while Rsymphony expects to find header and library files on the
users' system.  Thus the intention of @code{lpsymphony} is to provide an easy
to install interface to SYMPHONY.")
    ;; Symphony 5.4 or later is distributed under the terms of the EPL 1.0.
    ;; lpsimphony is released under the same terms.
    (license license:epl1.0)))

(define-public r-ihw
  (package
    (name "r-ihw")
    (version "1.32.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "IHW" version))
       (sha256
        (base32
         "0j7rfyhpzr8zzyvjyphlfviy07z30nzsqq6isi4l0r4qgaziy1s6"))))
    (properties `((upstream-name . "IHW")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocgenerics r-fdrtool r-lpsymphony r-slam))
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/IHW")
    (synopsis "Independent hypothesis weighting")
    (description
     "@dfn{Independent hypothesis weighting} (IHW) is a multiple testing
procedure that increases power compared to the method of Benjamini and
Hochberg by assigning data-driven weights to each hypothesis.  The input to
IHW is a two-column table of p-values and covariates.  The covariate can be
any continuous-valued or categorical variable that is thought to be
informative on the statistical properties of each hypothesis test, while it is
independent of the p-value under the null hypothesis.")
    (license license:artistic2.0)))

(define-public r-icobra
  (package
    (name "r-icobra")
    (version "1.32.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "iCOBRA" version))
       (sha256
        (base32
         "1m3awsz8h3gbi82an73kbxcsch2gl737zaanckl1vq2ghymqykrp"))))
    (properties `((upstream-name . "iCOBRA")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-dplyr
           r-dt
           r-ggplot2
           r-limma
           r-markdown
           r-reshape2
           r-rlang
           r-rocr
           r-scales
           r-shiny
           r-shinybs
           r-shinydashboard
           r-upsetr))
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/iCOBRA")
    (synopsis "Comparison and visualization of ranking and assignment methods")
    (description
     "This package provides functions for calculation and visualization of
performance metrics for evaluation of ranking and binary
classification (assignment) methods.  It also contains a Shiny application for
interactive exploration of results.")
    (license license:gpl2+)))

(define-public r-residualmatrix
  (package
    (name "r-residualmatrix")
    (version "1.14.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ResidualMatrix" version))
       (sha256
        (base32
         "1zq75i20kv0lgnh19zbhw5khxrag627ynqjvh3rj5kxbhj1l2z7c"))))
    (properties
     `((upstream-name . "ResidualMatrix")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-delayedarray r-matrix r-s4vectors))
    (native-inputs
     (list r-knitr))
    (home-page "https://github.com/LTLA/ResidualMatrix")
    (synopsis "Create a DelayedMatrix of regression residuals")
    (description
     "This package implements tools for delayed computation of a matrix of
residuals after fitting a linear model to each column of an input matrix.  It
also supports partial computation of residuals where selected factors are to
be preserved in the output matrix.  It implements a number of efficient
methods for operating on the delayed matrix of residuals, most notably matrix
multiplication and calculation of row/column sums or means.")
    (license license:gpl3)))

(define-public r-batchelor
  (package
    (name "r-batchelor")
    (version "1.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "batchelor" version))
       (sha256
        (base32
         "1h70rm0h711ndj9kz3q10yd9f6npjiwvk9wmkg6w3pdgjni1nrl0"))))
    (properties `((upstream-name . "batchelor")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-beachmat
           r-biocgenerics
           r-biocneighbors
           r-biocparallel
           r-biocsingular
           r-delayedarray
           r-delayedmatrixstats
           r-igraph
           r-matrix
           r-rcpp
           r-residualmatrix
           r-s4vectors
           r-scaledmatrix
           r-scuttle
           r-singlecellexperiment
           r-summarizedexperiment))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/batchelor")
    (synopsis "Single-Cell Batch Correction Methods")
    (description
     "This package implements a variety of methods for batch correction of
single-cell (RNA sequencing) data.  This includes methods based on detecting
mutually nearest neighbors, as well as several efficient variants of linear
regression of the log-expression values.  Functions are also provided to
perform global rescaling to remove differences in depth between batches, and
to perform a principal components analysis that is robust to differences in
the numbers of cells across batches.")
    (license license:gpl3)))

(define-public r-mast
  (package
    (name "r-mast")
    (version "1.30.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "MAST" version))
       (sha256
        (base32
         "1k8an0ifcgx54lqzpim3q37y4rhv57i94i46dlpmdyfghqqw0fny"))
       (snippet
        '(delete-file "docs/jquery.sticky-kit.min.js"))))
    (properties `((upstream-name . "MAST")))
    (build-system r-build-system)
    (arguments
     (list
      #:phases
      '(modify-phases %standard-phases
         (add-after 'unpack 'process-javascript
           (lambda* (#:key inputs #:allow-other-keys)
             (invoke "esbuild"
                     (assoc-ref inputs "js-jquery-sticky-kit")
                     "--minify"
                     "--outfile=docs/jquery.sticky-kit.min.js"))))))
    (propagated-inputs
     (list r-abind
           r-biobase
           r-biocgenerics
           r-data-table
           r-ggplot2
           r-matrix
           r-plyr
           r-progress
           r-reshape2
           r-s4vectors
           r-singlecellexperiment
           r-stringr
           r-summarizedexperiment))
    (native-inputs
     `(("esbuild" ,esbuild)
       ("js-jquery-sticky-kit"
        ,(origin
           (method url-fetch)
           (uri
            "https://cdn.jsdelivr.net/gh/leafo/sticky-kit@v1.1.2/jquery.sticky-kit.js")
           (sha256
            (base32
             "17c3a1hqc3ybwj7hpw8prazajp2x98aq7nyfn71h6lzjvblq297g"))))
       ("r-knitr" ,r-knitr)))
    (home-page "https://github.com/RGLab/MAST/")
    (synopsis "Model-based analysis of single cell transcriptomics")
    (description
     "This package provides methods and models for handling zero-inflated
single cell assay data.")
    (license license:gpl2+)))

(define-public r-monocle
  (package
    (name "r-monocle")
    (version "2.32.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "monocle" version))
       (sha256
        (base32
         "0b09mf9fph0mb0zing0prc757d223858q9lj4j3kfpfjpd3jffai"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biobase
           r-biocgenerics
           r-biocviews
           r-cluster
           r-combinat
           r-ddrtree
           r-dplyr
           r-fastica
           r-ggplot2
           r-hsmmsinglecell
           r-igraph
           r-irlba
           r-leidenbase
           r-limma
           r-mass
           r-matrix
           r-matrixstats
           r-pheatmap
           r-plyr
           r-proxy
           r-rann
           r-rcpp
           r-reshape2
           r-rtsne
           r-slam
           r-stringr
           r-tibble
           r-vgam
           r-viridis))
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/monocle")
    (synopsis "Clustering, differential expression, and trajectory analysis for single-cell RNA-Seq")
    (description
     "Monocle performs differential expression and time-series analysis for
single-cell expression experiments.  It orders individual cells according to
progress through a biological process, without knowing ahead of time which
genes define progress through that process.  Monocle also performs
differential expression analysis, clustering, visualization, and other useful
tasks on single cell expression data.  It is designed to work with RNA-Seq and
qPCR data, but could be used with other types as well.")
    (license license:artistic2.0)))

(define-public r-leidenbase
  (let ((commit "a11b8455fa3307d9e3ac4e3a5accddf3c83b9a96")
        (revision "1"))
    (package
      (name "r-leidenbase")
      (version (git-version "0.1.9" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/cole-trapnell-lab/leidenbase")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1f54mycsffvzmblz5pzgs3v4jygnbvz0c9d3x710gw5mxkq2p84f"))))
      (properties `((upstream-name . "leidenbase")))
      (build-system r-build-system)
      (inputs
       (list zlib))
      (native-inputs
       (list gfortran))
      (propagated-inputs
       (list r-igraph))
      (home-page "https://github.com/cole-trapnell-lab/leidenbase")
      (synopsis "R and C wrappers to run the Leiden find_partition function")
      (description
       "This package provides an R to C interface that runs the Leiden
community detection algorithm to find a basic partition.  It runs the
equivalent of the @code{find_partition} function.  This package includes the
required source code files from the official Leidenalg distribution and
several functions from the R igraph package.")
      (license license:gpl3+))))

(define-public r-sangerseqr
  (package
    (name "r-sangerseqr")
    (version "1.40.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "sangerseqR" version))
              (sha256
               (base32
                "01z2b7950yxhv8p73rfmq84bscrlpl956ihfyarnzdq9v9ljzzd9"))))
    (properties `((upstream-name . "sangerseqR")))
    (build-system r-build-system)
    (propagated-inputs (list r-biostrings r-pwalign r-shiny r-stringr))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/sangerseqR")
    (synopsis "Tools for Sanger Sequencing data in R")
    (description
     "This package contains several tools for analyzing Sanger Sequencing data
files in R, including reading @file{.scf} and @file{.ab1} files, making
basecalls and plotting chromatograms.")
    (license license:gpl2)))

(define-public r-sanssouci
  ;; sansscouci doesn't have a (versioned) release yet.
  ;; This is the latest commit as of packaging for Guix.
  (let ((commit "5fe20a9aaf4ac637fa83d9cc73ff1c22de97ca6f")
        (revision "1"))
    (package
      (name "r-sanssouci")
      (version (git-version "0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/pneuvial/sanssouci.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "13ycdd790qw64qy2zdvcrpj3fc8as628rsly32438d3rifnlc5sk"))))
      (build-system r-build-system)
      (propagated-inputs
       (list r-generics r-matrix r-matrixstats r-rcpp r-rcpparmadillo))
      (home-page "https://pneuvial.github.io/sanssouci")
      (synopsis "Post Hoc multiple testing inference")
      (description
       "The goal of sansSouci is to perform post hoc inference: in a multiple
testing context, sansSouci provides statistical guarantees on possibly
user-defined and/or data-driven sets of hypotheses.")
      (license license:gpl3))))

(define-public r-monocle3
  (package
    (name "r-monocle3")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cole-trapnell-lab/monocle3")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "16vpvlbms8fdvpfwzcig0rkg2mxnsq1h80d2l7q3953wm91qc9x4"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-assertthat
           r-batchelor
           r-biobase
           r-biocgenerics
           r-delayedmatrixstats
           r-dplyr
           r-ggplot2
           r-ggrepel
           r-grr
           r-htmlwidgets
           r-igraph
           r-irlba
           r-leidenbase
           r-limma
           r-lmtest
           r-mass
           r-matrix
           r-matrix-utils
           r-pbapply
           r-pbmcapply
           r-pheatmap
           r-plotly
           r-pryr
           r-proxy
           r-pscl
           r-purrr
           r-rann
           r-rcpp
           r-rcppparallel
           r-reshape2
           r-reticulate
           r-rhpcblasctl
           r-rsample
           r-rtsne
           r-shiny
           r-slam
           r-spdep
           r-speedglm
           r-stringr
           r-singlecellexperiment
           r-tibble
           r-tidyr
           r-uwot
           r-viridis))
    (home-page "https://github.com/cole-trapnell-lab/monocle3")
    (synopsis "Analysis toolkit for single-cell RNA-Seq data")
    (description
     "Monocle 3 is an analysis toolkit for single-cell RNA-Seq experiments.")
    (license license:expat)))

(define-public r-noiseq
  (package
    (name "r-noiseq")
    (version "2.48.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "NOISeq" version))
       (sha256
        (base32
         "1aph4phg58f4kpgavrxnn6dzh3lgms8sbjvip4f3gn8xs0p01mbz"))))
    (properties `((upstream-name . "NOISeq")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biobase r-matrix))
    (home-page "https://bioconductor.org/packages/NOISeq")
    (synopsis "Exploratory analysis and differential expression for RNA-seq data")
    (description
     "This package provides tools to support the analysis of RNA-seq
expression data or other similar kind of data.  It provides exploratory plots
to evaluate saturation, count distribution, expression per chromosome, type of
detected features, features length, etc.  It also supports the analysis of
differential expression between two experimental conditions with no parametric
assumptions.")
    (license license:artistic2.0)))

(define-public r-scdd
  (package
    (name "r-scdd")
    (version "1.28.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "scDD" version))
       (sha256
        (base32
         "106hawmyln6xj9m2npibb4lkd8qvz60lwjabhn3cpx3908x88ryb"))))
    (properties `((upstream-name . "scDD")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-arm
           r-biocparallel
           r-ebseq
           r-fields
           r-ggplot2
           r-mclust
           r-outliers
           r-s4vectors
           r-scran
           r-singlecellexperiment
           r-summarizedexperiment))
    (native-inputs
     (list r-knitr))
    (home-page "https://github.com/kdkorthauer/scDD")
    (synopsis "Mixture modeling of single-cell RNA-seq data")
    (description
     "This package implements a method to analyze single-cell RNA-seq data
utilizing flexible Dirichlet Process mixture models.  Genes with differential
distributions of expression are classified into several interesting patterns
of differences between two conditions.  The package also includes functions
for simulating data with these patterns from negative binomial
distributions.")
    (license license:gpl2)))

(define-public r-scone
  (package
    (name "r-scone")
    (version "1.28.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "scone" version))
       (sha256
        (base32
         "1qvnc6cds3avxh1k4z47vb2fih7q8a5b9hccnyrpfks5c0sd6yi0"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-aroma-light
           r-biocparallel
           r-boot
           r-class
           r-cluster
           r-compositions
           r-diptest
           r-edger
           r-fpc
           r-gplots
           r-hexbin
           r-limma
           r-matrixgenerics
           r-matrixstats
           r-mixtools
           r-rarpack
           r-rcolorbrewer
           r-rhdf5
           r-ruvseq
           r-singlecellexperiment
           r-summarizedexperiment))
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/scone")
    (synopsis "Single cell overview of normalized expression data")
    (description
     "SCONE is an R package for comparing and ranking the performance of
different normalization schemes for single-cell RNA-seq and other
high-throughput analyses.")
    (license license:artistic2.0)))

(define-public r-geoquery
  (package
    (name "r-geoquery")
    (version "2.72.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "GEOquery" version))
       (sha256
        (base32
         "0rqyv95l8h3s68ywhv3r7hm7j1mbm8prql7p2rlvv4xg31yqmnpl"))))
    (properties `((upstream-name . "GEOquery")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biobase
           r-curl
           r-data-table
           r-dplyr
           r-limma
           r-magrittr
           r-r-utils
           r-readr
           r-tidyr
           r-xml2))
    (native-inputs
     (list r-knitr))
    (home-page "https://github.com/seandavi/GEOquery/")
    (synopsis "Get data from NCBI Gene Expression Omnibus (GEO)")
    (description
     "The NCBI Gene Expression Omnibus (GEO) is a public repository of
microarray data.  Given the rich and varied nature of this resource, it is
only natural to want to apply BioConductor tools to these data.  GEOquery is
the bridge between GEO and BioConductor.")
    (license license:gpl2)))

(define-public r-illuminaio
  (package
    (name "r-illuminaio")
    (version "0.46.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "illuminaio" version))
       (sha256
        (base32
         "0l3n1plz5nvday0a1bm8173mikpfjbzrb1bp8yfvafy7lav4dsyv"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-base64))
    (home-page "https://github.com/HenrikBengtsson/illuminaio/")
    (synopsis "Parse Illumina microarray output files")
    (description
     "This package provides tools for parsing Illumina's microarray output
files, including IDAT.")
    (license license:gpl2)))

(define-public r-siggenes
  (package
    (name "r-siggenes")
    (version "1.78.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "siggenes" version))
       (sha256
        (base32
         "0j4nhdq82dzr19x5w4w4l97clra0z1kk6n24x56hrrj0xlm2cgnm"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biobase r-multtest r-scrime))
    (home-page "https://bioconductor.org/packages/siggenes/")
    (synopsis
     "Multiple testing using SAM and Efron's empirical Bayes approaches")
    (description
     "This package provides tools for the identification of differentially
expressed genes and estimation of the @dfn{False Discovery Rate} (FDR) using
both the Significance Analysis of Microarrays (SAM) and the @dfn{Empirical
Bayes Analyses of Microarrays} (EBAM).")
    (license license:lgpl2.0+)))

(define-public r-bumphunter
  (package
    (name "r-bumphunter")
    (version "1.46.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "bumphunter" version))
       (sha256
        (base32
         "08b5d0grmpk5pryffdh6gdafhp1brana353q2kmwhz7x55aq3w68"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotationdbi
           r-biocgenerics
           r-dorng
           r-foreach
           r-genomeinfodb
           r-genomicfeatures
           r-genomicranges
           r-iranges
           r-iterators
           r-limma
           r-locfit
           r-matrixstats
           r-s4vectors))
    (home-page "https://github.com/ririzarr/bumphunter")
    (synopsis "Find bumps in genomic data")
    (description
     "This package provides tools for finding bumps in genomic data in order
to identify differentially methylated regions in epigenetic epidemiology
studies.")
    (license license:artistic2.0)))

(define-public r-bumpymatrix
  (package
    (name "r-bumpymatrix")
    (version "1.12.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "BumpyMatrix" version))
              (sha256
               (base32
                "1f8xa184x6l363psbqn0a5yh16ghm2mciv726aii58bkln5qwkmb"))))
    (properties `((upstream-name . "BumpyMatrix")))
    (build-system r-build-system)
    (propagated-inputs (list r-iranges r-matrix r-s4vectors))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/BumpyMatrix")
    (synopsis "Bumpy matrix of non-scalar objects")
    (description
     "This package provides a class and subclasses for storing non-scalar
objects in matrix entries.  This is akin to a ragged array but the raggedness
is in the third dimension, much like a bumpy surface--hence the name.  Of
particular interest is the @code{BumpyDataFrameMatrix}, where each entry is a
Bioconductor data frame.  This allows us to naturally represent multivariate
data in a format that is compatible with two-dimensional containers like the
@code{SummarizedExperiment} and @code{MultiAssayExperiment} objects.")
    (license license:expat)))

(define-public r-mia
  (package
    (name "r-mia")
    (version "1.12.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "mia" version))
              (sha256
               (base32
                "1wn2lb7hn4zqqcs2jwavpsbk5kw8d78kxg2yv4jjpikpdz7haa2i"))))
    (properties `((upstream-name . "mia")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-ape
           r-biocgenerics
           r-biocparallel
           r-biostrings
           r-bluster
           r-decipher
           r-decontam
           r-delayedarray
           r-delayedmatrixstats
           r-dirichletmultinomial
           r-dplyr
           r-iranges
           r-mass
           r-matrixgenerics
           r-multiassayexperiment
           r-rlang
           r-s4vectors
           r-scater
           r-scuttle
           r-singlecellexperiment
           r-summarizedexperiment
           r-tibble
           r-tidyr
           r-treesummarizedexperiment
           r-vegan))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/microbiome/mia")
    (synopsis "Microbiome analysis")
    (description
     "The mia package implements tools for microbiome analysis based on the
@code{SummarizedExperiment}, @code{SingleCellExperiment} and
@code{TreeSummarizedExperiment} infrastructure.  Data wrangling and analysis
in the context of taxonomic data is the main scope.  Additional functions for
common task are implemented such as community indices calculation and
summarization.")
    (license license:artistic2.0)))

(define-public r-microbiome
  (package
    (name "r-microbiome")
    (version "1.26.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "microbiome" version))
              (sha256
               (base32
                "1azxfydcrxi46j6lykspzchwcj0dpghjia7vfb2bppz9gh8nvk37"))))
    (properties `((upstream-name . "microbiome")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biostrings
           r-compositions
           r-dplyr
           r-ggplot2
           r-phyloseq
           r-reshape2
           r-rtsne
           r-scales
           r-tibble
           r-tidyr
           r-vegan))
    (native-inputs (list r-knitr))
    (home-page "https://microbiome.github.io/microbiome/")
    (synopsis "Tools for microbiome analysis")
    (description
     "This package facilitates phyloseq exploration and analysis of taxonomic
profiling data.  This package provides tools for the manipulation, statistical
analysis, and visualization of taxonomic profiling data.  In addition to
targeted case-control studies, microbiome facilitates scalable exploration of
population cohorts.  This package supports the independent phyloseq data
format and expands the available toolkit in order to facilitate the
standardization of the analyses and the development of best practices.")
    (license license:bsd-2)))

;; This is a CRAN package but it depends on phyloseq, which is from
;; Bioconductor.
(define-public r-microbiomestat
  (package
    (name "r-microbiomestat")
    (version "1.2")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "MicrobiomeStat" version))
              (sha256
               (base32
                "1kpc68sl63k104xy7r6f3gxan0yx0rs7ksn8ldbq8xla00ddflgq"))))
    (properties `((upstream-name . "MicrobiomeStat")))
    (build-system r-build-system)
    (propagated-inputs (list r-foreach
                             r-ggplot2
                             r-ggrepel
                             r-lmertest
                             r-mass
                             r-matrix
                             r-matrixstats
                             r-modeest
                             r-statmod))
    (home-page "https://cran.r-project.org/package=MicrobiomeStat")
    (synopsis "Statistical methods for microbiome compositional data")
    (description
     "This package provides a suite of methods for powerful and robust
microbiome data analysis addressing zero-inflation, phylogenetic structure and
compositional effects.  The methods can be applied to the analysis of
other (high-dimensional) compositional data arising from sequencing
experiments.")
    (license license:gpl3)))

(define-public r-milor
  (package
    (name "r-milor")
    (version "2.0.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "miloR" version))
              (sha256
               (base32
                "1cb5iw90h6raflgmypd2wc8y4mbsv9g8alshh30k1kjm8vlsw92y"))))
    (properties `((upstream-name . "miloR")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocgenerics
           r-biocneighbors
           r-biocparallel
           r-biocsingular
           r-cowplot
           r-dplyr
           r-edger
           r-ggbeeswarm
           r-ggplot2
           r-ggraph
           r-ggrepel
           r-gtools
           r-igraph
           r-irlba
           r-limma
           r-matrix
           r-matrixgenerics
           r-matrixstats
           r-numderiv
           r-patchwork
           r-rcolorbrewer
           r-rcpp
           r-rcpparmadillo
           r-s4vectors
           r-singlecellexperiment
           r-stringr
           r-summarizedexperiment
           r-tibble
           r-tidyr))
    (native-inputs (list r-knitr))
    (home-page "https://marionilab.github.io/miloR")
    (synopsis "Differential neighbourhood abundance testing on a graph")
    (description
     "Milo performs single-cell differential abundance testing.  Cell states
are modelled as representative neighbourhoods on a nearest neighbour graph.
Hypothesis testing is performed using a negative bionomial generalized linear
model.")
    (license license:gpl3)))

(define-public r-minfi
  (package
    (name "r-minfi")
    (version "1.50.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "minfi" version))
       (sha256
        (base32
         "1hk44wddcygh4j7jmj1b0p40hf5qdy1m78x14j19h7z9aq4azdij"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-beanplot
           r-biobase
           r-biocgenerics
           r-biocparallel
           r-biostrings
           r-bumphunter
           r-data-table
           r-delayedarray
           r-delayedmatrixstats
           r-genefilter
           r-genomeinfodb
           r-genomicranges
           r-geoquery
           r-hdf5array
           r-illuminaio
           r-iranges
           r-lattice
           r-limma
           r-mass
           r-mclust
           r-nlme
           r-nor1mix
           r-preprocesscore
           r-quadprog
           r-rcolorbrewer
           r-reshape
           r-s4vectors
           r-siggenes
           r-summarizedexperiment))
    (native-inputs
     (list r-knitr))
    (home-page "https://github.com/hansenlab/minfi")
    (synopsis "Analyze Illumina Infinium DNA methylation arrays")
    (description
     "This package provides tools to analyze and visualize Illumina Infinium
methylation arrays.")
    (license license:artistic2.0)))

(define-public r-missmethyl
  (package
    (name "r-missmethyl")
    (version "1.38.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "missMethyl" version))
              (sha256
               (base32
                "0fnqa7v6lgld4pdqnvgdzly73aj4a4cn8sgszicxsyn1damxsjiw"))))
    (properties `((upstream-name . "missMethyl")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotationdbi
           r-biasedurn
           r-biobase
           r-biocgenerics
           r-genomicranges
           r-go-db
           r-illuminahumanmethylation450kanno-ilmn12-hg19
           r-illuminahumanmethylation450kmanifest
           r-illuminahumanmethylationepicanno-ilm10b4-hg19
           r-illuminahumanmethylationepicmanifest
           r-iranges
           r-limma
           r-methylumi
           r-minfi
           r-org-hs-eg-db
           r-ruv
           r-s4vectors
           r-statmod
           r-stringr
           r-summarizedexperiment))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/missMethyl")
    (synopsis "Analyzing Illumina HumanMethylation BeadChip data")
    (description
     "This is a package for normalization, testing for differential
variability and differential methylation and gene set testing for data from
Illumina's Infinium HumanMethylation arrays.  The normalization procedure is
subset-quantile within-array normalization (SWAN), which allows Infinium I and
II type probes on a single array to be normalized together.  The test for
differential variability is based on an empirical Bayes version of Levene's
test.  Differential methylation testing is performed using RUV, which can
adjust for systematic errors of unknown origin in high-dimensional data by
using negative control probes.  Gene ontology analysis is performed by taking
into account the number of probes per gene on the array, as well as taking
into account multi-gene associated probes.")
    (license license:gpl2)))

(define-public r-methylumi
  (package
    (name "r-methylumi")
    (version "2.50.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "methylumi" version))
       (sha256
        (base32
         "1ms3w43yvy810mr6dml9hqp3kw5jlpr9153kd62gvhjsgikaj3ja"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotate
           r-annotationdbi
           r-biobase
           r-biocgenerics
           r-fdb-infiniummethylation-hg19
           r-genefilter
           r-genomeinfodb
           r-genomicfeatures
           r-genomicranges
           r-ggplot2
           r-illuminaio
           r-iranges
           r-lattice
           r-matrixstats
           r-minfi
           r-reshape2
           r-s4vectors
           r-scales
           r-summarizedexperiment))
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/methylumi")
    (synopsis "Handle Illumina methylation data")
    (description
     "This package provides classes for holding and manipulating Illumina
methylation data.  Based on eSet, it can contain MIAME information, sample
information, feature information, and multiple matrices of data.  An
\"intelligent\" import function, methylumiR can read the Illumina text files
and create a MethyLumiSet.  methylumIDAT can directly read raw IDAT files from
HumanMethylation27 and HumanMethylation450 microarrays.  Normalization,
background correction, and quality control features for GoldenGate, Infinium,
and Infinium HD arrays are also included.")
    (license license:gpl2)))

(define-public r-lefser
  (package
    (name "r-lefser")
    (version "1.14.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "lefser" version))
              (sha256
               (base32
                "1qi4q3vijys4q1gfnflvvz8ws3j6f65rpprpgmrkqgiwfn7dvi6g"))))
    (properties `((upstream-name . "lefser")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-coin
           r-dplyr
           r-ggplot2
           r-mass
           r-s4vectors
           r-summarizedexperiment))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/waldronlab/lefser")
    (synopsis "LEfSE method for microbiome biomarker discovery")
    (description
     "Lefser is an implementation in R of the popular \"LDA Effect
Size\" (LEfSe) method for microbiome biomarker discovery.  It uses the
Kruskal-Wallis test, Wilcoxon-Rank Sum test, and Linear Discriminant Analysis
to find biomarkers of groups and sub-groups.")
    (license license:artistic2.0)))

(define-public r-lumi
  (package
    (name "r-lumi")
    (version "2.56.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "lumi" version))
       (sha256
        (base32
         "04r7dw00yvjrxs0h6mwh79lwp55f5v8s4pgmzcy2fnpb2cdslpzy"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-affy
           r-annotate
           r-annotationdbi
           r-biobase
           r-dbi
           r-genomicfeatures
           r-genomicranges
           r-kernsmooth
           r-lattice
           r-mass
           r-methylumi
           r-mgcv
           r-nleqslv
           r-preprocesscore
           r-rsqlite))
    (home-page "https://bioconductor.org/packages/lumi")
    (synopsis "BeadArray-specific methods for Illumina methylation and expression microarrays")
    (description
     "The lumi package provides an integrated solution for the Illumina
microarray data analysis.  It includes functions of Illumina
BeadStudio (GenomeStudio) data input, quality control, BeadArray-specific
variance stabilization, normalization and gene annotation at the probe level.
It also includes the functions of processing Illumina methylation microarrays,
especially Illumina Infinium methylation microarrays.")
    (license license:lgpl2.0+)))

(define-public r-linnorm
  (package
    (name "r-linnorm")
    (version "2.28.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Linnorm" version))
       (sha256
        (base32
         "1la6fwaap7957rrb4cs6w48kjz4hqrw3h39lpps419d4cbnsp5jz"))))
    (properties `((upstream-name . "Linnorm")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-amap
           r-apcluster
           r-ellipse
           r-fastcluster
           r-fpc
           r-ggdendro
           r-ggplot2
           r-gmodels
           r-igraph
           r-limma
           r-mass
           r-mclust
           r-rcpp
           r-rcpparmadillo
           r-rtsne
           r-statmod
           r-vegan
           r-zoo))
    (native-inputs
     (list r-knitr))
    (home-page "http://www.jjwanglab.org/Linnorm/")
    (synopsis "Linear model and normality based transformation method")
    (description
     "Linnorm is an R package for the analysis of RNA-seq, scRNA-seq, ChIP-seq
count data or any large scale count data.  It transforms such datasets for
parametric tests.  In addition to the transformtion function (@code{Linnorm}),
the following pipelines are implemented:

@enumerate
@item Library size/batch effect normalization (@code{Linnorm.Norm})
@item Cell subpopluation analysis and visualization using t-SNE or PCA K-means
  clustering or hierarchical clustering (@code{Linnorm.tSNE},
  @code{Linnorm.PCA}, @code{Linnorm.HClust})
@item Differential expression analysis or differential peak detection using
  limma (@code{Linnorm.limma})
@item Highly variable gene discovery and visualization (@code{Linnorm.HVar})
@item Gene correlation network analysis and visualization (@code{Linnorm.Cor})
@item Stable gene selection for scRNA-seq data; for users without or who do
  not want to rely on spike-in genes (@code{Linnorm.SGenes})
@item Data imputation (@code{Linnorm.DataImput}).
@end enumerate

Linnorm can work with raw count, CPM, RPKM, FPKM and TPM.  Additionally, the
@code{RnaXSim} function is included for simulating RNA-seq data for the
evaluation of DEG analysis methods.")
    (license license:expat)))

(define-public r-ioniser
  (package
    (name "r-ioniser")
    (version "2.28.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "IONiseR" version))
       (sha256
        (base32
         "1ihski4akl70v6kq03di6w1pz63dx6rxgmyjkm0dv0nhgkmrza22"))))
    (properties `((upstream-name . "IONiseR")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocgenerics
           r-biocparallel
           r-biostrings
           r-bit64
           r-dplyr
           r-ggplot2
           r-magrittr
           r-rhdf5
           r-shortread
           r-stringr
           r-tibble
           r-tidyr
           r-xvector))
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/IONiseR/")
    (synopsis "Quality assessment tools for Oxford Nanopore MinION data")
    (description
     "IONiseR provides tools for the quality assessment of Oxford Nanopore
MinION data.  It extracts summary statistics from a set of fast5 files and can
be used either before or after base calling.  In addition to standard
summaries of the read-types produced, it provides a number of plots for
visualising metrics relative to experiment run time or spatially over the
surface of a flowcell.")
    (license license:expat)))

;; This is a CRAN package, but it depends on multtest from Bioconductor.
(define-public r-mutoss
  (package
    (name "r-mutoss")
    (version "0.1-13")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "mutoss" version))
       (sha256
        (base32
         "0hgi9wpy3ai23dk6cdba6r118vvmgw210racsg3n1p24rv6ny3xn"))))
    (properties `((upstream-name . "mutoss")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-multcomp r-multtest r-mvtnorm r-plotrix))
    (home-page "https://github.com/kornl/mutoss/")
    (synopsis "Unified multiple testing procedures")
    (description
     "This package is designed to ease the application and comparison of
multiple hypothesis testing procedures for FWER, gFWER, FDR and FDX.  Methods
are standardized and usable by the accompanying mutossGUI package.")
    ;; Any version of the GPL.
    (license (list license:gpl2+ license:gpl3+))))

;; This is a CRAN package, but it depends on mutoss, which depends on multtest
;; from Bioconductor, so we put it here.
(define-public r-metap
  (package
    (name "r-metap")
    (version "1.10")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "metap" version))
       (sha256
        (base32
         "18l86026g5g0pdihw41h7yck1ad5k43bb7as5qih2k5xkr680mzx"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-lattice
           r-mathjaxr
           r-mutoss
           r-qqconf
           r-rdpack
           r-tfisher))
    (home-page "http://www.dewey.myzen.co.uk/meta/meta.html")
    (synopsis "Meta-analysis of significance values")
    (description
     "The canonical way to perform meta-analysis involves using effect sizes.
When they are not available this package provides a number of methods for
meta-analysis of significance values including the methods of Edgington,
Fisher, Stouffer, Tippett, and Wilkinson; a number of data-sets to replicate
published results; and a routine for graphical display.")
    (license license:gpl2)))

(define-public r-tradeseq
  (package
   (name "r-tradeseq")
   (version "1.18.0")
   (source (origin
            (method url-fetch)
            (uri (bioconductor-uri "tradeSeq" version))
            (sha256
             (base32
              "1kgvvvwvj3dgb2wik3y8j44ynda94bi05vvmiwlayrypqprwi1c1"))))
   (build-system r-build-system)
   (propagated-inputs
    (list r-biobase
          r-biocparallel
          r-edger
          r-ggplot2
          r-igraph
          r-magrittr
          r-mass
          r-matrix
          r-matrixstats
          r-mgcv
          r-pbapply
          r-princurve
          r-rcolorbrewer
          r-s4vectors
          r-singlecellexperiment
          r-slingshot
          r-summarizedexperiment
          r-tibble
          r-trajectoryutils
          r-viridis))
   (native-inputs
    (list r-knitr))
   (home-page "https://statomics.github.io/tradeSeq/index.html")
   (synopsis "Trajectory-based differential expression analysis")
   (description
    "This package provides a flexible method for fitting regression models that
can be used to find genes that are differentially expressed along one or
multiple lineages in a trajectory.  Based on the fitted models, it uses a
variety of tests suited to answer different questions of interest, e.g.  the
discovery of genes for which expression is associated with pseudotime, or which
are differentially expressed (in a specific region) along the trajectory.  It
fits a negative binomial generalized additive model (GAM) for each gene, and
performs inference on the parameters of the GAM.")
   (license license:expat)))

(define-public r-triform
  (package
    (name "r-triform")
    (version "1.29.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "triform" version))
       (sha256
        (base32
         "089b7f6dwpi9abj0ncswbi4s30k45996zb99sh43avw6jcb6qj60"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocgenerics r-iranges r-yaml))
    (home-page "https://bioconductor.org/packages/triform/")
    (synopsis "Find enriched regions in transcription factor ChIP-sequencing data")
    (description
     "The Triform algorithm uses model-free statistics to identify peak-like
distributions of TF ChIP sequencing reads, taking advantage of an improved
peak definition in combination with known profile characteristics.")
    (license license:gpl2)))

(define-public r-varianttools
  (package
    (name "r-varianttools")
    (version "1.46.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "VariantTools" version))
       (sha256
        (base32
         "1idil9vkd5risx9ahkagdacnj2g2a11gzsd7wmhg5gjmr77r53ww"))))
    (properties `((upstream-name . "VariantTools")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biobase
           r-biocgenerics
           r-biocparallel
           r-biostrings
           r-bsgenome
           r-genomeinfodb
           r-genomicfeatures
           r-genomicranges
           r-iranges
           r-matrix
           r-rsamtools
           r-rtracklayer
           r-s4vectors
           r-variantannotation))
    (home-page "https://bioconductor.org/packages/VariantTools/")
    (synopsis "Tools for exploratory analysis of variant calls")
    (description
     "Explore, diagnose, and compare variant calls using filters.  The
VariantTools package supports a workflow for loading data, calling single
sample variants and tumor-specific somatic mutations or other sample-specific
variant types (e.g., RNA editing).  Most of the functions operate on
alignments (BAM files) or datasets of called variants.  The user is expected
to have already aligned the reads with a separate tool, e.g., GSNAP via
gmapR.")
    (license license:artistic2.0)))

(define-public r-heatplus
  (package
    (name "r-heatplus")
    (version "3.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Heatplus" version))
       (sha256
        (base32
         "1pc63hk0smcflq4903hvs4pp8r0kzlx08j0m1kxlh14cxwppamyb"))))
    (properties `((upstream-name . "Heatplus")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-rcolorbrewer))
    (home-page "https://github.com/alexploner/Heatplus")
    (synopsis "Heatmaps with row and/or column covariates and colored clusters")
    (description
     "This package provides tools to display a rectangular heatmap (intensity
plot) of a data matrix.  By default, both samples (columns) and features (row)
of the matrix are sorted according to a hierarchical clustering, and the
corresponding dendrogram is plotted.  Optionally, panels with additional
information about samples and features can be added to the plot.")
    (license license:gpl2+)))

(define-public r-gosemsim
  (package
    (name "r-gosemsim")
    (version "2.30.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "GOSemSim" version))
       (sha256
        (base32
         "0985w9b2iw7jbmpymfvqmpzshsy0lvqw7whc1djdnh58lwscnhcq"))))
    (properties `((upstream-name . "GOSemSim")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotationdbi r-go-db r-rcpp r-rlang r-yulab-utils))
    (native-inputs
     (list r-knitr))
    (home-page "https://guangchuangyu.github.io/software/GOSemSim")
    (synopsis "GO-terms semantic similarity measures")
    (description
     "The semantic comparisons of @dfn{Gene Ontology} (GO) annotations provide
quantitative ways to compute similarities between genes and gene groups, and
have became important basis for many bioinformatics analysis approaches.
GOSemSim is an R package for semantic similarity computation among GO terms,
sets of GO terms, gene products and gene clusters.")
    (license license:artistic2.0)))

(define-public r-anota
  (package
    (name "r-anota")
    (version "1.52.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "anota" version))
       (sha256
        (base32
         "1pg9p6982901k08iqpk0cdb9hii3k7bci9gq6an711r8a4iy3lmc"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-multtest r-qvalue))
    (home-page "https://bioconductor.org/packages/anota/")
    (synopsis "Analysis of translational activity")
    (description
     "Genome wide studies of translational control is emerging as a tool to
study various biological conditions.  The output from such analysis is both
the mRNA level (e.g. cytosolic mRNA level) and the level of mRNA actively
involved in translation (the actively translating mRNA level) for each mRNA.
The standard analysis of such data strives towards identifying differential
translational between two or more sample classes - i.e., differences in
actively translated mRNA levels that are independent of underlying differences
in cytosolic mRNA levels.  This package allows for such analysis using partial
variances and the random variance model.  As 10s of thousands of mRNAs are
analyzed in parallel the library performs a number of tests to assure that
the data set is suitable for such analysis.")
    (license license:gpl3)))

(define-public r-anota2seq
  (package
    (name "r-anota2seq")
    (version "1.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "anota2seq" version))
       (sha256
        (base32 "140k37wswl4w2136pz9y0z82sphzxmgzgk3cr400kkm39wjn7vxm"))))
    (properties `((upstream-name . "anota2seq")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-deseq2
           r-edger
           r-limma
           r-multtest
           r-qvalue
           r-rcolorbrewer
           r-summarizedexperiment))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/anota2seq")
    (synopsis "Transcriptome-wide analysis of translational efficiency")
    (description
     "The anota2seq package provides analysis of translational efficiency and
differential expression analysis for polysome-profiling and ribosome-profiling
studies (two or more sample classes) quantified by RNA sequencing or
DNA-microarray.  Polysome-profiling and ribosome-profiling typically generate
data for two RNA sources, translated mRNA and total mRNA.  Analysis of
differential expression is used to estimate changes within each RNA source.
Analysis of translational efficiency aims to identify changes in translation
efficiency leading to altered protein levels that are independent of total
mRNA levels or buffering, a mechanism regulating translational efficiency so
that protein levels remain constant despite fluctuating total mRNA levels.")
    (license license:gpl3)))

(define-public r-fcscan
  (package
    (name "r-fcscan")
    (version "1.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "fcScan" version))
       (sha256
        (base32 "1gb15zrv44yn65wns7dk24rp54qv7wafqxgk1q8jnbkq9lp5wj2y"))))
    (properties `((upstream-name . "fcScan")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-doparallel
           r-foreach
           r-genomicranges
           r-iranges
           r-plyr
           r-rtracklayer
           r-summarizedexperiment
           r-variantannotation))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/fcScan")
    (synopsis "Detect clusters of coordinates with user defined options")
    (description
     "This package is used to detect combination of genomic coordinates
falling within a user defined window size along with user defined overlap
between identified neighboring clusters.  It can be used for genomic data
where the clusters are built on a specific chromosome or specific strand.
Clustering can be performed with a \"greedy\" option allowing thus the
presence of additional sites within the allowed window size.")
    (license license:artistic2.0)))

(define-public r-fgsea
  (package
    (name "r-fgsea")
    (version "1.30.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "fgsea" version))
       (sha256
        (base32
         "0nnvqsyrj24h5y8qw8llk9f4jgy0v734k37kcksfcpja17frdyjy"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-bh
           r-biocparallel
           r-cowplot
           r-data-table
           r-fastmatch
           r-ggplot2
           r-matrix
           r-rcpp
           r-scales))
    (native-inputs
     (list r-knitr))
    (home-page "https://github.com/ctlab/fgsea/")
    (synopsis "Fast gene set enrichment analysis")
    (description
     "The package implements an algorithm for fast gene set enrichment
analysis.  Using the fast algorithm makes more permutations and gets
more fine grained p-values, which allows using accurate standard approaches
to multiple hypothesis correction.")
    (license license:expat)))

(define-public r-dose
  (package
    (name "r-dose")
    (version "3.30.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "DOSE" version))
       (sha256
        (base32
         "1kyrr1xxcif45ynyq4wsv2247bjily5sgzn17m1g3ypw2hkqcj53"))))
    (properties `((upstream-name . "DOSE")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotationdbi
           r-biocparallel
           r-fgsea
           r-ggplot2
           r-gosemsim
           r-hdo-db
           r-qvalue
           r-reshape2
           r-yulab-utils))
    (native-inputs
     (list r-knitr))
    (home-page "https://guangchuangyu.github.io/software/DOSE/")
    (synopsis "Disease ontology semantic and enrichment analysis")
    (description
     "This package implements five methods proposed by Resnik, Schlicker,
Jiang, Lin and Wang, respectively, for measuring semantic similarities among
@dfn{Disease ontology} (DO) terms and gene products.  Enrichment analyses
including hypergeometric model and gene set enrichment analysis are also
implemented for discovering disease associations of high-throughput biological
data.")
    (license license:artistic2.0)))

(define-public r-enrichedheatmap
  (package
    (name "r-enrichedheatmap")
    (version "1.34.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "EnrichedHeatmap" version))
              (sha256
               (base32
                "0fmqz9xqs1108a5xxpsc63pplcc0i742r4zdk46p68my28ba2xfq"))))
    (properties `((upstream-name . "EnrichedHeatmap")))
    (build-system r-build-system)
    (propagated-inputs (list r-circlize
                             r-complexheatmap
                             r-genomicranges
                             r-getoptlong
                             r-iranges
                             r-locfit
                             r-matrixstats
                             r-rcpp))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/jokergoo/EnrichedHeatmap")
    (synopsis "Enriched heatmaps")
    (description
     "Enriched heatmap is a special type of heatmap which visualizes the
enrichment of genomic signals on specific target regions.  This type of
heatmap is just a normal heatmap but with some special settings, with the
functionality of @code{ComplexHeatmap}, it would be much easier to customize
the heatmap as well as concatenating to a list of heatmaps to show
correspondance between different data sources.")
    (license license:expat)))

(define-public r-enrichplot
  (package
    (name "r-enrichplot")
    (version "1.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "enrichplot" version))
       (sha256
        (base32
         "0rkp1zidz34h4d02m4jqv509vw0wyrc084hvj3lxr7mb9j7rnq48"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-aplot
           r-dose
           r-ggfun
           r-ggnewscale
           r-ggplot2
           r-ggraph
           r-ggtree
           r-gosemsim
           r-igraph
           r-magrittr
           r-plyr
           r-purrr
           r-rcolorbrewer
           r-reshape2
           r-rlang
           r-scatterpie
           r-shadowtext
           r-yulab-utils))
    (native-inputs
     (list r-knitr))
    (home-page "https://github.com/GuangchuangYu/enrichplot")
    (synopsis "Visualization of functional enrichment result")
    (description
     "The enrichplot package implements several visualization methods for
interpreting functional enrichment results obtained from ORA or GSEA analyses.
All the visualization methods are developed based on ggplot2 graphics.")
    (license license:artistic2.0)))

;; This is a CRAN package, but it depends on Bioconductor packages, so we
;; added it here.
(define-public r-classdiscovery
  (package
    (name "r-classdiscovery")
    (version "3.4.5")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "ClassDiscovery" version))
              (sha256
               (base32
                "0k70s7nlmxymxw84174ikqx854hi0pizd9acpr7ky8fb30fyxg31"))))
    (properties `((upstream-name . "ClassDiscovery")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biobase r-cluster r-mclust r-oompabase r-oompadata))
    (native-inputs
     (list r-xtable)) ;for vignettes
    (home-page "https://oompa.r-forge.r-project.org/")
    (synopsis
     "Classes and methods for \"Class Discovery\" with Microarrays or Proteomics")
    (description
     "This package defines classes for \"class discovery\" in the OOMPA project.
Class discovery primarily consists of unsupervised clustering methods with
attempts to assess their statistical significance.")
    (license license:asl2.0)))

(define-public r-clusterprofiler
  (package
    (name "r-clusterprofiler")
    (version "4.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "clusterProfiler" version))
       (sha256
        (base32
         "0wmfbr4n8a1mg8g3gl9gnqrvq5p6cfay2858fqx7wxpr2q8yynhz"))))
    (properties
     `((upstream-name . "clusterProfiler")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotationdbi
           r-dose
           r-downloader
           r-dplyr
           r-enrichplot
           r-go-db
           r-gosemsim
           r-gson
           r-httr
           r-igraph
           r-magrittr
           r-plyr
           r-qvalue
           r-rlang
           r-tidyr
           r-yulab-utils))
    (native-inputs
     (list r-knitr))
    (home-page "https://guangchuangyu.github.io/software/clusterProfiler/")
    (synopsis "Analysis and visualization of functional profiles for gene clusters")
    (description
     "This package implements methods to analyze and visualize functional
profiles (GO and KEGG) of gene and gene clusters.")
    (license license:artistic2.0)))

(define-public r-clusterexperiment
  (package
    (name "r-clusterexperiment")
    (version "2.24.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "clusterExperiment" version))
              (sha256
               (base32
                "0pi6qnchh89fhfr50fk4hrrkz1r8gfwjcd7254pgiaxcazv2380a"))))
    (build-system r-build-system)
    (native-inputs
     (list r-knitr))
    (propagated-inputs
     (list r-ape
           r-biocgenerics
           r-biocsingular
           r-cluster
           r-delayedarray
           r-edger
           r-hdf5array
           r-kernlab
           r-limma
           r-locfdr
           r-matrix
           r-matrixstats
           r-mbkmeans
           r-nmf
           r-phylobase
           r-pracma
           r-rcolorbrewer
           r-rcpp
           r-s4vectors
           r-scales
           r-singlecellexperiment
           r-stringr
           r-summarizedexperiment
           r-zinbwave))
    (home-page "https://bioconductor.org/packages/clusterExperiment/")
    (synopsis "Compare clusterings for single-cell sequencing")
    (description "This package provides functionality for running and comparing
many different clusterings of single-cell sequencing data or other large mRNA
expression data sets.")
    (license license:artistic2.0)))

(define-public r-mlinterfaces
  (package
    (name "r-mlinterfaces")
    (version "1.84.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "MLInterfaces" version))
       (sha256
        (base32
         "1m8clkb5rznzv2fwrl59csq8v0ibqwdm4fphq10b8pmyvj880qpn"))))
    (properties `((upstream-name . "MLInterfaces")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotate
           r-biobase
           r-biocgenerics
           r-cluster
           r-fpc
           r-gbm
           r-gdata
           r-genefilter
           r-ggvis
           r-hwriter
           r-magrittr
           r-mass
           r-mlbench
           r-pls
           r-rcolorbrewer
           r-rcpp
           r-rpart
           r-sfsmisc
           r-shiny
           r-summarizedexperiment
           r-threejs))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/MLInterfaces/")
    (synopsis "Interfaces to R machine learning procedures")
    (description
     "This package provides uniform interfaces to machine learning code for
data in R and Bioconductor containers.")
    ;; Any version of the LGPL.
    (license license:lgpl2.1+)))

(define-public r-annaffy
  (package
    (name "r-annaffy")
    (version "1.76.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "annaffy" version))
       (sha256
        (base32
         "1298mn8yz4i0qqw1i8pd4ym5f65dmyb8wcmpyahzz11na4diybmj"))))
    (build-system r-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-reference-to-non-free-data
           (lambda _
             (substitute* "DESCRIPTION"
               ((", KEGG.db") "")))))))
    (propagated-inputs
     (list r-annotationdbi r-biobase r-biocmanager r-dbi r-go-db))
    (home-page "https://bioconductor.org/packages/annaffy/")
    (synopsis "Annotation tools for Affymetrix biological metadata")
    (description
     "This package provides functions for handling data from Bioconductor
Affymetrix annotation data packages.  It produces compact HTML and text
reports including experimental data and URL links to many online databases.
It allows searching of biological metadata using various criteria.")
    ;; Any version of the LGPL according to the DESCRIPTION file.  A copy of
    ;; the LGPL 2.1 is included.
    (license license:lgpl2.1+)))

(define-public r-a4core
  (package
    (name "r-a4core")
    (version "1.52.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "a4Core" version))
       (sha256
        (base32
         "1jn2ikzijd282jl96i9hcvahkc6l2vsd79dxqiz7kwaqcp7hs5z9"))))
    (properties `((upstream-name . "a4Core")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biobase r-glmnet))
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/a4Core")
    (synopsis "Automated Affymetrix array analysis core package")
    (description
     "This is the core package for the automated analysis of Affymetrix
arrays.")
    (license license:gpl3)))

(define-public r-a4classif
  (package
    (name "r-a4classif")
    (version "1.52.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "a4Classif" version))
       (sha256
        (base32
         "15jcd66brvbjz0p3z2b43ac5k81s3vmcbkwmga0f7f5v53fp8qvw"))))
    (properties `((upstream-name . "a4Classif")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-a4core
           r-a4preproc
           r-biobase
           r-glmnet
           r-pamr
           r-rocr
           r-varselrf))
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/a4Classif/")
    (synopsis "Automated Affymetrix array analysis classification package")
    (description
     "This is the classification package for the automated analysis of
Affymetrix arrays.")
    (license license:gpl3)))

(define-public r-a4preproc
  (package
    (name "r-a4preproc")
    (version "1.52.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "a4Preproc" version))
       (sha256
        (base32
         "1yphs36pk2y4qajq24y443yzb2f55b08nl81ylpf65mvxlxd23w2"))))
    (properties `((upstream-name . "a4Preproc")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biobase r-biocgenerics))
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/a4Preproc/")
    (synopsis "Automated Affymetrix array analysis preprocessing package")
    (description
     "This is a package for the automated analysis of Affymetrix arrays.  It
is used for preprocessing the arrays.")
    (license license:gpl3)))

(define-public r-a4reporting
  (package
    (name "r-a4reporting")
    (version "1.52.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "a4Reporting" version))
       (sha256
        (base32
         "08zlg74qryqm238mld876v15a6c71brkz2y1gnbj0rnl1l9f9rbh"))))
    (properties `((upstream-name . "a4Reporting")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-xtable))
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/a4Reporting/")
    (synopsis "Automated Affymetrix array analysis reporting package")
    (description
     "This is a package for the automated analysis of Affymetrix arrays.  It
provides reporting features.")
    (license license:gpl3)))

(define-public r-a4base
  (package
    (name "r-a4base")
    (version "1.52.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "a4Base" version))
       (sha256
        (base32
         "19vapjmyd3ly3s2gqa2xfg99gyc6n6lv3c6rpi0ch4hzb7wiinvm"))))
    (properties `((upstream-name . "a4Base")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-a4core
           r-a4preproc
           r-annaffy
           r-biobase
           r-genefilter
           r-glmnet
           r-gplots
           r-limma
           r-mpm
           r-multtest))
    (home-page "https://bioconductor.org/packages/a4Base/")
    (synopsis "Automated Affymetrix array analysis base package")
    (description
     "This package provides basic features for the automated analysis of
Affymetrix arrays.")
    (license license:gpl3)))

(define-public r-a4
  (package
    (name "r-a4")
    (version "1.52.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "a4" version))
       (sha256
        (base32
         "1wzqpvz49wkbvqij48c3l6vqfvflgic8gzvjdigb1c6mr96l6vgg"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-a4base r-a4classif r-a4core r-a4preproc r-a4reporting))
    (home-page "https://bioconductor.org/packages/a4/")
    (synopsis "Automated Affymetrix array analysis umbrella package")
    (description
     "This package provides a software suite for the automated analysis of
Affymetrix arrays.")
    (license license:gpl3)))

(define-public r-abseqr
  (package
    (name "r-abseqr")
    (version "1.22.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "abseqR" version))
       (sha256
        (base32
         "0aj3kh4c8zk60n6yyh2jblbrvv77071r96m0mrcslqm71dwg9402"))))
    (properties `((upstream-name . "abseqR")))
    (build-system r-build-system)
    (inputs
     (list pandoc))
    (propagated-inputs
     (list r-biocparallel
           r-biocstyle
           r-circlize
           r-flexdashboard
           r-ggcorrplot
           r-ggdendro
           r-ggplot2
           r-gridextra
           r-knitr
           r-plotly
           r-plyr
           r-png
           r-rcolorbrewer
           r-reshape2
           r-rmarkdown
           r-stringr
           r-vegan
           r-venndiagram))
    (native-inputs
     (list r-knitr))
    (home-page "https://github.com/malhamdoosh/abseqR")
    (synopsis "Reporting and data analysis for Rep-Seq datasets of antibody libraries")
    (description
     "AbSeq is a comprehensive bioinformatic pipeline for the analysis of
sequencing datasets generated from antibody libraries and abseqR is one of its
packages.  AbseqR empowers the users of abseqPy with plotting and reporting
capabilities and allows them to generate interactive HTML reports for the
convenience of viewing and sharing with other researchers.  Additionally,
abseqR extends abseqPy to compare multiple repertoire analyses and perform
further downstream analysis on its output.")
    (license license:gpl3)))

(define-public r-bacon
  (package
    (name "r-bacon")
    (version "1.32.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "bacon" version))
       (sha256
        (base32
         "09lvjb0fa70kcl7mkp4agxv6jcm2ncmnrffav6zmz038imhmvdgm"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocparallel r-ellipse r-ggplot2))
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/bacon/")
    (synopsis "Controlling bias and inflation in association studies")
    (description
     "Bacon can be used to remove inflation and bias often observed in
epigenome- and transcriptome-wide association studies.  To this end bacon
constructs an empirical null distribution using a Gibbs Sampling algorithm by
fitting a three-component normal mixture on z-scores.")
    (license license:gpl2+)))

(define-public r-rgadem
  (package
    (name "r-rgadem")
    (version "2.52.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "rGADEM" version))
       (sha256
        (base32
         "1d1vfnxj1p4y1074ybmkxv4x3v0i3sn5w3k4bddzlrqbzyb9kkna"))))
    (properties `((upstream-name . "rGADEM")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biostrings r-bsgenome r-genomicranges r-iranges r-seqlogo))
    (home-page "https://bioconductor.org/packages/rGADEM/")
    (synopsis "De novo sequence motif discovery")
    (description
     "rGADEM is an efficient de novo motif discovery tool for large-scale
genomic sequence data.")
    (license license:artistic2.0)))

(define-public r-motiv
  (package
    (name "r-motiv")
    (version "1.43.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "MotIV" version))
       (sha256
        (base32
         "1yqqymcrnwlpv6h3w80yliv19922g32xqlqszaqjk6zp853qilh6"))))
    (properties `((upstream-name . "MotIV")))
    (build-system r-build-system)
    (inputs
     (list gsl))
    (propagated-inputs
     (list r-biocgenerics
           r-biostrings
           r-genomicranges
           r-iranges
           r-lattice
           r-rgadem
           r-s4vectors))
    (home-page "https://bioconductor.org/packages/MotIV/")
    (synopsis "Motif identification and validation")
    (description
     "This package is used for the identification and validation of sequence
motifs.  It makes use of STAMP for comparing a set of motifs to a given
database (e.g. JASPAR).  It can also be used to visualize motifs, motif
distributions, modules and filter motifs.")
    (license license:gpl2)))

(define-public r-motifdb
  (package
   (name "r-motifdb")
   (version "1.46.0")
   (source (origin
            (method url-fetch)
            (uri (bioconductor-uri "MotifDb" version))
            (sha256
             (base32 "1m8y76gg80625xz9g9101mfq19glk37p792yzz46807f7g6m8zbh"))))
   (properties `((upstream-name . "MotifDb")))
   (build-system r-build-system)
   (propagated-inputs
    (list r-biocgenerics
          r-biostrings
          r-genomicranges
          r-iranges
          r-rtracklayer
          r-s4vectors
          r-splitstackshape))
   (native-inputs
     (list r-formatr r-knitr r-markdown r-rmarkdown))
   (home-page "https://www.bioconductor.org/packages/MotifDb/")
   (synopsis "Annotated collection of protein-DNA binding sequence motifs")
   (description "This package provides more than 2000 annotated position
frequency matrices from nine public sources, for multiple organisms.")
   (license license:artistic2.0)))

(define-public r-motifbreakr
  (package
   (name "r-motifbreakr")
   (version "2.18.0")
   (source (origin
            (method url-fetch)
            (uri (bioconductor-uri "motifbreakR" version))
            (sha256
             (base32 "1mxs83l8yrbcx353h0wpc0lczn1jc0y5vg59y9g9dabk41csib47"))))
   (properties `((upstream-name . "motifbreakR")))
   (build-system r-build-system)
   (propagated-inputs
    (list r-biocgenerics
          r-biocparallel
          r-biostrings
          r-bsgenome
          r-genomeinfodb
          r-genomicranges
          r-gviz
          r-iranges
          r-matrixstats
          r-motifdb
          r-motifstack
          r-pwalign
          r-rtracklayer
          r-s4vectors
          r-stringr
          r-summarizedexperiment
          r-tfmpvalue
          r-variantannotation))
   (native-inputs
     (list r-knitr))
   (home-page "https://www.bioconductor.org/packages/motifbreakR/")
   (synopsis "Predicting disruptiveness of single nucleotide polymorphisms")
   (description "This package allows biologists to judge in the first place
whether the sequence surrounding the polymorphism is a good match, and in
the second place how much information is gained or lost in one allele of
the polymorphism relative to another.  This package gives a choice of
algorithms for interrogation of genomes with motifs from public sources:
@enumerate
@item a weighted-sum probability matrix;
@item log-probabilities;
@item weighted by relative entropy.
@end enumerate

This package can predict effects for novel or previously described variants in
public databases, making it suitable for tasks beyond the scope of its original
design.  Lastly, it can be used to interrogate any genome curated within
Bioconductor.")
   (license license:gpl2+)))

(define-public r-motifstack
  (package
    (name "r-motifstack")
    (version "1.48.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "motifStack" version))
       (sha256
        (base32
         "15v4z1a0kg4ic411aprzc9b7i2l6c456sfj3jaxy95anw3j3iskj"))
       (snippet
        '(delete-file "inst/htmlwidgets/lib/d3/d3.v4.min.js"))))
    (properties `((upstream-name . "motifStack")))
    (build-system r-build-system)
    (arguments
     (list
      #:phases
      '(modify-phases %standard-phases
         (add-after 'unpack 'process-javascript
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "inst/htmlwidgets/lib/d3"
               (let ((source (assoc-ref inputs "_"))
                     (target "d3.v4.min.js"))
                 (invoke "esbuild" source "--minify"
                         (string-append "--outfile=" target)))))))))
    (propagated-inputs
     (list r-ade4
           r-biostrings
           r-ggplot2
           r-htmlwidgets
           r-tfbstools
           r-xml))
    (native-inputs
     (list esbuild r-knitr
           (origin
             (method url-fetch)
             (uri "https://web.archive.org/web/20230428092426id_/\
https://d3js.org/d3.v4.js")
             (sha256
              (base32
               "0y7byf6kcinfz9ac59jxc4v6kppdazmnyqfav0dm4h550fzfqqlg")))))
    (home-page "https://bioconductor.org/packages/motifStack/")
    (synopsis "Plot stacked logos for DNA, RNA and amino acid sequences")
    (description
     "The motifStack package is designed for graphic representation of
multiple motifs with different similarity scores.  It works with both DNA/RNA
sequence motifs and amino acid sequence motifs.  In addition, it provides the
flexibility for users to customize the graphic parameters such as the font
type and symbol colors.")
    (license license:gpl2+)))

(define-public r-genomicscores
  (package
    (name "r-genomicscores")
    (version "2.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "GenomicScores" version))
       (sha256
        (base32
         "11sabicr2jm1cc9rdjr6vjqhf0gd9d7k04qlfhcdssvh02f957ia"))))
    (properties `((upstream-name . "GenomicScores")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotationhub
           r-biobase
           r-biocfilecache
           r-biocgenerics
           r-biocmanager
           r-biostrings
           r-delayedarray
           r-genomeinfodb
           r-genomicranges
           r-hdf5array
           r-httr
           r-iranges
           r-rhdf5
           r-s4vectors
           r-xml))
    (native-inputs
     (list r-knitr))
    (home-page "https://github.com/rcastelo/GenomicScores/")
    (synopsis "Work with genome-wide position-specific scores")
    (description
     "This package provides infrastructure to store and access genome-wide
position-specific scores within R and Bioconductor.")
    (license license:artistic2.0)))

(define-public r-genomicstate
  (package
    (name "r-genomicstate")
    (version "0.99.15")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "GenomicState" version
                              'annotation))
       (sha256
        (base32 "1r7z3n6wyrd2cclj5b7sg15wpmjdh9k5b1hjlw7jjx8j384l7l1h"))))
    (properties `((upstream-name . "GenomicState")))
    (build-system r-build-system)
    (propagated-inputs (list r-annotationdbi
                             r-annotationhub
                             r-bumphunter
                             r-derfinder
                             r-genomeinfodb
                             r-genomicfeatures
                             r-iranges
                             r-org-hs-eg-db
                             r-rtracklayer))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/LieberInstitute/GenomicState")
    (synopsis "Build and access GenomicState objects")
    (description
     "This package contains functions for building @code{GenomicState} objects
from different annotation sources such as Gencode.  It also provides access to
these files at JHPCE.")
    (license license:artistic2.0)))

(define-public r-atacseqqc
  (package
    (name "r-atacseqqc")
    (version "1.28.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ATACseqQC" version))
       (sha256
        (base32
         "0licdpzhh6xlma0hnvs4rr4bjr5q4q3g18kipf0is47zj4l91smy"))))
    (properties `((upstream-name . "ATACseqQC")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocgenerics
           r-biocparallel
           r-biostrings
           r-bsgenome
           r-chippeakanno
           r-edger
           r-genomeinfodb
           r-genomicalignments
           r-genomicranges
           r-genomicscores
           r-iranges
           r-kernsmooth
           r-limma
           r-motifstack
           r-preseqr
           r-randomforest
           r-rsamtools
           r-rtracklayer
           r-s4vectors))
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/ATACseqQC/")
    (synopsis "ATAC-seq quality control")
    (description
     "ATAC-seq, an assay for Transposase-Accessible Chromatin using
sequencing, is a rapid and sensitive method for chromatin accessibility
analysis.  It was developed as an alternative method to MNase-seq, FAIRE-seq
and DNAse-seq.  The ATACseqQC package was developed to help users to quickly
assess whether their ATAC-seq experiment is successful.  It includes
diagnostic plots of fragment size distribution, proportion of mitochondria
reads, nucleosome positioning pattern, and CTCF or other Transcript Factor
footprints.")
    (license license:gpl2+)))

(define-public r-gofuncr
  (package
    (name "r-gofuncr")
    (version "1.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "GOfuncR" version))
       (sha256
        (base32
         "1g3nppcmyj81ia3v9zvsr8jplk8kmvssspvxd94zxh6yb3wrrc6v"))))
    (properties `((upstream-name . "GOfuncR")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotationdbi
           r-genomicranges
           r-gtools
           r-iranges
           r-mapplots
           r-rcpp
           r-vioplot))
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/GOfuncR/")
    (synopsis "Gene ontology enrichment using FUNC")
    (description
     "GOfuncR performs a gene ontology enrichment analysis based on the
ontology enrichment software FUNC.  GO-annotations are obtained from
OrganismDb or OrgDb packages (@code{Homo.sapiens} by default); the GO-graph is
included in the package and updated regularly.  GOfuncR provides the standard
candidate vs background enrichment analysis using the hypergeometric test, as
well as three additional tests:

@enumerate
@item the Wilcoxon rank-sum test that is used when genes are ranked,
@item a binomial test that is used when genes are associated with two counts,
  and
@item a Chi-square or Fisher's exact test that is used in cases when genes are
associated with four counts.
@end enumerate

To correct for multiple testing and interdependency of the tests, family-wise
error rates are computed based on random permutations of the gene-associated
variables.  GOfuncR also provides tools for exploring the ontology graph and
the annotations, and options to take gene-length or spatial clustering of
genes into account.  It is also possible to provide custom gene coordinates,
annotations and ontologies.")
    (license license:gpl2+)))

(define-public r-abaenrichment
  (package
    (name "r-abaenrichment")
    (version "1.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ABAEnrichment" version))
       (sha256
        (base32
         "1sp3f72rzlr822dxx42bswynrwwfx6f520hdhfdikqp13p2y4044"))))
    (properties `((upstream-name . "ABAEnrichment")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-abadata
           r-data-table
           r-gofuncr
           r-gplots
           r-gtools
           r-rcpp))
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/ABAEnrichment/")
    (synopsis "Gene expression enrichment in human brain regions")
    (description
     "The package ABAEnrichment is designed to test for enrichment of user
defined candidate genes in the set of expressed genes in different human brain
regions.  The core function @code{aba_enrich} integrates the expression of the
candidate gene set (averaged across donors) and the structural information of
the brain using an ontology, both provided by the Allen Brain Atlas project.")
    (license license:gpl2+)))

(define-public r-annotationfuncs
  (package
    (name "r-annotationfuncs")
    (version "1.40.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "AnnotationFuncs" version))
       (sha256
        (base32
         "0xsm7741zm81bi4c9hy0zaacnk8a6bahdpc6srqzrbsz0pfzdyhr"))))
    (properties
     `((upstream-name . "AnnotationFuncs")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotationdbi r-dbi))
    (home-page "https://www.iysik.com/r/annotationfuncs")
    (synopsis "Annotation translation functions")
    (description
     "This package provides functions for handling translating between
different identifieres using the Biocore Data Team data-packages (e.g.
@code{org.Bt.eg.db}).")
    (license license:gpl2)))

(define-public r-annotationtools
  (package
    (name "r-annotationtools")
    (version "1.78.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "annotationTools" version))
       (sha256
        (base32
         "1hwawrp0pbrm9xf7fn65nj0q4s49qlnmxfjlmrv5fykyn3i1lhpq"))))
    (properties
     `((upstream-name . "annotationTools")))
    (build-system r-build-system)
    (propagated-inputs (list r-biobase))
    (home-page "https://bioconductor.org/packages/annotationTools/")
    (synopsis "Annotate microarrays and perform gene expression analyses")
    (description
     "This package provides functions to annotate microarrays, find orthologs,
and integrate heterogeneous gene expression profiles using annotation and
other molecular biology information available as flat file database (plain
text files).")
    ;; Any version of the GPL.
    (license (list license:gpl2+))))

(define-public r-allelicimbalance
  (package
    (name "r-allelicimbalance")
    (version "1.42.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "AllelicImbalance" version))
       (sha256
        (base32
         "0mbmw450gdlr46ggvhrgimkbir3lg5dnqsfadn7yyrn7wkcr302f"))))
    (properties
     `((upstream-name . "AllelicImbalance")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotationdbi
           r-biocgenerics
           r-biostrings
           r-bsgenome
           r-genomeinfodb
           r-genomicalignments
           r-genomicfeatures
           r-genomicranges
           r-gridextra
           r-gviz
           r-iranges
           r-lattice
           r-latticeextra
           r-nlme
           r-rsamtools
           r-s4vectors
           r-seqinr
           r-summarizedexperiment
           r-variantannotation))
    (native-inputs
     (list r-knitr))
    (home-page "https://github.com/pappewaio/AllelicImbalance")
    (synopsis "Investigate allele-specific expression")
    (description
     "This package provides a framework for allele-specific expression
investigation using RNA-seq data.")
    (license license:gpl3)))

(define-public r-aucell
  (package
    (name "r-aucell")
    (version "1.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "AUCell" version))
       (sha256
        (base32
         "009ha82393giapvmfw2mi8pmi88f61mr69qj64y5vphcr1smvcl0"))))
    (properties `((upstream-name . "AUCell")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocgenerics
           r-data-table
           r-delayedarray
           r-delayedmatrixstats
           r-gseabase
           r-matrix
           r-mixtools
           r-r-utils
           r-summarizedexperiment))
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/AUCell/")
    (synopsis "Analysis of gene set activity in single-cell RNA-seq data")
    (description
     "AUCell identifies cells with active gene sets (e.g. signatures,
gene modules, etc) in single-cell RNA-seq data.  AUCell uses the @dfn{Area
Under the Curve} (AUC) to calculate whether a critical subset of the input
gene set is enriched within the expressed genes for each cell.  The
distribution of AUC scores across all the cells allows exploring the relative
expression of the signature.  Since the scoring method is ranking-based,
AUCell is independent of the gene expression units and the normalization
procedure.  In addition, since the cells are evaluated individually, it can
easily be applied to bigger datasets, subsetting the expression matrix if
needed.")
    (license license:gpl3)))

(define-public r-ebimage
  (package
    (name "r-ebimage")
    (version "4.46.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "EBImage" version))
       (sha256
        (base32
         "0qaafn9l9lwrj0yph74g39pghjpg009qz7by64frxg1q0j7ksd2w"))))
    (properties `((upstream-name . "EBImage")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-abind
           r-biocgenerics
           r-fftwtools
           r-htmltools
           r-htmlwidgets
           r-jpeg
           r-locfit
           r-png
           r-rcurl
           r-tiff))
    (native-inputs
     (list r-knitr)) ; for vignettes
    (home-page "https://github.com/aoles/EBImage")
    (synopsis "Image processing and analysis toolbox for R")
    (description
     "EBImage provides general purpose functionality for image processing and
analysis.  In the context of (high-throughput) microscopy-based cellular
assays, EBImage offers tools to segment cells and extract quantitative
cellular descriptors.  This allows the automation of such tasks using the R
programming language and facilitates the use of other tools in the R
environment for signal processing, statistical modeling, machine learning and
visualization with image data.")
    ;; Any version of the LGPL.
    (license license:lgpl2.1+)))

(define-public r-yamss
  (package
    (name "r-yamss")
    (version "1.30.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "yamss" version))
       (sha256
        (base32
         "0l9wqijwlq1gwybj2fy6jwgq6ja1mvx8yfdv174g5mjflx8nmc06"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocgenerics
           r-data-table
           r-ebimage
           r-iranges
           r-limma
           r-matrix
           r-mzr
           r-s4vectors
           r-summarizedexperiment))
    (native-inputs
     (list r-knitr))
    (home-page "https://github.com/hansenlab/yamss")
    (synopsis "Tools for high-throughput metabolomics")
    (description
     "This package provides tools to analyze and visualize high-throughput
metabolomics data acquired using chromatography-mass spectrometry.  These tools
preprocess data in a way that enables reliable and powerful differential
analysis.")
    (license license:artistic2.0)))

(define-public r-gtrellis
  (package
    (name "r-gtrellis")
    (version "1.36.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "gtrellis" version))
       (sha256
        (base32
         "1qlsjpjxcw96qhrqjlbbinpry9yagn50jc3dnz459dbfvnywr82q"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-circlize r-genomicranges r-getoptlong r-iranges))
    (native-inputs
     (list r-knitr))
    (home-page "https://github.com/jokergoo/gtrellis")
    (synopsis "Genome level Trellis layout")
    (description
     "Genome level Trellis graph visualizes genomic data conditioned by
genomic categories (e.g. chromosomes).  For each genomic category, multiple
dimensional data which are represented as tracks describe different features
from different aspects.  This package provides high flexibility to arrange
genomic categories and to add self-defined graphics in the plot.")
    (license license:expat)))

(define-public r-somaticsignatures
  (package
    (name "r-somaticsignatures")
    (version "2.40.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "SomaticSignatures" version))
       (sha256
        (base32
         "0yvn2famnyxkxnw4r1vr0vma6ym9kb343vqgaqkvcp3zpm1fi0p8"))))
    (properties
     `((upstream-name . "SomaticSignatures")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biobase
           r-biostrings
           r-genomeinfodb
           r-genomicranges
           r-ggbio
           r-ggplot2
           r-iranges
           r-nmf
           r-pcamethods
           r-proxy
           r-reshape2
           r-s4vectors
           r-variantannotation))
    (native-inputs
     (list r-knitr))
    (home-page "https://github.com/juliangehring/SomaticSignatures")
    (synopsis "Somatic signatures")
    (description
     "This package identifies mutational signatures of @dfn{single nucleotide
variants} (SNVs).  It provides a infrastructure related to the methodology
described in Nik-Zainal (2012, Cell), with flexibility in the matrix
decomposition algorithms.")
    (license license:expat)))

(define-public r-yapsa
  (package
    (name "r-yapsa")
    (version "1.30.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "YAPSA" version))
       (sha256
        (base32
         "0908hyji07br1i1zms21a99m3mvi707xj8zbd1w64gzackghabhl"))))
    (properties `((upstream-name . "YAPSA")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biostrings
           r-bsgenome-hsapiens-ucsc-hg19
           r-circlize
           r-complexheatmap
           r-corrplot
           r-dendextend
           r-doparallel
           r-dplyr
           r-genomeinfodb
           r-genomicranges
           r-getoptlong
           r-ggbeeswarm
           r-ggplot2
           r-gridextra
           r-gtrellis
           r-keggrest
           r-limsolve
           r-magrittr
           r-pmcmrplus
           r-pracma
           r-reshape2
           r-somaticsignatures
           r-variantannotation))
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/YAPSA/")
    (synopsis "Yet another package for signature analysis")
    (description
     "This package provides functions and routines useful in the analysis of
somatic signatures (cf. L. Alexandrov et al., Nature 2013).  In particular,
functions to perform a signature analysis with known signatures and a
signature analysis on @dfn{stratified mutational catalogue} (SMC) are
provided.")
    (license license:gpl3)))

(define-public r-gcrma
  (package
    (name "r-gcrma")
    (version "2.76.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "gcrma" version))
       (sha256
        (base32
         "01b9b4v19yh42552awl21yyx9vhf9yh9r3l8849x4zwjpnf6anp3"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-affy
           r-affyio
           r-biobase
           r-biocmanager
           r-biostrings
           r-xvector))
    (home-page "https://bioconductor.org/packages/gcrma/")
    (synopsis "Background adjustment using sequence information")
    (description
     "Gcrma adjusts for background intensities in Affymetrix array data which
include optical noise and @dfn{non-specific binding} (NSB).  The main function
@code{gcrma} converts background adjusted probe intensities to expression
measures using the same normalization and summarization methods as a
@dfn{Robust Multiarray Average} (RMA).  Gcrma uses probe sequence information
to estimate probe affinity to NSB.  The sequence information is summarized in
a more complex way than the simple GC content.  Instead, the base types (A, T,
G or C) at each position along the probe determine the affinity of each probe.
The parameters of the position-specific base contributions to the probe
affinity is estimated in an NSB experiment in which only NSB but no
gene-specific binding is expected.")
    ;; Any version of the LGPL
    (license license:lgpl2.1+)))

(define-public r-simpleaffy
  (package
    (name "r-simpleaffy")
    (version "2.66.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "simpleaffy" version))
       (sha256
        (base32
         "04a11dsqd5y4b39nny94acnh0qhdazjc6d1803izza4vrgmw2csb"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-affy r-biobase r-biocgenerics r-gcrma r-genefilter))
    (home-page "https://bioconductor.org/packages/simpleaffy/")
    (synopsis "Very simple high level analysis of Affymetrix data")
    (description
     "This package provides high level functions for reading Affy @file{.CEL}
files, phenotypic data, and then computing simple things with it, such as
t-tests, fold changes and the like.  It makes heavy use of the @code{affy}
library.  It also has some basic scatter plot functions and mechanisms for
generating high resolution journal figures.")
    (license license:gpl2+)))

(define-public r-yaqcaffy
  (package
    (name "r-yaqcaffy")
    (version "1.50.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "yaqcaffy" version))
       (sha256
        (base32
         "18gphcjj15iivrahp52186bvdg07yd2dvrykfjdd4r1vyf33im96"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-simpleaffy))
    (home-page "https://bioconductor.org/packages/yaqcaffy/")
    (synopsis "Affymetrix quality control and reproducibility analysis")
    (description
     "This is a package that can be used for quality control of Affymetrix
GeneChip expression data and reproducibility analysis of human whole genome
chips with the MAQC reference datasets.")
    (license license:artistic2.0)))

(define-public r-quantro
  (package
    (name "r-quantro")
    (version "1.38.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "quantro" version))
       (sha256
        (base32
         "0b8afvnczkhmbdg5zna2h5v42zdyvk1zybn9cwcbw22rijiysq99"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biobase
           r-doparallel
           r-foreach
           r-ggplot2
           r-iterators
           r-minfi
           r-rcolorbrewer))
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/quantro/")
    (synopsis "Test for when to use quantile normalization")
    (description
     "This package provides a data-driven test for the assumptions of quantile
normalization using raw data such as objects that inherit eSets (e.g.
ExpressionSet, MethylSet).  Group level information about each sample (such as
Tumor / Normal status) must also be provided because the test assesses if
there are global differences in the distributions between the user-defined
groups.")
    (license license:gpl3+)))

(define-public r-yarn
  (package
    (name "r-yarn")
    (version "1.30.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "yarn" version))
       (sha256
        (base32
         "1r0dj65i8xg1yb6x7ggb3s40hvzbh596k2g9bxblsbl18z867ydp"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biobase
           r-biomart
           r-downloader
           r-edger
           r-gplots
           r-limma
           r-matrixstats
           r-preprocesscore
           r-quantro
           r-rcolorbrewer
           r-readr))
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/yarn/")
    (synopsis "Robust multi-condition RNA-Seq preprocessing and normalization")
    (description
     "Expedite large RNA-Seq analyses using a combination of previously
developed tools.  YARN is meant to make it easier for the user in performing
basic mis-annotation quality control, filtering, and condition-aware
normalization.  YARN leverages many Bioconductor tools and statistical
techniques to account for the large heterogeneity and sparsity found in very
large RNA-seq experiments.")
    (license license:artistic2.0)))

(define-public r-roar
  (package
    (name "r-roar")
    (version "1.40.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "roar" version))
       (sha256
        (base32
         "06d1w5kknm2mb6v6glmyd97n1a33529kgqkkyfbk277ikn45hi9f"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocgenerics
           r-genomeinfodb
           r-genomicalignments
           r-genomicranges
           r-iranges
           r-rtracklayer
           r-s4vectors
           r-summarizedexperiment))
    (home-page "https://github.com/vodkatad/roar/")
    (synopsis "Identify differential APA usage from RNA-seq alignments")
    (description
     "This package provides tools for identifying preferential usage of APA
sites, comparing two biological conditions, starting from known alternative
sites and alignments obtained from standard RNA-seq experiments.")
    (license license:gpl3)))

(define-public r-xbseq
  (package
    (name "r-xbseq")
    (version "1.22.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "XBSeq" version))
       (sha256
        (base32
         "1dvk2jpsdynqw5071z54yd5j0ddprhc1ppk834cz9liibd72d7vz"))))
    (properties `((upstream-name . "XBSeq")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biobase
           r-deseq2
           r-dplyr
           r-ggplot2
           r-locfit
           r-magrittr
           r-matrixstats
           r-pracma
           r-roar))
    (native-inputs
     (list r-knitr))
    (home-page "https://github.com/Liuy12/XBSeq")
    (synopsis "Test for differential expression for RNA-seq data")
    (description
     "XBSeq is a novel algorithm for testing RNA-seq @dfn{differential
expression} (DE), where a statistical model was established based on the
assumption that observed signals are the convolution of true expression
signals and sequencing noises.  The mapped reads in non-exonic regions are
considered as sequencing noises, which follows a Poisson distribution.  Given
measurable observed signal and background noise from RNA-seq data, true
expression signals, assuming governed by the negative binomial distribution,
can be delineated and thus the accurate detection of differential expressed
genes.")
    (license license:gpl3+)))

(define-public r-massspecwavelet
  (package
    (name "r-massspecwavelet")
    (version "1.70.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "MassSpecWavelet" version))
       (sha256
        (base32
         "0ckvnah0ck5z6calgrwk0k3y1nxpcyn41y84kz5bypzr77x6ssj2"))))
    (properties
     `((upstream-name . "MassSpecWavelet")))
    (build-system r-build-system)
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/MassSpecWavelet/")
    (synopsis "Mass spectrum processing by wavelet-based algorithms")
    (description
     "The MassSpecWavelet package aims to process @dfn{Mass Spectrometry} (MS)
data mainly through the use of wavelet transforms.  It supports peak detection
based on @dfn{Continuous Wavelet Transform} (CWT).")
    (license license:lgpl2.0+)))

(define-public r-xcms
  (package
    (name "r-xcms")
    (version "4.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "xcms" version))
       (sha256
        (base32
         "1zk9l4ll3047fj0rngvjka2hkg2by26n2h3xm1iyvfiw5068y74y"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biobase
           r-biocgenerics
           r-biocparallel
           r-iranges
           r-jsonlite
           r-lattice
           r-massspecwavelet
           r-metabocoreutils
           r-mscoreutils
           r-msexperiment
           r-msfeatures
           r-msnbase
           r-mzr
           r-progress
           r-protgenerics
           r-rcolorbrewer
           r-s4vectors
           r-spectra
           r-summarizedexperiment))
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/xcms/")
    (synopsis "LC/MS and GC/MS mass spectrometry data analysis")
    (description
     "This package provides a framework for processing and visualization of
chromatographically separated and single-spectra mass spectral data.  It
imports from AIA/ANDI NetCDF, mzXML, mzData and mzML files.  It preprocesses
data for high-throughput, untargeted analyte profiling.")
    (license license:gpl2+)))

(define-public r-wppi
  (package
    (name "r-wppi")
    (version "1.12.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "wppi" version))
              (sha256
               (base32
                "1mvr5bpsng87yl1bhd22h45830vw3sla1ysnr29px13if2q5ylmy"))))
    (properties `((upstream-name . "wppi")))
    (build-system r-build-system)
    ;; This is necessary because omnipathr attempts to write a configuration
    ;; file to HOME.
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'set-HOME
           (lambda _ (setenv "HOME" "/tmp"))))))
    (propagated-inputs (list r-dplyr
                             r-igraph
                             r-logger
                             r-magrittr
                             r-matrix
                             r-omnipathr
                             r-progress
                             r-purrr
                             r-rcurl
                             r-rlang
                             r-tibble
                             r-tidyr))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/AnaGalhoz37/wppi")
    (synopsis "Weighting protein-protein interactions")
    (description
     "This package predicts functional relevance of protein-protein
interactions based on functional annotations such as Human Protein Ontology
and Gene Ontology, and prioritizes genes based on network topology, functional
scores and a path search algorithm.")
    (license license:expat)))

(define-public r-wrench
  (package
    (name "r-wrench")
    (version "1.22.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Wrench" version))
       (sha256
        (base32
         "1nwbqagpcs38c66r1arbby76if6pafa4snd6zbp18y71jj8rgvl7"))))
    (properties `((upstream-name . "Wrench")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-limma r-locfit r-matrixstats))
    (native-inputs
     (list r-knitr))
    (home-page "https://github.com/HCBravoLab/Wrench")
    (synopsis "Wrench normalization for sparse count data")
    (description
     "Wrench is a package for normalization sparse genomic count data, like
that arising from 16s metagenomic surveys.")
    (license license:artistic2.0)))

(define-public r-wiggleplotr
  (package
    (name "r-wiggleplotr")
    (version "1.28.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "wiggleplotr" version))
       (sha256
        (base32
         "1px7rgvaj6kks6fp7yqsqjwy4knlzgllzxsiimv8nikmbd57dmi0"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-assertthat
           r-cowplot
           r-dplyr
           r-genomeinfodb
           r-genomicranges
           r-ggplot2
           r-iranges
           r-purrr
           r-rtracklayer
           r-s4vectors))
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/wiggleplotr/")
    (synopsis "Make read coverage plots from BigWig files")
    (description
     "This package provides tools to visualize read coverage from sequencing
experiments together with genomic annotations (genes, transcripts, peaks).
Introns of long transcripts can be rescaled to a fixed length for better
visualization of exonic read coverage.")
    (license license:asl2.0)))

(define-public r-widgettools
  (package
    (name "r-widgettools")
    (version "1.82.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "widgetTools" version))
       (sha256
        (base32
         "0knszrfapndnjzv5948qbfsjamlf7g04yry7yx294l8iwvwip6qv"))))
    (properties `((upstream-name . "widgetTools")))
    (build-system r-build-system)
    (home-page "https://bioconductor.org/packages/widgetTools/")
    (synopsis "Tools for creating interactive tcltk widgets")
    (description
     "This package contains tools to support the construction of tcltk
widgets in R.")
    ;; Any version of the LGPL.
    (license license:lgpl3+)))

(define-public r-webbioc
  (package
    (name "r-webbioc")
    (version "1.76.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "webbioc" version))
       (sha256
        (base32
         "1ws4w2kf514adk50sjadgasm7z1xvklcb8s243m12fvlg63dia9i"))))
    (build-system r-build-system)
    (inputs
     (list netpbm perl))
    (propagated-inputs
     (list r-affy
           r-annaffy
           r-biobase
           r-biocmanager
           r-gcrma
           r-multtest
           r-qvalue
           r-vsn))
    (home-page "https://www.bioconductor.org/")
    (synopsis "Bioconductor web interface")
    (description
     "This package provides an integrated web interface for doing microarray
analysis using several of the Bioconductor packages.  It is intended to be
deployed as a centralized bioinformatics resource for use by many users.
Currently only Affymetrix oligonucleotide analysis is supported.")
    (license license:gpl2+)))

(define-public r-zinbwave
  (package
    (name "r-zinbwave")
    (version "1.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "zinbwave" version))
       (sha256
        (base32
         "07zlvzfs1c4gcj9hwgqbjhgbrffi301620chrf5lvzly4v7hafwi"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocparallel
           r-edger
           r-genefilter
           r-matrix
           r-singlecellexperiment
           r-softimpute
           r-summarizedexperiment))
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/zinbwave")
    (synopsis "Zero-inflated negative binomial model for RNA-seq data")
    (description "This package implements a general and flexible zero-inflated
negative binomial model that can be used to provide a low-dimensional
representations of single-cell RNA-seq data.  The model accounts for zero
inflation (dropouts), over-dispersion, and the count nature of the data.
The model also accounts for the difference in library sizes and optionally
for batch effects and/or other covariates, avoiding the need for pre-normalize
the data.")
    (license license:artistic2.0)))

(define-public r-zfpkm
  (package
    (name "r-zfpkm")
    (version "1.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "zFPKM" version))
       (sha256
        (base32
         "1pgmw41xdq9gd2shc6kawgxz9sw1ydlgs0p35k99k2v50f86r2wy"))))
    (properties `((upstream-name . "zFPKM")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-checkmate r-dplyr r-ggplot2 r-summarizedexperiment r-tidyr))
    (native-inputs
     (list r-knitr))
    (home-page "https://github.com/ronammar/zFPKM/")
    (synopsis "Functions to facilitate zFPKM transformations")
    (description
     "This is a package to perform the zFPKM transform on RNA-seq FPKM data.
This algorithm is based on the publication by Hart et al., 2013 (Pubmed ID
24215113).")
    (license license:gpl3)))

(define-public r-rbowtie2
  (package
    (name "r-rbowtie2")
    (version "2.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Rbowtie2" version))
       (sha256
        (base32
         "1spwpbx1jjfjcxbwjkl64ccp9k8xs6lm828546qddsgp20y3bhgp"))))
    (properties `((upstream-name . "Rbowtie2")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-magrittr r-rsamtools))
    (inputs
     (list samtools zlib))
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/Rbowtie2/")
    (synopsis "R wrapper for Bowtie2 and AdapterRemoval")
    (description
     "This package provides an R wrapper of the popular @code{bowtie2}
sequencing reads aligner and @code{AdapterRemoval}, a convenient tool for
rapid adapter trimming, identification, and read merging.")
    (license license:gpl3+)))

(define-public r-progeny
  (package
    (name "r-progeny")
    (version "1.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "progeny" version))
       (sha256
        (base32
         "1f5y1sza9vm7wqd4mp5g7sypc9kbf9zpl015rbl41wz4y3ai21sb"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biobase
           r-decoupler
           r-dplyr
           r-ggplot2
           r-ggrepel
           r-gridextra
           r-reshape2
           r-tidyr))
    (native-inputs
     (list r-knitr))
    (home-page "https://github.com/saezlab/progeny")
    (synopsis "Pathway responsive gene activity inference")
    (description
     "This package provides a function to infer pathway activity from gene
expression.  It contains the linear model inferred in the publication
\"Perturbation-response genes reveal signaling footprints in cancer gene
expression\".")
    (license license:asl2.0)))

(define-public r-arrmnormalization
  (package
    (name "r-arrmnormalization")
    (version "1.44.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ARRmNormalization" version))
       (sha256
        (base32
         "0wg799c5pzw9g0dbkaa5j4jyccvixqbc8x3xnwk3cgi1dsbhpk34"))))
    (properties
     `((upstream-name . "ARRmNormalization")))
    (build-system r-build-system)
    (propagated-inputs (list r-arrmdata))
    (home-page "https://bioconductor.org/packages/ARRmNormalization/")
    (synopsis "Adaptive robust regression normalization for methylation data")
    (description
     "This is a package to perform the @dfn{Adaptive Robust Regression
method} (ARRm) for the normalization of methylation data from the Illumina
Infinium HumanMethylation 450k assay.")
    (license license:artistic2.0)))

(define-public r-biocfilecache
  (package
    (name "r-biocfilecache")
    (version "2.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BiocFileCache" version))
       (sha256
        (base32
         "0jv6hni5x91ssvz4ilhi3y6lylrw4w3xfk8ljmq6gjvyl886sw9f"))))
    (properties `((upstream-name . "BiocFileCache")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-curl
           r-dbi
           r-dbplyr
           r-dplyr
           r-filelock
           r-httr
           r-rsqlite))
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/BiocFileCache/")
    (synopsis "Manage files across sessions")
    (description
     "This package creates a persistent on-disk cache of files that the user
can add, update, and retrieve.  It is useful for managing resources (such as
custom Txdb objects) that are costly or difficult to create, web resources,
and data files used across sessions.")
    (license license:artistic2.0)))

(define-public r-iclusterplus
  (package
    (name "r-iclusterplus")
    (version "1.40.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "iClusterPlus" version))
       (sha256
        (base32
         "01rf4w2p1qw4hk9shsdaffyz9g91vffcn4qi1dllsrn3m8pyd1gm"))))
    (properties `((upstream-name . "iClusterPlus")))
    (build-system r-build-system)
    (native-inputs (list gfortran))
    (home-page "https://bioconductor.org/packages/iClusterPlus/")
    (synopsis "Integrative clustering of multi-type genomic data")
    (description
     "iClusterPlus is developed for integrative clustering analysis of
multi-type genomic data and is an enhanced version of iCluster proposed and
developed by Shen, Olshen and Ladanyi (2009).  Multi-type genomic data arise
from the experiments where biological samples (e.g. tumor samples) are
analyzed by multiple techniques, for instance, @dfn{array comparative genomic
hybridization} (aCGH), gene expression microarray, RNA-seq and DNA-seq, and so
on.  In the iClusterPlus model, binary observations such as somatic mutation
are modeled as Binomial processes; categorical observations such as copy
number states are realizations of Multinomial random variables; counts are
modeled as Poisson random processes; and continuous measures are modeled by
Gaussian distributions.")
    (license license:gpl2+)))

(define-public r-rbowtie
  (package
    (name "r-rbowtie")
    (version "1.44.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Rbowtie" version))
       (sha256
        (base32
         "1in3qani4xh5m5zpbny1nbsvh60ahs36bas8hplkdpd8hnhfla56"))))
    (properties `((upstream-name . "Rbowtie")))
    (build-system r-build-system)
    (arguments
     `(#:phases
       ;; Disable unsupported `popcnt' instructions on
       ;; architectures other than x86_64
       ,(if (string-prefix? "x86_64"
                            (or (%current-target-system)
                                (%current-system)))
            '%standard-phases
            '(modify-phases %standard-phases
               (add-after 'unpack 'patch-sources
                 (lambda _
                   (setenv "POPCNT_CAPABILITY" "0")))))))
    (inputs (list zlib))
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/Rbowtie/")
    (synopsis "R bowtie wrapper")
    (description
     "This package provides an R wrapper around the popular bowtie short read
aligner and around SpliceMap, a de novo splice junction discovery and
alignment tool.")
    (license license:artistic2.0)))

(define-public r-sgseq
  (package
    (name "r-sgseq")
    (version "1.38.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "SGSeq" version))
       (sha256
        (base32
         "1i3dg3fn648mzy49m15vbzr3wfnk91xyph5j4sc7ss978l25h17n"))))
    (properties `((upstream-name . "SGSeq")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotationdbi
           r-biocgenerics
           r-biostrings
           r-genomeinfodb
           r-genomicalignments
           r-genomicfeatures
           r-genomicranges
           r-igraph
           r-iranges
           r-rsamtools
           r-rtracklayer
           r-runit
           r-s4vectors
           r-summarizedexperiment))
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/SGSeq/")
    (synopsis "Splice event prediction and quantification from RNA-seq data")
    (description
     "SGSeq is a package for analyzing splice events from RNA-seq data.  Input
data are RNA-seq reads mapped to a reference genome in BAM format.  Genes are
represented as a splice graph, which can be obtained from existing annotation
or predicted from the mapped sequence reads.  Splice events are identified
from the graph and are quantified locally using structurally compatible reads
at the start or end of each splice variant.  The software includes functions
for splice event prediction, quantification, visualization and
interpretation.")
    (license license:artistic2.0)))

(define-public r-rhisat2
  (package
    (name "r-rhisat2")
    (version "1.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Rhisat2" version))
       (sha256
        (base32
         "0a7k9p08gq537275r7dfzh34ycxx8h4fmac74jimwhsa420793r5"))))
    (properties `((upstream-name . "Rhisat2")))
    (build-system r-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'make-reproducible
           (lambda _
             (substitute* "src/Makefile"
               (("`hostname`") "guix")
               (("`date`") "0")
               ;; Avoid shelling out to "which".
               (("^CC =.*") (which "gcc"))
               (("^CPP =.*") (which "g++")))
             #t)))))
    (propagated-inputs
     (list r-genomicranges r-sgseq r-txdbmaker))
    (native-inputs
     (list r-knitr))
    (home-page "https://github.com/fmicompbio/Rhisat2")
    (synopsis "R Wrapper for HISAT2 sequence aligner")
    (description
     "This package provides an R interface to the HISAT2 spliced short-read
aligner by Kim et al. (2015).  The package contains wrapper functions to
create a genome index and to perform the read alignment to the generated
index.")
    (license license:gpl3)))

(define-public r-quasr
  (package
    (name "r-quasr")
    (version "1.44.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "QuasR" version))
       (sha256
        (base32
         "116qnp7d7z4a56fv6n55yzrdlmma9gwrw234nmy8iapkn5ms3xky"))))
    (properties `((upstream-name . "QuasR")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotationdbi
           r-biobase
           r-biocgenerics
           r-biocparallel
           r-biostrings
           r-bsgenome
           r-genomeinfodb
           r-genomicfeatures
           r-genomicfiles
           r-genomicranges
           r-iranges
           r-rbowtie
           r-rhtslib
           r-rsamtools
           r-rtracklayer
           r-s4vectors
           r-shortread
           r-txdbmaker))
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/QuasR/")
    (synopsis "Quantify and annotate short reads in R")
    (description
     "This package provides a framework for the quantification and analysis of
short genomic reads.  It covers a complete workflow starting from raw sequence
reads, over creation of alignments and quality control plots, to the
quantification of genomic regions of interest.")
    (license license:gpl2)))

(define-public r-rqc
  (package
    (name "r-rqc")
    (version "1.38.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Rqc" version))
       (sha256
        (base32
         "0szd5ygcvpaiwqss8galxzxpslpfh93rk42s865v0qpv0s9v9x0l"))))
    (properties `((upstream-name . "Rqc")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocgenerics
           r-biocparallel
           r-biocstyle
           r-biostrings
           r-biovizbase
           r-genomicalignments
           r-genomicfiles
           r-ggplot2
           r-iranges
           r-knitr
           r-markdown
           r-plyr
           r-rcpp
           r-reshape2
           r-rsamtools
           r-s4vectors
           r-shiny
           r-shortread))
    (native-inputs
     (list r-knitr))
    (home-page "https://github.com/labbcb/Rqc")
    (synopsis "Quality control tool for high-throughput sequencing data")
    (description
     "Rqc is an optimized tool designed for quality control and assessment of
high-throughput sequencing data.  It performs parallel processing of entire
files and produces a report which contains a set of high-resolution
graphics.")
    (license license:gpl2+)))

(define-public r-birewire
  (package
    (name "r-birewire")
    (version "3.36.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BiRewire" version))
       (sha256
        (base32
         "1l3hkq6n7i9j82aym4ngwablj8jqb70a7y6skx0rjkflya6wbim7"))))
    (properties `((upstream-name . "BiRewire")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-igraph r-matrix r-rtsne r-slam))
    (home-page "https://bioconductor.org/packages/release/bioc/html/BiRewire.html")
    (synopsis "Tools for randomization of bipartite graphs")
    (description
     "This package provides functions for bipartite network rewiring through N
consecutive switching steps and for the computation of the minimal number of
switching steps to be performed in order to maximise the dissimilarity with
respect to the original network.  It includes functions for the analysis of
the introduced randomness across the switching steps and several other
routines to analyse the resulting networks and their natural projections.")
    (license license:gpl3)))

(define-public r-birta
  (package
    (name "r-birta")
    (version "1.31.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "birta" version))
       (sha256
        (base32
         "00a1kcfmcgdbx6wpnhk45wm45bynhry5m93l9hm75j2rwyc4lnca"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biobase r-limma r-mass))
    (home-page "https://bioconductor.org/packages/birta")
    (synopsis "Bayesian inference of regulation of transcriptional activity")
    (description
     "Expression levels of mRNA molecules are regulated by different
processes, comprising inhibition or activation by transcription factors and
post-transcriptional degradation by microRNAs.  @dfn{birta} (Bayesian
Inference of Regulation of Transcriptional Activity) uses the regulatory
networks of transcription factors and miRNAs together with mRNA and miRNA
expression data to predict switches in regulatory activity between two
conditions.  A Bayesian network is used to model the regulatory structure and
Markov-Chain-Monte-Carlo is applied to sample the activity states.")
    (license license:gpl2+)))

(define-public r-multidataset
  (package
    (name "r-multidataset")
    (version "1.32.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "MultiDataSet" version))
       (sha256
        (base32
         "1y1j5svi52cvixjsn13122kvd0z0c7zfki3p6rr8wjd5w7riiyqw"))))
    (properties `((upstream-name . "MultiDataSet")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biobase
           r-biocgenerics
           r-genomicranges
           r-ggplot2
           r-ggrepel
           r-iranges
           r-limma
           r-qqman
           r-s4vectors
           r-summarizedexperiment))
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/MultiDataSet/")
    (synopsis "Implementation of MultiDataSet and ResultSet")
    (description
     "This package provides an implementation of the BRGE's (Bioinformatic
Research Group in Epidemiology from Center for Research in Environmental
Epidemiology) MultiDataSet and ResultSet.  MultiDataSet is designed for
integrating multi omics data sets and ResultSet is a container for omics
results.  This package contains base classes for MEAL and rexposome
packages.")
    (license license:expat)))

(define-public r-ropls
  (package
    (name "r-ropls")
    (version "1.36.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ropls" version))
       (sha256
        (base32
         "0cdkwzrbr6h1dfi3qdh629hi2jlm1hxgq4dvx4z3vc2r6w0c6m11"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biobase
           r-ggplot2
           r-multiassayexperiment
           r-multidataset
           r-plotly
           r-summarizedexperiment))
    (native-inputs
     (list r-knitr)) ; for vignettes
    (home-page "https://dx.doi.org/10.1021/acs.jproteome.5b00354")
    (synopsis "Multivariate analysis and feature selection of omics data")
    (description
     "Latent variable modeling with @dfn{Principal Component Analysis} (PCA)
and @dfn{Partial Least Squares} (PLS) are powerful methods for visualization,
regression, classification, and feature selection of omics data where the
number of variables exceeds the number of samples and with multicollinearity
among variables.  @dfn{Orthogonal Partial Least Squares} (OPLS) enables to
separately model the variation correlated (predictive) to the factor of
interest and the uncorrelated (orthogonal) variation.  While performing
similarly to PLS, OPLS facilitates interpretation.

This package provides imlementations of PCA, PLS, and OPLS for multivariate
analysis and feature selection of omics data.  In addition to scores, loadings
and weights plots, the package provides metrics and graphics to determine the
optimal number of components (e.g. with the R2 and Q2 coefficients), check the
validity of the model by permutation testing, detect outliers, and perform
feature selection (e.g. with Variable Importance in Projection or regression
coefficients).")
    (license license:cecill)))

(define-public r-biosigner
  (package
    (name "r-biosigner")
    (version "1.32.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "biosigner" version))
       (sha256
        (base32
         "1c56s8lr5cdx27v0rdi4l4h680x6ga9wazf4dj1xkqyliqz65ps5"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biobase
           r-e1071
           r-multiassayexperiment
           r-multidataset
           r-randomforest
           r-ropls
           r-summarizedexperiment))
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/biosigner/")
    (synopsis "Signature discovery from omics data")
    (description
     "Feature selection is critical in omics data analysis to extract
restricted and meaningful molecular signatures from complex and high-dimension
data, and to build robust classifiers.  This package implements a method to
assess the relevance of the variables for the prediction performances of the
classifier.  The approach can be run in parallel with the PLS-DA, Random
Forest, and SVM binary classifiers.  The signatures and the corresponding
'restricted' models are returned, enabling future predictions on new
datasets.")
    (license license:cecill)))

(define-public r-annotatr
  (package
    (name "r-annotatr")
    (version "1.30.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "annotatr" version))
       (sha256
        (base32
         "11cc3nknbckm0v5r7dzgqzw0137w4c08ab10s9svb1y02z45a0qz"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotationdbi
           r-annotationhub
           r-dplyr
           r-genomeinfodb
           r-genomicfeatures
           r-genomicranges
           r-ggplot2
           r-iranges
           r-readr
           r-regioner
           r-reshape2
           r-rtracklayer
           r-s4vectors))
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/annotatr/")
    (synopsis "Annotation of genomic regions to genomic annotations")
    (description
     "Given a set of genomic sites/regions (e.g. ChIP-seq peaks, CpGs,
differentially methylated CpGs or regions, SNPs, etc.) it is often of interest
to investigate the intersecting genomic annotations.  Such annotations include
those relating to gene models (promoters, 5'UTRs, exons, introns, and 3'UTRs),
CpGs (CpG islands, CpG shores, CpG shelves), or regulatory sequences such as
enhancers.  The annotatr package provides an easy way to summarize and
visualize the intersection of genomic sites/regions with genomic
annotations.")
    (license license:gpl3)))

(define-public r-rsubread
  (package
    (name "r-rsubread")
    (version "2.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Rsubread" version))
       (sha256
        (base32
         "0nd5b1k0dsxpf93wqx01fgbvk40jcfs7b7w692bsx620lm42ipfg"))))
    (properties `((upstream-name . "Rsubread")))
    (build-system r-build-system)
    (inputs (list zlib))
    (propagated-inputs
     (list r-matrix))
    (home-page "https://bioconductor.org/packages/Rsubread/")
    (synopsis "Subread sequence alignment and counting for R")
    (description
     "This package provides tools for alignment, quantification and analysis
of second and third generation sequencing data.  It includes functionality for
read mapping, read counting, SNP calling, structural variant detection and
gene fusion discovery.  It can be applied to all major sequencing techologies
and to both short and long sequence reads.")
    (license license:gpl3)))

(define-public r-flames
  (package
    (name "r-flames")
    (version "1.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "FLAMES" version))
       (sha256
        (base32 "1fcz0v1vy0q5zmdpawlk4a7fbqd01954p0jby9nbzgbk85hl16mi"))))
    (properties `((upstream-name . "FLAMES")))
    (build-system r-build-system)
    (arguments
     (list
      #:phases
      '(modify-phases %standard-phases
         (add-after 'unpack 'fix-build-system
           (lambda _
             ;; One target uses & instead of &&, which leads to a command
             ;; being run despite the check failing.
             (substitute* "src/Makevars"
               ((" & ") " && ")))))))
    (propagated-inputs
     (list r-bambu
           r-basilisk
           r-biocgenerics
           r-biostrings
           r-circlize
           r-complexheatmap
           r-cowplot
           r-dplyr
           r-dropletutils
           r-future
           r-genomeinfodb
           r-genomicalignments
           r-genomicfeatures
           r-genomicranges
           r-ggbio
           r-ggplot2
           r-gridextra
           r-igraph
           r-iranges
           r-jsonlite
           r-magrittr
           r-matrix
           r-multiassayexperiment
           r-rcolorbrewer
           r-rcpp
           r-reticulate
           r-rhtslib
           r-rsamtools
           r-rtracklayer
           r-s4vectors
           r-scater
           r-scran
           r-scuttle
           r-singlecellexperiment
           r-stringr
           r-summarizedexperiment
           r-testthat
           r-tibble
           r-tidyr
           r-tidyselect
           r-txdbmaker
           r-withr
           r-zlibbioc))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/OliverVoogd/FLAMES")
    (synopsis
     "Full Length Analysis of Mutations and Splicing in long read RNA-seq data")
    (description
     "This is a package for semi-supervised isoform detection and annotation
from both bulk and single-cell long read RNA-seq data.  Flames provides
automated pipelines for analysing isoforms, as well as intermediate functions
for manual execution.")
    (license license:gpl2+)))

(define-public r-flowai
  (package
    (name "r-flowai")
    (version "1.34.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "flowAI" version))
              (sha256
               (base32
                "1siqza92iak5lz38hbfznqndrmq995ihp6sbam0n2d3mj8vwv9qa"))))
    (properties `((upstream-name . "flowAI")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-changepoint
           r-flowcore
           r-ggplot2
           r-knitr
           r-plyr
           r-rcolorbrewer
           r-reshape2
           r-rmarkdown
           r-scales))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/flowAI")
    (synopsis
     "Automatic and interactive quality control for flow cytometry data")
    (description
     "This package is able to perform an automatic or interactive quality
control on FCS data acquired using flow cytometry instruments.  By evaluating
three different properties:

@enumerate
@item flow rate
@item signal acquisition, and
@item dynamic range,
@end enumerate

the quality control enables the detection and removal of anomalies.")
    (license license:gpl2+)))

(define-public r-flowutils
  (package
    (name "r-flowutils")
    (version "1.59.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "flowUtils" version))
       (sha256
        (base32
         "11x362dqb9mjlsbq6g1qkb8hhnkvm22z5s3wkgmpyy9kyifjkm26"))))
    (properties `((upstream-name . "flowUtils")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biobase
           r-corpcor
           r-flowcore
           r-graph
           r-runit
           r-xml))
    (home-page "https://github.com/jspidlen/flowUtils")
    (synopsis "Utilities for flow cytometry")
    (description
     "This package provides utilities for flow cytometry data.")
    (license license:artistic2.0)))

(define-public r-consensusclusterplus
  (package
    (name "r-consensusclusterplus")
    (version "1.68.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ConsensusClusterPlus" version))
       (sha256
        (base32
         "0y91kz1n3byadk505432cnlj9dfiw521fgb460d6dmly9s0d9yif"))))
    (properties
     `((upstream-name . "ConsensusClusterPlus")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-all r-biobase r-cluster))
    (home-page "https://bioconductor.org/packages/ConsensusClusterPlus")
    (synopsis "Clustering algorithm")
    (description
     "This package provides an implementation of an algorithm for determining
cluster count and membership by stability evidence in unsupervised analysis.")
    (license license:gpl2)))

;; This is the latest commit and it solves a bug from the latest release.
(define-public r-cycombine
  (let ((commit "f18504bc83ff5daee2b5eb4b28f09abdaaa66698") (revision "1"))
    (package
      (name "r-cycombine")
      (version (git-version "0.2.6" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/biosurf/cyCombine")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1fiwnik8iahg01732fik85xhz359x32f1xc59h443pdf7jancskm"))))
      (properties `((upstream-name . "cyCombine")))
      (build-system r-build-system)
      (propagated-inputs
       (list r-biobase
             r-cytolib
             r-dplyr
             r-flowcore
             r-ggplot2
             r-knitr
             r-kohonen
             r-magrittr
             r-purrr
             r-rcolorbrewer
             r-readr
             r-readxl
             r-stringr
             r-sva
             r-tibble
             r-tidyr))
      (native-inputs (list r-knitr))
      (home-page "https://github.com/biosurf/cyCombine")
      (synopsis "Integration of single-cell cytometry datasets")
      (description
       "This package provides a method for combining single-cell cytometry
datasets, which increases the analytical flexibility and the statistical power
of the analyses while minimizing technical noise.")
      (license license:expat))))

;; This package bundles a version of Boost.  We cannot use the latest version
;; of Boost here, as we also need to make sure that the BH (r-bh) package is
;; compatible with whatever this package bundles.
(define-public r-cytolib
  (package
    (name "r-cytolib")
    (version "2.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "cytolib" version))
       (sha256
        (base32
         "0nrj892jihi4fv58si3il42s0vyz3gm4gmb65kidkky63gmsy6iq"))))
    (properties `((upstream-name . "cytolib")))
    (build-system r-build-system)
    (inputs (list openblas protobuf zlib))
    (native-inputs
     (list r-knitr))
    (propagated-inputs
     (list r-bh r-rhdf5lib r-rprotobuflib))
    (home-page "https://bioconductor.org/packages/cytolib/")
    (synopsis "C++ infrastructure for working with gated cytometry")
    (description
     "This package provides the core data structure and API to represent and
interact with gated cytometry data.")
    (license license:artistic2.0)))

(define-public r-flowcore
  (package
    (name "r-flowcore")
    (version "2.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "flowCore" version))
       (sha256
        (base32
         "1zq3qypmzk9ld48r50fmygfdqwwyykii8l0l6gh5bdrdan760bfn"))))
    (properties `((upstream-name . "flowCore")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-bh
           r-biobase
           r-biocgenerics
           r-cpp11
           r-cytolib
           r-matrixstats
           r-rcpp
           r-rprotobuflib
           r-s4vectors))
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/flowCore")
    (synopsis "Basic structures for flow cytometry data")
    (description
     "This package provides S4 data structures and basic functions to deal
with flow cytometry data.")
    (license license:artistic2.0)))

(define-public r-flowmeans
  (package
    (name "r-flowmeans")
    (version "1.64.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "flowMeans" version))
       (sha256
        (base32
         "0bkp3q71qj1r2bwfpfriflqklvcqpvh760as7sr489sb18y1xlms"))))
    (properties `((upstream-name . "flowMeans")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biobase r-feature r-flowcore r-rrcov))
    (home-page "https://bioconductor.org/packages/flowMeans")
    (synopsis "Non-parametric flow cytometry data gating")
    (description
     "This package provides tools to identify cell populations in Flow
Cytometry data using non-parametric clustering and segmented-regression-based
change point detection.")
    (license license:artistic2.0)))

(define-public r-ncdfflow
  (package
    (name "r-ncdfflow")
    (version "2.50.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ncdfFlow" version))
       (sha256
        (base32
         "1mbq3190nxlzhfb58i7s2f5f9fngda1icnihmyplagh64savlpcn"))))
    (properties `((upstream-name . "ncdfFlow")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-bh
           r-biobase
           r-biocgenerics
           r-cpp11
           r-flowcore
           r-rhdf5lib
           r-zlibbioc))
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/ncdfFlow/")
    (synopsis "HDF5 based storage for flow cytometry data")
    (description
     "This package provides HDF5 storage based methods and functions for
manipulation of flow cytometry data.")
    (license license:artistic2.0)))

(define-public r-ggcyto
  (package
    (name "r-ggcyto")
    (version "1.32.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ggcyto" version))
       (sha256
        (base32
         "0z4cfpylj2y3jcljzg15fpjnrhrpzgw49kcprq5sg4cxr96ia2rm"))))
    (properties `((upstream-name . "ggcyto")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-data-table
           r-flowcore
           r-flowworkspace
           r-ggplot2
           r-gridextra
           r-hexbin
           r-ncdfflow
           r-plyr
           r-rcolorbrewer
           r-rlang
           r-scales))
    (native-inputs
     (list r-knitr))
    (home-page "https://github.com/RGLab/ggcyto/issues")
    (synopsis "Visualize Cytometry data with ggplot")
    (description
     "With the dedicated fortify method implemented for @code{flowSet},
@code{ncdfFlowSet} and @code{GatingSet} classes, both raw and gated flow
cytometry data can be plotted directly with ggplot.  The @code{ggcyto} wrapper
and some custom layers also make it easy to add gates and population
statistics to the plot.")
    (license license:artistic2.0)))

(define-public r-flowviz
  (package
    (name "r-flowviz")
    (version "1.68.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "flowViz" version))
       (sha256
        (base32
         "1iz4skfs68jg2a59kfaxllf19mp9pk9mpfc2jh3vyas14c3l3zzj"))))
    (properties `((upstream-name . "flowViz")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biobase
           r-flowcore
           r-hexbin
           r-idpmisc
           r-kernsmooth
           r-lattice
           r-latticeextra
           r-mass
           r-rcolorbrewer))
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/flowViz/")
    (synopsis "Visualization for flow cytometry")
    (description
     "This package provides visualization tools for flow cytometry data.")
    (license license:artistic2.0)))

(define-public r-flowclust
  (package
    (name "r-flowclust")
    (version "3.42.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "flowClust" version))
       (sha256
        (base32
         "1zjjzn10nvbj7lhg9kvp8gr7m2m294pa3zdmajq65ffdynaaavnd"))))
    (properties `((upstream-name . "flowClust")))
    (build-system r-build-system)
    (arguments
     `(#:configure-flags
       (list "--configure-args=--enable-bundled-gsl=no")))
    (propagated-inputs
     (list r-biobase
           r-biocgenerics
           r-flowcore
           r-graph))
    (inputs
     (list gsl))
    (native-inputs
     (list pkg-config r-knitr))
    (home-page "https://bioconductor.org/packages/flowClust")
    (synopsis "Clustering for flow cytometry")
    (description
     "This package provides robust model-based clustering using a t-mixture
model with Box-Cox transformation.")
    (license license:artistic2.0)))

;; TODO: this package bundles an old version of protobuf.  It's not easy to
;; make it use our protobuf package instead.
(define-public r-rprotobuflib
  (package
    (name "r-rprotobuflib")
    (version "2.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "RProtoBufLib" version))
       (sha256
        (base32
         "1vhp43l2iknljy240kzgvckkzbg186nkfdd1m3la9fnf9sqr33z9"))))
    (properties `((upstream-name . "RProtoBufLib")))
    (build-system r-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'unpack-bundled-sources
           (lambda _
             (with-directory-excursion "src"
               (invoke "tar" "xf" "protobuf-3.8.0.tar.gz")))))))
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/RProtoBufLib/")
    (synopsis "C++ headers and static libraries of Protocol buffers")
    (description
     "This package provides the headers and static library of Protocol buffers
for other R packages to compile and link against.")
    (license license:bsd-3)))

(define-public r-flowworkspace
  (package
    (name "r-flowworkspace")
    (version "4.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "flowWorkspace" version))
       (sha256
        (base32
         "0k22hifg4sv0xiw983gf1d6y7zkn9072famwj72x47w8vag4i0xz"))))
    (properties `((upstream-name . "flowWorkspace")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-bh
           r-biobase
           r-biocgenerics
           r-cpp11
           r-cytolib
           r-data-table
           r-delayedarray
           r-dplyr
           r-flowcore
           r-ggplot2
           r-graph
           r-matrixstats
           r-ncdfflow
           r-rbgl
           r-rgraphviz
           r-rhdf5lib
           r-rprotobuflib
           r-s4vectors
           r-scales
           r-xml))
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/flowWorkspace/")
    (synopsis "Infrastructure for working with cytometry data")
    (description
     "This package is designed to facilitate comparison of automated gating
methods against manual gating done in flowJo.  This package allows you to
import basic flowJo workspaces into BioConductor and replicate the gating from
flowJo using the @code{flowCore} functionality.  Gating hierarchies, groups of
samples, compensation, and transformation are performed so that the output
matches the flowJo analysis.")
    (license license:artistic2.0)))

(define-public r-flowstats
  (package
    (name "r-flowstats")
    (version "4.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "flowStats" version))
       (sha256
        (base32
         "0q8g3sqmbhinfrb63yhv4lsczjz7pqwhwwsdlz2cg7s8yhp0cj6v"))))
    (properties `((upstream-name . "flowStats")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biobase
           r-biocgenerics
           r-clue
           r-cluster
           r-corpcor
           r-fda
           r-flowcore
           r-flowviz
           r-flowworkspace
           r-kernsmooth
           r-ks
           r-lattice
           r-mass
           r-mnormt
           r-ncdfflow
           r-rcolorbrewer
           r-rrcov))
    (home-page "http://www.github.com/RGLab/flowStats")
    (synopsis "Statistical methods for the analysis of flow cytometry data")
    (description
     "This package provides methods and functionality to analyze flow data
that is beyond the basic infrastructure provided by the @code{flowCore}
package.")
    (license license:artistic2.0)))

(define-public r-opencyto
  (package
    (name "r-opencyto")
    (version "2.16.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "openCyto" version))
       (sha256
        (base32
         "13ndjvm3l5pl1jgv7bqp78didqq3rx6br4p1ncqqp68mm048f2ll"))))
    (properties `((upstream-name . "openCyto")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-bh
           r-biobase
           r-biocgenerics
           r-cpp11
           r-data-table
           r-flowclust
           r-flowcore
           r-flowviz
           r-flowworkspace
           r-graph
           r-ncdfflow
           r-rbgl
           r-rcolorbrewer))
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/openCyto")
    (synopsis "Hierarchical gating pipeline for flow cytometry data")
    (description
     "This package is designed to facilitate the automated gating methods in a
sequential way to mimic the manual gating strategy.")
    (license license:artistic2.0)))

(define-public r-cytoml
  (package
    (name "r-cytoml")
    (version "2.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "CytoML" version))
       (sha256
        (base32
         "11kflx3yahl7xjl6x2rgpd4hl5y69qgarzhalmz4qdci7b7ln7kr"))))
    (properties `((upstream-name . "CytoML")))
    (build-system r-build-system)
    (inputs
     (list libxml2))
    (propagated-inputs
     (list r-bh
           r-biobase
           r-cpp11
           r-cytolib
           r-data-table
           r-dplyr
           r-flowcore
           r-flowworkspace
           r-ggcyto
           r-graph
           r-jsonlite
           r-opencyto
           r-rbgl
           r-rgraphviz
           r-rhdf5lib
           r-rprotobuflib
           r-tibble
           r-xml
           r-yaml))
    (native-inputs
     (list r-knitr))
    (home-page "https://github.com/RGLab/CytoML")
    (synopsis "GatingML interface for cross platform cytometry data sharing")
    (description
     "This package provides an interface to implementations of the GatingML2.0
standard to exchange gated cytometry data with other software platforms.")
    (license license:artistic2.0)))

(define-public r-flowsom
  (package
    (name "r-flowsom")
    (version "2.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "FlowSOM" version))
       (sha256
        (base32
         "0xqrp184p00pvzzf6ddmp2r8lpg97bm0q6smjwcrfb8rn4msxmis"))))
    (properties `((upstream-name . "FlowSOM")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocgenerics
           r-colorramps
           r-consensusclusterplus
           r-dplyr
           r-flowcore
           r-ggforce
           r-ggnewscale
           r-ggplot2
           r-ggpubr
           r-igraph
           r-magrittr
           r-rlang
           r-rtsne
           r-tidyr
           r-xml))
    (home-page "https://bioconductor.org/packages/FlowSOM/")
    (synopsis "Visualize and interpret cytometry data")
    (description
     "FlowSOM offers visualization options for cytometry data, by using
self-organizing map clustering and minimal spanning trees.")
    (license license:gpl2+)))

(define-public r-mixomics
  (package
    (name "r-mixomics")
    (version "6.28.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "mixOmics" version))
       (sha256
        (base32
         "03pdg2hz4wnyigqakb9098ak9ww8fhwypzn006hfad0s042p9wvj"))))
    (properties `((upstream-name . "mixOmics")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocparallel
           r-corpcor
           r-dplyr
           r-ellipse
           r-ggplot2
           r-ggrepel
           r-gridextra
           r-igraph
           r-lattice
           r-mass
           r-matrixstats
           r-rarpack
           r-rcolorbrewer
           r-reshape2
           r-tidyr))
    (native-inputs
     (list r-knitr))
    (home-page "http://www.mixOmics.org")
    (synopsis "Multivariate methods for exploration of biological datasets")
    (description
     "mixOmics offers a wide range of multivariate methods for the exploration
and integration of biological datasets with a particular focus on variable
selection.  The package proposes several sparse multivariate models we have
developed to identify the key variables that are highly correlated, and/or
explain the biological outcome of interest.  The data that can be analysed
with mixOmics may come from high throughput sequencing technologies, such as
omics data (transcriptomics, metabolomics, proteomics, metagenomics etc) but
also beyond the realm of omics (e.g.  spectral imaging).  The methods
implemented in mixOmics can also handle missing values without having to
delete entire rows with missing data.")
    (license license:gpl2+)))

(define-public r-depecher
  (package                              ;Source/Weave error
    (name "r-depecher")
    (version "1.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "DepecheR" version))
       (sha256
        (base32
         "15lpn1p2p28lhmzi7svszr0p1zkyaivi69q8pxcrv4lvips38hk4"))))
    (properties `((upstream-name . "DepecheR")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-beanplot
           r-clusterr
           r-collapse
           r-dosnow
           r-dplyr
           r-fnn
           r-foreach
           r-ggplot2
           r-gmodels
           r-gplots
           r-mass
           r-matrixstats
           r-mixomics
           r-moments
           r-rcpp
           r-rcppeigen
           r-reshape2
           r-robustbase
           r-viridis))
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/DepecheR/")
    (synopsis "Identify traits of clusters in high-dimensional entities")
    (description
     "The purpose of this package is to identify traits in a dataset that can
separate groups.  This is done on two levels.  First, clustering is performed,
using an implementation of sparse K-means.  Secondly, the generated clusters
are used to predict outcomes of groups of individuals based on their
distribution of observations in the different clusters.  As certain clusters
with separating information will be identified, and these clusters are defined
by a sparse number of variables, this method can reduce the complexity of
data, to only emphasize the data that actually matters.")
    (license license:expat)))

(define-public r-rcistarget
  (package
    (name "r-rcistarget")
    (version "1.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "RcisTarget" version))
       (sha256
        (base32
         "1856jnvqacr12j5hnnl4w2abjjd7qvkz7cs8yhbnqpy1896wki0v"))))
    (properties `((upstream-name . "RcisTarget")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-arrow
           r-aucell
           r-biocgenerics
           r-data-table
           r-dplyr
           r-genomeinfodb
           r-genomicranges
           r-gseabase
           r-r-utils
           r-s4vectors
           r-summarizedexperiment
           r-tibble
           r-zoo))
    (native-inputs
     (list r-knitr))
    (home-page "https://aertslab.org/#scenic")
    (synopsis "Identify transcription factor binding motifs enriched on a gene list")
    (description
     "RcisTarget identifies @dfn{transcription factor binding motifs} (TFBS)
over-represented on a gene list.  In a first step, RcisTarget selects DNA
motifs that are significantly over-represented in the surroundings of the
@dfn{transcription start site} (TSS) of the genes in the gene-set.  This is
achieved by using a database that contains genome-wide cross-species rankings
for each motif.  The motifs that are then annotated to TFs and those that have
a high @dfn{Normalized Enrichment Score} (NES) are retained.  Finally, for
each motif and gene-set, RcisTarget predicts the candidate target genes (i.e.
genes in the gene-set that are ranked above the leading edge).")
    (license license:gpl3)))

(define-public r-chicago
  (package
    (name "r-chicago")
    (version "1.32.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Chicago" version))
       (sha256
        (base32
         "05djywxsyfxb26d0clx51q7wmsw3rq56va444d6iy5n7xlhj9m94"))))
    (properties `((upstream-name . "Chicago")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-data-table r-delaporte r-hmisc r-mass r-matrixstats))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/Chicago")
    (synopsis "Capture Hi-C analysis of genomic organization")
    (description
     "This package provides a pipeline for analysing Capture Hi-C data.")
    (license license:artistic2.0)))

;; This is a CRAN package, but it depends on Bioconductor packages, so we put
;; it here.
(define-public r-ciara
  (package
    (name "r-ciara")
    (version "0.1.0")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "CIARA" version))
              (sha256
               (base32
                "0nr7wks9231326x0lhpbh824c6vcb5hr5jn89s9bmg9mci907bsf"))))
    (properties `((upstream-name . "CIARA")))
    (build-system r-build-system)
    (propagated-inputs (list r-biobase r-ggplot2 r-ggraph r-magrittr))
    (native-inputs (list r-knitr))
    (home-page "https://cran.r-project.org/package=CIARA")
    (synopsis "Cluster-independent algorithm for rare cell types identification")
    (description
     "This is a package to support identification of markers of rare cell
types by looking at genes whose expression is confined in small regions of the
expression space.")
    (license license:artistic2.0)))

(define-public r-cicero
  (package
    (name "r-cicero")
    (version "1.22.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "cicero" version))
       (sha256
        (base32
         "0x5ly5hkqli5b67hiaa6ha02damyps7vb4l51p0az1j5fxl39n8r"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-assertthat
           r-biobase
           r-biocgenerics
           r-data-table
           r-dplyr
           r-fnn
           r-genomicranges
           r-ggplot2
           r-glasso
           r-gviz
           r-igraph
           r-iranges
           r-matrix
           r-monocle
           r-plyr
           r-reshape2
           r-s4vectors
           r-stringi
           r-stringr
           r-tibble
           r-tidyr
           r-vgam))
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/cicero/")
    (synopsis "Predict cis-co-accessibility from single-cell data")
    (description
     "Cicero computes putative cis-regulatory maps from single-cell chromatin
accessibility data.  It also extends the monocle package for use in chromatin
accessibility data.")
    (license license:expat)))

;; This is the latest commit on the "monocle3" branch.
(define-public r-cicero-monocle3
  (let ((commit "fa2fb6515857a8cfc88bc9af044f34de1bcd2b7b")
        (revision "1"))
    (package (inherit r-cicero)
      (name "r-cicero-monocle3")
      (version (git-version "1.3.2" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/cole-trapnell-lab/cicero-release")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "077yza93wdhi08n40md20jwk55k9lw1f3y0063qkk90cpz60wi0c"))))
      (propagated-inputs
       (modify-inputs (package-propagated-inputs r-cicero)
         (delete "r-monocle")
         (prepend r-monocle3))))))

(define-public r-circrnaprofiler
  (package
    (name "r-circrnaprofiler")
    (version "1.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "circRNAprofiler" version))
       (sha256
        (base32
         "0gjnx4645kd7qag7dzs5dp9lhsv6zg5xn6jna3bap4mginl1jvxi"))))
    (properties
     `((upstream-name . "circRNAprofiler")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotationhub
           r-biostrings
           r-bsgenome
           r-bsgenome-hsapiens-ucsc-hg19
           r-deseq2
           r-dplyr
           r-edger
           r-genomeinfodb
           r-genomicranges
           r-ggplot2
           r-gwascat
           r-iranges
           r-magrittr
           r-r-utils
           r-readr
           r-reshape2
           r-rlang
           r-rtracklayer
           r-s4vectors
           r-seqinr
           r-stringi
           r-stringr
           r-universalmotif))
    (native-inputs
     (list r-knitr))
    (home-page
     "https://github.com/Aufiero/circRNAprofiler")
    (synopsis
     "Computational framework for the downstream analysis of circular RNA's")
    (description
     "@code{r-circrnaprofiler} is a computational framework for a comprehensive
in silico analysis of @dfn{circular RNA} (circRNAs).  This computational
framework allows combining and analyzing circRNAs previously detected by
multiple publicly available annotation-based circRNA detection tools.  It
covers different aspects of circRNAs analysis from differential expression
analysis, evolutionary conservation, biogenesis to functional analysis.")
    (license license:gpl3)))

(define-public r-cistopic
  (package
    (name "r-cistopic")
    (version "2.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/aertslab/cisTopic")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0c4553rnxq7b1w451kcc3iwvak4qa5h2b43xmfw6ii8096zd1gbf"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-aucell
           r-data-table
           r-dplyr
           r-dosnow
           r-dt
           r-feather
           r-fitdistrplus
           r-genomicranges
           r-ggplot2
           r-lda
           r-matrix
           r-plyr
           r-rcistarget
           r-rtracklayer
           r-s4vectors))
    (home-page "https://github.com/aertslab/cisTopic")
    (synopsis "Modelling of cis-regulatory topics from single cell epigenomics data")
    (description
     "The sparse nature of single cell epigenomics data can be overruled using
probabilistic modelling methods such as @dfn{Latent Dirichlet
Allocation} (LDA).  This package allows the probabilistic modelling of
cis-regulatory topics (cisTopics) from single cell epigenomics data, and
includes functionalities to identify cell states based on the contribution of
cisTopics and explore the nature and regulatory proteins driving them.")
    (license license:gpl3)))

(define-public r-cistopic-next
  (let ((commit "04cecbb9d1112fcc1a6edc28b5a506bcb49f2803")
        (revision "1"))
    (package
      (inherit r-cistopic)
      (name "r-cistopic-next")
      ;; The DESCRIPTION file says this is version 0.3.0, which is a bit odd
      ;; since the previous release is 2.1.0.  Oh well.
      (version (git-version "0.3.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/aertslab/cisTopic")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "11cg9szlysnsjiaahda4k5v2vh4rxx27zhz53hafgaq9mdz0kgi2"))))
      (properties `((upstream-name . "cisTopic")))
      (propagated-inputs
       (list r-aucell
             r-data-table
             r-dosnow
             r-dplyr
             r-dt
             r-feather
             r-fitdistrplus
             r-genomicranges
             r-ggplot2
             r-lda
             r-matrix
             r-plyr
             r-rcistarget
             r-rtracklayer
             r-s4vectors
             r-text2vec))
      (native-inputs
       (list r-knitr)))))

(define-public r-genie3
  (package
    (name "r-genie3")
    (version "1.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "GENIE3" version))
       (sha256
        (base32
         "0ijwyx42saqas52273c63c3yi948bh4pd4lifyqi3zjjs17bj1cg"))))
    (properties `((upstream-name . "GENIE3")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-dplyr r-reshape2))
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/GENIE3")
    (synopsis "Gene network inference with ensemble of trees")
    (description
     "This package implements the GENIE3 algorithm for inferring gene
regulatory networks from expression data.")
    (license license:gpl2+)))

(define-public r-roc
  (package
    (name "r-roc")
    (version "1.80.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ROC" version))
       (sha256
        (base32
         "074c6jg0dyr3hckizpg56f8ij8szj43cx6x5bbw7a01xdjdc70yb"))))
    (properties `((upstream-name . "ROC")))
    (build-system r-build-system)
    (native-inputs
     (list r-knitr))
    (home-page "https://www.bioconductor.org/packages/ROC/")
    (synopsis "Utilities for ROC curves")
    (description
     "This package provides utilities for @dfn{Receiver Operating
Characteristic} (ROC) curves, with a focus on micro arrays.")
    (license license:artistic2.0)))

(define-public r-watermelon
  (package
    (name "r-watermelon")
    (version "2.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "wateRmelon" version))
       (sha256
        (base32
         "19zlv9aglllsjq3jcnlw994bkanjdq0ib46k7wjsig04cx4apz1v"))))
    (properties `((upstream-name . "wateRmelon")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biobase
           r-illuminahumanmethylation450kanno-ilmn12-hg19
           r-illuminaio
           r-limma
           r-lumi
           r-matrixstats
           r-methylumi
           r-roc))
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/wateRmelon/")
    (synopsis "Illumina 450 methylation array normalization and metrics")
    (description
     "The standard index of DNA methylation (beta) is computed from methylated
and unmethylated signal intensities.  Betas calculated from raw signal
intensities perform well, but using 11 methylomic datasets we demonstrate that
quantile normalization methods produce marked improvement.  The commonly used
procedure of normalizing betas is inferior to the separate normalization of M
and U, and it is also advantageous to normalize Type I and Type II assays
separately.  This package provides 15 flavours of betas and three performance
metrics, with methods for objects produced by the @code{methylumi} and
@code{minfi} packages.")
    (license license:gpl3)))

(define-public r-gdsfmt
  (package
    (name "r-gdsfmt")
    (version "1.40.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "gdsfmt" version))
       (sha256
        (base32
         "1ska6q1800mmsxi6jilpnq4f6cy3vk0nb98gz3ik94lmbv2ram97"))
       (modules '((guix build utils)))
       ;; Remove bundled sources of zlib, lz4, and xz.  Don't attempt to build
       ;; them and link with system libraries instead.
       (snippet
        '(begin
           (for-each delete-file-recursively
                     '("src/LZ4"
                       "src/XZ"
                       "src/ZLIB"))
           (substitute* "src/Makevars"
             (("all: \\$\\(SHLIB\\)") "all:")
             (("\\$\\(SHLIB\\): liblzma.a") "")
             (("^	(ZLIB|LZ4)/.*") "")
             (("CoreArray/dVLIntGDS.cpp.*")
              "CoreArray/dVLIntGDS.cpp")
             (("CoreArray/dVLIntGDS.o.*")
              "CoreArray/dVLIntGDS.o")
             (("PKG_LIBS = ./liblzma.a")
              "PKG_LIBS = -llz4"))
           (substitute* "src/CoreArray/dStream.h"
             (("include \"../(ZLIB|LZ4|XZ/api)/(.*)\"" _ _ header)
              (string-append "include <" header ">")))))))
    (properties
     `((upstream-name . "gdsfmt")
       (updater-extra-inputs . ("lz4" "xz" "zlib"))))
    (build-system r-build-system)
    (inputs
     (list lz4 xz zlib))
    (native-inputs
     (list r-knitr))
    (home-page "http://corearray.sourceforge.net/")
    (synopsis
     "R Interface to CoreArray Genomic Data Structure (GDS) Files")
    (description
     "This package provides a high-level R interface to CoreArray @dfn{Genomic
Data Structure} (GDS) data files, which are portable across platforms with
hierarchical structure to store multiple scalable array-oriented data sets
with metadata information.  It is suited for large-scale datasets, especially
for data which are much larger than the available random-access memory.  The
@code{gdsfmt} package offers efficient operations specifically designed for
integers of less than 8 bits, since a diploid genotype, like
@dfn{single-nucleotide polymorphism} (SNP), usually occupies fewer bits than a
byte.  Data compression and decompression are available with relatively
efficient random access.  It is also allowed to read a GDS file in parallel
with multiple R processes supported by the package @code{parallel}.")
    (license license:lgpl3)))

(define-public r-bigmelon
  (package
    (name "r-bigmelon")
    (version "1.30.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "bigmelon" version))
       (sha256
        (base32
         "0fa1dwbk6hdiwg6dmbks7hy4cg3ydyd2sv94cyl0003awhf00nn9"))))
    (properties `((upstream-name . "bigmelon")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biobase
           r-biocgenerics
           r-gdsfmt
           r-geoquery
           r-illuminaio
           r-methylumi
           r-minfi
           r-watermelon))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/bigmelon/")
    (synopsis "Illumina methylation array analysis for large experiments")
    (description
     "This package provides methods for working with Illumina arrays using the
@code{gdsfmt} package.")
    (license license:gpl3)))

(define-public r-seqbias
  (package
    (name "r-seqbias")
    (version "1.50.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "seqbias" version))
       (sha256
        (base32
         "18pdpi855y1hhka96xc5886nqffjn1jhz9jr9p305iikdrsvmjp1"))))
    (properties `((upstream-name . "seqbias")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biostrings r-genomicranges r-rhtslib r-zlibbioc))
    (home-page "https://bioconductor.org/packages/seqbias/")
    (synopsis "Estimation of per-position bias in high-throughput sequencing data")
    (description
     "This package implements a model of per-position sequencing bias in
high-throughput sequencing data using a simple Bayesian network, the structure
and parameters of which are trained on a set of aligned reads and a reference
genome sequence.")
    (license license:lgpl3)))

(define-public r-reqon
  (package
    (name "r-reqon")
    (version "1.48.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ReQON" version))
       (sha256
        (base32
         "0w28b3ma8r8rshijdm5jj1z64v91my7hcvw18r9pdwjprh05bw0g"))))
    (properties `((upstream-name . "ReQON")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-rjava r-rsamtools r-seqbias))
    (home-page "https://bioconductor.org/packages/ReQON/")
    (synopsis "Recalibrating quality of nucleotides")
    (description
     "This package provides an implementation of an algorithm for
recalibrating the base quality scores for aligned sequencing data in BAM
format.")
    (license license:gpl2)))

(define-public r-wavcluster
  (package
    (name "r-wavcluster")
    (version "2.38.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "wavClusteR" version))
       (sha256
        (base32
         "0yr4jfaxs253xg5q0kds8fm97kqacx67pw1c80yx2w79v7nqskgv"))))
    (properties `((upstream-name . "wavClusteR")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocgenerics
           r-biostrings
           r-foreach
           r-genomicfeatures
           r-genomicranges
           r-ggplot2
           r-hmisc
           r-iranges
           r-mclust
           r-rsamtools
           r-rtracklayer
           r-s4vectors
           r-seqinr
           r-stringr))
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/wavClusteR/")
    (synopsis "Identification of RNA-protein interaction sites in PAR-CLIP data")
    (description
     "This package provides an integrated pipeline for the analysis of
PAR-CLIP data.  PAR-CLIP-induced transitions are first discriminated from
sequencing errors, SNPs and additional non-experimental sources by a non-
parametric mixture model.  The protein binding sites (clusters) are then
resolved at high resolution and cluster statistics are estimated using a
rigorous Bayesian framework.  Post-processing of the results, data export for
UCSC genome browser visualization and motif search analysis are provided.  In
addition, the package integrates RNA-Seq data to estimate the False
Discovery Rate of cluster detection.  Key functions support parallel multicore
computing.  While wavClusteR was designed for PAR-CLIP data analysis, it can
be applied to the analysis of other NGS data obtained from experimental
procedures that induce nucleotide substitutions (e.g. BisSeq).")
    (license license:gpl2)))

(define-public r-tilingarray
  (package
    (name "r-tilingarray")
    (version "1.82.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "tilingArray" version))
              (sha256
               (base32
                "1408275gvfilc2k8sfsja8bm5fdgnblf2sf5wdd3if51qi61clb0"))))
    (properties `((upstream-name . "tilingArray")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-affy
           r-biobase
           r-genefilter
           r-pixmap
           r-rcolorbrewer
           r-strucchange
           r-vsn))
    (home-page "https://bioconductor.org/packages/tilingArray")
    (synopsis "Transcript mapping with high-density oligonucleotide tiling arrays")
    (description
     "The package provides functionality that can be useful for the analysis
of the high-density tiling microarray data (such as from Affymetrix genechips)
or for measuring the transcript abundance and the architecture.  The main
functionalities of the package are:

@enumerate
@item the class segmentation for representing partitionings of a linear series
  of data;
@item the function segment for fitting piecewise constant models using a
  dynamic programming algorithm that is both fast and exact;
@item the function @code{confint} for calculating confidence intervals using
  the @code{strucchange} package;
@item the function @code{plotAlongChrom} for generating pretty plots;
@item the function @code{normalizeByReference} for probe-sequence dependent
  response adjustment from a (set of) reference hybridizations.
@end enumerate")
    (license license:artistic2.0)))

(define-public r-timeseriesexperiment
  (package
    (name "r-timeseriesexperiment")
    (version "1.13.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "TimeSeriesExperiment" version))
       (sha256
        (base32
         "0bdpxxr739qdg92qabfx122k9f43vw2hyxp4yxqlbp37vzgcdf2c"))))
    (properties
     `((upstream-name . "TimeSeriesExperiment")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-deseq2
           r-dplyr
           r-dynamictreecut
           r-edger
           r-ggplot2
           r-hmisc
           r-limma
           r-magrittr
           r-proxy
           r-s4vectors
           r-summarizedexperiment
           r-tibble
           r-tidyr
           r-vegan
           r-viridis))
    (native-inputs
     (list r-knitr))
    (home-page "https://github.com/nlhuong/TimeSeriesExperiment/")
    (synopsis "Analysis for short time-series data")
    (description
     "This package is a visualization and analysis toolbox for short time
course data which includes dimensionality reduction, clustering, two-sample
differential expression testing and gene ranking techniques.  The package also
provides methods for retrieving enriched pathways.")
    (license license:lgpl3+)))

(define-public r-variantfiltering
  (package
    (name "r-variantfiltering")
    (version "1.40.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "VariantFiltering" version))
       (sha256
        (base32
         "14kxlzgy71s8h34galx93q8kalspr1m23brssbr3wha0hxb0ghgd"))))
    (properties
     `((upstream-name . "VariantFiltering")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotationdbi
           r-biobase
           r-biocgenerics
           r-biocparallel
           r-biostrings
           r-bsgenome
           r-dt
           r-genomeinfodb
           r-genomicfeatures
           r-genomicranges
           r-genomicscores
           r-graph
           r-gviz
           r-iranges
           r-rbgl
           r-rsamtools
           r-s4vectors
           r-shiny
           r-shinyjs
           r-shinythemes
           r-shinytree
           r-summarizedexperiment
           r-variantannotation
           r-xvector))
    (home-page "https://github.com/rcastelo/VariantFiltering")
    (synopsis "Filtering of coding and non-coding genetic variants")
    (description
     "Filter genetic variants using different criteria such as inheritance
model, amino acid change consequence, minor allele frequencies across human
populations, splice site strength, conservation, etc.")
    (license license:artistic2.0)))

(define-public r-variancepartition
  (package
    (name "r-variancepartition")
    (version "1.34.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "variancePartition" version))
       (sha256
        (base32
         "00hrizpx05jfh7yrab6na10zkahm8ibrbxy4pa4dcldw65klqmn9"))))
    (properties
     `((upstream-name . "variancePartition")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-aod
           r-biobase
           r-biocparallel
           r-corpcor
           r-fancova
           r-ggplot2
           r-gplots
           r-iterators
           r-limma
           r-lme4
           r-lmertest
           r-mass
           r-matrix
           r-matrixstats
           r-pbkrtest
           r-rdpack
           r-remacor
           r-reshape2
           r-rhpcblasctl
           r-rlang
           r-scales))
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/variancePartition/")
    (synopsis "Analyze variation in gene expression experiments")
    (description
     "This is a package providing tools to quantify and interpret multiple
sources of biological and technical variation in gene expression experiments.
It uses a linear mixed model to quantify variation in gene expression
attributable to individual, tissue, time point, or technical variables.  The
package includes dream differential expression analysis for repeated
measures.")
    (license license:gpl2+)))

(define-public r-htqpcr
  (package
    (name "r-htqpcr")
    (version "1.56.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "HTqPCR" version))
       (sha256
        (base32
         "14cff36ikbqhd5xizihpxzsv9jimcpbgnd381jd154pgi60bil0m"))))
    (properties `((upstream-name . "HTqPCR")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-affy r-biobase r-gplots r-limma r-rcolorbrewer))
    (home-page (string-append "https://www.ebi.ac.uk/sites/ebi.ac.uk/files/"
                              "groups/bertone/software/HTqPCR.pdf"))
    (synopsis "Automated analysis of high-throughput qPCR data")
    (description
     "Analysis of Ct values from high throughput quantitative real-time
PCR (qPCR) assays across multiple conditions or replicates.  The input data
can be from spatially-defined formats such ABI TaqMan Low Density Arrays or
OpenArray; LightCycler from Roche Applied Science; the CFX plates from Bio-Rad
Laboratories; conventional 96- or 384-well plates; or microfluidic devices
such as the Dynamic Arrays from Fluidigm Corporation.  HTqPCR handles data
loading, quality assessment, normalization, visualization and parametric or
non-parametric testing for statistical significance in Ct values between
features (e.g.  genes, microRNAs).")
    (license license:artistic2.0)))

(define-public r-ucell
  (package
    (name "r-ucell")
    (version "2.8.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "UCell" version))
              (sha256
               (base32
                "09r64rrydwgvvvq8f5vzvn7dsh24l9sa1if21wcimrl28ci1g1mv"))))
    (properties `((upstream-name . "UCell")))
    (build-system r-build-system)
    (propagated-inputs (list r-biocneighbors
                             r-biocparallel
                             r-data-table
                             r-matrix
                             r-singlecellexperiment
                             r-summarizedexperiment))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/carmonalab/UCell")
    (synopsis "Rank-based signature enrichment analysis for single-cell data")
    (description
     "UCell is a package for evaluating gene signatures in single-cell datasets.
UCell signature scores, based on the Mann-Whitney U statistic, are robust to
dataset size and heterogeneity, and their calculation demands less computing
time and memory than other available methods, enabling the processing of large
datasets in a few minutes even on machines with limited computing power.
UCell can be applied to any single-cell data matrix, and includes functions to
directly interact with SingleCellExperiment and Seurat objects.")
    (license license:gpl3)))

(define-public r-ucsc-utils
  (package
    (name "r-ucsc-utils")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "UCSC.utils" version))
       (sha256
        (base32 "0w9f5xd6ryniapbpcrpczn8wimph2154w45cvdgqfa3dv5qy379l"))))
    (properties `((upstream-name . "UCSC.utils")))
    (build-system r-build-system)
    (propagated-inputs (list r-httr r-jsonlite r-s4vectors))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/UCSC.utils")
    (synopsis
     "Low-level utilities to retrieve data from the UCSC Genome Browser")
    (description
     "This package provides a set of low-level utilities to retrieve data from the
UCSC Genome Browser.  Most functions in the package access the data via the UCSC
REST API but some of them query the UCSC @code{MySQL} server directly.  Note
that the primary purpose of the package is to support higher-level
functionalities implemented in downstream packages like @code{GenomeInfoDb} or
txdbmaker.")
    (license license:artistic2.0)))

(define-public r-unifiedwmwqpcr
  (package
    (name "r-unifiedwmwqpcr")
    (version "1.38.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "unifiedWMWqPCR" version))
       (sha256
        (base32
         "0v3jpnm3nq66xvckmc8447xl7wngfhhiwq6vjx54wgc6f586vmgd"))))
    (properties
     `((upstream-name . "unifiedWMWqPCR")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocgenerics r-htqpcr))
    (home-page "https://bioconductor.org/packages/unifiedWMWqPCR")
    (synopsis "Unified Wilcoxon-Mann Whitney Test for differential expression in qPCR data")
    (description
     "This package implements the unified Wilcoxon-Mann-Whitney Test for qPCR
data.  This modified test allows for testing differential expression in qPCR
data.")
    (license license:gpl2+)))

(define-public r-universalmotif
  (package
    (name "r-universalmotif")
    (version "1.22.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "universalmotif" version))
       (sha256
        (base32
         "1kasmib3lp68pq5f9qzh38z7gpv8hik536iz16g8a73pc6bcbmzy"))))
    (properties
     `((upstream-name . "universalmotif")))
    (build-system r-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-reference-to-strip
           (lambda _
             (substitute* "src/Makevars"
               (("/usr/bin/strip") (which "strip"))))))))
    (propagated-inputs
     (list r-biocgenerics
           r-biostrings
           r-ggplot2
           r-iranges
           r-mass
           r-matrixgenerics
           r-rcpp
           r-rcppthread
           r-rlang
           r-s4vectors
           r-yaml))
    (native-inputs
     (list r-knitr))
    (home-page
     "https://bioconductor.org/packages/universalmotif/")
    (synopsis
     "Specific structures importer, modifier, and exporter for R")
    (description
     "This package allows importing most common @dfn{specific structure}
(motif) types into R for use by functions provided by other Bioconductor
motif-related packages.  Motifs can be exported into most major motif formats
from various classes as defined by other Bioconductor packages.  A suite of
motif and sequence manipulation and analysis functions are included, including
enrichment, comparison, P-value calculation, shuffling, trimming, higher-order
motifs, and others.")
    (license license:gpl3)))

(define-public r-ace
  (package
    (name "r-ace")
    (version "1.22.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "ACE" version))
              (sha256
               (base32
                "1gpn9z09cfcy2mj7m32xfb2bjjwl01g9qn9nqaivmqbi4k6icrr8"))))
    (properties `((upstream-name . "ACE")))
    (build-system r-build-system)
    (propagated-inputs (list r-biobase r-genomicranges r-ggplot2 r-qdnaseq))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/tgac-vumc/ACE")
    (synopsis
     "Absolute copy number estimation from low-coverage whole genome sequencing")
    (description
     "This package uses segmented copy number data to estimate tumor cell
percentage and produce copy number plots displaying absolute copy numbers.  For
this it uses segmented data from the @code{QDNAseq} package, which in turn uses
a number of dependencies to turn mapped reads into segmented data.  @code{ACE}
will run @code{QDNAseq} or use its output rds-file of segmented data.  It will
subsequently run through all samples in the object(s), for which it will create
individual subdirectories.  For each sample, it will calculate how well the
segments fit (the relative error) to integer copy numbers for each percentage
of @dfn{tumor cells} (cells with divergent segments).")
    (license license:gpl2)))

(define-public r-acgh
  (package
    (name "r-acgh")
    (version "1.82.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "aCGH" version))
              (sha256
               (base32
                "00dcx6kcqq5xcdy7rqgi51cvc20dsgrrki8aicbj1icx0m6601wa"))))
    (properties `((upstream-name . "aCGH")))
    (build-system r-build-system)
    (propagated-inputs (list r-biobase r-cluster r-multtest r-survival))
    (home-page "https://bioconductor.org/packages/aCGH")
    (synopsis
     "Classes and functions for array comparative genomic hybridization data")
    (description
     "This package provides functions for reading
@dfn{array comparative genomic hybridization} (aCGH) data from image analysis
output files and clone information files, creation of @code{aCGH} objects for
storing these data.  Basic methods are accessing/replacing, subsetting,
printing and plotting @code{aCGH} objects.")
    (license license:gpl2)))

(define-public r-acme
  (package
    (name "r-acme")
    (version "2.60.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "ACME" version))
              (sha256
               (base32
                "0jak83dlavhczaa2nqk5g2l0h4grnc9v24bg6v9mjfn6bqhl1s7j"))))
    (properties `((upstream-name . "ACME")))
    (build-system r-build-system)
    (propagated-inputs (list r-biobase r-biocgenerics))
    (home-page "https://bioconductor.org/packages/aCGH/")
    (synopsis "Calculating microarray enrichment")
    (description
     "This package implements @dfn{algorithms for calculating microarray
enrichment} (ACME), and it is a set of tools for analysing tiling array of
@dfn{combined chromatin immunoprecipitation with DNA microarray} (ChIP/chip),
DNAse hypersensitivity, or other experiments that result in regions of the
genome showing enrichment.  It does not rely on a specific array technology
(although the array should be a tiling array), is very general (can be applied
in experiments resulting in regions of enrichment), and is very insensitive to
array noise or normalization methods.  It is also very fast and can be applied
on whole-genome tiling array experiments quite easily with enough memory.")
    (license license:gpl2+)))

(define-public r-acde
  (package
    (name "r-acde")
    (version "1.34.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "acde" version))
              (sha256
               (base32
                "1h3drjxdmd837hc7l1xk6b4ha39i7bqkb8862h4h6s5rfa7dcc50"))))
    (properties `((upstream-name . "acde")))
    (build-system r-build-system)
    (propagated-inputs (list r-boot))
    (home-page "https://bioconductor.org/packages/acde")
    (synopsis
     "Identification of differentially expressed genes with artificial components")
    (description
     "This package provides a multivariate inferential analysis method for
detecting differentially expressed genes in gene expression data.  It uses
artificial components, close to the data's principal components but with an
exact interpretation in terms of differential genetic expression, to identify
differentially expressed genes while controlling the @dfn{false discovery
rate} (FDR).")
    (license license:gpl3)))

;; This is a CRAN package, but it depends on Bioconductor packages, so we put
;; it here.
(define-public r-activedriverwgs
  (package
    (name "r-activedriverwgs")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ActiveDriverWGS" version))
       (sha256
        (base32
         "0xnplgwxd197a4d422bsxg753q158i12ils16awd1cw30wafdxkk"))))
    (properties
     `((upstream-name . "ActiveDriverWGS")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biostrings
           r-bsgenome
           r-bsgenome-hsapiens-ucsc-hg19
           r-bsgenome-hsapiens-ucsc-hg38
           r-bsgenome-mmusculus-ucsc-mm9
           r-bsgenome-mmusculus-ucsc-mm10
           r-genomeinfodb
           r-genomicranges
           r-iranges
           r-s4vectors))
    (native-inputs
     (list r-knitr))
    (home-page "https://cran.r-project.org/web/packages/ActiveDriverWGS/")
    (synopsis "Driver discovery tool for cancer whole genomes")
    (description
     "This package provides a method for finding an enrichment of cancer
simple somatic mutations (SNVs and Indels) in functional elements across the
human genome.  ActiveDriverWGS detects coding and noncoding driver elements
using whole genome sequencing data.")
    (license license:gpl3)))

;; This is a CRAN package, but it depends on Bioconductor packages, so we put
;; it here.
(define-public r-activepathways
  (package
    (name "r-activepathways")
    (version "2.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ActivePathways" version))
       (sha256
        (base32
         "07nmlkgznbv46kxy5s13bs7p3z9cxsynh6csbid6qsvqrpzv78pk"))))
    (properties
     `((upstream-name . "ActivePathways")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-data-table r-ggplot2))
    (native-inputs
     (list r-knitr))
    (home-page "https://cran.r-project.org/web/packages/ActivePathways/")
    (synopsis "Multivariate pathway enrichment analysis")
    (description
     "This package represents an integrative method of analyzing multi omics
data that conducts enrichment analysis of annotated gene sets.  ActivePathways
uses a statistical data fusion approach, rationalizes contributing evidence
and highlights associated genes, improving systems-level understanding of
cellular organization in health and disease.")
    (license license:gpl3)))

(define-public r-bgmix
  (package
    (name "r-bgmix")
    (version "1.59.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BGmix" version))
       (sha256
        (base32
         "16fzgxcy4sk0kd67vzdxqz81s84dvh4bqss9cbl9bn6vhpfsnfyf"))))
    (properties `((upstream-name . "BGmix")))
    (build-system r-build-system)
    (arguments
     (list
      #:phases
      '(modify-phases %standard-phases
         ;; GCC's c++/bits/specfun.h (included by cmath) provides a std::beta
         ;; procedure.
         (add-after 'unpack 'avoid-naming-conflict
           (lambda _
             (substitute* "src/BGmix_main.cpp"
               (("\\bbeta\\b") "::beta")
               (("\\*\\*::beta,") "**beta,")))))))
    (propagated-inputs
     (list r-kernsmooth))
    (home-page "https://bioconductor.org/packages/BGmix/")
    (synopsis "Bayesian models for differential gene expression")
    (description
     "This package provides fully Bayesian mixture models for differential
gene expression.")
    (license license:gpl2)))

(define-public r-bgx
  (package
    (name "r-bgx")
    (version "1.70.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "bgx" version))
       (sha256
        (base32
         "0sglk8q12fvqk6m699v3lajwr1zb8wnrldfv2n8y7jjy2fqcvs19"))))
    (properties `((upstream-name . "bgx")))
    (build-system r-build-system)
    (arguments
     (list
      #:phases
      '(modify-phases %standard-phases
         (add-after 'unpack 'do-not-tune-cflags-for-reproducibility
           (lambda _
             (substitute* "configure.ac"
               (("AX_GCC_ARCHFLAG.*") ""))
             (delete-file "configure")
             (invoke "autoreconf" "-vif"))))))
    (inputs
     (list boost))
    (propagated-inputs
     (list r-affy r-biobase r-gcrma r-rcpp))
    (native-inputs
     (list autoconf automake))
    (home-page "https://bioconductor.org/packages/bgx/")
    (synopsis "Bayesian gene expression")
    (description
     "This package provides tools for Bayesian integrated analysis of
Affymetrix GeneChips.")
    (license license:gpl2)))

(define-public r-bhc
  (package
    (name "r-bhc")
    (version "1.56.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BHC" version))
       (sha256
        (base32
         "1fam1k42dwd0gh7dw2gz4yl6bnbkfrw0bd44v8sdpvwwyd1dq1r8"))))
    (properties `((upstream-name . "BHC")))
    (build-system r-build-system)
    (home-page "https://bioconductor.org/packages/BHC/")
    (synopsis "Bayesian hierarchical clustering")
    (description
     "The method implemented in this package performs bottom-up hierarchical
clustering, using a Dirichlet Process (infinite mixture) to model uncertainty
in the data and Bayesian model selection to decide at each step which clusters
to merge.  This avoids several limitations of traditional methods, for example
how many clusters there should be and how to choose a principled distance
metric.  This implementation accepts multinomial (i.e. discrete, with 2+
categories) or time-series data.  This version also includes a randomised
algorithm which is more efficient for larger data sets.")
    (license license:gpl3)))

(define-public r-bicare
  (package
    (name "r-bicare")
    (version "1.62.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BicARE" version))
       (sha256
        (base32
         "0397xg4sj988b0chzfpy23dszpjbdkch59wfynbn4pwyvl0qmp9j"))))
    (properties `((upstream-name . "BicARE")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biobase r-go-db r-gseabase r-multtest))
    (home-page "http://bioinfo.curie.fr")
    (synopsis "Biclustering analysis and results exploration")
    (description
     "This is a package for biclustering analysis and exploration of
results.")
    (license license:gpl2)))

(define-public r-bifet
  (package
    (name "r-bifet")
    (version "1.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BiFET" version))
       (sha256
        (base32
         "148j41mawxvzyal9lfjy7h1gp0w8zwmgc7s32wiqa30qjcc82snx"))))
    (properties `((upstream-name . "BiFET")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-genomicranges r-poibin))
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/BiFET")
    (synopsis "Bias-free footprint enrichment test")
    (description
     "BiFET identifies @dfn{transcription factors} (TFs) whose footprints are
over-represented in target regions compared to background regions after
correcting for the bias arising from the imbalance in read counts and GC
contents between the target and background regions.  For a given TF k, BiFET
tests the null hypothesis that the target regions have the same probability of
having footprints for the TF k as the background regions while correcting for
the read count and GC content bias.")
    (license license:gpl3)))

(define-public r-rsbml
  (package
    (name "r-rsbml")
    (version "2.62.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "rsbml" version))
       (sha256
        (base32
         "02pmxg0ppy1asjscp13kmqyj2phw4q138fj2b3xgmcagwxqzb4bs"))))
    (properties `((upstream-name . "rsbml")))
    (build-system r-build-system)
    (inputs
     (list libsbml))
    (propagated-inputs
     (list r-biocgenerics r-graph))
    (native-inputs
     (list pkg-config))
    (home-page "http://www.sbml.org")
    (synopsis "R support for SBML")
    (description
     "This package provides an R interface to libsbml for SBML parsing,
validating output, provides an S4 SBML DOM, converts SBML to R graph objects.")
    (license license:artistic2.0)))

(define-public r-hybridmtest
  (package
    (name "r-hybridmtest")
    (version "1.48.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "HybridMTest" version))
       (sha256
        (base32 "0fh0v29fpwzramqnbxmzpmgiziwswdx38734g2lx303b0blkbv7j"))))
    (properties `((upstream-name . "HybridMTest")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biobase
           r-fdrtool
           r-mass
           r-survival))
    (home-page "https://bioconductor.org/packages/HybridMTest")
    (synopsis "Hybrid multiple testing")
    (description
     "This package performs hybrid multiple testing that incorporates method
selection and assumption evaluations into the analysis using @acronym{EBP,
empirical Bayes probability} estimates obtained by Grenander density
estimation.  For instance, for 3-group comparison analysis, Hybrid Multiple
testing considers EBPs as weighted EBPs between F-test and H-test with EBPs
from Shapiro Wilk test of normality as weigth.  Instead of just using EBPs
from F-test only or using H-test only, this methodology combines both types of
EBPs through EBPs from Shapiro Wilk test of normality.  This methodology uses
then the law of total EBPs.")
    (license license:gpl2+)))

(define-public r-hypergraph
  (package
    (name "r-hypergraph")
    (version "1.76.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "hypergraph" version))
       (sha256
        (base32
         "12g33rw1qy1ij32k87qrw04pak20p8z04mch9lxjy5yrj5k5x812"))))
    (properties `((upstream-name . "hypergraph")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-graph))
    (home-page "https://bioconductor.org/packages/hypergraph")
    (synopsis "Hypergraph data structures")
    (description
     "This package implements some simple capabilities for representing and
manipulating hypergraphs.")
    (license license:artistic2.0)))

(define-public r-hyperdraw
  (package
    (name "r-hyperdraw")
    (version "1.56.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "hyperdraw" version))
       (sha256
        (base32
         "104xwij9vq5gwq4ml7fnhsp4apcldbgxmhppwklg6zqdfixpmig5"))))
    (properties `((upstream-name . "hyperdraw")))
    (build-system r-build-system)
    (inputs (list graphviz))
    (propagated-inputs
     (list r-graph r-hypergraph r-rgraphviz))
    (home-page "https://bioconductor.org/packages/hyperdraw")
    (synopsis "Visualizing hypergraphs")
    (description
     "This package provides functions for visualizing hypergraphs.")
    (license license:gpl2+)))

(define-public r-biggr
  (package
    (name "r-biggr")
    (version "1.40.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BiGGR" version))
       (sha256
        (base32
         "1ja5j9753rgx61jwkax0mzkxh7ybh3m4ampkr5ikw3afda23wg1z"))))
    (properties `((upstream-name . "BiGGR")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-hyperdraw
           r-hypergraph
           r-lim
           r-limsolve
           r-rsbml
           r-stringr))
    (home-page "https://bioconductor.org/packages/BiGGR/")
    (synopsis "Constraint based modeling using metabolic reconstruction databases")
    (description
     "This package provides an interface to simulate metabolic reconstruction
from the @url{http://bigg.ucsd.edu/, BiGG database} and other metabolic
reconstruction databases.  The package facilitates @dfn{flux balance
analysis} (FBA) and the sampling of feasible flux distributions.  Metabolic
networks and estimated fluxes can be visualized with hypergraphs.")
    (license license:gpl3+)))

(define-public r-bigmemoryextras
  (package
    (name "r-bigmemoryextras")
    (version "1.38.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "bigmemoryExtras" version))
       (sha256
        (base32
         "1k31h746j8r3f92vj62s38fw12qjkv5814ipcqfbswnxgaan17zj"))))
    (properties
     `((upstream-name . "bigmemoryExtras")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-bigmemory))
    (native-inputs
     (list r-knitr))
    (home-page "https://github.com/phaverty/bigmemoryExtras")
    (synopsis "Extension of the bigmemory package")
    (description
     "This package defines a @code{BigMatrix} @code{ReferenceClass} which adds
safety and convenience features to the @code{filebacked.big.matrix} class from
the @code{bigmemory} package.  @code{BigMatrix} protects against segfaults by
monitoring and gracefully restoring the connection to on-disk data and it also
protects against accidental data modification with a file-system-based
permissions system.  Utilities are provided for using @code{BigMatrix}-derived
classes as @code{assayData} matrices within the @code{Biobase} package's
@code{eSet} family of classes.  @code{BigMatrix} provides some optimizations
related to attaching to, and indexing into, file-backed matrices with
dimnames.  Additionally, the package provides a @code{BigMatrixFactor} class,
a file-backed matrix with factor properties.")
    (license license:artistic2.0)))

(define-public r-bigpint
  (package
    (name "r-bigpint")
    (version "1.15.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "bigPint" version))
       (sha256
        (base32
         "1r9gr5f9as09ifagal5k7713h95qjw508cf8ny3j1jmqd24v6xhs"))))
    (properties `((upstream-name . "bigPint")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-delayedarray
           r-dplyr
           r-ggally
           r-ggplot2
           r-gridextra
           r-hexbin
           r-hmisc
           r-htmlwidgets
           r-plotly
           r-plyr
           r-rcolorbrewer
           r-reshape
           r-shiny
           r-shinycssloaders
           r-shinydashboard
           r-stringr
           r-summarizedexperiment
           r-tidyr))
    (native-inputs
     (list r-knitr))
    (home-page "https://github.com/lindsayrutter/bigPint")
    (synopsis "Big multivariate data plotted interactively")
    (description
     "This package provides methods for visualizing large multivariate
datasets using static and interactive scatterplot matrices, parallel
coordinate plots, volcano plots, and litre plots.  It includes examples for
visualizing RNA-sequencing datasets and differentially expressed genes.")
    (license license:gpl3)))

(define-public r-chemmineob
  (package
    (name "r-chemmineob")
    (version "1.42.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ChemmineOB" version))
       (sha256
        (base32 "17yvmdg16pb1dmx160n6vcgw3sa1yf40f8yzivdyic3sckn4z4bw"))))
    (properties `((upstream-name . "ChemmineOB")))
    (build-system r-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-makefile
            (lambda _
              (substitute* "src/Makevars.in"
                (("/usr/include/openbabel3")
                 (string-append #$(this-package-input "openbabel")
                                "/include/openbabel3"))
                (("/usr/include/eigen3")
                 (string-append #$(this-package-input "eigen")
                                "/include/eigen3"))))))))
    (inputs (list eigen openbabel))
    (propagated-inputs (list r-bh r-biocgenerics r-rcpp r-zlibbioc))
    (native-inputs (list pkg-config r-knitr))
    (home-page "https://github.com/girke-lab/ChemmineOB")
    (synopsis "R interface to a subset of OpenBabel functionalities")
    (description
     "@code{ChemmineOB} provides an R interface to a subset of cheminformatics
functionalities implemented by the @code{OpelBabel} C++ project.
@code{OpenBabel} is an open source cheminformatics toolbox that includes
utilities for structure format interconversions, descriptor calculations,
compound similarity searching and more. @code{ChemineOB} aims to make a subset
of these utilities available from within R.  For non-developers,
@code{ChemineOB} is primarily intended to be used from @code{ChemmineR} as an
add-on package rather than used directly.")
    (license license:asl2.0)))

(define-public r-chemminer
  (package
    (name "r-chemminer")
    (version "3.56.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ChemmineR" version))
       (sha256
        (base32
         "1252xy33bgjrd4gh4shrgzklgxry2lf67svzxjra5yckf8am0rdc"))))
    (properties `((upstream-name . "ChemmineR")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-base64enc
           r-bh
           r-biocgenerics
           r-dbi
           r-digest
           r-dt
           r-ggplot2
           r-gridextra
           r-jsonlite
           r-png
           r-rcpp
           r-rcurl
           r-rjson
           r-rsvg
           r-stringi))
    (native-inputs
     (list r-knitr))
    (home-page "https://github.com/girke-lab/ChemmineR")
    (synopsis "Cheminformatics toolkit for R")
    (description
     "ChemmineR is a cheminformatics package for analyzing drug-like small
molecule data in R.  It contains functions for efficient processing of large
numbers of molecules, physicochemical/structural property predictions,
structural similarity searching, classification and clustering of compound
libraries with a wide spectrum of algorithms.  In addition, it offers
visualization functions for compound clustering results and chemical
structures.")
    (license license:artistic2.0)))

(define-public r-fmcsr
  (package
    (name "r-fmcsr")
    (version "1.46.0")
    (source
      (origin
        (method url-fetch)
        (uri (bioconductor-uri "fmcsR" version))
        (sha256
          (base32 "080src34jwkvbk98pa4ri530l9gi4ibr5mwxib1681np0cfal54g"))))
    (properties `((upstream-name . "fmcsR")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocgenerics r-chemminer r-runit))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/girke-lab/fmcsR")
    (synopsis "Mismatch tolerant maximum common substructure searching")
    (description
     "The fmcsR package introduces an efficient @dfn{maximum common
substructure} (MCS) algorithms combined with a novel matching strategy that
allows for atom and/or bond mismatches in the substructures shared among two
small molecules.  The resulting flexible MCSs (FMCSs) are often larger than
strict MCSs, resulting in the identification of more common features in their
source structures, as well as a higher sensitivity in finding compounds with
weak structural similarities.  The fmcsR package provides several utilities to
use the FMCS algorithm for pairwise compound comparisons, structure similarity
searching and clustering.")
    (license license:artistic2.0)))

(define-public r-bioassayr
  (package
    (name "r-bioassayr")
    (version "1.42.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "bioassayR" version))
       (sha256
        (base32
         "0rckhgg73cgz0ppqaywc7nvbnp1is6cyjfa7dn2l32xxrcl57pq6"))))
    (properties `((upstream-name . "bioassayR")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocgenerics
           r-chemminer
           r-dbi
           r-matrix
           r-rjson
           r-rsqlite
           r-xml))
    (native-inputs
     (list r-knitr))
    (home-page "https://github.com/girke-lab/bioassayR")
    (synopsis "Cross-target analysis of small molecule bioactivity")
    (description
     "bioassayR is a computational tool that enables simultaneous analysis of
thousands of bioassay experiments performed over a diverse set of compounds
and biological targets.  Unique features include support for large-scale
cross-target analyses of both public and custom bioassays, generation of
@dfn{high throughput screening fingerprints} (HTSFPs), and an optional
preloaded database that provides access to a substantial portion of publicly
available bioactivity data.")
    (license license:artistic2.0)))

(define-public r-biobroom
  (package
    (name "r-biobroom")
    (version "1.36.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "biobroom" version))
       (sha256
        (base32
         "0flj14d8aamr1fzcpd0k7jq5rs0j5dsijpahg066qrnapn5990d0"))))
    (properties `((upstream-name . "biobroom")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biobase r-broom r-dplyr r-tidyr))
    (native-inputs
     (list r-knitr))
    (home-page "https://github.com/StoreyLab/biobroom")
    (synopsis "Turn Bioconductor objects into tidy data frames")
    (description
     "This package contains methods for converting standard objects
constructed by bioinformatics packages, especially those in Bioconductor, and
converting them to @code{tidy} data.  It thus serves as a complement to the
@code{broom} package, and follows the same tidy, augment, glance division of
tidying methods.  Tidying data makes it easy to recombine, reshape and
visualize bioinformatics analyses.")
    ;; Any version of the LGPL.
    (license license:lgpl3+)))

(define-public r-graphite
  (package
    (name "r-graphite")
    (version "1.50.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "graphite" version))
       (sha256
        (base32
         "1xymgghgj4r1k9i73zqgava79l0djz945jnl5ad193flv13fpdgs"))))
    (properties `((upstream-name . "graphite")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotationdbi r-graph r-httr r-purrr r-rappdirs r-rlang))
    (native-inputs (list r-r-rsp))
    (home-page "https://bioconductor.org/packages/graphite/")
    (synopsis "Networks from pathway databases")
    (description
     "Graphite provides networks derived from eight public pathway databases,
and automates the conversion of node identifiers (e.g. from Entrez IDs to gene
symbols).")
    (license license:agpl3+)))

(define-public r-reactomepa
  (package
    (name "r-reactomepa")
    (version "1.48.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ReactomePA" version))
       (sha256
        (base32
         "0i5jrdzg6zcs1jx0lryq8d467lxi1hvd1x6p74ywwxga88p46zk6"))))
    (properties `((upstream-name . "ReactomePA")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotationdbi
           r-dose
           r-enrichplot
           r-ggplot2
           r-ggraph
           r-graphite
           r-gson
           r-igraph
           r-reactome-db))
    (native-inputs
     (list r-knitr))
    (home-page "https://guangchuangyu.github.io/software/ReactomePA")
    (synopsis "Reactome pathway analysis")
    (description
     "This package provides functions for pathway analysis based on the
REACTOME pathway database.  It implements enrichment analysis, gene set
enrichment analysis and several functions for visualization.")
    (license license:gpl2)))

(define-public r-ebarrays
  (package
    (name "r-ebarrays")
    (version "2.68.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "EBarrays" version))
       (sha256
        (base32
         "17ym1y90bz85afanggdjf91a49lz89qmmi2kyhxga0cw056lkdz0"))))
    (properties `((upstream-name . "EBarrays")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biobase r-cluster r-lattice))
    (home-page "https://bioconductor.org/packages/EBarrays/")
    (synopsis "Gene clustering and differential expression identification")
    (description
     "EBarrays provides tools for the analysis of replicated/unreplicated
microarray data.")
    (license license:gpl2+)))

(define-public r-biocbaseutils
  (package
    (name "r-biocbaseutils")
    (version "1.6.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "BiocBaseUtils" version))
              (sha256
               (base32
                "0f8bzhpi6xla52sbvpd20sgli7mp5xfk2aqpcv6jkf8s59jsmn54"))))
    (properties `((upstream-name . "BiocBaseUtils")))
    (build-system r-build-system)
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/BiocBaseUtils")
    (synopsis "General utility functions for developing Bioconductor packages")
    (description
     "The package provides utility functions related to package development.
These include functions that replace slots, and selectors for show methods.
It aims to coalesce the various helper functions often re-used throughout the
Bioconductor ecosystem.")
    (license license:artistic2.0)))

(define-public r-bioccasestudies
  (package
    (name "r-bioccasestudies")
    (version "1.52.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BiocCaseStudies" version))
       (sha256
        (base32
         "03n49b6fvyyzmvdy4yif3cl7yv21c09c8xdx4cvvax5zz4v4sab1"))))
    (properties
     `((upstream-name . "BiocCaseStudies")))
    (build-system r-build-system)
    (propagated-inputs (list r-biobase))
    (home-page "https://bioconductor.org/packages/BiocCaseStudies")
    (synopsis "Support for the case studies monograph")
    (description
     "This package provides software and data to support the case studies
monograph.")
    (license license:artistic2.0)))

(define-public r-bioccheck
  (package
    (name "r-bioccheck")
    (version "1.40.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "BiocCheck" version))
              (sha256
               (base32
                "0rvr68d5anir7bj913nxwwfj7i3h2dvadcfvjjdbdr3m35wyx1g8"))))
    (properties
     `((upstream-name . "BiocCheck")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocbaseutils
           r-biocfilecache
           r-biocmanager
           r-biocviews
           r-callr
           r-codetools
           r-graph
           r-httr2
           r-knitr
           r-rvest
           r-stringdist))
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/BiocCheck")
    (synopsis "Executes Bioconductor-specific package checks")
    (description "This package contains tools to perform additional quality
checks on R packages that are to be submitted to the Bioconductor repository.")
    (license license:artistic2.0)))

(define-public r-biocgraph
  (package
    (name "r-biocgraph")
    (version "1.66.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "biocGraph" version))
       (sha256
        (base32
         "0ji6pa8wfj57drpnp03vgas1pird84g15nnp6469jmx773izhfq0"))))
    (properties `((upstream-name . "biocGraph")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocgenerics r-geneplotter r-graph r-rgraphviz))
    (home-page "https://bioconductor.org/packages/biocGraph/")
    (synopsis "Graph examples and use cases in Bioinformatics")
    (description
     "This package provides examples and code that make use of the
different graph related packages produced by Bioconductor.")
    (license license:artistic2.0)))

(define-public r-biocstyle
  (package
    (name "r-biocstyle")
    (version "2.32.1")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "BiocStyle" version))
              (sha256
               (base32
                "1m0nn2m548726w576mx8mx29xj428q1rkpn7z7a7m8kzgcvvgkkr"))))
    (properties
     `((upstream-name . "BiocStyle")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocmanager r-bookdown r-knitr r-rmarkdown r-yaml))
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/BiocStyle")
    (synopsis "Bioconductor formatting styles")
    (description "This package provides standard formatting styles for
Bioconductor PDF and HTML documents.  Package vignettes illustrate use and
functionality.")
    (license license:artistic2.0)))

(define-public r-biocviews
  (package
    (name "r-biocviews")
    (version "1.72.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "biocViews" version))
              (sha256
               (base32
                "10j9s50ppfx8r752wkabls0n4gxp17z7dnaf352f3dcdbfa8ijh5"))))
    (properties
     `((upstream-name . "biocViews")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biobase
           r-biocmanager
           r-graph
           r-rbgl
           r-rcurl
           r-runit
           r-xml))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/biocViews")
    (synopsis "Bioconductor package categorization helper")
    (description "The purpose of biocViews is to create HTML pages that
categorize packages in a Bioconductor package repository according to keywords,
also known as views, in a controlled vocabulary.")
    (license license:artistic2.0)))

(define-public r-experimenthub
  (package
    (name "r-experimenthub")
    (version "2.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ExperimentHub" version))
       (sha256
        (base32
         "0mj1aqxyp84mn5m0fwk9lijysy594hhga3kv4mddv45vqxdsr08h"))))
    (properties `((upstream-name . "ExperimentHub")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotationhub
           r-biocfilecache
           r-biocgenerics
           r-biocmanager
           r-rappdirs
           r-s4vectors))
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/ExperimentHub/")
    (synopsis "Client to access ExperimentHub resources")
    (description
     "This package provides a client for the Bioconductor ExperimentHub web
resource.  ExperimentHub provides a central location where curated data from
experiments, publications or training courses can be accessed.  Each resource
has associated metadata, tags and date of modification.  The client creates
and manages a local cache of files retrieved enabling quick and reproducible
access.")
    (license license:artistic2.0)))

(define-public r-experimenthubdata
  (package
    (name "r-experimenthubdata")
    (version "1.30.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ExperimentHubData" version))
       (sha256
        (base32 "1adm3b0nacbdi4x5z71iiif9d876m3ip6crhbbr2hw4mbk6is7qn"))))
    (properties `((upstream-name . "ExperimentHubData")))
    (build-system r-build-system)
    (propagated-inputs (list r-annotationhubdata
                             r-biocgenerics
                             r-biocmanager
                             r-curl
                             r-dbi
                             r-experimenthub
                             r-httr
                             r-s4vectors))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/ExperimentHubData")
    (synopsis "Add resources to ExperimentHub")
    (description
     "This package provides functions to add metadata to @code{ExperimentHub}
db and resource files to AWS S3 buckets.")
    (license license:artistic2.0)))

(define-public r-grohmm
  (package
    (name "r-grohmm")
    (version "1.38.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "groHMM" version))
       (sha256
        (base32
         "1vmck9ridjjk22477sv8wx0x7n5irrsaf6nsh9lm3krk4i3iv715"))))
    (properties `((upstream-name . "groHMM")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-genomeinfodb
           r-genomicalignments
           r-genomicranges
           r-iranges
           r-mass
           r-rtracklayer
           r-s4vectors))
    (home-page "https://github.com/Kraus-Lab/groHMM")
    (synopsis "GRO-seq analysis pipeline")
    (description
     "This package provides a pipeline for the analysis of GRO-seq data.")
    (license license:gpl3+)))

(define-public r-multiassayexperiment
  (package
    (name "r-multiassayexperiment")
    (version "1.30.2")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "MultiAssayExperiment" version))
       (sha256
        (base32
         "1j74ql0lz2xf0jsf4cjfapdmxbf6hb22699d3dyj8gwd1s92myjz"))))
    (properties
     `((upstream-name . "MultiAssayExperiment")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biobase
           r-biocbaseutils
           r-biocgenerics
           r-delayedarray
           r-genomicranges
           r-iranges
           r-s4vectors
           r-summarizedexperiment
           r-tidyr))
    (native-inputs
     (list r-knitr r-r-rsp))
    (home-page "https://waldronlab.io/MultiAssayExperiment/")
    (synopsis "Integration of multi-omics experiments in Bioconductor")
    (description
     "MultiAssayExperiment harmonizes data management of multiple assays
performed on an overlapping set of specimens.  It provides a familiar
Bioconductor user experience by extending concepts from
@code{SummarizedExperiment}, supporting an open-ended mix of standard data
classes for individual assays, and allowing subsetting by genomic ranges or
rownames.")
    (license license:artistic2.0)))

(define-public r-bioconcotk
  (package
    (name "r-bioconcotk")
    (version "1.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BiocOncoTK" version))
       (sha256
        (base32
         "0h4cr7lrmbyx7kdj81s7ff75svr01rvzn54svqi93vjilawmpbm3"))))
    (properties `((upstream-name . "BiocOncoTK")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-bigrquery
           r-car
           r-complexheatmap
           r-curatedtcgadata
           r-dbi
           r-dplyr
           r-dt
           r-genomicfeatures
           r-genomicranges
           r-ggplot2
           r-ggpubr
           r-graph
           r-httr
           r-iranges
           r-magrittr
           r-mass
           r-plyr
           r-rgraphviz
           r-rjson
           r-s4vectors
           r-scales
           r-shiny
           r-summarizedexperiment))
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/BiocOncoTK")
    (synopsis "Bioconductor components for general cancer genomics")
    (description
     "The purpose of this package is to provide a central interface to various
tools for genome-scale analysis of cancer studies.")
    (license license:artistic2.0)))

(define-public r-biocor
  (package
    (name "r-biocor")
    (version "1.28.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BioCor" version))
       (sha256
        (base32
         "0jpi51f9cv3sv4l69974bba8c8k0inb6zxviv7i907ldaiiblr07"))))
    (properties `((upstream-name . "BioCor")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocparallel r-gseabase r-matrix))
    (native-inputs
     (list r-knitr))
    (home-page "https://llrs.github.io/BioCor/")
    (synopsis "Functional similarities")
    (description
     "This package provides tools to calculate functional similarities based
on the pathways described on KEGG and REACTOME or in gene sets.  These
similarities can be calculated for pathways or gene sets, genes, or clusters
and combined with other similarities.  They can be used to improve networks,
gene selection, testing relationships, and so on.")
    (license license:expat)))

(define-public r-biocpkgtools
  (package
    (name "r-biocpkgtools")
    (version "1.22.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BiocPkgTools" version))
       (sha256
        (base32
         "1nbzr1yjz65r0y8c312hnvagnc1pdgda4v6sxbpvlkl3z93jsy6x"))
       (snippet
        '(for-each delete-file
                   '("inst/htmlwidgets/lib/bioc_explore/bootstrap.min.js"
                     "inst/htmlwidgets/lib/bioc_explore/d3.v3.min.js"
                     "inst/htmlwidgets/lib/bioc_explore/jquery-2.2.4.min.js"
                     "inst/htmlwidgets/lib/bioc_explore/underscore-min.js")))))
    (properties `((upstream-name . "BiocPkgTools")))
    (build-system r-build-system)
    (arguments
     (list
      #:modules '((guix build utils)
                  (guix build r-build-system)
                  (srfi srfi-1))
      #:phases
      '(modify-phases %standard-phases
         (add-after 'unpack 'process-javascript
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "inst/htmlwidgets/lib/bioc_explore"
               (call-with-values
                   (lambda ()
                     (unzip2
                      `((,(assoc-ref inputs "js-bootstrap")
                         "bootstrap.min.js")
                        (,(assoc-ref inputs "js-d3")
                         "d3.v3.min.js")
                        (,(assoc-ref inputs "js-jquery")
                         "jquery-2.2.4.min.js")
                        (,(search-input-file inputs "/underscore.js")
                         "underscore-min.js"))))
                 (lambda (sources targets)
                   (for-each (lambda (source target)
                               (format #true "Processing ~a --> ~a~%"
                                       source target)
                               (invoke "esbuild" source "--minify"
                                       (string-append "--outfile=" target)))
                             sources targets)))))))))
    (propagated-inputs
     (list r-biocfilecache
           r-biocmanager
           r-biocviews
           r-dplyr
           r-dt
           r-gh
           r-graph
           r-htmltools
           r-htmlwidgets
           r-httr
           r-igraph
           r-jsonlite
           r-magrittr
           r-rbgl
           r-readr
           r-rlang
           r-rorcid
           r-rvest
           r-stringr
           r-tibble
           r-xml2))
    (native-inputs
     `(("esbuild" ,esbuild)
       ("r-knitr" ,r-knitr)
       ("js-bootstrap"
        ,(origin
           (method url-fetch)
           (uri
            "https://raw.githubusercontent.com/twbs/bootstrap/v3.3.6/dist/js/bootstrap.js")
           (sha256
            (base32
             "07fm28xbkb7a5n7zgmfxgbl2g5j010r4gvc54y79v1f119s3kz6y"))))
       ("js-d3"
        ,(origin
           (method url-fetch)
           (uri "https://d3js.org/d3.v3.js")
           (sha256
            (base32
             "1arr7sr08vy7wh0nvip2mi7dpyjw4576vf3bm45rp4g5lc1k1x41"))))
       ("js-jquery"
        ,(origin
           (method url-fetch)
           (uri "https://code.jquery.com/jquery-2.2.4.js")
           (sha256
            (base32
             "18m6qmmsm3knvybf6gpwmwiasi05y98gcpb364if8qh94gv90gl9"))))
       ("js-underscore"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/jashkenas/underscore")
                 (commit "1.8.3")))
           (file-name (git-file-name "underscorejs" "1.8.3"))
           (sha256
            (base32
             "1r54smxpl3c6jg6py29xjc2l1z49rlm1h48vr9i57wvnkbnbl0h3"))))))
    (home-page "https://github.com/seandavi/BiocPkgTools")
    (synopsis "Collection of tools for learning about Bioconductor packages")
    (description
     "Bioconductor has a rich ecosystem of metadata around packages, usage,
and build status.  This package is a simple collection of functions to access
that metadata from R.  The goal is to expose metadata for data mining and
value-added functionality such as package searching, text mining, and
analytics on packages.")
    (license license:expat)))

(define-public r-biocset
  (package
    (name "r-biocset")
    (version "1.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BiocSet" version))
       (sha256
        (base32
         "1r1if1k7qgni3dk7gsl4vcyvd6hnhx94rcq5yi9w7dvyyrbvnwwa"))))
    (properties `((upstream-name . "BiocSet")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotationdbi
           r-biocio
           r-dplyr
           r-keggrest
           r-ontologyindex
           r-plyr
           r-rlang
           r-s4vectors
           r-tibble
           r-tidyr))
    (native-inputs
     (list r-knitr))
    (home-page
     "https://bioconductor.org/packages/BiocSet")
    (synopsis
     "Representing Different Biological Sets")
    (description
     "BiocSet displays different biological sets in a triple tibble format.
These three tibbles are @code{element}, @code{set}, and @code{elementset}.
The user has the ability to activate one of these three tibbles to perform
common functions from the @code{dplyr} package.  Mapping functionality and
accessing web references for elements/sets are also available in BiocSet.")
    (license license:artistic2.0)))

(define-public r-biocworkflowtools
  (package
    (name "r-biocworkflowtools")
    (version "1.30.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BiocWorkflowTools" version))
       (sha256
        (base32
         "1gw0lmdhkzp5ar54yv9jagqmpxp7j9ajfmdqb2y2qrnhh39hymzs"))))
    (properties
     `((upstream-name . "BiocWorkflowTools")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocstyle
           r-bookdown
           r-git2r
           r-httr
           r-knitr
           r-rmarkdown
           r-rstudioapi
           r-stringr
           r-usethis))
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/BiocWorkflowTools/")
    (synopsis "Tools to aid the development of Bioconductor Workflow packages")
    (description
     "This package provides functions to ease the transition between
Rmarkdown and LaTeX documents when authoring a Bioconductor Workflow.")
    (license license:expat)))

(define-public r-biodist
  (package
    (name "r-biodist")
    (version "1.76.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "bioDist" version))
       (sha256
        (base32
         "1yxr5df1y8v5c9imh27j86s7g9wjvi51j2wyn8jgma875mad41hq"))))
    (properties `((upstream-name . "bioDist")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biobase r-kernsmooth))
    (home-page "https://bioconductor.org/packages/bioDist/")
    (synopsis "Different distance measures")
    (description
     "This package provides a collection of software tools for calculating
distance measures.")
    (license license:artistic2.0)))

(define-public r-pcatools
  (package
    (name "r-pcatools")
    (version "2.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "PCAtools" version))
       (sha256
        (base32
         "04a1fvwgqxzf7992lyh9qzz2z2fgmfwfjvpw5xr21pcrpas57l2r"))))
    (properties `((upstream-name . "PCAtools")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-beachmat
           r-bh
           r-biocparallel
           r-biocsingular
           r-cowplot
           r-delayedarray
           r-delayedmatrixstats
           r-dqrng
           r-ggplot2
           r-ggrepel
           r-lattice
           r-matrix
           r-rcpp
           r-reshape2))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/kevinblighe/PCAtools")
    (synopsis "PCAtools: everything Principal Components Analysis")
    (description
     "@dfn{Principal Component Analysis} (PCA) extracts the fundamental
structure of the data without the need to build any model to represent it.
This \"summary\" of the data is arrived at through a process of reduction that
can transform the large number of variables into a lesser number that are
uncorrelated (i.e. the 'principal components'), while at the same time being
capable of easy interpretation on the original data.  PCAtools provides
functions for data exploration via PCA, and allows the user to generate
publication-ready figures.  PCA is performed via @code{BiocSingular}; users
can also identify an optimal number of principal components via different
metrics, such as the elbow method and Horn's parallel analysis, which has
relevance for data reduction in single-cell RNA-seq (scRNA-seq) and high
dimensional mass cytometry data.")
    (license license:gpl3)))

(define-public r-rgreat
  (package
    (name "r-rgreat")
    (version "2.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "rGREAT" version))
       (sha256
        (base32
         "0sszyfnnzzn5xhs39zgkzr7f3da820nxr6qaw68wknblgmyyddi8"))))
    (properties `((upstream-name . "rGREAT")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotationdbi
           r-circlize
           r-digest
           r-doparallel
           r-dt
           r-foreach
           r-genomeinfodb
           r-genomicfeatures
           r-genomicranges
           r-getoptlong
           r-globaloptions
           r-go-db
           r-iranges
           r-org-hs-eg-db
           r-progress
           r-rcolorbrewer
           r-rcpp
           r-rcurl
           r-rjson
           r-s4vectors
           r-shiny
           r-txdb-hsapiens-ucsc-hg19-knowngene
           r-txdb-hsapiens-ucsc-hg38-knowngene))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/jokergoo/rGREAT")
    (synopsis "Client for GREAT analysis")
    (description
     "This package makes GREAT (Genomic Regions Enrichment of Annotations
Tool) analysis automatic by constructing a HTTP POST request according to
user's input and automatically retrieving results from GREAT web server.")
    (license license:expat)))

(define-public r-m3c
  (package
    (name "r-m3c")
    (version "1.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "M3C" version))
       (sha256
        (base32
         "1h3zxb3m68lq37g25zqxpmk2c9yrdhydyhh7ad2d30hif73qiacm"))))
    (properties `((upstream-name . "M3C")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-cluster
           r-corpcor
           r-doparallel
           r-dosnow
           r-foreach
           r-ggplot2
           r-matrix
           r-matrixcalc
           r-rtsne
           r-umap))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/M3C")
    (synopsis "Monte Carlo reference-based consensus clustering")
    (description
     "M3C is a consensus clustering algorithm that uses a Monte Carlo
simulation to eliminate overestimation of @code{K} and can reject the null
hypothesis @code{K=1}.")
    (license license:agpl3+)))

(define-public r-icens
  (package
    (name "r-icens")
    (version "1.76.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Icens" version))
       (sha256
        (base32
         "16bblkxp8pj7qaxfzmp30lfj8vy13d3q8cf5y0g40lk4rbldx3bz"))))
    (properties `((upstream-name . "Icens")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-survival))
    (home-page "https://bioconductor.org/packages/Icens")
    (synopsis "NPMLE for censored and truncated data")
    (description
     "This package provides many functions for computing the
@dfn{nonparametric maximum likelihood estimator} (NPMLE) for censored and
truncated data.")
    (license license:artistic2.0)))

;; This is a CRAN package but it depends on r-icens, which is published on
;; Bioconductor.
(define-public r-interval
  (package
    (name "r-interval")
    (version "1.1-1.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "interval" version))
       (sha256
        (base32
         "0kmz7wpgc4kspr11l0nls4rq0qk2lbsg3s4bpl0zs4r4pjcrwvrx"))))
    (properties `((upstream-name . "interval")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-icens r-mlecens r-perm r-survival))
    (home-page "https://cran.r-project.org/web/packages/interval/")
    (synopsis "Weighted Logrank tests and NPMLE for interval censored data")
    (description
     "This package provides functions to fit nonparametric survival curves,
plot them, and perform logrank or Wilcoxon type tests.")
    (license license:gpl2+)))

;; This is a CRAN package, but it depends on r-interval, which depends on a
;; Bioconductor package.
(define-public r-fhtest
  (package
    (name "r-fhtest")
    (version "1.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "FHtest" version))
       (sha256
        (base32
         "0x7lr88w0b09ng7nps490kgj8aqdjzmp9skv9iwqgn871pnpydms"))))
    (properties `((upstream-name . "FHtest")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-interval r-kmsurv r-mass r-perm r-survival))
    (home-page "https://cran.r-project.org/web/packages/FHtest/")
    (synopsis "Tests for survival data based on the Fleming-Harrington class")
    (description
     "This package provides functions to compare two or more survival curves
with:

@itemize
@item The Fleming-Harrington test for right-censored data based on
  permutations and on counting processes.
@item An extension of the Fleming-Harrington test for interval-censored data
  based on a permutation distribution and on a score vector distribution.
@end itemize
")
    (license license:gpl2+)))

(define-public r-fourcseq
  (package
    (name "r-fourcseq")
    (version "1.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "FourCSeq" version))
       (sha256
        (base32 "1rwdphcj26xis47n8j1fiyc3k3qbsgn0bhf5bhgy5vm11yqyvicb"))))
    (properties `((upstream-name . "FourCSeq")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biobase
           r-biostrings
           r-deseq2
           r-fda
           r-genomicalignments
           r-genomicranges
           r-ggbio
           r-ggplot2
           r-gtools
           r-lsd
           r-matrix
           r-reshape2
           r-rsamtools
           r-rtracklayer
           r-summarizedexperiment))
    (native-inputs
     (list r-knitr))
    (home-page
     "https://bioconductor.org/packages/release/bioc/html/FourCSeq.html")
    (synopsis "Analysis of multiplexed 4C sequencing data")
    (description
     "This package is an R package dedicated to the analysis of (multiplexed)
4C sequencing data.  @code{r-fourcseq} provides a pipeline to detect specific
interactions between DNA elements and identify differential interactions
between conditions.  The statistical analysis in R starts with individual bam
files for each sample as inputs.  To obtain these files, the package contains
a Python script to demultiplex libraries and trim off primer sequences.  With
a standard alignment software the required bam files can be then be
generated.")
    (license license:gpl3+)))

(define-public r-preprocesscore
  (package
    (name "r-preprocesscore")
    (version "1.66.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "preprocessCore" version))
       (sha256
        (base32
         "1cd91iakbpf2hjysc44sqqrah63cgq4fz3n8j81virfwl3ir95xm"))))
    (properties
     `((upstream-name . "preprocessCore")))
    (build-system r-build-system)
    (home-page "https://github.com/bmbolstad/preprocessCore")
    (synopsis "Collection of pre-processing functions")
    (description
     "This package provides a library of core pre-processing and normalization
routines.")
    (license license:lgpl2.0+)))

(define-public r-s4arrays
  (package
    (name "r-s4arrays")
    (version "1.4.1")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "S4Arrays" version))
              (sha256
               (base32
                "0wdzp1q6hyks4afm217yl2n770jsqaqxix71imni1r0qbxdfkdwl"))))
    (properties `((upstream-name . "S4Arrays")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-abind
           r-biocgenerics
           r-crayon
           r-iranges
           r-matrix
           r-s4vectors))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/S4Arrays")
    (synopsis "Foundation of array-like containers in Bioconductor")
    (description
     "The S4Arrays package defines the @code{Array} virtual class to be
extended by other S4 classes that wish to implement a container with an
array-like semantic.  It also provides:

@enumerate
@item low-level functionality meant to help the developer of such container to
  implement basic operations like display, subsetting, or coercion of their
  array-like objects to an ordinary matrix or array, and
@item a framework that facilitates block processing of array-like
  objects (typically on-disk objects).
@end enumerate
")
    (license license:artistic2.0)))

(define-public r-s4vectors
  (package
    (name "r-s4vectors")
    (version "0.42.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "S4Vectors" version))
              (sha256
               (base32
                "0aq0j70sl8a42gc1abxdwdy74mi9k4krz56yrbpwqv78x42cjjm9"))))
    (properties
     `((upstream-name . "S4Vectors")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocgenerics))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/S4Vectors")
    (synopsis "S4 implementation of vectors and lists")
    (description
     "The S4Vectors package defines the @code{Vector} and @code{List} virtual
classes and a set of generic functions that extend the semantic of ordinary
vectors and lists in R.  Package developers can easily implement vector-like
or list-like objects as concrete subclasses of @code{Vector} or @code{List}.
In addition, a few low-level concrete subclasses of general interest (e.g.
@code{DataFrame}, @code{Rle}, and @code{Hits}) are implemented in the
S4Vectors package itself.")
    (license license:artistic2.0)))

;; This is a CRAN package, but it depends on preprocessorcore, which is a
;; Bioconductor package.
(define-public r-wgcna
  (package
    (name "r-wgcna")
    (version "1.72-5")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "WGCNA" version))
       (sha256
        (base32
         "17g7lan1rpy6y4nmqksrf9ddp3gs58vdczfavgq1fp13zx1r2hq3"))))
    (properties `((upstream-name . "WGCNA")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotationdbi
           r-doparallel
           r-dynamictreecut
           r-fastcluster
           r-foreach
           r-go-db
           r-hmisc
           r-impute
           r-matrixstats
           r-preprocesscore
           r-rcpp
           r-survival))
    (home-page
     "http://www.genetics.ucla.edu/labs/horvath/CoexpressionNetwork/Rpackages/WGCNA/")
    (synopsis "Weighted correlation network analysis")
    (description
     "This package provides functions necessary to perform Weighted
Correlation Network Analysis on high-dimensional data.  It includes functions
for rudimentary data cleaning, construction and summarization of correlation
networks, module identification and functions for relating both variables and
modules to sample traits.  It also includes a number of utility functions for
data manipulation and visualization.")
    (license license:gpl2+)))

(define-public r-rgraphviz
  (package
    (name "r-rgraphviz")
    (version "2.48.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Rgraphviz" version))
       (sha256
        (base32
         "09i174a867vqyb6677sj88lk6bpfc4ajsmjgjkd7f7q66j46q655"))))
    (properties `((upstream-name . "Rgraphviz")))
    (build-system r-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'make-reproducible
           (lambda _
             ;; The replacement value is taken from src/graphviz/builddate.h
             (substitute* "src/graphviz/configure"
               (("VERSION_DATE=.*")
                "VERSION_DATE=20200427.2341\n"))
             #t)))))
    ;; FIXME: Rgraphviz bundles the sources of an older variant of
    ;; graphviz.  It does not build with the latest version of graphviz, so
    ;; we do not add graphviz to the inputs.
    (inputs (list zlib))
    (propagated-inputs
     (list r-graph))
    (native-inputs
     (list pkg-config))
    (home-page "https://bioconductor.org/packages/Rgraphviz")
    (synopsis "Plotting capabilities for R graph objects")
    (description
     "This package interfaces R with the graphviz library for plotting R graph
objects from the @code{graph} package.")
    (license license:epl1.0)))

(define-public r-fishpond
  (package
    (name "r-fishpond")
    (version "2.10.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "fishpond" version))
              (sha256
               (base32
                "02lzx8whxd3z4k0saf0vaq4np8lag00nkp3hd6da1w91vczs1kjl"))))
    (properties `((upstream-name . "fishpond")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-abind
           r-genomicranges
           r-gtools
           r-iranges
           r-jsonlite
           r-matrix
           r-matrixstats
           r-qvalue
           r-s4vectors
           r-singlecellexperiment
           r-summarizedexperiment
           r-svmisc))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/mikelove/fishpond")
    (synopsis "Downstream methods and tools for expression data")
    (description
     "The @code{fishpond} package contains methods for differential transcript
and gene expression analysis of RNA-seq data using inferential replicates for
uncertainty of abundance quantification, as generated by Gibbs sampling or
bootstrap sampling.  Also the package contains a number of utilities for
working with Salmon and Alevin quantification files.")
    (license license:gpl2)))

(define-public r-fithic
  (package
    (name "r-fithic")
    (version "1.30.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "FitHiC" version))
              (sha256
               (base32
                "107hm9k9wfr34fradldmjym2bzxb6qx8109p4a7xrqkiya0hmwwa"))))
    (properties `((upstream-name . "FitHiC")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-data-table r-fdrtool r-rcpp))
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/FitHiC")
    (synopsis "Confidence estimation for intra-chromosomal contact maps")
    (description
     "Fit-Hi-C is a tool for assigning statistical confidence estimates to
intra-chromosomal contact maps produced by genome-wide genome architecture
assays such as Hi-C.")
    (license license:gpl2+)))

(define-public r-hitc
  (package
    (name "r-hitc")
    (version "1.48.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "HiTC" version))
              (sha256
               (base32
                "0nxk3rdfw78bwyxjd3bbm093i8hqyk63rz0fvx3fi04qpa1fv39r"))))
    (properties `((upstream-name . "HiTC")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biostrings
           r-genomeinfodb
           r-genomicranges
           r-iranges
           r-matrix
           r-rcolorbrewer
           r-rtracklayer))
    (home-page "https://bioconductor.org/packages/HiTC")
    (synopsis "High throughput chromosome conformation capture analysis")
    (description
     "The HiTC package was developed to explore high-throughput \"C\" data
such as 5C or Hi-C.  Dedicated R classes as well as standard methods for
quality controls, normalization, visualization, and further analysis are also
provided.")
    (license license:artistic2.0)))

(define-public r-hdf5array
  (package
    (name "r-hdf5array")
    (version "1.32.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "HDF5Array" version))
       (sha256
        (base32
         "0j8rz99qw1rism3f5cv7rksccijmnchrkw7s9shxk0528f6dj6p1"))))
    (properties `((upstream-name . "HDF5Array")))
    (build-system r-build-system)
    (inputs
     (list))
    (propagated-inputs
     (list r-biocgenerics
           r-delayedarray
           r-iranges
           r-matrix
           r-rhdf5
           r-rhdf5filters
           r-rhdf5lib
           r-s4arrays
           r-s4vectors))
    (home-page "https://bioconductor.org/packages/HDF5Array")
    (synopsis "HDF5 back end for DelayedArray objects")
    (description "This package provides an array-like container for convenient
access and manipulation of HDF5 datasets.  It supports delayed operations and
block processing.")
    (license license:artistic2.0)))

(define-public r-rhdf5lib
  (package
    (name "r-rhdf5lib")
    (version "1.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Rhdf5lib" version))
       (sha256
        (base32
         "0q4ix3nzr69pmwm6vdna51yxlqi8frch0xyv6wmiy35n67d4fcab"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Delete bundled binaries
           (delete-file-recursively "src/wininclude/")
           (delete-file-recursively "src/winlib/")
           (delete-file "src/hdf5small_cxx_hl_1.10.7.tar.gz")))))
    (properties `((upstream-name . "Rhdf5lib")))
    (build-system r-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'do-not-use-bundled-hdf5
           (lambda* (#:key inputs #:allow-other-keys)
             (for-each delete-file '("configure" "configure.ac"))
             (substitute* "R/zzz.R"
               (("return\\(links\\)") "return(\" -lz\")"))
             (with-directory-excursion "src"
               (invoke "tar" "xvf" (assoc-ref inputs "hdf5-source"))
               (rename-file (string-append "hdf5-" ,(package-version hdf5-1.10))
                            "hdf5")
               ;; Remove timestamp and host system information to make
               ;; the build reproducible.
               (substitute* "hdf5/src/libhdf5.settings.in"
                 (("Configured on: @CONFIG_DATE@")
                  "Configured on: Guix")
                 (("Uname information:.*")
                  "Uname information: Linux\n")
                 ;; Remove unnecessary store reference.
                 (("C Compiler:.*")
                  "C Compiler: GCC\n"))
               (rename-file "hdf5/src/libhdf5.settings.in"
                            "hdf5/src/libhdf5.settings")
               (rename-file "Makevars.in" "Makevars")
               (substitute* "Makevars"
                 (("@BUILD_HDF5@") "")
                 (("@COPY_SZIP@") "")
                 (("HDF5_CXX_LIB=.*")
                  (string-append "HDF5_CXX_LIB="
                                 (assoc-ref inputs "hdf5") "/lib/libhdf5_cpp.a\n"))
                 (("HDF5_LIB=.*")
                  (string-append "HDF5_LIB="
                                 (assoc-ref inputs "hdf5") "/lib/libhdf5.a\n"))
                 (("HDF5_CXX_INCLUDE=.*") "HDF5_CXX_INCLUDE=./hdf5/c++/src\n")
                 (("HDF5_INCLUDE=.*") "HDF5_INCLUDE=./hdf5/src\n")
                 (("HDF5_HL_INCLUDE=.*") "HDF5_HL_INCLUDE=./hdf5/hl/src\n")
                 (("HDF5_HL_CXX_INCLUDE=.*") "HDF5_HL_CXX_INCLUDE=./hdf5/hl/c++/src\n")
                 (("HDF5_HL_LIB=.*")
                  (string-append "HDF5_HL_LIB="
                                 (assoc-ref inputs "hdf5") "/lib/libhdf5_hl.a\n"))
                 (("HDF5_HL_CXX_LIB=.*")
                  (string-append "HDF5_HL_CXX_LIB="
                                 (assoc-ref inputs "hdf5") "/lib/libhdf5_hl_cpp.a\n"))
                 (("@ZLIB_LIB_PATH@") "-lz")
                 (("@ZLIB_INCLUDE_PATH@") "")

                 ;; szip is non-free software
                 (("cp \"\\$\\{SZIP_LIB\\}.*") "")
                 (("PKG_LIBS =.*") "PKG_LIBS = -lz -lhdf5\n"))))))))
    (propagated-inputs
     (list hdf5-1.10 zlib))
    (native-inputs
     `(("hdf5-source" ,(package-source hdf5-1.10))
       ("r-knitr" ,r-knitr)))
    (home-page "https://bioconductor.org/packages/Rhdf5lib")
    (synopsis "HDF5 library as an R package")
    (description "This package provides C and C++ HDF5 libraries for use in R
packages.")
    (license license:artistic2.0)))

(define-public r-beachmat
  (package
    (name "r-beachmat")
    (version "2.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "beachmat" version))
       (sha256
        (base32
         "0gvxz9w2mi8scqyyhhnrasscs7hwvxxlqvh7j9m6akpi8qq91v6y"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocgenerics r-delayedarray r-matrix r-rcpp r-sparsearray))
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/beachmat")
    (synopsis "Compiling Bioconductor to handle each matrix type")
    (description "This package provides a consistent C++ class interface for a
variety of commonly used matrix types, including sparse and HDF5-backed
matrices.")
    (license license:gpl3)))

(define-public r-beadarray
  (package
    (name "r-beadarray")
    (version "2.54.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "beadarray" version))
              (sha256
               (base32
                "0yda6maj985k374bh6zkrs8qffw1jrz9gc3mja07ibyn4pvzkclq"))))
    (properties `((upstream-name . "beadarray")))
    (build-system r-build-system)
    (propagated-inputs (list r-annotationdbi
                             r-beaddatapackr
                             r-biobase
                             r-biocgenerics
                             r-genomicranges
                             r-ggplot2
                             r-hexbin
                             r-illuminaio
                             r-iranges
                             r-limma
                             r-reshape2))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/beadarray")
    (synopsis
     "Quality assessment and low-level analysis for Illumina BeadArray data")
    (description
     "The package is able to read bead-level data (raw TIFFs and text files)
output by BeadScan as well as bead-summary data from BeadStudio.  Methods for
quality assessment and low-level analysis are provided.")
    (license license:expat)))

(define-public r-beadarraysnp
  (package
    (name "r-beadarraysnp")
    (version "1.68.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "beadarraySNP" version))
       (sha256
        (base32 "1vrizh7fpzv1w2gki0qxhphrfj1jj6m6bjsjsbw0b4n4invybzps"))))
    (properties `((upstream-name . "beadarraySNP")))
    (build-system r-build-system)
    (propagated-inputs (list r-biobase r-quantsmooth))
    (home-page "https://bioconductor.org/packages/beadarraySNP")
    (synopsis "Normalization and reporting of Illumina SNP bead arrays")
    (description
     "This package is importing data from Illumina SNP experiments and it
performs copy number calculations and reports.")
    (license license:gpl2)))

(define-public r-beaddatapackr
  (package
    (name "r-beaddatapackr")
    (version "1.56.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "BeadDataPackR" version))
              (sha256
               (base32
                "0wz3prri0mfjczmb432faxfjc99wg4lq26l3a60rmxyxq8mcnpkv"))))
    (properties `((upstream-name . "BeadDataPackR")))
    (build-system r-build-system)
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/BeadDataPackR")
    (synopsis "Compression of Illumina BeadArray data")
    (description
     "This package provides functionality for the compression and
decompression of raw bead-level data from the Illumina BeadArray platform.")
    (license license:gpl2)))

;; This package includes files that have been taken from kentutils.  Some
;; parts of kentutils are not released under a free license, but this package
;; only uses files that are also found in the free parts of kentutils.
(define-public r-cner
  (package
    (name "r-cner")
    (version "1.40.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "CNEr" version))
       (sha256
        (base32 "19kfh7p8zlb1nbwpsj6i1b7pqy829dij7n7iavznhxw44cmif6sv"))))
    (properties `((upstream-name . "CNEr")))
    (build-system r-build-system)
    (inputs (list zlib))
    (propagated-inputs
     (list r-annotate
           r-biocgenerics
           r-biostrings
           r-dbi
           r-genomeinfodb
           r-genomicalignments
           r-genomicranges
           r-ggplot2
           r-go-db
           r-iranges
           r-keggrest
           r-powerlaw
           r-pwalign
           r-r-utils
           r-readr
           r-reshape2
           r-rsqlite
           r-rtracklayer
           r-s4vectors
           r-xvector))
    (native-inputs
     (list r-knitr))
    (home-page "https://github.com/ge11232002/CNEr")
    (synopsis "CNE Detection and Visualization")
    (description
     "This package provides tools for large-scale identification and
advanced visualization of sets of conserved noncoding elements.")
    ;; For all files in src/ucsc "license is hereby granted for all use -
    ;; public, private or commercial"; this includes those files that don't
    ;; have a license header, because they are included in the free parts of
    ;; the kentutils package.
    (license (list license:gpl2
                   (license:non-copyleft
                    "https://raw.githubusercontent.com/ucscGenomeBrowser/kent/v410_base/src/lib/LICENSE")))))

(define-public r-tfbstools
  (package
    (name "r-tfbstools")
    (version "1.42.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "TFBSTools" version))
       (sha256
        (base32
         "0bmmsgmx7159bdrflykkis0v4z1ixwlnmw1kx0y0kw15lw3cjnsh"))))
    (properties `((upstream-name . "TFBSTools")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biobase
           r-biocgenerics
           r-biocparallel
           r-biostrings
           r-bsgenome
           r-catools
           r-cner
           r-dbi
           r-dirichletmultinomial
           r-genomeinfodb
           r-genomicranges
           r-gtools
           r-iranges
           r-pwalign
           r-rsqlite
           r-rtracklayer
           r-s4vectors
           r-seqlogo
           r-tfmpvalue
           r-xml
           r-xvector))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/ge11232002/TFBSTools")
    (synopsis "Transcription factor binding site (TFBS) analysis")
    (description
     "TFBSTools is a package for the analysis and manipulation of
transcription factor binding sites.  It includes matrices conversion
between @dfn{Position Frequency Matrix} (PFM), @dfn{Position Weight
Matrix} (PWM) and @dfn{Information Content Matrix} (ICM).  It can also
scan putative TFBS from sequence/alignment, query JASPAR database and
provides a wrapper of de novo motif discovery software.")
    (license license:gpl2)))

(define-public r-maftools
  (package
    (name "r-maftools")
    (version "2.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "maftools" version))
       (sha256
        (base32 "0c2sf8l1wfgz3nh9ccm10y9wwh7rv55r09awavqlrr9glcbblp32"))))
    (properties `((upstream-name . "maftools")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-data-table
           r-dnacopy
           r-rcolorbrewer
           r-rhtslib
           r-survival
           r-zlibbioc))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/PoisonAlien/maftools")
    (synopsis "Summarize, analyze and visualize MAF files")
    (description
     "Analyze and visualize Mutation Annotation Format (MAF) files from large
scale sequencing studies.  This package provides various functions to perform
most commonly used analyses in cancer genomics and to create feature rich
customizable visualzations with minimal effort.")
    (license license:expat)))

(define-public r-motifmatchr
  (package
    (name "r-motifmatchr")
    (version "1.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "motifmatchr" version))
       (sha256
        (base32
         "1wpvdg5x51cbzpqa4s2s0zl9h7fsrsp87mxxlhxs65zx6yyfj2xl"))))
    (properties `((upstream-name . "motifmatchr")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biostrings
           r-bsgenome
           r-genomeinfodb
           r-genomicranges
           r-iranges
           r-matrix
           r-rcpp
           r-rcpparmadillo
           r-rsamtools
           r-s4vectors
           r-summarizedexperiment
           r-tfbstools))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/motifmatchr")
    (synopsis "Fast motif matching in R")
    (description
     "Quickly find motif matches for many motifs and many sequences.
This package wraps C++ code from the MOODS motif calling library.")
    (license license:gpl3)))

(define-public r-chromvar
  (package
    (name "r-chromvar")
    (version "1.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "chromVAR" version))
       (sha256
        (base32 "092y1x1sbqvjrwmr7sv2r5216wbwwc4lk6kwn57fnvvdnc0hc3sv"))))
    (properties `((upstream-name . "chromVAR")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocgenerics
           r-biocparallel
           r-biostrings
           r-bsgenome
           r-dt
           r-genomeinfodb
           r-genomicranges
           r-ggplot2
           r-iranges
           r-matrix
           r-miniui
           r-nabor
           r-plotly
           r-rcolorbrewer
           r-rcpp
           r-rcpparmadillo
           r-rsamtools
           r-rtsne
           r-s4vectors
           r-shiny
           r-summarizedexperiment
           r-tfbstools))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/release/bioc/html/chromVAR.html")
    (synopsis "Determine chromatin variation across regions")
    (description
     "This package @code{r-chromvar} determines variation in chromatin
accessibility across sets of annotations or peaks.  @code{r-chromvar} is
designed primarily for single-cell or sparse chromatin accessibility data like
single cell assay for transposase-accessible chromatin using
sequencing (@code{scATAC-seq} or sparse bulk ATAC or deoxyribonuclease
sequence (@code{DNAse-seq}) experiments.")
    (license license:expat)))

(define-public r-singlecellexperiment
  (package
    (name "r-singlecellexperiment")
    (version "1.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "SingleCellExperiment" version))
       (sha256
        (base32
         "09sbk0r9g55x8fbvv5rbrygdw3km4bbrj9x0abqwld2nxnxq1q3n"))))
    (properties
     `((upstream-name . "SingleCellExperiment")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocgenerics r-delayedarray r-genomicranges r-s4vectors
           r-summarizedexperiment))
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/SingleCellExperiment")
    (synopsis "S4 classes for single cell data")
    (description "This package defines an S4 class for storing data from
single-cell experiments.  This includes specialized methods to store and
retrieve spike-in information, dimensionality reduction coordinates and size
factors for each cell, along with the usual metadata for genes and
libraries.")
    (license license:gpl3)))

(define-public r-singler
  (package
    (name "r-singler")
    (version "2.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "SingleR" version))
       (sha256
        (base32 "0l33bsi1zfj7f4hzlivhh89phhx0qscrwd08c28mqvcyp91mhq16"))))
    (properties `((upstream-name . "SingleR")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-beachmat
           r-biocneighbors
           r-biocparallel
           r-biocsingular
           r-delayedarray
           r-delayedmatrixstats
           r-matrix
           r-rcpp
           r-s4vectors
           r-summarizedexperiment))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/LTLA/SingleR")
    (synopsis "Reference-based single-cell RNA-seq annotation")
    (description
     "This package performs unbiased cell type recognition from single-cell
RNA sequencing data, by leveraging reference transcriptomic datasets of pure
cell types to infer the cell of origin of each single cell independently.")
    (license license:gpl3)))

(define-public r-scuttle
  (package
    (name "r-scuttle")
    (version "1.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "scuttle" version))
       (sha256
        (base32
         "1cja56k5y5vi1x0jdxaxvh13fc2lzh8g30gp294pz6p61prrf3ws"))))
    (properties `((upstream-name . "scuttle")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-beachmat
           r-biocgenerics
           r-biocparallel
           r-delayedarray
           r-delayedmatrixstats
           r-genomicranges
           r-matrix
           r-rcpp
           r-s4vectors
           r-singlecellexperiment
           r-summarizedexperiment))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/scuttle")
    (synopsis "Single-cell RNA-Seq analysis utilities")
    (description
     "This package provides basic utility functions for performing single-cell
analyses, focusing on simple normalization, quality control and data
transformations.  It also provides some helper functions to assist development
of other packages.")
    (license license:gpl3)))

(define-public r-scater
  (package
    (name "r-scater")
    (version "1.32.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "scater" version))
              (sha256
               (base32
                "096crwk5a9i7h2pykyi6vgnrnxas9wdz5dx920yh74smyh527jiq"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-beachmat
           r-biocgenerics
           r-biocneighbors
           r-biocparallel
           r-biocsingular
           r-delayedarray
           r-ggbeeswarm
           r-ggplot2
           r-ggrastr
           r-ggrepel
           r-matrix
           r-matrixgenerics
           r-pheatmap
           r-rcolorbrewer
           r-rcppml
           r-rlang
           r-rtsne
           r-s4vectors
           r-scuttle
           r-singlecellexperiment
           r-summarizedexperiment
           r-uwot
           r-viridis))
    (native-inputs
     (list r-knitr))
    (home-page "https://github.com/davismcc/scater")
    (synopsis "Single-cell analysis toolkit for gene expression data in R")
    (description "This package provides a collection of tools for doing
various analyses of single-cell RNA-seq gene expression data, with a focus on
quality control.")
    (license license:gpl2+)))

(define-public r-scran
  (package
    (name "r-scran")
    (version "1.32.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "scran" version))
       (sha256
        (base32
         "0s1y2q4v596wxh8p5sr43mk9cnsc4kg36ld7vx71hbn3hwscqaa1"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-beachmat
           r-bh
           r-biocgenerics
           r-biocparallel
           r-biocsingular
           r-bluster
           r-delayedarray
           r-delayedmatrixstats
           r-dqrng
           r-edger
           r-igraph
           r-limma
           r-matrix
           r-metapod
           r-rcpp
           r-s4vectors
           r-scuttle
           r-singlecellexperiment
           r-statmod
           r-summarizedexperiment))
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/scran")
    (synopsis "Methods for single-cell RNA-Seq data analysis")
    (description "This package implements a variety of low-level analyses of
single-cell RNA-seq data.  Methods are provided for normalization of
cell-specific biases, assignment of cell cycle phase, and detection of highly
variable and significantly correlated genes.")
    (license license:gpl3)))

(define-public r-sparsearray
  (package
    (name "r-sparsearray")
    (version "1.4.8")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "SparseArray" version))
       (sha256
        (base32 "1ahwv6ih1700h1sccv9wh7hr5bhdif0ji5jbmsprnhjxfgn6xidm"))))
    (properties `((upstream-name . "SparseArray")))
    (build-system r-build-system)
    (propagated-inputs (list r-biocgenerics
                             r-iranges
                             r-matrix
                             r-matrixgenerics
                             r-matrixstats
                             r-s4arrays
                             r-s4vectors
                             r-xvector))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/SparseArray")
    (synopsis
     "Efficient in-memory representation of multidimensional sparse arrays")
    (description
     "The @code{SparseArray} package is an infrastructure package that
provides an array-like container for efficient in-memory representation of
multidimensional sparse data in R.  The package defines the @code{SparseArray}
virtual class and two concrete subclasses: @code{COO_SparseArray} and
@code{SVT_SparseArray}.  Each subclass uses its own internal representation of
the nonzero multidimensional data, the \"COO layout\" and the \"SVT layout\",
respectively.  @code{SVT_SparseArray} objects mimic as much as possible the
behavior of ordinary matrix and array objects in base R.  In particular, they
suppport most of the \"standard matrix and array API\" defined in base R and
in the @code{matrixStats} package from CRAN.")
    (license license:artistic2.0)))

(define-public r-sparsematrixstats
  (package
    (name "r-sparsematrixstats")
    (version "1.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "sparseMatrixStats" version))
       (sha256
        (base32
         "04hk9r6sh83dyxm2j7jdcj0m4g74ln218j21rs71c2gg6mrkmlgf"))))
    (properties
     `((upstream-name . "sparseMatrixStats")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-matrix r-matrixgenerics r-matrixstats r-rcpp))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/sparseMatrixStats/")
    (synopsis "Summary statistics for rows and columns of sparse matrices")
    (description
     "This package provides high performance functions for row and column
operations on sparse matrices.  Currently, the optimizations are limited to
data in the column sparse format.")
    (license license:expat)))

(define-public r-spatialexperiment
  (package
    (name "r-spatialexperiment")
    (version "1.14.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "SpatialExperiment" version))
              (sha256
               (base32
                "0f04azs6wscibcpbmc1m21wjswmgr2b8gvd1n2yavya1li0xxm4r"))))
    (properties `((upstream-name . "SpatialExperiment")))
    (build-system r-build-system)
    (propagated-inputs (list r-biocfilecache
                             r-biocgenerics
                             r-magick
                             r-rjson
                             r-s4vectors
                             r-singlecellexperiment
                             r-summarizedexperiment))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/drighelli/SpatialExperiment")
    (synopsis "S4 class for spatially resolved -omics data")
    (description
     "This package defines an S4 class for storing data from spatial -omics
experiments.  The class extends SingleCellExperiment to support storage and
retrieval of additional information from spot-based and molecule-based
platforms, including spatial coordinates, images, and image metadata.  A
specialized constructor function is included for data from the 10x Genomics
Visium platform.")
    (license license:gpl3)))

(define-public r-delayedmatrixstats
  (package
    (name "r-delayedmatrixstats")
    (version "1.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "DelayedMatrixStats" version))
       (sha256
        (base32
         "1cqf6y0f18469v1jgfvdjnskkc89i3sfg79axxqcwa33mfjj736s"))))
    (properties
     `((upstream-name . "DelayedMatrixStats")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-delayedarray
           r-iranges
           r-matrix
           r-matrixgenerics
           r-s4vectors
           r-sparsematrixstats))
    (native-inputs
     (list r-knitr))
    (home-page "https://github.com/PeteHaitch/DelayedMatrixStats")
    (synopsis "Functions that apply to rows and columns of DelayedMatrix objects")
    (description
     "This package provides a port of the @code{matrixStats} API for use with
@code{DelayedMatrix} objects from the @code{DelayedArray} package.  It
contains high-performing functions operating on rows and columns of
@code{DelayedMatrix} objects, e.g. @code{colMedians}, @code{rowMedians},
@code{colRanks}, @code{rowRanks}, @code{colSds}, and @code{rowSds}.  Functions
are optimized per data type and for subsetted calculations such that both
memory usage and processing time is minimized.")
    (license license:expat)))

(define-public r-mscoreutils
  (package
    (name "r-mscoreutils")
    (version "1.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "MsCoreUtils" version))
       (sha256
        (base32
         "18m6qbj43p6zsrsdp7in15z3hcnqzqcxbk24qvbq7328rrhdk84a"))))
    (properties `((upstream-name . "MsCoreUtils")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-clue r-mass r-rcpp r-s4vectors))
    (native-inputs
     (list r-knitr))
    (home-page "https://github.com/RforMassSpectrometry/MsCoreUtils")
    (synopsis "Core utils for mass spectrometry data")
    (description
     "This package defines low-level functions for mass spectrometry data and
is independent of any high-level data structures.  These functions include
mass spectra processing functions (noise estimation, smoothing, binning),
quantitative aggregation functions (median polish, robust summarisation,
etc.), missing data imputation, data normalisation (quantiles, vsn, etc.) as
well as misc helper functions, that are used across high-level data structure
within the R for Mass Spectrometry packages.")
    (license license:artistic2.0)))

(define-public r-msfeatures
  (package
    (name "r-msfeatures")
    (version "1.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "MsFeatures" version))
       (sha256
        (base32 "1prn45vlacj69388psimy0w05a84y8sdry6678pz611sd0ks1gka"))))
    (properties `((upstream-name . "MsFeatures")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-mscoreutils r-protgenerics r-summarizedexperiment))
    (native-inputs
     (list r-knitr))
    (home-page "https://github.com/RforMassSpectrometry/MsFeatures")
    (synopsis "Functionality for mass spectrometry features")
    (description
     "The MsFeature package defines functionality for Mass Spectrometry
features.  This includes functions to group (LC-MS) features based on some of
their properties, such as retention time (coeluting features), or correlation
of signals across samples.  This package hence can be used to group features, and
its results can be used as an input for the @code{QFeatures} package which
allows aggregating abundance levels of features within each group.  This
package defines concepts and functions for base and common data types,
implementations for more specific data types are expected to be implemented in
the respective packages (such as e.g. @code{xcms}).")
    (license license:artistic2.0)))

(define-public r-biocio
  (package
    (name "r-biocio")
    (version "1.14.0")
    (source
      (origin
        (method url-fetch)
        (uri (bioconductor-uri "BiocIO" version))
        (sha256
          (base32
            "0ppsldpn3vbliimjvnxadn61crck45j581dw4lc5f3w1vy5vs44h"))))
    (properties `((upstream-name . "BiocIO")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocgenerics r-s4vectors))
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/BiocIO")
    (synopsis "Standard input and output for Bioconductor packages")
    (description
      "This package implements `import()` and `export()` standard generics for
importing and exporting biological data formats. `import()` supports
whole-file as well as chunk-wise iterative import.  The `import()` interface
optionally provides a standard mechanism for 'lazy' access via `filter()` (on
row or element-like components of the file resource), `select()` (on
column-like components of the file resource) and `collect()`.  The `import()`
interface optionally provides transparent access to remote (e.g.  via https)
as well as local access.  Developers can register a file extension, e.g.,
`.loom` for dispatch from character-based URIs to specific `import()` /
`export()` methods based on classes representing file types, e.g.,
`LoomFile()`.")
    (license license:artistic2.0)))

(define-public r-msmseda
  (package
    (name "r-msmseda")
    (version "1.42.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "msmsEDA" version))
       (sha256
        (base32
         "1fzw36181rw7z62qggq7j22qhq8zrblpmv8qkqlfg140ywvwv08a"))))
    (properties `((upstream-name . "msmsEDA")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-gplots r-mass r-msnbase r-rcolorbrewer))
    (home-page
     "https://bioconductor.org/packages/msmsEDA")
    (synopsis "Exploratory data analysis of LC-MS/MS data by spectral counts")
    (description
     "Exploratory data analysis to assess the quality of a set of LC-MS/MS
experiments, and visualize de influence of the involved factors.")
    (license license:gpl2)))

(define-public r-msmstests
  (package
    (name "r-msmstests")
    (version "1.42.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "msmsTests" version))
       (sha256
        (base32
         "0fadwss9z4bfgkhrrqv322j8x40l5cl4ib85057h4fn03qdijqhq"))))
    (properties `((upstream-name . "msmsTests")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-edger r-msmseda r-msnbase r-qvalue))
    (home-page
     "https://bioconductor.org/packages/msmsTests")
    (synopsis "Differential LC-MS/MS expression tests")
    (description
     "This package provides statistical tests for label-free LC-MS/MS data
by spectral counts, to discover differentially expressed proteins between two
biological conditions.  Three tests are available: Poisson GLM regression,
quasi-likelihood GLM regression, and the negative binomial of the edgeR
package.  The three models admit blocking factors to control for nuisance
variables.  To assure a good level of reproducibility a post-test filter is
available, where we may set the minimum effect size considered biologicaly
relevant, and the minimum expression of the most abundant condition.")
    (license license:gpl2)))

(define-public r-catalyst
  (package
    (name "r-catalyst")
    (version "1.28.0")
    (source
      (origin
        (method url-fetch)
        (uri (bioconductor-uri "CATALYST" version))
        (sha256
          (base32
            "168yfkqlg6af8ndlw4wzb9adg4lgkgkjbsi4a435ks0n4c9dycb7"))))
    (properties `((upstream-name . "CATALYST")))
    (build-system r-build-system)
    (propagated-inputs
      (list r-circlize
            r-complexheatmap
            r-consensusclusterplus
            r-cowplot
            r-data-table
            r-dplyr
            r-drc
            r-flowcore
            r-flowsom
            r-ggplot2
            r-ggrepel
            r-ggridges
            r-gridextra
            r-matrix
            r-matrixstats
            r-nnls
            r-purrr
            r-rcolorbrewer
            r-reshape2
            r-rtsne
            r-s4vectors
            r-scales
            r-scater
            r-singlecellexperiment
            r-summarizedexperiment))
    (native-inputs
     (list r-knitr))
    (home-page "https://github.com/HelenaLC/CATALYST")
    (synopsis "Cytometry data analysis tools")
    (description
     "This package is @dfn{Cytometry dATa anALYSis Tools} (CATALYST).  Mass
cytometry like @dfn{Cytometry by time of flight} (CyTOF) uses heavy metal
isotopes rather than fluorescent tags as reporters to label antibodies,
thereby substantially decreasing spectral overlap and allowing for examination
of over 50 parameters at the single cell level.  While spectral overlap is
significantly less pronounced in CyTOF than flow cytometry, spillover due to
detection sensitivity, isotopic impurities, and oxide formation can impede
data interpretability.  @code{CATALYST} was designed to provide a pipeline for
preprocessing of cytometry data, including:

@enumerate
@item normalization using bead standards;
@item single-cell deconvolution;
@item bead-based compensation.
@end enumerate
")
    (license license:gpl2+)))

(define-public r-erma
  (package
    (name "r-erma")
    (version "1.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "erma" version))
       (sha256
        (base32
         "0f4y0szdqhg63vaw7f1lmgzb72bhfpz20g9naq84s8bq2xsf8cy6"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotationdbi
           r-biobase
           r-biocgenerics
           r-biocparallel
           r-genomeinfodb
           r-genomicfiles
           r-genomicranges
           r-ggplot2
           r-homo-sapiens
           r-iranges
           r-rtracklayer
           r-s4vectors
           r-shiny
           r-summarizedexperiment))
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/erma")
    (synopsis "Epigenomic road map adventures")
    (description
     "The epigenomics road map describes locations of epigenetic marks in DNA
from a variety of cell types.  Of interest are locations of histone
modifications, sites of DNA methylation, and regions of accessible chromatin.
This package presents a selection of elements of the road map including
metadata and outputs of the ChromImpute procedure applied to ENCODE cell lines
by Ernst and Kellis.")
    (license license:artistic2.0)))

(define-public r-ggbio
  (package
    (name "r-ggbio")
    (version "1.52.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ggbio" version))
       (sha256
        (base32
         "0g6h1h8azy79pzm0g8967f58gp33nl8866b285ybq6501nj8pb1p"))))
    (build-system r-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; See https://github.com/tengfei/ggbio/issues/117
         ;; This fix will be included in the next release.
         (add-after 'unpack 'fix-typo
           (lambda _
             (substitute* "R/GGbio-class.R"
               (("fechable") "fetchable"))
             #t)))))
    (propagated-inputs
     (list r-annotationdbi
           r-annotationfilter
           r-biobase
           r-biocgenerics
           r-biostrings
           r-biovizbase
           r-bsgenome
           r-ensembldb
           r-genomeinfodb
           r-genomicalignments
           r-genomicfeatures
           r-genomicranges
           r-ggally
           r-ggplot2
           r-gridextra
           r-gtable
           r-hmisc
           r-iranges
           r-organismdbi
           r-reshape2
           r-rlang
           r-rsamtools
           r-rtracklayer
           r-s4vectors
           r-scales
           r-summarizedexperiment
           r-variantannotation))
    (native-inputs
     (list r-knitr))
    (home-page "http://www.tengfei.name/ggbio/")
    (synopsis "Visualization tools for genomic data")
    (description
     "The ggbio package extends and specializes the grammar of graphics for
biological data.  The graphics are designed to answer common scientific
questions, in particular those often asked of high throughput genomics data.
All core Bioconductor data structures are supported, where appropriate.  The
package supports detailed views of particular genomic regions, as well as
genome-wide overviews.  Supported overviews include ideograms and grand linear
views.  High-level plots include sequence fragment length, edge-linked
interval to data view, mismatch pileup, and several splicing summaries.")
    (license license:artistic2.0)))

(define-public r-gqtlbase
  (package
    (name "r-gqtlbase")
    (version "1.21.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "gQTLBase" version))
       (sha256
        (base32
         "0nipibm1bk9k70ajbw1f6vjmz0dh7gk21l17q3m54bnawxsggrfh"))))
    (properties `((upstream-name . "gQTLBase")))
    (build-system r-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; This is an upstream bug.
         (add-after 'unpack 'fix-imports
           (lambda _
             (substitute* "NAMESPACE"
               ((".*maxffmode.*") "")
               (("importFrom\\(ff,.*") "import(ff)\n"))
             #t)))))
    (propagated-inputs
     (list r-batchjobs
           r-bbmisc
           r-biocgenerics
           r-bit
           r-doparallel
           r-ff
           r-ffbase
           r-foreach
           r-genomicfiles
           r-genomicranges
           r-rtracklayer
           r-s4vectors
           r-summarizedexperiment))
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/gQTLBase")
    (synopsis "Infrastructure for eQTL, mQTL and similar studies")
    (description
     "The purpose of this package is to simplify the storage and interrogation
of @dfn{quantitative trait loci} (QTL) archives, such as eQTL, mQTL, dsQTL,
and more.")
    (license license:artistic2.0)))

(define-public r-gqtlstats
  (package
    (name "r-gqtlstats")
    (version "1.21.3")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "gQTLstats" version))
       (sha256
        (base32
         "1h78l23idf867djmdk97b02jxgmz4vfr2dai01fp648d0lsx5mkl"))))
    (properties `((upstream-name . "gQTLstats")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotationdbi
           r-batchjobs
           r-bbmisc
           r-beeswarm
           r-biobase
           r-biocgenerics
           r-doparallel
           r-dplyr
           r-erma
           r-ffbase
           r-foreach
           r-genomeinfodb
           r-genomicfeatures
           r-genomicfiles
           r-genomicranges
           r-ggbeeswarm
           r-ggplot2
           r-gqtlbase
           r-hardyweinberg
           r-homo-sapiens
           r-iranges
           r-limma
           r-mgcv
           r-plotly
           r-reshape2
           r-s4vectors
           r-shiny
           r-snpstats
           r-summarizedexperiment
           r-variantannotation))
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/gQTLstats")
    (synopsis "Computationally efficient analysis for eQTL and allied studies")
    (description
     "This package provides tools for the computationally efficient analysis
of @dfn{quantitative trait loci} (QTL) data, including eQTL, mQTL, dsQTL, etc.
The software in this package aims to support refinements and functional
interpretation of members of a collection of association statistics on a
family of feature/genome hypotheses.")
    (license license:artistic2.0)))

(define-public r-gviz
  (package
    (name "r-gviz")
    (version "1.48.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Gviz" version))
       (sha256
        (base32
         "07w37kq6zx99z980if4brwmw25vjgvxd8xj3l1893idbdf70py0p"))))
    (properties `((upstream-name . "Gviz")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotationdbi
           r-biobase
           r-biocgenerics
           r-biomart
           r-biostrings
           r-biovizbase
           r-bsgenome
           r-digest
           r-ensembldb
           r-genomeinfodb
           r-genomicalignments
           r-genomicfeatures
           r-genomicranges
           r-iranges
           r-lattice
           r-latticeextra
           r-matrixstats
           r-rcolorbrewer
           r-rsamtools
           r-rtracklayer
           r-s4vectors
           r-xvector))
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/Gviz")
    (synopsis "Plotting data and annotation information along genomic coordinates")
    (description
     "Genomic data analyses requires integrated visualization of known genomic
information and new experimental data.  Gviz uses the biomaRt and the
rtracklayer packages to perform live annotation queries to Ensembl and UCSC
and translates this to e.g. gene/transcript structures in viewports of the
grid graphics package.  This results in genomic information plotted together
with your data.")
    (license license:artistic2.0)))

(define-public r-gwascat
  (package
    (name "r-gwascat")
    (version "2.36.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "gwascat" version))
       (sha256
        (base32
         "08b70drlasws3h9d8iznaxxp0qf4n1fi943m4yngy1c65h3a30rc"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotationdbi
           r-annotationhub
           r-biocfilecache
           r-biostrings
           r-genomeinfodb
           r-genomicfeatures
           r-genomicranges
           r-iranges
           r-readr
           r-s4vectors
           r-snpstats
           r-variantannotation))
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/gwascat")
    (synopsis "Tools for data in the EMBL-EBI GWAS catalog")
    (description
     "This package provides tools for representing and modeling data in the
EMBL-EBI GWAS catalog.")
    (license license:artistic2.0)))

(define-public r-gwastools
  (package
    (name "r-gwastools")
    (version "1.50.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "GWASTools" version))
              (sha256
               (base32
                "013qv2wac90s1rv992bwzq4diqfq7r18qk7gcifmi22samplpnw4"))))
    (properties `((upstream-name . "GWASTools")))
    (build-system r-build-system)
    (propagated-inputs (list r-biobase
                             r-data-table
                             r-dbi
                             r-dnacopy
                             r-gdsfmt
                             r-gwasexacthw
                             r-lmtest
                             r-logistf
                             r-quantsmooth
                             r-rsqlite
                             r-sandwich
                             r-survival))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/smgogarten/GWASTools")
    (synopsis "Tools for Genome Wide Association Studies")
    (description
     "This package provides classes for storing very large GWAS data sets and
annotation, and functions for GWAS data cleaning and analysis.")
    (license license:artistic2.0)))

(define-public r-kegggraph
  (package
    (name "r-kegggraph")
    (version "1.64.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "KEGGgraph" version))
       (sha256
        (base32 "0lffqbvwmfjbw5ymxr969znvb53hbqfgq1zwffsc768rv3h4c6p7"))))
    (properties `((upstream-name . "KEGGgraph")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-graph r-rcurl r-rgraphviz r-xml))
    (home-page "https://bioconductor.org/packages/KEGGgraph")
    (synopsis "Graph approach to Kegg Pathway database in R and Bioconductor")
    (description
     "@code{r-kegggraph} is an interface between Kegg Pathway database and graph
object as well as a collection of tools to analyze, dissect and visualize these
graphs.  It parses the regularly updated kgml (Kegg XML) files into graph models
maintaining all essential pathway attributes.  The package offers
functionalities including parsing, graph operation, visualization and etc.")
    (license license:gpl2+)))

(define-public r-ldblock
  (package
    (name "r-ldblock")
    (version "1.34.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ldblock" version))
       (sha256
        (base32
         "1l4l35bpz4j1iliy9dj09qq8r4nimd9pcm0gm4g3sb4325z6cd5h"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocgenerics
           r-httr
           r-matrix
           r-rlang))
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/ldblock")
    (synopsis "Data structures for linkage disequilibrium measures in populations")
    (description
     "This package defines data structures for @dfn{linkage
disequilibrium} (LD) measures in populations.  Its purpose is to simplify
handling of existing population-level data for the purpose of flexibly
defining LD blocks.")
    (license license:artistic2.0)))

;; This is a CRAN package, but it depends on r-snpstats, which is a
;; Bioconductor package.
(define-public r-ldheatmap
  (package
    (name "r-ldheatmap")
    (version "1.0-6")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "LDheatmap" version))
       (sha256
        (base32
         "0gr99kys1ahyl8s6cbj6rmh4vwid8kn92lcbjnwh0ahb73m2xjjc"))))
    (properties `((upstream-name . "LDheatmap")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-genetics r-rcpp r-snpstats))
    (home-page "https://stat.sfu.ca/statgen/research/ldheatmap.html")
    (synopsis "Graphical display of pairwise linkage disequilibria between SNPs")
    (description
     "This package provides tools to produce a graphical display, as a heat
map, of measures of pairwise linkage disequilibria between SNPs.  Users may
optionally include the physical locations or genetic map distances of each SNP
on the plot.")
    (license license:gpl3)))

;; This is a CRAN package, but it depends on r-rgraphviz, which is a
;; Bioconductor package.
(define-public r-abn
  (package
    (name "r-abn")
    (version "3.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "abn" version))
       (sha256
        (base32
         "1sixgahjcy82yiiixgxiqbm8jcajqz0m3h0hwwm202iwzi3vyhiv"))))
    (build-system r-build-system)
    (inputs
     (list gsl))
    (propagated-inputs
     (list r-doparallel
           r-foreach
           r-graph
           r-lme4
           r-mclogit
           r-nnet
           r-rcpp
           r-rcpparmadillo
           r-rgraphviz
           r-rjags
           r-stringi))
    (native-inputs
     (list r-knitr))
    (home-page "https://r-bayesian-networks.org/")
    (synopsis "Modelling multivariate data with additive bayesian networks")
    (description
     "Bayesian network analysis is a form of probabilistic graphical models
which derives from empirical data a directed acyclic graph, DAG, describing
the dependency structure between random variables.  An additive Bayesian
network model consists of a form of a DAG where each node comprises a
@dfn{generalized linear model} (GLM).  Additive Bayesian network models are
equivalent to Bayesian multivariate regression using graphical modelling, they
generalises the usual multivariable regression, GLM, to multiple dependent
variables.  This package provides routines to help determine optimal Bayesian
network models for a given data set, where these models are used to identify
statistical dependencies in messy, complex data.")
    (license license:gpl2+)))

;; This is a CRAN package, but it depends on r-rsamtools, which is a
;; Bioconductor package.
(define-public r-spp
  (package
    (name "r-spp")
    (version "1.16.0")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "spp" version))
              (sha256
               (base32
                "08zxxgyp0h6733b08jmml7k4rhfd3mi5dda3jrzid0s184y0z29w"))))
    (build-system r-build-system)
    (inputs
     (list zlib))
    (propagated-inputs
     (list r-bh r-catools r-rcpp r-rsamtools))
    (home-page "https://cran.r-project.org/web/packages/spp/")
    (synopsis "ChIP-Seq processing pipeline")
    (description "This package provides tools for analysis of ChIP-seq and
other functional sequencing data.")
    (license license:gpl2)))

(define-public r-parody
  (package
    (name "r-parody")
    (version "1.62.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "parody" version))
       (sha256
        (base32 "0k2gc8p9dk7j21n6hxdqjna09m6cggx47spkmxkqvwqm97mwqzrp"))))
    (properties `((upstream-name . "parody")))
    (build-system r-build-system)
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/parody")
    (synopsis "Parametric and resistant outlier detection")
    (description
     "The parody package provides routines for univariate and multivariate
outlier detection with a focus on parametric methods, but support for some
methods based on resistant statistics.")
    (license license:artistic2.0)))

(define-public r-pathview
  (package
    (name "r-pathview")
    (version "1.44.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "pathview" version))
       (sha256
        (base32 "1papw23dhhkqmm864dc3xm028xm0ywc6sjcm7xk93f2j7yj65x0v"))))
    (properties `((upstream-name . "pathview")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotationdbi
           r-graph
           r-kegggraph
           r-keggrest
           r-org-hs-eg-db
           r-png
           r-rgraphviz
           r-xml))
    (home-page "https://pathview.uncc.edu/")
    (synopsis "Tool set for pathway based data integration and visualization")
    (description
     "@code{r-pathview} is a tool set for pathway based data integration and
visualization.  It maps and renders a wide variety of biological data on
relevant pathway graphs.  All users need is to supply their data and specify
the target pathway.  This package automatically downloads the pathway graph
data, parses the data file, maps user data to the pathway, and render pathway
graph with the mapped data.  In addition, @code{r-pathview} also seamlessly
integrates with pathway and gene set (enrichment) analysis tools for
large-scale and fully automated analysis.")
    (license license:gpl3+)))

(define-public r-snapcgh
  (package
    (name "r-snapcgh")
    (version "1.72.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "snapCGH" version))
              (sha256
               (base32
                "0knjspxzc3z5dhx0a6kx9rkic85w65l84vy552x5bv8jzxkcwrsj"))))
    (properties `((upstream-name . "snapCGH")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-acgh
           r-cluster
           r-dnacopy
           r-glad
           r-limma
           r-tilingarray))
    (home-page "https://bioconductor.org/packages/snapCGH")
    (synopsis "Segmentation, normalisation and processing of the aCGH data")
    (description
     "This package provides methods for segmenting, normalising and processing
aCGH data.  snapCGH also includes plotting functions for visualising raw and
segmented data for individual and multiple arrays.")
    ;; Expanded from GPL
    (license (list license:gpl2+ license:gpl3+))))

(define-public r-snprelate
  (package
    (name "r-snprelate")
    (version "1.38.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "SNPRelate" version))
              (sha256
               (base32
                "0znk9vvnahkypmkrx82r1ryp41xr675b4ddpgx0nl3708xhncz5f"))))
    (properties `((upstream-name . "SNPRelate")))
    (build-system r-build-system)
    (propagated-inputs (list r-gdsfmt))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/zhengxwen/SNPRelate")
    (synopsis
     "Toolset for relatedness and Principal Component Analysis of SNP data")
    (description
     "Genome-wide association studies (GWAS) are widely used to investigate
the genetic basis of diseases and traits, but they pose many computational
challenges.  The R package SNPRelate provides a binary format for
single-nucleotide polymorphism (SNP) data in GWAS utilizing CoreArray Genomic
Data Structure (GDS) data files.  The GDS format offers the efficient
operations specifically designed for integers with two bits, since a SNP could
occupy only two bits.  SNPRelate is also designed to accelerate two key
computations on SNP data using parallel computing for multi-core symmetric
multiprocessing computer architectures: Principal Component Analysis (PCA) and
relatedness analysis using Identity-By-Descent measures.  The SNP GDS format
is also used by the GWASTools package with the support of S4 classes and
generic functions.  The extended GDS format is implemented in the SeqArray
package to support the storage of single nucleotide variations (SNVs),
insertion/deletion polymorphism (indel) and structural variation calls in
whole-genome and whole-exome variant data.")
    (license license:gpl3)))

(define-public r-snpstats
  (package
    (name "r-snpstats")
    (version "1.54.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "snpStats" version))
       (sha256
        (base32
         "160pl3dy7v89i90x262hxqgipas4pyim600l6m50g3c03vm5in03"))))
    (properties `((upstream-name . "snpStats")))
    (build-system r-build-system)
    (inputs (list zlib))
    (propagated-inputs
     (list r-biocgenerics r-matrix r-survival r-zlibbioc))
    (home-page "https://bioconductor.org/packages/snpStats")
    (synopsis "Methods for SNP association studies")
    (description
     "This package provides classes and statistical methods for large
@dfn{single-nucleotide polymorphism} (SNP) association studies.  This extends
the earlier snpMatrix package, allowing for uncertainty in genotypes.")
    (license license:gpl3)))

(define-public r-chromstar
  (package
    (name "r-chromstar")
    (version "1.30.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "chromstaR" version))
       (sha256
        (base32
         "0swkjq50p3ak0ddyg7pqg37fivdpr3v5jpakn38sk5i5ycn3qf3g"))))
    (properties `((upstream-name . "chromstaR")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-bamsignals
           r-biocgenerics
           r-chromstardata
           r-doparallel
           r-foreach
           r-genomeinfodb
           r-genomicalignments
           r-genomicranges
           r-ggplot2
           r-iranges
           r-mvtnorm
           r-reshape2
           r-rsamtools
           r-s4vectors))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/ataudt/chromstaR")
    (synopsis "Chromatin state analysis for ChIP-Seq data")
    (description
     "This package implements functions for combinatorial and differential
analysis of ChIP-seq data.  It includes uni- and multivariate peak-calling,
export to genome browser viewable files, and functions for enrichment
analyses.")
    (license license:artistic2.0)))

(define-public r-guitar
  (package
    (name "r-guitar")
    (version "2.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Guitar" version))
       (sha256
        (base32
         "1agr3n787dqly09wy274v9dg52vdb0n0z3gdh2zsbklkbmayqxqy"))))
    (properties `((upstream-name . "Guitar")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotationdbi
           r-dplyr
           r-genomicfeatures
           r-genomicranges
           r-ggplot2
           r-knitr
           r-magrittr
           r-rtracklayer))
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/Guitar")
    (synopsis "Visualize genomic features")
    (description
     "This package is designed for visualization of RNA-related genomic
features with respect to the landmarks of RNA transcripts, i.e., transcription
starting site, start codon, stop codon and transcription ending site.")
    (license license:gpl2)))

(define-public r-sushi
  (package
    (name "r-sushi")
    (version "1.34.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "Sushi" version))
              (sha256
               (base32
                "0adswrbzd93rhy3q56ypwkrk6155vd4zxapvznswyjlxp8ha813q"))))
    (properties `((upstream-name . "Sushi")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biomart r-zoo))
    (home-page "https://bioconductor.org/packages/Sushi")
    (synopsis "Tools for visualizing genomics data")
    (description
     "This package provides flexible, quantitative, and integrative genomic
visualizations for publication-quality multi-panel figures.")
    (license license:gpl2+)))

(define-public r-ballgown
  (package
    (name "r-ballgown")
    (version "2.36.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ballgown" version))
       (sha256
        (base32
         "0x3avbqf0ipmb70szf2chhbdg14ba2hwhyyz0wd66c1qwfk6aj9v"))))
    (properties `((upstream-name . "ballgown")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biobase
           r-genomeinfodb
           r-genomicranges
           r-iranges
           r-limma
           r-rcolorbrewer
           r-rtracklayer
           r-s4vectors
           r-sva))
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/ballgown")
    (synopsis "Flexible, isoform-level differential expression analysis")
    (description
     "This package provides tools for statistical analysis of assembled
transcriptomes, including flexible differential expression analysis,
visualization of transcript structures, and matching of assembled transcripts
to annotation.")
    (license license:artistic2.0)))

(define-public r-megadepth
  (package
    (name "r-megadepth")
    (version "1.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "megadepth" version))
       (sha256
        (base32
         "0394x1x81hgibvqb1dbrm8h3y72xfv9xi9fgqghjgnrw6farrkdk"))))
    (properties `((upstream-name . "megadepth")))
    (build-system r-build-system)
    (inputs (list))
    (propagated-inputs
     (list r-cmdfun
           r-dplyr
           r-fs
           r-genomicranges
           r-magrittr
           r-readr
           r-xfun))
    (native-inputs
     (list r-knitr))
    (home-page "https://github.com/LieberInstitute/megadepth")
    (synopsis "BigWig and BAM related utilities")
    (description
     "This package provides an R interface to Megadepth.  It is particularly
useful for computing the coverage of a set of genomic regions across bigWig or
BAM files.  With this package, you can build base-pair coverage matrices for
regions or annotations of your choice from BigWig files.")
    (license license:artistic2.0)))

(define-public r-beclear
  (package
    (name "r-beclear")
    (version "2.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BEclear" version))
       (sha256
        (base32
         "17i93h0q1fwl8wymmlgjsh8hbnaws6baf4jid5k579aap32zb5ha"))))
    (properties `((upstream-name . "BEclear")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-abind
           r-biocparallel
           r-data-table
           r-dixontest
           r-futile-logger
           r-ids
           r-matrix
           r-rcpp
           r-rdpack))
    (native-inputs
     (list r-knitr))
    (home-page "https://github.com/uds-helms/BEclear")
    (synopsis "Correction of batch effects in DNA methylation data")
    (description
     "This package provides functions to detect and correct for batch effects
in DNA methylation data.  The core function is based on latent factor models
and can also be used to predict missing values in any other matrix containing
real numbers.")
    (license license:gpl3)))

(define-public r-bgeecall
  (package
    (name "r-bgeecall")
    (version "1.18.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BgeeCall" version))
       (sha256
        (base32
         "1ipypkyfqx6mahiwl0gzdsi7z9j9cm5rgfw4ybgh6blfq8kg9qyk"))))
    (properties `((upstream-name . "BgeeCall")))
    (build-system r-build-system)
    (propagated-inputs
     (list kallisto
           r-biomart
           r-biostrings
           r-data-table
           r-dplyr
           r-genomicfeatures
           r-jsonlite
           r-rhdf5
           r-rslurm
           r-rtracklayer
           r-sjmisc
           r-tximport))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/BgeeDB/BgeeCall")
    (synopsis "RNA-Seq present/absent gene expression calls generation")
    (description
     "BgeeCall allows generating present/absent gene expression calls without
using an arbitrary cutoff like TPM<1.  Calls are generated based on reference
intergenic sequences.  These sequences are generated based on expression of
all RNA-Seq libraries of each species integrated in Bgee.")
    (license license:gpl3)))

(define-public r-bgeedb
  (package
    (name "r-bgeedb")
    (version "2.30.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BgeeDB" version))
       (sha256
        (base32
         "1jbb93inv7z0i8b1jr6ax98gqkqn73m86hj61irgyysk7hn4bbg6"))))
    (properties `((upstream-name . "BgeeDB")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biobase
           r-curl
           r-data-table
           r-digest
           r-dplyr
           r-graph
           r-r-utils
           r-rcurl
           r-rsqlite
           r-tidyr
           r-topgo))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/BgeeDB/BgeeDB_R")
    (synopsis "Annotation and gene expression data retrieval from Bgee database")
    (description
     "This package provides a package for the annotation and gene expression
data download from Bgee database, and TopAnat analysis: GO-like enrichment of
anatomical terms, mapped to genes by expression patterns.")
    (license license:gpl3)))

(define-public r-biobtreer
  (package
    (name "r-biobtreer")
    (version "1.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "biobtreeR" version))
       (sha256
        (base32
         "0660rc8b4vkic4dbf64wkky68f0hk92ip52x7gihvv203cj9fin4"))))
    (properties `((upstream-name . "biobtreeR")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-httpuv r-httr r-jsonlite r-stringi))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/tamerh/biobtreeR")
    (synopsis "Use biobtree tool from R")
    (description
     "The biobtreeR package provides an interface to biobtree, a tool which
covers large sets of bioinformatics datasets and allows search and chain
mappings functionalities.")
    (license license:expat)))

(define-public r-minet
  (package
    (name "r-minet")
    (version "3.62.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "minet" version))
       (sha256
        (base32
         "1627gqqk8v3sx5kgm7z3wkimbz7dpf37ljrmvwbd4z6ag2yiirbj"))))
    (properties `((upstream-name . "minet")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-infotheo))
    (home-page "http://minet.meyerp.com")
    (synopsis "Mutual information networks")
    (description
     "This package implements various algorithms for inferring mutual
information networks from data.")
    (license license:artistic2.0)))

(define-public r-genetclassifier
  (package
    (name "r-genetclassifier")
    (version "1.44.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "geNetClassifier" version))
       (sha256
        (base32
         "0jfczma8sh9dza105v306xr371f8mndndm6kw3ydgqlxd8zk3a6l"))))
    (properties
     `((upstream-name . "geNetClassifier")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biobase r-e1071 r-ebarrays r-minet))
    (home-page "https://www.cicancer.org")
    (synopsis "Classify diseases and build gene networks using expression profiles")
    (description
     "This is a comprehensive package to automatically train and validate a
multi-class SVM classifier based on gene expression data.  It provides
transparent selection of gene markers, their coexpression networks, and an
interface to query the classifier.")
    (license license:gpl2+)))

(define-public r-dir-expiry
  (package
    (name "r-dir-expiry")
    (version "1.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "dir.expiry" version))
       (sha256
        (base32
         "03958xwwywsl5ql6ss1rbkklk49ksgvbp4nqcmmqhn1hiwwn8rvi"))))
    (properties `((upstream-name . "dir.expiry")))
    (build-system r-build-system)
    (propagated-inputs (list r-filelock))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/dir.expiry")
    (synopsis "Managing expiration for cache directories")
    (description
     "This package implements an expiration system for access to versioned
directories.  Directories that have not been accessed by a registered function
within a certain time frame are deleted.  This aims to reduce disk usage by
eliminating obsolete caches generated by old versions of packages.")
    (license license:gpl3)))

(define-public r-basic4cseq
  (package
    (name "r-basic4cseq")
    (version "1.40.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Basic4Cseq" version))
       (sha256
        (base32 "0ipxhdsqa8q49d2i17kvwyn6w2rpyl9jmq9bhch7cjawykjla5kf"))))
    (properties `((upstream-name . "Basic4Cseq")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biostrings
           r-bsgenome-ecoli-ncbi-20080805
           r-catools
           r-genomicalignments
           r-genomicranges
           r-rcircos))
    (home-page "https://bioconductor.org/packages/Basic4Cseq")
    (synopsis "Analyzing 4C-seq data")
    (description
     "Basic4Cseq is an R package for basic filtering, analysis and subsequent
visualization of @acronym{4C-seq, circular chromosome conformation capture
sequencing} data.  Virtual fragment libraries can be created for any BSGenome
package, and filter functions for both reads and fragments and basic quality
controls are included.  Fragment data in the vicinity of the experiment's
viewpoint can be visualized as a coverage plot based on a running median
approach and a multi-scale contact profile.")
    (license license:lgpl3)))

(define-public r-basics
  (package
    (name "r-basics")
    (version "2.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BASiCS" version))
       (sha256
        (base32 "0rcc8na0nravf2687bcvbaklwa0z8b43lvdy0nf9xvfrfpr1zjnw"))))
    (properties `((upstream-name . "BASiCS")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-assertthat
           r-biobase
           r-biocgenerics
           r-biocparallel
           r-coda
           r-cowplot
           r-ggextra
           r-ggplot2
           r-hexbin
           r-mass
           r-matrix
           r-matrixstats
           r-posterior
           r-rcpp
           r-rcpparmadillo
           r-reshape2
           r-s4vectors
           r-scran
           r-scuttle
           r-singlecellexperiment
           r-summarizedexperiment
           r-viridis))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/catavallejos/BASiCS")
    (synopsis "Bayesian analysis of single-cell sequencing data")
    (description
     "@acronym{BASiCS, Bayesian analysis of single-cell sequencing data} is an
integrated Bayesian hierarchical model to perform statistical analyses of
single-cell RNA sequencing datasets in the context of supervised experiments
(where the groups of cells of interest are known a priori.  BASiCS performs
built-in data normalisation (global scaling) and technical noise quantification
(based on spike-in genes).  BASiCS provides an intuitive detection criterion
for highly (or lowly) variable genes within a single group of cells.
Additionally, BASiCS can compare gene expression patterns between two or more
pre-specified groups of cells.")
    (license license:gpl3)))

(define-public r-basicstarrseq
  (package
    (name "r-basicstarrseq")
    (version "1.32.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BasicSTARRseq" version))
       (sha256
        (base32 "1w69qc81pj3c4gq158ag59wbpm2aaiqmnnb79m0h7dlsnf964cjv"))))
    (properties `((upstream-name . "BasicSTARRseq")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-genomeinfodb
           r-genomicalignments
           r-genomicranges
           r-iranges
           r-s4vectors))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/BasicSTARRseq")
    (synopsis "Basic peak calling on STARR-seq data")
    (description
     "This package implements a method that aims to identify enhancers on
large scale.  The STARR-seq data consists of two sequencing datasets of the
same targets in a specifc genome.  The input sequences show which regions
where tested for enhancers.  Significant enriched peaks i.e. a lot more
sequences in one region than in the input where enhancers in the genomic DNA
are, can be identified.  So the approach pursued is to call peak every region
in which there is a lot more
(significant in a binomial model) STARR-seq signal than input signal and
propose an enhancer at that very same position.  Enhancers then are called
weak or strong dependent of there degree of enrichment in comparison to
input.")
    (license license:lgpl3)))

(define-public r-basilisk-utils
  (package
    (name "r-basilisk-utils")
    (version "1.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "basilisk.utils" version))
       (sha256
        (base32
         "04amc13h4ghy1zr9wkdl42wgicxz5s2iwsspxvvi18acm06nzgfh"))))
    (properties
     `((upstream-name . "basilisk.utils")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-dir-expiry))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/basilisk.utils")
    (synopsis "Basilisk installation utilities")
    (description
     "This package implements utilities for installation of the basilisk
package, primarily for creation of the underlying Conda instance.")
    (license license:gpl3)))

(define-public r-basilisk
  (package
    (name "r-basilisk")
    (version "1.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "basilisk" version))
       (sha256
        (base32
         "1jhaar524z4ly8d4c7nyjn094m4sgz4vhh8qljd2yvs30xxs0jin"))))
    (properties `((upstream-name . "basilisk")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-basilisk-utils r-dir-expiry r-reticulate))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/basilisk")
    (synopsis "Freeze Python dependencies inside Bioconductor packages")
    (description
     "This package installs a self-contained Conda instance that is managed by
the R/Bioconductor installation machinery.  This aims to provide a consistent
Python environment that can be used reliably by Bioconductor packages.
Functions are also provided to enable smooth interoperability of multiple
Python environments in a single R session.")
    (license license:gpl3)))

(define-public r-bayesknockdown
  (package
    (name "r-bayesknockdown")
    (version "1.30.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BayesKnockdown" version))
       (sha256
        (base32 "05plmh36s00jh4ckcgvi2s9ypa8mxlsy23mzysazbfsr022x3vqa"))))
    (properties `((upstream-name . "BayesKnockdown")))
    (build-system r-build-system)
    (propagated-inputs (list r-biobase))
    (home-page "https://bioconductor.org/packages/BayesKnockdown")
    (synopsis "Posterior probabilities for edges from knockdown data")
    (description
     "This package provides a simple, fast Bayesian method for computing
posterior probabilities for relationships between a single predictor variable
and multiple potential outcome variables, incorporating prior probabilities of
relationships.  In the context of knockdown experiments, the predictor
variable is the knocked-down gene, while the other genes are potential
targets.  It can also be used for differential expression/2-class data.")
    (license license:gpl3)))

(define-public r-bayesspace
  (package
    (name "r-bayesspace")
    (version "1.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BayesSpace" version))
       (sha256
        (base32 "0q1haqsyc61yxz1n197sv3s6csjpns4fi288kf58nnf5jgdwnyx5"))))
    (properties `((upstream-name . "BayesSpace")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-assertthat
           r-biocfilecache
           r-biocsingular
           r-coda
           r-dirichletreg
           r-ggplot2
           r-matrix
           r-mclust
           r-purrr
           r-rcpp
           r-rcpparmadillo
           r-rcppdist
           r-rcppprogress
           r-rcurl
           r-rhdf5
           r-s4vectors
           r-scales
           r-scater
           r-scran
           r-singlecellexperiment
           r-summarizedexperiment
           r-xgboost))
    (native-inputs (list r-knitr))
    (home-page "https://www.ezstatconsulting.com/BayesSpace/")
    (synopsis "Clustering and resolution enhancement of spatial transcriptomes")
    (description
     "This package provides tools for clustering and enhancing the resolution
of spatial gene expression experiments.  BayesSpace clusters a low-dimensional
representation of the gene expression matrix, incorporating a spatial prior to
encourage neighboring spots to cluster together.  The method can enhance the
resolution of the low-dimensional representation into \"sub-spots\", for which
features such as gene expression or cell type composition can be imputed.")
    (license license:expat)))

(define-public r-baynorm
  (package
    (name "r-baynorm")
    (version "1.22.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "bayNorm" version))
       (sha256
        (base32 "0b05446vcqxr7smxyhmvl6rxlckzzgqhf5v1059657kihhscqr7s"))))
    (properties `((upstream-name . "bayNorm")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-bb
           r-biocparallel
           r-dosnow
           r-fitdistrplus
           r-foreach
           r-iterators
           r-locfit
           r-mass
           r-matrix
           r-rcpp
           r-rcpparmadillo
           r-rcppprogress
           r-singlecellexperiment
           r-summarizedexperiment))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/WT215/bayNorm")
    (synopsis "Single-cell RNA sequencing data normalization")
    (description
     "The bayNorm package is used for normalizing single-cell RNA-seq data.
The main function is @code{bayNorm}, which is a wrapper function for gene
specific prior parameter estimation and normalization.  The input is a matrix
of scRNA-seq data with rows different genes and coloums different cells.  The
output is either point estimates from posterior (2D array) or samples from
posterior (3D array).")
    (license license:gpl2+)))

(define-public r-bbcanalyzer
  (package
    (name "r-bbcanalyzer")
    (version "1.34.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BBCAnalyzer" version))
       (sha256
        (base32 "16zm6fix5f5277dkdaxyxl1q1n52960ma3vw8001a5bjxhic58rn"))))
    (properties `((upstream-name . "BBCAnalyzer")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biostrings
           r-genomicranges
           r-iranges
           r-rsamtools
           r-summarizedexperiment
           r-variantannotation))
    (home-page "https://bioconductor.org/packages/BBCAnalyzer")
    (synopsis "Visualizing base counts")
    (description
     "BBCAnalyzer is a package for visualizing the relative or absolute number
of bases, deletions and insertions at defined positions in sequence alignment
data available as bam files in comparison to the reference bases.  Markers for
the relative base frequencies, the mean quality of the detected bases, known
mutations or polymorphisms and variants called in the data may additionally be
included in the plots.")
    (license license:lgpl3)))

(define-public r-bcrank
  (package
    (name "r-bcrank")
    (version "1.66.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BCRANK" version))
       (sha256
        (base32 "0ydajrbggvqakdf01pq5mqi1v3slz00irpbx4s9fmxvci9ganwp7"))))
    (properties `((upstream-name . "BCRANK")))
    (build-system r-build-system)
    (propagated-inputs (list r-biostrings))
    (home-page "https://bioconductor.org/packages/BCRANK")
    (synopsis "Predicting binding site consensus from ranked DNA sequences")
    (description
     "This package provides functions and classes for de novo prediction of
transcription factor binding consensus by heuristic search.")
    (license license:gpl2)))

(define-public r-biocthis
  (package
    (name "r-biocthis")
    (version "1.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "biocthis" version))
       (sha256
        (base32
         "1f09vw5qjxyirz5xbvnnjlwxg7z0i0y8yry49d1gyx0spyhzl7sp"))))
    (properties `((upstream-name . "biocthis")))
    (build-system r-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'set-HOME
           (lambda _ (setenv "HOME" "/tmp"))))))
    (propagated-inputs
     (list r-biocmanager
           r-fs
           r-glue
           r-rlang
           r-styler
           r-usethis))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/lcolladotor/biocthis")
    (synopsis "Automate package and project setup for Bioconductor packages")
    (description
     "This package expands the @code{usethis} package with the goal of helping
automate the process of creating R packages for Bioconductor or making them
Bioconductor-friendly.")
    (license license:artistic2.0)))

(define-public r-biocdockermanager
  (package
    (name "r-biocdockermanager")
    (version "1.11.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BiocDockerManager" version))
       (sha256
        (base32
         "0w393f14i253pnk0jzf2ci4g5cnxshwdjmix2r8arlnadh7spjyk"))))
    (properties
     `((upstream-name . "BiocDockerManager")))
    (build-system r-build-system)
    (propagated-inputs
     (list docker
           r-dplyr
           r-httr
           r-memoise
           r-readr
           r-whisker))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/BiocDockerManager")
    (synopsis "Access and manage Bioconductor Docker images")
    (description
     "This package works analogous to BiocManager but for Docker images.  Use
the BiocDockerManager package to install and manage Docker images provided by
the Bioconductor project.")
    (license license:artistic2.0)))

(define-public r-biodb
  (package
    (name "r-biodb")
    (version "1.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "biodb" version))
       (sha256
        (base32
         "0zkd6505q0myc5l9d69i1fvfhcrjqxkv208p84zr5p7xi8jmg3dw"))))
    (properties `((upstream-name . "biodb")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocfilecache
           r-chk
           r-git2r
           r-jsonlite
           r-lgr
           r-lifecycle
           r-openssl
           r-plyr
           r-progress
           r-r6
           r-rappdirs
           r-rcpp
           r-rcurl
           r-rsqlite
           r-stringr
           r-testthat
           r-withr
           r-xml
           r-yaml))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/biodb")
    (synopsis "Library for connecting to chemical and biological databases")
    (description
     "The biodb package provides access to standard remote chemical and
biological databases (ChEBI, KEGG, HMDB, ...), as well as to in-house local
database files (CSV, SQLite), with easy retrieval of entries, access to web
services, search of compounds by mass and/or name, and mass spectra matching
for LCMS and MSMS.  Its architecture as a development framework facilitates
the development of new database connectors for local projects or inside
separate published packages.")
    (license license:agpl3+)))

(define-public r-biomformat
  (package
    (name "r-biomformat")
    (version "1.32.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "biomformat" version))
       (sha256
        (base32
         "0jys0nmp833ax96vfdh1adikpyapr5gkmldx5kvv5bqiqc8aipzb"))))
    (properties `((upstream-name . "biomformat")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-jsonlite r-matrix r-plyr r-rhdf5))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/joey711/biomformat/")
    (synopsis "Interface package for the BIOM file format")
    (description
     "This is an R package for interfacing with the BIOM format.  This package
includes basic tools for reading biom-format files, accessing and subsetting
data tables from a biom object (which is more complex than a single table), as
well as limited support for writing a biom-object back to a biom-format file.
The design of this API is intended to match the Python API and other tools
included with the biom-format project, but with a decidedly \"R flavor\" that
should be familiar to R users.  This includes S4 classes and methods, as well
as extensions of common core functions/methods.")
    (license license:gpl2)))

(define-public r-mvcclass
  (package
    (name "r-mvcclass")
    (version "1.78.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "MVCClass" version))
       (sha256
        (base32
         "1wi6acw51q48v149l72b2pp0iyjrwq1bbmr53y3m609xfadg4qim"))))
    (properties `((upstream-name . "MVCClass")))
    (build-system r-build-system)
    (home-page "https://bioconductor.org/packages/MVCClass")
    (synopsis "Model-View-Controller (MVC) classes")
    (description
     "This package contains classes used in model-view-controller (MVC)
design.")
    (license license:lgpl2.1+)))

(define-public r-biomvcclass
  (package
    (name "r-biomvcclass")
    (version "1.72.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BioMVCClass" version))
       (sha256
        (base32
         "0dfdksk3nc6pzw21sbd4cdqr05489qh4984wv0f906hyb52waxk2"))))
    (properties `((upstream-name . "BioMVCClass")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biobase r-graph r-mvcclass r-rgraphviz))
    (home-page "https://bioconductor.org/packages/BioMVCClass")
    (synopsis "Model-View-Controller (MVC) classes that use Biobase")
    (description
     "This package contains classes used in model-view-controller (MVC)
design.")
    (license license:lgpl2.1+)))

(define-public r-biomvrcns
  (package
    (name "r-biomvrcns")
    (version "1.44.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "biomvRCNS" version))
       (sha256
        (base32
         "1wk0qagx9s2lxykc1fp7l35kl6q17lqy6x4fl2c57v3wa11f47mi"))))
    (properties `((upstream-name . "biomvRCNS")))
    (build-system r-build-system)
    (arguments
     (list
      #:phases
      '(modify-phases %standard-phases
         (add-after 'unpack 'patch-constants
           (lambda _
             (substitute* "src/biomvRCNS.c"
               (("DOUBLE_XMIN") "DBL_MIN")
               (("DOUBLE_XMAX") "DBL_MAX")))))))
    (propagated-inputs
     (list r-genomicranges r-gviz r-iranges r-mvtnorm))
    (home-page "https://bioconductor.org/packages/biomvRCNS")
    (synopsis "Copy number study and segmentation for multivariate biological data")
    (description
     "In this package, a @dfn{Hidden Semi Markov Model} (HSMM) and one
homogeneous segmentation model are designed and implemented for segmentation
genomic data, with the aim of assisting in transcripts detection using high
throughput technology like RNA-seq or tiling array, and copy number analysis
using aCGH or sequencing.")
    (license license:gpl2+)))

(define-public r-bionero
  (package
    (name "r-bionero")
    (version "1.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BioNERO" version))
       (sha256
        (base32
         "02jqlpd31mhxbvksj7ny34f6dv8skkkr76lkiaj5gm078kfr0zx5"))))
    (properties `((upstream-name . "BioNERO")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocparallel
           r-complexheatmap
           r-dynamictreecut
           r-genie3
           r-ggdendro
           r-ggnetwork
           r-ggplot2
           r-ggrepel
           r-igraph
           r-intergraph
           r-matrixstats
           r-minet
           r-netrep
           r-patchwork
           r-rcolorbrewer
           r-reshape2
           r-rlang
           r-summarizedexperiment
           r-sva
           r-wgcna))
    (native-inputs
     (list r-knitr))
    (home-page "https://github.com/almeidasilvaf/BioNERO")
    (synopsis "Biological network reconstruction omnibus")
    (description
     "BioNERO aims to integrate all aspects of biological network inference in
a single package, including data preprocessing, exploratory analyses, network
inference, and analyses for biological interpretations.  BioNERO can be used
to infer gene coexpression networks (GCNs) and gene regulatory networks (GRNs)
from gene expression data.  Additionally, it can be used to explore
topological properties of protein-protein interaction (PPI) networks.  GCN
inference relies on the popular WGCNA algorithm.  GRN inference is based on
the \"wisdom of the crowds\" principle, which consists in inferring GRNs with
multiple algorithms (here, CLR, GENIE3 and ARACNE) and calculating the average
rank for each interaction pair.  As all steps of network analyses are included
in this package, BioNERO makes users avoid having to learn the syntaxes of
several packages and how to communicate between them.  Finally, users can also
identify consensus modules across independent expression sets and calculate
intra and interspecies module preservation statistics between different
networks.")
    (license license:gpl3)))

(define-public r-bionet
  (package
    (name "r-bionet")
    (version "1.64.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BioNet" version))
       (sha256
        (base32
         "0lpqvm0cx7pnb99jh7na49x10w2v04xydiwxn9l41ngxysp8v0h6"))))
    (properties `((upstream-name . "BioNet")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotationdbi r-biobase r-graph r-igraph r-rbgl))
    (home-page "http://bioconductor.org/packages/release/bioc/html/BioNet.html")
    (synopsis "Functional analysis of biological networks")
    (description
     "This package provides functions for the integrated analysis of
protein-protein interaction networks and the detection of functional modules.
Different datasets can be integrated into the network by assigning p-values of
statistical tests to the nodes of the network.  E.g. p-values obtained from
the differential expression of the genes from an Affymetrix array are assigned
to the nodes of the network.  By fitting a beta-uniform mixture model and
calculating scores from the p-values, overall scores of network regions can be
calculated and an integer linear programming algorithm identifies the maximum
scoring subnetwork.")
    (license license:gpl2+)))

(define-public r-bionetstat
  (package
    (name "r-bionetstat")
    (version "1.22.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BioNetStat" version))
       (sha256
        (base32
         "1rbb36v64b9xbk2dsf6wsyrhwwbkysrj8fp1g22y3gisdk170sg8"))
       (snippet
        '(delete-file "inst/datatables/js/jquery.dataTables.min.js"))))
    (properties `((upstream-name . "BioNetStat")))
    (build-system r-build-system)
    (arguments
     (list
      #:phases
      '(modify-phases %standard-phases
         (add-after 'unpack 'process-javascript
           (lambda _
             (with-directory-excursion "inst/datatables/js/"
               (invoke "esbuild"
                       "jquery.dataTables.js"
                       "--minify"
                       "--outfile=jquery.dataTables.min.js")))))))
    (propagated-inputs
     (list r-biocparallel
           r-dt
           r-ggplot2
           r-hmisc
           r-igraph
           r-knitr
           r-markdown
           r-pathview
           r-pheatmap
           r-plyr
           r-psych
           r-rcolorbrewer
           r-rjsonio
           r-rmarkdown
           r-shiny
           r-shinybs
           r-whisker
           r-yaml))
    (native-inputs
     (list esbuild r-knitr r-rmarkdown))
    (home-page "https://github.com/jardimViniciusC/BioNetStat")
    (synopsis "Biological network analysis")
    (description
     "This package provides a package to perform differential network
analysis, differential node analysis (differential coexpression analysis),
network and metabolic pathways view.")
    (license license:gpl3+)))

(define-public r-bioqc
  (package
    (name "r-bioqc")
    (version "1.32.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BioQC" version))
       (sha256
        (base32
         "14yrqf5wl9r596zvlpf4y48nykg0gwzlxfgi3kdx0i5pfgvi43vk"))))
    (properties `((upstream-name . "BioQC")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biobase r-edger r-rcpp))
    (native-inputs
     (list r-knitr))
    (home-page "https://accio.github.io/BioQC/")
    (synopsis "Detect tissue heterogeneity in expression profiles with gene sets")
    (description
     "BioQC performs quality control of high-throughput expression data based
on tissue gene signatures.  It can detect tissue heterogeneity in gene
expression data.  The core algorithm is a Wilcoxon-Mann-Whitney test that is
optimised for high performance.")
    (license license:gpl3+)))

(define-public r-biotip
  (package
    (name "r-biotip")
    (version "1.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BioTIP" version))
       (sha256
        (base32
         "1bbjfv3bi20xlk8h7r5ipav6mfs27lmj0zjb5mk1gmp071dmnk0y"))))
    (properties `((upstream-name . "BioTIP")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-cluster
           r-genomicranges
           r-igraph
           r-mass
           r-psych
           r-scran
           r-stringr))
    (native-inputs
     (list r-knitr))
    (home-page "https://github.com/xyang2uchicago/BioTIP")
    (synopsis "R package for characterization of biological tipping-point")
    (description
     "This package adopts tipping-point theory to transcriptome profiles to
help unravel disease regulatory trajectory.")
    (license license:gpl2)))

(define-public r-biotmle
  (package
    (name "r-biotmle")
    (version "1.28.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "biotmle" version))
       (sha256
        (base32
         "06kixhzkrzxyic44kdmqrv19v2c4a7sri8cbhbvww8km4n5l39sf"))))
    (properties `((upstream-name . "biotmle")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-assertthat
           r-biocgenerics
           r-biocparallel
           r-dplyr
           r-drtmle
           r-ggplot2
           r-ggsci
           r-limma
           r-s4vectors
           r-summarizedexperiment
           r-superheat
           r-tibble))
    (native-inputs
     (list r-knitr))
    (home-page "https://code.nimahejazi.org/biotmle/")
    (synopsis "Targeted learning with moderated statistics for biomarker discovery")
    (description
     "This package provides tools for differential expression biomarker
discovery based on microarray and next-generation sequencing data that
leverage efficient semiparametric estimators of the average treatment effect
for variable importance analysis.  Estimation and inference of the (marginal)
average treatment effects of potential biomarkers are computed by targeted
minimum loss-based estimation, with joint, stable inference constructed across
all biomarkers using a generalization of moderated statistics for use with the
estimated efficient influence function.  The procedure accommodates the use of
ensemble machine learning for the estimation of nuisance functions.")
    (license license:expat)))

(define-public r-bsseq
  (package
    (name "r-bsseq")
    (version "1.40.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "bsseq" version))
       (sha256
        (base32
         "1b7a9ryl4lz6qy2pni3k9bk6hdf147s3m7fx0icg6fkgi0ymssyx"))))
    (properties `((upstream-name . "bsseq")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-beachmat
           r-biobase
           r-biocgenerics
           r-biocparallel
           r-biostrings
           r-bsgenome
           r-data-table
           r-delayedarray
           r-delayedmatrixstats
           r-genomeinfodb
           r-genomicranges
           r-gtools
           r-hdf5array
           r-iranges
           r-limma
           r-locfit
           r-permute
           r-r-utils
           r-rcpp
           r-rhdf5
           r-s4vectors
           r-scales
           r-summarizedexperiment))
    (native-inputs
     (list r-knitr))
    (home-page "https://github.com/hansenlab/bsseq")
    (synopsis "Analyze, manage and store bisulfite sequencing data")
    (description
     "This package provides a collection of tools for analyzing and
visualizing bisulfite sequencing data.")
    (license license:artistic2.0)))

(define-public r-dada2
  (package
    (name "r-dada2")
    (version "1.32.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "dada2" version))
              (sha256
               (base32
                "1ac0nb5bcmzmqpi3vjdmns6xqq3ar61dks6qb3c0k48c78w88b7b"))))
    (properties `((upstream-name . "dada2")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocgenerics
           r-biostrings
           r-ggplot2
           r-iranges
           r-rcpp
           r-rcppparallel
           r-reshape2
           r-shortread
           r-xvector))
    (native-inputs (list r-knitr))
    (home-page "https://benjjneb.github.io/dada2/")
    (synopsis
     "Accurate, high-resolution sample inference from amplicon sequencing data")
    (description
     "The dada2 package infers exact @dfn{amplicon sequence variants} (ASVs)
from high-throughput amplicon sequencing data, replacing the coarser and less
accurate OTU clustering approach.  The dada2 pipeline takes as input
demultiplexed fastq files, and outputs the sequence variants and their
sample-wise abundances after removing substitution and chimera errors.
Taxonomic classification is available via a native implementation of the RDP
naive Bayesian classifier, and species-level assignment to 16S rRNA gene
fragments by exact matching.")
    (license license:lgpl2.0)))

(define-public r-dmrseq
  (package
    (name "r-dmrseq")
    (version "1.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "dmrseq" version))
       (sha256
        (base32
         "0p7xpyl1wni17y7qsincpvllaynxwppmyg6hsza51srs4dhr7sf1"))))
    (properties `((upstream-name . "dmrseq")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotationhub
           r-annotatr
           r-biocparallel
           r-bsseq
           r-bumphunter
           r-delayedmatrixstats
           r-genomeinfodb
           r-genomicranges
           r-ggplot2
           r-iranges
           r-locfit
           r-matrixstats
           r-nlme
           r-outliers
           r-rcolorbrewer
           r-rtracklayer
           r-s4vectors))
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/dmrseq")
    (synopsis "Detection and inference of differentially methylated regions")
    (description
     "This package implements an approach for scanning the genome to detect
and perform accurate inference on differentially methylated regions from Whole
Genome Bisulfite Sequencing data.  The method is based on comparing detected
regions to a pooled null distribution, that can be implemented even when as
few as two samples per population are available.  Region-level statistics are
obtained by fitting a @dfn{generalized least squares} (GLS) regression model
with a nested autoregressive correlated error structure for the effect of
interest on transformed methylation proportions.")
    (license license:expat)))

(define-public r-omicade4
  (package
    (name "r-omicade4")
    (version "1.44.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "omicade4" version))
              (sha256
               (base32
                "18gp8qpmnbc5yjqyvvwvmrdybvm6zddc0r8bfglxz5wg4a91h4wk"))))
    (properties `((upstream-name . "omicade4")))
    (build-system r-build-system)
    (propagated-inputs (list r-ade4 r-biobase r-made4))
    (home-page "https://bioconductor.org/packages/omicade4")
    (synopsis "Multiple co-inertia analysis of omics datasets")
    (description
     "This package performes multiple co-inertia analysis of omics datasets.")
    (license license:gpl2)))

(define-public r-omnipathr
  (package
    (name "r-omnipathr")
    (version "3.11.10")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "OmnipathR" version))
       (sha256
        (base32 "1izy0ycrvac0k67dxq8879rbdghkrczd3s2g5v607js7bmfxybjz"))))
    (properties `((upstream-name . "OmnipathR")))
    (build-system r-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'set-HOME
           (lambda _ (setenv "HOME" "/tmp"))))))
    (propagated-inputs
     (list r-checkmate
           r-crayon
           r-curl
           r-digest
           r-dplyr
           r-httr
           r-igraph
           r-jsonlite
           r-later
           r-logger
           r-lubridate
           r-magrittr
           r-progress
           r-purrr
           r-rappdirs
           r-readr
           r-readxl
           r-rlang
           r-rmarkdown
           r-rvest
           r-stringi
           r-stringr
           r-tibble
           r-tidyr
           r-tidyselect
           r-withr
           r-xml2
           r-yaml))
    (native-inputs (list r-knitr))
    (home-page "https://saezlab.github.io/OmnipathR/")
    (synopsis "OmniPath web service client and more")
    (description
     "This package provides a client for the OmniPath web service and many
other resources.  It also includes functions to transform and pretty print
some of the downloaded data, functions to access a number of other resources.
Furthermore, OmnipathR features a close integration with the NicheNet method
for ligand activity prediction from transcriptomics data.")
    (license license:expat)))

(define-public r-biscuiteer
  (package
    (name "r-biscuiteer")
    (version "1.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "biscuiteer" version))
       (sha256
        (base32
         "15bisrcgyiv9ndfvgm0p4igyr4jwsf68ccis1nf1hi9x291k7m6y"))))
    (properties `((upstream-name . "biscuiteer")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biobase
           r-biocgenerics
           r-biocparallel
           r-biscuiteerdata
           r-bsseq
           r-data-table
           r-delayedmatrixstats
           r-dmrseq
           r-genomeinfodb
           r-genomicranges
           r-gtools
           r-hdf5array
           r-homo-sapiens
           r-impute
           r-iranges
           r-matrix
           r-matrixstats
           r-mus-musculus
           r-qdnaseq
           r-qualv
           r-r-utils
           r-readr
           r-rsamtools
           r-rtracklayer
           r-s4vectors
           r-summarizedexperiment
           r-variantannotation))
    (native-inputs
     (list r-knitr))
    (home-page "https://github.com/trichelab/biscuiteer")
    (synopsis "Convenience functions for the Biscuit package")
    (description
     "This package provides a test harness for bsseq loading of Biscuit
output, summarization of WGBS data over defined regions and in mappable
samples, with or without imputation, dropping of mostly-NA rows, age
estimates, etc.")
    (license license:gpl3)))

(define-public r-tcgabiolinks
  (package
    (name "r-tcgabiolinks")
    (version "2.32.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "TCGAbiolinks" version))
       (sha256
        (base32 "1br2dxi5rvl8z5myrnj4dnfh8xgksrqjyy0fsqh3vjspifvis4ih"))))
    (properties `((upstream-name . "TCGAbiolinks")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biomart
           r-data-table
           r-downloader
           r-dplyr
           r-genomicranges
           r-ggplot2
           r-httr
           r-iranges
           r-jsonlite
           r-knitr
           r-plyr
           r-purrr
           r-r-utils
           r-readr
           r-rvest
           r-s4vectors
           r-stringr
           r-summarizedexperiment
           r-tcgabiolinksgui-data
           r-tibble
           r-tidyr
           r-xml
           r-xml2))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/BioinformaticsFMRP/TCGAbiolinks")
    (synopsis "Integrative analysis with GDC data")
    (description
     "The aim of TCGAbiolinks is:

@enumerate
@item facilitate GDC open-access data retrieval;
@item prepare the data using the appropriate pre-processing strategies;
@item provide the means to carry out different standard analyses, and;
@item to easily reproduce earlier research results.
@end enumerate

In more detail, the package provides multiple methods for analysis (e.g.,
differential expression analysis, identifying differentially methylated
regions) and methods for visualization (e.g., survival plots, volcano plots,
starburst plots) in order to easily develop complete analysis pipelines.")
    (license license:gpl3+)))

(define-public r-tricycle
  (package
    (name "r-tricycle")
    (version "1.12.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "tricycle" version))
              (sha256
               (base32
                "11928pcd8xzmll28m0ihi3rasc4w1i0fshn4bh9n2lyigkr5ayi4"))))
    (properties `((upstream-name . "tricycle")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotationdbi
           r-circular
           r-dplyr
           r-genomicranges
           r-ggnewscale
           r-ggplot2
           r-iranges
           r-rcolorbrewer
           r-s4vectors
           r-scater
           r-scattermore
           r-singlecellexperiment
           r-summarizedexperiment))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/hansenlab/tricycle")
    (synopsis "Transferable representation and inference of cell cycle")
    (description
     "The package contains functions to infer and visualize cell cycle process
using Single-cell RNA-Seq data.  It exploits the idea of transfer learning,
projecting new data to the previous learned biologically interpretable space.
The @code{tricycle} provides a pre-learned cell cycle space, which could be
used to infer cell cycle time of human and mouse single cell samples.  In
addition, it also offer functions to visualize cell cycle time on different
embeddings and functions to build new reference.")
    (license license:gpl3)))

(define-public r-tximeta
  (package
    (name "r-tximeta")
    (version "1.22.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "tximeta" version))
       (sha256
        (base32
         "0lmkxl7zlp6z4i32jyy914kiwacnz89jnd2kqma81mq81r389p06"))))
    (properties `((upstream-name . "tximeta")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotationdbi
           r-annotationhub
           r-biocfilecache
           r-biostrings
           r-ensembldb
           r-genomeinfodb
           r-genomicfeatures
           r-genomicranges
           r-iranges
           r-jsonlite
           r-matrix
           r-s4vectors
           r-summarizedexperiment
           r-tibble
           r-txdbmaker
           r-tximport))
    (native-inputs
     (list r-knitr))
    (home-page "https://github.com/mikelove/tximeta")
    (synopsis "Transcript quantification import with automatic metadata")
    (description
     "This package implements transcript quantification import from Salmon and
alevin with automatic attachment of transcript ranges and release information,
and other associated metadata.  De novo transcriptomes can be linked to the
appropriate sources with linkedTxomes and shared for computational
reproducibility.")
    (license license:gpl2)))

(define-public r-phyloseq
  (package
    (name "r-phyloseq")
    (version "1.48.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "phyloseq" version))
       (sha256
        (base32 "0a8q4wcqcxs7933bxg9bg03ps6hnybvcpqmlpz1pjy3affzghcbi"))))
    (properties `((upstream-name . "phyloseq")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-ade4
           r-ape
           r-biobase
           r-biocgenerics
           r-biomformat
           r-biostrings
           r-cluster
           r-data-table
           r-foreach
           r-ggplot2
           r-igraph
           r-multtest
           r-plyr
           r-reshape2
           r-scales
           r-vegan))
    (native-inputs
     (list r-knitr))
    (home-page "https://github.com/joey711/phyloseq")
    (synopsis "Handling and analysis of high-throughput microbiome census data")
    (description
     "Phyloseq provides a set of classes and tools to facilitate the import,
storage, analysis, and graphical display of microbiome census data.")
    (license license:agpl3)))

;;;
;;; Avoid adding new packages to the end of this file. To reduce the chances
;;; of a merge conflict, place them above by existing packages with similar
;;; functionality or similar names.
;;;
