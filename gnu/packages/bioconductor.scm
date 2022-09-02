;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016, 2017, 2018, 2020, 2021 Roel Janssen <roel@gnu.org>
;;; Copyright © 2016 Pjotr Prins <pjotr.guix@thebird.nl>
;;; Copyright © 2016 Ben Woodcroft <donttrustben@gmail.com>
;;; Copyright © 2017, 2022 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017, 2018, 2019, 2020, 2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019, 2020, 2021, 2022 Simon Tournier <zimon.toutoune@gmail.com>
;;; Copyright © 2020 Peter Lo <peterloleungyau@gmail.com>
;;; Copyright © 2020, 2021, 2022 Mădălin Ionel Patrașcu <madalinionel.patrascu@mdc-berlin.de>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2021 Hong Li <hli@mdc-berlin.de>
;;; Copyright © 2021 Tim Howes <timhowes@lavabit.com>
;;; Copyright © 2021 Nicolas Vallet <nls.vallet@gmail.com>
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
  #:use-module (guix git-download)
  #:use-module (guix build-system r)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages cran)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages docker)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages graph)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages image)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages netpbm)
  #:use-module (gnu packages python)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module (srfi srfi-1))


;;; Annotations

(define-public r-org-eck12-eg-db
  (package
    (name "r-org-eck12-eg-db")
    (version "3.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "org.EcK12.eg.db" version 'annotation))
       (sha256
        (base32 "0c4p6jr83k0gm6pvn760yr8xf33wggrfcr6fg7a42a96bcf817gs"))))
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
    (version "3.13.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri
             "org.Bt.eg.db"
             version
             'annotation))
       (sha256
        (base32
         "0pwvwyfah8fhvaxdc8zkp3lp1v4mchhzr84r3hb0jx97icdvhafi"))))
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

(define-public r-reactome-db
  (package
    (name "r-reactome-db")
    (version "1.70.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "reactome.db" version 'annotation))
       (sha256
        (base32
         "05wc4fp0faq6h3kq5rwafnips043as31yq11mrjngfxvf5i10srg"))))
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
    (version "1.4.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "BSgenome.Mmusculus.UCSC.mm10"
                                     version 'annotation))
              (sha256
               (base32
                "12s0nm2na9brjad4rn9l7d3db2aj8qa1xvz0y1k7gk08wayb6bkf"))))
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
    (version "1.2.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "GenomeInfoDbData" version 'annotation))
              (sha256
               (base32
                "0di6nlqpsyqf693k2na65ayqldih563x3zfrczpqc5q2hl5kg35c"))))
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
    (version "3.7.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "GO.db" version 'annotation))
              (sha256
               (base32
                "0i3wcf5h3n0dawzc1hy0kv74f06j80c47n4p3g3fmrcxlhi3jpa5"))))
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
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri
             "IlluminaHumanMethylation450kanno.ilmn12.hg19"
             version 'annotation))
       (sha256
        (base32
         "059vlxsx3p3fcnywwirahsc6mlk813zpqnbv0jsrag6x5bb8z6r4"))))
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

(define-public r-org-ce-eg-db
  (package
    (name "r-org-ce-eg-db")
    (version "3.7.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "org.Ce.eg.db" version 'annotation))
              (sha256
               (base32
                "1w5br1ss4ha8wv4v2saj7cmbjc2jw0dyj2f2y269l078z31wcnaz"))))
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
    (version "3.7.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "org.Dm.eg.db" version 'annotation))
              (sha256
               (base32
                "1pqjrzlyg72bjpy8zsxvaglc7jsv176bnyi87xdajmkvsgxpm7b3"))))
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
    (version "3.7.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "org.Dr.eg.db" version 'annotation))
              (sha256
               (base32
                "1xs5wsbcpy0iwbjyiv7fax57djqc529ai5fk1qfsdcvlja3cpglx"))))
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
    (version "3.14.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "org.Hs.eg.db" version 'annotation))
              (sha256
               (base32
                "0mnddv42ll0sc0zxf7hkgilslykbvfn7xgxg1g8qi57q2dmpwb6j"))))
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
    (version "3.7.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "org.Mm.eg.db" version 'annotation))
              (sha256
               (base32
                "1i3nvrd3wjigf1rmgxq1p5xxc3p8v02h5gwi62s30rkrsyjjfjxx"))))
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
    (version "1.4.4")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "BSgenome.Hsapiens.UCSC.hg38"
                                     version 'annotation))
              (sha256
               (base32 "03xmh6q99nqjxz29m6j0ymxlk22jq0nlvpf4a2yhg3hgnxqkakh2"))))
    (properties
     `((upstream-name . "BSgenome.Hsapiens.UCSC.hg38")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-bsgenome))
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
    (version "3.15.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "TxDb.Hsapiens.UCSC.hg38.knownGene"
                                     version 'annotation))
              (sha256
               (base32 "1y9fqhkk5wgny43bxc0j82afy49vz34rblcmcfmwavngdkpnj879"))))
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
    (version "3.15.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "PFAM.db" version 'annotation))
       (sha256
        (base32 "03vjfb9vx1gxrw1jkq6y4i46qhjj9z2mkdiflglbd6kpfrgnl0z7"))))
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
    (version "1.12.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "adductData" version 'experiment))
              (sha256
               (base32 "02r7p1645vhhf7wn5x0aklmf7l97h6fjb8v9mldim4waccmpyg48"))))
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
   (version "1.24.0")
   (source (origin
            (method url-fetch)
            (uri (bioconductor-uri "AneuFinderData" version 'experiment))
            (sha256
             (base32 "0cncb8km0sc2xh95rgnnm38kys5ml0n8gh8cl6x7ls1xh9sm83f7"))))
   (build-system r-build-system)
   (home-page "https://bioconductor.org/packages/AneuFinderData/")
   (synopsis "Data package for @code{AneuFinder}")
   (description "This package contains whole-genome single cell sequencing data for
demonstration purposes in the @code{AneuFinder} package.")
   (license license:artistic2.0)))

(define-public r-arrmdata
  (package
    (name "r-arrmdata")
    (version "1.32.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "ARRmData" version 'experiment))
              (sha256
               (base32 "1cjzr58426s9z2bwjz3wzlkh52fv0q85cw4gbjnhwakh9sr3dd5p"))))
    (properties
     `((upstream-name . "ARRmData")))
    (build-system r-build-system)
    (home-page "https://www.bioconductor.org/packages/ARRmData/")
    (synopsis "Example dataset for normalization of Illumina 450k methylation data")
    (description
     "This package provides raw beta values from 36 samples across 3 groups
from Illumina 450k methylation arrays.")
    (license license:artistic2.0)))

(define-public r-bladderbatch
  (package
    (name "r-bladderbatch")
    (version "1.34.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "bladderbatch" version
                                     'experiment))
              (sha256
               (base32
                "1dpbaqsqizyi99r0imf5m4lndhhrkyiaqii9bi8rp18fjbjdd72k"))))
    (properties `((upstream-name . "bladderbatch")))
    (build-system r-build-system)
    (propagated-inputs (list r-biobase))
    (home-page "https://bioconductor.org/packages/bladderbatch")
    (synopsis "Bladder gene expression data illustrating batch effects")
    (description
     "This package contains microarray gene expression data on 57 bladder samples from
5 batches.  The data are used as an illustrative example for the sva package.")
    (license license:artistic2.0)))

(define-public r-biscuiteerdata
  (package
    (name "r-biscuiteerdata")
    (version "1.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "biscuiteerData" version 'experiment))
       (sha256
        (base32 "0nda6b8mkv93s513y0xfgxvi7zn8v07jx323ii709rknlncm6qqw"))))
    (properties
     `((upstream-name . "biscuiteerData")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotationhub r-curl r-experimenthub))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/biscuiteerData")
    (synopsis "Data package for Biscuiteer")
    (description
     "This package contains default datasets used by the Bioconductor package
biscuiteer.")
    (license license:gpl3)))

(define-public r-celldex
  (package
    (name "r-celldex")
    (version "1.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "celldex" version 'experiment))
       (sha256
        (base32 "1fjldmhb9yg6yr3aq5ldvc8xwqw71ix4cdlr53xxckgwljjq7x10"))))
    (properties `((upstream-name . "celldex")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotationdbi
           r-annotationhub
           r-delayedarray
           r-delayedmatrixstats
           r-experimenthub
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

(define-public r-chromstardata
  (package
    (name "r-chromstardata")
    (version "1.22.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "chromstaRData" version 'experiment))
       (sha256
        (base32 "1ajwnkibpi01c93nrplxhy6grw8jj5219g4pii4rkan4k5815kv1"))))
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
    (version "1.28.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "CopyhelpeR" version 'experiment))
       (sha256
        (base32 "0klrnxck0q14birnpwzkiwmj77hwdn6gazvdg0lqn9y6j5mbkyx1"))))
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

(define-public r-genelendatabase
  (package
    (name "r-genelendatabase")
    (version "1.32.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "geneLenDataBase" version 'experiment))
       (sha256
        (base32 "0p4rmd3qszsnyn47mfbk96zfa0bhpyyvsh4ma1ligjrsnmkicsaz"))))
    (properties
     `((upstream-name . "geneLenDataBase")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-rtracklayer r-genomicfeatures))
    (home-page "https://bioconductor.org/packages/geneLenDataBase/")
    (synopsis "Lengths of mRNA transcripts for a number of genomes")
    (description
     "This package provides the lengths of mRNA transcripts for a number of
genomes and gene ID formats, largely based on the UCSC table browser.")
    (license license:lgpl2.0+)))

(define-public r-genomationdata
  (package
    (name "r-genomationdata")
    (version "1.28.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "genomationData" version 'experiment))
       (sha256
        (base32 "0ckdgmarndpz6r0y9sd4nmypzjgivj32w2890yl15xmxkx4397fh"))))
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

(define-public r-msdata
  (package
    (name "r-msdata")
    (version "0.36.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "msdata" version 'experiment))
       (sha256
        (base32 "0nqb7d7fa9l15bxy3s9wmy2h79jb6ldwww0hzk5mifabacmzx691"))))
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

(define-public r-pasilla
  (package
    (name "r-pasilla")
    (version "1.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "pasilla" version 'experiment))
       (sha256
        (base32 "1vsxh7mv2krkbdqs5gsgjsxarjbll0bpyk94syrwh56z67n7jyib"))))
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
    (version "1.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "HSMMSingleCell" version 'experiment))
       (sha256
        (base32 "12whx0pl9461xbak5r9zi6ggx5is8sk6mgrbjwlmx3mbr9am116v"))))
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
    (version "1.38.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ALL" version 'experiment))
       (sha256
        (base32 "0410045x327wmfkksshd8yishw4yxij08vn8p65cdj7hb3qy3p0z"))))
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
    (version "1.44.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "affydata" version 'experiment))
       (sha256
        (base32 "1d8ims7hks536v739r5hhfkkzyzqrf398aqal3hzyfm0psv15jbp"))))
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
    (version "2.34.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "gageData" version 'experiment))
       (sha256
        (base32 "00s2aig9r0bvk45brc0shildrgl2z0i0k8xlvqc9h1s274nnslk9"))))
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
    (version "1.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "curatedTCGAData" version 'experiment))
       (sha256
        (base32 "0h3mpwy6lhyn8hfry13sdjgb35gqyi3g26igfjqzshc5wvsniwpr"))))
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
    (version "1.34.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "parathyroidSE" version
                                     'experiment))
              (sha256
               (base32
                "1h33x55c4gbzmh085skqif04wdcvjp2l9fm55qzwws27kwd30c16"))))
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

(define-public r-tcgabiolinksgui-data
  (package
    (name "r-tcgabiolinksgui-data")
    (version "1.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "TCGAbiolinksGUI.data" version 'experiment))
       (sha256
        (base32 "1mb2z59acs1pi1gqvgjyh62wnmbxskc5l0p42gpjajsjip5x1x7g"))))
    (properties `((upstream-name . "TCGAbiolinksGUI.data")))
    (build-system r-build-system)
    (native-inputs (list r-knitr))
    (home-page "https://github.com/BioinformaticsFMRP/TCGAbiolinksGUI.data")
    (synopsis "Data for the TCGAbiolinksGUI package")
    (description "This package provides supporting data for the
TCGAbiolinksGUI package.")
    (license license:gpl3)))


;;; Packages

(define-public r-abarray
  (package
    (name "r-abarray")
    (version "1.64.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "ABarray" version))
              (sha256
               (base32
                "0kjq00i2mb21xyjjs3jy09ps80f11cy37wywzjvmxyjxzbsk4d7r"))))
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
    (version "1.50.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "ABSSeq" version))
              (sha256
               (base32
                "1kwl0gcqwbgblwvpbvqlgnsi91km77j11f0q1f0gd6hhnv38mmlv"))))
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

(define-public r-adam
  (package
    (name "r-adam")
    (version "1.12.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "ADAM" version))
              (sha256
               (base32
                "1cgcjykik9hjrwlvvgaccprcrimgq5kwh9cj6367yk9m574a4gmn"))))
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
    (version "1.12.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "ADAMgui" version))
              (sha256
               (base32
                "0vvd5qdwkfcr7zg7z63x3vvrcg63r6c9p383yvcg2lp8zmx8hsbs"))))
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
    (version "1.6.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "ADImpute" version))
              (sha256
               (base32
                "0885kd8mpmwjpzpx14pi6l3mqcvsixk10vkf5h4sqb7di0nnna4w"))))
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
    (version "1.66.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "adSplit" version))
              (sha256
               (base32
                "1wl2gd0b7krf485clw67cxayp0g9argklkzn8nw1vrkil0vvr4jm"))))
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
    (version "1.14.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "AffiXcan" version))
              (sha256
               (base32
                "0wj9shzmlxpksbxny571xzfcmmqqzjlk1vq4mx1is2r6ma7jkblq"))))
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

(define-public r-affyrnadegradation
  (package
    (name "r-affyrnadegradation")
    (version "1.42.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "AffyRNADegradation" version))
              (sha256
               (base32
                "16akwmpzwxai7ks5bvc1yyb9sx2scv9b9gas5avb0sk5fk0h3nsf"))))
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
    (version "1.44.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "AGDEX" version))
              (sha256
               (base32
                "0c44fw5ajdjc13409rn3lsv0jhlqa2qcak9b1k8hpig486xxzsr9"))))
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
    (version "1.6.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "aggregateBioVar" version))
              (sha256
               (base32
                "0ngg12bgr95m4wm12scmrb55dgy4909c6qrg169l6dkng99v4nx1"))))
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
    (version "3.28.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "agilp" version))
              (sha256
               (base32
                "1pm329y2nfcnx98ggxq0prdd5pxfcl5iylvsjjnhw5lyz1awg1yf"))))
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
    (version "1.12.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "adductomicsR" version))
              (sha256
               (base32
                "0623qf06xgdsyz0in2wnxwvpdw8kj6cnwf8vlqmgp7g0n3w701ys"))))
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
    (version "2.46.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "AgiMicroRna" version))
              (sha256
               (base32
                "0jic89gyphbv7jzlfgm9bh1aq48lp86rq6hr34gsg9z0pa1192xa"))))
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

(define-public r-amountain
  (package
    (name "r-amountain")
    (version "1.22.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "AMOUNTAIN" version))
              (sha256
               (base32
                "0vdfabsrisdd7qq28f5ivd0v8zz49szqn677i5lhwnlaix220c54"))))
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

(define-public r-amaretto
  (package
    (name "r-amaretto")
    (version "1.12.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "AMARETTO" version))
              (sha256
               (base32
                "111dk19b9910icksyr592cvhc5gwvgknr5q4887j9yxbajd7hcmx"))))
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
    (version "2.20.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "Anaquin" version))
              (sha256
               (base32
                "1jgpnls2djl1yzvnk64qc83mljmlci7wflwkza3wr0sv6r47b0dd"))))
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

(define-public r-aneufinder
  (package
    (name "r-aneufinder")
    (version "1.24.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "AneuFinder" version))
              (sha256
               (base32
                "1acsp987jv2x4qwbgy3y7ff4r2qz7680b0nbr37m4lmncqfgh8yl"))))
    (build-system r-build-system)
    (native-inputs
     (list r-knitr))
    (propagated-inputs
     (list r-genomicranges
           r-aneufinderdata
           r-ecp
           r-foreach
           r-doparallel
           r-biocgenerics
           r-s4vectors
           r-genomeinfodb
           r-iranges
           r-rsamtools
           r-bamsignals
           r-dnacopy
           r-biostrings
           r-genomicalignments
           r-ggplot2
           r-reshape2
           r-ggdendro
           r-ggrepel
           r-reordercluster
           r-mclust
           r-cowplot))
    (home-page "https://bioconductor.org/packages/AneuFinder/")
    (synopsis "Copy number variation analysis in single-cell-sequencing data")
    (description "This package implements functions for copy number variant
calling, plotting, export and analysis from whole-genome single cell
sequencing data.")
    (license license:artistic2.0)))

(define-public r-anf
  (package
    (name "r-anf")
    (version "1.18.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "ANF" version))
              (sha256
               (base32
                "1fa2pbdapymrpz01ws0m2fbzf11d723x6rbsys29v06is57f5lpj"))))
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
    (version "1.38.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "annmap" version))
              (sha256
               (base32
                "0ywqbb8jia7rrkzcsf6a11kqf8dnx96z8n8xw7067mahycykbixv"))))
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
    (version "1.36.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "antiProfiles" version))
              (sha256
               (base32
                "1277kg5xpyb2yriyjy18p437q5lj22h4al7z7pygkzxzywxv9g40"))))
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

(define-public r-biocversion
  (package
    (name "r-biocversion")
    (version "3.15.2")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BiocVersion" version))
       (sha256
        (base32
         "0rs4nyza4hqqk204d037gi013135wgfhx5asq2dsdjc9vk5nwzfn"))))
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
    (version "0.42.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "BiocGenerics" version))
              (sha256
               (base32
                "0iv9bnpw2hycndwbmjsszqfwrksz6dfr6qcz78jkssc9ldsgmdhc"))))
    (properties
     `((upstream-name . "BiocGenerics")))
    (build-system r-build-system)
    (home-page "https://bioconductor.org/packages/BiocGenerics")
    (synopsis "S4 generic functions for Bioconductor")
    (description
     "This package provides S4 generic functions needed by many Bioconductor
packages.")
    (license license:artistic2.0)))

(define-public r-coverageview
  (package
    (name "r-coverageview")
    (version "1.34.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "CoverageView" version))
              (sha256
               (base32
                "0mh66l4yh6rpd1r7qbqwh5jkklqyvpfiap0zcqhz9kimssm2pbbp"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-s4vectors
           r-iranges
           r-genomicranges
           r-genomicalignments
           r-rtracklayer
           r-rsamtools))
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
   (version "2.38.0")
   (source (origin
             (method url-fetch)
             (uri (bioconductor-uri "cummeRbund" version))
             (sha256
              (base32
               "1p4anmi436zykp0ir307g75g23kj8b7shxg4r65qq6zdwflphm0q"))))
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

(define-public r-dearseq
  (package
    (name "r-dearseq")
    (version "1.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "dearseq" version))
       (sha256
        (base32
         "1f144k5gsclcmsnlsbisr2mivk91dbkci83wx1kznw6i15p4cpj1"))))
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

(define-public r-decipher
  (package
    (name "r-decipher")
    (version "2.24.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "DECIPHER" version))
              (sha256
               (base32
                "045q2bfzgq1yzhyrzvrhrnmlpka4gikrajxxwv05szksy5nvp7q5"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biostrings
           r-dbi
           r-iranges
           r-rsqlite
           r-s4vectors
           r-xvector))
    (home-page "https://www.bioconductor.org/packages/DECIPHER/")
    (synopsis "Tools for deciphering and managing biological sequences")
    (description "This package provides a toolset for deciphering and managing
biological sequences.")
    (license license:gpl3)))

(define-public r-deconvr
  (package
    (name "r-deconvr")
    (version "1.2.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "deconvR" version))
              (sha256
               (base32
                "091z3lncamscsvzj63zzbw7dr7vnkn0jwfkm5ljq4112w4rxgrm3"))))
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
    (version "2.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "decoupleR" version))
       (sha256
        (base32 "0q1w8yw3bwx8ai5z8rw8lz97w4cplxijq93634hza2vgkig1ck9m"))))
    (properties `((upstream-name . "decoupleR")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-broom
           r-dplyr
           r-magrittr
           r-matrix
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
    (version "1.42.1")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "deepSNV" version))
              (sha256
               (base32
                "0bgj1grv3a5bqhcdsw445x49kl3pz367svy6fnrzfsk9bmj46kgn"))))
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

(define-public r-delayedarray
  (package
    (name "r-delayedarray")
    (version "0.22.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "DelayedArray" version))
              (sha256
               (base32
                "11id63qza9dxl1364gllqafxmx25a0q22jv5q8h709bgc3f0grqy"))))
    (properties
     `((upstream-name . "DelayedArray")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocgenerics r-s4vectors r-iranges r-matrix
           r-matrixgenerics))
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

(define-public r-derfinderhelper
  (package
    (name "r-derfinderhelper")
    (version "1.30.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "derfinderHelper" version))
       (sha256
        (base32 "0r7zbx5bfmh5cjs12y8d9qwz53nz340gdy3sx7zcn4rzn7rpslp5"))))
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

(define-public r-drimseq
  (package
    (name "r-drimseq")
    (version "1.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "DRIMSeq" version))
       (sha256
        (base32 "1dph483ij43ayw0z5dbnp6gwp53ka7k5si1hp3miac7z8dqzv94l"))))
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

(define-public r-bluster
  (package
   (name "r-bluster")
   (version "1.6.0")
   (source (origin
            (method url-fetch)
            (uri (bioconductor-uri "bluster" version))
            (sha256
             (base32
              "1g496yc7mdhshf6r0n8xhj7ax936ia5z2cx72lqyk2vzzzl5c4v8"))))
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
    (version "1.32.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "IdeoViz" version))
              (sha256
               (base32
                "1wwh3ifdijhpm58lw7cmnx084xwfxnc7i0206w8rhrjnvnq6ljh3"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biobase
           r-iranges
           r-genomicranges
           r-rcolorbrewer
           r-rtracklayer
           r-genomeinfodb))
    (home-page "https://bioconductor.org/packages/IdeoViz/")
    (synopsis "Plots data along a chromosomal ideogram")
    (description "This package provides functions to plot data associated with
arbitrary genomic intervals along chromosomal ideogram.")
    (license license:gpl2)))

(define-public r-infercnv
  (package
    (name "r-infercnv")
    (version "1.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "infercnv" version))
       (sha256
        (base32
         "01f021fdxm058733rky46dlvqg7dmf5mn5x9lnq0fspp5665w3bl"))))
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
           r-leiden
           r-matrix
           r-paralleldist
           r-phyclust
           r-rann
           r-rcolorbrewer
           r-reshape
           r-rjags
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
    (version "2.30.1")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "IRanges" version))
              (sha256
               (base32
                "1r01c9lczkchgd9hbxxd6wrd5avhy52mfqjck7l9avjq1jimvzv3"))))
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
    (version "1.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "IsoformSwitchAnalyzeR" version))
       (sha256
        (base32 "0n1gb9azxa1mxpsqvw3i3kf72f45nyjj1kgwwrzhd88n3g63lvkd"))))
    (properties `((upstream-name . "IsoformSwitchAnalyzeR")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biobase
           r-biocgenerics
           r-biostrings
           r-bsgenome
           r-dbi
           r-dexseq
           r-dplyr
           r-drimseq
           r-edger
           r-futile-logger
           r-genomeinfodb
           r-genomicranges
           r-ggplot2
           r-gridextra
           r-iranges
           r-limma
           r-magrittr
           r-plyr
           r-rcolorbrewer
           r-rcurl
           r-readr
           r-reshape2
           r-rtracklayer
           r-stringr
           r-tibble
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

;; This is a CRAN package, but it depends on Bioconductor packages.
(define-public r-nmf
  (package
    (name "r-nmf")
    (version "0.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "NMF" version))
       (sha256
        (base32
         "14yxra6in5c1md5nr75y8cdmh9pg0lxqabqflvlhgg1vbg9i2628"))))
    (properties `((upstream-name . "NMF")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-cluster
           r-biobase
           r-biocmanager
           r-bigmemory ; suggested
           r-synchronicity ; suggested
           r-colorspace
           r-digest
           r-doparallel
           r-foreach
           r-ggplot2
           r-gridbase
           r-pkgmaker
           r-rcolorbrewer
           r-registry
           r-reshape2
           r-rngtools
           r-stringr))
    (native-inputs
     (list r-knitr))
    (home-page "http://renozao.github.io/NMF")
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
    (version "1.74.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "affy" version))
       (sha256
        (base32
         "02l77y4d4m4jwgkb3jdaskv6shmba5292whp0i29mg9asxv4rdc7"))))
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
    (version "1.72.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "affycomp" version))
       (sha256
        (base32
         "0aq5p56sqpvba0yhgd75302s9bazchh1izgymng6cpb78y5wfpj0"))))
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
    (version "1.56.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "AffyCompatible" version))
       (sha256
        (base32
         "0x3lj1jgqq67389rzfklah5p878ns9b4fpdpz455m2gq9sk7qsda"))))
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
    (version "1.54.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "affyContam" version))
       (sha256
        (base32
         "1pyd4rj6pp139kvhh97whi4afvx029w5lglr4mnscw7m3f618v0p"))))
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
    (version "1.68.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "affycoretools" version))
       (sha256
        (base32
         "05x64hy5jpmg973biwq4q9gzy1n0iqc0pxrix1f6bri1w6vil3ww"))))
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
           r-glimma
           r-ggplot2
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
    (version "1.66.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "affyio" version))
       (sha256
        (base32
         "19cw82qvzkz6vh2gm302y7digsf6xif7c9l2q9s6lkx2yflqpgfp"))))
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
    (version "1.68.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "affxparser" version))
       (sha256
        (base32
         "16x92gwsy7zdyz4md4cw847xn2ymqd6gqsn0rlr2nnf3qmnjnils"))))
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
    (version "1.74.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "annotate" version))
       (sha256
        (base32
         "0x6vddpiw2g713vicf70198x8dlrwf36p8jjygdsfnl56ls5bh2g"))))
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
    (version "1.58.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "AnnotationDbi" version))
              (sha256
               (base32
                "15cwy7lic89jwl3dr7j4pb5bx457jdpvzvylr71624s0p0j9rgwn"))))
    (properties
     `((upstream-name . "AnnotationDbi")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biobase
           r-biocgenerics
           r-dbi
           r-keggrest
           r-iranges
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
    (version "1.20.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "AnnotationFilter" version))
              (sha256
               (base32
                "082lpcd6yr2nkxndlck2wqqd3nfdx7lnpw8barxgv41q4l7v4ald"))))
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
    (version "1.38.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "AnnotationForge" version))
       (sha256
        (base32
         "18rcfadxdaggyjj3rj17nbvgddlqs6zlr5jmq9a02kin59czvzz8"))))
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
    (version "3.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "AnnotationHub" version))
       (sha256
        (base32
         "03dmbx43rsv9xv94lk12gpraq47ryc13jijwma3q05hl9wn8xjxs"))))
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
           r-interactivedisplaybase
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
    (version "3.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "aroma.light" version))
       (sha256
        (base32
         "1240v9wvsf205g998ms19hncki8g6shidg09dy5np9pqpiix4vys"))))
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
    (version "1.28.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "bamsignals" version))
       (sha256
        (base32
         "0ywbxq829hclhr5bb6p77rspxvfs580zlwd2f5kr3an6rdgyx9ky"))))
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
    (version "2.56.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "Biobase" version))
              (sha256
               (base32
                "1mnxky78an079p60427cjvk4fzilp0xzy6b85fq274qvdcrz8jbv"))))
    (properties
     `((upstream-name . "Biobase")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocgenerics))
    (home-page "https://bioconductor.org/packages/Biobase")
    (synopsis "Base functions for Bioconductor")
    (description
     "This package provides functions that are needed by many other packages
on Bioconductor or which replace R functions.")
    (license license:artistic2.0)))

(define-public r-biomart
  (package
    (name "r-biomart")
    (version "2.52.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "biomaRt" version))
              (sha256
               (base32
                "0yn3kanyrplc89a900xiz33nw1v23mkljvd5isizgs8gzvwzf8xg"))))
    (properties
     `((upstream-name . "biomaRt")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotationdbi
           r-biocfilecache
           r-digest
           r-httr
           r-progress
           r-rappdirs
           r-stringr
           r-xml
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
    (version "1.0.2")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "biomartr" version))
              (sha256
               (base32
                "0hr7wks88lbfcqzjzm4x265dk4lpmc3i2ndp7xcrx8ssj76wrmkz"))))
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
    (version "1.30.3")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "BiocParallel" version))
              (sha256
               (base32
                "1rs3wmasl9mx7f399iclvm0bnvggvjj2a88zbi294r5m8wxqlc92"))))
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
                         "inst/doc/Introduction_To_BiocParallel.pdf"
                         "inst/doc/Errors_Logs_And_Debugging.pdf"
                         "inst/doc/BiocParallel_BatchtoolsParam.R"
                         "inst/doc/Introduction_To_BiocParallel.R"
                         "inst/doc/Errors_Logs_And_Debugging.R"))

             ;; Remove time-dependent macro
             (substitute* '("inst/doc/BiocParallel_BatchtoolsParam.Rnw"
                            "inst/doc/Introduction_To_BiocParallel.Rnw"
                            "inst/doc/Errors_Logs_And_Debugging.Rnw"
                            "vignettes/BiocParallel_BatchtoolsParam.Rnw"
                            "vignettes/Introduction_To_BiocParallel.Rnw"
                            "vignettes/Errors_Logs_And_Debugging.Rnw")
               (("\\today") "later"))

             ;; Initialize the random number generator seed when building.
             (substitute* "R/rng.R"
               (("\"L'Ecuyer-CMRG\"\\)" m)
                (string-append
                 m "; if (!is.na(Sys.getenv(\"SOURCE_DATE_EPOCH\"))) {set.seed(100)}\n"))))))))
    (propagated-inputs
     (list r-bh r-codetools r-futile-logger r-snow))
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
    (version "2.64.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "Biostrings" version))
              (sha256
               (base32
                "1sz52hz89l9w2y2bvyis7kczslk1xnskls9l2bn1s3dhnjzdzhg8"))))
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
    (version "1.44.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "biovizBase" version))
       (sha256
        (base32
         "1ffzf7yvl47l8v8a50m8g9q33hgwvxg4fcm8ld2yy8hd2zl86zyd"))))
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
    (version "1.64.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "BSgenome" version))
              (sha256
               (base32
                "17gqrmaf6xxghgrzcansl9gfw3ghkrqp87swlnwgyghqvflr5qxc"))))
    (properties
     `((upstream-name . "BSgenome")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocgenerics
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
    (version "2.62.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Category" version))
       (sha256
        (base32
         "07js03cfdd6gzbzw14iavlqxynfcqszh988v6k1a3h074wxiivqd"))))
    (properties `((upstream-name . "Category")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotate
           r-annotationdbi
           r-biobase
           r-biocgenerics
           r-genefilter
           r-graph
           r-gseabase
           r-matrix
           r-rbgl
           r-dbi))
    (home-page "https://bioconductor.org/packages/Category")
    (synopsis "Category analysis")
    (description
     "This package provides a collection of tools for performing category
analysis.")
    (license license:artistic2.0)))

(define-public r-chipseeker
  (package
    (name "r-chipseeker")
    (version "1.32.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "ChIPseeker" version))
              (sha256
               (base32
                "001f85nk4myk9vgs05inlj2dfby4802p1iyzkfqg332yk52gsbl7"))))
    (build-system r-build-system)
    (native-inputs
     (list r-knitr))
    (propagated-inputs
     (list r-annotationdbi
           r-biocgenerics
           r-boot
           r-enrichplot
           r-iranges
           r-genomeinfodb
           r-genomicranges
           r-genomicfeatures
           r-ggplot2
           r-gplots
           r-gtools
           r-dplyr
           r-plotrix
           r-dplyr
           r-magrittr
           r-rcolorbrewer
           r-rtracklayer
           r-s4vectors
           r-txdb-hsapiens-ucsc-hg19-knowngene))
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
    (version "1.46.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "chipseq" version))
       (sha256
        (base32
         "1vh0hvgnw7ykj401v1q807sl14s4nixp1d8xbm41n01q6w8x834i"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocgenerics
           r-genomicranges
           r-iranges
           r-lattice
           r-s4vectors
           r-shortread))
    (home-page "https://bioconductor.org/packages/chipseq")
    (synopsis "Package for analyzing ChIPseq data")
    (description
     "This package provides tools for processing short read data from ChIPseq
experiments.")
    (license license:artistic2.0)))

(define-public r-complexheatmap
  (package
    (name "r-complexheatmap")
    (version "2.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ComplexHeatmap" version))
       (sha256
        (base32
         "15b49vlkl89prcw70mlw066z0gxhs26x8dpfn6qr3gz7hihygs65"))))
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

(define-public r-copywriter
  (package
    (name "r-copywriter")
    (version "2.28.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "CopywriteR" version))
       (sha256
        (base32
         "1k11kvam96hpg71hz2n9cfzizmb7d1bmq5zfvm34s7fn09is60xb"))))
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
    (version "1.36.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "DESeq2" version))
       (sha256
        (base32
         "06mvb0jqn2fg96wfwspv0kzpa8xpimzaldrcy8m2d4yk76xwsdr7"))))
    (properties `((upstream-name . "DESeq2")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biobase
           r-biocgenerics
           r-biocparallel
           r-genefilter
           r-geneplotter
           r-genomicranges
           r-ggplot2
           r-iranges
           r-locfit
           r-rcpp
           r-rcpparmadillo
           r-s4vectors
           r-summarizedexperiment))
    (native-inputs
     (list r-knitr))
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
    (version "1.42.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "DEXSeq" version))
       (sha256
        (base32
         "1dzx9mvm8pvcrwr88rin3flnpmzp3vq8mvspx9s8virqhv1102am"))))
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
    (version "1.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "diffcyt" version))
       (sha256
        (base32 "0mysylzmg24g7lm1xan4yklzqmskfgh53j6vjcz2gzakz5rq3rdb"))))
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
    (version "1.38.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "DirichletMultinomial" version))
       (sha256
        (base32
         "15l0h2qz80lmrm5rva3v7lkgddn42igyxxwims57zwpwyhrk9bmx"))))
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
    (version "1.8.1")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "dittoSeq" version))
              (sha256
               (base32
                "0vi0hcyffaxp6yxsrq95bdlrhr85dvbqm9c7rg6a6blkfgwhlzb4"))))
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
    (version "2.30.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "EDASeq" version))
       (sha256
        (base32
         "1qnpbmhxvqsma7ihi6yp3ad962xcanlxald84k2szh011ipxj7ws"))))
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
    (version "3.38.4")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "edgeR" version))
              (sha256
               (base32
                "1ww69xrg9qrmq7dix2k48j6akgn58ss3340hm7pjvzx508x1j6n6"))))
    (properties `((upstream-name . "edgeR")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-limma r-locfit r-rcpp))
    (home-page "http://bioinf.wehi.edu.au/edgeR")
    (synopsis "EdgeR does empirical analysis of digital gene expression data")
    (description "This package can do differential expression analysis of
RNA-seq expression profiles with biological replication.  It implements a range
of statistical methodology based on the negative binomial distributions,
including empirical Bayes estimation, exact tests, generalized linear models
and quasi-likelihood tests.  It be applied to differential signal analysis of
other types of genomic data that produce counts, including ChIP-seq, SAGE and
CAGE.")
    (license license:gpl2+)))

(define-public r-ensembldb
  (package
    (name "r-ensembldb")
    (version "2.20.2")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ensembldb" version))
       (sha256
        (base32
         "12n21dcimdhgyjzk33m6xbv0m9ihgyzcf66vr1jr5ycv3rq2s7xc"))))
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

(define-public r-fastseg
  (package
    (name "r-fastseg")
    (version "1.42.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "fastseg" version))
       (sha256
        (base32
         "1cr1b1jbgp1z1zpf71kl7mljbm2jpi6b97bf3bll3gnagfm489hy"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biobase r-biocgenerics r-genomicranges r-iranges
           r-s4vectors))
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
    (version "2.46.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "gage" version))
       (sha256
        (base32
         "03hx188h98qrbpjlf8v9sg2vqyfv49rp4c18ir11pg6hwqqrxh7b"))))
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
    (version "1.78.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "genefilter" version))
       (sha256
        (base32
         "1lp3alnljhsil8zylf8rvf8ik4wmsyciy3ij4rr9l4191dkkp4aq"))))
    (build-system r-build-system)
    (native-inputs
     (list gfortran r-knitr))
    (propagated-inputs
     (list r-annotate r-annotationdbi r-biobase r-biocgenerics
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
    (version "1.32.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "GeneOverlap" version))
              (sha256
               (base32
                "0nqwa3x9q1hl9nm06hqzzrn00rirc9kj6s320csjlf7x6rcidr93"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-rcolorbrewer r-gplots))
    (home-page "https://www.bioconductor.org/packages/GeneOverlap/")
    (synopsis "Test and visualize gene overlaps")
    (description "This package can be used to test two sets of gene lists
and visualize the results.")
    (license license:gpl3)))

(define-public r-genomation
  (package
    (name "r-genomation")
    (version "1.28.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "genomation" version))
              (sha256
               (base32
                "0rvay7gs4g2wi6h42kln8xwy9b05axj1x8mkfayl6pnnlva6xj79"))))
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
    (version "1.32.3")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "GenomeInfoDb" version))
              (sha256
               (base32
                "17nwcq2ivj3bdibdywfyjq4n6z0djispbh9ahqa55sp31ksq41xh"))))
    (properties
     `((upstream-name . "GenomeInfoDb")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocgenerics r-genomeinfodbdata r-iranges r-rcurl
           r-s4vectors))
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
    (version "1.32.1")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "GenomicAlignments" version))
              (sha256
               (base32
                "09pg7822camyav5zvlpv360sj5gz8q1bhk528qa2da2qsz74a3cz"))))
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

(define-public r-genomicfeatures
  (package
    (name "r-genomicfeatures")
    (version "1.48.3")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "GenomicFeatures" version))
              (sha256
               (base32
                "0f14p1ma2y8l60p9sxmh5j0axws9by1cznczb2jxipphpb4slpl1"))))
    (properties
     `((upstream-name . "GenomicFeatures")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotationdbi
           r-biobase
           r-biocgenerics
           r-biocio
           r-biomart
           r-biostrings
           r-dbi
           r-genomeinfodb
           r-genomicranges
           r-iranges
           r-rcurl
           r-rsqlite
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
    (version "1.32.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "GenomicFiles" version))
       (sha256
        (base32
         "06ycfna26klx27vvsnlpgv46bymfrc8z0zkpag7nm4m23153ivkz"))))
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
    (version "1.48.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "GenomicRanges" version))
              (sha256
               (base32
                "088rv1aclwq265pdg4hmks73nl0125vk0vigyi44n3djkrdx48yn"))))
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

(define-public r-gostats
  (package
    (name "r-gostats")
    (version "2.62.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "GOstats" version))
       (sha256
        (base32
         "121ly9vifarg8y7mc468571bbs0xv4sx6sflm5zcdqf0p83yvjrm"))))
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
           r-rgraphviz
           r-rbgl))
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
    (version "1.58.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "GSEABase" version))
       (sha256
        (base32
         "1qhvgyg392fd98h2qnmfmhg7mil5hp9cy3qmkqs4x1bhpv1m978g"))))
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

(define-public r-hpar
  (package
    (name "r-hpar")
    (version "1.38.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "hpar" version))
       (sha256
        (base32
         "07c6r703d5xp7y9bqmqalxgna2qrbk1h5s0d992m7360k259mgrj"))))
    (build-system r-build-system)
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/hpar/")
    (synopsis "Human Protein Atlas in R")
    (description "This package provides a simple interface to and data from
the Human Protein Atlas project.")
    (license license:artistic2.0)))

(define-public r-rhtslib
  (package
    (name "r-rhtslib")
    (version "1.28.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Rhtslib" version))
       (sha256
        (base32
         "07kws6afkxbmxq4w357mwwl712pdd16alvz7iqijjd2x7rjchj2f"))))
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

(define-public r-impute
  (package
    (name "r-impute")
    (version "1.70.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "impute" version))
              (sha256
               (base32
                "11b0z7py0im6y43k55xpzz5jnvc0ram9rk3n1n4mwhvs0vhy39r2"))))
    (native-inputs
     (list gfortran))
    (build-system r-build-system)
    (home-page "https://bioconductor.org/packages/impute")
    (synopsis "Imputation for microarray data")
    (description
     "This package provides a function to impute missing gene expression
microarray data, using nearest neighbor averaging.")
    (license license:gpl2+)))

(define-public r-interactivedisplaybase
  (package
    (name "r-interactivedisplaybase")
    (version "1.34.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "interactiveDisplayBase" version))
       (sha256
        (base32
         "0fdwx5ch0ch8axdkfiq7zzhhq5hwcvd6kf8fggw9nd3ah1yjwbdg"))))
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
    (version "1.36.3")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "KEGGREST" version))
       (sha256
        (base32
         "0lzb3z6pzm323q70931b7220ygml7jb4g81dybwa79wqiqz15pni"))))
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

(define-public r-lfa
  (package
    (name "r-lfa")
    (version "1.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "lfa" version))
       (sha256
        (base32 "044866h4fnxmzb3sh9vmrd2smgsbcqgvd19dgwxisi418cad577l"))))
    (properties `((upstream-name . "lfa")))
    (build-system r-build-system)
    (propagated-inputs (list r-corpcor))
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
    (version "3.52.2")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "limma" version))
              (sha256
               (base32
                "0m8p8pqmf48f2zdf3qs66hmychbc58g2hfg1wyxzsv180m6xkk65"))))
    (build-system r-build-system)
    (home-page "http://bioinf.wehi.edu.au/limma")
    (synopsis "Package for linear models for microarray and RNA-seq data")
    (description "This package can be used for the analysis of gene expression
studies, especially the use of linear models for analysing designed experiments
and the assessment of differential expression.  The analysis methods apply to
different technologies, including microarrays, RNA-seq, and quantitative PCR.")
    (license license:gpl2+)))

(define-public r-methylkit
  (package
    (name "r-methylkit")
    (version "1.22.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "methylKit" version))
              (sha256
               (base32
                "00asjzv05avfg0rrkmfbdqd6xx8d18zi72n3b1kf9wj81z2d2a35"))))
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
    (version "1.10.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "muscat" version))
       (sha256
        (base32
         "1j3zkhqgza92vdykb1yia1jjwsdqra6q9c0jk6p5p2x0778xqgfd"))))
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
    (version "3.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "MutationalPatterns" version))
       (sha256
        (base32
         "113b2hrc0n47qz144xhky93jcm6qh6flzadq5y0plga5jrz0rnwg"))))
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

(define-public r-msnbase
  (package
    (name "r-msnbase")
    (version "2.22.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "MSnbase" version))
       (sha256
        (base32
         "1xzn0k3c2wn6c6gv90hddy3c201sg927342zrw9ig2xap0r053x3"))))
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
           r-rcpp
           r-s4vectors
           r-scales
           r-vsn
           r-xml))
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
    (version "1.30.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "MSnID" version))
       (sha256
        (base32
         "1yiw95p40nz0pvq7s4i0xg02r9yqmnknak00z4lkw8jij3w3rkkq"))))
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
           r-msnbase
           r-msmstests
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
    (version "1.34.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "mzID" version))
       (sha256
        (base32
         "1q1aqyya9nd494s7m3rdaf3kixipdrwbj825g40kdljwrg85y961"))))
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
    (version "2.30.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "mzR" version))
       (sha256
        (base32
         "1dqa03hb42kbqfg15ksijdkyf9pr54gcl3in4mzjkld5sdi8ncds"))
       (modules '((guix build utils)))
       (snippet
        '(delete-file-recursively "src/boost"))))
    (properties `((upstream-name . "mzR")))
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

(define-public r-organism-dplyr
  (package
    (name "r-organism-dplyr")
    (version "1.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Organism.dplyr" version))
       (sha256
        (base32
         "0j29f85d66c45ww3417xx376vpz0mmvga5n7h2cl1sd4h70b55as"))))
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
    (version "1.38.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "OrganismDbi" version))
       (sha256
        (base32
         "0mxnxj8x4hc21psz39mf7qwvh1fsn6qyjgl5qffk1xxmasf69619"))))
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
           r-s4vectors))
    (home-page "https://bioconductor.org/packages/OrganismDbi")
    (synopsis "Software to enable the smooth interfacing of database packages")
    (description "The package enables a simple unified interface to several
annotation packages each of which has its own schema by taking advantage of
the fact that each of these packages implements a select methods.")
    (license license:artistic2.0)))

(define-public r-pcaexplorer
  (package
    (name "r-pcaexplorer")
    (version "2.22.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "pcaExplorer" version))
       (sha256
        (base32
         "0xkafpi6y5n8hljdaj183hd5z4ik7lpbklg2cbx1hwfz4n4hh1bl"))))
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
    (version "1.88.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "pcaMethods" version))
       (sha256
        (base32
         "1087sl7y707zld7zpf3ly51gnmdp93vn90dwa5440v7qawvg2h9b"))))
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

;; This is a CRAN package, but it depends on a Bioconductor package:
;; r-aroma-light, r-dnacopy..
(define-public r-pscbs
  (package
    (name "r-pscbs")
    (version "0.66.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "PSCBS" version))
       (sha256
        (base32 "14rs2wywipbkia3dbzfhpnkmfgdvm2bf586lggsx63sywlv5d02q"))))
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

(define-public r-protgenerics
  (package
    (name "r-protgenerics")
    (version "1.28.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ProtGenerics" version))
       (sha256
        (base32
         "04hcgj4q8dbzp1a29rbww2bxxrg679pgys3m09p0ydkpsx76rq05"))))
    (properties `((upstream-name . "ProtGenerics")))
    (build-system r-build-system)
    (home-page "https://github.com/lgatto/ProtGenerics")
    (synopsis "S4 generic functions for proteomics infrastructure")
    (description
     "This package provides S4 generic functions needed by Bioconductor
proteomics packages.")
    (license license:artistic2.0)))

(define-public r-rbgl
  (package
    (name "r-rbgl")
    (version "1.72.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "RBGL" version))
       (sha256
        (base32
         "0ph089vxla49sng0pdwiyh9rpk9i96cbsx5q2jn46jj4x51ijc7y"))))
    (properties `((upstream-name . "RBGL")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-bh r-graph))
    (home-page "https://www.bioconductor.org/packages/RBGL")
    (synopsis "Interface to the Boost graph library")
    (description
     "This package provides a fairly extensive and comprehensive interface to
the graph algorithms contained in the Boost library.")
    (license license:artistic2.0)))

(define-public r-rcas
  (package
    (name "r-rcas")
    (version "1.22.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "RCAS" version))
              (sha256
               (base32
                "05sj2ab7bxgf41gkmjaskhqm0198xlir1sw3f73x8rjg14rssmqf"))))
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
           r-ggseqlogo
           r-gprofiler2
           r-iranges
           r-knitr
           r-pbapply
           r-pheatmap
           r-plotly
           r-plotrix
           r-proxy
           r-ranger
           r-rsqlite
           r-rtracklayer
           r-rmarkdown
           r-s4vectors
           pandoc))
    (native-inputs
     (list r-knitr))
    (synopsis "RNA-centric annotation system")
    (description
     "RCAS aims to be a standalone RNA-centric annotation system that provides
intuitive reports and publication-ready graphics.  This package provides the R
library implementing most of the pipeline's features.")
    (home-page "https://github.com/BIMSBbioinfo/RCAS")
    (license license:artistic2.0)))

(define-public r-regioner
  (package
    (name "r-regioner")
    (version "1.28.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "regioneR" version))
       (sha256
        (base32
         "11whi2v211xiz9s7cjl14d8vavlry2fmhvx12rma25wkjmhrpa3f"))))
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
    (version "2.36.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ReportingTools" version))
       (sha256
        (base32
         "0r8cdqzfh1jxkghhk3j8x3y9kkmdyg9ibfhsic15jqkmp1im6khh"))))
    (properties
     `((upstream-name . "ReportingTools")))
    (build-system r-build-system)
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
     (list r-knitr))
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
    (version "2.40.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "rhdf5" version))
              (sha256
               (base32
                "00cp90mnb8p83jiflm6x4x0qf4p7gvgh47jk9jry6j3qyvfqaiff"))))
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
    (version "1.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "rhdf5filters" version))
       (sha256
        (base32
         "1ipg0v8nqz1imj63scqmpiswcxbl4ankg3knfq4p06ic6ypbbmvs"))))
    (properties `((upstream-name . "rhdf5filters")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-rhdf5lib))
    (inputs
     (list zlib))
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
    (version "2.12.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "Rsamtools" version))
              (sha256
               (base32
                "1wll703if12qrn0d11ljwf7rqhs4lb27fzyyz1hqwvzn3v361s10"))))
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
           r-xvector))
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

(define-public r-rtracklayer
  (package
    (name "r-rtracklayer")
    (version "1.56.1")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "rtracklayer" version))
              (sha256
               (base32
                "10qy9s6253mgj871qfqn03i8yw10mz7id4cxfyf67qxczz2xmjls"))))
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
     (list zlib))
    (propagated-inputs
     (list r-biocgenerics
           r-biocio
           r-biostrings
           r-genomeinfodb
           r-genomicalignments
           r-genomicranges
           r-iranges
           r-rcurl
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

(define-public r-scannotatr
  (package
    (name "r-scannotatr")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "scAnnotatR" version))
       (sha256
        (base32 "067q57kabhqd1z8l3d91fw74aaw89nb48gm6fll4hv00nqza3n5b"))))
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
    (version "1.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "scDblFinder" version))
       (sha256
        (base32 "0y14dvdm16b3bvlrnz03adfylm1kj6jrp2fwciyldij2lfal90y0"))))
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

(define-public r-scmap
  (package
    (name "r-scmap")
    (version "1.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "scmap" version))
       (sha256
        (base32 "0pfwaa9pgml11b84rpf7afdkmg8kxb4srgpc56571vaz388xrv7l"))))
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

(define-public r-scry
  (package
    (name "r-scry")
    (version "1.8.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "scry" version))
              (sha256
               (base32
                "16mj21r91jy8ircdz8rfrdli9gjy0hrx90kf6ghs305d3d4dl193"))))
    (properties `((upstream-name . "scry")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocsingular
           r-delayedarray
           r-glmpca
           r-hdf5array
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

(define-public r-seqlogo
  (package
    (name "r-seqlogo")
    (version "1.62.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "seqLogo" version))
       (sha256
        (base32
         "1lk3238m17acmd6lgjjbpscyxw8fm63wv34kbbr478wcih1wbwxr"))))
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
    (version "1.28.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "seqPattern" version))
              (sha256
               (base32
                "0nrrlr1nl9zxmp88qq8jn7wgmda6jh0xvp4nph94w4nwjsyb7xqn"))))
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

(define-public r-shortread
  (package
    (name "r-shortread")
    (version "1.54.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ShortRead" version))
       (sha256
        (base32
         "0303198b4v2wjah9kc829kn01030996l6di4jpf8q5ccd212rjhq"))))
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
           r-rhtslib
           r-hwriter
           r-iranges
           r-lattice
           r-latticeextra
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

(define-public r-simplifyenrichment
  (package
    (name "r-simplifyenrichment")
    (version "1.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "simplifyEnrichment" version))
       (sha256
        (base32
         "0qblgdxmr7zc981529cca3ykakql618q1im6gaxw8pwws5jgpyk6"))))
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

(define-public r-transcriptr
  (package
    (name "r-transcriptr")
    (version "1.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "transcriptR" version))
       (sha256
        (base32 "1zc6aasd5nzwl9jxr0rdriiq85adqdbfi5b9m3jyf69pa71sgy03"))))
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
    (version "1.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "TrajectoryUtils" version))
       (sha256
        (base32
         "07hcr3zplxlzlwc13wh9006m5kaqm57cm1b2x74bpp857f2q93dj"))))
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

(define-public r-slingshot
  (package
   (name "r-slingshot")
   (version "2.4.0")
   (source (origin
            (method url-fetch)
            (uri (bioconductor-uri "slingshot" version))
            (sha256
             (base32
              "0xapi66l5z2qdqns3fcjqcjal6npqj7rxra60lwjvbrq49pq69p2"))))
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

(define-public r-stager
  (package
    (name "r-stager")
    (version "1.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "stageR" version))
       (sha256
        (base32 "0ns3ih6l4na6irshrc5iy4d9qf7hrnqq3ndnlcjb2i1cn38l2w9y"))))
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
    (version "2.8.4")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "STRINGdb" version))
       (sha256
        (base32 "1jn6080v6097zpqsr4gfbx31gqqdhpzjrk63avk3v3xwawmf2379"))))
    (properties `((upstream-name . "STRINGdb")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-gplots
           r-hash
           r-igraph
           r-plotrix
           r-plyr
           r-png
           r-rcolorbrewer
           r-rcurl
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

(define-public r-structuralvariantannotation
  (package
    (name "r-structuralvariantannotation")
    (version "1.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "StructuralVariantAnnotation" version))
       (sha256
        (base32 "0f3x74ic3blg8nm5xlv79k0n8j3fpl98mmhfanqfzmdl0g3j6wx6"))))
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
    (version "1.26.1")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "SummarizedExperiment" version))
              (sha256
               (base32
                "02vlqzmslyijs09jl0gdjxqjjnnl4yqbqqqlb4vb7nr0fspmyz39"))))
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

(define-public r-sva
  (package
    (name "r-sva")
    (version "3.44.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "sva" version))
       (sha256
        (base32
         "0ka259rn0la0hjslj7w24q1dyyh79h84nw6mxp7armqbfjb207a4"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-edger
           r-genefilter
           r-mgcv
           r-biocparallel
           r-matrixstats
           r-limma))
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
    (version "2.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "systemPipeR" version))
       (sha256
        (base32
         "1yybbff29gwv6rm0nw4yjw73bbl5prfj8gj4zky917smjfd459im"))))
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
    (version "2.48.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "topGO" version))
              (sha256
               (base32
                "125r42ymk1irjmwk4sywjkcshs71s26p3zsvryfdvf56k5w162v6"))))
    (properties
     `((upstream-name . "topGO")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotationdbi
           r-dbi
           r-biobase
           r-biocgenerics
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

(define-public r-tximport
  (package
    (name "r-tximport")
    (version "1.24.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "tximport" version))
              (sha256
               (base32
                "1cnra82pvwz79a1hkw0phc6aa3v43r5p4nx8xyx5wzmkd7rjkc8x"))))
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
    (version "0.6.4")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "valr" version))
       (sha256
        (base32
         "0dd41irvibh6rwi52bw4zg4m7wpyihlp1kdkb8fdji3csw2fiz4k"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-broom
           r-dplyr
           r-ggplot2
           r-rcpp
           r-readr
           r-rlang
           r-rtracklayer ;bioconductor package
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
    (version "1.42.1")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "VariantAnnotation" version))
              (sha256
               (base32
                "12d5hkx6pby6l2asyg4jp4jb2x17ybwhqd55rl64h37mwcndbdg1"))))
    (properties
     `((upstream-name . "VariantAnnotation")))
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
           r-summarizedexperiment
           r-rhtslib
           r-rsamtools
           r-rtracklayer
           r-s4vectors
           r-xvector
           r-zlibbioc))
    (build-system r-build-system)
    (home-page "https://bioconductor.org/packages/VariantAnnotation")
    (synopsis "Package for annotation of genetic variants")
    (description "This R package can annotate variants, compute amino acid
coding changes and predict coding outcomes.")
    (license license:artistic2.0)))

(define-public r-vsn
  (package
    (name "r-vsn")
    (version "3.64.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "vsn" version))
       (sha256
        (base32
         "1ja7vdjvgx671l57f9fzfn4vc6q7xzfmqs4krg2rdyfaaf531gqf"))))
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
    (version "1.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "XINA" version))
       (sha256
        (base32 "03gf7mqpnwx12kny9fsaskgrw83b0wi2cf1j4dbq46pfxjx34v1g"))))
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
    (version "1.54.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "xmapbridge" version))
       (sha256
        (base32 "1n3nxc4jwxf5z32i70sza52nyk29adhp8vc3hac7r5b8mbi6gg10"))))
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
    (version "0.36.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "XVector" version))
              (sha256
               (base32
                "1f3sbqy279gb9k13l73j00ixywa1havlqy81zx766r1xkz15nvhk"))))
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
     (list r-biocgenerics r-iranges r-s4vectors))
    (home-page "https://bioconductor.org/packages/XVector")
    (synopsis "Representation and manpulation of external sequences")
    (description
     "This package provides memory efficient S4 classes for storing sequences
\"externally\" (behind an R external pointer, or on disk).")
    (license license:artistic2.0)))

(define-public r-zlibbioc
  (package
    (name "r-zlibbioc")
    (version "1.42.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "zlibbioc" version))
              (sha256
               (base32
                "0w0y9jixdk6akmasn55g9g0nhlh93hbca5bwx5w1fypnvqrqpxzv"))))
    (properties
     `((upstream-name . "zlibbioc")))
    (build-system r-build-system)
    (home-page "https://bioconductor.org/packages/zlibbioc")
    (synopsis "Provider for zlib-1.2.5 to R packages")
    (description "This package uses the source code of zlib-1.2.5 to create
libraries for systems that do not have these available via other means.")
    (license license:artistic2.0)))

(define-public r-zellkonverter
  (package
    (name "r-zellkonverter")
    (version "1.6.3")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "zellkonverter" version))
       (sha256
        (base32 "0l6v7a2zyxpq2w3vm85z439ldi3ld3pkc3wx95a1vxzbr31cpdzz"))))
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
    (version "1.74.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "geneplotter" version))
       (sha256
        (base32
         "13230mzrdralnvf9jp032s16a8mk3kx5476nnvpa4pvcgp1i1ijc"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotate
           r-annotationdbi
           r-biobase
           r-biocgenerics
           r-lattice
           r-rcolorbrewer))
    (home-page "https://bioconductor.org/packages/geneplotter")
    (synopsis "Graphics functions for genomic data")
    (description
     "This package provides functions for plotting genomic data.")
    (license license:artistic2.0)))

(define-public r-oligoclasses
  (package
    (name "r-oligoclasses")
    (version "1.58.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "oligoClasses" version))
       (sha256
        (base32
         "1m4x50gl1fm5waa531v7ml0q229q65qn9cgiwnvjg721fvra7mdk"))))
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
    (version "1.60.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "oligo" version))
       (sha256
        (base32
         "0y7j96rafm9b85sxq2483i73685i3j67lk33fn8nfcav6lmsv5vy"))))
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

(define-public r-qvalue
  (package
    (name "r-qvalue")
    (version "2.28.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "qvalue" version))
       (sha256
        (base32
         "0cvhm5cldcnnxwa293dig1pj9lvj2hnz9zh4gfr25sw0xlcjzmyw"))))
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

(define-public r-apeglm
  (package
   (name "r-apeglm")
   (version "1.18.0")
   (source (origin
            (method url-fetch)
            (uri (bioconductor-uri "apeglm" version))
            (sha256
             (base32
              "1ppwk4g66x46hpqsfsvhl12398d1srqr47nmp0y2gz212kff0rby"))))
   (properties `((upstream-name . "apeglm")))
   (build-system r-build-system)
   (propagated-inputs
    (list r-emdbook
          r-genomicranges
          r-rcpp
          r-rcppeigen
          r-rcppnumerical
          r-summarizedexperiment))
   (native-inputs (list r-knitr))
   (home-page "https://bioconductor.org/packages/apeglm")
   (synopsis "Approximate posterior estimation for GLM coefficients")
   (description "This package provides Bayesian shrinkage estimators for
effect sizes for a variety of GLM models, using approximation of the
posterior for individual coefficients.")
   (license license:gpl2)))

(define-public r-greylistchip
  (package
   (name "r-greylistchip")
   (version "1.28.1")
   (source (origin
            (method url-fetch)
            (uri (bioconductor-uri "GreyListChIP" version))
            (sha256
             (base32
              "0w52vwvjarql19bsv40b80yn701qx8c9d0clsjhj85wmzj2p6dhg"))))
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
    (version "3.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "DiffBind" version))
       (sha256
        (base32
         "0izlk8vmmal4dj0bjxhgzr25arfa9zgdv06rm70w7ylr0gl84pzr"))))
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

(define-public r-mbkmeans
  (package
    (name "r-mbkmeans")
    (version "1.12.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "mbkmeans" version))
              (sha256
               (base32
                "1f5krzlyqljz763vkp1a50danjn78xhn35s8qqdvzrmwyx0fzphg"))))
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

(define-public r-multtest
  (package
    (name "r-multtest")
    (version "2.52.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "multtest" version))
       (sha256
        (base32
         "037wcmwk1wvhjxgmlvnk289pkwishi1753ajkmy9x14xlmldix82"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-survival r-biocgenerics r-biobase r-mass))
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
    (version "1.74.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "graph" version))
              (sha256
               (base32
                "1b8hrjwjg82kicls1496fxfzv75xjvq2k6r9apzsd3qlbyg3ilg4"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocgenerics))
    (home-page "https://bioconductor.org/packages/graph")
    (synopsis "Handle graph data structures in R")
    (description
     "This package implements some simple graph handling capabilities for R.")
    (license license:artistic2.0)))

;; This is a CRAN package, but it depends on a Bioconductor package.
(define-public r-ggm
  (package
    (name "r-ggm")
    (version "2.5")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ggm" version))
       (sha256
        (base32
         "11wc6k2kj2ydy0dyks5mbvbhxm1r43id87anl1jg6dn0yv4m78di"))))
    (properties `((upstream-name . "ggm")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-graph r-igraph))
    (home-page "https://cran.r-project.org/package=ggm")
    (synopsis "Functions for graphical Markov models")
    (description
     "This package provides functions and datasets for maximum likelihood
fitting of some classes of graphical Markov models.")
    (license license:gpl2+)))

;; This is a CRAN package, but it depends on a Bioconductor package, r-graph.
(define-public r-perfmeas
  (package
    (name "r-perfmeas")
    (version "1.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "PerfMeas" version))
       (sha256
        (base32
         "1x7ancmb41zd1js24rx94plgbssyc71z2bvpic6mg34xjkwdjw93"))))
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

;; This is a CRAN package, but it depends on a Bioconductor package.
(define-public r-codedepends
  (package
    (name "r-codedepends")
    (version "0.6.5")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "CodeDepends" version))
       (sha256
        (base32
         "0l7kiv3awx50glf5cs841b4zzsff1ml90f0zr868ygvwsr4ps1hq"))))
    (properties `((upstream-name . "CodeDepends")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-codetools r-graph r-xml))
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
    (version "3.30.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ChIPpeakAnno" version))
       (sha256
        (base32
         "0a26glldxczcfymjvd45gv5m4hympziivm6wwx4ab9wld7n43l8y"))))
    (properties `((upstream-name . "ChIPpeakAnno")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotationdbi
           r-biocgenerics
           r-biomart
           r-biostrings
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
           r-rbgl
           r-regioner
           r-rsamtools
           r-rtracklayer
           r-s4vectors
           r-summarizedexperiment
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
   (version "1.8.1")
   (source (origin
            (method url-fetch)
            (uri (bioconductor-uri "MatrixGenerics" version))
            (sha256
             (base32
              "1liblnpziyyjxzrhdd5d89ilvfqqhbl87h3hsmdm0kwnmc73r37f"))))
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
    (version "1.74.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "marray" version))
              (sha256
               (base32 "0awfz0akz3sylyw1jxhxgadv1rqdzvy9v11933yxkl9a8m9ngm8i"))))
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
   (version "1.56.0")
   (source (origin
            (method url-fetch)
            (uri (bioconductor-uri "CGHbase" version))
            (sha256
             (base32 "1q8yy60r4g5nyv2gbfdgk192xd73c0rrjr668d5616ddb7sx8wcr"))))
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
   (version "2.58.0")
   (source (origin
            (method url-fetch)
            (uri (bioconductor-uri "CGHcall" version))
            (sha256
             (base32 "1qpsibp4gb09sn6fkwwrdjkh3a28lqfbk18c6fvn4m386j96ps65"))))
   (properties `((upstream-name . "CGHcall")))
   (build-system r-build-system)
   (propagated-inputs
    (list r-biobase r-cghbase r-impute r-dnacopy r-snowfall))
   (home-page "https://bioconductor.org/packages/CGHcall")
   (synopsis "Base functions and classes for arrayCGH data analysis")
   (description "This package contains functions and classes that are needed by
@code{arrayCGH} packages.")
   (license license:gpl2+)))

(define-public r-qdnaseq
  (package
    (name "r-qdnaseq")
    (version "1.32.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "QDNAseq" version))
              (sha256
               (base32 "0s360s72lfn9vjml88gg1m40n61s0dc66ilzgfjdcp65djdxxfvm"))))
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
    (version "2.30.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "baySeq" version))
       (sha256
        (base32
         "1yqykndyv32s2rk7x86qf410qr0pigc8z4gdkl8vhj4dgyr47n2j"))))
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
    (version "1.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ChIPComp" version))
       (sha256
        (base32
         "06q34y59gf1iz0rs7y5x8ndy1wa95j65rfmz37aym5c46ijqsnq0"))))
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
    (version "1.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "RiboProfiling" version))
       (sha256
        (base32
         "08m4rc530bkzcc43iwzg2fw9cjlf4wc2d8akv5vblsb42xdn8sqp"))))
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
    (version "1.30.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "riboSeqR" version))
       (sha256
        (base32
         "1zs3y0icsqrndjp9wwqz3jxysvyc9pch45y49j6g9b5b2l44ma26"))))
    (properties `((upstream-name . "riboSeqR")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-abind
           r-bayseq
           r-genomeinfodb
           r-genomicranges
           r-iranges
           r-rsamtools
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
    (version "1.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "InteractionSet" version))
       (sha256
        (base32
         "0qjimx25jvm8siq8hmlbf2z6mknzpbq945p06fsj826k57bpcsm5"))))
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
    (version "1.30.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "GenomicInteractions" version))
       (sha256
        (base32
         "0aph1hja5vfprxs3jl4zd1inhvih6m3v1p3jkm6w7xpj3jzvmgbx"))))
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
    (version "1.70.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ctc" version))
       (sha256
        (base32
         "0c9pgp25dqx12fmi4cqm7xyxjmy6g7wv9vbljgdjghaij2lrc4pb"))))
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
    (version "1.48.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "goseq" version))
       (sha256
        (base32
         "1w0rwzhqkvp2x7y5v0qcyjbss0p95gb1jrnx5sdkqginbvrmrd48"))))
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
    (version "2.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Glimma" version))
       (sha256
        (base32
         "1k17ay09vhb2hakg1vrgvpp1zliavlj7cdkxaal162bc3v8pyvyz"))))
    (properties `((upstream-name . "Glimma")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-deseq2
           r-edger
           r-htmlwidgets
           r-jsonlite
           r-limma
           r-s4vectors
           r-summarizedexperiment))
    (native-inputs
     (list r-knitr))
    (home-page "https://github.com/Shians/Glimma")
    (synopsis "Interactive HTML graphics")
    (description
     "This package generates interactive visualisations for analysis of
RNA-sequencing data using output from limma, edgeR or DESeq2 packages in an
HTML page.  The interactions are built on top of the popular static
representations of analysis results in order to provide additional
information.")
    (license license:lgpl3)))

(define-public r-rots
  (package
    (name "r-rots")
    (version "1.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ROTS" version))
       (sha256
        (base32
         "021a578p8kcl5yd9myiy0h2qp10r30ggnip2kp6xs7dx8nzic96r"))))
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
    (version "1.68.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "plgem" version))
       (sha256
        (base32
         "07zxflxcay17hxjw3wh5kfdwl2x8537csb18p1qzmyrkvscnja77"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biobase r-mass))
    (home-page "http://www.genopolis.it")
    (synopsis "Detect differential expression in microarray and proteomics datasets")
    (description
     "The Power Law Global Error Model (PLGEM) has been shown to faithfully
model the variance-versus-mean dependence that exists in a variety of
genome-wide datasets, including microarray and proteomics data.  The use of
PLGEM has been shown to improve the detection of differentially expressed
genes or proteins in these datasets.")
    (license license:gpl2)))

(define-public r-inspect
  (package
    (name "r-inspect")
    (version "1.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "INSPEcT" version))
       (sha256
        (base32
         "0jx887vhxwd8zlqajr9czvn9nx88ryyxlnl58hxrlajjpcjkz9ax"))))
    (properties `((upstream-name . "INSPEcT")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biobase
           r-biocgenerics
           r-biocparallel
           r-deseq2
           r-desolve
           r-gdata
           r-genomeinfodb
           r-genomicalignments
           r-genomicfeatures
           r-genomicranges
           r-iranges
           r-kernsmooth
           r-plgem
           r-proc
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
    (version "1.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "DNABarcodes" version))
       (sha256
        (base32
         "0n2qlvpcjhrxr3br27gz9vhwcpf7sn6g4xdjazvvi3gqcgk90xc6"))))
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
    (version "1.30.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "RUVSeq" version))
       (sha256
        (base32
         "001h07b074hvj16bjdp9llb9psphw7r6kpwhq61bj4519y6lpg7x"))))
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
    (version "1.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BiocNeighbors" version))
       (sha256
        (base32
         "1a43hzmcpxviqa9723hkafr6gm358amfpqj9d56imclkkfkdz95x"))))
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
    (version "1.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ScaledMatrix" version))
       (sha256
        (base32
         "0p6065mbn77hphpjfchz3r3raspl127f11n39mwh9bih4zg375cl"))))
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
    (version "1.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "treeio" version))
       (sha256
        (base32
         "1hc5m0b2qqxrh3z0inny2jizrpn9d4yn9pn3k1h18xb4ggyijyla"))))
    (properties `((upstream-name . "treeio")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-ape
           r-dplyr
           r-jsonlite
           r-magrittr
           r-rlang
           r-tibble
           r-tidytree))
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

(define-public r-ggtree
  (package
    (name "r-ggtree")
    (version "3.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ggtree" version))
       (sha256
        (base32
         "033r748npv0l72yb9sk6lqnj0l7cd36ykf788145qv8ck5i2gyk4"))))
    (properties `((upstream-name . "ggtree")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-ape
           r-aplot
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
    (version "1.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "metapod" version))
       (sha256
        (base32
         "19g9c08alg4qqr710si465wlb5dy759m5d8wn91zwj24077dds7b"))))
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
    (version "1.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BiocSingular" version))
       (sha256
        (base32
         "1sraycnn0jahpi8kni1y8ik00ga89fvwqjmbr8388968q22mvm3x"))))
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
    (version "3.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "destiny" version))
       (sha256
        (base32
         "1c85ky5ggdsi0ab1l4ipl85gc1kj1zv3wp08qrvslax3z0yw0ljy"))))
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
           r-rspectra
           r-scales
           r-scatterplot3d
           r-singlecellexperiment
           r-smoother
           r-summarizedexperiment
           r-tidyr
           r-tidyselect
           r-vim))
    (native-inputs
     (list r-knitr r-nbconvertr)) ; for vignettes
    (home-page "https://bioconductor.org/packages/destiny/")
    (synopsis "Create and plot diffusion maps")
    (description "This package provides tools to create and plot diffusion
maps.")
    ;; Any version of the GPL
    (license license:gpl3+)))

(define-public r-savr
  (package
    (name "r-savr")
    (version "1.34.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "savR" version))
       (sha256
        (base32
         "04zlf3lyr6vnpj80m6fd2is2f7302sxwih8nzzjnc4ss972jid2k"))))
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
    (version "1.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ChIPexoQual" version))
       (sha256
        (base32
         "1r4s8awvwwj1g33jpnzfxji23mfy0chkhi14i0ml5sh090xijpaz"))))
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
    (version "1.36.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "copynumber" version))
              (sha256
               (base32
                "1gr8q9ri49x8qlmbsi6k6wcak1w9v48wr1qy7axc86brzx6z6mhd"))))
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
    (version "1.70.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "DNAcopy" version))
       (sha256
        (base32
         "10bh4p8nbl84rfngsm3bi9w542m159kff95f8c2hvjcxv5yw7iwc"))))
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
    (version "2.0.8")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "HTSCluster" version))
       (sha256
        (base32
         "0wnbfh6hdx8692jilgmv8sys1zm6fqc6mim7vvjhyqlmpm8gm0kg"))))
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
    (version "1.36.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "EBSeq" version))
       (sha256
        (base32
         "192xl9fwsh04w563yk33mfl303d1kqby2ssbqkckqsdr4jb7d57y"))))
    (properties `((upstream-name . "EBSeq")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-blockmodeling r-gplots r-testthat))
    (home-page "https://bioconductor.org/packages/EBSeq")
    (synopsis "Differential expression analysis of RNA-seq data")
    (description
     "This package provides tools for differential expression analysis at both
gene and isoform level using RNA-seq data")
    (license license:artistic2.0)))

(define-public r-karyoploter
  (package
    (name "r-karyoploter")
    (version "1.22.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "karyoploteR" version))
              (sha256
               (base32
                "0hawq9wi3ikvlcdgnjfy5fiiwfq22zwx1p8xf5h4bpypp96pknsk"))))
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
    (version "1.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "lpsymphony" version))
       (sha256
        (base32
         "0kc708ss5byzw8qh439mb4nq6hsfmz73gfamiznw3lv352brd33g"))))
    (build-system r-build-system)
    (arguments
     (list
      #:phases
      '(modify-phases %standard-phases
         (add-after 'unpack 'make-build-order-reproducible
           (lambda _
             (substitute* '("src/SYMPHONY/Cgl/configure.ac"
                            "src/SYMPHONY/Cgl/configure")
               (("for file in `ls \\*/Makefile.in`")
                "for file in `ls */Makefile.in | sort`")))))))
    (inputs
     (list zlib))
    (native-inputs
     (list pkg-config r-knitr))
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
    (version "1.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "IHW" version))
       (sha256
        (base32
         "1gsfy75dz7xh16z844llcmjnp0a0ridszmrbbv2bdaa43na5msmf"))))
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
    (version "1.24.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "iCOBRA" version))
       (sha256
        (base32
         "1gvra5bgsf6lvs4f2md3xx7xxsx4j8079c2nr8vz9lvy2sfyl6s9"))))
    (properties `((upstream-name . "iCOBRA")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-dplyr
           r-dt
           r-ggplot2
           r-limma
           r-markdown
           r-reshape2
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
    (version "1.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ResidualMatrix" version))
       (sha256
        (base32
         "1pjr3gva0jwj2pgqr4k4nl1ir1153hhrk1d400r30w0di472hns4"))))
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
    (version "1.12.3")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "batchelor" version))
       (sha256
        (base32
         "00ix3hvhgalxg63qnynv2waa273jk336lg47k72qwxfzimsxfjxc"))))
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
    (version "1.22.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "MAST" version))
       (sha256
        (base32
         "1kmrqxcfzzcs8l33n9qn0vahc6wxq6ks3cjx95vg96maf2qzhzzi"))))
    (properties `((upstream-name . "MAST")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-abind
           r-biobase
           r-biocgenerics
           r-data-table
           r-ggplot2
           r-plyr
           r-progress
           r-reshape2
           r-s4vectors
           r-singlecellexperiment
           r-stringr
           r-summarizedexperiment))
    (native-inputs
     (list r-knitr))
    (home-page "https://github.com/RGLab/MAST/")
    (synopsis "Model-based analysis of single cell transcriptomics")
    (description
     "This package provides methods and models for handling zero-inflated
single cell assay data.")
    (license license:gpl2+)))

(define-public r-monocle
  (package
    (name "r-monocle")
    (version "2.24.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "monocle" version))
       (sha256
        (base32
         "11g1wx0f1yzhg3x1aa3d5l7pqlzxj16s0gha21skxkgld8k2x8xn"))))
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
           r-qlcmatrix
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
    (version "2.40.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "NOISeq" version))
       (sha256
        (base32
         "0ah6adlhv4254jkssinn2ik8n811hd1nw85bnzqk2kwhl49nrk27"))))
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
    (version "1.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "scDD" version))
       (sha256
        (base32
         "0bjww338z5qf2g97kbh85h9kpagjr59ff9f4alm33h16xz5mb7k0"))))
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
    (version "1.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "scone" version))
       (sha256
        (base32
         "05id34n6min03ha1chg5mrvx399qm2mby9kxkaz5w8fbidp97851"))))
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
    (version "2.64.2")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "GEOquery" version))
       (sha256
        (base32
         "1cvkvq2haz831qi8w0gd3ayvxfxsl0z5klhki4gkfi9xqdv1gi9x"))))
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
    (version "0.38.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "illuminaio" version))
       (sha256
        (base32
         "1xk057a9w4ps8xi8jyw8imkjcicfmzns8g92grn4af7yiip68h62"))))
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
    (version "1.70.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "siggenes" version))
       (sha256
        (base32
         "0amjqm2c8p1vjzx109p7n81wbsbx8rljwn6mbkl7dpi834im9d7l"))))
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
    (version "1.38.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "bumphunter" version))
       (sha256
        (base32
         "0k92ps9chqsimbc7vsr8swg679zfv8nfn5zahbqq4nknhhy7hwxw"))))
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

(define-public r-milor
  (package
    (name "r-milor")
    (version "1.4.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "miloR" version))
              (sha256
               (base32
                "1jz9p3grnczx0bpdw6j64x21in8zgm3qy19hmm296har2rx9m5zs"))))
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
           r-matrixstats
           r-patchwork
           r-rcolorbrewer
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
    (version "1.42.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "minfi" version))
       (sha256
        (base32
         "0255z7w5i5k01w8wn7jkb37h3q7m7vg0szqgk76h330yydnmkrq6"))))
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

(define-public r-methylumi
  (package
    (name "r-methylumi")
    (version "2.42.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "methylumi" version))
       (sha256
        (base32
         "0klkinq55lfj1d4z8gkrv98849079x1l5gd15habw7jq9xxvhjww"))))
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

(define-public r-lumi
  (package
    (name "r-lumi")
    (version "2.48.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "lumi" version))
       (sha256
        (base32
         "06zmll5j1yymsm3byarhllrz4q1w5mzv267a9g6visn73wan8y9d"))))
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
    (version "2.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Linnorm" version))
       (sha256
        (base32
         "1002lllgns5klv3q2wsikkbypa2bafpka7a8mri0y5bfxncfr2zb"))))
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
    (version "2.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "IONiseR" version))
       (sha256
        (base32
         "0cgx1dcfh617l9vr4r3ky8w7f0snl0vpavfd9n1h5n68p0p42dwi"))))
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
    (version "0.1-12")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "mutoss" version))
       (sha256
        (base32
         "1yk7p7pb2xm38d3j19ysgwmix48lvimbhkhjjwk5jmr1a0ysx298"))))
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
    (version "1.8")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "metap" version))
       (sha256
        (base32
         "0asmypxfxly4xmcjf3yzid5zqlzg88z0brij2splfk4avsl035gf"))))
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
   (version "1.10.0")
   (source (origin
            (method url-fetch)
            (uri (bioconductor-uri "tradeSeq" version))
            (sha256
             (base32
              "0v9nqxrwa69qhmyaicn2vvs8haha4kzs93iqim306331vadp9qm0"))))
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
    (version "1.38.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "VariantTools" version))
       (sha256
        (base32
         "18nxcamfgnw4n2ab0czxglw0sqc9wzdqzpjv43lcyyal23lzzsix"))))
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
    (version "3.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Heatplus" version))
       (sha256
        (base32
         "0b1mzxysmrqinp93p587apna8p0llmawblwj93icydqxxm2jkhb1"))))
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
    (version "2.22.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "GOSemSim" version))
       (sha256
        (base32
         "1hp15pzd0m0g9f8kglyfsgjqxnvxcmm9022xnsrkzfvmj2yw14vd"))))
    (properties `((upstream-name . "GOSemSim")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotationdbi r-go-db r-rcpp))
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
    (version "1.44.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "anota" version))
       (sha256
        (base32
         "1x75r5znl8jllqsgzpxsqj62ch11bpwhmyzmbjmb8sz8f8ww923c"))))
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

(define-public r-sigpathway
  (package
    (name "r-sigpathway")
    (version "1.64.0")
    (source
      (origin
        (method url-fetch)
        (uri (bioconductor-uri "sigPathway" version))
        (sha256
          (base32
            "1c2kwhbxgf66az7ssm2mab9n5x59zy4kxq8vblz5r9636xqaysif"))))
    (properties `((upstream-name . "sigPathway")))
    (build-system r-build-system)
    (home-page "https://www.pnas.org/cgi/doi/10.1073/pnas.0506577102")
    (synopsis "Pathway analysis")
    (description
     "This package is used to conduct pathway analysis by calculating the NT_k
and NE_k statistics in a statistical framework for determining whether a
specified group of genes for a pathway has a coordinated association with a
phenotype of interest.")
    (license license:gpl2)))

(define-public r-fcscan
  (package
    (name "r-fcscan")
    (version "1.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "fcScan" version))
       (sha256
        (base32 "0yv7ifw0xxx1v9z8dxszv0cb72q3frd74dyxfbvrcs6x9y9v3jzp"))))
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
    (version "1.22.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "fgsea" version))
       (sha256
        (base32
         "0innyggai6l4fpl4qrblzdc52vqw9jaszmip0yr1lv7rzwyl6mpg"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-bh
           r-biocparallel
           r-data-table
           r-fastmatch
           r-ggplot2
           r-gridextra
           r-matrix
           r-rcpp))
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
    (version "3.22.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "DOSE" version))
       (sha256
        (base32
         "11lg4ql0bi54p2wg3z1dw9rwqai37khgcqbs4cb7zf67ml8jadwp"))))
    (properties `((upstream-name . "DOSE")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotationdbi
           r-biocparallel
           r-do-db
           r-fgsea
           r-ggplot2
           r-gosemsim
           r-qvalue
           r-reshape2))
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

(define-public r-enrichplot
  (package
    (name "r-enrichplot")
    (version "1.16.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "enrichplot" version))
       (sha256
        (base32
         "17ln1wbkq8sp7jw0dpkccj5qcsl382sgd7zic04dk99z9ag3mh02"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-aplot
           r-dose
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

(define-public r-clusterprofiler
  (package
    (name "r-clusterprofiler")
    (version "4.4.4")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "clusterProfiler" version))
       (sha256
        (base32
         "0k5jhry0j6wa7779n3hrw4ld4bvyahpgpbwi2a0g704m3dd3mqp5"))))
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
    (version "2.16.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "clusterExperiment" version))
              (sha256
               (base32
                "1xd2kxmdg51hhj0zvz7pxmpdvb1sya7prsf9ny2wj2y8ivrqgn4f"))))
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
           r-howmany
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
    (version "1.76.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "MLInterfaces" version))
       (sha256
        (base32
         "179d19kxjipfkc40z15337x1vzqd7vz3gbmr2lw5w7x9l857ngs5"))))
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
           r-threejs))
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
    (version "1.68.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "annaffy" version))
       (sha256
        (base32
         "1fbqknwbl4534h66xrhcryg9pavm9fkja47gqbsxf8bd5yhk5mgq"))))
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
    (version "1.44.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "a4Core" version))
       (sha256
        (base32
         "1ky1lphq6bqxj6h12pg06cvs451fziqam8gd56wmpk6r5pbg4390"))))
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
    (version "1.44.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "a4Classif" version))
       (sha256
        (base32
         "1v61vgpqrf7bhk44n2gkxb8dm5d0rr8c9rd6fdcjs50nhij0lbiw"))))
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
    (version "1.44.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "a4Preproc" version))
       (sha256
        (base32
         "098yzy7x5536bj76iavismdsdn7x6x07aw0j3knj6i9www9y8yz9"))))
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
    (version "1.44.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "a4Reporting" version))
       (sha256
        (base32
         "03sypayh187gqc6hykkqr1g0vb3zxc2c3xyp00jfbn12b35acnb0"))))
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
    (version "1.44.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "a4Base" version))
       (sha256
        (base32
         "15zqirz16gpks9f5d3d76h85b936za2jih74vfr55l5arqrrvvsn"))))
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
    (version "1.44.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "a4" version))
       (sha256
        (base32
         "1zs8fs6mdd7fhsmx4k824mid0jk400cm6dwfhl8z5lg85y8y2n0r"))))
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
    (version "1.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "abseqR" version))
       (sha256
        (base32
         "0jh3rj6ag07vpw6fymqm6m4jkrm9mgf50zkjncahxdf52mna8a9b"))))
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
    (version "1.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "bacon" version))
       (sha256
        (base32
         "1zvcxdj3r892898ik5gq3jdbfig1438qws4bwd465ik8vi7g39v8"))))
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
    (version "2.44.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "rGADEM" version))
       (sha256
        (base32
         "013xdwz0c3n0n9hxf8kkx570qry961pgdjsp023531pl5ww2ing4"))))
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
   (version "1.38.0")
   (source (origin
            (method url-fetch)
            (uri (bioconductor-uri "MotifDb" version))
            (sha256
             (base32 "1cyfz0l0yvdii3idaiq5w39yzxlzfpifa4v5pv7hdjfjj83a8rbi"))))
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
     (list r-knitr))
   (home-page "https://www.bioconductor.org/packages/MotifDb/")
   (synopsis "Annotated collection of protein-DNA binding sequence motifs")
   (description "This package provides more than 2000 annotated position
frequency matrices from nine public sources, for multiple organisms.")
   (license license:artistic2.0)))

(define-public r-motifbreakr
  (package
   (name "r-motifbreakr")
   (version "2.10.0")
   (source (origin
            (method url-fetch)
            (uri (bioconductor-uri "motifbreakR" version))
            (sha256
             (base32 "0sad73jjx52qzp1fmygp6xqvaxwl5szi69f00f94i1pdyq70qhlg"))))
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
    (version "1.40.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "motifStack" version))
       (sha256
        (base32
         "0d2ihx73chczbv6f91n04qb372plrdv7k4qws8shyw1fmvb1rq0z"))))
    (properties `((upstream-name . "motifStack")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-ade4
           r-biostrings
           r-ggplot2
           r-htmlwidgets
           r-tfbstools
           r-xml))
    (native-inputs
     (list r-knitr))
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
    (version "2.8.2")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "GenomicScores" version))
       (sha256
        (base32
         "12rcxw69an1d5q7ar58xy8s871l47imw2nm08j054ivxslx8597j"))))
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

(define-public r-atacseqqc
  (package
    (name "r-atacseqqc")
    (version "1.20.2")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ATACseqQC" version))
       (sha256
        (base32
         "0jj7n0mcj0gciw0ksazlksgmwzp51a40pwqhf0c7la0cc4bnrkqp"))))
    (properties `((upstream-name . "ATACseqQC")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocgenerics
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
    (version "1.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "GOfuncR" version))
       (sha256
        (base32
         "02vdfsjrqp0m06mfbspwkxjyqxfca0w1idgygpi1a9i5m4fqhwpk"))))
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
    (version "1.70.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "annotationTools" version))
       (sha256
        (base32
         "122b424zida3j0vqkn8d06sg3jpc3ngsgidr8kgg00n4cjngkc51"))))
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
    (version "1.34.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "AllelicImbalance" version))
       (sha256
        (base32
         "0w4xd0xzkwx7bbhrgqligpahlhg85rginknx520z891r8v0bim2z"))))
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
    (version "1.18.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "AUCell" version))
       (sha256
        (base32
         "17wr7dycll0l1gax4w268qw7is163bs51rj6p1qnx1dgc9ibnsgr"))))
    (properties `((upstream-name . "AUCell")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocgenerics
           r-data-table
           r-delayedarray
           r-delayedmatrixstats
           r-gseabase
           r-mixtools
           r-r-utils
           r-shiny
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
    (version "4.38.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "EBImage" version))
       (sha256
        (base32
         "1vcx45bw36k9daw9dywj5bz77jmqk4gjfwsym8ajjnc1jmlq20si"))))
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
    (version "1.22.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "yamss" version))
       (sha256
        (base32
         "1lcfxw73cxvpy3bnq28pxdy5128mpq5xklsa0mzxdjyqc4g55hy8"))))
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
    (version "1.28.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "gtrellis" version))
       (sha256
        (base32
         "1s4xczzv6hz2kyv32xgcq84540w75qr3f644w1s4c3kwxgyq2gff"))))
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
    (version "2.32.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "SomaticSignatures" version))
       (sha256
        (base32
         "1ydnp54laznzpi08s403kxhnr5nqhvm3iilaxlcdlz0ngxhm6vx6"))))
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
    (version "1.22.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "YAPSA" version))
       (sha256
        (base32
         "1klqfif4sadkxw7agywk2ncvcdqsnfb1d6adnacdfdasr8abvhid"))))
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
    (version "2.68.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "gcrma" version))
       (sha256
        (base32
         "13a8igr2b02gsa6n3437kb33wg6h7si82fmqi35dzpfzhvx0qf6d"))))
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
    (version "1.30.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "quantro" version))
       (sha256
        (base32
         "1zfrz7lxyrbf0c8d277npzj1h4six9whkqplvcjmn3li0xj5qng3"))))
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
    (version "1.22.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "yarn" version))
       (sha256
        (base32
         "0z5202pqq02fwm8qf1g36004k7sv668s1xacbpr1cvw5sl452lbg"))))
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
    (version "1.32.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "roar" version))
       (sha256
        (base32
         "0zq1praf5h9294cvmrb06l3chx8v40xw2sfvhlnh1516x9sjkwfc"))))
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
    (version "1.62.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "MassSpecWavelet" version))
       (sha256
        (base32
         "0g9izdy3f7h1zmsfbq45ahdz0ak5013rp3vxc4ijb1mpqx8ldd39"))))
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
    (version "3.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "xcms" version))
       (sha256
        (base32
         "0p2zd2728lj5q8y24gdfvsjijd6zl2i73hrcf017n32jq7vn71xm"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biobase
           r-biocgenerics
           r-biocparallel
           r-iranges
           r-lattice
           r-massspecwavelet
           r-mscoreutils
           r-msfeatures
           r-msnbase
           r-mzr
           r-plyr
           r-protgenerics
           r-rann
           r-rcolorbrewer
           r-robustbase
           r-s4vectors
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
    (version "1.4.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "wppi" version))
              (sha256
               (base32
                "1008s39bb7sd261cy1vfgdah7bmhfw9qq322fh7g4wvpfw63ii9f"))))
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
    (version "1.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Wrench" version))
       (sha256
        (base32
         "1zx65s4m71wj85s2sq8ip54pq12r4sxfv8b2rxc41gfc5aj0zzca"))))
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
    (version "1.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "wiggleplotr" version))
       (sha256
        (base32
         "0s128mm5w8n072k6j0fv1mxnxjpwisjp5lpz8a9z96cnn69bnr0i"))))
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
    (version "1.74.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "widgetTools" version))
       (sha256
        (base32
         "10w1s5h4za6ibmphvj145ir3lp22qgah2z8fvmipmf8ciq1jf86d"))))
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
    (version "1.68.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "webbioc" version))
       (sha256
        (base32
         "1g3srxsa2fqcn3r4wz4y19fwjmw3vawlcvdw6lbjdnvbgcafq1ah"))))
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
    (version "1.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "zinbwave" version))
       (sha256
        (base32
         "0vpz721sciw5b4ypxj5lj8p53gwkpfwlwkn6k3y8i65zg80p1g6i"))))
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
    (version "1.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "zFPKM" version))
       (sha256
        (base32
         "1h7g553rgb5mkmmsp8dyqqs9n9x17xmmcg3iijhb54nyrr2j1mji"))))
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
    (version "2.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Rbowtie2" version))
       (sha256
        (base32
         "0dhdx27vrkhd4fak0qb5q9amlcpi97xhf3ry39zk0ifx5zpjynkg"))))
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
    (version "1.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "progeny" version))
       (sha256
        (base32
         "1rhy2l2yf9ndxlvff8756s6n8qyi42nz7a75lgygj5aqqckkj21b"))))
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
    (version "1.36.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ARRmNormalization" version))
       (sha256
        (base32
         "1pnvw8psbql787m8lrmhd9xbmgkc3dbc70yfds1aggv50dk3yjk1"))))
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
    (version "2.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BiocFileCache" version))
       (sha256
        (base32
         "1bdbmlixrd8wvs25nmzdksq5hwnsxf8b6ds9qwx01h284vky5vsw"))))
    (properties `((upstream-name . "BiocFileCache")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-curl
           r-dbi
           r-dbplyr
           r-dplyr
           r-filelock
           r-httr
           r-rappdirs
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
    (version "1.32.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "iClusterPlus" version))
       (sha256
        (base32
         "0xzx3vly3p99zc5a69pra4jjp8d3bdhx7dl1l76w459cs58zy0sm"))))
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
    (version "1.36.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Rbowtie" version))
       (sha256
        (base32
         "1ya1irwshsyy9l1fj51b04nv1ahq7a47ck7q19h2cly6yskc4x1q"))))
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
    (version "1.30.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "SGSeq" version))
       (sha256
        (base32
         "0hz45367i70wl97silnimicdvs3g41zyf8syc6igz6471wbwkxwp"))))
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
    (version "1.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Rhisat2" version))
       (sha256
        (base32
         "0hzair41l47kzykymd169a34pfhb98vrjgkgdf15m17csyz7pnv7"))))
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
     (list r-genomicfeatures r-genomicranges r-sgseq))
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
    (version "1.36.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "QuasR" version))
       (sha256
        (base32
         "1m0c0rdakkdn4rr6dh51c6rs40cbxkvz93n6s0m2kc6fqjv9zplf"))))
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
           r-shortread))
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
    (version "1.30.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Rqc" version))
       (sha256
        (base32
         "11j8m69zdcmpjb3xzr4s8sqmv8aqgl8q7k81gnd09l3nyjzy0h1k"))))
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
    (version "3.28.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BiRewire" version))
       (sha256
        (base32
         "0r3i7n45qgj8wzdsx8wmfk0lc4xbcvxvmfziiqzig7r706q2c2hm"))))
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
    (version "1.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "MultiDataSet" version))
       (sha256
        (base32
         "0rfs6jkzh1i4mj1pgfk4lwzmcl8pcwizra3q3282x3d8h2g98qnf"))))
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
    (version "1.28.2")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ropls" version))
       (sha256
        (base32
         "07gpx15r8c3wljiwxnff2zp7wxbhzz9r7njk8zg8hpy2q5qm3i6c"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biobase
           r-multiassayexperiment
           r-multidataset
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
    (version "1.24.2")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "biosigner" version))
       (sha256
        (base32
         "0vdv2by3qv7y8vzr8qgg7apwwgsa0fhlfrhzns7g3nas7883c89m"))))
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
    (version "1.22.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "annotatr" version))
       (sha256
        (base32
         "1fbax9v3d486c8lwf3yfjbf4w7zf53wmdpxk2clwm8ngm7w0pqm0"))))
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
    (version "2.10.4")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Rsubread" version))
       (sha256
        (base32
         "155h25gbagqns7wpriil17li0jkdd1z1pcz0dlnikdqj4saf97rl"))))
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
    (version "1.60.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ConsensusClusterPlus" version))
       (sha256
        (base32
         "1021cix4mr9qsafskw4kk1l3wdzx9pk2gcwjifz6f4zqxss9v07p"))))
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

(define-public r-cytolib
  (package
    (name "r-cytolib")
    (version "2.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "cytolib" version))
       (sha256
        (base32
         "18b532sicca5l8sn334prrm7g1z1cakiwydccz4i833168pnsjyg"))))
    (properties `((upstream-name . "cytolib")))
    (build-system r-build-system)
    (native-inputs
     (list r-knitr))
    (propagated-inputs
     (list r-bh
           r-rcpp
           r-rcpparmadillo
           r-rcppparallel
           r-rhdf5lib
           r-rprotobuflib))
    (home-page "https://bioconductor.org/packages/cytolib/")
    (synopsis "C++ infrastructure for working with gated cytometry")
    (description
     "This package provides the core data structure and API to represent and
interact with gated cytometry data.")
    (license license:artistic2.0)))

(define-public r-flowcore
  (package
    (name "r-flowcore")
    (version "2.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "flowCore" version))
       (sha256
        (base32
         "17nci6rc4i0vs0ibw5q8zy30ap7q4550qpq4ifkbblqbyzxlzkhr"))))
    (properties `((upstream-name . "flowCore")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-bh
           r-biobase
           r-biocgenerics
           r-cytolib
           r-matrixstats
           r-rcpp
           r-rcpparmadillo
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
    (version "1.56.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "flowMeans" version))
       (sha256
        (base32
         "1n4li43ydwwf5gvgmdml4ba28cxymybg5wnz6jvp35n959fwxv6y"))))
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
    (version "2.42.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ncdfFlow" version))
       (sha256
        (base32
         "18ba8rygcd1ys150pk38r4w5lxwm6sl76zkd294yg847dygsqa4m"))))
    (properties `((upstream-name . "ncdfFlow")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-bh
           r-biobase
           r-biocgenerics
           r-flowcore
           r-rcpp
           r-rcpparmadillo
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
    (version "1.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ggcyto" version))
       (sha256
        (base32
         "0sycyvdpa77mykzr709a7padh6478zmnzapibbq90qkc7bxnj359"))))
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
    (version "1.60.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "flowViz" version))
       (sha256
        (base32
         "175ygncrv6q6mb8pahixs89m9wm6hdpzx489gc9s8lgad2vrvz8f"))))
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
    (version "3.34.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "flowClust" version))
       (sha256
        (base32
         "055vm9s8aha92znhpjqkipzprw8bkrinwjaik4ygmhym7w6vbblk"))))
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
    (version "2.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "RProtoBufLib" version))
       (sha256
        (base32
         "1mvqwrm1y0vij66gdwgpf5l1h660wsi9jzjfs4ihw3zm4cb0q5pp"))))
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
    (version "4.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "flowWorkspace" version))
       (sha256
        (base32
         "0riyi9628cx1j5x6hmdd28yq75xh25j8ckcdz8dnb94dpvnhaqss"))))
    (properties `((upstream-name . "flowWorkspace")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-aws-s3
           r-aws-signature
           r-bh
           r-biobase
           r-biocgenerics
           r-cytolib
           r-data-table
           r-delayedarray
           r-digest
           r-dplyr
           r-flowcore
           r-ggplot2
           r-graph
           r-lattice
           r-latticeextra
           r-matrixstats
           r-ncdfflow
           r-rbgl
           r-rcpp
           r-rcpparmadillo
           r-rcppparallel
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
    (version "4.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "flowStats" version))
       (sha256
        (base32
         "1jbc92ah2mlpnd7v3k0207v4qz3rg9g9yy6r6y0s0cc5nifdyhwj"))))
    (properties `((upstream-name . "flowStats")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biobase
           r-biocgenerics
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
    (version "2.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "openCyto" version))
       (sha256
        (base32
         "1nz5fra0jf70jwyfbcz5ksnz5xb62vfnfwfasr0zwwvjvmmvrs1y"))))
    (properties `((upstream-name . "openCyto")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biobase
           r-biocgenerics
           r-clue
           r-data-table
           r-flowclust
           r-flowcore
           r-flowstats
           r-flowviz
           r-flowworkspace
           r-graph
           r-gtools
           r-ks
           r-lattice
           r-mass
           r-ncdfflow
           r-plyr
           r-r-utils
           r-rbgl
           r-rcolorbrewer
           r-rcpp
           r-rrcov))
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
    (version "2.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "CytoML" version))
       (sha256
        (base32
         "0vp7advfh1d8596hjpzayasjhga4mx0l104sgz2asscbrjm4v7rr"))))
    (properties `((upstream-name . "CytoML")))
    (build-system r-build-system)
    (inputs
     (list libxml2 zlib))
    (propagated-inputs
     (list r-base64enc
           r-bh
           r-biobase
           r-corpcor
           r-cytolib
           r-data-table
           r-dplyr
           r-flowcore
           r-flowworkspace
           r-ggcyto
           r-graph
           r-jsonlite
           r-lattice
           r-opencyto
           r-plyr
           r-rbgl
           r-rcpp
           r-rcpparmadillo
           r-rcppparallel
           r-rgraphviz
           r-rhdf5lib
           r-rprotobuflib
           r-runit
           r-tibble
           r-xml
           r-xml2
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
    (version "2.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "FlowSOM" version))
       (sha256
        (base32
         "0balsds5mm981cqamdjv3ndq1y9arharisd6f2lrpkzgvwawa645"))))
    (properties `((upstream-name . "FlowSOM")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocgenerics
           r-colorramps
           r-consensusclusterplus
           r-cytoml
           r-dplyr
           r-flowcore
           r-flowworkspace
           r-ggforce
           r-ggnewscale
           r-ggplot2
           r-ggpointdensity
           r-ggpubr
           r-ggrepel
           r-igraph
           r-magrittr
           r-pheatmap
           r-rcolorbrewer
           r-rlang
           r-rtsne
           r-scattermore
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
    (version "6.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "mixOmics" version))
       (sha256
        (base32
         "0fwc2w7frj0bjijzfckkxf7ipx1z13gw7907q4zr5qfl9mh127w7"))))
    (properties `((upstream-name . "mixOmics")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocparallel
           r-corpcor
           r-dplyr
           r-ellipse
           r-ggrepel
           r-ggplot2
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
    (version "1.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "DepecheR" version))
       (sha256
        (base32
         "0rixczdds5gpac50wap6s68kmpdj4208l38gcihkrysz5frbvqjp"))))
    (properties `((upstream-name . "DepecheR")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-beanplot
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
    (version "1.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "RcisTarget" version))
       (sha256
        (base32
         "17fnjkg6rjqj33v7slg81skqag10y6dc14g5iv69gqshjal4w4im"))))
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
           r-tibble))
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
    (version "1.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Chicago" version))
       (sha256
        (base32
         "13vzxmvxpc3r9gii37zvhhr5nbnaggrva97g6m2n02qn9daf6vmm"))))
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
    (version "1.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "cicero" version))
       (sha256
        (base32
         "1ip12ijazlmcfbym078slxykpkz7d1zwvs8l8aqdnqpxjfk1ipx5"))))
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
    (version "1.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "circRNAprofiler" version))
       (sha256
        (base32
         "1gwm416shhv2p3gh1n6kv1rvx0n0imy25b7z62z4s8b3gs3nfp5j"))))
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
    (version "1.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "GENIE3" version))
       (sha256
        (base32
         "0h3vnpnznb9rda8gfwp4cnd2mqsvs1vzmfx90dchn5pqaphz1k2l"))))
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
    (version "1.72.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ROC" version))
       (sha256
        (base32
         "0yfq0d0j2bzqdnjs6l2h6p48kmv9wfphlqym3brgndlnadipq1v2"))))
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
    (version "2.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "wateRmelon" version))
       (sha256
        (base32
         "0adqyfabrvfcaj3mwp0rbqlcgpj92yb205cyhibbrs5gdr5ri4pv"))))
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
    (version "1.32.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "gdsfmt" version))
       (sha256
        (base32
         "1cdwyivgfc6yw5hj9b3j57wx55gckwhx6fwx2lvqynrjzjyzf3q0"))
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
    (properties `((upstream-name . "gdsfmt")))
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
    (version "1.22.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "bigmelon" version))
       (sha256
        (base32
         "1msch4qbifkdqv0bbw03xj6d9w28z91mf4ki41rqg6048cq17h2k"))))
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
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/bigmelon/")
    (synopsis "Illumina methylation array analysis for large experiments")
    (description
     "This package provides methods for working with Illumina arrays using the
@code{gdsfmt} package.")
    (license license:gpl3)))

(define-public r-seqbias
  (package
    (name "r-seqbias")
    (version "1.44.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "seqbias" version))
       (sha256
        (base32
         "085nq6pf0bdn17wsbr5jnyy512v7rf67xff9rp5wz47mcifbv6rg"))))
    (properties `((upstream-name . "seqbias")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biostrings r-genomicranges r-rhtslib))
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
    (version "1.42.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ReQON" version))
       (sha256
        (base32
         "1f5pplm8fy3wvl0b6n18gph4dq9i9x5qiyjrj0bk0kwlkbpba74r"))))
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
    (version "2.30.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "wavClusteR" version))
       (sha256
        (base32
         "04di095i9i19j9ppx8gdsk7n18vd02d4rjdi9d4a3p0xv05ihnb6"))))
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
    (version "1.32.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "VariantFiltering" version))
       (sha256
        (base32
         "1bjqn8qik221x0bqvgd99p87v45iihwp6cxckh4ks964pd0c1xk8"))))
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

(define-public r-genomegraphs
  (package
    (name "r-genomegraphs")
    (version "1.46.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "GenomeGraphs" version))
       (sha256
        (base32
         "05vavhz936v7cknig2f2mn3fd9fiy54r3swlvifpawramblp1ags"))))
    (properties `((upstream-name . "GenomeGraphs")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biomart))
    (home-page "https://bioconductor.org/packages/GenomeGraphs/")
    (synopsis "Plotting genomic information from Ensembl")
    (description
     "Genomic data analyses requires integrated visualization of known genomic
information and new experimental data.  GenomeGraphs uses the biomaRt package
to perform live annotation queries to Ensembl and translates this to e.g.
gene/transcript structures in viewports of the grid graphics package.  This
results in genomic information plotted together with your data.  Another
strength of GenomeGraphs is to plot different data types such as array CGH,
gene expression, sequencing and other data, together in one plot using the
same genome coordinate system.")
    (license license:artistic2.0)))

(define-public r-wavetiling
  (package
    (name "r-wavetiling")
    (version "1.28.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "waveTiling" version))
       (sha256
        (base32
         "0d7l559zlmly8mncmh1zhkqmsml0bwwfpm7ccp8l26y852vwf7hf"))))
    (properties `((upstream-name . "waveTiling")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-affy
           r-biobase
           r-biostrings
           r-genomegraphs
           r-genomicranges
           r-iranges
           r-oligo
           r-oligoclasses
           r-preprocesscore
           r-waveslim))
    (home-page "https://r-forge.r-project.org/projects/wavetiling/")
    (synopsis "Wavelet-based models for tiling array transcriptome analysis")
    (description
     "This package is designed to conduct transcriptome analysis for tiling
arrays based on fast wavelet-based functional models.")
    (license license:gpl2+)))

(define-public r-variancepartition
  (package
    (name "r-variancepartition")
    (version "1.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "variancePartition" version))
       (sha256
        (base32
         "0wk1xql8b0gxyrqz9hs54xvmp7qdw9b8jnv88p4vgv061iwyk7wv"))))
    (properties
     `((upstream-name . "variancePartition")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-aod
           r-biobase
           r-biocparallel
           r-doparallel
           r-foreach
           r-ggplot2
           r-gplots
           r-iterators
           r-limma
           r-lme4
           r-lmertest
           r-mass
           r-matrix
           r-pbkrtest
           r-progress
           r-rdpack
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
    (version "1.50.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "HTqPCR" version))
       (sha256
        (base32
         "0am98rzwpi3kybq1l27c5qn3n1pg5aqwmh6jq9q0lzbjjin3haqc"))))
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

(define-public r-unifiedwmwqpcr
  (package
    (name "r-unifiedwmwqpcr")
    (version "1.32.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "unifiedWMWqPCR" version))
       (sha256
        (base32
         "1skfs94a6bv05c844zf5vfqw1fbgxyppgdnckdbhxg2a2470a4wh"))))
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
    (version "1.14.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "universalmotif" version))
       (sha256
        (base32
         "1sm54z8aq3534qjsa19wychhwcvwnjlkydmiqqvidiiwcxwqpjsr"))))
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
    (version "1.14.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "ACE" version))
              (sha256
               (base32
                "1xnw288vz810vjkidar5h218wyc0q2hx0k4zi3r88vaz5rfhc05m"))))
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
    (version "1.74.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "aCGH" version))
              (sha256
               (base32
                "00ni0kwy68v33ggfi8g5vffirhmbhaxg4l54hcqhx75m535z1x7d"))))
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
    (version "2.52.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "ACME" version))
              (sha256
               (base32
                "0ilcsgpc4m47gifxc0yzx2xi3g4day515mncnnjvfdj3iq8xwk25"))))
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
    (version "1.26.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "acde" version))
              (sha256
               (base32
                "0lgq546y4qklfzbc6fjr3d656hn76p6dn4694qfiafql2nlsjbj2"))))
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
    (version "1.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ActiveDriverWGS" version))
       (sha256
        (base32
         "13b5yazgv9kckcp6gck183mh1m0q8lc5ixagmcy9s8kv2wz7wq45"))))
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
    (version "1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ActivePathways" version))
       (sha256
        (base32
         "1crq164vyqhdq5y3q09k3m5zljqrhcd5ak0jrc0xqvzf0pasml2m"))))
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
    (version "1.56.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BGmix" version))
       (sha256
        (base32
         "03f6nknp3n49yvg2d9qsmds676rva70pr4wjz0md228jczgjk0vj"))))
    (properties `((upstream-name . "BGmix")))
    (build-system r-build-system)
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
    (version "1.62.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "bgx" version))
       (sha256
        (base32
         "0q2y4n6bcc9pvz5sgfkw1lrb00rrp7q29i1vh7srdfmfhgpyz6bk"))))
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
    (version "1.48.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BHC" version))
       (sha256
        (base32
         "1kqajd16981y5yaak2imaq1i7pilgqdr3nbhggsakh787j1d9rc5"))))
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
    (version "1.54.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BicARE" version))
       (sha256
        (base32
         "0qjh5bsjcjry6k1vzdaascwy2shjrkc2bw0w57w0qa458cbi89z2"))))
    (properties `((upstream-name . "BicARE")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biobase r-gseabase r-multtest))
    (home-page "http://bioinfo.curie.fr")
    (synopsis "Biclustering analysis and results exploration")
    (description
     "This is a package for biclustering analysis and exploration of
results.")
    (license license:gpl2)))

(define-public r-bifet
  (package
    (name "r-bifet")
    (version "1.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BiFET" version))
       (sha256
        (base32
         "03ypbbn8i0f4bl4m6wfdcv702jydniak56wqjb1vlrckd9aphwzq"))))
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
    (version "2.54.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "rsbml" version))
       (sha256
        (base32
         "1v11pspkml6xdsacgwxw8r4qdhbnn2h2sqgpm9aidaq9p2085b0v"))))
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

(define-public r-hypergraph
  (package
    (name "r-hypergraph")
    (version "1.68.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "hypergraph" version))
       (sha256
        (base32
         "0xmryqj5yw1ns6wbhjqbb6h14jlj89zrznzvqnvd4n03ci20kzzp"))))
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
    (version "1.48.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "hyperdraw" version))
       (sha256
        (base32
         "0ndw4y6b15jy4w86vfkahmdc81d3ycjsvqy1mxi55dwvd8xq0ap6"))))
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
    (version "1.32.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BiGGR" version))
       (sha256
        (base32
         "0n57bgl6xz5b1gpw4isimq2pqxmlabn7jzhbjg2fbxcklabdvrcw"))))
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
    (version "1.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "bigPint" version))
       (sha256
        (base32
         "1hp69j2qcidrxqs3dxjjngb09nbzp5x2yy4jz1rjmv6ghif9ccfj"))))
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

(define-public r-chemminer
  (package
    (name "r-chemminer")
    (version "3.48.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ChemmineR" version))
       (sha256
        (base32
         "1nri4zkc9lp1mqgsi0h58486vixwiv2989b6pmx2aj5c3575i0ma"))))
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
    (version "1.38.0")
    (source
      (origin
        (method url-fetch)
        (uri (bioconductor-uri "fmcsR" version))
        (sha256
          (base32 "1mblmk21dxc9v2fikhvfg2njwgl190gkysppl6msxizwcmxsmh30"))))
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
    (version "1.34.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "bioassayR" version))
       (sha256
        (base32
         "0zbrci0vgk4qca28i0qb2izhyrz3r95l1w54h9h3zj9f3vd61wrz"))))
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
    (version "1.28.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "biobroom" version))
       (sha256
        (base32
         "04x1z9nchm4mbhqr31011zdprc4md156j4zf003s7xx0n278xsgh"))))
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
    (version "1.42.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "graphite" version))
       (sha256
        (base32
         "0nl5mkgrvf7vsqjy48ij9b1dmxfvp9lf8cpay55h93c4qz4x606g"))))
    (properties `((upstream-name . "graphite")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotationdbi r-graph r-httr r-rappdirs r-rlang))
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
    (version "1.40.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ReactomePA" version))
       (sha256
        (base32
         "1fd72m2afxbbvbgwy8knp6fiq1h561plmsh4r8a08w21ngmkz2s5"))))
    (properties `((upstream-name . "ReactomePA")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotationdbi
           r-dose
           r-enrichplot
           r-ggplot2
           r-ggraph
           r-graphite
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
    (version "2.60.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "EBarrays" version))
       (sha256
        (base32
         "027zarnpxpdnyl877swp5ypxj7zvq0cjp2q2xs6g6yn5dpqjvxxk"))))
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
    (version "1.32.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "BiocCheck" version))
              (sha256
               (base32
                "1k1gxzmxx26hmwdxgagv93mv4jwyygkk8703ds6nvryzhqffzkbc"))))
    (properties
     `((upstream-name . "BiocCheck")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-codetools
           r-graph
           r-httr
           r-knitr
           r-biocmanager
           r-biocviews
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
    (version "1.58.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "biocGraph" version))
       (sha256
        (base32
         "1y59a7c4ahhn1g2wz2hkx83izfn8i85mmxxp63jdd0rg7zwhr6nn"))))
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
    (version "2.24.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "BiocStyle" version))
              (sha256
               (base32
                "1nwiib201b9q1x19ihqjqr5jl0vnid8wfgpi8sa3y02bn722g5a5"))))
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
    (version "1.64.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "biocViews" version))
              (sha256
               (base32
                "1lahla53awdqiglfiygbxg5pkzfabym7n5abgyp1nvqsvsj0g126"))))
    (properties
     `((upstream-name . "biocViews")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biobase
           r-biocmanager
           r-graph
           r-rbgl
           r-rcurl
           r-xml
           r-runit))
    (home-page "https://bioconductor.org/packages/biocViews")
    (synopsis "Bioconductor package categorization helper")
    (description "The purpose of biocViews is to create HTML pages that
categorize packages in a Bioconductor package repository according to keywords,
also known as views, in a controlled vocabulary.")
    (license license:artistic2.0)))

(define-public r-experimenthub
  (package
    (name "r-experimenthub")
    (version "2.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ExperimentHub" version))
       (sha256
        (base32
         "11hna8vrm1az1zk7pw2dv0wh84sd0hw2bi55w40hqvs36csb7lkl"))))
    (properties `((upstream-name . "ExperimentHub")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotationhub
           r-biocfilecache
           r-biocgenerics
           r-biocmanager
           r-curl
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

(define-public r-grohmm
  (package
    (name "r-grohmm")
    (version "1.30.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "groHMM" version))
       (sha256
        (base32
         "0v2mk7xcy483w2nygpmyjp73kj3v5pkk1kf1wr41n33dxqlddqai"))))
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
    (version "1.22.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "MultiAssayExperiment" version))
       (sha256
        (base32
         "1wnp52l9vifxn1wzqgndzp9b6ih0s1cflxx1fhw32k32d05cw9q1"))))
    (properties
     `((upstream-name . "MultiAssayExperiment")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biobase
           r-biocgenerics
           r-genomicranges
           r-iranges
           r-s4vectors
           r-summarizedexperiment
           r-tidyr))
    (native-inputs
     (list r-knitr))
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
    (version "1.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BiocOncoTK" version))
       (sha256
        (base32
         "1alplszw84vqa1mvzp996f94s40scmh4qwbrqhg43hrnyvbnq7pi"))))
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
    (version "1.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BioCor" version))
       (sha256
        (base32
         "004mksswampwisljcdz6fswwbgdjdii3y86gjzib0gf8v4w7w4q3"))))
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
    (version "1.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BiocPkgTools" version))
       (sha256
        (base32
         "1v0824vmg49q9lh0igdyniryyknw6vmh462rn25kmg9hdna0w99h"))))
    (properties `((upstream-name . "BiocPkgTools")))
    (build-system r-build-system)
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
           r-rvest
           r-stringr
           r-tibble
           r-tidyr
           r-tidyselect
           r-xml2))
    (native-inputs
     (list r-knitr))
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
    (version "1.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BiocSet" version))
       (sha256
        (base32
         "1ghba7020inrdxlbrrgds9gjymjxjma2p89b9lgkjin89zalqglh"))))
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
    (version "1.22.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BiocWorkflowTools" version))
       (sha256
        (base32
         "1jj4icpkhrv9f6yx3vghkpdil1pfghf3yvc9756wmndvhs100r5l"))))
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
    (version "1.68.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "bioDist" version))
       (sha256
        (base32
         "04nrvrcvpj0sn8p2i8n3ggsl2s7r4na576174i7bn1sj21vr0yb0"))))
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
    (version "2.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "PCAtools" version))
       (sha256
        (base32
         "03s4dh008fys2rrcpzanc0892p63f6jyyvzc9m42jbi1dlkyx26v"))))
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
    (version "1.28.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "rGREAT" version))
       (sha256
        (base32
         "0px72r8mjimf5mxfwb9qz46kqpgjw5gaqq41hy0212ymjd8whaky"))))
    (properties `((upstream-name . "rGREAT")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-genomicranges r-getoptlong r-iranges r-rcurl r-rjson))
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
    (version "1.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "M3C" version))
       (sha256
        (base32
         "120gd7gkgc98d1l6hl2ij799b3jksdnga5iyb44ps9mbc79hl012"))))
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
    (version "1.68.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Icens" version))
       (sha256
        (base32
         "0jnbfv7js8bw0ginql90krrpk0p54whj9igw0zk3jc45jqvj2vyc"))))
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
    (version "1.1-0.8")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "interval" version))
       (sha256
        (base32
         "0g0k9nri19p3y3s70ic1w3i3sxq8fbsxaikd7c4d6afqzr8hk2nl"))))
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
    (version "1.5")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "FHtest" version))
       (sha256
        (base32
         "00mql2r4f5hxhdqf27q3x9s5rz2zzakx2myym97b1w1s7c5znl4q"))))
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
    (version "1.58.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "preprocessCore" version))
       (sha256
        (base32
         "1sqpp00hhv6gypflrjw8qpqyqgdcp29m86gmi1di1574x8casdkf"))))
    (properties
     `((upstream-name . "preprocessCore")))
    (build-system r-build-system)
    (home-page "https://github.com/bmbolstad/preprocessCore")
    (synopsis "Collection of pre-processing functions")
    (description
     "This package provides a library of core pre-processing and normalization
routines.")
    (license license:lgpl2.0+)))

(define-public r-s4vectors
  (package
    (name "r-s4vectors")
    (version "0.34.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "S4Vectors" version))
              (sha256
               (base32
                "0j3ybhzdhlhw8527nks3mjja28asjya2n0m0rjkh4bw66rkfys4k"))))
    (properties
     `((upstream-name . "S4Vectors")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocgenerics))
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
    (version "1.71")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "WGCNA" version))
       (sha256
        (base32
         "027pkc4pyn9bifqbjs05318gvlm06mffw016j50n59wfi2g39x91"))))
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
           r-rcpp
           r-survival
           r-matrixstats
           r-preprocesscore))
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
    (version "2.40.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Rgraphviz" version))
       (sha256
        (base32
         "1r6ff7w2bmyfl1vzjvpgnhb8f5arwjlpab8fw5ph8fgyiqbcx94l"))))
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

(define-public r-fithic
  (package
    (name "r-fithic")
    (version "1.22.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "FitHiC" version))
              (sha256
               (base32
                "0iv14yx3g7shzl8qrjknyxbmiylj51sbd1wzr1ff9lc5shgl55kq"))))
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
    (version "1.40.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "HiTC" version))
              (sha256
               (base32
                "1pkshlrra26cad0hf8a54brlkazni6rsvrplh36azxapx5rpps4s"))))
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
    (version "1.24.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "HDF5Array" version))
       (sha256
        (base32
         "1r1lg7k60qgb489xkypd4gvm1fmdlihvylb5va6xj58ipndbfday"))))
    (properties `((upstream-name . "HDF5Array")))
    (build-system r-build-system)
    (inputs
     (list zlib))
    (propagated-inputs
     (list r-biocgenerics
           r-delayedarray
           r-iranges
           r-matrix
           r-rhdf5
           r-rhdf5filters
           r-rhdf5lib
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
    (version "1.18.2")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Rhdf5lib" version))
       (sha256
        (base32
         "1jpb8h7c724yz51zjfqs90bsqxgmy1rry2ra9qamsgqpr2j9764g"))
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
                 (("@ZLIB_LIB@") "-lz")
                 (("@ZLIB_INCLUDE@") "")
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
    (version "2.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "beachmat" version))
       (sha256
        (base32
         "0fc6vvjjq1mfjfj2zqkap3rwvinnfqjs0cpk1447sspvd1rjya8c"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocgenerics r-delayedarray r-matrix r-rcpp))
    (native-inputs
     (list r-knitr))
    (home-page "https://bioconductor.org/packages/beachmat")
    (synopsis "Compiling Bioconductor to handle each matrix type")
    (description "This package provides a consistent C++ class interface for a
variety of commonly used matrix types, including sparse and HDF5-backed
matrices.")
    (license license:gpl3)))

;; This package includes files that have been taken from kentutils.  Some
;; parts of kentutils are not released under a free license, but this package
;; only uses files that are also found in the free parts of kentutils.
(define-public r-cner
  (package
    (name "r-cner")
    (version "1.32.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "CNEr" version))
       (sha256
        (base32 "05zvr5fv8nprxqh2wvvrlf737dq242i20p1rpyqjaxihl6xl62kq"))))
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
    (version "1.34.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "TFBSTools" version))
       (sha256
        (base32
         "0l6j1r2cx7jfd39qzbyynk4jvzd81ys6yypzxjc97js4kkyrx29w"))))
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
    (version "2.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "maftools" version))
       (sha256
        (base32 "1gqfi95v4fs64n4walij0g2kds3fbbwp6lih5yakmgf6kj8fpkk6"))))
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
    (version "1.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "motifmatchr" version))
       (sha256
        (base32
         "1ssn00mxwk23zr5na0vcmxvm69i68f0ga0wqlv1nk2isg0wpv878"))))
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
    (version "1.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "chromVAR" version))
       (sha256
        (base32 "0vhsvkm4kvln0002f13ayk57f9fmiz1kw9vwpsm1vds1vahd656m"))))
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
    (version "1.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "SingleCellExperiment" version))
       (sha256
        (base32
         "0s1aqbvlfnzijzfywjfpinqmxqj269dq2d3zlgf4xw9c1nwwnv7p"))))
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
    (version "1.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "SingleR" version))
       (sha256
        (base32 "0qbyc6ygw2xv3li9187i3axsw6ihwpa7pkvxvy9cagv7xck45c5y"))))
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
    (version "1.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "scuttle" version))
       (sha256
        (base32
         "0nnmq3wf436xaw4arc4y3ldvn6ilsg52xzbccmid0icb8z3y2kzn"))))
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
    (version "1.24.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "scater" version))
              (sha256
               (base32
                "0dqirggw7my5nq4ln9q0ya18ciqplkz9gx318ffias9ag3yii5rw"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-beachmat
           r-biocgenerics
           r-biocneighbors
           r-biocparallel
           r-biocsingular
           r-delayedarray
           r-delayedmatrixstats
           r-ggbeeswarm
           r-ggplot2
           r-ggrepel
           r-gridextra
           r-matrix
           r-rcolorbrewer
           r-rcppml
           r-rlang
           r-rtsne
           r-s4vectors
           r-scuttle
           r-singlecellexperiment
           r-summarizedexperiment
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
    (version "1.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "scran" version))
       (sha256
        (base32
         "0xg7dl35915a65pmzkxdacsm4iqf97ayljdjljcvqx1ycmn7x68w"))))
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

(define-public r-sparsematrixstats
  (package
    (name "r-sparsematrixstats")
    (version "1.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "sparseMatrixStats" version))
       (sha256
        (base32
         "0p12kay7p5zbfm2589wdx0n9jhgpf5fb2fsmkhn3p4ck4xcy13x2"))))
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

(define-public r-delayedmatrixstats
  (package
    (name "r-delayedmatrixstats")
    (version "1.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "DelayedMatrixStats" version))
       (sha256
        (base32
         "1qlwv69c0r2w3zkmsr8r7w6sr3hf1ha0sfcrsjx4ks8f0ww7aqsv"))))
    (properties
     `((upstream-name . "DelayedMatrixStats")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-delayedarray
           r-iranges
           r-matrix
           r-matrixgenerics
           r-matrixstats
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
    (version "1.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "MsCoreUtils" version))
       (sha256
        (base32
         "077x1zcy27x8akmagjn75j97082cgnahrbfw0qx08q455m5x3xzh"))))
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
    (version "1.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "MsFeatures" version))
       (sha256
        (base32 "111iqcq4q315pb4j8z427shin9b00p179m2s9h6dd7imvbd68yq3"))))
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
    (version "1.6.0")
    (source
      (origin
        (method url-fetch)
        (uri (bioconductor-uri "BiocIO" version))
        (sha256
          (base32
            "16j826w4zrmbgpmq6nyglcrjailsfv48ih1rz1qn383g7v503ydp"))))
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
    (version "1.34.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "msmsEDA" version))
       (sha256
        (base32
         "0jnaq9ar4mnf3pfhka9hvk61p51ny9jws49xi8z29dq288b42b42"))))
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
    (version "1.34.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "msmsTests" version))
       (sha256
        (base32
         "1wzdz0p9wmr243xkmymx9fwskafkyxgmlip4sd1fy2s06px7r0xi"))))
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
    (version "1.20.1")
    (source
      (origin
        (method url-fetch)
        (uri (bioconductor-uri "CATALYST" version))
        (sha256
          (base32
            "05vfqwa9qsm16px77s9bzygs6zymcxshymmpvz86a9l1cy1yxbza"))))
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
            r-magrittr
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
    (version "1.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "erma" version))
       (sha256
        (base32
         "1ilq01cr2ipxpmp422fikiz6nj4nasjhj0ikcagjn2zmmarpgi1b"))))
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
    (version "1.44.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ggbio" version))
       (sha256
        (base32
         "0iyhjalwq1jmldpn20iv8l2kmz6sm20ddry2yz2zn7yq0wszp3vg"))))
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
    (version "1.40.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Gviz" version))
       (sha256
        (base32
         "0as3sxhv21bqqrpvafcqim7798hhkzj3q40hy1rqyhv2lhj4rbvi"))))
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
    (version "2.28.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "gwascat" version))
       (sha256
        (base32
         "19ymdxj8966i4yk0zalfw23938cpv4q7pywg4qb242p44na5y9sl"))))
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

(define-public r-kegggraph
  (package
    (name "r-kegggraph")
    (version "1.56.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "KEGGgraph" version))
       (sha256
        (base32 "15pq040pcg8hr18xixmjp59xb7mgvygjv6kisqk8yv99l1611ndx"))))
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
    (version "1.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ldblock" version))
       (sha256
        (base32
         "08ss03b93czwb4x60hsi30ad4lmamvq5mxa8nj0g18z68qcraijm"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocgenerics
           r-ensdb-hsapiens-v75
           r-ensembldb
           r-genomeinfodb
           r-genomicfiles
           r-httr
           r-matrix
           r-rsamtools
           r-snpstats
           r-variantannotation))
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
    (version "2.7-1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "abn" version))
       (sha256
        (base32
         "1w3jns96m8b9scvaa4hcla3i88a0cfh9qis2l04yixvda5q91gpr"))))
    (build-system r-build-system)
    (inputs
     (list gsl))
    (propagated-inputs
     (list r-doparallel
           r-foreach
           r-graph
           r-lme4
           r-nnet
           r-rcpp
           r-rcpparmadillo
           r-rgraphviz
           r-rjags))
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

(define-public r-pathview
  (package
    (name "r-pathview")
    (version "1.36.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "pathview" version))
       (sha256
        (base32 "1472k107f21cflbx2fip92g8gl9wlwxgwfvgvl73ma0y0jzs0qdq"))))
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

(define-public r-snpstats
  (package
    (name "r-snpstats")
    (version "1.46.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "snpStats" version))
       (sha256
        (base32
         "0a5b5nqc7n965jk45ijwkzbn416ib4gfhp8xl39z8f2bdskip4a2"))))
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
    (version "1.22.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "chromstaR" version))
       (sha256
        (base32
         "1xjwmnr4hk8v3nwvhqd6ixk5qr2dv0n4mb9wd6nl7cgjfhjsdgj7"))))
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
    (version "2.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Guitar" version))
       (sha256
        (base32
         "09grsasnnk7rmlzjh4lhas9r5spzcsrvmdqj6fx1dk22sckcqahh"))))
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
    (version "2.28.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ballgown" version))
       (sha256
        (base32
         "0l8q3fymskxmsi5jcikzjz5xi66lpzgv7bjymir4izah2v68z708"))))
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
    (version "1.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "megadepth" version))
       (sha256
        (base32
         "0qq82dmd3drr2bhn51bgbc6ml40klfmmhj6wdj72n9ya6n60lwy8"))))
    (properties `((upstream-name . "megadepth")))
    (build-system r-build-system)
    (inputs (list megadepth))
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
    (version "2.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BEclear" version))
       (sha256
        (base32
         "0x43yfnmb2d782g3g52nqdfs90i3zrrlqz8qy3ybmgv5f8n92p15"))))
    (properties `((upstream-name . "BEclear")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-abind
           r-biocparallel
           r-data-table
           r-dixontest
           r-futile-logger
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
    (version "1.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BgeeCall" version))
       (sha256
        (base32
         "1g12cms66zb45p347h3b358vjhnq76galvwqwq86xby4hnwpdzkh"))))
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
    (version "2.22.3")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BgeeDB" version))
       (sha256
        (base32
         "1f6lrazaibbz21sqvj59rq6ps9m1riw2y0kyidbn29mxf4ibwh3k"))))
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
    (version "1.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "biobtreeR" version))
       (sha256
        (base32
         "0cx46hdqqm6mbj0vp4y86axv0qccd4sgk2jwwjvnqp5pynq9bbqa"))))
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
    (version "3.54.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "minet" version))
       (sha256
        (base32
         "0q6jw2jqkl9qynjpzaygz45c7dmx1l5y2d8s1illpcf87siawcam"))))
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
    (version "1.36.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "geNetClassifier" version))
       (sha256
        (base32
         "1kh7mp5h0n7yd1klcd7w4v7i3fh9pkmvgf7189wangfzbcsr4f70"))))
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
    (version "1.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "dir.expiry" version))
       (sha256
        (base32
         "098wzm8hlpy70c99k2sl4k8z2dllhw7rwdj8dhcskr7kaw71k3sq"))))
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

(define-public r-basilisk-utils
  (package
    (name "r-basilisk-utils")
    (version "1.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "basilisk.utils" version))
       (sha256
        (base32
         "1jnqv0rlljkq27rd4ixl763v335f2aanm4fzr386yc81fj4vnmhk"))))
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
    (version "1.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "basilisk" version))
       (sha256
        (base32
         "1p90wq8a9wrpqpgmcy4zgh5skdw65gg2gsb3lnx78zk9khq0yyzh"))))
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

(define-public r-biocthis
  (package
    (name "r-biocthis")
    (version "1.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "biocthis" version))
       (sha256
        (base32
         "1hdgjp00d2si3mr7m1d289i9wn7g927z6n8n27d5sm94lb91qln0"))))
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
    (version "1.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BiocDockerManager" version))
       (sha256
        (base32
         "0kl6r8ad728a8dvqx0safj7v5gj1rxxcdiw44jkr1pd5ddv0xbi6"))))
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
    (version "1.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "biodb" version))
       (sha256
        (base32
         "02i0n29bp9d9p1ibslxca5m37qsgny2hlgg7d364lf7kc6y2bqni"))))
    (properties `((upstream-name . "biodb")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocfilecache
           r-chk
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
    (version "1.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "biomformat" version))
       (sha256
        (base32
         "12wqjipxhngmlnrdmx329dqmkmy2wa4nkkrhwaqv2nwy90dncs9n"))))
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
    (version "1.70.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "MVCClass" version))
       (sha256
        (base32
         "0apcjlq4i2mg8mlfqgvlcsqkiy51whzid3nd0m830jff0ywgh47g"))))
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
    (version "1.64.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BioMVCClass" version))
       (sha256
        (base32
         "078pnyygbvbfxziqspfr1nn78w67xyb4qmiwc34czga5psblvfwz"))))
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
    (version "1.36.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "biomvRCNS" version))
       (sha256
        (base32
         "0i576g7pkivqaxff1pkb760mdpx8v9fh071aic1mwfnlfa7k87ln"))))
    (properties `((upstream-name . "biomvRCNS")))
    (build-system r-build-system)
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
    (version "1.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BioNERO" version))
       (sha256
        (base32
         "1nyzjbl0gcwvbj2nxfwykirikf8j3rsx5ny45bqjbcb4r23k65kj"))))
    (properties `((upstream-name . "BioNERO")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocparallel
           r-complexheatmap
           r-deseq2
           r-dynamictreecut
           r-genie3
           r-ggnetwork
           r-ggnewscale
           r-ggplot2
           r-ggpubr
           r-igraph
           r-intergraph
           r-matrixstats
           r-minet
           r-netrep
           r-networkd3
           r-rcolorbrewer
           r-reshape2
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
    (version "1.56.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BioNet" version))
       (sha256
        (base32
         "0kcw71j4nmdkn373wk21ak6h0v4gycivnfrklb72kk1qcmsy1wrm"))))
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
    (version "1.16.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BioNetStat" version))
       (sha256
        (base32
         "0zs6pymvxb95sji0rnnzaz3whj7hsvj2kl4n4gzj7w1q0prbfpb2"))))
    (properties `((upstream-name . "BioNetStat")))
    (build-system r-build-system)
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
     (list r-knitr))
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
    (version "1.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BioQC" version))
       (sha256
        (base32
         "0vb2nnzqvyv25pw8qshcmijravswafg0858pkgqjgiv7wsr2mn3m"))))
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
    (version "1.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BioTIP" version))
       (sha256
        (base32
         "1sihi5zy7mlabh3ix1wvdqz8ibfq1avl8bnxjhvxyzq40zbcklh6"))))
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
    (version "1.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "biotmle" version))
       (sha256
        (base32
         "1frig90krvfdk6nwpmslpj0pvligyzwzfwwci7hzwcmbglk5jj22"))))
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
    (version "1.32.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "bsseq" version))
       (sha256
        (base32
         "1jpfib2vb6hd7pgh3d33jgch24lba175zmbalwsbgvlmmyyf1ki5"))))
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

(define-public r-dmrseq
  (package
    (name "r-dmrseq")
    (version "1.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "dmrseq" version))
       (sha256
        (base32
         "1c99l62fi26bnbvpzrlsvvs722za0l5vfhddcrhzzzasabhccb4n"))))
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

(define-public r-omnipathr
  (package
    (name "r-omnipathr")
    (version "3.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "OmnipathR" version))
       (sha256
        (base32 "0vk0fv09j3ql78mzzhdxwxb2b83qqdz2qfd8wpp1vydmcx2vvgni"))))
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
           r-magrittr
           r-progress
           r-purrr
           r-rappdirs
           r-readr
           r-readxl
           r-rlang
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
    (version "1.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "biscuiteer" version))
       (sha256
        (base32
         "0y7vbdaafiga16yr0d22w1v4p0jmczndcar0r0km06f5y1b74amr"))))
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
    (version "2.24.3")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "TCGAbiolinks" version))
       (sha256
        (base32 "0visvfhzaf0p1rb5vjkmw1c91zfxpks8nl9nbl9xlnpm8lkmmkms"))))
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
    (version "1.4.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "tricycle" version))
              (sha256
               (base32
                "0bjkajcz6xcfak6071d0ihakrvgf7s0pmkn6vqkjd6yxbfld7zln"))))
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
    (version "1.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "tximeta" version))
       (sha256
        (base32
         "1vq7x1sf5h8iwdalalbrybxzbq47s2ymb75himj5wkv77mgcivfl"))))
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
    (version "1.40.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "phyloseq" version))
       (sha256
        (base32 "0hcyv4ziyaw74mc9vf7bad3q9izi9p0whg3hspbs6w8b3hp34y2k"))))
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
