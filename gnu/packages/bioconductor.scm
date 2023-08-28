;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015-2023 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016, 2017, 2018, 2020, 2021 Roel Janssen <roel@gnu.org>
;;; Copyright © 2016 Pjotr Prins <pjotr.guix@thebird.nl>
;;; Copyright © 2016 Ben Woodcroft <donttrustben@gmail.com>
;;; Copyright © 2017, 2022 Efraim Flashner <efraim@flashner.co.il>
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
  #:use-module (gnu packages java)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages netpbm)
  #:use-module (gnu packages python)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module ((srfi srfi-1) #:hide (zip)))


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

(define-public r-bcellviper
  (package
    (name "r-bcellviper")
    (version "1.34.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "bcellViper" version
                                     'experiment))
              (sha256
               (base32
                "1fpgh70x2r68v0ximgcdphnyzq2hgiwbamyhbac3yka8flhrd1fm"))))
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

(define-public r-breakpointrdata
  (package
    (name "r-breakpointrdata")
    (version "1.16.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "breakpointRdata" version 'experiment))
              (sha256
               (base32
                "0f23i4ynb4vgn22c3d2l64z92rzv3qnwd4j8qyvalklrxkwilhfn"))))
    (properties `((upstream-name . "breakpointRdata")))
    (build-system r-build-system)
    (native-inputs (list r-knitr))
    (home-page "https://github.com/daewoooo/breakpointRdata")
    (synopsis "Strand-seq data for demonstration purposes")
    (description
     "This package is a collection of Strand-seq data.  The main purpose is to
demonstrate functionalities of the @code{breakpointR} package.")
    (license license:expat)))

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

(define-public r-champdata
  (package
    (name "r-champdata")
    (version "2.30.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "ChAMPdata" version 'experiment))
              (sha256
               (base32
                "0rz762szfl02h4d3dj7ckd41ji9mdsja8nxqw6fl086z337041zw"))))
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

(define-public r-flowsorted-blood-450k
  (package
    (name "r-flowsorted-blood-450k")
    (version "1.36.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "FlowSorted.Blood.450k"
                                     version 'experiment))
              (sha256
               (base32
                "1ha9qsp5g3g2yhnk574x6xhg95bb29ywvmg3ns1c50z69v6wbraq"))))
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
    (version "2.2.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "FlowSorted.Blood.EPIC" version
                                     'experiment))
              (sha256
               (base32
                "1vybj69jxnirqg6ik03q3pb1vv23z8mir7wpi2ys7iljf5ixzgl1"))))
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

(define-public r-hdcytodata
  (package
    (name "r-hdcytodata")
    (version "1.18.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "HDCytoData" version 'experiment))
              (sha256
               (base32
                "1fn8q6ds10z3ymdarkfyh88pcqnrry9yhzammp84vf86f0bl8mrc"))))
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
    (version "1.34.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "Illumina450ProbeVariants.db"
                                     version 'experiment))
              (sha256
               (base32
                "1c1iqxi17s1a1sa1vab2ma7pjq1dxal7ibsiahj66ys0pa4sm42p"))))
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
    (version "2.36.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ITALICSData" version 'experiment))
       (sha256
        (base32 "09d2igic3b5p7wpq98hb2lffxm1nfq9mwmnqlbdn3jv49pgz3hmw"))))
    (properties `((upstream-name . "ITALICSData")))
    (build-system r-build-system)
    (home-page "http://bioinfo.curie.fr")
    (synopsis "ITALICS data")
    (description "This package provides data needed to use the ITALICS
package.")
    ;; Expanded from GPL
    (license (list license:gpl2+ license:gpl3+))))

(define-public r-macrophage
  (package
    (name "r-macrophage")
    (version "1.12.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "macrophage" version
                                     'experiment))
              (sha256
               (base32
                "0ml8v92w021fmzsn4yl90ap3l4l3b9c1pk8pzsrm122p82wzlyms"))))
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

(define-public r-mousegastrulationdata
  (package
    (name "r-mousegastrulationdata")
    (version "1.14.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "MouseGastrulationData" version
                                     'experiment))
              (sha256
               (base32
                "155kci5isq7mlbvv8mdl2jjpafm3pbv1likhls70aa746spr1h17"))))
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
    (version "0.44.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "minfiData" version 'experiment))
              (sha256
               (base32
                "15s3kc629m2c78vkidmp6kcc28sn1wzjzrxazmd8z7x8cdad3q4g"))))
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

(define-public r-msigdb
  (package
    (name "r-msigdb")
    (version "1.8.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "msigdb" version
                                     'experiment))
              (sha256
               (base32
                "1kgsqg1d5r852qas09g6fs0pcmk6jgbb6g983v6iqw19qsiy6jby"))))
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

(define-public r-tximportdata
  (package
    (name "r-tximportdata")
    (version "1.24.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "tximportData" version
                                     'experiment))
              (sha256
               (base32
                "0mgbwpybg2xd6x1ijrflmjh5w63qz6ylnzszbbyp437n618m7riy"))))
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
    (version "1.68.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "ABarray" version))
              (sha256
               (base32
                "0vrsyx06acdkb3hq350zdnx3bqzz43grf1w8n0pmxlcr2dncchv9"))))
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
    (version "1.54.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "ABSSeq" version))
              (sha256
               (base32
                "1my2slp88qpb15qidjd646hlslvn8brv6i553h21c4c76jzxzsiz"))))
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
    (version "2.40.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ADaCGH2" version))
       (sha256
        (base32 "1mmckrxhv23kl3g4d3n4hfdm97sfg2k36khzy9i6d73g7c033hsv"))))
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
           r-snapcgh
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
    (version "1.16.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "ADAM" version))
              (sha256
               (base32
                "1hy7xdf6v2fqggvc9rdl90gn0l6vfbmvb23c61i8q45s3qsaxksp"))))
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
    (version "1.16.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "ADAMgui" version))
              (sha256
               (base32
                "0jc0sbmmc98z75x73ns3qwcvklvx73wqla87cki8sls6ywa8w93j"))))
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
    (version "1.10.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "ADImpute" version))
              (sha256
               (base32
                "0a3r4bkf6g28lgsq21077ag9ba6zrfv6yflawx9fg73zdx4266jp"))))
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
    (version "1.70.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "adSplit" version))
              (sha256
               (base32
                "0nbdcrc2wiq3mkpayjslz66ik2vk3h3f18zg6df9xaivanxrkk5z"))))
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
    (version "1.18.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "AffiXcan" version))
              (sha256
               (base32
                "0nf5dkidar6zp5rvpd7i19gdb7mnam9rmca3z3anxi0pdnalwc9d"))))
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
    (version "1.52.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "affyILM" version))
       (sha256
        (base32 "02g0bg21ms0gmq6dvdcj91b0p47ppqcfvgj73y0mbdx6prrmcagx"))))
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
    (version "1.74.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "affylmGUI" version))
       (sha256
        (base32 "1m1gvz313h4x080889b05fjq60wn57dxlgq03qkwy65scl3x2fcn"))))
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
    (version "1.76.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "affyPLM" version))
       (sha256
        (base32 "1660nn4541f2k5qpzxkkkf9h92ndzqr0j1jpzh4czs466766kn8y"))))
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
    (version "1.46.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "AffyRNADegradation" version))
              (sha256
               (base32
                "14s3kvxcc3qj931bf5ya088flijmn5z89hps3di98kkdsl3w6d4g"))))
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
    (version "1.48.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "AGDEX" version))
              (sha256
               (base32
                "0p9qmwhi4ik24m51cvgxnny4yfqv4v0rvra16bj5d3w9bw9yf3an"))))
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
    (version "1.10.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "aggregateBioVar" version))
              (sha256
               (base32
                "0g7n6dj1scad833y2182946hn3yh4jad20wvw9ic8gmqb438vihj"))))
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
    (version "3.32.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "agilp" version))
              (sha256
               (base32
                "0rm646iqc8hf8vfk0pdg064hm9k4k371bp1q4k7z2l16zhs8a07r"))))
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
    (version "1.16.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "adductomicsR" version))
              (sha256
               (base32
                "17cm65n2awbwmk5i8h1n30abk6pww42ngcy9m24dxz1qdbnvx5fn"))))
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
    (version "2.50.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "AgiMicroRna" version))
              (sha256
               (base32
                "1gydc2sy0lf2h83dzr60w9k3ipqd8h62q6764xpn31girwx70rdz"))))
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
    (version "1.32.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "AIMS" version))
              (sha256
               (base32
                "1mbwv70ypkb3x086css94m89wq9pqzd23i7nar1844vbqpw3j83q"))))
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
    (version "1.8.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "airpart" version))
              (sha256
               (base32
                "024vcxf23irlysc8srqii3zqhhldpwdcj0i4zzhz18x3bdy9cbj0"))))
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

(define-public r-amountain
  (package
    (name "r-amountain")
    (version "1.26.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "AMOUNTAIN" version))
              (sha256
               (base32
                "12ml67882lscv05np4m80fg9d48dwkaa6kx5cga6x19kdx6xs2cj"))))
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
    (version "1.22.1")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "amplican" version))
              (sha256
               (base32
                "1bws4awpjvswzj53zwn9x5ra76ngpqn2h8hlr6y0x7j9wwwqldc7"))))
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
    (version "1.16.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "AMARETTO" version))
              (sha256
               (base32
                "1yp2npw9mdjy0wchbp0y1r1ifyy63hdz2y3y8cia9c76nfv4627f"))))
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
    (version "2.24.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "Anaquin" version))
              (sha256
               (base32
                "0f2xc0pm7ld72fnmqirr0q2a5xfh12cag6s2yysblslh9ajyzcmw"))))
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
    (version "2.2.1")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "ANCOMBC" version))
              (sha256
               (base32
                "05gngz6cqihxg4zlf7ymw93qj61a1i19hgp4fkc0cxnkq0pambrd"))))
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
    (version "1.16.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "animalcules" version))
              (sha256
               (base32
                "1z46lpd2zi3b88i27qm48wy4acb2xsjh63zgxhfx2y4bgvr29ni7"))))
    (properties `((upstream-name . "animalcules")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-ape
           r-assertthat
           r-biomformat
           r-caret
           r-covr
           r-deseq2
           r-dplyr
           r-dt
           r-forcats
           r-ggplot2
           r-glmnet
           r-gunifrac
           r-lattice
           r-limma
           r-magrittr
           r-matrix
           r-multiassayexperiment
           r-plotly
           r-plotroc
           r-rentrez
           r-reshape2
           r-s4vectors
           r-scales
           r-shiny
           r-shinyjs
           r-summarizedexperiment
           r-tibble
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
    (version "1.30.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "AnnotationHubData" version))
       (sha256
        (base32 "109gi5cs82j135q00wg9dfx0z5m9bs1kc7s246ym13ik43z0h3qq"))))
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
    (version "1.12.3")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "AnVIL" version))
              (sha256
               (base32
                "0dx10gcch6csk8nw3ffz4yvn5jf0v80ynsp3dg1az0ybkqyrzbih"))))
    (properties `((upstream-name . "AnVIL")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocmanager
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
           r-tidyselect))
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
    (version "1.32.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "ALDEx2" version))
              (sha256
               (base32
                "0gdkc8qwx3vpk5i09znlfrag7gk87piz61z90k96v6bm6x8sclb8"))))
    (properties `((upstream-name . "ALDEx2")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocparallel
           r-genomicranges
           r-iranges
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
    (version "1.16.1")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "alevinQC" version))
              (sha256
               (base32
                "137bvqyh1cqmhf9x3xl6n1dv0380lpcr2nxhd60b7zqiw4p14i5a"))))
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
    (version "1.14.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "AlphaBeta" version))
              (sha256
               (base32
                "11431d453xszzjyxr10npnblhlrfw8hl5jgabpxla7cj77w02wnr"))))
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
    (version "4.2.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "AlpsNMR" version))
              (sha256
               (base32
                "0rp82y7163pjbl7n6fyywh4l0sgzn1z8kp83v0kg7xk3810mj9sm"))))
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
    (version "2.62.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "altcdfenvs" version))
              (sha256
               (base32
                "1jpimj195rdw86pp623ylrmcxkxmn8qmc84vsw9prnf3gdz3q18b"))))
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
    (version "1.28.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "AneuFinder" version))
              (sha256
               (base32
                "1l33yb20pynkvlla1dmgbwjhnhxh067fci0ciryxmbzqwq2sn1kc"))))
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
    (version "1.22.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "ANF" version))
              (sha256
               (base32
                "08vkkfccfq8j4hanxsmjx5657kkw4qcp46qfhqvp1sd6wym69wzw"))))
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
    (version "1.42.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "annmap" version))
              (sha256
               (base32
                "0gilb8yxba1cqi678w9xkq77m8lf3k8dqcldh68cll61xsgihl1v"))))
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
    (version "1.40.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "antiProfiles" version))
              (sha256
               (base32
                "0ix0r36fs6vr0gyryi1j15pcc0hvpynsg9505w95dsn3p4fdnhg8"))))
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
    (version "1.60.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "ArrayExpress" version))
              (sha256
               (base32
                "1ib33fw379sakk084csa3pwcc3wvba38ily6mmv2ax1wh16i0pfz"))))
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
    (version "3.56.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "arrayQualityMetrics" version))
              (sha256
               (base32
                "06plgmgla7hvryqfcid4y35s24r50s39d9f8sjchwbxqciwy72wj"))))
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

(define-public r-asafe
  (package
    (name "r-asafe")
    (version "1.26.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "ASAFE" version))
              (sha256
               (base32
                "05xw42552x7by63psb05hvjraax66flg94i331ca61zx107q7fai"))))
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
    (version "1.44.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "ASEB" version))
              (sha256
               (base32
                "1dq6b5rg9iw6hdjmd7g0w64z7cxm52yg1cjyv355qs064dx26nph"))))
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
    (version "1.34.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "ASGSCA" version))
              (sha256
               (base32
                "0vb37rvrb08qvv0i2k9jnqpajzpj044ww05w3kq1kypbby0c84zs"))))
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
    (version "2.16.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "ASICS" version))
              (sha256
               (base32
                "0svl9wnfn9z88j1yyl30b1f3d14h01x4cga8q04x530723xnjws7"))))
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
    (version "2.10.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "ASpli" version))
              (sha256
               (base32
                "0462hf7anpmmqq4585kmjilw1q229r38lijbxq8xg0f3m8xvr1ga"))))
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
    (version "1.18.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "AssessORF" version))
              (sha256
               (base32
                "16nv6sh7z3l6ff3a7zrixkc04mhrak38q045xlirgljkcragz1c5"))))
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
    (version "2.18.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "ASSET" version))
              (sha256
               (base32
                "1vksbfd4wd2xcc0nl3wp53vm0jyqy97p0hqps2aml04745vkg023"))))
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
    (version "1.6.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "atena" version))
              (sha256
               (base32
                "1057mkxrjfnqhb1mydhd7vlb1s8h7n4zc4cl063gw4sgj6mw99ij"))))
    (properties `((upstream-name . "atena")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-annotationhub
           r-biocgenerics
           r-biocparallel
           r-genomeinfodb
           r-genomicalignments
           r-genomicranges
           r-iranges
           r-matrix
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
    (version "1.16.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "atSNP" version))
              (sha256
               (base32
                "14j8lbry4wpn8izch1nra5npg4qsh49ql4bf21jvvr3gl3qa1g7l"))))
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
    (version "1.52.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "attract" version))
              (sha256
               (base32
                "1lb6npxm5f82z8278v6fh7k5w8d4z73881iplm3ashc27wiw6sz3"))))
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
    (version "1.14.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "AWFisher" version))
              (sha256
               (base32
                "1c6rr1z1rhvn8w1kb3nnjlfacfr22vwm1rsa1xqm2hmghs01bq4x"))))
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
    (version "1.8.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "awst" version))
              (sha256
               (base32
                "0nwpfxfrx9rw4vl02vr311ivmmk96ajlwyhwms642hjv74j2yiji"))))
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
    (version "1.26.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "BaalChIP" version))
              (sha256
               (base32
                "0bp8p7cn59iv08cf4yw9xl1f83dcr6v1kqvggxjals8y4gmls9nz"))))
    (properties `((upstream-name . "BaalChIP")))
    (build-system r-build-system)
    (inputs (list perl)) ; extra/get.overlaps.v2_chrXY.perl
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

(define-public r-basespacer
  (package
    (name "r-basespacer")
    (version "1.44.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "BaseSpaceR" version))
              (sha256
               (base32
                "15aqf2s51gl6gcnv24170v9hlq1vgya58qh1f5vjmark7j2k8vvm"))))
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
    (version "1.38.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "BADER" version))
              (sha256
               (base32
                "0g3yvgrarpdcxpvjrxg0gbdcagknh80cr0xyzinzpmiiz0rywmzc"))))
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
    (version "1.28.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "BadRegionFinder" version))
              (sha256
               (base32
                "04bzb3i461gwkq9ygkjljpjk32c3arqr08hfzxyig1sarrryzl3q"))))
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
    (version "3.2.5")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "bambu" version))
              (sha256
               (base32
                "1vqxmz2lknnx1g61y7skvznsnxv3clajngz9mnggg1z3p5mr6cnh"))))
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
    (version "1.16.1")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "BANDITS" version))
              (sha256
               (base32
                "04l6iy0sij7zhzswhxjzir3xmrjnp6aapp0a93xp01xiv2sgdzni"))))
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
    (version "1.24.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "banocc" version))
              (sha256
               (base32
                "0yi70y87isb34jc1wrnz4gr0d0f2zw44555s50j3qdnj1x8cld9y"))))
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
    (version "1.8.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "barcodetrackR" version))
              (sha256
               (base32
                "155lxqqq2mgrz9i04xvv8y7gh2iacw6qd3mzijzbvrbm605qb3p8"))))
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
    (version "3.17.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BiocVersion" version))
       (sha256
        (base32
         "0n29adnb62agp9li8rmn68z653d2m41iy9zjz75h43a05drlgp33"))))
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
    (version "0.46.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "BiocGenerics" version))
              (sha256
               (base32
                "19kwpis282i8x31xlh8nc3z6vvn23p3wpx7wmrqhclf8ymq61c7z"))))
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
    (version "1.18.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "breakpointR" version))
              (sha256
               (base32
                "001n99lilymgqxmpi4v89gw60j1mx13rvppv1ff1pbnk1zcmg53n"))))
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
    (version "1.2.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "cardelino" version))
              (sha256
               (base32
                "0ivhqd3da23iy2qklk2nljkjjr943m9r2y1q51fphld33izmv64v"))))
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
    (version "1.8.1")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "CelliD" version))
              (sha256
               (base32
                "0vigvqjrlqbi5kviaj8qvyq3v8afgbc5pjrz7zmx2ckf4hdp0g03"))))
    (properties `((upstream-name . "CelliD")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-data-table
           r-biocparallel
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
           r-seurat
           r-stringr
           r-tictoc
	   r-singlecellexperiment
           r-summarizedexperiment
           r-umap))
    (native-inputs (list r-knitr r-scater))
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

(define-public r-coverageview
  (package
    (name "r-coverageview")
    (version "1.38.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "CoverageView" version))
              (sha256
               (base32
                "1ci07y75nvvv1mxy23v6jp5xb6n023fvlqh9il5lsdqwyfl5nz9s"))))
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
   (version "2.42.0")
   (source (origin
             (method url-fetch)
             (uri (bioconductor-uri "cummeRbund" version))
             (sha256
              (base32
               "1xin1azby96xxp4yyd4wc3rfcjc2g9p01ksfd89bk2vxcclccqxn"))))
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
    (version "1.72.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "daMA" version))
              (sha256
               (base32
                "0z4y57hna312pkhkchair8calm7wl40rx8lcm8i6h1789hba063d"))))
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
    (version "1.12.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "DAMEfinder" version))
              (sha256
               (base32
                "0m8g1sp55mxa0qswpqkzk73myhhy3s49c21hf97sk0lxis5lagcd"))))
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
    (version "1.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "dearseq" version))
       (sha256
        (base32
         "12ld1f3892ag1a3lmkwjlkk6pd79ibykg8jrmddx2x33k23cv67g"))))
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
    (version "1.18.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "debCAM" version))
              (sha256
               (base32
                "11vqfkyd3fklc8fhn850kklph8x4pmwclb9xbqji4i21222m89hh"))
              (snippet
               '(for-each delete-file
                          '("inst/java/CornerDetect.jar"
                            "inst/java/lib/pj20150107.jar")))))
    (properties `((upstream-name . "debCAM")))
    (build-system r-build-system)
    (arguments
     (list
      ;; XXX: since the upgrade to R 4.3.0 this package takes too long to be
      ;; loaded.
      #:tests? #false
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
     (list (list icedtea "jdk")
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
     (list r-knitr zip))
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
    (version "2.28.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "DECIPHER" version))
              (sha256
               (base32
                "16z6yk8rr2115z6g1l7fl01binxm29vnxsnsm2wzfvc5vv49927n"))))
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
    (version "2.16.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "decompTumor2Sig" version))
              (sha256
               (base32
                "028sczy1d108i05ymr1wpj6jdrcds476wbmmc7rzzflzyg4aix75"))))
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
    (version "1.42.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "DeconRNASeq" version))
              (sha256
               (base32
                "0bmkyci31p7g097i8fvc0s1fz47hv6vp5rcfqqkvclm86wfkkmkc"))))
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
    (version "1.20.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "decontam" version))
              (sha256
               (base32
                "08niwixy4m6gqmazisxzbbla9nsxicpa685jy3r6knapwaznvcb2"))))
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
    (version "1.6.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "deconvR" version))
              (sha256
               (base32
                "1r5js5prwy1libnf1g1a4pdi15pj216bb8ajhzii3symn5r0cdj9"))))
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
    (version "2.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "decoupleR" version))
       (sha256
        (base32 "012d76jwgg6fwfjc00zdws59y6jbj0grzd4lgjrqs2afp2ycmh9s"))))
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
    (version "1.46.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "deepSNV" version))
              (sha256
               (base32
                "11cnyy3hyn8akhmax25293mx2blcs8ba5vfax6mx6hjhb577hkwb"))))
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
    (version "1.36.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "DEGreport" version))
              (sha256
               (base32
                "15xm1l2qgsyzaw820a1fq5qdzh5pj4dmr1hx6s6b6wm2p02cvvai"))
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
    (version "0.26.6")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "DelayedArray" version))
              (sha256
               (base32
                "0jy2spqysa5x1s84kbr1jbbdmlh3q44lrw1qck2fln3b6q5vgz0k"))))
    (properties
     `((upstream-name . "DelayedArray")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocgenerics
           r-iranges
           r-matrix
           r-matrixgenerics
           r-s4arrays
           r-s4vectors))
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
    (version "1.10.2")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "densvis" version))
              (sha256
               (base32
                "0ypwnxi7ilczcvf5lwvyh6ck0jpvn1x90aggq66yqnn6yjy5md8s"))))
    (properties `((upstream-name . "densvis")))
    (build-system r-build-system)
    (propagated-inputs (list r-assertthat r-basilisk r-irlba r-rcpp
                             r-reticulate))
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

(define-public r-derfinderhelper
  (package
    (name "r-derfinderhelper")
    (version "1.34.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "derfinderHelper" version))
       (sha256
        (base32 "0ww4lvwmm8rf44wgksg98bqh7zlm503c4gc8dwilb1w0dz2k7qll"))))
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
    (version "2.14.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "DMRcate" version))
              (sha256
               (base32
                "1kcp0hgxnrcslphdqb25jsicif52r1sk8c7sn73jkdxy4n64idbm"))))
    (properties `((upstream-name . "DMRcate")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-bsseq
           r-dss
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
    (version "1.28.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "DRIMSeq" version))
       (sha256
        (base32 "1i9lcp31g94mllkwcyfablj27qr8v7vqk9j96ywrdfpj8dmcrw98"))))
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
    (version "1.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "DropletUtils" version))
       (sha256
        (base32 "09xwfb4ihpsp465vb1zbcwm6ww6qi3spn9d8p4i1gczyc0p9pf1y"))))
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

(define-public r-dss
  (package
    (name "r-dss")
    (version "2.48.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "DSS" version))
              (sha256
               (base32
                "1aslbzwqfp609ym98k6cjjf6r1ns9hxcpx4igfjhqf2v0pazz2q8"))))
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
    (version "1.78.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "DynDoc" version))
              (sha256
               (base32
                "16cb4pby6ja0xy8ygbgr6zfbyp4agyhlds5sayc5ryq50vafykah"))))
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
   (version "1.10.0")
   (source (origin
            (method url-fetch)
            (uri (bioconductor-uri "bluster" version))
            (sha256
             (base32
              "15f103lpl686730vl8g69fhki9iq2ncgr5dm6qm2xc9bq9vh65s9"))))
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
    (version "1.36.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "IdeoViz" version))
              (sha256
               (base32
                "1za1cxf734mar9hbvqrijdg61g0jag9jdzmi8p8ka62xbikxqa3s"))))
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
    (version "1.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "infercnv" version))
       (sha256
        (base32
         "19fk4pzhwj9pfh4vg943xb4cdfpq65ax75d61w1mm3f71gg56z5g"))))
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
           r-singlecellexperiment
           r-seurat
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
    (version "2.34.1")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "IRanges" version))
              (sha256
               (base32
                "013a3vcw1v5vn0sg2d9cwrdksch48kilvxp8cr79y0nr4vk58q9z"))))
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
    (version "2.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "IsoformSwitchAnalyzeR" version))
       (sha256
        (base32 "1zjwhxlayz2sb77vspw280didhawj282i5gvxnydcdparg165zwf"))))
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
    (version "2.60.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ITALICS" version))
       (sha256
        (base32 "09yia158mrv9r32iw88jlbfd6l7hzmglv8ni7i28x18qvnhp2a08"))))
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
    (version "1.10.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "Nebulosa" version))
              (sha256
               (base32
                "0lqm9mfmaxdhhs9di2kjg2rixng78lrrikyp7blmpyqk4c41j3nh"))))
    (properties `((upstream-name . "Nebulosa")))
    (build-system r-build-system)
    (propagated-inputs (list r-ggplot2
                             r-ks
                             r-matrix
                             r-patchwork
                             r-seurat
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

;; This is a CRAN package, but it depends on Bioconductor packages.
(define-public r-nmf
  (package
    (name "r-nmf")
    (version "0.26")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "NMF" version))
       (sha256
        (base32
         "1h1fpjnj6vjvi9ygxpfxs8k5bhly0aflr54zj88khgzkylp5ci4d"))))
    (properties `((upstream-name . "NMF")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-cluster
           r-codetools
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
           r-rcolorbrewer
           r-registry
           r-reshape2
           r-rngtools
           r-stringr))
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
    (version "1.78.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "affy" version))
       (sha256
        (base32
         "1xj8pnaa782k1hxaiba6mcsqr21bk8xz31916836jz5l9848zjsw"))))
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
    (version "1.76.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "affycomp" version))
       (sha256
        (base32
         "0wccj8q8sl0zc68nr5qf1qih8awqf1h6bwi5hq9d7r59p4g4r9n1"))))
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
    (version "1.58.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "affyContam" version))
       (sha256
        (base32
         "07a6kd51rlj433yxmyd2j8nl8qn1hw8wv3mk270dp8xxlk613y3p"))))
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
    (version "1.72.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "affycoretools" version))
       (sha256
        (base32
         "1h065fhw0n2ga74csyhqbj4qfhkg058pqa684ixcski4rhsd7hcf"))))
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
    (version "1.70.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "affyio" version))
       (sha256
        (base32
         "1r1c0iwix0n8r433x4bv8hvzhkablc4xkdgg2b1fk4ijhd1iwf3c"))))
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
    (version "1.72.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "affxparser" version))
       (sha256
        (base32
         "0n86my3bzy3g7xn60kizs5ym5m016zrkg7dv96kx246mrnv0ax8y"))))
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
    (version "1.78.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "annotate" version))
       (sha256
        (base32
         "1104r6jdy3nza8329xmq428ljfbfi2xwkd92sw519g0yqryh09vy"))))
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
    (version "1.62.2")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "AnnotationDbi" version))
              (sha256
               (base32
                "0vprm84k79pfnkkg9vf3gyb1nhzmin5lp5375rsaj6fnzbd46dw9"))))
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
    (version "1.24.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "AnnotationFilter" version))
              (sha256
               (base32
                "10jkxjmsshrr08c397qvkgrcfwzvrbd2hci1nal4vd5mm77f9cl9"))))
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
    (version "1.42.2")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "AnnotationForge" version))
       (sha256
        (base32
         "0b4dmjv7y50c1rn76wlhnlz93kidvg1byj72vq2s11kdzyq3pmss"))))
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
    (version "3.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "AnnotationHub" version))
       (sha256
        (base32
         "0ri8qj5j10bhprkb810c2hl5sl944kpb8rf5lab6nxykzgyz73v0"))))
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
    (version "3.30.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "aroma.light" version))
       (sha256
        (base32
         "118yqvbj4ad2b2bik0zi3ir15kxh9fbdi9qqyk52isvcdbv6069p"))))
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
    (version "1.32.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "bamsignals" version))
       (sha256
        (base32
         "18058j3fc47qcwvzgpb8mbc7cd1pywzrz3a36iqz93ihzjmymydk"))))
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
    (version "2.60.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "Biobase" version))
              (sha256
               (base32
                "1xjs5nxr3dffllp599hf0cx71a2czqmhf7zj2sp6rz06kcxib905"))))
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
    (version "2.56.1")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "biomaRt" version))
              (sha256
               (base32
                "0jqv2mv4ridi5lffva20a5s479bzpxhblyymricb17fd400rag8f"))))
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
    (version "1.0.4")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "biomartr" version))
              (sha256
               (base32
                "0hv4z6ycmn58ha7j7zfmyhvs2i37cm48gcalg19dli2kaw1c4210"))))
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
    (version "1.34.2")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "BiocParallel" version))
              (sha256
               (base32
                "0j0yi0g0zri0liy9xm2j3k848smhib5mmkvwcw6281iwnpn7yypq"))))
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
    (version "2.68.1")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "Biostrings" version))
              (sha256
               (base32
                "13cnjbq2iykv83ycb4151d7yys21s3v15fc72v3s02m1i92lqyq4"))))
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
    (version "1.48.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "biovizBase" version))
       (sha256
        (base32
         "0na37f2nfavgxd2xhprfb6bv68zm2h6glbkffspc3gsnw1q5h6da"))))
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
    (version "1.68.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "BSgenome" version))
              (sha256
               (base32
                "0fx2mkzyhbn8iayw14nr913wd4xhp76b1zjb85akrxaab2dm4zz8"))))
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
    (version "2.66.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Category" version))
       (sha256
        (base32
         "1wjf6xbnys7nv8z4xrzck1wk5xifkdni672kigs0aqdxqj8d80f5"))))
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

(define-public r-champ
  (package
    (name "r-champ")
    (version "2.30.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "ChAMP" version))
              (sha256
               (base32
                "1px2pm90lxwh0yn7h1kmmf94flhjs5p4i0iav7ya1xfybyg4w2zc"))))
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
    (version "1.36.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "ChIPseeker" version))
              (sha256
               (base32
                "0nngygci1g8bb3rsica348zi59hskr6hf8319csia6cman89gvqm"))))
    (build-system r-build-system)
    (native-inputs
     (list r-knitr))
    (propagated-inputs
     (list r-annotationdbi
           r-aplot
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
    (version "1.50.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "chipseq" version))
       (sha256
        (base32
         "0q83c4dm9qbdxlh4cbdzp357fn4sa16d7dqb4s65650pcnjg3a48"))))
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
    (version "2.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ComplexHeatmap" version))
       (sha256
        (base32
         "08jpbw7qns736zq1fwk9lyr4qm7rgzzw3hksv59zwb1458a79pzy"))))
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
    (version "1.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "conos" version))
       (sha256
        (base32 "1wdhb3jxh4id6xaghawzip8s264g9jxp4i5xy7jfhi67yfxszx6w"))))
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
    (version "1.40.2")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "DESeq2" version))
       (sha256
        (base32
         "0rb6b2aqn3an5ria4yjasjr7aldr5606rkc4yw275x9ddii22djg"))))
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
    (version "1.46.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "DEXSeq" version))
       (sha256
        (base32
         "0z4i3lsaziq3hprk64akx7jl6b757kmd8k6gv5pn36pavc6x1a0g"))))
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
    (version "1.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "diffcyt" version))
       (sha256
        (base32 "1vsn9z06vmfw6741x7dri3d9wh4rqhzak9r506y3c3h2gjjwilv1"))))
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
    (version "1.42.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "DirichletMultinomial" version))
       (sha256
        (base32
         "1vslg6hp498ar4l1b8rdxscz5wj1xci866j8975wr378lnymb942"))))
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
    (version "1.12.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "dittoSeq" version))
              (sha256
               (base32
                "0g5xxzd2xvh0m1fmyzzriwnrq1ckcmb95h9yl60h5w3c3ph22438"))))
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
    (version "2.34.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "EDASeq" version))
       (sha256
        (base32
         "0gf4k02b5qywrg5rpg6hs9gyzcdlpw36ndgz3ca19plicair4mih"))))
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
    (version "3.42.4")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "edgeR" version))
              (sha256
               (base32
                "1lyn017jqgn6d987zrk0kp2p2nw3mxf8zjspk31pky532p9pkhs3"))))
    (properties `((upstream-name . "edgeR")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-limma r-locfit r-rcpp))
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
    (version "1.18.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "EnhancedVolcano" version))
              (sha256
               (base32
                "1mcyhgdvfmkkh0rlhf51an7j1zi96lqdhifki2aqmlx0lqvg4qxq"))))
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
    (version "1.36.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "ENmix" version))
              (sha256
               (base32
                "1cwj45rnb6ra1bnd0plkjfvfsix25xr51yh6rwrfj524c35r70w1"))))
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
           r-matrixstats
           r-minfi
           r-preprocesscore
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
    (version "2.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ensembldb" version))
       (sha256
        (base32
         "0rl5ix755055i95y5xfb4zv5gih2gkqwqia1r1b3yx12z5ybvkc6"))))
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
    (version "1.46.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "fastseg" version))
       (sha256
        (base32
         "0gygyf1xq1z6vzand43n965p366zh0k3rv58gnyqa81whj4dvcq3"))))
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
    (version "2.50.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "gage" version))
       (sha256
        (base32
         "16wx2w70i12420022xz2zx8pizzyxb7z1pcmqn515cs89f1qcnli"))))
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
    (version "1.82.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "genefilter" version))
       (sha256
        (base32
         "1wpg8banz6s6smycfqf9hqryp4kf2gs069s7npyvpq53wy0kxyny"))))
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
    (version "1.36.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "GeneOverlap" version))
              (sha256
               (base32
                "1bimsayaf68j15im591r6rbvydhbmiqcp1pphd5zzg6hzp6va927"))))
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
    (version "1.32.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "genomation" version))
              (sha256
               (base32
                "0y6a2qxxp5mnkllcq46j00069z22ixnrbkd15s007a35lhkibz21"))))
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
    (version "1.36.1")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "GenomeInfoDb" version))
              (sha256
               (base32
                "1c3fgni846vjw152m4aklb8kwrwjw3rww116a2cbii70nr86p5qg"))))
    (properties
     `((upstream-name . "GenomeInfoDb")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocgenerics r-genomeinfodbdata r-iranges r-rcurl r-s4vectors))
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
    (version "1.36.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "GenomicAlignments" version))
              (sha256
               (base32
                "1irzcz9s97gqbyj9j71wxv2ikdfd8g14296fg33w2ykfdm80nzf0"))))
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
    (version "1.24.2")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "GenomicDataCommons" version))
              (sha256
               (base32
                "0dgvhi6nbc1qvrdwww2r39gxd2xmbadvy03lxh5nny9pyhhdlz3l"))))
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
    (version "1.52.1")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "GenomicFeatures" version))
              (sha256
               (base32
                "166l0pzg00kaagg3adnx1xy5bgmv42lm06a47i30lh14dc0k79wq"))))
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
    (version "1.36.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "GenomicFiles" version))
       (sha256
        (base32
         "0nxz8lvpmyllgs1ksgbha8qqs4fhppkg5casppnas9x47dj4bxph"))))
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
    (version "1.52.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "GenomicRanges" version))
              (sha256
               (base32
                "0vbr8nqn4pk0w3wx94q4f44psy6lxgx4b7p8hp0vf46flwggj2lj"))))
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
    (version "2.64.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "GLAD" version))
              (sha256
               (base32
                "0fi7wwn0ai4bwy8wgwl4dh3fbl6zhrkcy662dc1fcnk5i7rnv3y5"))))
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
    (version "4.18.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "GlobalAncova" version))
              (sha256
               (base32
                "1fld18rmi6rqm2nr549a4740w1iq3mf5df67v9ba6h42r4abs1ar"))))
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
    (version "5.54.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "globaltest" version))
              (sha256
               (base32
                "0jjhhg4vi44xqlvmcd80m30m93qyd4c79i7m9jn5hz3mw211xaig"))))
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
    (version "2.66.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "GOstats" version))
       (sha256
        (base32
         "1jjcjc1zp42l3gw36rr84al07ghih0cjx4p8gn8rlmv1jsz6kp1h"))))
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
    (version "1.62.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "GSEABase" version))
       (sha256
        (base32
         "0xfcgvsglcwkwsb1azmp4n4hx95fxnjk7rlwggb1f5rjw440qjnh"))))
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
    (version "1.48.2")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "GSVA" version))
              (sha256
               (base32
                "1y0dz9ayyrz4ylppa8f4m4b20yajzdl1sz4wpz8kcsfhh71ii64y"))))
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

(define-public r-harshlight
  (package
    (name "r-harshlight")
    (version "1.72.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "Harshlight" version))
              (sha256
               (base32
                "1rg3gx42a68jqbdmbqx7lh97y0rird1lci7146hnxa4svj827d5p"))))
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
    (version "1.42.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "hpar" version))
       (sha256
        (base32
         "1sg0mjada72a13xh3k6xsiaff7xj4mp76r6i8iab8nfkvng07p4i"))))
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

(define-public r-raggedexperiment
  (package
    (name "r-raggedexperiment")
    (version "1.24.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "RaggedExperiment" version))
              (sha256
               (base32
                "1xs7dz5mkh9zs078g2a0izij8vxrwhzh7gyjglp057gicsca0hal"))))
    (properties `((upstream-name . "RaggedExperiment")))
    (build-system r-build-system)
    (propagated-inputs (list r-biocgenerics
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

(define-public r-rhtslib
  (package
    (name "r-rhtslib")
    (version "2.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Rhtslib" version))
       (sha256
        (base32
         "1p8imns26knh21r254n7i9xvy8xzz3zizx0kq4si7cdaf2b4xayi"))))
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
    (version "1.74.1")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "impute" version))
              (sha256
               (base32
                "0nz8ggzk2g5w20yyhp6av6hy69kbmg3ncfpq9ppmq6p5a0msx92l"))))
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
    (version "1.38.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "interactiveDisplayBase" version))
       (sha256
        (base32
         "1p3lwk8g14srkhl3x3i0v2d9071251ldy9964mii11k1bp2g08am"))))
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
    (version "1.40.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "KEGGREST" version))
       (sha256
        (base32
         "1zmsk5w8i3cq0983nx1d24a6awrbq7aj2wrapsnizq9gvrsrhbbb"))))
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
    (version "2.0.11")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "lfa" version))
       (sha256
        (base32 "0x169fxwlccsqwj1bpviaky3hfr0zdwsdrlgfvrb4j6j95qfgnns"))))
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
    (version "3.56.2")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "limma" version))
              (sha256
               (base32
                "0miyba9frn1p4pkclzpr0bfazsk0br2jgpwpwwh773d3103hkn0r"))))
    (build-system r-build-system)
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
    (version "1.14.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Maaslin2" version))
       (sha256
        (base32 "06mb72hbzihdficv73yqbb2m86bkw78w3vbw1rm98n0npxq2fch6"))))
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
    (version "1.74.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "made4" version))
              (sha256
               (base32
                "1233l5160msxp85vm2h0j3k073vv5d9wj55709x4s657fi9la6ij"))))
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
    (version "1.76.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "makecdfenv" version))
              (sha256
               (base32
                "1a6kngwmc7nvzlgpnhdv4j180847gjh4i3dshld0pcvy3b6bjv8r"))))
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
    (version "1.72.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "MANOR" version))
       (sha256
        (base32 "0ili9xy7vvw17mz44lxadv1vkf3ahpks2l34qa9yfccpalwhzriw"))))
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
    (version "1.18.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "maser" version))
              (sha256
               (base32
                "1ma5d1i56gw8hm2pgvzzvmdd1cm2xb42rgf5lb6sf4v47yzlyb9f"))))
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

(define-public r-metagenomeseq
  (package
    (name "r-metagenomeseq")
    (version "1.42.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "metagenomeSeq" version))
       (sha256
        (base32 "0293c531l0sjz8qxy7nrajfl081v9df0af9242017msbvyszq6wk"))))
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
    (version "1.20.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "MetaNeighbor" version))
              (sha256
               (base32
                "151ip70la12kj80c0airi1yyjbj69gnkshd6kfk88wsvq7s179id"))))
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

(define-public r-methylkit
  (package
    (name "r-methylkit")
    (version "1.26.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "methylKit" version))
              (sha256
               (base32
                "1qi4gwlxsxr4cgs8s12qybrs89vr79xwd7n9in1af3hyn0swikn1"))))
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
    (version "2.60.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "Mfuzz" version))
              (sha256
               (base32
                "0rfna5ycwkx4xb2gv1wpfn7f7nz6f79w5nlkb7719l5730d81wnz"))))
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
    (version "1.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "MMUPHin" version))
       (sha256
        (base32 "14cn1mg9ffhzy1vgschl2h0wzah3c1g67q367f2m050a3z2i2ndy"))
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
    (version "1.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "muscat" version))
       (sha256
        (base32
         "1jcy07m8ywpgsmwkq6s7zj32w7gvxqj52j562irv2h2q429awdxk"))))
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
    (version "3.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "MutationalPatterns" version))
       (sha256
        (base32
         "1p9n5afnr4cx690w9d5q1hkwqffsbnsn72wibqcl5rbqgfsvhf8h"))))
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
    (version "1.32.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "msa" version))
              (sha256
               (base32
                "08fg4y8rmkvilsx2hxqwxa90p2wqrl4iyndp7wfjicvplmaxp708"))))
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
    (version "2.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "MSnbase" version))
       (sha256
        (base32
         "0qpgpvzg0vw45ia3dpkpfs56dj06crjrrg4i9l46bj8f2pgzcf0g"))))
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
    (version "1.34.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "MSnID" version))
       (sha256
        (base32
         "1mxlr76xrgp3limhr20naanhqr6b0d2wbk9l63j0pi2yckszxjjc"))))
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
    (version "1.38.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "mzID" version))
       (sha256
        (base32
         "0grl7798vnpxb6nw98zj0lbvsrhkjfsyn0y15bi4v7vhyhkk26xm"))))
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
    (version "2.34.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "mzR" version))
       (sha256
        (base32
         "1jsna4xwyph1gg72wwqlpavb65g5nc3db1vmcs1qcw1mdgasdjhk"))
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
    (version "1.28.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Organism.dplyr" version))
       (sha256
        (base32
         "0yrd7a7c0n6c987gyy76jidpvsdwrv2097f9wkvh4y5dd1w0mk7p"))))
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
    (version "1.42.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "OrganismDbi" version))
       (sha256
        (base32
         "1pw3qd6n0xf7vq3x73612bndcqhwrqkpi29f6crazdzjcy3fivlb"))))
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
    (version "1.30.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "Oscope" version))
              (sha256
               (base32
                "055j7y1icikqivc795hzl1l5v5ga2bfcqk2x0kivawbrqvj4yq5v"))))
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
    (version "2.26.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "pcaExplorer" version))
       (sha256
        (base32
         "04r4qsiasbfb0pwqyqmw2w3qdkhgwixwffp1s8hjflwnb63hxzjx"))))
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
    (version "1.92.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "pcaMethods" version))
       (sha256
        (base32
         "14awrgq56s948x74j4iv9k1yy5b7axw1kqzg827y5znjc1i3aqy6"))))
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
    (version "1.0.1")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "pfamAnalyzeR" version))
              (sha256
               (base32
                "0zff887lc4bjrv683kqsw47vjwmf6886wybklsf2wd6hpy23mxfy"))))
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
    (version "1.32.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ProtGenerics" version))
       (sha256
        (base32
         "0fgsb47qrvrl4y3yr094mas22rai5h5yl9mciadd6zcpamp2p01i"))))
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
    (version "1.76.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "RBGL" version))
       (sha256
        (base32
         "0y6ghiwraw3whs57i31qnvlvqlbl9143wxv2yg48rp7qxipxvn6s"))))
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
    (version "1.26.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "RCAS" version))
              (sha256
               (base32
                "073fhbh0pzyca90q2irbaiwj21sci8jf9ksflx9pm9fr1giy37i4"))))
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
    (version "1.32.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "regioneR" version))
       (sha256
        (base32
         "0grlqga9d6mch9l487fdll65wi0m2kpq2097igfhx185i8jvwsz1"))))
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
    (version "2.39.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ReportingTools" version))
       (sha256
        (base32
         "15h7vqdxfv7y0f82ff7a8brqnscs324x22izlkgjk2wqahnmr2l1"))
       (snippet
        '(for-each delete-file
                   (list "inst/doc/jslib/jquery-1.8.0.min.js"
                         "inst/extdata/jslib/jquery-1.8.0.min.js")))))
    (properties
     `((upstream-name . "ReportingTools")))
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
             (call-with-values
                 (lambda ()
                   (unzip2
                    `((,(assoc-ref inputs "_")
                       "inst/doc/jslib/jquery-1.8.0.min.js"))))
               (lambda (sources targets)
                 (for-each (lambda (source target)
                             (format #true "Processing ~a --> ~a~%"
                                     source target)
                             (invoke "esbuild" source "--minify"
                                     (string-append "--outfile=" target)))
                           sources targets)))
             (copy-file "inst/doc/jslib/jquery-1.8.0.min.js"
                        "inst/extdata/jslib/jquery-1.8.0.min.js"))))))
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
     (list esbuild r-knitr
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
    (version "2.44.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "rhdf5" version))
              (sha256
               (base32
                "1akbr55lylwp20b2j351h1gcj1sc5y2j2pjjdpz43m6b7mji23a0"))))
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
    (version "1.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "rhdf5filters" version))
       (sha256
        (base32
         "1v9wxa5q0iwphrmkf9x70jpbq9r9rqz2hm94smqnnpfxszila49m"))))
    (properties `((upstream-name . "rhdf5filters")))
    (build-system r-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'use-system-libraries
            (lambda _
              (substitute* "src/blosc/Makefile.in"
                (("libblosc.a: blosc") "libwhatever.a:")
                (("libblosc.a")
                 (string-append #$(this-package-input "c-blosc")
                                "/lib/libblosc.so"))
                (("../zstd/libzstd.a") "-lzstd"))
              (substitute* "src/bzip2/Makefile"
                (("libH5Zbz2.so: bzip2-1.0.8/libbz2.a") "libH5Zbz2.so:")
                (("./bzip2-1.0.8/libbz2.a") "-lbz2"))
              (substitute* "src/zstd/Makefile"
                (("libH5Zzstd.so: libzstd.a") "libH5Zzstd.so:")
                (("libzstd.a") "-lzstd"))
              (substitute* "src/vbz/Makefile"
                (("../zstd/libzstd.a") "-lzstd")))))))
    (propagated-inputs
     (list r-rhdf5lib))
    (inputs
     (list bzip2 c-blosc zlib (list zstd "lib")))
    (native-inputs
     (list r-biocstyle r-knitr r-rmarkdown))
    (home-page "https://github.com/grimbough/rhdf5filters")
    (synopsis "HDF5 compression filters")
    (description
     "This package provides a collection of compression filters for use with
HDF5 datasets.")
    (license license:bsd-2)))

(define-public r-rsamtools
  (package
    (name "r-rsamtools")
    (version "2.16.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "Rsamtools" version))
              (sha256
               (base32
                "18kh6vmyjmv7zklqr5ifpfixjwldxgwj550c69mqfkk7m0zk52cl"))))
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
    (version "1.30.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "RTCGA" version))
              (sha256
               (base32
                "1y7ciq1270dc1c9y490fygrsylyzwl52fqi1vj7r75hy3rnwnv25"))))
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
    (version "2.30.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "RTCGAToolbox" version))
              (sha256
               (base32
                "03p5rgjzc1265q60jkvk92vhcib5wdnv6bz997r70q9c39y03wm5"))))
    (properties `((upstream-name . "RTCGAToolbox")))
    (build-system r-build-system)
    (propagated-inputs (list r-biocgenerics
                             r-data-table
                             r-delayedarray
                             r-genomeinfodb
                             r-genomicranges
                             r-httr
                             r-raggedexperiment
                             r-rcircos
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
    (version "1.60.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "rtracklayer" version))
              (sha256
               (base32
                "0gy8df0ndq6nyly4b5h3kby8k77rc4j9n7zhnliryvirpr6alm9m"))))
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

(define-public r-saturn
  (package
    (name "r-saturn")
    (version "1.8.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "satuRn" version))
              (sha256
               (base32
                "0frm7iblxkc8ajcdqrfgsvf4krn6x8cr3mx7fnzq06xij0mqm3sj"))))
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
    (version "1.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "scAnnotatR" version))
       (sha256
        (base32 "0mxq2dknrhgj1487kfshkayx963c7qdgxwmlh9fbfr0lwc7gljfn"))))
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
    (version "1.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "scDblFinder" version))
       (sha256
        (base32 "0lvacrgfj9i8mwlcv5ykxs69hd3bm6bvr6fsddxffsr7cckpn31w"))))
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
    (version "1.22.3")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "scmap" version))
       (sha256
        (base32 "06i3r7zybwcgnak9ml2jaz7fy70zjqdh28v03ckaqhvck49kdqdm"))))
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
    (version "1.12.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "scry" version))
              (sha256
               (base32
                "0406slld468m9lxswv1akcdmap6izvzz6zljyrs5av546nfxpmrz"))))
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
    (version "1.40.1")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "SeqArray" version))
              (sha256
               (base32
                "1771vk23psjavvi1nf2z8i2xawygdh4amawlijnskci8y9w4x5dm"))))
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
    (version "1.66.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "seqLogo" version))
       (sha256
        (base32
         "0727yrmm194gskrcpmq4fh0qmi6dnp70gx627psrpvzlk6vlcl8k"))))
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
    (version "1.32.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "seqPattern" version))
              (sha256
               (base32
                "033hss67dxi6fbvn6ya7vc61f60m20hwms9kmdia0icyp8nv2yil"))))
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

(define-public r-shinymethyl
  (package
    (name "r-shinymethyl")
    (version "1.36.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "shinyMethyl" version))
              (sha256
               (base32
                "1rqwwglj0475gr14bxazfmcvsy7rq6nlw2zcswa684751wy15w0r"))))
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
    (version "1.58.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ShortRead" version))
       (sha256
        (base32
         "0wpr3ksr7j5yk6m1zaa47dwdgznajsz8as4p9vf7schaqvhq1wc4"))))
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
    (version "1.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "simplifyEnrichment" version))
       (sha256
        (base32
         "1vs57j361fbwv7vaybs71vc208qn76djmbmakhwwcvxwrx77jp5f"))))
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
    (version "1.20.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "singscore" version))
              (sha256
               (base32
                "1s1kyvkbfgwskf0hi4b44c2yx256rjp1yk9ijpld9qlm7z7pi67q"))))
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
    (version "1.20.2")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "TCGAutils" version))
              (sha256
               (base32
                "0nnfrd5x3mii9adizvz79jinlxn2lhg4civ9v0wwygmdhk7rrm1n"))))
    (properties `((upstream-name . "TCGAutils")))
    (build-system r-build-system)
    (propagated-inputs (list r-annotationdbi
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
    (version "1.78.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "tkWidgets" version))
              (sha256
               (base32
                "1xp0gdfrhhfzfww4ig3y90f7334y24rhzrmlxmm4wav8slvcran2"))))
    (properties `((upstream-name . "tkWidgets")))
    (build-system r-build-system)
    (propagated-inputs (list r-dyndoc r-widgettools))
    (home-page "https://bioconductor.org/packages/tkWidgets")
    (synopsis "R based tk widgets")
    (description
     "This package implements widgets to provide user interfaces.")
    (license license:artistic2.0)))

(define-public r-transcriptr
  (package
    (name "r-transcriptr")
    (version "1.28.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "transcriptR" version))
       (sha256
        (base32 "0i38zs33drzk9szy2mdfs9ff9c3ccjgiyyynrrsinrx0szc8l22r"))))
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
    (version "1.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "TrajectoryUtils" version))
       (sha256
        (base32
         "1q3c226fskl45gq28xj2w5paz2s9n9kxm0bkkgg0329w399cs09k"))))
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

(define-public r-scds
  (package
    (name "r-scds")
    (version "1.16.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "scds" version))
              (sha256
               (base32
                "059gahl679if8xsfgpxmi3h9fiagb6v08rd6rs7yv96s3pb8qywn"))))
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
   (version "2.8.0")
   (source (origin
            (method url-fetch)
            (uri (bioconductor-uri "slingshot" version))
            (sha256
             (base32
              "1xr17rg8nfpcvdwa19qrcaqffkz048l5lkqlm6gw0abm8bg2k8x7"))))
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
    (version "1.10.1")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "Spectra" version))
              (sha256
               (base32
                "0vq5ha5m1vny7nrhgfmmkjd30zv0xaignp1a1m0xj08lnhkkmd69"))))
    (properties `((upstream-name . "Spectra")))
    (build-system r-build-system)
    (propagated-inputs (list r-biocgenerics
                             r-biocparallel
                             r-fs
                             r-iranges
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
    (version "1.22.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "stageR" version))
       (sha256
        (base32 "0advih0g2g5w8bx9f21chz8a66f4v84qn3p5skxi084pp142v5ms"))))
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
    (version "2.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "STRINGdb" version))
       (sha256
        (base32 "0mcp9zzbayz208b573ll69glz63b2pnrxsl900y9zd9crim6vck4"))))
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
    (version "1.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "StructuralVariantAnnotation" version))
       (sha256
        (base32 "0gkps2pls9bpqr8sxm8zwdkn156g8r6mhf4bx17nif5s35qld1rb"))))
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
    (version "1.30.2")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "SummarizedExperiment" version))
              (sha256
               (base32
                "05dy57fi43rpq9bhbsc4apa62xki99r84098pbvi3rjmac811425"))))
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
    (version "1.50.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "survcomp" version))
              (sha256
               (base32
                "1va0ijx3qk3rjhzaflygfwlzn5a670y833qly8in8p5r3al9bdj6"))))
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
    (version "3.48.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "sva" version))
       (sha256
        (base32
         "1f6ia57fn2sn7c2zdfswvj9kx8xayssrgf5q6hrr9nrxc0jq19g3"))))
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
    (version "2.6.3")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "systemPipeR" version))
       (sha256
        (base32
         "068rikfq32awhvj0abl30bghv5k2z4zlfkbxpmsdapxhmdzhgnba"))))
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
    (version "2.52.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "topGO" version))
              (sha256
               (base32
                "0avjjvih387qhw8n4vjz4jq66q7sicgacsw53b85lq2c6ppib662"))))
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
    (version "1.28.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "tximport" version))
              (sha256
               (base32
                "0km1vfn4jcp05jk5srrqanjngq6ghh5im6h1c424b926g9s7w7k1"))))
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
    (version "0.6.8")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "valr" version))
       (sha256
        (base32
         "156sqh474synjvdm1j332ab75rqab0n81d674xbgs3rfxlr2ksgz"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-broom
           r-cli
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
    (version "1.46.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "VariantAnnotation" version))
              (sha256
               (base32
                "0masx496cd4ypz6r0l8kk8vi81sipqdnx4iyifjvkrds1p87mvsm"))))
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
    (version "3.68.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "vsn" version))
       (sha256
        (base32
         "0i6c0wyhj0gcq0mw13zyk60cza2baq4yrj1n2dzqgsqkmhm5lfla"))))
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
    (version "1.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "XINA" version))
       (sha256
        (base32 "1byk8xgn9rwfbi677b99ysxi4a9n58fr3hbnwjzhnziy2n2cd703"))))
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
    (version "1.58.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "xmapbridge" version))
       (sha256
        (base32 "09pskmhjymqvhk26mk5yy1k195v536a2qp4k6xdd67xhf5ag260i"))))
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
    (version "0.40.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "XVector" version))
              (sha256
               (base32
                "1v10hfz658gnb6p7pzdl28jbyypv91wx70i0dvi384nfgznhvmj2"))))
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
    (version "1.46.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "zlibbioc" version))
              (sha256
               (base32
                "0x3d5kihckqnfjkc7dxk6mc194vwwr03jfjx2qh15g5494gfmm53"))))
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
    (version "1.10.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "zellkonverter" version))
       (sha256
        (base32 "1p89i2dfwcgrdchdnxrywp3jjjn5jjs5bisrx5kav26yh2pyx1wk"))))
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
    (version "1.78.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "geneplotter" version))
       (sha256
        (base32
         "03x90l7rvxk0z03p5immqbyyhiygi3mkik636al1cjdcxl9dr5f8"))))
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
    (version "1.62.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "oligoClasses" version))
       (sha256
        (base32
         "0i745q2kx7rdfbkz3cwzzp1m4diny25mc301vy0sh4z5lpi5mdm8"))))
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
    (version "1.64.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "oligo" version))
       (sha256
        (base32
         "0gs26h0hm8xlv3aayz4w0vwmw15x5602g02728spsm3n0qra3b1j"))))
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
    (version "1.10.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "QFeatures" version))
              (sha256
               (base32
                "1xnmd14nf4cqbfxkjsl6af312k7l27ars5g8qdnljylkn8kq276z"))))
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
                             r-s4vectors
                             r-summarizedexperiment))
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
    (version "1.66.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "quantsmooth" version))
       (sha256
        (base32 "05lja0viz5jgc7g3b8qfhd1x596vczid0dz8278qpl4zwv9i17vx"))))
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
    (version "2.32.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "qvalue" version))
       (sha256
        (base32
         "1552390pf3wrrir6c9xshgll69a33fhbdnh2cvpnznma1ag490ki"))))
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
   (version "1.22.1")
   (source (origin
            (method url-fetch)
            (uri (bioconductor-uri "apeglm" version))
            (sha256
             (base32
              "19r1mhpkn3xy6l188c14k9xgn25xqc718bwrlmnz63lak6mp4ws7"))))
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
   (version "1.32.0")
   (source (origin
            (method url-fetch)
            (uri (bioconductor-uri "GreyListChIP" version))
            (sha256
             (base32
              "1sfpf9msnzyrc8b0xzc2406bq2gkcwrrhv7fa9ynqv2ip6xwsc8s"))))
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
    (version "3.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "DiffBind" version))
       (sha256
        (base32
         "1p5fxfpvjz85a2n1jsmj3niqzymnlsja9b53q2yxvj327qq58z8a"))))
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
    (version "1.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "MBECS" version))
       (sha256
        (base32 "0gfr3c5k7xjd342zfdksgh22mrk3ryr4cp89nar0mlpgzxg4l4vz"))))
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
           r-markdown
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
    (version "1.16.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "mbkmeans" version))
              (sha256
               (base32
                "0c48ycgba1m10p1w2qz9r7yxw15nqzlz8fp44d6lbrvj3svmgjwi"))))
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
    (version "1.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "MultiBaC" version))
       (sha256
        (base32 "1qwf1mvw81hb929xpnx5x910f7qnax9lkylhwzcdw1814s4zj25i"))))
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
    (version "2.56.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "multtest" version))
       (sha256
        (base32
         "19c6l8pkyp0m2zdzhr81cnmx5lqf501sap9h6ig5iiv66sf2ysmz"))))
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
    (version "1.78.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "graph" version))
              (sha256
               (base32
                "184izzxc8bpagcac8wqinjp8kmf9zpvjibl0g32nnr64kp9f4rsn"))))
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

(define-public r-ggtreeextra
  (package
    (name "r-ggtreeextra")
    (version "1.10.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "ggtreeExtra" version))
              (sha256
               (base32
                "19xikly6ksh03kgpz35wj2d2gdbpikhrk71ahj9ghhkmlwdyjbcr"))))
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
    (version "1.7.2")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "ggpicrust2" version))
              (sha256
               (base32
                "0yk62cc0vmv6dyfiwvvbgpsqlvp1cw61db60153xvzmcdvd077cl"))))
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
    (version "1.18.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "PepsNMR" version))
              (sha256
               (base32
                "17rbadvay3kbp4r6p023yww4biw9cl359q200ciygr7d2ffcas85"))))
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
    (version "3.34.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ChIPpeakAnno" version))
       (sha256
        (base32
         "0y9qskkdkl2vdbn66md4xisny0i7g2biasylp357vrkv41d3al6l"))))
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
   (version "1.12.2")
   (source (origin
            (method url-fetch)
            (uri (bioconductor-uri "MatrixGenerics" version))
            (sha256
             (base32
              "1bzdhm2dj93xffla00hphxn45mpyn3cr8nv8d5xjqgx8j136biyy"))))
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
    (version "1.78.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "marray" version))
              (sha256
               (base32 "1z8fx47ng1av8nfpk0i7wpb8k8lsrn66k613p36mbh19m5fx05ks"))))
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
   (version "1.60.0")
   (source (origin
            (method url-fetch)
            (uri (bioconductor-uri "CGHbase" version))
            (sha256
             (base32 "1r050acahvfp2ib88sy9xj4yjzy2d8h73hndi1s075h0zv12mjj7"))))
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
   (version "2.62.0")
   (source (origin
            (method url-fetch)
            (uri (bioconductor-uri "CGHcall" version))
            (sha256
             (base32 "02d8j0al062k33h6n3ihn7kwbqp3pjwg9zzbr45ay1m1jm6bappi"))))
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
    (version "1.36.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "QDNAseq" version))
              (sha256
               (base32 "0lcf5zkv44s7xsa2svxia7inv6iz2k1kilfj5zcq80r339pj3vx4"))))
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
    (version "2.31.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "baySeq" version))
       (sha256
        (base32
         "0lq2wfm3ibrpha9mqhhp6dgsx4jm2kwxvvrj0b62dzqspvg743wh"))))
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
    (version "1.30.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ChIPComp" version))
       (sha256
        (base32
         "1v27bqik5cgcvs1kb67s52r56v6fakvmi81iybvvvbxvcn1dmy9r"))))
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
    (version "1.30.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "RiboProfiling" version))
       (sha256
        (base32
         "0qgzq82v4glkc0krk81zj9z71cnrvflsf1w45r9zmaw6bq45rfqd"))))
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
    (version "1.32.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "riboSeqR" version))
       (sha256
        (base32
         "0la8kmxxh5jnqsrmmvyhi313bjdbqkiq16hcxar6mgyjhxlsm610"))))
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
    (version "1.28.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "InteractionSet" version))
       (sha256
        (base32
         "1vs3mqf3x8zk7p83jkv41kag1bmn5zxrr3j1ldqk6wxsl77h55c5"))))
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
    (version "1.34.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "GenomicInteractions" version))
       (sha256
        (base32
         "0qhb2mm5nynnw0cj5da0i6hb1bz7cyns0gp7xy0wscmfyh2wahbv"))))
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
    (version "1.74.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ctc" version))
       (sha256
        (base32
         "0ph22v1an2havnh9sm7hms0q2pq4fjad2cm4dffiy3344cv70sn1"))))
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
    (version "1.52.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "goseq" version))
       (sha256
        (base32
         "1r13n64s9fbx951yaa4mpna71vynbkjz78irk96yzh2x2zzxpjvx"))))
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
    (version "2.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Glimma" version))
       (sha256
        (base32
         "0171wizl516chzzwnbc2z2bf5sk4a491fcs4yhq5ycqaqpjmbg35"))
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
    (version "1.12.2")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "glmGamPoi" version))
              (sha256
               (base32
                "0d6q8vn8z90k8ffskcn9jmgg5x5pfb3wjv67bqskasy38inn1zg7"))))
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
    (version "1.28.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ROTS" version))
       (sha256
        (base32
         "0b2fl4wn696ia2kxihc6088ncnvd87ffq40bz58qi1bhq6krcmwv"))))
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
    (version "1.72.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "plgem" version))
       (sha256
        (base32
         "1kzrscaqsrsvdq2c9g93l7bjmv1s5p1l3cbrz6skkqls9h4gsw08"))))
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
    (version "1.20.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "plyranges" version))
              (sha256
               (base32
                "1qv8snpcxpg16v5ji7sq3bnmgrgghz9h4mh246mcnnxfhi44b6nv"))))
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
    (version "1.30.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "INSPEcT" version))
       (sha256
        (base32
         "1nyfl78fjj956bk1xf32k3jdqhlffx6grbl7vajn48q5ldsx8kcx"))))
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
    (version "1.30.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "DNABarcodes" version))
       (sha256
        (base32
         "1jys0lj92zkqa3bb22fg35q5y8ij5pqhy3yvzg8zc8bjpzdvrg3a"))))
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
    (version "1.34.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "RUVSeq" version))
       (sha256
        (base32
         "0kxaaqf6py7kfndgpd0faxv6g610zj16pyvn9b4x9s3b22ib06j1"))))
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
    (version "1.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BiocNeighbors" version))
       (sha256
        (base32
         "1i4b37n9darizfq9i4vvbnxgrwhkvvd25a4cpfjv7nqywjfbsfjd"))))
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
    (version "1.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ScaledMatrix" version))
       (sha256
        (base32
         "0xhw77w28r3agv0hw7fjpn1fp2p9bz7kdzwzx7gsa8411nffmd4d"))))
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
    (version "1.24.2")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "treeio" version))
       (sha256
        (base32
         "0xjhyrqpr54aaj4han69ndkbmz5kwnf01adh80v2r8jc8jwdfy6m"))))
    (properties `((upstream-name . "treeio")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-ape
           r-cli
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

(define-public r-treesummarizedexperiment
  (package
    (name "r-treesummarizedexperiment")
    (version "2.8.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "TreeSummarizedExperiment" version))
              (sha256
               (base32
                "1grnz82a7l5dpwssc24yxhllqdcvza2gc4h4cc6zxjcdg5r4l1ra"))))
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
    (version "3.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ggtree" version))
       (sha256
        (base32
         "174bdjqvq51rpdyjlcwgbxfmkpsbz6m94ymmhmsq3x39xlcbrq7y"))))
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
    (version "1.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "metapod" version))
       (sha256
        (base32
         "0lr2sgbgdbqq626nsm3vyhghs8nalg5cxycr4cwsqh9nqrmip3wa"))))
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
    (version "1.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BiocSingular" version))
       (sha256
        (base32
         "1a33zsw353pryq30178sbj9jzsmb4m7spqbx11hsicri8s1c0lb9"))))
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
    (version "3.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "destiny" version))
       (sha256
        (base32
         "0hc85dq68h7dy5jcflpjj05231ny7na9i4lksfsw89jhmxgzhacd"))))
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
    (version "1.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ChIPexoQual" version))
       (sha256
        (base32
         "1iysqs2k0xxiwd48ymahf6rywwj9ingjcfmnhx9d1x6w2h00v7lz"))))
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
    (version "1.74.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "DNAcopy" version))
       (sha256
        (base32
         "1bj87ksvch5cm5zxwgfiiql0j94pvzbpn59wp6bacyip65z7w3ax"))))
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
    (version "2.0.10")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "HTSCluster" version))
       (sha256
        (base32
         "0scn4fsfmlkzxibfhsh6krm2cl9c8hsmyjgn48k9dyjf0ylyxg9n"))))
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
    (version "1.40.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "EBSeq" version))
       (sha256
        (base32
         "0y23k607f61csj2ciaqnl8pzsjisqlcnnhlasv36446n8f3silx5"))))
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
    (version "1.26.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "karyoploteR" version))
              (sha256
               (base32
                "1nqcm2jlazm8wsa4p6a59m1j8y1qdwg0w3kii4dqnkann3q2q68x"))))
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
    (version "1.28.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "lpsymphony" version))
       (sha256
        (base32
         "08b4d7k5qx19bpg12pw89ckk8x6r2n28qjdxbmy1cxn6dcgzhijd"))))
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
     (list gfortran pkg-config r-knitr))
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
    (version "1.28.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "IHW" version))
       (sha256
        (base32
         "0i4yahgkrg9rnc0xldm026k6bd1bn5nkgkaf6kpyyday41rxzyq8"))))
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
    (version "1.28.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "iCOBRA" version))
       (sha256
        (base32
         "120bcakhhl6xqsfclzgmn7rcxmld289bjl6j7nrli7aiq5ilz8h4"))))
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
    (version "1.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ResidualMatrix" version))
       (sha256
        (base32
         "1yay0i0nh2ax35ayqlpnmq0839syl5991hacyrbv3li3gqjc9qqv"))))
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
    (version "1.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "batchelor" version))
       (sha256
        (base32
         "1cswij2mdjsdh99rmyh68wscg6h7cs0cff69gnki4gzzjfxy8x3f"))))
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
    (version "1.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "MAST" version))
       (sha256
        (base32
         "0vm7v2anbrci6mcans9y9svd6xjm4waf2kny3rz0cjgsasw60ycx"))
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
    (version "2.28.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "monocle" version))
       (sha256
        (base32
         "05pix638s4hkig2i048m5jjhvw6cqr2b5qvfkk14i64p9kpifmqj"))))
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

(define-public r-sangerseqr
  (package
    (name "r-sangerseqr")
    (version "1.36.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "sangerseqR" version))
              (sha256
               (base32
                "0hhq5zsx4srd9ndr370m44f80qdlrqafjv1838wf65gzijffhfqb"))))
    (properties `((upstream-name . "sangerseqR")))
    (build-system r-build-system)
    (propagated-inputs (list r-biostrings r-shiny r-stringr))
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
    (version "2.44.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "NOISeq" version))
       (sha256
        (base32
         "0cxhy4yrww6bh4z603389fikhix8rhfy04ylhm74fdmdng4jblg6"))))
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
    (version "1.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "scDD" version))
       (sha256
        (base32
         "1ndb1zcbdy00xkfx18slrm5bnld9ci5scysc37clhsq2lq5xb25z"))))
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
    (version "1.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "scone" version))
       (sha256
        (base32
         "0v3rd2h0n52qz9kqxa3l49rjfssfk252dy7j2nvi34y85win2p1w"))))
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
    (version "2.68.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "GEOquery" version))
       (sha256
        (base32
         "1xyrfj8b7j2wdjlbmwhx1c0hfbvr7l7jxyw2v64fbw8604zprv4s"))))
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
    (version "0.42.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "illuminaio" version))
       (sha256
        (base32
         "190i3b9qmh26bic1lzi54mw4p1nrg57qijl1pg6b29w3i6srq692"))))
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
    (version "1.74.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "siggenes" version))
       (sha256
        (base32
         "0h5asj2w4xgfj9xapjawmxldnhq789py39drlr8illyhcczkzkbz"))))
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
    (version "1.42.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "bumphunter" version))
       (sha256
        (base32
         "1xghz87702fg9r6n1igygf4ybb8mw8ff0i02qkx9jmm6vmmfhv18"))))
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
    (version "1.8.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "BumpyMatrix" version))
              (sha256
               (base32
                "021xn5d08phmwv6g8a5d4ap5kcf5syhm6vpr784l4k54lflssr5i"))))
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
    (version "1.8.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "mia" version))
              (sha256
               (base32
                "1p78yacx0cgihva7m2n3vnll5w4b47vl98hy12pq9rnmhk6r591z"))))
    (properties `((upstream-name . "mia")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-ape
           r-biocgenerics
           r-biocparallel
           r-biostrings
           r-decipher
           r-decontam
           r-delayedarray
           r-delayedmatrixstats
           r-dirichletmultinomial
           r-dplyr
           r-iranges
           r-mass
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
    (version "1.22.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "microbiome" version))
              (sha256
               (base32
                "120wyjv2r9cbjgxbp70nzf249hai8cqn59gcbgwnazynfy5ih1w8"))))
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
    (version "1.1")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "MicrobiomeStat" version))
              (sha256
               (base32
                "1j5sar85a1gksc83pc4ypxwb0c7whxglx069zarphrgqfazcr1m4"))))
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
                             r-phyloseq
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
    (version "1.8.1")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "miloR" version))
              (sha256
               (base32
                "073s239aqkixsrarqxfv4nmpcj025k32nhql63qaxrkrvvdd34di"))))
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
    (version "1.46.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "minfi" version))
       (sha256
        (base32
         "18b9yg9hnnm1saaiprm2hj86aajjwm1zwvpj0yadfa3s811pw4nq"))))
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
    (version "1.34.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "missMethyl" version))
              (sha256
               (base32
                "1jvrdr57mv93pcbyazcg1bcx1zd0kp72hi1if839gw54hk3igs3h"))))
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
    (version "2.46.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "methylumi" version))
       (sha256
        (base32
         "1aa0pwjyp2p9a4mx4n4qw88ndgrj56p669yzdkd7hxhc3x55nzlf"))))
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
    (version "1.10.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "lefser" version))
              (sha256
               (base32
                "0wi70b4k7s0xj7bh46s0x0fckqc5qspzq8k4k913vg6cnhqgw2dd"))))
    (properties `((upstream-name . "lefser")))
    (build-system r-build-system)
    (propagated-inputs (list r-coin r-ggplot2 r-mass r-summarizedexperiment))
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
    (version "2.52.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "lumi" version))
       (sha256
        (base32
         "1lxxqr7x9jdjrsj5j95jb0dbd7p208vifxw3j4s4140zh1ppxnpj"))))
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
    (version "2.24.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Linnorm" version))
       (sha256
        (base32
         "1sqkmjah2lxahnvwv04a970za2b8kzvmxnb41k9xrnvj0akigc2d"))))
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
    (version "2.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "IONiseR" version))
       (sha256
        (base32
         "1yrsd5n9zam6wslc6savfn122v3wzzcc46w20mb849qq2np4frf0"))))
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
   (version "1.14.0")
   (source (origin
            (method url-fetch)
            (uri (bioconductor-uri "tradeSeq" version))
            (sha256
             (base32
              "14b90x9h34mhbc4sdpzkygrrswf84pi1ddwpzxhvlvr9gs443xqs"))))
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
    (version "1.42.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "VariantTools" version))
       (sha256
        (base32
         "1vkdw626r1ffdsvry6qwhd1i3lkyb9wzrp9zf3dfafi02cap6r47"))))
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
    (version "3.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Heatplus" version))
       (sha256
        (base32
         "031f25w960jp5nhd78v3iv6pib266cpbawhi9rrd7csw89vnswfx"))))
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
    (version "2.26.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "GOSemSim" version))
       (sha256
        (base32
         "15z7wqnp0s8fiysl3qc76pjaj3xik2br2mz2z3nmf28vxig69mx9"))))
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
    (version "1.48.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "anota" version))
       (sha256
        (base32
         "0ic12p1qyfrmh3l1wv7s9yi8sr84crj4nksmcx0lybb6rak9gy0x"))))
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
    (version "1.66.2")
    (source
      (origin
        (method url-fetch)
        (uri (bioconductor-uri "sigPathway" version))
        (sha256
          (base32
            "0k86hlz7zbbw7559bd2sl59pr441kihgwvg8nr75mj8d50n783sy"))))
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
    (version "1.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "fcScan" version))
       (sha256
        (base32 "071sylwwj27bk39pkn5a29r7bmfpmyaixkkg7dqcdq230c1dsrps"))))
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
    (version "1.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "fgsea" version))
       (sha256
        (base32
         "0chnrwiwk31crrx4am1yrbxjdqd3jycgjgczqzj7lxaa9v7lvm7z"))))
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
    (version "3.26.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "DOSE" version))
       (sha256
        (base32
         "1whvgzw8p0nm3kchdndrxj4x3fhmq0vgbz77d54sqq1qri4j35qx"))))
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
    (version "1.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "enrichplot" version))
       (sha256
        (base32
         "10vmlw58hgzcpv2hlxk9ircza889vs9z01z575633qj0ivsczh0z"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-aplot
           r-dose
           r-ggplot2
           r-ggraph
           r-ggnewscale
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
    (version "3.4.0")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "ClassDiscovery" version))
              (sha256
               (base32
                "06l7jvpcdynij05hb0ka33zpg7xdyqyzfzx24s0bnlspp83yc62s"))))
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
    (version "4.8.2")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "clusterProfiler" version))
       (sha256
        (base32
         "0iijby2j9i6sbdc3iwhqqb8xlz25k3dpiyq91p7yybggpr2p1nw4"))))
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
    (version "2.20.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "clusterExperiment" version))
              (sha256
               (base32
                "0h22zh6jpd2wsh6b9rnrmx8897aqlrsnw82kwphx8lay4r1vv706"))))
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
    (version "1.80.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "MLInterfaces" version))
       (sha256
        (base32
         "1s5b27n01crfzrpshhiv6g0q0qf0dip1gw4nkrkg5sh5x9dsikq6"))))
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
    (version "1.72.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "annaffy" version))
       (sha256
        (base32
         "0jy3qk31lb11bixncb2w2hw1ibvik31bfi2l5vyq5wjyxndfyax6"))))
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
    (version "1.48.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "a4Core" version))
       (sha256
        (base32
         "14q9q3dxnmmm491af53hd0dwwqiz47xxrwy05axh85x4fh01j3xx"))))
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
    (version "1.48.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "a4Classif" version))
       (sha256
        (base32
         "1z5xypz5jvb06pk71x6953iirhh1w1mggm0pxqibg0ag9lx2zzmv"))))
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
    (version "1.48.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "a4Preproc" version))
       (sha256
        (base32
         "1dxh0zbv4z4gqnwbv7yn247gx7cqv68vb43p6f98c2kyqilfpjg9"))))
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
    (version "1.48.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "a4Reporting" version))
       (sha256
        (base32
         "0lx7fx2vlpq3c347gx842qlcvnzl0r25i5a55l0qbf8rb5liq2yv"))))
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
    (version "1.48.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "a4Base" version))
       (sha256
        (base32
         "01c8rps321820b0pchfqv8shglb26rys7hqkwygpzzx3jj310v8x"))))
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
    (version "1.48.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "a4" version))
       (sha256
        (base32
         "0n49scwwmg51gcakbc0bfjcy1lcpdxg974l11yk35kj0bg9ynpgi"))))
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
    (version "1.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "abseqR" version))
       (sha256
        (base32
         "05kcwmv3d59lrdnpi82701biwawnnpfvpwr170xdi3hfdff6g59c"))))
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
    (version "1.28.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "bacon" version))
       (sha256
        (base32
         "1h2yh85c6d4j2abdh5l7m6zcxh9i5xzrwwjkwas4nfs48h74z293"))))
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
    (version "2.48.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "rGADEM" version))
       (sha256
        (base32
         "08li2wql2zhg06z0rjws9qcnv3ishgnpc6k8xyjcf7yfyg7qpk1d"))))
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
   (version "1.42.0")
   (source (origin
            (method url-fetch)
            (uri (bioconductor-uri "MotifDb" version))
            (sha256
             (base32 "18d0qj9sn4bhfjy2mwsz2nnm41xlsqjslsv69nkhv19w9zd842pw"))))
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
   (version "2.14.2")
   (source (origin
            (method url-fetch)
            (uri (bioconductor-uri "motifbreakR" version))
            (sha256
             (base32 "13fv0rkyb32grswlgzd3zr35p9xpibj2iq62sr23if4w6z5nbml2"))))
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
    (version "1.44.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "motifStack" version))
       (sha256
        (base32
         "1g46cxn1h3cqr0yrj6ancshzygiqr9finf1vmmig3h9g0ijgr5lz"))
       (snippet
        '(delete-file "inst/htmlwidgets/lib/d3/d3.v4.min.js"))))
    (properties `((upstream-name . "motifStack")))
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
             (with-directory-excursion "inst/htmlwidgets/lib/d3"
               (call-with-values
                   (lambda ()
                     (unzip2
                      `((,(assoc-ref inputs "_")
                         "d3.v4.min.js"))))
                 (lambda (sources targets)
                   (for-each (lambda (source target)
                               (format #true "Processing ~a --> ~a~%"
                                       source target)
                               (invoke "esbuild" source "--minify"
                                       (string-append "--outfile=" target)))
                             sources targets)))))))))
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
    (version "2.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "GenomicScores" version))
       (sha256
        (base32
         "1z8h9sbr6xqwbsfisr33qdxg0xdrw8r4vvxl1pf4x8vv08zw0vx6"))))
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
    (version "1.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ATACseqQC" version))
       (sha256
        (base32
         "0mcrkmirss484d2mskikyjdz5cmbhdk52yxkffz9g8g5m9lkq5xk"))))
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
    (version "1.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "GOfuncR" version))
       (sha256
        (base32
         "05509xf768x8asqadjj5s50m0yqnklrxi28bmqd22cn6cbmahszw"))))
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
    (version "1.74.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "annotationTools" version))
       (sha256
        (base32
         "0dwbh2h2mp8gy0kwqmkri3q2glnzcj8cn83j1qyh7lk15anc33g8"))))
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
    (version "1.38.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "AllelicImbalance" version))
       (sha256
        (base32
         "1ms3x3x2gvhgllff3nwawg5jlpmgx6dhwll0dghklnv3lssbx4c7"))))
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
    (version "1.22.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "AUCell" version))
       (sha256
        (base32
         "1zqjmg8nxxj30lxppl685ihynbz44vw7qm3kwjq6cp83c5556hz9"))))
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
    (version "4.42.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "EBImage" version))
       (sha256
        (base32
         "0z0ajmy1zx7mqcjm2ibyk2qg3cj00kl76padyksbdbmwdncxs8yb"))))
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
    (version "1.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "yamss" version))
       (sha256
        (base32
         "0kjlp62s2393jdwp0crizsgp4iqagbgnd3hdl9vpbr9qrjxg4s7m"))))
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
    (version "1.32.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "gtrellis" version))
       (sha256
        (base32
         "1kk7ysscq8xdbn35b20s8sn8gl93sy0c718fmvr1yrhiqaxq5alr"))))
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
    (version "2.36.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "SomaticSignatures" version))
       (sha256
        (base32
         "075w88x7lv2fkp2ipqgxp4pzh6kbjw3nmp0qra2p4fss4j3g8d6s"))))
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
    (version "1.25.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "YAPSA" version))
       (sha256
        (base32
         "0xva7vljq2k78rzjnjlp4x3ylwk86jaqn2cgbff4h4sb8rfdya7c"))))
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
    (version "2.72.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "gcrma" version))
       (sha256
        (base32
         "0k4fsmqkv82d3a6v3gwphvbri5sgbd3f1s4qyv960rhyk2xj2b4p"))))
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
    (version "1.34.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "quantro" version))
       (sha256
        (base32
         "08m94q65c11isbkx9xljm2bx2aq2rml1bqh5i1cg81xg2ilm9h6a"))))
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
    (version "1.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "yarn" version))
       (sha256
        (base32
         "0ddfl4jp0n8sb665hcpsij959fl28rj22n1h61wp2374kpc0fwfj"))))
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
    (version "1.36.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "roar" version))
       (sha256
        (base32
         "0p5q6nyp8d0gl95rd89xrs08gv87m1q6d0x4ws2b6sd4gc1nw2ny"))))
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
    (version "1.66.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "MassSpecWavelet" version))
       (sha256
        (base32
         "1nc1imxia71sxxvi77f91yhwxza2l8kk1d7zkp5680xzw6yd6ajr"))))
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
    (version "3.22.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "xcms" version))
       (sha256
        (base32
         "112g2lpi074cr8g09cqwjbcwv9aw4djr7ashxpnxjycd3ayc0j3v"))))
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
           r-multtest
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
    (version "1.8.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "wppi" version))
              (sha256
               (base32
                "06y4pq8msjzwrw1jgyj9yskgpfhvcz54qcsv0h14bakhyrwk00pg"))))
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
    (version "1.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Wrench" version))
       (sha256
        (base32
         "18x12395928d0mf8kffmdjqkdxrzgqzzhhvs7sdzldwyas6hfg2h"))))
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
    (version "1.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "wiggleplotr" version))
       (sha256
        (base32
         "15gbs10bi1alxxbxaj4h3hzfgck4nxygy896y34x81w1rn7in3vp"))))
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
    (version "1.78.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "widgetTools" version))
       (sha256
        (base32
         "03bvh2jk97jj40z35q5n1is2wxrs4dggw08ndyywy4pzx1diqdc4"))))
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
    (version "1.72.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "webbioc" version))
       (sha256
        (base32
         "0z2sixzs0rwdwdhxs5mqzghgiw4g64l8p9ag5lw289bzs4c4kqnd"))))
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
    (version "1.22.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "zinbwave" version))
       (sha256
        (base32
         "1bmxbzpgmwama42a3vi1n0sic166v5zs0kl9mhmrh0rrx0nv303k"))))
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
    (version "1.22.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "zFPKM" version))
       (sha256
        (base32
         "1fkzyq282xafyapgi5xi1c2dmfkyikqvf622pycjs80fas38044h"))))
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
    (version "2.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Rbowtie2" version))
       (sha256
        (base32
         "11h4irhi0pxd0l378im455amqfamqypyl0ri2cs8nk1lg184ridr"))))
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
    (version "1.22.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "progeny" version))
       (sha256
        (base32
         "047x6by3xa15gvi3kny5pkqxaq8d2kzcfi55ic5j7a351715l6l7"))))
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
    (version "1.40.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ARRmNormalization" version))
       (sha256
        (base32
         "001nwl605prg942vmnbb1rwjwgpscpjs27ssic3h10rlmpb65yzp"))))
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
    (version "2.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BiocFileCache" version))
       (sha256
        (base32
         "1qk1n50arlk0mfkv9p3zl3lqbapzfkjdbadv51hsp4h9lyaw9sbg"))))
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
    (version "1.36.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "iClusterPlus" version))
       (sha256
        (base32
         "1lbkmin9pkk9yzpmwrfyniyqnwmp0wcgiirq8prmzi5mvndl6wm3"))))
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
    (version "1.40.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Rbowtie" version))
       (sha256
        (base32
         "1hfr1zdvikvygzgyy58f5rnz5jkmsrhwa930h331wx5012hhmnv7"))))
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
    (version "1.34.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "SGSeq" version))
       (sha256
        (base32
         "0qsmy85400in2xvw3bnzjnk8ni3lipqjc81npk3fmvbp6cb85njq"))))
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
    (version "1.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Rhisat2" version))
       (sha256
        (base32
         "1x9phnrk27v2r3ldqa9cd6yp2q3y5p2rm9wmra3wgrmhz9gxq1qy"))))
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
    (version "1.40.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "QuasR" version))
       (sha256
        (base32
         "08vns1wbgpxw1x6djp84f9hl3gqaybbw9917ghfzk0x3ijpvggbg"))))
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
    (version "1.34.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Rqc" version))
       (sha256
        (base32
         "15w5bmcl7h3fz6qhdqz4qc3qkg3pxcmzapg4mnnckzmyg8kh0i4l"))))
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
    (version "3.32.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BiRewire" version))
       (sha256
        (base32
         "1741raw1834093y5zhgx9jywfgz0wl0idlkynvab2c8vi40kc9a8"))))
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
    (version "1.28.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "MultiDataSet" version))
       (sha256
        (base32
         "1qyvwhmdv27xsyljyzyh52pn6x0wqx8rslvr2j7vqcjyywv270x6"))))
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
    (version "1.32.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ropls" version))
       (sha256
        (base32
         "1cq5ixaxag5r6nvl73c8bznxkdjhxw1r7qx90ml7qm4jr7wyi173"))))
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
    (version "1.28.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "biosigner" version))
       (sha256
        (base32
         "19sl75168zv6qqmgsfnhcyw1z0lp0phmky40lsl6bbmy4k2hfw38"))))
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
    (version "1.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "annotatr" version))
       (sha256
        (base32
         "1vjbk1vpqjxy9wv2a6sc32yyk0bhkbvdx9hl3vs5yjifwdswh2pw"))))
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
    (version "2.14.2")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Rsubread" version))
       (sha256
        (base32
         "1dgbvhsd0rki1skwrb4acd3cfy7c9slsjq1s7r2469zbs3xf12xc"))))
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

(define-public r-flowai
  (package
    (name "r-flowai")
    (version "1.30.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "flowAI" version))
              (sha256
               (base32
                "0ydy98qsiqpw0b6fvmlv09kza94qjcl40ma9pknzgbq21ac4z25g"))))
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
    (version "1.64.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ConsensusClusterPlus" version))
       (sha256
        (base32
         "0ig6xfhd70my5j4xrdy8srg1wi8nb3kcxlrld9py47psrq9vdadm"))))
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
    (version "2.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "cytolib" version))
       (sha256
        (base32
         "0r58960jhq44qs6p0l0kd4iazflxdar7sc7m2xa7dwwl9zh5zm42"))))
    (properties `((upstream-name . "cytolib")))
    (build-system r-build-system)
    (native-inputs
     (list r-knitr))
    (propagated-inputs
     (list r-bh
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
    (version "2.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "flowCore" version))
       (sha256
        (base32
         "09jirhhf1qfanyhbq1ybmw6smv8163b7cqgj0qw69grvs8bb0b6m"))))
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
    (version "1.60.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "flowMeans" version))
       (sha256
        (base32
         "18lp1pickpd68fl7a4nidyvyh4yf53gbgn2f2yxaxfp2mk3mj88w"))))
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
    (version "2.46.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ncdfFlow" version))
       (sha256
        (base32
         "0nrj4g02kx763wb70fzvhidmzp82gr1g3fp1aa1bv34cj4lnp47q"))))
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
    (version "1.28.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ggcyto" version))
       (sha256
        (base32
         "1b2xsn0avm6r4zylygdhab3zz3v8qvwbm1xr58894dwgysdk8zc5"))))
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
    (version "1.64.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "flowViz" version))
       (sha256
        (base32
         "0y16hzadpr5c99cwkphc07vh6lypgfk5nacbmn7mmrw01b3gbk67"))))
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
    (version "3.38.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "flowClust" version))
       (sha256
        (base32
         "1gmkmx6gzx4w39ijcad3lx2xnr2b833mhs8gcmc6k21k57aznzs2"))))
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
    (version "2.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "RProtoBufLib" version))
       (sha256
        (base32
         "0qlb3fp4lj3wys2hgrdrccfdf0lf85qnk0g117n959mg3y0dqikq"))))
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
    (version "4.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "flowWorkspace" version))
       (sha256
        (base32
         "1ln98xnnqlz3hqdyhypzarw7jcghd5gjaw8h42yw41w0plhh4wyr"))))
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
    (version "4.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "flowStats" version))
       (sha256
        (base32
         "13mdpdndalm0d6azjancw0xcbdc674ivkj8kp6ccfpdd7gydhr4j"))))
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
    (version "2.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "openCyto" version))
       (sha256
        (base32
         "0z0p09mkap1jqlq1x33i2ik87pbhwq85xgqyfsx4r360nhv06pna"))))
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
    (version "2.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "CytoML" version))
       (sha256
        (base32
         "19rlg15nk2205vma4whpqmvlazww5i3ibxhmk9cljsaj4ql560vf"))))
    (properties `((upstream-name . "CytoML")))
    (build-system r-build-system)
    (inputs
     (list libxml2 #;zlib
           ))
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
    (version "2.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "FlowSOM" version))
       (sha256
        (base32
         "13y0ny61skj14mfhjwpa5zmflzdqxy2vf6gd4m9358g4wxfbhkv0"))))
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
    (version "6.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "mixOmics" version))
       (sha256
        (base32
         "0dqndpmi56g772sra49vdrkjs4m9h2gzimwv3bwmw2l0krh2ax8s"))))
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
    (version "1.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "DepecheR" version))
       (sha256
        (base32
         "1kn1w1fs19gjvg8nhbvj6hyp32h6k81dnal4ab12xl1jgaa3xj5p"))))
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
    (version "1.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "RcisTarget" version))
       (sha256
        (base32
         "0dp2y85mfn995rvg698xnxlkz1kd8g0byjypxx62cpfldnkrinxl"))))
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
    (version "1.28.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Chicago" version))
       (sha256
        (base32
         "04h4a5nyc78jlsg9pj8ay559bwb8y1nm80v9zvyxd7r490yn7k7s"))))
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
    (version "1.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "cicero" version))
       (sha256
        (base32
         "12zdygnwqjla99wzvvxzcmiwmh4v0rmnx4yix02gbj8xl4bxmm51"))))
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
    (version "1.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "circRNAprofiler" version))
       (sha256
        (base32
         "0gagfm7v5bnnfj1zy6zf4cg91bj8nvv4vm38f3a1s4q2xc8pcvfj"))))
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
    (version "1.22.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "GENIE3" version))
       (sha256
        (base32
         "1q4wi3k95c8vsyfx6359p2p29ascjg1cxmpp9bf99ixbjs71rd33"))))
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
    (version "1.76.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ROC" version))
       (sha256
        (base32
         "0fq59bwc5i8zdw0v0jr5j5lm4hk6p7b88i2xndsgj4fq65yr50g1"))))
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
    (version "2.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "wateRmelon" version))
       (sha256
        (base32
         "0qslh2xhhyaf0nsgcqav6qgh05n8kp0s6p1zggc02iid30i1ibrj"))))
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
    (version "1.36.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "gdsfmt" version))
       (sha256
        (base32
         "11qib2znznzvyb0x9qm1nfg9lhyqy63yrdjicy7n3n6l8dfd2lx7"))
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
    (version "1.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "bigmelon" version))
       (sha256
        (base32
         "1l1ym89qqbq6qx07bm2p8gcl5zd9xh0nbw10fb7s0pfbrkj8xy64"))))
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
    (version "1.48.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "seqbias" version))
       (sha256
        (base32
         "1sspl03m2hf6s5rw6nfqrycb236nvcgygql6apdlg3cjnikf090j"))))
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
    (version "1.46.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ReQON" version))
       (sha256
        (base32
         "1knzla5d8n22121a9licrjh2rxrmh05304a9d4bs2f1r7aiwhgnh"))))
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
    (version "2.34.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "wavClusteR" version))
       (sha256
        (base32
         "0491x3m0015g83m6a7pkc4783768clgykhlcmd2xr6cgrhih10g2"))))
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
    (version "1.78.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "tilingArray" version))
              (sha256
               (base32
                "0wj3wb4x9s0v189p20rlwghsx82x314yyhhsnfmrl266qb1ambrn"))))
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
    (version "1.36.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "VariantFiltering" version))
       (sha256
        (base32
         "0v0shsv0s9fzakdb4p84jfc4z57ryan27r1dkbvb3v25kjrhd8fi"))))
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
    (version "1.30.2")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "variancePartition" version))
       (sha256
        (base32
         "17jssd327l0miw52iadag2dbk8w4mhv2vwjpzdw89p8gww47bmbv"))))
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
    (version "1.54.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "HTqPCR" version))
       (sha256
        (base32
         "12p0jb9bpz4x612vwj77d6l5h8rihfkzmhp8qy1gvv2zmn2a54jf"))))
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
    (version "2.4.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "UCell" version))
              (sha256
               (base32
                "01qcwmiqri4xvwr3j4k1g062rfj6bbc0bvh0ifq1jq2xrm1azw9y"))))
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

(define-public r-unifiedwmwqpcr
  (package
    (name "r-unifiedwmwqpcr")
    (version "1.36.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "unifiedWMWqPCR" version))
       (sha256
        (base32
         "1gza678sd5m2rbki0l5hniki6gmds2cljkmywmk5v5m9swh9azq7"))))
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
    (version "1.18.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "universalmotif" version))
       (sha256
        (base32
         "0v1085dl16a3494f1fxc9rk1ffz3si89mdwbmnnczyhj5p13pfx8"))))
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
    (version "1.18.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "ACE" version))
              (sha256
               (base32
                "158v25ivbmsh54ywb5spfsd1nrmvfq83s8fkp14i7q5ckr2kvjhm"))))
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
    (version "1.78.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "aCGH" version))
              (sha256
               (base32
                "0k8fbwhh1w0b0zy5qgixmcnwxi3v78f1bjmjg1yx6paniwk58bzn"))))
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
    (version "2.56.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "ACME" version))
              (sha256
               (base32
                "1z6j4wy355pljn9wf12zzq4zqrhaik0i2phy7jg89jsys7n2mlxy"))))
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
    (version "1.30.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "acde" version))
              (sha256
               (base32
                "088dq6mc9m1bx0dprcz8cdpfp447lnxq7420r139mhcik0z21vky"))))
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
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ActivePathways" version))
       (sha256
        (base32
         "1niwzbzqs1s0mx6ia8fmvg9db6kajs0rdxgnnn1pq68yd8rjn79g"))))
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
    (version "1.66.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "bgx" version))
       (sha256
        (base32
         "1g6k3ryj8dz17asa4imnrk77z4dk9an5ssrqr17r0g797d5f4qjm"))))
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
    (version "1.52.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BHC" version))
       (sha256
        (base32
         "0w60h1jr9kwvgg5d6bcrq4gl1aa1v6xrn43ymsc2312019psrrjy"))))
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
    (version "1.58.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BicARE" version))
       (sha256
        (base32
         "1q7dsvj6nvczs76jcxyy77298vgk4zk083bldmbbgnwparrgjii9"))))
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
    (version "1.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BiFET" version))
       (sha256
        (base32
         "05lwz2pw2vnhmlhp5vv9j45jrc5ssdw4lx6mkxnsvds8zl3y9294"))))
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
    (version "2.58.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "rsbml" version))
       (sha256
        (base32
         "0mgg1qfvxgyjk6fw4x0cv486vrsgf1kbqr6nqhgxj6lk8w6909fm"))))
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
    (version "1.72.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "hypergraph" version))
       (sha256
        (base32
         "13f3m8m6i99hzm94hriry645jcn0a1ki8z8wmn3mkasdi6bzx20h"))))
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
    (version "1.52.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "hyperdraw" version))
       (sha256
        (base32
         "0vhfvmv6b3kab6a8hy2y9zwxgzwb5vliaaxhlrgz8i4pvvq5nhqb"))))
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
    (version "1.36.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BiGGR" version))
       (sha256
        (base32
         "03pd4a3l912zdfk35flagikqimp01wp76nslid32l43d7yg9p57w"))))
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

(define-public r-chemminer
  (package
    (name "r-chemminer")
    (version "3.52.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ChemmineR" version))
       (sha256
        (base32
         "1m4vgq3z0kya7kbncawr9csvbwnh74mh012w7g2ialwd3nwdf91x"))))
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
    (version "1.42.0")
    (source
      (origin
        (method url-fetch)
        (uri (bioconductor-uri "fmcsR" version))
        (sha256
          (base32 "0ci77gbz93i3s2j2gjp7y3ssn6bcdcmqnl23smnjai7qjmln64dp"))))
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
    (version "1.38.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "bioassayR" version))
       (sha256
        (base32
         "0ilwniyrp3m8mi1vmggd17f9g6wdw7a78l2db190w29alzpbfmp6"))))
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
    (version "1.32.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "biobroom" version))
       (sha256
        (base32
         "0kb6ylq94vywfky0mcrva109darcv361sqvwb19qn92p8vh5199l"))))
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
    (version "1.46.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "graphite" version))
       (sha256
        (base32
         "1fhjkgl6567qchi5sfdqnznalm1l7c7pabz8jc9d7ib9x4g922q5"))))
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
    (version "1.44.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ReactomePA" version))
       (sha256
        (base32
         "1hy8qz7d2826kf6pkl3v8cjwx35ap8xr92jw5wv445p3xcd1clzn"))))
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
    (version "2.64.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "EBarrays" version))
       (sha256
        (base32
         "1k50br4hpkrwv1lnn0wp1c7kj32vk0gg19aivmw5d6brjdd39c1f"))))
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
    (version "1.2.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "BiocBaseUtils" version))
              (sha256
               (base32
                "143fmjbi8spaj3njvc1xvsjszfxs7bv3vxik8pisw5y8lqzx7hqm"))))
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
    (version "1.36.1")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "BiocCheck" version))
              (sha256
               (base32
                "0a0fnmqln13iglnw8smbbr4k7hdvacipxa04zhqylygpsq1246bc"))))
    (properties
     `((upstream-name . "BiocCheck")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocfilecache
           r-biocmanager
           r-biocviews
           r-codetools
           r-graph
           r-httr
           r-knitr
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
    (version "1.62.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "biocGraph" version))
       (sha256
        (base32
         "0fnncmi82qb9lkg1zfyps7n3nrw1s3wcqbixh420w53hmdyryryl"))))
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
    (version "2.28.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "BiocStyle" version))
              (sha256
               (base32
                "04npnfmz1p1vpwrdsim309k7518i4p1li04xnmw8c9zgdb6yl61a"))))
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
    (version "1.68.1")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "biocViews" version))
              (sha256
               (base32
                "04rzrwxd9n4l3agmbkx03hhcmy2fx049q5n4glld46mvx3vjvc48"))))
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
    (version "2.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ExperimentHub" version))
       (sha256
        (base32
         "1md6lan98h95jv776zyvl5im39gz4h6fdxw571vfahgr4b2nxvm4"))))
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

(define-public r-grohmm
  (package
    (name "r-grohmm")
    (version "1.34.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "groHMM" version))
       (sha256
        (base32
         "1zg30cb4s97zdv6c0f53ix6a97aan5w3lx698sa4cqvi4hbz8f35"))))
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
    (version "1.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "MultiAssayExperiment" version))
       (sha256
        (base32
         "00xgca1q9ai5x730bvx73f25lpwkr3dn2sia3msa49fpl4nrzr20"))))
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
    (version "1.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BiocOncoTK" version))
       (sha256
        (base32
         "11135a4l0vn84qmpyclz052zy45s0m38av94604jqxgb51q9lwyf"))))
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
    (version "1.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BioCor" version))
       (sha256
        (base32
         "1aw8yh9l0jbjvkqgyzyr0wmwjh3ppmb0lwg8hxsfcv52ycsmvk7p"))))
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
    (version "1.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BiocPkgTools" version))
       (sha256
        (base32
         "0s51dd9kjlh5vckwmynvahvg1pzl2ddvfn3s2kz77m0l0nbh7zfx"))
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
    (version "1.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BiocSet" version))
       (sha256
        (base32
         "1q5794gsinpy9hv5n1vx79bkqxi1jxzxjl95jlw9pqjmlnki07i5"))))
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
    (version "1.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BiocWorkflowTools" version))
       (sha256
        (base32
         "1zvjj0hpqwdv7yifqwyb14166ppxw91d964gz4xclism5z5lvyln"))))
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
    (version "1.72.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "bioDist" version))
       (sha256
        (base32
         "1mx4abv7p76qsa0yzj0b4d03xklx09im01bx02lwa1w527gdmfc3"))))
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
    (version "2.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "PCAtools" version))
       (sha256
        (base32
         "07kl7s2zd7nkhg4hib2mr3w5lhqy780aw53yqsd7bfsjh53k8g95"))))
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
    (version "2.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "rGREAT" version))
       (sha256
        (base32
         "0qq3fqzd973jlf6ibfww5yicnyz2qvs2b2mn35p77k2mp4d7qp52"))))
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
           r-rcurl
           r-rcpp
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
    (version "1.22.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "M3C" version))
       (sha256
        (base32
         "158brq3w5h735s9yq93xx0y4p79yhgz72rpy0cyk4fjia5yaij5c"))))
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
    (version "1.72.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Icens" version))
       (sha256
        (base32
         "1aq4iwk1cp96jzldhx7d4q7i77zbdg8nzkzh12g29yhwzj39nl3q"))))
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
    (version "1.62.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "preprocessCore" version))
       (sha256
        (base32
         "0z37ag22j1wh80vwq1kyr99rvadj9ppkddapfqyql3vj5x44cf4d"))))
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
    (version "1.0.4")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "S4Arrays" version))
              (sha256
               (base32
                "1pwkbp94r8vv43vgxcgqzbs1njhqlahfgsbf8rhr0ndy1bjyly2g"))))
    (properties `((upstream-name . "S4Arrays")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-biocgenerics r-crayon r-iranges r-matrix r-s4vectors))
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
    (version "0.38.1")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "S4Vectors" version))
              (sha256
               (base32
                "0qpjx64ilc1niglhf6pn47damgaf5vdzwkvnlhcixqzpl28ndbs9"))))
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
    (version "1.72-1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "WGCNA" version))
       (sha256
        (base32
         "1p3zsl5r6l5r6ylnrxmbxjpim5qgmncgdjcgn5j69rzk3rv85gqx"))))
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
    (version "2.44.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Rgraphviz" version))
       (sha256
        (base32
         "1dv1vk73achjz5b5zzw2d517nbjfrlj7gppd48pfhk1w0mvz3q61"))))
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
    (version "2.6.2")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "fishpond" version))
              (sha256
               (base32
                "0zsw4j6gk25303xpdwnkda2sq3mb4zb4p1mzwiyf7hdyf87zis05"))))
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
    (version "1.26.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "FitHiC" version))
              (sha256
               (base32
                "0mqrzh1rcwyqlhq9wxcy4gp47hsb70qrsissm9nj0cb9j6ihi407"))))
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
    (version "1.44.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "HiTC" version))
              (sha256
               (base32
                "00csfr3yhbllwc8797xg49qb0djpvbpfi6mbnps284nlqy5vpdwh"))))
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
    (version "1.28.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "HDF5Array" version))
       (sha256
        (base32
         "1riwxxnbi4z0lf42psh3m3ivxlgmmlnqhgsih6911c754r6s39qg"))))
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
    (version "1.22.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Rhdf5lib" version))
       (sha256
        (base32
         "1j8i4rmq85n7jys86a9zyj1n4qn7bhc1sqgcq8dyh7zqfdvb9bcw"))
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
    (version "2.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "beachmat" version))
       (sha256
        (base32
         "0ga9y9npl885jdx7gw5h6lh7fr0z5p7cvcwpz3hvwm4dy235j6gj"))))
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

(define-public r-beadarray
  (package
    (name "r-beadarray")
    (version "2.50.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "beadarray" version))
              (sha256
               (base32
                "0rd41193rq6v4a97ibp0l2bz140nsv91plhn7iim8d8n9hgxqhjp"))))
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
    (version "1.66.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "beadarraySNP" version))
       (sha256
        (base32 "1pymsm4j3wiyjvgsjb3vhjwdvhfmh91pxdhg4ihrj71pqpif3w9m"))))
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
    (version "1.52.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "BeadDataPackR" version))
              (sha256
               (base32
                "0hm4brxg2rxazdcnhjiz6f4di2q8wssrrp35m79vm93sac9w42y3"))))
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
    (version "1.36.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "CNEr" version))
       (sha256
        (base32 "04arfcr2ay71hyy0xsh6jzappmcnnzfr1nyj1l7dqhhhk81ksjfd"))))
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
    (version "1.38.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "TFBSTools" version))
       (sha256
        (base32
         "136s96cmd1aq878xy9mqd4k46awywqpmm55lpvash8mv4gg02vg8"))))
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
    (version "2.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "maftools" version))
       (sha256
        (base32 "1jvmqn33hxzscr8j7y9zw5sngglcy0zkl0386053lx5dl5s5iqyq"))))
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
    (version "1.22.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "motifmatchr" version))
       (sha256
        (base32
         "0vqsxfbrxs25xr27vsr5syqjn6fwvn5dw0g76w4rdqk5d08p56ci"))))
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
    (version "1.22.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "chromVAR" version))
       (sha256
        (base32 "001133rfcwmf4p3dbif933y9i1mkr3krw38g646wj4ikycszwih4"))))
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
    (version "1.22.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "SingleCellExperiment" version))
       (sha256
        (base32
         "1v2px6gvr0wzj2cyml57hxj5ghynlvizkchbywd3kx05j1h2mckd"))))
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
    (version "2.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "SingleR" version))
       (sha256
        (base32 "0dwlg86wkivsag5ffjk81nlq34lwl3kk065k9l6lak24ziq6c4ij"))))
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
    (version "1.10.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "scuttle" version))
       (sha256
        (base32
         "13207k7b9qrpcngk4vd3jhc3zz815anwci1dqgadjyn4c8ragmnj"))))
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
    (version "1.28.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "scater" version))
              (sha256
               (base32
                "0v6qaflv7b504yxxv6bmnlikg36bhfahgqxigpq6qcfrhqapmqck"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-beachmat
           r-biocgenerics
           r-biocneighbors
           r-biocparallel
           r-biocsingular
           r-delayedarray
           r-delayedmatrixstats
           r-densvis
           r-ggbeeswarm
           r-ggplot2
           r-ggrepel
           r-ggrastr
           r-matrix
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
    (version "1.28.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "scran" version))
       (sha256
        (base32
         "1qfmxarw5x17ag66r9i51vxm5h8fxm4rdicv6s8q5pnaf188hdcp"))))
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
    (version "1.12.2")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "sparseMatrixStats" version))
       (sha256
        (base32
         "00jalzg6yphi8ci4iid7x38jlsrvvdswrq7cqa7jybs26ayjldw1"))))
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
    (version "1.10.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "SpatialExperiment" version))
              (sha256
               (base32
                "18fcfyjvp9nzadlicsnz62wva8ik0z6jqg1b906avm2vk6rbxw70"))))
    (properties `((upstream-name . "SpatialExperiment")))
    (build-system r-build-system)
    (propagated-inputs (list r-biocfilecache
                             r-biocgenerics
                             r-dropletutils
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
    (version "1.22.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "DelayedMatrixStats" version))
       (sha256
        (base32
         "13iqlw74zh65y2ckwg0b3xbqc6jgj34xjgsg9axfv7j7znwk9igg"))))
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
    (version "1.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "MsCoreUtils" version))
       (sha256
        (base32
         "055xzlq37d75ghfk87bxbsv4yifidnwxc3w2gp8mcqxwkxxhd70a"))))
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
    (version "1.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "MsFeatures" version))
       (sha256
        (base32 "0cb4b0mf5yh8amqwa3r0mbk2f3rgq40gdlkhifk50fxnqdy4fjsh"))))
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
    (version "1.10.0")
    (source
      (origin
        (method url-fetch)
        (uri (bioconductor-uri "BiocIO" version))
        (sha256
          (base32
            "03hslg7k6khchf54gmlbkvwbkfn5ppz0wp7lh75gsnr0licsjkwx"))))
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
    (version "1.38.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "msmsEDA" version))
       (sha256
        (base32
         "108azzd6ibcgb1gqgcrz4shk3rdr3vfpzy0z6zknlsxwz7sbcnfi"))))
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
    (version "1.38.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "msmsTests" version))
       (sha256
        (base32
         "1y3nklhlib2l9smcl6slv1653vyg9ip29if286l59rjrdwh50cwa"))))
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
    (version "1.24.0")
    (source
      (origin
        (method url-fetch)
        (uri (bioconductor-uri "CATALYST" version))
        (sha256
          (base32
            "0f6h0qngv7my83k402rydvi1pla695r5kxq02k1s5caxcsi1caza"))))
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
    (version "1.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "erma" version))
       (sha256
        (base32
         "14rgj8g9x248p2cc86aqn289qamsd91d6fnydi5d34q605ph73yj"))))
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
    (version "1.48.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ggbio" version))
       (sha256
        (base32
         "0n7nghrrs7fp0b7jk2pfbkzl0lbv55xxk2xqnq2i77ps22ms0h9y"))))
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
    (version "1.44.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Gviz" version))
       (sha256
        (base32
         "1lp345hxlddsc91lxrzay073g2dabc8h1cynyh87y85vzffhwplw"))))
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
    (version "2.32.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "gwascat" version))
       (sha256
        (base32
         "1i64bk0ac2v0w5yjxdhnbjh05l6n9fi7gxap0zp73wxj7kl75wkd"))))
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
    (version "1.46.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "GWASTools" version))
              (sha256
               (base32
                "1g86xg2d9wi6xijhc89zxhbw190aviw9nkwvbwgz85jf71wf34hv"))))
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
    (home-page "https://github.com/smgogarten/GWASTools")
    (synopsis "Tools for Genome Wide Association Studies")
    (description
     "This package provides classes for storing very large GWAS data sets and
annotation, and functions for GWAS data cleaning and analysis.")
    (license license:artistic2.0)))

(define-public r-kegggraph
  (package
    (name "r-kegggraph")
    (version "1.60.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "KEGGgraph" version))
       (sha256
        (base32 "0bgigkmnz5zvsygcifk2fc7i8rahmqr4brk65qa3104clqqvdfkl"))))
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
    (version "1.30.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ldblock" version))
       (sha256
        (base32
         "18gqmwwi1i4gndqljy809n5zzx38agawxxwhlknpas6k2iskd1xq"))))
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
    (version "2.7-5")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "abn" version))
       (sha256
        (base32
         "0ibznjhy7vmh2myarvmxy06rvddbpbarbp201px62mig2pb9aq4y"))))
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
    (native-inputs
     (list r-r-rsp))
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
    (version "1.40.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "pathview" version))
       (sha256
        (base32 "0mld7vys6s5k8snk1gniph9wkw3726vsx430nhyyr76fp4ps2mks"))))
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
    (version "1.70.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "snapCGH" version))
              (sha256
               (base32
                "0yz5q7w3lqv8ac6a764xlbm4hq3ncnngqng879qbpmzhgb4r155g"))))
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
    (version "1.34.1")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "SNPRelate" version))
              (sha256
               (base32
                "0js932qvhlwmnrr2jfvsch2zm6w6a1z5wydns3r1bw24r817dlgh"))))
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
    (version "1.50.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "snpStats" version))
       (sha256
        (base32
         "1v2j2943apphs8i68ap9c8sxd4j5a04qda5scq4dqqkzqd0c5b6i"))))
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
    (version "1.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "chromstaR" version))
       (sha256
        (base32
         "1qrcmydk080m1rzwbzx2km4vpbahnq5y73mrnpgjjjrxbndz9xz9"))))
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
    (version "2.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Guitar" version))
       (sha256
        (base32
         "1v1yw90waq0ccj6pxdipipy5bnk0k762m5kadl4h3jgpcrkf2fvc"))))
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
    (version "2.32.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ballgown" version))
       (sha256
        (base32
         "05lvpvy4gs3vg4ks23prx0mdp730h1m9kpg4al03ah062gyq3ahx"))))
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
    (version "1.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "megadepth" version))
       (sha256
        (base32
         "1zzhgfnrr3r5ismbbj9jcqss8mr8ll6p4d3z026ya2khb0i7clc7"))))
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
    (version "2.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BEclear" version))
       (sha256
        (base32
         "1sdw69dhrzkcpc7flvp1gkixvpsbrckyg756rlm17hjjwfxlz5lb"))))
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
    (version "1.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BgeeCall" version))
       (sha256
        (base32
         "0j83scxpq51bhxwfps7chcawzsqrd7pk7x2jg29zj4zbp1jbrnl0"))))
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
    (version "2.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BgeeDB" version))
       (sha256
        (base32
         "1s6v17bavidlfy6habi4bv6lmwkrx8c5was1zcq0kb05qdvpn4ph"))))
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
    (version "1.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "biobtreeR" version))
       (sha256
        (base32
         "0cbhlxh0w736695niyjd59fcvplna2f2x2av9k0sd1r3dy6lrcd4"))))
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
    (version "3.58.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "minet" version))
       (sha256
        (base32
         "04ll8nqhmyj2v12n2xskia8dh5px8mzj8fbyw1pzahk9vp085gkr"))))
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
    (version "1.40.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "geNetClassifier" version))
       (sha256
        (base32
         "1bn7h8jwmmgqzp0s3xdbsicm8pxmrgvnxmjnjs10d1rvp8znzjnn"))))
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
    (version "1.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "dir.expiry" version))
       (sha256
        (base32
         "1vccq28734s7zk7pc7i9zq4rzbi373xqqvgkzamjpii36phkdmlw"))))
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
    (version "1.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "basilisk.utils" version))
       (sha256
        (base32
         "0g5apvna9wzjlm7g9hdafy44nrg5rp3qh4anwpgwwp2vr0vxn37k"))))
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
    (version "1.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "basilisk" version))
       (sha256
        (base32
         "0bg6jfl12jsmhgby7x7g2vfmi61rx0jdksi97hb0zajgh1nvhirh"))))
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
    (version "1.10.3")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "biocthis" version))
       (sha256
        (base32
         "1v0qrypdzl1bg85k8i7qamb6709cgk4ypmisjh6bn5r36nqd5qx4"))))
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
    (version "1.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "biodb" version))
       (sha256
        (base32
         "1lcjq1zfarhc7pqqrkqnaycsv2f27n4n8mfvrc3c9ww4lsza555k"))))
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
    (version "1.28.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "biomformat" version))
       (sha256
        (base32
         "04kl2432wq6ybdhispvp98ylgyk3kkhmjx1nxdvcal7bfpy2vskk"))))
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
    (version "1.74.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "MVCClass" version))
       (sha256
        (base32
         "1vrn2c5m4w7g13s1zd1am5b02lp026zg6gvpg8dvsras0j56rhr6"))))
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
    (version "1.68.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BioMVCClass" version))
       (sha256
        (base32
         "07iay2dpq3margh1ny2snlc75g7fpfs5bf2gmhv88d31b8blg57r"))))
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
    (version "1.38.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "biomvRCNS" version))
       (sha256
        (base32
         "0n026b744ah5kcnipsyiqqs7vlz5n5im1kgv35i2pgxyzvf7a8sn"))))
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
    (version "1.8.5")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BioNERO" version))
       (sha256
        (base32
         "0nrvq6cn55qzp66pqssyfxl2wh5dfqndchcv8qgfqajsnz8i35xm"))))
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
    (version "1.60.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BioNet" version))
       (sha256
        (base32
         "19caj3aj6gndkxkrd9s2x8v59hcdwdyxrx0ji473c3d5qrykskl1"))))
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
    (version "1.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BioNetStat" version))
       (sha256
        (base32
         "0bvfgppsdih6s70iizd7zh3zkb0hg4s4h1728xmyjqbvkl8fsdf4"))
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
    (version "1.28.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BioQC" version))
       (sha256
        (base32
         "0g6imi03l4xm0chx3i9wd2vdy0ls78lnylp294fq0jldl4n6y0nw"))))
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
    (version "1.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "BioTIP" version))
       (sha256
        (base32
         "0lqnxaycrva2092h4swmk6na2pq2kp951dmicyw399djd1i7yj50"))))
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
    (version "1.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "biotmle" version))
       (sha256
        (base32
         "1hjkhwbld8m52lsy2ba6cnp02n0xykaxm9650r5zmzry38dw9nb5"))))
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
    (version "1.36.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "bsseq" version))
       (sha256
        (base32
         "1mvhh99h1l6f5rlz0fzqkjpz9hblj2rbv8nx4j9n93g8riz7wsi3"))))
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
    (version "1.28.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "dada2" version))
              (sha256
               (base32
                "1vxy2106rzygy0dr38wmw84zrqp9gsvaphliikdmhx0n2gv6xhd1"))))
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
    (version "1.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "dmrseq" version))
       (sha256
        (base32
         "1xj23dqvmxi1sn4qn4zwvn9ggv128kr3gxmd7906463s9ap8qjja"))))
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
    (version "1.40.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "omicade4" version))
              (sha256
               (base32
                "086nh0m82gjlxyg6i230wpkin4zc3a7cfla0qzvj28h0n8irkm5r"))))
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
    (version "3.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "OmnipathR" version))
       (sha256
        (base32 "0r2nv3nbadnvqby7p5bdpl9fxjb30h0c8kjv1dkqqnhk5rbfl4hf"))))
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
           r-rmarkdown
           r-progress
           r-purrr
           r-rappdirs
           r-readr
           r-readxl
           r-rlang
           r-rvest
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
    (version "1.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "biscuiteer" version))
       (sha256
        (base32
         "07nak6mfw9m8ldl336sqgkl3638ibv1hxwd89hwxdrn914wz3hw3"))))
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
    (version "2.28.3")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "TCGAbiolinks" version))
       (sha256
        (base32 "1hh09ya4jg062k1ibp1cpvdrgv6gwr95ch57iycgd3cjc5g0xhii"))))
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
    (version "1.8.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "tricycle" version))
              (sha256
               (base32
                "1ags05rhi431jsk0kqs7rmxj853mf5c0ibfqknbwy2wmbz8p1vq6"))))
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
    (version "1.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "tximeta" version))
       (sha256
        (base32
         "03dmnpfffj18c6009qg985i596l4ckf3myin12g2p6xr56880bz1"))))
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
    (version "1.44.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "phyloseq" version))
       (sha256
        (base32 "0rmv8f1m8ck97v991wdly3fma2rjczhi974lh7ikwx6rcx6fp9ah"))))
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
