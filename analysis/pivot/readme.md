# DISCLAIMER

As per the UK Biobank (UKB) requirements, any exome and genotype data
downloaded from the UKB Research Analysis Platform (RAP) MUST be encrypted w/
AES256 at rest. We do this with 7z which uses AES256 by default.

# Collider Analysis

## Analysis Overview

The purpose of this analysis is to evaluate the hypothesis that controlling for
a particular disease may induce a correlation between between sample genotype
status and polygenic measures when both the genotype status and polygenic scores
are causal drivers of that disease.
The induction of this correlation may occur when no causal relationship or
correlation exists between genotypes and polygenic scores.
We test our hypothesis using an ad-hoc cohort of 392521 European samples from
the UKB partitioned into sub-cohorts for case-control analysis of specific
diseases of interest.
Genotype status is measured using whole exome genotype calls from the UKB, while
polygenic effects are measured using built-in PRS scores from UKB
sample metadata.

This induced correlation when controlling for a common causal target is called
a "causal" collider effect. Due to the collider between rare oligogenic
variation and polygenic causes of disease, we expect to find a statistically
significant negative correlation between PRS and monogenic/oligogenic events
only when controlling for disease.

## Input Files

The analysis depends on the output of our custom process which extracts and
annotates genotype data from regions of interest.

The files required for analysis are:

 - cohort-meta.tsv - Contains all cohort metadata included in cohort browser.
 
  - required columns:
  
    - `Participant ID`,
    - `Year of birth`,
    - `Genetic sex`,
    - `LDL direct | Instance 0`,
    - `Date G20 first reported (parkinson's disease)`,
    - `Genetic principal components | Array 1`,
    - `Genetic principal components | Array 2`,
    - `Genetic principal components | Array 3`,
    - `Genetic principal components | Array 4`,
    - `Genetic principal components | Array 5`,
    - `Standard PRS for breast cancer (BC)`,
    - `Standard PRS for low density lipoprotein cholesterol (LDL_SF)`,
    - `Standard PRS for parkinson's disease (PD)`
    
 - geno.mtx - Genotype data in Matrix Market format
 
  - sample.ids - sample.ID index for geno.mtx
  - marker.ids - marker ID index for geno.mtx
  
 - annotations.variant.tsv.gz - OpenCRAVAT-annotated TSV variant file for all
                                variants in necessary regions for all samples
                                in cohort.
## Products of data pipeline

Below is a list of the outputs from the pipeline that runs on the UKB RAP. Some
but not all are used in this analysis, but are included here for reference.

**cohort-meta.tsv**

The headers in this TSV can vary depending on the columns selected in the RAP
cohort browser dataset. To perform the full analysis, each cohort MUST have:
 - the "Participant ID" - this is the UKB anonymized "EID" of the sample
 - a column for each PRS value analyzed, they must match exactly with the PRS
   names defined in the [main.R](./main.R) `PRS_MAP` list.
  
**eid_sid.tsv**

This file is a space-separated CSV created by our process to map sample names
b/w the cohort browser and the exome data. The first column contains the EID as
is found in the cohort browser. The second column contains the original IDs as
used to produce the exome output.

**markers.gt.tsv.gz**

This file contains the post-processed exome data filtered by a particular marker
region and containing only the exome data for samples in each cohort. It
contains one marker/sample combination per row and only variant ALT calls for
any given sample are included (either HET (1) or HOM (2)).

The columns are:
 - sample_id (str) - the ID as mapped by column 2 of `eid_sid.tsv`
 - chrom (str) - chromosome (e.g. 'chr1', 'chr10', 'chrX')
 - pos (int) - variant 1-indexed position
 - ref - reference allele
 - alt - alternate allele
 - alt_count - alt calls in sample at locus (1 => HET, 2 => HOM)

**annotations.variant.tsv.gz**

This file contains the zipped TSV output from the OpenCRAVAT annotator. Not all
the columns in all runs are used in the analysis. To learn more about which
annotations are used and the format of this output see [annotation_parser.R](./annotation_parsers.R)
and the OpenCRAVAT annotation.

## Process Overview

The [main.R](./main.Rmd) script is the entry point for analysis. It is used to
set the global state (like the definition of cohorts, PRS and genes of interest)
and execute sub-scripts that process the input files to create results. Each
script contains documentation overviewing the component logic. A high-level
description is given here.

The main script sets up a relationship between cohorts and genes of interest,
defines the PRS columns of interest to pull from the cohort browser dataset,
defines the source and result directories.

Cohort metadata is copied to the output directory before the exome genotype data
is parsed into an intermediate format in which the sample IDs are mapped to the
UKB RAP EIDs. Afterwards, the annotation files for each cohort are parsed and
used to identify variants of interest on the gene associated with the cohort
disease. The presence of these variants of interest is used to classify samples
into either having or not having a genomic event of interest.

Once we've classified the samples we calculate the test statistic for each
pair of disease/gene/PRS combinations.

We show the presence of a significant collider effect between rare oligogenic
effects and polygenic risk scores when controlling for particular diseases.
Additionally, we show that this collider effect is not induced when controlling
for diseases towards which the genes and PRS have no known causal pathway.