---
title: "Causal Segmentation Data Extraction Workflow Documentation"
author: "CJ Williams"
email: "cj@atmoscape.net"
date: "2024-04-01"
---

# Causal Segmentation Cohort Genotype Extraction and Annotation Workflow

This workflow, composed as a set of scripts (in bash, python and awk), filters,
formats and annotates whole exome genotype data from the
[UK Biobank (UKB)](https://www.ukbiobank.ac.uk/).
Given the name of a cohort stored in the
[UKB Research Analysis Platform (RAP)](https://ukbiobank.dnanexus.com/landing),
and a regions BED file, the process extracts cohort metadata and then processes
BGEN files by chromosome to create analytic products used as inputs to the
causal segmentation analysis written in R.

## Dependencies

The workflow should be run as a DNANexus job on a
[workstation](https://documentation.dnanexus.com/developer/cloud-workstation)
machine or DNANexus applet/app.
These scripts must be run within the context of a UKB RAP project with access
to the final release population level exome BGEN files
[(Data-Field 23159)](https://biobank.ctsu.ox.ac.uk/showcase/field.cgi?id=23159).
The process also depends on a number of Linux tools and Python modules:

### Binaries

| tool      | version used by authors   |
|:---------:|:-------------------------:|
| bcftools  | v.1.16                    |
| plink2    | v2.00a5.1LM AVX2 Intel    |
| jq        | jq-1.6                    |
| python3   | 3.8.10                    |
| dxpy      | v0.375.1                  |

### Python3 Modules

 - names_generator
 - numpy
 - tqdm
 - pgenlib
 - Bio
 - open-cravat

### OpenCRAVAT Modules

## Workflow Description

To execute the workflow make the script files executable and run:

```bash
./path/to/run.sh $COHORT_NAME $BEDFILE_PATH
```
### Inputs

The workflow can take either 1 or 2 parameters as input:

 - 1 parameter - passing existing run ID will pick up on a previous extraction
 - 2 parameters - passing name of a UKB RAP cohort and  path to a BED file will start new extraction

### Outputs

The RUN_ID is a randomly generated human-readable string printed early in the
workflow execution.
Outputs and intermediaries are upload to the RAP Project to the
`/run/$RUN_ID/` directory:

 - `eids.txt`
 - `$BEDFILE_PATH` - copied from input
 - `pgen_outputs/chr*.[pvar|pgen|psam]` - filtered & formated plink2 GT files for each chromosome
 - `pgen_outputs/markers.[pvar|pgen|psam]` - all chromosomes concatenated plink2 gt files
 - `geno.mtx` - sparse "Matrix Market" representation of cohort GT calls
  
   - `marker.ids` - colnames for `geno.mtx`
   - `sample.ids` - rownames for `geno.mtx`
   
 - `markers.annotations.tsv.gz` - gzipped open-cravat variant annotations TSV
