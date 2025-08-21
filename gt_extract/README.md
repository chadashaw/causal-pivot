# Genotype Extraction Toolkit (gt_extract)

This directory contains a comprehensive toolkit for extracting,
processing, and annotating whole exome sequencing data from the UK
Biobank Research Analysis Platform (RAP). The toolkit is designed for
scalable cloud-based processing of genomic data for causal analysis
workflows.

## Overview

The gt_extract toolkit provides an end-to-end pipeline for:

-   **Cohort Selection**: Extract participant IDs from UKB RAP cohorts
-   **Genomic Filtering**: Filter variants by genomic regions using BED
    files
-   **Format Conversion**: Convert from BGEN to PLINK2 to sparse matrix
    formats
-   **Variant Annotation**: Comprehensive annotation using OpenCRAVAT
-   **Quality Control**: Built-in error handling and resume capabilities

## Architecture

### Core Components

```         
gt_extract/
├── scripts/                    # Main extraction workflow
│   ├── run.sh                 # Primary orchestration script
│   ├── common.sh              # Shared functions and constants
│   ├── annotate_markers.sh    # OpenCRAVAT annotation pipeline
│   └── pgen_to_mmT.py         # PLINK2 to sparse matrix converter
├── tcg_cloud_workstation/     # Custom DNAnexus cloud workstation
│   ├── dxapp.json            # App configuration
│   ├── src/code.py           # Workstation setup code
│   └── resources/            # Pre-installed tools and configs
├── bin/                       # Compiled binaries (not in repo)
└── build_cloud_workstation.sh # Workstation deployment script
```

### Workflow Pipeline

1.  **Setup Phase**: Initialize run environment and download cohort
    metadata
2.  **Extraction Phase**: Process chromosomes in parallel (1-22, X, Y)
3.  **Integration Phase**: Concatenate chromosomal data
4.  **Annotation Phase**: Add functional annotations via OpenCRAVAT
5.  **Output Phase**: Generate analysis-ready sparse matrices

## Prerequisites

### Platform Requirements

-   **UK Biobank RAP Project** with access to WES data (Field 23159)
-   **DNAnexus Platform** account with appropriate permissions
-   **High-memory compute instances** for large cohort processing

### Dependencies

#### System Tools

| Tool     | Version      | Purpose                       |
|----------|--------------|-------------------------------|
| bcftools | v1.16+       | VCF/BCF file manipulation     |
| plink2   | v2.00a5.1LM+ | Genotype file conversion      |
| jq       | v1.6+        | JSON processing               |
| python3  | 3.8+         | Data processing scripts       |
| dxpy     | v0.375+      | DNAnexus platform integration |

#### Python Packages

-   **pgenlib**: PLINK2 binary file reading
-   **numpy**: Numerical computing
-   **tqdm**: Progress bars
-   **Bio**: Bioinformatics utilities
-   **open-cravat**: Variant annotation framework

#### OpenCRAVAT Modules

Pre-configured annotation modules for comprehensive variant
characterization.

## Usage

### Quick Start

1.  **Deploy Cloud Workstation**:

    ``` bash
    ./build_cloud_workstation.sh
    ./tcg_cloud_workstation/start_job.sh my_extraction_job 24h
    ```

2.  **Start New Extraction**:

    ``` bash
    # SSH into workstation, then:
    cd ~/scripts
    ./run.sh my_cohort_name path/to/regions.bed
    ```

3.  **Resume Interrupted Run**:

    ``` bash
    ./run.sh previous_run_id
    ```

### Input Specifications

#### Cohort Selection

-   **Format**: UKB RAP Cohort Browser dataset name
-   **Location**: Relative path from project root
-   **Example**: `"my_project/cohorts/PD_cases_controls"`

#### Genomic Regions

-   **Format**: Standard BED file (tab-separated)

-   **Columns**: chromosome, start, end

-   **Example**:

    ```         
    1   1000000   2000000
    2   3000000   4000000
    ```

### Output Structure

All outputs are organized under `/runs/{RUN_ID}/` in the DNAnexus
project:

```         
/runs/{RUN_ID}/
├── eids.txt                          # Participant IDs
├── regions.bed                       # Input regions (copied)
├── pgen_outputs/                     # PLINK2 format files
│   ├── chr1.[pvar|pgen|psam]        # Per-chromosome files
│   ├── chr2.[pvar|pgen|psam]        
│   ├── ...
│   └── markers.[pvar|pgen|psam]     # Concatenated all chromosomes
├── geno.mtx                          # Sparse matrix (Matrix Market)
├── marker.ids                       # Column names for matrix
├── sample.ids                       # Row names for matrix
└── markers.annotations.tsv.gz       # OpenCRAVAT annotations
```

### Key Output Files

#### Genotype Matrix (`geno.mtx`)

-   **Format**: Matrix Market coordinate format
-   **Content**: Sparse representation of genotype calls
-   **Encoding**: 0=homozygous reference, 1=heterozygous, 2=homozygous
    alternate

#### Annotations (`markers.annotations.tsv.gz`)

-   **Format**: Gzipped tab-separated values
-   **Content**: Comprehensive variant annotations from OpenCRAVAT
-   **Includes**: Gene symbols, consequence predictions, population
    frequencies, pathogenicity scores