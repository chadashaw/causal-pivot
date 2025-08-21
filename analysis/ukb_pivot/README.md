# UK Biobank Empirical Analysis

This directory contains the main empirical analysis using UK Biobank
data to test the Causal Pivot hypothesis across three gene-disease
pairs: LDLR-high cholesterol, BRCA1-breast cancer, and GBA-Parkinson's
disease.

## Overview

The Causal Pivot analysis evaluates whether controlling for disease
induces a correlation between rare variant genotype status and polygenic
risk scores when both are causal drivers of the same disease. This
correlation (collider bias) serves as evidence for causal relationships
between genetic factors and disease outcomes.

### Study Population

-   **Sample Size**: 392,521 European ancestry samples from UK Biobank
-   **Data Sources**: Whole exome sequencing + polygenic risk scores
-   **Cohort Design**: Case-control analysis for each disease

### Statistical Approach

-   **Method**: Maximum likelihood estimation with likelihood ratio
    tests
-   **Null Hypothesis**: No association between rare variants and PRS in
    disease cohort
-   **Test Statistic**: Permutation-based significance testing

## Data Requirements

### Required Input Files

The analysis requires preprocessed data from the genotype extraction
pipeline:

#### Core Data Files

-   **cohort-meta.tsv**: Sample metadata from UK Biobank cohort browser
-   **geno.mtx**: Genotype calls in Matrix Market sparse format
-   **sample.ids**: Sample ID mapping for genotype matrix
-   **marker.ids**: Variant ID mapping for genotype matrix
-   **annotations.variant.tsv.gz**: OpenCRAVAT variant annotations

#### Required Metadata Columns

-   `Participant ID`: UK Biobank anonymized sample ID (EID)
-   `Year of birth`, `Genetic sex`: Demographics
-   `LDL direct | Instance 0`: Cholesterol measurements
-   `Date G20 first reported (parkinson's disease)`: Disease onset dates
-   `Genetic principal components | Array 1-5`: Population structure
    controls
-   `Standard PRS for breast cancer (BC)`: Breast cancer polygenic risk
    score
-   `Standard PRS for low density lipoprotein cholesterol (LDL_SF)`:
    Cholesterol PRS
-   `Standard PRS for parkinson's disease (PD)`: Parkinson's disease PRS

See [genotype extraction documentation](../../gt_extract/README.md) for
data preprocessing details. \## Analysis Workflow

### 1. Setup and Configuration

``` r
# Restore R environment
renv::restore()

# Set working directory
setwd("analysis/ukb_pivot")

# Run complete analysis pipeline (executes scripts in correct order)
source("main.R")
```

**Important**: The `main.R` script executes all analysis components in
the correct order: #. setup.Rmd - Data loading and environment setup #.
rv.summaries.Rmd - Summary plots and supplementary data exports #.
lrt.Rmd - Primary statistical analysis #. liabilityG.lrt.Rmd - Liability
model validation #. collider.plots.Rmd - Empirical collider
visualizations #. ancestry.control.Rmd - Population (confounder) control
analysis #. genetic.burden.Rmd - Oligogenic burden analysis

### 2. Core Analysis Components

#### Data Processing

-   **setup.Rmd**: Environment setup and data loading
-   **utils/data.R**: Data parsing and cohort generation functions
-   **utils/variant.classifiers.R**: OpenCRAVAT annotation processing

#### Statistical Analysis

-   **lrt.Rmd**: Maximum likelihood estimation and likelihood ratio
    tests
-   **liabilityG.lrt.Rmd**: Continuous outcome liability model for
    Hypercholesterolemia cohort
-   **utils/lrt.R**: Core statistical functions for MLE and permutation
    testing

#### Validation and Controls

-   **ancestry.control.Rmd**: Population stratification analysis using
    k-NN
-   **genetic.burden.Rmd**: Pathway burden analysis for lysosomal
    storage diseases
-   **collider.plots.Rmd**: Age-stratified and mutation type analyses

### 3. Key Parameters

#### Gene-Disease Pairs

-   **LDLR**: Low-density lipoprotein receptor (high cholesterol)
-   **BRCA1**: Breast cancer susceptibility gene 1 (breast cancer)
-   **GBA**: Glucocerebrosidase (Parkinson's disease)

#### Statistical Settings

-   **Permutation Count**: 32-256 (configurable via `n.mle.perms`)
-   **Optimization**: Simulated annealing with multiple runs
-   **Significance**: Permutation-based p-values with 95th percentile
    thresholds

## Output Files

### Statistical Results

-   **logitG.mle.result.rds**: Primary LRT results for all gene-disease
    pairs
-   **liabilityG.mle.result.rds**: Liability-scale model validation
    results
-   **ancestry.control.result.rds**: Population stratification control
    analysis
-   **genetic.burden.result.rds**: Pathway-based burden test results

### Figures and Plots

-   **plots/svg/main/**: Main manuscript figures (Figure 2-5) in SVG
    format
-   **plots/svg/supplemental/**: Supplemental figures (S1-S12) in SVG
    format
-   **plots/png/**, **plots/pdf/**: Corresponding PNG and PDF format
    outputs

### Data Products

-   **cohorts.rds**: Processed cohort definitions and sample mappings
-   **rv.prs.df.rds**: Rare variant and PRS data matrix
-   **cohort.dfs.rds**: Case-control datasets for each disease

## Usage Notes

### Reproducibility

-   Random seeds set in permutation testing via `set.seed()` calls
-   Package versions locked via renv
-   Intermediate results cached where appropriate

For questions about specific analysis components, see individual R
Markdown files for detailed documentation.
