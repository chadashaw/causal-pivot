# The Causal Pivot: A structural approach to genetic heterogeneity and variant discovery in complex diseases

This repository contains the code for the study published in *The American Journal of Human Genetics* (2025).
The research presents the Causal Pivot (CP) as a structural causal model (SCM) for analyzing genetic heterogeneity in complex diseases, leveraging established causal factors to detect contributions of additional suspected causes.

## Overview

The study examines three gene-disease pairs using UK Biobank data:
- **LDLR** variants and high cholesterol (HC)  
- **BRCA1** variants and breast cancer (BC)
- **GBA** variants and Parkinson's disease (PD)

We developed a likelihood ratio test (LRT) using maximum likelihood estimation (MLE) to detect the causal pivot effect
and performed comprehensive power analysis using simulations.

## Repository Structure

```
├── analysis/
│   ├── ukb_pivot/          # Main empirical analysis on UK Biobank data
│   ├── lrt_power/          # Power analysis and simulation studies  
│   ├── equations/          # Mathematical foundations (MLE equations)
│   └── examples/           # Simple reproducible examples
├── gt_extract/             # Genotype extraction workflow (DNAnexus)
```


## Quick Start

### Prerequisites

- R (≥4.0) with renv package management
- Required R packages specified in `renv.lock`

### Setup

1. Clone the repository:
```bash
git clone [repository-url]
cd causal-pivot
```

2. Restore R environment:
```r
renv::restore()
```

3. Set up data directories (see component READMEs for details)

### Running the Analysis

#### Main Empirical Analysis
```r
# Navigate to analysis directory
setwd("analysis/ukb_pivot")

# Run complete analysis pipeline
source("main.R")
```

#### Power Analysis
```r
# Navigate to power analysis directory  
setwd("analysis/lrt_power")

# Run power simulations
rmarkdown::render("lrt.sim.Rmd")
```

## Key Results and Figures

### Main Figures
- **Figure 2**: 4-panel power analysis showing LRT performance vs. competing methods
- **Figure 3**: Collider effects across PRS tertiles and mutation type distributions  
- **Figure 4**: Ancestry control analysis using k-nearest neighbors
- **Figure 5**: Genetic burden analysis in lysosomal storage disease pathway

### Supplemental Figures
- **S1-S5**: Theoretical foundations and data characterization
- **S6, S8**: Empirical permutation contour plots for statistical validation
- **S7**: Control variant analysis (negative control)
- **S10**: Power analysis with confounders
- **S12**: Age-stratified analysis

Figures are generated in `analysis/ukb_pivot/plots/` with separate directories for main and supplemental figures.

## Methods Summary

### Causal Pivot Detection
- Uses collider bias to detect causal relationships between PRS and rare variants
- Likelihood ratio test compares causal vs. non-causal models
- Robust to population stratification and other confounders

### Statistical Approach
- Maximum likelihood estimation for parameter inference
- Permutation testing for statistical significance
- Power analysis across parameter space using simulations

### Data Sources
- UK Biobank whole genome sequencing data (≥150k samples)
- Polygenic risk scores from published GWAS
- Rare variant annotations from OpenCRAVAT

## Component Documentation

Each analysis component has detailed documentation:

- [**Empirical Analysis**](analysis/ukb_pivot/README.md) - Main UK Biobank analysis workflow
- [**Power Analysis**](analysis/lrt_power/README.md) - Simulation studies and power calculations  
- [**Equations**](analysis/equations/README.md) - Mathematical foundations and MLE equations
- [**Genotype Extraction**](gt_extract/README.md) - Data preprocessing workflow

## Reproducibility

### Local Execution
All analyses require preprocessed UK Biobank data generated using the gt_extract toolkit. No example data is provided due to data use restrictions. See component READMEs for specific requirements.

### Cloud Execution
The original analysis used DNAnexus cloud platform for large-scale computations. See [genotype extraction documentation](gt_extract/README.md) for cloud execution details.

### Environment Management
This project uses `renv` for reproducible package management across multiple R projects. Navigate to each analysis directory and run `renv::restore()` to install exact package versions for that component.

## Citation

Shaw CA, Williams CJ, Tan T, Illera D, Di N, Shulman J, Belmont JW. The Causal Pivot: A structural approach to genetic heterogeneity and variant discovery in complex diseases. *The American Journal of Human Genetics*. 2025;112(9):1-15. doi:10.1016/j.ajhg.2025.07.012

## Contact

[Contact information for corresponding authors]

## License

[License information]

# Disclaimer

Claude Sonnet 4 by Anthropic was used to generate much of the README documentation;
however, it was all manually reviewed by one of the authors.
