# Power Analysis and Simulation Studies

This directory contains simulation studies and power analysis for the Causal Pivot likelihood ratio test (LRT).
The analysis investigates test performance across different parameter spaces and compares against alternative statistical approaches.

## Overview

### Objectives
1. **Power Analysis**: Evaluate LRT performance across biologically meaningful parameter ranges
2. **Method Comparison**: Compare Causal Pivot LRT against traditional approaches
3. **Robustness Testing**: Assess sensitivity to parameter misspecification

### Statistical Methods Compared
- **Causal Pivot LRT**: Primary method using maximum likelihood estimation
- **Forward Logistic Regression**: Traditional case-control analysi
- **Wilcoxon Rank-Sum Test**: Non-parametric comparison
- **Z-test**: Simple proportion test

## Key Files

### Main Analysis
- **lrt.sim.Rmd**: Primary power analysis with comprehensive simulations and figure generation

### Simulation Runners
- **runners/main.logitG.R**: Logistic model simulations
- **runners/main.logitG_A.R**: Age-adjusted logistic model simulations
- **runners/beta.spec.R**: Beta parameter specification studies
- **runners/misspec.omega.R**: Allele frequency misspecification analysis
- **runners/disease.enrichment.R**: Disease enrichment scenarios

### Utilities
- **utils.R**: Helper functions for simulation and analysis
- **dx.run.lrt.sh**: DNAnexus cloud execution script

## Simulation Framework

### Model Types
1. **LogitG Model**: Standard logistic regression framework
2. **LogitG_A Model**: Age-adjusted logistic regression
3. **LiabilityG Model**: Liability threshold model validation

### Parameter Space
- **Effect Sizes**: γ (rare variant effect), η (interaction effect), β (PRS effect)
- **Baseline Parameters**: α (baseline logit, determines disease prevalence)
- **Allele Frequencies**: ω (rare variant frequency) 
- **Sample Sizes**: N (sample size)

## Usage

### Environment Setup
```r
# Restore R environment
renv::restore()
```

### Local Execution
```r
# Navigate to power analysis directory
setwd("analysis/lrt_power")

# Run main power analysis (generates all figures)
rmarkdown::render("lrt.sim.Rmd")
```

### Cloud Execution (DNAnexus)
```bash
# Submit simulation job to DNAnexus
bash dx.run.lrt.sh
```

### Custom Simulations
```r
# Load simulation utilities
source("utils.R")

# Define custom parameter grid
param_grid <- expand.grid(
  gamma = seq(-1, 1, 0.2),
  eta = seq(-0.5, 0.5, 0.1),
  n = c(1000, 5000, 10000)
)

# Run simulations (example)
results <- run_power_simulation(param_grid)

# Generate power curves from results
power_curves <- results %>%
  group_by(method, n) %>%
  summarize(power = mean(p_value < 0.05)) %>%
  ggplot(aes(x = n, y = power, color = method)) +
  geom_line() +
  geom_point() +
  labs(x = "Sample Size", y = "Statistical Power")
```

## Technical Notes

### Reproducibility
- **Random Seeds**: Set via `set.seed()` in simulation scripts for reproducible results
- **Package Versions**: Locked via renv.lock
- **Platform Independence**: Runs on standard R installations

## Examples and Documentation

For detailed methodology and results interpretation, see the main simulation report: `lrt.sim.Rmd`
