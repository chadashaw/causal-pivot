# Figure Generation Index

This document provides a complete index of all figures generated in the Causal Pivot analysis, organized by manuscript section and supplementary materials.

## Main Manuscript Figures

### Figure 2: Power Analysis (4-panel composite)
- **Location**: `analysis/lrt_power/lrt.sim.Rmd`
- **Files**: 
  - Panel A: `fig2.panelA.main`
  - Panel B: `fig2.panelB.main`
  - Panel C: `fig2.panelC.main`
  - Panel D: `fig2.panelD.main`
  - Composite: `fig2`
- **Output**: `analysis/lrt_power/output/svg/fig2.*`
- **Description**: 4-panel power analysis comparing Causal Pivot LRT against competing methods

### Figure 3: Collider Effects and PRS Analysis
- **Location**: `analysis/ukb_pivot/collider.plots.Rmd`
- **Files**: Multiple plots combined for manuscript figure
- **Output**: `analysis/ukb_pivot/plots/svg/main/`
- **Description**: PRS tertile analysis and mutation type distributions

### Figure 4: Ancestry Control Analysis
- **Location**: `analysis/ukb_pivot/ancestry.control.Rmd`
- **Files**: 
  - Main: `fig4`
  - With age: `fig4.w.age`
- **Output**: `analysis/ukb_pivot/plots/svg/composed/fig4.*`
- **Description**: k-nearest neighbors ancestry stratification analysis

### Figure 5: Genetic Burden Analysis  
- **Location**: `analysis/ukb_pivot/genetic.burden.Rmd`
- **Files**: `genetic.burden.viz`
- **Output**: `analysis/ukb_pivot/plots/svg/main/`
- **Description**: Lysosomal storage disease pathway burden analysis

## Supplementary Figures

### Supplementary Figure 1-2: Data Characterization
- **Location**: `analysis/ukb_pivot/setup.Rmd`
- **Output**: `analysis/ukb_pivot/plots/svg/supplementary/`

### Supplementary Figure 3: Power Analysis Extensions
- **Location**: `analysis/lrt_power/lrt.sim.Rmd`
- **Files**: `supp.fig3`
- **Output**: `analysis/lrt_power/output/svg/supp.fig3.*`

### Supplementary Figure 4: Rare Variant Summary
- **Location**: `analysis/ukb_pivot/rv.summaries.Rmd`
- **Output**: `analysis/ukb_pivot/plots/svg/supplementary/`

### Supplementary Figure 5: PRS Distribution
- **Location**: `analysis/ukb_pivot/rv.summaries.Rmd`
- **Files**: `supp.fig5`
- **Output**: `analysis/ukb_pivot/plots/svg/supplementary/supp.fig5.*`

### Supplementary Figure 6: LogitG MLE Permutation Contour
- **Location**: `analysis/ukb_pivot/lrt.Rmd`
- **Files**: `supp.fig6`
- **Output**: `analysis/ukb_pivot/plots/svg/supplemental/supp.fig6.*`
- **Description**: Empirical permutation contour plots for statistical validation

### Supplementary Figure 7: Control Variant Analysis
- **Location**: `analysis/ukb_pivot/rv.summaries.Rmd`
- **Files**: Control variant plots
- **Output**: `analysis/ukb_pivot/plots/svg/supplementary/`

### Supplementary Figure 8: LiabilityG MLE Permutation Contour
- **Location**: `analysis/ukb_pivot/liabilityG.lrt.Rmd`
- **Files**: `supp.fig8`
- **Output**: `analysis/ukb_pivot/plots/svg/supplemental/supp.fig8.*`
- **Description**: Liability model validation permutation contours

### Supplementary Figure 9: Additional Power Analysis
- **Location**: `analysis/lrt_power/lrt.sim.Rmd`
- **Files**: Various power analysis plots
- **Output**: `analysis/lrt_power/output/svg/`

### Supplementary Figure 10: Age-Adjusted Power Analysis
- **Location**: `analysis/lrt_power/lrt.sim.Rmd`
- **Files**: `supp.fig10`
- **Output**: `analysis/lrt_power/output/svg/supplemental/supp.fig10.*`
- **Description**: Power analysis with age confounders

### Supplementary Figure 11: Additional Collider Analysis
- **Location**: `analysis/ukb_pivot/collider.plots.Rmd`
- **Files**: Various collider plots
- **Output**: `analysis/ukb_pivot/plots/svg/supplementary/`

### Supplementary Figure 12: Age-Stratified Analysis
- **Location**: `analysis/ukb_pivot/collider.plots.Rmd`
- **Files**: `supp.fig12`
- **Output**: `analysis/ukb_pivot/plots/svg/supplemental/supp.fig12.*`
- **Description**: Age tertile stratified collider analysis

## Additional Analysis Plots

### Maximum Likelihood Estimation Diagnostics
- **Location**: `analysis/ukb_pivot/lrt.Rmd` and `analysis/ukb_pivot/liabilityG.lrt.Rmd`
- **Files**: Various MLE diagnostic plots
- **Output**: `analysis/ukb_pivot/plots/svg/maximum.likelihood.estimation/`
- **Description**: Permutation histograms and contour plots for MLE validation

### Collider Effect Visualizations
- **Location**: `analysis/ukb_pivot/collider.plots.Rmd`
- **Files**: 
  - Case-control scatter: `cc.scatter`
  - Control variant scatter: `cv.cc.scatter`
  - Tertile analysis: `tertiles.3x3`
- **Output**: `analysis/ukb_pivot/plots/svg/collider/`

### Theoretical Foundations
- **Location**: `analysis/equations/conditional.means.R`
- **Files**: Theoretical plots
- **Output**: `analysis/equations/plots/`

## Plot Directory Structure

```
analysis/
├── ukb_pivot/plots/
│   ├── svg/
│   │   ├── main/              # Main manuscript figures
│   │   ├── supplemental/      # Supplementary figures  
│   │   ├── composed/          # Multi-panel composed figures
│   │   ├── collider/          # Collider effect plots
│   │   └── maximum.likelihood.estimation/  # MLE diagnostics
│   ├── pdf/                   # PDF versions
│   └── png/                   # PNG versions
├── lrt_power/output/
│   ├── svg/
│   │   ├── main/              # Main power analysis figures
│   │   └── supplemental/      # Supplementary power figures
│   ├── pdf/
│   └── png/
└── equations/plots/           # Theoretical foundation plots
```

## Figure Generation Commands

### Main Analysis Pipeline
```r
# Navigate to main analysis
setwd("analysis/ukb_pivot")
source("main.R")  # Generates all empirical analysis figures
```

### Power Analysis
```r
# Navigate to power analysis  
setwd("analysis/lrt_power")
rmarkdown::render("lrt.sim.Rmd")  # Generates all power analysis figures
```

### Individual Components
```r
# Generate specific figure sets
rmarkdown::render("lrt.Rmd")           # Figure S6 and MLE diagnostics
rmarkdown::render("ancestry.control.Rmd")  # Figure 4
rmarkdown::render("collider.plots.Rmd")    # Figure 3 and S12
rmarkdown::render("genetic.burden.Rmd")    # Figure 5
```

## Notes

- All figures are generated in SVG format for publication quality
- PDF and PNG versions are also created for compatibility
- Figure naming follows manuscript convention (main: `fig[N]`, supplementary: `supp.fig[N]`)
- Plot dimensions are optimized for journal requirements
- Intermediate diagnostic plots are saved in separate subdirectories