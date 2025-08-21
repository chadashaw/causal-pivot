# Figure Generation Index

This document provides a complete index of all figures generated in the Causal Pivot analysis, organized by manuscript section and supplementary materials.

## Main Manuscript Figures

### Figure 2: Power Analysis (4-panel composite)

-   **Location**: [analysis/lrt_power/lrt.sim.Rmd](analysis/lrt_power/lrt.sim.Rmd)
-   **Files**:
    -   Panel A: `fig2.panelA.main`
    -   Panel B: `fig2.panelBi.main`
    -   Panel C: `fig2.panelC.main`
    -   Panel D: `fig2.panelD.main`
    -   Composite: `fig2`
-   **Description**: 4-panel power analysis comparing Causal Pivot LRT against competing methods

### Figure 3: Collider Effects and PRS Analysis

-   **Location**: [analysis/ukb_pivot/collider.plots.Rmd](analysis/ukb_pivot/collider.plots.Rmd) and [analysis/ukb_pivot/rv.summaries.Rmd](analysis/ukb_pivot/rv.summaries.Rmd)
-   **Files**:
    -   Panel A: Multiple collider plots (from collider.plots.Rmd)
    -   Panel B: `fig3B` - variant distribution histogram (from rv.summaries.Rmd)
-   **Description**: PRS tertile analysis and mutation type distributions

### Figure 4: Ancestry Control Analysis

-   **Location**: [analysis/ukb_pivot/ancestry.control.Rmd](analysis/ukb_pivot/ancestry.control.Rmd)
-   **Files**:
    -   Main: `fig4` (saved to main/ subdirectory)
    -   With age: `fig4.w.age` (saved to misc/ subdirectory)
-   **Description**: k-nearest neighbors ancestry stratification analysis

### Figure 5: Genetic Burden Analysis

-   **Location**: [analysis/ukb_pivot/genetic.burden.Rmd](analysis/ukb_pivot/genetic.burden.Rmd)
-   **Files**: `fig5`
-   **Description**: Lysosomal storage disease pathway burden analysis

## Supplementary Figures

### Supplementary Figure 1: Collider Effect Odds Ratios

-   **Location**: [analysis/lrt_power/conditional.means.R](analysis/lrt_power/conditional.means.R)
-   **Files**: `supp.fig1`
-   **Description**: Odds ratios showing how collider effect varies with PRS effect size (β) for cases and controls

### Supplementary Figure 2: Conditional PRS Means

-   **Location**: [analysis/lrt_power/conditional.means.R](analysis/lrt_power/conditional.means.R)
-   **Files**: `supp.fig2`
-   **Description**: E[X\|Y,G] curves showing conditional expectations vs β parameter for different Y,G combinations

### Supplementary Figure 3: Liability Model Power Analyses

-   **Location**: [analysis/lrt_power/lrt.sim.Rmd](analysis/lrt_power/lrt.sim.Rmd)
-   **Files**: `supp.fig3`
-   **Description**: LRT power of LiabilityG model against competing methods

### Supplementary Figure 4: Normal UKB PRS Distributions

-   **Location**: [analysis/ukb_pivot/setup.Rmd](analysis/ukb_pivot/setup.Rmd)
-   **Files**: `supp.fig4`
-   **Description**: PRS density distributions across diseases

### Supplementary Figure 5: Variant Mutation Type Waffle Plot

-   **Location**: [analysis/ukb_pivot/rv.summaries.Rmd](analysis/ukb_pivot/rv.summaries.Rmd)
-   **Files**: `supp.fig5`
-   **Description**: Waffle plot of pathogenic rare variants by sequence ontology

### Supplementary Figure 6: LogitG MLE Permutation Contour

-   **Location**: [analysis/ukb_pivot/lrt.Rmd](analysis/ukb_pivot/lrt.Rmd)
-   **Files**: `supp.fig6`
-   **Description**: Empirical permutation contour plots for statistical validation

### Supplementary Figure 7: Control Variants PRS Tertile 3x3

-   **Location**: [analysis/ukb_pivot/collider.plots.Rmd](analysis/ukb_pivot/collider.plots.Rmd)
-   **Files**: `supp.fig7`
-   **Description**: 3x3 tertiles plot showing control (synonymous) variants distribution across PRS tertiles

### Supplementary Figure 8: LiabilityG MLE Permutation Contour

-   **Location**: [analysis/ukb_pivot/liabilityG.lrt.Rmd](analysis/ukb_pivot/liabilityG.lrt.Rmd)
-   **Files**: `supp.fig8`
-   **Description**: Liability model validation permutation contours

### Supplementary Figure 10: Added Cause Logit MLE Power

-   **Location**: [analysis/lrt_power/lrt.sim.Rmd](analysis/lrt_power/lrt.sim.Rmd)
-   **Files**: `supp.fig10`
-   **Description**: Power analysis with age confounders

### Supplementary Figure 12: Age-Stratified PRS Tertiles

-   **Location**: [analysis/ukb_pivot/collider.plots.Rmd](analysis/ukb_pivot/collider.plots.Rmd)
-   **Files**: `supp.fig12`
-   **Description**: Age stratified, PRS tertile collider analysis

## Additional Analysis Plots

### Maximum Likelihood Estimation Diagnostics

-   **Location**: [analysis/ukb_pivot/lrt.Rmd](analysis/ukb_pivot/lrt.Rmd) and [analysis/ukb_pivot/liabilityG.lrt.Rmd](analysis/ukb_pivot/liabilityG.lrt.Rmd)
-   **Files**: Various MLE diagnostic plots
-   **Description**: Permutation histograms and contour plots for MLE validation

### Collider Effect Visualizations

-   **Location**: [analysis/ukb_pivot/collider.plots.Rmd](analysis/ukb_pivot/collider.plots.Rmd)
-   **Files**:
    -   Case-control scatter: `cc.scatter`
    -   Control variant scatter: `cv.cc.scatter`
    -   Tertile analysis: `tertiles.3x3`
