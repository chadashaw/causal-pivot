# Mathematical Foundations and MLE Equations

This directory contains the mathematical foundation for the Causal Pivot analysis, including maximum likelihood estimation (MLE) equations and theoretical frameworks for different model specifications.

## Overview

The Causal Pivot method relies on structural causal models to detect genetic heterogeneity through collider bias. The mathematical framework implements likelihood-based inference for parameters in logistic and liability threshold models.

### Core Concept
The Causal Pivot leverages collider bias induced when conditioning on disease status creates correlation between independent causal factors (rare variants and polygenic risk scores). The induced correlation serves as evidence for causal relationships.

## Model Specifications

### 1. LogitG Model (`equations.logitG.R`)
**Standard logistic regression framework for binary outcomes**

#### Model Structure
- **Forward Model**: `logit(P(Y=1|X,G)) = α + βX + γG + ηXG`
- **Reverse Model**: `logit(P(G=1|X,Y=1)) = function(α,β,γ,η,ω)`

#### Key Parameters
- **α**: Intercept (baseline disease risk)
- **β**: Polygenic risk score effect
- **γ**: Rare variant main effect  
- **η**: Interaction effect (causal pivot parameter)
- **ω**: Rare variant population frequency

#### Functions Implemented
- `fY1XG1()`, `fY1XG0()`: Conditional disease probabilities
- `L()`: Log-likelihood function for MLE
- `D()`: Score equations (derivatives)
- `LR()`: Likelihood ratio test statistic
- `fisher.info()`: Fisher information matrix calculation

### 2. LogitG_A Model (`equations.logitG_A.R`)
**Age-adjusted logistic regression with additional confounders**

#### Model Extension
- **Forward Model**: `logit(P(Y=1|X,G,A)) = α + βX + γG + ηXG + ζA + κXA`
- **Additional Parameters**:
  - **ζ**: Age main effect
  - **κ**: Age-PRS interaction

#### Use Cases
- Population stratification control
- Age-dependent disease relationships
- Confounding adjustment

### 3. LiabilityG Model (`equations.liabilityG.R`)
**Liability threshold model for disease susceptibility**

#### Theoretical Framework
- **Liability Scale**: Continuous underlying disease risk
- **Threshold Model**: Disease manifests above liability threshold
- **Genetic Effects**: Additive contributions to liability

#### Applications
- Polygenic disease architecture modeling
- Continuous risk score validation
- Threshold effect investigation

## Computational Implementation

### Maximum Likelihood Estimation
```r
# Load equation system
source("equations.logitG.R")
equations <- define.logitG.equations(cases.only = TRUE)

# Estimate parameters
mle_result <- optim(
  par = start_values,
  fn = equations$L,      # Log-likelihood
  gr = equations$D,      # Score equations
  control = list(fnscale = -1)
)
```

### Likelihood Ratio Testing
```r
# Calculate test statistic
lr_stat <- equations$LR(mle_params)
chi2_stat <- 2 * lr_stat
p_value <- pchisq(chi2_stat, df = 2, lower.tail = FALSE)
```

### Fisher Information
```r
# Calculate confidence intervals
fisher_info <- equations$fisher.info(mle_params)
covariance_matrix <- solve(fisher_info)
std_errors <- sqrt(diag(covariance_matrix))
```

## Mathematical Details

### Conditional Probability Framework
The reverse model conditions on disease status to create collider bias:

**Key Insight**: Under causal pivot hypothesis, `P(G=1|X,Y=1)` varies with X due to collider bias, even when G⊥X in the population.

### Likelihood Construction
The likelihood integrates over all possible genetic states:

`L(α,β,γ,η,ω) = Π P(G_i|X_i,Y_i=1) * P(Y_i=1)`

### Parameter Interpretation
- **γ = 0**: No rare variant main effect
- **η = 0**: No causal pivot (null hypothesis)
- **η ≠ 0**: Evidence for causal pivot effect
- **β**: Captures polygenic component independent of rare variants

### Identifiability Conditions
1. **Sufficient Cases**: Adequate samples with Y=1
2. **Variant Diversity**: Both G=0 and G=1 cases present
3. **PRS Variation**: Adequate polygenic score distribution
4. **Parameter Constraints**: Identifiable parameter space

## Numerical Considerations

### Optimization Challenges
- **Multi-modal Likelihood**: Multiple local optima possible
- **Parameter Correlations**: High correlation between γ and η
- **Boundary Conditions**: Parameters near constraint boundaries
- **Convergence**: Sensitive to starting values

### Stability Enhancements
- **Multiple Starts**: Run optimization from different initial values
- **Robust Optimization**: Use simulated annealing or other global methods
- **Parameter Bounds**: Constrain to reasonable parameter ranges
- **Outlier Filtering**: Remove extreme parameter estimates

### Computational Efficiency
- **Vectorized Operations**: Optimize R code for speed
- **Analytical Derivatives**: Use exact gradients when possible
- **Parallel Processing**: Multiple optimization runs in parallel
- **Caching**: Store intermediate calculations

## Validation and Testing

### Theoretical Validation
- **Simulation Studies**: Verify parameter recovery
- **Asymptotic Properties**: Confirm MLE consistency
- **Power Analysis**: Validate test performance

### Empirical Validation
- **Known Relationships**: Test on established gene-disease pairs
- **Negative Controls**: Verify null hypothesis behavior
- **Sensitivity Analysis**: Parameter robustness testing

### Model Comparison
- **Nested Models**: LogitG vs LogitG_A
- **Alternative Specifications**: Different parameterizations
- **Goodness of Fit**: Model adequacy assessment

## Usage Examples

### Basic Implementation
```r
# Define model equations
source("equations.logitG.R")
logitG_equations <- define.logitG.equations(cases.only = TRUE)

# Prepare data
model_data <- list(
  Y = case_status,
  X = prs_scores,
  G = rare_variant_status
)

# Estimate parameters
start_params <- c(gamma = 0, eta = 0)
mle_fit <- run_mle(model_data, start_params, logitG_equations)
```

### Extended Analysis
```r
# Age-adjusted model
source("equations.logitG_A.R")  
logitG_A_equations <- define.logitG_A.equations(cases.only = TRUE)

# Include age covariate
extended_data <- modifyList(model_data, list(A = age_standardized))
extended_fit <- run_mle(extended_data, extended_params, logitG_A_equations)
```

## Technical References

### Statistical Theory
- Maximum likelihood estimation in logistic regression
- Likelihood ratio testing for nested models
- Fisher information and asymptotic inference
- Collider bias in causal inference

### Computational Methods
- Numerical optimization algorithms
- Gradient-based parameter estimation
- Robust statistical computation
- Simulation-based validation

For implementation details and specific function documentation, see individual equation files and the main analysis pipeline in `analysis/ukb_pivot/`.