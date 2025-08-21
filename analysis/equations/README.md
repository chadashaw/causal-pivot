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
**LogitG w/ additional factor**

#### Model Extension
- **Forward Model**: `logit(P(Y=1|X,G,A)) = α + βX + γG + ηXG + ζA + κXA`
- **Additional Parameters**:
  - **ζ**: Additional factor (A) main effect
  - **κ**: Additional factor (A) - PRS interaction

#### Use Cases
- Population stratification control
- Additional covariate adjustment
- Confounding control

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